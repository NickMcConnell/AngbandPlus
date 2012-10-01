-- File: generators.lua
-- This file contains various functions that allows randomly generated content to be
-- created. Random monsters, items, dungeons, etc... All code for randomly generated
-- content should be placed in this file.

-- This function will generate a random monster in one of the r_info slot.
-- It will replace an existing entry, or you can assign it to a new one.
-- If you want the value to be saved in the save file, use monster
-- slots 2050 to 2099, they are reserved specifically for that. 1030 is
-- used for random dungeon bosses.

-- "r_idx" is the monster's r_info ID.
-- The "rank" is the depth and overall difficulty of the monster.
-- "mtype" is the type of monsters to be generated:
--      0. Normal monster.
--      1. Random dungeon boss.
--	2. Random Flow boss.
--	3. Townsfolk.
--	4. Random dragon.
--	5. Skeleton Lord. (will have standard skeleton resistances)

function generate_monster (r_idx, rank, mtype)

	local mrace
	local mracechar
	local mracename
	local randval
	local rndcolor
	local i
	local j
	local speedbonus
	local twisted
	local charnum
	local skillmult

	-- 0. Female
	-- 1. Male
	-- 2. Neutral
	-- 3. To be determined.
	local msex = 3

	local monstertype
	local statspoints
	local skillpoints

	local strchance
	local dexchance
	local mindchance
	local attackchance
	local rangedchance
	local magicchance

	local friends = 0

	-- Diviner's Wish ability is ressolved upon the boss's generation.
	twisted = 0
	if (p_ptr.events[29014] > 0 and mtype == 1) then

		local ppower
		local ipower
		local dlev
		local difference

		-- Basic player's power and item's power.
		ppower = (p_ptr.skill[27] / 10) * p_ptr.abilities[(CLASS_DIVINER * 10) + 10]
		ipower = kind(p_ptr.events[29014]).level * 5

		-- Affect powers
		if (kind(p_ptr.events[29014]).level > (dungeon(200).maxdepth * 2)) then

			difference = kind(p_ptr.events[29014]).level - (dungeon(200).maxdepth * 2);

			ipower = ipower + ((ipower * (difference * 5)) / 100)
			ppower = ppower - ((ppower * (difference * 5)) / 100)
			if (ppower < 0) then ppower = 0 end
		elseif ((dungeon(200).maxdepth * 2) > kind(p_ptr.events[29014]).level) then

			difference = (dungeon(200).maxdepth * 2) - kind(p_ptr.events[29014]).level

			ipower = ipower - ((ipower * (difference * 5)) / 100)
			ppower = ppower + ((ppower * (difference * 5)) / 100)
			if (ipower < 0) then ipower = 0 end
		end

		-- Roll!
		if (not(lua_randint(ppower) >= lua_randint(ipower))) then

			-- The wish has been twisted.
			rank = rank + multiply_divide(rank, kind(p_ptr.events[29014]).level, 100)
			twisted = 1
			p_ptr.events[29014] = 0
		end
	end

	-- Fate can be twisted...
	if (p_ptr.events[29012] == 1 and not(mtype == 1 and twisted == 1) and not(mtype >= 2)) then

		rank = rank + p_ptr.events[29011]
		if (rank < 1) then rank = 1 end
	end

	-- First, determine the monster's race and name.
	charnum = 0
	mrace = lua_randint(36)

	-- 'a' Ant.
	if (mrace == 1) then

		mracechar = "a"
		charnum = 97
		mracename = "Ant"
		msex = 2

	-- 'b' Bat.
	elseif (mrace == 2) then
		
		mracechar = "b"
		charnum = 98
		mracename = "Bat"
		msex = 2

	-- 'd' Lesser Dragon.
	elseif (mrace == 3) then
		
		mracechar = "d"
		charnum = 100
		randval = lua_randint(4)
		if (randval == 1) then mracename = "Dragon"
		elseif (randval == 2) then mracename = "Wyvern"
		elseif (randval == 3) then mracename = "Drake"
		else mracename = "Dragonkin" end
		msex = 2
	-- 'e' Eye.
	elseif (mrace == 4) then
		
		mracechar = "e"
		charnum = 101
		randval = lua_randint(3)
		if (randval == 1) then mracename = "Eye"
		elseif (randval == 2) then mracename = "Watcher"
		else mracename = "Gazer" end
		msex = 2
	-- 'f' Feline.
	elseif (mrace == 5) then
		
		mracechar = "f"
		charnum = 102
		randval = lua_randint(5)
		if (randval == 1) then mracename = "Cat"
		elseif (randval == 2) then mracename = "Lion"
		elseif (randval == 3) then mracename = "Tiger"
		elseif (randval == 4) then mracename = "Panther"
		else mracename = "Feline" end
		msex = 2
	-- 'g' Golem.
	elseif (mrace == 6) then
		
		mracechar = "g"
		charnum = 103
		mracename = "Golem"
		msex = 2
	-- 'h' Humanoid.
	elseif (mrace == 7) then
		
		mracechar = "h"
		charnum = 104
		randval = lua_randint(4)
		if (randval == 1) then mracename = "Elf"
		elseif (randval == 2) then mracename = "Dwarf"
		elseif (randval == 3) then mracename = "Gnome"
		else mracename = "Humanoid" end
		msex = lua_randint(2) - 1
	-- 'j' Jelly.
	elseif (mrace == 8) then
		
		mracechar = "j"
		charnum = 106
		randval = lua_randint(3)
		if (randval == 1) then mracename = "Jelly"
		elseif (randval == 2) then mracename = "Slime"
		else mracename = "Ooze" end
		msex = 2
	-- 'k' Kobold.
	elseif (mrace == 9) then
		
		mracechar = "k"
		charnum = 107
		mracename = "Kobold"
		msex = 2
	-- 'm' Mold.
	elseif (mrace == 10) then
		
		mracechar = "m"
		charnum = 109
		randval = lua_randint(3)
		if (randval == 1) then mracename = "Mold"
		elseif (randval == 2) then mracename = "Plant"
		else mracename = "Weed" end
		msex = 2
	-- 'n' Naga.
	elseif (mrace == 11) then
		
		mracechar = "n"
		charnum = 110
		mracename = "Naga"
		msex = 0
	-- 'p' Human.
	elseif (mrace == 12) then
		
		mracechar = "p"
		charnum = 112
		randval = lua_randint(6)
		if (randval == 1) then mracename = "Warrior"
		elseif (randval == 2) then mracename = "Soldier"
		elseif (randval == 3) then mracename = "Messenger"
		elseif (randval == 4) then mracename = "Fanatic"
		elseif (randval == 5) then
			if (lua_randint(2) == 1) then
				mracename = "Lady"
				msex = 0
			else
				mracename = "Lord"
				msex = 1
			end
		else mracename = "Human" end
		if (msex == 3) then msex = lua_randint(2) - 1 end
	-- 'q' Quadroped/Beast.
	elseif (mrace == 13) then
		
		mracechar = "q"
		charnum = 113
		randval = lua_randint(3)
		if (randval == 1) then mracename = "Beast"
		elseif (randval == 2) then mracename = "Animal"
		else mracename = "Quadroped" end
		msex = 2
	-- 'r' Rat/Rodent.
	elseif (mrace == 14) then
		
		mracechar = "r"
		charnum = 114
		randval = lua_randint(3)
		if (randval == 1) then mracename = "Rat"
		elseif (randval == 2) then mracename = "Mouse"
		else mracename = "Rodent" end
		msex = 2
	-- 's' Skeleton.
	elseif (mrace == 15) then
		
		mracechar = "s"
		charnum = 115
		randval = lua_randint(2)
		if (randval == 1) then mracename = "Skeleton"
		else mracename = "Bones" end
		msex = 2
	-- 'u' Demon.
	elseif (mrace == 16) then
		
		mracechar = "u"
		charnum = 117
		randval = lua_randint(5)
		if (randval == 1) then mracename = "Imp"
		elseif (randval == 2) then mracename = "Fiend"
		elseif (randval == 3) then mracename = "Familiar"
		elseif (randval == 4) then mracename = "Devil"
		else mracename = "Demon" end
		msex = 2
	-- 'w' Worm.
	elseif (mrace == 17) then
		
		mracechar = "w"
		charnum = 119
		mracename = "Worm"
		msex = 2
	-- 'z' Zombie.
	elseif (mrace == 18) then
		
		mracechar = "z"
		charnum = 122
		randval = lua_randint(3)
		if (randval == 1) then mracename = "Zombie"
		elseif (randval == 2) then mracename = "Ghoul"
		else mracename = "Corpse" end
		msex = 2
	-- 'A' Angel.
	elseif (mrace == 19) then
		
		mracechar = "A"
		charnum = 65
		randval = lua_randint(2)
		if (randval == 1) then mracename = "Angel"
		else mracename = "Celestial" end
		msex = lua_randint(2) - 1
	-- 'B' Bird.
	elseif (mrace == 20) then
		
		mracechar = "B"
		charnum = 66
		randval = lua_randint(5)
		if (randval == 1) then mracename = "Bird"
		elseif (randval == 2) then mracename = "Eagle"
		elseif (randval == 3) then mracename = "Crow"
		elseif (randval == 4) then mracename = "Raven"
		else mracename = "Roc" end
		msex = 2
	-- 'C' Canine.
	elseif (mrace == 21) then
		
		mracechar = "C"
		charnum = 67
		randval = lua_randint(3)
		if (randval == 1) then mracename = "Dog"
		elseif (randval == 2) then mracename = "Wolf"
		else mracename = "Canine" end
		msex = 2
	-- 'E' Elemental/Spirit.
	elseif (mrace == 22) then
		
		mracechar = "E"
		charnum = 69
		randval = lua_randint(2)
		if (randval == 1) then mracename = "Elemental"
		else mracename = "Spirit" end
		msex = 2
	-- 'G' Ghost.
	elseif (mrace == 23) then
		
		mracechar = "G"
		charnum = 71
		randval = lua_randint(2)
		if (randval == 1) then mracename = "Ghost"
		else mracename = "Phantom" end
		msex = 2
	-- 'H' Hybrid.
	elseif (mrace == 24) then
		
		mracechar = "H"
		charnum = 72
		randval = lua_randint(4)
		if (randval == 1) then mracename = "Minotaur"
		elseif (randval == 2) then mracename = "Horror"
		elseif (randval == 3) then mracename = "Monster"
		else mracename = "Aberation" end
		msex = 2
	-- 'I' Insect.
	elseif (mrace == 25) then
		
		mracechar = "I"
		charnum = 73
		randval = lua_randint(5)
		if (randval == 1) then mracename = "Insect"
		elseif (randval == 2) then mracename = "Mantis"
		elseif (randval == 3) then mracename = "Scorpion"
		elseif (randval == 4) then mracename = "Wasp"
		else mracename = "Bug" end
		msex = 2
	-- 'J' Snake.
	elseif (mrace == 26) then
		
		mracechar = "J"
		charnum = 74
		randval = lua_randint(5)
		if (randval == 1) then mracename = "Snake"
		elseif (randval == 2) then mracename = "Serpent"
		elseif (randval == 3) then mracename = "Python"
		elseif (randval == 4) then mracename = "Viper"
		else mracename = "Cobra" end
		msex = 2
	-- 'L' Lich.
	elseif (mrace == 27) then
		
		mracechar = "L"
		charnum = 76
		mracename = "Lich"
		msex = 2
	-- 'M' Hydra.
	elseif (mrace == 28) then
		
		mracechar = "M"
		charnum = 77
		mracename = "Hydra"
		msex = 2
	-- 'P' Giant.
	elseif (mrace == 29) then
		
		mracechar = "P"
		charnum = 80
		mracename = "Giant"
		msex = 2
	-- 'R' Reptile.
	elseif (mrace == 30) then
		
		mracechar = "R"
		charnum = 82
		randval = lua_randint(5)
		if (randval == 1) then mracename = "Lizard"
		elseif (randval == 2) then mracename = "Dinosaur"
		elseif (randval == 3) then mracename = "Gator"
		elseif (randval == 4) then mracename = "Crocodile"
		else mracename = "Reptile" end
		msex = 2
	-- 'S' Spider.
	elseif (mrace == 31) then
		
		mracechar = "S"
		charnum = 83
		randval = lua_randint(2)
		if (randval == 1) then mracename = "Spider"
		else mracename = "Arachnid" end
		msex = 2
	-- 'W' Wight.
	elseif (mrace == 32) then
		
		mracechar = "W"
		charnum = 87
		randval = lua_randint(3)
		if (randval == 1) then mracename = "Wight"
		elseif (randval == 2) then mracename = "Wraith"
		else mracename = "Specter" end
		msex = 2
	-- 'Y' Yeti.
	elseif (mrace == 33) then
		
		mracechar = "Y"
		charnum = 89
		randval = lua_randint(2)
		if (randval == 1) then mracename = "Yeti"
		else mracename = "Sasquatch" end
		msex = 2
	-- 'Z' Hound.
	elseif (mrace == 34) then
		
		mracechar = "Z"
		charnum = 90
		mracename = "Hound"
		msex = 2
	-- '*' Sphere.
	elseif (mrace == 35) then
		
		mracechar = "*"
		charnum = 42
		randval = lua_randint(4)
		if (randval == 1) then mracename = "Sphere"
		elseif (randval == 2) then mracename = "Ball"
		elseif (randval == 3) then mracename = "Globe"
		else mracename = "Orb" end
		msex = 2
	-- '&' Devling.
	elseif (mrace == 36) then
		
		mracechar = "&"
		charnum = 38
		mracename = "Devling"
		msex = 2
	-- Default(should not happen)
	else
		mracechar = "?"
		charnum = 63
		mracename = "Unknown"
		msex = 2
	end

	-- The Flow boss is treated differently.
	if (mtype == 2) then
		mracechar = "@"
		charnum = 64
		mracename = "Flow Boss"
		msex = 2
	end
	-- Townsfolks are "t".
	if (mtype == 3) then
		mracechar = "t"
		charnum = 116
		mracename = "Townsfolk"
		msex = lua_randint(2) - 1
	end

	-- We want a dragon.
	if (mtype == 4) then

		if (rank >= 70) then
			mracechar = "D"
			charnum = 68
			mracename = "Ancient Dragon"
			msex = 2
		else
			mracechar = "d"
			charnum = 100
			mracename = "Dragon"
			msex = 2
		end
	end

	-- Skeleton Lord is a Skeleton.
	if (mtype == 5) then

		mracechar = "s"
		charnum = 115
		mracename = "Skeleton"
		msex = 2
	end

	-- Assign base char.
	m_race(r_idx).d_char = charnum
	m_race(r_idx).x_char = charnum

	-- Generate a name for this monster.

	-- Normal monsters.
	if (mtype == 0 or mtype == 4) then
		
		local ntype
		local string1 = ""
		local string2 = ""
		local string3 = ""

		ntype = lua_randint(3)

		if (ntype == 1) then

			string1 = get_monster_name_syllab_1()
			string2 = get_monster_name_syllab_2()
			m_race(r_idx).name_char = string.format('%s%s %s', string1, string2, mracename)
		elseif (ntype == 2) then

			string1 = get_monster_name_syllab_1()
			string2 = get_monster_name_syllab_2()
			string3 = get_monster_adjective()
			m_race(r_idx).name_char = string.format('%s %s%s %s', string3, string1, string2, mracename)
		else
			string1 = get_monster_adjective()
			m_race(r_idx).name_char = string.format('%s %s', string1, mracename)
		end

	-- Unique/Dungeon Boss
	elseif (mtype == 1) then

		local ntype
		local string1 = ""
		local string2 = ""
		local string3 = ""

		ntype = lua_randint(3)

		if (ntype == 1) then

			string1 = get_monster_name_syllab_1()
			string2 = get_monster_name_syllab_2()
			string3 = get_monster_adjective()

			m_race(r_idx).name_char = string.format('%s%s The %s %s', string1, string2, string3, mracename)
		elseif (ntype == 2) then

			string1 = get_monster_name_syllab_1()
			string2 = get_monster_name_syllab_2()
			m_race(r_idx).name_char = string.format('%s%s', string1, string2)
		else
			string1 = get_monster_name_syllab_1()
			string2 = get_monster_name_syllab_2()
			m_race(r_idx).name_char = string.format('%s%s The %s', string1, string2, mracename)
		end
	-- Flow boss.
	elseif (mtype == 2) then

		-- The goal was to name it "Boss of Item".
		-- There was no easy at the moment of doing this in lua only, so I've made this source function.
		-- Will eventually be replaced if I get a way of working confortably with item names in lua.
		boss_of_global_object(2098)
	-- Townsfolk.
	elseif (mtype == 3) then

		m_race(r_idx).name_char = string.format('Townsfolk')
	-- Skeleton Lord.
	elseif (mtype == 5) then

		local ntype
		local string1 = ""
		local string2 = ""
		local string3 = ""

		string1 = get_monster_name_syllab_1()
		string2 = get_monster_name_syllab_2()
		m_race(r_idx).name_char = string.format('%s%s The Skeleton Lord', string1, string2)
	end

	-- Give the monster a color.
	if (mtype == 2) then

		if (get_object_flag4(global_object, TR4_CRAFTED)) then rndcolor = TERM_ORANGE
		elseif (global_object.name1 > 0) then rndcolor = TERM_YELLOW
		elseif (global_object.name2 > 0) then rndcolor = TERM_L_GREEN
		elseif (global_object.cursed > 0) then rndcolor = TERM_L_RED
		elseif (get_object_flag4(global_object, TR4_LEVELS)) then rndcolor = TERM_BLUE
		elseif (get_object_flag4(global_object, TR4_ENCHANTED)) then rndcolor = TERM_VIOLET
		else rndcolor = kind(global_object).x_attr end
	else
		rndcolor = lua_randint(14) + 1
	end
	m_race(r_idx).d_attr = rndcolor
	m_race(r_idx).x_attr = rndcolor

	-- Determine what the monster's "type" will be.
	-- In other, what will this monster be optimized for.
	-- 1. Melee
	-- 2. Ranged
	-- 3. Magic
	-- 4. Melee/Magic
	-- 5. Melee/Ranged
	-- 6. Ranged/Magic
	-- 7+. Random
	monstertype = lua_randint(10)

	-- Depth and rarity.
	m_race(r_idx).level = rank
	m_race(r_idx).rarity = 1

	-- Assume not asleep and 20 vision.
	m_race(r_idx).sleep = 0
	m_race(r_idx).aaf = 20

	-- Determine hp.
	if (mtype == 0) then

		m_race(r_idx).hdice = lua_randint(rank) + (rank / 2) + (rank / 10)
		m_race(r_idx).hdice = m_race(r_idx).hdice + multiply_divide(m_race(r_idx).hdice, (rank * 4), 100)
		if (m_race(r_idx).hdice < 1) then m_race(r_idx).hdice = 1 end

		m_race(r_idx).hside = lua_randint(rank) + (rank / 2) + (rank / 10)
		m_race(r_idx).hside = m_race(r_idx).hside + multiply_divide(m_race(r_idx).hside, (rank * 4), 100)
		if (m_race(r_idx).hside < 1) then m_race(r_idx).hside = 1 end
	else
		m_race(r_idx).hdice = rank + (rank / 2) + (rank / 10)
		m_race(r_idx).hdice = m_race(r_idx).hdice + multiply_divide(m_race(r_idx).hdice, (rank * 6), 100)
		if (m_race(r_idx).hdice < 1) then m_race(r_idx).hdice = 1 end

		m_race(r_idx).hside = rank + (rank / 2) + (rank / 10)
		m_race(r_idx).hside = m_race(r_idx).hside + multiply_divide(m_race(r_idx).hside, (rank * 6), 100)
		if (m_race(r_idx).hside < 1) then m_race(r_idx).hside = 1 end
	end

	-- Determine stats points and skill points.
	-- They will be assigned to monster's stats and skills.
	-- How they are spread is determined by the monster's type.
	statspoints = multiply_divide(rank, (rank * 10), 100)
	if (statspoints < 0) then statspoints = 1 end

	-- Melee monsters.
	if (monstertype == 1) then

		strchance = 70
		dexchance = 25
		mindchance = 5
		attackchance = 100
		rangedchance = 0
		magicchance = 0

		skillmult = rank * 3
		if (skillmult > 100) then skillmult = 100 end
		skillpoints = multiply_divide(rank / 3, skillmult, 100)
		if (skillpoints <= 0) then skillpoints = 1 end
	-- Ranged monsters.
	elseif (monstertype == 2) then

		strchance = 10
		dexchance = 80
		mindchance = 10
		attackchance = 0
		rangedchance = 100
		magicchance = 0

		skillmult = rank * 3
		if (skillmult > 100) then skillmult = 100 end
		skillpoints = multiply_divide(rank / 3, skillmult, 100)
		if (skillpoints <= 0) then skillpoints = 1 end
	-- Magic monsters.
	elseif (monstertype == 3) then

		strchance = 10
		dexchance = 10
		mindchance = 80
		attackchance = 0
		rangedchance = 0
		magicchance = 100

		skillmult = rank * 3
		if (skillmult > 100) then skillmult = 100 end
		skillpoints = multiply_divide(rank / 3, skillmult, 100)
		if (skillpoints <= 0) then skillpoints = 1 end
	-- Melee/Magic
	elseif (monstertype == 4) then

		strchance = 40
		dexchance = 20
		mindchance = 40
		attackchance = 50
		rangedchance = 0
		magicchance = 50

		skillmult = rank * 3
		if (skillmult > 100) then skillmult = 100 end
		skillpoints = multiply_divide(rank / 2, skillmult, 100)
		if (skillpoints <= 0) then skillpoints = 1 end
	-- Melee/Ranged
	elseif (monstertype == 5) then

		strchance = 45
		dexchance = 45
		mindchance = 10
		attackchance = 50
		rangedchance = 50
		magicchance = 0

		skillmult = rank * 3
		if (skillmult > 100) then skillmult = 100 end
		skillpoints = multiply_divide(rank / 2, skillmult, 100)
		if (skillpoints <= 0) then skillpoints = 1 end
	-- Ranged/Magic
	elseif (monstertype == 6) then

		strchance = 10
		dexchance = 45
		mindchance = 45
		attackchance = 0
		rangedchance = 50
		magicchance = 50

		skillmult = rank * 3
		if (skillmult > 100) then skillmult = 100 end
		skillpoints = multiply_divide(rank / 2, skillmult, 100)
		if (skillpoints <= 0) then skillpoints = 1 end
	-- Random
	else

		strchance = 33
		dexchance = 33
		mindchance = 33
		attackchance = 33
		rangedchance = 33
		magicchance = 33

		skillmult = rank * 3
		if (skillmult > 100) then skillmult = 100 end
		skillpoints = multiply_divide(rank, skillmult, 100)
		if (skillpoints <= 0) then skillpoints = 1 end
	end

	-- Basic stats.
	m_race(r_idx).str = 5
	m_race(r_idx).dex = 5
	m_race(r_idx).mind = 5
	m_race(r_idx).skill_attack = 0
	m_race(r_idx).skill_ranged = 0
	m_race(r_idx).skill_magic = 0

	-- Increase stats.
	i = 0
	while (i < statspoints) do

		local strpercent
		local dexpercent
		local mindpercent
		local statspower
		local roll

		statspower = strchance + dexchance + mindchance

		strpercent = multiply_divide(strchance, 100, statspower)
		dexpercent = multiply_divide(dexchance, 100, statspower)
		mindpercent = multiply_divide(mindchance, 100, statspower)

		roll = lua_randint(100)

		if (roll >= (100 - strpercent)) then

			m_race(r_idx).str = m_race(r_idx).str + 1

		elseif (roll >= (100 - strpercent - dexpercent)) then

			m_race(r_idx).dex = m_race(r_idx).dex + 1
		else

			m_race(r_idx).mind = m_race(r_idx).mind + 1
		end

		i = i + 1

	end
	-- Increase skills.
	i = 0
	while (i < skillpoints) do

		local attackpercent
		local rangedpercent
		local magicpercent
		local skillspower
		local roll

		skillspower = attackchance + rangedchance + magicchance

		attackpercent = multiply_divide(attackchance, 100, skillspower)
		rangedpercent = multiply_divide(rangedchance, 100, skillspower)
		magicpercent = multiply_divide(magicchance, 100, skillspower)

		roll = lua_randint(100)

		if (roll >= (100 - attackpercent)) then

			m_race(r_idx).skill_attack = m_race(r_idx).skill_attack + 1

		elseif (roll >= (100 - attackpercent - rangedpercent)) then

			m_race(r_idx).skill_ranged = m_race(r_idx).skill_ranged + 1
		else

			m_race(r_idx).skill_magic = m_race(r_idx).skill_magic + 1
		end

		i = i + 1

	end

	-- Base AC
	m_race(r_idx).ac = (lua_randint(rank) + (rank / 2)) * rank
	-- Dungeon bosses have a bit more.
	if (mtype == 1 or mtype == 2) then m_race(r_idx).ac = m_race(r_idx).ac + multiply_divide(m_race(r_idx).ac, (rank * 2), 100) end

	-- Base Speed
	speedbonus = 110 + (lua_randint(rank * 2) - 6)
	if (speedbonus > 160) then speedbonus = 160 end
	if ((rank >= 10) and (speedbonus < 110)) then speedbonus = 110 end

	-- We want dungeon bosses to be fairly fast, save for early ones.
	if ((mtype == 1 or mtype == 2) and speedbonus < 120 and rank >= 10) then speedbonus = 120 end

	m_race(r_idx).speed = speedbonus

	-- Attacks and spells.
	if (monstertype == 3) then m_race(r_idx).attacks = 0
	elseif (monstertype == 2 or monstertype == 6) then m_race(r_idx).attacks = 1
	else
		if (rank >= 10) then	
			m_race(r_idx).attacks = lua_randint(rank / 10) + lua_randint(2)
		else
			m_race(r_idx).attacks = lua_randint(2)
		end
	end

	if (monstertype == 1 or monstertype == 2 or monstertype == 5) then
		m_race(r_idx).spells = 0
		m_race(r_idx).spellchance = 0
	elseif (not(monstertype == 3)) then

		if (rank >= 40) then
			m_race(r_idx).spells = lua_randint(rank / 40) + 1
		else
			m_race(r_idx).spells = 1
		end
		m_race(r_idx).spellchance = lua_randint(40) + 10
	else
		if (rank >= 20) then
			m_race(r_idx).spells = lua_randint(rank / 20) + 1
		else
			m_race(r_idx).spells = 1
		end
		m_race(r_idx).spellchance = 100
	end

	-- Clear attacks and spells.
	for i = 0, 19 do

		m_race(r_idx).attack[i+1].name = ""
		m_race(r_idx).attack[i+1].act = ""
		m_race(r_idx).attack[i+1].type = 0
		m_race(r_idx).attack[i+1].effect = 0
		m_race(r_idx).attack[i+1].ddice = 0
		m_race(r_idx).attack[i+1].dside = 0
		m_race(r_idx).attack[i+1].element = 0
		m_race(r_idx).attack[i+1].special1 = 0
		m_race(r_idx).attack[i+1].special2 = 0
	end
	for i = 0, 19 do

		m_race(r_idx).spell[i+1].name = ""
		m_race(r_idx).spell[i+1].act = ""
		m_race(r_idx).spell[i+1].type = 0
		m_race(r_idx).spell[i+1].power = 0
		m_race(r_idx).spell[i+1].special1 = 0
		m_race(r_idx).spell[i+1].special2 = 0
		m_race(r_idx).spell[i+1].special3 = 0
		m_race(r_idx).spell[i+1].summchar = 0
		m_race(r_idx).spell[i+1].cost = 0
	end

	-- Melee attacks.

	-- We generate some melee/ranged attacks.
	if (monstertype == 3) then i = 0
	else i = lua_randint(4) end

	j = 0
	while (j < i) do

		local element
		local attname
		local attype
		local rangedchance
		local ddice
		local dside

		element = lua_randint(23)

		-- 1. Melee.
		-- 2. Ranged.
		attype = lua_randint(2)
		if (attype == 2) then attype = 3 end

		-- Ranged and ranged/magic always get a ranged, and at 100%.
		if (monstertype == 2 or monstertype == 6) then

			attype = 3
			rangedchance = 100
		else

			-- The first attack is always melee.
			if (j == 0) then attype = 1
			else
				if (attype == 3) then rangedchance = lua_randint(60) + 15 end
			end
		end

		m_race(r_idx).attack[j+1].type = attype
		if (attype == 3) then m_race(r_idx).attack[j+1].special2 = rangedchance end

		-- Chance to use a ranged attack.

		-- Some attack types are forbidden.
		-- This also result in more chances of getting Physical, which is fine.
		if (element == GF_RADIO or element == GF_CHAOS or element == GF_TOXIC) then element = GF_PHYSICAL end

		-- Depending on the monster, let's vary the attacks names a bit! :)
		if (attype == 1) then

			if (mracechar == "p" or mracechar == "h" or mracechar == "A" or mracechar == "u" or mracechar == "P" or mracechar == "s" or mracechar == "n" or mracechar == "k" or mracechar == "&") then

				local aname
				aname = lua_randint(6)

				if (aname == 1) then

					if (lua_randint(2) == 1) then m_race(r_idx).attack[j+1].name = string.format('%s Sword', get_element_name(element))
					else m_race(r_idx).attack[j+1].name = string.format('%s Blade', get_element_name(element)) end
					if (lua_randint(2) == 1) then m_race(r_idx).attack[j+1].act = "slash"
					else m_race(r_idx).attack[j+1].act = "hit" end
				end
				if (aname == 2) then

					if (lua_randint(2) == 1) then m_race(r_idx).attack[j+1].name = string.format('%s Lance', get_element_name(element))
					else m_race(r_idx).attack[j+1].name = string.format('%s Spear', get_element_name(element)) end
					if (lua_randint(2) == 1) then m_race(r_idx).attack[j+1].act = "trust"
					else m_race(r_idx).attack[j+1].act = "hit" end
				end
				if (aname == 3) then

					if (lua_randint(2) == 1) then m_race(r_idx).attack[j+1].name = string.format('%s Staff', get_element_name(element))
					else m_race(r_idx).attack[j+1].name = string.format('%s Mace', get_element_name(element)) end
					if (lua_randint(2) == 1) then m_race(r_idx).attack[j+1].act = "bash"
					else m_race(r_idx).attack[j+1].act = "hit" end
				end
				if (aname == 4) then

					if (lua_randint(2) == 1) then m_race(r_idx).attack[j+1].name = string.format('%s Great Axe', get_element_name(element))
					else m_race(r_idx).attack[j+1].name = string.format('%s Axe', get_element_name(element)) end
					if (lua_randint(2) == 1) then m_race(r_idx).attack[j+1].act = "chop"
					else m_race(r_idx).attack[j+1].act = "hit" end
				end
				if (aname == 5) then

					if (lua_randint(2) == 1) then
						m_race(r_idx).attack[j+1].name = string.format('%s Punch', get_element_name(element))
						m_race(r_idx).attack[j+1].act = "punch"
					else
						m_race(r_idx).attack[j+1].name = string.format('%s Kick', get_element_name(element))
						m_race(r_idx).attack[j+1].act = "kick"
					end
				end
				if (aname == 6) then

					m_race(r_idx).attack[j+1].name = "!"
					m_race(r_idx).attack[j+1].act = "hit"
				end
			elseif (mracechar == "q" or mracechar == "d" or mracechar == "f" or mracechar == "C" or mracechar == "Z") then

				local aname
				aname = lua_randint(4)

				if (aname == 1) then

					m_race(r_idx).attack[j+1].name = string.format('%s Claw', get_element_name(element))
					if (lua_randint(2) == 1) then m_race(r_idx).attack[j+1].act = "claw"
					else m_race(r_idx).attack[j+1].act = "hit" end
				end
				if (aname == 2) then

					if (lua_randint(2) == 1) then m_race(r_idx).attack[j+1].name = string.format('%s Bite', get_element_name(element))
					else m_race(r_idx).attack[j+1].name = string.format('%s Fangs', get_element_name(element)) end
					m_race(r_idx).attack[j+1].act = "bite"
				end
				if (aname == 3) then

					m_race(r_idx).attack[j+1].name = string.format('%s Tail', get_element_name(element))
					m_race(r_idx).attack[j+1].act = "hit"
				end
				if (aname == 4) then

					m_race(r_idx).attack[j+1].name = "!"
					m_race(r_idx).attack[j+1].act = "hit"
				end

			elseif (mracechar == "g" or mracechar == "H") then

				local aname
				aname = lua_randint(3)

				if (aname == 1) then

					m_race(r_idx).attack[j+1].name = string.format('%s Punch', get_element_name(element))
					if (lua_randint(2) == 1) then m_race(r_idx).attack[j+1].act = "punch"
					else m_race(r_idx).attack[j+1].act = "hit" end
				end
				if (aname == 2) then

					m_race(r_idx).attack[j+1].name = string.format('%s Crush', get_element_name(element))
					m_race(r_idx).attack[j+1].act = "crush"
				end
				if (aname == 3) then

					m_race(r_idx).attack[j+1].name = "!"
					m_race(r_idx).attack[j+1].act = "hit"
				end
			elseif (mracechar == "S" or mracechar == "I") then

				local aname
				aname = lua_randint(3)

				if (aname == 1) then

					m_race(r_idx).attack[j+1].name = string.format('%s Bite', get_element_name(element))
					m_race(r_idx).attack[j+1].act = "bite"
				end
				if (aname == 2) then

					m_race(r_idx).attack[j+1].name = string.format('%s Sting', get_element_name(element))
					m_race(r_idx).attack[j+1].act = "sting"
				end
				if (aname == 3) then

					m_race(r_idx).attack[j+1].name = "!"
					m_race(r_idx).attack[j+1].act = "hit"
				end
			elseif (mracechar == "G" or mracechar == "E") then

				local aname
				aname = lua_randint(2)

				if (aname == 1) then

					m_race(r_idx).attack[j+1].name = string.format('%s Hand', get_element_name(element))
					m_race(r_idx).attack[j+1].act = "touch"
				end
				if (aname == 2) then

					m_race(r_idx).attack[j+1].name = "!"
					m_race(r_idx).attack[j+1].act = "hit"
				end
			else
				m_race(r_idx).attack[j+1].name = "!"
				m_race(r_idx).attack[j+1].act = "hit"
			end
		else

			if (mracechar == "p" or mracechar == "h" or mracechar == "A" or mracechar == "u" or mracechar == "P" or mracechar == "s" or mracechar == "n" or mracechar == "k" or mracechar == "&") then

				local aname
				aname = lua_randint(3)

				if (aname == 1) then

					m_race(r_idx).attack[j+1].name = string.format('%s Arrow', get_element_name(element))
					m_race(r_idx).attack[j+1].act = "shoot"
				end
				if (aname == 2) then

					m_race(r_idx).attack[j+1].name = string.format('%s Bolt', get_element_name(element))
					m_race(r_idx).attack[j+1].act = "shoot"
				end
				if (aname == 3) then

					m_race(r_idx).attack[j+1].name = string.format('%s Shot', get_element_name(element))
					m_race(r_idx).attack[j+1].act = "shoot"
				end
			else
				m_race(r_idx).attack[j+1].name = string.format('%s Attack', get_element_name(element))
				m_race(r_idx).attack[j+1].act = "shoot"
			end
		end

		-- Assign the element.
		m_race(r_idx).attack[j+1].element = element

		-- Damages.
		ddice = lua_randint(rank / 7) + 1
		dside = lua_randint(rank / 5) + 3

		m_race(r_idx).attack[j+1].ddice = ddice
		m_race(r_idx).attack[j+1].dside = dside

		-- Next attack.
		j = j + 1
	end

	-- Generate some spells.
	if (monstertype == 1 or monstertype == 2 or monstertype == 5) then i = 0
	elseif (not(monstertype == 3)) then i = lua_randint(4 + (rank / 60))
	else i = lua_randint(4 + (rank / 30)) + 1 end

	j = 0
	while (j < i) do

		local stype

		-- Choose a spell's type.
		stype = lua_randint(5)

		-- Magic-only monsters must have at least one offensive spell.
		if (monstertype == 3 and j == 0) then stype = lua_randint(2) end

		-- 1 and 2. Bolt/Ball spell.
		if (stype == 1 or stype == 2) then

			local power
			local rad
			local element
			local cost

			element = 0
			while ((element == 0 or element == GF_RADIO or element == GF_CHAOS or element == GF_TOXIC) or (element == GF_MANA and rank < 50)) do

				element = lua_randint(23)
			end

			-- Power
			power = lua_randint(rank / 2) + (rank / 4) + 5

			-- Radius
			if (stype == 2) then

				rad = lua_randint(rank / 20) + 2
			else
				rad = 0
			end

			-- Cost.
			cost = power - 3

			-- Generate a name.
			if (stype == 1) then m_race(r_idx).spell[j+1].name = string.format('%s Bolt', get_element_name(element))
			else m_race(r_idx).spell[j+1].name = string.format('%s Ball', get_element_name(element)) end

			-- Build the spell.
			m_race(r_idx).spell[j+1].act = "cast"
			m_race(r_idx).spell[j+1].type = stype
			m_race(r_idx).spell[j+1].power = power
			m_race(r_idx).spell[j+1].special1 = element
			m_race(r_idx).spell[j+1].special2 = rad
			m_race(r_idx).spell[j+1].cost = cost
		end
		-- 3. Summon Kind
		if (stype == 3) then

			local schar
			local summ
			local power
			local num
			local duration
			local cost

			summ = ""

			schar = lua_randint(33)
			if (schar == 1) then
				summ = "Ant"
				m_race(r_idx).spell[j+1].summchar = 97
			end
			if (schar == 2) then
				summ = "Bat"
				m_race(r_idx).spell[j+1].summchar = 98
			end
			if (schar == 3) then
				summ = "Dragon"
				m_race(r_idx).spell[j+1].summchar = 100
			end
			if (schar == 4) then
				summ = "Eye"
				m_race(r_idx).spell[j+1].summchar = 101
			end
			if (schar == 5) then
				summ = "Feline"
				m_race(r_idx).spell[j+1].summchar = 102
			end
			if (schar == 6) then
				summ = "Golem"
				m_race(r_idx).spell[j+1].summchar = 103
			end
			if (schar == 7) then
				summ = "Humanoid"
				m_race(r_idx).spell[j+1].summchar = 104
			end
			if (schar == 8) then
				summ = "Slime"
				m_race(r_idx).spell[j+1].summchar = 106
			end
			if (schar == 9) then
				summ = "Kobold"
				m_race(r_idx).spell[j+1].summchar = 107
			end
			if (schar == 10) then
				summ = "Mold"
				m_race(r_idx).spell[j+1].summchar = 109
			end
			if (schar == 11) then
				summ = "Naga"
				m_race(r_idx).spell[j+1].summchar = 110
			end
			if (schar == 12) then
				summ = "Human"
				m_race(r_idx).spell[j+1].summchar = 112
			end
			if (schar == 13) then
				summ = "Quadroped"
				m_race(r_idx).spell[j+1].summchar = 113
			end
			if (schar == 14) then
				summ = "Rodent"
				m_race(r_idx).spell[j+1].summchar = 114
			end
			if (schar == 15) then
				summ = "Skeleton"
				m_race(r_idx).spell[j+1].summchar = 115
			end
			if (schar == 16) then
				summ = "Demon"
				m_race(r_idx).spell[j+1].summchar = 117
			end
			if (schar == 17) then
				summ = "Worm"
				m_race(r_idx).spell[j+1].summchar = 119
			end
			if (schar == 18) then
				summ = "Zombie"
				m_race(r_idx).spell[j+1].summchar = 122
			end
			if (schar == 19) then
				summ = "Bird"
				m_race(r_idx).spell[j+1].summchar = 66
			end
			if (schar == 20) then
				summ = "Canine"
				m_race(r_idx).spell[j+1].summchar = 67
			end
			if (schar == 21) then
				summ = "Spirit"
				m_race(r_idx).spell[j+1].summchar = 69
			end
			if (schar == 22) then
				summ = "Ghost"
				m_race(r_idx).spell[j+1].summchar = 71
			end
			if (schar == 23) then
				summ = "Hybrid"
				m_race(r_idx).spell[j+1].summchar = 72
			end
			if (schar == 24) then
				summ = "Insect"
				m_race(r_idx).spell[j+1].summchar = 73
			end
			if (schar == 25) then
				summ = "Snake"
				m_race(r_idx).spell[j+1].summchar = 74
			end
			if (schar == 26) then
				summ = "Hydra"
				m_race(r_idx).spell[j+1].summchar = 77
			end
			if (schar == 27) then
				summ = "Giant"
				m_race(r_idx).spell[j+1].summchar = 80
			end
			if (schar == 28) then
				summ = "Reptile"
				m_race(r_idx).spell[j+1].summchar = 82
			end
			if (schar == 29) then
				summ = "Spider"
				m_race(r_idx).spell[j+1].summchar = 83
			end
			if (schar == 30) then
				summ = "Wight"
				m_race(r_idx).spell[j+1].summchar = 87
			end
			if (schar == 31) then
				summ = "Hound"
				m_race(r_idx).spell[j+1].summchar = 90
			end
			if (schar == 32) then
				summ = "Sphere"
				m_race(r_idx).spell[j+1].summchar = 42
			end
			if (schar == 33) then
				summ = "Devling"
				m_race(r_idx).spell[j+1].summchar = 38
			end

			-- Power.
			power = lua_randint(rank / 2) + (rank / 2) + 1

			-- Number.
			num = lua_randint(rank / 5) + 1
			if (num > 15) then num = 15 end

			-- Cost.
			cost = (power * num) * 2

			-- Duration.
			duration = lua_randint(rank / 2) + 5

			-- Generate a name.
			if (num > 1) then m_race(r_idx).spell[j+1].name = string.format('Summon %ss', summ)
			else m_race(r_idx).spell[j+1].name = string.format('Summon %s', summ) end

			-- Build the spell.
			m_race(r_idx).spell[j+1].act = "cast"
			m_race(r_idx).spell[j+1].type = 6
			m_race(r_idx).spell[j+1].power = power
			m_race(r_idx).spell[j+1].special1 = num
			m_race(r_idx).spell[j+1].special2 = duration
			m_race(r_idx).spell[j+1].cost = cost
		end
		-- 4. Summon Specific
		if (stype == 4) then

			local which
			local chosen
			local power
			local num
			local duration
			local cost

			chosen = 0

			while (chosen == 0) do

				-- What it should be: which = lua_randint(max_r_idx)
				-- However, I do not want random monsters to be selected.
				-- Let's leave it at regular enemies.
				which = lua_randint(1000)

				-- No UNIQUE, SPECIAL_GENE or Nightmares.
				if (m_race(which).level > 0 and m_race(which).level <= rank and not(get_monster_flag1(which, RF1_UNIQUE)) and not(get_monster_flag9(which, RF9_SPECIAL_GENE)) and m_race(which).cursed == 0) then

					chosen = 1
				end
			end

			power = which

			-- Number.
			num = lua_randint(rank / 5) + 1
			if (num > 15) then num = 15 end

			-- Cost.
			cost = (m_race(power).level * num) * 2

			-- Duration.
			duration = lua_randint(rank / 2) + 5

			-- Generate a name.
			if (num > 1) then m_race(r_idx).spell[j+1].name = string.format('Summon %ss', m_race(power).name_char)
			else m_race(r_idx).spell[j+1].name = string.format('Summon %s', m_race(power).name_char) end

			-- Build the spell.
			m_race(r_idx).spell[j+1].act = "cast"
			m_race(r_idx).spell[j+1].type = 7
			m_race(r_idx).spell[j+1].power = power
			m_race(r_idx).spell[j+1].special1 = num
			m_race(r_idx).spell[j+1].special2 = duration
			m_race(r_idx).spell[j+1].cost = cost
		end
		-- 5. Teleport.
		if (stype == 5) then

			local power
			local cost

			-- Power.
			power = lua_randint(rank / 2) + 3

			-- Cost.
			cost = power

			-- Generate a name.
			if (power >= 50) then m_race(r_idx).spell[j+1].name = "Teleportation"
			elseif (power >= 25) then m_race(r_idx).spell[j+1].name = "Teleport Away"
			elseif (power >= 10) then m_race(r_idx).spell[j+1].name = "Phase Door"
			else m_race(r_idx).spell[j+1].name = "Blink Away" end

			-- Build the spell.
			m_race(r_idx).spell[j+1].act = "cast"
			m_race(r_idx).spell[j+1].type = 8
			m_race(r_idx).spell[j+1].power = power
			m_race(r_idx).spell[j+1].cost = cost
		end

		-- Next spell.
		j = j + 1
	end

	-- Clear resistances.
	for i = 0, (MAX_RESIST-1) do
		m_race(r_idx).resistances[i+1] = 0
	end

	-- Resistances!
	-- For each "pass" through the loop, we increase a resistance by 25.
	i = lua_randint(rank / 5)

	-- No more than 50 passes.
	if (i > 50) then i = 50 end

	j = 0
	while (j < i) do

		local which

		which = lua_randint(16)

		if (m_race(r_idx).resistances[which+1] < 100) then

			m_race(r_idx).resistances[which+1] = m_race(r_idx).resistances[which+1] + 25
			j = j + 1
		end
	end

	-- Counters.
	if (rank >= 30) then
		m_race(r_idx).countertype = 19
		m_race(r_idx).counterchance = 100
	elseif (rank >= 10) then

		local which
		which = lua_randint(4)
		if (which == 1) then
			m_race(r_idx).countertype = 3
			m_race(r_idx).counterchance = 100
		end
		if (which == 2) then
			m_race(r_idx).countertype = 17
			m_race(r_idx).counterchance = 100
		end
		if (which == 3) then
			m_race(r_idx).countertype = 18
			m_race(r_idx).counterchance = 100
		end
		if (which == 4) then
			m_race(r_idx).countertype = 19
			m_race(r_idx).counterchance = 100
		end
	else
		local which
		which = lua_randint(6)
		if (which == 1) then
			m_race(r_idx).countertype = 1
			m_race(r_idx).counterchance = 100
		end
		if (which == 2) then
			m_race(r_idx).countertype = 2
			m_race(r_idx).counterchance = 100
		end
		if (which == 3) then
			m_race(r_idx).countertype = 16
			m_race(r_idx).counterchance = 100
		end
		if (which >= 4) then
			m_race(r_idx).countertype = 0
			m_race(r_idx).counterchance = 0
		end
	end

	-- Lives.
	if (rank >= 20) then

		local livesmult
		local lives

		lives = rank
		livesmult = rank / 6

		lives = multiply_divide(lives, livesmult, 100)

		if (mtype == 1 or mtype == 2) then
			local lmult2

			lmult2 = lua_randint(100) + 100
			m_race(r_idx).lives = multiply_divide(lives, lmult2, 100)
		else
			local lmult2

			lmult2 = lua_randint(60) + 66
			m_race(r_idx).lives = multiply_divide(lives, lmult2, 100)
		end
	else
		m_race(r_idx).lives = 0
	end

	-- Weight.
	if (mracechar == "g" or mracechar == "P") then

		m_race(r_idx).weight = lua_randint(20000) + 20000
	elseif (mracechar == "k" or mracechar == "&") then

		m_race(r_idx).weight = lua_randint(500) + 600
	elseif (mracechar == "*") then

		m_race(r_idx).weight = 10
	elseif (mracechar == "R" or mracechar == "d" or mracechar == "M" or mracechar == "d") then

		m_race(r_idx).weight = lua_randint(4000) + 2000
	else
		m_race(r_idx).weight = lua_randint(1000) + 1000
	end

	-- Flags

	-- Clear the flags.
	m_race(r_idx).flags1 = 0
	m_race(r_idx).flags2 = 0
	m_race(r_idx).flags3 = 0
	m_race(r_idx).flags4 = 0
	m_race(r_idx).flags5 = 0
	m_race(r_idx).flags6 = 0
	m_race(r_idx).flags7 = 0
	m_race(r_idx).flags8 = 0
	m_race(r_idx).flags9 = 0

	-- They can appear in dungeons.
	give_monster_race_flag8(r_idx, RF8_DUNGEON)

	-- FORCE_MAXHP
	-- Dungeon bosses have it.
	-- Eventually, everyone does.
	if (mtype == 1 or mtype == 2 or mtype == 5) then give_monster_race_flag1(r_idx, RF1_FORCE_MAXHP)
	else
		local fchance
		fchance = rank * 4
		if (fchance > 100) then fchance = 100 end

		if (lua_randint(100) <= fchance) then
			give_monster_race_flag1(r_idx, RF1_FORCE_MAXHP)
		end
	end

	-- Dungeon bosses are uniques.
	-- And SPECIAL_GENE.
	if (mtype == 1 or mtype == 2 or mtype == 5) then
		give_monster_race_flag1(r_idx, RF1_UNIQUE)
		give_monster_race_flag9(r_idx, RF9_SPECIAL_GENE)
	end

	-- All monsters can bash doors.
	-- Humanoid ones can open them too.
	give_monster_race_flag2(r_idx, RF2_BASH_DOOR)
	if (mracechar == "p" or mracechar == "h" or mracechar == "A" or mracechar == "u" or mracechar == "P" or mracechar == "s" or mracechar == "n" or mracechar == "k" or mracechar == "&") then
		give_monster_race_flag2(r_idx, RF2_OPEN_DOOR)
	end

	-- Status resistance flags.
	-- Each of them has 33% chances of showing up.
	-- The Skeleton Lord has them all.
	if (lua_randint(100) <= 33 or mtype == 5) then give_monster_race_flag3(r_idx, RF3_NO_STUN) end
	if (lua_randint(100) <= 33 or mtype == 5) then give_monster_race_flag3(r_idx, RF3_NO_FEAR) end
	if (lua_randint(100) <= 33 or mtype == 5) then give_monster_race_flag3(r_idx, RF3_NO_CONF) end
	if (lua_randint(100) <= 33 or mtype == 5) then give_monster_race_flag3(r_idx, RF3_NO_SLEEP) end

	-- Appear in groups?
	-- Normal monsters only.
	if (mtype == 0 and lua_randint(100) <= 10) then
		give_monster_race_flag1(r_idx, RF1_FRIENDS)
		friends = 1
	end

	-- Drops
	-- Dungeon bosses have great drops.
	if ((mtype == 1 and twisted == 0) or (mtype == 5)) then

		give_monster_race_flag1(r_idx, RF1_ONLY_ITEM)
		give_monster_race_flag1(r_idx, RF1_DROP_GOOD)
		give_monster_race_flag1(r_idx, RF1_DROP_GREAT)
		if (rank >= 100) then

			give_monster_race_flag1(r_idx, RF1_DROP_4D2)
			give_monster_race_flag1(r_idx, RF1_DROP_3D2)
			give_monster_race_flag1(r_idx, RF1_DROP_2D2)
			give_monster_race_flag1(r_idx, RF1_DROP_1D2)
		elseif (rank >= 75) then

			give_monster_race_flag1(r_idx, RF1_DROP_4D2)
			give_monster_race_flag1(r_idx, RF1_DROP_2D2)
			give_monster_race_flag1(r_idx, RF1_DROP_1D2)
		elseif (rank >= 40) then

			give_monster_race_flag1(r_idx, RF1_DROP_3D2)
			give_monster_race_flag1(r_idx, RF1_DROP_1D2)
		elseif (rank >= 20) then

			give_monster_race_flag1(r_idx, RF1_DROP_2D2)
			give_monster_race_flag1(r_idx, RF1_DROP_1D2)
		elseif (rank >= 10) then

			give_monster_race_flag1(r_idx, RF1_DROP_2D2)
		else
			give_monster_race_flag1(r_idx, RF1_DROP_1D2)
		end
	else
		-- Maybe it drops something.
		if (lua_randint(100) >= 50 and not(mtype == 2)) then give_monster_race_flag1(r_idx, RF1_DROP_60) end
	end

	-- Gender flags
	if (msex == 0) then give_monster_race_flag1(r_idx, RF1_FEMALE) end
	if (msex == 1) then give_monster_race_flag1(r_idx, RF1_MALE) end

	-- Race flags
	if (mracechar == "d" or mracechar == "D") then give_monster_race_flag3(r_idx, RF3_DRAGON) end
	if (mracechar == "P") then give_monster_race_flag3(r_idx, RF3_GIANT) end
	if (mracechar == "u") then give_monster_race_flag3(r_idx, RF3_DEMON) end
	if (mracechar == "s" or mracechar == "z" or mracechar == "G" or mracechar == "W" or mracechar == "L") then give_monster_race_flag3(r_idx, RF3_UNDEAD) end

	-- Evil?
	if (not(mtype == 3)) then
		if (mracechar == "u" or mtype == 5) then give_monster_race_flag3(r_idx, RF3_EVIL)
		elseif ((mracechar == "s" or mracechar == "z" or mracechar == "G" or mracechar == "W" or mracechar == "L") and lua_randint(100) <= 80) then give_monster_race_flag3(r_idx, RF3_EVIL)
		else
			if (lua_randint(100) <= 30) then give_monster_race_flag3(r_idx, RF3_EVIL) end
		end
	end

	-- Let's make it so that plants don't move.
	if (mracechar == "m") then give_monster_race_flag1(r_idx, RF1_NEVER_MOVE) end

	-- Eyes might also have this.
	if (mracechar == "e" and lua_randint(100) >= 50) then give_monster_race_flag1(r_idx, RF1_NEVER_MOVE) end

	-- Some monster should be able to fly.
	if (mracechar == "B" or mracechar == "d" or mracechar == "A" or mracechar == "e" or mracechar == "b") then give_monster_race_flag7(r_idx, RF7_CAN_FLY) end

	-- Some demons and spirits can fly.
	if ((mracechar == "u" or mracechar == "E") and lua_randint(100) >= 50) then give_monster_race_flag7(r_idx, RF7_CAN_FLY) end

	-- Move through walls for Ghosts.
	-- Random dungeon bosses will never get it. Otherwise, they would move through the locked door,
	-- and we don't want that.
	if (not(mtype == 1)) then

		if (mracechar == "G") then give_monster_race_flag2(r_idx, RF2_PASS_WALL) end

		-- Spirits can have it sometimes.
		if (mracechar == "E" and lua_randint(100) >= 50) then give_monster_race_flag2(r_idx, RF2_PASS_WALL) end
	end

	-- It's a random monster.
	give_monster_race_flag7(r_idx, RF7_RANDOM)

	-- Not playable.
	give_monster_race_flag7(r_idx, RF7_UNPLAYABLE)

	-- Townsfolks gets some more flags.
	if (mtype == 3) then
		give_monster_race_flag7(r_idx, RF7_FRIENDLY)
		give_monster_race_flag7(r_idx, RF7_PET)
		give_monster_race_flag7(r_idx, RF7_TOWNSFOLK)
	end

	-- Events codes, etc...
	m_race(r_idx).treasuretval = 0
	m_race(r_idx).treasuresval = 0
	m_race(r_idx).treasurechance = 0
	m_race(r_idx).treasuremagic = 0
	m_race(r_idx).event = 0
	m_race(r_idx).extra1 = 0
	m_race(r_idx).extra2 = 0
	m_race(r_idx).fixedlevel = 0
	m_race(r_idx).townnum = 0
	m_race(r_idx).dunnum = 0
	m_race(r_idx).cursed = 0
	m_race(r_idx).event_before_melee = 0
	m_race(r_idx).event_after_melee = 0
	m_race(r_idx).event_before_ranged = 0
	m_race(r_idx).event_after_ranged = 0
	m_race(r_idx).event_before_magic = 0
	m_race(r_idx).event_after_magic = 0
	m_race(r_idx).event_before_move = 0
	m_race(r_idx).event_after_move = 0
	m_race(r_idx).event_passive = 0
	m_race(r_idx).event_take_damages = 0
	m_race(r_idx).event_death = 0
	m_race(r_idx).event_spawn = 0
	m_race(r_idx).event_misc = 0

	-- We've never seen this monster, nor killed it.
	-- Reset several "sight" flags.
	-- (Some of them may be obsolete)
	m_race(r_idx).r_sights = 0
	m_race(r_idx).r_deaths = 0
	m_race(r_idx).r_pkills = 0
	m_race(r_idx).r_tkills = 0
	m_race(r_idx).r_wake = 0
	m_race(r_idx).r_ignore = 0
	m_race(r_idx).r_xtra1 = 0
	m_race(r_idx).r_xtra2 = 0
	m_race(r_idx).r_drop_gold = 0
	m_race(r_idx).r_drop_item = 0
	m_race(r_idx).r_cast_inate = 0
	m_race(r_idx).r_cast_spell = 0

	-- Roll for a possible special ability.
	-- This won't happen at first, but then gradually will.
	if (rank >= 40 and not(mtype == 3)) then

		local abchance
		local whichab
		local ablevel

		abchance = (rank / 4) + 1

		if (abchance > 100) then abchance = 100 end

		-- Abilities.
		-- New ones are unlocked as rank progress.

		if (rank >= 50) then ablevel = 3
		elseif (rank >= 40) then ablevel = 2
		else ablevel = 1 end

		whichab = lua_randint(ablevel)

		-- Blink when hit.
		if (whichab == 1) then

			m_race(r_idx).event_take_damages = 9
		end

		-- An aura.
		if (whichab == 2) then

			m_race(r_idx).event_after_move = -1
			m_race(r_idx).event_misc = lua_randint(12) * (-1)
		end

		-- Create fields.
		if (whichab == 3) then

			m_race(r_idx).event_after_move = -2
			m_race(r_idx).event_misc = lua_randint(5) * (-1)
		end
	end

	-- Dungeon bosses have some particularities.
	if (mtype == 1) then
		m_race(r_idx).event = 29999
		m_race(r_idx).extra1 = 1

		-- Wish will come true!
		if (not(p_ptr.events[29014] == 0)) then

			m_race(r_idx).treasuretval = kind(p_ptr.events[29014]).tval
			m_race(r_idx).treasuresval = kind(p_ptr.events[29014]).sval
			m_race(r_idx).treasurechance = 100
			p_ptr.events[29014] = 0
		end

		-- Give it a fixed level to ensure challenge.
		m_race(r_idx).fixedlevel = rank + (rank / 2)

		-- Make sure we've never killed this monster.
		m_race(r_idx).cur_num = 0
		m_race(r_idx).max_num = 1
	end
	-- So does flow bosses.
	if (mtype == 2) then
		
		give_monster_race_flag7(r_idx, RF7_DEATH_DIALOG)

		m_race(r_idx).extra2 = 38

		-- Make sure we've never killed this monster.
		m_race(r_idx).cur_num = 0
		m_race(r_idx).max_num = 1
	end
	-- Townsfolks have a dialog.
	-- There are 5 dialogs reserved for townsfolks.
	-- One is assigned randomly. What part of it will be said also depends on events related to
	-- these dialogs.
	if (mtype == 3) then
		local whichdialog

		whichdialog = lua_randint(5)-1
		whichdialog = whichdialog + 29000

		m_race(r_idx).extra2 = whichdialog
	end
	-- Skeleton Lords
	if (mtype == 5) then
		
		-- Give it a fixed level to ensure challenge.
		m_race(r_idx).fixedlevel = rank + (rank / 2)

		-- It will have the "standard" skeleton resistances, no matter what.
		m_race(r_idx).resistances[3] = 100
		m_race(r_idx).resistances[6] = 100
		m_race(r_idx).resistances[7] = -100
		m_race(r_idx).resistances[8] = 100
		m_race(r_idx).resistances[14] = 100
		if (m_race(r_idx).resistances[16] < 25) then m_race(r_idx).resistances[16] = 25 end

		-- Make sure we've never killed this monster.
		m_race(r_idx).cur_num = 0
		m_race(r_idx).max_num = 1
	end

	-- Experience value.
	m_race(r_idx).mexp = rank * 10
	if (mtype == 1 or mtype == 2 or mtype == 5) then m_race(r_idx).mexp = m_race(r_idx).mexp + multiply_divide(m_race(r_idx).mexp, (rank * 5), 100) end
	if (friends == 1) then m_race(r_idx).mexp = (m_race(r_idx).mexp / 3) end

	-- Clear body part.
	m_race(r_idx).body_parts[BODY_WEAPON+1] = 0
        m_race(r_idx).body_parts[BODY_TORSO+1] = 0
        m_race(r_idx).body_parts[BODY_ARMS+1] = 0
        m_race(r_idx).body_parts[BODY_FINGER+1] = 0
        m_race(r_idx).body_parts[BODY_HEAD+1] = 0
        m_race(r_idx).body_parts[BODY_LEGS+1] = 0

	-- Body parts.
	if (mracechar == "p" or mracechar == "h" or mracechar == "A" or mracechar == "P" or mracechar == "s" or mracechar == "z" or mracechar == "W" or mracechar == "k" or mracechar == "&" or mracechar == "t") then

		m_race(r_idx).body_parts[BODY_WEAPON+1] = 1
        	m_race(r_idx).body_parts[BODY_TORSO+1] = 1
        	m_race(r_idx).body_parts[BODY_ARMS+1] = 1
        	m_race(r_idx).body_parts[BODY_FINGER+1] = 2
        	m_race(r_idx).body_parts[BODY_HEAD+1] = 1
        	m_race(r_idx).body_parts[BODY_LEGS+1] = 1
	elseif (mracechar == "n") then

		m_race(r_idx).body_parts[BODY_WEAPON+1] = 1
        	m_race(r_idx).body_parts[BODY_TORSO+1] = 1
        	m_race(r_idx).body_parts[BODY_ARMS+1] = 1
        	m_race(r_idx).body_parts[BODY_FINGER+1] = 2
        	m_race(r_idx).body_parts[BODY_HEAD+1] = 1
        	m_race(r_idx).body_parts[BODY_LEGS+1] = 0
	elseif (mracechar == "u") then

		m_race(r_idx).body_parts[BODY_WEAPON+1] = lua_randint(2) - 1

		if (m_race(r_idx).body_parts[BODY_WEAPON+1] > 0) then
        		m_race(r_idx).body_parts[BODY_TORSO+1] = lua_randint(2) - 1
        		m_race(r_idx).body_parts[BODY_ARMS+1] = 1
        		m_race(r_idx).body_parts[BODY_FINGER+1] = 2
        		m_race(r_idx).body_parts[BODY_HEAD+1] = 1
        		m_race(r_idx).body_parts[BODY_LEGS+1] = lua_randint(2) - 1
		else
			m_race(r_idx).body_parts[BODY_TORSO+1] = 0
        		m_race(r_idx).body_parts[BODY_ARMS+1] = 0
        		m_race(r_idx).body_parts[BODY_FINGER+1] = 0
        		m_race(r_idx).body_parts[BODY_HEAD+1] = 0
        		m_race(r_idx).body_parts[BODY_LEGS+1] = 0
		end
	else
		m_race(r_idx).body_parts[BODY_WEAPON+1] = 0
        	m_race(r_idx).body_parts[BODY_TORSO+1] = 0
        	m_race(r_idx).body_parts[BODY_ARMS+1] = 0
        	m_race(r_idx).body_parts[BODY_FINGER+1] = 0
        	m_race(r_idx).body_parts[BODY_HEAD+1] = 0
        	m_race(r_idx).body_parts[BODY_LEGS+1] = 0
	end

	-- We're done!
	-- Have fun fighting this monster! :)
	
end

function get_monster_name_syllab_1 ()

	local randchoice
	local syllab

        randchoice = lua_randint(100)
        if (randchoice == 1) then syllab = "Ari"
        elseif (randchoice == 2) then syllab = "Lem"
        elseif (randchoice == 3) then syllab = "Sar"
        elseif (randchoice == 4) then syllab = "Bal"
        elseif (randchoice == 5) then syllab = "Ka"
        elseif (randchoice == 6) then syllab = "Cla"
        elseif (randchoice == 7) then syllab = "Shen"
        elseif (randchoice == 8) then syllab = "Ima"
        elseif (randchoice == 9) then syllab = "Zeno"
        elseif (randchoice == 10) then syllab = "Po"
        elseif (randchoice == 11) then syllab = "Bu"
        elseif (randchoice == 12) then syllab = "Ya"
        elseif (randchoice == 13) then syllab = "Yo"
        elseif (randchoice == 14) then syllab = "Fre"
        elseif (randchoice == 15) then syllab = "Upo"
        elseif (randchoice == 16) then syllab = "We"
        elseif (randchoice == 17) then syllab = "Xa"
        elseif (randchoice == 18) then syllab = "Xen"
        elseif (randchoice == 19) then syllab = "Xan"
        elseif (randchoice == 20) then syllab = "To"
        elseif (randchoice == 21) then syllab = "Ter"
        elseif (randchoice == 22) then syllab = "Te"
        elseif (randchoice == 23) then syllab = "Pe"
        elseif (randchoice == 24) then syllab = "Shin"
        elseif (randchoice == 25) then syllab = "Tog"
        elseif (randchoice == 26) then syllab = "Go"
        elseif (randchoice == 27) then syllab = "Ga"
        elseif (randchoice == 28) then syllab = "Gog"
        elseif (randchoice == 29) then syllab = "Gar"
        elseif (randchoice == 30) then syllab = "Mud"
        elseif (randchoice == 31) then syllab = "Mung"
        elseif (randchoice == 32) then syllab = "Ned"
        elseif (randchoice == 33) then syllab = "Na"
        elseif (randchoice == 34) then syllab = "Ne"
        elseif (randchoice == 35) then syllab = "Ni"
        elseif (randchoice == 36) then syllab = "Sho"
        elseif (randchoice == 37) then syllab = "Sor"
        elseif (randchoice == 38) then syllab = "Per"
        elseif (randchoice == 39) then syllab = "Shi"
        elseif (randchoice == 40) then syllab = "Grim"
        elseif (randchoice == 41) then syllab = "Blood"
        elseif (randchoice == 42) then syllab = "Hell"
        elseif (randchoice == 43) then syllab = "Ke"
        elseif (randchoice == 44) then syllab = "Hi"
        elseif (randchoice == 45) then syllab = "Ha"
        elseif (randchoice == 46) then syllab = "He"
        elseif (randchoice == 47) then syllab = "Her"
        elseif (randchoice == 48) then syllab = "Hes"
        elseif (randchoice == 49) then syllab = "Hir"
        elseif (randchoice == 50) then syllab = "Zer"
        elseif (randchoice == 51) then syllab = "Za"
        elseif (randchoice == 52) then syllab = "Tol"
        elseif (randchoice == 53) then syllab = "Kim"
        elseif (randchoice == 54) then syllab = "Ko"
        elseif (randchoice == 55) then syllab = "Ash"
        elseif (randchoice == 56) then syllab = "Ar"
        elseif (randchoice == 57) then syllab = "Bru"
        elseif (randchoice == 58) then syllab = "Bre"
        elseif (randchoice == 59) then syllab = "Fo"
        elseif (randchoice == 60) then syllab = "Fa"
        elseif (randchoice == 61) then syllab = "Fi"
        elseif (randchoice == 62) then syllab = "Vel"
        elseif (randchoice == 63) then syllab = "Nim"
        elseif (randchoice == 64) then syllab = "Vah"
        elseif (randchoice == 65) then syllab = "Vik"
        elseif (randchoice == 66) then syllab = "Dim"
        elseif (randchoice == 67) then syllab = "Dem"
        elseif (randchoice == 68) then syllab = "Dam"
        elseif (randchoice == 69) then syllab = "Spi"
        elseif (randchoice == 70) then syllab = "Fug"
        elseif (randchoice == 71) then syllab = "Bug"
        elseif (randchoice == 72) then syllab = "Wo"
        elseif (randchoice == 73) then syllab = "Wa"
        elseif (randchoice == 74) then syllab = "Wi"
        elseif (randchoice == 75) then syllab = "Vok"
        elseif (randchoice == 76) then syllab = "Iy"
        elseif (randchoice == 77) then syllab = "Ra"
        elseif (randchoice == 78) then syllab = "Re"
        elseif (randchoice == 79) then syllab = "Ri"
        elseif (randchoice == 80) then syllab = "Ro"
        elseif (randchoice == 81) then syllab = "Jo"
        elseif (randchoice == 82) then syllab = "Je"
        elseif (randchoice == 83) then syllab = "Ji"
        elseif (randchoice == 84) then syllab = "Ja"
        elseif (randchoice == 85) then syllab = "Tet"
        elseif (randchoice == 86) then syllab = "Tez"
        elseif (randchoice == 87) then syllab = "Tes"
        elseif (randchoice == 88) then syllab = "Mor"
        elseif (randchoice == 89) then syllab = "Mer"
        elseif (randchoice == 90) then syllab = "Myr"
        elseif (randchoice == 91) then syllab = "Har"
        elseif (randchoice == 92) then syllab = "Hak"
        elseif (randchoice == 93) then syllab = "Fak"
        elseif (randchoice == 94) then syllab = "Jak"
        elseif (randchoice == 95) then syllab = "Yuk"
        elseif (randchoice == 96) then syllab = "Yek"
        elseif (randchoice == 97) then syllab = "Yak"
        elseif (randchoice == 98) then syllab = "Shem"
        elseif (randchoice == 99) then syllab = "Sift"
        else syllab = "Buft" end

        return (syllab)
end

function get_monster_name_syllab_2 ()

	local randchoice
	local syllab

        randchoice = lua_randint(100)
        if (randchoice == 1) then syllab = "oon"
        elseif (randchoice == 2) then syllab = "un"
        elseif (randchoice == 3) then syllab = "im"
        elseif (randchoice == 4) then syllab = "ioon"
        elseif (randchoice == 5) then syllab = "ar"
        elseif (randchoice == 6) then syllab = "ur"
        elseif (randchoice == 7) then syllab = "er"
        elseif (randchoice == 8) then syllab = "gut"
        elseif (randchoice == 9) then syllab = "got"
        elseif (randchoice == 10) then syllab = "ot"
        elseif (randchoice == 11) then syllab = "et"
        elseif (randchoice == 12) then syllab = "at"
        elseif (randchoice == 13) then syllab = "spur"
        elseif (randchoice == 14) then syllab = "sha"
        elseif (randchoice == 15) then syllab = "ix"
        elseif (randchoice == 16) then syllab = "ax"
        elseif (randchoice == 17) then syllab = "ex"
        elseif (randchoice == 18) then syllab = "kix"
        elseif (randchoice == 19) then syllab = "kax"
        elseif (randchoice == 20) then syllab = "kex"
        elseif (randchoice == 21) then syllab = "ust"
        elseif (randchoice == 22) then syllab = "us"
        elseif (randchoice == 23) then syllab = "es"
        elseif (randchoice == 24) then syllab = "shar"
        elseif (randchoice == 25) then syllab = "she"
        elseif (randchoice == 26) then syllab = "sho"
        elseif (randchoice == 27) then syllab = "shy"
        elseif (randchoice == 28) then syllab = "shim"
        elseif (randchoice == 29) then syllab = "tim"
        elseif (randchoice == 30) then syllab = "tom"
        elseif (randchoice == 31) then syllab = "tem"
        elseif (randchoice == 32) then syllab = "tam"
        elseif (randchoice == 33) then syllab = "on"
        elseif (randchoice == 34) then syllab = "oo"
        elseif (randchoice == 35) then syllab = "uu"
        elseif (randchoice == 36) then syllab = "ii"
        elseif (randchoice == 37) then syllab = "aa"
        elseif (randchoice == 38) then syllab = "gem"
        elseif (randchoice == 39) then syllab = "gam"
        elseif (randchoice == 40) then syllab = "gim"
        elseif (randchoice == 41) then syllab = "lek"
        elseif (randchoice == 42) then syllab = "lak"
        elseif (randchoice == 43) then syllab = "lax"
        elseif (randchoice == 44) then syllab = "lex"
        elseif (randchoice == 45) then syllab = "lix"
        elseif (randchoice == 46) then syllab = "dal"
        elseif (randchoice == 47) then syllab = "del"
        elseif (randchoice == 48) then syllab = "dol"
        elseif (randchoice == 49) then syllab = "dyl"
        elseif (randchoice == 50) then syllab = "ryl"
        elseif (randchoice == 51) then syllab = "pox"
        elseif (randchoice == 52) then syllab = "pex"
        elseif (randchoice == 53) then syllab = "pax"
        elseif (randchoice == 54) then syllab = "pux"
        elseif (randchoice == 55) then syllab = "puk"
        elseif (randchoice == 56) then syllab = "tax"
        elseif (randchoice == 57) then syllab = "tex"
        elseif (randchoice == 58) then syllab = "tox"
        elseif (randchoice == 59) then syllab = "tux"
        elseif (randchoice == 60) then syllab = "tix"
        elseif (randchoice == 61) then syllab = "gyr"
        elseif (randchoice == 62) then syllab = "toon"
        elseif (randchoice == 63) then syllab = "teem"
        elseif (randchoice == 64) then syllab = "oon"
        elseif (randchoice == 65) then syllab = "eem"
        elseif (randchoice == 66) then syllab = "koon"
        elseif (randchoice == 67) then syllab = "keem"
        elseif (randchoice == 68) then syllab = "yel"
        elseif (randchoice == 69) then syllab = "yal"
        elseif (randchoice == 70) then syllab = "yil"
        elseif (randchoice == 71) then syllab = "yol"
        elseif (randchoice == 72) then syllab = "hoh"
        elseif (randchoice == 73) then syllab = "heh"
        elseif (randchoice == 74) then syllab = "hah"
        elseif (randchoice == 75) then syllab = "hiz"
        elseif (randchoice == 76) then syllab = "haz"
        elseif (randchoice == 77) then syllab = "hez"
        elseif (randchoice == 78) then syllab = "hoz"
        elseif (randchoice == 79) then syllab = "rol"
        elseif (randchoice == 80) then syllab = "rel"
        elseif (randchoice == 81) then syllab = "rul"
        elseif (randchoice == 82) then syllab = "ryl"
        elseif (randchoice == 83) then syllab = "pol"
        elseif (randchoice == 84) then syllab = "pal"
        elseif (randchoice == 85) then syllab = "pel"
        elseif (randchoice == 86) then syllab = "pil"
        elseif (randchoice == 87) then syllab = "glud"
        elseif (randchoice == 88) then syllab = "blud"
        elseif (randchoice == 89) then syllab = "brun"
        elseif (randchoice == 90) then syllab = "gram"
        elseif (randchoice == 91) then syllab = "jin"
        elseif (randchoice == 92) then syllab = "jan"
        elseif (randchoice == 93) then syllab = "jen"
        elseif (randchoice == 94) then syllab = "jun"
        elseif (randchoice == 95) then syllab = "plod"
        elseif (randchoice == 96) then syllab = "perk"
        elseif (randchoice == 97) then syllab = "plam"
        elseif (randchoice == 98) then syllab = "loom"
        elseif (randchoice == 99) then syllab = "uus"
        else syllab = "fer" end

        return (syllab)
end

function get_monster_adjective ()

	local randchoice
	local monadj

        randchoice = lua_randint(100)
        if (randchoice == 1) then monadj = "Cruel"
        elseif (randchoice == 2) then monadj = "Mad"
        elseif (randchoice == 3) then monadj = "Deranged"
        elseif (randchoice == 4) then monadj = "Violent"
        elseif (randchoice == 5) then monadj = "Rebel"
        elseif (randchoice == 6) then monadj = "Killer"
        elseif (randchoice == 7) then monadj = "Evil"
        elseif (randchoice == 8) then monadj = "Perverted"
        elseif (randchoice == 9) then monadj = "Stealthy"
        elseif (randchoice == 10) then monadj = "Clever"
        elseif (randchoice == 11) then monadj = "Vicious"
        elseif (randchoice == 12) then monadj = "Bloody"
        elseif (randchoice == 13) then monadj = "Hardened"
        elseif (randchoice == 14) then monadj = "Freak"
        elseif (randchoice == 15) then monadj = "Wild"
        elseif (randchoice == 16) then monadj = "Bloodlusted"
        elseif (randchoice == 17) then monadj = "Devious"
        elseif (randchoice == 18) then monadj = "Fanatic"
        elseif (randchoice == 19) then monadj = "Foul"
        elseif (randchoice == 20) then monadj = "Bad"
        elseif (randchoice == 21) then monadj = "Big"
        elseif (randchoice == 22) then monadj = "Strong"
        elseif (randchoice == 23) then monadj = "Agile"
        elseif (randchoice == 24) then monadj = "Smart"
        elseif (randchoice == 25) then monadj = "Sturdy"
        elseif (randchoice == 26) then monadj = "Mystic"
        elseif (randchoice == 27) then monadj = "Corrupted"
        elseif (randchoice == 28) then monadj = "Lazy"
        elseif (randchoice == 29) then monadj = "Vigilent"
        elseif (randchoice == 30) then monadj = "Powerful"
        elseif (randchoice == 31) then monadj = "Mean"
        elseif (randchoice == 32) then monadj = "Dominant"
        elseif (randchoice == 33) then monadj = "Scary"
        elseif (randchoice == 34) then monadj = "Spooky"
        elseif (randchoice == 35) then monadj = "Haunted"
        elseif (randchoice == 36) then monadj = "Possessed"
        elseif (randchoice == 37) then monadj = "Disgusting"
        elseif (randchoice == 38) then monadj = "Brave"
        elseif (randchoice == 39) then monadj = "Daring"
        elseif (randchoice == 40) then monadj = "Grim"
        elseif (randchoice == 41) then monadj = "Red"
        elseif (randchoice == 42) then monadj = "Yellow"
        elseif (randchoice == 43) then monadj = "Orange"
        elseif (randchoice == 44) then monadj = "Green"
        elseif (randchoice == 45) then monadj = "Blue"
        elseif (randchoice == 46) then monadj = "Purple"
        elseif (randchoice == 47) then monadj = "Black"
        elseif (randchoice == 48) then monadj = "Dark"
        elseif (randchoice == 49) then monadj = "White"
        elseif (randchoice == 50) then monadj = "Pink"
        elseif (randchoice == 51) then monadj = "Slayer"
        elseif (randchoice == 52) then monadj = "Hunter"
        elseif (randchoice == 53) then monadj = "Warrior"
        elseif (randchoice == 54) then monadj = "Heavy"
        elseif (randchoice == 55) then monadj = "Light"
        elseif (randchoice == 56) then monadj = "Pale"
        elseif (randchoice == 57) then monadj = "Enraged"
        elseif (randchoice == 58) then monadj = "Berserker"
        elseif (randchoice == 59) then monadj = "Violet"
        elseif (randchoice == 60) then monadj = "Umber"
        elseif (randchoice == 61) then monadj = "Creepy"
        elseif (randchoice == 62) then monadj = "Mysterious"
        elseif (randchoice == 63) then monadj = "Grey"
        elseif (randchoice == 64) then monadj = "Brown"
        elseif (randchoice == 65) then monadj = "Slimy"
        elseif (randchoice == 66) then monadj = "Disfigured"
        elseif (randchoice == 67) then monadj = "Greedy"
        elseif (randchoice == 68) then monadj = "Shiny"
        elseif (randchoice == 69) then monadj = "Spiky"
        elseif (randchoice == 70) then monadj = "Ugly"
        elseif (randchoice == 71) then monadj = "Aggressive"
        elseif (randchoice == 72) then monadj = "Dire"
        elseif (randchoice == 73) then monadj = "Spotted"
        elseif (randchoice == 74) then monadj = "Goon"
        elseif (randchoice == 75) then monadj = "Gold"
        elseif (randchoice == 76) then monadj = "Silver"
        elseif (randchoice == 77) then monadj = "Bronze"
        elseif (randchoice == 78) then monadj = "Metallic"
        elseif (randchoice == 79) then monadj = "Fearless"
        elseif (randchoice == 80) then monadj = "Reckless"
        elseif (randchoice == 81) then monadj = "Frantic"
        elseif (randchoice == 82) then monadj = "Confident"
        elseif (randchoice == 83) then monadj = "Desperate"
        elseif (randchoice == 84) then monadj = "Murderous"
        elseif (randchoice == 85) then monadj = "Angry"
        elseif (randchoice == 86) then monadj = "Ravenous"
        elseif (randchoice == 87) then monadj = "Ferocious"
        elseif (randchoice == 88) then monadj = "Sneaky"
        elseif (randchoice == 89) then monadj = "Lunatic"
        elseif (randchoice == 90) then monadj = "Solid"
        elseif (randchoice == 91) then monadj = "Fierce"
        elseif (randchoice == 92) then monadj = "Frightening"
        elseif (randchoice == 93) then monadj = "Feral"
        elseif (randchoice == 94) then monadj = "Blood-covered"
        elseif (randchoice == 95) then monadj = "Diabolic"
        elseif (randchoice == 96) then monadj = "Terrible"
        elseif (randchoice == 97) then monadj = "Unholy"
        elseif (randchoice == 98) then monadj = "Odd"
        elseif (randchoice == 99) then monadj = "Strange"
        else monadj = "Ancient" end

        return (monadj)
end


-- Flow dungeons code!
function prepare_flow_dungeon (level)

	local i

	dungeon(201).floor1 = flow_random_floor()
        dungeon(201).floor_percent1 = 34
        dungeon(201).floor2 = flow_random_floor()
        dungeon(201).floor_percent2 = 33
        dungeon(201).floor3 = flow_random_floor()
        dungeon(201).floor_percent3 = 33
        dungeon(201).outer_wall = flow_random_wall()
        dungeon(201).inner_wall = flow_random_wall()
        dungeon(201).fill_type1 = flow_random_wall()
        dungeon(201).fill_percent1 = 34
        dungeon(201).fill_type2 = flow_random_wall()
        dungeon(201).fill_percent2 = 33
        dungeon(201).fill_type3 = flow_random_wall()
        dungeon(201).fill_percent3 = 33
	dungeon(201).principal = TRUE
        dungeon(201).next = 0
        dungeon(201).min_plev = 0
        dungeon(201).mode = 0
        dungeon(201).min_m_alloc_level = 28
        dungeon(201).max_m_alloc_chance = 160
	dungeon(201).quest = 9001

	dungeon(201).mindepth = level
	dungeon(201).maxdepth = level

	-- Clear previous flags.
	dungeon(201).flags1 = 0

	give_dungeon_flag1(201, DF1_NO_UP)
	give_dungeon_flag1(201, DF1_NO_DOWN)
	give_dungeon_flag1(201, DF1_RANDOM_ONLY)
	give_dungeon_flag1(201, DF1_WEIRD)
	give_dungeon_flag1(201, DF1_BIG)

	-- Generate some random monsters!
	for i = 2050, 2097 do
		generate_monster(i, level, 0)
	end
end

function flow_random_floor ()

	local i
	local f

	i = lua_randint(13)

	if (i == 1) then f = FEAT_FLOOR end
	if (i == 2) then f = FEAT_GRASS end
	if (i == 3) then f = FEAT_DIRT end
	if (i == 4) then f = FEAT_SHAL_WATER end
	if (i == 5) then f = FEAT_SHAL_LAVA end
	if (i == 6) then f = FEAT_DEEP_WATER end
	if (i == 7) then f = FEAT_DEEP_LAVA end
	if (i == 8) then f = FEAT_SNOW end
	if (i == 9) then f = FEAT_FLOOR end
	if (i == 10) then f = FEAT_GRASS end
	if (i == 11) then f = FEAT_DIRT end
	if (i == 12) then f = FEAT_SHAL_WATER end
	if (i == 13) then f = FEAT_SNOW end

	return (f)
end

-- Also includes floor! :)
function flow_random_wall ()

	local i
	local f

	i = lua_randint(10)

	if (i == 1) then f = FEAT_TREES end
	if (i == 2) then f = FEAT_MOUNTAIN end
	if (i == 3) then f = FEAT_WALL_SOLID end
	if (i == 4) then f = FEAT_RUBBLE end
	if (i == 5) then f = FEAT_MAGMA end
	if (i == 6) then f = FEAT_QUARTZ end
	if (i == 7) then f = FEAT_FLOOR end
	if (i == 8) then f = FEAT_GRASS end
	if (i == 9) then f = FEAT_DIRT end
	if (i == 10) then f = FEAT_SNOW end

	return (f)
end

add_event_handler("generate_monster", generate_monster)
add_event_handler("get_monster_name_syllab_1", get_monster_name_syllab_1)
add_event_handler("get_monster_name_syllab_2", get_monster_name_syllab_2)
add_event_handler("get_monster_adjective", get_monster_adjective)
add_event_handler("prepare_flow_dungeon", prepare_flow_dungeon)
add_event_handler("flow_random_floor", flow_random_floor)
add_event_handler("flow_random_wall", flow_random_wall)
