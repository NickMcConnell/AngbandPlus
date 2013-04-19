-- Jedi Quests 1 and 2. Just enough to get the player started.
SKILL_JEDI = 89
-- Partially based on Fireproofing quest

jedi_questa = {}
jedi_questb = {}

-- The map definition itself
jedi_questa.MAP =
[[#!map

# Permanent wall
F:X:63:3

# Trees
F:#:96:3

# Cobblestone Road
F:O:88:3

# Floor
F:.:181:3


# Stegnocentipede
F:L:200:3:1126

# Quest exit
F:<:6:3

D:XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
D:X###############################################################X
D:X#<...............................##############################X
D:X################################.##############################X
D:X################################.#############.....############X
D:X################################.#########......##......#######X
D:X##################...............#######....#######....########X
D:X#################................######..###########..#########X
D:X#################........................############.#########X
D:X##################...............####################.#########X
D:X###########.########............#####################.#########X
D:X##########..###########........######################.#########X
D:X##########...########################################.#########X
D:X############...######################################.#########X
D:X#############................................................##X
D:X################...#####..............##################.....##X
D:X#################..#######.........######################...###X
D:X################...##########....#########################..###X
D:X##############################..##########################.####X
D:X##############################..##############.###########.####X
D:X###############################.############..###########...###X
D:X###############################.##########...#########.....####X
D:X###############.......#########.........................#######X
D:X###############..####.....#####.##########...##################X
D:X############.....######.........##########..###################X
D:X###########.......###########..###########.####################X
D:X############........########.....##############################X
D:X###################....########.###############################X
D:X##############......###########..............##################X
D:X############.....########################.......###############X
D:X##############....############################.........########X
D:X################....##########.............########.........###X
D:X#################....###........########.................######X
D:X###############...............#############.........###########X
D:X##############.....................#####################.....##X
D:X##################################...........................L#X
D:X###############################################################X
D:XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# Starting position
P:4:4
]]

-- Map helper
jedi_questa.place_random = function(minY, minX, maxY, maxX, monster)
	y = randint(maxY - minY + 1) + minY
	x = randint(maxX - minX + 1) + minX
	return place_monster_one(y, x, monster, 0, TRUE, MSTATUS_ENEMY)
end






-- Quest data and hooks
add_quest
{
	["global"] = "JEDI_QUESTA",
	["name"] = "Jedi Quest 1",
	["desc"] = function()
		-- Quest taken
		if (quest(JEDI_QUESTA).status == QUEST_STATUS_TAKEN) then
			print_hook("#####yJedi Quest 1: Centipedes!! (Danger Level: 5)\n")
			print_hook("Destroy the centipedes that are troubling our enclave\n")
			print_hook("\n")
		-- Quest done, book not gotten yet
		elseif (quest(JEDI_QUESTA).status == QUEST_STATUS_COMPLETED) then
			print_hook("#####yJedi Quest 1: Centipedes!!\n")
			print_hook("You have destroyed the centipedes that are troubling the enclave.\n")
			print_hook("Perhaps you should see about a reward.\n")
			print_hook("\n")
		end
	end,
	["level"] = 1,

	["hooks"] =
	{
		-- Start the game without the quest, need to request it
		[HOOK_BIRTH_OBJECTS] = function()
			quest(JEDI_QUESTA).status = QUEST_STATUS_UNTAKEN
			end,

		[HOOK_GEN_QUEST] = function()
			-- Only if player doing this quest
			if (player.inside_quest ~= JEDI_QUESTA) then
				return FALSE
			else

			load_map(jedi_questa.MAP, 2, 2)
			level_flags2 = DF2_NO_GENO
		

			-- generate the White centipedes 1080
			centipedes = damroll(8, 2) -- plus one on the map
			while(centipedes > 0) do
				if 0 < jedi_questa.place_random(4, 4, 14, 37, 1080) then
					centipedes = centipedes - 1
				end
			end

			-- generate the Monastic centipedes 611
			centipedes = damroll(1, 2)
			while(centipedes > 0) do
				if 0 < jedi_questa.place_random(14, 34, 37, 67, 1088) then
					centipedes = centipedes - 1
				end
			end

			-- generate more Monastic centipedes 611
			centipedes = damroll(1, 2) - 1
			while(centipedes > 0) do
				if 0 < jedi_questa.place_random(4, 34, 14, 67, 1090) then
					centipedes = centipedes - 1
				end
			end

			-- generate even more Monastic centipedes 611
			centipedes = damroll(1, 2) - 1
			while(centipedes > 0) do
				if 0 < jedi_questa.place_random(14, 4, 37, 34, 1092) then
					centipedes = centipedes - 1
				end
			end

			-- Flesh golem 256
			golems = 2
			while(golems > 0) do
				if 0 < jedi_questa.place_random(10, 10, 37, 67, 1091) then
					golems = golems - 1
				end
			end

			-- Clay golem 261
			golems = 2
			while(golems > 0) do
				if 0 < jedi_questa.place_random(10, 10, 37, 67, 1092) then
					golems = golems - 1
				end
			end

			-- Iron golem 367
			golems = 2
			while(golems > 0) do
				if 0 < jedi_questa.place_random(10, 10, 37, 67, 1091) then
					golems = golems - 1
				end
			end

			-- Mithril Golem 464
			golems = 1
			while(golems > 0) do
				if 0 < jedi_questa.place_random(10, 10, 37, 67, 1092) then
					golems = golems - 1
				end
			end

			-- one Master lich is on the map

			return TRUE
			end
		end,
		
		[HOOK_CHAR_DUMP] = function()
			if (quest(JEDI_QUESTB).status == QUEST_STATUS_FAILED) then
				print_hook("\n You failed the Jedi Order, and the centipedes overran them ")
			elseif (quest(JEDI_QUESTB).status == QUEST_STATUS_COMPLETED) or 
				(quest(JEDI_QUESTB).status == QUEST_STATUS_REWARDED) or 
				(quest(JEDI_QUESTB).status == QUEST_STATUS_FINISHED) then
					print_hook("\n You stopped centipedes from destroying the Jedi Enclave ")			
			end
			return FALSE
		end,

		
		[HOOK_STAIR] = function()
			local ret

			-- only ask this if player about to go up stairs of quest and hasn't won yet
			if (player.inside_quest ~= JEDI_QUESTA) or (quest(JEDI_QUESTA).status == QUEST_STATUS_COMPLETED) then
				return FALSE
			end

			if cave(player.py, player.px).feat ~= FEAT_LESS then return end

			-- flush all pending input
			flush()

			-- confirm
			ret = get_check("Really abandon the quest?")

			-- if yes, then
			if ret == TRUE then
				-- fail the quest
				quest(JEDI_QUESTA).status = QUEST_STATUS_FAILED
				return FALSE
			else 
				-- if no, they stay in the quest
				return TRUE
			end
		end,
		[HOOK_MONSTER_DEATH] = function()
			-- if they're in the quest and haven't won, continue
			if (player.inside_quest ~= JEDI_QUESTA) or (quest(JEDI_QUESTA).status == QUEST_STATUS_COMPLETED) then
				return FALSE
			end

			i = 1
			count = -1
			while i <= m_max do
				local monster = m_list[i]
				if (monster.r_idx > 0) and (monster.status <= MSTATUS_ENEMY) then
					count = count + 1
				end
				i = i + 1
			end

			if count == 0 then
				quest(JEDI_QUESTA).status = QUEST_STATUS_COMPLETED
				msg_print(TERM_YELLOW, "The enclave is safer now.")
			end
		end,
	},
}


add_quest
{
	["global"] = "JEDI_QUESTB",
	["name"] = "Jedi Quest 2",
	["desc"] = function()
		-- Quest taken
		if (quest(JEDI_QUESTB).status == QUEST_STATUS_TAKEN) then
			print_hook("#####yJedi Quest 2: Celegorm the Fair!! (Danger Level: 5)\n")
			print_hook("Kill Celegorm the Fair before he can destroy our enclave.\n")
			print_hook("He is on Level 10 of the caves to the east.\n")
			print_hook("He is quite powerful, so be prepared before you face him.\n")
			print_hook("\n")
		-- Quest done, book not gotten yet
		elseif (quest(JEDI_QUESTB).status == QUEST_STATUS_COMPLETED) then
			print_hook("#####yJedi Quest 2: Celegorm the Fair!!\n")
			print_hook("You have stopped Celegorm.\n")
			print_hook("Perhaps you should see about a reward.\n")
			print_hook("\n")
		end
	end,
	["level"] = 5,

	["hooks"] =
	{
		-- Start the game without the quest, need to request it
		[HOOK_BIRTH_OBJECTS] = function()
			quest(JEDI_QUESTB).status = QUEST_STATUS_UNTAKEN
			end,
			
		[HOOK_NEW_MONSTER] = function(r_idx)
	
			if (r_idx == test_monster_name("Celegorm the Fair")) then
				if (quest(JEDI_QUESTB).status == QUEST_STATUS_TAKEN) then
					return FALSE
				else
				
					return TRUE
				end
			end
			
		end,			

		[HOOK_CHAR_DUMP] = function()
			if (quest(JEDI_QUESTB).status == QUEST_STATUS_FAILED) then
				print_hook("\n You failed the Jedi Order, and allowed Celegorm through. ")
			elseif (quest(JEDI_QUESTB).status == QUEST_STATUS_COMPLETED) or 
				(quest(JEDI_QUESTB).status == QUEST_STATUS_REWARDED) or 
				(quest(JEDI_QUESTB).status == QUEST_STATUS_FINISHED) then
					print_hook("\n You stopped Celegorm from destroying the Jedi Enclave ")			
			end
			return FALSE
		end,				

			
		[HOOK_MONSTER_DEATH] = function(m_idx)
		
			m_ptr = monster(m_idx)
			if (m_ptr.r_idx == test_monster_name("Celegorm the Fair")) then
				quest(JEDI_QUESTB).status = QUEST_STATUS_COMPLETED
				msg_print(TERM_YELLOW, "You have killed Celegorm the Fair.")
			end
		end,
	},
}


-- Library store action
add_building_action
{
	["index"] = 100,
	["action"] = function()
		-- the quest hasn't been requested already, right?
		
		if get_class_name() ~= "Jedi" then
			msg_print("I'm sorry, you can't help me.")
			end
		
		if quest(JEDI_QUESTA).status == QUEST_STATUS_UNTAKEN then
			-- quest has been taken now
			quest(JEDI_QUESTA).status = QUEST_STATUS_TAKEN

			-- issue instructions
			msg_print("Our enclave is being threatened by centipedes.")
			msg_print("Please go out and destroy them.")

			return TRUE, FALSE, TRUE
		-- if quest completed
		elseif (quest(JEDI_QUESTA).status == QUEST_STATUS_COMPLETED) then
			msg_print("Thank you! Here is a hood.")
				local obj = create_object(32, 51);
				obj.ident = bor(obj.ident, IDENT_MENTAL, IDENT_KNOWN)
				inven_carry(obj, FALSE)
				end_object(obj)
			msg_print("Also, let me train you in the ways of the Jedi")
			skill(SKILL_JEDI).value = skill(SKILL_JEDI).value + (3 * (skill(SKILL_JEDI).mod))
			quest(JEDI_QUESTA).status = QUEST_STATUS_REWARDED
		
			

		-- if the player asks for a quest when they already have it, but haven't failed it, give them some extra instructions
		elseif (quest(JEDI_QUESTA).status == QUEST_STATUS_TAKEN) then
			msg_print("Please go exterminate those centipedes.")

		-- quest failed or completed, then give no more quests
		elseif (quest(JEDI_QUESTA).status == QUEST_STATUS_FAILED) then
			msg_print("I have no more quests for you. You have failed us.")
			
		elseif (quest(JEDI_QUESTA).status == QUEST_STATUS_REWARDED)	then
			quest(JEDI_QUESTB).status = QUEST_STATUS_TAKEN
			msg_print("Celegorm the Fair has learned of our Enclave!")
			msg_print("Please go to the caves and stop him before he destroys us.")
			return TRUE, FALSE, TRUE
		
		elseif (quest(JEDI_QUESTB).status == QUEST_STATUS_COMPLETED) then
			msg_print("Thank you! You are now promoted to Jedi Master.")
				local obj = create_object(36, 79);
				obj.ident = bor(obj.ident, IDENT_MENTAL, IDENT_KNOWN)
				inven_carry(obj, FALSE)
				end_object(obj)
				local obj1 = create_object(8, 200);
				obj1.ident = bor(obj1.ident, IDENT_MENTAL, IDENT_KNOWN)
				inven_carry(obj1, FALSE)
				end_object(obj1)
			msg_print("Also, let me train you further in the ways of the Jedi")
			skill(SKILL_JEDI).value = skill(SKILL_JEDI).value + (3 * (skill(SKILL_JEDI).mod))	
			msg_print("We've learned that the forces of evil here are being led by the Necromancer")
			msg_print("Go to Bree, and complete the quests there, and when you feel ready,")
			msg_print("Destroy the Necromancer!")
			quest(JEDI_QUESTB).status = QUEST_STATUS_REWARDED
			quest(1).status = QUEST_STATUS_TAKEN
			
		elseif (quest(JEDI_QUESTB).status == QUEST_STATUS_REWARDED) then
			msg_print("I have no more quests for you. Go to Bree")
		end
		return TRUE
	end,
}
