-- the winning quest

function gen_sauron_monsters()


	-- Neil
	if (current_dungeon_idx == 4) and (dun_level == 99) then
		s = test_monster_name("Sauron, the Sorcerer")
		m_allow_special[s + 1] = TRUE
		gen_joke_place_monster(s)
		m_allow_special[s + 1] = FALSE
	end
end

add_hook_script(HOOK_LEVEL_END_GEN, "gen_sauron_monsters", "gen_sauron_monsters")

function gen_morgoth_monsters()


	-- Neil
	if (current_dungeon_idx == 4) and (dun_level == 101) then
		m = test_monster_name("Morgoth, Lord of Darkness")
		m_allow_special[m + 1] = TRUE
		gen_joke_place_monster(m)
		m_allow_special[m + 1] = FALSE
	end
end

add_hook_script(HOOK_LEVEL_END_GEN, "gen_morgoth_monsters", "gen_morgoth_monsters")



add_quest
{
        ["global"] =    "SAURON_QUEST",
        ["name"] =      "Sauron, the Sorcerer",
        ["desc"] =
        {
                        "You must defeat Sauron the Sorcerer to gain access to Morgoth's realm.",
        },
        ["level"] =     99,
        ["data"] =      {},
        ["hooks"] =     {
                        -- Initialization
                        [HOOK_INIT_GAME] = function(when)
                                if when == "end" then
                                        SAURON_IDX = test_monster_name("Sauron, the Sorcerer")
                                end
                        end,

                        [HOOK_BIRTH_OBJECTS] = function()
                                quest(SAURON_QUEST).status = QUEST_STATUS_TAKEN
                        end,

                        -- no level 100
                        [HOOK_STAIR] = function(dir)
                                if dir == "down" and dun_level == 99 and quest(SAURON_QUEST).status ~= QUEST_STATUS_FINISHED then
                                        msg_print("There is a magical field blocking the way down.")
                                        return TRUE
                                end
                        end,

                        -- ok when he dies, we're happier
                        [HOOK_MONSTER_DEATH] = function(m_idx)
                                if monster(m_idx).r_idx == SAURON_IDX then
                                        cmsg_print(TERM_YELLOW, "The force field blocking downstairs dissipates.")
                                        cmsg_print(TERM_YELLOW, "A down stairway magically appears before you.")

                                        cave_set_feat(player.py, player.px, FEAT_MORE)

		                        quest(SAURON_QUEST).status = QUEST_STATUS_FINISHED
                                	quest(MORGOTH_QUEST).status = QUEST_STATUS_TAKEN
                                end
                        end,
        },
}

add_quest
{
        ["global"] =    "MORGOTH_QUEST",
        ["name"] =      "Morgoth, Lord of Darkness",
        ["desc"] =
        {
                        "You must defeat Morgoth, Lord of Darkness.",
        },
        ["level"] =     100,
        ["data"] =      {},
        ["hooks"] =     {
                        -- Initialization
                        [HOOK_INIT_GAME] = function(when)
                                if when == "end" then
                                        MORGOTH_IDX = test_monster_name("Morgoth, Lord of Darkness")
                                end
                        end,

                        [HOOK_BIRTH_OBJECTS] = function()
                                quest(MORGOTH_QUEST).status = QUEST_STATUS_UNTAKEN
                        end,

                        -- No morgy until sauron death(shouldnt happen, but I'm paranoid
                        [HOOK_NEW_MONSTER] = function(r_idx)
                                if r_idx == MORGOTH_IDX and quest(SAURON_QUEST).status ~= QUEST_STATUS_FINISHED then
                                        return TRUE
                                end
                        end,

                        -- ok when he dies, we're happier
                        [HOOK_MONSTER_DEATH] = function(m_idx)
                                if monster(m_idx).r_idx == MORGOTH_IDX then
					cmsg_print(TERM_L_GREEN, "*** CONGRATULATIONS ***");
					cmsg_print(TERM_L_GREEN, "You have banished Morgoth's foul spirit from Ea, and as you watch, a cleansing")
					cmsg_print(TERM_L_GREEN, "winds roars through the dungeon, dispersing the nether mists around where the")
					cmsg_print(TERM_L_GREEN, "body fell. You feel thanks, and a touch of sorrow, from the Valar")
					cmsg_print(TERM_L_GREEN, "for your deed. You will be forever heralded, your deed forever legendary.")
					cmsg_print(TERM_L_GREEN, "You may retire (commit suicide) when you are ready.")

					-- Total winner
					total_winner = WINNER_NORMAL
                			has_won = WINNER_NORMAL
					player.redraw = bor(player.redraw, PR_TITLE)

		                        quest(MORGOTH_QUEST).status = QUEST_STATUS_FINISHED
                                end
                        end,
        },
}
