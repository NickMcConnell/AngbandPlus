bool quest_main_monsters_hook(int r_idx)
{
        /* Sauron */
        if (r_idx == 860)
        {
                /* No Sauron until Necromancer dies */
                if (r_info[819].max_num) return TRUE;
        }
        /* Morgoth */
        else if (r_idx == 862)
        {
                /* No Morgoth until Sauron dies */
                if (r_info[860].max_num) return TRUE;
        }
        return FALSE;                
}
bool quest_morgoth_hook(int m_idx)
{
        /* Using test_monster_name() here would be a lot less ugly, but would take much more time */
        monster_race *r_ptr = &r_info[862];

        /* Need to kill him */
        if (!r_ptr->max_num)
        {
		/* Total winner */
		total_winner = TRUE;
                quest[QUEST_MORGOTH].status = QUEST_STATUS_FINISHED;

		/* Redraw the "title" */
		p_ptr->redraw |= (PR_TITLE);

		/* Congratulations */
                if (quest[QUEST_ONE].status == QUEST_STATUS_FINISHED)
                {
                        cmsg_print(TERM_L_GREEN, "*** CONGRATULATIONS ***");
                        cmsg_print(TERM_L_GREEN, "You have banished Morgoth's foul spirit from Ea, and as you watch a cleansing");
                        cmsg_print(TERM_L_GREEN, "winds roars through the dungeon, dispersing the nether mists around where the");
                        cmsg_print(TERM_L_GREEN, "body fell. You feel the thankfullness, and a touch of sorrow, from the Valar");
                        cmsg_print(TERM_L_GREEN, "for your deed. You will be forever heralded, your deed forever legendary.");
                        cmsg_print(TERM_L_GREEN, "You may retire (commit suicide) when you are ready.");
                }
                else
                {
                        cmsg_print(TERM_VIOLET, "*** CONGRATULATIONS ***");
                        cmsg_print(TERM_VIOLET, "You have banished Morgoth from Arda, and made Ea a safer place.");
                        cmsg_print(TERM_VIOLET, "As you look down at the dispersing mists around Morgoth, a sudden intuition");
                        cmsg_print(TERM_VIOLET, "grasps you. Fingering the One Ring, you gather the nether mists around");
                        cmsg_print(TERM_VIOLET, "yourself, and inhale deeply their seductive power.");
                        cmsg_print(TERM_VIOLET, "You will be forever feared, your orders forever obeyed.");
                        cmsg_print(TERM_VIOLET, "You may retire (commit suicide) when you are ready.");
                }

                /* End the plot */
                del_hook(HOOK_MONSTER_DEATH, quest_morgoth_hook);
                *(quest[QUEST_MORGOTH].plot) = QUEST_NULL;
                process_hooks_restart = TRUE;
        }
        return (FALSE);
};
bool quest_morgoth_dump_hook(int q_idx)
{
        if (quest[QUEST_MORGOTH].status >= QUEST_STATUS_COMPLETED)
        {
                if (quest[QUEST_ONE].status == QUEST_STATUS_FINISHED)
                        fprintf(hook_file, "\n You saved Arda and became a famed king.");
                else
                        fprintf(hook_file, "\n You became a new force of darkness and enslaved all free people.");
        }
        return (FALSE);
}
bool quest_morgoth_init_hook(int q_idx)
{
        if ((quest[QUEST_MORGOTH].status >= QUEST_STATUS_TAKEN) && (quest[QUEST_MORGOTH].status < QUEST_STATUS_FINISHED))
        {
                add_hook(HOOK_MONSTER_DEATH, quest_morgoth_hook, "morgort_death");
        }
        add_hook(HOOK_CHAR_DUMP, quest_morgoth_dump_hook, "morgoth_dump");
        add_hook(HOOK_NEW_MONSTER, quest_main_monsters_hook, "main_new_monster");
        return (FALSE);
}

bool quest_sauron_hook(int m_idx)
{
        /* Using test_monster_name() here would be a lot less ugly, but would take much more time */
        monster_race *r_ptr = &r_info[860];

        /* Need to kill him */
        if (!r_ptr->max_num)
        {
                cmsg_print(TERM_YELLOW, "Well done! You are on the way to slaying Morgoth...");
                quest[QUEST_SAURON].status = QUEST_STATUS_FINISHED;

                quest[QUEST_MORGOTH].status = QUEST_STATUS_TAKEN;
                quest_describe(QUEST_MORGOTH);

                del_hook(HOOK_MONSTER_DEATH, quest_sauron_hook);
                add_hook(HOOK_MONSTER_DEATH, quest_morgoth_hook, "morgort_death");
                *(quest[QUEST_SAURON].plot) = QUEST_MORGOTH;

                process_hooks_restart = TRUE;
        }
        return (FALSE);
};
bool quest_sauron_init_hook(int q_idx)
{
        if ((quest[QUEST_SAURON].status >= QUEST_STATUS_TAKEN) && (quest[QUEST_SAURON].status < QUEST_STATUS_FINISHED))
        {
                add_hook(HOOK_MONSTER_DEATH, quest_sauron_hook, "sauron_death");
        }
        add_hook(HOOK_NEW_MONSTER, quest_main_monsters_hook, "main_new_monster");
        return (FALSE);
}

bool quest_necro_hook(int m_idx)
{
        /* Using test_monster_name() here would be a lot less ugly, but would take much more time */
        monster_race *r_ptr = &r_info[819];

        /* Need to kill him */
        if (!r_ptr->max_num)
        {
                cmsg_print(TERM_YELLOW, "You see the spirit of the necromancer rise and flee...");
                cmsg_print(TERM_YELLOW, "It looks like it was indeed Sauron...");
                cmsg_print(TERM_YELLOW, "You should report that to Galadriel as soon as possible.");

                quest[QUEST_NECRO].status = QUEST_STATUS_FINISHED;

                *(quest[QUEST_NECRO].plot) = QUEST_ONE;
                quest[*(quest[QUEST_NECRO].plot)].init(*(quest[QUEST_NECRO].plot));

                del_hook(HOOK_MONSTER_DEATH, quest_necro_hook);
                process_hooks_restart = TRUE;
        }
        return (FALSE);
};
bool quest_necro_init_hook(int q_idx)
{
        if ((quest[QUEST_NECRO].status >= QUEST_STATUS_TAKEN) && (quest[QUEST_NECRO].status < QUEST_STATUS_FINISHED))
        {
                add_hook(HOOK_MONSTER_DEATH, quest_necro_hook, "necro_death");
        }
        add_hook(HOOK_NEW_MONSTER, quest_main_monsters_hook, "main_new_monster");
        return (FALSE);
}