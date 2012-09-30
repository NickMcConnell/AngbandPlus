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
                /* No Sauron until Morgoth dies */
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
                cmsg_print(TERM_L_GREEN, "*** CONGRATULATIONS ***");
                cmsg_print(TERM_L_GREEN, "Thanks to you, Arda is free !");
                cmsg_print(TERM_L_GREEN, "You have won the game!");
                cmsg_print(TERM_L_GREEN, "You may retire (commit suicide) when you are ready.");

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
                fprintf(hook_file, "\n You saved Arda and became a famed king.");
        }
        return (FALSE);
}
bool quest_morgoth_init_hook(int q_idx)
{
        if ((quest[QUEST_MORGOTH].status >= QUEST_STATUS_TAKEN) && (quest[QUEST_MORGOTH].status < QUEST_STATUS_FINISHED))
        {
                add_hook(HOOK_MONSTER_DEATH, quest_morgoth_hook, "morgort_death");
                add_hook(HOOK_NEW_MONSTER, quest_main_monsters_hook, "main_new_monster");
        }
        add_hook(HOOK_CHAR_DUMP, quest_morgoth_dump_hook, "morgoth_dump");
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
                add_hook(HOOK_NEW_MONSTER, quest_main_monsters_hook, "main_new_monster");
        }
        return (FALSE);
}

bool quest_necro_hook(int m_idx)
{
        /* Using test_monster_name() here would be a lot less ugly, but would take much more time */
        monster_race *r_ptr = &r_info[819];

        /* Need to kill her */
        if (!r_ptr->max_num)
        {
                cmsg_print(TERM_YELLOW, "You see the spirit of the necromancer rise and flee...");
                cmsg_print(TERM_YELLOW, "It looks like it was indeed Sauron...");

                quest[QUEST_NECRO].status = QUEST_STATUS_FINISHED;

                quest[QUEST_SAURON].status = QUEST_STATUS_TAKEN;
                quest_describe(QUEST_SAURON);
                del_hook(HOOK_MONSTER_DEATH, quest_necro_hook);
                add_hook(HOOK_MONSTER_DEATH, quest_sauron_hook, "sauron_death");
                *(quest[QUEST_NECRO].plot) = QUEST_SAURON;

                process_hooks_restart = TRUE;
        }
        return (FALSE);
};
bool quest_necro_init_hook(int q_idx)
{
        if ((quest[QUEST_NECRO].status >= QUEST_STATUS_TAKEN) && (quest[QUEST_NECRO].status < QUEST_STATUS_FINISHED))
        {
                add_hook(HOOK_MONSTER_DEATH, quest_necro_hook, "necro_death");
                add_hook(HOOK_NEW_MONSTER, quest_main_monsters_hook, "main_new_monster");
        }
        return (FALSE);
}
