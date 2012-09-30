bool quest_thieves_gen_hook(int q_idx)
{
	int x, y;
        int xstart = 2;
        int ystart = 2;
        bool again = TRUE;

        if (p_ptr->inside_quest != QUEST_THIEVES) return FALSE;

	/* Start with perm walls */
	for (y = 0; y < cur_hgt; y++)
	{
		for (x = 0; x < cur_wid; x++)
		{
			cave[y][x].feat = FEAT_PERM_SOLID; 
		}
	}

        dun_level = quest[p_ptr->inside_quest].level;

	/* Set the correct monster hook */
	set_mon_num_hook();

	/* Prepare allocation table */
	get_mon_num_prep();

        init_flags = INIT_CREATE_DUNGEON;
        process_dungeon_file("thieves.map", &ystart, &xstart, cur_hgt, cur_wid, TRUE);

        /* Rip the inventory from the player */
        cmsg_print(TERM_YELLOW, "You feel a vicious blow on your head.");
        while (again)
        {
                again = FALSE;
                for (x = 0; x < INVEN_TOTAL; x++)
                {
                        object_type *o_ptr = &inventory[x];

                        if (!o_ptr->k_idx) continue;

                        if ((x >= INVEN_WIELD) && cursed_p(o_ptr)) continue;

                        inven_drop(x, 99, 4, 24, TRUE);

                        /* Thats ugly .. but it works */
                        again = TRUE;
                        break;
                }
        }

        del_hook(HOOK_GEN_QUEST, quest_thieves_gen_hook);
        process_hooks_restart = TRUE;

        return TRUE;
}
bool quest_thieves_hook(int q_idx)
{
        int i, mcnt = 0;

        if (p_ptr->inside_quest != QUEST_THIEVES) return FALSE;

        /* ALARM !!! */
        if (cave[17][22].feat == FEAT_OPEN)
        {
                cmsg_print(TERM_L_RED, "An alarm rings!");
                aggravate_monsters(0);
                cave_set_feat(14, 20, FEAT_OPEN);
                cave_set_feat(14, 16, FEAT_OPEN);
                cave_set_feat(14, 12, FEAT_OPEN);
                cave_set_feat(14, 8, FEAT_OPEN);
                cave_set_feat(20, 20, FEAT_OPEN);
                cave_set_feat(20, 16, FEAT_OPEN);
                cave_set_feat(20, 12, FEAT_OPEN);
                cave_set_feat(20, 8, FEAT_OPEN);
                msg_print("The door explodes.");
                cave_set_feat(17, 22, FEAT_FLOOR);
        }

	/* Process the monsters (backwards) */
	for (i = m_max - 1; i >= 1; i--)
	{
		/* Access the monster */
                monster_type *m_ptr = &m_list[i];

		/* Ignore "dead" monsters */
		if (!m_ptr->r_idx) continue;

                if (m_ptr->status < MSTATUS_FRIEND) mcnt++;
        }

        /* Nobody left ? */
        if (!mcnt)
        {
                msg_print("The magic hiding the stairs is now gone.");
                cave_set_feat(23, 4, FEAT_LESS);

                quest[p_ptr->inside_quest].status = QUEST_STATUS_COMPLETED;
                del_hook(HOOK_END_TURN, quest_thieves_hook);
                process_hooks_restart = TRUE;

                cmsg_print(TERM_YELLOW, "You stopped the thieves and saved Bree!");
                return (FALSE);
        }
        return FALSE;
};
bool quest_thieves_finish_hook(int q_idx)
{
        object_type forge, *q_ptr;

        if (q_idx != QUEST_THIEVES) return FALSE;

        c_put_str(TERM_YELLOW, "Thank you for killing the band of thieves! Take this small reward.", 8, 0);

        q_ptr = &forge;
        object_prep(q_ptr, lookup_kind(TV_SWORD, SV_LONG_SWORD));
        q_ptr->number = 1;
        apply_magic(q_ptr, 5, TRUE, TRUE, FALSE);
        object_aware(q_ptr);
        object_known(q_ptr);
        (void)inven_carry(q_ptr, FALSE);

        /* Continue the plot */
        *(quest[q_idx].plot) = QUEST_HOBBIT;

        del_hook(HOOK_QUEST_FINISH, quest_thieves_finish_hook);
        process_hooks_restart = TRUE;

        return TRUE;
}

bool quest_thieves_feeling_hook(int q_idx)
{
        if (p_ptr->inside_quest != QUEST_THIEVES) return FALSE;

        msg_print("You wake up in a prison cell.");
        msg_print("All your possessions have been stolen!");

        del_hook(HOOK_FEELING, quest_thieves_feeling_hook);
        process_hooks_restart = TRUE;

        return TRUE;
}

bool quest_thieves_init_hook(int q_idx)
{
        if ((quest[QUEST_THIEVES].status >= QUEST_STATUS_TAKEN) && (quest[QUEST_THIEVES].status < QUEST_STATUS_FINISHED))
        {
                add_hook(HOOK_END_TURN, quest_thieves_hook, "thieves_end_turn");
                add_hook(HOOK_QUEST_FINISH, quest_thieves_finish_hook, "thieves_finish");
                add_hook(HOOK_GEN_QUEST, quest_thieves_gen_hook, "thieves_geb");
                add_hook(HOOK_FEELING, quest_thieves_feeling_hook, "thieves_feel");
        }
        return (FALSE);
}
