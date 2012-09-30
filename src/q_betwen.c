#undef cquest
#define cquest (quest[QUEST_BETWEEN])

bool quest_between_move_hook(int loc)
{
        int y = loc >> 8, x = loc & 0xFF;
        cave_type *c_ptr = &cave[y][x];

        if (cquest.status != QUEST_STATUS_TAKEN) return FALSE;

        /* The tower of Turgon */
        if ((c_ptr->feat == FEAT_SHOP) && (c_ptr->special == 27))
        {
                cmsg_print(TERM_YELLOW, "Turgon is there.");
                cmsg_print(TERM_YELLOW, "'Ah, thanks you noble hero, now please return to Minas Anor to finish the link.'");

                cquest.status = QUEST_STATUS_COMPLETED;

                return TRUE;
        }

        /* Only 1 ambush */
        if (cquest.data[0]) return (FALSE);

        if (!p_ptr->wild_mode)
        {
                if (p_ptr->wilderness_y > 19) return (FALSE);
        }
        else
        {
                if (py > 19) return (FALSE);
        }

        /* Mark as entered */
        cquest.data[0] = TRUE;

        p_ptr->wild_mode = FALSE;
        p_ptr->inside_quest = QUEST_BETWEEN;
	p_ptr->leaving = TRUE;

        cmsg_print(TERM_YELLOW, "Looks like a full wing of dragonriders ambushes you !");

        return FALSE;
}
bool quest_between_gen_hook(int q_idx)
{
	int x, y;
        int xstart = 2;
        int ystart = 2;

        if (p_ptr->inside_quest != QUEST_BETWEEN) return FALSE;

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
        hack_allow_special = TRUE;
        process_dungeon_file_full = TRUE;
        process_dungeon_file("between.map", &ystart, &xstart, cur_hgt, cur_wid, TRUE);
        process_dungeon_file_full = FALSE;
        hack_allow_special = FALSE;

        return TRUE;
}
bool quest_between_finish_hook(int q_idx)
{
        if (q_idx != QUEST_BETWEEN) return FALSE;

        c_put_str(TERM_YELLOW, "Ah you finally arrived, I hope your travel wasn't too hard.", 8, 0);
        c_put_str(TERM_YELLOW, "As a reward you can freely use teh between gates for quick travel.", 9, 0);

        /* Continue the plot */
        *(quest[q_idx].plot) = QUEST_NULL;

        del_hook(HOOK_QUEST_FINISH, quest_between_finish_hook);
        process_hooks_restart = TRUE;

        return TRUE;
}
bool quest_between_death_hook(int m_idx)
{
        int i, mcnt = 0;

        if (p_ptr->inside_quest != QUEST_BETWEEN) return FALSE;

	for (i = m_max - 1; i >= 1; i--)
	{
		/* Access the monster */
                monster_type *m_ptr = &m_list[i];

		/* Ignore "dead" monsters */
		if (!m_ptr->r_idx) continue;

                if (m_ptr->status <= MSTATUS_NEUTRAL) mcnt++;
        }

        if (mcnt < 2)
        {
                cmsg_print(TERM_YELLOW, "You can escape now.");
                cave_set_feat(py, px, FEAT_LESS);

                return FALSE;
        }


        return FALSE;
}
bool quest_between_dump_hook(int q_idx)
{
        if (cquest.status >= QUEST_STATUS_COMPLETED)
        {
                fprintf(hook_file, "\n You established a permanent between liaison between Minas Anor and Gondolin,");
                fprintf(hook_file, "\n  thus allowing the last alliance to exist.");
        }
        return (FALSE);
}
bool quest_between_init_hook(int q_idx)
{
        if ((cquest.status >= QUEST_STATUS_TAKEN) && (cquest.status < QUEST_STATUS_FINISHED))
        {
                add_hook(HOOK_MOVE, quest_between_move_hook, "between_move");
                add_hook(HOOK_GEN_QUEST, quest_between_gen_hook, "between_gen");
                add_hook(HOOK_QUEST_FINISH, quest_between_finish_hook, "between_finish");
                add_hook(HOOK_MONSTER_DEATH, quest_between_death_hook, "between_death");
        }
        add_hook(HOOK_CHAR_DUMP, quest_between_dump_hook, "between_dump");
        return (FALSE);
}
