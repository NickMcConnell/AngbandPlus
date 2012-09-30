#undef cquest
#define cquest (quest[QUEST_HOBBIT])

bool quest_hobbit_gen_hook(int q_idx)
{
        int x, y, try = 10000;

        if ((cquest.status != QUEST_STATUS_TAKEN) || (dun_level != cquest.data[0]) || (dungeon_type != DUNGEON_MAZE)) return FALSE;

        /* Find a good position */
        while(try)
        {
                /* Get a random spot */
                y = randint(cur_hgt - 4) + 2;
                x = randint(cur_wid - 4) + 2;

                /* Is it a good spot ? */
                if (cave_empty_bold(y, x)) break;

                /* One less try */
                try--;
        }

        /* Place the hobbit */
        hack_allow_special = TRUE;
        place_monster_one(y, x, test_monster_name("Merton Proudfoot, the lost hobbit"), 0, FALSE, MSTATUS_FRIEND);
        hack_allow_special = FALSE;

        return FALSE;
}
bool quest_hobbit_finish_hook(int q_idx)
{
        object_type forge, *q_ptr;

        if (q_idx != QUEST_HOBBIT) return FALSE;

        c_put_str(TERM_YELLOW, "I see that Merton is back, thank you so much.", 8, 0);
        c_put_str(TERM_YELLOW, "Take this as a proof of my gratitude.", 9, 0);

        q_ptr = &forge;
        object_prep(q_ptr, lookup_kind(TV_ROD, SV_ROD_RECALL));
        q_ptr->number = 1;
        object_aware(q_ptr);
        object_known(q_ptr);
        q_ptr->ident |= IDENT_STOREB;
        (void)inven_carry(q_ptr, FALSE);

        /* Continue the plot */
        *(quest[q_idx].plot) = (!rand_int(2))?QUEST_TROLL:QUEST_WIGHT;

        del_hook(HOOK_QUEST_FINISH, quest_hobbit_finish_hook);
        process_hooks_restart = TRUE;

        return TRUE;
}
bool quest_hobbit_give_hook(int m_idx)
{
        object_type *o_ptr = &inventory[hook_option];
        monster_type *m_ptr = &m_list[m_idx];

        if (m_ptr->r_idx != test_monster_name("Merton Proudfoot, the lost hobbit")) return (FALSE);

        if ((o_ptr->tval != TV_SCROLL) || (o_ptr->sval != SV_SCROLL_WORD_OF_RECALL)) return (FALSE);

        msg_print("Oh thank you noble one!");
        msg_print("Merton Proudfoot reads the scroll and is recalled to the safety of his home.");

        delete_monster_idx(m_idx);
        inven_item_increase(hook_option, -1);
        inven_item_optimize(hook_option);

        cquest.status = QUEST_STATUS_COMPLETED;

        del_hook(HOOK_GIVE, quest_hobbit_give_hook);
        process_hooks_restart = TRUE;

        return TRUE;
}
bool quest_hobbit_dump_hook(int q_idx)
{
        if (cquest.status >= QUEST_STATUS_COMPLETED)
        {
                fprintf(hook_file, "\n You saved a young hobbit from an horrible fate.");
        }
        return (FALSE);
}
bool quest_hobbit_init_hook(int q_idx)
{
        /* Get a level to place the hobbit */
        if (!cquest.data[0])
        {
                cquest.data[0] = rand_range(26, 34);
                if (wizard) message_add(format("Hobbit level %d", cquest.data[0]), TERM_BLUE);
        }

        if ((cquest.status >= QUEST_STATUS_TAKEN) && (cquest.status < QUEST_STATUS_FINISHED))
        {
                add_hook(HOOK_GIVE, quest_hobbit_give_hook, "hobbit_give");
                add_hook(HOOK_GEN_LEVEL, quest_hobbit_gen_hook, "hobbit_gen");
                add_hook(HOOK_QUEST_FINISH, quest_hobbit_finish_hook, "hobbit_finish");
        }
        add_hook(HOOK_CHAR_DUMP, quest_hobbit_dump_hook, "hobbit_dump");
        return (FALSE);
}
