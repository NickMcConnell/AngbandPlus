#undef cquest
#define cquest (quest[QUEST_ONE])

bool quest_one_move_hook(int loc)
{
        int y = loc >> 8, x = loc & 0xFF;
        cave_type *c_ptr = &cave[y][x];

        if (cquest.status == QUEST_STATUS_UNTAKEN)
        {
                if (quest[QUEST_NECRO].status < QUEST_STATUS_FINISHED) return (FALSE);

                /* The mirror of galadriel */
                if ((c_ptr->feat != FEAT_SHOP) || (c_ptr->special != 23)) return (FALSE);

                cmsg_print(TERM_YELLOW, "You meet Galadriel, she seems woried.");
                cmsg_print(TERM_YELLOW, "'So it was Sauron that lurked in Dol Guldur...'");
                cmsg_print(TERM_YELLOW, "'The Ennemy is growing in power, Morgoth will be unreachable as long'");
                cmsg_print(TERM_YELLOW, "'as his most powerful servant, Sauron, lives. But the power of Sauron'");
                cmsg_print(TERM_YELLOW, "'lies in the One Ring, our only hope is that you find it'");
                cmsg_print(TERM_YELLOW, "'and destroy it. I know it will be hard, but *NEVER* use it'");
                cmsg_print(TERM_YELLOW, "'or it will corrupt you forever.'");
                cmsg_print(TERM_YELLOW, "'Without the ring being destroyed, Sauron wont be permanently defeated'");
                cmsg_print(TERM_YELLOW, "'When you have it you will have to bring it to Mount Doom, in Mordor,'");
                cmsg_print(TERM_YELLOW, "'to destroy it in the Great Fire where it was once forged.'");
                cmsg_print(TERM_YELLOW, "'I do not know where to find it, seek it through middle earth. Maybe there'");
                cmsg_print(TERM_YELLOW, "'other people that might know'");

                /* Continue the plot */
                cquest.status = QUEST_STATUS_TAKEN;
                cquest.init(QUEST_ONE);

                return TRUE;
        }

        return FALSE;
}
bool quest_one_drop_hook(int o_idx)
{
        object_type *o_ptr = &inventory[o_idx];

        if (cquest.status != QUEST_STATUS_TAKEN) return FALSE;

        if (o_ptr->name1 != ART_POWER) return FALSE;
        if (cave[py][px].feat != FEAT_GREAT_FIRE) return FALSE;

        cmsg_print(TERM_YELLOW, "You drop the One Ring in the Great Fire, it is rapidly consumed");
        cmsg_print(TERM_YELLOW, "by the searing flames.");
        cmsg_print(TERM_YELLOW, "You feel the powers of evils weakening.");
        cmsg_print(TERM_YELLOW, "Now you can go onto the hunt for Sauron!");

        inven_item_increase(o_idx, -99);
        inven_item_optimize(o_idx);

        /* Continue the plot */
        cquest.status = QUEST_STATUS_FINISHED;
        *(quest[QUEST_ONE].plot) = QUEST_SAURON;
        quest[*(quest[QUEST_ONE].plot)].status = QUEST_STATUS_TAKEN;
        quest[*(quest[QUEST_ONE].plot)].init(*(quest[QUEST_ONE].plot));

        return TRUE;
}
bool quest_one_wield_hook(int o_idx)
{
        object_type *o_ptr = &inventory[o_idx];

        if (cquest.status != QUEST_STATUS_TAKEN) return FALSE;

        if (o_ptr->name1 != ART_POWER) return FALSE;

        if (!get_check("You are warned not to wear it, are you sure?")) return TRUE;
        if (!get_check("You are warned not to wear it, are you *REALLY* sure?")) return TRUE;

        cmsg_print(TERM_YELLOW, "As you put it on your finger you feel dark powers sapping your soul.");
        cmsg_print(TERM_YELLOW, "The ring firmly holds to your finger!");

        /* Continue the plot */
        cquest.status = QUEST_STATUS_FAILED_DONE;
        *(quest[QUEST_ONE].plot) = QUEST_SAURON;
        quest[*(quest[QUEST_ONE].plot)].status = QUEST_STATUS_TAKEN;
        quest[*(quest[QUEST_ONE].plot)].init(*(quest[QUEST_ONE].plot));

        return FALSE;
}
bool quest_one_identify_hook(int item)
{
        if (cquest.status == QUEST_STATUS_TAKEN)
        {
                object_type *o_ptr;

                /* Inventory */
                if (item >= 0)
                {
                        o_ptr = &inventory[item];
                }
	
                /* Floor */
                else
                {
                        o_ptr = &o_list[0 - item];
                }

                if (o_ptr->name1 == ART_POWER)
                {
                        cmsg_print(TERM_YELLOW, "You finally found the One Ring, source of Sauron power, and key to");
                        cmsg_print(TERM_YELLOW, "its destruction. Remember, bring it to Mount Doom and destroy it.");
                        cmsg_print(TERM_YELLOW, "And *NEVER* use it.");
                }
        }

        return (FALSE);
}
bool quest_one_death_hook(int m_idx)
{
        int r_idx = m_list[m_idx].r_idx;
        bool ok = FALSE;

        if (a_info[ART_POWER].cur_num) return FALSE;

        /* Paranoia */
        if (cquest.status != QUEST_STATUS_TAKEN) return (FALSE);

        if (magik(30) && (r_idx == test_monster_name("Sauron, the Sorcerer")))
        {
                ok = TRUE;
        }
        else if (magik(10) && (r_idx == test_monster_name("Ar-Pharazon the Golden")))
        {
                ok = TRUE;
        }
        else if (magik(10) && (r_idx == test_monster_name("Shelob, Spider of Darkness")))
        {
                ok = TRUE;
        }
        else if (magik(10) && (r_idx == test_monster_name("The Watcher in the Water")))
        {
                ok = TRUE;
        }
        else if (magik(10) && (r_idx == test_monster_name("Glaurung, Father of the Dragons")))
        {
                ok = TRUE;
        }
        else if (magik(10) && (r_idx == test_monster_name("Feagwath, the Undead Sorcerer")))
        {
                ok = TRUE;
        }

        if (ok)
        {
                /* Get local object */
                object_type forge, *q_ptr = &forge;

                /* Mega-Hack -- Prepare to make "Grond" */
                object_prep(q_ptr, lookup_kind(TV_RING, SV_RING_POWER));

                /* Mega-Hack -- Mark this item as "the one ring" */
                q_ptr->name1 = ART_POWER;

                /* Mega-Hack -- Actually create "the one ring" */
                apply_magic(q_ptr, -1, TRUE, TRUE, TRUE);

                /* Drop it in the dungeon */
                drop_near(q_ptr, -1, m_list[m_idx].fy, m_list[m_idx].fx);
        }

        return (FALSE);
}
bool quest_one_dump_hook(int q_idx)
{
        if (cquest.status == QUEST_STATUS_FINISHED)
        {
                fprintf(hook_file, "\n You destroyed the One Ring, thus weakening Sauron.");
        }
        if (cquest.status == QUEST_STATUS_FAILED_DONE)
        {
                fprintf(hook_file, "\n You felt under the evil influence of the One Ring and decided to wear it.");
        }
        return (FALSE);
}
bool quest_one_init_hook(int q_idx)
{
        if ((cquest.status >= QUEST_STATUS_TAKEN) && (cquest.status < QUEST_STATUS_FINISHED))
        {
                add_hook(HOOK_MONSTER_DEATH, quest_one_death_hook, "one_death");
                add_hook(HOOK_DROP, quest_one_drop_hook, "one_drop");
                add_hook(HOOK_WIELD, quest_one_wield_hook, "one_wield");
                add_hook(HOOK_IDENTIFY, quest_one_identify_hook, "one_id");
        }
        if (cquest.status == QUEST_STATUS_UNTAKEN)
        {
                add_hook(HOOK_MOVE, quest_one_move_hook, "one_move");
        }
        add_hook(HOOK_CHAR_DUMP, quest_one_dump_hook, "one_dump");
        return (FALSE);
}
