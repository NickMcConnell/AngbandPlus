/* File: help.c */

/* Purpose: ingame help */

/*
 * Copyright (c) 2001 DarkGod
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

bool help_move(int loc)
{
        int y = loc >> 8, x = loc & 0xFF;
        cave_type *c_ptr = &cave[y][x];

        if (!(p_ptr->help.help1 & HELP1_BETWEEN) && (c_ptr->feat == FEAT_BETWEEN))
        {
                p_ptr->help.help1 |= HELP1_BETWEEN;
                cmsg_print(TERM_YELLOW, "Between Gates can be entered by pressing the > key. They will transport");
                cmsg_print(TERM_YELLOW, "you to an other gate, but beware of the cold damage that might kill you.");
        }
        else if (!(p_ptr->help.help1 & HELP1_ALTAR) && (c_ptr->feat >= FEAT_ALTAR_HEAD) && (c_ptr->feat <= FEAT_ALTAR_TAIL))
        {
                p_ptr->help.help1 |= HELP1_ALTAR;
                cmsg_print(TERM_YELLOW, "Altars are the way to reach the Valar and the Maiars, powers of the world,");
                cmsg_print(TERM_YELLOW, "usualy called Gods. You can press O to offer a god a sacrifice and to");
                cmsg_print(TERM_YELLOW, "become his/her follower. Beware that once you follow a god it is hard to change.");
        }
        else if (!(p_ptr->help.help1 & HELP1_FOUNTAIN) && (c_ptr->feat == FEAT_FOUNTAIN))
        {
                p_ptr->help.help1 |= HELP1_FOUNTAIN;
                cmsg_print(TERM_YELLOW, "Fountains are always magical, you can quaff from them by pressing H.");
                cmsg_print(TERM_YELLOW, "Beware that unlike potions they cannot be identified.");
        }

        if (!(p_ptr->help.help1 & HELP1_IDENTIFY) && (c_ptr->o_idx))
        {
                p_ptr->help.help1 |= HELP1_IDENTIFY;
                cmsg_print(TERM_YELLOW, "So you found your first item, nice eh ? Now when you stumble across");
                cmsg_print(TERM_YELLOW, "objects you can pick them up by pressing g, and if you are wondering");
                cmsg_print(TERM_YELLOW, "what they do press I to get some basic information.");
                cmsg_print(TERM_YELLOW, "You may also want to identify them with scrolls, staves, rods or spells.");
        }

        return (FALSE);
}

bool help_end_turn(int loc)
{
        if (!(p_ptr->help.help1 & HELP1_WILD_MODE) && (p_ptr->wild_mode))
        {
                p_ptr->help.help1 |= HELP1_WILD_MODE;
                cmsg_print(TERM_YELLOW, "Ahh wilderness travel... The overview mode will allow you to travel");
                cmsg_print(TERM_YELLOW, "fast, but that comes to the cost of GREATLY increased food cunsumption.");
                cmsg_print(TERM_YELLOW, "So you should really watch your hungriness status.");
        }

        return (FALSE);
}

void ingame_help(bool enable)
{
        if (enable)
        {
                ingame_help(FALSE);
                add_hook(HOOK_MOVE, help_move, "help_move");
                add_hook(HOOK_END_TURN, help_end_turn, "help_end_turn");
        }
        else
        {
                del_hook(HOOK_MOVE, help_move);
                del_hook(HOOK_END_TURN, help_end_turn);
        }
}
