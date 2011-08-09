/* File: racial.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

/* Purpose: Racial powers (and mutations) */

#include "angband.h"


bool can_do_cmd_cast(void)
{
	if (dun_level && (d_info[dungeon_type].flags1 & DF1_NO_MAGIC))
	{
#ifdef JP
		msg_print("ダンジョンが魔法を吸収した！");
#else
		msg_print("The dungeon absorbs all attempted magic!");
#endif
		msg_print(NULL);
		return FALSE;
	}
	else if (p_ptr->anti_magic)
	{
#ifdef JP
		msg_print("反魔法バリアが魔法を邪魔した！");
#else
		msg_print("An anti-magic shell disrupts your magic!");
#endif
		return FALSE;
	}
	else if (IS_SHERO())
	{
#ifdef JP
		msg_format("狂戦士化していて頭が回らない！");
#else
		msg_format("You cannot think directly!");
#endif
		return FALSE;
	}
	else
		return TRUE;
}

void stop_mouth(void)
{
	if (music_singing_any()) bard_stop_singing();
	if (hex_spelling_any()) stop_hex_spell_all();
}


