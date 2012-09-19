#include "angband.h"

/*
 * Hook to determine if an object is contertible in an arrow/bolt
 */
static bool item_tester_hook_convertible(object_type *o_ptr)
{
	if((o_ptr->tval==TV_JUNK) || (o_ptr->tval==TV_SKELETON)) return TRUE;

	if ((o_ptr->tval == TV_CORPSE) && (o_ptr->sval == SV_SKELETON)) return TRUE;
	/* Assume not */
	return (FALSE);
}


/*
 * do_cmd_cast calls this function if the player's class
 * is 'archer'.
 */
static bool do_cmd_archer(void)
{
	int ext=0;
	char ch;

	object_type	forge;
	object_type     *q_ptr;

	char com[80];
	char o_name[MAX_NLEN];

	q_ptr = &forge;

	if(p_ptr->lev >= 20)
#ifdef JP
		sprintf(com, "[S]弾, [A]矢, [B]クロスボウの矢 :");
#else
		sprintf(com, "Create [S]hots, Create [A]rrow or Create [B]olt ?");
#endif
	else if(p_ptr->lev >= 10)
#ifdef JP
		sprintf(com, "[S]弾, [A]矢:");
#else
		sprintf(com, "Create [S]hots or Create [A]rrow ?");
#endif
	else
#ifdef JP
		sprintf(com, "[S]弾:");
#else
		sprintf(com, "Create [S]hots ?");
#endif

	if (p_ptr->confused)
	{
#ifdef JP
		msg_print("混乱してる！");
#else
		msg_print("You are too confused!");
#endif
		return FALSE;
	}

	if (p_ptr->blind)
	{
#ifdef JP
		msg_print("目が見えない！");
#else
		msg_print("You are blind!");
#endif
		return FALSE;
	}

	while (TRUE)
	{
		if (!get_com(com, &ch, TRUE))
		{
			return FALSE;
		}
		if (ch == 'S' || ch == 's')
		{
			ext = 1;
			break;
		}
		if ((ch == 'A' || ch == 'a')&&(p_ptr->lev >= 10))
		{
			ext = 2;
			break;
		}
		if ((ch == 'B' || ch == 'b')&&(p_ptr->lev >= 20))
		{
			ext = 3;
			break;
		}
	}

	/**********Create shots*********/
	if (ext == 1)
	{
		int x,y, dir;
		cave_type *c_ptr;

		if (!get_rep_dir(&dir, FALSE)) return FALSE;
		y = py + ddy[dir];
		x = px + ddx[dir];
		c_ptr = &cave[y][x];

		if (!have_flag(f_info[get_feat_mimic(c_ptr)].flags, FF_CAN_DIG))
		{
#ifdef JP
			msg_print("そこには岩石がない。");
#else
			msg_print("You need pile of rubble.");
#endif
			return FALSE;
		}
		else if (!cave_have_flag_grid(c_ptr, FF_CAN_DIG) || !cave_have_flag_grid(c_ptr, FF_HURT_ROCK))
		{
#ifdef JP
			msg_print("硬すぎて崩せなかった。");
#else
			msg_print("You failed to make ammo.");
#endif
		}
		else
		{
			s16b slot;

			/* Get local object */
			q_ptr = &forge;

			/* Hack -- Give the player some small firestones */
			object_prep(q_ptr, lookup_kind(TV_SHOT, m_bonus(1, p_ptr->lev) + 1));
			q_ptr->number = (byte)rand_range(15,30);
			object_aware(q_ptr);
			object_known(q_ptr);
			apply_magic(q_ptr, p_ptr->lev, AM_NO_FIXED_ART);
			q_ptr->discount = 99;

			slot = inven_carry(q_ptr);

			object_desc(o_name, q_ptr, 0);
#ifdef JP
			msg_format("%sを作った。", o_name);
#else
			msg_print("You make some ammo.");
#endif

			/* Auto-inscription */
			if (slot >= 0) autopick_alter_item(slot, FALSE);

			/* Destroy the wall */
			cave_alter_feat(y, x, FF_HURT_ROCK);

			p_ptr->update |= (PU_FLOW);
		}
	}
	/**********Create arrows*********/
	else if (ext == 2)
	{
		int item;
		cptr q, s;
		s16b slot;

		item_tester_hook = item_tester_hook_convertible;

		/* Get an item */
#ifdef JP
		q = "どのアイテムから作りますか？ ";
		s = "材料を持っていない。";
#else
		q = "Convert which item? ";
		s = "You have no item to convert.";
#endif
		if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return FALSE;

		/* Get the item (in the pack) */
		if (item >= 0)
		{
			q_ptr = &inventory[item];
		}

		/* Get the item (on the floor) */
		else
		{
			q_ptr = &o_list[0 - item];
		}

		/* Get local object */
		q_ptr = &forge;

		/* Hack -- Give the player some small firestones */
		object_prep(q_ptr, lookup_kind(TV_ARROW, m_bonus(1, p_ptr->lev)+ 1));
		q_ptr->number = (byte)rand_range(5, 10);
		object_aware(q_ptr);
		object_known(q_ptr);
		apply_magic(q_ptr, p_ptr->lev, AM_NO_FIXED_ART);

		q_ptr->discount = 99;

		object_desc(o_name, q_ptr, 0);
#ifdef JP
		msg_format("%sを作った。", o_name);
#else
		msg_print("You make some ammo.");
#endif

		if (item >= 0)
		{
			inven_item_increase(item, -1);
			inven_item_describe(item);
			inven_item_optimize(item);
		}
		else
		{
			floor_item_increase(0 - item, -1);
			floor_item_describe(0 - item);
			floor_item_optimize(0 - item);
		}

		slot = inven_carry(q_ptr);

		/* Auto-inscription */
		if (slot >= 0) autopick_alter_item(slot, FALSE);
	}
	/**********Create bolts*********/
	else if (ext == 3)
	{
		int item;
		cptr q, s;
		s16b slot;

		item_tester_hook = item_tester_hook_convertible;

		/* Get an item */
#ifdef JP
		q = "どのアイテムから作りますか？ ";
		s = "材料を持っていない。";
#else
		q = "Convert which item? ";
		s = "You have no item to convert.";
#endif
		if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return FALSE;

		/* Get the item (in the pack) */
		if (item >= 0)
		{
			q_ptr = &inventory[item];
		}

		/* Get the item (on the floor) */
		else
		{
			q_ptr = &o_list[0 - item];
		}

		/* Get local object */
		q_ptr = &forge;

		/* Hack -- Give the player some small firestones */
		object_prep(q_ptr, lookup_kind(TV_BOLT, m_bonus(1, p_ptr->lev)+1));
		q_ptr->number = (byte)rand_range(4, 8);
		object_aware(q_ptr);
		object_known(q_ptr);
		apply_magic(q_ptr, p_ptr->lev, AM_NO_FIXED_ART);

		q_ptr->discount = 99;

		object_desc(o_name, q_ptr, 0);
#ifdef JP
		msg_format("%sを作った。", o_name);
#else
		msg_print("You make some ammo.");
#endif

		if (item >= 0)
		{
			inven_item_increase(item, -1);
			inven_item_describe(item);
			inven_item_optimize(item);
		}
		else
		{
			floor_item_increase(0 - item, -1);
			floor_item_describe(0 - item);
			floor_item_optimize(0 - item);
		}

		slot = inven_carry(q_ptr);

		/* Auto-inscription */
		if (slot >= 0) autopick_alter_item(slot, FALSE);
	}
	return TRUE;
}

void create_ammo_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Create Ammo", ""));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Create arrows, bolts or shots.", ""));
		break;
	case SPELL_CAST:
		var_set_bool(res, do_cmd_archer());
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}