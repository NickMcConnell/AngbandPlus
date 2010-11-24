#include "angband.h"

static bool gain_magic(void)
{
	int item;
	int pval;
	int ext = 0;
	cptr q, s;
	object_type *o_ptr;
	char o_name[MAX_NLEN];

	/* Only accept legal items */
	item_tester_hook = item_tester_hook_recharge;

	/* Get an item */
#ifdef JP
q = "どのアイテムの魔力を取り込みますか? ";
s = "魔力を取り込めるアイテムがない。";
#else
	q = "Gain power of which item? ";
	s = "You have nothing to gain power.";
#endif

	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return (FALSE);

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}

	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item];
	}

	if (o_ptr->tval == TV_STAFF && o_ptr->sval == SV_STAFF_NOTHING)
	{
#ifdef JP
		msg_print("この杖には発動の為の能力は何も備わっていないようだ。");
#else
		msg_print("This staff doesn't have any magical ability.");
#endif
		return FALSE;
	}


	if (!object_is_known(o_ptr))
	{
#ifdef JP
		msg_print("鑑定されていないと取り込めない。");
#else
		msg_print("You need to identify before absorbing.");
#endif
		return FALSE;
	}

	if (o_ptr->timeout)
	{
#ifdef JP
		msg_print("充填中のアイテムは取り込めない。");
#else
		msg_print("This item is still charging.");
#endif
		return FALSE;
	}

	pval = o_ptr->pval;
	if (o_ptr->tval == TV_ROD)
		ext = 72;
	else if (o_ptr->tval == TV_WAND)
		ext = 36;

	if (o_ptr->tval == TV_ROD)
	{
		p_ptr->magic_num2[o_ptr->sval + ext] += o_ptr->number;
		if (p_ptr->magic_num2[o_ptr->sval + ext] > 99) p_ptr->magic_num2[o_ptr->sval + ext] = 99;
	}
	else
	{
		int num;
		for (num = o_ptr->number; num; num--)
		{
			int gain_num = pval;
			if (o_ptr->tval == TV_WAND) gain_num = (pval + num - 1) / num;
			if (p_ptr->magic_num2[o_ptr->sval + ext])
			{
				gain_num *= 256;
				gain_num = (gain_num/3 + randint0(gain_num/3)) / 256;
				if (gain_num < 1) gain_num = 1;
			}
			p_ptr->magic_num2[o_ptr->sval + ext] += gain_num;
			if (p_ptr->magic_num2[o_ptr->sval + ext] > 99) p_ptr->magic_num2[o_ptr->sval + ext] = 99;
			p_ptr->magic_num1[o_ptr->sval + ext] += pval * 0x10000;
			if (p_ptr->magic_num1[o_ptr->sval + ext] > 99 * 0x10000) p_ptr->magic_num1[o_ptr->sval + ext] = 99 * 0x10000;
			if (p_ptr->magic_num1[o_ptr->sval + ext] > p_ptr->magic_num2[o_ptr->sval + ext] * 0x10000) p_ptr->magic_num1[o_ptr->sval + ext] = p_ptr->magic_num2[o_ptr->sval + ext] * 0x10000;
			if (o_ptr->tval == TV_WAND) pval -= (pval + num - 1) / num;
		}
	}

	object_desc(o_name, o_ptr, 0);
	/* Message */
#ifdef JP
	msg_format("%sの魔力を取り込んだ。", o_name);
#else
	msg_format("You absorb magic of %s.", o_name);
#endif

	/* Eliminate the item (from the pack) */
	if (item >= 0)
	{
		inven_item_increase(item, -999);
		inven_item_describe(item);
		inven_item_optimize(item);
	}

	/* Eliminate the item (from the floor) */
	else
	{
		floor_item_increase(0 - item, -999);
		floor_item_describe(0 - item);
		floor_item_optimize(0 - item);
	}
	energy_use = 100;
	return TRUE;
}

void absorb_magic_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Absorb Magic", "魔力の取り込み"));
		break;
	case SPELL_DESC:
		var_set_string(res, T("", ""));
		break;
	case SPELL_CAST:
		var_set_bool(res, gain_magic());
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_absorb_magic(void) { return cast_spell(absorb_magic_spell); }