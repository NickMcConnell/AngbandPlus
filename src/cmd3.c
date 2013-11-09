/* File: cmd3.c */

/* Purpose: Inventory commands */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"



/*
 * Display inventory
 */
void do_cmd_inven(void)
{
	char out_val[160];


	/* Note that we are in "inventory" mode */
	command_wrk = FALSE;

	/* Note that we are in "inventory" mode */
	if (easy_floor) command_wrk = (USE_INVEN);

	/* Save screen */
	screen_save();

	/* Hack -- show empty slots */
	item_tester_full = TRUE;

	/* Display the inventory */
	(void)show_inven(0);

	/* Hack -- hide empty slots */
	item_tester_full = FALSE;

#ifdef JP
	sprintf(out_val, "持ち物： 合計 %3d.%1d kg (限界の%ld%%) コマンド: ",
	        lbtokg1(p_ptr->total_weight) , lbtokg2(p_ptr->total_weight) ,
	        (p_ptr->total_weight * 100) / ((adj_str_wgt[p_ptr->stat_ind[A_STR]] * ((pclass_is_(CLASS_TERRORKNIGHT) || pclass_is_(CLASS_RELICSKNIGHT)) ? 150 : 100)) 
	        / 2));
#else
	sprintf(out_val, "Inventory: carrying %d.%d pounds (%ld%% of capacity). Command: ",
	        (int)(p_ptr->total_weight / 10), (int)(p_ptr->total_weight % 10),
	        (p_ptr->total_weight * 100) / ((adj_str_wgt[p_ptr->stat_ind[A_STR]] * ((pclass_is_(CLASS_TERRORKNIGHT) || pclass_is_(CLASS_RELICSKNIGHT)) ? 150 : 100)) / 2));
#endif


	/* Get a command */
	prt(out_val, 0, 0);

	/* Get a new command */
	command_new = inkey();

	/* Load screen */
	screen_load();


	/* Process "Escape" */
	if (command_new == ESCAPE)
	{
		int wid, hgt;

		/* Get size */
		Term_get_size(&wid, &hgt);

		/* Reset stuff */
		command_new = 0;
		command_gap = wid - 30;
	}

	/* Process normal keys */
	else
	{
		/* Hack -- Use "display" mode */
		command_see = TRUE;
	}
}


/*
 * Display equipment
 */
void do_cmd_equip(void)
{
	char out_val[160];


	/* Note that we are in "equipment" mode */
	command_wrk = TRUE;

	/* Note that we are in "equipment" mode */
	if (easy_floor) command_wrk = (USE_EQUIP);

	/* Save the screen */
	screen_save();

	/* Hack -- show empty slots */
	item_tester_full = TRUE;

	/* Display the equipment */
	(void)show_equip(0);

	/* Hack -- undo the hack above */
	item_tester_full = FALSE;

	/* Build a prompt */
#ifdef JP
	sprintf(out_val, "装備： 合計 %3d.%1d kg (限界の%ld%%) コマンド: ",
	        lbtokg1(p_ptr->total_weight) , lbtokg2(p_ptr->total_weight) ,
	        (p_ptr->total_weight * 100) / ((adj_str_wgt[p_ptr->stat_ind[A_STR]] * ((pclass_is_(CLASS_TERRORKNIGHT) || pclass_is_(CLASS_RELICSKNIGHT)) ? 150 : 100)) 
	        / 2));
#else
	sprintf(out_val, "Equipment: carrying %d.%d pounds (%ld%% of capacity). Command: ",
	        (int)(p_ptr->total_weight / 10), (int)(p_ptr->total_weight % 10),
	        (p_ptr->total_weight * 100) / ((adj_str_wgt[p_ptr->stat_ind[A_STR]] * ((pclass_is_(CLASS_TERRORKNIGHT) || pclass_is_(CLASS_RELICSKNIGHT)) ? 150 : 100)) / 2));
#endif


	/* Get a command */
	prt(out_val, 0, 0);

	/* Get a new command */
	command_new = inkey();

	/* Restore the screen */
	screen_load();


	/* Process "Escape" */
	if (command_new == ESCAPE)
	{
		int wid, hgt;

		/* Get size */
		Term_get_size(&wid, &hgt);

		/* Reset stuff */
		command_new = 0;
		command_gap = wid - 30;
	}

	/* Process normal keys */
	else
	{
		/* Enter "display" mode */
		command_see = TRUE;
	}
}


/*
 * The "wearable" tester
 */
static bool item_tester_hook_wear(object_type *o_ptr)
{
	u32b flgs[TR_FLAG_SIZE];

	/* Extract the flags */
	object_flags(o_ptr, flgs);

	/* Sexual restriction */
	if (have_flag(flgs, TR_FEMALE_ONLY))
	{
		if (p_ptr->psex != SEX_FEMALE) return FALSE;
	}
	if (have_flag(flgs, TR_MALE_ONLY))
	{
		if (p_ptr->psex != SEX_MALE) return FALSE;
	}

	/* Check for a usable slot */
	if (wield_slot(o_ptr) >= INVEN_RARM) return (TRUE);

	/* Assume not wearable */
	return (FALSE);
}


static bool item_tester_hook_mochikae(object_type *o_ptr)
{
	/* Check for a usable slot */
	if (((o_ptr->tval >= TV_DIGGING) && (o_ptr->tval <= TV_SWORD)) ||
	    (o_ptr->tval == TV_SHIELD) || (o_ptr->tval == TV_CARD) || (o_ptr->tval == TV_TRUMP)) return (TRUE);

	/* Assume not wearable */
	return (FALSE);
}


/*
 * Wield or wear a single item from the pack or floor
 */
void do_cmd_wield(void)
{
	int i, item, slot;

	object_type forge;
	object_type *q_ptr;

	object_type *o_ptr;

	cptr act;

	char o_name[MAX_NLEN];

	cptr q, s;

	/* Restrict the choices */
	item_tester_hook = item_tester_hook_wear;

	/* Get an item */
#ifdef JP
	q = "どれを装備しますか? ";
	s = "装備可能なアイテムがない。";
#else
	q = "Wear/Wield which item? ";
	s = "You have nothing you can wear or wield.";
#endif

	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

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


	/* Check the slot */
	slot = wield_slot(o_ptr);
#if 1 /* EASY_RING -- TNB */

	if ((o_ptr->tval == TV_RING) && inventory[INVEN_LEFT].k_idx &&
		inventory[INVEN_RIGHT].k_idx)
	{
		/* Restrict the choices */
		item_tester_tval = TV_RING;
		item_tester_no_ryoute = TRUE;

		/* Choose a ring from the equipment only */
#ifdef JP
q = "どちらの指輪と取り替えますか?";
#else
		q = "Replace which ring? ";
#endif

#ifdef JP
s = "おっと。";
#else
		s = "Oops.";
#endif

		if (!get_item(&slot, q, s, (USE_EQUIP)))
			return;
	}

#endif /* EASY_RING -- TNB */

	if (((o_ptr->tval == TV_SHIELD) || (o_ptr->tval == TV_CARD) || (o_ptr->tval == TV_TRUMP)) &&
		buki_motteruka(INVEN_RARM) && buki_motteruka(INVEN_LARM))
	{
		/* Restrict the choices */
		item_tester_hook = object_is_melee_weapon;
		item_tester_no_ryoute = TRUE;

		/* Choose a weapon from the equipment only */
#ifdef JP
q = "どちらの武器と取り替えますか?";
#else
		q = "Replace which weapon? ";
#endif

#ifdef JP
s = "おっと。";
#else
		s = "Oops.";
#endif

		if (!get_item(&slot, q, s, (USE_EQUIP)))
			return;
		if ((slot == INVEN_RARM) && !object_is_cursed(&inventory[INVEN_RARM]))
		{
			object_type *or_ptr = &inventory[INVEN_RARM];
			object_type *ol_ptr = &inventory[INVEN_LARM];
			object_type *otmp_ptr;
			object_type object_tmp;
			char ol_name[MAX_NLEN];

			otmp_ptr = &object_tmp;

			object_desc(ol_name, ol_ptr, (OD_OMIT_PREFIX | OD_NAME_ONLY));

			object_copy(otmp_ptr, ol_ptr);
			object_copy(ol_ptr, or_ptr);
			object_copy(or_ptr, otmp_ptr);
#ifdef JP
			msg_format("%sを%sに構えなおした。", ol_name, left_hander ? "左手" : "右手");
#else
			msg_format("You wield %s at %s hand.", ol_name, left_hander ? "left" : "right");
#endif

			if (p_ptr->magical_weapon) set_magical_weapon(0, 0, slot, FALSE);
			slot = INVEN_LARM;
		}
	}

	/* 二刀流にするかどうか */
	if ((o_ptr->tval >= TV_DIGGING) && (o_ptr->tval <= TV_SWORD) && (slot == INVEN_LARM))
	{
#ifdef JP
		if (!get_check("二刀流で戦いますか？"))
#else
		if (!get_check("Dual wielding? "))
#endif
		{
			slot = INVEN_RARM;
		}
	}

	if ((o_ptr->tval >= TV_DIGGING) && (o_ptr->tval <= TV_SWORD) &&
	    inventory[INVEN_LARM].k_idx &&
		inventory[INVEN_RARM].k_idx)
	{
		/* Restrict the choices */
		item_tester_hook = item_tester_hook_mochikae;

		/* Choose a ring from the equipment only */
#ifdef JP
q = "どちらの手に装備しますか?";
#else
		q = "Equip which hand? ";
#endif

#ifdef JP
s = "おっと。";
#else
		s = "Oops.";
#endif

		if (!get_item(&slot, q, s, (USE_EQUIP)))
			return;
	}

	/* Prevent wielding into a cursed slot */
	if (object_is_cursed(&inventory[slot]))
	{
		/* Describe it */
		object_desc(o_name, &inventory[slot], (OD_OMIT_PREFIX | OD_NAME_ONLY));

		/* Message */
#ifdef JP
		msg_format("%s%sは呪われているようだ。",
		           describe_use(slot) , o_name );
#else
		msg_format("The %s you are %s appears to be cursed.",
		           o_name, describe_use(slot));
#endif


		/* Cancel the command */
		return;
	}

	if (object_is_cursed(o_ptr) && confirm_wear &&
	    (object_is_known(o_ptr) || (o_ptr->ident & IDENT_SENSE)))
	{
		char dummy[MAX_NLEN+80];

		/* Describe it */
		object_desc(o_name, o_ptr, (OD_OMIT_PREFIX | OD_NAME_ONLY));

#ifdef JP
		sprintf(dummy, "本当に%s{呪われている}を使いますか？", o_name);
#else
		sprintf(dummy, "Really use the %s {cursed}? ", o_name);
#endif


		if (!get_check(dummy))
			return;
	}

	/* Check if completed a quest */
	for (i = 0; i < max_quests; i++)
	{
		if ((quest[i].type == QUEST_TYPE_FIND_ARTIFACT) &&
		    (quest[i].status == QUEST_STATUS_TAKEN) &&
		    (quest[i].k_idx == o_ptr->name1))
		{
			if (record_fix_quest) do_cmd_write_nikki(NIKKI_FIX_QUEST_C, i, NULL);
			quest[i].status = QUEST_STATUS_COMPLETED;
			quest[i].complev = (byte)p_ptr->lev;

			/* Make a sound */
			sound(SOUND_QUEST);

#ifdef JP
			msg_print("クエストを達成した！");
#else
			msg_print("You completed the quest!");
#endif

			msg_print(NULL);
			if (quest_is_fixed(i) && !(quest[i].flags & QUEST_FLAG_ALI_CHAOS)) change_your_alignment(ALI_LNC, 10);
			else change_your_alignment(ALI_LNC, -10);

			if (quest[i].flags & QUEST_FLAG_ALI_EVIL) change_your_alignment(ALI_GNE, -10);
			else change_your_alignment(ALI_GNE, 10);
		}
	}

	/* Player found the "Runeweapon" finally! */
	if (object_is_snapdragon_runeweapon(o_ptr))
	{
		runeweapon_type *runeweapon = &runeweapon_list[o_ptr->xtra3];
		if (!(runeweapon->status & RW_STATUS_FOUND))
		{
			char t_name[MAX_NLEN];
			runeweapon->status |= (RW_STATUS_FOUND);

			object_desc(t_name, o_ptr, OD_NAME_ONLY);
			if (runeweapon->ancestor[0])
			{
#ifdef JP
				msg_format("%d階、先祖%sの魂を封じた%sをついに手に入れた！", dun_level, runeweapon->ancestor, t_name);
#else
				msg_format("You found %s spirit of ancestor %s sealed in finally on dungeon level %d!", t_name, runeweapon->ancestor, dun_level);
#endif
			}
			else
			{
#ifdef JP
				msg_format("%d階、ヴァレリアの覇者jnkの魂を封じた%sをついに手に入れた！", dun_level, t_name);
#else
				msg_format("You found %s spirit of jnk the Champion of Valeria sealed in finally on dungeon level %d!", t_name, dun_level);
#endif
			}
			if (record_fix_art) do_cmd_write_nikki(NIKKI_ART, 0, t_name);
		}
	}

	if (easy_band) identify_item(o_ptr);

	/* Take a turn */
	energy_use = 100;

	/* Get local object */
	q_ptr = &forge;

	/* Obtain local object */
	object_copy(q_ptr, o_ptr);

	/* Modify quantity */
	q_ptr->number = 1;

	/* Decrease the item (from the pack) */
	if (item >= 0)
	{
		inven_item_increase(item, -1);
		inven_item_optimize(item);
	}

	/* Decrease the item (from the floor) */
	else
	{
		floor_item_increase(0 - item, -1);
		floor_item_optimize(0 - item);
	}

	/* Access the wield slot */
	o_ptr = &inventory[slot];

	/* Take off existing item */
	if (o_ptr->k_idx)
	{
		/* Take off existing item */
		(void)inven_takeoff(slot, 255);
	}

	/* Wear the new stuff */
	object_copy(o_ptr, q_ptr);

	/* Player touches it */
	o_ptr->marked |= OM_TOUCHED;

	/* Increase the weight */
	p_ptr->total_weight += q_ptr->weight;

	/* Increment the equip counter by hand */
	equip_cnt++;

	/* Where is the item now */
	if (slot == INVEN_RARM)
	{
		if ((o_ptr->tval != TV_SHIELD) && (o_ptr->tval != TV_CARD) && (o_ptr->tval != TV_TRUMP) && (empty_hands() & EMPTY_HAND_LARM) && ((o_ptr->weight > 99) || (o_ptr->tval == TV_POLEARM)) && (!p_ptr->riding || (p_ptr->pet_extra_flags & PF_RYOUTE)))
#ifdef JP
			act = "を両手で構えた";
#else
			act = "You are wielding";
#endif
		else
#ifdef JP
			act = (left_hander ? "を左手に装備した" : "を右手に装備した");
#else
			act = "You are wielding";
#endif

	}
	else if (slot == INVEN_LARM)
	{
#ifdef JP
		act = (left_hander ? "を右手に装備した" : "を左手に装備した");
#else
		act = "You are wielding";
#endif

	}
	else if (slot == INVEN_BOW)
	{
#ifdef JP
		act = "を射撃用に装備した";
#else
		act = "You are shooting with";
#endif

		if (pclass_is_(CLASS_GUNNER)) energy_use = 0;
	}
	else if (slot == INVEN_LITE)
	{
#ifdef JP
		act = "を光源にした";
#else
		act = "Your light source is";
#endif

	}
	else
	{
#ifdef JP
		act = "を装備した";
#else
		act = "You are wearing";
#endif

	}

	/* Describe the result */
	object_desc(o_name, o_ptr, 0);

	/* Message */
#ifdef JP
	msg_format("%s(%c)%s。", o_name, index_to_label(slot), act );
#else
	msg_format("%s %s (%c).", act, o_name, index_to_label(slot));
#endif


	/* Cursed! */
	if (object_is_cursed(o_ptr))
	{
		/* Warn the player */
#ifdef JP
		msg_print("うわ！ すさまじく冷たい！");
#else
		msg_print("Oops! It feels deathly cold!");
#endif

		change_your_alignment(ALI_LNC, -1);

		/* Note the curse */
		o_ptr->ident |= (IDENT_SENSE);
	}

	/* 属性転換のアミュレット仕様変更 */
	if ((o_ptr->tval == TV_AMULET) && (o_ptr->sval == SV_AMULET_ALIGNMENT))
	{
		change_alignment_lnc();
		
		inven_item_increase(INVEN_NECK, -1);
		inven_item_optimize(INVEN_NECK);
#ifdef JP
		msg_print("アミュレットは粉々に砕け散った!");
#else
		msg_print("The Amulet has destroyed!");
#endif
	}

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Recalculate torch */
	p_ptr->update |= (PU_TORCH);

	/* Recalculate mana */
	p_ptr->update |= (PU_MANA);

	p_ptr->redraw |= (PR_EQUIPPY);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);
}


void kamaenaoshi(int item)
{
	object_type *o_ptr, *o2_ptr;
	char o_name[MAX_NLEN];

	if ((item == INVEN_RARM) && buki_motteruka(INVEN_LARM))
	{
		o_ptr = &inventory[INVEN_RARM];
		o2_ptr = &inventory[INVEN_LARM];
		object_copy(o_ptr, o2_ptr);
		p_ptr->total_weight += o2_ptr->weight;
		inven_item_increase(INVEN_LARM,-1);
		inven_item_optimize(INVEN_LARM);
		object_desc(o_name, o_ptr, 0);
		if (((o_ptr->weight > 99) || (o_ptr->tval == TV_POLEARM)) && (!p_ptr->riding || (p_ptr->pet_extra_flags & PF_RYOUTE)))
#ifdef JP
			msg_format("%sを両手で構えた。", o_name );
#else
			msg_format("You are wielding %s with two-handed.", o_name );
#endif
		 else
#ifdef JP
			msg_format("%sを%sで構えた。", o_name, (left_hander ? "左手" : "右手"));
#else
			msg_format("You are wielding %s with %s hand.", o_name, (left_hander ? "left":"right") );
#endif
	}
	else if ((item == INVEN_LARM) && buki_motteruka(INVEN_RARM))
	{
		o_ptr = &inventory[INVEN_RARM];
		object_desc(o_name, o_ptr, 0);
		if (((o_ptr->weight > 99) || (o_ptr->tval == TV_POLEARM)) && (!p_ptr->riding || (p_ptr->pet_extra_flags & PF_RYOUTE)))
#ifdef JP
			msg_format("%sを両手で構えた。", o_name );
#else
			msg_format("You are wielding %s with two-handed.", o_name );
#endif
	}
	else if ((item == INVEN_LARM) && !(empty_hands() & EMPTY_HAND_RARM))
	{
		o_ptr = &inventory[INVEN_LARM];
		o2_ptr = &inventory[INVEN_RARM];
		object_copy(o_ptr, o2_ptr);
		p_ptr->total_weight += o2_ptr->weight;
		inven_item_increase(INVEN_RARM,-1);
		inven_item_optimize(INVEN_RARM);
		object_desc(o_name, o_ptr, 0);
#ifdef JP
		msg_format("%sを持ち替えた。", o_name );
#else
		msg_format("You switched hand of %s.", o_name );
#endif
	}
}


/*
 * Take off an item
 */
void do_cmd_takeoff(void)
{
	int item;

	object_type *o_ptr;

	cptr q, s;

	item_tester_no_ryoute = TRUE;
	/* Get an item */
#ifdef JP
	q = "どれを装備からはずしますか? ";
	s = "はずせる装備がない。";
#else
	q = "Take off which item? ";
	s = "You are not wearing anything to take off.";
#endif

	if (!get_item(&item, q, s, (USE_EQUIP))) return;

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


	if (dungeon_type == DUNGEON_HEAVEN)
	{
		if (object_is_known(o_ptr) && (o_ptr->name1 == ART_BRUNHILD))
		{
			if (inven_cnt >= INVEN_PACK)
			{
				msg_print("天界から帰還するにはブリュンヒルドが必要です。");
				return;
			}
		}
	}


	/* Item is cursed */
	if (object_is_cursed(o_ptr))
	{
		if ((o_ptr->curse_flags & TRC_PERMA_CURSE) || !pclass_is_(CLASS_TERRORKNIGHT) || !pclass_is_(CLASS_RELICSKNIGHT))
		{
			/* Oops */
#ifdef JP
			msg_print("ふーむ、どうやら呪われているようだ。");
#else
			msg_print("Hmmm, it seems to be cursed.");
#endif

			/* Nope */
			return;
		}

		if (((o_ptr->curse_flags & TRC_HEAVY_CURSE) && one_in_(7)) || one_in_(4))
		{
#ifdef JP
			msg_print("呪われた装備を力づくで剥がした！");
#else
			msg_print("You teared a cursed equipment off by sheer strength!");
#endif

			/* Hack -- Assume felt */
			o_ptr->ident |= (IDENT_SENSE);

			o_ptr->curse_flags = 0L;

			/* Take note */
			o_ptr->feeling = FEEL_NONE;

			/* Recalculate the bonuses */
			p_ptr->update |= (PU_BONUS);

			/* Window stuff */
			p_ptr->window |= (PW_EQUIP);

#ifdef JP
			msg_print("呪いを打ち破った。");
#else
			msg_print("You break the curse.");
#endif
		}
		else
		{
#ifdef JP
			msg_print("装備を外せなかった。");
#else
			msg_print("You couldn't remove the equipment.");
#endif
			energy_use = 50;
			return;
		}
	}

	/* Take a partial turn */
	energy_use = 50;

	/* Take off the item */
	(void)inven_takeoff(item, 255);
	if (pclass_is_(CLASS_GUNNER) && (item == INVEN_BOW)) energy_use = 0;

	kamaenaoshi(item);

	p_ptr->redraw |= (PR_EQUIPPY);
}


/*
 * Drop an item
 */
void do_cmd_drop(void)
{
	int item, amt = 1;

	object_type *o_ptr;

	cptr q, s;

	item_tester_no_ryoute = TRUE;
	/* Get an item */
#ifdef JP
	q = "どのアイテムを落としますか? ";
	s = "落とせるアイテムを持っていない。";
#else
	q = "Drop which item? ";
	s = "You have nothing to drop.";
#endif

	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN))) return;

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


	if (dungeon_type == DUNGEON_HEAVEN)
	{
		if (object_is_known(o_ptr) && (o_ptr->name1 == ART_BRUNHILD))
		{
			msg_print("天界から帰還するにはブリュンヒルドが必要です。");
			return;
		}
	}


	/* Hack -- Cannot remove cursed items */
	if ((item >= INVEN_RARM) && object_is_cursed(o_ptr))
	{
		/* Oops */
#ifdef JP
		msg_print("ふーむ、どうやら呪われているようだ。");
#else
		msg_print("Hmmm, it seems to be cursed.");
#endif


		/* Nope */
		return;
	}


	/* See how many items */
	if (o_ptr->number > 1)
	{
		/* Get a quantity */
		amt = get_quantity(NULL, o_ptr->number);

		/* Allow user abort */
		if (amt <= 0) return;
	}


	/* Take a partial turn */
	energy_use = 50;

	/* Drop (some of) the item */
	inven_drop(item, amt);

	if ((item == INVEN_RARM) || (item == INVEN_LARM)) kamaenaoshi(item);

	p_ptr->redraw |= (PR_EQUIPPY);
}


/*
 * Destroy an item
 */
void do_cmd_destroy(void)
{
	int			item, amt = 1;
	int			old_number;

	bool		force = FALSE;

	object_type		*o_ptr;
	object_type             forge;
	object_type             *q_ptr = &forge;

	char		o_name[MAX_NLEN];

	char		out_val[MAX_NLEN+40];

	cptr q, s;

	/* Hack -- force destruction */
	if (command_arg > 0) force = TRUE;


	/* Get an item */
#ifdef JP
	q = "どのアイテムを壊しますか? ";
	s = "壊せるアイテムを持っていない。";
#else
	q = "Destroy which item? ";
	s = "You have nothing to destroy.";
#endif

	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

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


	/* See how many items */
	if (o_ptr->number > 1)
	{
		/* Get a quantity */
		amt = get_quantity(NULL, o_ptr->number);

		/* Allow user abort */
		if (amt <= 0) return;
	}


	/* Describe the object */
	old_number = o_ptr->number;
	o_ptr->number = amt;
	object_desc(o_name, o_ptr, 0);
	o_ptr->number = old_number;

	/* Verify unless quantity given */
	if (!force)
	{
		if (confirm_destroy || (object_value(o_ptr) > 0))
		{
			/* Make a verification */
#ifdef JP
		sprintf(out_val, "本当に%sを壊しますか? ", o_name);
#else
			sprintf(out_val, "Really destroy %s? ", o_name);
#endif

			if (!get_check(out_val)) return;
		}
	}

	/* Take a turn */
	energy_use = 100;

	/* Artifacts cannot be destroyed */
	if (!can_player_destroy_object(o_ptr))
	{
		energy_use = 0;

		/* Message */
#ifdef JP
		msg_format("%sは破壊不可能だ。", o_name);
#else
		msg_format("You cannot destroy %s.", o_name);
#endif

		/* Done */
		return;
	}

	object_copy(q_ptr, o_ptr);

	/* Message */
#ifdef JP
	msg_format("%sを壊した。", o_name);
#else
	msg_format("You destroy %s.", o_name);
#endif

	sound(SOUND_DESTITEM);

	/* Reduce the charges of rods/wands */
	reduce_charges(o_ptr, amt);

	/* Eliminate the item (from the pack) */
	if (item >= 0)
	{
		inven_item_increase(item, -amt);
		inven_item_describe(item);
		inven_item_optimize(item);
	}

	/* Eliminate the item (from the floor) */
	else
	{
		floor_item_increase(0 - item, -amt);
		floor_item_describe(0 - item);
		floor_item_optimize(0 - item);
	}

	if (q_ptr->tval == TV_HOLY_BOOK)
		change_your_alignment(ALI_GNE, -1 * q_ptr->sval);

	else if (q_ptr->tval == TV_DEATH_BOOK)
		change_your_alignment(ALI_GNE, q_ptr->sval);

	else if (q_ptr->tval == TV_SYMBIOTIC_BOOK)
		change_your_alignment(ALI_LNC, -1);

	else if (q_ptr->tval == TV_WITCH_BOOK)
		change_your_alignment(ALI_LNC, q_ptr->sval);

	else if (q_ptr->tval == TV_CRUSADE_BOOK)
		change_your_alignment(ALI_GNE, -1 * q_ptr->sval);

	else if (object_value_real(q_ptr) > 30000)
		change_your_alignment(ALI_LNC, -2);

	else if (object_value_real(q_ptr) > 10000)
		change_your_alignment(ALI_LNC, -1);
}


/*
 * Observe an item which has been *identify*-ed
 */
void do_cmd_observe(void)
{
	int			item;

	object_type		*o_ptr;

	char		o_name[MAX_NLEN];

	cptr q, s;

	item_tester_no_ryoute = TRUE;
	/* Get an item */
#ifdef JP
	q = "どのアイテムを調べますか? ";
	s = "調べられるアイテムがない。";
#else
	q = "Examine which item? ";
	s = "You have nothing to examine.";
#endif

	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR))) return;

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


	/* Require full knowledge */
	if (!(o_ptr->ident & IDENT_MENTAL))
	{
#ifdef JP
		msg_print("このアイテムについて特に知っていることはない。");
#else
		msg_print("You have no special knowledge about that item.");
#endif

		return;
	}


	/* Description */
	object_desc(o_name, o_ptr, 0);

	/* Describe */
#ifdef JP
	msg_format("%sを調べている...", o_name);
#else
	msg_format("Examining %s...", o_name);
#endif

	/* Describe it fully */
#ifdef JP
	if (!screen_object(o_ptr, NULL, TRUE)) msg_print("特に変わったところはないようだ。");
#else
	if (!screen_object(o_ptr, NULL, TRUE)) msg_print("You see nothing special.");
#endif

}



/*
 * Remove the inscription from an object
 * XXX Mention item (when done)?
 */
void do_cmd_uninscribe(void)
{
	int   item;

	object_type *o_ptr;

	cptr q, s;

	item_tester_no_ryoute = TRUE;
	/* Get an item */
#ifdef JP
	q = "どのアイテムの銘を消しますか? ";
	s = "銘を消せるアイテムがない。";
#else
	q = "Un-inscribe which item? ";
	s = "You have nothing to un-inscribe.";
#endif

	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR))) return;

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

	/* Nothing to remove */
	if (!o_ptr->inscription)
	{
#ifdef JP
		msg_print("このアイテムには消すべき銘がない。");
#else
		msg_print("That item had no inscription to remove.");
#endif

		return;
	}

	/* Message */
#ifdef JP
	msg_print("銘を消した。");
#else
	msg_print("Inscription removed.");
#endif


	/* Remove the incription */
	o_ptr->inscription = 0;

	/* Combine the pack */
	p_ptr->notice |= (PN_COMBINE);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP);

	/* .や$の関係で, 再計算が必要なはず -- henkma */
	p_ptr->update |= (PU_BONUS);

}


/*
 * Inscribe an object with a comment
 */
void do_cmd_inscribe(void)
{
	int			item;

	object_type		*o_ptr;

	char		o_name[MAX_NLEN];

	char		out_val[80];

	cptr q, s;

	item_tester_no_ryoute = TRUE;
	/* Get an item */
#ifdef JP
	q = "どのアイテムに銘を刻みますか? ";
	s = "銘を刻めるアイテムがない。";
#else
	q = "Inscribe which item? ";
	s = "You have nothing to inscribe.";
#endif

	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR))) return;

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

	/* Describe the activity */
	object_desc(o_name, o_ptr, OD_OMIT_INSCRIPTION);

	/* Message */
#ifdef JP
	msg_format("%sに銘を刻む。", o_name);
#else
	msg_format("Inscribing %s.", o_name);
#endif

	msg_print(NULL);

	/* Start with nothing */
	strcpy(out_val, "");

	/* Use old inscription */
	if (o_ptr->inscription)
	{
		/* Start with the old inscription */
		strcpy(out_val, quark_str(o_ptr->inscription));
	}

	/* Get a new inscription (possibly empty) */
#ifdef JP
	if (get_string("銘: ", out_val, 80))
#else
	if (get_string("Inscription: ", out_val, 80))
#endif
	{
		/* Save the inscription */
		o_ptr->inscription = quark_add(out_val);

		/* Combine the pack */
		p_ptr->notice |= (PN_COMBINE);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

		/* .や$の関係で, 再計算が必要なはず -- henkma */
		p_ptr->update |= (PU_BONUS);
	}
}



/*
 * An "item_tester_hook" for refilling lanterns
 */
static bool item_tester_refill_lantern(object_type *o_ptr)
{
	/* Flasks of oil are okay */
	if (o_ptr->tval == TV_FLASK) return (TRUE);

	/* Laterns are okay */
	if ((o_ptr->tval == TV_LITE) &&
	    (o_ptr->sval == SV_LITE_LANTERN)) return (TRUE);

	/* Assume not okay */
	return (FALSE);
}


/*
 * Refill the players lamp (from the pack or floor)
 */
static void do_cmd_refill_lamp(void)
{
	int item;

	object_type *o_ptr;
	object_type *j_ptr;

	cptr q, s;


	/* Restrict the choices */
	item_tester_hook = item_tester_refill_lantern;

	/* Get an item */
#ifdef JP
	q = "どの油つぼから注ぎますか? ";
	s = "油つぼがない。";
#else
	q = "Refill with which flask? ";
	s = "You have no flasks of oil.";
#endif

	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

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


	/* Take a partial turn */
	energy_use = 50;

	/* Access the lantern */
	j_ptr = &inventory[INVEN_LITE];

	/* Refuel */
	j_ptr->xtra4 += o_ptr->xtra4;

	/* Message */
#ifdef JP
	msg_print("ランプに油を注いだ。");
#else
	msg_print("You fuel your lamp.");
#endif

	/* Comment */
	if ((o_ptr->name2 == EGO_LITE_DARKNESS) && (j_ptr->xtra4 > 0))
	{
		j_ptr->xtra4 = 0;
#ifdef JP
		msg_print("ランプが消えてしまった！");
#else
		msg_print("Your lamp has gone out!");
#endif
	}
	else if ((o_ptr->name2 == EGO_LITE_DARKNESS) || (j_ptr->name2 == EGO_LITE_DARKNESS))
	{
		j_ptr->xtra4 = 0;
#ifdef JP
		msg_print("しかしランプは全く光らない。");
#else
		msg_print("Curiously, your lamp doesn't light.");
#endif
	}
	else if (j_ptr->xtra4 >= FUEL_LAMP)
	{
		j_ptr->xtra4 = FUEL_LAMP;
#ifdef JP
		msg_print("ランプの油は一杯だ。");
#else
		msg_print("Your lamp is full.");
#endif

	}

	/* Decrease the item (from the pack) */
	if (item >= 0)
	{
		inven_item_increase(item, -1);
		inven_item_describe(item);
		inven_item_optimize(item);
	}

	/* Decrease the item (from the floor) */
	else
	{
		floor_item_increase(0 - item, -1);
		floor_item_describe(0 - item);
		floor_item_optimize(0 - item);
	}

	/* Recalculate torch */
	p_ptr->update |= (PU_TORCH);
}


/*
 * An "item_tester_hook" for refilling torches
 */
static bool item_tester_refill_torch(object_type *o_ptr)
{
	/* Torches are okay */
	if ((o_ptr->tval == TV_LITE) &&
	    (o_ptr->sval == SV_LITE_TORCH)) return (TRUE);

	/* Assume not okay */
	return (FALSE);
}


/*
 * Refuel the players torch (from the pack or floor)
 */
static void do_cmd_refill_torch(void)
{
	int item;

	object_type *o_ptr;
	object_type *j_ptr;

	cptr q, s;


	/* Restrict the choices */
	item_tester_hook = item_tester_refill_torch;

	/* Get an item */
#ifdef JP
	q = "どの松明で明かりを強めますか? ";
	s = "他に松明がない。";
#else
	q = "Refuel with which torch? ";
	s = "You have no extra torches.";
#endif

	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

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


	/* Take a partial turn */
	energy_use = 50;

	/* Access the primary torch */
	j_ptr = &inventory[INVEN_LITE];

	/* Refuel */
	j_ptr->xtra4 += o_ptr->xtra4 + 5;

	/* Message */
#ifdef JP
	msg_print("松明を結合した。");
#else
	msg_print("You combine the torches.");
#endif


	/* Comment */
	if ((o_ptr->name2 == EGO_LITE_DARKNESS) && (j_ptr->xtra4 > 0))
	{
		j_ptr->xtra4 = 0;
#ifdef JP
		msg_print("松明が消えてしまった！");
#else
		msg_print("Your torch has gone out!");
#endif
	}
	else if ((o_ptr->name2 == EGO_LITE_DARKNESS) || (j_ptr->name2 == EGO_LITE_DARKNESS))
	{
		j_ptr->xtra4 = 0;
#ifdef JP
		msg_print("しかし松明は全く光らない。");
#else
		msg_print("Curiously, your torche don't light.");
#endif
	}
	/* Over-fuel message */
	else if (j_ptr->xtra4 >= FUEL_TORCH)
	{
		j_ptr->xtra4 = FUEL_TORCH;
#ifdef JP
		msg_print("松明の寿命は十分だ。");
#else
		msg_print("Your torch is fully fueled.");
#endif

	}

	/* Refuel message */
	else
	{
#ifdef JP
		msg_print("松明はいっそう明るく輝いた。");
#else
		msg_print("Your torch glows more brightly.");
#endif

	}

	/* Decrease the item (from the pack) */
	if (item >= 0)
	{
		inven_item_increase(item, -1);
		inven_item_describe(item);
		inven_item_optimize(item);
	}

	/* Decrease the item (from the floor) */
	else
	{
		floor_item_increase(0 - item, -1);
		floor_item_describe(0 - item);
		floor_item_optimize(0 - item);
	}

	/* Recalculate torch */
	p_ptr->update |= (PU_TORCH);
}


/*
 * Refill the players lamp, or restock his torches
 */
void do_cmd_refill(void)
{
	object_type *o_ptr;

	/* Get the light */
	o_ptr = &inventory[INVEN_LITE];

	/* It is nothing */
	if (o_ptr->tval != TV_LITE)
	{
#ifdef JP
		msg_print("光源を装備していない。");
#else
		msg_print("You are not wielding a light.");
#endif

	}

	/* It's a lamp */
	else if (o_ptr->sval == SV_LITE_LANTERN)
	{
		do_cmd_refill_lamp();
	}

	/* It's a torch */
	else if (o_ptr->sval == SV_LITE_TORCH)
	{
		do_cmd_refill_torch();
	}

	/* No torch to refill */
	else
	{
#ifdef JP
		msg_print("この光源は寿命を延ばせない。");
#else
		msg_print("Your light cannot be refilled.");
#endif

	}
}


/*
 * Target command
 */
void do_cmd_target(void)
{
	/* Target set */
	if (target_set(TARGET_KILL))
	{
#ifdef JP
		msg_print("ターゲット決定。");
#else
		msg_print("Target Selected.");
#endif

	}

	/* Target aborted */
	else
	{
#ifdef JP
		msg_print("ターゲット解除。");
#else
		msg_print("Target Aborted.");
#endif

	}
}



/*
 * Look command
 */
void do_cmd_look(void)
{
	/* Look around */
	if (target_set(TARGET_LOOK))
	{
#ifdef JP
		msg_print("ターゲット決定。");
#else
		msg_print("Target Selected.");
#endif

	}
}



/*
 * Allow the player to examine other sectors on the map
 */
void do_cmd_locate(void)
{
	int		dir, y1, x1, y2, x2;

	char	tmp_val[80];

	char	out_val[160];

	int wid, hgt;

	/* Get size */
	get_screen_size(&wid, &hgt);


	/* Start at current panel */
	y2 = y1 = panel_row_min;
	x2 = x1 = panel_col_min;

	/* Show panels until done */
	while (1)
	{
		/* Describe the location */
		if ((y2 == y1) && (x2 == x1))
		{
#ifdef JP
			strcpy(tmp_val, "真上");
#else
			tmp_val[0] = '\0';
#endif

		}
		else
		{
#ifdef JP
			sprintf(tmp_val, "%s%s",
			        ((y2 < y1) ? "北" : (y2 > y1) ? "南" : ""),
			        ((x2 < x1) ? "西" : (x2 > x1) ? "東" : ""));
#else
			sprintf(tmp_val, "%s%s of",
			        ((y2 < y1) ? " North" : (y2 > y1) ? " South" : ""),
			        ((x2 < x1) ? " West" : (x2 > x1) ? " East" : ""));
#endif

		}

		/* Prepare to ask which way to look */
		sprintf(out_val,
#ifdef JP
		        "マップ位置 [%d(%02d),%d(%02d)] (プレイヤーの%s)  方向?",
#else
		        "Map sector [%d(%02d),%d(%02d)], which is%s your sector.  Direction?",
#endif

		        y2 / (hgt / 2), y2 % (hgt / 2),
		        x2 / (wid / 2), x2 % (wid / 2), tmp_val);

		/* Assume no direction */
		dir = 0;

		/* Get a direction */
		while (!dir)
		{
			char command;

			/* Get a command (or Cancel) */
			if (!get_com(out_val, &command, TRUE)) break;

			/* Extract the action (if any) */
			dir = get_keymap_dir(command);

			/* Error */
			if (!dir) bell();
		}

		/* No direction */
		if (!dir) break;

		/* Apply the motion */
		if (change_panel(ddy[dir], ddx[dir]))
		{
			y2 = panel_row_min;
			x2 = panel_col_min;
		}
	}


	/* Recenter the map around the player */
	verify_panel();

	/* Update stuff */
	p_ptr->update |= (PU_MONSTERS);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);

	/* Handle stuff */
	handle_stuff();
}



/*
 * Sorting hook -- Comp function -- see below
 *
 * We use "u" to point to array of monster indexes,
 * and "v" to select the type of sorting to perform on "u".
 */
bool ang_sort_comp_hook(vptr u, vptr v, int a, int b)
{
	u16b *who = (u16b*)(u);

	u16b *why = (u16b*)(v);

	int w1 = who[a];
	int w2 = who[b];

	int z1, z2;


	/* Sort by player kills */
	if (*why >= 4)
	{
		/* Extract player kills */
		z1 = r_info[w1].r_pkills;
		z2 = r_info[w2].r_pkills;

		/* Compare player kills */
		if (z1 < z2) return (TRUE);
		if (z1 > z2) return (FALSE);
	}


	/* Sort by total kills */
	if (*why >= 3)
	{
		/* Extract total kills */
		z1 = r_info[w1].r_tkills;
		z2 = r_info[w2].r_tkills;

		/* Compare total kills */
		if (z1 < z2) return (TRUE);
		if (z1 > z2) return (FALSE);
	}


	/* Sort by monster level */
	if (*why >= 2)
	{
		/* Extract levels */
		z1 = r_info[w1].level;
		z2 = r_info[w2].level;

		/* Compare levels */
		if (z1 < z2) return (TRUE);
		if (z1 > z2) return (FALSE);
	}


	/* Sort by monster experience */
	if (*why >= 1)
	{
		/* Extract experience */
		z1 = r_info[w1].mexp;
		z2 = r_info[w2].mexp;

		/* Compare experience */
		if (z1 < z2) return (TRUE);
		if (z1 > z2) return (FALSE);
	}


	/* Compare indexes */
	return (w1 <= w2);
}


/*
 * Sorting hook -- Swap function -- see below
 *
 * We use "u" to point to array of monster indexes,
 * and "v" to select the type of sorting to perform.
 */
void ang_sort_swap_hook(vptr u, vptr v, int a, int b)
{
	u16b *who = (u16b*)(u);

	u16b holder;

	/* Unused */
	(void)v;

	/* Swap */
	holder = who[a];
	who[a] = who[b];
	who[b] = holder;
}



/*
 * Identify a character, allow recall of monsters
 *
 * Several "special" responses recall "multiple" monsters:
 *   ^A (all monsters)
 *   ^U (all unique monsters)
 *   ^N (all non-unique monsters)
 *
 * The responses may be sorted in several ways, see below.
 *
 * Note that the player ghosts are ignored. XXX XXX XXX
 */
void do_cmd_query_symbol(void)
{
	int		i, n, r_idx;
	char	sym, query;
	char	buf[128];

	bool	all = FALSE;
	bool	uniq = FALSE;
	bool	norm = FALSE;
	char    temp[80] = "";

	bool	recall = FALSE;

	u16b	why = 0;
	u16b	*who;

	/* Get a character, or abort */
#ifdef JP
	if (!get_com("知りたい文字を入力して下さい(記号 or ^A全,^Uユ,^N非ユ,^M名前): ", &sym, FALSE)) return;
#else
	if (!get_com("Enter character to be identified(^A:All,^U:Uniqs,^N:Non uniqs,^M:Name): ", &sym, FALSE)) return;
#endif


	/* Find that character info, and describe it */
	for (i = 0; ident_info[i]; ++i)
	{
		if (sym == ident_info[i][0]) break;
	}

	/* Describe */
	if (sym == KTRL('A'))
	{
		all = TRUE;
#ifdef JP
		strcpy(buf, "全モンスターのリスト");
#else
		strcpy(buf, "Full monster list.");
#endif

	}
	else if (sym == KTRL('U'))
	{
		all = uniq = TRUE;
#ifdef JP
		strcpy(buf, "ユニーク・モンスターのリスト");
#else
		strcpy(buf, "Unique monster list.");
#endif

	}
	else if (sym == KTRL('N'))
	{
		all = norm = TRUE;
#ifdef JP
		strcpy(buf, "ユニーク外モンスターのリスト");
#else
		strcpy(buf, "Non-unique monster list.");
#endif

	}
	/* XTRA HACK WHATSEARCH */
	else if (sym == KTRL('M'))
	{
		all = TRUE;
#ifdef JP
		if (!get_string("名前(英語の場合小文字で可)",temp, 70))
#else
		if (!get_string("Enter name:",temp, 70))
#endif
		{
			temp[0]=0;
			return;
		}
#ifdef JP
		sprintf(buf, "名前:%sにマッチ",temp);
#else
		sprintf(buf, "Monsters with a name \"%s\"",temp);
#endif
	}
	else if (ident_info[i])
	{
		sprintf(buf, "%c - %s.", sym, ident_info[i] + 2);
	}
	else
	{
#ifdef JP
		sprintf(buf, "%c - %s", sym, "無効な文字");
#else
		sprintf(buf, "%c - %s.", sym, "Unknown Symbol");
#endif

	}

	/* Display the result */
	prt(buf, 0, 0);

	/* Allocate the "who" array */
	C_MAKE(who, max_r_idx + runeweapon_num, u16b);

	/* Collect matching monsters */
	for (n = 0, i = 1; i < (max_r_idx + runeweapon_num); i++)
	{
		monster_race *r_ptr = &r_info[i];

		/* Empty monster */
		if (!r_ptr->name) continue;

		/* Nothing to recall */
		if (!cheat_know && !r_ptr->r_sights) continue;

		/* Require non-unique monsters if needed */
		if (norm && (r_ptr->flags1 & (RF1_UNIQUE))) continue;

		/* Require unique monsters if needed */
		if (uniq && !(r_ptr->flags1 & (RF1_UNIQUE))) continue;

		/* XTRA HACK WHATSEARCH */
		if (temp[0])
		{
			int xx;
			char temp2[80];

			for (xx = 0; temp[xx] && xx < 80; xx++)
			{
#ifdef JP
				if (iskanji(temp[xx])) { xx++; continue; }
#endif
				if (isupper(temp[xx])) temp[xx] = tolower(temp[xx]);
			}

#ifdef JP
			strcpy(temp2, r_name+r_ptr->E_name);
#else
			strcpy(temp2, r_name+r_ptr->name);
#endif
			for (xx = 0; temp2[xx] && xx < 80; xx++)
				if (isupper(temp2[xx])) temp2[xx] = tolower(temp2[xx]);

#ifdef JP
			if (my_strstr(temp2, temp) || my_strstr(r_name + r_ptr->name, temp))
#else
			if (my_strstr(temp2, temp))
#endif
				who[n++] = i;
		}
		else
		/* Collect "appropriate" monsters */
		if (all || (r_ptr->d_char == sym)) who[n++] = i;
	}

	/* Nothing to recall */
	if (!n)
	{
		/* Free the "who" array */
		C_KILL(who, max_r_idx + runeweapon_num, u16b);

		return;
	}


	/* Prompt XXX XXX XXX */
#ifdef JP
	put_str("思い出を見ますか? (k:殺害順/y/n): ", 0, 36);
#else
	put_str("Recall details? (k/y/n): ", 0, 40);
#endif


	/* Query */
	query = inkey();

	/* Restore */
	prt(buf, 0, 0);

	why = 2;

	/* Select the sort method */
	ang_sort_comp = ang_sort_comp_hook;
	ang_sort_swap = ang_sort_swap_hook;

	/* Sort the array */
	ang_sort(who, &why, n);

	/* Sort by kills (and level) */
	if (query == 'k')
	{
		why = 4;
		query = 'y';
	}

	/* Catch "escape" */
	if (query != 'y')
	{
		/* Free the "who" array */
		C_KILL(who, max_r_idx + runeweapon_num, u16b);

		return;
	}

	/* Sort if needed */
	if (why == 4)
	{
		/* Select the sort method */
		ang_sort_comp = ang_sort_comp_hook;
		ang_sort_swap = ang_sort_swap_hook;

		/* Sort the array */
		ang_sort(who, &why, n);
	}


	/* Start at the end */
	i = n - 1;

	/* Scan the monster memory */
	while (1)
	{
		/* Extract a race */
		r_idx = who[i];

		/* Hack -- Auto-recall */
		monster_race_track(r_idx);

		/* Hack -- Handle stuff */
		handle_stuff();

		/* Hack -- Begin the prompt */
		roff_top(r_idx);

		/* Hack -- Complete the prompt */
#ifdef JP
		Term_addstr(-1, TERM_WHITE, " ['r'思い出, ESC]");
#else
		Term_addstr(-1, TERM_WHITE, " [(r)ecall, ESC]");
#endif


		/* Interact */
		while (1)
		{
			/* Recall */
			if (recall)
			{
				/* Save the screen */
				screen_save();

				/* Recall on screen */
				screen_roff(who[i], 0);

				/* Hack -- Complete the prompt (again) */
#ifdef JP
				Term_addstr(-1, TERM_WHITE, " ['r'思い出, ESC]");
#else
				Term_addstr(-1, TERM_WHITE, " [(r)ecall, ESC]");
#endif

			}

			/* Command */
			query = inkey();

			/* Unrecall */
			if (recall)
			{
				/* Restore */
				screen_load();
			}

			/* Normal commands */
			if (query != 'r') break;

			/* Toggle recall */
			recall = !recall;

			/* Hack -- Begin the prompt */
			roff_top(r_idx);

			/* Hack -- Complete the prompt */
#ifdef JP
			Term_addstr(-1, TERM_WHITE, " ['r'思い出, ESC]");
#else
			Term_addstr(-1, TERM_WHITE, " [(r)ecall, ESC]");
#endif
		}

		/* Stop scanning */
		if (query == ESCAPE) break;

		/* Move to "prev" monster */
		if (query == '-')
		{
			if (++i == n)
			{
				i = 0;
				if (!expand_list) break;
			}
		}

		/* Move to "next" monster */
		else
		{
			if (i-- == 0)
			{
				i = n - 1;
				if (!expand_list) break;
			}
		}
	}

	/* Free the "who" array */
	C_KILL(who, max_r_idx + runeweapon_num, u16b);

	/* Re-display the identity */
	prt(buf, 0, 0);
}


