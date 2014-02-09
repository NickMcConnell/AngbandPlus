/*
 * File: cmd-obj.c
 * Purpose: Handle objects in various ways
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * Copyright (c) 2007-9 Andrew Sidwell, Chris Carr, Ed Graham, Erik Osheim
 *                       Jeff Greene, Diego Gonzalez
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 2, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */
#include "angband.h"
#include "game-cmd.h"
#include "cmds.h"


/*** Utility bits and bobs ***/

/*
 * Check to see if the player can use a rod/wand/staff/activatable object.
 */
static int check_devices(object_type *o_ptr)
{
	int fail;
	const char *msg;
	const char *what = NULL;

	/* Get the right string */
	switch (o_ptr->tval)
	{
		case TV_ROD:   msg = "zap the rod";   break;
		case TV_WAND:  msg = "use the wand";  what = "wand";  break;
		case TV_STAFF: msg = "use the staff"; what = "staff"; break;
		default:       msg = "activate it";  break;
	}

	/* Figure out how hard the item is to use */
	fail = get_use_device_chance(o_ptr);

	/* Roll for usage */
	if (randint1(1000) < fail)
	{
		if (flush_failure) flush();
		msg_format("You failed to %s properly.", msg);
		return FALSE;
	}

	/* Notice empty staffs */
	if (what && o_ptr->pval <= 0)
	{
		if (flush_failure) flush();
		msg_format("The %s has no charges left.", what);
		o_ptr->ident |= (IDENT_EMPTY);
		p_ptr->notice |= (PN_COMBINE | PN_REORDER);
		p_ptr->window |= (PW_INVEN);

		return FALSE;
	}

	return TRUE;
}


/*** Inscriptions ***/


/*
 * Remove inscription
 */
void do_cmd_uninscribe(cmd_code code, cmd_arg args[])
{
	object_type *o_ptr = object_from_item_idx(args[0].item);

	if (!obj_has_inscrip(o_ptr))
	{
		msg_print("That item had no inscription to remove.");
		return;
	}

	/* Remove the inscription */
	o_ptr->obj_note = 0;

	/*The object kind has an autoinscription*/
	if (ACCESS_AUTOINSCRIPTIONS(o_ptr) &&
	    (get_autoinscription_index(o_ptr->k_idx) != -1))
	{
		char tmp_val[160];
		char o_name2[80];

		/*make a fake object so we can give a proper message*/
		object_type *i_ptr;
		object_type object_type_body;

		/* Get local object */
		i_ptr = &object_type_body;

		/* Wipe the object */
		object_wipe(i_ptr);

		/* Create the object */
		object_prep(i_ptr, o_ptr->k_idx);

		/*now describe with correct amount*/
		object_desc(o_name2, sizeof(o_name2), i_ptr, ODESC_PLURAL | ODESC_FULL);

		/* Prompt */
		strnfmt(tmp_val, sizeof(tmp_val),
			"Remove automatic inscription for %s?", o_name2);

		/* Auto-Inscribe if they want that */
		if (get_check(tmp_val)) remove_autoinscription(o_ptr->k_idx);
	}

	msg_print("Inscription removed.");

	o_ptr->obj_note = 0;

	p_ptr->notice |= (PN_COMBINE | PN_REORDER | PN_SORT_QUIVER);
	p_ptr->redraw |= (PR_INVEN | PR_EQUIP | PR_ITEMLIST);
}


/*
 * Add inscription
 */
void do_cmd_inscribe(cmd_code code, cmd_arg args[])
{
	object_type *o_ptr = object_from_item_idx(args[0].item);

	o_ptr->obj_note = quark_add(args[1].string);

	p_ptr->notice |= (PN_COMBINE | PN_REORDER | PN_SORT_QUIVER);
	p_ptr->redraw |= (PR_INVEN | PR_EQUIP | PR_ITEMLIST);
}


/*
 * Inscribe a given item
 */
static void obj_inscribe_aux(object_type *o_ptr)
{
	char o_name[80];
	char tmp[80] = "";

	/* Describe the activity */
	object_desc(o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_FULL);

	/* Message */
	msg_format("Inscribing %s.", o_name);

	/* Start with nothing */
	my_strcpy(tmp, "", sizeof(tmp));

	/* Use old inscription */
	if (o_ptr->obj_note)
	{
		/* Start with the old inscription */
		strnfmt(tmp, sizeof(tmp), "%s", quark_str(o_ptr->obj_note));
	}

	/* Get a new inscription (possibly empty) */
	if (get_string("Inscription: ", tmp, sizeof(tmp)))
	{
		char tmp_val[160];
		char o_name2[80];

		/*make a fake object so we can give a proper message*/
		object_type *i_ptr;
		object_type object_type_body;

		/* Add an autoinscription? */
		if (ACCESS_AUTOINSCRIPTIONS(o_ptr))
		{
			/* Get local object */
			i_ptr = &object_type_body;

			/* Wipe the object */
			object_wipe(i_ptr);

			/* Create the object */
			object_prep(i_ptr, o_ptr->k_idx);

			/*now describe with correct amount*/
			object_desc(o_name2, sizeof(o_name2), i_ptr, ODESC_FULL | ODESC_PLURAL);

			/* Prompt */
			strnfmt(tmp_val, sizeof(tmp_val),
				"Automatically inscribe all %s with %s?",
				o_name2, tmp);

			/* Auto-Inscribe if they want that */
			if (get_check(tmp_val)) add_autoinscription(o_ptr->k_idx, tmp);
		}

		/* Expand certain patterns in inscriptions */
		expand_inscription(o_ptr, tmp, tmp_val, sizeof(tmp_val));

		/* Save the inscription */
		o_ptr->obj_note = quark_add(tmp_val);

		/* Combine / Reorder the pack */
		p_ptr->notice |= (PN_COMBINE | PN_REORDER | PN_SORT_QUIVER);

		/* Redraw stuff */
		p_ptr->redraw |= (PR_INVEN | PR_EQUIP | PR_ITEMLIST);
	}

}


/*
 * Inscribe an item
 */
void obj_inscribe(object_type *o_ptr, int item)
{
	/* FIXME The passed value of o_ptr is always overwritten before use */

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

	/* inscribe the item */
	obj_inscribe_aux(o_ptr);
}


/*** Examination ***/
void obj_examine(object_type *o_ptr, int item)
{
	track_object(item);

	(void)item;

	object_info_screen(o_ptr);
}


/*** Taking off/putting on ***/


/*
 * Take off an item
 */
void do_cmd_takeoff(cmd_code code, cmd_arg args[])
{
	int item = args[0].item;

	if (!item_is_available(item, NULL, USE_EQUIP | USE_QUIVER))
	{
		msg_print("You are not wielding that item.");
		return;
	}

	if (!obj_can_takeoff(object_from_item_idx(item)))
	{
		msg_print("You cannot take off that item.");
		return;
	}

	(void)inven_takeoff(item, 255);
	pack_overflow();
	p_ptr->p_energy_use = BASE_ENERGY_MOVE / 2;
}


/*
 * Wield or wear an item
 */
void do_cmd_wield(cmd_code code, cmd_arg args[])
{
	object_type *equip_o_ptr;
	char o_name[80];

	unsigned n;

	int item = args[0].item;
	int slot = args[1].number;
	object_type *o_ptr = object_from_item_idx(item);

	if (!item_is_available(item, NULL, USE_INVEN | USE_FLOOR))
	{
		msg_print("You do not have that item to wield.");
		return;
	}

	/* Check the slot */
	if (!slot_can_wield_item(slot, o_ptr))
	{
		object_desc(o_name, sizeof(o_name), o_ptr,  ODESC_PREFIX | ODESC_FULL);

		msg_print("You cannot wield that item there.");
		return;
	}

	/*Hack - don't allow quest items to be worn*/
	if(o_ptr->ident & (IDENT_QUEST))
	{
		msg_print("You cannot wield quest items.");
		return;
	}

	/* Hack - Throwing weapons can be wielded in the quiver too. */
	/* Analyze object's inscription and verify the presence of "@v" */
	if (is_throwing_weapon(o_ptr) && !IS_QUIVER_SLOT(slot) && o_ptr->obj_note)
	{
		/* Get the note */
		cptr note = quark_str(o_ptr->obj_note);

		/* Find the next '@' */
		while ((note = strchr(note, '@')) != NULL)
		{
			/* Found "@v" ? */
			if (note[1] == 'v')
			{
				/* Force quiver */
				slot = QUIVER_START;

				break;
			}

			/* Keep searching */
			++note;
		}
	}

	equip_o_ptr = &inventory[slot];

	/* If the slot is open, wield and be done */
	if (!equip_o_ptr->k_idx)
	{
		wield_item(o_ptr, item, slot);
		return;
	}

	/* If the slot is in the quiver and objects can be combined */
	if (((obj_is_ammo(equip_o_ptr)) || (is_throwing_weapon(o_ptr)))
		                  && object_similar(equip_o_ptr, o_ptr))
	{
		wield_item(o_ptr, item, slot);
		return;
	}

	/* Prevent wielding into a cursed slot */
	if (cursed_p(equip_o_ptr))
	{
		object_desc(o_name, sizeof(o_name), equip_o_ptr,  ODESC_BASE);
		msg_format("The %s you are %s appears to be cursed.", o_name,
				   describe_use(slot));
		return;
	}

		/* "!t" checks for taking off */
		n = check_for_inscrip(equip_o_ptr, "!t");
		while (n--)
		{
			/* Prompt */
			object_desc(o_name, sizeof(o_name), equip_o_ptr,  ODESC_PREFIX | ODESC_FULL);

		/* Forget it */
		if (!get_check(format("Really take off %s? ", o_name))) return;
	}

	wield_item(o_ptr, item, slot);

}


/*
 * Drop an item
 */
void do_cmd_drop(cmd_code code, cmd_arg args[])
{
	int item = args[0].item;
	object_type *o_ptr = object_from_item_idx(item);
	int amt = args[1].number;

	if (!item_is_available(item, NULL, USE_INVEN | USE_EQUIP | USE_QUIVER))
	{
		msg_print("You do not have that item to drop it.");
		return;
	}

	/* Hack -- Cannot remove cursed items */
	if ((item >= INVEN_WIELD) && cursed_p(o_ptr))
	{
		msg_print("Hmmm, it seems to be cursed.");
		return;
	}

	/* Cursed quiver */
	else if (IS_QUIVER_SLOT(item) && p_ptr->state.cursed_quiver)
	{
		/* Oops */
		msg_print("Your quiver is cursed!");

		/* Nope */
		return;
	}

	inven_drop(item, amt);
	p_ptr->p_energy_use = BASE_ENERGY_MOVE / 2;
}


static void obj_drop(object_type *o_ptr, int item)
{
	int amt;

	amt = get_quantity(NULL, o_ptr->number);
	if (amt <= 0) return;

	cmd_insert(CMD_DROP, item, amt);
}

static void obj_wield(object_type *o_ptr, int item)
{
	int slot = wield_slot(o_ptr);

	/* Usually if the slot is taken we'll just replace the item in the slot,
	 * but in some cases we need to ask the user which slot they actually
	 * want to replace */
	if (inventory[slot].k_idx)
	{
		if (o_ptr->tval == TV_RING)
		{
			cptr q = "Replace which ring? ";
			cptr s = "Error in obj_wield, please report";
			item_tester_hook = obj_is_ring;
			if (!get_item(&slot, q, s, USE_EQUIP)) return;
		}

		if (obj_is_ammo(o_ptr) && !object_similar(&inventory[slot], o_ptr))
		{
			cptr q = "Replace which ammunition? ";
			cptr s = "Error in obj_wield, please report";
			item_tester_hook = obj_is_ammo;
			if (!get_item(&slot, q, s, USE_QUIVER)) return;
		}
	}

	/* Hack - Throwing weapons can be wielded in the quiver too. */
	/* Analyze object's inscription and verify the presence of "@v" */
	if (is_throwing_weapon(o_ptr) && !IS_QUIVER_SLOT(slot) && o_ptr->obj_note)
	{
		/* Get the note */
		cptr note = quark_str(o_ptr->obj_note);

		/* Find the next '@' */
		while ((note = strchr(note, '@')) != NULL)
		{
			/* Found "@v" ? */
			if (note[1] == 'v')
			{
				/* Force quiver */
				slot = QUIVER_START;

				break;
			}

			/* Keep searching */
			++note;
		}
	}

	cmd_insert(CMD_WIELD, item, slot);
}

static void swap_weapons(void)
{
	object_type *o_ptr = &inventory[INVEN_MAIN_WEAPON];
	object_type *j_ptr = &inventory[INVEN_SWAP_WEAPON];
	object_type object_type_body;
	object_type *i_ptr = & object_type_body;
	char o_name[80];
	cptr act;

	/* Not holding anything */
	if ((!o_ptr->k_idx) && (!j_ptr->k_idx))
	{
		msg_print("But you are wielding no weapons.");
		return;
	}

	/* Can't swap because of a cursed weapon */
	if (cursed_p(o_ptr))
	{
		object_desc(o_name, sizeof(o_name), o_ptr,  ODESC_BASE);
		msg_format("The %s you are %s appears to be cursed.", o_name,  describe_use(INVEN_MAIN_WEAPON));
		return;
	}

	p_ptr->p_energy_use = BASE_ENERGY_MOVE;

	/* Give the player a message for teh item they are taking off */
	if (o_ptr->k_idx)
	{
		/* The player took off a bow */
		if (obj_is_bow(o_ptr))
		{
			act = "You were shooting";
		}

		/* Took off weapon */
		else act = "You were wielding";

		/* Describe the object */
		object_desc(o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_FULL);
		msg_format("%s %s (%c).", act, o_name, index_to_label(INVEN_SWAP_WEAPON));
	}

	/* Make a record of the primary weapon, and wipe it */
	object_copy(i_ptr, o_ptr);
	object_wipe(o_ptr);

	/* Insert the swap weapon if there is one */
	if (j_ptr->k_idx)
	{
		wield_item(j_ptr, INVEN_SWAP_WEAPON, INVEN_MAIN_WEAPON);
	}

	/* if a previous weapon, place it in the secondary weapon slot */
	if (i_ptr->k_idx)
	{
		object_copy(j_ptr, i_ptr);
	}

	/* Recalculate bonuses, torch, mana */
	p_ptr->update |= (PU_BONUS | PU_TORCH | PU_MANA | PU_NATIVE);
	p_ptr->redraw |= (PR_INVEN | PR_EQUIP | PR_ITEMLIST);
}

/* Search the backpack for a weapon with @x and wield it*/
static void wield_swap_weapon(void)
{
	int i;

	object_type *o_ptr = &inventory[INVEN_MAIN_WEAPON];

	/* Can't swap because of a cursed weapon */
	if (cursed_p(o_ptr))
	{
		char o_name[80];

		object_desc(o_name, sizeof(o_name), o_ptr,  ODESC_BASE);
		msg_format("The %s you are %s appears to be cursed.", o_name,  describe_use(INVEN_MAIN_WEAPON));
		return;
	}

	/* Check every object */
	for (i = 0; i < INVEN_MAX_PACK; ++i)
	{
		o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Skip empty inscriptions */
		if (!o_ptr->obj_note) continue;

		if(!obj_is_weapon(o_ptr)) continue;

		/* Look for '@x' */
		if(!strstr(quark_str(o_ptr->obj_note), "@x")) continue;

		/* Wield it */
		wield_item(o_ptr, i, INVEN_MAIN_WEAPON);

		p_ptr->p_energy_use = BASE_ENERGY_MOVE;

		/* Recalculate bonuses, torch, mana */
		p_ptr->update |= (PU_BONUS | PU_TORCH | PU_MANA | PU_NATIVE);
		p_ptr->redraw |= (PR_INVEN | PR_EQUIP | PR_ITEMLIST);

		/* We are done */
		return;
	}

	/* Didn't find anything */
	msg_print("Please inscribe a weapon with '@x' in order to swap it.");
}

/*
 * Depending on game options, either swap weapons between the main weapon
 * slot and the swap weapon slot, or search for weapon with the @x inscription and wield it
 */
void do_cmd_swap_weapon(cmd_code code, cmd_arg args[])
{
	(void)code;
	(void)args;

	if (adult_swap_weapons) swap_weapons();
	else wield_swap_weapon();
}

void textui_cmd_swap_weapon(void)
{
	cmd_insert(CMD_SWAP);
}


/*
 * Peruse spells in a book
 */
void obj_browse(object_type *o_ptr, int item)
{
	(void)item;

	(void)get_spell_menu(o_ptr, BOOK_BROWSE);
}


/*
 * Study a book to gain a new spell
 */
void obj_study(object_type *o_ptr, int item)
{
	/* Track the object kind */
	track_object(item);

	/* Mage -- Choose a spell to study */
	if (cp_ptr->flags & CF_CHOOSE_SPELLS)
	{
		int spell = get_spell_menu(o_ptr, BOOK_STUDY);
		if (spell >= 0)
			cmd_insert(CMD_STUDY_SPELL, spell);
		else if (spell == -2)
			msg_print("You cannot learn any spells from that book.");
	}

	/* Priest -- Choose a book to study */
	else
	{
		cmd_insert(CMD_STUDY_BOOK, item);
	}
}


/*
 * Helper function to help spells that target traps (disarming, etc...)
 */
static bool is_trap_spell(byte spell_book, int spell)
{
	if (spell_book == TV_MAGIC_BOOK)
	{
		if (spell == SPELL_TRAP_DOOR_DESTRUCTION) return (TRUE);
	}
	else if (spell_book == TV_PRAYER_BOOK)
	{
		if (spell == PRAYER_UNBARRING_WAYS) return (TRUE);
	}
	else if (spell_book == TV_DRUID_BOOK)
	{
		if (spell == DRUID_TRAP_DOOR_DESTRUCTION) return (TRUE);

	}
	return (FALSE);
}

void obj_cast(object_type *o_ptr, int item)
{
	int spell, dir = DIR_UNKNOWN;
	cptr noun = cast_spell(MODE_SPELL_NOUN, cp_ptr->spell_book, 1, 0);
	bool trap_spell;

	/* Track the object kind */
	track_object(item);

	/* Ask for a spell */
	spell = get_spell_menu(o_ptr, BOOK_CAST);

	if (spell < 0)
	{
		if (spell == -2) msg_format("You don't know any %ss in that book.", noun);
		return;
	}

	trap_spell = is_trap_spell(cp_ptr->spell_book, spell);

	if (spell_needs_aim(cp_ptr->spell_book, spell) && !get_aim_dir(&dir, trap_spell))
		return;

	cmd_insert(CMD_CAST, spell, dir);
}


/*
 * Determine if the player can read scrolls.
 */
static bool player_can_read(void)
{
	if (p_ptr->timed[TMD_BLIND])
	{
		msg_print("You can't see anything.");
		return FALSE;
	}

	if (!player_can_see_bold(p_ptr->py, p_ptr->px))
	{
		msg_print("You have no light to read by.");
		return FALSE;
	}

	if (p_ptr->timed[TMD_CONFUSED])
	{
		msg_print("You are too confused to read!");
		return FALSE;
	}


	return TRUE;
}

/*
 * Eat something
 */
static bool eat_food(object_type *o_ptr, bool *ident)
{
	/* Analyze the food */
	switch (o_ptr->sval)
	{
		case SV_FOOD_POISON:
		{
			if (!(p_ptr->state.resist_pois || p_ptr->timed[TMD_OPP_POIS] || p_ptr->state.immune_pois))
			{
				if (inc_timed(TMD_POISONED, rand_int(10) + 10, TRUE))
				{
					*ident = TRUE;
				}
			}
			break;
		}

		case SV_FOOD_BLINDNESS:
		{
			if (!p_ptr->state.resist_blind)
			{
				if (inc_timed(TMD_BLIND, rand_int(200) + 200, TRUE))
				{
					*ident = TRUE;
				}
			}
			break;
		}

		case SV_FOOD_PARANOIA:
		{
			if (!p_ptr->state.resist_fear)
			{
				if (inc_timed(TMD_AFRAID, rand_int(10) + 10, TRUE))
				{
					*ident = TRUE;
				}
			}
			break;
		}

		case SV_FOOD_CONFUSION:
		{
			if (allow_player_confusion())
			{
				if (inc_timed(TMD_CONFUSED, rand_int(10) + 10, TRUE))
				{
					*ident = TRUE;
				}
			}
			break;
		}

		case SV_FOOD_HALLUCINATION:
		{
			if (!p_ptr->state.resist_chaos)
			{
				if (inc_timed(TMD_IMAGE, rand_int(250) + 250, TRUE))
				{
					*ident = TRUE;
				}
			}
			break;
		}

		case SV_FOOD_PARALYSIS:
		{
			if (!p_ptr->state.free_act)
			{
				if (inc_timed(TMD_PARALYZED, rand_int(10) + 10, TRUE))
				{
					*ident = TRUE;
				}
			}
			break;
		}

		case SV_FOOD_WEAKNESS:
		{
			take_hit(damroll(6, 6), "poisonous food");
			(void)do_dec_stat(A_STR);
			*ident = TRUE;
			break;
		}

		case SV_FOOD_SICKNESS:
		{
			take_hit(damroll(6, 6), "poisonous food");
			(void)do_dec_stat(A_CON);
			*ident = TRUE;
			break;
		}

		case SV_FOOD_STUPIDITY:
		{
			take_hit(damroll(8, 8), "poisonous food");
			(void)do_dec_stat(A_INT);
			*ident = TRUE;
			break;
		}

		case SV_FOOD_NAIVETY:
		{
			take_hit(damroll(8, 8), "poisonous food");
			(void)do_dec_stat(A_WIS);
			*ident = TRUE;
			break;
		}

		case SV_FOOD_UNHEALTH:
		{
			take_hit(damroll(10, 10), "poisonous food");
			(void)do_dec_stat(A_CON);
			*ident = TRUE;
			break;
		}

		case SV_FOOD_DISEASE:
		{
			take_hit(damroll(10, 10), "poisonous food");
			(void)do_dec_stat(A_STR);
			*ident = TRUE;
			break;
		}

		case SV_FOOD_CURE_POISON:
		{
			if (clear_timed(TMD_POISONED, TRUE)) *ident = TRUE;
			break;
		}

		case SV_FOOD_CURE_BLINDNESS:
		{
			if (clear_timed(TMD_BLIND, TRUE)) *ident = TRUE;
			break;
		}

		case SV_FOOD_CURE_PARANOIA:
		{
			if (clear_timed(TMD_AFRAID, TRUE)) *ident = TRUE;
			break;
		}

		case SV_FOOD_CURE_CONFUSION:
		{
			if (clear_timed(TMD_CONFUSED, TRUE)) *ident = TRUE;
			break;
		}

		case SV_FOOD_CURE_SERIOUS:
		{
			if (hp_player(damroll(6, 8))) *ident = TRUE;
			break;
		}

		case SV_FOOD_RESTORE_STR:
		{
			if (do_res_stat(A_STR)) *ident = TRUE;
			break;
		}

		case SV_FOOD_RESTORE_CON:
		{
			if (do_res_stat(A_CON)) *ident = TRUE;
			break;
		}

		case SV_FOOD_RESTORING:
		{
			if (do_res_stat(A_STR)) *ident = TRUE;
			if (do_res_stat(A_INT)) *ident = TRUE;
			if (do_res_stat(A_WIS)) *ident = TRUE;
			if (do_res_stat(A_DEX)) *ident = TRUE;
			if (do_res_stat(A_CON)) *ident = TRUE;
			if (do_res_stat(A_CHR)) *ident = TRUE;
			break;
		}

		case SV_FOOD_FIRST_AID:
		{
			if (hp_player(damroll(1, 6))) *ident = TRUE;
			break;
		}

		case SV_FOOD_MINOR_CURES:
		{
			if (hp_player(damroll(2, 6))) *ident = TRUE;
			break;
		}

		case SV_FOOD_LIGHT_CURES:
		{
			if (hp_player(damroll(3, 6))) *ident = TRUE;
			break;
		}

		case SV_FOOD_RESTORATION:
		{
			if (hp_player(damroll(3, 6))) *ident = TRUE;
			break;
		}

		case SV_FOOD_MAJOR_CURES:
		{
			if (hp_player(damroll(3, 12))) *ident = TRUE;
			break;
		}

		case SV_FOOD_RATION:
		case SV_FOOD_FINE_MUSH:
		case SV_FOOD_SLIME_MOLD:
		{
			msg_print("That tastes good.");
			*ident = TRUE;
			break;
		}

		case SV_FOOD_WAYBREAD:
		{
			msg_print("That tastes good.");
			(void)clear_timed(TMD_POISONED, TRUE);
			(void)hp_player(damroll(4, 8));
			*ident = TRUE;
			break;
		}

	}

	/* Food can feed the player */
	(void)set_food(p_ptr->food + o_ptr->pval);

	return (TRUE);
}


/*
 * Quaff a potion
 */
static bool quaff_potion(object_type *o_ptr, bool *ident)
{
	/* Analyze the potion */
	switch (o_ptr->sval)
	{
		case SV_POTION_WATER:
		case SV_POTION_APPLE_JUICE:
		case SV_POTION_SLIME_MOLD:
		{
			msg_print("You feel less thirsty.");
			*ident = TRUE;
			break;
		}

		case SV_POTION_SLOWNESS:
		{
			if (inc_timed(TMD_SLOW, randint(25) + 15, TRUE)) *ident = TRUE;
			break;
		}

		case SV_POTION_SALT_WATER:
		{
			msg_print("The potion makes you vomit!");
			(void)set_food(PY_FOOD_STARVE - 1);
			(void)clear_timed(TMD_POISONED, TRUE);
			(void)inc_timed(TMD_PARALYZED, 4, TRUE);
			*ident = TRUE;
			break;
		}

		case SV_POTION_POISON:
		{
			if (!(p_ptr->state.resist_pois || p_ptr->timed[TMD_OPP_POIS] || p_ptr->state.immune_pois))
			{
				if (inc_timed(TMD_POISONED, rand_int(15) + 10, TRUE))
				{
					*ident = TRUE;
				}
			}
			break;
		}

		case SV_POTION_BLINDNESS:
		{
			if (!p_ptr->state.resist_blind)
			{
				if (inc_timed(TMD_BLIND, rand_int(100) + 100, TRUE))
				{
					*ident = TRUE;
				}
			}
			break;
		}

		case SV_POTION_CONFUSION:
		{
			if (allow_player_confusion())
			{
				if (inc_timed(TMD_CONFUSED, rand_int(20) + 15, TRUE))
				{
					*ident = TRUE;
				}
			}
			break;
		}

		case SV_POTION_SLEEP:
		{
			if (!p_ptr->state.free_act)
			{
				if (inc_timed(TMD_PARALYZED, rand_int(4) + 4, TRUE))
				{
					*ident = TRUE;
				}
			}
			break;
		}

		case SV_POTION_LOSE_MEMORIES:
		{
			if (!p_ptr->state.hold_life && (p_ptr->exp > 0))
			{
				msg_print("You feel your memories fade.");
				lose_exp(p_ptr->exp / 4);
				*ident = TRUE;
			}
			break;
		}

		case SV_POTION_DRAIN_MANA:
		{
			if (p_ptr->csp)
			{
				p_ptr->csp /= 2;
				msg_print("Your feel your head cloud up.");
				p_ptr->redraw |= (PR_MANA);
				*ident = TRUE;
			}
			break;
		}

		case SV_POTION_RUINATION:
		{
			msg_print("Your nerves and muscles feel weak and lifeless!");
			take_hit(damroll(10, 10), "a potion of Ruination");
			(void)dec_stat(A_DEX, 25, TRUE);
			(void)dec_stat(A_WIS, 25, TRUE);
			(void)dec_stat(A_CON, 25, TRUE);
			(void)dec_stat(A_STR, 25, TRUE);
			(void)dec_stat(A_CHR, 25, TRUE);
			(void)dec_stat(A_INT, 25, TRUE);
			*ident = TRUE;
			break;
		}

		case SV_POTION_DEC_STR:
		{
			if (do_dec_stat(A_STR)) *ident = TRUE;
			break;
		}

		case SV_POTION_DEC_INT:
		{
			if (do_dec_stat(A_INT)) *ident = TRUE;
			break;
		}

		case SV_POTION_DEC_WIS:
		{
			if (do_dec_stat(A_WIS)) *ident = TRUE;
			break;
		}

		case SV_POTION_DEC_DEX:
		{
			if (do_dec_stat(A_DEX)) *ident = TRUE;
			break;
		}

		case SV_POTION_DEC_CON:
		{
			if (do_dec_stat(A_CON)) *ident = TRUE;
			break;
		}

		case SV_POTION_DEC_CHR:
		{
			if (do_dec_stat(A_CHR)) *ident = TRUE;
			break;
		}

		case SV_POTION_DETONATIONS:
		{
			msg_print("Massive explosions rupture your body!");
			take_hit(damroll(50, 20), "a potion of Detonation");
			(void)set_stun(p_ptr->timed[TMD_STUN] + 75);
			(void)set_cut(p_ptr->timed[TMD_CUT] + 5000);
			*ident = TRUE;
			break;
		}

		case SV_POTION_DEATH:
		{
			msg_print("A feeling of Death flows through your body.");
			take_hit(5000, "a potion of Death");
			*ident = TRUE;
			break;
		}

		case SV_POTION_INFRAVISION:
		{
			if (inc_timed(TMD_SINFRA, 100 + randint(100), TRUE))
			{
				*ident = TRUE;
			}
			break;
		}

		case SV_POTION_DETECT_INVIS:
		{
			if (inc_timed(TMD_SINVIS, 12 + randint(12), TRUE))
			{
				*ident = TRUE;
			}
			break;
		}

		case SV_POTION_SLOW_POISON:
		{
			if (dec_timed(TMD_POISONED, p_ptr->timed[TMD_POISONED] / 2 + 1, TRUE)) *ident = TRUE;
			break;
		}

		case SV_POTION_CURE_POISON:
		{
			if (clear_timed(TMD_POISONED, TRUE)) *ident = TRUE;
			break;
		}

		case SV_POTION_BOLDNESS:
		{
			if (clear_timed(TMD_AFRAID, TRUE)) *ident = TRUE;
			break;
		}

		case SV_POTION_SPEED:
		{
			if (!p_ptr->timed[TMD_FAST])
			{
				if (set_timed(TMD_FAST, randint(25) + 15, TRUE)) *ident = TRUE;
			}
			else
			{
				(void)inc_timed(TMD_FAST, 5, TRUE);
			}
			break;
		}

		case SV_POTION_RESIST_HEAT:
		{
			if (inc_timed(TMD_OPP_FIRE, randint(10) + 10, TRUE))
			{
				*ident = TRUE;
			}
			break;
		}

		case SV_POTION_RESIST_COLD:
		{
			if (inc_timed(TMD_OPP_COLD, randint(10) + 10, TRUE))
			{
				*ident = TRUE;
			}
			break;
		}

		case SV_POTION_HEROISM:
		{
			if (game_mode == GAME_NPPMORIA)
			{
				if (!p_ptr->timed[TMD_HERO])
				{
					p_ptr->chp +=10;
					p_ptr->mhp +=10;
				}
			}
			if (hp_player(10)) *ident = TRUE;
			if (clear_timed(TMD_AFRAID, TRUE)) *ident = TRUE;
			if (inc_timed(TMD_HERO, randint(25) + 25, TRUE)) *ident = TRUE;

			break;
		}

		case SV_POTION_BERSERK_STRENGTH:
		{
			if (game_mode == GAME_NPPMORIA)
			{
				if (!p_ptr->timed[TMD_SHERO])
				{
					p_ptr->mhp +=15;
					p_ptr->chp +=15;
				}
			}
			if (hp_player(30)) *ident = TRUE;
			if (clear_timed(TMD_AFRAID, TRUE)) *ident = TRUE;
			if (inc_timed(TMD_SHERO, randint(25) + 25, TRUE)) *ident = TRUE;
			break;
		}

		case SV_POTION_CURE_LIGHT:
		{
			int dam_dice = 3;
			int dam_side = 8;

			if (game_mode == GAME_NPPMORIA)
			{
				dam_dice = 2;
				dam_side = 7;
			}

			if (hp_player(damroll(dam_dice, dam_side))) *ident = TRUE;
			if (clear_timed(TMD_BLIND, TRUE)) *ident = TRUE;
			if (set_cut(p_ptr->timed[TMD_CUT] - 10)) *ident = TRUE;
			break;
		}

		case SV_POTION_CURE_SERIOUS:
		{
			int dam_dice = 5;
			int dam_side = 10;

			if (game_mode == GAME_NPPMORIA)
			{
				dam_dice = 4;
				dam_side = 7;
			}

			if (hp_player(damroll(dam_dice, dam_side))) *ident = TRUE;
			if (clear_timed(TMD_BLIND, TRUE)) *ident = TRUE;
			if (clear_timed(TMD_CONFUSED, TRUE)) *ident = TRUE;
			if (set_cut((p_ptr->timed[TMD_CUT] / 2) - 50)) *ident = TRUE;
			break;
		}

		case SV_POTION_CURE_CRITICAL:
		{
			int dam_dice = 8;
			int dam_side = 10;

			if (game_mode == GAME_NPPMORIA)
			{
				dam_dice = 6;
				dam_side = 7;
			}

			if (hp_player(damroll(dam_dice, dam_side))) *ident = TRUE;
			if (clear_timed(TMD_BLIND, TRUE)) *ident = TRUE;
			if (clear_timed(TMD_CONFUSED, TRUE)) *ident = TRUE;
			if (clear_timed(TMD_POISONED, TRUE)) *ident = TRUE;
			if (set_stun(0)) *ident = TRUE;
			if (set_cut(0)) *ident = TRUE;
			break;
		}

		case SV_POTION_HEALING:
		{
			int heal = 325;

			if (game_mode == GAME_NPPMORIA)
			{
				heal = 1000;
			}

			if (hp_player(heal)) *ident = TRUE;
			if (clear_timed(TMD_BLIND, TRUE)) *ident = TRUE;
			if (clear_timed(TMD_CONFUSED, TRUE)) *ident = TRUE;
			if (clear_timed(TMD_POISONED, TRUE)) *ident = TRUE;
			if (set_stun(0)) *ident = TRUE;
			if (set_cut(0)) *ident = TRUE;
			break;
		}

		case SV_POTION_STAR_HEALING:
		{
			if (hp_player(1500)) *ident = TRUE;
			if (clear_timed(TMD_BLIND, TRUE)) *ident = TRUE;
			if (clear_timed(TMD_CONFUSED, TRUE)) *ident = TRUE;
			if (clear_timed(TMD_POISONED, TRUE)) *ident = TRUE;
			if (set_stun(0)) *ident = TRUE;
			if (set_cut(0)) *ident = TRUE;
			break;
		}

		case SV_POTION_LIFE:
		{
			msg_print("You feel life flow through your body!");
			restore_level();
			(void)clear_timed(TMD_POISONED, TRUE);
			(void)clear_timed(TMD_BLIND, TRUE);
			(void)clear_timed(TMD_CONFUSED, TRUE);
			(void)clear_timed(TMD_IMAGE, TRUE);
			(void)set_stun(0);
			(void)set_cut(0);
			(void)do_res_stat(A_STR);
			(void)do_res_stat(A_CON);
			(void)do_res_stat(A_DEX);
			(void)do_res_stat(A_WIS);
			(void)do_res_stat(A_INT);
			(void)do_res_stat(A_CHR);

			/* Recalculate max. hitpoints */
			update_stuff();

			hp_player(5000);

			*ident = TRUE;
			break;
		}

		case SV_POTION_RESTORE_MANA:
		{
			if (p_ptr->csp < p_ptr->msp)
			{
				p_ptr->csp = p_ptr->msp;
				p_ptr->csp_frac = 0;
				msg_print("Your feel your head clear.");
				p_ptr->redraw |= (PR_MANA);
				*ident = TRUE;
			}
			break;
		}

		case SV_POTION_RESTORE_EXP:
		{
			if (restore_level()) *ident = TRUE;
			break;
		}

		case SV_POTION_RES_STR:
		{
			if (do_res_stat(A_STR)) *ident = TRUE;
			break;
		}

		case SV_POTION_RES_INT:
		{
			if (do_res_stat(A_INT)) *ident = TRUE;
			break;
		}

		case SV_POTION_RES_WIS:
		{
			if (do_res_stat(A_WIS)) *ident = TRUE;
			break;
		}

		case SV_POTION_RES_DEX:
		{
			if (do_res_stat(A_DEX)) *ident = TRUE;
			break;
		}

		case SV_POTION_RES_CON:
		{
			if (do_res_stat(A_CON)) *ident = TRUE;
			break;
		}

		case SV_POTION_RES_CHR:
		{
			if (do_res_stat(A_CHR)) *ident = TRUE;
			break;
		}

		case SV_POTION_INC_STR:
		{
			if (do_inc_stat(A_STR)) *ident = TRUE;
			break;
		}

		case SV_POTION_INC_INT:
		{
			if (do_inc_stat(A_INT)) *ident = TRUE;
			break;
		}

		case SV_POTION_INC_WIS:
		{
			if (do_inc_stat(A_WIS)) *ident = TRUE;
			break;
		}

		case SV_POTION_INC_DEX:
		{
			if (do_inc_stat(A_DEX)) *ident = TRUE;
			break;
		}

		case SV_POTION_INC_CON:
		{
			if (do_inc_stat(A_CON)) *ident = TRUE;
			break;
		}

		case SV_POTION_INC_CHR:
		{
			if (do_inc_stat(A_CHR)) *ident = TRUE;
			break;
		}

		case SV_POTION_AUGMENTATION:
		{
			if (do_inc_stat(A_STR)) *ident = TRUE;
			if (do_inc_stat(A_INT)) *ident = TRUE;
			if (do_inc_stat(A_WIS)) *ident = TRUE;
			if (do_inc_stat(A_DEX)) *ident = TRUE;
			if (do_inc_stat(A_CON)) *ident = TRUE;
			if (do_inc_stat(A_CHR)) *ident = TRUE;
			break;
		}

		case SV_POTION_ENLIGHTENMENT:
		{
			msg_print("An image of your surroundings forms in your mind...");
			wiz_light();
			*ident = TRUE;
			break;
		}

		case SV_POTION_STAR_ENLIGHTENMENT:
		{
			msg_print("You begin to feel more enlightened...");
			wiz_light();
			(void)do_inc_stat(A_INT);
			(void)do_inc_stat(A_WIS);
			detect(DETECT_RADIUS, DETECT_ENLIGHTENMENT);
			identify_and_squelch_pack();
			self_knowledge();
			*ident = TRUE;
			break;
		}

		case SV_POTION_SELF_KNOWLEDGE:
		{
			msg_print("You begin to know yourself a little better...");
			self_knowledge();
			*ident = TRUE;
			break;
		}

		case SV_POTION_EXPERIENCE:
		{
			if (p_ptr->exp < PY_MAX_EXP)
			{
				s32b ee = (p_ptr->exp / 2) + 10;
				if (ee > 100000L) ee = 100000L;
				msg_print("You feel more experienced.");
				gain_exp(ee);
				*ident = TRUE;
			}
			break;
		}

		case SV_POTION_RESIST_ACID:
		{
			if (inc_timed(TMD_OPP_ACID, randint(10) + 10, TRUE))
			{
				*ident = TRUE;
			}
			break;
		}

		case SV_POTION_RESIST_ELECTRICITY:
		{
			if (inc_timed(TMD_OPP_ELEC, randint(10) + 10, TRUE))
			{
				*ident = TRUE;
			}
			break;
		}

		case SV_POTION_RESIST_POISON:
		{
			if (inc_timed(TMD_OPP_POIS, randint(15) + 15, TRUE))
			{
				*ident = TRUE;
			}
			break;
		}

		case SV_POTION_RESISTANCE:
		{
			int act_time = randint(30) + 30;
			if(inc_timed(TMD_OPP_ACID, act_time, TRUE)) *ident = TRUE;
			if(inc_timed(TMD_OPP_ELEC, act_time, TRUE)) *ident = TRUE;
			if(inc_timed(TMD_OPP_FIRE, act_time, TRUE)) *ident = TRUE;
			if(inc_timed(TMD_OPP_COLD, act_time, TRUE)) *ident = TRUE;
			if(inc_timed(TMD_OPP_POIS, act_time, TRUE)) *ident = TRUE;
			break;
		}

		case SV_POTION_INVULNERABILITY:
		{
			if (p_ptr->timed[TMD_INVULN])
			{
				if (inc_timed(TMD_INVULN, 5, TRUE)) *ident = TRUE;
			}
			else if (inc_timed(TMD_INVULN, randint(10) + 10, TRUE)) *ident = TRUE;
			break;
		}

	}

	/*
	 * Some potions can feed the player
	 * Hack - but they can't gorge the player.
	 */
	if (o_ptr->pval)
	{
		int new_food = p_ptr->food + o_ptr->pval;

		if (new_food >= PY_FOOD_MAX)
		{
			new_food = PY_FOOD_MAX - 1;
		}

		if (new_food > p_ptr->food)(void)set_food(new_food);
	}

	return (TRUE);
}


/*
 * Read a scroll
 */
static bool read_scroll(object_type *o_ptr, bool *ident)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int k;

	bool used_up = TRUE;

	/* Analyze the scroll */
	switch (o_ptr->sval)
	{
		case SV_SCROLL_DARKNESS:
		{
			if (!p_ptr->state.resist_blind)
			{
				(void)inc_timed(TMD_BLIND, 3 + randint(5), TRUE);
			}
			if (unlight_area(10, 3)) *ident = TRUE;
			break;
		}

		case SV_SCROLL_AGGRAVATE_MONSTER:
		{
			msg_print("There is a high pitched humming noise.");
			aggravate_monsters(SOURCE_PLAYER);
			*ident = TRUE;
			break;
		}

		case SV_SCROLL_CURSE_ARMOR:
		{
			if (curse_armor()) *ident = TRUE;
			break;
		}

		case SV_SCROLL_CURSE_WEAPON:
		{
			if (curse_weapon()) *ident = TRUE;
			break;
		}

		case SV_SCROLL_SUMMON_MONSTER:
		{
			sound(MSG_SUM_MONSTER);
			for (k = 0; k < randint(3); k++)
			{
				if (summon_specific(py, px, p_ptr->depth, 0, MPLACE_OVERRIDE))
				{
					*ident = TRUE;
				}
			}
			break;
		}

		case SV_SCROLL_SUMMON_UNDEAD:
		{
			sound(MSG_SUM_UNDEAD);
			for (k = 0; k < randint(3); k++)
			{
				if (summon_specific(py, px, p_ptr->depth, SUMMON_UNDEAD, MPLACE_OVERRIDE))
				{
					*ident = TRUE;
				}
			}
			break;
		}

		case SV_SCROLL_SUMMON_UNIQUE:
		{

			if (summon_specific(py, px, p_ptr->depth, SUMMON_UNIQUE, MPLACE_OVERRIDE))
			{
				*ident = TRUE;
			}

			break;
		}

		case SV_SCROLL_TRAP_CREATION:
		{
			if (trap_creation(SOURCE_OTHER)) *ident = TRUE;
			break;
		}

		case SV_SCROLL_PHASE_DOOR:
		{
			teleport_player(10, FALSE);
			*ident = TRUE;
			break;
		}

		case SV_SCROLL_TELEPORT:
		{
			teleport_player(100, FALSE);
			*ident = TRUE;
			break;
		}

		case SV_SCROLL_TELEPORT_LEVEL:
		{
			if (!teleport_player_level(SOURCE_PLAYER)) used_up = FALSE;
			*ident = TRUE;
			break;
		}

		case SV_SCROLL_WORD_OF_RECALL:
		{
			if (!set_recall()) used_up = FALSE;
			*ident = TRUE;
			break;
		}

		case SV_SCROLL_IDENTIFY:
		{
			*ident = TRUE;
			if (!ident_spell()) used_up = FALSE;
			break;
		}

		case SV_SCROLL_STAR_IDENTIFY:
		{
			*ident = TRUE;
			if (!identify_fully()) used_up = FALSE;
			break;
		}

		case SV_SCROLL_REMOVE_CURSE:
		{
			if (remove_curse(FALSE))
			{
				msg_print("You feel as if someone is watching over you.");
				*ident = TRUE;
			}
			break;
		}

		case SV_SCROLL_STAR_REMOVE_CURSE:
		{
			remove_curse(TRUE);
			*ident = TRUE;
			break;
		}

		case SV_SCROLL_ENCHANT_ARMOR:
		{
			*ident = TRUE;
			if (!enchant_spell(0, 0, 1)) used_up = FALSE;
			break;
		}

		case SV_SCROLL_ENCHANT_WEAPON_TO_HIT:
		{
			if (!enchant_spell(1, 0, 0)) used_up = FALSE;
			*ident = TRUE;
			break;
		}

		case SV_SCROLL_ENCHANT_WEAPON_TO_DAM:
		{
			if (!enchant_spell(0, 1, 0)) used_up = FALSE;
			*ident = TRUE;
			break;
		}

		case SV_SCROLL_STAR_ENCHANT_ARMOR:
		{
			if (!enchant_spell(0, 0, randint(3) + 2)) used_up = FALSE;
			*ident = TRUE;
			break;
		}

		case SV_SCROLL_STAR_ENCHANT_WEAPON:
		{
			if (!enchant_spell(randint(3), randint(3), 0)) used_up = FALSE;
			*ident = TRUE;
			break;
		}

		case SV_SCROLL_RECHARGING:
		{
			if (!recharge(60, FALSE, 50)) used_up = FALSE;
			*ident = TRUE;
			break;
		}

		case SV_SCROLL_STAR_RECHARGING:
		{
			if (!recharge(150, TRUE, 75)) used_up = FALSE;
			*ident = TRUE;
			break;
		}

		case SV_SCROLL_LIGHT:
		{
			int dam_dice = 2;

			if (game_mode == GAME_NPPMORIA) dam_dice = 1;

			if (light_area(damroll(dam_dice, 8), 2)) *ident = TRUE;
			break;
		}

		case SV_SCROLL_MAPPING:
		{
			detect(DETECT_RADIUS + 5, DETECT_MAP);
			*ident = TRUE;
			break;
		}

		case SV_SCROLL_DETECT_GOLD:
		{
			if (detect(DETECT_RADIUS, DETECT_ALL_TREASURE)) *ident = TRUE;
			break;
		}

		case SV_SCROLL_DETECT_ITEM:
		{
			if (detect(DETECT_RADIUS, DETECT_OBJECTS)) *ident = TRUE;
			break;
		}

		case SV_SCROLL_DETECT_TRAP:
		{
			if (detect(DETECT_RADIUS, DETECT_TRAPS)) *ident = TRUE;
			break;
		}

		case SV_SCROLL_DETECT_DOOR:
		{
			if (detect(DETECT_RADIUS, DETECT_DOORS_STAIRS)) *ident = TRUE;
			break;
		}

		case SV_SCROLL_DETECT_INVIS:
		{
			if (detect(DETECT_RADIUS, DETECT_INVISIBLE)) *ident = TRUE;
			break;
		}

		case SV_SCROLL_SATISFY_HUNGER:
		{
			if (set_food(PY_FOOD_MAX - 1)) *ident = TRUE;
			break;
		}

		case SV_SCROLL_BLESSING:
		{
			if (inc_timed(TMD_BLESSED, randint(12) + 6, TRUE)) *ident = TRUE;
			break;
		}

		case SV_SCROLL_HOLY_CHANT:
		{
			if (inc_timed(TMD_BLESSED, randint(24) + 12, TRUE)) *ident = TRUE;
			break;
		}

		case SV_SCROLL_HOLY_PRAYER:
		{
			if (inc_timed(TMD_BLESSED, randint(48) + 24, TRUE)) *ident = TRUE;
			break;
		}

		case SV_SCROLL_MONSTER_CONFUSION:
		{
			if (p_ptr->confusing == 0)
			{
				msg_print("Your hands begin to glow.");
				p_ptr->confusing = TRUE;
				*ident = TRUE;
			}
			break;
		}

		case SV_SCROLL_PROTECTION_FROM_EVIL:
		{
			k = 3 * p_ptr->lev;
			if (inc_timed(TMD_PROTEVIL, randint(25) + k, TRUE)) *ident = TRUE;
			break;
		}

		case SV_SCROLL_RUNE_OF_PROTECTION:
		{
			if (!warding_glyph()) used_up = FALSE;
			*ident = TRUE;
			break;
		}

		case SV_SCROLL_TRAP_DOOR_DESTRUCTION:
		{
			if (destroy_doors_touch()) *ident = TRUE;
			break;
		}

		case SV_SCROLL_STAR_DESTRUCTION:
		{
			destroy_area(py, px, 15);
			*ident = TRUE;
			break;
		}

		case SV_SCROLL_CREATE_MONSTER_TRAP:
		{
			if (make_monster_trap()) *ident = TRUE;
			else used_up = FALSE;
			break;
		}

		case SV_SCROLL_DISPEL_UNDEAD:
		{
			if (dispel_undead(60)) *ident = TRUE;
			break;
		}

		case SV_SCROLL_CREATE_RANDART:
		{
			object_type *o_ptr;

			int item;

			/* artifact power is based on depth */
			int randart_power = 10 + effective_depth(p_ptr->depth);

			/* Get an item */
			cptr q = "Choose an item to be made into an artifact. ";
			cptr s = "You have no eligible item.";
			/* Only accept legal items. */
			item_tester_hook = item_tester_hook_randart;

			if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR))) break;

			/* Got the item */
			o_ptr = &inventory[item];

			if ((adult_no_artifacts) || (adult_no_xtra_artifacts))
			{
				msg_print("Nothing happens.");
				break;
			}

			/* occasional power boost */
			while (one_in_(25)) randart_power += 25;

			if (make_one_randart(o_ptr, randart_power, FALSE))
			{
				*ident = TRUE;
				used_up = TRUE;

				/* Identify it fully */
				object_aware(o_ptr);
				object_known(o_ptr);

				/* Mark the item as fully known */
				o_ptr->ident |= (IDENT_MENTAL);

				object_history(o_ptr, ORIGIN_ACQUIRE, 0);

				/* Let the player know what they just got */
				object_info_screen(o_ptr);

			}
			else msg_print("The Artifact creation failed");
			break;
		}

		case SV_SCROLL_BANISHMENT:
		{
			if (!banishment()) used_up = FALSE;
			*ident = TRUE;
			break;
		}

		case SV_SCROLL_MASS_BANISHMENT:
		{
			(void)mass_banishment();
			*ident = TRUE;
			break;
		}

		case SV_SCROLL_ACQUIREMENT:
		{
			acquirement(py, px, 1, TRUE);
			*ident = TRUE;
			break;
		}

		case SV_SCROLL_STAR_ACQUIREMENT:
		{
			acquirement(py, px, randint(2) + 1, TRUE);
			*ident = TRUE;
			break;
		}
		case SV_SCROLL_CREATE_FOOD:
		{
			create_food();
			*ident = TRUE;
			break;
		}
		case SV_SCROLL_CREATE_DOORS:
		{
			door_creation();
			*ident = TRUE;
			break;
		}
		case SV_SCROLL_SLEEP_MONSTER:
		{
			sleep_monsters_touch();
			*ident = TRUE;
			break;
		}
	}

	return (used_up);
}


/*
 * Use a staff
 */
static bool use_staff(object_type *o_ptr, bool *ident)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int k;

	bool use_charge = TRUE;

	/* Analyze the staff */
	switch (o_ptr->sval)
	{
		case SV_STAFF_DARKNESS:
		{
			if (!p_ptr->state.resist_blind)
			{
				if (inc_timed(TMD_BLIND, 3 + randint(5), TRUE)) *ident = TRUE;
			}
			if (unlight_area(10, 3)) *ident = TRUE;
			break;
		}

		case SV_STAFF_SLOWNESS:
		{
			if (inc_timed(TMD_SLOW, randint(30) + 15, TRUE)) *ident = TRUE;
			break;
		}

		case SV_STAFF_HASTE_MONSTERS:
		{
			if (speed_monsters()) *ident = TRUE;
			break;
		}

		case SV_STAFF_SUMMONING:
		{
			sound(MSG_SUM_MONSTER);
			for (k = 0; k < randint(4); k++)
			{
				if (summon_specific(py, px, p_ptr->depth, 0, MPLACE_OVERRIDE))
				{
					*ident = TRUE;
				}
			}
			break;
		}

		case SV_STAFF_TELEPORTATION:
		{
			teleport_player(100, FALSE);
			*ident = TRUE;
			break;
		}

		case SV_STAFF_IDENTIFY:
		{
			if (!ident_spell()) use_charge = FALSE;
			*ident = TRUE;
			break;
		}

		case SV_STAFF_STARLIGHT:
		{
			int dam_dice = 6; /* (game_mode == GAME_NPPANGBAND) */
			if (game_mode == GAME_NPPMORIA) dam_dice = 2;

			if (!p_ptr->timed[TMD_BLIND])
			{
				msg_print("The end of the staff glows brightly...");
			}
			for (k = 0; k < 8; k++) light_line(ddd[k], damroll(dam_dice, 8));
			*ident = TRUE;
			break;
		}

		case SV_STAFF_LIGHT:
		{
			if (light_area(damroll(2, 8), 2)) *ident = TRUE;
			break;
		}

		case SV_STAFF_MAPPING:
		{
			detect(DETECT_RADIUS + 5, DETECT_MAP);
			*ident = TRUE;
			break;
		}

		case SV_STAFF_DETECT_GOLD:
		{
			if (detect(DETECT_RADIUS, DETECT_TREASURE)) *ident = TRUE;
			if (detect(DETECT_RADIUS, DETECT_GOLD)) *ident = TRUE;
			break;
		}

		case SV_STAFF_DETECT_ITEM:
		{
			if (detect(DETECT_RADIUS, DETECT_OBJECTS)) *ident = TRUE;
			break;
		}

		case SV_STAFF_DETECT_TRAP:
		{
			if (detect(DETECT_RADIUS, DETECT_TRAPS)) *ident = TRUE;
			break;
		}

		case SV_STAFF_DETECT_DOOR:
		{
			if (detect(DETECT_RADIUS, DETECT_DOORS_STAIRS)) *ident = TRUE;
			break;
		}

		case SV_STAFF_DETECT_INVIS:
		{
			if (detect(DETECT_RADIUS, DETECT_INVISIBLE)) *ident = TRUE;
			break;
		}

		case SV_STAFF_DETECT_EVIL:
		{
			if (detect(DETECT_RADIUS, DETECT_EVIL)) *ident = TRUE;
			break;
		}

		case SV_STAFF_CURE_LIGHT:
		{
			int dam_dice = 2;  /* (game_mode == GAME_NPPANGBAND) */
			int dam_side = 10;

			if (game_mode == GAME_NPPMORIA)
			{
				dam_dice = 1;
				dam_side = 8;
			}

			if (hp_player(damroll(dam_dice, dam_side))) *ident = TRUE;
			break;
		}

		case SV_STAFF_CURING:
		{
			if (clear_timed(TMD_BLIND, TRUE)) *ident = TRUE;
			if (clear_timed(TMD_POISONED, TRUE)) *ident = TRUE;
			if (clear_timed(TMD_CONFUSED, TRUE)) *ident = TRUE;
			if (set_stun(0)) *ident = TRUE;
			if (set_cut(0)) *ident = TRUE;
			break;
		}

		case SV_STAFF_HEALING:
		{
			if (hp_player(325)) *ident = TRUE;
			if (set_stun(0)) *ident = TRUE;
			if (set_cut(0)) *ident = TRUE;
			break;
		}

		case SV_STAFF_THE_MAGI:
		{
			if (do_res_stat(A_INT)) *ident = TRUE;
			if (p_ptr->csp < p_ptr->msp)
			{
				p_ptr->csp = p_ptr->msp;
				p_ptr->csp_frac = 0;
				*ident = TRUE;
				msg_print("Your feel your head clear.");
				p_ptr->redraw |= (PR_MANA);
			}
			break;
		}

		case SV_STAFF_SLEEP_MONSTERS:
		{
			if (game_mode == GAME_NPPMORIA)
			{
				if (sleep_monsters(500)) *ident = TRUE;
			}
			else if (sleep_monsters(damroll(3, p_ptr->lev))) *ident = TRUE; /* (game_mode == GAME_NPPANGBAND) */
			break;
		}

		case SV_STAFF_SLOW_MONSTERS:
		{
			if (slow_monsters(damroll (2, p_ptr->lev))) *ident = TRUE;
			break;
		}

		case SV_STAFF_SPEED:
		{
			if (!p_ptr->timed[TMD_FAST])
			{
				if (set_timed(TMD_FAST, randint(30) + 15, TRUE)) *ident = TRUE;
			}
			else
			{
				(void)inc_timed(TMD_FAST, 5, FALSE);
			}
			break;
		}

		case SV_STAFF_PROBING:
		{
			probing();
			*ident = TRUE;
			break;
		}

		case SV_STAFF_DISPEL_EVIL:
		{
			if (dispel_evil(60)) *ident = TRUE;
			break;
		}

		case SV_STAFF_MASS_POLYMORPH:
		{
			if (mass_polymorph()) *ident = TRUE;
			break;
		}

		case SV_STAFF_REMOVE_CURSE:
		{
			if (remove_curse(FALSE)) *ident = TRUE;
			break;
		}

		case SV_STAFF_POWER:
		{
			if (dispel_monsters(120)) *ident = TRUE;
			break;
		}

		case SV_STAFF_HOLINESS:
		{
			if (dispel_evil(120)) *ident = TRUE;
			k = 3 * p_ptr->lev;
			if (inc_timed(TMD_PROTEVIL, randint(25) + k, TRUE)) *ident = TRUE;
			if (clear_timed(TMD_POISONED, TRUE)) *ident = TRUE;
			if (clear_timed(TMD_AFRAID, TRUE)) *ident = TRUE;
			if (hp_player(75)) *ident = TRUE;
			if (set_stun(0)) *ident = TRUE;
			if (set_cut(0)) *ident = TRUE;
			break;
		}

		case SV_STAFF_BANISHMENT:
		{
			if (!banishment()) use_charge = FALSE;
			*ident = TRUE;
			break;
		}

		case SV_STAFF_EARTHQUAKES:
		{
			earthquake(py, px, 10, FALSE);
			*ident = TRUE;
			break;
		}

		case SV_STAFF_DESTRUCTION:
		{
			destroy_area(py, px, 15);
			*ident = TRUE;
			break;
		}

		case SV_STAFF_MASS_IDENTIFY:
		{
		  	mass_identify(3);
			*ident = TRUE;
		  	break;
		}

	}

	return (use_charge);
}


/*
 * Aim a wand
 */
static bool aim_wand(object_type *o_ptr, bool *ident, int dir)
{
	int sval;


	/* Special allowance for disarming and traps */
	bool is_disarm = FALSE;

	if ((object_aware_p(o_ptr)) && ((o_ptr->sval == SV_WAND_DISARMING) ||
			(o_ptr->sval == SV_WAND_TRAP_DOOR_DEST))) is_disarm = TRUE;

	/* Allow direction to be canceled for free */
	if (!dir)
	{
		if (!get_aim_dir(&dir, is_disarm)) return (FALSE);
	}

	/* Take a turn */
	p_ptr->p_energy_use = BASE_ENERGY_MOVE;

	/* Not identified yet */
	*ident = FALSE;

	/* Sound */
	/* TODO: Create wand sound?  Do the individual effects have sounds? */
	/* sound(MSG_ZAP_ROD); */

	/* XXX Hack -- Extract the "sval" effect */
	sval = o_ptr->sval;

	/* XXX Hack -- Wand of wonder can do anything before it */
	if (sval == SV_WAND_WONDER) sval = rand_int(SV_WAND_WONDER);

	/* Analyze the wand */
	switch (sval)
	{
		case SV_WAND_HEAL_MONSTER:
		{
			if (heal_monster(dir, damroll(4, 6))) *ident = TRUE;
			break;
		}

		case SV_WAND_HASTE_MONSTER:
		{
			if (speed_monster(dir)) *ident = TRUE;
			break;
		}

		case SV_WAND_CLONE_MONSTER:
		{
			if (clone_monster(dir)) *ident = TRUE;
			break;
		}

		case SV_WAND_TELEPORT_AWAY:
		{
			if (teleport_monster(dir)) *ident = TRUE;
			break;
		}

		case SV_WAND_DISARMING:
		{
			if (disarm_trap(dir)) *ident = TRUE;
			break;
		}

		case SV_WAND_TRAP_DOOR_DEST:
		{
			if (destroy_door(dir)) *ident = TRUE;
			break;
		}

		case SV_WAND_STONE_TO_MUD:
		{
			if (wall_to_mud(dir, 20 + randint(30))) *ident = TRUE;
			break;
		}

		case SV_WAND_LIGHT:
		{
			int dam_dice = 6;

			if (game_mode == GAME_NPPMORIA) dam_dice = 2;

			msg_print("A line of blue shimmering light appears.");
			light_line(dir, damroll(dam_dice, 8));
			*ident = TRUE;
			break;
		}

		case SV_WAND_SLEEP_MONSTER:
		{
			if (sleep_monster(dir)) *ident = TRUE;
			break;
		}

		case SV_WAND_SLOW_MONSTER:
		{
			if (slow_monster(dir)) *ident = TRUE;
			break;
		}

		case SV_WAND_CONFUSE_MONSTER:
		{
			if (confuse_monster(dir, (p_ptr->lev * 2 / 3))) *ident = TRUE;
			break;
		}

		case SV_WAND_FEAR_MONSTER:
		{
			if (fear_monster(dir, 10)) *ident = TRUE;
			break;
		}

		case SV_WAND_DRAIN_LIFE:
		{
			int dam = 150;

			if (game_mode == GAME_NPPMORIA) dam = 75;

			if (drain_life(dir, dam)) *ident = TRUE;
			break;
		}

		case SV_WAND_POLYMORPH:
		{
			if (poly_monster(dir)) *ident = TRUE;
			break;
		}

		case SV_WAND_STINKING_CLOUD:
		{
			fire_ball(GF_POIS, dir, 12, 2);
			*ident = TRUE;
			break;
		}

		case SV_WAND_MAGIC_MISSILE:
		{
			fire_bolt_or_beam(20, GF_MISSILE, dir, damroll(3, 4));
			*ident = TRUE;
			break;
		}

		case SV_WAND_ACID_BOLT:
		{
			fire_bolt_or_beam(20, GF_ACID, dir, damroll(10, 8));
			*ident = TRUE;
			break;
		}

		case SV_WAND_ELEC_BOLT:
		{
			int dam_dice = 6;
			int dam_side = 6;

			if (game_mode == GAME_NPPMORIA)
			{
				dam_dice = 4;
				dam_side = 8;
			}

			fire_bolt_or_beam(20, GF_ELEC, dir, damroll(dam_dice, dam_side));
			*ident = TRUE;
			break;
		}

		case SV_WAND_FIRE_BOLT:
		{
			int dam_dice = 12;

			if (game_mode == GAME_NPPMORIA)
			{
				dam_dice = 9;
			}


			fire_bolt_or_beam(20, GF_FIRE, dir, damroll(dam_dice, 8));
			*ident = TRUE;
			break;
		}

		case SV_WAND_COLD_BOLT:
		{
			fire_bolt_or_beam(20, GF_COLD, dir, damroll(6, 8));
			*ident = TRUE;
			break;
		}

		case SV_WAND_ACID_BALL:
		{
			int dam = 120;

			if (game_mode == GAME_NPPMORIA)
			{
				dam = 60;
			}

			fire_ball(GF_ACID, dir, dam, 2);
			*ident = TRUE;
			break;
		}

		case SV_WAND_ELEC_BALL:
		{
			int dam = 64;

			if (game_mode == GAME_NPPMORIA)
			{
				dam = 32;
			}

			fire_ball(GF_ELEC, dir, dam, 2);
			*ident = TRUE;
			break;
		}

		case SV_WAND_FIRE_BALL:
		{
			int dam = 144;

			if (game_mode == GAME_NPPMORIA)
			{
				dam = 72;
			}

			fire_ball(GF_FIRE, dir, dam, 2);
			*ident = TRUE;
			break;
		}

		case SV_WAND_COLD_BALL:
		{
			int dam = 96;

			if (game_mode == GAME_NPPMORIA)
			{
				dam = 48;
			}

			fire_ball(GF_COLD, dir, dam, 2);
			*ident = TRUE;
			break;
		}

		case SV_WAND_WONDER:
		{
			msg_print("Oops.  Wand of wonder activated.");
			break;
		}

		case SV_WAND_DRAGON_FIRE:
		{
			fire_ball(GF_FIRE, dir, 200, 3);
			*ident = TRUE;
			break;
		}

		case SV_WAND_DRAGON_COLD:
		{
			fire_ball(GF_COLD, dir, 160, 3);
			*ident = TRUE;
			break;
		}

		case SV_WAND_DRAGON_BREATH:
		{
			switch (randint(5))
			{
				case 1:
				{
					fire_ball(GF_ACID, dir, 200, 3);
					break;
				}

				case 2:
				{
					fire_ball(GF_ELEC, dir, 160, 3);
					break;
				}

				case 3:
				{
					fire_ball(GF_FIRE, dir, 200, 3);
					break;
				}

				case 4:
				{
					fire_ball(GF_COLD, dir, 160, 3);
					break;
				}

				default:
				{
					fire_ball(GF_POIS, dir, 120, 3);
					break;
				}
			}

			*ident = TRUE;
			break;
		}

		case SV_WAND_ANNIHILATION:
		{
			if (drain_life(dir, 250)) *ident = TRUE;
			break;
		}

		case SV_WAND_WALL_BUILDING:
		{
			if (build_wall(dir, damroll(4, 8))) *ident = TRUE;
			break;
		}
	}


	return (TRUE);
}


/*
 * Zap a rod
 */
static bool zap_rod(object_type *o_ptr, bool *ident, int dir)
{
	bool used_charge = TRUE;
	object_kind *k_ptr = &k_info[o_ptr->k_idx];

	/* Take a turn */
	p_ptr->p_energy_use = BASE_ENERGY_MOVE;

	/* Not identified yet */
	*ident = FALSE;

	/* Still charging? */
	if (o_ptr->timeout > (o_ptr->pval - k_ptr->pval))
	{

		if (o_ptr->number == 1)
			msg_print("The rod is still charging");
		else
			msg_print("The rods are all still charging");

 		return FALSE;
	}

	/* Sound */
	sound(MSG_ZAP_ROD);

	/* Analyze the rod */
	switch (o_ptr->sval)
	{
		case SV_ROD_DETECT_TRAP:
		{
			if (detect(DETECT_RADIUS, DETECT_TRAPS)) *ident = TRUE;
			break;
		}

		case SV_ROD_DETECT_DOOR:
		{
			if (detect(DETECT_RADIUS, DETECT_DOORS_STAIRS)) *ident = TRUE;
			break;
		}

		case SV_ROD_IDENTIFY:
		{
			*ident = TRUE;
			if (!ident_spell()) used_charge = FALSE;
			break;
		}

		case SV_ROD_RECALL:
		{
			if (!set_recall()) used_charge = FALSE;
			*ident = TRUE;
			break;
		}

		case SV_ROD_ILLUMINATION:
		{
			if (light_area(damroll(2, 8), 2)) *ident = TRUE;
			break;
		}

		case SV_ROD_MAPPING:
		{
			detect(DETECT_RADIUS + 5, DETECT_MAP);
			*ident = TRUE;
			break;
		}

		case SV_ROD_DETECTION:
		{
			if (detect(DETECT_RADIUS, DETECT_ALL)) *ident = TRUE;
			*ident = TRUE;
			break;
		}

		case SV_ROD_PROBING:
		{
			if (!probing()) used_charge = FALSE;
			*ident = TRUE;
			break;
		}

		case SV_ROD_CURING:
		{
			if (clear_timed(TMD_BLIND, TRUE)) *ident = TRUE;
			if (clear_timed(TMD_POISONED, TRUE)) *ident = TRUE;
			if (clear_timed(TMD_CONFUSED, TRUE)) *ident = TRUE;
			if (set_stun(0)) *ident = TRUE;
			if (set_cut(0)) *ident = TRUE;
			break;
		}

		case SV_ROD_HEALING:
		{
			if (hp_player(550)) *ident = TRUE;
			if (set_stun(0)) *ident = TRUE;
			if (set_cut(0)) *ident = TRUE;
			break;
		}

		case SV_ROD_RESTORATION:
		{
			if (restore_level()) *ident = TRUE;
			if (do_res_stat(A_STR)) *ident = TRUE;
			if (do_res_stat(A_INT)) *ident = TRUE;
			if (do_res_stat(A_WIS)) *ident = TRUE;
			if (do_res_stat(A_DEX)) *ident = TRUE;
			if (do_res_stat(A_CON)) *ident = TRUE;
			if (do_res_stat(A_CHR)) *ident = TRUE;
			break;
		}

		case SV_ROD_SPEED:
		{
			if (!p_ptr->timed[TMD_FAST])
			{
				if (set_timed(TMD_FAST, randint(30) + 15, TRUE)) *ident = TRUE;
			}
			else
			{
				(void)inc_timed(TMD_FAST, 5, FALSE);
			}
			break;
		}

		case SV_ROD_STONE_TO_MUD:
		{
			if (wall_to_mud(dir, 20 + randint(30))) *ident = TRUE;
			break;
		}

		case SV_ROD_TELEPORT_AWAY:
		{
			if (teleport_monster(dir)) *ident = TRUE;
			break;
		}

		case SV_ROD_DISARMING:
		{
			if (disarm_trap(dir)) *ident = TRUE;
			break;
		}

		case SV_ROD_LIGHT:
		{
			msg_print("A line of blue shimmering light appears.");
			light_line(dir, damroll(6, 8));
			*ident = TRUE;
			break;
		}

		case SV_ROD_SLEEP_MONSTER:
		{
			if (sleep_monster(dir)) *ident = TRUE;
			break;
		}

		case SV_ROD_SLOW_MONSTER:
		{
			if (slow_monster(dir)) *ident = TRUE;
			break;
		}

		case SV_ROD_DRAIN_LIFE:
		{
			if (drain_life(dir, 150)) *ident = TRUE;
			break;
		}

		case SV_ROD_POLYMORPH:
		{
			if (poly_monster(dir)) *ident = TRUE;
			break;
		}

		case SV_ROD_ACID_BOLT:
		{
			fire_bolt_or_beam(10, GF_ACID, dir, damroll(12, 8));
			*ident = TRUE;
			break;
		}

		case SV_ROD_ELEC_BOLT:
		{
			fire_bolt_or_beam(10, GF_ELEC, dir, damroll(6, 6));
			*ident = TRUE;
			break;
		}

		case SV_ROD_FIRE_BOLT:
		{
			fire_bolt_or_beam(10, GF_FIRE, dir, damroll(16, 8));
			*ident = TRUE;
			break;
		}

		case SV_ROD_COLD_BOLT:
		{
			fire_bolt_or_beam(10, GF_COLD, dir, damroll(10, 8));
			*ident = TRUE;
			break;
		}

		case SV_ROD_ACID_BALL:
		{
			fire_ball(GF_ACID, dir, 120, 2);
			*ident = TRUE;
			break;
		}

		case SV_ROD_ELEC_BALL:
		{
			fire_ball(GF_ELEC, dir, 64, 2);
			*ident = TRUE;
			break;
		}

		case SV_ROD_FIRE_BALL:
		{
			fire_ball(GF_FIRE, dir, 144, 2);
			*ident = TRUE;
			break;
		}

		case SV_ROD_COLD_BALL:
		{
			fire_ball(GF_COLD, dir, 96, 2);
			*ident = TRUE;
			break;
		}
		case SV_ROD_STAR_IDENTIFY:
		{
			if (!identify_fully()) used_charge = FALSE;
			*ident = TRUE;
			break;
		}
		case SV_ROD_MASS_IDENTIFY:
		{
			mass_identify(3);
			*ident = TRUE;
			break;
		}
	}

	/* We used the object. */
	if (used_charge)
	{
		o_ptr->timeout += k_ptr->pval;

	}

	return TRUE;
}


/*
 * Activate a wielded object.  Wielded objects never stack.
 * And even if they did, activatable objects never stack.
 *
 * Currently, only (some) artifacts, and Dragon Scale Mail, can be activated.
 * But one could, for example, easily make an activatable "Ring of Plasma".
 *
 * Note that it always takes a turn to activate an artifact, even if
 * the user hits "escape" at the "direction" prompt.
 */
static bool activate_object(object_type *o_ptr, int dir)
{
	int k, i, chance;


	/* Check the recharge */
	if (o_ptr->timeout)
	{
		msg_print("It whines, glows and fades...");
		return FALSE;
	}

	/* Activate the artifact */
	message(MSG_ACT_ARTIFACT, 0, "You activate it...");

	/* Artifacts, except for special artifacts with dragon scale mail*/
	if ((o_ptr->art_num) && (o_ptr->art_num < z_info->art_norm_max))
	{
		bool did_activation = TRUE;

		artifact_type *a_ptr = &a_info[o_ptr->art_num];
		char o_name[80];

		/* Get the basic name of the object */
		object_desc(o_name, sizeof(o_name), o_ptr, ODESC_BASE);

		switch (a_ptr->activation)
		{
			case ACT_ILLUMINATION:
			{
				msg_format("The %s wells with clear light...", o_name);
				light_area(damroll(2, 15), 3);
				break;
			}

			case ACT_MAGIC_MAP:
			{
				msg_format("The %s shines brightly...", o_name);
				detect(DETECT_RADIUS + 5, DETECT_MAP);
				break;
			}

			case ACT_CLAIRVOYANCE:
			{
				msg_format("The %s glows a deep green...", o_name);
				wiz_light();
				detect(DETECT_RADIUS, DETECT_DOORS_STAIRS_TRAPS);
				break;
			}

			case ACT_PROT_EVIL:
			{
				msg_format("The %s lets out a shrill wail...", o_name);
				k = 3 * p_ptr->lev;
				(void)inc_timed(TMD_PROTEVIL, randint(25) + k, TRUE);
				break;
			}

			case ACT_DISP_EVIL:
			{
				msg_format("The %s floods the area with goodness...", o_name);
				dispel_evil(p_ptr->lev * 5);
				break;
			}

			case ACT_HASTE2:
			{
				msg_format("The %s glows brightly...", o_name);
				if (!p_ptr->timed[TMD_FAST])
				{
					(void)set_timed(TMD_FAST, randint(75) + 75, TRUE);
				}
				else
				{
					(void)inc_timed(TMD_FAST, 5, FALSE);
				}
				break;
			}

			case ACT_FIRE3:
			{
				msg_format("The %s glows deep red...", o_name);
				fire_ball(GF_FIRE, dir, 120, 3);
				break;
			}

			case ACT_FROST5:
			{
				msg_format("The %s glows bright white...", o_name);
				fire_ball(GF_COLD, dir, 200, 3);
				break;
			}

			case ACT_ELEC2:
			{
				msg_format("The %s glows deep blue...", o_name);
				fire_ball(GF_ELEC, dir, 250, 3);
				break;
			}

			case ACT_BIZZARE:
			{
				msg_format("The %s glows intensely black...", o_name);
				ring_of_power(dir);
				break;
			}


			case ACT_STAR_BALL:
			{
				msg_format("Your %s is surrounded by lightning...", o_name);
				for (i = 0; i < 8; i++) fire_ball(GF_ELEC, ddd[i], 150, 3);
				break;
			}

			case ACT_RAGE_BLESS_RESIST:
			{
				int act_time = randint(50) + 50;
				msg_format("Your %s glows many colours...", o_name);
				if (game_mode == GAME_NPPMORIA)
				{
					if (!p_ptr->timed[TMD_SHERO])
					{
						p_ptr->mhp +=15;
						p_ptr->chp +=15;
					}
				}
				(void)hp_player(30);
				(void)clear_timed(TMD_AFRAID, TRUE);
				(void)inc_timed(TMD_SHERO, randint(50) + 50, TRUE);
				(void)inc_timed(TMD_BLESSED, randint(50) + 50, TRUE);
				(void)inc_timed(TMD_OPP_ACID, act_time, TRUE);
				(void)inc_timed(TMD_OPP_ELEC, act_time, TRUE);
				(void)inc_timed(TMD_OPP_FIRE, act_time, TRUE);
				(void)inc_timed(TMD_OPP_COLD, act_time, TRUE);
				(void)inc_timed(TMD_OPP_POIS, act_time, TRUE);
				break;
			}

			case ACT_HEAL2:
			{
				msg_format("Your %s glows a bright white...", o_name);
				msg_print("You feel much better...");
				(void)hp_player(1000);
				(void)set_cut(0);
				break;
			}

			case ACT_PHASE:
			{
				msg_format("Your %s twists space around you...", o_name);
				teleport_player(10, FALSE);
				break;
			}

			case ACT_BANISHMENT:
			{
				msg_format("Your %s glows deep blue...", o_name);
				if (!banishment()) return FALSE;
				break;
			}

			case ACT_TRAP_DOOR_DEST:
			{
				msg_format("Your %s glows bright red...", o_name);
				destroy_doors_touch();
				break;
			}

			case ACT_DETECT:
			{
				msg_format("Your %s glows bright white...", o_name);
				msg_print("An image forms in your mind...");
				(void)detect(DETECT_RADIUS, DETECT_ALL);
				break;
			}

			case ACT_HEAL1:
			{
				msg_format("Your %s glows deep blue...", o_name);
				msg_print("You feel a warm tingling inside...");
				(void)hp_player(500);
				(void)set_cut(0);
				break;
			}

			case ACT_RESIST:
			{
				int act_time = randint(20) + 20;
				msg_format("Your %s glows many colours...", o_name);
				(void)inc_timed(TMD_OPP_ACID, act_time, TRUE);
				(void)inc_timed(TMD_OPP_ELEC, act_time, TRUE);
				(void)inc_timed(TMD_OPP_FIRE, act_time, TRUE);
				(void)inc_timed(TMD_OPP_COLD, act_time, TRUE);
				(void)inc_timed(TMD_OPP_POIS, act_time, TRUE);
				break;
			}

			case ACT_SLEEP:
			{
				msg_format("Your %s glows deep blue...", o_name);
				sleep_monsters_touch();
				break;
			}

			case ACT_RECHARGE1:
			{
				msg_format("Your %s glows bright yellow...", o_name);
				if (!recharge(60, FALSE, 50)) return FALSE;
				break;
			}

			case ACT_TELEPORT:
			{
				msg_format("Your %s twists space around you...", o_name);
				teleport_player(100, FALSE);
				break;
			}

			case ACT_RESTORE_LIFE:
			{
				msg_format("Your %s glows a deep red...", o_name);
				restore_level();
				break;
			}

			case ACT_MISSILE:
			{
				msg_format("Your %s glows extremely brightly...", o_name);
				fire_bolt(GF_MISSILE, dir, damroll(2, 6));
				break;
			}

			case ACT_FIRE1:
			{
				msg_format("Your %s is covered in fire...", o_name);
				fire_bolt(GF_FIRE, dir, damroll(9, 8));
				break;
			}

			case ACT_FROST1:
			{
				msg_format("Your %s is covered in frost...", o_name);
				fire_bolt(GF_COLD, dir, damroll(6, 8));
				break;
			}

			case ACT_LIGHTNING_BOLT:
			{
				msg_format("Your %s is covered in sparks...", o_name);
				fire_bolt(GF_ELEC, dir, damroll(4, 8));
				break;
			}

			case ACT_ACID1:
			{
				msg_format("Your %s is covered in acid...", o_name);
				fire_bolt(GF_ACID, dir, damroll(5, 8));
				break;
			}

			case ACT_ARROW:
			{
				msg_format("Your %s grows magical spikes...", o_name);
				fire_bolt(GF_ARROW, dir, 150);
				break;
			}

			case ACT_HASTE1:
			{
				msg_format("Your %s glows bright green...", o_name);
				if (!p_ptr->timed[TMD_FAST])
				{
					(void)set_timed(TMD_FAST, randint(20) + 20, TRUE);
				}
				else
				{
					(void)inc_timed(TMD_FAST, 5, FALSE);
				}
				break;
			}

			case ACT_REM_FEAR_POIS:
			{
				msg_format("Your %s glows deep blue...", o_name);
				(void)clear_timed(TMD_AFRAID, TRUE);
				(void)clear_timed(TMD_POISONED, TRUE);
				break;
			}

			case ACT_STINKING_CLOUD:
			{
				msg_format("Your %s throbs deep green...", o_name);
				fire_ball(GF_POIS, dir, 12, 3);
				break;
			}

			case ACT_FROST2:
			{
				msg_format("Your %s is covered in frost...", o_name);
				fire_ball(GF_COLD, dir, 48, 2);
				break;
			}

			case ACT_FROST3:
			{
				msg_format("Your %s glows a intense blue...", o_name);
				fire_ball(GF_COLD, dir, 100, 2);
				break;
			}

			case ACT_FROST4:
			{
				msg_format("Your %s glows a pale blue...", o_name);
				fire_bolt(GF_COLD, dir, damroll(12, 8));
				break;
			}

			case ACT_FIRE2:
			{
				msg_format("Your %s rages in fire...", o_name);
				fire_ball(GF_FIRE, dir, 72, 2);
				break;
			}

			case ACT_DRAIN_LIFE2:
			{
				msg_format("Your %s glows black...", o_name);
				drain_life(dir, 120);
				break;
			}

			case ACT_STONE_TO_MUD:
			{
				msg_format("Your %s pulsates...", o_name);
				(void)wall_to_mud(dir, 20 + randint(30));
				break;
			}

			case ACT_MASS_BANISHMENT:
			{
				msg_format("Your %s lets out a long, shrill note...", o_name);
				(void)mass_banishment();
				break;
			}

			case ACT_CURE_WOUNDS:
			{
				msg_format("Your %s radiates deep purple...", o_name);
				hp_player(damroll(6, 10));
				(void)set_cut((p_ptr->timed[TMD_CUT] / 2) - 50);
				break;
			}

			case ACT_TELE_AWAY:
			{
				msg_format("Your %s glows deep red...", o_name);
				teleport_monster(dir);
				break;
			}

			case ACT_WOR:
			{
				msg_format("Your %s glows soft white...", o_name);
				if (!set_recall()) return (FALSE);
				break;
			}

			case ACT_CONFUSE:
			{
				msg_format("Your %s glows in scintillating colours...", o_name);
				confuse_monster(dir, (p_ptr->lev * 2 / 3));
				break;
			}

			case ACT_IDENTIFY:
			{
				msg_format("Your %s glows yellow...", o_name);
				if (!identify_fully()) return FALSE;
				break;
			}

			case ACT_PROBE:
			{
				msg_format("Your %s glows brightly...", o_name);
				probing();
				break;
			}

			case ACT_DRAIN_LIFE1:
			{
				msg_format("Your %s glows white...", o_name);
				drain_life(dir, 90);
				break;
			}

			case ACT_FIREBRAND:
			{
				msg_format("Your %s glows deep red...", o_name);
				if (!brand_bolts(TRUE)) return FALSE;
				break;
			}

			case ACT_STARLIGHT:
			{
				msg_format("Your %s glows with the light of a thousand stars...", o_name);
				for (k = 0; k < 8; k++) strong_light_line(ddd[k]);
				break;
			}

			case ACT_MANA_BOLT:
			{
				msg_format("Your %s glows white...", o_name);
				fire_bolt(GF_MANA, dir, damroll(12, 8));
				break;
			}

			case ACT_BERSERKER:
			{
				msg_format("Your %s glows in anger...", o_name);
				if (game_mode == GAME_NPPMORIA)
				{
					if (!p_ptr->timed[TMD_SHERO])
					{
						p_ptr->mhp +=15;
						p_ptr->chp +=15;
					}
				}
				inc_timed(TMD_SHERO, randint(50) + 50, TRUE);
				break;
			}

			case ACT_RES_ACID:
			{
				msg_format("Your %s glows light gray...", o_name);
				(void)inc_timed(TMD_OPP_ACID, randint(20) + 20, TRUE);
				break;
			}

			case ACT_RES_ELEC:
			{
				msg_format("Your %s glows light blue...", o_name);
				(void)inc_timed(TMD_OPP_ELEC, randint(20) + 20, TRUE);
				break;
			}

			case ACT_RES_FIRE:
			{
				msg_format("Your %s glows light red...", o_name);
				(void)inc_timed(TMD_OPP_FIRE, randint(20) + 20, TRUE);
				break;
			}

			case ACT_RES_COLD:
			{
				msg_format("Your %s glows bright white...", o_name);
				(void)inc_timed(TMD_OPP_COLD, randint(20) + 20, TRUE);
				break;
			}

			case ACT_RES_POIS:
			{
				msg_format("Your %s glows light green...", o_name);
				(void)inc_timed(TMD_OPP_POIS, randint(20) + 20, TRUE);
				break;
			}

			default:
			{
				if ((a_ptr->tval != TV_DRAG_ARMOR) &&
					(a_ptr->tval != TV_DRAG_SHIELD)) return (FALSE);
				else did_activation = FALSE;

				break;
			}
		}

		if (did_activation)
		{

			/* Set the recharge time */
			if (a_ptr->randtime)
				o_ptr->timeout = a_ptr->time + (byte)randint(a_ptr->randtime);
			else o_ptr->timeout = a_ptr->time;

			/* Window stuff */
			p_ptr->redraw |= (PR_INVEN | PR_EQUIP | PR_ITEMLIST);

			/* Done */
			return FALSE;
		}
	}

	/* Hack -- Dragon Scale Mail can be activated as well */
	if ((o_ptr->tval == TV_DRAG_ARMOR) ||
		(o_ptr->tval == TV_DRAG_SHIELD))
	{

		u16b value = o_ptr->sval;
		int time_2 = randint(10) + 10;

		if (o_ptr->tval == TV_DRAG_ARMOR) value *= 2;

		/* Branch on the sub-type */
		switch (o_ptr->ego_num)
		{
			case EGO_DRAGON_BLUE:
			{
				value *= 50;

				sound(MSG_BR_ELEC);
				msg_print("You breathe lightning.");
				fire_arc(GF_ELEC, dir, value, 0, 30);

				inc_timed(TMD_OPP_ELEC, time_2, TRUE);

				o_ptr->timeout = rand_int(value) + (value);
				break;
			}

			case EGO_DRAGON_WHITE:
			{
				value *= 50;

				sound(MSG_BR_FROST);
				msg_print("You breathe frost.");
				fire_arc(GF_COLD, dir, value, 0, 30);

				inc_timed(TMD_OPP_COLD, time_2, TRUE);

				o_ptr->timeout = rand_int(value) + (value);
				break;
			}

			case EGO_DRAGON_BLACK:
			{
				value *= 50;

				sound(MSG_BR_ACID);
				msg_print("You breathe acid.");
				fire_arc(GF_ACID, dir, value, 0, 30);

				inc_timed(TMD_OPP_ACID, time_2, TRUE);

				o_ptr->timeout = rand_int(value) + value;
				break;
			}

			case EGO_DRAGON_GREEN:
			{
				value *= 50;

				sound(MSG_BR_GAS);
				msg_print("You breathe poison gas.");
				fire_arc(GF_POIS, dir, value, 0, 30);

				inc_timed(TMD_OPP_POIS, time_2, TRUE);

				o_ptr->timeout = rand_int(value) + value;
				break;
			}

			case EGO_DRAGON_RED:
			{
				value *= 50;

				sound(MSG_BR_FIRE);
				msg_print("You breathe fire.");
				fire_arc(GF_FIRE, dir, value, 0, 30);

				(void)inc_timed(TMD_OPP_FIRE, time_2, TRUE);

				o_ptr->timeout = rand_int(value) + value;
				break;
			}

			case EGO_DRAGON_MULTIHUED:
			{
				value *= 75;

				chance = rand_int(5);
				sound(((chance == 1) ? MSG_BR_ELEC :
				       ((chance == 2) ? MSG_BR_FROST :
				        ((chance == 3) ? MSG_BR_ACID :
				         ((chance == 4) ? MSG_BR_GAS : MSG_BR_FIRE)))));
				msg_format("You breathe %s.",
				           ((chance == 1) ? "lightning" :
				            ((chance == 2) ? "frost" :
				             ((chance == 3) ? "acid" :
				              ((chance == 4) ? "poison gas" : "fire")))));
				fire_arc(((chance == 1) ? GF_ELEC :
				           ((chance == 2) ? GF_COLD :
				            ((chance == 3) ? GF_ACID :
				             ((chance == 4) ? GF_POIS : GF_FIRE)))),
				          dir, value, 0, 30);

				/* Increase the bonus to resistances */
				time_2 = randint(20) + 20;
				inc_timed(TMD_OPP_ELEC, time_2, TRUE);
				inc_timed(TMD_OPP_COLD, time_2, TRUE);
				inc_timed(TMD_OPP_ACID, time_2, TRUE);
				inc_timed(TMD_OPP_POIS, time_2, TRUE);
				inc_timed(TMD_OPP_FIRE, time_2, TRUE);

				o_ptr->timeout = rand_int(value * 3 / 4) + (value * 3 / 4);
				break;
			}

			case EGO_DRAGON_BRONZE:
			{
				value *= 50;

				sound(MSG_BR_CONF);
				msg_print("You breathe confusion.");
				fire_arc(GF_CONFUSION, dir, value, 0, 30);
				o_ptr->timeout = rand_int(value) + value;
				break;
			}

			case EGO_DRAGON_GOLD:
			{
				value *= 50;

				sound(MSG_BR_SOUND);
				msg_print("You breathe sound.");
				fire_arc(GF_SOUND, dir, value, 0, 30);
				o_ptr->timeout = rand_int(value) + value;
				break;
			}

			case EGO_DRAGON_CHAOS:
			{
				value *= 60;

				chance = rand_int(2);

				sound(((chance == 1 ? MSG_BR_CHAOS : MSG_BR_DISENCHANT)));

				msg_format("You breathe %s.",
				           ((chance == 1 ? "chaos" : "disenchantment")));
				fire_arc((chance == 1 ? GF_CHAOS : GF_DISENCHANT),
				          dir, value, 0, 30);
				o_ptr->timeout = rand_int(value) + value;
				break;
			}

			case EGO_DRAGON_LAW:
			{
				value *= 60;

				chance = rand_int(2);
				sound(((chance == 1 ? MSG_BR_SOUND : MSG_BR_SHARDS)));
				msg_format("You breathe %s.",
				           ((chance == 1 ? "sound" : "shards")));
				fire_arc((chance == 1 ? GF_SOUND : GF_SHARD),
				          dir, value, 0, 30);
				o_ptr->timeout = rand_int(value) + value;
				break;
			}

			case EGO_DRAGON_BALANCE:
			{
				value *= 75;

				chance = rand_int(4);

				sound(((chance == 1) ? MSG_BR_CHAOS :
				       ((chance == 2) ? MSG_BR_DISENCHANT :
				        ((chance == 3) ? MSG_BR_SOUND : MSG_BR_SHARDS))));

				msg_format("You breathe %s.",
				           ((chance == 1) ? "chaos" :
				            ((chance == 2) ? "disenchantment" :
				             ((chance == 3) ? "sound" : "shards"))));

				fire_arc(((chance == 1) ? GF_CHAOS :
				           ((chance == 2) ? GF_DISENCHANT :
				            ((chance == 3) ? GF_SOUND : GF_SHARD))),
				          dir, value, 0, 30);

				o_ptr->timeout = rand_int(value) + value;
				break;
			}

			case EGO_DRAGON_PSEUDO:
			{
				value *= 65;

				chance = rand_int(2);

				sound(((chance == 0 ? MSG_BR_LIGHT : MSG_BR_DARK)));

				msg_format("You breathe %s.",
				           ((chance == 0 ? "light" : "darkness")));

				fire_arc((chance == 0 ? GF_LIGHT : GF_DARK), dir, value, 0, 30);

				o_ptr->timeout = rand_int(value) + value;
				break;
			}

			case EGO_DRAGON_POWER:
			{
				value *= 100;

				sound(MSG_BR_ELEMENTS);
				msg_print("You breathe the elements.");
				fire_arc(GF_MISSILE, dir, value, 0, 30);
				o_ptr->timeout = rand_int(value) + value;
				break;
			}
		}

		/* Window stuff */
		p_ptr->redraw |= (PR_INVEN | PR_EQUIP | PR_ITEMLIST);

		/* Success */
		return FALSE;
	}

	/* Hack -- some Rings can be activated for double resist and element ball */
	if (o_ptr->tval == TV_RING)
	{
		/* Branch on the sub-type */
		switch (o_ptr->sval)
		{
			case SV_RING_ACID:
			{
				fire_ball(GF_ACID, dir, 70, 2);
				inc_timed(TMD_OPP_ACID, randint(20) + 20, TRUE);
				o_ptr->timeout = rand_int(50) + 50;
				break;
			}

			case SV_RING_FLAMES:
			{
				fire_ball(GF_FIRE, dir, 80, 2);
				inc_timed(TMD_OPP_FIRE, randint(20) + 20, TRUE);
				o_ptr->timeout = rand_int(50) + 50;
				break;
			}

			case SV_RING_ICE:
			{
				fire_ball(GF_COLD, dir, 75, 2);
				inc_timed(TMD_OPP_COLD, randint(20) + 20, TRUE);
				o_ptr->timeout = rand_int(50) + 50;
				break;
			}

			case SV_RING_LIGHTNING:
			{
				fire_ball(GF_ELEC, dir, 85, 2);
				inc_timed(TMD_OPP_ELEC, randint(20) + 20, TRUE);
				o_ptr->timeout = rand_int(50) + 50;
				break;
			}
		}

		/* Redraw stuff */
		p_ptr->redraw |= (PR_EQUIP);

		/* Success */
		return FALSE;
	}

	/* Mistake */
	msg_print("Oops.  That object cannot be activated.");

	/* Not used up */
	return (FALSE);
}


/*
 * Use an object
 */
static bool use_object(object_type *o_ptr, bool *ident, bool aware, int dir)
{
	bool used;

	/* Analyze the object */
	switch (o_ptr->tval)
	{
		case TV_FOOD:
		{
			used = eat_food(o_ptr, ident);
			break;
		}

		case TV_POTION:
		{
			used = quaff_potion(o_ptr, ident);
			break;
		}

		case TV_SCROLL:
		{
			used = read_scroll(o_ptr, ident);
			break;
		}

		case TV_STAFF:
		{
			used = use_staff(o_ptr, ident);
			break;
		}

		case TV_WAND:
		{
			used = aim_wand(o_ptr, ident, dir);
			break;
		}

		case TV_ROD:
		{
			used = zap_rod(o_ptr, ident, dir);

			break;
		}

		default:
		{
			used = activate_object(o_ptr, dir);
			break;
		}
	}

	return (used);
}


/*** Using items the traditional way ***/


/*
 * Use an object.
 *
 */
void do_cmd_use(cmd_code code, cmd_arg args[])
{
	int item = args[0].item;
	object_type *o_ptr = object_from_item_idx(item);
	bool ident = FALSE;
	bool used = FALSE;
	bool was_aware = object_flavor_is_aware(o_ptr);
	int dir = 5;
	int px = p_ptr->px, py = p_ptr->py;
	int snd;
	use_type use;
	int items_allowed = 0;

	/* Determine how this item is used. */
	if (obj_is_rod(o_ptr))
	{
		object_kind *k_ptr = &k_info[o_ptr->k_idx];

		if (o_ptr->timeout > (o_ptr->pval - k_ptr->pval))
		{
			if (o_ptr->number == 1) msg_print("The rod is still charging");
			else msg_print("The rods are all still charging");

			return;
		}

		use = USE_TIMEOUT;
		snd = MSG_ZAP_ROD;
		items_allowed = USE_INVEN | USE_FLOOR;
	}
	else if (obj_is_wand(o_ptr))
	{
		if (!obj_has_charges(o_ptr))
		{
			msg_print("That wand has no charges.");
			o_ptr->ident |= (IDENT_EMPTY);
			p_ptr->notice |= (PN_COMBINE | PN_REORDER);
			p_ptr->window |= (PW_INVEN);
			return;
		}

		use = USE_CHARGE;
		snd = MSG_ZAP_ROD;
		items_allowed = USE_INVEN | USE_FLOOR;
	}
	else if (obj_is_staff(o_ptr))
	{
		if (!obj_has_charges(o_ptr))
		{
			msg_print("That staff has no charges.");
			o_ptr->ident |= (IDENT_EMPTY);
			p_ptr->notice |= (PN_COMBINE | PN_REORDER);
			p_ptr->window |= (PW_INVEN);
			return;
		}

		use = USE_CHARGE;
		snd = MSG_ZAP_ROD;
		items_allowed = USE_INVEN | USE_FLOOR;
	}
	else if (obj_is_food(o_ptr))
	{
		use = USE_SINGLE;
		snd = MSG_EAT;
		items_allowed = USE_INVEN | USE_FLOOR;
	}
	else if (obj_is_potion(o_ptr))
	{
		use = USE_SINGLE;
		snd = MSG_QUAFF;
		items_allowed = USE_INVEN | USE_FLOOR;
	}
	else if (obj_is_scroll(o_ptr))
	{
		/* Check player can use scroll */
		if (!player_can_read())
			return;

		use = USE_SINGLE;
		snd = MSG_GENERIC;
		items_allowed = USE_INVEN | USE_FLOOR;
	}
	else if (obj_is_activatable(o_ptr))
	{
		if (!obj_can_activate(o_ptr))
		{
			msg_print("That item is still charging.");
			return;
		}

		use = USE_TIMEOUT;
		snd = MSG_ACT_ARTIFACT;
		items_allowed = USE_EQUIP;
	}
	else
	{
		msg_print("The item cannot be used at the moment");
	}

	/* Check if item is within player's reach. */
	if (items_allowed == 0 || !item_is_available(item, NULL, items_allowed))
	{
		msg_print("You cannot use that item from its current location.");
		return;
	}

	/* track the object used */
	track_object(item);

	/* If the item requires a direction, get one (allow canceling) */
	if (obj_needs_aim(o_ptr))
		dir = args[1].direction;

	/* Check for use if necessary, and execute the effect */
	if ((use != USE_CHARGE && use != USE_TIMEOUT) ||
	    check_devices(o_ptr))
	{
		/* Special message for artifacts */
		if (!artifact_p(o_ptr))
		{
			/* Make a noise! */
			sound(snd);
		}

		/* mark the item (the place in the inventory might shift) */
		o_ptr->obj_in_use = TRUE;

		/* Do effect */
		used = use_object(o_ptr, &ident, was_aware, dir);

		/* make sure we still have the right item if the inventory was moved around */
		if (find_object_in_use(&item))
		{
			o_ptr = object_from_item_idx(item);
		}

		/* Clear the item mark */
		o_ptr->obj_in_use = FALSE;

		/* Quit if the item wasn't used and no knowledge was gained */
		if (!used && (was_aware || !ident)) return;

	}

	/* Tried the object */
	object_tried(o_ptr);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP);

	/* Handle first-time use */
	if (ident)
	{
		/* Successfully determined the object function */
		if (!object_aware_p(o_ptr))
		{
			/* Object level */
			int lev = k_info[o_ptr->k_idx].k_level;

			object_aware(o_ptr);
			gain_exp((lev + (p_ptr->lev / 2)) / p_ptr->lev);
			apply_autoinscription(o_ptr);
		}
	}

	/* If the item is a null pointer or has been wiped, be done now */
	if (!o_ptr || o_ptr->k_idx <= 1) return;

	/* Use the turn */
	p_ptr->p_energy_use = BASE_ENERGY_MOVE;

	/* Mark as tried and redisplay */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER | PN_SORT_QUIVER);
	p_ptr->redraw |= (PR_INVEN | PR_EQUIP | PR_OBJECT | PR_ITEMLIST);

	/* If there are no more of the item left, then we're done. */
	if (!o_ptr->number) return;

	/* Chargeables act differently to single-used items when not used up */
	if (used && (use == USE_CHARGE))
	{

		/* Use a single charge */
		o_ptr->pval--;

		/* Describe charges */
		if (item >= 0)
			inven_item_charges(item);
		else
			floor_item_charges(0 - item);

	}
	else if (used && use == USE_SINGLE)
	{

		/* Destroy a potion in the pack */
		if (item >= 0)
		{
			inven_item_increase(item, -1);
			inven_item_describe(item);
			inven_item_optimize(item);
		}

		/* Destroy a potion on the floor */
		else
		{
			floor_item_increase(0 - item, -1);
			floor_item_describe(0 - item);
			floor_item_optimize(0 - item);
		}
	}

	/* Hack to make Glyph of Warding work properly */
	if (cave_x_idx[py][px] == FEAT_GLYPH)
	{
		/* Shift any objects to further away */
		for (o_ptr = get_first_object(py, px); o_ptr; o_ptr = get_next_object(o_ptr))
		{
			drop_near(o_ptr, 0, py, px);
		}

		/* Delete the "moved" objects from their original position */
		delete_object(py, px);
	}
}


/*** Handling bits ***/


/* Item "action" type */
typedef struct
{
	void (*action)(object_type *, int);
	cmd_code command;
	const char *desc;

	const char *prompt;
	const char *noop;

	bool (*filter)(const object_type *o_ptr);
	int mode;
	bool (*prereq)(void);
} item_act_t;


/* All possible item actions */
static item_act_t item_actions[] =
{
	/* Not setting IS_HARMLESS for this one because it could cause a true
	 * dangerous command to not be prompted, later.
	 */

	/* ACTION_UNINSCRIBE */
	{ NULL, CMD_UNINSCRIBE, "uninscribe",
	  "Un-inscribe which item? ", "You have nothing to un-inscribe.",
	  obj_has_inscrip, (USE_EQUIP | USE_INVEN | USE_FLOOR | USE_QUIVER), NULL },

	 /* ACTION_INSCRIBE */
	{ obj_inscribe, CMD_NULL, "inscribe",
	  "Inscribe which item? ", "You have nothing to inscribe.",
	  NULL, (USE_EQUIP | USE_INVEN | USE_FLOOR | IS_HARMLESS | USE_QUIVER), NULL },

	  /* ACTION_EXAMINE */
	{ obj_examine, CMD_NULL, "examine",
	  "Examine which item? ", "You have nothing to examine.",
	  NULL, (USE_EQUIP | USE_INVEN | USE_FLOOR | IS_HARMLESS | USE_QUIVER), NULL },

	/* ACTION_TAKEOFF */
	{ NULL, CMD_TAKEOFF, "takeoff",
	  "Take off which item? ", "You are not wearing anything you can take off.",
	  obj_can_takeoff, (USE_EQUIP | USE_QUIVER), NULL },

	  /* ACTION_WIELD */
	{ obj_wield, CMD_WIELD, "wield",
	  "Wear/Wield which item? ", "You have nothing you can wear or wield.",
	  obj_can_wear, (USE_INVEN | USE_FLOOR), NULL },

	  /* ACTION_DROP */
	{ obj_drop, CMD_NULL, "drop",
	  "Drop which item? ", "You have nothing to drop.",
	  NULL, (USE_EQUIP | USE_INVEN | USE_QUIVER), NULL },

	  /* ACTION_BROWSE */
	/*** Spellbooks ***/
	{ obj_browse, CMD_NULL, "browse",
	  "Browse which book? ", "You have no books that you can read.",
	  obj_can_browse, (USE_INVEN | USE_FLOOR | IS_HARMLESS), NULL },

	  /* ACTION_STUDY */
	{ obj_study, CMD_NULL, "study",
	  "Study which book? ", "You have no books that you can study.",
	  obj_can_study, (USE_INVEN | USE_FLOOR), player_can_study },

	  /* ACTION_CAST */
	{ obj_cast, CMD_NULL, "cast",
	  "Use which book? ", "You have no books that you can cast from.",
	  obj_can_cast, (USE_INVEN | USE_FLOOR), player_can_cast },

	/* ACTION_USE_STAFF */
	{ NULL, CMD_USE_STAFF, "use",
	  "Use which staff? ", "You have no staff to use.",
	  obj_is_staff, (USE_INVEN | USE_FLOOR | SHOW_FAIL), NULL },

	  /* ACTION_AIM_WAND */
	{ NULL, CMD_USE_WAND, "aim",
      "Aim which wand? ", "You have no wand to aim.",
	  obj_is_wand, (USE_INVEN | USE_FLOOR | SHOW_FAIL), NULL },

	  /* ACTION_ZAP_ROD */
	{ NULL, CMD_USE_ROD, "zap",
      "Zap which rod? ", "You have no charged rods to zap.",
	  obj_is_rod, (USE_INVEN | USE_FLOOR | SHOW_FAIL), NULL },

	  /* ACTION_ACTIVATE */
	{ NULL, CMD_ACTIVATE, "activate",
      "Activate which item? ", "You have nothing to activate.",
	  obj_is_activatable, (USE_EQUIP | SHOW_FAIL), NULL },

	  /* ACTION_EAT_FOOD */
	{ NULL, CMD_EAT, "eat",
      "Eat which item? ", "You have nothing to eat.",
	  obj_is_food, (USE_INVEN | USE_FLOOR), NULL },

	  /* ACTION_QUAFF_POTION */
	{ NULL, CMD_QUAFF, "quaff",
      "Quaff which potion? ", "You have no potions to quaff.",
	  obj_is_potion, (USE_INVEN | USE_FLOOR), NULL },

	  /* ACTION_READ_SCROLL */
	{ NULL, CMD_READ_SCROLL, "read",
      "Read which scroll? ", "You have no scrolls to read.",
	  obj_is_scroll, (USE_INVEN | USE_FLOOR), player_can_read },

	  /* ACTION_REFILL */
	{ NULL, CMD_REFILL, "refill",
      "Refuel with what fuel source? ", "You have nothing to refuel with.",
	  obj_can_refill, (USE_INVEN | USE_FLOOR), NULL },
};


/* List matching up to item_actions[] */
typedef enum
{
	ACTION_UNINSCRIBE = 0,
	ACTION_INSCRIBE,
	ACTION_EXAMINE,
	ACTION_TAKEOFF,
	ACTION_WIELD,
	ACTION_DROP,

	ACTION_BROWSE,
	ACTION_STUDY,
	ACTION_CAST,

	ACTION_USE_STAFF,
	ACTION_AIM_WAND,
	ACTION_ZAP_ROD,
	ACTION_ACTIVATE,
	ACTION_EAT_FOOD,
	ACTION_QUAFF_POTION,
	ACTION_READ_SCROLL,
	ACTION_REFILL
} item_act;

static bool trap_related_object(object_type *o_ptr)
{
	if ((o_ptr->tval == TV_WAND) && (object_aware_p(o_ptr)))
	{
		if ((o_ptr->sval == SV_WAND_DISARMING) ||
			(o_ptr->sval == SV_WAND_TRAP_DOOR_DEST)) return (TRUE);
	}
	else if ((o_ptr->tval == TV_ROD) && (object_aware_p(o_ptr)))
	{
		if (o_ptr->sval == SV_ROD_DISARMING) return (TRUE);
	}

	return (FALSE);
}

/*** Old-style noun-verb functions ***/


/*
 * Generic "do item action" function
 */
static void do_item(item_act act)
{
	int item;
	object_type *o_ptr;
	bool cmd_needs_aim = FALSE;

	cptr q, s;

	if (item_actions[act].prereq)
	{
		if (!item_actions[act].prereq())
			return;
	}

	/* Don't allow activation of swap weapons */
	if (adult_swap_weapons)
	{
		if (item_actions[act].command == CMD_ACTIVATE)  item_tester_swap = TRUE;
	}

	/* Get item */
	q = item_actions[act].prompt;
	s = item_actions[act].noop;
	item_tester_hook = item_actions[act].filter;

	if (!get_item(&item, q, s, item_actions[act].mode)) return;

	/* Get the item */
	o_ptr = object_from_item_idx(item);

	/* These commands need an aim */
	if (item_actions[act].command == CMD_QUAFF ||
		item_actions[act].command == CMD_ACTIVATE ||
		item_actions[act].command == CMD_USE_WAND ||
		item_actions[act].command == CMD_USE_ROD ||
		item_actions[act].command == CMD_USE_STAFF ||
		item_actions[act].command == CMD_READ_SCROLL)
	{
		cmd_needs_aim = TRUE;
	}

	/* Execute the item command */
	if (item_actions[act].action != NULL)
		item_actions[act].action(o_ptr, item);
	else if (cmd_needs_aim && obj_needs_aim(o_ptr))
	{
		int dir;
		bool trap_related = trap_related_object(o_ptr);
		if (!get_aim_dir(&dir, trap_related))
			return;

		cmd_insert(item_actions[act].command, item, dir);
	}
	else
		cmd_insert(item_actions[act].command, item);

}


/* Wrappers */
void textui_cmd_uninscribe(void) { do_item(ACTION_UNINSCRIBE); }
void textui_cmd_inscribe(void) { do_item(ACTION_INSCRIBE); }
void do_cmd_observe(void) { do_item(ACTION_EXAMINE); }
void textui_cmd_takeoff(void) { do_item(ACTION_TAKEOFF); }
void textui_cmd_wield(void) { do_item(ACTION_WIELD); }
void textui_cmd_drop(void) { do_item(ACTION_DROP); }
void do_cmd_browse(void) { do_item(ACTION_BROWSE); }
void textui_cmd_study(void) { do_item(ACTION_STUDY); }
void textui_cmd_cast(void) { do_item(ACTION_CAST); }
void textui_cmd_pray(void) { do_item(ACTION_CAST); }
void textui_cmd_use_staff(void) { do_item(ACTION_USE_STAFF); }
void textui_cmd_aim_wand(void) { do_item(ACTION_AIM_WAND); }
void textui_cmd_zap_rod(void) {do_item(ACTION_ZAP_ROD);}
void textui_cmd_activate(void) { do_item(ACTION_ACTIVATE); }
void textui_cmd_eat_food(void) { do_item(ACTION_EAT_FOOD); }
void textui_cmd_quaff_potion(void) { do_item(ACTION_QUAFF_POTION); }
void textui_cmd_read_scroll(void) { do_item(ACTION_READ_SCROLL); }
void textui_cmd_refill(void) { do_item(ACTION_REFILL); }
