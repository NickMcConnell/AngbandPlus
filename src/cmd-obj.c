/**
 * \file cmd-obj.c
 * \brief Handle objects in various ways
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * Copyright (c) 2007-9 Andi Sidwell, Chris Carr, Ed Graham, Erik Osheim
 *
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
#include "cave.h"
#include "cmd-core.h"
#include "cmds.h"
#include "effects.h"
#include "game-input.h"
#include "init.h"
#include "obj-desc.h"
#include "obj-gear.h"
#include "obj-ignore.h"
#include "obj-info.h"
#include "obj-knowledge.h"
#include "obj-make.h"
#include "obj-pile.h"
#include "obj-tval.h"
#include "obj-util.h"
#include "player-attack.h"
#include "player-calcs.h"
#include "player-spell.h"
#include "player-timed.h"
#include "player-util.h"
#include "target.h"
#include "trap.h"

static bool check_shapechanged(void);

/**
 * ------------------------------------------------------------------------
 * Utility bits and bobs
 * ------------------------------------------------------------------------
 */

/**
 * Check to see if the player can use a rod/wand/device/activatable object.
 */
static int check_devices(struct object *obj)
{
	int fail;
	const char *action;
	const char *what = NULL;
	bool activated = false;

	/* Get the right string */
	if (tval_is_rod(obj)) {
		action = "zap the gadget";
	} else if (tval_is_wand(obj)) {
		action = "use the wand";
		what = "wand";
	} else if (tval_is_device(obj)) {
		action = "use the device";
		what = "device";
	} else {
		action = "activate it";
		activated = true;
	}

	/* Figure out how hard the item is to use */
	fail = get_use_device_chance(obj);

	/* Roll for usage */
	if (randint1(1000) < fail) {
		event_signal(EVENT_INPUT_FLUSH);
		msg("You failed to %s properly.", action);
		return false;
	}

	/* Notice empty devices */
	if (what && obj->pval <= 0) {
		event_signal(EVENT_INPUT_FLUSH);
		msg("The %s has no charges left.", what);
		return false;
	}

	/* Notice activations */
	if (activated) {
		if (obj->effect)
			obj->known->effect = obj->effect;
		else if (obj->activation)
			obj->known->activation = obj->activation;
	}

	return true;
}


/**
 * Return the chance of an effect beaming, given a tval.
 */
static int beam_chance(int tval)
{
	switch (tval)
	{
		case TV_WAND: return 20;
		case TV_GADGET:  return 10;
	}

	return 0;
}


/**
 * Print an artifact activation message.
 */
static void activation_message(struct object *obj)
{
	const char *message;

	/* See if we have a message, then print it */
	if (!obj->activation) return;
	if (!obj->activation->message) return;
	if (obj->artifact && obj->artifact->alt_msg) {
		message = obj->artifact->alt_msg;
	} else {
		message = obj->activation->message;
	}
	print_custom_message(obj, message, MSG_GENERIC);
}



/**
 * ------------------------------------------------------------------------
 * Inscriptions
 * ------------------------------------------------------------------------
 */

/**
 * Remove inscription
 */
void do_cmd_uninscribe(struct command *cmd)
{
	struct object *obj;

	if (check_shapechanged())
		return;

	/* Get arguments */
	if (cmd_get_item(cmd, "item", &obj,
			/* Prompt */ "Uninscribe which item?",
			/* Error  */ "You have nothing you can uninscribe.",
			/* Filter */ obj_has_inscrip,
			/* Choice */ USE_EQUIP | USE_INVEN | USE_QUIVER | USE_FLOOR) != CMD_OK)
		return;

	obj->note = 0;
	msg("Inscription removed.");

	player->upkeep->notice |= (PN_COMBINE | PN_IGNORE);
	player->upkeep->redraw |= (PR_INVEN | PR_EQUIP);
}

/**
 * Add inscription
 */
void do_cmd_inscribe(struct command *cmd)
{
	struct object *obj;
	const char *str;

	char prompt[1024];
	char o_name[80];

	if (check_shapechanged())
		return;

	/* Get arguments */
	if (cmd_get_item(cmd, "item", &obj,
			/* Prompt */ "Inscribe which item?",
			/* Error  */ "You have nothing to inscribe.",
			/* Filter */ NULL,
			/* Choice */ USE_EQUIP | USE_INVEN | USE_QUIVER | USE_FLOOR | IS_HARMLESS) != CMD_OK)
		return;

	/* Form prompt */
	object_desc(o_name, sizeof(o_name), obj, ODESC_PREFIX | ODESC_FULL);
	strnfmt(prompt, sizeof prompt, "Inscribing %s.", o_name);

	if (cmd_get_string(cmd, "inscription", &str,
			quark_str(obj->note) /* Default */,
			prompt, "Inscribe with what? ") != CMD_OK)
		return;

	obj->note = quark_add(str);
	string_free((char *)str);

	player->upkeep->notice |= (PN_COMBINE | PN_IGNORE);
	player->upkeep->redraw |= (PR_INVEN | PR_EQUIP);
}


/**
 * Autoinscribe all appropriate objects
 */
void do_cmd_autoinscribe(struct command *cmd)
{
	if (player_is_shapechanged(player)) return;

	autoinscribe_ground();
	autoinscribe_pack();

	player->upkeep->redraw |= (PR_INVEN | PR_EQUIP);
}


/**
 * ------------------------------------------------------------------------
 * Taking off/putting on
 * ------------------------------------------------------------------------
 */

/**
 * Take off an item
 */
void do_cmd_takeoff(struct command *cmd)
{
	struct object *obj;

	if (check_shapechanged())
		return;

	/* Get arguments */
	if (cmd_get_item(cmd, "item", &obj,
			/* Prompt */ "Take off or unwield which item?",
			/* Error  */ "You have nothing to take off or unwield.",
			/* Filter */ obj_can_takeoff,
			/* Choice */ USE_EQUIP) != CMD_OK)
		return;

	inven_takeoff(obj);
	combine_pack();
	pack_overflow(obj);
	player->upkeep->energy_use = z_info->move_energy / 2;
}


/**
 * Wield or wear an item
 */
void do_cmd_wield(struct command *cmd)
{
	struct object *equip_obj;
	char o_name[80];
	const char *act;

	unsigned n;

	int slot;
	struct object *obj;

	if (check_shapechanged())
		return;

	/* Get arguments */
	if (cmd_get_item(cmd, "item", &obj,
			/* Prompt */ "Wear or wield which item?",
			/* Error  */ "You have nothing to wear or wield.",
			/* Filter */ obj_can_wear,
			/* Choice */ USE_INVEN | USE_FLOOR) != CMD_OK)
		return;

	/* Get the slot the object wants to go in, and the item currently there */
	slot = wield_slot(obj);
	equip_obj = slot_object(player, slot);

	/* If the slot is open, wield and be done */
	if (!equip_obj) {
		inven_wield(obj, slot);
		return;
	}

/* This will be repurposed for cyberkit */
#ifdef undef
	/* Usually if the slot is taken we'll just replace the item in the slot,
	 * but for rings we need to ask the user which slot they actually
	 * want to replace */
	if (tval_is_ring(obj)) {
		if (cmd_get_item(cmd, "replace", &equip_obj,
						 /* Prompt */ "Replace which ring? ",
						 /* Error  */ "Error in do_cmd_wield(), please report.",
						 /* Filter */ tval_is_ring,
						 /* Choice */ USE_EQUIP) != CMD_OK)
			return;

		/* Change slot if necessary */
		slot = equipped_item_slot(player->body, equip_obj);
	}
#endif

	/* Prevent wielding into a stickied slot */
	if (!obj_can_takeoff(equip_obj)) {
		object_desc(o_name, sizeof(o_name), equip_obj, ODESC_BASE);
		msg("You cannot remove the %s you are %s.", o_name,
			equip_describe(player, slot));
		return;
	}

	/* "!t" checks for taking off */
	n = check_for_inscrip(equip_obj, "!t");
	while (n--) {
		/* Prompt */
		object_desc(o_name, sizeof(o_name), equip_obj,
					ODESC_PREFIX | ODESC_FULL);
		
		/* Forget it */
		if (!get_check(format("Really take off %s? ", o_name))) return;
	}

	/* Describe the object */
	object_desc(o_name, sizeof(o_name), equip_obj, ODESC_PREFIX | ODESC_FULL);

	/* Took off weapon */
	if (slot_type_is(slot, EQUIP_WEAPON))
		act = "You were wielding";
	/* Took off gun */
	else if (slot_type_is(slot, EQUIP_GUN))
		act = "You were holding";
	/* Took off light */
	else if (slot_type_is(slot, EQUIP_LIGHT))
		act = "You were holding";
	/* Took off something else */
	else
		act = "You were wearing";

	inven_wield(obj, slot);

	/* Message */
	msgt(MSG_WIELD, "%s %s (%c).", act, o_name, gear_to_label(equip_obj));
}

/**
 * Drop an item
 */
void do_cmd_drop(struct command *cmd)
{
	int amt;
	struct object *obj;

	if (check_shapechanged())
		return;

	/* Get arguments */
	if (cmd_get_item(cmd, "item", &obj,
			/* Prompt */ "Drop which item?",
			/* Error  */ "You have nothing to drop.",
			/* Filter */ NULL,
			/* Choice */ USE_EQUIP | USE_INVEN | USE_QUIVER) != CMD_OK)
		return;

	if (cmd_get_quantity(cmd, "quantity", &amt, obj->number) != CMD_OK)
		return;

	/* Cannot remove stickied items */
	if (object_is_equipped(player->body, obj) && !obj_can_takeoff(obj)) {
		msg("Hmmm, it seems to be stuck.");
		return;
	}

	inven_drop(obj, amt);
	player->upkeep->energy_use = z_info->move_energy / 2;
}

/**
 * ------------------------------------------------------------------------
 * Using items the traditional way
 * ------------------------------------------------------------------------
 */

enum use {
	USE_TIMEOUT,
	USE_CHARGE,
	USE_SINGLE
};

/**
 * Use an object the right way.
 */
static void use_aux(struct command *cmd, struct object *obj, enum use use,
					int snd)
{
	struct effect *effect = object_effect(obj);
	bool can_use = true;
	bool was_aware, from_floor;
	bool known_aim = false;
	bool none_left = false;
	int dir = 5;
	char label = gear_to_label(obj);
	struct trap_kind *rune = lookup_trap("glyph of warding");

	/* Get arguments */
	assert(cmd_get_arg_item(cmd, "item", &obj) == CMD_OK);

	was_aware = object_flavor_is_aware(obj);

	/* Determine whether we know an item needs to be be aimed */
	if (tval_is_wand(obj) || tval_is_rod(obj) || was_aware ||
		(obj->effect && (obj->known->effect == obj->effect)) ||
		(obj->activation && (obj->known->activation == obj->activation))) {
		known_aim = true;
	}

	if (obj_needs_aim(obj)) {
		/* Unknown things with no obvious aim get a random direction */
		if (!known_aim) {
			dir = ddd[randint0(8)];
		} else if (cmd_get_target(cmd, "target", &dir) != CMD_OK) {
			return;
		}

		/* Confusion wrecks aim */
		player_confuse_dir(player, &dir, false);
	}

	/* track the object used */
	track_object(player->upkeep, obj);

	/* Verify effect */
	assert(effect);

	/* Check for use if necessary */
	if ((use == USE_CHARGE) || (use == USE_TIMEOUT)) {
		can_use = check_devices(obj);
	}

	/* Execute the effect */
	if (can_use) {
		int beam = beam_chance(obj->tval);
		int boost, level, charges = 0;
		int number = 0;
		bool ident = false, used;
		struct object *work_obj;

		/* Get the level */
		if (obj->artifact)
			level = obj->artifact->level;
		else
			level = obj->kind->level;

		/* Sound and/or message */
		if (obj->activation) {
			msgt(snd, "You activate it.");
			activation_message(obj);
		} else if (obj->kind->effect_msg) {
			msgt(snd, obj->kind->effect_msg);
		} else if (obj->kind->vis_msg && !player->timed[TMD_BLIND]) {
			msgt(snd, obj->kind->vis_msg);
		} else {
			/* Make a noise! */
			sound(snd);
		}

		/* Boost damage effects if skill > difficulty */
		boost = MAX((player->state.skills[SKILL_DEVICE] - level) / 2, 0);

		/*
		 * Tentatively deduct the amount used - the effect could leave
		 * the object inaccessible making it difficult to do after a
		 * successful use.  For the same reason, get a copy of the
		 * object to use for propagating knowledge.
		 */
		if (use == USE_SINGLE) {
			if (object_is_carried(player, obj)) {
				work_obj = gear_object_for_use(obj, 1, false, &none_left);
				from_floor = false;
			} else {
				work_obj = floor_object_for_use(obj, 1, false, &none_left);
				from_floor = true;
			}
			/* Record number for messages after use */
			number = (none_left) ? 0 : obj->number;
		} else  {
			if (use == USE_CHARGE) {
				charges = obj->pval;
				/* Use a single charge */
				obj->pval--;
			} else if (use == USE_TIMEOUT) {
				charges = obj->timeout;
				obj->timeout += randcalc(obj->time, 0, RANDOMISE);
			}
			work_obj = object_new();
			object_copy(work_obj, obj);
			work_obj->oidx = 0;
			if (obj->known) {
				work_obj->known = object_new();
				object_copy(work_obj->known, obj->known);
				work_obj->known->oidx = 0;
			}
			from_floor = !object_is_carried(player, obj);
		}

		/* Do effect; use original not copy (proj. effect handling) */
		target_fix();
		used = effect_do(effect,
							source_player(),
							obj,
							&ident,
							was_aware,
							dir,
							beam,
							boost,
							cmd);
		target_release();

		if (!used) {
			/* Restore the tentative deduction. */
			if (use == USE_SINGLE) {
				/* Drop copy to simplify subsequent logic */
				struct object *dropped = object_new();

				object_copy(dropped, work_obj);
				if (work_obj->known) {
					dropped->known = object_new();
					object_copy(dropped->known, work_obj->known);
				}
				if (from_floor) {
					drop_near(cave, &dropped, 0, player->grid, false, true);
				} else {
					inven_carry(player, dropped, true, false);
				}
			} else if (use == USE_CHARGE) {
				obj->pval = charges;
			} else if (use == USE_TIMEOUT) {
				obj->timeout = charges;
			}

			/*
			 * Quit if the item wasn't used and no knowledge was
			 * gained
			 */
			if (was_aware || !ident) {
				if (work_obj->known) {
					object_delete(&work_obj->known);
				}
				object_delete(&work_obj);
				return;
			}
		}

		/* Increase knowledge */
		if (use == USE_SINGLE) {
			char name[80];
			int old_num = work_obj->number;

			/* Single use items are automatically learned */
			if (!was_aware) {
				object_learn_on_use(player, work_obj);
			}
			/* Get a description */
			work_obj->number = number + ((used) ? 0 : 1);
			object_desc(name, sizeof(name), work_obj, ODESC_PREFIX | ODESC_FULL);
			work_obj->number = old_num;
			if (from_floor) {
				/* Print a message */
				msg("You see %s.", name);
			} else {
				msg("You have %s (%c).", name, label);
			}
		} else {
			/* Wearables may need update, other things become known or tried */
			if (tval_is_wearable(work_obj)) {
				update_player_object_knowledge(player);
			} else if (!was_aware && ident) {
				object_learn_on_use(player, work_obj);
			} else {
				object_flavor_tried(work_obj);
			}
		}

		if (used && use == USE_CHARGE) {
			/* Describe charges */
			if (from_floor)
				floor_item_charges(work_obj);
			else
				inven_item_charges(work_obj);
		}

		/* Clean up created copy. */
		if (work_obj->known)
			object_delete(&work_obj->known);
		object_delete(&work_obj);
	} else {
		from_floor = !object_is_carried(player, obj);
	}

	/* Use the turn */
	player->upkeep->energy_use = z_info->move_energy;

	/* Autoinscribe if we are guaranteed to still have any */
	if (!none_left && !from_floor)
		apply_autoinscription(obj);

	/* Mark as tried and redisplay */
	player->upkeep->notice |= (PN_COMBINE);
	player->upkeep->redraw |= (PR_INVEN | PR_EQUIP | PR_OBJECT);

	/* Hack to make Glyph of Warding work properly */
	if (square_trap_specific(cave, player->grid, rune->tidx)) {
		/* Push objects off the grid */
		if (square_object(cave, player->grid))
			push_object(player->grid);
	}
}


/**
 * Run a card
 */
void do_cmd_run_card(struct command *cmd)
{
	struct object *obj;

	if (check_shapechanged())
		return;

	/* Check player can use card */
	if (!player_can_run(player, true))
		return;

	/* Get the card */
	if (cmd_get_item(cmd, "item", &obj,
			"Run which card? ",
			"You have no cards to run.",
			tval_is_card,
			USE_INVEN | USE_FLOOR) != CMD_OK) return;

	use_aux(cmd, obj, USE_SINGLE, MSG_GENERIC);
}

/**
 * Use a device
 */
void do_cmd_use_device(struct command *cmd)
{
	struct object *obj;

	if (check_shapechanged())
		return;

	/* Get an item */
	if (cmd_get_item(cmd, "item", &obj,
			"Use which device? ",
			"You have no devices to use.",
			tval_is_device,
			USE_INVEN | USE_FLOOR | SHOW_FAIL) != CMD_OK) return;

	if (!obj_has_charges(obj)) {
		msg("That device has no charges.");
		return;
	}

	use_aux(cmd, obj, USE_CHARGE, MSG_USE_DEVICE);
}

/**
 * Aim a wand 
 */
void do_cmd_aim_wand(struct command *cmd)
{
	struct object *obj;

	if (check_shapechanged())
		return;

	/* Get an item */
	if (cmd_get_item(cmd, "item", &obj,
			"Aim which wand? ",
			"You have no wands to aim.",
			tval_is_wand,
			USE_INVEN | USE_FLOOR | SHOW_FAIL) != CMD_OK) return;

	if (!obj_has_charges(obj)) {
		msg("That wand has no charges.");
		return;
	}

	use_aux(cmd, obj, USE_CHARGE, MSG_ZAP_ROD);
}

/**
 * Zap a rod 
 */
void do_cmd_zap_rod(struct command *cmd)
{
	struct object *obj;

	if (check_shapechanged())
		return;

	/* Get an item */
	if (cmd_get_item(cmd, "item", &obj,
			"Zap which gadget? ",
			"You have no gadgets to zap.",
			tval_is_rod,
			USE_INVEN | USE_FLOOR | SHOW_FAIL) != CMD_OK) return;

	if (!obj_can_zap(obj)) {
		msg("That gadget is still charging.");
		return;
	}

	use_aux(cmd, obj, USE_TIMEOUT, MSG_ZAP_ROD);
}

/**
 * Activate an object 
 */
void do_cmd_activate(struct command *cmd)
{
	struct object *obj;

	if (check_shapechanged())
		return;

	/* Get an item */
	if (cmd_get_item(cmd, "item", &obj,
			"Activate which item? ",
			"You have no items to activate.",
			obj_is_pack_activatable,
			USE_EQUIP | USE_INVEN | SHOW_FAIL) != CMD_OK) return;

	if (!obj_can_activate(obj)) {
		msg("That item is still charging.");
		return;
	}

	/* Special case for candle type ("MIMIC_KNOW") light sources in your pack (equipped lights
	 * must still activate like other artifacts)
	 * This equips the item and unequips it, taking care to give no excess messages except when
	 * dropping a light from a stack.
	 **/
	if ((tval_is_light(obj)) && (kf_has(obj->kind->kind_flags, KF_MIMIC_KNOW)) && 
		(!object_is_equipped(player->body, obj))) {
		char o_name[80];
		int number = obj->number;
		obj->number = 1;
		object_desc(o_name, sizeof(o_name), obj, ODESC_PREFIX | ODESC_BASE);
		obj->number = number;
		msg("You light %s.", o_name);
		int slot = wield_slot(obj);
		struct object *equip_obj = slot_object(player, slot);
		if (equip_obj)
			do_inven_takeoff(equip_obj, false, false);
		do_inven_wield(obj, slot, false, false);
		do_inven_takeoff(obj, false, true);
		if (equip_obj)
			do_inven_wield(equip_obj, slot, false, true);
		return;
	} else {
		use_aux(cmd, obj, USE_TIMEOUT, MSG_ACT_ARTIFACT);
	}
}

/**
 * Eat some food (or, if an Android, recharge)
 */
void do_cmd_eat_food(struct command *cmd)
{
	struct object *obj = NULL;
	int use = USE_SINGLE;

	if (player_is_shapechanged(player)) {
		msg("In %s form, you can only eat from the floor.",	player->shape->name);
		if (get_check("Do you want to change back? " )) {
			player_resume_normal_shape(player);
		}
	}

	/* Get an item */
	if (player_has(player, PF_NO_FOOD)) {
		if (player_has(player, PF_EAT_BATTERIES)) {
			if (cmd_get_item(cmd, "item", &obj,
				"Recharge from which battery? ",
				"You have no batteries.",
				tval_is_battery,
				USE_INVEN | USE_FLOOR) != CMD_OK) return;
			if (!of_has(obj->flags, OF_BURNS_OUT)) {
				use = USE_TIMEOUT;
			}
		}
	} else {
		if (cmd_get_item(cmd, "item", &obj,
				"Eat which food? ",
				"You have no food to eat.",
				tval_is_edible,
				USE_INVEN | USE_FLOOR) != CMD_OK) return;
	}

	use_aux(cmd, obj, use, MSG_EAT);
}

static bool check_shapechanged(void)
{
	if (player_is_shapechanged(player)) {
		msg("You cannot do this while in %s form.",	player->shape->name);
		if (get_check("Do you want to change back? " )) {
			player_resume_normal_shape(player);
		} else {
			return true;
		}
	}
	return false;
}

/**
 * Take a pill
 */
void do_cmd_quaff_pill(struct command *cmd)
{
	struct object *obj;

	if (check_shapechanged())
		return;

	/* Get an item */
	if (cmd_get_item(cmd, "item", &obj,
			"Take which pill? ",
			"You have no pills which you can take.",
			tval_is_pill,
			USE_INVEN | USE_FLOOR) != CMD_OK) return;

	use_aux(cmd, obj, USE_SINGLE, MSG_QUAFF);
}

/**
 * Use a printer
 */
void do_cmd_use_printer(struct command *cmd)
{
	struct object *obj;

	if (check_shapechanged())
		return;

	/* Get an item */
	if (cmd_get_item(cmd, "item", &obj,
			"Use which printer? ",
			"You have no printers to use.",
			tval_is_printer,
			USE_INVEN | USE_FLOOR) != CMD_OK) return;

	/* Hack */
	obj->pval = 1;
	use_aux(cmd, obj, USE_CHARGE, MSG_PRINT);
	obj->pval = 0;
}

/**
 * Use any usable item
 */
void do_cmd_use(struct command *cmd)
{
	struct object *obj;

	if (check_shapechanged())
		return;

	/* Get an item */
	if (cmd_get_item(cmd, "item", &obj,
			"Use which item? ",
			"You have no items to use.",
			obj_is_useable,
			USE_EQUIP | USE_INVEN | USE_QUIVER | USE_FLOOR | SHOW_FAIL | QUIVER_TAGS | SHOW_FAIL) != CMD_OK)
		return;

	if (tval_is_ammo(obj))				do_cmd_fire(cmd);
	else if (tval_is_pill(obj))			do_cmd_quaff_pill(cmd);
	else if (tval_is_edible(obj))		do_cmd_eat_food(cmd);
	else if (tval_is_rod(obj))			do_cmd_zap_rod(cmd);
	else if (tval_is_wand(obj))			do_cmd_aim_wand(cmd);
	else if (tval_is_device(obj))		do_cmd_use_device(cmd);
	else if (tval_is_card(obj))			do_cmd_run_card(cmd);
	else if (tval_is_printer(obj))		do_cmd_use_printer(cmd);
	else if (obj_can_refill(obj))		do_cmd_refill(cmd);
	else if (obj_is_activatable(obj)) {
		if (object_is_equipped(player->body, obj)) {
			do_cmd_activate(cmd);
		} else {
			msg("Equip the item to use it.");
		}
	} else
		msg("The item cannot be used at the moment");
}


/**
 * ------------------------------------------------------------------------
 * Refuelling
 * ------------------------------------------------------------------------
 */

static void refill_lamp(struct object *lamp, struct object *obj)
{
	if (obj->timeout > 0) {
		/* Cool down */
		msg("You cannot use that battery yet.");
	} else {
		/* Refuel */
		int timeout = obj->timeout ? obj->timeout : obj->pval;
		timeout += lamp->timeout;
		int rpval = randcalc(lamp->kind->pval, player->lev, RANDOMISE);
		if (timeout > rpval)
			timeout = rpval;
		lamp->timeout = timeout;

		/* Message */
		msg("You %srecharge your light.", ((int)lamp->timeout == rpval) ? "fully " : "");
	}

	/* Refilled from another light */
	if (of_has(obj->flags, OF_TAKES_FUEL)) {
		/* Unstack if necessary */
		if (obj->number > 1) {
			/* Obtain a local object, split */
			struct object *used = object_split(obj, 1);

			/* Remove fuel */
			used->timeout = 0;

			/* Carry or drop */
			if (object_is_carried(player, obj) && inven_carry_okay(used))
				inven_carry(player, used, true, true);
			else
				drop_near(cave, &used, 0, player->grid, false, true);
		} else
			/* Empty a single lantern */
			obj->timeout = 0;

		/* Combine the pack (later) */
		player->upkeep->notice |= (PN_COMBINE);

		/* Redraw stuff */
		player->upkeep->redraw |= (PR_INVEN);
	} else { /* Refilled from a battery */
		if (of_has(obj->flags, OF_BURNS_OUT)) {
			struct object *used;
			bool none_left = false;

			/* Decrease the item from the pack or the floor */
			if (object_is_carried(player, obj))
				used = gear_object_for_use(obj, 1, true, &none_left);
			else
				used = floor_object_for_use(obj, 1, true, &none_left);
			if (used->known)
				object_delete(&used->known);
			object_delete(&used);
		} else {
			obj->timeout = randcalc(obj->time, 0, AVERAGE);
		}
	}

	/* Recalculate torch */
	player->upkeep->update |= (PU_TORCH);

	/* Redraw stuff */
	player->upkeep->redraw |= (PR_EQUIP);
}


void do_cmd_refill(struct command *cmd)
{
	struct object *light = equipped_item_by_slot_name(player, "light");
	struct object *obj;

	if (check_shapechanged())
		return;

	/* Get an item */
	if (cmd_get_item(cmd, "item", &obj,
			"Recharge from which battery? ",
			"You have nothing you can recharge with.",
			obj_can_refill,
			USE_INVEN | USE_FLOOR) != CMD_OK) return;

	/* Check what we're wielding. */
	if (!light || !tval_is_light(light)) {
		msg("You are not using a light.");
		return;
	} else if (of_has(light->flags, OF_NO_FUEL)) {
		msg("Your light cannot be recharged.");
		return;
	} else if (of_has(light->flags, OF_TAKES_FUEL)) {
		bool was_removable = obj_can_takeoff(light);

		refill_lamp(light, obj);

		bool is_removable = obj_can_takeoff(light);
		if ((!is_removable) && (was_removable)) {
			msgt(MSG_FAULTY, "As you recharge it, your light clamps itself firmly to you!");
		}
	} else {
		msg("Your light cannot be recharged.");
		return;
	}

	player->upkeep->energy_use = z_info->move_energy / 2;
}



/**
 * ------------------------------------------------------------------------
 * Intrinsic abilities
 * ------------------------------------------------------------------------
 */

/**
 * Use an intrinsic ability.
 * Most should be accessible by a single keypress (you will typically not
 * have too many to be immediately visible, and even in that case they
 * would not all need to be behind a sub-menu) 
 */
void do_cmd_cast(struct command *cmd)
{
	int spell_index, dir = 0;
	const char *error = "You have no techniques you can use.";

	int n_spells = 0;
	combine_books(&n_spells, NULL, NULL, NULL);

	/* Maybe some still work?
	 * Most should make this check, though.
	 */
	if (player_is_shapechanged(player)) {
		/* Count the abilities - if you don't have any, prompt to change back */
		if (n_spells == 0) {
			if (get_check("Change back to your original form? " )) {
				player_resume_normal_shape(player);
			}
			return;
		}
	} else if (n_spells == 0) {
		msg(error);
		return;
	}

	/* Get arguments */
	if (cmd_get_spell(cmd, "technique", &spell_index,
			/* Verb */   "use",
			/* Error */  error,
			/* Filter */ NULL) != CMD_OK) { 
		return;
	}

	/* Cool down? */
	if (player->cooldown[spell_index] > 0) {
		msg("You can't use that technique for another %d turns.", player->cooldown[spell_index]);
		return;
	}

	if (spell_needs_aim(spell_index)) {
		if (cmd_get_target(cmd, "target", &dir) == CMD_OK)
			player_confuse_dir(player, &dir, false);
		else
			return;
	}

	/* Cast a spell */
	target_fix();
	if (spell_cast(spell_index, dir, cmd)) {
		if (player->timed[TMD_FASTCAST]) {
			player->upkeep->energy_use = (z_info->move_energy * 3) / 4;
		} else {
			player->upkeep->energy_use = z_info->move_energy;
		}
	}
	target_release();
}
