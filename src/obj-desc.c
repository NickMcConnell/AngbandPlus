/**
 * \file obj-desc.c
 * \brief Create object name descriptions
 *
 * Copyright (c) 1997 - 2007 Angband contributors
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
#include "obj-chest.h"
#include "obj-desc.h"
#include "obj-gear.h"
#include "obj-ignore.h"
#include "obj-knowledge.h"
#include "obj-tval.h"
#include "obj-util.h"

/**
 * Puts the object base kind's name into buf.
 */
void object_base_name(char *buf, size_t max, int tval, bool plural)
{
	struct object_base *kb = &kb_info[tval];
	size_t end = 0;

	if (kb->name && kb->name[0]) 
		(void) obj_desc_name_format(buf, max, end, kb->name, NULL, plural);
}


/**
 * Puts a very stripped-down version of an object's name into buf.
 * If easy_know is true, then the IDed names are used, otherwise
 * flavours, card names, etc will be used.
 *
 * Just truncates if the buffer isn't big enough.
 */
void object_kind_name(char *buf, size_t max, const struct object_kind *kind,
					  bool easy_know)
{
	/* If not aware, the plain flavour (e.g. Copper) will do. */
	if (!easy_know && !kind->aware && kind->flavor)
		my_strcpy(buf, kind->flavor->text, max);

	/* Use proper name (Healing, or whatever) */
	else
		(void) obj_desc_name_format(buf, max, 0, kind->name, NULL, false);
}


/**
 * A modifier string, put where '#' goes in the basename below.
 */
static const char *obj_desc_get_modstr(const struct object_kind *kind)
{
	if (tval_can_have_flavor_k(kind))
		return kind->flavor ? kind->flavor->text : "";

	return "";
}

/**
 * An object's basic name - a generic name for flavored objects (with the
 * actual name added later depending on awareness, the name from object.txt
 * for almost everything else, and a bit extra for books. 
 */
static const char *obj_desc_get_basename(const struct object *obj, bool aware,
										 bool terse, int mode)
{
	bool show_flavor = !terse && obj->kind->flavor;

	if (mode & ODESC_STORE)
		show_flavor = false;
	if (aware && !OPT(player, show_flavors)) show_flavor = false;

	/* Artifacts are special */
	if (obj->artifact && (aware || object_is_known_artifact(obj) || terse ||
						  !obj->kind->flavor))
		return obj->kind->name;

	/* Analyze the object */
	switch (obj->tval)
	{
		default:
			return obj->kind->name;

		case TV_DEVICE:
			return (show_flavor ? "& # device~" : "& device~");

		case TV_WAND:
			return (show_flavor ? "& # gun~" : "& gun~");

		case TV_GADGET:
			return (show_flavor ? "& # gadget~" : "& gadget~");

		case TV_LIGHT:
			if (!obj->kind->flavor)
				return obj->kind->name;
			return (show_flavor ? "& # candle~" : "& candle~");

		case TV_PILL:
			if (show_flavor) {
				if (aware) {
					return "& pill~ (#)";
				} else {
					return "& # pill~";
				}
			} else {
				return "& pill~";
			}
		case TV_CARD:
			return (show_flavor ? "& card~ titled #" : "& card~");

		case TV_MUSHROOM:
			return (show_flavor ? "& # mushroom~" : "& mushroom~");
	}

	return "(nothing)";
}


/**
 * Start to description, indicating number/uniqueness (a, the, no more, 7, etc)
 */
static size_t obj_desc_name_prefix(char *buf, size_t max, size_t end,
		const struct object *obj, const char *basename,
		const char *modstr, bool terse)
{
	if (obj->number == 0) {
		strnfcat(buf, max, &end, "no more ");
	} else if (obj->number > 1) {
		strnfcat(buf, max, &end, "%d ", obj->number);
	} else if (object_is_known_artifact(obj)) {
		strnfcat(buf, max, &end, "the ");
	} else if (*basename == '&') {
		bool an = false;
		const char *lookahead = basename + 1;

		while (*lookahead == ' ') lookahead++;

		if (*lookahead == '#') {
			if (modstr && is_a_vowel(*modstr))
				an = true;
		} else if (is_a_vowel(*lookahead)) {
			an = true;
		}

		if (!terse) {
			if (an)
				strnfcat(buf, max, &end, "an ");
			else
				strnfcat(buf, max, &end, "a ");			
		}
	}

	return end;
}



/**
 * Formats 'fmt' into 'buf', with the following formatting characters:
 *
 * '~' at the end of a word (e.g. "fridge~") will pluralise
 *
 * '|x|y|' will be output as 'x' if singular or 'y' if plural
 *    (e.g. "kni|fe|ves|")
 *
 * '#' will be replaced with 'modstr' (which may contain the pluralising
 * formats given above).
 */
size_t obj_desc_name_format(char *buf, size_t max, size_t end,
		const char *fmt, const char *modstr, bool pluralise)
{
	/* Copy the string */
	while (*fmt) {
		/* Skip */
		if (*fmt == '&') {
			while (*fmt == ' ' || *fmt == '&')
				fmt++;
			continue;
		} else if (*fmt == '~') {
			/* Pluralizer (regular English plurals) */
			char prev = *(fmt - 1);

			if (!pluralise)	{
				fmt++;
				continue;
			}

			/* e.g. cutlass-e-s, torch-e-s, box-e-s */
			if (prev == 's' || prev == 'h' || prev == 'x')
				strnfcat(buf, max, &end, "es");
			else
				strnfcat(buf, max, &end, "s");
		} else if (*fmt == '|') {
			/* Special plurals 
			* e.g. kni|fe|ves|
			*          ^  ^  ^ */
			const char *singular = fmt + 1;
			const char *plural   = strchr(singular, '|');
			const char *endmark  = NULL;

			if (plural) {
				plural++;
				endmark = strchr(plural, '|');
			}

			if (!singular || !plural || !endmark) return end;

			if (!pluralise)
				strnfcat(buf, max, &end, "%.*s", plural - singular - 1,
						 singular);
			else
				strnfcat(buf, max, &end, "%.*s", endmark - plural, plural);

			fmt = endmark;
		} else if (*fmt == '#') {
			/* Add modstr, with pluralisation if relevant */
			end = obj_desc_name_format(buf, max, end, modstr, NULL,	pluralise);
		}

		else
			buf[end++] = *fmt;

		fmt++;
	}

	buf[end] = 0;

	return end;
}


/**
 * Format object obj's name into 'buf'.
 */
static size_t obj_desc_name(char *buf, size_t max, size_t end,
		const struct object *obj, bool prefix, int mode, bool terse)
{
	bool store = mode & ODESC_STORE ? true : false;
	bool spoil = mode & ODESC_SPOIL ? true : false;
	
	/* Actual name for flavoured objects if aware, or in store, or spoiled */
	bool aware = object_flavor_is_aware(obj) || store || spoil;
	bool mimic = (obj->kind->flavor && kf_has(obj->kind->kind_flags, KF_MIMIC_KNOW) );

	/* Pluralize if (not forced singular) and
	 * (not a known/visible artifact) and
	 * (not one in stack or forced plural) */
	bool plural = !(mode & ODESC_SINGULAR) &&
		!obj->artifact &&
		(obj->number != 1 || (mode & ODESC_PLURAL));
	const char *basename = obj_desc_get_basename(obj, aware, terse, mode);
	if (mimic && !aware)
		basename = obj->kind->flavor->text;
	const char *modstr = obj_desc_get_modstr(obj->kind);

	/* Some items have unadorned names */
	if ((obj->tval == TV_WAND) || (obj->tval == TV_DEVICE) ||
		(obj->tval == TV_GADGET) || (obj->tval == TV_LIGHT)) {
		const char *name = obj->kind->name;
		bool show_flavor = !terse && obj->kind->flavor && !store;
		char buf2[256];

		if ((aware) && (!(OPT(player, show_flavors))))
			show_flavor = false;

		if (aware) {
			if (show_flavor) {
				name = buf2;
				strnfmt(buf2, sizeof(buf2), "& # %s", obj->kind->name);
			} else {
				; // use kind name
			}
		} else {
			basename = obj_desc_get_basename(obj, aware, terse, mode);
			name = basename;
		}

		/* Quantity prefix */
		if (prefix)
			end = obj_desc_name_prefix(buf, max, end, obj, name, modstr, terse);

		end = obj_desc_name_format(buf, max, end, name, modstr, plural);
	} else {
		/* Prepend extra names */
		if (obj->kind->flavor && aware && !mimic) {
			const char *space = " ";
			char buf2[256];
			*buf2 = 0;
			size_t end2 = 0;

			/* Quantity prefix */
			if (prefix) {
				char buf3[256];
				size_t end3 = 0;
				*buf3 = 0;
				strnfcat(buf3, sizeof(buf3), &end3, "&%s", obj->kind->name);
				obj_desc_name_prefix(buf2, sizeof(buf2), end2, obj, buf3, modstr, terse);
			}

			/* Contract "foo- pill" into "foo-pill", and don't put a space between an item name
			 * and suffix when it doesn't take a class name
			 **/
			const char *spacename = obj->kind->name;
			if (!isalpha(spacename[strlen(spacename)-1]))
					space = "";
			strnfcat(buf, max, &end, "%s%s%s", buf2, obj->kind->name, space);
		} else {
			/* Quantity prefix */
			if (prefix)
				end = obj_desc_name_prefix(buf, max, end, obj, basename, modstr, terse);
		}

		/* Append extra names of various kinds */
		if ((object_is_known_artifact(obj)) && (obj->artifact->name[0] == '<'))
			strnfcat(buf, max, &end, "%s ", obj->artifact->name + 1);

		/* Base name */
		end = obj_desc_name_format(buf, max, end, basename, modstr, plural);

		/* Append extra names of various kinds */
		if ((object_is_known_artifact(obj)) && (obj->artifact->name[0] != '<'))
			strnfcat(buf, max, &end, " %s", obj->artifact->name);
		else if ((obj->known->ego && !(mode & ODESC_NOEGO)) || (obj->ego && store))
			strnfcat(buf, max, &end, " %s", obj->ego->name);
		else if (aware && !obj->artifact &&
				 (obj->kind->flavor || obj->kind->tval == TV_CARD)) {
			;
		}
	}
	return end;
}

/**
 * Is obj armor?
 */
static bool obj_desc_show_armor(const struct object *obj)
{
	if (player->obj_k->ac && (obj->ac || tval_is_armor(obj))) return true;

	return false;
}

/**
 * Special descriptions for types of chest traps
 */
static size_t obj_desc_chest(const struct object *obj, char *buf, size_t max,
							 size_t end)
{
	if (!tval_is_chest(obj)) return end;

	/* The chest is unopened, but we know nothing about its trap/lock */
	if (obj->pval && !obj->known->pval) return end;

	/* Describe the traps */
	strnfcat(buf, max, &end, format(" (%s)", chest_trap_name(obj)));

	return end;
}

/**
 * Describe combat properties of an item - damage dice, to-hit, to-dam, armor
 * class, missile multipler
 */
static size_t obj_desc_combat(const struct object *obj, char *buf, size_t max, 
							  size_t end, int mode)
{
	bool spoil = mode & ODESC_SPOIL ? true : false;

	/* Display damage dice if they are known */
	if (kf_has(obj->kind->kind_flags, KF_SHOW_DICE) &&
		(player->obj_k->dd && player->obj_k->ds))
		strnfcat(buf, max, &end, " (%dd%d)", obj->dd, obj->ds);

	/* Display shooting power as part of the multiplier */
	if (kf_has(obj->kind->kind_flags, KF_SHOW_MULT))
		strnfcat(buf, max, &end, " (x%d)",
				 obj->pval + obj->modifiers[OBJ_MOD_MIGHT]);

	/* No more if the object hasn't been assessed */
	if (!((obj->notice & OBJ_NOTICE_ASSESSED) || spoil)) return end;

	/* Show weapon bonuses if we know of any */
	if (player->obj_k->to_h && player->obj_k->to_d &&
		(tval_is_weapon(obj) || obj->to_d ||
		 (obj->to_h && !tval_is_body_armor(obj) && !(obj->tval == TV_BOOTS)) ||
		 (!object_has_standard_to_h(obj) && !obj->artifact && !obj->ego))) {
		/* In general show full combat bonuses */
		strnfcat(buf, max, &end, " (%+d,%+d)", obj->to_h, obj->to_d);
	} else if (obj->to_h < 0 && object_has_standard_to_h(obj)) {
		/* Special treatment for body armor with only a to-hit penalty */
		strnfcat(buf, max, &end, " (%+d)", obj->to_h);
	} else if (obj->to_d != 0 && player->obj_k->to_d) {
		/* To-dam icon known only */
		strnfcat(buf, max, &end, " (%+d)", obj->to_d);
	} else if (obj->to_h != 0 && player->obj_k->to_h) {
		/* To-hit icon known only */
		strnfcat(buf, max, &end, " (%+d)", obj->to_h);
	}

	/* Show armor bonuses */
	if (player->obj_k->to_a) {
		if (obj_desc_show_armor(obj))
			strnfcat(buf, max, &end, " [%d,%+d]", obj->ac, obj->to_a);
		else if (obj->to_a)
			strnfcat(buf, max, &end, " [%+d]", obj->to_a);
	} else if (obj_desc_show_armor(obj)) {
		strnfcat(buf, max, &end, " [%d]", obj->ac);
	}

	return end;
}

/**
 * Describe remaining light for refuellable lights
 */
static size_t obj_desc_light(const struct object *obj, char *buf, size_t max,
							 size_t end)
{
	/* Fuelled light sources get number of remaining turns appended. Lights that don't require fuel or
	 * which should be indistinguishable from other lights when unIDed don't.
	 **/
	if (tval_is_light(obj) && !of_has(obj->flags, OF_NO_FUEL) && kf_has(obj->kind->kind_flags, KF_EASY_KNOW))
		strnfcat(buf, max, &end, " (%d turns)", obj->timeout);

	return end;
}

/**
 * Describe numerical modifiers to stats and other player qualities which
 * allow numerical bonuses - speed, stealth, etc
 */
static size_t obj_desc_mods(const struct object *obj, char *buf, size_t max,
							size_t end)
{
	int i, j, num_mods = 0;
	int mods[OBJ_MOD_MAX] = { 0 };

	/* Run through possible modifiers and store distinct ones */
	for (i = 0; i < OBJ_MOD_MAX; i++) {
		/* Check for known non-zero mods */
		if (obj->modifiers[i] != 0) {
			/* If no mods stored yet, store and move on */
			if (!num_mods) {
				mods[num_mods++] = obj->modifiers[i];
				continue;
			}

			/* Run through the existing mods, quit on duplicates */
			for (j = 0; j < num_mods; j++)
				if (mods[j] == obj->modifiers[i]) break;

			/* Add another mod if needed */
			if (j == num_mods)
				mods[num_mods++] = obj->modifiers[i];
		}
	}

	if (!num_mods) return end;

	/* Print the modifiers */
	strnfcat(buf, max, &end, " <");
	for (j = 0; j < num_mods; j++) {
		if (j) strnfcat(buf, max, &end, ", ");
		strnfcat(buf, max, &end, "%+d", mods[j]);
	}
	strnfcat(buf, max, &end, ">");

	return end;
}

/**
 * Describe charges or charging status for re-usable items with magic effects
 */
static size_t obj_desc_charges(const struct object *obj, char *buf, size_t max,
							   size_t end, int mode)
{
	bool aware = object_flavor_is_aware(obj) || (mode & ODESC_STORE);

	/* Wands and devices have charges, others may be charging */
	if (aware && tval_can_have_charges(obj)) {
		strnfcat(buf, max, &end, " (%d charge%s)", obj->pval,
				 PLURAL(obj->pval));
	} else if (obj->timeout > 0) {
		if (tval_can_have_timeout(obj)) {
			/* Lights are 'lit' if they have less than max timeout.
			 * Lights must also be allowed to be 'charging', because they may have activations.
			 * This assumes that nothing with fuel will also have an activation.
			 */
			const char *charging = "charging";
			if (tval_is_light(obj)) {
				if ((obj->timeout < randcalc(obj->kind->pval, 0, AVERAGE)) && (of_has(obj->flags, OF_BURNS_OUT)))
					charging = "lit";
				else if (!(of_has(obj->flags, OF_NO_FUEL)))
					return end;
				if ((of_has(obj->flags, OF_NO_FUEL))) {
					if (obj->timeout == randcalc(obj->kind->pval, 0, AVERAGE))
						return end;
				}
			}
			if (obj->number > 1)
				strnfcat(buf, max, &end, " (%d %s)", number_charging(obj), charging);
			else if (tval_can_have_timeout(obj) || obj->activation || obj->effect)
				/* Artifacts, single rods */
				strnfcat(buf, max, &end, " (%s)", charging);
		}
	}

	return end;
}

/**
 * Add player-defined inscriptions or game-defined descriptions
 */
static size_t obj_desc_inscrip(const struct object *obj, char *buf,
							   size_t max, size_t end)
{
	const char *u[6] = { 0, 0, 0, 0, 0, 0 };
	int n = 0;

	/* Get inscription */
	if (obj->note)
		u[n++] = quark_str(obj->note);

	/* Use special inscription, if any */
	if (!object_flavor_is_aware(obj)) {
		if (tval_can_have_charges(obj) && (obj->pval == 0))
			u[n++] = "empty";
		if (object_flavor_was_tried(obj))
			u[n++] = "tried";
	}

	/* Note faults */
	if (obj->known->faults)
		u[n++] = "faulty";

	/* Note ignore */
	if (ignore_item_ok(obj))
		u[n++] = "ignore";

	/* Note quest */
	if (of_has(obj->flags, OF_QUEST_SPECIAL))
		u[n++] = "special";

	/* Note unknown properties */
	if (!object_icons_known(obj) && (obj->known->notice & OBJ_NOTICE_ASSESSED))
		u[n++] = "??";

	if (n) {
		int i;
		for (i = 0; i < n; i++) {
			if (i == 0)
				strnfcat(buf, max, &end, " {");
			strnfcat(buf, max, &end, "%s", u[i]);
			if (i < n - 1)
				strnfcat(buf, max, &end, ", ");
		}

		strnfcat(buf, max, &end, "}");
	}

	return end;
}


/**
 * Add "unseen" to the end of unaware items in stores,
 * and "??" to not fully known unflavored items 
 */
static size_t obj_desc_aware(const struct object *obj, char *buf, size_t max,
							 size_t end)
{
	if (!object_flavor_is_aware(obj)) {
		strnfcat(buf, max, &end, " {unseen}");
	} else if (!object_icons_known(obj)) {
		strnfcat(buf, max, &end, " {??}");
	} else if (obj->known->faults) {
		strnfcat(buf, max, &end, " {faulty}");
	}

	return end;
}


/**
 * Describes item `obj` into buffer `buf` of size `max`.
 *
 * ODESC_PREFIX prepends a 'the', 'a' or number
 * ODESC_BASE results in a base description.
 * ODESC_COMBAT will add to-hit, to-dam and AC info.
 * ODESC_EXTRA will add pval/charge/inscription/ignore info.
 * ODESC_PLURAL will pluralise regardless of the number in the stack.
 * ODESC_STORE turns off ignore markers, for in-store display.
 * ODESC_SPOIL treats the object as fully identified.
 *
 * Setting 'prefix' to true prepends a 'the', 'a' or the number in the stack,
 * respectively.
 *
 * \returns The number of bytes used of the buffer.
 */
size_t object_desc(char *buf, size_t max, const struct object *obj, int mode)
{
	bool prefix = mode & ODESC_PREFIX ? true : false;
	bool spoil = mode & ODESC_SPOIL ? true : false;
	bool terse = mode & ODESC_TERSE ? true : false;

	size_t end = 0;

	/* Simple description for null item */
	if (!obj || !obj->known)
		return strnfmt(buf, max, "(nothing)");

	/* Unknown items and cash get straightforward descriptions */
	if (obj->known && obj->kind != obj->known->kind) {
		if (prefix)
			return strnfmt(buf, max, "an unknown item");
		return strnfmt(buf, max, "unknown item");
	}

	if (tval_is_money(obj))
		return strnfmt(buf, max, "$%d worth of %s%s",
				obj->pval, obj->kind->name,
				ignore_item_ok(obj) ? " {ignore}" : "");

	/* Egos and kinds whose name we know are seen */
	if (obj->known->ego && obj->ego && !spoil)
		obj->ego->everseen = true;

	if (object_flavor_is_aware(obj) && !spoil)
		obj->kind->everseen = true;

	/** Construct the name **/

	/* Copy the base name to the buffer */
	end = obj_desc_name(buf, max, end, obj, prefix, mode, terse);

	/* Combat properties */
	if (mode & ODESC_COMBAT) {
		if (tval_is_chest(obj))
			end = obj_desc_chest(obj, buf, max, end);
		else if (tval_is_light(obj))
			end = obj_desc_light(obj, buf, max, end);

		end = obj_desc_combat(obj->known, buf, max, end, mode);
	}

	/* Modifiers, charges, flavour details, inscriptions */
	if (mode & ODESC_EXTRA) {
		end = obj_desc_mods(obj->known, buf, max, end);

		end = obj_desc_charges(obj, buf, max, end, mode);

		if (mode & ODESC_STORE)
			end = obj_desc_aware(obj, buf, max, end);
		else
			end = obj_desc_inscrip(obj, buf, max, end);
	}

	return end;
}
