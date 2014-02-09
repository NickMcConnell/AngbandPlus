/* File: object1.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * 						Jeff Greene, Diego Gonzalez
 *
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


/*
 * Efficient version of '(T) += sprintf((T), "%s", (S))'
 */
#define object_desc_str_macro(T,S) do { \
 \
	cptr s = (S); \
 \
	/* Copy the string */ \
	while (*s) *(T)++ = *s++; \
 \
} while (0)






/*
 * Strip an "object name" into a buffer.
 */
void strip_name(char *buf, int k_idx)
{
	char *t;

	object_kind *k_ptr = &k_info[k_idx];

	cptr str = (k_name + k_ptr->name);


	/* Skip past leading characters */
	while ((*str == ' ') || (*str == '&')) str++;

	/* Copy useful chars */
	for (t = buf; *str; str++)
	{
		if (*str != '~') *t++ = *str;
	}

	/* Terminate the new name */
	*t = '\0';
}

static const char *obj_desc_get_modstr(const object_type *o_ptr)
{
	object_kind *k_ptr = &k_info[o_ptr->k_idx];

	switch (o_ptr->tval)
	{
		case TV_AMULET:
		case TV_RING:
		case TV_STAFF:
		case TV_WAND:
		case TV_ROD:
		case TV_POTION:
		case TV_FOOD:
			return (flavor_text + flavor_info[k_ptr->flavor].text);

		case TV_SCROLL:
		case TV_PARCHMENT:
			return scroll_adj[o_ptr->sval];

		case TV_MAGIC_BOOK:
		case TV_PRAYER_BOOK:
		case TV_DRUID_BOOK:
			return (k_name + k_ptr->name);
	}

	return "";
}



static const char *obj_desc_get_basename(const object_type *o_ptr, bool aware)
{
	object_kind *k_ptr = &k_info[o_ptr->k_idx];

	bool show_flavor = k_ptr->flavor ? TRUE : FALSE;

	if (o_ptr->ident & IDENT_STORE) show_flavor = FALSE;
	if (aware && !show_flavors) show_flavor = FALSE;

	/* Known artifacts get special treatment */
	if (artifact_p(o_ptr) && aware)
	{
		/* An exception for unidentified special artifacts */
		if (!k_ptr->flavor || object_is_known(o_ptr)) return (k_name + k_ptr->name);
	}

	/* Analyze the object */
	switch (o_ptr->tval)
	{
		case TV_SKELETON:
		case TV_BOTTLE:
		case TV_JUNK:
		case TV_SPIKE:
		case TV_FLASK:
		case TV_CHEST:
		case TV_SHOT:
		case TV_BOLT:
		case TV_ARROW:
		case TV_BOW:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_DIGGING:
		case TV_BOOTS:
		case TV_GLOVES:
		case TV_CLOAK:
		case TV_CROWN:
		case TV_HELM:
		case TV_SHIELD:
		case TV_SOFT_ARMOR:
		case TV_HARD_ARMOR:
		case TV_DRAG_ARMOR:
		case TV_DRAG_SHIELD:
		case TV_LIGHT:
		case TV_PARCHMENT:
			return (k_name + k_ptr->name);

		case TV_AMULET:
			return (show_flavor ? "& # Amulet~" : "& Amulet~");

		case TV_RING:
			return (show_flavor ? "& # Ring~" : "& Ring~");

		case TV_STAFF:
			return (show_flavor ? "& # Sta|ff|ves|" : "& Sta|ff|ves|");

		case TV_WAND:
			return (show_flavor ? "& # Wand~" : "& Wand~");

		case TV_ROD:
			return (show_flavor ? "& # Rod~" : "& Rod~");

		case TV_POTION:
			return (show_flavor ? "& # Potion~" : "& Potion~");

		case TV_SCROLL:
			return (show_flavor ? "& Scroll~ titled \"#\"" : "& Scroll~");

		case TV_MAGIC_BOOK:
			return "& Book~ of Magic Spells #";

		case TV_PRAYER_BOOK:
			return "& Holy Book~ of Prayers #";

		case TV_DRUID_BOOK:
				return "& Book~ of Druid Spells #";

		case TV_FOOD:
			if (o_ptr->sval < SV_FOOD_MIN_FOOD)
				return (show_flavor ? "& # Mushroom~" : "& Mushroom~");
			else
				return (k_name + k_ptr->name);
	}

	return "(nothing)";
}



/*
 * Copy 'src' into 'buf, replacing '#' with 'modstr' (if found), putting a plural
 * in the place indicated by '~' if required, or using alterate...
 */
static size_t obj_desc_name(char *buf, size_t max, size_t end,
		const object_type *o_ptr, bool prefix, byte mode,
		bool spoil)
{
	object_kind *k_ptr = &k_info[o_ptr->k_idx];

	bool known = object_is_known(o_ptr) || (o_ptr->ident & IDENT_STORE) || spoil;
	bool aware = object_flavor_is_aware(o_ptr) || (o_ptr->ident & IDENT_STORE) || spoil;

	const char *basename = obj_desc_get_basename(o_ptr, aware);
	const char *modstr = obj_desc_get_modstr(o_ptr);

	bool pluralise = (mode & ODESC_PLURAL) ? TRUE : FALSE;

	if (o_ptr->number > 1)
		pluralise = TRUE;
	if (mode & ODESC_SINGULAR)
		pluralise = FALSE;

	/* Add a pseudo-numerical prefix if desired */
	if (prefix)
	{
		if (o_ptr->number <= 0)
		{
			strnfcat(buf, max, &end, "no more ");

			/* Pluralise for grammatical correctness */
			pluralise = TRUE;
		}
		else if (o_ptr->number > 1)
			strnfcat(buf, max, &end, "%d ", o_ptr->number);
		else if ((known) && artifact_p(o_ptr))
			strnfcat(buf, max, &end, "The ");

		else if (*basename == '&')
		{
			bool an = FALSE;
			const char *lookahead = basename + 1;

			while (*lookahead == ' ') lookahead++;

			if (*lookahead == '#')
			{
				if (modstr && is_a_vowel(*modstr))
					an = TRUE;
			}
			else if (is_a_vowel(*lookahead))
			{
				an = TRUE;
			}

			if (an)
				strnfcat(buf, max, &end, "an ");
			else
				strnfcat(buf, max, &end, "a ");
		}
	}

	/* Perfectly balanced throwing weapons are indicated. */
	if ((known) && (o_ptr->ident & IDENT_PERFECT_BALANCE))
	{
		strnfcat(buf, max, &end, "Well-balanced ");
	}



/*
 * Names have the following elements:
 *
 * '~' indicates where to place an 's' or an 'es'.  Other plural forms should
 * be handled with the syntax '|singular|plural|', e.g. "kni|fe|ves|".
 *
 * '#' indicates the position of the "modifier", e.g. the flavour or spellbook
 * name.
 */

	/* Copy the string */
	while (*basename)
	{
		if (*basename == '&')
		{
			while (*basename == ' ' || *basename == '&')
				basename++;
			continue;
		}

		/* Pluralizer (regular English plurals) */
		else if (*basename == '~')
		{
			char prev = *(basename - 1);

			if (!pluralise)
			{
				basename++;
				continue;
			}

			/* e.g. cutlass-e-s, torch-e-s, box-e-s */
			if (prev == 's' || prev == 'h' || prev == 'x')
				strnfcat(buf, max, &end, "es");
			else
				strnfcat(buf, max, &end, "s");
		}

		/* Special plurals */
		else if (*basename == '|')
		{
			/* e.g. & Wooden T|o|e|rch~
			 *                 ^ ^^ */
			const char *singular = basename + 1;
			const char *plural   = strchr(singular, '|');
			const char *endmark  = NULL;

			if (plural)
			{
				plural++;
				endmark = strchr(plural, '|');
			}

			if (!singular || !plural || !endmark) return end;

			if (!pluralise)
				strnfcat(buf, max, &end, "%.*s", plural - singular - 1, singular);
			else
				strnfcat(buf, max, &end, "%.*s", endmark - plural, plural);

			basename = endmark;
		}

		/* Handle pluralisation in the modifier XXX */
		else if (*basename == '#')
		{
			const char *basename = modstr;

			while (basename && *basename && (end < max - 1))
			{
				/* Special plurals */
				if (*basename == '|')
				{
					/* e.g. & Wooden T|o|e|rch~
					 *                 ^ ^^ */
					const char *singular = basename + 1;
					const char *plural   = strchr(singular, '|');
					const char *endmark  = NULL;

					if (plural)
					{
						plural++;
						endmark = strchr(plural, '|');
					}

					if (!singular || !plural || !endmark) return end;

					if (!pluralise)
						strnfcat(buf, max, &end, "%.*s", plural - singular - 1, singular);
					else
						strnfcat(buf, max, &end, "%.*s", endmark - plural, plural);

					basename = endmark;
				}

				else
					buf[end++] = *basename;

				basename++;
			}
		}

		else
			buf[end++] = *basename;

		basename++;
	}

	/* 0-terminate, just in case XXX */
	buf[end] = 0;


	/** Append extra names of various kinds **/

	if ((known) && o_ptr->art_num)
		strnfcat(buf, max, &end, " %s", a_info[o_ptr->art_num].name);

	else if ((o_ptr->ego_num) && (known || spoil))
	{
		strnfcat(buf, max, &end, " %s", e_name + e_info[o_ptr->ego_num].name);
	}


	else if (aware && !artifact_p(o_ptr) && (k_ptr->flavor || k_ptr->tval == TV_SCROLL))
		strnfcat(buf, max, &end, " of %s", k_name + k_ptr->name);

	return end;
}

/*
 * Is o_ptr a weapon?
 */
static bool obj_desc_show_weapon(const object_type *o_ptr)
{
	u32b f1, f2, f3, fn;
	object_flags(o_ptr, &f1, &f2, &f3, &fn);

	if (f3 & TR3_SHOW_MODS) return TRUE;
	if (o_ptr->to_h && o_ptr->to_d) return TRUE;

	/* You need to list both to_h and to_d for things like unaware rings of accuracy and damage e.g. to differentiate (+8) */
	if ((o_ptr->to_h || o_ptr->to_d) && !object_flavor_is_aware(o_ptr)) return TRUE;

	switch (o_ptr->tval)
	{
		case TV_SHOT:
		case TV_BOLT:
		case TV_ARROW:
		case TV_BOW:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_DIGGING:
		{
			return TRUE;
		}
	}

	return FALSE;
}

/*
 * Is o_ptr armor?
 */
static bool obj_desc_show_armor(const object_type *o_ptr)
{
	if (o_ptr->ac) return TRUE;

	switch (o_ptr->tval)
	{
		case TV_BOOTS:
		case TV_GLOVES:
		case TV_CLOAK:
		case TV_CROWN:
		case TV_HELM:
		case TV_SHIELD:
		case TV_SOFT_ARMOR:
		case TV_HARD_ARMOR:
		case TV_DRAG_ARMOR:
		case TV_DRAG_SHIELD:
		{
			return TRUE;
		}
	}

	return FALSE;
}

static size_t obj_desc_chest(const object_type *o_ptr, char *buf, size_t max, size_t end)
{
	bool known = object_is_known(o_ptr) || (o_ptr->ident & IDENT_STORE);

	if (o_ptr->tval != TV_CHEST) return end;
	if (!known) return end;

	/* Special label for quest chests */
	if (o_ptr->ident & (IDENT_QUEST)) strnfcat(buf, max, &end, " (special)");

	/* May be "empty" */
	else if (!o_ptr->pval) strnfcat(buf, max, &end, " (empty)");

	/* May be "disarmed" */
	else if (o_ptr->pval < 0)
	{
		if (chest_traps[0 - o_ptr->pval])
			strnfcat(buf, max, &end, " (disarmed)");
		else
			strnfcat(buf, max, &end, " (unlocked)");
	}

	/* Describe the traps, if any */
	else
	{
		/* Describe the traps */
		switch (chest_traps[o_ptr->pval])
		{
			case 0:
				strnfcat(buf, max, &end, " (Locked)");
				break;

			case CHEST_LOSE_STR:
				strnfcat(buf, max, &end, " (Poison Needle)");
				break;

			case CHEST_LOSE_CON:
				strnfcat(buf, max, &end, " (Poison Needle)");
				break;

			case CHEST_POISON:
				strnfcat(buf, max, &end, " (Gas Trap)");
				break;

			case CHEST_PARALYZE:
				strnfcat(buf, max, &end, " (Gas Trap)");
				break;

			case CHEST_EXPLODE:
				strnfcat(buf, max, &end, " (Explosion Device)");
				break;

			case CHEST_SUMMON:
				strnfcat(buf, max, &end, " (Summoning Runes)");
				break;

			default:
				strnfcat(buf, max, &end, " (Multiple Traps)");
				break;
		}
	}

	return end;
}

static size_t obj_desc_combat(const object_type *o_ptr, char *buf, size_t max,
		size_t end, bool spoil)
{
	object_kind *k_ptr = &k_info[o_ptr->k_idx];
	u32b f1, f2, f3, fn, knownf1, knownf2, knownf3, knownfn;
	object_flags(o_ptr, &f1, &f2, &f3, &fn);

	object_flags_known(o_ptr, &knownf1, &knownf2, &knownf3, &knownfn);

	/* Dump base weapon info */
	switch (o_ptr->tval)
	{
		/* Weapons */
		case TV_SHOT:
		case TV_BOLT:
		case TV_ARROW:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_DIGGING:
		{
			/* Only display the real damage dice if the combat stats are known */
			if ((spoil) || (object_known_p(o_ptr)))
				strnfcat(buf, max, &end, " (%dd%d)", o_ptr->dd, o_ptr->ds);
			else
				strnfcat(buf, max, &end, " (%dd%d)", k_ptr->dd, k_ptr->ds);
			break;
		}

		/* Missile launchers */
		case TV_BOW:
		{
			/* Display shooting power as part of the multiplier */
			if ((f1 & TR1_MIGHT) && (spoil || (knownf1 & TR1_MIGHT)))
				strnfcat(buf, max, &end, " (x%d)", (o_ptr->sval % 10) + o_ptr->pval);
			else
				strnfcat(buf, max, &end, " (x%d)", o_ptr->sval % 10);
			break;
		}
	}


	/* Show weapon bonuses */
	if (spoil || object_known_p(o_ptr))
	{
		if (obj_desc_show_weapon(o_ptr) || o_ptr->to_d || o_ptr->to_h)
		{
			/* Make an exception for body armor with only a to-hit penalty */
			if (o_ptr->to_h < 0 && o_ptr->to_d == 0 &&
			    (o_ptr->tval == TV_SOFT_ARMOR ||
			     o_ptr->tval == TV_HARD_ARMOR ||
			     o_ptr->tval == TV_DRAG_ARMOR ||
			     o_ptr->tval == TV_DRAG_SHIELD))
				strnfcat(buf, max, &end, " (%+d)", o_ptr->to_h);

			/* Otherwise, always use the full tuple */
			else
				strnfcat(buf, max, &end, " (%+d,%+d)", o_ptr->to_h, o_ptr->to_d);
		}
	}


	/* Show armor bonuses */
	if (spoil || object_known_p(o_ptr))
	{
		if (obj_desc_show_armor(o_ptr))
			strnfcat(buf, max, &end, " [%d,%+d]", o_ptr->ac, o_ptr->to_a);
		else if (o_ptr->to_a)
			strnfcat(buf, max, &end, " [%+d]", o_ptr->to_a);
	}
	else if (obj_desc_show_armor(o_ptr))
	{
		strnfcat(buf, max, &end, " [%d]", o_ptr->ac);
	}

	return end;
}

static size_t obj_desc_light(const object_type *o_ptr, char *buf, size_t max, size_t end)
{
	/* Fuelled light sources get number of remaining turns appended */
	if (fuelable_lite_p(o_ptr))
		strnfcat(buf, max, &end, " (%d turns)", o_ptr->timeout);

	return end;
}

static size_t obj_desc_pval(const object_type *o_ptr, char *buf, size_t max, size_t end)
{
	u32b f1, f2, f3, fn;
	object_flags(o_ptr, &f1, &f2, &f3, &fn);

	if (!(f1 & TR1_PVAL_MASK)) return end;

	strnfcat(buf, max, &end, " (%+d", o_ptr->pval);

	if (!(f3 & TR3_HIDE_TYPE))
	{
		if (f1 & TR1_STEALTH)
			strnfcat(buf, max, &end, " stealth");
		else if (f1 & TR1_SEARCH)
			strnfcat(buf, max, &end, " searching");
		else if (f1 & TR1_INFRA)
			strnfcat(buf, max, &end, " infravision");
		else if (f1 & TR1_SPEED)
			strnfcat(buf, max, &end, " speed");
		else if (f1 & TR1_BLOWS)
			strnfcat(buf, max, &end, " attack%s", PLURAL(o_ptr->pval));
	}

	strnfcat(buf, max, &end, ")");

	return end;
}

static size_t obj_desc_charges(const object_type *o_ptr, char *buf, size_t max, size_t end)
{
	object_kind *k_ptr = &k_info[o_ptr->k_idx];

	bool aware = object_flavor_is_aware(o_ptr) || (o_ptr->ident & IDENT_STORE);

	/* See if the object is "known" */
	bool known = (object_known_p(o_ptr) ? TRUE : FALSE);

	/* Wands and Staffs have charges */
	if (aware && known && (o_ptr->tval == TV_STAFF || o_ptr->tval == TV_WAND))
		strnfcat(buf, max, &end, " (%d charge%s)", o_ptr->pval, PLURAL(o_ptr->pval));

	/* Charging things */
	else if (o_ptr->timeout > 0)
	{
		/* Hack -- Rods have a "charging" indicator */
		if (known && (o_ptr->tval == TV_ROD))
		{
			/* Hack -- Dump " (# charging)" if relevant */
			if (o_ptr->timeout >= 1)
			{

				/* Stacks of rods display an exact count of charging rods. */
				if (o_ptr->number > 1)
				{
					int power;

					/* Paranoia. */
					if (k_ptr->pval == 0) k_ptr->pval = 1;

					/* Find out how many rods are charging, by dividing
				 	 * current timeout by each rod's maximum timeout.
				 	 * Ensure that any remainder is rounded up.  Display
				 	 * very discharged stacks as merely fully discharged.
				 	 */
					power = (o_ptr->timeout + (k_ptr->pval - 1)) / k_ptr->pval;

					if (power > o_ptr->number) power = o_ptr->number;

					/* Display prettily */
					strnfcat(buf, max, &end, " (%d charging)", power);
				}


				/* Display prettily */
				else strnfcat(buf, max, &end, " (charging)");
			}
		}


		/* Indicate "charging" objects, but not rods or lites */
		if (known && o_ptr->timeout && o_ptr->tval != TV_ROD
			    && !fuelable_lite_p(o_ptr))
		{

			/* Hack -- Dump " (charging)" if relevant */
			strnfcat(buf, max, &end, " (charging)");
		}
	}

	return end;
}

static size_t obj_desc_inscrip(const object_type *o_ptr, char *buf, size_t max, size_t end)
{
	const char *u[6] = { 0, 0, 0, 0, 0, 0};
	int n = 0;

	/* See if the object is "known" */
	bool known = (object_known_p(o_ptr) ? TRUE : FALSE);
	bool aware = (object_aware_p(o_ptr) ? TRUE : FALSE);

	u32b f1, f2, f3, fn;
	object_flags(o_ptr, &f1, &f2, &f3, &fn);

	/* Get inscription, pdeudo-id, or store discount */
	if (o_ptr->obj_note) u[n++] = quark_str(o_ptr->obj_note);
	if (o_ptr->discount >= INSCRIP_NULL)
	{
		u[n++] = inscrip_text[o_ptr->discount - INSCRIP_NULL];
	}
	else if (cursed_p(o_ptr) && known)
	{
		u[n++] = "cursed";
	}
	else if ((o_ptr->ident & IDENT_EMPTY) && (!known))
	{
		u[n++] = "empty";
	}
	else if ((!aware) && object_tried_p(o_ptr))
	{
		u[n++] = "tried";
	}
	else if (o_ptr->discount > 0)
	{
		char buf[80];
		my_strcpy(buf, format("%d pct off", o_ptr->discount), sizeof(buf));

		u[n++] = buf;
	}

	/* Use the "unknown" inscription */
	else if (!known && can_be_pseudo_ided(o_ptr) &&
			(o_ptr->discount < INSCRIP_NULL))
	{
		u[n++] = "unknown";
	}

	if (n)
	{
		int i;
		for (i = 0; i < n; i++)
		{
			if (i == 0)
				strnfcat(buf, max, &end, " {");
			strnfcat(buf, max, &end, "%s", u[i]);
			if (i < n-1)
				strnfcat(buf, max, &end, ", ");
		}

		strnfcat(buf, max, &end, "}");
	}

	return end;
}



/*
 * Creates a description of the item "o_ptr", and stores it in "buf".
 *
 * The given "buf" should be at least 80 chars long to hold the longest
 * possible description, which can get pretty long, including inscriptions,
 * such as:
 * "no more Maces of Disruption (Defender) (+10,+10) [+5] (+3 to stealth)".
 *
 * Describes item `o_ptr` into buffer `buf` of size `max`.
 *
 * ODESC_PREFIX prepends a 'the', 'a' or number
 * ODESC_BASE results in a base description.
 * ODESC_COMBAT will add to-hit, to-dam and AC info.
 * ODESC_EXTRA will add pval/charge/inscription/squelch info.
 * ODESC_PLURAL will pluralise regardless of the number in the stack.
 * ODESC_STORE turns off squelch markers, for in-store display.
 * ODESC_SPOIL treats the object as fully identified.
 *
 * Setting 'prefix' to TRUE prepends a 'the', 'a' or the number in the stack,
 * respectively.
 *
 * \returns The number of bytes used of the buffer.
 */
size_t object_desc(char *buf, size_t max, const object_type *o_ptr, byte mode)
{
	bool prefix = mode & ODESC_PREFIX;
	bool spoil = (mode & ODESC_SPOIL);

	size_t end = 0;

	bool aware;
	bool known;

	u32b f1, f2, f3, fn;

	object_kind *k_ptr = &k_info[o_ptr->k_idx];

	/* Extract some flags */
	object_flags(o_ptr, &f1, &f2, &f3, &fn);

	/* See if the object is "aware" */
	aware = (object_aware_p(o_ptr) ? TRUE : FALSE);

	/* See if the object is "known" */
	known = (object_known_p(o_ptr) ? TRUE : FALSE);

	/* Object is in the inventory of a store */
	if (o_ptr->ident & IDENT_STORE)
	{

		/* Pretend known and aware */
		aware = TRUE;
		known = TRUE;
	}

	/* Player has now seen the item
	 *
	 * This code must be exactly here to properly handle objects in
	 * stores (fake assignment to "aware", see above) and unaware objects
	 * in the dungeon.
	 */
	if (aware) k_ptr->everseen = TRUE;

	/* We've seen it at least once now we're aware of it */
	if (known && o_ptr->ego_num) e_info[o_ptr->ego_num].everseen = TRUE;

	/*** Some things get really simple descriptions ***/

	if (o_ptr->tval == TV_GOLD)
	{
		return strnfmt(buf, max, "%d gold pieces worth of %s",
				o_ptr->pval, k_name + k_ptr->name);
	}
	else if (!o_ptr->tval)
	{
		return strnfmt(buf, max, "(nothing)");
	}

	/* Copy the base name to the buffer */
	end = obj_desc_name(buf, max, end, o_ptr, prefix, mode, spoil);

	if (mode & ODESC_COMBAT)
	{
		if (o_ptr->tval == TV_CHEST)
			end = obj_desc_chest(o_ptr, buf, max, end);
		else if (o_ptr->tval == TV_LIGHT)
			end = obj_desc_light(o_ptr, buf, max, end);
		end = obj_desc_combat(o_ptr, buf, max, end, spoil);
	}

	if (mode & ODESC_EXTRA)
	{
		if (spoil || known)
			end = obj_desc_pval(o_ptr, buf, max, end);

		end = obj_desc_charges(o_ptr, buf, max, end);

		end = obj_desc_inscrip(o_ptr, buf, max, end);
	}

	return end;

}

/*
 * Describe an item and pretend the item is fully known and has no flavor.
 */
void object_desc_spoil(char *buf, size_t max, const object_type *o_ptr, int pref, int mode)
{
	object_type object_type_body;
	object_type *i_ptr = &object_type_body;

	/* Make a backup */
	object_copy(i_ptr, o_ptr);

	/* HACK - Pretend the object is in a store inventory */
	i_ptr->ident |= IDENT_STORE;

	/* Describe */
	object_desc(buf, max, i_ptr, ODESC_FULL | ODESC_SPOIL);
}


/*
 * Describe an item's random attributes for "character dumps"
 */
void identify_random_gen(const object_type *o_ptr)
{
	/* Set hooks for character dump */
	object_info_out_flags = object_flags_known;

	/* Set the indent/wrap */
	text_out_indent = 3;
	text_out_wrap = 65;

	/* Dump the info */
	if (object_info_out(o_ptr, FALSE))
		text_out("\n");

	/* Dump object history if necessary */
	if (history_interesting(o_ptr))
	{
		char buf[200];

		/* Get the history and dump it */
		if (format_object_history(buf, sizeof(buf), o_ptr))
		{
			text_out(buf);

		       	text_out("\n");
		}
	}

	/* Reset indent/wrap */
	text_out_indent = 0;
	text_out_wrap = 0;
}



