/* File: borg3.c */

/*
 * Copyright (c) 1997 Ben Harrison
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.
 */

#include "angband.h"

#include "borg.h"


#ifdef ALLOW_BORG


/*
 * This file helps the Borg analyze "objects" and "shops", and to
 * deal with objects and spells.
 */



/*
 * Hack -- single character constants
 */

static const char p1 = '(', p2 = ')';
static const char c1 = '{', c2 = '}';
static const char b1 = '[', b2 = ']';



/*
 * Return the slot that items of the given type are wielded into
 *
 * Note that "rings" are now automatically wielded into the left hand
 *
 * Returns "-1" if the item cannot (or should not) be wielded
 */
int borg_wield_slot(auto_item *item)
{
	/* Slot for equipment */
	switch (item->tval)
	{
		case TV_SWORD:
		case TV_POLEARM:
		case TV_HAFTED:
		case TV_DIGGING:
		{
			return (INVEN_WIELD);
		}

		case TV_BOW:
		{
			return (INVEN_BOW);
		}

		case TV_RING:
		{
			return (INVEN_LEFT);
		}

		case TV_AMULET:
		{
			return (INVEN_NECK);
		}

		case TV_LITE:
		{
			return (INVEN_LITE);
		}

		case TV_DRAG_ARMOR:
		case TV_HARD_ARMOR:
		case TV_SOFT_ARMOR:
		{
			return (INVEN_BODY);
		}

		case TV_CLOAK:
		{
			return (INVEN_OUTER);
		}

		case TV_SHIELD:
		{
			return (INVEN_ARM);
		}

		case TV_CROWN:
		case TV_HELM:
		{
			return (INVEN_HEAD);
		}

		case TV_GLOVES:
		{
			return (INVEN_HANDS);
		}

		case TV_BOOTS:
		{
			return (INVEN_FEET);
		}
	}

	/* No slot available */
	return (-1);
}




/*
 * Determine the "base price" of a known item (see below)
 *
 * This function is adapted from "object_value_known()".
 *
 * This routine is called only by "borg_item_analyze()", which
 * uses this function to guess at the "value" of an item, if it
 * was to be sold to a store, with perfect "charisma" modifiers.
 */
static s32b borg_object_value_known(auto_item *item)
{
	s32b value;


	object_kind *k_ptr = &k_info[item->kind];

	/* Worthless items */
	if (!k_ptr->cost) return (0L);

	/* Extract the base value */
	value = k_ptr->cost;


	/* Hack -- use artifact base costs */
	if (item->name1)
	{
		artifact_type *a_ptr = &a_info[item->name1];

		/* Worthless artifacts */
		if (!a_ptr->cost) return (0L);

		/* Hack -- use the artifact cost */
		value = a_ptr->cost;
	}

	/* Hack -- add in ego-item bonus cost */
	if (item->name2)
	{
		ego_item_type *e_ptr = &e_info[item->name2];

		/* Worthless ego-items */
		if (!e_ptr->cost) return (0L);

		/* Hack -- reward the ego-item cost */
		value += e_ptr->cost;
	}


	/* Analyze pval bonus */
	switch (item->tval)
	{
		/* Wands/Staffs */
		case TV_WAND:
		case TV_STAFF:
		{
			/* Pay extra for charges */
			value += ((value / 20) * item->pval);
	
			break;
		}

		/* Wearable items */
		case TV_SHOT:
		case TV_ARROW:
		case TV_BOLT:
		case TV_BOW:
		case TV_DIGGING:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_BOOTS:
		case TV_GLOVES:
		case TV_HELM:
		case TV_CROWN:
		case TV_SHIELD:
		case TV_CLOAK:
		case TV_SOFT_ARMOR:
		case TV_HARD_ARMOR:
		case TV_DRAG_ARMOR:
		case TV_LITE:
		case TV_AMULET:
		case TV_RING:
		{
			/* Hack -- Negative "pval" is always bad */
			if (item->pval < 0) return (0L);
	
			/* No pval */
			if (!item->pval) break;
	
			/* Give credit for stat bonuses */
			if (item->flags1 & TR1_STR) value += (item->pval * 200L);
			if (item->flags1 & TR1_INT) value += (item->pval * 200L);
			if (item->flags1 & TR1_WIS) value += (item->pval * 200L);
			if (item->flags1 & TR1_DEX) value += (item->pval * 200L);
			if (item->flags1 & TR1_CON) value += (item->pval * 200L);
			if (item->flags1 & TR1_CHR) value += (item->pval * 200L);
	
			/* Give credit for stealth and searching */
			if (item->flags1 & TR1_STEALTH) value += (item->pval * 100L);
			if (item->flags1 & TR1_SEARCH) value += (item->pval * 100L);
	
			/* Give credit for infra-vision and tunneling */
			if (item->flags1 & TR1_INFRA) value += (item->pval * 50L);
			if (item->flags1 & TR1_TUNNEL) value += (item->pval * 50L);
	
			/* Give credit for extra attacks */
			if (item->flags1 & TR1_BLOWS) value += (item->pval * 2000L);
	
			/* Give credit for speed bonus */
			if (item->flags1 & TR1_SPEED) value += (item->pval * 30000L);
	
			break;
		}
	}


	/* Analyze the item */
	switch (item->tval)
	{
		/* Rings/Amulets */
		case TV_RING:
		case TV_AMULET:
		{
			/* Hack -- negative bonuses are bad */
			if (item->to_a < 0) return (0L);
			if (item->to_h < 0) return (0L);
			if (item->to_d < 0) return (0L);
	
			/* Give credit for bonuses */
			value += ((item->to_h + item->to_d + item->to_a) * 100L);
	
			break;
		}

		/* Armor */
		case TV_BOOTS:
		case TV_GLOVES:
		case TV_CLOAK:
		case TV_CROWN:
		case TV_HELM:
		case TV_SHIELD:
		case TV_SOFT_ARMOR:
		case TV_HARD_ARMOR:
		case TV_DRAG_ARMOR:
		{
			/* Hack -- negative armor bonus */
			if (item->to_a < 0) return (0L);
	
			/* Give credit for bonuses */
			value += ((item->to_h + item->to_d + item->to_a) * 100L);
	
			break;
		}

		/* Bows/Weapons */
		case TV_BOW:
		case TV_DIGGING:
		case TV_HAFTED:
		case TV_SWORD:
		case TV_POLEARM:
		{
			/* Hack -- negative hit/damage bonuses */
			if (item->to_h + item->to_d < 0) return (0L);
	
			/* Factor in the bonuses */
			value += ((item->to_h + item->to_d + item->to_a) * 100L);
	
			/* Hack -- Factor in extra damage dice */
			if ((item->dd > k_ptr->dd) && (item->ds == k_ptr->ds))
			{
				value += (item->dd - k_ptr->dd) * item->ds * 200L;
			}
	
			break;
		}

		/* Ammo */
		case TV_SHOT:
		case TV_ARROW:
		case TV_BOLT:
		{
			/* Hack -- negative hit/damage bonuses */
			if (item->to_h + item->to_d < 0) return (0L);
	
			/* Factor in the bonuses */
			value += ((item->to_h + item->to_d) * 5L);
	
			/* Hack -- Factor in extra damage dice */
			if ((item->dd > k_ptr->dd) && (item->ds == k_ptr->ds))
			{
				value += (item->dd - k_ptr->dd) * item->ds * 5L;
			}
	
			break;
		}
	}


	/* Return the value */
	return (value);
}



/*
 * Analyze an item given a description
 *
 * From the description, extract the item identity, and the various
 * bonuses, plus the "aware" and "known" flags (in an encoded state),
 * plus the object flags, taking into account any inscriptions.
 *
 * Note the use of a "prefix binary search" on the arrays of object
 * base names, and on the arrays of artifact/ego-item special names.
 *
 * A "prefix binary search" starts out just like a normal binary search,
 * in that it searches a sorted array of entries for a specific entry,
 * using a simple "less than or equal to" comparison.  When it finds
 * an entry, however, instead of simply checking for "equality" of the
 * entry to the key, it must check whether the key is a "prefix" of the
 * entry.  And if any entry can be a prefix of another entry, then it
 * must check whether the key is a "prefix" of any of the entries which
 * precede the "found" entry.  Technically, it only has to check the
 * preceding N entries, where N is the maximum distance between any two
 * entries sharing a "prefix" relation, but note that only in the case
 * of "failure" will the search need to check more than a few entries,
 * even if it scans all the way to the start of the list.
 *
 * We use the object kind to guess at the object weight and flags, and
 * then we use the artifact/ego-item information to update our guesses.
 *
 * We also guess at the value of the item, as given by "object_value()".
 *
 * Note that we will fail if the "description" was "partial", that is,
 * if it was "broken" by the display functions for any reason.  This
 * should only be an issue in "stores", which "chop" the description
 * to a length of about 60 characters, which may be "messy".  Luckily,
 * objects in stores never have important inscriptions, and we should
 * correctly handle objects with "bizarre" inscriptions, or even with
 * "broken" inscriptions, so we should be okay.
 *
 * In theory, the Borg could be taught to use "*identify*" on certain
 * objects, and to use the result to inscribe the object with special
 * "BORG_xxx" inscriptions describing the special flags, if any, on
 * the object.  It may be necessary to have a special inscription to
 * indicate that no such special flags were discovered.  XXX XXX XXX
 */
void borg_item_analyze(auto_item *item, cptr desc)
{
	int i, m, n;

	int d1 = 0;
	int d2 = 0;
	int ac = 0;
	int th = 0;
	int td = 0;
	int ta = 0;

	bool done = FALSE;

	char *scan;
	char *tail;

	char buf[128];


	/* Wipe the item */
	WIPE(item, auto_item);

	/* Save the item description */
	strcpy(item->desc, desc);

	/* Advance to the "inscription", or the end of the description */
	for (scan = item->desc; *scan && (*scan != c1); scan++) /* loop */;

	/* Save inscription */
	item->note = scan;

	/* Empty item */
	if (!desc[0]) return;


	/* Assume singular */
	item->iqty = 1;

	/* Notice prefix "a " */
	if ((desc[0] == 'a') && (desc[1] == ' '))
	{
		/* Skip "a " */
		desc += 2;
	}

	/* Notice prefix "a " */
	else if ((desc[0] == 'a') && (desc[1] == 'n') && (desc[2] == ' '))
	{
		/* Skip "an " */
		desc += 3;
	}

	/* Notice prefix "The " */
	else if ((desc[0] == 'T') && (desc[1] == 'h') &&
	         (desc[2] == 'e') && (desc[3] == ' '))
	{
		/* Skip "The " */
		desc += 4;
	}

	/* Notice "numerical" prefixes */
	else if (isdigit(desc[0]))
	{
		cptr s;

		/* Find the first space */
		for (s = desc; *s && (*s != ' '); s++) /* loop */;

		/* Paranoia XXX XXX */
		if (*s != ' ') return;

		/* Extract a quantity */
		item->iqty = atoi(desc);

		/* Skip the quantity and space */
		desc = s + 1;
	}


	/* Paranoia XXX XXX */
	if (!desc[0]) return;


	/* Obtain a copy of the description */
	strcpy(buf, desc);

	/* Advance to the "inscription" or end of string */
	for (scan = buf; *scan && (*scan != c1); scan++) /* loop */;

	/* Nuke the space before the inscription */
	if ((scan[0] == c1) && (scan[-1] == ' ')) *--scan = '\0';

	/* Note that "scan" points at the "tail" of "buf" */

	/* Hack -- non-aware, singular, flavored items */
	if (item->iqty == 1)
	{
		if (prefix(buf, "Scroll titled ")) item->tval = TV_SCROLL;
		else if (streq(scan-7, " Potion")) item->tval = TV_POTION;
		else if (streq(scan-6, " Staff")) item->tval = TV_STAFF;
		else if (streq(scan-5, " Wand")) item->tval = TV_WAND;
		else if (streq(scan-4, " Rod")) item->tval = TV_ROD;
		else if (streq(scan-5, " Ring")) item->tval = TV_RING;
		else if (streq(scan-7, " Amulet")) item->tval = TV_AMULET;
		else if (streq(scan-9, " Mushroom")) item->tval = TV_FOOD;
	}

	/* Hack -- non-aware, plural, flavored items */
	else
	{
		if (prefix(buf, "Scrolls titled ")) item->tval = TV_SCROLL;
		else if (streq(scan-8, " Potions")) item->tval = TV_POTION;
		else if (streq(scan-7, " Staffs")) item->tval = TV_STAFF;
		else if (streq(scan-6, " Wands")) item->tval = TV_WAND;
		else if (streq(scan-5, " Rods")) item->tval = TV_ROD;
		else if (streq(scan-6, " Rings")) item->tval = TV_RING;
		else if (streq(scan-8, " Amulets")) item->tval = TV_AMULET;
		else if (streq(scan-10, " Mushrooms")) item->tval = TV_FOOD;
	}

	/* Accept non-aware flavored objects */
	if (item->tval)
	{
		/* Guess at weight and cost */
		switch (item->tval)
		{
			case TV_FOOD:
			{
				item->weight = 1;
				item->value = 5L;
				break;
			}

			case TV_POTION:
			{
				item->weight = 4;
				item->value = 20L;
				break;
			}

			case TV_SCROLL:
			{
				item->weight = 5;
				item->value = 20L;
				break;
			}

			case TV_STAFF:
			{
				item->weight = 50;
				item->value = 70L;
				break;
			}

			case TV_WAND:
			{
				item->weight = 10;
				item->value = 50L;
				break;
			}

			case TV_ROD:
			{
				item->weight = 15;
				item->value = 90L;
				break;
			}

			case TV_RING:
			{
				item->weight = 2;
				item->value = 45L;
				break;
			}

			case TV_AMULET:
			{
				item->weight = 3;
				item->value = 45L;
				break;
			}
		}

		/* Done XXX XXX */
		return;
	}


	/* Start at the beginning */
	tail = buf;

	/* Check singular items */
	if (item->iqty == 1)
	{
		/* Start the search */
		m = 0; n = ab_ptr->single_num;

		/* Simple binary search */
		while (m < n - 1)
		{
			/* Pick a "middle" entry */
			i = (m + n) / 2;

			/* Search to the right (or here) */
			if (strcmp(ab_ptr->single_text[i], tail) <= 0)
			{
				m = i;
			}

			/* Search to the left */
			else
			{
				n = i;
			}
		}

		/* Check prefixes XXX */
		for (i = m; i >= 0; i--)
		{
			/* Verify prefix */
			if (prefix(tail, ab_ptr->single_text[i]))
			{
				/* Save the item kind */
				item->kind = ab_ptr->single_what[i];
		
				/* Skip past the base name */
				tail += strlen(ab_ptr->single_text[i]);

				/* Done */
				break;
			}
		}
	}

	/* Check plural items */
	else
	{
		/* Start the search */
		m = 0; n = ab_ptr->plural_num;

		/* Simple binary search */
		while (m < n - 1)
		{
			/* Pick a "middle" entry */
			i = (m + n) / 2;

			/* Search to the right (or here) */
			if (strcmp(ab_ptr->plural_text[i], tail) <= 0)
			{
				m = i;
			}

			/* Search to the left */
			else
			{
				n = i;
			}
		}

		/* Check prefixes XXX */
		for (i = m; i >= 0; i--)
		{
			/* Verify prefix */
			if (prefix(tail, ab_ptr->plural_text[i]))
			{
				/* Save the item kind */
				item->kind = ab_ptr->plural_what[i];
		
				/* Skip past the base name */
				tail += strlen(ab_ptr->plural_text[i]);

				/* Done */
				break;
			}
		}
	}


	/* Oops XXX XXX */
	if (!item->kind)
	{
		borg_oops("bizarre object");
		return;
	}


	/* Extract some info */
	item->tval = k_info[item->kind].tval;
	item->sval = k_info[item->kind].sval;

	/* Guess at the weight */
	item->weight = k_info[item->kind].weight;

	/* Extract the base flags */
	item->flags1 = k_info[item->kind].flags1;
	item->flags2 = k_info[item->kind].flags2;
	item->flags3 = k_info[item->kind].flags3;


	/* Analyze "bonuses" */
	switch (item->tval)
	{
		/* Basic items */
		case TV_MAGIC_BOOK:
		case TV_PRAYER_BOOK:
		case TV_FLASK:
		case TV_FOOD:
		case TV_POTION:
		case TV_SCROLL:
		case TV_SPIKE:
		case TV_SKELETON:
		case TV_BOTTLE:
		case TV_JUNK:
		{
			/* Always "able" */
			item->able = TRUE;
	
			break;
		}

		/* Chests */
		case TV_CHEST:
		{
			/* XXX XXX XXX */
	
			/* Require the prefix and suffix */
			if (!prefix(tail, " (")) break;
			if (!suffix(tail, ")")) break;
	
			/* Assume "able" */
			item->able = TRUE;
	
			/* Hack -- assume "trapped" */
			item->pval = 63;
	
			/* Hack -- extract "empty" */
			if (streq(tail, " (empty)")) item->pval = 0;
	
			break;
		}

		/* Wands/Staffs -- charges */
		case TV_WAND:
		case TV_STAFF:
		{
			/* Assume a single charge XXX XXX XXX */
			item->pval = 1;

			/* Require the prefix and suffix */
			if (!prefix(tail, " (")) break; /* --(-- */
			if (!suffix(tail, " charge)") && !suffix(tail, " charges)")) break;
	
			/* Extract the "charges" */
			item->pval = atoi(tail+2);
	
			/* Assume "able" */
			item->able = TRUE;
	
			break;
		}

		/* Rods -- charging */
		case TV_ROD:
		{
			/* Always "able" */
			item->able = TRUE;
	
			/* Mega-Hack -- fake "charges" */
			item->pval = 1;
	
			/* Mega-Hack -- "charging" means no "charges" */
			if (streq(tail, " (charging)")) item->pval = 0;
	
			break;
		}

		/* Wearable items */
		case TV_SHOT:
		case TV_ARROW:
		case TV_BOLT:
		case TV_BOW:
		case TV_DIGGING:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_BOOTS:
		case TV_GLOVES:
		case TV_HELM:
		case TV_CROWN:
		case TV_SHIELD:
		case TV_CLOAK:
		case TV_SOFT_ARMOR:
		case TV_HARD_ARMOR:
		case TV_DRAG_ARMOR:
		case TV_LITE:
		case TV_AMULET:
		case TV_RING:
		{
			/* Hack -- handle "easy know" */
			if (k_info[item->kind].flags3 & TR3_EASY_KNOW)
			{
				/* Always known */
				item->able = TRUE;
			}
	
			/* No suffix */
			if (tail[0] != ' ') break;
	
			/* Start the search */
			m = 0; n = ab_ptr->artego_num;
	
			/* Binary search */
			while (m < n - 1)
			{
				/* Pick a "middle" entry */
				i = (m + n) / 2;
	
				/* Search to the right (or here) */
				if (strcmp(ab_ptr->artego_text[i], tail) <= 0)
				{
					m = i;
				}
	
				/* Search to the left */
				else
				{
					n = i;
				}
			}
	
			/* Check prefixes XXX */
			for (i = m; i >= 0; i--)
			{
				/* Verify prefix */
				if (prefix(tail, ab_ptr->artego_text[i]))
				{
					/* Paranoia */
					item->able = TRUE;
		
					/* Save the artifact name */
					if (ab_ptr->artego_what[i] < 256)
					{
						item->name1 = ab_ptr->artego_what[i];
					}
	
					/* Save the ego-item name */
					else
					{
						item->name2 = ab_ptr->artego_what[i] - 256;
					}
	
					/* Skip the artifact/ego-item name */
					tail += strlen(ab_ptr->artego_text[i]);
	
					/* Done */
					break;
				}
			}
	
			/* Hack -- grab "charging" suffix */
			if (suffix(tail, " (charging)"))
			{
				/* Remove the suffix */
				tail[strlen(tail)-11] = '\0';
	
				/* Remember it */
				item->timeout = 999;
			}
	
			/* Hack -- handle Lite's */
			if (item->tval == TV_LITE)
			{
				/* Torch or Lantern */
				if ((item->sval == SV_LITE_TORCH) ||
				    (item->sval == SV_LITE_LANTERN))
				{
					/* Parse turns of light */
					if (prefix(tail, " (with ") &&
					    suffix(tail, " of light)"))
					{
						/* Extract "turns of lite" */
						item->pval = atoi(tail+7);
			
						/* Assume "able" */
						item->able = TRUE;
					}	
				}
				
				/* Hack -- Artifact */
				else
				{
					/* Assume "able" */
					/* item->able = TRUE; */
	
					/* Hack -- huge fuel */
					item->pval = 29999;
				}
	
				break;
			}
	
			/* Hack -- Skip spaces */
			while (tail[0] == ' ') tail++;
	
			/* No suffix */
			if (!tail[0]) break;
	
			/* Parse "weapon-style" damage strings */
			if ((tail[0] == p1) &&
			    ((item->tval == TV_HAFTED) ||
			     (item->tval == TV_POLEARM) ||
			     (item->tval == TV_SWORD) ||
			     (item->tval == TV_DIGGING) ||
			     (item->tval == TV_BOLT) ||
			     (item->tval == TV_ARROW) ||
			     (item->tval == TV_SHOT)))
			{
				/* First extract the damage string */
				for (scan = tail; *scan != p2; scan++) /* loop */;
				scan++;
	
				/* Hack -- Notice "end of string" */
				if (scan[0] != ' ') done = TRUE;
	
				/* Terminate the string and advance */
				*scan++ = '\0';
	
				/* Parse the damage string, or stop XXX */
				if (sscanf(tail, "(%dd%d)", &d1, &d2) != 2) break;
	
				/* Save the values */
				item->dd = d1;
				item->ds = d2;
	
				/* No extra information means not identified */
				if (done) break;
	
				/* Skip the "damage" info */
				tail = scan;
			}
	
			/* Parse the "damage" string for bows */
			else if ((tail[0] == p1) &&
			         (item->tval == TV_BOW))
			{
				/* First extract the damage string */
				for (scan = tail; *scan != p2; scan++) /* loop */;
				scan++;
	
				/* Hack -- Notice "end of string" */
				if (scan[0] != ' ') done = TRUE;
	
				/* Terminate the string and advance */
				*scan++ = '\0';
	
				/* Parse the multiplier string, or stop */
				if (sscanf(tail, "(x%d)", &d1) != 1) break;
	
				/* No extra information means not identified */
				if (done) break;
	
				/* Skip the "damage" info */
				tail = scan;
			}
	
	
			/* Parse the "bonus" string */
			if (tail[0] == p1)
			{
				/* Extract the extra info */
				for (scan = tail; *scan != p2; scan++) /* loop */;
				scan++;
	
				/* Hack -- Notice "end of string" */
				if (scan[0] != ' ') done = TRUE;
	
				/* Terminate the damage, advance */
				*scan++ = '\0';
	
				/* Parse standard "bonuses" */
				if (sscanf(tail, "(%d,%d)", &th, &td) == 2)
				{
					item->to_h = th;
					item->to_d = td;
					item->able = TRUE;
				}
	
				/* XXX XXX Hack -- assume non-final bonuses are "to_hit" */
				else if (!done && sscanf(tail, "(%d)", &th) == 1)
				{
					item->to_h = th;
					item->able = TRUE;
				}
	
				/* XXX XXX Hack -- assume final bonuses are "pval" codes */
				else if (done)
				{
					item->pval = atoi(tail + 1);
					item->able = TRUE;
				}
	
				/* Oops */
				else
				{
					break;
				}
	
				/* Nothing left */
				if (done) break;
	
				/* Skip the "damage bonus" info */
				tail = scan;
			}
	
	
			/* Parse the "bonus" string */
			if (tail[0] == b1)
			{
				/* Extract the extra info */
				for (scan = tail; *scan != b2; scan++) /* loop */;
				scan++;
	
				/* Hack -- Notice "end of string" */
				if (scan[0] != ' ') done = TRUE;
	
				/* Terminate the armor string, advance */
				*scan++ = '\0';
	
				/* Parse the armor, and bonus */
				if (sscanf(tail, "[%d,%d]", &ac, &ta) == 2)
				{
					item->ac = ac;
					item->to_a = ta;
					item->able = TRUE;
				}
	
				/* Negative armor bonus */
				else if (sscanf(tail, "[-%d]", &ta) == 1)
				{
					item->to_a = -ta;
					item->able = TRUE;
				}
	
				/* Positive armor bonus */
				else if (sscanf(tail, "[+%d]", &ta) == 1)
				{
					item->to_a = ta;
					item->able = TRUE;
				}
	
				/* Just base armor */
				else if (sscanf(tail, "[%d]", &ac) == 1)
				{
					item->ac = ac;
				}
	
				/* Oops */
				else
				{
					break;
				}
	
				/* Nothing left */
				if (done) break;
	
				/* Skip the "armor" data */
				tail = scan;
			}
	
	
			/* Parse the final "pval" string, if any */
			if (tail[0] == p1)
			{
				/* Assume identified */
				item->able = TRUE;
	
				/* Hack -- Grab it */
				item->pval = atoi(tail + 1);
			}
	
			break;
		}
	}


	/* Hack -- repair rings of damage */
	if ((item->tval == TV_RING) && (item->sval == SV_RING_DAMAGE))
	{
		/* Bonus to dam, not pval */
		item->to_d = item->pval;
		item->pval = 0;
	}

	/* Hack -- repair rings of accuracy */
	if ((item->tval == TV_RING) && (item->sval == SV_RING_ACCURACY))
	{
		/* Bonus to hit, not pval */
		item->to_h = item->pval;
		item->pval = 0;
	}


	/* XXX XXX XXX Repair various "ego-items" */


	/* Hack -- examine artifacts */
	if (item->name1)
	{
		/* XXX XXX Hack -- fix "weird" artifacts */
		if ((item->tval != a_info[item->name1].tval) ||
		    (item->sval != a_info[item->name1].sval))
		{
			/* Save the kind */
			item->kind = lookup_kind(item->tval, item->sval);

			/* Save the tval/sval */
			item->tval = k_info[item->kind].tval;
			item->sval = k_info[item->kind].sval;
		}

		/* Extract the weight */
		item->weight = a_info[item->name1].weight;

		/* Extract the artifact flags */
		item->flags1 = a_info[item->name1].flags1;
		item->flags2 = a_info[item->name1].flags2;
		item->flags3 = a_info[item->name1].flags3;
	}


	/* Hack -- examine ego-items */
	if (item->name2)
	{
		/* XXX Extract the weight */

		/* Extract the ego-item flags */
		item->flags1 |= e_info[item->name2].flags1;
		item->flags2 |= e_info[item->name2].flags2;
		item->flags3 |= e_info[item->name2].flags3;
	}


	/* Known items */
	if (item->able)
	{
		/* Process various fields */
		item->value = borg_object_value_known(item);
	}

	/* Aware items */
	else
	{
		/* Aware items can assume template cost */
		item->value = k_info[item->kind].cost;
	}


	/* Special ego-items */
	switch (item->name2)
	{
		case EGO_HA:
		case EGO_DF:
		case EGO_BLESS_BLADE:
		case EGO_PERMANENCE:
		case EGO_ELVENKIND:
		case EGO_MAGI:
		case EGO_AMAN:
		{
			/* Use "*identify*" */
			item->do_star = TRUE;
			break;
		}
	}


	/* No inscription XXX XXX */
	if (!item->note[0]) return;

	/* Ignore open curly */
	strcpy(buf, item->note+1);

	/* Verify end of inscription XXX XXX */
	if (buf[strlen(buf)-1] != c2) return;

	/* Remove close curly */
	buf[strlen(buf)-1] = '\0';


	/* Self-induced inscriptions */
	if (prefix(buf, "BORG_"))
	{
		/* Assume "*identified*" */
		item->do_star = FALSE;

		/* Now "examine" it */
		if (streq(buf+5, "*"))
		{
			/* Use "examine" */
			item->do_exam = TRUE;
		}

		/* Resist (flags2) */
		else if (prefix(buf+5, "RES_"))
		{
			if (streq(buf+5, "RES_ACID")) item->flags2 |= (TR2_RES_ACID);
			else if (streq(buf+5, "RES_ELEC")) item->flags2 |= (TR2_RES_ELEC);
			else if (streq(buf+5, "RES_FIRE")) item->flags2 |= (TR2_RES_FIRE);
			else if (streq(buf+5, "RES_COLD")) item->flags2 |= (TR2_RES_COLD);
			else if (streq(buf+5, "RES_POIS")) item->flags2 |= (TR2_RES_POIS);
			else if (streq(buf+5, "RES_FEAR")) item->flags2 |= (TR2_RES_FEAR);
			else if (streq(buf+5, "RES_LITE")) item->flags2 |= (TR2_RES_LITE);
			else if (streq(buf+5, "RES_DARK")) item->flags2 |= (TR2_RES_DARK);
			else if (streq(buf+5, "RES_BLIND")) item->flags2 |= (TR2_RES_BLIND);
			else if (streq(buf+5, "RES_CONFU")) item->flags2 |= (TR2_RES_CONFU);
			else if (streq(buf+5, "RES_SOUND")) item->flags2 |= (TR2_RES_SOUND);
			else if (streq(buf+5, "RES_SHARD")) item->flags2 |= (TR2_RES_SHARD);
			else if (streq(buf+5, "RES_NEXUS")) item->flags2 |= (TR2_RES_NEXUS);
			else if (streq(buf+5, "RES_NETHR")) item->flags2 |= (TR2_RES_NETHR);
			else if (streq(buf+5, "RES_CHAOS")) item->flags2 |= (TR2_RES_CHAOS);
			else if (streq(buf+5, "RES_DISEN")) item->flags2 |= (TR2_RES_DISEN);
		}
		
		/* Sustain (flags2) */
		else if (prefix(buf+5, "SUST_"))
		{
			if (streq(buf+5, "SUST_STR")) item->flags2 |= (TR2_SUST_STR);
			else if (streq(buf+5, "SUST_INT")) item->flags2 |= (TR2_SUST_INT);
			else if (streq(buf+5, "SUST_WIS")) item->flags2 |= (TR2_SUST_WIS);
			else if (streq(buf+5, "SUST_DEX")) item->flags2 |= (TR2_SUST_DEX);
			else if (streq(buf+5, "SUST_CON")) item->flags2 |= (TR2_SUST_CON);
			else if (streq(buf+5, "SUST_CHR")) item->flags2 |= (TR2_SUST_CHR);
		}

		/* Misc (flags3) */
		else
		{
			if (streq(buf+5, "SLOW_DIGEST")) item->flags3 |= (TR3_SLOW_DIGEST);
			else if (streq(buf+5, "FEATHER")) item->flags3 |= (TR3_FEATHER);
			else if (streq(buf+5, "LITE")) item->flags3 |= (TR3_LITE);
			else if (streq(buf+5, "REGEN")) item->flags3 |= (TR3_REGEN);
			else if (streq(buf+5, "TELEPATHY")) item->flags3 |= (TR3_TELEPATHY);
			else if (streq(buf+5, "SEE_INVIS")) item->flags3 |= (TR3_SEE_INVIS);
			else if (streq(buf+5, "FREE_ACT")) item->flags3 |= (TR3_FREE_ACT);
			else if (streq(buf+5, "HOLD_LIFE")) item->flags3 |= (TR3_HOLD_LIFE);
		}
	}
	
	/* Game-induced inscriptions */
	else
	{
		/* Special "discount" */
		if (streq(buf, "on sale"))
		{
			item->discount = 50;
			item->value -= item->value * 50 / 100;
		}

		/* Standard "discounts" */
		else if (streq(buf, "25% off"))
		{
			item->discount = 25;
			item->value -= item->value * 25 / 100;
		}
		else if (streq(buf, "50% off"))
		{
			item->discount = 50;
			item->value -= item->value * 50 / 100;
		}
		else if (streq(buf, "75% off"))
		{
			item->discount = 75;
			item->value -= item->value * 75 / 100;
		}
		else if (streq(buf, "90% off"))
		{
			item->discount = 90;
			item->value -= item->value * 90 / 100;
		}

		/* Standard "notations" */
		else if (streq(buf, "empty"))
		{
			item->empty = TRUE;
			item->pval = 0;
		}
		else if (streq(buf, "tried"))
		{
			item->tried = TRUE;
		}

		/* Useless indicators */
		else if (streq(buf, "cursed"))
		{
			item->value = 0L;
		}
		else if (streq(buf, "broken"))
		{
			item->value = 0L;
		}
		else if (streq(buf, "worthless"))
		{
			item->value = 0L;
		}
		else if (streq(buf, "terrible"))
		{
			item->value = 0L;
			item->special = TRUE;
		}

		/* Average indicators */
		else if (streq(buf, "average"))
		{
			item->average = TRUE;
		}

		/* Blessed indicators */
		else if (streq(buf, "good"))
		{
			item->blessed = TRUE;
		}
		else if (streq(buf, "excellent"))
		{
			item->blessed = TRUE;
		}
		else if (streq(buf, "special"))
		{
			item->blessed = TRUE;
			item->special = TRUE;
		}
	}
}



/*
 * Hack -- Select an item
 */
void borg_send_item_index(int i)
{
	/* Choose from inventory */
	if (i < INVEN_WIELD)
	{
		/* Send item index (inventory) */
		borg_keypress(I2A(i));
	}

	/* Choose from equipment */
	else
	{
		/* Send equipment selector key */
		borg_keypress('/');

		/* Send item index (equipment) */
		borg_keypress(I2A(i - INVEN_WIELD));
	}
}


/*
 * Send a command to inscribe item number "i" with the inscription "str".
 */
void borg_send_inscribe_item(int i, cptr str)
{
	/* Send action (inscribe) */
	borg_keypress(c1);

	/* Send item index */
	borg_send_item_index(i);

	/* Send the label string */
	borg_keypresses(str);

	/* Send end of string */
	borg_keypress('\n');
}


/*
 * Send a command to uninscribe item number "i"
 */
void borg_send_uninscribe_item(int i)
{
	/* Send action (uninscribe) */
	borg_keypress(c2);

	/* Send item index */
	borg_send_item_index(i);
}



/*
 * Find the slot of an item with the given tval/sval, if available.
 * Given multiple choices, choose the item with the largest "pval".
 * Given multiple choices, choose the smallest available pile.
 */
int borg_slot(int tval, int sval)
{
	int i, n = -1;

	/* Scan the pack */
	for (i = 0; i < INVEN_PACK; i++)
	{
		auto_item *item = &borg_items[i];

		/* Skip empty items */
		if (!item->iqty) continue;

		/* Skip un-aware items */
		if (!item->kind) continue;

		/* Require correct tval */
		if (item->tval != tval) continue;

		/* Require correct sval */
		if (item->sval != sval) continue;

		/* Prefer largest "pval" */
		if ((n >= 0) && (item->pval < borg_items[n].pval)) continue;

		/* Prefer smallest pile */
		if ((n >= 0) && (item->iqty > borg_items[n].iqty)) continue;

		/* Save this item */
		n = i;
	}

	/* Done */
	return (n);
}



/*
 * Hack -- refuel a torch
 */
bool borg_refuel_torch(void)
{
	int i;

	/* Look for a torch */
	i = borg_slot(TV_LITE, SV_LITE_TORCH);

	/* None available */
	if (i < 0) return (FALSE);

	/* Log the message */
	borg_note(format("# Refueling with %s.", borg_items[i].desc));

	/* Send action (fuel) */
	borg_keypress('F');

	/* Send item index */
	borg_send_item_index(i);

	/* Success */
	return (TRUE);
}


/*
 * Hack -- refuel a lantern
 */
bool borg_refuel_lantern(void)
{
	int i;

	/* Look for a torch */
	i = borg_slot(TV_FLASK, 0);

	/* None available */
	if (i < 0) return (FALSE);

	/* Log the message */
	borg_note(format("# Refueling with %s.", borg_items[i].desc));

	/* Send action (fuel) */
	borg_keypress('F');

	/* Send item index */
	borg_send_item_index(i);

	/* Success */
	return (TRUE);
}




/*
 * Hack -- attempt to eat the given food (by sval)
 */
bool borg_eat_food(int sval)
{
	int i;

	/* Look for that food */
	i = borg_slot(TV_FOOD, sval);

	/* None available */
	if (i < 0) return (FALSE);

	/* Log the message */
	borg_note(format("# Eating %s.", borg_items[i].desc));

	/* Send action (eat) */
	borg_keypress('E');

	/* Send item index */
	borg_send_item_index(i);

	/* Success */
	return (TRUE);
}


/*
 * Hack -- attempt to quaff the given potion (by sval)
 */
bool borg_quaff_potion(int sval)
{
	int i;

	/* Look for that potion */
	i = borg_slot(TV_POTION, sval);

	/* None available */
	if (i < 0) return (FALSE);

	/* Log the message */
	borg_note(format("# Quaffing %s.", borg_items[i].desc));

	/* Send action (quaff) */
	borg_keypress('q');

	/* Send item index */
	borg_send_item_index(i);

	/* Success */
	return (TRUE);
}


/*
 * Hack -- attempt to read the given scroll (by sval)
 */
bool borg_read_scroll(int sval)
{
	int i;

	/* Blind or Confused */
	if (borg_base_is_blind || borg_base_is_confused) return (FALSE);

	/* XXX XXX XXX Dark */

	/* Look for that scroll */
	i = borg_slot(TV_SCROLL, sval);

	/* None available */
	if (i < 0) return (FALSE);

	/* Log the message */
	borg_note(format("# Reading %s.", borg_items[i].desc));

	/* Send action (read) */
	borg_keypress('r');

	/* Send item index */
	borg_send_item_index(i);

	/* Success */
	return (TRUE);
}


/*
 * Hack -- attempt to zap the given (charged) rod (by sval)
 */
bool borg_zap_rod(int sval)
{
	int i;

	/* Look for that rod */
	i = borg_slot(TV_ROD, sval);

	/* None available */
	if (i < 0) return (FALSE);

	/* Hack -- Still charging */
	if (!borg_items[i].pval) return (FALSE);

	/* Log the message */
	borg_note(format("# Zapping %s.", borg_items[i].desc));

	/* Send action (zap) */
	borg_keypress('z');

	/* Send item index */
	borg_send_item_index(i);

	/* Success */
	return (TRUE);
}


/*
 * Hack -- attempt to aim the given (charged) wand (by sval)
 */
bool borg_aim_wand(int sval)
{
	int i;

	/* Look for that wand */
	i = borg_slot(TV_WAND, sval);

	/* None available */
	if (i < 0) return (FALSE);

	/* No charges */
	if (!borg_items[i].pval) return (FALSE);

	/* Log the message */
	borg_note(format("# Aiming %s.", borg_items[i].desc));

	/* Send action (aim) */
	borg_keypress('a');

	/* Send item index */
	borg_send_item_index(i);

	/* Success */
	return (TRUE);
}


/*
 * Hack -- attempt to use the given (charged) staff (by sval)
 */
bool borg_use_staff(int sval)
{
	int i;

	/* Look for that staff */
	i = borg_slot(TV_STAFF, sval);

	/* None available */
	if (i < 0) return (FALSE);

	/* No charges */
	if (!borg_items[i].pval) return (FALSE);

	/* Log the message */
	borg_note(format("# Using %s.", borg_items[i].desc));

	/* Send action (use) */
	borg_keypress('u');

	/* Send item index */
	borg_send_item_index(i);

	/* Success */
	return (TRUE);
}


/*
 * Hack -- attempt to activate the given dragon scale mail (by sval)
 */
bool borg_activate_dragon(int sval)
{
	int i;

	/* Check the equipment */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		auto_item *item = &borg_items[i];

		/* Require dragon scale mail */
		if (item->tval != TV_DRAG_ARMOR) continue;
		
		/* Require requested dragon scale mail */
		if (item->sval != sval) continue;

		/* Check charge */
		if (item->timeout) continue;

		/* Hack -- decline artifacts */
		if (item->name1) continue;

		/* Log the message */
		borg_note(format("# Activating %s.", item->desc));

		/* Send action (activate) */
		borg_keypress('A');

		/* Send item index XXX XXX XXX */
		borg_keypress(I2A(i - INVEN_WIELD));

		/* Success */
		return (TRUE);
	}

	/* Oops */
	return (FALSE);
}


/*
 * Hack -- attempt to use the given artifact (by index)
 */
bool borg_activate_artifact(int name1)
{
	int i;

	/* Check the equipment */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		auto_item *item = &borg_items[i];

		/* Skip incorrect artifacts */
		if (item->name1 != name1) continue;

		/* Check charge */
		if (item->timeout) continue;

		/* Log the message */
		borg_note(format("# Activating %s.", item->desc));

		/* Send action (activate) */
		borg_keypress('A');

		/* Send item index XXX XXX XXX */
		borg_keypress(I2A(i - INVEN_WIELD));

		/* Success */
		return (TRUE);
	}

	/* Oops */
	return (FALSE);
}


/*
 * Determine fail rate of a spell (assume we have enough mana)
 */
int borg_spell_fail(int book, int what)
{
	auto_magic *as = &borg_magics[book][what];
	int chance, minfail;

	/* The borg must be able to "cast" spells */
	if (mb_ptr->spell_book != TV_MAGIC_BOOK) return (100);

	/* The book must be possessed */
	if (amt_book[book] <= 0) return (100);

	/* The spell must be "known" */
	if (as->status < BORG_MAGIC_TEST) return (100);

	/* Base fail rate */
	chance = as->sfail;

	/* Reduce failure rate by "effective" level adjustment */
	chance -= 3 * (b_ptr->lev - as->level);

	/* Reduce failure rate by INT adjustment */
	chance -= 3 * (adj_mag_stat[b_ptr->stat_ind[A_INT]] - 1);

	/* Extract minimum fail rate */
	minfail = adj_mag_fail[b_ptr->stat_ind[A_INT]];

	/* Non-mages never get too good */
	if (minfail < 5 && b_ptr->pclass != CLASS_MAGE) minfail = 5;

	/* Enforce minimum */
	if (chance < minfail) chance = minfail;

	/* Stunning makes casting harder */
	if (borg_base_is_stun) chance += 25;

	/* Never higher than 95 */
	if (chance > 95) chance = 95;

	/* Return chance */
	return (chance);
}

/*
 * Determine if borg can cast a given spell (when fully rested)
 */
bool borg_spell_legal(int book, int what)
{
	auto_magic *as = &borg_magics[book][what];

	/* The borg must be able to "cast" spells */
	if (mb_ptr->spell_book != TV_MAGIC_BOOK) return (FALSE);

	/* The book must be possessed */
	if (amt_book[book] <= 0) return (FALSE);

	/* The spell must be "known" */
	if (as->status < BORG_MAGIC_TEST) return (FALSE);

	/* The spell must be affordable (when rested) */
	if (as->power > b_ptr->msp) return (FALSE);

	/* Success */
	return (TRUE);
}

/*
 * Determine if borg can cast a given spell (right now)
 */
bool borg_spell_okay(int book, int what)
{
	auto_magic *as = &borg_magics[book][what];

	/* Require ability (when rested) */
	if (!borg_spell_legal(book, what)) return (FALSE);

	/* Hack -- blind/confused */
	if (borg_base_is_blind || borg_base_is_confused) return (FALSE);

	/* XXX XXX XXX Dark */

	/* The spell must be affordable (now) */
	if (as->power > b_ptr->csp) return (FALSE);

	/* Success */
	return (TRUE);
}

/*
 * Attempt to cast a spell
 */
bool borg_spell(int book, int what)
{
	int i;

	auto_magic *as = &borg_magics[book][what];

	/* Require ability (right now) */
	if (!borg_spell_okay(book, what)) return (FALSE);

	/* Look for the book */
	i = borg_base_book[book];

	/* Paranoia */
	if (i < 0) return (FALSE);

	/* Debugging Info */
	borg_note(format("# Casting %s (%d,%d).", as->name, book, what));

	/* Send action (cast spell) */
	borg_keypress('m');

	/* Send item index */
	borg_send_item_index(i);

	/* Send spell index */
	borg_keypress(I2A(what));

	/* Success */
	return (TRUE);
}

/*
 * Determine fail rate of a prayer (assume we have enough mana)
 */
int borg_prayer_fail(int book, int what)
{
	auto_magic *as = &borg_magics[book][what];
	int chance, minfail;

	/* The borg must be able to "pray" prayers */
	if (mb_ptr->spell_book != TV_PRAYER_BOOK) return (100);

	/* The book must be possessed */
	if (amt_book[book] <= 0) return (100);

	/* The spell must be "known" */
	if (as->status < BORG_MAGIC_TEST) return (100);

	/* Base fail rate */
	chance = as->sfail;

	/* Reduce failure rate by "effective" level adjustment */
	chance -= 3 * (b_ptr->lev - as->level);

	/* Reduce failure rate by WIS adjustment */
	chance -= 3 * (adj_mag_stat[b_ptr->stat_ind[A_WIS]] - 1);

	/* Extract minimum fail rate */
	minfail = adj_mag_fail[b_ptr->stat_ind[A_WIS]];

	/* Non-priests never get too good */
	if (minfail < 5 && b_ptr->pclass != CLASS_PRIEST) minfail = 5;

	/* Enforce minimum */
	if (chance < minfail) chance = minfail;

	/* Stunning makes praying harder */
	if (borg_base_is_stun) chance += 25;

	/* Never higher than 95 */
	if (chance > 95) chance = 95;

	/* Return chance */
	return (chance);
}


/*
 * Determine if borg can pray a given prayer (when fully rested)
 */
bool borg_prayer_legal(int book, int what)
{
	auto_magic *as = &borg_magics[book][what];

	/* The borg must be able to "pray" prayers */
	if (mb_ptr->spell_book != TV_PRAYER_BOOK) return (FALSE);

	/* Look for the book */
	if (amt_book[book] <= 0) return (FALSE);

	/* The prayer must be "known" */
	if (as->status < BORG_MAGIC_TEST) return (FALSE);

	/* The prayer must be affordable (when fully rested) */
	if (as->power > b_ptr->msp) return (FALSE);

	/* Success */
	return (TRUE);
}

/*
 * Determine if borg can pray a given prayer (right now)
 */
bool borg_prayer_okay(int book, int what)
{
	auto_magic *as = &borg_magics[book][what];

	/* Require ability (when rested) */
	if (!borg_prayer_legal(book, what)) return (FALSE);

	/* Hack -- blind/confused */
	if (borg_base_is_blind || borg_base_is_confused) return (FALSE);

	/* XXX XXX XXX Dark */

	/* The prayer must be affordable (right now) */
	if (as->power > b_ptr->csp) return (FALSE);

	/* Success */
	return (TRUE);
}

/*
 * Attempt to pray a prayer
 */
bool borg_prayer(int book, int what)
{
	int i;

	auto_magic *as = &borg_magics[book][what];

	/* Require ability (right now) */
	if (!borg_prayer_okay(book, what)) return (FALSE);

	/* Look for the book */
	i = borg_base_book[book];

	/* Paranoia */
	if (i < 0) return (FALSE);

	/* Debugging Info */
	borg_note(format("# Praying %s (%d,%d).", as->name, book, what));

	/* Send action (pray prayer) */
	borg_keypress('p');

	/* Send item index */
	borg_send_item_index(i);

	/* Send prayer index */
	borg_keypress(I2A(what));

	/* Success */
	return (TRUE);
}



/*
 * Optimize the weight extractions XXX XXX XXX
 */
static char borg_old_weight_buf[160];


/*
 * Watch an "equip" sub-window
 */
void borg_watch_equip(int which)
{
	int i, j;

	byte t_a;

	char buf[160];

	const term *borg_term_pointer_old = borg_term_pointer;


	/* Access the appropriate sub-window */
	borg_term_pointer = angband_term[which];


	/* Extract the inventory */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		/* Access the row */
		j = (i - INVEN_WIELD);

		/* Attempt to get some text */
		if ((0 == borg_what_text(3, j, -80, &t_a, buf)) &&
		    (buf[0] && (buf[0] != ' ')))
		{
			int k;

			/* Strip trailing spaces */
			for (k = strlen(buf); (k > 0) && (buf[k-1] == ' '); k--) /* loop */;
			buf[k] = '\0';
		}

		/* Default to "nothing" */
		else
		{
			buf[0] = '\0';
		}

		/* Handle explicitly empty slots */
		if (streq(buf, "(nothing)")) strcpy(buf, "");

		/* Ignore "unchanged" items */
		if (streq(buf, borg_items[i].desc)) continue;

		/* Analyze the item (no price) */
		borg_item_analyze(&borg_items[i], buf);

		/* Ignore changing light XXX XXX XXX */
		if (i == INVEN_LITE) continue;

		/* May need to update spells */
		borg_do_spell = TRUE;
		borg_do_spell_aux = 0;
	}


	/* Restore the sub-window accessor */
	borg_term_pointer = borg_term_pointer_old;
}


/*
 * Watch an "inven" sub-window
 *
 * Hack -- steal inventory weight from the game.  XXX XXX XXX
 */
void borg_watch_inven(int which)
{
	int i;

	int w1a, w1b;

	bool done = FALSE;

	byte t_a;

	char buf[160];

	const term *borg_term_pointer_old = borg_term_pointer;


	/* Hack -- Steal the weight description */
	borg_steal_inventory_weight(buf);

	/* Check the cache */
	if (!streq(borg_old_weight_buf, buf))
	{
		/* Update the cache */
		strcpy(borg_old_weight_buf, buf);

		/* Hack -- Parse the current total weight */
		if (sscanf(buf, "Inventory (carrying %d.%d pounds)",
		           &w1a, &w1b) == 2)
		{
			/* Save the current weight */
			b_ptr->total_weight = borg_base_wgt = w1a * 10 + w1b;
		}
	}


	/* Access the appropriate sub-window */
	borg_term_pointer = angband_term[which];


	/* Extract the inventory */
	for (i = 0; i < INVEN_PACK; i++)
	{
		/* Attempt to get some text */
		if (!done &&
		    (0 == borg_what_text(3, i, -80, &t_a, buf)) &&
		    (buf[0] && (buf[0] != ' ')))
		{
			int k;

			/* Strip trailing spaces */
			for (k = strlen(buf); (k > 0) && (buf[k-1] == ' '); k--) /* loop */;
			buf[k] = '\0';
		}

		/* Default to "nothing" */
		else
		{
			buf[0] = '\0';
			done = TRUE;
		}

		/* Handle explicitly empty slots */
		if (streq(buf, "(nothing)")) strcpy(buf, "");

		/* Ignore "unchanged" items */
		if (streq(buf, borg_items[i].desc)) continue;

		/* Analyze the item (no price) */
		borg_item_analyze(&borg_items[i], buf);

		/* Hack -- Clear "shop" goals */
		borg_goal_shop = borg_goal_ware = borg_goal_item = -1;

		/* Note changed inventory */
		borg_do_crush_junk = TRUE;
		borg_do_crush_hole = TRUE;
		borg_do_crush_slow = TRUE;

		/* May need to update spells */
		borg_do_spell = TRUE;
		borg_do_spell_aux = 0;
	}


	/* Restore the sub-window accessor */
	borg_term_pointer = borg_term_pointer_old;
}


/*
 * Parse the "equip" screen
 */
void borg_parse_equip(void)
{
	int i;

	int row, col;

	byte t_a;

	char buf[160];


	/* Find the column */
	for (col = 0; col < 55; col++)
	{
		/* Look for first prefix */
		if ((0 == borg_what_text(col, 1, 3, &t_a, buf)) &&
		    (buf[0] == I2A(0)) && (buf[1] == p2) && (buf[2] == ' '))
		{
			break;
		}
	}


	/* Extract the inventory */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		/* Access the row */
		row = i - INVEN_WIELD;

		/* Attempt to get some text */
		if ((0 == borg_what_text(col, row+1, 3, &t_a, buf)) &&
		    (buf[0] == I2A(row)) && (buf[1] == p2) && (buf[2] == ' ') &&
		    (0 == borg_what_text(col+3, row+1, -80, &t_a, buf)) &&
		    (buf[0] && (buf[0] != ' ')))
		{
			int k;

			/* Strip trailing spaces */
			for (k = strlen(buf); (k > 0) && (buf[k-1] == ' '); k--) /* loop */;
			buf[k] = '\0';
		}

		/* Default to "nothing" */
		else
		{
			buf[0] = '\0';
		}

		/* Handle explicitly empty slots */
		if (streq(buf, "(nothing)")) strcpy(buf, "");

		/* Ignore "unchanged" items */
		if (streq(buf, borg_items[i].desc)) continue;

		/* Analyze the item (no price) */
		borg_item_analyze(&borg_items[i], buf);

		/* Ignore changing light XXX XXX XXX */
		if (i == INVEN_LITE) continue;

		/* May need to update spells */
		borg_do_spell = TRUE;
		borg_do_spell_aux = 0;
	}
}


/*
 * Parse the "inven" screen
 */
void borg_parse_inven(void)
{
	int i;

	int row, col;

	int w1a, w1b;

	bool done = FALSE;

	byte t_a;

	char buf[160];


	/* Hack -- Grab the weight description */
	(void)borg_what_text(0, 0, -80, &t_a, buf);

	/* Check the cache */
	if (!streq(borg_old_weight_buf, buf))
	{
		/* Update the cache */
		strcpy(borg_old_weight_buf, buf);

		/* Hack -- Parse the current total weight */
		if (sscanf(buf, "Inventory (carrying %d.%d pounds)",
		           &w1a, &w1b) == 2)
		{
			/* Save the current weight */
			b_ptr->total_weight = borg_base_wgt = w1a * 10 + w1b;
		}
	}


	/* Find the column */
	for (col = 0; col < 55; col++)
	{
		/* Look for first prefix */
		if ((0 == borg_what_text(col, 1, 3, &t_a, buf)) &&
		    (buf[0] == I2A(0)) && (buf[1] == p2) && (buf[2] == ' '))
		{
			break;
		}
	}


	/* Extract the inventory */
	for (i = 0; i < INVEN_PACK; i++)
	{
		/* Access the row */
		row = i;

		/* Attempt to get some text */
		if (!done &&
		    (0 == borg_what_text(col, row+1, 3, &t_a, buf)) &&
		    (buf[0] == I2A(row)) && (buf[1] == p2) && (buf[2] == ' ') &&
		    (0 == borg_what_text(col+3, row+1, -80, &t_a, buf)) &&
		    (buf[0] && (buf[0] != ' ')))
		{
			int k;

			/* Strip trailing spaces */
			for (k = strlen(buf); (k > 0) && (buf[k-1] == ' '); k--) /* loop */;
			buf[k] = '\0';
		}

		/* Default to "nothing" */
		else
		{
			buf[0] = '\0';
			done = TRUE;
		}

		/* Handle explicitly empty slots */
		if (streq(buf, "(nothing)")) strcpy(buf, "");

		/* Ignore "unchanged" items */
		if (streq(buf, borg_items[i].desc)) continue;

		/* Analyze the item (no price) */
		borg_item_analyze(&borg_items[i], buf);

		/* Hack -- Clear "shop" goals */
		borg_goal_shop = borg_goal_ware = borg_goal_item = -1;

		/* Note changed inventory */
		borg_do_crush_junk = TRUE;
		borg_do_crush_hole = TRUE;
		borg_do_crush_slow = TRUE;

		/* May need to update spells */
		borg_do_spell = TRUE;
		borg_do_spell_aux = 0;
	}
}



/*
 * Hack -- Parse the "spell" info (given the book)
 */
void borg_parse_spell(int book)
{
	int what;

	byte t_a;

	char buf[160];


	/* Can we use spells/prayers? */
	if (!mb_ptr->spell_book) return;


	/* Process the spells */
	for (what = 0; what < 9; what++)
	{
		int row = ROW_SPELL + 1 + what;
		int col = COL_SPELL;

		/* Access the spell */
		auto_magic *as = &borg_magics[book][what];

		/* Skip illegible spells */
		if (as->status == BORG_MAGIC_ICKY) continue;

#if 0
		/* Format: "spell-name...................." at col 20+5 */
		if (0 != borg_what_text(col-30, row, -30, &t_a, buf)) continue;
#endif

		/* Format: "Lv Mana Freq Comment" at col 20+35 */
		if (0 != borg_what_text(col, row, -20, &t_a, buf)) continue;

		/* Note "forgotten" spells */
		if (prefix(buf + 13, "forgott"))
		{
			/* Forgotten */
			as->status = BORG_MAGIC_LOST;
		}

		/* Note "difficult" spells */
		else if (b_ptr->lev < as->level)
		{
			/* Unknown */
			as->status = BORG_MAGIC_HIGH;
		}

		/* Note "unknown" spells */
		else if (prefix(buf + 13, "unknown"))
		{
			/* Unknown */
			as->status = BORG_MAGIC_OKAY;
		}

		/* Note "untried" spells */
		else if (prefix(buf + 13, "untried"))
		{
			/* Untried */
			as->status = BORG_MAGIC_TEST;
		}

		/* Note "known" spells */
		else
		{
			/* Known */
			as->status = BORG_MAGIC_KNOW;
		}
	}
}




/*
 * Hack -- help analyze the magic
 *
 * The comments yield the "name" of the spell or prayer.
 *
 * Also, the leading letter in the comment indicates how we use the
 * spell or prayer, if at all, using "A" for "attack", "D" for "call
 * light" and "detection", "E" for "escape", "H" for healing, "O" for
 * "object manipulation", and "F" for "terrain feature manipulation",
 * plus "!" for entries that can soon be handled.
 */

static byte borg_magic_method[2][9][9] =
{
	/*** Spells ***/

	{
		{
			/* Magic for Beginners (sval 0) */
			BORG_MAGIC_AIM	/* A "Magic Missile" */,
			BORG_MAGIC_EXT	/*   "Detect Monsters" */,
			BORG_MAGIC_NOP	/* E "Phase Door" */,
			BORG_MAGIC_NOP	/* D "Light Area" */,
			BORG_MAGIC_NOP	/*   "Treasure Detection" */,
			BORG_MAGIC_NOP	/* H "Cure Light Wounds" */,
			BORG_MAGIC_NOP	/*   "Object Detection" */,
			BORG_MAGIC_NOP	/* D "Find Hidden Traps/Doors" */,
			BORG_MAGIC_AIM	/* A "Stinking Cloud" */
		},

		{
			/* Conjurings and Tricks (sval 1) */
			BORG_MAGIC_AIM	/*   "Confusion" */,
			BORG_MAGIC_AIM	/* A "Lightning Bolt" */,
			BORG_MAGIC_NOP	/* F "Trap/Door Destruction" */,
			BORG_MAGIC_AIM	/*   "Sleep I" */,
			BORG_MAGIC_NOP	/* H "Cure Poison" */,
			BORG_MAGIC_NOP	/* E "Teleport Self" */,
			BORG_MAGIC_AIM	/* A "Spear of Light" */,
			BORG_MAGIC_AIM	/* A "Frost Bolt" */,
			BORG_MAGIC_AIM	/* F "Turn Stone to Mud" */
		},

		{
			/* Incantations and Illusions (sval 2) */
			BORG_MAGIC_NOP	/* H "Satisfy Hunger" */,
			BORG_MAGIC_OBJ	/* O "Recharge Item I" */,
			BORG_MAGIC_NOP	/*   "Sleep II" */,
			BORG_MAGIC_AIM	/*   "Polymorph Other" */,
			BORG_MAGIC_OBJ	/* O "Identify" */,
			BORG_MAGIC_NOP	/*   "Sleep III" */,
			BORG_MAGIC_AIM	/* A "Fire Bolt" */,
			BORG_MAGIC_AIM	/*   "Slow Monster" */,
			BORG_MAGIC_ICK	/*   "(blank)" */
		},

		{
			/* Sorcery and Evocations (sval 3) */
			BORG_MAGIC_AIM	/* A "Frost Ball" */,
			BORG_MAGIC_OBJ	/* O "Recharge Item II" */,
			BORG_MAGIC_AIM	/*   "Teleport Other" */,
			BORG_MAGIC_NOP	/*   "Haste Self" */,
			BORG_MAGIC_AIM	/* A "Fire Ball" */,
			BORG_MAGIC_NOP	/*   "Word of Destruction" */,
			BORG_MAGIC_WHO	/*   "Genocide" */,
			BORG_MAGIC_ICK	/*   "(blank)" */,
			BORG_MAGIC_ICK	/*   "(blank)" */
		},

		{
			/* Resistance of Scarabtarices (sval 4) */
			BORG_MAGIC_NOP	/*   "Resist Fire" */,
			BORG_MAGIC_NOP	/*   "Resist Cold" */,
			BORG_MAGIC_NOP	/*   "Resist Acid" */,
			BORG_MAGIC_NOP	/*   "Resist Poison" */,
			BORG_MAGIC_NOP	/*   "Resistance" */,
			BORG_MAGIC_ICK	/*   "(blank)" */,
			BORG_MAGIC_ICK	/*   "(blank)" */,
			BORG_MAGIC_ICK	/*   "(blank)" */,
			BORG_MAGIC_ICK	/*   "(blank)" */
		},

		{
			/* Mordenkainen's Escapes (sval 5) */
			BORG_MAGIC_NOP	/*   "Door Creation" */,
			BORG_MAGIC_NOP	/*   "Stair Creation" */,
			BORG_MAGIC_NOP	/*   "Teleport Level" */,
			BORG_MAGIC_NOP	/*   "Earthquake" */,
			BORG_MAGIC_NOP	/* E "Word of Recall" */,
			BORG_MAGIC_ICK	/*   "(blank)" */,
			BORG_MAGIC_ICK	/*   "(blank)" */,
			BORG_MAGIC_ICK	/*   "(blank)" */,
			BORG_MAGIC_ICK	/*   "(blank)" */
		},

		{
			/* Kelek's Grimoire of Power (sval 6) */
			BORG_MAGIC_EXT	/*   "Detect Evil" */,
			BORG_MAGIC_NOP	/*   "Detect Enchantment" */,
			BORG_MAGIC_OBJ	/* O "Recharge Item III" */,
			BORG_MAGIC_WHO	/*   "Genocide" */,
			BORG_MAGIC_NOP	/*   "Mass Genocide" */,
			BORG_MAGIC_ICK	/*   "(blank)" */,
			BORG_MAGIC_ICK	/*   "(blank)" */,
			BORG_MAGIC_ICK	/*   "(blank)" */,
			BORG_MAGIC_ICK	/*   "(blank)" */
		},

		{
			/* Tenser's transformations... (sval 7) */
			BORG_MAGIC_NOP	/* ! "Heroism" */,
			BORG_MAGIC_NOP	/*   "Shield" */,
			BORG_MAGIC_NOP	/* ! "Berserker" */,
			BORG_MAGIC_NOP	/*   "Essence of Speed" */,
			BORG_MAGIC_NOP	/*   "Globe of Invulnerability" */,
			BORG_MAGIC_ICK	/*   "(blank)" */,
			BORG_MAGIC_ICK	/*   "(blank)" */,
			BORG_MAGIC_ICK	/*   "(blank)" */,
			BORG_MAGIC_ICK	/*   "(blank)" */
		},

		{
			/* Raal's Tome of Destruction (sval 8) */
			BORG_MAGIC_AIM	/* A "Acid Bolt" */,
			BORG_MAGIC_AIM	/* A "Cloud Kill" */,
			BORG_MAGIC_AIM	/* A "Acid Ball" */,
			BORG_MAGIC_AIM	/* A "Ice Storm" */,
			BORG_MAGIC_AIM	/* A "Meteor Swarm" */,
			BORG_MAGIC_AIM	/* A "Mana Storm" */,
			BORG_MAGIC_ICK	/*   "(blank)" */,
			BORG_MAGIC_ICK	/*   "(blank)" */,
			BORG_MAGIC_ICK	/*   "(blank)" */
		}
	},


	/*** Prayers ***/

	{
		{
			/* Beginners Handbook (sval 0) */
			BORG_MAGIC_EXT	/*   "Detect Evil" */,
			BORG_MAGIC_NOP	/*   "Cure Light Wounds" */,
			BORG_MAGIC_NOP	/*   "Bless" */,
			BORG_MAGIC_NOP	/* H "Remove Fear" */,
			BORG_MAGIC_NOP	/* D "Call Light" */,
			BORG_MAGIC_NOP	/* D "Find Traps" */,
			BORG_MAGIC_NOP	/* D "Detect Doors/Stairs" */,
			BORG_MAGIC_NOP	/*   "Slow Poison" */,
			BORG_MAGIC_ICK	/*   "(blank)" */
		},

		{
			/* Words of Wisdom (sval 1) */
			BORG_MAGIC_AIM	/*   "Confuse Creature" */,
			BORG_MAGIC_NOP	/* E "Portal" */,
			BORG_MAGIC_NOP	/* H "Cure Serious Wounds" */,
			BORG_MAGIC_NOP	/*   "Chant" */,
			BORG_MAGIC_NOP	/*   "Sanctuary" */,
			BORG_MAGIC_NOP	/* H "Satisfy Hunger" */,
			BORG_MAGIC_NOP	/*   "Remove Curse" */,
			BORG_MAGIC_NOP	/*   "Resist Heat and Cold" */,
			BORG_MAGIC_ICK	/*   "(blank)" */
		},

		{
			/* Chants and Blessings (sval 2) */
			BORG_MAGIC_NOP	/* H "Neutralize Poison" */,
			BORG_MAGIC_AIM	/* A "Orb of Draining" */,
			BORG_MAGIC_NOP	/* H "Cure Critical Wounds" */,
			BORG_MAGIC_EXT	/*   "Sense Invisible" */,
			BORG_MAGIC_NOP	/*   "Protection from Evil" */,
			BORG_MAGIC_NOP	/*   "Earthquake" */,
			BORG_MAGIC_NOP	/* D "Sense Surroundings" */,
			BORG_MAGIC_NOP	/* H "Cure Mortal Wounds" */,
			BORG_MAGIC_NOP	/*   "Turn Undead" */
		},

		{
			/* Exorcism and Dispelling (sval 3) */
			BORG_MAGIC_NOP	/*   "Prayer" */,
			BORG_MAGIC_NOP	/* ! "Dispel Undead" */,
			BORG_MAGIC_NOP	/* H "Heal" */,
			BORG_MAGIC_NOP	/* ! "Dispel Evil" */,
			BORG_MAGIC_NOP	/*   "Glyph of Warding" */,
			BORG_MAGIC_NOP	/* ! "Holy Word" */,
			BORG_MAGIC_ICK	/*   "(blank)" */,
			BORG_MAGIC_ICK	/*   "(blank)" */,
			BORG_MAGIC_ICK	/*   "(blank)" */
		},

		{
			/* Ethereal openings (sval 4) */
			BORG_MAGIC_NOP	/* E "Blink" */,
			BORG_MAGIC_NOP	/* E "Teleport" */,
			BORG_MAGIC_AIM	/*   "Teleport Away" */,
			BORG_MAGIC_NOP	/*   "Teleport Level" */,
			BORG_MAGIC_NOP	/* E "Word of Recall" */,
			BORG_MAGIC_NOP	/*   "Alter Reality" */,
			BORG_MAGIC_ICK	/*   "(blank)" */,
			BORG_MAGIC_ICK	/*   "(blank)" */,
			BORG_MAGIC_ICK	/*   "(blank)" */
		},

		{
			/* Godly Insights... (sval 5) */
			BORG_MAGIC_EXT	/*   "Detect Monsters" */,
			BORG_MAGIC_EXT	/* D "Detection" */,
			BORG_MAGIC_OBJ	/* O "Perception" */,
			BORG_MAGIC_NOP	/*   "Probing" */,
			BORG_MAGIC_NOP	/* D "Clairvoyance" */,
			BORG_MAGIC_ICK	/*   "(blank)" */,
			BORG_MAGIC_ICK	/*   "(blank)" */,
			BORG_MAGIC_ICK	/*   "(blank)" */,
			BORG_MAGIC_ICK	/*   "(blank)" */
		},

		{
			/* Purifications and Healing (sval 6) */
			BORG_MAGIC_NOP	/* H "Cure Serious Wounds" */,
			BORG_MAGIC_NOP	/* H "Cure Mortal Wounds" */,
			BORG_MAGIC_NOP	/* H "Healing" */,
			BORG_MAGIC_NOP	/* ! "Restoration" */,
			BORG_MAGIC_NOP	/* ! "Remembrance" */,
			BORG_MAGIC_ICK	/*   "(blank)" */,
			BORG_MAGIC_ICK	/*   "(blank)" */,
			BORG_MAGIC_ICK	/*   "(blank)" */,
			BORG_MAGIC_ICK	/*   "(blank)" */
		},

		{
			/* Holy Infusions (sval 7) */
			BORG_MAGIC_NOP	/* F "Unbarring Ways" */,
			BORG_MAGIC_OBJ	/* O "Recharging" */,
			BORG_MAGIC_NOP	/*   "Dispel Curse" */,
			BORG_MAGIC_OBJ	/* O "Enchant Weapon" */,
			BORG_MAGIC_OBJ	/* O "Enchant Armour" */,
			BORG_MAGIC_NOP	/*   "Elemental Brand" */,
			BORG_MAGIC_ICK	/*   "(blank)" */,
			BORG_MAGIC_ICK	/*   "(blank)" */,
			BORG_MAGIC_ICK	/*   "(blank)" */
		},

		{
			/* Wrath of God (sval 8) */
			BORG_MAGIC_NOP	/* ! "Dispel Undead" */,
			BORG_MAGIC_NOP	/* ! "Dispel Evil" */,
			BORG_MAGIC_NOP	/*   "Banishment" */,
			BORG_MAGIC_NOP	/*   "Word of Destruction" */,
			BORG_MAGIC_AIM	/*   "Annihilation" */,
			BORG_MAGIC_ICK	/*   "(blank)" */,
			BORG_MAGIC_ICK	/*   "(blank)" */,
			BORG_MAGIC_ICK	/*   "(blank)" */,
			BORG_MAGIC_ICK	/*   "(blank)" */
		}
	}
};



/*
 * Hack -- help analyze the magic
 *
 * The comments yield the "name" of the spell or prayer.
 *
 * Also, the leading letter in the comment indicates how we use the
 * spell or prayer, if at all, using "A" for "attack", "D" for "call
 * light" and "detection", "E" for "escape", "H" for healing, "O" for
 * "object manipulation", "F" for "terrain feature manipulation",
 * "X" for "never use this", and "!" for "soon to be handled".
 *
 * The value indicates how much we want to know the spell/prayer.  A
 * rating of zero indicates that the spell/prayer is useless, and should
 * never be learned or used.  A rating from 1 to 49 indicates that the
 * spell/prayer is worth some experience to use once, so we should study
 * (and use) it when we get bored in town.  A rating from 50 to 99 means
 * that the spell/prayer should be learned as soon as possible (and used
 * when bored).  Note that spells/prayers which are "bizarre", such as
 * "genocide" and "mass genocide" have a rating of zero.
 *
 * XXX XXX XXX Verify ratings.
 */

static byte borg_magic_rating[2][9][9] =
{
	/*** Spells ***/

	{
		{
			/* Magic for Beginners (sval 0) */
			85			/* A "Magic Missile" */,
			5			/*   "Detect Monsters" */,
			75			/* E "Phase Door" */,
			75			/* D "Light Area" */,
			5			/*   "Treasure Detection" */,
			55			/* H "Cure Light Wounds" */,
			5			/*   "Object Detection" */,
			95			/* D "Find Hidden Traps/Doors" */,
			85			/* A "Stinking Cloud" */
		},

		{
			/* Conjurings and Tricks (sval 1) */
			5			/*   "Confusion" */,
			85			/* A "Lightning Bolt" */,
			55			/* F "Trap/Door Destruction" */,
			5			/*   "Sleep I" */,
			65			/* H "Cure Poison" */,
			95			/* E "Teleport Self" */,
			55			/* A "Spear of Light" */,
			85			/* A "Frost Bolt" */,
			75			/* F "Turn Stone to Mud" */
		},

		{
			/* Incantations and Illusions (sval 2) */
			95			/* H "Satisfy Hunger" */,
			55			/* O "Recharge Item I" */,
			5			/*   "Sleep II" */,
			5			/*   "Polymorph Other" */,
			95			/* O "Identify" */,
			5			/*   "Sleep III" */,
			85			/* A "Fire Bolt" */,
			5			/*   "Slow Monster" */,
			0			/*   "(blank)" */
		},

		{
			/* Sorcery and Evocations (sval 3) */
			85			/* A "Frost Ball" */,
			75			/* O "Recharge Item II" */,
			5			/*   "Teleport Other" */,
			5			/*   "Haste Self" */,
			85			/* A "Fire Ball" */,
			0			/* X "Word of Destruction" */,
			5			/*   "Genocide" */,
			0			/*   "(blank)" */,
			0			/*   "(blank)" */
		},

		{
			/* Resistance of Scarabtarices (sval 4) */
			5			/*   "Resist Fire" */,
			5			/*   "Resist Cold" */,
			5			/*   "Resist Acid" */,
			5			/*   "Resist Poison" */,
			5			/*   "Resistance" */,
			0			/*   "(blank)" */,
			0			/*   "(blank)" */,
			0			/*   "(blank)" */,
			0			/*   "(blank)" */
		},

		{
			/* Mordenkainen's Escapes (sval 5) */
			5			/*   "Door Creation" */,
			5			/*   "Stair Creation" */,
			5			/*   "Teleport Level" */,
			0			/* X "Earthquake" */,
			75			/* E "Word of Recall" */,
			0			/*   "(blank)" */,
			0			/*   "(blank)" */,
			0			/*   "(blank)" */,
			0			/*   "(blank)" */
		},

		{
			/* Kelek's Grimoire of Power (sval 6) */
			5			/*   "Detect Evil" */,
			5			/*   "Detect Enchantment" */,
			75			/* O "Recharge Item III" */,
			0			/*   "Genocide" */,
			0			/*   "Mass Genocide" */,
			0			/*   "(blank)" */,
			0			/*   "(blank)" */,
			0			/*   "(blank)" */,
			0			/*   "(blank)" */
		},

		{
			/* Tenser's transformations... (sval 7) */
			55			/* H "Heroism" */,
			5			/*   "Shield" */,
			55			/* H "Berserker" */,
			5			/*   "Essence of Speed" */,
			5			/*   "Globe of Invulnerability" */,
			0			/*   "(blank)" */,
			0			/*   "(blank)" */,
			0			/*   "(blank)" */,
			0			/*   "(blank)" */
		},

		{
			/* Raal's Tome of Destruction (sval 8) */
			85			/* A "Acid Bolt" */,
			85			/* A "Cloud Kill" */,
			85			/* A "Acid Ball" */,
			85			/* A "Ice Storm" */,
			85			/* A "Meteor Swarm" */,
			85			/* A "Mana Storm" */,
			0			/*   "(blank)" */,
			0			/*   "(blank)" */,
			0			/*   "(blank)" */
		}
	},


	/*** Prayers ***/

	{
		{
			/* Beginners Handbook (sval 0) */
			5			/*   "Detect Evil" */,
			55			/* H "Cure Light Wounds" */,
			5			/*   "Bless" */,
			65			/* H "Remove Fear" */,
			65			/* D "Call Light" */,
			75			/* D "Find Traps" */,
			75			/* D "Detect Doors/Stairs" */,
			5			/*   "Slow Poison" */,
			0			/*   "(blank)" */
		},

		{
			/* Words of Wisdom (sval 1) */
			5			/*   "Confuse Creature" */,
			95			/* E "Portal" */,
			5			/* H "Cure Serious Wounds" */,
			5			/*   "Chant" */,
			5			/*   "Sanctuary" */,
			95			/* H "Satisfy Hunger" */,
			5			/*   "Remove Curse" */,
			5			/*   "Resist Heat and Cold" */,
			0			/*   "(blank)" */
		},

		{
			/* Chants and Blessings (sval 2) */
			65			/* H "Neutralize Poison" */,
			85			/* A "Orb of Draining" */,
			55			/* H "Cure Critical Wounds" */,
			5			/*   "Sense Invisible" */,
			5			/*   "Protection from Evil" */,
			0			/* X "Earthquake" */,
			65			/* D "Sense Surroundings" */,
			55			/* H "Cure Mortal Wounds" */,
			5			/*   "Turn Undead" */
		},

		{
			/* Exorcism and Dispelling (sval 3) */
			5			/*   "Prayer" */,
			5			/* ! "Dispel Undead" */,
			55			/* H "Heal" */,
			5			/* ! "Dispel Evil" */,
			5			/*   "Glyph of Warding" */,
			5			/* ! "Holy Word" */,
			0			/*   "(blank)" */,
			0			/*   "(blank)" */,
			0			/*   "(blank)" */
		},

		{
			/* Ethereal openings (sval 4) */
			65			/* E "Blink" */,
			65			/* E "Teleport" */,
			5			/*   "Teleport Away" */,
			5			/*   "Teleport Level" */,
			75			/* E "Word of Recall" */,
			5			/*   "Alter Reality" */,
			0			/*   "(blank)" */,
			0			/*   "(blank)" */,
			0			/*   "(blank)" */
		},

		{
			/* Godly Insights... (sval 5) */
			5			/*   "Detect Monsters" */,
			65			/* D "Detection" */,
			75			/* O "Perception" */,
			5			/*   "Probing" */,
			65			/* D "Clairvoyance" */,
			0			/*   "(blank)" */,
			0			/*   "(blank)" */,
			0			/*   "(blank)" */,
			0			/*   "(blank)" */
		},

		{
			/* Purifications and Healing (sval 6) */
			55			/* H "Cure Serious Wounds" */,
			55			/* H "Cure Mortal Wounds" */,
			55			/* H "Healing" */,
			5			/* ! "Restoration" */,
			5			/* ! "Remembrance" */,
			0			/*   "(blank)" */,
			0			/*   "(blank)" */,
			0			/*   "(blank)" */,
			0			/*   "(blank)" */
		},

		{
			/* Holy Infusions (sval 7) */
			55			/* F "Unbarring Ways" */,
			55			/* O "Recharging" */,
			5			/*   "Dispel Curse" */,
			55			/* O "Enchant Weapon" */,
			55			/* O "Enchant Armour" */,
			5			/*   "Elemental Brand" */,
			0			/*   "(blank)" */,
			0			/*   "(blank)" */,
			0			/*   "(blank)" */
		},

		{
			/* Wrath of God (sval 8) */
			5			/* ! "Dispel Undead" */,
			5			/* ! "Dispel Evil" */,
			5			/*   "Banishment" */,
			0			/* X "Word of Destruction" */,
			5			/*   "Annihilation" */,
			0			/*   "(blank)" */,
			0			/*   "(blank)" */,
			0			/*   "(blank)" */,
			0			/*   "(blank)" */
		}
	}
};



/*
 * Prepare a book
 */
void borg_prepare_book(int book)
{
	int i, what;

	int spell[64], num = 0;


	/* Reset each spell entry */
	for (what = 0; what < 9; what++)
	{
		auto_magic *as = &borg_magics[book][what];

		/* Assume no name */
		as->name = NULL;

		/* Assume illegible */
		as->status = BORG_MAGIC_ICKY;

		/* Assume illegible */
		as->method = BORG_MAGIC_ICK;

		/* Impossible values */
		as->level = 99;
		as->power = 99;

		/* Impossible value */
		as->cheat = 99;

		/* High fail rate */
		as->sfail = 99;
	}


	/* Can we use spells/prayers? */
	if (!mb_ptr->spell_book) return;


	/* Extract spells */
	for (i = 0; i < 64; i++)
	{
		/* Check for this spell */
		if ((i < 32) ?
		    (spell_flags[mb_ptr->spell_type][book][0] & (1L << i)) :
		    (spell_flags[mb_ptr->spell_type][book][1] & (1L << (i - 32))))
		{
			/* Collect this spell */
			spell[num++] = i;
		}
	}


	/* Process each existing spell */
	for (what = 0; what < num; what++)
	{
		auto_magic *as = &borg_magics[book][what];

		magic_type *s_ptr = &mb_ptr->info[spell[what]];

		/* Skip "illegible" spells */
		if (s_ptr->slevel == 99) continue;

		/* Save the spell name */
		as->name = spell_names[mb_ptr->spell_type][spell[what]];

		/* Save the spell index */
		as->cheat = spell[what];

		/* Hack -- assume excessive level */
		as->status = BORG_MAGIC_HIGH;

		/* Access the correct "method" */
		as->method = borg_magic_method[mb_ptr->spell_type][book][what];

		/* Access the correct "rating" */
		as->rating = borg_magic_rating[mb_ptr->spell_type][book][what];

		/* Extract the level and power */
		as->level = s_ptr->slevel;
		as->power = s_ptr->smana;

		/* Extract base fail rate */
		as->sfail = s_ptr->sfail;
	}
}



#else

#ifdef MACINTOSH
static int HACK = 0;
#endif /* MACINTOSH */

#endif /* ALLOW_BORG */

