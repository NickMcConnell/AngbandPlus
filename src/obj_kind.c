/* File: obj_kind.c */

/* Purpose: Code for the object templates */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"


/* Base size of k_info */
#define K_INFO_BASE_SIZE  500

/* Amount of entries to add when resizing k_info */
#define K_INFO_RESIZE  50

/* Size of the allocated */
static s32b k_info_size = K_INFO_BASE_SIZE;


/* Allocate k_info */
errr k_info_alloc(void)
{
	/* Create the storage for the object templates */
	C_MAKE(k_info, k_info_size, object_kind);

	/* Success */
	return 0;
}


/* Free k_info */
errr k_info_free(void)
{
	k_info_size = K_INFO_BASE_SIZE;

	C_KILL(k_info, k_info_size, object_kind);

	/* Success */
	return 0;
}


void k_info_reset(void)
{
	int i;

	/* Reset the "objects" */
	for (i = 1; i < max_k_idx; i++)
	{
		object_kind *k_ptr = &k_info[i];

		/* Reset "tried" */
		k_ptr->tried = FALSE;

		/* Reset "aware" */
		k_ptr->aware = FALSE;
	}
}


/* Add a new object template */
object_kind *k_info_add(object_kind *k_info_entry)
{
	/* Resize if necessary */
	while (k_info_size <= max_k_idx)
	{
		k_info_size += K_INFO_RESIZE;

		/* Reallocate the extra memory */
		k_info = realloc(k_info, k_info_size * sizeof(object_kind));

		/* Failure */
#ifdef JP
if (!k_info) quit("メモリー不足!");
#else
		if (!k_info) quit("Out of memory!");
#endif


		/* Wipe the new memory */
		(void)C_WIPE(&k_info[(k_info_size - K_INFO_RESIZE)], K_INFO_RESIZE, object_kind);
	}

	/* Increase the maximum index of the array */
	max_k_idx++;

	/* Copy the new object_kind */
	COPY(&k_info[max_k_idx-1], k_info_entry, object_kind);

	/* Success */
	return (&k_info[max_k_idx-1]);
}


/*
 * Initialize some other arrays
 */
errr init_object_alloc(void)
{
	int i, j;
	object_kind *k_ptr;
	alloc_entry *table;
	s16b num[MAX_DEPTH];
	s16b aux[MAX_DEPTH];


	/*** Analyze object allocation info ***/

	/* Clear the "aux" array */
	(void)C_WIPE(&aux, MAX_DEPTH, s16b);

	/* Clear the "num" array */
	(void)C_WIPE(&num, MAX_DEPTH, s16b);

	/* Free the old "alloc_kind_table" (if it exists) */
	if (alloc_kind_table)
	{
		C_KILL(alloc_kind_table, alloc_kind_size, alloc_entry);
	}

	/* Size of "alloc_kind_table" */
	alloc_kind_size = 0;

	/* Scan the objects */
	for (i = 1; i < max_k_idx; i++)
	{
		k_ptr = &k_info[i];

		/* Scan allocation pairs */
		for (j = 0; j < 4; j++)
		{
			/* Count the "legal" entries */
			if (k_ptr->chance[j])
			{
				/* Count the entries */
				alloc_kind_size++;

				/* Group by level */
				num[k_ptr->locale[j]]++;
			}
		}
	}

	/* Collect the level indexes */
	for (i = 1; i < MAX_DEPTH; i++)
	{
		/* Group by level */
		num[i] += num[i-1];
	}

	/* Paranoia */
#ifdef JP
if (!num[0]) quit("町のアイテムがない！");
#else
	if (!num[0]) quit("No town objects!");
#endif



	/*** Initialize object allocation info ***/

	/* Allocate the alloc_kind_table */
	C_MAKE(alloc_kind_table, alloc_kind_size, alloc_entry);

	/* Access the table entry */
	table = alloc_kind_table;

	/* Scan the objects */
	for (i = 1; i < max_k_idx; i++)
	{
		k_ptr = &k_info[i];

		/* Scan allocation pairs */
		for (j = 0; j < 4; j++)
		{
			/* Count the "legal" entries */
			if (k_ptr->chance[j])
			{
				int p, x, y, z;

				/* Extract the base level */
				x = k_ptr->locale[j];

				/* Extract the base probability */
				p = (100 / k_ptr->chance[j]);

				/* Skip entries preceding our locale */
				y = (x > 0) ? num[x-1] : 0;

				/* Skip previous entries at this locale */
				z = y + aux[x];

				/* Load the entry */
				table[z].index = i;
				table[z].level = x;
				table[z].prob1 = p;
				table[z].prob2 = p;
				table[z].prob3 = p;

				/* Another entry complete for this locale */
				aux[x]++;
			}
		}
	}

	/* Success */
	return (0);
}


int get_object_level(object_type *o_ptr)
{
#if 0
	return (byte)get_object_level_callback(o_ptr);
#else
	return k_info[o_ptr->k_idx].level;
#endif
}


s32b get_object_cost(object_type *o_ptr)
{
#if 0
	return get_object_cost_callback(o_ptr);
#else
	return k_info[o_ptr->k_idx].cost;
#endif
}


cptr get_object_name(object_type *o_ptr)
{
#if 0
	return get_object_name_callback(o_ptr);
#else
	return (k_name + k_info[o_ptr->k_idx].name);
#endif
}


/* The player is "aware" of the item's effects */
bool get_object_aware(object_type *o_ptr)
{
#if 0
	return get_object_aware_callback(o_ptr);
#else
	return k_info[o_ptr->k_idx].aware;
#endif
}


/* The player has "tried" one of the items */
bool get_object_tried(object_type *o_ptr)
{
#if 0
	return get_object_tried_callback(o_ptr);
#else
	return k_info[o_ptr->k_idx].tried;
#endif
}


bool object_is_potion(object_type *o_ptr)
{
	return (k_info[o_ptr->k_idx].tval == TV_POTION);
}

bool object_is_shoukinkubi(object_type *o_ptr)
{
	int i;

	if (astral_mode) return FALSE;
	if (p_ptr->today_mon > 0 && (streq(r_name + r_info[o_ptr->pval].name, r_name + r_info[today_mon].name))) return TRUE;
	if (o_ptr->pval == MON_ZEBRA) return TRUE;
	for (i = 0; i < MAX_KUBI; i++)
		if (o_ptr->pval == kubi_r_idx[i]) break;
	if (i < MAX_KUBI) return TRUE;
	return FALSE;
}

bool object_is_runeweapon(object_type *o_ptr)
{
	if (!o_ptr->k_idx) return FALSE;
	return (k_info[o_ptr->k_idx].gen_flags & TRG_RUNEWEAPON) ? TRUE : FALSE;
}

bool object_is_snapdragon_runeweapon(object_type *o_ptr)
{
	if (object_is_runeweapon(o_ptr) && o_ptr->art_name)
	{
		if (o_ptr->xtra3 && (o_ptr->xtra3 <= runeweapon_num)) return TRUE;
		else if (p_ptr->is_dead & DEATH_SNAP_DRAGON)
		{
			if (o_ptr == &runeweapon_list[0].weapon) return TRUE;
		}
	}

	return FALSE;
}

bool object_is_astral_runeweapon(object_type *o_ptr)
{
	return (astral_mode && object_is_runeweapon(o_ptr) && o_ptr->art_name && (o_ptr->xtra3 == 1));
}

/*
 * Equipment including all wearable objects and ammo
 */
bool object_is_equipment(object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
	case TV_STONE: case TV_TAROT: case TV_SCRATCH_CARD:
	case TV_BULLET: case TV_ROUND: case TV_SHELL: case TV_ROCKET:
	case TV_ARROW: case TV_BOLT: case TV_BOW:
	case TV_DIGGING: case TV_HAFTED: case TV_POLEARM: case TV_SWORD: 
	case TV_BOOTS: case TV_GLOVES: case TV_HELM: case TV_CROWN:
	case TV_SHIELD: case TV_CLOAK:
	case TV_SOFT_ARMOR: case TV_HARD_ARMOR:
	case TV_LITE: case TV_AMULET: case TV_RING: case TV_CARD: case TV_TRUMP:
		return TRUE;

	default:
		return FALSE;
	}
}

/*
 * Rare weapons/aromors
 * including Blade of Chaos, Dragon armors, etc.
 */
bool object_is_rare(object_type *o_ptr)
{
	switch(o_ptr->tval)
	{
	case TV_TAROT:
		return TRUE;
		break;

/*	case TV_ARROW:
	case TV_BOLT:
		if (o_ptr->sval == SV_AMMO_HEAVY) return TRUE;
		break;
*/
	case TV_BOW:
		if (o_ptr->sval == SV_SNIPER_RIFLE ||
			o_ptr->sval == SV_SHOTGUN ||
			o_ptr->sval == SV_ROCKET_LAUNCHER) return TRUE;
		break;

	case TV_HAFTED:
		if (o_ptr->sval == SV_MACE_OF_DISRUPTION ||
		    o_ptr->sval == SV_WIZSTAFF) return TRUE;
		break;

	case TV_POLEARM:
		if (o_ptr->sval == SV_DEATH_SCYTHE) return TRUE;
		break;

	case TV_SWORD:
		if (o_ptr->sval == SV_YOUTOU ||
		    o_ptr->sval == SV_DARK_SWORD ||
		    o_ptr->sval == SV_DIAMOND_EDGE ||
		    o_ptr->sval == SV_MITHRIL_SWORD) return TRUE;
		break;

	case TV_SHIELD:
		if (o_ptr->sval == SV_DRAGON_SHIELD ||
		    o_ptr->sval == SV_MIRROR_SHIELD) return TRUE;
		break;

	case TV_HELM:
		if (o_ptr->sval == SV_DRAGON_HELM) return TRUE;
		break;

	case TV_BOOTS:
		if (o_ptr->sval == SV_PAIR_OF_DRAGON_GREAVE) return TRUE;
		break;

	case TV_CLOAK:
		if (o_ptr->sval == SV_BOLMARKAN_CLOAK ||
		    o_ptr->sval == SV_CLOAK_OF_IVORY_TOWER ||
		    o_ptr->sval == SV_SHADOW_CLOAK ||
		    o_ptr->sval == SV_SIR_COAT) return TRUE;
		break;

	case TV_GLOVES:
		if (o_ptr->sval == SV_SET_OF_DRAGON_GLOVES) return TRUE;
		break;

	case TV_SOFT_ARMOR:
		if (o_ptr->sval == SV_DRAGON_LEATHER_ARMOR ||
		    o_ptr->sval == SV_KUROSHOUZOKU ||
		    o_ptr->sval == SV_ROBE_DECOLLETE ||
		    o_ptr->sval == SV_ROBE_MONTANTE ||
		    o_ptr->sval == SV_SWALLOW_TAILED_COAT ||
		    o_ptr->sval == SV_FROCK_COAT) return TRUE;
		break;

	case TV_HARD_ARMOR:
		if (o_ptr->sval == SV_DRAGON_SCALE_MAIL) return TRUE;
		break;

	case TV_AMULET:
		if (o_ptr->sval == SV_AMULET_ALIGNMENT ||
		    o_ptr->sval == SV_AMULET_THE_MAGI ||
		    o_ptr->sval == SV_AMULET_REFLECTION ||
		    o_ptr->sval == SV_AMULET_RESISTANCE ||
		    o_ptr->sval == SV_AMULET_TELEPATHY ||
		    o_ptr->sval == SV_AMULET_FOL ||
		    o_ptr->sval == SV_AMULET_OHN ||
		    o_ptr->sval == SV_AMULET_SOL ||
		    o_ptr->sval == SV_AMULET_VAN) return TRUE;
		break;

	case TV_RING:
		if (o_ptr->sval == SV_RING_SHOTS ||
		    o_ptr->sval == SV_RING_SUSTAIN ||
		    o_ptr->sval == SV_RING_SPEED ||
		    o_ptr->sval == SV_RING_DEC_MANA ||
		    o_ptr->sval == SV_RING_RES_DISENCHANT ||
		    o_ptr->sval == SV_RING_LORDLY ||
		    o_ptr->sval == SV_RING_ATTACKS) return TRUE;
		break;

	case TV_CARD:
		return TRUE;
		break;

	case TV_TRUMP:
		return TRUE;
		break;

	case TV_FOOD:
		if (o_ptr->sval == SV_FOOD_INC_STR ||
		    o_ptr->sval == SV_FOOD_INC_INT ||
		    o_ptr->sval == SV_FOOD_INC_WIS ||
		    o_ptr->sval == SV_FOOD_INC_DEX ||
		    o_ptr->sval == SV_FOOD_INC_CON ||
		    o_ptr->sval == SV_FOOD_INC_CHR ||
		    o_ptr->sval == SV_FOOD_AUGMENTATION) return TRUE;
		break;

	default:
		break;
	}

	/* Any others are not "rare" objects. */
	return FALSE;
}
