/**
 * \file obj-make.c
 * \brief Object generation functions.
 *
 * Copyright (c) 1987-2007 Angband contributors
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
#include "alloc.h"
#include "cave.h"
#include "effects.h"
#include "init.h"
#include "obj-chest.h"
#include "obj-fault.h"
#include "obj-gear.h"
#include "obj-knowledge.h"
#include "obj-make.h"
#include "obj-pile.h"
#include "obj-power.h"
#include "obj-slays.h"
#include "obj-tval.h"
#include "obj-util.h"
#include <math.h>

/** Let the stats see into the artifact probability table */
extern double *wiz_stats_prob;

/**
 * Stores cumulative probability distribution for objects at each level.  The
 * value at ilv * (z_info->k_max + 1) + itm is the probablity, out of
 * obj_alloc[ilv * (z_info->k_max + 1) + z_info->k_max], that an item whose
 * index is less than itm occurs at level, ilv.
 */
static u32b *obj_alloc;

/**
 * Has the same layout and interpretation as obj_alloc, but only items that
 * are good or better contribute to the cumulative probability distribution.
 */
static u32b *obj_alloc_great;

/**
 * Store the total allocation value for each tval by level.  The value at
 * ilv * TV_MAX + tval is the total for tval at the level, ilv.
 */
static u32b *obj_total_tval;

/**
 * Same layout and interpretation as obj_total_tval, but only items that are
 * good or better contribute.
 */
static u32b *obj_total_tval_great;

static alloc_entry *alloc_ego_table;

struct money {
	char *name;
	int type;
};

static struct money *money_type;
static int num_money_types;

/*
 * Initialize object allocation info
 */
static void alloc_init_objects(void) {
	int item, lev;
	int k_max = z_info->k_max;

	/* Allocate */
	obj_alloc = mem_alloc((z_info->max_obj_depth + 1) * (k_max + 1) * sizeof(*obj_alloc));
	obj_alloc_great = mem_alloc((z_info->max_obj_depth + 1) * (k_max + 1) * sizeof(*obj_alloc_great));
	obj_total_tval = mem_zalloc((z_info->max_obj_depth + 1) * TV_MAX * sizeof(*obj_total_tval));
	obj_total_tval_great = mem_zalloc((z_info->max_obj_depth + 1) * TV_MAX * sizeof(*obj_total_tval));

	/* The cumulative chance starts at zero for each level. */
	for (lev = 0; lev <= z_info->max_obj_depth; lev++) {
		obj_alloc[lev * (k_max + 1)] = 0;
		obj_alloc_great[lev * (k_max + 1)] = 0;
	}

	/* Fill the cumulative probability tables */
	for (item = 0; item < k_max; item++) {
		const struct object_kind *kind = &k_info[item];

		int min = kind->alloc_min;
		int max = kind->alloc_max;

		/* Go through all the dungeon levels */
		for (lev = 0; lev <= z_info->max_obj_depth; lev++) {
			int rarity = kind->alloc_prob;

			/* Add to the cumulative prob. in the standard table */
			if ((lev < min) || (lev > max)) rarity = 0;
			assert(rarity >= 0 && obj_alloc[(lev * (k_max + 1)) + item] <= (u32b)-1 - rarity);
			obj_alloc[(lev * (k_max + 1)) + item + 1] =
				obj_alloc[(lev * (k_max + 1)) + item] + rarity;

			obj_total_tval[lev * TV_MAX + kind->tval] += rarity;

			/* Add to the cumulative prob. in the "great" table */
			if (!kind_is_good(kind)) rarity = 0;
			obj_alloc_great[(lev * (k_max + 1)) + item + 1] =
				obj_alloc_great[(lev * (k_max + 1)) + item] + rarity;

			obj_total_tval_great[lev * TV_MAX + kind->tval] += rarity;
		}
	}
}

/*
 * Initialize ego-item allocation info
 */
static void alloc_init_egos(void) {
	/* Allocate the alloc_ego_table */
	alloc_ego_table = mem_zalloc(z_info->e_max * sizeof(alloc_entry));

	/* Scan the ego-items */
	for (int i = 0; i < z_info->e_max; i++) {
		struct ego_item *ego = &e_info[i];

		/* Load the entry */
		alloc_ego_table[i].index = i;
		alloc_ego_table[i].prob2 = ego->alloc_prob;
	}
}

/*
 * Initialize money info
 */
static void init_money_svals(void)
{
	int *money_svals;
	int i;

	/* Count the money types and make a list */
	num_money_types = tval_sval_count("cash");
	money_type = mem_zalloc(num_money_types * sizeof(struct money));
	money_svals = mem_zalloc(num_money_types * sizeof(struct money));
	assert(num_money_types);
	tval_sval_list("cash", money_svals, num_money_types);

	/* List the money types */
	for (i = 0; i < num_money_types; i++) {
		struct object_kind *kind = lookup_kind(TV_GOLD, money_svals[i]);
		money_type[i].name = string_make(kind->name);
		money_type[i].type = money_svals[i];
	}

	mem_free(money_svals);
}

static void init_obj_make(void) {
	alloc_init_objects();
	alloc_init_egos();
	init_money_svals();
}

static void cleanup_obj_make(void) {
	int i;
	for (i = 0; i < num_money_types; i++) {
		string_free(money_type[i].name);
	}
	mem_free(money_type);
	mem_free(alloc_ego_table);
	mem_free(obj_total_tval_great);
	mem_free(obj_total_tval);
	mem_free(obj_alloc_great);
	mem_free(obj_alloc);
}

/*** Make an ego item ***/

/**
 * This is a safe way to choose a random new flag to add to an object.
 * It takes the existing flags and an array of new flags,
 * and returns an entry from newf, or 0 if there are no
 * new flags available.
 */
static int get_new_attr(bitflag flags[OF_SIZE], bitflag newf[OF_SIZE])
{
	size_t i;
	int options = 0, flag = 0;

	for (i = of_next(newf, FLAG_START); i != FLAG_END; i = of_next(newf, i + 1))
	{
		/* skip this one if the flag is already present */
		if (of_has(flags, i)) continue;

		/* each time we find a new possible option, we have a 1-in-N chance of
		 * choosing it and an (N-1)-in-N chance of keeping a previous one */
		if (one_in_(++options)) flag = i;
	}

	return flag;
}

/**
 * Get a random new base resist on an item
 */
static int random_base_resist(struct object *obj, int *resist)
{
	int i, r, count = 0;

	/* Count the available base resists */
	for (i = ELEM_BASE_MIN; i < ELEM_HIGH_MIN; i++)
		if (obj->el_info[i].res_level == 0) count++;

	if (count == 0) return false;

	/* Pick one */
	r = randint0(count);

	/* Find the one we picked */
	for (i = ELEM_BASE_MIN; i < ELEM_HIGH_MIN; i++) {
		if (obj->el_info[i].res_level != 0) continue;
		if (r == 0) {
			*resist = i;
			return true;
		}
		r--;
	}

	return false;
}

/**
 * Get a random new high resist on an item
 */
static int random_high_resist(struct object *obj, int *resist)
{
	int i, r, count = 0;

	/* Count the available high resists */
	for (i = ELEM_HIGH_MIN; i < ELEM_HIGH_MAX; i++)
		if (obj->el_info[i].res_level == 0) count++;

	if (count == 0) return false;

	/* Pick one */
	r = randint0(count);

	/* Find the one we picked */
	for (i = ELEM_HIGH_MIN; i < ELEM_HIGH_MAX; i++) {
		if (obj->el_info[i].res_level != 0) continue;
		if (r == 0) {
			*resist = i;
			return true;
		}
		r--;
	}

	return false;
}


/**
 * Return the index, i, into the cumulative probability table, tbl, such that
 * tbl[i] <= p < tbl[i + 1].  p must be less than tbl[n - 1] and tbl[0] must be
 * zero.
 */
static int binary_search_probtable(const u32b *tbl, int n, u32b p)
{
	int ilow = 0, ihigh = n;

	assert(tbl[0] == 0 && tbl[n - 1] > p);
	while (1) {
		int imid;

		if (ilow == ihigh - 1) {
			assert(tbl[ilow] <= p && tbl[ihigh] > p);
			return ilow;
		}
		imid = (ilow + ihigh) / 2;
		if (tbl[imid] <= p) {
			ilow = imid;
		} else {
			ihigh = imid;
		}
	}
}

/**
 * Randomly select from a table of probabilities
 * prob is a table of doubles, total is the sum of these values
 * Returns an index into the table
 */
 static int select_random_table(double total, double *prob, int length) {
	double value = Rand_double(total);
	int last = -1;
	for (int i=0;i<length; i++) {
		if (prob[i] > 0.0) {
			last = i;
			if (value < prob[i]) {
				return i;
			} else {
				value -= prob[i];
			}
		}
	}
	assert(last >= 0);
	return last;
}

/**
 * Select a base item for an ego.
 */
static struct object_kind *select_ego_kind(struct ego_item *ego, int level, int tval)
{
	struct poss_item *poss;
	double *prob = mem_zalloc(sizeof(*prob) * z_info->k_max);
	double total = 0.0;

	/* Fill a table of usable base items */
	for (poss = ego->poss_items; poss; poss = poss->next) {
		if ((tval == 0) || (tval == k_info[poss->kidx].tval)) {
			assert(poss->kidx);
			prob[poss->kidx] = obj_alloc[poss->kidx] - obj_alloc[poss->kidx-1];
			total += prob[poss->kidx];
		}
	}

	/* No possibilities */
	if (total == 0.0) {
		mem_free(prob);
		return NULL;
	}

	/* Select at random */
	int idx = select_random_table(total, prob, z_info->k_max);
	mem_free(prob);
	return k_info + idx;
}

/**
 * Return true if at least one item has a matching tval
 */
static bool ego_can_use_tval(struct ego_item *ego, int tval)
{
	struct poss_item *poss;

	for (poss = ego->poss_items; poss; poss = poss->next) {
		if (tval == k_info[poss->kidx].tval)
			return true;
	}

	return false;
}

/**
 * Select an ego-item at random, based on the level.
 */
static struct ego_item *ego_find_random(int level, int tval)
{
	int i;
	double *prob = mem_zalloc(sizeof(*prob) * z_info->e_max);
	struct alloc_entry *table = alloc_ego_table;
	double total = 0.0;

	/* Go through all possible ego items and find ones which are possible */
	for (i = 0; i < z_info->e_max; i++) {
		struct ego_item *ego = &e_info[i];
		double p = 0.0;

		if ((tval == 0) || (ego_can_use_tval(ego, tval))) {
			if (level <= ego->alloc_max) {
				p = table[i].prob2;
				if (level >= ego->alloc_min) {
					/* Between min and max levels - scale linearly
					 * from maximum probability at native depth to
					 * zero at maximum depth + 1
					 **/
					p *= (ego->alloc_max + 1) - level;
					p /= (ego->alloc_max + 1) - ego->alloc_min;
				} else {
					/* Out of depth.
					 * Divide by the # of levels OOD, * a constant
					 **/
					p /= 1.0 + (((double)(ego->alloc_min - level)) / 3.0);
				}
			}
		}

		prob[i] = p;
		total += prob[i];
	}

	/* No possibilities */
	if (total == 0.0) {
		mem_free(prob);
		return NULL;
	}

	/* Select at random */
	int idx = select_random_table(total, prob, z_info->e_max);
	mem_free(prob);
	return e_info + idx;
}


/**
 * Apply generation magic to an ego-item.
 */
void ego_apply_magic(struct object *obj, int level)
{
	int i, x, resist = 0, pick = 0;
	bitflag newf[OF_SIZE];

	/* Resist or power? */
	if (kf_has(obj->ego->kind_flags, KF_RAND_RES_POWER))
		pick = randint1(3);

	/* Extra powers */
	if (kf_has(obj->ego->kind_flags, KF_RAND_SUSTAIN)) {
		create_obj_flag_mask(newf, false, OFT_SUST, OFT_MAX);
		of_on(obj->flags, get_new_attr(obj->flags, newf));
	} else if (kf_has(obj->ego->kind_flags, KF_RAND_POWER) || (pick == 1)) {
		create_obj_flag_mask(newf, false, OFT_PROT, OFT_MISC, OFT_MAX);
		of_on(obj->flags, get_new_attr(obj->flags, newf));
	} else if (kf_has(obj->ego->kind_flags, KF_RAND_BASE_RES) || (pick > 1)) {
		/* Get a base resist if available, mark it as random */
		if (random_base_resist(obj, &resist)) {
			obj->el_info[resist].res_level = 1;
			obj->el_info[resist].flags |= EL_INFO_RANDOM | EL_INFO_IGNORE;
		}
	} else if (kf_has(obj->ego->kind_flags, KF_RAND_HI_RES)) {
		/* Get a high resist if available, mark it as random */
		if (random_high_resist(obj, &resist)) {
			obj->el_info[resist].res_level = 1;
			obj->el_info[resist].flags |= EL_INFO_RANDOM | EL_INFO_IGNORE;
		}
	}

	/* Apply extra obj->ego bonuses */
	obj->to_h += randcalc(obj->ego->to_h, level, RANDOMISE);
	obj->to_d += randcalc(obj->ego->to_d, level, RANDOMISE);
	obj->to_a += randcalc(obj->ego->to_a, level, RANDOMISE);

	/* Apply pval */
	obj->pval = MAX(obj->pval, randcalc(obj->ego->pval, level, RANDOMISE));
	int weightmul = randcalc(obj->ego->weight, level, RANDOMISE);
	if (weightmul != 0) {
		obj->weight = ((obj->weight * weightmul) + (obj->weight / 2)) / 100;
	}

	/* Apply modifiers */
	for (i = 0; i < OBJ_MOD_MAX; i++) {
		x = randcalc(obj->ego->modifiers[i], level, RANDOMISE);
		obj->modifiers[i] += x;
	}

	/* Apply flags */
	of_union(obj->flags, obj->ego->flags);
	of_diff(obj->flags, obj->ego->flags_off);
	of_union(obj->carried_flags, obj->ego->flags);
	of_diff(obj->carried_flags, obj->ego->flags_off);
	pf_union(obj->pflags, obj->ego->pflags);

	/* Add slays, brands and faults */
	copy_slays(&obj->slays, obj->ego->slays);
	copy_brands(&obj->brands, obj->ego->brands);
	copy_faults(obj, obj->ego->faults);

	/* Add resists */
	for (i = 0; i < ELEM_MAX; i++) {
		/* Take the larger of ego and base object resist levels */
		obj->el_info[i].res_level =
			MAX(obj->ego->el_info[i].res_level, obj->el_info[i].res_level);

		/* Union of flags so as to know when ignoring is notable */
		obj->el_info[i].flags |= obj->ego->el_info[i].flags;
	}

	/* Add effect (ego effect will trump object effect, when there are any) */
	if (obj->ego->effect) {
		obj->effect = obj->ego->effect;
		obj->time = obj->ego->time;
	}

	return;
}

/**
 * Apply minimum standards for ego-items.
 */
static void ego_apply_minima(struct object *obj)
{
	int i;

	if (!obj->ego) return;

	if (obj->ego->min_to_h != NO_MINIMUM &&
			obj->to_h < obj->ego->min_to_h)
		obj->to_h = obj->ego->min_to_h;
	if (obj->ego->min_to_d != NO_MINIMUM &&
			obj->to_d < obj->ego->min_to_d)
		obj->to_d = obj->ego->min_to_d;
	if (obj->ego->min_to_a != NO_MINIMUM &&
			obj->to_a < obj->ego->min_to_a)
		obj->to_a = obj->ego->min_to_a;

	for (i = 0; i < OBJ_MOD_MAX; i++) {
		if (obj->modifiers[i] < obj->ego->min_modifiers[i])
			obj->modifiers[i] = obj->ego->min_modifiers[i];
	}
}


/**
 * Try to find an ego-item for an object, setting obj->ego if successful and
 * applying various bonuses.
 */
static struct ego_item *find_ego_item(int level, int tval)
{
	/* Try to get a legal ego type for this item */
	return ego_find_random(level, tval);
}


/*** Make an artifact ***/

/**
 * Copy artifact data to a normal object.
 */
void copy_artifact_data(struct object *obj, const struct artifact *art)
{
	int i;
	struct object_kind *kind = lookup_kind(art->tval, art->sval);

	/* Extract the data */
	for (i = 0; i < OBJ_MOD_MAX; i++)
		obj->modifiers[i] = art->modifiers[i];
	obj->ac = art->ac;
	obj->dd = art->dd;
	obj->ds = art->ds;
	obj->to_a = art->to_a;
	obj->to_h = art->to_h;
	obj->to_d = art->to_d;
	obj->weight = art->weight;

	/* Activations can come from the artifact or the kind */
	if (art->activation) {
		obj->activation = art->activation;
		obj->time = art->time;
	} else if (kind->activation) {
		obj->activation = kind->activation;
		obj->time = kind->time;
	}

	/* Fix for artifact lights */
	of_off(obj->flags, OF_TAKES_FUEL);
	of_off(obj->flags, OF_BURNS_OUT);

	/* Timeouts are always 0 */
	obj->timeout = 0;

	of_union(obj->flags, art->flags);
	of_union(obj->carried_flags, art->flags);
	pf_union(obj->pflags, art->pflags);
	copy_slays(&obj->slays, art->slays);
	copy_brands(&obj->brands, art->brands);
	copy_faults(obj, art->faults);
	for (i = 0; i < ELEM_MAX; i++) {
		/* Take the larger of artifact and base object resist levels */
		obj->el_info[i].res_level =
			MAX(art->el_info[i].res_level, obj->el_info[i].res_level);

		/* Union of flags so as to know when ignoring is notable */
		obj->el_info[i].flags |= art->el_info[i].flags;
	}
}

static double make_artifact_probs(double *prob, int lev, int tval, bool max)
{
	double total = 0.0;
	for (int i = 0; i < z_info->a_max; i++) {
		struct artifact *art = &a_info[i];
		struct object_kind *kind = lookup_kind(art->tval, art->sval);

		/* No chance by default */
		prob[i] = 0.0;

		/* Skip "empty" items */
		if (!art->name) continue;

		/* Make sure the kind was found */
		if (!kind) continue;

		/* Cannot make an artifact twice */
		if (art->created) continue;

		/* Must have the correct tval, if one is provided */
		if ((tval != 0) && (art->tval != tval)) continue;

		/* Rarity - sets the basic probability */
		prob[i] = art->alloc_prob;

		/* Enforce maximum depth (strictly, if max mode is set) */
		if (art->alloc_max <= lev) {
			if (max) {
				prob[i] = 0.0;
			} else {
				/* Get the "out-of-depth factor" */
				prob[i] /= ((lev - art->alloc_max) + 1) * 10;
			}
		}

		/* Enforce minimum "depth" (loosely) */
		else if (art->alloc_min > lev) {
			/* Get the "out-of-depth factor" */
			prob[i] /= 1.0 + ((art->alloc_min - lev) * (art->alloc_min - lev) * 0.1);
		}

		/* If in depth, reduce probability at higher levels */
		else {
			prob[i] *= (art->alloc_max + 1) - lev;
			prob[i] /= (art->alloc_max + 1) - art->alloc_min;
		}

		total += prob[i];
	}
	return total;
}

/**
 * Attempt to create an artifact.
 *
 * With the exceptions of being in town or having artifacts turned
 * off, this will always create an artifact if there are any artifacts
 * left to create - or if tval is specified, any artifacts left of
 * that tval. This means that even though the maximum depth is
 * otherwise enforced strictly, if this is the only way to produce
 * an artifact then the max depth will be treated as a soft limit.
 *
 * This routine should only be called by "apply_magic()" and stats.
 *
 * Note -- see "apply_magic()"
 */
struct object *make_artifact(int lev, int tval)
{
	static int prev_lev = -1;
	static int prev_tval = -1;
	static double *prob;
	static double total;

	if (!prob)
		prob = mem_zalloc(z_info->a_max * sizeof(double));

	wiz_stats_prob = prob;

	struct object *obj = NULL;

	/* Make sure birth no artifacts isn't set */
	if (OPT(player, birth_no_artifacts)) return NULL;

	/* No artifacts in the town */
	if (!player->depth) return NULL;

	/* Check the artifact list */
	if ((lev != prev_lev) || (tval != prev_tval)) {
		total = make_artifact_probs(prob, lev, tval, true);
		if (total == 0.0) {
			/* No matches. Try with loose maximum depth */
			total = make_artifact_probs(prob, lev, tval, false);
		}
	}

	/* Still nothing - give up */
	if (total == 0.0)
		return NULL;

	/* Select an artifact.
	 * Generate a random value between 0 and the total probability, scan the array until
	 * the running sum exceeds it.
	 **/
	double rand = Rand_double(total);
	double sum = 0.0;
	int a_idx = 0;
	for (a_idx=0;a_idx<z_info->a_max;a_idx++) {
		sum += prob[a_idx];
		if (rand < sum) {
			break;
		}
	}
	assert(a_idx < z_info->a_max);

	/* Generate the base item */
	struct artifact *art = &a_info[a_idx];
	struct object_kind *kind = lookup_kind(art->tval, art->sval);
	obj = object_new();
	object_prep(obj, kind, art->alloc_min, RANDOMISE);

	/* Mark the item as an artifact */
	obj->artifact = art;
	copy_artifact_data(obj, obj->artifact);
	obj->artifact->created = true;

	return obj;
}


/**
 * Create a fake artifact directly from a blank object
 *
 * This function is used for describing artifacts, and for creating them for
 * debugging.
 *
 * Since this is now in no way marked as fake, we must make sure this function
 * is never used to create an actual game object
 */
bool make_fake_artifact(struct object *obj, const struct artifact *artifact)
{
	struct object_kind *kind;

	/* Don't bother with empty artifacts */
	if (!artifact->tval) return false;

	/* Get the "kind" index */
	kind = lookup_kind(artifact->tval, artifact->sval);
	if (!kind) return false;

	/* Create the artifact */
	object_prep(obj, kind, 0, MAXIMISE);
	obj->artifact = (struct artifact *)artifact;
	copy_artifact_data(obj, artifact);

	return (true);
}


/*** Apply magic to an item ***/

/**
 * Apply magic to a weapon.
 */
static void apply_magic_weapon(struct object *obj, int level, int power)
{
	if (power <= 0)
		return;

	obj->to_h += randint1(5) + m_bonus(5, level);
	obj->to_d += randint1(5) + m_bonus(5, level);

	if (power > 1) {
		obj->to_h += m_bonus(10, level);
		obj->to_d += m_bonus(10, level);

		if (tval_is_melee_weapon(obj)) {
			/* Super-charge the damage dice */
			while ((obj->dd * obj->ds > 0) && one_in_(4 * obj->dd * obj->ds)) {
				/* More dice or sides means more likely to get still more */
				if (randint0(obj->dd + obj->ds) < obj->dd) {
					int newdice = randint1(2 + obj->dd/obj->ds);
					while (((obj->dd + 1) * obj->ds <= 40) && newdice) {
						if (!one_in_(3)) {
							obj->dd++;
						}
						newdice--;
					}
				} else {
					int newsides = randint1(2 + obj->ds/obj->dd);
					while ((obj->dd * (obj->ds + 1) <= 40) && newsides) {
						if (!one_in_(3)) {
							obj->ds++;
						}
						newsides--;
					}
				}
			}
		} else if (tval_is_ammo(obj)) {
			/* Up to two chances to enhance damage dice. */
			if (one_in_(6) == 1) {
				obj->ds++;
				if (one_in_(10) == 1) {
					obj->ds++;
				}
			}
		}
	}
}


/**
 * Apply magic to armour
 */
static void apply_magic_armour(struct object *obj, int level, int power)
{
	if (power <= 0)
		return;

	obj->to_a += randint1(5) + m_bonus(5, level);
	if (power > 1)
		obj->to_a += m_bonus(10, level);
}


/**
 * Wipe an object clean and make it a standard object of the specified kind.
 */
void object_prep(struct object *obj, struct object_kind *k, int lev,
				 aspect rand_aspect)
{
	int i;

	/* Clean slate */
	memset(obj, 0, sizeof(*obj));

	/* Assign the kind and copy across data */
	obj->kind = k;
	obj->tval = k->tval;
	obj->sval = k->sval;
	obj->ac = k->ac;
	obj->dd = k->dd;
	obj->ds = k->ds;
	obj->weight = k->weight;
	obj->effect = k->effect;
	obj->time = k->time;

	/* Default number */
	obj->number = 1;

	/* Copy flags */
	of_copy(obj->flags, k->base->flags);
	of_union(obj->flags, k->flags);
	of_copy(obj->carried_flags, k->carried_flags);
	pf_copy(obj->pflags, k->pflags);

	/* Assign modifiers */
	for (i = 0; i < OBJ_MOD_MAX; i++)
		obj->modifiers[i] = randcalc(k->modifiers[i], lev, rand_aspect);

	/* Assign charges (wands/devices only) */
	if (tval_can_have_charges(obj))
		obj->pval = randcalc(k->charge, lev, rand_aspect);

	/* Assign pval for food, batteries, pills, printers and launchers */
	if (tval_is_edible(obj) || tval_is_pill(obj) || tval_is_fuel(obj) ||
		tval_is_launcher(obj) || tval_is_printer(obj))
			obj->pval = randcalc(k->pval, lev, rand_aspect);

	/* Default fuel */
	if (tval_is_light(obj))
		obj->timeout = randcalc(k->pval, lev, rand_aspect);

	/* Default magic */
	obj->to_h = randcalc(k->to_h, lev, rand_aspect);
	obj->to_d = randcalc(k->to_d, lev, rand_aspect);
	obj->to_a = randcalc(k->to_a, lev, rand_aspect);

	/* Default slays, brands and faults */
	copy_slays(&obj->slays, k->slays);
	copy_brands(&obj->brands, k->brands);
	copy_faults(obj, k->faults);

	/* Default resists */
	for (i = 0; i < ELEM_MAX; i++) {
		obj->el_info[i].res_level = k->el_info[i].res_level;
		obj->el_info[i].flags = k->el_info[i].flags;
		obj->el_info[i].flags |= k->base->el_info[i].flags;
	}
}

/**
 * Attempt to apply faults to an object, with a corresponding increase in
 * generation level of the object
 */
static int apply_fault(struct object *obj, int lev)
{
	int pick, max_faults = randint1(4);
	int power = randint1(9) + 10 * m_bonus(9, lev);
	int new_lev = lev;

	if (of_has(obj->flags, OF_BLESSED)) return lev;

	while (max_faults--) {
		/* Try to break it */
		int tries = 3;
		while (tries--) {
			pick = randint1(z_info->fault_max - 1);
			if (faults[pick].poss[obj->tval]) {
				if (append_object_fault(obj, pick, power)) {
					new_lev += randint1(1 + power / 10);
				}
				break;
			}
		}
	}

	return new_lev;
}

/**
 * Applying magic to an object, which includes creating ego-items, and applying
 * random bonuses,
 *
 * The `good` argument forces the item to be at least `good`, and the `great`
 * argument does likewise.  Setting `allow_artifacts` to true allows artifacts
 * to be created here.
 *
 * If `good` or `great` are not set, then the `lev` argument controls the
 * quality of item.
 *
 * Returns 0 if a normal object, 1 if a good object, 2 if an ego item, 3 if an
 * artifact.
 */

int apply_magic(struct object *obj, int lev, bool allow_artifacts, bool good,
				bool great, bool extra_roll)
{
	s16b power = 0;

	/* It's "good" */
	if (good) {
		power = 1;

		/* It's "great" */
		if (great)
			power = 2;
	}

	/* Give it a chance to be faulty */
	if (one_in_(20) && tval_is_wearable(obj)) {
		lev = apply_fault(obj, lev);
	}

	/* Apply magic */
	if (tval_is_weapon(obj)) {
		apply_magic_weapon(obj, lev, power);
	} else if (tval_is_armor(obj)) {
		apply_magic_armour(obj, lev, power);
	} else if (tval_is_chest(obj)) {
		/* Get a random, level-dependent set of chest traps */
		obj->pval = pick_chest_traps(obj);
	}

	/* Apply minima from ego items if necessary */
	ego_apply_minima(obj);

	return power;
}


/*** Generate a random object ***/

/**
 * Hack -- determine if a template is "good".
 *
 * Note that this test only applies to the object *kind*, so it is
 * possible to choose a kind which is "good", and then later cause
 * the actual object to be faulty.  We do explicitly forbid objects
 * which are known to be boring or which start out somewhat damaged.
 */
bool kind_is_good(const struct object_kind *kind)
{
	/* Some item types are (almost) always good */

	/* Armor -- Good unless damaged */
	if (kind_tval_is_armor(kind)) {
		if (randcalc(kind->to_a, 0, MINIMISE) < 0) return (false);
		return true;
	}

	/* Weapons, including melee, guns and ammo -- Good unless damaged */
	if (kind_tval_is_weapon(kind)) {
		if (randcalc(kind->to_h, 0, MINIMISE) < 0) return (false);
		if (randcalc(kind->to_d, 0, MINIMISE) < 0) return (false);
		return true;
	}

	/* Anything with the GOOD flag */
	if (kf_has(kind->kind_flags, KF_GOOD))
		return true;

	/* Assume not good */
	return (false);
}


/**
 * Choose an object kind of a given tval given a dungeon level.
 */
static struct object_kind *get_obj_num_by_kind(int level, bool good, int tval)
{
	const u32b *objects;
	u32b total = 0;
	u32b value;
	int item;

	assert(level >= 0 && level <= z_info->max_obj_depth);
	assert(tval >= 0 && tval < TV_MAX);
	if (good) {
		objects = obj_alloc_great + level * (z_info->k_max + 1);
		total = obj_total_tval_great[level * TV_MAX + tval];
	}
	if (!total) {
		objects = obj_alloc + level * (z_info->k_max + 1);
		total = obj_total_tval[level * TV_MAX + tval];
	}

	/* No appropriate items of that tval */
	if (!total) return NULL;

	/* Pick an object */
	value = randint0(total);

	/*
	 * Find it.  Having a loop to calculate the cumulative probability
	 * here with only the tval and applying a binary search was slower
	 * for a test of getting a TV_SWORD from 4.2's available objects.
	 * So continue to use the O(N) search.
	 */
	for (item = 0; item < z_info->k_max; item++) {
		if (objkind_byid(item)->tval == tval) {
			u32b prob = objects[item + 1] - objects[item];

			if (value < prob) break;
			value -= prob;
		}
	}

	/* Return the item index */
	return objkind_byid(item);
}

/**
 * Choose an object kind given a dungeon level to choose it for.
 * If tval = 0, we can choose an object of any type.
 * Otherwise we can only choose one of the given tval.
 */
struct object_kind *get_obj_num(int level, bool good, int tval)
{
	const u32b *objects;
	u32b value;
	int item;

	/* Occasional level boost */
	if ((level > 0) && one_in_(z_info->great_obj))
		/* What a bizarre calculation */
		level = 1 + (level * z_info->max_obj_depth / randint1(z_info->max_obj_depth));

	/* Paranoia */
	level = MIN(level, z_info->max_obj_depth);
	level = MAX(level, 0);

	if (tval)
		return get_obj_num_by_kind(level, good, tval);

	objects = (good ? obj_alloc_great : obj_alloc) + (level * (z_info->k_max + 1));

	if ((!objects[z_info->k_max]) && good)
		objects = obj_alloc + (level * (z_info->k_max + 1));

	/* Pick an object. */
	if (!objects[z_info->k_max]) {
		return NULL;
	}
	value = randint0(objects[z_info->k_max]);

	/* Find it with a binary search. */
	item = binary_search_probtable(objects, z_info->k_max + 1, value);

	/* Return the item index */
	return objkind_byid(item);
}

static double artifact_prob(double depth)
{
	/* Debug: print the artifact generation probability table */
	static bool first = false;
	if (first) {
		first = false;
		for(int i=1;i<=100;i++) {
			double d = artifact_prob(i);
			fprintf(stderr,"l%d, prob %lf\n", i, d);
		}
	}
	
	/* The following weird math is a combination of two ease curves (S shape, 3x^2-2x^3).
	 * 'Full' gives the overall shape.
	 * 'Mid' gives some boost at midlevels centered at 'midpoint'.
	 *  Finally there is a small constant addition to make things more generous at low levels.
	 */
	double x = MIN(depth, 100) * 0.01;
	double full = ((x*x)*3)-((x*x*x)*2);
	double midpoint = 0.1;
	double midlin = (x < midpoint) ? (x / midpoint) : (1.0 - x) / (1.0 - midpoint);
	double mid = ((midlin*midlin)*3)-((midlin*midlin*midlin)*2);
	double chance = (full + (mid * 0.5)) / 140;
	chance += 0.0002;
	return chance;
}

static double ego_prob(double depth, bool good, bool great)
{
	/* Debug: print the ego generation probability table */
	static bool first = false;
	if (first) {
		first = false;
		for(int i=1;i<=100;i++) {
			double d = ego_prob(i, false, false);
			fprintf(stderr,"l%d, prob %lf\n", i, d);
		}
	}

	/* Chance of being `good` and `great` */
	/* This has changed over the years:
	 * 3.0.0:   good = MIN(75, lev + 10);      great = MIN(20, lev / 2);
	 * 3.3.0:   good = (lev + 2) * 3;          great = MIN(lev / 4 + lev, 50);
	 * 3.4.0:   good = (2 * lev) + 5
	 * 3.4 was in between 3.0 and 3.3, 3.5 attempts to keep the same
	 * area under the curve as 3.4, but make the generation chances
	 * flatter.  This depresses good items overall since more items
	 * are created deeper.
	 * This change is meant to go in conjunction with the changes
	 * to ego item allocation levels. (-fizzix)
	 *
	 * 4.2.x:   great = 30, great = 10% at level 0 .. 30% at level 66+;
	 *
	 * (MS) New ego generation needs lower chances, as every ego asked for
	 * is obtained. This also means that great = 1.0 would disqualify
	 * any item that can't be made into an ego item - including !oExp
	 * and similar valuables.
	 *
	 * To mimic the original odds would need to reduce to 0.274 at level 1,
	 * 0.280 at level 33, 0.342 at level 70, 0.377 at level 98. But this
	 * is only a rough guideline (it will change with the ego and item
	 * lists, and is based on egos-per-depth generated, not actual levels.
	 * The original is also probably too ego heavy later on.)
	 *
	 * Using 'diving' instead:
	 * Level	Original	New 1/.3/.1	Ratio	.8/.125/.025	.8/.15/.0325
	 * 5		0.765		2.165		2.83		0.568		0.765
	 * 10		1.202		3.117		2.593		0.786		1.074
	 * 20		2.230		5.399		2.421		1.712		2.087
	 * 40		5.254		13.584		2.459		4.190		5.474
	 * 70		16.56		33.25		2.007		8.752		10.560
	 * 95		36.70		65.86		1.794		15.076		18.997
	 * 
	 * What proportion of 'great' items should be 'ego' should probably depend
	 * on what the character is likely to want at that point.
	 * 	- at level 1, any ego item is a big win
	 *  - at level 15, you might have something better already, but chances are still good.
	 *  - at level 30, you have a few artifacts and the rest mostly egos. And you want stat gain pills.
	 *  - at level 50, you have mostly artifacts. There are only a few ego items which would be an improvement. But you want gain/heal pills.
	 *  - at level 90, you have probably all good artifacts. Almost all ego items are chaff. But you want endgame consumables.
	 * 
	 * The same may apply to some extent for good and normal treasures - but this is going against wanting less egos early.
	 * (The majority of early egos are from the floor. The majority of late ones are from monsters.)
	 * 
	 * The following chances are a close match to original results based on diving sims below level 40.
	 * They then reduce, until being only about half as generous below level 95.
	 * Both these are (probably) Good Things.
	 */
	if (great)
		return 0.8 - (MIN(depth, 95.0) / (0.6 / 95.0));		/* 80% at level 0, 20% at level 95+ */
	else if (good)
		return 0.15;										/* 15% */
	else
		return 0.0325 + ((MIN(depth, 40) / 40.0) * 0.0675);	/* 3.25% at level 0, 10% at level 40+ */
}

/**
 * Attempt to make an object
 *
 * \param c is the current dungeon level.
 * \param lev is the creation level of the object (not necessarily == depth).
 * \param good is whether the object is to be good
 * \param great is whether the object is to be great
 * \param extra_roll is whether we get an extra roll in apply_magic()
 * \param value is the value to be returned to the calling function
 * \param tval is the desired tval, or 0 if we allow any tval
 * \param name is the desired object's name, or NULL if we allow any item (of that tval)
 *
 * \return a pointer to the newly allocated object, or NULL on failure.
 */
struct object *make_object_named(struct chunk *c, int lev, bool good, bool great,
						   bool extra_roll, s32b *value, int tval, const char *name)
{
	int base, tries = 3;
	struct object_kind *kind = NULL;
	struct object *new_obj = NULL;
	bool makeego = false;
	double chance = 0.0;
	double random = Rand_double(1.0);

	/* Try to make an artifact.
	 * There are no artifacts in the town, more are generated at depth and
	 * the chances go up for good, great or extra.
	 **/
	if ((!name) && (player->depth)) {
		double depth = lev;
		if (extra_roll)
			depth = (depth * 1.15) + 20;
		else if (great)
			depth = (depth * 1.1) + 15;
		else if (good)
			depth = (depth * 1.05) + 5;
		chance = artifact_prob(depth);

		if (random < chance) {
			/* This will always create an artifact if there are any artifacts
			 * left to create - or if tval is specified, any artifacts left of
			 * that tval.
			 **/
			new_obj = make_artifact(lev, tval);
			if (!new_obj) {
				good = true;
				great = true;
			} else {
				kind = new_obj->kind;
			}
		}
	}

	/* If an artifact hasn't been generated and a named item wasn't
	 * specified, try to create an ego item
	 **/
	if ((!name) && (!new_obj)) {
		double egochance = ego_prob(lev, good, great);
		random -= chance;
		if (random < egochance) {
			/* Make an ego item */
			makeego = true;

			/* Occasionally boost the generation level of an ego item */
			if (lev > 0 && one_in_(z_info->great_ego)) {
				lev = 1 + (lev * z_info->max_depth / randint1(z_info->max_depth));

				/* Ensure valid allocation level */
				if (lev >= z_info->max_depth)
					lev = z_info->max_depth - 1;
			}
		}
	}

	if (!new_obj) {
		struct ego_item *ego = NULL;

		/* Base level for the object */
		base = (good ? (lev + 10) : lev);

		if (name) {
			/* Use the given name, and either the given tval or try all */
			int sval = -1;

			if (tval) {
				sval = lookup_sval(tval, name);
			} else {
				for(tval=0; tval<TV_MAX; tval++) {
					sval = lookup_sval(tval, name);
					if (sval >= 0)
						break;
				}
			}
			if (sval >= 0) {
				kind = lookup_kind(tval, sval);
			}
		} else {
			if (makeego) {
				/* Select an ego item. This might fail */
				ego = find_ego_item(lev, tval);
			}
			if (ego) {
				/* Choose from the ego's allowed kinds */
				kind = select_ego_kind(ego, lev, tval);
			} else {
				/* Try to choose an object kind */
				while (tries--) {
					kind = get_obj_num(base, good || great, tval);
					break;
				}
			}
		}
		if (!kind)
			return NULL;

		/* Make the object, prep it and apply magic */
		new_obj = object_new();
		object_prep(new_obj, kind, lev, RANDOMISE);

		/* Actually apply the ego template to the item */
		if (ego) {
			assert(!new_obj->ego);
			new_obj->ego = ego;
			ego_apply_magic(new_obj, lev);
		}
		apply_magic(new_obj, lev, true, good, great, extra_roll);

		/* Generate multiple items */
		if (!new_obj->artifact && kind->gen_mult_prob >= randint1(100))
			new_obj->number = randcalc(kind->stack_size, lev, RANDOMISE);
	}

	if (new_obj->number > new_obj->kind->base->max_stack)
		new_obj->number = new_obj->kind->base->max_stack;

	/* Get the value */
	if (value)
		*value = object_value_real(new_obj, new_obj->number);

	/* Boost of 20% per level OOD for non-faulty objects */
	if ((!new_obj->faults) && (kind->alloc_min > c->depth)) {
		if (value) *value += (kind->alloc_min - c->depth) * (*value / 5);
	}

	return new_obj;
}


/**
 * Attempt to make an object
 *
 * \param c is the current dungeon level.
 * \param lev is the creation level of the object (not necessarily == depth).
 * \param good is whether the object is to be good
 * \param great is whether the object is to be great
 * \param extra_roll is whether we get an extra roll in apply_magic()
 * \param value is the value to be returned to the calling function
 * \param tval is the desired tval, or 0 if we allow any tval
 *
 * \return a pointer to the newly allocated object, or NULL on failure.
 */
struct object *make_object(struct chunk *c, int lev, bool good, bool great,
						   bool extra_roll, s32b *value, int tval)
{
	return make_object_named(c, lev, good, great, extra_roll, value, tval, NULL);
}

/**
 * Scatter some objects near the player
 */
void do_acquirement(struct loc grid, int level, int num, bool good, bool great)
{
	struct object *nice_obj;

	/* Acquirement */
	while (num) {
		/* Make a good (or great) object (if possible) */
		nice_obj = make_object(cave, level, good, great, true, NULL, 0);
		if (!nice_obj) continue;

		num--;
		nice_obj->origin = ORIGIN_ACQUIRE;
		nice_obj->origin_depth = player->depth;

		/* Drop the object */
		drop_near(cave, &nice_obj, 0, grid, true, false);
	}
}

/**
 * Scatter some objects near the player
 */
void acquirement(struct loc grid, int level, int num, bool great)
{
	do_acquirement(grid, level, num, true, great);
}


/*** Make a gold item ***/

/**
 * Get a money kind by name, or level-appropriate 
 */
struct object_kind *money_kind(const char *name, int value)
{
	int rank = num_money_types;
	int max_gold_drop = (3 + z_info->max_depth + ((z_info->max_depth * z_info->max_depth) / 25)) * 10;

	/* Check for specified treasure variety */
	if (name) {
		for (rank = 0; rank < num_money_types; rank++)
			if (streq(name, money_type[rank].name))
				break;
	}

	/* Pick a treasure variety scaled by level */
	if (rank == num_money_types) {
		double lv = log(value);
		double lm = log(max_gold_drop);
		double maxrrank = ((lv / lm) * (num_money_types + 8.0)) - 8.0;
		int maxrank = maxrrank;

		/* Do not create illegal treasure types */
		if (maxrank >= num_money_types) maxrank = num_money_types - 1;
		if (maxrank < 0) maxrank = 0;

		int minrank = maxrank - (maxrank / 4);

		rank = minrank + randint0(1 + maxrank - minrank);

		while ((randint0(rank+4) <= 2) && (rank < num_money_types - 1))
			rank++;
	}
	return lookup_kind(TV_GOLD, money_type[rank].type);
}

/**
 * Make a money object
 *
 * \param lev the dungeon level
 * \param coin_type the name of the type of money object to make
 * \return a pointer to the newly minted cash (cannot fail)
 */
struct object *make_gold(int lev, char *coin_type)
{
	struct object *new_gold = mem_zalloc(sizeof(*new_gold));
	int value;

	/* Repeat until a value below SHRT_MAX is found (as the roll is open ended, and pvals are 16 bit) */
	do {
		int avg = 3 + lev + ((lev * lev) / 25);
		int spread = avg;
		do {
			value = rand_spread(avg, spread);
		} while (value <= 0);

		/* Increase the range to infinite.
		 * Don't do this in the town.
		 * Be more generous if no-selling
		 **/
		int exploder = 25000 / (lev + 3);

		if (OPT(player, birth_no_selling))
			exploder = (5000 / (lev + 7))+150;
		
		if (exploder < 20)
			exploder = 20;

		if (player->depth) {
			while ((randint0(exploder) < 100) && value <= (INT_MAX / 10))
				value *= 10;
		}
	} while (value >= SHRT_MAX);
	/* Prepare a gold object */
	object_prep(new_gold, money_kind(coin_type, value), lev, RANDOMISE);

	new_gold->pval = value;

	return new_gold;
}

struct init_module obj_make_module = {
	.name = "object/obj-make",
	.init = init_obj_make,
	.cleanup = cleanup_obj_make
};
