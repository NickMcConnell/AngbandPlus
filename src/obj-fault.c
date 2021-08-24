/**
 * \file obj-fault.c
 * \brief functions to deal with object faults
 *
 * Copyright (c) 2016 Nick McConnell
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
#include "effects.h"
#include "init.h"
#include "obj-fault.h"
#include "obj-gear.h"
#include "obj-knowledge.h"
#include "obj-pile.h"
#include "obj-util.h"
#include "player-timed.h"
#include "player-util.h"

struct fault *faults;

/**
 * Return the index of the fault with the given name
 */
int lookup_fault(const char *name)
{
	int i;

	for (i = 1; i < z_info->fault_max; i++) {
		struct fault *fault = &faults[i];
		if (fault->name && streq(name, fault->name))
			return i;
	}
	return 0;
}

/**
 * Copy all the faults from a template to an actual object.
 *
 * \param obj the object the faults are being attached to
 * \param source the faults being copied
 */
void copy_faults(struct object *obj, int *source)
{
	int i;

	if (!source) return;

	if (!obj->faults) {
		obj->faults = mem_zalloc(z_info->fault_max * sizeof(struct fault_data));
	}

	for (i = 0; i < z_info->fault_max; i++) {
		if (!source[i]) continue;
		obj->faults[i].power = source[i];

		/* Timeouts need to be set for new objects */
		obj->faults[i].timeout = randcalc(faults[i].obj->time, 0, RANDOMISE);
	}
}

/**
 * Check whether two objects have the exact same faults
 *
 * \param obj1 the first object
 * \param obj2 the second object
 */
bool faults_are_equal(const struct object *obj1, const struct object *obj2)
{
	int i;

	if (!obj1->faults && !obj2->faults) return true;
	if (obj1->faults && !obj2->faults) return false;
	if (!obj1->faults && obj2->faults) return false;

	for (i = 0; i < z_info->fault_max; i++) {
		if (obj1->faults[i].power != obj2->faults[i].power) return false;
	}

	return true;
}

/**
 * Detect if a fault is in the conflict list of another fault
 */
static bool faults_conflict(int first, int second)
{
	struct fault *c = &faults[first];
	char buf[80] = "|";

	/* First fault has no conflicts */
	if (!c->conflict) {
		return false;
	}

	/* Build the conflict strong and search for it */
	my_strcat(buf, faults[second].name, sizeof(buf));
	my_strcat(buf, "|", sizeof(buf));
	if (strstr(c->conflict, buf)) {
		return true;
	}

	return false;
}

/**
 * Check an object for active faults, and remove the "faults" field if
 * none is found
 */
static void check_object_faults(struct object *obj)
{
	int i;

	/* Look for a valid fault, return if one found */
	for (i = 0; i < z_info->fault_max; i++) {
		if (obj->faults[i].power) {
			return;
		}
	}

	/* Free the fault structure */
	mem_free(obj->faults);
	obj->faults = NULL;
}


/**
 * Append a given fault with a given power to an object
 *
 * \param the object to fault
 * \param pick the fault to append
 * \param power the power of the new fault
 */
bool append_object_fault(struct object *obj, int pick, int power)
{
	struct fault *c = &faults[pick];
	int i;

	if (!obj->faults)
		obj->faults = mem_zalloc(z_info->fault_max * sizeof(struct fault_data));

	/* Reject conflicting faults */
	for (i = 0; i < z_info->fault_max; i++) {
		if (obj->faults[i].power && faults_conflict(i, pick)) {
			check_object_faults(obj);
			return false;
		}
	}

	/* Reject faults with effects foiled by an existing object property */
	if (c->obj->effect && c->obj->effect->index == effect_lookup("TIMED_INC")) {
		int idx = c->obj->effect->subtype;
		struct timed_effect_data *status;
		assert(idx < TMD_MAX);
		status = &timed_effects[idx];
		if (status->fail_code == TMD_FAIL_FLAG_OBJECT) {
			if (of_has(obj->flags, status->fail)) {
				check_object_faults(obj);
				return false;
			}
		} else if (status->fail_code == TMD_FAIL_FLAG_RESIST) {
			if (obj->el_info[status->fail].res_level > 0) {
				check_object_faults(obj);
				return false;
			}
		} else if (status->fail_code == TMD_FAIL_FLAG_VULN) {
			if (obj->el_info[status->fail].res_level < 0) {
				check_object_faults(obj);
				return false;
			}
		}
	}

	/* Reject faults which explicitly conflict with an object property */
	for (i = of_next(c->conflict_flags, FLAG_START); i != FLAG_END;
		 i = of_next(c->conflict_flags, i + 1)) {
		if (of_has(obj->flags, i)) {
			check_object_faults(obj);
			return false;
		}
	}

	/* Adjust power if our pick is a duplicate */
	if (power > obj->faults[pick].power) {
		obj->faults[pick].power = power;
		obj->faults[pick].timeout = randcalc(c->obj->time, 0, RANDOMISE);
		return true;
	}

	check_object_faults(obj);
	return false;
}

/**
 * Check an artifact template for active faults, remove conflicting faults, and
 * remove the "faults" field if no faults remain
 */
void check_artifact_faults(struct artifact *art)
{
	int i;

	/* Look for a valid fault, return if one found */
	for (i = 0; i < z_info->fault_max; i++) {
		if (art->faults && art->faults[i]) {
			return;
		}
	}

	/* Free the fault structure */
	mem_free(art->faults);
	art->faults = NULL;
}

/**
 *
 */
bool artifact_fault_conflicts(struct artifact *art, int pick)
{
	struct fault *c = &faults[pick];
	int i;

	/* Reject faults with effects foiled by an existing artifact property */
	if (c->obj->effect && c->obj->effect->index == effect_lookup("TIMED_INC")) {
		int idx = c->obj->effect->subtype;
		struct timed_effect_data *status;
		assert(idx < TMD_MAX);
		status = &timed_effects[idx];
		if (status->fail_code == TMD_FAIL_FLAG_OBJECT) {
			if (of_has(art->flags, status->fail)) {
				check_artifact_faults(art);
				return true;
			}
		} else if (status->fail_code == TMD_FAIL_FLAG_RESIST) {
			if (art->el_info[status->fail].res_level > 0) {
				check_artifact_faults(art);
				return true;
			}
		} else if (status->fail_code == TMD_FAIL_FLAG_VULN) {
			if (art->el_info[status->fail].res_level < 0) {
				check_artifact_faults(art);
				return true;
			}
		}
	}

	/* Reject faults which explicitly conflict with an artifact property */
	for (i = of_next(c->conflict_flags, FLAG_START); i != FLAG_END;
		 i = of_next(c->conflict_flags, i + 1)) {
		if (of_has(art->flags, i)) {
			check_artifact_faults(art);
			return true;
		}
	}

	return false;
}

/**
 * Append a given fault with a given power to an artifact
 *
 * \param the artifact to fault
 * \param pick the fault to append
 * \param power the power of the new fault
 */
bool append_artifact_fault(struct artifact *art, int pick, int power)
{
	int i;

	if (!art->faults)
		art->faults = mem_zalloc(z_info->fault_max * sizeof(int));

	/* Reject conflicting faults */
	for (i = 0; i < z_info->fault_max; i++) {
		if (art->faults[i] && faults_conflict(i, pick)) {
			check_artifact_faults(art);
			return false;
		}
	}

	/* Reject faults with effects foiled by an existing artifact property */
	if (artifact_fault_conflicts(art, pick)) {
		check_artifact_faults(art);
		return false;
	}

	/* Adjust power if our pick is a duplicate */
	if (power > art->faults[pick]) {
		art->faults[pick] = power;
	}

	check_artifact_faults(art);
	return true;
}

/**
 * Do a fault effect.
 *
 * \param i the index into the faults array
 */
bool do_fault_effect(int i, struct object *obj)
{
	struct fault *fault = &faults[i];
	struct effect *effect = fault->obj->effect;
	bool ident = false;
	bool was_aware = player_knows_fault(player, i);
	int dir = randint1(8);

	if (dir > 4) {
		dir++;
	}
	if (fault->obj->effect_msg) {
		msgt(MSG_GENERIC, fault->obj->effect_msg);
	}
	effect_do(effect, source_object(obj), NULL, &ident, was_aware, dir, 0, 0, NULL);
	fault->obj->known->effect = fault->obj->effect;
	disturb(player);
	return !was_aware && ident;
}
