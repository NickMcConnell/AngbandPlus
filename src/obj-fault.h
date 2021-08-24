/**
 * \file obj-fault.h
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
#ifndef INCLUDED_OBJ_FAULT_H
#define INCLUDED_OBJ_FAULT_H

#include "object.h"

extern struct fault *faults;

void init_fault_knowledge(void);
int lookup_fault(const char *name);
void copy_faults(struct object *obj, int *source);
bool faults_are_equal(const struct object *obj1, const struct object *obj2);
bool append_object_fault(struct object *obj, int pick, int power);
void check_artifact_faults(struct artifact *art);
bool artifact_fault_conflicts(struct artifact *art, int pick);
bool append_artifact_fault(struct artifact *art, int pick, int power);
bool do_fault_effect(int i, struct object *obj);

#endif /* !INCLUDED_OBJ_FAULT_H */
