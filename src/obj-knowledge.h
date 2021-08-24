/**
 * \file obj-knowledge.h
 * \brief Object knowledge
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
#include "object.h"
#include "player.h"

enum icon_variety {
	ICON_VAR_COMBAT,
	ICON_VAR_MOD,
	ICON_VAR_RESIST,
	ICON_VAR_BRAND,
	ICON_VAR_SLAY,
	ICON_VAR_FAULT,
	ICON_VAR_FLAG
};


enum combat_icons {
	COMBAT_ICON_TO_A = 0,
	COMBAT_ICON_TO_H,
	COMBAT_ICON_TO_D,
	COMBAT_ICON_MAX
};

struct icon {
	enum icon_variety variety;
	int index;
	quark_t note;
	const char *name;
};

int max_icons(void);
enum icon_variety icon_variety(size_t i);
bool player_knows_icon(struct player *p, size_t i);
char *icon_name(size_t i);
char *icon_desc(size_t i);
quark_t icon_note(size_t i);
void icon_set_note(size_t i, const char *inscription);

void player_learn_icon(struct player *p, size_t i, bool message);
bool player_knows_brand(struct player *p, int i);
bool player_knows_slay(struct player *p, int i);
bool player_knows_fault(struct player *p, int i);
bool player_knows_ego(struct player *p, struct ego_item *ego,
	const struct object *obj);
bool object_effect_is_known(const struct object *obj);
bool object_is_known_artifact(const struct object *obj);
bool object_is_in_store(const struct object *obj);
bool object_has_standard_to_h(const struct object *obj);
bool object_has_icon(const struct object *obj, int icon_no);
bool object_icons_known(const struct object *obj);
bool object_fully_known(const struct object *obj);
bool object_flag_is_known(const struct player *p, const struct object *obj,
	int flag);
bool object_element_is_known(const struct player *p, const struct object *obj,
	int element);

void object_set_base_known(struct object *obj);
void object_sense(struct player *p, struct object *obj);
void object_see(struct player *p, struct object *obj);
void object_touch(struct player *p, struct object *obj);
void object_grab(struct player *p, struct object *obj);
void player_know_object(struct player *p, struct object *obj);
void update_player_object_knowledge(struct player *p);

void player_learn_flag(struct player *p, int flag);
void player_learn_fault(struct player *p, struct fault *fault);
void player_learn_innate(struct player *p);
void player_learn_all_icons(struct player *p);

void equip_learn_on_defend(struct player *p);
void equip_learn_on_ranged_attack(struct player *p);
void equip_learn_on_melee_attack(struct player *p);
void equip_learn_flag(struct player *p, int flag);
void equip_learn_element(struct player *p, int element);
void equip_learn_after_time(struct player *p);

int object_find_unknown_icon(struct player *p, struct object *obj);
void object_learn_unknown_icon(struct player *p, struct object *obj);
void object_learn_on_wield(struct player *p, struct object *obj);
void shape_learn_on_assume(struct player *p, const char *name);
void object_learn_on_use(struct player *p, struct object *obj);
void object_learn_brand(struct player *p, struct object *obj, int index);
void object_learn_slay(struct player *p, struct object *obj, int index);
void missile_learn_on_ranged_attack(struct player *p, struct object *obj);

bool easy_know(const struct object *obj);
bool object_flavor_is_aware(const struct object *obj);
bool object_flavor_was_tried(const struct object *obj);
void object_flavor_aware(struct object *obj);
void object_flavor_tried(struct object *obj);
