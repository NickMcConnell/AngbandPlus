/**
 * \file player-quest.h
 * \brief Quest-related variables and functions
 *
 * Copyright (c) 2013 Angband developers
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

#ifndef QUEST_H
#define QUEST_H

/* Quest list */
extern struct quest *quests;
struct store_context;

/* Functions */
bool is_quest(int level);
void player_quests_reset(struct player *p);
void player_quests_free(struct player *p);
struct quest *get_quest_by_grid(struct loc grid);
bool quest_check(const struct monster *m);
extern struct file_parser quests_parser;
void quest_reward(struct quest *q, bool success);
struct quest *get_quest_by_name(const char *name);
bool quest_item_check(const struct object *obj);
void quest_enter_level(struct chunk *c);
void quest_changed_level(void);
void quest_changing_level(void);
bool quest_is_rewardable(const struct quest *q);
bool quest_special_endings(struct store_context *ctx);
bool quest_selling_object(struct object *obj, struct store_context *ctx);

#endif /* QUEST_H */
