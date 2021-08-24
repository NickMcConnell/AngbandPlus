/**
 * \file player-quest.c
 * \brief All quest-related code
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
#include "angband.h"
#include "datafile.h"
#include "generate.h"
#include "init.h"
#include "mon-make.h"
#include "mon-util.h"
#include "monster.h"
#include "obj-desc.h"
#include "obj-make.h"
#include "obj-pile.h"
#include "obj-util.h"
#include "player-birth.h"
#include "player-calcs.h"
#include "player-quest.h"
#include "store.h"
#include "trap.h"
#include "ui-knowledge.h"
#include "ui-store.h"
#include "world.h"

/**
 * Array of quests
 */
struct quest *quests;

/**
 * Parsing functions for quest.txt
 */
static enum parser_error parse_quest_name(struct parser *p) {
	const char *name = parser_getstr(p, "name");
	struct quest *h = parser_priv(p);

	struct quest *q = mem_zalloc(sizeof(*q));
	q->next = h;
	parser_setpriv(p, q);
	q->name = string_make(name);
	q->quests = 0;
	q->town = q->store = -1;
	q->entry_min = -1;
	q->entry_max = -1;
	q->entry_feature = FEAT_NONE;

	return PARSE_ERROR_NONE;
}

static enum parser_error parse_quest_level(struct parser *p) {
	struct quest *q = parser_priv(p);
	assert(q);

	q->level = parser_getuint(p, "level");
	return PARSE_ERROR_NONE;
}

static enum parser_error parse_quest_intro(struct parser *p) {
	struct quest *q = parser_priv(p);
	assert(q);

	q->intro = string_make(parser_getstr(p, "text"));
	return PARSE_ERROR_NONE;
}

static enum parser_error parse_quest_desc(struct parser *p) {
	struct quest *q = parser_priv(p);
	assert(q);

	q->desc = string_make(parser_getstr(p, "text"));
	return PARSE_ERROR_NONE;
}

static enum parser_error parse_quest_succeed(struct parser *p) {
	struct quest *q = parser_priv(p);
	assert(q);

	q->succeed = string_make(parser_getstr(p, "text"));
	return PARSE_ERROR_NONE;
}

static enum parser_error parse_quest_failure(struct parser *p) {
	struct quest *q = parser_priv(p);
	assert(q);

	q->failure = string_make(parser_getstr(p, "text"));
	return PARSE_ERROR_NONE;
}

static enum parser_error parse_quest_unlock(struct parser *p) {
	struct quest *q = parser_priv(p);
	assert(q);

	q->unlock = string_make(parser_getstr(p, "text"));
	return PARSE_ERROR_NONE;
}

static enum parser_error parse_quest_object(struct parser *p) {
	struct quest *q = parser_priv(p);
	assert(q);

	q->target_item = string_make(parser_getstr(p, "object"));
	return PARSE_ERROR_NONE;
}

static enum parser_error parse_quest_entrymin(struct parser *p) {
	struct quest *q = parser_priv(p);
	assert(q);

	q->entry_min = parser_getuint(p, "min");
	return PARSE_ERROR_NONE;
}

static enum parser_error parse_quest_entrymax(struct parser *p) {
	struct quest *q = parser_priv(p);
	assert(q);

	q->entry_max = parser_getuint(p, "max");
	return PARSE_ERROR_NONE;
}

static enum parser_error parse_quest_min_found(struct parser *p) {
	struct quest *q = parser_priv(p);
	assert(q);

	q->min_found = parser_getuint(p, "min");
	return PARSE_ERROR_NONE;
}

static enum parser_error parse_quest_max_remaining(struct parser *p) {
	struct quest *q = parser_priv(p);
	assert(q);

	q->max_remaining = parser_getuint(p, "max");
	return PARSE_ERROR_NONE;
}

static enum parser_error parse_quest_entryfeature(struct parser *p) {
	struct quest *q = parser_priv(p);
	assert(q);

	q->entry_feature = lookup_feat(parser_getsym(p, "feature"));

	return PARSE_ERROR_NONE;
}

static enum parser_error parse_quest_flags(struct parser *p) {
	struct quest *q = parser_priv(p);
	assert(q);

	const char *in = parser_getstr(p, "flags");
	if (strstr(in, "active"))
		q->flags |= QF_ACTIVE;
	if (strstr(in, "essential"))
		q->flags |= QF_ESSENTIAL;
	if (strstr(in, "locked"))
		q->flags |= QF_LOCKED;
	return PARSE_ERROR_NONE;
}

static enum parser_error parse_quest_race(struct parser *p) {
	struct quest *q = parser_priv(p);
	const char *name = parser_getstr(p, "race");
	assert(q);

	q->race = lookup_monster(name);
	if (!q->race)
		return PARSE_ERROR_INVALID_MONSTER;

	return PARSE_ERROR_NONE;
}

static enum parser_error parse_quest_store(struct parser *p) {
	struct quest *q = parser_priv(p);
	assert(q);

	/* Read a town index, as a town name or the name of another quest */
	const char *location = parser_getsym(p, "town");

	if (!location)
		return PARSE_ERROR_INVALID_VALUE;

	const char *name = NULL;
	if (parser_hasval(p, "store"))
		name = parser_getsym(p, "store");

	q->loc = mem_realloc(q->loc, sizeof(struct quest_location) * (1 + q->quests));
	struct quest_location *ql = q->loc + q->quests;
	q->quests++;
	ql->town = ql->store = -1;
	ql->location = string_make(location);
	ql->storename = string_make(name);

	return PARSE_ERROR_NONE;
}

static enum parser_error parse_quest_number(struct parser *p) {
	struct quest *q = parser_priv(p);
	assert(q);

	q->max_num = parser_getuint(p, "number");
	return PARSE_ERROR_NONE;
}

struct parser *init_parse_quest(void) {
	struct parser *p = parser_new();
	parser_setpriv(p, NULL);
	parser_reg(p, "name str name", parse_quest_name);
	parser_reg(p, "level uint level", parse_quest_level);
	parser_reg(p, "race str race", parse_quest_race);
	parser_reg(p, "number uint number", parse_quest_number);
	parser_reg(p, "store sym town ?sym store", parse_quest_store);
	parser_reg(p, "object str object", parse_quest_object);
	parser_reg(p, "entrymin uint min", parse_quest_entrymin);
	parser_reg(p, "entrymax uint max", parse_quest_entrymax);
	parser_reg(p, "entryfeature sym feature", parse_quest_entryfeature);
	parser_reg(p, "intro str text", parse_quest_intro);
	parser_reg(p, "desc str text", parse_quest_desc);
	parser_reg(p, "succeed str text", parse_quest_succeed);
	parser_reg(p, "failure str text", parse_quest_failure);
	parser_reg(p, "unlock str text", parse_quest_unlock);
	parser_reg(p, "min-found uint min", parse_quest_min_found);
	parser_reg(p, "max-remaining uint max", parse_quest_max_remaining);
	parser_reg(p, "flags str flags", parse_quest_flags);
	return p;
}

static errr run_parse_quest(struct parser *p) {
	return parse_file_quit_not_found(p, "quest");
}

static errr finish_parse_quest(struct parser *p) {
	struct quest *quest, *next = NULL;
	int count;

	/* Count the entries */
	z_info->quest_max = 0;
	quest = parser_priv(p);
	while (quest) {
		z_info->quest_max++;
		quest = quest->next;
	}

	/* Allocate the direct access list and copy the data to it */
	quests = mem_zalloc(z_info->quest_max * sizeof(*quest));
	count = z_info->quest_max - 1;
	for (quest = parser_priv(p); quest; quest = next, count--) {
		memcpy(&quests[count], quest, sizeof(*quest));
		quests[count].index = count;
		next = quest->next;
		if (count < z_info->quest_max - 1)
			quests[count].next = &quests[count + 1];
		else
			quests[count].next = NULL;

		mem_free(quest);
	}

	parser_destroy(p);
	return 0;
}

static void cleanup_quest(void)
{
	int idx;
	for (idx = 0; idx < z_info->quest_max; idx++)
		string_free(quests[idx].name);
	mem_free(quests);
}

struct file_parser quests_parser = {
	"quest",
	init_parse_quest,
	run_parse_quest,
	finish_parse_quest,
	cleanup_quest
};


/** Complete the current quest, successfully */
static void succeed_quest(struct quest *q) {
	if (!(q->flags & QF_SUCCEEDED))
		msgt(MSG_LEVEL, "Your task is complete!");
	q->flags |= QF_SUCCEEDED;
	q->flags |= QF_UNREWARDED;
	q->flags &= ~QF_FAILED;
}

/** Complete the current quest, unsuccessfully */
static void fail_quest(struct quest *q) {
	if (!(q->flags & QF_FAILED))
		msgt(MSG_LEVEL, "You have failed in your task!");
	q->flags |= QF_FAILED;
	q->flags &= ~QF_SUCCEEDED;
	q->flags |= QF_UNREWARDED;
}

/** Complete the current quest, successfully */
static void quest_succeed(void) {
	assert(player->active_quest >= 0);
	struct quest *q = &player->quests[player->active_quest];
	succeed_quest(q);
}

/** Complete the current quest, unsuccessfully */
static void quest_fail(void) {
	assert(player->active_quest >= 0);
	struct quest *q = &player->quests[player->active_quest];
	fail_quest(q);
}

/**
 * Check if the given level is a quest level.
 */
bool is_quest(int level)
{
	size_t i;

	/* Town is never a quest */
	if (!level) return false;

	for (i = 0; i < z_info->quest_max; i++)
		if (player->quests[i].level == level)
			return true;

	return false;
}

/**
 * Copy all the standard quests to the player quest history
 */
void player_quests_reset(struct player *p)
{
	size_t i;

	if (p->quests)
		player_quests_free(p);
	p->quests = mem_zalloc(z_info->quest_max * sizeof(struct quest));

	for (i = 0; i < z_info->quest_max; i++) {
		p->quests[i].name = string_make(quests[i].name);
		p->quests[i].succeed = string_make(quests[i].succeed);
		p->quests[i].failure = string_make(quests[i].failure);
		p->quests[i].intro = string_make(quests[i].intro);
		p->quests[i].desc = string_make(quests[i].desc);
		p->quests[i].target_item = string_make(quests[i].target_item);
		p->quests[i].level = quests[i].level;
		p->quests[i].race = quests[i].race;
		p->quests[i].max_num = quests[i].max_num;
		p->quests[i].flags = quests[i].flags;
		p->quests[i].quests = quests[i].quests;
		p->quests[i].town = quests[i].town;
		p->quests[i].store = quests[i].store;
		p->quests[i].loc = mem_alloc(quests[i].quests * sizeof(struct quest_location));
		memcpy(p->quests[i].loc,  quests[i].loc,  quests[i].quests * sizeof(struct quest_location));
		p->quests[i].unlock = quests[i].unlock;
		p->quests[i].entry_min = quests[i].entry_min;
		p->quests[i].entry_max= quests[i].entry_max;
		p->quests[i].min_found = quests[i].min_found;
		p->quests[i].max_remaining = quests[i].max_remaining;
		p->quests[i].entry_feature = quests[i].entry_feature;
	}
}

/**
 * Free the player quests
 */
void player_quests_free(struct player *p)
{
	size_t i;

	for (i = 0; i < z_info->quest_max; i++) {
		string_free(p->quests[i].name);
		string_free(p->quests[i].succeed);
		string_free(p->quests[i].failure);
		string_free(p->quests[i].intro);
		string_free(p->quests[i].desc);
		string_free(p->quests[i].target_item);
		mem_free(p->quests[i].loc);
	}
	mem_free(p->quests);
}

/**
 * Creates magical stairs after finishing a quest monster.
 */
static void build_quest_stairs(struct loc grid)
{
	struct loc new_grid = player->grid;

	/* Stagger around */
	while (!square_changeable(cave, grid) &&
		   !square_iswall(cave, grid) &&
		   !square_isdoor(cave, grid)) {
		/* Pick a location */
		scatter(cave, &new_grid, grid, 1, false);

		/* Stagger */
		grid = new_grid;
	}

	/* Push any objects */
	push_object(grid);

	/* Explain the staircase */
	msg("A staircase down is revealed...");

	/* Create stairs down */
	square_set_feat(cave, grid, FEAT_MORE);

	/* Update the visuals */
	player->upkeep->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
}

/**
 * Return the quest entered at a given grid in the Town, or NULL if there is none.
 * The quest must be active.
 */
struct quest *get_quest_by_grid(struct loc grid)
{
	for (int i = 0; i < z_info->quest_max; i++) {
		struct quest *q = &player->quests[i];
		if (q->flags & QF_ACTIVE) {
			if ((q->x == grid.x) && (q->y == grid.y)) {
				return q;
			}
		}
	}
	return NULL;
}

/**
 * Return the quest with a given name.
 */
struct quest *get_quest_by_name(const char *name)
{
	if (!name)
		return NULL;
	for (int i = 0; i < z_info->quest_max; i++) {
		struct quest *q = &player->quests[i];
		if (streq(q->name, name))
			return q;
	}
	return NULL;
}

/* Add a start item to a list */
static void add_item(struct start_item *si, int tval, const char *name, int min, int max)
{
	struct start_item *prev = NULL;
	while (si->max != 0) {
		prev = si;
		si++;
	}
	si->tval = tval;
	si->sval = lookup_sval(tval, name);
	si->min = min;
	si->max = max;
	si->next = NULL;
	if (prev)
		prev->next = si;
}

/** Drop an item on the floor at the specified place, mark it as a quest item */
static void quest_item_at(struct chunk *c, struct loc xy, struct object *obj)
{
	bool dummy;
	obj->origin = ORIGIN_SPECIAL;
	obj->origin_depth = player->depth;
	of_on(obj->flags, OF_QUEST_SPECIAL);
	floor_carry(c, xy, obj, &dummy);
}

/** Enter a quest level. This is called after the vault is generated.
 * At this point cave may not be set - so use the passed in chunk.
 **/
void quest_enter_level(struct chunk *c)
{
	assert(player->active_quest >= 0);
	struct quest *q = &player->quests[player->active_quest];
	const char *n = q->name;
	s32b value;

	 if (streq(n, "Msing Pills")) {
		/* Traps:
		 * Place a portal at each side first.
		 */
		struct trap_kind *glyph = lookup_trap("portal");
		if (glyph) {
			int tidx = glyph->tidx;
			place_trap(c, loc(1, 9), tidx, 0);
			place_trap(c, loc(c->width-2, 9), tidx, 0);
		}

		/* Items:
		 * Place one piece of fruit next to where you start, and
		 * one of each other type in random clear positions.
		 */

		char *fruit[] = {
			 "apple", "pear", "orange", "satsuma", "banana",
			 "pineapple", "melon", "pepper", "habanero", "choke-apple",
			 "snozzcumber"
		};
		const int nfruit = sizeof(fruit)/sizeof(fruit[0]);

		shuffle_sized(fruit, nfruit, sizeof(fruit[0]));
		for(int i=0;i<nfruit;i++) {
			struct loc xy = loc(13, 14);	// left of the <
			if (i > 0) {
				/* Find a space (avoiding the unreachable center) */
				do {
					xy = loc(randint1(c->width - 1), randint1(c->height - 1));
				} while ((xy.y == 9) || (!square_isempty(c, xy)));
			}
			struct object *obj = make_object_named(c, 1, false, false, false, &value, TV_FOOD, fruit[i]);
			bool dummy;
			obj->origin = ORIGIN_SPECIAL;
			obj->origin_depth = player->depth;
			obj->number = 1;
			floor_carry(c, xy, obj, &dummy);
		}

		/*
		 * Fill every other clear position (outside the central area)
		 * with random non-useless but typically low-level pills - total ~200.
		 */

		for(int x=1;x<c->width;x++) {
			for(int y=1;y<c->height;y++) {
				struct loc xy = loc(x, y);
				if ((y != 9) || (x == 5) || (x == 22)) {
					if (square_isempty(c, xy)) {
						struct object *obj = NULL;
						do {
							if (obj)
								object_delete(&obj);
							obj = make_object_named(c, 1, false, false, false, &value, TV_PILL, NULL);
						} while ((!obj) || (value <= 5) || (obj->number > 1));	/* not a nasty or sugar */
						quest_item_at(c, xy, obj);
					}
				}
			}
		}

		/* Monsters:
		 * 4 uniques.
		 * Resurrect centrally when killed? This seems too mean, though.
		 */

		struct monster_group_info info = { 0, 0 };
		place_new_monster(c, loc(1, 1), lookup_monster("Inky"), false, false, info, ORIGIN_DROP);
		place_new_monster(c, loc(c->width-2, 1), lookup_monster("Blinky"), false, false, info, ORIGIN_DROP);
		place_new_monster(c, loc(1, c->height - 2), lookup_monster("Pinky"), false, false, info, ORIGIN_DROP);
		place_new_monster(c, loc(c->width-2, c->height - 2), lookup_monster("Clyde"), false, false, info, ORIGIN_DROP);
	 }
}

static struct object * has_special_flag(struct object *obj, void *data)
{
	return (of_has(obj->flags, OF_QUEST_SPECIAL)) ? obj : NULL;
}

static struct object * kind_has_special_flag(struct object *obj, void *data)
{
	if (of_has(obj->flags, OF_QUEST_SPECIAL)) {
		struct object_kind *k = (struct object_kind *)data;
		if (obj->kind == k)
			return obj;
	}
	return NULL;
}

static struct object * obj_of_kind(struct object *obj, void *data)
{
	struct object_kind *k = (struct object_kind *)data;
	if (obj->kind == k)
		return obj;
	return NULL;
}

/**
 * Remove the QUEST_SPECIAL flag on all items - they are now ordinary items which can e.g. be sold.
 * Must at least check your home, your gear and the level.
 * This usually happens when you fail a quest - but some quests might allow you to keep the stuff when you succeed as well.
 */
static void quest_remove_flags(void)
{
	struct object *obj;
	do {
		obj = find_object(has_special_flag, NULL);
		if (obj)
			of_off(obj->flags, OF_QUEST_SPECIAL);
	} while (obj);
}

/** Delete all items with the QUEST_SPECIAL flag.
 * Must at least check your home, your gear and the level.
 * This usually happens when you succeed.
 */
static void quest_remove_specials(void)
{
	struct object *obj;
	do {
		obj = find_object(has_special_flag, NULL);
		if (obj)
			remove_object(obj);
	} while (obj);
}

/** Count all items of a given kind
 */
static int quest_count_kind(struct object_kind *k)
{
	struct object *obj;
	int count = 0;
	do {
		obj = find_object(obj_of_kind, k);
		if (obj)
			count++;
	} while (obj);
	return count;
}

/** Returns a (static) table of the count of all items of a given kind
 */
static int *quest_locate_kind(struct object_kind *k)
{
	struct object *obj;
	static int table[MAX_LOCATION];
	struct location location;
	memset(table, 0, sizeof(table));
	do {
		obj = locate_object(obj_of_kind, k, &location);
		if (obj)
			table[location.type]++;
	} while (obj);
	return table;
}

/** Delete all items of a given kind
 */
static void quest_remove_kind(struct object_kind *k)
{
	struct object *obj;
	do {
		obj = find_object(obj_of_kind, k);
		if (obj)
			remove_object(obj);
	} while (obj);
}

/** Special cases for quests when you change levels
 * Called before level gen occurs
 */
void quest_changing_level(void)
{
	struct player *p = player;

	/* Handle returning to the town from a quest level */
	if ((p->active_quest >= 0) && (!player->depth)) {
		struct quest *quest = &player->quests[player->active_quest];

		/* Fail, or reward */
		if (!(quest->flags & QF_SUCCEEDED)) {
			quest->flags |= QF_FAILED;
		} else {
			quest->flags |= QF_UNREWARDED;
		}

		/* No longer active */
		quest->flags &= ~QF_ACTIVE;

		/* Not generating or in a quest any more */
		p->active_quest = -1;
	}
}

/** Special cases for quests when you change levels
 * Called after level gen is complete (and the old level has been discarded)
 * This is also called after reloading a level.
 */
void quest_changed_level(void)
{
	/* Quest specific checks */
	for(int i=0;i<z_info->quest_max;i++) {
		struct quest *q = player->quests + i;
		if (q->flags & QF_ACTIVE) {
			if (strstr(q->name, "Pie")) {
				if (q->flags & QF_SUCCEEDED) {
					/* Pie quest: if the card ever disappears unrecoverably, then you have failed */
					struct object_kind *kind = lookup_kind(TV_CARD, lookup_sval(TV_CARD, "security"));
					int count = quest_count_kind(kind);
					if (!count) {
						fail_quest(q);
					}
				}
			}
		}
	}
}

/** Check for special quest behaviour when selling an object to the store.
 * Returns true if handled, false if the normal selling dialog should continue.
 */
bool quest_selling_object(struct object *obj, struct store_context *ctx)
{
	struct store *store = ctx->store;

	/* Selling the card to the BM triggers an alternate completion */
	struct object_kind *security = lookup_kind(TV_CARD, lookup_sval(TV_CARD, "security"));
	if (obj->kind == security) {
		if (store->sidx == STORE_B_MARKET) {
			store_long_text(ctx, "So you found her dead? Unfortunate, but at least we got the card back "
									"and that is what really matters. You'll get the same $8000 reward she "
									"would have had, and get to see some of the goods that most customers "
									"don't. And we may have more for you to do, if you really want to take "
									"her place.");
			fail_quest(get_quest_by_name("Soldier, Sailor, Chef, Pie"));
			player->au += 8000;
			player->bm_faction++;
			for (int j = 0; j < 10; j++)
				store_maint(store);
			remove_object(obj);
			return true;
		}
	}

	return false;
}

/** Asked for a quest at a store, but none of the usual cases match.
 * This handles the special cases (such as completing with an alternative ending at a different store, as in "Pie").
 * It returns true if it has handled it, false for the generic no-quest message.
 */
bool quest_special_endings(struct store_context *ctx)
{
	struct store *store = ctx->store;
	/* Pie quest: if you take the card to the BM */
	if (store->sidx == STORE_B_MARKET) {
		struct object_kind *kind = lookup_kind(TV_CARD, lookup_sval(TV_CARD, "security"));
		struct object *obj = find_object(obj_of_kind, kind);
		if (obj) {
			while (find_object(obj_of_kind, kind)) {};
			int *locs = quest_locate_kind(kind);
			if (locs[LOCATION_PLAYER] > 0) {
				screen_save();
				int response = store_get_long_check(ctx, "So you have a certain card with you that we were expecting our Ky "
															"to bring back. If you could tell me how you ended up with it and "
															"return it to us, you'll be paid appropriately...");
				screen_load();
				if (response) {
					// accepted
					quest_selling_object(obj, ctx);
					return true;
				}
			}
		}
	}
	return false;
}

/** Returns true if the quest can have the reward given.
 * This implies QF_SUCCESS | QF_UNREWARDED, and for many quests that is all that is needed.
 * However, there are cases where additional checks are needed - for example, that you have
 * not just obtained the McGuffin but have it with you now.
 */
bool quest_is_rewardable(const struct quest *q)
{
	/* Must be successful and unrewarded */
	if ((q->flags & (QF_SUCCEEDED | QF_UNREWARDED)) != (QF_SUCCEEDED | QF_UNREWARDED))
		return false;

	/* Special cases */

	/* You must have the card with you */
	if (strstr(q->name, "Pie")) {
		struct object_kind *security = lookup_kind(TV_CARD, lookup_sval(TV_CARD, "security"));
		int *locs = quest_locate_kind(security);
		if (locs[LOCATION_PLAYER] != 1) {
			msg("Great news that you found it, but you'll to need to fetch it back.");
			return false;
		} else {
			quest_remove_kind(security);
		}
	}

	/* Default to OK */
	return true;
}

/**
 * Generate a reward for completing a quest.
 * Passed true if the quest was completed successfully.
 * Make sure overflowing inventory is handled reasonably.
 */
void quest_reward(const struct quest *q, bool success)
{
	const char *n = q->name;
	int au = 0;
	struct start_item si[4];
	memset(si, 0, sizeof(si));

	if (success) {
		if (streq(n, "Rats")) {
			add_item(si, TV_FOOD, "cheese", 6, 9);
			au = 200;
		} else if (streq(n, "Msing Pills")) {
			add_item(si, TV_PILL, "augmentation", 2, 2);
		} else if (strstr(n, "Pie")) {
			add_item(si, TV_FOOD, "Hunter's pie", 12, 12);
			add_item(si, TV_MUSHROOM, "clarity", 5, 5);
			add_item(si, TV_MUSHROOM, "emergency", 7, 7);
		}
		quest_remove_specials();
	} else {
		quest_remove_flags();
	}

	if (si[0].max) {
		add_start_items(player, si, false, false, ORIGIN_REWARD);
	}
	player->au += au;
}

/** Return true if the item is a target of the quest */
static bool item_is_target(const struct quest *q, const struct object *obj) {
	char oname[64];
	object_desc(oname, sizeof(oname), obj, ODESC_SPOIL | ODESC_BASE);
	if ((!q) || (!obj))
		return false;
	if (!q->target_item)
		return false;
	if (!of_has(obj->flags, OF_QUEST_SPECIAL))
		return false;
	return (my_stristr(oname, q->target_item) != NULL);
}

/**
 * You picked up an item.
 * Check if that completes your quest.
 */
bool quest_item_check(const struct object *obj) {
	/* If you aren't in a quest, or it doesn't have a quest item,
	 * or the item picked up is not a quest item, bail out early.
	 **/
	if (player->active_quest < 0)
		return false;
	struct quest *q = &player->quests[player->active_quest];
	if (!item_is_target(q, obj))
		return false;

	/* Find the number of targets carried */
	int gear_items = 0;
	struct object *gear_obj = player->gear;
	while (gear_obj) {
		if (item_is_target(q, gear_obj))
			gear_items += gear_obj->number;
		gear_obj = gear_obj->next;
	};

	/* And the number on the level (including monster held) */
	int level_items = 0;
	for (int y = 1; y < cave->height; y++) {
		for (int x = 1; x < cave->width; x++) {
			struct loc grid = loc(x, y);
			for (struct object *obj = square_object(cave, grid); obj; obj = obj->next) {
				if (item_is_target(q, obj))
					level_items += obj->number;
			}
		}
	}

	/* Does this meet the quest conditions?
	 * Possible conditions include
	 * 'at least X' (where X is commonly 1)
	 * 'all still remaining' (i.e. destroyed items are OK)
	 * 'all that were there at the start' (so not OK)
	 *
	 * There is also 'quest flag' vs item name XXX
	 *
	 * What if you un-complete a quest? (by destroying items, or leaving them behind?)
	 * A quest might complete immediately, or only when leaving the quest level, or only when
	 * visiting the questgiver - add flags.
	 *
	 * Might also be possible to *fail* a quest - by destroying the wrong object, or killing the wrong monster...
	 *
	 * Combinations of these ("all still remaining, but at least 2")
	 *
	 * A "minimum found" and "maximum remaining" is useful, though.
	 **/
	if (gear_items >= q->min_found) {
		if (level_items <= q->max_remaining) {
			quest_succeed();
			return true;
		}
	}

	return false;
}

/**
 * Check if this (now dead) monster is a quest monster, and act appropriately
 */
bool quest_check(const struct monster *m) {
	int i, total = 0;

	/* First check for a quest level */
	if (player->active_quest >= 0) {
		struct quest *q = &player->quests[player->active_quest];
		if (m->race == q->race) {
			if (q->cur_num < q->max_num) {
				/* You've killed a quest target */
				q->cur_num++;
				if (q->cur_num == q->max_num) {
					/* You've killed the last quest target */
					quest_succeed();
					return true;
				}
			}
		}
	}

	/* Then check for monsters found outside special levels that affect quests.
	 * These don't necessarily have the RF_QUESTOR flag (consider a 'random nonunique' quest)
	 **/
	bool questor = rf_has(m->race->flags, RF_QUESTOR);
	if (questor) {
		if (streq(m->race->name, "Ky, the Pie Spy")) {
			succeed_quest(get_quest_by_name("Soldier, Sailor, Chef, Pie"));
			return true;
		}
	}

	/* Now dealing only with the win quests - so don't bother with non-questors */
	if (!questor) return false;

	/* Mark quests as complete */
	for (i = 0; i < z_info->quest_max; i++) {
		if (player->quests[i].flags & QF_ESSENTIAL) {
			/* Note completed quests */
			if (player->quests[i].level == m->race->level) {
				player->quests[i].level = 0;
				player->quests[i].cur_num++;
			}

			/* Count incomplete quests */
			if (player->quests[i].level) total++;
		}
	}

	/* Build magical stairs */
	build_quest_stairs(m->grid);

	/* Nothing left, game over... */
	if (total == 0) {
		player->total_winner = true;
		player->upkeep->redraw |= (PR_TITLE);
		msg("*** CONGRATULATIONS ***");
		msg("You have won the game!");
		msg("You may retire (commit suicide) when you are ready.");
	}

	return true;
}
