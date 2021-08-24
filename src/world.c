/**
 * \file world.c
 * \brief World (surface, towns, wilderness): initialization, generation, etc.
 *
 * Copyright (c) 1997 Ben Harrison
 * Copyright (c) 2021 Mike Searle
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
 *
 * This file is used to initialize and run the world: the overall layout of
 *		towns, dungeons and any surface wilderness between them.
 *
 * The world array is built from the "world.txt" data file in the
 * "lib/gamedata" directory.
 */

#include <math.h>

#include "game-world.h"
#include "generate.h"
#include "init.h"
#include "message.h"
#include "player.h"
#include "player-quest.h"
#include "player-util.h"
#include "ui-store.h"
#include "world.h"
#include "z-rand.h"
#include "z-util.h"
#include "z-virt.h"

struct town *t_info;

/* Array of distances between pairs of towns */
static int *world_distance;

/* Seeds the RNG to produce town distance arrays */
u32b world_town_seed;

/* Arrays of names from town-names.txt */
static char **town_full_name;
static int town_full_names;
static char **town_front_name;
static int town_front_names;
static char **town_back_name;
static int town_back_names;

/* List of dungeons */
static struct dungeon *dungeons;

/* Initial world */
static struct level *world_init;

static void cleanup_levels(struct level **world);
static struct level *dup_levels(struct level *src);

/** Clean up towns and related world objects
 */
void world_cleanup_towns(void)
{
	for(int i=0;i<z_info->town_max;i++) {
		struct town *t = t_info + i;
		mem_free(t->name);
		mem_free(t->connect);
		mem_free(t->stores);
		string_free(t->underground);
		string_free(t->geography);
		t->connect = NULL;
	}
	stores = NULL;
	mem_free(t_info);
	t_info = NULL;
	z_info->town_max = 0;
}

/**
 * Change the player's current town
 */
void world_change_town(struct town *t)
{
	player->upkeep->last_level = player->town ? player->town->name : NULL;
	player->town = t;
	stores = player->town->stores;
}

/**
 * Add a connection in both directions between two towns
 * By this point all towns must have been created
 */
void world_connect_towns(struct town *a, struct town *b)
{
	a->connections++;
	a->connect = mem_realloc(a->connect, sizeof(*(a->connect)) * a->connections);
	a->connect[a->connections - 1] = b;
	b->connections++;
	b->connect = mem_realloc(b->connect, sizeof(*(b->connect)) * b->connections);
	b->connect[b->connections - 1] = a;
}

/** Return the time of day that the daily flight leaves (as a turn-of-day)
 */
int world_departure_time(struct town *from, struct town *to)
{
	// In a flight to Soviet Russia, you seed the timer from the RNG
	u32b fi = from - t_info;
	u32b ti = to - t_info;
	u32b shook = (ti + (fi * z_info->town_max));
	shook = LCRNG(shook);
	shook %= ((60 * 24) / z_info->town_max);

	// Fill in the rest to make sure there are no same minute departures
	shook *= z_info->town_max;
	shook += ti;

	// Convert from minutes to turns
	double out = shook;
	out *= (10L * z_info->day_length);
	out /= (24 * 60);
	return out;
}


/** Return the distance between two towns (m)
 */
int world_between(struct town *from, struct town *to)
{
	return world_distance[(from - t_info)+((to-t_info) * z_info->town_max)];
}

/** Return the price to fly between two towns
 */
int world_airline_fare(struct town *from, struct town *to)
{
	int d = world_between(from, to);
	int fare = 500 + (d / 200) + ((d / 15000) * (d / 15000));

	/* It gets increasingly expensive to fly as the danger level increases */
	int markup = danger_depth(player);
	markup *= markup;
	markup /= 10;
	markup += 100;
	fare *= markup;
	fare /= 100;

	return store_roundup(fare);
}

/** Return the time taken to fly between two towns (in turns)
 */
int world_flight_time(struct town *from, struct town *to)
{
	int d = world_between(from, to);
	int minutes = 15 + (d / 5000); // 300km/h, cruising - plus a bit.
	double turns = minutes;
	turns *= (10L * z_info->day_length);
	turns /= (24 * 60);
	return turns;
}

/**
 * Return the number of flight connections from a town
 **/
int world_connections(struct town *t)
{
	return t->connections;
}

/** Return any town matching the name, or NULL if none do.
 */
struct town *get_town_by_name(const char *name)
{
	if (!name)
		return NULL;
	for(int i=0; i<z_info->town_max; i++) {
		if (t_info[i].name && streq(t_info[i].name, name)) {
			return t_info + i;
		}
	}
	return NULL;
}

/** Return any town matching the name, or NULL if none do.
 */
static struct town *get_town_by_dungeon(const char *name)
{
	if (!name)
		return NULL;
	for(int i=0; i<z_info->town_max; i++) {
		if (t_info[i].downto && strstr(t_info[i].downto, name)) {
			return t_info + i;
		}
	}
	return NULL;
}

/** Select a random string from a string array */
static char *world_random_text(char **array, int length)
{
	return array[randint0(length)];
}

/** Generate a random name in static buffer. No more than 12 chars, to fit on the left panel.
 */
static char *world_random_name(void)
{
	static char buf[64];
	char *front;
	char *mid;
	char *back;

	do {
		/* 1 in 5 are single names. The rest are combined from two halves. */
		if (one_in_(5)) {
			front = world_random_text(town_full_name, town_full_names);
			mid = "";
			back = "";
		} else {
			front = world_random_text(town_front_name, town_front_names);
			mid = " ";
			back = world_random_text(town_back_name, town_back_names);
		}
		strnfmt(buf, sizeof(buf), "%s%s%s", front, mid, back);
	} while (strlen(buf) > 12);

	return buf;
}

/** Generate a unique random name
 * Returned as string_make'd string
 */
static char *world_make_name(void)
{
	char *name;
	struct town *t;
	do
	{
		name = world_random_name();
		t = get_town_by_name(name);
	} while ((!name ) || (t));
	return string_make(name);
}

/** Set a town's name (passed name must be string_make'd) */
static void world_name_town(struct town *t, char *name)
{
	t->name = name;
}

/** Describe a town.
 * If this town has been visited, this should give some information (what shops are there).
 * If not, just gush! (Mention surroundings etc, maybe nearest town)
 * The result is returned in a static buffer.
 ***/
char *world_describe_town(struct town *t)
{
	static char buf[256];

	#define GUFF(N) guff_##N[randint0(sizeof(guff_##N) / sizeof(const char *))]

	static const char *guff_really[] = {
		"intensely",
		"startlingly",
		"especially",
		"unusually",
		"uniquely",
		"spectacularly",
		"amazingly",
		"stunningly",
		"terrifically",
		"sparklingly",
		"supremely",
	};
	static const char *guff_pretty[] = {
		"pretty",
		"handsome",
		"attractive",
		"striking",
		"picturesque",
		"eye-catching",
	};
	static const char *guff_town[] = {
		" town",
		" little town",
		" village",
		" old town",
		" hamlet",
		", bustling town",
		", quiet town",
		", unspoiled town",
	};
	static const char *guff_sits[] = {
		"nestles",
		"sits",
		"lies",
		"is sited",
		"is situated",
		"is placed",
		"is centered",
		"completes",
	};
	static const char *guff_dramatic[] = {
		"dramatic",
		"wonderful",
		"superb",
		"extraordinary",
		"fairy-tale",
		"authentic",
		"famous",
	};
	static const char *guff_surround[] = {
		"surroundings",
		"embrace",
		"setting",
		"vista",
		"view",
	};
	static const char *guff_over[] = {
		"features",
		"showcases",
		"benefits from",
	};
	static const char *guff_ideal[] = {
		"The ideal",
		"The perfect",
		"A classic",
	};
	static const char *guff_destination[] = {
		"destination",
		"target",
	};
	static const char *guff_daring[] = {
		"daring",
		"adventurous",
		"serious",
		"curious",
		"interested",
	};
	static const char *guff_tourist[] = {
		"traveller",
		"tourist",
	};

	sprintf(buf, "This %s %s%s %s in the %s %s of %s and %s %s "
					"%s %s for the %s %s!",
		GUFF(really), GUFF(pretty), GUFF(town), GUFF(sits), GUFF(dramatic), GUFF(surround), t->geography, GUFF(over), t->underground,
		GUFF(ideal), GUFF(destination), GUFF(daring), GUFF(tourist));

	#undef GUFF

	return buf;
}

/**
 * Add and return an empty town. Has a random name.
 * Note that as this uses realloc, it invalidates all existing pointers to towns.
 */
static struct town *world_new_town(void)
{
	z_info->town_max++;
	t_info = mem_realloc(t_info, sizeof(struct town) * z_info->town_max);
	struct town *ret = t_info + (z_info->town_max - 1);
	memset(ret, 0, sizeof(*ret));
	world_name_town(ret, world_make_name());
	return ret;
}

/** Build town-town distances - save the seed used so it can be restored
*/
void world_build_distances(void)
{
	if (world_distance)
		mem_free(world_distance);
	world_distance = mem_zalloc(sizeof(*world_distance) *  z_info->town_max * z_info->town_max);
	{
		int *tx = mem_zalloc(sizeof(int) * z_info->town_max);
		int *ty = mem_zalloc(sizeof(int) * z_info->town_max);

		// Repeat until nowhere is too close to anywhere else
		bool ok;
		int size = 1399399;
		int reps = 0;
		do {
			if (world_town_seed == 0)
				world_town_seed = Rand_u32b() | 1;
			u32b rng = world_town_seed;

			for(int i=0;i<z_info->town_max;i++) {
				rng = LCRNG(rng);
				tx[i] = rng % size;
				rng = LCRNG(rng);
				ty[i] = rng % size;
			}
			ok = true;
			for(int i=0;i<z_info->town_max;i++) {
				for(int j=0;j<z_info->town_max;j++) {
					if (i != j) {
						u32b dist = ((abs(tx[i] - tx[j])) + (abs(ty[i] - ty[j])));
						if (dist < 80000) {
							ok = false;
							break;
						}
					}
				}
			}
			if (!ok) {
				size += 9973;
				world_town_seed = Rand_u32b() | 1;
			}
			reps++;
		} while (!ok);

		// Fill distance array
		for(int i=0;i<z_info->town_max;i++) {
			for(int j=0;j<z_info->town_max;j++) {
				double xc1 = tx[i];
				double yc1 = ty[i];
				double xc2 = tx[j];
				double yc2 = ty[j];
				world_distance[j + (i * z_info->town_max)] =
					sqrt(((xc1 - xc2) * (xc1 - xc2)) + ((yc1 - yc2) * (yc1 - yc2)));
			}
		}

		// and clean up
		mem_free(tx);
		mem_free(ty);
	}
}

static char *lev_or_none(const char *level) {
	return streq(level, "None") ? NULL : string_make(level);
}

/**
 * Add a level entry to the world
 * Parameters: depth, name, up to, down to
 */
static void add_level(int depth, const char *name, const char *up, const char *down)
{
	struct level *lev = mem_zalloc(sizeof(struct level));
	lev->depth = depth;
	lev->name = string_make(name);
	lev->up = lev_or_none(up);
	lev->down = lev_or_none(down);
	lev->next = world;
	world = lev;
}

/**
 * Generate dungeons
 */
static void world_init_dungeons(void)
{
	/* Start with a clear world */
	cleanup_levels(&world);

	/* Copy in any levels defined directly in the file */
	world = dup_levels(world_init);

	/* For each dungeon, generate a random depth for min and max.
	 * Then for each level, create a level entry.
	 * Link it to the directly above or below entries.
	 **/
	struct dungeon *d = dungeons;
	while (d) {
		char name[80];
		char up[80];
		char down[80];
		int min = randcalc(d->min, 0, RANDOMISE);
		int max = randcalc(d->max, 0, RANDOMISE);
		for(int l=min; l<=max; l++) {
			strnfmt(name, sizeof(name), "%s %d", d->name, l);
			if (l == min)
				strcpy(up, "None");
			else
				strnfmt(up, sizeof(up), "%s %d", d->name, l-1);
			if (l == max)
				strcpy(down, "None");
			else
				strnfmt(down, sizeof(down), "%s %d", d->name, l+1);
			add_level(l, name, up, down);
		}
		d = d->next;
	}

	/* World structure is complete.
	 * Can now check that all levels referred to exist
	 **/
	struct level *level_check;
	for (level_check = world; level_check; level_check = level_check->next) {
		struct level *level_find = world;

		/* Check upwards */
		if (level_check->up) {
			while (level_find && !streq(level_check->up, level_find->name)) {
				level_find = level_find->next;
			}
			if (!level_find) {
				quit_fmt("Invalid level reference %s", level_check->up);
			}
		}

		/* Check downwards */
		level_find = world;
		if (level_check->down) {
			while (level_find && !streq(level_check->down, level_find->name)) {
				level_find = level_find->next;
			}
			if (!level_find) {
				quit_fmt("Invalid level reference %s", level_check->down);
			}
		}
	}
}

/** Connect a town to a level. NULL (to disconnect) is allowed.
 */
static void world_town_level(struct town *town, const char *dungeon)
{
	if (town->downto)
		string_free(town->downto);
	town->downto = NULL;
	if (dungeon)
		town->downto = string_make(dungeon);
}

/**
 * Connect a town to a dungeon. Dungeon name must be non-null, but strings
 * which are not prefixes of any level are allowed (they disconnect the town
 * from all levels, like passing NULL to world_town_level)
 */
static void world_town_dungeon(struct town *town, const char *dungeon)
{
	/* Find the lowest depth */
	struct level *lev = world;
	int bestdepth = 1000000;
	const char *bestname = NULL;
	do {
		if (!strncmp(lev->name, dungeon, strlen(dungeon))) {
			int depth = atoi(lev->name + strlen(dungeon));
			if (depth < bestdepth) {
				bestdepth = depth;
				bestname = lev->name;
			}
		}
		lev = lev->next;
	} while (lev);
	world_town_level(town, bestname);
}

/** Assign a location (town and store) to quests.
 *
 * The 'location' may be a town name (so convert with get_town_by_name),
 * or a quest name (convert by get_quest_by_name and extract the destination).
 * Then convert into a town and store index.
 * The order of quests should not matter, though. So if a quest can't be found,
 * repeat until no more progress can be made.
 * 
 * If all quests with references are now found, continue - otherwise error.
 *
 * Finally for each town, select one quest randomly.
 *
 * Returns true if successful
 */ 
bool world_locate_quests(void)
{
	/* Clear */
	for (int i = 0; i < z_info->quest_max; i++) {
		struct quest *q = &player->quests[i];
		q->town = q->store = -1;
		for(int l = 0; l < q->quests; l++) {
			q->loc[l].store = q->loc[l].town = -1;
		}
	}

	/* Select randomly */
	int *select = mem_zalloc(sizeof(int) * z_info->quest_max);
	for (int i = 0; i < z_info->quest_max; i++) {
		struct quest *q = &player->quests[i];
		if (q->quests)
			select[i] = randint0(q->quests);
	}

	/* Quests with town names - must also have a store name */
	for (int i = 0; i < z_info->quest_max; i++) {
		struct quest *q = &player->quests[i];

		for(int l = 0; l < q->quests; l++) {
			struct town *town = get_town_by_dungeon(q->loc[l].location);
			struct store *store = get_store_by_name(q->loc[l].storename);
			if (town) {
				if (!store) {
					quit(format("Unable to find quest: town %s has no store %s",
						q->loc[l].location, q->loc[l].storename));
					return false;
				} else {
					q->loc[l].town = town - t_info;
					q->loc[l].store = store->sidx;

					/* Are all locations of this quest now known?
					 * If so, fill in town and store
					 **/
					bool ok = true;
					for(int k=0; k<q->quests; k++) {
						if ((q->loc[l].store < 0) || (q->loc[l].town < 0)) {
							ok = false;
							break;
						}
					}
					if (ok) {
						q->town = q->loc[select[i]].town;
						q->store = q->loc[select[i]].store;
					}
				}
			}
		}
	}

	/* Quests with quest names.
	 * Repeat until no more progress can be made
	 **/
	bool missing;
	bool found;
	bool unknown;
	char *badname = "";
	do {
		missing = found = unknown = false;
		for (int i = 0; i < z_info->quest_max; i++) {
		struct quest *q = &player->quests[i];
			for(int l = 0; l < q->quests; l++) {
				/* Not yet found */
				if (q->loc[l].town < 0) {
					struct quest *src = get_quest_by_name(q->loc[l].location);
					missing = true;
					if (!(*badname))
						badname = q->loc[l].location;

					/* It's a known quest */
					if (src) {
						/* It has a known location */
						if ((src->town >= 0) && (src->store >= 0)) {
							/* Place this quest there */
							q->loc[l].town = src->town;
							found = true;
							missing = false;

							/* Assign store if needed */
							if (q->loc[l].storename) {
								struct store *store = get_store_by_name(q->loc[l].storename);
								if (!store) {
									quit(format("Unable to find quest: town %s has no store %s",
										q->loc[l].location, q->loc[l].storename));
									return false;
								}
								q->loc[l].store = store->sidx;
							} else {
								q->loc[l].store = src->store;
							}

							/* Are all locations of this quest now known?
							 * If so, fill in town and store
							 **/
							bool ok = true;
							for(int k=0; k<q->quests; k++) {
								if ((q->loc[l].store < 0) || (q->loc[l].town < 0)) {
									ok = false;
									break;
								}
							}
							if (ok) {
								q->town = q->loc[select[i]].town;
								q->store = q->loc[select[i]].store;
							}
						}
					} else {
						/* Unrecognized name */
						unknown = true;
						badname = q->loc[l].location;
					}
				}
			}
		}
	} while (found);

	/* At this point if missing == true, at least one quest's location is unknown.
	 * If unknown is also true, at least one quest's name was unrecognized.
	 */
	if (missing) {
		if (unknown) {
			quit(format("Unable to find quest: '%s' unrecognised", badname));
		} else {
			quit(format("Unable to find quest: circular reference to '%s'?", badname));
		}
		return false;
	}
	return true;
}

/** Connect all possible pairs of towns */
void world_connect_complete(void)
{
	for(int i=0;i<z_info->town_max;i++)
		for(int j=0; j<i; j++)
			world_connect_towns(t_info+i, t_info+j);
}

/**
 * Generate towns, etc.
 * Returns true if successful.
 */
bool world_init_towns(void)
{
	bool success = true;

	world_init_dungeons();

	/* Clear */
	world_cleanup_towns();
	Rand_quick = false;

	/* New seed */
	world_town_seed = Rand_u32b() | 1;

	/*
	 * Towns.
	 * There is always one town with the Fortress, and at least a few others.
	 * You start somewhere other than the Fortress.
	 * For quests to work, well-known names must be used for any town with a quest (which is probably all of them)
	 * ... although a reasonable extension would be to not allow quests to start in a town which is not used this game.
	 *
	 * So, determine the number of towns.
	 * Assign each a dungeon, and one of a small selection of names.
	 * Some of these dungeons are necessary (perhaps all), though - because there is an endboss, midboss or other special level required for a quest.
	 * 		- but as they are not referred to by dungeon name - only by town and level - outside world.txt, it's OK to switch names around. (And not
	 * just names - other properties, including depth? But this may prevent boss/levels being at a reasonable place.)
	 * 		- This probably needs to be limited to a subset of roughly equivalent levels, if it's done at all.
	 *
	 * How are they connected?
	 * Complete is possible.
	 * Could put Fortress in the centermost, and only connect to Fortress or somehere closer. (From Fortress, all are reachable)
	 * (Could even switch midgame, e.g. to <>Fortress only. Or *from* that to completing "Platinum Class", as a bonus)
	 *
	 * - Backgrounds - at least one is always volcanic ("Volcano"?) for quests. One may be an island (atoll? Need some way to keep you in)
	 * 		- 'caves' is the cavern (or mine?) 'famous jewlry' is the mine etc.
	 */

	/* Create towns, dungeons */
	struct town *t;
	int geog[5] = { 0, 1, 2, 3, 4, };
	shuffle(geog, sizeof(geog) / sizeof(*geog));

	world_town_dungeon(t = world_new_town(), "Fortress");
	t->underground = "a historic underground fortress, said to extend hundreds of levels down.";

	world_town_dungeon(t = world_new_town(), "Sewers");
	t->underground = "mysterious catacombs of unknown purpose and extent.";

	world_town_dungeon(t = world_new_town(), "Caverns");
	t->underground = "lofty caverns and wonderful waterfalls of twisted rock.";

	world_town_dungeon(t = world_new_town(), "Mine");
	t->underground = "the famous gold mines - maybe you'll get lucky?";

	world_town_dungeon(t = world_new_town(), "Stores");
	t->underground = "an abandoned bunker, once a deadly secret.";

	/* The 5 non-volcano towns.
	 * One has a lake (and no street?)
	 */
	t = t_info + geog[0];
	t->geography = "a placid, bottle-green lake";
	t->lake = true;

	/* Nothing special for the others? */
	t = t_info + geog[1];
	t->geography = "rolling green hills";

	t = t_info + geog[2];
	t->geography = "soaring granite cliffs";

	t = t_info + geog[3];
	t->geography = "snow-capped mountains";

	t = t_info + geog[4];
	t->geography = "the distant, smoky mountains";

	/* Volcano - fixed geography and dungeon */
	world_town_dungeon(t = world_new_town(), "Volcano");
	t->geography = "an awesome active volcano";
	t->underground = "enticing volcanic caves!";
	t->lava_num = 3 + randint0(3);

	for(int i=0; i<z_info->town_max; i++) {
		t->geography = string_make(t->geography);
		t->underground = string_make(t->underground);
	}

	/* Find and enter the player's town (any, except Fortress) */
	world_change_town(t_info + randint1(z_info->town_max - 1));

	/* Make connections */
	world_connect_complete();

	/* Generate distances */
	world_build_distances();

	/* Generate stores and contents */
	store_reset();

	/* Place quests */
	success = world_locate_quests();

	assert(player);
	town_gen_all(player, z_info->town_wid, z_info->town_hgt);

	return success;
}

/**
 * ------------------------------------------------------------------------
 * Initialize town names
 * ------------------------------------------------------------------------ */

static enum parser_error parse_town_name(struct parser *p,  char ***names, int *n_names) {
	(*n_names)++;
	*names = mem_realloc(*names, sizeof(**names) * (*n_names));
	(*names)[(*n_names)-1] = string_make(parser_getstr(p, "text"));
	return PARSE_ERROR_NONE;
}

static enum parser_error parse_town_name_simple(struct parser *p) {
	return parse_town_name(p, &town_full_name, &town_full_names);
}

static enum parser_error parse_town_name_front(struct parser *p) {
	return parse_town_name(p, &town_front_name, &town_front_names);
}

static enum parser_error parse_town_name_back(struct parser *p) {
	return parse_town_name(p, &town_back_name, &town_back_names);
}

struct parser *init_parse_town_names(void) {
	struct parser *p = parser_new();
	parser_reg(p, "S str text", parse_town_name_simple);
	parser_reg(p, "F str text", parse_town_name_front);
	parser_reg(p, "B str text", parse_town_name_back);
	return p;
}

static errr run_parse_town_names(struct parser *p) {
	return parse_file_quit_not_found(p, "town-names");
}

static errr finish_parse_town_names(struct parser *p) {
	parser_destroy(p);
	return 0;
}

static void cleanup_town_names(void)
{
	for(int i=0;i<town_full_names;i++)
		mem_free(town_full_name[i]);
	mem_free(town_full_name);
	for(int i=0;i<town_front_names;i++)
		mem_free(town_front_name[i]);
	mem_free(town_front_name);
	for(int i=0;i<town_back_names;i++)
		mem_free(town_back_name[i]);
	mem_free(town_back_name);
}

struct file_parser town_names_parser = {
	"town-names",
	init_parse_town_names,
	run_parse_town_names,
	finish_parse_town_names,
	cleanup_town_names
};

/**
 * ------------------------------------------------------------------------
 * Initialize world map
 * ------------------------------------------------------------------------ */
static enum parser_error parse_world_level(struct parser *p) {
	const int depth = parser_getint(p, "depth");
	const char *name = parser_getsym(p, "name");
	const char *up = parser_getsym(p, "up");
	const char *down = parser_getsym(p, "down");
	struct level *last = parser_priv(p);
	struct level *lev = mem_zalloc(sizeof *lev);

	if (last) {
		last->next = lev;
	} else {
		world = lev;
	}
	lev->depth = depth;
	lev->name = string_make(name);
	lev->up = lev_or_none(up);
	lev->down = lev_or_none(down);
	parser_setpriv(p, lev);
	return PARSE_ERROR_NONE;
}

static enum parser_error parse_world_dungeon(struct parser *p) {
	struct dungeon *d = mem_zalloc(sizeof *d);

	if (dungeons) {
		d->next = dungeons;
	}
	dungeons = d;
	d->min = parser_getrand(p, "min");
	d->max = parser_getrand(p, "max");
	const char *name = parser_getsym(p, "name");
	d->name = string_make(name);
	return PARSE_ERROR_NONE;
}

struct parser *init_parse_world(void) {
	struct parser *p = parser_new();

	parser_reg(p, "level int depth sym name sym up sym down",
			   parse_world_level);
	parser_reg(p, "dungeon rand min rand max sym name",
			   parse_world_dungeon);
	return p;
}

static errr run_parse_world(struct parser *p) {
	return parse_file_quit_not_found(p, "world");
}

static errr finish_parse_world(struct parser *p) {
	world_init = dup_levels(world);
	parser_destroy(p);
	return 0;
}

static struct level *dup_levels(struct level *src) {
	struct level *last = NULL;
	struct level *lev = NULL;
	while (src) {
		lev = mem_zalloc(sizeof(struct level));
		memcpy(lev, src, sizeof(struct level));
		lev->name = string_make(lev->name);
		lev->up = string_make(lev->up);
		lev->down = string_make(lev->down);
		lev->next = last;
		last = lev;
		src = src->next;
	};
	return lev;
}

static void cleanup_levels(struct level **world) {
	struct level *level = *world;
	while (level) {
		struct level *old = level;
		string_free(level->name);
		string_free(level->up);
		string_free(level->down);
		level = level->next;
		mem_free(old);
	}
	*world = NULL;
}

static void cleanup_world(void)
{
	cleanup_levels(&world_init);
	cleanup_levels(&world);
}

struct file_parser world_parser = {
	"world",
	init_parse_world,
	run_parse_world,
	finish_parse_world,
	cleanup_world
};

