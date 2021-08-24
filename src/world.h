/**
 * \file world.h
 * \brief World (surface, towns, wilderness): interface
 *
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
 */

struct town {
	struct town **connect;		/* Array of connected towns */
	u32b connections;			/* Total number of connected towns */
	char *name;					/* Name of town */
	struct store *stores;		/* Stores */
	char *downto;				/* Go down to this level */
	char *underground;			/* "over ancient caverns" */
	char *geography;			/* "an active volcano" */
	bool lake;					/* generate a lake */
	byte lava_num;				/* and this many streamers of lava */
};

/* The world contains z->town_max towns, in this array */
extern struct town *t_info;

extern u32b world_town_seed;

extern struct file_parser world_parser;
extern struct file_parser town_names_parser;

extern int world_connections(struct town *t);
extern bool world_init_towns(void);
extern void world_cleanup_towns(void);
void world_change_town(struct town *t);
void world_connect_towns(struct town *a, struct town *b);
int world_departure_time(struct town *from, struct town *to);
int world_between(struct town *from, struct town *to);
int world_airline_fare(struct town *from, struct town *to);
int world_flight_time(struct town *from, struct town *to);
void world_build_distances(void);
char *world_describe_town(struct town *t);
