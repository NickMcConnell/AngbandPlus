/**
 * \file list-origins.h
 * \brief List of object origins
 */
ORIGIN(NONE,		-1,	"")
ORIGIN(FLOOR,		1,	"Found lying on the floor %s")
ORIGIN(CHEST,		1,	"Taken from a container found %s")
ORIGIN(SPECIAL,		1,	"Found lying on the floor of a special room %s")
ORIGIN(PIT,			1,	"Found lying on the floor in a pit %s")
ORIGIN(VAULT,		1,	"Found lying on the floor in a vault %s")
ORIGIN(LABYRINTH,	1,	"Found lying on the floor of a labyrinth %s")
ORIGIN(CAVERN,		1,	"Found lying on the floor of a cavern %s")
ORIGIN(RUBBLE,		1,	"Found under some rubble %s")
ORIGIN(MIXED,		-1,	"")                 /* stack with mixed origins */
ORIGIN(DROP,		2,	"Dropped by %s %s") /* normal monster drops */
ORIGIN(DROP_SPECIAL,2,	"Dropped by %s %s") /* from monsters in special rooms */
ORIGIN(DROP_PIT,	2,	"Dropped by %s %s") /* from monsters in pits/nests */
ORIGIN(DROP_VAULT,	2,	"Dropped by %s %s") /* from monsters in vaults */
ORIGIN(STATS,		-1,	"")  /* ^ only the above are considered by main-stats */
ORIGIN(ACQUIRE,		1,	"Snatched by an acquisition card %s")
ORIGIN(STORE,		0,	"Bought from a store")
ORIGIN(STOLEN,		-1,	"Stolen from a store")
ORIGIN(BIRTH,		0,	"With you from the beginning")
ORIGIN(REWARD,		0,	"A reward for completing a task")
ORIGIN(CHEAT,		0,	"Created by debug option")
ORIGIN(DROP_BREED,	2,	"Dropped by %s %s") /* from breeders */
ORIGIN(DROP_SUMMON,	2,	"Dropped by %s %s") /* from combat summons */
ORIGIN(DROP_UNKNOWN,1,	"Dropped by an unknown monster %s")
ORIGIN(DROP_POLY,	2,	"Dropped by %s %s") /* from polymorphees */
ORIGIN(DROP_MIMIC,	2,	"Dropped by %s %s") /* from mimics */
ORIGIN(DROP_WIZARD,	2,	"Dropped by %s %s") /* from wizard mode summons */
ORIGIN(DROP_SHK,	2,	"Dropped by a shopkeeper") /* from shks */
ORIGIN(PROMOTION,	0,	"Supplied from Field HQ")
ORIGIN(PRINTER,		1,	"Constructed yourself %s")
