/* File: icon.h */

/* Purpose: icon environment definitions */

/*
 * Copyright (c) 1997-2009 Tim Baker
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#ifndef _INCLUDE_ICON_H_
#define _INCLUDE_ICON_H_

#include "icon-dll.h"

/*
 * Constants for g_icon_type[] index.
 * icon_type.type constants.
 */

/* Special type "none". It's a transparent icon. */
#define ICON_TYPE_NONE 0

#define ICON_TYPE_BLANK 1
#define ICON_TYPE_DEFAULT 2

/*
 * Constants for g_assign[] index.
 */
#define ASSIGN_MONSTER 0
#define ASSIGN_OBJECT 1
#define ASSIGN_CHARACTER 2
#define ASSIGN_FEATURE 3
#define ASSIGN_SHOPKEEPER 4
#define ASSIGN_ARTIFACT 5
#define ASSIGN_MAX 6

/*
 * Constants for t_assign.assignType.
 */
#define ASSIGN_TYPE_ALTERNATE 0
#define ASSIGN_TYPE_FLAVOR 1
#define ASSIGN_TYPE_ICON 2
#define ASSIGN_TYPE_SPRITE 3
#define ASSIGN_TYPE_MAX 4

/* One assigned alternate */
typedef struct t_assign_alternate {
	int assignType; /* Required field */
	int index;
} t_assign_alternate;

/* One assigned flavor */
typedef struct t_assign_flavor {
	int assignType; /* Required field */
	int group;
	int index;
} t_assign_flavor;

/* One assigned icon */
typedef struct t_assign_icon {
	int assignType; /* Required field */
	int type;
	int index;
	int ascii;
} t_assign_icon;

/* One assigned sprite */
typedef struct t_assign_sprite {
	int assignType; /* Required field */
	int index;
} t_assign_sprite;

/* One assignment */
typedef union t_assign {
	int assignType;
	t_assign_alternate alternate;
	t_assign_flavor flavor;
	t_assign_icon icon;
	t_assign_sprite sprite;
} t_assign;

/*
 * Icon assignment for each member of each group (monster,
 * object, features, etc)
 */
typedef struct t_assign_group {
	int count; /* Number of elements in array */
	t_assign *assign; /* Array of iassignments */
} t_assign_group;

extern t_assign_group g_assign[ASSIGN_MAX];
extern t_assign g_assign_none;

/* Constants for g_feat_lite[] */
#define FT_LIGHT_NONE 0 /* Always use 1 icon, no tinting */
#define FT_LIGHT_ICON 1 /* Use 3 icons, no tinting */
#define FT_LIGHT_TINT 2 /* Use 1 icon, with tinting as needed */

extern int *g_feat_lite;

/*
 * When TRUE, uses 4 levels of grid light. The icon set must
 * support 4 levels for this to work.
 */
extern int g_torchlite;

/* Feature index for masked features */
extern int *g_background;

/* One icon for each feature, for each shape */
extern t_assign *g_assignshape[GRID_SHAPE_MAX];
extern bool g_assignshape_inuse;
extern cptr keyword_wall[];

/* Layers of feature icons */
enum {
ICON_LAYER_1,
ICON_LAYER_2,
ICON_LAYER_3,
ICON_LAYER_4,
ICON_LAYER_MAX
};

/* Per-layer assignment for each cave location */
extern t_assign *g_icon_map[ICON_LAYER_MAX][DUNGEON_HGT];

#define GRID_EFFECT
#ifdef GRID_EFFECT
/*
 * Bolt/ball/object effects and invert-on-hit effect
 */
enum {
	GRID_EFFECT_INVERT = 0x0001,
};
typedef struct {
	IconSpec icon;
	int flags; /* GRID_EFFECT_XXX */
} t_grid_effect;
extern t_grid_effect *g_grid_effect[DUNGEON_HGT];
#endif /* GRID_EFFECT */

/*
 * Information about what to display.
 */
typedef struct t_display {
	bool blank; /* Totally uninteresting grid */
	bool anim; /* The icon is animated */
	TintPtr tint; /* Tint to apply, or NULL */
	IconSpec fg; /* Foreground */
	IconSpec bg[ICON_LAYER_MAX]; /* Background (when typeFG is masked) */
} t_display;

/*
 * TYPE_FLAVOR refers to one of the predefined flavor types, which
 * are themselves defined by the structure below.
 */
typedef struct t_flavor {
	char *desc; /* Keyword for this flavor (potion, ring, mushroom) */
	int tval; /* Object tval flavor applies to */
	int count; /* Number of assignments */
	int *sorted; /* Map unsorted index to assignment index */
	IconSpec *icon; /* Assignments */
	byte *color; /* TERM_XXX colors */
} t_flavor;

extern t_flavor *g_flavor; /* Array of flavor types */
extern int g_flavor_count; /* Number of flavors */
extern Tcl_HashTable g_flavor_table; /* Map flavor name to g_flavor[] index */

typedef struct t_sprite {
	int count; /* Number of frames */
	int frame; /* Index of current frame */
	int speed; /* Ticks between frames */
	int ticks; /* Tick counter */
	int reverse; /* */
	bool changed; /* */
	IconSpec *icon; /* Assignments */
} t_sprite;

extern t_sprite *g_sprite; /* Array of sprites */
extern int g_sprite_count; /* Number of sprites in g_sprite[] array */

/*
 * Constants for t_alternate.reason.
 */
#define REASON_NONE			0	/* No reason */
#define REASON_NUMBER		1	/* Use "second" if only one object in stack */
#define REASON_IDENT		2	/* Use "second" if object identified */
#define REASON_FEATURE		3	/* Use "second" icon for door/pillar */
#define REASON_FRIEND		4	/* Use "second" icon for friendly monster */

typedef struct t_alternate {
	int reason; /* Reason code */
	int count; /* Number of icons */
	IconSpec *icon; /* The icons */
} t_alternate;

extern t_alternate *g_alternate; /* Array of "alternate" info */
extern int g_alternate_count; /* Number of elements in g_alternate[] array */

/* g_effect[EFFECT_SPELL_BALL/BOLT].icon index */
enum {
EFFECT_SPELL_ARROW,
EFFECT_SPELL_MISSILE,
EFFECT_SPELL_MANA,
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)
EFFECT_SPELL_HOLY_ORB,
#endif /* ANGBANDTK, KANGBANDTK */
EFFECT_SPELL_LITE_WEAK,
EFFECT_SPELL_DARK_WEAK,
EFFECT_SPELL_WATER,
EFFECT_SPELL_PLASMA,
EFFECT_SPELL_METEOR,
EFFECT_SPELL_ICE,
EFFECT_SPELL_GRAVITY,
EFFECT_SPELL_INERTIA,
EFFECT_SPELL_FORCE,
EFFECT_SPELL_TIME,
EFFECT_SPELL_ACID,
EFFECT_SPELL_ELEC,
EFFECT_SPELL_FIRE,
EFFECT_SPELL_COLD,
EFFECT_SPELL_POIS,
EFFECT_SPELL_LITE,
EFFECT_SPELL_DARK,
EFFECT_SPELL_CONFUSION,
EFFECT_SPELL_SOUND,
EFFECT_SPELL_SHARD,
EFFECT_SPELL_NEXUS,
EFFECT_SPELL_NETHER,
EFFECT_SPELL_CHAOS,
EFFECT_SPELL_DISENCHANT,
#if defined(ZANGBANDTK)
EFFECT_SPELL_ROCKET,
EFFECT_SPELL_NUKE,
EFFECT_SPELL_DEATH_RAY,
EFFECT_SPELL_HOLY_FIRE,
EFFECT_SPELL_HELL_FIRE,
EFFECT_SPELL_DISINTEGRATE,
EFFECT_SPELL_PSI,
EFFECT_SPELL_PSI_DRAIN,
EFFECT_SPELL_TELEKINESIS,
EFFECT_SPELL_DOMINATION,
#endif /* ZANGBANDTK */
EFFECT_SPELL_MAX
};

/* g_effect[EFFECT_AMMO].icon index */
enum {
EFFECT_AMMO_ARROW,
EFFECT_AMMO_BOLT,
EFFECT_AMMO_MAX
};

/* g_effect[] index */
#define EFFECT_SPELL_BALL 0
#define EFFECT_SPELL_BOLT 1
#define EFFECT_AMMO 2
#define EFFECT_MAX 3

typedef struct t_effect {
	char **name; /* Keyword for each icon */
	IconSpec *icon; /*  */
} t_effect;

extern t_effect *g_effect; /* Array of "effect" info */
extern cptr keyword_effect_spell[];
extern cptr keyword_effect_ammo[];

typedef struct t_darken {
	TintTable table; /* Tint table for darkening feature icons */
	int brightness;
	int contrast;
	double gamma;
} t_darken;
extern t_darken g_darken[3];

/* Tint table for torchlite -- NOT USED */
extern TintTable g_yellow;

/* Recalculate g_icon_map[] */
extern bool g_icon_map_changed;

extern bool allow_animation; /* Option: Animation */
extern void angtk_start_timer(void);
extern void angtk_stop_timer(void);

extern void icons_init(void);
extern void icons_exit(void);
extern void icons_setup(int width, int height, int depth, int style);
extern void icons_unload(void);

/* t_grid -> t_display */
extern void get_display_info(int y, int x, t_display *displayPtr);

extern IconPtr SetIconBits(IconPtr bg, IconPtr fg, IconPtr mk, TintTable t,
	IconPtr b);

extern unsigned char *g_palette_rgb;

extern int *g_image_monster;

extern void FinalIcon(IconSpec *iconOut, t_assign *assignPtr, int hack,
	object_type *o_ptr, monster_type *m_ptr);
extern int update_sprites(void);
extern bool is_sprite(t_assign *assignPtr);
extern int assign_parse(Tcl_Interp *interp, t_assign *assignPtr, char *desc);
extern char *assign_print(char *buf, t_assign *assignPtr);
extern char *assign_print2(char *buf, int assignType, int assignIndex);
extern char *assign_print_object(char *buf, object_type *o_ptr);
extern char *assign_print_monster(char *buf, monster_type *m_ptr);
extern void get_object_assign(t_assign *assignPtr, object_type *o_ptr);
extern void get_monster_assign(t_assign *assignPtr, monster_type *m_ptr);
extern void Bind_Assign(int to, int toindex, t_assign *assignPtr);

extern int objcmd_vault _ANSI_ARGS_((ClientData clientData,
		    Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]));
extern int vault_icon(int y, int x, bool test_feat, t_assign assign[ICON_LAYER_MAX]);

#endif /* _INCLUDE_ICON_H_ */
