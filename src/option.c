/*
 * File: options.c
 * Purpose: Options table and definitions.
 *
 * Copyright (c) 1997 Ben Harrison
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
#include "option.h"

/*
 * Option screen interface
 */
const byte option_page[OPT_PAGE_MAX][OPT_PAGE_PER] =
{
	/*** User-Interface ***/

	{
		OPT_rogue_like_commands,
		OPT_quick_messages,
		OPT_floor_query_flag,
		OPT_carry_query_flag,
		OPT_use_old_target,
		OPT_always_pickup,
		OPT_always_repeat,
		OPT_stack_force_notes,
		OPT_ring_bell,
		OPT_easy_open,
		OPT_easy_alter,
		OPT_easy_floor,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE
	},

	/*** Disturbance ***/

	{
		OPT_run_ignore_stairs,
		OPT_run_ignore_doors,
		OPT_run_cut_corners,
		OPT_run_use_corners,
		OPT_disturb_move,
		OPT_disturb_near,
		OPT_disturb_panel,
		OPT_disturb_state,
		OPT_disturb_minor,
		OPT_verify_destroy,
		OPT_auto_more,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE
	},

	/*** Game-Play ***/

	{
		OPT_auto_scum,
		OPT_expand_look,
		OPT_view_perma_grids,
		OPT_view_torch_grids,
		OPT_dungeon_align,
		OPT_dungeon_stair,
		OPT_flow_by_sound,
		OPT_flow_by_smell,
		OPT_smart_monsters,
		OPT_smart_packs,
		OPT_smart_learn,
		OPT_smart_cheat,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE
	},

	/*** Efficiency ***/

	{
		OPT_view_reduce_lite,
		OPT_hidden_player,
		OPT_avoid_abort,
		OPT_avoid_other,
		OPT_flush_failure,
		OPT_flush_disturb,
		OPT_compress_savefile,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
	},

	/*** Display ***/

	{
		OPT_depth_in_feet,
		OPT_show_labels,
		OPT_show_weights,
		OPT_show_choices,
		OPT_show_details,
		OPT_show_flavors,
		OPT_hilite_player,
		OPT_view_yellow_lite,
		OPT_view_bright_lite,
		OPT_view_granite_lite,
		OPT_view_special_lite,
 		OPT_center_player,
 		OPT_run_avoid_center,
		OPT_show_piles,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE
	},

	/*** Birth ***/

	{
		OPT_birth_point_based,
		OPT_birth_auto_roller,
		OPT_birth_maximize,
		OPT_birth_preserve,
		OPT_birth_ironman,
		OPT_birth_no_stores,
		OPT_birth_no_artifacts,
		OPT_birth_rand_artifacts,
		OPT_birth_no_stacking,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE
	},

	/*** Cheat ***/

	{
		OPT_cheat_peek,
		OPT_cheat_hear,
		OPT_cheat_room,
		OPT_cheat_xtra,
		OPT_cheat_know,
		OPT_cheat_live,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE
	}
};

typedef struct
{
	const char *name;
	const char *description;
	bool normal;
} option_entry;

static option_entry options[OPT_MAX] =
{
{ "rogue_like_commands", "Rogue-like commands",                         FALSE }, /* 0 */
{ "quick_messages",      "Activate quick messages",                     TRUE },  /* 1 */
{ "floor_query_flag",	"Prompt for floor item selection", 				FALSE }, /* 2 */
{ "carry_query_flag",	"Prompt before picking things up", 				TRUE }, /* 3 */
{ "use_old_target",      "Use old target by default",                   FALSE }, /* 4 */
{ "pickup_always",       "Always pickup items",                         FALSE }, /* 5 */
{ "always_repeat",		"Repeat obvious commands", 						FALSE }, /* 6 */
{ "depth_in_feet",       "Show dungeon level in feet",                  FALSE }, /* 7 */
{ "stack_force_notes",	"Merge inscriptions when stacking", 			FALSE }, /* 8 */ /* eliminate? */
{ NULL,					NULL, 											FALSE }, /* 9 */
{ "show_labels",         "Show labels in equipment listings",           TRUE },  /* 10 */
{ "show_weights",		"Show weights in all object listings", 			TRUE }, /* 11 */
{ "show_choices",		"Show choices in inven/equip windows", 			TRUE }, /* 12 */
{ "show_details",		"Show details in monster descriptions", 		TRUE }, /* 13 */
{ "ring_bell",           "Audible bell (on errors, etc)",               TRUE },  /* 14 */
{ "show_flavors",        "Show flavors in object descriptions",         TRUE },  /* 15 */
{ "run_ignore_stairs",	"When running, ignore stairs", 					TRUE }, /* 16 */
{ "run_ignore_doors",	"When running, ignore doors", 					TRUE }, /* 17 */
{ "run_cut_corners",	"When running, cut corners", 					TRUE }, /* 18 */
{ "run_use_corners",	"When running, use corners", 					TRUE }, /* 19 */
{ "disturb_move",        "Disturb whenever any monster moves",          FALSE }, /* 20 */
{ "disturb_near",        "Disturb whenever viewable monster moves",     TRUE },  /* 21 */
{ "disturb_detect",      "Disturb whenever leaving trap detected area", TRUE },  /* 22 */
{ "disturb_state",       "Disturb whenever player state changes",       TRUE },  /* 23 */
{ "disturb_minor",       "Disturb whenever boring things happen",       TRUE },  /* 24 */
{ NULL,                  NULL,                                          FALSE }, /* 25 */
{ NULL,                  NULL,                                          FALSE }, /* 26 */
{ NULL,                  NULL,                                          FALSE }, /* 27 */
{ "verify_destroy",		"Verify destruction of objects", 				TRUE }, /* 28 */ /* eliminate? */
{ NULL,                  NULL,                                          FALSE }, /* 29 */
{ NULL,                  NULL,                                          FALSE }, /* 30 */
{ NULL,                  NULL,                                          FALSE }, /* 31 */
{ NULL,                  NULL,                                          FALSE }, /* 32 */
{ "auto_scum",			"Auto-scum for good levels", 					FALSE }, /* 33 */ /* relocate? */
{ NULL,                  NULL,                                          FALSE }, /* 34 */
{ NULL,                  NULL,                                          FALSE }, /* 35 */
{ "expand_look",		"Expand the power of the look command", 		TRUE }, /* 36 */ /* eliminate? */
{ NULL,                  NULL,                                          FALSE }, /* 37 */
{ "view_perma_grids",	"Map remembers all perma-lit grids", 			TRUE }, /* 38 */
{ "view_torch_grids",	"Map remembers all torch-lit grids", 			FALSE }, /* 39 */
{ "dungeon_align",		"Generate dungeons with aligned rooms", 		TRUE }, /* 40 */ /* relocate? */
{ "dungeon_stair",		"Generate dungeons with connected stairs", 		TRUE }, /* 41 */ /* relocate? */
{ "flow_by_sound",		"Monsters chase current location (v.slow)", 	FALSE }, /* 42 */ /* relocate? */
{ "flow_by_smell",		"Monsters chase recent locations (v.slow)", 	FALSE }, /* 43 */ /* relocate? */
{ NULL,                  NULL,                                          FALSE }, /* 44 */
{ NULL,                  NULL,                                          FALSE }, /* 45 */
{ "smart_learn",		"Monsters learn from their mistakes", 			FALSE },	/* 46 */ /* relocate? */
{ "smart_cheat",		"Monsters exploit players weaknesses", 			FALSE },	/* 47 */ /* relocate? */
{ "view_reduce_lite",	"Reduce lite-radius when running", 				FALSE }, /* 48 */ /* low-end */
{ "hidden_player",		"Hide player symbol when running", 				FALSE }, /* 49 */ /* low-end */
{ "avoid_abort",		"Avoid checking for user abort", 				FALSE }, /* 50 */ /* low-end */
{ "avoid_other",		"Avoid processing special colors", 				FALSE }, /* 51 */ /* low-end */
{ "flush_failure",		"Flush input on various failures", 				TRUE }, /* 52 */
{ "flush_disturb",		"Flush input whenever disturbed", 				FALSE },	/* 53 */
{ NULL,                  NULL,                                          FALSE }, /* 54 */
{ NULL,                  NULL,                                          FALSE }, /* 55 */
{ NULL,                  NULL,                                          FALSE }, /* 56 */
{ NULL,                  NULL,                                          FALSE }, /* 57 */
{ "compress_savefile",	"Compress messages in savefiles", 				TRUE }, /* 58 */
{ "hilite_player",		"Hilite the player with the cursor", 			FALSE }, /* 59 */
{ "view_yellow_lite",	"Use special colors for torch lite", 			FALSE }, /* 60 */
{ "view_bright_lite",	"Use special colors for field of view", 		FALSE },  /* 61 */
{ "view_granite_lite",	"Use special colors for wall grids", 			FALSE }, /* 62 */
{ "view_special_lite",	"Use special colors for floor grids", 			FALSE }, /* 63 */
{ "easy_open",			"Open/Disarm/Close without direction", 			FALSE }, /* 64 */
{ "easy_alter",			"Open/Disarm doors/traps on movement", 			FALSE }, /* 65 */
{ "easy_floor",			"Display floor stacks in a list", 				FALSE }, /* 66 */
{ "show_piles",			"Show stacks using special attr/char", 			FALSE }, /* 67 */
{ "center_player",		"Center map continuously (very slow)", 			FALSE }, /* 68 */
{ "run_avoid_center",	"Avoid centering while running", 				FALSE }, /* 69 */
{ NULL,                  NULL,                                          FALSE }, /* 70 */
{ "auto_more",			"Automatically clear '-more-' prompts", 		FALSE }, /* 71 */
{ "smart_monsters",		"Monsters behave more intelligently", 			FALSE }, /* 72 */
{ "smart_packs",		"Monsters act smarter in groups (v.slow)", 		FALSE }, /* 73 */
{ NULL,                  NULL,                                          FALSE }, /* 74 */
{ NULL,                  NULL,                                          FALSE }, /* 75 */
{ NULL,                  NULL,                                          FALSE }, /* 76 */
{ NULL,                  NULL,                                          FALSE }, /* 77 */
{ NULL,                  NULL,                                          FALSE }, /* 78 */
{ NULL,                  NULL,                                          FALSE }, /* 79 */
{ NULL,                  NULL,                                          FALSE }, /* 80 */
{ NULL,                  NULL,                                          FALSE }, /* 81 */
{ NULL,                  NULL,                                          FALSE }, /* 82 */
{ NULL,                  NULL,                                          FALSE }, /* 83 */
{ NULL,                  NULL,                                          FALSE }, /* 84 */
{ NULL,                  NULL,                                          FALSE }, /* 85 */
{ NULL,                  NULL,                                          FALSE }, /* 86 */
{ NULL,                  NULL,                                          FALSE }, /* 87 */
{ NULL,                  NULL,                                          FALSE }, /* 88 */
{ NULL,                  NULL,                                          FALSE }, /* 89 */
{ NULL,                  NULL,                                          FALSE }, /* 90 */
{ NULL,                  NULL,                                          FALSE }, /* 91 */
{ NULL,                  NULL,                                          FALSE }, /* 92 */
{ NULL,                  NULL,                                          FALSE }, /* 93 */
{ NULL,                  NULL,                                          FALSE }, /* 94 */
{ NULL,                  NULL,                                          FALSE }, /* 95 */
{ NULL,                  NULL,                                          FALSE }, /* 96 */
{ NULL,                  NULL,                                          FALSE }, /* 97 */
{ NULL,                  NULL,                                          FALSE }, /* 98 */
{ NULL,                  NULL,                                          FALSE }, /* 99 */
{ NULL,                  NULL,                                          FALSE }, /* 100 */
{ NULL,                  NULL,                                          FALSE }, /* 101 */
{ NULL,                  NULL,                                          FALSE }, /* 102 */
{ NULL,                  NULL,                                          FALSE }, /* 103 */
{ NULL,                  NULL,                                          FALSE }, /* 104 */
{ NULL,                  NULL,                                          FALSE }, /* 105 */
{ NULL,                  NULL,                                          FALSE }, /* 106 */
{ NULL,                  NULL,                                          FALSE }, /* 107 */
{ NULL,                  NULL,                                          FALSE }, /* 108 */
{ NULL,                  NULL,                                          FALSE }, /* 109 */
{ NULL,                  NULL,                                          FALSE }, /* 110 */
{ NULL,                  NULL,                                          FALSE }, /* 111 */
{ NULL,                  NULL,                                          FALSE }, /* 112 */
{ NULL,                  NULL,                                          FALSE }, /* 113 */
{ NULL,                  NULL,                                          FALSE }, /* 114 */
{ NULL,                  NULL,                                          FALSE }, /* 115 */
{ NULL,                  NULL,                                          FALSE }, /* 116 */
{ NULL,                  NULL,                                          FALSE }, /* 117 */
{ NULL,                  NULL,                                          FALSE }, /* 118 */
{ NULL,                  NULL,                                          FALSE }, /* 119 */
{ NULL,                  NULL,                                          FALSE }, /* 120 */
{ NULL,                  NULL,                                          FALSE }, /* 121 */
{ NULL,                  NULL,                                          FALSE }, /* 122 */
{ NULL,                  NULL,                                          FALSE }, /* 123 */
{ NULL,                  NULL,                                          FALSE }, /* 124 */
{ NULL,                  NULL,                                          FALSE }, /* 125 */
{ NULL,                  NULL,                                          FALSE }, /* 126 */
{ NULL,                  NULL,                                          FALSE }, /* 127 */
{ "birth_point_based",	"Birth: Allow purchase of stats using points", 	FALSE }, 	/* 128 */
{ "birth_auto_roller",	"Birth: Allow specification of minimal stats", 	FALSE }, 	/* 129 */
{ "birth_maximize",		"Birth: Maximize effect of race/class bonuses",	TRUE }, 	/* 130 */
{ "birth_preserve",		"Birth: Preserve artifacts when leaving level",	TRUE }, 	/* 131 */
{ "birth_ironman",		"Birth: Restrict the use of stairs/recall", 	FALSE }, 	/* 132 */
{ "birth_no_stores",	"Birth: Restrict the use of stores/home", 		FALSE }, 	/* 133 */
{ "birth_no_artifacts",	"Birth: Restrict creation of artifacts", 		FALSE },	/* 134 */
{ "birth_rand_artifacts",	"Birth: Randomize some of the artifacts (beta)", FALSE }, /* 135 */
{ "birth_no_stacking",		"Birth: Don't stack objects on the floor", 	FALSE },	/* 136 */
{ NULL,                  NULL,                                          FALSE }, /* 137 */
{ NULL,                  NULL,                                          FALSE }, /* 138 */
{ NULL,                  NULL,                                          FALSE }, /* 139 */
{ NULL,                  NULL,                                          FALSE }, /* 140 */
{ NULL,                  NULL,                                          FALSE }, /* 141 */
{ NULL,                  NULL,                                          FALSE }, /* 142 */
{ NULL,                  NULL,                                          FALSE }, /* 143 */
{ NULL,                  NULL,                                          FALSE }, /* 144 */
{ NULL,                  NULL,                                          FALSE }, /* 145 */
{ NULL,                  NULL,                                          FALSE }, /* 146 */
{ NULL,                  NULL,                                          FALSE }, /* 147 */
{ NULL,                  NULL,                                          FALSE }, /* 148 */
{ NULL,                  NULL,                                          FALSE }, /* 149 */
{ NULL,                  NULL,                                          FALSE }, /* 150 */
{ NULL,                  NULL,                                          FALSE }, /* 151 */
{ NULL,                  NULL,                                          FALSE }, /* 152 */
{ NULL,                  NULL,                                          FALSE }, /* 153 */
{ NULL,                  NULL,                                          FALSE }, /* 154 */
{ NULL,                  NULL,                                          FALSE }, /* 155 */
{ NULL,                  NULL,                                          FALSE }, /* 156 */
{ NULL,                  NULL,                                          FALSE }, /* 157 */
{ NULL,                  NULL,                                          FALSE }, /* 158 */
{ NULL,                  NULL,                                          FALSE }, /* 159 */
{ "cheat_peek",          "Cheat: Peek into object creation",            FALSE }, /* 160 */
{ "cheat_hear",          "Cheat: Peek into monster creation",           FALSE }, /* 161 */
{ "cheat_room",          "Cheat: Peek into dungeon creation",           FALSE }, /* 162 */
{ "cheat_xtra",          "Cheat: Peek into something else",             FALSE }, /* 163 */
{ "cheat_know",          "Cheat: Know complete monster info",           FALSE }, /* 164 */
{ "cheat_live",          "Cheat: Allow player to avoid death",          FALSE }, /* 165 */
{ NULL,                  NULL,                                          FALSE }, /* 166 */
{ NULL,                  NULL,                                          FALSE }, /* 167 */
{ NULL,                  NULL,                                          FALSE }, /* 168 */
{ NULL,                  NULL,                                          FALSE }, /* 169 */
{ NULL,                  NULL,                                          FALSE }, /* 170 */
{ NULL,                  NULL,                                          FALSE }, /* 171 */
{ NULL,                  NULL,                                          FALSE }, /* 172 */
{ NULL,                  NULL,                                          FALSE }, /* 173 */
{ NULL,                  NULL,                                          FALSE }, /* 174 */
{ NULL,                  NULL,                                          FALSE }, /* 175 */
{ NULL,                  NULL,                                          FALSE }, /* 176 */
{ NULL,                  NULL,                                          FALSE }, /* 177 */
{ NULL,                  NULL,                                          FALSE }, /* 178 */
{ NULL,                  NULL,                                          FALSE }, /* 179 */
{ NULL,                  NULL,                                          FALSE }, /* 180 */
{ NULL,                  NULL,                                          FALSE }, /* 181 */
{ NULL,                  NULL,                                          FALSE }, /* 182 */
{ NULL,                  NULL,                                          FALSE }, /* 183 */
{ NULL,                  NULL,                                          FALSE }, /* 184 */
{ NULL,                  NULL,                                          FALSE }, /* 185 */
{ NULL,                  NULL,                                          FALSE }, /* 186 */
{ NULL,                  NULL,                                          FALSE }, /* 187 */
{ NULL,                  NULL,                                          FALSE }, /* 188 */
{ NULL,                  NULL,                                          FALSE }, /* 189 */
{ NULL,                  NULL,                                          FALSE }, /* 190 */
{ NULL,                  NULL,                                          FALSE }, /* 191 */
{ "adult_point_based",   "Adult: Allow purchase of stats using points", FALSE},		/* 192 */
{ "adult_auto_roller",   "Adult: Allow specification of minimal stats", FALSE},		/* 193 */
{ "adult_maximize",      "Adult: Maximize effect of race/class bonuses", TRUE},			/* 194 */
{ "adult_preserve",      "Adult: Preserve artifacts when leaving level", TRUE},			/* 195 */
{ "adult_ironman",       "Adult: Restrict the use of stairs/recall", 	FALSE},			/* 196 */
{ "adult_no_stores",     "Adult: Restrict the use of stores/home", 		FALSE},			/* 197 */
{ "adult_no_artifacts",  "Adult: Restrict creation of artifacts", 		FALSE},		/* 198 */
{ "adult_rand_artifacts","Adult: Randomize some of the artifacts (beta)", FALSE},		/* 199 */
{ "adult_no_stacking", 	 "Adult: Don't stack objects on the floor", 	FALSE},		/* 200 */
{ NULL,                  NULL,                                          FALSE }, /* 201 */
{ NULL,                  NULL,                                          FALSE }, /* 202 */
{ NULL,                  NULL,                                          FALSE }, /* 203 */
{ NULL,                  NULL,                                          FALSE }, /* 204 */
{ NULL,                  NULL,                                          FALSE }, /* 205 */
{ NULL,                  NULL,                                          FALSE }, /* 206 */
{ NULL,                  NULL,                                          FALSE }, /* 207 */
{ NULL,                  NULL,                                          FALSE }, /* 208 */
{ NULL,                  NULL,                                          FALSE }, /* 209 */
{ NULL,                  NULL,                                          FALSE }, /* 210 */
{ NULL,                  NULL,                                          FALSE }, /* 211 */
{ NULL,                  NULL,                                          FALSE }, /* 212 */
{ NULL,                  NULL,                                          FALSE }, /* 213 */
{ NULL,                  NULL,                                          FALSE }, /* 214 */
{ NULL,                  NULL,                                          FALSE }, /* 215 */
{ NULL,                  NULL,                                          FALSE }, /* 216 */
{ NULL,                  NULL,                                          FALSE }, /* 217 */
{ NULL,                  NULL,                                          FALSE }, /* 218 */
{ NULL,                  NULL,                                          FALSE }, /* 219 */
{ NULL,                  NULL,                                          FALSE }, /* 220 */
{ NULL,                  NULL,                                          FALSE }, /* 221 */
{ NULL,                  NULL,                                          FALSE }, /* 222 */
{ NULL,                  NULL,                                          FALSE }, /* 223 */
{ "score_peek",          "Score: Peek into object creation",            FALSE }, /* 224 */
{ "score_hear",          "Score: Peek into monster creation",           FALSE }, /* 225 */
{ "score_room",          "Score: Peek into dungeon creation",           FALSE }, /* 226 */
{ "score_xtra",          "Score: Peek into something else",             FALSE }, /* 227 */
{ "score_know",          "Score: Know complete monster info",           FALSE }, /* 228 */
{ "score_live",          "Score: Allow player to avoid death",          FALSE }, /* 229 */
{ NULL,                  NULL,                                          FALSE }, /* 230 */
{ NULL,                  NULL,                                          FALSE }, /* 231 */
{ NULL,                  NULL,                                          FALSE }, /* 232 */
{ NULL,                  NULL,                                          FALSE }, /* 233 */
{ NULL,                  NULL,                                          FALSE }, /* 234 */
{ NULL,                  NULL,                                          FALSE }, /* 235 */
{ NULL,                  NULL,                                          FALSE }, /* 236 */
{ NULL,                  NULL,                                          FALSE }, /* 237 */
{ NULL,                  NULL,                                          FALSE }, /* 238 */
{ NULL,                  NULL,                                          FALSE }, /* 239 */
{ NULL,                  NULL,                                          FALSE }, /* 240 */
{ NULL,                  NULL,                                          FALSE }, /* 241 */
{ NULL,                  NULL,                                          FALSE }, /* 242 */
{ NULL,                  NULL,                                          FALSE }, /* 243 */
{ NULL,                  NULL,                                          FALSE }, /* 244 */
{ NULL,                  NULL,                                          FALSE }, /* 245 */
{ NULL,                  NULL,                                          FALSE }, /* 246 */
{ NULL,                  NULL,                                          FALSE }, /* 247 */
{ NULL,                  NULL,                                          FALSE }, /* 248 */
{ NULL,                  NULL,                                          FALSE }, /* 249 */
{ NULL,                  NULL,                                          FALSE }, /* 250 */
{ NULL,                  NULL,                                          FALSE }, /* 251 */
{ NULL,                  NULL,                                          FALSE }, /* 252 */
{ NULL,                  NULL,                                          FALSE }, /* 253 */
{ NULL,                  NULL,                                          FALSE }, /* 254 */
{ NULL,                  NULL,                                          FALSE }, /* 255 */
};

#if 0
/*
 * Option indexes (offsets)
 *
 * These values are hard-coded by savefiles (and various pieces of code).
 */
#define OPT_BIRTH					128
#define OPT_CHEAT					160
#define OPT_ADULT					192
#define OPT_SCORE					224

#define OPT_NONE					255
#define OPT_MAX						256


/*
 * Option indexes (normal)
 *
 * These values are hard-coded by savefiles.
 */
/* xxx */
/* xxx */
/* xxx */
/* xxx */
/* xxx */
/* xxx */
/* xxx xxx */

#endif


/* Accessor functions */
const char *option_name(int opt)
{
	if (opt >= OPT_MAX) return NULL;
	return options[opt].name;
}

const char *option_desc(int opt)
{
	if (opt >= OPT_MAX) return NULL;
	return options[opt].description;
}

/* Setup functions */
void option_set(int opt, bool on)
{
	op_ptr->opt[opt] = on;
}

void option_set_defaults(void)
{
	size_t opt;

	for (opt = 0; opt < OPT_MAX; opt++)
		op_ptr->opt[opt] = options[opt].normal;
}
