/*
 * File: init.c
 * Purpose: Various game initialisation routines
 *
 * Copyright (c) 1997 Ben Harrison
 * Copyright (c) 2018 MAngband and PWMAngband Developers
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


#include "s-angband.h"


/*
 * This file is used to initialize various variables and arrays for the
 * Angband game.
 *
 * Several of the arrays for Angband are built from data files in the
 * "lib/gamedata" directory.
 */


/*
 * Server options, set in mangband.cfg
 */
bool cfg_report_to_meta = false;
bool cfg_mang_meta = true;
char *cfg_meta_address = NULL;
s32b cfg_meta_port = 8800;
char *cfg_bind_name = NULL;
char *cfg_report_address = NULL;
char *cfg_console_password = NULL;
char *cfg_dungeon_master = NULL;
bool cfg_secret_dungeon_master = true;
bool cfg_no_steal = true;
bool cfg_newbies_cannot_drop = true;
s32b cfg_level_unstatic_chance = 60;
bool cfg_random_artifacts = false;
s32b cfg_retire_timer = -1;
s16b cfg_limit_stairs = 0;
bool cfg_diving_mode = 0;
bool cfg_no_ghost = false;
bool cfg_more_towns = false;
bool cfg_artifact_drop_shallow = true;
bool cfg_limit_player_connections = true;
s16b cfg_max_townies = -1;
s16b cfg_max_trees = -1;
s32b cfg_tcp_port = 18346;
bool cfg_chardump_color = false;
s16b cfg_pvp_hostility = PVP_SAFE;
bool cfg_base_monsters = true;
bool cfg_extra_monsters = false;
bool cfg_ghost_diving = false;
bool cfg_console_local_only = false;
char *cfg_load_pref_file = NULL;
s16b cfg_preserve_artifacts = 3;
bool cfg_safe_recharge = false;
s16b cfg_party_sharelevel = -1;
bool cfg_turn_based = false;
bool cfg_limited_esp = false;
bool cfg_double_purse = false;
bool cfg_ai_learn = false;


static const char *slots[] =
{
    #define EQUIP(a, b, c, d, e, f) #a,
    #include "../common/list-equip-slots.h"
    #undef EQUIP
    NULL
};


const char *list_obj_flag_names[] =
{
    #define OF(a) #a,
    #include "../common/list-object-flags.h"
    #undef OF
    NULL
};


const char *list_element_names[] =
{
    #define ELEM(a) #a,
    #include "../common/list-elements.h"
    #undef ELEM
    NULL
};


static const char *effect_list[] = {
    NULL,
    #define EFFECT(x, a, b, c, d, e) #x,
    #include "list-effects.h"
    #undef EFFECT
    NULL
};


static const char *trap_flags[] =
{
    #define TRF(a, b) #a,
    #include "../common/list-trap-flags.h"
    #undef TRF
    NULL
};


static const char *terrain_flags[] =
{
    #define TF(a, b) #a,
    #include "list-terrain-flags.h"
    #undef TF
    NULL
};


static const char *player_info_flags[] =
{
    #define PF(a, b, c) #a,
    #include "../common/list-player-flags.h"
    #undef PF
    NULL
};


errr grab_effect_data(struct parser *p, struct effect *effect)
{
    const char *type;
    int val;

    if (grab_name("effect", parser_getsym(p, "eff"), effect_list, N_ELEMENTS(effect_list), &val))
        return PARSE_ERROR_INVALID_EFFECT;
    effect->index = val;

    if (parser_hasval(p, "type"))
    {
        type = parser_getsym(p, "type");

        if (type == NULL) return PARSE_ERROR_UNRECOGNISED_PARAMETER;

        /* Check for a value */
        val = effect_param(effect->index, type);
        if (val < 0) return PARSE_ERROR_INVALID_VALUE;
        effect->params[0] = val;
    }

    if (parser_hasval(p, "xtra"))
        effect->params[1] = parser_getint(p, "xtra");

    return PARSE_ERROR_NONE;
}


/* Free the sub-paths */
static void free_file_paths(void)
{
    string_free(ANGBAND_DIR_GAMEDATA);
    string_free(ANGBAND_DIR_CUSTOMIZE);
    string_free(ANGBAND_DIR_HELP);
    string_free(ANGBAND_DIR_SCREENS);
    string_free(ANGBAND_DIR_TILES);
    string_free(ANGBAND_DIR_USER);
    string_free(ANGBAND_DIR_SAVE);
    string_free(ANGBAND_DIR_SCORES);
}


/*
 * Find the default paths to all of our important sub-directories.
 *
 * All of the sub-directories should, by default, be located inside
 * the main directory, whose location is very system dependent. (On multi-
 * user systems such as Linux this is not the default - see config.h for
 * more info.)
 *
 * This function takes a writable buffer, initially containing the
 * "path" to the "config", "lib" and "data" directories, for example,
 * "/etc/angband/", "/usr/share/angband" and "/var/games/angband" -
 * or a system dependent string, for example, ":lib:".  The buffer
 * must be large enough to contain at least 32 more characters.
 *
 * Various command line options may allow some of the important
 * directories to be changed to user-specified directories, most
 * importantly, the "scores" and "user" and "save" directories,
 * but this is done after this function, see "main.c".
 *
 * In general, the initial path should end in the appropriate "PATH_SEP"
 * string.  All of the "sub-directory" paths (created below or supplied
 * by the user) will NOT end in the "PATH_SEP" string, see the special
 * "path_build()" function in "util.c" for more information.
 *
 * Hack -- first we free all the strings, since this is known
 * to succeed even if the strings have not been allocated yet,
 * as long as the variables start out as "NULL".  This allows
 * this function to be called multiple times, for example, to
 * try several base "path" values until a good one is found.
 */
void init_file_paths(const char *configpath, const char *libpath, const char *datapath)
{
    char buf[MSG_LEN];
    char *userpath = NULL;

    /*** Free everything ***/

    /* Free the sub-paths */
    free_file_paths();

    /*** Prepare the paths ***/

#define BUILD_DIRECTORY_PATH(dest, basepath, dirname) \
    path_build(buf, sizeof(buf), (basepath), (dirname)); \
    dest = string_make(buf);

    /* Paths generally containing configuration data for Angband. */
    BUILD_DIRECTORY_PATH(ANGBAND_DIR_GAMEDATA, configpath, "gamedata");
    BUILD_DIRECTORY_PATH(ANGBAND_DIR_CUSTOMIZE, configpath, "customize");
    BUILD_DIRECTORY_PATH(ANGBAND_DIR_HELP, libpath, "help");
    BUILD_DIRECTORY_PATH(ANGBAND_DIR_SCREENS, libpath, "screens");
    BUILD_DIRECTORY_PATH(ANGBAND_DIR_TILES, libpath, "tiles");

    /* Build the path to the user specific directory */
    BUILD_DIRECTORY_PATH(ANGBAND_DIR_USER, datapath, "user");

#ifdef USE_PRIVATE_PATHS
    userpath = ANGBAND_DIR_USER;
#else /* !USE_PRIVATE_PATHS */
    userpath = (char *)datapath;
#endif /* USE_PRIVATE_PATHS */

    /* Build the path to the score and save directories */
    BUILD_DIRECTORY_PATH(ANGBAND_DIR_SCORES, userpath, "scores");
    BUILD_DIRECTORY_PATH(ANGBAND_DIR_SAVE, userpath, "save");

#undef BUILD_DIRECTORY_PATH
}


/*
 * Create any missing directories.
 *
 * We create only those dirs which may be empty. The others are assumed
 * to contain required files and therefore must exist at startup.
 */
void create_needed_dirs(void)
{
    char dirpath[MSG_LEN];

    path_build(dirpath, sizeof(dirpath), ANGBAND_DIR_SCORES, "");
    if (!dir_create(dirpath)) quit_fmt("Cannot create '%s'", dirpath);
}


/*
 * Initialize game constants
 */


static enum parser_error parse_constants_level_max(struct parser *p)
{
    struct angband_constants *z;
    const char *label;
    int value;

    z = parser_priv(p);
    label = parser_getsym(p, "label");
    value = parser_getint(p, "value");

    if (value < 0) return PARSE_ERROR_INVALID_VALUE;

    if (streq(label, "monsters"))
        z->level_monster_max = value;
    else
        return PARSE_ERROR_UNDEFINED_DIRECTIVE;

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_constants_mon_gen(struct parser *p)
{
    struct angband_constants *z;
    const char *label;
    int value;

    z = parser_priv(p);
    label = parser_getsym(p, "label");
    value = parser_getint(p, "value");

    if (value < 0) return PARSE_ERROR_INVALID_VALUE;

    if (streq(label, "chance"))
        z->alloc_monster_chance = value;
    else if (streq(label, "level-min"))
        z->level_monster_min = value;
    else if (streq(label, "town-day"))
        z->town_monsters_day = value;
    else if (streq(label, "town-night"))
        z->town_monsters_night = value;
    else if (streq(label, "repro-max"))
        z->repro_monster_max = value;
    else if (streq(label, "ood-chance"))
        z->ood_monster_chance = value;
    else if (streq(label, "ood-amount"))
        z->ood_monster_amount = value;
    else
        return PARSE_ERROR_UNDEFINED_DIRECTIVE;

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_constants_mon_play(struct parser *p)
{
    struct angband_constants *z;
    const char *label;
    int value;

    z = parser_priv(p);
    label = parser_getsym(p, "label");
    value = parser_getint(p, "value");

    if (value < 0) return PARSE_ERROR_INVALID_VALUE;

    if (streq(label, "break-glyph"))
        z->glyph_hardness = value;
    else if (streq(label, "mult-rate"))
        z->repro_monster_rate = value;
    else if (streq(label, "life-drain"))
        z->life_drain_percent = value;
    else if (streq(label, "flee-range"))
        z->flee_range = value;
    else if (streq(label, "turn-range"))
        z->turn_range = value;
    else
        return PARSE_ERROR_UNDEFINED_DIRECTIVE;

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_constants_dun_gen(struct parser *p)
{
    struct angband_constants *z;
    const char *label;
    int value;

    z = parser_priv(p);
    label = parser_getsym(p, "label");
    value = parser_getint(p, "value");

    if (value < 0) return PARSE_ERROR_INVALID_VALUE;

    if (streq(label, "cent-max"))
        z->level_room_max = value;
    else if (streq(label, "door-max"))
        z->level_door_max = value;
    else if (streq(label, "wall-max"))
        z->wall_pierce_max = value;
    else if (streq(label, "tunn-max"))
        z->tunn_grid_max = value;
    else if (streq(label, "amt-room"))
        z->room_item_av = value;
    else if (streq(label, "amt-item"))
        z->both_item_av = value;
    else if (streq(label, "amt-gold"))
        z->both_gold_av = value;
    else if (streq(label, "pit-max"))
        z->level_pit_max = value;
    else
        return PARSE_ERROR_UNDEFINED_DIRECTIVE;

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_constants_world(struct parser *p)
{
    struct angband_constants *z;
    const char *label;
    int value;

    z = parser_priv(p);
    label = parser_getsym(p, "label");
    value = parser_getint(p, "value");

    if (value < 0) return PARSE_ERROR_INVALID_VALUE;

    if (streq(label, "max-depth"))
        z->max_depth = value;
    else if (streq(label, "day-length"))
        z->day_length = value;
    else if (streq(label, "dungeon-hgt"))
        z->dungeon_hgt = value;
    else if (streq(label, "dungeon-wid"))
        z->dungeon_wid = value;
    else if (streq(label, "town-hgt"))
        z->town_hgt = value;
    else if (streq(label, "town-wid"))
        z->town_wid = value;
    else if (streq(label, "feeling-total"))
        z->feeling_total = value;
    else if (streq(label, "feeling-need"))
        z->feeling_need = value;
    else if (streq(label, "stair-skip"))
        z->stair_skip = value;
    else if (streq(label, "move-energy"))
        z->move_energy = value;
    else
        return PARSE_ERROR_UNDEFINED_DIRECTIVE;

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_constants_carry_cap(struct parser *p)
{
    struct angband_constants *z;
    const char *label;
    int value;

    z = parser_priv(p);
    label = parser_getsym(p, "label");
    value = parser_getint(p, "value");

    if (value < 0) return PARSE_ERROR_INVALID_VALUE;

    if (streq(label, "pack-size"))
        z->pack_size = value;
    else if (streq(label, "quiver-size"))
        z->quiver_size = value;
    else if (streq(label, "quiver-slot-size"))
        z->quiver_slot_size = value;
    else if (streq(label, "floor-size"))
        z->floor_size = value;
    else
        return PARSE_ERROR_UNDEFINED_DIRECTIVE;

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_constants_store(struct parser *p)
{
    struct angband_constants *z;
    const char *label;
    int value;

    z = parser_priv(p);
    label = parser_getsym(p, "label");
    value = parser_getint(p, "value");

    if (value < 0) return PARSE_ERROR_INVALID_VALUE;

    if (streq(label, "inven-max"))
        z->store_inven_max = value;
    else if (streq(label, "turns"))
        z->store_turns = value;
    else if (streq(label, "shuffle"))
        z->store_shuffle = value;
    else if (streq(label, "magic-level"))
        z->store_magic_level = value;
    else
        return PARSE_ERROR_UNDEFINED_DIRECTIVE;

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_constants_obj_make(struct parser *p)
{
    struct angband_constants *z;
    const char *label;
    int value;

    z = parser_priv(p);
    label = parser_getsym(p, "label");
    value = parser_getint(p, "value");

    if (value < 0) return PARSE_ERROR_INVALID_VALUE;

    if (streq(label, "max-depth"))
        z->max_obj_depth = value;
    else if (streq(label, "great-obj"))
        z->great_obj = value;
    else if (streq(label, "great-ego"))
        z->great_ego = value;
    else if (streq(label, "fuel-torch"))
        z->fuel_torch = value;
    else if (streq(label, "fuel-lamp"))
        z->fuel_lamp = value;
    else if (streq(label, "default-lamp"))
        z->default_lamp = value;
    else
        return PARSE_ERROR_UNDEFINED_DIRECTIVE;

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_constants_player(struct parser *p)
{
    struct angband_constants *z;
    const char *label;
    int value;

    z = parser_priv(p);
    label = parser_getsym(p, "label");
    value = parser_getint(p, "value");

    if (value < 0) return PARSE_ERROR_INVALID_VALUE;

    if (streq(label, "max-sight"))
        z->max_sight = value;
    else if (streq(label, "max-range"))
        z->max_range = value;
    else if (streq(label, "start-gold"))
        z->start_gold = value;
    else
        return PARSE_ERROR_UNDEFINED_DIRECTIVE;

    return PARSE_ERROR_NONE;
}


static struct parser *init_parse_constants(void)
{
    struct angband_constants *z = mem_zalloc(sizeof(*z));
    struct parser *p = parser_new();

    parser_setpriv(p, z);
    parser_reg(p, "level-max sym label int value", parse_constants_level_max);
    parser_reg(p, "mon-gen sym label int value", parse_constants_mon_gen);
    parser_reg(p, "mon-play sym label int value", parse_constants_mon_play);
    parser_reg(p, "dun-gen sym label int value", parse_constants_dun_gen);
    parser_reg(p, "world sym label int value", parse_constants_world);
    parser_reg(p, "carry-cap sym label int value", parse_constants_carry_cap);
    parser_reg(p, "store sym label int value", parse_constants_store);
    parser_reg(p, "obj-make sym label int value", parse_constants_obj_make);
    parser_reg(p, "player sym label int value", parse_constants_player);

    return p;
}


static errr run_parse_constants(struct parser *p)
{
    return parse_file_quit_not_found(p, "constants");
}


static errr finish_parse_constants(struct parser *p)
{
    z_info = parser_priv(p);
    parser_destroy(p);

    return 0;
}


static void cleanup_constants(void)
{
    mem_free(z_info);
}


static struct file_parser constants_parser =
{
    "constants",
    init_parse_constants,
    run_parse_constants,
    finish_parse_constants,
    cleanup_constants
};


/*
 * Initialize game constants.
 *
 * Assumption: Paths are set up correctly before calling this function.
 */
static void init_game_constants(void)
{
    plog("Initializing constants...");
    if (run_parser(&constants_parser)) quit("Cannot initialize constants");
}


/*
 * Free the game constants
 */
static void cleanup_game_constants(void)
{
    cleanup_parser(&constants_parser);
}


/*
 * Initialize random names
 */


struct name
{
    struct name *next;
    char *str;
};


struct names_parse
{
    unsigned int section;
    unsigned int nnames[RANDNAME_NUM_TYPES];
    struct name *names[RANDNAME_NUM_TYPES];
};


static enum parser_error parse_names_section(struct parser *p)
{
    unsigned int section = parser_getint(p, "section");
    struct names_parse *s = parser_priv(p);

    if (s->section >= RANDNAME_NUM_TYPES) return PARSE_ERROR_OUT_OF_BOUNDS;
    s->section = section;

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_names_word(struct parser *p)
{
    const char *name = parser_getstr(p, "name");
    struct names_parse *s = parser_priv(p);
    struct name *ns = mem_zalloc(sizeof(*ns));

    s->nnames[s->section]++;
    ns->next = s->names[s->section];
    ns->str = string_make(name);
    s->names[s->section] = ns;

    return PARSE_ERROR_NONE;
}


static struct parser *init_parse_names(void)
{
    struct parser *p = parser_new();
    struct names_parse *n = mem_zalloc(sizeof(*n));

    n->section = 0;
    parser_setpriv(p, n);
    parser_reg(p, "section int section", parse_names_section);
    parser_reg(p, "word str name", parse_names_word);

    return p;
}


static errr run_parse_names(struct parser *p)
{
    return parse_file_quit_not_found(p, "names");
}


static errr finish_parse_names(struct parser *p)
{
    int i;
    unsigned int j;
    struct names_parse *n = parser_priv(p);
    struct name *nm;

    num_names = mem_zalloc(RANDNAME_NUM_TYPES * sizeof(u32b));
    name_sections = mem_zalloc(sizeof(char**) * RANDNAME_NUM_TYPES);
    for (i = 0; i < RANDNAME_NUM_TYPES; i++)
    {
        num_names[i] = n->nnames[i];
        name_sections[i] = mem_alloc(sizeof(char*) * (n->nnames[i] + 1));
        for (nm = n->names[i], j = 0; nm && j < n->nnames[i]; nm = nm->next, j++)
            name_sections[i][j] = nm->str;
        name_sections[i][n->nnames[i]] = NULL;
        while (n->names[i])
        {
            nm = n->names[i]->next;
            mem_free(n->names[i]);
            n->names[i] = nm;
        }
    }
    mem_free(n);
    parser_destroy(p);

    return 0;
}


static void cleanup_names(void)
{
    strings_free(name_sections, num_names, RANDNAME_NUM_TYPES);
}


static struct file_parser names_parser =
{
    "names",
    init_parse_names,
    run_parse_names,
    finish_parse_names,
    cleanup_names
};


/*
 * Initialize traps
 */


static enum parser_error parse_trap_name(struct parser *p)
{
    const char *name = parser_getsym(p, "name");
    const char *desc = parser_getstr(p, "desc");
    struct trap_kind *h = parser_priv(p);
    struct trap_kind *t = mem_zalloc(sizeof(*t));

    t->next = h;
    t->name = string_make(name);
    t->desc = string_make(desc);
    parser_setpriv(p, t);

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_trap_graphics(struct parser *p)
{
    char glyph = parser_getchar(p, "glyph");
    const char *color = parser_getsym(p, "color");
    int attr = 0;
    struct trap_kind *t = parser_priv(p);

    if (!t) return PARSE_ERROR_MISSING_RECORD_HEADER;
    t->d_char = glyph;
    if (strlen(color) > 1)
		attr = color_text_to_attr(color);
    else
		attr = color_char_to_attr(color[0]);
    if (attr < 0) return PARSE_ERROR_INVALID_COLOR;
    t->d_attr = attr;

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_trap_appear(struct parser *p)
{
    struct trap_kind *t = parser_priv(p);

    if (!t) return PARSE_ERROR_MISSING_RECORD_HEADER;
    t->rarity = parser_getuint(p, "rarity");
    t->min_depth = parser_getuint(p, "mindepth");
    t->max_num = parser_getuint(p, "maxnum");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_trap_visibility(struct parser *p)
{
    struct trap_kind *t = parser_priv(p);
    dice_t *dice = NULL;
    const char *string = NULL;

    if (!t) return PARSE_ERROR_MISSING_RECORD_HEADER;

    dice = dice_new();

    if (dice == NULL) return PARSE_ERROR_INVALID_DICE;

    string = parser_getstr(p, "visibility");

    if (!dice_parse_string(dice, string))
    {
        dice_free(dice);
        return PARSE_ERROR_INVALID_DICE;
    }

    dice_random_value(dice, NULL, &t->power);
    dice_free(dice);

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_trap_flags(struct parser *p)
{
    struct trap_kind *t = parser_priv(p);
    char *flags;
    char *s;

    if (!t) return PARSE_ERROR_MISSING_RECORD_HEADER;
    if (!parser_hasval(p, "flags")) return PARSE_ERROR_NONE;
    flags = string_make(parser_getstr(p, "flags"));

    s = strtok(flags, " |");
    while (s)
    {
		if (grab_flag(t->flags, TRF_SIZE, trap_flags, s))
        {
			string_free(s);
			return PARSE_ERROR_INVALID_FLAG;
		}
		s = strtok(NULL, " |");
    }

    string_free(flags);
    return PARSE_ERROR_NONE;
}


static enum parser_error parse_trap_effect(struct parser *p)
{
	struct trap_kind *t = parser_priv(p);
    struct effect *new_effect = mem_zalloc(sizeof(*new_effect));
    errr ret;

	if (!t) return PARSE_ERROR_MISSING_RECORD_HEADER;

    /* Fill in the detail */
    ret = grab_effect_data(p, new_effect);
    if (ret) return ret;

    new_effect->next = t->effect;
    t->effect = new_effect;

	return PARSE_ERROR_NONE;
}


static enum parser_error parse_trap_param(struct parser *p)
{
    struct trap_kind *t = parser_priv(p);

    if (!t) return PARSE_ERROR_MISSING_RECORD_HEADER;

    /* If there is no effect, assume that this is human and not parser error. */
    if (t->effect == NULL) return PARSE_ERROR_NONE;

    t->effect->params[1] = parser_getint(p, "p2");

    if (parser_hasval(p, "p3"))
        t->effect->params[2] = parser_getint(p, "p3");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_trap_dice(struct parser *p)
{
    struct trap_kind *t = parser_priv(p);
    dice_t *dice = NULL;
    const char *string = NULL;

    if (!t) return PARSE_ERROR_MISSING_RECORD_HEADER;

    /* If there is no effect, assume that this is human and not parser error. */
    if (t->effect == NULL) return PARSE_ERROR_NONE;

    dice = dice_new();

    if (dice == NULL) return PARSE_ERROR_INVALID_DICE;

    string = parser_getstr(p, "dice");

    if (dice_parse_string(dice, string))
        t->effect->dice = dice;
    else
    {
        dice_free(dice);
        return PARSE_ERROR_INVALID_DICE;
    }

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_trap_expr(struct parser *p)
{
    struct trap_kind *t = parser_priv(p);
    expression_t *expression = NULL;
    expression_base_value_f function = NULL;
    const char *name;
    const char *base;
    const char *expr;

    if (!t) return PARSE_ERROR_MISSING_RECORD_HEADER;

    /* If there is no effect, assume that this is human and not parser error. */
    if (t->effect == NULL) return PARSE_ERROR_NONE;

    /* If there are no dice, assume that this is human and not parser error. */
    if (t->effect->dice == NULL) return PARSE_ERROR_NONE;

    name = parser_getsym(p, "name");
    base = parser_getsym(p, "base");
    expr = parser_getstr(p, "expr");
    expression = expression_new();

    if (expression == NULL) return PARSE_ERROR_INVALID_EXPRESSION;

    function = spell_value_base_by_name(base);
    expression_set_base_value(expression, function);

    if (expression_add_operations_string(expression, expr) < 0)
        return PARSE_ERROR_BAD_EXPRESSION_STRING;

    if (dice_bind_expression(t->effect->dice, name, expression) < 0)
        return PARSE_ERROR_UNBOUND_EXPRESSION;

    /* The dice object makes a deep copy of the expression, so we can free it */
    expression_free(expression);

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_trap_effect_xtra(struct parser *p)
{
	struct trap_kind *t = parser_priv(p);
    struct effect *new_effect = mem_zalloc(sizeof(*new_effect));
    errr ret;

	if (!t) return PARSE_ERROR_MISSING_RECORD_HEADER;

    /* Fill in the detail */
    ret = grab_effect_data(p, new_effect);
    if (ret) return ret;

    new_effect->next = t->effect_xtra;
    t->effect_xtra = new_effect;

	return PARSE_ERROR_NONE;
}


static enum parser_error parse_trap_param_xtra(struct parser *p)
{
    struct trap_kind *t = parser_priv(p);

    if (!t) return PARSE_ERROR_MISSING_RECORD_HEADER;

    /* If there is no effect, assume that this is human and not parser error. */
    if (t->effect_xtra == NULL) return PARSE_ERROR_NONE;

    t->effect_xtra->params[1] = parser_getint(p, "p2");

    if (parser_hasval(p, "p3"))
        t->effect_xtra->params[2] = parser_getint(p, "p3");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_trap_dice_xtra(struct parser *p)
{
    struct trap_kind *t = parser_priv(p);
    dice_t *dice = NULL;
    const char *string = NULL;

    if (!t) return PARSE_ERROR_MISSING_RECORD_HEADER;

    /* If there is no effect, assume that this is human and not parser error. */
    if (t->effect_xtra == NULL) return PARSE_ERROR_NONE;

    dice = dice_new();

    if (dice == NULL) return PARSE_ERROR_INVALID_DICE;

    string = parser_getstr(p, "dice");

    if (dice_parse_string(dice, string))
        t->effect_xtra->dice = dice;
    else
    {
        dice_free(dice);
        return PARSE_ERROR_INVALID_DICE;
    }

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_trap_expr_xtra(struct parser *p)
{
    struct trap_kind *t = parser_priv(p);
    expression_t *expression = NULL;
    expression_base_value_f function = NULL;
    const char *name;
    const char *base;
    const char *expr;

    if (!t) return PARSE_ERROR_MISSING_RECORD_HEADER;

    /* If there is no effect, assume that this is human and not parser error. */
    if (t->effect_xtra == NULL) return PARSE_ERROR_NONE;

    /* If there are no dice, assume that this is human and not parser error. */
    if (t->effect_xtra->dice == NULL) return PARSE_ERROR_NONE;

    name = parser_getsym(p, "name");
    base = parser_getsym(p, "base");
    expr = parser_getstr(p, "expr");
    expression = expression_new();

    if (expression == NULL) return PARSE_ERROR_INVALID_EXPRESSION;

    function = spell_value_base_by_name(base);
    expression_set_base_value(expression, function);

    if (expression_add_operations_string(expression, expr) < 0)
        return PARSE_ERROR_BAD_EXPRESSION_STRING;

    if (dice_bind_expression(t->effect_xtra->dice, name, expression) < 0)
        return PARSE_ERROR_UNBOUND_EXPRESSION;

    /* The dice object makes a deep copy of the expression, so we can free it */
    expression_free(expression);

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_trap_save_flags(struct parser *p)
{
    struct trap_kind *t = parser_priv(p);
    char *s = string_make(parser_getstr(p, "flags"));
    char *u;

    my_assert(t);

    u = strtok(s, " |");
    while (u)
    {
        bool found = false;

        if (!grab_flag(t->save_flags, OF_SIZE, list_obj_flag_names, u)) found = true;
        if (!found) break;
        u = strtok(NULL, " |");
    }
    mem_free(s);

    return (u? PARSE_ERROR_INVALID_FLAG: PARSE_ERROR_NONE);
}


static enum parser_error parse_trap_desc(struct parser *p)
{
    struct trap_kind *t = parser_priv(p);

    my_assert(t);

    t->text = string_append(t->text, parser_getstr(p, "text"));

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_trap_msg(struct parser *p)
{
    struct trap_kind *t = parser_priv(p);

    my_assert(t);

    t->msg = string_append(t->msg, parser_getstr(p, "text"));

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_trap_msg_good(struct parser *p)
{
    struct trap_kind *t = parser_priv(p);

    my_assert(t);

    t->msg_good = string_append(t->msg_good, parser_getstr(p, "text"));

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_trap_msg_bad(struct parser *p)
{
    struct trap_kind *t = parser_priv(p);

    my_assert(t);

    t->msg_bad = string_append(t->msg_bad, parser_getstr(p, "text"));

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_trap_msg_xtra(struct parser *p)
{
    struct trap_kind *t = parser_priv(p);

    my_assert(t);

    t->msg_xtra = string_append(t->msg_xtra, parser_getstr(p, "text"));

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_trap_msg_death(struct parser *p)
{
    struct trap_kind *t = parser_priv(p);

    my_assert(t);

    t->msg_death_type = parser_getint(p, "type");
    t->msg_death = string_append(t->msg_death, parser_getstr(p, "text"));

    return PARSE_ERROR_NONE;
}


static struct parser *init_parse_trap(void)
{
    struct parser *p = parser_new();

    parser_setpriv(p, NULL);
    parser_reg(p, "name sym name str desc", parse_trap_name);
    parser_reg(p, "graphics char glyph sym color", parse_trap_graphics);
    parser_reg(p, "appear uint rarity uint mindepth uint maxnum", parse_trap_appear);
    parser_reg(p, "visibility str visibility", parse_trap_visibility);
    parser_reg(p, "flags ?str flags", parse_trap_flags);
	parser_reg(p, "effect sym eff ?sym type ?int xtra", parse_trap_effect);
    parser_reg(p, "param int p2 ?int p3", parse_trap_param);
    parser_reg(p, "dice str dice", parse_trap_dice);
    parser_reg(p, "expr sym name sym base str expr", parse_trap_expr);
    parser_reg(p, "effect-xtra sym eff ?sym type ?int xtra", parse_trap_effect_xtra);
    parser_reg(p, "param-xtra int p2 ?int p3", parse_trap_param_xtra);
    parser_reg(p, "dice-xtra str dice", parse_trap_dice_xtra);
    parser_reg(p, "expr-xtra sym name sym base str expr", parse_trap_expr_xtra);
    parser_reg(p, "save str flags", parse_trap_save_flags);
    parser_reg(p, "desc str text", parse_trap_desc);
    parser_reg(p, "msg str text", parse_trap_msg);
    parser_reg(p, "msg-good str text", parse_trap_msg_good);
    parser_reg(p, "msg-bad str text", parse_trap_msg_bad);
    parser_reg(p, "msg-xtra str text", parse_trap_msg_xtra);
    parser_reg(p, "msg-death int type str text", parse_trap_msg_death);

    return p;
}


static errr run_parse_trap(struct parser *p)
{
    return parse_file_quit_not_found(p, "trap");
}


static errr finish_parse_trap(struct parser *p)
{
	struct trap_kind *t, *n;
    int tidx;

	/* Scan the list for the max id */
	z_info->trap_max = 0;
	t = parser_priv(p);
	while (t)
    {
		z_info->trap_max++;
		t = t->next;
	}

	/* Allocate the direct access list and copy the data to it */
    trap_info = mem_zalloc(z_info->trap_max * sizeof(*t));
    tidx = z_info->trap_max - 1;
    for (t = parser_priv(p); t; t = n, tidx--)
    {
		memcpy(&trap_info[tidx], t, sizeof(*t));
        trap_info[tidx].tidx = tidx;
        n = t->next;
        if (tidx < z_info->trap_max - 1) trap_info[tidx].next = &trap_info[tidx + 1];
        else trap_info[tidx].next = NULL;
        mem_free(t);
    }

    parser_destroy(p);
    return 0;
}


static void cleanup_trap(void)
{
	int i;

    /* Paranoia */
    if (!trap_info) return;

	for (i = 0; i < z_info->trap_max; i++)
    {
		string_free(trap_info[i].name);
		string_free(trap_info[i].text);
        string_free(trap_info[i].desc);
        string_free(trap_info[i].msg);
        string_free(trap_info[i].msg_good);
        string_free(trap_info[i].msg_bad);
        string_free(trap_info[i].msg_xtra);
        string_free(trap_info[i].msg_death);
        free_effect(trap_info[i].effect);
        free_effect(trap_info[i].effect_xtra);
	}
	mem_free(trap_info);
}


static struct file_parser trap_parser =
{
    "trap",
    init_parse_trap,
    run_parse_trap,
    finish_parse_trap,
    cleanup_trap
};


/*
 * Initialize terrain
 */


static enum parser_error parse_feat_name(struct parser *p)
{
    const char *name = parser_getstr(p, "name");
    struct feature *h = parser_priv(p);
    struct feature *f = mem_zalloc(sizeof(*f));

    f->next = h;
    f->name = string_make(name);
    parser_setpriv(p, f);

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_feat_graphics(struct parser *p)
{
    char glyph = parser_getchar(p, "glyph");
    const char *color = parser_getsym(p, "color");
    int attr = 0;
    struct feature *f = parser_priv(p);

    if (!f) return PARSE_ERROR_MISSING_RECORD_HEADER;
    f->d_char = glyph;
    if (strlen(color) > 1)
        attr = color_text_to_attr(color);
    else
        attr = color_char_to_attr(color[0]);
    if (attr < 0) return PARSE_ERROR_INVALID_COLOR;
    f->d_attr = attr;

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_feat_mimic(struct parser *p)
{
    const char *mimic_feat = parser_getstr(p, "feat");
    struct feature *f = parser_priv(p);

    if (!f) return PARSE_ERROR_MISSING_RECORD_HEADER;
    f->mimic = string_make(mimic_feat);

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_feat_priority(struct parser *p)
{
    unsigned int priority = parser_getuint(p, "priority");
    struct feature *f = parser_priv(p);

    if (!f) return PARSE_ERROR_MISSING_RECORD_HEADER;
    f->priority = priority;

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_feat_flags(struct parser *p)
{
    struct feature *f = parser_priv(p);
    char *flags;
    char *s;

    if (!f) return PARSE_ERROR_MISSING_RECORD_HEADER;
    if (!parser_hasval(p, "flags")) return PARSE_ERROR_NONE;
    flags = string_make(parser_getstr(p, "flags"));

    s = strtok(flags, " |");
    while (s)
    {
        if (grab_flag(f->flags, TF_SIZE, terrain_flags, s))
        {
            string_free(flags);
            return PARSE_ERROR_INVALID_FLAG;
        }
        s = strtok(NULL, " |");
    }

    string_free(flags);
    return PARSE_ERROR_NONE;
}


static enum parser_error parse_feat_info(struct parser *p)
{
    struct feature *f = parser_priv(p);

    if (!f) return PARSE_ERROR_MISSING_RECORD_HEADER;
    f->shopnum = parser_getint(p, "shopnum");
    f->dig = parser_getint(p, "dig");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_feat_desc(struct parser *p)
{
    struct feature *f = parser_priv(p);

    if (!f) return PARSE_ERROR_MISSING_RECORD_HEADER;
    f->desc = string_append(f->desc, parser_getstr(p, "text"));

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_feat_hurt_msg(struct parser *p)
{
    struct feature *f = parser_priv(p);

    if (!f) return PARSE_ERROR_MISSING_RECORD_HEADER;
    f->hurt_msg = string_append(f->hurt_msg, parser_getstr(p, "text"));

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_feat_died_flavor(struct parser *p)
{
    struct feature *f = parser_priv(p);

    if (!f) return PARSE_ERROR_MISSING_RECORD_HEADER;
    f->died_flavor = string_append(f->died_flavor, parser_getstr(p, "text"));

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_feat_die_msg(struct parser *p)
{
    struct feature *f = parser_priv(p);

    if (!f) return PARSE_ERROR_MISSING_RECORD_HEADER;
    f->die_msg = string_append(f->die_msg, parser_getstr(p, "text"));

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_feat_resist_flag(struct parser *p)
{
    int flag;
    struct feature *f = parser_priv(p);

    if (!f) return PARSE_ERROR_MISSING_RECORD_HEADER;
    flag = lookup_flag(r_info_flags, parser_getsym(p, "flag"));
    if (flag == FLAG_END) return PARSE_ERROR_INVALID_FLAG;
    f->resist_flag = flag;

    return PARSE_ERROR_NONE;
}


static struct parser *init_parse_feat(void)
{
    struct parser *p = parser_new();

    parser_setpriv(p, NULL);
    parser_reg(p, "name str name", parse_feat_name);
    parser_reg(p, "graphics char glyph sym color", parse_feat_graphics);
    parser_reg(p, "mimic str feat", parse_feat_mimic);
    parser_reg(p, "priority uint priority", parse_feat_priority);
    parser_reg(p, "flags ?str flags", parse_feat_flags);
    parser_reg(p, "info int shopnum int dig", parse_feat_info);
    parser_reg(p, "desc str text", parse_feat_desc);
    parser_reg(p, "hurt-msg str text", parse_feat_hurt_msg);
    parser_reg(p, "died-flavor str text", parse_feat_died_flavor);
    parser_reg(p, "die-msg str text", parse_feat_die_msg);
    parser_reg(p, "resist-flag sym flag", parse_feat_resist_flag);

    return p;
}


static errr run_parse_feat(struct parser *p)
{
    return parse_file_quit_not_found(p, "terrain");
}


static errr finish_parse_feat(struct parser *p)
{
    struct feature *f, *n;
    int fidx;

    /* Scan the list for the max id */
    z_info->f_max = 0;
    f = parser_priv(p);
    while (f)
    {
        z_info->f_max++;
        f = f->next;
    }

    /* Allocate the direct access list and copy the data to it */
    f_info = mem_zalloc(z_info->f_max * sizeof(*f));
    fidx = z_info->f_max - 1;
    for (f = parser_priv(p); f; f = n, fidx--)
    {
        memcpy(&f_info[fidx], f, sizeof(*f));
        f_info[fidx].fidx = fidx;
        n = f->next;
        if (fidx < z_info->f_max - 1) f_info[fidx].next = &f_info[fidx + 1];
        else f_info[fidx].next = NULL;
        mem_free(f);
    }

    /* Set the terrain constants */
    set_terrain();

    parser_destroy(p);
    return 0;
}


static void cleanup_feat(void)
{
    int i;

    /* Paranoia */
    if (!f_info) return;

    for (i = 0; i < z_info->f_max; i++)
    {
        string_free(f_info[i].die_msg);
        string_free(f_info[i].died_flavor);
        string_free(f_info[i].hurt_msg);
        string_free(f_info[i].mimic);
        string_free(f_info[i].desc);
        string_free(f_info[i].name);
    }
    mem_free(f_info);
}


static struct file_parser feat_parser =
{
    "terrain",
    init_parse_feat,
    run_parse_feat,
    finish_parse_feat,
    cleanup_feat
};


/*
 * Initialize player bodies
 */


static enum parser_error parse_body_body(struct parser *p)
{
    struct player_body *h = parser_priv(p);
    struct player_body *b = mem_zalloc(sizeof(*b));

    b->next = h;
    b->name = string_make(parser_getstr(p, "name"));
    parser_setpriv(p, b);

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_body_slot(struct parser *p)
{
    struct player_body *b = parser_priv(p);
    struct equip_slot *slot = b->slots;
    char *slot_type;
    int n;

    if (!b) return PARSE_ERROR_MISSING_RECORD_HEADER;

    /* Go to the last valid slot, then allocate a new one */
    if (!slot)
    {
        b->slots = mem_zalloc(sizeof(struct equip_slot));
        slot = b->slots;
    }
    else
    {
        while (slot->next) slot = slot->next;
        slot->next = mem_zalloc(sizeof(struct equip_slot));
        slot = slot->next;
    }

    slot_type = string_make(parser_getsym(p, "slot"));
    n = lookup_flag(slots, slot_type);
    if (!n) return PARSE_ERROR_INVALID_FLAG;
    slot->type = n;
    slot->name = string_make(parser_getsym(p, "name"));
    b->count++;
    mem_free(slot_type);

    return PARSE_ERROR_NONE;
}


static struct parser *init_parse_body(void)
{
    struct parser *p = parser_new();

    parser_setpriv(p, NULL);
    parser_reg(p, "body str name", parse_body_body);
    parser_reg(p, "slot sym slot sym name", parse_body_slot);

    return p;
}


static errr run_parse_body(struct parser *p)
{
    return parse_file_quit_not_found(p, "body");
}


static errr finish_parse_body(struct parser *p)
{
    struct player_body *b;
    int i;

    /* Scan the list for the max slots */
    z_info->equip_slots_max = 0;
    bodies = parser_priv(p);
    for (b = bodies; b; b = b->next)
    {
        if (b->count > z_info->equip_slots_max)
            z_info->equip_slots_max = b->count;
    }

    /* Allocate the slot list and copy */
    for (b = bodies; b; b = b->next)
    {
        struct equip_slot *s_new, *s, *sn = NULL;

        s_new = mem_zalloc(z_info->equip_slots_max * sizeof(struct equip_slot));
        for (i = 0, s = b->slots; s; i++, s = sn)
        {
            memcpy(&s_new[i], s, sizeof(*s));
            s_new[i].next = NULL;
            sn = s->next;
            mem_free(s);
        }
        b->slots = s_new;
    }

    parser_destroy(p);
    return 0;
}


static struct file_parser body_parser =
{
    "body",
    init_parse_body,
    run_parse_body,
    finish_parse_body,
    cleanup_body
};


/*
 * Initialize player histories
 */


static struct history_chart *histories;


static struct history_chart *findchart(struct history_chart *hs, unsigned int idx)
{
    for (; hs; hs = hs->next)
    {
        if (hs->idx == idx) break;
    }
    return hs;
}


static enum parser_error parse_history_chart(struct parser *p)
{
    struct history_chart *oc = parser_priv(p);
    struct history_chart *c;
    struct history_entry *e = mem_zalloc(sizeof(*e));
    unsigned int idx = parser_getuint(p, "chart");

    c = findchart(oc, idx);
    if (!c)
    {
        c = mem_zalloc(sizeof(*c));
        c->next = oc;
        c->idx = idx;
        parser_setpriv(p, c);
    }

    e->isucc = parser_getint(p, "next");
    e->roll = parser_getint(p, "roll");

    e->next = c->entries;
    c->entries = e;

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_history_phrase(struct parser *p)
{
    struct history_chart *h = parser_priv(p);

    if (!h) return PARSE_ERROR_MISSING_RECORD_HEADER;
    my_assert(h->entries);
    h->entries->text = string_append(h->entries->text, parser_getstr(p, "text"));

    return PARSE_ERROR_NONE;
}


static struct parser *init_parse_history(void)
{
    struct parser *p = parser_new();

    parser_setpriv(p, NULL);
    parser_reg(p, "chart uint chart int next int roll", parse_history_chart);
    parser_reg(p, "phrase str text", parse_history_phrase);

    return p;
}


static errr run_parse_history(struct parser *p)
{
    return parse_file_quit_not_found(p, "history");
}


static errr finish_parse_history(struct parser *p)
{
    struct history_chart *c;
    struct history_entry *e, *prev, *next;

    histories = parser_priv(p);

    /*
     * Go fix up the entry successor pointers. We can't compute them at
     * load-time since we may not have seen the successor history yet. Also,
     * we need to put the entries in the right order; the parser actually
     * stores them backwards, which is not desirable.
     */
    for (c = histories; c; c = c->next)
    {
        e = c->entries;
        prev = NULL;
        while (e)
        {
            next = e->next;
            e->next = prev;
            prev = e;
            e = next;
        }
        c->entries = prev;
        for (e = c->entries; e; e = e->next)
        {
            if (!e->isucc) continue;
            e->succ = findchart(histories, e->isucc);
            if (!e->succ) return -1;
        }
    }

    parser_destroy(p);
    return 0;
}


static void cleanup_history(void)
{
    struct history_chart *c, *next_c;
    struct history_entry *e, *next_e;

    c = histories;
    while (c)
    {
        next_c = c->next;
        e = c->entries;
        while (e)
        {
            next_e = e->next;
            string_free(e->text);
            mem_free(e);
            e = next_e;
        }
        mem_free(c);
        c = next_c;
    }
}


static struct file_parser history_parser =
{
    "history",
    init_parse_history,
    run_parse_history,
    finish_parse_history,
    cleanup_history
};


/*
 * Initialize player races
 */


static enum parser_error parse_p_race_name(struct parser *p)
{
    struct player_race *h = parser_priv(p);
    struct player_race *r = mem_zalloc(sizeof(*r));

    r->next = h;
    r->name = string_make(parser_getstr(p, "name"));

    /* Default body is humanoid */
    r->body = 0;

    parser_setpriv(p, r);

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_p_race_stats(struct parser *p)
{
    struct player_race *r = parser_priv(p);

    if (!r) return PARSE_ERROR_MISSING_RECORD_HEADER;
    r->r_adj[STAT_STR] = parser_getint(p, "str");
    r->r_adj[STAT_DEX] = parser_getint(p, "dex");
    r->r_adj[STAT_CON] = parser_getint(p, "con");
    r->r_adj[STAT_INT] = parser_getint(p, "int");
    r->r_adj[STAT_WIS] = parser_getint(p, "wis");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_p_race_skill_disarm_phys(struct parser *p)
{
    struct player_race *r = parser_priv(p);

    if (!r) return PARSE_ERROR_MISSING_RECORD_HEADER;
    r->r_skills[SKILL_DISARM_PHYS] = parser_getint(p, "disarm");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_p_race_skill_disarm_magic(struct parser *p)
{
    struct player_race *r = parser_priv(p);

    if (!r) return PARSE_ERROR_MISSING_RECORD_HEADER;
    r->r_skills[SKILL_DISARM_MAGIC] = parser_getint(p, "disarm");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_p_race_skill_device(struct parser *p)
{
    struct player_race *r = parser_priv(p);

    if (!r) return PARSE_ERROR_MISSING_RECORD_HEADER;
    r->r_skills[SKILL_DEVICE] = parser_getint(p, "device");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_p_race_skill_save(struct parser *p)
{
    struct player_race *r = parser_priv(p);

    if (!r) return PARSE_ERROR_MISSING_RECORD_HEADER;
    r->r_skills[SKILL_SAVE] = parser_getint(p, "save");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_p_race_skill_stealth(struct parser *p)
{
    struct player_race *r = parser_priv(p);

    if (!r) return PARSE_ERROR_MISSING_RECORD_HEADER;
    r->r_skills[SKILL_STEALTH] = parser_getint(p, "stealth");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_p_race_skill_search(struct parser *p)
{
    struct player_race *r = parser_priv(p);

    if (!r) return PARSE_ERROR_MISSING_RECORD_HEADER;
    r->r_skills[SKILL_SEARCH] = parser_getint(p, "search");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_p_race_skill_melee(struct parser *p)
{
    struct player_race *r = parser_priv(p);

    if (!r) return PARSE_ERROR_MISSING_RECORD_HEADER;
    r->r_skills[SKILL_TO_HIT_MELEE] = parser_getint(p, "melee");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_p_race_skill_shoot(struct parser *p)
{
    struct player_race *r = parser_priv(p);

    if (!r) return PARSE_ERROR_MISSING_RECORD_HEADER;
    r->r_skills[SKILL_TO_HIT_BOW] = parser_getint(p, "shoot");
    r->r_skills[SKILL_TO_HIT_THROW] = r->r_skills[SKILL_TO_HIT_BOW];

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_p_race_skill_dig(struct parser *p)
{
    struct player_race *r = parser_priv(p);

    if (!r) return PARSE_ERROR_MISSING_RECORD_HEADER;
    r->r_skills[SKILL_DIGGING] = parser_getint(p, "dig");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_p_race_info(struct parser *p)
{
    struct player_race *r = parser_priv(p);

    if (!r) return PARSE_ERROR_MISSING_RECORD_HEADER;
    r->r_mhp = parser_getint(p, "mhp");
    r->r_exp = parser_getint(p, "exp");
    r->infra = parser_getint(p, "infra");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_p_race_history(struct parser *p)
{
    struct player_race *r = parser_priv(p);

    if (!r) return PARSE_ERROR_MISSING_RECORD_HEADER;
    r->history = findchart(histories, parser_getuint(p, "hist"));
    r->b_age = parser_getint(p, "b-age");
    r->m_age = parser_getint(p, "m-age");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_p_race_height(struct parser *p)
{
    struct player_race *r = parser_priv(p);

    if (!r) return PARSE_ERROR_MISSING_RECORD_HEADER;
    r->m_b_ht = parser_getint(p, "mbht");
    r->m_m_ht = parser_getint(p, "mmht");
    r->f_b_ht = parser_getint(p, "fbht");
    r->f_m_ht = parser_getint(p, "fmht");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_p_race_weight(struct parser *p)
{
    struct player_race *r = parser_priv(p);

    if (!r) return PARSE_ERROR_MISSING_RECORD_HEADER;
    r->m_b_wt = parser_getint(p, "mbwt");
    r->m_m_wt = parser_getint(p, "mmwt");
    r->f_b_wt = parser_getint(p, "fbwt");
    r->f_m_wt = parser_getint(p, "fmwt");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_p_race_obj_flags(struct parser *p)
{
    struct player_race *r = parser_priv(p);
    char *flags;
    char *s;

    if (!r) return PARSE_ERROR_MISSING_RECORD_HEADER;
    if (!parser_hasval(p, "flags")) return PARSE_ERROR_NONE;
    flags = string_make(parser_getstr(p, "flags"));
    s = strtok(flags, " |");
    while (s)
    {
        if (grab_flag(r->flags, OF_SIZE, list_obj_flag_names, s)) break;
        s = strtok(NULL, " |");
    }
    string_free(flags);

    return (s? PARSE_ERROR_INVALID_FLAG: PARSE_ERROR_NONE);
}


static enum parser_error parse_p_race_play_flags(struct parser *p)
{
    struct player_race *r = parser_priv(p);
    char *flags;
    char *s;

    if (!r) return PARSE_ERROR_MISSING_RECORD_HEADER;
    if (!parser_hasval(p, "flags")) return PARSE_ERROR_NONE;
    flags = string_make(parser_getstr(p, "flags"));
    s = strtok(flags, " |");
    while (s)
    {
        if (grab_flag(r->pflags, PF_SIZE, player_info_flags, s)) break;
        s = strtok(NULL, " |");
    }
    string_free(flags);

    return (s? PARSE_ERROR_INVALID_FLAG: PARSE_ERROR_NONE);
}


static enum parser_error parse_p_race_values(struct parser *p)
{
    struct player_race *r = parser_priv(p);
    char *s;
    char *t;

    if (!r) return PARSE_ERROR_MISSING_RECORD_HEADER;

    s = string_make(parser_getstr(p, "values"));
    t = strtok(s, " |");

    while (t)
    {
        int value = 0;
        int index = 0;
        bool found = false;

        if (!grab_index_and_int(&value, &index, list_element_names, "RES_", t))
        {
            found = true;
            r->el_info[index].res_level = value;
        }
        if (!found) break;
        t = strtok(NULL, " |");
    }

    string_free(s);
    return (t? PARSE_ERROR_INVALID_VALUE: PARSE_ERROR_NONE);
}


static struct parser *init_parse_p_race(void)
{
    struct parser *p = parser_new();

    parser_setpriv(p, NULL);
    parser_reg(p, "name str name", parse_p_race_name);
    parser_reg(p, "stats int str int int int wis int dex int con", parse_p_race_stats);
    parser_reg(p, "skill-disarm-phys int disarm", parse_p_race_skill_disarm_phys);
    parser_reg(p, "skill-disarm-magic int disarm", parse_p_race_skill_disarm_magic);
    parser_reg(p, "skill-device int device", parse_p_race_skill_device);
    parser_reg(p, "skill-save int save", parse_p_race_skill_save);
    parser_reg(p, "skill-stealth int stealth", parse_p_race_skill_stealth);
    parser_reg(p, "skill-search int search", parse_p_race_skill_search);
    parser_reg(p, "skill-melee int melee", parse_p_race_skill_melee);
    parser_reg(p, "skill-shoot int shoot", parse_p_race_skill_shoot);
    parser_reg(p, "skill-dig int dig", parse_p_race_skill_dig);
    parser_reg(p, "info int mhp int exp int infra", parse_p_race_info);
    parser_reg(p, "history uint hist int b-age int m-age", parse_p_race_history);
    parser_reg(p, "height int mbht int mmht int fbht int fmht", parse_p_race_height);
    parser_reg(p, "weight int mbwt int mmwt int fbwt int fmwt", parse_p_race_weight);
    parser_reg(p, "obj-flags ?str flags", parse_p_race_obj_flags);
    parser_reg(p, "player-flags ?str flags", parse_p_race_play_flags);
    parser_reg(p, "values str values", parse_p_race_values);

    return p;
}


static errr run_parse_p_race(struct parser *p)
{
    return parse_file_quit_not_found(p, "p_race");
}


static errr finish_parse_p_race(struct parser *p)
{
    struct player_race *r;
    int num = 0;

    races = parser_priv(p);
    for (r = races; r; r = r->next) num++;
    for (r = races; r; r = r->next, num--) r->ridx = num - 1;
    parser_destroy(p);
    return 0;
}


static struct file_parser p_race_parser =
{
    "p_race",
    init_parse_p_race,
    run_parse_p_race,
    finish_parse_p_race,
    cleanup_p_race
};


/*
 * Initialize player magic realms
 */


static enum parser_error parse_realm_name(struct parser *p)
{
    struct magic_realm *h = parser_priv(p);
    struct magic_realm *realm = mem_zalloc(sizeof(*realm));
    const char *name = parser_getstr(p, "name");

    realm->next = h;
    realm->name = string_make(name);

    parser_setpriv(p, realm);

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_realm_stat(struct parser *p)
{
    struct magic_realm *realm = parser_priv(p);

    if (!realm) return PARSE_ERROR_MISSING_RECORD_HEADER;
    realm->stat = stat_name_to_idx(parser_getsym(p, "stat"));

    if (realm->stat < 0)
        return PARSE_ERROR_INVALID_SPELL_STAT;

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_realm_verb(struct parser *p)
{
    struct magic_realm *realm = parser_priv(p);
    const char *verb = parser_getstr(p, "verb");

    if (!realm) return PARSE_ERROR_MISSING_RECORD_HEADER;
    realm->verb = string_make(verb);

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_realm_spell_noun(struct parser *p)
{
    struct magic_realm *realm = parser_priv(p);
    const char *spell = parser_getstr(p, "spell");

    if (!realm) return PARSE_ERROR_MISSING_RECORD_HEADER;
    realm->spell_noun = string_make(spell);

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_realm_book_noun(struct parser *p)
{
    struct magic_realm *realm = parser_priv(p);
    const char *book = parser_getstr(p, "book");

    if (!realm) return PARSE_ERROR_MISSING_RECORD_HEADER;
    realm->book_noun = string_make(book);

    return PARSE_ERROR_NONE;
}


static struct parser *init_parse_realm(void)
{
    struct parser *p = parser_new();

    parser_setpriv(p, NULL);
    parser_reg(p, "name str name", parse_realm_name);
    parser_reg(p, "stat sym stat", parse_realm_stat);
    parser_reg(p, "verb str verb", parse_realm_verb);
    parser_reg(p, "spell-noun str spell", parse_realm_spell_noun);
    parser_reg(p, "book-noun str book", parse_realm_book_noun);

    return p;
}


static errr run_parse_realm(struct parser *p)
{
    return parse_file_quit_not_found(p, "realm");
}


static errr finish_parse_realm(struct parser *p)
{
    realms = parser_priv(p);
    parser_destroy(p);
    return 0;
}


static struct file_parser realm_parser =
{
    "realm",
    init_parse_realm,
    run_parse_realm,
    finish_parse_realm,
    cleanup_realm
};


/*
 * Initialize player classes
 */


static enum parser_error parse_class_name(struct parser *p)
{
    struct player_class *h = parser_priv(p);
    struct player_class *c = mem_zalloc(sizeof(*c));

    c->name = string_make(parser_getstr(p, "name"));
    c->next = h;
    parser_setpriv(p, c);

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_class_color(struct parser *p)
{
    const char *color = parser_getsym(p, "color");
    struct player_class *c = parser_priv(p);

    if (!c) return PARSE_ERROR_MISSING_RECORD_HEADER;
    c->attr = color_char_to_attr(color[0]);

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_class_stats(struct parser *p)
{
    struct player_class *c = parser_priv(p);

    if (!c) return PARSE_ERROR_MISSING_RECORD_HEADER;

    c->c_adj[STAT_STR] = parser_getint(p, "str");
    c->c_adj[STAT_INT] = parser_getint(p, "int");
    c->c_adj[STAT_WIS] = parser_getint(p, "wis");
    c->c_adj[STAT_DEX] = parser_getint(p, "dex");
    c->c_adj[STAT_CON] = parser_getint(p, "con");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_class_skill_disarm_phys(struct parser *p)
{
    struct player_class *c = parser_priv(p);

    if (!c) return PARSE_ERROR_MISSING_RECORD_HEADER;
    c->c_skills[SKILL_DISARM_PHYS] = parser_getint(p, "base");
    c->x_skills[SKILL_DISARM_PHYS] = parser_getint(p, "incr");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_class_skill_disarm_magic(struct parser *p)
{
    struct player_class *c = parser_priv(p);

    if (!c) return PARSE_ERROR_MISSING_RECORD_HEADER;
    c->c_skills[SKILL_DISARM_MAGIC] = parser_getint(p, "base");
    c->x_skills[SKILL_DISARM_MAGIC] = parser_getint(p, "incr");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_class_skill_device(struct parser *p)
{
    struct player_class *c = parser_priv(p);

    if (!c) return PARSE_ERROR_MISSING_RECORD_HEADER;
    c->c_skills[SKILL_DEVICE] = parser_getint(p, "base");
    c->x_skills[SKILL_DEVICE] = parser_getint(p, "incr");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_class_skill_save(struct parser *p)
{
    struct player_class *c = parser_priv(p);

    if (!c) return PARSE_ERROR_MISSING_RECORD_HEADER;
    c->c_skills[SKILL_SAVE] = parser_getint(p, "base");
    c->x_skills[SKILL_SAVE] = parser_getint(p, "incr");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_class_skill_stealth(struct parser *p)
{
    struct player_class *c = parser_priv(p);

    if (!c) return PARSE_ERROR_MISSING_RECORD_HEADER;
    c->c_skills[SKILL_STEALTH] = parser_getint(p, "base");
    c->x_skills[SKILL_STEALTH] = parser_getint(p, "incr");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_class_skill_search(struct parser *p)
{
    struct player_class *c = parser_priv(p);

    if (!c) return PARSE_ERROR_MISSING_RECORD_HEADER;
    c->c_skills[SKILL_SEARCH] = parser_getint(p, "base");
    c->x_skills[SKILL_SEARCH] = parser_getint(p, "incr");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_class_skill_melee(struct parser *p)
{
    struct player_class *c = parser_priv(p);

    if (!c) return PARSE_ERROR_MISSING_RECORD_HEADER;
    c->c_skills[SKILL_TO_HIT_MELEE] = parser_getint(p, "base");
    c->x_skills[SKILL_TO_HIT_MELEE] = parser_getint(p, "incr");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_class_skill_shoot(struct parser *p)
{
    struct player_class *c = parser_priv(p);

    if (!c) return PARSE_ERROR_MISSING_RECORD_HEADER;
    c->c_skills[SKILL_TO_HIT_BOW] = parser_getint(p, "base");
    c->c_skills[SKILL_TO_HIT_THROW] = c->c_skills[SKILL_TO_HIT_BOW];
    c->x_skills[SKILL_TO_HIT_BOW] = parser_getint(p, "incr");
    c->x_skills[SKILL_TO_HIT_THROW] = c->x_skills[SKILL_TO_HIT_BOW];

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_class_skill_dig(struct parser *p)
{
    struct player_class *c = parser_priv(p);

    if (!c) return PARSE_ERROR_MISSING_RECORD_HEADER;
    c->c_skills[SKILL_DIGGING] = parser_getint(p, "base");
    c->x_skills[SKILL_DIGGING] = parser_getint(p, "incr");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_class_info(struct parser *p)
{
    struct player_class *c = parser_priv(p);

    if (!c) return PARSE_ERROR_MISSING_RECORD_HEADER;
    c->c_mhp = parser_getint(p, "mhp");
    c->c_exp = parser_getint(p, "exp");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_class_attack(struct parser *p)
{
    struct player_class *c = parser_priv(p);

    if (!c) return PARSE_ERROR_MISSING_RECORD_HEADER;
    c->max_attacks = parser_getint(p, "max-attacks");
    c->min_weight = parser_getint(p, "min-weight");
    c->att_multiply = parser_getint(p, "att-multiply");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_class_title(struct parser *p)
{
    struct player_class *c = parser_priv(p);
    int i;

    if (!c) return PARSE_ERROR_MISSING_RECORD_HEADER;
    for (i = 0; i < PY_MAX_LEVEL / 5; i++)
    {
        if (!c->title[i])
        {
            c->title[i] = string_make(parser_getstr(p, "title"));
            break;
        }
    }

    if (i >= PY_MAX_LEVEL / 5) return PARSE_ERROR_TOO_MANY_ENTRIES;
    return PARSE_ERROR_NONE;
}


static enum parser_error parse_class_equip(struct parser *p)
{
    struct player_class *c = parser_priv(p);
    struct start_item *si;
    int tval, sval;

    if (!c) return PARSE_ERROR_MISSING_RECORD_HEADER;

    tval = tval_find_idx(parser_getsym(p, "tval"));
    if (tval < 0) return PARSE_ERROR_UNRECOGNISED_TVAL;

    sval = lookup_sval(tval, parser_getsym(p, "sval"));
    if (sval < 0) return PARSE_ERROR_UNRECOGNISED_SVAL;

    si = mem_zalloc(sizeof(*si));
    si->kind = lookup_kind(tval, sval);
    si->min = parser_getuint(p, "min");
    si->max = parser_getuint(p, "max");
    si->flag = parser_getuint(p, "flag");

    if ((si->min > si->kind->base->max_stack) || (si->max > si->kind->base->max_stack))
    {
        mem_free(si);
        return PARSE_ERROR_INVALID_ITEM_NUMBER;
    }

    si->next = c->start_items;
    c->start_items = si;

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_class_flags(struct parser *p)
{
    struct player_class *c = parser_priv(p);
    char *flags;
    char *s;

    if (!c) return PARSE_ERROR_MISSING_RECORD_HEADER;
    if (!parser_hasval(p, "flags")) return PARSE_ERROR_NONE;
    flags = string_make(parser_getstr(p, "flags"));
    s = strtok(flags, " |");
    while (s)
    {
        if (grab_flag(c->pflags, PF_SIZE, player_info_flags, s)) break;
        s = strtok(NULL, " |");
    }

    string_free(flags);
    return (s? PARSE_ERROR_INVALID_FLAG: PARSE_ERROR_NONE);
}


static enum parser_error parse_class_realm(struct parser *p)
{
    struct player_class *c = parser_priv(p);

    if (!c) return PARSE_ERROR_MISSING_RECORD_HEADER;
    c->magic.spell_realm = lookup_realm(parser_getstr(p, "realm"));

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_class_magic(struct parser *p)
{
    struct player_class *c = parser_priv(p);
    int num_books;

    if (!c) return PARSE_ERROR_MISSING_RECORD_HEADER;
    if (!c->magic.spell_realm) return PARSE_ERROR_NONE;
    c->magic.spell_first = parser_getuint(p, "first");
    c->magic.spell_weight = parser_getint(p, "weight");
    num_books = parser_getint(p, "books");
    c->magic.books = mem_zalloc(num_books * sizeof(struct class_book));

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_class_book(struct parser *p)
{
    struct player_class *c = parser_priv(p);
    int tval, sval = 0, spells;

    if (!c) return PARSE_ERROR_MISSING_RECORD_HEADER;

    tval = tval_find_idx(parser_getsym(p, "tval"));
    if (tval < 0) return PARSE_ERROR_UNRECOGNISED_TVAL;

    /* Hack -- ghost/mimic spells have no sval */
    if ((tval != TV_GHOST_REALM) && (tval != TV_MIMIC_REALM))
    {
        sval = lookup_sval(tval, parser_getsym(p, "sval"));
        if (sval < 0) return PARSE_ERROR_UNRECOGNISED_SVAL;
    }

    c->magic.books[c->magic.num_books].tval = tval;
    c->magic.books[c->magic.num_books].sval = sval;
    spells = parser_getuint(p, "spells");
    c->magic.books[c->magic.num_books].spells = mem_zalloc(spells * sizeof(struct class_spell));
    c->magic.books[c->magic.num_books++].realm = parser_getuint(p, "realm");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_class_spell(struct parser *p)
{
    struct player_class *c = parser_priv(p);
    struct class_book *book = &c->magic.books[c->magic.num_books - 1];

    if (!c) return PARSE_ERROR_MISSING_RECORD_HEADER;

    book->spells[book->num_spells].name = string_make(parser_getsym(p, "name"));
    book->spells[book->num_spells].sidx = c->magic.total_spells;
    c->magic.total_spells++;
    book->spells[book->num_spells].bidx = c->magic.num_books - 1;
    book->spells[book->num_spells].slevel = parser_getint(p, "level");
    book->spells[book->num_spells].smana = parser_getint(p, "mana");
    book->spells[book->num_spells].sfail = parser_getint(p, "fail");
    book->spells[book->num_spells].sexp = parser_getint(p, "exp");
    book->spells[book->num_spells].sproj = parser_getuint(p, "sproj");
    book->num_spells++;

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_class_effect(struct parser *p)
{
    struct player_class *c = parser_priv(p);
    struct class_book *book = &c->magic.books[c->magic.num_books - 1];
    struct class_spell *spell = &book->spells[book->num_spells - 1];
    struct effect *new_effect = mem_zalloc(sizeof(*new_effect));
    errr ret;

    if (!c) return PARSE_ERROR_MISSING_RECORD_HEADER;

    /* Fill in the detail */
    ret = grab_effect_data(p, new_effect);
    if (ret) return ret;

    new_effect->next = spell->effect;
    spell->effect = new_effect;

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_class_flag(struct parser *p)
{
    struct player_class *c = parser_priv(p);
    struct class_book *book = &c->magic.books[c->magic.num_books - 1];
    struct class_spell *spell = &book->spells[book->num_spells - 1];
    int flag;

    if (!c) return PARSE_ERROR_MISSING_RECORD_HEADER;

    /* If there is no effect, assume that this is human and not parser error. */
    if (spell->effect == NULL) return PARSE_ERROR_NONE;

    /* Hack -- mimic spells are defined by their RSF_XXX flag */
    if (grab_name("flag", parser_getsym(p, "flag"), r_info_spell_flags, RSF_MAX, &flag))
        return PARSE_ERROR_INVALID_FLAG;
    spell->effect->flag = flag;

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_class_param(struct parser *p)
{
    struct player_class *c = parser_priv(p);
    struct class_book *book = &c->magic.books[c->magic.num_books - 1];
    struct class_spell *spell = &book->spells[book->num_spells - 1];

    if (!c) return PARSE_ERROR_MISSING_RECORD_HEADER;

    /* If there is no effect, assume that this is human and not parser error. */
    if (spell->effect == NULL) return PARSE_ERROR_NONE;

    spell->effect->params[1] = parser_getint(p, "p2");

    if (parser_hasval(p, "p3"))
        spell->effect->params[2] = parser_getint(p, "p3");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_class_dice(struct parser *p)
{
    struct player_class *c = parser_priv(p);
    struct class_book *book = &c->magic.books[c->magic.num_books - 1];
    struct class_spell *spell = &book->spells[book->num_spells - 1];
    dice_t *dice = NULL;
    const char *string = NULL;

    if (!c) return PARSE_ERROR_MISSING_RECORD_HEADER;

    /* If there is no effect, assume that this is human and not parser error. */
    if (spell->effect == NULL) return PARSE_ERROR_NONE;

    dice = dice_new();

    if (dice == NULL) return PARSE_ERROR_INVALID_DICE;

    string = parser_getstr(p, "dice");

    if (dice_parse_string(dice, string))
        spell->effect->dice = dice;
    else
    {
        dice_free(dice);
        return PARSE_ERROR_INVALID_DICE;
    }

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_class_expr(struct parser *p)
{
    struct player_class *c = parser_priv(p);
    struct class_book *book = &c->magic.books[c->magic.num_books - 1];
    struct class_spell *spell = &book->spells[book->num_spells - 1];
    expression_t *expression = NULL;
    expression_base_value_f function = NULL;
    const char *name;
    const char *base;
    const char *expr;

    if (!c) return PARSE_ERROR_MISSING_RECORD_HEADER;

    /* If there is no effect, assume that this is human and not parser error. */
    if (spell->effect == NULL) return PARSE_ERROR_NONE;

    /* If there are no dice, assume that this is human and not parser error. */
    if (spell->effect->dice == NULL) return PARSE_ERROR_NONE;

    name = parser_getsym(p, "name");
    base = parser_getsym(p, "base");
    expr = parser_getstr(p, "expr");
    expression = expression_new();

    if (expression == NULL) return PARSE_ERROR_INVALID_EXPRESSION;

    function = spell_value_base_by_name(base);
    expression_set_base_value(expression, function);

    if (expression_add_operations_string(expression, expr) < 0)
        return PARSE_ERROR_BAD_EXPRESSION_STRING;

    if (dice_bind_expression(spell->effect->dice, name, expression) < 0)
        return PARSE_ERROR_UNBOUND_EXPRESSION;

    /* The dice object makes a deep copy of the expression, so we can free it */
    expression_free(expression);

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_class_msg_self(struct parser *p)
{
    struct player_class *c = parser_priv(p);
    struct class_book *book = &c->magic.books[c->magic.num_books - 1];
    struct class_spell *spell = &book->spells[book->num_spells - 1];

    if (!c) return PARSE_ERROR_MISSING_RECORD_HEADER;

    /* If there is no effect, assume that this is human and not parser error. */
    if (spell->effect == NULL) return PARSE_ERROR_NONE;

    spell->effect->self_msg = string_make(parser_getstr(p, "msg_self"));

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_class_msg_other(struct parser *p)
{
    struct player_class *c = parser_priv(p);
    struct class_book *book = &c->magic.books[c->magic.num_books - 1];
    struct class_spell *spell = &book->spells[book->num_spells - 1];

    if (!c) return PARSE_ERROR_MISSING_RECORD_HEADER;

    /* If there is no effect, assume that this is human and not parser error. */
    if (spell->effect == NULL) return PARSE_ERROR_NONE;

    spell->effect->other_msg = string_make(parser_getstr(p, "msg_other"));

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_class_desc(struct parser *p)
{
    struct player_class *c = parser_priv(p);
    struct class_book *book = &c->magic.books[c->magic.num_books - 1];
    struct class_spell *spell = &book->spells[book->num_spells - 1];

    if (!c) return PARSE_ERROR_MISSING_RECORD_HEADER;

    spell->text = string_append(spell->text, parser_getstr(p, "desc"));

    return PARSE_ERROR_NONE;
}


static struct parser *init_parse_class(void)
{
    struct parser *p = parser_new();

    parser_setpriv(p, NULL);
    parser_reg(p, "name str name", parse_class_name);
    parser_reg(p, "color sym color", parse_class_color);
    parser_reg(p, "stats int str int int int wis int dex int con", parse_class_stats);
    parser_reg(p, "skill-disarm-phys int base int incr", parse_class_skill_disarm_phys);
    parser_reg(p, "skill-disarm-magic int base int incr", parse_class_skill_disarm_magic);
    parser_reg(p, "skill-device int base int incr", parse_class_skill_device);
    parser_reg(p, "skill-save int base int incr", parse_class_skill_save);
    parser_reg(p, "skill-stealth int base int incr", parse_class_skill_stealth);
    parser_reg(p, "skill-search int base int incr", parse_class_skill_search);
    parser_reg(p, "skill-melee int base int incr", parse_class_skill_melee);
    parser_reg(p, "skill-shoot int base int incr", parse_class_skill_shoot);
    parser_reg(p, "skill-dig int base int incr", parse_class_skill_dig);
    parser_reg(p, "info int mhp int exp", parse_class_info);
    parser_reg(p, "attack int max-attacks int min-weight int att-multiply", parse_class_attack);
    parser_reg(p, "equip sym tval sym sval uint min uint max uint flag", parse_class_equip);
    parser_reg(p, "flags ?str flags", parse_class_flags);
    parser_reg(p, "title str title", parse_class_title);
    parser_reg(p, "realm str realm", parse_class_realm);
    parser_reg(p, "magic uint first int weight int books", parse_class_magic);
    parser_reg(p, "book sym tval sym sval uint spells uint realm", parse_class_book);
    parser_reg(p, "spell sym name int level int mana int fail int exp uint sproj",
        parse_class_spell);
    parser_reg(p, "effect sym eff ?sym type ?int xtra", parse_class_effect);
    parser_reg(p, "flag sym flag", parse_class_flag);
    parser_reg(p, "param int p2 ?int p3", parse_class_param);
    parser_reg(p, "dice str dice", parse_class_dice);
    parser_reg(p, "expr sym name sym base str expr", parse_class_expr);
    parser_reg(p, "msg_self str msg_self", parse_class_msg_self);
    parser_reg(p, "msg_other str msg_other", parse_class_msg_other);
    parser_reg(p, "desc str desc", parse_class_desc);

    return p;
}


static errr run_parse_class(struct parser *p)
{
    return parse_file_quit_not_found(p, "class");
}


static errr finish_parse_class(struct parser *p)
{
    struct player_class *c;
    int num = 0;

    classes = parser_priv(p);
    for (c = classes; c; c = c->next) num++;
    for (c = classes; c; c = c->next, num--) c->cidx = num - 1;
    parser_destroy(p);
    return 0;
}


static struct file_parser class_parser =
{
    "class",
    init_parse_class,
    run_parse_class,
    finish_parse_class,
    cleanup_class
};


/*
 * Initialize flavors
 */


static char flavor_glyph;
static int flavor_tval;


static enum parser_error parse_flavor_flavor(struct parser *p)
{
    struct flavor *h = parser_priv(p);
    struct flavor *f = mem_zalloc(sizeof(*f));
    const char *attr;
    int d_attr;

    f->next = h;

    f->fidx = parser_getuint(p, "index");
    f->tval = flavor_tval;
    f->d_char = flavor_glyph;

    if (parser_hasval(p, "sval"))
        f->sval = lookup_sval(f->tval, parser_getsym(p, "sval"));
    else
        f->sval = SV_UNKNOWN;

    attr = parser_getsym(p, "attr");
    if (strlen(attr) == 1)
        d_attr = color_char_to_attr(attr[0]);
    else
        d_attr = color_text_to_attr(attr);
    if (d_attr < 0) return PARSE_ERROR_INVALID_COLOR;
    f->d_attr = d_attr;

    if (parser_hasval(p, "desc"))
        f->text = string_append(f->text, parser_getstr(p, "desc"));

    parser_setpriv(p, f);

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_flavor_kind(struct parser *p)
{
    flavor_glyph = parser_getchar(p, "glyph");
    flavor_tval = tval_find_idx(parser_getsym(p, "tval"));
    if (flavor_tval < 0) return PARSE_ERROR_UNRECOGNISED_TVAL;

    return PARSE_ERROR_NONE;
}


static struct parser *init_parse_flavor(void)
{
    struct parser *p = parser_new();

    parser_setpriv(p, NULL);

    parser_reg(p, "kind sym tval char glyph", parse_flavor_kind);
    parser_reg(p, "flavor uint index sym attr ?str desc", parse_flavor_flavor);
    parser_reg(p, "fixed uint index sym sval sym attr ?str desc", parse_flavor_flavor);

    return p;
}


static errr run_parse_flavor(struct parser *p)
{
    return parse_file_quit_not_found(p, "flavor");
}


static errr finish_parse_flavor(struct parser *p)
{
    flavors = parser_priv(p);
    parser_destroy(p);
    return 0;
}


static void cleanup_flavor(void)
{
    struct flavor *f, *next;

    f = flavors;
    while (f)
    {
        next = f->next;
        string_free(f->text);
        mem_free(f);
        f = next;
    }
}


static struct file_parser flavor_parser =
{
    "flavor",
    init_parse_flavor,
    run_parse_flavor,
    finish_parse_flavor,
    cleanup_flavor
};


/*
 * Initialize socials
 */


static enum parser_error parse_soc_n(struct parser *p)
{
    struct social *s = mem_zalloc(sizeof(*s));

    s->next = parser_priv(p);
    s->name = string_make(parser_getstr(p, "name"));
    parser_setpriv(p, s);

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_soc_i(struct parser *p)
{
    struct social *s = parser_priv(p);

    if (!s) return PARSE_ERROR_MISSING_RECORD_HEADER;

    s->target = parser_getuint(p, "target");
    s->max_dist = parser_getuint(p, "max-dist");

    return PARSE_ERROR_NONE;
}


static enum parser_error parse_soc_d(struct parser *p)
{
    struct social *s = parser_priv(p);

    if (!s) return PARSE_ERROR_MISSING_RECORD_HEADER;

    s->text = string_append(s->text, parser_getstr(p, "desc"));

    return PARSE_ERROR_NONE;
}


static struct parser *init_parse_soc(void)
{
    struct parser *p = parser_new();

    parser_setpriv(p, NULL);
    parser_reg(p, "N str name", parse_soc_n);
    parser_reg(p, "I uint target uint max-dist", parse_soc_i);
    parser_reg(p, "D str desc", parse_soc_d);

    return p;
}


static errr run_parse_soc(struct parser *p)
{
    return parse_file_quit_not_found(p, "socials");
}


static errr finish_parse_soc(struct parser *p)
{
    struct social *s, *n;
    int sidx;

    /* Scan the list for the max id */
    z_info->soc_max = 0;
    s = parser_priv(p);
    while (s)
    {
        z_info->soc_max++;
        s = s->next;
    }

    /* Allocate the direct access list and copy the data to it */
    soc_info = mem_zalloc(z_info->soc_max * sizeof(*s));
    sidx = z_info->soc_max - 1;
    for (s = parser_priv(p); s; s = n, sidx--)
    {
        memcpy(&soc_info[sidx], s, sizeof(*s));
        soc_info[sidx].sidx = sidx;
        n = s->next;
        if (sidx < z_info->soc_max - 1) soc_info[sidx].next = &soc_info[sidx + 1];
        else soc_info[sidx].next = NULL;
        mem_free(s);
    }

    parser_destroy(p);
    return 0;
}


static void cleanup_soc(void)
{
    int i;

    /* Paranoia */
    if (!soc_info) return;

    for (i = 0; i < z_info->soc_max; i++)
    {
        string_free(soc_info[i].name);
        string_free(soc_info[i].text);
    }
    mem_free(soc_info);
}


static struct file_parser soc_parser =
{
    "socials",
    init_parse_soc,
    run_parse_soc,
    finish_parse_soc,
    cleanup_soc
};


/*
 * Initialize hints
 */


static enum parser_error parse_hint(struct parser *p)
{
    struct hint *h = parser_priv(p);
    struct hint *n = mem_zalloc(sizeof(*n));

    n->hint = string_make(parser_getstr(p, "text"));
    n->next = h;

    parser_setpriv(p, n);
    return PARSE_ERROR_NONE;
}


static struct parser *init_parse_hints(void)
{
    struct parser *p = parser_new();

    parser_reg(p, "H str text", parse_hint);
    return p;
}


static errr run_parse_hints(struct parser *p)
{
    return parse_file_quit_not_found(p, "hints");
}


static errr finish_parse_hints(struct parser *p)
{
    hints = parser_priv(p);
    parser_destroy(p);
    return 0;
}


static void cleanup_hints(void)
{
    struct hint *h, *next;

    h = hints;
    while (h)
    {
        next = h->next;
        string_free(h->hint);
        mem_free(h);
        h = next;
    }
}


static struct file_parser hints_parser =
{
    "hints",
    init_parse_hints,
    run_parse_hints,
    finish_parse_hints,
    cleanup_hints
};


/*
 * Game data initialization
 */


/*
 * A list of all the above parsers, plus those found in mon-init.c and
 * obj-init.c
 */
static struct
{
    const char *name;
    struct file_parser *parser;
} pl[] =
{
    {"projections", &projection_parser},
    {"timed effects", &player_timed_parser},
    {"features", &feat_parser},
    {"wilderness feats", &wild_feat_parser},
    {"wilderness info", &wild_info_parser},
    {"town feats", &town_feat_parser},
    {"towns", &town_info_parser},
    {"dungeons", &dungeon_info_parser},
    {"object bases", &object_base_parser},
    {"slays", &slay_parser},
    {"brands", &brand_parser},
    {"monster pain messages", &pain_parser},
    {"monster bases", &mon_base_parser},
    {"summons", &summon_parser},
    {"curses", &curse_parser},
    {"activations", &act_parser},
    {"objects", &object_parser},
    {"ego-items", &ego_parser},
    {"artifacts", &artifact_parser},
    {"object properties", &object_property_parser},
    {"object power calculations", &object_power_parser},
    {"blow methods", &meth_parser},
    {"blow effects", &eff_parser},
    {"monster spells", &mon_spell_parser},
    {"monsters", &monster_parser},
    {"monster pits", &pit_parser},
    {"traps", &trap_parser},
    {"quests", &quests_parser},
    {"history charts", &history_parser},
    {"bodies", &body_parser},
    {"player races", &p_race_parser},
    {"magic realms", &realm_parser},
    {"player classes", &class_parser},
    {"flavours", &flavor_parser},
    {"socials", &soc_parser},
    {"hints", &hints_parser},
    {"random names", &names_parser}
};


/*
 * Initialize just the internal arrays.
 */
static void init_arrays(void)
{
    int i;

    for (i = 0; i < N_ELEMENTS(pl); i++)
    {
        plog_fmt("Initializing %s...", pl[i].name);
        if (run_parser(pl[i].parser)) quit_fmt("Cannot initialize %s.", pl[i].name);
    }
}


/*
 * Free all the internal arrays
 */
static void cleanup_arrays(void)
{
    int i;

    for (i = N_ELEMENTS(pl) - 1; i >= 0; i--)
        cleanup_parser(pl[i].parser);
}


static struct init_module arrays_module =
{
    "arrays",
    init_arrays,
    cleanup_arrays
};


extern struct init_module z_quark_module;
extern struct init_module generate_module;
extern struct init_module rune_module;
extern struct init_module mon_make_module;
extern struct init_module obj_make_module;
extern struct init_module ignore_module;
extern struct init_module store_module;


static struct init_module *modules[] =
{
    &z_quark_module,
    &arrays_module,
    &generate_module,
    &rune_module,
    &mon_make_module,
    &obj_make_module,
    &ignore_module,
    &store_module,
    NULL
};


/*
 * Initialize Angband's data stores and allocate memory for structures,
 * etc, so that the game can get started.
 */
void init_angband(void)
{
    int i;

    /* Initialize player presets */
    plog("Initializing player presets...");
    process_pref_file_xtra("presets.prf");

    init_game_constants();

    /* Initialize modules */
    for (i = 0; modules[i]; i++)
    {
        if (modules[i]->init) modules[i]->init();
    }

    /* Initialize some other things */
    plog("Initializing other stuff...");

    /* Allocate space for houses */
    houses_init();

    /* Initialize the wilderness info */
    init_wild_info();

    /* Prepare chat channels */
    for (i = 0; i < MAX_CHANNELS; i++)
    {
        channels[i].name[0] = '\0';
        channels[i].id = 0;
        channels[i].num = 0;
        channels[i].mode = 0;
        chan_audit = chan_debug = chan_cheat = 0;
    }
    my_strcpy(channels[0].name, DEFAULT_CHANNEL, sizeof(channels[0].name));
    channels[0].mode = CM_SERVICE | CM_PLOG;
    for (i = 1; i < 4; i++)
    {
        channels[i].id = i;
        channels[i].mode = (CM_SECRET | CM_KEYLOCK | CM_SERVICE);
        switch (i)
        {
            case 1:
                chan_audit = i;
                my_strcpy(channels[i].name, "#audit", sizeof(channels[0].name));
                break;
            case 2:
                chan_debug = i;
                my_strcpy(channels[i].name, "#debug", sizeof(channels[0].name));
                break;
            case 3:
                chan_cheat = i;
                my_strcpy(channels[i].name, "#cheat", sizeof(channels[0].name));
                break;
        }
    }

    /* Initialize randart info */
    if (cfg_random_artifacts)
    {
        plog("Initializing randarts...");
        init_randart_generator();
    }

    /* Initialize RNG */
    plog("Initializing RNG...");
    Rand_init();

    /* Done */
    plog("Initialization complete");
}


/*
 * Dirty hack -- unset server options
 */
static void unload_server_cfg(void)
{
    string_free(cfg_meta_address);
    string_free(cfg_bind_name);
    string_free(cfg_report_address);
    string_free(cfg_console_password);
    string_free(cfg_dungeon_master);
    string_free(cfg_load_pref_file);
}


/*
 * Free all the stuff initialised in init_angband()
 */
void cleanup_angband(void)
{
    int i;
    static byte done = 0;

    /* Don't re-enter */
    if (done) return;
    done++;

    /* Stop the main loop */
    remove_timer_tick();

    /* Free wilderness info */
    free_wild_info();

    /* Free options from mangband.cfg */
    unload_server_cfg();

    /* Misc */
    wipe_player_names();

    /* Free the allocation tables */
    for (i = 0; modules[i]; i++)
    {
        if (modules[i]->cleanup) modules[i]->cleanup();
    }

    cleanup_game_constants();

    /* Free attr/chars used for dumps */
    textui_prefs_free();

    /* Free the houses */
    houses_free();

    /* Free the format() buffer */
    vformat_kill();

    /* Free the directories */
    free_file_paths();

    /* Stop the network server */
    Stop_net_server();
}


static bool str_to_boolean(char * str)
{
    /* false by default */
    return !(my_stricmp(str, "true"));
}


/*
 * Try to set a server option.  This is handled very sloppily right now,
 * since server options are manually mapped to global variables.  Maybe
 * the handeling of this will be unified in the future with some sort of
 * options structure.
 */
static void set_server_option(char *option, char *value)
{
    /* Due to the lame way that C handles strings, we can't use a switch statement */
    if (!strcmp(option, "REPORT_TO_METASERVER"))
        cfg_report_to_meta = str_to_boolean(value);
    else if (!strcmp(option, "MANGBAND_METASERVER"))
        cfg_mang_meta = str_to_boolean(value);
    else if (!strcmp(option, "META_ADDRESS"))
    {
        string_free(cfg_meta_address);
        cfg_meta_address = string_make(value);
    }
    else if (!strcmp(option, "META_PORT"))
    {
        cfg_meta_port = atoi(value);

        /* We probably ought to do some sanity check here */
        if ((cfg_meta_port > 65535) || (cfg_meta_port < 1))
            cfg_meta_port = 8800;
    }
    else if (!strcmp(option, "BIND_NAME"))
    {
        string_free(cfg_bind_name);
        cfg_bind_name = string_make(value);
    }
    else if (!strcmp(option, "REPORT_ADDRESS"))
    {
        string_free(cfg_report_address);
        cfg_report_address = string_make(value);
    }
    else if (!strcmp(option, "CONSOLE_PASSWORD"))
    {
        string_free(cfg_console_password);
        cfg_console_password = string_make(value);
    }
    else if (!strcmp(option, "DUNGEON_MASTER_NAME"))
    {
        string_free(cfg_dungeon_master);
        cfg_dungeon_master = string_make(value);
        plog_fmt("Dungeon Master Set as [%s]", cfg_dungeon_master);
    }
    else if (!strcmp(option, "SECRET_DUNGEON_MASTER"))
        cfg_secret_dungeon_master = str_to_boolean(value);
    else if (!strcmp(option, "FPS"))
    {
        cfg_fps = atoi(value);

        /* Hack -- reinstall the timer handler to match the new FPS */
        install_timer_tick(run_game_loop, cfg_fps);
    }
    else if (!strcmp(option, "NO_STEAL"))
        cfg_no_steal = str_to_boolean(value);
    else if (!strcmp(option, "NEWBIES_CANNOT_DROP"))
        cfg_newbies_cannot_drop = str_to_boolean(value);
    else if (!strcmp(option, "LEVEL_UNSTATIC_CHANCE"))
        cfg_level_unstatic_chance = atoi(value);
    else if (!strcmp(option, "RETIRE_TIMER"))
        cfg_retire_timer = atoi(value);
    else if (!strcmp(option, "ALLOW_RANDOM_ARTIFACTS"))
        cfg_random_artifacts = str_to_boolean(value);
    else if (!strcmp(option, "LIMIT_STAIRS"))
    {
        cfg_limit_stairs = atoi(value);

        /* Sanity checks */
        if (cfg_limit_stairs < 0) cfg_limit_stairs = 0;
        if (cfg_limit_stairs > 3) cfg_limit_stairs = 3;
    }
    else if (!strcmp(option, "DIVING_MODE"))
    {
        cfg_diving_mode = atoi(value);

        /* Sanity checks */
        if (cfg_diving_mode < 0) cfg_diving_mode = 0;
        if (cfg_diving_mode > 2) cfg_diving_mode = 2;
    }
    else if (!strcmp(option, "NO_GHOST"))
        cfg_no_ghost = str_to_boolean(value);
    else if (!strcmp(option, "MORE_TOWNS"))
        cfg_more_towns = str_to_boolean(value);
    else if (!strcmp(option, "ARTIFACT_DROP_SHALLOW"))
        cfg_artifact_drop_shallow = str_to_boolean(value);
    else if (!strcmp(option, "LIMIT_PLAYER_CONNECTIONS"))
        cfg_limit_player_connections = str_to_boolean(value);
    else if (!strcmp(option, "MAX_TOWNIES"))
        cfg_max_townies = atoi(value);
    else if (!strcmp(option, "MAX_TREES"))
        cfg_max_trees = atoi(value);
    else if (!strcmp(option, "TCP_PORT"))
    {
        cfg_tcp_port = atoi(value);

        /* We probably ought to do some sanity check here */
        if (cfg_tcp_port & 0x01) /* Odd number */
            cfg_tcp_port++;
        if ((cfg_tcp_port > 65535) || (cfg_tcp_port < 1))
            cfg_tcp_port = 18346;
    }
    else if (!strcmp(option, "CHARACTER_DUMP_COLOR"))
        cfg_chardump_color = str_to_boolean(value);
    else if (!strcmp(option, "PVP_HOSTILITY"))
    {
        cfg_pvp_hostility = atoi(value);

        /* Sanity checks */
        if (cfg_pvp_hostility < 0) cfg_pvp_hostility = 0;
        if (cfg_pvp_hostility > 4) cfg_pvp_hostility = 4;
    }
    else if (!strcmp(option, "BASE_MONSTERS"))
        cfg_base_monsters = str_to_boolean(value);
    else if (!strcmp(option, "EXTRA_MONSTERS"))
        cfg_extra_monsters = str_to_boolean(value);
    else if (!strcmp(option, "GHOST_DIVING"))
        cfg_ghost_diving = str_to_boolean(value);
    else if (!strcmp(option, "CONSOLE_LOCAL_ONLY"))
        cfg_console_local_only = str_to_boolean(value);
    else if (!strcmp(option, "LOAD_PREF_FILE"))
    {
        string_free(cfg_load_pref_file);
        cfg_load_pref_file = string_make(value);
    }
    else if (!strcmp(option, "PRESERVE_ARTIFACTS"))
    {
        cfg_preserve_artifacts = atoi(value);

        /* Sanity checks */
        if (cfg_preserve_artifacts < 0) cfg_preserve_artifacts = 0;
        if (cfg_preserve_artifacts > 4) cfg_preserve_artifacts = 4;
    }
    else if (!strcmp(option, "SAFE_RECHARGE"))
        cfg_safe_recharge = str_to_boolean(value);
    else if (!strcmp(option, "PARTY_SHARELEVEL"))
        cfg_party_sharelevel = atoi(value);
    else if (!strcmp(option, "TURN_BASED"))
        cfg_turn_based = str_to_boolean(value);
    else if (!strcmp(option, "LIMITED_ESP"))
        cfg_limited_esp = str_to_boolean(value);
    else if (!strcmp(option, "DOUBLE_PURSE"))
        cfg_double_purse = str_to_boolean(value);
    else if (!strcmp(option, "AI_LEARN"))
        cfg_ai_learn = str_to_boolean(value);
    else plog_fmt("Error : unrecognized mangband.cfg option %s", option);
}


/*
 * Parse the loaded mangband.cfg file, and if a valid expression was found
 * try to set it using set_server_option.
 */
static void load_server_cfg_aux(ang_file *cfg)
{
    char line[256];
    char *newword;
    char *option;
    char *value;
    bool first_token;

    /* Read in lines until we hit EOF */
    while (file_getl(cfg, line, sizeof(line)))
    {
        /* Parse the line that has been read in */
        /* If the line begins with a # or is empty, ignore it */
        if ((line[0] == '#') || (line[0] == '\0')) continue;

        /* Reset option and value */
        option = NULL;
        value = NULL;

        /*
         * Split the line up into words
         * We pass the string to be parsed to strsep on the first call,
         * and subsequently pass it null.
         */
        first_token = 1;
        newword = strtok(first_token ? line : NULL, " ");
        while (newword)
        {
            first_token = 0;

            /* Set the option or value */
            if (!option) option = newword;
            else if ((!value) && (newword[0] != '='))
            {
                value = newword;

                /* Hack -- ignore "" around values */
                if (value[0] == '"') value++;
                if (value[strlen(value) - 1] == '"')
                    value[strlen(value) - 1] = '\0';
            }

            /* If we have a completed option and value, then try to set it */
            if (option && value)
            {
                set_server_option(option, value);
                break;
            }

            newword = strtok(first_token? line: NULL, " ");
        }
    }
}


/*
 * Load in the mangband.cfg file.  This is a file that holds many
 * options that have historically been #defined in config.h.
 */
void load_server_cfg(void)
{
    ang_file *cfg;

    /* Attempt to open the file */
    cfg = file_open("mangband.cfg", MODE_READ, FTYPE_TEXT);

    /* Failure */
    if (!cfg)
    {
        plog("Error : cannot open file mangband.cfg");
        return;
    }

    /* Default */
    cfg_fps = 12;

    /* Actually parse the file */
    load_server_cfg_aux(cfg);

    /* Close it */
    file_close(cfg);
}
