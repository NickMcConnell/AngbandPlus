/**
 * \file player-ability.c
 * \brief All ability-related code
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
 */

#include "angband.h"
#include "datafile.h"
#include "effects.h"
#include "game-input.h"
#include "init.h"
#include "obj-init.h"
#include "player.h"
#include "player-ability.h"
#include "player-calcs.h"
#include "project.h"
#include "ui-display.h"
#include "ui-input.h"
#include "ui-output.h"
#include "z-textblock.h"

static const char *ability_name(int a);

/** The list of abilities (indexed by player flag) */
struct ability *ability[PF_MAX];

/*
 * An ability is a player flag - it may be entirely positive, entirely negative
 * or a mixture and includes "talents" which can be bought with talent points,
 * and "mutations" which are acquired through a mutation - and it may include
 * some abilities which are neither, or both.
 * 
 * Talents can be bought at any time, but some may be birth-only or limited to
 * being gained after a certain level.
 * 
 * They are stored as player race/class flags and tested in the same way, but
 * have an additional ability structure. These are added to ability[] when the
 * ability.txt is parsed - a player flag with ability[<flag>] == NULL is not
 * an ability.
 * 
 * Gaining an ability may be a requirement to gain another, or block it.
 * 
 * Abilities can be listed in a player knowledge screen.
 */


/**
 * Parsing functions for ability.txt
 */
static enum parser_error parse_ability_name(struct parser *p) {
	struct ability *a = mem_zalloc(sizeof(*a));
	parser_setpriv(p, a);
	a->name = string_make(parser_getstr(p, "name"));

	parsing_magic = &(a->magic);

	/* Determine which entry to accept */
	#define PF(N) if (!my_stricmp(#N, a->name)) { ability[PF_##N] = a; return PARSE_ERROR_NONE; }
	#include "list-player-flags.h"
	#undef PF

	return PARSE_ERROR_INVALID_PLAYER_FLAG;
}

static enum parser_error parse_ability_forbid(struct parser *p) {
	struct ability *a = parser_priv(p);
	assert(a);
	const char *forbid = parser_getstr(p, "forbid");
	int index = 0;

	/* Locate by name and set the flag. It's done this way to avoid needing a second pass with the names read. */
	#define PF(N) if (!my_stricmp(#N, forbid)) { a->forbid[index] = true; return PARSE_ERROR_NONE; } index++;
	#include "list-player-flags.h"
	#undef PF

	return PARSE_ERROR_INVALID_PLAYER_FLAG;
}

static enum parser_error parse_ability_require(struct parser *p) {
	struct ability *a = parser_priv(p);
	assert(a);
	const char *require = parser_getstr(p, "require");
	int index = 0;

	/* Locate by name and set the flag. It's done this way to avoid needing a second pass with the names read. */
	#define PF(N) if (!my_stricmp(#N, require)) { a->require[index] = true; return PARSE_ERROR_NONE; } index++;
	#include "list-player-flags.h"
	#undef PF

	return PARSE_ERROR_INVALID_PLAYER_FLAG;
}

static enum parser_error parse_ability_stats(struct parser *p) {
	struct ability *a = parser_priv(p);
	assert(a);

	a->a_adj[STAT_STR] = parser_getint(p, "str");
	a->a_adj[STAT_DEX] = parser_getint(p, "dex");
	a->a_adj[STAT_CON] = parser_getint(p, "con");
	a->a_adj[STAT_INT] = parser_getint(p, "int");
	a->a_adj[STAT_WIS] = parser_getint(p, "wis");
	a->a_adj[STAT_CHR] = parser_getint(p, "chr");
	a->a_adj[STAT_SPD] = parser_getint(p, "spd");
	return PARSE_ERROR_NONE;
}

static enum parser_error parse_ability_gain(struct parser *p) {
	struct ability *a = parser_priv(p);
	assert(a);

	a->gain = string_make(parser_getstr(p, "gain"));
	return PARSE_ERROR_NONE;
}

static enum parser_error parse_ability_lose(struct parser *p) {
	struct ability *a = parser_priv(p);
	assert(a);

	a->lose = string_make(parser_getstr(p, "lose"));
	return PARSE_ERROR_NONE;
}

static enum parser_error parse_ability_brief(struct parser *p) {
	struct ability *a = parser_priv(p);
	assert(a);

	a->brief = string_make(parser_getstr(p, "brief"));
	return PARSE_ERROR_NONE;
}

static enum parser_error parse_ability_class(struct parser *p) {
	struct ability *a = parser_priv(p);
	assert(a);

	a->class = string_make(parser_getstr(p, "class"));
	return PARSE_ERROR_NONE;
}

static enum parser_error parse_ability_desc(struct parser *p) {
	struct ability *a = parser_priv(p);
	assert(a);

	a->desc = string_make(parser_getstr(p, "desc"));
	return PARSE_ERROR_NONE;
}

static enum parser_error parse_ability_desc_future(struct parser *p) {
	struct ability *a = parser_priv(p);
	assert(a);

	a->desc_future = string_make(parser_getstr(p, "desc_future"));
	return PARSE_ERROR_NONE;
}

static enum parser_error parse_ability_maxlevel(struct parser *p) {
	struct ability *a = parser_priv(p);
	assert(a);

	a->maxlevel = parser_getint(p, "max");
	return PARSE_ERROR_NONE;
}

static enum parser_error parse_ability_minlevel(struct parser *p) {
	struct ability *a = parser_priv(p);
	assert(a);

	a->minlevel = parser_getint(p, "min");
	return PARSE_ERROR_NONE;
}

static enum parser_error parse_ability_cost(struct parser *p) {
	struct ability *a = parser_priv(p);
	assert(a);

	a->cost = parser_getint(p, "cost");
	return PARSE_ERROR_NONE;
}

static enum parser_error parse_ability_ac(struct parser *p) {
	struct ability *a = parser_priv(p);
	assert(a);

	a->ac = parser_getint(p, "ac");
	return PARSE_ERROR_NONE;
}

static enum parser_error parse_ability_tohit(struct parser *p) {
	struct ability *a = parser_priv(p);
	assert(a);

	a->tohit = parser_getint(p, "tohit");
	return PARSE_ERROR_NONE;
}

static enum parser_error parse_ability_todam(struct parser *p) {
	struct ability *a = parser_priv(p);
	assert(a);

	a->todam = parser_getint(p, "todam");
	return PARSE_ERROR_NONE;
}

static enum parser_error parse_ability_flag(struct parser *p) {
	struct ability *a = parser_priv(p);

	if (!a)
		return PARSE_ERROR_MISSING_RECORD_HEADER;

	const char *text = parser_getstr(p, "flag");
	u32b flag = 0;

	if (!my_stricmp(text, "birth"))
		flag = AF_BIRTH;
	if (!my_stricmp(text, "mutation"))
		flag = AF_MUTATION;
	if (!my_stricmp(text, "talent"))
		flag = AF_TALENT;
	if (!my_stricmp(text, "nasty"))
		flag = AF_NASTY;

	if (flag)
		a->flags |= flag;
	else
		return PARSE_ERROR_INVALID_FLAG;

	return PARSE_ERROR_NONE;
}

static enum parser_error parse_ability_obj_flags(struct parser *p) {
	struct ability *a = parser_priv(p);
	char *flags;
	char *s;

	if (!a)
		return PARSE_ERROR_MISSING_RECORD_HEADER;
	if (!parser_hasval(p, "flags"))
		return PARSE_ERROR_NONE;
	flags = string_make(parser_getstr(p, "flags"));
	s = strtok(flags, " |");
	while (s) {
		if (grab_flag(a->oflags, OF_SIZE, list_obj_flag_names, s))
			break;
		s = strtok(NULL, " |");
	}
	mem_free(flags);
	return s ? PARSE_ERROR_INVALID_FLAG : PARSE_ERROR_NONE;
}

static enum parser_error parse_ability_obj_flags_off(struct parser *p) {
	struct ability *a = parser_priv(p);
	char *flags;
	char *s;

	if (!a)
		return PARSE_ERROR_MISSING_RECORD_HEADER;
	if (!parser_hasval(p, "flags"))
		return PARSE_ERROR_NONE;
	flags = string_make(parser_getstr(p, "flags"));
	s = strtok(flags, " |");
	while (s) {
		if (grab_flag(a->oflags_off, OF_SIZE, list_obj_flag_names, s))
			break;
		s = strtok(NULL, " |");
	}
	mem_free(flags);
	return s ? PARSE_ERROR_INVALID_FLAG : PARSE_ERROR_NONE;
}

static enum parser_error parse_ability_play_flags(struct parser *p) {
	struct ability *a = parser_priv(p);
	char *flags;
	char *s;

	if (!a)
		return PARSE_ERROR_MISSING_RECORD_HEADER;
	if (!parser_hasval(p, "flags"))
		return PARSE_ERROR_NONE;
	flags = string_make(parser_getstr(p, "flags"));
	s = strtok(flags, " |");
	while (s) {
		if (grab_flag(a->pflags, PF_SIZE, player_info_flags, s))
			break;
		s = strtok(NULL, " |");
	}
	mem_free(flags);
	return s ? PARSE_ERROR_INVALID_FLAG : PARSE_ERROR_NONE;
}

static enum parser_error parse_ability_values(struct parser *p) {
	struct ability *a = parser_priv(p);
	char *s;
	char *t;

	if (!a)
		return PARSE_ERROR_MISSING_RECORD_HEADER;
	s = string_make(parser_getstr(p, "values"));
	t = strtok(s, " |");

	while (t) {
		int value = 0;
		int index = 0;
		bool found = false;
		if (!grab_short_value(a->modifiers, obj_mods, t))
			found = true;
		if (!grab_index_and_int(&value, &index, list_element_names, "RES_", t)) {
			found = true;
			a->el_info[index].res_level = value;
		}
		if (!found)
			break;

		t = strtok(NULL, " |");
	}

	mem_free(s);
	return t ? PARSE_ERROR_INVALID_VALUE : PARSE_ERROR_NONE;
}

static enum parser_error parse_ability_blow(struct parser *p) {
	struct ability *a = parser_priv(p);

	if (!a)
		return PARSE_ERROR_MISSING_RECORD_HEADER;

	a->attacks = mem_realloc(a->attacks, (a->nattacks + 1) * sizeof(a->attacks[0]));

	struct attack *att = &(a->attacks[a->nattacks]);

	att->msg = string_make(parser_getsym(p, "msg"));
	att->damage = parser_getrand(p, "damage");

	/* Read an element name. If present it must be valid - for a blow without an element,
	 * the field must be missing.
	 */
	if (parser_hasval(p, "type")) {
		const char *type = parser_getsym(p, "type");

		if (type == NULL)
			return PARSE_ERROR_UNRECOGNISED_PARAMETER;

		/* Check for a value */
		int val = code_index_in_array(list_element_names, type);
		if (val < 0)
			return PARSE_ERROR_INVALID_VALUE;
		else
			att->element = val;
	} else {
		att->element = -1;
	}

	a->nattacks++;
	return PARSE_ERROR_NONE;
}

static enum parser_error parse_ability_random_effect(struct parser *p) {
	struct ability *a = parser_priv(p);
	assert(a);

	a->effect_randomly = parser_getint(p, "delay");
	return PARSE_ERROR_NONE;
}

static enum parser_error parse_ability_effect(struct parser *p) {
	struct ability *a = parser_priv(p);
	struct effect *effect;
	struct effect *new_effect = mem_zalloc(sizeof(*new_effect));
	assert(a);

	/* Go to the next vacant effect and set it to the new one  */
	if (a->effect) {
		effect = a->effect;
		while (effect->next)
			effect = effect->next;
		effect->next = new_effect;
	} else
		a->effect = new_effect;

	/* Fill in the detail */
	return grab_effect_data(p, new_effect);
	return PARSE_ERROR_NONE;
}


struct parser *init_parse_ability(void) {
	struct parser *p = parser_new();
	parser_setpriv(p, NULL);
	parser_reg(p, "name str name", parse_ability_name);
	parser_reg(p, "stats int str int int int wis int dex int con int chr int spd", parse_ability_stats);
	parser_reg(p, "gain str gain", parse_ability_gain);
	parser_reg(p, "lose str lose", parse_ability_lose);
	parser_reg(p, "brief str brief", parse_ability_brief);
	parser_reg(p, "forbid str forbid", parse_ability_forbid);
	parser_reg(p, "require str require", parse_ability_require);
	parser_reg(p, "desc str desc", parse_ability_desc);
	parser_reg(p, "class str class", parse_ability_class);
	parser_reg(p, "ac int ac", parse_ability_ac);
	parser_reg(p, "tohit int tohit", parse_ability_tohit);
	parser_reg(p, "todam int todam", parse_ability_todam);
	parser_reg(p, "desc_future str desc_future", parse_ability_desc_future);
	parser_reg(p, "cost int cost", parse_ability_cost);
	parser_reg(p, "maxlevel int max", parse_ability_maxlevel);
	parser_reg(p, "minlevel int min", parse_ability_minlevel);
	parser_reg(p, "flag str flag", parse_ability_flag);
	parser_reg(p, "obj-flags ?str flags", parse_ability_obj_flags);
	parser_reg(p, "obj-flags-off ?str flags", parse_ability_obj_flags_off);
	parser_reg(p, "player-flags ?str flags", parse_ability_play_flags);
	parser_reg(p, "values str values", parse_ability_values);
	parser_reg(p, "blow sym msg rand damage ?sym type", parse_ability_blow);
	parser_reg(p, "random-effect int delay", parse_ability_random_effect);
	parser_reg(p, "effect sym eff ?sym type ?int radius ?int other", parse_ability_effect);
	init_parse_magic(p);
	return p;
}

static errr run_parse_ability(struct parser *p) {
	return parse_file_quit_not_found(p, "ability");
}

static errr finish_parse_ability(struct parser *p) {
	parser_destroy(p);
	return 0;
}

static void cleanup_ability(void)
{
	int idx;
	for (idx = 0; idx < PF_MAX; idx++) {
		if (ability[idx]) {
			string_free(ability[idx]->name);
			cleanup_magic(&(ability[idx]->magic));
			mem_free(ability[idx]);
		}
	}
}

struct file_parser ability_parser = {
	"ability",
	init_parse_ability,
	run_parse_ability,
	finish_parse_ability,
	cleanup_ability
};

/* The effect of all abilities combined on a given stat */
int ability_to_stat(int stat) {
	assert(stat < STAT_MAX);
	int bonus = 0;
	for(int i=0; i<PF_MAX; i++) {
		if (ability[i]) {
			if (player_has(player, i)) {
				bonus += ability[i]->a_adj[stat];
			}
		}
	}
	return bonus;
}

/* Forbidden and prerequisite combos */
static bool ability_allowed(unsigned a, bool gain) {
	assert(a < PF_MAX);
	assert(ability[a]);

	/* Is it possible to lose this ability? */
	if (!gain) {
		/* FIXME */
		return true;
	}

	/* Is it forbidden? */
	for(int i=0; i<PF_MAX; i++) {
		if (ability[i]) {
			if (ability[i]->forbid[a]) {
				if (pf_has(player->state.pflags_base, i)) {
					return false;
				}
			}
		}
	}

	/* Does it meet all requirements? */
	for(int i=0; i<PF_MAX; i++) {
		if (ability[i]) {
			if (ability[a]->require[i]) {
				if (!pf_has(player->state.pflags_base, i)) {
					return false;
				}
			}
		}
	}

	/* Check minimum and maximum level and class */
	if (player->lev < ability[a]->minlevel) {
		return false;
	}
	if ((ability[a]->maxlevel) && (player->lev > ability[a]->maxlevel)) {
		return false;
	}
	if ((ability[a]->class) && (!my_stristr(ability[a]->class, player->class->name))) {
		return false;
	}

	return true;
}

/* Return true if it is possible to gain an ability */
static bool can_gain_ability(unsigned a, bool birth) {
	assert(a < PF_MAX);
	assert(ability[a]);

	if (pf_has(player->state.pflags_base, a))
		return false;

	if (!ability_allowed(a, true))
		return false;

	if ((!birth) && (ability[a]->flags & AF_BIRTH))
		return false;

	return true;
}

/* Return true if it is possible to gain a talent. */
static bool can_gain_talent(unsigned a, bool birth) {
	assert(a < PF_MAX);
	assert(ability[a]);

	if (ability[a]->flags & AF_TALENT) {
		if (ability[a]->cost <= player->talent_points) {
			return can_gain_ability(a, birth);
		}
	}

	return false;
}

/* Recalculate everything needed after an ability has been changed */
void changed_abilities(void) {
	/* Update player */
	handle_stuff(player);

	/* Flush messages */
	event_signal(EVENT_MESSAGE_FLUSH);

	/* Update stuff */
	player->upkeep->update |= PU_BONUS | PU_HP | PU_SPELLS | PU_TORCH | PU_UPDATE_VIEW | PU_PANEL | PU_INVEN;

	update_stuff(player);

	/* Update stuff */
	player->upkeep->update |= PU_BONUS | PU_HP | PU_SPELLS | PU_TORCH | PU_UPDATE_VIEW | PU_PANEL | PU_INVEN;
	player->upkeep->redraw |= PR_BASIC | PR_EXTRA | PR_LIGHT | PR_INVEN | PR_EQUIP;

	handle_stuff(player);
}

/* Attempt to gain an ability, returning true if successful.
 * This does not care about talent points & so could be used for mutations, etc.
 * Can fail if already present or blocked.
 */
static bool gain_ability(unsigned a, bool birth) {
	assert(a < PF_MAX);
	assert(ability[a]);

	if (!can_gain_ability(a, birth))
		return false;

	pf_on(player->ability_pflags, a);
	if (ability[a]->gain)
		msg(ability[a]->gain);
	changed_abilities();
	return true;
}

/* Attempt to remove an ability, returning true if successful.
 * Can fail if not already present or blocked.
 */
static bool lose_ability(unsigned a) {
	assert(a < PF_MAX);
	assert(ability[a]);

	if (!pf_has(player->state.pflags_base, a))
		return false;

	if (!ability_allowed(a, false))
		return false;

	pf_off(player->ability_pflags, a);
	if (ability[a]->lose)
		msg(ability[a]->lose);
	changed_abilities();
	return true;
}

/* Attempt to gain a talent, using TP if successful.
 * That's talent points, not toilet paper.
 * Returns true if successfully gained.
 */
static bool gain_talent(int a, bool birth) {
	assert(a < PF_MAX);
	assert(ability[a]);

	if (!can_gain_talent(a, birth))
		return false;

	if (!gain_ability(a, birth))
		return false;

	player->talent_points -= ability[a]->cost;
	return true;
}

/* Attempt to mutate.
 * This may fail (typically only if there is nothing possible to lose or gain - an unlikely
 * situation. But there are special circumstances, such as being an Android.)
 * It may also remove a mutation, especially if you have a lot of mutations already.
 * If it returns true, then something happened (and a message has been printed).
 * 
 * This assumes that mutations can always be safely removed, i.e. that none depend on each
 * other (although forbidding each other is OK).
 */
bool mutate(void) {
	return get_mutation(AF_MUTATION, true);
}

/* Get a mutation meeting the specified flags.
 */
bool get_mutation(unsigned long flags, bool allow_loss)
{
	/* Androids cannot mutate */
	if (player_has(player, PF_NO_MUTATIONS)) {
		return false;
	}

	/* Mutants may be abe to pick and choose */
	bool reject = false;
	if (player_has(player, PF_REROLL_MUTATIONS)) {
		if (player->lev >= 5) {
			int chance = 25 + player->lev;	/* 30% at level 5 rising to 75% at level 50 */
			reject = (randint0(100) < chance);
		}
	}

	/* Are any mutations available to gain or lose? */
	bool ok = false;
	for(int i=0;i<PF_MAX;i++) {
		if (ability[i]) {
			if ((ability[i]->flags & flags) == flags) {
				if ((pf_has(player->state.pflags_base, i) && allow_loss) || (can_gain_ability(i, true))) {
					ok = true;
					break;
				}
			}
		}
	}

	/* No possible choices */
	if (!ok)
		return false;

	/* At least one is available, either to gain or lose. Select randomly. */
	int mut;
	do {
		mut = randint0(PF_MAX);
		if (ability[mut]) {
			if ((ability[mut]->flags & flags) == flags) {
				if ((pf_has(player->state.pflags_base, mut) && allow_loss) || (can_gain_ability(mut, true))) {
					break;
				}
			}
		}
	} while (true);

	if (reject) {
		char buf[256];
		const char *name = ability_name(mut);
		const char *gain = "gain the";
		const char *accept = "accept the mutation";
		const char *reject = "fight the mutation off";
		const char *accit = "accept";
		if (player_has(player, mut)) {
			gain = "lose your";
			accept = "accept losing your mutation";
			reject = "hold on to the mutation";
			accit = "accept losing";
		}
		strnfmt(buf, sizeof(buf), "You feel about to %s %s mutation... %s it? ", gain, name, accit);
		if (!get_check(buf)) {
			msg("You %s.", reject);
			return get_mutation(flags, allow_loss);
		} else {
			msg("You %s.", accept);
		}
	}

	/* If you already have it, remove it - otherwise gain it */
	if (pf_has(player->state.pflags_base, mut))
		lose_ability(mut);
	else
		gain_ability(mut, true);

	return true;
}

/* Return an ability's printable (_ => space) name in a static buffer */
static const char *ability_name(int a) {
	static char buf[32];
	assert(a < PF_MAX);
	assert(ability[a]);
	my_strcpy(buf, ability[a]->name, sizeof(buf));
	for(int i=0;i<(int)strlen(buf);i++) {
		if (buf[i] == '_')
			buf[i] = ' ';
	}
	return buf;
}

/* Initialize player talents from the race/role
 * Stores the initial talent points in player->talent_points, and returns the total per-level TP to gain (to pass
 * to init_talent after birth talents have been selected)
 */
int setup_talents(void) {
	int base = player->race->tp_base + player->extension->tp_base + player->class->tp_base;
	int max = player->race->tp_max + player->extension->tp_max + player->class->tp_max;

	/* If something (such as an extension) has reduced base or max talents below 0, take it
	 * from the other - but never allow negative base or max to get through.
	 */
	if (base < 0) {
		max += base;
		base = 0;
	}
	if (max < 0) {
		base += max;
		max = 0;
	}
	if (base < 0) {
		base = 0;
	}
	if (max < 0) {
		max = 0;
	}

	player->talent_points = base;

	/* These must take all your TP */
	ability[PF_PATIENCE]->cost = ability[PF_UNKNOWN_TALENTS]->cost = player->talent_points;
	
	return max;
}

/* Display the abilities which you have or could gain, with more information
 * on the 'selected' and total number of talent points.
 * Returns whether to 'flip' into race/class ability mode in *flip.
 * Returns the total number of abilities displayed.
 * If 'birth' is set, also display birth-only talents.
 * 		This also has a check on exit with birth-only talents and talent points remaining
 * 		+ different blurb describing colour of birth-only talents...
 *
 * Display format is:
 * <General guff about talents, abilities, etc, including talent points remaining>
 * <line>
 * <List of talents L2R T2B columnar>
 * <line>
 * <Selected talent's full description>
 */
int cmd_abilities(struct player *p, bool birth, int selected, bool *flip) {
	char ntp[64];
	if (flip)
		*flip = false;

	bool leaving = false; 
	int navail;
	do {
		/* Clear the screen, print the unchanging parts */
		Term_clear();

		/* Build the top message */
		const char *tops = "\nThis is a list of all your abilities (including talents and abilities gained through other means, such as mutations), plus any additional talents which you can currently gain. Existing intrinsic abilities are displayed in grey, existing abilities gained through your equipment in blue, existing abilities gained through temporary effects in magenta, gainable talents in green";
		const char *tops_b = ", and talents which can only be gained at character creation in orange";
		if (!birth)
			tops_b = "";
		const char *tops_e = ". The currently selected talent is highlighted. To browse through the list of abilities and see what each does, use the arrow keys. To gain the selected talent, select one and press Space to gain it. To see your race and class abilities, press A. Press Enter to exit. You have";
		if (p->talent_points)
			strnfmt(ntp, sizeof(ntp), "%d", p->talent_points);
		else
			my_strcpy(ntp, "no", sizeof(ntp));
		const char *ntp2 = "s";
		if (p->talent_points == 1)
			ntp2 = "";

		/* Format and output it */
		struct textblock *tb = textblock_new();
		textblock_append_c(tb, COLOUR_L_YELLOW, "%s%s%s %s talent point%s remaining.", tops, tops_b, tops_e, ntp, ntp2);
		size_t *line_starts = NULL;
		size_t *line_lengths = NULL;
		int w, h;
		Term_get_size(&w, &h);
		int top = textblock_calculate_lines(tb, &line_starts, &line_lengths, w) + 2;
		textui_textblock_place(tb, SCREEN_REGION, NULL);
		textblock_free(tb);

		/* Loop: print the changing parts, read the keyboard and act on it until exiting (ESC, return). */
		leaving = false;
		int avail[PF_MAX];
		bool gain[PF_MAX];

		navail = 0;
		/* Scan for abilities */
		for (int i = 0; i < PF_MAX; i++) {
			if (ability[i]) {
				/* Gainable */
				if (can_gain_talent(i, birth)) {
					gain[navail] = true;
					avail[navail++] = i;
				}
			}
		}
		for (int i = 0; i < PF_MAX; i++) {
			if (ability[i]) {
				if (can_gain_talent(i, birth)) {
					/* Don't add - it's already been gained */
				} else if (player_has(p, i)) {
					/* Already has */
					gain[navail] = false;
					avail[navail++] = i;
				}
			}
		}

		/* There is now an array of gainable or already present abilities in avail[]/gain[],
		 * with avail[] having the indices into ability[] and gain[] being true if it
		 * is gainable. Gainables are at the front of the array.
		 * This may be empty. In which case, print a message and exit early.
		 */
		int columns = 1;
		if (navail == 0) {
			c_prt(COLOUR_ORANGE, "You currently have no abilities and can gain no talents. Press any key to exit.", top, 0);
		} else {

			/* Find longest ability name - this (+ 1 blank) is the column size */
			int column_width = 0;
			for (int i = 0; i < navail; i++) {
				if ((int)strlen(ability[avail[i]]->name) > column_width)
					column_width = strlen(ability[avail[i]]->name);
			}
			column_width++;
			columns = w / column_width;
			if (columns > navail)
				columns = navail;
			int last_row_length = navail % columns;
			if (last_row_length == 0)
				last_row_length = columns;

			/* Abilities are now arranged by row and column, the last row usually being incomplete. */

			/* Display grid */
			for (int i = 0; i < navail; i++) {
				int col = (i % columns) * column_width;
				int row = (i / columns) + top;
				int colour;
				if (!gain[i]) {
					if (i == selected) {
						colour = COLOUR_WHITE;
					} else {
						if (pf_has(player->state.pflags_base, avail[i])) {
							/* Intrinsic */
							colour = COLOUR_SLATE;
						} else if (pf_has(player->state.pflags_equip, avail[i])) {
							/* Equipment */
							colour = COLOUR_L_BLUE;
						} else {
							/* Temporary */
							colour = COLOUR_MAGENTA;
						}
					}
				} else {
					if (ability[avail[i]]->flags & AF_BIRTH) {
						if (i == selected) {
							colour = COLOUR_YELLOW;
						} else {
							colour = COLOUR_ORANGE;
						}
					} else {
						if (i == selected) {
							colour = COLOUR_L_GREEN;
						} else {
							colour = COLOUR_GREEN;
						}
					}
				}
				c_prt(colour, ability_name(avail[i]), row, col);
			}

			/* Display info */
			int costco = COLOUR_GREEN;
			if (ability[avail[selected]]->cost < 0)
				costco = COLOUR_RED;

			/* Description */
			tb = textblock_new();
			if (gain[selected])
				textblock_append_c(tb, COLOUR_SLATE, "With this talent %s", ability[avail[selected]]->desc_future ? ability[avail[selected]]->desc_future : ability[avail[selected]]->desc);
			else {
				if (ability[avail[selected]]->desc)
					textblock_append_c(tb, COLOUR_SLATE, "%c%s", toupper(ability[avail[selected]]->desc[0]), ability[avail[selected]]->desc + 1);
			}

			/* Stats, AC, skills, etc */
			for(int i=0;i<STAT_MAX;i++) {
				if (ability[avail[selected]]->a_adj[i]) {
					textblock_append_c(tb, COLOUR_YELLOW, " (%+d to %s)", ability[avail[selected]]->a_adj[i], stat_names[i]);
				}
			}
			if (ability[avail[selected]]->ac)
				textblock_append_c(tb, COLOUR_YELLOW, " (%+d to AC)", ability[avail[selected]]->ac);
			if (ability[avail[selected]]->tohit)
				textblock_append_c(tb, COLOUR_YELLOW, " (%+d to hit)", ability[avail[selected]]->tohit);
			if (ability[avail[selected]]->todam)
				textblock_append_c(tb, COLOUR_YELLOW, " (%+d to damage)", ability[avail[selected]]->todam);
			for(int i=0;i<ELEM_MAX;i++) {
				if (ability[avail[selected]]->el_info[i].res_level) {
					textblock_append_c(tb, COLOUR_YELLOW, " (%+d vs %s)", ability[avail[selected]]->el_info[i].res_level, projections[i].name);
				}
			}
			for(int i=0;i<OBJ_MOD_MAX;i++) {
				if (ability[avail[selected]]->modifiers[i]) {
					char name[256];
					my_strcpy(name, obj_mods[i], sizeof name);
					titlecase(name);
					textblock_append_c(tb, COLOUR_YELLOW, " (%+d to %s)", ability[avail[selected]]->modifiers[i], name);
				}
			}
			for(int i=0;i<PF_MAX;i++) {
				if (pf_has(ability[avail[selected]]->pflags, i)) {
					textblock_append_c(tb, COLOUR_YELLOW, " (%s)", player_info_flags[i]);
				}
			}
			for(int i=0;i<OF_MAX;i++) {
				if (of_has(ability[avail[selected]]->oflags, i)) {
					textblock_append_c(tb, COLOUR_YELLOW, " (%s)", list_obj_flag_names[i]);
				}
			}
		//@ obj, player fs
			/* Cost to gain */
			if (gain[selected])
				textblock_append_c(tb, costco, " Cost: %d", ability[avail[selected]]->cost);

			line_starts = NULL;
			line_lengths = NULL;
			int lines = textblock_calculate_lines(tb, &line_starts, &line_lengths, w);
			region bottom_region = SCREEN_REGION;
			bottom_region.row = h - lines;
			textui_textblock_place(tb, bottom_region, NULL);
			textblock_free(tb);
		}

		/* Redraw */
		Term_redraw();

		/* Read a key */
		struct keypress ch = inkey();
		switch(ch.code) {
			/* Navigate around the grid */
			case '2':
			case KC_PGDOWN:
			case ARROW_DOWN:
			selected += columns;
			if (selected >= navail)
				selected %= columns;
			break;
			case '4':
			case ARROW_LEFT:
			selected--;
			if (selected < 0)
				selected = navail - 1;
			break;
			case '6':
			case ARROW_RIGHT:
			selected++;
			if (selected >= navail)
				selected = 0;
			break;
			case '8':
			case KC_PGUP:
			case ARROW_UP:
			selected -= columns;
			if (selected < 0)
				selected += navail;
			break;

			/* Select */
			case ' ': {
				/* Prompt to confirm.
				 * If not wanted, continue otherwise exit.
				 * (This must attract attention. Use a pop up?)
				 */
				if (gain[selected]) {
					bool ok = true;
					if (!birth) {
						const char *prompt = format("Really permanently gain %s?", ability_name(avail[selected]));
						c_prt(COLOUR_ORANGE, prompt, 0, 0);
						ch = inkey();
						prt("", 0, 0);
						if ((ch.code == ESCAPE) || strchr("Nn", ch.code))
							ok = false;
					}
					if (ok) {
						gain_talent(avail[selected], birth);
						leaving = !birth;
					}
				} else {
					msg("You already have that ability.");
				}
				break;
			}

			/* Flip */
			case 'a':
			case 'A':
			if (flip) {
				*flip = true;
				leaving = true;
			}
			break;

			/* Leave */
			case ESCAPE:
			case 'Q':
			case KC_ENTER:
			leaving = true;
			break;
		}
	} while (!leaving);

	return navail;
}

/* Complete init of player talents (roll out TP-gain levels)
 * This must be done after birth talents (including Patience etc.) have been fixed.
 **/
void init_talent(int tp) {
	int bp = player->race->tp_base + player->class->tp_base;
	memset(player->talent_gain, 0, sizeof(player->talent_gain));

	/* Distribute fairly evenly between level 2 and maximum.
	 * (Or max/2 and maximum for Patience, or level 3 and 90% of max for Unknown)
	 */
	int min = 2;
	int max = PY_MAX_LEVEL - 1;

	/* Patience = more TP, but later */
	if (player_has(player, PF_PATIENCE)) {
		tp += (((bp * 3) + 1) / 2) + 1;
		min = PY_MAX_LEVEL / 2;
		bp = 0;
	}

	/* This doesn't require that all talents *must* be taken immediately */
	if (player_has(player, PF_PRECOCITY)) {
		bp = ((tp * 2) + 2) / 3;
		tp = 0;
	}

	/* Avoid getting TP close to max level for unknown talents */
	if (player_has(player, PF_UNKNOWN_TALENTS)) {
		min++;
		max -= (PY_MAX_LEVEL / 10);
	}

	/* If there are any level-gain TP level, spread them.
	 * FIXME: try to be more equally spaced? (e.g. minimise the difference between pairs, including ends)
	 **/
	for (int i = 0; i < tp; i++)
		player->talent_gain[rand_range(min, max)]++;

	player->talent_points = bp;
}

/* Handle ability gain at level up.
 * Give in the original and new max level - call this only when gaining a level for the first time.
 * If more than one level is gained at once, it should only be called once.
 * Return true if anything noticeable happened.
 **/
bool ability_levelup(struct player *p, int from, int to)
{
	/* For all levels gained, sum TP per level */
	for(int level = from+1; level <= to; level++)
		p->talent_points += p->talent_gain[level-1];

	int gains = p->talent_points;
	if (gains == 0)
		return false;

	/* Gains is now the number of talent points available.
	 * Both random and interactive need to know what talents are available (there could be none)
	 */
	int avail[PF_MAX];
	int navail = 0;

	for (int i = 0; i < PF_MAX; i++) {
		if (ability[i]) {
			if ((ability[i]->flags & AF_TALENT) && (ability[i]->cost <= gains)) {
				if (ability_allowed(i, true)) {
					avail[navail++] = i;
				}
			}
		}
	}

	/* No possible talents to gain? Leave now... */
	if (navail == 0)
		return false;

	/* Unknown Talents:
	 * Nothing 'nasty' or -ve value.
	 * Going for a random talent as soon as it became available would prevent gaining >1-point talents.
	 * It would also be predictable when it happened.
	 * So don't always gain the talent, especially for less valuable ones and at lower levels.
	 * Exception is maximum level - at this point everything must go!
	 */
	if (player_has(p, PF_UNKNOWN_TALENTS)) {
		for(int level = from+1; level <= to; level++) {
			int tal[PF_MAX];
			int ntal = 0;
			for (int i = 0; i < navail; i++) {
				if ((!(ability[avail[i]]->flags & AF_NASTY)) && (ability[avail[i]]->cost > 0)) {
					int bodge = 7; /* 5 => 1 in 6 cost 1, 1 in 3 cost 2 etc. This is scaled so that when you are rapidly gaining levels early you get less, while to avoid hitting
					max level with a lot of TP remaining it will trigger more often. */
					if (level > 12)
						bodge--;

					if (level > 25)
						bodge--;

					if (level > 34)
						bodge--;

					if (level > 41)
						bodge--;

					if (level > 45)
						bodge--;

					if ((level == PY_MAX_LEVEL) || (randint0(ability[avail[i]]->cost + bodge) < ability[avail[i]]->cost))
						tal[ntal++] = avail[i];
				}
			}

			/* No talents available to gain this time around? */
			if (ntal == 0)
				continue;

			if (level == PY_MAX_LEVEL) {
				/* Take as many talents as can be taken, from the most valuable talent
				 * to the least, until out of TP.
				 **/
				for (int j = 0; j < ntal; j++) {
					int best = -1;
					for (int i = 0; i < ntal; i++) {
						if ((!player_has(p, tal[i])) && (ability[best]->cost > ability[tal[i]]->cost))
							best = tal[i];
					}
					if (best >= 0)
						gain_talent(best, false);
				}
			} else {
				/* Take one at random */
				gain_talent(tal[randint0(ntal)], false);
			}
		}
	} else {
		/* Not Unknown Talents. There is no need to force a decision immediately, though. */
		msgt(MSG_LEVEL, "You may now gain new talents. Press Ctrl-T at any time to browse and gain talents.");
	}

	return true;
}
