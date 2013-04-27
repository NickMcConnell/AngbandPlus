/*
 * File: z-msg.c
 * Purpose: Message handling
 *
 * Copyright (c) 2007 Elly, Andrew Sidwell
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *  * Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 *
 *  * Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 */
#include "z-virt.h"
#include "z-term.h"
#include "z-msg.h"

/*
 * The "message memorization" package.
 *
 * Each call to "message_add(s)" will add a new "most recent" message
 * to the "message recall list", using the contents of the string "s".
 *
 * The number of memorized messages is available as "message_num()".
 *
 * Old messages can be retrieved by "message_str(age)", where the "age"
 * of the most recently memorized message is zero, and the oldest "age"
 * which is available is "message_num() - 1".  Messages outside this
 * range are returned as the empty string.
 *
 * The messages are stored in a special manner that maximizes "efficiency",
 * that is, we attempt to maximize the number of semi-sequential messages
 * that can be retrieved, given a limited amount of storage space, without
 * causing the memorization of new messages or the recall of old messages
 * to be too expensive.
 *
 * We keep a buffer of chars to hold the "text" of the messages, more or
 * less in the order they were memorized, and an array of offsets into that
 * buffer, representing the actual messages, but we allow the "text" to be
 * "shared" by two messages with "similar" ages, as long as we never cause
 * sharing to reach too far back in the the buffer.
 *
 * The implementation is complicated by the fact that both the array of
 * offsets, and the buffer itself, are both treated as "circular arrays"
 * for efficiency purposes, but the strings may not be "broken" across
 * the ends of the array.
 *
 * When we want to memorize a new message, we attempt to "reuse" the buffer
 * space by checking for message duplication within the recent messages.
 *
 * Otherwise, if we need more buffer space, we grab a full quarter of the
 * total buffer space at a time, to keep the reclamation code efficient.
 *
 * The "message_add()" function is rather "complex", because it must be
 * extremely efficient, both in space and time, for use with the Borg.
 */

/*
 * Standard sound (and message) names
 */
const char* const angband_sound_name[MSG_MAX] =
{
	"",
	"hit",
	"miss",
	"flee",
	"drop",
	"kill",
	"level",
	"death",
	"study",
	"teleport",
	"shoot",
	"quaff",
	"zap_rod",
	"walk",
	"tpother",
	"hitwall",
	"eat",
	"store1",
	"store2",
	"store3",
	"store4",
	"dig",
	"opendoor",
	"shutdoor",
	"tplevel",
	"bell",
	"nothing_to_open",
	"lockpick_fail",
	"stairs_down", 
	"hitpoint_warn",
	"act_artifact", 
	"use_staff", 
	"destroy", 
	"mon_hit", 
	"mon_touch", 
	"mon_punch", 
	"mon_kick", 
	"mon_claw", 
	"mon_bite", 
	"mon_sting", 
	"mon_butt", 
	"mon_crush", 
	"mon_engulf", 
	"mon_crawl", 
	"mon_drool", 
	"mon_spit", 
	"mon_gaze", 
	"mon_wail", 
	"mon_spore", 
	"mon_beg", 
	"mon_insult", 
	"mon_moan", 
	"recover", 
	"blind", 
	"confused", 
	"poisoned", 
	"afraid", 
	"paralyzed", 
	"drugged", 
	"speed", 
	"slow", 
	"shield", 
	"blessed", 
	"hero", 
	"berserk", 
	"prot_evil", 
	"invuln", 
	"see_invis", 
	"infrared", 
	"res_acid", 
	"res_elec", 
	"res_fire", 
	"res_cold", 
	"res_pois", 
	"stun", 
	"cut", 
	"stairs_up", 
	"store_enter", 
	"store_leave", 
	"store_home", 
	"money1", 
	"money2", 
	"money3", 
	"shoot_hit", 
	"store5", 
	"lockpick", 
	"disarm", 
	"identify_bad", 
	"identify_ego", 
	"identify_art", 
	"breathe_elements", 
	"breathe_frost", 
	"breathe_elec", 
	"breathe_acid", 
	"breathe_gas", 
	"breathe_fire", 
	"breathe_confusion", 
	"breathe_disenchant", 
	"breathe_chaos", 
	"breathe_shards", 
	"breathe_sound", 
	"breathe_light", 
	"breathe_dark", 
	"breathe_nether", 
	"breathe_nexus", 
	"breathe_time", 
	"breathe_inertia", 
	"breathe_gravity", 
	"breathe_plasma", 
	"breathe_force", 
	"summon_monster", 
	"summon_angel", 
	"summon_undead", 
	"summon_animal", 
	"summon_spider", 
	"summon_hound", 
	"summon_hydra", 
	"summon_demon", 
	"summon_dragon", 
	"summon_gr_undead", 
	"summon_gr_dragon", 
	"summon_gr_demon", 
	"summon_ringwraith", 
	"summon_unique", 
	"wield", 
	"cursed", 
	"pseudo_id", 
	"hungry", 
	"notice", 
	"ambient_day", 
	"ambient_nite", 
	"ambient_dng1", 
	"ambient_dng2", 
	"ambient_dng3", 
	"ambient_dng4", 
	"ambient_dng5", 
	"mon_create_trap", 
	"mon_shriek", 
	"mon_cast_fear", 
	"hit_good", 
	"hit_great", 
	"hit_superb", 
	"hit_hi_great", 
	"hit_hi_superb", 
	"cast_spell", 
	"pray_prayer",
	"kill_unique",
	"kill_king",
	"drain_stat",
	"multiply",
};

/* Maximum number of messages to remember */
#define MESSAGE_MAX		2048

typedef struct _message_t
{
	char *str;
	struct _message_t *newer;
	struct _message_t *older;
	u16b type;
	u16b count;
} message_t;

typedef struct _msgcolor_t
{
	u16b type;
	byte color;
	struct _msgcolor_t *next;
} msgcolor_t;

typedef struct _msgqueue_t
{
	message_t *head;
	message_t *tail;
	msgcolor_t *colors;
	u32b count;
} msgqueue_t;

static msgqueue_t *messages = NULL;

/* Functions operating on the entire list */
errr messages_init(void)
{
	messages = ZNEW(msgqueue_t);
	return 0;
}

void messages_free(void)
{
	msgcolor_t *c = messages->colors;
	msgcolor_t *nextc;
	message_t *m = messages->head;
	message_t *nextm;

	while (m)
	{
		nextm = m->older;
		FREE(m->str);
		FREE(m);
		m = nextm;
	}

	while (c)
	{
		nextc = c->next;
		FREE(c);
		c = nextc;
	}

	FREE(messages);
}

u16b messages_num(void)
{
	return messages->count;
}

/* Functions for individual messages */

void message_add(const char *str, u16b type)
{
	message_t *m;

	if (messages->head &&
	    messages->head->type == type &&
	    !strcmp(messages->head->str, str))
	{
    	messages->head->count++;
		return;
	}

	m = ZNEW(message_t);
	m->str = string_make(str);
	m->type = type;
	m->count = 1;
	m->older = messages->head;

	if (messages->head) 
		messages->head->newer = m;

	messages->head = m;
	messages->count++;


	if (!messages->tail) 
		messages->tail = m;

	if (messages->count > MESSAGE_MAX)
	{
		message_t *old_tail = messages->tail;

		messages->tail = old_tail->newer;
		messages->tail->older = NULL;
		FREE(old_tail->str);
		FREE(old_tail);
		messages->count--;
	}
}

static message_t *message_get(u16b age)
{
	message_t *m = messages->head;

	while (m && age--)
		m = m->older;

	return m;
}


const char *message_str(u16b age)
{
	message_t *m = message_get(age);
	return (m ? m->str : "");
}

u16b message_count(u16b age)
{
	message_t *m = message_get(age);
	return (m ? m->count : 0);
}

u16b message_type(u16b age)
{
	message_t *m = message_get(age);
	return (m ? m->type : 0);
}

byte message_color(u16b age)
{
	message_t *m = message_get(age);
	return (m ? message_type_color(m->type) : TERM_WHITE);
}


/* Message-color functions */

errr message_color_define(u16b type, byte color)
{
	msgcolor_t *mc = messages->colors;

	if (!mc)
	{
		messages->colors = ZNEW(msgcolor_t);
		messages->colors->type = type;
		messages->colors->color = color;
		return 0;
	}

	while (mc->next)
	{
		if (mc->type == type)
		{
			mc->color = color;
			return 0;
		}
		mc = mc->next;
	}

	mc->next = ZNEW(msgcolor_t);
	mc->next->type = type;
	mc->next->color = color;

	return 0;
}

byte message_type_color(u16b type)
{
	byte color = TERM_WHITE;
	if (messages)
		{
		msgcolor_t *mc = messages->colors;

		while (mc && mc->type != type)
			mc = mc->next;

		if (mc && (mc->color != TERM_DARK))
			color = mc->color;
		};
	return color;
}
