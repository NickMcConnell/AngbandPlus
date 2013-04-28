/* File: sound.c */

/* Purpose: sound management and script commands */

/*
 * Copyright (c) 1997-2009 Tim Baker
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include <tcl.h>
#include "angband.h"
#include "tnb.h"
#include "interp.h"
#include "cmdinfo-dll.h"
#include "sound-dll.h"
#include "util-dll.h"
#include "stubs.h"

#define SND_MSG_COLORS 1

cptr keyword_sound_event[] = {
"action_fail",
"store_enter",
"store_leave",
"store_purchase",
"store_sell",
"store_sell_good",
"store_sell_bad",
"home_enter",
"home_leave",
"activate",
"use_food",
"use_potion",
"use_rod",
"use_scroll",
"use_staff",
"use_wand",
"use_fuel",
"use_spike",
"spell_cast",
"spell_learn",
"spell_ball",
"spell_beam",
"spell_bolt",
"spell_teleport",
"spell_destruction",
"spell_earthquake",
"spell_shatter",
#if defined(OANGBANDTK)
"monster_trap_set",
"monster_trap_hit",
#endif
"disarm_trap",
"pick_lock",
"open",
"close",
"pack_drop",
"pack_pickup",
"pack_remove",
"pack_wear",
"pack_destroy",
"pack_overflow",
"pack_steal",
"pack_disenchant",
"pack_damage_acid",
"pack_drain",
"amnesia",
"state_blind",
"state_confused",
"state_cut",
"state_stun",
"state_poisoned",
"state_paralyzed",
"state_afraid",
"state_hallucinate",
"state_hungry",
"state_hungry_end",
"state_full",
"state_full_end",
"state_gorged",
"state_gorged_end",
"state_fast",
"state_slow",
"state_shield",
"state_blessed",
"state_hero",
"state_berserk",
"state_protection_evil",
"state_invulnerable",
"state_see_invisible",
"state_infravision",
"state_resist_acid",
"state_resist_lightning",
"state_resist_fire",
"state_resist_cold",
"state_resist_poison",
"state_recover",
"state_change",
"attack_hit",
"attack_miss",
"attack_good",
"attack_great",
"attack_superb",
"attack_great2",
"attack_superb2",
"attack_bow",
"attack_throw",
"bash",
"missile_hit",
"missile_miss",
"tunnel_rubble",
"tunnel_magma",
"tunnel_quartz",
"tunnel_granite",
"tunnel_permanent",
"tunnel_door",
"stat_drain",
"stat_restore",
"stat_gain",
"experience_drain",
"experience_restore",
"experience_level_up",
"experience_level_down",
"trap_miss",
"trap_door",
"trap_pit_open",
"trap_pit_spiked",
"trap_pit_poison",
"trap_rune_summon",
"trap_rune_teleport",
"trap_spot_fire",
"trap_spot_acid",
"trap_dart_slow",
"trap_dart_strength",
"trap_dart_dexterity",
"trap_dart_constitution",
"trap_gas_blind",
"trap_gas_confuse",
"trap_gas_poison",
"trap_gas_sleep",
"money",
"money200",
"money600",
"monster_death",
"monster_death_unique",
"monster_kill_other",
"monster_multiply",
"monster_teleport",
"monster_pickup",
"break_door",
"target_set",
"walk",
#if defined(KANGBANDTK) || defined(OANGBANDTK) || defined(ZANGBANDTK)
"walk_water",
#endif
#if defined(KANGBANDTK) || defined(ZANGBANDTK)
"drown_in_water",
"burned_by_lava",
#endif
"depth_up",
"depth_down",
"depth_jump",
"rest",
"discover",
"disturb",
"disturb_unique",
"ident_worthless",
"ident_artifact",
"ident_ego",
"object_destroyed",
"object_auto_destroyed",
"warn_hitpoints",
"character_death",
"character_win",
"character_retire",
"town_day",
"town_night",
#if defined(ANGBANDTK) || defined(OANGBANDTK)
"dungeon",
#endif
#if defined(KANGBANDTK)
"dungeon",
"quest_done",
#endif /* */
#if defined(ZANGBANDTK)
"wild_day",
"wild_night",
"dungeon",
"quest_done",
"mutate",
"lose_mutation",
#endif /* ZANGBANDTK */
NULL
};

#if defined(OANGBANDTK)

cptr keyword_monster_spell[32 * 4] = {
	/* flags4 */
	"SHRIEK",
	"LASH",
	"BOULDER",
	"SHOT",
	"ARROW",
	"BOLT",
	"MISSL",
	"PMISSL",
	"BR_ACID",
	"BR_ELEC",
	"BR_FIRE",
	"BR_COLD",
	"BR_POIS",
	"BR_PLAS",
	"BR_LITE",
	"BR_DARK",
	"BR_CONF",
	"BR_SOUN",
	"BR_SHAR",
	"BR_INER",
	"BR_GRAV",
	"BR_WALL",
	"BR_NEXU",
	"BR_NETH",
	"BR_CHAO",
	"BR_DISE",
	"BR_TIME",
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,

	/* flags5 */
	"BA_ACID",
	"BA_ELEC",
	"BA_FIRE",
	"BA_COLD",
	"BA_POIS",
	"BA_LITE",
	"BA_DARK",
	"BA_CONF",
	"BA_SOUN",
	"BA_SHAR",
	"BA_WATE",
	"BA_NETH",
	"BA_CHAO",
	"BA_MANA",
	NULL,
	NULL,
	"BO_ACID",
	"BO_ELEC",
	"BO_FIRE",
	"BO_COLD",
	"BO_POIS",
	"BO_PLAS",
	"BO_ICE",
	"BO_WATE",
	"BO_NETH",
	"BO_MANA",
	NULL,
	"BEAM_ELEC",
	"BEAM_ICE",
	"BEAM_NETH",
	"ARC_HFIRE",
	"ARC_FORCE",

	/* flags6 */
	"HASTE",
	"ADD_MANA",
	"HEAL",
	"CURE",
	"BLINK",
	"TPORT",
	NULL,
	"TELE_SELF_TO",
	"TELE_TO",
	"TELE_AWAY",
	"TELE_LEVEL",
	NULL,
	"DARKNESS",
	"TRAPS",
	"FORGET",
	"DRAIN_MANA",
	NULL,
	NULL,
	"MIND_BLAST",
	"BRAIN_SMASH",
	"WOUND",
	NULL,
	NULL,
	NULL,
	NULL,
	"HUNGER",
	NULL,
	"SCARE",
	"BLIND",
	"CONF",
	"SLOW",
	"HOLD",

	/* flags7 */
	"S_KIN",
	NULL,
	NULL,
	"S_MONSTER",
	"S_MONSTERS",
	NULL,
	NULL,
	NULL,
	"S_ANT",
	"S_SPIDER",
	"S_HOUND",
	"S_ANIMAL",
	NULL,
	NULL,
	"S_THIEF",
	"S_BERTBILLTOM",
	NULL,
	NULL,
	NULL,
	NULL,
	"S_DRAGON",
	"S_HI_DRAGON",
	NULL,
	NULL,
	"S_DEMON",
	"S_HI_DEMON",
	NULL,
	NULL,
	"S_UNDEAD",
	"S_HI_UNDEAD",
	"S_WRAITH",
	"S_UNIQUE",
};

cptr keyword_sound_martial_art[] = {
"punch",
"kick",
"knee",
"chop",
"uppercut",
"boot",
"bang_on",
"slam",
"grapple_with",
"hammer",
"head_butt",
"strangle",
"roundhouse_kick",
"assault",
"crush",
"double_kick",
"thunderclap_belt",
"blizzard_gouge",
"tsunami_whirl",
"stormwind_chop",
NULL
};

#endif /* OANGBANDTK */

#if defined(ZANGBANDTK)

cptr keyword_sound_martial_art[] = {
"punch",
"kick",
"strike",
"knee",
"elbow",
"butt",
"kick2",
"uppercut",
"double_kick",
"cats_claw",
"jump_kick",
"eagles_claw",
"circle_kick",
"iron_fist",
"flying_kick",
"dragon_fist",
"crushing_blow",
NULL
};

#endif /* ZANGBANDTK */

typedef struct t_sound_dir t_sound_dir;
struct t_sound_dir
{
	int id;
	char *extPath;
	char *utfPath;
};
t_sound_dir *g_sound_dir = NULL; /* List of directories */
int g_sound_dir_count = 0; /* Number of directories */
int g_sound_dir_id = 0; /* Unique id */

typedef int SndDirId;

/* A single sound assignment */
typedef struct t_sound t_sound;
struct t_sound
{
	SndDirId dirId; /* Directory identifier (int) */
	SoundId sndId; /* Sound identifier (char *) */
};

/* A sound list holds a list of sounds assigned to an event */
typedef struct t_sound_list t_sound_list;
struct t_sound_list
{
	t_sound *sound;
	int count;
#define SND_STATIC_SPACE 3
	t_sound space[SND_STATIC_SPACE];
};

/* A sound configuration holds sound assignments for each group */
typedef struct t_sound_config t_sound_config;
struct t_sound_config
{
	int id; /* Unique id */
	t_sound_list *group[SNDGRP_MAX]; /* List of sound-lists for each event */
	int active;
	char *utfTitle; /* Human-readable name */
	char *utfFile; /* File name */
};

t_sound_config *g_sound_config = NULL; /* Array of configurations */
int g_sound_config_count = 0; /* Number of configurations */
int g_sound_config_id = 0; /* Unique id */

/*
 * Keep a list of recently-played sounds so the user
 * can find out why that weird noise was made.
 */
typedef struct t_sound_hist t_sound_hist;
struct t_sound_hist
{
	t_sound sound;
	int config, group, event;
};
#define SND_HIST_MAX 200
static t_sound_hist s_sound_hist[SND_HIST_MAX];
int s_sound_hist_head = 0, s_sound_hist_tail = 0;

typedef struct t_sound_group t_sound_group;
struct t_sound_group
{
	Tcl_HashTable hash;
	const char **keyword;
#ifdef SND_MSG_COLORS
	int *colors;
#endif
	int count;
};
t_sound_group g_sound_group[SNDGRP_MAX];

cptr keyword_sound_group[] = {
	"event", "monster", "monster_spell", "monster_attack",
#if defined(OANGBANDTK) || defined(ZANGBANDTK)
	"martial_art",
#endif /* */
	NULL
};

typedef struct t_sound_channel t_sound_channel;
struct t_sound_channel
{
	bool free; /* Is channel free? */
	bool purge; /* Purge when done playing */
	int index; /* Self-pointer into s_channel array */
	t_sound sound; /* Sound identifier */
	SoundData snd_data; /* Sound data from sound.dll */
#ifdef SOUND3D
	int m_idx; /* Monster whose sound is playing */
#endif
};

static bool s_sound_init = FALSE; /* TRUE if sound_init() completed */

static t_sound_hist s_sound_to_play;
static bool s_sound_no_dups = FALSE, s_sound_purge = TRUE;
static t_sound_channel s_channel[MAX_SND_CHANNELS];
static SoundId s_null_sound = "";
static u32b s_rand_sound;
static int s_sound_volume = 100;

/* Temporary array to collect sounds to play for an event */
#define SND_PLAYLIST_MAX 50
static t_sound *s_play_list[SND_PLAYLIST_MAX];

/* Keep track of when to play the sound */
static u32b *monster_sound_times = NULL;

#define RBM_MAX 25
extern cptr r_info_blow_method[];

SoundStubs *soundStubsPtr = NULL;

/* Convert sound names to unique id */
static Tcl_HashTable s_sound_id_table;

#ifdef SOUND3D
bool sound_3D = FALSE;
int sound_x, sound_y;
int sound_m_idx;
int sound_3D_count = 0;
#endif

/*
 * sound_done --
 *
 * A sound has finished playing. If the same sound isn't playing on
 * another channel, then the sound data is freed. In any case, the
 * channel is marked free.
 */
void sound_done(int chan)
{
	int i;
	t_sound_channel *info = &s_channel[chan];

	/* See if the sound is playing on another channel */
	for (i = 0; i < MAX_SND_CHANNELS; ++i)
	{
		/* Skip this channel */
		if (i == chan) continue;

		/* Other channel is not playing a sound */
		if (s_channel[i].free) continue;

		/* Other channel is playing our sound */
		if (s_channel[i].snd_data == info->snd_data) break;
	}

	/* If the sound is not playing on another channel, free it */
	if (i == MAX_SND_CHANNELS)
	{
		sound_hook_free_data(info->snd_data, chan);

		/* XXX Unlock/Purge sound data here */
		if (info->purge) ;
	}

	/* Free the channel */
	sound_hook_free_channel(chan);

	/* This channel is now free */
	info->free = TRUE;

#ifdef SOUND3D
	if (info->m_idx != -1)
	{
		info->m_idx = -1;
		sound_3D_count--;
	}
#endif
}

SoundId sound_id(cptr string)
{
    int dummy;

	/* We are allowed to assign the "null" sound */
	if (!string[0]) return s_null_sound;

	return (SoundId) Tcl_GetHashKey(&s_sound_id_table,
		Tcl_CreateHashEntry(&s_sound_id_table, string, &dummy));
}

void sound_play_aux(SoundData snd_data, t_sound snd, int chan)
{
	t_sound_channel *info = &s_channel[chan];

	/* Find an available channel */
	if (chan == -1)
	{
		int i;
		
		info = s_channel;
		
		for (i = 0; i < MAX_SND_CHANNELS; ++i, ++info)
		{
			if (info->free)
			{
				chan = i;
				break;
			}
		}

		/* No free channels. Can't play sound */
		if (chan == -1) return;
	}

	/* XXX The channel index must be passed along with the sound data */

	info->free = FALSE;
	info->index = chan;
	info->sound = snd;
	info->snd_data = snd_data;
	info->purge = s_sound_purge;

	/* XXX Cancel any playing sound on this channel */

	/* XXX Begin playing the new sound on this channel */

	sound_hook_play(snd_data, chan);

#ifdef SOUND3D
	if (sound_3D)
	{
		soundStubsPtr->position(chan, sound_x, sound_y);
		info->m_idx = sound_m_idx;
		sound_3D_count++;
	}
	else
	{
		soundStubsPtr->position(chan, -1, -1);
		info->m_idx = -1;
	}
#endif
}

char *sound_to_path(char *buf, t_sound *sndPtr)
{
	int i;

	if (sndPtr->sndId == s_null_sound)
	{
		return strcpy(buf, "no-such-sound");
	}
	for (i = 0; i < g_sound_dir_count; i++)
	{
		if (g_sound_dir[i].id == sndPtr->dirId)
		{
			if (g_sound_dir[i].extPath == NULL)
				return strcpy(buf, "no-such-sound");
			(void) sprintf(buf, "%s%s%s", g_sound_dir[i].extPath, PATH_SEP,
				sndPtr->sndId);
			return buf;
		}
	}
	return strcpy(buf, "no-such-sound");
}

void sound_play(t_sound snd)
{
	int i, free;
	bool no_dups = s_sound_no_dups;
	SoundData snd_data = 0;
	t_sound_channel	*info = &s_channel[0];
	
	/* Cancel no duplicates */
	s_sound_no_dups = FALSE;

	/* No sounds! */
	if (!use_sound) return;

	/* Check for no sound */
	if (snd.sndId == s_null_sound) return;

	/*
	 * On a system that doesn't tell us when a sound is finished, we must
	 * manually check each channel to free up resources.
	 */
	sound_hook_purge();

	/* Assume no free channels */
	free = -1;
	
	/* Check every channel */
	for (i = 0; i < MAX_SND_CHANNELS; ++i, ++info)
	{
		/* This is a free channel */
		if (info->free)
		{
			if (free == -1) free = i;
			continue;
		}
		
		/* Already playing it */
		if ((info->sound.dirId == snd.dirId) &&
			(info->sound.sndId == snd.sndId))
		{
			/* Sometimes don't play a sound more than once */
			if (no_dups) return;
			
			/* Efficiency -- Reuse the sound data */
			if (soundStubsPtr->shareData)
			{
				snd_data = info->snd_data;
			}
		}
	}
	
	/* No free channels */
	if (free == -1) return;

	/* Sometimes get the sound */
	if (!snd_data)
	{
		char path[1024];

		/* Build the pathname for this sound */
		sound_to_path(path, &snd);

		/* Load the sound file or resource here */
		snd_data = sound_hook_load(path, free);

		/* Error, or doesn't exist */
		if (!snd_data) return;
	}
	
	/* Play the sound */
	sound_play_aux(snd_data, snd, free);
}

void sound(int group, int index, int mode)
{
	int i, cfg, cnt;
	t_sound_list *sndList;
	t_sound *sndPtr;
	int cfgList[SND_PLAYLIST_MAX];
	
	/* No sounds! */
	if (!use_sound) return;

	/* No sound configurations */
	if (!g_sound_config_count) return;

	/* Collect sounds from every active configuration */
	cnt = 0;
	for (cfg = 0; cfg < g_sound_config_count; cfg++)
	{
		if (!g_sound_config[cfg].active) continue;
		sndList = &g_sound_config[cfg].group[group][index];
		for (i = 0; i < sndList->count; i++)
		{
			s_play_list[cnt] = &sndList->sound[i];
			cfgList[cnt] = cfg;
			++cnt;
		}
	}

	/*
	 * Do absolutely nothing if the assignment has no sound.
	 * This is to fix a bug where we use a staff of Dispel Evil,
	 * and a monster dies, which plays SND_MONSTER_KILLED which is
	 * zero, which results in no sound being played.
	 *
	 * This also allows us to sound() a general sound, followed
	 * by a sound() of a more specific sound. See sense_inventory()
	 * in dungeon.c.
	 */
	if (!cnt) return;

	/*
	 * Get sound identifier. Currently I pick a random sound. In
	 * the future I might add "mode" flag to allow sounds to be
	 * played in order.
	 */
	if (cnt == 1)
	{
		i = 0;
	}
	else
	{
		Rand_quick = TRUE;
		Rand_value = s_rand_sound;
		i = rand_int(cnt);
		s_rand_sound = Rand_value;
		Rand_quick = FALSE;
	}
	sndPtr = s_play_list[i];

	/* We are allowed to assign the "null" sound */
	if (sndPtr->sndId == s_null_sound) return;

	/* The sound to play */
	s_sound_to_play.sound = *sndPtr;
	s_sound_to_play.config = g_sound_config[cfgList[i]].id;
	s_sound_to_play.group = group;
	s_sound_to_play.event = index;

	/* Sometimes don't over-play sound */
	if (mode & 0x01) s_sound_no_dups = TRUE;
	
	/* Sometimes play immediately */
	if (mode & 0x02) sound_flush();
}

void sound_flush(void)
{
	if (!use_sound) return;

	if (s_sound_to_play.sound.sndId == s_null_sound) return;

	s_sound_hist[s_sound_hist_head++] = s_sound_to_play;
	if (s_sound_hist_head == SND_HIST_MAX)
		s_sound_hist_head = 0;
	if (s_sound_hist_head == s_sound_hist_tail)
	{
		++s_sound_hist_tail;
		if (s_sound_hist_tail == SND_HIST_MAX)
			s_sound_hist_tail = 0;
	}

	/* Play the sound */
	sound_play(s_sound_to_play.sound);

	/* Forget the sound */
	s_sound_to_play.sound.sndId = s_null_sound;
}

void sound_cancel(void)
{
	/* Forget the sound */
	s_sound_to_play.sound.sndId = s_null_sound;
	s_sound_no_dups = FALSE;
}

int sound_playing(void)
{
	int	i, count = 0;
	
	/* Check each channel. */
	for (i = 0; i < MAX_SND_CHANNELS; ++i)
	{
		/* We found a busy channel. */
		if (s_channel[i].free == FALSE) count++;
	}
	
	/* Return count of busy channels */
	return count;
}

void sound_stop(int group, int index)
{
	t_sound_list *sndList;
	int cfg, cnt, i, j;

	cnt = 0;
	for (cfg = 0; cfg < g_sound_config_count; cfg++)
	{
		if (!g_sound_config[cfg].active) continue;
		sndList = &g_sound_config[cfg].group[group][index];
		for (i = 0; i < sndList->count; i++)
			s_play_list[cnt++] = &sndList->sound[i];
	}

	/* No sound is assigned */
	if (!cnt) return;

	/* Stop the sound (on all channels) */
	for (i = 0; i < MAX_SND_CHANNELS; ++i)
	{
		t_sound *chanSndPtr = &s_channel[i].sound;

		if (s_channel[i].free) continue;

		for (j = 0; j < cnt; j++)
		{
			t_sound *sndPtr = s_play_list[j];
			
			if ((chanSndPtr->dirId == sndPtr->dirId) &&
				(chanSndPtr->sndId == sndPtr->sndId))
			{
				sound_hook_stop(s_channel[i].snd_data, i);
				break;
			}
		}
	}
}

/*
 * Save sound identifiers played with messages.
 */
static t_sound_hist message__snd[MESSAGE_MAX];

void sound_message(s16b x)
{
	if (x < 0 || x >= MESSAGE_MAX) return;
	message__snd[x] = s_sound_to_play;
}

/*
 * Return the "sound" associated with a saved message
 */
void message_snd(Tcl_Interp *interp, s16b age)
{
	char buf[24];
	s16b x;
	Tcl_DString dString;
	t_sound *sndPtr;

	/* Forgotten messages have no sound */
	if ((age < 0) || (age >= message_num())) return;

	/* Acquire the "logical" index */
	x = (message__next + MESSAGE_MAX - (age + 1)) % MESSAGE_MAX;

	/* Return the sound identifier */
	sndPtr = &message__snd[x].sound;

	if (sndPtr->sndId == s_null_sound) return;

	Tcl_DStringInit(&dString);
	(void) sprintf(buf, "snddir%d", sndPtr->dirId);
	Tcl_DStringAppendElement(&dString, buf);
	Tcl_DStringAppendElement(&dString, sndPtr->sndId);
	Tcl_DStringResult(interp, &dString);
}

void sound_message_play(s16b age)
{
	s16b x;
	t_sound *sndPtr;

	/* Forgotten messages have no sound */
	if ((age < 0) || (age >= message_num())) return;

	/* Acquire the "logical" index */
	x = (message__next + MESSAGE_MAX - (age + 1)) % MESSAGE_MAX;

	/* Return the sound identifier */
	sndPtr = &message__snd[x].sound;

	if (sndPtr->sndId == s_null_sound) return;

	sound_play(*sndPtr);
}

#ifdef SND_MSG_COLORS
int message_snd_color(s16b age)
{
	s16b x;
	t_sound_hist *sndHistPtr;

	if (!s_sound_init)
		return TERM_WHITE;

	/* Forgotten messages have no sound */
	if ((age < 0) || (age >= message_num()))
		return TERM_WHITE;

	/* Acquire the "logical" index */
	x = (message__next + MESSAGE_MAX - (age + 1)) % MESSAGE_MAX;

	sndHistPtr = &message__snd[x];

	/* FIXME: Should be able to assign a color without a sound */
	if (sndHistPtr->sound.sndId == s_null_sound)
		return TERM_WHITE;

	return g_sound_group[sndHistPtr->group].colors[sndHistPtr->event];
}
#endif /* SND_MSG_COLORS */

/*
 * How monster sounds work --
 * When a monster comes into view (easy visibility), then we
 * request that the sound for that monster race be played.
 * In order to avoid overplaying the sound for a monster,
 * the sound will only play after g_monster_delay seconds have
 * passed since the previous playing. The sound always plays
 * the first time the monster is encountered on a level.
 *
 * Oh yeah. The sound isn't played for a sleeping monster.
 * But if a monster (in view) wakes up, the sound plays (if
 * g_monster_delay turns have passed).
 */

int g_monster_delay = 120;

void sound_monster_reset(void)
{
	int i;

	/* Always play the first time we see the monster */
	for (i = 0; i < MAX_R_IDX; i++)
	{
		monster_sound_times[i] = 0L;
	}
}

#ifdef SOUND3D

void sound_monster(int m_idx)
{
	u32b sec;
	monster_type *m_ptr = &m_list[m_idx];
	int r_idx = m_ptr->r_idx;

	if (!g_monster_delay) return;

	sec = Milliseconds() / 1000;

	/* Only play the sound once every g_monster_delay seconds */
	if ((sec - monster_sound_times[r_idx]) < g_monster_delay) return;

	/* Won't play again for g_monster_delay seconds */
	monster_sound_times[r_idx] = sec;

	sound_3D = TRUE;
	sound_x = m_ptr->fx;
	sound_y = m_ptr->fy;
	sound_m_idx = m_idx;

	/* Play the sound */
	sound(SNDGRP_MONSTER, r_idx, 0x02);

	sound_3D = FALSE;
}

void sound_position(int who)
{
	if (!sound_3D_count) return;

	if (who == -1)
		soundStubsPtr->position(-1, p_ptr_px, p_ptr_py);
	else
	{
		int i;

		for (i = 0; i < MAX_SND_CHANNELS; i++)
		{
			if (s_channel[i].free) continue;
			if (s_channel[i].m_idx != who) continue;
			soundStubsPtr->position(i, m_list[who].fx, m_list[who].fy);
			break;
		}
	}
}
#else
void sound_monster(int r_idx)
{
	u32b sec;

	if (!g_monster_delay) return;

	sec = Milliseconds() / 1000;

	/* Only play the sound once every g_monster_delay seconds */
	if ((sec - monster_sound_times[r_idx]) < g_monster_delay) return;

	/* Won't play again for g_monster_delay seconds */
	monster_sound_times[r_idx] = sec;

	/* Play the sound */
	sound(SNDGRP_MONSTER, r_idx, 0x02);
}
#endif

/*
 * Ambient dungeon sounds play every g_ambient_delay seconds.
 */
int g_ambient_delay = 120;

void sound_load_dll(char *dllName)
{
	ShLibRef shLib;
	struct stub {
		int (*Sound_Init)(SoundStubs **stubsPtrPtr, SoundDoneProc doneProc);
	} stub;
	t_stub init[] = {
		{STUB_DESC(Sound_Init, struct stub)},
		{NULL, 0, 0}
	};

	shLib = ShLib_Load(dllName);
	if (shLib == NULL)
		quit_fmt("error loading %s\n%s", dllName, ShLib_Error());

	if (Stub_Load(shLib, init, &stub) == -1)
	{
		quit_fmt("%s: %s", dllName, Stub_Error());
	}
	if ((*stub.Sound_Init)(&soundStubsPtr, sound_done))
		quit("Sound_Init() returned an error");
}

#ifdef TNB_SOUND

static void GetWaveOutDevice(void)
{
	WAVEOUTCAPS caps;
	Tcl_Obj *listObjPtr;
	int i, n;

	if ((n = waveOutGetNumDevs()) == 0)
	{
		return;
	}

	listObjPtr = Tcl_NewListObj(0, NULL);

	for (i = 0; i < n; i++)
	{
		UINT rval;
		if ((rval = waveOutGetDevCaps((UINT)i, &caps, sizeof(WAVEOUTCAPS))))
		{
			continue;
		}
		Tcl_ListObjAppendElement(g_interp, listObjPtr,
			Tcl_NewStringObj(caps.szPname, -1));
	}

	Tcl_SetObjResult(g_interp, listObjPtr);
}

static void GetWaveOutCaps(int d)
{
	WAVEOUTCAPS caps;
	Tcl_Obj *listObjPtr;
	int j;

	char *format[12] = {
		"11 Mono 8", "11 Stereo 8", "11 Mono 16", "11 Stereo 16",
		"22 Mono 8", "22 Stereo 8", "22 Mono 16", "22 Stereo 16",
		"44 Mono 8", "44 Stereo 8", "44 Mono 16", "44 Stereo 16"
	};
		
	if (waveOutGetDevCaps((UINT) d, &caps, sizeof(WAVEOUTCAPS)))
	{
		return;
	}

	listObjPtr = Tcl_NewListObj(0, NULL);

	for (j = 0; j < 12; j++)
	{
		if (!(caps.dwFormats & (1 << j))) continue;
		Tcl_ListObjAppendElement(g_interp, listObjPtr,
			Tcl_NewStringObj(format[j], -1));
	}

	Tcl_SetObjResult(g_interp, listObjPtr);
}

static void GetWaveOutSupport(int d)
{
	WAVEOUTCAPS caps;
	Tcl_Obj *listObjPtr;
	int j;

	char *support[7] = {
		"Pitch", "Playback Rate", "Volume", "Left/Right Volume",
		"Sync", "Sample Accurate", "Direct Sound"
	};
		
	if (waveOutGetDevCaps((UINT) d, &caps, sizeof(WAVEOUTCAPS)))
	{
		return;
	}

	listObjPtr = Tcl_NewListObj(0, NULL);

	for (j = 0; j < 7; j++)
	{
		if (!(caps.dwSupport & (1 << j))) continue;
		Tcl_ListObjAppendElement(g_interp, listObjPtr,
			Tcl_NewStringObj(support[j], -1));
	}

	{
		char buf[64];
		DWORD vol;
		(void) waveOutGetVolume(d, &vol);
		if ((caps.dwSupport & WAVECAPS_LRVOLUME) == WAVECAPS_LRVOLUME)
			(void) sprintf(buf, "volume is %d/%d", LOWORD(vol), HIWORD(vol));
		else
			(void) sprintf(buf, "volume is %d", LOWORD(vol));
		Tcl_ListObjAppendElement(g_interp, listObjPtr,
			Tcl_NewStringObj(buf, -1));
	}

	Tcl_SetObjResult(g_interp, listObjPtr);
}

#endif /* TNB_SOUND */

static int ConfigFromObj(Tcl_Interp *interp, Tcl_Obj *objPtr, int *index)
{
	char *t = Tcl_GetString(objPtr);
	int i, id;

	if (sscanf(t, "sndcfg%d", &id) != 1)
	{
		goto error;
	}
	for (i = 0; i < g_sound_config_count; i++)
	{
		if (g_sound_config[i].id == id)
		{
			(*index) = i;
			return TCL_OK;
		}
	}
error:
	FormatResult(interp, "invalid sound configuration \"%s\"", t);
	return TCL_ERROR;
}

static int DirFromObj(Tcl_Interp *interp, Tcl_Obj *objPtr, int *index)
{
	char *t = Tcl_GetString(objPtr);
	int i, id;

	if (sscanf(t, "snddir%d", &id) != 1)
	{
		goto error;
	}
	for (i = 0; i < g_sound_dir_count; i++)
	{
		if (g_sound_dir[i].id == id)
		{
			(*index) = i;
			return TCL_OK;
		}
	}
error:
	FormatResult(interp, "invalid sound directory \"%s\"", t);
	return TCL_ERROR;
}

static int GroupFromObj(Tcl_Interp *interp, Tcl_Obj *objPtr, int *index)
{
	return Tcl_GetIndexFromObj(interp, objPtr, keyword_sound_group,
		"sound group", 0, index);
}

static int ValidateKeyword(Tcl_Interp *interp, int group, char *keyword, int *index)
{
	Tcl_HashEntry *hPtr;

	if ((hPtr = Tcl_FindHashEntry(&g_sound_group[group].hash, keyword)) == NULL)
	{
		/* Set the error */
		FormatResult(interp, "unknown keyword \"%s\"", keyword);

		/* Failure */
		return TCL_ERROR;
	}

	/* Return the sound index */
	(*index) = (int) Tcl_GetHashValue(hPtr);

	/* Success */
	return TCL_OK;
}

static int KeywordFromObj(Tcl_Interp *interp, int group, Tcl_Obj *objPtr, int *index)
{
	return ValidateKeyword(interp, group, Tcl_GetString(objPtr), index);
}

static int ValidateIndex(Tcl_Interp *interp, t_sound_list *sndList, int index)
{
	if ((index < 0) || (index >= sndList->count))
	{
		/* Set the error */
		FormatResult(interp, "bad sound index \"%d\": "
			"must be from 0 to %d", index, sndList->count - 1);

		/* Failure */
		return TCL_ERROR;
	}

	/* Success */
	return TCL_OK;
}

static int ConfigConfigure(Tcl_Interp *interp, t_sound_config *cfgPtr, int objc, Tcl_Obj *CONST objv[])
{
	static CONST char *optionStr[] = {"-active", "-file", "-title", NULL};
	int i = 0, option;

	if (objc & 1)
	{
		return TCL_ERROR;
	}
	while (objc > 1)
	{
		if (Tcl_GetIndexFromObj(interp, objv[i], optionStr,
			"option", 0, &option) != TCL_OK)
		{
			return TCL_ERROR;
		}
		switch (option)
		{
			case 0: /* -active */
			{
				int active;
				if (Tcl_GetBooleanFromObj(interp, objv[i + 1], &active)
					!= TCL_OK)
				{
					return TCL_ERROR;
				}
				cfgPtr->active = active;
				break;
			}
			
			case 1: /* -file */
			{
				int len;
				char *t = Tcl_GetStringFromObj(objv[i + 1], &len);
				if (cfgPtr->utfFile)
					Tcl_FreeDebug(cfgPtr->utfFile);
				cfgPtr->utfFile = Tcl_AllocDebug(len + 1);
				(void) strcpy(cfgPtr->utfFile, t);
				break;
			}
			
			case 2: /* -title */
			{
				int len;
				char *t = Tcl_GetStringFromObj(objv[i + 1], &len);
				if (cfgPtr->utfTitle)
					Tcl_FreeDebug(cfgPtr->utfTitle);
				cfgPtr->utfTitle = Tcl_AllocDebug(len + 1);
				(void) strcpy(cfgPtr->utfTitle, t);
				break;
	    	}
	    }
	    objc -= 2;
	    i += 2;
	}

	return TCL_OK;
}

static int DirConfigure(Tcl_Interp *interp, t_sound_dir *dirPtr, int objc, Tcl_Obj *CONST objv[])
{
	static CONST char *optionStr[] = {"-path", NULL};
	int i = 0, option;

	if (objc & 1)
	{
		return TCL_ERROR;
	}
	while (objc > 1)
	{
		if (Tcl_GetIndexFromObj(interp, objv[i], optionStr,
			"option", 0, &option) != TCL_OK)
		{
			return TCL_ERROR;
		}
		switch (option)
		{
			case 0: /* -path */
			{
				int len;
				char *extStr;
				char *utfStr = Tcl_GetString(objv[i + 1]);
				Tcl_DString dString;

				if (dirPtr->extPath)
				{
					Tcl_FreeDebug(dirPtr->extPath);
					dirPtr->extPath = NULL;
				}
				if (dirPtr->utfPath)
				{
					Tcl_FreeDebug(dirPtr->utfPath);
					dirPtr->utfPath = NULL;
				}
				if (!utfStr[0])
					break;

				utfStr = Tcl_TranslateFileName(interp, utfStr, &dString);
				if (utfStr == NULL)
					return TCL_ERROR;
				len = Tcl_DStringLength(&dString);
				dirPtr->utfPath = Tcl_AllocDebug(len + 1);
				(void) strcpy(dirPtr->utfPath, utfStr);
				Tcl_DStringFree(&dString);

				extStr = Tcl_UtfToExternalDString(NULL, dirPtr->utfPath,
					len, &dString);
				if (extStr == NULL)
					return TCL_ERROR;
				len = Tcl_DStringLength(&dString);
				dirPtr->extPath = Tcl_AllocDebug(len + 1);
				(void) strcpy(dirPtr->extPath, extStr);
				Tcl_DStringFree(&dString);
				break;
			}
		}
		objc -= 2;
		i += 2;
	}

	return TCL_OK;
}

static void SoundListDelete(t_sound_list *sndList, int snd)
{
	int i;

	if (sndList->count <= SND_STATIC_SPACE)
	{
		for (i = snd; i < sndList->count - 1; i++)
			sndList->sound[i] = sndList->sound[i + 1];
		--sndList->count;
		return;
	}

	sndList->sound = (t_sound *) Array_Delete(sndList->sound,
		&sndList->count, sizeof(t_sound), snd);

	if (sndList->count <= SND_STATIC_SPACE)
	{
		for (i = 0; i < sndList->count; i++)
			sndList->space[i] = sndList->sound[i];
		Tcl_FreeDebug(sndList->sound);
		sndList->sound = sndList->space;
	}
}

/*
 * (sound config) cget $dir $option
 */
int objcmd_sound_config_cget(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	static CONST char *optionStr[] = {"-active", "-file", "-title", NULL};
	int cfg, option;
	t_sound_config *cfgPtr;

	if (ConfigFromObj(interp, objV[1], &cfg) != TCL_OK)
	{
		return TCL_ERROR;
	}
	cfgPtr = &g_sound_config[cfg];

    if (Tcl_GetIndexFromObj(interp, objV[2], optionStr, "option", 0, 
		&option) != TCL_OK)
	{
		return TCL_ERROR;
	}
	switch (option)
	{
		case 0: /* -active */
		{
			BooleanResult(interp, cfgPtr->active);
			break;
		}
		case 1: /* -file */
		{
			if (cfgPtr->utfFile)
				StringResult(interp, cfgPtr->utfFile);
			break;
		}
		case 2: /* -title */
		{
			if (cfgPtr->utfTitle)
				StringResult(interp, cfgPtr->utfTitle);
			break;
		}
	}

	return TCL_OK;
}

/*
 * (sound config) configure $cfg ?$option $value ...?
 */
int objcmd_sound_config_configure(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	int cfg;
	t_sound_config *cfgPtr;

	if (ConfigFromObj(interp, objV[1], &cfg) != TCL_OK)
	{
		return TCL_ERROR;
	}
	cfgPtr = &g_sound_config[cfg];
	return ConfigConfigure(interp, cfgPtr, objC - 2, objV + 2);
}

/*
 * (sound config) count $cfg $grp $evt
 */
int objcmd_sound_config_count(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	int cfg, grp, evt;
	t_sound_list *sndList;

	if (ConfigFromObj(interp, objV[1], &cfg) != TCL_OK)
	{
		return TCL_ERROR;
	}
	if (GroupFromObj(interp, objV[2], &grp) != TCL_OK)
	{
		return TCL_ERROR;
	}
	if (KeywordFromObj(interp, grp, objV[3], &evt) != TCL_OK)
	{
		return TCL_ERROR;
	}
	sndList = &g_sound_config[cfg].group[grp][evt];
	IntResult(interp, sndList->count);

	return TCL_OK;
}

/*
 * (sound config) delete $cfg
 */
int objcmd_sound_config_delete(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	int cfg, grp, evt;
	t_sound_config *cfgPtr;

	if (ConfigFromObj(interp, objV[1], &cfg) != TCL_OK)
	{
		return TCL_ERROR;
	}
	cfgPtr = &g_sound_config[cfg];

	if (cfgPtr->utfFile)
		Tcl_FreeDebug(cfgPtr->utfFile);
	if (cfgPtr->utfTitle)
		Tcl_FreeDebug(cfgPtr->utfTitle);

	for (grp = 0; grp < SNDGRP_MAX; grp++)
	{
		/* Get the number of elements in this group from the global array */
		int cnt = g_sound_group[grp].count;

		/* Free each sound list for each event in this group */
		for (evt = 0; evt < cnt; evt++)
		{
			if (cfgPtr->group[grp][evt].count > SND_STATIC_SPACE)
				Tcl_FreeDebug(cfgPtr->group[grp][evt].sound);
		}

		/* Free sound lists for all events in this group */
		Tcl_FreeDebug(cfgPtr->group[grp]);
	}

	g_sound_config = (t_sound_config *) Array_Delete(g_sound_config,
		&g_sound_config_count, sizeof(t_sound_config), cfg);

	return TCL_OK;
}

/*
 * (sound config) listof
 */
int objcmd_sound_config_listof(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	Tcl_DString dString;
	int i;

	Tcl_DStringInit(&dString);
	for (i = 0; i < g_sound_config_count; i++)
	{
		char buf[24];
		(void) sprintf(buf, "sndcfg%d", g_sound_config[i].id);
		Tcl_DStringAppendElement(&dString, buf);
	}
	Tcl_DStringResult(interp, &dString);
	
	return TCL_OK;
}

/*
 * (sound config) new ?$option $value?
 */
int objcmd_sound_config_new(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	t_sound_config *cfgPtr;
	int grp, evt;
	char buf[24];

	g_sound_config = Array_Insert(g_sound_config, &g_sound_config_count,
		sizeof(t_sound_config), g_sound_config_count);
	cfgPtr = &g_sound_config[g_sound_config_count - 1];

	cfgPtr->id = ++g_sound_config_id;
	cfgPtr->active = TRUE;
	cfgPtr->utfFile = NULL;
	cfgPtr->utfTitle = NULL;

	for (grp = 0; grp < SNDGRP_MAX; grp++)
	{
		/* Get the number of elements in this group from the global array */
		int cnt = g_sound_group[grp].count;

		/* Allocate a new sound list for each event in this group */
		cfgPtr->group[grp] =
			(t_sound_list *) Tcl_AllocDebug(sizeof(t_sound_list) * cnt);

		/* Initialize each sound list for each event in this group */
		for (evt = 0; evt < cnt; evt++)
		{
			cfgPtr->group[grp][evt].sound = cfgPtr->group[grp][evt].space;
			cfgPtr->group[grp][evt].count = 0;
		}
	}

	if (ConfigConfigure(interp, cfgPtr, objC - 1, objV + 1) != TCL_OK)
	{
		/* FIXME: delete cfgPtr */
		return TCL_ERROR;
	}

	/* Return the sound configuration identifier */
	(void) sprintf(buf, "sndcfg%d", cfgPtr->id);
	Tcl_SetResult(interp, buf, TCL_VOLATILE);

	return TCL_OK;
}

/*
 * (sound config) nextid
 */
int objcmd_sound_config_nextid(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	char buf[24];

	(void) sprintf(buf, "sndcfg%d", g_sound_config_id + 1);
	Tcl_SetResult(interp, buf, TCL_VOLATILE);

	return TCL_OK;
}

/*
 * (sound dir) delete $dir
 */
int objcmd_sound_dir_delete(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	int cfg, dir, grp, evt, snd;
	t_sound_dir *dirPtr;

	if (DirFromObj(interp, objV[1], &dir) != TCL_OK)
	{
		return TCL_ERROR;
	}
	dirPtr = &g_sound_dir[dir];

	/* Delete all assignments to sounds in this directory in all configurations */
	for (cfg = 0; cfg < g_sound_config_count; cfg++)
	{
		t_sound_config *cfgPtr = &g_sound_config[cfg];
		for (grp = 0; grp < SNDGRP_MAX; grp++)
		{
			int cnt = g_sound_group[grp].count;
			for (evt = 0; evt < cnt; evt++)
			{
				t_sound_list *sndList = &cfgPtr->group[grp][evt];
				for (snd = sndList->count - 1; snd >= 0; snd--)
				{
					if (sndList->sound[snd].dirId == dirPtr->id)
						SoundListDelete(sndList, snd);
				}
			}
		}
	}

	if (dirPtr->extPath)
		Tcl_FreeDebug(dirPtr->extPath);
	if (dirPtr->utfPath)
		Tcl_FreeDebug(dirPtr->utfPath);
		
	g_sound_dir = (t_sound_dir *) Array_Delete(g_sound_dir,
		&g_sound_dir_count, sizeof(t_sound_dir), dir);

	return TCL_OK;
}

/*
 * (sound dir) new ?$option $value ...?
 */
int objcmd_sound_dir_new(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	char buf[24];
	t_sound_dir *dirPtr;

	g_sound_dir = Array_Insert(g_sound_dir, &g_sound_dir_count,
		sizeof(t_sound_dir), g_sound_dir_count);
	dirPtr = &g_sound_dir[g_sound_dir_count - 1];

	dirPtr->id = ++g_sound_dir_id;
	dirPtr->extPath = NULL;
	dirPtr->utfPath = NULL;

	if (DirConfigure(interp, dirPtr, objC - 1, objV + 1) != TCL_OK)
	{
		/* FIXME: Free dirPtr */
		return TCL_ERROR;
	}

#if 0
	utfStr = Tcl_GetString(objV[1]);
	utfStr = Tcl_TranslateFileName(interp, utfStr, &dString);
	if (utfStr == NULL)
		return TCL_ERROR;
	len = Tcl_DStringLength(&dString);
	dirPtr->utfPath = Tcl_AllocDebug(len + 1);
	(void) strcpy(dirPtr->utfPath, utfStr);
	Tcl_DStringFree(&dString);

	extStr = Tcl_UtfToExternalDString(NULL, dirPtr->utfPath, len, &dString);
	if (extStr == NULL)
		return TCL_ERROR;
	len = Tcl_DStringLength(&dString);
	dirPtr->extPath = Tcl_AllocDebug(len + 1);
	(void) strcpy(dirPtr->extPath, extStr);
	Tcl_DStringFree(&dString);
#endif

	/* Return the sound directory identifier */
	(void) sprintf(buf, "snddir%d", dirPtr->id);
	Tcl_SetResult(interp, buf, TCL_VOLATILE);

	return TCL_OK;
}

/*
 * (sound dir) nextid
 */
int objcmd_sound_dir_nextid(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	char buf[24];

	(void) sprintf(buf, "snddir%d", g_sound_dir_id + 1);
	Tcl_SetResult(interp, buf, TCL_VOLATILE);

	return TCL_OK;
}

/*
 * (sound dir) cget $dir $option
 */
int objcmd_sound_dir_cget(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	static CONST char *optionStr[] = {"-path", NULL};
	int dir, option;
	t_sound_dir *dirPtr;

	if (DirFromObj(interp, objV[1], &dir) != TCL_OK)
	{
		return TCL_ERROR;
	}
	dirPtr = &g_sound_dir[dir];

    if (Tcl_GetIndexFromObj(interp, objV[2], optionStr, "option", 0, 
		&option) != TCL_OK)
	{
		return TCL_ERROR;
    }
    switch (option)
    {
    	case 0: /* -path */
    	{
    		Tcl_SetResult(interp, dirPtr->utfPath, TCL_VOLATILE);
    		break;
    	}
    }
	
	return TCL_OK;
}

/*
 * (sound dir) configure $dir ?$option $value ..."
 */
int objcmd_sound_dir_configure(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	int dir;
	t_sound_dir *dirPtr;

	if (DirFromObj(interp, objV[1], &dir) != TCL_OK)
	{
		return TCL_ERROR;
	}
	dirPtr = &g_sound_dir[dir];
	return DirConfigure(interp, dirPtr, objC - 2, objV + 2);
}

/*
 * (sound dir) listof
 */
int objcmd_sound_dir_listof(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	int i;
	Tcl_DString dString;

	Tcl_DStringInit(&dString);
	for (i = 0; i < g_sound_dir_count; i++)
	{
		char buf[24];
		(void) sprintf(buf, "snddir%d", g_sound_dir[i].id);
		Tcl_DStringAppendElement(&dString, buf);
	}
	Tcl_DStringResult(interp, &dString);
	
	return TCL_OK;
}

/*
 * (sound) assign $cfg $grp $evt ?$index? ?$dir $snd?
 */
int objcmd_sound_assign(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	int i, cfg, grp, evt, dir, snd;
	t_sound_list *sndList;
	t_sound *sndPtr;
	Tcl_DString dString;
	char buf[24], *t;

	if (ConfigFromObj(interp, objV[1], &cfg) != TCL_OK)
	{
		return TCL_ERROR;
	}
	if (GroupFromObj(interp, objV[2], &grp) != TCL_OK)
	{
		return TCL_ERROR;
	}
	if (KeywordFromObj(interp, grp, objV[3], &evt) != TCL_OK)
	{
		return TCL_ERROR;
	}
	sndList = &g_sound_config[cfg].group[grp][evt];
	
	/* Return list of sounds */
	if (objC == 4)
	{
		Tcl_DStringInit(&dString);
		for (i = 0; i < sndList->count; i++)
		{
			(void) sprintf(buf, "snddir%d", sndList->sound[i].dirId);
			Tcl_DStringAppendElement(&dString, buf);
			Tcl_DStringAppendElement(&dString, sndList->sound[i].sndId);
		}
		Tcl_DStringResult(interp, &dString);
		return TCL_OK;
	}

	if (Tcl_GetIntFromObj(interp, objV[4], &snd) != TCL_OK)
	{
		return TCL_ERROR;
	}
	if (ValidateIndex(interp, sndList, snd) != TCL_OK)
	{
		return TCL_ERROR;
	}
	sndPtr = &sndList->sound[snd];

	/* Get the n'th sound */
	if (objC == 5)
	{
		Tcl_DStringInit(&dString);
		(void) sprintf(buf, "snddir%d", sndPtr->dirId);
		Tcl_DStringAppendElement(&dString, buf);
		Tcl_DStringAppendElement(&dString, sndPtr->sndId);
		Tcl_DStringResult(interp, &dString);
		return TCL_OK;
	}

	if (DirFromObj(interp, objV[5], &dir) != TCL_OK)
	{
		return TCL_ERROR;
	}
	sndPtr->dirId = g_sound_dir[dir].id;

	t = Tcl_GetString(objV[6]);
	sndPtr->sndId = sound_id(t);

	return TCL_OK;
}

/*
 * (sound) activate $boolean
 */
int objcmd_sound_activate(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	int active;

	if (Tcl_GetBooleanFromObj(interp, objV[1], &active) != TCL_OK)
	{
		return TCL_ERROR;
    }

    if (s_sound_init)
		sound_hook_activate(active);

	return TCL_OK;
}

#ifdef SND_MSG_COLORS
/*
 * (sound) color $grp $evt ?$color?
 */
int objcmd_sound_color(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	int grp, evt;

	if (GroupFromObj(interp, objV[1], &grp) != TCL_OK)
	{
		return TCL_ERROR;
	}
	if (KeywordFromObj(interp, grp, objV[2], &evt) != TCL_OK)
	{
		return TCL_ERROR;
	}
	if (evt < 0 || evt >= g_sound_group[grp].count)
	{
		Tcl_SetResult(interp,
			format("bad index \"%d\": must be from 0 to %d",
			evt, g_sound_group[grp].count - 1), TCL_VOLATILE);
		return TCL_ERROR;
	}
	if (objC == 4)
	{
		int attr;

		if (Tcl_GetIndexFromObj(interp, objV[3], keyword_term_color,
			"terminal color", 0, &attr) != TCL_OK)
		{
			return TCL_ERROR;
		}
		g_sound_group[grp].colors[evt] = attr;
	}

	StaticResult(interp,
		(char *) keyword_term_color[g_sound_group[grp].colors[evt]]);

	return TCL_OK;
}
#endif /* SND_MSG_COLORS */

/*
 * (sound) count $group
 */
int objcmd_sound_count(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	int grp;

	if (GroupFromObj(interp, objV[1], &grp) != TCL_OK)
	{
		return TCL_ERROR;
	}
	IntResult(interp, g_sound_group[grp].count);

	return TCL_OK;
}

/*
 * (sound) delete $cfg $grp $evt $index
 */
int objcmd_sound_delete(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	int cfg, grp, evt, snd;
	t_sound_list *sndList;

	if (ConfigFromObj(interp, objV[1], &cfg) != TCL_OK)
	{
		return TCL_ERROR;
	}
	if (GroupFromObj(interp, objV[2], &grp) != TCL_OK)
	{
		return TCL_ERROR;
	}
	if (KeywordFromObj(interp, grp, objV[3], &evt) != TCL_OK)
	{
		return TCL_ERROR;
	}
	sndList = &g_sound_config[cfg].group[grp][evt];

	if (Tcl_GetIntFromObj(interp, objV[4], &snd) != TCL_OK)
	{
		return TCL_ERROR;
	}
	if (ValidateIndex(interp, sndList, snd) != TCL_OK)
	{
		return TCL_ERROR;
	}

#if 1
	SoundListDelete(sndList, snd);
#else
	if (sndList->count <= SND_STATIC_SPACE)
	{
		for (i = snd; i < sndList->count - 1; i++)
			sndList->sound[i] = sndList->sound[i + 1];
		--sndList->count;
		return TCL_OK;
	}

	sndList->sound = (t_sound *) Array_Delete(sndList->sound,
		&sndList->count, sizeof(t_sound), snd);

	if (sndList->count <= SND_STATIC_SPACE)
	{
		for (i = snd; i < sndList->count; i++)
			sndList->space[i] = sndList->sound[i];
		Tcl_FreeDebug(sndList->sound);
		sndList->sound = sndList->space;
	}
#endif

	return TCL_OK;
}

/*
 * (sound) exists $grp $evt
 */
int objcmd_sound_exists(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	int grp, evt;

	if (GroupFromObj(interp, objV[1], &grp) != TCL_OK)
	{
		return TCL_ERROR;
	}
	if (Tcl_GetIntFromObj(interp, objV[2], &evt) != TCL_OK)
	{
		return TCL_ERROR;
	}
	if (evt < 0 || evt >= g_sound_group[grp].count)
	{
		Tcl_SetResult(interp,
			format("bad index \"%d\": must be from 0 to %d",
			evt, g_sound_group[grp].count - 1), TCL_VOLATILE);
		return TCL_ERROR;
	}

	BooleanResult(interp, g_sound_group[grp].keyword[evt] != NULL);

	return TCL_OK;
}

/*
 * (sound) groups
 */
int objcmd_sound_groups(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	int i;
	Tcl_DString dString;

	Tcl_DStringInit(&dString);
	for (i = 0; i < SNDGRP_MAX; i++)
	{
		Tcl_DStringAppendElement(&dString, keyword_sound_group[i]);
	}
	Tcl_DStringResult(interp, &dString);
	
	return TCL_OK;
}

static int sound_hist_count(void)
{
	if (s_sound_hist_head == s_sound_hist_tail)
		return 0;
	if (s_sound_hist_tail < s_sound_hist_head)
		return s_sound_hist_head - s_sound_hist_tail;
	return SND_HIST_MAX - 1;
}

/*
 * (sound history) count
 */
int objcmd_sound_hist_count(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	IntResult(interp, sound_hist_count());

	return TCL_OK;
}

/*
 * (sound history) get $index
 */
int objcmd_sound_hist_get(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	int i, n;
	t_sound_hist *histPtr;
	Tcl_DString dString;
	char buf[24];

	if (Tcl_GetIntFromObj(interp, objV[1], &n) != TCL_OK)
	{
		return TCL_ERROR;
	}
	if (n < 0 || n >= sound_hist_count())
	{
		return TCL_ERROR;
	}
	if (s_sound_hist_tail < s_sound_hist_head)
	{
		i = s_sound_hist_tail + n;
	}
	else if (n <= SND_HIST_MAX - s_sound_hist_tail - 1)
	{
		i = s_sound_hist_tail + n;
	}
	else
	{
		i = n - (SND_HIST_MAX - s_sound_hist_tail - 1) - 1;
	}
	histPtr = &s_sound_hist[i];
	Tcl_DStringInit(&dString);
	(void) sprintf(buf, "snddir%d", histPtr->sound.dirId);
	Tcl_DStringAppendElement(&dString, buf);
	Tcl_DStringAppendElement(&dString, histPtr->sound.sndId);
	(void) sprintf(buf, "sndcfg%d", histPtr->config);
	Tcl_DStringAppendElement(&dString, buf);
	Tcl_DStringAppendElement(&dString, keyword_sound_group[histPtr->group]);
	Tcl_DStringAppendElement(&dString,
		g_sound_group[histPtr->group].keyword[histPtr->event]);
	Tcl_DStringResult(interp, &dString);

	return TCL_OK;
}

/*
 * (sound) init $shlib
 */
int objcmd_sound_init(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	char *extDllPath, *utfString;
	Tcl_DString extDString;

	/* Already initialized */
	if (s_sound_init)
		return TCL_ERROR;

	/* Get the DLL path */
	utfString = Tcl_GetString(objV[1]);

	/* Convert to system format */
	extDllPath = UtfToExt_TranslateFileName(interp, utfString, &extDString);

	/* Bad path */
	if (extDllPath == NULL)
	{
		/* Note: Tcl_DStringFree() is called for us */
		return TCL_ERROR;
	}

	/* Load the DLL */
	sound_load_dll(extDllPath);

	/* Clean up */
	Tcl_DStringFree(&extDString);

	sound_init();

	return TCL_OK;
}

/*
 * (sound) insert $cfg $grp $evt $ind $dirId $snd
 */
int objcmd_sound_insert(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	char *t;
	int i, cfg, grp, evt, snd, dir;
	t_sound_list *sndList;

    if (ConfigFromObj(interp, objV[1], &cfg) != TCL_OK)
	{
		return TCL_ERROR;
    }
    if (GroupFromObj(interp, objV[2], &grp) != TCL_OK)
	{
		return TCL_ERROR;
    }
	if (KeywordFromObj(interp, grp, objV[3], &evt) != TCL_OK)
	{
		return TCL_ERROR;
	}
	sndList = &g_sound_config[cfg].group[grp][evt];

	if (Tcl_GetIntFromObj(interp, objV[4], &snd) != TCL_OK)
	{
		return TCL_ERROR;
    }
	if (snd > sndList->count)
	{
		snd = sndList->count;
	}
	else if (snd < 0)
	{
		snd = 0;
	}

    if (DirFromObj(interp, objV[5], &dir) != TCL_OK)
	{
		return TCL_ERROR;
    }

	t = Tcl_GetStringFromObj(objV[6], NULL);

	/* Already using an array */
	if (sndList->count > SND_STATIC_SPACE)
	{
		sndList->sound = (t_sound *) Array_Insert(sndList->sound,
			&sndList->count, sizeof(t_sound), snd);
	}

	/* Static space used up. Alloc an array */
	else if (sndList->count == SND_STATIC_SPACE)
	{
		t_sound *sound = Array_New(SND_STATIC_SPACE + 1, sizeof(t_sound));
		/* Copy existing sounds to new array */
		for (i = 0; i < sndList->count; i++)
			sound[i] = sndList->sound[i];
		sndList->sound = sound;
		/* Move following sounds up 1 */
		for (i = sndList->count; i > snd; i--)
			sndList->sound[i] = sndList->sound[i - 1];
		++sndList->count;
	}

	/* Use static space */
	else
	{
		/* Move following sounds up 1 */
		for (i = sndList->count; i > snd; i--)
			sndList->sound[i] = sndList->sound[i - 1];
		++sndList->count;
	}

	sndList->sound[snd].dirId = g_sound_dir[dir].id;
	sndList->sound[snd].sndId = sound_id(t);

	return TCL_OK;
}

/*
 * (sound) keyword $grp $index
 */
int objcmd_sound_keyword(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	int grp, evt;
	char *keyword;

    if (GroupFromObj(interp, objV[1], &grp) != TCL_OK)
	{
		return TCL_ERROR;
    }
	if (Tcl_GetIntFromObj(interp, objV[2], &evt) != TCL_OK)
	{
		return TCL_ERROR;
    }
	if (evt < 0 || evt >= g_sound_group[grp].count)
	{
		Tcl_SetResult(interp,
			format("bad index \"%d\": must be from 0 to %d",
				evt, g_sound_group[grp].count - 1), TCL_VOLATILE);
		return TCL_ERROR;
	}

	keyword = (char *) g_sound_group[grp].keyword[evt];
	Tcl_SetResult(interp, keyword ? keyword : "", TCL_VOLATILE);

	return TCL_OK;
}

/*
 * (sound) play $dirId $snd
 */
int objcmd_sound_play(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	char *t;
	int dir;
	t_sound snd;

    if (DirFromObj(interp, objV[1], &dir) != TCL_OK)
	{
		return TCL_ERROR;
    }
	t = Tcl_GetStringFromObj(objV[2], NULL);

	snd.dirId = g_sound_dir[dir].id;
	snd.sndId = sound_id(t);
	sound_play(snd);

	return TCL_OK;
}

/*
 * (sound) play2 $grp $evt $mode
 */
int objcmd_sound_play2(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	int grp, evt, mode;

    if (GroupFromObj(interp, objV[1], &grp) != TCL_OK)
	{
		return TCL_ERROR;
    }
	if (KeywordFromObj(interp, grp, objV[2], &evt) != TCL_OK)
	{
		return TCL_ERROR;
	}
	if (Tcl_GetIntFromObj(interp, objV[3], &mode) != TCL_OK)
	{
		return TCL_ERROR;
    }
    sound(grp, evt, mode);

	return TCL_OK;
}

/*
 * (sound) flush
 */
int objcmd_sound_flush(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	sound_flush();
	return TCL_OK;
}

/*
 * (sound) setting $keyword ?$value?
 */
int
objcmd_sound_setting(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	static CONST char *cmdOption[] = {"ambient_delay", "monster_delay", NULL};
	int index;

    if (Tcl_GetIndexFromObj(interp, objV[1], cmdOption, "option", 0, 
		&index) != TCL_OK)
	{
		return TCL_ERROR;
    }

	switch (index)
	{
		case 0: /* ambient_delay */
			if (objC == 2)
			{
				IntResult(interp, g_ambient_delay);
				break;
			}
			if (Tcl_GetIntFromObj(interp, objV[2], &g_ambient_delay) != TCL_OK)
			{
				return TCL_ERROR;
		    }
			break;

		case 1: /* monster_delay */
			if (objC == 2)
			{
				IntResult(interp, g_monster_delay);
				break;
			}
			if (Tcl_GetIntFromObj(interp, objV[2], &g_monster_delay) != TCL_OK)
			{
				return TCL_ERROR;
		    }
			break;
	}

	return TCL_OK;
}
/*
 * (sound) stop
 */
int
objcmd_sound_stop(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
//	CommandInfo *infoCmd = (CommandInfo *) clientData;
//	int objC = objc - infoCmd->depth;
//	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	int i;

	/* Stop the sounds (on all channels) */
	for (i = 0; i < MAX_SND_CHANNELS; ++i)
	{
		if (s_channel[i].free) continue;

		sound_hook_stop(s_channel[i].snd_data, i);
	}

	return TCL_OK;
}

/*
 * (sound) volume ?$value?
 */
int
objcmd_sound_volume(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	int volume;

	if (objC == 1)
	{
		IntResult(interp, s_sound_volume);
		return TCL_OK;
	}
	if (Tcl_GetIntFromObj(interp, objV[1], &volume) != TCL_OK)
	{
		return TCL_ERROR;
	}
	if ((volume < 0) || (volume > 100))
	{
		FormatResult(interp,
			"invalid sound volume \"%d\": must be from 0 to 100", volume);
		return TCL_ERROR;
	}
	s_sound_volume = volume;
	sound_hook_volume(volume);
	return TCL_OK;
}

/*
 * (sound) window $win
 */
int objcmd_sound_window(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

#ifdef PLATFORM_WIN
	char *t;
	extern void *Plat_XWindowToHWND(Window xwin);
	Window win;
	Tk_Window tkwin;
	
	if (!s_sound_init)
		return TCL_OK;

	t = Tcl_GetStringFromObj(objV[1], NULL);
	tkwin = Tk_NameToWindow(interp, t, Tk_MainWindow(interp));
	if (!tkwin)
		return TCL_ERROR;
	win = Tk_WindowId(tkwin);
	sound_hook_window(Plat_XWindowToHWND(win));
#endif /* PLATFORM_WIN */

	return TCL_OK;
}

#if 0

/*
 * objcmd_sound --
 *
 *	Implements the "sound" Tcl command. Usage:
 *	> Claim/Release the sound system
 *		sound activate BOOLEAN
 *	> Count number of assignments, or number of sounds
 *		sound count GROUP ?KEYWORD?
 *	> Delete sound(s) from an assignment
 *		sound delete GROUP KEYWORD INDEX
 *	> Replace a sound assignment
 *		sound assign GROUP KEYWORD INDEX SOUND
 *	> Insert a sound in an assignment
 *		sound insert GROUP KEYWORD INDEX SOUND
 *	> Query a sound assignment
 *		sound assign GROUP KEYWORD ?INDEX?
 */
int
objcmd_sound(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	static CONST char *cmdOption[] = {"activate", "assign", "count", "delete",
		"desc", "exists", "groups", "insert", "keyword", "play", "setting",
		"window", "caps", "device", "music", "support", "init", "find",
		"play2", "flush", NULL};
	int index;

	t_sound *sound_ptr;
	char *t;
	int i;
	Tcl_Obj *listObjPtr;

    if (objC < 2)
    {
		Tcl_WrongNumArgs(interp, infoCmd->depth + 1, objv, "option ?arg ...?");
		return TCL_ERROR;
    }

    if (Tcl_GetIndexFromObj(interp, objV[1], cmdOption, "option", 0, 
		&index) != TCL_OK)
	{
		return TCL_ERROR;
    }

	switch (index)
	{
#ifdef TNB_SOUND

		case 12: /* caps */
			if (Tcl_GetIntFromObj(interp, objV[2], &i) != TCL_OK)
			{
				return TCL_ERROR;
		    }
			GetWaveOutCaps(i);
			break;

		case 13: /* device */
			GetWaveOutDevice();
			break;

		case 14: /* music */
			{
#if 0
#if 1
			char buffer[512];

			t = Tcl_GetStringFromObj(objV[2], NULL);
			sprintf(buffer, "open %s type sequencer alias MUSIC", t);
			if (mciSendString(buffer, NULL, 0, NULL) != 0)
			{
				StaticResult(interp, "couldn't open music");
				return TCL_ERROR;
			}

			if (mciSendString("play MUSIC from 0", NULL, 0, NULL) != 0)
			{
				(void) mciSendString("close all", NULL, 0, NULL);
				StaticResult(interp, "couldn't play music");
				return TCL_ERROR;
			}

			/* XXX Need to close it when done (notify) */
#else
			DWORD dwError;
			MCI_OPEN_PARMS mciOpenParms;
			MCI_PLAY_PARMS mciPlayParms;

			t = Tcl_GetStringFromObj(objV[2], NULL);
			mciOpenParms.lpstrDeviceType = "sequencer";
			mciOpenParms.lpstrElementName = t;
			dwError = mciSendCommand(NULL,
				MCI_OPEN,
				MCI_OPEN_TYPE | MCI_OPEN_ELEMENT,
				(DWORD)(LPVOID)&mciOpenParms);
			if (dwError)
			{
				StaticResult(interp, "couldn't open music");
				return TCL_ERROR;
			}
			dwError = mciSendCommand(mciOpenParms.wDeviceID,
				MCI_PLAY,
				0,
				(DWORD)(LPVOID) &mciPlayParms);
			if (dwError)
			{
				mciSendCommand(mciOpenParms.wDeviceID, MCI_CLOSE, 0, NULL);
				StaticResult(interp, "couldn't play music");
				return TCL_ERROR;
			}
#endif
#endif /* if 0 */
			}
			break;

		case 15: /* support */
			if (Tcl_GetIntFromObj(interp, objV[2], &i) != TCL_OK)
			{
				return TCL_ERROR;
		    }
			GetWaveOutSupport(i);
			break;

#endif /* TNB_SOUND */

		case 17: /* find */
		{
		    if (objC != 3)
		    {
				Tcl_WrongNumArgs(interp, infoCmd->depth + 2, objV, "group");
				return TCL_ERROR;
		    }
		    if (GroupFromObj(interp, objV[2], &index) != TCL_OK)
			{
				return TCL_ERROR;
		    }
			listObjPtr = Tcl_NewListObj(0, NULL);
			for (i = 0; i < g_sound[index].count; i++)
			{
				if (!g_sound[index].sound[i].keyword) continue;
				Tcl_ListObjAppendElement(interp, listObjPtr,
					Tcl_NewStringObj(g_sound[index].sound[i].keyword, -1));
			}
			Tcl_SetObjResult(interp, listObjPtr);
			break;
		}
	}

	return TCL_OK;

bad_index:
	FormatResult(interp, "bad index \"%d\": must be from 0 to %d",
		i, g_sound[index].count - 1);
	return TCL_ERROR;
}

#endif /* 0 */

static CommandInit commandInit[] = {
 {0, "angband", 0, 0, (char *) NULL, (Tcl_ObjCmdProc *) NULL, (ClientData) 0},
  {1, "sound", 0, 0, (char *) NULL, (Tcl_ObjCmdProc *) NULL, (ClientData) 0},
   {2, "activate", 2, 2, "boolean", objcmd_sound_activate, (ClientData) 0},
   {2, "assign", 4, 7, "cfg grp evt ?index? ?dir snd?", objcmd_sound_assign, (ClientData) 0},
   {2, "config", 0, 0, (char *) NULL, (Tcl_ObjCmdProc *) NULL, (ClientData) 0},
    {3, "configure", 2, 0, "cfg ?option value...?", objcmd_sound_config_configure, (ClientData) 0},
    {3, "cget", 3, 3, "cfg option", objcmd_sound_config_cget, (ClientData) 0},
    {3, "count", 4, 4, "cfg grp evt", objcmd_sound_config_count, (ClientData) 0},
    {3, "delete", 2, 2, "cfg", objcmd_sound_config_delete, (ClientData) 0},
    {3, "listof", 0, 0, (char *) NULL, objcmd_sound_config_listof, (ClientData) 0},
    {3, "new", 0, 0, "?option value ...", objcmd_sound_config_new, (ClientData) 0},
    {3, "nextid", 0, 0, (char *) NULL, objcmd_sound_config_nextid, (ClientData) 0},
   {2, "count", 2, 2, "group", objcmd_sound_count, (ClientData) 0},
   {2, "color", 3, 4, "grp evt ?attr?", objcmd_sound_color, (ClientData) 0},
   {2, "delete", 5, 5, "cfg grp evt index", objcmd_sound_delete, (ClientData) 0},
   {2, "dir", 0, 0, (char *) NULL, (Tcl_ObjCmdProc *) NULL, (ClientData) 0},
    {3, "cget", 3, 3, "dir option", objcmd_sound_dir_cget, (ClientData) 0},
    {3, "configure", 2, 0, "dir ?option value...?", objcmd_sound_dir_configure, (ClientData) 0},
    {3, "delete", 2, 2, "dir", objcmd_sound_dir_delete, (ClientData) 0},
    {3, "listof", 0, 0, (char *) NULL, objcmd_sound_dir_listof, (ClientData) 0},
    {3, "new", 0, 0, "?option value ...?", objcmd_sound_dir_new, (ClientData) 0},
    {3, "nextid", 0, 0, (char *) NULL, objcmd_sound_dir_nextid, (ClientData) 0},
   {2, "exists", 3, 3, "grp evt", objcmd_sound_exists, (ClientData) 0},
   {2, "flush", 0, 0, (char *) NULL, objcmd_sound_flush, (ClientData) 0},
   {2, "groups", 0, 0, (char *) NULL, objcmd_sound_groups, (ClientData) 0},
   {2, "init", 2, 2, "shlib", objcmd_sound_init, (ClientData) 0},
   {2, "insert", 7, 7, "cfg grp evt index dir snd", objcmd_sound_insert, (ClientData) 0},
   {2, "history", 0, 0, (char *) NULL, (Tcl_ObjCmdProc *) NULL, (ClientData) 0},
    {3, "count", 0, 0, (char *) NULL, objcmd_sound_hist_count, (ClientData) 0},
    {3, "get", 2, 2, "index", objcmd_sound_hist_get, (ClientData) 0},
   {2, "keyword", 3, 3, "grp evt", objcmd_sound_keyword, (ClientData) 0},
   {2, "play", 3, 3, "dir snd", objcmd_sound_play, (ClientData) 0},
   {2, "play2", 4, 4, "grp evt mode", objcmd_sound_play2, (ClientData) 0},
   {2, "setting", 2, 3, "option ?value?", objcmd_sound_setting, (ClientData) 0},
   {2, "stop", 0, 0, (char *) NULL, objcmd_sound_stop, (ClientData) 0},
   {2, "volume", 1, 2, "?volume?", objcmd_sound_volume, (ClientData) 0},
   {2, "window", 2, 2, "window", objcmd_sound_window, (ClientData) 0},
 {0, (char *) NULL, 0, 0, (char *) NULL, (Tcl_ObjCmdProc *) NULL, (ClientData) 0}
};

void sound_init_1(void)
{
	/* sound_message() is called when reading savefile */
	s_sound_to_play.sound.sndId = s_null_sound;

	(void) CommandInfo_Init(g_interp, commandInit, NULL);
}

static char *text_head = NULL;

void sound_init(void)
{
	const char **keyword;
	cptr s;
	int i, j, offset, count;

	count = sizeof(keyword_sound_event) / sizeof(cptr) - 1;
	g_sound_group[SNDGRP_EVENT].count = count;
	g_sound_group[SNDGRP_EVENT].keyword = keyword_sound_event;

	g_sound_group[SNDGRP_MONSTER_ATTACK].count = RBM_MAX;

#if defined(OANGBANDTK) || defined(ZANGBANDTK)
	count = sizeof(keyword_sound_martial_art) / sizeof(cptr) - 1;
	g_sound_group[SNDGRP_MARTIAL_ART].count = count;
	g_sound_group[SNDGRP_MARTIAL_ART].keyword = keyword_sound_martial_art;
#endif /* */

	C_MAKE(monster_sound_times, MAX_R_IDX, u32b);
	
	Tcl_InitHashTable(&s_sound_id_table, TCL_STRING_KEYS);

	for (i = 0; i < MAX_SND_CHANNELS; ++i)
	{
		s_channel[i].free = TRUE;
	}

	g_sound_group[SNDGRP_MONSTER].count = MAX_R_IDX;
	g_sound_group[SNDGRP_MONSTER].keyword =
		(const char **) Tcl_AllocDebug(MAX_R_IDX * sizeof(char *));
	keyword = g_sound_group[SNDGRP_MONSTER].keyword;

	/* mon123\0 + mon12\0 + mon1\0 */
	count = (MAX_R_IDX - 99) * 7 + (99 - 9) * 6 + 10 * 5;
	text_head = Tcl_AllocDebug(count);
	offset = 0;
	for (i = 0; i < MAX_R_IDX; i++)
	{
		monster_race *r_ptr = &r_info[i];
		if (r_ptr->name != 0)
		{
			keyword[i] = text_head + offset;
			offset += sprintf(text_head + offset, "mon%d", i) + 1;
		}
		else
		{
			keyword[i] = NULL;
		}
	}

#if defined(ANGBANDTK) || defined(KANGBANDTK)

	/* Player ghost */
	keyword[MAX_R_IDX - 1] = NULL;

#endif /* ANGBANDTK, KANGBANDTK */

	count = 32 + 32 + 32;
#if defined(OANGBANDTK)
	count += 32;
#endif
	g_sound_group[SNDGRP_MONSTER_SPELL].count = count;

#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(ZANGBANDTK)

	g_sound_group[SNDGRP_MONSTER_SPELL].keyword =
		(const char **) Tcl_AllocDebug(sizeof(char *) * count);
	keyword = g_sound_group[SNDGRP_MONSTER_SPELL].keyword;

	/*
	 * XXX Hack -- The keywords for g_sound_mon_spell[] are borrowed
	 * from the arrays used to parse the r_info.txt file.
	 */
	for (i = 0; i < 32; i++)
	{
		s = r_info_flags4[i];
		if (s[0] == 'X' && s[1] == 'X' && s[2] == 'X')
		{
			s = NULL;
		}
		*keyword++ = s;
	}
	for (i = 0; i < 32; i++)
	{
		s = r_info_flags5[i];
		if (s[0] == 'X' && s[1] == 'X' && s[2] == 'X')
		{
			s = NULL;
		}
		*keyword++ = s;
	}
	for (i = 0; i < 32; i++)
	{
		s = r_info_flags6[i];
		if (s[0] == 'X' && s[1] == 'X' && s[2] == 'X')
		{
			s = NULL;
		}
		*keyword++ = s;
	}

#endif /* ANGBANDTK, KANGBANDTK, ZANGBANDTK */

#if defined(OANGBANDTK)

	g_sound_group[SNDGRP_MONSTER_SPELL].keyword = keyword_monster_spell;
	keyword = g_sound_group[SNDGRP_MONSTER_SPELL].keyword;

#endif /* OANGBANDTK */

	g_sound_group[SNDGRP_MONSTER_ATTACK].keyword =
		(const char **) Tcl_AllocDebug(sizeof(char *) * RBM_MAX);
	keyword = g_sound_group[SNDGRP_MONSTER_ATTACK].keyword;
	for (i = 0; i < RBM_MAX; i++)
	{
		s = r_info_blow_method[i];
		if (!s[0] || (s[0] == 'X' && s[1] == 'X' && s[2] == 'X'))
		{
			s = NULL;
		}
		*keyword++ = s;
	}

	/*
	 * Create a hash table for each group, to find a sound by
	 * keyword.
	 */
	for (i = 0; i < SNDGRP_MAX; i++)
	{
		int count = g_sound_group[i].count;
		Tcl_InitHashTable(&g_sound_group[i].hash, TCL_STRING_KEYS);
		keyword = g_sound_group[i].keyword;
#ifdef SND_MSG_COLORS
		g_sound_group[i].colors = (int *) Tcl_AllocDebug(sizeof(int) * count);
#endif
		for (j = 0; j < count; j++)
		{
			Tcl_HashEntry *hPtr;
			int isNew;

#ifdef SND_MSG_COLORS
			g_sound_group[i].colors[j] = TERM_WHITE;
#endif
			if (keyword[j] == NULL) continue;

			hPtr = Tcl_CreateHashEntry(&g_sound_group[i].hash, keyword[j], &isNew);
			Tcl_SetHashValue(hPtr, j);
		}
	}

	/* Hack -- seed for sounds */
	s_rand_sound = rand_int(0x10000000);

	/* Sounds are initialized */
	s_sound_init = TRUE;
}

void sound_exit(void)
{
	int i, g;

	/* Do nothing if sound_init() wasn't called */
	if (!s_sound_init) return;

	/* Stop all sounds, free all sounds, free all channels */
	for (i = 0; i < MAX_SND_CHANNELS; ++i)
	{
		if (s_channel[i].free) continue;
		sound_hook_stop(s_channel[i].snd_data, i);
	}

	for (i = 0; i < g_sound_config_count; i++)
	{
		t_sound_config *cfgPtr = &g_sound_config[i];

		if (cfgPtr->utfTitle)
			Tcl_FreeDebug(cfgPtr->utfTitle);
		if (cfgPtr->utfFile)
			Tcl_FreeDebug(cfgPtr->utfFile);
		for (g = 0; g < SNDGRP_MAX; g++)
		{
			int e;
			for (e = 0; e < g_sound_group[g].count; e++)
			{
				t_sound_list *sndList = &cfgPtr->group[g][e];
				if (sndList->count > SND_STATIC_SPACE)
					Tcl_FreeDebug(sndList->sound);
			}
			Tcl_FreeDebug(cfgPtr->group[g]);
		}
	}
	if (g_sound_config)
		Tcl_FreeDebug(g_sound_config);

	for (i = 0; i < g_sound_dir_count; i++)
	{
		t_sound_dir *dirPtr = &g_sound_dir[i];
		if (dirPtr->utfPath)
			Tcl_FreeDebug(dirPtr->utfPath);
		if (dirPtr->extPath)
			Tcl_FreeDebug(dirPtr->extPath);
	}
	if (g_sound_dir)
		Tcl_FreeDebug(g_sound_dir);

	Tcl_FreeDebug(g_sound_group[SNDGRP_MONSTER].keyword);
	Tcl_FreeDebug(g_sound_group[SNDGRP_MONSTER_ATTACK].keyword);
#if !defined(OANGBANDTK) /* This points to static memory */
	Tcl_FreeDebug(g_sound_group[SNDGRP_MONSTER_SPELL].keyword);
#endif
#ifdef SND_MSG_COLORS
	for (i = 0; i < SNDGRP_MAX; i++)
		Tcl_FreeDebug(g_sound_group[i].colors);
#endif

	Tcl_FreeDebug(text_head);

	/* Final cleanup */
	sound_hook_exit();
}

