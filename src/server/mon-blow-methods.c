/*
 * File: mon-blow-methods.c
 * Purpose: Monster melee methods module.
 *
 * Copyright (c) 1997 Ben Harrison, David Reeve Sward, Keldon Jones.
 * Copyright (c) 2013 Ben Semmler
 * Copyright (c) 2016 MAngband and PWMAngband Developers
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
 * Return whether or not the given blow causes cuts.
 *
 * Values are in list-blow-methods.h.
 *
 * method is the RBM_ constant for the method.
 */
bool monster_blow_method_cut(monster_blow_method_t method)
{
	static const bool blow_cuts[] =
    {
		#define RBM(x, c, s, miss, p, m, a, d, f) c,
		#include "list-blow-methods.h"
		#undef RBM
        false
	};

	if (method >= RBM_MAX)
		return false;

	return blow_cuts[method];
}


/*
 * Return whether or not the given blow causes stunning.
 *
 * Values are in list-blow-methods.h.
 *
 * method is the RBM_ constant for the method.
 */
bool monster_blow_method_stun(monster_blow_method_t method)
{
	static const bool blow_stuns[] =
    {
		#define RBM(x, c, s, miss, p, m, a, d, f) s,
		#include "list-blow-methods.h"
		#undef RBM
        false
	};

	if (method >= RBM_MAX)
		return false;

	return blow_stuns[method];
}


/*
 * Return a message type to display.
 *
 * Values are in list-blow-methods.h.
 *
 * method is the RBM_ constant for the method.
 */
int monster_blow_method_message(monster_blow_method_t method)
{
	static const int blow_messages[] =
    {
		#define RBM(x, c, s, miss, p, m, a, d, f) m,
		#include "list-blow-methods.h"
		#undef RBM
        MSG_GENERIC
	};

	if (method >= RBM_MAX)
		return MSG_GENERIC;

	return blow_messages[method];
}


/*
 * Return a randomly chosen string to append to an RBM_INSULT message.
 */
static const char *monster_blow_random_insult(void)
{
    #define MAX_DESC_INSULT 8
    static const char *desc_insult[MAX_DESC_INSULT] =
    {
        "insults %s!",
        "insults your mother!",
        "gives %s the finger!",
        "humiliates %s!",
        "defiles %s!",
        "dances around %s!",
        "makes obscene gestures!",
        "moons %s!!!"
    };

    return desc_insult[randint0(MAX_DESC_INSULT)];
    #undef MAX_DESC_INSULT
}


/*
 * Return a randomly chosen string to append to an RBM_MOAN message.
 */
static const char *monster_blow_random_moan(void)
{
    #define MAX_DESC_MOAN 8
    static const char *desc_moan[MAX_DESC_MOAN] =
    {
        "wants his mushrooms back.",
        "tells you to get off his land.",
        "looks for his dogs.",
        "says 'Did you kill my Fang?'",
        "asks 'Do you want to buy any mushrooms?'",
        "seems sad about something.",
        "asks if you have seen his dogs.",
        "mumbles something about mushrooms."
    };

    return desc_moan[randint0(MAX_DESC_MOAN)];
    #undef MAX_DESC_MOAN
}


/*
 * Return an action string to be appended on the attack message.
 *
 * Values are in list-blow-methods.h.
 *
 * method is the RBM_ constant for the method.
 */
const char *monster_blow_method_action(monster_blow_method_t method)
{
	static const char *blow_actions[] =
    {
		#define RBM(x, c, s, miss, p, m, a, d, f) a,
		#include "list-blow-methods.h"
		#undef RBM
        NULL
	};
	const char *action = NULL;

	if (method >= RBM_MAX)
		return NULL;

	action = blow_actions[method];

	if (method == RBM_INSULT && action == NULL)
		action = monster_blow_random_insult();
	else if (method == RBM_MOAN && action == NULL)
		action = monster_blow_random_moan();

	return action;
}


/*
 * Return whether or not the player is notified of a missed attack.
 *
 * Values are in list-blow-methods.h.
 *
 * method is the RBM_ constant for the method.
 */
bool monster_blow_method_miss(monster_blow_method_t method)
{
	static const bool blow_misses[] =
    {
		#define RBM(x, c, s, miss, p, m, a, d, f) miss,
		#include "list-blow-methods.h"
		#undef RBM
        false
	};

	if (method >= RBM_MAX)
		return false;

	return blow_misses[method];
}


/*
 * Return whether or not the given blow is a physical attack.
 *
 * Values are in list-blow-methods.h.
 *
 * method is the RBM_ constant for the method.
 */
bool monster_blow_method_physical(monster_blow_method_t method)
{
	static const bool blow_physicals[] =
    {
		#define RBM(x, c, s, miss, p, m, a, d, f) p,
		#include "list-blow-methods.h"
		#undef RBM
        false
	};

	if (method >= RBM_MAX)
		return false;

	return blow_physicals[method];
}


const char *monster_blow_method_flavor(monster_blow_method_t method)
{
	static const char *flav_actions[] =
    {
		#define RBM(x, c, s, miss, p, m, a, d, f) f,
		#include "list-blow-methods.h"
		#undef RBM
        NULL
	};

	if (method >= RBM_MAX)
		return NULL;

	return flav_actions[method];
}


/*
 * Return a description for the given monster blow method flags.
 *
 * Returns a sensible placeholder string for an out-of-range flag.
 * Descriptions are in list-blow-methods.h.
 *
 * method is one of the RBM_ flags.
 */
const char *monster_blow_method_description(monster_blow_method_t method)
{
    static const char *r_blow_method_description[] =
    {
        #define RBM(x, c, s, miss, p, m, a, d, f) d,
        #include "list-blow-methods.h"
        #undef RBM
        NULL
    };

    /* Return a placeholder for RBM_NONE, since it doesn't make sense to describe a blow that doesn't have a method */
    if ((method <= RBM_NONE) || (method >= RBM_MAX)) return "do something weird";

    return r_blow_method_description[method];
}


/*
 * Return the RBM_ constant matching the given string.
 *
 * Values are stringified RBM_ constants.
 *
 * string contains a value to search for.
 */
monster_blow_method_t blow_method_name_to_idx(const char *string)
{
    int i;
    static const char *r_info_blow_method[] =
    {
        #define RBM(x, c, s, miss, p, m, a, d, f) #x,
        #include "list-blow-methods.h"
        #undef RBM
        NULL
    };

    for (i = 0; r_info_blow_method[i]; i++)
    {
        if (streq(string, r_info_blow_method[i])) break;
    }

    return i;
}


/*
 * Return whether the given method is valid.
 *
 * method is one of the RBM_ flags.
 */
bool monster_blow_method_is_valid(monster_blow_method_t method)
{
    return (method < RBM_MAX);
}

