/*
 * File: party.c
 * Purpose: Support for the "party" system
 *
 * Copyright (c) 2019 MAngband and PWMAngband Developers
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


/*** Player lookup ***/


/*
 * Lookup a player by name.
 */
struct player *player_lookup(const char *name)
{
    int i;

    /* Find name */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *p = player_get(i);

        /* Check this one */
        if (streq(name, p->name))
        {
            /* Found him */
            return p;
        }
    }

    /* No match */
    return NULL;
}


/* The party information */
party_type parties[MAX_PARTIES];


/*** Party functions ***/


/*
 * Lookup a party number by name.
 */
static int party_lookup(const char *name)
{
    int i;

    /* Check each party */
    for (i = 0; i < MAX_PARTIES; i++)
    {
        /* Check name */
        if (streq(parties[i].name, name))
            return i;
    }

    /* No match */
    return -1;
}


/*
 * Check for the existance of a player in a party.
 */
bool player_in_party(int party_id, struct player *p)
{
    return (p->party == party_id);
}


bool in_party(struct player *p, int party_id)
{
    return (party_id && player_in_party(party_id, p));
}


bool is_party_owner(struct player *p, struct player *q)
{
    return (p->party && streq(parties[p->party].owner, q->name));
}


/*
 * Check if two players given by their IDs are in the same party
 */
bool master_in_party(s16b p1_id, s16b p2_id)
{
    struct player *p1_ptr = NULL;
    struct player *p2_ptr = NULL;
    int i;

    /* Find IDs */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *p = player_get(i);

        /* Check this one */
        if (p1_id == p->id) p1_ptr = p;
        if (p2_id == p->id) p2_ptr = p;
    }

    /* Player IDs not found */
    if (!p1_ptr || !p2_ptr) return false;

    /* Same IDs */
    if (p1_id == p2_id) return true;

    /* Not in a party */
    if (!p1_ptr->party || !p2_ptr->party) return false;

    /* Same party */
    if (p1_ptr->party == p2_ptr->party) return true;

    /* Different parties */
    return false;
}


bool pvm_check(struct player *p, struct monster *mon)
{
   /* Paranoia */
    if (!mon->race) return false;

    /* Hack -- dungeon master and his monsters */
    if (p->dm_flags & DM_MONSTER_FRIEND) return false;

    /* Same party */
    if (master_in_party(mon->master, p->id)) return false;

    /* Always hostile by default */
    return true;
}


/*
 * Create a new party, owned by "p", and called "name".
 */
static bool party_create(struct player *p, const char *name)
{
    int index = 0, i;
    hturn oldest;

    ht_copy(&oldest, &turn);

    /* Check for already existing party by that name */
    if (party_lookup(name) != -1)
    {
        msg(p, "A party by that name already exists.");
        return false;
    }

    /* Make sure this guy isn't in some other party already */
    if (p->party != 0)
    {
        msg(p, "You already belong to a party!");
        return false;
    }

    /* Find the "best" party index */
    for (i = 1; i < MAX_PARTIES; i++)
    {
        /* Check deletion time of disbanded parties */
        if ((parties[i].num == 0) && (ht_cmp(&parties[i].created, &oldest) < 0))
        {
            /* Track oldest */
            ht_copy(&oldest, &parties[i].created);
            index = i;
        }
    }

    /* Make sure we found an empty slot */
    if ((index == 0) || (ht_cmp(&oldest, &turn) == 0))
    {
        /* Error */
        msg(p, "There aren't enough party slots!");
        return false;
    }

    /* Set party name */
    my_strcpy(parties[index].name, name, sizeof(parties[0].name));

    /* Set owner name */
    my_strcpy(parties[index].owner, p->name, sizeof(parties[0].owner));

    /* Add the owner as a member */
    p->party = index;
    parties[index].num++;

    /* Set the "creation time" */
    ht_copy(&parties[index].created, &turn);

    /* Resend party info */
    Send_party(p);

    /* Success */
    return true;
}


/*
 * Check if one player is hostile toward the other
 */
static bool check_hostile(struct player *attacker, struct player *target)
{
    hostile_type *h_ptr;

    /* Scan list */
    for (h_ptr = attacker->hostile; h_ptr; h_ptr = h_ptr->next)
    {
        /* Identical ID's yield hostility */
        if (h_ptr->id == target->id) return true;
    }

    /* Not hostile */
    return false;
}


/*
 * Add a player to a party.
 */
static bool party_add(struct player *q, const char *name)
{
    int party_id = q->party, i;
    char desc[NORMAL_WID];

    /* Find name */
    struct player *p = player_lookup(name);

    /* Check for existance */
    if (!p)
    {
        /* Oops */
        msg(q, "That player is not currently in the game.");

        return false;
    }

    /* Make sure this added person is neutral */
    if (p->party != 0)
    {
        /* Message */
        msg(q, "That player is already in a party.");

        /* Abort */
        return false;
    }

    /* You can't add a hostile player! */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *player = player_get(i);

        /* Check this one */
        if ((player->party == party_id) &&
            (check_hostile(p, player) || check_hostile(player, p)))
        {
            /* Player is hostile to someone in the party */
            msg(q, "Ask this hostile player to make peace first.");

            /* Abort */
            return false;
        }
    }

    /* Tell the party about its new member */
    my_strcpy(desc, p->name, sizeof(desc));
    my_strcap(desc);
    party_msg_format(party_id, "%s has been added to party %s.", desc, parties[party_id].name);

    /* One more player in this party */
    parties[party_id].num++;

    /* Tell him about it */
    msg(p, "You've been added to party '%s'.", parties[party_id].name);

    /* Set his party number */
    p->party = party_id;

    /* Resend info */
    Send_party(p);

    /* Success */
    return true;
}


/*
 * Remove a person from a party.
 *
 * Removing the party owner destroys the party.
 */
static bool party_remove(struct player *q, const char *name)
{
    int party_id = q->party, i;

    /* Find name */
    struct player *p = player_lookup(name);

    /* Check for existance */
    if (!p)
    {
        /* Oops */
        msg(q, "That player is not currently in the game.");

        return false;
    }

    /* Make sure this is the owner */
    if (!streq(parties[party_id].owner, q->name))
    {
        /* Message */
        msg(q, "You must be the owner to delete someone.");

        /* Abort */
        return false;
    }

    /* Make sure they were in the party to begin with */
    if (!player_in_party(party_id, p))
    {
        /* Message */
        msg(q, "You can only delete party members.");

        /* Abort */
        return false;
    }

    /* See if this is the owner we're deleting */
    if (q == p)
    {
        /* Remove the party altogether */

        /* Set the number of people in this party to zero */
        parties[party_id].num = 0;

        /* Remove everyone else */
        for (i = 1; i <= NumPlayers; i++)
        {
            struct player *player = player_get(i);

            /* Check if they are in here */
            if (player_in_party(party_id, player))
            {
                player->party = 0;
                msg(player, "Your party has been disbanded.");
                Send_party(player);
            }
        }

        /* Set the creation time to "disbanded time" */
        ht_copy(&parties[party_id].created, &turn);

        /* Empty the name */
        my_strcpy(parties[party_id].name, "", sizeof(parties[0].name));
    }

    /* Keep the party, just lose a member */
    else
    {
        char desc[NORMAL_WID];

        /* Lose a member */
        parties[party_id].num--;

        /* Set his party number back to "neutral" */
        p->party = 0;

        /* Messages */
        my_strcpy(desc, p->name, sizeof(desc));
        my_strcap(desc);
        msg(p, "You have been removed from your party.");
        party_msg_format(party_id, "%s has been removed from the party.", desc);

        /* Resend info */
        Send_party(p);
    }

    return true;
}


/*
 * A player wants to leave a party.
 */
void party_leave(struct player *p)
{
    int party_id = p->party;
    char desc[NORMAL_WID];

    /* Make sure he belongs to a party */
    if (!party_id)
    {
        msg(p, "You don't belong to a party.");
        return;
    }

    /* If he's the owner, use the other function */
    if (streq(p->name, parties[party_id].owner))
    {
        /* Call party_remove */
        party_remove(p, p->name);
        return;
    }

    /* Lose a member */
    parties[party_id].num--;

    /* Set him back to "neutral" */
    p->party = 0;

    /* Inform people */
    my_strcpy(desc, p->name, sizeof(desc));
    my_strcap(desc);
    msg(p, "You have been removed from your party.");
    party_msg_format(party_id, "%s has left the party.", desc);

    /* Resend info */
    Send_party(p);
}


/*
 * Send a message to everyone in a party.
 */
static void party_msg(int party_id, const char *msg)
{
    int i;

    /* Check for this guy */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *player = player_get(i);

        /* Check this guy */
        if (player_in_party(party_id, player))
            msg_print(player, msg, MSG_WHISPER);
    }
}


/*
 * Send a formatted message to a party.
 */
void party_msg_format(int party_id, const char *fmt, ...)
{
    va_list vp;
    char buf[MSG_LEN];

    /* Begin the Varargs Stuff */
    va_start(vp, fmt);

    /* Format the args, save the length */
    vstrnfmt(buf, MSG_LEN, fmt, vp);

    /* End the Varargs Stuff */
    va_end(vp);

    /* Display */
    party_msg(party_id, buf);
}


bool party_share_with(struct player *p, int party_id, struct player *q)
{
    return (in_party(q, party_id) && COORDS_EQUAL(&q->wpos, &p->wpos) &&
        ((cfg_party_sharelevel == -1) || (abs(q->lev - p->lev) <= cfg_party_sharelevel)));
}


/*
 * Split some experience among party members.
 *
 * This should ONLY be used while killing monsters.  The amount
 * should be the monster base experience times the monster level.
 *
 * This algorithm may need some work....  However, it does have some nifty
 * features, such as:
 *
 * 1) A party with just one member functions identically as before.
 *
 * 2) A party with two equally-levelled members functions such that each
 * member gets half as much experience as he would have by killing the monster
 * by himself.
 *
 * 3) Higher-leveled members of a party get higher percentages of the
 * experience.
 */
void party_exp_gain(struct player *p, int party_id, s32b amount)
{
    int i;
    s32b new_exp, new_exp_frac, average_lev = 0, num_members = 0;
    s32b modified_level;

    /* Calculate the average level */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *q = player_get(i);

        /* Check for his existance in the party */
        if (party_share_with(p, party_id, q))
        {
            /* Increase the "divisor" */
            average_lev += q->lev;
            num_members++;
        }
    }

    /* Now, distribute the experience */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *q = player_get(i);

        /* Check for existance in the party */
        if (party_share_with(p, party_id, q))
        {
            /* Calculate this guy's experience */
            if (q->lev * num_members < average_lev) /* below average */
            {
                if ((average_lev - q->lev * num_members) > 2 * num_members)
                    modified_level = q->lev * num_members + 2 * num_members;
                else
                    modified_level = average_lev;
            }
            else
            {
                if ((q->lev * num_members - average_lev) > 2 * num_members)
                    modified_level = q->lev * num_members - 2 * num_members;
                else
                    modified_level = average_lev;
            }

            new_exp = (amount * modified_level) / (average_lev * num_members * q->lev);
            new_exp_frac = ((((amount * modified_level) % (average_lev * num_members * q->lev))
                * 0x10000L) / (average_lev * num_members * q->lev)) + q->exp_frac;

            /* Keep track of experience */
            if (new_exp_frac >= 0x10000L)
            {
                new_exp++;
                q->exp_frac = new_exp_frac - 0x10000L;
            }
            else
                q->exp_frac = new_exp_frac;

            /* Gain experience */
            player_exp_gain(q, new_exp);
        }
    }
}


/*
 * Add a player to another player's list of hostilities.
 */
static bool add_hostility(struct player *attacker, struct player *target, bool silent)
{
    hostile_type *h_ptr;
    char desc[NORMAL_WID];

    /* Check for sillyness */
    if (attacker == target)
    {
        /* Message */
        if (!silent) msg(attacker, "You cannot be hostile toward yourself.");

        return false;
    }

    /* Check for existance */
    if (!target)
    {
        /* Oops */
        if (!silent) msg(attacker, "That player is not currently in the game.");

        return false;
    }

    /* Make sure players aren't in the same party */
    if (in_party(target, attacker->party))
    {
        /* Message */
        if (!silent)
        {
            my_strcpy(desc, target->name, sizeof(desc));
            my_strcap(desc);
            msg(attacker, "%s is in your party!", desc);
        }

        return false;
    }

    /* Ensure we don't add the same player twice */
    for (h_ptr = attacker->hostile; h_ptr; h_ptr = h_ptr->next)
    {
        /* Check this ID */
        if (h_ptr->id == target->id)
        {
            /* Message */
            if (!silent) msg(attacker, "You are already hostile toward %s.", target->name);

            return false;
        }
    }

    /* Create a new hostility node */
    h_ptr = mem_zalloc(sizeof(hostile_type));

    /* Set ID in node */
    h_ptr->id = target->id;

    /* Put this node at the beginning of the list */
    h_ptr->next = attacker->hostile;
    attacker->hostile = h_ptr;

    /* Message */
    msgt(attacker, MSG_HOSTILE, "You are now hostile toward %s.", target->name);

    /* Notify the victim */
    my_strcpy(desc, attacker->name, sizeof(desc));
    my_strcap(desc);
    msgt(target, MSG_HOSTILE, "%s is now hostile towards you.", desc);

    /* Success */
    return true;
}


/*
 * Remove an entry from a player's list of hostilities
 */
static bool remove_hostility(struct player *attacker, struct player *target, bool silent)
{
    hostile_type *h_ptr, *temp;

    /* Check for sillyness */
    if (attacker == target)
    {
        /* Message */
        if (!silent) msg(attacker, "You are not hostile toward yourself.");

        return false;
    }

    /* Check for existance */
    if (!target)
    {
        /* Oops */
        if (!silent) msg(attacker, "That player is not currently in the game.");

        return false;
    }

    /* Initialize lock-step */
    temp = NULL;

    /* Search entries */
    for (h_ptr = attacker->hostile; h_ptr; temp = h_ptr, h_ptr = h_ptr->next)
    {
        /* Check entry */
        if (h_ptr->id == target->id)
        {
            /* Delete this entry */
            if (temp)
            {
                /* Skip over */
                temp->next = h_ptr->next;
            }
            else
            {
                /* Adjust beginning of list */
                attacker->hostile = h_ptr->next;
            }

            /* Message */
            msg(attacker, "No longer hostile toward %s.", target->name);

            /* Delete node */
            mem_free(h_ptr);

            /* Success */
            return true;
        }
    }

    /* Message */
    if (!silent) msg(attacker, "You are not hostile toward %s.", target->name);

    /* Failure */
    return false;
}


/*
 * Main PvP handling function
 *
 * This function should be called each time an action involves PvP combat.
 *
 * Modes:
 *  PVP_CHECK_ONE: checks minimal hostility between players
 *  PVP_CHECK_BOTH: checks hostility between players
 *  PVP_DIRECT: checks if direct damage must be applied
 *  PVP_INDIRECT: checks if indirect damage must be applied
 *  PVP_ADD: adds hostility
 *  PVP_REMOVE: removes hostility
 *
 * Returns true if a hostile response must be taken
 */
bool pvp_check(struct player *attacker, struct player *target, int mode, bool silent, byte feat)
{
    /* Paranoia: we cannot be hostile toward self! */
    if (attacker == target) return false;

    switch (mode)
    {
        case PVP_CHECK_ONE:
        {
            /* Always safe */
            if (feat_issafefloor(feat)) return false;

            /* Same party: can't be hostile */
            if (in_party(target, attacker->party)) return false;

            /* Brutal mode: always hostile */
            if (cfg_pvp_hostility == PVP_BRUTAL) return true;

            /* One player must be hostile */
            return (check_hostile(attacker, target) || check_hostile(target, attacker));
        }
        case PVP_INDIRECT:
        {
            /* Always safe */
            if (feat_issafefloor(feat)) return false;

            /* Same party: can't be hostile */
            if (in_party(target, attacker->party)) return false;

            /* Dangerous mode: add hostilities and take a hostile response */
            if (cfg_pvp_hostility == PVP_DANGEROUS)
            {
                add_hostility(attacker, target, true);
                add_hostility(target, attacker, true);
                return true;
            }

            /* Brutal mode: always hostile */
            if (cfg_pvp_hostility == PVP_BRUTAL) return true;

            /* Both players must be hostile */
            return (check_hostile(attacker, target) && check_hostile(target, attacker));
        }
        case PVP_CHECK_BOTH:
        case PVP_DIRECT:
        {
            /* Always safe */
            if (feat_issafefloor(feat)) return false;

            /* Same party: can't be hostile */
            if (in_party(target, attacker->party)) return false;

            /* Brutal mode: always hostile */
            if (cfg_pvp_hostility == PVP_BRUTAL) return true;

            /* Both players must be hostile */
            return (check_hostile(attacker, target) && check_hostile(target, attacker));
        }
        case PVP_ADD:
        {
            /* No-PvP mode: can't be hostile */
            if (cfg_pvp_hostility == PVP_DISABLED)
            {
                if (!silent)
                    msg(attacker, "You cannot be hostile toward this player.");
            }

            /* Brutal mode: already hostile */
            else if (cfg_pvp_hostility == PVP_BRUTAL)
            {
                if (!silent)
                    msg(attacker, "You are already hostile toward this player.");
            }

            /* Add hostility */
            else
            {
                bool hostile = add_hostility(attacker, target, silent);

                /* Unsafe modes make both players hostile */
                if ((cfg_pvp_hostility != PVP_SAFE) && hostile)
                    add_hostility(target, attacker, true);
            }
            break;
        }
        case PVP_REMOVE:
        {
            /* No-PvP mode: can't be hostile */
            if (cfg_pvp_hostility == PVP_DISABLED)
            {
                if (!silent)
                    msg(attacker, "You are not hostile toward this player.");
            }

            /* Brutal mode: always hostile */
            else if (cfg_pvp_hostility == PVP_BRUTAL)
            {
                if (!silent)
                    msg(attacker, "You cannot make peace with this player.");
            }

            /* Remove hostility */
            else
            {
                bool unhostile = remove_hostility(attacker, target, silent);

                /* Always un-hostile both players */
                if (unhostile) remove_hostility(target, attacker, true);
            }
            break;
        }
    }

    return false;
}


void do_cmd_party(struct player *p, s16b command, char *buf)
{
    /* Check arena */
    if (p->arena_num != -1)
    {
        msg(p, "You cannot access party commands while inside an arena.");
        return;
    }

    switch (command)
    {
        case PARTY_CREATE:
        {
            party_create(p, buf);
            break;
        }

        case PARTY_ADD:
        {
            party_add(p, buf);
            break;
        }

        case PARTY_DELETE:
        {
            party_remove(p, buf);
            break;
        }

        case PARTY_REMOVE_ME:
        {
            party_leave(p);
            break;
        }

        case PARTY_HOSTILE:
        {
            pvp_check(p, player_lookup(buf), PVP_ADD, false, 0x00);
            break;
        }

        case PARTY_PEACE:
        {
            pvp_check(p, player_lookup(buf), PVP_REMOVE, false, 0x00);
            break;
        }
    }
}


/* Message nearby party members */
void party_msg_near(struct player *p, const char *msg)
{
    char buf[MSG_LEN];
    int i;
    int y = p->py;
    int x = p->px;
    int party = p->party;

    /* Not a member of any party */
    if (!party) return;

    /* Format the message */
    strnfmt(buf, sizeof(buf), "%s%s", p->name, msg);

    /* Display */
    for (i = 1; i <= NumPlayers; i++)
    {
        /* Check this player */
        struct player *q = player_get(i);

        /* Don't send the message to the player who caused it */
        if (p == q) continue;

        /* Make sure this player is at this depth */
        if (!COORDS_EQUAL(&q->wpos, &p->wpos)) continue;

        /* Meh, different party */
        if (!player_in_party(party, q)) continue;

        /* Can he see this player? */
        if (square_isview(q, y, x))
        {
            /* Send the message */
            msg_print(q, buf, MSG_PY_MISC);
        }
    }
}


/*** Player database ***/


/* The hash table itself */
static hash_entry *hash_table[NUM_HASH_ENTRIES];


/*
 * Return the slot in which an ID should be stored.
 */
static int hash_slot(int id)
{
    /* Be very efficient */
    return (id & (NUM_HASH_ENTRIES - 1));
}


/*
 * Lookup a player entry by ID. Will return NULL if the entry doesn't exist.
 */
hash_entry *lookup_player(int id)
{
    int slot;
    hash_entry *ptr;

    /* Get the slot */
    slot = hash_slot(id);

    /* Acquire the pointer to the first element in the chain */
    ptr = hash_table[slot];

    /* Search the chain, looking for the correct ID */
    while (ptr)
    {
        /* Check this entry */
        if (ptr->id == id) return ptr;

        /* Next entry in chain */
        ptr = ptr->next;
    }

    /* Not found */
    return NULL;
}


/*
 * Lookup a player entry by name. Will return NULL if the entry doesn't exist.
 */
hash_entry *lookup_player_by_name(const char *name)
{
    hash_entry *ptr;
    int i;

    /* Search in each array slot */
    for (i = 0; i < NUM_HASH_ENTRIES; i++)
    {
        /* Acquire pointer to this chain */
        ptr = hash_table[i];

        /* Check all entries in this chain */
        while (ptr)
        {
            /* Check this name */
            if (!strcmp(ptr->name, name)) return ptr;

            /* Next entry in chain */
            ptr = ptr->next;
        }
    }

    /* Not found */
    return NULL;
}


/*
 * Add a name to the hash table.
 */
void add_player_name(int id, u32b account, const char *name, hturn *death_turn)
{
    int slot;
    hash_entry *ptr;

    /* Get the destination slot */
    slot = hash_slot(id);

    /* Create a new hash entry struct */
    ptr = mem_zalloc(sizeof(hash_entry));

    /* Make a copy of the player name in the entry */
    ptr->name = string_make(name);

    /* Set the entry's id */
    ptr->id = id;

    /* Set the entry's account id */
    ptr->account = account;

    /* Set the entry's time of death */
    ht_copy(&ptr->death_turn, death_turn);

    /* Add the rest of the chain to this entry */
    ptr->next = hash_table[slot];

    /* Put this entry in the table */
    hash_table[slot] = ptr;
}


/*
 * Remove a player by name.
 */
void remove_player_name(const char *name)
{
    hash_entry *ptr;

    /* Search for the entry */
    ptr = lookup_player_by_name(name);

    /* Mark this character as "dead" */
    if (ptr) ht_copy(&ptr->death_turn, &turn);
}


/*
 * Delete a player by name.
 *
 * This is useful for fault tolerance, as it is possible to have
 * two entries for one player name, if the server crashes hideously
 * or the machine has a power outage or something.
 */
void delete_player_name(const char *name)
{
    int i;
    hash_entry *ptr, *old_ptr;

    /* Search in each array slot */
    for (i = 0; i < NUM_HASH_ENTRIES; i++)
    {
        /* Acquire pointer to this chain */
        ptr = hash_table[i];

        /* Keep a pointer one step behind this one */
        old_ptr = NULL;

        /* Check all entries in this chain */
        while (ptr)
        {
            /* Check this name */
            if (!strcmp(ptr->name, name))
            {
                hash_entry *next = ptr->next;

                /* Delete this one from the table */
                if (old_ptr == NULL)
                    hash_table[i] = next;
                else old_ptr->next = next;

                /* Free the memory in the player name */
                string_free(ptr->name);

                /* Free the memory for this struct */
                mem_free(ptr);

                /* Advance to next entry in the chain */
                ptr = next;
            }
            else
            {
                /* Remember this entry */
                old_ptr = ptr;

                /* Advance to next entry in the chain */
                ptr = ptr->next;
            }
        }
    }
}


/*
 * Return a list of the player ID's stored in the table.
 */
u32b player_id_count(u32b account)
{
    int i;
    u16b len = 0;
    hash_entry *ptr;

    /* Count up the number of valid entries */
    for (i = 0; i < NUM_HASH_ENTRIES; i++)
    {
        /* Acquire this chain */
        ptr = hash_table[i];

        /* Check this chain */
        while (ptr)
        {
            /* One more entry */
            if (!account || (ptr->account == account)) len++;

            /* Next entry in chain */
            ptr = ptr->next;
        }
    }

    /* Return length */
    return len;
}


/*
 * Return a list of the player ID's stored in the table.
 */
u32b player_id_list(int **list, u32b account)
{
    int i, k = 0;
    u32b len = player_id_count(account);
    hash_entry *ptr;

    /* Nothing to do */
    if (!len) return 0;

    /* Allocate memory for the list */
    (*list) = mem_zalloc(len * sizeof(int));

    /* Look again, this time storing ID's */
    for (i = 0; i < NUM_HASH_ENTRIES; i++)
    {
        /* Acquire this chain */
        ptr = hash_table[i];

        /* Check this chain */
        while (ptr)
        {
            /* Store this ID */
            if (!account || (ptr->account == account)) (*list)[k++] = ptr->id;

            /* Next entry in chain */
            ptr = ptr->next;
        }
    }

    /* Return length */
    return len;
}


/*
 * Purge the player database.
 */
void purge_player_names(void)
{
    int i;
    hash_entry *ptr, *old_ptr;

    /* Entry points */
    for (i = 0; i < NUM_HASH_ENTRIES; i++)
    {
        /* Acquire this chain */
        ptr = hash_table[i];

        /* Keep a pointer one step behind this one */
        old_ptr = NULL;

        /* Check this chain for expired characters */
        while (ptr)
        {
            /* Check this one */
            if (!player_expiry(&ptr->death_turn))
            {
                hash_entry *next = ptr->next;

                /* Delete this one from the table */
                if (old_ptr == NULL) hash_table[i] = next;
                else old_ptr->next = next;

                /* Free the memory in the player name */
                string_free(ptr->name);

                /* Free the memory for this struct */
                mem_free(ptr);

                /* Advance to next entry in the chain */
                ptr = next;
            }
            else
            {
                /* Remember this entry */
                old_ptr = ptr;

                /* Advance to next entry in the chain */
                ptr = ptr->next;
            }
        }
    }
}


/*
 * Free player names from memory
 */
void wipe_player_names(void)
{
    int i;
    hash_entry *ptr;
    hash_entry *next;

    /* Entry points */
    for (i = 0; i < NUM_HASH_ENTRIES; i++)
    {
        /* Acquire this chain */
        ptr = hash_table[i];

        /* Check this chain */
        while (ptr)
        {
            next = ptr->next;

            string_free(ptr->name);
            mem_free(ptr);

            ptr = next;
        }
    }
}


/*
 * Returns the number of days before expiration
 * Returns -1 if the character is still alive
 * Returns 0 if the character is dead and has expired
 */
int player_expiry(hturn *death_turn)
{
    int dd, dh, dm;
    int sd, sh, sm;

    /* Character is still alive */
    if (ht_zero(death_turn)) return -1;

    /* Get number of minutes since death */
    get_real_time(death_turn, &dd, &dh, &dm);
    dm = dm + 60 * dh + 1440 * dd;

    /* Get number of minutes since the server started */
    get_real_time(&turn, &sd, &sh, &sm);
    sm = sm + 60 * sh + 1440 * sd;

    /* Get number of days elapsed */
    dd = (sm - dm) / 1440;

    /* Character has expired */
    if (dd > EXPIRY_DELAY_DEAD) dd = EXPIRY_DELAY_DEAD;

    return (EXPIRY_DELAY_DEAD - dd);
}
