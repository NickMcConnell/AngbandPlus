/*
 * File: cmd-obj.c
 * Purpose: Handle objects in various ways
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * Copyright (c) 2007-9 Andi Sidwell, Chris Carr, Ed Graham, Erik Osheim
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


/*
 * Utility bits and bobs
 */


/*
 * Check to see if the player can use a rod/wand/staff/activatable object.
 */
static bool check_devices(struct player *p, struct object *obj)
{
    int fail;
    const char *action;
    bool activated = false;

    /* Horns are not magical and therefore never fail */
    if (tval_is_horn(obj)) return true;

    /* Get the right string */
    if (tval_is_rod(obj))
        action = "zap the rod";
    else if (tval_is_wand(obj))
        action = "use the wand";
    else if (tval_is_staff(obj))
        action = "use the staff";
    else
    {
        action = "activate it";
        activated = true;
    }

    /* Figure out how hard the item is to use */
    fail = get_use_device_chance(p, obj);

    /* Roll for usage */
    if (CHANCE(fail, 1000))
    {
        msg(p, "You failed to %s properly.", action);
        return false;
    }

    /* Notice activations */
    if (activated) object_notice_effect(p, obj);

    return true;
}


/*
 * Print an artifact activation message.
 */
static void activation_message(struct player *p, struct object *obj)
{
    const char *message;

    /* See if we have a message */
    if (!obj->activation) return;
    if (!obj->activation->message) return;
    if (true_artifact_p(obj) && obj->artifact->alt_msg)
        message = obj->artifact->alt_msg;
    else
        message = obj->activation->message;

    print_custom_message(p, obj, message, MSG_GENERIC);
}


/*
 * Inscriptions
 */


/*
 * Remove inscription
 */
void do_cmd_uninscribe(struct player *p, int item)
{
    struct object *obj = object_from_index(p, item, true, true);

    /* Paranoia: requires an item */
    if (!obj) return;

    /* Restrict ghosts */
    /* One exception: players in undead form can uninscribe items (from pack only) */
    if (p->ghost && !(p->dm_flags & DM_GHOST_HANDS) &&
        !(player_undead(p) && object_is_carried(p, obj)))
    {
        msg(p, "You cannot uninscribe items!");
        return;
    }

    /* Check preventive inscription '!g' */
    if (!object_is_carried(p, obj) &&
        object_prevent_inscription(p, obj, INSCRIPTION_PICKUP, false))
    {
        msg(p, "The item's inscription prevents it.");
        return;
    }

    /* Nothing to remove */
    if (!obj->note)
    {
        msg(p, "That item had no inscription to remove.");
        return;
    }

    /* Check preventive inscription '!}' */
    if (protected_p(p, obj, INSCRIPTION_UNINSCRIBE, false))
    {
        msg(p, "The item's inscription prevents it.");
        return;
    }

    obj->note = 0;
    msg(p, "Inscription removed.");

    /* PWMAngband: remove autoinscription if aware */
    if (p->obj_aware[obj->kind->kidx])
    {
        remove_autoinscription(p, obj->kind->kidx);
        Send_autoinscription(p, obj->kind);
    }

    /* Update global "preventive inscriptions" */
    update_prevent_inscriptions(p);

    /* Notice, update and redraw */
    p->upkeep->notice |= (PN_COMBINE | PN_IGNORE);
    p->upkeep->update |= (PU_INVEN);
    p->upkeep->redraw |= (PR_INVEN | PR_EQUIP);
    if (!object_is_carried(p, obj))
        redraw_floor(&p->wpos, &obj->grid);
}


/*
 * Add inscription
 */
void do_cmd_inscribe(struct player *p, int item, const char *inscription)
{
    struct object *obj = object_from_index(p, item, true, true);
    char o_name[NORMAL_WID];
    s32b price;
    const char *c;

    /* Paranoia: requires an item */
    if (!obj) return;

    /* Empty inscription: uninscribe the item instead */
    if (STRZERO(inscription))
    {
        do_cmd_uninscribe(p, item);
        return;
    }

    /* Restrict ghosts */
    /* One exception: players in undead form can inscribe items (from pack only) */
    if (p->ghost && !(p->dm_flags & DM_GHOST_HANDS) &&
        !(player_undead(p) && object_is_carried(p, obj)))
    {
        msg(p, "You cannot inscribe items!");
        return;
    }

    /* Check preventive inscription '!g' */
    if (!object_is_carried(p, obj) &&
        object_prevent_inscription(p, obj, INSCRIPTION_PICKUP, false))
    {
        msg(p, "The item's inscription prevents it.");
        return;
    }

    /* Check preventive inscription '!{' */
    if (protected_p(p, obj, INSCRIPTION_INSCRIBE, true))
    {
        msg(p, "The item's inscription prevents it.");
        return;
    }

    /* Check ownership */
    if (strstr(inscription, "!g") && (p->id != obj->owner))
    {
        msg(p, "You must own this item first.");
        return;
    }

    /* Don't allow certain inscriptions when selling */
    c = my_stristr(inscription, "for sale");
    if (c)
    {
        /* Can't sell unindentified items */
        if (!object_is_known(p, obj))
        {
            msg(p, "You must identify this item first.");
            return;
        }

        /* Can't sell overpriced items */
        c += 8; /* skip "for sale" */
        if (*c == ' ')
        {
            price = atoi(c);
            if (price > PY_MAX_GOLD)
            {
                msg(p, "Your price is too high!");
                return;
            }
        }
    }

    /* Form prompt */
    object_desc(p, o_name, sizeof(o_name), obj, ODESC_PREFIX | ODESC_FULL);
    msg(p, "Inscribing %s.", o_name);
    message_flush(p);

    /* Save the inscription */
    obj->note = quark_add(inscription);

    /* PWMAngband: add autoinscription if aware and inscription has the right format (@xn) */
    if (p->obj_aware[obj->kind->kidx] && (strlen(inscription) == 3) && (inscription[0] == '@') &&
        isalpha(inscription[1]) && isdigit(inscription[2]))
    {
        add_autoinscription(p, obj->kind->kidx, inscription);
        Send_autoinscription(p, obj->kind);
    }

    /* Update global "preventive inscriptions" */
    update_prevent_inscriptions(p);

    /* Notice, update and redraw */
    p->upkeep->notice |= (PN_COMBINE | PN_IGNORE);
    p->upkeep->update |= (PU_INVEN);
    p->upkeep->redraw |= (PR_INVEN | PR_EQUIP);
    if (!object_is_carried(p, obj))
        redraw_floor(&p->wpos, &obj->grid);
}


/*
 * Examination
 */


void do_cmd_observe(struct player *p, int item)
{
    struct object *obj = object_from_index(p, item, true, true);
    char o_name[NORMAL_WID];

    /* Paranoia: requires an item */
    if (!obj) return;

    /* Track object for object recall */
    track_object(p->upkeep, obj);

    /* Get name */
    object_desc(p, o_name, sizeof(o_name), obj, ODESC_PREFIX | ODESC_FULL);

    /* Inform */
    msg(p, "Examining %s...", o_name);

    /* Capitalize object name for header */
    my_strcap(o_name);

    /* Display object recall modally and wait for a keypress */
    display_object_recall_interactive(p, obj, o_name);
}


/*
 * Taking off/putting on
 */


/*
 * Take off an item
 */
void do_cmd_takeoff(struct player *p, int item)
{
    struct object *obj = object_from_index(p, item, true, true);

    /* Paranoia: requires an item */
    if (!obj) return;

    /* Paranoia */
    if (!object_is_carried(p, obj)) return;

    /* Restrict ghosts */
    if (p->ghost && !(p->dm_flags & DM_GHOST_BODY))
    {
        msg(p, "You need a tangible body to remove items!");
        return;
    }

    /* Verify potential overflow */
    if (!inven_carry_okay(p, obj))
    {
        msg(p, "Your pack is full and would overflow!");
        return;
    }

    /* Check preventive inscription '!t' */
    if (object_prevent_inscription(p, obj, INSCRIPTION_TAKEOFF, false))
    {
        msg(p, "The item's inscription prevents it.");
        return;
    }

    /* Item is stuck */
    if (!obj_can_takeoff(obj))
    {
        /* Oops */
        msg(p, "Hmmm, it seems to be stuck.");

        /* Nope */
        return;
    }       

    /* Take a turn */
    use_energy(p);

    /* Take off the item */
    inven_takeoff(p, obj);
    combine_pack(p);
}


/*
 * Wield or wear an item
 */
void do_cmd_wield(struct player *p, int item, int slot)
{
    struct object *equip_obj;
    char o_name[NORMAL_WID];
    const char *act;
    struct object *obj = object_from_index(p, item, true, true);

    /* Paranoia: requires an item */
    if (!obj) return;

    /* Restrict ghosts */
    if (p->ghost && !(p->dm_flags & DM_GHOST_BODY))
    {
        msg(p, "You need a tangible body to wield items!");
        return;
    }

    /* Some checks */
    if (!object_is_carried(p, obj))
    {
        /* Winners cannot pickup artifacts except the Crown and Grond */
        if (true_artifact_p(obj) && restrict_winner(p, obj))
        {
            msg(p, "You cannot wield that item anymore.");
            return;
        }

        /* Restricted by choice */
        if (obj->artifact && (cfg_no_artifacts || OPT(p, birth_no_artifacts)))
        {
            msg(p, "You cannot wield that item.");
            return;
        }

        /* Note that the pack is too heavy */
        if (p->upkeep->total_weight + obj->weight > weight_limit(&p->state) * 6)
        {
            msg(p, "You are already too burdened to wield that item.");
            return;
        }

        /* Restricted by choice */
        if (!is_owner(p, obj))
        {
            msg(p, "This item belongs to someone else!");
            return;
        }

        /* Must meet level requirement */
        if (!has_level_req(p, obj))
        {
            msg(p, "You don't have the required level!");
            return;
        }

        /* Check preventive inscription '!g' */
        if (object_prevent_inscription(p, obj, INSCRIPTION_PICKUP, false))
        {
            msg(p, "The item's inscription prevents it.");
            return;
        }
    }

    /* Check preventive inscription '!w' */
    if (object_prevent_inscription(p, obj, INSCRIPTION_WIELD, false))
    {
        msg(p, "The item's inscription prevents it.");
        return;
    }

    /* Paranoia: requires proper item */
    if (!item_tester_hook_wear(p, obj)) return;

    /* Paranoia */
    if (slot == -1) return;

    /* Get the slot the object wants to go in, and the item currently there */
    equip_obj = slot_object(p, slot);

    /* If the slot is open, wield and be done */
    if (!equip_obj)
    {
        inven_wield(p, obj, slot);
        return;
    }

    /* Prevent wielding into a stuck slot */
    if (!obj_can_takeoff(equip_obj))
    {
        object_desc(p, o_name, sizeof(o_name), equip_obj, ODESC_BASE);
        msg(p, "The %s you are %s appears to be stuck.", o_name, equip_describe(p, slot));
        return;
    }

    /* Check preventive inscription '!t' */
    if (object_prevent_inscription(p, equip_obj, INSCRIPTION_TAKEOFF, false))
    {
        msg(p, "The item's inscription prevents it.");
        return;
    }

    /* Never drop true artifacts above their base depth except the Crown and Grond */
    if (!inven_carry_okay(p, equip_obj) && !inven_drop_okay(p, equip_obj))
    {
        object_desc(p, o_name, sizeof(o_name), equip_obj, ODESC_BASE);
        msg(p, "Your pack is full and you can't drop the %s here.", o_name);
        return;
    }

    /*
     * Hack -- prevent anyone but total winners from wielding the Massive Iron
     * Crown of Morgoth or the Mighty Hammer 'Grond'.
     */
    if (!p->total_winner)
    {
        /*
         * Attempting to wear the crown if you are not a winner is a very,
         * very bad thing to do.
         */
        if (true_artifact_p(obj) && strstr(obj->artifact->name, "of Morgoth"))
        {
            msg(p, "You are blasted by the Crown's power!");

            /* This should pierce invulnerability */
            take_hit(p, 10000, "the Massive Iron Crown of Morgoth", false,
                "was blasted by the Massive Iron Crown of Morgoth");
            return;
        }

        /* Attempting to wield Grond isn't so bad. */
        if (true_artifact_p(obj) && strstr(obj->artifact->name, "Grond"))
        {
            msg(p, "You are far too weak to wield the mighty Grond.");
            return;
        }
    }

    /* Describe the object */
    object_desc(p, o_name, sizeof(o_name), equip_obj, ODESC_PREFIX | ODESC_FULL);

    /* Describe removal by slot */
    if (slot_type_is(p, slot, EQUIP_WEAPON))
        act = "You were wielding";
    else if (slot_type_is(p, slot, EQUIP_BOW) || slot_type_is(p, slot, EQUIP_LIGHT))
        act = "You were holding";
    else if (slot_type_is(p, slot, EQUIP_TOOL))
        act = "You were using";
    else
        act = "You were wearing";

    /* Message */
    msgt(p, MSG_WIELD, "%s %s (%c).", act, o_name, gear_to_label(p, equip_obj));

    inven_wield(p, obj, slot);
}


/*
 * Drop an item
 */
void do_cmd_drop(struct player *p, int item, int quantity)
{
    struct object *obj = object_from_index(p, item, true, true);

    /* Paranoia: requires an item */
    if (!obj) return;

    /* Paranoia */
    if (!object_is_carried(p, obj)) return;

    /* Restrict ghosts */
    if (p->ghost && !(p->dm_flags & DM_GHOST_BODY))
    {
        msg(p, "You need a tangible body to drop items!");
        return;
    }

    /* Handle the newbies_cannot_drop option */
    if (newbies_cannot_drop(p))
    {
        msg(p, "You are not experienced enough to drop items.");
        return;
    }

    /* Check preventive inscription '^d' */
    if (check_prevent_inscription(p, INSCRIPTION_DROP))
    {
        msg(p, "The item's inscription prevents it.");
        return;
    }

    /* Check preventive inscription '!d' */
    if (object_prevent_inscription(p, obj, INSCRIPTION_DROP, false))
    {
        msg(p, "The item's inscription prevents it.");
        return;
    }

    /* Cannot remove stuck items */
    if (object_is_equipped(p->body, obj) && !obj_can_takeoff(obj))
    {
        msg(p, "Hmmm, it seems to be stuck.");
        return;
    }

    /* Take a turn */
    use_energy(p);

    /* Drop (some of) the item */
    inven_drop(p, obj, quantity, false);
}


/*
 * Destroy an item
 */
void do_cmd_destroy(struct player *p, int item, bool des)
{
    struct object *obj = object_from_index(p, item, true, true);
    char o_name[NORMAL_WID];

    /* Paranoia: requires an item */
    if (!obj) return;

    /* Restrict ghosts */
    if (des && p->ghost && !(p->dm_flags & DM_GHOST_BODY))
    {
        msg(p, "You need a tangible body to destroy items!");
        return;
    }

    /* Check preventive inscription '^k' */
    if (check_prevent_inscription(p, INSCRIPTION_DESTROY))
    {
        msg(p, "The item's inscription prevents it.");
        return;
    }

    /* Some checks */
    if (!object_is_carried(p, obj))
    {
        /* Restricted by choice */
        if (!is_owner(p, obj))
        {
            msg(p, "This item belongs to someone else!");
            return;
        }

        /* Must meet level requirement */
        if (!has_level_req(p, obj))
        {
            msg(p, "You don't have the required level!");
            return;
        }

        /* Check preventive inscription '!g' */
        if (object_prevent_inscription(p, obj, INSCRIPTION_PICKUP, false))
        {
            msg(p, "The item's inscription prevents it.");
            return;
        }
    }

    /* Check preventive inscription '!k' */
    if (object_prevent_inscription(p, obj, INSCRIPTION_DESTROY, false))
    {
        msg(p, "The item's inscription prevents it.");
        return;
    }

    /* Can't ignore or destroy stuck items we're wielding. */
    if (object_is_equipped(p->body, obj) && !obj_can_takeoff(obj))
    {
        /* Message */
        if (des) msg(p, "You cannot destroy the stuck item.");
        else msg(p, "You cannot ignore stuck equipment.");

        /* Done */
        return;
    }

    /* Describe */
    object_desc(p, o_name, sizeof(o_name), obj, ODESC_PREFIX | ODESC_FULL);

    /* Artifacts cannot be destroyed */
    if (des && obj->artifact)
    {
        /* Message */
        msg(p, "You cannot destroy %s.", o_name);

        /* Done */
        return;
    }

    /* Destroying or ignoring from equipment? Update object flags! */
    if (object_is_equipped(p->body, obj)) p->upkeep->redraw |= (PR_EQUIP);

    /* Destroy */
    if (des)
    {
        /* Message */
        msgt(p, MSG_DESTROY, "You destroy %s.", o_name);

        /* Eliminate the item */
        use_object(p, obj, obj->number, true);
    }

    /* Ignore */
    else
    {
        /* Message */
        if (obj->known->notice & OBJ_NOTICE_IGNORE)
            msgt(p, MSG_DESTROY, "Showing %s again.", o_name);
        else
            msgt(p, MSG_DESTROY, "Ignoring %s.", o_name);

        /* Set ignore flag as appropriate */
        p->upkeep->notice |= PN_IGNORE;

        /* Toggle ignore */
        if (obj->known->notice & OBJ_NOTICE_IGNORE)
            obj->known->notice &= ~OBJ_NOTICE_IGNORE;
        else
            obj->known->notice |= OBJ_NOTICE_IGNORE;
    }
}


/*
 * Casting and browsing
 */


/*
 * Determine if a spell is "okay" for the player to cast or study
 * The spell must be legible, not forgotten, and also, to cast,
 * it must be known, and to study, it must not be known.
 */
static bool spell_okay(struct player *p, int spell_index, bool known)
{
    const struct class_spell *spell = spell_by_index(&p->clazz->magic, spell_index);

    /* Spell is illegible - never ok */
    if (spell->slevel > p->lev) return false;

    /* Spell is forgotten - never ok */
    if (p->spell_flags[spell_index] & PY_SPELL_FORGOTTEN) return false;

    /* Spell is learned - cast ok, no study */
    if (p->spell_flags[spell_index] & PY_SPELL_LEARNED) return (known);

    /* Spell has never been learned - study ok, no cast */
    return (!known);
}


/*
 * Allow user to choose a spell/prayer from the given book.
 *
 * Returns -1 if the user hits escape.
 * Returns -2 if there are no legal choices.
 * Returns a valid spell otherwise.
 *
 * The "prompt" should be "cast", "recite", "study", or "use"
 * The "known" should be true for cast/pray, false for study
 */
static int get_spell(struct player *p, struct object *obj, int spell_index, const char *prompt,
    bool known)
{
    const struct class_book *book = player_object_to_book(p, obj);
    int sidx;

    /* Set the spell number */
    if (spell_index < p->clazz->magic.total_spells)
        sidx = book->spells[spell_index].sidx;
    else
    {
        const struct class_spell *spell = &book->spells[spell_index - p->clazz->magic.total_spells];

        sidx = spell->sidx;

        /* Projected spells */
        if (!spell->sproj)
        {
            msg(p, "You cannot project that spell.");
            return -1;
        }
    }

    /* Verify the spell */
    if (!spell_okay(p, sidx, known))
    {
        if (prompt) msg(p, prompt);
        return -1;
    }

    return sidx;
}


/* Study a book to gain a new spell */
void do_cmd_study(struct player *p, int book_index, int spell_index)
{
    const struct class_book *book;
    int sidx = -1;
    const struct class_spell *spell;
    int i, k = 0;
    struct object *obj = object_from_index(p, book_index, true, true);

    /* Paranoia: requires an item */
    if (!obj) return;

    /* Restrict ghosts */
    /* One exception: players in undead form can read books (from pack only) */
    if (p->ghost && !(p->dm_flags & DM_GHOST_HANDS) &&
        !(player_undead(p) && object_is_carried(p, obj)))
    {
        msg(p, "You cannot read books!");
        return;
    }

    if (!player_can_cast(p, true)) return;

    /* Check preventive inscription '^G' */
    if (check_prevent_inscription(p, INSCRIPTION_STUDY))
    {
        msg(p, "The item's inscription prevents it.");
        return;
    }

    /* Restricted by choice */
    if (!object_is_carried(p, obj) && !is_owner(p, obj))
    {
        msg(p, "This item belongs to someone else!");
        return;
    }

    /* Must meet level requirement */
    if (!object_is_carried(p, obj) && !has_level_req(p, obj))
    {
        msg(p, "You don't have the required level!");
        return;
    }

    /* Get the book */
    book = player_object_to_book(p, obj);

    /* Requires a spellbook */
    if (!book) return;

    /* Elementalists can increase the power of their spells */
    if (streq(book->realm->name, "elemental"))
    {
        /* Check if spell is learned */
        sidx = get_spell(p, obj, spell_index, NULL, true);
        if (sidx != -1)
        {
            int max_power = ((obj->sval == lookup_sval(obj->tval, "[Elemental]"))? 10: 5);

            spell = &book->spells[spell_index];

            /* Check max spellpower */
            if (p->spell_power[sidx] == max_power)
            {
                msg(p, "You already know everything about this spell.");
                return;
            }

            /* Check level */
            if (spell->slevel + p->spell_power[sidx] > p->lev)
            {
                msg(p, "You are too low level to improve this spell.");
                return;
            }

            /* Check allocated points */
            max_power = 0;
            for (i = 0; i < p->clazz->magic.total_spells; i++)
                max_power += p->spell_power[i];
            if (max_power >= p->lev * 2)
            {
                msg(p, "You are too low level to improve this spell.");
                return;
            }

            /* Take a turn */
            use_energy(p);

            /* Improve spellpower */
            p->spell_power[sidx]++;

            /* Mention the result */
            msgt(p, MSG_STUDY, "You improve your knowledge of the %s spell.", spell->name);

            /* Redraw */
            p->upkeep->redraw |= PR_SPELL;

            return;
        }
    }

    if (!p->upkeep->new_spells)
    {
        msg(p, "You cannot learn any new %ss!", book->realm->spell_noun);
        return;
    }

    /* Spellcaster -- learn a selected spell */
    if (player_has(p, PF_CHOOSE_SPELLS))
    {
        const char *prompt = format("You cannot learn that %s!", book->realm->spell_noun);

        /* Ask for a spell */
        sidx = get_spell(p, obj, spell_index, prompt, false);

        /* Allow cancel */
        if (sidx == -1) return;

        spell = spell_by_index(&p->clazz->magic, sidx);
    }

    /* Cleric -- learn a random prayer */
    else
    {
        sidx = -1;

        /* Extract prayers */
        for (i = 0; i < book->num_spells; i++)
        {
            /* Skip non "okay" prayers */
            if (!spell_okay(p, book->spells[i].sidx, false)) continue;

            /* Apply the randomizer */
            if ((++k > 1) && randint0(k)) continue;

            /* Track it */
            sidx = book->spells[i].sidx;

            spell = &book->spells[i];
        }
    }

    /* Nothing to study */
    if (sidx < 0)
    {
        /* Message */
        msg(p, "You cannot learn any %ss in that book.", book->realm->spell_noun);

        /* Abort */
        return;
    }

    /* Take a turn */
    use_energy(p);

    /* Learn the spell */
    p->spell_flags[sidx] |= PY_SPELL_LEARNED;
    p->spell_power[sidx]++;

    /* Find the next open entry in "spell_order[]" */
    for (i = 0; i < p->clazz->magic.total_spells; i++)
    {
        /* Stop at the first empty space */
        if (p->spell_order[i] == 99) break;
    }

    /* Add the spell to the known list */
    p->spell_order[i++] = sidx;

    /* Mention the result */
    msgt(p, MSG_STUDY, "You have learned the %s of %s.", spell->realm->spell_noun, spell->name);

    /* One less spell available */
    p->upkeep->new_spells--;

    /* Message if needed */
    if (p->upkeep->new_spells)
    {
        /* Message */
        msg(p, "You can learn %d more %s%s.", p->upkeep->new_spells, book->realm->spell_noun,
            PLURAL(p->upkeep->new_spells));
    }

    /* Redraw */
    p->upkeep->redraw |= (PR_STUDY | PR_SPELL);
}


/* Cast the specified spell */
static bool spell_cast(struct player *p, int spell_index, int dir, quark_t note, bool projected)
{
    int chance;

    /* Spell failure chance */
    chance = spell_chance(p, spell_index);

    /* Fail or succeed */
    if (magik(chance))
        msg(p, "You failed to concentrate hard enough!");
    else
    {
        struct source who_body;
        struct source *who = &who_body;
        const struct class_spell *spell = spell_by_index(&p->clazz->magic, spell_index);
        bool pious = streq(spell->realm->name, "divine");

        /* Set current spell */
        p->current_spell = spell_index;

        /* Save current inscription */
        p->current_item = (s16b)note;

        /* Only fire in direction 5 if we have a target */
        if ((dir == DIR_TARGET) && !target_okay(p)) return false;

        source_player(who, get_player_index(get_connection(p->conn)), p);

        /* Projected */
        if (projected)
        {
            project_aimed(who, PROJ_PROJECT, dir, spell_index,
                PROJECT_STOP | PROJECT_KILL | PROJECT_PLAY, "killed");
        }

        /* Cast the spell */
        else
        {
            bool ident = false;
            struct beam_info beam;

            fill_beam_info(p, spell_index, &beam);

            if (spell->effect && spell->effect->other_msg)
                msg_print_near(p, (pious? MSG_PY_PRAYER: MSG_PY_SPELL), spell->effect->other_msg);
            if (!effect_do(spell->effect, who, &ident, true, dir, &beam, 0, note, NULL))
                return false;
        }

        /* A spell was cast */
        sound(p, (pious? MSG_PRAYER: MSG_SPELL));

        cast_spell_end(p);

        /* Put on cooldown */
        p->spell_cooldown[spell->sidx] = spell->cooldown;
    }

    /* Use some mana */
    use_mana(p);

    return true;
}


/*
 * Unknown item hook for get_item()
 */
static bool item_tester_unknown(struct player *p, const struct object *obj)
{
    return (object_runes_known(obj)? false: true);
}


/*
 * Returns true if there are any objects available to identify (whether on floor or in gear).
 */
static bool spell_identify_unknown_available(struct player *p)
{
    int floor_max = z_info->floor_size;
    struct object **floor_list = mem_zalloc(floor_max * sizeof(*floor_list));
    int floor_num;
    struct object *obj;
    bool unidentified_gear = false;

    floor_num = scan_floor(p, chunk_get(&p->wpos), floor_list, floor_max,
        OFLOOR_TEST | OFLOOR_SENSE | OFLOOR_VISIBLE, item_tester_unknown);
    for (obj = p->gear; obj; obj = obj->next)
    {
        if (object_test(p, item_tester_unknown, obj))
        {
            unidentified_gear = true;
            break;
        }
    }
    mem_free(floor_list);

    return (unidentified_gear || (floor_num > 0));
}


/*
 * Cast a spell from a book
 */
bool do_cmd_cast(struct player *p, int book_index, int spell_index, int dir)
{
    const char *prompt;
    const struct class_spell *spell;
    struct object *obj = object_from_index(p, book_index, true, true);
    const struct class_book *book;
    int sidx;

    /* Cancel repeat */
    if (!p->firing_request) return true;

    /* Check energy */
    if (!has_energy(p, true)) return false;

    /* Paranoia: requires an item */
    if (!obj)
    {
        /* Cancel repeat */
        disturb(p, 0);
        return true;
    }

    /* Clear current */
    current_clear(p);

    /* Check the player can cast spells at all */
    if (!player_can_cast(p, true))
    {
        /* Cancel repeat */
        disturb(p, 0);
        return true;
    }

    /* Check preventive inscription '^m' */
    if (check_prevent_inscription(p, INSCRIPTION_CAST))
    {
        msg(p, "The item's inscription prevents it.");

        /* Cancel repeat */
        disturb(p, 0);
        return true;
    }

    /* Restricted by choice */
    if (!object_is_carried(p, obj) && !is_owner(p, obj))
    {
        msg(p, "This item belongs to someone else!");

        /* Cancel repeat */
        disturb(p, 0);
        return true;
    }

    /* Must meet level requirement */
    if (!object_is_carried(p, obj) && !has_level_req(p, obj))
    {
        msg(p, "You don't have the required level!");

        /* Cancel repeat */
        disturb(p, 0);
        return true;
    }

    /* Get the book */
    book = player_object_to_book(p, obj);

    /* Requires a spellbook */
    if (!book)
    {
        /* Cancel repeat */
        disturb(p, 0);
        return true;
    }

    /* Check preventive inscription '!m' */
    if (object_prevent_inscription(p, obj, INSCRIPTION_CAST, false))
    {
        msg(p, "The item's inscription prevents it.");

        /* Cancel repeat */
        disturb(p, 0);
        return true;
    }

    /* Restrict ghosts */
    /* One exception: players in undead form can cast spells (from pack only) */
    if (p->ghost && !(p->dm_flags & DM_GHOST_HANDS) &&
        !(player_undead(p) && object_is_carried(p, obj)))
    {
        msg(p, "You cannot %s that %s.", book->realm->verb, book->realm->spell_noun);

        /* Cancel repeat */
        disturb(p, 0);
        return true;
    }

    /* Ask for a spell */
    prompt = format("You cannot %s that %s.", book->realm->verb, book->realm->spell_noun);
    sidx = get_spell(p, obj, spell_index, prompt, true);
    if (sidx == -1)
    {
        /* Cancel repeat */
        disturb(p, 0);
        return true;
    }

    /* Get the spell */
    spell = spell_by_index(&p->clazz->magic, sidx);

    /* Check for unknown objects to prevent wasted player turns. */
    if (spell_is_identify(p, sidx) && !spell_identify_unknown_available(p))
    {
        msg(p, "You have nothing to identify.");

        /* Cancel repeat */
        disturb(p, 0);
        return true;
    }

    /* Check mana */
    if ((spell->smana > p->csp) && !OPT(p, risky_casting))
    {
        /* Warning */
        msg(p, "You do not have enough mana to %s this %s.", spell->realm->verb,
            spell->realm->spell_noun);

        /* Cancel repeat */
        disturb(p, 0);
        return true;
    }

    /* Check cooldown */
    if (p->spell_cooldown[spell->sidx])
    {
        /* Warning */
        msg(p, "This %s is on cooldown.", spell->realm->spell_noun);

        /* Cancel repeat */
        disturb(p, 0);
        return true;
    }

    /* Antimagic field (no effect on psi powers which are not "magical") */
    if (strcmp(book->realm->name, "psi") && check_antimagic(p, chunk_get(&p->wpos), NULL))
    {
        use_energy(p);

        /* Cancel repeat */
        disturb(p, 0);
        return true;
    }

    /* Spell cost */
    p->spell_cost = spell->smana;

    /* Cast a spell */
    if (!spell_cast(p, sidx, dir, obj->note,
        (spell_index >= p->clazz->magic.total_spells)? true: false))
    {
        /* Cancel repeat */
        disturb(p, 0);
        return true;
    }

    /* Take a turn, or half a turn if fast casting */
    if (p->timed[TMD_FASTCAST])
        p->energy -= move_energy(p->wpos.depth) / 2;
    else
        p->energy -= move_energy(p->wpos.depth);
    if (p->energy < 0) p->energy = 0;

    /* Repeat */
    if (p->firing_request > 0) p->firing_request--;
    if (p->firing_request > 0) cmd_cast(p, book_index, spell_index, dir);
    return true;
}


/*
 * Using items the traditional way
 */


/* Basic tval testers */
static bool item_tester_hook_use(struct player *p, struct object *obj)
{
    /* Non-staves are out */
    if (!tval_is_staff(obj)) return false;

    /* Notice empty staves */
    if (obj->pval <= 0)
    {
        msg(p, "The staff has no charges left.");
        return false;
    }

    /* Otherwise OK */
    return true;
}


static bool item_tester_hook_aim(struct player *p, struct object *obj)
{
    /* Non-wands are out */
    if (!tval_is_wand(obj)) return false;

    /* Notice empty wands */
    if (obj->pval <= 0)
    {
        msg(p, "The wand has no charges left.");
        return false;
    }

    /* Otherwise OK */
    return true;
}


static bool item_tester_hook_eat(struct player *p, struct object *obj)
{
    return tval_is_edible(obj);
}


static bool item_tester_hook_quaff(struct player *p, struct object *obj)
{
    return tval_is_potion(obj);
}


static bool item_tester_hook_read(struct player *p, struct object *obj)
{
    return tval_is_scroll(obj);
}


/* Determine if an object is zappable */
static bool item_tester_hook_zap(struct player *p, struct object *obj)
{
    /* Non-rods are out */
    if (!tval_is_rod(obj)) return false;

    /* All still charging? */
    if (number_charging(obj) == obj->number)
    {
        msg(p, "The rod is still charging.");
        return false;
    }

    /* Otherwise OK */
    return true;
}


/* Determine if an object is activatable */
static bool item_tester_hook_activate(struct player *p, struct object *obj)
{
    /* Check the recharge */
    if (obj->timeout)
    {
        msg(p, "The item is still charging.");
        return false;
    }

    /* Check effect */
    if (object_effect(obj)) return true;

    /* Assume not */
    return false;
}


/* List of commands */
enum
{
    CMD_EAT = 0,
    CMD_QUAFF,
    CMD_READ,
    CMD_USE,
    CMD_AIM,
    CMD_ZAP,
    CMD_ACTIVATE
};


/* Types of item use */
enum
{
    USE_TIMEOUT,
    USE_CHARGE,
    USE_SINGLE
};


/* Command parameters */
typedef struct
{
    u32b dm_flag;
    bool player_undead;
    const char *msg_ghost;
    int p_note;
    int eq_only;
    int g_note;
    bool check_antimagic;
    int use;
    int snd;
    bool (*item_tester_hook)(struct player *, struct object *);
} cmd_param;


/* List of command parameters */
static cmd_param cmd_params[] =
{
    {DM_GHOST_BODY, false, "You need a tangible body to eat food!", INSCRIPTION_EAT, 0,
        INSCRIPTION_EAT, false, USE_SINGLE, MSG_EAT, item_tester_hook_eat},
    {DM_GHOST_BODY, false, "You need a tangible body to quaff potions!", INSCRIPTION_QUAFF, 0,
        INSCRIPTION_QUAFF, false, USE_SINGLE, MSG_QUAFF, item_tester_hook_quaff},
    {DM_GHOST_HANDS, true, "You cannot read scrolls!", INSCRIPTION_READ, 0,
        INSCRIPTION_READ, false, USE_SINGLE, MSG_GENERIC, item_tester_hook_read},
    {DM_GHOST_HANDS, true, "You cannot use staves!", INSCRIPTION_USE, 0,
        INSCRIPTION_USE, true, USE_CHARGE, MSG_USE_STAFF, item_tester_hook_use},
    {DM_GHOST_HANDS, true, "You cannot aim wands!", INSCRIPTION_AIM, 0,
        INSCRIPTION_AIM, true, USE_CHARGE, MSG_GENERIC, item_tester_hook_aim},
    {DM_GHOST_HANDS, true, "You cannot zap rods!", INSCRIPTION_ZAP, 0,
        INSCRIPTION_ZAP, true, USE_TIMEOUT, MSG_ZAP_ROD, item_tester_hook_zap},
    {DM_GHOST_BODY, true, "You need a tangible body to activate items!", -1, 1,
        INSCRIPTION_ACTIVATE, true, USE_TIMEOUT, MSG_ACT_ARTIFACT, item_tester_hook_activate}
};


/*
 * Use an item in the pack or on the floor. Returns true if the item has been completely
 * used up, false otherwise.
 */
static bool do_cmd_use_end(struct player *p, struct object *obj, bool ident, bool used, int use)
{
    bool none_left = false;

    /* Paranoia: requires an item */
    if (!obj) return false;

    /* Only take a turn if used */
    if (used) use_energy(p);

    /* ID the object by use if appropriate, otherwise, mark it as "tried" */
    if (ident && !p->was_aware)
        object_learn_on_use(p, obj);
    else if (used)
        object_flavor_tried(p, obj);

    /* Some uses are "free" */
    if (used)
    {
        /* Chargeables act differently to single-used items when not used up */
        if (use == USE_CHARGE)
        {
            /* Use a single charge */
            obj->pval--;

            /* Describe charges */
            if (object_is_carried(p, obj))
                inven_item_charges(p, obj);

            /* Redraw */
            else
                redraw_floor(&p->wpos, &obj->grid);
        }
        else if (use == USE_TIMEOUT)
        {
            /* Rods: drain the charge */
            if (tval_can_have_timeout(obj))
            {
                obj->timeout += randcalc(obj->time, 0, RANDOMISE);

                /* Redraw */
                if (!object_is_carried(p, obj))
                    redraw_floor(&p->wpos, &obj->grid);
            }

            /* Other activatable items */
            else
                obj->timeout = randcalc(obj->time, 0, RANDOMISE);
        }
        else if (use == USE_SINGLE)
        {
            /* Log ownership change (in case we use item from the floor) */
            object_audit(p, obj);

            /* Destroy an item */
            none_left = use_object(p, obj, 1, true);
        }
    }

    /* Mark as tried and redisplay */
    p->upkeep->notice |= (PN_COMBINE);
    p->upkeep->redraw |= (PR_INVEN | PR_EQUIP);

    /* Hack -- delay pack updates when an item request is pending */
    if (p->current_value == ITEM_PENDING) p->upkeep->notice |= PN_WAIT;
    else p->upkeep->notice &= ~(PN_WAIT);

    return none_left;
}


bool execute_effect(struct player *p, struct object **obj_address, struct effect *effect, int dir,
    bool *ident, bool *used, bool *notice)
{
    struct beam_info beam;
    int boost, level;
    byte tval;
    quark_t note;
    bool no_ident = false;
    struct effect *e = effect;
    struct source who_body;
    struct source *who = &who_body;

    /* Get the level */
    if ((*obj_address)->artifact)
        level = get_artifact_level(*obj_address);
    else
        level = (*obj_address)->kind->level;

    /* Check for "obvious" effects beforehand */
    if (effect->index == EF_IDENTIFY)
    {
        object_flavor_aware(p, *obj_address);

        /* PWMAngband: mark "aware" and redisplay */
        p->upkeep->redraw |= PR_INVEN;
        redraw_stuff(p);
    }

    /* Boost damage effects if skill > difficulty */
    boost = MAX(p->state.skills[SKILL_DEVICE] - level, 0);

    /* Various hacks */
    tval = (*obj_address)->tval;
    note = (*obj_address)->note;
    while (e && !no_ident)
    {
        switch (e->index)
        {
            /* Teleport */
            case EF_TELEPORT:
            {
                /* Use up one charge before teleporting the player */
                if (tval_is_staff(*obj_address))
                    do_cmd_use_staff_discharge(p, *obj_address, true, true);

                /* Fall through */
            }

            /* Glyph + teleport level */
            case EF_GLYPH:
            case EF_TELEPORT_LEVEL:
            {
                /* Use up the scroll first */
                if (tval_is_scroll(*obj_address) &&
                    do_cmd_read_scroll_end(p, *obj_address, true, true))
                {
                    *obj_address = NULL;
                }

                /* Already used up, don't call do_cmd_use_end again */
                no_ident = true;
                break;
            }

            /* Experience gain */
            case EF_GAIN_EXP:
            {
                /* Limit the effect of the Potion of Experience */
                if (((*obj_address)->owner && (p->id != (*obj_address)->owner)) ||
                    ((*obj_address)->origin == ORIGIN_STORE) || ((*obj_address)->askprice == 1))
                {
                    e->subtype = 1;
                }

                break;
            }

            /* Dragon breath */
            case EF_BREATH:
            {
                /* Use up the potion first, since breathing frost could destroy it */
                if (tval_is_potion(*obj_address) &&
                    do_cmd_use_end(p, *obj_address, true, true, USE_SINGLE))
                {
                    *obj_address = NULL;
                }

                /* Already used up, don't call do_cmd_use_end again */
                no_ident = true;
                break;
            }

            /* Polymorphing */
            case EF_POLY_RACE:
            {
                /* Monster race */
                boost = (*obj_address)->modifiers[OBJ_MOD_POLY_RACE];

                break;
            }
        }

        e = e->next;
    }

    fill_beam_info(NULL, tval, &beam);

    /* Do effect */
    if (effect->other_msg) msg_misc(p, effect->other_msg);
    source_player(who, get_player_index(get_connection(p->conn)), p);
    *used = effect_do(effect, who, ident, p->was_aware, dir, &beam, boost, note, NULL);

    /* Notice */
    if (*ident) *notice = true;
    if (no_ident) *ident = false;

    /* Quit if the item wasn't used and no knowledge was gained */
    return (!*used && (p->was_aware || !*ident));
}


/*
 * Use an object the right way.
 */
static void use_aux(struct player *p, int item, int dir, cmd_param *p_cmd)
{
    struct object *obj = object_from_index(p, item, true, true);
    struct effect *effect;
    bool ident = false, used = false, can_use = true;
    bool notice = false;
    int aim;

    /* Paranoia: requires an item */
    if (!obj) return;

    /* Clear current */
    current_clear(p);

    /* Set current item */
    p->current_item = item;

    /* Restrict ghosts */
    /* Sometimes players in undead form can use items (from pack only) */
    if (p->ghost && !(p->dm_flags & p_cmd->dm_flag) &&
        !(p_cmd->player_undead && player_undead(p) && object_is_carried(p, obj)))
    {
        msg(p, p_cmd->msg_ghost);
        return;
    }

    /* Check preventive inscription */
    if ((p_cmd->p_note >= 0) && check_prevent_inscription(p, p_cmd->p_note))
    {
        msg(p, "The item's inscription prevents it.");
        return;
    }

    /* Hack -- restrict to equipped items */
    if (p_cmd->eq_only && !object_is_equipped(p->body, obj)) return;

    /* Some checks */
    if (!object_is_carried(p, obj))
    {
        /* Restricted by choice */
        if (!is_owner(p, obj))
        {
            msg(p, "This item belongs to someone else!");
            return;
        }

        /* Must meet level requirement */
        if (!has_level_req(p, obj))
        {
            msg(p, "You don't have the required level!");
            return;
        }

        /* Check preventive inscription '!g' */
        if (object_prevent_inscription(p, obj, INSCRIPTION_PICKUP, false))
        {
            msg(p, "The item's inscription prevents it.");
            return;
        }
    }

    /* Paranoia: requires a proper object */
    if (!p_cmd->item_tester_hook(p, obj)) return;

    /* Check preventive inscription */
    if (object_prevent_inscription(p, obj, p_cmd->g_note, false))
    {
        msg(p, "The item's inscription prevents it.");
        return;
    }

    /* Antimagic field (except horns which are not magical) */
    if (p_cmd->check_antimagic && !tval_is_horn(obj) &&
        check_antimagic(p, chunk_get(&p->wpos), NULL))
    {
        use_energy(p);
        return;
    }

    /* The player is aware of the object's flavour */
    p->was_aware = object_flavor_is_aware(p, obj);

    /* Track the object used */
    track_object(p->upkeep, obj);

    /* Figure out effect to use */
    effect = object_effect(obj);

    /* Verify effect */
    my_assert(effect);

    /* Check for unknown objects to prevent wasted player turns. */
    if ((effect->index == EF_IDENTIFY) && !spell_identify_unknown_available(p))
    {
        msg(p, "You have nothing to identify.");
        return;
    }

    aim = obj_needs_aim(p, obj);
    if (aim != AIM_NONE)
    {
        /* Determine whether we know an item needs to be aimed */
        bool known_aim = (aim == AIM_NORMAL);

        /* Unknown things with no obvious aim get a random direction */
        if (!known_aim) dir = ddd[randint0(8)];

        /* Confusion wrecks aim */
        player_confuse_dir(p, &dir);
    }

    /* Check for use if necessary */
    if ((p_cmd->use == USE_CHARGE) || (p_cmd->use == USE_TIMEOUT))
        can_use = check_devices(p, obj);

    /* Execute the effect */
    if (can_use)
    {
        /* Sound and/or message */
        sound(p, p_cmd->snd);
        activation_message(p, obj);

        if (execute_effect(p, &obj, effect, dir, &ident, &used, &notice)) return;
    }

    /* Take a turn if device failed */
    else
        use_energy(p);

    /* If the item is a null pointer or has been wiped, be done now */
    if (!obj) return;

    if (notice) object_notice_effect(p, obj);

    /* Use the object, check if none left */
    if (do_cmd_use_end(p, obj, ident, used, p_cmd->use)) return;

    /* Hack -- rings of polymorphing get destroyed when activated */
    if (tval_is_ring(obj) && (obj->sval == lookup_sval(obj->tval, "Polymorphing")) && used)
    {
        msg(p, "Your ring explodes in a bright flash of light!");
        use_object(p, obj, 1, true);
    }
}


/* Use a staff */
void do_cmd_use_staff(struct player *p, int item)
{
    use_aux(p, item, 0, &cmd_params[CMD_USE]);
}


/* Aim a wand */
void do_cmd_aim_wand(struct player *p, int item, int dir)
{
    /* Use the object */
    use_aux(p, item, dir, &cmd_params[CMD_AIM]);
}


/* Zap a rod */
void do_cmd_zap_rod(struct player *p, int item, int dir)
{
    /* Use the object */
    use_aux(p, item, dir, &cmd_params[CMD_ZAP]);
}


/* Activate a wielded object */
void do_cmd_activate(struct player *p, int item, int dir)
{
    /* Use the object */
    use_aux(p, item, dir, &cmd_params[CMD_ACTIVATE]);
}


/* Eat some food */
void do_cmd_eat_food(struct player *p, int item)
{
    /* Use the object */
    use_aux(p, item, 0, &cmd_params[CMD_EAT]);
}


/* Quaff a potion (from the pack or the floor) */
void do_cmd_quaff_potion(struct player *p, int item, int dir)
{
    /* Use the object */
    use_aux(p, item, dir, &cmd_params[CMD_QUAFF]);
}


/* Determine if the player can read scrolls. */
static bool can_read_scroll(struct player *p)
{
    if (p->timed[TMD_BLIND])
    {
        msg(p, "You can't see anything.");
        return false;
    }

    if (no_light(p))
    {
        msg(p, "You have no light to read by.");
        return false;
    }

    if (p->timed[TMD_CONFUSED])
    {
        msg(p, "You are too confused to read!");
        return false;
    }

    if (one_in_(2) && p->timed[TMD_AMNESIA])
    {
        msg(p, "You can't remember how to read!");
        return false;
    }

    return true;
}


/* Read a scroll (from the pack or floor) */
void do_cmd_read_scroll(struct player *p, int item)
{
    /* Check some conditions */
    if (!can_read_scroll(p)) return;

    /* Use the object */
    use_aux(p, item, 0, &cmd_params[CMD_READ]);
}


/* Use an item */
void do_cmd_use_any(struct player *p, int item, int dir)
{
    struct object *obj = object_from_index(p, item, true, true);

    /* Paranoia: requires an item */
    if (!obj) return;

    /* Fire a missile */
    if (obj->tval == p->state.ammo_tval) do_cmd_fire(p, dir, item);

    /* Eat some food */
    else if (item_tester_hook_eat(p, obj)) do_cmd_eat_food(p, item);

    /* Quaff a potion */
    else if (item_tester_hook_quaff(p, obj)) do_cmd_quaff_potion(p, item, dir);

    /* Read a scroll */
    else if (item_tester_hook_read(p, obj)) do_cmd_read_scroll(p, item);

    /* Use a staff */
    else if (item_tester_hook_use(p, obj)) do_cmd_use_staff(p, item);

    /* Aim a wand */
    else if (item_tester_hook_aim(p, obj)) do_cmd_aim_wand(p, item, dir);

    /* Zap a rod */
    else if (item_tester_hook_zap(p, obj)) do_cmd_zap_rod(p, item, dir);

    /* Activate a wielded object */
    else if (object_is_equipped(p->body, obj) && item_tester_hook_activate(p, obj))
        do_cmd_activate(p, item, dir);

    /* Oops */
    else msg(p, "You cannot use that!");
}


/*
 * Refuelling
 */


/*
 * Hook for refilling lamps
 */
static bool item_tester_refill_lamp(struct object *obj)
{
    /* Flasks of oil are okay */
    if (tval_is_fuel(obj)) return true;

    /* Non-empty, non-everburning lamps are okay */
    if (tval_is_light(obj) && of_has(obj->flags, OF_TAKES_FUEL) && (obj->timeout > 0) &&
        !of_has(obj->flags, OF_NO_FUEL))
    {
        return true;
    }

    /* Assume not okay */
    return false;
}


/*
 * Refill the players lamp (from the pack or floor)
 */
static void refill_lamp(struct player *p, struct object *lamp, struct object *obj)
{
    /* Refuel */
    lamp->timeout += (obj->timeout? obj->timeout: obj->pval);

    /* Message */
    msg(p, "You fuel your lamp.");

    /* Comment */
    if (lamp->timeout >= z_info->fuel_lamp)
    {
        lamp->timeout = z_info->fuel_lamp;
        msg(p, "Your lamp is full.");
    }

    /* Refilled from a lamp */
    if (of_has(obj->flags, OF_TAKES_FUEL))
    {
        /* Unstack if necessary */
        if (obj->number > 1)
        {
            /* Obtain a local object, split */
            struct object *used = object_split(obj, 1);
            struct chunk *c = chunk_get(&p->wpos);

            /* Remove fuel */
            used->timeout = 0;

            /* Carry or drop */
            if (object_is_carried(p, obj))
                inven_carry(p, used, true, true);
            else
                drop_near(p, c, &used, 0, &p->grid, false, DROP_FADE);
        }

        /* Empty a single lamp */
        else
            obj->timeout = 0;

        /* Combine the pack (later) */
        p->upkeep->notice |= (PN_COMBINE);

        /* Redraw */
        p->upkeep->redraw |= (PR_INVEN);
    }

    /* Refilled from a flask */
    else
    {
        /* Decrease the item */
        use_object(p, obj, 1, true);
    }

    /* Recalculate torch */
    p->upkeep->update |= (PU_BONUS);

    /* Redraw */
    p->upkeep->redraw |= (PR_EQUIP);
}


/*
 * Refill the players light source
 */
void do_cmd_refill(struct player *p, int item)
{
    struct object *light = equipped_item_by_slot_name(p, "light");
    struct object *obj = object_from_index(p, item, true, true);

    /* Paranoia: requires an item */
    if (!obj) return;

    /* Restrict ghosts */
    if (p->ghost && !(p->dm_flags & DM_GHOST_BODY))
    {
        msg(p, "You need a tangible body to refill light sources!");
        return;
    }

    /* Check preventive inscription '!F' */
    if (object_prevent_inscription(p, obj, INSCRIPTION_REFILL, false))
    {
        msg(p, "The item's inscription prevents it.");
        return;
    }

    /* Some checks */
    if (!object_is_carried(p, obj))
    {
        /* Restricted by choice */
        if (!is_owner(p, obj))
        {
            msg(p, "This item belongs to someone else!");
            return;
        }

        /* Must meet level requirement */
        if (!has_level_req(p, obj))
        {
            msg(p, "You don't have the required level!");
            return;
        }

        /* Check preventive inscription '!g' */
        if (object_prevent_inscription(p, obj, INSCRIPTION_PICKUP, false))
        {
            msg(p, "The item's inscription prevents it.");
            return;
        }
    }

    /* Paranoia: requires a refill */
    if (!item_tester_refill_lamp(obj)) return;

    /* Check what we're wielding. */
    if (!light || !tval_is_light(light))
    {
        msg(p, "You are not wielding a light.");
        return;
    }
    if (of_has(light->flags, OF_NO_FUEL) || !of_has(light->flags, OF_TAKES_FUEL))
    {
        msg(p, "Your light cannot be refilled.");
        return;
    }

    /* Take a turn */
    use_energy(p);

    refill_lamp(p, light, obj);
}


/*
 * Use a scroll, check if none left
 */
bool do_cmd_read_scroll_end(struct player *p, struct object *obj, bool ident, bool used)
{
    return do_cmd_use_end(p, obj, ident, used, USE_SINGLE);
}


void do_cmd_use_staff_discharge(struct player *p, struct object *obj, bool ident, bool used)
{
    do_cmd_use_end(p, obj, ident, used, USE_CHARGE);
}


void do_cmd_zap_rod_end(struct player *p, struct object *obj, bool ident, bool used)
{
    do_cmd_use_end(p, obj, ident, used, USE_TIMEOUT);
}


void do_cmd_activate_end(struct player *p, struct object *obj, bool ident, bool used)
{
    do_cmd_use_end(p, obj, ident, used, USE_TIMEOUT);
}


