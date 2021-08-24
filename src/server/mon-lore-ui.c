/*
 * File: mon-lore-ui.c
 * Purpose: Monster memory UI
 *
 * Copyright (c) 1997-2007 Ben Harrison, James E. Wilson, Robert A. Koeneke
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
 * Place a monster recall title into a textblock.
 *
 * If graphics are turned on, this appends the title with the appropriate tile.
 * Note: if the title is the only thing in the textblock, make sure to append a newline
 * so that the textui stuff works properly.
 *
 * race is the monster race we are describing.
 */
static void lore_title(struct player *p, const struct monster_race *race)
{
    byte standard_attr, optional_attr;
    char standard_char, optional_char;

    my_assert(race);

    /* Get the chars */
    standard_char = race->d_char;
    optional_char = monster_x_char[race->ridx];

    /* Get the attrs */
    standard_attr = race->d_attr;
    optional_attr = monster_x_attr[race->ridx];

    /* A title (use "The" for non-uniques) */
    if (!monster_is_unique(race)) text_out(p, "The ");
    else if (OPT(p, purple_uniques))
    {
        standard_attr = COLOUR_VIOLET;
        if (!(optional_attr & 0x80)) optional_attr = COLOUR_VIOLET;
    }

    /* Dump the name */
    text_out(p, race->name);

    /* Append the "standard" attr/char info */
    text_out(p, " ('");
    text_out_c(p, standard_attr, "%c", standard_char);
    text_out(p, "')");

    /* Append the "optional" attr/char info */
    if ((optional_attr != standard_attr) || (optional_char != standard_char))
    {
        text_out(p, "/('");
        text_out_c(p, optional_attr, "%c", optional_char);
        text_out(p, "'):");
    }

    text_out(p, "\n\n");
}


/*
 * Place a full monster recall description (with title) into a textblock.
 *
 * race is the monster race we are describing.
 */
void lore_description(struct player *p, const struct monster_race *race)
{
    struct monster_lore mutable_lore;
    struct monster_lore *lore = &mutable_lore;
    bitflag known_flags[RF_SIZE];

    my_assert(race);

    /* Get the lores (player + global) */
    get_global_lore(p, race, lore);

    /* Now get the known monster flags */
    monster_flags_known(race, lore, known_flags);

    /* Monster name */
    lore_title(p, race);

    /* Show kills of monster vs. player(s) */
    lore_append_kills(p, race, lore, known_flags);

    /* Monster description */
    lore_append_flavor(p, race);

    /* Describe the monster type, speed, life, and armor */
    lore_append_movement(p, race, lore, known_flags);
    lore_append_toughness(p, race, lore, known_flags);

    /* Describe the experience and item reward when killed */
    lore_append_exp(p, race, lore, known_flags);
    lore_append_drop(p, race, lore, known_flags);

    /* Describe the special properties of the monster */
    lore_append_abilities(p, race, lore, known_flags);
    lore_append_awareness(p, race, lore, known_flags);
    lore_append_friends(p, race, lore, known_flags);

    /* Describe the spells, spell-like abilities and melee attacks */
    lore_append_spells(p, race, lore, known_flags);
    lore_append_attack(p, race, lore, known_flags);

    /* Do we know everything */
    if (lore_is_fully_known(p, race))
        text_out(p, "You know everything about this monster.");

    /* Notice "Quest" monsters */
    if (rf_has(race->flags, RF_QUESTOR))
        text_out(p, "You feel an intense desire to kill this monster...");

    mem_free(lore->blows);
    mem_free(lore->blow_known);
}


/*
 * Display monster information, using text_out()
 */
void describe_monster(struct player *p, const struct monster_race *race)
{
    /* Let the player scroll through this info */
    p->special_file_type = SPECIAL_FILE_OTHER;

    /* Prepare player structure for text */
    text_out_init(p);

    /* Dump info into player */
    lore_description(p, race);

    /* Restore height and width of current dungeon level */
    text_out_done(p);
}