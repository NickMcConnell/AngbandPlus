/*
 * File: mon-lore.c
 * Purpose: Monster memory code.
 *
 * Copyright (c) 1997-2007 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * Copyright (c) 2020 MAngband and PWMAngband Developers
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
 * Monster genders
 */
enum monster_sex
{
    MON_SEX_NEUTER = 0,
    MON_SEX_MALE,
    MON_SEX_FEMALE,
    MON_SEX_MAX
};
typedef enum monster_sex monster_sex_t;


/*
 * Determine the color to code a monster spell
 *
 * This function assigns a color to each monster spell, depending on how
 * dangerous the attack is to the player given current state. Spells may be
 * colored green (least dangerous), yellow, orange, or red (most dangerous).
 */
static int spell_color(struct player *p, const struct monster_race *race, int spell_index)
{
    const struct monster_spell *spell = monster_spell_by_index(spell_index);
    struct monster_spell_level *level = spell->level;

    /* No spell */
    if (!spell) return COLOUR_DARK;

    /* Get the right level */
    while (level->next && race->spell_power >= level->next->power) level = level->next;

    /* Unresistable spells just use the default color */
    if (!level->lore_attr_resist && !level->lore_attr_immune) return level->lore_attr;

    /* Spells with a save */
    if (level->save_message)
    {
        /* Mixed results if the save may fail, perfect result if it can't */
        if (p->known_state.skills[SKILL_SAVE] < 100)
        {
            /* Special case - teleport level */
            if (spell->effect->index == EF_TELEPORT_LEVEL)
            {
                if (p->known_state.el_info[ELEM_NEXUS].res_level > 0)
                    return level->lore_attr_resist;
                return level->lore_attr;
            }

            /* Timed effects with or without damage */
            if (level->lore_attr_immune)
            {
                struct effect *eff;

                for (eff = spell->effect; eff; eff = eff->next)
                {
                    if (eff->index != EF_TIMED_INC) continue;
                    if (player_inc_check(p, NULL, eff->subtype, true)) return level->lore_attr;
                }
                return level->lore_attr_resist;
            }

            /* Straight damage */
            return level->lore_attr;
        }
        if (level->lore_attr_immune) return level->lore_attr_immune;
        return level->lore_attr_resist;
    }

    /* Bolts, balls and breaths */
    if ((spell->effect->index == EF_BOLT) || (spell->effect->index == EF_BALL) ||
        (spell->effect->index == EF_BREATH))
    {
        /* Treat by element */
        switch (spell->effect->subtype)
        {
            /* Special case - sound */
            case ELEM_SOUND:
            {
                if (p->known_state.el_info[ELEM_SOUND].res_level > 0)
                    return level->lore_attr_immune;
                if (of_has(p->known_state.flags, OF_PROT_STUN))
                    return level->lore_attr_resist;
                return level->lore_attr;
            }

            /* Special case - nexus */
            case ELEM_NEXUS:
            {
                if (p->known_state.el_info[ELEM_NEXUS].res_level > 0)
                    return level->lore_attr_immune;
                if (p->known_state.skills[SKILL_SAVE] >= 100)
                    return level->lore_attr_resist;
                return level->lore_attr;
            }

            /* Elements that stun or confuse */
            case ELEM_GRAVITY:
            case ELEM_PLASMA:
            case ELEM_FORCE:
            case ELEM_WATER:
            {
                if (!of_has(p->known_state.flags, OF_PROT_STUN))
                    return level->lore_attr;
                if (!of_has(p->known_state.flags, OF_PROT_CONF) &&
                    (spell->effect->subtype == ELEM_WATER))
                {
                    return level->lore_attr;
                }
                return level->lore_attr_resist;
            }

            /* Special case - ice (PWMAngband: cold + stun) */
            case ELEM_ICE:
            {
                if (!of_has(p->known_state.flags, OF_PROT_STUN))
                {
                    if (p->known_state.el_info[ELEM_COLD].res_level > 0)
                        return level->lore_attr_resist;
                    return level->lore_attr;
                }
                if (p->known_state.el_info[ELEM_COLD].res_level > 0)
                    return level->lore_attr_immune;
                return level->lore_attr_resist;
            }

            /* All other elements */
            default:
            {
                if (p->known_state.el_info[spell->effect->subtype].res_level == 3)
                    return level->lore_attr_immune;
                if (p->known_state.el_info[spell->effect->subtype].res_level > 0)
                    return level->lore_attr_resist;
                return level->lore_attr;
            }
        }
    }

    return level->lore_attr;
}


/*
 * Determine the color to code a monster melee blow effect
 *
 * This function assigns a color to each monster blow effect, depending on how
 * dangerous the attack is to the player given current state. Blows may be
 * colored green (least dangerous), yellow, orange, or red (most dangerous).
 */
static int blow_color(struct player *p, int blow_idx)
{
    const struct blow_effect *blow = &blow_effects[blow_idx];

    /* Some blows just use the default color */
    if (!blow->lore_attr_resist && !blow->lore_attr_immune) return blow->lore_attr;

    /* Effects with immunities are straightforward */
    if (blow->lore_attr_immune)
    {
        int i;

        for (i = ELEM_ACID; i < ELEM_POIS; i++)
        {
            if (proj_name_to_idx(blow->name) == i) break;
        }

        if (p->known_state.el_info[i].res_level == 3) return blow->lore_attr_immune;
        if (p->known_state.el_info[i].res_level > 0) return blow->lore_attr_resist;
        return blow->lore_attr;
    }

    /* Now look at what player attributes can protect from the effects */
    if (streq(blow->effect_type, "theft"))
    {
        if (p->lev + adj_dex_safe[p->known_state.stat_ind[STAT_DEX]] >= 100)
            return blow->lore_attr_resist;
        return blow->lore_attr;
    }
    if (streq(blow->effect_type, "drain"))
    {
        int i;
        bool found = false;

        for (i = 0; i < z_info->pack_size; i++)
        {
            struct object *obj = p->upkeep->inven[i];

            if (!obj) continue;
            if ((!object_is_known(p, obj) || (obj->pval > 0)) && tval_can_have_charges(obj))
            {
                found = true;
                break;
            }
        }
        if (found) return blow->lore_attr;
        return blow->lore_attr_resist;
    }
    if (streq(blow->effect_type, "eat-food"))
    {
        int i;
        bool found = false;

        for (i = 0; i < z_info->pack_size; i++)
        {
            struct object *obj = p->upkeep->inven[i];

            if (!obj) continue;
            if (tval_is_edible(obj))
            {
                found = true;
                break;
            }
        }
        if (found) return blow->lore_attr;
        return blow->lore_attr_resist;
    }
    if (streq(blow->effect_type, "eat-light"))
    {
        int light_slot = slot_by_name(p, "light");
        struct object *obj = slot_object(p, light_slot);

        if (obj)
        {
            bitflag f[OF_SIZE];

            object_flags_known(obj, f, object_flavor_is_aware(p, obj));
            if (!object_is_known(p, obj) || (!of_has(f, OF_NO_FUEL) && (obj->timeout > 0)))
                return blow->lore_attr;
        }
        return blow->lore_attr_resist;
    }
    if (streq(blow->effect_type, "element"))
    {
        if (p->known_state.el_info[blow->resist].res_level > 0)
            return blow->lore_attr_resist;
        return blow->lore_attr;
    }
    if (streq(blow->effect_type, "flag"))
    {
        if (of_has(p->known_state.flags, blow->resist))
            return blow->lore_attr_resist;
        return blow->lore_attr;
    }
    if (streq(blow->effect_type, "all_sustains"))
    {
        if (of_has(p->known_state.flags, OF_SUST_STR) && of_has(p->known_state.flags, OF_SUST_INT) &&
            of_has(p->known_state.flags, OF_SUST_WIS) && of_has(p->known_state.flags, OF_SUST_DEX) &&
            of_has(p->known_state.flags, OF_SUST_CON))
        {
            return blow->lore_attr_resist;
        }
        return blow->lore_attr;
    }
    if (streq(blow->effect_type, "disease"))
    {
        if ((p->known_state.el_info[ELEM_POIS].res_level > 0) &&
            of_has(p->known_state.flags, OF_SUST_CON))
        {
            return blow->lore_attr_resist;
        }
        return blow->lore_attr;
    }

    return blow->lore_attr;
}


void lore_learn_spell_if_has(struct monster_lore *lore, const struct monster_race *race, int flag)
{
    if (rsf_has(race->spell_flags, flag)) rsf_on(lore->spell_flags, flag);
}


void lore_learn_spell_if_visible(struct monster_lore *lore, bool visible, int flag)
{
    if (visible) rsf_on(lore->spell_flags, flag);
}


void lore_learn_flag_if_visible(struct monster_lore *lore, bool visible, int flag)
{
    if (visible) rf_on(lore->flags, flag);
}


/*
 * Update which bits of lore are known
 */
void lore_update(const struct monster_race *race, struct monster_lore *lore)
{
    int i;
    bitflag mask[RF_SIZE];

    if (!race || !lore) return;

    /* Assume some "obvious" flags */
    create_mon_flag_mask(mask, RFT_OBV, RFT_MAX);
    mflag_union(lore->flags, mask);

    /* Blows */
    for (i = 0; i < z_info->mon_blows_max; i++)
    {
        if (!race->blow) break;
        if (lore->blows[i] || lore->all_known)
            lore->blow_known[i] = true;
    }

    /* Killing a monster reveals some properties */
    if (lore->pkills || lore->all_known)
    {
        lore->armour_known = true;
        lore->drop_known = true;
        create_mon_flag_mask(mask, RFT_RACE_A, RFT_RACE_N, RFT_DROP, RFT_MAX);
        mflag_union(lore->flags, mask);
        rf_on(lore->flags, RF_FORCE_DEPTH);
    }

    /* Awareness */
    if ((((int)lore->wake * (int)lore->wake) > race->sleep) || (lore->ignore == UCHAR_MAX) ||
        lore->all_known || ((race->sleep == 0) && (lore->pkills >= 10)))
    {
        lore->sleep_known = true;
    }

    /* Spellcasting frequency */
    if ((lore->cast_innate + lore->cast_spell > 100) || lore->all_known)
        lore->spell_freq_known = true;

    /* Flags for probing and cheating */
    if (lore->all_known)
    {
        rf_setall(lore->flags);
        rsf_copy(lore->spell_flags, race->spell_flags);
    }
}


/*
 * Learn everything about a monster.
 *
 * Sets the all_known variable, all flags and all relevant spell flags.
 */
static void cheat_monster_lore(const struct monster_race *race, struct monster_lore *lore)
{
    my_assert(race);
    my_assert(lore);

    /* Full knowledge */
    lore->all_known = true;
    lore_update(race, lore);
}


/*
 * Learn about a monster (by "probing" it)
 */
void lore_do_probe(struct player *p, struct monster *mon)
{
    struct monster_lore *lore = get_lore(p, mon->race);
    struct actor_race *monster_race = &p->upkeep->monster_race;

    /* Full knowledge */
    lore->all_known = true;
    lore_update(mon->race, lore);

    /* Update monster recall window */
    if (ACTOR_RACE_EQUAL(monster_race, mon)) p->upkeep->redraw |= (PR_MONSTER);
}


/*
 * Determine whether the monster is fully known
 */
bool lore_is_fully_known(struct player *p, const struct monster_race *race)
{
    unsigned i;
    struct monster_lore *lore = get_lore(p, race);

    /* Check if already known */
    if (lore->all_known) return true;

    if (!lore->armour_known) return false;
    if (!lore->drop_known) return false;
    if (!lore->sleep_known) return false;

    /* Only check spells if the monster can cast them */
    if (!lore->spell_freq_known && race->freq_spell) return false;

    /* Check if blows are known */
    for (i = 0; i < (unsigned)z_info->mon_blows_max; i++)
    {
        /* Only check if the blow exists */
        if (!race->blow[i].method) break;

        if (!lore->blow_known[i]) return false;
    }

    /* Check all the flags */
    for (i = 0; i < RF_SIZE; i++)
    {
        if (!lore->flags[i]) return false;
    }

    /* Check spell flags */
    for (i = 0; i < RSF_SIZE; i++)
    {
        if (lore->spell_flags[i] != race->spell_flags[i]) return false;
    }

    /* The player knows everything */
    lore->all_known = true;
    lore_update(race, lore);
    return true;
}


/*
 * Take note that the given monster just dropped some treasure
 *
 * Note that learning the "GOOD"/"GREAT" flags gives information
 * about the treasure (even when the monster is killed for the first
 * time, such as uniques, and the treasure has not been examined yet).
 *
 * This "indirect" method is used to prevent the player from learning
 * exactly how much treasure a monster can drop from observing only
 * a single example of a drop.  This method actually observes how much
 * gold and items are dropped, and remembers that information to be
 * described later by the monster recall code.
 */
void lore_treasure(struct player *p, struct monster *mon, int num_item, int num_gold)
{
    struct actor_race *monster_race = &p->upkeep->monster_race;

    my_assert(num_item >= 0);
    my_assert(num_gold >= 0);

    /* Note the number of things dropped */
    /* --- obsolete --- */

    /* Update monster recall window */
    if (ACTOR_RACE_EQUAL(monster_race, mon)) p->upkeep->redraw |= (PR_MONSTER);
}


/*
 * Copies into `flags` the flags of the given monster race that are known
 * to the given lore structure (usually the player's knowledge).
 *
 * Known flags will be 1 for present, or 0 for not present. Unknown flags
 * will always be 0.
 */
void monster_flags_known(const struct monster_race *race, const struct monster_lore *lore,
    bitflag flags[RF_SIZE])
{
    rf_copy(flags, race->flags);
    rf_inter(flags, lore->flags);
}


/*
 * Return a description for the given monster race awareness value.
 *
 * Descriptions are in a table within the function.
 * Returns a sensible string for values not in the table.
 *
 * awareness is the inactivity counter of the race (monster_race.sleep).
 */
static const char *lore_describe_awareness(s16b awareness)
{
    /* Value table ordered descending, for priority. Terminator is {SHRT_MAX, NULL}. */
    static const struct lore_awareness
    {
        s16b threshold;
        const char *description;
    } lore_awareness_description[] =
    {
        {200,       "prefers to ignore"},
        {95,        "pays very little attention to"},
        {75,        "pays little attention to"},
        {45,        "tends to overlook"},
        {25,        "takes quite a while to see"},
        {5,         "is fairly observant of"},
        {3,         "is observant of"},
        {1,         "is very observant of"},
        {0,         "is vigilant for"},
        {SHRT_MAX, NULL}
    };
    const struct lore_awareness *current = lore_awareness_description;

    while ((current->threshold != SHRT_MAX) && (current->description != NULL))
    {
        if (awareness > current->threshold) return current->description;

        current++;
    }

    /* Values zero and less are the most vigilant */
    return "is ever vigilant for";
}


/*
 * Return a description for the given monster race speed value.
 *
 * Descriptions are in a table within the function.
 * Returns a sensible string for values not in the table.
 *
 * speed is the speed rating of the race (monster_race.speed).
 */
static const char *lore_describe_speed(byte speed)
{
    /* Value table ordered descending, for priority. Terminator is {UCHAR_MAX, NULL}. */
    static const struct lore_speed
    {
        byte threshold;
        const char *description;
    } lore_speed_description[] =
    {
        {130,       "incredibly quickly"},
        {120,       "very quickly"},
        {115,       "quickly"},
        {110,       "fairly quickly"},
        {109,       "normal speed"},
        {99,        "slowly"},
        {89,        "very slowly"},
        {0,         "incredibly slowly"},
        {UCHAR_MAX, NULL}
    };
    const struct lore_speed *current = lore_speed_description;

    while ((current->threshold != UCHAR_MAX) && (current->description != NULL))
    {
        if (speed > current->threshold) return current->description;

        current++;
    }

    /* Return a weird description, since the value wasn't found in the table */
    return "erroneously";
}


/*
 * Append the monster speed, in words, to a textblock.
 *
 * race is the monster race we are describing.
 */
static void lore_adjective_speed(struct player *p, const struct monster_race *race)
{
    /*
     * "at" is separate from the normal speed description in order to use the
     * normal text colour
     */
    if (race->speed == 110) text_out(p, "at ");

    text_out_c(p, COLOUR_GREEN, lore_describe_speed(race->speed));
}


/*
 * Append the monster speed, in multipliers, to a textblock.
 *
 * race is the monster race we are describing.
 */
static void lore_multiplier_speed(struct player *p, const struct monster_race *race)
{
    char buf[13] = "";
    int multiplier = 10 * frame_energy(race->speed) / frame_energy(110);
    byte int_mul = multiplier / 10;
    byte dec_mul = multiplier % 10;
    byte attr = COLOUR_ORANGE;

    text_out(p, "at ");

    strnfmt(buf, sizeof(buf), "%d.%dx", int_mul, dec_mul);
    text_out_c(p, COLOUR_L_BLUE, buf);

    text_out(p, " normal speed, which is ");
    multiplier = 100 * frame_energy(race->speed) / frame_energy(p->state.speed);
    int_mul = multiplier / 100;
    dec_mul = multiplier % 100;
    if (!dec_mul)
        strnfmt(buf, sizeof(buf), "%dx", int_mul);
    else if (!(dec_mul % 10))
        strnfmt(buf, sizeof(buf), "%d.%dx", int_mul, dec_mul / 10);
    else
        strnfmt(buf, sizeof(buf), "%d.%02dx", int_mul, dec_mul);

    if (p->state.speed > race->speed)
        attr = COLOUR_L_GREEN;
    else if (p->state.speed < race->speed)
        attr = COLOUR_RED;

    if (p->state.speed == race->speed)
        text_out(p, "the same as you");
    else
    {
        text_out_c(p, attr, buf);
        text_out(p, " your speed");
    }
}


/*
 * Return a value describing the sex of the provided monster race
 */
static monster_sex_t lore_monster_sex(const struct monster_race *race)
{
    if (rf_has(race->flags, RF_FEMALE)) return MON_SEX_FEMALE;
    if (rf_has(race->flags, RF_MALE)) return MON_SEX_MALE;
    return MON_SEX_NEUTER;
}


/*
 * Return a pronoun for a monster; used as the subject of a sentence.
 *
 * Descriptions are in a table within the function. Table must match monster_sex_t values.
 *
 * sex is the gender value (as provided by `lore_monster_sex()`.
 * title_case indicates whether the initial letter should be capitalized;
 * `true` is capitalized, `false` is not.
 */
static const char *lore_pronoun_nominative(monster_sex_t sex, bool title_case)
{
    static const char *lore_pronouns[MON_SEX_MAX][2] =
    {
        {"it", "It"},
        {"he", "He"},
        {"she", "She"}
    };

    int pronoun_index = MON_SEX_NEUTER, case_index = 0;

    if (sex < MON_SEX_MAX) pronoun_index = sex;
    if (title_case) case_index = 1;

    return lore_pronouns[pronoun_index][case_index];
}


/*
 * Return a possessive pronoun for a monster.
 *
 * Descriptions are in a table within the function. Table must match monster_sex_t values.
 *
 * sex is the gender value (as provided by `lore_monster_sex()`.
 * title_case indicates whether the initial letter should be capitalized;
 * `true` is capitalized, `false` is not.
 */
static const char *lore_pronoun_possessive(monster_sex_t sex, bool title_case)
{
    static const char *lore_pronouns[MON_SEX_MAX][2] =
    {
        {"its", "Its"},
        {"his", "His"},
        {"her", "Her"}
    };

    int pronoun_index = MON_SEX_NEUTER, case_index = 0;

    if (sex < MON_SEX_MAX) pronoun_index = sex;
    if (title_case) case_index = 1;

    return lore_pronouns[pronoun_index][case_index];
}


/*
 * Append a clause containing a list of descriptions of monster flags from
 * list-mon-race-flags.h to a textblock.
 *
 * The text that joins the list is drawn using the default attributes.
 * The list uses a serial comma ("a, b, c, and d").
 *
 * f is the set of flags to be described.
 * attr is the attribute each list item will be drawn with.
 * start is a string to start the clause.
 * conjunction is a string that is added before the last item.
 * end is a string that is added after the last item.
 */
static void lore_append_clause(struct player *p, bitflag *f, byte attr, const char *start,
    const char *conjunction, const char *end)
{
    int count = rf_count(f);
    bool comma = (count > 2);

    if (count)
    {
        int flag;

        text_out(p, start);
        for (flag = rf_next(f, FLAG_START); flag; flag = rf_next(f, flag + 1))
        {
            /* First entry starts immediately */
            if (flag != rf_next(f, FLAG_START))
            {
                if (comma) text_out(p, ",");

                /* Last entry */
                if (rf_next(f, flag + 1) == FLAG_END)
                {
                    text_out(p, " ");
                    text_out(p, conjunction);
                }
                text_out(p, " ");
            }
            text_out_c(p, attr, describe_race_flag(flag));
        }
        text_out(p, end);
    }
}


/*
 * Append a list of spell descriptions.
 *
 * This is a modified version of `lore_append_clause()` to format spells.
 *
 * f is the set of flags to be described.
 * know_hp is whether the player knows the monster's AC.
 * race is the monster race.
 * conjunction is a string that is added before the last item.
 * end is a string that is added after the last item.
 */
static void lore_append_spell_clause(struct player *p, bitflag *f, bool know_hp,
    const struct monster_race *race, const char *conjunction, const char *end)
{
    int count = rsf_count(f);
    bool comma = (count > 2);

    if (count)
    {
        int spell;

        for (spell = rsf_next(f, FLAG_START); spell; spell = rsf_next(f, spell + 1))
        {
            int color = spell_color(p, race, spell);
            int damage = mon_spell_lore_damage(spell, race, know_hp);

            /* First entry starts immediately */
            if (spell != rsf_next(f, FLAG_START))
            {
                if (comma) text_out(p, ",");

                /* Last entry */
                if (rsf_next(f, spell + 1) == FLAG_END)
                {
                    text_out(p, " ");
                    text_out(p, conjunction);
                }
                text_out(p, " ");
            }
            text_out_c(p, color, mon_spell_lore_description(spell, race));
            if (damage > 0)
                text_out_c(p, color, " (%d)", damage);
        }
        text_out(p, end);
    }
}


/*
 * Append the kill history to a texblock for a given monster race.
 *
 * Known race flags are passed in for simplicity/efficiency.
 *
 * race is the monster race we are describing.
 * lore is the known information about the monster race.
 * known_flags is the preprocessed bitfield of race flags known to the player.
 */
void lore_append_kills(struct player *p, const struct monster_race *race,
    const struct monster_lore *lore, const bitflag known_flags[RF_SIZE])
{
    monster_sex_t msex = MON_SEX_NEUTER;
    bool out = true;

    my_assert(race && lore);

    /* Extract a gender (if applicable) */
    msex = lore_monster_sex(race);

    /* Treat by whether unique, then by whether they have any player kills */
    if (rf_has(known_flags, RF_UNIQUE))
    {
        /* A killer... */
        if (lore->tdeaths)
        {
            /* Killed players */
            text_out(p, "%s has slain %d player%s", lore_pronoun_nominative(msex, true),
                lore->tdeaths, PLURAL(lore->tdeaths));

            /* Did we get killed too? */
            if (lore->pdeaths)
                text_out(p, " (including you %d time%s)", lore->pdeaths, PLURAL(lore->pdeaths));

            /* But we've also killed it */
            if (lore->pkills)
                text_out(p, ", but you have taken revenge! ");

            /* Someone else has killed it */
            else if (lore->tkills)
                text_out(p, ", but someone has taken revenge! ");

            /* Unavenged */
            else
            {
                text_out(p, ", who %s unavenged. ",
                    VERB_AGREEMENT(lore->tdeaths, "remains", "remain"));
            }
        }

        /* Dead unique (killed by the player) who never killed anyone */
        else if (lore->pkills)
            text_out(p, "You have slain this foe. ");    

        /* Dead unique (killed by someone else) who never killed anyone */
        else if (lore->tkills)
            text_out(p, "Someone has slain this foe. ");

        /* Alive and never killed anyone */
        else
            out = false;
    }
    else if (lore->tdeaths)
    {
        /* Killed players */
        text_out(p, "%d %s been killed by this creature",
            lore->tdeaths, VERB_AGREEMENT(lore->tdeaths, "player has", "players have"));

        /* Did we get killed too? */
        if (lore->pdeaths)
            text_out(p, " (including you %d time%s)", lore->pdeaths, PLURAL(lore->pdeaths));

        /* Some kills by the player */
        if (lore->pkills)
        {
            text_out(p, ", and you have exterminated at least %d of the creatures. ",
                lore->pkills);
        }

        /* Some kills by other players */
        else if (lore->tkills)
        {
            text_out(p, ", and players have exterminated at least %d of the creatures. ",
                lore->tkills);
        }

        /* No kills */
        else
        {
            text_out_c(p, COLOUR_RED, ", and %s is not ever known to have been defeated. ",
                lore_pronoun_nominative(msex, false));
        }
    }
    else
    {
        /* Some kills by the player */
        if (lore->pkills)
            text_out(p, "You have killed at least %d of these creatures. ", lore->pkills);

        /* Some kills by other players */
        else if (lore->tkills)
            text_out(p, "Players have killed at least %d of these creatures. ", lore->tkills);

        /* Killed none */
        else
            text_out(p, "No battles to the death are recalled. ");
    }

    /* Separate */
    if (out) text_out(p, "\n\n");
}


/*
 * Append the monster race description to a textblock.
 *
 * race is the monster race we are describing.
 */
void lore_append_flavor(struct player *p, const struct monster_race *race)
{
    my_assert(race);

    text_out(p, "%s\n\n", race->text);
}


/*
 * Append the monster type, location, and movement patterns to a textblock.
 *
 * Known race flags are passed in for simplicity/efficiency.
 *
 * race is the monster race we are describing.
 * lore is the known information about the monster race.
 * known_flags is the preprocessed bitfield of race flags known to the player.
 */
void lore_append_movement(struct player *p, const struct monster_race *race,
    const struct monster_lore *lore, bitflag known_flags[RF_SIZE])
{
    int f;
    bitflag flags[RF_SIZE];

    my_assert(race && lore);

    text_out(p, "This");

    /* Get adjectives */
    create_mon_flag_mask(flags, RFT_RACE_A, RFT_MAX);
    rf_inter(flags, race->flags);
    for (f = rf_next(flags, FLAG_START); f; f = rf_next(flags, f + 1))
        text_out_c(p, COLOUR_L_BLUE, " %s", describe_race_flag(f));

    /* Get noun */
    create_mon_flag_mask(flags, RFT_RACE_N, RFT_MAX);
    rf_inter(flags, race->flags);
    f = rf_next(flags, FLAG_START);
    if (f)
        text_out_c(p, COLOUR_L_BLUE, " %s", describe_race_flag(f));
    else
        text_out_c(p, COLOUR_L_BLUE, " creature");

    /* Describe location */
    if (race->level == 0)
        text_out(p, " lives in a town");
    else
    {
        byte colour = (race->level > p->max_depth)? COLOUR_RED: COLOUR_L_BLUE;

        if (rf_has(known_flags, RF_FORCE_DEPTH))
            text_out(p, " is found ");
        else
            text_out(p, " is normally found ");

        if (rf_has(known_flags, RF_WILD_ONLY))
        {
            text_out(p, "in the wilderness ");
            text_out(p, "(level ");
            text_out_c(p, colour, "%d", race->level);
            text_out(p, ")");
        }
        else
        {
            text_out(p, "at depths of ");
            text_out_c(p, colour, "%d", race->level * 50);
            text_out(p, " feet (level ");
            text_out_c(p, colour, "%d", race->level);
            text_out(p, ")");
        }
    }

    text_out(p, ", and moves");

    /* Random-ness */
    if (flags_test(known_flags, RF_SIZE, RF_RAND_50, RF_RAND_25, FLAG_END))
    {
        /* Adverb */
        if (rf_has(known_flags, RF_RAND_50) && rf_has(known_flags, RF_RAND_25))
            text_out(p, " extremely");
        else if (rf_has(known_flags, RF_RAND_50))
            text_out(p, " somewhat");
        else if (rf_has(known_flags, RF_RAND_25))
            text_out(p, " a bit");

        /* Adjective */
        text_out(p, " erratically");

        /* Hack -- occasional conjunction */
        if (race->speed != 110) text_out(p, ", and");
    }

    /* Speed */
    text_out(p, " ");

    if (OPT(p, effective_speed))
        lore_multiplier_speed(p, race);
    else
        lore_adjective_speed(p, race);

    /* The speed description also describes "attack speed" */
    if (rf_has(known_flags, RF_NEVER_MOVE))
    {
        text_out(p, ", but ");
        text_out_c(p, COLOUR_L_GREEN, "does not deign to chase intruders");
    }

    /* End this sentence */
    text_out(p, ". ");
}


/*
 * Append the monster AC, HP, and hit chance to a textblock.
 *
 * Known race flags are passed in for simplicity/efficiency.
 *
 * race is the monster race we are describing.
 * lore is the known information about the monster race.
 * known_flags is the preprocessed bitfield of race flags known to the player.
 */
void lore_append_toughness(struct player *p, const struct monster_race *race,
    const struct monster_lore *lore, bitflag known_flags[RF_SIZE])
{
    monster_sex_t msex = MON_SEX_NEUTER;
    long chance = 0, chance2 = 0;
    struct object *weapon = equipped_item_by_slot_name(p, "weapon");

    my_assert(race && lore);

    /* Extract a gender (if applicable) */
    msex = lore_monster_sex(race);

    /* Describe monster "toughness" */
    if (lore->armour_known)
    {
        /* Armor */
        text_out(p, "%s has an armor rating of ", lore_pronoun_nominative(msex, true));
        text_out_c(p, COLOUR_L_BLUE, "%d", race->ac);

        /* Hitpoints */
        text_out(p, ", and a");
        if (!rf_has(known_flags, RF_UNIQUE)) text_out(p, "n average");
        text_out(p, " life rating of ");
        text_out_c(p, COLOUR_L_BLUE, "%d", race->avg_hp);
        text_out(p, ". ");

        /* Player's base chance to hit */
        chance = chance_of_melee_hit(p, weapon);

        /* The following calculations are based on test_hit(); make sure to keep it in sync */
        if (chance < 9) chance = 9;
        chance2 = 12 + (100 - 12 - 5) * (chance - (race->ac * 2 / 3)) / chance;
        if (chance2 < 12) chance2 = 12;

        text_out(p, "You have a");
        if ((chance2 / 10) == 8) text_out(p, "n");
        text_out_c(p, COLOUR_L_BLUE, " %d", chance2);
        text_out(p, " percent chance to hit such a creature in melee (if you can see it). ");
    }
}


/*
 * Append the experience value description to a textblock.
 *
 * Known race flags are passed in for simplicity/efficiency.
 *
 * race is the monster race we are describing.
 * lore is the known information about the monster race.
 * known_flags is the preprocessed bitfield of race flags known to the player.
 */
void lore_append_exp(struct player *p, const struct monster_race *race,
    const struct monster_lore *lore, bitflag known_flags[RF_SIZE])
{
    const char *ordinal, *article;
    char buf[20] = "";
    long exp_integer, exp_fraction;
    s16b level;

    /* Check legality and that this is a placeable monster */
    my_assert(race && lore);
    if (!race->rarity) return;

    /* Introduction */
    if (rf_has(known_flags, RF_UNIQUE))
        text_out(p, "Killing");
    else
        text_out(p, "A kill of");

    text_out(p, " this creature");

    /* Calculate the integer exp part */
    exp_integer = (long)race->mexp * race->level / p->lev;

    /*
     * Calculate the fractional exp part scaled by 100, must use long arithmetic
     * to avoid overflow
     */
    exp_fraction = ((((long)race->mexp * race->level % p->lev) * (long)1000 / p->lev + 5) / 10);

    /* Calculate textual representation */
    strnfmt(buf, sizeof(buf), "%d", exp_integer);
    if (exp_fraction) my_strcat(buf, format(".%02d", exp_fraction), sizeof(buf));

    /* Mention the experience */
    text_out(p, " is worth ");
    text_out_c(p, COLOUR_BLUE, "%s point%s", buf, PLURAL((exp_integer == 1) && (exp_fraction == 0)));

    /* Take account of annoying English */
    ordinal = "th";
    level = p->lev % 10;
    if ((p->lev / 10) == 1) /* nothing */;
    else if (level == 1) ordinal = "st";
    else if (level == 2) ordinal = "nd";
    else if (level == 3) ordinal = "rd";

    /* Take account of "leading vowels" in numbers */
    article = "a";
    level = p->lev;
    if ((level == 8) || (level == 11) || (level == 18)) article = "an";

    /* Mention the dependance on the player's level */
    text_out(p, " for %s %u%s level character. ", article, level, ordinal);
}


/*
 * Append the monster drop description to a textblock.
 *
 * Known race flags are passed in for simplicity/efficiency.
 *
 * race is the monster race we are describing.
 * lore is the known information about the monster race.
 * known_flags is the preprocessed bitfield of race flags known to the player.
 */
void lore_append_drop(struct player *p, const struct monster_race *race,
    const struct monster_lore *lore, bitflag known_flags[RF_SIZE])
{
    int n = 0;
    monster_sex_t msex = MON_SEX_NEUTER;

    my_assert(race && lore);
    if (!lore->drop_known) return;

    /* Extract a gender (if applicable) */
    msex = lore_monster_sex(race);

    /* Count maximum drop */
    n = mon_create_drop_count(race, true);

    /* Drops gold and/or items */
    if (n > 0)
    {
        bool only_item = rf_has(known_flags, RF_ONLY_ITEM);
        bool only_gold = rf_has(known_flags, RF_ONLY_GOLD);

        /* Paranoia */
        if (only_item && only_gold) return;

        text_out(p, "%s may carry", lore_pronoun_nominative(msex, true));

        /* Count drops */
        if (n == 1) text_out_c(p, COLOUR_BLUE, " a single ");
        else if (n == 2) text_out_c(p, COLOUR_BLUE, " one or two ");
        else
        {
            text_out(p, " up to ");
            text_out_c(p, COLOUR_BLUE, "%d ", n);
        }

        /* Quality */
        if (rf_has(known_flags, RF_DROP_GREAT)) text_out_c(p, COLOUR_BLUE, "exceptional ");
        else if (rf_has(known_flags, RF_DROP_GOOD)) text_out_c(p, COLOUR_BLUE, "good ");

        /* Objects or treasures */
        if (only_item && !only_gold)
            text_out_c(p, COLOUR_BLUE, "object%s", PLURAL(n));
        else if (!only_item && only_gold)
            text_out_c(p, COLOUR_BLUE, "treasure%s", PLURAL(n));
        else if (!only_item && !only_gold)
            text_out_c(p, COLOUR_BLUE, "object%s or treasure%s", PLURAL(n), PLURAL(n));

        text_out(p, ". ");
    }
}


/*
 * Append the monster abilities (resists, weaknesses, other traits) to a textblock.
 *
 * Known race flags are passed in for simplicity/efficiency.
 *
 * race is the monster race we are describing.
 * lore is the known information about the monster race.
 * known_flags is the preprocessed bitfield of race flags known to the player.
 */
void lore_append_abilities(struct player *p, const struct monster_race *race,
    const struct monster_lore *lore, bitflag known_flags[RF_SIZE])
{
    int flag;
    char start[40];
    const char *initial_pronoun;
    bool prev = false;
    bitflag current_flags[RF_SIZE], test_flags[RF_SIZE], susc_flags[RF_SIZE];
    monster_sex_t msex = MON_SEX_NEUTER;

    my_assert(race && lore);

    /* Extract a gender (if applicable) and get a pronoun for the start of sentences */
    msex = lore_monster_sex(race);
    initial_pronoun = lore_pronoun_nominative(msex, true);

    /* Describe environment-shaping abilities. */
    create_mon_flag_mask(current_flags, RFT_ALTER, RFT_MAX);
    rf_inter(current_flags, known_flags);
    strnfmt(start, sizeof(start), "%s can ", initial_pronoun);
    lore_append_clause(p, current_flags, COLOUR_WHITE, start, "and", ". ");

    /* Describe detection traits */
    create_mon_flag_mask(current_flags, RFT_DET, RFT_MAX);
    rf_inter(current_flags, known_flags);
    strnfmt(start, sizeof(start), "%s is ", initial_pronoun);
    lore_append_clause(p, current_flags, COLOUR_WHITE, start, "and", ". ");

    /* Describe special things */
    if (rf_has(known_flags, RF_UNAWARE))
        text_out(p, "%s disguises itself to look like something else. ", initial_pronoun);
    if (rf_has(known_flags, RF_MULTIPLY))
        text_out_c(p, COLOUR_ORANGE, "%s breeds explosively. ", initial_pronoun);
    if (rf_has(known_flags, RF_REGENERATE))
        text_out(p, "%s regenerates quickly. ", initial_pronoun);

    /* Describe light */
    if (race->light > 1)
    {
        text_out(p, "%s illuminates %s surroundings. ", initial_pronoun,
            lore_pronoun_possessive(msex, false));
    }
    else if (race->light == 1)
        text_out(p, "%s is illuminated. ", initial_pronoun);
    else if (race->light == -1)
        text_out(p, "%s is darkened. ", initial_pronoun);
    else if (race->light < -1)
    {
        text_out(p, "%s shrouds %s surroundings in darkness. ", initial_pronoun,
            lore_pronoun_possessive(msex, false));
    }

    if (rf_has(known_flags, RF_ANTI_MAGIC))
        text_out(p, "%s is surrounded by an anti-magic field. ", initial_pronoun);
    if (rf_has(known_flags, RF_LEVITATE))
        text_out(p, "%s floats just above the floor. ", initial_pronoun);

    /* Collect susceptibilities */
    create_mon_flag_mask(current_flags, RFT_VULN, RFT_VULN_I, RFT_MAX);
    rf_inter(current_flags, known_flags);
    strnfmt(start, sizeof(start), "%s is hurt by ", initial_pronoun);
    lore_append_clause(p, current_flags, COLOUR_VIOLET, start, "and", "");
    if (!rf_is_empty(current_flags)) prev = true;

    /* Collect immunities and resistances */
    create_mon_flag_mask(current_flags, RFT_RES, RFT_MAX);
    rf_inter(current_flags, known_flags);

    /* Note lack of vulnerability as a resistance */
    create_mon_flag_mask(test_flags, RFT_VULN, RFT_MAX);
    for (flag = rf_next(test_flags, FLAG_START); flag; flag = rf_next(test_flags, flag + 1))
    {
        if (rf_has(lore->flags, flag) && !rf_has(known_flags, flag))
            rf_on(current_flags, flag);
    }

    if (prev)
        my_strcpy(start, ", but resists ", sizeof(start));
    else
        strnfmt(start, sizeof(start), "%s resists ", initial_pronoun);
    lore_append_clause(p, current_flags, COLOUR_L_UMBER, start, "and", "");
    if (!rf_is_empty(current_flags)) prev = true;

    /* Collect known but average susceptibilities */
    rf_wipe(current_flags);
    create_mon_flag_mask(test_flags, RFT_RES, RFT_MAX);
    create_mon_flag_mask(susc_flags, RFT_VULN_I, RFT_MAX);
    for (flag = rf_next(test_flags, FLAG_START); flag; flag = rf_next(test_flags, flag + 1))
    {
        int susc_flag;
        bool vuln = false;

        /* Vulnerabilities need to be specifically removed */
        for (susc_flag = rf_next(susc_flags, FLAG_START); susc_flag;
            susc_flag = rf_next(susc_flags, susc_flag + 1))
        {
            if (streq(describe_race_flag(flag), describe_race_flag(susc_flag)))
            {
                vuln = rf_has(known_flags, susc_flag);
                break;
            }
        }

        if (rf_has(lore->flags, flag) && !rf_has(known_flags, flag) && !vuln)
            rf_on(current_flags, flag);
    }
    if (prev)
        my_strcpy(start, ", and does not resist ", sizeof(start));
    else
        strnfmt(start, sizeof(start), "%s does not resist ", initial_pronoun);

    /* Special case for undead */
    if (rf_has(known_flags, RF_UNDEAD)) rf_off(current_flags, RF_IM_NETHER);

    lore_append_clause(p, current_flags, COLOUR_L_UMBER, start, "or", "");
    if (!rf_is_empty(current_flags)) prev = true;

    /* Collect non-effects */
    create_mon_flag_mask(current_flags, RFT_PROT, RFT_MAX);
    rf_inter(current_flags, known_flags);
    if (prev)
        my_strcpy(start, ", and cannot be ", sizeof(start));
    else
        strnfmt(start, sizeof(start), "%s cannot be ", initial_pronoun);
    lore_append_clause(p, current_flags, COLOUR_L_UMBER, start, "or", "");
    if (!rf_is_empty(current_flags)) prev = true;

    /* Full stop. */
    if (prev) text_out(p, ". ");
}


/*
 * Append how the monster reacts to intruders and at what distance it does so.
 *
 * Known race flags are passed in for simplicity/efficiency.
 *
 * race is the monster race we are describing.
 * lore is the known information about the monster race.
 * known_flags is the preprocessed bitfield of race flags known to the player.
 */
void lore_append_awareness(struct player *p, const struct monster_race *race,
    const struct monster_lore *lore, bitflag known_flags[RF_SIZE])
{
    monster_sex_t msex = MON_SEX_NEUTER;

    my_assert(race && lore);

    /* Extract a gender (if applicable) and get a pronoun for the start of sentences */
    msex = lore_monster_sex(race);

    /* Do we know how aware it is? */
    if (lore->sleep_known)
    {
        const char *aware = lore_describe_awareness(race->sleep);

        text_out(p, "%s %s intruders, which %s may notice from ",
            lore_pronoun_nominative(msex, true), aware, lore_pronoun_nominative(msex, false));
        text_out_c(p, COLOUR_L_BLUE, "%d", 10 * race->hearing);
        text_out(p, " feet. ");
    }
}


/*
 * Append information about what other races the monster appears with and if they work together.
 *
 * Known race flags are passed in for simplicity/efficiency.
 *
 * race is the monster race we are describing.
 * lore is the known information about the monster race.
 * known_flags is the preprocessed bitfield of race flags known to the player.
 */
void lore_append_friends(struct player *p, const struct monster_race *race,
    const struct monster_lore *lore, bitflag known_flags[RF_SIZE])
{
    monster_sex_t msex = MON_SEX_NEUTER;

    my_assert(race && lore);

    /* Extract a gender (if applicable) and get a pronoun for the start of sentences */
    msex = lore_monster_sex(race);

    /* Describe friends */
    if (race->friends || race->friends_base)
    {
        text_out(p, "%s may appear with other monsters", lore_pronoun_nominative(msex, true));
        if (rf_has(known_flags, RF_GROUP_AI))
            text_out(p, " and hunts in packs");
        text_out(p, ". ");
    }
}


/*
 * Append the monster's attack spells to a textblock.
 *
 * Known race flags are passed in for simplicity/efficiency.
 *
 * race is the monster race we are describing.
 * lore is the known information about the monster race.
 * known_flags is the preprocessed bitfield of race flags known to the player.
 */
void lore_append_spells(struct player *p, const struct monster_race *race,
    const struct monster_lore *lore, bitflag known_flags[RF_SIZE])
{
    monster_sex_t msex = MON_SEX_NEUTER;
    bool breath = false;
    bool magic = false;
    const char *initial_pronoun;
    bool know_hp;
    bitflag current_flags[RSF_SIZE], test_flags[RSF_SIZE];

    my_assert(race && lore);

    know_hp = lore->armour_known;

    /* Extract a gender (if applicable) */
    msex = lore_monster_sex(race);
    initial_pronoun = lore_pronoun_nominative(msex, true);

    /* Collect innate (non-breath) attacks */
    create_mon_spell_mask(current_flags, RST_INNATE, RST_NONE);
    create_mon_spell_mask(test_flags, RST_BREATH, RST_NONE);
    rsf_diff(current_flags, test_flags);
    rsf_inter(current_flags, lore->spell_flags);
    if (!rsf_is_empty(current_flags))
    {
        text_out(p, "%s may ", initial_pronoun);
        lore_append_spell_clause(p, current_flags, know_hp, race, "or", ". ");
    }

    /* Collect breaths */
    create_mon_spell_mask(current_flags, RST_BREATH, RST_NONE);
    rsf_inter(current_flags, lore->spell_flags);
    if (!rsf_is_empty(current_flags))
    {
        text_out(p, "%s may ", initial_pronoun);
        text_out_c(p, COLOUR_L_RED, "breathe ");
        lore_append_spell_clause(p, current_flags, know_hp, race, "or", "");
        breath = true;
    }

    /* Collect spell information */
    rsf_copy(current_flags, lore->spell_flags);
    create_mon_spell_mask(test_flags, RST_INNATE, RST_NONE);
    rsf_diff(current_flags, test_flags);
    if (!rsf_is_empty(current_flags))
    {
        magic = true;

        /* Intro */
        if (breath)
            text_out(p, ", and may ");
        else
            text_out(p, "%s may ", initial_pronoun);

        /* Verb Phrase */
        text_out_c(p, COLOUR_L_RED, "cast spells");

        /* Adverb */
        if (rf_has(known_flags, RF_SMART)) text_out(p, " intelligently");

        /* List */
        text_out(p, " which ");
        lore_append_spell_clause(p, current_flags, know_hp, race, "or", "");
    }

    /* End the sentence about innate/other spells */
    if ((breath || magic) && race->freq_spell)
    {
        /* Calculate total casting and average frequency */
        int average_frequency = race->freq_spell;

        /* Describe the spell frequency */
        if (lore->spell_freq_known)
        {
            text_out(p, "; ");
            text_out_c(p, COLOUR_L_GREEN, "1");
            text_out(p, " time in ");
            text_out_c(p, COLOUR_L_GREEN, "%d", 100 / average_frequency);
        }

        /* Guess at the frequency */
        else if (lore->cast_innate || lore->cast_spell)
        {
            average_frequency = MAX(((average_frequency + 9) / 10) * 10, 1);

            text_out(p, "; about ");
            text_out_c(p, COLOUR_L_GREEN, "1");
            text_out(p, " time in ");
            text_out_c(p, COLOUR_L_GREEN, "%d", 100 / average_frequency);
        }

        /* End this sentence */
        text_out(p, ". ");
    }
}


/*
 * Append the monster's melee attacks to a textblock.
 *
 * Known race flags are passed in for simplicity/efficiency.
 *
 * race is the monster race we are describing.
 * lore is the known information about the monster race.
 * known_flags is the preprocessed bitfield of race flags known to the player.
 */
void lore_append_attack(struct player *p, const struct monster_race *race,
    const struct monster_lore *lore, bitflag known_flags[RF_SIZE])
{
    int i, known_attacks, total_attacks, described_count, total_centidamage;
    monster_sex_t msex = MON_SEX_NEUTER;

    my_assert(race && lore);

    /* Extract a gender (if applicable) */
    msex = lore_monster_sex(race);

    /* Notice lack of attacks */
    if (rf_has(known_flags, RF_NEVER_BLOW))
    {
        text_out(p, "%s has no physical attacks. ", lore_pronoun_nominative(msex, true));
        return;
    }

    total_attacks = 0;
    known_attacks = 0;

    /* Count the number of defined and known attacks */
    for (i = 0; i < z_info->mon_blows_max; i++)
    {
        /* Skip non-attacks */
        if (!race->blow[i].method) continue;

        total_attacks++;
        if (lore->blow_known[i]) known_attacks++;
    }

    /* Describe the lack of knowledge */
    if (known_attacks == 0)
    {
        text_out_c(p, COLOUR_ORANGE, "Nothing is known about %s attack. ",
            lore_pronoun_possessive(msex, false));
        return;
    }

    described_count = 0;
    total_centidamage = 99; // round up the final result to the next higher point

    /* Describe each melee attack */
    for (i = 0; i < z_info->mon_blows_max; i++)
    {
        random_value dice;
        const char *effect_str = NULL;

        /* Skip unknown and undefined attacks */
        if (!race->blow[i].method || !lore->blow_known[i]) continue;

        /* Extract the attack info */
        dice = race->blow[i].dice;
        effect_str = race->blow[i].effect->desc;

        /* Introduce the attack description */
        if (described_count == 0)
            text_out(p, "%s can ", lore_pronoun_nominative(msex, true));
        else if (described_count < known_attacks - 1)
            text_out(p, ", ");
        else
            text_out(p, ", and ");

        /* Describe the method */
        text_out(p, race->blow[i].method->desc);

        /* Describe the effect (if any) */
        if (effect_str && (strlen(effect_str) > 0))
        {
            int index = blow_effect_index(race->blow[i].effect->name);
            long chance = 0, chance2 = 0;

            /* Describe the attack type */
            text_out(p, " to ");
            text_out_c(p, blow_color(p, index), "%s", effect_str);

            text_out(p, " (");

            /* Describe damage (if known) */
            if (dice.dice && dice.sides)
            {
                text_out_c(p, COLOUR_L_GREEN, "%dd%d", dice.dice, dice.sides);
                text_out(p, ", ");
            }

            /* Monster's base chance to hit */
            chance = (race->blow[i].effect->power + (race->level * 3));

            /* The following calculations are based on check_hit(); make sure to keep it in sync */
            if (chance < 9) chance = 9;
            chance2 = 12 + (100 - 12 - 5) * (chance - ((p->state.ac + p->state.to_a) * 2 / 3)) / chance;
            if (chance2 < 12) chance2 = 12;

            text_out_c(p, COLOUR_L_BLUE, "%d", chance2);
            text_out(p, "%%)");

            total_centidamage += (chance2 * randcalc(dice, 0, AVERAGE));
        }

        described_count++;
    }

    text_out(p, ", averaging");
    if (known_attacks < total_attacks)
        text_out_c(p, COLOUR_ORANGE, " at least");
    text_out_c(p, COLOUR_L_GREEN, " %d", total_centidamage / 100);
    text_out(p, " damage on each of %s turns. ", lore_pronoun_possessive(msex, false));
}


/*
 * Learn everything about a monster
 */
void get_global_lore(struct player *p, const struct monster_race *race,
    struct monster_lore* lore)
{
    /* Get the lores (player + global) */
    struct monster_lore *lp_ptr = get_lore(p, race);
    const struct monster_lore *lm_ptr = &race->lore;

    /* Hack -- create a copy of the monster memory (player) */
    memcpy(lore, lp_ptr, sizeof(struct monster_lore));

    /* Hack -- allocate space for the monster lore, copy again */
    lore->blows = mem_zalloc(z_info->mon_blows_max * sizeof(byte));
    memcpy(lore->blows, lp_ptr->blows, z_info->mon_blows_max * sizeof(byte));
    lore->blow_known = mem_zalloc(z_info->mon_blows_max * sizeof(bool));
    memcpy(lore->blow_known, lp_ptr->blow_known, z_info->mon_blows_max * sizeof(bool));

    /* Add the global monster memory */
    lore->spawned = lm_ptr->spawned;
    lore->seen = lm_ptr->seen;
    lore->tdeaths = lm_ptr->tdeaths;
    lore->tkills = lm_ptr->tkills;

    /* DM has perfect monster lore */
    if (is_dm_p(p)) cheat_monster_lore(race, lore);
}


/*
 * Get the lore record for this monster race.
 */
struct monster_lore *get_lore(struct player *p, const struct monster_race *race)
{
    my_assert(race);

    return &p->lore[race->ridx];
}
