/*
 * File: mon-lore.c
 * Purpose: Monster memory code.
 *
 * Copyright (c) 1997-2007 Ben Harrison, James E. Wilson, Robert A. Koeneke
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
 * Initializes the color-coding of monster attacks / spells.
 *
 * This function assigns a color to each monster melee attack type and each
 * monster spell, depending on how dangerous the attack is to the player
 * given current gear and state. Attacks may be colored green (least
 * dangerous), yellow, orange, or red (most dangerous). The colors are stored
 * in `melee_colors` and `spell_colors`, which the calling function then
 * uses when printing the monster recall.
 *
 * TODO: Is it possible to simplify this using the new monster spell refactor?
 * We should be able to loop over all spell effects and check for resistance
 * in a nicer way.
 */
void get_attack_colors(struct player *p, int melee_colors[RBE_MAX], int spell_colors[RSF_MAX])
{
    int i;
    struct object *obj;
    bool known;
    bitflag f[OF_SIZE];
    struct player_state st = p->known_state;
    int tmp_col;

    /* Initialize the colors to green */
    for (i = 0; i < RBE_MAX; i++) melee_colors[i] = COLOUR_L_GREEN;
    for (i = 0; i < RSF_MAX; i++) spell_colors[i] = COLOUR_L_GREEN;

    /* Scan for potentially vulnerable items */
    for (obj = p->gear; obj; obj = obj->next)
    {
        /* Extract the flags */
        object_flags_known(obj, f, object_flavor_is_aware(p, obj));

        /*
         * Don't reveal the nature of an object.
         * Assume the player is conservative with unknown items.
         */
        known = object_is_known(p, obj);

        /* Drain charges - requires a charged item */
        if ((!known || (obj->pval > 0)) && tval_can_have_charges(obj))
            melee_colors[RBE_DRAIN_CHARGES] = COLOUR_L_RED;

        /* Steal item - requires non-artifacts */
        if (!object_is_equipped(p->body, obj) && (!known || !obj->artifact) &&
            p->lev + adj_dex_safe[st.stat_ind[STAT_DEX]] < 100)
        {
            melee_colors[RBE_EAT_ITEM] = COLOUR_L_RED;
        }

        /* Eat food - requires food */
        if (tval_is_edible(obj))
            melee_colors[RBE_EAT_FOOD] = COLOUR_YELLOW;

        /* Eat light - requires a fueled light */
        if (object_is_equipped(p->body, obj) && tval_is_light(obj) &&
            (!known || (!of_has(f, OF_NO_FUEL) && (obj->timeout > 0))))
        {
            melee_colors[RBE_EAT_LIGHT] = COLOUR_YELLOW;
        }

        /* Disenchantment - requires an enchanted item */
        if (object_is_equipped(p->body, obj) &&
            (!known || (obj->to_a > 0) || (obj->to_h > 0) || (obj->to_d > 0)) &&
            (st.el_info[ELEM_DISEN].res_level <= 0))
        {
            melee_colors[RBE_DISENCHANT] = COLOUR_L_RED;
            spell_colors[RSF_BR_DISE] = COLOUR_L_RED;
        }
    }

    /* Acid */
    if (st.el_info[ELEM_ACID].res_level == 3) tmp_col = COLOUR_L_GREEN;
    else if (st.el_info[ELEM_ACID].res_level > 0) tmp_col = COLOUR_YELLOW;
    else tmp_col = COLOUR_ORANGE;
    melee_colors[RBE_ACID] = tmp_col;
    spell_colors[RSF_BR_ACID] = tmp_col;
    spell_colors[RSF_BO_ACID] = tmp_col;
    spell_colors[RSF_BA_ACID] = tmp_col;

    /* Cold and ice */
    if (st.el_info[ELEM_COLD].res_level == 3) tmp_col = COLOUR_L_GREEN;
    else if (st.el_info[ELEM_COLD].res_level > 0) tmp_col = COLOUR_YELLOW;
    else tmp_col = COLOUR_ORANGE;
    melee_colors[RBE_COLD] = tmp_col;
    spell_colors[RSF_BR_COLD] = tmp_col;
    spell_colors[RSF_BO_COLD] = tmp_col;
    spell_colors[RSF_BA_COLD] = tmp_col;
    if (!of_has(st.flags, OF_PROT_STUN))
    {
        if (tmp_col == COLOUR_L_GREEN) tmp_col = COLOUR_YELLOW;
        else if (tmp_col == COLOUR_YELLOW) tmp_col = COLOUR_ORANGE;
        else tmp_col = COLOUR_L_RED;
    }
    spell_colors[RSF_BO_ICEE] = tmp_col;

    /* Elec */
    if (st.el_info[ELEM_ELEC].res_level == 3) tmp_col = COLOUR_L_GREEN;
    else if (st.el_info[ELEM_ELEC].res_level > 0) tmp_col = COLOUR_YELLOW;
    else tmp_col = COLOUR_ORANGE;
    melee_colors[RBE_ELEC] = tmp_col;
    spell_colors[RSF_BR_ELEC] = tmp_col;
    spell_colors[RSF_BO_ELEC] = tmp_col;
    spell_colors[RSF_BA_ELEC] = tmp_col;

    /* Fire */
    if (st.el_info[ELEM_FIRE].res_level == 3) tmp_col = COLOUR_L_GREEN;
    else if (st.el_info[ELEM_FIRE].res_level > 0) tmp_col = COLOUR_YELLOW;
    else tmp_col = COLOUR_ORANGE;
    melee_colors[RBE_FIRE] = tmp_col;
    spell_colors[RSF_BR_FIRE] = tmp_col;
    spell_colors[RSF_BO_FIRE] = tmp_col;
    spell_colors[RSF_BA_FIRE] = tmp_col;

    /* Poison (and disease) */
    if (st.el_info[ELEM_POIS].res_level <= 0)
    {
        melee_colors[RBE_POISON] = COLOUR_ORANGE;
        spell_colors[RSF_BR_POIS] = COLOUR_ORANGE;
        spell_colors[RSF_BA_POIS] = COLOUR_ORANGE;
        melee_colors[RBE_DISEASE] = COLOUR_ORANGE;
    }

    /* Nexus */
    if (st.el_info[ELEM_NEXUS].res_level <= 0)
    {
        if (st.skills[SKILL_SAVE] < 100)
            spell_colors[RSF_BR_NEXU] = COLOUR_L_RED;
        else
            spell_colors[RSF_BR_NEXU] = COLOUR_YELLOW;
    }

    /* Nether */
    if (st.el_info[ELEM_NETHER].res_level <= 0)
    {
        spell_colors[RSF_BR_NETH] = COLOUR_ORANGE;
        spell_colors[RSF_BA_NETH] = COLOUR_ORANGE;
        spell_colors[RSF_BO_NETH] = COLOUR_ORANGE;
    }

    /* Inertia, gravity, and time */
    spell_colors[RSF_BR_INER] = COLOUR_ORANGE;
    if (!of_has(st.flags, OF_PROT_STUN))
        spell_colors[RSF_BR_GRAV] = COLOUR_L_RED;
    else
        spell_colors[RSF_BR_GRAV] = COLOUR_ORANGE;
    if (st.el_info[ELEM_TIME].res_level > 0)
    {
        melee_colors[RBE_TIME] = COLOUR_YELLOW;
        spell_colors[RSF_BR_TIME] = COLOUR_ORANGE;
    }
    else
    {
        melee_colors[RBE_TIME] = COLOUR_ORANGE;
        spell_colors[RSF_BR_TIME] = COLOUR_L_RED;
    }

    /* Sound */
    if (st.el_info[ELEM_SOUND].res_level <= 0)
    {
        spell_colors[RSF_BR_SOUN] = COLOUR_ORANGE;
        if (of_has(st.flags, OF_PROT_STUN))
            spell_colors[RSF_BR_SOUN] = COLOUR_YELLOW;
    }

    /* Force and plasma */
    if (!of_has(st.flags, OF_PROT_STUN))
    {
        spell_colors[RSF_BR_WALL] = COLOUR_YELLOW;
        spell_colors[RSF_BR_PLAS] = COLOUR_ORANGE;
        spell_colors[RSF_BO_PLAS] = COLOUR_ORANGE;
    }
    else
    {
        spell_colors[RSF_BR_PLAS] = COLOUR_YELLOW;
        spell_colors[RSF_BO_PLAS] = COLOUR_YELLOW;
    }

    /* Shards */
    if (st.el_info[ELEM_SHARD].res_level <= 0)
        spell_colors[RSF_BR_SHAR] = COLOUR_ORANGE;

    /* Confusion */
    if (!of_has(st.flags, OF_PROT_CONF))
        melee_colors[RBE_CONFUSE] = COLOUR_ORANGE;

    /* Chaos */
    if (st.el_info[ELEM_CHAOS].res_level <= 0)
        spell_colors[RSF_BR_CHAO] = COLOUR_ORANGE;

    /* Light */
    if (st.el_info[ELEM_LIGHT].res_level <= 0)
        spell_colors[RSF_BR_LIGHT] = COLOUR_ORANGE;

    /* Darkness */
    if (st.el_info[ELEM_DARK].res_level <= 0)
    {
        spell_colors[RSF_BR_DARK] = COLOUR_ORANGE;
        spell_colors[RSF_BA_DARK] = COLOUR_L_RED;
    }

    /* Water */
    if (!of_has(st.flags, OF_PROT_CONF) || !of_has(st.flags, OF_PROT_STUN))
    {
        spell_colors[RSF_BR_WATE] = COLOUR_L_RED;
        spell_colors[RSF_BA_WATE] = COLOUR_L_RED;
        spell_colors[RSF_BO_WATE] = COLOUR_L_RED;
    }
    else
    {
        spell_colors[RSF_BR_WATE] = COLOUR_ORANGE;
        spell_colors[RSF_BA_WATE] = COLOUR_ORANGE;
        spell_colors[RSF_BO_WATE] = COLOUR_ORANGE;
    }

    /* Mana */
    if (st.el_info[ELEM_MANA].res_level >= 0)
    {
        spell_colors[RSF_BR_MANA] = COLOUR_ORANGE;
        spell_colors[RSF_BA_MANA] = COLOUR_ORANGE;
        spell_colors[RSF_BO_MANA] = COLOUR_ORANGE;
    }
    else
    {
        spell_colors[RSF_BR_MANA] = COLOUR_L_RED;
        spell_colors[RSF_BA_MANA] = COLOUR_L_RED;
        spell_colors[RSF_BO_MANA] = COLOUR_L_RED;
    }

    /* These attacks only apply without a perfect save */
    if (st.skills[SKILL_SAVE] < 100)
    {
        /* Amnesia */
        melee_colors[RBE_FORGET] = COLOUR_YELLOW;
        spell_colors[RSF_FORGET] = COLOUR_YELLOW;

        /* Fear */
        if (!of_has(st.flags, OF_PROT_FEAR))
        {
            melee_colors[RBE_TERRIFY] = COLOUR_YELLOW;
            spell_colors[RSF_SCARE] = COLOUR_YELLOW;
        }

        /* Paralysis and slow */
        if (!of_has(st.flags, OF_FREE_ACT))
        {
            melee_colors[RBE_PARALYZE] = COLOUR_L_RED;
            spell_colors[RSF_HOLD] = COLOUR_L_RED;
            spell_colors[RSF_SLOW] = COLOUR_ORANGE;
        }

        /* Blind */
        if (!of_has(st.flags, OF_PROT_BLIND))
            spell_colors[RSF_BLIND] = COLOUR_ORANGE;

        /* Confusion */
        if (!of_has(st.flags, OF_PROT_CONF))
            spell_colors[RSF_CONF] = COLOUR_ORANGE;

        /* Cause wounds */
        spell_colors[RSF_CAUSE_1] = COLOUR_YELLOW;
        spell_colors[RSF_CAUSE_2] = COLOUR_YELLOW;
        spell_colors[RSF_CAUSE_3] = COLOUR_YELLOW;
        spell_colors[RSF_CAUSE_4] = COLOUR_YELLOW;

        /* Mind blast */
        spell_colors[RSF_MIND_BLAST] = (of_has(st.flags, OF_PROT_CONF)? COLOUR_YELLOW: COLOUR_ORANGE);

        /* Brain smash slows even when conf/blind resisted */
        spell_colors[RSF_BRAIN_SMASH] = ((of_has(st.flags, OF_PROT_BLIND) &&
            of_has(st.flags, OF_FREE_ACT) &&
            of_has(st.flags, OF_PROT_CONF))? COLOUR_ORANGE: COLOUR_L_RED);
    }

    /* Gold theft */
    if (p->lev + adj_dex_safe[st.stat_ind[STAT_DEX]] < 100 && p->au)
        melee_colors[RBE_EAT_GOLD] = COLOUR_YELLOW;

    /* Melee blindness and hallucinations */
    if (!of_has(st.flags, OF_PROT_BLIND))
        melee_colors[RBE_BLIND] = COLOUR_YELLOW;
    if (st.el_info[ELEM_CHAOS].res_level <= 0)
        melee_colors[RBE_HALLU] = COLOUR_YELLOW;

    /* Stat draining is bad */
    if (!of_has(st.flags, OF_SUST_STR))
        melee_colors[RBE_LOSE_STR] = COLOUR_ORANGE;
    if (!of_has(st.flags, OF_SUST_INT))
        melee_colors[RBE_LOSE_INT] = COLOUR_ORANGE;
    if (!of_has(st.flags, OF_SUST_WIS))
        melee_colors[RBE_LOSE_WIS] = COLOUR_ORANGE;
    if (!of_has(st.flags, OF_SUST_DEX))
        melee_colors[RBE_LOSE_DEX] = COLOUR_ORANGE;
    if (!of_has(st.flags, OF_SUST_CON))
        melee_colors[RBE_LOSE_CON] = COLOUR_ORANGE;

    if (!of_has(st.flags, OF_SUST_CON)) melee_colors[RBE_DISEASE] = COLOUR_ORANGE;

    /* Drain all gets a red warning */
    if (!of_has(st.flags, OF_SUST_STR) || !of_has(st.flags, OF_SUST_INT) ||
        !of_has(st.flags, OF_SUST_WIS) || !of_has(st.flags, OF_SUST_DEX) ||
        !of_has(st.flags, OF_SUST_CON))
    {
        melee_colors[RBE_LOSE_ALL] = COLOUR_L_RED;
    }

    /* Hold life isn't 100% effective */
    melee_colors[RBE_EXP_10] = melee_colors[RBE_EXP_20] =
        melee_colors[RBE_EXP_40] = melee_colors[RBE_EXP_80] =
            (of_has(st.flags, OF_HOLD_LIFE)? COLOUR_YELLOW: COLOUR_ORANGE);

    /* Shatter is always noteworthy */
    melee_colors[RBE_SHATTER] = COLOUR_YELLOW;

    /* Heal (and drain mana) and haste are always noteworthy */
    spell_colors[RSF_HEAL] = COLOUR_YELLOW;
    spell_colors[RSF_DRAIN_MANA] = COLOUR_YELLOW;
    spell_colors[RSF_HASTE] = COLOUR_YELLOW;

    /* Player teleports and traps are annoying */
    spell_colors[RSF_TELE_TO] = COLOUR_YELLOW;
    spell_colors[RSF_TELE_AWAY] = COLOUR_YELLOW;
    if ((st.el_info[ELEM_NEXUS].res_level <= 0) && (st.skills[SKILL_SAVE] < 100))
        spell_colors[RSF_TELE_LEVEL] = COLOUR_YELLOW;
    spell_colors[RSF_TRAPS] = COLOUR_YELLOW;

    /* Summons are potentially dangerous */
    spell_colors[RSF_S_MONSTER] = COLOUR_ORANGE;
    spell_colors[RSF_S_MONSTERS] = COLOUR_ORANGE;
    spell_colors[RSF_S_KIN] = COLOUR_ORANGE;
    spell_colors[RSF_S_ANIMAL] = COLOUR_ORANGE;
    spell_colors[RSF_S_SPIDER] = COLOUR_ORANGE;
    spell_colors[RSF_S_HOUND] = COLOUR_ORANGE;
    spell_colors[RSF_S_HYDRA] = COLOUR_ORANGE;
    spell_colors[RSF_S_AINU] = COLOUR_ORANGE;
    spell_colors[RSF_S_DEMON] = COLOUR_ORANGE;
    spell_colors[RSF_S_DRAGON] = COLOUR_ORANGE;
    spell_colors[RSF_S_UNDEAD] = COLOUR_ORANGE;

    /* High level summons are very dangerous */
    spell_colors[RSF_S_HI_DEMON] = COLOUR_L_RED;
    spell_colors[RSF_S_HI_DRAGON] = COLOUR_L_RED;
    spell_colors[RSF_S_HI_UNDEAD] = COLOUR_L_RED;
    spell_colors[RSF_S_UNIQUE] = COLOUR_L_RED;
    spell_colors[RSF_S_WRAITH] = COLOUR_L_RED;

    /* Shrieking can lead to bad combos */
    spell_colors[RSF_SHRIEK] = COLOUR_ORANGE;

    /*
     * Ranged attacks can't be resisted (only mitigated by accuracy)
     * They are colored yellow to indicate the damage is a hard value
     */
    spell_colors[RSF_ARROW_X] = COLOUR_YELLOW;
    spell_colors[RSF_ARROW_1] = COLOUR_YELLOW;
    spell_colors[RSF_ARROW_2] = COLOUR_YELLOW;
    spell_colors[RSF_ARROW_3] = COLOUR_YELLOW;
    spell_colors[RSF_ARROW_4] = COLOUR_YELLOW;
    spell_colors[RSF_BOULDER] = COLOUR_YELLOW;

    /* PWMAngband */
    melee_colors[RBE_DISARM] = COLOUR_YELLOW;
    if (!p->ghost) melee_colors[RBE_FAMINE] = COLOUR_YELLOW;
    spell_colors[RSF_ANIM_DEAD] = COLOUR_ORANGE;
}


/*
 * Update which bits of lore are known
 */
void lore_update(const struct monster_race *race, struct monster_lore *lore)
{
    int i;

    if (!race || !lore) return;

    /* Assume some "obvious" flags */
    flags_set(lore->flags, RF_SIZE, RF_OBVIOUS_MASK, FLAG_END);

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
        flags_set(lore->flags, RF_SIZE, RF_RACE_MASK, FLAG_END);
        flags_set(lore->flags, RF_SIZE, RF_DROP_MASK, FLAG_END);
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

    /* Know all the flags */
    rf_setall(lore->flags);
    rsf_copy(lore->spell_flags, race->spell_flags);
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
 * Return a description for the given monster race flag.
 *
 * Returns an empty string for an out-of-range flag. Descriptions are in list-mon-flag.h.
 *
 * flag is one of the RF_ flags.
 */
static const char *lore_describe_race_flag(int flag)
{
    static const char *r_flag_description[] =
    {
        #define RF(a, b, c) c,
        #include "../common/list-mon-race-flags.h"
        #undef RF
        NULL
    };

    if ((flag <= RF_NONE) || (flag >= RF_MAX)) return "";

    return r_flag_description[flag];
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
        {110,       "quickly"},
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
 * Insert into a list the description for a given flag, if it is set.
 * Return the next index available for insertion.
 *
 * The function returns an incremented index if it inserted something;
 * otherwise, it returns the same index (which is used for the next insertion attempt).
 *
 * flag is the RF_ flag to check for in `known_flags`.
 * known_flags is the preprocessed set of flags for the lore/race.
 * list is the list in which the description will be inserted.
 * index is where in `list` the description will be inserted.
 */
static int lore_insert_flag_description(int flag, const bitflag known_flags[RF_SIZE],
    const char *list[], int index)
{
    if (rf_has(known_flags, flag))
    {
        list[index] = lore_describe_race_flag(flag);
        return index + 1;
    }

    return index;
}


/*
 * Insert into a list the description for a given flag, if a flag is not known to the player as a
 * vulnerability. Return the next index available for insertion.
 *
 * The function returns an incremented index if it inserted something;
 * otherwise, it returns the same index (which is used for the next insertion attempt).
 *
 * flag is the RF_ flag to check for in `known_flags`.
 * known_flags is the preprocessed set of flags for the lore/race.
 * lore is the base knowledge about the monster
 * list is the list in which the description will be inserted.
 * index is where in `list` the description will be inserted.
 */
static int lore_insert_unknown_vulnerability(int flag, const bitflag known_flags[RF_SIZE],
    const struct monster_lore *lore, const char *list[], int index)
{
    if (rf_has(lore->flags, flag) && !rf_has(known_flags, flag))
    {
        list[index] = lore_describe_race_flag(flag);
        return index + 1;
    }

    return index;
}


/*
 * Insert into a list the description for a spell if it is known to the player.
 * Return the next index available for insertion.
 *
 * The function returns an incremented index if it inserted something;
 * otherwise, it returns the same index (which is used for the next insertion attempt).
 *
 * spell is the RSF_ flag to describe.
 * race is the monster race of the spell.
 * lore is the player's current knowledge about the monster.
 * spell_colors is where the color for `spell` will be chosen from.
 * know_hp indicates whether or know the player has determined the monster's AC/HP.
 * name_list is the list in which the description will be inserted.
 * color_list is the list in which the selected color will be inserted.
 * damage_list is the list in which the max spell damage will be inserted.
 * index is where in `name_list`, `color_list`, and `damage_list` the description will be inserted.
 */
static int lore_insert_spell_description(int spell, const struct monster_race *race,
    const struct monster_lore *lore, const int spell_colors[RSF_MAX], bool know_hp,
    const char *name_list[], int color_list[], int damage_list[], int index)
{
    if (rsf_has(lore->spell_flags, spell))
    {
        name_list[index] = mon_spell_lore_description(spell);
        color_list[index] = spell_colors[spell];
        damage_list[index] = mon_spell_lore_damage(spell, race, know_hp);
        return index + 1;
    }

    return index;
}


/*
 * Append a list of items to a textblock, with each item using the provided attribute.
 *
 * The text that joins the list is drawn using the default attributes.
 * The list uses a serial comma ("a, b, c, and d").
 *
 * list is a list of strings that should be joined and appended; drawn with the attribute in `attribute`.
 * count is the number of items in `list`.
 * attr is the attribute each list item will be drawn with.
 * conjunction is a string that is added before the last item.
 */
static void lore_append_list(struct player *p, const char *list[], int count, byte attr,
    const char *conjunction)
{
    int i;

    my_assert(count >= 0);

    for (i = 0; i < count; i++)
    {
        if (i)
        {
            if (count > 2)
                text_out(p, ", ");
            else
                text_out(p, " ");

            if (i == count - 1)
                text_out(p, conjunction);
        }

        text_out_c(p, attr, list[i]);
    }
}


/*
 * Append a list of spell descriptions.
 *
 * This is a modified version of `lore_append_list()` to format spells,
 * without having to do a lot of allocating and freeing of formatted strings.
 *
 * name_list is a list of base spell descriptions.
 * color_list is the list of attributes which the description should be drawn with.
 * damage_list is a value that should be appended to the base spell description (if it is greater than zero).
 * count is the number of items in the lists.
 * conjunction is a string that is added before the last item.
 */
static void lore_append_spell_descriptions(struct player *p, const char *name_list[],
    int color_list[], int damage_list[], int count, const char *conjunction)
{
    int i;

    my_assert(count >= 0);

    for (i = 0; i < count; i++)
    {
        if (i)
        {
            if (count > 2)
                text_out(p, ", ");
            else
                text_out(p, " ");

            if (i == count - 1)
                text_out(p, conjunction);
        }

        text_out_c(p, color_list[i], name_list[i]);

        if (damage_list[i])
            text_out_c(p, color_list[i], " (%d)", damage_list[i]);
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

    /* Treat uniques differently */
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

    /* Not unique, but killed players */
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

    /* Normal monsters */
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
    my_assert(race && lore);

    text_out(p, "This");

    if (rf_has(race->flags, RF_ANIMAL))
        text_out_c(p, COLOUR_L_BLUE, " %s", lore_describe_race_flag(RF_ANIMAL));
    if (rf_has(race->flags, RF_EVIL))
        text_out_c(p, COLOUR_L_BLUE, " %s", lore_describe_race_flag(RF_EVIL));
    if (rf_has(race->flags, RF_UNDEAD))
        text_out_c(p, COLOUR_L_BLUE, " %s", lore_describe_race_flag(RF_UNDEAD));
    if (rf_has(race->flags, RF_NONLIVING))
        text_out_c(p, COLOUR_L_BLUE, " %s", lore_describe_race_flag(RF_NONLIVING));
    if (rf_has(race->flags, RF_METAL))
        text_out_c(p, COLOUR_L_BLUE, " %s", lore_describe_race_flag(RF_METAL));

    if (rf_has(race->flags, RF_DRAGON))
        text_out_c(p, COLOUR_L_BLUE, " %s", lore_describe_race_flag(RF_DRAGON));
    else if (rf_has(race->flags, RF_DEMON))
        text_out_c(p, COLOUR_L_BLUE, " %s", lore_describe_race_flag(RF_DEMON));
    else if (rf_has(race->flags, RF_GIANT))
        text_out_c(p, COLOUR_L_BLUE, " %s", lore_describe_race_flag(RF_GIANT));
    else if (rf_has(race->flags, RF_TROLL))
        text_out_c(p, COLOUR_L_BLUE, " %s", lore_describe_race_flag(RF_TROLL));
    else if (rf_has(race->flags, RF_ORC))
        text_out_c(p, COLOUR_L_BLUE, " %s", lore_describe_race_flag(RF_ORC));
    else
        text_out_c(p, COLOUR_L_BLUE, " creature");

    /* Describe location */
    if (race->level == 0) text_out(p, " lives in the town");
    else
    {
        byte colour = (race->level > p->max_depth)? COLOUR_RED: COLOUR_L_BLUE;

        if (rf_has(known_flags, RF_FORCE_DEPTH))
            text_out(p, " is found ");
        else
            text_out(p, " is normally found ");

        text_out(p, "at depths of ");
        text_out_c(p, colour, "%d", race->level * 50);
        text_out(p, " feet (level ");
        text_out_c(p, colour, "%d", race->level);
        text_out(p, ")");
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

    /* "at" is separate from the normal speed description in order to use the normal text colour */
    if (race->speed == 110) text_out(p, "at ");

    text_out_c(p, COLOUR_GREEN, lore_describe_speed(race->speed));

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

        /* Player's chance to hit it */
        chance = py_attack_hit_chance(p, weapon);

        /* The following calculations are based on test_hit(); make sure to keep it in sync */

        /* Avoid division by zero errors, and starting higher on the scale */
        if (chance < 9) chance = 9;

        chance2 = 90 * (chance - (race->ac * 2 / 3)) / chance + 5;

        /* There is always a 12 percent chance to hit */
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

    my_assert(race && lore);

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
    int list_index;
    const char *descs[64];
    const char *initial_pronoun;
    bool prev = false;
    monster_sex_t msex = MON_SEX_NEUTER;

    /* "Local" macros for easier reading; undef'd at end of function */
    #define LORE_INSERT_FLAG_DESCRIPTION(x) lore_insert_flag_description((x), known_flags, descs, list_index)
    #define LORE_INSERT_UNKNOWN_VULN(x) lore_insert_unknown_vulnerability((x), known_flags, lore, descs, list_index)

    my_assert(race && lore);

    /* Extract a gender (if applicable) and get a pronoun for the start of sentences */
    msex = lore_monster_sex(race);
    initial_pronoun = lore_pronoun_nominative(msex, true);

    /* Collect special abilities. */
    list_index = 0;
    list_index = LORE_INSERT_FLAG_DESCRIPTION(RF_OPEN_DOOR);
    list_index = LORE_INSERT_FLAG_DESCRIPTION(RF_BASH_DOOR);
    list_index = LORE_INSERT_FLAG_DESCRIPTION(RF_PASS_WALL);
    list_index = LORE_INSERT_FLAG_DESCRIPTION(RF_KILL_WALL);
    list_index = LORE_INSERT_FLAG_DESCRIPTION(RF_MOVE_BODY);
    list_index = LORE_INSERT_FLAG_DESCRIPTION(RF_KILL_BODY);
    list_index = LORE_INSERT_FLAG_DESCRIPTION(RF_TAKE_ITEM);
    list_index = LORE_INSERT_FLAG_DESCRIPTION(RF_KILL_ITEM);

    /* Describe special abilities. */
    if (list_index > 0)
    {
        text_out(p, "%s can ", initial_pronoun);
        lore_append_list(p, descs, list_index, COLOUR_WHITE, "and ");
        text_out(p, ". ");
    }

    /* Collect detection traits */
    list_index = 0;
    list_index = LORE_INSERT_FLAG_DESCRIPTION(RF_INVISIBLE);
    list_index = LORE_INSERT_FLAG_DESCRIPTION(RF_COLD_BLOOD);
    list_index = LORE_INSERT_FLAG_DESCRIPTION(RF_EMPTY_MIND);
    list_index = LORE_INSERT_FLAG_DESCRIPTION(RF_WEIRD_MIND);

    /* Describe detection traits */
    if (list_index > 0)
    {
        text_out(p, "%s is ", initial_pronoun);
        lore_append_list(p, descs, list_index, COLOUR_WHITE, "and ");
        text_out(p, ". ");
    }

    /* Describe special things */
    if (rf_has(known_flags, RF_UNAWARE))
        text_out(p, "%s disguises itself to look like something else. ", initial_pronoun);
    if (rf_has(known_flags, RF_MULTIPLY))
        text_out_c(p, COLOUR_ORANGE, "%s breeds explosively. ", initial_pronoun);
    if (rf_has(known_flags, RF_REGENERATE))
        text_out(p, "%s regenerates quickly. ", initial_pronoun);
    if (rf_has(known_flags, RF_HAS_LIGHT))
    {
        text_out(p, "%s illuminates %s surroundings. ", initial_pronoun,
            lore_pronoun_possessive(msex, false));
    }
    if (rf_has(known_flags, RF_ANTI_MAGIC))
        text_out(p, "%s is surrounded by an anti-magic field. ", initial_pronoun);

    /* Collect susceptibilities */
    list_index = 0;
    list_index = LORE_INSERT_FLAG_DESCRIPTION(RF_HURT_ROCK);
    list_index = LORE_INSERT_FLAG_DESCRIPTION(RF_HURT_LIGHT);
    list_index = LORE_INSERT_FLAG_DESCRIPTION(RF_HURT_FIRE);
    list_index = LORE_INSERT_FLAG_DESCRIPTION(RF_HURT_COLD);

    /* Describe susceptibilities */
    if (list_index > 0)
    {
        text_out(p, "%s is hurt by ", initial_pronoun);
        lore_append_list(p, descs, list_index, COLOUR_VIOLET, "and ");
        prev = true;
    }

    /* Collect immunities and resistances */
    list_index = 0;
    list_index = LORE_INSERT_FLAG_DESCRIPTION(RF_IM_ACID);
    list_index = LORE_INSERT_FLAG_DESCRIPTION(RF_IM_ELEC);
    list_index = LORE_INSERT_FLAG_DESCRIPTION(RF_IM_FIRE);
    list_index = LORE_INSERT_FLAG_DESCRIPTION(RF_IM_COLD);
    list_index = LORE_INSERT_FLAG_DESCRIPTION(RF_IM_POIS);
    list_index = LORE_INSERT_FLAG_DESCRIPTION(RF_IM_WATER);
    list_index = LORE_INSERT_FLAG_DESCRIPTION(RF_IM_NETHER);
    list_index = LORE_INSERT_FLAG_DESCRIPTION(RF_IM_PLASMA);
    list_index = LORE_INSERT_FLAG_DESCRIPTION(RF_IM_NEXUS);
    list_index = LORE_INSERT_FLAG_DESCRIPTION(RF_IM_DISEN);

    /* Note lack of vulnerability as a resistance */
    list_index = LORE_INSERT_UNKNOWN_VULN(RF_HURT_LIGHT);
    list_index = LORE_INSERT_UNKNOWN_VULN(RF_HURT_ROCK);

    /* Describe immunities and resistances */
    if (list_index > 0)
    {
        /* Output connecting text */
        if (prev)
            text_out(p, ", but resists ");
        else
            text_out(p, "%s resists ", initial_pronoun);

        /* Write the text */
        lore_append_list(p, descs, list_index, COLOUR_L_UMBER, "and ");
        prev = true;
    }

    /* Collect known but average susceptibilities */
    list_index = 0;
    list_index = LORE_INSERT_UNKNOWN_VULN(RF_IM_ACID);
    list_index = LORE_INSERT_UNKNOWN_VULN(RF_IM_ELEC);
    if (rf_has(lore->flags, RF_IM_FIRE) && !rf_has(known_flags, RF_IM_FIRE) &&
        !rf_has(known_flags, RF_HURT_FIRE))
    {
        descs[list_index++] = "fire";
    }
    if (rf_has(lore->flags, RF_IM_COLD) && !rf_has(known_flags, RF_IM_COLD) &&
        !rf_has(known_flags, RF_HURT_COLD))
    {
        descs[list_index++] = "cold";
    }
    list_index = LORE_INSERT_UNKNOWN_VULN(RF_IM_POIS);
    list_index = LORE_INSERT_UNKNOWN_VULN(RF_IM_WATER);
    list_index = LORE_INSERT_UNKNOWN_VULN(RF_IM_NETHER);
    list_index = LORE_INSERT_UNKNOWN_VULN(RF_IM_PLASMA);
    list_index = LORE_INSERT_UNKNOWN_VULN(RF_IM_NEXUS);
    list_index = LORE_INSERT_UNKNOWN_VULN(RF_IM_DISEN);

    /* Describe */
    if (list_index > 0)
    {
        /* Output connecting text */
        if (prev)
            text_out(p, ", and does not resist ");
        else
            text_out(p, "%s does not resist ", initial_pronoun);

        /* Write the text */
        lore_append_list(p, descs, list_index, COLOUR_L_UMBER, "or ");
        prev = true;
    }

    /* Collect non-effects */
    list_index = 0;
    list_index = LORE_INSERT_FLAG_DESCRIPTION(RF_NO_STUN);
    list_index = LORE_INSERT_FLAG_DESCRIPTION(RF_NO_FEAR);
    list_index = LORE_INSERT_FLAG_DESCRIPTION(RF_NO_CONF);
    list_index = LORE_INSERT_FLAG_DESCRIPTION(RF_NO_SLEEP);

    /* Describe non-effects */
    if (list_index > 0)
    {
        /* Output connecting text */
        if (prev)
            text_out(p, ", and cannot be ");
        else
            text_out(p, "%s cannot be ", initial_pronoun);

        lore_append_list(p, descs, list_index, COLOUR_L_UMBER, "or ");
        prev = true;
    }

    /* Full stop. */
    if (prev) text_out(p, ". ");

    #undef LORE_INSERT_FLAG_DESCRIPTION
    #undef LORE_INSERT_UNKNOWN_VULN
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
        text_out_c(p, COLOUR_L_BLUE, "%d", 10 * race->aaf);
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
 * spell_colors is a list of colors that is associated with each RSF_ spell.
 */
void lore_append_spells(struct player *p, const struct monster_race *race,
    const struct monster_lore *lore, bitflag known_flags[RF_SIZE], const int spell_colors[RSF_MAX])
{
    int i, average_frequency;
    monster_sex_t msex = MON_SEX_NEUTER;
    bool breath = false;
    bool magic = false;
    int list_index;
    const char *initial_pronoun;
    const char *name_list[64];
    int color_list[64];
    int damage_list[64];
    bool know_hp;

    /* "Local" macros for easier reading; undef'd at end of function */
    #define LORE_INSERT_SPELL_DESCRIPTION(x) \
        lore_insert_spell_description((x), race, lore, spell_colors, know_hp, name_list, color_list, damage_list, list_index)
    #define LORE_RESET_LISTS() \
    { \
        list_index = 0; \
        for (i = 0; i < 64; i++) \
        { \
            damage_list[i] = 0; \
            color_list[i] = COLOUR_WHITE; \
        } \
    }

    my_assert(race && lore);

    know_hp = lore->armour_known;

    /* Extract a gender (if applicable) */
    msex = lore_monster_sex(race);
    initial_pronoun = lore_pronoun_nominative(msex, true);

    /* Collect innate attacks */
    LORE_RESET_LISTS();
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_SHRIEK);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_ARROW_X);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_ARROW_1);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_ARROW_2);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_ARROW_3);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_ARROW_4);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_BOULDER);

    /* Describe innate attacks */
    if (list_index > 0)
    {
        text_out(p, "%s may ", initial_pronoun);
        lore_append_spell_descriptions(p, name_list, color_list, damage_list, list_index, "or ");
        text_out(p, ". ");
    }

    /* Collect breaths */
    LORE_RESET_LISTS();
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_BR_ACID);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_BR_ELEC);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_BR_FIRE);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_BR_COLD);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_BR_POIS);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_BR_NETH);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_BR_LIGHT);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_BR_DARK);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_BR_SOUN);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_BR_CHAO);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_BR_DISE);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_BR_NEXU);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_BR_TIME);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_BR_INER);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_BR_GRAV);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_BR_SHAR);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_BR_PLAS);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_BR_WALL);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_BR_MANA);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_BR_WATE);

    /* Describe breaths */
    if (list_index > 0)
    {
        /* Note breath */
        breath = true;

        /* Display */
        text_out(p, "%s may ", initial_pronoun);
        text_out_c(p, COLOUR_L_RED, "breathe ");
        lore_append_spell_descriptions(p, name_list, color_list, damage_list, list_index, "or ");
    }

    /* Collect spell information */
    LORE_RESET_LISTS();

    /* Ball spells */
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_BA_MANA);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_BA_DARK);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_BA_WATE);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_BA_NETH);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_BA_FIRE);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_BA_ACID);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_BA_COLD);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_BA_ELEC);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_BA_POIS);

    /* Bolt spells */
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_BO_MANA);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_BO_PLAS);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_BO_ICEE);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_BO_WATE);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_BO_NETH);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_BO_FIRE);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_BO_ACID);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_BO_COLD);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_BO_ELEC);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_MISSILE);

    /* Curses */
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_BRAIN_SMASH);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_MIND_BLAST);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_CAUSE_4);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_CAUSE_3);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_CAUSE_2);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_CAUSE_1);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_FORGET);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_SCARE);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_BLIND);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_CONF);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_SLOW);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_HOLD);

    /* Healing and haste */
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_DRAIN_MANA);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_HEAL);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_HASTE);

    /* Teleports */
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_BLINK);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_TPORT);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_TELE_TO);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_TELE_AWAY);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_TELE_LEVEL);

    /* Annoyances */
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_DARKNESS);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_TRAPS);

    /* Summoning */
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_S_KIN);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_S_MONSTER);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_S_MONSTERS);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_S_ANIMAL);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_S_SPIDER);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_S_HOUND);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_S_HYDRA);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_S_AINU);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_S_DEMON);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_S_UNDEAD);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_S_DRAGON);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_S_HI_UNDEAD);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_S_HI_DRAGON);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_S_HI_DEMON);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_S_WRAITH);
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_S_UNIQUE);

    /* PWMAngband */
    list_index = LORE_INSERT_SPELL_DESCRIPTION(RSF_ANIM_DEAD);

    /* Describe spells */
    if (list_index > 0)
    {
        /* Note magic */
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
        lore_append_spell_descriptions(p, name_list, color_list, damage_list, list_index, "or ");
    }

    /* End the sentence about innate/other spells */
    if (breath || magic)
    {
        /* Calculate total casting and average frequency */
        average_frequency = race->freq_spell;

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
            average_frequency = ((average_frequency + 9) / 10) * 10;
            text_out(p, "; about ");
            text_out_c(p, COLOUR_L_GREEN, "1");
            text_out(p, " time in ");
            text_out_c(p, COLOUR_L_GREEN, "%d", 100 / average_frequency);
        }

        /* End this sentence */
        text_out(p, ". ");
    }

    #undef LORE_INSERT_SPELL_DESCRIPTION
    #undef LORE_RESET_LISTS
}


/*
 * Append the monster's melee attacks to a textblock.
 *
 * Known race flags are passed in for simplicity/efficiency.
 *
 * race is the monster race we are describing.
 * lore is the known information about the monster race.
 * known_flags is the preprocessed bitfield of race flags known to the player.
 * melee_colors is a list of colors that is associated with each RBE_ effect.
 */
void lore_append_attack(struct player *p, const struct monster_race *race,
    const struct monster_lore *lore, bitflag known_flags[RF_SIZE], const int melee_colors[RBE_MAX])
{
    int i, total_attacks, described_count;
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

    /* Count the number of "known" attacks */
    for (total_attacks = 0, i = 0; i < z_info->mon_blows_max; i++)
    {
        /* Skip non-attacks */
        if (!race->blow[i].method) continue;

        /* Count known attacks */
        if (lore->blow_known[i]) total_attacks++;
    }

    /* Describe the lack of knowledge */
    if (total_attacks == 0)
    {
        text_out(p, "Nothing is known about %s attack. ", lore_pronoun_possessive(msex, false));
        return;
    }

    described_count = 0;

    /* Describe each melee attack */
    for (i = 0; i < z_info->mon_blows_max; i++)
    {
        random_value dice;
        const char *method_str = NULL;
        const char *effect_str = NULL;

        /* Skip unknown and undefined attacks */
        if (!race->blow[i].method || !lore->blow_known[i]) continue;

        /* Extract the attack info */
        dice = race->blow[i].dice;
        method_str = monster_blow_method_description(race->blow[i].method);
        effect_str = monster_blow_effect_description(race->blow[i].effect);

        /* Introduce the attack description */
        if (described_count == 0)
            text_out(p, "%s can ", lore_pronoun_nominative(msex, true));
        else if (described_count < total_attacks - 1)
            text_out(p, ", ");
        else
            text_out(p, ", and ");

        /* Describe the method */
        text_out(p, method_str);

        /* Describe the effect (if any) */
        if (effect_str && (strlen(effect_str) > 0))
        {
            /* Describe the attack type */
            text_out(p, " to ");
            text_out_c(p, melee_colors[race->blow[i].effect], "%s", effect_str);

            /* Describe damage (if known) */
            if (dice.dice && dice.sides)
            {
                text_out(p, " with damage ");
                text_out_c(p, COLOUR_L_GREEN, "%dd%d", dice.dice, dice.sides);
            }
        }

        /* Count the attacks as printed */
        described_count++;
    }

    /* Finish sentence above */
    text_out(p, ". ");
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
