/*
 * File: mon-spell.c
 * Purpose: Monster spell casting and selection
 *
 * Copyright (c) 2010-14 Chris Carr and Nick McConnell
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
 * Spell casting
 */


typedef enum
{
    SPELL_TAG_NONE,
    SPELL_TAG_NAME,
    SPELL_TAG_PRONOUN,
    SPELL_TAG_KIN
} spell_tag_t;


static spell_tag_t spell_tag_lookup(const char *tag)
{
    if (strncmp(tag, "name", 4) == 0) return SPELL_TAG_NAME;
    if (strncmp(tag, "pronoun", 7) == 0) return SPELL_TAG_PRONOUN;
    if (strncmp(tag, "kin", 3) == 0) return SPELL_TAG_KIN;
    return SPELL_TAG_NONE;
}


/*
 * Print a monster spell message.
 *
 * We fill in the monster name and/or pronoun where necessary in
 * the message to replace instances of {name} or {pronoun}.
 */
static void spell_message(struct player *p, struct monster *mon, const struct monster_spell *spell,
    bool seen, bool hits, bool mvm)
{
    char buf[MSG_LEN] = "\0";
    char m_name[NORMAL_WID], m_poss[NORMAL_WID];
    const char *next;
    const char *s;
    const char *tag;
    const char *in_cursor;
    size_t end = 0;
    char tmp[MSG_LEN];

    /* Get the monster name (or "it") */
    monster_desc(p, m_name, sizeof(m_name), mon, MDESC_STANDARD);

    /* Get the monster possessive ("his"/"her"/"its") */
    monster_desc(p, m_poss, sizeof(m_poss), mon, MDESC_PRO_VIS | MDESC_POSS);

    /* Get the message */
    if (!seen) in_cursor = spell->blind_message;
    else if (!hits) in_cursor = spell->miss_message;
    else in_cursor = spell->message;

    next = strchr(in_cursor, '{');
    while (next)
    {
        /* Copy the text leading up to this { */
        strnfcat(buf, sizeof(buf), &end, "%.*s", next - in_cursor, in_cursor);

        s = next + 1;
        while (*s && isalpha((unsigned char) *s)) s++;

        /* Valid tag */
        if (*s == '}')
        {
            /* Start the tag after the { */
            tag = next + 1;
            in_cursor = s + 1;

            switch (spell_tag_lookup(tag))
            {
                case SPELL_TAG_NAME:
                {
                    strnfcat(buf, sizeof(buf), &end, m_name);
                    break;
                }
                case SPELL_TAG_PRONOUN:
                {
                    strnfcat(buf, sizeof(buf), &end, m_poss);
                    break;
                }
                case SPELL_TAG_KIN:
                {
                    strnfcat(buf, sizeof(buf), &end,
                        (rf_has(mon->race->flags, RF_UNIQUE)? "minions": "kin"));
                    break;
                }
                default: break;
            }
        }

        /* An invalid tag, skip it */
        else
            in_cursor = next + 1;

        next = strchr(in_cursor, '{');
    }
    strnfcat(buf, sizeof(buf), &end, in_cursor);

    /* Hack -- replace "you"/"your" by "something"/"some" */
    if (mvm)
    {
        strrep(tmp, sizeof(tmp), buf, "your", "some");
        strrep(buf, sizeof(buf), tmp, "you", "something");
    }

    if (spell->msgt) msgt(p, spell->msgt, buf);
    else msg(p, buf);
}


static const struct monster_spell *monster_spell_by_index(int index)
{
    const struct monster_spell *spell = monster_spells;

    while (spell)
    {
        if (spell->index == index) break;
        spell = spell->next;
    }
    return spell;
}


/*
 * Types of monster spells used for spell selection.
 */
static const struct mon_spell_info
{
    u16b index; /* Numerical index (RSF_FOO) */
    int type;   /* Type bitflag */
    byte save;  /* Type of saving throw */
} mon_spell_types[] =
{
    #define RSF(a, b, c) {RSF_##a, b, c},
    #include "../common/list-mon-spells.h"
    #undef RSF
    {RSF_MAX, 0, 0}
};


/*
 * Process a monster spell
 *
 * p is the affected player
 * c is the current cave level
 * index is the monster spell flag (RSF_FOO)
 * mon is the attacking monster
 * seen is whether the player can see the monster at this moment
 */
void do_mon_spell(struct player *p, struct chunk *c, int index, struct monster *mon, bool seen)
{
    bool ident = false, hits = false;

    /* Extract the monster level */
    int rlev = ((mon->race->level >= 1)? mon->race->level: 1);

    const struct monster_spell *spell = monster_spell_by_index(index);
    const struct mon_spell_info *info = &mon_spell_types[index];

    struct actor who_body;
    struct actor *who = &who_body;

    ACTOR_PLAYER(who, get_player_index(get_connection(p->conn)), p);

    /* Antimagic field prevents magical spells from working */
    if (!(info->type & (RST_BREATH | RST_ATTACK | RST_MISSILE)) && check_antimagic(p, c, mon))
        return;

    /* Antisummon field prevents summoning spells from working */
    if ((info->type & RST_SUMMON) && check_antisummon(p, mon)) return;

    /* See if it hits */
    if (spell->hit == 100)
        hits = true;
    else if (spell->hit == 0)
        hits = false;
    else
        hits = check_hit(who, spell->hit, rlev);

    /* Tell the player what's going on */
    disturb(p, 1);
    spell_message(p, mon, spell, seen, hits, false);

    if (!hits) return;

    /* Try a saving throw if available */
    if (((info->save & RSV_SKILL) && (randint0(100) < p->state.skills[SKILL_SAVE])) ||
        ((info->save & RSV_UNDEAD) && resist_undead_attacks(p, mon->race)))
    {
        msg(p, spell->save_message);
        return;
    }

    /* Learn about projectable attacks */
    if (info->type & (RST_BOLT | RST_BALL | RST_BREATH | RST_HACK))
        update_smart_learn(mon, p, 0, 0, spell->effect->params[0]);

    /* Do effects */
    effect_do(p, spell->effect, &ident, true, 0, NULL, 0, 0, mon, NULL);
}


/*
 * Spell selection
 */


/*
 * Elemental damage properties of monster breaths.
 */
static const struct breath_damage
{
    int divisor;
    int cap;
} breath[] =
{
    #define ELEM(a, b, c, d, e, f, g, h, col, pvp) {g, h},
    #include "../common/list-elements.h"
    #undef ELEM
    {0, 0}
};


static bool mon_spell_is_valid(int index)
{
    return ((index > RSF_NONE) && (index < RSF_MAX));
}


static bool monster_spell_is_projectable(int index)
{
    return ((mon_spell_types[index].type & (RST_BOLT | RST_BALL | RST_BREATH))? true: false);
}


static bool monster_spell_is_breath(int index)
{
    return ((mon_spell_types[index].type & RST_BREATH)? true: false);
}


static bool mon_spell_has_damage(int index)
{
    return ((mon_spell_types[index].type & (RST_BOLT | RST_BALL | RST_BREATH | RST_ATTACK))?
        true: false);
}


bool mon_spell_is_innate(int index)
{
    return ((mon_spell_types[index].type & RST_INNATE)? true: false);
}


/*
 * Test a spell bitflag for a type of spell.
 * Returns true if any desired type is among the flagset
 *
 * f is the set of spell flags we're testing
 * types is the spell type(s) we're looking for
 */
bool test_spells(bitflag *f, int types)
{
    const struct mon_spell_info *info;

    for (info = mon_spell_types; info->index < RSF_MAX; info++)
    {
        if (rsf_has(f, info->index) && (info->type & types))
            return true;
    }

    return false;
}


/*
 * Set a spell bitflag to allow only breaths.
 */
void set_breath(bitflag *f)
{
    const struct mon_spell_info *info;

    for (info = mon_spell_types; info->index < RSF_MAX; info++)
    {
        if (rsf_has(f, info->index) && !(info->type & RST_BREATH))
            rsf_off(f, info->index);
    }
}


/*
 * Set a spell bitflag to ignore a specific set of spell types.
 *
 * f is the set of spell flags we're pruning
 * types is the spell type(s) we're ignoring
 */
void ignore_spells(bitflag *f, int types)
{
    const struct mon_spell_info *info;

    for (info = mon_spell_types; info->index < RSF_MAX; info++)
    {
        if (rsf_has(f, info->index) && (info->type & types))
            rsf_off(f, info->index);
    }
}


/*
 * Turn off spells with a side effect or a gf_type that is resisted by
 * something in flags, subject to intelligence and chance.
 *
 * p is the affected player
 * spells is the set of spells we're pruning
 * flags is the set of flags we're testing
 * pflags is the set of player flags we're testing
 * el is the attack element
 * race is the monster type we're operating on
 */
void unset_spells(struct player *p, bitflag *spells, bitflag *flags, bitflag *pflags,
    struct element_info *el, const struct monster_race *race)
{
    const struct mon_spell_info *info;
    bool smart = rf_has(race->flags, RF_SMART);

    for (info = mon_spell_types; info->index < RSF_MAX; info++)
    {
        const struct monster_spell *spell = monster_spell_by_index(info->index);
        const struct effect *effect;

        /* Ignore missing spells */
        if (!spell) continue;
        if (!rsf_has(spells, info->index)) continue;

        /* Get the effect */
        effect = spell->effect;

        /* First we test the elemental spells */
        if (info->type & (RST_BOLT | RST_BALL | RST_BREATH))
        {
            int element = effect->params[0];
            int learn_chance = el[element].res_level * (smart? 50: 25);

            if (randint0(100) < learn_chance)
                rsf_off(spells, info->index);
        }

        /* Now others with resisted effects */
        else
        {
            while (effect)
            {
                int protect_flag = timed_protect_flag(effect->params[0]);

                /* Timed effects */
                if ((smart || !one_in_(3)) && (effect->index == EF_TIMED_INC) && protect_flag &&
                    of_has(flags, protect_flag))
                {
                    break;
                }

                /* Mana drain */
                if ((smart || one_in_(2)) && (effect->index == EF_DRAIN_MANA) &&
                    pf_has(pflags, PF_NO_MANA))
                {
                    break;
                }

                effect = effect->next;
            }
            if (effect)
                rsf_off(spells, info->index);
        }
    }
}


/*
 * Determine the damage of a spell attack which ignores monster hp
 * (i.e. bolts and balls, including arrows/boulders/storms/etc.)
 *
 * spell is the attack type
 * race is the monster race of the attacker
 * dam_aspect is the damage calc required (min, avg, max, random)
 */
static int nonhp_dam(const struct monster_spell *spell, const struct monster_race *race,
    aspect dam_aspect)
{
    int dam = 0;
    struct effect *effect = spell->effect;

    /* Set the reference race for calculations */
    ref_race = race;

    /* Now add the damage for each effect */
    while (effect)
    {
        random_value rand;

        memset(&rand, 0, sizeof(rand));
        if (effect->dice)
        {
            dice_roll(effect->dice, NULL, &rand);
            dam += randcalc(rand, 0, dam_aspect);
        }
        effect = effect->next;
    }

    ref_race = NULL;

    return dam;
}


/*
 * Determine the damage of a monster breath attack
 *
 * element is the attack element
 * hp is the monster's hp
 */
int breath_dam(int element, int hp)
{
    int dam;

    /* Damage is based on monster's current hp */
    dam = hp / breath[element].divisor;

    /* Check for maximum damage */
    if (dam > breath[element].cap) dam = breath[element].cap;

    return dam;
}


/*
 * Calculate the damage of a monster spell.
 *
 * index is the index of the spell in question.
 * hp is the hp of the casting monster.
 * race is the race of the casting monster.
 * dam_aspect is the damage calc we want (min, max, avg, random).
 */
static int mon_spell_dam(int index, int hp, const struct monster_race *race, aspect dam_aspect)
{
    const struct monster_spell *spell = monster_spell_by_index(index);

    if (monster_spell_is_breath(index)) return breath_dam(spell->effect->params[0], hp);
    return nonhp_dam(spell, race, dam_aspect);
}


/*
 * Calculate a monster's maximum spell power.
 *
 * race is the monster we're studying
 * resist is the degree of resistance we're assuming to any
 *   attack type (-1 = vulnerable ... 3 = immune)
 */
int best_spell_power(const struct monster_race *race, int resist)
{
    const struct mon_spell_info *info;
    int dam = 0, best_dam = 0;

    /* Extract the monster level */
    int rlev = ((race->level >= 1)? race->level: 1);

    for (info = mon_spell_types; info->index < RSF_MAX; info++)
    {
        if (rsf_has(race->spell_flags, info->index))
        {
            /* Get the spell */
            const struct monster_spell *spell = monster_spell_by_index(info->index);

            if (!spell) continue;

            /* Get the maximum basic damage output of the spell (could be 0) */
            dam = mon_spell_dam(info->index, mon_hp(race, MAXIMISE), race, MAXIMISE);

            /* For all attack forms the player can save against, damage is halved */
            if (spell->save_message) dam /= 2;

            /* Adjust the real damage by the assumed resistance (if it is a resistable type) */
            if (monster_spell_is_projectable(info->index))
                dam = adjust_dam(NULL, spell->effect->params[0], dam, MAXIMISE, resist);

            /* Add the power rating (crucial for non-damaging spells) */

            /* First we adjust the real damage if necessary */
            if (spell->power.dice)
                dam = (dam * spell->power.dice) / 100;

            /* Then we add any flat rating for this effect */
            dam += spell->power.base;

            /* Then we add any rlev-dependent rating */
            if (spell->power.m_bonus == 1)
                dam += (spell->power.sides * rlev) / 100;
            else if (spell->power.m_bonus == 2)
                dam += spell->power.sides / (rlev + 1);

            /* Update the best_dam tracker */
            if (dam > best_dam) best_dam = dam;
        }
    }

    return best_dam;
}


const char *mon_spell_lore_description(int index)
{
    const struct monster_spell *spell = monster_spell_by_index(index);

    if (!mon_spell_is_valid(index)) return "";

    return spell->lore_desc;
}


int mon_spell_lore_damage(int index, const struct monster_race *race, bool know_hp)
{
    int hp;

    if (!mon_spell_is_valid(index) || !mon_spell_has_damage(index)) return 0;

    hp = (know_hp? race->avg_hp: 0);
    return mon_spell_dam(index, hp, race, MAXIMISE);
}


/*
 * PWMAngband
 */


/*
 * Set all spell bitflags in a set of spell flags.
 */
void init_spells(bitflag *f)
{
    const struct mon_spell_info *info;

    for (info = mon_spell_types; info->index < RSF_MAX; info++)
    {
        if (info->index) rsf_on(f, info->index);
    }
}


/*
 * Process a monster spell (MvM)
 *
 * p is the affected player
 * c is the current cave level
 * target_m_ptr is the target monster
 * index is the monster attack flag (RSF_FOO)
 * mon is the attacking monster
 * seen is whether the player can see the monster at this moment
 */
void do_mon_spell_MvM(struct player *p, struct chunk *c, struct monster *target_m_ptr, int index,
    struct monster *mon, bool seen)
{
    bool ident = false, hits = false;

    /* Extract the monster level */
    int rlev = ((mon->race->level >= 1)? mon->race->level: 1);

    const struct monster_spell *spell = monster_spell_by_index(index);
    const struct mon_spell_info *info = &mon_spell_types[index];

    struct actor target_body;
    struct actor *target = &target_body;

    ACTOR_MONSTER(target, target_m_ptr);

    /* Antimagic field prevents magical spells from working */
    if (!(info->type & (RST_BREATH | RST_ATTACK | RST_MISSILE)) && check_antimagic(p, c, mon))
        return;

    /* Antisummon field prevents summoning spells from working */
    if ((info->type & RST_SUMMON) && check_antisummon(p, mon)) return;

    /* See if it hits */
    if (spell->hit == 100)
        hits = true;
    else if (spell->hit == 0)
        hits = false;
    else
        hits = check_hit(target, spell->hit, rlev);

    /* Tell the player what's going on */
    disturb(p, 1);
    spell_message(p, mon, spell, seen, hits, true);

    if (!hits) return;

    /* Do effects */
    effect_do(p, spell->effect, &ident, true, 0, NULL, 0, 0, mon, target_m_ptr);
}


bool is_spell_summon(int index)
{
    const struct mon_spell_info *info = &mon_spell_types[index];

    if (info->type & RST_SUMMON) return true;
    return false;
}


int spell_effect(int index)
{
    const struct monster_spell *spell = monster_spell_by_index(index);

    return spell->effect->params[0];
}


int breath_effect(struct player *p, bitflag mon_breath[RSF_SIZE])
{
    int flag, thrown_breath;
    int breath[20], num = 0;
    const struct monster_spell *spell;
    char buf[NORMAL_WID];

    /* Extract the breath attacks */
    for (flag = rsf_next(mon_breath, FLAG_START); flag != FLAG_END;
        flag = rsf_next(mon_breath, flag + 1))
    {
        breath[num++] = flag;
    }

    /* Choose a breath attack */
    thrown_breath = breath[randint0(num)];
    spell = monster_spell_by_index(thrown_breath);

    /* Message */
    msgt(p, spell->msgt, "You breathe %s.", spell->lore_desc);
    strnfmt(buf, sizeof(buf), " breathes %s.", spell->lore_desc);
    msg_misc(p, buf);

    return spell_effect(thrown_breath);
}
