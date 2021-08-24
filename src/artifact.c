/* Purpose: Artifact code */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"
#include <assert.h>

/*
 * Cursed arts now pick up bad flags at the end. To avoid idiocy like
 * arts that give resistance and vulnerability to fire simultaneously,
 * we implement the following check:
 */
static bool _add_bad_flag(object_type *o_ptr, int bad_flag, int good_flag)
{
    u32b flags[OF_ARRAY_SIZE];
    obj_flags(o_ptr, flags); /* Careful: Might be a cursed ego! */
    if (!have_flag(flags, good_flag))
    {
        add_flag(o_ptr->flags, bad_flag);
        return TRUE;
    }
    return FALSE;
}

void one_bless(obj_ptr obj)
{
    switch (randint0(16))
    {
        case 0: add_flag(obj->flags, OF_SUST_STR); break;
        case 1: add_flag(obj->flags, OF_SUST_INT); break;
        case 2: add_flag(obj->flags, OF_SUST_WIS); break;
        case 3: add_flag(obj->flags, OF_SUST_DEX); break;
        case 4: add_flag(obj->flags, OF_SUST_CON); break;
        case 5: add_flag(obj->flags, OF_SUST_CHR); break;
        case 6: add_flag(obj->flags, OF_LIGHT); break;
        case 7: add_flag(obj->flags, OF_HOLD_LIFE); break;
        case 8: add_flag(obj->flags, OF_REGEN); break;
        case 9: add_flag(obj->flags, OF_FREE_ACT); break;
        case 10: add_flag(obj->flags, OF_SEE_INVIS); break;
        case 11: add_flag(obj->flags, OF_REFLECT); break;
        case 12: add_flag(obj->flags, OF_ESP_EVIL); break;
        case 13: add_flag(obj->flags, OF_SLOW_DIGEST); break;
        case 14: add_flag(obj->flags, OF_WARNING); break;
        case 15: add_flag(obj->flags, OF_LEVITATION); break;
    }
}

/*
 * Choose one random sustain
 */
void one_sustain(object_type *o_ptr)
{
    switch (randint0(6))
    {
        case 0: add_flag(o_ptr->flags, OF_SUST_STR); break;
        case 1: add_flag(o_ptr->flags, OF_SUST_INT); break;
        case 2: add_flag(o_ptr->flags, OF_SUST_WIS); break;
        case 3: add_flag(o_ptr->flags, OF_SUST_DEX); break;
        case 4: add_flag(o_ptr->flags, OF_SUST_CON); break;
        case 5: add_flag(o_ptr->flags, OF_SUST_CHR); break;
    }
}


/*
 * Choose one random high resistance
 */
void one_high_resistance(object_type *o_ptr)
{
    switch (randint0(12))
    {
        case  0: add_flag(o_ptr->flags, OF_RES_(GF_POIS));   break;
        case  1: add_flag(o_ptr->flags, OF_RES_(GF_LIGHT));   break;
        case  2: add_flag(o_ptr->flags, OF_RES_(GF_DARK));   break;
        case  3: add_flag(o_ptr->flags, OF_RES_(GF_SHARDS)); break;
        case  4: add_flag(o_ptr->flags, OF_RES_(GF_BLIND));  break;
        case  5: add_flag(o_ptr->flags, OF_RES_(GF_CONFUSION)); break;
        case  6: add_flag(o_ptr->flags, OF_RES_(GF_SOUND));  break;
        case  7: add_flag(o_ptr->flags, OF_RES_(GF_NETHER)); break;
        case  8: add_flag(o_ptr->flags, OF_RES_(GF_NEXUS));  break;
        case  9: add_flag(o_ptr->flags, OF_RES_(GF_CHAOS));  break;
        case 10: add_flag(o_ptr->flags, OF_RES_(GF_DISENCHANT));  break;
        case 11: add_flag(o_ptr->flags, OF_RES_(GF_FEAR));   break;
    }
}
bool one_high_vulnerability(object_type *o_ptr)
{
    int attempts = 100;
    int i;
    for (i = 0; i < attempts; i++)
    {
        int gf;
        switch (randint0(12))
        {
        case  0: gf = GF_POIS; break;
        case  1: gf = GF_LIGHT; break;
        case  2: gf = GF_DARK; break;
        case  3: gf = GF_SHARDS; break;
        case  4: gf = GF_BLIND; break;
        case  5: gf = GF_CONFUSION; break;
        case  6: gf = GF_SOUND; break;
        case  7: gf = GF_NETHER; break;
        case  8: gf = GF_NEXUS; break;
        case  9: gf = GF_CHAOS; break;
        case 10: gf = GF_DISENCHANT; break;
        case 11: gf = GF_FEAR; break;
        }
        if (_add_bad_flag(o_ptr, OF_VULN_(gf), OF_RES_(gf)))
            return TRUE;
    }
    return FALSE;
}
void one_undead_resistance(object_type *o_ptr)
{
    switch (randint1(6))
    {
        case 1: add_flag(o_ptr->flags, OF_RES_(GF_COLD));   break;
        case 2: add_flag(o_ptr->flags, OF_RES_(GF_POIS));   break;
        case 3: add_flag(o_ptr->flags, OF_RES_(GF_DARK));   break;
        case 4: add_flag(o_ptr->flags, OF_RES_(GF_NETHER)); break;
        case 5: add_flag(o_ptr->flags, OF_RES_(GF_DISENCHANT));  break;
        case 6: add_flag(o_ptr->flags, OF_RES_(GF_FEAR));   break;
    }
}
void one_demon_resistance(object_type *o_ptr)
{
    switch (randint1(6))
    {
        case 1: add_flag(o_ptr->flags, OF_RES_(GF_FIRE));   break;
        case 2: add_flag(o_ptr->flags, OF_RES_(GF_CONFUSION));   break;
        case 3: add_flag(o_ptr->flags, OF_RES_(GF_NEXUS));  break;
        case 4: add_flag(o_ptr->flags, OF_RES_(GF_CHAOS));  break;
        case 5: add_flag(o_ptr->flags, OF_RES_(GF_DISENCHANT));  break;
        case 6: add_flag(o_ptr->flags, OF_RES_(GF_FEAR));   break;
    }
}

void one_holy_resistance(object_type *o_ptr)
{
    switch (randint1(4))
    {
        case 1: add_flag(o_ptr->flags, OF_RES_(GF_LIGHT));   break;
        case 2: add_flag(o_ptr->flags, OF_RES_(GF_SOUND));  break;
        case 3: add_flag(o_ptr->flags, OF_RES_(GF_SHARDS)); break;
        case 4: add_flag(o_ptr->flags, OF_RES_(GF_DISENCHANT));  break;
    }
}

/*
 * Choose one random high resistance ( except poison and disenchantment )
 */
void one_lordly_high_resistance(object_type *o_ptr)
{
    switch (randint0(10))
    {
        case 0: add_flag(o_ptr->flags, OF_RES_(GF_LIGHT));   break;
        case 1: add_flag(o_ptr->flags, OF_RES_(GF_DARK));   break;
        case 2: add_flag(o_ptr->flags, OF_RES_(GF_SHARDS)); break;
        case 3: add_flag(o_ptr->flags, OF_RES_(GF_BLIND));  break;
        case 4: add_flag(o_ptr->flags, OF_RES_(GF_CONFUSION));   break;
        case 5: add_flag(o_ptr->flags, OF_RES_(GF_SOUND));  break;
        case 6: add_flag(o_ptr->flags, OF_RES_(GF_NETHER)); break;
        case 7: add_flag(o_ptr->flags, OF_RES_(GF_NEXUS));  break;
        case 8: add_flag(o_ptr->flags, OF_RES_(GF_CHAOS));  break;
        case 9: add_flag(o_ptr->flags, OF_RES_(GF_FEAR));   break;
    }
}

/*
 * Choose one random element resistance
 */
void one_ele_resistance(object_type *o_ptr)
{
    switch (randint0(4))
    {
        case  0: add_flag(o_ptr->flags, OF_RES_(GF_ACID)); break;
        case  1: add_flag(o_ptr->flags, OF_RES_(GF_ELEC)); break;
        case  2: add_flag(o_ptr->flags, OF_RES_(GF_COLD)); break;
        case  3: add_flag(o_ptr->flags, OF_RES_(GF_FIRE)); break;
    }
}
bool one_ele_vulnerability(object_type *o_ptr)
{
    int attempts = 100;
    int i;
    for (i = 0; i < attempts; i++)
    {
        int gf = 0;
        switch (randint0(4))
        {
            case  0: gf = GF_ACID; break;
            case  1: gf = GF_ELEC; break;
            case  2: gf = GF_FIRE; break;
            case  3: gf = GF_COLD; break;
        }
        if (_add_bad_flag(o_ptr, OF_VULN_(gf), OF_RES_(gf)))
            return TRUE;
    }
    return FALSE;
}

void one_ele_slay(object_type *o_ptr)
{
    switch (randint0(5))
    {
        case  0: add_flag(o_ptr->flags, OF_BRAND_ACID); break;
        case  1: add_flag(o_ptr->flags, OF_BRAND_COLD); break;
        case  2: add_flag(o_ptr->flags, OF_BRAND_FIRE); break;
        case  3: add_flag(o_ptr->flags, OF_BRAND_ELEC); break;
        case  4: add_flag(o_ptr->flags, OF_BRAND_POIS); break;
    }
}


/*
 * Choose one random element or poison resistance
 */
void one_dragon_ele_resistance(object_type *o_ptr)
{
    if (one_in_(7))
    {
        add_flag(o_ptr->flags, OF_RES_(GF_POIS));
    }
    else
    {
        one_ele_resistance(o_ptr);
    }
}
bool one_dragon_ele_vulnerability(object_type *o_ptr)
{
    if (one_in_(7) && !have_flag(o_ptr->flags, OF_RES_(GF_POIS)))
    {
        add_flag(o_ptr->flags, OF_VULN_(GF_POIS));
        return TRUE;
    }
    return one_ele_vulnerability(o_ptr);
}

/*
 * Choose one lower rank esp
 */
void one_low_esp(object_type *o_ptr)
{
    switch (randint1(9))
    {
    case 1: add_flag(o_ptr->flags, OF_ESP_ANIMAL);   break;
    case 2: add_flag(o_ptr->flags, OF_ESP_UNDEAD);   break;
    case 3: add_flag(o_ptr->flags, OF_ESP_DEMON);   break;
    case 4: add_flag(o_ptr->flags, OF_ESP_ORC);   break;
    case 5: add_flag(o_ptr->flags, OF_ESP_TROLL);   break;
    case 6: add_flag(o_ptr->flags, OF_ESP_GIANT);   break;
    case 7: add_flag(o_ptr->flags, OF_ESP_DRAGON);   break;
    case 8: add_flag(o_ptr->flags, OF_ESP_HUMAN);   break;
    case 9: add_flag(o_ptr->flags, OF_ESP_GOOD);   break;
    }
}



/*
 * Choose one random resistance
 */
void one_resistance(object_type *o_ptr)
{
    if (one_in_(3))
    {
        one_ele_resistance(o_ptr);
    }
    else
    {
        one_high_resistance(o_ptr);
    }
}
bool one_vulnerability(object_type *o_ptr)
{
    if (one_in_(3))
        return one_ele_vulnerability(o_ptr);

    return one_high_vulnerability(o_ptr);
}

bool one_stat_biff(object_type *o_ptr)
{
    int attempts = 100;
    int i;
    for (i = 0; i < attempts; i++)
    {
        int n = randint0(6);
        if (_add_bad_flag(o_ptr, OF_DEC_STR + n, OF_STR + n))
            return TRUE;
    }
    return FALSE;
}

bool one_biff(object_type *o_ptr)
{
    int attempts = 100;
    int i;
    for (i = 0; i < attempts; i++)
    {
        int n = randint0(100);
        if (n < 10)
        {
            if (_add_bad_flag(o_ptr, OF_DEC_STEALTH, OF_STEALTH))
            {
                return TRUE;
            }
        }
        else if (n < 13)
        {
            if (_add_bad_flag(o_ptr, OF_DEC_SPEED, OF_SPEED))
            {
                return TRUE;
            }
        }
        else if (n < 17)
        {
            if (_add_bad_flag(o_ptr, OF_DEC_LIFE, OF_LIFE))
            {
                return TRUE;
            }
        }
        else if (n < 22)
        {
            if (_add_bad_flag(o_ptr, OF_DEC_MAGIC_MASTERY, OF_MAGIC_MASTERY))
            {
                return TRUE;
            }
        }
        else if (n < 30)
        {
            if (_add_bad_flag(o_ptr, OF_DEC_SPELL_CAP, OF_SPELL_CAP))
            {
                return TRUE;
            }
        }
        else if (n < 35)
        {
            if (_add_bad_flag(o_ptr, OF_DEC_SPELL_POWER, OF_SPELL_POWER))
            {
                return TRUE;
            }
        }
        else if (n < 67)
        {
            if (one_stat_biff(o_ptr))
            {
                return TRUE;
            }
        }
        else
        {
            if (one_vulnerability(o_ptr))
                return TRUE;
        }
    }
    return FALSE;
}

/*
 * Choose one random ability
 */
void one_ability(object_type *o_ptr)
{
    switch (randint0(10))
    {
    case 0: add_flag(o_ptr->flags, OF_LEVITATION);     break;
    case 1: add_flag(o_ptr->flags, OF_LIGHT);        break;
    case 2: add_flag(o_ptr->flags, OF_SEE_INVIS);   break;
    case 3: add_flag(o_ptr->flags, OF_WARNING);     break;
    case 4: add_flag(o_ptr->flags, OF_SLOW_DIGEST); break;
    case 5: add_flag(o_ptr->flags, OF_REGEN);       break;
    case 6: add_flag(o_ptr->flags, OF_FREE_ACT);    break;
    case 7: add_flag(o_ptr->flags, OF_HOLD_LIFE);   break;
    case 8:
    case 9:
        one_low_esp(o_ptr);
        break;
    }
}

void curse_object(object_type *o_ptr)
{
    int ct = randint1(2);
    int v = obj_value_real(o_ptr) / 10000;
    int i;

    o_ptr->curse_flags |= OFC_CURSED;

    one_biff(o_ptr);
    ct--;

    ct += randint1(randint1(v)); /* TODO */

    for (i = 0; i < ct; i++)
    {
        int n = randint0(70 + v*v);
        if (n < 25)
        {
            do { o_ptr->curse_flags |= get_curse(0, o_ptr); } while (one_in_(2));
        }
        else if (n < 35)
            o_ptr->curse_flags |= get_curse(1, o_ptr);
        else if (n < 40)
            o_ptr->curse_flags |= OFC_HEAVY_CURSE;
        else if (n < 45)
            o_ptr->curse_flags |= get_curse(2, o_ptr);
        else if (n < 100)
            one_biff(o_ptr);
        else if (n < 105)
            add_flag(o_ptr->flags, OF_AGGRAVATE);
        else if (n < 107)
            add_flag(o_ptr->flags, OF_TY_CURSE);
        else if (n < 108)
            o_ptr->curse_flags |= OFC_PERMA_CURSE;
        else if (n < 250)
            one_biff(o_ptr);
        else
        {
            if (one_in_(2))
            {
                o_ptr->curse_flags |= OFC_PERMA_CURSE;
                if (one_in_(2))
                    add_flag(o_ptr->flags, OF_TY_CURSE);
                if (one_in_(2))
                    add_flag(o_ptr->flags, OF_NO_TELE);
                if (one_in_(2))
                    add_flag(o_ptr->flags, OF_AGGRAVATE);
            }
            else
                one_biff(o_ptr);
        }
    }
}

void get_bloody_moon_flags(object_type *o_ptr)
{
    int dummy, i;

    for (i = 0; i < OF_ARRAY_SIZE; i++)
    {
        o_ptr->flags[i] = 0;
        o_ptr->known_flags[i] = 0;
    }

    dummy = randint1(2) + randint1(2);
    for (i = 0; i < dummy; i++)
    {
        switch (randint1(26))
        {
        case  1: add_flag(o_ptr->flags, OF_SLAY_EVIL); break;
        case  2: add_flag(o_ptr->flags, OF_SLAY_GOOD); break;
        case  3: add_flag(o_ptr->flags, OF_SLAY_LIVING); break;
        case  4: add_flag(o_ptr->flags, OF_SLAY_DRAGON); break;
        case  5: add_flag(o_ptr->flags, OF_SLAY_DEMON); break;
        case  6: add_flag(o_ptr->flags, OF_SLAY_UNDEAD); break;
        case  7: add_flag(o_ptr->flags, OF_SLAY_ANIMAL); break;
        case  8: add_flag(o_ptr->flags, OF_SLAY_HUMAN); break;
        case  9: add_flag(o_ptr->flags, OF_SLAY_TROLL); break;
        case 10: add_flag(o_ptr->flags, OF_SLAY_GIANT); break;

        case 11: add_flag(o_ptr->flags, OF_KILL_DRAGON); break;
        case 12: add_flag(o_ptr->flags, OF_KILL_DEMON); break;
        case 13: add_flag(o_ptr->flags, OF_KILL_UNDEAD); break;
        case 14: add_flag(o_ptr->flags, OF_KILL_ANIMAL); break;
        case 15: add_flag(o_ptr->flags, OF_KILL_HUMAN); break;
        case 16: add_flag(o_ptr->flags, OF_KILL_TROLL); break;
        case 17: add_flag(o_ptr->flags, OF_KILL_GIANT); break;

        case 18: add_flag(o_ptr->flags, OF_BRAND_ACID); break;
        case 19: add_flag(o_ptr->flags, OF_BRAND_ELEC); break;
        case 20: add_flag(o_ptr->flags, OF_BRAND_FIRE); break;
        case 21: add_flag(o_ptr->flags, OF_BRAND_COLD); break;
        case 22: add_flag(o_ptr->flags, OF_BRAND_POIS); break;
        case 23: add_flag(o_ptr->flags, OF_BRAND_CHAOS); break;
        case 24: add_flag(o_ptr->flags, OF_BRAND_VAMP); break;
        case 25: add_flag(o_ptr->flags, OF_BRAND_MANA); break;
        case 26: add_flag(o_ptr->flags, OF_VORPAL); break;
        }
    }

    dummy = randint1(2);
    for (i = 0; i < dummy; i++)
        one_resistance(o_ptr);

    for (i = 0; i < 2; i++)
    {
        int tmp = randint0(11);
        if (tmp < 6) add_flag(o_ptr->flags, OF_STR + tmp);
        else one_ability(o_ptr);
    }
}

/*
 * Create the artifact of the specified number
 */
bool create_named_art(art_ptr art, point_t pos)
{
    if (no_artifacts) return FALSE;
    if (!art) return FALSE;

    if ( random_artifacts
      && !(art->gen_flags & OFG_FIXED_ART)
      && randint0(100) < random_artifact_pct )
    {
        object_type forge;
        if (art_create_replacement(&forge, art, 0))
            return drop_near(&forge, pos, -1);
    }
    else
    {
        object_type forge;
        if (art_create_std(&forge, art, 0))
            return drop_near(&forge, pos, -1);
    }
    return FALSE;
}
