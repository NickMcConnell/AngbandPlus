/* Purpose: Artifact code */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

static void random_resistance(object_type * o_ptr);
static void random_slay(object_type *o_ptr);


/* Chance of using syllables to form the name instead of the "template" files */
#define SINDARIN_NAME   10
#define TABLE_NAME      20
#define A_CURSED        13
#define WEIRD_LUCK      12
#define BIAS_LUCK       20
#define IM_LUCK         7
#define IM_DEPTH        100
bool immunity_hack = FALSE;
int slaying_hack = 0;
static bool has_pval;

/*
 * Bias luck needs to be higher than weird luck,
 * since it is usually tested several times...
 */
#define ACTIVATION_CHANCE 5


/*
 * Use for biased artifact creation
 */
static int artifact_bias;


/*
 * Choose one random sustain
 */
void one_sustain(object_type *o_ptr)
{
    switch (randint0(6))
    {
        case 0: add_flag(o_ptr->art_flags, TR_SUST_STR); break;
        case 1: add_flag(o_ptr->art_flags, TR_SUST_INT); break;
        case 2: add_flag(o_ptr->art_flags, TR_SUST_WIS); break;
        case 3: add_flag(o_ptr->art_flags, TR_SUST_DEX); break;
        case 4: add_flag(o_ptr->art_flags, TR_SUST_CON); break;
        case 5: add_flag(o_ptr->art_flags, TR_SUST_CHR); break;
    }
}


/*
 * Choose one random high resistance
 */
void one_high_resistance(object_type *o_ptr)
{
    switch (randint0(12))
    {
        case  0: add_flag(o_ptr->art_flags, TR_RES_POIS);   break;
        case  1: add_flag(o_ptr->art_flags, TR_RES_LITE);   break;
        case  2: add_flag(o_ptr->art_flags, TR_RES_DARK);   break;
        case  3: add_flag(o_ptr->art_flags, TR_RES_SHARDS); break;
        case  4: add_flag(o_ptr->art_flags, TR_RES_BLIND);  break;
        case  5: add_flag(o_ptr->art_flags, TR_RES_CONF);   break;
        case  6: add_flag(o_ptr->art_flags, TR_RES_SOUND);  break;
        case  7: add_flag(o_ptr->art_flags, TR_RES_NETHER); break;
        case  8: add_flag(o_ptr->art_flags, TR_RES_NEXUS);  break;
        case  9: add_flag(o_ptr->art_flags, TR_RES_CHAOS);  break;
        case 10: add_flag(o_ptr->art_flags, TR_RES_DISEN);  break;
        case 11: add_flag(o_ptr->art_flags, TR_RES_FEAR);   break;
    }
}


/*
 * Choose one random high resistance ( except poison and disenchantment )
 */
void one_lordly_high_resistance(object_type *o_ptr)
{
    switch (randint0(10))
    {
        case 0: add_flag(o_ptr->art_flags, TR_RES_LITE);   break;
        case 1: add_flag(o_ptr->art_flags, TR_RES_DARK);   break;
        case 2: add_flag(o_ptr->art_flags, TR_RES_SHARDS); break;
        case 3: add_flag(o_ptr->art_flags, TR_RES_BLIND);  break;
        case 4: add_flag(o_ptr->art_flags, TR_RES_CONF);   break;
        case 5: add_flag(o_ptr->art_flags, TR_RES_SOUND);  break;
        case 6: add_flag(o_ptr->art_flags, TR_RES_NETHER); break;
        case 7: add_flag(o_ptr->art_flags, TR_RES_NEXUS);  break;
        case 8: add_flag(o_ptr->art_flags, TR_RES_CHAOS);  break;
        case 9: add_flag(o_ptr->art_flags, TR_RES_FEAR);   break;
    }
}


/*
 * Choose one random element resistance
 */
void one_ele_resistance(object_type *o_ptr)
{
    switch (randint0(4))
    {
        case  0: add_flag(o_ptr->art_flags, TR_RES_ACID); break;
        case  1: add_flag(o_ptr->art_flags, TR_RES_ELEC); break;
        case  2: add_flag(o_ptr->art_flags, TR_RES_COLD); break;
        case  3: add_flag(o_ptr->art_flags, TR_RES_FIRE); break;
    }
}

void one_ele_slay(object_type *o_ptr)
{
    switch (randint0(5))
    {
        case  0: add_flag(o_ptr->art_flags, TR_BRAND_ACID); break;
        case  1: add_flag(o_ptr->art_flags, TR_BRAND_COLD); break;
        case  2: add_flag(o_ptr->art_flags, TR_BRAND_FIRE); break;
        case  3: add_flag(o_ptr->art_flags, TR_BRAND_ELEC); break;
        case  4: add_flag(o_ptr->art_flags, TR_BRAND_POIS); break;
    }
}


/*
 * Choose one random element or poison resistance
 */
void one_dragon_ele_resistance(object_type *o_ptr)
{
    if (one_in_(7))
    {
        add_flag(o_ptr->art_flags, TR_RES_POIS);
    }
    else
    {
        one_ele_resistance(o_ptr);
    }
}


/*
 * Choose one lower rank esp
 */
void one_low_esp(object_type *o_ptr)
{
    switch (randint1(9))
    {
    case 1: add_flag(o_ptr->art_flags, TR_ESP_ANIMAL);   break;
    case 2: add_flag(o_ptr->art_flags, TR_ESP_UNDEAD);   break;
    case 3: add_flag(o_ptr->art_flags, TR_ESP_DEMON);   break;
    case 4: add_flag(o_ptr->art_flags, TR_ESP_ORC);   break;
    case 5: add_flag(o_ptr->art_flags, TR_ESP_TROLL);   break;
    case 6: add_flag(o_ptr->art_flags, TR_ESP_GIANT);   break;
    case 7: add_flag(o_ptr->art_flags, TR_ESP_DRAGON);   break;
    case 8: add_flag(o_ptr->art_flags, TR_ESP_HUMAN);   break;
    case 9: add_flag(o_ptr->art_flags, TR_ESP_GOOD);   break;
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


/*
 * Choose one random ability
 */
void one_ability(object_type *o_ptr)
{
    switch (randint0(10))
    {
    case 0: add_flag(o_ptr->art_flags, TR_LEVITATION);     break;
    case 1: add_flag(o_ptr->art_flags, TR_LITE);        break;
    case 2: add_flag(o_ptr->art_flags, TR_SEE_INVIS);   break;
    case 3: add_flag(o_ptr->art_flags, TR_WARNING);     break;
    case 4: add_flag(o_ptr->art_flags, TR_SLOW_DIGEST); break;
    case 5: add_flag(o_ptr->art_flags, TR_REGEN);       break;
    case 6: add_flag(o_ptr->art_flags, TR_FREE_ACT);    break;
    case 7: add_flag(o_ptr->art_flags, TR_HOLD_LIFE);   break;
    case 8:
    case 9:
        one_low_esp(o_ptr);
        break;
    }
}


static void curse_artifact(object_type * o_ptr)
{
    if (o_ptr->pval > 0) o_ptr->pval = 0 - (o_ptr->pval + randint1(4));
    if (o_ptr->to_a > 0) o_ptr->to_a = 0 - (o_ptr->to_a + randint1(4));
    if (o_ptr->to_h > 0) o_ptr->to_h = 0 - (o_ptr->to_h + randint1(4));
    if (o_ptr->to_d > 0) o_ptr->to_d = 0 - (o_ptr->to_d + randint1(4));

    o_ptr->curse_flags |= (TRC_HEAVY_CURSE | TRC_CURSED);
    remove_flag(o_ptr->art_flags, TR_BLESSED);

    if (one_in_(4)) o_ptr->curse_flags |= TRC_PERMA_CURSE;
    if (one_in_(3)) add_flag(o_ptr->art_flags, TR_TY_CURSE);
    if (one_in_(2)) add_flag(o_ptr->art_flags, TR_AGGRAVATE);
    if (one_in_(3)) add_flag(o_ptr->art_flags, TR_DRAIN_EXP);
    if (one_in_(2)) add_flag(o_ptr->art_flags, TR_TELEPORT);
    else if (one_in_(3)) add_flag(o_ptr->art_flags, TR_NO_TELE);

    if ((p_ptr->pclass != CLASS_WARRIOR) && (p_ptr->pclass != CLASS_ARCHER) && (p_ptr->pclass != CLASS_CAVALRY) && (p_ptr->pclass != CLASS_BERSERKER) && (p_ptr->pclass != CLASS_WEAPONSMITH) && one_in_(3))
        add_flag(o_ptr->art_flags, TR_NO_MAGIC);
}


static void random_plus(object_type * o_ptr)
{
    int this_type = (object_is_weapon_ammo(o_ptr) ? 23 : 19);

    switch (artifact_bias)
    {
    case BIAS_WARRIOR:
        if (!(have_flag(o_ptr->art_flags, TR_STR)))
        {
            add_flag(o_ptr->art_flags, TR_STR);
            if (one_in_(2)) return;
        }

        if (!(have_flag(o_ptr->art_flags, TR_CON)))
        {
            add_flag(o_ptr->art_flags, TR_CON);
            if (one_in_(2)) return;
        }

        if (!(have_flag(o_ptr->art_flags, TR_DEX)))
        {
            add_flag(o_ptr->art_flags, TR_DEX);
            if (one_in_(2)) return;
        }
        break;

    case BIAS_MAGE:
        if (!(have_flag(o_ptr->art_flags, TR_INT)))
        {
            add_flag(o_ptr->art_flags, TR_INT);
            if (one_in_(2)) return;
        }
        if ((o_ptr->tval == TV_GLOVES) && !(have_flag(o_ptr->art_flags, TR_MAGIC_MASTERY)))
        {
            add_flag(o_ptr->art_flags, TR_MAGIC_MASTERY);
            if (one_in_(2)) return;
        }
        break;

    case BIAS_PRIESTLY:
        if (!(have_flag(o_ptr->art_flags, TR_WIS)))
        {
            add_flag(o_ptr->art_flags, TR_WIS);
            if (one_in_(2)) return;
        }
        break;

    case BIAS_RANGER:
        if (!(have_flag(o_ptr->art_flags, TR_DEX)))
        {
            add_flag(o_ptr->art_flags, TR_DEX);
            if (one_in_(2)) return;
        }

        if (!(have_flag(o_ptr->art_flags, TR_CON)))
        {
            add_flag(o_ptr->art_flags, TR_CON);
            if (one_in_(2)) return;
        }

        if (!(have_flag(o_ptr->art_flags, TR_STR)))
        {
            add_flag(o_ptr->art_flags, TR_STR);
            if (one_in_(2)) return;
        }
        break;

    case BIAS_ROGUE:
        if (!(have_flag(o_ptr->art_flags, TR_STEALTH)))
        {
            add_flag(o_ptr->art_flags, TR_STEALTH);
            if (one_in_(2)) return;
        }
        if (!(have_flag(o_ptr->art_flags, TR_SEARCH)))
        {
            add_flag(o_ptr->art_flags, TR_SEARCH);
            if (one_in_(2)) return;
        }
        break;

    case BIAS_STR:
        if (!(have_flag(o_ptr->art_flags, TR_STR)))
        {
            add_flag(o_ptr->art_flags, TR_STR);
            if (one_in_(2)) return;
        }
        break;

    case BIAS_WIS:
        if (!(have_flag(o_ptr->art_flags, TR_WIS)))
        {
            add_flag(o_ptr->art_flags, TR_WIS);
            if (one_in_(2)) return;
        }
        break;

    case BIAS_INT:
        if (!(have_flag(o_ptr->art_flags, TR_INT)))
        {
            add_flag(o_ptr->art_flags, TR_INT);
            if (one_in_(2)) return;
        }
        break;

    case BIAS_DEX:
        if (!(have_flag(o_ptr->art_flags, TR_DEX)))
        {
            add_flag(o_ptr->art_flags, TR_DEX);
            if (one_in_(2)) return;
        }
        break;

    case BIAS_CON:
        if (!(have_flag(o_ptr->art_flags, TR_CON)))
        {
            add_flag(o_ptr->art_flags, TR_CON);
            if (one_in_(2)) return;
        }
        break;

    case BIAS_CHR:
        if (!(have_flag(o_ptr->art_flags, TR_CHR)))
        {
            add_flag(o_ptr->art_flags, TR_CHR);
            if (one_in_(2)) return;
        }
        break;
    }

    if ((artifact_bias == BIAS_MAGE || artifact_bias == BIAS_PRIESTLY) && (o_ptr->tval == TV_SOFT_ARMOR) && (o_ptr->sval == SV_ROBE))
    {
        if (!(have_flag(o_ptr->art_flags, TR_DEC_MANA)) && one_in_(3))
        {
            add_flag(o_ptr->art_flags, TR_DEC_MANA);
            if (one_in_(2)) return;
        }
    }

    switch (randint1(this_type))
    {
    case 1: case 2:
        add_flag(o_ptr->art_flags, TR_STR);
        if (!artifact_bias && !one_in_(13))
            artifact_bias = BIAS_STR;
        else if (!artifact_bias && one_in_(7))
            artifact_bias = BIAS_WARRIOR;
        break;
    case 3: case 4:
        add_flag(o_ptr->art_flags, TR_INT);
        if (!artifact_bias && !one_in_(13))
            artifact_bias = BIAS_INT;
        else if (!artifact_bias && one_in_(7))
            artifact_bias = BIAS_MAGE;
        break;
    case 5: case 6:
        add_flag(o_ptr->art_flags, TR_WIS);
        if (!artifact_bias && !one_in_(13))
            artifact_bias = BIAS_WIS;
        else if (!artifact_bias && one_in_(7))
            artifact_bias = BIAS_PRIESTLY;
        break;
    case 7: case 8:
        add_flag(o_ptr->art_flags, TR_DEX);
        if (!artifact_bias && !one_in_(13))
            artifact_bias = BIAS_DEX;
        else if (!artifact_bias && one_in_(7))
            artifact_bias = BIAS_ROGUE;
        break;
    case 9: case 10:
        add_flag(o_ptr->art_flags, TR_CON);
        if (!artifact_bias && !one_in_(13))
            artifact_bias = BIAS_CON;
        else if (!artifact_bias && one_in_(9))
            artifact_bias = BIAS_RANGER;
        break;
    case 11: case 12:
        add_flag(o_ptr->art_flags, TR_CHR);
        if (!artifact_bias && !one_in_(13))
            artifact_bias = BIAS_CHR;
        break;
    case 13: case 14:
        add_flag(o_ptr->art_flags, TR_STEALTH);
        if (!artifact_bias && one_in_(3))
            artifact_bias = BIAS_ROGUE;
        break;
    case 15: case 16:
        add_flag(o_ptr->art_flags, TR_SEARCH);
        if (!artifact_bias && one_in_(9))
            artifact_bias = BIAS_RANGER;
        break;
    case 17: case 18:
        add_flag(o_ptr->art_flags, TR_INFRA);
        break;
    case 19:
        add_flag(o_ptr->art_flags, TR_SPEED);
        if (!artifact_bias && one_in_(11))
            artifact_bias = BIAS_ROGUE;
        break;
    case 20: case 21:
        if (one_in_(3))
            add_flag(o_ptr->art_flags, TR_SPEED);
        else
            add_flag(o_ptr->art_flags, TR_TUNNEL);
        break;
    case 22: case 23:
        if (o_ptr->tval == TV_BOW) random_plus(o_ptr);
        else
        {
            if (slaying_hack == 0)
            {
                if (one_in_(3))
                {
                    add_flag(o_ptr->art_flags, TR_BLOWS);
                    if (!artifact_bias && one_in_(11))
                        artifact_bias = BIAS_WARRIOR;
                }
                else
                    random_slay(o_ptr);
            }
            else
                random_plus(o_ptr);
        }
        break;
    }
}

static bool double_check_immunity(object_type * o_ptr)
{
    if (randint1(IM_DEPTH) < (object_level - 40))
        return TRUE;
    else if (immunity_hack)
    {
        return FALSE;
    }
    else
    {
        int i;
        int ct = randint1(3);

        immunity_hack = TRUE;

        if (object_is_melee_weapon(o_ptr))
        {
            for (i = 0; i < ct; i++)
                random_slay(o_ptr);

            switch (randint1(10))
            {
            case 1: case 2:
                if (!have_flag(o_ptr->art_flags, TR_SPEED))
                {
                    add_flag(o_ptr->art_flags, TR_SPEED);
                    has_pval = TRUE;
                    break;
                }
                /* vvvv FALL THRU vvvv */
            case 3: case 4: case 5: case 6:
                add_flag(o_ptr->art_flags, TR_SHOW_MODS);
                o_ptr->to_h += 5 + randint1(10);
                o_ptr->to_d += 5 + randint1(10);
                break;
            case 7: case 8:
                if (!have_flag(o_ptr->art_flags, TR_BLOWS))
                {
                    add_flag(o_ptr->art_flags, TR_BLOWS);
                    has_pval = TRUE;
                    break;
                }
                /* vvvv FALL THRU vvvv */
            case 9: case 10:
                add_flag(o_ptr->art_flags, TR_STR);
                add_flag(o_ptr->art_flags, TR_DEX);
                add_flag(o_ptr->art_flags, TR_CON);
                add_flag(o_ptr->art_flags, TR_FREE_ACT);
                add_flag(o_ptr->art_flags, TR_SEE_INVIS);
                has_pval = TRUE;
                break;
            }
        }
        else if (object_is_armour(o_ptr))
        {
            for (i = 0; i < ct; i++)
                random_resistance(o_ptr);

            switch (randint1(10))
            {
            case 1: case 2:
                if (!have_flag(o_ptr->art_flags, TR_SPEED))
                {
                    add_flag(o_ptr->art_flags, TR_SPEED);
                    has_pval = TRUE;
                    break;
                }
                /* vvvv FALL THRU vvvv */
            case 3: case 4: case 5: case 6:
                add_flag(o_ptr->art_flags, TR_SHOW_MODS);
                o_ptr->to_h += 5 + randint1(10);    /* These will get trimmed later ... */
                o_ptr->to_d += 5 + randint1(10);
                break;
            case 7: case 8:
                o_ptr->to_a += 5 + randint1(10);
                add_flag(o_ptr->art_flags, TR_FREE_ACT);
                add_flag(o_ptr->art_flags, TR_SEE_INVIS);
                add_flag(o_ptr->art_flags, TR_HOLD_LIFE);
                add_flag(o_ptr->art_flags, TR_LEVITATION);
                break;
            case 9: case 10:
                add_flag(o_ptr->art_flags, TR_STR);
                add_flag(o_ptr->art_flags, TR_DEX);
                add_flag(o_ptr->art_flags, TR_CON);
                has_pval = TRUE;
                break;
            }
        }
        else if (o_ptr->tval == TV_RING || o_ptr->tval == TV_AMULET)
        {
            /*
            ct /= 2;
            for (i = 0; i < ct; i++)
                random_resistance(o_ptr);
            */

            switch (randint1(3))
            {
            case 1:
                if (!have_flag(o_ptr->art_flags, TR_SPEED))
                {
                    add_flag(o_ptr->art_flags, TR_SPEED);
                    has_pval = TRUE;
                    break;
                }
            case 2:
                add_flag(o_ptr->art_flags, TR_SHOW_MODS);
                o_ptr->to_h += 5 + randint1(10);    /* These will get trimmed later ... */
                o_ptr->to_d += 5 + randint1(10);
                break;

            case 3:
                if (one_in_(2)) 
                {
                    add_flag(o_ptr->art_flags, TR_STR);
                    has_pval = TRUE;
                }
                if (one_in_(2)) 
                {
                    add_flag(o_ptr->art_flags, TR_INT);
                    has_pval = TRUE;
                }
                if (one_in_(2)) 
                {
                    add_flag(o_ptr->art_flags, TR_WIS);
                    has_pval = TRUE;
                }
                if (one_in_(2)) 
                {
                    add_flag(o_ptr->art_flags, TR_DEX);
                    has_pval = TRUE;
                }
                if (one_in_(2)) 
                {
                    add_flag(o_ptr->art_flags, TR_CON);
                    has_pval = TRUE;
                }
                if (one_in_(2)) 
                {
                    add_flag(o_ptr->art_flags, TR_CHR);
                    has_pval = TRUE;
                }
                break;
            }
        }
        return FALSE;
    }
}

static void random_resistance(object_type * o_ptr)
{
    switch (artifact_bias)
    {
    case BIAS_ACID:
        if (!(have_flag(o_ptr->art_flags, TR_RES_ACID)))
        {
            add_flag(o_ptr->art_flags, TR_RES_ACID);
            if (one_in_(2)) return;
        }
        if (one_in_(BIAS_LUCK) && !(have_flag(o_ptr->art_flags, TR_IM_ACID)) && double_check_immunity(o_ptr))
        {
            add_flag(o_ptr->art_flags, TR_IM_ACID);
            if (!one_in_(IM_LUCK))
            {
                remove_flag(o_ptr->art_flags, TR_IM_ELEC);
                remove_flag(o_ptr->art_flags, TR_IM_COLD);
                remove_flag(o_ptr->art_flags, TR_IM_FIRE);
            }
            if (one_in_(2)) return;
        }
        break;

    case BIAS_ELEC:
        if (!(have_flag(o_ptr->art_flags, TR_RES_ELEC)))
        {
            add_flag(o_ptr->art_flags, TR_RES_ELEC);
            if (one_in_(2)) return;
        }
        if ((o_ptr->tval >= TV_CLOAK) && (o_ptr->tval <= TV_HARD_ARMOR) &&
            !(have_flag(o_ptr->art_flags, TR_SH_ELEC)))
        {
            add_flag(o_ptr->art_flags, TR_SH_ELEC);
            if (one_in_(2)) return;
        }
        if (one_in_(BIAS_LUCK) && !(have_flag(o_ptr->art_flags, TR_IM_ELEC)) && double_check_immunity(o_ptr))
        {
            add_flag(o_ptr->art_flags, TR_IM_ELEC);
            if (!one_in_(IM_LUCK))
            {
                remove_flag(o_ptr->art_flags, TR_IM_ACID);
                remove_flag(o_ptr->art_flags, TR_IM_COLD);
                remove_flag(o_ptr->art_flags, TR_IM_FIRE);
            }

            if (one_in_(2)) return;
        }
        break;

    case BIAS_FIRE:
        if (!(have_flag(o_ptr->art_flags, TR_RES_FIRE)))
        {
            add_flag(o_ptr->art_flags, TR_RES_FIRE);
            if (one_in_(2)) return;
        }
        if ((o_ptr->tval >= TV_CLOAK) &&
            (o_ptr->tval <= TV_HARD_ARMOR) &&
            !(have_flag(o_ptr->art_flags, TR_SH_FIRE)))
        {
            add_flag(o_ptr->art_flags, TR_SH_FIRE);
            if (one_in_(2)) return;
        }
        if (one_in_(BIAS_LUCK)
            && !(have_flag(o_ptr->art_flags, TR_IM_FIRE))
            && double_check_immunity(o_ptr))
        {
            add_flag(o_ptr->art_flags, TR_IM_FIRE);
            if (!one_in_(IM_LUCK))
            {
                remove_flag(o_ptr->art_flags, TR_IM_ELEC);
                remove_flag(o_ptr->art_flags, TR_IM_COLD);
                remove_flag(o_ptr->art_flags, TR_IM_ACID);
            }
            if (one_in_(2)) return;
        }
        break;

    case BIAS_COLD:
        if (!(have_flag(o_ptr->art_flags, TR_RES_COLD)))
        {
            add_flag(o_ptr->art_flags, TR_RES_COLD);
            if (one_in_(2)) return;
        }
        if ((o_ptr->tval >= TV_CLOAK) &&
            (o_ptr->tval <= TV_HARD_ARMOR) &&
            !(have_flag(o_ptr->art_flags, TR_SH_COLD)))
        {
            add_flag(o_ptr->art_flags, TR_SH_COLD);
            if (one_in_(2)) return;
        }
        if (one_in_(BIAS_LUCK) && !(have_flag(o_ptr->art_flags, TR_IM_COLD)) && double_check_immunity(o_ptr))
        {
            add_flag(o_ptr->art_flags, TR_IM_COLD);
            if (!one_in_(IM_LUCK))
            {
                remove_flag(o_ptr->art_flags, TR_IM_ELEC);
                remove_flag(o_ptr->art_flags, TR_IM_ACID);
                remove_flag(o_ptr->art_flags, TR_IM_FIRE);
            }
            if (one_in_(2)) return;
        }
        break;

    case BIAS_POIS:
        if (!(have_flag(o_ptr->art_flags, TR_RES_POIS)))
        {
            add_flag(o_ptr->art_flags, TR_RES_POIS);
            if (one_in_(2)) return;
        }
        break;

    case BIAS_WARRIOR:
        if (!one_in_(3) && (!(have_flag(o_ptr->art_flags, TR_RES_FEAR))))
        {
            add_flag(o_ptr->art_flags, TR_RES_FEAR);
            if (one_in_(2)) return;
        }
        if (one_in_(9) && (!(have_flag(o_ptr->art_flags, TR_NO_MAGIC))))
        {
            add_flag(o_ptr->art_flags, TR_NO_MAGIC);
            if (one_in_(2)) return;
        }
        break;

    case BIAS_NECROMANTIC:
        if (!(have_flag(o_ptr->art_flags, TR_RES_NETHER)))
        {
            add_flag(o_ptr->art_flags, TR_RES_NETHER);
            if (one_in_(2)) return;
        }
        if (!(have_flag(o_ptr->art_flags, TR_RES_POIS)))
        {
            add_flag(o_ptr->art_flags, TR_RES_POIS);
            if (one_in_(2)) return;
        }
        if (!(have_flag(o_ptr->art_flags, TR_RES_DARK)))
        {
            add_flag(o_ptr->art_flags, TR_RES_DARK);
            if (one_in_(2)) return;
        }
        break;

    case BIAS_CHAOS:
        if (!(have_flag(o_ptr->art_flags, TR_RES_CHAOS)))
        {
            add_flag(o_ptr->art_flags, TR_RES_CHAOS);
            if (one_in_(2)) return;
        }
        if (!(have_flag(o_ptr->art_flags, TR_RES_CONF)))
        {
            add_flag(o_ptr->art_flags, TR_RES_CONF);
            if (one_in_(2)) return;
        }
        if (!(have_flag(o_ptr->art_flags, TR_RES_DISEN)))
        {
            add_flag(o_ptr->art_flags, TR_RES_DISEN);
            if (one_in_(2)) return;
        }
        break;
    }

    switch (randint1(42))
    {
        case 1:
            if (!one_in_(WEIRD_LUCK))
                random_resistance(o_ptr);
            else if (double_check_immunity(o_ptr))
            {
                add_flag(o_ptr->art_flags, TR_IM_ACID);
                if (!artifact_bias)
                    artifact_bias = BIAS_ACID;
            }
            break;
        case 2:
            if (!one_in_(WEIRD_LUCK))
                random_resistance(o_ptr);
            else if (double_check_immunity(o_ptr))
            {
                add_flag(o_ptr->art_flags, TR_IM_ELEC);
                if (!artifact_bias)
                    artifact_bias = BIAS_ELEC;
            }
            break;
        case 3:
            if (!one_in_(WEIRD_LUCK))
                random_resistance(o_ptr);
            else if (double_check_immunity(o_ptr))
            {
                add_flag(o_ptr->art_flags, TR_IM_COLD);
                if (!artifact_bias)
                    artifact_bias = BIAS_COLD;
            }
            break;
        case 4:
            if (!one_in_(WEIRD_LUCK))
                random_resistance(o_ptr);
            else if (double_check_immunity(o_ptr))
            {
                add_flag(o_ptr->art_flags, TR_IM_FIRE);
                if (!artifact_bias)
                    artifact_bias = BIAS_FIRE;
            }
            break;
        case 5:
        case 6:
        case 13:
            add_flag(o_ptr->art_flags, TR_RES_ACID);
            if (!artifact_bias)
                artifact_bias = BIAS_ACID;
            break;
        case 7:
        case 8:
        case 14:
            add_flag(o_ptr->art_flags, TR_RES_ELEC);
            if (!artifact_bias)
                artifact_bias = BIAS_ELEC;
            break;
        case 9:
        case 10:
        case 15:
            add_flag(o_ptr->art_flags, TR_RES_FIRE);
            if (!artifact_bias)
                artifact_bias = BIAS_FIRE;
            break;
        case 11:
        case 12:
        case 16:
            add_flag(o_ptr->art_flags, TR_RES_COLD);
            if (!artifact_bias)
                artifact_bias = BIAS_COLD;
            break;
        case 17:
        case 18:
            add_flag(o_ptr->art_flags, TR_RES_POIS);
            if (!artifact_bias && !one_in_(4))
                artifact_bias = BIAS_POIS;
            else if (!artifact_bias && one_in_(2))
                artifact_bias = BIAS_NECROMANTIC;
            else if (!artifact_bias && one_in_(2))
                artifact_bias = BIAS_ROGUE;
            break;
        case 19:
        case 20:
            add_flag(o_ptr->art_flags, TR_RES_FEAR);
            if (!artifact_bias && one_in_(3))
                artifact_bias = BIAS_WARRIOR;
            break;
        case 21:
            add_flag(o_ptr->art_flags, TR_RES_LITE);
            break;
        case 22:
            add_flag(o_ptr->art_flags, TR_RES_DARK);
            break;
        case 23:
        case 24:
            add_flag(o_ptr->art_flags, TR_RES_BLIND);
            break;
        case 25:
        case 26:
            add_flag(o_ptr->art_flags, TR_RES_CONF);
            if (!artifact_bias && one_in_(6))
                artifact_bias = BIAS_CHAOS;
            break;
        case 27:
        case 28:
            add_flag(o_ptr->art_flags, TR_RES_SOUND);
            break;
        case 29:
        case 30:
            add_flag(o_ptr->art_flags, TR_RES_SHARDS);
            break;
        case 31:
        case 32:
            add_flag(o_ptr->art_flags, TR_RES_NETHER);
            if (!artifact_bias && one_in_(3))
                artifact_bias = BIAS_NECROMANTIC;
            break;
        case 33:
        case 34:
            add_flag(o_ptr->art_flags, TR_RES_NEXUS);
            break;
        case 35:
        case 36:
            add_flag(o_ptr->art_flags, TR_RES_CHAOS);
            if (!artifact_bias && one_in_(2))
                artifact_bias = BIAS_CHAOS;
            break;
        case 37:
        case 38:
            add_flag(o_ptr->art_flags, TR_RES_DISEN);
            break;
        case 39:
            if (o_ptr->tval >= TV_CLOAK && o_ptr->tval <= TV_HARD_ARMOR)
                add_flag(o_ptr->art_flags, TR_SH_ELEC);
            else
                random_resistance(o_ptr);
            if (!artifact_bias)
                artifact_bias = BIAS_ELEC;
            break;
        case 40:
            if (o_ptr->tval >= TV_CLOAK && o_ptr->tval <= TV_HARD_ARMOR)
                add_flag(o_ptr->art_flags, TR_SH_FIRE);
            else
                random_resistance(o_ptr);
            if (!artifact_bias)
                artifact_bias = BIAS_FIRE;
            break;
        case 41:
            if (o_ptr->tval == TV_SHIELD || o_ptr->tval == TV_CLOAK ||
                o_ptr->tval == TV_HELM || o_ptr->tval == TV_HARD_ARMOR)
                add_flag(o_ptr->art_flags, TR_REFLECT);
            else
                random_resistance(o_ptr);
            break;
        case 42:
            if (o_ptr->tval >= TV_CLOAK && o_ptr->tval <= TV_HARD_ARMOR)
                add_flag(o_ptr->art_flags, TR_SH_COLD);
            else
                random_resistance(o_ptr);
            if (!artifact_bias)
                artifact_bias = BIAS_COLD;
            break;
    }
}



static void random_misc(object_type * o_ptr)
{
    switch (artifact_bias)
    {
    case BIAS_RANGER:
        if (!(have_flag(o_ptr->art_flags, TR_SUST_CON)))
        {
            add_flag(o_ptr->art_flags, TR_SUST_CON);
            if (one_in_(2)) return;
        }
        break;

    case BIAS_STR:
        if (!(have_flag(o_ptr->art_flags, TR_SUST_STR)))
        {
            add_flag(o_ptr->art_flags, TR_SUST_STR);
            if (one_in_(2)) return;
        }
        break;

    case BIAS_WIS:
        if (!(have_flag(o_ptr->art_flags, TR_SUST_WIS)))
        {
            add_flag(o_ptr->art_flags, TR_SUST_WIS);
            if (one_in_(2)) return;
        }
        break;

    case BIAS_INT:
        if (!(have_flag(o_ptr->art_flags, TR_SUST_INT)))
        {
            add_flag(o_ptr->art_flags, TR_SUST_INT);
            if (one_in_(2)) return;
        }
        break;

    case BIAS_DEX:
        if (!(have_flag(o_ptr->art_flags, TR_SUST_DEX)))
        {
            add_flag(o_ptr->art_flags, TR_SUST_DEX);
            if (one_in_(2)) return;
        }
        break;

    case BIAS_CON:
        if (!(have_flag(o_ptr->art_flags, TR_SUST_CON)))
        {
            add_flag(o_ptr->art_flags, TR_SUST_CON);
            if (one_in_(2)) return;
        }
        break;

    case BIAS_CHR:
        if (!(have_flag(o_ptr->art_flags, TR_SUST_CHR)))
        {
            add_flag(o_ptr->art_flags, TR_SUST_CHR);
            if (one_in_(2)) return;
        }
        break;

    case BIAS_CHAOS:
        if (!(have_flag(o_ptr->art_flags, TR_TELEPORT)))
        {
            add_flag(o_ptr->art_flags, TR_TELEPORT);
            if (one_in_(2)) return;
        }
        break;

    case BIAS_FIRE:
        if (!(have_flag(o_ptr->art_flags, TR_LITE)))
        {
            add_flag(o_ptr->art_flags, TR_LITE); /* Freebie */
        }
        break;
    }

    switch (randint1(33))
    {
        case 1:
            add_flag(o_ptr->art_flags, TR_SUST_STR);
            if (!artifact_bias)
                artifact_bias = BIAS_STR;
            break;
        case 2:
            add_flag(o_ptr->art_flags, TR_SUST_INT);
            if (!artifact_bias)
                artifact_bias = BIAS_INT;
            break;
        case 3:
            add_flag(o_ptr->art_flags, TR_SUST_WIS);
            if (!artifact_bias)
                artifact_bias = BIAS_WIS;
            break;
        case 4:
            add_flag(o_ptr->art_flags, TR_SUST_DEX);
            if (!artifact_bias)
                artifact_bias = BIAS_DEX;
            break;
        case 5:
            add_flag(o_ptr->art_flags, TR_SUST_CON);
            if (!artifact_bias)
                artifact_bias = BIAS_CON;
            break;
        case 6:
            add_flag(o_ptr->art_flags, TR_SUST_CHR);
            if (!artifact_bias)
                artifact_bias = BIAS_CHR;
            break;
        case 7:
        case 8:
        case 14:
            add_flag(o_ptr->art_flags, TR_FREE_ACT);
            break;
        case 9:
            add_flag(o_ptr->art_flags, TR_HOLD_LIFE);
            if (!artifact_bias && one_in_(5))
                artifact_bias = BIAS_PRIESTLY;
            else if (!artifact_bias && one_in_(6))
                artifact_bias = BIAS_NECROMANTIC;
            break;
        case 10:
        case 11:
            add_flag(o_ptr->art_flags, TR_LITE);
            break;
        case 12:
        case 13:
            add_flag(o_ptr->art_flags, TR_LEVITATION);
            break;
        case 15:
        case 16:
        case 17:
            add_flag(o_ptr->art_flags, TR_SEE_INVIS);
            break;
        case 19:
        case 20:
            add_flag(o_ptr->art_flags, TR_SLOW_DIGEST);
            break;
        case 21:
        case 22:
            add_flag(o_ptr->art_flags, TR_REGEN);
            break;
        case 23:
            add_flag(o_ptr->art_flags, TR_TELEPORT);
            break;
        case 24:
        case 25:
        case 26:
            if (object_is_armour(o_ptr))
                random_misc(o_ptr);
            else
            {
                o_ptr->to_a = 4 + randint1(11);
            }
            break;
        case 27:
        case 28:
        case 29:
        {
            int bonus_h, bonus_d;
            add_flag(o_ptr->art_flags, TR_SHOW_MODS);
            bonus_h = 4 + (randint1(11));
            bonus_d = 4 + (randint1(11));
            if ((o_ptr->tval != TV_SWORD) && (o_ptr->tval != TV_POLEARM) && (o_ptr->tval != TV_HAFTED) && (o_ptr->tval != TV_DIGGING) && (o_ptr->tval != TV_GLOVES) && (o_ptr->tval != TV_RING))
            {
                bonus_h /= 2;
                bonus_d /= 2;
            }
            o_ptr->to_h += bonus_h;
            o_ptr->to_d += bonus_d;
            break;
        }
        case 30:
            add_flag(o_ptr->art_flags, TR_NO_MAGIC);
            break;
        case 31:
            add_flag(o_ptr->art_flags, TR_NO_TELE);
            break;
        case 32:
            add_flag(o_ptr->art_flags, TR_WARNING);
            break;

        case 18:
            switch (randint1(3))
            {
            case 1:
                add_flag(o_ptr->art_flags, TR_ESP_EVIL);
                if (!artifact_bias && one_in_(3))
                    artifact_bias = BIAS_LAW;
                break;
            case 2:
                add_flag(o_ptr->art_flags, TR_ESP_NONLIVING);
                if (!artifact_bias && one_in_(3))
                    artifact_bias = BIAS_MAGE;
                break;
            case 3:
                add_flag(o_ptr->art_flags, TR_TELEPATHY);
                if (!artifact_bias && one_in_(9))
                    artifact_bias = BIAS_MAGE;
                break;
            }
            break;

        case 33:
        {
            int idx[3];
            int n = randint1(3);

            idx[0] = randint1(8);

            idx[1] = randint1(7);
            if (idx[1] >= idx[0]) idx[1]++;

            idx[2] = randint1(6);
            if (idx[2] >= idx[0]) idx[2]++;
            if (idx[2] >= idx[1]) idx[2]++;

            while (n--) switch (idx[n])
            {
            case 1:
                add_flag(o_ptr->art_flags, TR_ESP_ANIMAL);
                if (!artifact_bias && one_in_(4))
                    artifact_bias = BIAS_RANGER;
                break;
            case 2:
                add_flag(o_ptr->art_flags, TR_ESP_UNDEAD);
                if (!artifact_bias && one_in_(3))
                    artifact_bias = BIAS_PRIESTLY;
                else if (!artifact_bias && one_in_(6))
                    artifact_bias = BIAS_NECROMANTIC;
                break;
            case 3:
                add_flag(o_ptr->art_flags, TR_ESP_DEMON);
                break;
            case 4:
                add_flag(o_ptr->art_flags, TR_ESP_ORC);
                break;
            case 5:
                add_flag(o_ptr->art_flags, TR_ESP_TROLL);
                break;
            case 6:
                add_flag(o_ptr->art_flags, TR_ESP_GIANT);
                break;
            case 7:
                add_flag(o_ptr->art_flags, TR_ESP_HUMAN);
                if (!artifact_bias && one_in_(6))
                    artifact_bias = BIAS_ROGUE;
                break;
            case 8:
                add_flag(o_ptr->art_flags, TR_ESP_GOOD);
                if (!artifact_bias && one_in_(3))
                    artifact_bias = BIAS_LAW;
                break;
            }
            break;
        }
    }
}

static void random_slay_aux(object_type *o_ptr)
{
    switch (randint1(29 + object_level/10))
    {
        case 1:
        case 2:
            add_flag(o_ptr->art_flags, TR_SLAY_ANIMAL);
            break;
        case 3:
        case 4:
            add_flag(o_ptr->art_flags, TR_SLAY_HUMAN);
            break;
        case 5:
            add_flag(o_ptr->art_flags, TR_SLAY_EVIL);
            break;
        case 6:
            add_flag(o_ptr->art_flags, TR_SLAY_ORC);
            break;
        case 7:
        case 8:
            add_flag(o_ptr->art_flags, TR_SLAY_TROLL);
            break;
        case 9:
        case 10:
            add_flag(o_ptr->art_flags, TR_SLAY_GIANT);
            break;
        case 11:
        case 12:
            if (randint1(150) <= object_level)
                add_flag(o_ptr->art_flags, TR_KILL_DRAGON);
            else
                add_flag(o_ptr->art_flags, TR_SLAY_DRAGON);
            break;
        case 13:
        case 14:
            add_flag(o_ptr->art_flags, TR_SLAY_DEMON);
            if (!artifact_bias && one_in_(9))
                artifact_bias = BIAS_PRIESTLY;
            break;
        case 15:
        case 16:
            add_flag(o_ptr->art_flags, TR_SLAY_UNDEAD);
            if (!artifact_bias && one_in_(9))
                artifact_bias = BIAS_PRIESTLY;
            break;
        case 17:
        case 18:
        case 19:
            if (o_ptr->tval == TV_SWORD)
            {
                add_flag(o_ptr->art_flags, TR_VORPAL);
                if (!artifact_bias && one_in_(9))
                    artifact_bias = BIAS_WARRIOR;
            }
            else
                random_slay_aux(o_ptr);
            break;
        case 20:
            if (o_ptr->tval == TV_HAFTED || o_ptr->tval == TV_DIGGING)
                add_flag(o_ptr->art_flags, TR_IMPACT);
            else
                random_slay_aux(o_ptr);
            break;
        case 21:
            add_flag(o_ptr->art_flags, TR_BRAND_FIRE);
            if (!artifact_bias)
                artifact_bias = BIAS_FIRE;
            break;
        case 22:
            add_flag(o_ptr->art_flags, TR_BRAND_COLD);
            if (!artifact_bias)
                artifact_bias = BIAS_COLD;
            break;
        case 23:
            add_flag(o_ptr->art_flags, TR_BRAND_ELEC);
            if (!artifact_bias)
                artifact_bias = BIAS_ELEC;
            break;
        case 24:
            add_flag(o_ptr->art_flags, TR_BRAND_ACID);
            if (!artifact_bias)
                artifact_bias = BIAS_ACID;
            break;
        case 25:
            add_flag(o_ptr->art_flags, TR_BRAND_POIS);
            if (!artifact_bias && !one_in_(3))
                artifact_bias = BIAS_POIS;
            else if (!artifact_bias && one_in_(6))
                artifact_bias = BIAS_NECROMANTIC;
            else if (!artifact_bias)
                artifact_bias = BIAS_ROGUE;
            break;
        case 26:
            add_flag(o_ptr->art_flags, TR_VAMPIRIC);
            if (!artifact_bias)
                artifact_bias = BIAS_NECROMANTIC;
            break;
        case 27:
            if (have_flag(o_ptr->art_flags, TR_KILL_EVIL) && !one_in_(100))
            {
                random_slay_aux(o_ptr);
            }
            else if (have_flag(o_ptr->art_flags, TR_SLAY_EVIL) && !one_in_(20))
            {
                random_slay_aux(o_ptr);
            }
            else
            {
                add_flag(o_ptr->art_flags, TR_FORCE_WEAPON);
                if (!artifact_bias)
                    artifact_bias = (one_in_(2) ? BIAS_MAGE : BIAS_PRIESTLY);
            }
            break;
        case 28:
            add_flag(o_ptr->art_flags, TR_CHAOTIC);
            if (!artifact_bias)
                artifact_bias = BIAS_CHAOS;
            break;
        case 29:
            add_flag(o_ptr->art_flags, TR_VAMPIRIC);
            if (!artifact_bias)
                artifact_bias = BIAS_NECROMANTIC;
            break;
        case 30:
            add_flag(o_ptr->art_flags, TR_KILL_DEMON);
            if (!artifact_bias && one_in_(3))
                artifact_bias = BIAS_PRIESTLY;
            break;
        case 31:
            add_flag(o_ptr->art_flags, TR_KILL_UNDEAD);
            if (!artifact_bias && one_in_(3))
                artifact_bias = BIAS_PRIESTLY;
            break;
        default:
            if (!have_flag(o_ptr->art_flags, TR_ORDER) && !have_flag(o_ptr->art_flags, TR_WILD) && one_in_(100))
                add_flag(o_ptr->art_flags, TR_ORDER);
            else if (!have_flag(o_ptr->art_flags, TR_WILD) && !have_flag(o_ptr->art_flags, TR_ORDER) &&  one_in_(100))
                add_flag(o_ptr->art_flags, TR_WILD);
            else 
            {
                if (randint1(2500) <= object_level)
                    add_flag(o_ptr->art_flags, TR_KILL_EVIL);
                else
                    add_flag(o_ptr->art_flags, TR_SLAY_EVIL);
                if (!artifact_bias && one_in_(2))
                    artifact_bias = BIAS_LAW;
                else if (!artifact_bias && one_in_(9))
                    artifact_bias = BIAS_PRIESTLY;
            }
            break;
    }
}
static void random_slay(object_type *o_ptr)
{
    if (o_ptr->tval == TV_BOW)
    {
        if (o_ptr->sval == SV_CRIMSON || o_ptr->sval == SV_RAILGUN)
        {
            if (one_in_(2))
            {
                random_plus(o_ptr);
                has_pval = TRUE;
            }
            else
                one_high_resistance(o_ptr);
            return;
        }
        else if (o_ptr->sval == SV_HARP)
        {
            if (one_in_(2))
                random_resistance(o_ptr);
            else
            {
                random_plus(o_ptr);
                has_pval = TRUE;
            }
            return;
        }
        switch (randint1(6))
        {
            case 1:
                if (one_in_(100))
                {
                    random_slay_aux(o_ptr);
                    break;
                }
            case 2:
            case 3:
                add_flag(o_ptr->art_flags, TR_XTRA_MIGHT);
                if (!one_in_(7)) remove_flag(o_ptr->art_flags, TR_XTRA_SHOTS);
                if (!artifact_bias && one_in_(9))
                    artifact_bias = BIAS_RANGER;
                break;
            default:
                add_flag(o_ptr->art_flags, TR_XTRA_SHOTS);
                if (!one_in_(7)) remove_flag(o_ptr->art_flags, TR_XTRA_MIGHT);
                if (!artifact_bias && one_in_(9))
                    artifact_bias = BIAS_RANGER;
                break;
        }
        return;
    }

    switch (artifact_bias)
    {
    case BIAS_CHAOS:
        if (!(have_flag(o_ptr->art_flags, TR_CHAOTIC)))
        {
            add_flag(o_ptr->art_flags, TR_CHAOTIC);
            if (one_in_(2)) return;
        }
        break;

    case BIAS_PRIESTLY:
        if((o_ptr->tval == TV_SWORD || o_ptr->tval == TV_POLEARM) &&
           !(have_flag(o_ptr->art_flags, TR_BLESSED)))
        {
            /* A free power for "priestly" random artifacts */
            add_flag(o_ptr->art_flags, TR_BLESSED);
        }
        break;

    case BIAS_NECROMANTIC:
        if (!(have_flag(o_ptr->art_flags, TR_VAMPIRIC)))
        {
            add_flag(o_ptr->art_flags, TR_VAMPIRIC);
            if (one_in_(2)) return;
        }
        if (!(have_flag(o_ptr->art_flags, TR_BRAND_POIS)) && one_in_(2))
        {
            add_flag(o_ptr->art_flags, TR_BRAND_POIS);
            if (one_in_(2)) return;
        }
        break;

    case BIAS_RANGER:
        if (!(have_flag(o_ptr->art_flags, TR_SLAY_ANIMAL)))
        {
            add_flag(o_ptr->art_flags, TR_SLAY_ANIMAL);
            if (one_in_(2)) return;
        }
        break;

    case BIAS_ROGUE:
        if ((((o_ptr->tval == TV_SWORD) && (o_ptr->sval == SV_DAGGER)) ||
             ((o_ptr->tval == TV_POLEARM) && (o_ptr->sval == SV_SPEAR))) &&
             !(have_flag(o_ptr->art_flags, TR_THROW)))
        {
            /* Free power for rogues... */
            add_flag(o_ptr->art_flags, TR_THROW);
        }
        if (!(have_flag(o_ptr->art_flags, TR_BRAND_POIS)))
        {
            add_flag(o_ptr->art_flags, TR_BRAND_POIS);
            if (one_in_(2)) return;
        }
        break;

    case BIAS_POIS:
        if (!(have_flag(o_ptr->art_flags, TR_BRAND_POIS)))
        {
            add_flag(o_ptr->art_flags, TR_BRAND_POIS);
            if (one_in_(2)) return;
        }
        break;

    case BIAS_FIRE:
        if (!(have_flag(o_ptr->art_flags, TR_BRAND_FIRE)))
        {
            add_flag(o_ptr->art_flags, TR_BRAND_FIRE);
            if (one_in_(2)) return;
        }
        break;

    case BIAS_COLD:
        if (!(have_flag(o_ptr->art_flags, TR_BRAND_COLD)))
        {
            add_flag(o_ptr->art_flags, TR_BRAND_COLD);
            if (one_in_(2)) return;
        }
        break;

    case BIAS_ELEC:
        if (!(have_flag(o_ptr->art_flags, TR_BRAND_ELEC)))
        {
            add_flag(o_ptr->art_flags, TR_BRAND_ELEC);
            if (one_in_(2)) return;
        }
        break;

    case BIAS_ACID:
        if (!(have_flag(o_ptr->art_flags, TR_BRAND_ACID)))
        {
            add_flag(o_ptr->art_flags, TR_BRAND_ACID);
            if (one_in_(2)) return;
        }
        break;

    case BIAS_LAW:
        if (!(have_flag(o_ptr->art_flags, TR_SLAY_EVIL)))
        {
            add_flag(o_ptr->art_flags, TR_SLAY_EVIL);
            if (one_in_(2)) return;
        }
        if (!(have_flag(o_ptr->art_flags, TR_SLAY_UNDEAD)))
        {
            add_flag(o_ptr->art_flags, TR_SLAY_UNDEAD);
            if (one_in_(2)) return;
        }
        if (!(have_flag(o_ptr->art_flags, TR_SLAY_DEMON)))
        {
            add_flag(o_ptr->art_flags, TR_SLAY_DEMON);
            if (one_in_(2)) return;
        }
        break;
    }

    random_slay_aux(o_ptr);
}


static void give_activation_power(object_type *o_ptr)
{
    int type = 0, chance = 0;

    switch (artifact_bias)
    {
        case BIAS_ELEC:
            if (!one_in_(3))
            {
                type = ACT_BO_ELEC_1;
            }
            else if (!one_in_(5))
            {
                type = ACT_BA_ELEC_2;
            }
            else
            {
                type = ACT_BA_ELEC_3;
            }
            chance = 101;
            break;

        case BIAS_POIS:
            type = ACT_BA_POIS_1;
            chance = 101;
            break;

        case BIAS_FIRE:
            if (!one_in_(3))
            {
                type = ACT_BO_FIRE_1;
            }
            else if (!one_in_(5))
            {
                type = ACT_BA_FIRE_1;
            }
            else
            {
                type = ACT_BA_FIRE_2;
            }
            chance = 101;
            break;

        case BIAS_COLD:
            chance = 101;
            if (!one_in_(3))
                type = ACT_BO_COLD_1;
            else if (!one_in_(3))
                type = ACT_BA_COLD_1;
            else if (!one_in_(3))
                type = ACT_BA_COLD_2;
            else
                type = ACT_BA_COLD_3;
            break;

        case BIAS_CHAOS:
            chance = 50;
            if (one_in_(6))
                type = ACT_SUMMON_DEMON;
            else
                type = ACT_CALL_CHAOS;
            break;

        case BIAS_PRIESTLY:
            chance = 101;

            if (one_in_(13))
                type = ACT_CHARM_UNDEAD;
            else if (one_in_(12))
                type = ACT_BANISH_EVIL;
            else if (one_in_(11))
                type = ACT_DISP_EVIL;
            else if (one_in_(10))
                type = ACT_PROT_EVIL;
            else if (one_in_(9))
                type = ACT_CURE_1000;
            else if (one_in_(8))
                type = ACT_CURE_700;
            else if (one_in_(7))
                type = ACT_REST_ALL;
            else if (one_in_(6))
                type = ACT_REST_LIFE;
            else
                type = ACT_CURE_MW;
            break;

        case BIAS_NECROMANTIC:
            chance = 101;
            if (one_in_(66))
                type = ACT_WRAITH;
            else if (one_in_(13))
                type = ACT_DISP_GOOD;
            else if (one_in_(9))
                type = ACT_MASS_GENO;
            else if (one_in_(8))
                type = ACT_GENOCIDE;
            else if (one_in_(13))
                type = ACT_SUMMON_UNDEAD;
            else if (one_in_(9))
                type = ACT_VAMPIRE_2;
            else if (one_in_(6))
                type = ACT_CHARM_UNDEAD;
            else
                type = ACT_VAMPIRE_1;
            break;

        case BIAS_LAW:
            chance = 101;
            if (one_in_(8))
                type = ACT_BANISH_EVIL;
            else if (one_in_(4))
                type = ACT_DISP_EVIL;
            else
                type = ACT_PROT_EVIL;
            break;

        case BIAS_ROGUE:
            chance = 101;
            if (one_in_(50))
                type = ACT_SPEED;
            else if (one_in_(4))
                type = ACT_SLEEP;
            else if (one_in_(3))
                type = ACT_DETECT_ALL;
            else if (one_in_(8))
                type = ACT_ID_FULL;
            else
                type = ACT_ID_PLAIN;
            break;

        case BIAS_MAGE:
            chance = 66;
            if (one_in_(20))
                type = ACT_SUMMON_ELEMENTAL;
            else if (one_in_(10))
                type = ACT_SUMMON_PHANTOM;
            else if (one_in_(5))
                type = ACT_RUNE_EXPLO;
            else
                type = ACT_ESP;
            break;

        case BIAS_WARRIOR:
            chance = 80;
            if (one_in_(100))
                type = ACT_INVULN;
            else
                type = ACT_BERSERK;
            break;

        case BIAS_RANGER:
            chance = 101;
            if (one_in_(20))
                type = ACT_CHARM_ANIMALS;
            else if (one_in_(7))
                type = ACT_SUMMON_ANIMAL;
            else if (one_in_(6))
                type = ACT_CHARM_ANIMAL;
            else if (one_in_(4))
                type = ACT_RESIST_ALL;
            else if (one_in_(3))
                type = ACT_SATIATE;
            else
                type = ACT_CURE_POISON;
            break;
    }

    while (!type || (randint1(100) >= chance))
    {
        type = randint1(255);
        switch (type)
        {
            case ACT_SUNLIGHT:
            case ACT_BO_MISS_1:
            case ACT_BA_POIS_1:
            case ACT_BO_ELEC_1:
            case ACT_BO_ACID_1:
            case ACT_BO_COLD_1:
            case ACT_BO_FIRE_1:
            case ACT_CONFUSE:
            case ACT_SLEEP:
            case ACT_QUAKE:
            case ACT_CURE_LW:
            case ACT_CURE_MW:
            case ACT_CURE_POISON:
            case ACT_BERSERK:
            case ACT_LIGHT:
            case ACT_MAP_LIGHT:
            case ACT_DEST_DOOR:
            case ACT_STONE_MUD:
            case ACT_TELEPORT:
            case ACT_WIZ_LITE:
                chance = 101;
                break;
            case ACT_BA_COLD_1:
            case ACT_BA_FIRE_1:
            case ACT_DRAIN_1:
            case ACT_TELE_AWAY:
            case ACT_ESP:
            case ACT_RESIST_ALL:
            case ACT_DETECT_ALL:
            case ACT_RECALL:
            case ACT_SATIATE:
            case ACT_RECHARGE:
                chance = 85;
                break;
            case ACT_TERROR:
            case ACT_PROT_EVIL:
            case ACT_ID_PLAIN:
                chance = 75;
                break;
            case ACT_DRAIN_2:
            case ACT_VAMPIRE_1:
            case ACT_BO_MISS_2:
            case ACT_BA_FIRE_2:
            case ACT_REST_LIFE:
                chance = 66;
                break;
            case ACT_BA_COLD_3:
            case ACT_BA_ELEC_3:
            case ACT_WHIRLWIND:
            case ACT_VAMPIRE_2:
            case ACT_CHARM_ANIMAL:
                chance = 50;
                break;
            case ACT_SUMMON_ANIMAL:
                chance = 40;
                break;
            case ACT_DISP_EVIL:
            case ACT_BA_MISS_3:
            case ACT_DISP_GOOD:
            case ACT_BANISH_EVIL:
            case ACT_GENOCIDE:
            case ACT_MASS_GENO:
            case ACT_CHARM_UNDEAD:
            case ACT_CHARM_OTHER:
            case ACT_SUMMON_PHANTOM:
            case ACT_REST_ALL:
            case ACT_RUNE_EXPLO:
                chance = 33;
                break;
            case ACT_CALL_CHAOS:
            case ACT_ROCKET:
            case ACT_CHARM_ANIMALS:
            case ACT_CHARM_OTHERS:
            case ACT_SUMMON_ELEMENTAL:
            case ACT_CURE_700:
            case ACT_SPEED:
            case ACT_ID_FULL:
            case ACT_RUNE_PROT:
                chance = 25;
                break;
            case ACT_CURE_1000:
            case ACT_XTRA_SPEED:
            case ACT_DETECT_XTRA:
            case ACT_DIM_DOOR:
                chance = 10;
                break;
            case ACT_SUMMON_UNDEAD:
            case ACT_SUMMON_DEMON:
            case ACT_WRAITH:
            case ACT_INVULN:
            case ACT_ALCHEMY:
                chance = 5;
                break;
            default:
                chance = 0;
        }
    }

    /* A type was chosen... */
    o_ptr->xtra2 = type;
    add_flag(o_ptr->art_flags, TR_ACTIVATE);
    o_ptr->timeout = 0;
}

static void get_random_name_aux(char *return_name, object_type *o_ptr, int power)
{    
    if (o_ptr->tval == TV_LITE)
    {
        cptr filename;
        switch (power)
        {
        case 0:
            filename = "lite_cursed.txt";
            break;
        case 1:
            filename = "lite_low.txt";
            break;
        case 2:
            filename = "lite_med.txt";
            break;
        default:
            filename = "lite_high.txt";
        }
        get_rnd_line(filename, artifact_bias, return_name);
    }
    else if (o_ptr->tval == TV_RING)
    {
        cptr filename;
        switch (power)
        {
        case 0:
            filename = "ring_cursed.txt";
            break;
        case 1:
            filename = "ring_low.txt";
            break;
        case 2:
            filename = "ring_med.txt";
            break;
        default:
            filename = "ring_high.txt";
        }
        get_rnd_line(filename, artifact_bias, return_name);
    }
    else if (o_ptr->tval == TV_AMULET)
    {
        cptr filename;
        switch (power)
        {
        case 0:
            filename = "amulet_cursed.txt";
            break;
        case 1:
            filename = "amulet_low.txt";
            break;
        case 2:
            filename = "amulet_med.txt";
            break;
        default:
            filename = "amulet_high.txt";
        }
        get_rnd_line(filename, artifact_bias, return_name);
    }
    else
    {
        cptr filename;

        if (object_is_armour(o_ptr))
        {
            switch (power)
            {
            case 0:
                filename = "a_cursed.txt";
                break;
            case 1:
            case 2:
                filename = "a_med.txt";
                break;
            default:
                filename = "a_high.txt";
            }
        }
        else
        {
            switch (power)
            {
            case 0:
                filename = "w_cursed.txt";
                break;
            case 1:
            case 2:
                filename = "w_med.txt";
                break;
            default:
                filename = "w_high.txt";
            }
        }

        get_rnd_line(filename, artifact_bias, return_name);
    }
}

void get_random_name(char *return_name, object_type *o_ptr, int power)
{
    int attempt;
    return_name[0] = '\0';
    for (attempt = 0;; ++attempt)
    {
        get_random_name_aux(return_name, o_ptr, power);
        if (return_name[0] != '\0') break;
        if (attempt > 100) break;
    }
}

s32b create_artifact(object_type *o_ptr, u32b mode)
{
    char    new_name[1024];
    int     powers = 0;
    int     power_level;
    s32b    total_flags;
    bool    a_cursed = FALSE;
    int     warrior_artifact_bias = 0;
    int     i;
    bool    has_resistance = FALSE, boosted_ac = FALSE, boosted_dam = FALSE, boosted_hit = FALSE;
    int     lev = object_level;
    bool    is_falcon_sword = FALSE;
    int     max_a = MAX(o_ptr->to_a, 30);
    int     max_h = MAX(o_ptr->to_h, 32);
    int     max_d = MAX(o_ptr->to_d, 32);

    if (no_artifacts) return 0;
    
    if (o_ptr->tval == TV_SWORD && o_ptr->sval == SV_HAYABUSA)
        is_falcon_sword = TRUE;

    immunity_hack = FALSE;
    slaying_hack = 0;
    has_pval = FALSE;

    if (o_ptr->tval == TV_BOW && o_ptr->sval == SV_HARP)
        has_pval = TRUE;

    /* Hack for Demeter. Torches start with a pval of 4000! Not sure about lanterns ... */
    if (o_ptr->tval == TV_LITE /*&& o_ptr->sval == SV_LITE_TORCH*/)
        o_ptr->pval = 0;

    if (lev > 127)
        lev = 127; /* no going to heaven or hell for uber nutso craziness */

    /* Reset artifact bias */
    artifact_bias = 0;

    /* Nuke enchantments */
    o_ptr->name1 = 0;
    o_ptr->name2 = 0;
    o_ptr->name3 = 0;

    for (i = 0; i < TR_FLAG_SIZE; i++)
        o_ptr->art_flags[i] |= k_info[o_ptr->k_idx].flags[i];

    if (o_ptr->pval) has_pval = TRUE;

    if ((mode & (CREATE_ART_SCROLL/* | CREATE_ART_GOOD*/)) && one_in_(4))
    {
        switch (p_ptr->pclass)
        {
            case CLASS_WARRIOR:
            case CLASS_BERSERKER:
            case CLASS_ARCHER:
            case CLASS_SAMURAI:
            case CLASS_CAVALRY:
            case CLASS_WEAPONSMITH:
            case CLASS_DUELIST:
            case CLASS_BLOOD_KNIGHT:
            case CLASS_WEAPONMASTER:
            case CLASS_RUNE_KNIGHT:
            case CLASS_MAULER:
                artifact_bias = BIAS_WARRIOR;
                break;
            case CLASS_MAGE:
            case CLASS_HIGH_MAGE:
            case CLASS_SORCERER:
            case CLASS_MAGIC_EATER:
            case CLASS_BLUE_MAGE:
            case CLASS_WARLOCK:
            case CLASS_BLOOD_MAGE:
            case CLASS_NECROMANCER:
            case CLASS_DEVICEMASTER:
                artifact_bias = BIAS_MAGE;
                warrior_artifact_bias = 20;
                break;
            case CLASS_PRIEST:
                artifact_bias = BIAS_PRIESTLY;
                warrior_artifact_bias = 30;
                break;
            case CLASS_ROGUE:
            case CLASS_NINJA:
            case CLASS_SCOUT:
                artifact_bias = BIAS_ROGUE;
                warrior_artifact_bias = 30;
                break;
            case CLASS_RANGER:
            case CLASS_SNIPER:
                artifact_bias = BIAS_RANGER;
                warrior_artifact_bias = 30;
                break;
            case CLASS_PALADIN:
                artifact_bias = BIAS_PRIESTLY;
                warrior_artifact_bias = 60;
                break;
            case CLASS_WARRIOR_MAGE:
            case CLASS_RED_MAGE:
            case CLASS_PSION:
                artifact_bias = BIAS_MAGE;
                warrior_artifact_bias = 20;
                break;
            case CLASS_CHAOS_WARRIOR:
                artifact_bias = BIAS_CHAOS;
                warrior_artifact_bias = 60;
                break;
            case CLASS_MONK:
            case CLASS_FORCETRAINER:
                artifact_bias = BIAS_PRIESTLY;
                warrior_artifact_bias = 40;
                break;
            case CLASS_MINDCRAFTER:
                artifact_bias = BIAS_PRIESTLY;
                warrior_artifact_bias = 20;
                break;
            case CLASS_BARD:
                artifact_bias = BIAS_PRIESTLY;
                warrior_artifact_bias = 20;
                break;
            case CLASS_TOURIST:
            case CLASS_ARCHAEOLOGIST:
            case CLASS_TIME_LORD:
                warrior_artifact_bias = 20;
                break;
            case CLASS_IMITATOR:
                artifact_bias = BIAS_RANGER;
                warrior_artifact_bias = 40;
                break;
            case CLASS_BEASTMASTER:
                artifact_bias = BIAS_CHR;
                warrior_artifact_bias = 30;
                break;
            case CLASS_MIRROR_MASTER:
                if (randint1(4) > 1) 
                {
                    artifact_bias = BIAS_MAGE;
                }
                else
                {
                    artifact_bias = BIAS_ROGUE;
                }
                warrior_artifact_bias = 20;
                break;

            default:
                artifact_bias = BIAS_WARRIOR;
                break;
        }
    }

    if ((mode & (CREATE_ART_SCROLL | CREATE_ART_GOOD)) && (randint1(100) <= warrior_artifact_bias))
        artifact_bias = BIAS_WARRIOR;

    strcpy(new_name, "");

    if (!(mode & CREATE_ART_GOOD) && one_in_(A_CURSED))
        a_cursed = TRUE;

    if (mode & CREATE_ART_CURSED)
        a_cursed = TRUE;

    if (((o_ptr->tval == TV_AMULET) || (o_ptr->tval == TV_RING)) && object_is_cursed(o_ptr))
        a_cursed = TRUE;

    powers = randint1(5) + 1;

    while (one_in_(powers) || one_in_(7 * 90/MAX(object_level, 1)) || one_in_(10 * 70/MAX(object_level, 1)))
        powers++;

    if (!a_cursed)
    {
        if (one_in_(WEIRD_LUCK))
            powers *= 2;
    }

    if (a_cursed) powers /= 2;

    if ( (o_ptr->tval == TV_LITE && o_ptr->sval != SV_LITE_JUDGE) 
      || o_ptr->tval == TV_AMULET
      || (o_ptr->tval == TV_RING && o_ptr->sval != SV_RING_NARYA 
                                 && o_ptr->sval != SV_RING_NENYA
                                 && o_ptr->sval != SV_RING_VILYA
                                 && o_ptr->sval != SV_RING_POWER ) ) 
    {
        if (!one_in_(WEIRD_LUCK))
        {
            if (powers > 3) powers = powers*3/4;
            if (powers > 5) powers = 5;

            /* Artifacting high rings of damage is now possible ... */
            if (o_ptr->to_d)
                o_ptr->to_d = randint1((o_ptr->to_d + 1)/2) + randint1(o_ptr->to_d/2);
            if (o_ptr->to_a)
                o_ptr->to_a = randint1((o_ptr->to_a + 1)/2) + randint1(o_ptr->to_a/2);
        }
    }

    /* Playtesting shows that FA, SI and HL are too rare ... let's boost these a bit */
    switch (o_ptr->tval)
    {
    case TV_BOLT:
    case TV_SHOT:
    case TV_ARROW:
        o_ptr->number = 1;    /* Where's the best place for this? */
        break;

    case TV_RING:
        if (one_in_(2))
            add_flag(o_ptr->art_flags, TR_FREE_ACT);
        if (one_in_(2))
            add_flag(o_ptr->art_flags, TR_SEE_INVIS);
        break;

    case TV_AMULET:
        if (one_in_(3))
            add_flag(o_ptr->art_flags, TR_FREE_ACT);
        if (one_in_(3))
            add_flag(o_ptr->art_flags, TR_HOLD_LIFE);
        break;

        
    case TV_LITE:
        if (one_in_(3))
            add_flag(o_ptr->art_flags, TR_HOLD_LIFE);
        if (one_in_(10))
            add_flag(o_ptr->art_flags, TR_DARKNESS);
        break;

    case TV_BOOTS:
        if (one_in_(4))
            add_flag(o_ptr->art_flags, TR_FREE_ACT);
        if (one_in_(7))
            add_flag(o_ptr->art_flags, TR_LEVITATION);
        break;

    case TV_GLOVES:
        if (one_in_(4))
            add_flag(o_ptr->art_flags, TR_FREE_ACT);
        break;

    case TV_HELM:
        if (one_in_(4))
            add_flag(o_ptr->art_flags, TR_SEE_INVIS);
        break;
    }

    /* Main loop */
    while (powers > 0)
    {
        powers--;
        /* Attempt to craft artifacts by appropriate to type.  For example, arrows should
           not grant extra blows or +3 strength :)  More realistically, weapons should
           preferentially get slays while armor should favor resists */
        switch (o_ptr->tval)
        {
        case TV_BOLT:
        case TV_SHOT:
        case TV_ARROW:
            /* Ammo either boosts up damage dice, or gains slays */
            if ( randint1(225) < lev
              && slaying_hack == 0 ) /* OK: Slaying can now only happen once! */
            {
                if (slaying_hack == 0 && one_in_(3)) /* double damage */
                {
                    powers -= o_ptr->dd - 1;
                    o_ptr->dd *= 2;
                    slaying_hack += o_ptr->dd;
                }
                else
                {
                    powers++;
                    powers++;
                    do
                    {
                        o_ptr->dd++;
                        powers--;
                        slaying_hack++;
                    }
                    while (one_in_(o_ptr->dd));
                        
                    do
                    {
                        o_ptr->ds++;
                        powers--;
                        slaying_hack++;
                    }
                    while (one_in_(o_ptr->ds));
                }            
            }
            else
                random_slay(o_ptr);
            break;

        case TV_LITE:
            switch (randint1(7))
            {
            case 1: case 2:
                random_plus(o_ptr);
                has_pval = TRUE;
                break;
            case 3:
                if (one_in_(45))
                {
                    add_flag(o_ptr->art_flags, TR_TELEPATHY);
                }
                else if (one_in_(15))
                {
                    o_ptr->xtra2 = ACT_WIZ_LITE;
                    add_flag(o_ptr->art_flags, TR_ACTIVATE);
                    o_ptr->timeout = 0;
                }
                else if (one_in_(77))
                {
                    add_flag(o_ptr->art_flags, TR_SPELL_POWER);
                    add_flag(o_ptr->art_flags, TR_STR);
                    add_flag(o_ptr->art_flags, TR_DEX);
                    add_flag(o_ptr->art_flags, TR_CON);
                    has_pval = TRUE;
                }
                else
                {
                    random_plus(o_ptr);
                    has_pval = TRUE;
                }
                break;
            case 4: case 5: case 6:
                if (one_in_(3))
                    one_high_resistance(o_ptr);
                else
                    random_resistance(o_ptr);
                break;
            case 7:
                random_misc(o_ptr);
                break;
            }        
            break;

        case TV_SWORD:
        case TV_HAFTED:
        case TV_POLEARM:
        case TV_DIGGING:
            switch (randint1(7))
            {
            case 1: case 2:
                random_plus(o_ptr);
                has_pval = TRUE;
                break;
            case 3:
                if ( slaying_hack == 0 /* OK: Slaying can now only happen once! */
                    && (is_falcon_sword || !have_flag(o_ptr->art_flags, TR_BLOWS))
                    && randint1(225) < lev)
                {
                    int odds = o_ptr->dd * o_ptr->ds / 2;
                    if (odds < 3) odds = 3;
                    if (a_cursed && !one_in_(13)) break;
                    /* spiked code from EGO_SLAYING_WEAPON */
                    if (slaying_hack == 0 && one_in_(odds)) /* double damage */
                    {
                        powers -= o_ptr->dd - 1;
                        slaying_hack += o_ptr->dd;
                        o_ptr->dd *= 2;
                    }
                    else
                    {
                        powers++;
                        powers++;
                        do
                        {
                            o_ptr->dd++;
                            powers--;
                            slaying_hack++;
                        }
                        while (one_in_(o_ptr->dd));
                        
                        do
                        {
                            o_ptr->ds++;
                            powers--;
                            slaying_hack++;
                        }
                        while (one_in_(o_ptr->ds));
                    }            
                }
                else if (!boosted_dam && !boosted_hit && randint1(225) < lev)
                {
                    if (one_in_(7))
                    {
                        o_ptr->to_h = 10 + randint1(22);
                        o_ptr->to_d = 10 + randint1(22);
                        boosted_dam = TRUE;
                        boosted_hit = TRUE;
                        powers--;
                    }
                    else if (one_in_(2))
                    {
                        o_ptr->to_h = 10 + randint1(22);
                        boosted_hit = TRUE;
                    }
                    else
                    {
                        o_ptr->to_d = 10 + randint1(22);
                        boosted_dam = TRUE;
                    }
                }
                else
                    random_resistance(o_ptr);
                break;
            case 4:
                random_misc(o_ptr);
                break;
            case 5: case 6: case 7: 
                random_slay(o_ptr);
                break;
            }
            break;

        case TV_BOW:
            switch (randint1(8))
            {
            case 1: case 2:
                random_plus(o_ptr);
                has_pval = TRUE;
                break;
            case 3: case 4: case 5:
                random_slay(o_ptr);
                break;
            case 6:
                random_misc(o_ptr);
                break;
            case 7: case 8:
                random_resistance(o_ptr);
                break;
            }
            break;

        case TV_RING:
            switch (randint1(7))
            {
            case 1: case 2:
                random_plus(o_ptr);
                has_pval = TRUE;
                break;
            case 3:
                if (one_in_(10))
                {
                    add_flag(o_ptr->art_flags, TR_SPEED);
                    has_pval = TRUE;
                }
                else if (one_in_(100))
                {
                    add_flag(o_ptr->art_flags, TR_WEAPONMASTERY);
                    has_pval = TRUE;
                }
                else if (one_in_(20) && randint1(150) < lev - 50)
                {
                    add_flag(o_ptr->art_flags, TR_BLOWS);
                    has_pval = TRUE;
                }
                else if (one_in_(15))
                {
                    add_flag(o_ptr->art_flags, TR_SUST_STR);
                    add_flag(o_ptr->art_flags, TR_SUST_INT);
                    add_flag(o_ptr->art_flags, TR_SUST_WIS);
                    add_flag(o_ptr->art_flags, TR_SUST_DEX);
                    add_flag(o_ptr->art_flags, TR_SUST_CON);
                    add_flag(o_ptr->art_flags, TR_SUST_CHR);
                    add_flag(o_ptr->art_flags, TR_HOLD_LIFE);
                    powers--;
                }
                else if (one_in_(77))
                {
                    add_flag(o_ptr->art_flags, TR_SPELL_POWER);
                    add_flag(o_ptr->art_flags, TR_STR);
                    add_flag(o_ptr->art_flags, TR_DEX);
                    add_flag(o_ptr->art_flags, TR_CON);
                    has_pval = TRUE;
                }
                else if (one_in_(2))
                {
                    add_flag(o_ptr->art_flags, TR_SHOW_MODS);
                    o_ptr->to_h = 4 + (randint1(11));
                    o_ptr->to_d = 4 + (randint1(11));
                }
                else
                {
                    one_high_resistance(o_ptr);
                }
                break;
            case 4: case 5: case 6:
                random_resistance(o_ptr);
                break;
            case 7:
                random_misc(o_ptr);
                break;
            }
            break;

        case TV_AMULET:
            switch (randint1(7))
            {
            case 1: case 2:
                random_plus(o_ptr);
                has_pval = TRUE;
                break;
            case 3:
                if (one_in_(45))
                {
                    add_flag(o_ptr->art_flags, TR_TELEPATHY);
                }
                else if (artifact_bias == BIAS_MAGE && one_in_(10))
                {
                    add_flag(o_ptr->art_flags, TR_DEC_MANA);
                }
                else if (one_in_(7))
                {
                    add_flag(o_ptr->art_flags, TR_REFLECT);
                }
                else if (one_in_(77))
                {
                    add_flag(o_ptr->art_flags, TR_SPELL_POWER);
                    add_flag(o_ptr->art_flags, TR_STR);
                    add_flag(o_ptr->art_flags, TR_DEX);
                    add_flag(o_ptr->art_flags, TR_CON);
                    has_pval = TRUE;
                }
                else if (one_in_(2))
                {
                    add_flag(o_ptr->art_flags, TR_SHOW_MODS);
                    o_ptr->to_h = 4 + (randint1(11));
                    o_ptr->to_d = 4 + (randint1(11));
                }
                else
                {
                    one_high_resistance(o_ptr);
                }
                break;
            case 4: case 5: case 6:
                random_resistance(o_ptr);
                break;
            case 7:
                random_misc(o_ptr);
                break;
            }
            break;

        default: /* Assume Armor */
            switch (randint1(7))
            {
                case 1: case 2:
                    random_plus(o_ptr);
                    has_pval = TRUE;
                    break;
                case 3:
                    if (!has_resistance 
                      && (object_is_body_armour(o_ptr) || object_is_shield(o_ptr)) 
                      && one_in_(4) )
                    {
                        add_flag(o_ptr->art_flags, TR_RES_ACID);
                        add_flag(o_ptr->art_flags, TR_RES_COLD);
                        add_flag(o_ptr->art_flags, TR_RES_FIRE);
                        add_flag(o_ptr->art_flags, TR_RES_ELEC);
                        if (one_in_(3))
                            add_flag(o_ptr->art_flags, TR_RES_POIS);
                        has_resistance = TRUE;
                        powers -= 3;
                    }
                    else if (o_ptr->tval == TV_BOOTS && one_in_(3))
                    {
                        add_flag(o_ptr->art_flags, TR_SPEED);
                        has_pval = TRUE;
                    }
                    else if (o_ptr->tval == TV_BOOTS && one_in_(2))
                    {
                        add_flag(o_ptr->art_flags, TR_LEVITATION);
                    }
                    else if (!boosted_ac && randint1(225) < lev)
                    {
                        o_ptr->to_a = 20 + randint1(20);
                        if (object_is_body_armour(o_ptr) && one_in_(7))
                            o_ptr->ac += 5;

                        boosted_ac = TRUE;
                        powers--;
                    }
                    else if (o_ptr->tval == TV_GLOVES && one_in_(2))
                    {
                        add_flag(o_ptr->art_flags, TR_SHOW_MODS);
                        o_ptr->to_h = 4 + (randint1(11));
                        o_ptr->to_d = 4 + (randint1(11));
                    }
                    else if (o_ptr->tval == TV_GLOVES && one_in_(2))
                    {
                        add_flag(o_ptr->art_flags, TR_DEX);
                        has_pval = TRUE;
                    }
                    else
                        one_high_resistance(o_ptr);
                    break;
                case 4:
                    if (!(object_is_body_armour(o_ptr) || object_is_shield(o_ptr))
                      || one_in_(3) )
                    {
                        random_misc(o_ptr);
                        break;
                    }
                case 5: case 6: case 7: 
                    random_resistance(o_ptr);                    
                    break;
            }
        }
    };

    if (has_pval)
    {
        if (have_flag(o_ptr->art_flags, TR_BLOWS))
        {
            if (o_ptr->tval == TV_RING)
            {
                o_ptr->pval = 1;
                if (one_in_(30)) o_ptr->pval++;
            }
            else
            {
                o_ptr->pval = 1;
                if (one_in_(15)) o_ptr->pval++;
                if (is_falcon_sword)
                    o_ptr->pval++;
            }
        }
        else
        {
            /* Hengband:  1: 0.0%  2:20.0%  3:32.0%  4+:48.0%  
               Chengband: 1:16.7%  2:28.6%  3:28.6%  4+:26.0% 
               4+ usually becomes a 4. The only tweak I made was changing
               "|| one_in_(pval)" to "|| one_in_(pval+5)."
            */
            if (mode & CREATE_ART_SCROLL)
                o_ptr->pval = 0; /* Surprise! Always re-roll the pval ... */
            do
            {
                o_ptr->pval++;
            }
            while (o_ptr->pval < randint1(5) || one_in_(o_ptr->pval+5));
        }
    }

    if ((o_ptr->pval > 4) && !one_in_(WEIRD_LUCK))
        o_ptr->pval = 4;

    if (have_flag(o_ptr->art_flags, TR_SPELL_POWER))
        o_ptr->pval = -o_ptr->pval;

    /* give it some plusses... */
    if (object_is_armour(o_ptr))
    {
        if (!boosted_ac)
            o_ptr->to_a += randint1(o_ptr->to_a > 19 ? 1 : 20 - o_ptr->to_a);

        if (o_ptr->to_a > max_a)
            o_ptr->to_a = max_a;
    }
    else if (object_is_weapon_ammo(o_ptr))
    {
        if (!boosted_dam)
            o_ptr->to_d += randint1(o_ptr->to_d > 19 ? 1 : 20 - o_ptr->to_d);

        if (!boosted_hit)
            o_ptr->to_h += randint1(o_ptr->to_h > 19 ? 1 : 20 - o_ptr->to_h);

        if (o_ptr->to_h > max_h)
            o_ptr->to_h = max_h;

        if (o_ptr->to_d > max_d)
            o_ptr->to_d = max_d;

        if ((have_flag(o_ptr->art_flags, TR_WIS)) && (o_ptr->pval > 0)) add_flag(o_ptr->art_flags, TR_BLESSED);
    }

    /* Fix up Damage Dice for (Wild) and (Order) weapons */
    if (have_flag(o_ptr->art_flags, TR_ORDER))
    {
        remove_flag(o_ptr->art_flags, TR_WILD);
        o_ptr->dd = o_ptr->dd * o_ptr->ds;
        o_ptr->ds = 1;
    }
    else if (have_flag(o_ptr->art_flags, TR_WILD))
    {
        o_ptr->ds = o_ptr->dd * o_ptr->ds;
        o_ptr->dd = 1;
    }

    /* Just to be sure */
    add_flag(o_ptr->art_flags, TR_IGNORE_ACID);
    add_flag(o_ptr->art_flags, TR_IGNORE_ELEC);
    add_flag(o_ptr->art_flags, TR_IGNORE_FIRE);
    add_flag(o_ptr->art_flags, TR_IGNORE_COLD);

    if (a_cursed) curse_artifact(o_ptr);

    if (!a_cursed &&
        !have_flag(o_ptr->art_flags, TR_ACTIVATE))
    {
        int odds = object_is_armour(o_ptr) ? ACTIVATION_CHANCE * 2 : ACTIVATION_CHANCE;
        if (one_in_(odds))
        {
            o_ptr->xtra2 = 0;
            give_activation_power(o_ptr);
        }
    }

    if (object_is_armour(o_ptr) || o_ptr->tval == TV_RING || o_ptr->tval == TV_AMULET)
    {
        int lower = 10;

        if (o_ptr->tval == TV_RING)
            lower = 15;

        while ((o_ptr->to_d+o_ptr->to_h) > 20)
        {
            if (one_in_(o_ptr->to_d) && one_in_(o_ptr->to_h)) break;
            o_ptr->to_d -= (s16b)randint0(3);
            o_ptr->to_h -= (s16b)randint0(3);
        }
        while ((o_ptr->to_d+o_ptr->to_h) > lower && !immunity_hack)
        {
            if (one_in_(o_ptr->to_d) || one_in_(o_ptr->to_h)) break;
            o_ptr->to_d -= (s16b)randint0(3);
            o_ptr->to_h -= (s16b)randint0(3);
        }
    }

    if ((artifact_bias == BIAS_MAGE || artifact_bias == BIAS_INT) 
      && (o_ptr->tval == TV_GLOVES)) 
    {
        add_flag(o_ptr->art_flags, TR_FREE_ACT);
    }

    if (o_ptr->tval == TV_BOW && o_ptr->sval == SV_HARP)
    {
        o_ptr->to_h = 0;
        o_ptr->to_d = 0;
        remove_flag(o_ptr->art_flags, TR_SHOW_MODS);
    }

    if ((o_ptr->tval == TV_SWORD) && (o_ptr->sval == SV_DOKUBARI))
    {
        o_ptr->to_h = 0;
        o_ptr->to_d = 0;
        remove_flag(o_ptr->art_flags, TR_BLOWS);
        remove_flag(o_ptr->art_flags, TR_FORCE_WEAPON);
        remove_flag(o_ptr->art_flags, TR_SLAY_ANIMAL);
        remove_flag(o_ptr->art_flags, TR_SLAY_EVIL);
        remove_flag(o_ptr->art_flags, TR_SLAY_UNDEAD);
        remove_flag(o_ptr->art_flags, TR_SLAY_DEMON);
        remove_flag(o_ptr->art_flags, TR_SLAY_ORC);
        remove_flag(o_ptr->art_flags, TR_SLAY_TROLL);
        remove_flag(o_ptr->art_flags, TR_SLAY_GIANT);
        remove_flag(o_ptr->art_flags, TR_SLAY_DRAGON);
        remove_flag(o_ptr->art_flags, TR_KILL_DRAGON);
        remove_flag(o_ptr->art_flags, TR_SLAY_HUMAN);
        remove_flag(o_ptr->art_flags, TR_VORPAL);
        remove_flag(o_ptr->art_flags, TR_BRAND_POIS);
        remove_flag(o_ptr->art_flags, TR_BRAND_ACID);
        remove_flag(o_ptr->art_flags, TR_BRAND_ELEC);
        remove_flag(o_ptr->art_flags, TR_BRAND_FIRE);
        remove_flag(o_ptr->art_flags, TR_BRAND_COLD);
    }

    /* Hack: Lights won't value at all unless they are artifacts and the
       only way an object is marked as a rand-art is via the art_name field. */
    if (o_ptr->tval == TV_LITE)
        o_ptr->art_name = quark_add("Temp");

    total_flags = new_object_cost(o_ptr);
    if (cheat_peek) msg_format("Score: %d", total_flags);

    if (object_is_jewelry(o_ptr))
    {
        if (a_cursed) power_level = 0;
        else if (total_flags < 15000) power_level = 1;
        else if (total_flags < 60000) power_level = 2;
        else power_level = 3;
    }
    else if (!object_is_weapon_ammo(o_ptr))
    {
        /* For armors */
        if (a_cursed) power_level = 0;
        else if (total_flags < 30000) power_level = 1;
        else if (total_flags < 70000) power_level = 2;
        else 
        {
            power_level = 3;
            if (one_in_(17)) add_flag(o_ptr->art_flags, TR_AGGRAVATE);
            if (one_in_(5)) add_flag(o_ptr->art_flags, TR_HOLD_LIFE);
        }
    }
    else
    {
        /* For weapons */
        if (a_cursed) power_level = 0;
        else if (total_flags <  50000) power_level = 1;
        else if (total_flags < 100000) power_level = 2;
        else 
        {
            power_level = 3;
            if (one_in_(17)) add_flag(o_ptr->art_flags, TR_AGGRAVATE);
            if (one_in_(5)) add_flag(o_ptr->art_flags, TR_FREE_ACT);
            if (one_in_(5)) add_flag(o_ptr->art_flags, TR_SEE_INVIS);
        }
    }

    if (mode & CREATE_ART_SCROLL)
    {
        char dummy_name[80] = "";
        cptr ask_msg = "What do you want to call the artifact? ";

        /* Identify it fully */
        object_aware(o_ptr);
        object_known(o_ptr);

        /* Mark the item as fully known */
        o_ptr->ident |= (IDENT_MENTAL);

        (void)screen_object(o_ptr, 0L);

        if (!get_string(ask_msg, dummy_name, sizeof dummy_name)
            || !dummy_name[0])
        {
            get_random_name(new_name, o_ptr, power_level);
        }
        else
            sprintf(new_name, "'%s'", dummy_name);

        virtue_add(VIRTUE_INDIVIDUALISM, 2);
        virtue_add(VIRTUE_ENCHANTMENT, 5);
    }
    else
    {
        get_random_name(new_name, o_ptr, power_level);
    }

    if (cheat_xtra)
    {
        if (artifact_bias) msg_format("Biased artifact: %d.", artifact_bias);
        else msg_print("No bias in artifact.");
    }

    /* Save the inscription */
    o_ptr->art_name = quark_add(new_name);

    /* Window stuff */
    p_ptr->window |= (PW_INVEN | PW_EQUIP);

    return total_flags;
}


bool activate_random_artifact(object_type * o_ptr)
{
    int plev = p_ptr->lev;
    int k, dir, dummy = 0;

    if (!o_ptr->art_name) return FALSE; /* oops? */

    /* Activate for attack */
    switch (o_ptr->xtra2)
    {
        case ACT_SUNLIGHT:
        {
            if (!get_aim_dir(&dir)) return FALSE;
            msg_print("A line of sunlight appears.");

            (void)lite_line(dir);
            o_ptr->timeout = 10;
            break;
        }

        case ACT_BO_MISS_1:
        {
            msg_print("It glows extremely brightly...");

            if (!get_aim_dir(&dir)) return FALSE;
            fire_bolt(GF_MISSILE, dir, device_power(damroll(2, 6)));
            o_ptr->timeout = 2;
            break;
        }

        case ACT_BA_POIS_1:
        {
            msg_print("It throbs deep green...");

            if (!get_aim_dir(&dir)) return FALSE;
            fire_ball(GF_POIS, dir, device_power(12), 3);
            o_ptr->timeout = randint0(4) + 4;
            break;
        }

        case ACT_BO_ELEC_1:
        {
            msg_print("It is covered in sparks...");

            if (!get_aim_dir(&dir)) return FALSE;
            fire_bolt(GF_ELEC, dir, device_power(damroll(4, 8)));
            o_ptr->timeout = randint0(5) + 5;
            break;
        }

        case ACT_BO_ACID_1:
        {
            msg_print("It is covered in acid...");

            if (!get_aim_dir(&dir)) return FALSE;
            fire_bolt(GF_ACID, dir, device_power(damroll(5, 8)));
            o_ptr->timeout = randint0(6) + 6;
            break;
        }

        case ACT_BO_COLD_1:
        {
            msg_print("It is covered in frost...");

            if (!get_aim_dir(&dir)) return FALSE;
            fire_bolt(GF_COLD, dir, device_power(damroll(6, 8)));
            o_ptr->timeout = randint0(7) + 7;
            break;
        }

        case ACT_BO_FIRE_1:
        {
            msg_print("It is covered in fire...");

            if (!get_aim_dir(&dir)) return FALSE;
            fire_bolt(GF_FIRE, dir, device_power(damroll(9, 8)));
            o_ptr->timeout = randint0(8) + 8;
            break;
        }

        case ACT_BA_COLD_1:
        {
            msg_print("It is covered in frost...");

            if (!get_aim_dir(&dir)) return FALSE;
            fire_ball(GF_COLD, dir, device_power(48), 2);
            o_ptr->timeout = 400;
            break;
        }

        case ACT_BA_FIRE_1:
        {
            msg_print("It glows an intense red...");

            if (!get_aim_dir(&dir)) return FALSE;
            fire_ball(GF_FIRE, dir, device_power(72), 2);
            o_ptr->timeout = 400;
            break;
        }

        case ACT_DRAIN_1:
        {
            msg_print("It glows black...");

            if (!get_aim_dir(&dir)) return FALSE;
            if (drain_life(dir, device_power(100)))
                hp_player(device_power(100));
            o_ptr->timeout = randint0(100) + 100;
            break;
        }

        case ACT_BA_COLD_2:
        {
            msg_print("It glows an intense blue...");

            if (!get_aim_dir(&dir)) return FALSE;
            fire_ball(GF_COLD, dir, device_power(100), 2);
            o_ptr->timeout = 300;
            break;
        }

        case ACT_BA_ELEC_2:
        {
            msg_print("It crackles with electricity...");

            if (!get_aim_dir(&dir)) return FALSE;
            fire_ball(GF_ELEC, dir, device_power(100), 3);
            o_ptr->timeout = 500;
            break;
        }

        case ACT_DRAIN_2:
        {
            msg_print("It glows black...");

            if (!get_aim_dir(&dir)) return FALSE;
            if (drain_life(dir, device_power(120)))
                hp_player(device_power(120));
            o_ptr->timeout = 400;
            break;
        }

        case ACT_VAMPIRE_1:
        {
            if (!get_aim_dir(&dir)) return FALSE;
            for (dummy = 0; dummy < 3; dummy++)
            {
                if (drain_life(dir, device_power(50)))
                    hp_player(device_power(50));
            }
            o_ptr->timeout = 400;
            break;
        }

        case ACT_BO_MISS_2:
        {
            msg_print("It grows magical spikes...");

            if (!get_aim_dir(&dir)) return FALSE;
            fire_bolt(GF_ARROW, dir, device_power(150));
            o_ptr->timeout = randint0(90) + 90;
            break;
        }

        case ACT_BA_FIRE_2:
        {
            msg_print("It glows deep red...");

            if (!get_aim_dir(&dir)) return FALSE;
            fire_ball(GF_FIRE, dir, device_power(120), 3);
            o_ptr->timeout = randint0(225) + 225;
            break;
        }

        case ACT_BA_COLD_3:
        {
            msg_print("It glows bright white...");

            if (!get_aim_dir(&dir)) return FALSE;
            fire_ball(GF_COLD, dir, device_power(200), 3);
            o_ptr->timeout = randint0(325) + 325;
            break;
        }

        case ACT_BA_ELEC_3:
        {
            msg_print("It glows deep blue...");

            if (!get_aim_dir(&dir)) return FALSE;
            fire_ball(GF_ELEC, dir, device_power(250), 3);
            o_ptr->timeout = randint0(425) + 425;
            break;
        }

        case ACT_WHIRLWIND:
        {
            {
                int y = 0, x = 0;
                cave_type       *c_ptr;
                monster_type    *m_ptr;

                for (dir = 0; dir <= 9; dir++)
                {
                    y = py + ddy[dir];
                    x = px + ddx[dir];
                    c_ptr = &cave[y][x];

                    /* Get the monster */
                    m_ptr = &m_list[c_ptr->m_idx];

                    /* Hack -- attack monsters */
                    if (c_ptr->m_idx && (m_ptr->ml || cave_have_flag_bold(y, x, FF_PROJECT)))
                        py_attack(y, x, 0);
                }
            }
            o_ptr->timeout = 250;
            break;
        }

        case ACT_VAMPIRE_2:
        {
            if (!get_aim_dir(&dir)) return FALSE;
            for (dummy = 0; dummy < 3; dummy++)
            {
                if (drain_life(dir, device_power(100)))
                    hp_player(device_power(100));
            }

            o_ptr->timeout = 400;
            break;
        }


        case ACT_CALL_CHAOS:
        {
            msg_print("It glows in scintillating colours...");

            call_chaos(100);
            o_ptr->timeout = 350;
            break;
        }

        case ACT_ROCKET:
        {
            if (!get_aim_dir(&dir)) return FALSE;
            msg_print("You launch a rocket!");

            fire_ball(GF_ROCKET, dir, device_power(250 + plev*3), 2);
            o_ptr->timeout = 400;
            break;
        }

        case ACT_DISP_EVIL:
        {
            msg_print("It floods the area with goodness...");

            dispel_evil(device_power(p_ptr->lev * 5));
            o_ptr->timeout = randint0(300) + 300;
            break;
        }

        case ACT_DISP_GOOD:
        {
            msg_print("It floods the area with evil...");

            dispel_good(device_power(p_ptr->lev * 5));
            o_ptr->timeout = randint0(300) + 300;
            break;
        }

        case ACT_BA_MISS_3:
        {
            if (!get_aim_dir(&dir)) return FALSE;
            msg_print("You breathe the elements.");

            fire_ball(GF_MISSILE, dir, device_power(300), 4);
            o_ptr->timeout = 500;
            break;
        }

        /* Activate for other offensive action */

        case ACT_CONFUSE:
        {
            msg_print("It glows in scintillating colours...");

            if (!get_aim_dir(&dir)) return FALSE;
            confuse_monster(dir, device_power(20));
            o_ptr->timeout = 15;
            break;
        }

        case ACT_SLEEP:
        {
            msg_print("It glows deep blue...");

            sleep_monsters_touch();
            o_ptr->timeout = 55;
            break;
        }

        case ACT_QUAKE:
        {
            earthquake(py, px, 10);
            o_ptr->timeout = 50;
            break;
        }

        case ACT_TERROR:
        {
            turn_monsters(device_power(40 + p_ptr->lev));
            o_ptr->timeout = 3 * (p_ptr->lev + 10);
            break;
        }

        case ACT_TELE_AWAY:
        {
            if (!get_aim_dir(&dir)) return FALSE;
            (void)fire_beam(GF_AWAY_ALL, dir, device_power(plev));
            o_ptr->timeout = 200;
            break;
        }

        case ACT_BANISH_EVIL:
        {
            if (banish_evil(device_power(100)))
            {
                msg_print("The power of the artifact banishes evil!");

            }
            o_ptr->timeout = 250 + randint1(250);
            break;
        }

        case ACT_GENOCIDE:
        {
            msg_print("It glows deep blue...");

            (void)symbol_genocide(device_power(200), TRUE);
            o_ptr->timeout = 500;
            break;
        }

        case ACT_MASS_GENO:
        {
            msg_print("It lets out a long, shrill note...");

            (void)mass_genocide(device_power(200), TRUE);
            o_ptr->timeout = 1000;
            break;
        }

        /* Activate for summoning / charming */

        case ACT_CHARM_ANIMAL:
        {
            if (!get_aim_dir(&dir)) return FALSE;
            (void)charm_animal(dir, device_power(plev));
            o_ptr->timeout = 300;
            break;
        }

        case ACT_CHARM_UNDEAD:
        {
            if (!get_aim_dir(&dir)) return FALSE;
            (void)control_one_undead(dir, device_power(plev));
            o_ptr->timeout = 333;
            break;
        }

        case ACT_CHARM_OTHER:
        {
            if (!get_aim_dir(&dir)) return FALSE;
            (void)charm_monster(dir, device_power(plev));
            o_ptr->timeout = 400;
            break;
        }

        case ACT_CHARM_ANIMALS:
        {
            (void)charm_animals(device_power(plev * 2));
            o_ptr->timeout = 500;
            break;
        }

        case ACT_CHARM_OTHERS:
        {
            charm_monsters(device_power(plev * 2));
            o_ptr->timeout = 750;
            break;
        }

        case ACT_SUMMON_ANIMAL:
        {
            (void)summon_specific(-1, py, px, device_power(plev), SUMMON_ANIMAL_RANGER, (PM_ALLOW_GROUP | PM_FORCE_PET));
            o_ptr->timeout = 200 + randint1(300);
            break;
        }

        case ACT_SUMMON_PHANTOM:
        {
            msg_print("You summon a phantasmal servant.");

            (void)summon_specific(-1, py, px, device_power(dun_level), SUMMON_PHANTOM, (PM_ALLOW_GROUP | PM_FORCE_PET));
            o_ptr->timeout = 200 + randint1(200);
            break;
        }

        case ACT_SUMMON_ELEMENTAL:
        {
            bool pet = one_in_(3);
            u32b mode = 0L;

            if (!(pet && (plev < 50))) mode |= PM_ALLOW_GROUP;
            if (pet) mode |= PM_FORCE_PET;
            else mode |= PM_NO_PET;

            if (summon_specific((pet ? -1 : 0), py, px, device_power((plev * 3) / 2), SUMMON_ELEMENTAL, mode))
            {
                msg_print("An elemental materializes...");


                if (pet)
                    msg_print("It seems obedient to you.");

                else
                    msg_print("You fail to control it!");

            }

            o_ptr->timeout = 750;
            break;
        }

        case ACT_SUMMON_DEMON:
        {
            bool pet = one_in_(3);
            u32b mode = 0L;

            if (!(pet && (plev < 50))) mode |= PM_ALLOW_GROUP;
            if (pet) mode |= PM_FORCE_PET;
            else mode |= PM_NO_PET;

            if (summon_specific((pet ? -1 : 0), py, px, device_power((plev * 3) / 2), SUMMON_DEMON, mode))
            {
                msg_print("The area fills with a stench of sulphur and brimstone.");

                if (pet)
                    msg_print("'What is thy bidding... Master?'");

                else
                    msg_print("'NON SERVIAM! Wretch! I shall feast on thy mortal soul!'");

            }

            o_ptr->timeout = 666 + randint1(333);
            break;
        }

        case ACT_SUMMON_UNDEAD:
        {
            bool pet = one_in_(3);
            int type;
            u32b mode = 0L;

            type = (plev > 47 ? SUMMON_HI_UNDEAD : SUMMON_UNDEAD);

            if (!pet || ((plev > 24) && one_in_(3))) mode |= PM_ALLOW_GROUP;
            if (pet) mode |= PM_FORCE_PET;
            else mode |= (PM_ALLOW_UNIQUE | PM_NO_PET);

            if (summon_specific((pet ? -1 : 0), py, px, device_power((plev * 3) / 2), type, mode))
            {
                msg_print("Cold winds begin to blow around you, carrying with them the stench of decay...");

                if (pet)
                    msg_print("Ancient, long-dead forms arise from the ground to serve you!");

                else
                    msg_print("'The dead arise... to punish you for disturbing them!'");

            }

            o_ptr->timeout = 666 + randint1(333);
            break;
        }

        /* Activate for healing */

        case ACT_CURE_LW:
        {
            (void)hp_player(device_power(30));
            o_ptr->timeout = 10;
            break;
        }

        case ACT_CURE_MW:
        {
            msg_print("It radiates deep purple...");

            hp_player(device_power(damroll(4, 8)));
            (void)set_cut((p_ptr->cut / 2) - 50, TRUE);
            o_ptr->timeout = randint0(3) + 3;
            break;
        }

        case ACT_CURE_POISON:
        {
            msg_print("It glows deep blue...");

            (void)set_poisoned(0, TRUE);
            o_ptr->timeout = 5;
            break;
        }

        case ACT_REST_LIFE:
        {
            msg_print("It glows a deep red...");

            restore_level();
            o_ptr->timeout = 450;
            break;
        }

        case ACT_REST_ALL:
        {
            msg_print("It glows a deep green...");

            (void)do_res_stat(A_STR);
            (void)do_res_stat(A_INT);
            (void)do_res_stat(A_WIS);
            (void)do_res_stat(A_DEX);
            (void)do_res_stat(A_CON);
            (void)do_res_stat(A_CHR);
            (void)restore_level();
            o_ptr->timeout = 750;
            break;
        }

        case ACT_CURE_700:
        {
            msg_print("It glows deep blue...");

            msg_print("You feel a warm tingling inside...");

            (void)hp_player(device_power(700));
            (void)set_cut(0, TRUE);
            o_ptr->timeout = 250;
            break;
        }

        case ACT_CURE_1000:
        {
            msg_print("It glows a bright white...");

            msg_print("You feel much better...");

            (void)hp_player(device_power(1000));
            (void)set_cut(0, TRUE);
            o_ptr->timeout = 888;
            break;
        }

        /* Activate for timed effect */

        case ACT_ESP:
        {
            (void)set_tim_esp(device_power(randint1(30) + 25), FALSE);
            o_ptr->timeout = 200;
            break;
        }

        case ACT_BERSERK:
        {
            (void)set_hero(device_power(randint1(50) + 50), FALSE);
            (void)set_blessed(device_power(randint1(50) + 50), FALSE);
            o_ptr->timeout = 100 + randint1(100);
            break;
        }

        case ACT_PROT_EVIL:
        {
            msg_print("It lets out a shrill wail...");

            k = 3 * p_ptr->lev;
            (void)set_protevil(device_power(randint1(25) + k), FALSE);
            o_ptr->timeout = randint0(225) + 225;
            break;
        }

        case ACT_RESIST_ALL:
        {
            msg_print("It glows many colours...");

            (void)set_oppose_acid(device_power(randint1(40) + 40), FALSE);
            (void)set_oppose_elec(device_power(randint1(40) + 40), FALSE);
            (void)set_oppose_fire(device_power(randint1(40) + 40), FALSE);
            (void)set_oppose_cold(device_power(randint1(40) + 40), FALSE);
            (void)set_oppose_pois(device_power(randint1(40) + 40), FALSE);
            o_ptr->timeout = 200;
            break;
        }

        case ACT_SPEED:
        {
            msg_print("It glows bright green...");

            (void)set_fast(device_power(randint1(20) + 20), FALSE);
            o_ptr->timeout = 250;
            break;
        }

        case ACT_XTRA_SPEED:
        {
            msg_print("It glows brightly...");

            (void)set_fast(device_power(randint1(75) + 75), FALSE);
            o_ptr->timeout = randint0(200) + 200;
            break;
        }

        case ACT_WRAITH:
        {
            set_wraith_form(device_power(randint1(plev / 2) + (plev / 2)), FALSE);
            o_ptr->timeout = 1000;
            break;
        }

        case ACT_INVULN:
        {
            (void)set_invuln(device_power(randint1(8) + 8), FALSE);
            o_ptr->timeout = 1000;
            break;
        }

        /* Activate for general purpose effect (detection etc.) */
        case ACT_WIZ_LITE:
            msg_print("It flashes bright red ...");
            virtue_add(VIRTUE_KNOWLEDGE, 1);
            virtue_add(VIRTUE_ENLIGHTENMENT, 1);
            wiz_lite(p_ptr->tim_superstealth > 0);
            detect_traps(DETECT_RAD_DEFAULT, TRUE);
            detect_doors(DETECT_RAD_DEFAULT);
            detect_stairs(DETECT_RAD_DEFAULT);
            o_ptr->timeout = randint0(20) + 20;
            break;

        case ACT_LIGHT:
        {
            msg_print("It wells with clear light...");

            lite_area(device_power(damroll(2, 15)), 3);
            o_ptr->timeout = randint0(10) + 10;
            break;
        }

        case ACT_MAP_LIGHT:
        {
            msg_print("It shines brightly...");

            map_area(DETECT_RAD_MAP);
            lite_area(device_power(damroll(2, 15)), 3);
            o_ptr->timeout = randint0(50) + 50;
            break;
        }

        case ACT_DETECT_ALL:
        {
            msg_print("It glows bright white...");

            msg_print("An image forms in your mind...");

            detect_all(DETECT_RAD_DEFAULT);
            o_ptr->timeout = randint0(55) + 55;
            break;
        }

        case ACT_DETECT_XTRA:
        {
            msg_print("It glows brightly...");

            detect_all(DETECT_RAD_DEFAULT);
            probing();
            identify_fully(NULL);
            o_ptr->timeout = 1000;
            break;
        }

        case ACT_ID_FULL:
        {
            msg_print("It glows yellow...");

            identify_fully(NULL);
            o_ptr->timeout = 750;
            break;
        }

        case ACT_ID_PLAIN:
        {
            if (!ident_spell(NULL)) return FALSE;
            o_ptr->timeout = 10;
            break;
        }

        case ACT_RUNE_EXPLO:
        {
            msg_print("It glows bright red...");

            explosive_rune();
            o_ptr->timeout = 200;
            break;
        }

        case ACT_RUNE_PROT:
        {
            msg_print("It glows light blue...");

            warding_glyph();
            o_ptr->timeout = 400;
            break;
        }

        case ACT_SATIATE:
        {
            (void)set_food(PY_FOOD_MAX - 1);
            o_ptr->timeout = 200;
            break;
        }

        case ACT_DEST_DOOR:
        {
            msg_print("It glows bright red...");

            destroy_doors_touch();
            o_ptr->timeout = 10;
            break;
        }

        case ACT_STONE_MUD:
        {
            msg_print("It pulsates...");

            if (!get_aim_dir(&dir)) return FALSE;
            wall_to_mud(dir);
            o_ptr->timeout = 5;
            break;
        }

        case ACT_RECHARGE:
        {
            recharge(device_power(130));
            o_ptr->timeout = 70;
            break;
        }

        case ACT_ALCHEMY:
        {
            msg_print("It glows bright yellow...");

            (void)alchemy();
            o_ptr->timeout = 500;
            break;
        }

        case ACT_DIM_DOOR:
        {
            msg_print("You open a dimensional gate. Choose a destination.");

            if (!dimension_door(device_power(p_ptr->lev / 2 + 10))) return FALSE;
            o_ptr->timeout = 100;
            break;
        }


        case ACT_TELEPORT:
        {
            msg_print("It twists space around you...");

            teleport_player(100, 0L);
            if (mut_present(MUT_ASTRAL_GUIDE))
                energy_use = 30;
            o_ptr->timeout = 45;
            break;
        }

        case ACT_RECALL:
        {
            msg_print("It glows soft white...");
            if (!word_of_recall()) return FALSE;
            o_ptr->timeout = 200;
            break;
        }

        default:
        {
            msg_format("Unknown activation effect: %d.", o_ptr->xtra2);

            return FALSE;
        }
    }

    return TRUE;
}


void get_bloody_moon_flags(object_type *o_ptr)
{
    int dummy, i;

    for (i = 0; i < TR_FLAG_SIZE; i++)
        o_ptr->art_flags[i] = a_info[ART_BLOOD].flags[i];

    dummy = randint1(2) + randint1(2);
    for (i = 0; i < dummy; i++)
    {
        int flag = randint0(26);
        if (flag >= 20) add_flag(o_ptr->art_flags, TR_KILL_UNDEAD + flag - 20);
        else if (flag == 19) add_flag(o_ptr->art_flags, TR_KILL_ANIMAL);
        else if (flag == 18) add_flag(o_ptr->art_flags, TR_SLAY_HUMAN);
        else add_flag(o_ptr->art_flags, TR_CHAOTIC + flag);
    }

    dummy = randint1(2);
    for (i = 0; i < dummy; i++) one_resistance(o_ptr);

    for (i = 0; i < 2; i++)
    {
        int tmp = randint0(11);
        if (tmp < 6) add_flag(o_ptr->art_flags, TR_STR + tmp);
        else add_flag(o_ptr->art_flags, TR_STEALTH + tmp - 6);
    }
}


void random_artifact_resistance(object_type * o_ptr, artifact_type *a_ptr)
{
    bool give_resistance = FALSE, give_power = FALSE;

    if (o_ptr->name1 == ART_GOTHMOG)
    {
        if (prace_is_(RACE_MON_DEMON))
        {
            o_ptr->dd = 6;
            o_ptr->ds = 6;
            o_ptr->to_h = 22;
            o_ptr->to_d = 25;
        }
        else
        {
            add_flag(o_ptr->art_flags, TR_AGGRAVATE);
            o_ptr->curse_flags |= (TRC_CURSED | TRC_HEAVY_CURSE);
        }
    }

    if (o_ptr->name1 == ART_TWILIGHT)
    {
        if (giant_is_(GIANT_FIRE)) /* Boss reward for Fire Giants */
        {
            o_ptr->to_h = 10;
            o_ptr->to_d = 10;
            o_ptr->to_a = 0;
            add_flag(o_ptr->art_flags, TR_SLAY_EVIL);
            give_resistance = TRUE;
        }
        else
        {
            add_flag(o_ptr->art_flags, TR_AGGRAVATE);
            add_flag(o_ptr->art_flags, TR_TY_CURSE);
            o_ptr->curse_flags |=
                (TRC_CURSED | TRC_HEAVY_CURSE);
            o_ptr->curse_flags |= get_curse(2, o_ptr);
        }
    }

    if (o_ptr->name1 == ART_TERROR) /* Terror Mask is for warriors... */
    {
        if (p_ptr->pclass == CLASS_WARRIOR || 
            p_ptr->pclass == CLASS_CAVALRY || 
            p_ptr->pclass == CLASS_BERSERKER ||
            p_ptr->pclass == CLASS_MAULER )
        {
            give_power = TRUE;
            give_resistance = TRUE;
        }
        else
        {
            add_flag(o_ptr->art_flags, TR_AGGRAVATE);
            add_flag(o_ptr->art_flags, TR_TY_CURSE);
            o_ptr->curse_flags |=
                (TRC_CURSED | TRC_HEAVY_CURSE);
            o_ptr->curse_flags |= get_curse(2, o_ptr);
            return;
        }
    }

    if (o_ptr->name1 == ART_MURAMASA)
    {
        if (p_ptr->pclass != CLASS_SAMURAI && p_ptr->pclass != CLASS_BLOOD_KNIGHT)
        {
            add_flag(o_ptr->art_flags, TR_NO_MAGIC);
            o_ptr->curse_flags |= (TRC_HEAVY_CURSE);
        }
    }

    if (o_ptr->name1 == ART_DR_JONES)
    {
        if (p_ptr->pclass == CLASS_ARCHAEOLOGIST)
        {
            give_power = TRUE;
            give_resistance = TRUE;
            o_ptr->pval += 2;
            add_flag(o_ptr->art_flags, TR_SEARCH);
            add_flag(o_ptr->art_flags, TR_FREE_ACT);
        }
    }

    if (o_ptr->name1 == ART_XIAOLONG)
    {
        if (p_ptr->pclass == CLASS_MONK)
            add_flag(o_ptr->art_flags, TR_BLOWS);
    }

    if (o_ptr->name1 == ART_BLOOD)
    {
        get_bloody_moon_flags(o_ptr);
    }

    if (o_ptr->name1 == ART_HEAVENLY_MAIDEN)
    {
        if (p_ptr->psex != SEX_FEMALE)
        {
            add_flag(o_ptr->art_flags, TR_AGGRAVATE);
        }
    }

    if (a_ptr->gen_flags & (TRG_XTRA_POWER)) give_power = TRUE;
    if (a_ptr->gen_flags & (TRG_XTRA_H_RES)) give_resistance = TRUE;
    if (a_ptr->gen_flags & (TRG_XTRA_RES_OR_POWER))
    {
        /* Give a resistance OR a power */
        if (one_in_(2)) give_resistance = TRUE;
        else give_power = TRUE;
    }

    if (give_power)
    {
        one_ability(o_ptr);
    }

    if (give_resistance)
    {
        one_high_resistance(o_ptr);
    }
}

bool reforge_artifact(object_type *src, object_type *dest)
{
    bool        result = FALSE;
    object_type forge = {0};    
    object_type best = {0}, worst = {0};
    int         base_power, best_power = -10000000, power = 0, worst_power = 10000000;
    int         min_power, max_power;
    int         old_level, i;

    /* Score the Original */
    base_power = object_value_real(src);

    /* Setup thresholds. For weak objects, its better to use a generous range ... */
    if (base_power < 1000)
    {
        min_power = 0;
        max_power = base_power + 5000;
    }
    else
    {
        min_power = 7 * base_power / 10;
        max_power = 3 * base_power / 2;
    }

    /* Better Fame means better results! */
    old_level = object_level;
    object_level = MAX(p_ptr->fame, 100);

    for (i = 0; i < MAX(1, MIN(10000, p_ptr->fame * p_ptr->fame)) && !result; i++)
    {
        object_copy(&forge, dest);
        create_artifact(&forge, CREATE_ART_GOOD);
        power = object_value_real(&forge);

        if (power > best_power)
        {
            object_copy(&best, &forge);
            best_power = power;
        }
        if (power < worst_power)
        {
            object_copy(&worst, &forge);
            worst_power = power;
        }

        if (power >= min_power && power <= max_power)
        {
            object_copy(dest, &forge);
            result = TRUE;
        }
    }

    object_level = old_level;

    if (!result)
    {
        /* Failed! Return best or worst */
        if (worst_power > base_power)
            object_copy(dest, &worst);
        else
            object_copy(dest, &best);

        result = TRUE;
    }

    /* Flavor: Keep name of source artifact if possible */
    if (src->name1)
    {
        dest->name3 = src->name1;
        dest->art_name = quark_add(a_name + a_info[src->name1].name);
    }
    else
        dest->art_name = src->art_name;

    return result;
}

bool create_replacement_art(int a_idx, object_type *o_ptr)
{
    object_type        forge1 = {0};
    object_type        forge2 = {0};
    object_type        best = {0}, worst = {0};
    int                base_power, best_power, power = 0, worst_power = 10000000;
    int                old_level;
    artifact_type  *a_ptr = &a_info[a_idx];
    int                i;

    if (!a_ptr->name) return FALSE;
    if (no_artifacts) return FALSE;

    /* Score the Original */
    if (!create_named_art_aux(a_idx, &forge1)) return FALSE;
    if (object_is_weapon_ammo(&forge1))
    {
        forge1.to_h = MAX(10, forge1.to_h);
        forge1.to_d = MAX(10, forge1.to_d);
    }
    if (object_is_armour(&forge1))
    {
        forge1.to_a = MAX(10, forge1.to_a);
    }
    base_power = object_value_real(&forge1);
    
    best_power = -10000000;
    power = 0;
    old_level = object_level;

    if (object_level < a_ptr->level)
        object_level = a_ptr->level;

/*    for (i = 0; i < 1+m_bonus(2, object_level); i++)
    {
        object_prep(&forge2, forge1.k_idx);
        create_artifact(&forge2, CREATE_ART_GOOD);
        power = object_value_real(&forge2);

        if (power > best_power)
        {
            object_copy(&best, &forge2);
            best_power = power;
        }
        if (power > base_power * 7 / 10)
            break;
    }

    if (best_power < base_power * 4 / 10)
    {
        for (i = 0; i < 15; i++)
        {
            object_prep(&forge2, forge1.k_idx);
            create_artifact(&forge2, CREATE_ART_GOOD);
            power = object_value_real(&forge2);
            if (power > best_power)
            {
                object_copy(&keeper, &forge2);
                best_power = power;
            }
            if (power > base_power * 4 / 10)
                break;
        }
    }

    keeper.name3 = a_idx;
    object_level = old_level;
    object_copy(o_ptr, &keeper);
    o_ptr->weight = forge1.weight;

*/
    for (i = 0; i < 10000; i++)
    {
        object_prep(&forge2, forge1.k_idx);
        create_artifact(&forge2, CREATE_ART_GOOD);
        power = object_value_real(&forge2);

        if (power > best_power)
        {
            object_copy(&best, &forge2);
            best_power = power;
        }
        if (power < worst_power)
        {
            object_copy(&worst, &forge2);
            worst_power = power;
        }

        if (power > base_power * 7 / 10 && power < base_power * 2)
        {
            object_level = old_level;
            object_copy(o_ptr, &forge2);
            o_ptr->name3 = a_idx;
            o_ptr->weight = forge1.weight;
            return TRUE;
        }
    }

    /* Failed! Return best or worst */
    object_level = old_level;
    if (worst_power > base_power)
        object_copy(o_ptr, &worst);
    else
        object_copy(o_ptr, &best);

    o_ptr->name3 = a_idx;
    o_ptr->weight = forge1.weight;

    return TRUE;
}

/*
 * Create the artifact of the specified number
 */
bool create_named_art_aux(int a_idx, object_type *o_ptr)
{
    int k_idx;
    artifact_type *a_ptr = &a_info[a_idx];

    if (!a_ptr->name) return FALSE;
    k_idx = lookup_kind(a_ptr->tval, a_ptr->sval);
    if (!k_idx) return FALSE;

    object_prep(o_ptr, k_idx);

    o_ptr->name1 = a_idx;
    o_ptr->pval = a_ptr->pval;
    o_ptr->ac = a_ptr->ac;
    o_ptr->dd = a_ptr->dd;
    o_ptr->ds = a_ptr->ds;
    o_ptr->to_a = a_ptr->to_a;
    o_ptr->to_h = a_ptr->to_h;
    o_ptr->to_d = a_ptr->to_d;
    o_ptr->weight = a_ptr->weight;

    if (a_ptr->gen_flags & TRG_CURSED) o_ptr->curse_flags |= (TRC_CURSED);
    if (a_ptr->gen_flags & TRG_HEAVY_CURSE) o_ptr->curse_flags |= (TRC_HEAVY_CURSE);
    if (a_ptr->gen_flags & TRG_PERMA_CURSE) o_ptr->curse_flags |= (TRC_PERMA_CURSE);
    if (a_ptr->gen_flags & (TRG_RANDOM_CURSE0)) o_ptr->curse_flags |= get_curse(0, o_ptr);
    if (a_ptr->gen_flags & (TRG_RANDOM_CURSE1)) o_ptr->curse_flags |= get_curse(1, o_ptr);
    if (a_ptr->gen_flags & (TRG_RANDOM_CURSE2)) o_ptr->curse_flags |= get_curse(2, o_ptr);

    random_artifact_resistance(o_ptr, a_ptr);

    return TRUE;
}

bool create_named_art(int a_idx, int y, int x)
{
    if (no_artifacts) return FALSE;

    if (random_artifacts)
    {
        object_type forge;
        if (create_replacement_art(a_idx, &forge))
            return drop_near(&forge, -1, y, x) ? TRUE : FALSE;
    }
    else
    {
        object_type forge;
        if (create_named_art_aux(a_idx, &forge))
            return drop_near(&forge, -1, y, x) ? TRUE : FALSE;
    }
    return FALSE;
}
