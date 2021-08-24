#include "angband.h"
#include <assert.h>

int hit_chance(int hand, int to_h, int ac)
{
    int chance = plr->skills.thn + (plr->attack_info[hand].to_h + to_h) * BTH_PLUS_ADJ;
    int odds;

    chance = chance * plr->attack_info[hand].skill_mul / 1000;
    if (plr_tim_find(T_STUN))
        chance -= chance * MIN(100, plr_tim_amount(T_STUN)) / 150;
    chance += virtue_current(VIRTUE_VALOUR) / 10;
    if (chance <= 0) return 0;

    odds = 95*(chance - ac*3/4)*1000/(chance*100);
    if (odds < 50) odds = 50;
    return (odds+5)/10;
}

int throw_hit_chance(int to_h, int ac, int range)
{
    int chance = plr->skill_tht + (plr->shooter_info.to_h + to_h) * BTH_PLUS_ADJ - range;
    int odds;

    if (plr_tim_find(T_STUN))
        chance -= chance * MIN(100, plr_tim_amount(T_STUN)) / 150;
    if (chance <= 0) return 0;

    odds = 95*(chance - ac*3/4)*1000/(chance*100);
    if (odds < 50) odds = 50;
    return (odds+5)/10;
}

int bow_hit_chance(int to_h, int ac)
{
    int chance;
    int odds;

    chance = plr->skills.thb + to_h * BTH_PLUS_ADJ;
    if (plr_tim_find(T_STUN))
        chance -= chance * MIN(100, plr_tim_amount(T_STUN)) / 150;
    if (chance <= 0) return 0;
    if (plr->concent)
    {
        ac *= (10 - plr->concent);
        ac /= 10;
    }

    odds = 95*(chance - ac*3/4)*1000/(chance*100);
    if (odds < 50) odds = 50;
    return (odds+5)/10;
}

/**********************************************************************
 * Number of Blows
 **********************************************************************/

/* TODO: This should be moved to class_t.calc_weapon_bonuses */
void init_blows_calc(object_type *o_ptr, plr_attack_info_ptr info)
{
    switch (plr->pclass)
    {
    case CLASS_WARRIOR:
        info->blows_calc.max = 600;
        info->blows_calc.wgt = 70;
        info->blows_calc.mul = 50 + plr->lev/2;
        break;

    case CLASS_MAULER:
        info->blows_calc.max = 300;
        info->blows_calc.wgt = 280;
        info->blows_calc.mul = 125;
        break;

    case CLASS_RAGE_MAGE:
        info->blows_calc.max = 300;
        info->blows_calc.wgt = 70;
        info->blows_calc.mul = 30;
        break;

    case CLASS_MAGE:
    case CLASS_NECROMANCER:
    case CLASS_HIGH_MAGE:
    case CLASS_YELLOW_MAGE:
    case CLASS_GRAY_MAGE:
    case CLASS_BLUE_MAGE:
        info->blows_calc.max = 400;
        info->blows_calc.wgt = 100;
        info->blows_calc.mul = 20;
        break;

    case CLASS_WARLOCK:
        info->blows_calc.max = 400;
        info->blows_calc.wgt = 100;
        info->blows_calc.mul = 35;
        switch (plr->psubclass)
        {
        case WARLOCK_DRAGONS:
            if ( plr->riding
              && (object_is_(o_ptr, TV_POLEARM, SV_LANCE) || object_is_(o_ptr, TV_POLEARM, SV_HEAVY_LANCE)) )
            {
                info->blows_calc.mul = 65;
            }
            break;
        case WARLOCK_ANGELS:
        case WARLOCK_DEMONS:
            info->blows_calc.max = 450;
            break;
        case WARLOCK_HOUNDS:
            info->blows_calc.max = 475;
            break;
        case WARLOCK_GIANTS:
            info->blows_calc.wgt = 200;
            info->blows_calc.mul = 50 + plr->lev/5;
            info->blows_calc.max = 500;
            break;
        }
        break;

    case CLASS_PSION:
        info->blows_calc.max = 400;
        info->blows_calc.wgt = 100;
        info->blows_calc.mul = 30;
        break;

    case CLASS_PRIEST:
    case CLASS_MAGIC_EATER:
    case CLASS_MINDCRAFTER:
        info->blows_calc.max = 500;
        info->blows_calc.wgt = 100;
        info->blows_calc.mul = 35;
        break;

    case CLASS_HIGH_PRIEST:
        info->blows_calc.max = 500;
        info->blows_calc.wgt = 100;
        info->blows_calc.mul = 30;
        break;

    case CLASS_DEVICEMASTER:
        info->blows_calc.max = 400;
        info->blows_calc.wgt = 100;
        info->blows_calc.mul = 35;
        if (plr->psubclass == DEVICEMASTER_POTIONS || plr->psubclass == DEVICEMASTER_SCROLLS)
            info->blows_calc.max = 500;
        break;

    case CLASS_ROGUE:
        info->blows_calc.max = MAX(400, 500 + (150 - o_ptr->weight));
        info->blows_calc.wgt = 40;
        info->blows_calc.mul = 30;
        break;

    case CLASS_SCOUT:
        info->blows_calc.max = 400;
        info->blows_calc.wgt = 70;
        info->blows_calc.mul = 25;
        break;

    case CLASS_RANGER:
        info->blows_calc.max = 500;
        info->blows_calc.wgt = 70;
        info->blows_calc.mul = 40;
        break;

    case CLASS_PALADIN:
    case CLASS_SAMURAI:
        info->blows_calc.max = 550;
        info->blows_calc.wgt = 70;
        info->blows_calc.mul = 45;
        break;

    case CLASS_MYSTIC:
        info->blows_calc.max = 100;
        info->blows_calc.wgt = 100;
        info->blows_calc.mul = 10;
        break;

    case CLASS_WEAPONSMITH:
    case CLASS_RUNE_KNIGHT:
        info->blows_calc.max = 525;
        info->blows_calc.wgt = 150;
        info->blows_calc.mul = 55;
        break;

    case CLASS_WARRIOR_MAGE:
    case CLASS_RED_MAGE:
        info->blows_calc.max = 525; info->blows_calc.wgt = 70; info->blows_calc.mul = 30; break;

    case CLASS_CHAOS_WARRIOR:
        info->blows_calc.max = 550; info->blows_calc.wgt = 70; info->blows_calc.mul = 45; break;

    case CLASS_MONK:
        info->blows_calc.max = 500; info->blows_calc.wgt = 60; info->blows_calc.mul = 30; break;

    case CLASS_TIME_LORD:
        info->blows_calc.max = 400; info->blows_calc.wgt = 100; info->blows_calc.mul = 30; break;

    case CLASS_ARCHAEOLOGIST:
        info->blows_calc.max = 400; info->blows_calc.wgt = 70; info->blows_calc.mul = 30;
        if (archaeologist_is_favored_weapon(o_ptr))
        {
            info->blows_calc.max = 500;
            info->blows_calc.mul = 40;
        }
        break;

    case CLASS_BLOOD_KNIGHT:
        info->blows_calc.max = 300; info->blows_calc.wgt = 150; info->blows_calc.mul = 30; break;

    case CLASS_DUELIST:
        info->blows_calc.max = 100; info->blows_calc.wgt = 70; info->blows_calc.mul = 40; break;

    case CLASS_WILD_TALENT:
        info->blows_calc.max = 450; info->blows_calc.wgt = 70; info->blows_calc.mul = 40; break;

    case CLASS_BEASTMASTER:
        info->blows_calc.max = 500; info->blows_calc.wgt = 70; info->blows_calc.mul = 35; break;

    case CLASS_CAVALRY:
        if (plr->riding && obj_has_flag(o_ptr, OF_RIDING)) {info->blows_calc.max = 550; info->blows_calc.wgt = 70; info->blows_calc.mul = 65;}
        else {info->blows_calc.max = 500; info->blows_calc.wgt = 100; info->blows_calc.mul = 35;}
        break;
    case CLASS_SORCERER:
        info->blows_calc.max = 100; info->blows_calc.wgt = 1; info->blows_calc.mul = 10; break;

    case CLASS_ARCHER:
    case CLASS_BARD:
        info->blows_calc.max = 450; info->blows_calc.wgt = 70; info->blows_calc.mul = 20; break;

    case CLASS_FORCETRAINER:
        info->blows_calc.max = 400; info->blows_calc.wgt = 60; info->blows_calc.mul = 20; break;

    case CLASS_MIRROR_MASTER:
    case CLASS_SNIPER:
        info->blows_calc.max = 400; info->blows_calc.wgt = 100; info->blows_calc.mul = 30; break;

    case CLASS_NINJA:
        info->blows_calc.max = 425; info->blows_calc.wgt = 20; info->blows_calc.mul = 10; break;

    case CLASS_MONSTER:
        info->blows_calc.max = 500; info->blows_calc.wgt = 70; info->blows_calc.mul = 50;
        if (prace_is_(RACE_MON_LICH))
        {
            info->blows_calc.max = 400;
            info->blows_calc.mul = 30;
        }
        else if (prace_is_(RACE_MON_POSSESSOR))
        {
            info->blows_calc.max = 400;
        }
        else if (prace_is_(RACE_MON_MIMIC))
        {
            info->blows_calc.max = 400;
        }
        else if (prace_is_(RACE_MON_TROLL))
        {
            info->blows_calc.max = 550;
        }
        else if (prace_is_(RACE_MON_GIANT))
        {
            info->blows_calc.max = 550;
            info->blows_calc.mul = 50 + plr->lev/5;
            info->blows_calc.wgt = 200;
            if (giant_is_(GIANT_HRU) && plr->lev >= 40)
                info->blows_calc.mul = 80;
        }
        else if ( prace_is_(RACE_MON_JELLY)
               || demon_is_(DEMON_KHORNE) )
        {
            info->blows_calc.max = 600;
            info->blows_calc.mul = 50 + plr->lev/5;
        }
        else if (prace_is_(RACE_MON_LEPRECHAUN))
        {
            info->blows_calc.max = 300; /* but gold gives extra blows ... */
            info->blows_calc.mul = 20;
        }
        else if (prace_is_(RACE_MON_SWORD))
        {
            info->blows_calc.max = 525;
            if (plr->lev >= 45) /* Death Scythes retaliate! */
                info->blows_calc.max = 300;
        }
        else if (prace_is_(RACE_MON_GOLEM))
        {
            info->blows_calc.max = 100;
        }
        break;
    }
}

/**********************************************************************
 * Ranged Attacks
 **********************************************************************/
int display_shooter_mode = 0;

/* Multiplier scaled by 100 */
int bow_mult(object_type *o_ptr)
{
    int mult = o_ptr->mult;

    mult += plr->shooter_info.to_mult;

    if (weaponmaster_is_(WEAPONMASTER_CROSSBOWS))
    {
        int idx = plr->stat_ind[A_STR] + 4;
        if (idx > 40-3)
            idx = 40-3;
        mult = mult * (100 + (int)(adj_str_td[idx]) - 128) / 100;
    }
    else
        mult = mult * (100 + (int)(adj_str_td[plr->stat_ind[A_STR]]) - 128) / 100;

    return mult;
}

int bow_range(object_type *o_ptr)
{
    int range, mult;

    mult = bow_mult(o_ptr);
    range = 13 + mult/80;

    switch (o_ptr->sval)
    {
    case SV_LIGHT_XBOW:
    case SV_HEAVY_XBOW:
        if (plr->concent)
            range += (plr->concent + 1) / 2;
        break;
    }

    return MIN(MAX_RANGE, range);
}
