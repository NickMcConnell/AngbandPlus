#include "angband.h"
#include <assert.h>

int hit_chance(int hand, int to_h, int ac)
{
    int chance = p_ptr->skills.thn + (p_ptr->attack_info[hand].to_h + to_h) * BTH_PLUS_ADJ;
    int odds;

    chance = chance * p_ptr->attack_info[hand].skill_mul / 1000;
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
    int chance = p_ptr->skill_tht + (p_ptr->shooter_info.to_h + to_h) * BTH_PLUS_ADJ - range;
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

    chance = p_ptr->skills.thb + to_h * BTH_PLUS_ADJ;
    if (plr_tim_find(T_STUN))
        chance -= chance * MIN(100, plr_tim_amount(T_STUN)) / 150;
    if (chance <= 0) return 0;
    if (p_ptr->concent)
    {
        ac *= (10 - p_ptr->concent);
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
    switch (p_ptr->pclass)
    {
    case CLASS_WARRIOR:
        info->blows_calc.max = 600;
        info->blows_calc.wgt = 70;
        info->blows_calc.mult = 50 + p_ptr->lev/2;
        break;

    case CLASS_MAULER:
        info->blows_calc.max = 300;
        info->blows_calc.wgt = 280;
        info->blows_calc.mult = 125;
        break;

    case CLASS_RAGE_MAGE:
        info->blows_calc.max = 300;
        info->blows_calc.wgt = 70;
        info->blows_calc.mult = 30;
        break;

    case CLASS_MAGE:
    case CLASS_NECROMANCER:
    case CLASS_HIGH_MAGE:
    case CLASS_YELLOW_MAGE:
    case CLASS_GRAY_MAGE:
        info->blows_calc.max = 400;
        info->blows_calc.wgt = 100;
        info->blows_calc.mult = 20;
        break;

    case CLASS_WARLOCK:
        info->blows_calc.max = 400;
        info->blows_calc.wgt = 100;
        info->blows_calc.mult = 35;
        switch (p_ptr->psubclass)
        {
        case WARLOCK_DRAGONS:
            if ( p_ptr->riding
              && (object_is_(o_ptr, TV_POLEARM, SV_LANCE) || object_is_(o_ptr, TV_POLEARM, SV_HEAVY_LANCE)) )
            {
                info->blows_calc.mult = 65;
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
            info->blows_calc.mult = 50 + p_ptr->lev/5;
            info->blows_calc.max = 500;
            break;
        }
        break;

    case CLASS_PSION:
        info->blows_calc.max = 400;
        info->blows_calc.wgt = 100;
        info->blows_calc.mult = 30;
        break;

    case CLASS_PRIEST:
    case CLASS_MAGIC_EATER:
    case CLASS_MINDCRAFTER:
        info->blows_calc.max = 500;
        info->blows_calc.wgt = 100;
        info->blows_calc.mult = 35;
        break;

    case CLASS_DEVICEMASTER:
        info->blows_calc.max = 400;
        info->blows_calc.wgt = 100;
        info->blows_calc.mult = 35;
        if (p_ptr->psubclass == DEVICEMASTER_POTIONS || p_ptr->psubclass == DEVICEMASTER_SCROLLS)
            info->blows_calc.max = 500;
        break;

    case CLASS_ROGUE:
        info->blows_calc.max = MAX(400, 500 + (150 - o_ptr->weight));
        info->blows_calc.wgt = 40;
        info->blows_calc.mult = 30;
        break;

    case CLASS_SCOUT:
        info->blows_calc.max = 400;
        info->blows_calc.wgt = 70;
        info->blows_calc.mult = 25;
        break;

    case CLASS_RANGER:
        info->blows_calc.max = 500;
        info->blows_calc.wgt = 70;
        info->blows_calc.mult = 40;
        break;

    case CLASS_PALADIN:
    case CLASS_SAMURAI:
        info->blows_calc.max = 550;
        info->blows_calc.wgt = 70;
        info->blows_calc.mult = 45;
        break;

    case CLASS_MYSTIC:
        info->blows_calc.max = 100;
        info->blows_calc.wgt = 100;
        info->blows_calc.mult = 10;
        break;

    case CLASS_WEAPONSMITH:
    case CLASS_RUNE_KNIGHT:
        info->blows_calc.max = 525;
        info->blows_calc.wgt = 150;
        info->blows_calc.mult = 55;
        break;

    case CLASS_WARRIOR_MAGE:
    case CLASS_RED_MAGE:
        info->blows_calc.max = 525; info->blows_calc.wgt = 70; info->blows_calc.mult = 30; break;

    case CLASS_CHAOS_WARRIOR:
        info->blows_calc.max = 550; info->blows_calc.wgt = 70; info->blows_calc.mult = 45; break;

    case CLASS_MONK:
        info->blows_calc.max = 500; info->blows_calc.wgt = 60; info->blows_calc.mult = 30; break;

    case CLASS_TIME_LORD:
        info->blows_calc.max = 400; info->blows_calc.wgt = 100; info->blows_calc.mult = 30; break;

    case CLASS_ARCHAEOLOGIST:
        info->blows_calc.max = 400; info->blows_calc.wgt = 70; info->blows_calc.mult = 30;
        if (archaeologist_is_favored_weapon(o_ptr))
        {
            info->blows_calc.max = 500;
            info->blows_calc.mult = 40;
        }
        break;

    case CLASS_BLOOD_KNIGHT:
        info->blows_calc.max = 300; info->blows_calc.wgt = 150; info->blows_calc.mult = 30; break;

    case CLASS_DUELIST:
        info->blows_calc.max = 100; info->blows_calc.wgt = 70; info->blows_calc.mult = 40; break;

    case CLASS_WILD_TALENT:
        info->blows_calc.max = 450; info->blows_calc.wgt = 70; info->blows_calc.mult = 40; break;

    case CLASS_BEASTMASTER:
        info->blows_calc.max = 500; info->blows_calc.wgt = 70; info->blows_calc.mult = 35; break;

    case CLASS_CAVALRY:
        if (p_ptr->riding && obj_has_flag(o_ptr, OF_RIDING)) {info->blows_calc.max = 550; info->blows_calc.wgt = 70; info->blows_calc.mult = 65;}
        else {info->blows_calc.max = 500; info->blows_calc.wgt = 100; info->blows_calc.mult = 35;}
        break;
    case CLASS_SORCERER:
        info->blows_calc.max = 100; info->blows_calc.wgt = 1; info->blows_calc.mult = 10; break;

    case CLASS_ARCHER:
    case CLASS_BARD:
        info->blows_calc.max = 450; info->blows_calc.wgt = 70; info->blows_calc.mult = 20; break;

    case CLASS_FORCETRAINER:
        info->blows_calc.max = 400; info->blows_calc.wgt = 60; info->blows_calc.mult = 20; break;

    case CLASS_MIRROR_MASTER:
    case CLASS_SNIPER:
        info->blows_calc.max = 400; info->blows_calc.wgt = 100; info->blows_calc.mult = 30; break;

    case CLASS_NINJA:
        info->blows_calc.max = 425; info->blows_calc.wgt = 20; info->blows_calc.mult = 10; break;

    case CLASS_MONSTER:
        info->blows_calc.max = 500; info->blows_calc.wgt = 70; info->blows_calc.mult = 50;
        if (prace_is_(RACE_MON_LICH))
        {
            info->blows_calc.max = 400;
            info->blows_calc.mult = 30;
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
            info->blows_calc.mult = 50 + p_ptr->lev/5;
            info->blows_calc.wgt = 200;
            if (giant_is_(GIANT_HRU) && p_ptr->lev >= 40)
                info->blows_calc.mult = 80;
        }
        else if ( prace_is_(RACE_MON_JELLY)
               || demon_is_(DEMON_KHORNE) )
        {
            info->blows_calc.max = 600;
            info->blows_calc.mult = 50 + p_ptr->lev/5;
        }
        else if (prace_is_(RACE_MON_LEPRECHAUN))
        {
            info->blows_calc.max = 300; /* but gold gives extra blows ... */
            info->blows_calc.mult = 20;
        }
        else if (prace_is_(RACE_MON_SWORD))
        {
            info->blows_calc.max = 525;
            if (p_ptr->lev >= 45) /* Death Scythes retaliate! */
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

    mult += p_ptr->shooter_info.to_mult;

    if (weaponmaster_is_(WEAPONMASTER_CROSSBOWS))
    {
        int idx = p_ptr->stat_ind[A_STR] + 4;
        if (idx > 40-3)
            idx = 40-3;
        mult = mult * (100 + (int)(adj_str_td[idx]) - 128) / 100;
    }
    else
        mult = mult * (100 + (int)(adj_str_td[p_ptr->stat_ind[A_STR]]) - 128) / 100;

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
        if (p_ptr->concent)
            range += (p_ptr->concent + 1) / 2;
        break;
    }

    return MIN(MAX_RANGE, range);
}

static void _display_missile_slay(int bow_mult, int slay_mult, int crit_mult,
                                  bool force, int shots,
                                  int dd, int ds, int to_d, int to_d_xtra,
                                  cptr name, int color, doc_ptr doc)
{
    int dam = dd*(ds + 1)/2 + to_d;

    if (force) slay_mult += 50;
    dam = dam * slay_mult / 100;
    dam = dam * crit_mult / 100;
    if (p_ptr->concent) dam = boost_concentration_damage(dam);
    dam = dam * bow_mult / 100;

    dam += to_d_xtra;
    if (plr_tim_find(T_STUN))
        dam -= dam * MIN(100, plr_tim_amount(T_STUN)) / 150;

    doc_printf(doc, " <color:%c>%-8.8s</color>", attr_to_attr_char(color), name);
    doc_printf(doc, ": %d/%d\n", dam, shots * dam / 100);
}


static void _shooter_info_aux(doc_ptr doc, object_type *bow, object_type *arrow, int ct)
{
    char         o_name[MAX_NLEN];
    u32b         flgs[OF_ARRAY_SIZE];
    int          mult;
    int          to_h = 0;
    int          to_d = 0;
    int          to_h_bow = 0;
    int          to_d_bow = 0;
    int          to_h_xtra = p_ptr->shooter_info.dis_to_h;
    int          to_d_xtra = p_ptr->shooter_info.dis_to_d;
    int          dd = arrow->dd;
    int          ds = arrow->ds;
    critical_t   crit = {0};
    int          crit_pct = 0;
    int          num_fire = 0;
    doc_ptr      cols[2] = {0};
    bool         force = FALSE;

    cols[0] = doc_alloc(60);
    cols[1] = doc_alloc(10);

    missile_flags_known(arrow, flgs);
    mult = bow_mult(bow);

    if (obj_is_art(arrow))
        num_fire = 100;
    else if (p_ptr->shooter_info.base_shot)
        num_fire = NUM_SHOTS * 100 * 100 / bow_energy(bow->sval);

    if (obj_is_known(bow))
    {
        to_h_bow = bow->to_h;
        to_d_bow = bow->to_d;
    }
    to_h_bow += skills_bow_calc_bonus(bow->sval);

    if (obj_is_known(arrow))
    {
        to_h = arrow->to_h;
        to_d = arrow->to_d;
    }

    if (weaponmaster_is_(WEAPONMASTER_CROSSBOWS) && p_ptr->lev >= 15)
        to_d += 1 + p_ptr->lev/10;

    if (p_ptr->big_shot)
        ds += 2;

    {
        const int ct = 10 * 1000;
        int i, crits = 0;
        /* Compute Average Effects of Criticals by sampling */
        for (i = 0; i < ct; i++)
        {
            critical_t tmp = critical_shot(arrow->weight, arrow->to_h);
            if (tmp.desc)
            {
                crit.mul += tmp.mul;
                crit.to_d += tmp.to_d;
                crits++;
            }
            else
                crit.mul += 100;
        }
        crit.mul = crit.mul / ct;
        crit.to_d = crit.to_d * 100 / ct;
        crit_pct = crits * 1000 / ct;
    }

    /* First Column */
    object_desc(o_name, arrow, OD_OMIT_INSCRIPTION | OD_COLOR_CODED);
    doc_printf(cols[0], "<color:u> Ammo #%-2d</color>: <indent><style:indent>%s</style></indent>\n", ct, o_name);

    doc_printf(cols[0], " %-8.8s: %d%%\n", "Breakage", breakage_chance(arrow));
    doc_printf(cols[0], " %-8.8s: %d.%d lbs\n", "Weight", arrow->weight/10, arrow->weight%10);
    doc_printf(cols[0], " %-8.8s: %d + %d = %d\n", "To Hit", to_h, to_h_bow + to_h_xtra, to_h + to_h_bow + to_h_xtra);
    doc_printf(cols[0], " %-8.8s: %d (%s)\n", "To Dam", to_d, "Multiplier Applies");
    doc_printf(cols[0], " %-8.8s: %d (%s)\n", "To Dam", to_d_bow + to_d_xtra, "Multiplier Does Not Apply");
    doc_printf(cols[0], " <color:G>%-8.8s</color>\n", "Damage");

    if (crit.to_d)
    {
        doc_printf(cols[0], " %-8.8s: %d.%02dx + %d.%02d\n", "Crits",
                        crit.mul/100, crit.mul%100, crit.to_d/100, crit.to_d%100);
    }
    else
    {
        doc_printf(cols[0], " %-8.8s: %d.%02dx (%d.%d%%)\n", "Crits",
                        crit.mul/100, crit.mul%100, crit_pct / 10, crit_pct % 10);
    }

    to_d_xtra = to_d_bow + to_d_xtra + crit.to_d/100;

    _display_missile_slay(mult, 100, crit.mul, FALSE, num_fire, dd, ds, to_d, to_d_xtra, "Normal", TERM_WHITE, cols[0]);

    if (have_flag(flgs, OF_BRAND_MANA) && p_ptr->csp >= 1 + arrow->dd * arrow->ds / 2)
    {
        force = TRUE;
        _display_missile_slay(mult, 100, crit.mul, force, num_fire, dd, ds, to_d, to_d_xtra, "Force", TERM_L_BLUE, cols[0]);
    }

    if (display_shooter_mode == SP_FINAL)
    {
        int snipe = sniper_multiplier(display_shooter_mode, arrow, NULL) * 10;
        _display_missile_slay(mult, snipe, crit.mul, force, num_fire, dd, ds, to_d, to_d_xtra, "All", TERM_VIOLET, cols[0]);
    }

    if (have_flag(flgs, OF_SLAY_LIVING))
        _display_missile_slay(mult, 143, crit.mul, force, num_fire, dd, ds, to_d, to_d_xtra, "Living", TERM_YELLOW, cols[0]);

    if (have_flag(flgs, OF_KILL_ANIMAL))
        _display_missile_slay(mult, 224, crit.mul, force, num_fire, dd, ds, to_d, to_d_xtra, "Animals", TERM_YELLOW, cols[0]);
    else if (have_flag(flgs, OF_SLAY_ANIMAL))
        _display_missile_slay(mult, 162, crit.mul, force, num_fire, dd, ds, to_d, to_d_xtra, "Animals", TERM_YELLOW, cols[0]);

    if (display_shooter_mode == SP_HOLYNESS)
    {
        int snipe = sniper_multiplier(display_shooter_mode, arrow, NULL) * 10;
        _display_missile_slay(mult, snipe, crit.mul, force, num_fire, dd, ds, to_d, to_d_xtra, "Evil", TERM_VIOLET, cols[0]);
    }
    else if (have_flag(flgs, OF_KILL_EVIL))
        _display_missile_slay(mult, 186, crit.mul, force, num_fire, dd, ds, to_d, to_d_xtra, "Evil", TERM_YELLOW, cols[0]);
    else if (have_flag(flgs, OF_SLAY_EVIL))
        _display_missile_slay(mult, 143, crit.mul, force, num_fire, dd, ds, to_d, to_d_xtra, "Evil", TERM_YELLOW, cols[0]);

    if (display_shooter_mode == SP_EVILNESS)
    {
        int snipe = sniper_multiplier(display_shooter_mode, arrow, NULL) * 10;
        _display_missile_slay(mult, snipe, crit.mul, force, num_fire, dd, ds, to_d, to_d_xtra, "Good", TERM_VIOLET, cols[0]);
    }
    else if (have_flag(flgs, OF_SLAY_GOOD))
        _display_missile_slay(mult, 143, crit.mul, force, num_fire, dd, ds, to_d, to_d_xtra, "Good", TERM_YELLOW, cols[0]);

    if (have_flag(flgs, OF_KILL_HUMAN))
        _display_missile_slay(mult, 224, crit.mul, force, num_fire, dd, ds, to_d, to_d_xtra, "Human", TERM_YELLOW, cols[0]);
    else if (have_flag(flgs, OF_SLAY_HUMAN))
        _display_missile_slay(mult, 162, crit.mul, force, num_fire, dd, ds, to_d, to_d_xtra, "Human", TERM_YELLOW, cols[0]);

    if (have_flag(flgs, OF_KILL_UNDEAD))
        _display_missile_slay(mult, 280, crit.mul, force, num_fire, dd, ds, to_d, to_d_xtra, "Undead", TERM_YELLOW, cols[0]);
    else if (have_flag(flgs, OF_SLAY_UNDEAD))
        _display_missile_slay(mult, 190, crit.mul, force, num_fire, dd, ds, to_d, to_d_xtra, "Undead", TERM_YELLOW, cols[0]);

    if (have_flag(flgs, OF_KILL_DEMON))
        _display_missile_slay(mult, 280, crit.mul, force, num_fire, dd, ds, to_d, to_d_xtra, "Demons", TERM_YELLOW, cols[0]);
    else if (have_flag(flgs, OF_SLAY_DEMON))
        _display_missile_slay(mult, 190, crit.mul, force, num_fire, dd, ds, to_d, to_d_xtra, "Demons", TERM_YELLOW, cols[0]);

    if (have_flag(flgs, OF_KILL_ORC))
        _display_missile_slay(mult, 280, crit.mul, force, num_fire, dd, ds, to_d, to_d_xtra, "Orcs", TERM_YELLOW, cols[0]);
    else if (have_flag(flgs, OF_SLAY_ORC))
        _display_missile_slay(mult, 190, crit.mul, force, num_fire, dd, ds, to_d, to_d_xtra, "Orcs", TERM_YELLOW, cols[0]);

    if (have_flag(flgs, OF_KILL_TROLL))
        _display_missile_slay(mult, 280, crit.mul, force, num_fire, dd, ds, to_d, to_d_xtra, "Trolls", TERM_YELLOW, cols[0]);
    else if (have_flag(flgs, OF_SLAY_TROLL))
        _display_missile_slay(mult, 190, crit.mul, force, num_fire, dd, ds, to_d, to_d_xtra, "Trolls", TERM_YELLOW, cols[0]);

    if (have_flag(flgs, OF_KILL_GIANT))
        _display_missile_slay(mult, 280, crit.mul, force, num_fire, dd, ds, to_d, to_d_xtra, "Giants", TERM_YELLOW, cols[0]);
    else if (have_flag(flgs, OF_SLAY_GIANT))
        _display_missile_slay(mult, 190, crit.mul, force, num_fire, dd, ds, to_d, to_d_xtra, "Giants", TERM_YELLOW, cols[0]);

    if (have_flag(flgs, OF_KILL_DRAGON))
        _display_missile_slay(mult, 280, crit.mul, force, num_fire, dd, ds, to_d, to_d_xtra, "Dragons", TERM_YELLOW, cols[0]);
    else if (have_flag(flgs, OF_SLAY_DRAGON))
        _display_missile_slay(mult, 190, crit.mul, force, num_fire, dd, ds, to_d, to_d_xtra, "Dragons", TERM_YELLOW, cols[0]);

    if (have_flag(flgs, OF_BRAND_ACID))
        _display_missile_slay(mult, 162, crit.mul, force, num_fire, dd, ds, to_d, to_d_xtra, "Acid", TERM_RED, cols[0]);

    if (display_shooter_mode == SP_ELEC)
    {
        int snipe = sniper_multiplier(display_shooter_mode, arrow, NULL) * 10;
        _display_missile_slay(mult, snipe, crit.mul, force, num_fire, dd, ds, to_d, to_d_xtra, "Elec", TERM_VIOLET, cols[0]);
    }
    else if (have_flag(flgs, OF_BRAND_ELEC))
        _display_missile_slay(mult, 162, crit.mul, force, num_fire, dd, ds, to_d, to_d_xtra, "Elec", TERM_RED, cols[0]);

    if (display_shooter_mode == SP_FIRE)
    {
        int snipe = sniper_multiplier(display_shooter_mode, arrow, NULL) * 10;
        _display_missile_slay(mult, snipe, crit.mul, force, num_fire, dd, ds, to_d, to_d_xtra, "Fire", TERM_VIOLET, cols[0]);
    }
    else if (have_flag(flgs, OF_BRAND_FIRE))
        _display_missile_slay(mult, 162, crit.mul, force, num_fire, dd, ds, to_d, to_d_xtra, "Fire", TERM_RED, cols[0]);

    if (display_shooter_mode == SP_COLD)
    {
        int snipe = sniper_multiplier(display_shooter_mode, arrow, NULL) * 10;
        _display_missile_slay(mult, snipe, crit.mul, force, num_fire, dd, ds, to_d, to_d_xtra, "Cold", TERM_VIOLET, cols[0]);
    }
    else if (have_flag(flgs, OF_BRAND_COLD))
        _display_missile_slay(mult, 162, crit.mul, force, num_fire, dd, ds, to_d, to_d_xtra, "Cold", TERM_RED, cols[0]);

    if (have_flag(flgs, OF_BRAND_POIS))
        _display_missile_slay(mult, 162, crit.mul, force, num_fire, dd, ds, to_d, to_d_xtra, "Poison", TERM_RED, cols[0]);

    if (display_shooter_mode == SP_KILL_WALL)
    {
        int snipe = sniper_multiplier(display_shooter_mode, arrow, NULL) * 10;
        _display_missile_slay(mult, snipe, crit.mul, force, num_fire, dd, ds, to_d, to_d_xtra, "HurtRock", TERM_VIOLET, cols[0]);
        _display_missile_slay(mult, snipe, crit.mul, force, num_fire, dd, ds, to_d, to_d_xtra, "Nonliving", TERM_VIOLET, cols[0]);
    }
    if (display_shooter_mode == SP_LITE)
    {
        int snipe = sniper_multiplier(display_shooter_mode, arrow, NULL) * 10;
        _display_missile_slay(mult, snipe, crit.mul, force, num_fire, dd, ds, to_d, to_d_xtra, "HurtLite", TERM_VIOLET, cols[0]);
    }
    /* Second Column */
    to_h = to_h + to_h_bow + to_h_xtra;

    doc_insert(cols[1], " <color:G>AC Hit</color>\n");
    doc_printf(cols[1], "%3d %2d%%\n", 25, bow_hit_chance(to_h, 25));
    doc_printf(cols[1], "%3d %2d%%\n", 50, bow_hit_chance(to_h, 50));
    doc_printf(cols[1], "%3d %2d%%\n", 100, bow_hit_chance(to_h, 100));
    doc_printf(cols[1], "%3d %2d%%\n", 150, bow_hit_chance(to_h, 150));
    doc_printf(cols[1], "%3d %2d%%\n", 175, bow_hit_chance(to_h, 175));
    doc_printf(cols[1], "%3d %2d%%\n", 200, bow_hit_chance(to_h, 200));

    doc_insert_cols(doc, cols, 2, 1);
    doc_free(cols[0]);
    doc_free(cols[1]);
}

void display_shooter_info(doc_ptr doc)
{
    object_type *bow_ptr = NULL;
    int          slot = equip_find_obj(TV_BOW, SV_ANY);
    char         o_name[MAX_NLEN];
    int          mult;
    int          num_fire = 0;
    int          to_h = 0;
    int          to_d = 0;
    int          i, j;

    if (!slot || prace_is_(RACE_MON_JELLY) || !p_ptr->shooter_info.tval_ammo)
        return;

    bow_ptr = equip_obj(slot);
    assert(bow_ptr);

    mult = bow_mult(bow_ptr);

    if (p_ptr->shooter_info.base_shot)
        num_fire = NUM_SHOTS * 100 * 100 / bow_energy(bow_ptr->sval);

    if (obj_is_known(bow_ptr))
    {
        to_h = bow_ptr->to_h;
        to_d = bow_ptr->to_d;
    }
    to_h += skills_bow_calc_bonus(bow_ptr->sval);

    /* Shooter */
    object_desc(o_name, bow_ptr, OD_OMIT_INSCRIPTION | OD_COLOR_CODED);
    doc_printf(doc, " <color:y>Shooting</color>: <indent><style:indent>%s</style></indent>\n", o_name);

    doc_printf(doc, " %-8.8s: %d'\n", "Range", (bow_range(bow_ptr) + 1) * 10);
    doc_printf(doc, " %-8.8s: %d.%02d\n", "Shots", num_fire/100, num_fire%100);
    doc_printf(doc, " %-8.8s: %d.%02dx\n", "Mult", mult/100, mult%100);
    doc_printf(doc, " %-8.8s: %s (%+d To Hit)\n", "Profic", skills_bow_describe_current(bow_ptr->sval), skills_bow_calc_bonus(bow_ptr->sval));
    doc_printf(doc, " %-8.8s: %d + %d = %d\n", "To Hit", to_h, p_ptr->shooter_info.dis_to_h, to_h + p_ptr->shooter_info.dis_to_h);
    if (weaponmaster_is_(WEAPONMASTER_CROSSBOWS) && p_ptr->lev >= 15)
        doc_printf(doc, " %-8.8s: %d (%s)\n", "To Dam", 1 + p_ptr->lev/10, "Multiplier Applies");
    doc_printf(doc, " %-8.8s: %d (%s)\n", "Xtra Dam", p_ptr->shooter_info.dis_to_d + to_d, "Multiplier Does Not Apply");
    doc_newline(doc);

    /* Ammo */
    j = 0;
    for (i = quiver_find_first(obj_can_shoot); i; i = quiver_find_next(obj_can_shoot, i))
    {
        obj_ptr ammo = quiver_obj(i);
        _shooter_info_aux(doc, bow_ptr, ammo, ++j);
    }
    for (i = pack_find_first(obj_can_shoot); i; i = pack_find_next(obj_can_shoot, i))
    {
        obj_ptr ammo = pack_obj(i);
        _shooter_info_aux(doc, bow_ptr, ammo, ++j);
    }

}

