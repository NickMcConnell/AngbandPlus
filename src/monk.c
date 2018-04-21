#include "angband.h"

/* TODO: Move special py_attack code here
         Move blows calculations here
         Move posture code here */

static int _max_tries(int lvl, u32b defense)
{
    int tries = 0;
    if (defense & KAMAE_BYAKKO)
        tries = (lvl < 3 ? 1 : lvl / 3);
    else if (defense & KAMAE_SUZAKU)
        tries = 1;
    else if (defense & KAMAE_GENBU)
        tries = 1;
    else if (mystic_get_toggle() == MYSTIC_TOGGLE_OFFENSE)
        tries = 1 + lvl/4;
    else if (mystic_get_toggle() == MYSTIC_TOGGLE_DEFENSE)
        tries = 1 + lvl/15;
    else
        tries = (lvl < 7 ? 1 : lvl / 7);

    return tries;
}

static int _get_attack_idx(int lvl, u32b defense)
{
    int tries = _max_tries(lvl, defense);
    int i;
    int attack_idx = 0;
    int best_attack_idx = 0;
    int min_level;

    if (p_ptr->stun || p_ptr->confused)
        return 0; /* Punch for 1d4 */

    for (i = 0; i < tries; i++)
    {
        martial_arts *ma_ptr;
        do
        {
            attack_idx = randint0(MAX_MA);
            ma_ptr = &ma_blows[attack_idx];

            if (p_ptr->pclass == CLASS_FORCETRAINER && ma_ptr->min_level > 1) 
                min_level = ma_ptr->min_level + 3;
            else min_level = ma_ptr->min_level;
        }
        while (min_level > lvl || randint1(lvl) < ma_ptr->chance);

        if (ma_ptr->min_level > ma_blows[best_attack_idx].min_level)
            best_attack_idx = attack_idx;
    }

    return best_attack_idx;
}

typedef struct _attack_s {
    int count;
    int mul;
    int to_d;
} _attack_t;

void _get_attack_counts(int tot, _attack_t *counts, int hand)
{
    int i;
    _attack_t *_attack_ptr;
    critical_t crit;

    for (i = 0; i < MAX_MA; i++)
    {
        _attack_ptr = &counts[i];
        _attack_ptr->count = 0;
        _attack_ptr->mul = 0;
        _attack_ptr->to_d = 0;
    }

    for (i = 0; i < tot; i++)
    {
        int attack_idx = _get_attack_idx(p_ptr->lev, p_ptr->special_defense);
        martial_arts *ma_ptr = &ma_blows[attack_idx];

        _attack_ptr = &counts[attack_idx];
        _attack_ptr->count++;
        
        /* Crits depend on the attack chosen. The following won't be stable
           for attacks that occur infrequently, but hopefully things will just
           average out */
        crit = monk_get_critical(ma_ptr, hand, 0);

        if (crit.desc)
        {
            _attack_ptr->mul += crit.mul;
            _attack_ptr->to_d += crit.to_d;
        }
        else
            _attack_ptr->mul += 100;
    }
}

static int _get_weight(void)
{
    int weight = 8;
    if (p_ptr->special_defense & KAMAE_SUZAKU) weight = 4;
    if (mystic_get_toggle() == MYSTIC_TOGGLE_DEFENSE) weight = 6;
    if (mystic_get_toggle() == MYSTIC_TOGGLE_OFFENSE) weight = 10;
    if ((p_ptr->pclass == CLASS_FORCETRAINER) && (p_ptr->magic_num1[0]))
    {
        weight += (p_ptr->magic_num1[0]/30);
        if (weight > 20) weight = 20;
    }
    return weight * p_ptr->lev;
}

critical_t monk_get_critical(martial_arts *ma_ptr, int hand, int mode)
{
    int min_level = ma_ptr->min_level;
    int weight = _get_weight();

    if (mode == MYSTIC_CRITICAL)
        weight += weight/2 + 300;
    
    if (p_ptr->pclass == CLASS_FORCETRAINER) min_level = MAX(1, min_level - 3);

    return critical_norm(weight, min_level, p_ptr->weapon_info[hand].to_h, 0, 0);
}

int monk_get_attack_idx(void)
{
    return _get_attack_idx(p_ptr->lev, p_ptr->special_defense);
}

void monk_display_attack_info(int hand, int row, int col)
{
    _attack_t counts[MAX_MA];
    int i;
    const int tot = 10 * 1000;
    char buf[128];
    int tot_dam = 0;
    int tot_mul = 0;
    int tot_to_d = 0;
    int blows = NUM_BLOWS(hand);
    int to_d = p_ptr->weapon_info[hand].to_d * 100;
    int r = row, c = col;
    critical_t crit;

    sprintf(buf, "%-15s %6s %6s %7s", "Attack", "Dice", "Pct", "Dam");
    c_put_str(TERM_YELLOW, buf, r++, c);

    _get_attack_counts(tot, counts, hand);
    for (i = 0; i < MAX_MA; i++)
    {
        martial_arts *ma_ptr = &ma_blows[i];
        int dd = ma_ptr->dd + p_ptr->weapon_info[hand].to_dd;
        int ds = ma_ptr->ds + p_ptr->weapon_info[hand].to_ds;
        char tmp[20];
        int dam = dd * (ds + 1) * 100 * counts[i].count / (2 * tot);

    /*    if (counts[i].count == 0) continue; */

        tot_dam += dam;
        tot_mul += counts[i].mul;
        tot_to_d += counts[i].to_d;

        sprintf(tmp, "%dd%d", dd, ds);
        sprintf(buf, "%-15s %6s %3d.%02d%% %3d.%02d", ma_ptr->name, tmp, counts[i].count/100, counts[i].count%100, dam/100, dam%100);
        put_str(buf, r++, c);
    }

    sprintf(buf, "%20s %3d.%02d  +%3d", "Total:", tot_dam/100, tot_dam%100, to_d/100);
    put_str(buf, r++, c + 10);

    crit.mul = tot_mul/tot;
    crit.to_d = tot_to_d*100/tot;
    sprintf(buf, "%20s %3d.%02dx +%3d.%02d", "Criticals:", crit.mul/100, crit.mul%100, crit.to_d/100, crit.to_d%100);
    put_str(buf, r++, c + 10);

    /* Account for criticals in all that follows ... */
    tot_dam = tot_dam * crit.mul/100;
    to_d = to_d + crit.to_d;
    sprintf(buf, "%20s %3d.%02d  +%3d.%02d", "One Strike:", tot_dam/100, tot_dam%100, to_d/100, to_d%100);
    put_str(buf, r++, c + 10);

    r = row;
    c = col + 40;

    c_put_str(TERM_YELLOW, "Your Fists", r++, c);

    sprintf(buf, "Number of Blows: %d.%2.2d", blows/100, blows%100);
    put_str(buf, r++, c);

    sprintf(buf, "To Hit:  0  50 100 150 200 (AC)");
    put_str(buf, r++, c);

    sprintf(buf, "        %2d  %2d  %2d  %2d  %2d (%%)", 
        hit_chance(0, 0, 0), 
        hit_chance(0, 0, 50), 
        hit_chance(0, 0, 100), 
        hit_chance(0, 0, 150), 
        hit_chance(0, 0, 200)
    );

    put_str(buf, r++, c);

    r++;
    c_put_str(TERM_YELLOW, "Average Damage:", r++, c);
    sprintf(buf, "One Strike: %d.%02d", (tot_dam + to_d)/100, (tot_dam + to_d)%100);
    put_str(buf, r++, c+1);
    sprintf(buf, "One Attack: %d.%02d", blows*(tot_dam + to_d)/10000, ((blows*(tot_dam + to_d))/100)%100);
    put_str(buf, r++, c+1);

    if (have_flag(p_ptr->weapon_info[hand].flags, TR_BRAND_ACID))
    {
        sprintf(buf, " %d.%02d", blows*(tot_dam*17/10 + to_d)/10000, ((blows*(tot_dam*17/10 + to_d))/100)%100);
        c_put_str(TERM_RED, "      Acid:", r, c+1);
        put_str(buf, r++, c+12);
    }
    if (have_flag(p_ptr->weapon_info[hand].flags, TR_BRAND_ELEC))
    {
        sprintf(buf, " %d.%02d", blows*(tot_dam*17/10 + to_d)/10000, ((blows*(tot_dam*17/10 + to_d))/100)%100);
        c_put_str(TERM_RED, "      Elec:", r, c+1);
        put_str(buf, r++, c+12);
    }
    if (have_flag(p_ptr->weapon_info[hand].flags, TR_BRAND_FIRE))
    {
        sprintf(buf, " %d.%02d", blows*(tot_dam*17/10 + to_d)/10000, ((blows*(tot_dam*17/10 + to_d))/100)%100);
        c_put_str(TERM_RED, "      Fire:", r, c+1);
        put_str(buf, r++, c+12);
    }
    if (have_flag(p_ptr->weapon_info[hand].flags, TR_BRAND_COLD))
    {
        sprintf(buf, " %d.%02d", blows*(tot_dam*17/10 + to_d)/10000, ((blows*(tot_dam*17/10 + to_d))/100)%100);
        c_put_str(TERM_RED, "      Cold:", r, c+1);
        put_str(buf, r++, c+12);
    }
    if (have_flag(p_ptr->weapon_info[hand].flags, TR_BRAND_POIS))
    {
        sprintf(buf, " %d.%02d", blows*(tot_dam*17/10 + to_d)/10000, ((blows*(tot_dam*17/10 + to_d))/100)%100);
        c_put_str(TERM_RED, "      Pois:", r, c+1);
        put_str(buf, r++, c+12);
    }
}

static bool _monk_check_spell(void)
{
    if (p_ptr->pclass == CLASS_WILD_TALENT)
        return TRUE;
    if (p_ptr->prace == RACE_MON_POSSESSOR && !p_ptr->weapon_ct)
        return TRUE;
    if (!p_ptr->weapon_info[0].bare_hands)
    {
        msg_print("You need to fight bare handed.");
        return FALSE;
    }
    if (p_ptr->riding)
    {
        msg_print("You need to get off your pet.");
        return FALSE;
    }
    return TRUE;
}

static bool choose_kamae(void)
{
    char choice;
    int new_kamae = 0;
    int i;
    char buf[80];

    if (p_ptr->confused)
    {
        msg_print("You are too confused.");
        return FALSE;
    }

    screen_save();

    prt(" a) No form", 2, 20);
    for (i = 0; i < MAX_KAMAE; i++)
    {
        if (p_ptr->lev >= kamae_shurui[i].min_level)
        {
            sprintf(buf," %c) %-12s  %s",I2A(i+1), kamae_shurui[i].desc, kamae_shurui[i].info);
            prt(buf, 3+i, 20);
        }
    }

    prt("", 1, 0);
    prt("        Choose Form: ", 1, 14);

    while(1)
    {
        choice = inkey();
        if (choice == ESCAPE)
        {
            screen_load();
            return FALSE;
        }
        else if ((choice == 'a') || (choice == 'A'))
        {
            if (p_ptr->action == ACTION_KAMAE)
                set_action(ACTION_NONE);
            else
                msg_print("You are not assuming a posture.");
            screen_load();
            return TRUE;
        }
        else if ((choice == 'b') || (choice == 'B'))
        {
            new_kamae = 0;
            break;
        }
        else if (((choice == 'c') || (choice == 'C')) && (p_ptr->lev > 29))
        {
            new_kamae = 1;
            break;
        }
        else if (((choice == 'd') || (choice == 'D')) && (p_ptr->lev > 34))
        {
            new_kamae = 2;
            break;
        }
        else if (((choice == 'e') || (choice == 'E')) && (p_ptr->lev > 39))
        {
            new_kamae = 3;
            break;
        }
    }
    set_action(ACTION_KAMAE);

    if (p_ptr->special_defense & (KAMAE_GENBU << new_kamae))
        msg_print("You reassume a posture.");
    else
    {
        p_ptr->special_defense &= ~(KAMAE_MASK);
        p_ptr->update |= (PU_BONUS);
        p_ptr->redraw |= (PR_STATE);
        msg_format("You assume a posture of %s form.",kamae_shurui[new_kamae].desc);
        p_ptr->special_defense |= (KAMAE_GENBU << new_kamae);
    }
    p_ptr->redraw |= PR_STATE;
    screen_load();
    return TRUE;
}

void monk_double_attack_spell(int cmd, variant *res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Double Attack");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attack twice at an adjacent enemy. This action consumes double the normal energy.");
        break;
    case SPELL_CAST:
        var_set_bool(res, FALSE);
        if (_monk_check_spell())
        {
            int x, y, dir = 0;

            if (!get_rep_dir(&dir, FALSE)) return;

            y = py + ddy[dir];
            x = px + ddx[dir];
            if (cave[y][x].m_idx)
            {
                if (one_in_(2)) msg_print("Ahhhtatatatatatatatatatatatatatataatatatatattaaaaa!!!!");
                else msg_print("Oraoraoraoraoraoraoraoraoraoraoraoraoraoraoraoraora!!!!");

                py_attack(y, x, 0);
                if (cave[y][x].m_idx)
                {
                    handle_stuff();
                    py_attack(y, x, 0);
                }
            }
            else
            {
                msg_print("You don't see any monster in this direction");
                msg_print(NULL);
            }
            var_set_bool(res, TRUE);
        }
        break;
    case SPELL_ENERGY:
        var_set_int(res, 100 + ENERGY_NEED());
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void monk_posture_spell(int cmd, variant *res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Assume a Posture");
        break;
    case SPELL_DESC:
        var_set_string(res, "");
        break;
    case SPELL_CAST:
        var_set_bool(res, FALSE);
        if ( _monk_check_spell()
          && choose_kamae() )
        {
            p_ptr->update |= (PU_BONUS);
            var_set_bool(res, TRUE);
        }
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static int _get_powers(spell_info* spells, int max)
{
    int ct = 0;
    
    spell_info* spell = &spells[ct++];
    spell->level = 25;
    spell->cost = 0;
    spell->fail = 0;
    spell->fn = monk_posture_spell;

    spell = &spells[ct++];
    spell->level = 30;
    spell->cost = 30;
    spell->fail = calculate_fail_rate(spell->level, 80, p_ptr->stat_ind[A_STR]);
    spell->fn = monk_double_attack_spell;

    return ct;
}

static void _ac_bonus_imp(int slot)
{
    if (!equip_obj(slot))
    {
        switch (equip_slot_type(slot))
        {
        case EQUIP_SLOT_BODY_ARMOR:
            p_ptr->to_a += p_ptr->lev*3/2;
            p_ptr->dis_to_a += p_ptr->lev*3/2;
            break;
        case EQUIP_SLOT_CLOAK:
            if (p_ptr->lev > 15)
            {
                p_ptr->to_a += (p_ptr->lev - 13)/3;
                p_ptr->dis_to_a += (p_ptr->lev - 13)/3;
            }
            break;
        case EQUIP_SLOT_WEAPON_SHIELD: /* Oops: was INVEN_LARM only and "/3" ... */
            if (p_ptr->lev > 10)
            {
                p_ptr->to_a += (p_ptr->lev - 8)/6;
                p_ptr->dis_to_a += (p_ptr->lev - 8)/6;
            }
            break;
        case EQUIP_SLOT_HELMET:
            if (p_ptr->lev >= 5)
            {
                p_ptr->to_a += (p_ptr->lev - 2)/3;
                p_ptr->dis_to_a += (p_ptr->lev - 2)/3;
            }
            break;
        case EQUIP_SLOT_GLOVES:
            p_ptr->to_a += p_ptr->lev/2;
            p_ptr->dis_to_a += p_ptr->lev/2;
            break;
        case EQUIP_SLOT_BOOTS:
            p_ptr->to_a += p_ptr->lev/3;
            p_ptr->dis_to_a += p_ptr->lev/3;
            break;
        }
    }
}

void monk_ac_bonus(void)
{
    if (!(heavy_armor()))
        equip_for_each_slot(_ac_bonus_imp);
}

void monk_posture_calc_bonuses(void)
{
    int i;
    if (!heavy_armor() && p_ptr->pclass != CLASS_WILD_TALENT)
    {
        if (p_ptr->special_defense & KAMAE_BYAKKO)
        {
            p_ptr->to_a -= 40;
            p_ptr->dis_to_a -= 40;
        }
        else if (p_ptr->special_defense & KAMAE_SEIRYU)
        {
            p_ptr->to_a -= 50;
            p_ptr->dis_to_a -= 50;
            res_add(RES_ACID);
            res_add(RES_FIRE);
            res_add(RES_ELEC);
            res_add(RES_COLD);
            res_add(RES_POIS);
            p_ptr->sh_fire = TRUE;
            p_ptr->sh_elec = TRUE;
            p_ptr->sh_cold = TRUE;
            p_ptr->levitation = TRUE;
        }
        else if (p_ptr->special_defense & KAMAE_GENBU)
        {
            p_ptr->to_a += (p_ptr->lev*p_ptr->lev)/50;
            p_ptr->dis_to_a += (p_ptr->lev*p_ptr->lev)/50;
            p_ptr->reflect = TRUE;
            for (i = 0; i < MAX_HANDS; i++)
            {
                p_ptr->weapon_info[i].xtra_blow -= 200;
                if (p_ptr->lev > 42) 
                    p_ptr->weapon_info[i].xtra_blow -= 100;
            }
        }
        else if (p_ptr->special_defense & KAMAE_SUZAKU)
        {
            for (i = 0; i < MAX_HANDS; i++)
            {
                p_ptr->weapon_info[i].to_h -= (p_ptr->lev / 3);
                p_ptr->weapon_info[i].to_d -= (p_ptr->lev / 6);

                p_ptr->weapon_info[i].dis_to_h -= (p_ptr->lev / 3);
                p_ptr->weapon_info[i].dis_to_d -= (p_ptr->lev / 6);
                p_ptr->weapon_info[i].base_blow /= 2;
            }
            p_ptr->levitation = TRUE;
        }
    }
}

static void _calc_bonuses(void)
{
    if (!heavy_armor())
    {
        p_ptr->pspeed += p_ptr->lev/10;
        p_ptr->sh_retaliation = TRUE;
        if  (p_ptr->lev >= 25)
            p_ptr->free_act = TRUE;
    }
    monk_ac_bonus();
}

static void _get_flags(u32b flgs[TR_FLAG_SIZE])
{
    if (!heavy_armor())
    {
        if (p_ptr->lev >= 10)
            add_flag(flgs, TR_SPEED);
        if (p_ptr->lev >= 25)
            add_flag(flgs, TR_FREE_ACT);
    }
}

static caster_info *_caster_info(void)
{
    static caster_info me = {0};
    static bool init = FALSE;
    if (!init)
    {
        me.magic_desc = "prayer";
        me.which_stat = A_WIS;
        me.weight = 350;
        me.min_fail = 5;
        me.options = CASTER_ALLOW_DEC_MANA;
        init = TRUE;
    }
    return &me;
}

class_t *monk_get_class_t(void)
{
    static class_t me = {0};
    static bool init = FALSE;

    if (!init)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 45,  34,  36,   5,  32,  24,  64,  60};
    skills_t xs = { 15,  11,  10,   0,   0,   0,  18,  18};

        me.name = "Monk";
        me.desc = "The Monk character class is very different from all other classes. "
                    "Their training in martial arts makes them much more powerful with "
                    "no armor or weapons. To gain the resistances necessary for "
                    "survival a monk may need to wear some kind of armor, but if the "
                    "armor he wears is too heavy, it will severely disturb his martial "
                    "arts maneuvers. As the monk advances levels, new, powerful forms "
                    "of attack become available. Their defensive capabilities increase "
                    "likewise, but if armour is being worn, this effect decreases. "
                    "Wisdom determines a Monk's spell casting ability.\n \n"
                    "The different sects of monks are devoted to different areas of "
                    "magic. They select a realm from Life, Nature, Craft, Trump and Death. "
                    "They will eventually learn all prayers in the discipline of their "
                    "choice. They have two class powers - 'Assume a Posture' and "
                    "'Double Attack'. They can choose different forms of postures in "
                    "different situations, and use powerful combinations of attacks for "
                    "the finishing blow.";

        me.stats[A_STR] =  2;
        me.stats[A_INT] = -1;
        me.stats[A_WIS] =  1;
        me.stats[A_DEX] =  3;
        me.stats[A_CON] =  2;
        me.stats[A_CHR] =  1;
        me.base_skills = bs;
        me.extra_skills = xs;
        me.life = 110;
        me.base_hp = 12;
        me.exp = 130;
        me.pets = 35;
        
        me.calc_bonuses = _calc_bonuses;
        me.get_flags = _get_flags;
        me.caster_info = _caster_info;
        /* TODO: This class uses spell books, so we are SOL
        me.get_spells = _get_spells;*/
        me.get_powers = _get_powers;
        me.character_dump = spellbook_character_dump;
        init = TRUE;
    }

    return &me;
}
