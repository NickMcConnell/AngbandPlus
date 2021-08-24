#include "angband.h"

#include <assert.h>

static bool _is_martial_arts(void)
{
    int i;
    for (i = 0; i < MAX_HANDS; i++)
        if (plr->attack_info[i].type == PAT_MONK) return TRUE;
    return FALSE;
}
static bool _monk_check_spell(void)
{
    if (plr->pclass == CLASS_WILD_TALENT || mut_present(MUT_DRACONIAN_METAMORPHOSIS))
        return TRUE;
    if ((plr->prace == RACE_MON_POSSESSOR || plr->prace == RACE_MON_MIMIC) && !plr->weapon_ct)
        return TRUE;
    if (!_is_martial_arts())
    {
        msg_print("You need to fight bare handed.");
        return FALSE;
    }
    if (plr->riding)
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
    rect_t display = ui_menu_rect();
    char buf[80];

    if (display.cx > 40)
        display.cx = 40;

    if (plr_tim_find(T_CONFUSED))
    {
        msg_print("You are too confused.");
        return FALSE;
    }

    screen_save();

    Term_erase(display.x, display.y, display.cx);
    put_str("Choose Form: ", display.y, display.x + 1);

    Term_erase(display.x, display.y + 1, display.cx);
    put_str(" a) No form", display.y + 1, display.x + 1);

    for (i = 0; i < MAX_KAMAE; i++)
    {
        if (plr->lev >= kamae_shurui[i].min_level)
        {
            sprintf(buf," %c) %-12s  %s",I2A(i+1), kamae_shurui[i].desc, kamae_shurui[i].info);
            Term_erase(display.x, display.y + 2 + i, display.cx);
            put_str(buf, display.y + 2 + i, display.x + 1);
        }
    }

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
            if (plr->action == ACTION_KAMAE)
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
        else if (((choice == 'c') || (choice == 'C')) && (plr->lev > 29))
        {
            new_kamae = 1;
            break;
        }
        else if (((choice == 'd') || (choice == 'D')) && (plr->lev > 34))
        {
            new_kamae = 2;
            break;
        }
        else if (((choice == 'e') || (choice == 'E')) && (plr->lev > 39))
        {
            new_kamae = 3;
            break;
        }
    }
    set_action(ACTION_KAMAE);

    if (plr->special_defense & (KAMAE_GENBU << new_kamae))
        msg_print("You reassume a posture.");
    else
    {
        plr->special_defense &= ~(KAMAE_MASK);
        plr->update |= (PU_BONUS);
        plr->redraw |= (PR_STATE);
        msg_format("You assume a posture of %s form.",kamae_shurui[new_kamae].desc);
        plr->special_defense |= (KAMAE_GENBU << new_kamae);
    }
    plr->redraw |= PR_STATE;
    screen_load();
    return TRUE;
}

static bool _double_attack_begin(plr_attack_ptr context)
{
    if (context->info.type != PAT_MONK) return TRUE;
    context->info.base_blow *= 2;
    context->info.xtra_blow *= 2;
    if (!(context->flags & PAC_DISPLAY))
    {
        if (one_in_(2)) msg_print("Ahhhtatatatatatatatatatatatatatataatatatatattaaaaa!!!!");
        else msg_print("Oraoraoraoraoraoraoraoraoraoraoraoraoraoraoraoraora!!!!");
    }
    return TRUE;
}
static void _double_attack_end(plr_attack_ptr context)
{
    if (context->info.type != PAT_MONK) return;
    context->info.base_blow /= 2;
    context->info.xtra_blow /= 2;
}
void monk_double_attack_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Double Attack");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attack an adjacent enemy with twice the normal number of attacks. This action consumes double the normal energy.");
        break;
    case SPELL_CAST:
    case SPELL_ON_BROWSE: {
        plr_attack_t context = {0};
        context.hooks.begin_weapon_f = _double_attack_begin;
        context.hooks.end_weapon_f = _double_attack_end;
        var_set_bool(res, FALSE);
        if (cmd == SPELL_CAST)
        {
            if (_monk_check_spell())
                var_set_bool(res, plr_attack_special_aux(&context, 1));
        }
        else
        {
            plr_attack_display_aux(&context);
            var_set_bool(res, TRUE);
        }
        break; }
    case SPELL_ENERGY:
        var_set_int(res, 100 + ENERGY_NEED());
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _posture_spell(int cmd, var_ptr res)
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
            plr->update |= (PU_BONUS);
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
    spell->fn = _posture_spell;

    spell = &spells[ct++];
    spell->level = 30;
    spell->cost = 30;
    spell->fail = calculate_fail_rate(spell->level, 80, plr->stat_ind[A_STR]);
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
            plr->to_a += plr->monk_lvl*3/2;
            plr->dis_to_a += plr->monk_lvl*3/2;
            break;
        case EQUIP_SLOT_CLOAK:
            if (plr->lev > 15)
            {
                plr->to_a += (plr->monk_lvl - 13)/3;
                plr->dis_to_a += (plr->monk_lvl - 13)/3;
            }
            break;
        case EQUIP_SLOT_WEAPON_SHIELD: /* Oops: was INVEN_LARM only and "/3" ... */
            if (plr->lev > 10)
            {
                plr->to_a += (plr->monk_lvl - 8)/6;
                plr->dis_to_a += (plr->monk_lvl - 8)/6;
            }
            break;
        case EQUIP_SLOT_HELMET:
            if (plr->lev >= 5)
            {
                plr->to_a += (plr->monk_lvl - 2)/3;
                plr->dis_to_a += (plr->monk_lvl - 2)/3;
            }
            break;
        case EQUIP_SLOT_GLOVES:
            plr->to_a += plr->monk_lvl/2;
            plr->dis_to_a += plr->monk_lvl/2;
            break;
        case EQUIP_SLOT_BOOTS:
            plr->to_a += plr->monk_lvl/3;
            plr->dis_to_a += plr->monk_lvl/3;
            break;
        }
    }
}

void monk_ac_bonus(void)
{
    if (!heavy_armor())
        equip_for_each_slot(_ac_bonus_imp);
}

void monk_posture_calc_bonuses(void)
{
    int i;
    if (!heavy_armor())
    {
        if (plr->special_defense & KAMAE_BYAKKO)
        {
            plr->to_a -= 40;
            plr->dis_to_a -= 40;

            /* Didn't this used to give vulnerabilites?? */
            res_add_vuln(GF_ACID);
            res_add_vuln(GF_FIRE);
            res_add_vuln(GF_ELEC);
            res_add_vuln(GF_COLD);
            res_add_vuln(GF_POIS);
        }
        else if (plr->special_defense & KAMAE_SEIRYU)
        {
            plr->to_a -= 50;
            plr->dis_to_a -= 50;
            res_add(GF_ACID);
            res_add(GF_FIRE);
            res_add(GF_ELEC);
            res_add(GF_COLD);
            res_add(GF_POIS);
            plr->sh_fire = TRUE;
            plr->sh_elec = TRUE;
            plr->sh_cold = TRUE;
            plr->levitation = TRUE;
        }
        else if (plr->special_defense & KAMAE_GENBU)
        {
            plr->to_a += (plr->lev*plr->lev)/50;
            plr->dis_to_a += (plr->lev*plr->lev)/50;
            plr->reflect = TRUE;
            for (i = 0; i < MAX_HANDS; i++)
            {
                plr->attack_info[i].xtra_blow -= 200;
                if (plr->lev > 42)
                    plr->attack_info[i].xtra_blow -= 100;
            }
        }
        else if (plr->special_defense & KAMAE_SUZAKU)
        {
            plr->pspeed += 10;
            for (i = 0; i < MAX_HANDS; i++)
            {
                plr->attack_info[i].to_h -= (plr->lev / 3);
                plr->attack_info[i].to_d -= (plr->lev / 6);

                plr->attack_info[i].dis_to_h -= (plr->lev / 3);
                plr->attack_info[i].dis_to_d -= (plr->lev / 6);
                plr->attack_info[i].base_blow /= 2;
            }
            plr->levitation = TRUE;
        }
    }
}

void monk_posture_calc_stats(s16b stats[MAX_STATS])
{
    if (!heavy_armor())
    {
        if (plr->special_defense & KAMAE_BYAKKO)
        {
            stats[A_STR] += 2;
            stats[A_DEX] += 2;
            stats[A_CON] -= 3;
        }
        else if (plr->special_defense & KAMAE_SEIRYU)
        {
        }
        else if (plr->special_defense & KAMAE_GENBU)
        {
            stats[A_INT] -= 1;
            stats[A_WIS] -= 1;
            stats[A_DEX] -= 2;
            stats[A_CON] += 3;
        }
        else if (plr->special_defense & KAMAE_SUZAKU)
        {
            stats[A_STR] -= 2;
            stats[A_INT] += 1;
            stats[A_WIS] += 1;
            stats[A_DEX] += 2;
            stats[A_CON] -= 2;
        }
    }
}

void monk_posture_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    if (plr->special_defense & KAMAE_GENBU)
        add_flag(flgs, OF_REFLECT);
    if (plr->special_defense & KAMAE_SUZAKU)
    {
        add_flag(flgs, OF_LEVITATION);
        add_flag(flgs, OF_SPEED);
    }
    if (plr->special_defense & KAMAE_BYAKKO)
    {
        add_flag(flgs, OF_VULN_(GF_FIRE));
        add_flag(flgs, OF_VULN_(GF_COLD));
        add_flag(flgs, OF_VULN_(GF_ACID));
        add_flag(flgs, OF_VULN_(GF_ELEC));
        add_flag(flgs, OF_VULN_(GF_POIS));
    }
    if (plr->special_defense & KAMAE_SEIRYU)
    {
        add_flag(flgs, OF_RES_(GF_FIRE));
        add_flag(flgs, OF_RES_(GF_COLD));
        add_flag(flgs, OF_RES_(GF_ACID));
        add_flag(flgs, OF_RES_(GF_ELEC));
        add_flag(flgs, OF_RES_(GF_POIS));
        add_flag(flgs, OF_LEVITATION);
        add_flag(flgs, OF_AURA_FIRE);
        add_flag(flgs, OF_AURA_ELEC);
        add_flag(flgs, OF_AURA_COLD);
    }
}

static void _calc_bonuses(void)
{
    plr->monk_lvl = plr->lev;
    monk_posture_calc_bonuses();
    if (!heavy_armor())
    {
        plr->pspeed += plr->lev/10;
        plr->sh_retaliation = TRUE;
        if  (plr->lev >= 25)
            plr->free_act++;
    }
    monk_ac_bonus();
    switch (plr->realm1)
    {
    case REALM_DAEMON:
        plr->monk_tbl = "Monk.Demon";
        break;
    case REALM_CHAOS:
        plr->monk_tbl = "Monk.Chaos";
        break;
    case REALM_DEATH:
        plr->monk_tbl = "Monk.Death";
        break;
    default:
        plr->monk_tbl = "Monk";
    }
}

static void _calc_stats(s16b stats[MAX_STATS])
{
    monk_posture_calc_stats(stats);
}

static void _get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    monk_posture_get_flags(flgs);
    if (!heavy_armor())
    {
        add_flag(flgs, OF_AURA_REVENGE);
        if (plr->lev >= 10)
            add_flag(flgs, OF_SPEED);
        if (plr->lev >= 25)
            add_flag(flgs, OF_FREE_ACT);
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
        me.encumbrance.max_wgt = 350;
        me.encumbrance.weapon_pct = 100;
        me.encumbrance.enc_wgt = 1000;
        me.min_fail = 5;
        me.realm1_choices = CH_LIFE | CH_NATURE | CH_DEATH | CH_ENCHANT | CH_DAEMON | CH_CHAOS;
        init = TRUE;
    }
    return &me;
}

static void _birth(void)
{
    plr_birth_obj_aux(TV_POTION, SV_POTION_HEROISM, randint1(5));
    plr_birth_obj_aux(TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR, 1);
    plr_birth_spellbooks();
}

static void _timer_on(plr_tim_ptr timer)
{
    if (timer->id == T_CONFUSED && plr->action == ACTION_KAMAE)
    {
        msg_print("Your posture gets loose.");
        plr->special_defense &= ~(KAMAE_MASK);
        plr->update |= PU_BONUS;
        plr->redraw |= PR_STATE;
        plr->action = ACTION_NONE;
    }
}

plr_class_ptr monk_get_class(void)
{
    static plr_class_ptr me = NULL;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 45,  34,  36,   5,  32,  24,  64,  60};
    skills_t xs = { 75,  55,  50,   0,   0,   0,  90,  90};

        me = plr_class_alloc(CLASS_MONK);
        me->name = "Monk";
        me->desc = "The Monk character class is very different from all other classes. "
                    "Their training in martial arts makes them much more powerful with "
                    "no armor or weapons. To gain the resistances necessary for "
                    "survival a monk may need to wear some kind of armor, but if the "
                    "armor he wears is too heavy, it will severely disturb his martial "
                    "arts maneuvers. As the monk advances levels, new, powerful forms "
                    "of attack become available. Their defensive capabilities increase "
                    "likewise, but if armour is being worn, this effect decreases. "
                    "Wisdom determines a Monk's spell casting ability.\n \n"
                    "The different sects of monks are devoted to different areas of "
                    "magic. They select a realm from Life, Nature, Craft, Daemon and Death; "
                    "their choice may even influence their martial arts attacks! "
                    "They will eventually learn all prayers in the discipline of their "
                    "choice. They have two class powers - 'Assume a Posture' and "
                    "'Double Attack'. They can choose different forms of postures in "
                    "different situations, and use powerful combinations of attacks for "
                    "the finishing blow.";

        me->stats[A_STR] =  2;
        me->stats[A_INT] = -1;
        me->stats[A_WIS] =  1;
        me->stats[A_DEX] =  3;
        me->stats[A_CON] =  2;
        me->stats[A_CHR] =  1;
        me->skills = bs;
        me->extra_skills = xs;
        me->life = 110;
        me->base_hp = 12;
        me->exp = 130;
        me->pets = 35;
        me->flags = CLASS_SENSE1_MED | CLASS_SENSE1_WEAK |
                    CLASS_SENSE2_SLOW | CLASS_SENSE2_STRONG | CLASS_MARTIAL_ARTS;

        me->hooks.birth = _birth;
        me->hooks.calc_bonuses = _calc_bonuses;
        me->hooks.calc_stats = _calc_stats;
        me->hooks.get_flags = _get_flags;
        me->hooks.caster_info = _caster_info;
        me->hooks.get_powers = _get_powers;
        me->hooks.character_dump = spellbook_character_dump;
        me->hooks.timer_on = _timer_on;
    }

    return me;
}
