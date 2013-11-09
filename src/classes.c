/*
 *  Hooks and Callbacks for various classes
 */

#include "angband.h"

int get_class_idx(cptr name)
{
    int i;
    for (i = 0; i < MAX_CLASS; i++)
    {
        if (strcmp(name, get_class_t_aux(i, 0)->name) == 0)
            return i;
    }
    return -1;
}

/* Goal: This should be the one and only switch off of p_ptr->pclass in the
   entire system! */
class_t *get_class_t_aux(int pclass, int psubclass)
{
class_t *result = NULL;

    switch (pclass)
    {
    case CLASS_ARCHAEOLOGIST:
        result = archaeologist_get_class_t();
        break;
    case CLASS_ARCHER:
        result = archer_get_class_t();
        break;
    case CLASS_BARD:
        result = bard_get_class_t();
        break;
    case CLASS_BEASTMASTER:
        result = beastmaster_get_class_t();
        break;
    case CLASS_BERSERKER:
        result = berserker_get_class_t();
        break;
    case CLASS_BLUE_MAGE:
        result = blue_mage_get_class_t();
        break;
    case CLASS_BLOOD_KNIGHT:
        result = blood_knight_get_class_t();
        break;
    case CLASS_BLOOD_MAGE:
        result = blood_mage_get_class_t();
        break;
    case CLASS_CAVALRY:
        result = cavalry_get_class_t();
        break;
    case CLASS_CHAOS_WARRIOR:
        result = chaos_warrior_get_class_t();
        break;
    case CLASS_DEVICEMASTER:
        result = devicemaster_get_class_t();
        break;
    case CLASS_DUELIST:
        result = duelist_get_class_t();
        break;
    case CLASS_FORCETRAINER:
        result = force_trainer_get_class_t();
        break;
    case CLASS_HIGH_MAGE:
        result = high_mage_get_class_t();
        break;
    case CLASS_IMITATOR:
        result = imitator_get_class_t();
        break;
    case CLASS_MAGE:
        result = mage_get_class_t();
        break;
    case CLASS_MAGIC_EATER:
        result = magic_eater_get_class_t();
        break;
    case CLASS_MAULER:
        result = mauler_get_class_t();
        break;
    case CLASS_MINDCRAFTER:
        result = mindcrafter_get_class_t();
        break;
    case CLASS_MIRROR_MASTER:
        result = mirror_master_get_class_t();
        break;
    case CLASS_MONK:
        result = monk_get_class_t();
        break;
    case CLASS_MONSTER:
        result = monster_get_class_t();
        break;
    case CLASS_MYSTIC:
        result = mystic_get_class_t();
        break;
    case CLASS_NECROMANCER:
        result = necromancer_get_class_t();
        break;
    case CLASS_NINJA:
        result = ninja_get_class_t();
        break;
    case CLASS_PALADIN:
        result = paladin_get_class_t();
        break;
    case CLASS_PRIEST:
        result = priest_get_class_t();
        break;
    case CLASS_PSION:
        result = psion_get_class_t();
        break;
    case CLASS_RANGER:
        result = ranger_get_class_t();
        break;
    case CLASS_RAGE_MAGE:
        result = rage_mage_get_class_t();
        break;
    case CLASS_RED_MAGE:
        result = red_mage_get_class_t();
        break;
    case CLASS_ROGUE:
        result = rogue_get_class_t();
        break;
    case CLASS_RUNE_KNIGHT:
        result = rune_knight_get_class_t();
        break;
    case CLASS_SAMURAI:
        result = samurai_get_class_t();
        break;
    case CLASS_SCOUT:
        result = scout_get_class_t();
        break;
    case CLASS_SNIPER:
        result = sniper_get_class_t();
        break;
    case CLASS_SORCERER:
        result = sorcerer_get_class_t();
        break;
    case CLASS_TIME_LORD:
        result = time_lord_get_class_t();
        break;
    case CLASS_TOURIST:
        result = tourist_get_class_t();
        break;
    case CLASS_WARLOCK:
        result = warlock_get_class_t(psubclass);
        break;
    case CLASS_WARRIOR:
        result = warrior_get_class_t();
        break;
    case CLASS_WARRIOR_MAGE:
        result = warrior_mage_get_class_t();
        break;
    case CLASS_WEAPONSMITH:
        result = weaponsmith_get_class_t();
        break;
    case CLASS_WEAPONMASTER:
        result = weaponmaster_get_class_t();
        break;
    case CLASS_WILD_TALENT:
        result = wild_talent_get_class_t();
        break;
    }

    return result;
}

class_t *get_class_t(void)
{
    return get_class_t_aux(p_ptr->pclass, p_ptr->psubclass);
}

caster_info *get_caster_info(void)
{
    caster_info *result = NULL;
    class_t *class_ptr = get_class_t();
    race_t *race_ptr = get_race_t();

    if (race_ptr->caster_info) /* Monster Races: Lich, Angel, Demon */
        result = (race_ptr->caster_info)();
    else if (class_ptr->caster_info)
        result = (class_ptr->caster_info)();
    return result;
}

/* 
Helper for getting powers

Sample usage:
    static power_info _powers[] =
    {
        { A_WIS, {15, 0, 30, clear_mind_spell}}, 
        { -1, {-1, -1, -1, NULL}}
    };
    ...
    static int _get_powers(spell_info* spells, int max)
    {
        return get_powers_aux(spells, max, _powers);
    }
    ...
    class_t *mindcrafter_get_class_t(void)
    {
    ...
        me.get_powers = _get_powers;
    ...
    }
*/
int get_powers_aux(spell_info* spells, int max, power_info* table)
{
    int i;
    int ct = 0;
    
    for (i = 0; ; i++)
    {
        power_info *base = &table[i];
        
        if (ct >= max) break;
        if (!base->spell.fn) break;

    /*    if (base->spell.level <= p_ptr->lev) */
        {
            spell_info* current = &spells[ct];
            current->fn = base->spell.fn;
            current->level = base->spell.level;
            current->cost = base->spell.cost;

            current->fail = calculate_fail_rate(
                base->spell.level, 
                base->spell.fail, 
                p_ptr->stat_ind[base->stat]
            );            
            ct++;
        }
    }
    return ct;
}

int get_spells_aux(spell_info* spells, int max, spell_info* table)
{
    int i;
    int ct = 0;
    caster_info *caster_ptr = get_caster_info();
    int idx = p_ptr->stat_ind[caster_ptr->which_stat];

    for (i = 0; ; i++)
    {
        spell_info *base = &table[i];
        if (ct >= max) break;
        if (!base->fn) break;

        if (base->level <= p_ptr->lev)
        {
            spell_info* current = &spells[ct];
            current->fn = base->fn;
            current->level = base->level;
            current->cost = base->cost;

            current->fail = calculate_fail_rate(base->level, base->fail, idx);
            ct++;
        }
    }
    return ct;
}

void dump_spells_aux(FILE *fff, spell_info *table, int ct)
{        
    int i;
    variant vn, vd, vc;

    if (!ct) return;

    var_init(&vn);
    var_init(&vd);
    var_init(&vc);

    fprintf(fff, "=================================== Spells ====================================\n\n");
    fprintf(fff, "%-20.20s Lvl Cost Fail Desc\n", "");
    for (i = 0; i < ct; i++)
    {
        spell_info *spell = &table[i];        

        spell->fn(SPELL_NAME, &vn);
        spell->fn(SPELL_INFO, &vd);
        spell->fn(SPELL_COST_EXTRA, &vc);

        fprintf(fff, "%-20.20s %3d %4d %3d%% %s\n", 
            var_get_string(&vn), 
            spell->level, calculate_cost(spell->cost + var_get_int(&vc)), spell->fail, 
            var_get_string(&vd)
        );
    }

    var_clear(&vn);
    var_clear(&vd);
    var_clear(&vc);
}

void dump_powers_aux(FILE *fff, spell_info *table, int ct)
{        
    int i;
    variant vn, vd, vc;
    if (!ct) return;

    var_init(&vn);
    var_init(&vd);
    var_init(&vc);

    fprintf(fff, "=================================== Powers ====================================\n\n");
    fprintf(fff, "%-20.20s Lvl Cost Fail Desc\n", "");
    for (i = 0; i < ct; i++)
    {
        spell_info *spell = &table[i];        

        spell->fn(SPELL_NAME, &vn);
        spell->fn(SPELL_INFO, &vd);
        spell->fn(SPELL_COST_EXTRA, &vc);

        fprintf(fff, "%-20.20s %3d %4d %3d%% %s\n", 
            var_get_string(&vn), 
            spell->level, calculate_cost(spell->cost + var_get_int(&vc)), spell->fail, 
            var_get_string(&vd)
        );
    }

    var_clear(&vn);
    var_clear(&vd);
    var_clear(&vc);
}

static void _dump_book(FILE *fff, int realm, int book)
{
    int          k_idx = lookup_kind(realm2tval(realm), book);
    int          i, increment = 64;
    caster_info *caster_ptr = get_caster_info();

    if ((p_ptr->pclass == CLASS_SORCERER) || (p_ptr->pclass == CLASS_RED_MAGE)) increment = 0;
    else if (realm == p_ptr->realm1) increment = 0;
    else if (realm == p_ptr->realm2) increment = 32;

    if (realm == REALM_HISSATSU)
        fprintf(fff, "     %-25.25s Lvl  SP Desc\n", k_name + k_info[k_idx].name);
    else
    {
        if (caster_ptr && (caster_ptr->options & CASTER_USE_HP))
            fprintf(fff, "     %-23.23s Profic Lvl  HP Fail Desc\n", k_name + k_info[k_idx].name);
        else
            fprintf(fff, "     %-23.23s Profic Lvl  SP Fail Desc\n", k_name + k_info[k_idx].name);
    }

    for (i = 0; i < 8; i++)
    {
        int         s_idx = book * 8 + i;
        magic_type *s_ptr;
        int         cost;
        bool        max = FALSE;
        char        proficiency[10];
        char        info[80];
        cptr        comment;
        char        line[160];

        if (is_magic(realm))
            s_ptr = &mp_ptr->info[realm - 1][s_idx];
        else
            s_ptr = &technic_info[realm - MIN_TECHNIC][s_idx];

        if (s_ptr->slevel >= 99) continue;

        if (realm == REALM_HISSATSU)
            cost = s_ptr->smana;
        else
        {
            s16b exp = experience_of_spell(s_idx, realm);
            int  exp_level = spell_exp_level(exp);
            
            cost = mod_need_mana(s_ptr->smana, s_idx, realm);

            max = FALSE;
            if (!increment && (exp_level == EXP_LEVEL_MASTER)) max = TRUE;
            else if ((increment == 32) && (exp_level >= EXP_LEVEL_EXPERT)) max = TRUE;
            else if ((p_ptr->pclass == CLASS_RED_MAGE) && (exp_level >= EXP_LEVEL_SKILLED)) max = TRUE;

            strncpy(proficiency, exp_level_str[exp_level], 4);
            proficiency[3] = ']';
            proficiency[4] = '\0';
        }

        strcpy(info, do_spell(realm, s_idx, SPELL_INFO));
        comment = info;
        
        if (p_ptr->pclass == CLASS_SORCERER || p_ptr->pclass == CLASS_RED_MAGE)
        {
            if (s_ptr->slevel > p_ptr->max_plv)
                comment = "unknown";
            else if (s_ptr->slevel > p_ptr->lev)
                comment = "forgotten";
        }
        else if ((realm == p_ptr->realm1) ?
            ((p_ptr->spell_forgotten1 & (1L << s_idx))) :
            ((p_ptr->spell_forgotten2 & (1L << s_idx))))
        {
            comment = "forgotten";
        }
        else if (!((realm == p_ptr->realm1) ?
            (p_ptr->spell_learned1 & (1L << s_idx)) :
            (p_ptr->spell_learned2 & (1L << s_idx))))
        {
            comment = "unknown";
        }
        else if (!((realm == p_ptr->realm1) ?
            (p_ptr->spell_worked1 & (1L << s_idx)) :
            (p_ptr->spell_worked2 & (1L << s_idx))))
        {
            comment = "untried";
        }

        sprintf(line, "  %c) ", I2A(i));
        if (realm == REALM_HISSATSU)
        {
            strcat(line, format("%-25s %3d %3d %s",
                do_spell(realm, s_idx, SPELL_NAME),
                s_ptr->slevel, cost, comment));
        }
        else
        {
            strcat(line, format("%-25s%c%-4s %3d %3d %3d%% %s",
                do_spell(realm, s_idx, SPELL_NAME),
                (max ? '!' : ' '), proficiency,
                s_ptr->slevel, cost, spell_chance(s_idx, realm), comment));
        }

        fprintf(fff, "%s\n", line);
    }
    fprintf(fff, "\n");
}

static bool _has_spells(int realm, int book)
{
    int i;
    for (i = 0; i < 8; i++)
    {
        int            s_idx = book * 8 + i;
        magic_type *s_ptr;

        if (is_magic(realm))
            s_ptr = &mp_ptr->info[realm - 1][s_idx];
        else
            s_ptr = &technic_info[realm - MIN_TECHNIC][s_idx];

        if (s_ptr->slevel >= 99) continue;

        if (p_ptr->pclass == CLASS_SORCERER || p_ptr->pclass == CLASS_RED_MAGE)
        {
            if (s_ptr->slevel > p_ptr->max_plv) continue;
            else if (s_ptr->slevel > p_ptr->lev) return TRUE;
        }
        else if ((realm == p_ptr->realm1) ?
            ((p_ptr->spell_forgotten1 & (1L << s_idx))) :
            ((p_ptr->spell_forgotten2 & (1L << s_idx))))
        {
            return TRUE;
        }
        else if (realm == p_ptr->realm1 ?
                    (p_ptr->spell_learned1 & (1L << s_idx)) :
                    (p_ptr->spell_learned2 & (1L << s_idx)))
        {
            return TRUE;
        }
    }
    return FALSE;
}

static bool _has_book(int realm, int book)
{
    int tval = realm2tval(realm);
    int sval = book;
    int i;

    for (i = 0; i < INVEN_PACK; i++)
    {
        if (inventory[i].tval == tval && inventory[i].sval == sval)
            return TRUE;
    }
    return FALSE;
}

static void _dump_realm(FILE *fff, int realm)
{
    int i;
    bool first = TRUE;
    for (i = 0; i < 4; i++)
    {
        bool dump = FALSE;

        /* Red Mages only learn from first 2 books (except for Arcane) */
        if (p_ptr->pclass == CLASS_RED_MAGE && realm != REALM_ARCANE && i > 1) break;

        /* Necromancy probably should be a technique ... Too late now :( */
        if (p_ptr->pclass == CLASS_RED_MAGE && realm == REALM_NECROMANCY) break;

        /* Red Mages and Sorcerers don't learn spells, so make sure the user actually
           has the book in their pack (otherwise we spoil ... ) */
        if (p_ptr->pclass == CLASS_RED_MAGE || p_ptr->pclass == CLASS_SORCERER)
            dump = _has_book(realm, i);
        else
            dump = _has_spells(realm, i) || _has_book(realm, i);

        if (dump)
        {
            if (first)
            {
                fprintf(fff, "Realm: %s\n\n", realm_names[realm]);
                first = FALSE;    
            }
            _dump_book(fff, realm, i);
        }
    }
}

void spellbook_character_dump(FILE *fff)
{
    fprintf(fff, "\n==================================== Spells ===================================\n\n");

    if (p_ptr->pclass == CLASS_RED_MAGE || p_ptr->pclass == CLASS_SORCERER)
    {
        int realm;
        for (realm = REALM_LIFE; realm <= MAX_MAGIC; realm++)
            _dump_realm(fff, realm);
    }
    else
    {
        if (p_ptr->realm1)
            _dump_realm(fff, p_ptr->realm1);
        if (p_ptr->realm2)
            _dump_realm(fff, p_ptr->realm2);
    }

    if (p_ptr->old_realm)
    {
        int i;
        for (i = 0; i < MAX_MAGIC; i++)
        {
            if (!(p_ptr->old_realm & 1L << i)) continue;
            fprintf(fff, "\n You were able to use %s magic before.", realm_names[i+1]);
        }
        fputc('\n', fff);
    }
}

