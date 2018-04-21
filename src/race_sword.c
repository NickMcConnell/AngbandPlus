#include "angband.h"

static int _rank(void)
{
    int rank = 0;

    if (p_ptr->lev >= 10) rank++;
    if (p_ptr->lev >= 25) rank++;
    if (p_ptr->lev >= 35) rank++;
    if (p_ptr->lev >= 45) rank++;
    return rank;
}

/**********************************************************************
 * Essences
 **********************************************************************/
#define _MAX_ESSENCE 255
#define _MIN_SPECIAL 240  /* cf TR_FLAG_MAX */
enum {
    _ESSENCE_AC = _MIN_SPECIAL,
    _ESSENCE_TO_HIT,
    _ESSENCE_TO_DAM,
    _ESSENCE_XTRA_DICE
};

static int _essences[_MAX_ESSENCE] = {0};

static void _load(savefile_ptr file)
{
    int ct, i;

    for (i = 0; i < _MAX_ESSENCE; i++)
        _essences[i] = 0;

    ct = savefile_read_s16b(file);
    for (i = 0; i < ct; i++)
    {
        int j = savefile_read_s16b(file);
        int n = savefile_read_s16b(file);

        if (0 <= j && j < _MAX_ESSENCE)
            _essences[j] += n;
    }
}

static void _save(savefile_ptr file)
{
    int ct = 0, i;

    for (i = 0; i < _MAX_ESSENCE; i++)
    {
        if (_essences[i])
            ct++;
    }

    savefile_write_s16b(file, ct);

    for (i = 0; i < _MAX_ESSENCE; i++)
    {
        if (_essences[i])
        {
            savefile_write_s16b(file, i);
            savefile_write_s16b(file, _essences[i]);
        }
    }
}

static bool _skip_flag(int which)
{
    switch (which)
    {
    case TR_RIDING:
    case TR_THROW:
    case TR_HIDE_TYPE:
    case TR_SHOW_MODS:
    case TR_IGNORE_ACID:
    case TR_IGNORE_ELEC:
    case TR_IGNORE_FIRE:
    case TR_IGNORE_COLD:
    case TR_ACTIVATE:
    case TR_FULL_NAME:
    case TR_FIXED_FLAVOR:
        return TRUE;
    }
    return FALSE;
}

static bool _add_essence(int which, int amount)
{
    int n = _essences[which];

    if (amount > 0)
        n += amount;

    if (n > 10000)
        n = 10000;

    if (n != _essences[which])
    {
        _essences[which] = n;
        return TRUE;
    }

    return FALSE;
}

static bool _absorb(object_type *o_ptr)
{
    bool result = FALSE;
    int i;
    int div = 1;
    object_kind *k_ptr = &k_info[o_ptr->k_idx];
    u32b flags[TR_FLAG_SIZE];
    object_flags(o_ptr, flags);

    if (o_ptr->curse_flags & TRC_AGGRAVATE)
        div++;
    if (o_ptr->curse_flags & (TRC_TY_CURSE | TRC_HEAVY_CURSE))
        div++;

    if (!have_flag(flags, TR_ORDER) && !have_flag(flags, TR_WILD))
    {
        if (_add_essence(_ESSENCE_XTRA_DICE, (o_ptr->ds - k_ptr->ds)/1/*div?*/))
            result = TRUE;
        if (_add_essence(_ESSENCE_XTRA_DICE, (o_ptr->dd - k_ptr->dd)/1/*div?*/))
            result = TRUE;
    }

    for (i = 0; i < TR_FLAG_MAX; i++)
    {
        if (_skip_flag(i)) continue;
        if (have_flag(flags, i))
        {
            if (is_pval_flag(i))
            {
                if (_add_essence(i, o_ptr->pval/div))
                    result = TRUE;
            }
            else
            {
                _essences[i]++;
                result = TRUE;
            }
        }
    }

    if (_add_essence(_ESSENCE_AC, o_ptr->to_a/div))
        result = TRUE;
    if (_add_essence(_ESSENCE_TO_HIT, o_ptr->to_h/div))
        result = TRUE;
    if (_add_essence(_ESSENCE_TO_DAM, o_ptr->to_d/div))
        result = TRUE;

    if (result)
    {
        p_ptr->update |= PU_BONUS;
        msg_print("You grow stronger!");
    }
    return result;
}

static int _calc_amount(int amount, int power, int rep)
{
    int result = 0;
    int ct = 0;

    while (amount > 0)
    {
        if (amount >= power)
        {
            result++;
            ct++;
        }
        amount -= power;

        if (ct % rep == 0)
        {
            power *= 2;
            if (power > 1024)
                power = 1024;
        }
    }

    return result;
}

static int _calc_needed(int amount, int power, int rep)
{
    int result = 0;
    int ct = 0;
    /* It would be cleaner to loop until _calc_amount increased, but the
       power required increases exponentially! */
    for (;;)
    {
        if (amount >= power)
            ct++;
        result += power;
        amount -= power;
        if (amount < 0)
            break;
        if (ct % rep == 0)
        {
            power *= 2;
            if (power > 1024)
                power = 1024;
        }
    }

    return result;
}

static int _calc_stat_bonus(int flag)
{
    return _calc_amount(_essences[flag], 3, 1);
}

static void _add_stat_flag(int flag, u32b flgs[TR_FLAG_SIZE])
{
    if (_calc_stat_bonus(flag))
        add_flag(flgs, flag);
}

static int _res_power(int which)
{
    switch (which)
    {
    case RES_ACID:
    case RES_FIRE:
    case RES_COLD:
    case RES_ELEC:
    case RES_CONF:
    case RES_FEAR:
    case RES_TIME:
    case RES_TELEPORT:
        return 2;

    case RES_SOUND:
    case RES_SHARDS:
    case RES_CHAOS:
        return 4;

    case RES_DISEN:
    case RES_POIS:
    case RES_LITE:
    case RES_DARK:
    case RES_NETHER:
    case RES_NEXUS:
        return 3;
    }

    return 2;
}

/**********************************************************************
 * Attacks and Bonuses
 **********************************************************************/
typedef struct {
    int flag;
    int power;
    cptr name;
} _flag_info_t;

static _flag_info_t _slay_flag_info[] = {
    { TR_SLAY_EVIL, 64, "Slay Evil" },
    { TR_SLAY_GOOD, 16, "Slay Good" },
    { TR_SLAY_UNDEAD, 16, "Slay Undead" },
    { TR_SLAY_DEMON, 16, "Slay Demon" },
    { TR_SLAY_DRAGON, 16, "Slay Dragon" },
    { TR_SLAY_HUMAN, 16, "Slay Human" },
    { TR_SLAY_ANIMAL, 8, "Slay Animal" },
    { TR_SLAY_ORC, 4, "Slay Orc" },
    { TR_SLAY_TROLL, 8, "Slay Troll" },
    { TR_SLAY_GIANT, 8, "Slay Giant" },
    { TR_SLAY_LIVING, 8, "Slay Living" },

    { TR_BRAND_ACID, 24, "Brand Acid" },
    { TR_BRAND_ELEC, 24, "Brand Elec" },
    { TR_BRAND_FIRE, 24, "Brand Fire" },
    { TR_BRAND_COLD, 24, "Brand Cold" },
    { TR_BRAND_POIS, 24, "Brand Poison" },
    { TR_CHAOTIC, 32, "Chaotic" },
    { TR_VAMPIRIC, 32, "Vampiric" },
    { TR_VORPAL, 16, "Vorpal" },
    { TR_VORPAL2, 32, "*Vorpal*" },

    /* Alt: These could be achieved from corresponding slay flags with enough essences */
    { TR_KILL_EVIL, 16, "Kill Evil" },
    { TR_KILL_UNDEAD, 8, "Kill Undead" },
    { TR_KILL_DEMON, 8, "Kill Demon" },
    { TR_KILL_DRAGON, 8, "Kill Dragon" },
    { TR_KILL_HUMAN, 8, "Kill Human" },
    { TR_KILL_ANIMAL, 8, "Kill Animal"},
    { TR_KILL_ORC, 8, "Kill Orc" },
    { TR_KILL_TROLL, 8, "Kill Troll" },
    { TR_KILL_GIANT, 8, "Kill Giant" },

    { -1, 0, NULL}
};

static int _rank_decay(int p)
{
    int r = _rank();
    for (; r < 4; r++)
        p /= 2;

    return MAX(1, p);
}

static int _slay_power(int i)
{
    return _rank_decay(_slay_flag_info[i].power);
}

static int _blows_mult(void)
{
    switch (_rank())
    {
    case 0: return 100;
    case 1: return 100;
    case 2: return 75;
    case 3: return 50;
    case 4:
    default: return 20;
    }
}

static void _calc_weapon_bonuses(object_type *o_ptr, weapon_info_t *info_ptr)
{
    int i;
    int to_hit = _calc_amount(_essences[_ESSENCE_TO_HIT], 1, 1);
    int to_dam = _calc_amount(_essences[_ESSENCE_TO_DAM], 1, 1);
    int to_dd = _calc_amount(_essences[_ESSENCE_XTRA_DICE], _rank_decay(64), 1);
    int blows = _calc_amount(_essences[TR_BLOWS], _rank_decay(32), 1);

    for (i = 0; i < TR_FLAG_SIZE; i++)
        o_ptr->art_flags[i] = 0;

    add_flag(o_ptr->art_flags, TR_NO_REMOVE);

    for (i = 0; ; i++)
    {
        int j = _slay_flag_info[i].flag;
        if (j < 0) break;
        if (_essences[j] >= _slay_power(i))
            add_flag(o_ptr->art_flags, j);
    }

    info_ptr->xtra_blow += blows * _blows_mult();

    info_ptr->to_h += to_hit;
    info_ptr->dis_to_h += to_hit;

    info_ptr->to_d += to_dam;
    info_ptr->dis_to_d += to_dam;

    info_ptr->to_dd += to_dd;
}

static void _calc_bonuses(void) 
{
    int i;
    int to_a = py_prorata_level(150);

    to_a += _calc_amount(_essences[_ESSENCE_AC], 2, 10);
    if (p_ptr->current_r_idx == MON_DEATH_SCYTHE)
        to_a -= 50;
    p_ptr->to_a += to_a;
    p_ptr->dis_to_a += to_a;

    p_ptr->pspeed += 1;
    
    p_ptr->levitation = TRUE;
    p_ptr->no_cut = TRUE;
    res_add(RES_BLIND);
    res_add(RES_POIS);
    p_ptr->hold_life = TRUE;

    if (p_ptr->lev >= 10)
        p_ptr->pspeed += 1;

    if (p_ptr->lev >= 25)
        p_ptr->pspeed += 1;

    if (p_ptr->lev >= 35)
        p_ptr->pspeed += 2;

    if (p_ptr->lev >= 45)
    {
        p_ptr->pspeed += 2;
        p_ptr->sh_retaliation = TRUE;
    }

    for (i = 0; i < RES_MAX; i++)
    {
        int j = res_get_object_flag(i);
        int n = _calc_amount(_essences[j], _res_power(i), 1);

        for (; n; --n)
            res_add(i);
    }
    if (_essences[TR_IM_ACID] >= 3)
        res_add_immune(RES_ACID);
    if (_essences[TR_IM_ELEC] >= 3)
        res_add_immune(RES_ELEC);
    if (_essences[TR_IM_FIRE] >= 3)
        res_add_immune(RES_FIRE);
    if (_essences[TR_IM_COLD] >= 3)
        res_add_immune(RES_COLD);

    p_ptr->life += 3*_calc_amount(_essences[TR_LIFE], 7, 1);

    p_ptr->skills.stl += _calc_amount(_essences[TR_STEALTH], 2, 1);
    p_ptr->pspeed += _calc_amount(_essences[TR_SPEED], 1, 10);
    p_ptr->skills.dev += 8*_calc_amount(_essences[TR_MAGIC_MASTERY], 2, 1);
    p_ptr->device_power += _calc_amount(_essences[TR_DEVICE_POWER], 2, 1);
    p_ptr->skills.srh += 5*_calc_amount(_essences[TR_SEARCH], 2, 1);
    p_ptr->skills.fos += 5*_calc_amount(_essences[TR_SEARCH], 2, 1);
    p_ptr->see_infra += _calc_amount(_essences[TR_INFRA], 2, 1);
    p_ptr->skill_dig += 20*_calc_amount(_essences[TR_TUNNEL], 2, 1);

    if (_essences[TR_SUST_STR] >= 5)
        p_ptr->sustain_str = TRUE;
    if (_essences[TR_SUST_INT] >= 5)
        p_ptr->sustain_int = TRUE;
    if (_essences[TR_SUST_WIS] >= 5)
        p_ptr->sustain_wis = TRUE;
    if (_essences[TR_SUST_DEX] >= 5)
        p_ptr->sustain_dex = TRUE;
    if (_essences[TR_SUST_CON] >= 5)
        p_ptr->sustain_con = TRUE;
    if (_essences[TR_SUST_CHR] >= 5)
        p_ptr->sustain_chr = TRUE;

    if (_essences[TR_TELEPATHY] >= 2)
        p_ptr->telepathy = TRUE;
    if (_essences[TR_ESP_ANIMAL] >= 2)
        p_ptr->esp_animal = TRUE;
    if (_essences[TR_ESP_UNDEAD] >= 2)
        p_ptr->esp_undead = TRUE;
    if (_essences[TR_ESP_DEMON] >= 2)
        p_ptr->esp_demon = TRUE;
    if (_essences[TR_ESP_ORC] >= 2)
        p_ptr->esp_orc = TRUE;
    if (_essences[TR_ESP_TROLL] >= 2)
        p_ptr->esp_troll = TRUE;
    if (_essences[TR_ESP_GIANT] >= 2)
        p_ptr->esp_giant = TRUE;
    if (_essences[TR_ESP_DRAGON] >= 2)
        p_ptr->esp_dragon = TRUE;
    if (_essences[TR_ESP_HUMAN] >= 2)
        p_ptr->esp_human = TRUE;
    if (_essences[TR_ESP_EVIL] >= 2)
        p_ptr->esp_evil = TRUE;
    if (_essences[TR_ESP_GOOD] >= 2)
        p_ptr->esp_good = TRUE;
    if (_essences[TR_ESP_NONLIVING] >= 2)
        p_ptr->esp_nonliving = TRUE;
    if (_essences[TR_ESP_UNIQUE] >= 2)
        p_ptr->esp_unique = TRUE;

    if (_essences[TR_NO_MAGIC] >= 5)
        p_ptr->anti_magic = TRUE;
    if (_essences[TR_FREE_ACT] >= 2)
        p_ptr->free_act = TRUE;
    if (_essences[TR_SEE_INVIS] >= 3)
        p_ptr->see_inv = TRUE;
    if (_essences[TR_SLOW_DIGEST] >= 2)
        p_ptr->slow_digest = TRUE;
    if (_essences[TR_REGEN] >= 7)
        p_ptr->regenerate = TRUE;
    if (_essences[TR_REFLECT] >= 3)
        p_ptr->reflect = TRUE;

    if (_essences[TR_SH_FIRE] >= 7)
        p_ptr->sh_fire = TRUE;
    if (_essences[TR_SH_ELEC] >= 7)
        p_ptr->sh_elec = TRUE;
    if (_essences[TR_SH_COLD] >= 7)
        p_ptr->sh_cold = TRUE;
}

static void _calc_stats(s16b stats[MAX_STATS])
{
    int i;
    for (i = 0; i < 6; i++)
        stats[i] += _calc_stat_bonus(TR_STR + i);
}

static void _get_flags(u32b flgs[TR_FLAG_SIZE]) 
{
    int i;

    add_flag(flgs, TR_SPEED);
    add_flag(flgs, TR_LITE);
    add_flag(flgs, TR_RES_BLIND);
    add_flag(flgs, TR_RES_POIS);
    add_flag(flgs, TR_HOLD_LIFE);
    add_flag(flgs, TR_LEVITATION);

    for (i = 0; i < 6; i++) /* Assume in order */
    {
        if (i != 0) /* Bug: Giving TR_STR marks the player as cursed ... */
            _add_stat_flag(TR_STR + i, flgs);
        if (_essences[TR_SUST_STR + i] >= 5)
            add_flag(flgs, TR_SUST_STR + i);
    }

    for (i = 0; i < RES_MAX; i++)
    {
        int j = res_get_object_flag(i);
        int n = _calc_amount(_essences[j], _res_power(i), 1);

        if (n)
            add_flag(flgs, j);
    }

    if (_calc_amount(_essences[TR_STEALTH], 2, 1))
        add_flag(flgs, TR_STEALTH);
    if (_calc_amount(_essences[TR_MAGIC_MASTERY], 2, 1))
        add_flag(flgs, TR_MAGIC_MASTERY);
    if (_calc_amount(_essences[TR_SEARCH], 2, 1))
        add_flag(flgs, TR_SEARCH);
    if (_calc_amount(_essences[TR_INFRA], 2, 1))
        add_flag(flgs, TR_INFRA);
    if (_calc_amount(_essences[TR_TUNNEL], 2, 1))
        add_flag(flgs, TR_TUNNEL);

    if (_essences[TR_TELEPATHY] >= 2)
        add_flag(flgs, TR_TELEPATHY);

    for (i = TR_ESP_ANIMAL; i <= TR_ESP_UNIQUE; i++)
    {
        if (_essences[i] >= 2)
            add_flag(flgs, i);
    }

    if (_essences[TR_NO_MAGIC] >= 5)
        add_flag(flgs, TR_NO_MAGIC);
    if (_essences[TR_FREE_ACT] >= 2)
        add_flag(flgs, TR_FREE_ACT);
    if (_essences[TR_SEE_INVIS] >= 3)
        add_flag(flgs, TR_SEE_INVIS);
    if (_essences[TR_SLOW_DIGEST] >= 2)
        add_flag(flgs, TR_SLOW_DIGEST);
    if (_essences[TR_REGEN] >= 7)
        add_flag(flgs, TR_REGEN);
    if (_essences[TR_REFLECT] >= 3)
        add_flag(flgs, TR_REFLECT);

    if (_essences[TR_SH_FIRE] >= 7)
        add_flag(flgs, TR_SH_FIRE);
    if (_essences[TR_SH_ELEC] >= 7)
        add_flag(flgs, TR_SH_ELEC);
    if (_essences[TR_SH_COLD] >= 7)
        add_flag(flgs, TR_SH_COLD);

    if (_essences[TR_IM_ACID] >= 3)
        add_flag(flgs, TR_IM_ACID);
    if (_essences[TR_IM_ELEC] >= 3)
        add_flag(flgs, TR_IM_ELEC);
    if (_essences[TR_IM_FIRE] >= 3)
        add_flag(flgs, TR_IM_FIRE);
    if (_essences[TR_IM_COLD] >= 3)
        add_flag(flgs, TR_IM_COLD);
}

/**********************************************************************
 * Powers
 **********************************************************************/
static void _absorb_spell(int cmd, variant *res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Absorb Weapon");
        break;
    case SPELL_DESC:
        var_set_string(res, "Destroys a single weapon, absorbing the essence of its power.");
        break;
    case SPELL_CAST:
    {
        object_type * o_ptr;
        int item;
        char o_name[MAX_NLEN];

        var_set_bool(res, FALSE);
        item_tester_hook = object_is_melee_weapon;

        if (!get_item(&item, "Absorb which item? ", "You have nothing to absorb.", USE_INVEN | USE_FLOOR)) break;

        if (item >= 0)
            o_ptr = &inventory[item];
        else
            o_ptr = &o_list[0 - item];

        object_desc(o_name, o_ptr, OD_NAME_ONLY);
        msg_format("You absorb the power of %s!", o_name);
        _absorb(o_ptr);

        if (item >= 0)
        {
            inven_item_increase(item, -1);
            inven_item_describe(item);
            inven_item_optimize(item);
        }
        else
        {
            floor_item_increase(0 - item, -1);
            floor_item_describe(0 - item);
            floor_item_optimize(0 - item);
        }
        

        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _detect_spell(int cmd, variant *res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Detect Weapons");
        break;
    case SPELL_DESC:
        var_set_string(res, "Locate nearby weapons, animated or not.");
        break;
    case SPELL_CAST:
    {
        int rng = DETECT_RAD_DEFAULT;
        int i, y, x;
        bool detect = FALSE;

        if (d_info[dungeon_type].flags1 & DF1_DARKNESS) rng /= 3;

        for (i = 1; i < o_max; i++)
        {
            object_type *o_ptr = &o_list[i];

            if (!o_ptr->k_idx) continue;
            if (o_ptr->held_m_idx) continue;
            y = o_ptr->iy;
            x = o_ptr->ix;
            if (distance(py, px, y, x) > rng) continue;
            if (!object_is_melee_weapon(o_ptr)) continue;
            o_ptr->marked |= OM_FOUND;
            p_ptr->window |= PW_OBJECT_LIST;
            lite_spot(y, x);
            detect = TRUE;
        }
        if (detect_monsters_string(DETECT_RAD_DEFAULT, "|/"))
            detect = TRUE;

        if (detect)
            msg_print("You sense your kind.");

        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _judge_spell(int cmd, variant *res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Identify Weapon");
        break;
    case SPELL_DESC:
        var_set_string(res, "Identifies a weapon.");
        break;
    case SPELL_CAST:
        if (p_ptr->lev >= 35)
            var_set_bool(res, identify_fully(object_is_melee_weapon));
        else
            var_set_bool(res, ident_spell(object_is_melee_weapon));
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static power_info _powers[] = 
{
    { A_STR, {  1,  0,  0, _absorb_spell } },
    { A_STR, {  5,  1, 30, _detect_spell } },
    { A_STR, { 10, 10, 50, _judge_spell } },
    /*{ A_STR, { 25, 30, 60, _animate_spell } },*/
    {    -1, { -1, -1, -1, NULL}}
};

static int _get_powers(spell_info* spells, int max) 
{
    return get_powers_aux(spells, max, _powers);
}

/**********************************************************************
 * Birth and Evolution
 **********************************************************************/
static void _birth(void) 
{ 
    object_type forge;
    int i;

    for (i = 0; i < _MAX_ESSENCE; i++)
        _essences[i] = 0;

    p_ptr->current_r_idx = MON_BROKEN_DEATH_SWORD;
    equip_on_change_race();

    object_prep(&forge, lookup_kind(TV_SWORD, SV_BROKEN_SWORD));
    add_flag(forge.art_flags, TR_NO_REMOVE);
    forge.to_h =  1;
    forge.to_d =  3;
    add_outfit(&forge);
}

static object_type *_weapon(void)
{
    return equip_obj(p_ptr->weapon_info[0].slot);
}

static void _upgrade_weapon(int tval, int sval)
{
    object_type *o_ptr = _weapon();

    object_prep(o_ptr, lookup_kind(tval, sval));
    o_ptr->to_h = p_ptr->lev / 5;
    o_ptr->to_d = p_ptr->lev / 3;

    add_flag(o_ptr->art_flags, TR_NO_REMOVE);
    object_aware(o_ptr);
    object_known(o_ptr);
    o_ptr->ident |= IDENT_FULL;

    p_ptr->update |= PU_BONUS;
    p_ptr->window |= PW_INVEN | PW_EQUIP;
}

static void _gain_level(int new_level) 
{
    if (p_ptr->current_r_idx == MON_BROKEN_DEATH_SWORD && new_level >= 10)
    {
        p_ptr->current_r_idx = MON_DEATH_SWORD;
        equip_on_change_race();
        _upgrade_weapon(TV_SWORD, SV_LONG_SWORD);
        msg_print("You have evolved into a Death Sword.");
        p_ptr->redraw |= PR_MAP;
    }
    if (p_ptr->current_r_idx == MON_DEATH_SWORD && new_level >= 25)
    {
        p_ptr->current_r_idx = MON_POLEAXE_OF_ANIMATED_ATTACK;
        equip_on_change_race();
        _upgrade_weapon(TV_POLEARM, SV_BATTLE_AXE);
        msg_print("You have evolved into a Poleaxe of Animated Attack.");
        p_ptr->redraw |= PR_MAP;
    }
    if (p_ptr->current_r_idx == MON_POLEAXE_OF_ANIMATED_ATTACK && new_level >= 35)
    {
        p_ptr->current_r_idx = MON_HELLBLADE;
        equip_on_change_race();
        _upgrade_weapon(TV_SWORD, SV_BLADE_OF_CHAOS);
        msg_print("You have evolved into a Hellblade.");
        p_ptr->redraw |= PR_MAP;
    }
    if (p_ptr->current_r_idx == MON_HELLBLADE && new_level >= 45)
    {
        p_ptr->current_r_idx = MON_DEATH_SCYTHE;
        equip_on_change_race();
        _upgrade_weapon(TV_POLEARM, SV_DEATH_SCYTHE_HACK);
        msg_print("You have evolved into a Death Scythe.");
        p_ptr->redraw |= PR_MAP;
    }

    /* Note: These boosts will be completely wiped out next evolution */
    {
    object_type *o_ptr = _weapon();

        if (one_in_(15) && new_level < 45) /* Sorry, Mr. Death Scythe, but you are too powerful! */
        {
            o_ptr->dd++;
            msg_print("You grow sharper!");
        }
        else if (one_in_(2) || (new_level % 7) == 0)
        {
            o_ptr->to_d++;
            msg_print("You grow more deadly!");
        }
        else
        {
            o_ptr->to_h++;
            msg_print("You grow more accurate!");
        }
    }
}

/**********************************************************************
 * Character Dump
 **********************************************************************/
static void _dump_ability_flag(doc_ptr doc, int which, int threshold, cptr name)
{
    int n = _essences[which];
    if (n > 0)
    {
        doc_printf(doc, "   %-22.22s %5d %5d %5.5s\n",
            name,
            n, 
            threshold,
            n >= threshold ? "Y" : ""
        );
    }
}
static void _dump_bonus_flag(doc_ptr doc, int which, int power, int rep, cptr name)
{
    int n = _essences[which];
    if (n > 0)
    {
        doc_printf(doc, "   %-22.22s %5d %5d %+5d\n",
            name,
            n, 
            _calc_needed(n, power, rep),
            _calc_amount(n, power, rep)
        );
    }
}

static void _character_dump(doc_ptr doc)
{
    int i;
    doc_printf(doc, "<topic:Essences>=================================== <color:keypress>E</color>ssences ==================================\n\n");
    doc_printf(doc, "   <color:G>%-22.22s Total  Need Bonus</color>\n", "Stats");
    for (i = 0; i < 6; i++) /* Assume in order */
        _dump_bonus_flag(doc, TR_STR + i, 3, 1, stat_name_true[A_STR + i]);

    doc_printf(doc, "\n   <color:G>%-22.22s Total  Need Bonus</color>\n", "Skills");
    _dump_bonus_flag(doc, _ESSENCE_TO_HIT, 1, 1, "To Hit");
    _dump_bonus_flag(doc, _ESSENCE_TO_DAM, 1, 1, "To Dam");
    _dump_bonus_flag(doc, _ESSENCE_AC, 2, 10, "To AC");
    _dump_bonus_flag(doc, TR_STEALTH, 2, 1, "Stealth");
    _dump_bonus_flag(doc, TR_SPEED, 1, 10, "Speed");
    if (_essences[TR_BLOWS])
    {
        int blows = _calc_amount(_essences[TR_BLOWS], _rank_decay(32), 1);

        blows = blows * _blows_mult();
        doc_printf(doc, "   %-22.22s %5d %5d %5.5s\n",
            "Attacks",
            _essences[TR_BLOWS], 
            _calc_needed(_essences[TR_BLOWS], _rank_decay(32), 1),
            blows ? format("+%d.%2.2d", blows / 100, blows % 100) : ""
        );
    }
    _dump_bonus_flag(doc, _ESSENCE_XTRA_DICE, _rank_decay(64), 1, "Slaying");
    _dump_bonus_flag(doc, TR_LIFE, 7, 1, "Life");
    _dump_bonus_flag(doc, TR_SEARCH, 2, 1, "Searching");
    _dump_bonus_flag(doc, TR_INFRA, 2, 1, "Infravision");
    _dump_bonus_flag(doc, TR_TUNNEL, 2, 1, "Digging");
    _dump_bonus_flag(doc, TR_MAGIC_MASTERY, 2, 1, "Magic Mastery");
    _dump_bonus_flag(doc, TR_LITE, 1, 1, "Light");
 
    doc_printf(doc, "\n   <color:G>%-22.22s Total  Need Bonus</color>\n", "Slays");
    for (i = 0; ; i++)
    {
        int j = _slay_flag_info[i].flag;
        if (j < 0) break;
        _dump_ability_flag(doc, j, _slay_power(i), _slay_flag_info[i].name);
    }

    doc_printf(doc, "\n   <color:G>%-22.22s Total  Need Bonus</color>\n", "Resistances");
    for (i = 0; i < RES_MAX; i++)
        _dump_bonus_flag(doc, res_get_object_flag(i), _res_power(i), 1, format("%^s", res_name(i)));

    _dump_ability_flag(doc, TR_IM_ACID, 3, "Immune Acid");
    _dump_ability_flag(doc, TR_IM_ELEC, 3, "Immune Elec");
    _dump_ability_flag(doc, TR_IM_FIRE, 3, "Immune Fire");
    _dump_ability_flag(doc, TR_IM_COLD, 3, "Immune Cold");

    doc_printf(doc, "\n   <color:G>%-22.22s Total  Need Bonus</color>\n", "Abilities");
    _dump_ability_flag(doc, TR_FREE_ACT, 2, "Free Action");
    _dump_ability_flag(doc, TR_SEE_INVIS, 3, "See Invisible");
    _dump_ability_flag(doc, TR_SLOW_DIGEST, 2, "Slow Digestion");
    _dump_ability_flag(doc, TR_REGEN, 7, "Regeneration");
    _dump_ability_flag(doc, TR_NO_MAGIC, 5, "Antimagic");
    _dump_ability_flag(doc, TR_REFLECT, 3, "Reflection");
    _dump_ability_flag(doc, TR_SH_FIRE, 7, "Aura Fire");
    _dump_ability_flag(doc, TR_SH_ELEC, 7, "Aura Elec");
    _dump_ability_flag(doc, TR_SH_COLD, 7, "Aura Cold");
    for (i = 0; i < 6; i++) /* Assume in order */
        _dump_ability_flag(doc, TR_SUST_STR + i, 5, format("Sustain %s", stat_name_true[A_STR + i]));

    doc_printf(doc, "\n   <color:G>%-22.22s Total  Need Bonus</color>\n", "ESP");
    _dump_ability_flag(doc, TR_TELEPATHY, 2, "Telepathy");
    _dump_ability_flag(doc, TR_ESP_ANIMAL, 2, "ESP Animals");
    _dump_ability_flag(doc, TR_ESP_UNDEAD, 2, "ESP Undead");
    _dump_ability_flag(doc, TR_ESP_DEMON, 2, "ESP Demon");
    _dump_ability_flag(doc, TR_ESP_ORC, 2, "ESP Orc");
    _dump_ability_flag(doc, TR_ESP_TROLL, 2, "ESP Troll");
    _dump_ability_flag(doc, TR_ESP_GIANT, 2, "ESP Giant");
    _dump_ability_flag(doc, TR_ESP_DRAGON, 2, "ESP Dragon");
    _dump_ability_flag(doc, TR_ESP_HUMAN, 2, "ESP Human");
    _dump_ability_flag(doc, TR_ESP_EVIL, 2, "ESP Evil");
    _dump_ability_flag(doc, TR_ESP_GOOD, 2, "ESP Good");
    _dump_ability_flag(doc, TR_ESP_NONLIVING, 2, "ESP Nonliving");
    _dump_ability_flag(doc, TR_ESP_UNIQUE, 2, "ESP Unique");

    doc_newline(doc);
}

/**********************************************************************
 * Public
 **********************************************************************/
race_t *mon_sword_get_race(void)
{
    static race_t me = {0};
    static bool   init = FALSE;
    static cptr   titles[5] =  {"Broken Death Sword", "Death Sword", "Poleaxe of Animated Attack", 
                                "Hellblade", "Death Scythe"};    
    int           rank = _rank();

    if (!init)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 25,  37,  40,   4,  14,   5,  56,  20};
    skills_t xs = { 12,  12,  12,   0,   0,   0,  20,   7};

        me.skills = bs;
        me.extra_skills = xs;

        me.name = "Death-Sword";
        me.desc = "Death Swords are mighty weapons animated by magical means. As such, "
                    "they are unable to use equipment the way other players can. Instead, "
                    "they simply are a weapon of their current form. But never fear, Death "
                    "Swords have the power to absorb magical essences from the weapons they "
                    "find, gaining power in the process.";

        me.infra = 3;
        me.exp = 150;
        me.base_hp = 30;
        me.shop_adjust = 110; /* I think shopkeepers are puzzled, more than anything else! */

        me.calc_bonuses = _calc_bonuses;
        me.calc_stats = _calc_stats;
        me.calc_weapon_bonuses = _calc_weapon_bonuses;
        me.character_dump = _character_dump;
        me.get_flags = _get_flags;
        me.gain_level = _gain_level;
        me.get_powers = _get_powers;
        me.birth = _birth;

        me.load_player = _load;
        me.save_player = _save;

        me.flags = RACE_IS_MONSTER | RACE_IS_NONLIVING;
        me.pseudo_class_idx = CLASS_WARRIOR;

        init = TRUE;
    }

    me.subname = titles[rank];
    me.stats[A_STR] =  1 + rank;
    me.stats[A_INT] = -5;
    me.stats[A_WIS] = -5;
    me.stats[A_DEX] =  0 + rank/2;
    me.stats[A_CON] =  1 + rank;
    me.stats[A_CHR] =  0;
    me.life = 85 + 5*rank;
    me.boss_r_idx = MON_STORMBRINGER;

    me.equip_template = mon_get_equip_template();
    return &me;
}

void sword_absorb_object(object_type *o_ptr)
{
    if (object_is_melee_weapon(o_ptr))
    {
        char o_name[MAX_NLEN];
        object_desc(o_name, o_ptr, OD_NAME_ONLY);
        msg_format("You attempt to drain power from %s.", o_name);
        _absorb(o_ptr);
    }
}

bool sword_disenchant(void)
{
    bool result = FALSE;
    int  r = _rank();
    int  i;

    for (i = 0; i < _MAX_ESSENCE; i++)
    {
        int n = _essences[i];
        
        if (!n) continue;
        if (i == TR_SPEED) continue;
        if (res_save(RES_DISEN, 44)) continue;
        
        _essences[i] -= MAX(1, _essences[i] * randint1(r) / 20);
        if (_essences[i] < n)
            result = TRUE;
    }
    if (result)
    {
        p_ptr->update |= PU_BONUS;
        msg_print("You feel power draining from your body!");
    }
    return result;
}


int sword_calc_torch(void)
{
    return 1 + _calc_amount(_essences[TR_LITE], 1, 1);
}
