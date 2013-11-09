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
            _essences[j] = n;
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

static int _calc_amount(int amount, int power)
{
    int result = 0;

    while (amount > 0)
    {
        if (amount >= power)
            result++;
        amount -= power;
        power *= 2;
    }

    return result;
}

static int _calc_stat_bonus(int flag)
{
    return _calc_amount(_essences[flag], 3);
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
        return 1;

    case RES_SOUND:
    case RES_SHARDS:
        return 3;

    case RES_CHAOS:
    case RES_DISEN:
        return 2;
    }

    return 2;
}

/**********************************************************************
 * Equipment
 **********************************************************************/
static equip_template_t _template1 =  {1, { 
    {EQUIP_SLOT_WEAPON, "You", 0},
}};

/**********************************************************************
 * Attacks and Bonuses
 **********************************************************************/
typedef struct {
    int flag;
    int power;
    cptr name;
} _flag_info_t;

static _flag_info_t _slay_flag_info[] = {
    { TR_SLAY_EVIL, 15, "Slay Evil" },
    { TR_SLAY_UNDEAD, 2, "Slay Undead" },
    { TR_SLAY_DEMON, 2, "Slay Demon" },
    { TR_SLAY_DRAGON, 5, "Slay Dragon" },
    { TR_SLAY_HUMAN, 3, "Slay Human" },
    { TR_SLAY_ANIMAL, 1, "Slay Animal" },
    { TR_SLAY_ORC, 1, "Slay Orc" },
    { TR_SLAY_TROLL, 1, "Slay Troll" },
    { TR_SLAY_GIANT, 1, "Slay Giant" },

    { TR_BRAND_ACID, 7, "Brand Acid" },
    { TR_BRAND_ELEC, 7, "Brand Elec" },
    { TR_BRAND_FIRE, 7, "Brand Fire" },
    { TR_BRAND_COLD, 7, "Brand Cold" },
    { TR_BRAND_POIS, 7, "Brand Poison" },
    { TR_CHAOTIC, 10, "Chaotic" },
    { TR_VAMPIRIC, 17, "Vampiric" },
    { TR_VORPAL, 10, "Vorpal" },

    { TR_KILL_EVIL, 3, "Kill Evil" },
    { TR_KILL_UNDEAD, 3, "Kill Undead" },
    { TR_KILL_DEMON, 3, "Kill Demon" },
    { TR_KILL_DRAGON, 3, "Kill Dragon" },
    { TR_KILL_HUMAN, 2, "Kill Human" },
    { TR_KILL_ANIMAL, 2, "Kill Animal"},
    { TR_KILL_ORC, 2, "Kill Orc" },
    { TR_KILL_TROLL, 2, "Kill Troll" },
    { TR_KILL_GIANT, 2, "Kill Giant" },

    { -1, 0, NULL}
};

static void _calc_weapon_bonuses(object_type *o_ptr, weapon_info_t *info_ptr)
{
    int i;
    int to_hit = _calc_amount(_essences[_ESSENCE_TO_HIT], 1);
    int to_dam = _calc_amount(_essences[_ESSENCE_TO_DAM], 1);
    int to_dd = _calc_amount(_essences[_ESSENCE_XTRA_DICE], 50);

    for (i = 0; i < TR_FLAG_SIZE; i++)
        o_ptr->art_flags[i] = 0;

    add_flag(o_ptr->art_flags, TR_NO_REMOVE);

    for (i = 0; ; i++)
    {
        int j = _slay_flag_info[i].flag;
        if (j < 0) break;
        if (_essences[j] >= _slay_flag_info[i].power)
            add_flag(o_ptr->art_flags, j);
    }

    if (_essences[TR_BLOWS] >= 25)
        info_ptr->xtra_blow++;

    info_ptr->to_h += to_hit;
    info_ptr->dis_to_h += to_hit;

    info_ptr->to_d += to_dam;
    info_ptr->dis_to_d += to_dam;

    info_ptr->to_dd += to_dd;
}

static void _calc_bonuses(void) 
{
    int i;
    int l = p_ptr->lev;
    int to_a = l + l*l/50 + l*l*l/2500;

    to_a += _essences[_ESSENCE_AC] / 2;
    p_ptr->to_a += to_a;
    p_ptr->dis_to_a += to_a;

    p_ptr->pspeed += 1;

    p_ptr->lite = TRUE;
    p_ptr->no_cut = TRUE;
    res_add(RES_BLIND);
    res_add(RES_POIS);
    p_ptr->hold_life = TRUE;

    if (p_ptr->lev >= 10)
        p_ptr->pspeed += 1;

    if (p_ptr->lev >= 25)
    {
        p_ptr->pspeed += 1;
        res_add(RES_COLD);
        res_add(RES_ELEC);
    }

    if (p_ptr->lev >= 35)
        p_ptr->pspeed += 2;

    if (p_ptr->lev >= 45)
        p_ptr->pspeed += 5;

    for (i = 0; i < 6; i++) /* Assume in order */
        p_ptr->stat_add[A_STR + i] += _calc_stat_bonus(TR_STR + i);

    for (i = 0; i < RES_MAX; i++)
    {
        int j = res_get_object_flag(i);
        int n = _calc_amount(_essences[j], _res_power(i));

        for (; n; --n)
            res_add(i);
    }

    p_ptr->skills.stl += _calc_amount(_essences[TR_STEALTH], 2);
    p_ptr->pspeed += _essences[TR_SPEED];
    p_ptr->skills.dev += 8*_calc_amount(_essences[TR_MAGIC_MASTERY], 2);
    p_ptr->device_power += _calc_amount(_essences[TR_MAGIC_MASTERY], 2);
    p_ptr->skills.srh += 5*_calc_amount(_essences[TR_SEARCH], 2);
    p_ptr->skills.fos += 5*_calc_amount(_essences[TR_SEARCH], 2);
    p_ptr->see_infra += _calc_amount(_essences[TR_INFRA], 2);
    p_ptr->skill_dig += 20*_calc_amount(_essences[TR_TUNNEL], 2);

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
    if (_essences[TR_LEVITATION] >= 3)
        p_ptr->levitation = TRUE;
    if (_essences[TR_FREE_ACT] >= 1)
        p_ptr->free_act = TRUE;
    if (_essences[TR_SEE_INVIS] >= 1)
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

static void _get_flags(u32b flgs[TR_FLAG_SIZE]) 
{
    int i;

    add_flag(flgs, TR_SPEED);
    add_flag(flgs, TR_LITE);
    add_flag(flgs, TR_RES_BLIND);
    add_flag(flgs, TR_RES_POIS);
    add_flag(flgs, TR_HOLD_LIFE);

    if (p_ptr->lev >= 25)
    {
        add_flag(flgs, TR_RES_COLD);
        add_flag(flgs, TR_RES_ELEC);
    }

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
        int n = _calc_amount(_essences[j], _res_power(i));

        if (n)
            add_flag(flgs, j);
    }

    if (_calc_amount(_essences[TR_STEALTH], 2))
        add_flag(flgs, TR_STEALTH);
    if (_calc_amount(_essences[TR_MAGIC_MASTERY], 2))
        add_flag(flgs, TR_MAGIC_MASTERY);
    if (_calc_amount(_essences[TR_SEARCH], 2))
        add_flag(flgs, TR_SEARCH);
    if (_calc_amount(_essences[TR_INFRA], 2))
        add_flag(flgs, TR_INFRA);
    if (_calc_amount(_essences[TR_TUNNEL], 2))
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
    if (_essences[TR_LEVITATION] >= 3)
        add_flag(flgs, TR_LEVITATION);
    if (_essences[TR_FREE_ACT] >= 1)
        add_flag(flgs, TR_FREE_ACT);
    if (_essences[TR_SEE_INVIS] >= 1)
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
}

/**********************************************************************
 * Powers
 **********************************************************************/
void _absorb_spell(int cmd, variant *res)
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

    object_prep(&forge, lookup_kind(TV_SWORD, SV_BROKEN_SWORD));
    add_flag(forge.art_flags, TR_NO_REMOVE);
    forge.to_h =  1;
    forge.to_d =  3;
    add_outfit(&forge);
}

static bool _drain_essences(int div)
{
    bool result = FALSE;
    int r = _rank();
    int i;

    for (i = 0; i < _MAX_ESSENCE; i++)
    {
        int n = _essences[i];
        
        if (!n) continue;
        if (i == TR_SPEED) continue;
        
        _essences[i] -= _essences[i] * randint1(r) / div;
        if (_essences[i] < n)
            result = TRUE;
    }
    if (result)
        p_ptr->update |= PU_BONUS;
    return result;
}

static void _upgrade_weapon(int tval, int sval)
{
    bool drained = FALSE;
    object_type *o_ptr = equip_obj(p_ptr->weapon_info[0].slot);

    object_prep(o_ptr, lookup_kind(tval, sval));
    o_ptr->to_h = p_ptr->lev / 5;
    o_ptr->to_d = p_ptr->lev / 3;

    add_flag(o_ptr->art_flags, TR_NO_REMOVE);
    object_aware(o_ptr);
    object_known(o_ptr);
    o_ptr->ident |= IDENT_MENTAL;

    p_ptr->update |= PU_BONUS;
    p_ptr->window |= PW_INVEN | PW_EQUIP | PW_PLAYER;

    if (_drain_essences(10))
        msg_print("You feel your magic drain to support your new form.");
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
        _upgrade_weapon(TV_POLEARM, SV_LOCHABER_AXE);
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
    object_type *o_ptr = equip_obj(p_ptr->weapon_info[0].slot);

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
static void _dump_ability_flag(FILE* fff, int which, int threshold, cptr name)
{
    int n = _essences[which];
    if (n > 0)
    {
        fprintf(fff, "   %-22.22s %5d %5.5s\n", 
            name,
            n, 
            n >= threshold ? "Y" : ""
        );
    }
}
static void _dump_bonus_flag(FILE* fff, int which, int power, cptr name)
{
    int n = _essences[which];
    if (n > 0)
    {
        fprintf(fff, "   %-22.22s %5d %+5d\n", 
            name,
            n, 
            _calc_amount(n, power)
        );
    }
}

static void _character_dump(FILE* fff)
{
    int i;
    fprintf(fff, "\n\n=================================== Essences ==================================\n");
    fprintf(fff, "\n   %-22.22s Total Bonus\n", "Stats");
    fprintf(fff, "   ---------------------- ----- -----\n");
    for (i = 0; i < 6; i++) /* Assume in order */
        _dump_bonus_flag(fff, TR_STR + i, 3, stat_name_true[A_STR + i]);

    fprintf(fff, "\n   %-22.22s Total Bonus\n", "Skills");
    fprintf(fff, "   ---------------------- ----- -----\n");
    _dump_bonus_flag(fff, _ESSENCE_TO_HIT, 1, "To Hit");
    _dump_bonus_flag(fff, _ESSENCE_TO_DAM, 1, "To Dam");
    if (_essences[_ESSENCE_AC])
    {
        fprintf(fff, "   %-22.22s %5d %+5d\n", 
            "To AC",
            _essences[_ESSENCE_AC], 
            _essences[_ESSENCE_AC] / 2
        );
    }
    _dump_bonus_flag(fff, TR_STEALTH, 2, "Stealth");
    if (_essences[TR_SPEED])
    {
        fprintf(fff, "   %-22.22s %5d %+5d\n", 
            "Speed",
            _essences[TR_SPEED], 
            _essences[TR_SPEED]
        );
    }
    if (_essences[TR_BLOWS])
    {
        fprintf(fff, "   %-22.22s %5d %5.5s\n", 
            "Attacks",
            _essences[TR_BLOWS], 
            _essences[TR_BLOWS] >= 25 ? "+1" : ""
        );
    }
    _dump_bonus_flag(fff, _ESSENCE_XTRA_DICE, 50, "Slaying");
    _dump_bonus_flag(fff, TR_SEARCH, 2, "Searching");
    _dump_bonus_flag(fff, TR_INFRA, 2, "Infravision");
    _dump_bonus_flag(fff, TR_TUNNEL, 2, "Digging");
    _dump_bonus_flag(fff, TR_MAGIC_MASTERY, 2, "Magic Mastery");

    fprintf(fff, "\n   %-22.22s Total Bonus\n", "Slays");
    fprintf(fff, "   ---------------------- ----- -----\n");
    for (i = 0; ; i++)
    {
        int j = _slay_flag_info[i].flag;
        if (j < 0) break;
        _dump_ability_flag(fff, j, _slay_flag_info[i].power, _slay_flag_info[i].name);
    }

    fprintf(fff, "\n   %-22.22s Total Bonus\n", "Resistances");
    fprintf(fff, "   ---------------------- ----- -----\n");
    for (i = 0; i < RES_MAX; i++)
        _dump_bonus_flag(fff, res_get_object_flag(i), _res_power(i), format("%^s", res_name(i)));

    fprintf(fff, "\n   %-22.22s Total Bonus\n", "Abilities");
    fprintf(fff, "   ---------------------- ----- -----\n");
    _dump_ability_flag(fff, TR_FREE_ACT, 1, "Free Action");
    _dump_ability_flag(fff, TR_SEE_INVIS, 1, "See Invisible");
    _dump_ability_flag(fff, TR_LEVITATION, 3, "Levitation");
    _dump_ability_flag(fff, TR_SLOW_DIGEST, 2, "Slow Digestion");
    _dump_bonus_flag(fff, TR_LITE, 1, "Light");
    _dump_ability_flag(fff, TR_REGEN, 7, "Regeneration");
    _dump_ability_flag(fff, TR_NO_MAGIC, 5, "Antimagic");
    _dump_ability_flag(fff, TR_REFLECT, 3, "Reflection");
    _dump_ability_flag(fff, TR_SH_FIRE, 7, "Aura Fire");
    _dump_ability_flag(fff, TR_SH_ELEC, 7, "Aura Elec");
    _dump_ability_flag(fff, TR_SH_COLD, 7, "Aura Cold");
    for (i = 0; i < 6; i++) /* Assume in order */
        _dump_ability_flag(fff, TR_SUST_STR + i, 5, format("Sustain %s", stat_name_true[A_STR + i]));

    fprintf(fff, "\n   %-22.22s Total Bonus\n", "ESP");
    fprintf(fff, "   ---------------------- ----- -----\n");
    _dump_ability_flag(fff, TR_TELEPATHY, 2, "Telepathy");
    _dump_ability_flag(fff, TR_ESP_ANIMAL, 2, "ESP Animals");
    _dump_ability_flag(fff, TR_ESP_UNDEAD, 2, "ESP Undead");
    _dump_ability_flag(fff, TR_ESP_DEMON, 2, "ESP Demon");
    _dump_ability_flag(fff, TR_ESP_ORC, 2, "ESP Orc");
    _dump_ability_flag(fff, TR_ESP_TROLL, 2, "ESP Troll");
    _dump_ability_flag(fff, TR_ESP_GIANT, 2, "ESP Giant");
    _dump_ability_flag(fff, TR_ESP_DRAGON, 2, "ESP Dragon");
    _dump_ability_flag(fff, TR_ESP_HUMAN, 2, "ESP Human");
    _dump_ability_flag(fff, TR_ESP_EVIL, 2, "ESP Evil");
    _dump_ability_flag(fff, TR_ESP_GOOD, 2, "ESP Good");
    _dump_ability_flag(fff, TR_ESP_NONLIVING, 2, "ESP Nonliving");
    _dump_ability_flag(fff, TR_ESP_UNIQUE, 2, "ESP Unique");
}

/**********************************************************************
 * Public
 **********************************************************************/
race_t *mon_sword_get_race_t(void)
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

        me.calc_bonuses = _calc_bonuses;
        me.calc_weapon_bonuses = _calc_weapon_bonuses;
        me.character_dump = _character_dump;
        me.get_flags = _get_flags;
        me.gain_level = _gain_level;
        me.get_powers = _get_powers;
        me.birth = _birth;

        me.load_player = _load;
        me.save_player = _save;

        me.flags = RACE_IS_MONSTER;

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

    me.equip_template = &_template1;
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
    if (!res_save(RES_DISEN, 70) && _drain_essences(20))
    {
        msg_print("You feel power draining from your body!");
        result = TRUE;
    }
    return result;
}

int sword_calc_torch(void)
{
    return _calc_amount(_essences[TR_LITE], 1);
}