#include "angband.h"

static int _rank(void)
{
    int rank = 0;

    if (plr->lev >= 10) rank++;
    if (plr->lev >= 25) rank++;
    if (plr->lev >= 35) rank++;
    if (plr->lev >= 45) rank++;
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

static bool _of_filter(of_info_ptr info)
{
    switch (info->id)
    {
    case OF_RIDING:
    case OF_THROWING:
    case OF_HIDE_TYPE:
    case OF_SHOW_MODS:
    case OF_IGNORE_ACID:
    case OF_IGNORE_ELEC:
    case OF_IGNORE_FIRE:
    case OF_IGNORE_COLD:
    case OF_ACTIVATE:
    case OF_FULL_NAME:
    case OF_FIXED_FLAVOR:
        return FALSE;
    }
    return TRUE;
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
    bool         result = FALSE;
    int          i;
    int          div = 1;
    vec_ptr      v = NULL;
    object_kind *k_ptr = &k_info[o_ptr->k_idx];
    u32b         flags[OF_ARRAY_SIZE];

    obj_flags(o_ptr, flags);

    if (o_ptr->curse_flags & OFC_AGGRAVATE)
        div++;
    if (o_ptr->curse_flags & (OFC_TY_CURSE | OFC_HEAVY_CURSE))
        div++;

    /* Object Flags */
    v = of_lookup_filter(_of_filter);
    for (i = 0; i < vec_length(v); i++)
    {
        of_info_ptr info = vec_get(v, i);
        if (have_flag(flags, info->id))
        {
            if (info->flags & OFT_PVAL)
            {
                if (_add_essence(info->id, o_ptr->pval/div))
                    result = TRUE;
            }
            else
            {
                _essences[info->id]++;
                result = TRUE;
            }
        }
    }
    vec_free(v);
    v = NULL;

    /* Special Handling for non-flag based essences */
    if (_add_essence(_ESSENCE_XTRA_DICE, (o_ptr->ds - k_ptr->ds)/1/*div?*/))
        result = TRUE;
    if (_add_essence(_ESSENCE_XTRA_DICE, (o_ptr->dd - k_ptr->dd)/1/*div?*/))
        result = TRUE;

    if (_add_essence(_ESSENCE_AC, o_ptr->to_a/div))
        result = TRUE;
    if (_add_essence(_ESSENCE_TO_HIT, o_ptr->to_h/div))
        result = TRUE;
    if (_add_essence(_ESSENCE_TO_DAM, o_ptr->to_d/div))
        result = TRUE;

    if (result)
    {
        plr->update |= PU_BONUS;
        msg_print("<color:B>You grow stronger!</color>");
    }
    return result;
}

static bool _absorb_object(object_type *o_ptr)
{
    if (obj_is_weapon(o_ptr))
    {
        char o_name[MAX_NLEN];
        object_desc(o_name, o_ptr, OD_NAME_ONLY | OD_COLOR_CODED);
        msg_format("You attempt to drain power from %s.", o_name);
        _absorb(o_ptr);
        return TRUE;
    }
    return FALSE;
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

static void _add_stat_flag(int flag, u32b flgs[OF_ARRAY_SIZE])
{
    if (_calc_stat_bonus(flag))
        add_flag(flgs, flag);
}

static int _res_power(int which)
{
    switch (which)
    {
    case GF_ACID:
    case GF_FIRE:
    case GF_COLD:
    case GF_ELEC:
    case GF_CONF:
    case GF_FEAR:
    case GF_TIME:
    case GF_TELEPORT:
        return 2;

    case GF_SOUND:
    case GF_SHARDS:
    case GF_CHAOS:
        return 4;

    case GF_DISEN:
    case GF_POIS:
    case GF_LIGHT:
    case GF_DARK:
    case GF_NETHER:
    case GF_NEXUS:
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
    { OF_SLAY_EVIL, 64, "Slay Evil" },
    { OF_SLAY_GOOD, 16, "Slay Good" },
    { OF_SLAY_UNDEAD, 16, "Slay Undead" },
    { OF_SLAY_DEMON, 16, "Slay Demon" },
    { OF_SLAY_DRAGON, 16, "Slay Dragon" },
    { OF_SLAY_HUMAN, 16, "Slay Human" },
    { OF_SLAY_ANIMAL, 8, "Slay Animal" },
    { OF_SLAY_ORC, 4, "Slay Orc" },
    { OF_SLAY_TROLL, 8, "Slay Troll" },
    { OF_SLAY_GIANT, 8, "Slay Giant" },
    { OF_SLAY_LIVING, 8, "Slay Living" },

    { OF_BRAND_ACID, 24, "Brand Acid" },
    { OF_BRAND_ELEC, 24, "Brand Elec" },
    { OF_BRAND_FIRE, 24, "Brand Fire" },
    { OF_BRAND_COLD, 24, "Brand Cold" },
    { OF_BRAND_POIS, 24, "Brand Poison" },
    { OF_BRAND_LIGHT, 12, "Brand Light" },
    { OF_BRAND_DARK, 12, "Brand Dark" },
    { OF_BRAND_PLASMA, 12, "Brand Plasma" },
    { OF_BRAND_CHAOS, 32, "Chaotic" },
    { OF_BRAND_VAMP, 32, "Vampiric" },
    { OF_VORPAL, 16, "Vorpal" },
    { OF_VORPAL2, 32, "*Vorpal*" },

    /* Alt: These could be achieved from corresponding slay flags with enough essences */
    { OF_KILL_EVIL, 16, "Kill Evil" },
    { OF_KILL_UNDEAD, 8, "Kill Undead" },
    { OF_KILL_DEMON, 8, "Kill Demon" },
    { OF_KILL_DRAGON, 8, "Kill Dragon" },
    { OF_KILL_HUMAN, 8, "Kill Human" },
    { OF_KILL_ANIMAL, 8, "Kill Animal"},
    { OF_KILL_ORC, 8, "Kill Orc" },
    { OF_KILL_TROLL, 8, "Kill Troll" },
    { OF_KILL_GIANT, 8, "Kill Giant" },

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

static void _calc_weapon_bonuses(obj_ptr obj, plr_attack_info_ptr info)
{
    int i;
    int to_hit = _calc_amount(_essences[_ESSENCE_TO_HIT], 1, 1);
    int to_dam = _calc_amount(_essences[_ESSENCE_TO_DAM], 1, 1);
    int to_dd = _calc_amount(_essences[_ESSENCE_XTRA_DICE], _rank_decay(64), 1);
    int blows = _calc_amount(_essences[OF_BLOWS], _rank_decay(32), 1);

    for (i = 0; i < OF_ARRAY_SIZE; i++)
        obj->flags[i] = 0;

    add_flag(obj->flags, OF_NO_REMOVE);

    for (i = 0; ; i++)
    {
        int j = _slay_flag_info[i].flag;
        if (j < 0) break;
        if (_essences[j] >= _slay_power(i))
        {
            add_flag(obj->flags, j);
            add_flag(obj->known_flags, j);
        }
    }

    info->xtra_blow += blows * _blows_mult();

    info->to_h += to_hit;
    info->dis_to_h += to_hit;

    info->to_d += to_dam;
    info->dis_to_d += to_dam;

    info->to_dd += to_dd;
}

static void _calc_bonuses(void) 
{
    int i;
    int to_a = plr_prorata_level(150);

    to_a += _calc_amount(_essences[_ESSENCE_AC], 2, 10);
    if (plr_mon_race_is_("/.death"))
        to_a -= 50;
    plr->to_a += to_a;
    plr->dis_to_a += to_a;

    plr->pspeed += 1;
    
    plr->levitation = TRUE;
    plr->no_cut = TRUE;
    res_add(GF_BLIND);
    res_add(GF_POIS);

    if (plr->lev >= 10)
        plr->pspeed += 1;

    if (plr->lev >= 25)
        plr->pspeed += 1;

    if (plr->lev >= 35)
        plr->pspeed += 2;

    if (plr->lev >= 45)
    {
        plr->pspeed += 2;
        plr->sh_retaliation = TRUE;
    }

    for (i = GF_RES_MIN; i <= GF_RES_MAX; i++)
    {
        int j = OF_RES_(i);
        int n = _calc_amount(_essences[j], _res_power(i), 1);

        for (; n; --n)
            res_add(i);
    }
    if (_essences[OF_IM_(GF_ACID)] >= 3)
        res_add_immune(GF_ACID);
    if (_essences[OF_IM_(GF_ELEC)] >= 3)
        res_add_immune(GF_ELEC);
    if (_essences[OF_IM_(GF_FIRE)] >= 3)
        res_add_immune(GF_FIRE);
    if (_essences[OF_IM_(GF_COLD)] >= 3)
        res_add_immune(GF_COLD);

    plr->life += 3*_calc_amount(_essences[OF_LIFE], 7, 1);

    plr->skills.stl += _calc_amount(_essences[OF_STEALTH], 2, 1);
    plr->pspeed += _calc_amount(_essences[OF_SPEED], 1, 10);
    plr->skills.dev += 8*_calc_amount(_essences[OF_MAGIC_MASTERY], 2, 1);
    plr->device_power += _calc_amount(_essences[OF_DEVICE_POWER], 2, 1);
    plr->skills.srh += 5*_calc_amount(_essences[OF_SEARCH], 2, 1);
    plr->skills.fos += 5*_calc_amount(_essences[OF_SEARCH], 2, 1);
    plr->see_infra += _calc_amount(_essences[OF_INFRA], 2, 1);
    plr->skill_dig += 20*_calc_amount(_essences[OF_TUNNEL], 2, 1);

    if (_essences[OF_SUST_STR] >= 5)
        plr->sustain_str = TRUE;
    if (_essences[OF_SUST_INT] >= 5)
        plr->sustain_int = TRUE;
    if (_essences[OF_SUST_WIS] >= 5)
        plr->sustain_wis = TRUE;
    if (_essences[OF_SUST_DEX] >= 5)
        plr->sustain_dex = TRUE;
    if (_essences[OF_SUST_CON] >= 5)
        plr->sustain_con = TRUE;
    if (_essences[OF_SUST_CHR] >= 5)
        plr->sustain_chr = TRUE;

    if (_essences[OF_TELEPATHY] >= 2)
        plr->telepathy = TRUE;
    if (_essences[OF_ESP_ANIMAL] >= 2)
        plr->esp_animal = TRUE;
    if (_essences[OF_ESP_UNDEAD] >= 2)
        plr->esp_undead = TRUE;
    if (_essences[OF_ESP_DEMON] >= 2)
        plr->esp_demon = TRUE;
    if (_essences[OF_ESP_ORC] >= 2)
        plr->esp_orc = TRUE;
    if (_essences[OF_ESP_TROLL] >= 2)
        plr->esp_troll = TRUE;
    if (_essences[OF_ESP_GIANT] >= 2)
        plr->esp_giant = TRUE;
    if (_essences[OF_ESP_DRAGON] >= 2)
        plr->esp_dragon = TRUE;
    if (_essences[OF_ESP_HUMAN] >= 2)
        plr->esp_human = TRUE;
    if (_essences[OF_ESP_EVIL] >= 2)
        plr->esp_evil = TRUE;
    if (_essences[OF_ESP_GOOD] >= 2)
        plr->esp_good = TRUE;
    if (_essences[OF_ESP_NONLIVING] >= 2)
        plr->esp_nonliving = TRUE;
    if (_essences[OF_ESP_UNIQUE] >= 2)
        plr->esp_unique = TRUE;

    if (_essences[OF_NO_MAGIC] >= 5)
        plr->anti_magic = TRUE;
    plr->free_act += _calc_amount(_essences[OF_FREE_ACT], 2, 1);
    plr->see_inv += _calc_amount(_essences[OF_SEE_INVIS], 3, 1);
    plr->hold_life += _calc_amount(_essences[OF_HOLD_LIFE], 3, 1);
    if (_essences[OF_SLOW_DIGEST] >= 2)
        plr->slow_digest = TRUE;
    plr->regen += 5 * _calc_amount(_essences[OF_REGEN], 2, 10);
    if (_essences[OF_REFLECT] >= 3)
        plr->reflect = TRUE;

    if (_essences[OF_AURA_FIRE] >= 7)
        plr->sh_fire = TRUE;
    if (_essences[OF_AURA_ELEC] >= 7)
        plr->sh_elec = TRUE;
    if (_essences[OF_AURA_COLD] >= 7)
        plr->sh_cold = TRUE;
}

static void _calc_stats(s16b stats[MAX_STATS])
{
    int i;
    for (i = 0; i < 6; i++)
        stats[i] += _calc_stat_bonus(OF_STR + i);
}

static void _get_flags(u32b flgs[OF_ARRAY_SIZE]) 
{
    int i;

    add_flag(flgs, OF_SPEED);
    add_flag(flgs, OF_LIGHT);
    add_flag(flgs, OF_RES_(GF_BLIND));
    add_flag(flgs, OF_RES_(GF_POIS));
    add_flag(flgs, OF_LEVITATION);

    for (i = 0; i < 6; i++) /* Assume in order */
    {
        if (i != 0) /* Bug: Giving TR_STR marks the player as cursed ... */
            _add_stat_flag(OF_STR + i, flgs);
        if (_essences[OF_SUST_STR + i] >= 5)
            add_flag(flgs, OF_SUST_STR + i);
    }

    for (i = GF_RES_MIN; i <= GF_RES_MAX; i++)
    {
        int j = OF_RES_(i);
        int n = _calc_amount(_essences[j], _res_power(i), 1);

        if (n)
            add_flag(flgs, j);
    }

    if (_calc_amount(_essences[OF_STEALTH], 2, 1))
        add_flag(flgs, OF_STEALTH);
    if (_calc_amount(_essences[OF_MAGIC_MASTERY], 2, 1))
        add_flag(flgs, OF_MAGIC_MASTERY);
    if (_calc_amount(_essences[OF_SEARCH], 2, 1))
        add_flag(flgs, OF_SEARCH);
    if (_calc_amount(_essences[OF_INFRA], 2, 1))
        add_flag(flgs, OF_INFRA);
    if (_calc_amount(_essences[OF_TUNNEL], 2, 1))
        add_flag(flgs, OF_TUNNEL);

    if (_essences[OF_TELEPATHY] >= 2)
        add_flag(flgs, OF_TELEPATHY);

    if (_essences[OF_ESP_EVIL] >= 2)
        add_flag(flgs, OF_ESP_EVIL);
    if (_essences[OF_ESP_GOOD] >= 2)
        add_flag(flgs, OF_ESP_GOOD);
    if (_essences[OF_ESP_NONLIVING] >= 2)
        add_flag(flgs, OF_ESP_NONLIVING);
    if (_essences[OF_ESP_UNIQUE] >= 2)
        add_flag(flgs, OF_ESP_UNIQUE);
    if (_essences[OF_ESP_DRAGON] >= 2)
        add_flag(flgs, OF_ESP_DRAGON);
    if (_essences[OF_ESP_DEMON] >= 2)
        add_flag(flgs, OF_ESP_DEMON);
    if (_essences[OF_ESP_UNDEAD] >= 2)
        add_flag(flgs, OF_ESP_UNDEAD);
    if (_essences[OF_ESP_ANIMAL] >= 2)
        add_flag(flgs, OF_ESP_ANIMAL);
    if (_essences[OF_ESP_HUMAN] >= 2)
        add_flag(flgs, OF_ESP_HUMAN);
    if (_essences[OF_ESP_ORC] >= 2)
        add_flag(flgs, OF_ESP_ORC);
    if (_essences[OF_ESP_TROLL] >= 2)
        add_flag(flgs, OF_ESP_TROLL);
    if (_essences[OF_ESP_GIANT] >= 2)
        add_flag(flgs, OF_ESP_GIANT);

    if (_essences[OF_NO_MAGIC] >= 5)
        add_flag(flgs, OF_NO_MAGIC);
    if (_calc_amount(_essences[OF_FREE_ACT], 2, 1))
        add_flag(flgs, OF_FREE_ACT);
    if (_calc_amount(_essences[OF_SEE_INVIS], 3, 1))
        add_flag(flgs, OF_SEE_INVIS);
    if (_calc_amount(_essences[OF_HOLD_LIFE], 3, 1))
        add_flag(flgs, OF_HOLD_LIFE);
    if (_essences[OF_SLOW_DIGEST] >= 2)
        add_flag(flgs, OF_SLOW_DIGEST);
    if (_calc_amount(_essences[OF_REGEN], 2, 10))
        add_flag(flgs, OF_REGEN);
    if (_essences[OF_REFLECT] >= 3)
        add_flag(flgs, OF_REFLECT);

    if (_essences[OF_AURA_FIRE] >= 7)
        add_flag(flgs, OF_AURA_FIRE);
    if (_essences[OF_AURA_ELEC] >= 7)
        add_flag(flgs, OF_AURA_ELEC);
    if (_essences[OF_AURA_COLD] >= 7)
        add_flag(flgs, OF_AURA_COLD);

    if (_essences[OF_IM_(GF_ACID)] >= 3)
        add_flag(flgs, OF_IM_(GF_ACID));
    if (_essences[OF_IM_(GF_ELEC)] >= 3)
        add_flag(flgs, OF_IM_(GF_ELEC));
    if (_essences[OF_IM_(GF_FIRE)] >= 3)
        add_flag(flgs, OF_IM_(GF_FIRE));
    if (_essences[OF_IM_(GF_COLD)] >= 3)
        add_flag(flgs, OF_IM_(GF_COLD));
}

/**********************************************************************
 * Powers
 **********************************************************************/
static void _absorb_spell(int cmd, var_ptr res)
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
        obj_prompt_t prompt = {0};
        char o_name[MAX_NLEN];

        var_set_bool(res, FALSE);
        prompt.prompt = "Absorb which item?";
        prompt.error = "You have nothing to absorb.";
        prompt.filter = obj_is_weapon;
        prompt.where[0] = INV_PACK;
        prompt.where[1] = INV_FLOOR;

        obj_prompt(&prompt);
        if (!prompt.obj) return;

        object_desc(o_name, prompt.obj, OD_NAME_ONLY);
        msg_format("You absorb the power of %s!", o_name);
        _absorb(prompt.obj);

        prompt.obj->number = 0;
        obj_release(prompt.obj, 0);

        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static bool _detect = FALSE;
static void _detect_obj(point_t pos, obj_ptr obj)
{
    int rng = DETECT_RAD_DEFAULT;
    if (!obj_is_weapon(obj)) return;
    if (plr_distance(pos) > rng) return;
    obj->marked |= OM_FOUND;
    plr->window |= PW_OBJECT_LIST;
    draw_pos(pos);
    _detect = TRUE;
}
static void _detect_pile(point_t pos, obj_ptr pile)
{
    obj_ptr obj;
    for (obj = pile; obj; obj = obj->next)
        _detect_obj(pos, obj);
}
static void _detect_spell(int cmd, var_ptr res)
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
        _detect = FALSE;
        dun_iter_floor_obj(cave, _detect_pile);
        if (detect_monsters_string(DETECT_RAD_DEFAULT, "|/")) _detect = TRUE;
        if (_detect) msg_print("You sense your kind.");
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _judge_spell(int cmd, var_ptr res)
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
        if (plr->lev >= 35)
            var_set_bool(res, identify_fully(obj_is_weapon));
        else
            var_set_bool(res, ident_spell(obj_is_weapon));
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

    plr_mon_race_set("|.broken");

    object_prep(&forge, lookup_kind(TV_SWORD, SV_BROKEN_SWORD));
    add_flag(forge.flags, OF_NO_REMOVE);
    forge.to_h =  1;
    forge.to_d =  3;
    plr_birth_obj(&forge);

    plr_birth_obj_aux(TV_STAFF, EFFECT_NOTHING, 1);
}

static obj_ptr _weapon(void)
{
    return equip_obj(plr->attack_info[0].slot);
}

static void _upgrade_weapon(int tval, int sval)
{
    obj_ptr me = _weapon();

    obj_loc_t loc = me->loc; /* XXX object_prep clobbers this XXX */
    object_prep(me, lookup_kind(tval, sval));
    me->loc = loc;

    me->to_h = plr->lev / 5;
    me->to_d = plr->lev / 3;

    add_flag(me->flags, OF_NO_REMOVE);
    obj_identify_fully(me);

    plr->update |= PU_BONUS;
    plr->window |= PW_INVEN | PW_EQUIP;
}

static void _gain_level(int new_level) 
{
    if (plr_mon_race_is_("|.broken") && new_level >= 10)
    {
        plr_mon_race_evolve("|.death");
        _upgrade_weapon(TV_SWORD, SV_LONG_SWORD);
    }
    if (plr_mon_race_is_("|.death") && new_level >= 25)
    {
        plr_mon_race_evolve("/.animated");
        _upgrade_weapon(TV_POLEARM, SV_BATTLE_AXE);
    }
    if (plr_mon_race_is_("/.animated") && new_level >= 35)
    {
        plr_mon_race_evolve("|.hell");
        _upgrade_weapon(TV_SWORD, SV_BLADE_OF_CHAOS);
    }
    if (plr_mon_race_is_("|.hell") && new_level >= 45)
    {
        plr_mon_race_evolve("/.death");
        _upgrade_weapon(TV_POLEARM, SV_DEATH_SCYTHE_HACK);
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
        doc_printf(doc, " %-18.18s %5d %5d %5.5s\n",
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
        doc_printf(doc, " %-18.18s %5d %5d %+5d\n",
            name,
            n, 
            _calc_needed(n, power, rep),
            _calc_amount(n, power, rep)
        );
    }
}

static void _dump_stats(doc_ptr doc)
{
    int i;
    doc_printf(doc, " <color:G>%-18.18s Total  Need Bonus</color>\n", "Stats");
    for (i = 0; i < 6; i++) /* Assume in order */
        _dump_bonus_flag(doc, OF_STR + i, 3, 1, stat_name_true[A_STR + i]);
    doc_newline(doc);
}

static void _dump_skills(doc_ptr doc)
{
    doc_printf(doc, " <color:G>%-18.18s Total  Need Bonus</color>\n", "Skills");
    _dump_bonus_flag(doc, _ESSENCE_TO_HIT, 1, 1, "To Hit");
    _dump_bonus_flag(doc, _ESSENCE_TO_DAM, 1, 1, "To Dam");
    _dump_bonus_flag(doc, _ESSENCE_AC, 2, 10, "To AC");
    _dump_bonus_flag(doc, OF_STEALTH, 2, 1, "Stealth");
    _dump_bonus_flag(doc, OF_SPEED, 1, 10, "Speed");
    if (_essences[OF_BLOWS])
    {
        int blows = _calc_amount(_essences[OF_BLOWS], _rank_decay(32), 1);

        blows = blows * _blows_mult();
        doc_printf(doc, " %-18.18s %5d %5d %5.5s\n",
            "Attacks",
            _essences[OF_BLOWS], 
            _calc_needed(_essences[OF_BLOWS], _rank_decay(32), 1),
            blows ? format("+%d.%2.2d", blows / 100, blows % 100) : ""
        );
    }
    _dump_bonus_flag(doc, _ESSENCE_XTRA_DICE, _rank_decay(64), 1, "Slaying");
    _dump_bonus_flag(doc, OF_LIFE, 7, 1, "Life");
    _dump_bonus_flag(doc, OF_SEARCH, 2, 1, "Searching");
    _dump_bonus_flag(doc, OF_INFRA, 2, 1, "Infravision");
    _dump_bonus_flag(doc, OF_TUNNEL, 2, 1, "Digging");
    _dump_bonus_flag(doc, OF_MAGIC_MASTERY, 2, 1, "Magic Mastery");
    _dump_bonus_flag(doc, OF_LIGHT, 5, 1, "Light");
    doc_newline(doc);
}

static void _dump_slays(doc_ptr doc)
{
    int i;
    doc_printf(doc, " <color:G>%-18.18s Total  Need Bonus</color>\n", "Slays");
    for (i = 0; ; i++)
    {
        int j = _slay_flag_info[i].flag;
        if (j < 0) break;
        _dump_ability_flag(doc, j, _slay_power(i), _slay_flag_info[i].name);
    }
    doc_newline(doc);
}

static void _dump_resists(doc_ptr doc)
{
    int i;
    doc_printf(doc, " <color:G>%-18.18s Total  Need Bonus</color>\n", "Resistances");
    for (i = GF_RES_MIN; i <= GF_RES_MAX; i++)
        _dump_bonus_flag(doc, OF_RES_(i), _res_power(i), 1, format("%^s", res_name(i)));

    _dump_ability_flag(doc, OF_IM_(GF_ACID), 3, "Immune Acid");
    _dump_ability_flag(doc, OF_IM_(GF_ELEC), 3, "Immune Elec");
    _dump_ability_flag(doc, OF_IM_(GF_FIRE), 3, "Immune Fire");
    _dump_ability_flag(doc, OF_IM_(GF_COLD), 3, "Immune Cold");

    doc_newline(doc);
}

static void _dump_abilities(doc_ptr doc)
{
    int i;
    doc_printf(doc, " <color:G>%-18.18s Total  Need Bonus</color>\n", "Abilities");
    _dump_bonus_flag(doc, OF_FREE_ACT, 2, 1, "Free Action");
    _dump_bonus_flag(doc, OF_SEE_INVIS, 3, 1, "See Invisible");
    _dump_bonus_flag(doc, OF_HOLD_LIFE, 3, 1, "Hold Life");
    _dump_ability_flag(doc, OF_SLOW_DIGEST, 2, "Slow Digestion");
    {
        int n = _essences[OF_REGEN];
        if (n > 0)
        {
            doc_printf(doc, " %-18.18s %5d %5d %+5d%%\n",
                "Regeneration",
                n, 
                _calc_needed(n, 2, 10),
                5*_calc_amount(n, 2, 10)
            );
        }
    }
    _dump_ability_flag(doc, OF_NO_MAGIC, 5, "Antimagic");
    _dump_ability_flag(doc, OF_REFLECT, 3, "Reflection");
    _dump_ability_flag(doc, OF_AURA_FIRE, 7, "Aura Fire");
    _dump_ability_flag(doc, OF_AURA_ELEC, 7, "Aura Elec");
    _dump_ability_flag(doc, OF_AURA_COLD, 7, "Aura Cold");
    for (i = 0; i < 6; i++) /* Assume in order */
        _dump_ability_flag(doc, OF_SUST_STR + i, 5, format("Sust %s", stat_name_true[A_STR + i]));
    doc_newline(doc);
}

static void _dump_esp(doc_ptr doc)
{
    doc_printf(doc, " <color:G>%-18.18s Total  Need Bonus</color>\n", "ESP");
    _dump_ability_flag(doc, OF_TELEPATHY, 2, "Telepathy");
    _dump_ability_flag(doc, OF_ESP_ANIMAL, 2, "ESP Animals");
    _dump_ability_flag(doc, OF_ESP_UNDEAD, 2, "ESP Undead");
    _dump_ability_flag(doc, OF_ESP_DEMON, 2, "ESP Demon");
    _dump_ability_flag(doc, OF_ESP_ORC, 2, "ESP Orc");
    _dump_ability_flag(doc, OF_ESP_TROLL, 2, "ESP Troll");
    _dump_ability_flag(doc, OF_ESP_GIANT, 2, "ESP Giant");
    _dump_ability_flag(doc, OF_ESP_DRAGON, 2, "ESP Dragon");
    _dump_ability_flag(doc, OF_ESP_HUMAN, 2, "ESP Human");
    _dump_ability_flag(doc, OF_ESP_EVIL, 2, "ESP Evil");
    _dump_ability_flag(doc, OF_ESP_GOOD, 2, "ESP Good");
    _dump_ability_flag(doc, OF_ESP_NONLIVING, 2, "ESP Nonliving");
    _dump_ability_flag(doc, OF_ESP_UNIQUE, 2, "ESP Unique");
    doc_newline(doc);
}

static void _character_dump(doc_ptr doc)
{
    doc_ptr cols[2];

    doc_printf(doc, "<topic:Essences>=================================== <color:keypress>E</color>ssences ==================================\n\n");

    cols[0] = doc_alloc(39);
    cols[1] = doc_alloc(39);

    _dump_stats(cols[0]);
    _dump_skills(cols[0]);
    _dump_abilities(cols[0]);
    _dump_esp(cols[0]);

    _dump_slays(cols[1]);
    _dump_resists(cols[1]);

    doc_insert_cols(doc, cols, 2, 1);
    doc_free(cols[0]);
    doc_free(cols[1]);
}

/**********************************************************************
 * Public
 **********************************************************************/
plr_race_ptr mon_sword_get_race(void)
{
    static plr_race_ptr me = NULL;
    static cptr   titles[5] =  {"Broken Death Sword", "Death Sword", "Poleaxe of Animated Attack", 
                                "Hellblade", "Death Scythe"};    
    int           rank = _rank();

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 25,  24,  40,   4,  20,   5,  56,  20};
    skills_t xs = { 60,  50,  60,   0,   0,   0, 100,  35};

        me = plr_race_alloc(RACE_MON_SWORD);
        me->skills = bs;
        me->extra_skills = xs;

        me->name = "Death-Sword";
        me->desc = "Death Swords are mighty weapons animated by magical means. As such, "
                    "they are unable to use equipment the way other players can. Instead, "
                    "they simply are a weapon of their current form. But never fear, Death "
                    "Swords have the power to absorb magical essences from the weapons they "
                    "find, gaining power in the process.";

        me->infra = 3;
        me->exp = 150;
        me->base_hp = 30;
        me->shop_adjust = 110; /* I think shopkeepers are puzzled, more than anything else! */

        me->hooks.calc_bonuses = _calc_bonuses;
        me->hooks.calc_stats = _calc_stats;
        me->hooks.calc_weapon_bonuses = _calc_weapon_bonuses;
        me->hooks.character_dump = _character_dump;
        me->hooks.get_flags = _get_flags;
        me->hooks.gain_level = _gain_level;
        me->hooks.get_powers = _get_powers;
        me->hooks.birth = _birth;

        me->hooks.load_player = _load;
        me->hooks.save_player = _save;
        me->hooks.destroy_object = _absorb_object;

        me->flags = RACE_IS_MONSTER | RACE_IS_NONLIVING;
        me->pseudo_class_id = CLASS_WARRIOR;
    }

    me->subname = titles[rank];
    me->stats[A_STR] =  1 + rank;
    me->stats[A_INT] = -5;
    me->stats[A_WIS] = -5;
    me->stats[A_DEX] =  0 + rank/2;
    me->stats[A_CON] =  1 + rank;
    me->stats[A_CHR] =  0;
    me->life = 85 + 5*rank;
    me->boss_r_idx = mon_race_parse("|.Stormbringer")->id;

    me->equip_template = plr_equip_template();

    if (birth_hack || spoiler_hack)
    {
        me->subname = NULL;
        me->subdesc = NULL;
    }
    return me;
}

static cptr _essence_name(int i)
{
    switch (i)
    {
    case _ESSENCE_AC: return "AC";
    case _ESSENCE_TO_HIT: return "Accuracy";
    case _ESSENCE_TO_DAM: return "Damage";
    case _ESSENCE_XTRA_DICE: return "Slaying";
    }
    return of_lookup(i)->name;
}
bool sword_disenchant(void)
{
    bool result = FALSE;
    int  r = _rank();
    int  i;

    for (i = 0; i < _MAX_ESSENCE; i++)
    {
        int old = _essences[i];
        int lose = 0;
        
        if (!old) continue;
        if (i == OF_SPEED) continue;
        if (res_save(GF_DISEN, 43)) continue;

        lose = 10*old*_1d(r)/20;
        _essences[i] -= lose/10;
        if (lose%10 && _1d(10) <= lose%10)
            _essences[i]--;
        
        if (_essences[i] < old)
        {
            if (0 || plr->wizard)
                msg_format("You lost %d essences of %s.", old - _essences[i], _essence_name(i));
            result = TRUE;
        }
    }
    if (result)
    {
        plr->update |= PU_BONUS;
        msg_print("<color:r>You feel power draining from your body!</color>");
    }
    return result;
}


int sword_calc_torch(void)
{
    return MAX(1, _calc_amount(_essences[OF_LIGHT], 5, 1));
}
