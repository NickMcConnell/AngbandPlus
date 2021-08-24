#include "angband.h"

#include <assert.h>

static cptr _desc = 
    "Golems are creatures animated by powerful magics: living stone conjured "
    "to serve their creators. But you have broken loose and are running amok!\n \n"
    "Golems are very slow when it comes to moving about, but this slowness does not "
    "affect their non-movement actions. Except for melee, where they are limited "
    "to just a single (but mighty) blow. They fight best with their powerful fists. "
    "Golems have a very high armor class, possess good resistances and saving throws, "
    "and are very strong in melee. They are also resistant to magic, and become "
    "more resistant as they evolve. This resistance is quite powerful, reducing the "
    "damage taken from all magical attacks.\n \n"
    "There are two varieties of golem, both sharing a common evolutionary "
    "heritage. First there is the mighty Colossus, the biggest (and slowest) of their "
    "kind. Next is the Sky Golem, a creature of powerful magic, resistant to the "
    "ravages of time.";

static cptr _mon_name(int r_idx)
{
    if (r_idx)
        return mon_race_lookup(r_idx)->name;
    return ""; /* Birth Menu */
}

static int _rank(void)
{
    int r = 0;
    if (plr->lev >= 10) r++;
    if (plr->lev >= 20) r++;
    if (plr->lev >= 30) r++;
    if (plr->lev >= 40) r++;
    if (plr->lev >= 45) r++;
    return r;
}

static void _birth(void) 
{ 
    object_type forge;

    plr_mon_race_set("g.clay");
    skills_innate_init("Punch", WEAPON_EXP_BEGINNER, WEAPON_EXP_MASTER);

    object_prep(&forge, lookup_kind(TV_HARD_ARMOR, SV_CHAIN_MAIL));
    plr_birth_obj(&forge);

    object_prep(&forge, lookup_kind(TV_BOOTS, SV_PAIR_OF_METAL_SHOD_BOOTS));
    plr_birth_obj(&forge);

    plr_birth_obj_aux(TV_STAFF, EFFECT_NOTHING, 1);
    plr_birth_light();
}

/**********************************************************************
 * Common Evolution: 
 *       10       20      30         40     45
 *  Clay -> Stone -> Iron -> Mithril -> Eog -> Colossus | Sky | Spellwarp
 **********************************************************************/
static void _gain_level(int new_level) 
{
    if (plr_mon_race_is_("g.clay") && new_level >= 10)
        plr_mon_race_evolve("g.stone");
    if (plr_mon_race_is_("g.stone") && new_level >= 20)
        plr_mon_race_evolve("g.iron");
    if (plr_mon_race_is_("g.iron") && new_level >= 30)
        plr_mon_race_evolve("g.mithril");
    if (plr_mon_race_is_("g.mithril") && new_level >= 40)
        plr_mon_race_evolve("g.eog");
    if (plr_mon_race_is_("g.eog") && new_level >= 45)
    {
        switch (plr->psubrace)
        {
        case GOLEM_COLOSSUS:
            plr_mon_race_evolve("g.colossus");
            break;
        case GOLEM_SKY:
            plr_mon_race_evolve("g.sky");
            break;
        }
    }
}

/**********************************************************************
 * Attacks
 **********************************************************************/
static int _attack_level_aux(int l)
{
    switch (plr->psubrace)
    {
    case GOLEM_COLOSSUS:
        l = MAX(1, l * 100 / 100);
        break;
    case GOLEM_SKY:
        l = MAX(1, l * 95 / 100);
        break;
    }
    return l;
}
static int _attack_level(void)
{
    return _attack_level_aux(plr->lev);
}
static dice_t _calc_dice(int dam, int pct_dice)
{
    dice_t dice = {0};
    int dice_dam = dam * pct_dice; /* scaled by 100 */
    int x = mysqrt(2*dice_dam); /* scaled by sqrt(100) = 10 */
    dice.dd = MAX(1, (x + 5)/10);
    dice.ds = MAX(5, (x + 5)/10);
    dice.base = MAX(0, dam - dice_avg_roll(dice));
    return dice;
}
static void _spoiler_dump(doc_ptr doc)
{
    int cl;
    for (cl = 1; cl <= 50; cl++)
    {
        int l = _attack_level_aux(cl);
        int dam = 25 + prorate(450, l, 50, 1, 1, 1);
        dice_t dice = _calc_dice(dam, 30);
        int avg_dam = dice_avg_roll(dice);

        doc_printf(doc, "%2d %3d ", cl, l);
        doc_printf(doc, "<color:%c>Punch(%dd%d+%d)=%d</color>\n",
            dam == avg_dam ? 'w' : 'r',
            dice.dd, dice.ds, dice.base, avg_dam);
    }
}
static void _calc_innate_attacks(void)
{
    int          l = _attack_level();
    int          dam = 25 + prorate(450, l, 50, 1, 1, 1);
    mon_blow_ptr blow = mon_blow_alloc(RBM_PUNCH);

    blow->power = l*BTH_PLUS_ADJ;
    blow->weight = 100 + 8*l;
    mon_blow_push_effect(blow, RBE_HURT, _calc_dice(dam, 30));
    if (l >= 30)
        mon_blow_push_effect(blow, GF_STUN, dice_create(3, 3 + l/16, 0))->pct = l;
    vec_add(plr->innate_blows, blow);
}
static void _calc_innate_bonuses(mon_blow_ptr blow)
{
    if (blow->method != RBM_PUNCH) return;
    plr->innate_attack_info.xtra_blow = 0;  /* never gain extra attacks */
    blow->blows = 100;
    if (!equip_find_first(obj_is_shield)) /* give 2-handed wielding bonus */
    {
        int to_h = ((int)(adj_str_th[plr->stat_ind[A_STR]]) - 128) + ((int)(adj_dex_th[plr->stat_ind[A_DEX]]) - 128);
        int to_d = ((int)(adj_str_td[plr->stat_ind[A_STR]]) - 128) * 3/4;

        plr->innate_attack_info.to_h += to_h;
        plr->innate_attack_info.dis_to_h += to_h;
        plr->innate_attack_info.to_d += to_d;
        plr->innate_attack_info.dis_to_d += to_d;
    }
}
static bool _begin_weapon(plr_attack_ptr context)
{
    if (context->info.type != PAT_INNATE) return TRUE;
    if (context->blow->method != RBM_PUNCH) return TRUE;
    if (!context->blow->blows) return FALSE;
    context->skill = context->skill * (100 + 2*_attack_level()) / 100;
    return TRUE;
}
static void _attack_init(plr_attack_ptr context)
{
    context->hooks.begin_weapon_f = _begin_weapon;
}

/**********************************************************************
 * Bonuses
 **********************************************************************/
static void _calc_bonuses(void) 
{
    /* Clay Golem */
    plr->to_a += 5;
    plr->dis_to_a += 5;
    plr->free_act++;
    plr->hold_life++;
    res_add(GF_POIS);

    /* Stone Golem */
    if (plr->lev >= 10)
    {
        plr->to_a += 5;
        plr->dis_to_a += 5;
        res_add(GF_FEAR);
    }

    /* Iron Golem */
    if (plr->lev >= 20)
    {
        plr->to_a += 10;
        plr->dis_to_a += 10;
        plr->see_inv++;
        res_add(GF_FIRE);
        res_add(GF_COLD);
        res_add(GF_ELEC);
        plr->magic_resistance += 5;
    }

    /* Mithril Golem */
    if (plr->lev >= 30)
    {
        plr->to_a += 10;
        plr->dis_to_a += 10;
        plr->pspeed -= 1;
        plr->reflect = TRUE;
        res_add(GF_CONF);
        res_add(GF_SHARDS);
        plr->magic_resistance += 5;
    }

    /* Eog Golem */
    if (plr->lev >= 40)
    {
        plr->to_a += 10;            /* +40 */
        plr->dis_to_a += 10;
        plr->pspeed -= 1;
        plr->magic_resistance += 5; /* 15% */
    }

    if (plr_mon_race_is_("g.colossus"))
    {
        res_add(GF_SOUND);
        res_add(GF_DISEN);
        plr->pspeed -= 3;
        plr->to_a += 35;
        plr->dis_to_a += 35;
        plr->magic_resistance += 5;
    }
    else if (plr_mon_race_is_("g.sky"))
    {
        res_add(GF_COLD);
        res_add(GF_TIME);
        res_add(GF_LIGHT);
        plr->to_a += 10;
        plr->dis_to_a += 10;
        plr->magic_resistance += 5;
    }
}

static void _get_flags(u32b flgs[OF_ARRAY_SIZE]) 
{
    /* Clay Golem */
    add_flag(flgs, OF_FREE_ACT);
    add_flag(flgs, OF_HOLD_LIFE);
    add_flag(flgs, OF_RES_(GF_POIS));

    /* Stone Golem */
    if (plr->lev >= 10)
    {
        add_flag(flgs, OF_RES_(GF_FEAR));
    }

    /* Iron Golem */
    if (plr->lev >= 20)
    {
        add_flag(flgs, OF_SEE_INVIS);
        add_flag(flgs, OF_RES_(GF_FIRE));
        add_flag(flgs, OF_RES_(GF_COLD));
        add_flag(flgs, OF_RES_(GF_ELEC));
        add_flag(flgs, OF_MAGIC_RESISTANCE);
    }

    /* Mithril Golem */
    if (plr->lev >= 30)
    {
        add_flag(flgs, OF_DEC_SPEED);
        add_flag(flgs, OF_RES_(GF_CONF));
        add_flag(flgs, OF_RES_(GF_SHARDS));
        add_flag(flgs, OF_REFLECT);
    }

    /* Eog Golem */
    if (plr->lev >= 40)
    {
    }

    if (plr_mon_race_is_("g.colossus"))
    {
        add_flag(flgs, OF_RES_(GF_SOUND));
        add_flag(flgs, OF_RES_(GF_DISEN));
    }
    else if (plr_mon_race_is_("g.sky"))
    {
        add_flag(flgs, OF_RES_(GF_TIME));
        add_flag(flgs, OF_RES_(GF_COLD));
        add_flag(flgs, OF_RES_(GF_LIGHT));
    }
}

/**********************************************************************
 * Powers
 **********************************************************************/
static bool _check_punch(void)
{
    mon_blow_ptr punch = mon_blows_find(plr->innate_blows, RBM_PUNCH);
    if (!punch) return FALSE; /* paranoia */
    if (!punch->blows) return FALSE;
    return TRUE;
}
static void _big_punch_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Big Punch");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attack an adjacent opponent with a single devastating blow.");
        break;
    case SPELL_ON_BROWSE:
    case SPELL_CAST:
        var_set_bool(res, FALSE);
        if (_check_punch())
        {
            if (cmd == SPELL_CAST)
                var_set_bool(res, plr_attack_special(PLR_HIT_CRIT, PAC_NO_WEAPON));
            else
            {
                plr_attack_display_special(PLR_HIT_CRIT, PAC_NO_WEAPON);
                var_set_bool(res, TRUE);
            }
        }
        else
            msg_print("You need to use your bare fists for this power.");
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
void _breathe_cold_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Breathe Cold");
        break;
    case SPELL_DESC:
        var_set_string(res, "Breathes cold at chosen target");
        break;
    default:
        breath_spell_innate(cmd, res, 3, GF_COLD, plr->chp/2);
    }
}
void _breathe_disintegration_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Breathe Disintegration");
        break;
    case SPELL_DESC:
        var_set_string(res, "A disintegration breath. Not even the dungeon walls can withstand its power!");
        break;
    default:
        breath_spell_innate(cmd, res, 3, GF_DISINTEGRATE, plr->chp/4);
    }
}
void _breathe_light_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Breathe Light");
        break;
    case SPELL_DESC:
        var_set_string(res, "Breathes light at chosen target");
        break;
    default:
        breath_spell_innate(cmd, res, 3, GF_LIGHT, plr->chp/3);
    }
}
void _breathe_time_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Breathe Time");
        break;
    case SPELL_DESC:
        var_set_string(res, "Breathes time at chosen target");
        break;
    default:
        breath_spell_innate(cmd, res, 3, GF_TIME, plr->chp/5);
    }
}
void _shoot_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Shoot");
        break;
    case SPELL_DESC:
        var_set_string(res, "Shoot a missile at chosen target.");
        break;
    default:
        bolt_spell_aux(cmd, res, GF_MISSILE, dice_create(15, 15, 0));
    }
}
static void _stone_smash_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Stone Smash");
        break;
    case SPELL_DESC:
        var_set_string(res, "Destroys adjacent targeted wall.");
        break;
    case SPELL_CAST:
    {
        int dir;
        point_t pos;
        
        var_set_bool(res, FALSE);
        if (!get_rep_dir2(&dir)) return;
        if (dir == 5) return;

        pos = point_step(plr->pos, dir);
        if (!dun_pos_interior(cave, pos)) return;

        dun_tunnel(cave, pos, ACTION_FORCE | ACTION_QUIET);
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}
static power_info _powers[] = 
{
    { A_STR, {  5,  5, 30, _stone_smash_spell} },
    { A_CON, { 10, 10, 30, stone_skin_spell} },
    { A_STR, { 40,100,  0, _big_punch_spell} },
    {    -1, { -1, -1, -1, NULL} }
};
static power_info _colossus_powers[] = 
{
    { A_STR, { 45, 15, 50, _shoot_spell} },
    {    -1, { -1, -1, -1, NULL} }
};
static power_info _sky_powers[] = 
{
    { A_STR, { 45, 10, 50, ice_bolt_spell} },
    { A_CON, { 45, 30, 50, _breathe_light_spell} },
    { A_CON, { 45, 30, 50, _breathe_cold_spell} },
    { A_CON, { 50, 35, 60, _breathe_time_spell} },
    {    -1, { -1, -1, -1, NULL} }
};
static int _get_powers(spell_info* spells, int max) 
{
    int ct = get_powers_aux(spells, max, _powers);
    if (plr_mon_race_is_("g.colossus"))
        ct += get_powers_aux(spells + ct, max - ct, _colossus_powers);
    else if (plr_mon_race_is_("g.sky"))
        ct += get_powers_aux(spells + ct, max - ct, _sky_powers);
    return ct;
}

static name_desc_t _info[GOLEM_MAX] = {
    { "Colossus", "The Colossus is the biggest of all golems, truly immense. "
                  "Unfortunately, they are also the slowest of all golems on "
                  "account of their great size." },
    { "Sky Golem", "The Sky Golem is the product of powerful enchantments, resistant "
                   "to the ravages of time. They may even breathe time!" },
};

/**********************************************************************
 * Public
 **********************************************************************/
plr_race_ptr mon_golem_get_race(int psubrace)
{
    static plr_race_ptr me = NULL;
    int           rank = _rank();

    if (birth_hack && psubrace >= GOLEM_MAX)
        psubrace = 0;

    assert(0 <= psubrace && psubrace < GOLEM_MAX);

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 25,  18,  40,   0,  10,   7,  70,  30};
    skills_t xs = { 50,  35,  75,   0,   0,   0, 150,  50};

        me = plr_race_alloc(RACE_MON_GOLEM);

        me->name = "Golem";
        me->desc = _desc;
        me->skills = bs;
        me->extra_skills = xs;
        me->infra = 5;
        me->shop_adjust = 130;
        me->base_hp = 50;
        me->pseudo_class_id = CLASS_MAULER;
        me->boss_r_idx = mon_race_parse("g.Destroyer")->id;
        me->flags = RACE_IS_MONSTER | RACE_IS_NONLIVING;

        me->hooks.attack_init = _attack_init;
        me->hooks.calc_innate_attacks = _calc_innate_attacks;
        me->hooks.calc_innate_bonuses = _calc_innate_bonuses;
        me->hooks.birth = _birth;
        me->hooks.gain_level = _gain_level;
        me->hooks.calc_bonuses = _calc_bonuses;
        me->hooks.get_powers = _get_powers;
        me->hooks.get_flags = _get_flags;
    }

    me->subid = psubrace;
    me->subname = _mon_name(plr->current_r_idx);

    me->stats[A_STR] =  1 + rank;
    me->stats[A_INT] = -2 - (rank+1)/2;
    me->stats[A_WIS] = -2 - (rank+1)/2;
    me->stats[A_DEX] = -1 - (rank+1)/2;
    me->stats[A_CON] =  1 + rank;
    me->stats[A_CHR] =  0 + (rank+1)/2;

    switch (psubrace)
    {
    case GOLEM_SKY:
        me->life = 100 + 2*rank;
        me->exp = 250;
        break;

    case GOLEM_COLOSSUS:
    default:
        if (plr_mon_race_is_("g.colossus"))
        {
            me->stats[A_STR] += 3;
            me->stats[A_DEX] -= 2;
            me->stats[A_CON] += 3;
        }
        me->life = 100 + 5*rank;
        me->exp = 200;
        break;
    }

    if (birth_hack || spoiler_hack)
    {
        me->subname = _info[psubrace].name;
        me->subdesc = _info[psubrace].desc;
    }
    me->hooks.character_dump = plr->wizard ? _spoiler_dump : NULL;

    return me;
}

