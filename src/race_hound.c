#include "angband.h"

#include <assert.h>

/**********************************************************************
 * Hound Evolution
 **********************************************************************/
#define _MAX_PER_TIER 10
#define _MAX_TIERS     7

typedef struct {
    int level;
    cptr r_ids[_MAX_PER_TIER];
} _tier_t;

static _tier_t _tiers[_MAX_TIERS] = {
    {  1, { "Z.clear", NULL } },
    {  8, { "Z.light", "Z.shadow", NULL } },
    { 15, { "Z.fire", "Z.cold", "Z.energy", "Z.air", "Z.water", NULL } },
    { 23, { "Z.earth", "Z.vibration", "Z.nexus", "Z.multi-hued", NULL } },
    { 31, { "Z.inertia", "Z.impact", "Z.nether", NULL } },
    { 39, { "Z.gravity", "Z.time", "Z.plasma", "Z.chaos", NULL } },
    { 47, { "Z.tindalos", "Z.mana", "Z.aether", NULL } },
};

static int _count(cptr list[])
{
    int i;
    for (i = 0; ; i++)
    {
        if (!list[i]) return i;
        assert(mon_race_parse(list[i]));
    }
    /* return 0;  error: missing sentinel ... unreachable */
}
static cptr _random(cptr list[])
{
    if (spoiler_hack)
        return list[0];
    return list[randint0(_count(list))];
}
static int _find(sym_t what, cptr list[])
{
    int i;
    for (i = 0; ; i++)
    {
        cptr s = list[i];
        if (!s) return -1;
        if (sym_equals(what, s)) return i;
    }
    /* return -1;  unreachable */
}
static int _find_tier(sym_t r_id)
{
    int i;
    for (i = 0; i < _MAX_TIERS; i++)
    {
        if (_find(r_id, _tiers[i].r_ids) >= 0) return i;
    }
    return -1;
}
static cptr _mon_name(int r_idx)
{
    if (r_idx)
        return mon_race_lookup(r_idx)->name;
    return ""; /* Birth Menu */
}

/**********************************************************************
 * Hound Equipment
 **********************************************************************/
static void _birth(void)
{
    object_type    forge;

    plr_mon_race_set("Z.clear");
    skills_innate_init("Claw", WEAPON_EXP_BEGINNER, WEAPON_EXP_MASTER);
    skills_innate_init("Bite", WEAPON_EXP_BEGINNER, WEAPON_EXP_MASTER);

    object_prep(&forge, lookup_kind(TV_RING, 0));
    forge.name2 = EGO_RING_COMBAT;
    forge.to_d = 3;
    add_flag(forge.flags, OF_MELEE);
    plr_birth_obj(&forge);

    object_prep(&forge, lookup_kind(TV_BOOTS, SV_PAIR_OF_METAL_SHOD_BOOTS));
    plr_birth_obj(&forge);

    plr_birth_food();
    plr_birth_light();
}

/**********************************************************************
 * Hound Attacks
 **********************************************************************/
void hound_calc_innate_bonuses(mon_blow_ptr blow)
{
    if (blow->method == RBM_CLAW)
    {
        plr->innate_attack_info.blows_calc.wgt = 100;
        plr->innate_attack_info.blows_calc.mul = 40;
        plr->innate_attack_info.blows_calc.max = 200;
        plr_calc_blows_innate(blow);
    }
    if (blow->method == RBM_BITE)
    {
        plr->innate_attack_info.blows_calc.wgt = 200;
        plr->innate_attack_info.blows_calc.mul = 40;
        plr->innate_attack_info.blows_calc.max = 300;
        plr_calc_blows_innate(blow);
    }
}
void hound_calc_innate_attacks(void)
{
    int l = plr->lev;
    int to_d = plr_prorata_level(15);
    mon_blow_ptr blow;

    /* Claws */
    blow = mon_blow_alloc(RBM_CLAW);
    blow->power = l*3/2;
    blow->weight = 100;
    mon_blow_push_effect(blow, RBE_HURT, dice_create(1 + l/17, 3 + l/21, to_d));
    hound_calc_innate_bonuses(blow);
    vec_add(plr->innate_blows, blow);

    /* Bite */
    blow = mon_blow_alloc(RBM_BITE);
    blow->power = l*3/2;
    blow->weight = 200;
    mon_blow_push_effect(blow, RBE_HURT, dice_create(1 + l/23, 4 + l/5, to_d));
    if (plr_mon_race_is_("Z.tindalos"))
        mon_blow_push_effect(blow, GF_TIME, dice_create(6, 6, 0))->pct = 50;
    hound_calc_innate_bonuses(blow);
    vec_add(plr->innate_blows, blow);
}

/**********************************************************************
 * Hound Breath
 **********************************************************************/
static int _breath_effect(void)
{
    mon_race_ptr r = plr_mon_race();
    mon_spell_group_ptr g;
    mon_spell_ptr s;

    assert(r->spells); /* should have evolved by now ... */
    if (!r->spells) return GF_FIRE;

    g = r->spells->groups[MST_BREATH];
    if (!g) return GF_FIRE;
    if (!g->count) return GF_FIRE;

    s = &g->spells[randint0(g->count)];
    return s->id.effect;
}

static int _breath_amount(void)
{
    int pct = 15 + plr_prorata_level(15);
    return MAX(5, plr->chp * pct / 100);
}

static cptr _breath_desc(void)
{
    if (plr_mon_race_is_("Z.aether")) return "almost anything";
    if (plr_mon_race_is_("Z.tindalos")) return "nether or time";
    if (plr_mon_race_is_("Z.multi-hued")) return "acid, fire, cold, lightning or poison";
    return gf_name(_breath_effect());
}

static void _breathe_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Breathe");
        break;
    case SPELL_DESC:
        var_printf(res, "Breathes %s at your opponent.", _breath_desc());
        break;
    case SPELL_COST_EXTRA: {
        int l = plr->lev;
        int cst = l*l/100;
        var_set_int(res, cst);
        break; }
    default:
        breath_spell_innate(cmd, res, 1 + plr->lev/20, _breath_effect(), _breath_amount());
    }
}

void _bark_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Bark");
        break;
    case SPELL_DESC:
        var_set_string(res, "You will make enough noise to wake the neighbors.");
        break;
    case SPELL_CAST:
        msg_print("Woof! Woof!");
        aggravate_monsters(who_create_plr());
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void hound_leap_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Pounce");
        break;
    case SPELL_DESC:
        var_set_string(res, "Leap towards a nearby prey and attack in a single action.");
        break;
    default:
        rush_attack_spell(cmd, res);
        break;
    }
}

void hound_run_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Run");
        break;
    case SPELL_DESC:
        var_set_string(res, "By moving quickly, you will be able to run down your prey or to flee quickly in a pinch.");
        break;
    default:
        quick_walk_spell(cmd, res);
        break;
    }
}

void hound_sniff_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Sniff");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attempt to smell a nearby meal.");
        break;
    case SPELL_CAST:
        detect_monsters_living(DETECT_RAD_DEFAULT, "You smell nearby monsters.");
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void hound_stalk_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Stalk");
        break;
    case SPELL_DESC:
        var_set_string(res, "By moving slowly, you will be able to sneak up on your prey very effectively.");
        break;
    case SPELL_CAST:
        if (plr->action == ACTION_STALK) set_action(ACTION_NONE);
        else set_action(ACTION_STALK);
        var_set_bool(res, TRUE);
        break;
    case SPELL_ENERGY:
        var_set_int(res, 0);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

/**********************************************************************
 * Hounds: Evolution is tier based with a random choice from each tier.
 **********************************************************************/
static power_info _powers[] = {
    { A_DEX, {  1,  0,  0, _bark_spell } },
    { A_DEX, {  1,  1, 30, hound_sniff_spell } },
    { A_CON, {  8,  5, 30, _breathe_spell}},
    { A_DEX, { 10,  0,  0, hound_stalk_spell}},
    { A_DEX, { 15,  0,  0, hound_run_spell}},
    { A_DEX, { 20, 10, 30, hound_leap_spell}},
    {    -1, { -1, -1, -1, NULL}}
};
static power_info _tindalos_powers[] = {
    { A_DEX, {  5,  2, 30, phase_door_spell}},
    { A_DEX, { 25, 15, 50, teleport_to_spell}},
    {    -1, { -1, -1, -1, NULL}}
};

static int _get_powers(spell_info* spells, int max) {
    int ct = get_powers_aux(spells, max, _powers);
    if (plr_mon_race_is_("Z.tindalos"))
        ct += get_powers_aux(spells + ct, max - ct, _tindalos_powers);
    return ct;
}
static void _calc_bonuses(void) {
    int to_a = plr_prorata_level_aux(25, 1, 2, 2);
    int tier = _find_tier(plr->current_r_idx);

    plr->skill_dig += 100;
    plr->to_a += to_a;
    plr->dis_to_a += to_a;

    if (tier > 3)
        plr->pspeed += (tier - 3);

    /* Tier 1 */
    if (plr_mon_race_is_("Z.light"))
        res_add(GF_LIGHT);
    else if (plr_mon_race_is_("Z.shadow"))
    {
        res_add(GF_DARK);
        res_add_vuln(GF_LIGHT);
    }
    /* Tier 2 */
    else if (plr_mon_race_is_("Z.fire"))
    {
        res_add(GF_FIRE);
        res_add(GF_FIRE);
        res_add_vuln(GF_COLD);
    }
    else if (plr_mon_race_is_("Z.cold"))
    {
        res_add(GF_COLD);
        res_add(GF_COLD);
        res_add_vuln(GF_FIRE);
    }
    else if (plr_mon_race_is_("Z.energy"))
    {
        res_add(GF_ELEC);
        res_add(GF_ELEC);
    }
    else if (plr_mon_race_is_("Z.air"))
    {
        res_add(GF_POIS);
        res_add(GF_POIS);
    }
    else if (plr_mon_race_is_("Z.water"))
    {
        res_add(GF_ACID);
        res_add(GF_ACID);
    }
    /* Tier 3 */
    else if (plr_mon_race_is_("Z.earth"))
    {
        res_add(GF_SHARDS);
    }
    else if (plr_mon_race_is_("Z.vibration"))
    {
        res_add(GF_SOUND);
        res_add(GF_CONF);
        plr->free_act++;
    }
    else if (plr_mon_race_is_("Z.nexus"))
    {
        res_add(GF_NEXUS);
        res_add(GF_CONF);
        plr->free_act++;
    }
    else if (plr_mon_race_is_("Z.multi-hued"))
    {
        plr->pspeed += 3;
        res_add(GF_FIRE);
        res_add(GF_COLD);
        res_add(GF_ACID);
        res_add(GF_ELEC);
        res_add(GF_POIS);
        res_add(GF_CONF);
        plr->free_act++;
    }
    /* Tier 4 */
    else if (plr_mon_race_is_("Z.inertia"))
    {
        res_add(GF_CONF);
        plr->free_act++;
        plr->no_slow = TRUE;
    }
    else if (plr_mon_race_is_("Z.impact"))
    {
        res_add(GF_CONF);
        plr->free_act++;
    }
    else if (plr_mon_race_is_("Z.nether"))
    {
        plr->pspeed += 5;
        res_add(GF_NETHER);
        res_add(GF_CONF);
        plr->free_act++;
    }
    /* Tier 5 */
    else if (plr_mon_race_is_("Z.gravity"))
    {
        res_add(GF_CONF);
        plr->free_act++;
    }
    else if (plr_mon_race_is_("Z.time"))
    {
        plr->pspeed += 7;
        res_add(GF_TIME);
        res_add(GF_CONF);
        plr->free_act++;
    }
    else if (plr_mon_race_is_("Z.plasma"))
    {
        plr->pspeed += 5;
        res_add(GF_ELEC);
        res_add(GF_FIRE);
        res_add(GF_CONF);
        plr->free_act++;
    }
    else if (plr_mon_race_is_("Z.chaos"))
    {
        plr->pspeed += 5;
        res_add(GF_CHAOS);
        res_add(GF_CONF);
        plr->free_act++;
    }
    /* Tier 6 */
    else if (plr_mon_race_is_("Z.tindalos"))
    {
        plr->pspeed += 7;
        res_add(GF_NETHER);
        res_add(GF_TIME);
        res_add(GF_CONF);
        plr->free_act++;
        plr->levitation = TRUE;
        plr->pass_wall = TRUE;
        plr->no_passwall_dam = TRUE;
    }
    else if (plr_mon_race_is_("Z.mana"))
    {
        plr->pspeed += 10;
        res_add(GF_CONF);
        plr->free_act++;
    }
    else if (plr_mon_race_is_("Z.aether"))
    {
        plr->pspeed += 5;
        res_add(GF_FIRE);
        res_add(GF_COLD);
        res_add(GF_ACID);
        res_add(GF_ELEC);
        res_add(GF_POIS);
        res_add(GF_LIGHT);
        res_add(GF_DARK);
        res_add(GF_CONF);
        res_add(GF_NETHER);
        res_add(GF_NEXUS);
        res_add(GF_SOUND);
        res_add(GF_SHARDS);
        res_add(GF_CHAOS);
        res_add(GF_DISEN);
        res_add(GF_TIME);
        plr->sh_cold = TRUE;
        plr->sh_fire = TRUE;
        plr->sh_elec = TRUE;
        plr->free_act++;
    }
}

static void _get_flags(u32b flgs[OF_ARRAY_SIZE]) {
    int tier = _find_tier(plr->current_r_idx);
    if (tier > 3)
        add_flag(flgs, OF_SPEED);

    /* Tier 1 */
    if (plr_mon_race_is_("Z.light"))
    {
        add_flag(flgs, OF_RES_(GF_LIGHT));
    }
    else if (plr_mon_race_is_("Z.shadow"))
    {
        add_flag(flgs, OF_RES_(GF_DARK));
        add_flag(flgs, OF_VULN_(GF_LIGHT));
    }
    /* Tier 2 */
    else if (plr_mon_race_is_("Z.fire"))
    {
        add_flag(flgs, OF_RES_(GF_FIRE));
        add_flag(flgs, OF_VULN_(GF_COLD));
    }
    else if (plr_mon_race_is_("Z.cold"))
    {
        add_flag(flgs, OF_RES_(GF_COLD));
        add_flag(flgs, OF_VULN_(GF_FIRE));
    }
    else if (plr_mon_race_is_("Z.energy"))
    {
        add_flag(flgs, OF_RES_(GF_ELEC));
    }
    else if (plr_mon_race_is_("Z.air"))
    {
        add_flag(flgs, OF_RES_(GF_POIS));
    }
    else if (plr_mon_race_is_("Z.water"))
    {
        add_flag(flgs, OF_RES_(GF_ACID));
    }
    /* Tier 3 */
    else if (plr_mon_race_is_("Z.earth"))
    {
        add_flag(flgs, OF_RES_(GF_SHARDS));
    }
    else if (plr_mon_race_is_("Z.vibration"))
    {
        add_flag(flgs, OF_RES_(GF_SOUND));
        add_flag(flgs, OF_RES_(GF_CONF));
        add_flag(flgs, OF_FREE_ACT);
    }
    else if (plr_mon_race_is_("Z.nexus"))
    {
        add_flag(flgs, OF_RES_(GF_NEXUS));
        add_flag(flgs, OF_RES_(GF_CONF));
        add_flag(flgs, OF_FREE_ACT);
    }
    else if (plr_mon_race_is_("Z.multi-hued"))
    {
        add_flag(flgs, OF_SPEED);
        add_flag(flgs, OF_RES_(GF_FIRE));
        add_flag(flgs, OF_RES_(GF_COLD));
        add_flag(flgs, OF_RES_(GF_ACID));
        add_flag(flgs, OF_RES_(GF_ELEC));
        add_flag(flgs, OF_RES_(GF_POIS));
        add_flag(flgs, OF_RES_(GF_CONF));
        add_flag(flgs, OF_FREE_ACT);
    }
    /* Tier 4 */
    else if (plr_mon_race_is_("Z.inertia"))
    {
        add_flag(flgs, OF_RES_(GF_CONF));
        add_flag(flgs, OF_FREE_ACT);
    }
    else if (plr_mon_race_is_("Z.impact"))
    {
        add_flag(flgs, OF_RES_(GF_CONF));
        add_flag(flgs, OF_FREE_ACT);
    }
    else if (plr_mon_race_is_("Z.nether"))
    {
        add_flag(flgs, OF_SPEED);
        add_flag(flgs, OF_RES_(GF_NETHER));
        add_flag(flgs, OF_RES_(GF_CONF));
        add_flag(flgs, OF_FREE_ACT);
    }
    /* Tier 5 */
    else if (plr_mon_race_is_("Z.gravity"))
    {
        add_flag(flgs, OF_RES_(GF_CONF));
        add_flag(flgs, OF_FREE_ACT);
    }
    else if (plr_mon_race_is_("Z.time"))
    {
        add_flag(flgs, OF_SPEED);
        add_flag(flgs, OF_RES_(GF_TIME));
        add_flag(flgs, OF_RES_(GF_CONF));
        add_flag(flgs, OF_FREE_ACT);
    }
    else if (plr_mon_race_is_("Z.plasma"))
    {
        add_flag(flgs, OF_SPEED);
        add_flag(flgs, OF_RES_(GF_ELEC));
        add_flag(flgs, OF_RES_(GF_FIRE));
        add_flag(flgs, OF_RES_(GF_CONF));
        add_flag(flgs, OF_FREE_ACT);
    }
    else if (plr_mon_race_is_("Z.chaos"))
    {
        add_flag(flgs, OF_SPEED);
        add_flag(flgs, OF_RES_(GF_CHAOS));
        add_flag(flgs, OF_RES_(GF_CONF));
        add_flag(flgs, OF_FREE_ACT);
    }
    /* Tier 6 */
    else if (plr_mon_race_is_("Z.tindalos"))
    {
        add_flag(flgs, OF_SPEED);
        add_flag(flgs, OF_RES_(GF_NETHER));
        add_flag(flgs, OF_RES_(GF_TIME));
        add_flag(flgs, OF_RES_(GF_CONF));
        add_flag(flgs, OF_FREE_ACT);
        add_flag(flgs, OF_LEVITATION);
    }
    else if (plr_mon_race_is_("Z.mana"))
    {
        add_flag(flgs, OF_SPEED);
        add_flag(flgs, OF_RES_(GF_CONF));
        add_flag(flgs, OF_FREE_ACT);
    }
    else if (plr_mon_race_is_("Z.aether"))
    {
        add_flag(flgs, OF_SPEED);
        add_flag(flgs, OF_RES_(GF_FIRE));
        add_flag(flgs, OF_RES_(GF_COLD));
        add_flag(flgs, OF_RES_(GF_ACID));
        add_flag(flgs, OF_RES_(GF_ELEC));
        add_flag(flgs, OF_RES_(GF_POIS));
        add_flag(flgs, OF_RES_(GF_LIGHT));
        add_flag(flgs, OF_RES_(GF_DARK));
        add_flag(flgs, OF_RES_(GF_CONF));
        add_flag(flgs, OF_RES_(GF_NETHER));
        add_flag(flgs, OF_RES_(GF_NEXUS));
        add_flag(flgs, OF_RES_(GF_SOUND));
        add_flag(flgs, OF_RES_(GF_SHARDS));
        add_flag(flgs, OF_RES_(GF_CHAOS));
        add_flag(flgs, OF_RES_(GF_DISEN));
        add_flag(flgs, OF_RES_(GF_TIME));
        add_flag(flgs, OF_AURA_COLD);
        add_flag(flgs, OF_AURA_FIRE);
        add_flag(flgs, OF_AURA_ELEC);
        add_flag(flgs, OF_FREE_ACT);
    }
}
static void _gain_level(int new_level) {
    int tier = _find_tier(plr->current_r_idx);
    if (tier < 0 || tier == _MAX_TIERS - 1) return;
    if (plr->lev >= _tiers[tier + 1].level)
    {
        cptr which = _random(_tiers[tier+1].r_ids);
        plr_mon_race_evolve(which);
    }
}
plr_race_ptr mon_hound_get_race(void)
{
    static plr_race_ptr me = NULL;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 25,  20,  31,   4,  20,  15,  56,  30};
    skills_t xs = { 40,  40,  50,   5,   0,   0, 100,  35};

        me = plr_race_alloc(RACE_MON_HOUND);
        me->name = "Hound";
        me->desc = "While Hounds typically hunt in packs, you have chosen to go it alone. "
                    "You will begin life in the weak form of a Clear Hound. As you mature "
                    "you will take a number of evolutionary steps. At each such step, the "
                    "form you evolve into will be randomly determined. But each tier offers "
                    "more powerful choices than the last and who knows? You may even evolve "
                    "into an Aether Hound some day.\n \n"
                    "Hounds are monsters so cannot choose a normal class. They are not magical "
                    "so cannot cast spells. However, they are fantastic hunters so learn a "
                    "small number of abilities to aid in this endeavor. And of course, you "
                    "are probably aware that hounds love to breathe!\n \n"
                    "The body of the Hound is canine, so they cannot wield weapons. Instead, "
                    "they attack with their vicious bite and razor sharp claws. They are not "
                    "so powerful in melee as are dragons, but they are not that bad either. "
                    "Hounds can wear four rings on their front paws, a pair of boots on their "
                    "hind legs, an amulet around their neck and even a cloak and a suit of "
                    "body armor.";

        me->skills = bs;
        me->extra_skills = xs;
        me->infra = 5;
        me->exp = 150;
        me->life = 100;
        me->base_hp = 22;
        me->shop_adjust = 110;
        me->pseudo_class_id = CLASS_ROGUE;
        me->boss_r_idx = mon_race_parse("C.Carcharoth")->id;

        me->hooks.calc_innate_attacks = hound_calc_innate_attacks;
        me->hooks.calc_innate_bonuses = hound_calc_innate_bonuses;
        me->hooks.calc_bonuses = _calc_bonuses;
        me->hooks.get_powers = _get_powers;
        me->hooks.get_flags = _get_flags;
        me->hooks.gain_level = _gain_level;
        me->hooks.birth = _birth;

        me->flags = RACE_IS_MONSTER; /* | RACE_IS_ILLITERATE? No ... think Scooby-doo! */
    }

    me->subname = _mon_name(plr->current_r_idx);
    me->stats[A_STR] =  1 + plr->lev/12;
    me->stats[A_INT] = -3;
    me->stats[A_WIS] = -5;
    me->stats[A_DEX] =  2 + plr->lev/15;
    me->stats[A_CON] =  1 + plr->lev/15;
    me->stats[A_CHR] =  0 + plr->lev/25;
    me->equip_template = plr_equip_template();

    return me;
}
