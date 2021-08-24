#include "angband.h"

static cptr _desc =
    "Leprechauns are small, mischevous creatures, always scheming for gold. "
    "They are weak, but quick and stealthy. For combat, they prefer not to use "
    "weapons at all. Rather, by keeping at least one hand free, they may pilfer "
    "objects or gold from their foes and flee to safety, all in a single action. However, "
    "they are capable of fighting with normal weapons should the need arise, but "
    "one seldom hears of leprechauns inspiring fear in combat!\n \n"
    "Leprechauns value riches above all else and even begin the game with their "
    "legendary pot of gold. Leprechauns also have the luck of the Irish, and good "
    "drops are more common for them. For magic, the leprechaun has a few talents "
    "and even a bit of cursing offense (Malicious and Death Leprechauns are vile, "
    "evil creatures, after all). They are masters of teleportation and detection. And while "
    "one never fears their combat (though one despises their thievery, of course), they "
    "are great masters of bow and device alike. At high levels, they move with incredible "
    "speed and seem to be in multiple places at once. Dexterity determines the leprechaun's "
    "skill with their magic.\n \n"
    "The leprechaun uses gold for spell casting the way other players use mana. Gold is "
    "also used to pay for recharging devices or to power mana branded weapons. In addition, "
    "increasing the amount of gold on hand increases the power of the leprechaun, granting "
    "extra blows per round, extra shots per round, increased AC, life rating and even "
    "increasing their power with magical devices. To succeed in life, greed is obviously "
    "the primary virtue of the leprechaun!";

static void _birth(void)
{
    plr_mon_race_set("h.cheerful");
    skills_innate_init("Greedy Hands", WEAPON_EXP_BEGINNER, WEAPON_EXP_MASTER);

    msg_print("You feel the luck of the Irish!");
    mut_gain(MUT_GOOD_LUCK);
    mut_lock(MUT_GOOD_LUCK);

    plr->au = 50000;

    plr_birth_food();
    plr_birth_light();
}

static int _get_toggle(void)
{
    return plr->magic_num1[0];
}

static int _set_toggle(s32b toggle)
{
    int result = plr->magic_num1[0];

    if (toggle == result) return result;

    plr->magic_num1[0] = toggle;

    plr->redraw |= PR_STATUS;
    plr->update |= PU_BONUS;
    handle_stuff();

    return result;
}

/**********************************************************************
 * Innate Attacks
 **********************************************************************/
static void _calc_innate_bonuses(mon_blow_ptr blow)
{
    if (blow->method == RBM_TOUCH)
    {
        plr->innate_attack_info.blows_calc.wgt = 100;
        plr->innate_attack_info.blows_calc.mul = 25;
        plr->innate_attack_info.blows_calc.max = 300;
        plr_calc_blows_innate(blow);
    }
}
static void _calc_innate_attacks(void)
{
    int l = plr->lev;
    mon_blow_ptr blow = mon_blow_alloc(RBM_TOUCH);

    blow->name = "Greedy Hands";
    blow->msg = "You pilfer.";
    blow->power = l;
    mon_blow_push_effect(blow, RBE_EAT_ITEM, dice_create(1, 3 + l/15, 0));
    _calc_innate_bonuses(blow);
    vec_add(plr->innate_blows, blow);
}

/****************************************************************
 * Spells
 ****************************************************************/
static void _toggle_spell(int which, int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_CAST:
        var_set_bool(res, FALSE);
        if (_get_toggle() == which)
            _set_toggle(TOGGLE_NONE);
        else
            _set_toggle(which);
        var_set_bool(res, TRUE);
        break;
    case SPELL_ENERGY:
        if (_get_toggle() != which)
            var_set_int(res, 0);    /* no charge for dismissing a technique */
        else
            var_set_int(res, 100);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void blink_toggle_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Blinking Death");
        break;
    case SPELL_DESC:
        var_set_string(res, "When using this technique, you will execute a short range, line of sight teleport after every action.");
        break;
    default:
        _toggle_spell(LEPRECHAUN_TOGGLE_BLINK, cmd, res);
        break;
    }
}

void _hoarding_toggle_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Hoarding");
        break;
    case SPELL_DESC:
        var_set_string(res, "When using this technique, any items you 'destroy' will be automatically converted into gold.");
        break;
    default:
        _toggle_spell(LEPRECHAUN_TOGGLE_HOARDING, cmd, res);
        break;
    }
}

/**********************************************************************
 * Leprechaun Spells and Abilities
 **********************************************************************/
void _fanaticism_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Fanaticism");
        break;
    case SPELL_DESC:
        var_set_string(res, "Summon many devoted leprechaun servants at chosen foe.");
        break;
    case SPELL_CAST:
    {
        int i;
        point_t pos;

        var_set_bool(res, FALSE);
        if (!target_set(TARGET_KILL)) return;
        pos = who_pos(plr->target);

        for (i = 0; i < 8; i++)
            summon_named_creature(who_create_plr(), pos, mon_race_parse("h.fanatic"), PM_FORCE_PET);

        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static spell_info _spells[] =
{ /* Lvl    Au  Fail */
    {  1,    5,   20, phase_door_spell},
    {  1,   10,   20, detect_treasure_spell},
    { 10,   25,   30, detection_spell},
    { 15,   25,   30, cause_wounds_I_spell},
    { 15,   20,   30, teleport_spell},
    { 15,  100,   40, teleport_to_spell},
    { 20,  250,   40, telekinesis_spell},
    { 30,  200,   50, cause_wounds_III_spell},
    { 35,  500,   60, animate_dead_spell},
    { 37, 1000,   60, recharging_spell},
    { 40,    0,    0, blink_toggle_spell},
    { 42,    0,    0, _hoarding_toggle_spell},
    { 45, 1500,   60, _fanaticism_spell},
    { -1,   -1,   -1, NULL}
};

static int _get_spells(spell_info* spells, int max)
{
    return get_spells_aux(spells, max, _spells);
}

static caster_info * _caster_info(void)
{
    static caster_info me = {0};
    static bool init = FALSE;
    if (!init)
    {
        me.magic_desc = "greedy power";
        me.which_stat = A_DEX;
        me.options = CASTER_USE_AU;
        init = TRUE;
    }
    return &me;
}

static void _calc_bonuses(void)
{
    int ac = MIN(plr->au / 250000, 25);

    plr->to_a += ac;
    plr->dis_to_a += ac;

    if (plr->au >= 10 * 1000 * 1000)
    {
        plr->device_power += 2;
    }
    else if (plr->au >= 5 * 1000 * 1000)
    {
        plr->device_power += 1;
    }

    plr->skills.thb += MIN(plr->au / 100000, 100);

    if (plr_mon_race_is_("h.cheerful"))
    {
        plr->align += 200;
        plr->pspeed += 5;
    }
    else if (plr_mon_race_is_("h.malicious"))
    {
        plr->align -= 200;
        plr->pspeed += 7;
        plr->levitation = TRUE;
        res_add_vuln(GF_LIGHT);
    }
    else if (plr_mon_race_is_("h.death"))
    {
        plr->align -= 200;
        plr->pspeed += 10;
        plr->levitation = TRUE;
        res_add_vuln(GF_LIGHT);
        res_add(GF_NETHER);
    }
}

static void _get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    if (plr->au >= 5 * 1000 * 1000)
        add_flag(flgs, OF_DEVICE_POWER);

    if (plr_mon_race_is_("h.cheerful"))
    {
        add_flag(flgs, OF_SPEED);
    }
    else if (plr_mon_race_is_("h.malicious"))
    {
        add_flag(flgs, OF_SPEED);
        add_flag(flgs, OF_LEVITATION);
        add_flag(flgs, OF_VULN_(GF_LIGHT));
    }
    else if (plr_mon_race_is_("h.death"))
    {
        add_flag(flgs, OF_SPEED);
        add_flag(flgs, OF_LEVITATION);
        add_flag(flgs, OF_RES_(GF_NETHER));
        add_flag(flgs, OF_VULN_(GF_LIGHT));
    }
}

static void _calc_weapon_bonuses(obj_ptr obj, plr_attack_info_ptr info)
{
    info->xtra_blow += MIN(plr->au / 100000, 100);
}

static void _player_action(void)
{
    if (_get_toggle() == LEPRECHAUN_TOGGLE_BLINK)
        teleport_player(10, TELEPORT_LINE_OF_SIGHT);
}

/**********************************************************************
 * Leprechaun Evolution
 **********************************************************************/
static void _gain_level(int new_level)
{
    if (plr_mon_race_is_("h.cheerful") && new_level >= 15)
        plr_mon_race_evolve("h.malicious");
    if (plr_mon_race_is_("h.malicious") && new_level >= 30)
        plr_mon_race_evolve("h.death");
}

/**********************************************************************
 * Public Interface
 **********************************************************************/
int leprechaun_get_toggle(void)
{
    int result = TOGGLE_NONE;
    if (plr->prace == RACE_MON_LEPRECHAUN)
        result = _get_toggle();
    return result;
}

bool leprechaun_steal(int m_idx)
{
    bool result = FALSE;
    monster_type *m_ptr = dun_mon(cave, m_idx);

    if ( !mon_save_p(m_ptr, A_DEX)
      || (mon_tim_find(m_ptr, MT_SLEEP) && !mon_save_p(m_ptr, A_DEX)))
    {
        obj_ptr loot = mon_pick_pocket(m_ptr);

        if (!loot)
        {
            msg_print("There is nothing to steal!");
        }
        else
        {
            char o_name[MAX_NLEN];

            result = TRUE;
            object_desc(o_name, loot, 0);
            if (mon_save_p(m_ptr, A_DEX))
            {
                msg_format("Oops! You drop %s.", o_name);
                drop_near(loot, plr->pos, -1);
            }
            else if (loot->tval == TV_GOLD)
            {
                msg_format("You steal %d gold pieces worth of %s.", (int)loot->pval, o_name);
                sound(SOUND_SELL);
                plr->au += loot->pval;
                stats_on_gold_find(loot->pval);
                plr->redraw |= (PR_GOLD);
                if (prace_is_(RACE_MON_LEPRECHAUN)) /* possessors and mimics require this check */
                    plr->update |= (PU_BONUS | PU_HP | PU_MANA);
            }
            else
            {
                pack_carry(loot);
                msg_format("You steal %s.", o_name);
            }
            obj_free(loot);
        }
    }
    return result;
}

bool _destroy_object(object_type *o_ptr)
{
    if (_get_toggle() == LEPRECHAUN_TOGGLE_HOARDING)
    {
        int amt = obj_value_real(o_ptr) * o_ptr->number / 3;

        if (amt > 0)
        {
            char o_name[MAX_NLEN];

            object_desc(o_name, o_ptr, OD_COLOR_CODED);
            msg_format("You turn %s to %d coins worth of gold.", o_name, amt);

            plr->au += amt;
            stats_on_gold_selling(amt); /* ? */

            plr->redraw |= (PR_GOLD);
            plr->update |= (PU_BONUS | PU_HP | PU_MANA);

            return TRUE;
        }
    }
    return FALSE;
}

plr_race_ptr mon_leprechaun_get_race(void)
{
    static plr_race_ptr me = NULL;
    static cptr   titles[3] =  {"Cheerful Leprechaun", "Malicious Leprechaun", "Death Leprechaun"};
    int           rank = 0;

    if (plr->lev >= 15) rank++;
    if (plr->lev >= 30) rank++;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 30,  45,  38,  10,  24,  16,  48,  50 };
    skills_t xs = { 60,  90,  55,   5,   0,   0,  65,  50 };

        me = plr_race_alloc(RACE_MON_LEPRECHAUN);
        me->skills = bs;
        me->extra_skills = xs;

        me->name = "Leprechaun";
        me->desc = _desc;

        me->infra = 5;
        me->exp = 150;
        me->base_hp = 15;
        me->shop_adjust = 85;

        me->hooks.get_spells = _get_spells;
        me->hooks.caster_info = _caster_info;
        me->hooks.calc_innate_attacks = _calc_innate_attacks;
        me->hooks.calc_innate_bonuses = _calc_innate_bonuses;
        me->hooks.calc_bonuses = _calc_bonuses;
        me->hooks.calc_weapon_bonuses = _calc_weapon_bonuses;
        me->hooks.get_flags = _get_flags;
        me->hooks.gain_level = _gain_level;
        me->hooks.birth = _birth;
        me->hooks.player_action = _player_action;
        me->hooks.destroy_object = _destroy_object;

        me->pseudo_class_id = CLASS_ROGUE;
        me->flags = RACE_IS_MONSTER;
    }

    me->life = 80;
    if (!spoiler_hack)
        me->life += MIN(plr->au / 500000, 20);

    if (!birth_hack && !spoiler_hack)
        me->subname = titles[rank];
    me->stats[A_STR] = -2 - 2*rank;
    me->stats[A_INT] = 1;
    me->stats[A_WIS] = 1;
    me->stats[A_DEX] = 3 + 2*rank;
    me->stats[A_CON] = -2;
    me->stats[A_CHR] = -2;

    return me;
}
