#include "angband.h"

/****************************************************************
 * Timers
 ****************************************************************/
enum { _STEALTHY_SNIPE = T_CUSTOM,
       _NIMBLE_DODGE };
/* _STEALTHY_SNIPE */
static bool _stealthy_snipe_on(plr_tim_ptr timer)
{
    msg_print("You are a stealthy sniper.");
    plr->update |= PU_BONUS;
    return TRUE;
}
static void _stealthy_snipe_off(plr_tim_ptr timer)
{
    msg_print("You are no longer a stealthy sniper.");
    plr->update |= PU_BONUS;
}
static void _stealthy_snipe_bonus(plr_tim_ptr timer)
{
    plr->stealthy_snipe = TRUE;
}
static status_display_t _stealthy_snipe_display(plr_tim_ptr timer)
{
    return status_display_create("Snipe", "Ss", TERM_UMBER);
}
static plr_tim_info_ptr _stealthy_snipe(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(_STEALTHY_SNIPE, "Stealthy Sniper");
    info->desc = "Your archery no longer provokes monsters to retaliation.";
    info->on_f = _stealthy_snipe_on;
    info->off_f = _stealthy_snipe_off;
    info->calc_bonuses_f = _stealthy_snipe_bonus;
    info->status_display_f = _stealthy_snipe_display;
    return info;
}
/* _NIMBLE_DODGE */
static bool _nimble_dodge_on(plr_tim_ptr timer)
{
    msg_print("You begin to dodge enemy breaths.");
    plr->update |= PU_BONUS;
    return TRUE;
}
static void _nimble_dodge_off(plr_tim_ptr timer)
{
    msg_print("You no longer dodge enemy breaths.");
    plr->update |= PU_BONUS;
}
static void _nimble_dodge_bonus(plr_tim_ptr timer)
{
    plr->nimble_dodge = TRUE;
}
static status_display_t _nimble_dodge_display(plr_tim_ptr timer)
{
    return status_display_create("Dodge", "Dg", TERM_L_BLUE);
}
static plr_tim_info_ptr _nimble_dodge(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(_NIMBLE_DODGE, "Nimble Dodge");
    info->desc = "You dodge enemy breath attacks.";
    info->on_f = _nimble_dodge_on;
    info->off_f = _nimble_dodge_off;
    info->calc_bonuses_f = _nimble_dodge_bonus;
    info->status_display_f = _nimble_dodge_display;
    return info;
}

static void _register_timers(void)
{
    plr_tim_register(_stealthy_snipe());
    plr_tim_register(_nimble_dodge());
}
/****************************************************************
 * Spells
 ****************************************************************/
static void _cavern_creation_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Cavern Creation");
        break;
    case SPELL_DESC:
        var_set_string(res, "Stone to Mud all surrounding walls.");
        break;
    case SPELL_CAST: {
        int dir, ct = 0;
        for (dir = 0; dir < 8; dir++)
        {
            point_t p = point_step(plr->pos, ddd[dir]);

            if (!dun_pos_interior(cave, p)) continue;
            if (dun_tunnel(cave, p, ACTION_FORCE | ACTION_QUIET) == ACTION_SUCCESS)
                ct++;
        }
        if (ct) plr->update |= (PU_FLOW | PU_MON_FLOW | PU_BONUS);
        var_set_bool(res, TRUE);
        break; }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _dark_stalker_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Dark Stalker");
        break;
    case SPELL_DESC:
        var_set_string(res, "Temporarily grants enhanced stealth.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_duration(50, 50));
        break;
    case SPELL_CAST:
        plr_tim_add(T_STEALTH, 50 + randint1(50));
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _greater_mapping_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Greater Mapping");
        break;
    default:
        clairvoyance_spell(cmd, res);
        break;
    }
}

static void _greater_whirlwind_attack_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Greater Ambush");
        break;
    case SPELL_DESC:
        var_set_string(res, "Perform a massive ambush on nearby monsters.");
        break;
    case SPELL_CAST:
    {
        int i;
        mon_ptr mon;

/*       cba
        d218l
        e3@7k
        f456j
         ghi  */

        static point_t offsets[] = {
            { 0, -1}, {-1, -1}, {-1,  0}, {-1,  1},
            { 0,  1}, { 1,  1}, { 1,  0}, { 1, -1},
            { 1, -2}, { 0, -2}, {-1, -2}, {-2, -1},
            {-2,  0}, {-2,  1}, {-1,  2}, { 0,  2},
            { 1,  2}, { 2,  1}, { 2,  0}, { 2, -1},
            { 0,  0}, /* sentinel */
        };

        for (i = 0;; i++)
        {
            point_t v = offsets[i];
            point_t p;

            if (!v.x && !v.y) break;

            p = point_add(plr->pos, v);
            if (!dun_pos_interior(cave, p)) continue;
            if (!plr_project(p)) continue;

            mon = dun_mon_at(cave, p);
            if (!mon) continue;

            if (mon->ml || cell_project(dun_cell_at(cave, p)))
            {
                if (cave_pt_is_visible(p) && plr_can_see(p))
                {
                    char c = '*';
                    byte a = TERM_WHITE;

                    print_rel(c, a, p.y, p.x);
                    move_cursor_relative(p);
                    Term_fresh();
                    Term_xtra(TERM_XTRA_DELAY, delay_animation);
                    draw_pos(p);
                    Term_fresh();
                }
                else
                    Term_xtra(TERM_XTRA_DELAY, delay_animation);

                plr_attack_normal(p);
            }
        }
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _lookout_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Lookout");
        break;
    default:
        detect_monsters_spell(cmd, res);
        break;
    }
}

static void _mapping_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Mapping");
        break;
    default:
        magic_mapping_spell(cmd, res);
        break;
    }
}

static void _nimble_dodge_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Nimble Dodge");
        break;
    case SPELL_DESC:
        var_set_string(res, "For a short time, you will have a chance of dodging enemy breath attacks.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_duration(20, 20));
        break;
    case SPELL_CAST:
        plr_tim_add(_NIMBLE_DODGE, 20 + randint1(20));
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _reconnaissance_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Reconnaissance");
        break;
    case SPELL_DESC:
        var_set_string(res, "Quickly scout nearby terrain for enemies, traps and loot.");
        break;
    case SPELL_CAST:
        map_area(DETECT_RAD_MAP);
        detect_all(DETECT_RAD_DEFAULT);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _retreat_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Retreat");
        break;
    default:
        teleport_spell(cmd, res);
        break;
    }
}

static void _sniping_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Sniping");
        break;
    case SPELL_DESC:
        var_set_string(res, "Shoot a sleeping enemy sentry with great precision.");
        break;
    case SPELL_CAST:
        var_set_bool(res, FALSE);
        if (!equip_find_obj(TV_BOW, SV_ANY))
        {
            msg_print("You need a bow to use this talent.");
            break;
        }
        command_cmd = 'f'; /* Hack for inscriptions (e.g. '@f1') */
        var_set_bool(res, plr_shoot_special(PLR_SHOOT_AMBUSH, 0));
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _spying_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Spying");
        break;
    default:
        telepathy_spell(cmd, res);
        break;
    }
}

static void _stealthy_snipe_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Stealthy Snipe");
        break;
    case SPELL_DESC:
        var_set_string(res, "For a short while, your missile attacks will not anger distant monsters.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_duration(6, 6));
        break;
    case SPELL_CAST:
        plr_tim_add(_STEALTHY_SNIPE, 6 + randint1(6));
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _whirlwind_attack_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Ambush");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attack all adjacent monsters in a single ambush.");
        break;
    default:
        massacre_spell(cmd, res);
        break;
    }
}

/****************************************************************
 * Spell Table and Exports
 ****************************************************************/

static spell_info _spells[] =
{
    /*lvl cst fail spell */
    {  1,  1, 30, _lookout_spell},
    {  5,  2, 30, strafing_spell},
    {  9, 10, 35, _mapping_spell},
    { 13,  8, 40, stone_to_mud_spell},
    { 17, 12, 50, _spying_spell},
    { 21, 30, 50, _dark_stalker_spell},
    { 23, 15, 50, _reconnaissance_spell},
    { 25, 18, 50, _whirlwind_attack_spell},
    { 29, 25, 50, _retreat_spell},
    { 33, 25,  0, _sniping_spell},
    { 35, 40, 55, _nimble_dodge_spell},
    { 37, 24, 45, _cavern_creation_spell},
    { 41, 70, 50, _stealthy_snipe_spell},
    { 45, 60, 70, _greater_mapping_spell},
    { 49, 42, 65, _greater_whirlwind_attack_spell},
    { -1, -1, -1, NULL}
};

static int _get_spells(spell_info* spells, int max)
{
    int ct = 0;

    if (heavy_armor())
    {
        msg_print("Your talents are disrupted!");
        return 0;
    }
    ct = get_spells_aux(spells, max, _spells);
    if (ct == 0)
        msg_print("You have no powers yet! Why not go kill stuff?");

    return ct;
}

static bool _cave_is_open(point_t pos)
{
    dun_cell_ptr cell = dun_cell_at(cave, pos);
    if (cell_is_wall(cell)) return FALSE;
    if (door_is_closed(cell)) return FALSE;
    return TRUE;
}

static int _count_open_terrain(void)
{
    int dir;
    int count = 0;
    for (dir = 0; dir < 8; dir++)
    {
        point_t p = point_step(plr->pos, ddd[dir]);

        if (!dun_pos_interior(cave, p))
        {
            /* Count the edge of wilderness maps as open. XXX impossible case XXX
               Count the edge of dungeon maps as permanent walls. */
            if (cave->type->id == D_SURFACE) count++;
            continue;
        }
        if (_cave_is_open(p)) count++;
    }
    return count;
}

static int _prorate_effect(int amt)
{
    int base = (amt + 3) / 4;
    int xtra = amt - base;
    xtra = xtra * (plr->lev/2) / 25;

    return base + xtra;
}

static int _unfettered_body(int ct)
{
    int amt = (ct + 1) * (ct + 1) - 41;
    return _prorate_effect(amt);
}

static int _unfettered_mind(int ct)
{
    int amt = (ct + 1) * (ct + 1)/2 - 20;
    return _prorate_effect(amt);
}

static void _calc_bonuses(void)
{
    int ct = _count_open_terrain();
    bool disrupt = heavy_armor();

    plr->pass_tree = TRUE;

    /* Hack: Heavy Armor negates advantages of being in the open, and
       actually incurs penalties for being entrenched! */
    if (disrupt)
        ct = 0;

    plr->open_terrain_ct = ct; /* Nimble Dodge needs this information! */

    /* Unfettered Body */
    if (plr->lev >= 1)
    {
        int amt = _unfettered_body(ct);
        plr->to_a += amt;
        plr->dis_to_a += amt;
    }

    /* Unfettered Mind */
    if (plr->lev >= 1)
    {
        plr->skills.sav += _unfettered_mind(ct);
    }

    if (!disrupt && plr->lev >= 20)
        plr->ambush = 300 + plr->lev*4;

    if (!disrupt && plr->lev >= 50)
        plr->peerless_stealth = TRUE;
}
static void _get_flags(u32b flgs[OF_ARRAY_SIZE])
{
}

static void _character_dump(doc_ptr doc)
{
    int ct = _count_open_terrain();
    bool disrupt = heavy_armor();

    if (!disrupt && plr->lev >= 5)
    {
        spell_info spells[MAX_SPELLS];
        int        ct = _get_spells(spells, MAX_SPELLS);

        plr_display_spells(doc, spells, ct);
    }

    doc_printf(doc, "<topic:Abilities>================================== <color:keypress>A</color>bilities ==================================\n\n");

    /* Hack: Heavy Armor negates advantages of being in the open, and
       actually incurs penalties for being entrenched! */
    if (disrupt)
    {
        doc_printf(doc, "  * Your talents are disrupted by the weight of your armor.\n");
        ct = 0;
    }
    else
    {
        if (ct >= 6)
            doc_printf(doc, "  * You are out in the open (%d adjacent open squares).\n", ct);
        else if (ct >= 3)
            doc_printf(doc, "  * You are somewhat confined (%d adjacent open squares).\n", ct);
        else
            doc_printf(doc, "  * You are very confined (%d adjacent open squares).\n", ct);
    }

    /* Unfettered Body */
    if (plr->lev >= 1)
    {
        int amt = _unfettered_body(ct);
        if (amt > 0)
            doc_printf(doc, "  * You gain %+d to your AC being out in the open.\n", amt);
        else if (amt < 0)
            doc_printf(doc, "  * You lose %+d to your AC being so confined.\n", amt);
    }

    /* Unfettered Mind */
    if (plr->lev >= 1)
    {
        int amt = _unfettered_mind(ct);
        if (amt > 0)
            doc_printf(doc, "  * You gain %+d to your Saving Throws being out in the open.\n", amt);
        else if (amt < 0)
            doc_printf(doc, "  * You lose %+d to your Saving Throws being so confined.\n", amt);
    }

    if (!disrupt && plr->lev >= 20)
        doc_printf(doc, "  * You ambush sleeping monsters for extra damage.\n");

    if (!disrupt && plr->lev >= 50)
        doc_printf(doc, "  * You have Peerless Stealth and will never aggravate monsters.\n");

    doc_newline(doc);
}

static caster_info * _caster_info(void)
{
    static caster_info me = {0};
    static bool init = FALSE;
    if (!init)
    {
        me.magic_desc = "technique";
        me.which_stat = A_WIS;
        me.encumbrance.max_wgt = 350;
        me.encumbrance.weapon_pct = 50;
        me.encumbrance.enc_wgt = 800;
        me.options = CASTER_GAIN_SKILL;
        init = TRUE;
    }
    return &me;
}

static void _move_player(void)
{
    plr->update |= PU_BONUS;
}

static void _birth(void)
{
    plr_birth_obj_aux(TV_SWORD, SV_DAGGER, 1);
    plr_birth_obj_aux(TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR, 1);
    plr_birth_obj_aux(TV_BOW, SV_SHORT_BOW, 1);
    plr_birth_obj_aux(TV_ARROW, SV_ARROW, rand_range(20, 30));
}

plr_class_ptr scout_get_class(void)
{
    static plr_class_ptr me = NULL;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 30,  33,  34,   6,  50,  24,  50,  65 };
    skills_t xs = { 75,  55,  50,   0,   0,   0, 100, 125 };

        me = plr_class_alloc(CLASS_SCOUT);
        me->name = "Scout";
        me->desc = "The scout is the vanguard of any attack, and excels at stealth and observation "
                    "skills. The scout is not the best at one-on-one combat, but is unparalleled at "
                    "ambush techniques to destroy groups of weak sentries. The scout is lightly "
                    "armored, and heavy armors disrupt their abilities. Furthermore, the scout "
                    "can only effectively dodge in open areas, being confined severely hampers "
                    "the scout's defensive abilities.";

        me->stats[A_STR] =  1;
        me->stats[A_INT] = -1;
        me->stats[A_WIS] =  2;
        me->stats[A_DEX] =  3;
        me->stats[A_CON] =  0;
        me->stats[A_CHR] =  0;
        me->skills = bs;
        me->extra_skills = xs;
        me->life = 104;
        me->base_hp = 8;
        me->exp = 130;
        me->pets = 40;
        me->flags = CLASS_SENSE1_FAST | CLASS_SENSE1_STRONG |
                    CLASS_SENSE2_MED | CLASS_SENSE2_STRONG;

        me->hooks.birth = _birth;
        me->hooks.calc_bonuses = _calc_bonuses;
        me->hooks.get_flags = _get_flags;
        me->hooks.caster_info = _caster_info;
        me->hooks.get_spells = _get_spells;
        me->hooks.move_player = _move_player;
        me->hooks.character_dump = _character_dump;
        me->hooks.register_timers = _register_timers;
    }

    return me;
}
