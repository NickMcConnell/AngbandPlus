#include "angband.h"

static bool _on_mirror = FALSE;

static int _count;
static void _cell_count_mirror(point_t pos, dun_cell_ptr cell)
{
    if (floor_has_mirror(cell))
        _count++;
}
static int _mirrors_ct(void)
{
    _count = 0;
    dun_iter_interior(cave, _cell_count_mirror);
    return _count;
}

static int _mirrors_max(void)
{
    return 4 + plr->lev/10;
}

static bool _mirror_place(void)
{
    if (!dun_place_mirror(cave, plr->pos))
    {
        msg_print("The object resists the spell.");
        return FALSE;
    }
    return TRUE;
}

static void _banishing_mirror_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Banishing Mirror");
        break;
    case SPELL_DESC:
        if (_on_mirror)
            var_set_string(res, "Quickly teleport away a nearby monster.");
        else
            var_set_string(res, "Teleport away a nearby monster.");
        break;
    case SPELL_ENERGY:
        var_set_int(res, _on_mirror ? 50 : 100);
        break;
    default:
        beam_spell_aux(cmd, res, GF_TELEPORT, spell_dice(0, 0, plr->lev));
    }
}

static void _binding_field_spell(int cmd, var_ptr res)
{
    dice_t dice = spell_dam_dice(0, 0, 5 + 11*plr->lev);
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Binding Field");
        break;
    case SPELL_DESC:
        var_set_string(res, "Generates a magical triangle which damages all monsters in the area. The vertices of the triangle is you and two mirrors in sight.");
        break;
    case SPELL_INFO:
        var_set_string(res, dice_info_dam(dice));
        break;
    case SPELL_CAST:
        if (!plr_binding_field(dice_roll(dice)))
            msg_print("You were not able to choose suitable mirrors!");
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _break_mirrors_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Break Mirrors");
        break;
    case SPELL_DESC:
        var_set_string(res, "Destroys all mirrors on the current levels. Monsters close to a mirror take damage.");
        break;
    case SPELL_CAST:
        remove_all_mirrors(TRUE);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _drip_of_light_spell(int cmd, var_ptr res)
{
    int  dd = 3 + (plr->lev-1)/5;
    int  ds = 4;
    bool beam = _on_mirror && plr->lev >= 10;

    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Drip of Light");
        break;
    case SPELL_DESC:
        if (beam)
            var_set_string(res, "Fires a beam of light");
        else
            var_set_string(res, "Fires a bolt of light");
        break;
    default:
        if (beam)
            beam_spell(cmd, res, GF_LIGHT, dd, ds);
        else
            bolt_spell(cmd, res, GF_LIGHT, dd, ds);
    }
}

static void _illusion_light_spell(int cmd, var_ptr res)
{
    int mult = _on_mirror ? 4 : 3;
    int power = spell_power(plr->lev * mult);

    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Illusion Light");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attempts to slow, stun, confuse, scare, freeze all monsters in sight. This is more powerful if you are standing on a mirror.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_power(power));
        break;
    case SPELL_CAST:
        plr_project_los(GF_SLOW, power);
        plr_project_los(GF_STUN, 5 + plr->lev/5);
        plr_project_los(GF_OLD_CONF, power);
        plr_project_los(GF_FEAR, power);
        plr_project_los(GF_STASIS, power/3);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _make_mirror_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Make Mirror");
        break;
    case SPELL_DESC:
        var_set_string(res, "Makes a mirror under you.");
        break;
    case SPELL_CAST:
        if (_mirrors_ct() < _mirrors_max())
            _mirror_place();
        else
            msg_print("There are too many mirrors to control!");
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _mirror_clashing_spell(int cmd, var_ptr res)
{
    int dd = 8 + (plr->lev - 5)/4;
    int ds = 8;
    int rad = 0;
    if (plr->lev > 20)
        rad = 1 + (plr->lev - 20)/8;
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Mirror Clashing");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a ball of shards.");
        break;
    default:
        ball_spell_aux(cmd, res, rad, GF_SHARDS, spell_dam_dice(dd, ds, 0)); 
    }
}

static void _mirror_concentration_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Mirror Concentration");
        break;
    case SPELL_DESC:
        var_set_string(res, "");
        break;
    case SPELL_CAST:
        var_set_bool(res, FALSE);
        if (plr_pet_count())
        {
            msg_print("You need to focus on your pets.");
            return;
        }
        if (_on_mirror)
        {
            msg_print("You feel your head clear a little.");

            plr->csp += (5 + plr->lev * plr->lev / 100);
            if (plr->csp >= plr->msp)
            {
                plr->csp = plr->msp;
                plr->csp_frac = 0;
            }

            plr->redraw |= (PR_MANA);
            var_set_bool(res, TRUE);
        }
        else
        {
            msg_print("You need to on a mirror to use this spell!");
        }
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _mirror_of_light_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Mirror of Light");
        break;
    case SPELL_DESC:
        var_set_string(res, "Lights up nearby area and the inside of a room permanently.");
        break;
    case SPELL_CAST:
        lite_area(damroll(2, plr->lev/2), plr->lev/10 + 1);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _mirror_of_recall_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Mirror of Recall");
        break;
    case SPELL_DESC:
        var_set_string(res, "Recalls player from dungeon to town, or from town to the deepest level of dungeon.");
        break;
    case SPELL_CAST:
        var_set_bool(res, dun_mgr_recall_plr());
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _mirror_of_ruffnor_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Mirror of Ruffnor");
        break;
    case SPELL_DESC:
        var_set_string(res, "Generates barrier which completely protect you from almost all damages. Takes a few your turns when the barrier breaks or duration time is exceeded.");
        break;
    case SPELL_CAST:
        plr_tim_add(T_INVULN, spell_power(500 + _1d(1000)));
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}


static void _mirror_of_seeing_spell(int cmd, var_ptr res)
{
    int lvl = plr->lev;

    if (_on_mirror)
        lvl += 4;

    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Mirror of Seeing");
        break;
    case SPELL_DESC:
        if (lvl >= 39)
            var_set_string(res, "Detects monsters in your vicinity. Grants temporary ESP. Maps nearby area.");
        else if (lvl >= 29)
            var_set_string(res, "Detects monsters in your vicinity. Grants temporary ESP");
        else if (lvl >= 19)
            var_set_string(res, "Detects monsters in your vicinity.");
        else if (lvl >= 5)
            var_set_string(res, "Detects visible monsters in your vicinity.");
        else
            var_set_string(res, "Does nothing since you are too weak. Try standing on a mirror.");
        break;
    case SPELL_CAST:
    {
        if (lvl < 5)
            msg_print("You need a mirror to concentrate!");

        if (lvl >= 5)
            detect_monsters_normal(DETECT_RAD_DEFAULT);
        if (lvl >= 19)
            detect_monsters_invis(DETECT_RAD_DEFAULT);
        if (lvl >= 29)
            plr_tim_add(T_TELEPATHY, lvl + randint1(lvl));
        if (lvl >= 39)
            map_area(DETECT_RAD_MAP);

        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _mirror_of_wandering_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Mirror of Wandering");
        break;
    case SPELL_DESC:
        if (_on_mirror)
            var_set_string(res, "Quickly teleport a long distance.");
        else
            var_set_string(res, "Teleport a long distance.");
        break;
    case SPELL_CAST:
        teleport_player(plr->lev*5, 0);
        var_set_bool(res, TRUE);
        break;
    case SPELL_ENERGY:
        if (_on_mirror)
        {
            var_set_int(res, 50);
            break;
        }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _mirror_shifting_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Mirror Shifting");
        break;
    case SPELL_DESC:
        var_set_string(res, "Recreates current dungeon level. Can only be used on a mirror.");
        break;
    case SPELL_CAST:
        if (!_on_mirror)
            msg_print("You cannot find the World of the Mirror!");
        else
            alter_reality();
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _mirror_sleeping_fn(point_t pos, dun_cell_ptr cell)
{
    if (!floor_has_mirror(cell)) return;
    dun_burst(cave, who_create_mirror(pos), 2, pos, GF_SLEEP, plr->lev);
}
static void _mirror_sleeping_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Mirror Sleeping");
        break;
    case SPELL_DESC:
        var_set_string(res, "Generate balls which send monsters to sleep on all mirrors in the whole level.");
        break;
    case SPELL_CAST:
        dun_iter_interior(cave, _mirror_sleeping_fn);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _mirror_tunnel_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Mirror Tunnel");
        break;
    case SPELL_DESC:
        if (_on_mirror)
            var_set_string(res, "Quickly teleport to a given location.");
        else
            var_set_string(res, "Teleport to a given location.");
        break;
    case SPELL_CAST:
    {
        point_t pos;
        int rng = 10 + plr->lev/2;
        var_set_bool(res, FALSE);

        msg_print("You go through the mirror world ...");
        pos = target_pos(rng);
        if (!dun_pos_interior(cave, pos)) return;
        if (!dimension_door_aux(pos, rng))
            msg_print("You fail to pass into the mirror plane correctly!");

        var_set_bool(res, TRUE);
        break;
    }
    case SPELL_ENERGY:
        if (_on_mirror)
        {
            var_set_int(res, 50);
            break;
        }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _multi_shadow_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Multishadow");
        break;
    case SPELL_DESC:
        var_set_string(res, "Completely protects you from any attacks at one in two chance.");
        break;
    case SPELL_CAST:
        plr_tim_add(T_MULTISHADOW, 6 + randint1(6));
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _robe_of_dust_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Robe of Dust");
        break;
    case SPELL_DESC:
        var_set_string(res, "Gives aura of shards of mirror for a while, damaging any monster that attacks you in melee.");
        break;
    case SPELL_CAST:
        plr_tim_add(T_AURA_SHARDS, 20 + randint1(20));
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _seal_of_mirror_fn(point_t pos, dun_cell_ptr cell)
{
    mon_ptr mon;
    if (!floor_has_mirror(cell)) return;
    mon = dun_mon_at(cave, pos);
    if (mon)
    {
        int dam = spell_power(plr->lev*4 + 100);
        gf_affect_m(who_create_plr(), mon, GF_GENOCIDE, dam, GF_AFFECT_SPELL);
        if (mon_is_deleted(mon))
            dun_remove_mirror(cave, pos);
    }
}
static void _seal_of_mirror_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Seal of Mirror");
        break;
    case SPELL_DESC:
        var_set_string(res, "Eliminates a monster on a mirror from current dungeon level.");
        break;
    case SPELL_CAST:
        dun_iter_interior(cave, _seal_of_mirror_fn);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _seeker_ray_spell(int cmd, var_ptr res)
{
    dice_t dice = {0};
    dice.dd = 11 + (plr->lev - 5)/4;
    dice.ds = 8;
    dice.scale = spell_power(1000);

    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Seeker Ray");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a beam of mana. If the beam hit a mirror, it breaks that mirror and reflects toward another mirror.");
        break;
    case SPELL_INFO:
        var_set_string(res, dice_info_dam(dice));
        break;
    case SPELL_CAST:
    {
        point_t pos = get_fire_pos();
        var_set_bool(res, FALSE);
        if (!dun_pos_interior(cave, pos)) return;
        plr_seeker_ray(pos, dice_roll(dice));
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _shield_of_water_spell(int cmd, var_ptr res)
{
    int lvl = plr->lev; /* Boost if _on_mirror? */

    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Shield of Water");
        break;
    case SPELL_DESC:
        if (lvl >= 40)
            var_set_string(res, "Gives a bonus to AC, reflection and magic resistance.");
        else if (lvl >= 32)
            var_set_string(res, "Gives a bonus to AC and reflection.");
        else
            var_set_string(res, "Gives a bonus to AC.");
        break;
    case SPELL_CAST:
        plr_tim_add(T_STONE_SKIN, 20 + randint1(20));
        if (lvl >= 32) 
            plr_tim_add(T_REFLECT, 20 + randint1(20));
        if (lvl >= 40) 
            plr_tim_add(T_RES_MAGIC, 20 + randint1(20));
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _super_ray_spell(int cmd, var_ptr res)
{
    dice_t dice = {0};
    dice.dd = 1;
    dice.ds = plr->lev * 2;
    dice.base = 150 + plr->to_d_spell;
    dice.scale = spell_power(1000);

    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Super Ray");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a powerful beam of mana. If the beam hit a mirror, it breaks that mirror and fires 8 beams of mana to 8 different directions from that point.");
        break;
    case SPELL_INFO:
        var_set_string(res, dice_info_dam(dice));
        break;
    case SPELL_CAST:
    {
        point_t pos = get_fire_pos();
        var_set_bool(res, FALSE);
        if (!dun_pos_interior(cave, pos)) return;
        plr_super_ray(pos, dice_roll(dice));
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _warped_mirror_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Warped Mirror");
        break;
    case SPELL_DESC:
        if (_on_mirror)
            var_set_string(res, "Quickly teleport a short distance.");
        else
            var_set_string(res, "Teleport a short distance.");
        break;
    case SPELL_CAST:
        teleport_player(10, 0);
        var_set_bool(res, TRUE);
        break;
    case SPELL_ENERGY:
        if (_on_mirror)
        {
            var_set_int(res, 50);
            break;
        }
    default:
        default_spell(cmd, res);
        break;
    }
}

/****************************************************************
 * Spell Table and Exports
 ****************************************************************/
static spell_info _spells[] = 
{
    /*lvl cst fail spell */
    { 1,   1,  15, _mirror_of_seeing_spell},
    { 1,   2,  40, _make_mirror_spell},
    { 2,   2,  20, _drip_of_light_spell},
    { 3,   2,  20, _warped_mirror_spell},
    { 5,   3,  35, _mirror_of_light_spell},
    { 6,   5,  35, _mirror_of_wandering_spell},
    {10,   5,  30, _robe_of_dust_spell},
    {12,  12,  30, _banishing_mirror_spell},
    {15,  15,  30, _mirror_clashing_spell},
    {19,  13,  30, _mirror_sleeping_spell},
    {23,  18,  50, _seeker_ray_spell},
    {25,  20,  40, _seal_of_mirror_spell},
    {27,  30,  60, _shield_of_water_spell},
    {29,  30,  60, _super_ray_spell},
    {31,  35,  60, _illusion_light_spell},
    {33,  50,  80, _mirror_shifting_spell},
    {36,  30,  80, _mirror_tunnel_spell},
    {38,  40,  70, _mirror_of_recall_spell},
    {40,  50,  55, _multi_shadow_spell},
    {43,  55,  70, _binding_field_spell},
    {46,  70,  75, _mirror_of_ruffnor_spell},
    { -1, -1,  -1, NULL}
};

static power_info _powers[] =
{
    { A_NONE, { 1, 0,  0, _break_mirrors_spell}}, 
    { A_INT,  {30, 0, 50, _mirror_concentration_spell}}, 
    { -1, {-1, -1, -1, NULL}}
};
static int _get_spells(spell_info* spells, int max)
{
    dun_cell_ptr cell = dun_cell_at(cave, plr->pos);
    _on_mirror = floor_has_mirror(cell);
    return get_spells_aux(spells, max, _spells);
}

static int _get_powers(spell_info* spells, int max)
{    
    dun_cell_ptr cell = dun_cell_at(cave, plr->pos);
    _on_mirror = floor_has_mirror(cell);
    return get_powers_aux(spells, max, _powers);
}

static void _calc_bonuses(void)
{
    if (equip_find_art("*.Yata-no-Kagami"))
    {
        plr->dec_mana++;
        plr->easy_spell++;
    }
    if (equip_find_art(").Gil-Galad"))
        plr->dec_mana++;

    if (plr->lev >= 40) 
        plr->reflect = TRUE;
}

static void _get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    if(plr->lev >= 40)
        add_flag(flgs, OF_REFLECT);
}

static void _on_fail(const spell_info *spell)
{
    if (randint1(100) < (spell->fail / 2))
    {
        int b = randint1(100);

        if (b <= 50)
        {
        }
        else if (b <= 80)
        {
            msg_print("Weird visions seem to dance before your eyes...");
            teleport_player(10, TELEPORT_PASSIVE);
        }
        else if (b <= 95 && !mut_present(MUT_WEIRD_MIND))
        {
            msg_print("Your brain is addled!");
            plr_tim_add(T_HALLUCINATE, 5 + randint1(10));
        }
        else
        {
            msg_print("Your mind unleashes its power in an uncontrollable storm!");
            dun_burst(cave, who_create_unctrl_power(), 2 + plr->lev/10, plr->pos, GF_MANA, 2*plr->lev);
            plr->csp = MAX(0, plr->csp - plr->lev * MAX(1, plr->lev / 10));
        }
    }
}

static caster_info * _caster_info(void)
{
    static caster_info me = {0};
    static bool init = FALSE;
    if (!init)
    {
        me.magic_desc = "mirror magic";
        me.which_stat = A_INT;
        me.encumbrance.max_wgt = 400;
        me.encumbrance.weapon_pct = 50;
        me.encumbrance.enc_wgt = 800;
        me.on_fail = _on_fail;
        me.options = CASTER_GAIN_SKILL;
        init = TRUE;
    }
    return &me;
}

static void _birth(void)
{
    plr_birth_obj_aux(TV_SWORD, SV_DAGGER, 1);
    plr_birth_obj_aux(TV_SOFT_ARMOR, SV_ROBE, 1);
    plr_birth_obj_aux(TV_POTION, SV_POTION_SPEED, rand_range(2, 5));
}

plr_class_ptr mirror_master_get_class(void)
{
    static plr_class_ptr me = NULL;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 30,  33,  40,   3,  14,  16,  34,  30 };
    skills_t xs = { 50,  55,  60,   0,   0,   0,  30,  50 };

        me = plr_class_alloc(CLASS_MIRROR_MASTER);
        me->name = "Mirror-Master";
        me->desc = "Mirror-Masters are spell casters; like other mages, they must live "
                    "by their wits. They can create magical mirrors, and employ them "
                    "in the casting of Mirror-Magic spells. Intelligence determines a "
                    "Mirror-Master's spell casting ability.\n \n"
                    "Mirror-Masters gain more spells and each spell becomes more "
                    "powerful as they gain levels. They can use their spells even when "
                    "blinded. For Mirror-Masters, the arrangement of mirrors is very "
                    "important: Some attack spells reflect from mirrors, and some other "
                    "spells are effective only against monsters standing on a mirror. "
                    "A Mirror-Master standing on a mirror has greater ability and, for "
                    "example, can perform quick teleports. The maximum number of "
                    "magical Mirrors which can be controlled simultaneously depends on "
                    "the level, and breaking unnecessary mirrors is important work for "
                    "them. They have two class powers - 'Break Mirrors', which breaks "
                    "all mirrors in current dungeon level and 'Mirror Concentration', "
                    "which allows them to rapidly regenerate their mana on a mirror.";

        me->stats[A_STR] = -2;
        me->stats[A_INT] =  3;
        me->stats[A_WIS] =  1;
        me->stats[A_DEX] = -1;
        me->stats[A_CON] = -2;
        me->stats[A_CHR] = -2;
        me->skills = bs;
        me->extra_skills = xs;
        me->life = 100;
        me->base_hp = 4;
        me->exp = 130;
        me->pets = 30;
        me->flags = CLASS_SENSE1_MED | CLASS_SENSE1_WEAK |
                    CLASS_SENSE2_FAST | CLASS_SENSE2_STRONG;

        me->hooks.birth = _birth;
        me->hooks.calc_bonuses = _calc_bonuses;
        me->hooks.get_flags = _get_flags;
        me->hooks.caster_info = _caster_info;
        me->hooks.get_spells = _get_spells;
        me->hooks.get_powers = _get_powers;
    }

    return me;
}

static void _remove_mirror_grid(point_t pos, dun_cell_ptr cell)
{
    if (!floor_has_mirror(cell)) return;
    dun_remove_mirror(cave, pos);
}
static void _explode_mirror_grid(point_t pos, dun_cell_ptr cell)
{
    if (!floor_has_mirror(cell)) return;
    dun_remove_mirror(cave, pos);
    dun_burst(cave, who_create_mirror(pos), 2, pos, GF_SHARDS, 5 + plr->lev/2);
}
void remove_all_mirrors(bool explode)
{
    if (explode) dun_iter_interior(cave, _explode_mirror_grid);
    else dun_iter_interior(cave, _remove_mirror_grid);
}
