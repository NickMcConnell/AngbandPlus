#include "angband.h"

static void _birth(void)
{
    plr_birth_obj_aux(TV_SWORD, SV_DAGGER, 1);
    plr_birth_obj_aux(TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR, 1);
    plr_birth_obj_aux(TV_BOW, SV_LIGHT_XBOW, 1);
    plr_birth_obj_aux(TV_BOLT, SV_BOLT, rand_range(20, 30));
}

/************************************************************************
 * Bonuses
 ***********************************************************************/
static void _calc_shooter_bonuses(object_type *o_ptr, plr_shoot_info_ptr info_ptr)
{
    if (info_ptr->tval_ammo == TV_BOLT)
    {
        info_ptr->to_h += 10 + plr->lev/5;
        info_ptr->dis_to_h += 10 + plr->lev/5;
    }
}

/************************************************************************
 * Custom Shooting
 ***********************************************************************/
enum {
    _LITE = PLR_SHOOT_CUSTOM,
    _AWAY,
    _FIRE,
    _KILL_WALL,
    _COLD,
    _KILL_TRAP,
    _ELEC,
    _KNOCKBACK,
    _DOUBLE,
    _EVILNESS,
    _HOLINESS,
    _FINAL,
    _NEEDLE,
};
static bool _begin(plr_shoot_ptr context)
{
    context->mult = context->mult * (10 + plr->concent) / 10; /* XXX mult only: new */
    return TRUE;
}
static bool _begin_bow(plr_shoot_ptr context)
{
    context->skill = context->skill * 10 / MAX(1, 10 - plr->concent);
    /* XXX consider improving crits as well ... that seems OP though
     * note that skill will improve the crit multiplier:
     * C   Crit             Mult     Eff Mult
     * 0x: 1.10x            6.00x       6.60x
     * 1x: 1.11x (10.4%)    6.60x       7.33x
     * 2x: 1.13x (11.7%)    7.20x       8.14x
     * 3x: 1.15x (13.4%)    7.80x       8.97x
     * 4x: 1.18x (15.6%)    8.40x       9.91x
     * 5x: 1.21x (18.7%)    9.00x      10.89x
     * 6x: 1.26x (23.4%)    9.60x      12.10x
     * 7x: 1.36x (31.1%)   10.20x      13.87x
     */
    if (!(context->flags & PSC_DISPLAY) && context->mode == _DOUBLE)
        plr->concent = (plr->concent + 1) / 2;
    return TRUE;
}
static int _get_shots(plr_shoot_ptr context)
{
    if (context->mode == _DOUBLE) return 2;
    return 1;
}
static int _prepath(plr_shoot_ptr context, point_t pos)
{
    if (context->mode == _KILL_WALL && !dun_mon_at(cave, pos))
    {
        if (dun_tunnel(cave, pos, ACTION_FORCE|ACTION_QUIET) == ACTION_SUCCESS)
        {
            dun_cell_ptr cell = dun_cell_at(cave, pos);
            if (cell->flags & CELL_MAP)
                msg_print("Wall rocks were shattered.");
            cell->flags &= ~CELL_MAP;
            plr->update |= (PU_VIEW | PU_LIGHT | PU_FLOW | PU_MON_FLOW | PU_MON_LIGHT);
            context->hit_body = TRUE;
            context->action = AMMO_BREAK;
            return PATH_STOP;
        }
    }
    return PATH_OK;
}
static int _path(plr_shoot_ptr context, point_t pos)
{
    if (context->mode == _LITE || context->mode == _HOLINESS)
    {
        dun_grid_at(cave, pos)->flags |= CELL_LIT;
        plr->update |= PU_LIGHT;
    }
    if (context->mode == _KILL_TRAP)
    {
        gf_affect_o(who_create_plr(), pos, GF_KILL_TRAP, 0, 0); /* chests */
        gf_affect_f(who_create_plr(), pos, GF_KILL_TRAP, 0, 0);
    }
    if (context->mode == _EVILNESS)
    {
        dun_grid_at(cave, pos)->flags &= ~(CELL_LIT | CELL_MAP);
        plr->update |= PU_LIGHT;
    }
    return PATH_OK;
}
static slay_t _calc_slay(plr_shoot_ptr context, mon_ptr mon, slay_ptr best_slay)
{
    slay_t slay = {0};
    switch (context->mode)
    {
    case _KILL_WALL:
        if (!mon || mon_vuln(mon, GF_DISINTEGRATE))
        {
            if (mon) mon_lore_resist(mon, GF_DISINTEGRATE);
            slay.id = _KILL_WALL;
            slay.name = "Rock";
            slay.mul = 150 + 40*plr->concent;
        }
        else if (!mon || mon_is_nonliving(mon))
        {
            if (mon) mon_lore_nonliving(mon);
            slay.id = _KILL_WALL;
            slay.name = "Nonliving";
            slay.mul = 150 + 40*plr->concent;
        }
        break;
    case _EVILNESS:
        if (!mon || mon_is_good(mon))
        {
            if (mon) mon_lore_good(mon);
            slay.id = OF_SLAY_GOOD;
            slay.name = "Good";
            slay.mul = 150 + 60*plr->concent;
            if (have_flag(context->ammo_flags, OF_SLAY_GOOD))
                slay.mul += 50;
        }
        break;
    case _HOLINESS:
        if (!mon || mon_is_evil(mon))
        {
            if (mon) mon_lore_evil(mon);
            slay.id = OF_SLAY_EVIL;
            slay.name = "Evil";
            slay.mul = 150 + 60*plr->concent;
            if (mon && mon_vuln(mon, GF_LIGHT))
            {
                mon_lore_resist(mon, GF_LIGHT);
                slay.mul += 40*plr->concent;
            }
            if (have_flag(context->ammo_flags, OF_KILL_EVIL))
                slay.mul += 100;
            else if (have_flag(context->ammo_flags, OF_SLAY_EVIL))
                slay.mul += 50;
        }
        break;
    case _FINAL:
        slay.id = _FINAL;
        slay.name = "<color:v>*ALL*</color>";
        slay.mul = 800;
        break;
    }
    return slay;
}
static slay_t _calc_brand(plr_shoot_ptr context, mon_ptr mon, slay_ptr best_brand)
{
    slay_t brand = {0};
    int res_pct = 0;
    switch (context->mode)
    {
    case _FIRE:
        if (mon) res_pct = mon_res_pct(mon, GF_FIRE);
        if (res_pct) mon_lore_resist(mon, GF_FIRE);
        if (res_pct <= 0)
        {
            brand.id = OF_BRAND_FIRE;
            brand.name = "Fire";
            brand.mul = 150 + 50*plr->concent;
            if (have_flag(context->ammo_flags, OF_BRAND_FIRE)) brand.mul += 50;
            if (res_pct < 0) slay_scale(&brand, 100 - res_pct);
        }
        break;
    case _COLD:
        if (mon) res_pct = mon_res_pct(mon, GF_COLD);
        if (res_pct) mon_lore_resist(mon, GF_COLD);
        if (res_pct <= 0)
        {
            brand.id = OF_BRAND_COLD;
            brand.name = "Cold";
            brand.mul = 150 + 50*plr->concent;
            if (have_flag(context->ammo_flags, OF_BRAND_COLD)) brand.mul += 50;
            if (res_pct < 0) slay_scale(&brand, 100 - res_pct);
        }
        break;
    case _ELEC:
        if (mon) res_pct = mon_res_pct(mon, GF_ELEC);
        if (res_pct) mon_lore_resist(mon, GF_ELEC);
        if (res_pct <= 0)
        {
            brand.id = OF_BRAND_ELEC;
            brand.name = "Elec";
            brand.mul = 180 + 60*plr->concent;
            if (have_flag(context->ammo_flags, OF_BRAND_ELEC))
                brand.mul += 70;
            if (res_pct < 0) slay_scale(&brand, 100 - res_pct);
        }
        break;
    case _LITE:
        if (mon) res_pct = mon_res_pct(mon, GF_LIGHT);
        if (!mon || res_pct < 0) /* GF_LIGHT_WEAK */
        {
            if (res_pct) mon_lore_resist(mon, GF_LIGHT);
            brand.id = OF_BRAND_LIGHT;
            brand.name = "Light";
            brand.mul = 200 + 20*plr->concent;
        }
        break;
    }
    return brand;
}
static bool _check_hit(plr_shoot_ptr context, mon_ptr mon)
{
    int dis = 1 + context->path_pos;
    bool hit = FALSE;

    if (context->mode == _NEEDLE)
        hit = TRUE;
    else
        hit = test_hit_fire(context->skill - dis, mon_ac(mon), mon->ml);

    return hit;
}
static void _before_hit(plr_shoot_ptr context, mon_ptr mon)
{
    if (context->mode == _NEEDLE)
    {
        mon_race_ptr race = mon->race;
        int lvl = race->alloc.lvl/(3 + plr->concent);
        int N = _1d(lvl) + 8 - plr->concent;
        if (_1d(N) == 1 && !mon_race_is_unique(race))
        {
            char m_name[MAX_NLEN];
            monster_desc(m_name, mon, 0);
            context->dam = mon->hp + 1;
            cmsg_format(TERM_RED, "You shot %s on a fatal spot!", m_name);
        }
        else
            context->dam_base = context->dam = 1;
    }
}
static void _after_hit(plr_shoot_ptr context, mon_ptr mon)
{
    if (mon_is_valid(mon) && context->mode == _KNOCKBACK)
    {
        /* animate path, knockback then return ... */
        do_monster_knockback(mon, 3 + _1d(5));
        if (context->action == AMMO_PIERCE) /* paranoia ... don't want a double hit */
            context->action = AMMO_DROP;
    }
    else if (context->mode == _EVILNESS || context->mode == _HOLINESS)
    {
        /* increase the breakage chance ... note that AMMO_PIERCE => AMMO_BREAK */
        /* XXX cf _breakage in plr_shoot ... the game engine will no longer allow artifact|endurance
         * ammo to be destroyed XXX */
        if (context->action != AMMO_PIERCE && !obj_is_art(context->ammo) && randint0(100) < 40)
            context->action = AMMO_BREAK;
    }
    else if (context->mode == _NEEDLE || context->mode == _FINAL)
    {
        context->action = AMMO_BREAK;
    }
}
static void _end(plr_shoot_ptr context)
{
    if (context->flags & PSC_DISPLAY) return;
    if (!context->shots) return; /* fear */

    switch (context->mode)
    {
    case _AWAY:
        teleport_player(10 + 2*plr->concent, 0);
        break;
    case _FINAL:
        msg_print("You experience a powerful recoil!");
        plr_tim_add(T_SLOW, 7 + _1d(6));
        if (!res_save(GF_STUN, 100))
            plr_tim_add(T_STUN, _1d(25));
        break;
    }

    if (plr->concent)
        reset_concentration(FALSE);
}
static void _shoot_init(plr_shoot_ptr context)
{
    context->hooks.begin_f = _begin;
    context->hooks.begin_bow_f = _begin_bow;
    context->hooks.prepath_f = _prepath;
    context->hooks.path_f = _path;
    context->hooks.get_shots_f = _get_shots;
    context->hooks.calc_slay_f = _calc_slay;
    context->hooks.calc_brand_f = _calc_brand;
    context->hooks.check_hit_f = _check_hit;
    context->hooks.before_hit_f = _before_hit;
    context->hooks.after_hit_f = _after_hit;
    context->hooks.end_f = _end;
}

/************************************************************************
 * Concentration
 ***********************************************************************/
static int _max_concentration(void)
{
    return 2 + (plr->lev + 5)/10;
}

void reset_concentration(bool msg)
{
    if (msg)
        msg_print("You stop concentrating.");

    if (plr->concent >= CONCENT_TELE_THRESHOLD)
        plr_tim_unlock(T_TELEPATHY);

    plr->concent = 0;
    reset_concent = FALSE;
    plr->update |= PU_BONUS;
    plr->redraw |= PR_STATUS;
    plr->update |= PU_MONSTERS;
}

static void _concentrate(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Concentration");
        break;
    case SPELL_DESC:
        var_set_string(res,
            "Concentrate your mind for more powerful shooting. As you increase "
            "your focus, you will gain access to more deadly archery techniques. "
            "In addition, you will land more critical shots as your aim improves.");
        break;
    case SPELL_INFO:
        var_set_string(res, format("(%d of %d)", plr->concent, _max_concentration()));
        break;
    case SPELL_CAST: {
        int max = _max_concentration();
        if (plr->concent < max)
        {
            plr->concent++;
            msg_format("You concentrate deeply (<color:%c>%dx</color>).",
                plr->concent == max ? 'r' : 'B',
                plr->concent);
            if (plr->concent >= CONCENT_TELE_THRESHOLD)
                plr_tim_lock(T_TELEPATHY);
            plr->update |= PU_BONUS;
            plr->redraw |= PR_STATUS;
            plr->update |= PU_MONSTERS;
        }
        else
            msg_format("You maintain maximum focus (<color:r>%dx</color>).", plr->concent);
        reset_concent = FALSE;
        var_set_bool(res, TRUE);
        break; }
    default:
        default_spell(cmd, res);
    }
}
static void _timer_on(plr_tim_ptr timer)
{
    switch (timer->id)
    {
    case T_PARALYZED:
    case T_CONFUSED:
    case T_HALLUCINATE:
    case T_STUN:
        if (plr->concent) reset_concentration(TRUE);
        break;
    }
}
/************************************************************************
 * Spells
 ***********************************************************************/
static bool _do_shot(int which)
{
    bool result = FALSE;
    if (!equip_find_obj(TV_BOW, SV_ANY))
    {
        msg_print("You need to wield a bow!");
        return FALSE;
    }
    command_cmd = 'f'; /* hack for @fa inscriptions */
    result = plr_shoot_special(which, 0);
    return result;
}
static void _default(int which, int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_CAST:
        var_set_bool(res, _do_shot(which));
        break;
    case SPELL_ON_BROWSE:
        if (plr->concent) {
            plr_shoot_t ctx = {0};
            ctx.mode = which;
            plr_shoot_display_aux(&ctx);
            var_set_bool(res, TRUE);
        }
        break;
    case SPELL_ENERGY:
        var_set_int(res, energy_use); /* roundabout ... but plr_shoot already set this */
        break;
    default:
        default_spell(cmd, res);
    }
}
static void _shining_arrow(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Shining Arrow");
        break;
    case SPELL_DESC:
        var_set_string(res,
            "Shoot a glowing arrow that lights up the dungeon. This "
            "shot also does increased damage from light against "
            "enemies that are hurt by bright light.");
        break;
    default:
        _default(_LITE, cmd, res);
    }
}
static void _shoot_and_away(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Shoot and Away");
        break;
    case SPELL_DESC:
        var_set_string(res, "Shoot at target and then blink away in a single move.");
        break;
    case SPELL_ON_BROWSE:
        break;
    default:
        _default(_AWAY, cmd, res);
    }
}
static void _disarming_shot(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Disarming Shot");
        break;
    case SPELL_DESC:
        var_set_string(res, "Shoot an arrow able to shatter traps.");
        break;
    case SPELL_ON_BROWSE:
        break;
    default:
        _default(_KILL_TRAP, cmd, res);
    }
}
static void _burning_shot(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Burning Shot");
        break;
    case SPELL_DESC:
        var_set_string(res, "Deals extra damage of fire.");
        break;
    default:
        _default(_FIRE, cmd, res);
    }
}
static void _shatter(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Shatter");
        break;
    case SPELL_DESC:
        var_set_string(res, "Shoot an arrow able to shatter rocks.");
        break;
    default:
        _default(_KILL_WALL, cmd, res);
    }
}
static void _freezing_shot(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Freezing Shot");
        break;
    case SPELL_DESC:
        var_set_string(res, "Deals extra damage of cold.");
        break;
    default:
        _default(_COLD, cmd, res);
    }
}
static void _knockback(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Knockback");
        break;
    case SPELL_DESC:
        var_set_string(res, "A powerful shot that knocks an enemy target backwards.");
        break;
    case SPELL_ON_BROWSE:
        break;
    default:
        _default(_KNOCKBACK, cmd, res);
    }
}
static void _piercing_shot(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Piercing Shot");
        break;
    case SPELL_DESC:
        var_set_string(res, "An arrow pierces some monsters.");
        break;
    default:
        _default(PLR_SHOOT_PIERCE, cmd, res);
    }
}
static void _evil_shot(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Evil Shot");
        break;
    case SPELL_DESC:
        var_set_string(res, "Deals more damage to good monsters.");
        break;
    default:
        _default(_EVILNESS, cmd, res);
    }
}
static void _holy_shot(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Holy Shot");
        break;
    case SPELL_DESC:
        var_set_string(res, "Deals more damage to evil monsters.");
        break;
    default:
        _default(_HOLINESS, cmd, res);
    }
}
static void _exploding_shot(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Exploding Shot");
        break;
    case SPELL_DESC:
        var_set_string(res, "An arrow explodes when it hits a monster.");
        break;
    case SPELL_ON_BROWSE:
        break;
    default:
        _default(PLR_SHOOT_EXPLODE, cmd, res);
    }
}
static void _double_shot(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Double Shot");
        break;
    case SPELL_DESC:
        var_set_string(res, "Shoot arrows twice.");
        break;
    case SPELL_ON_BROWSE:
        break;
    default:
        _default(_DOUBLE, cmd, res);
    }
}
static void _thunder_shot(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Thunder Shot");
        break;
    case SPELL_DESC:
        var_set_string(res, "Deals great extra damage of lightning.");
        break;
    default:
        _default(_ELEC, cmd, res);
    }
}
static void _needle_shot(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Needle Shot");
        break;
    case SPELL_DESC:
        var_set_string(res, "Deals quick death or 1 damage.");
        break;
    case SPELL_ON_BROWSE:
        break;
    default:
        _default(_NEEDLE, cmd, res);
    }
}
static void _saint_stars_arrow(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Saint Stars Arrow");
        break;
    case SPELL_DESC:
        var_set_string(res, "Deals great damage to all monsters, and some side effects to you.");
        break;
    default:
        _default(_FINAL, cmd, res);
    }
}
static spell_info _spells[] = 
{
   /*lvl  cst fail  spell */
    {  1,   0,   0, _concentrate },
    {  2,   1,   0, _shining_arrow },
    {  3,   1,   0, _shoot_and_away },
    {  5,   1,   0, _disarming_shot },
    {  8,   2,   0, _burning_shot },
    { 10,   2,   0, _shatter },
    { 13,   2,   0, _freezing_shot },
    { 18,   2,   0, _knockback },
    { 22,   3,   0, _piercing_shot },
    { 25,   4,   0, _evil_shot },
    { 26,   4,   0, _holy_shot },
    { 30,   3,   0, _exploding_shot },
    { 32,   4,   0, _double_shot },
    { 36,   3,   0, _thunder_shot },
    { 40,   3,   0, _needle_shot },
    { 48,   7,   0, _saint_stars_arrow },
    { -1,  -1,  -1, NULL}
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
        me.magic_desc = "sniping";
        me.options = CASTER_USE_CONCENTRATION;
        me.which_stat = A_DEX;
        init = TRUE;
    }
    return &me;
}

/************************************************************************
 * Powers
 ***********************************************************************/
static int _get_powers(spell_info* spells, int max)
{
    int ct = 0;

    spell_info* spell = &spells[ct++];
    spell->level = 15;
    spell->cost = 20;
    spell->fail = calculate_fail_rate(spell->level, 80, plr->stat_ind[A_INT]);
    spell->fn = probing_spell;

    return ct;
}

/************************************************************************
 * Class
 ***********************************************************************/
static void _character_dump(doc_ptr doc)
{
    spell_info spells[MAX_SPELLS];
    int        ct = _get_spells(spells, MAX_SPELLS);

    plr_display_spells(doc, spells, ct);
}
plr_class_ptr sniper_get_class(void)
{
    static plr_class_ptr me = NULL;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 25,  24,  28,   5,  32,  18,  35,  72};
    skills_t xs = { 60,  50,  50,   0,   0,   0,  60, 140};

        me = plr_class_alloc(CLASS_SNIPER);
        me->name = "Sniper";
        me->desc = "Snipers are specialists in marksmanship, but not like archers who "
                    "fire off arrow after arrow in swift succession. They don't just "
                    "increase accuracy and power of shots by concentration, they can use "
                    "fearsome archery techniques.\n \n"
                    "What they require is powerful bows or crossbows, good quality "
                    "ammunition and the fortitude to bear up without flinching under " 
                    "any situation.\n \n"
                    "Snipers know their enemies well and can shoot them from the shadows. "
                    "They have no time for magic.";

        me->stats[A_STR] =  2;
        me->stats[A_INT] = -1;
        me->stats[A_WIS] = -1;
        me->stats[A_DEX] =  2;
        me->stats[A_CON] =  1;
        me->stats[A_CHR] =  0;
        me->skills = bs;
        me->extra_skills = xs;
        me->life = 100;
        me->base_hp = 4;
        me->exp = 110;
        me->pets = 40;
        me->flags = CLASS_SENSE1_SLOW | CLASS_SENSE1_STRONG;
        
        me->hooks.birth = _birth;
        me->hooks.calc_shooter_bonuses = _calc_shooter_bonuses;
        me->hooks.get_powers = _get_powers;
        me->hooks.caster_info = _caster_info;
        me->hooks.get_spells = _get_spells;
        me->hooks.character_dump = _character_dump;
        me->hooks.timer_on = _timer_on;
        me->hooks.shoot_init = _shoot_init;
    }

    return me;
}
