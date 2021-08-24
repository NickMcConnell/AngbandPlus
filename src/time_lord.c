#include "angband.h"

#include <assert.h>

/****************************************************************
 * Private Helpers
 ****************************************************************/

/* Finding what monster to evolve into is trivial, since the monster_race type
   keeps a pointer in that direction. However, we would like to reverse evolution
   turning harder monsters into easier ones. This fn will scan the monster race
   table looking for a monster that evolves into this one. In case multiple races
   evolve into a given form, a random choice is selected.
   Returns 0 if no such race can be found.
*/
static int _find_evolution_idx(int r_idx)
{
    monster_race *r_ptr;

    if (r_idx <= 0) return 0;
    r_ptr = mon_race_lookup(r_idx);
    return r_ptr->evolution.id;
}

static int _r_idx;
static bool _devolution_p(mon_race_ptr r) { return r->evolution.id == _r_idx; }
static int _find_devolution_idx(int r_idx)
{
    vec_ptr v;
    int id = 0;
    _r_idx = r_idx;
    v = mon_race_filter(_devolution_p);
    if (vec_length(v))
    {
        mon_race_ptr r = vec_random(v);
        id = r->id;
    }
    vec_free(v);
    return id;
}

/*  Evolve or Devolve a Monster. I spiked this from monster_gain_exp() in melee2.c without
    any great understanding on my part.
    UPDATE: Use this for polymorph monster as well (spells3.c).
*/
void mon_change_race(mon_ptr mon, int new_r_idx, cptr verb)
{
    char m_name[80], new_name[80];
    int old_hp, old_maxhp, old_align;
    mon_race_ptr old_race;

    assert(mon);
    if (new_r_idx <= 0) return;

    old_hp = mon->hp;
    old_maxhp = mon->max_maxhp;
    old_race = mon->race;
    old_align = mon->align;

    assert(mon_true_race(mon)->alloc.cur_num > 0);
    mon_true_race(mon)->alloc.cur_num--;

    monster_desc(m_name, mon, 0);
    mon->race = mon_race_lookup(new_r_idx);
    mon->align = mon->race->align;
    mon->apparent_race = mon->race;
    mon_drop_init(mon);

    mon_true_race(mon)->alloc.cur_num++;

    mon->max_maxhp = dice_roll(mon->race->hp);
    mon->maxhp = mon->max_maxhp;
    mon->hp = old_hp * mon->maxhp / old_maxhp;

    mon->mspeed = get_mspeed(mon->race);

    /* maintain allegiance with master if possible */
    if (!mon_is_pet(mon) && !mon->align)
        mon->align = old_align;

    mon->exp = 0;

    if (mon_is_pet(mon) || mon->ml)
    {
        if (!ignore_unview || plr_can_see(mon->pos))
        {
            if (plr_tim_find(T_HALLUCINATE))
            {
                monster_race *hallu_race;
                do
                {
                    hallu_race = vec_random(mon_alloc_tbl);
                }
                while (mon_race_is_unique(hallu_race));
                msg_format("%^s changed into %s.", m_name, hallu_race->name);
            }
            else
            {
                monster_desc(new_name, mon, 0);
                cmsg_format(TERM_L_BLUE, "%^s %s into %s.", m_name, verb, new_name);
            }
        }
        /* XXX detect evolution vs polymorph effects XXX */
        if (!plr_tim_find(T_HALLUCINATE) && old_race->evolution.id == new_r_idx)
            old_race->lore.flags |= RFL_EVOLUTION;
        mon_set_parent(mon, 0);
    }

    update_mon(mon, FALSE);
    draw_pos(mon->pos);
}

static bool _monster_save(monster_race* r_ptr, int power)
{
    if (mon_race_is_unique(r_ptr))
        return r_ptr->alloc.lvl > randint1(2*power/3);
    else
        return r_ptr->alloc.lvl > randint1(power);
}

bool devolve_monster(int m_idx, bool msg)
{
    monster_type* m_ptr = dun_mon(cave, m_idx);
    monster_race *r_ptr;
    int r_idx = real_r_idx(m_ptr);
    char m_name[MAX_NLEN];

    if (r_idx <= 0) return FALSE;

    r_ptr = mon_race_lookup(r_idx);    /* We'll use the current race for a saving throw */
    r_idx = _find_devolution_idx(r_idx);
    monster_desc(m_name, m_ptr, 0);

    if (r_idx <= 0)
    {
        if (msg)
            msg_format("%^s is too primitive for further devolution.", m_name);
        return FALSE;
    }

    if (_monster_save(r_ptr, 2*plr->lev))
    {
        if (msg)
            msg_format("%^s resists.", m_name);
        return FALSE;
    }

    mon_tim_delete(m_ptr, MT_SLEEP);
    mon_change_race(m_ptr, r_idx, "devolved");
    return TRUE;
}

bool evolve_monster(int m_idx, bool msg)
{
    monster_type* m_ptr = dun_mon(cave, m_idx);
    monster_race *r_ptr;
    int r_idx = real_r_idx(m_ptr);
    char m_name[MAX_NLEN];

    if (r_idx <= 0) return FALSE;
    monster_desc(m_name, m_ptr, 0);
    r_idx = _find_evolution_idx(r_idx);

    if (r_idx <= 0)
    {
        if (msg)
            msg_format("%^s has reached evolutionary perfection.", m_name);
        return FALSE;
    }
    r_ptr = mon_race_lookup(r_idx);    /* We'll use the target race for a saving throw */
    mon_tim_delete(m_ptr, MT_SLEEP);
    if (_monster_save(r_ptr, 2*plr->lev))
    {
        if (msg)
            msg_format("%^s resists.", m_name);
        return FALSE;
    }
    mon_change_race(m_ptr, r_idx, "evolved");
    return TRUE;
}

/****************************************************************
 * Private Timers
 ****************************************************************/
enum {
    _QUICKEN = T_CUSTOM,
    _SHIELD,
    _FORESIGHT,
};

/* _QUICKEN */
static bool _quicken_on(plr_tim_ptr timer)
{
    if (plr_tim_find(T_FAST)) return FALSE;
    msg_print("You feel time slow down.");
    plr->update |= PU_BONUS;
    return TRUE;
}
static void _quicken_off(plr_tim_ptr timer)
{
    msg_print("You feel time speed up.");
    plr->update |= PU_BONUS;
}
static void _quicken_bonus(plr_tim_ptr timer)
{
    plr_bonus_speed(3); /* won't stack with T_FAST; also works if riding */
}
static void _quicken_flags(plr_tim_ptr timer, u32b flags[OF_ARRAY_SIZE])
{
    add_flag(flags, OF_SPEED);
}
static status_display_t _quicken_display(plr_tim_ptr timer)
{
    return status_display_create("Quick", "Qk", TERM_YELLOW);
}
static plr_tim_info_ptr _quicken(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(_QUICKEN, "Quicken");
    info->desc = "The flow of time has slowed down.";
    info->on_f = _quicken_on;
    info->off_f = _quicken_off;
    info->calc_bonuses_f = _quicken_bonus;
    info->flags_f = _quicken_flags;
    info->status_display_f = _quicken_display;
    return info;
}

/* _SHIELD */
static bool _shield_on(plr_tim_ptr timer)
{
    msg_print("You are cloaked in time.");
    return TRUE;
}
static void _shield_off(plr_tim_ptr timer)
{
    msg_print("You are no longer cloaked in time.");
}
static status_display_t _shield_display(plr_tim_ptr timer)
{
    return status_display_create("Shield", "Sh", TERM_L_BLUE);
}
static plr_tim_info_ptr _shield(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(_SHIELD, "Shield");
    info->desc = "You are protected by a temporal aura.";
    info->on_f = _shield_on;
    info->off_f = _shield_off;
    info->status_display_f = _shield_display;
    return info;
}

/* _FORESIGHT */
static bool _foresight_on(plr_tim_ptr timer)
{
    msg_print("You can see the future!");
    return TRUE;
}
static void _foresight_off(plr_tim_ptr timer)
{
    msg_print("You can no longer see the future.");
}
static status_display_t _foresight_display(plr_tim_ptr timer)
{
    return status_display_create("Foresight", "Fs", TERM_YELLOW);
}
static plr_tim_info_ptr _foresight(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(_FORESIGHT, "Foresight");
    info->desc = "You are looking into the future to avoid monster attacks.";
    info->on_f = _foresight_on;
    info->off_f = _foresight_off;
    info->status_display_f = _foresight_display;
    info->dispel_prob = 100;
    return info;
}

static void _register_timers(void)
{
    plr_tim_register(_quicken());
    plr_tim_register(_shield());
    plr_tim_register(_foresight());
}

/****************************************************************
 * Attack
 ****************************************************************/
static void _after_hit(mon_attack_ptr context)
{
    if (plr_tim_find(_SHIELD)) /* paranoia */
    {
        int dam = 2 + damroll(1 + (plr->lev / 10), 2 + (plr->lev / 10));
        gf_affect_m(who_create_plr(), context->mon, GF_TIME, dam, GF_AFFECT_AURA);
    }
}
static void _mon_attack_init(mon_attack_ptr context)
{
    if (plr_tim_find(_SHIELD))
        context->after_hit_f = _after_hit;
}

/****************************************************************
 * Private Spells
 ****************************************************************/
static void _bolt_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Bolt");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a temporal bolt at chosen foe. Time based attacks may produce various "
                            "effects on a monster including slowing, stasis, and many others.");
        break;
    default:
        bolt_spell(cmd, res, GF_TIME, 3 + plr->lev/4, 4);
    }
}

static void _regeneration_spell(int cmd, var_ptr res)
{
    int b = spell_power(80);
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Regeneration");
        break;
    case SPELL_DESC:
        var_set_string(res, "Speeds your recovery from physical damage.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_duration(b, b));
        break;
    case SPELL_CAST:
        plr_tim_add(T_REGEN, b + randint1(b));
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _foretell_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Foretell");
        break;
    case SPELL_DESC:
        var_set_string(res, "Detects nearby monsters.");
        break;
    case SPELL_CAST:
        detect_monsters_normal(DETECT_RAD_DEFAULT);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _quicken_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Quicken");
        break;
    case SPELL_DESC:
        var_set_string(res, "Gives a small speed boost for a short while.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_duration(7, 7));
        break;
    case SPELL_CAST:
        plr_tim_add(_QUICKEN, spell_power(7 + randint1(7)));
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _withering_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Wither");
        break;
    case SPELL_DESC:
        var_set_string(res, "Destroy an adjacent wall, tree or door.");
        break;
    case SPELL_CAST:
    {
        int dir;
        point_t pos;
        dun_cell_ptr cell;
        bool tree;

        var_set_bool(res, FALSE);
        if (!get_rep_dir2(&dir)) return;
        var_set_bool(res, TRUE);

        if (dir == 5) return;

        pos = point_step(plr->pos, dir);
        if (!dun_pos_interior(cave, pos)) return;
        cell = dun_cell_at(cave, pos);
        tree = cell_is_tree(cell);

        if (cell_is_door(cell))
        {
            cave->type->place_floor(cave, pos);
            msg_print("The door withers away.");
        }
        else if (dun_tunnel(cave, pos, ACTION_FORCE | ACTION_QUIET) == ACTION_SUCCESS)
        {
            if (tree) msg_print("The tree shrivels and dies.");
            else msg_print("The wall turns to dust.");
        }
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _blast_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Blast");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a temporal blast at chosen foe. Time based attacks may produce various "
                            "effects on a monster including slowing, stasis, and many others.");
        break;
    default:
        ball_spell(cmd, res, 2, GF_TIME, 15 + 3*plr->lev/2);
    }
}

static bool _reversion_p(mon_ptr mon)
{
    mon_race_ptr race = real_r_ptr(mon);
    if ( mon_can_multiply(mon)
      && race->alloc.cur_num > 1  /* shouldn't this be 2 ... well, breeding in *band has never been 'bio-logical' */
      && !_monster_save(race, 3*plr->lev) )
    {
        return TRUE;
    }
    return FALSE;
}
static void _back_to_origins_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Revert");
        break;
    case SPELL_DESC:
        var_set_string(res, "Eliminate monster offspring.");
        break;
    case SPELL_CAST:
    {
        vec_ptr v = dun_filter_mon(cave, _reversion_p);
        int i;

        for (i = 0; i < vec_length(v); i++)
        {
            mon_ptr mon = vec_get(v, i);
            delete_monster(mon);
        }
        if (vec_length(v) > 0)
            msg_print("You feel the local population has reverted to an earlier state.");
        else
            msg_print("You feel the local population is stable.");

        vec_free(v);
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _haste_spell(int cmd, var_ptr res)
{
    int base = spell_power(plr->lev);
    int sides = spell_power(20 + plr->lev);
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Haste");
        break;
    case SPELL_DESC:
        var_set_string(res, "You gain a temporary speed boost.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_duration(base, sides));
        break;
    case SPELL_CAST:
        plr_tim_add(T_FAST, base + randint1(sides));
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _wave_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Wave");
        break;
    case SPELL_DESC:
        var_set_string(res, "Produce a wave of time, affecting all monsters in sight.");
        break;
    default:
        los_spell_aux(cmd, res, GF_TIME, spell_dam_dice(1, 3*plr->lev/2, 0));
    }
}

static void _shield_spell(int cmd, var_ptr res)
{
    int b = spell_power(15);
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Shield");
        break;
    case SPELL_DESC:
        var_set_string(res, "Grants an Aura of Time for a short while.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_duration(b, b));
        break;
    case SPELL_CAST:
        plr_tim_add(_SHIELD, b + randint1(b));
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _rewind_time_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Rewind");
        break;
    case SPELL_DESC:
        var_set_string(res, "Temporal escape:  You flee to safety, but forget some of your recent experiences.");
        break;
    case SPELL_CAST:
        var_set_bool(res, FALSE);
        if (!get_check("You will irreversibly alter the time line. Are you sure?")) return;
        var_set_bool(res, TRUE);

        if (cave->type->id == D_SURFACE)
        {
            msg_print("Nothing happens.");
            return;
        }

        dun_mgr_recall_plr();

        if (plr->prace == RACE_ANDROID)
        {
            dec_stat(A_CON, 10, TRUE);
            if (one_in_(2)) return;
            dec_stat(A_INT, 10, TRUE);
            if (one_in_(2)) return;
            dec_stat(A_DEX, 10, TRUE);
            if (one_in_(2)) return;
            dec_stat(A_WIS, 10, TRUE);
            if (one_in_(2)) return;
            dec_stat(A_STR, 10, TRUE);
            if (one_in_(2)) return;
            dec_stat(A_CHR, 10, TRUE);
        }
        else
        {
            int amount = 0;

            if (plr->lev < 3) return;
            amount = exp_requirement(plr->lev-1);
            amount -= exp_requirement(plr->lev-2);
            if (amount > 100000) amount = 100000;
            if (amount > plr->max_exp) amount = plr->max_exp;
            if (amount > plr->exp) plr->exp = 0;
            else plr->exp -= amount;
            plr->max_exp -= amount;
            check_experience();
        }
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static int  _breath_dam(void) {
    int l = (plr->lev - 30);
    return spell_power(9*plr->lev/2 + l*l/4 + plr->to_d_spell); /* 325 max damage ... */
}
static void _breath_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Breath");
        break;
    case SPELL_DESC:
        var_set_string(res, "Breathe time at chosen foe. Time based attacks may produce various "
                            "effects on a monster including slowing, stasis, and many others.");
        break;
    default:
        breath_spell(cmd, res, 3, GF_TIME, _breath_dam());
    }
}

static void _remember_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Remembrance");
        break;
    case SPELL_DESC:
        var_set_string(res, "Restores life and stats.");
        break;
    case SPELL_CAST:
        do_res_stat(A_STR);
        do_res_stat(A_INT);
        do_res_stat(A_WIS);
        do_res_stat(A_DEX);
        do_res_stat(A_CON);
        do_res_stat(A_CHR);
        restore_level();
        plr_restore_life(1000);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _stasis_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Stasis");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attempts to suspend all monsters in view.");
        break;
    default:
        los_spell(cmd, res, GF_STASIS, 4*plr->lev);
    }
}

static void _travel_spell(int cmd, var_ptr res)
{
    int r = spell_power(plr->lev / 2 + 10);
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Travel");
        break;
    case SPELL_DESC:
        var_set_string(res, "Travel instantaneously to given location. Be careful you don't accidentally get lost!");
        break;
    case SPELL_INFO:
        var_set_string(res, info_range(r));
        break;
    case SPELL_CAST:
        var_set_bool(res, dimension_door(r));
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _double_move_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Double Move");
        break;
    case SPELL_DESC:
        var_set_string(res, "After casting this spell, you may take two additional free moves. Make the most of them!");
        break;
    case SPELL_CAST:
        if (plr->free_turns)
        {
            msg_print("You're wasting your free turns!");
        }
        else
        {
            plr->free_turns = 3; /* this spell + 2 more free moves */
            plr->redraw |= PR_EFFECTS;
        }
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _foresee_spell(int cmd, var_ptr res)
{
    int b = spell_power(7);
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Foresee");
        break;
    case SPELL_DESC:
        var_set_string(res, "For a very short time, you will be able to look into the future.");
        break;
    case SPELL_INFO:
        var_set_string(res, format("dur %d", b));
        break;
    case SPELL_CAST:
        plr_tim_add(_FORESIGHT, b);
        var_set_bool(res, TRUE);
        break;
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
    {  1,  2, 30, _bolt_spell},
    {  3,  3, 40, _regeneration_spell},
    {  6,  4, 40, _foretell_spell},
    {  8,  8, 50, _quicken_spell},
    { 10,  9, 50, _withering_spell},
    { 13, 10, 50, _blast_spell},
    { 17, 12, 50, _back_to_origins_spell},
    { 23, 15, 60, _haste_spell},
    { 27, 20, 60, _wave_spell},
    { 30, 10, 60, _shield_spell},
    { 33, 50, 70, _rewind_time_spell},
    { 35, 35, 70, _breath_spell},
    { 37, 50, 70, _remember_spell},
    { 39, 30, 70, _stasis_spell},
    { 41, 20, 80, _travel_spell},
    { 45, 80, 80, _double_move_spell},
    { 49,100, 80, _foresee_spell},
    { -1, -1, -1, NULL}
};

static int _get_spells(spell_info* spells, int max)
{
    return get_spells_aux(spells, max, _spells);
}

static void _dec_mana(cptr which)
{
    if (equip_find_art(which))
    {
        plr->dec_mana++;
        plr->easy_spell++;
    }
}

static void _calc_bonuses(void)
{
    _dec_mana("|.Eternal");
    _dec_mana("(.Eternity");
    _dec_mana("=.Ages");

    if (plr->lev >= 30) res_add(GF_TIME);
    plr->pspeed += plr->lev / 7;
}

static void _get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    if (plr->lev >= 30) add_flag(flgs, OF_RES_(GF_TIME));
    if (plr->lev >= 7) add_flag(flgs, OF_SPEED);
}

static void _on_fail(const spell_info *spell)
{
    if (randint1(100) < (spell->fail / 2))
    {
        int b = randint1(100);
        if (b <= 90)
        {
        }
        else if (b <= 95)
        {
            plr_tim_remove(T_FAST);
            plr_tim_add(T_SLOW, randint1(5) + 5);
            msg_print("You feel caught in a temporal inversion!");
        }
        else if (b <= 99)
        {
            lose_exp(plr->exp / 4);
            msg_print("You feel life's experiences fade away!");
        }
        else
        {
            dec_stat(A_STR, 10, FALSE);
            dec_stat(A_INT, 10, FALSE);
            dec_stat(A_WIS, 10, FALSE);
            dec_stat(A_DEX, 10, FALSE);
            dec_stat(A_CON, 10, FALSE);
            dec_stat(A_CHR, 10, FALSE);
            msg_print("You feel as weak as a newborn kitten!");
        }
    }
}

static caster_info * _caster_info(void)
{
    static caster_info me = {0};
    static bool init = FALSE;
    if (!init)
    {
        me.magic_desc = "timecraft";
        me.which_stat = A_WIS;
        me.encumbrance.max_wgt = 400;
        me.encumbrance.weapon_pct = 67;
        me.encumbrance.enc_wgt = 800;
        me.on_fail = _on_fail;
        me.options = CASTER_GAIN_SKILL;
        init = TRUE;
    }
    return &me;
}

static void _character_dump(doc_ptr doc)
{
    spell_info spells[MAX_SPELLS];
    int        ct = _get_spells(spells, MAX_SPELLS);

    plr_display_spells(doc, spells, ct);
}

static void _birth(void)
{
    plr_birth_obj_aux(TV_SWORD, SV_SHORT_SWORD, 1);
    plr_birth_obj_aux(TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR, 1);
    plr_birth_obj_aux(TV_POTION, SV_POTION_SPEED, rand_range(4, 7));
}

static void _player_action(void)
{
    if (plr->free_turns)
    {
        plr->redraw |= PR_EFFECTS;
        plr->free_turns--;
        /* 3->2 is the spell - no energy
         * 2->1 is the first move - no energy
         * 1->0 is the second and gets charged energy */
        if (plr->free_turns)
            energy_use = 0;
    }
}

static void _prt_effects(doc_ptr doc)
{
    if (plr->free_turns)
        doc_insert(doc, "<color:y>DblMove</color>\n");
}

/************************************************************************
 * Public
 ************************************************************************/
plr_class_ptr time_lord_get_class(void)
{
    static plr_class_ptr me = NULL;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 25,  35,  35,   2,  16,   8,  48,  20 };
    skills_t xs = { 35,  55,  50,   0,   0,   0,  65,  65 };

        me = plr_class_alloc(CLASS_TIME_LORD);
        me->name = "Time-Lord";
        me->desc = "Time-Lords are masters of temporal magic, altering the flow of time "
                  "to their advantage. They don't learn spells from books, but rather "
                  "gain new powers as they grow more experienced. They are the only class "
                  "that can affect monsters with time based attacks. Not only do these damage "
                  "their foes, but they also inflict a wide variety of possible effects, from "
                  "slowing to amnesia, from evolution to devolution, from weakening to stasis. "
                  "In addition to temporal attacks, the Time-Lord gains great powers of speed, and "
                  "they grow faster with experience. It is said that masters of time can even see "
                  "the future, avoiding attacks that would otherwise prove fatal! Also, legend "
                  "has it that the greatest Time-Lords are able to take multiple actions in a single "
                  "turn.\n \n"
                  "Time-Lords are mediocre fighters and not good at archery at all. They are OK with "
                  "magical devices, but nowhere near as proficient as are mages. They have midling "
                  "stealth. At high levels, they become resistant to time. The Time-Lord's primary "
                  "magic stat is Wisdom.";

        me->stats[A_STR] = -1;
        me->stats[A_INT] =  0;
        me->stats[A_WIS] =  3;
        me->stats[A_DEX] = -1;
        me->stats[A_CON] = -1;
        me->stats[A_CHR] =  0;

        me->skills = bs;
        me->extra_skills = xs;
        me->life = 96;
        me->base_hp = 0;
        me->exp = 125;
        me->pets = 20;
        me->flags = CLASS_SENSE1_FAST | CLASS_SENSE1_WEAK |
                    CLASS_SENSE2_MED | CLASS_SENSE2_STRONG;

        me->hooks.birth = _birth;
        me->hooks.register_timers = _register_timers;
        me->hooks.calc_bonuses = _calc_bonuses;
        me->hooks.get_flags = _get_flags;
        me->hooks.caster_info = _caster_info;
        me->hooks.get_spells = _get_spells;
        me->hooks.character_dump = _character_dump;
        me->hooks.mon_attack_init = _mon_attack_init;
        me->hooks.player_action = _player_action;
        me->hooks.prt_effects = _prt_effects;
    }

    return me;
}

bool check_foresight(void)
{
    if (psion_check_foresight()) return TRUE;
    if (plr->pclass != CLASS_TIME_LORD) return FALSE;

    if (plr_tim_find(_FORESIGHT) && randint1(100) <= 25)
    {
        msg_print("You saw that one coming!");
        return TRUE;
    }

    return FALSE;
}

