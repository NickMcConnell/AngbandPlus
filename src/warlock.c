/****************************************************************
 * The Warlock
 ****************************************************************/

#include "angband.h"

#include <assert.h>

/****************************************************************
 * Toggles are techniques or songs that grant bonuses as long
 * as they are maintained. In addition, the warlock might pay
 * upkeep to maintain the 'toggle'.
 ****************************************************************/
static void _toggle_on(int toggle)
{
    switch(toggle)
    {
    case WARLOCK_DRAGON_TOGGLE_HEROIC_CHARGE:
        plr_tim_lock(T_HERO);
        break;
    case WARLOCK_DRAGON_TOGGLE_BLESS:
        plr_tim_lock(T_BLESSED);
        break;
    }
}

static void _toggle_off(int toggle)
{
    switch(toggle)
    {
    case WARLOCK_DRAGON_TOGGLE_HEROIC_CHARGE:
        plr_tim_unlock(T_HERO);
        break;
    case WARLOCK_DRAGON_TOGGLE_BLESS:
        plr_tim_unlock(T_BLESSED);
        break;
    }
}

static int _get_toggle(void)
{
    return plr->magic_num1[0];
}

static int _set_toggle(s32b toggle)
{
    int result = plr->magic_num1[0];

    if (toggle == result) return result;

    _toggle_off(result);
    plr->magic_num1[0] = toggle;
    _toggle_on(toggle);

    plr->redraw |= PR_STATUS;
    plr->update |= PU_BONUS;
    handle_stuff();

    return result;
}

int warlock_get_toggle(void)
{
    int result = TOGGLE_NONE;
    if (warlock_is_(WARLOCK_DRAGONS) && plr->riding)
        result = _get_toggle();
    else if (plr->pclass == CLASS_WARLOCK) /* In case I add toggles for other pacts ... */
        result = _get_toggle();
    return result;
}

void warlock_stop_singing(void)
{   /* Dragon Songs */
    if (warlock_is_(WARLOCK_DRAGONS))
        _set_toggle(TOGGLE_NONE);
}

/****************************************************************
 * The Eldritch Blast
 ****************************************************************/
static int _blast_range(void)
{
    int rng = 5;

    if (plr->lev >= 48)
        rng = 8;
    else if (plr->lev >= 32)
        rng = 7;
    else if (plr->lev >= 16)
        rng = 6;

    return rng;
}

static int _blast_dd(void)
{
    return 1 + (plr->lev/5) + (plr->lev * plr->lev * 10/2500);
}

static int _blast_ds(void)
{
    static int _table[] =
    {
        4    /* 3 */,
        5    /* 4 */,
        5    /* 5 */,
        6    /* 6 */,
        6    /* 7 */,
        6    /* 8 */,
        6    /* 9 */,
        6    /* 10 */,
        6    /* 11 */,
        6    /* 12 */,
        6    /* 13 */,
        6    /* 14 */,
        7    /* 15 */,
        7    /* 16 */,
        7    /* 17 */,
        8    /* 18/00-18/09 */,
        8    /* 18/10-18/19 */,
        8    /* 18/20-18/29 */,
        8    /* 18/30-18/39 */,
        8    /* 18/40-18/49 */,
        9    /* 18/50-18/59 */,
        9    /* 18/60-18/69 */,
        10    /* 18/70-18/79 */,
        10    /* 18/80-18/89 */,
        11    /* 18/90-18/99 */,
        11    /* 18/100-18/109 */,
        12    /* 18/110-18/119 */,
        12    /* 18/120-18/129 */,
        13    /* 18/130-18/139 */,
        13    /* 18/140-18/149 */,
        14    /* 18/150-18/159 */,
        14    /* 18/160-18/169 */,
        15    /* 18/170-18/179 */,
        16    /* 18/180-18/189 */,
        17    /* 18/190-18/199 */,
        18    /* 18/200-18/209 */,
        19    /* 18/210-18/219 */,
        20    /* 18/220+ */
    };
    return _table[plr->stat_ind[A_CHR]];
}

static dice_t _blast_dice(int scale)
{
    dice_t dice = spell_dam_dice(_blast_dd(), _blast_ds(), 0);
    if (scale)
    {
        assert(dice.scale);
        dice.scale = dice.scale * scale / 1000;
    }
    return dice;
}

static void _blast_aux(int cmd, var_ptr res, dice_t dice, int rng)
{
    switch (cmd)
    {
    case SPELL_INFO:
        var_printf(res, "dam ~%d (rng %d)", dice_avg_roll(dice), rng);
        break;
    default:
        default_spell(cmd, res);
    }
}
static void _blast_ball_aux(int cmd, var_ptr res, int rad, int gf, dice_t dice, int rng)
{
    switch (cmd)
    {
    case SPELL_CAST:
        var_set_bool(res, plr_cast_ball_aux(rad, gf, dice, rng));
        break;
    default:
        _blast_aux(cmd, res, dice, rng);
    }
}
static void _blast_beam_aux(int cmd, var_ptr res, int gf, dice_t dice, int rng)
{
    switch (cmd)
    {
    case SPELL_CAST:
        var_set_bool(res, plr_cast_beam_aux(gf, dice, rng));
        break;
    default:
        _blast_aux(cmd, res, dice, rng);
    }
}
static void _basic_blast(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Eldritch Blast");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires your basic Eldritch Blast.");
        break;
    default:
        _blast_ball_aux(cmd, res, 0, GF_ELDRITCH, _blast_dice(1000), _blast_range());
    }
}

static void _extended_blast(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Blast: Extended");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a slightly weakened Eldritch Blast with increased range.");
        break;
    default:
        _blast_ball_aux(cmd, res, 0, GF_ELDRITCH, _blast_dice(750), _blast_range() + plr->lev/5);
    }
}

static void _spear_blast(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Blast: Spear");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires an Eldritch Beam.");
        break;
    default:
        _blast_beam_aux(cmd, res, GF_ELDRITCH, _blast_dice(1000), _blast_range());
    }
}

static void _burst_blast(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Blast: Burst");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires an Eldritch Blast with increased radius.");
        break;
    default:
        _blast_ball_aux(cmd, res, 2, GF_ELDRITCH, _blast_dice(1000), _blast_range());
    }
}

static void _stunning_blast(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Blast: Stunning");
        break;
    case SPELL_DESC:
        var_set_string(res, "Augments your Eldritch Blast with stunning effects.");
        break;
    default:
        _blast_ball_aux(cmd, res, 0, GF_ELDRITCH_STUN, _blast_dice(1000), _blast_range());
    }
}

static void _empowered_blast(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Blast: Empowered");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a very powerful Eldritch Blast, but you can't use your powers again for a bit.");
        break;
    default:
        _blast_ball_aux(cmd, res, 0, GF_ELDRITCH, _blast_dice(1750), _blast_range());
        if (cmd == SPELL_CAST && var_get_bool(res))
            plr_tim_add(T_NO_SPELLS, 2);
    }
}

/****************************************************************
 * The Warlock Pacts: Implemented as a Subclass
 ****************************************************************/
#define _MAX_SPELLS  20

typedef struct {
    cptr name;
    cptr desc;
    cptr alliance;
    calc_bonuses_f calc_bonuses;
    calc_weapon_bonuses_f calc_weapon_bonuses;
    stats_f calc_stats;
    flags_f get_flags;
    process_player_f process_player;
    status_display_f status_display;
    int stats[MAX_STATS];
    skills_t base_skills;
    skills_t extra_skills;
    int life;
    int base_hp;
    int exp;
    spell_info spells[_MAX_SPELLS];        /* There is always a sentinel at the end */
    ang_spell special_blast;
} _pact_t, *_pact_ptr;

/****************************************************************
 * Warlock Pact: Undead
 ****************************************************************/
static void _undead_calc_bonuses(void)
{
    plr->align -= 200;
    res_add(GF_COLD);
    if (plr->lev >= 15)
    {
        plr->see_inv++;
        res_add(GF_POIS);
    }
    if (plr->lev >= 30)
    {
        res_add(GF_NETHER);
        plr->hold_life++;
    }
    if (plr->lev >= 35)
        res_add(GF_DARK);

    if (equip_find_art("~.Death"))
    {
        plr->dec_mana++;
        plr->easy_spell++;
    }
}

static void _undead_calc_stats(s16b stats[MAX_STATS])
{
    stats[A_WIS] -= 3 * plr->lev/50;
    stats[A_CON] += 3 * plr->lev/50;
}

static void _undead_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_RES_(GF_COLD));
    if (plr->lev >= 15)
    {
        add_flag(flgs, OF_RES_(GF_POIS));
        add_flag(flgs, OF_SEE_INVIS);
    }
    if (plr->lev >= 30)
    {
        add_flag(flgs, OF_RES_(GF_NETHER));
        add_flag(flgs, OF_HOLD_LIFE);
    }
    if (plr->lev >= 35)
        add_flag(flgs, OF_RES_(GF_DARK));
}

static void _draining_blast(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Blast: Draining");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires an Eldritch Blast which also does Drain Life.");
        break;
    default:
        _blast_ball_aux(cmd, res, 0, GF_ELDRITCH_DRAIN, _blast_dice(1000), _blast_range());
    }
}

static _pact_t _undead_pact = {
  "Undead",
  "Undead are tough creatures, each having survived death at least once. Warlocks who make a pact "
      "with Undead will find themselves gaining additional resistances such as cold, poison, darkness and "
      "nether. In addition, they will be able to see their kind, which tend to be invisible, and gain "
      "resistance to life draining forces. They will also gain access to undeadly powers, which typically "
      "involve detection and control over the forces of the netherworld. In the end, this pact will allow "
      "its practitioners to destroy monsters with a single word, and, perhaps, to even partially leave "
      "the world of the living altogether. Of course, allying with the undead substantially reduces the "
      "warlock's ability to fight these creatures.",
  "", /* RF3_UNDEAD suffices */
  _undead_calc_bonuses,
  NULL,
  _undead_calc_stats,
  _undead_get_flags,
  NULL,
  NULL,
/*  S   I   W   D   C   C */
  {-1,  2, -3,  0,  2,  4},
/* Dsrm Dvce Save Stlh Srch Prcp Thn Thb*/
  {  20,  40,  40,   4,  16,  20, 48, 35},
  {  40,  75,  60,   0,   0,   0, 65, 55},
/*Life  BaseHP     Exp */
   107,     15,    135,
  {
    {  1,   1, 30, detect_life_spell},
    {  3,   2, 30, detect_unlife_spell},
    {  5,   3, 30, scare_spell},
    {  7,   5, 50, nether_bolt_spell},
    { 10,   7, 60, satisfy_hunger_spell},
    { 15,  10, 60, animate_dead_spell},
    { 20,  20, 60, restore_life_spell},
    { 22,  10, 50, enslave_undead_spell},
    { 25,  15, 40, nether_ball_spell},
    { 32,  40, 60, summon_undead_spell},
    { 35,  30, 70, darkness_storm_I_spell},
    { 37,  50, 80, polymorph_vampire_spell},
    { 40,  70, 80, summon_hi_undead_spell},
    { 42,  50, 80, genocide_spell},
    { 50, 100, 80, wraithform_spell},
    { -1,   0,  0, NULL },
  },
  _draining_blast
};

/****************************************************************
 * Warlock Pact: Dragons
 * The main bonus of this pact is that they are Dragonriders!
 ****************************************************************/
static monster_type *_get_mount(void)
{
    monster_type *result = NULL;
    if (plr->riding)
        result = dun_mon(cave, plr->riding);
    return result;
}

static bool _is_lance(object_type *o_ptr)
{
    return object_is_(o_ptr, TV_POLEARM, SV_LANCE)
        || object_is_(o_ptr, TV_POLEARM, SV_HEAVY_LANCE);
}

static void _dragon_calc_bonuses(void)
{
    res_add(GF_FEAR);
    if (plr->lev >= 30)
        plr->sustain_str = TRUE;
}

static status_display_t _dragon_status_display(void)
{
    status_display_t result = {0};
    switch (_get_toggle())
    {
    case WARLOCK_DRAGON_TOGGLE_CANTER:
        result.name = "Canter"; result.abbrev = "Ctr", result.color = TERM_L_BLUE;
        break;
    case WARLOCK_DRAGON_TOGGLE_GALLOP:
        result.name = "Gallop"; result.abbrev = "Glp", result.color = TERM_RED;
        break;
    case WARLOCK_DRAGON_TOGGLE_HEALING:
        result.name = "Healing"; result.abbrev = "Hl", result.color = TERM_YELLOW;
        break;
    case WARLOCK_DRAGON_TOGGLE_HEROIC_CHARGE:
        result.name = "Heroic Charge"; result.abbrev = "Chg", result.color = TERM_VIOLET;
        break;
    }
    return result;
}

static void _dragon_calc_weapon_bonuses(obj_ptr obj, plr_attack_info_ptr info)
{
    if ( _get_toggle() == WARLOCK_DRAGON_TOGGLE_HEROIC_CHARGE
      && plr->riding
      && _is_lance(obj) )
    {
        info->to_dd += 2;
    }
}

static void _dragon_calc_stats(s16b stats[MAX_STATS])
{
    stats[A_STR] += 3 * plr->lev/50;
}

static void _dragon_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_RES_(GF_FEAR));
    if (plr->lev >= 30)
        add_flag(flgs, OF_SUST_STR);
}

static void _dragon_blast(int cmd, var_ptr res)
{
    dice_t dice = spell_dam_dice(_blast_dd(), _blast_ds(), 0);
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Blast: Dragon");
        break;
    case SPELL_DESC:
        var_set_string(res, "Breathes your eldritch blast at a chosen foe.");
        break;
    case SPELL_CAST:
        var_set_bool(res,
            plr_cast_breath_aux(1 + plr->lev/20, GF_ELDRITCH, dice, _blast_range()));
        /* XXX PROJECT_FULL_DAM */
        break;
    default:
        default_spell(cmd, res);
    }
}

/* Dragon Spells */
static void _dragon_lore_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Dragon's Lore");
        break;
    default:
        identify_spell(cmd, res);
        break;
    }
}

static void _dragon_eye_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Dragon's Eye");
        break;
    default:
        telepathy_spell(cmd, res);
        break;
    }
}

static void _understanding_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Understanding");
        break;
    default:
        probing_spell(cmd, res);
        break;
    }
}

static void _word_of_command_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Word of Command");
        break;
    case SPELL_DESC:
        var_set_string(res, "By uttering a word of obediance, the true dragon rider can bend the will of all but the mightiest serpents.");
        break;
    case SPELL_CAST:                        /*v-- This is meaningless, but set high enough so that the project code actually works */
        plr_project_los(GF_CONTROL_PACT_MONSTER, 100);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

/* Dragon Songs */
static void _dragon_upkeep_song(void)
{
    int           cost = 0;
    monster_type *mount = _get_mount();

    /* Check if the player got thrown, dismounted, or their steed was slain */
    if (!mount)
    {
        _set_toggle(TOGGLE_NONE);
        return;
    }

    switch (_get_toggle())
    {
    case WARLOCK_DRAGON_TOGGLE_BLESS:
        cost = 1;
        break;
    case WARLOCK_DRAGON_TOGGLE_CANTER:
        cost = 2;
        break;
    case WARLOCK_DRAGON_TOGGLE_GALLOP:
        cost = 5;
        break;
    case WARLOCK_DRAGON_TOGGLE_HEALING:
        cost = plr->lev/2;
        break;
    case WARLOCK_DRAGON_TOGGLE_HEROIC_CHARGE:
        cost = plr->lev/2;
        break;
    }
    if (cost > plr->csp)
    {
        msg_print("You can no longer maintain the song.");
        _set_toggle(TOGGLE_NONE);
    }
    else
    {
        sp_player(-cost);

        /* Apply the Song. Most songs are handled in calc_bonuses(), or
           perhaps scattered throughout the code base as 'hacks' */
        if (_get_toggle() == WARLOCK_DRAGON_TOGGLE_HEALING)
        {
            hp_player(plr->lev);
            if (mount->hp < mount->maxhp)
            {
                int heal = MIN(plr->lev*3, mount->maxhp - mount->hp);
                mount->hp += heal;
            }
        }
        else if (_get_toggle() == WARLOCK_DRAGON_TOGGLE_HEROIC_CHARGE)
        {
            mon_tim_remove(mount, T_STUN);
            mon_tim_remove(mount, T_CONFUSED);
            mon_tim_remove(mount, T_FEAR);
        }
    }
}

static void _dragon_song(int which, cptr desc, int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_CAST:
        var_set_bool(res, FALSE);
        if (!_get_mount())
        {
            msg_print("You may only sing when riding.");
            return;
        }
        if (_get_toggle() == which)
        {
            msg_format("You stop singing %s.", desc);
            _set_toggle(TOGGLE_NONE);
        }
        else
        {
            msg_format("You begin singing %s.", desc);
            _set_toggle(which);
        }
        var_set_bool(res, TRUE);
        break;
    case SPELL_ENERGY:
        if (_get_toggle() != which)
            var_set_int(res, 0);    /* no charge for dismissing a song */
        else
            var_set_int(res, 100);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}


static void _bless_song(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Song of Blessing");
        break;
    case SPELL_DESC:
        var_set_string(res, "Adagio. When sung, both you and your steed will gain enhanced melee skill.");
        break;
    default:
        _dragon_song(WARLOCK_DRAGON_TOGGLE_BLESS, "of heoic deeds", cmd, res);
        break;
    }
}

static void _canter_song(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Joyful Song");
        break;
    case SPELL_DESC:
        var_set_string(res, "Allegro. This is a pleasing melody, and your steed will prance along in time with the song.");
        break;
    default:
        _dragon_song(WARLOCK_DRAGON_TOGGLE_CANTER, "a joyful, upbeat melody", cmd, res);
        break;
    }
}

static void _gallop_song(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Song of Haste");
        break;
    case SPELL_DESC:
        var_set_string(res, "Presto. A quick beat marks this song of urgency.");
        break;
    default:
        _dragon_song(WARLOCK_DRAGON_TOGGLE_GALLOP, "a spurring melody", cmd, res);
        break;
    }
}

static void _healing_song(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Song of Life");
        break;
    case SPELL_DESC:
        var_set_string(res, "Largo. Slowly and majestically, both you and your mount feel the life giving effects of this powerful ballad.");
        break;
    default:
        _dragon_song(WARLOCK_DRAGON_TOGGLE_HEALING, "a rejuvenating melody", cmd, res);
        break;
    }
}

static void _heroic_charge_song(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Song of War");
        break;
    case SPELL_DESC:
        var_set_string(res, "Molto allegro. A song of defiance, its melody captures the essence of "
                            "the dragon's lust for treasure and conquest. Woe be to all that hear "
                            "this ancient dragon song.");
        break;
    default:
        _dragon_song(WARLOCK_DRAGON_TOGGLE_HEROIC_CHARGE, "of battle and riches", cmd, res);
        break;
    }
}

/* Riding Techniques */
static void _mount_jump_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Jump");
        break;
    case SPELL_CAST:
        if (!plr->riding)
        {
            msg_print("This is a riding technique. Where is your dragon?");
            var_set_bool(res, FALSE);
            return;
        }
    default:
        jump_spell(cmd, res);
        break;
    }
}

static void _mount_attack_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Guided Attack");
        break;
    case SPELL_DESC:
        var_set_string(res, "Guide your dragon to attack a chosen foe.");
        break;
    case SPELL_CAST: {
        mon_ptr mount = plr_riding_mon();
        mon_ptr foe;

        var_set_bool(res, FALSE);
        if (!mount)
        {
            msg_print("This is a riding technique. Where is your dragon?");
            return;
        }
        foe = plr_target_adjacent_mon();
        if (!foe) return;

        if (mount->energy_need > 300)
        {
            msg_print("You sense your dragon is too tired for another attack.");
            return;
        }

        mon_attack(mount, foe->pos);
        mount->energy_need += ENERGY_NEED();
        var_set_bool(res, TRUE);
        break; }
    default:
        default_spell(cmd, res);
        break;
    }
}

static int _hack_dir;
static bool _dragonrider_ai(mon_spell_cast_ptr cast)
{
    mon_spells_ptr spells = cast->race->spells;
    mon_spell_group_ptr group;

    /* steel dragons? */
    if (!spells) return FALSE; 
    if (!spells->groups[MST_BREATH]) return FALSE;

    if (_hack_dir == 5)
    {
        cast->dest = who_pos(plr->target);
        if (who_is_mon(plr->target))
        {
            char tmp[MAX_NLEN];
            cast->mon2 = who_mon(plr->target);
            monster_desc(tmp, cast->mon2, 0);
            tmp[0] = toupper(tmp[0]);
            sprintf(cast->name2, "<color:o>%s</color>", tmp);
        }
    }
    else
    {
        cast->dest.x = plr->pos.x + 99 * ddx[_hack_dir];
        cast->dest.y = plr->pos.y + 99 * ddy[_hack_dir];
    }

    if (!cast->mon2)
        strcpy(cast->name2, "<color:o>the ground</color>");

    group = spells->groups[MST_BREATH];
    cast->spell = &group->spells[randint0(group->count)];
    return TRUE;
}

static void _mount_breathe_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Guided Breath");
        break;
    case SPELL_DESC:
        var_set_string(res, "Guide your dragon to breathe at a chosen foe.");
        break;
    case SPELL_CAST:
    {
        monster_type *mount = _get_mount();

        var_set_bool(res, FALSE);
        if (!mount)
        {
            msg_print("This is a riding technique. Where is your dragon?");
            return;
        }

        if (mount->energy_need > 300)
        {
            msg_print("You sense your dragon is too tired for another attack.");
            return;
        }

        if (!get_fire_dir(&_hack_dir)) return;

        if (mon_spell_cast_mon(mount, _dragonrider_ai))
            mount->energy_need += ENERGY_NEED();

        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _pets_breathe_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Dragons' Fury");
        break;
    case SPELL_DESC:
        var_set_string(res, "Guide all of your pet dragon's to breathe at a chosen target.");
        break;
    case SPELL_CAST: {
        mon_ptr mount = _get_mount();
        vec_ptr pets;
        int     i;

        var_set_bool(res, FALSE);
        if (!mount)
        {
            msg_print("This is a riding technique. Where is your dragon?");
            return;
        }

        if (mount->energy_need > 300)
        {
            msg_print("You sense your dragon is too tired for another attack.");
            return;
        }

        if (!get_fire_dir(&_hack_dir)) return;

        pets = plr_pets_for_dismiss(); /* since we might delete_monster */
        msg_print("<color:v>Dragons: As One!!</color>");
        msg_boundary();

        if (mon_spell_cast_mon(mount, _dragonrider_ai))
        {
            mount->energy_need += ENERGY_NEED();
            msg_boundary();
        }

        for (i = 0; i < vec_length(pets); i++)
        {
            mon_ptr pet = vec_get(pets, i);

            if (!mon_is_valid(pet)) continue;
            if (pet == mount) continue;
            if (!mon_is_dragon(pet)) continue;

            if (mon_spell_cast_mon(pet, _dragonrider_ai))
            {
                pet->energy_need += ENERGY_NEED();
                if (one_in_(2))
                {
                    if (mon_show_msg(pet))
                    {
                        char m_name[MAX_NLEN];
                        monster_desc(m_name, pet, 0);
                        msg_format("%^s disappears!", m_name);
                    }
                    delete_monster(pet);
                }
                msg_boundary();
            }
        }
        vec_free(pets);
        var_set_bool(res, TRUE);
        break; }
    default:
        default_spell(cmd, res);
        break;
    }
}

static _pact_t _dragons_pact = {
  "Dragons",
  "The bond between a Dragonrider and a Dragon is one of the strongest and most enduring ties "
  "seen the world over. Each draws strength, power, and encouragement from the other, and together "
  "they make a formidable foe. An alliance with Dragonkind enables this bond to form, and the Warlock "
  "of this pact gains impressive powers for mounted combat. Indeed, they are among the elite riders "
  "of the world, only slightly inferior to Cavalry and Beastmasters. But where they have a slight "
  "deficiency in skill (and perhaps in melee as well), they have a strong advantage in riding techniques "
  "and magical enhancements. Dragonriders favor the lance above all other weapons and seek continuously "
  "for the lengendary 'Dragonlance' which they view as their long lost birthright.",
  "", /* RF3_DRAGON suffices */
  _dragon_calc_bonuses,
  _dragon_calc_weapon_bonuses,
  _dragon_calc_stats,
  _dragon_get_flags,
  _dragon_upkeep_song,
  _dragon_status_display,
/*  S   I   W   D   C   C */
  { 2,  0,  0, -1,  1,  3},
/* Dsrm Dvce Save Stlh Srch Prcp Thn Thb*/
  {  20,  25,  30,   1,  14,  12, 52, 35},
  {  35,  55,  50,   0,   0,   0, 70, 55},
/*Life  BaseHP     Exp */
   102,     10,    130,
  {
    {  5,   3, 40, detect_menace_spell},
    {  7,   5, 40, detect_objects_spell},
    { 15,  15, 60, _dragon_lore_spell},
    { 20,  40, 70, summon_dragon_spell},
    { 23,  10, 50, _mount_attack_spell},
    { 25,   0,  0, _bless_song},
    { 27,  15, 60, _understanding_spell},
    { 29,   0, 60, _mount_jump_spell},
    { 30,   0,  0, _canter_song},
    { 32,  20, 50, _dragon_eye_spell},
    { 35,  30, 60, _mount_breathe_spell},
    { 37,   0,  0, _gallop_song},
    { 40,  90, 80, _word_of_command_spell},
    { 42,   0,  0, _healing_song},
    { 45,   0,  0, _heroic_charge_song},
    { 50, 100, 70, _pets_breathe_spell},
    { -1,   0,  0, NULL },
  },
  _dragon_blast
};

/****************************************************************
 * Warlock Pact: Angels
 ****************************************************************/
static void _angel_calc_bonuses(void)
{
    plr->align += 200;
    plr->levitation = TRUE;
    if (plr->lev >= 15)
        plr->see_inv++;
    if (plr->lev >= 35)
        plr->reflect = TRUE;

    if (equip_find_art("~.Crusade") || equip_find_art("~.Life"))
    {
        plr->dec_mana++;
        plr->easy_spell++;
    }
}

static void _angel_calc_stats(s16b stats[MAX_STATS])
{
    stats[A_WIS] += 3 * plr->lev/50;
}

static void _angel_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_LEVITATION);
    if (plr->lev >= 15)
        add_flag(flgs, OF_SEE_INVIS);
    if (plr->lev >= 35)
        add_flag(flgs, OF_REFLECT);
}

static void _dispelling_blast(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Blast: Dispelling");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires an Eldritch Blast which also does Dispel Magic.");
        break;
    default:
        _blast_ball_aux(cmd, res, 0, GF_ELDRITCH_DISPEL, _blast_dice(1000), _blast_range());
    }
}

static _pact_t _angels_pact = {
  "Angels",
  "Angels are heavenly beings who use a variety of techniques to smite those they view as evil. "
    "Warlocks who make pacts with Angels will also find their saving throws significantly improved, "
    "and (eventually) their body immune to bolt-like effects. Since Angels are strongly aligned with "
    "the forces of good, making a pact with Angels will reduce damage done to all good monsters "
    "by a substantial amount.",
  "A", /* + RF3_GOOD ... They are even allied with Fallen Angels! */
  _angel_calc_bonuses,
  NULL,
  _angel_calc_stats,
  _angel_get_flags,
  NULL,
  NULL,
/*  S   I   W   D   C   C */
  { 1,  1,  2,  1,  1,  3},
/* Dsrm Dvce Save Stlh Srch Prcp Thn Thb*/
  {  20,  35,  40,   1,  16,   8, 48, 35},
  {  35,  55,  75,   0,   0,   0, 65, 55},
/*Life  BaseHP     Exp */
    98,      4,    150,
  {
    {  2,  2, 30, bless_spell},
    {  5,  5, 40, light_area_spell},
    {  7,  4, 30, detect_monsters_spell},
    { 10,  7, 40, heroism_spell},
    { 20, 20, 60, remove_curse_I_spell},
    { 25, 15, 60, earthquake_spell},
    { 27, 20, 50, protection_from_evil_spell},
    { 29, 20, 60, dispel_evil_spell},
    { 33, 30, 70, starburst_I_spell},
    { 37, 20, 60, healing_I_spell},
    { 42, 70, 60, destruction_spell},
    { 47,100, 90, summon_angel_spell},
    { 50,120, 90, crusade_spell},
    { -1,   0,  0, NULL },
  },
  _dispelling_blast
};

/****************************************************************
 * Warlock Pact: Demons
 ****************************************************************/
static void _demon_calc_bonuses(void)
{
    res_add(GF_FIRE);
    plr->device_power += 3 * plr->lev/50;
    if (plr->lev >= 15)
        plr->hold_life++;
    if (plr->lev >= 30)
        plr->no_eldritch = TRUE;
    if (plr->lev >= 40)
        plr->no_charge_drain = TRUE;
    if (plr->lev >= 45)
        plr->kill_wall = TRUE;
    if (plr->lev >= 50)
        res_add_immune(GF_FIRE);

    if (equip_find_art("~.Daemon"))
    {
        plr->dec_mana++;
        plr->easy_spell++;
    }
}

static void _demon_calc_stats(s16b stats[MAX_STATS])
{
    stats[A_INT] += 3 * plr->lev/50;
}

static void _demon_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_RES_(GF_FIRE));
    if (plr->lev >= 10) add_flag(flgs, OF_DEVICE_POWER);
    if (plr->lev >= 15) add_flag(flgs, OF_HOLD_LIFE);
    if (plr->lev >= 50) add_flag(flgs, OF_IM_(GF_FIRE));
}

static void _vengeful_blast(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Blast: Vengeful");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires an extremely deadly Eldritch Blast, but you also take damage.");
        break;
    default:
        _blast_ball_aux(cmd, res, 0, GF_ELDRITCH, _blast_dice(2000), _blast_range());
        if (cmd == SPELL_CAST && var_get_bool(res))
            take_hit(DAMAGE_USELIFE, 100, "vengeful blast");
    }
}

static _pact_t _demons_pact = {
  "Demons",
  "Demons are crafty creatures of the netherworld, using whatever means at their disposal to bring "
    "down their enemies. Warlocks who make pacts with Demons will find their abilities to use "
    "all magical devices improved, and gain the ability to Recharge these devices at will. "
    "Eventually, Demon Warlocks attain the ability to crush walls beneath their footsteps. "
    "Making a pact with Demons will reduce damage done to all demons by a substantial amount.",
  "", /* RF3_DEMON is sufficient */
  _demon_calc_bonuses,
  NULL,
  _demon_calc_stats,
  _demon_get_flags,
  NULL,
  NULL,
/*  S   I   W   D   C   C */
  { 3,  1,-10,  1,  1,  3},
/* Dsrm Dvce Save Stlh Srch Prcp Thn Thb*/
  {  20,  40,  40,   0,  12,   2, 64, 35},
  {  35,  75,  60,   0,   0,   0, 90, 55},
/*Life  BaseHP     Exp */
   100,     15,    150,
  {
    {  3,   2, 25, evil_bless_spell},
    {  6,   5, 30, resist_fire_spell},
    { 11,   9, 35, summon_manes_spell},
    { 20,  10, 50, teleport_spell},
    { 30,  25, 60, recharging_spell},
    { 37,  40, 80, kiss_of_succubus_spell},
    { 40,  50, 80, polymorph_demon_spell},
    { 43,  90, 90, summon_greater_demon_spell},
    { 45,  80, 85, hellfire_spell},
    { -1,   0,  0, NULL },
  },
  _vengeful_blast
};

/****************************************************************
 * Warlock Pact: Hounds
 ****************************************************************/
static void _hound_calc_bonuses(void)
{
    plr->skill_dig += 60;
    plr->pspeed += 5 * plr->lev / 50;

    if (plr->lev >= 5)
        res_add(GF_FIRE);
    if (plr->lev >= 10)
        res_add(GF_COLD);
    if (plr->lev >= 15)
        res_add(GF_ELEC);
    if (plr->lev >= 20)
        res_add(GF_ACID);
    if (plr->lev >= 35)
        res_add(GF_POIS);
}

static void _hound_calc_stats(s16b stats[MAX_STATS])
{
    stats[A_DEX] += 3 * plr->lev/50;
}

static void _hound_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    if (plr->lev >= 10)
        add_flag(flgs, OF_SPEED);

    if (plr->lev >= 5)
        add_flag(flgs, OF_RES_(GF_FIRE));
    if (plr->lev >= 10)
        add_flag(flgs, OF_RES_(GF_COLD));
    if (plr->lev >= 15)
        add_flag(flgs, OF_RES_(GF_ELEC));
    if (plr->lev >= 20)
        add_flag(flgs, OF_RES_(GF_ACID));
    if (plr->lev >= 35)
        add_flag(flgs, OF_RES_(GF_POIS));
}

#define _AETHER_EFFECT_CT 15
static void _aether_blast(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Blast: Aether");
        break;
    case SPELL_DESC:
        var_set_string(res, "You channel the aether to unleash a random number of random effects via your Eldritch Blast.");
        break;
    case SPELL_INFO:
        var_set_string(res, dice_info_dam_each(_blast_dice(500)));
        break;
    case SPELL_CAST:
    {
        int i;
        int ct = rand_range(3, 7);
        dice_t dice = _blast_dice(500);
        int effects[_AETHER_EFFECT_CT] =
               {GF_ACID,  GF_ELEC,   GF_FIRE,      GF_COLD,       GF_POIS,
                GF_LIGHT,  GF_DARK,   GF_CONFUSION, GF_NETHER,     GF_NEXUS,
                GF_SOUND, GF_SHARDS, GF_CHAOS,     GF_DISENCHANT, GF_TIME};
        int resists[_AETHER_EFFECT_CT] =
               {GF_ACID, GF_ELEC,  GF_FIRE,     GF_COLD,      GF_POIS,
                GF_LIGHT, GF_DARK,  GF_CONF,     GF_NETHER,    GF_NEXUS,
                GF_SOUND,GF_SHARDS,GF_CHAOS,    GF_DISEN,     GF_TIME};
        point_t pos;

        var_set_bool(res, FALSE);
        pos = plr_get_ball_target_aux(GF_ELDRITCH, _blast_range());
        if (!dun_pos_interior(cave, pos)) return;

        for (i = 0; i < ct; i++)
        {
            int idx = randint0(_AETHER_EFFECT_CT);
            int effect = effects[idx];
            int resist = resists[idx];
            int dam = dice_roll(dice);

            msg_format("You channel <color:%c>%s</color>.",
                attr_to_attr_char(res_color(resist)),
                res_name(resist)
            );
            plr_ball(0, pos, effect, dam);
            msg_boundary();
        }

        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
    }
}

static void _dog_whistle_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Dog Whistle");
        break;
    case SPELL_DESC:
        var_set_string(res, "By emitting a shrill whistle, unaudible to most, you attempt to control nearby canines.");
        break;
    case SPELL_CAST:
        plr_project_los(GF_CONTROL_PACT_MONSTER, 1000);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _aether_shield_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Aether Shield");
        break;
    case SPELL_DESC:
        var_set_string(res, "Much like the dreaded Aether hound, you will gain protective elemental auras for a bit.");
        break;
    case SPELL_CAST:
        plr_tim_add(T_AURA_FIRE, randint1(30) + 20);
        if (plr->lev >= 25)
            plr_tim_add(T_AURA_COLD, randint1(30) + 20);
        if (plr->lev >= 35)
            plr_tim_add(T_AURA_ELEC, randint1(30) + 20);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static _pact_t _hounds_pact = {
  "Hounds",
  "An alliance with hounds is one of the weaker pacts the warlock may make. Hounds are fast, stealthy "
  "and nimble, and the warlock will gain these attributes as well, in addition to basic elemental "
  "resistances. They also gain decent melee, but other skills, especially device skills, are somewhat "
  "lacking. Of course, they may call on their kin for assistance, but this may not be enough against "
  "the more difficult foes of the world. Still, hounds are very common and many consider them to be "
  "annoying as well, so perhaps an alliance is in order?",
  "ZC",
  _hound_calc_bonuses,
  NULL,
  _hound_calc_stats,
  _hound_get_flags,
  NULL,
  NULL,
/*  S   I   W   D   C   C */
  { 0, -2, -2,  2,  2,  2},
/* Dsrm Dvce Save Stlh Srch Prcp  Thn Thb*/
  {  20,  25,  31,   5,  20,  15,  56, 25},
  {  35,  50,  50,   0,   0,   0, 100, 55},
/*Life  BaseHP     Exp */
   102,     12,    110,
  {
    {  1,   1, 30, hound_sniff_spell},
    { 20,  20, 60, summon_hounds_spell},
    { 27,  20, 50, haste_self_spell},
    { 30,  20, 50, resistance_spell},
    { 32,  30, 70, _dog_whistle_spell},
    { 40,  20, 60, _aether_shield_spell},
    { -1,   0,  0, NULL },
  },
  _aether_blast
};

/****************************************************************
 * Warlock Pact: Spiders
 ****************************************************************/
static void _spider_calc_bonuses(void)
{
    plr->pspeed += 7 * plr->lev / 50;

    if (plr->lev >= 10)
        res_add(GF_POIS);
    if (plr->lev >= 20)
        res_add(GF_NEXUS);
    if (plr->lev >= 30)
        res_add(GF_TELEPORT);

    plr->pass_web = TRUE;
}

static void _spider_calc_stats(s16b stats[MAX_STATS])
{
    stats[A_DEX] += 3 * plr->lev/50;
}

static void _spider_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    if (plr->lev >= 8)
        add_flag(flgs, OF_SPEED);

    if (plr->lev >= 10)
        add_flag(flgs, OF_RES_(GF_POIS));
    if (plr->lev >= 20)
        add_flag(flgs, OF_RES_(GF_NEXUS));
}

static void _phase_blast(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Blast: Phasing");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fire an Eldritch Blast and then jump to safety in a single move.");
        break;
    default:
        _blast_ball_aux(cmd, res, 0, GF_ELDRITCH, _blast_dice(1000), _blast_range());
        if (cmd == SPELL_CAST && var_get_bool(res))
            teleport_player(25, 0);
    }
}

static void _nexus_ball_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Nexus Ball");
        break;
    case SPELL_DESC:
        var_set_string(res, "Generates a ball of nexus on chosen target.");
        break;
    default:
        ball_spell(cmd, res, 2, GF_NEXUS, 30 + plr_prorata_level(70));
    }
}

/* Spider Jumps*/
static int _jump_rad(void) { return 2 + plr->lev/10; }

static int _poison_jump_dam(void) { return spell_power(plr->lev + plr->to_d_spell); }
static void _poison_jump_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Poison Jump");
        break;
    case SPELL_DESC:
        var_set_string(res, "Generates a ball of poison as you jump to safety.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_damage(0, 0, _poison_jump_dam()));
        break;
    case SPELL_CAST:
        plr_burst(_jump_rad(), GF_POIS, _poison_jump_dam());
        teleport_player(30, 0);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
    }
}

static int _nexus_jump_dam(void) { return spell_power(plr->lev + 5 + plr->to_d_spell); }
static void _nexus_jump_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Nexus Jump");
        break;
    case SPELL_DESC:
        var_set_string(res, "Generates a ball of nexus as you jump to safety.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_damage(0, 0, _nexus_jump_dam()));
        break;
    case SPELL_CAST:
        plr_burst(_jump_rad(), GF_NEXUS, _nexus_jump_dam());
        teleport_player(30, 0);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
    }
}

static int _greater_nexus_jump_dam(void) { return spell_power(plr_prorata_level_aux(200, 0, 0, 1) + plr->to_d_spell); }
static void _greater_nexus_jump_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Greater Nexus Jump");
        break;
    case SPELL_DESC:
        var_set_string(res, "Generates a huge ball of nexus as you jump to safety.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_damage(0, 0, _greater_nexus_jump_dam()));
        break;
    case SPELL_CAST:
        plr_burst(_jump_rad() + 3, GF_NEXUS, _greater_nexus_jump_dam());
        teleport_player(30, 0);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
    }
}

static _pact_t _spiders_pact = {
  "Spiders",
  "Of all the icky, crawling things in the world, you have chosen to make an alliance "
    "with spiders? Sure, they are stealthy, and fast, and very adept at hiding. Able "
    "to extrude a fine sticky substance to ensnare the unwary, the spider lurks nearby, "
    "or so you assume since you very seldom see them. Moreover, many spiders are adept at "
    "the powers of teleportation, able to jump quickly from one location to another, which "
    "can be quite baffling to a would be predator. They are comfortable with both poison "
    "and nexus. But, aside from all of this, they are a bit on the squishy side. That is, "
    "if you can manage to catch one!",
  "S",
  _spider_calc_bonuses,
  NULL,
  _spider_calc_stats,
  _spider_get_flags,
  NULL,
  NULL,
/*  S   I   W   D   C   C */
  {-1,  0, -2,  2,  0,  2},
/* Dsrm Dvce Save Stlh Srch Prcp Thn Thb*/
  {  20,  37,  31,   7,  12,   2, 56, 60},
  {  35,  55,  50,   0,   0,   0, 60, 75},
/*Life  BaseHP     Exp */
    97,      1,    120,
  {
    {  1,   1, 30, phase_door_spell},
    {  3,   3, 30, stinking_cloud_spell},
    {  5,   4, 30, detect_monsters_spell},
    {  7,   5, 30, teleport_spell},
    { 15,   7, 60, summon_spiders_spell},
    { 17,   8, 50, _nexus_ball_spell},
    { 22,   9, 50, _poison_jump_spell},
    { 29,  12, 50, _nexus_jump_spell},
    { 30,  10, 60, spider_web_spell},
    { 35,  20, 60, dimension_door_spell},
    { 42,  40, 70, _greater_nexus_jump_spell},
    { -1,   0,  0, NULL },
  },
  _phase_blast
};

/****************************************************************
 * Warlock Pact: Giants
 ****************************************************************/
static void _giant_calc_bonuses(void)
{
    if (plr->lev >= 30)
        res_add(GF_SOUND);
    if (plr->lev >= 40)
        res_add(GF_SHARDS);
    if (plr->lev >= 50)
        res_add(GF_CHAOS);
    plr->skill_tht += 2*plr->lev;
}

static void _giant_calc_weapon_bonuses(obj_ptr obj, plr_attack_info_ptr info)
{
    if (obj->weight >= 200)
    {
        int to_h = 10 * plr->lev / 50;
        int to_d = 10 * plr->lev / 50;

        info->to_h += to_h;
        info->dis_to_h += to_h;

        info->to_d += to_d;
        info->dis_to_d += to_d;
    }
}

static void _giant_calc_stats(s16b stats[MAX_STATS])
{
    stats[A_STR] += 5 * plr->lev/50;
    stats[A_CON] += 3 * plr->lev/50;
}

static void _giant_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    if (plr->lev >= 30)
        add_flag(flgs, OF_RES_(GF_SOUND));
    if (plr->lev >= 40)
        add_flag(flgs, OF_RES_(GF_SHARDS));
    if (plr->lev >= 50)
        add_flag(flgs, OF_RES_(GF_CHAOS));
}

static void _confusing_blast(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Blast: Confusing");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires an Eldritch Blast that also confuses your opponent.");
        break;
    default:
        _blast_ball_aux(cmd, res, 0, GF_ELDRITCH_CONFUSE, _blast_dice(1000), _blast_range());
    }
}

static void _giant_healing_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Giant's Healing");
        break;
    case SPELL_DESC:
        var_set_string(res, "All powerful giants can heal themselves, right? Why not you?");
        break;
    case SPELL_INFO:
        var_set_string(res, format("Heals %d", spell_power(plr->lev * 4)));
        break;
    case SPELL_CAST:
        hp_player(spell_power(plr->lev * 4));
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static _pact_t _giants_pact = {
  "Giants",
  "An alliance with giants and titans grants impressive physical strength and fortitude, "
    "but other stats will generally suffer. Later in life, you will gain giant-like resistances "
    "including sound, shards and chaos. Your combat will be impressive, but, like giants, you "
    "will favor heavy weapons. Oh, and you won't mind tossing a boulder or two should the need "
    "arise. Since this alliance offers so much in the way of physical advantages, it offers very "
    "little in the way of magical abilities or skill. Warlocks with this alliance are the best "
    "possible fighters being nearly indistiguishable in skill from Warriors!",
  "", /* RF3_GIANT will suffice */
  _giant_calc_bonuses,
  _giant_calc_weapon_bonuses,
  _giant_calc_stats,
  _giant_get_flags,
  NULL,
  NULL,
/*  S   I   W   D   C   C */
  { 2, -4, -4, -3,  2,  2},
/* Dsrm Dvce Save Stlh Srch Prcp  Thn Thb*/
  {  20,  20,  31,   0,  12,   2,  70, 30},
  {  35,  40,  50,   0,   0,   0, 150, 75},
/*Life  BaseHP     Exp */
   112,     25,    140,
  {
    {  5,   0, 50, throw_boulder_spell},
    { 10,   7,  0, stunning_blow_spell},
    { 30,  30,  0, monster_toss_spell},
    { 40,  80, 50, summon_kin_spell},
    { 50,  50, 70, _giant_healing_spell},
    { -1,   0,  0, NULL },
  },
  _confusing_blast
};

/****************************************************************
 * Private Helpers
 ****************************************************************/
static _pact_ptr _get_pact(int which)
{
    switch (which)
    {
    case WARLOCK_UNDEAD:
        return &_undead_pact;
    case WARLOCK_DRAGONS:
        return &_dragons_pact;
    case WARLOCK_ANGELS:
        return &_angels_pact;
    case WARLOCK_DEMONS:
        return &_demons_pact;
    case WARLOCK_HOUNDS:
        return &_hounds_pact;
    case WARLOCK_SPIDERS:
        return &_spiders_pact;
    case WARLOCK_GIANTS:
        return &_giants_pact;
    }
    return NULL;
}

/****************************************************************
 * Powers and Spells
 ****************************************************************/
#define MAX_WARLOCK_BLASTS    7

static spell_info _powers[MAX_WARLOCK_BLASTS] =
{
    /*lvl cst fail spell */
    {  1,  0,  20, _basic_blast},
    { 10,  0,  40, _extended_blast},
    { 18,  0,  45, _spear_blast},
    { 26,  0,  60, _burst_blast},
    { 33,  0,  60, _stunning_blast},
    { 37,  0,  70, NULL},
    { 42,  0,  75, _empowered_blast},
};

static int _get_powers(spell_info* spells, int max)
{
    int       i;
    int       ct = 0;
    int       stat_idx = plr->stat_ind[A_CHR];
    _pact_ptr pact = _get_pact(plr->psubclass);

    assert(pact);

    /* This is debatable. Empowered blast is supposed to block your powers
       for one turn. They used to be spells, but now they are actually class
       powers, so won't be blocked by T_NO_SPELLS ... Unless we do the
       following. Note, this now blocks all pact spells as well! */
    if (plr_tim_find(T_NO_SPELLS))
        return 0;

    for (i = 0; i < MAX_WARLOCK_BLASTS; i++)
    {
        spell_info *base = &_powers[i];
        if (ct >= max) break;
        if (base->level <= plr->lev)
        {
            spell_info* current = &spells[ct];
            current->fn = base->fn;
            current->level = base->level;
            current->cost = base->cost;
            current->fail = calculate_fail_rate(base->level, base->fail, stat_idx);
            if (current->fn == NULL)
                current->fn = pact->special_blast;

            ct++;
        }
    }
    return ct;
}
static int _get_spells(spell_info* spells, int max)
{
    int       i;
    int       ct = 0;
    int       stat_idx = plr->stat_ind[A_CHR];
    _pact_ptr pact = _get_pact(plr->psubclass);

    assert(pact);

    for (i = 0; ; i++)
    {
        spell_info *base = &pact->spells[i];
        if (base->level <= 0) break;
        if (ct >= max) break;
        if (base->level <= plr->lev)
        {
            spell_info* current = &spells[ct++];
            current->fn = base->fn;
            current->level = base->level;
            current->cost = base->cost;
            current->fail = calculate_fail_rate(base->level, base->fail, stat_idx);
        }
    }

    return ct;
}

static void _character_dump(doc_ptr doc)
{
    spell_info spells[MAX_SPELLS];
    int        ct = _get_spells(spells, MAX_SPELLS);

    plr_display_spells(doc, spells, ct);
}

static caster_info * _caster_info(void)
{
    static caster_info me = {0};
    static bool init = FALSE;
    if (!init)
    {
        me.magic_desc = "arcane power";
        me.which_stat = A_CHR;
        me.encumbrance.max_wgt = 450;
        me.encumbrance.enc_wgt = 800;
        me.encumbrance.weapon_pct = 67;
        me.options = CASTER_GAIN_SKILL;

        /* dragon pact wants heavy lances ... */
        if (plr->psubclass == WARLOCK_DRAGONS)
            me.encumbrance.weapon_pct = 33;
        /* giant pact is more of a 'warrior' ... */
        else if (plr->psubclass == WARLOCK_GIANTS)
        {
            me.encumbrance.weapon_pct = 20;
            me.encumbrance.max_wgt = 600;
        }

        init = TRUE;
    }
    return &me;
}

static void _birth(void)
{
    if (plr->psubclass == WARLOCK_GIANTS)
    {
        skills_weapon_init(TV_SWORD, SV_CLAYMORE, WEAPON_EXP_BEGINNER);
        plr_birth_obj_aux(TV_SWORD, SV_CLAYMORE, 1);
    }
    else
        plr_birth_obj_aux(TV_SWORD, SV_SHORT_SWORD, 1);
    plr_birth_obj_aux(TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR, 1);
    plr_birth_obj_aux(TV_POTION, SV_POTION_SPEED, 1);
}

/****************************************************************
 * Public API
 ****************************************************************/
bool warlock_is_pact_monster(monster_race *r_ptr)
{
    if (plr->pclass == CLASS_WARLOCK)
    {
        _pact_ptr pact = _get_pact(plr->psubclass);
        char     *pc = my_strchr(pact->alliance, r_ptr->display.c);

        if (pc != NULL)
            return TRUE;

        switch (plr->psubclass)
        {
        case WARLOCK_UNDEAD:
            if (mon_race_is_undead(r_ptr))
                return TRUE;
            break;

        case WARLOCK_DRAGONS:
            if (mon_race_is_dragon(r_ptr))
                return TRUE;
            break;

        case WARLOCK_ANGELS:
            /* Angel pact is now all good monsters!!! */
            if (mon_race_is_good(r_ptr))
                return TRUE;
            break;

        case WARLOCK_DEMONS:
            if (mon_race_is_demon(r_ptr))
                return TRUE;
            break;

        case WARLOCK_GIANTS:
            if (mon_race_is_giant(r_ptr))
                return TRUE;
            break;
        }
    }

    return FALSE;
}

plr_class_ptr warlock_get_class(int psubclass)
{
    static plr_class_ptr me = NULL;

    if (!me)
    {
        me = plr_class_alloc(CLASS_WARLOCK);
        me->name = "Warlock";
        me->desc =
        "A Warlock is a magical class but, unlike normal spellcasters, they derive their powers, "
        "stats, skills and bonuses by making a pact with a given class of monsters. "
        "This pact is irrevocable and is made "
        "at the outset of the Warlock's career. Depending on the class of monsters with whom they "
        "ally, the warlock will gain unique bonuses, abilities and magical powers. For example, "
        "alliance with the forces of the netherworld grants resistances to poison and nether, enhanced "
        "constitution and spells to control and conjure the unliving as well as spells to access "
        "the damaging forces of the netherworld directly. Each such alliance has thematic bonuses "
        "and powers, and you can read the details in the respective help sections. On final comment: "
        "allying with a given class of monsters dramatically reduces the warlock's ability to fight "
        "these foes. Instead, the warlock seeks cooperation with their brethren, or, perhaps, domination "
        "over them. But direct assaults are rarely successful.\n \n"
        "In addition to pact related spells, all warlocks gain access to the unique power of the "
        "Eldritch Blast. Their primary spell stat is Charisma since they seek dominion and alliance "
        "with their chosen kin, and these monsters tend to have a strong will of their own, resisting "
        "the binding forces which the warlock imposes to gain both mastery and power.";

        me->hooks.birth = _birth;
        me->hooks.caster_info = _caster_info;
        me->hooks.get_spells = _get_spells;
        me->hooks.get_powers = _get_powers;
        me->hooks.character_dump = _character_dump;
        me->flags = CLASS_SENSE1_FAST | CLASS_SENSE1_WEAK |
                    CLASS_SENSE2_MED | CLASS_SENSE2_STRONG;

        me->pets = 15;
    }

    me->stats[A_STR] = 0;
    me->stats[A_INT] = 0;
    me->stats[A_WIS] = 0;
    me->stats[A_DEX] = 0;
    me->stats[A_CON] = 0;
    me->stats[A_CHR] = 0;
    me->life = 100;
    me->base_hp = 0;
    me->exp = 100;

    me->subname = "";
    me->subdesc = "";

    if (0 <= psubclass && psubclass < WARLOCK_MAX)
    {
        int       i;
        _pact_ptr pact = _get_pact(psubclass);

        me->subname = pact->name;
        me->subdesc = pact->desc;

        for (i = 0; i < MAX_STATS; i++)
            me->stats[i] += pact->stats[i];

        me->skills = pact->base_skills;
        me->extra_skills = pact->extra_skills;

        me->life = pact->life;
        me->base_hp = pact->base_hp;
        me->exp = pact->exp;

        me->hooks.calc_bonuses = pact->calc_bonuses;
        me->hooks.calc_weapon_bonuses = pact->calc_weapon_bonuses;
        me->hooks.status_display = pact->status_display;
        me->hooks.calc_stats = pact->calc_stats;
        me->hooks.get_flags = pact->get_flags;
        me->hooks.process_player = pact->process_player;
    }

    return me;
}

