#include "angband.h"

#include <assert.h>
/**********************************************************************
 * Repose of The Dead ... Shared with RACE_MON_VAMPIRE
 **********************************************************************/
static bool _repose_of_the_dead = FALSE;
void repose_of_the_dead_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Repose of the Dead");
        break;
    case SPELL_DESC:
        var_set_string(res, "Sleep the sleep of the dead for a few rounds, during which time nothing can awaken you, except perhaps death. When (if?) you wake up, you will be thoroughly refreshed!");
        break;
    case SPELL_CAST:
        var_set_bool(res, FALSE);
        if (!get_check("You will enter a deep slumber. Are you sure?")) return;
        _repose_of_the_dead = TRUE;
        plr_tim_add(T_PARALYZED, 4 + randint1(4));
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
void repose_timer_on(plr_tim_ptr timer)
{
    if (_repose_of_the_dead && timer->id == T_PARALYZED)
    {
        timer->flags |= TF_NO_MSG;
        msg_print("You enter a deep sleep!");
    }
}
void repose_timer_off(plr_tim_ptr timer)
{
    if (_repose_of_the_dead && timer->id == T_PARALYZED)
    {
        msg_print("You awake refreshed!");
        restore_level();
        plr_restore_life(1000);
        plr_tim_remove(T_POISON);
        plr_tim_remove(T_BLIND);
        plr_tim_remove(T_CONFUSED);
        plr_tim_remove(T_HALLUCINATE);
        plr_tim_remove(T_STUN);
        plr_tim_remove(T_CUT);
        do_res_stat(A_STR);
        do_res_stat(A_CON);
        do_res_stat(A_DEX);
        do_res_stat(A_WIS);
        do_res_stat(A_INT);
        do_res_stat(A_CHR);
        plr_tim_remove(T_BERSERK);
        _repose_of_the_dead = FALSE;
    }
}

/**********************************************************************
 * Foul Necomantic Touches
 * Necromancers can attack directly with the Hand of Vecna (innate attack).
 * Also, the Hand of Vecna increases the damage of touch spells.
 **********************************************************************/
void _calc_innate_bonuses(mon_blow_ptr blow)
{
    if (blow->method == RBM_HAND_OF_VECNA)
    {
        plr->innate_attack_info.blows_calc.wgt = 200;
        plr->innate_attack_info.blows_calc.mul = 30;
        plr->innate_attack_info.blows_calc.max = 300;
        plr_calc_blows_innate(blow);
    }
}
static void _calc_innate_attacks(void) 
{
    if (equip_find_art("].Vecna"))
    {
        mon_race_ptr race = mon_race_parse("L.Vecna");
        mon_blow_ptr blow = mon_blows_find(race->blows, RBM_HAND_OF_VECNA);

        if (blow)
        {
            mon_blow_ptr copy = mon_blow_copy(blow);
            copy->power = 50 + 3*plr->lev; /* compensate for very poor melee skills */
            _calc_innate_bonuses(copy);
            vec_add(plr->innate_blows, copy);
        }
        /* XXX cf _blow_is_masked in plr_attack.c! */
    }
}
static dice_t _calc_dice(int dam, int pct_dice)
{
    dice_t dice = {0};
    int dice_dam = dam * pct_dice; /* scaled by 100 */
    int x; /* scaled by sqrt(100) = 10 */

    x = mysqrt(4*dice_dam);
    dice.dd = MAX(1, (x + 5)/10);
    dice.ds = MAX(2, (x + 5)/17);
    dice.base = MAX(0, dam - dice_avg_roll(dice));
    return dice;
}
static dice_t _necro_dice(int dam)
{
    dice_t dice = _calc_dice(dam, 30);
    /* allow weaponmastery to boost damage (e.g. Rings of Power) */
    dice.dd += plr->innate_attack_info.to_dd;
    dice.ds += plr->innate_attack_info.to_ds;
    return dice;
}
static int _necro_calc_dam(int dam, int xtra)
{
    int d = plr_prorata_level(dam);
    if (equip_find_art("].Vecna")) d = d * 3 / 2;
    d = spell_power(d);
    d += xtra;
    return d;
}
static cptr _necro_info_dam(int dam)
{
    dice_t dice = _necro_dice(dam);
    return info_damage(dice.dd, dice.ds, dice.base);
}
static int _necro_damroll(int dam)
{
    dice_t dice = _necro_dice(dam);
    return dice_roll(dice);
}
static bool _necro_check_touch(void)
{
    int slot;

    if (plr->weapon_ct > 0) /* Wielding a weapon blocks hand based attacks */
    {
        msg_print("You can't touch while wielding a weapon.");
        return FALSE;
    }
    else if (!equip_find_empty_hand() && !mut_present(MUT_DRACONIAN_METAMORPHOSIS))
    {
        msg_print("You need a free hand to touch.");
        return FALSE;
    }

    slot = equip_find_obj(TV_GLOVES, SV_ANY);
    if (slot && !obj_is_specified_art(equip_obj(slot), "].Vecna"))
    {
        msg_print("You can't touch while wielding gloves.");
        return FALSE;
    }
    return TRUE;
}
static bool _necro_do_touch(int type, int dam)
{
    mon_ptr mon;
    char    mon_name[MAX_NLEN_MON];

    if (!_necro_check_touch()) return FALSE;

    mon = plr_target_adjacent_mon();
    if (!mon) return FALSE;
    /* XXX We already had to pass a fear roll to cast a spell ...
    if (!fear_allow_melee(mon))
    {
        msg_print("You are too scared to do that!");
        return TRUE;
    } */

    if (type != GF_DEATH_TOUCH)
        dam = _necro_damroll(dam);
    monster_desc(mon_name, mon, 0);
    cmsg_format(TERM_L_UMBER, "You touch %s.", mon_name); /* note: gf_affect_m should supply flavor! */
    if (plr_touch_mon(mon, type, dam) && type == GF_OLD_DRAIN)
        hp_player(dam);
    return TRUE;
}

/**********************************************************************
 * Summons
 **********************************************************************/
static void _necro_do_summon(int what, int num, bool fail)
{
    point_t pos = plr->pos;
    int     mode = PM_ALLOW_UNIQUE | PM_ALLOW_GROUP;

    if (fail) /* Failing spells should not be insta-death ... */
        num = MAX(1, num/4);
    else
        num = spell_power(num);

    if (!fail && use_old_target && target_okay() && plr_view(who_pos(plr->target)) && !one_in_(3))
        pos = who_pos(plr->target);
    if (trump_summoning(num, !fail, pos, 0, what, mode))
    {
        if (fail)
        {
            if (num == 1)
                msg_print("The summoned monster gets angry!");
            else
                msg_print("The summoned monsters get angry!");
        }
    }
}

/**********************************************************************
 * Spells: Note, we are still using the old "Book Spell System"
 **********************************************************************/
#define _UNDEAD_SIGHT 10  /* pending a realm re-write ... _auto_detect */
#define _UNDEAD_LORE  11  /* _auto_id */
cptr do_necromancy_spell(int spell, int mode)
{
    bool name = (mode == SPELL_NAME) ? TRUE : FALSE;
    bool desc = (mode == SPELL_DESC) ? TRUE : FALSE;
    bool info = (mode == SPELL_INFO) ? TRUE : FALSE;
    bool cast = (mode == SPELL_CAST) ? TRUE : FALSE;
    bool fail = (mode == SPELL_FAIL) ? TRUE : FALSE;

    int plev = plr->lev;
    dice_t dice = {0};

    dice.scale = spell_power(1000);
    dice.scale += virtue_current(VIRTUE_UNLIFE) / 2; /* roughly +/- 6% */
    dice.scale -= virtue_current(VIRTUE_FAITH) / 2;

    /* XXX TODO XXX */

    switch (spell)
    {
    /* Stench of Death */
    case 0:
        if (name) return "Cold Touch";
        if (desc) return "Damage an adjacent monster with a chilling touch.";
       {int dam = _necro_calc_dam(70, 8 + plr->to_d_spell);
        if (info) return _necro_info_dam(dam);
        if (cast && !_necro_do_touch(GF_COLD, dam)) return NULL;}
        break;

    case 1:
        if (name) return "Summon Rat";
        if (desc) return "Summons a rat to feast on the dead!";
        if (cast || fail) _necro_do_summon(SUMMON_RAT, 1, fail);
        break;

    case 2:
        if (name) return "Detect Life";
        if (desc) return "Detects all living monsters in your vicinity.";
        if (info) return info_radius(DETECT_RAD_DEFAULT);
        if (cast) detect_monsters_living(DETECT_RAD_DEFAULT, "You sense the presence of life around you.");
        break;

    case 3:
        if (name) return "Create Darkness";
        if (desc) return "Darkens your surroundings.";
        if (cast) unlite_area(0, 3);
        break;

    case 4:
        if (name) return "Poison Touch";
        if (desc) return "Damage an adjacent monster with a venomous touch.";
       {int dam = _necro_calc_dam(85, 15 + plr->to_d_spell);
        if (info) return _necro_info_dam(dam);
        if (cast && !_necro_do_touch(GF_POIS, dam)) return NULL;}
        break;

    case 5:
        if (name) return "Summon Bats";
        if (desc) return "Summons bats to feast on the living!";
        if (cast || fail) _necro_do_summon(SUMMON_BAT, 1 + randint1(2), fail);
        break;

    case 6:
        if (name) return "Eldritch Howl";
        if (desc) return "Emit a terrifying howl.";
        if (cast) plr_project_los(GF_ELDRITCH_HOWL, spell_power(plev * 3));
        break;

    case 7:
        if (name) return "Black Touch";
        if (desc) return "Damage an adjacent monster with a dark touch.";
       {int dam = _necro_calc_dam(90, 25 + plr->to_d_spell);
        if (info) return _necro_info_dam(dam);
        if (cast && !_necro_do_touch(GF_DARK, dam)) return NULL;}
        break;

    /* Sepulchral Ways */
    case 8:
        if (name) return "Summon Wolves";
        if (desc) return "Summons wolves to feast on the living!";
        if (cast || fail) _necro_do_summon(SUMMON_WOLF, 1 + randint1(2), fail);
        break;

    case 9:
        if (name) return "Black Cloak";
        if (desc) return "You become shrouded in darkness, provided you remove your torch.";
        if (cast) plr_tim_add(T_SUPERSTEALTH, spell_power(randint1(plev) + plev));
        break;

    case 10:
        assert(_UNDEAD_SIGHT == 10);
        if (name) return "Undead Sight";
        if (desc) return "Learn about your nearby surroundings by communing with the dead.";
        if (info) return info_radius(DETECT_RAD_MAP);
        if (cast)
        {
            map_area(DETECT_RAD_MAP);
            detect_traps(DETECT_RAD_DEFAULT, TRUE);
            detect_doors(DETECT_RAD_DEFAULT);
            detect_stairs(DETECT_RAD_DEFAULT);
            detect_recall(DETECT_RAD_DEFAULT);
        }
        break;

    case 11:
        assert(_UNDEAD_LORE == 11);
        if (name) return "Undead Lore";
        if (desc) return "Ask the dead to examine an object for you.";
        if (cast) ident_spell(NULL);
        break;

    case 12:
        if (name) return "Repelling Touch";
        if (desc) return "Conjure a foul wind to blow an adjacent monster away.";
    
        if (cast)
        {
            int dir;
            point_t pos;
            mon_ptr mon;

            if (!_necro_check_touch()) return NULL;
            if (!get_rep_dir2(&dir)) return NULL;
            if (dir == 5) return NULL;

            pos = point_step(plr->pos, dir);
            mon = dun_mon_at(cave, pos);

            if (!mon)
            {
                msg_print("There is no monster.");
                return NULL;
            }
            else
            {
                int i;
                point_t old_pos = pos;
                point_t tgt_pos = pos;

                plr_on_touch_mon(mon);
    
                for (i = 0; i < 10; i++)
                {
                    pos = point_step(pos, dir);
                    if (!dun_allow_mon_at(cave, pos)) break;
                    tgt_pos = pos;
                }
                if (!point_equals(tgt_pos, old_pos))
                {
                    char m_name[80];
                    monster_desc(m_name, mon, 0);
                    msg_format("A foul wind blows %s away!", m_name);
                    dun_move_mon(cave, mon, tgt_pos);
                    draw_pos(old_pos);
                    draw_pos(tgt_pos);
    
                    if (mon->race->light || mon->race->lantern)
                        plr->update |= PU_MON_LIGHT;
                }
            }
        }
        break;

    case 13:
        if (name) return "Vampiric Touch";
        if (desc) return "Steal life from an adjacent foe.";
       {int dam = _necro_calc_dam(100, 66 + plr->to_d_spell);
        if (info) return _necro_info_dam(dam);
        if (cast && !_necro_do_touch(GF_OLD_DRAIN, dam)) return NULL;}
        break;

    case 14:
        if (name) return "Dread of Night";
        if (desc) return "Summons Dread to do your bidding. Beware of failure!";
        if (cast || fail) _necro_do_summon(SUMMON_DREAD, 1 + randint0(3), fail);
        break;

    case 15:
        if (name) return "Entomb";
        if (desc) return "Entombs chosen foe.";
        if (cast && !plr_cast_ball(0, GF_ENTOMB, dice)) return NULL;
        break;

    /* Return of the Dead */
    case 16:
        if (name) return "Summon Zombies";
        if (desc) return "The dead are back and hungry for brains!";
        if (cast || fail) _necro_do_summon(SUMMON_ZOMBIE, 2 + randint1(3), fail);
        break;

    case 17:
        if (name) return "Summon Skeletons";
        if (desc) return "Summon skeletal assistance.";
        if (cast || fail) _necro_do_summon(SUMMON_SKELETON, 1 + randint0(3), fail);
        break;

    case 18:
        if (name) return "Summon Ghosts";
        if (desc) return "Recall the spirits of slain warriors for unholy servitude.";
        if (cast || fail) _necro_do_summon(SUMMON_GHOST, 1 + randint0(3), fail);
        break;

    case 19:
        if (name) return "Summon Vampires";
        if (desc) return "Its time to command the commanders!";
        if (cast || fail) _necro_do_summon(SUMMON_VAMPIRE, 1 + randint0(2), fail);
        break;

    case 20:
        if (name) return "Summon Wraiths";
        if (desc) return "Summon wights and wraiths to do your bidding.";
        if (cast || fail) _necro_do_summon(SUMMON_WIGHT, 1 + randint0(2), fail);
        break;

    case 21:
        if (name) return "Summon Liches";
        if (desc) return "Call forth former necromancers.";
        if (cast || fail) _necro_do_summon(SUMMON_LICH, 1 + randint0(2), fail);
        break;

    case 22:
        if (name) return "Unholy Word";
        if (desc) return "Utter an unspeakable word. The morale of your visible evil pets is temporarily boosted and they will serve you with renewed enthusiasm.";
        if (cast) plr_project_los(GF_UNHOLY_WORD, plev * 6);
        break;

    case 23:
        if (name) return "Lost Cause";
        if (desc) return "Make a last ditch Kamikaze effort for victory!";
        if (cast)
        {
            /* Note: It is common pet strategy to surround yourself with minions when meleeing
             * an enemy. In this case, a simple typo will kill you ... several times over! */
            if (!get_check("Are you sure? Exploding nearby pets may prove fatal!")) return NULL;
            discharge_minion();
        }
        break;

    /* Necromantic Tome */
    case 24:
        if (name) return "Draining Touch";
        if (desc) return "Steal mana from an adjacent foe.";
       {int dam = _necro_calc_dam(50, plr->to_d_spell);
        if (info) return _necro_info_dam(dam);
        if (cast && !_necro_do_touch(GF_DRAINING_TOUCH, dam)) return NULL;}
        break;

    case 25:
        if (name) return "Unhallow Ground";
        if (desc) return "Makes the current square unholy.";
        if (cast) warding_glyph(); /* TODO: Add new cave feature! */
        break;

    case 26:
    {
        int base = spell_power(20);
        if (name) return "Shield of the Dead";
        if (desc) return "Grants temporary protection";
        if (info) return info_duration(base, base);
        if (cast)
        {
            plr_tim_add(T_RES_NETHER, randint1(base) + base);
            plr_tim_add(T_RES_POIS, randint1(base) + base);
            plr_tim_add(T_RES_COLD, randint1(base) + base);
            plr_tim_add(T_STONE_SKIN, randint1(base) + base);
        }
        break;
    }
    case 27:
        if (name) return "Rending Touch";
        if (desc) return "Damage an adjacent monster with a disintegrating touch.";
       {int dam = _necro_calc_dam(250, plr->to_d_spell);
        if (info) return _necro_info_dam(dam);
        if (cast && !_necro_do_touch(GF_DISINTEGRATE, dam)) return NULL;}
        break;

    case 28:
        if (name) return "Repose of the Dead";
        if (desc) return "Sleep the sleep of the dead for a few rounds, during which time nothing can awaken you, except perhaps death. When (if?) you wake up, you will be thoroughly refreshed!";
        if (cast && !cast_spell(repose_of_the_dead_spell)) return NULL;
        break;

    case 29:
        if (name) return "Sepulchral Wind";
        if (desc) return "You call forth the wind of the dead. All nearby monsters are blown away!";
        {
            int power = spell_power(plev * 4);
            if (info) return info_power(power);
            if (cast) plr_project_los(GF_TELEPORT, power);
        }
        break;

    case 30:
        if (name) return "Deadly Touch";
        if (desc) return "Attempt to kill an adjacent monster.";
        if (cast && !_necro_do_touch(GF_DEATH_TOUCH, plev * 200)) return NULL;
        break;

    case 31:
        if (name) return "Necromancy";
        if (desc) return "Bridge the world of the living with the world of the dead! Vast hordes of undead will come forth to serve the one true necromancer!";
        if (cast)
        {
            int i;
            int sp_sides = 20 + plev;
            int sp_base = plev;
            int power = spell_power(plev);

            power += randint1(power);

            for (i = 0; i < 18; i++)
            {
                int attempt = 10;
                point_t mp;
                int what;

                while (attempt--)
                {
                    mp = scatter(plr->pos, 4);
                    if (dun_allow_mon_at(cave, mp)) break;
                }
                if (attempt < 0) continue;
                switch (randint1(4))
                {
                case 1: what = SUMMON_LICH; break;
                case 2: what = SUMMON_WIGHT; break;
                case 3: what = SUMMON_VAMPIRE; break;
                case 4:
                default: what = SUMMON_GHOST; break;
                }
                summon_specific(who_create_plr(), mp, power, what, (PM_ALLOW_GROUP | PM_FORCE_PET | PM_HASTE));
            }
            plr_tim_add(T_FAST, randint1(sp_sides) + sp_base);
        }
        break;

    }

    return "";
}

/**********************************************************************
 * Hooks
 **********************************************************************/
static void _calc_bonuses(void)
{
    plr->align -= 200;
    plr->spell_cap += 2;
    plr->see_nocto = DUN_VIEW_MAX;

    if (equip_find_art("~.Vecna"))
        plr->dec_mana++;
    if (equip_find_art("].Vecna"))
        plr->easy_spell++;

    if (plr->lev >= 5) res_add(GF_COLD);
    if (plr->lev >= 15) plr->see_inv++;
    if (plr->lev >= 25) plr->hold_life++;
    if (plr->lev >= 30) plr->wizard_sight = TRUE;
    if (plr->lev >= 35) res_add(GF_POIS);
    if (plr->lev >= 45) plr->hold_life++;
}

static void _get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    if (plr->lev >= 5) add_flag(flgs, OF_RES_(GF_COLD));
    if (plr->lev >= 15) add_flag(flgs, OF_SEE_INVIS);
    if (plr->lev >= 25) add_flag(flgs, OF_HOLD_LIFE);
    if (plr->lev >= 35) add_flag(flgs, OF_RES_(GF_POIS));
}

static int _get_powers(spell_info* spells, int max)
{
    int ct = 0;

    spell_info* spell = &spells[ct++];
    spell->level = 1;
    spell->cost = 1;
    spell->fail = calculate_fail_rate(spell->level, 30, plr->stat_ind[A_INT]);
    spell->fn = animate_dead_spell;

    spell = &spells[ct++];
    spell->level = 5;
    spell->cost = 5;
    spell->fail = calculate_fail_rate(spell->level, 30, plr->stat_ind[A_INT]);
    spell->fn = enslave_undead_spell;

    return ct;
}

static caster_info * _caster_info(void)
{
    static caster_info me = {0};
    static bool init = FALSE;
    if (!init)
    {
        me.magic_desc = "spell";
        me.which_stat = A_INT;
        me.encumbrance.max_wgt = 430;
        me.encumbrance.weapon_pct = 100;
        me.encumbrance.enc_wgt = 600;
        me.options = CASTER_GLOVE_ENCUMBRANCE | CASTER_ALLOW_DEC_MANA;
        me.realm1_choices = CH_NECROMANCY;
        init = TRUE;
    }
    return &me;
}

static void _birth(void)
{
    plr_birth_obj_aux(TV_SOFT_ARMOR, SV_ROBE, 1);
    plr_birth_spellbooks();
}

static bool _destroy_object(obj_ptr obj)
{
    if (obj->tval == TV_LIFE_BOOK || obj->tval == TV_CRUSADE_BOOK)
    {
        char name[MAX_NLEN];
        int  sp = 0;
        int  osp = plr->csp;

        switch (obj->sval)
        {
        case 0: sp = 10; break;
        case 1: sp = 25; break;
        case 2: sp = 100; break;
        case 3: sp = 666; break;
        }

        sp_player(sp);
        object_desc(name, obj, OD_COLOR_CODED);
        msg_format("You gleefully destroy %s!", name);
        if (plr->csp > osp)
            msg_print("You feel your head clear.");

        return TRUE;
    }
    return FALSE;
}
static void _kill_monster(mon_ptr mon)
{
    /* Vecna always drops the Decrepit Hand of Vecna */
    if (mon_race_is_(mon->race, "L.Vecna"))
    {
        art_ptr art = arts_parse("].Vecna");
        assert(art);
        if (art && !art->generated)
        {
            object_type forge;
            if (art_create_std(&forge, art, 0))
                dun_drop_near(cave, &forge, plr->pos);
        }
    }
    /* Undead Uniques might drop the Shriveled Eye of Vecna */
    else if ( mon_is_undead(mon)
           && (mon_is_unique(mon) || mon_race_is_(mon->race, "W.nazgul")) )
    {
        art_ptr art = arts_parse("~.Vecna");
        assert(art);
        if (art && !art->generated)
        {
            object_type forge;
            if ( mon->race->alloc.lvl >= 50
              && _1d(200) < mon->race->alloc.lvl
              && art_create_std(&forge, art, 0) )
            {
                dun_drop_near(cave, &forge, plr->pos);
            }
        }
    }
}
static bool _auto_id(obj_ptr obj)
{
    int cost = plr_can_auto_cast(REALM_NECROMANCY, _UNDEAD_LORE);
    if (cost)
    {
        /* plr_auto_cast would prompt for the object via do_spell(SPELL_CAST)
         * do it by hand */
        sp_player(-cost);
        identify_item(obj);
        spell_stats_on_cast_old(REALM_NECROMANCY, _UNDEAD_LORE);
        return TRUE;
    }
    return FALSE;
}
static bool _auto_detect(void)
{
    return plr_auto_cast(REALM_NECROMANCY, _UNDEAD_SIGHT);
}
/**********************************************************************
 * The Class
 **********************************************************************/
plr_class_ptr necromancer_get_class(void)
{
    static plr_class_ptr me = NULL;

    if (!me)
    {               /* dis, dev, sav, stl, srh, fos, thn, thb */
        skills_t bs = { 30,  40,  38,   4,  16,  20,  34,  20};
        skills_t xs = { 35,  75,  55,   0,   0,   0,  30,  35};

        me = plr_class_alloc(CLASS_NECROMANCER);
        me->name = "Necromancer";
        me->desc = "A Necromancer attempts to gain both power and knowledge through "
                  "communion with the dead. They use powerful necromancy magic to "
                  "summon aid from the dead, whether directly in terms of undead "
                  "servitude, or indirectly through other-worldly knowledge. Necromancy "
                  "also offers myriad foul offensive spells, but all of these require "
                  "the Necromancer to physically touch his foe. To do so, the Necromancer "
                  "may wield neither weapon, nor gloves. But a powerful necromancer is truly "
                  "awe inspiring, and may even kill foes with a single deadly touch! "
                  "In addition, they forever hunt for the legendary Eye and Hand of Vecna in "
                  "order to complete their power.\n\n"
                  "The necromancer can see in the dark and need not wear an ordinary light "
                  "source. Without a light source they can hide in shadows, but this requires "
                  "a necromantic spell.",
        
        me->stats[A_STR] = -2;
        me->stats[A_INT] =  3;
        me->stats[A_WIS] = -4;
        me->stats[A_DEX] =  0;
        me->stats[A_CON] = -2;
        me->stats[A_CHR] =  2;
        me->skills = bs;
        me->extra_skills = xs;
        me->life = 95;
        me->base_hp = 2;
        me->exp = 125;
        me->pets = 10;
        me->flags = CLASS_SENSE1_MED | CLASS_SENSE1_WEAK |
                    CLASS_SENSE2_FAST | CLASS_SENSE2_STRONG | CLASS_MAGE_BONUS;

        me->hooks.birth = _birth;
        me->hooks.caster_info = _caster_info;
        me->hooks.calc_bonuses = _calc_bonuses;
        me->hooks.calc_innate_attacks = _calc_innate_attacks;
        me->hooks.calc_innate_bonuses = _calc_innate_bonuses;
        me->hooks.get_flags = _get_flags;
        me->hooks.get_powers = _get_powers;
        me->hooks.character_dump = spellbook_character_dump;
        me->hooks.destroy_object = _destroy_object;
        me->hooks.kill_monster = _kill_monster;
        me->hooks.timer_on = repose_timer_on;
        me->hooks.timer_off = repose_timer_off;
        me->hooks.auto_id = _auto_id;
        me->hooks.auto_detect = _auto_detect;
    }

    return me;
}
