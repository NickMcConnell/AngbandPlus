#include "angband.h"

/* Check for valid equipment. Note, the pointer we return
   will point to a descriptive error message indefinitely.
   We don't use format(...). Also, different messages will
   have different addresses, so that _calc_bonuses() can
   keep the user up to date as to why their powers don't work. */
cptr _equip_error(void)
{
    int wgt = equip_weight(obj_is_armor);

    if (wgt > (100 + (plr->lev * 4)))
        return "The weight of your equipment is disrupting your talents.";

    if (equip_find_obj(TV_SHIELD, SV_ANY) || equip_find_obj(TV_CAPTURE, SV_ANY))
        return "Your shield is disrupting your talents.";

    if (plr->weapon_ct > 1)
        return "Dual wielding is disrupting your talents.";

    if (equip_find_obj(TV_SWORD, SV_POISON_NEEDLE))
        return "The Poison Needle is not an honorable dueling weapon.";

    if (plr->anti_magic)
        return "An anti-magic barrier disrupts your talents.";

    return NULL;
}
cptr _equip_short_error(void)
{
    int wgt = equip_weight(obj_is_armor);

    if (wgt > (100 + (plr->lev * 4)))
        return "<color:r>Heavy Armor</color>";

    if (equip_find_obj(TV_SHIELD, SV_ANY) || equip_find_obj(TV_CAPTURE, SV_ANY))
        return "<color:g>Icky Shield</color>";

    if (plr->weapon_ct > 1)
        return "<color:r>Duel Wield</color>";

    if (equip_find_obj(TV_SWORD, SV_POISON_NEEDLE))
        return "<color:g>Icky Weapon</color>";

    if (plr->anti_magic)
        return "<color:r>Anti-magic</color>";

    return NULL;
}

/************************************************************************
 * Current Challenge
 ************************************************************************/
cptr duelist_current_challenge(void)
{
    static char current_challenge[200];

    if (who_is_mon(plr->duelist_target))
    {
        monster_desc(current_challenge, who_mon(plr->duelist_target), MD_ASSUME_VISIBLE);
        return current_challenge;
    }
    if (_equip_error())
        return "Talents Disrupted";

    return "No Current Challenge";
}

static bool _challenge_p(mon_ptr mon)
{
    if (mon_is_pet(mon)) return FALSE;
    if (!mon->ml) return FALSE;
    if (mon->cdis > MAX_RANGE) return FALSE;
    return TRUE;
}
bool duelist_can_challenge(void)
{
    vec_ptr v = dun_filter_mon(cave, _challenge_p);
    bool    b = vec_length(v) > 0;
    vec_free(v);
    return b;
}
static void _clear_challenge(void)
{
    mon_ptr mon = who_mon(plr->duelist_target);
    plr->duelist_target = who_create_null();
    if (mon)
    {
        update_mon(mon, FALSE);
        if (plr->pet_extra_flags & PF_HILITE)
            dun_draw_pos(mon->dun, mon->pos); /* XXX might be off-level */
    }
}
static void _set_challenge(mon_ptr mon)
{
    _clear_challenge();
    plr->duelist_target = who_create_mon(mon);
    update_mon(mon, FALSE);
    if (plr->pet_extra_flags & PF_HILITE)
        dun_draw_pos(mon->dun, mon->pos);
}
bool duelist_issue_challenge(void)
{
    bool result = FALSE;
    who_t who = who_create_null();

    if (target_set(TARGET_MARK))
        who = plr->target;

    if (who_is_mon(who))
    {
        mon_ptr foe = who_mon(who);
        if (foe == who_mon(plr->duelist_target))
            msg_format("%^s has already been challenged.", duelist_current_challenge());
        else
        {
            _set_challenge(foe);
            msg_format("You challenge %s to a duel!", duelist_current_challenge());
            mon_tim_remove(foe, MT_SLEEP);
            set_hostile(foe);
            result = TRUE;
        }
    }
    else if (!who_is_null(plr->duelist_target))
    {
        _clear_challenge();
        msg_print("You cancel your current challenge!");
    }

    plr->redraw |= PR_HEALTH_BARS | PR_STATUS;
    return result;
}

/************************************************************************
 * Melee
 ************************************************************************/
static bool _begin_weapon(plr_attack_ptr context)
{
    bool boost = FALSE;
    if (context->info.type != PAT_WEAPON) return TRUE;
    if (context->flags & PAC_DISPLAY)
        boost = who_is_mon(plr->duelist_target);
    else if (context->mon == who_mon(plr->duelist_target))
        boost = TRUE;
    if (boost)
    {
        context->skill *= 2;
        if (context->flags & PAC_DISPLAY)
            context->info.info = "Enhanced skill displayed for current duel.";
        else
            context->cookie = context; /* bool */
    }
    return TRUE;
}
static void _mod_damage(plr_attack_ptr context)
{
    int dam = context->dam;
    if (context->flags & PAC_DISPLAY) return;
    if (context->info.type != PAT_WEAPON) return;
    if (context->mon != who_mon(plr->duelist_target)) return;
    if (!context->dam) return;  /* MON_INVULNER() */
                
    if (plr->lev >= 10) /* Careful Aim */
    {
        context->dam *= 2;
    }
    if ( plr->lev >= 15    /* Hamstring */
      && !mon_save_p(context->mon, A_DEX) )
    {
        if (_1d(100) > mon_res_pct(context->mon, GF_SLOW))
        {
            msg_format("You <color:y>hamstring</color> %s.", context->mon_name_obj);
            mon_tim_add(context->mon, T_SLOW, 50);
        }
        else mon_lore_resist(context->mon, GF_SLOW);
    }
    if ( plr->lev >= 25    /* Stunning Blow */
        && !mon_save_p(context->mon, A_DEX) )
    {
        if (_1d(100) > mon_res_pct(context->mon, GF_STUN))
        {
            msg_format("%^s is dealt a <color:B>stunning</color> blow.", context->mon_name);
            mon_stun(context->mon, mon_stun_amount(context->dam));
        }
        else mon_lore_resist(context->mon, GF_STUN);
    }
    if ( plr->lev >= 20    /* Wounding Strike */
      && !mon_save_p(context->mon, A_DEX) )
    {
        msg_format("%^s is dealt a <color:r>wounding</color> strike.", context->mon_name);
        context->dam += MIN(context->mon->hp / 5, randint1(3) * dam);
    }
    if ( plr->lev >= 40    /* Greater Wounding Strike */
      && !mon_save_p(context->mon, A_DEX) )
    {
        msg_format("%^s is dealt a <color:v>*WOUNDING*</color> strike.", context->mon_name);
        context->dam += MIN(context->mon->hp * 2 / 5, rand_range(2, 10) * dam);
    }
    context->dam_drain = context->dam;
}
static void _end(plr_attack_ptr context)
{
    if (context->flags & PAC_DISPLAY) return;
    if (context->stop != STOP_MON_DEAD) return;
    if (!context->cookie) return;

    if (mon_is_fixed_unique(context->mon))
        virtue_add(VIRTUE_HONOUR, _1d(3));
    else if (_1d(20) < mon_lvl(context->mon) - plr->lev)
        virtue_add(VIRTUE_HONOUR, 1);

    plr->duelist_target = who_create_null();
    plr->redraw |= PR_HEALTH_BARS;
    plr->redraw |= PR_STATUS;

    if (plr->lev >= 35 && duelist_can_challenge())    /* Endless Duel */
    {
        /* Hacks so that get_fire_dir() actually allows user to select a new target */
        plr->target = who_create_null();
        command_dir = 0;
        msg_print("Your chosen target is vanquished!  Select another.");
        msg_print(NULL);
        duelist_issue_challenge();
    }
    else
        msg_print("Your chosen target is vanquished!");
}
static void _attack_init(plr_attack_ptr context)
{
    context->hooks.begin_weapon_f = _begin_weapon;
    context->hooks.mod_damage_f = _mod_damage;
    context->hooks.end_f = _end;
}

/************************************************************************
 * Rush Attack (cf rush_attack)
 ************************************************************************/
typedef enum { 
    _rush_cancelled,  /* Don't charge player energy ... they made a dumb request */
    _rush_failed,     /* Rush to foe was blocked by another monster, or foe out of range */
    _rush_succeeded   /* Got him! */
} _rush_result;

typedef enum {
    _rush_normal,     /* Attacks first monster in the way */
    _rush_acrobatic,   /* Displaces intervening monsters (waking them up) */
    _rush_phase,
} _rush_type;

static bool _cell_allow_rush(dun_cell_ptr cell)
{
    if (cell->type == FEAT_WALL) return FALSE;
    if (cell->type == FEAT_TREE) return FALSE;
    if (cell->type == FEAT_CHASM) return FALSE;
    if (door_is_closed(cell)) return FALSE;
    if (cell->type == FEAT_PATTERN) return FALSE;
    return TRUE;
}
_rush_result _rush_attack(int rng, _rush_type type)
{
    _rush_result result = _rush_cancelled;
    mon_ptr mon = who_mon(plr->duelist_target);
    point_t tgt, cur, next;
    point_t path[32];
    int path_n, i;
    bool moved = FALSE;
    int flg = 0;

    if (type == _rush_normal)
        flg = PROJECT_STOP | PROJECT_KILL;
    else if (type == _rush_acrobatic)
        flg = PROJECT_THRU | PROJECT_KILL;
    else
        flg = PROJECT_DISI | PROJECT_THRU;

    if (!mon)
    {
        msg_print("You need to select a foe first (Mark Target).");
        return result;
    }
    tgt = mon->pos;

    /* Foe must be visible. For all charges except the phase charge, the
       foe must also be in your line of sight */
    if (!mon->ml || (type != _rush_phase && !plr_view(tgt)))
    {
        msg_format("%^s is not in your line of sight.", duelist_current_challenge());
        return result;
    }

    project_length = rng;
    path_n = project_path(path, rng, plr->pos, tgt, flg);
    project_length = 0;

    if (!path_n) return result;

    result = _rush_failed;

    cur = plr->pos;

    /* Project along the path */
    for (i = 0; i < path_n; i++)
    {
        mon_ptr mon2;
        dun_cell_ptr cell;
        bool can_enter = FALSE;
        bool old_pass_wall = plr->pass_wall;

        next = path[i];
        cell = dun_cell_at(cave, next);
        mon2 = dun_mon_at(cave, next);

        switch (type)
        {
        case _rush_normal:
            if (_cell_allow_rush(cell))
                can_enter = !mon2 && cell_allow_plr(cell);
            break;

        case _rush_acrobatic:
            can_enter = !mon2 && cell_allow_plr(cell);
            break;
        
        case _rush_phase:
            plr->pass_wall = TRUE;
            can_enter = !mon2 && cell_allow_plr(cell);
            plr->pass_wall = old_pass_wall;
            break;
        }

        if (can_enter)
        {
            cur = next;
            continue;
        }

        if (!mon2)
        {
            msg_print("Failed!");
            break;
        }

        /* Move player before updating the monster */
        if (!dun_plr_at(cave, cur))
            move_player_effect(cur, MPE_FORGET_FLOW | MPE_HANDLE_STUFF | MPE_DONT_PICKUP);
        moved = TRUE;

        /* Update the monster */
        update_mon(mon2, TRUE);

        /* But it is not the monster we seek! */
        if (mon != mon2)
        {
            /* Acrobatic Charge attempts to displace monsters on route */
            if (type == _rush_acrobatic)
            {
                /* Swap position of player and monster */
                mon_tim_remove(mon2, MT_SLEEP);
                move_player_effect(next, MPE_FORGET_FLOW | MPE_HANDLE_STUFF | MPE_DONT_PICKUP);
                cur = next;
                continue;
            }
            /* Normal Charge just attacks first monster on route */
            else
                msg_format("There is %s in the way!", mon2->ml ? "another monster" : "someone");
        }

        /* Attack the monster */
        result = _rush_succeeded;
        plr_attack_normal(next);
        break;
    }

    if (!moved && !dun_plr_at(cave, cur))
        move_player_effect(cur, MPE_FORGET_FLOW | MPE_HANDLE_STUFF | MPE_DONT_PICKUP);
    return result;
}

/****************************************************************
 * Private Spells
 ****************************************************************/
static void _acrobatic_charge_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Acrobatic Charge");
        break;
    case SPELL_DESC:
        var_set_string(res, "Move up to 7 squares and attack your marked foe, displacing any monsters in your way.");
        break;
    case SPELL_CAST:
        var_set_bool(res, _rush_attack(7, _rush_acrobatic) != _rush_cancelled);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _charge_target_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Charge");
        break;
    case SPELL_DESC:
        var_set_string(res, "Move up to 5 squares and attack your marked foe.");
        break;
    case SPELL_CAST:
        var_set_bool(res, _rush_attack(5, _rush_normal) != _rush_cancelled);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _darting_duel_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Darting Duel");
        break;
    case SPELL_DESC:
        var_set_string(res, "Move up to 5 squares and attack your marked foe. Strafe if you attack your foe.");
        break;
    case SPELL_CAST:
        {
            mon_ptr mon = who_mon(plr->duelist_target);
            _rush_result r = _rush_attack(5, _rush_normal);
            if (r == _rush_cancelled)
                var_set_bool(res, FALSE);
            else 
            {
                var_set_bool(res, TRUE);
                if (r == _rush_succeeded && mon == who_mon(plr->duelist_target)) /* Endless Duel might change challenge */
                {
                    mon_anger(mon);
                    teleport_player(10, TELEPORT_LINE_OF_SIGHT);
                }
            }
        }
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _disengage_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Disengage");
        break;
    case SPELL_DESC:
        var_set_string(res, "You teleport (range 100), and prevent your marked foe from following, even if it's a monster that can normally follow teleportation. After the teleport, your foe is no longer marked.");
        break;
    case SPELL_CAST:
        if (!who_is_mon(plr->duelist_target))
        {
            msg_print("You need to mark your target first.");
            var_set_bool(res, FALSE);
        }
        else if (!who_mon(plr->duelist_target)->ml)
        {
            msg_print("You may not disengage unless your foe is visible.");
            var_set_bool(res, FALSE);
        }
        else
        {
            teleport_player(100, TELEPORT_DISENGAGE);
            plr->duelist_target = who_create_null();
            msg_print("You disengage from your current challenge.");
            plr->redraw |= PR_HEALTH_BARS;
        
            var_set_bool(res, TRUE);
        }
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _isolation_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Isolation");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attempts to teleport away all monsters in Line of Sight other than your marked foe.");
        break;
    case SPELL_CAST:
        if (!who_is_mon(plr->duelist_target))
        {
            msg_print("You need to mark your target first.");
            var_set_bool(res, FALSE);
        }
        else
        {
            plr_project_los(GF_ISOLATION, plr->lev * 4);
            var_set_bool(res, TRUE);
        }
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _mark_target_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Mark Target");
        break;
    case SPELL_DESC:
        var_set_string(res, "Mark selected monster as designated foe. You may only mark a single target at a time, and receive great benefits when fighting this target.");
        break;

    case SPELL_INFO:
        var_set_string(res, format("%^s", duelist_current_challenge()));
        break;

    case SPELL_CAST:
        var_set_bool(res, duelist_issue_challenge());
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _phase_charge_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Phase Charge");
        break;
    case SPELL_DESC:
        var_set_string(res, "Move up to 10 squares and attack your marked foe. Functions even if there are walls or closed doors between you and your target.");
        break;
    case SPELL_CAST:
        var_set_bool(res, _rush_attack(10, _rush_phase) != _rush_cancelled);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void strafing_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Strafing");
        break;
    case SPELL_DESC:
        var_set_string(res, "Blink to a new location in the line of sight of your current location.");
        break;
    case SPELL_ENERGY:
        if (mut_present(MUT_ASTRAL_GUIDE))
            var_set_int(res, 30);
        else
            default_spell(cmd, res);
        break;
    case SPELL_CAST:
        teleport_player(10, TELEPORT_LINE_OF_SIGHT);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

/****************************************************************
 * Spell Table
 ****************************************************************/

#define MAX_DUELIST_SPELLS    8

static spell_info _spells[MAX_DUELIST_SPELLS] = 
{
    /*lvl cst fail spell */
    {  1,   0,  0, _mark_target_spell },
    {  8,  10,  0, _charge_target_spell },
    { 16,   8,  0, strafing_spell },
    { 24,  25,  0, _disengage_spell },
    { 32,  30,  0, _acrobatic_charge_spell },
    { 40,  60,  0, _isolation_spell },
    { 45,  60,  0, _darting_duel_spell },
    { 48,  60,  0, _phase_charge_spell },
}; 

static int _get_spells(spell_info* spells, int max)
{
    int i;
    int ct = 0;
    int stat_idx = plr->stat_ind[A_DEX];
    cptr msg = _equip_error();

    if (msg)
    {
        msg_print(msg);
        return 0;
    }
    
    /* Initialize a (copied) spell list with current casting costs and fail rates */
    for (i = 0; i < MAX_DUELIST_SPELLS; i++)
    {
        spell_info *base = &_spells[i];
        if (ct >= max) break;
        if (base->level <= plr->lev)
        {
            spell_info* current = &spells[ct];
            current->fn = base->fn;
            current->level = base->level;
            current->cost = base->cost;

            current->fail = calculate_fail_rate(base->level, base->fail, stat_idx);            
            ct++;
        }
    }
    return ct;
}
static int _ac_penalty(void)
{
    return 20 + 30*plr->lev/50;
}
static void _calc_bonuses(void)
{
    static cptr last_msg = NULL;
    cptr msg = _equip_error();

    plr->to_a -= _ac_penalty();
    plr->dis_to_a -= _ac_penalty();

    if (!msg)
    {
        int x = (plr->stat_ind[A_INT] + 3);
        int l = plr->lev;
        int to_a = x/2 + x*l/50;
        plr->to_a += to_a;
        plr->dis_to_a += to_a;
    }

    if (msg != last_msg)
    {
        last_msg = msg;
        plr->redraw |= PR_EFFECTS;
        if (msg)
        {
            msg_print(msg);
            if (who_is_mon(plr->duelist_target))
            {
                msg_format("%^s is no longer your target.", duelist_current_challenge());
                plr->duelist_target = who_create_null();
                plr->redraw |= PR_HEALTH_BARS;
            }
        }
        else
            msg_print("You regain your talents.");
    }

}
static void _character_dump(doc_ptr doc)
{
    int wgt = 100 + 4*plr->lev;
    doc_printf(doc, "<topic:Abilities>================================== <color:keypress>A</color>bilities ==================================\n\n");
    doc_printf(doc, "  * You suffer <color:r>%d</color> to armor class against non-challenged foes.\n", -_ac_penalty());
    doc_printf(doc, "  * You are restricted to <color:r>%d.%d lbs</color> of armor.\n", wgt/10, wgt%10);
    if (who_is_mon(plr->duelist_target))
    {
        int sav = plr_skill_sav(plr->duelist_target);
        skill_desc_t desc = skills_describe(sav, 7); /* cf _build_general2 in plr_display.c */
        doc_printf(doc, "  * You have challenged <color:U>%s</color> to a duel.\n", duelist_current_challenge());
        doc_printf(doc, "  * You have AC <color:G>%d</color> in this challenge.\n", plr_ac(who_mon(plr->duelist_target)));
        doc_printf(doc, "  * You have <color:%c>%s</color> saving throws in this challenge.\n", attr_to_attr_char(desc.color), desc.desc);
    }
    doc_newline(doc);

    if (!_equip_error())
    {
        spell_info spells[MAX_SPELLS];
        int        ct = _get_spells(spells, MAX_SPELLS);

        plr_display_spells(doc, spells, ct);
    }
}
static void _prt_effects(doc_ptr doc)
{
    cptr msg = _equip_short_error();
    if (msg)
    {
        doc_insert(doc, msg);
        doc_newline(doc);
    }
}
static status_display_t _status_display(void)
{
    status_display_t d = {0};

    if (who_is_mon(plr->duelist_target))
    {
        d.color = TERM_YELLOW;
        d.name = who_mon(plr->duelist_target)->apparent_race->name;
    }
    else
    {
        d.color = TERM_L_BLUE;
        d.name = "No Current Challenge";
    }
    d.abbrev = "Duel";
    return d;
}
static void _calc_weapon_bonuses(object_type *o_ptr, plr_attack_info_ptr info)
{
    if (!_equip_error())
    {
        int to_d = (plr->stat_ind[A_DEX] + 3 - 10) + plr->lev/2 - o_ptr->weight/10;
        info->to_d += to_d;
        info->dis_to_d += to_d;
    }
    info->xtra_blow = 0; /* always just 1 blow ... note plr_calc_blows_hand hasn't been called yet. */
}

static caster_info * _caster_info(void)
{
    static caster_info me = {0};
    static bool init = FALSE;
    if (!init)
    {
        me.magic_desc = "challenge";
        me.options = CASTER_USE_HP;
        me.which_stat = A_DEX;
        init = TRUE;
    }
    return &me;
}

static void _birth(void)
{
    plr_birth_obj_aux(TV_SWORD, SV_RAPIER, 1);
    plr_birth_obj_aux(TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR, 1);
    plr_birth_obj_aux(TV_POTION, SV_POTION_SPEED, 1);
}

plr_class_ptr duelist_get_class(void)
{
    static plr_class_ptr me = NULL;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 30,  24,  23,   3,  22,  16,  50,   0};
    skills_t xs = { 50,  50,  50,   0,   0,   0,  70,   0};

        me = plr_class_alloc(CLASS_DUELIST);
        me->name = "Duelist";
        me->desc =
            "The duelist is the ultimate one-on-one fighter, but finds himself at a severe "
            "disadvantage when facing numerous strong foes. When facing an enemy, the duelist "
            "first issues a challenge, marking that foe for a one on one duel. Of course, this "
            "wakes the monster up (There is no honor in dueling a sleeping enemy). And while "
            "the duelist will honor the fight as a one on one affair, many monsters have no "
            "such scruples.\n\n"
            "Against a challenged foe, the duelist is very strong gaining a bonus to saving "
            "throws, armor class, damage reduction and combat prowess. On the other hand, due "
            "to the single-mindedness of their focus, the duelist is quite vulnerable to unchallenged "
            "opponents. Most of the techniques of this class aim at enforcing the sanctity of the "
            "duel.\n\n"
            "The duelist only ever gains a single attack in combat, but they make the most of this "
            "blow by gaining enhanced effects as they gain experience. Able to wound, stun, and even "
            "hamstring their foes, the prowess of the duelist in a one on one encounter is legendary!\n\n"
            "Duelists favor light armors and cannot equip a shield. They gain no extra bonus when "
            "wielding a weapon with both hands. For their techniques the duelist relies on dexterity.";
        me->stats[A_STR] =  2;
        me->stats[A_INT] =  1;
        me->stats[A_WIS] = -2;
        me->stats[A_DEX] =  2;
        me->stats[A_CON] = -3;
        me->stats[A_CHR] =  2;
        me->skills = bs;
        me->extra_skills = xs;
        me->life = 100;
        me->base_hp = 4;
        me->exp = 150;
        me->pets = 35;
        me->flags = CLASS_SENSE1_FAST | CLASS_SENSE1_STRONG;

        me->hooks.birth = _birth;
        me->hooks.calc_bonuses = _calc_bonuses;
        me->hooks.calc_weapon_bonuses = _calc_weapon_bonuses;
        me->hooks.caster_info = _caster_info;
        me->hooks.get_spells = _get_spells;
        me->hooks.attack_init = _attack_init;
        me->hooks.prt_effects = _prt_effects;
        me->hooks.status_display = _status_display;
        me->hooks.character_dump = _character_dump;
    }

    return me;
}

