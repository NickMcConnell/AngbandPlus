#include "angband.h"

/* Check for valid equipment. Note, the pointer we return
   will point to a descriptive error message indefinitely.
   We don't use format(...). Also, different messages will
   have different addresses, so that _calc_bonuses() can
   keep the user up to date as to why their powers don't work. */
cptr _equip_error(void)
{
    int wgt = equip_weight(obj_is_armor);

    if (wgt > (100 + (p_ptr->lev * 4)))
        return "The weight of your equipment is disrupting your talents.";

    if (equip_find_obj(TV_SHIELD, SV_ANY) || equip_find_obj(TV_CAPTURE, SV_ANY))
        return "Your shield is disrupting your talents.";

    if (p_ptr->weapon_ct > 1)
        return "Dual wielding is disrupting your talents.";

    if (equip_find_obj(TV_SWORD, SV_POISON_NEEDLE))
        return "The Poison Needle is not an honorable dueling weapon.";

    if (p_ptr->anti_magic)
        return "An anti-magic barrier disrupts your talents.";

    return NULL;
}
cptr _equip_short_error(void)
{
    int wgt = equip_weight(obj_is_armor);

    if (wgt > (100 + (p_ptr->lev * 4)))
        return "<color:r>Heavy Armor</color>";

    if (equip_find_obj(TV_SHIELD, SV_ANY) || equip_find_obj(TV_CAPTURE, SV_ANY))
        return "<color:g>Icky Shield</color>";

    if (p_ptr->weapon_ct > 1)
        return "<color:r>Duel Wield</color>";

    if (equip_find_obj(TV_SWORD, SV_POISON_NEEDLE))
        return "<color:g>Icky Weapon</color>";

    if (p_ptr->anti_magic)
        return "<color:r>Anti-magic</color>";

    return NULL;
}

/************************************************************************
 * Current Challenge
 ************************************************************************/
cptr duelist_current_challenge(void)
{
    static char current_challenge[200];

    /* paranoia ... this only seems to happen with wizard mode summoned monsters
       after a save and restore, so probably the wizard 'n' command is broken */
    if (p_ptr->duelist_target_idx && !dun_mon(cave, p_ptr->duelist_target_idx)->r_idx)
        p_ptr->duelist_target_idx = 0;

    if (p_ptr->duelist_target_idx)
    {
        monster_desc(current_challenge, dun_mon(cave, p_ptr->duelist_target_idx), MD_ASSUME_VISIBLE);
        return current_challenge;
    }
    if (_equip_error())
        return "Talents Disrupted";

    return "No Current Challenge";
}

static bool _challenge_p(mon_ptr mon)
{
    if (is_pet(mon)) return FALSE;
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

bool duelist_issue_challenge(void)
{
    bool result = FALSE;
    int m_idx = 0;

    if (target_set(TARGET_MARK))
    {
        if (target_who > 0)
            m_idx = target_who;
        else
        {
            mon_ptr mon = mon_at_xy(target_col, target_row);
            if (mon) m_idx = mon->id;
        }
    }

    if (m_idx)
    {
        if (m_idx == p_ptr->duelist_target_idx)
            msg_format("%^s has already been challenged.", duelist_current_challenge());
        else
        {
            mon_ptr mon = dun_mon(cave, m_idx);
            /* of course, we must first set the target index before duelist_current_challenge()
               will return the correct text */
            p_ptr->duelist_target_idx = m_idx;
            msg_format("You challenge %s to a duel!", duelist_current_challenge());
            mon_tim_remove(mon, MT_SLEEP);
            set_hostile(mon);
            result = TRUE;
        }
    }
    else if (p_ptr->duelist_target_idx)
    {
        p_ptr->duelist_target_idx = 0;
        msg_print("You cancel your current challenge!");
    }

    p_ptr->redraw |= PR_HEALTH_BARS;
    return result;
}

/************************************************************************
 * Melee
 ************************************************************************/
static bool _begin_weapon(plr_attack_ptr context)
{
    if (context->flags & PAC_DISPLAY) return TRUE;
    if (context->info.type != PAT_WEAPON) return TRUE;
    if (context->mon->id != p_ptr->duelist_target_idx) return TRUE;
    context->skill *= 2;
    context->cookie = context; /* bool */
    return TRUE;
}
static void _mod_damage(plr_attack_ptr context)
{
    int dam = context->dam;
    if (context->flags & PAC_DISPLAY) return;
    if (context->info.type != PAT_WEAPON) return;
    if (context->mon->id != p_ptr->duelist_target_idx) return;
    if (!context->dam) return;  /* MON_INVULNER() */
                
    if (p_ptr->lev >= 10) /* Careful Aim */
    {
        context->dam *= 2;
    }
    if ( p_ptr->lev >= 15    /* Hamstring */
        && !(context->race->flags1 & (RF1_UNIQUE))
        && !mon_save_p(context->race->id, A_DEX) )
    {
        msg_format("You <color:y>hamstring</color> %s.", context->mon_name_obj);
        mon_tim_add(context->mon, T_SLOW, 50);
    }
    if ( p_ptr->lev >= 25    /* Stunning Blow */
        && !(context->race->flags3 & (RF3_NO_STUN))
        && !mon_save_p(context->race->id, A_DEX) )
    {
        msg_format("%^s is dealt a <color:B>stunning</color> blow.", context->mon_name);
        mon_stun(context->mon, mon_stun_amount(context->dam));
    }
    if ( p_ptr->lev >= 20    /* Wounding Strike */
      && !mon_save_p(context->race->id, A_DEX) )
    {
        msg_format("%^s is dealt a <color:r>wounding</color> strike.", context->mon_name);
        context->dam += MIN(context->mon->hp / 5, randint1(3) * dam);
    }
    if ( p_ptr->lev >= 40    /* Greater Wounding Strike */
      && !mon_save_p(context->race->id, A_DEX) )
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

    p_ptr->duelist_target_idx = 0;
    p_ptr->redraw |= PR_HEALTH_BARS;

    if (p_ptr->lev >= 35 && duelist_can_challenge())    /* Endless Duel */
    {
        /* Hacks so that get_fire_dir() actually allows user to select a new target */
        target_who = 0;
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

_rush_result _rush_attack(int rng, _rush_type type)
{
    _rush_result result = _rush_cancelled;
    mon_ptr mon;
    int tm_idx;
    point_t tgt, cur, next;
    point_t path[32];
    int path_n, i;
    bool moved = FALSE;
    int flg = 0;
    int dis = 0;

    if (type == _rush_normal)
        flg = PROJECT_STOP | PROJECT_KILL;
    else if (type == _rush_acrobatic)
        flg = PROJECT_THRU | PROJECT_KILL;
    else
        flg = PROJECT_DISI | PROJECT_THRU;

    if (!p_ptr->duelist_target_idx)
    {
        msg_print("You need to select a foe first (Mark Target).");
        return result;
    }
    tm_idx = p_ptr->duelist_target_idx;

    mon = dun_mon(cave, tm_idx);
    tgt = mon->pos;
    dis = point_distance(tgt, p_ptr->pos);

    /* Foe must be visible. For all charges except the phase charge, the
       foe must also be in your line of sight */
    if (!mon->ml || (type != _rush_phase && !los(tgt.y, tgt.x, p_ptr->pos.y, p_ptr->pos.x)))
    {
        msg_format("%^s is not in your line of sight.", duelist_current_challenge());
        return result;
    }

    if (dis > rng)
    {
        msg_format("Your foe is out of range (%d vs %d).", dis, rng);
        if (!get_check("Charge anyway? ")) return result;
    }

    project_length = rng;
    path_n = project_path(path, rng, p_ptr->pos, tgt, flg);
    project_length = 0;

    if (!path_n) return result;

    result = _rush_failed;

    cur = p_ptr->pos;

    /* Project along the path */
    for (i = 0; i < path_n; i++)
    {
        mon_ptr mon;
        cave_type *c_ptr;
        bool can_enter = FALSE;
        bool old_pass_wall = p_ptr->pass_wall;

        next = path[i];
        c_ptr = cave_at(next);
        mon = mon_at(next);

        switch (type)
        {
        case _rush_normal:
            can_enter = cave_empty_at(next) && player_can_enter(c_ptr->feat, 0);
            break;

        case _rush_acrobatic:
            can_enter = !mon && player_can_enter(c_ptr->feat, 0);
            break;
        
        case _rush_phase:
            p_ptr->pass_wall = TRUE;
            can_enter = !mon && player_can_enter(c_ptr->feat, 0);
            p_ptr->pass_wall = old_pass_wall;
            break;
        }

        if (can_enter)
        {
            cur = next;
            continue;
        }

        if (!mon)
        {
            msg_print("Failed!");
            break;
        }

        /* Move player before updating the monster */
        if (!plr_at(cur)) move_player_effect(cur, MPE_FORGET_FLOW | MPE_HANDLE_STUFF | MPE_DONT_PICKUP);
        moved = TRUE;

        /* Update the monster */
        update_mon(mon, TRUE);

        /* But it is not the monster we seek! */
        if (tm_idx != mon->id)
        {
            /* Acrobatic Charge attempts to displace monsters on route */
            if (type == _rush_acrobatic)
            {
                /* Swap position of player and monster */
                mon_tim_remove(mon, MT_SLEEP);
                move_player_effect(next, MPE_FORGET_FLOW | MPE_HANDLE_STUFF | MPE_DONT_PICKUP);
                cur = next;
                continue;
            }
            /* Normal Charge just attacks first monster on route */
            else
                msg_format("There is %s in the way!", mon->ml ? (tm_idx ? "another monster" : "a monster") : "someone");
        }

        /* Attack the monster */
        if (tm_idx == p_ptr->duelist_target_idx) result = _rush_succeeded;
        plr_attack_normal(next);
        break;
    }

    if (!moved && !plr_at(cur)) move_player_effect(cur, MPE_FORGET_FLOW | MPE_HANDLE_STUFF | MPE_DONT_PICKUP);
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
            int tmp = p_ptr->duelist_target_idx;
            _rush_result r = _rush_attack(5, _rush_normal);
            if (r == _rush_cancelled)
                var_set_bool(res, FALSE);
            else 
            {
                var_set_bool(res, TRUE);
                if (r == _rush_succeeded && tmp == p_ptr->duelist_target_idx)
                {
                    monster_type *m_ptr = dun_mon(cave, p_ptr->duelist_target_idx);
                    mon_anger(m_ptr);
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
        if (!p_ptr->duelist_target_idx)
        {
            msg_print("You need to mark your target first.");
            var_set_bool(res, FALSE);
        }
        else if (!dun_mon(cave, p_ptr->duelist_target_idx)->ml)
        {
            msg_print("You may not disengage unless your foe is visible.");
            var_set_bool(res, FALSE);
        }
        else
        {
            teleport_player(100, TELEPORT_DISENGAGE);
            p_ptr->duelist_target_idx = 0;
            msg_print("You disengage from your current challenge.");
            p_ptr->redraw |= PR_HEALTH_BARS;
        
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
        if (!p_ptr->duelist_target_idx)
        {
            msg_print("You need to mark your target first.");
            var_set_bool(res, FALSE);
        }
        else
        {
            project_los(GF_ISOLATION, p_ptr->lev * 4);
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
    int stat_idx = p_ptr->stat_ind[A_DEX];
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
        if (base->level <= p_ptr->lev)
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

static void _calc_bonuses(void)
{
    static cptr last_msg = NULL;
    cptr msg = _equip_error();

    p_ptr->to_a -= 50;
    p_ptr->dis_to_a -= 50;

    if (!msg)
    {
        int x = (p_ptr->stat_ind[A_INT] + 3);
        int l = p_ptr->lev;
        int to_a = x/2 + x*l/50;
        p_ptr->to_a += to_a;
        p_ptr->dis_to_a += to_a;
    }

    if (msg != last_msg)
    {
        last_msg = msg;
        p_ptr->redraw |= PR_EFFECTS;
        if (msg)
        {
            msg_print(msg);
            if (p_ptr->duelist_target_idx)
            {
                msg_format("%^s is no longer your target.", duelist_current_challenge());
                p_ptr->duelist_target_idx = 0;
                p_ptr->redraw |= PR_HEALTH_BARS;
            }
        }
        else
            msg_print("You regain your talents.");
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

static void _calc_weapon_bonuses(object_type *o_ptr, plr_attack_info_ptr info)
{
    int to_d = (p_ptr->stat_ind[A_DEX] + 3 - 10) + p_ptr->lev/2 - o_ptr->weight/10;

    if (!_equip_error())
    {
        info->to_d += to_d;
        info->dis_to_d += to_d;

        /* Blows should always be 1 ... even with Quickthorn and Shiva's Jacket! 
           But, don't make Tonberry gloves a gimme. Negative attacks now are 0 attacks!
        */
        if (info->base_blow + info->xtra_blow > 100)
        {
            info->base_blow = 100;
            info->xtra_blow = 0;
        }
    }
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
    skills_t xs = { 10,  10,  10,   0,   0,   0,  14,   0};

        me = plr_class_alloc(CLASS_DUELIST);
        me->name = "Duelist";
        me->desc = "The duelist is the ultimate one-on-one fighter, but finds himself at a severe "
                  "disadvantage when facing numerous strong foes. When facing an enemy, the duelist "
                  "first issues a challenge, marking that foe for a one on one duel. Of course, this "
                  "wakes the monster up (There is no honor in dueling a sleeping enemy). And while "
                  "the duelist will honor the fight as a one on one affair, many monsters have no "
                  "such scruples.\n \n"
                  "Against a challenged foe, the duelist is very strong gaining a bonus to saving "
                  "throws, armor class, damage reduction and combat prowess. On the other hand, due "
                  "to the single-mindedness of their focus, the duelist is quite vulnerable to unchallenged "
                  "opponents. Most of the techniques of this class aim at enforcing the sanctity of the "
                  "duel.\n \n"
                  "The duelist only ever gains a single attack in combat, but they make the most of this "
                  "blow by gaining enhanced effects as they gain experience. Able to wound, stun, and even "
                  "hamstring their foes, the prowess of the duelist in a one on one encounter is legendary!\n \n"
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
    }

    return me;
}

