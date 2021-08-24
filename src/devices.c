#include "angband.h"

#include <assert.h>

/* Devices: We are following the do_spell() pattern which is quick and dirty,
   but not my preferred approach ... */

/* Fail Rates ... Scaled by 10 (95.2% returned as 952)
 * cf design/devices.ods */
int _difficulty(int d)
{
    /* A non-linear difficulty protects the high level end game
     * devices from inappropriate usage. -Rockets are now much
     * harder to use (and _Healing only marginally more difficult).
     * Formerly, OF_MAGIC_MASTERY was useless to device classes
     * like the mage. No longer! 100 -> 130, but with cubic weighting.
     * Two versions: The first is more punishing than the second, depending
     * on the chosen cutoff. As usual, see design/devices.ods or ^A"2.
    return d + 30 * d * d / 100 * d / 10000; */
    int cutoff = 40;
    int xtra   = 30;
    if (d > cutoff)
    {
        int l = d - cutoff;
        int m = 100 - cutoff;
        assert(cutoff < 100);
        return d + xtra * l * l / m * l / (m * m);
    }
    return d;
}
/* in progress: I find this calculation hard to grok ... let's
 * rephrase in terms of skill vs. difficulty and expose a simple
 * api so I can view fail(s,d) (a function of 2 variables).
 * cf ^A"2 for online spoiler tables (wizard1.c) */
int device_calc_fail_rate_aux(int skill, int difficulty)
{
    int min = USE_DEVICE;
    int fail = 0;
    difficulty = _difficulty(difficulty);
    if (skill > difficulty) difficulty -= (skill - difficulty)*2;
    else skill -= (difficulty - skill)*2;
    if (difficulty < min) difficulty = min;
    if (skill < min) skill = min;
    if (skill > difficulty)
        fail = difficulty * 500 / skill;
    else
        fail = 1000 - skill * 500 / difficulty;
    return fail;
}

/* XXX design is sloppy atm ... I want devicemasters to have a skill
 * boost with their speciality device type (e.g. wands), and less
 * magic skills overall. But the "effect" api layer has no info about
 * the obj->tval ... so we'll need to pass that along. The "effect" layer
 * is public and is also used for equipment with activations. */
static int effect_calc_fail_rate_aux(effect_t *effect, int skill_boost)
{
    int skill = plr->skills.dev + skill_boost;
    int fail;

    if (plr_tim_find(T_CONFUSED)) skill = 3 * skill / 4;

    fail = device_calc_fail_rate_aux(skill, effect->difficulty);
    if (plr_tim_find(T_STUN))
    {
        fail += 500 * plr_tim_amount(T_STUN) / 100;
        if (fail > 950) fail = 950;
    }
    return fail;
}

int effect_calc_fail_rate(effect_t *effect)
{
    return effect_calc_fail_rate_aux(effect, 0);
}

int device_calc_fail_rate(object_type *o_ptr)
{
    int lev, chance, fail;

    if (o_ptr->activation.type)
    {
        effect_t effect = o_ptr->activation;
        int      skill_boost = 0;

        if (devicemaster_is_speciality(o_ptr))
            skill_boost = 5 + 3*plr->lev/5; /* 40+15 base = 115 -> 150 */

        if (obj_has_flag(o_ptr, OF_EASY_SPELL))
        {
            int d = effect.difficulty;

            d -= MAX(o_ptr->pval, effect.difficulty * 10 * o_ptr->pval / 300);
            if (d <= 1) d = 1;
            effect.difficulty = d;
        }

        if (o_ptr->curse_flags & OFC_CURSED)
            effect.difficulty += effect.difficulty / 5;

        return effect_calc_fail_rate_aux(&effect, skill_boost);
    }

    lev = k_info[o_ptr->k_idx].level;
    if (lev > 50) lev = 50 + (lev - 50)/2;
    chance = plr->skills.dev;
    if (plr_tim_find(T_CONFUSED)) chance = chance / 2;
    chance = chance - lev;
    if (chance < USE_DEVICE)
        fail = 1000 - 1000/(3 * (USE_DEVICE - chance + 1));
    else
        fail = (USE_DEVICE-1)*1000/chance;

    if (plr_tim_find(T_STUN))
    {
        fail += 500 * plr_tim_amount(T_STUN) / 100;
        if (fail > 950) fail = 950;
    }
    if (o_ptr->tval == TV_SCROLL && fail > 500) fail = 500;
    return fail;
}

/* Hack: When using an unknown rod we force the user to target. Also
   Trap Location should not spoil with the view_unsafe_grids option. */
bool device_known = FALSE;

/* Hack: Allow player to learn device power thru actual use. They can
 * also learn the fail rate (i.e. difficulty) by failing enough times,
 * but that is handled elsewhere. We deal solely with OFL_DEVICE_POWER. */
bool device_lore = FALSE;

/* Hack: When using an unknown device, was there an observable effect?
   If so, identify the device. */
bool device_noticed = FALSE;

/* Hack for Device Master's desperation. This power uses all the charges
   in a device at once (with diminishing returns) and potentially destroys
   the device as well. */
int  device_extra_power = 0;

/* Hack for identifying all relevant objects in a single action.
   It's ugly, but worthwhile! */
int  device_available_charges = 0; /* How many can we do? */
int  device_used_charges = 0;      /* How many did we do? */
static bool _use_charges = FALSE;

static void _do_identify_aux(obj_ptr obj)
{
    char name[MAX_NLEN];
    bool old_known;

    if (_use_charges && device_used_charges >= device_available_charges) return;

    old_known = identify_item(obj);
    object_desc(name, obj, OD_COLOR_CODED);
    switch (obj->loc.where)
    {
    case INV_EQUIP:
        msg_format("%^s: %s (%c).", equip_describe_slot(obj->loc.v.slot),
                name, slot_label(obj->loc.v.slot));
        break;
/* case INV_PACK:
        msg_format("In your pack: %s.", name);
        break;
    case INV_QUIVER:
        msg_format("In your quiver: %s.", name);
        break;*/
    case INV_PACK:
    case INV_QUIVER:
        obj->marked |= OM_DELAYED_MSG;
        plr->notice |= PN_CARRY;
        break;
    case INV_FLOOR:
        msg_format("On the ground: %s.", name);
        break;
    }
    autopick_alter_obj(obj, destroy_identify && !old_known);
    obj_release(obj, OBJ_RELEASE_ID | OBJ_RELEASE_QUIET);
    if (_use_charges) device_used_charges++;
}

void mass_identify(bool use_charges) /* shared with Sorcery spell */
{
    inv_ptr floor = inv_filter_floor(plr->pos, obj_exists);

    _use_charges = use_charges;
    pack_for_each_that(_do_identify_aux, obj_is_unknown);
    equip_for_each_that(_do_identify_aux, obj_is_unknown);
    quiver_for_each_that(_do_identify_aux, obj_is_unknown);
    inv_for_each_that(floor, _do_identify_aux, obj_is_unknown);

    inv_free(floor);
}

static int _cmd_handler(obj_prompt_context_ptr context, int cmd)
{
    if (cmd == '*')
        return OP_CMD_DISMISS;
    return OP_CMD_SKIPPED;
}

static bool _do_identify(void)
{
    obj_prompt_t prompt = {0};

    assert(device_used_charges == 0);

    prompt.prompt = "Identify which item <color:w>(<color:keypress>*</color> for all)</color>?";
    prompt.error = "All items are identified.";
    prompt.filter = obj_is_unknown;
    prompt.where[0] = INV_PACK;
    prompt.where[1] = INV_EQUIP;
    prompt.where[2] = INV_QUIVER;
    prompt.where[3] = INV_FLOOR;
    prompt.cmd_handler = _cmd_handler;

    switch (obj_prompt(&prompt))
    {
    case OP_CUSTOM:
        mass_identify(TRUE);
        return TRUE;
    case OP_SUCCESS:
        _use_charges = TRUE;
        _do_identify_aux(prompt.obj);
        return TRUE;
    }
    return FALSE;
}

/* Using Devices
      if (!device_try(o_ptr)) ... "You failed to use the device" ...
      if (device_use(o_ptr)) ... Decrement Charges/Unstack/Etc. ...
*/
bool device_try(object_type *o_ptr)
{
    int fail = device_calc_fail_rate(o_ptr);
    if (randint0(1000) < fail)
        return FALSE;
    return TRUE;
}

bool device_use(object_type *o_ptr, int boost)
{
    device_known = obj_is_known(o_ptr);
    if (do_device(o_ptr, SPELL_CAST, boost))
        return TRUE;
    return FALSE;
}

static int _scroll_power(int val)
{
    if (devicemaster_is_(DEVICEMASTER_SCROLLS))
    {
        val += val * device_extra_power / 100;
        return device_power_aux(val, /*plr->device_power + */plr->lev/10);
    }
    return val;
}

static int _potion_power(int val)
{
    if (devicemaster_is_(DEVICEMASTER_POTIONS))
    {
        val += val * device_extra_power / 100;
        return device_power_aux(val, /*plr->device_power + */plr->lev/10);
    }
    return val;
}

static cptr _do_potion(int sval, int mode)
{
    bool desc = (mode == SPELL_DESC) ? TRUE : FALSE;
    bool info = (mode == SPELL_INFO) ? TRUE : FALSE;
    bool cast = (mode == SPELL_CAST) ? TRUE : FALSE;

    switch (sval)
    {
    case SV_POTION_WATER:
        if (desc) return "It is just water.";
        if (cast)
        {
            msg_print("You feel less thirsty.");
            device_noticed = TRUE;
        }
        break;
    case SV_POTION_APPLE_JUICE:
        if (desc) return "It tastes sweet.";
        if (cast)
        {
            msg_print("You feel less thirsty.");
            device_noticed = TRUE;
        }
        break;
    case SV_POTION_SLIME_MOLD:
        if (desc) return "It tastes weird.";
        if (cast)
        {
            msg_print("You feel less thirsty.");
            device_noticed = TRUE;
        }
        break;
    case SV_POTION_SLOWNESS:
        if (desc) return "It slows you down temporarily when you quaff it.";
        if (cast)
        {
            if (plr_tim_add(T_SLOW, randint1(25) + 15))
                device_noticed = TRUE;
        }
        break;
    case SV_POTION_SALT_WATER:
        if (desc) return "It makes you nearly faint from hunger and paralyzes you, but it cures poison when you quaff it.";
        if (cast)
        {
            if ( !(get_race()->flags & RACE_IS_NONLIVING)
              && !prace_is_(RACE_MON_JELLY) )
            {
                msg_print("The potion makes you vomit!");
                set_food(PY_FOOD_STARVE - 1);
                plr_tim_add(T_PARALYZED, randint1(4));
                device_noticed = TRUE;
            }
        }
        break;
    case SV_POTION_POISON:
        if (desc) return "It poisons you when you quaff it.";
        if (cast)
        {
            if (!res_save_default(GF_POIS))
            {
                if (plr_tim_add(T_POISON, randint0(15) + 10))
                    device_noticed = TRUE;
            }
        }
        break;
    case SV_POTION_BLINDNESS:
        if (desc) return "It blinds you when you quaff it.";
        if (cast)
        {
            if (!res_save_default(GF_BLIND))
            {
                if (plr_tim_add(T_BLIND, randint0(100) + 100))
                    device_noticed = TRUE;
            }
        }
        break;
    case SV_POTION_CONFUSION: /* Booze */
        if (desc) return "It confuses and hallucinates you when you quaff it. If you are a monk, you may be a drunken master.";
        if (cast)
        {
            if (plr->pclass != CLASS_MONK)
                virtue_add(VIRTUE_HARMONY, -1);
            if (!res_save_default(GF_CONFUSION))
            {
                if (plr_tim_add(T_CONFUSED, randint0(20) + 15))
                    device_noticed = TRUE;
            }

            if (!res_save_default(GF_CHAOS))
            {
                if (one_in_(2) && !mut_present(MUT_WEIRD_MIND))
                {
                    if (plr_tim_add(T_HALLUCINATE, randint0(25) + 25))
                        device_noticed = TRUE;
                }
                if (one_in_(13) && (plr->pclass != CLASS_MONK))
                {
                    device_noticed = TRUE;
                    if (one_in_(3)) lose_all_info();
                    else wiz_dark();
                    teleport_player_aux(100, TELEPORT_NONMAGICAL | TELEPORT_PASSIVE);
                    wiz_dark();
                    msg_print("You wake up somewhere with a sore head...");
                    msg_print("You can't remember a thing, or how you got here!");
                }
            }
        }
        break;
    case SV_POTION_SLEEP:
        if (desc) return "It paralyzes you when you quaff it.";
        if (cast)
        {
            if (!free_act_save_p(0))
            {
                msg_print("You fall asleep.");
                if (plr_tim_add(T_PARALYZED, randint1(4)))
                {
                    device_noticed = TRUE;
                }
            }
        }
        break;
    case SV_POTION_LOSE_MEMORIES:
        if (desc) return "You lose experience when you quaff it.";
        if (cast)
        {
            if (!plr->hold_life && (plr->exp > 0))
            {
                msg_print("You feel your memories fade.");
                virtue_add(VIRTUE_KNOWLEDGE, -5);
                lose_exp(plr->exp / 4);
                device_noticed = TRUE;
            }
        }
        break;
    case SV_POTION_RUINATION:
        if (desc) return "You take damage and it decreases all your stats permanently when you quaff it.";
        if (cast)
        {
            msg_print("Your nerves and muscles feel weak and lifeless!");
            take_hit(DAMAGE_LOSELIFE, damroll(10, 10), "a potion of Ruination");

            dec_stat(A_DEX, 25, TRUE);
            dec_stat(A_WIS, 25, TRUE);
            dec_stat(A_CON, 25, TRUE);
            dec_stat(A_STR, 25, TRUE);
            dec_stat(A_CHR, 25, TRUE);
            dec_stat(A_INT, 25, TRUE);
            device_noticed = TRUE;
        }
        break;
    case SV_POTION_DEC_STR:
        if (desc) return "It decreases your strength when you quaff it.";
        if (cast)
        {
            if (do_dec_stat(A_STR)) device_noticed = TRUE;
        }
        break;
    case SV_POTION_DEC_INT:
        if (desc) return "It decreases your intelligence when you quaff it.";
        if (cast)
        {
            if (do_dec_stat(A_INT)) device_noticed = TRUE;
        }
        break;
    case SV_POTION_DEC_WIS:
        if (desc) return "It decreases your wisdom when you quaff it.";
        if (cast)
        {
            if (do_dec_stat(A_WIS)) device_noticed = TRUE;
        }
        break;
    case SV_POTION_DEC_DEX:
        if (desc) return "It decreases your dexterity when you quaff it.";
        if (cast)
        {
            if (do_dec_stat(A_DEX)) device_noticed = TRUE;
        }
        break;
    case SV_POTION_DEC_CON:
        if (desc) return "It decreases your constitution when you quaff it.";
        if (cast)
        {
            if (do_dec_stat(A_CON)) device_noticed = TRUE;
        }
        break;
    case SV_POTION_DEC_CHR:
        if (desc) return "It decreases your charisma when you quaff it.";
        if (cast)
        {
            if (do_dec_stat(A_CHR)) device_noticed = TRUE;
        }
        break;
    case SV_POTION_DETONATIONS:
        if (desc) return "It explodes in your mouth when you quaff it.";
        if (cast)
        {
            msg_print("Massive explosions rupture your body!");
            take_hit(DAMAGE_NOESCAPE, damroll(50, 20), "a potion of Detonation");

            if (_1d(100) > res_pct(GF_STUN))
                plr_tim_add(T_STUN, STUN_MASSIVE);
            if (!plr->no_cut) plr_tim_add(T_CUT, 5000);
            device_noticed = TRUE;
        }
        break;
    case SV_POTION_DEATH:
        if (desc) return "You die when you quaff it.";
        if (cast)
        {
            virtue_add(VIRTUE_VITALITY, -1);
            virtue_add(VIRTUE_UNLIFE, 5);
            msg_print("A feeling of Death flows through your body.");
            take_hit(DAMAGE_LOSELIFE, 5000, "a potion of Death");
            device_noticed = TRUE;
        }
        break;
    case SV_POTION_INFRAVISION:
        if (desc) return "It gives temporary infravision when you quaff it.";
        if (info) return info_duration(_potion_power(100), _potion_power(100));
        if (cast)
        {
            int dur = _potion_power(100 + randint1(100));
            if (plr_tim_add(T_INFRAVISION, dur))
                device_noticed = TRUE;
        }
        break;
    case SV_POTION_DETECT_INVIS:
        if (desc) return "It gives temporary see invisible when you quaff it.";
        if (info) return info_duration(_potion_power(12), _potion_power(12));
        if (cast)
        {
            int dur = _potion_power(12 + randint1(12));
            if (plr_tim_add(T_SEE_INVIS, dur))
                device_noticed = TRUE;
        }
        break;
    case SV_POTION_SLOW_POISON:
        if (desc) return "It reduces poison when you quaff it.";
        if (cast)
        {
            if (plr_tim_recover(T_POISON, 90, 10))
                device_noticed = TRUE;
        }
        break;
    case SV_POTION_CURE_POISON:
        if (desc) return "It cures poison a bit when you quaff it.";
        if (cast)
        {
            if (plr_tim_recover(T_POISON, 80, 50))
                device_noticed = TRUE;
        }
        break;
    case SV_POTION_BOLDNESS:
        if (desc) return "It removes fear when you quaff it.";
        if (cast)
        {
            if (plr->afraid)
            {
                fear_clear_p();
                device_noticed = TRUE;
            }
        }
        break;
    case SV_POTION_SPEED:
        if (desc) return "It hastes you temporarily when you quaff it.";
        if (info) return format("Dur d%d+%d", _potion_power(25), _potion_power(15));
        if (cast)
        {
            int dur = _potion_power(randint1(25) + 15);
            if (plr_tim_add(T_FAST, dur)) device_noticed = TRUE;
        }
        break;
    case SV_POTION_RESIST_HEAT:
        if (desc) return "You get temporary resistance to fire when you quaff it. This resistance is cumulative with equipment.";
        if (info) return format("Dur d%d+%d", _potion_power(10), _potion_power(10));
        if (cast)
        {
            int dur = _potion_power(10 + randint1(10));
            if (plr_tim_add(T_RES_FIRE, dur))
                device_noticed = TRUE;
        }
        break;
    case SV_POTION_RESIST_COLD:
        if (desc) return "You get temporary resistance to cold when you quaff it. This resistance is cumulative with equipment.";
        if (info) return format("Dur d%d+%d", _potion_power(10), _potion_power(10));
        if (cast)
        {
            int dur = _potion_power(10 + randint1(10));
            if (plr_tim_add(T_RES_COLD, dur))
                device_noticed = TRUE;
        }
        break;
    case SV_POTION_HEROISM:
        if (desc) return "It removes fear and causes you temporary heroism when you quaff it.";
        if (info) return format("Dur d%d+%d", _potion_power(25), _potion_power(25));
        if (cast)
        {
            int dur = _potion_power(25 + randint1(25));
            if (plr_tim_add(T_HERO, dur)) device_noticed = TRUE;
        }
        break;
    case SV_POTION_BERSERK_STRENGTH:
        if (desc) return "It removes fear and causes you to go berserk when you quaff it.";
        if (info) return format("Dur d%d+%d", _potion_power(25), _potion_power(25));
        if (cast)
        {
            int dur = _potion_power(25 + randint1(25));
            if (plr_tim_add(T_BERSERK, dur)) device_noticed = TRUE;
            if (hp_player(30)) device_noticed = TRUE;
        }
        break;
    case SV_POTION_CURE_LIGHT:
        if (desc) return "It heals you trivially, cures blindness and reduces cuts when you quaff it.";
        if (info) return info_heal(2, _potion_power(8), 0);
        if (cast)
        {
            if (hp_player(_potion_power(damroll(2, 8)))) device_noticed = TRUE;
            if (plr_tim_remove(T_BLIND)) device_noticed = TRUE;
            plr_tim_subtract(T_CUT, 10);
        }
        break;
    case SV_POTION_CURE_SERIOUS:
        if (desc) return "It heals you a bit, cures blindness, confusion and reduces cuts when you quaff it.";
        if (info) return info_heal(4, _potion_power(8), 0);
        if (cast)
        {
            if (hp_player(_potion_power(damroll(4, 8)))) device_noticed = TRUE;
            if (plr_tim_remove(T_BLIND)) device_noticed = TRUE;
            if (plr_tim_remove(T_CONFUSED)) device_noticed = TRUE;
            if (plr_tim_recover(T_CUT, 50, 0)) device_noticed = TRUE;
            plr_tim_subtract(T_CUT, 50);
        }
        break;
    case SV_POTION_CURE_CRITICAL:
        if (desc) return "It heals you a bit and cures blindness, confusion, stunned, and cuts when you quaff it.";
        if (info) return info_heal(6, _potion_power(8), 0);
        if (cast)
        {
            if (hp_player(_potion_power(damroll(6, 8)))) device_noticed = TRUE;
            if (plr_tim_remove(T_BLIND)) device_noticed = TRUE;
            if (plr_tim_remove(T_CONFUSED)) device_noticed = TRUE;
            if (plr_tim_remove(T_STUN)) device_noticed = TRUE;
            if (plr_tim_remove(T_CUT)) device_noticed = TRUE;
        }
        break;
    case SV_POTION_BLOOD:
        if (desc) return "A much needed infusion! It heals you a bit and cures blindness, confusion, and stunned when you quaff it.";
        if (info) return info_heal(0, 0, _potion_power(200));
        if (cast)
        {
            if (hp_player(_potion_power(200))) device_noticed = TRUE;
            if (plr_tim_remove(T_BLIND)) device_noticed = TRUE;
            if (plr_tim_remove(T_CONFUSED)) device_noticed = TRUE;
            if (plr_tim_remove(T_STUN)) device_noticed = TRUE;
        }
        break;
    case SV_POTION_HEALING: {
        int amt = 300;
        if (desc) return "It heals you and cures blindness, confusion, stunned, and cuts when you quaff it.";
        if (info) return info_heal(0, 0, _potion_power(amt));
        if (cast)
        {
            if (hp_player(_potion_power(amt))) device_noticed = TRUE;
            if (plr_tim_remove(T_BLIND)) device_noticed = TRUE;
            if (plr_tim_remove(T_CONFUSED)) device_noticed = TRUE;
            if (plr_tim_remove(T_STUN)) device_noticed = TRUE;
            if (plr_tim_remove(T_CUT)) device_noticed = TRUE;
        }
        break; }
    case SV_POTION_STAR_HEALING:
        if (desc) return "It heals you and cures blindness, confusion, poison, stunned, and cuts when you quaff it.";
        if (info) return info_heal(0, 0, _potion_power(1000));
        if (cast)
        {
            if (hp_player(_potion_power(1000))) device_noticed = TRUE;
            if (plr_tim_remove(T_BLIND)) device_noticed = TRUE;
            if (plr_tim_remove(T_CONFUSED)) device_noticed = TRUE;
            if (plr_tim_remove(T_POISON)) device_noticed = TRUE;
            if (plr_tim_remove(T_STUN)) device_noticed = TRUE;
            if (plr_tim_remove(T_CUT)) device_noticed = TRUE;
        }
        break;
    case SV_POTION_LIFE:
        if (desc) return "It heals you completely, restores life, experience and all your stats and cures blindness, confusion, poison, hallucination, stunned, and cuts when you quaff it.";
        if (info) return info_heal(0, 0, _potion_power(5000));
        if (cast)
        {
            virtue_add(VIRTUE_VITALITY, 1);
            virtue_add(VIRTUE_UNLIFE, -5);
            msg_print("You feel life flow through your body!");
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
            update_stuff(); /* hp may change if Con was drained ... */
            hp_player(_potion_power(5000));
            device_noticed = TRUE;
        }
        break;
    case SV_POTION_CLARITY:
        if (desc) return "It clears your mind a bit when you quaff it.";
        if (info) return format("5d%d + %d", _potion_power(6), _potion_power(5));
        if (cast)
        {
            int amt = _potion_power(damroll(5, 6) + 5);

            if (plr->pclass == CLASS_RUNE_KNIGHT)
                msg_print("You are unaffected.");
            else if (sp_player(amt))
                device_noticed = TRUE;
            if (plr_tim_remove(T_BERSERK)) device_noticed = TRUE;
            if (plr_tim_remove(T_STUN)) device_noticed = TRUE;
            if (device_noticed)
                msg_print("You feel your mind clear.");
        }
        break;
    case SV_POTION_GREAT_CLARITY:
        if (desc) return "It greatly clears your mind when you quaff it.";
        if (info) return format("10d%d + %d", _potion_power(10), _potion_power(15));
        if (cast)
        {
            int amt = _potion_power(damroll(10, 10) + 15);

            if (plr->pclass == CLASS_RUNE_KNIGHT)
                msg_print("You are unaffected.");
            else if (sp_player(amt))
                device_noticed = TRUE;
            if (plr_tim_remove(T_BERSERK)) device_noticed = TRUE;
            if (plr_tim_remove(T_STUN)) device_noticed = TRUE;
            if (device_noticed)
                msg_print("You feel your mind clear.");
        }
        break;
    case SV_POTION_RESTORE_MANA:
        if (desc) return "It restores mana to full and cures berserk when you quaff it. It also partially recharges any devices in your pack.";
        if (cast)
        {
            if (restore_mana()) device_noticed = TRUE;
            if (plr_tim_remove(T_BERSERK)) device_noticed = TRUE;
        }
        break;
    case SV_POTION_RESTORE_EXP:
        if (desc) return "It restores your life and experience when you quaff it.";
        if (cast)
        {
            if (restore_level()) device_noticed = TRUE;
            if (plr_restore_life(150)) device_noticed = TRUE;
        }
        break;
    case SV_POTION_RES_STR:
        if (desc) return "It restores your strength when you quaff it.";
        if (cast)
        {
            if (do_res_stat(A_STR)) device_noticed = TRUE;
        }
        break;
    case SV_POTION_RES_INT:
        if (desc) return "It restores your intelligence when you quaff it.";
        if (cast)
        {
            if (do_res_stat(A_INT)) device_noticed = TRUE;
        }
        break;
    case SV_POTION_RES_WIS:
        if (desc) return "It restores your wisdom when you quaff it.";
        if (cast)
        {
            if (do_res_stat(A_WIS)) device_noticed = TRUE;
        }
        break;
    case SV_POTION_RES_DEX:
        if (desc) return "It restores your dexterity when you quaff it.";
        if (cast)
        {
            if (do_res_stat(A_DEX)) device_noticed = TRUE;
        }
        break;
    case SV_POTION_RES_CON:
        if (desc) return "It restores your constitution when you quaff it.";
        if (cast)
        {
            if (do_res_stat(A_CON)) device_noticed = TRUE;
        }
        break;
    case SV_POTION_RES_CHR:
        if (desc) return "It restores your charisma when you quaff it.";
        if (cast)
        {
            if (do_res_stat(A_CHR)) device_noticed = TRUE;
        }
        break;
    case SV_POTION_INC_STR:
        if (desc) return "It increases your strength when you quaff it.";
        if (cast)
        {
            if (do_inc_stat(A_STR)) device_noticed = TRUE;
        }
        break;
    case SV_POTION_INC_INT:
        if (desc) return "It increases your intelligence when you quaff it.";
        if (cast)
        {
            if (do_inc_stat(A_INT)) device_noticed = TRUE;
        }
        break;
    case SV_POTION_INC_WIS:
        if (desc) return "It increases your wisdom when you quaff it.";
        if (cast)
        {
            if (do_inc_stat(A_WIS)) device_noticed = TRUE;
        }
        break;
    case SV_POTION_INC_DEX:
        if (desc) return "It increases your dexterity when you quaff it.";
        if (cast)
        {
            if (do_inc_stat(A_DEX)) device_noticed = TRUE;
        }
        break;
    case SV_POTION_INC_CON:
        if (desc) return "It increases your constitution when you quaff it.";
        if (cast)
        {
            if (do_inc_stat(A_CON)) device_noticed = TRUE;
        }
        break;
    case SV_POTION_INC_CHR:
        if (desc) return "It increases your charisma when you quaff it.";
        if (cast)
        {
            if (do_inc_stat(A_CHR)) device_noticed = TRUE;
        }
        break;
    case SV_POTION_AUGMENTATION:
        if (desc) return "It increases all your stats when you quaff it.";
        if (cast)
        {
            if (do_inc_stat(A_STR)) device_noticed = TRUE;
            if (do_inc_stat(A_INT)) device_noticed = TRUE;
            if (do_inc_stat(A_WIS)) device_noticed = TRUE;
            if (do_inc_stat(A_DEX)) device_noticed = TRUE;
            if (do_inc_stat(A_CON)) device_noticed = TRUE;
            if (do_inc_stat(A_CHR)) device_noticed = TRUE;
        }
        break;
    case SV_POTION_ENLIGHTENMENT:
        if (desc) return "It maps, lights permanently and detects all items on the entire level when you quaff it.";
        if (cast)
        {
            virtue_add(VIRTUE_KNOWLEDGE, 1);
            virtue_add(VIRTUE_ENLIGHTENMENT, 1);
            msg_print("An image of your surroundings forms in your mind...");
            wiz_lite();
            device_noticed = TRUE;
        }
        break;
    case SV_POTION_STAR_ENLIGHTENMENT:
        if (desc) return "It maps, lights permanently and detects all items on the entire level, increases your intelligence and wisdom, detects all traps, doors, stairs, treasures in your vicinity, identifies all items in pack and gives information about yourself when you quaff it.";
        if (cast)
        {
            msg_print("You begin to feel more enlightened...");
            virtue_add(VIRTUE_KNOWLEDGE, 1);
            virtue_add(VIRTUE_ENLIGHTENMENT, 2);
            msg_print(NULL);
            wiz_lite();
            do_inc_stat(A_INT);
            do_inc_stat(A_WIS);
            detect_traps(DETECT_RAD_DEFAULT, TRUE);
            detect_doors(DETECT_RAD_DEFAULT);
            detect_stairs(DETECT_RAD_DEFAULT);
            detect_recall(DETECT_RAD_DEFAULT);
            detect_treasure(DETECT_RAD_DEFAULT);
            detect_objects_gold(DETECT_RAD_DEFAULT);
            detect_objects_normal(DETECT_RAD_DEFAULT);
            identify_pack();
            self_knowledge();
            device_noticed = TRUE;
        }
        break;
    case SV_POTION_SELF_KNOWLEDGE:
        if (desc) return "It gives information about yourself when you quaff it.";
        if (cast)
        {
            msg_print("You begin to know yourself a little better...");
            msg_print(NULL);
            self_knowledge();
            device_noticed = TRUE;
        }
        break;
    case SV_POTION_EXPERIENCE:
        if (desc) return "You become more experienced when you quaff it.";
        if (cast)
        {
            if (plr->prace == RACE_ANDROID) break;
            virtue_add(VIRTUE_ENLIGHTENMENT, 1);
            if (plr->exp < PY_MAX_EXP)
            {
                s32b ee = _potion_power((plr->exp / 2) + 10);
                s32b max = _potion_power(100000);
                if (mut_present(MUT_FAST_LEARNER))
                {
                    ee = ee * 5/3;
                    max = max * 5/3;
                }
                if (ee > max) ee = max;
                msg_print("You feel more experienced.");
                gain_exp(ee);
                device_noticed = TRUE;
            }
        }
        break;
    case SV_POTION_RESISTANCE:
        if (desc) return "You get temporary resistance to the elements and poison when you quaff it. ";
        if (info) return format("Dur d%d+%d", _potion_power(20), _potion_power(20));
        if (cast)
        {
            int dur = _potion_power(20 + randint1(20));
            plr_tim_add(T_RES_ACID, dur);
            plr_tim_add(T_RES_ELEC, dur);
            plr_tim_add(T_RES_FIRE, dur);
            plr_tim_add(T_RES_COLD, dur);
            plr_tim_add(T_RES_POIS, dur);
            device_noticed = TRUE;
        }
        break;
    case SV_POTION_CURING: {
        int amt = 50;
        if (desc) return "It heals you a bit and cures blindness, poison, confusion, stunning, cuts and hallucination when you quaff it.";
        if (info) return info_heal(0, 0, _potion_power(amt));
        if (cast)
        {
            if (hp_player(_potion_power(amt))) device_noticed = TRUE;
            if (plr_tim_remove(T_BLIND)) device_noticed = TRUE;
            if (plr_tim_recover(T_POISON, 65, 150)) device_noticed = TRUE;
            if (plr_tim_remove(T_CONFUSED)) device_noticed = TRUE;
            if (plr_tim_remove(T_STUN)) device_noticed = TRUE;
            if (plr_tim_remove(T_CUT)) device_noticed = TRUE;
            if (plr_tim_remove(T_HALLUCINATE)) device_noticed = TRUE;
            if (plr_tim_remove(T_BERSERK)) device_noticed = TRUE;
        }
        break; }
    case SV_POTION_CURE_MUTATION:
        if (desc) return "It cures a single mutation when quaffed.";
        if (cast)
        {
            if (mut_lose_random(NULL)) device_noticed = TRUE;
        }
        break;
    case SV_POTION_INVULNERABILITY:
        if (desc) return "You become invulnerable temporarily when you quaff it.";
        if (info) return format("Dur d%d+%d", _potion_power(7), _potion_power(7));
        if (cast)
        {
            int dur = _potion_power(500 + _1d(1000));
            if (plr_tim_add(T_INVULN, dur)) device_noticed = TRUE;
        }
        break;
    case SV_POTION_NEW_LIFE:
        if (desc) return "It changes your life rating and max of all your stats and cures all mutations when you quaff it.";
        if (cast)
        {
            do_cmd_rerate(FALSE);
            plr_restore_life(1000);
            get_max_stats();
            plr->update |= PU_BONUS;
            mut_lose_all();
            device_noticed = TRUE;
            if (plr->pclass == CLASS_WILD_TALENT)
                wild_talent_new_life();
        }
        break;
    case SV_POTION_GIANT_STRENGTH:
        if (desc) return "It greatly increases your stature temporarily when you quaff it.";
        if (info) return format("Dur d%d+%d", _potion_power(20), _potion_power(20));
        if (cast)
        {
            if (plr_tim_add(T_GIANT_STRENGTH, 20 + randint1(20)))
                device_noticed = TRUE;
        }
        break;
    case SV_POTION_POLYMORPH:
        if (desc) return "It mutates you when you quaff it. Rarely it cures all mutations.";
        if (cast)
        {
            int count = mut_count(mut_unlocked_pred);
            if (count > 1 && one_in_(23))
            {
                mut_lose_all();
                if (plr->pclass == CLASS_WILD_TALENT)
                    wild_talent_new_life();
            }
            else
            {
                do
                {
                    if (one_in_(2))
                    {
                        if(mut_gain_random(NULL))
                        {
                            count++;
                            device_noticed = TRUE;
                        }
                    }
                    else if (count > 5 || one_in_(6 - count))
                    {
                        if (mut_lose_random(NULL))
                        {
                            count--;
                            device_noticed = TRUE;
                        }
                    }
                } while (!device_noticed || one_in_(2));

                if (plr->pclass == CLASS_WILD_TALENT && one_in_(2))
                    wild_talent_scramble();
            }
        }
        break;
    case SV_POTION_STONE_SKIN:
        if (desc) return "It temporarily turns your skin to stone, granting enhanced armor class, when you quaff it.";
        if (info) return format("Dur d%d+%d", _potion_power(20), _potion_power(20));
        if (cast)
        {
            if (plr_tim_add(T_STONE_SKIN, _potion_power(20 + randint1(20))))
                device_noticed = TRUE;
        }
        break;
    }
    return "";
}

static cptr _do_scroll(int sval, int mode)
{
    bool desc = (mode == SPELL_DESC) ? TRUE : FALSE;
    bool info = (mode == SPELL_INFO) ? TRUE : FALSE;
    bool cast = (mode == SPELL_CAST) ? TRUE : FALSE;

    switch (sval)
    {
    case SV_SCROLL_DARKNESS:
        if (desc) return "It darkens nearby area or current room and blinds you when you read it.";
        if (cast)
        {
            if (!res_save_default(GF_BLIND) && !res_save_default(GF_DARK))
            {
                if (plr_tim_add(T_BLIND, 3 + randint1(5))) device_noticed = TRUE;
            }
            if (unlite_area(10, 3)) device_noticed = TRUE;
        }
        break;
    case SV_SCROLL_AGGRAVATE_MONSTER:
        if (desc) return "It aggravates monsters in your vicinity when you read it.";
        if (cast)
        {
            msg_print("There is a high pitched humming noise.");
            aggravate_monsters(who_create_null());
            device_noticed = TRUE;
        }
        break;
    case SV_SCROLL_CURSE_ARMOR:
        if (desc) return "It makes your current armour (Blasted) when you read it.";
        if (cast)
        {
            int slot = equip_random_slot(obj_is_armor);
            if (slot && curse_armor(slot)) device_noticed = TRUE;
        }
        break;
    case SV_SCROLL_CURSE_WEAPON:
        if (desc) return "It makes your wielding weapon (Shattered) when you read it.";
        if (cast)
        {
            int slot = equip_random_slot(obj_is_weapon);
            if (slot && curse_weapon(FALSE, slot)) device_noticed = TRUE;
        }
        break;
    case SV_SCROLL_SUMMON_MONSTER:
        if (desc) return "It summons several monsters as enemies when you read it.";
        if (cast)
        {
            int i;
            for (i = 0; i < randint1(3); i++)
            {
                if (summon_specific(who_create_null(), plr->pos, cave->dun_lvl, 0, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_NO_PET)))
                    device_noticed = TRUE;
            }
        }
        break;
    case SV_SCROLL_SUMMON_UNDEAD:
        if (desc) return "It summons several undead monsters as enemies when you read it.";
        if (cast)
        {
            int i;
            for (i = 0; i < randint1(3); i++)
            {
                if (summon_specific(who_create_null(), plr->pos, cave->dun_lvl, SUMMON_UNDEAD, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_NO_PET)))
                    device_noticed = TRUE;
            }
        }
        break;
    case SV_SCROLL_SUMMON_PET:
        if (desc)
        {
            if (plr->prace == RACE_MON_RING)
                return "It summons a ring bearer as your pet when you read it.";
            else
                return "It summons a monster as your pet when you read it.";
        }
        if (cast)
        {
            int type = 0;
            if (plr->prace == RACE_MON_RING)
                type = SUMMON_RING_BEARER;
            if (summon_specific(who_create_plr(), plr->pos, _scroll_power(cave->dun_lvl), type, (PM_ALLOW_GROUP | PM_FORCE_PET)))
                device_noticed = TRUE;
        }
        break;
    case SV_SCROLL_SUMMON_KIN:
        if (desc) return "It summons a monster corresponds to your race as your pet when you read it.";
        if (cast)
        {
            if (summon_kin_player(_scroll_power(plr->lev), plr->pos, (PM_FORCE_PET | PM_ALLOW_GROUP)))
                device_noticed = TRUE;
        }
        break;
    case SV_SCROLL_TRAP_CREATION:
        if (desc) return "It creates traps on the squares adjacent to you when you read it.";
        if (cast)
        {
            if (trap_creation(plr->pos))
                device_noticed = TRUE;
        }
        break;
    case SV_SCROLL_PHASE_DOOR:
        if (desc) return "It teleports you a short distance when you read it.";
        if (cast)
        {
            teleport_player(10, 0L);
            if (mut_present(MUT_ASTRAL_GUIDE))
                energy_use = energy_use / 3;
            device_noticed = TRUE;
        }
        break;
    case SV_SCROLL_TELEPORT:
        if (desc) return "It teleports you a random distance when you read it.";
        if (cast)
        {
            int rng = rand_range(10, 100);
            teleport_player(rng, 0);
            if (mut_present(MUT_ASTRAL_GUIDE))
                energy_use = energy_use / 3;
            device_noticed = TRUE;
        }
        break;
    case SV_SCROLL_TELEPORT_LEVEL:
        if (desc) return "It teleports you one dungeon level up or down immediately when you read it.";
        if (cast)
        {
            dun_teleport_level_plr(cave);
            device_noticed = TRUE;
        }
        break;
    case SV_SCROLL_WORD_OF_RECALL:
        if (desc) return "It recalls you to the town, or back into the dungeon you have entered when you read it.";
        if (cast)
        {
            device_noticed = TRUE;
            if (!dun_mgr_recall_plr()) return NULL;
        }
        break;
    case SV_SCROLL_IDENTIFY:
        if (desc) return "It identifies an item when you read it.";
        if (cast)
        {
            device_noticed = TRUE;
            if (!_do_identify()) return NULL;
        }
        break;
    case SV_SCROLL_STAR_IDENTIFY:
        if (desc) return "It reveals all information about an item when you read it.";
        if (cast)
        {
            device_noticed = TRUE;
            if (!identify_fully(NULL)) return NULL;
        }
        break;
    case SV_SCROLL_REMOVE_CURSE:
        if (desc) return "It removes normal curses from equipped items when you read it.";
        if (cast)
        {
            if (remove_curse())
            {
                msg_print("You feel as if someone is watching over you.");
                device_noticed = TRUE;
            }
        }
        break;
    case SV_SCROLL_STAR_REMOVE_CURSE:
        if (desc) return "It removes normal and heavy curses from equipped items when you read it.";
        if (cast)
        {
            if (remove_all_curse())
                msg_print("You feel as if someone is watching over you.");
            device_noticed = TRUE;
        }
        break;
    case SV_SCROLL_ENCHANT_ARMOR:
        if (desc) return "It increases an armour's to-AC when you read it.";
        if (cast)
        {
            if (!enchant_spell(0, 0, 1)) return NULL;
            device_noticed = TRUE;
        }
        break;
    case SV_SCROLL_ENCHANT_WEAPON_TO_HIT:
        if (desc) return "It increases a weapon's to-hit when you read it.";
        if (cast)
        {
            if (!enchant_spell(1, 0, 0)) return NULL;
            device_noticed = TRUE;
        }
        break;
    case SV_SCROLL_ENCHANT_WEAPON_TO_DAM:
        if (desc) return "It increases a weapon's to-dam when you read it.";
        if (cast)
        {
            if (!enchant_spell(0, 1, 0)) return NULL;
            device_noticed = TRUE;
        }
        break;
    case SV_SCROLL_STAR_ENCHANT_ARMOR:
        if (desc) return "It increases an armour's to-ac powerfully when you read it.";
        if (cast)
        {
            if (!enchant_spell(0, 0, randint1(3) + 2)) return NULL;
            device_noticed = TRUE;
        }
        break;
    case SV_SCROLL_STAR_ENCHANT_WEAPON:
        if (desc) return "It increases a weapon's to-hit and to-dam when you read it.";
        if (cast)
        {
            if (!enchant_spell(randint1(3), randint1(3), 0)) return NULL;
            device_noticed = TRUE;
        }
        break;
    case SV_SCROLL_RECHARGING:
        if (desc) return "It attempts to recharge a magical device using the mana of a source device. This usually destroys the source device.";
        if (cast)
        {
            if (!recharge_from_device(_scroll_power(100))) return NULL;
            device_noticed = TRUE;
        }
        break;
    case SV_SCROLL_MUNDANITY:
        if (desc) return "This removes the ego or artifact status and all enchantment of an item. As a bonus, if you have a stack of them, the extras are destroyed.";
        if (cast)
        {
            if (!mundane_spell(FALSE)) return NULL;
            device_noticed = TRUE;
        }
        break;
    case SV_SCROLL_LIGHT:
        if (desc) return "It lights up nearby area or the current room permanently when you read it.";
        if (cast)
        {
            if (lite_area(damroll(2, 8), 2)) device_noticed = TRUE;
        }
        break;
    case SV_SCROLL_MAPPING:
        if (desc) return "It maps your vicinity when you read it.";
        if (cast)
        {
            map_area(_scroll_power(DETECT_RAD_MAP));
            device_noticed = TRUE;
        }
        break;
    case SV_SCROLL_DETECT_GOLD:
        if (desc) return "It detects all treasures in your vicinity when you read it.";
        if (cast)
        {
            if (detect_treasure(_scroll_power(DETECT_RAD_DEFAULT))) device_noticed = TRUE;
            if (detect_objects_gold(_scroll_power(DETECT_RAD_DEFAULT))) device_noticed = TRUE;
        }
        break;
    case SV_SCROLL_DETECT_ITEM:
        if (desc) return "It detects all items in your vicinity when you read it.";
        if (cast)
        {
            if (detect_objects_normal(_scroll_power(DETECT_RAD_DEFAULT))) device_noticed = TRUE;
        }
        break;
    case SV_SCROLL_DETECT_TRAP:
        if (desc) return "It detects all traps in your vicinity when you read it.";
        if (cast)
        {
            if (detect_traps(_scroll_power(DETECT_RAD_DEFAULT), device_known)) device_noticed = TRUE;
        }
        break;
    case SV_SCROLL_DETECT_DOOR:
        if (desc) return "It detects all doors and stairs in your vicinity when you read it.";
        if (cast)
        {
            if (detect_doors(_scroll_power(DETECT_RAD_DEFAULT))) device_noticed = TRUE;
            if (detect_stairs(_scroll_power(DETECT_RAD_DEFAULT))) device_noticed = TRUE;
            if (detect_recall(_scroll_power(DETECT_RAD_DEFAULT))) device_noticed = TRUE;
        }
        break;
    case SV_SCROLL_DETECT_INVIS:
        if (desc) return "It detects all invisible monsters in your vicinity when you read it.";
        if (cast)
        {
            if (detect_monsters_invis(_scroll_power(DETECT_RAD_DEFAULT))) device_noticed = TRUE;
        }
        break;
    case SV_SCROLL_DETECT_MONSTERS:
        if (desc) return "It detects all monsters in your vicinity when you read it.";
        if (cast)
        {
            if (detect_monsters_normal(_scroll_power(DETECT_RAD_DEFAULT))) device_noticed = TRUE;
        }
        break;
    case SV_SCROLL_SATISFY_HUNGER:
        if (desc) return "It satisfies hunger when you read it.";
        if (cast)
        {
            if (set_food(PY_FOOD_MAX - 1)) device_noticed = TRUE;
        }
        break;
    case SV_SCROLL_BLESSING:
        if (desc) return "It blesses you temporarily when you read it.";
        if (cast)
        {
            if (plr_tim_add(T_BLESSED, _scroll_power(randint1(12) + 6))) device_noticed = TRUE;
        }
        break;
    case SV_SCROLL_HOLY_CHANT:
        if (desc) return "It blesses you temporarily when you read it.";
        if (cast)
        {
            if (plr_tim_add(T_BLESSED, _scroll_power(randint1(24) + 12))) device_noticed = TRUE;
        }
        break;
    case SV_SCROLL_HOLY_PRAYER:
        if (desc) return "It blesses you temporarily when you read it.";
        if (cast)
        {
            if (plr_tim_add(T_BLESSED, _scroll_power(randint1(48) + 24))) device_noticed = TRUE;
        }
        break;
    case SV_SCROLL_MONSTER_CONFUSION:
        if (desc) return "You can confuse monster you hit just for once when you read it.";
        if (cast)
        {
            if (!(plr->special_attack & ATTACK_CONFUSE))
            {
                msg_print("Your hands begin to glow.");
                plr->special_attack |= ATTACK_CONFUSE;
                plr->redraw |= (PR_STATUS);
                device_noticed = TRUE;
            }
        }
        break;
    case SV_SCROLL_PROTECTION_FROM_EVIL:
        if (desc) return "It gives temporary protection from lesser evil creatures when you read it.";
        if (cast)
        {
            if (plr_tim_add(T_PROT_EVIL, _scroll_power(randint1(25) + 3 * plr->lev)))
                device_noticed = TRUE;
        }
        break;
    case SV_SCROLL_RUNE_OF_PROTECTION:
        if (desc) return "It creates a glyph on the floor you stand when you read it.";
        if (cast)
        {
            warding_glyph();
            device_noticed = TRUE;
        }
        break;
    case SV_SCROLL_TRAP_DOOR_DESTRUCTION:
        if (desc) return "It destroys traps on the floors adjacent to you when you read it.";
        if (cast)
        {
            if (destroy_doors_touch()) device_noticed = TRUE;
        }
        break;
    case SV_SCROLL_STAR_DESTRUCTION:
        if (desc) return "It destroys everything nearby you when you read it.";
        if (cast)
        {
            if (destroy_area(plr->pos, 13 + randint0(5), _scroll_power(2000)))
                device_noticed = TRUE;
            else
                msg_print("The dungeon trembles...");
        }
        break;
    case SV_SCROLL_DISPEL_UNDEAD:
        if (desc) return "It damages all undead monsters in sight when you read it.";
        if (info) return info_damage(0, 0, _scroll_power(80));
        if (cast)
        {
            if (plr_project_los(GF_DISP_UNDEAD, _scroll_power(80)))
                device_noticed = TRUE;
        }
        break;
    case SV_SCROLL_SPELL:
        if (desc) return "It increases the number you can study spells when you read. If you are the class can't study or don't need to study, it has no effect.";
        if (cast)
        {
            if (plr->pclass == CLASS_WARRIOR ||
                plr->pclass == CLASS_MINDCRAFTER ||
                plr->pclass == CLASS_PSION ||
                plr->pclass == CLASS_SORCERER ||
                plr->pclass == CLASS_ARCHER ||
                plr->pclass == CLASS_MAGIC_EATER ||
                plr->pclass == CLASS_DEVICEMASTER ||
                plr->pclass == CLASS_RED_MAGE ||
                plr->pclass == CLASS_SAMURAI ||
                plr->pclass == CLASS_CAVALRY ||
                plr->pclass == CLASS_WEAPONSMITH ||
                plr->pclass == CLASS_MIRROR_MASTER ||
                plr->pclass == CLASS_TIME_LORD ||
                plr->pclass == CLASS_BLOOD_KNIGHT ||
                plr->pclass == CLASS_WARLOCK ||
                plr->pclass == CLASS_ARCHAEOLOGIST ||
                plr->pclass == CLASS_DUELIST ||
                plr->pclass == CLASS_RUNE_KNIGHT ||
                plr->pclass == CLASS_WILD_TALENT ||
                plr->pclass == CLASS_NINJA ||
                plr->pclass == CLASS_SCOUT ||
                plr->pclass == CLASS_MYSTIC ||
                plr->pclass == CLASS_MAULER ||
                plr->pclass == CLASS_SKILLMASTER )
            {
                msg_print("There is no effect.");
            }
            else
            {
                plr->add_spells++;
                plr->update |= (PU_SPELLS);
            }
            device_noticed = TRUE;
        }
        break;
    case SV_SCROLL_GENOCIDE:
        if (desc) return "It eliminates an entire class of monster, exhausting you. Powerful or unique monsters may resist.";
        if (cast)
        {
            if (!symbol_genocide(_scroll_power(300), TRUE)) return NULL;
            device_noticed = TRUE;
        }
        break;
    case SV_SCROLL_MASS_GENOCIDE:
        if (desc) return "It eliminates all nearby monsters, exhausting you. Powerful or unique monsters may be able to resist.";
        if (cast)
        {
            mass_genocide(_scroll_power(300), TRUE);
            device_noticed = TRUE;
        }
        break;
    case SV_SCROLL_ACQUIREMENT:
        if (desc) return "It creates one great item when you read it.";
        if (cast)
        {
            acquirement(plr->pos.y, plr->pos.x, 1, TRUE, FALSE);
            device_noticed = TRUE;
        }
        break;
    case SV_SCROLL_STAR_ACQUIREMENT:
        if (desc) return "It creates some great items when you read it.";
        if (cast)
        {
            acquirement(plr->pos.y, plr->pos.x, _scroll_power(randint1(2) + 1), TRUE, FALSE);
            device_noticed = TRUE;
        }
        break;
    case SV_SCROLL_FOREST_CREATION:
        if (desc) return "It surrounds you with verdure.";
        if (cast)
        {
            if (tree_creation()) device_noticed = TRUE;
        }
        break;
    case SV_SCROLL_WALL_CREATION:
        if (desc) return "It surrounds you with rock.";
        if (cast)
        {
            if (wall_stone()) device_noticed = TRUE;
        }
        break;
    case SV_SCROLL_VENGEANCE:
        if (desc) return "For a short time, monsters that attack you receive an equal amount of damage in retaliation.";
        if (cast)
        {
            plr_tim_add(T_REVENGE, _scroll_power(randint1(25) + 25));
            device_noticed = TRUE;
        }
        break;
    case SV_SCROLL_RUMOR:
        if (desc) return "A rumor is in it.";
        if (cast)
        {
            char Rumor[1024];
            errr err = get_rnd_line("rumors.txt", 0, Rumor);

            if (err) strcpy(Rumor, "Some rumors are wrong.");
            msg_format("<color:B>There is message on the scroll. It says:</color> %s", Rumor);
            msg_print("The scroll disappears in a puff of smoke!");
            device_noticed = TRUE;
        }
        break;
    case SV_SCROLL_ARTIFACT:
        if (desc) return "It creates an artifact from a nameless weapon or armour when you read it. Don't be greedy - you will get only one artifact.";
        if (cast)
        {
            if (no_artifacts)
            {
                if (!brand_weapon(-1)) return NULL;
            }
            else
            {
                if (!artifact_scroll()) return NULL;
            }
            device_noticed = TRUE;
            /* XXX do_device() resets this and is *not* re-entrant (i.e. globals = evil)
             * art_create_random will obj_display, using do_device for info on obj->activation.
             * So we must set *after* calling artifact_scroll() ... XXX */
        }
        break;
    case SV_SCROLL_RUSTPROOF:
        if (desc) return "It makes a chosen equipment item immune to the corrosive effects of acid.";
        if (cast)
        {
            device_noticed = TRUE;
            if (!rustproof())
                return NULL;
        }
        break;
    case SV_SCROLL_CRAFTING:
        if (desc) return "It makes a chosen weapon, armor or ammo an ego item when you read it.";
        if (cast)
        {
            device_noticed = TRUE;
            if (!cast_crafting())
                return NULL;
        }
        break;
    case SV_SCROLL_RESET_RECALL:
        if (desc) return "It resets the dungeon level for recall spell when you read it.";
        if (cast)
        {
            device_noticed = TRUE;
            if (!reset_recall()) return NULL;
        }
        break;
    case SV_SCROLL_FIRE:
        if (desc) return "It creates a huge fire ball centered on you.";
        if (info) return info_damage(0, 0, _scroll_power(333));
        if (cast)
        {
            device_noticed = TRUE;
            plr_burst(4, GF_FIRE, _scroll_power(333));
            if (!devicemaster_is_(DEVICEMASTER_SCROLLS) && !res_save_default(GF_FIRE))
            {
                int dam = res_calc_dam(GF_FIRE, 25 + randint1(25));
                take_hit(DAMAGE_NOESCAPE, dam, "a Scroll of Fire");
            }
        }
        break;
    case SV_SCROLL_ICE:
        if (desc) return "It creates a huge ice ball centered on you.";
        if (info) return info_damage(0, 0, _scroll_power(400));
        if (cast)
        {
            device_noticed = TRUE;
            plr_burst(4, GF_ICE, _scroll_power(400));
            if (!devicemaster_is_(DEVICEMASTER_SCROLLS) && !res_save_default(GF_COLD))
            {
                int dam = res_calc_dam(GF_COLD, 30 + randint1(30));
                take_hit(DAMAGE_NOESCAPE, dam, "a Scroll of Ice");
            }
        }
        break;
    case SV_SCROLL_CHAOS:
        if (desc) return "It creates a huge ball of logrus centered on you.";
        if (info) return info_damage(0, 0, _scroll_power(500));
        if (cast)
        {
            device_noticed = TRUE;
            plr_burst(4, GF_CHAOS, _scroll_power(500));
            if (!devicemaster_is_(DEVICEMASTER_SCROLLS) && !res_save_default(GF_CHAOS))
            {
                int dam = res_calc_dam(GF_CHAOS, 50 + randint1(50));
                take_hit(DAMAGE_NOESCAPE, dam, "a Scroll of Logrus");
            }
        }
        break;
    case SV_SCROLL_MANA:
        if (desc) return "It creates a huge ball of pure mana centered on you.";
        if (info) return info_damage(0, 0, _scroll_power(550));
        if (cast)
        {
            device_noticed = TRUE;
            plr_burst(4, GF_MANA, _scroll_power(550));
            if (!devicemaster_is_(DEVICEMASTER_SCROLLS))
                take_hit(DAMAGE_NOESCAPE, 50 + randint1(50), "a Scroll of Mana");
        }
        break;
    case SV_SCROLL_BANISHMENT:
        if (desc) return "It teleports all monsters in sight away unless resisted.";
        if (info) return info_power(_scroll_power(150));
        if (cast)
        {
            if (plr_project_los(GF_TELEPORT, _scroll_power(150)))
                device_noticed = TRUE;
        }
        break;
    }
    return "";
}

cptr do_device(object_type *o_ptr, int mode, int boost)
{
    cptr result = NULL;

    device_noticed = FALSE;
    device_used_charges = 0;
    device_lore = FALSE;

    if (o_ptr->activation.type)
    {
        if (obj_has_flag(o_ptr, OF_DEVICE_POWER))
            boost += device_power_aux(100, o_ptr->pval) - 100;

        result = do_effect(&o_ptr->activation, mode, boost);
    }
    else
    {
        switch (o_ptr->tval)
        {
        case TV_SCROLL: result = _do_scroll(o_ptr->sval, mode); break;
        case TV_POTION: result = _do_potion(o_ptr->sval, mode); break;
        }
    }
    device_known = FALSE;
    device_extra_power = 0;
    device_available_charges = 0;
    return result;
}

/************************************************************************
 * Effects
 ************************************************************************/
effect_t obj_get_effect(object_type *o_ptr)
{
    if (o_ptr->activation.type)
        return o_ptr->activation;
    if (o_ptr->art_id)
    {
        art_ptr art = arts_lookup(o_ptr->art_id);
        assert(art);
        if (art->activation.type)
            return art->activation;
    }
    if (o_ptr->name2 && e_info[o_ptr->name2].activation.type)
        return e_info[o_ptr->name2].activation;
    return k_info[o_ptr->k_idx].activation;
}

cptr obj_get_effect_msg(object_type *o_ptr)
{
    if (o_ptr->activation.type)
        return NULL;

    if (o_ptr->art_id)
    {
        art_ptr art = arts_lookup(o_ptr->art_id);
        assert(art);
        if (art->activation.type)
            return art->activation_msg;
    }
    if (o_ptr->name2 && e_info[o_ptr->name2].activation.type)
    {
        return NULL;
    }

    return k_info[o_ptr->k_idx].activation_msg;
}

bool obj_has_effect(object_type *o_ptr)
{
    effect_t e = obj_get_effect(o_ptr);
    if (e.type)
        return TRUE;
    return FALSE;
}

bool effect_try(effect_t *effect)
{
    int fail = effect_calc_fail_rate(effect);
    if (randint0(1000) < fail)
        return FALSE;
    return TRUE;
}

bool effect_use(effect_t *effect, int boost)
{
    device_noticed = FALSE;
    device_used_charges = 0;
    device_available_charges = 1;
    if (do_effect(effect, SPELL_CAST, boost))
        return TRUE;
    return FALSE;
}

int effect_value(effect_t *effect)
{
    int  result = 0;
    cptr hack = do_effect(effect, SPELL_VALUE, 0);
    if (hack)
        result = atoi(hack);
    return result;
}

int effect_cost_extra(effect_t *effect)
{
    int  result = 0;
    cptr hack = do_effect(effect, SPELL_COST_EXTRA, 0);
    if (hack)
        result = atoi(hack);
    return result;
}

byte effect_color(effect_t *effect)
{
    byte result = TERM_WHITE;
    cptr hack = do_effect(effect, SPELL_COLOR, 0);
    if (hack && strlen(hack))
        result = atoi(hack);
    return result;
}

typedef struct
{
    cptr text;
    int  type;
    int  level;
    int  cost;
    int  rarity;
    int  bias;
    bool known;
} _effect_info_t, *_effect_info_ptr;

/*  Allocation Table for Random Artifact Activations
    This also assists parsing a_info.txt, k_info.txt and e_info.txt.
    Order is irrelevant. Use Rarity 0 to exclude allocations.
*/
static _effect_info_t _effect_info[] =
{
    /* Detection:                                   Lv    T   R  Bias */
    {"LIGHT_AREA",      EFFECT_LIGHT_AREA,           1,  10,  1, BIAS_MAGE},
    {"LIGHT_MAP_AREA",  EFFECT_LIGHT_MAP_AREA,      20,  50,  3, 0},
    {"ENLIGHTENMENT",   EFFECT_ENLIGHTENMENT,       20,  50,  2, BIAS_PRIESTLY | BIAS_ARCHER},
    {"CLAIRVOYANCE",    EFFECT_CLAIRVOYANCE,        35, 100,  8, BIAS_MAGE},

    {"DETECT_TRAPS",    EFFECT_DETECT_TRAPS,         3,  10,  1, BIAS_ROGUE},
    {"DETECT_MONSTERS", EFFECT_DETECT_MONSTERS,      5,  20,  1, BIAS_MAGE | BIAS_ARCHER},
    {"DETECT_OBJECTS",  EFFECT_DETECT_OBJECTS,       8,  25,  1, BIAS_ROGUE},
    {"DETECT_ALL",      EFFECT_DETECT_ALL,          25,  50,  3, BIAS_ROGUE},
    {"DETECT_GOLD",     EFFECT_DETECT_GOLD,          5,  20,  1, BIAS_ROGUE},
    {"DETECT_INVISIBLE",EFFECT_DETECT_INVISIBLE,     5,  20,  1, 0},
    {"DETECT_DOOR_STAIRS",EFFECT_DETECT_DOOR_STAIRS,10,  25,  1, 0},
    {"DETECT_EVIL",     EFFECT_DETECT_EVIL,         20,  30,  1, BIAS_PRIESTLY},

    /* Utility:                                     Lv    T   R  Bias */
    {"PHASE_DOOR",      EFFECT_PHASE_DOOR,           8,  20,  1, BIAS_MAGE | BIAS_ARCHER},
    {"TELEPORT",        EFFECT_TELEPORT,            12,  20,  1, BIAS_ROGUE},
    {"TELEPORT_AWAY",   EFFECT_TELEPORT_AWAY,       20,  50,  2, BIAS_MAGE},
    {"STRAFING",        EFFECT_STRAFING,            20,  10,  3, BIAS_ARCHER},
    {"DIMENSION_DOOR",  EFFECT_DIMENSION_DOOR,      50, 100,  8, BIAS_MAGE | BIAS_ARCHER},
    {"ESCAPE",          EFFECT_ESCAPE,              30,  70,  3, BIAS_ROGUE},
    {"RECALL",          EFFECT_RECALL,              25, 100,  2, BIAS_MAGE},

    {"STONE_TO_MUD",    EFFECT_STONE_TO_MUD,        15,  20,  1, BIAS_STR | BIAS_RANGER},
    {"EARTHQUAKE",      EFFECT_EARTHQUAKE,          25, 100,  1, BIAS_STR},
    {"DESTRUCTION",     EFFECT_DESTRUCTION,         50, 250,  6, BIAS_DEMON},
    {"GENOCIDE",        EFFECT_GENOCIDE,            70, 500, 16, BIAS_NECROMANTIC},
    {"MASS_GENOCIDE",   EFFECT_MASS_GENOCIDE,       80, 750, 16, BIAS_NECROMANTIC},

    {"RECHARGE_FROM_DEVICE", EFFECT_RECHARGE_FROM_DEVICE, 35, 500,  3, BIAS_MAGE | BIAS_DEMON},
    {"RECHARGE_FROM_PLAYER", EFFECT_RECHARGE_FROM_PLAYER, 90, 500, 16, BIAS_MAGE | BIAS_DEMON},

    {"ENCHANTMENT",     EFFECT_ENCHANTMENT,         30, 900, 16, 0},
    {"IDENTIFY",        EFFECT_IDENTIFY,            15,  50,  1, BIAS_ROGUE | BIAS_MAGE},
    {"IDENTIFY_FULL",   EFFECT_IDENTIFY_FULL,       50, 200,  3, BIAS_ROGUE | BIAS_MAGE},
    {"PROBING",         EFFECT_PROBING,             30,  50,  1, BIAS_MAGE},
    {"RUNE_EXPLOSIVE",  EFFECT_RUNE_EXPLOSIVE,      30, 100,  2, BIAS_MAGE},
    {"RUNE_PROTECTION", EFFECT_RUNE_PROTECTION,     70, 500,  4, BIAS_PRIESTLY},

    {"SATISFY_HUNGER",  EFFECT_SATISFY_HUNGER,       5, 100,  1, BIAS_RANGER},
    {"DESTROY_TRAP",    EFFECT_DESTROY_TRAP,        20,  50,  1, 0},
    {"DESTROY_TRAPS",   EFFECT_DESTROY_TRAPS,       25,  50,  1, BIAS_ROGUE},
    {"WHIRLWIND_ATTACK",EFFECT_WHIRLWIND_ATTACK,    50, 100,  4, BIAS_WARRIOR},
    {"LIST_UNIQUES",    EFFECT_LIST_UNIQUES,        80, 250,  8, 0},
    {"LIST_ARTIFACTS",  EFFECT_LIST_ARTIFACTS,      80, 250,  8, 0},
    {"BANISH_EVIL",     EFFECT_BANISH_EVIL,         50, 100,  4, BIAS_PRIESTLY | BIAS_LAW},
    {"BANISH_ALL",      EFFECT_BANISH_ALL,          50, 100,  8, BIAS_MAGE},
    {"TELEKINESIS",     EFFECT_TELEKINESIS,         25, 100,  2, BIAS_MAGE},
    {"ALCHEMY",         EFFECT_ALCHEMY,             70, 100,  4, BIAS_MAGE},
    {"SELF_KNOWLEDGE",  EFFECT_SELF_KNOWLEDGE,      70, 500,  3, BIAS_MAGE},
    {"GENOCIDE_ONE",    EFFECT_GENOCIDE_ONE,        60, 250,  8, BIAS_NECROMANTIC},

    /* Timed Buffs:                                 Lv    T   R  Bias */
    {"STONE_SKIN",      EFFECT_STONE_SKIN,          25, 150,  2, BIAS_WARRIOR | BIAS_PROTECTION},
    {"RESIST_ACID",     EFFECT_RESIST_ACID,         15, 100,  1, BIAS_ACID | BIAS_PROTECTION},
    {"RESIST_ELEC",     EFFECT_RESIST_ELEC,         15, 100,  1, BIAS_ELEC | BIAS_PROTECTION},
    {"RESIST_FIRE",     EFFECT_RESIST_FIRE,         15, 100,  1, BIAS_FIRE | BIAS_DEMON | BIAS_PROTECTION},
    {"RESIST_COLD",     EFFECT_RESIST_COLD,         15, 100,  1, BIAS_COLD | BIAS_PROTECTION},
    {"RESIST_POIS",     EFFECT_RESIST_POIS,         30, 150,  2, BIAS_POIS | BIAS_PROTECTION},
    {"RESISTANCE",      EFFECT_RESISTANCE,          35, 200,  4, BIAS_RANGER | BIAS_PROTECTION},
    {"PROT_EVIL",       EFFECT_PROT_EVIL,           35, 200,  2, BIAS_PRIESTLY | BIAS_LAW | BIAS_PROTECTION},
    {"HOLY_GRAIL",      EFFECT_HOLY_GRAIL,          50, 500,  4, BIAS_PRIESTLY},
    {"BLESS",           EFFECT_BLESS,               10, 100,  1, BIAS_PRIESTLY | BIAS_DEMON},
    {"HEROISM",         EFFECT_HEROISM,             15, 100,  1, BIAS_WARRIOR | BIAS_PRIESTLY | BIAS_DEMON},
    {"BERSERK",         EFFECT_BERSERK,             20, 100,  2, BIAS_WARRIOR},
    {"SPEED",           EFFECT_SPEED,               25, 150,  4, BIAS_ROGUE | BIAS_MAGE | BIAS_ARCHER},
    {"SPEED_HERO",      EFFECT_SPEED_HERO,          35, 200,  6, BIAS_WARRIOR},
    {"SPEED_HERO_BLESS",EFFECT_SPEED_HERO_BLESS,    40, 250,  8, 0},
    {"LIGHT_SPEED",     EFFECT_LIGHT_SPEED,         99, 999, 99, 0},
    {"ENLARGE_WEAPON",  EFFECT_ENLARGE_WEAPON,      80, 900,  0, 0},
    {"TELEPATHY",       EFFECT_TELEPATHY,           30, 150,  8, BIAS_MAGE | BIAS_ARCHER},
    {"WRAITHFORM",      EFFECT_WRAITHFORM,          90, 666, 16, BIAS_NECROMANTIC},
    {"INVULNERABILITY", EFFECT_INVULNERABILITY,     90, 777, 16, BIAS_MAGE | BIAS_PROTECTION},

    /* Pets:                                        Lv    T   R  Bias */
    {"SUMMON_MONSTERS", EFFECT_SUMMON_MONSTERS,     30, 500,  1, BIAS_MAGE},
    {"SUMMON_HOUNDS",   EFFECT_SUMMON_HOUNDS,       35, 500,  1, BIAS_RANGER},
    {"SUMMON_ANTS",     EFFECT_SUMMON_ANTS,         20, 200,  1, BIAS_RANGER},
    {"SUMMON_HYDRAS",   EFFECT_SUMMON_HYDRAS,       40, 500,  1, BIAS_RANGER},
    {"SUMMON_OCTOPUS",  EFFECT_SUMMON_OCTOPUS,      30, 600, 16, 0},
    {"SUMMON_DAWN",     EFFECT_SUMMON_DAWN,         60, 888, 16, 0},
    {"SUMMON_PHANTASMAL",EFFECT_SUMMON_PHANTASMAL,  25, 150,  1, BIAS_MAGE},
    {"SUMMON_ELEMENTAL",EFFECT_SUMMON_ELEMENTAL,    30, 150,  1, BIAS_MAGE},
    {"SUMMON_DRAGON",   EFFECT_SUMMON_DRAGON,       60, 600,  2, BIAS_MAGE},
    {"SUMMON_UNDEAD",   EFFECT_SUMMON_UNDEAD,       60, 600,  2, BIAS_NECROMANTIC},
    {"SUMMON_DEMON",    EFFECT_SUMMON_DEMON,        66, 666,  2, BIAS_CHAOS | BIAS_DEMON},
    {"SUMMON_CYBERDEMON",EFFECT_SUMMON_CYBERDEMON,  90, 900, 16, BIAS_CHAOS},
    {"SUMMON_ANGEL",    EFFECT_SUMMON_ANGEL,        70, 777,  8, BIAS_PRIESTLY},
    {"SUMMON_KRAKEN",   EFFECT_SUMMON_KRAKEN,       90, 900,  8, 0},

    {"CHARM_ANIMAL",    EFFECT_CHARM_ANIMAL,        30, 200,  1, BIAS_RANGER},
    {"CHARM_DEMON",     EFFECT_CHARM_DEMON,         50, 500,  1, BIAS_CHAOS | BIAS_DEMON},
    {"CHARM_UNDEAD",    EFFECT_CHARM_UNDEAD,        50, 500,  1, BIAS_NECROMANTIC},
    {"CHARM_MONSTER",   EFFECT_CHARM_MONSTER,       30, 100,  1, BIAS_MAGE},

    {"RETURN_PETS",     EFFECT_RETURN_PETS,         10,   0,  0, 0},
    {"CAPTURE_PET",     EFFECT_CAPTURE_PET,         20,   0,  0, 0},

    /* Healing and Recovery:                        Lv    T   R  Bias */
    {"RESTORE_STATS",   EFFECT_RESTORE_STATS,       50, 600,  2, BIAS_PRIESTLY},
    {"RESTORE_EXP",     EFFECT_RESTORE_EXP,         40, 500,  1, BIAS_PRIESTLY},
    {"RESTORING",       EFFECT_RESTORING,           70, 800,  3, BIAS_PRIESTLY},
    {"HEAL",            EFFECT_HEAL,                40,  50,  1, BIAS_PRIESTLY},  /* weak: "Cure Wounds" */
    {"CURING",          EFFECT_CURING,              45,  70,  1, BIAS_PRIESTLY},
    {"HEAL_CURING",     EFFECT_HEAL_CURING,         60, 200,  4, BIAS_PRIESTLY},
    {"HEAL_CURING_HERO",EFFECT_HEAL_CURING_HERO,    70, 200,  4, BIAS_PRIESTLY},
    {"RESTORE_MANA",    EFFECT_RESTORE_MANA,        80, 900,  8, BIAS_MAGE},
    {"CURE_POIS",       EFFECT_CURE_POIS,           10,  25,  1, BIAS_POIS | BIAS_PRIESTLY | BIAS_RANGER},
    {"CURE_FEAR",       EFFECT_CURE_FEAR,           25,  25,  1, BIAS_PRIESTLY},
    {"CURE_FEAR_POIS",  EFFECT_CURE_FEAR_POIS,      30,  25,  1, BIAS_PRIESTLY},
    {"REMOVE_CURSE",    EFFECT_REMOVE_CURSE,        30, 200,  1, BIAS_PRIESTLY},
    {"REMOVE_ALL_CURSE",EFFECT_REMOVE_ALL_CURSE,    70, 500,  4, BIAS_PRIESTLY},
    {"CLARITY",         EFFECT_CLARITY,             20,  15, 12, BIAS_PRIESTLY | BIAS_MAGE},
    {"GREAT_CLARITY",   EFFECT_GREAT_CLARITY,       80,  75, 64, BIAS_PRIESTLY | BIAS_MAGE},

    /* Offense: Bolts                               Lv    T   R  Bias */
    {"BOLT_MISSILE",    EFFECT_BOLT_MISSILE,         1,  10,  1, BIAS_MAGE | BIAS_ARCHER},
    {"BOLT_ACID",       EFFECT_BOLT_ACID,           15,  20,  1, BIAS_ACID},
    {"BOLT_ELEC",       EFFECT_BOLT_ELEC,           15,  20,  1, BIAS_ELEC},
    {"BOLT_FIRE",       EFFECT_BOLT_FIRE,           15,  20,  1, BIAS_FIRE | BIAS_DEMON},
    {"BOLT_COLD",       EFFECT_BOLT_COLD,           15,  20,  1, BIAS_COLD},
    {"BOLT_POIS",       EFFECT_BOLT_POIS,           10,  10,  1, BIAS_POIS},
    {"BOLT_LIGHT",      EFFECT_BOLT_LIGHT,          20,  25,  1, 0},
    {"BOLT_DARK",       EFFECT_BOLT_DARK,           20,  25,  1, BIAS_NECROMANTIC},
    {"BOLT_CONF",       EFFECT_BOLT_CONF,           30,  50,  2, 0},
    {"BOLT_NETHER",     EFFECT_BOLT_NETHER,         20,  25,  1, BIAS_NECROMANTIC | BIAS_DEMON},
    {"BOLT_NEXUS",      EFFECT_BOLT_NEXUS,          20,  25,  2, 0},
    {"BOLT_SOUND",      EFFECT_BOLT_SOUND,          40,  50,  2, BIAS_LAW},
    {"BOLT_SHARDS",     EFFECT_BOLT_SHARDS,         40,  50,  2, BIAS_LAW},
    {"BOLT_CHAOS",      EFFECT_BOLT_CHAOS,          50, 100,  2, BIAS_CHAOS},
    {"BOLT_DISEN",      EFFECT_BOLT_DISEN,          40,  50,  2, 0},
    {"BOLT_TIME",       EFFECT_BOLT_TIME,           60, 200, 90, 0},
    {"BOLT_WATER",      EFFECT_BOLT_WATER,          55, 150,  4, BIAS_MAGE},
    {"BOLT_MANA",       EFFECT_BOLT_MANA,           50, 100,  4, BIAS_MAGE},
    {"BOLT_ICE",        EFFECT_BOLT_ICE,            50, 100,  4, BIAS_MAGE | BIAS_COLD},
    {"BOLT_PLASMA",     EFFECT_BOLT_PLASMA,         50, 100,  4, BIAS_FIRE},

    /* Offense: Beams                               Lv    T   R  Bias */
    {"BEAM_LIGHT_WEAK", EFFECT_BEAM_LIGHT_WEAK,     10,  20,  1, 0},
    {"BEAM_LIGHT",      EFFECT_BEAM_LIGHT,          40,  30,  2, 0},
    {"BEAM_GRAVITY",    EFFECT_BEAM_GRAVITY,        50,  80,  8, 0},
    {"BEAM_DISINTEGRATE",EFFECT_BEAM_DISINTEGRATE,  60,  60, 16, 0},
    {"BEAM_ACID",       EFFECT_BEAM_ACID,           20,  20,  2, BIAS_ACID},
    {"BEAM_ELEC",       EFFECT_BEAM_ELEC,           20,  20,  2, BIAS_ELEC},
    {"BEAM_FIRE",       EFFECT_BEAM_FIRE,           20,  20,  2, BIAS_FIRE | BIAS_DEMON},
    {"BEAM_COLD",       EFFECT_BEAM_COLD,           20,  20,  2, BIAS_COLD},
    {"BEAM_SOUND",      EFFECT_BEAM_SOUND,          45,  30,  3, BIAS_LAW},
    {"BEAM_CHAOS",      EFFECT_BEAM_CHAOS,          55,  50,  3, BIAS_CHAOS},

    /* Offense: Balls                               Lv    T   R  Bias */
    {"BALL_ACID",       EFFECT_BALL_ACID,           25,  50,  1, BIAS_ACID},
    {"BALL_ELEC",       EFFECT_BALL_ELEC,           25,  50,  1, BIAS_ELEC},
    {"BALL_FIRE",       EFFECT_BALL_FIRE,           25,  50,  1, BIAS_FIRE | BIAS_DEMON},
    {"BALL_COLD",       EFFECT_BALL_COLD,           25,  50,  1, BIAS_COLD},
    {"BALL_POIS",       EFFECT_BALL_POIS,           10,   5,  1, BIAS_POIS},
    {"BALL_LIGHT",      EFFECT_BALL_LIGHT,          65, 100,  2, 0},
    {"BALL_DARK",       EFFECT_BALL_DARK,           66, 100,  2, BIAS_NECROMANTIC},
    {"BALL_CONF",       EFFECT_BALL_CONF,           50, 150,  2, 0},
    {"BALL_NETHER",     EFFECT_BALL_NETHER,         40,  50,  2, BIAS_NECROMANTIC | BIAS_DEMON},
    {"BALL_NEXUS",      EFFECT_BALL_NEXUS,          50, 100,  2, 0},
    {"BALL_SOUND",      EFFECT_BALL_SOUND,          60, 100,  2, BIAS_LAW},
    {"BALL_SHARDS",     EFFECT_BALL_SHARDS,         60, 100,  2, BIAS_LAW},
    {"BALL_CHAOS",      EFFECT_BALL_CHAOS,          65, 150,  4, BIAS_CHAOS},
    {"BALL_DISEN",      EFFECT_BALL_DISEN,          55, 100,  2, 0},
    {"BALL_TIME",       EFFECT_BALL_TIME,           80, 250, 32, 0},
    {"BALL_WATER",      EFFECT_BALL_WATER,          70, 200,  4, BIAS_MAGE},
    {"BALL_MANA",       EFFECT_BALL_MANA,           80, 200,  6, BIAS_MAGE},
    {"BALL_DISINTEGRATE", EFFECT_BALL_DISINTEGRATE, 60, 200, 16, 0},

    /* Offense: Breaths                             Lv    T   R  Bias */
    {"BREATHE_ACID",    EFFECT_BREATHE_ACID,        40, 100,  2, BIAS_ACID},
    {"BREATHE_ELEC",    EFFECT_BREATHE_ELEC,        40, 100,  2, BIAS_ELEC},
    {"BREATHE_FIRE",    EFFECT_BREATHE_FIRE,        40, 100,  2, BIAS_FIRE},
    {"BREATHE_COLD",    EFFECT_BREATHE_COLD,        40, 100,  2, BIAS_COLD},
    {"BREATHE_POIS",    EFFECT_BREATHE_POIS,        40, 100,  2, BIAS_POIS},
    {"BREATHE_LIGHT",   EFFECT_BREATHE_LIGHT,       50, 125,  3, 0},
    {"BREATHE_DARK",    EFFECT_BREATHE_DARK,        50, 125,  3, BIAS_NECROMANTIC},
    {"BREATHE_CONF",    EFFECT_BREATHE_CONF,        60, 200,  4, 0},
    {"BREATHE_NETHER",  EFFECT_BREATHE_NETHER,      50,  75,  2, BIAS_NECROMANTIC | BIAS_DEMON},
    {"BREATHE_NEXUS",   EFFECT_BREATHE_NEXUS,       60, 150,  4, 0},
    {"BREATHE_SOUND",   EFFECT_BREATHE_SOUND,       70, 200,  4, BIAS_LAW},
    {"BREATHE_SHARDS",  EFFECT_BREATHE_SHARDS,      70, 200,  4, BIAS_LAW},
    {"BREATHE_CHAOS",   EFFECT_BREATHE_CHAOS,       75, 250,  4, BIAS_CHAOS},
    {"BREATHE_DISEN",   EFFECT_BREATHE_DISEN,       60, 150,  8, 0},
    {"BREATHE_TIME",    EFFECT_BREATHE_TIME,        90, 500, 32, 0},
    {"BREATHE_ELEMENTS", EFFECT_BREATHE_ELEMENTS,   60, 100, 64, 0},
    {"BREATHE_HOLY_FIRE", EFFECT_BREATHE_HOLY_FIRE, 80, 100, 64, BIAS_LAW},
    {"BREATHE_HELL_FIRE", EFFECT_BREATHE_HELL_FIRE, 80, 100, 64, BIAS_DEMON},

    {"BREATHE_ONE_MULTIHUED", EFFECT_BREATHE_ONE_MULTIHUED, 0, 0, 0, 0},
    {"BREATHE_ONE_CHAOS",EFFECT_BREATHE_ONE_CHAOS,   0, 0, 0, 0},
    {"BREATHE_ONE_LAW", EFFECT_BREATHE_ONE_LAW,      0, 0, 0, 0},
    {"BREATHE_ONE_BALANCE",EFFECT_BREATHE_ONE_BALANCE, 0, 0, 0, 0},
    {"BREATHE_ONE_SHINING",EFFECT_BREATHE_ONE_SHINING, 0, 0, 0, 0},

    /* Offense: Other                               Lv    T   R  Bias */
    {"DISPEL_EVIL",     EFFECT_DISPEL_EVIL,         50, 200,  2, BIAS_PRIESTLY | BIAS_LAW},
    {"DISPEL_EVIL_HERO",EFFECT_DISPEL_EVIL_HERO,    60, 250,  3, BIAS_PRIESTLY},
    {"DISPEL_GOOD",     EFFECT_DISPEL_GOOD,         50, 150,  1, BIAS_NECROMANTIC},
    {"DISPEL_LIFE",     EFFECT_DISPEL_LIFE,         55, 200,  1, BIAS_NECROMANTIC},
    {"DISPEL_DEMON",    EFFECT_DISPEL_DEMON,        60, 200,  2, BIAS_LAW},
    {"DISPEL_UNDEAD",   EFFECT_DISPEL_UNDEAD,       60, 200,  2, BIAS_PRIESTLY},
    {"DISPEL_MONSTERS", EFFECT_DISPEL_MONSTERS,     70, 250,  8, 0},
    {"DRAIN_LIFE",      EFFECT_DRAIN_LIFE,          40, 100,  2, BIAS_NECROMANTIC},
    {"STAR_BALL",       EFFECT_STAR_BALL,           80, 900, 64, BIAS_LAW},
    {"ROCKET",          EFFECT_ROCKET,              70, 200,  8, BIAS_DEMON},
    {"MANA_STORM",      EFFECT_MANA_STORM,          80, 250,  8, BIAS_MAGE},
    {"CONFUSING_LIGHT",  EFFECT_CONFUSING_LIGHT,    60, 100,  6, BIAS_CHAOS},
    {"ARROW",           EFFECT_ARROW,               30, 100,  2, BIAS_RANGER | BIAS_ARCHER},
    {"WRATH_OF_GOD",    EFFECT_WRATH_OF_GOD,        80, 250, 32, BIAS_LAW},
    {"METEOR",          EFFECT_METEOR,              55, 150,  8, 0},
    {"HOLINESS",        EFFECT_HOLINESS,            50, 250,  8, BIAS_PRIESTLY | BIAS_LAW},
    {"STARBURST",       EFFECT_STARBURST,           70, 500, 32, BIAS_LAW},
    {"DARKNESS_STORM",  EFFECT_DARKNESS_STORM,      70, 500, 32, BIAS_NECROMANTIC},
    {"PESTICIDE",       EFFECT_PESTICIDE,           10,   5,  8, 0},

    /* Misc                                         Lv    T   R  Bias */
    {"POLY_SELF",       EFFECT_POLY_SELF,           20, 500,  1, BIAS_CHAOS},
    {"ANIMATE_DEAD",    EFFECT_ANIMATE_DEAD,        25, 100,  1, BIAS_NECROMANTIC},
    {"SCARE_MONSTERS",  EFFECT_SCARE_MONSTERS,      20, 100,  1, BIAS_NECROMANTIC},
    {"SLEEP_MONSTERS",  EFFECT_SLEEP_MONSTERS,      25, 100,  1, BIAS_ROGUE},
    {"SLOW_MONSTERS",   EFFECT_SLOW_MONSTERS,       25, 100,  1, BIAS_RANGER},
    {"STASIS_MONSTERS", EFFECT_STASIS_MONSTERS,     50, 250,  8, BIAS_MAGE},
    {"CONFUSE_MONSTERS",EFFECT_CONFUSE_MONSTERS,    25, 100,  1, BIAS_CHAOS},
    {"PIERCING_SHOT",   EFFECT_PIERCING_SHOT,       30, 100,  0, BIAS_ARCHER},
    {"CHARGE",          EFFECT_CHARGE,              15, 100,  0, 0},
    {"WALL_BUILDING",   EFFECT_WALL_BUILDING,       90, 750,  0, 0},
    {"SLEEP_MONSTER",   EFFECT_SLEEP_MONSTER,        5, 100,  1, 0},
    {"SLOW_MONSTER",    EFFECT_SLOW_MONSTER,         5, 100,  1, 0},
    {"CONFUSE_MONSTER", EFFECT_CONFUSE_MONSTER,      5, 100,  1, 0},
    {"SCARE_MONSTER",   EFFECT_SCARE_MONSTER,       10, 100,  1, 0},
    {"POLYMORPH",       EFFECT_POLYMORPH,           15, 100,  2, BIAS_CHAOS},
    {"STARLIGHT",       EFFECT_STARLIGHT,           20, 100,  2, 0},
    {"NOTHING",         EFFECT_NOTHING,              1,   1,  0, 0},
    {"ENDLESS_QUIVER",  EFFECT_ENDLESS_QUIVER,      50, 150,  0, BIAS_ARCHER},

    /* Bad Effects                                  Lv    T   R  Bias */
    {"AGGRAVATE",       EFFECT_AGGRAVATE,           10, 100,  1, BIAS_DEMON},
    {"HEAL_MONSTER",    EFFECT_HEAL_MONSTER,         2,  50,  0, 0},
    {"HASTE_MONSTER",   EFFECT_HASTE_MONSTER,       20, 100,  0, 0},
    {"HASTE_MONSTERS",  EFFECT_HASTE_MONSTERS,      10,  50,  0, 0},
    {"CLONE_MONSTER",   EFFECT_CLONE_MONSTER,       15, 100,  0, 0},
    {"DARKNESS",        EFFECT_DARKNESS,             5,  10,  0, 0},
    {"SUMMON_ANGRY_MONSTERS",
                        EFFECT_SUMMON_ANGRY_MONSTERS,10,200,  0, 0},
    {"SLOWNESS",        EFFECT_SLOWNESS,             5,  10,  0, 0},

    /* Specific Artifacts                           Lv    T   R  Bias */
    {"JEWEL",           EFFECT_JEWEL,                0,   0,  0, 0},
    {"HERMES",          EFFECT_HERMES,               0,   0,  0, 0},
    {"ARTEMIS",         EFFECT_ARTEMIS,              0,   0,  0, 0},
    {"DEMETER",         EFFECT_DEMETER,              0,   0,  0, 0},
    {"EYE_VECNA",       EFFECT_EYE_VECNA,            0,   0,  0, 0},
    {"ONE_RING",        EFFECT_ONE_RING,             0,   0,  0, 0},
    {"BLADETURNER",     EFFECT_BLADETURNER,          0,   0,  0, 0},
    {"BLOODY_MOON",     EFFECT_BLOODY_MOON,          0,   0,  0, 0},
    {"SACRED_KNIGHTS",  EFFECT_SACRED_KNIGHTS,       0,   0,  0, 0},
    {"GONG",            EFFECT_GONG,                 0,   0,  0, 0},
    {"MURAMASA",        EFFECT_MURAMASA,             0,   0,  0, 0},

    {0}
};

_effect_info_ptr _get_effect_info(int type)
{
    int i;
    for (i = 0; ; i++)
    {
        _effect_info_ptr e = &_effect_info[i];
        if (!e->type) break;
        if (e->type == type) return e;
    }
    return NULL;
}

bool effect_is_known(int type)
{
    _effect_info_ptr e = _get_effect_info(type);
    if (e)
        return e->known;
    return FALSE;
}

bool effect_learn(int type)
{
    _effect_info_ptr e = _get_effect_info(type);
    if (e && !e->known)
    {
        e->known = TRUE;
        return TRUE;
    }
    return FALSE;
}

int effect_parse_type(cptr type)
{
    int i;
    for (i = 0; ; i++)
    {
        if (!_effect_info[i].text) break;
        if (streq(type, _effect_info[i].text))
            return _effect_info[i].type;
    }
    return EFFECT_NONE;
}

errr effect_parse(char *line, effect_t *effect) /* LIGHT_AREA:<Lvl>:<Timeout>:<Extra> */
{
    char *tokens[5];
    int   num = tokenize(line, 5, tokens, 0);
    int   i;

    if (num < 1) return PARSE_ERROR_TOO_FEW_ARGUMENTS;

    WIPE(effect, effect_t);

    for (i = 0; ; i++)
    {
        if (!_effect_info[i].text) break;
        if (streq(tokens[0], _effect_info[i].text))
        {
            effect->type = _effect_info[i].type;
            break;
        }
    }
    if (num >= 2)
    {
        effect->power = atoi(tokens[1]);
        effect->difficulty = effect->power;
    }
    if (num >= 3)
        effect->cost = atoi(tokens[2]);
    if (num >= 4)
        effect->extra = atoi(tokens[3]);

    if (!effect->type) return 1;
    return 0;
}

static int _choose_random_p(effect_p p)
{
    int i, n;
    int tot = 0;

    for (i = 0; ; i++)
    {
        if (!_effect_info[i].type) break;
        if (!_effect_info[i].rarity) continue;
        if (p && !p(_effect_info[i].type)) continue;

        tot += MAX(255 / _effect_info[i].rarity, 1);
    }

    if (!tot) return -1;
    n = randint1(tot);

    for (i = 0; ; i++)
    {
        if (!_effect_info[i].type) break;
        if (!_effect_info[i].rarity) continue;
        if (p && !p(_effect_info[i].type)) continue;

        n -= MAX(255 / _effect_info[i].rarity, 1);
        if (n <= 0) return i;
    }
    return -1;
}

static int _choose_random(int bias)
{
    int i, n;
    int tot = 0;
    int lvl = cave->difficulty; /* XXX need to pass this in */

/*  if (one_in_(3)) bias = 0; */

    for (i = 0; ; i++)
    {
        if (!_effect_info[i].type) break;
        if (_effect_info[i].level < lvl / 3) continue;
        if (bias && !(_effect_info[i].bias & bias)) continue;
        if (!_effect_info[i].rarity) continue;

        tot += MAX(255 / _effect_info[i].rarity, 1);
    }

    if (!tot) return -1;
    n = randint1(tot);

    for (i = 0; ; i++)
    {
        if (!_effect_info[i].type) break;
        if (_effect_info[i].level < lvl / 3) continue;
        if (bias && !(_effect_info[i].bias & bias)) continue;
        if (!_effect_info[i].rarity) continue;

        n -= MAX(255 / _effect_info[i].rarity, 1);
        if (n <= 0) return i;
    }
    return -1;
}

static void _add_index(object_type *o_ptr, int index)
{
    if (index >= 0)
    {
        o_ptr->activation.type = _effect_info[index].type;
        o_ptr->activation.power = _effect_info[index].level;
        o_ptr->activation.difficulty = _effect_info[index].level;
        o_ptr->activation.cost = _effect_info[index].cost;
        o_ptr->activation.extra = 0;
        o_ptr->timeout = 0;
        add_flag(o_ptr->flags, OF_ACTIVATE); /* for object lore */
    }
}

bool effect_add_random_p(object_type *o_ptr, effect_p p)
{
    int i;
    if (obj_is_(o_ptr, TV_HAFTED, SV_WIZSTAFF)) return FALSE;
    i = _choose_random_p(p);
    if (i >= 0)
    {
        _add_index(o_ptr, i);
        return TRUE;
    }
    return FALSE;
}

bool effect_add_random(object_type *o_ptr, int bias)
{
    int i;
    if (obj_is_(o_ptr, TV_HAFTED, SV_WIZSTAFF)) return FALSE;
    i = _choose_random(bias);
    if (i >= 0)
    {
        _add_index(o_ptr, i);
        return TRUE;
    }
    return FALSE;
}

bool effect_add(object_type *o_ptr, int type)
{
    int i;
    for (i = 0; ; i++)
    {
        if (!_effect_info[i].type) break;
        if (_effect_info[i].type == type)
        {
            _add_index(o_ptr, i);
            return TRUE;
        }
    }
    return FALSE;
}

/***********************************************************************
 * Redoing Devices (Wands, Staves and Rods)
 ***********************************************************************/

#define _DROP_GOOD       0x0001
#define _DROP_GREAT      0x0002
#define _NO_DESTROY      0x0004
#define _STOCK_TOWN      0x0008
#define _COMMON          0x0010
#define _RARE            0x0020

device_effect_info_t wand_effect_table[] =
{
    /*                            Lvl Cost Rarity  Max  Difficulty Flags */
    {EFFECT_BOLT_MISSILE,           1,   3,     1,  20,    10,  0, _STOCK_TOWN},
    {EFFECT_HEAL_MONSTER,           2,   3,     1,  20,     0,  0, 0},
    {EFFECT_BEAM_LIGHT_WEAK,         2,   3,     1,  20,    10,  0, _STOCK_TOWN},
    {EFFECT_BALL_POIS,              5,   4,     1,  20,    33,  0, _STOCK_TOWN},
    {EFFECT_SLEEP_MONSTER,          5,   5,     1,  20,    33,  0, _STOCK_TOWN},
    {EFFECT_SLOW_MONSTER,           5,   5,     1,  20,    33,  0, _STOCK_TOWN},
    {EFFECT_CONFUSE_MONSTER,        5,   5,     1,  20,    33,  0, 0},
    {EFFECT_SCARE_MONSTER,          7,   5,     1,  20,    33,  0, 0},
    {EFFECT_STONE_TO_MUD,          10,   5,     1,   0,    10,  0, _COMMON},
    {EFFECT_POLYMORPH,             12,   6,     1,  30,     0,  0, 0},
    {EFFECT_BOLT_COLD,             12,   7,     1,  30,    33,  0, 0},
    {EFFECT_BOLT_ELEC,             15,   7,     1,  30,    33,  0, 0},
    {EFFECT_BOLT_ACID,             17,   8,     1,  35,    33,  0, _STOCK_TOWN},
    {EFFECT_BOLT_FIRE,             19,   9,     1,  35,    33,  0, _STOCK_TOWN},
    {EFFECT_HASTE_MONSTER,         20,   3,     1,  40,     0,  0, 0},
    {EFFECT_TELEPORT_AWAY,         20,  10,     1,   0,    10,  0, _COMMON},
    {EFFECT_DESTROY_TRAPS,         20,  10,     1,   0,    10,  0, 0},
    {EFFECT_CHARM_MONSTER,         25,  11,     1,  50,    33,  0, 0},
    {EFFECT_BALL_COLD,             26,   5,     1,   0,    50, 10, 0},
    {EFFECT_BALL_ELEC,             28,   5,     1,   0,    50, 10, 0},
    {EFFECT_BALL_ACID,             29,   7,     1,   0,    50, 10, _STOCK_TOWN},
    {EFFECT_BALL_FIRE,             30,   8,     1,   0,    50, 10, _STOCK_TOWN},
    {EFFECT_BOLT_WATER,            30,   9,     1,   0,    50, 10, 0},
    {EFFECT_DRAIN_LIFE,            32,  20,     1,   0,    50, 10, 0},
    {EFFECT_BOLT_PLASMA,           38,  10,     1,   0,    50, 10, _STOCK_TOWN},
    {EFFECT_BOLT_ICE,              40,  11,     1,   0,    50, 10, 0},
    {EFFECT_ARROW,                 45,  13,     1,   0,    50, 10, 0},
    {EFFECT_BALL_NEXUS,            47,  14,     1,   0,    50, 10, _DROP_GOOD},
    {EFFECT_BREATHE_COLD,          50,  15,     1,   0,    60, 10, _DROP_GOOD | _NO_DESTROY},
    {EFFECT_BREATHE_FIRE,          50,  16,     1,   0,    60, 10, _DROP_GOOD | _NO_DESTROY},
    {EFFECT_BEAM_GRAVITY,          55,  32,     2,   0,    33,  0, _DROP_GOOD | _NO_DESTROY},
    {EFFECT_METEOR,                55,  32,     2,   0,    50, 10, _DROP_GOOD | _NO_DESTROY},
    {EFFECT_BREATHE_ONE_MULTIHUED, 60,  17,     2,   0,    60, 10, _DROP_GOOD | _NO_DESTROY},
    {EFFECT_GENOCIDE_ONE,          65,  35,     2,   0,    60, 10, _DROP_GOOD | _NO_DESTROY},
    {EFFECT_BALL_WATER,            70,  20,     2,   0,    60, 10, _DROP_GOOD | _NO_DESTROY},
    {EFFECT_BALL_DISINTEGRATE,     75,  20,     2,   0,    60, 10, _DROP_GOOD | _DROP_GREAT | _NO_DESTROY},
    {EFFECT_ROCKET,                85,  20,     3,   0,    70, 20, _DROP_GOOD | _DROP_GREAT | _NO_DESTROY},
    {EFFECT_WALL_BUILDING,        100,  50,    16,   0,     0,  0, _DROP_GOOD | _DROP_GREAT | _NO_DESTROY},
    {0}
};

device_effect_info_t rod_effect_table[] =
{
    /*                            Lvl Cost Rarity  Max  Difficulty Flags */
    {EFFECT_PESTICIDE,              1,   7,     1,  30,    10,  0, 0},
    {EFFECT_DETECT_TRAPS,           5,   9,     1,  30,    10,  0, 0},
    {EFFECT_LIGHT_AREA,             10,  10,     1,  40,    10,  0, 0},
    {EFFECT_DETECT_DOOR_STAIRS,    12,  10,     1,  40,    10,  0, 0},
    {EFFECT_DETECT_MONSTERS,       15,  10,     1,  40,    10,  0, 0},
    {EFFECT_BEAM_ELEC,             17,   8,     1,  50,    33,  0, 0},
    {EFFECT_BEAM_COLD,             19,   8,     1,  50,    33,  0, 0},
    {EFFECT_BEAM_FIRE,             21,   9,     1,  60,    33,  0, 0},
    {EFFECT_BEAM_ACID,             23,   9,     1,  60,    33,  0, 0},
    {EFFECT_BEAM_LIGHT,             25,  12,     2,   0,    50, 10, 0},
    /*{EFFECT_RECALL,                27,  15,     1,   0,    10,  0, 0},*/
    {EFFECT_DETECT_ALL,            30,  17,     2,   0,    10,  0, _COMMON},
    {EFFECT_ESCAPE,                30,  20,     1,   0,    10,  0, 0},
    {EFFECT_BEAM_CHAOS,            32,  12,     2,  60,    33,  0, 0},
    {EFFECT_BEAM_SOUND,            32,  12,     2,  70,    33,  0, 0},
    {EFFECT_CLARITY,               35,  15,     3,  80,    33,  0, _DROP_GOOD},
    {EFFECT_BALL_ELEC,             40,  13,     1,   0,    50, 10, 0},
    {EFFECT_BALL_COLD,             40,  14,     1,   0,    50, 10, 0},
    {EFFECT_BALL_FIRE,             42,  15,     1,   0,    50, 10, 0},
    {EFFECT_BALL_ACID,             44,  16,     1,   0,    50, 10, 0},
    {EFFECT_BOLT_MANA,             45,  17,     2,   0,    50, 10, _DROP_GOOD},
    {EFFECT_BALL_NETHER,           45,  18,     1,   0,    50, 10, 0},
    {EFFECT_BALL_DISEN,            47,  19,     2,   0,    50, 10, _DROP_GOOD},
    {EFFECT_ENLIGHTENMENT,         50,  33,     2,   0,    10,  0, _COMMON},
    {EFFECT_BALL_SOUND,            52,  22,     2,   0,    50, 10, _DROP_GOOD},
    {EFFECT_BEAM_DISINTEGRATE,     60,  37,     2,   0,    33,  0, _DROP_GOOD},
    {EFFECT_SPEED_HERO,            70,  40,     2,   0,    10,  0, _DROP_GOOD | _DROP_GREAT},
    {EFFECT_GREAT_CLARITY,         75,  60,     4,   0,    50, 10, _DROP_GOOD | _DROP_GREAT},
    {EFFECT_HEAL_CURING_HERO,      80,  60,     3,   0,    60, 10, _DROP_GOOD | _DROP_GREAT},
    {EFFECT_RESTORING,             80,  60,     3,   0,     0,  0, _DROP_GOOD | _DROP_GREAT | _RARE},
    {EFFECT_BALL_MANA,             80,  24,     2,   0,    60, 10, _DROP_GOOD | _DROP_GREAT},
    {EFFECT_BALL_SHARDS,           80,  25,     2,   0,    60, 10, _DROP_GOOD | _DROP_GREAT},
    {EFFECT_BALL_CHAOS,            85,  27,     3,   0,    70, 10, _DROP_GOOD | _DROP_GREAT},
    {EFFECT_CLAIRVOYANCE,          90, 100,     3,   0,     0,  0, _DROP_GOOD | _DROP_GREAT | _RARE},
    {EFFECT_BALL_LIGHT,             95,  27,     3,   0,    70, 10, _DROP_GOOD | _DROP_GREAT},
    {0}
};

device_effect_info_t staff_effect_table[] =
{
    /*                            Lvl Cost Rarity  Max  Difficulty Flags */
    {EFFECT_NOTHING,                1,   1,     0,   0,     0,  0, 0},
    {EFFECT_DARKNESS,               1,   3,     1,  15,     0,  0, 0},
    {EFFECT_LIGHT_AREA,              1,   3,     1,  30,    10,  0, _STOCK_TOWN},
    {EFFECT_DETECT_GOLD,            5,   4,     1,  30,    10,  0, _STOCK_TOWN},
    {EFFECT_DETECT_OBJECTS,         5,   4,     1,  30,    10,  0, _STOCK_TOWN},
    {EFFECT_DETECT_INVISIBLE,       5,   4,     1,  30,    10,  0, 0},
    {EFFECT_DETECT_TRAPS,           5,   5,     1,  30,    10,  0, _STOCK_TOWN},
    {EFFECT_DETECT_DOOR_STAIRS,     5,   5,     1,  30,    10,  0, _STOCK_TOWN},
    {EFFECT_DETECT_EVIL,            7,   5,     1,  30,    10,  0, 0},
    {EFFECT_HASTE_MONSTERS,        10,   5,     1,  30,    50, 10, 0},
    {EFFECT_SUMMON_ANGRY_MONSTERS, 10,   5,     1,  30,    50, 10, 0},
    {EFFECT_IDENTIFY,              10,   4,     1,   0,    10,  0, _STOCK_TOWN | _COMMON},
    {EFFECT_SLEEP_MONSTERS,        10,   6,     1,  40,    33,  0, 0},
    {EFFECT_SLOW_MONSTERS,         10,   6,     1,  40,    33,  0, 0},
    {EFFECT_CONFUSE_MONSTERS,      15,   8,     1,  40,    33,  0, 0},
    {EFFECT_TELEPORT,              20,  10,     1,   0,    10,  0, 0},
    {EFFECT_ENLIGHTENMENT,         20,  10,     1,  70,    10,  0, _STOCK_TOWN},
    {EFFECT_STARLIGHT,              20,  10,     1,  50,    33,  0, 0},
    {EFFECT_EARTHQUAKE,            20,  10,     2,   0,    10,  0, 0},
    {EFFECT_HEAL,                  20,  10,     2,  70,    33,  0,  _COMMON}, /* Cure Wounds for ~50hp */
    {EFFECT_CURING,                25,  12,     1,  70,    10,  0, 0}, /* Curing no longer heals */
    {EFFECT_SUMMON_HOUNDS,         27,  25,     2,   0,    10,  0, 0},
    {EFFECT_SUMMON_HYDRAS,         27,  25,     3,   0,    10,  0, 0},
    {EFFECT_SUMMON_ANTS,           27,  20,     2,   0,    10,  0, 0},
    {EFFECT_PROBING,               30,  15,     3,  70,    10,  0, _STOCK_TOWN},
    {EFFECT_TELEPATHY,             30,  16,     2,   0,    10,  0, 0},
    {EFFECT_SUMMON_MONSTERS,       32,  30,     2,   0,    33,  0, 0},
    {EFFECT_ANIMATE_DEAD,          35,  17,     2,  70,    33,  0, 0},
    {EFFECT_SLOWNESS,              40,  19,     3,  70,    50, 10, 0},
    {EFFECT_SPEED,                 40,  19,     2,   0,    10,  0, _COMMON},
    {EFFECT_IDENTIFY_FULL,         40,  20,     3,   0,    10,  0, _COMMON},
    {EFFECT_REMOVE_CURSE,          40,  20,     4,   0,    10,  0, 0},
    {EFFECT_DISPEL_DEMON,          45,  10,     2,   0,    50, 10, 0},
    {EFFECT_DISPEL_UNDEAD,         45,  10,     2,   0,    50, 10, 0},
    {EFFECT_DISPEL_LIFE,           50,  12,     3,   0,    50, 10, 0},
    {EFFECT_DISPEL_EVIL,           55,  13,     3,   0,    50, 10, 0},
    {EFFECT_DISPEL_MONSTERS,       55,  15,     5,   0,    50, 10, 0},
    {EFFECT_DESTRUCTION,           50,  15,     2,   0,    50, 10, _DROP_GOOD},
    {EFFECT_CONFUSING_LIGHT,        55,  26,     2,   0,    50, 10, _DROP_GOOD},
    {EFFECT_HEAL_CURING,           55,  10,     3,   0,    60, 10, _DROP_GOOD | _DROP_GREAT},
    {EFFECT_BANISH_EVIL,           60,  31,     2,   0,    33,  0, _DROP_GOOD},
    {EFFECT_BANISH_ALL,            70,  32,     3,   0,    33,  0, _DROP_GOOD},
    {EFFECT_MANA_STORM,            85,  10,     3,   0,    60, 10, _DROP_GOOD | _DROP_GREAT | _NO_DESTROY},
    {EFFECT_STARBURST,             85,  12,     3,   0,    60, 10, _DROP_GOOD | _DROP_GREAT | _NO_DESTROY},
    {EFFECT_DARKNESS_STORM,        85,  12,     3,   0,    60, 10, _DROP_GOOD | _DROP_GREAT | _NO_DESTROY},
    {EFFECT_GENOCIDE,              90,  50,     8,   0,    60, 10, _DROP_GOOD | _DROP_GREAT | _RARE},
    {EFFECT_RESTORE_MANA,         100, 100,    16,   0,     0,  0, _DROP_GOOD | _DROP_GREAT | _NO_DESTROY | _RARE},
    {0}
};

/* MAX(1, _rand_normal(1, 10)) is problematic. Think about why! */
static int _bounds_check(int value, int min, int max)
{
    int result = value;
    if (result < min)
        result = min;
    if (result > max)
        result = max;
    return result;
}

/* I like to set my deviation as a percentage of the mean.
   Also, the scaling and rounding makes the distribution no
   longer normal, but I like small casting costs to vary as
   well (e.g. _rand_normal(1, 10) can give 2 (about 3% of the
   time) whereas randnor(1, 0.1), even if legal, would not. */
static int _rand_normal(int mean, int pct)
{
    int result = 0;
    int m = mean * 10;
    int d = m * pct / 100;
    int r = randnor(m, d);

    assert(mean >= 0);
    assert(pct >= 0);

    result = r/10;
    if (randint0(10) < (r%10))
        result++;

    return result;
}
/*static int _rand_normal_hi(int mean, int pct)
{
    int result = _rand_normal(mean, pct);
    if (result < mean)
        result = mean + (mean - result);
    return result;
}*/

static int _effect_rarity(device_effect_info_ptr entry, int level)
{
    int r = entry->rarity;
    if (!r) return 0;
    if (entry->max_depth && entry->max_depth < level) return 0;
    if (entry->flags & _RARE)
    {
        int n = entry->counts.found;
        while (n--)
            r *= 2;
    }
    else if (level > entry->level)
    {
        int d = level - entry->level;
        int spread = entry->max_depth ? entry->max_depth - entry->level : 100 - entry->level;
        int n = (entry->flags & _COMMON) ? spread/2 : spread*2/7;
        while (d >= n)
        {
            r *= 2;
            d -= n;
        }
        r += d*r/n;
    }
    return r;
}

static void _device_pick_effect(object_type *o_ptr, device_effect_info_ptr table, int level, int mode)
{
    int i, n;
    int tot = 0;

    for (i = 0; ; i++)
    {
        device_effect_info_ptr entry = &table[i];
        int                    rarity;

        if (!entry->type) break;

        entry->prob = 0;
        rarity = _effect_rarity(entry, level);

        if (!rarity) continue;
        if (entry->level > device_level(o_ptr)) continue;
        if ((mode & AM_GOOD) && !(entry->flags & _DROP_GOOD)) continue;
        if ((mode & AM_GREAT) && !(entry->flags & _DROP_GREAT)) continue;
        if ((mode & AM_STOCK_TOWN) && !(entry->flags & _STOCK_TOWN)) continue;

        entry->prob = 64 / rarity;
        tot += entry->prob;
    }

    if (!tot) return;
    n = randint1(tot);

    for (i = 0; ; i++)
    {
        device_effect_info_ptr entry = &table[i];

        if (!entry->type) break;
        if (!entry->prob) continue;

        n -= entry->prob;
        if (n <= 0)
        {
            int cost;

            o_ptr->activation.type = entry->type;

            /* Power is the casting level of the device and determines damage or power of the effect.
               Difficulty is the level of the effect, and determines the fail rate of the effect.
               Difficulty is set to the base level of the effect, and then scaled based on the
               power of the effect using the difficulty_base and xtra percentages. */
            o_ptr->activation.power = device_level(o_ptr);
            o_ptr->activation.difficulty = entry->level;
            if (o_ptr->activation.power > o_ptr->activation.difficulty)
            {
                int d = 10*(o_ptr->activation.power - o_ptr->activation.difficulty);
                int b = entry->difficulty_base * d / 100;

                b += randint0(entry->difficulty_xtra * d / 100);
                o_ptr->activation.difficulty += b/10;
                if (randint0(10) < b%10)
                    o_ptr->activation.difficulty++;
                if (o_ptr->activation.difficulty > o_ptr->activation.power) /* paranoia */
                    o_ptr->activation.difficulty = o_ptr->activation.power;
            }

            cost = entry->cost;
            cost += effect_cost_extra(&o_ptr->activation);
            o_ptr->activation.cost = _bounds_check(_rand_normal(cost, 5), 1, 1000);

            if (entry->flags & _NO_DESTROY)
            {
                add_flag(o_ptr->flags, OF_IGNORE_ACID);
                add_flag(o_ptr->flags, OF_IGNORE_ELEC);
                add_flag(o_ptr->flags, OF_IGNORE_FIRE);
                add_flag(o_ptr->flags, OF_IGNORE_COLD);
            }

            return;
        }
    }
}

static bool _is_valid_device(object_type *o_ptr)
{
    switch (o_ptr->tval)
    {
    case TV_WAND:
    case TV_ROD:
    case TV_STAFF:
        return TRUE;
    }
    return obj_is_(o_ptr, TV_HAFTED, SV_WIZSTAFF);
}

/* Initialize a device with a random effect for monster drops, dungeon objects, etc */
bool device_init(object_type *o_ptr, int level, int mode)
{
    if (!_is_valid_device(o_ptr))
        return FALSE;

    if (!(mode & (AM_STOCK_TOWN | AM_STOCK_BM)) && one_in_(GREAT_OBJ))
    {
        int boost = level;
        if (boost < 20)
            boost = 20;
        level += rand_range(boost/4, boost/2);
    }

    if (level > 100)
        level = 100;

    /* device_level
     * 90%+-10% means 84.13% <= level (modulo rounding, of course)
     * 95%+-10% means ~70% <= level. So ~30% at or *above* level.
     * See how generous I've become ;) */
    o_ptr->xtra3 = _bounds_check(_rand_normal(level*95/100, 10), 1, 100);

    switch (o_ptr->tval)
    {
    case TV_WAND:
        _device_pick_effect(o_ptr, wand_effect_table, o_ptr->xtra3, mode);
        if (!o_ptr->activation.type)
            return FALSE;
        /* device_max_sp */
        o_ptr->xtra4 = _bounds_check(_rand_normal(3*o_ptr->xtra3, 15), o_ptr->activation.cost*4, 1000);
        break;
    case TV_ROD:
        _device_pick_effect(o_ptr, rod_effect_table, o_ptr->xtra3, mode);
        if (!o_ptr->activation.type)
            return FALSE;
        /* device_max_sp: rods have fewer sp but regen more quickly. */
        o_ptr->xtra4 = _bounds_check(_rand_normal(3*o_ptr->xtra3/2, 15), o_ptr->activation.cost*2, 1000);
        break;
    case TV_HAFTED:
        assert(o_ptr->sval == SV_WIZSTAFF);
    case TV_STAFF:
        _device_pick_effect(o_ptr, staff_effect_table, o_ptr->xtra3, mode);
        if (!o_ptr->activation.type)
        {
            if (obj_is_(o_ptr, TV_HAFTED, SV_WIZSTAFF) && (mode & AM_GREAT))
                return device_init(o_ptr, level, mode & ~AM_GREAT);
            else if (obj_is_(o_ptr, TV_HAFTED, SV_WIZSTAFF) && (mode & AM_GOOD))
                return device_init(o_ptr, level, mode & ~AM_GOOD);
            else
                return FALSE;
        }
        /* device_max_sp */
        o_ptr->xtra4 = _bounds_check(_rand_normal(3*o_ptr->xtra3, 15), o_ptr->activation.cost*4, 1000);
        break;
    }
    /* device_sp */
    o_ptr->xtra5 = _bounds_check(_rand_normal(o_ptr->xtra4/2, 25), o_ptr->activation.cost, o_ptr->xtra4);
    o_ptr->xtra5 *= 100; /* scale current sp by 100 for smoother regeneration */

    add_flag(o_ptr->flags, OF_ACTIVATE);

    /* cf obj_create_device in ego.c for egos */
    return TRUE;
}

static device_effect_info_ptr _device_find_effect(device_effect_info_ptr table, int effect)
{
    int i;

    for (i = 0; ; i++)
    {
        device_effect_info_ptr entry = &table[i];

        if (!entry->type) break;
        if (entry->type == effect) return entry;
    }

    return NULL;
}

/* XXX When a Devicemaster transfers an effect, we better re-calc the
 * cost. For example, transfering -Disitegrate from entry level to
 * a high end P100 device would grant too many charges. */
void device_init_cost(obj_ptr obj)
{
    device_effect_info_ptr e = NULL;
    int cost;
    switch (obj->tval)
    {
    case TV_WAND:
        e = _device_find_effect(wand_effect_table, obj->activation.type);
        break;
    case TV_ROD:
        e = _device_find_effect(rod_effect_table, obj->activation.type);
        break;
    case TV_STAFF:
        e = _device_find_effect(staff_effect_table, obj->activation.type);
        break;
    }
    assert(e);
    cost = e->cost;
    cost += effect_cost_extra(&obj->activation);
    obj->activation.cost = _bounds_check(_rand_normal(cost, 5), 1, 1000);
}

bool device_is_valid_effect(int tval, int effect)
{
    switch (tval)
    {
    case TV_WAND:
        return _device_find_effect(wand_effect_table, effect) != NULL;
    case TV_ROD:
        return _device_find_effect(rod_effect_table, effect) != NULL;
    case TV_STAFF:
        return _device_find_effect(staff_effect_table, effect) != NULL;
    }
    return FALSE;
}

/* Initialize a device with a fixed effect. This is useful for birth objects, quest rewards, etc */
bool device_init_fixed(object_type *o_ptr, int effect)
{
    device_effect_info_ptr e_ptr = NULL;

    if (!_is_valid_device(o_ptr))
        return FALSE;

    switch (o_ptr->tval)
    {
    case TV_WAND:
        e_ptr = _device_find_effect(wand_effect_table, effect);
        if (!e_ptr)
            return FALSE;
        break;
    case TV_ROD:
        e_ptr = _device_find_effect(rod_effect_table, effect);
        if (!e_ptr)
            return FALSE;
        break;
    case TV_STAFF:
        e_ptr = _device_find_effect(staff_effect_table, effect);
        if (!e_ptr)
            return FALSE;
        break;
    }

    o_ptr->xtra3 = e_ptr->level;
    if (o_ptr->xtra3 < 7)
        o_ptr->xtra3 = 7;

    if (o_ptr->tval == TV_ROD)
        o_ptr->xtra4 = 3 * o_ptr->xtra3 / 2;
    else
        o_ptr->xtra4 = 3 * o_ptr->xtra3;
    o_ptr->xtra5 = o_ptr->xtra4; /* Fully Charged */
    o_ptr->xtra5 *= 100; /* scale current sp by 100 for smoother regeneration */

    o_ptr->activation.type = e_ptr->type;
    o_ptr->activation.power = o_ptr->xtra3;
    o_ptr->activation.difficulty = e_ptr->level;
    o_ptr->activation.cost = e_ptr->cost + effect_cost_extra(&o_ptr->activation);

    if (e_ptr->flags & _NO_DESTROY)
    {
        add_flag(o_ptr->flags, OF_IGNORE_ACID);
        add_flag(o_ptr->flags, OF_IGNORE_ELEC);
        add_flag(o_ptr->flags, OF_IGNORE_FIRE);
        add_flag(o_ptr->flags, OF_IGNORE_COLD);
    }

    add_flag(o_ptr->flags, OF_ACTIVATE);

    return TRUE;
}

/* TODO: See wiz_obj.c for reliance on xtra fields */
int device_level(object_type *o_ptr)
{
    if (_is_valid_device(o_ptr))
        return o_ptr->xtra3;
    return 0;
}

int device_sp(object_type *o_ptr)
{
    if (_is_valid_device(o_ptr))
        return o_ptr->xtra5 / 100;
    return 0;
}

int device_charges(object_type *o_ptr)
{
    if (_is_valid_device(o_ptr) && o_ptr->activation.cost)
        return  device_sp(o_ptr) / o_ptr->activation.cost;
    return 0;
}

int device_max_charges(object_type *o_ptr)
{
    if (_is_valid_device(o_ptr) && o_ptr->activation.cost)
        return  device_max_sp(o_ptr) / o_ptr->activation.cost;
    return 0;
}

void device_decrease_sp(object_type *o_ptr, int amt)
{
    if (_is_valid_device(o_ptr))
    {
        int charges = device_charges(o_ptr);
        o_ptr->xtra5 -= amt * 100;
        if (o_ptr->xtra5 < 0)
            o_ptr->xtra5 = 0;
        if (device_charges(o_ptr) != charges)
            plr->window |= PW_INVEN;
    }
}

void device_increase_sp(object_type *o_ptr, int amt)
{
    if (_is_valid_device(o_ptr))
    {
        int charges = device_charges(o_ptr);
        o_ptr->xtra5 += amt * 100;
        if (o_ptr->xtra5 > o_ptr->xtra4 * 100)
            o_ptr->xtra5 = o_ptr->xtra4 * 100;
        if (device_charges(o_ptr) != charges)
            plr->window |= PW_INVEN;
    }
}

bool device_is_fully_charged(object_type *o_ptr)
{
    if (_is_valid_device(o_ptr))
    {
        if (o_ptr->xtra5 == o_ptr->xtra4 * 100)
            return TRUE;
        else
            return FALSE;
    }
    return FALSE; /* ?? */
}

/* Note: Rods fire every 10 game turns; wands and staves fire every 100 game turns.*/
void device_regen_sp_aux(object_type *o_ptr, int per_mill)
{
    if (!device_is_fully_charged(o_ptr))
    {
        int div = 1000;
        int amt = o_ptr->xtra4 * 100 * per_mill;
        int charges = device_charges(o_ptr);

        o_ptr->xtra5 += amt / div;
        if (randint0(div) < (amt % div))
            o_ptr->xtra5++;

        if (o_ptr->xtra5 > o_ptr->xtra4 * 100)
            o_ptr->xtra5 = o_ptr->xtra4 * 100;

        if (device_is_fully_charged(o_ptr))
            recharged_notice(o_ptr);

        if (device_charges(o_ptr) != charges)
            plr->window |= PW_INVEN;
    }
}

static obj_ptr _get_quiver(void)
{
    slot_t slot = equip_find_obj(TV_QUIVER, SV_ANY);
    if (!slot) return NULL;
    return equip_obj(slot);
}

void device_regen_sp(obj_ptr o, int base_per_mill)
{
    int  per_mill = base_per_mill;

    if (!_is_valid_device(o))
        return;

    if (device_is_fully_charged(o))
        return;

    if (devicemaster_is_speciality(o))
        per_mill += base_per_mill;

    if (obj_has_flag(o, OF_REGEN))
        per_mill += o->pval * base_per_mill;

    /* XXX Hack for Mage Quiver of Regeneration. Only devices
     * inside the quiver should benefit. */
    if (o->loc.where == INV_QUIVER)
    {
        obj_ptr q = _get_quiver();
        if (q && q->sval == SV_QUIVER_MAGE && q->name2 == EGO_QUIVER_REGEN)
            per_mill += q->pval * base_per_mill;
    }

    #if 0
    if (plr->wizard)
    {
        char name[MAX_NLEN];
        object_desc(name, o, OD_COLOR_CODED);
        msg_format("<color:B>Regenerating %s by %d.%d%%.</color>",
            name, per_mill / 10, per_mill % 10);
    }
    #endif

    device_regen_sp_aux(o, per_mill);
}

int device_max_sp(object_type *o_ptr)
{
    if (_is_valid_device(o_ptr))
        return o_ptr->xtra4;
    return 0;
}

int device_value(object_type *o_ptr, int options)
{
    int  result = 0;
    u32b flgs[OF_ARRAY_SIZE];
    int  pval = 0;

    if (!_is_valid_device(o_ptr))
        return 0;

    switch (o_ptr->tval)
    {
    case TV_WAND: result = 100; break;
    case TV_STAFF: result = 100; break;
    case TV_ROD: result = 250; break;
    }

    if ((options & COST_REAL) || obj_is_known(o_ptr))
    {
        pval = o_ptr->pval;
        if (o_ptr->activation.type)
        {
            int value = effect_value(&o_ptr->activation);

            if (o_ptr->activation.cost)
            {
                int base_charges = 40; /* scaled by 10 */
                int charges = device_max_sp(o_ptr) * 10 / o_ptr->activation.cost;

                if (o_ptr->tval == TV_ROD)
                {
                    base_charges /= 2;
                    value = value * 150 / 100; /* rods should value more than wands (durable+fast recharge) */
                }

                value = value * charges / base_charges;
            }
            result += value;
        }
    }

    if (options & COST_REAL)
        obj_flags(o_ptr, flgs);
    else
        obj_flags_known(o_ptr, flgs);

    if ((options & COST_REAL) || obj_is_known(o_ptr))
    {
        if (o_ptr->name2 == EGO_DEVICE_RESISTANCE) /* I don't want artifacts to get an extra boost for TR_IGNORE_* */
            result += result * 25 / 100;
    }
    if (have_flag(flgs, OF_REGEN))
        result += result * 20 * pval / 100;

    if (have_flag(flgs, OF_EASY_SPELL))
        result += result * 10 * pval / 100;

    if (have_flag(flgs, OF_DEVICE_POWER))
        result += result * 25 * pval / 100;

    if (have_flag(flgs, OF_HOLD_LIFE))
        result += result * 30 / 100;

    if (have_flag(flgs, OF_SPEED))
        result += result * 25 * pval / 100;

    return result;
}

/* Statistics */
device_effect_info_ptr device_get_effect_info(int tval, int effect)
{
    switch (tval)
    {
    case TV_WAND:
        return _device_find_effect(wand_effect_table, effect);
        break;
    case TV_ROD:
        return _device_find_effect(rod_effect_table, effect);
        break;
    case TV_STAFF:
        return _device_find_effect(staff_effect_table, effect);
        break;
    }
    return NULL;
}

static void _device_stats_reset_imp(device_effect_info_ptr table)
{
    int i;

    for (i = 0; ; i++)
    {
        device_effect_info_ptr entry = &table[i];
        if (!entry->type) break;
        WIPE(&entry->counts, counts_t);
    }
}

void device_stats_reset(void)
{
    _device_stats_reset_imp(wand_effect_table);
    _device_stats_reset_imp(rod_effect_table);
    _device_stats_reset_imp(staff_effect_table);
}

static void _device_stats_save_imp(savefile_ptr file, device_effect_info_ptr table)
{
    int i, ct = 0;

    for (i = 0; ; i++)
    {
        device_effect_info_ptr entry = &table[i];
        if (!entry->type) break;
        ct++;
    }

    savefile_write_s32b(file, ct);
    for (i = 0; ; i++)
    {
        device_effect_info_ptr entry = &table[i];
        if (!entry->type) break;
        savefile_write_s32b(file, entry->type);

        savefile_write_s32b(file, entry->counts.generated);
        savefile_write_s32b(file, entry->counts.found);
        savefile_write_s32b(file, entry->counts.bought);
        savefile_write_s32b(file, entry->counts.used);
        savefile_write_s32b(file, entry->counts.destroyed);
    }

}

static void _device_stats_load_imp(savefile_ptr file, device_effect_info_ptr table)
{
    int ct, i;

    /* We reset since not every current table entry need exist in the savefile.
       In other words, code changes over time and I might add a new entry :) */
    _device_stats_reset_imp(table);

    ct = savefile_read_s32b(file);
    for (i = 0; i < ct; i++)
    {
        int                     type = savefile_read_s32b(file);
        counts_t                counts;
        device_effect_info_ptr  entry = _device_find_effect(table, type);

        counts.generated = savefile_read_s32b(file);
        counts.found = savefile_read_s32b(file);
        counts.bought = savefile_read_s32b(file);
        counts.used = savefile_read_s32b(file);
        counts.destroyed = savefile_read_s32b(file);

        if (entry)
            entry->counts = counts;
    }
}

void device_stats_on_save(savefile_ptr file)
{
    int i, ct = 0;
    _device_stats_save_imp(file, wand_effect_table);
    _device_stats_save_imp(file, rod_effect_table);
    _device_stats_save_imp(file, staff_effect_table);

    for (i = 0; ; i++)
    {
        if (!_effect_info[i].type) break;
        if (_effect_info[i].known) ct++;
    }
    savefile_write_s32b(file, ct);
    for (i = 0; ; i++)
    {
        if (!_effect_info[i].type) break;
        if (_effect_info[i].known)
            savefile_write_s32b(file, _effect_info[i].type);
    }
}

void device_stats_on_load(savefile_ptr file)
{
    int i, ct;

    _device_stats_load_imp(file, wand_effect_table);
    _device_stats_load_imp(file, rod_effect_table);
    _device_stats_load_imp(file, staff_effect_table);

    ct = savefile_read_s32b(file);
    for (i = 0; i < ct; i++)
    {
        int type = savefile_read_s32b(file);
        _effect_info_ptr e = _get_effect_info(type);
        if (e)
            e->known = TRUE;
    }
}

void device_stats_on_find(object_type *o_ptr)
{
    if (_is_valid_device(o_ptr))
    {
        device_effect_info_ptr entry = device_get_effect_info(o_ptr->tval, o_ptr->activation.type);
        if (entry)
            entry->counts.found++;
    }
}

void device_stats_on_use(object_type *o_ptr, int num)
{
    if (_is_valid_device(o_ptr))
    {
        device_effect_info_ptr entry = device_get_effect_info(o_ptr->tval, o_ptr->activation.type);
        if (entry)
            entry->counts.used += num;
    }
}

void device_stats_on_destroy(object_type *o_ptr)
{
    if (_is_valid_device(o_ptr))
    {
        device_effect_info_ptr entry = device_get_effect_info(o_ptr->tval, o_ptr->activation.type);
        if (entry)
            entry->counts.destroyed++;
    }
}

void device_stats_on_purchase(object_type *o_ptr)
{
    if (_is_valid_device(o_ptr))
    {
        device_effect_info_ptr entry = device_get_effect_info(o_ptr->tval, o_ptr->activation.type);
        if (entry)
            entry->counts.bought++;
    }
}

static int _extra(effect_t *effect, int def)
{
    int result = effect->extra;
    if (!result)
        result = def;
    return result;
}

static int _boost(int value, int boost)
{
    return MAX(0, value * (100 + boost) / 100);
}

/* Device casting is non-linear in difficulty (cf design/devices.ods)
 * Yet device power (e.g. damage) is (or was?) linear. This is hardly fair! */
typedef struct { int w1, w2, w3; } _weights_t;
static _weights_t _weights(int w1, int w2, int w3)
{
    _weights_t w;
    w.w1 = w1;
    w.w2 = w2;
    w.w3 = w3;
    return w;
}
typedef struct { int lvl, max; } _level_t;
static _level_t _level_aux(int lvl, int max)
{
    _level_t l;
    l.lvl = lvl;
    l.max = max;
    return l;
}
static _level_t _level(int lvl)
{
    return _level_aux(lvl, 100);
}
static _level_t _level_offset(int lvl, int start)
{
    int l = MAX(0, lvl - start);
    return _level_aux(l, 100 - start);
}
static int _power_curve_aux(int amt, _level_t l, _weights_t w)
{
    int result = 0;
    int wt = w.w1 + w.w2 + w.w3;

    if (l.lvl >= l.max)
        return amt;

    result += amt * l.lvl * w.w1 / (l.max*wt);
    result += amt * l.lvl * l.lvl * w.w2 / (l.max*l.max*wt);
    result += (amt * l.lvl * l.lvl / l.max) * l.lvl * w.w3 / (l.max*l.max*wt);

    return result;
}
static int _power_curve(int amt, int lvl)
{
    return _power_curve_aux(amt, _level(lvl), _weights(1, 1, 1));
}
static int _power_curve_offset(int amt, int lvl, int start)
{
    return _power_curve_aux(amt, _level_offset(lvl, start), _weights(1, 1, 1));
}
static void _list_unique(int id, mon_ptr mon)
{
    if (mon_is_unique(mon))
    {
        char buf[MAX_NLEN_MON];
        monster_desc(buf, mon, MD_ASSUME_VISIBLE | MD_IGNORE_FUZZY);
        msg_format("%s.", buf);
        device_noticed = TRUE;
    }
}
static void _list_artifact(point_t pos, obj_ptr obj)
{
    if (obj->art_id)
    {
        art_ptr art = arts_lookup(obj->art_id);
        msg_format("%s. ", art->name);
        device_noticed = TRUE;
    }
    if (obj->art_name)
    {
        msg_format("%s. ", quark_str(obj->art_name));
        device_noticed = TRUE;
    }
}

/************************************************************************
 * Helpers
 ***********************************************************************/
static cptr _device_bolt(int gf, dice_t dice)
{
    point_t p = plr_get_target(gf);
    bool notice;
    gf_info_ptr gfi;

    if (!dun_pos_interior(cave, p)) return NULL;

    notice = device_bolt(p, gf, dice_roll(dice));

    gfi = gf_lookup(gf);
    if (gfi->flags & GFF_ELEMENTAL)
        device_noticed = TRUE;
    else
        device_noticed = notice;

    return "";
}
static cptr _device_rocket(int rad, dice_t dice)
{
    point_t p = get_fire_pos();
    if (!dun_pos_interior(cave, p)) return NULL;
    device_rocket(rad, p, GF_ROCKET, dice_roll(dice));
    device_noticed = TRUE;
    return "";
}
static cptr _device_beam(int gf, dice_t dice)
{
    point_t p = plr_get_beam_target(gf);
    bool notice;
    gf_info_ptr gfi;

    if (!dun_pos_interior(cave, p)) return NULL;

    notice = device_beam(p, gf, dice_roll(dice));

    gfi = gf_lookup(gf);
    if ((gfi->flags & GFF_ELEMENTAL) || gf == GF_LIGHT_WEAK) /* XXX GFF_NOTICE */
        device_noticed = TRUE;
    else
        device_noticed = notice;

    return "";
}
static cptr _device_ball(int rad, int gf, dice_t dice)
{
    point_t p = plr_get_ball_target(gf);
    bool notice;
    gf_info_ptr gfi;

    if (!dun_pos_interior(cave, p)) return NULL;

    notice = device_ball(rad, p, gf, dice_roll(dice));

    gfi = gf_lookup(gf);
    if (gfi->flags & GFF_ELEMENTAL)
        device_noticed = TRUE;
    else
        device_noticed = notice;

    return "";
}
static cptr _breath_name(int gf)
{
    if (gf == GF_MISSILE) return "<color:v>the Elements</color>"; /* PDSM */
    return gf_name(gf);
}
static cptr _device_breath(int rad, int gf, dice_t dice)
{
    point_t p = plr_get_breath_target(gf);
    bool notice;
    gf_info_ptr gfi;
    if (!dun_pos_interior(cave, p)) return NULL;

    msg_format("It breathes %s.", _breath_name(gf));
    notice = device_breath(rad, p, gf, dice_roll(dice));

    gfi = gf_lookup(gf);
    if (gfi->flags & GFF_ELEMENTAL)
        device_noticed = TRUE;
    else
        device_noticed = notice;

    return "";
}
static cptr _device_los(int gf, dice_t dice)
{
    if (device_project_los(gf, dice_roll(dice)))
    {
        if (gf == GF_AWAY_EVIL) msg_print("The holy power banishes evil!");
        device_noticed = TRUE;
    }
    return "";
}
static cptr _device_one_ring(dice_t dice)
{
    switch (_1d(10))
    {
    case 1:
    case 2:
        msg_print("You are surrounded by a malignant aura.");
        /* Decrease all stats (permanently) */
        dec_stat(A_STR, 50, TRUE);
        dec_stat(A_INT, 50, TRUE);
        dec_stat(A_WIS, 50, TRUE);
        dec_stat(A_DEX, 50, TRUE);
        dec_stat(A_CON, 50, TRUE);
        dec_stat(A_CHR, 50, TRUE);
        /* Lose some experience (permanently) */
        plr->exp -= (plr->exp / 4);
        plr->max_exp -= (plr->exp / 4);
        check_experience();
        break;
    case 3:
        msg_print("You are surrounded by a powerful aura.");
        plr_project_los(GF_DISP_ALL, 1000);
        break;
    case 4:
    case 5:
    case 6:
        dice.base = 600;
        return _device_ball(3, GF_MANA, dice);
    case 7:
    case 8:
    case 9:
    case 10:
        dice.base = 500;
        return _device_bolt(GF_MANA, dice);
    }
    return "";
}
/************************************************************************
 * The Effects
 ***********************************************************************/
#define _BOOST(n) (_boost((n), boost))
cptr do_effect(effect_t *effect, int mode, int boost)
{
    bool name = (mode == SPELL_NAME);
    bool desc = (mode == SPELL_DESC);
    bool info = (mode == SPELL_INFO);
    bool cast = (mode == SPELL_CAST);
    bool value = (mode == SPELL_VALUE);
    bool color = (mode == SPELL_COLOR);
    bool cost = (mode == SPELL_COST_EXTRA);
    int  dir = 0;
    dice_t dice = {0};

    dice.scale = _boost(1000, boost); /* dice scaling is per mil; boost is per cent */

    switch (effect->type)
    {
    case EFFECT_NONE:
        if (name) return "Nothing";
        if (desc) return "It does absolutely nothing at all.";
        if (cast)
        {
            msg_print("Nothing happens.");
            device_noticed = TRUE;
        }
        break;
    /* Detection */
    case EFFECT_LIGHT_AREA:
        if (name) return "Illumination";
        if (desc) return "It lights up nearby area or current room permanently.";
        if (info) return info_damage(2 + effect->power/20, _BOOST(15), 0);
        if (value) return format("%d", 300);
        if (color) return format("%d", TERM_YELLOW);
        if (cast)
        {
            if (lite_area(_BOOST(damroll(2 + effect->power/20, 15)), 3))
                device_noticed = TRUE;
        }
        break;
    case EFFECT_LIGHT_MAP_AREA:
        if (name) return "Magic Mapping and Illumination";
        if (desc) return "It maps your vicinity and lights up your current room.";
        if (info) return info_damage(2 + effect->power/20, _BOOST(15), 0);
        if (value) return format("%d", 1300);
        if (color) return format("%d", TERM_L_BLUE);
        if (cast)
        {
            map_area(DETECT_RAD_MAP);
            lite_area(_BOOST(damroll(2 + effect->power/20, 15)), 3);
            device_noticed = TRUE;
        }
        break;
    case EFFECT_ENLIGHTENMENT:
        if (name) return "Enlightenment";
        if (desc) return "It maps your vicinity.";
        if (value) return format("%d", 1000);
        if (color) return format("%d", TERM_L_BLUE);
        if (cast)
        {
            map_area(DETECT_RAD_MAP);
            device_noticed = TRUE;
        }
        break;
    case EFFECT_CLAIRVOYANCE:
        if (name) return "Clairvoyance";
        if (desc) return "It maps, lights permanently and detects all items on the entire level.";
        if (value) return format("%d", 10000);
        if (color) return format("%d", TERM_ORANGE);
        if (cast)
        {
            virtue_add(VIRTUE_KNOWLEDGE, 1);
            virtue_add(VIRTUE_ENLIGHTENMENT, 1);
            wiz_lite();
            detect_traps(DETECT_RAD_DEFAULT, TRUE);
            detect_doors(DETECT_RAD_DEFAULT);
            detect_stairs(DETECT_RAD_DEFAULT);
            detect_recall(DETECT_RAD_DEFAULT);
            device_noticed = TRUE;
        }
        break;
    case EFFECT_DETECT_TRAPS:
        if (name) return "Detect Traps";
        if (desc) return "It detects all traps in your vicinity.";
        if (value) return format("%d", 300);
        if (color) return format("%d", TERM_L_BLUE);
        if (cast)
        {
            if (detect_traps(DETECT_RAD_DEFAULT, device_known))
                device_noticed = TRUE;
        }
        break;
    case EFFECT_DETECT_MONSTERS:
        if (name) return "Detect Monsters";
        if (desc) return "It detects all visible monsters in your vicinity.";
        if (value) return format("%d", 500);
        if (color) return format("%d", TERM_L_BLUE);
        if (cast)
        {
            if (detect_monsters_normal(DETECT_RAD_DEFAULT))
                device_noticed = TRUE;
        }
        break;
    case EFFECT_DETECT_OBJECTS:
        if (name) return "Detect Objects";
        if (desc) return "It detects all objects in your vicinity.";
        if (value) return format("%d", 500);
        if (color) return format("%d", TERM_L_BLUE);
        if (cast)
        {
            if (detect_objects_normal(DETECT_RAD_DEFAULT))
                device_noticed = TRUE;
        }
        break;
    case EFFECT_DETECT_ALL:
        if (name) return "Detection";
        if (desc) return "It detects all traps, doors, stairs, treasures, items and monsters in your vicinity.";
        if (value) return format("%d", 2000);
        if (color) return format("%d", TERM_ORANGE);
        if (cast)
        {
            detect_all(DETECT_RAD_DEFAULT);
            device_noticed = TRUE;
        }
        break;
    case EFFECT_DETECT_GOLD:
        if (name) return "Detect Treasure";
        if (desc) return "It detects all treasures in your vicinity when you use it.";
        if (value) return format("%d", 300);
        if (color) return format("%d", TERM_YELLOW);
        if (cast)
        {
            if (detect_treasure(DETECT_RAD_DEFAULT))
                device_noticed = TRUE;
            if (detect_objects_gold(DETECT_RAD_DEFAULT))
                device_noticed = TRUE;
        }
        break;
    case EFFECT_DETECT_INVISIBLE:
        if (name) return "Detect Invisible";
        if (desc) return "It detects all invisible monsters in your vicinity when you use it.";
        if (value) return format("%d", 300);
        if (color) return format("%d", TERM_L_BLUE);
        if (cast)
        {
            if (detect_monsters_invis(DETECT_RAD_DEFAULT))
                device_noticed = TRUE;
        }
        break;
    case EFFECT_DETECT_DOOR_STAIRS:
        if (name) return "Detect Doors & Stairs";
        if (desc) return "It detects all doors and stairs in your vicinity when you use it.";
        if (value) return format("%d", 300);
        if (color) return format("%d", TERM_L_BLUE);
        if (cast)
        {
            if (detect_doors(DETECT_RAD_DEFAULT))
                device_noticed = TRUE;
            if (detect_stairs(DETECT_RAD_DEFAULT))
                device_noticed = TRUE;
            if (detect_recall(DETECT_RAD_DEFAULT))
                device_noticed = TRUE;
        }
        break;
    case EFFECT_DETECT_EVIL:
        if (name) return "Detect Evil";
        if (desc) return "It detects all evil monsters in your vicinity when you use it.";
        if (value) return format("%d", 500);
        if (color) return format("%d", TERM_L_DARK);
        if (cast)
        {
            if (detect_monsters_evil(DETECT_RAD_DEFAULT))
                device_noticed = TRUE;
        }
        break;

    /* Utility */
    case EFFECT_PHASE_DOOR:
        if (name) return "Phase Door";
        if (desc) return "It teleports you a short distance.";
        if (value) return format("%d", 1000);
        if (cast)
        {
            teleport_player(10, 0L);
            if (mut_present(MUT_ASTRAL_GUIDE))
                energy_use = energy_use / 3;
            device_noticed = TRUE;
        }
        break;
    case EFFECT_TELEPORT:
        if (name) return "Teleport";
        if (desc) return "It teleports you a long distance.";
        if (value) return format("%d", 1500);
        if (cast)
        {
            teleport_player(100, 0L);
            energy_use = energy_use * 3 / 2;
            if (mut_present(MUT_ASTRAL_GUIDE))
                energy_use = energy_use / 3;
            device_noticed = TRUE;
        }
        break;
    case EFFECT_TELEPORT_AWAY:
        if (name) return "Teleport Other";
        if (desc) return "It fires a beam that teleports all affected monsters away.";
        if (value) return format("%d", 1500);
        dice.base = 5*MAX_SIGHT;
        if (cast && !plr_cast_beam(GF_TELEPORT, dice)) return NULL;
        break;
    case EFFECT_STRAFING:
        if (name) return "Strafing";
        if (desc) return "It teleports you to a nearby visible location.";
        if (value) return format("%d", 1500);
        if (cast)
        {
            if (mut_present(MUT_ASTRAL_GUIDE))
                energy_use = energy_use / 3;
            teleport_player(10, TELEPORT_LINE_OF_SIGHT);
            device_noticed = TRUE;
        }
        break;
    case EFFECT_DIMENSION_DOOR:
        if (name) return "Dimension Door";
        if (desc) return "It teleports you to a chosen location.";
        if (info) return info_range(_BOOST(effect->power / 2 + 10));
        if (value) return format("%d", 10000);
        if (cast)
        {
            if (dimension_door(_BOOST(effect->power / 2 + 10)))
                device_noticed = TRUE;
        }
        break;
    case EFFECT_ESCAPE:
        if (name) return "Getaway";
        if (desc) return "It provides a random means of escape.";
        if (value) return format("%d", 2000);
        if (cast)
        {
            switch (randint1(13))
            {
            case 1: case 2: case 3: case 4: case 5:
                if (mut_present(MUT_ASTRAL_GUIDE))
                    energy_use /= 3;
                teleport_player(10, 0L);
                break;
            case 6: case 7: case 8: case 9: case 10:
                if (mut_present(MUT_ASTRAL_GUIDE))
                    energy_use /= 3;
                teleport_player(222, 0L);
                break;
            case 11: case 12:
                dun_create_stairs(cave, FALSE);
                break;
            default:
                if (get_check("Teleport Level? "))
                    dun_teleport_level_plr(cave);
            }
            device_noticed = TRUE;
        }
        break;
    case EFFECT_RECALL:
        if (name) return "Recall";
        if (desc) return "It recalls you to the surface, or back into a dungeon you have entered.";
        if (value) return format("%d", 1000);
        if (color) return format("%d", TERM_L_BLUE);
        if (cast)
        {
            device_noticed = TRUE;
            if (!dun_mgr_recall_plr()) return NULL;
        }
        break;

    case EFFECT_STONE_TO_MUD:
        dice.dd = 1;
        dice.ds = 30;
        dice.base = 20;
        if (name) return "Stone to Mud";
        if (desc) return "It turns a door, rock, or wall to mud.";
        if (value) return format("%d", 1000);
        if (color) return format("%d", TERM_L_UMBER);
        if (cast) return _device_beam(GF_KILL_WALL, dice);
        break;
    case EFFECT_EARTHQUAKE:
        if (name) return "Earthquake";
        if (desc) return "It causes a massive earthquake nearby.";
        if (value) return format("%d", 1000);
        if (color) return format("%d", TERM_L_UMBER);
        if (cast)
        {
            if (!earthquake(plr->pos, _extra(effect, 10)))
                msg_print("The dungeon trembles.");
            device_noticed = TRUE;
        }
        break;
    case EFFECT_DESTRUCTION:
    {
        int power = _extra(effect, 150 + _power_curve_offset(400, effect->power, 50));
        if (name) return "Destruction";
        if (desc) return "It destroys everything nearby.";
        if (info) return format("Power %d", _BOOST(power));
        if (value) return format("%d", power*20);
        if (color) return format("%d", TERM_RED);
        if (cost) return format("%d", power/15);
        if (cast)
        {
            if (destroy_area(plr->pos, 13 + randint0(5), _BOOST(power)))
                device_noticed = TRUE;
            else
                msg_print("The dungeon trembles...");
        }
        break;
    }
    case EFFECT_GENOCIDE:
    {
        int power = _extra(effect, effect->power * 3);
        if (name) return "Genocide";
        if (desc) return "It eliminates an entire class of monster, exhausting you. Powerful or unique monsters may resist.";
        if (info) return format("Power %d", _BOOST(power));
        if (value) return format("%d", power*50);
        if (color) return format("%d", TERM_L_DARK);
        if (cast)
        {
            if (!symbol_genocide(_BOOST(power), TRUE)) return NULL;
            device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_MASS_GENOCIDE:
    {
        int power = _extra(effect, 100 + effect->power * 3);
        if (name) return "Mass Genocide";
        if (desc) return "It eliminates all nearby monsters, exhausting you. Powerful or unique monsters may be able to resist.";
        if (info) return format("Power %d", _BOOST(power));
        if (value) return format("%d", power*60);
        if (color) return format("%d", TERM_L_DARK);
        if (cast)
        {
            if (mass_genocide(_BOOST(power), TRUE))
                device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_RECHARGE_FROM_DEVICE:
    {
        int power = _extra(effect, 100);
        if (name) return "Recharging";
        if (desc) return "It attempts to recharge a magical device using the mana of a source device.";
        if (info) return format("Power %d", _BOOST(power));
        if (value) return format("%d", power*30);
        if (color) return format("%d", TERM_L_BLUE);
        if (cast)
        {
            device_noticed = TRUE;
            if (!recharge_from_device(_BOOST(power))) return NULL;
        }
        break;
    }
    case EFFECT_RECHARGE_FROM_PLAYER:
    {
        int power = _extra(effect, 100 + effect->power);
        if (name) return "*Recharging*";
        if (desc) return "It attempts to recharge a magical device using your mana as the source.";
        if (info) return format("Power %d", _BOOST(power));
        if (value) return format("%d", power*30);
        if (color) return format("%d", TERM_L_BLUE);
        if (cast)
        {
            device_noticed = TRUE;
            if (!recharge_from_player(_BOOST(power))) return NULL;
        }
        break;
    }
    case EFFECT_ENCHANTMENT:
        if (name) return "Enchantment";
        if (desc) return "It attempts to enchant a weapon, ammo or armor.";
        if (value) return format("%d", 5000);
        if (cast)
        {
            enchantment_hack = TRUE; /* TODO: Hephaestus only ... */
            device_noticed = TRUE;
            cast_enchantment();
            enchantment_hack = FALSE;
        }
        break;
    case EFFECT_IDENTIFY:
        if (name) return "Identify";
        if (desc) return "It identifies an item.";
        if (value) return format("%d", 500);
        if (cast)
        {
            device_noticed = TRUE;
            if (!_do_identify()) return NULL;
        }
        break;
    case EFFECT_IDENTIFY_FULL:
        if (name) return "*Identify*";
        if (desc) return "It reveals all information about an item.";
        if (value) return format("%d", 5000);
        if (cast)
        {
            device_noticed = TRUE;
            if (!identify_fully(NULL)) return NULL;
        }
        break;
    case EFFECT_PROBING:
        if (name) return "Probing";
        if (desc) return "It probes all visible monsters' alignment, HP, AC, speed, current experience and true character.";
        if (value) return format("%d", 1000);
        if (color) return format("%d", TERM_L_BLUE);
        if (cast)
        {
            if (probing()) device_noticed = TRUE;
        }
        break;
    case EFFECT_RUNE_EXPLOSIVE:
        if (name) return "Explosive Rune";
        if (desc) return "It sets a rune which will explode on a passing monster.";
        if (value) return format("%d", 500);
        if (color) return format("%d", TERM_L_RED);
        if (cast)
        {
            if (explosive_rune()) device_noticed = TRUE;
        }
        break;
    case EFFECT_RUNE_PROTECTION:
        if (name) return "Rune of Protection";
        if (desc) return "It creates a glyph that inhibits monsters from attacking you.";
        if (value) return format("%d", 5000);
        if (color) return format("%d", TERM_L_GREEN);
        if (cast)
        {
            if (warding_glyph()) device_noticed = TRUE;
        }
        break;
    case EFFECT_SATISFY_HUNGER:
        if (name) return "Satisfy Hunger";
        if (desc) return "It fills your belly with nourishing victuals.";
        if (value) return format("%d", 500);
        if (color) return format("%d", TERM_L_GREEN);
        if (cast)
        {
            if (set_food(PY_FOOD_MAX - 1)) device_noticed = TRUE;
        }
        break;
    case EFFECT_DESTROY_TRAP:
        if (name) return "Trap and Door Destruction";
        if (desc) return "It destroys all traps and doors in adjacent squares.";
        if (value) return format("%d", 500);
        if (color) return format("%d", TERM_L_RED);
        if (cast)
        {
            if (destroy_doors_touch()) device_noticed = TRUE;
        }
        break;
    case EFFECT_DESTROY_TRAPS:
        if (name) return "Unbarring Ways";
        if (desc) return "It fires a beam which destroys traps and doors.";
        if (value) return format("%d", 1000);
        if (color) return format("%d", TERM_L_RED);
        if (cast) return _device_beam(GF_KILL_DOOR, dice);
        break;
    case EFFECT_WHIRLWIND_ATTACK:
        if (name) return "Whirlwind Attack";
        if (desc) return "It causes you to attack all adjacent monsters in a single turn.";
        if (value) return format("%d", 5000);
        if (color) return format("%d", TERM_BLUE);
        if (cast)
        {
            int dir;
            for (dir = 0; dir < 8; dir++)
            {
                point_t pos = point_step(plr->pos, ddd[dir]);
                dun_cell_ptr cell = dun_cell_at(cave, pos);
                mon_ptr mon = dun_mon_at(cave, pos);
                if (mon && (mon->ml || cell_project(cell)))
                {
                    plr_attack_normal(pos);
                    device_noticed = TRUE;
                }
            }
        }
        break;
    case EFFECT_LIST_UNIQUES:
        if (name) return "List Uniques";
        if (desc) return "It lists all uniques on the current level.";
        if (value) return format("%d", 12000);
        if (color) return format("%d", TERM_ORANGE);
        if (cast) dun_iter_mon(cave, _list_unique);
        break;
    case EFFECT_LIST_ARTIFACTS:
        if (name) return "List Artifacts";
        if (desc) return "It lists all artifacts on the current level.";
        if (value) return format("%d", 15000);
        if (color) return format("%d", TERM_ORANGE);
        if (cast) dun_iter_floor_obj(cave, _list_artifact);
        break;
    case EFFECT_BANISH_EVIL:
        dice.base = _extra(effect, 100);
        if (name) return "Banish Evil";
        if (desc) return "It attempts to teleport all visible evil monsters away.";
        if (info) return dice_info_power(dice);
        if (value) return format("%d", 50*dice_roll(dice));
        if (color) return format("%d", TERM_L_DARK);
        if (cast) return _device_los(GF_AWAY_EVIL, dice);
        break;
    case EFFECT_BANISH_ALL:
        dice.base = _extra(effect, 150);
        if (name) return "Banish";
        if (desc) return "It teleports all monsters in sight away unless resisted.";
        if (info) return dice_info_power(dice);
        if (value) return format("%d", 70*dice_roll(dice));
        if (color) return format("%d", TERM_L_BLUE);
        if (cast) return _device_los(GF_TELEPORT, dice);
        break;
    case EFFECT_TELEKINESIS:
    {
        int weight = effect->power * 7;
        if (name) return "Telekinesis";
        if (desc) return "It pulls a distant item close to you.";
        if (info) return info_weight(_BOOST(weight));
        if (value) return format("%d", 8*weight);
        if (color) return format("%d", TERM_L_BLUE);
        if (cast)
        {
            if (!get_aim_dir(&dir)) return NULL;
            fetch(dir, _BOOST(weight), FALSE);
            device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_ALCHEMY:
        if (name) return "Alchemy";
        if (desc) return "It turns an item into gold.";
        if (value) return format("%d", 2000);
        if (color) return format("%d", TERM_YELLOW);
        if (cast)
        {
            if (!alchemy()) return NULL;
            device_noticed = TRUE;
        }
        break;
    case EFFECT_SELF_KNOWLEDGE:
        if (name) return "Self Knowledge";
        if (desc) return "It reveals information about your stats, resistances and life rating.";
        if (value) return format("%d", 2500);
        if (color) return format("%d", TERM_L_BLUE);
        if (cast)
        {
            self_knowledge();
            device_noticed = TRUE;
        }
        break;
    case EFFECT_GENOCIDE_ONE:
        dice.base = _extra(effect, 50 + effect->power * 3);
        if (name) return "Annihilation";
        if (desc) return "It removes a monster from current dungeon level unless resisted when you use it.";
        if (info) return dice_info_power(dice);
        if (value) return format("%d", dice_roll(dice)*50);
        if (color) return format("%d", TERM_L_DARK);
        if (cast) return _device_ball(0, GF_GENOCIDE, dice);
        break;
    /* Timed Buffs */
    case EFFECT_STONE_SKIN:
    {
        int power = _extra(effect, 20);
        if (name) return "Stone Skin";
        if (desc) return "It temporarily turns your skin to stone, granting enhanced armor class.";
        if (info) return format("Dur d%d+%d", _BOOST(power), _BOOST(power));
        if (value) return format("%d", 4000 + 50*power);
        if (color) return format("%d", TERM_L_UMBER);
        if (cast)
        {
            if (plr_tim_add(T_STONE_SKIN, _BOOST(power + randint1(power))))
                device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_RESIST_ACID:
    {
        int power = _extra(effect, 20);
        if (name) return "Resist Acid";
        if (desc) return "It grants temporary acid resistance.";
        if (info) return format("Dur d%d+%d", _BOOST(power), _BOOST(power));
        if (value) return format("%d", 1000 + 25*power);
        if (color) return format("%d", res_color(GF_ACID));
        if (cast)
        {
            if (plr_tim_add(T_RES_ACID, _BOOST(power + randint1(power))))
                device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_RESIST_ELEC:
    {
        int power = _extra(effect, 20);
        if (name) return "Resist Lightning";
        if (desc) return "It grants temporary lightning resistance.";
        if (info) return format("Dur d%d+%d", _BOOST(power), _BOOST(power));
        if (value) return format("%d", 1000 + 25*power);
        if (color) return format("%d", res_color(GF_ELEC));
        if (cast)
        {
            if (plr_tim_add(T_RES_ELEC, _BOOST(power + randint1(power))))
                device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_RESIST_FIRE:
    {
        int power = _extra(effect, 20);
        if (name) return "Resist Fire";
        if (desc) return "It grants temporary fire resistance.";
        if (info) return format("Dur d%d+%d", _BOOST(power), _BOOST(power));
        if (value) return format("%d", 1000 + 25*power);
        if (color) return format("%d", res_color(GF_FIRE));
        if (cast)
        {
            if (plr_tim_add(T_RES_FIRE, _BOOST(power + randint1(power))))
                device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_RESIST_COLD:
    {
        int power = _extra(effect, 20);
        if (name) return "Resist Cold";
        if (desc) return "It grants temporary cold resistance.";
        if (info) return format("Dur d%d+%d", _BOOST(power), _BOOST(power));
        if (value) return format("%d", 1000 + 25*power);
        if (color) return format("%d", res_color(GF_COLD));
        if (cast)
        {
            if (plr_tim_add(T_RES_COLD, _BOOST(power + randint1(power))))
                device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_RESIST_POIS:
    {
        int power = _extra(effect, 20);
        if (name) return "Resist Poison";
        if (desc) return "It grants temporary poison resistance.";
        if (info) return format("Dur d%d+%d", _BOOST(power), _BOOST(power));
        if (value) return format("%d", 2500 + 25*power);
        if (color) return format("%d", res_color(GF_POIS));
        if (cast)
        {
            if (plr_tim_add(T_RES_POIS, _BOOST(power + randint1(power))))
                device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_RESISTANCE:
    {
        int power = _extra(effect, 20);
        if (name) return "Resistance";
        if (desc) return "It grants temporary resistance to the elements and poison.";
        if (info) return format("Dur d%d+%d", _BOOST(power), _BOOST(power));
        if (value) return format("%d", 5000 + 25*power);
        if (color) return format("%d", TERM_ORANGE);
        if (cast)
        {
            int dur = _BOOST(power + randint1(power));
            if (plr_tim_add(T_RES_ACID, dur)) device_noticed = TRUE;
            if (plr_tim_add(T_RES_ELEC, dur)) device_noticed = TRUE;
            if (plr_tim_add(T_RES_FIRE, dur)) device_noticed = TRUE;
            if (plr_tim_add(T_RES_COLD, dur)) device_noticed = TRUE;
            if (plr_tim_add(T_RES_POIS, dur)) device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_PROT_EVIL:
    {
        int power = _extra(effect, 100);
        if (name) return "Protection from Evil";
        if (desc) return "It gives temporary melee protection from evil creatures.";
        if (info) return format("Dur d%d+%d", 25, _BOOST(power));
        if (value) return format("%d", 2000 + 10*power);
        if (color) return format("%d", TERM_L_DARK);
        if (cast)
        {
            if (plr_tim_add(T_PROT_EVIL, _BOOST(randint1(25) + power)))
                device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_HOLY_GRAIL:
        if (name) return "Healing and Magic Resistance";
        if (desc) return "It heals you and gives temporary resistance to magic.";
        if (info) return format("Dur d%d+%d", 10, _BOOST(10));
        if (value) return format("%d", 5000);
        if (color) return format("%d", TERM_L_BLUE);
        if (cast)
        {
            if (hp_player(50))
                device_noticed = TRUE;
            if (plr_tim_add(T_RES_MAGIC, _BOOST(10 + randint1(10))))
                device_noticed = TRUE;
        }
        break;
    case EFFECT_BLESS:
    {
        int power = _extra(effect, 24);
        if (name) return "Holy Prayer";
        if (desc) return "It blesses you temporarily when you read it.";
        if (info) return format("Dur d%d+%d", _BOOST(power), 6);
        if (value) return format("%d", 1000 + 25*power);
        if (color) return format("%d", TERM_WHITE);
        if (cast)
        {
            if (plr_tim_add(T_BLESSED, _BOOST(randint1(power) + 6)))
                device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_HEROISM:
    {
        int power = _extra(effect, 25);
        if (name) return "Heroism";
        if (desc) return "It grants temporary heroism.";
        if (info) return format("Dur d%d+%d", _BOOST(power), _BOOST(power));
        if (value) return format("%d", 1500 + 25*power);
        if (color) return format("%d", TERM_L_RED);
        if (cast)
        {
            if (plr_tim_add(T_HERO, _BOOST(randint1(power) + power)))
                device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_BERSERK:
    {
        int power = _extra(effect, 25);
        if (name) return "Berserk";
        if (desc) return "It causes you to enter a berserk rage, granting enhanced combat prowess but diminished stealth and skills.";
        if (info) return format("Dur d%d+%d", _BOOST(power), _BOOST(power));
        if (value) return format("%d", 1500 + 25*power);
        if (color) return format("%d", TERM_RED);
        if (cast)
        {
            if (plr_tim_add(T_BERSERK, _BOOST(randint1(power) + power)))
                device_noticed = TRUE;
            if (hp_player(30))
                device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_SPEED:
    {
        int power = _extra(effect, 20);
        if (name) return "Speed";
        if (desc) return "It grants a temporary speed boost.";
        if (info) return format("Dur d%d+%d", _BOOST(power), _BOOST(power));
        if (value) return format("%d", 2500 + 25*power);
        if (color) return format("%d", TERM_L_RED);
        if (cast)
        {
            if (plr_tim_add(T_FAST, _BOOST(randint1(power) + power)))
                device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_SPEED_HERO:
    {
        int power = _extra(effect, effect->power/2);
        if (name) return "Heroic Speed";
        if (desc) return "It grants temporary speed and heroism.";
        if (info) return format("Dur d%d+%d", _BOOST(power), _BOOST(power));
        if (value) return format("%d", 5000 + 25*power);
        if (color) return format("%d", TERM_L_RED);
        if (cast)
        {
            int dur = _BOOST(randint1(power) + power);
            if (plr_tim_add(T_FAST, dur)) device_noticed = TRUE;
            if (plr_tim_add(T_HERO, dur)) device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_SPEED_HERO_BLESS:
    {
        int power = _extra(effect, 15);
        if (name) return "Heroic Song";
        if (desc) return "It grants temporary speed, blessing and heroism.";
        if (info) return format("Dur d%d+%d", _BOOST(power), _BOOST(power));
        if (value) return format("%d", 5000 + 30*power);
        if (color) return format("%d", TERM_L_RED);
        if (cast)
        {
            int dur = _BOOST(randint1(power) + power);
            if (plr_tim_add(T_FAST, dur)) device_noticed = TRUE;
            if (plr_tim_add(T_HERO, dur)) device_noticed = TRUE;
            if (plr_tim_add(T_BLESSED, dur)) device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_LIGHT_SPEED:
    {
        int power = _extra(effect, 16);
        if (name) return "Light Speed";
        if (desc) return "It temporarily grants you impossible powers of motion.";
        if (info) return format("Dur %d", _BOOST(power));
        if (value) return format("%d", 5000 + 500*power);
        if (color) return format("%d", TERM_VIOLET);
        if (cast)
        {
            if (plr_tim_add(T_LIGHT_SPEED, _BOOST(power)))
                device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_ENLARGE_WEAPON:
    {
        int power = _extra(effect, 7);
        if (name) return "Enlarge Weapon";
        if (desc) return "It temporarily increases the damage dice of your melee weapon.";
        if (info) return format("Dur %d", _BOOST(power));
        if (value) return format("%d", 2000 + 250*power);
        if (color) return format("%d", TERM_ORANGE);
        if (cast)
        {
            if (plr_tim_add(T_ENLARGE_WEAPON, _BOOST(power)))
                device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_TELEPATHY:
    {
        int power = _extra(effect, 30);
        if (name) return "Telepathy";
        if (desc) return "It grants you the power of telepathy temporarily.";
        if (info) return format("Dur d%d+%d", _BOOST(power), _BOOST(25));
        if (value) return format("%d", 2000 + 50*power);
        if (color) return format("%d", TERM_L_BLUE);
        if (cast)
        {
            if (plr_tim_add(T_TELEPATHY, _BOOST(randint1(power) + 25)))
                device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_WRAITHFORM:
    {
        int power = _extra(effect, 25);
        if (name) return "Wraithform";
        if (desc) return "It turns you int a wraith, giving the ability to pass through walls as well as reducing the amount of physical damage sustained from attacks.";
        if (info) return format("Dur d%d+%d", _BOOST(power), _BOOST(power));
        if (value) return format("%d", 10000 + 100*power);
        if (color) return format("%d", TERM_L_DARK);
        if (cast)
        {
            if (plr_tim_add(T_WRAITH, _BOOST(randint1(power) + power)))
                device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_INVULNERABILITY:
    {
        if (name) return "Globe of Invulnerability";
        if (desc) return "It generates barrier which completely protect you from almost all damages. Takes a few your turns when the barrier breaks or duration time is exceeded.";
        if (value) return format("%d", 10000);
        if (color) return format("%d", TERM_L_BLUE);
        if (cast)
        {
            if (plr_tim_add(T_INVULN, _BOOST(500 + _1d(1000))))
                device_noticed = TRUE;
        }
        break;
    }

    /* Pets */
    case EFFECT_SUMMON_MONSTERS:
        if (name) return "Summon Monsters";
        if (desc) return "It attempts to summon monsters for assistance.";
        if (value) return format("%d", 1000);
        if (cast)
        {
            int num = randint1(3);
            int i;
            for (i = 0; i < num; i++)
            {
                if (summon_specific(who_create_plr(), plr->pos, cave->dun_lvl, 0, PM_FORCE_PET | PM_ALLOW_GROUP))
                    device_noticed = TRUE;
            }
        }
        break;
    case EFFECT_SUMMON_HOUNDS:
        if (name) return "Summon Hounds";
        if (desc) return "It attempts to summon hounds for assistance.";
        if (value) return format("%d", 1200);
        if (cast)
        {
            int num = randint1(3);
            int i;
            for (i = 0; i < num; i++)
            {
                if (summon_specific(who_create_plr(), plr->pos, cave->dun_lvl, SUMMON_HOUND, PM_FORCE_PET | PM_ALLOW_GROUP))
                    device_noticed = TRUE;
            }
        }
        break;
    case EFFECT_SUMMON_ANTS:
        if (name) return "Summon Ants";
        if (desc) return "It attempts to summon ants for assistance.";
        if (value) return format("%d", 1200);
        if (cast)
        {
            int num = randint1(3);
            int i;
            for (i = 0; i < num; i++)
            {
                if (summon_specific(who_create_plr(), plr->pos, cave->dun_lvl, SUMMON_ANT, PM_FORCE_PET | PM_ALLOW_GROUP))
                    device_noticed = TRUE;
            }
        }
        break;
    case EFFECT_SUMMON_HYDRAS:
        if (name) return "Summon Hydras";
        if (desc) return "It attempts to summon hydras for assistance.";
        if (value) return format("%d", 1200);
        if (cast)
        {
            int num = randint1(3);
            int i;
            for (i = 0; i < num; i++)
            {
                if (summon_specific(who_create_plr(), plr->pos, cave->dun_lvl, SUMMON_HYDRA, PM_FORCE_PET | PM_ALLOW_GROUP))
                    device_noticed = TRUE;
            }
        }
        break;
    case EFFECT_SUMMON_OCTOPUS:
        if (name) return "Summon Octopus";
        if (desc) return "It attempts to summon octopi for assistance.";
        if (value) return format("%d", 1200);
        if (cast)
        {
            int num = randint0(3);
            int i;
            for (i = 0; i < num; i++)
            {
                if (summon_named_creature(who_create_plr(), plr->pos, mon_race_parse("l.kshitigarbha"), PM_FORCE_PET | PM_ALLOW_GROUP))
                    device_noticed = TRUE;
            }
        }
        break;
    case EFFECT_SUMMON_DAWN:
        if (name) return "Summon Legion of the Dawn";
        if (desc) return "It attempts to summon Warriors of the Dawn for assistance.";
        if (value) return format("%d", 2500);
        if (cast)
        {
            if (summon_specific(who_create_plr(), plr->pos, cave->dun_lvl, SUMMON_DAWN, (PM_ALLOW_GROUP | PM_FORCE_PET)))
            {
                msg_print("You summon the Legion of the Dawn.");
                device_noticed = TRUE;
            }
        }
        break;
    case EFFECT_SUMMON_PHANTASMAL:
        if (name) return "Summon Phantasmal Servant";
        if (desc) return "It attempts to summon a single Phantasmal Servant for assistance.";
        if (value) return format("%d", 1000);
        if (cast)
        {
            if (summon_specific(who_create_plr(), plr->pos, cave->dun_lvl, SUMMON_PHANTOM, (PM_ALLOW_GROUP | PM_FORCE_PET)))
            {
                msg_print("You summon a phantasmal servant.");
                device_noticed = TRUE;
            }
        }
        break;
    case EFFECT_SUMMON_ELEMENTAL:
        if (name) return "Summon Elemental";
        if (desc) return "It attempts to conjure a single elemental to serve you.";
        if (value) return format("%d", 1000);
        if (cast)
        {
            bool pet = one_in_(3);
            int  lvl = cave->dun_lvl;
            u32b mode = pet ? PM_FORCE_PET : PM_NO_PET;
            who_t  who = pet ? who_create_plr() : who_create_null();

            if (!pet || lvl >= 50)
                mode |= PM_ALLOW_GROUP;

            if (summon_specific(who, plr->pos, lvl, SUMMON_ELEMENTAL, mode))
            {
                device_noticed = TRUE;
                msg_print("An elemental materializes...");
                if (pet)
                    msg_print("It seems obedient to you.");
                else
                    msg_print("You fail to control it!");
            }
        }
        break;
    case EFFECT_SUMMON_DRAGON:
        if (name) return "Summon Dragon";
        if (desc) return "It attempts to summon a single dragon for assistance.";
        if (value) return format("%d", 1500);
        if (cast)
        {
            if (summon_specific(who_create_plr(), plr->pos, cave->dun_lvl, SUMMON_DRAGON, PM_FORCE_PET))
                device_noticed = TRUE;
        }
        break;
    case EFFECT_SUMMON_UNDEAD:
        if (name) return "Summon Undead";
        if (desc) return "It attempts to summon a single undead monster to serve you.";
        if (value) return format("%d", 1500);
        if (color) return format("%d", TERM_L_DARK);
        if (cast)
        {
            bool pet = one_in_(3);
            int  lvl = cave->dun_lvl;
            int  type = lvl > 75 ? SUMMON_HI_UNDEAD : SUMMON_UNDEAD;
            u32b mode = pet ? PM_FORCE_PET : PM_NO_PET;
            who_t  who = pet ? who_create_plr() : who_create_null();

            if (!pet || lvl >= 50)
                mode |= PM_ALLOW_GROUP;

            if (summon_specific(who, plr->pos, lvl, type, mode))
            {
                device_noticed = TRUE;
                msg_print("Cold winds begin to blow around you, carrying with them the stench of decay...");
                if (pet)
                    msg_print("Ancient, long-dead forms arise from the ground to serve you!");
                else
                    msg_print("'The dead arise... to punish you for disturbing them!'");
            }
        }
        break;
    case EFFECT_SUMMON_DEMON:
        if (name) return "Summon Demon";
        if (desc) return "It attempts to summon a single demon to serve you.";
        if (value) return format("%d", 1500);
        if (color) return format("%d", TERM_RED);
        if (cast)
        {
            bool pet = one_in_(3);
            int  lvl = cave->dun_lvl;
            u32b mode = pet ? PM_FORCE_PET : PM_NO_PET;
            who_t  who = pet ? who_create_plr() : who_create_null();

            if (!pet || lvl >= 50)
                mode |= PM_ALLOW_GROUP;

            if (summon_specific(who, plr->pos, lvl, SUMMON_DEMON, mode))
            {
                device_noticed = TRUE;
                msg_print("The area fills with a stench of sulphur and brimstone.");
                if (pet)
                    msg_print("'What is thy bidding... Master?'");
                else
                    msg_print("'NON SERVIAM! Wretch! I shall feast on thy mortal soul!'");
            }
        }
        break;
    case EFFECT_SUMMON_CYBERDEMON:
        if (name) return "Summon Cyberdemon";
        if (desc) return "It attempts to summon a single Cyberdemon for assistance.";
        if (value) return format("%d", 7500);
        if (color) return format("%d", TERM_VIOLET);
        if (cast)
        {
            if (summon_specific(who_create_plr(), plr->pos, cave->dun_lvl, SUMMON_CYBER, PM_FORCE_PET))
                device_noticed = TRUE;
        }
        break;
    case EFFECT_SUMMON_ANGEL:
        if (name) return "Summon Angel";
        if (desc) return "It attempts to summon a single angel for assistance.";
        if (value) return format("%d", 5000);
        if (color) return format("%d", TERM_YELLOW);
        if (cast)
        {
            if (summon_specific(who_create_plr(), plr->pos, cave->dun_lvl, SUMMON_ANGEL, PM_FORCE_PET))
                device_noticed = TRUE;
        }
        break;
    case EFFECT_SUMMON_KRAKEN:
        if (name) return "Summon Kraken";
        if (desc) return "It attempts to summon powerful kraken for assistance.";
        if (value) return format("%d", 5000);
        if (color) return format("%d", TERM_BLUE);
        if (cast)
        {
            int num = randint0(3);
            int ct = 0;
            int i;
            plr_burst(3, GF_WATER_FLOW, 3);
            device_noticed = TRUE;
            for (i = 0; i < num; i++)
                ct += summon_specific(who_create_plr(), plr->pos, cave->dun_lvl, SUMMON_KRAKEN, PM_FORCE_PET);
            if (!ct)
                msg_print("No help arrives.");
        }
        break;

    case EFFECT_CHARM_ANIMAL:
        dice.base = _extra(effect, effect->power);
        if (name) return "Charm Animal";
        if (desc) return "It attempts to charm a single animal.";
        if (info) return dice_info_power(dice);
        if (value) return format("%d", 10*dice_roll(dice));
        if (cast) return _device_bolt(GF_CONTROL_ANIMAL, dice);
        break;
    case EFFECT_CHARM_DEMON:
        dice.base = _extra(effect, effect->power);
        if (name) return "Dominate Demon";
        if (desc) return "It attempts to dominate a single demon.";
        if (info) return dice_info_power(dice);
        if (value) return format("%d", 15*dice_roll(dice));
        if (cast) return _device_bolt(GF_CONTROL_DEMON, dice);
        break;
    case EFFECT_CHARM_UNDEAD:
        dice.base = _extra(effect, effect->power);
        if (name) return "Enslave Undead";
        if (desc) return "It attempts to enslave a single undead monster.";
        if (info) return dice_info_power(dice);
        if (value) return format("%d", 15*dice_roll(dice));
        if (cast) return _device_bolt(GF_CONTROL_UNDEAD, dice);
        break;
    case EFFECT_CHARM_MONSTER:
        dice.base = _extra(effect, effect->power);
        if (name) return "Charm Monster";
        if (desc) return "It attempts to charm a single monster.";
        if (info) return dice_info_power(dice);
        if (value) return format("%d", 15*dice_roll(dice));
        if (cast) return _device_bolt(GF_CHARM, dice);
        break;
    case EFFECT_RETURN_PETS:
        if (name) return "Return Pets";
        if (desc) return "It calls your pets back to you.";
        if (value) return format("%d", 500);
        if (cast)
        {
            mon_pack_ptr pets = plr_pack();
            int i, ct = mon_pack_count(pets);

            assert(cave->id == plr->dun_id);
            stop_mouth(); /* this is for TV_WHISTLE */
            for (i = 0;  i < ct; i++)
            {
                mon_ptr pet = vec_get(pets->members, i);
                if (pet->dun->id != plr->dun_id) continue;
                teleport_monster_to(pet, plr->pos, 100, TELEPORT_PASSIVE);
                device_noticed = TRUE;
            }
        }
        break;

    case EFFECT_CAPTURE_PET:
        if (name) return "Capture Pet";
        if (desc) return "It attempts to capture targetted monster.";
        if (value) return format("%d", 500);
        if (cast)
        {
            /* TODO: This is handled elsewhere in cm6.c, since we need the object_type
               for the capture ball in order to "reconstitute" the captured pet.
             */
        }
        break;

    /* Healing and Recovery */
    case EFFECT_RESTORE_STATS:
        if (name) return "Restore Stats";
        if (desc) return "It restores your stats.";
        if (value) return format("%d", 5000);
        if (color) return format("%d", TERM_L_GREEN);
        if (cast)
        {
            if (do_res_stat(A_STR)) device_noticed = TRUE;
            if (do_res_stat(A_INT)) device_noticed = TRUE;
            if (do_res_stat(A_WIS)) device_noticed = TRUE;
            if (do_res_stat(A_DEX)) device_noticed = TRUE;
            if (do_res_stat(A_CON)) device_noticed = TRUE;
            if (do_res_stat(A_CHR)) device_noticed = TRUE;
        }
        break;
    case EFFECT_RESTORE_EXP:
        if (name) return "Restore Life";
        if (desc) return "It restores your life and experience.";
        if (value) return format("%d", 1000);
        if (color) return format("%d", TERM_L_GREEN);
        if (cast)
        {
            if (restore_level()) device_noticed = TRUE;
            if (plr_restore_life(150)) device_noticed = TRUE;
        }
        break;
    case EFFECT_RESTORING:
        if (name) return "Restoring";
        if (desc) return "It restores your stats, life and experience.";
        if (value) return format("%d", 6000);
        if (color) return format("%d", TERM_L_GREEN);
        if (cast)
        {
            if (do_res_stat(A_STR)) device_noticed = TRUE;
            if (do_res_stat(A_INT)) device_noticed = TRUE;
            if (do_res_stat(A_WIS)) device_noticed = TRUE;
            if (do_res_stat(A_DEX)) device_noticed = TRUE;
            if (do_res_stat(A_CON)) device_noticed = TRUE;
            if (do_res_stat(A_CHR)) device_noticed = TRUE;
            if (restore_level()) device_noticed = TRUE;
            if (plr_restore_life(1000)) device_noticed = TRUE;
        }
        break;
    case EFFECT_HEAL:
    {
        int amt = _extra(effect, 25 + effect->power);
        if (name) return (amt < 100) ? "Cure Wounds" : "Healing";
        if (desc) return "It heals your hitpoints and cures cuts.";
        if (info) return info_heal(0, 0, _BOOST(amt));
        if (value) return format("%d", 15*amt);
        if (color) return format("%d", TERM_YELLOW);
        if (cast)
        {
            amt = _BOOST(amt);

            if (hp_player(amt)) device_noticed = TRUE;
            if (amt >= 100)
            {
                if (plr_tim_remove(T_CUT)) device_noticed = TRUE;
            }
            else
            {
                plr_tim_subtract(T_CUT, amt);
            }
        }
        break;
    }
    case EFFECT_CURING:
    {
        if (name) return "Curing";
        if (desc) return "It cures blindness, poison, confusion, stunning, cuts and hallucination.";
        if (value) return format("%d", 1000);
        if (color) return format("%d", TERM_L_GREEN);
        if (cast)
        {
            if (plr_tim_remove(T_BLIND)) device_noticed = TRUE;
            if (plr_tim_recover(T_POISON, 80, 100)) device_noticed = TRUE;
            if (plr_tim_remove(T_CONFUSED)) device_noticed = TRUE;
            if (plr_tim_remove(T_STUN)) device_noticed = TRUE;
            if (plr_tim_remove(T_CUT)) device_noticed = TRUE;
            if (plr_tim_remove(T_HALLUCINATE)) device_noticed = TRUE;
            if (plr_tim_remove(T_BERSERK)) device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_HEAL_CURING:
    {
        int amt = _extra(effect, 30 + 4*effect->power);
        if (amt < 100)
        {
            if (name) return "Cure Wounds";
            if (desc) return "It heals your hitpoints and cures blindness and cuts.";
        }
        else
        {
            if (name) return "Healing";
            if (desc) return "It heals your hitpoints and cures your ailments.";
        }
        if (info) return info_heal(0, 0, _BOOST(amt));
        if (value) return format("%d", 500 + 15*amt);
        if (color) return format("%d", TERM_YELLOW);
        if (cost) return format("%d", amt/10);
        if (cast)
        {
            amt = _BOOST(amt);

            if (hp_player(amt)) device_noticed = TRUE;
            if (plr_tim_remove(T_BLIND)) device_noticed = TRUE;
            if (amt >= 100)
            {
                if (plr_tim_remove(T_CUT)) device_noticed = TRUE;
                if (plr_tim_remove(T_CONFUSED)) device_noticed = TRUE;
                if (plr_tim_remove(T_STUN)) device_noticed = TRUE;
            }
            else
            {
                plr_tim_subtract(T_CUT, amt);
            }
        }
        break;
    }
    case EFFECT_HEAL_CURING_HERO:
    {
        int amt = _extra(effect, 300 + _power_curve_offset(477, effect->power, 70));
        if (name) return "Angelic Healing";
        if (desc) return "It heals your hitpoints, cures what ails you, and makes you heroic.";
        if (info) return info_heal(0, 0, _BOOST(amt));
        /* XXX The following is too low for -AngelicHealing, but avoids over-valuing Lohengrin */
        if (value) return format("%d", 750 + 15*amt);
        if (color) return format("%d", TERM_YELLOW);
        if (cast)
        {
            if (hp_player(_BOOST(amt))) device_noticed = TRUE;
            if (plr_tim_remove(T_BLIND)) device_noticed = TRUE;
            if (plr_tim_remove(T_CUT)) device_noticed = TRUE;
            if (plr_tim_remove(T_CONFUSED)) device_noticed = TRUE;
            if (plr_tim_recover(T_POISON, 50, 300))
                device_noticed = TRUE;
            if (plr_tim_remove(T_STUN)) device_noticed = TRUE;
            if (plr_tim_add(T_HERO, _BOOST(randint1(25) + 25))) device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_RESTORE_MANA:
        if (name) return "Restore Mana";
        if (desc) return "It completely restores your mana. It also partially recharges any devices in your pack.";
        if (value) return format("%d", 10000);
        if (color) return format("%d", TERM_L_BLUE);
        if (cast)
        {
            if (restore_mana()) device_noticed = TRUE;
            if (plr_tim_remove(T_BERSERK)) device_noticed = TRUE;
        }
        break;
    case EFFECT_CURE_POIS:
        if (name) return "Cure Poison";
        if (desc) return "It cures poison.";
        if (value) return format("%d", 500);
        if (color) return format("%d", res_color(GF_POIS));
        if (cast)
        {
            if (plr_tim_recover(T_POISON, 80, 100))
                device_noticed = TRUE;
        }
        break;
    case EFFECT_CURE_FEAR:
        if (name) return "Boldness";
        if (desc) return "It restores your courage.";
        if (value) return format("%d", 750);
        if (color) return format("%d", res_color(GF_FEAR));
        if (cast)
        {
            if (plr->afraid)
            {
                fear_clear_p();
                device_noticed = TRUE;
            }
        }
        break;
    case EFFECT_CURE_FEAR_POIS:
        if (name) return "Cure Fear and Poison";
        if (desc) return "It cures poison and restores your courage in battle.";
        if (value) return format("%d", 1250);
        if (color) return format("%d", res_color(GF_FEAR));
        if (cast)
        {
            if (plr_tim_recover(T_POISON, 90, 10))
                device_noticed = TRUE;
            if (plr->afraid)
            {
                fear_clear_p();
                device_noticed = TRUE;
            }
        }
        break;
    case EFFECT_REMOVE_CURSE:
        if (name) return "Remove Curse";
        if (desc) return "It removes normal curses from equipped items.";
        if (value) return format("%d", 1000);
        if (color) return format("%d", TERM_L_GREEN);
        if (cast)
        {
            if (remove_curse())
            {
                msg_print("You feel as if someone is watching over you.");
                device_noticed = TRUE;
            }
        }
        break;
    case EFFECT_REMOVE_ALL_CURSE:
        if (name) return "*Remove Curse*";
        if (desc) return "It removes normal and heavy curses from equipped items.";
        if (value) return format("%d", 5000);
        if (color) return format("%d", TERM_L_GREEN);
        if (cast)
        {
            if (remove_all_curse())
            {
                msg_print("You feel as if someone is watching over you.");
                device_noticed = TRUE;
            }
        }
        break;
    case EFFECT_CLARITY:
    {
        int amt = _extra(effect, effect->cost);
        if (name) return "Clarity";
        if (desc) return "It clears your mind, restoring some mana.";
        if (info) return format("%dsp", _BOOST(amt));
        if (value) return format("%d", 1000 + 50*amt);
        if (color) return format("%d", TERM_L_BLUE);
        if (cast)
        {
            if (plr->pclass == CLASS_RUNE_KNIGHT)
                msg_print("You are unaffected.");
            else if (sp_player(_BOOST(amt)))
                device_noticed = TRUE;
            if (plr_tim_remove(T_BERSERK)) device_noticed = TRUE;
            if (plr_tim_remove(T_STUN)) device_noticed = TRUE;
            if (device_noticed)
                msg_print("You feel your mind clear.");
        }
        break;
    }
    case EFFECT_GREAT_CLARITY: /* FYI: This is a separate effect from EFFECT_CLARITY for purposes */
    {                          /*      of statistics tracking, which is by effect id. */
        int amt = _extra(effect, effect->cost);
        if (name) return "Great Clarity";
        if (desc) return "It clears your mind, restoring some mana.";
        if (info) return format("%dsp", _BOOST(amt));
        if (value) return format("%d", 1000 + 50*amt);
        if (color) return format("%d", TERM_L_BLUE);
        if (cast)
        {
            if (plr->pclass == CLASS_RUNE_KNIGHT)
                msg_print("You are unaffected.");
            else if (sp_player(_BOOST(amt)))
                device_noticed = TRUE;
            if (plr_tim_remove(T_BERSERK)) device_noticed = TRUE;
            if (plr_tim_remove(T_STUN)) device_noticed = TRUE;
            if (device_noticed)
                msg_print("You feel your mind clear.");
        }
        break;
    }

    /* Offense: Bolts */
    case EFFECT_BOLT_MISSILE:
        dice.dd = _extra(effect, 2 + effect->power/10);
        dice.ds = 6;
        if (name) return "Magic Missile";
        if (desc) return "It fires a weak bolt of magic.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 20*dice_avg_roll(dice));
        if (cast) return _device_bolt(GF_MISSILE, dice);
        break;
    case EFFECT_BOLT_ACID:
        dice.dd = _extra(effect, 6 + effect->power/7);
        dice.ds = 8;
        if (name) return "Acid Bolt";
        if (desc) return "It fires a bolt of acid.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 30*dice_avg_roll(dice));
        if (color) return format("%d", res_color(GF_ACID));
        if (cast) return _device_bolt(GF_ACID, dice);
        break;
    case EFFECT_BOLT_ELEC:
        dice.dd = _extra(effect, 4 + effect->power/9);
        dice.ds = 8;
        if (name) return "Lightning Bolt";
        if (desc) return "It fires a bolt of lightning.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 25*dice_avg_roll(dice));
        if (color) return format("%d", res_color(GF_ELEC));
        if (cast) return _device_bolt(GF_ELEC, dice);
        break;
    case EFFECT_BOLT_FIRE:
        dice.dd = _extra(effect, 7 + effect->power/6);
        dice.ds = 8;
        if (name) return "Fire Bolt";
        if (desc) return "It fires a bolt of fire.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 25*dice_avg_roll(dice));
        if (color) return format("%d", res_color(GF_FIRE));
        if (cast) return _device_bolt(GF_FIRE, dice);
        break;
    case EFFECT_BOLT_COLD:
        dice.dd = _extra(effect, 5 + effect->power/8);
        dice.ds = 8;
        if (name) return "Frost Bolt";
        if (desc) return "It fires a bolt of frost.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 25*dice_avg_roll(dice));
        if (color) return format("%d", res_color(GF_COLD));
        if (cast) return _device_bolt(GF_COLD, dice);
        break;
    case EFFECT_BOLT_POIS:
        dice.dd = _extra(effect, 5 + effect->power/8);
        dice.ds = 8;
        if (name) return "Poison Dart";
        if (desc) return "It fires a poison dart.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 20*dice_avg_roll(dice));
        if (color) return format("%d", res_color(GF_POIS));
        if (cast) return _device_bolt(GF_POIS, dice);
        break;
    case EFFECT_BOLT_LIGHT:
        dice.dd = _extra(effect, 5 + effect->power/8);
        dice.ds = 8;
        if (name) return "Light Bolt";
        if (desc) return "It fires a bolt of light.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 30*dice_avg_roll(dice));
        if (color) return format("%d", res_color(GF_LIGHT));
        if (cast) return _device_bolt(GF_LIGHT, dice);
        break;
    case EFFECT_BOLT_DARK:
        dice.dd = _extra(effect, 5 + effect->power/8);
        dice.ds = 8;
        if (name) return "Dark Bolt";
        if (desc) return "It fires a bolt of darkness.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 30*dice_avg_roll(dice));
        if (color) return format("%d", res_color(GF_DARK));
        if (cast) return _device_bolt(GF_DARK, dice);
        break;
    case EFFECT_BOLT_CONF:
        dice.dd = _extra(effect, 5 + effect->power/8);
        dice.ds = 8;
        if (name) return "Confusion Bolt";
        if (desc) return "It fires a bolt of confusion.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 25*dice_avg_roll(dice));
        if (color) return format("%d", res_color(GF_CONFUSION));
        if (cast) return _device_bolt(GF_CONFUSION, dice);
        break;
    case EFFECT_BOLT_NETHER:
        dice.dd = _extra(effect, 7 + effect->power/6);
        dice.ds = 8;
        if (name) return "Nether Bolt";
        if (desc) return "It fires a bolt of nether.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 30*dice_avg_roll(dice));
        if (color) return format("%d", res_color(GF_NETHER));
        if (cast) return _device_bolt(GF_NETHER, dice);
        break;
    case EFFECT_BOLT_NEXUS:
        dice.dd = _extra(effect, 7 + effect->power/6);
        dice.ds = 8;
        if (name) return "Nexus Bolt";
        if (desc) return "It fires a bolt of nexus.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 35*dice_avg_roll(dice));
        if (color) return format("%d", res_color(GF_NEXUS));
        if (cast) return _device_bolt(GF_NEXUS, dice);
        break;
    case EFFECT_BOLT_SOUND:
        dice.dd = _extra(effect, 7 + effect->power/6);
        dice.ds = 8;
        if (name) return "Sound Bolt";
        if (desc) return "It fires a bolt of sound.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 45*dice_avg_roll(dice));
        if (color) return format("%d", res_color(GF_SOUND));
        if (cast) return _device_bolt(GF_SOUND, dice);
        break;
    case EFFECT_BOLT_SHARDS:
        dice.dd = _extra(effect, 7 + effect->power/5);
        dice.ds = 8;
        if (name) return "Shard Bolt";
        if (desc) return "It fires a bolt of shards.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 45*dice_avg_roll(dice));
        if (color) return format("%d", res_color(GF_SHARDS));
        if (cast) return _device_bolt(GF_SHARDS, dice);
        break;
    case EFFECT_BOLT_CHAOS:
        dice.dd = _extra(effect, 7 + effect->power/6);
        dice.ds = 8;
        if (name) return "Chaos Bolt";
        if (desc) return "It fires a bolt of chaos.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 35*dice_avg_roll(dice));
        if (color) return format("%d", res_color(GF_CHAOS));
        if (cast) return _device_bolt(GF_CHAOS, dice);
        break;
    case EFFECT_BOLT_DISEN:
        dice.dd = _extra(effect, 7 + effect->power/6);
        dice.ds = 8;
        if (name) return "Disenchantment Bolt";
        if (desc) return "It fires a bolt of disenchantment.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 35*dice_avg_roll(dice));
        if (color) return format("%d", res_color(GF_DISEN));
        if (cast) return _device_bolt(GF_DISENCHANT, dice);
        break;
    case EFFECT_BOLT_TIME:
        dice.dd = _extra(effect, 7 + effect->power/6);
        dice.ds = 8;
        if (name) return "Time Bolt";
        if (desc) return "It fires a bolt of time.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 45*dice_avg_roll(dice));
        if (color) return format("%d", res_color(GF_TIME));
        if (cast) return _device_bolt(GF_TIME, dice);
        break;
    case EFFECT_BOLT_WATER:
        dice.dd = 1;
        dice.ds = _extra(effect, _power_curve(400, effect->power));
        dice.base = 20;
        if (name) return "Water Bolt";
        if (desc) return "It fires a bolt of water.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 40*dice_avg_roll(dice));
        if (color) return format("%d", TERM_BLUE);
        if (cost) return format("%d", dice_avg_roll(dice)/7);
        if (cast) return _device_bolt(GF_WATER, dice);
        break;
    case EFFECT_BOLT_MANA:
        dice.dd = 1;
        dice.ds = _extra(effect, _power_curve(500, effect->power));
        dice.base = 50;
        if (name) return "Mana Bolt";
        if (desc) return "It fires a powerful bolt of mana.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 40*dice_avg_roll(dice));
        if (color) return format("%d", TERM_L_BLUE);
        if (cost) return format("%d", dice_avg_roll(dice)/8);
        if (cast) return _device_bolt(GF_MANA, dice);
        break;
    case EFFECT_BOLT_ICE:
        dice.dd = 1;
        dice.ds = _extra(effect, _power_curve(400, effect->power));
        dice.base = 30;
        if (name) return "Ice Bolt";
        if (desc) return "It fires a bolt of ice.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 40*dice_avg_roll(dice));
        if (color) return format("%d", res_color(GF_COLD));
        if (cost) return format("%d", dice_avg_roll(dice)/7);
        if (cast) return _device_bolt(GF_ICE, dice);
        break;
    case EFFECT_BOLT_PLASMA:
        dice.dd = 1;
        dice.ds = _extra(effect, _power_curve(400, effect->power));
        dice.base = 40;
        if (name) return "Plasma Bolt";
        if (desc) return "It fires a bolt of plasma.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 40*dice_avg_roll(dice));
        if (color) return format("%d", res_color(GF_FIRE));
        if (cost) return format("%d", dice_avg_roll(dice)/7);
        if (cast) return _device_bolt(GF_PLASMA, dice);
        break;

    /* Offense: Beams */
    case EFFECT_BEAM_LIGHT_WEAK:
        dice.dd = _extra(effect, 6);
        dice.ds = 8;
        if (name) return "Beam of Light";
        if (desc) return "It fires a beam of light.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 20*dice_avg_roll(dice));
        if (color) return format("%d", res_color(GF_LIGHT));
        if (cast) return _device_beam(GF_LIGHT_WEAK, dice);
        break;
    case EFFECT_BEAM_LIGHT:
        dice.base = _extra(effect, 10 + _power_curve(275, effect->power));
        if (name) return "Beam of Light";
        if (desc) return "It fires a powerful beam of light.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 40*dice_avg_roll(dice));
        if (color) return format("%d", res_color(GF_LIGHT));
        if (cost) return format("%d", dice_avg_roll(dice)/7);
        if (cast) return _device_beam(GF_LIGHT, dice);
        break;
    case EFFECT_BEAM_GRAVITY:
        dice.dd = _extra(effect, 9 + effect->power/8);
        dice.ds = 8;
        if (name) return "Beam of Gravity";
        if (desc) return "It fires a beam of gravity.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 55*dice_avg_roll(dice));
        if (color) return format("%d", TERM_L_UMBER);
        if (cast) return _device_beam(GF_GRAVITY, dice);
        break;
    case EFFECT_BEAM_DISINTEGRATE:
        dice.dd = _extra(effect, 9 + effect->power/8);
        dice.ds = 8;
        if (name) return "Beam of Disintegration";
        if (desc) return "It fires a beam of disintegration.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 40*dice_avg_roll(dice));
        if (color) return format("%d", TERM_SLATE);
        if (cast) return _device_beam(GF_DISINTEGRATE, dice);
        break;
    case EFFECT_BEAM_ACID:
        dice.base = _extra(effect, 5 + _power_curve(270, effect->power));
        if (name) return "Shoot Acid";
        if (desc) return "It fires a beam of acid.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 35*dice_avg_roll(dice));
        if (color) return format("%d", res_color(GF_ACID));
        if (cost) return format("%d", dice_avg_roll(dice)/6);
        if (cast) return _device_beam(GF_ACID, dice);
        break;
    case EFFECT_BEAM_ELEC:
        dice.base = _extra(effect, 5 + _power_curve(250, effect->power));
        if (name) return "Lightning Strike";
        if (desc) return "It fires a beam of lightning.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 30*dice_avg_roll(dice));
        if (color) return format("%d", res_color(GF_ELEC));
        if (cost) return format("%d", dice_avg_roll(dice)/6);
        if (cast) return _device_beam(GF_ELEC, dice);
        break;
    case EFFECT_BEAM_FIRE:
        dice.base = _extra(effect, 5 + _power_curve(280, effect->power));
        if (name) return "Line of Fire";
        if (desc) return "It fires a beam of fire.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 30*dice_avg_roll(dice));
        if (color) return format("%d", res_color(GF_FIRE));
        if (cost) return format("%d", dice_avg_roll(dice)/6);
        if (cast) return _device_beam(GF_FIRE, dice);
        break;
    case EFFECT_BEAM_COLD:
        dice.base = _extra(effect, 5 + _power_curve(260, effect->power));
        if (name) return "Ray of Cold";
        if (desc) return "It fires a beam of frost.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 30*dice_avg_roll(dice));
        if (color) return format("%d", res_color(GF_COLD));
        if (cost) return format("%d", dice_avg_roll(dice)/6);
        if (cast) return _device_beam(GF_COLD, dice);
        break;
    case EFFECT_BEAM_SOUND:
        dice.dd = _extra(effect, 7 + effect->power/6);
        dice.ds = 8;
        if (name) return "Sound Strike";
        if (desc) return "It fires a beam of sound.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 50*dice_avg_roll(dice));
        if (color) return format("%d", res_color(GF_SOUND));
        if (cost) return format("%d", dice_avg_roll(dice)/5);
        if (cast) return _device_beam(GF_SOUND, dice);
        break;
    case EFFECT_BEAM_CHAOS:
        dice.dd = _extra(effect, 7 + effect->power/6);
        dice.ds = 8;
        if (name) return "Chaos Strike";
        if (desc) return "It fires a beam of chaos.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 40*dice_avg_roll(dice));
        if (color) return format("%d", res_color(GF_CHAOS));
        if (cost) return format("%d", dice_avg_roll(dice)/5);
        if (cast) return _device_beam(GF_CHAOS, dice);
        break;

    /* Offense: Balls */
    case EFFECT_BALL_ACID:
        dice.base = _extra(effect, 20 + _power_curve(300, effect->power));
        if (name) return "Acid Ball";
        if (desc) return "It fires a ball of acid.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 35*dice_avg_roll(dice));
        if (color) return format("%d", res_color(GF_ACID));
        if (cost) return format("%d", dice_avg_roll(dice)/8);
        if (cast) return _device_ball(2, GF_ACID, dice);
        break;
    case EFFECT_BALL_ELEC:
        dice.base = _extra(effect, 20 + _power_curve(250, effect->power));
        if (name) return "Lightning Ball";
        if (desc) return "It fires a ball of lightning.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 30*dice_avg_roll(dice));
        if (color) return format("%d", res_color(GF_ELEC));
        if (cost) return format("%d", dice_avg_roll(dice)/8);
        if (cast) return _device_ball(2, GF_ELEC, dice);
        break;
    case EFFECT_BALL_FIRE:
        dice.base = _extra(effect, 20 + _power_curve(350, effect->power));
        if (name) return "Fire Ball";
        if (desc) return "It fires a ball of fire.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 30*dice_avg_roll(dice));
        if (color) return format("%d", res_color(GF_FIRE));
        if (cost) return format("%d", dice_avg_roll(dice)/8);
        if (cast) return _device_ball(2, GF_FIRE, dice);
        break;
    case EFFECT_BALL_COLD:
        dice.base = _extra(effect, 20 + _power_curve(275, effect->power));
        if (name) return "Frost Ball";
        if (desc) return "It fires a ball of frost.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 30*dice_avg_roll(dice));
        if (color) return format("%d", res_color(GF_COLD));
        if (cost) return format("%d", dice_avg_roll(dice)/8);
        if (cast) return _device_ball(2, GF_COLD, dice);
        break;
    case EFFECT_BALL_POIS:
        dice.base = _extra(effect, 12 + effect->power/4);
        if (name) return "Stinking Cloud";
        if (desc) return "It fires a ball of poison.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 20*dice_avg_roll(dice));
        if (color) return format("%d", res_color(GF_POIS));
        if (cast) return _device_ball(2, GF_POIS, dice);
        break;
    case EFFECT_BALL_LIGHT:
        dice.base = _extra(effect, 200 + _power_curve_offset(350, effect->power, 80));
        if (name) return "Star Burst";
        if (desc) return "It fires a huge ball of powerful light.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 50*dice_avg_roll(dice));
        if (color) return format("%d", res_color(GF_LIGHT));
        if (cost) return format("%d", dice_avg_roll(dice)/10);
        if (cast) return _device_ball(4, GF_LIGHT, dice);
        break;
    case EFFECT_BALL_DARK:
        dice.base = _extra(effect, 100 + 7*effect->power/2);
        if (name) return "Darkness Storm";
        if (desc) return "It fires a huge ball of darkness.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 50*dice_avg_roll(dice));
        if (color) return format("%d", res_color(GF_DARK));
        if (cost) return format("%d", dice_avg_roll(dice)/10);
        if (cast) return _device_ball(4, GF_DARK, dice);
        break;
    case EFFECT_BALL_CONF:
        dice.base = _extra(effect, 30 + effect->power);
        if (name) return "Confusion Ball";
        if (desc) return "It fires a ball of confusion.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 30*dice_avg_roll(dice));
        if (color) return format("%d", res_color(GF_CONFUSION));
        if (cast) return _device_ball(3, GF_CONFUSION, dice);
        break;
    case EFFECT_BALL_NETHER:
        dice.base = _extra(effect, 50 + _power_curve_offset(200, effect->power, 30));
        if (name) return "Nether Ball";
        if (desc) return "It fires a ball of nether.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 25*dice_avg_roll(dice));
        if (color) return format("%d", res_color(GF_NETHER));
        if (cost) return format("%d", dice_avg_roll(dice)/10);
        if (cast) return _device_ball(3, GF_NETHER, dice);
        break;
    case EFFECT_BALL_NEXUS:
        dice.base = _extra(effect, 100 + _power_curve_offset(200, effect->power, 40));
        if (name) return "Nexus Ball";
        if (desc) return "It fires a ball of nexus.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 40*dice_avg_roll(dice));
        if (color) return format("%d", res_color(GF_NEXUS));
        if (cost) return format("%d", dice_avg_roll(dice)/8);
        if (cast) return _device_ball(3, GF_NEXUS, dice);
        break;
    case EFFECT_BALL_SOUND:
        dice.base = _extra(effect, 70 + _power_curve_offset(280, effect->power, 40));
        if (name) return "Sound Ball";
        if (desc) return "It fires a ball of sound.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 50*dice_avg_roll(dice));
        if (color) return format("%d", res_color(GF_SOUND));
        if (cost) return format("%d", dice_avg_roll(dice)/8);
        if (cast) return _device_ball(3, GF_SOUND, dice);
        break;
    case EFFECT_BALL_SHARDS:
        dice.base = _extra(effect, 175 + _power_curve_offset(325, effect->power, 75));
        if (name) return "Shard Ball";
        if (desc) return "It fires a ball of shards.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 50*dice_avg_roll(dice));
        if (color) return format("%d", res_color(GF_SHARDS));
        if (cost) return format("%d", dice_avg_roll(dice)/10);
        if (cast) return _device_ball(2, GF_SHARDS, dice);
        break;
    case EFFECT_BALL_CHAOS:
        dice.base = _extra(effect, 150 + _power_curve_offset(350, effect->power, 70));
        if (name) return "Invoke Logrus";
        if (desc) return "It fires a huge ball of chaos.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 40*dice_avg_roll(dice));
        if (color) return format("%d", res_color(GF_CHAOS));
        if (cost) return format("%d", dice_avg_roll(dice)/10);
        if (cast) return _device_ball(5, GF_CHAOS, dice);
        break;
    case EFFECT_BALL_DISEN:
        dice.base = _extra(effect, 90 + _power_curve_offset(250, effect->power, 40));
        if (name) return "Disenchantment Ball";
        if (desc) return "It fires a ball of disenchantment.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 40*dice_avg_roll(dice));
        if (color) return format("%d", res_color(GF_DISEN));
        if (cost) return format("%d", dice_avg_roll(dice)/9);
        if (cast) return _device_ball(3, GF_DISENCHANT, dice);
        break;
    case EFFECT_BALL_TIME:
        dice.base = _extra(effect, 50 + effect->power);
        if (name) return "Temporal Storm";
        if (desc) return "It fires a ball of time.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 50*dice_avg_roll(dice));
        if (color) return format("%d", res_color(GF_TIME));
        if (cast) return _device_ball(3, GF_TIME, dice);
        break;
    case EFFECT_BALL_WATER:
        dice.base = _extra(effect, 150 + _power_curve_offset(200, effect->power, 50));
        if (name) return "Whirlpool";
        if (desc) return "It fires a huge ball of water.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 45*dice_avg_roll(dice));
        if (color) return format("%d", TERM_BLUE);
        if (cost) return format("%d", dice_avg_roll(dice)/9);
        if (cast) return _device_ball(4, GF_WATER, dice);
        break;
    case EFFECT_BALL_MANA:
        dice.base = _extra(effect, 150 + _power_curve_offset(300, effect->power, 60));
        if (name) return "Mana Ball";
        if (desc) return "It fires a powerful ball of mana.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 50*dice_avg_roll(dice));
        if (color) return format("%d", TERM_L_BLUE);
        if (cost) return format("%d", dice_avg_roll(dice)/10);
        if (cast) return _device_ball(2, GF_MANA, dice);
        break;
    case EFFECT_BALL_DISINTEGRATE:
        dice.base = _extra(effect, 150 + _power_curve_offset(200, effect->power, 50));
        if (name) return "Disintegrate";
        if (desc) return "It fires a powerful ball of disintegration.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 45*dice_avg_roll(dice));
        if (color) return format("%d", TERM_SLATE);
        if (cost) return format("%d", dice_avg_roll(dice)/9);
        if (cast) return _device_ball(2, GF_DISINTEGRATE, dice);
        break;

    /* Offense: Breaths */
    case EFFECT_BREATHE_ACID:
        dice.base = _extra(effect, 100 + effect->power*3);
        if (name) return "Breathe Acid";
        if (desc) return "It breathes acid.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 40*dice_avg_roll(dice));
        if (color) return format("%d", res_color(GF_ACID));
        if (cast) return _device_breath(3, GF_ACID, dice);
        break;
    case EFFECT_BREATHE_ELEC:
        dice.base = _extra(effect, 70 + effect->power*3);
        if (name) return "Breathe Lightning";
        if (desc) return "It breathes lightning.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 35*dice_avg_roll(dice));
        if (color) return format("%d", res_color(GF_ELEC));
        if (cast) return _device_breath(3, GF_ELEC, dice);
        break;
    case EFFECT_BREATHE_FIRE:
        dice.base = _extra(effect, 160 + _power_curve_offset(300, effect->power, 40));
        if (name) return "Dragon's Flame";
        if (desc) return "It breathes fire.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 35*dice_avg_roll(dice));
        if (color) return format("%d", res_color(GF_FIRE));
        if (cost) return format("%d", dice_avg_roll(dice)/8);
        if (cast) return _device_breath(3, GF_FIRE, dice);
        break;
    case EFFECT_BREATHE_COLD:
        dice.base = _extra(effect, 150 + _power_curve_offset(300, effect->power, 40));
        if (name) return "Dragon's Frost";
        if (desc) return "It breathes frost.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 35*dice_avg_roll(dice));
        if (color) return format("%d", res_color(GF_COLD));
        if (cost) return format("%d", dice_avg_roll(dice)/8);
        if (cast) return _device_breath(3, GF_COLD, dice);
        break;
    case EFFECT_BREATHE_POIS:
        dice.base = _extra(effect, 60 + effect->power*2);
        if (name) return "Breathe Poison";
        if (desc) return "It breathes poison.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 30*dice_avg_roll(dice));
        if (color) return format("%d", res_color(GF_POIS));
        if (cast) return _device_breath(3, GF_POIS, dice);
        break;
    case EFFECT_BREATHE_LIGHT:
        dice.base = _extra(effect, 50 + effect->power*2);
        if (name) return "Breathe Light";
        if (desc) return "It breathes light.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 40*dice_avg_roll(dice));
        if (color) return format("%d", res_color(GF_LIGHT));
        if (cast) return _device_breath(3, GF_LIGHT, dice);
        break;
    case EFFECT_BREATHE_DARK:
        dice.base = _extra(effect, 50 + effect->power*2);
        if (name) return "Breathe Darkness";
        if (desc) return "It breathes darkness.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 40*dice_avg_roll(dice));
        if (color) return format("%d", res_color(GF_DARK));
        if (cast) return _device_breath(3, GF_DARK, dice);
        break;
    case EFFECT_BREATHE_CONF:
        dice.base = _extra(effect, 50 + effect->power*2);
        if (name) return "Breathe Confusion";
        if (desc) return "It breathes confusion.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 35*dice_avg_roll(dice));
        if (color) return format("%d", res_color(GF_CONFUSION));
        if (cast) return _device_breath(3, GF_CONFUSION, dice);
        break;
    case EFFECT_BREATHE_NETHER:
        dice.base = _extra(effect, 75 + effect->power*2);
        if (name) return "Breathe Nether";
        if (desc) return "It breathes nether.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 30*dice_avg_roll(dice));
        if (color) return format("%d", res_color(GF_NETHER));
        if (cast) return _device_breath(3, GF_NETHER, dice);
        break;
    case EFFECT_BREATHE_NEXUS:
        dice.base = _extra(effect, 50 + effect->power*2);
        if (name) return "Breathe Nexus";
        if (desc) return "It breathes nexus.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 45*dice_avg_roll(dice));
        if (color) return format("%d", res_color(GF_NEXUS));
        if (cast) return _device_breath(3, GF_NEXUS, dice);
        break;
    case EFFECT_BREATHE_SOUND:
        dice.base = _extra(effect, 50 + effect->power*2);
        if (name) return "Breathe Sound";
        if (desc) return "It breathes sound.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 55*dice_avg_roll(dice));
        if (color) return format("%d", res_color(GF_SOUND));
        if (cast) return _device_breath(3, GF_SOUND, dice);
        break;
    case EFFECT_BREATHE_SHARDS:
        dice.base = _extra(effect, 100 + effect->power*2);
        if (name) return "Breathe Shards";
        if (desc) return "It breathes shards.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 55*dice_avg_roll(dice));
        if (color) return format("%d", res_color(GF_SHARDS));
        if (cast) return _device_breath(3, GF_SHARDS, dice);
        break;
    case EFFECT_BREATHE_CHAOS:
        dice.base = _extra(effect, 75 + effect->power*2);
        if (name) return "Breathe Chaos";
        if (desc) return "It breathes chaos.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 45*dice_avg_roll(dice));
        if (color) return format("%d", res_color(GF_CHAOS));
        if (cast) return _device_breath(3, GF_CHAOS, dice);
        break;
    case EFFECT_BREATHE_DISEN:
        dice.base = _extra(effect, 50 + effect->power*2);
        if (name) return "Breathe Disenchantment";
        if (desc) return "It breathes disenchantment.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 45*dice_avg_roll(dice));
        if (color) return format("%d", res_color(GF_DISEN));
        if (cast) return _device_breath(3, GF_DISENCHANT, dice);
        break;
    case EFFECT_BREATHE_TIME:
        dice.base = _extra(effect, 50 + effect->power*2);
        if (name) return "Breathe Time";
        if (desc) return "It breathes time.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 55*dice_avg_roll(dice));
        if (color) return format("%d", res_color(GF_TIME));
        if (cast) return _device_breath(3, GF_TIME, dice);
        break;
    case EFFECT_BREATHE_ONE_MULTIHUED:
        dice.base = _extra(effect, 170 + _power_curve_offset(300, effect->power, 40));
        if (name) return "Dragon's Breath";
        if (desc) return "It breathes acid, lightning, fire, frost or poison.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 40*dice_avg_roll(dice));
        if (color) return format("%d", TERM_ORANGE);
        if (cost) return format("%d", dice_avg_roll(dice)/8);
        if (cast)
        {
            int gf[5] = { GF_ACID, GF_ELEC, GF_FIRE, GF_COLD, GF_POIS };
            int i = randint0(5);
            return _device_breath(3, gf[i], dice);
        }
        break;
    case EFFECT_BREATHE_ONE_CHAOS:
        dice.base = _extra(effect, 75 + effect->power*2);
        if (name) return "Breathe";
        if (desc) return "It breathes chaos or disenchantment.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 45*dice_avg_roll(dice));
        if (color) return format("%d", res_color(GF_CHAOS));
        if (cast)
        {
            int gf[2] = { GF_CHAOS, GF_DISENCHANT };
            int i = randint0(2);
            return _device_breath(3, gf[i], dice);
        }
        break;
    case EFFECT_BREATHE_ONE_LAW:
        dice.base = _extra(effect, 100 + effect->power*2);
        if (name) return "Breathe";
        if (desc) return "It breathes sound or shards.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 55*dice_avg_roll(dice));
        if (color) return format("%d", res_color(GF_SOUND));
        if (cast)
        {
            int gf[2] = { GF_SOUND, GF_SHARDS };
            int i = randint0(2);
            return _device_breath(3, gf[i], dice);
        }
        break;
    case EFFECT_BREATHE_ONE_BALANCE:
        dice.base = _extra(effect, 100 + effect->power*2);
        if (name) return "Breathe";
        if (desc) return "It breathes sound, shards, chaos or disenchantment.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 55*dice_avg_roll(dice));
        if (color) return format("%d", res_color(GF_DISEN));
        if (cast)
        {
            int gf[4] = { GF_SOUND, GF_SHARDS, GF_CHAOS, GF_DISENCHANT };
            int i = randint0(4);
            return _device_breath(4, gf[i], dice);
        }
        break;
    case EFFECT_BREATHE_ONE_SHINING:
        dice.base = _extra(effect, 50 + effect->power*2);
        if (name) return "Breathe";
        if (desc) return "It breathes light or darkness.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 40*dice_avg_roll(dice));
        if (color) return format("%d", res_color(GF_LIGHT));
        if (cast)
        {
            int gf[2] = { GF_LIGHT, GF_DARK };
            int i = randint0(2);
            return _device_breath(3, gf[i], dice);
        }
        break;
    case EFFECT_BREATHE_ELEMENTS:
        dice.base = _extra(effect, 100 + effect->power*2);
        if (name) return "Breathe Elements";
        if (desc) return "It breathes the elements.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 55*dice_avg_roll(dice));
        if (color) return format("%d", TERM_VIOLET);
        if (cast) return _device_breath(4, GF_MISSILE, dice);
        break;
    case EFFECT_BREATHE_HOLY_FIRE:
        dice.base = _extra(effect, 100 + effect->power*2);
        if (name) return "Breathe Holy Fire";
        if (desc) return "It breathes holy fire to punish evil.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 77*dice_avg_roll(dice));
        if (color) return format("%d", TERM_YELLOW);
        if (cast) return _device_breath(5, GF_HOLY_FIRE, dice);
        break;
    case EFFECT_BREATHE_HELL_FIRE:
        dice.base = _extra(effect, 100 + effect->power*2);
        if (name) return "Breathe Hell Fire";
        if (desc) return "It breathes hell fire to destroy the good.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 66*dice_avg_roll(dice));
        if (color) return format("%d", TERM_L_DARK);
        if (cast) return _device_breath(5, GF_HELL_FIRE, dice);
        break;

    /* Offense: Other */
    case EFFECT_DISPEL_EVIL:
        dice.base = _extra(effect, 100 + _power_curve_offset(200, effect->power, 50));
        if (name) return "Dispel Evil";
        if (desc) return "It damages all evil monsters in sight.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 30*dice_roll(dice));
        if (color) return format("%d", TERM_YELLOW);
        if (cost) return format("%d", dice_roll(dice)/8);
        if (cast) return _device_los(GF_DISP_EVIL, dice);
        break;
    case EFFECT_DISPEL_EVIL_HERO:
        dice.base = _extra(effect, 2*effect->power);
        if (name) return "Dispel Evil";
        if (desc) return "It damages all evil monsters in sight and grants temporary heroism.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 500 + 30*dice_roll(dice));
        if (color) return format("%d", TERM_YELLOW);
        if (cost) return format("%d", dice_roll(dice)/8);
        if (cast)
        {
            _device_los(GF_DISP_EVIL, dice);
            plr_tim_add(T_HERO, 25 + _1d(25));
        }
        break;
    case EFFECT_DISPEL_GOOD:
        dice.base = _extra(effect, 2*effect->power);
        if (name) return "Dispel Good";
        if (desc) return "It damages all good monsters in sight.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 20*dice_roll(dice));
        if (color) return format("%d", TERM_L_DARK);
        if (cost) return format("%d", dice_roll(dice)/12);
        if (cast) return _device_los(GF_DISP_GOOD, dice);
        break;
    case EFFECT_DISPEL_LIFE:
        dice.base = _extra(effect, 100 + _power_curve_offset(200, effect->power, 50));
        if (name) return "Dispel Life";
        if (desc) return "It damages all living monsters in sight.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 30*dice_roll(dice));
        if (color) return format("%d", TERM_L_DARK);
        if (cost) return format("%d", dice_roll(dice)/9);
        if (cast) return _device_los(GF_DISP_LIVING, dice);
        break;
    case EFFECT_DISPEL_DEMON:
        dice.base = _extra(effect, 150 + _power_curve_offset(350, effect->power, 50));
        if (name) return "Dispel Demons";
        if (desc) return "It damages all demonic monsters in sight.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 20*dice_roll(dice));
        if (color) return format("%d", TERM_YELLOW);
        if (cost) return format("%d", dice_roll(dice)/15);
        if (cast) return _device_los(GF_DISP_DEMON, dice);
        break;
    case EFFECT_DISPEL_UNDEAD:
        dice.base = _extra(effect, 150 + _power_curve_offset(350, effect->power, 50));
        if (name) return "Dispel Undead";
        if (desc) return "It damages all undead monsters in sight.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 20*dice_roll(dice));
        if (color) return format("%d", TERM_YELLOW);
        if (cost) return format("%d", dice_roll(dice)/15);
        if (cast) return _device_los(GF_DISP_UNDEAD, dice);
        break;
    case EFFECT_DISPEL_MONSTERS:
        dice.base = _extra(effect, 100 + _power_curve_offset(150, effect->power, 50));
        if (name) return "Dispel Monsters";
        if (desc) return "It damages all monsters in sight.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 40*dice_roll(dice));
        if (color) return format("%d", TERM_YELLOW);
        if (cost) return format("%d", dice_roll(dice)/8);
        if (cast) return _device_los(GF_DISP_ALL, dice);
        break;
    case EFFECT_DRAIN_LIFE:
        dice.base = _extra(effect, 50 + effect->power/2);
        if (name) return "Vampirism";
        if (desc) return "It fires a bolt that steals life from a foe when you use it.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 35*dice_roll(dice));
        if (color) return format("%d", TERM_L_DARK);
        if (cast) {
            point_t p = get_fire_pos();
            int d = dice_roll(dice);
            if (!dun_pos_interior(plr_dun(), p)) return NULL;
            if (device_bolt(p, GF_OLD_DRAIN, d))
            {
                vamp_player(d);
                device_noticed = TRUE;
            }
        }
        break;
    case EFFECT_STAR_BALL:
        dice.base = _extra(effect, 150);
        if (name) return "Star Ball";
        if (desc) return "It fires a multitude of lightning balls in random directions.";
        if (info) return dice_info_dam_each(dice);
        if (value) return format("%d", 50*dice_roll(dice));
        if (color) return format("%d", TERM_YELLOW);
        if (cast) {
            plr_star_ball(_5d(3), GF_ELEC, dice);
            device_noticed = TRUE;
        }
        break;
    case EFFECT_WRATH_OF_GOD:
        dice.base = _extra(effect, 25 + effect->power*3/2);
        if (name) return "Wrath of the God";
        if (desc) return "It drops many balls of disintegration near the target.";
        if (info) return dice_info_dam_each(dice);
        if (value) return format("%d", 50*dice_roll(dice));
        if (cast && !plr_cast_wrath_of_god(GF_DISINTEGRATE, dice)) return NULL;
        break;
    case EFFECT_ROCKET:
        dice.base = _extra(effect, 200 + _power_curve_offset(300, effect->power, 60));
        if (name) return "Rocket";
        if (desc) return "It fires a rocket.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 50*dice_avg_roll(dice));
        if (color) return format("%d", TERM_UMBER);
        if (cost) return format("%d", dice_avg_roll(dice)/11);
        if (cast) return _device_rocket(2, dice);
        break;
    case EFFECT_METEOR:
        dice.dd = _extra(effect, 15 + effect->power/5);
        dice.ds = 13;
        if (name) return "Meteor";
        if (desc) return "It fires a meteor when you use it.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 40*dice_avg_roll(dice));
        if (color) return format("%d", TERM_UMBER);
        if (cast) return _device_bolt(GF_METEOR, dice);
        break;
    case EFFECT_MANA_STORM:
        dice.base = _extra(effect, 375 + _power_curve_offset(200, effect->power, 80));
        if (name) return "Mana Storm";
        if (desc) return "It produces a huge mana ball centered on you.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 45*dice_avg_roll(dice));
        if (color) return format("%d", TERM_RED);
        if (cost) return format("%d", dice_avg_roll(dice)/10);
        if (cast)
        {
            msg_print("Mighty magics rend your enemies!");
            device_burst(5, GF_MANA, dice_roll(dice));
            device_noticed = TRUE;
        }
        break;
    case EFFECT_CONFUSING_LIGHT:
    {
        int pow = _extra(effect, effect->power);
        if (name) return "Confusing Lights";
        if (desc) return "It emits dazzling lights which slow, stun, confuse, scare and even freeze nearby monsters.";
        if (info) return format("Power %d", pow);
        if (value) return format("%d", 60*pow);
        if (color) return format("%d", res_color(GF_CONFUSION));
        if (cast) 
        {
            msg_print("It glares nearby monsters with a dazzling array of confusing lights!");
            pow = _BOOST(pow);
            confusing_lights(pow);
            device_noticed = TRUE; /* You see the dazzling lights, no? */
        }
        break;
    }
    case EFFECT_ARROW:
        dice.base = _extra(effect, 70 + effect->power);
        if (name) return "Magic Arrow";
        if (desc) return "It fires a powerful magical arrow.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 30*dice_avg_roll(dice));
        if (color) return format("%d", TERM_SLATE);
        if (cost) return format("%d", dice_avg_roll(dice)/8);
        if (cast) return _device_bolt(GF_ARROW, dice);
        break;
    case EFFECT_HOLINESS:
        dice.base = _extra(effect, effect->power*2);
        if (name) return "Holiness";
        if (desc) return "It does damage to all evil monsters in sight, gives temporary protection from lesser evil creature, cures poison, stunned, cuts, removes fear and heals you when you use it.";
        if (info) return dice_info_power(dice);
        if (value) return format("%d", 5000 + 30*dice_roll(dice));
        if (color) return format("%d", TERM_YELLOW);
        if (cast)
        {
            _device_los(GF_DISP_EVIL, dice); /* only Dispel Evil learns the device */
            plr_tim_add(T_PROT_EVIL, dice_roll(dice)/2);
            hp_player(dice_roll(dice));
            plr_tim_remove(T_STUN);
            plr_tim_remove(T_CUT);
        }
        break;
    case EFFECT_STARBURST:
        dice.base = _extra(effect, 375 + _power_curve_offset(200, effect->power, 80));
        if (name) return "Star Burst";
        if (desc) return "It produces a huge ball of light centered on you.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 45*dice_avg_roll(dice));
        if (color) return format("%d", TERM_YELLOW);
        if (cost) return format("%d", dice_avg_roll(dice)/10);
        if (cast)
        {
            if (!res_save_default(GF_BLIND) && !res_save_default(GF_LIGHT))
                plr_tim_add(T_BLIND, 3 + randint1(5));
            device_burst(5, GF_LIGHT, dice_roll(dice));
            device_noticed = TRUE;
        }
        break;
    case EFFECT_DARKNESS_STORM:
        dice.base = _extra(effect, 375 + _power_curve_offset(200, effect->power, 80));
        if (name) return "Darkness Storm";
        if (desc) return "It produces a huge ball of darkness centered on you.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 45*dice_avg_roll(dice));
        if (color) return format("%d", TERM_L_DARK);
        if (cost) return format("%d", dice_avg_roll(dice)/10);
        if (cast)
        {
            if (!res_save_default(GF_BLIND) && !res_save_default(GF_DARK))
                plr_tim_add(T_BLIND, 3 + randint1(5));
            device_burst(5, GF_DARK, dice_roll(dice));
            device_noticed = TRUE;
        }
        break;
    case EFFECT_PESTICIDE:
        dice.base = _extra(effect, 4);
        if (name) return "Pesticide";
        if (desc) return "It does slight damage to all monsters in sight when you zap it.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 250);
        if (color) return format("%d", res_color(GF_POIS));
        if (cast) return _device_los(GF_DISP_ALL, dice);
        break;

    /* Misc */
    case EFFECT_POLY_SELF:
        if (name) return "Polymorph";
        if (desc) return "It mutates you. Warning: You might not like the results!";
        if (value) return format("%d", 500);
        if (color) return format("%d", TERM_RED);
        if (cast)
        {
            if (get_check("This might be risky. Are you sure? "))
            {
                do_poly_self();
                device_noticed = TRUE;
            }
        }
        break;
    case EFFECT_ANIMATE_DEAD:
        if (name) return "Animate Dead";
        if (desc) return "It raises corpses and skeletons nearby you from dead and makes them your pet when you use it.";
        if (value) return format("%d", 750);
        if (color) return format("%d", TERM_L_DARK);
        if (cast)
        {
            if (plr_animate_dead())
                device_noticed = TRUE;
        }
        break;
    case EFFECT_SCARE_MONSTERS:
        dice.base = _extra(effect, effect->power*3);
        if (name) return "Terrify Monsters";
        if (desc) return "It attempts to frighten all nearby visible monsters.";
        if (info) return dice_info_power(dice);
        if (value) return format("%d", 10*dice_roll(dice));
        if (color) return format("%d", TERM_L_RED);
        if (cast) return _device_los(GF_FEAR, dice);
        break;
    case EFFECT_SLEEP_MONSTERS:
        dice.base = _extra(effect, 10 + effect->power);
        if (name) return "Sleep Monsters";
        if (desc) return "It attempts to sleep all nearby visible monsters.";
        if (info) return dice_info_power(dice);
        if (value) return format("%d", 15*dice_roll(dice));
        if (color) return format("%d", TERM_BLUE);
        if (cast) return _device_los(GF_SLEEP, dice);
        break;
    case EFFECT_SLOW_MONSTERS:
        dice.base = _extra(effect, 10 + effect->power);
        if (name) return "Slow Monsters";
        if (desc) return "It attempts to slow all nearby visible monsters.";
        if (info) return dice_info_power(dice);
        if (value) return format("%d", 15*dice_roll(dice));
        if (color) return format("%d", TERM_UMBER);
        if (cast) return _device_los(GF_SLOW, dice);
        break;
    case EFFECT_STASIS_MONSTERS:
        dice.base = _extra(effect, 10 + effect->power*3/2);
        if (name) return "Freeze Monsters";
        if (desc) return "It attempts to freeze all nearby visible monsters.";
        if (info) return dice_info_power(dice);
        if (value) return format("%d", 30*dice_roll(dice));
        if (color) return format("%d", TERM_BLUE);
        if (cast) return _device_los(GF_STASIS, dice);
        break;
    case EFFECT_CONFUSE_MONSTERS:
        dice.base = _extra(effect, 10 + effect->power);
        if (name) return "Confuse Monsters";
        if (desc) return "It attempts to confuse all nearby visible monsters.";
        if (info) return dice_info_power(dice);
        if (value) return format("%d", 15*dice_roll(dice));
        if (color) return format("%d", res_color(GF_CONFUSION));
        if (cast) return _device_los(GF_OLD_CONF, dice);
        break;
    case EFFECT_CHARGE:
        if (name) return "Charge";
        if (desc) return "If riding, you charge a chosen foe doing extra damage.";
        if (value) return format("%d", 5000);
        if (color) return format("%d", TERM_L_RED);
        if (cast)
        {
            bool charged = FALSE;
            /* For the lance activation, the player really should be riding.
               At the moment, only the Heavy Lance 'Impaler' has this effect. */
            if (!plr->riding)
            {
                msg_print("You need to be mounted in order to charge.");
                return NULL;
            }
            charged = rush_attack(7, NULL);
            if (!charged) return NULL;
        }
        break;
    case EFFECT_PIERCING_SHOT:
        if (name) return "Piercing Shot";
        if (desc) return "It shoots a bolt through multiple foes.";
        if (value) return format("%d", 1500);
        if (color) return format("%d", TERM_L_RED);
        if (cast)
        {
            if (!plr_shoot_special(PLR_SHOOT_PIERCE, 0)) return NULL;
            device_known = TRUE;
        }
        break;
    case EFFECT_ENDLESS_QUIVER: /* should only be on a quiver ... */
        if (name) return "Endless Quiver";
        if (desc) return "Your quiver will refill with average ammo.";
        if (value) return format("%d", 1500);
        if (color) return format("%d", TERM_L_RED);
        if (cast)
        {
            obj_t forge = {0};
            int   tval = plr->shooter_info.tval_ammo;

            if (!tval) tval = TV_ARROW;

            object_prep(&forge, lookup_kind(tval, SV_ARROW)); /* Hack: SV_ARROW == SV_BOLT == SV_PEBBLE */
            forge.number = MAX(0, MIN(50, quiver_capacity() - quiver_count(NULL)));
            obj_identify_fully(&forge);

            if (!forge.number)
                msg_print("Your quiver is full.");
            else
            {
                msg_print("Your quiver refills.");
                quiver_carry(&forge);
            }
        }
        break;
    case EFFECT_WALL_BUILDING:
        if (name) return "Wall Building";
        if (desc) return "It creates a wall of stone.";
        if (value) return format("%d", 50000);
        if (color) return format("%d", TERM_UMBER);
        if (cast) return _device_beam(GF_MAKE_WALL, dice);
        break;
    case EFFECT_SLEEP_MONSTER:
        dice.base = _extra(effect, 10 + effect->power);
        if (name) return "Sleep Monster";
        if (desc) return "It puts a monster to sleep when you use it.";
        if (info) return dice_info_power(dice);
        if (value) return format("%d", 10*dice_roll(dice));
        if (color) return format("%d", TERM_BLUE);
        if (cast) return _device_bolt(GF_SLEEP, dice);
        break;
    case EFFECT_SLOW_MONSTER:
        dice.base = _extra(effect, 10 + effect->power);
        if (name) return "Slow Monster";
        if (desc) return "It slows a monster down when you use it.";
        if (info) return dice_info_power(dice);
        if (value) return format("%d", 500);
        if (color) return format("%d", TERM_UMBER);
        if (cast) return _device_bolt(GF_SLOW, dice);
        break;
    case EFFECT_CONFUSE_MONSTER:
        dice.base = _extra(effect, 10 + effect->power);
        if (name) return "Confuse Monster";
        if (desc) return "It confuses a monster when you use it.";
        if (info) return dice_info_power(dice);
        if (value) return format("%d", 10*dice_roll(dice));
        if (color) return format("%d", res_color(GF_CONFUSION));
        if (cast) return _device_bolt(GF_OLD_CONF, dice);
        break;
    case EFFECT_SCARE_MONSTER:
        dice.base = _extra(effect, 10 + effect->power);
        if (name) return "Scare Monster";
        if (desc) return "It scares a monster when you use it.";
        if (info) return dice_info_power(dice);
        if (value) return format("%d", 10*dice_roll(dice));
        if (color) return format("%d", res_color(GF_FEAR));
        if (cast) return _device_bolt(GF_FEAR, dice);
        break;
    case EFFECT_POLYMORPH:
        dice.base = _extra(effect, 10 + effect->power);
        if (name) return "Polymorph";
        if (desc) return "It changes a monster into another when you use it.";
        if (info) return dice_info_power(dice);
        if (value) return format("%d", 500);
        if (color) return format("%d", TERM_ORANGE);
        if (cast) return _device_bolt(GF_OLD_POLY, dice);
        break;
    case EFFECT_STARLIGHT:
        dice.dd = _extra(effect, 6 + effect->power / 10);
        dice.ds = 10;
        if (name) return "Starlight";
        if (desc) return "It fires a line of light directed randomly for multiple times when you use it.";
        if (info) return dice_info_dam_each(dice);
        if (value) return format("%d", 750);
        if (color) return format("%d", TERM_YELLOW);
        if (cast) {
            plr_star_light(_5d(3), GF_LIGHT_WEAK, dice);
            device_noticed = TRUE;
        }
        break;
    case EFFECT_NOTHING:
        if (name) return "Nothing";
        if (desc) return "It is your food.";
        if (value) return format("%d", 1);
        if (cast)
            msg_print("What a pity ... You are wasting your food!");
        break;

    /* Bad Effects */
    case EFFECT_AGGRAVATE:
        if (name) return "Aggravate Monsters";
        if (desc) return "It aggravates nearby monsters.";
        if (value) return format("%d", 100); /* This actually *can* be useful ... */
        if (color) return format("%d", TERM_L_DARK);
        if (cast)
        {
            aggravate_monsters(who_create_null());
            device_known = TRUE;
        }
        break;
    case EFFECT_HEAL_MONSTER:
        dice.dd = 10;
        dice.ds = 10;
        if (name) return "Heal Monster";
        if (desc) return "It heals a monster when you use it.";
        if (value) return format("%d", 5);
        if (cast) return _device_bolt(GF_OLD_HEAL, dice);
        break;
    case EFFECT_HASTE_MONSTER:
        dice.base = _extra(effect, effect->power);
        if (name) return "Haste Monster";
        if (desc) return "It hastes a monster when you use it.";
        if (value) return format("%d", 15);
        if (cast) return _device_bolt(GF_OLD_SPEED, dice);
        break;
    case EFFECT_HASTE_MONSTERS:
        if (name) return "Haste Monsters";
        if (desc) return "It hastes all monsters in sight when you use it.";
        if (cast) return _device_los(GF_OLD_SPEED, dice);
        break;
    case EFFECT_CLONE_MONSTER:
        if (name) return "Clone Monster";
        if (desc) return "It clones a non-unique monster when you use it.";
        if (value) return format("%d", 10);
        if (cast) return _device_bolt(GF_OLD_CLONE, dice);
        break;
    case EFFECT_DARKNESS:
        if (name) return "Darkness";
        if (desc) return "It darkens nearby area or current room and blinds you when you use it.";
        if (color) return format("%d", TERM_L_DARK);
        if (cast)
        {
            if (!res_save_default(GF_BLIND) && !res_save_default(GF_DARK))
            {
                if (plr_tim_add(T_BLIND, 3 + randint1(5)))
                    device_noticed = TRUE;
            }
            if (unlite_area(10, 3))
                device_noticed = TRUE;
        }
        break;
    case EFFECT_SUMMON_ANGRY_MONSTERS:
        if (name) return "Summoning";
        if (desc) return "It summons several monsters as enemies when you use it.";
        if (color) return format("%d", TERM_RED);
        if (cast)
        {
            int i;
            int num = randint1(4);
            for (i = 0; i < num; i++)
            {
                if (summon_specific(who_create_null(), plr->pos, cave->dun_lvl, 0, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_NO_PET)))
                    device_noticed = TRUE;
            }
        }
        break;
    case EFFECT_SLOWNESS:
        if (name) return "Slowness";
        if (desc) return "It slows you down temporarily when you use it.";
        if (color) return format("%d", TERM_UMBER);
        if (cast)
        {
            if (plr_tim_add(T_SLOW, randint1(30) + 15))
                device_noticed = TRUE;
        }
        break;

    /* Specific Artifacts ... Try to minimize! */
    case EFFECT_JEWEL:
        if (name) return "Clairvoyance";
        if (desc) return "It maps, lights permanently and detects all items on the entire level.";
        if (value) return format("%d", 10000);
        if (color) return format("%d", TERM_VIOLET);
        if (cast)
        {
            virtue_add(VIRTUE_KNOWLEDGE, 1);
            virtue_add(VIRTUE_ENLIGHTENMENT, 1);
            wiz_lite();
            msg_print("The Jewel drains your vitality...");
            take_hit(DAMAGE_LOSELIFE, damroll(3, 8), "the Jewel of Judgement");
            detect_traps(DETECT_RAD_DEFAULT, TRUE);
            detect_doors(DETECT_RAD_DEFAULT);
            detect_stairs(DETECT_RAD_DEFAULT);
            detect_recall(DETECT_RAD_DEFAULT);
            device_noticed = TRUE;
        }
        break;
    case EFFECT_HERMES:
        if (name) return "Haste and Dimension Door";
        if (desc) return "It hastes you and teleports you to a chosen nearby location.";
        if (value) return format("%d", 15000);
        if (cast)
        {
            if (plr_tim_add(T_FAST, _BOOST(randint1(75) + 75))) device_noticed = TRUE;
            if (dimension_door(_BOOST(plr->lev / 2 + 10))) device_noticed = TRUE;
        }
        break;
    case EFFECT_ARTEMIS:
        if (name) return "Create Arrows";
        if (desc) return "It magically creates arrows out of thin air!";
        if (value) return format("%d", 7500);
        if (cast)
        {
            object_type forge;
            char o_name[MAX_NLEN];

            object_prep(&forge, lookup_kind(TV_ARROW, m_bonus(1, plr->lev)+ 1));
            forge.number = (byte)rand_range(5, 10);
            apply_magic(&forge, plr->lev, AM_NO_FIXED_ART);
            obj_identify(&forge);

            forge.discount = 99;

            object_desc(o_name, &forge, 0);
            msg_format("It creates %s.", o_name);

            pack_carry(&forge);
            device_noticed = TRUE;
        }
        break;
    case EFFECT_DEMETER:
        if (name) return "Flame of Demeter";
        if (desc) return "It heals you and satiates your hunger.";
        if (info) return info_heal(0, 0, _BOOST(500));
        if (value) return format("%d", 10000);
        if (cast)
        {
            if (hp_player(_BOOST(500))) device_noticed = TRUE;
            if (set_food(PY_FOOD_MAX - 1)) device_noticed = TRUE;
        }
        break;
    case EFFECT_EYE_VECNA:
        if (name) return "Eye of Vecna";
        if (desc) return "It opens your eyes to visions of all that surrounds you.";
        if (value) return format("%d", 10000);
        if (cast)
        {
            take_hit(DAMAGE_LOSELIFE, damroll(8, 8), "the Eye of Vecna");
            wiz_map();
            device_noticed = TRUE;
        }
        break;
    case EFFECT_ONE_RING:
        if (name) return "Something Weird";
        if (desc) return "It does something completely unpredictable and probably rather bad.";
        if (value) return format("%d", 1000);
        if (cast) return _device_one_ring(dice);
        break;
    case EFFECT_BLADETURNER:
        dice.base = 300;
        if (name) return "Heroism, Resistance and Breathe Elements";
        if (desc) return "It grants temporary heroism, blessing and elemental resistance and also allows you to breathe the elements.";
        if (value) return format("%d", 10000);
        if (cast)
        {
            if (!_device_breath(4, GF_MISSILE, dice)) return NULL;
            plr_tim_add(T_HERO, _BOOST(_1d(50) + 50));
            plr_tim_add(T_BLESSED, _BOOST(_1d(50) + 50));
            plr_tim_add(T_RES_ACID, _BOOST(_1d(50) + 50));
            plr_tim_add(T_RES_ELEC, _BOOST(_1d(50) + 50));
            plr_tim_add(T_RES_FIRE, _BOOST(_1d(50) + 50));
            plr_tim_add(T_RES_COLD, _BOOST(_1d(50) + 50));
            plr_tim_add(T_RES_POIS, _BOOST(_1d(50) + 50));
        }
        break;
    case EFFECT_BLOODY_MOON:
        if (name) return "Change Zokusei";
        if (desc) return "It changes its slays and resistances.";
        if (value) return format("%d", 5000);
        if (cast)
        {
            /* TODO: Again, we need the underlying object ...
               For now, we safely assume the artifact is Bloody Moon. */
            int slot = equip_find_art("/.Bloody Moon");
            if (slot)
            {
                object_type *o_ptr = equip_obj(slot);
                get_bloody_moon_flags(o_ptr);
                obj_identify_fully(o_ptr);
                obj_display(o_ptr);
                if (plr->prace == RACE_ANDROID) android_calc_exp();
                plr->update |= (PU_BONUS | PU_HP);
                device_noticed = TRUE;
            }
        }
        break;
    case EFFECT_SACRED_KNIGHTS:
        if (name) return "Dispel Curse and Probing";
        if (desc) return "It removes all normal curses from your equipment and probes nearby monsters.";
        if (value) return format("%d", 5000);
        if (cast)
        {
            if (remove_all_curse())
            {
                msg_print("You feel as if someone is watching over you.");
                device_noticed = TRUE;
            }
            if (probing())
                device_noticed = TRUE;
        }
        break;
    case EFFECT_GONG:
        dice.base = _extra(effect, 3*effect->power);
        if (name) return "Bang a Gong";
        if (desc) return "It makes some very loud noise.";
        if (info) return dice_info_dam(dice);
        if (value) return format("%d", 50*dice_roll(dice));
        if (cast)
        {
            if (!res_save_default(GF_SOUND))
                gf_affect_p(who_create_unctrl_power(), GF_SOUND, dice_roll(dice), GF_AFFECT_SPELL);
            plr_burst(10, GF_SOUND, dice_roll(dice));
            device_noticed = TRUE;
        }
        break;
    case EFFECT_MURAMASA:
        if (name) return "Gain Strength";
        if (desc) return "It attempts to increase your strength, but is destroyed upon failure.";
        if (value) return format("%d", 5000);
        if (cast)
        {
            if (get_check("Are you sure?!"))
            {
                msg_print("");
                do_inc_stat(A_STR);
                if (one_in_(2))
                {
                    /* TODO: We need to know the exact equipment slot being used so
                        we can destroy it. For now, we assume the artifact is Muramasa.
                        Note: Effects might someday be triggered by spells, so passing
                        an object to this routine won't always make sense!
                     */
                    int slot = equip_find_art("|.Muramasa");
                    if (slot)
                    {
                        msg_print("The Muramasa is destroyed!");
                        curse_weapon(TRUE, slot);
                    }
                }
            }
        }
        break;
    default:
        if (name) return format("Invalid Effect: %d", effect->type);
    }
    return "";
}
#undef _BOOST

