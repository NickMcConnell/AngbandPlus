#include "angband.h"

#include <assert.h>

static bool ang_sort_comp_pet(vptr u, vptr v, int a, int b)
{
    u16b *who = (u16b*)(u);

    int w1 = who[a];
    int w2 = who[b];

    monster_type *m_ptr1 = &m_list[w1];
    monster_type *m_ptr2 = &m_list[w2];
    monster_race *r_ptr1 = &r_info[m_ptr1->r_idx];
    monster_race *r_ptr2 = &r_info[m_ptr2->r_idx];

    /* Unused */
    (void)v;

    if (m_ptr1->nickname && !m_ptr2->nickname) return TRUE;
    if (m_ptr2->nickname && !m_ptr1->nickname) return FALSE;

    if ((r_ptr1->flags1 & RF1_UNIQUE) && !(r_ptr2->flags1 & RF1_UNIQUE)) return TRUE;
    if ((r_ptr2->flags1 & RF1_UNIQUE) && !(r_ptr1->flags1 & RF1_UNIQUE)) return FALSE;

    if (r_ptr1->level > r_ptr2->level) return TRUE;
    if (r_ptr2->level > r_ptr1->level) return FALSE;

    if (m_ptr1->hp > m_ptr2->hp) return TRUE;
    if (m_ptr2->hp > m_ptr1->hp) return FALSE;
    
    return w1 <= w2;
}

/* Devices: We are following the do_spell() pattern which is quick and dirty,
   but not my preferred approach ... */

/* Fail Rates ... Scaled by 10 (95.2% returned as 952) */
static int _rod_calc_fail_rate(object_type *o_ptr)
{
    int lev, chance, fail;

    lev = k_info[o_ptr->k_idx].level;
    chance = p_ptr->skills.dev;
    if (p_ptr->confused) chance = chance / 2;
    if (p_ptr->stun) chance = chance * 2 / 3;

    fail = lev+5;
    if (chance > fail) fail -= (chance - fail)*2;
    else chance -= (fail - chance)*2;
    if (fail < USE_DEVICE) fail = USE_DEVICE;
    if (chance < USE_DEVICE) chance = USE_DEVICE;

    if (chance > fail)
        return fail*1000/(chance*2);

    return 1000 - chance*1000/(fail*2);
}

int device_calc_fail_rate(object_type *o_ptr)
{
    int lev, chance, fail;

    if (p_ptr->pclass == CLASS_BERSERKER) return 1000;
    if (o_ptr->tval == TV_ROD) return _rod_calc_fail_rate(o_ptr);

    lev = k_info[o_ptr->k_idx].level;
    if (lev > 50) lev = 50 + (lev - 50)/2;
    chance = p_ptr->skills.dev;
    if (p_ptr->confused) chance = chance / 2;
    if (p_ptr->stun) chance = chance * 2 / 3;
    chance = chance - lev;
    if (chance < USE_DEVICE) 
        fail = 1000 - 1000/(3 * (USE_DEVICE - chance + 1));
    else
        fail = (USE_DEVICE-1)*1000/chance;

    if (o_ptr->tval == TV_SCROLL && fail > 500) fail = 500;

    return fail;
}

/* Hack: When using an unkown rod we force the user to target. Also
   Trap Location should not spoil with the view_unsafe_grids option. */
bool device_known = FALSE;

/* Hack: When using an unkown device, was there an observable effect?
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

static bool _do_identify_hook(object_type *o_ptr)
{
    if (!object_is_known(o_ptr))
        return TRUE;
    return FALSE;
}

static void _do_identify_aux(int item)
{
    object_type    *o_ptr;
    char            o_name[MAX_NLEN];
    bool            old_known;

    if (item >= 0)
        o_ptr = &inventory[item];
    else
        o_ptr = &o_list[-item];

    old_known = identify_item(o_ptr);
    object_desc(o_name, o_ptr, 0);

    if (equip_is_valid_slot(item))
        msg_format("%^s: %s (%c).", equip_describe_slot(item), o_name, index_to_label(item));
    else if (item >= 0)
        msg_format("In your pack: %s (%c).", o_name, index_to_label(item));
    else
        msg_format("On the ground: %s.", o_name);

    autopick_alter_item(item, (bool)(destroy_identify && !old_known));
}

static bool _do_identify(void)
{
    int             item;
    cptr            q, s;
    int             options = USE_EQUIP | USE_INVEN | USE_FLOOR;

    assert(device_used_charges == 0);
    if (device_available_charges > 1)
        options |=  OPTION_ALL;

    item_tester_no_ryoute = TRUE;
    item_tester_hook = _do_identify_hook;

    if (can_get_item())
        q = "Identify which item? ";
    else
        q = "All items are identified. ";

    s = "You have nothing to identify.";
    if (!get_item(&item, q, s, options)) 
        return FALSE;

    if (item == INVEN_ALL)
    {
        int i;
        int this_o_idx, next_o_idx;

        /* Equipment and Pack */
        for (i = 0; i < INVEN_TOTAL && device_used_charges < device_available_charges; i++)
        {
            if (!inventory[i].k_idx) continue;
            if (object_is_known(&inventory[i])) continue;
            _do_identify_aux(i);
            device_used_charges++;
        }

        /* Floor */
        for (this_o_idx = cave[py][px].o_idx; 
                this_o_idx && device_used_charges < device_available_charges; 
                this_o_idx = next_o_idx)
        {
            object_type *o_ptr = &o_list[this_o_idx];

            next_o_idx = o_ptr->next_o_idx;
            if (object_is_known(o_ptr)) continue;
            _do_identify_aux(-this_o_idx);
            device_used_charges++;
        }
    }
    else
    {
        _do_identify_aux(item);
        device_used_charges++;
    }

    return TRUE;
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

bool device_use(object_type *o_ptr)
{
    device_known = object_is_aware(o_ptr);
    if (do_device(o_ptr->tval, o_ptr->sval, SPELL_CAST))
        return TRUE;
    return FALSE;
}

static int _device_power_hack(int val)
{
    if (magic_eater_hack) return spell_power(val);
    return device_power(val);
}

static int _rod_power(int val)
{
    val += val * device_extra_power / 100;
    if (devicemaster_is_(DEVICEMASTER_RODS))
        return device_power_aux(val, p_ptr->device_power + p_ptr->lev/5);
    return _device_power_hack(val);
}

static int _staff_power(int val)
{
    val += val * device_extra_power / 100;
    if (devicemaster_is_(DEVICEMASTER_STAVES))
        return device_power_aux(val, p_ptr->device_power + p_ptr->lev/10);
    return _device_power_hack(val);
}

static int _wand_power(int val)
{
    val += val * device_extra_power / 100;
    if (devicemaster_is_(DEVICEMASTER_WANDS))
        return device_power_aux(val, p_ptr->device_power + p_ptr->lev/10);
    return _device_power_hack(val);
}

static int _scroll_power(int val)
{
    if (devicemaster_is_(DEVICEMASTER_SCROLLS))
    {
        val += val * device_extra_power / 100;
        return device_power_aux(val, /*p_ptr->device_power + */p_ptr->lev/10);
    }
    return val;
}

static int _potion_power(int val)
{
    if (devicemaster_is_(DEVICEMASTER_POTIONS))
    {
        val += val * device_extra_power / 100;
        return device_power_aux(val, /*p_ptr->device_power + */p_ptr->lev/10);
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
            if (set_slow(randint1(25) + 15, FALSE)) 
                device_noticed = TRUE;
        }
        break;
    case SV_POTION_SALT_WATER:
        if (desc) return "It makes you nearly faint from hunger and paralyzes you, but it cures poison when you quaff it.";
        if (cast)
        {
            if ( !(get_race_t()->flags & RACE_IS_NONLIVING)
              && !prace_is_(RACE_MON_JELLY) )
            {
                msg_print("The potion makes you vomit!");
                set_food(PY_FOOD_STARVE - 1);
                set_poisoned(0, TRUE);
                set_paralyzed(randint1(4), FALSE);
                device_noticed = TRUE;
            }
        }
        break;
    case SV_POTION_POISON:
        if (desc) return "It poisons you when you quaff it.";
        if (cast)
        {
            if (!res_save_default(RES_POIS))
            {
                if (set_poisoned(p_ptr->poisoned + randint0(15) + 10, FALSE))
                    device_noticed = TRUE;
            }
        }
        break;
    case SV_POTION_BLINDNESS:
        if (desc) return "It blinds you when you quaff it.";
        if (cast)
        {
            if (!res_save_default(RES_BLIND))
            {
                if (set_blind(p_ptr->blind + randint0(100) + 100, FALSE))
                    device_noticed = TRUE;
            }
        }
        break;
    case SV_POTION_CONFUSION: /* Booze */
        if (desc) return "It confuses and hallucinates you when you quaff it. If you are a monk, you may be a drunken master.";
        if (cast)
        {
            if (p_ptr->pclass != CLASS_MONK) 
                virtue_add(VIRTUE_HARMONY, -1);
            if (!res_save_default(RES_CONF))
            {
                if (p_ptr->pclass == CLASS_MONK) 
                    p_ptr->special_attack |= ATTACK_SUIKEN;
                if (set_confused(randint0(20) + 15, FALSE))
                    device_noticed = TRUE;
            }

            if (!res_save_default(RES_CHAOS))
            {
                if (one_in_(2))
                {
                    if (set_image(p_ptr->image + randint0(25) + 25, FALSE))
                        device_noticed = TRUE;
                }
                if (one_in_(13) && (p_ptr->pclass != CLASS_MONK))
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
            if (!p_ptr->free_act)
            {
                msg_print("You fall asleep.");

                if (ironman_nightmare)
                {
                    msg_print("A horrible vision enters your mind.");
                    get_mon_num_prep(get_nightmare, NULL);
                    have_nightmare(get_mon_num(MAX_DEPTH));
                    get_mon_num_prep(NULL, NULL);
                }
                if (set_paralyzed(randint1(4), FALSE))
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
            if (!p_ptr->hold_life && (p_ptr->exp > 0))
            {
                msg_print("You feel your memories fade.");
                virtue_add(VIRTUE_KNOWLEDGE, -5);
                lose_exp(p_ptr->exp / 4);
                device_noticed = TRUE;
            }
        }
        break;
    case SV_POTION_RUINATION:
        if (desc) return "You take damage and it decreases all your stats permanently when you quaff it.";
        if (cast)
        {
            msg_print("Your nerves and muscles feel weak and lifeless!");
            take_hit(DAMAGE_LOSELIFE, damroll(10, 10), "a potion of Ruination", -1);

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
            take_hit(DAMAGE_NOESCAPE, damroll(50, 20), "a potion of Detonation", -1);

            set_stun(p_ptr->stun + 75, FALSE);
            set_cut(p_ptr->cut + 5000, FALSE);
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
            take_hit(DAMAGE_LOSELIFE, 5000, "a potion of Death", -1);
            device_noticed = TRUE;
        }
        break;
    case SV_POTION_INFRAVISION:
        if (desc) return "It gives temporary infravision when you quaff it.";
        if (info) return info_duration(_potion_power(100), _potion_power(100));
        if (cast)
        {
            int dur = _potion_power(100 + randint1(100));
            if (set_tim_infra(p_ptr->tim_infra + dur, FALSE))
            {
                device_noticed = TRUE;
            }
        }
        break;
    case SV_POTION_DETECT_INVIS:
        if (desc) return "It gives temporary see invisible when you quaff it.";
        if (info) return info_duration(_potion_power(12), _potion_power(12));
        if (cast)
        {
            int dur = _potion_power(12 + randint1(12));
            if (set_tim_invis(p_ptr->tim_invis + dur, FALSE))
            {
                device_noticed = TRUE;
            }
        }
        break;
    case SV_POTION_SLOW_POISON:
        if (desc) return "It reduces poison when you quaff it.";
        if (cast)
        {
            if (set_poisoned(p_ptr->poisoned / 2, TRUE)) device_noticed = TRUE;
        }
        break;
    case SV_POTION_CURE_POISON:
        if (desc) return "It cures poison when you quaff it.";
        if (cast)
        {
            if (set_poisoned(0, TRUE)) device_noticed = TRUE;
        }
        break;
    case SV_POTION_BOLDNESS:
        if (desc) return "It removes fear when you quaff it.";
        if (cast)
        {
            if (p_ptr->afraid)
            {
                fear_clear_p();
                device_noticed = TRUE;
            }
        }
        break;
    case SV_POTION_SPEED:
        if (desc) return "It hastes you temporarily when you quaff it.";
        if (info) return format("Dur d%d + %d", _potion_power(25), _potion_power(15));
        if (cast)
        {
            if (!p_ptr->fast)
            {
                int dur = _potion_power(randint1(25) + 15);
                if (set_fast(dur, FALSE)) device_noticed = TRUE;
            }
            else if (p_ptr->pclass == CLASS_MAULER)
                set_fast(p_ptr->fast + 10, FALSE);
            else
                set_fast(p_ptr->fast + 5, FALSE);
        }
        break;
    case SV_POTION_RESIST_HEAT:
        if (desc) return "You get temporary resistance to fire when you quaff it. This resistance is cumulative with equipment.";
        if (info) return format("Dur d%d + %d", _potion_power(10), _potion_power(10));
        if (cast)
        {
            int dur = _potion_power(10 + randint1(10));
            if (set_oppose_fire(p_ptr->oppose_fire + dur, FALSE))
            {
                device_noticed = TRUE;
            }
        }
        break;
    case SV_POTION_RESIST_COLD:
        if (desc) return "You get temporary resistance to cold when you quaff it. This resistance is cumulative with equipment.";
        if (info) return format("Dur d%d + %d", _potion_power(10), _potion_power(10));
        if (cast)
        {
            int dur = _potion_power(10 + randint1(10));
            if (set_oppose_cold(p_ptr->oppose_cold + dur, FALSE))
            {
                device_noticed = TRUE;
            }
        }
        break;
    case SV_POTION_HEROISM:
        if (desc) return "It removes fear and causes you temporary heroism when you quaff it.";
        if (info) return format("Dur d%d + %d", _potion_power(25), _potion_power(25));
        if (cast)
        {
            int dur = _potion_power(25 + randint1(25));
            if (set_hero(p_ptr->hero + dur, FALSE)) device_noticed = TRUE;
        }
        break;
    case SV_POTION_BERSERK_STRENGTH:
        if (desc) return "It removes fear and causes you to go berserk when you quaff it.";
        if (info) return format("Dur d%d + %d", _potion_power(25), _potion_power(25));
        if (cast)
        {
            int dur = _potion_power(25 + randint1(25));
            if (set_shero(p_ptr->shero + dur, FALSE)) device_noticed = TRUE;
            if (hp_player(30)) device_noticed = TRUE;
        }
        break;
    case SV_POTION_CURE_LIGHT:
        if (desc) return "It heals you trivially, cures blindness and berserk and reduces cuts when you quaff it.";
        if (info) return info_heal(2, _potion_power(8), 0);
        if (cast)
        {
            if (hp_player(_potion_power(damroll(2, 8)))) device_noticed = TRUE;
            if (set_blind(0, TRUE)) device_noticed = TRUE;
            if (set_cut(p_ptr->cut - 10, TRUE)) device_noticed = TRUE;
            if (set_shero(0,TRUE)) device_noticed = TRUE;
        }
        break;
    case SV_POTION_CURE_SERIOUS:
        if (desc) return "It heals you a bit, cures blindness, confusion and berserk and reduces cuts when you quaff it.";
        if (info) return info_heal(4, _potion_power(8), 0);
        if (cast)
        {
            if (hp_player(_potion_power(damroll(4, 8)))) device_noticed = TRUE;
            if (set_blind(0, TRUE)) device_noticed = TRUE;
            if (set_confused(0, TRUE)) device_noticed = TRUE;
            if (set_cut((p_ptr->cut / 2) - 50, TRUE)) device_noticed = TRUE;
            if (set_shero(0,TRUE)) device_noticed = TRUE;
        }
        break;
    case SV_POTION_CURE_CRITICAL:
        if (desc) return "It heals you a bit and cures blindness, confusion, poison, stunned, cuts and berserk when you quaff it.";
        if (info) return info_heal(6, _potion_power(8), 0);
        if (cast)
        {
            if (hp_player(_potion_power(damroll(6, 8)))) device_noticed = TRUE;
            if (set_blind(0, TRUE)) device_noticed = TRUE;
            if (set_confused(0, TRUE)) device_noticed = TRUE;
            if (set_poisoned(0, TRUE)) device_noticed = TRUE;
            if (set_stun(0, TRUE)) device_noticed = TRUE;
            if (set_cut(0, TRUE)) device_noticed = TRUE;
            if (set_shero(0,TRUE)) device_noticed = TRUE;
        }
        break;
    case SV_POTION_BLOOD:
        if (desc) return "A much needed infusion! It heals you a bit and cures blindness, confusion, poison, and stunned when you quaff it.";
        if (info) return info_heal(0, 0, _potion_power(200));
        if (cast)
        {
            if (hp_player(_potion_power(200))) device_noticed = TRUE;
            if (set_blind(0, TRUE)) device_noticed = TRUE;
            if (set_confused(0, TRUE)) device_noticed = TRUE;
            if (set_poisoned(0, TRUE)) device_noticed = TRUE;
            if (set_stun(0, TRUE)) device_noticed = TRUE;
        }
        break;
    case SV_POTION_HEALING:
        if (desc) return "It heals you and cures blindness, confusion, poison, stunned, cuts and berserk when you quaff it.";
        if (info) return info_heal(0, 0, _potion_power(300)); 
        if (cast)
        {
            if (hp_player(_potion_power(300))) device_noticed = TRUE;
            if (set_blind(0, TRUE)) device_noticed = TRUE;
            if (set_confused(0, TRUE)) device_noticed = TRUE;
            if (set_poisoned(0, TRUE)) device_noticed = TRUE;
            if (set_stun(0, TRUE)) device_noticed = TRUE;
            if (set_cut(0, TRUE)) device_noticed = TRUE;
            if (set_shero(0,TRUE)) device_noticed = TRUE;
        }
        break;
    case SV_POTION_STAR_HEALING:
        if (desc) return "It heals you and cures blindness, confusion, poison, stunned, cuts and berserk when you quaff it.";
        if (info) return info_heal(0, 0, _potion_power(1000)); 
        if (cast)
        {
            if (hp_player(_potion_power(1000))) device_noticed = TRUE;
            if (set_blind(0, TRUE)) device_noticed = TRUE;
            if (set_confused(0, TRUE)) device_noticed = TRUE;
            if (set_poisoned(0, TRUE)) device_noticed = TRUE;
            if (set_stun(0, TRUE)) device_noticed = TRUE;
            if (set_cut(0, TRUE)) device_noticed = TRUE;
            if (set_shero(0,TRUE)) device_noticed = TRUE;
        }
        break;
    case SV_POTION_LIFE:
        if (desc) return "It heals you completely, restores experience and all your stats and cures blindness, confusion, poison, hallucination, stunned, cuts and berserk when you quaff it.";
        if (info) return info_heal(0, 0, _potion_power(5000)); 
        if (cast)
        {
            virtue_add(VIRTUE_VITALITY, 1);
            virtue_add(VIRTUE_UNLIFE, -5);
            msg_print("You feel life flow through your body!");
            restore_level();
            set_poisoned(0, TRUE);
            set_blind(0, TRUE);
            set_confused(0, TRUE);
            set_image(0, TRUE);
            set_stun(0, TRUE);
            set_cut(0, TRUE);
            do_res_stat(A_STR);
            do_res_stat(A_CON);
            do_res_stat(A_DEX);
            do_res_stat(A_WIS);
            do_res_stat(A_INT);
            do_res_stat(A_CHR);
            set_shero(0,TRUE);
            update_stuff();
            hp_player(_potion_power(5000));
            device_noticed = TRUE;
        }
        break;
    case SV_POTION_CLARITY:
        if (desc) return "It clears your mind a bit when you quaff it.";
        if (info) return format("5d%d + %d", _potion_power(6), _potion_power(5));
        if (cast)
        {
            if (sp_player(_potion_power(damroll(5, 6) + 5)))
            {
                msg_print("You feel your mind clear.");
                device_noticed = TRUE;
            }
        }
        break;
    case SV_POTION_GREAT_CLARITY:
        if (desc) return "It greatly clears your mind when you quaff it.";
        if (info) return format("10d%d + %d", _potion_power(10), _potion_power(15));
        if (cast)
        {
            if (sp_player(_potion_power(damroll(10, 10) + 15)))
            {
                msg_print("You feel your mind clear.");
                device_noticed = TRUE;
            }
        }
        break;
    case SV_POTION_RESTORE_MANA:
        if (desc) return "It restores mana to full and cures berserk when you quaff it.";
        if (cast)
        {
            if (restore_mana()) device_noticed = TRUE;
            if (set_shero(0,TRUE)) device_noticed = TRUE;
        }
        break;
    case SV_POTION_RESTORE_EXP:
        if (desc) return "It restores your experience when you quaff it.";
        if (cast)
        {
            if (restore_level()) device_noticed = TRUE;
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
            wiz_lite(p_ptr->tim_superstealth > 0);
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
            wiz_lite(p_ptr->tim_superstealth > 0);
            do_inc_stat(A_INT);
            do_inc_stat(A_WIS);
            detect_traps(DETECT_RAD_DEFAULT, TRUE);
            detect_doors(DETECT_RAD_DEFAULT);
            detect_stairs(DETECT_RAD_DEFAULT);
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
            if (p_ptr->prace == RACE_ANDROID) break;
            virtue_add(VIRTUE_ENLIGHTENMENT, 1);
            if (p_ptr->exp < PY_MAX_EXP)
            {
                s32b ee = _potion_power((p_ptr->exp / 2) + 10);
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
        if (info) return format("Dur d%d + %d", _potion_power(20), _potion_power(20));
        if (cast)
        {
            int dur = _potion_power(20 + randint1(20));
            set_oppose_acid(dur, FALSE);
            set_oppose_elec(dur, FALSE);
            set_oppose_fire(dur, FALSE);
            set_oppose_cold(dur, FALSE);
            set_oppose_pois(dur, FALSE);
            device_noticed = TRUE;
        }
        break;
    case SV_POTION_CURING:
        if (desc) return "It heals you a bit and cures blindness, poison, confusion, stunning, cuts and hallucination when you quaff it.";
        if (info) return info_heal(0, 0, _potion_power(50));
        if (cast)
        {
            if (hp_player(_potion_power(50))) device_noticed = TRUE;
            if (set_blind(0, TRUE)) device_noticed = TRUE;
            if (set_poisoned(0, TRUE)) device_noticed = TRUE;
            if (set_confused(0, TRUE)) device_noticed = TRUE;
            if (set_stun(0, TRUE)) device_noticed = TRUE;
            if (set_cut(0, TRUE)) device_noticed = TRUE;
            if (set_image(0, TRUE)) device_noticed = TRUE;
            if (set_shero(0,TRUE)) device_noticed = TRUE;
        }
        break;
    case SV_POTION_INVULNERABILITY:
        if (desc) return "You become invulnerable temporarily when you quaff it.";
        if (info) return format("Dur d%d + %d", _potion_power(4), _potion_power(4));
        if (cast)
        {
            int dur = _potion_power(4 + randint1(4));
            set_invuln(p_ptr->invuln + dur, FALSE);
            device_noticed = TRUE;
        }
        break;
    case SV_POTION_NEW_LIFE:
        if (desc) return "It changes your life rating and max of all your stats and cures all mutations when you quaff it.";
        if (cast)
        {
            do_cmd_rerate(FALSE);
            get_max_stats();
            p_ptr->update |= PU_BONUS;
            mut_lose_all();
            device_noticed = TRUE;
            if (p_ptr->pclass == CLASS_WILD_TALENT)
                wild_talent_new_life();
            if (p_ptr->pclass == CLASS_PSION && get_check("Relearn Powers? "))
                psion_relearn_powers();
        }
        break;
    case SV_POTION_NEO_TSUYOSHI:
        if (desc) return "It cures hallucination and increases your strength and constitution temporarily when you quaff it but your strength and constitution decrease permanently than before when the effect expires.";
        if (cast)
        {
            set_image(0, TRUE);
            set_tsuyoshi(p_ptr->tsuyoshi + randint1(100) + 100, FALSE);
            device_noticed = TRUE;
        }
        break;
    case SV_POTION_TSUYOSHI:
        if (desc) return "It decreases your strength and constitution permanently and makes you hallucinate when you quaff it.";
        if (cast)
        {
            msg_print("Brother OKURE!");
            msg_print(NULL);
            p_ptr->tsuyoshi = 1;
            set_tsuyoshi(0, TRUE);
            if (!res_save_default(RES_CHAOS))
                set_image(50 + randint1(50), FALSE);
            device_noticed = TRUE;
        }
        break;
    case SV_POTION_GIANT_STRENGTH:
        if (desc) return "It greatly increases your stature temporarily when you quaff it.";
        if (info) return format("Dur d%d + %d", _potion_power(20), _potion_power(20));
        if (cast)
        {
            if (set_tim_building_up(_potion_power(20 + randint1(20)), FALSE)) device_noticed = TRUE;
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
                if (p_ptr->pclass == CLASS_WILD_TALENT)
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

                if (p_ptr->pclass == CLASS_WILD_TALENT && one_in_(2))
                    wild_talent_scramble();
            }
        }
        break;
    case SV_POTION_STONE_SKIN:
        if (desc) return "It temporarily turns your skin to stone, granting enhanced armor class, when you quaff it.";
        if (info) return format("Dur d%d + %d", _potion_power(20), _potion_power(20));
        if (cast)
        {
            if (set_shield(_potion_power(20 + randint1(20)), FALSE)) device_noticed = TRUE;
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
            if (!res_save_default(RES_BLIND) && !res_save_default(RES_DARK))
            {
                if (set_blind(p_ptr->blind + 3 + randint1(5), FALSE)) device_noticed = TRUE;
            }
            if (unlite_area(10, 3)) device_noticed = TRUE;
        }
        break;
    case SV_SCROLL_AGGRAVATE_MONSTER:
        if (desc) return "It aggravates monsters in your vicinity when you read it.";
        if (cast)
        {
            msg_print("There is a high pitched humming noise.");
            aggravate_monsters(0);
            device_noticed = TRUE;
        }
        break;
    case SV_SCROLL_CURSE_ARMOR:
        if (desc) return "It makes your current armour (Blasted) when you read it.";
        if (cast)
        {
            int slot = equip_random_slot(object_is_armour);
            if (slot && curse_armor(slot)) device_noticed = TRUE;
        }
        break;
    case SV_SCROLL_CURSE_WEAPON:
        if (desc) return "It makes your wielding weapon (Shattered) when you read it.";
        if (cast)
        {
            int slot = equip_random_slot(object_is_melee_weapon);
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
                if (summon_specific(0, py, px, dun_level, 0, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_NO_PET)))
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
                if (summon_specific(0, py, px, dun_level, SUMMON_UNDEAD, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_NO_PET)))
                    device_noticed = TRUE;
            }
        }
        break;
    case SV_SCROLL_SUMMON_PET:
        if (desc) 
        {
            if (p_ptr->prace == RACE_MON_RING)
                return "It summons a ring bearer as your pet when you read it.";
            else
                return "It summons a monster as your pet when you read it.";
        }
        if (cast)
        {
            int type = 0;
            if (p_ptr->prace == RACE_MON_RING)
                type = SUMMON_RING_BEARER;
            if (summon_specific(-1, py, px, _scroll_power(dun_level), type, (PM_ALLOW_GROUP | PM_FORCE_PET)))
                device_noticed = TRUE;
        }
        break;
    case SV_SCROLL_SUMMON_KIN:
        if (desc) return "It summons a monster corresponds to your race as your pet when you read it.";
        if (cast)
        {
            if (summon_kin_player(_scroll_power(p_ptr->lev), py, px, (PM_FORCE_PET | PM_ALLOW_GROUP)))
                device_noticed = TRUE;
        }
        break;
    case SV_SCROLL_TRAP_CREATION:
        if (desc) return "It creates traps on the squares adjacent to you when you read it.";
        if (cast)
        {
            if (trap_creation(py, px)) 
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
        if (desc) return "It teleports you a long distance when you read it.";
        if (cast)
        {
            teleport_player(100, 0L);
            energy_use = energy_use * 3 / 2;
            if (mut_present(MUT_ASTRAL_GUIDE))
                energy_use = energy_use / 3;
            device_noticed = TRUE;
        }
        break;
    case SV_SCROLL_TELEPORT_LEVEL:
        if (desc) return "It teleports you one dungeon level up or down immediately when you read it.";
        if (cast)
        {
            teleport_level(0);
            device_noticed = TRUE;
        }
        break;
    case SV_SCROLL_WORD_OF_RECALL:
        if (desc) return "It recalls you to the town, or back into the dungeon you have entered when you read it.";
        if (cast)
        {
            device_noticed = TRUE;
            if (!word_of_recall()) return NULL;
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
        if (desc) return "It recharges wands, staffs or rods when you read it.";
        if (cast)
        {
            if (!recharge(_scroll_power(75))) return NULL;
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
            if (set_blessed(p_ptr->blessed + _scroll_power(randint1(12) + 6), FALSE)) device_noticed = TRUE;
        }
        break;
    case SV_SCROLL_HOLY_CHANT:
        if (desc) return "It blesses you temporarily when you read it.";
        if (cast)
        {
            if (set_blessed(p_ptr->blessed + _scroll_power(randint1(24) + 12), FALSE)) device_noticed = TRUE;
        }
        break;
    case SV_SCROLL_HOLY_PRAYER:
        if (desc) return "It blesses you temporarily when you read it.";
        if (cast)
        {
            if (set_blessed(p_ptr->blessed + _scroll_power(randint1(48) + 24), FALSE)) device_noticed = TRUE;
        }
        break;
    case SV_SCROLL_MONSTER_CONFUSION:
        if (desc) return "You can confuse monster you hit just for once when you read it.";
        if (cast)
        {
            if (!(p_ptr->special_attack & ATTACK_CONFUSE))
            {
                msg_print("Your hands begin to glow.");
                p_ptr->special_attack |= ATTACK_CONFUSE;
                p_ptr->redraw |= (PR_STATUS);
                device_noticed = TRUE;
            }
        }
        break;
    case SV_SCROLL_PROTECTION_FROM_EVIL:
        if (desc) return "It gives temporary protection from lesser evil creatures when you read it.";
        if (cast)
        {
            if (set_protevil(p_ptr->protevil + _scroll_power(randint1(25) + 3 * p_ptr->lev), FALSE)) 
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
            if (destroy_area(py, px, 13 + randint0(5), _scroll_power(2000)))
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
            if (dispel_undead(_scroll_power(80))) device_noticed = TRUE;
        }
        break;
    case SV_SCROLL_SPELL:
        if (desc) return "It increases the number you can study spells when you read. If you are the class can't study or don't need to study, it has no effect.";
        if (cast)
        {
            if ((p_ptr->pclass == CLASS_WARRIOR) ||
                (p_ptr->pclass == CLASS_IMITATOR) || 
                (p_ptr->pclass == CLASS_MINDCRAFTER) || 
                (p_ptr->pclass == CLASS_PSION) || 
                (p_ptr->pclass == CLASS_SORCERER) || 
                (p_ptr->pclass == CLASS_ARCHER) || 
                (p_ptr->pclass == CLASS_MAGIC_EATER) || 
                p_ptr->pclass == CLASS_DEVICEMASTER || 
                (p_ptr->pclass == CLASS_RED_MAGE) || 
                (p_ptr->pclass == CLASS_SAMURAI) || 
                (p_ptr->pclass == CLASS_BLUE_MAGE) || 
                (p_ptr->pclass == CLASS_CAVALRY) || 
                (p_ptr->pclass == CLASS_BERSERKER) || 
                (p_ptr->pclass == CLASS_WEAPONSMITH) || 
                (p_ptr->pclass == CLASS_MIRROR_MASTER) || 
                (p_ptr->pclass == CLASS_TIME_LORD) || 
                (p_ptr->pclass == CLASS_BLOOD_KNIGHT) || 
                (p_ptr->pclass == CLASS_WARLOCK) || 
                (p_ptr->pclass == CLASS_ARCHAEOLOGIST) || 
                (p_ptr->pclass == CLASS_DUELIST) || 
                (p_ptr->pclass == CLASS_RUNE_KNIGHT) ||
                (p_ptr->pclass == CLASS_WILD_TALENT) ||
                (p_ptr->pclass == CLASS_NINJA) ||
                p_ptr->pclass == CLASS_SCOUT ||
                p_ptr->pclass == CLASS_MYSTIC ||
                p_ptr->pclass == CLASS_MAULER)
            {
                msg_print("There is no effect.");
            }
            else
            {
                p_ptr->add_spells++;
                p_ptr->update |= (PU_SPELLS);
            }
            device_noticed = TRUE;
        }
        break;
    case SV_SCROLL_GENOCIDE:
        if (desc) return "It eliminates an entire class of monster, exhausting you. Powerful or unique monsters may resist.";
        if (cast)
        {
            symbol_genocide(_scroll_power(300), TRUE);
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
            acquirement(py, px, 1, TRUE, FALSE);
            device_noticed = TRUE;
        }
        break;
    case SV_SCROLL_STAR_ACQUIREMENT:
        if (desc) return "It creates some great items when you read it.";
        if (cast)
        {
            acquirement(py, px, _scroll_power(randint1(2) + 1), TRUE, FALSE);
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
            set_tim_eyeeye(_scroll_power(randint1(25) + 25), FALSE);
            device_noticed = TRUE;
        }
        break;
    case SV_SCROLL_RUMOR:
        if (desc) return "A rumor is in it.";
        if (cast)
        {
            char Rumor[1024];
            errr err = 0;

            switch (randint1(20))
            {
            case 1:
                err = get_rnd_line("chainswd.txt", 0, Rumor);
                break;
            case 2:
                err = get_rnd_line("error.txt", 0, Rumor);
                break;
            case 3:
            case 4:
            case 5:
                err = get_rnd_line("death.txt", 0, Rumor);
                break;
            default:
                err = get_rnd_line("rumors.txt", 0, Rumor);
            }

            if (err) strcpy(Rumor, "Some rumors are wrong.");
            msg_print("There is message on the scroll. It says:");
            msg_print(NULL);
            msg_format("%s", Rumor);
            msg_print(NULL);
            msg_print("The scroll disappears in a puff of smoke!");
            device_noticed = TRUE;
        }
        break;
    case SV_SCROLL_ARTIFACT:
        if (desc) return "It creates an artifact from a nameless weapon or armour when you read it. Don't be greedy - you will get only one artifact.";
        if (cast)
        {
            device_noticed = TRUE;
            if (no_artifacts)
            {
                if (!brand_weapon(-1)) return NULL;
            }
            else
            {
                if (!artifact_scroll()) return NULL;
            }
        }
        break;
    case SV_SCROLL_MADNESS:
        if (desc) return "It seems to be the hurried scriblings of a mad wizard on the verge of some great arcane discovery.  You can't make heads or tails of it. Do you read it to see what happens?";
        if (cast)
        {
            int item;
            object_type *o_ptr;
            int n = randint0(_scroll_power(100));

            item_tester_hook = item_tester_hook_nameless_weapon_armour;
            if (!get_item(&item, "Use which item? ", "You have nothing to use.", (USE_EQUIP | USE_INVEN | USE_FLOOR))) return NULL;

            if (item >= 0)
                o_ptr = &inventory[item];
            else
                o_ptr = &o_list[0 - item];

            if (o_ptr->number > 1)
            {
                msg_print("Don't be greedy.  Just try it out on a single object at a time.");
                return NULL;
            }
            
            device_noticed = TRUE;

            /* TODO: Add more goodies ... */
            if (n < 10)
            {
                msg_print("Ooops!  That didn't work at all!");
                destroy_area(py, px, 13 + randint0(5), 300);
            }
            else if (n < 15)
            {
                msg_print("You faintly hear crazy laughter for a moment.");
                summon_cyber(-1, py, px);
            }
            else if (n < 25)
            {
                msg_print("The scroll explodes violently!");
                call_chaos(100);
            }
            else if (n < 65)
            {                
                curse_weapon(FALSE, item);    /* This curses armor too ... */
            }
            else if (n < 90)
            {
                if (object_is_melee_weapon(o_ptr))
                {
                    if (!brand_weapon_aux(item)) return NULL;
                }
                else
                    msg_print("Funny, nothing happened.");
            }
            else
            {
                if (no_artifacts)
                {
                    if (object_is_melee_weapon(o_ptr))
                    {
                        if (!brand_weapon_aux(item)) return NULL;
                    }
                }
                else
                    create_artifact(o_ptr, CREATE_ART_SCROLL | CREATE_ART_GOOD);
            }
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
            fire_ball(GF_FIRE, 0, _scroll_power(666), 4);
            if (!devicemaster_is_(DEVICEMASTER_SCROLLS) && !res_save_default(RES_FIRE))
            {
                int dam = res_calc_dam(RES_FIRE, 25 + randint1(25));
                take_hit(DAMAGE_NOESCAPE, dam, "a Scroll of Fire", -1);
            }
        }
        break;
    case SV_SCROLL_ICE:
        if (desc) return "It creates a huge ice ball centered on you.";
        if (info) return info_damage(0, 0, _scroll_power(400));
        if (cast)
        {
            device_noticed = TRUE;
            fire_ball(GF_ICE, 0, _scroll_power(800), 4);
            if (!devicemaster_is_(DEVICEMASTER_SCROLLS) && !res_save_default(RES_COLD))
            {
                int dam = res_calc_dam(RES_COLD, 30 + randint1(30));
                take_hit(DAMAGE_NOESCAPE, dam, "a Scroll of Ice", -1);
            }
        }
        break;
    case SV_SCROLL_CHAOS:
        if (desc) return "It creates a huge ball of logrus centered on you.";
        if (info) return info_damage(0, 0, _scroll_power(500));
        if (cast)
        {
            device_noticed = TRUE;
            fire_ball(GF_CHAOS, 0, _scroll_power(1000), 4);
            if (!devicemaster_is_(DEVICEMASTER_SCROLLS) && !res_save_default(RES_CHAOS))
            {
                int dam = res_calc_dam(RES_CHAOS, 50 + randint1(50));
                take_hit(DAMAGE_NOESCAPE, dam, "a Scroll of Logrus", -1);
            }
        }
        break;
    case SV_SCROLL_MANA:
        if (desc) return "It creates a huge ball of pure mana centered on you.";
        if (info) return info_damage(0, 0, _scroll_power(550));
        if (cast)
        {
            device_noticed = TRUE;
            fire_ball(GF_MANA, 0, _scroll_power(1100), 4);
            if (!devicemaster_is_(DEVICEMASTER_SCROLLS))
                take_hit(DAMAGE_NOESCAPE, 50 + randint1(50), "a Scroll of Mana", -1);
        }
        break;
    }
    return "";
}

static cptr _do_staff(int sval, int mode)
{
    bool desc = (mode == SPELL_DESC) ? TRUE : FALSE;
    bool info = (mode == SPELL_INFO) ? TRUE : FALSE;
    bool cast = (mode == SPELL_CAST) ? TRUE : FALSE;

    switch (sval)
    {
    case SV_STAFF_DARKNESS:
        if (desc) return "It darkens nearby area or current room and blinds you when you use it.";
        if (cast)
        {
            if (!res_save_default(RES_BLIND) && !res_save_default(RES_DARK))
            {
                if (set_blind(p_ptr->blind + 3 + randint1(5), FALSE)) device_noticed = TRUE;
            }
            if (unlite_area(10, 3)) device_noticed = TRUE;
        }
        break;
    case SV_STAFF_SLOWNESS:
        if (desc) return "It slows you down temporarily when you use it.";
        if (cast)
        {
            if (set_slow(p_ptr->slow + randint1(30) + 15, FALSE)) device_noticed = TRUE;
        }
        break;
    case SV_STAFF_HASTE_MONSTERS:
        if (desc) return "It hastes all monsters in sight when you use it.";
        if (cast)
        {
            if (speed_monsters()) device_noticed = TRUE;
        }
        break;
    case SV_STAFF_SUMMONING:
        if (desc) return "It summons several monsters as enemies when you use it.";
        if (cast)
        {
            int i;
            int num = randint1(4);
            for (i = 0; i < num; i++)
            {
                if (summon_specific(0, py, px, dun_level, 0, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_NO_PET)))
                    device_noticed = TRUE;
            }
        }
        break;
    case SV_STAFF_TELEPORTATION:
        if (desc) return "It teleports you a long distance when you use it.";
        if (cast)
        {
            if (mut_present(MUT_ASTRAL_GUIDE))
                energy_use = energy_use / 3;
            teleport_player(100, 0L);
            device_noticed = TRUE;
        }
        break;
    case SV_STAFF_IDENTIFY:
        if (desc) return "It identifies an item when you use it.";
        if (cast)
        {
            if (!_do_identify()) return NULL;
            device_noticed = TRUE;
        }
        break;
    case SV_STAFF_REMOVE_CURSE:
        if (desc) return "It removes normal curses from equipped items when you use it.";
        if (cast && remove_curse())
        {
            if (magic_eater_hack)
                msg_print("You feel as if someone is watching over you.");
            else if (!p_ptr->blind)
                msg_print("The staff glows blue for a moment.");
            device_noticed = TRUE;
        }
        break;
    case SV_STAFF_STARLITE:
        if (desc) return "It fires a line of light directed randomly for multiple times when you use it.";
        if (cast)
        {
            int num = damroll(5, 3);
            int y = 0, x = 0, k;
            int attempts;

            if (!p_ptr->blind && !magic_eater_hack)
                msg_print("The end of the staff glows brightly...");

            for (k = 0; k < num; k++)
            {
                attempts = 1000;
                while (attempts--)
                {
                    scatter(&y, &x, py, px, 4, 0);
                    if (!cave_have_flag_bold(y, x, FF_PROJECT)) continue;
                    if (!player_bold(y, x)) break;
                }
                project(0, 0, y, x, _staff_power(damroll(6 + p_ptr->lev / 8, 10)), GF_LITE_WEAK,
                          (PROJECT_BEAM | PROJECT_THRU | PROJECT_GRID | PROJECT_KILL), -1);
            }
            device_noticed = TRUE;
        }
        break;
    case SV_STAFF_LITE:
        if (desc) return "It lights up nearby area or the current room permanently when you use it.";
        if (cast)
        {
            if (lite_area(damroll(2, 8), 2)) device_noticed = TRUE;
        }
        break;
    case SV_STAFF_MAPPING:
        if (desc) return "It maps your vicinity when you use it.";
        if (cast)
        {
            map_area(_staff_power(DETECT_RAD_MAP));
            device_noticed = TRUE;
        }
        break;
    case SV_STAFF_DETECT_GOLD:
        if (desc) return "It detects all treasures in your vicinity when you use it.";
        if (cast)
        {
            if (detect_treasure(_staff_power(DETECT_RAD_DEFAULT))) device_noticed = TRUE;
            if (detect_objects_gold(_staff_power(DETECT_RAD_DEFAULT))) device_noticed = TRUE;
        }
        break;
    case SV_STAFF_DETECT_ITEM:
        if (desc) return "It detects all items in your vicinity when you use it.";
        if (cast)
        {
            if (detect_objects_normal(_staff_power(DETECT_RAD_DEFAULT))) device_noticed = TRUE;
        }
        break;
    case SV_STAFF_DETECT_TRAP:
        if (desc) return "It detects all traps in your vicinity when you use it.";
        if (cast)
        {
            if (detect_traps(_staff_power(DETECT_RAD_DEFAULT), device_known)) device_noticed = TRUE;
        }
        break;
    case SV_STAFF_DETECT_DOOR:
        if (desc) return "It detects all doors and stairs in your vicinity when you use it.";
        if (cast)
        {
            if (detect_doors(_staff_power(DETECT_RAD_DEFAULT))) device_noticed = TRUE;
            if (detect_stairs(_staff_power(DETECT_RAD_DEFAULT))) device_noticed = TRUE;
        }
        break;
    case SV_STAFF_DETECT_INVIS:
        if (desc) return "It detects all invisible monsters in your vicinity when you use it.";
        if (cast)
        {
            if (detect_monsters_invis(_staff_power(DETECT_RAD_DEFAULT))) device_noticed = TRUE;
        }
        break;
    case SV_STAFF_DETECT_EVIL:
        if (desc) return "It detects all evil monsters in your vicinity when you use it.";
        if (cast)
        {
            if (detect_monsters_evil(_staff_power(DETECT_RAD_DEFAULT))) device_noticed = TRUE;
        }
        break;
    case SV_STAFF_CURE_LIGHT:
        if (desc) return "It heals you a bit when you use it.";
        if (info) return info_heal(6, _staff_power(8), 0);
        if (cast)
        {
            if (hp_player(_staff_power(damroll(6, 8)))) device_noticed = TRUE;
            if (set_blind(0, TRUE)) device_noticed = TRUE;
            if (set_confused(0, TRUE)) device_noticed = TRUE;
            if (set_cut((p_ptr->cut / 2) - 50, TRUE)) device_noticed = TRUE;
            if (set_shero(0,TRUE)) device_noticed = TRUE;
        }
        break;
    case SV_STAFF_CURING:
        if (desc) return "It cures blindness, poison, confusion, stunned, cuts, hallucination and berserk when you use it.";
        if (cast)
        {
            if (set_blind(0, TRUE)) device_noticed = TRUE;
            if (set_poisoned(0, TRUE)) device_noticed = TRUE;
            if (set_confused(0, TRUE)) device_noticed = TRUE;
            if (set_stun(0, TRUE)) device_noticed = TRUE;
            if (set_cut(0, TRUE)) device_noticed = TRUE;
            if (set_image(0, TRUE)) device_noticed = TRUE;
            if (set_shero(0,TRUE)) device_noticed = TRUE;
        }
        break;
    case SV_STAFF_HEALING:
        if (desc) return "It heals you and cures stunned, cuts and berserk when you use it.";
        if (info) return info_heal(0, 0, _staff_power(300));
        if (cast)
        {
            if (hp_player(_staff_power(300))) device_noticed = TRUE;
            if (set_stun(0, TRUE)) device_noticed = TRUE;
            if (set_cut(0, TRUE)) device_noticed = TRUE;
            if (set_shero(0,TRUE)) device_noticed = TRUE;
        }
        break;
    case SV_STAFF_THE_MAGI:
        if (desc) return "It restores mana to full, restores your intelligence and cures berserk when you use it.";
        if (cast)
        {
            if (do_res_stat(A_INT)) device_noticed = TRUE;
            if (restore_mana()) device_noticed = TRUE;
            if (set_shero(0,TRUE)) device_noticed = TRUE;
        }
        break;
    case SV_STAFF_SLEEP_MONSTERS:
        if (desc) return "It puts all monsters in sight to sleep when you use it.";
        if (cast)
        {
            if (sleep_monsters(_staff_power(20 + p_ptr->lev))) device_noticed = TRUE;
        }
        break;
    case SV_STAFF_SLOW_MONSTERS:
        if (desc) return "It slows all monsters in sight down when you use it.";
        if (cast)
        {
            if (slow_monsters(_staff_power(p_ptr->lev*3))) device_noticed = TRUE;
        }
        break;
    case SV_STAFF_SPEED:
        if (desc) return "It hastes you temporarily when you use it.";
        if (info) return info_duration(_staff_power(15), _staff_power(30));
        if (cast)
        {
            if (set_fast(_staff_power(randint1(30) + 15), FALSE)) device_noticed = TRUE;
        }
        break;
    case SV_STAFF_PROBING:
        if (desc) return "It probes all monsters' alignment, HP, AC, speed, current experience and true character in sight when you use it.";
        if (cast)
        {
            probing();
            device_noticed = TRUE;
        }
        break;
    case SV_STAFF_DISPEL_EVIL:
        if (desc) return "It damages all evil monsters in sight when you use it.";
        if (info) return info_damage(0, 0, _staff_power(100));
        if (cast)
        {
            if (dispel_evil(_staff_power(100))) device_noticed = TRUE;
        }
        break;
    case SV_STAFF_POWER:
        if (desc) return "It does damage to all monsters in sight when you use it.";
        if (info) return info_damage(0, 0, _staff_power(150));
        if (cast)
        {
            if (dispel_monsters(_staff_power(150))) device_noticed = TRUE;
        }
        break;
    case SV_STAFF_HOLINESS:
        if (desc) return "It does damage to all evil monsters in sight, gives temporary protection from lesser evil creature, cures poison, stuuned, cuts, removes fear and heals you a bit when you use it.";
        if (info) return info_damage(0, 0, _staff_power(150));
        if (cast)
        {
            int k = 3 * p_ptr->lev;
            if (dispel_evil(_staff_power(150))) device_noticed = TRUE;
            if (set_protevil(p_ptr->protevil + randint1(25) + k, FALSE)) device_noticed = TRUE;
            if (set_poisoned(0, TRUE)) device_noticed = TRUE;
            if (hp_player(_staff_power(50))) device_noticed = TRUE;
            if (set_stun(0, TRUE)) device_noticed = TRUE;
            if (set_cut(0, TRUE)) device_noticed = TRUE;
        }
        break;
    case SV_STAFF_GENOCIDE:
        if (desc) return "It eliminates an entire class of monster, exhausting you. Powerful or unique monsters may resist.";
        if (cast)
        {
            symbol_genocide((magic_eater_hack ? p_ptr->lev + 50 : _staff_power(200)), TRUE);
            device_noticed = TRUE;
        }
        break;
    case SV_STAFF_EARTHQUAKES:
        if (desc) return "It causes a earthquake nearby you when you use it.";
        if (cast)
        {
            if (!earthquake(py, px, 10))
                msg_print("The dungeon trembles.");
            device_noticed = TRUE;
        }
        break;
    case SV_STAFF_DESTRUCTION:
        if (desc) return "It destroys everything nearby you when you use it.";
        if (info) return format("Power %d", _staff_power(4 * p_ptr->lev));
        if (cast)
        {
            if (destroy_area(py, px, 13 + randint0(5), _staff_power(4 * p_ptr->lev)))
                device_noticed = TRUE;
        }
        break;
    case SV_STAFF_ANIMATE_DEAD:
        if (desc) return "It raises corpses and skeletons nearby you from dead and makes them your pet when you use it.";
        if (cast)
        {
            if (animate_dead(0, py, px))
                device_noticed = TRUE;
        }
        break;
    case SV_STAFF_MSTORM:
        if (desc) return "It produces a huge mana ball centered on you when you use it. If you are not magically inclined, you take damage as well.";
        if (info) return info_damage(1, _staff_power(200), _staff_power(350));
        if (cast)
        {
            msg_print("Mighty magics rend your enemies!");
            project(0, 5, py, px,
                _staff_power((randint1(200) + 350) * 2), 
                GF_MANA, PROJECT_KILL | PROJECT_ITEM | PROJECT_GRID, -1);
            if ( p_ptr->pclass != CLASS_MAGE
              && p_ptr->pclass != CLASS_HIGH_MAGE 
              && p_ptr->pclass != CLASS_SORCERER 
              && p_ptr->pclass != CLASS_DEVICEMASTER
              && p_ptr->pclass != CLASS_MAGIC_EATER 
              && p_ptr->pclass != CLASS_BLUE_MAGE 
              && p_ptr->pclass != CLASS_BLOOD_MAGE )
            {
                take_hit(DAMAGE_NOESCAPE, 50, "unleashing magics too mighty to control", -1);
            }
            device_noticed = TRUE;
        }
        break;
    case SV_STAFF_NOTHING:
        if (desc) return "It does nothing when you use it.";
        if (cast)
        {
            msg_print("Nothing happens.");
            if ( prace_is_(RACE_SKELETON) 
              || prace_is_(RACE_GOLEM) 
              || prace_is_(RACE_ZOMBIE) 
              || prace_is_(RACE_SPECTRE) )
            {
                msg_print("What a waste.  It's your food!");
            }
        }
        break;
    }    
    return "";
}

static cptr _do_wand(int sval, int mode)
{
    bool desc = (mode == SPELL_DESC) ? TRUE : FALSE;
    bool info = (mode == SPELL_INFO) ? TRUE : FALSE;
    bool cast = (mode == SPELL_CAST) ? TRUE : FALSE;
    bool old_target_pet = target_pet;
    int  dir = 0;

    if (cast)
    {
        /* Aim */
        if (sval == SV_WAND_HEAL_MONSTER || sval == SV_WAND_HASTE_MONSTER)
            target_pet = TRUE;

        if (!get_aim_dir(&dir))
        {
            target_pet = old_target_pet;
            return NULL;
        }
        target_pet = old_target_pet;
        /* XXX Hack -- Wand of wonder can do anything before it */
        if (sval == SV_WAND_WONDER)
        {
            int vir = virtue_current(VIRTUE_CHANCE);
            sval = randint0(SV_WAND_WONDER);

            if (vir > 0)
            {
                while (randint1(300) < vir) sval++;
                if (sval > SV_WAND_COLD_BALL) sval = randint0(4) + SV_WAND_ACID_BALL;
            }
            else if (vir < 0)
            {
                while (randint1(300) < -vir) sval--;
                if (sval < SV_WAND_HEAL_MONSTER) sval = randint0(3) + SV_WAND_HEAL_MONSTER;
            }
            if (sval < SV_WAND_TELEPORT_AWAY)
                virtue_add(VIRTUE_CHANCE, 1);
        }
    }

    /* Fire! */
    switch (sval)
    {
    case SV_WAND_HEAL_MONSTER:
        if (desc) return "It heals a monster when you use it.";
        if (cast)
        {
            if (heal_monster(dir, _wand_power(damroll(10, 10)))) device_noticed = TRUE;
        }
        break;
    case SV_WAND_HASTE_MONSTER:
        if (desc) return "It hastes a monster when you use it.";
        if (cast)
        {
            if (speed_monster(dir)) device_noticed = TRUE;
        }
        break;
    case SV_WAND_CLONE_MONSTER:
        if (desc) return "It clones a monster when you use it. Unique monsters are not cloned.";
        if (cast)
        {
            if (clone_monster(dir)) device_noticed = TRUE;
        }
        break;
    case SV_WAND_TELEPORT_AWAY:
        if (desc) return "It fires a beam teleports all monsters on the line when you use it.";
        if (cast)
        {
            if (teleport_monster(dir)) device_noticed = TRUE;
        }
        break;
    case SV_WAND_DISARMING:
        if (desc) return "It fires a beam destroys all traps on the line when you use it.";
        if (cast)
        {
            if (disarm_trap(dir)) device_noticed = TRUE;
        }
        break;
    case SV_WAND_TRAP_DOOR_DEST:
        if (desc) return "It fires a beam destroys all traps and doors on the line when you use it.";
        if (cast)
        {
            if (destroy_door(dir)) device_noticed = TRUE;
        }
        break;
    case SV_WAND_STONE_TO_MUD:
        if (desc) return "It turns a door, rock, wall square to mud when you use it.";
        if (cast)
        {
            if (wall_to_mud(dir)) device_noticed = TRUE;
        }
        break;
    case SV_WAND_LITE:
        if (desc) return "It fires a line of light when you use it.";
        if (cast)
        {
            msg_print("A line of blue shimmering light appears.");
            lite_line(dir);
            device_noticed = TRUE;
        }
        break;
    case SV_WAND_SLEEP_MONSTER:
        if (desc) return "It puts a monster to sleep when you use it.";
        if (info) return format("Power %d", _wand_power(10 + p_ptr->lev));
        if (cast)
        {
            if (sleep_monster(dir, _wand_power(3*p_ptr->lev))) device_noticed = TRUE;
        }
        break;
    case SV_WAND_SLOW_MONSTER:
        if (desc) return "It slows a monster down when you use it.";
        if (cast)
        {
            if (slow_monster(dir)) device_noticed = TRUE;
        }
        break;
    case SV_WAND_CONFUSE_MONSTER:
        if (desc) return "It confuses a monster when you use it.";
        if (cast)
        {
            if (confuse_monster(dir, _wand_power(10 + p_ptr->lev))) device_noticed = TRUE;
        }
        break;
    case SV_WAND_FEAR_MONSTER:
        if (desc) return "It scares a monster when you use it.";
        if (cast)
        {
            if (fear_monster(dir, _wand_power(p_ptr->lev))) device_noticed = TRUE;
        }
        break;
    case SV_WAND_DRAIN_LIFE:
        if (desc) return "It fires a bolt that steals life from a foe when you use it.";
        if (info) return info_damage(0, 0, _wand_power(50 + p_ptr->lev/2));
        if (cast)
        {
            int dam = _wand_power(50 + p_ptr->lev/2);
            if (drain_life(dir, dam)) 
            {
                if (p_ptr->pclass != CLASS_BLOOD_MAGE)
                    hp_player(dam);
                device_noticed = TRUE;
            }
        }
        break;
    case SV_WAND_POLYMORPH:
        if (desc) return "It changes a monster into another when you use it.";
        if (cast)
        {
            if (poly_monster(dir)) device_noticed = TRUE;
        }
        break;
    case SV_WAND_STINKING_CLOUD:
        if (desc) return "It fires a ball of poison when you use it.";
        if (info) return info_damage(0, 0, _wand_power(12 + p_ptr->lev/4));
        if (cast)
        {
            fire_ball(GF_POIS, dir, _wand_power(12 + p_ptr->lev/4), 2);
            device_noticed = TRUE;
        }
        break;
    case SV_WAND_MAGIC_MISSILE:
        if (desc) return "It fires a bolt or beam of magic when you use it.";
        if (info) return info_damage(_wand_power(2 + p_ptr->lev/10), 6, 0);
        if (cast)
        {
            fire_bolt_or_beam(20, GF_MISSILE, dir, _wand_power(damroll(2 + p_ptr->lev/10, 6)));
            device_noticed = TRUE;
        }
        break;
    case SV_WAND_ACID_BOLT:
        if (desc) return "It fires a bolt or beam of acid when you use it.";
        if (info) return info_damage(_wand_power(6 + p_ptr->lev/7), 8, 0);
        if (cast)
        {
            fire_bolt_or_beam(20, GF_ACID, dir, _wand_power(damroll(6 + p_ptr->lev/7, 8)));
            device_noticed = TRUE;
        }
        break;
    case SV_WAND_CHARM_MONSTER:
        if (desc) return "It charms a monster into your pet when you use it.";
        if (cast)
        {
            if (charm_monster(dir, MAX(20, _wand_power(p_ptr->lev))))
            device_noticed = TRUE;
        }
        break;
    case SV_WAND_FIRE_BOLT:
        if (desc) return "It fires a bolt or beam of fire when you use it.";
        if (info) return info_damage(_wand_power(7 + p_ptr->lev/6), 8, 0);
        if (cast)
        {
            fire_bolt_or_beam(20, GF_FIRE, dir, _wand_power(damroll(7 + p_ptr->lev/6, 8)));
            device_noticed = TRUE;
        }
        break;
    case SV_WAND_COLD_BOLT:
        if (desc) return "It fires a bolt or beam of cold when you use it.";
        if (info) return info_damage(_wand_power(5 + p_ptr->lev/8), 8, 0);
        if (cast)
        {
            fire_bolt_or_beam(20, GF_COLD, dir, _wand_power(damroll(5 + p_ptr->lev/8, 8)));
            device_noticed = TRUE;
        }
        break;
    case SV_WAND_ACID_BALL:
        if (desc) return "It fires a ball of acid when you use it.";
        if (info) return info_damage(0, 0, _wand_power(60 + 3*p_ptr->lev/4));
        if (cast)
        {
            fire_ball(GF_ACID, dir, _wand_power(60 + 3*p_ptr->lev/4), 2);
            device_noticed = TRUE;
        }
        break;
    case SV_WAND_ELEC_BALL:
        if (desc) return "It fires a ball of lightning when you use it.";
        if (info) return info_damage(0, 0, _wand_power(40 + 3*p_ptr->lev/4));
        if (cast)
        {
            fire_ball(GF_ELEC, dir, _wand_power(40 + 3*p_ptr->lev/4), 2);
            device_noticed = TRUE;
        }
        break;
    case SV_WAND_FIRE_BALL:
        if (desc) return "It fires a ball of fire when you use it.";
        if (info) return info_damage(0, 0, _wand_power(70 + 3*p_ptr->lev/4));
        if (cast)
        {
            fire_ball(GF_FIRE, dir, _wand_power(70 + 3*p_ptr->lev/4), 2);
            device_noticed = TRUE;
        }
        break;
    case SV_WAND_COLD_BALL:
        if (desc) return "It fires a ball of cold when you use it.";
        if (info) return info_damage(0, 0, _wand_power(50 + 3*p_ptr->lev/4));
        if (cast)
        {
            fire_ball(GF_COLD, dir, _wand_power(50 + 3*p_ptr->lev/4), 2);
            device_noticed = TRUE;
        }
        break;
    case SV_WAND_WONDER:
        if (desc) return "It has a random effect when you use it.";
        if (cast) msg_print("Oops.  Wand of wonder activated.");
        break;
    case SV_WAND_DRAGON_FIRE:
        if (desc) return "It breathes fire when you use it.";
        if (info) return info_damage(0, 0, _wand_power(200));
        if (cast)
        {
            fire_ball(GF_FIRE, dir, _wand_power(200), -3);
            device_noticed = TRUE;
        }
        break;
    case SV_WAND_DRAGON_COLD:
        if (desc) return "It breathes cold when you use it.";
        if (info) return info_damage(0, 0, _wand_power(180));
        if (cast)
        {
            fire_ball(GF_COLD, dir, _wand_power(180), -3);
            device_noticed = TRUE;
        }
        break;
    case SV_WAND_DRAGON_BREATH:
        if (desc) return "It breathes acid, lightning, fire, cold or poison when you use it.";
        if (info) return format("dam %d-%d", _wand_power(180), _wand_power(240));
        if (cast)
        {
            switch (randint1(5))
            {
            case 1: fire_ball(GF_ACID, dir, _wand_power(240), -3); break;
            case 2: fire_ball(GF_ELEC, dir, _wand_power(210), -3); break;
            case 3: fire_ball(GF_FIRE, dir, _wand_power(240), -3); break;
            case 4: fire_ball(GF_COLD, dir, _wand_power(210), -3); break;
            case 5: fire_ball(GF_POIS, dir, _wand_power(180), -3); break;
            }
            device_noticed = TRUE;
        }
        break;
    case SV_WAND_DISINTEGRATE:
        if (desc) return "It fires a ball of disintegration when you use it.";
        if (info) return info_damage(0, 0, _wand_power(200 + p_ptr->lev*2));
        if (cast)
        {
            fire_ball(GF_DISINTEGRATE, dir, _wand_power(200 + randint1(p_ptr->lev * 2)), 2);
            device_noticed = TRUE;
        }
        break;
    case SV_WAND_ROCKETS:
        if (desc) return "It fires a rocket when you use it.";
        if (info) return info_damage(0, 0, _wand_power(250 + p_ptr->lev*3));
        if (cast)
        {
            msg_print("You launch a rocket!");
            fire_rocket(GF_ROCKET, dir, _wand_power(250 + p_ptr->lev*3), 2);
            device_noticed = TRUE;
        }
        break;
    case SV_WAND_STRIKING:
        if (desc) return "It fires a bolt of meteor when you use it.";
        if (info) return info_damage(_wand_power(15 + p_ptr->lev/3), 13, 0);
        if (cast)
        {
            fire_bolt(GF_METEOR, dir, _wand_power(damroll(15 + p_ptr->lev/3, 13)));
            device_noticed = TRUE;
        }
        break;
    case SV_WAND_GENOCIDE:
        if (desc) return "It removes a monster from current dungeon level unless resisted when you use it.";
        if (cast)
        {
            fire_ball_hide(GF_GENOCIDE, dir, magic_eater_hack ? p_ptr->lev + 50 : _wand_power(250), 0);
            device_noticed = TRUE;
        }
        break;
    }
    return "";
}

static cptr _do_rod(int sval, int mode)
{
    bool desc = (mode == SPELL_DESC) ? TRUE : FALSE;
    bool info = (mode == SPELL_INFO) ? TRUE : FALSE;
    bool cast = (mode == SPELL_CAST) ? TRUE : FALSE;
    int dir = 0;

    if (cast)
    {
        if (!device_known && !get_aim_dir(&dir)) return NULL;
    }

    switch (sval)
    {
    case SV_ROD_ESCAPING:
        if (desc) return "It teleports you when you zap it.";
        if (cast)
        {
            teleport_player(25 + p_ptr->lev / 2, 0);
            if (mut_present(MUT_ASTRAL_GUIDE))
                energy_use = energy_use / 3;
            device_noticed = TRUE;
        }
        break;
    case SV_ROD_DETECT_MONSTERS:
        if (desc) return "It detects all monsters in your vicinity when you zap it.";
        if (cast)
        {
            if (detect_monsters_normal(_rod_power(DETECT_RAD_DEFAULT))) device_noticed = TRUE;
        }
        break;
    case SV_ROD_DETECT_TRAP:
        if (desc) return "It detects all traps in your vicinity when you zap it.";
        if (cast)
        {
            if (detect_traps(_rod_power(DETECT_RAD_DEFAULT), device_known)) device_noticed = TRUE;
        }
        break;
    case SV_ROD_DETECT_DOOR:
        if (desc) return "It detects all doors and stairs in your vicinity when you zap it.";
        if (cast)
        {
            if (detect_doors(_rod_power(DETECT_RAD_DEFAULT))) device_noticed = TRUE;
            if (detect_stairs(_rod_power(DETECT_RAD_DEFAULT))) device_noticed = TRUE;
        }
        break;
    case SV_ROD_IDENTIFY:
        if (desc) return "It identifies an item when you zap it.";
        if (cast)
        {
            if (!ident_spell(NULL)) return NULL;
            device_noticed = TRUE;
        }
        break;
    case SV_ROD_RECALL:
        if (desc) return "It recalls you to the town, or back into the dungeon you have entered when you zap it.";
        if (cast)
        {
            device_noticed = TRUE;
            if (!word_of_recall()) return NULL;
        }
        break;
    case SV_ROD_ILLUMINATION:
        if (desc) return "It lights up nearby area or current room permanently when you zap it.";
        if (cast)
        {
            if (lite_area(_rod_power(damroll(2, 8)), 2)) device_noticed = TRUE;
        }
        break;
    case SV_ROD_MAPPING:
        if (desc) return "It maps your vicinity when you zap it.";
        if (cast)
        {
            map_area(_rod_power(DETECT_RAD_MAP));
            device_noticed = TRUE;
        }
        break;
    case SV_ROD_DETECTION:
        if (desc) return "It detects all traps, doors, stairs, treasures, items and monsters in the neighborhood when you zap it.";
        if (cast)
        {
            detect_all(_rod_power(DETECT_RAD_DEFAULT));
            device_noticed = TRUE;
        }
        break;
    case SV_ROD_PROBING:
        if (desc) return "It probes all monsters' alignment, HP, AC, speed, current experience and true character in sight when you zap it.";
        if (cast)
        {
            probing();
            device_noticed = TRUE;
        }
        break;
    case SV_ROD_CURING:
        if (desc) return "It cures blindness, poison, confusion, stunned, cuts, hallucination and berserk when you zap it.";
        if (info) return info_heal(0, 0, _rod_power(50));
        if (cast)
        {
            if (hp_player(_rod_power(50))) device_noticed = TRUE;
            if (set_blind(0, TRUE)) device_noticed = TRUE;
            if (set_poisoned(0, TRUE)) device_noticed = TRUE;
            if (set_confused(0, TRUE)) device_noticed = TRUE;
            if (set_stun(0, TRUE)) device_noticed = TRUE;
            if (set_cut(0, TRUE)) device_noticed = TRUE;
            if (set_image(0, TRUE)) device_noticed = TRUE;
            if (set_shero(0,TRUE)) device_noticed = TRUE;
        }
        break;
    case SV_ROD_HEALING:
        if (desc) return "It heals you and cures stunned, cuts and berserk when you zap it.";
        if (info) return info_heal(0, 0, _rod_power(500));
        if (cast)
        {
            if (hp_player(_rod_power(500))) device_noticed = TRUE;
            if (set_stun(0, TRUE)) device_noticed = TRUE;
            if (set_cut(0, TRUE)) device_noticed = TRUE;
            if (set_shero(0,TRUE)) device_noticed = TRUE;
        }
        break;
    case SV_ROD_RESTORATION:
        if (desc) return "It restores experience and all your stats when you zap it.";
        if (cast)
        {
            if (restore_level()) device_noticed = TRUE;
            if (do_res_stat(A_STR)) device_noticed = TRUE;
            if (do_res_stat(A_INT)) device_noticed = TRUE;
            if (do_res_stat(A_WIS)) device_noticed = TRUE;
            if (do_res_stat(A_DEX)) device_noticed = TRUE;
            if (do_res_stat(A_CON)) device_noticed = TRUE;
            if (do_res_stat(A_CHR)) device_noticed = TRUE;
        }
        break;
    case SV_ROD_SPEED:
        if (desc) return "It hastes you temporarily when you zap it.";
        if (info) return info_duration(_rod_power(15), _rod_power(30));
        if (cast)
        {
            if (set_fast(_rod_power(randint1(30) + 15), FALSE)) device_noticed = TRUE;
        }
        break;
    case SV_ROD_PESTICIDE:
        if (desc) return "It does slight damage to all monsters in sight when you zap it.";
        if (info) return info_damage(0, 0, _rod_power(4));
        if (cast)
        {
            if (dispel_monsters(_rod_power(4))) device_noticed = TRUE;
        }
        break;
    case SV_ROD_TELEPORT_AWAY:
        if (desc) return "It fires a beam that teleports all monsters when you zap it.";
        if (cast)
        {
            if (device_known && !get_aim_dir(&dir)) return NULL;
            if (teleport_monster(dir)) device_noticed = TRUE;
        }
        break;
    case SV_ROD_DISARMING:
        if (desc) return "It fires a beam that destroys all traps on the line when you zap it.";
        if (cast)
        {
            if (device_known && !get_aim_dir(&dir)) return NULL;
            if (disarm_trap(dir)) device_noticed = TRUE;
        }
        break;
    case SV_ROD_LITE:
        if (desc) return "It fires a line of light when you zap it.";
        if (cast)
        {
            if (device_known && !get_aim_dir(&dir)) return NULL;
            msg_print("A line of blue shimmering light appears.");
            lite_line(dir);
            device_noticed = TRUE;
        }
        break;
    case SV_ROD_SLEEP_MONSTER:
        if (desc) return "It puts a monster to sleep when you zap it.";
        if (info) return format("Power %d", _rod_power(30 + p_ptr->lev));
        if (cast)
        {
            if (device_known && !get_aim_dir(&dir)) return NULL;
            if (sleep_monster(dir, _rod_power(30 + p_ptr->lev))) device_noticed = TRUE;
        }
        break;
    case SV_ROD_SLOW_MONSTER:
        if (desc) return "It slows a monster down when you zap it.";
        if (cast)
        {
            if (device_known && !get_aim_dir(&dir)) return NULL;
            if (slow_monster(dir)) device_noticed = TRUE;
        }
        break;
    case SV_ROD_DRAIN_LIFE:
        if (desc) return "It fires a bolt that steals life from a foe when you zap it.";
        if (info) return info_damage(0, 0, _rod_power(60 + p_ptr->lev/2));
        if (cast)
        {
            int dam = _rod_power(60 + p_ptr->lev/2);
            if (device_known && !get_aim_dir(&dir)) return NULL;
            if (drain_life(dir, dam)) 
            {
                if (p_ptr->pclass != CLASS_BLOOD_MAGE)
                    hp_player(dam);
                device_noticed = TRUE;
            }
        }
        break;
    case SV_ROD_POLYMORPH:
        if (desc) return "It changes a monster into another when you zap it.";
        if (cast)
        {
            if (device_known && !get_aim_dir(&dir)) return NULL;
            if (poly_monster(dir)) device_noticed = TRUE;
        }
        break;
    case SV_ROD_ACID_BOLT:
        if (desc) return "It fires a bolt or beam of acid when you zap it.";
        if (info) return info_damage(_rod_power(6 + p_ptr->lev/7), 8, 0);
        if (cast)
        {
            if (device_known && !get_aim_dir(&dir)) return NULL;
            fire_bolt_or_beam(10, GF_ACID, dir, _rod_power(damroll(6 + p_ptr->lev/7, 8)));
            device_noticed = TRUE;
        }
        break;
    case SV_ROD_ELEC_BOLT:
        if (desc) return "It fires a bolt or beam of lightning when you zap it.";
        if (info) return info_damage(_rod_power(4 + p_ptr->lev/9), 8, 0);
        if (cast)
        {
            if (device_known && !get_aim_dir(&dir)) return NULL;
            fire_bolt_or_beam(10, GF_ELEC, dir, _rod_power(damroll(4 + p_ptr->lev/9, 8)));
            device_noticed = TRUE;
        }
        break;
    case SV_ROD_FIRE_BOLT:
        if (desc) return "It fires a bolt or beam of fire when you zap it.";
        if (info) return info_damage(_rod_power(7 + p_ptr->lev/6), 8, 0);
        if (cast)
        {
            if (device_known && !get_aim_dir(&dir)) return NULL;
            fire_bolt_or_beam(10, GF_FIRE, dir, _rod_power(damroll(7 + p_ptr->lev/6, 8)));
            device_noticed = TRUE;
        }
        break;
    case SV_ROD_COLD_BOLT:
        if (desc) return "It fires a bolt or beam of cold when you zap it.";
        if (info) return info_damage(_rod_power(5 + p_ptr->lev/8), 8, 0);
        if (cast)
        {
            if (device_known && !get_aim_dir(&dir)) return NULL;
            fire_bolt_or_beam(10, GF_COLD, dir, _rod_power(damroll(5 + p_ptr->lev/8, 8)));
            device_noticed = TRUE;
        }
        break;
    case SV_ROD_ACID_BALL:
        if (desc) return "It fires a ball of acid when you zap it.";
        if (info) return info_damage(0, 0, _rod_power(60 + p_ptr->lev));
        if (cast)
        {
            if (device_known && !get_aim_dir(&dir)) return NULL;
            fire_ball(GF_ACID, dir, _rod_power(60 + p_ptr->lev), 2);
            device_noticed = TRUE;
        }
        break;
    case SV_ROD_ELEC_BALL:
        if (desc) return "It fires a ball of lightning when you zap it.";
        if (info) return info_damage(0, 0, _rod_power(40 + p_ptr->lev));
        if (cast)
        {
            if (device_known && !get_aim_dir(&dir)) return NULL;
            fire_ball(GF_ELEC, dir, _rod_power(40 + p_ptr->lev), 2);
            device_noticed = TRUE;
        }
        break;
    case SV_ROD_FIRE_BALL:
        if (desc) return "It fires a ball of fire when you zap it.";
        if (info) return info_damage(0, 0, _rod_power(70 + p_ptr->lev));
        if (cast)
        {
            if (device_known && !get_aim_dir(&dir)) return NULL;
            fire_ball(GF_FIRE, dir, _rod_power(70 + p_ptr->lev), 2);
            device_noticed = TRUE;
        }
        break;
    case SV_ROD_COLD_BALL:
        if (desc) return "It fires a ball of cold when you zap it.";
        if (info) return info_damage(0, 0, _rod_power(50 + p_ptr->lev));
        if (cast)
        {
            if (device_known && !get_aim_dir(&dir)) return NULL;
            fire_ball(GF_COLD, dir, _rod_power(50 + p_ptr->lev), 2);
            device_noticed = TRUE;
        }
        break;
    case SV_ROD_MANA_BOLT:
        if (desc) return "It fires a bolt of mana when you zap it.";
        if (info) return info_damage(0, 0, _rod_power(100 + 2*p_ptr->lev));
        if (cast)
        {
            if (device_known && !get_aim_dir(&dir)) return NULL;
            fire_bolt(GF_MANA, dir, _rod_power(100 + 2*p_ptr->lev));
            device_noticed = TRUE;
        }
        break;
    case SV_ROD_MANA_BALL:
        if (desc) return "It fires a ball of mana when you zap it.";
        if (info) return info_damage(0, 0, _rod_power(200 + 2*p_ptr->lev));
        if (cast)
        {
            if (device_known && !get_aim_dir(&dir)) return NULL;
            fire_ball(GF_MANA, dir, _rod_power(200 + 2*p_ptr->lev), 2);
            device_noticed = TRUE;
        }
        break;
    case SV_ROD_HAVOC:
        if (desc) return "It is capable of firing almost anything, at random.";
        if (cast)
        {
            call_chaos(_rod_power(200));
            device_noticed = TRUE;
        }
        break;
    case SV_ROD_STONE_TO_MUD:
        if (desc) return "It turns a door, rock and wall square to mud when you zap it.";
        if (cast)
        {
            if (device_known && !get_aim_dir(&dir)) return NULL;
            if (wall_to_mud(dir)) device_noticed = TRUE;
        }
        break;
    case SV_ROD_AGGRAVATE:
        if (desc) return "It aggravates monsters in your vicinity when you zap it.";
        if (cast)
        {
            aggravate_monsters(0);
            device_noticed = TRUE;
        }
        break;
    }
    return "";
}

cptr do_device(int tval, int sval, int mode)
{
    cptr result = NULL;

    device_noticed = FALSE;
    device_used_charges = 0;
    switch (tval)
    {
    case TV_STAFF: result = _do_staff(sval, mode); break;
    case TV_WAND: result = _do_wand(sval, mode); break;
    case TV_ROD: result = _do_rod(sval, mode); break;
    case TV_SCROLL: result = _do_scroll(sval, mode); break;
    case TV_POTION: result = _do_potion(sval, mode); break;
    }
    device_known = FALSE;
    device_extra_power = 0;
    device_available_charges = 0;
    return result;
}

/* Effects: We are following the do_spell() pattern which is quick and dirty,
   but not my preferred approach ... Also, we could conceivably merge all
   devices into effects, handling rods, staves, wands, potions, scrolls and
   activations uniformly.  For the moment, effects are *just* activations, 
   and I should mention that each type of effect has its own little quirky
   fail rate calculation ... sigh. */
effect_t obj_get_effect(object_type *o_ptr)
{
    if (o_ptr->activation.type)
        return o_ptr->activation;
    if (o_ptr->name1 && a_info[o_ptr->name1].activation.type)
        return a_info[o_ptr->name1].activation;
    if (o_ptr->name2 && e_info[o_ptr->name2].activation.type)
        return e_info[o_ptr->name2].activation;
    return k_info[o_ptr->k_idx].activation;
}

cptr obj_get_effect_msg(object_type *o_ptr)
{
    u32b offset;
    
    if (o_ptr->activation.type)
        return 0;

    if (o_ptr->name1 && a_info[o_ptr->name1].activation.type)
    {
        offset = a_info[o_ptr->name1].activation_msg;
        if (offset)
            return a_text + offset;
        else
            return 0;
    }
    if (o_ptr->name2 && e_info[o_ptr->name2].activation.type)
    {
        return 0;
    }

    offset = k_info[o_ptr->k_idx].activation_msg;
    if (offset)
        return k_text + offset;

    return 0;
}

bool obj_has_effect(object_type *o_ptr)
{
    effect_t e = obj_get_effect(o_ptr);
    if (e.type)
        return TRUE;
    return FALSE;
}

/* Scaled by 10 so that, for example, 83.4% failure returns 834 */
int  effect_calc_fail_rate(effect_t *effect)
{
    int chance, fail;

    if (p_ptr->pclass == CLASS_BERSERKER) return 1000;

    chance = p_ptr->skills.dev;
    if (p_ptr->confused) chance = chance / 2;
    if (p_ptr->stun) chance = chance * 2 / 3;

    fail = effect->level + 5;
    if (chance > fail) fail -= (chance - fail)*2;
    else chance -= (fail - chance)*2;
    if (fail < USE_DEVICE) fail = USE_DEVICE;
    if (chance < USE_DEVICE) chance = USE_DEVICE;

    if (chance > fail)
        return fail * 1000 / (chance*2);
    else
        return 1000 - chance * 1000 / (fail*2);
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
    device_known = TRUE;
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

typedef struct 
{
    cptr text;
    int  type;
    int  level;
    int  timeout;
    int  rarity;
    int  bias;
} _effect_info_t;

/*  Allocation Table for Random Artifact Activations
    This also assists parsing a_info.txt, k_info.txt and e_info.txt.
    Order is irrelevant. Use Rarity 0 to exclude allocations.
*/
static _effect_info_t _effect_info[] = 
{
    /* Detection:                                   Lv    T   R  Bias */
    {"LITE_AREA",       EFFECT_LITE_AREA,            1,  10,  1, BIAS_MAGE},
    {"LITE_MAP_AREA",   EFFECT_LITE_MAP_AREA,       20,  50,  3, 0},
    {"ENLIGHTENMENT",   EFFECT_ENLIGHTENMENT,       20,  50,  2, BIAS_PRIESTLY | BIAS_ARCHER},
    {"CLAIRVOYANCE",    EFFECT_CLAIRVOYANCE,        35, 100,  8, BIAS_MAGE},

    {"DETECT_TRAPS",    EFFECT_DETECT_TRAPS,         3,  10,  1, BIAS_ROGUE},
    {"DETECT_MONSTERS", EFFECT_DETECT_MONSTERS,      5,  20,  1, BIAS_MAGE | BIAS_ARCHER},
    {"DETECT_OBJECTS",  EFFECT_DETECT_OBJECTS,       8,  25,  1, BIAS_ROGUE},
    {"DETECT_ALL",      EFFECT_DETECT_ALL,          25,  50,  3, BIAS_ROGUE},

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

    {"RECHARGING",      EFFECT_RECHARGING,          35, 500,  3, BIAS_MAGE | BIAS_DEMON},
    {"ENCHANTMENT",     EFFECT_ENCHANTMENT,         30, 900, 16, 0},
    {"IDENTIFY",        EFFECT_IDENTIFY,            15,  50,  1, BIAS_ROGUE | BIAS_MAGE},
    {"IDENTIFY_FULL",   EFFECT_IDENTIFY_FULL,       50, 200,  3, BIAS_ROGUE | BIAS_MAGE},
    {"PROBING",         EFFECT_PROBING,             30,  50,  1, BIAS_MAGE},
    {"RUNE_EXPLOSIVE",  EFFECT_RUNE_EXPLOSIVE,      30, 100,  2, BIAS_MAGE},
    {"RUNE_PROTECTION", EFFECT_RUNE_PROTECTION,     70, 500,  4, BIAS_PRIESTLY},

    {"SATISFY_HUNGER",  EFFECT_SATISFY_HUNGER,       5, 100,  1, BIAS_RANGER},
    {"DESTROY_TRAP",    EFFECT_DESTROY_TRAP,        20,  50,  1, 0},
    {"DESTROY_TRAPS",   EFFECT_DESTROY_TRAPS,       25,  50,  1, BIAS_ROGUE},
    {"WHIRLWIND_ATTACK",EFFECT_WHIRLWIND_ATTACK,    50, 500,  4, BIAS_WARRIOR},
    {"LIST_UNIQUES",    EFFECT_LIST_UNIQUES,        80, 250,  8, 0},
    {"LIST_ARTIFACTS",  EFFECT_LIST_ARTIFACTS,      80, 250,  8, 0},
    {"BANISH_EVIL",     EFFECT_BANISH_EVIL,         50, 100,  4, BIAS_PRIESTLY | BIAS_LAW},
    {"BANISH_ALL",      EFFECT_BANISH_ALL,          50, 100,  8, BIAS_MAGE},
    {"TELEKINESIS",     EFFECT_TELEKINESIS,         25, 100,  2, BIAS_MAGE},
    {"ALCHEMY",         EFFECT_ALCHEMY,             70, 500,  4, BIAS_MAGE},
    {"SELF_KNOWLEDGE",  EFFECT_SELF_KNOWLEDGE,      70, 500,  3, BIAS_MAGE},

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
    {"SPEED_ESSENTIA",  EFFECT_SPEED_ESSENTIA,      90, 999,  0, 0},
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

    {"RETURN_PETS",     EFFECT_RETURN_PETS,         10,   0,  0, 0},
    {"CAPTURE_PET",     EFFECT_CAPTURE_PET,         20,   0,  0, 0},

    /* Healing and Recovery:                        Lv    T   R  Bias */
    {"RESTORE_STATS",   EFFECT_RESTORE_STATS,       50, 600,  2, BIAS_PRIESTLY},
    {"RESTORE_EXP",     EFFECT_RESTORE_EXP,         40, 500,  1, BIAS_PRIESTLY},
    {"RESTORING",       EFFECT_RESTORING,           70, 800,  3, BIAS_PRIESTLY},
    {"HEAL",            EFFECT_HEAL,                40, 500,  1, BIAS_PRIESTLY},
    {"CURING",          EFFECT_CURING,              45, 200,  1, BIAS_PRIESTLY},
    {"HEAL_CURING",     EFFECT_HEAL_CURING,         60, 900,  4, BIAS_PRIESTLY},
    {"HEAL_CURING_HERO",EFFECT_HEAL_CURING_HERO,    70, 900,  4, BIAS_PRIESTLY},
    {"RESTORE_MANA",    EFFECT_RESTORE_MANA,        80, 900,  8, BIAS_MAGE},
    {"CURE_POIS",       EFFECT_CURE_POIS,           10,  50,  1, BIAS_POIS | BIAS_PRIESTLY | BIAS_RANGER},
    {"CURE_FEAR",       EFFECT_CURE_FEAR,           25, 100,  1, BIAS_PRIESTLY},
    {"CURE_FEAR_POIS",  EFFECT_CURE_FEAR_POIS,      30, 100,  1, BIAS_PRIESTLY},
    {"REMOVE_CURSE",    EFFECT_REMOVE_CURSE,        30, 200,  1, BIAS_PRIESTLY},
    {"REMOVE_ALL_CURSE",EFFECT_REMOVE_ALL_CURSE,    70, 500,  4, BIAS_PRIESTLY},
    {"CLARITY",         EFFECT_CLARITY,             20, 100, 12, BIAS_PRIESTLY | BIAS_MAGE},

    /* Offense: Bolts                               Lv    T   R  Bias */
    {"BOLT_MISSILE",    EFFECT_BOLT_MISSILE,         1,  10,  1, BIAS_MAGE | BIAS_ARCHER},
    {"BOLT_ACID",       EFFECT_BOLT_ACID,           15,  20,  1, BIAS_ACID},
    {"BOLT_ELEC",       EFFECT_BOLT_ELEC,           15,  20,  1, BIAS_ELEC},
    {"BOLT_FIRE",       EFFECT_BOLT_FIRE,           15,  20,  1, BIAS_FIRE | BIAS_DEMON},
    {"BOLT_COLD",       EFFECT_BOLT_COLD,           15,  20,  1, BIAS_COLD},
    {"BOLT_POIS",       EFFECT_BOLT_POIS,           10,  10,  1, BIAS_POIS},
    {"BOLT_LITE",       EFFECT_BOLT_LITE,           20,  25,  1, 0},
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

    /* Offense: Beams                               Lv    T   R  Bias */
    {"BEAM_LITE_WEAK",  EFFECT_BEAM_LITE_WEAK,      10,  20,  1, 0},
    {"BEAM_LITE",       EFFECT_BEAM_LITE,           40, 100,  2, 0},

    /* Offense: Balls                               Lv    T   R  Bias */
    {"BALL_ACID",       EFFECT_BALL_ACID,           25,  50,  1, BIAS_ACID},
    {"BALL_ELEC",       EFFECT_BALL_ELEC,           25,  50,  1, BIAS_ELEC},
    {"BALL_FIRE",       EFFECT_BALL_FIRE,           25,  50,  1, BIAS_FIRE | BIAS_DEMON},
    {"BALL_COLD",       EFFECT_BALL_COLD,           25,  50,  1, BIAS_COLD},
    {"BALL_POIS",       EFFECT_BALL_POIS,           10,   5,  1, BIAS_POIS},
    {"BALL_LITE",       EFFECT_BALL_LITE,           65, 100,  2, 0},
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

    /* Offense: Breaths                             Lv    T   R  Bias */
    {"BREATHE_ACID",    EFFECT_BREATHE_ACID,        40, 100,  2, BIAS_ACID},
    {"BREATHE_ELEC",    EFFECT_BREATHE_ELEC,        40, 100,  2, BIAS_ELEC},
    {"BREATHE_FIRE",    EFFECT_BREATHE_FIRE,        40, 100,  2, BIAS_FIRE | BIAS_DEMON},
    {"BREATHE_COLD",    EFFECT_BREATHE_COLD,        40, 100,  2, BIAS_COLD},
    {"BREATHE_POIS",    EFFECT_BREATHE_POIS,        40, 100,  2, BIAS_POIS},
    {"BREATHE_LITE",    EFFECT_BREATHE_LITE,        50, 125,  3, 0},
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
    {"CONFUSING_LITE",  EFFECT_CONFUSING_LITE,      60, 100,  6, BIAS_CHAOS},
    {"ARROW",           EFFECT_ARROW,               30, 100,  2, BIAS_RANGER | BIAS_ARCHER},
    {"WRATH_OF_GOD",    EFFECT_WRATH_OF_GOD,        80, 250, 32, BIAS_LAW},

    /* Misc                                         Lv    T   R  Bias */
    {"POLY_SELF",       EFFECT_POLY_SELF,           20, 500,  1, BIAS_CHAOS},
    {"ANIMATE_DEAD",    EFFECT_ANIMATE_DEAD,        25, 100,  1, BIAS_NECROMANTIC},
    {"SCARE_MONSTERS",  EFFECT_SCARE_MONSTERS,      20, 100,  1, BIAS_NECROMANTIC},
    {"SLEEP_MONSTERS",  EFFECT_SLEEP_MONSTERS,      25, 100,  1, BIAS_ROGUE},
    {"SLOW_MONSTERS",   EFFECT_SLOW_MONSTERS,       25, 100,  1, BIAS_RANGER},
    {"STASIS_MONSTERS", EFFECT_STASIS_MONSTERS,     50, 250,  8, BIAS_MAGE},
    {"CONFUSE_MONSTERS",EFFECT_CONFUSE_MONSTERS,    25, 100,  1, BIAS_CHAOS},
    {"FISHING",         EFFECT_FISHING,             10,   0,  0, 0},
    {"AGGRAVATE",       EFFECT_AGGRAVATE,           10, 100,  1, BIAS_DEMON},
    {"PIERCING_SHOT",   EFFECT_PIERCING_SHOT,       30, 100,  0, BIAS_ARCHER},
    {"CHARGE",          EFFECT_CHARGE,              15, 100,  0, 0},

    /* Specific Artifacts                           Lv    T   R  Bias */
    {"JEWEL",           EFFECT_JEWEL,                0,   0,  0, 0},
    {"HERMES",          EFFECT_HERMES,               0,   0,  0, 0},
    {"ARTEMIS",         EFFECT_ARTEMIS,              0,   0,  0, 0},
    {"DEMETER",         EFFECT_DEMETER,              0,   0,  0, 0},
    {"EYE_VECNA",       EFFECT_EYE_VECNA,            0,   0,  0, 0},
    {"ONE_RING",        EFFECT_ONE_RING,             0,   0,  0, 0},
    {"BLADETURNER",     EFFECT_BLADETURNER,          0,   0,  0, 0},
    {"MITO_KOUMON",     EFFECT_MITO_KOUMON,          0,   0,  0, 0},
    {"BLOODY_MOON",     EFFECT_BLOODY_MOON,          0,   0,  0, 0},
    {"SACRED_KNIGHTS",  EFFECT_SACRED_KNIGHTS,       0,   0,  0, 0},
    {"GONG",            EFFECT_GONG,                 0,   0,  0, 0},
    {"MURAMASA",        EFFECT_MURAMASA,             0,   0,  0, 0},

    { 0, 0, 0, 0, 0, 0 }
};

errr effect_parse(char *line, effect_t *effect) /* LITE_AREA:<Lvl>:<Timeout>:<Extra> */
{
    char *tokens[5];
    int   num = tokenize(line, 5, tokens, 0);
    int   i;
    
    if (num < 1) return PARSE_ERROR_TOO_FEW_ARGUMENTS;

    WIPE(effect, effect_t);

    switch (num)
    {
    case 4: effect->extra = atoi(tokens[3]);
    case 3: effect->timeout = atoi(tokens[2]);
    case 2: effect->level = atoi(tokens[1]);
    case 1:
        for (i = 0; ; i++)
        {
            if (!_effect_info[i].text) break;
            if (streq(tokens[0], _effect_info[i].text)) 
            {
                effect->type = _effect_info[i].type;
                break;
            }
        }
    }
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
    
/*  if (one_in_(3)) bias = 0; */

    for (i = 0; ; i++)
    {
        if (!_effect_info[i].type) break;
        if (bias && !(_effect_info[i].bias & bias)) continue;
        if (!_effect_info[i].rarity) continue;

        tot += MAX(255 / _effect_info[i].rarity, 1);
    }

    if (!tot) return -1;
    n = randint1(tot);

    for (i = 0; ; i++)
    {
        if (!_effect_info[i].type) break;
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
        o_ptr->activation.level = _effect_info[index].level;
        o_ptr->activation.timeout = _effect_info[index].timeout;
        o_ptr->activation.extra = 0;
        o_ptr->timeout = 0;
    }
}

bool effect_add_random_p(object_type *o_ptr, effect_p p)
{
    int i = _choose_random_p(p);
    if (i >= 0)
    {
        _add_index(o_ptr, i);
        return TRUE;
    }
    return FALSE;
}

bool effect_add_random(object_type *o_ptr, int bias)
{
    int i = _choose_random(bias);
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

#define _BOOST(n) (_boost((n), boost))
cptr do_effect(effect_t *effect, int mode, int boost)
{
    bool name = (mode == SPELL_NAME) ? TRUE : FALSE;
    bool desc = (mode == SPELL_DESC) ? TRUE : FALSE;
    bool info = (mode == SPELL_INFO) ? TRUE : FALSE;
    bool cast = (mode == SPELL_CAST) ? TRUE : FALSE;
    bool value = (mode == SPELL_VALUE) ? TRUE : FALSE;
    int  dir = 0;

    switch (effect->type)
    {
    /* Detection */
    case EFFECT_LITE_AREA:
        if (name) return "Illumination";
        if (desc) return "It lights up nearby area or current room permanently.";
        if (info) return info_damage(2 + effect->level/20, _BOOST(15), 0);
        if (value) return format("%d", 100);
        if (cast)
        {
            if (lite_area(_BOOST(damroll(2 + effect->level/20, 15)), 3))
                device_noticed = TRUE;
        }
        break;
    case EFFECT_LITE_MAP_AREA:
        if (name) return "Magic Mapping and Illumination";
        if (desc) return "It maps your vicinity and lights up your current room.";
        if (info) return info_damage(2 + effect->level/20, _BOOST(15), 0);
        if (value) return format("%d", 5000);
        if (cast)
        {
            map_area(DETECT_RAD_MAP);
            lite_area(_BOOST(damroll(2 + effect->level/20, 15)), 3);
            device_noticed = TRUE;
        }
        break;
    case EFFECT_ENLIGHTENMENT:
        if (name) return "Enlightenment";
        if (desc) return "It maps your vicinity.";
        if (value) return format("%d", 4500);
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
        if (cast)
        {
            virtue_add(VIRTUE_KNOWLEDGE, 1);
            virtue_add(VIRTUE_ENLIGHTENMENT, 1);
            wiz_lite(p_ptr->tim_superstealth > 0);
            detect_traps(DETECT_RAD_DEFAULT, TRUE);
            detect_doors(DETECT_RAD_DEFAULT);
            detect_stairs(DETECT_RAD_DEFAULT);
            device_noticed = TRUE;
        }
        break;
    case EFFECT_DETECT_TRAPS:
        if (name) return "Detect Traps";
        if (desc) return "It detects all traps in your vicinity.";
        if (value) return format("%d", 100);
        if (cast)
        {
            if (detect_traps(DETECT_RAD_DEFAULT, device_known))
                device_noticed = TRUE;
        }
        break;
    case EFFECT_DETECT_MONSTERS:
        if (name) return "Detect Monsters";
        if (desc) return "It detects all visible monsters in your vicinity.";
        if (value) return format("%d", 1000);
        if (cast)
        {
            if (detect_monsters_normal(DETECT_RAD_DEFAULT))
                device_noticed = TRUE;
        }
        break;
    case EFFECT_DETECT_OBJECTS:
        if (name) return "Detect Objects";
        if (desc) return "It detects all objects in your vicinity.";
        if (value) return format("%d", 250);
        if (cast)
        {
            if (detect_objects_normal(DETECT_RAD_DEFAULT))
                device_noticed = TRUE;
        }
        break;
    case EFFECT_DETECT_ALL:
        if (name) return "Detection";
        if (desc) return "It detects all traps, doors, stairs, treasures, items and monsters in your vicinity.";
        if (value) return format("%d", 5000);
        if (cast)
        {
            detect_all(DETECT_RAD_DEFAULT);
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
        if (cast)
        {
            if (!get_aim_dir(&dir)) return NULL;
            if (teleport_monster(dir)) device_noticed = TRUE;
        }
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
        if (info) return info_range(_BOOST(effect->level / 2 + 10));
        if (value) return format("%d", 10000);
        if (cast)
        {
            if (dimension_door(_BOOST(effect->level / 2 + 10)))
                device_noticed = TRUE;
        }
        break;
    case EFFECT_ESCAPE:
        if (name) return "Getaway";
        if (desc) return "It provides a random means of escape.";
        if (value) return format("%d", 1500);
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
                stair_creation(FALSE);
                break;
            default:
                if (get_check("Teleport Level? "))
                    teleport_level(0);
            }
            device_noticed = TRUE;
        }
        break;
    case EFFECT_RECALL:
        if (name) return "Word of Recall";
        if (desc) return "It recalls you to the surface, or back into a dungeon you have entered.";
        if (value) return format("%d", 1000);
        if (cast)
        {
            device_noticed = TRUE;
            if (!word_of_recall()) return NULL;
        }
        break;

    case EFFECT_STONE_TO_MUD:
        if (name) return "Stone to Mud";
        if (desc) return "It turns a door, rock, or wall to mud.";
        if (value) return format("%d", 1000);
        if (cast)
        {
            if (!get_aim_dir(&dir)) return NULL;
            if (wall_to_mud(dir)) device_noticed = TRUE;
        }
        break;
    case EFFECT_EARTHQUAKE:
        if (name) return "Earthquake";
        if (desc) return "It causes a massive earthquake nearby.";
        if (value) return format("%d", 1000);
        if (cast)
        {
            if (!earthquake(py, px, _extra(effect, 10)))
                msg_print("The dungeon trembles.");
            device_noticed = TRUE;
        }
        break;
    case EFFECT_DESTRUCTION:
    {
        int power = _extra(effect, 200);
        if (name) return "Destruction";
        if (desc) return "It destroys everything nearby.";
        if (info) return format("Power %d", _BOOST(power));
        if (value) return format("%d", power*20);
        if (cast)
        {
            if (destroy_area(py, px, 13 + randint0(5), _BOOST(power)))
                device_noticed = TRUE;
            else
                msg_print("The dungeon trembles...");
        }
        break;
    }
    case EFFECT_GENOCIDE:
    {
        int power = _extra(effect, 300);
        if (name) return "Genocide";
        if (desc) return "It eliminates an entire class of monster, exhausting you. Powerful or unique monsters may resist.";
        if (info) return format("Power %d", _BOOST(power));
        if (value) return format("%d", power*50);
        if (cast)
        {
            symbol_genocide(_BOOST(power), TRUE);
            device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_MASS_GENOCIDE:
    {
        int power = _extra(effect, 300);
        if (name) return "Mass Genocide";
        if (desc) return "It eliminates all nearby monsters, exhausting you. Powerful or unique monsters may be able to resist.";
        if (info) return format("Power %d", _BOOST(power));
        if (value) return format("%d", power*60);
        if (cast)
        {
            if (mass_genocide(_BOOST(power), TRUE))
                device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_RECHARGING:
    {
        int power = _extra(effect, 75);
        if (name) return "Recharging";
        if (desc) return "It attempts to recharge a magical device, but may destroy the device on failure.";
        if (info) return format("Power %d", _BOOST(power));
        if (value) return format("%d", power*30);
        if (cast)
        {
            device_noticed = TRUE;
            if (!recharge(_BOOST(power))) return NULL;
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
            if (!ident_spell(NULL)) return NULL;
        }
        break;
    case EFFECT_IDENTIFY_FULL:
        if (name) return "*Identify*";
        if (desc) return "It reveals all information about an item.";
        if (value) return format("%d", 1500);
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
        if (cast)
        {
            if (probing()) device_noticed = TRUE;
        }
        break;
    case EFFECT_RUNE_EXPLOSIVE:
        if (name) return "Explosive Rune";
        if (desc) return "It sets a rune which will explode on a passing monster.";
        if (value) return format("%d", 500);
        if (cast)
        {
            if (explosive_rune()) device_noticed = TRUE;
        }
        break;
    case EFFECT_RUNE_PROTECTION:
        if (name) return "Rune of Protection";
        if (desc) return "It creates a glyph that inhibits monsters from attacking you.";
        if (value) return format("%d", 5000);
        if (cast)
        {
            if (warding_glyph()) device_noticed = TRUE;
        }
        break;
    case EFFECT_SATISFY_HUNGER:
        if (name) return "Satisfy Hunger";
        if (desc) return "It fills your belly with nourishing victuals.";
        if (value) return format("%d", 500);
        if (cast)
        {
            if (set_food(PY_FOOD_MAX - 1)) device_noticed = TRUE;
        }
        break;
    case EFFECT_DESTROY_TRAP:
        if (name) return "Trap and Door Destruction";
        if (desc) return "It destroys all traps and doors in adjacent squares.";
        if (value) return format("%d", 500);
        if (cast)
        {
            if (destroy_doors_touch()) device_noticed = TRUE;
        }
        break;
    case EFFECT_DESTROY_TRAPS:
        if (name) return "Unbarring Ways";
        if (desc) return "It fires a beam which destroys traps and doors.";
        if (value) return format("%d", 1000);
        if (cast)
        {
            if (!get_aim_dir(&dir)) return NULL;
            if (destroy_door(dir)) device_noticed = TRUE;
        }
        break;
    case EFFECT_WHIRLWIND_ATTACK:
        if (name) return "Whirlwind Attack";
        if (desc) return "It causes you to attack all adjacent monsters in a single turn.";
        if (value) return format("%d", 5000);
        if (cast)
        {
            int           y = 0, x = 0;
            cave_type    *c_ptr;
            monster_type *m_ptr;
            int           dir;

            for (dir = 0; dir < 8; dir++)
            {
                y = py + ddy_ddd[dir];
                x = px + ddx_ddd[dir];
                c_ptr = &cave[y][x];
                m_ptr = &m_list[c_ptr->m_idx];
                if (c_ptr->m_idx && (m_ptr->ml || cave_have_flag_bold(y, x, FF_PROJECT)))
                {
                    py_attack(y, x, 0);
                    device_noticed = TRUE;
                }
            }
        }
        break;
    case EFFECT_LIST_UNIQUES:
        if (name) return "List Uniques";
        if (desc) return "It lists all uniques on the current level.";
        if (value) return format("%d", 12000);
        if (cast)
        { 
            int i;
            for (i = m_max - 1; i >= 1; i--)
            {
                if (!m_list[i].r_idx) continue;
                if (r_info[m_list[i].r_idx].flags1 & RF1_UNIQUE)
                {
                    msg_format("%s. ", r_name + r_info[m_list[i].r_idx].name);
                    device_noticed = TRUE;
                }
            }
        }
        break;
    case EFFECT_LIST_ARTIFACTS:
        if (name) return "List Artifacts";
        if (desc) return "It lists all artifacts on the current level.";
        if (value) return format("%d", 15000);
        if (cast)
        { 
            int i;
            for (i = 1; i < o_max; i++)
            {
                object_type *o_ptr = &o_list[i];
                if (!o_ptr->k_idx) continue;
                if (o_ptr->held_m_idx) continue;
                if (o_ptr->name1)
                {
                    msg_format("%s. ", a_name + a_info[o_ptr->name1].name);
                    device_noticed = TRUE;
                }
                if (o_ptr->art_name)
                {
                    msg_format("%s. ", quark_str(o_ptr->art_name));
                    device_noticed = TRUE;
                }
            }
        }
        break;
    case EFFECT_BANISH_EVIL:
    {
        int power = _extra(effect, 100);
        if (name) return "Banish Evil";
        if (desc) return "It attempts to teleport all visible evil monsters away.";
        if (info) return info_power(_BOOST(power));
        if (value) return format("%d", 50*power);
        if (cast)
        {
            if (banish_evil(_BOOST(power)))
            {
                msg_print("The holy power banishes evil!");
                device_noticed = TRUE;
            }
        }
        break;
    }
    case EFFECT_BANISH_ALL:
    {
        int power = _extra(effect, 150);
        if (name) return "Banish";
        if (desc) return "It teleports all monsters in sight away unless resisted.";
        if (info) return info_power(_BOOST(power));
        if (value) return format("%d", 70*power);
        if (cast)
        {
            if (banish_monsters(_BOOST(power)))
                device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_TELEKINESIS:
    {
        int weight = effect->level * 15;
        if (name) return "Telekinesis";
        if (desc) return "It pulls a distant item close to you.";
        if (info) return info_weight(_BOOST(weight));
        if (value) return format("%d", 10*weight);
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
        if (cast)
        {
            self_knowledge();
            device_noticed = TRUE;
        }
        break;

    /* Timed Buffs */
    case EFFECT_STONE_SKIN:
    {
        int power = _extra(effect, 20);
        if (name) return "Stone Skin";
        if (desc) return "It temporarily turns your skin to stone, granting enhanced armor class.";
        if (info) return format("Dur d%d + %d", _BOOST(power), _BOOST(power));
        if (value) return format("%d", 4000 + 50*power);
        if (cast)
        {
            if (set_shield(_BOOST(power + randint1(power)), FALSE))
                device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_RESIST_ACID:
    {
        int power = _extra(effect, 20);
        if (name) return "Resist Acid";
        if (desc) return "It grants temporary acid resistance.";
        if (info) return format("Dur d%d + %d", _BOOST(power), _BOOST(power));
        if (value) return format("%d", 1000 + 25*power);
        if (cast)
        {
            if (set_oppose_acid(_BOOST(power + randint1(power)), FALSE))
                device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_RESIST_ELEC:
    {
        int power = _extra(effect, 20);
        if (name) return "Resist Lightning";
        if (desc) return "It grants temporary lightning resistance.";
        if (info) return format("Dur d%d + %d", _BOOST(power), _BOOST(power));
        if (value) return format("%d", 1000 + 25*power);
        if (cast)
        {
            if (set_oppose_elec(_BOOST(power + randint1(power)), FALSE))
                device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_RESIST_FIRE:
    {
        int power = _extra(effect, 20);
        if (name) return "Resist Fire";
        if (desc) return "It grants temporary fire resistance.";
        if (info) return format("Dur d%d + %d", _BOOST(power), _BOOST(power));
        if (value) return format("%d", 1000 + 25*power);
        if (cast)
        {
            if (set_oppose_fire(_BOOST(power + randint1(power)), FALSE))
                device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_RESIST_COLD:
    {
        int power = _extra(effect, 20);
        if (name) return "Resist Cold";
        if (desc) return "It grants temporary cold resistance.";
        if (info) return format("Dur d%d + %d", _BOOST(power), _BOOST(power));
        if (value) return format("%d", 1000 + 25*power);
        if (cast)
        {
            if (set_oppose_cold(_BOOST(power + randint1(power)), FALSE))
                device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_RESIST_POIS:
    {
        int power = _extra(effect, 20);
        if (name) return "Resist Poison";
        if (desc) return "It grants temporary poison resistance.";
        if (info) return format("Dur d%d + %d", _BOOST(power), _BOOST(power));
        if (value) return format("%d", 2500 + 25*power);
        if (cast)
        {
            if (set_oppose_pois(_BOOST(power + randint1(power)), FALSE))
                device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_RESISTANCE:
    {
        int power = _extra(effect, 20);
        if (name) return "Resistance";
        if (desc) return "It grants temporary resistance to the elements and poison.";
        if (info) return format("Dur d%d + %d", _BOOST(power), _BOOST(power));
        if (value) return format("%d", 5000 + 25*power);
        if (cast)
        {
            int dur = _BOOST(power + randint1(power));
            if (set_oppose_acid(dur, FALSE)) device_noticed = TRUE;
            if (set_oppose_elec(dur, FALSE)) device_noticed = TRUE;
            if (set_oppose_fire(dur, FALSE)) device_noticed = TRUE;
            if (set_oppose_cold(dur, FALSE)) device_noticed = TRUE;
            if (set_oppose_pois(dur, FALSE)) device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_PROT_EVIL:
    {
        int power = _extra(effect, 100);
        if (name) return "Protection from Evil";
        if (desc) return "It gives temporary melee protection from evil creatures.";
        if (info) return format("Dur d%d + %d", 25, _BOOST(power));
        if (value) return format("%d", 2000 + 10*power);
        if (cast)
        {
            if (set_protevil(_BOOST(randint1(25) + power), FALSE))
                device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_HOLY_GRAIL:
        if (name) return "Healing and Magic Resistance";
        if (desc) return "It heals you and gives temporary resistance to magic.";
        if (info) return format("Dur d%d + %d", 10, _BOOST(10));
        if (value) return format("%d", 5000);
        if (cast)
        {
            if (hp_player(50)) 
                device_noticed = TRUE;
            if (set_resist_magic(_BOOST(10 + randint1(10)), FALSE)) 
                device_noticed = TRUE;
        }
        break;
    case EFFECT_BLESS:
    {
        int power = _extra(effect, 24);
        if (name) return "Holy Prayer";
        if (desc) return "It blesses you temporarily when you read it.";
        if (info) return format("Dur d%d + %d", _BOOST(power), 6);
        if (value) return format("%d", 1000 + 25*power);
        if (cast)
        {
            if (set_blessed(_BOOST(randint1(power) + 6), FALSE))
                device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_HEROISM:
    {
        int power = _extra(effect, 25);
        if (name) return "Heroism";
        if (desc) return "It grants temporary heroism.";
        if (info) return format("Dur d%d + %d", _BOOST(power), _BOOST(power));
        if (value) return format("%d", 1500 + 25*power);
        if (cast)
        {
            if (set_hero(_BOOST(randint1(power) + power), FALSE))
                device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_BERSERK:
    {
        int power = _extra(effect, 25);
        if (name) return "Berserk";
        if (desc) return "It causes you to enter a berserk rage, granting enhanced combat prowess but diminished stealth and skills.";
        if (info) return format("Dur d%d + %d", _BOOST(power), _BOOST(power));
        if (value) return format("%d", 1500 + 25*power);
        if (cast)
        {
            if (set_shero(_BOOST(randint1(power) + power), FALSE))
                device_noticed = TRUE;
            if (hp_player(30))
                device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_SPEED:
    {
        int power = _extra(effect, 20);
        if (name) return "Haste Self";
        if (desc) return "It grants a temporary speed boost.";
        if (info) return format("Dur d%d + %d", _BOOST(power), _BOOST(power));
        if (value) return format("%d", 2500 + 25*power);
        if (cast)
        {
            if (set_fast(_BOOST(randint1(power) + power), FALSE))
                device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_SPEED_HERO:
    {
        int power = _extra(effect, 20);
        if (name) return "Heroic Speed";
        if (desc) return "It grants temporary speed and heroism.";
        if (info) return format("Dur d%d + %d", _BOOST(power), _BOOST(power));
        if (value) return format("%d", 5000 + 25*power);
        if (cast)
        {
            int dur = _BOOST(randint1(power) + power);
            if (set_fast(dur, FALSE)) device_noticed = TRUE;
            if (set_hero(dur, FALSE)) device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_SPEED_HERO_BLESS:
    {
        int power = _extra(effect, 15);
        if (name) return "Heroic Song";
        if (desc) return "It grants temporary speed, blessing and heroism.";
        if (info) return format("Dur d%d + %d", _BOOST(power), _BOOST(power));
        if (value) return format("%d", 7500 + 50*power);
        if (cast)
        {
            int dur = _BOOST(randint1(power) + power);
            if (set_fast(dur, FALSE)) device_noticed = TRUE;
            if (set_hero(dur, FALSE)) device_noticed = TRUE;
            if (set_blessed(dur, FALSE)) device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_SPEED_ESSENTIA:
    {
        int power = _extra(effect, 5);
        if (name) return "Speed Essentia";
        if (desc) return "It temporarily grants you extra melee attacks.";
        if (info) return format("Dur d%d + %d", _BOOST(power), _BOOST(power));
        if (value) return format("%d", 5000 + 1000*power);
        if (cast)
        {
            if (set_tim_speed_essentia(_BOOST(5 + randint1(5)), FALSE))
                device_noticed = TRUE;
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
        if (cast)
        {
            if (set_lightspeed(_BOOST(power), FALSE))
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
        if (value) return format("%d", 5000 + 500*power);
        if (cast)
        {
            if (set_tim_enlarge_weapon(_BOOST(power), FALSE))
                device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_TELEPATHY:
    {
        int power = _extra(effect, 30);
        if (name) return "Telepathy";
        if (desc) return "It grants you the power of telepathy temporarily.";
        if (info) return format("Dur d%d + %d", _BOOST(power), _BOOST(25));
        if (value) return format("%d", 2000 + 50*power);
        if (cast)
        {
            if (set_tim_esp(_BOOST(randint1(power) + 25), FALSE))
                device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_WRAITHFORM:
    {
        int power = _extra(effect, 25);
        if (name) return "Wraithform";
        if (desc) return "It turns you int a wraith, giving the ability to pass through walls as well as reducing the amount of physical damage sustained from attacks.";
        if (info) return format("Dur d%d + %d", _BOOST(power), _BOOST(power));
        if (value) return format("%d", 10000 + 100*power);
        if (cast)
        {
            if (set_wraith_form(_BOOST(randint1(power) + power), FALSE))
                device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_INVULNERABILITY:
    {
        int power = _extra(effect, 8);
        if (name) return "Globe of Invulnerability";
        if (desc) return "It generates barrier which completely protect you from almost all damages. Takes a few your turns when the barrier breaks or duration time is exceeded.";
        if (info) return format("Dur d%d + %d", _BOOST(power), _BOOST(power));
        if (value) return format("%d", 15000 + 1000*power);
        if (cast)
        {
            if (set_invuln(_BOOST(randint1(power) + power), FALSE))
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
                if (summon_specific(-1, py, px, dun_level, 0, PM_FORCE_PET | PM_ALLOW_GROUP))
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
                if (summon_specific(-1, py, px, dun_level, SUMMON_HOUND, PM_FORCE_PET | PM_ALLOW_GROUP))
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
                if (summon_specific(-1, py, px, dun_level, SUMMON_ANT, PM_FORCE_PET | PM_ALLOW_GROUP))
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
                if (summon_specific(-1, py, px, dun_level, SUMMON_HYDRA, PM_FORCE_PET | PM_ALLOW_GROUP))
                    device_noticed = TRUE;
            }
        }
        break;
    case EFFECT_SUMMON_OCTOPUS:
        if (name) return "Summon Ocotpus";
        if (desc) return "It attempts to summon octopi for assistance.";
        if (value) return format("%d", 1200);
        if (cast)
        {
            int num = randint0(3);
            int i;
            for (i = 0; i < num; i++)
            {
                if (summon_named_creature(-1, py, px, MON_JIZOTAKO, PM_FORCE_PET | PM_ALLOW_GROUP))
                    device_noticed = TRUE;
            }
        }
        break;
    case EFFECT_SUMMON_DAWN:
        if (name) return "Summon Legion of the Dawn";
        if (desc) return "It attempts to summon Warriors of the Dawn for assistance.";
        if (value) return format("%d", 5000);
        if (cast)
        {
            if (summon_specific(-1, py, px, dun_level, SUMMON_DAWN, (PM_ALLOW_GROUP | PM_FORCE_PET)))
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
            if (summon_specific(-1, py, px, dun_level, SUMMON_PHANTOM, (PM_ALLOW_GROUP | PM_FORCE_PET)))
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
            int  lvl = dun_level;
            u32b mode = pet ? PM_FORCE_PET : PM_NO_PET;
            int  who = pet ? -1 : 0;

            if (!pet || lvl >= 50) 
                mode |= PM_ALLOW_GROUP;

            if (summon_specific(who, py, px, lvl, SUMMON_ELEMENTAL, mode))
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
            if (summon_specific(-1, py, px, dun_level, SUMMON_DRAGON, PM_FORCE_PET))
                device_noticed = TRUE;
        }
        break;
    case EFFECT_SUMMON_UNDEAD:
        if (name) return "Summon Undead";
        if (desc) return "It attempts to summon a single undead monster to serve you.";
        if (value) return format("%d", 1500);
        if (cast)
        {
            bool pet = one_in_(3);
            int  lvl = dun_level;
            int  type = lvl > 75 ? SUMMON_HI_UNDEAD : SUMMON_UNDEAD;
            u32b mode = pet ? PM_FORCE_PET : PM_NO_PET;
            int  who = pet ? -1 : 0;

            if (!pet || lvl >= 50) 
                mode |= PM_ALLOW_GROUP;

            if (summon_specific(who, py, px, lvl, type, mode))
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
        if (cast)
        {
            bool pet = one_in_(3);
            int  lvl = dun_level;
            u32b mode = pet ? PM_FORCE_PET : PM_NO_PET;
            int  who = pet ? -1 : 0;

            if (!pet || lvl >= 50) 
                mode |= PM_ALLOW_GROUP;

            if (summon_specific(who, py, px, lvl, SUMMON_DEMON, mode))
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
        if (cast)
        {
            if (summon_specific(-1, py, px, dun_level, SUMMON_CYBER, PM_FORCE_PET))
                device_noticed = TRUE;
        }
        break;
    case EFFECT_SUMMON_ANGEL:
        if (name) return "Summon Angel";
        if (desc) return "It attempts to summon a single angel for assistance.";
        if (value) return format("%d", 5000);
        if (cast)
        {
            if (summon_specific(-1, py, px, dun_level, SUMMON_ANGEL, PM_FORCE_PET))
                device_noticed = TRUE;
        }
        break;
    case EFFECT_SUMMON_KRAKEN:
        if (name) return "Summon Kraken";
        if (desc) return "It attempts to summon powerful kraken for assistance.";
        if (value) return format("%d", 5000);
        if (cast)
        {
            int num = randint0(3);
            int ct = 0;
            int i;
            fire_ball_hide(GF_WATER_FLOW, 0, 3, 3);
            device_noticed = TRUE;
            for (i = 0; i < num; i++)
                ct += summon_specific(-1, py, px, dun_level, SUMMON_KRAKEN, PM_FORCE_PET);
            if (!ct)
                msg_print("No help arrives.");
        }
        break;

    case EFFECT_CHARM_ANIMAL:
    {
        int lvl = _extra(effect, p_ptr->lev);
        if (name) return "Charm Animal";
        if (desc) return "It attempts to charm a single animal.";
        if (info) return format("Power %d", _BOOST(lvl));
        if (value) return format("%d", 10*_extra(effect, 50));
        if (cast)
        {
            if (!get_aim_dir(&dir)) return FALSE;
            if (charm_animal(dir, _BOOST(lvl)))
                device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_CHARM_DEMON:
    {
        int lvl = _extra(effect, p_ptr->lev);
        if (name) return "Dominate Demon";
        if (desc) return "It attempts to dominate a single demon.";
        if (info) return format("Power %d", _BOOST(lvl));
        if (value) return format("%d", 15*_extra(effect, 50));
        if (cast)
        {
            if (!get_aim_dir(&dir)) return FALSE;
            if (control_one_demon(dir, _BOOST(lvl)))
                device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_CHARM_UNDEAD:
    {
        int lvl = _extra(effect, p_ptr->lev);
        if (name) return "Enslave Undead";
        if (desc) return "It attempts to enslave a single undead monster.";
        if (info) return format("Power %d", _BOOST(lvl));
        if (value) return format("%d", 15*_extra(effect, 50));
        if (cast)
        {
            if (!get_aim_dir(&dir)) return FALSE;
            if (control_one_undead(dir, _BOOST(lvl)))
                device_noticed = TRUE;
        }
        break;
    }

    case EFFECT_RETURN_PETS:
        if (name) return "Return Pets";
        if (desc) return "It calls your pets back to you.";
        if (value) return format("%d", 500);
        if (cast)
        {
            int pet_ctr, i;
            u16b *who;
            int max_pet = 0;
            u16b dummy_why;

            stop_mouth();

            C_MAKE(who, max_m_idx, u16b);

            for (pet_ctr = m_max - 1; pet_ctr >= 1; pet_ctr--)
            {
                if (is_pet(&m_list[pet_ctr]) && (p_ptr->riding != pet_ctr))
                    who[max_pet++] = pet_ctr;
            }

            ang_sort_comp = ang_sort_comp_pet;
            ang_sort_swap = ang_sort_swap_hook;
            ang_sort(who, &dummy_why, max_pet);

            for (i = 0; i < max_pet; i++)
            {
                pet_ctr = who[i];
                teleport_monster_to(pet_ctr, py, px, 100, TELEPORT_PASSIVE);
                device_noticed = TRUE;
            }

            C_KILL(who, max_m_idx, u16b);
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
        if (desc) return "It restores your experience.";
        if (value) return format("%d", 1000);
        if (cast)
        {
            if (restore_level()) device_noticed = TRUE;
        }
        break;
    case EFFECT_RESTORING:
        if (name) return "Restoring";
        if (desc) return "It restores your stats and experience.";
        if (value) return format("%d", 6000);
        if (cast)
        {
            if (do_res_stat(A_STR)) device_noticed = TRUE;
            if (do_res_stat(A_INT)) device_noticed = TRUE;
            if (do_res_stat(A_WIS)) device_noticed = TRUE;
            if (do_res_stat(A_DEX)) device_noticed = TRUE;
            if (do_res_stat(A_CON)) device_noticed = TRUE;
            if (do_res_stat(A_CHR)) device_noticed = TRUE;
            if (restore_level()) device_noticed = TRUE;
        }
        break;
    case EFFECT_HEAL:
    {
        int amt = _extra(effect, 50);
        if (name) return "Cure Wounds";
        if (desc) return "It heals your hitpoints and cuts.";
        if (info) return info_heal(0, 0, _BOOST(amt));
        if (value) return format("%d", 10*amt);
        if (cast)
        {
            amt = _BOOST(amt);

            if (hp_player(amt)) device_noticed = TRUE;
            if (amt >= 100)
            {
                if (set_cut(0, TRUE)) device_noticed = TRUE;
            }
            else
            {
                if (set_cut(p_ptr->cut - amt, TRUE)) device_noticed = TRUE;
            }
        }
        break;
    }
    case EFFECT_CURING:
    {
        int amt = _extra(effect, 50);
        if (name) return "Curing";
        if (desc) return "It heals you a bit and cures blindness, poison, confusion, stunning, cuts and hallucination when you quaff it.";
        if (info) return info_heal(0, 0, _BOOST(amt));
        if (value) return format("%d", 500 + 10*amt);
        if (cast)
        {
            if (hp_player(_BOOST(amt))) device_noticed = TRUE;
            if (set_blind(0, TRUE)) device_noticed = TRUE;
            if (set_poisoned(0, TRUE)) device_noticed = TRUE;
            if (set_confused(0, TRUE)) device_noticed = TRUE;
            if (set_stun(0, TRUE)) device_noticed = TRUE;
            if (set_cut(0, TRUE)) device_noticed = TRUE;
            if (set_image(0, TRUE)) device_noticed = TRUE;
            if (set_shero(0,TRUE)) device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_HEAL_CURING:
    {
        int amt = _extra(effect, 300);
        if (name) return "Healing";
        if (desc) return "It heals your hitpoints and cures your ailments.";
        if (info) return info_heal(0, 0, _BOOST(amt));
        if (value) return format("%d", 10*amt);
        if (cast)
        {
            amt = _BOOST(amt);

            if (hp_player(amt)) device_noticed = TRUE;
            if (set_blind(0, TRUE)) device_noticed = TRUE;
            if (amt >= 100)
            {
                if (set_cut(0, TRUE)) device_noticed = TRUE;
                if (set_confused(0, TRUE)) device_noticed = TRUE;
                if (set_poisoned(0, TRUE)) device_noticed = TRUE;
                if (set_stun(0, TRUE)) device_noticed = TRUE;
            }
            else
            {
                if (set_cut(p_ptr->cut - amt, TRUE)) device_noticed = TRUE;
            }
            if (set_shero(0,TRUE)) device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_HEAL_CURING_HERO:
    {
        int amt = _extra(effect, 777);
        if (name) return "Angelic Healing";
        if (desc) return "It heals your hitpoints, cures what ails you, and makes you heroic.";
        if (info) return info_heal(0, 0, _BOOST(amt));
        if (value) return format("%d", 1000 + 10*amt);
        if (cast)
        {
            if (hp_player(_BOOST(amt))) device_noticed = TRUE;
            if (set_blind(0, TRUE)) device_noticed = TRUE;
            if (set_cut(0, TRUE)) device_noticed = TRUE;
            if (set_confused(0, TRUE)) device_noticed = TRUE;
            if (set_poisoned(0, TRUE)) device_noticed = TRUE;
            if (set_stun(0, TRUE)) device_noticed = TRUE;
            if (set_shero(0,TRUE)) device_noticed = TRUE;
            if (set_hero(_BOOST(randint1(25) + 25), FALSE)) device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_RESTORE_MANA:
        if (name) return "Restore Mana";
        if (desc) return "It completely restores your mana.";
        if (value) return format("%d", 1000);
        if (cast)
        {
            if (restore_mana()) device_noticed = TRUE;
            if (set_shero(0,TRUE)) device_noticed = TRUE;
        }
        break;
    case EFFECT_CURE_POIS:
        if (name) return "Cure Poison";
        if (desc) return "It cures poison.";
        if (value) return format("%d", 100);
        if (cast)
        {
            if (set_poisoned(0, TRUE)) device_noticed = TRUE;
        }
        break;
    case EFFECT_CURE_FEAR:
        if (name) return "Boldness";
        if (desc) return "It restores your courage.";
        if (value) return format("%d", 250);
        if (cast)
        {
            if (p_ptr->afraid)
            {
                fear_clear_p();
                device_noticed = TRUE;
            }
        }
        break;
    case EFFECT_CURE_FEAR_POIS:
        if (name) return "Cure Fear and Poison";
        if (desc) return "It cures poison and restores your courage in battle.";
        if (value) return format("%d", 500);
        if (cast)
        {
            if (set_poisoned(0, TRUE)) device_noticed = TRUE;
            if (p_ptr->afraid)
            {
                fear_clear_p();
                device_noticed = TRUE;
            }
        }
        break;
    case EFFECT_REMOVE_CURSE:
        if (name) return "Remove Curse";
        if (desc) return "It removes normal curses from equipped items.";
        if (value) return format("%d", 150);
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
        if (value) return format("%d", 1000);
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
        int amt = _extra(effect, 25);
        if (name) return "Clarity";
        if (desc) return "It clears your mind, restoring some mana.";
        if (info) return format("%dsp", _BOOST(amt));
        if (value) return format("%d", 10*amt);
        if (cast)
        {
            if (sp_player(_BOOST(amt)))
            {
                msg_print("You feel your mind clear.");
                device_noticed = TRUE;
            }
        }
        break;
    }

    /* Offense: Bolts */
    case EFFECT_BOLT_MISSILE:
    {
        int dd = _extra(effect, 2 + p_ptr->lev/10);
        if (name) return "Magic Missile";
        if (desc) return "It fires a weak bolt of magic.";
        if (info) return info_damage(_BOOST(dd), 6, 0);
        if (value) return format("%d", 250 + 125*_extra(effect, 2));
        if (cast)
        {
            if (!get_aim_dir(&dir)) return NULL;
            fire_bolt_or_beam(20, GF_MISSILE, dir, _BOOST(damroll(dd, 6)));
            device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_BOLT_ACID:
    {
        int dd = _extra(effect, 6 + p_ptr->lev/7);
        if (name) return "Acid Bolt";
        if (desc) return "It fires a bolt of acid.";
        if (info) return info_damage(_BOOST(dd), 8, 0);
        if (value) return format("%d", 250 + 150*_extra(effect, 6));
        if (cast)
        {
            if (!get_aim_dir(&dir)) return NULL;
            fire_bolt_or_beam(20, GF_ACID, dir, _BOOST(damroll(dd, 8)));
            device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_BOLT_ELEC:
    {
        int dd = _extra(effect, 4 + p_ptr->lev/9);
        if (name) return "Lightning Bolt";
        if (desc) return "It fires a bolt of lightning.";
        if (info) return info_damage(_BOOST(dd), 8, 0);
        if (value) return format("%d", 250 + 150*_extra(effect, 4));
        if (cast)
        {
            if (!get_aim_dir(&dir)) return NULL;
            fire_bolt_or_beam(20, GF_ELEC, dir, _BOOST(damroll(dd, 8)));
            device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_BOLT_FIRE:
    {
        int dd = _extra(effect, 7 + p_ptr->lev/6);
        if (name) return "Fire Bolt";
        if (desc) return "It fires a bolt of fire.";
        if (info) return info_damage(_BOOST(dd), 8, 0);
        if (value) return format("%d", 250 + 150*_extra(effect, 7));
        if (cast)
        {
            if (!get_aim_dir(&dir)) return NULL;
            fire_bolt_or_beam(20, GF_FIRE, dir, _BOOST(damroll(dd, 8)));
            device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_BOLT_COLD:
    {
        int dd = _extra(effect, 5 + p_ptr->lev/8);
        if (name) return "Frost Bolt";
        if (desc) return "It fires a bolt of frost.";
        if (info) return info_damage(_BOOST(dd), 8, 0);
        if (value) return format("%d", 250 + 150*_extra(effect, 5));
        if (cast)
        {
            if (!get_aim_dir(&dir)) return NULL;
            fire_bolt_or_beam(20, GF_COLD, dir, _BOOST(damroll(dd, 8)));
            device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_BOLT_POIS:
    {
        int dd = _extra(effect, 5 + p_ptr->lev/8);
        if (name) return "Poison Dart";
        if (desc) return "It fires a poison dart.";
        if (info) return info_damage(_BOOST(dd), 8, 0);
        if (value) return format("%d", 250 + 150*_extra(effect, 5));
        if (cast)
        {
            if (!get_aim_dir(&dir)) return NULL;
            fire_bolt(GF_POIS, dir, _BOOST(damroll(dd, 8)));
            device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_BOLT_LITE:
    {
        int dd = _extra(effect, 5 + p_ptr->lev/8);
        if (name) return "Light Bolt";
        if (desc) return "It fires a bolt of light.";
        if (info) return info_damage(_BOOST(dd), 8, 0);
        if (value) return format("%d", 250 + 150*_extra(effect, 5));
        if (cast)
        {
            if (!get_aim_dir(&dir)) return NULL;
            fire_bolt_or_beam(20, GF_LITE, dir, _BOOST(damroll(dd, 8)));
            device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_BOLT_DARK:
    {
        int dd = _extra(effect, 5 + p_ptr->lev/8);
        if (name) return "Dark Bolt";
        if (desc) return "It fires a bolt of darkness.";
        if (info) return info_damage(_BOOST(dd), 8, 0);
        if (value) return format("%d", 250 + 150*_extra(effect, 5));
        if (cast)
        {
            if (!get_aim_dir(&dir)) return NULL;
            fire_bolt_or_beam(20, GF_DARK, dir, _BOOST(damroll(dd, 8)));
            device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_BOLT_CONF:
    {
        int dd = _extra(effect, 5 + p_ptr->lev/8);
        if (name) return "Confusion Bolt";
        if (desc) return "It fires a bolt of confusion.";
        if (info) return info_damage(_BOOST(dd), 8, 0);
        if (value) return format("%d", 250 + 150*_extra(effect, 5));
        if (cast)
        {
            if (!get_aim_dir(&dir)) return NULL;
            fire_bolt(GF_CONFUSION, dir, _BOOST(damroll(dd, 8)));
            device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_BOLT_NETHER:
    {
        int dd = _extra(effect, 10 + p_ptr->lev/6);
        if (name) return "Nether Bolt";
        if (desc) return "It fires a bolt of nether.";
        if (info) return info_damage(_BOOST(dd), 8, 0);
        if (value) return format("%d", 250 + 150*_extra(effect, 10));
        if (cast)
        {
            if (!get_aim_dir(&dir)) return NULL;
            fire_bolt(GF_NETHER, dir, _BOOST(damroll(dd, 8)));
            device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_BOLT_NEXUS:
    {
        int dd = _extra(effect, 7 + p_ptr->lev/6);
        if (name) return "Nexus Bolt";
        if (desc) return "It fires a bolt of nexus.";
        if (info) return info_damage(_BOOST(dd), 8, 0);
        if (value) return format("%d", 250 + 150*_extra(effect, 7));
        if (cast)
        {
            if (!get_aim_dir(&dir)) return NULL;
            fire_bolt(GF_NEXUS, dir, _BOOST(damroll(dd, 8)));
            device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_BOLT_SOUND:
    {
        int dd = _extra(effect, 7 + p_ptr->lev/6);
        if (name) return "Sound Bolt";
        if (desc) return "It fires a bolt of sound.";
        if (info) return info_damage(_BOOST(dd), 8, 0);
        if (value) return format("%d", 250 + 150*_extra(effect, 7));
        if (cast)
        {
            if (!get_aim_dir(&dir)) return NULL;
            fire_bolt(GF_SOUND, dir, _BOOST(damroll(dd, 8)));
            device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_BOLT_SHARDS:
    {
        int dd = _extra(effect, 7 + p_ptr->lev/5);
        if (name) return "Shard Bolt";
        if (desc) return "It fires a bolt of shards.";
        if (info) return info_damage(_BOOST(dd), 8, 0);
        if (value) return format("%d", 250 + 150*_extra(effect, 7));
        if (cast)
        {
            if (!get_aim_dir(&dir)) return NULL;
            fire_bolt(GF_SHARDS, dir, _BOOST(damroll(dd, 8)));
            device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_BOLT_CHAOS:
    {
        int dd = _extra(effect, 7 + p_ptr->lev/6);
        if (name) return "Chaos Bolt";
        if (desc) return "It fires a bolt of chaos.";
        if (info) return info_damage(_BOOST(dd), 8, 0);
        if (value) return format("%d", 250 + 150*_extra(effect, 7));
        if (cast)
        {
            if (!get_aim_dir(&dir)) return NULL;
            fire_bolt(GF_CHAOS, dir, _BOOST(damroll(dd, 8)));
            device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_BOLT_DISEN:
    {
        int dd = _extra(effect, 7 + p_ptr->lev/6);
        if (name) return "Disenchantment Bolt";
        if (desc) return "It fires a bolt of disenchantment.";
        if (info) return info_damage(_BOOST(dd), 8, 0);
        if (value) return format("%d", 250 + 150*_extra(effect, 7));
        if (cast)
        {
            if (!get_aim_dir(&dir)) return NULL;
            fire_bolt(GF_DISENCHANT, dir, _BOOST(damroll(dd, 8)));
            device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_BOLT_TIME:
    {
        int dd = _extra(effect, 7 + p_ptr->lev/6);
        if (name) return "Time Bolt";
        if (desc) return "It fires a bolt of time.";
        if (info) return info_damage(_BOOST(dd), 8, 0);
        if (value) return format("%d", 250 + 200*_extra(effect, 7));
        if (cast)
        {
            if (!get_aim_dir(&dir)) return NULL;
            fire_bolt(GF_TIME, dir, _BOOST(damroll(dd, 8)));
            device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_BOLT_WATER:
    {
        int dd = _extra(effect, 7 + p_ptr->lev/4);
        if (name) return "Water Bolt";
        if (desc) return "It fires a bolt of water.";
        if (info) return info_damage(_BOOST(dd), 8, 0);
        if (value) return format("%d", 250 + 150*_extra(effect, 7));
        if (cast)
        {
            if (!get_aim_dir(&dir)) return NULL;
            fire_bolt(GF_WATER, dir, _BOOST(damroll(dd, 8)));
            device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_BOLT_MANA:
    {
        int dam = _extra(effect, 100 + 2*p_ptr->lev);
        if (name) return "Mana Bolt";
        if (desc) return "It fires a powerful bolt of mana.";
        if (info) return info_damage(0, 0, _BOOST(dam));
        if (value) return format("%d", 15*_extra(effect, 100));
        if (cast)
        {
            if (device_known && !get_aim_dir(&dir)) return NULL;
            fire_bolt(GF_MANA, dir, _BOOST(dam));
            device_noticed = TRUE;
        }
        break;
    }

    /* Offense: Beams */
    case EFFECT_BEAM_LITE_WEAK:
    {
        int dd = _extra(effect, 6);
        if (name) return "Beam of Light";
        if (desc) return "It fires a beam of light.";
        if (info) return info_damage(_BOOST(dd), 8, 0);
        if (value) return format("%d", 100 + 50*_extra(effect, 6));
        if (cast)
        {
            if (!get_aim_dir(&dir)) return NULL;
            msg_print("A line of blue shimmering light appears.");
            project_hook(GF_LITE_WEAK, dir, _BOOST(damroll(dd, 8)), PROJECT_BEAM | PROJECT_GRID | PROJECT_KILL);
            device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_BEAM_LITE:
    {
        int dam = _extra(effect, 50);
        if (name) return "Beam of Light";
        if (desc) return "It fires a powerful beam of light.";
        if (info) return info_damage(0, 0, _BOOST(dam));
        if (value) return format("%d", 10*dam);
        if (cast)
        {
            if (!get_aim_dir(&dir)) return NULL;
            msg_print("A line of pure white light appears.");
            fire_beam(GF_LITE, dir, _BOOST(dam));
            device_noticed = TRUE;
        }
        break;
    }

    /* Offense: Balls */
    case EFFECT_BALL_ACID:
    {
        int dam = _extra(effect, 60 + p_ptr->lev);
        if (name) return "Acid Ball";
        if (desc) return "It fires a ball of acid.";
        if (info) return info_damage(0, 0, _BOOST(dam));
        if (value) return format("%d", 500 + 150*_extra(effect, 60));
        if (cast)
        {
            if (!get_aim_dir(&dir)) return NULL;
            fire_ball(GF_ACID, dir, _BOOST(dam), 2);
            device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_BALL_ELEC:
    {
        int dam = _extra(effect, 40 + p_ptr->lev);
        if (name) return "Lightning Ball";
        if (desc) return "It fires a ball of lightning.";
        if (info) return info_damage(0, 0, _BOOST(dam));
        if (value) return format("%d", 500 + 150*_extra(effect, 40));
        if (cast)
        {
            if (!get_aim_dir(&dir)) return NULL;
            fire_ball(GF_ELEC, dir, _BOOST(dam), 2);
            device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_BALL_FIRE:
    {
        int dam = _extra(effect, 70 + p_ptr->lev);
        if (name) return "Fire Ball";
        if (desc) return "It fires a ball of fire.";
        if (info) return info_damage(0, 0, _BOOST(dam));
        if (value) return format("%d", 500 + 150*_extra(effect, 70));
        if (cast)
        {
            if (!get_aim_dir(&dir)) return NULL;
            fire_ball(GF_FIRE, dir, _BOOST(dam), 2);
            device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_BALL_COLD:
    {
        int dam = _extra(effect, 50 + p_ptr->lev);
        if (name) return "Frost Ball";
        if (desc) return "It fires a ball of frost.";
        if (info) return info_damage(0, 0, _BOOST(dam));
        if (value) return format("%d", 500 + 150*_extra(effect, 50));
        if (cast)
        {
            if (!get_aim_dir(&dir)) return NULL;
            fire_ball(GF_COLD, dir, _BOOST(dam), 2);
            device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_BALL_POIS:
    {
        int dam = _extra(effect, 12 + p_ptr->lev/4);
        if (name) return "Stinking Cloud";
        if (desc) return "It fires a ball of poison.";
        if (info) return info_damage(0, 0, _BOOST(dam));
        if (value) return format("%d", 10*_extra(effect, 20));
        if (cast)
        {
            if (!get_aim_dir(&dir)) return NULL;
            fire_ball(GF_POIS, dir, _BOOST(dam), 2);
            device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_BALL_LITE:
    {
        int dam = _extra(effect, 100 + 4*p_ptr->lev);
        if (name) return "Star Burst";
        if (desc) return "It fires a huge ball of powerful light.";
        if (info) return info_damage(0, 0, _BOOST(dam));
        if (value) return format("%d", 10*_extra(effect, 150));
        if (cast)
        {
            if (!get_aim_dir(&dir)) return NULL;
            fire_ball(GF_LITE, dir, _BOOST(dam), 4);
            device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_BALL_DARK:
    {
        int dam = _extra(effect, 100 + 4*p_ptr->lev);
        if (name) return "Darkness Storm";
        if (desc) return "It fires a huge ball of darkness.";
        if (info) return info_damage(0, 0, _BOOST(dam));
        if (value) return format("%d", 10*_extra(effect, 150));
        if (cast)
        {
            if (!get_aim_dir(&dir)) return NULL;
            fire_ball(GF_DARK, dir, _BOOST(dam), 4);
            device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_BALL_CONF:
    {
        int dam = _extra(effect, 30 + p_ptr->lev);
        if (name) return "Confusion Ball";
        if (desc) return "It fires a ball of confusion.";
        if (info) return info_damage(0, 0, _BOOST(dam));
        if (value) return format("%d", 10*_extra(effect, 80));
        if (cast)
        {
            if (!get_aim_dir(&dir)) return NULL;
            fire_ball(GF_NETHER, dir, _BOOST(dam), 3);
            device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_BALL_NETHER:
    {
        int dam = _extra(effect, 100 + p_ptr->lev);
        if (name) return "Nether Ball";
        if (desc) return "It fires a ball of nether.";
        if (info) return info_damage(0, 0, _BOOST(dam));
        if (value) return format("%d", 10*_extra(effect, 150));
        if (cast)
        {
            if (!get_aim_dir(&dir)) return NULL;
            fire_ball(GF_NETHER, dir, _BOOST(dam), 3);
            device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_BALL_NEXUS:
    {
        int dam = _extra(effect, 60 + p_ptr->lev);
        if (name) return "Nexus Ball";
        if (desc) return "It fires a ball of nexus.";
        if (info) return info_damage(0, 0, _BOOST(dam));
        if (value) return format("%d", 10*_extra(effect, 110));
        if (cast)
        {
            if (!get_aim_dir(&dir)) return NULL;
            fire_ball(GF_NEXUS, dir, _BOOST(dam), 3);
            device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_BALL_SOUND:
    {
        int dam = _extra(effect, 80 + 3*p_ptr->lev/2);
        if (name) return "Sound Ball";
        if (desc) return "It fires a ball of sound.";
        if (info) return info_damage(0, 0, _BOOST(dam));
        if (value) return format("%d", 10*_extra(effect, 130));
        if (cast)
        {
            if (!get_aim_dir(&dir)) return NULL;
            fire_ball(GF_SOUND, dir, _BOOST(dam), 3);
            device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_BALL_SHARDS:
    {
        int dam = _extra(effect, 100 + 2*p_ptr->lev);
        if (name) return "Shard Ball";
        if (desc) return "It fires a ball of shards.";
        if (info) return info_damage(0, 0, _BOOST(dam));
        if (value) return format("%d", 10*_extra(effect, 150));
        if (cast)
        {
            if (!get_aim_dir(&dir)) return NULL;
            fire_ball(GF_SHARDS, dir, _BOOST(dam), 2);
            device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_BALL_CHAOS:
    {
        int dam = _extra(effect, 100 + 3*p_ptr->lev);
        if (name) return "Invoke Logrus";
        if (desc) return "It fires a huge ball of chaos.";
        if (info) return info_damage(0, 0, _BOOST(dam));
        if (value) return format("%d", 500 + 20*_extra(effect, 100));
        if (cast)
        {
            if (!get_aim_dir(&dir)) return NULL;
            fire_ball(GF_CHAOS, dir, _BOOST(dam), 5);
            device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_BALL_DISEN:
    {
        int dam = _extra(effect, 90 + p_ptr->lev);
        if (name) return "Disenchantment Ball";
        if (desc) return "It fires a ball of disenchantment.";
        if (info) return info_damage(0, 0, _BOOST(dam));
        if (value) return format("%d", 10*_extra(effect, 140));
        if (cast)
        {
            if (!get_aim_dir(&dir)) return NULL;
            fire_ball(GF_DISENCHANT, dir, _BOOST(dam), 3);
            device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_BALL_TIME:
    {
        int dam = _extra(effect, 50 + p_ptr->lev);
        if (name) return "Time Ball";
        if (desc) return "It fires a ball of time.";
        if (info) return info_damage(0, 0, _BOOST(dam));
        if (value) return format("%d", 25*_extra(effect, 100));
        if (cast)
        {
            if (!get_aim_dir(&dir)) return NULL;
            fire_ball(GF_TIME, dir, _BOOST(dam), 3);
            device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_BALL_WATER:
    {
        int dam = _extra(effect, 100 + 3*p_ptr->lev);
        if (name) return "Whirlpool";
        if (desc) return "It fires a huge ball of water.";
        if (info) return info_damage(0, 0, _BOOST(dam));
        if (value) return format("%d", 15*_extra(effect, 100));
        if (cast)
        {
            if (!get_aim_dir(&dir)) return NULL;
            fire_ball(GF_WATER, dir, _BOOST(dam), 4);
            device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_BALL_MANA:
    {
        int dam = _extra(effect, 100 + 5*p_ptr->lev);
        if (name) return "Mana Ball";
        if (desc) return "It fires a powerful ball of mana.";
        if (info) return info_damage(0, 0, _BOOST(dam));
        if (value) return format("%d", 20*_extra(effect, 100));
        if (cast)
        {
            if (device_known && !get_aim_dir(&dir)) return NULL;
            fire_ball(GF_MANA, dir, _BOOST(dam), 2);
            device_noticed = TRUE;
        }
        break;
    }

    /* Offense: Breaths */
    case EFFECT_BREATHE_ACID:
    {
        int dam = _extra(effect, p_ptr->lev*4/5 + p_ptr->chp/3);
        if (name) return "Breathe Acid";
        if (desc) return "It allows you to breathe acid.";
        if (info) return info_damage(0, 0, _BOOST(dam));
        if (value) return format("%d", 50*_extra(effect, 100));
        if (cast)
        {
            if (!get_aim_dir(&dir)) return NULL;
            stop_mouth();
            fire_ball(GF_ACID, dir, _BOOST(dam), -2);
            device_noticed = TRUE;
        }
        break;
    }           
    case EFFECT_BREATHE_ELEC:
    {
        int dam = _extra(effect, p_ptr->lev*2/5 + p_ptr->chp/3);
        if (name) return "Breathe Lightning";
        if (desc) return "It allows you to breathe lightning.";
        if (info) return info_damage(0, 0, _BOOST(dam));
        if (value) return format("%d", 50*_extra(effect, 100));
        if (cast)
        {
            if (!get_aim_dir(&dir)) return NULL;
            stop_mouth();
            fire_ball(GF_ELEC, dir, _BOOST(dam), -2);
            device_noticed = TRUE;
        }
        break;
    }           
    case EFFECT_BREATHE_FIRE:
    {
        int dam = _extra(effect, p_ptr->lev + p_ptr->chp/3);
        if (name) return "Breathe Fire";
        if (desc) return "It allows you to breathe fire.";
        if (info) return info_damage(0, 0, _BOOST(dam));
        if (value) return format("%d", 50*_extra(effect, 100));
        if (cast)
        {
            if (!get_aim_dir(&dir)) return NULL;
            stop_mouth();
            fire_ball(GF_FIRE, dir, _BOOST(dam), -2);
            device_noticed = TRUE;
        }
        break;
    }           
    case EFFECT_BREATHE_COLD:
    {
        int dam = _extra(effect, p_ptr->lev/2 + p_ptr->chp/3);
        if (name) return "Breathe Frost";
        if (desc) return "It allows you to breathe frost.";
        if (info) return info_damage(0, 0, _BOOST(dam));
        if (value) return format("%d", 50*_extra(effect, 100));
        if (cast)
        {
            if (!get_aim_dir(&dir)) return NULL;
            stop_mouth();
            fire_ball(GF_COLD, dir, _BOOST(dam), -2);
            device_noticed = TRUE;
        }
        break;
    }           
    case EFFECT_BREATHE_POIS:
    {
        int dam = _extra(effect, p_ptr->lev*3/5 + p_ptr->chp/3);
        if (name) return "Breathe Poison";
        if (desc) return "It allows you to breathe poison.";
        if (info) return info_damage(0, 0, _BOOST(dam));
        if (value) return format("%d", 50*_extra(effect, 100));
        if (cast)
        {
            if (!get_aim_dir(&dir)) return NULL;
            stop_mouth();
            fire_ball(GF_POIS, dir, _BOOST(dam), -2);
            device_noticed = TRUE;
        }
        break;
    }           
    case EFFECT_BREATHE_LITE:
    {
        int dam = _extra(effect, p_ptr->lev/2 + p_ptr->chp/5);
        if (name) return "Breathe Light";
        if (desc) return "It allows you to breathe light.";
        if (info) return info_damage(0, 0, _BOOST(dam));
        if (value) return format("%d", 60*_extra(effect, 100));
        if (cast)
        {
            if (!get_aim_dir(&dir)) return NULL;
            stop_mouth();
            fire_ball(GF_LITE, dir, _BOOST(dam), -2);
            device_noticed = TRUE;
        }
        break;
    }           
    case EFFECT_BREATHE_DARK:
    {
        int dam = _extra(effect, p_ptr->lev/2 + p_ptr->chp/5);
        if (name) return "Breathe Darkness";
        if (desc) return "It allows you to breathe darkness.";
        if (info) return info_damage(0, 0, _BOOST(dam));
        if (value) return format("%d", 60*_extra(effect, 100));
        if (cast)
        {
            if (!get_aim_dir(&dir)) return NULL;
            stop_mouth();
            fire_ball(GF_DARK, dir, _BOOST(dam), -2);
            device_noticed = TRUE;
        }
        break;
    }           
    case EFFECT_BREATHE_CONF:
    {
        int dam = _extra(effect, p_ptr->lev/2 + p_ptr->chp/5);
        if (name) return "Breathe Confusion";
        if (desc) return "It allows you to breathe confusion.";
        if (info) return info_damage(0, 0, _BOOST(dam));
        if (value) return format("%d", 60*_extra(effect, 100));
        if (cast)
        {
            if (!get_aim_dir(&dir)) return NULL;
            stop_mouth();
            fire_ball(GF_CONFUSION, dir, _BOOST(dam), -2);
            device_noticed = TRUE;
        }
        break;
    }           
    case EFFECT_BREATHE_NETHER:
    {
        int dam = _extra(effect, p_ptr->lev + p_ptr->chp/3);
        if (name) return "Breathe Nether";
        if (desc) return "It allows you to breathe nether.";
        if (info) return info_damage(0, 0, _BOOST(dam));
        if (value) return format("%d", 60*_extra(effect, 100));
        if (cast)
        {
            if (!get_aim_dir(&dir)) return NULL;
            stop_mouth();
            fire_ball(GF_NETHER, dir, _BOOST(dam), -2);
            device_noticed = TRUE;
        }
        break;
    }           
    case EFFECT_BREATHE_NEXUS:
    {
        int dam = _extra(effect, p_ptr->lev/2 + p_ptr->chp/5);
        if (name) return "Breathe Nexus";
        if (desc) return "It allows you to breathe nexus.";
        if (info) return info_damage(0, 0, _BOOST(dam));
        if (value) return format("%d", 60*_extra(effect, 100));
        if (cast)
        {
            if (!get_aim_dir(&dir)) return NULL;
            stop_mouth();
            fire_ball(GF_NEXUS, dir, _BOOST(dam), -2);
            device_noticed = TRUE;
        }
        break;
    }           
    case EFFECT_BREATHE_SOUND:
    {
        int dam = _extra(effect, p_ptr->lev/2 + p_ptr->chp/5);
        if (name) return "Breathe Sound";
        if (desc) return "It allows you to breathe sound.";
        if (info) return info_damage(0, 0, _BOOST(dam));
        if (value) return format("%d", 60*_extra(effect, 100));
        if (cast)
        {
            if (!get_aim_dir(&dir)) return NULL;
            stop_mouth();
            fire_ball(GF_SOUND, dir, _BOOST(dam), -2);
            device_noticed = TRUE;
        }
        break;
    }           
    case EFFECT_BREATHE_SHARDS:
    {
        int dam = _extra(effect, p_ptr->lev + p_ptr->chp/3);
        if (name) return "Breathe Shards";
        if (desc) return "It allows you to breathe shards.";
        if (info) return info_damage(0, 0, _BOOST(dam));
        if (value) return format("%d", 70*_extra(effect, 100));
        if (cast)
        {
            if (!get_aim_dir(&dir)) return NULL;
            stop_mouth();
            fire_ball(GF_SHARDS, dir, _BOOST(dam), -2);
            device_noticed = TRUE;
        }
        break;
    }           
    case EFFECT_BREATHE_CHAOS:
    {
        int dam = _extra(effect, p_ptr->lev + p_ptr->chp/4);
        if (name) return "Breathe Chaos";
        if (desc) return "It allows you to breathe chaos.";
        if (info) return info_damage(0, 0, _BOOST(dam));
        if (value) return format("%d", 70*_extra(effect, 100));
        if (cast)
        {
            if (!get_aim_dir(&dir)) return NULL;
            stop_mouth();
            fire_ball(GF_CHAOS, dir, _BOOST(dam), -2);
            device_noticed = TRUE;
        }
        break;
    }           
    case EFFECT_BREATHE_DISEN:
    {
        int dam = _extra(effect, p_ptr->lev + p_ptr->chp/4);
        if (name) return "Breathe Disenchantment";
        if (desc) return "It allows you to breathe disenchantment.";
        if (info) return info_damage(0, 0, _BOOST(dam));
        if (value) return format("%d", 70*_extra(effect, 100));
        if (cast)
        {
            if (!get_aim_dir(&dir)) return NULL;
            stop_mouth();
            fire_ball(GF_DISENCHANT, dir, _BOOST(dam), -2);
            device_noticed = TRUE;
        }
        break;
    }           
    case EFFECT_BREATHE_TIME:
    {
        int dam = _extra(effect, p_ptr->lev/3 + p_ptr->chp/6);
        if (name) return "Breathe Time";
        if (desc) return "It allows you to breathe time.";
        if (info) return info_damage(0, 0, _BOOST(dam));
        if (value) return format("%d", 70*_extra(effect, 100));
        if (cast)
        {
            if (!get_aim_dir(&dir)) return NULL;
            stop_mouth();
            fire_ball(GF_TIME, dir, _BOOST(dam), -2);
            device_noticed = TRUE;
        }
        break;
    }           
    case EFFECT_BREATHE_ONE_MULTIHUED:
    {
        int dam = _extra(effect, p_ptr->chp/3);
        if (name) return "Breathe";
        if (desc) return "It allows you to breathe one of acid, lightning, fire, frost or poison.";
        if (info) return info_damage(0, 0, _BOOST(dam));
        if (value) return format("%d", 50*_extra(effect, 100));
        if (cast)
        {
            struct { int  type; cptr desc; } _choices[5] = {
                { GF_ACID, "acid"},
                { GF_ELEC, "lightning"},
                { GF_FIRE, "fire"},
                { GF_COLD, "frost"},
                { GF_POIS, "poison"},
            };
            int which = randint0(5);

            if (!get_aim_dir(&dir)) return NULL;
            stop_mouth();
            msg_format("You breathe %s.", _choices[which].desc);
            fire_ball(_choices[which].type, dir, _BOOST(dam), -2);
            device_noticed = TRUE;
        }
        break;
    }           
    case EFFECT_BREATHE_ONE_CHAOS:
    {
        int dam = _extra(effect, p_ptr->chp/4);
        if (name) return "Breathe";
        if (desc) return "It allows you to breathe one of chaos or disenchantment.";
        if (info) return info_damage(0, 0, _BOOST(dam));
        if (value) return format("%d", 60*_extra(effect, 100));
        if (cast)
        {
            struct { int  type; cptr desc; } _choices[2] = {
                { GF_CHAOS, "chaos"},
                { GF_DISENCHANT, "disenchantment"},
            };
            int which = randint0(2);

            if (!get_aim_dir(&dir)) return NULL;
            stop_mouth();
            msg_format("You breathe %s.", _choices[which].desc);
            fire_ball(_choices[which].type, dir, _BOOST(dam), -2);
            device_noticed = TRUE;
        }
        break;
    }           
    case EFFECT_BREATHE_ONE_LAW:
    {
        int dam = _extra(effect, p_ptr->chp/4);
        if (name) return "Breathe";
        if (desc) return "It allows you to breathe one of sound or shards.";
        if (info) return info_damage(0, 0, _BOOST(dam));
        if (value) return format("%d", 60*_extra(effect, 100));
        if (cast)
        {
            struct { int  type; cptr desc; } _choices[2] = {
                { GF_SOUND, "sound"},
                { GF_SHARDS, "shards"},
            };
            int which = randint0(2);

            if (!get_aim_dir(&dir)) return NULL;
            stop_mouth();
            msg_format("You breathe %s.", _choices[which].desc);
            fire_ball(_choices[which].type, dir, _BOOST(dam), -2);
            device_noticed = TRUE;
        }
        break;
    }           
    case EFFECT_BREATHE_ONE_BALANCE:
    {
        int dam = _extra(effect, p_ptr->chp/4);
        if (name) return "Breathe";
        if (desc) return "It allows you to breathe one of sound, shards, chaos or disenchantment.";
        if (info) return info_damage(0, 0, _BOOST(dam));
        if (value) return format("%d", 60*_extra(effect, 100));
        if (cast)
        {
            struct { int  type; cptr desc; } _choices[4] = {
                { GF_SOUND, "sound"},
                { GF_SHARDS, "shards"},
                { GF_CHAOS, "chaos"},
                { GF_DISENCHANT, "disenchantment"},
            };
            int which = randint0(4);

            if (!get_aim_dir(&dir)) return NULL;
            stop_mouth();
            msg_format("You breathe %s.", _choices[which].desc);
            fire_ball(_choices[which].type, dir, _BOOST(dam), -2);
            device_noticed = TRUE;
        }
        break;
    }           
    case EFFECT_BREATHE_ONE_SHINING:
    {
        int dam = _extra(effect, p_ptr->chp/5);
        if (name) return "Breathe";
        if (desc) return "It allows you to breathe one of light or darkness.";
        if (info) return info_damage(0, 0, _BOOST(dam));
        if (value) return format("%d", 60*_extra(effect, 100));
        if (cast)
        {
            struct { int  type; cptr desc; } _choices[2] = {
                { GF_LITE, "light"},
                { GF_DARK, "darkness"},
            };
            int which = randint0(2);

            if (!get_aim_dir(&dir)) return NULL;
            stop_mouth();
            msg_format("You breathe %s.", _choices[which].desc);
            fire_ball(_choices[which].type, dir, _BOOST(dam), -2);
            device_noticed = TRUE;
        }
        break;
    }           
    case EFFECT_BREATHE_ELEMENTS:
    {
        int dam = _extra(effect, p_ptr->chp/3);
        if (name) return "Breathe Elements";
        if (desc) return "It allows you to breathe the elements.";
        if (info) return info_damage(0, 0, _BOOST(dam));
        if (value) return format("%d", 100*_extra(effect, 100));
        if (cast)
        {
            if (!get_aim_dir(&dir)) return NULL;
            stop_mouth();
            fire_ball(GF_MISSILE, dir, _BOOST(dam), -2);
            device_noticed = TRUE;
        }
        break;
    }           

    /* Offense: Other */
    case EFFECT_DISPEL_EVIL:
    {
        int dam = _extra(effect, 100);
        if (name) return "Dispel Evil";
        if (desc) return "It damages all evil monsters in sight.";
        if (info) return info_damage(0, 0, _BOOST(dam));
        if (value) return format("%d", 20*dam);
        if (cast)
        {
            if (dispel_evil(_BOOST(dam))) 
                device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_DISPEL_EVIL_HERO:
    {
        int dam = _extra(effect, 100);
        if (name) return "Dispel Evil";
        if (desc) return "It damages all evil monsters in sight and grants temporary heroism.";
        if (info) return info_damage(0, 0, _BOOST(dam));
        if (value) return format("%d", 500 + 20*dam);
        if (cast)
        {
            if (dispel_evil(_BOOST(dam))) 
                device_noticed = TRUE;
            if (set_hero(_BOOST(25 + randint1(25)), FALSE))
                device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_DISPEL_GOOD:
    {
        int dam = _extra(effect, 100);
        if (name) return "Dispel Good";
        if (desc) return "It damages all good monsters in sight.";
        if (info) return info_damage(0, 0, _BOOST(dam));
        if (value) return format("%d", 20*dam);
        if (cast)
        {
            if (dispel_good(_BOOST(dam))) 
                device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_DISPEL_LIFE:
    {
        int dam = _extra(effect, 100);
        if (name) return "Dispel Life";
        if (desc) return "It damages all living monsters in sight.";
        if (info) return info_damage(0, 0, _BOOST(dam));
        if (value) return format("%d", 20*dam);
        if (cast)
        {
            if (dispel_living(_BOOST(dam))) 
                device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_DISPEL_DEMON:
    {
        int dam = _extra(effect, 100);
        if (name) return "Dispel Demons";
        if (desc) return "It damages all demonic monsters in sight.";
        if (info) return info_damage(0, 0, _BOOST(dam));
        if (value) return format("%d", 20*dam);
        if (cast)
        {
            if (dispel_demons(_BOOST(dam))) 
                device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_DISPEL_UNDEAD:
    {
        int dam = _extra(effect, 100);
        if (name) return "Dispel Undead";
        if (desc) return "It damages all undead monsters in sight.";
        if (info) return info_damage(0, 0, _BOOST(dam));
        if (value) return format("%d", 20*dam);
        if (cast)
        {
            if (dispel_undead(_BOOST(dam))) 
                device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_DISPEL_MONSTERS:
    {
        int dam = _extra(effect, 4); /* Default is low to mimic Faramir's activation for the Ring race */
        if (name) return "Dispel Monsters";
        if (desc) return "It damages all monsters in sight.";
        if (info) return info_damage(0, 0, _BOOST(dam));
        if (value) return format("%d", 20*dam);
        if (cast)
        {
            if (dispel_monsters(_BOOST(dam))) 
                device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_DRAIN_LIFE:
    {
        int dam = _extra(effect, 50 + p_ptr->lev/2);
        if (name) return "Drain Life";
        if (desc) return "It fires a bolt that steals life from a foe when you use it.";
        if (info) return info_damage(0, 0, _BOOST(dam));
        if (value) return format("%d", 20*_extra(effect, 75));
        if (cast)
        {
            if (!get_aim_dir(&dir)) return NULL;
            dam = _BOOST(dam);
            if (drain_life(dir, dam)) 
            {
                hp_player(dam);
                device_noticed = TRUE;
            }
        }
        break;
    }
    case EFFECT_STAR_BALL:
    {
        int dam = _extra(effect, 150);
        if (name) return "Star Ball";
        if (desc) return "It fires a multitude of lightning balls in random directions.";
        if (info) return info_damage(0, 0, _BOOST(dam));
        if (value) return format("%d", 50*dam);
        if (cast)
        {
            int num = _BOOST(damroll(5, 3));
            int y, x, i;
            int attempts;

            for (i = 0; i < num; i++)
            {
                attempts = 1000;
                while (attempts--)
                {
                    scatter(&y, &x, py, px, 4, 0);
                    if (!cave_have_flag_bold(y, x, FF_PROJECT)) continue;
                    if (!player_bold(y, x)) break;
                }
                project(0, 3, y, x, _BOOST(dam), GF_ELEC,
                    (PROJECT_THRU | PROJECT_STOP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL), -1);
            }
        }
        break;
    }
    case EFFECT_WRATH_OF_GOD:
    {
        int dam = _extra(effect, 25 + p_ptr->lev*3);
        if (name) return "Wrath of the God";
        if (desc) return "It drops many balls of disintegration near the target.";
        if (info) return info_damage(0, 0, _BOOST(dam));
        if (value) return format("%d", 50*dam);
        if (cast)
        {
            if (!cast_wrath_of_the_god(_BOOST(dam), 2)) return NULL;
        }
        break;
    }

    case EFFECT_ROCKET:
    {
        int dam = _extra(effect, 250 + p_ptr->lev*3);
        if (name) return "Rocket";
        if (desc) return "It fires a rocket.";
        if (info) return info_damage(0, 0, _BOOST(dam));
        if (value) return format("%d", 40*_extra(effect, 250));
        if (cast)
        {
            if (!get_aim_dir(&dir)) return NULL;
            fire_rocket(GF_ROCKET, dir, _BOOST(dam), 2);
            device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_MANA_STORM:
    {
        int dam = _extra(effect, 350);
        if (name) return "Mana Storm";
        if (desc) return "It produces a huge mana ball centered on you. If you are not magically inclined, you take damage as well.";
        if (info) return info_damage(1, _BOOST(200), _BOOST(dam));
        if (value) return format("%d", 2000 + 20*dam);
        if (cast)
        {
            msg_print("Mighty magics rend your enemies!");
            project(0, 5, py, px,
                _BOOST((randint1(200) + dam) * 2), 
                GF_MANA, PROJECT_KILL | PROJECT_ITEM | PROJECT_GRID, -1);
            if ( p_ptr->pclass != CLASS_MAGE
              && p_ptr->pclass != CLASS_HIGH_MAGE 
              && p_ptr->pclass != CLASS_SORCERER 
              && p_ptr->pclass != CLASS_DEVICEMASTER
              && p_ptr->pclass != CLASS_MAGIC_EATER 
              && p_ptr->pclass != CLASS_BLUE_MAGE 
              && p_ptr->pclass != CLASS_BLOOD_MAGE
              && p_ptr->prace != RACE_MON_RING )
            {
                take_hit(DAMAGE_NOESCAPE, 50, "unleashing magics too mighty to control", -1);
            }
            device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_CONFUSING_LITE:
    {
        int pow = _extra(effect, p_ptr->lev*4);
        if (name) return "Confusing Lights";
        if (desc) return "It emits dazzling lights which slow, stun, confuse, scare and even freeze nearby monsters.";
        if (info) return format("Power %d", pow);
        if (value) return format("%d", 10*_extra(effect, 200));
        if (cast)
        {
            msg_print("You glare nearby monsters with a dazzling array of confusing lights!");
            pow = _BOOST(pow);
            slow_monsters(pow);
            stun_monsters(pow);
            confuse_monsters(pow);
            turn_monsters(pow);
            stasis_monsters(pow);
            device_noticed = TRUE; /* You see the dazzling lights, no? */
        }
        break;
    }
    case EFFECT_ARROW:
    {
        int dam = _extra(effect, 150);
        if (name) return "Magic Arrow";
        if (desc) return "It fires a powerful magical arrow.";
        if (info) return info_damage(0, 0, _BOOST(dam));
        if (value) return format("%d", 20*dam);
        if (cast)
        {
            if (!get_aim_dir(&dir)) return NULL;
            fire_bolt(GF_ARROW, dir, _BOOST(dam));
            device_noticed = TRUE;
        }
        break;
    }

    /* Misc */
    case EFFECT_POLY_SELF:
        if (name) return "Polymorph";
        if (desc) return "It mutates you. Warning: You might not like the results!";
        if (value) return format("%d", 500);
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
        if (cast)
        {
            if (animate_dead(0, py, px))
                device_noticed = TRUE;
        }
        break;
    case EFFECT_SCARE_MONSTERS:
    {
        int pow = _extra(effect, p_ptr->lev*3);
        if (name) return "Terrify Monsters";
        if (desc) return "It attempts to frighten all nearby visible monsters.";
        if (info) return format("Power %d", pow);
        if (value) return format("%d", 5*_extra(effect, 150));
        if (cast)
        {
            if (turn_monsters(_BOOST(pow)))
                device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_SLEEP_MONSTERS:
    {
        int pow = _extra(effect, p_ptr->lev*3);
        if (name) return "Sleep Monsters";
        if (desc) return "It attempts to sleep all nearby visible monsters.";
        if (info) return format("Power %d", pow);
        if (value) return format("%d", 5*_extra(effect, 150));
        if (cast)
        {
            if (sleep_monsters(_BOOST(pow)))
                device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_SLOW_MONSTERS:
    {
        int pow = _extra(effect, p_ptr->lev*3);
        if (name) return "Slow Monsters";
        if (desc) return "It attempts to slow all nearby visible monsters.";
        if (info) return format("Power %d", pow);
        if (value) return format("%d", 5*_extra(effect, 150));
        if (cast)
        {
            if (slow_monsters(_BOOST(pow)))
                device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_STASIS_MONSTERS:
    {
        int pow = _extra(effect, p_ptr->lev*3);
        if (name) return "Freeze Monsters";
        if (desc) return "It attempts to freeze all nearby visible monsters.";
        if (info) return format("Power %d", pow);
        if (value) return format("%d", 15*_extra(effect, 150));
        if (cast)
        {
            if (stasis_monsters(_BOOST(pow)))
                device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_CONFUSE_MONSTERS:
    {
        int pow = _extra(effect, p_ptr->lev*3);
        if (name) return "Confuse Monsters";
        if (desc) return "It attempts to confuse all nearby visible monsters.";
        if (info) return format("Power %d", pow);
        if (value) return format("%d", 5*_extra(effect, 150));
        if (cast)
        {
            if (confuse_monsters(_BOOST(pow)))
                device_noticed = TRUE;
        }
        break;
    }
    case EFFECT_FISHING:
        if (name) return "Fishing";
        if (desc) return "It allows you to relax and unwind from the pressures of adventuring.";
        if (value) return format("%d", 100);
        if (cast)
        {
            int x, y;
            if (!get_rep_dir2(&dir)) return NULL;
            y = py+ddy[dir];
            x = px+ddx[dir];
            tsuri_dir = dir;
            if (!cave_have_flag_bold(y, x, FF_WATER))
            {
                msg_print("There is no fishing place.");
                break;
            }
            else if (cave[y][x].m_idx)
            {
                char m_name[MAX_NLEN];
                monster_desc(m_name, &m_list[cave[y][x].m_idx], 0);
                msg_format("%^s is standing in your way.", m_name);
                energy_use = 0;
                break;
            }
            set_action(ACTION_FISH);
            p_ptr->redraw |= (PR_STATE);
            device_noticed = TRUE;
        }
        break;
    case EFFECT_CHARGE:
        if (name) return "Charge";
        if (desc) return "If riding, you charge a chosen foe doing extra damage.";
        if (value) return format("%d", 5000);
        if (cast)
        {
            bool charged = FALSE;
            /* For the lance activation, the player really should be riding.
               At the moment, only the Heavy Lance 'Impaler' has this effect. */
            if (!p_ptr->riding)
            {
                msg_print("You need to be mounted in order to charge.");
                return NULL;
            }
            p_ptr->weapon_info[0].to_d += 2;
            charged = rush_attack(7, NULL);
            p_ptr->weapon_info[0].to_d -= 2;
            if (!charged) return NULL;
        }
        break;
    case EFFECT_AGGRAVATE:
        if (name) return "Aggravate Monsters";
        if (desc) return "It aggravates nearby monsters.";
        if (value) return format("%d", 100); /* This actually *can* be useful ... */
        if (cast)
        {
            aggravate_monsters(0);
            device_known = TRUE;
        }
        break;
    case EFFECT_PIERCING_SHOT:
        if (name) return "Piercing Shot";
        if (desc) return "It shoots a bolt through multiple foes.";
        if (value) return format("%d", 1500);
        if (cast)
        {
            bool fired = FALSE;
            msg_print("");
            shoot_hack = SHOOT_PIERCE;
            fired = do_cmd_fire();
            shoot_hack = SHOOT_NONE;
            if (!fired) return NULL;
            device_known = TRUE;
        }
        break;

    /* Specific Artifacts ... Try to minimize! */
    case EFFECT_JEWEL:
        if (name) return "Clairvoyance and Recall";
        if (desc) return "It maps, lights permanently and detects all items on the entire level.";
        if (value) return format("%d", 10000);
        if (cast)
        {
            virtue_add(VIRTUE_KNOWLEDGE, 1);
            virtue_add(VIRTUE_ENLIGHTENMENT, 1);
            wiz_lite(p_ptr->tim_superstealth > 0);
            msg_print("The Jewel drains your vitality...");
            take_hit(DAMAGE_LOSELIFE, damroll(3, 8), "the Jewel of Judgement", -1);
            detect_traps(DETECT_RAD_DEFAULT, TRUE);
            detect_doors(DETECT_RAD_DEFAULT);
            detect_stairs(DETECT_RAD_DEFAULT);
            if (get_check("Activate recall? "))
                word_of_recall();
            device_noticed = TRUE;
        }
        break;
    case EFFECT_HERMES:
        if (name) return "Haste and Dimension Door";
        if (desc) return "It hastes you and teleports you to a chosen nearby location.";
        if (value) return format("%d", 15000);
        if (cast)
        {
            if (set_fast(_BOOST(randint1(75) + 75), FALSE)) device_noticed = TRUE;
            if (dimension_door(_BOOST(p_ptr->lev / 2 + 10))) device_noticed = TRUE;
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
            int slot;

            object_prep(&forge, lookup_kind(TV_ARROW, m_bonus(1, p_ptr->lev)+ 1));
            forge.number = (byte)rand_range(5, 10);
            object_aware(&forge);
            object_known(&forge);
            apply_magic(&forge, p_ptr->lev, AM_NO_FIXED_ART);

            forge.discount = 99;

            object_desc(o_name, &forge, 0);
            msg_format("It creates %s.", o_name);

            slot = inven_carry(&forge);
            if (slot >= 0) autopick_alter_item(slot, FALSE);

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
            take_hit(DAMAGE_LOSELIFE, damroll(8, 8), "the Eye of Vecna", -1);
            wiz_lite(TRUE);
            device_noticed = TRUE;
        }
        break;
    case EFFECT_ONE_RING:
        if (name) return "Something Weird";
        if (desc) return "It does something completely unpredictable and probably rather bad.";
        if (value) return format("%d", 1000);
        if (cast)
        {
            if (!get_aim_dir(&dir)) return NULL;
            ring_of_power(dir);
            device_noticed = TRUE;
        }
        break;
    case EFFECT_BLADETURNER:
        if (name) return "Heroism, Resistance and Breathe Elements";
        if (desc) return "It grants temporary heroism, blessing and elemental resistance and also allows you to breathe the elements.";
        if (value) return format("%d", 10000);
        if (cast)
        {
            if (!get_aim_dir(&dir)) return NULL;
            msg_print("You breathe the elements.");

            fire_ball(GF_MISSILE, dir, _BOOST(300), 4);
            device_noticed = TRUE;

            msg_print("Your armor glows many colours...");

            set_hero(_BOOST(randint1(50) + 50), FALSE);
            set_blessed(_BOOST(randint1(50) + 50), FALSE);
            set_oppose_acid(_BOOST(randint1(50) + 50), FALSE);
            set_oppose_elec(_BOOST(randint1(50) + 50), FALSE);
            set_oppose_fire(_BOOST(randint1(50) + 50), FALSE);
            set_oppose_cold(_BOOST(randint1(50) + 50), FALSE);
            set_oppose_pois(_BOOST(randint1(50) + 50), FALSE);
        }
        break;
    case EFFECT_MITO_KOUMON:
        if (name) return "Reveal Identity";
        if (desc) return "It reveals your true identity.";
        if (value) return format("%d", 1000);
        if (cast)
        {
            int count = 0, i;
            monster_type *m_ptr;
            cptr kakusan = "";

            if (summon_named_creature(0, py, px, MON_SUKE, PM_FORCE_PET))
            {
                msg_print("Suke-san appears.");
                kakusan = "Suke-san";
                count++;
            }
            if (summon_named_creature(0, py, px, MON_KAKU, PM_FORCE_PET))
            {
                msg_print("Kaku-san appears.");
                kakusan = "Kaku-san";
                count++;
            }
            if (!count)
            {
                for (i = m_max - 1; i > 0; i--)
                {
                    m_ptr = &m_list[i];
                    if (!m_ptr->r_idx) continue;
                    if (!((m_ptr->r_idx == MON_SUKE) || (m_ptr->r_idx == MON_KAKU))) continue;
                    if (!los(m_ptr->fy, m_ptr->fx, py, px)) continue;
                    if (!projectable(m_ptr->fy, m_ptr->fx, py, px)) continue;
                    count++;
                    break;
                }
            }
            if (count)
            {
                msg_format("%^s says 'WHO do you think this person is! Bow your head, down your knees!'", kakusan);

                sukekaku = TRUE;
                stun_monsters(120);
                confuse_monsters(120);
                turn_monsters(120);
                stasis_monsters(120);
                sukekaku = FALSE;
                device_noticed = TRUE;
            }
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
            int slot = equip_find_artifact(ART_BLOOD);
            if (slot)
            {
                get_bloody_moon_flags(equip_obj(slot));
                if (p_ptr->prace == RACE_ANDROID) calc_android_exp();
                p_ptr->update |= (PU_BONUS | PU_HP);
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
    {
        int dam = _extra(effect, 3*p_ptr->lev);
        if (name) return "Bang a Gong";
        if (desc) return "It makes some very loud noise.";
        if (info) return info_damage(0, 0, _BOOST(dam));
        if (value) return format("%d", 100*_extra(effect, 150));
        if (cast)
        {
            if (!res_save_default(RES_SOUND))
                project(-1, 0, py, px, _BOOST(dam), GF_SOUND, PROJECT_KILL | PROJECT_HIDE, -1);
            project(0, 18, py, px, _BOOST(dam*2), GF_SOUND, PROJECT_KILL | PROJECT_ITEM, -1);
            device_noticed = TRUE;
        }
        break;
    }           
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
                    int slot = equip_find_artifact(ART_MURAMASA);
                    if (slot)
                    {
                        msg_print("The Muramasa is destroyed!");
                        curse_weapon(TRUE, slot);
                    }
                }
            }
        }
        break;
    }
    return "";
}
#undef _BOOST

