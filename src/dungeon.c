/* File: dungeonc */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies. Other copyrights may also apply.
 */

/* Purpose: Angband game engine */

#include "angband.h"
#include <assert.h>

#define TY_CURSE_CHANCE 200
#define CHAINSWORD_NOISE 100

static bool load = TRUE;
static int wild_regen = 20;

/*
 * Return a "feeling" (or NULL) about an item. Method 1 (Heavy).
 *
 * For strong sensing, we have now have (3.0.3 and later):
 *
 *                    egos         artifacts
 *                    =========    =========
 * average -> good -> excellent -> special
 *         -> bad  -> awful     -> terrible
 */
byte value_check_aux1(object_type *o_ptr)
{
    /* Artifacts */
    if (obj_is_art(o_ptr))
    {
        /* Cursed/Broken */
        if (obj_is_cursed(o_ptr) || obj_is_broken(o_ptr)) return FEEL_TERRIBLE;

        /* Normal */
        return FEEL_SPECIAL;
    }

    /* Ego-Items */
    if (obj_is_ego(o_ptr))
    {
        /* Cursed/Broken */
        if ((obj_is_cursed(o_ptr) || obj_is_broken(o_ptr)) && !obj_is_device(o_ptr)) return FEEL_AWFUL;

        /* Normal */
        return FEEL_EXCELLENT;
    }

    /* Cursed items */
    if (obj_is_cursed(o_ptr) && !obj_is_device(o_ptr)) return FEEL_BAD;

    /* Broken items */
    if (obj_is_broken(o_ptr)) return FEEL_BROKEN;

    if (o_ptr->tval == TV_RING || o_ptr->tval == TV_AMULET) return FEEL_AVERAGE;

    /* Good "armor" bonus */
    if (o_ptr->to_a > 0) return FEEL_GOOD;

    /* Good "weapon" bonus */
    if (o_ptr->to_h + o_ptr->to_d > 0) return FEEL_GOOD;

    /* Default to "average" */
    return FEEL_AVERAGE;
}


/*
 * Return a "feeling" (or NULL) about an item. Method 2 (Light).
 *
 * For weak sensing, we have:
 *
 * average -> enchanted
 *         -> cursed
 */
static byte value_check_aux2(object_type *o_ptr)
{
    /* Cursed items (all of them) */
    if (obj_is_cursed(o_ptr) && !obj_is_device(o_ptr)) return FEEL_CURSED;

    /* Broken items (all of them) */
    if (obj_is_broken(o_ptr)) return FEEL_BROKEN;

    /* Artifacts -- except cursed/broken ones */
    if (obj_is_art(o_ptr)) return FEEL_ENCHANTED;

    /* Ego-Items -- except cursed/broken ones */
    if (obj_is_ego(o_ptr)) return FEEL_ENCHANTED;

    /* Good armor bonus */
    if (o_ptr->to_a > 0) return FEEL_ENCHANTED;

    /* Good weapon bonuses */
    if (o_ptr->to_h + o_ptr->to_d > 0) return FEEL_ENCHANTED;

    return FEEL_AVERAGE;
}

static bool _sense_strong = FALSE;

static void _sense_obj(obj_ptr obj)
{
    byte feel;
    char name[MAX_NLEN];
    bool strong = _sense_strong;

    if (obj->ident & IDENT_SENSE) return;
    if (obj_is_known(obj)) return;
    if (obj->loc.where == INV_PACK && !one_in_(3)) return;

    if (!strong && p_ptr->good_luck && !randint0(13))
        strong = TRUE;
    feel = strong ? value_check_aux1(obj) : value_check_aux2(obj);
    if (!feel) return;

    /*if (disturb_minor) disturb(0, 0);*/

    object_desc(name, obj, OD_OMIT_PREFIX | OD_NAME_ONLY | OD_COLOR_CODED);
    msg_boundary();
    if (obj->loc.where == INV_EQUIP)
    {
        msg_format("You feel the %s (%c) you are wearing %s %s...",
               name, slot_label(obj->loc.v.slot),
               obj->number == 1 ? "is" : "are",
                   game_inscriptions[feel]);
    }
    else
    {
        msg_format("You feel the %s (%c) in your %s %s %s...",
               name, slot_label(obj->loc.v.slot),
               obj->loc.where == INV_QUIVER ? "quiver" : "pack", 
               obj->number == 1 ? "is" : "are",
                   game_inscriptions[feel]);
    }

    obj->ident |= IDENT_SENSE;
    obj->feeling = feel;

    autopick_alter_obj(obj, destroy_feeling && obj->loc.where != INV_EQUIP);
    obj_release(obj, OBJ_RELEASE_ID | OBJ_RELEASE_QUIET);
}

/*
 * Sense the inventory
 */
static int _adj_pseudo_id(int num)
{
    int result = num * adj_pseudo_id[p_ptr->stat_ind[A_WIS]] / 100;
    int lev = p_ptr->lev;

    result = result * (625 - virtue_current(VIRTUE_KNOWLEDGE)) / 625;

    /* Hack: Pseudo-id becomes instantaneous at CL35 */
    if (lev >= 35) return 0;
    for (;;)
    {
        lev -= 5;
        if (lev < 0) break;
        result /= 2;
    }
    return result;
}

static int _get_pseudo_id_flags(void)
{
    if (p_ptr->pclass == CLASS_MONSTER)
    {
        race_t *race_ptr = get_race();
        return get_class_aux(race_ptr->pseudo_class_idx, 0)->flags;
    }
    return get_class()->flags;
}

static void sense_inventory1(void)
{
    int  plev = p_ptr->lev + 10;
    bool strong = FALSE;
    int  flags = _get_pseudo_id_flags();

    if (plr_tim_find(T_CONFUSED)) return;

    if (flags & CLASS_SENSE1_STRONG)
        strong = TRUE;
    else if (!(flags & CLASS_SENSE1_WEAK))
        return;
    if (flags & CLASS_SENSE1_FAST)
    {
        if (0 != randint0(_adj_pseudo_id(9000) / (plev * plev + 40)))
            return;
    }
    else if (flags & CLASS_SENSE1_MED)
    {
        if (0 != randint0(_adj_pseudo_id(20000) / (plev * plev + 40)))
            return;
    }
    else if (flags & CLASS_SENSE1_SLOW)
    {
        if (0 != randint0(_adj_pseudo_id(80000) / (plev * plev + 40)))
            return;
    }
    if (virtue_current(VIRTUE_KNOWLEDGE) >= 100)
        strong = TRUE;

    /*** Sense everything ***/
    _sense_strong = strong;
    pack_for_each_that(_sense_obj, obj_can_sense1);
    equip_for_each_that(_sense_obj, obj_can_sense1);
    quiver_for_each_that(_sense_obj, obj_can_sense1);
}


static void sense_inventory2(void)
{
    int  plev = p_ptr->lev + 10;
    bool strong = FALSE;
    int  flags = _get_pseudo_id_flags();

    if (plr_tim_find(T_CONFUSED)) return;

    if (flags & CLASS_SENSE2_STRONG)
        strong = TRUE;
    else if (!(flags & CLASS_SENSE2_WEAK))
        return;
    if (flags & CLASS_SENSE2_FAST)
    {
        if (0 != randint0(_adj_pseudo_id(9000) / (plev * plev + 40)))
            return;
    }
    else if (flags & CLASS_SENSE2_MED)
    {
        if (0 != randint0(_adj_pseudo_id(20000) / (plev * plev + 40)))
            return;
    }
    else if (flags & CLASS_SENSE2_SLOW)
    {
        if (0 != randint0(_adj_pseudo_id(80000) / (plev * plev + 40)))
            return;
    }
    else /* Super duper slow */
    {
        if (0 != randint0(_adj_pseudo_id(240000) / (plev + 5)))
            return;
    }

    /*** Sense everything ***/
    _sense_strong = strong;
    pack_for_each_that(_sense_obj, obj_can_sense2);
    equip_for_each_that(_sense_obj, obj_can_sense2);
}



/*
 * Go to any level (ripped off from wiz_jump)
 */
static void pattern_teleport(void)
{
    int min_level = 1;
    int max_level = 99;

    /* Ask for level */
    if (get_check("Teleport level? "))

    {
        char    ppp[80];
        char    tmp_val[160];

        max_level = dun_type()->plr_max_lvl;
        min_level = dun_type()->min_dun_lvl;

        /* Prompt */
        sprintf(ppp, "Teleport to level (%d-%d): ", min_level, max_level);


        /* Default */
        sprintf(tmp_val, "%d", cave->dun_lvl);

        /* Ask for a level */
        if (!get_string(ppp, tmp_val, 10)) return;

        /* Extract request */
        command_arg = atoi(tmp_val);
        if (command_arg < min_level) command_arg = min_level;
        if (command_arg > max_level) command_arg = max_level;
    }
    else if (get_check("Normal teleport? "))
    {
        teleport_player(200, 0L);
        return;
    }
    else if (get_check("Recall? "))
    {
        dun_mgr_recall_plr();
        return;
    }
    else
    {
        return;
    }

    /* Paranoia */
    if (command_arg < min_level) command_arg = min_level;

    /* Paranoia */
    if (command_arg > max_level) command_arg = max_level;

    /* Accept request */
    msg_format("You teleport to dungeon level %d.", command_arg);

    /* Change level */
    energy_use = 0;
    dun_mgr_wizard_jump(cave->dun_type_id, command_arg);
}


static void wreck_the_pattern(void)
{
    int to_ruin = 0;
    int pattern_type = f_info[cave_at(p_ptr->pos)->feat].subtype;

    if (pattern_type == PATTERN_TILE_WRECKED)
    {
        /* Ruined already */
        return;
    }

    msg_print("You bleed on the Pattern!");
    msg_print("Something terrible happens!");

    if (!plr_tim_find(T_INVULN))
        take_hit(DAMAGE_NOESCAPE, damroll(10, 8), "corrupting the Pattern");

    to_ruin = randint1(45) + 35;

    while (to_ruin--)
    {
        point_t pos = scatter(p_ptr->pos, 4);
        if (pattern_tile(pos.y, pos.x) &&
            (f_info[cave_at(pos)->feat].subtype != PATTERN_TILE_WRECKED))
        {
            cave_set_feat(pos.y, pos.x, feat_pattern_corrupted);
        }
    }

    cave_set_feat(p_ptr->pos.y, p_ptr->pos.x, feat_pattern_corrupted);
}


/* Returns TRUE if we are on the Pattern... */
static bool pattern_effect(void)
{
    int pattern_type;

    if (!pattern_tile(p_ptr->pos.y, p_ptr->pos.x)) return FALSE;

    if ((prace_is_(RACE_AMBERITE)) &&
        (plr_tim_amount(T_CUT) > 0) && one_in_(10))
    {
        wreck_the_pattern();
    }

    pattern_type = f_info[cave_at(p_ptr->pos)->feat].subtype;

    switch (pattern_type)
    {
    case PATTERN_TILE_END:
        plr_tim_remove(T_POISON);
        plr_tim_remove(T_HALLUCINATE);
        plr_tim_remove(T_STUN);
        plr_tim_remove(T_CUT);
        plr_tim_remove(T_BLIND);
        fear_clear_p();
        do_res_stat(A_STR);
        do_res_stat(A_INT);
        do_res_stat(A_WIS);
        do_res_stat(A_DEX);
        do_res_stat(A_CON);
        do_res_stat(A_CHR);
        restore_level();
        hp_player(1000);
        plr_restore_life(1000);

        cave_set_feat(p_ptr->pos.y, p_ptr->pos.x, feat_pattern_old);

        msg_print("This section of the Pattern looks less powerful.");

        /*
         * We could make the healing effect of the
         * Pattern center one-time only to avoid various kinds
         * of abuse, like luring the win monster into fighting you
         * in the middle of the pattern...
         */
        break;

    case PATTERN_TILE_OLD:
        /* No effect */
        break;

    case PATTERN_TILE_TELEPORT:
        pattern_teleport();
        break;

    case PATTERN_TILE_WRECKED:
        if (!plr_tim_find(T_INVULN))
            take_hit(DAMAGE_NOESCAPE, 200, "walking the corrupted Pattern");
        break;

    default:
        if (prace_is_(RACE_AMBERITE) && !one_in_(2))
            return TRUE;
        else if (!plr_tim_find(T_INVULN))
        {
            int dd = 1 + cave->difficulty / 40;
            int ds = 3 + cave->difficulty / 20;
            take_hit(DAMAGE_NOESCAPE, damroll(dd, ds), "walking the Pattern");
        }
        break;
    }

    return TRUE;
}





/*
 * Regenerate hit points                -RAK-
 */
static void regenhp(int percent)
{
    s32b new_chp;
    u32b new_chp_frac;
    s32b old_chp;

    if (p_ptr->special_defense & KATA_KOUKIJIN) return;
    if (p_ptr->action == ACTION_QUICK_WALK) return;
    if (p_ptr->action == ACTION_STALK) return;
    if (mimic_no_regen()) return;
    if (weaponmaster_get_toggle() == TOGGLE_SHADOW_STANCE) return;

    /* Save the old hitpoints */
    old_chp = p_ptr->chp;

    /*
     * Extract the new hitpoints
     *
     * 'percent' is the Regen factor in unit (1/2^16)
     */
    new_chp = 0;
    new_chp_frac = (p_ptr->mhp * percent + PY_REGEN_HPBASE);

    /* Convert the unit (1/2^16) to (1/2^32) */
    s64b_LSHIFT(new_chp, new_chp_frac, 16);

    /* Regenerating */
    s64b_add(&(p_ptr->chp), &(p_ptr->chp_frac), new_chp, new_chp_frac);


    /* Fully healed */
    if (0 < s64b_cmp(p_ptr->chp, p_ptr->chp_frac, p_ptr->mhp, 0))
    {
        p_ptr->chp = p_ptr->mhp;
        p_ptr->chp_frac = 0;
    }

    /* Notice changes */
    if (old_chp != p_ptr->chp)
    {
        /* Redraw */
        p_ptr->redraw |= (PR_HP);

        /* Blood Knights get extra attacks depending on how wounded they are */
        if (p_ptr->pclass == CLASS_BLOOD_KNIGHT)
            p_ptr->update |= PU_BONUS;

        if (weaponmaster_is_(WEAPONMASTER_STAVES))
            p_ptr->update |= (PU_BONUS);

        wild_regen = 20;
    }
}


/*
 * Regenerate mana points
 */
static void _decay_mana(void)
{
    /* PY_REGEN_NORMAL is the Regen factor in unit (1/2^16) */
    s32b decay = 0;
    u32b decay_frac = (p_ptr->msp * 32 * PY_REGEN_NORMAL + PY_REGEN_MNBASE);

    /* Convert the unit (1/2^16) to (1/2^32) */
    s64b_LSHIFT(decay, decay_frac, 16);

    /* Decay */
    s64b_sub(&(p_ptr->csp), &(p_ptr->csp_frac), decay, decay_frac);

    /* Stop decaying */
    if (p_ptr->csp < p_ptr->msp)
    {
        p_ptr->csp = p_ptr->msp;
        p_ptr->csp_frac = 0;
    }
}
static void regenmana(int percent)
{
    s32b old_csp = p_ptr->csp;

    if (p_ptr->pclass == CLASS_RUNE_KNIGHT || p_ptr->pclass == CLASS_RAGE_MAGE) return;
    if (mimic_no_regen())
    {
        if (p_ptr->csp > p_ptr->msp) /* Doppelganger Samurai/Mystics should still decay supercharged mana! */
            _decay_mana();
        return;
    }

    /*
     * Excess mana will decay 32 times faster than normal
     * regeneration rate.
     */
    if (p_ptr->csp > p_ptr->msp)
    {
        _decay_mana();
    }
    /* Regenerating mana (unless the player has excess mana) */
    else if (percent > 0)
    {
        /* (percent/100) is the Regen factor in unit (1/2^16) */
        s32b new_mana = 0;
        u32b new_mana_frac = (p_ptr->msp * percent / 100 + PY_REGEN_MNBASE);

        /* Convert the unit (1/2^16) to (1/2^32) */
        s64b_LSHIFT(new_mana, new_mana_frac, 16);

        /* Regenerate */
        s64b_add(&(p_ptr->csp), &(p_ptr->csp_frac), new_mana, new_mana_frac);

        /* Must set frac to zero even if equal */
        if (p_ptr->csp >= p_ptr->msp)
        {
            p_ptr->csp = p_ptr->msp;
            p_ptr->csp_frac = 0;
        }
    }


    /* Reduce mana (even when the player has excess mana) */
    if (percent < 0)
    {
        /* PY_REGEN_NORMAL is the Regen factor in unit (1/2^16) */
        s32b reduce_mana = 0;
        u32b reduce_mana_frac = (p_ptr->msp * PY_REGEN_NORMAL + PY_REGEN_MNBASE);

        /* Convert the unit (1/2^16) to (1/2^32) */
        s64b_LSHIFT(reduce_mana, reduce_mana_frac, 16);

        /* Reduce mana */
        s64b_sub(&(p_ptr->csp), &(p_ptr->csp_frac), reduce_mana, reduce_mana_frac);

        /* Check overflow */
        if (p_ptr->csp < 0)
        {
            p_ptr->csp = 0;
            p_ptr->csp_frac = 0;
        }
    }


    /* Redraw mana */
    if (old_csp != p_ptr->csp)
    {
        /* Redraw */
        p_ptr->redraw |= (PR_MANA);

        /* Window stuff */
        p_ptr->window |= (PW_SPELL);

        wild_regen = 20;
    }
}




/*
 * Regenerate the monsters (once per 100 game turns)
 *
 * XXX XXX XXX Should probably be done during monster turns.
 */
static void _regen_mon(int id, mon_ptr mon)
{
    if (mon->hp < mon->maxhp)
    {
        mon_race_ptr race = mon_race(mon);
        int          amt = mon->maxhp / 100;

        if (!amt) if (one_in_(2)) amt = 1;
        if (race->flags2 & RF2_REGENERATE) amt *= 2;
        if (amt >= 400) amt = 400;

        mon->hp += amt;
        if (mon->hp > mon->maxhp) mon->hp = mon->maxhp;
        check_mon_health_redraw(mon->id);
    }
}
static void regen_monsters(void) { dun_iter_mon(cave, _regen_mon); }


/*
 * Regenerate the captured monsters (once per 30 game turns)
 *
 * XXX XXX XXX Should probably be done during monster turns.
 */
static bool _is_captured_mon(obj_ptr obj) { return obj->tval == TV_CAPTURE && obj->pval; }
static void _regen_captured_mon(obj_ptr obj)
{
    monster_race *r_ptr = &r_info[obj->pval];
    if (obj->xtra4 < obj->xtra5)
    {
        int amt = obj->xtra5 / 100;
        if (!amt && one_in_(2)) amt = 1;
        if (r_ptr->flags2 & RF2_REGENERATE) amt *= 2;
        obj->xtra4 = MIN(obj->xtra5, obj->xtra4 + amt);
    }
}

static void regen_captured_monsters(void)
{
    pack_for_each_that(_regen_captured_mon, _is_captured_mon);
    equip_for_each_that(_regen_captured_mon, _is_captured_mon);
}


void notice_lite_change(object_type *o_ptr)
{
    /* Hack -- notice interesting fuel steps */
    if ((o_ptr->xtra4 < 100) || (!(o_ptr->xtra4 % 100)))
    {
        /* Window stuff */
        p_ptr->window |= (PW_EQUIP);
    }

    /* Hack -- Special treatment when blind */
    if (plr_tim_find(T_BLIND))
    {
        /* Hack -- save some light for later */
        if (o_ptr->xtra4 == 0) o_ptr->xtra4++;
    }

    /* The light is now out */
    else if (o_ptr->xtra4 == 0)
    {
        disturb(0, 0);
        msg_print("Your light has gone out!");

        /* Recalculate torch radius */
        p_ptr->update |= (PU_TORCH);

        /* Some ego light lose its effects without fuel */
        p_ptr->update |= (PU_BONUS);
    }

    /* The light is getting dim */
    else if (o_ptr->name2 == EGO_LITE_DURATION)
    {
        if ((o_ptr->xtra4 < 50) && (!(o_ptr->xtra4 % 5))
            && (dun_mgr()->turn % (TURNS_PER_TICK*2)))
        {
            if (disturb_minor) disturb(0, 0);
            msg_print("Your light is growing faint.");

        }
    }

    /* The light is getting dim */
    else if ((o_ptr->xtra4 < 100) && (!(o_ptr->xtra4 % 10)))
    {
        if (disturb_minor) disturb(0, 0);
        msg_print("Your light is growing faint.");

    }
}

void fame_on_failure(void)
{
    int dec = p_ptr->fame/2;
    if (dec > 30)
        dec = 30;
    assert (dec <= p_ptr->fame);
    p_ptr->fame -= dec;
}

/*
 * Forcibly pseudo-identify an object in the inventory
 * (or on the floor)
 *
 * note: currently this function allows pseudo-id of any object,
 * including silly ones like potions & scrolls, which always
 * get '{average}'. This should be changed, either to stop such
 * items from being pseudo-id'd, or to allow psychometry to
 * detect whether the unidentified potion/scroll/etc is
 * good (Cure Light Wounds, Restore Strength, etc) or
 * bad (Poison, Weakness etc) or 'useless' (Slime Mold Juice, etc).
 */
bool psychometry(void)
{
    obj_prompt_t prompt = {0};
    char         o_name[MAX_NLEN];
    byte         feel;
    bool         okay = FALSE;

    prompt.prompt = "Meditate on which item?";
    prompt.error = "You have nothing appropriate.";
    prompt.where[0] = INV_PACK;
    prompt.where[1] = INV_EQUIP;
    prompt.where[2] = INV_QUIVER;
    prompt.where[3] = INV_FLOOR;

    obj_prompt(&prompt);
    if (!prompt.obj) return FALSE;

    /* It is fully known, no information needed */
    if (obj_is_known(prompt.obj))
    {
        msg_print("You cannot find out anything more about that.");

        return TRUE;
    }

    /* Check for a feeling */
    feel = value_check_aux1(prompt.obj);

    /* Get an object description */
    object_desc(o_name, prompt.obj, (OD_OMIT_PREFIX | OD_NAME_ONLY));

    /* Skip non-feelings */
    if (!feel)
    {
        msg_format("You do not perceive anything unusual about the %s.", o_name);

        return TRUE;
    }

    msg_format("You feel that the %s %s %s...",
               o_name, ((prompt.obj->number == 1) ? "is" : "are"),
               game_inscriptions[feel]);


    /* We have "felt" it */
    prompt.obj->ident |= (IDENT_SENSE);

    /* "Inscribe" it */
    prompt.obj->feeling = feel;

    /* Player touches it */
    prompt.obj->marked |= OM_TOUCHED;

    /* Valid "tval" codes */
    switch (prompt.obj->tval)
    {
    case TV_SHOT:
    case TV_ARROW:
    case TV_BOLT:
    case TV_BOW:
    case TV_DIGGING:
    case TV_HAFTED:
    case TV_POLEARM:
    case TV_SWORD:
    case TV_BOOTS:
    case TV_GLOVES:
    case TV_HELM:
    case TV_CROWN:
    case TV_SHIELD:
    case TV_CLOAK:
    case TV_SOFT_ARMOR:
    case TV_HARD_ARMOR:
    case TV_DRAG_ARMOR:
    case TV_CARD:
    case TV_RING:
    case TV_AMULET:
    case TV_LITE:
    case TV_FIGURINE:
        okay = TRUE;
        break;
    }

    autopick_alter_obj(prompt.obj, okay && destroy_feeling);
    obj_release(prompt.obj, OBJ_RELEASE_ID | OBJ_RELEASE_QUIET);

    /* Something happened */
    return (TRUE);
}


/*
 * If player has inscribed the object with "!!", let him know when it's
 * recharged. -LM-
 */
void recharged_notice(object_type *o_ptr)
{
    char o_name[MAX_NLEN];

    cptr s;

    /* No inscription */
    if (!o_ptr->inscription) return;

    /* Find a '!' */
    s = my_strchr(quark_str(o_ptr->inscription), '!');

    /* Process notification request. */
    while (s)
    {
        /* Find another '!' */
        if (s[1] == '!')
        {
            /* Describe (briefly) */
            object_desc(o_name, o_ptr, OD_OMIT_PREFIX | OD_OMIT_INSCRIPTION | OD_COLOR_CODED);

            /* Notify the player */
            if (o_ptr->number > 1)
                msg_format("Your %s are recharged.", o_name);
            else
                msg_format("Your %s is recharged.", o_name);

            disturb(0, 0);

            /* Done. */
            return;
        }

        /* Keep looking for '!'s */
        s = my_strchr(s + 1, '!');
    }
}


/* Choose one of items that have cursed flag */
static u32b _curse_flag = 0;
static bool _object_is_cursed(object_type *o_ptr) {
    if (o_ptr->curse_flags & _curse_flag)
        return TRUE;
    return FALSE;
}
static object_type *choose_cursed_obj_name(u32b flag)
{
    int slot;
    _curse_flag = flag;
    slot = equip_random_slot(_object_is_cursed);
    if (slot)
        return equip_obj(slot);
    return NULL;
}

/*
 * Handle timed damage and regeneration every 10 game turns
 */
static void process_world_aux_hp_and_sp(void)
{
    feature_type *f_ptr = &f_info[cave_at(p_ptr->pos)->feat];
    bool cave_no_regen = FALSE;
    int upkeep_factor = 0;
    int upkeep_regen;

    /* Default regeneration */
    int regen_amount = PY_REGEN_NORMAL;


    /*** Damage over Time ***/

    /* (Vampires) Take damage from sunlight. Note, Vampires are vulnerable
       to light so start with -50% resistance. Rather than res_save(RES_LIGHT)
       we will simply take damage so long as there is light vulnerability. */
    if (prace_is_(RACE_VAMPIRE) || prace_is_(RACE_MON_VAMPIRE) || p_ptr->mimic_form == MIMIC_VAMPIRE)
    {
        int slot;
        if (cave->dun_type_id == D_SURFACE && res_pct(RES_LITE) < 0 && !plr_tim_find(T_INVULN) && is_daytime())
        {
            if ((cave_at(p_ptr->pos)->info & (CAVE_GLOW | CAVE_MNDK)) == CAVE_GLOW)
            {
                msg_print("The sun's rays scorch your undead flesh!");
                take_hit(DAMAGE_NOESCAPE, 1, "sunlight");
                cave_no_regen = TRUE;
            }
        }

        slot = equip_find_obj(TV_LITE, SV_ANY);
        if (slot)
        {
            object_type *lite = equip_obj(slot);

            if ( !obj_has_flag(lite, OF_DARKNESS)
              && res_pct(RES_LITE) < 0)
            {
                char o_name [MAX_NLEN];
                char ouch [MAX_NLEN+40];

                object_desc(o_name, lite, OD_OMIT_PREFIX | OD_NAME_ONLY);
                msg_format("The %s scorches your undead flesh!", o_name);
                cave_no_regen = TRUE;
                object_desc(o_name, lite, OD_NAME_ONLY);
                sprintf(ouch, "wielding %s", o_name);
                if (!plr_tim_find(T_INVULN)) take_hit(DAMAGE_NOESCAPE, 1, ouch);
            }
        }
    }

    if (have_flag(f_ptr->flags, FF_LAVA) && !plr_tim_find(T_INVULN) && !elemental_is_(ELEMENTAL_FIRE))
    {
        int damage = 0;

        if (have_flag(f_ptr->flags, FF_DEEP))
        {
            damage = 6000 + randint0(4000);
        }
        else if (!p_ptr->levitation)
        {
            damage = 3000 + randint0(2000);
        }

        damage = res_calc_dam(RES_FIRE, damage);
        if (p_ptr->levitation) damage = damage / 5;

        if (damage)
        {
            damage = damage / 100 + (randint0(100) < (damage % 100));

            if (p_ptr->levitation)
            {
                msg_print("The heat burns you!");
                take_hit(DAMAGE_NOESCAPE, damage, format("flying over %s", f_name + f_info[get_feat_mimic(cave_at(p_ptr->pos))].name));
            }
            else
            {
                cptr name = f_name + f_info[get_feat_mimic(cave_at(p_ptr->pos))].name;
                msg_format("The %s burns you!", name);
                take_hit(DAMAGE_NOESCAPE, damage, name);
            }

            cave_no_regen = TRUE;
        }
    }

    if (have_flag(f_ptr->flags, FF_WATER) && have_flag(f_ptr->flags, FF_DEEP) &&
        !p_ptr->levitation && !p_ptr->can_swim &&
        !elemental_is_(ELEMENTAL_WATER) && !prace_is_(RACE_WATER_ELF))
    {
        if (plr_total_weight() > weight_limit())
        {
            /* Take damage */
            msg_print("You are drowning!");
            take_hit(DAMAGE_NOESCAPE, randint1(p_ptr->lev), "drowning");

            cave_no_regen = TRUE;
        }
    }

    if (p_ptr->riding)
        plr_on_ride_mon(p_ptr->riding);

    /* Spectres -- take damage when moving through walls */
    /*
     * Added: ANYBODY takes damage if inside through walls
     * without wraith form -- NOTE: Spectres will never be
     * reduced below 0 hp by being inside a stone wall; others
     * WILL BE!
     */
    if (!have_flag(f_ptr->flags, FF_MOVE) && !have_flag(f_ptr->flags, FF_CAN_FLY))
    {
        if (!plr_tim_find(T_INVULN) && !plr_tim_find(T_WRAITH))
        {
            int dam;
            cptr dam_desc;

            dam = 1 + p_ptr->lev/5;
            /* Passwall now takes more energy ...
            dam = MAX(1 + p_ptr->lev/5, 1 + p_ptr->mhp/24);*/
            if (p_ptr->pass_wall)
            {
                if (p_ptr->no_passwall_dam)
                    dam = 0;
                else
                {
                    msg_print("Your molecules feel disrupted!");
                    dam_desc = "density";
                    if (p_ptr->prace == RACE_SPECTRE && dam > p_ptr->chp)
                        dam = p_ptr->chp;
                }
            }
            else
            {
                msg_print("You are being crushed!");
                dam_desc = "solid rock";
            }

            if (dam)
            {
                cave_no_regen = TRUE;
                take_hit(DAMAGE_NOESCAPE, dam, dam_desc);
            }
        }
    }


    /*** handle regeneration ***/

    /* Getting Weak */
    if (p_ptr->food < PY_FOOD_WEAK)
    {
        /* Lower regeneration */
        if (p_ptr->food < PY_FOOD_STARVE)
        {
            regen_amount = 0;
        }
        else if (p_ptr->food < PY_FOOD_FAINT)
        {
            regen_amount = PY_REGEN_FAINT;
        }
        else
        {
            regen_amount = PY_REGEN_WEAK;
        }
    }

    /* Are we walking the pattern? */
    if (pattern_effect())
    {
        cave_no_regen = TRUE;
    }
    else
    {
        regen_amount = regen_amount * p_ptr->regen/100;
    }

    if ( p_ptr->action == ACTION_SEARCH
      || p_ptr->action == ACTION_REST
      || p_ptr->action == ACTION_GLITTER )
    {
        regen_amount = regen_amount * 2;
    }

    upkeep_factor = calculate_upkeep();

    /* No regeneration while special action */
    if (p_ptr->action == ACTION_LEARN ||
        p_ptr->action == ACTION_QUICK_WALK ||
        p_ptr->action == ACTION_STALK ||
        (p_ptr->special_defense & KATA_KOUKIJIN) ||
        weaponmaster_get_toggle() == TOGGLE_SHADOW_STANCE)
    {
        upkeep_factor += 100;
    }

    /* Regenerate the mana */
    upkeep_regen = (100 - upkeep_factor) * regen_amount;

    if (plr_mage_bonus())
        upkeep_regen = upkeep_regen * 2;

    regenmana(upkeep_regen);

    if (magic_eater_regen(regen_amount))
        wild_regen = 20;

    if ((p_ptr->csp == 0) && (p_ptr->csp_frac == 0))
    {
        if (p_ptr->msp == 0 && !one_in_(5))
        {
            /* Currently, non-mana characters can't have many pets. Maybe this is OK for
               warriors but what about a Trump Blood Mage? */
        }
        else
        {
            while (upkeep_factor > 100)
            {
                msg_print("Too many pets to control at once!");
                msg_print(NULL);
                do_cmd_pet_dismiss();
                upkeep_factor = calculate_upkeep();
                msg_format("Upkeep: %d%% mana.", upkeep_factor);
                msg_print(NULL);
            }
        }
    }

    /* Poisoned or cut yields no healing */
    if (plr_tim_find(T_POISON)) regen_amount = 0;
    if (plr_tim_find(T_CUT) && p_ptr->pclass != CLASS_BLOOD_KNIGHT) regen_amount = 0;

    /* Special floor -- Pattern, in a wall -- yields no healing */
    if (cave_no_regen) regen_amount = 0;

    regen_amount = (regen_amount * mutant_regenerate_mod) / 100;

    /* Regenerate Hit Points if needed */
    if ((p_ptr->chp < p_ptr->mhp) && !cave_no_regen)
    {
        regenhp(regen_amount);
    }
}

/*
 * Handle timeout every 10 game turns
 */
static void process_world_aux_timeout(void)
{
    /*** Timeout Various Things ***/
    plr_tim_tick();

    if (p_ptr->prace == RACE_DOPPELGANGER)
        mimic_upkeep();

    /* Mimic */
    if (p_ptr->tim_mimic)
    {
        (void)set_mimic(p_ptr->tim_mimic - 1, p_ptr->mimic_form, TRUE);
    }

    if (p_ptr->fasting && one_in_(7))
    {
        if (randint1(PY_FOOD_ALERT) > p_ptr->food)
        {
            switch (randint1(8))
            {
            case 1: do_res_stat(A_STR); break;
            case 2: do_res_stat(A_INT); break;
            case 3: do_res_stat(A_WIS); break;
            case 4: do_res_stat(A_DEX); break;
            case 5: do_res_stat(A_CON); break;
            case 6: do_res_stat(A_CHR); break;
            case 7: restore_level(); break;
            case 8: plr_restore_life(150); break;
            }
        }
    }
}


/*
 * Handle burning fuel every 10 game turns
 */
static void process_world_aux_light(void)
{
    int slot = equip_find_obj(TV_LITE, SV_ANY);
    if (slot)
    {
        object_type *lite = equip_obj(slot);
        if ( !(lite->name1 || lite->name3 || lite->art_name || lite->sval == SV_LITE_FEANOR)
          && lite->xtra4 > 0 )
        {
            if (lite->name2 == EGO_LITE_DURATION)
            {
                if (dun_mgr()->turn % (TURNS_PER_TICK*2)) lite->xtra4--;
            }
            else lite->xtra4--;
            notice_lite_change(lite);
        }
    }
}


/*
 * Handle curse effects once every 10 game turns
 */
static void process_world_aux_curse(void)
{
    if ((p_ptr->cursed & TRC_P_FLAG_MASK))
    {
        /*
         * Hack: Uncursed teleporting items (e.g. Trump Weapons)
         * can actually be useful!
         */
        if ((p_ptr->cursed & OFC_TELEPORT_SELF) && one_in_(200))
        {
            char o_name[MAX_NLEN];
            object_type *o_ptr;
            int i, i_keep = 0, count = 0;

            /* Scan the equipment with random teleport ability */
            for (i = 1; i <= equip_max(); i++)
            {
                o_ptr = equip_obj(i);

                if (!o_ptr) continue;
                if (obj_has_flag(o_ptr, OF_TELEPORT))
                {
                    /* {.} will stop random teleportation. */
                    if (!o_ptr->inscription || !my_strchr(quark_str(o_ptr->inscription), '.'))
                    {
                        count++;
                        if (one_in_(count)) i_keep = i;
                    }
                }
            }

            if (i_keep)
            {
                o_ptr = equip_obj(i_keep);
                object_desc(o_name, o_ptr, (OD_OMIT_PREFIX | OD_NAME_ONLY));
                msg_format("Your %s is activating teleportation.", o_name);
                if (get_check_strict("Teleport? ", CHECK_OKAY_CANCEL))
                {
                    disturb(0, 0);
                    teleport_player(50, 0L);
                }
                else
                {
                    msg_format("You can inscribe {.} on your %s to disable random teleportation. ", o_name);
                    disturb(1, 0);
                }
                obj_learn_flag(o_ptr, OF_TELEPORT);
            }
        }
        /* Make a chainsword noise */
        if ((p_ptr->cursed & OFC_CHAINSWORD) && one_in_(CHAINSWORD_NOISE))
        {
            char noise[1024];
            if (!get_rnd_line("chainswd.txt", 0, noise))
                msg_print(noise);
            disturb(FALSE, FALSE);
        }
        /* TY Curse */
        if ((p_ptr->cursed & OFC_TY_CURSE) && one_in_(TY_CURSE_CHANCE))
        {
            int count = 0;
            (void)activate_ty_curse(FALSE, &count);
            equip_learn_curse(OFC_TY_CURSE);
        }
        /* Handle experience draining */
        if (p_ptr->prace != RACE_ANDROID &&
            ((p_ptr->cursed & OFC_DRAIN_EXP) && one_in_(4)))
        {
            p_ptr->exp -= (p_ptr->lev+1)/2;
            if (p_ptr->exp < 0) p_ptr->exp = 0;
            p_ptr->max_exp -= (p_ptr->lev+1)/2;
            if (p_ptr->max_exp < 0) p_ptr->max_exp = 0;
            check_experience();
            equip_learn_curse(OFC_DRAIN_EXP);
            equip_learn_flag(OF_DRAIN_EXP);
        }
        /* Add light curse (Later) */
        if ((p_ptr->cursed & OFC_ADD_L_CURSE) && one_in_(2000))
        {
            u32b new_curse;
            object_type *o_ptr;

            o_ptr = choose_cursed_obj_name(OFC_ADD_L_CURSE);

            new_curse = get_curse(0, o_ptr);
            if (!(o_ptr->curse_flags & new_curse))
            {
                char o_name[MAX_NLEN];

                object_desc(o_name, o_ptr, (OD_OMIT_PREFIX | OD_NAME_ONLY));

                o_ptr->curse_flags |= new_curse;
                msg_format("There is a malignant black aura surrounding your %s...", o_name);

                o_ptr->feeling = FEEL_NONE;

                p_ptr->update |= (PU_BONUS);
                obj_learn_curse(o_ptr, OFC_ADD_L_CURSE);
            }
        }
        /* Add heavy curse (Later) */
        if ((p_ptr->cursed & OFC_ADD_H_CURSE) && one_in_(2000))
        {
            u32b new_curse;
            object_type *o_ptr;

            o_ptr = choose_cursed_obj_name(OFC_ADD_H_CURSE);

            new_curse = get_curse(1, o_ptr);
            if (!(o_ptr->curse_flags & new_curse))
            {
                char o_name[MAX_NLEN];

                object_desc(o_name, o_ptr, (OD_OMIT_PREFIX | OD_NAME_ONLY));

                o_ptr->curse_flags |= new_curse;
                msg_format("There is a malignant black aura surrounding your %s...", o_name);

                o_ptr->feeling = FEEL_NONE;

                p_ptr->update |= (PU_BONUS);
                obj_learn_curse(o_ptr, OFC_ADD_H_CURSE);
            }
        }
        /* Call animal */
        if ((p_ptr->cursed & OFC_CALL_ANIMAL) && one_in_(2500))
        {
            if (summon_specific(0, p_ptr->pos, cave->dun_lvl, SUMMON_ANIMAL,
                (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_NO_PET)))
            {
                char o_name[MAX_NLEN];
                object_type *o_ptr = choose_cursed_obj_name(OFC_CALL_ANIMAL);

                object_desc(o_name, o_ptr, (OD_OMIT_PREFIX | OD_NAME_ONLY));
                msg_format("Your %s have attracted an animal!", o_name);

                disturb(0, 0);
                obj_learn_curse(o_ptr, OFC_CALL_ANIMAL);
            }
        }
        /* Call demon */
        if ((p_ptr->cursed & OFC_CALL_DEMON) && one_in_(1111))
        {
            if (summon_specific(0, p_ptr->pos, cave->dun_lvl, SUMMON_DEMON, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_NO_PET)))
            {
                char o_name[MAX_NLEN];
                object_type *o_ptr = choose_cursed_obj_name(OFC_CALL_DEMON);

                object_desc(o_name, o_ptr, (OD_OMIT_PREFIX | OD_NAME_ONLY));
                msg_format("Your %s have attracted a demon!", o_name);

                disturb(0, 0);
                obj_learn_curse(o_ptr, OFC_CALL_DEMON);
            }
        }
        /* Call dragon */
        if ((p_ptr->cursed & OFC_CALL_DRAGON) && one_in_(800))
        {
            if (summon_specific(0, p_ptr->pos, cave->dun_lvl, SUMMON_DRAGON,
                (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_NO_PET)))
            {
                char o_name[MAX_NLEN];
                object_type *o_ptr = choose_cursed_obj_name(OFC_CALL_DRAGON);

                object_desc(o_name, o_ptr, (OD_OMIT_PREFIX | OD_NAME_ONLY));
                msg_format("Your %s have attracted a dragon!", o_name);

                disturb(0, 0);
                obj_learn_curse(o_ptr, OFC_CALL_DRAGON);
            }
        }
        if ((p_ptr->cursed & OFC_COWARDICE) && one_in_(1500))
        {
            if (!fear_save_p(fear_threat_level()))
            {
                disturb(0, 0);
                msg_print("It's so dark... so scary!");

                fear_add_p(FEAR_SCARED);
                equip_learn_curse(OFC_COWARDICE);
            }
        }
        /* Teleport player */
        if ((p_ptr->cursed & OFC_TELEPORT) && one_in_(200) && !p_ptr->anti_tele)
        {
            disturb(0, 0);

            /* Teleport player */
            teleport_player(40, TELEPORT_PASSIVE);
            equip_learn_curse(OFC_TELEPORT);
            equip_learn_flag(OF_TELEPORT);
        }
        /* Handle HP draining */
        if ((p_ptr->cursed & OFC_DRAIN_HP) && one_in_(666))
        {
            char o_name[MAX_NLEN];
            object_type *o_ptr = choose_cursed_obj_name(OFC_DRAIN_HP);

            object_desc(o_name, o_ptr, (OD_OMIT_PREFIX | OD_NAME_ONLY));
            msg_format("Your %s drains HP from you!", o_name);
            take_hit(DAMAGE_LOSELIFE, MIN(p_ptr->lev*2, 100), o_name);
            obj_learn_curse(o_ptr, OFC_DRAIN_HP);
        }
        /* Handle mana draining */
        if ((p_ptr->cursed & OFC_DRAIN_MANA) && p_ptr->csp && one_in_(666))
        {
            char o_name[MAX_NLEN];
            object_type *o_ptr = choose_cursed_obj_name(OFC_DRAIN_MANA);

            object_desc(o_name, o_ptr, (OD_OMIT_PREFIX | OD_NAME_ONLY));
            msg_format("Your %s drains mana from you!", o_name);
            p_ptr->csp -= MIN(p_ptr->lev, 50);
            if (p_ptr->csp < 0)
            {
                p_ptr->csp = 0;
                p_ptr->csp_frac = 0;
            }
            p_ptr->redraw |= PR_MANA;
            obj_learn_curse(o_ptr, OFC_DRAIN_MANA);
        }
    }

    /* Rarely, take damage from the Jewel of Judgement */
    if (one_in_(999) && !p_ptr->anti_magic)
    {
        int slot = equip_find_art(ART_JUDGE);
        if (slot)
        {
            object_type *o_ptr = equip_obj(slot);
            if (obj_is_known(o_ptr))
                msg_print("The Jewel of Judgement drains life from you!");
            else
                msg_print("Something drains life from you!");
            take_hit(DAMAGE_LOSELIFE, MIN(p_ptr->lev, 50), "the Jewel of Judgement");
        }
    }

    if (mon_race_lookup(MON_SAURON)->max_num && one_in_(666))
    {
        int slot = equip_find_ego(EGO_RING_NAZGUL);
        if (slot)
        {
            object_type *o_ptr = equip_obj(slot);

            o_ptr->curse_flags |= OFC_HEAVY_CURSE;
            o_ptr->curse_flags |= OFC_CURSED;
            o_ptr->curse_flags |= get_curse(2, o_ptr);
            p_ptr->update |= PU_BONUS;

            msg_boundary();
            cmsg_print(TERM_VIOLET, "You behold the Eye of Sauron!");
            if (one_in_(2))
            {
                msg_print("You feel your life draining away...");
                lose_exp(p_ptr->exp / 16);
            }
            while (one_in_(2))
            {
                do_dec_stat(randint0(6));
            }
            if (one_in_(2))
            {
                msg_print("You forget yourself in utter terror!");
                lose_all_info();
            }
            if (!p_ptr->no_stun && one_in_(2))
                plr_tim_add(T_STUN, randint1(40));
            if (one_in_(2))
                plr_tim_add(T_CONFUSED, randint1(5) + 5);
        }
    }

    if (one_in_(666))
    {
        int slot = equip_find_art(ART_HAND_OF_VECNA);
        if (slot)
        {
            object_type *o_ptr = equip_obj(slot);
            if (obj_is_known(o_ptr))
                msg_print("The Hand of Vecna strangles you!");
            else
                msg_print("The Hand strangles you!");
            take_hit(DAMAGE_LOSELIFE, MIN(p_ptr->lev, 50), "the Hand of Vecna");
        }
    }

    if (one_in_(666))
    {
        int slot = equip_find_art(ART_EYE_OF_VECNA);
        if (slot)
        {
            object_type *o_ptr = equip_obj(slot);
            if (obj_is_known(o_ptr))
                msg_print("The Eye of Vecna causes mental anquish!");
            else
                msg_print("The Eye causes mental anquish!");

            p_ptr->csp -= MIN(p_ptr->lev, 50);
            if (p_ptr->csp < 0)
            {
                p_ptr->csp = 0;
                p_ptr->csp_frac = 0;
            }
            p_ptr->redraw |= PR_MANA;
        }
    }

    if (one_in_(66))
    {
        int slot = equip_find_art(ART_BLOODRIP);
        if (slot)
        {
            object_type *o_ptr = equip_obj(slot);
            if (!p_ptr->no_cut && plr_tim_add(T_CUT, 66))
            {
                if (obj_is_known(o_ptr))
                    msg_print("Bloodrip feeds on you!");
                else
                    msg_print("Something feeds on you!");
            }
        }
    }
}


/*
 * Handle recharging objects once every 10 game turns
 */
static bool _recharge_changed = FALSE;
static void _recharge_equip(obj_ptr obj)
{
    if (obj_is_(obj, TV_HAFTED, SV_WIZSTAFF))
    {
        device_regen_sp(obj, 10);
    }
    else if (obj->timeout > 0)
    {
        obj->timeout--;
        if (!obj->timeout)
        {
            recharged_notice(obj);
            _recharge_changed = TRUE;
        }
    }
}
static void _recharge_pack(obj_ptr obj)
{
    switch (obj->tval)
    {
    case TV_ROD:
        device_regen_sp(obj, 10);
        break;
    case TV_WAND:
    case TV_STAFF:
        if ((dun_mgr()->turn % (TURNS_PER_TICK*10)) == 0)
            device_regen_sp(obj, 10);
        break;
    }

    if (obj_is_(obj, TV_HAFTED, SV_WIZSTAFF))
    {
        if ((dun_mgr()->turn % (TURNS_PER_TICK*10)) == 0)
            device_regen_sp(obj, 10);
    }

    /* artifact mushrooms for the snotling ... they never stack */
    if (object_is_mushroom(obj) && obj->timeout)
    {
        obj->timeout--;
        if (obj->timeout < 0) obj->timeout = 0;
        if (!obj->timeout)
        {
            recharged_notice(obj);
            _recharge_changed = TRUE;
        }
    }
}

static void process_world_aux_recharge(void)
{
    _recharge_changed = FALSE;
    equip_for_each(_recharge_equip);
    pack_for_each(_recharge_pack);
    quiver_for_each(_recharge_pack); /* Mage Quiver */
    if (_recharge_changed)
    {
        p_ptr->window |= PW_EQUIP | PW_INVEN;
        wild_regen = 20;
    }
}


/*
 * Count number of adjacent monsters
 */
static int get_monster_crowd_number(int m_idx)
{
    monster_type *m_ptr = dun_mon(cave, m_idx);
    int my = m_ptr->pos.y;
    int mx = m_ptr->pos.x;
    int i;
    int count = 0;

    for (i = 0; i < 8; i++)
    {
        int ay = my + ddy_ddd[i];
        int ax = mx + ddx_ddd[i];

        if (!in_bounds(ay, ax)) continue;

        /* Count number of monsters */
        if (mon_at_xy(ax, ay)) count++;
     }

    return count;
}



/*
 * Dungeon rating is no longer linear
 */
#define RATING_BOOST(delta) (delta * delta + 50 * delta)

/*
 * Examine all monsters and unidentified objects,
 * and get the feeling of current dungeon floor
 */
static byte get_dungeon_feeling(void)
{
    const int base = 10;
    int       rating = 0;
    bool      special = FALSE;

    /* Hack -- no feeling in the town */
    if (cave->dun_type_id == D_SURFACE) return 0;

    /* Examine each monster */
   {int_map_iter_ptr iter;
    int boss_r_idx = plr_race()->boss_r_idx;
    for (iter = int_map_iter_alloc(cave->mon);
            int_map_iter_is_valid(iter);
            int_map_iter_next(iter))
    {
        mon_ptr mon = int_map_iter_current(iter);
        mon_race_ptr race = mon_race(mon);
        int     delta = 0;

        if (mon_is_pet(mon)) continue;
        if (boss_r_idx == mon->r_idx) special = TRUE;

        /* Unique monsters */
        if (race->flags1 & (RF1_UNIQUE))
        {
            /* Nearly out-of-depth unique monsters */
            if (race->level + 10 > cave->dun_lvl)
            {
                /* Boost rating by twice delta-depth */
                delta += (race->level + 10 - cave->dun_lvl) * 2 * base;
            }
        }
        else
        {
            /* Out-of-depth monsters */
            if (race->level > cave->dun_lvl)
            {
                /* Boost rating by delta-depth */
                delta += (race->level - cave->dun_lvl) * base;
            }
        }

        /* Unusually crowded monsters get a little bit of rating boost */
        if (race->flags1 & RF1_FRIENDS)
        {
            if (5 <= get_monster_crowd_number(mon->id)) delta += 1;
        }
        else
        {
            if (2 <= get_monster_crowd_number(mon->id)) delta += 1;
        }


        rating += RATING_BOOST(delta);
    }
    int_map_iter_free(iter);}

    /* Examine each unidentified object */
   {point_map_iter_ptr iter;
    for (iter = point_map_iter_alloc(cave->obj_pos);
            point_map_iter_is_valid(iter);
            point_map_iter_next(iter))
    {
        obj_ptr obj = point_map_iter_current(iter);
        obj_kind_ptr kind = &k_info[obj->k_idx];
        int delta = 0;

        /* Skip known objects */
        if (obj_is_known(obj))
        {
            /* Touched? */
            if (obj->marked & OM_TOUCHED) continue;
        }

        /* Skip pseudo-known objects */
        if (obj->ident & IDENT_SENSE) continue;

        /* Force Special Feelings for artifacts no matter what. */
        if (obj_is_art(obj)) special = TRUE;

        if (obj_is_ego(obj) || obj_is_dragon_armor(obj))
        {
            s32b cost = obj_value_real(obj);

            delta += 10 * base;
            if (cost > 10000) delta += 10 * base;
            if (cost > 50000) delta += 10 * base;
            if (cost > 100000) delta += 10 * base;
        }

        /* Out-of-depth objects */
        if (kind->level > cave->dun_lvl)
            delta += (kind->level - cave->dun_lvl) * base;

        rating += RATING_BOOST(delta);
    }
    point_map_iter_free(iter);}


    if (special) return 1;
    if (rating > RATING_BOOST(1000)) return 2;
    if (rating > RATING_BOOST(800)) return 3;
    if (rating > RATING_BOOST(600)) return 4;
    if (rating > RATING_BOOST(400)) return 5;
    if (rating > RATING_BOOST(300)) return 6;
    if (rating > RATING_BOOST(200)) return 7;
    if (rating > RATING_BOOST(100)) return 8;
    if (rating > RATING_BOOST(0)) return 9;
    return 10;
}


/*
 * Update dungeon feeling, and announce it if changed
 */
static void update_dungeon_feeling(void)
{
    byte new_feeling;

    if (cave->dun_type_id == D_SURFACE) return;
    if (!quests_allow_feeling()) return;

    if (cave->feeling_delay > 0)
    {
        cave->feeling_delay--;
        return;
    }

    new_feeling = get_dungeon_feeling();
    cave->feeling_delay = plr_feeling_delay(cave);
    if (cave->feeling != new_feeling)
    {
        cave->feeling = new_feeling;
        do_cmd_feeling();
        p_ptr->redraw |= PR_DEPTH;
        if (disturb_minor) disturb(0, 0);
    }
}


/*
 * Handle certain things once every 10 game turns
 */
static void _daylight_again(point_t pos, cave_ptr grid) /* ... following me to bed ... */
{
    grid->info |= (CAVE_GLOW | CAVE_AWARE);
    if (view_perma_grids) grid->info |= (CAVE_MARK);
    dun_note_pos(dun_mgr()->surface, pos);
}
static void _night_has_come(point_t pos, cave_ptr grid)
{
    feat_ptr feat = dun_grid_feat_mimic(grid);
    if ( !is_mirror_grid(grid)
      && !have_flag(feat->flags, FF_QUEST_ENTER)
      && !have_flag(feat->flags, FF_ENTRANCE) )
    {
        grid->info &= ~(CAVE_GLOW);
        if (!have_flag(feat->flags, FF_REMEMBER))
        {
            grid->info &= ~(CAVE_MARK);
            dun_note_pos(dun_mgr()->surface, pos);
        }
    }
}
static void _glow_grid(point_t pos, cave_ptr grid)
{
    feat_ptr feat = dun_grid_feat_mimic(grid);
    int      i;

    if (!have_flag(feat->flags, FF_GLOW)) return;
    grid->info |= CAVE_GLOW;

    if (!dun_pos_interior(cave, pos)) return;
    for (i = 0; i < 8; i++)
    {
        point_t p = point_step(pos, ddd[i]);
        cave_at(p)->info |= CAVE_GLOW;
    }
}
void process_world(void)
{
    dun_mgr_ptr dm = dun_mgr();
    int day, hour, min;
    const s32b A_DAY = TURNS_PER_TICK * TOWN_DAWN;
    s32b prev_turn_in_today = ((dm->turn - TURNS_PER_TICK) % A_DAY + A_DAY / 4) % A_DAY;
    int prev_min = (1440 * prev_turn_in_today / A_DAY) % 60;

    extract_day_hour_min(&day, &hour, &min);

    /* Update dungeon feeling, and announce it if changed */
    if (cave->dun_id == p_ptr->dun_id) update_dungeon_feeling();

    /* Every 10 game turns */
    if (dm->turn % TURNS_PER_TICK) return;

    /* Hack -- Check for creature regeneration */
    if (!(dm->turn % (TURNS_PER_TICK*10))) regen_monsters();
    if (!(dm->turn % (TURNS_PER_TICK*3))) regen_captured_monsters();

    /* everything below requires player presence */
    if (cave->dun_id != p_ptr->dun_id) return;

    /*** Attempt timed autosave ***/
    if (autosave_t && autosave_freq)
    {
        if (!(dm->turn % ((s32b)autosave_freq * TURNS_PER_TICK)))
            do_cmd_save_game(TRUE);
    }

    if (mon_fight && !ignore_unview)
    {
        msg_print("You hear noise.");
    }

    /* Date changes */
    if (!hour && !min)
    {
        if (min != prev_min)
            determine_today_mon();
    }

    /*** Handle the wilderness/town (sunrise and set) ***/
    if (dm->surface)
    {
        if (!(dm->turn % ((TURNS_PER_TICK * TOWN_DAWN) / 2)))
        {
            bool dawn = !(dm->turn % (TURNS_PER_TICK * TOWN_DAWN));

            if (dawn)
            {
                if (p_ptr->dun_id == dm->surface->dun_id)
                    msg_print("The sun has risen.");
                dun_iter_grids(dm->surface, _daylight_again);
            }
            else
            {
                if (p_ptr->dun_id == dm->surface->dun_id)
                    msg_print("The sun has fallen.");
                dun_iter_grids(dm->surface, _night_has_come);
                dun_iter_grids(dm->surface, _glow_grid);
            }

            p_ptr->update |= (PU_VIEW | PU_LITE | PU_MON_LITE | PU_MONSTERS);
            p_ptr->redraw |= (PR_MAP);
            p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);

            if (plr_on_surface() && (p_ptr->special_defense & NINJA_S_STEALTH))
            {
                if (cave_at(p_ptr->pos)->info & CAVE_GLOW) set_superstealth(FALSE);
            }
        }
    }

    /* Check for creature generation. */
    if (p_ptr->action == ACTION_GLITTER && one_in_(50))
        ring_summon_ring_bearer();

    /*** Check the Food, and Regenerate ***/
    /* Digest quickly when gorged */
    if (p_ptr->food >= PY_FOOD_MAX)
    {
        /* Digest a lot of food */
        (void)set_food(p_ptr->food - 100);
    }

    /* Digest normally -- Every 50 game turns */
    else if (!(dm->turn % (TURNS_PER_TICK*5)))
    {
        /* Basic digestion rate based on speed */
        int digestion = SPEED_TO_ENERGY(p_ptr->pspeed);

        /* Regeneration takes more food */
        if (p_ptr->regen > 100)
            digestion += 10*(p_ptr->regen-100)/100;
        if (p_ptr->special_defense & (KAMAE_MASK | KATA_MASK))
            digestion += 20;
        if (p_ptr->cursed & OFC_FAST_DIGEST)
            digestion += 30;

        /* Slow digestion takes less food */
        if (p_ptr->slow_digest)
            digestion /= 2;

        /* Temperance slows digestion */
        digestion = digestion * (375 - virtue_current(VIRTUE_TEMPERANCE)) / 375;

        /* Minimal digestion */
        if (digestion < 1) digestion = 1;
        /* Maximal digestion */
        if (digestion > 100) digestion = 100;

        /* Digest some food */
        (void)set_food(p_ptr->food - digestion);
    }


    /* Getting Faint */
    if ((p_ptr->food < PY_FOOD_FAINT))
    {
        /* Faint occasionally */
        if (!plr_tim_find(T_PARALYZED) && (randint0(100) < 10))
        {
            /* Message */
            msg_print("You faint from the lack of food.");

            disturb(1, 0);

            /* Hack -- faint (bypass free action) */
            plr_tim_add(T_PARALYZED, randint1(4));
        }

        /* Starve to death (slowly) */
        if (p_ptr->food < PY_FOOD_STARVE)
        {
            /* Calculate damage */
            int dam = (PY_FOOD_STARVE - p_ptr->food) / 10;

            /* Take damage */
            if (!plr_tim_find(T_INVULN)) take_hit(DAMAGE_LOSELIFE, dam, "starvation");
        }
    }

    /* Process timed damage and regeneration */
    process_world_aux_hp_and_sp();

    /* Process timeout */
    process_world_aux_timeout();

    /* Process light */
    process_world_aux_light();

    /* Process mutation effects */
    mut_process();

    /* Process curse effects */
    process_world_aux_curse();

    /* Process recharging */
    process_world_aux_recharge();

    /* Feel the inventory */
    sense_inventory1();
    sense_inventory2();

    plr_hook_process_world();
}



/*
 * Verify use of "wizard" mode
 */
static bool enter_wizard_mode(void)
{
    /* Ask first time */
    if (!p_ptr->noscore)
    {
        /* Wizard mode is not permitted */
        if (!allow_debug_opts || arg_wizard)
        {
            msg_print("Wizard mode is not permitted.");
            return FALSE;
        }
        else
        {
#ifndef ALLOW_WIZARD
            msg_print("Wizard mode is only permitted in special builds (#define ALLOW_WIZARD in z-config.h).");
            return FALSE;
#endif
        }

        /* Mention effects */
        msg_print("Wizard mode is for debugging and experimenting.");
        msg_print("The game will not be scored if you enter wizard mode.");
        if (!get_check("Are you sure you want to enter wizard mode? "))
        {
            return (FALSE);
        }

        /* Mark savefile */
        p_ptr->noscore |= 0x0002;
    }

    /* Success */
    return (TRUE);
}


#ifdef ALLOW_WIZARD

/*
 * Verify use of "debug" commands
 */
static bool enter_debug_mode(void)
{
    /* Ask first time */
    if (!p_ptr->noscore)
    {
        /* Debug mode is not permitted */
        if (!allow_debug_opts)
        {
            msg_print("Use of debug command is not permitted.");
            return FALSE;
        }

        /* Mention effects */
        msg_print("The debug commands are for debugging and experimenting.");
        msg_print("The game will not be scored if you use debug commands.");
        if (!get_check("Are you sure you want to use debug commands? "))
        {
            return (FALSE);
        }

        /* Mark savefile */
        p_ptr->noscore |= 0x0008;
    }

    /* Success */
    return (TRUE);
}

/*
 * Hack -- Declare the Debug Routines
 */
extern void do_cmd_debug(void);

#endif /* ALLOW_WIZARD */



/*
 * Parse and execute the current command
 * Give "Warning" on illegal commands.
 *
 * XXX XXX XXX Make some "blocks"
 */
static void _dispatch_command(int old_now_turn)
{
    switch (command_cmd)
    {
        /* Ignore */
        case ' ':
        case '\r':
        case '\n':
        case ESCAPE:
            break;

        /*** Wizard Commands ***/

        /* Toggle Wizard Mode */
        case KTRL('W'):
        {
            if (p_ptr->wizard)
            {
                p_ptr->wizard = FALSE;
                msg_print("Wizard mode off.");

            }
            else if (enter_wizard_mode())
            {
                p_ptr->wizard = TRUE;
                msg_print("Wizard mode on.");

            }

            /* Update monsters */
            p_ptr->update |= PU_MONSTERS;
            p_ptr->redraw |= PR_EFFECTS;
            break;
        }


#ifdef ALLOW_WIZARD

        /* Special "debug" commands */
        case KTRL('A'):
        {
            /* Enter debug mode */
            if (enter_debug_mode())
            {
                do_cmd_debug();
            }
            break;
        }

#endif /* ALLOW_WIZARD */


        /*** Inventory Commands ***/

        /* Wear/wield equipment */
        case 'w':
        {
            equip_wield_ui();
            break;
        }

        /* Take off equipment */
        case 't':
        {
            equip_takeoff_ui();
            break;
        }

        /* Drop an item */
        case 'd':
        {
            do_cmd_drop();
            break;
        }

        /* Destroy an item */
        case 'k':
        {
            obj_destroy_ui();
            break;
        }

        /* Equipment list */
        case 'e':
        {
            equip_ui();
            break;
        }

        /* Inventory list */
        case 'i':
        {
            pack_ui();
            break;
        }


        /*** Various commands ***/

        /* Identify an object */
        case 'I':
        {
            obj_inspect_ui();
            break;
        }

        /* Hack -- toggle windows */
        case KTRL('I'):
        {
            toggle_inven_equip();
            toggle_mon_obj_lists();
            break;
        }


        /*** Standard "Movement" Commands ***/

        /* Alter a grid */
        case '+':
        {
            do_cmd_alter();
            break;
        }

        /* Dig a tunnel */
        case 'T':
        {
            do_cmd_tunnel();
            break;
        }

        /* Move (usually pick up things) */
        case ';':
        {
#ifdef ALLOW_EASY_DISARM /* TNB */

            do_cmd_walk(FALSE);

#else /* ALLOW_EASY_DISARM -- TNB */

            do_cmd_walk(always_pickup);

#endif /* ALLOW_EASY_DISARM -- TNB */
            break;
        }

        /* Move (usually do not pick up) */
        case '-':
        {
#ifdef ALLOW_EASY_DISARM /* TNB */

            do_cmd_walk(TRUE);

#else /* ALLOW_EASY_DISARM -- TNB */

            do_cmd_walk(!always_pickup);

#endif /* ALLOW_EASY_DISARM -- TNB */

            break;
        }


        /*** Running, Resting, Searching, Staying */

        /* Begin Running -- Arg is Max Distance */
        case '.':
        {
            do_cmd_run();
            break;
        }

        /* Stay still (usually pick things up) */
        case ',':
        {
            do_cmd_stay(always_pickup);
            break;
        }

        case 'g':
        {
            do_cmd_get();
            break;
        }

        case KTRL('G'):
        {
            do_cmd_autoget();
            break;
        }
        /* Rest -- Arg is time */
        case 'R':
        {
            do_cmd_rest();
            break;
        }

        /* Search for traps/doors */
        case 's':
        {
            do_cmd_search();
            break;
        }

        /* Toggle search mode */
        case 'S':
        {
            if (p_ptr->action == ACTION_SEARCH) set_action(ACTION_NONE);
            else set_action(ACTION_SEARCH);
            break;
        }


        /*** Stairs and Doors and Chests and Traps ***/


        /* Enter quest level -KMW- */
        case SPECIAL_KEY_QUEST:
        {
            do_cmd_quest();
            break;
        }

        /* Go up staircase */
        case '<':
        {
            if (!plr_on_surface())
                do_cmd_go_up();
            break;
        }

        /* Go down staircase */
        case '>':
        {
            do_cmd_go_down();
            break;
        }

        /* Open a door or chest */
        case 'o':
        {
            do_cmd_open();
            break;
        }

        /* Close a door */
        case 'c':
        {
            do_cmd_close();
            break;
        }

        /* Jam a door with spikes */
        case 'j':
        {
            do_cmd_spike();
            break;
        }

        /* Bash a door */
        case 'B':
        {
            do_cmd_bash();
            break;
        }

        /* Disarm a trap or chest */
        case 'D':
        {
            do_cmd_disarm();
            break;
        }


        /*** Magic and Prayers ***/

        /* Gain new spells/prayers */
        case 'G':
        {
            if (p_ptr->pclass == CLASS_SORCERER || p_ptr->pclass == CLASS_RED_MAGE)
                msg_print("You don't have to learn spells!");
            else if (p_ptr->pclass == CLASS_SKILLMASTER)
                skillmaster_gain_skill();
            else if (p_ptr->pclass == CLASS_SAMURAI)
                samurai_gain_spell();
            else if (p_ptr->pclass == CLASS_RAGE_MAGE)
                rage_mage_gain_spell();
            else if (p_ptr->pclass == CLASS_MAGIC_EATER)
                magic_eater_gain();
            else if (p_ptr->pclass == CLASS_GRAY_MAGE)
                gray_mage_gain_spell();
            else if (p_ptr->pclass == CLASS_PSION)
            {
                msg_print("You can only gain spells at certain levels.");
            }
            else
                do_cmd_study();
            break;
        }

        /* Browse a book */
        case 'b':
        {
            if (p_ptr->prace == RACE_MON_RING)
                ring_browse();
            else if (p_ptr->pclass == CLASS_MAGIC_EATER)
                magic_eater_browse();
            else if (p_ptr->pclass == CLASS_RAGE_MAGE)
                rage_mage_browse_spell();
            else if (p_ptr->pclass == CLASS_SAMURAI)
                samurai_browse_spell();
            else if (p_ptr->pclass == CLASS_SKILLMASTER)
                skillmaster_browse();
            else if (p_ptr->pclass == CLASS_GRAY_MAGE)
                gray_mage_browse();
            else if (p_ptr->pclass == CLASS_ARCHAEOLOGIST ||
                     p_ptr->pclass == CLASS_DUELIST ||
                     p_ptr->pclass == CLASS_WARLOCK ||
                     p_ptr->pclass == CLASS_PSION ||
                     p_ptr->pclass == CLASS_BLOOD_KNIGHT ||
                     p_ptr->pclass == CLASS_MINDCRAFTER ||
                     p_ptr->pclass == CLASS_MIRROR_MASTER ||
                     p_ptr->pclass == CLASS_MONSTER ||
                     p_ptr->pclass == CLASS_NINJA ||
                     p_ptr->pclass == CLASS_RUNE_KNIGHT ||
                     p_ptr->pclass == CLASS_WILD_TALENT ||
                     p_ptr->pclass == CLASS_WEAPONMASTER ||
                     p_ptr->pclass == CLASS_DEVICEMASTER ||
                     p_ptr->pclass == CLASS_SCOUT ||
                     p_ptr->pclass == CLASS_MAULER ||
                     p_ptr->pclass == CLASS_MYSTIC ||
                     p_ptr->pclass == CLASS_SNIPER ||
                     p_ptr->pclass == CLASS_TIME_LORD )
            {
                /* This is the preferred entry point ... I'm still working on
                   coverting everything else */
                do_cmd_spell_browse();
            }
            else do_cmd_browse();
            break;
        }

        /* Cast a spell */
        case 'm':
            /* -KMW- */
            if (p_ptr->pclass == CLASS_WARRIOR || p_ptr->pclass == CLASS_ARCHER || p_ptr->pclass == CLASS_CAVALRY)
            {
                msg_print("You cannot cast spells!");
            }
            else if (plr_tim_find(T_NO_SPELLS))
            {
                msg_print("Your spells are blocked!");
                /*energy_use = 100;*/
            }
            else if (!fear_allow_magic())
            {
                msg_print("You are too scared!");
                energy_use = 100;
            }
            else if ( cave->dun_lvl && (cave->flags & DF_NO_MAGIC)
                   && p_ptr->pclass != CLASS_BLOOD_KNIGHT
                   && p_ptr->pclass != CLASS_WEAPONMASTER
                   && p_ptr->pclass != CLASS_MAULER
                   && p_ptr->prace  != RACE_MON_POSSESSOR
                   && p_ptr->prace  != RACE_MON_MIMIC)
            {
                msg_print("The dungeon absorbs all attempted magic!");
                msg_print(NULL);
            }
            else if ( p_ptr->anti_magic
                   && p_ptr->pclass != CLASS_BLOOD_KNIGHT
                   && p_ptr->pclass != CLASS_WEAPONMASTER
                   && p_ptr->pclass != CLASS_MAULER
                   && p_ptr->prace  != RACE_MON_POSSESSOR
                   && p_ptr->prace  != RACE_MON_MIMIC)
            {
                cptr which_power = "magic";
                if (p_ptr->pclass == CLASS_MINDCRAFTER || p_ptr->pclass == CLASS_PSION)
                    which_power = "psionic powers";
                else if (p_ptr->pclass == CLASS_SAMURAI)
                    which_power = "hissatsu";
                else if (p_ptr->pclass == CLASS_MIRROR_MASTER)
                    which_power = "mirror magic";
                else if (p_ptr->pclass == CLASS_NINJA)
                    which_power = "ninjutsu";
                else if (mp_ptr->spell_book == TV_LIFE_BOOK)
                    which_power = "prayer";
                else if (mp_ptr->spell_book == TV_RAGE_BOOK)
                    which_power = "rage";

                msg_format("An anti-magic shell disrupts your %s!", which_power);
                equip_learn_flag(OF_NO_MAGIC);
                energy_use = 0;
            }
            else if (plr_tim_find(T_BERSERK) && p_ptr->pclass != CLASS_BLOOD_KNIGHT && p_ptr->pclass != CLASS_RAGE_MAGE)
            {
                msg_format("You cannot think clearly!");
                energy_use = 0;
                flush(); /* XXX Macro 'mdd' while T_BERSERK will drop your best spellbook! */
            }
            else
            {
                if (p_ptr->prace == RACE_MON_RING)
                    ring_cast();
                else if (p_ptr->prace == RACE_MON_POSSESSOR || p_ptr->prace == RACE_MON_MIMIC)
                    possessor_cast();
                else if (p_ptr->pclass == CLASS_MAGIC_EATER)
                    magic_eater_cast(0);
                else if (p_ptr->pclass == CLASS_SKILLMASTER)
                    skillmaster_cast();
                else if (p_ptr->pclass == CLASS_GRAY_MAGE)
                    gray_mage_cast();
                else if (p_ptr->pclass == CLASS_BLUE_MAGE)
                    blue_mage_cast();
                else if (p_ptr->pclass == CLASS_ARCHAEOLOGIST ||
                            p_ptr->pclass == CLASS_DUELIST ||
                            p_ptr->pclass == CLASS_WARLOCK ||
                            p_ptr->pclass == CLASS_BLOOD_KNIGHT ||
                            p_ptr->pclass == CLASS_MINDCRAFTER ||
                            p_ptr->pclass == CLASS_MIRROR_MASTER ||
                            p_ptr->pclass == CLASS_MONSTER ||
                            p_ptr->pclass == CLASS_NINJA ||
                            p_ptr->pclass == CLASS_PSION ||
                            p_ptr->pclass == CLASS_RUNE_KNIGHT ||
                            p_ptr->pclass == CLASS_WILD_TALENT ||
                            p_ptr->pclass == CLASS_WEAPONMASTER ||
                            p_ptr->pclass == CLASS_DEVICEMASTER ||
                            p_ptr->pclass == CLASS_RAGE_MAGE ||
                            p_ptr->pclass == CLASS_SAMURAI ||
                            p_ptr->pclass == CLASS_SCOUT ||
                            p_ptr->pclass == CLASS_MAULER ||
                            p_ptr->pclass == CLASS_MYSTIC ||
                            p_ptr->pclass == CLASS_PSION ||
                            p_ptr->pclass == CLASS_SNIPER ||
                            p_ptr->pclass == CLASS_TIME_LORD )
                {
                    /* This is the preferred entrypoint for spells ...
                        I'm still working on coverting everything else */
                    do_cmd_spell();
                }
                else
                    do_cmd_cast();
            }
            break;

        /* Issue a pet command */
        case 'p':
            do_cmd_pet();
            break;

        /*** Use various objects ***/

        /* Inscribe an object */
        case '{':
        {
            obj_inscribe_ui();
            break;
        }

        /* Uninscribe an object */
        case '}':
        {
            obj_uninscribe_ui();
            break;
        }

        /* Activate an artifact */
        case 'A':
        {
            do_cmd_activate();
            break;
        }

        /* Eat some food */
        case 'E':
        {
            do_cmd_eat_food();
            break;
        }

        /* Fuel your lantern/torch */
        case 'F':
        {
            do_cmd_refill();
            break;
        }

        /* Fire an item */
        case 'f':
        {
            do_cmd_fire();
            break;
        }

        /* Throw an item */
        case 'v':
        {
            plr_throw_t context = {0};
            plr_throw(&context);
            break;
        }

        /* Aim a wand */
        case 'a':
        {
            do_cmd_aim_wand();
            break;
        }

        /* Zap a rod */
        case 'z':
        {
            do_cmd_zap_rod();
            break;
        }

        /* Quaff a potion */
        case 'q':
        {
            do_cmd_quaff_potion();
            break;
        }

        /* Read a scroll */
        case 'r':
        {
            do_cmd_read_scroll();
            break;
        }

        /* Use a staff */
        case 'u':
        {
            do_cmd_use_staff();
            break;
        }

        /* Use racial power */
        case 'U':
        {
            if (!fear_allow_magic())
            {
                msg_print("You are too scared!");
                energy_use = 100;
            }
            else
                do_cmd_power();
            break;
        }


        /*** Looking at Things (nearby or on map) ***/

        /* Full dungeon map */
        case 'M':
            do_cmd_view_map();
            break;

        case KTRL('V'):
            viewport_verify_aux(p_ptr->pos, VIEWPORT_FORCE_CENTER);
            break;

        /* Locate player on map */
        case 'L':
            do_cmd_locate();
            break;

        /* Look around */
        case 'l':
        {
            do_cmd_look();
            break;
        }

        case '[':
            if (!plr_tim_find(T_HALLUCINATE))
                do_cmd_list_monsters(MON_LIST_NORMAL);
            break;

        case ']':
            if (!plr_tim_find(T_HALLUCINATE))
                do_cmd_list_objects();
            break;

        /* Target monster or location */
        case '*':
        {
            do_cmd_target();
            break;
        }



        /*** Help and Such ***/

        /* Help */
        case '?':
        {
            do_cmd_help();
            break;
        }

        /* Identify symbol */
        case '/':
        {
            do_cmd_query_symbol();
            break;
        }

        /* Character description */
        case 'C':
        {
            plr_display();
            /*do_cmd_change_name();*/
            break;
        }


        /*** System Commands ***/

        /* Single line from a pref file */
        case '!':
        {
            do_cmd_pref();
            break;
        }

        case '$':
        {
            do_cmd_reload_autopick();
            break;
        }

        case '_':
        {
            do_cmd_edit_autopick();
            break;
        }

        /* Interact with macros */
        case '@':
        {
            do_cmd_macros();
            break;
        }

        /* Interact with visuals */
        case '%':
        {
            do_cmd_visuals();
            do_cmd_redraw();
            break;
        }

        /* Interact with colors */
        case '&':
        {
            do_cmd_colors();
            do_cmd_redraw();
            break;
        }

        /* Interact with options */
        case '=':
        {
            do_cmd_options();
            do_cmd_redraw();
            break;
        }

        /*** Misc Commands ***/

        /* Take notes */
        case ':':
        {
            do_cmd_note();
            break;
        }

        /* Version info */
        case 'V':
        {
            do_cmd_version();
            break;
        }

        /* Repeat level feeling */
        case KTRL('F'):
        {
            do_cmd_feeling();
            break;
        }

        /* Show previous messages */
        case KTRL('P'):
        {
            do_cmd_messages(old_now_turn);
            break;
        }

        /* Show quest status -KMW- */
        case KTRL('Q'):
        {
            quests_display();
            break;
        }

        /* Redraw the screen */
        case KTRL('R'):
        {
            now_turn = old_now_turn;
            do_cmd_redraw();
            break;
        }

#ifndef VERIFY_SAVEFILE

        /* Hack -- Save and don't quit */
        case KTRL('S'):
        {
            do_cmd_save_game(FALSE);
            break;
        }

#endif /* VERIFY_SAVEFILE */

        case KTRL('T'):
        {
            do_cmd_time();
            break;
        }

        /* Save and quit */
        case KTRL('X'):
        case SPECIAL_KEY_QUIT:
        {
            do_cmd_save_and_exit();
            break;
        }

        /* Quit (commit suicide) */
        case 'Q':
        {
            do_cmd_suicide();
            break;
        }

        /* Check artifacts, uniques, objects */
        case '~':
        {
            do_cmd_knowledge();
            break;
        }

        /* Save "screen dump" */
        case ')':
        {
            do_cmd_save_screen();
            break;
        }

        case '`':
        {
            do_cmd_travel();
            break;
        }

        /* Hack -- Unknown command */
        default:
        {
            if (flush_failure) flush();
            if (one_in_(2))
            {
                char error_m[1024];
                sound(SOUND_ILLEGAL);
                if (get_rnd_line("error.txt", 0, error_m) == ERROR_SUCCESS)
                    msg_print(error_m);
                else
                    msg_print("Unknown command. Type <color:y>?</color> for help.");
            }
            else
                msg_print("Unknown command. Type <color:y>?</color> for help.");

            break;
        }
    }
}

static void process_command(void)
{
    int old_now_turn = now_turn;

#ifdef ALLOW_REPEAT /* TNB */

    /* Handle repeating the last command */
    repeat_check(FALSE);

#endif /* ALLOW_REPEAT -- TNB */

    now_turn = dun_mgr()->turn;
    msg_boundary();

    if (p_ptr->pclass == CLASS_SNIPER && p_ptr->concent)
        reset_concent = TRUE;

    switch (command_cmd)
    {
    case SPECIAL_KEY_STORE: {
        cave_type *c_ptr = cave_at(p_ptr->pos);

        if (cave_have_flag_grid(c_ptr, FF_STORE))
        {
            int which = f_info[c_ptr->feat].subtype;

            if (which == SHOP_HOME) home_ui();
            else if (which == SHOP_MUSEUM) museum_ui();
            else
            {
                town_ptr town = towns_current_town();
                if (!town)
                    msg_print("The shop is closed!"); /* XXX bug on D_SURFACE for ROOM_WILDERNESS */
                else
                {
                    shop_ptr shop = town_get_shop(town, which);
                    shop_ui(shop);
                }
            }
        }
        break; }
    case SPECIAL_KEY_BUILDING: {
        cave_type *c_ptr = cave_at(p_ptr->pos);

        if (cave_have_flag_grid(c_ptr, FF_BLDG))
        {
            int which = f_info[c_ptr->feat].subtype;
            town_ptr town = towns_current_town();
            bldg_ptr bldg = town_get_bldg(town, which);

            bldg_ui(bldg);
        }
        break; }
    default:
        pack_lock();
        _dispatch_command(old_now_turn);
        pack_unlock();
    }

    if (!energy_use)
        now_turn = old_now_turn;
}

static void _shimmer_aux(int id, mon_ptr mon)
{
    if (mon->ml)
    {
        mon_race_ptr race = mon_apparent_race(mon);
        if (race->flags1 & (RF1_ATTR_MULTI | RF1_SHAPECHANGER))
        {
            shimmer_monsters = TRUE;
            lite_pos(mon->pos);
        }
    }
}

static void _repair_aux(int id, mon_ptr mon)
{
    if (mon->mflag2 & MFLAG2_MARK)
    {
        if (mon->mflag2 & MFLAG2_SHOW)
        {
            mon->mflag2 &= ~MFLAG2_SHOW;
            repair_monsters = TRUE;
        }
        else
        {
            mon->mflag2 &= ~MFLAG2_MARK;
            mon->ml = FALSE;
            update_mon(mon, FALSE);
            check_mon_health_redraw(mon->id);
            lite_pos(mon->pos);
        }
    }
}

/*
 * Process the player
 *
 * Notice the annoying code to handle "pack overflow", which
 * must come first just in case somebody manages to corrupt
 * the savefiles by clever use of menu commands or something.
 */
void process_player(void)
{
    /* Give the player some energy */
    if (!(load && p_ptr->energy_need <= 0))
        p_ptr->energy_need -= SPEED_TO_ENERGY(p_ptr->pspeed);

    /* No turn yet */
    if (p_ptr->energy_need > 0) return;
    if (!command_rep) prt_time();

    /*** Check for interupts ***/

    /* Complete resting */
    if (resting < 0)
    {
        /* Basic resting */
        if (resting == -1)
        {
            /* Stop resting */
            if ( (p_ptr->chp == p_ptr->mhp || mimic_no_regen())
              && ( p_ptr->csp >= p_ptr->msp
                || p_ptr->pclass == CLASS_RUNE_KNIGHT
                || p_ptr->pclass == CLASS_RAGE_MAGE
                || mimic_no_regen() )
              && !magic_eater_can_regen() )
            {
                set_action(ACTION_NONE);
            }
        }

        /* Complete resting */
        else if (resting == -2)
        {
            /* Stop resting */
            if ( (p_ptr->chp == p_ptr->mhp || mimic_no_regen())
              && ( p_ptr->csp >= p_ptr->msp
                || p_ptr->pclass == CLASS_RUNE_KNIGHT
                || p_ptr->pclass == CLASS_RAGE_MAGE
                || mimic_no_regen() )
              && !magic_eater_can_regen()
              && !plr_tim_find(T_BLIND)
              && !plr_tim_find(T_CONFUSED)
              && !plr_tim_find(T_POISON)
              && !p_ptr->afraid
              && !plr_tim_find(T_STUN)
              && !plr_tim_find(T_CUT)
              && !plr_tim_find(T_SLOW)
              && !plr_tim_find(T_PARALYZED)
              && !plr_tim_find(T_HALLUCINATE))
            {
                set_action(ACTION_NONE);
            }
        }
    }

    /* Handle "abort" */
    if (check_abort)
    {
        /* Check for "player abort" (semi-efficiently for resting) */
        if ( running
          || travel.run
          || command_rep
          || p_ptr->action == ACTION_REST
          || p_ptr->action == ACTION_GLITTER )
        {
            /* Do not wait */
            inkey_scan = TRUE;

            /* Check for a key */
            if (inkey())
            {
                /* Flush input */
                flush();

                /* Disturb */
                disturb(0, 0);

                /* Hack -- Show a Message */
                msg_print("Canceled.");

            }
        }
    }

    if (p_ptr->riding && !plr_tim_find(T_CONFUSED) && !plr_tim_find(T_BLIND))
    {
        monster_type *m_ptr = dun_mon(cave, p_ptr->riding);
        monster_race *r_ptr = mon_race(m_ptr);

        if (mon_tim_find(m_ptr, MT_SLEEP))
        {
            char m_name[80];
            mon_tim_delete(m_ptr, MT_SLEEP); /* spurs? */
            monster_desc(m_name, m_ptr, 0);
            msg_format("You have waked %s up.", m_name);
        }

        if (mon_tim_find(m_ptr, T_STUN))
        {
            if (randint0(r_ptr->level < skills_riding_current()))
                mon_tim_remove(m_ptr, T_STUN);
            else
                mon_tim_subtract(m_ptr, T_STUN, 1);
        }

        if (mon_tim_find(m_ptr, T_CONFUSED))
        {
            if (randint0(r_ptr->level < skills_riding_current()))
                mon_tim_remove(m_ptr, T_CONFUSED);
            else
                mon_tim_subtract(m_ptr, T_CONFUSED, 1);
        }

        if (mon_tim_find(m_ptr, T_FEAR))
        {
            if (randint0(r_ptr->level < skills_riding_current()))
                mon_tim_remove(m_ptr, T_FEAR);
            else
                mon_tim_subtract(m_ptr, T_FEAR, 1);
        }

        /* Handle "p_ptr->update" and "p_ptr->redraw" and "p_ptr->window" */
        handle_stuff();
    }

    if (!load) bard_check_music();
    if (!load) check_hex();
    if (!load) revenge_spell();
    if (!load) plr_hook_process_player();
    if (load) equip_on_load();

    load = FALSE;

    if ((p_ptr->pclass == CLASS_FORCETRAINER) && (p_ptr->magic_num1[0]))
    {
        if (p_ptr->magic_num1[0] < 40)
        {
            p_ptr->magic_num1[0] = 0;
        }
        else p_ptr->magic_num1[0] -= 40;
        p_ptr->update |= PU_BONUS;
        p_ptr->redraw |= PR_EFFECTS;
    }
    if (p_ptr->action == ACTION_LEARN)
    {
        s32b cost = 0L;
        u32b cost_frac = (p_ptr->msp + 30L) * 256L;

        /* Convert the unit (1/2^16) to (1/2^32) */
        s64b_LSHIFT(cost, cost_frac, 16);


        if (s64b_cmp(p_ptr->csp, p_ptr->csp_frac, cost, cost_frac) < 0)
        {
            /* Mana run out */
            p_ptr->csp = 0;
            p_ptr->csp_frac = 0;
            set_action(ACTION_NONE);
        }
        else
        {
            /* Reduce mana */
            s64b_sub(&(p_ptr->csp), &(p_ptr->csp_frac), cost, cost_frac);
        }
        p_ptr->redraw |= PR_MANA;
    }

    if (p_ptr->special_defense & KATA_MASK)
    {
        if (p_ptr->special_defense & KATA_MUSOU)
        {
            if (p_ptr->csp < 3)
            {
                set_action(ACTION_NONE);
            }
            else
            {
                p_ptr->csp -= 2;
                p_ptr->redraw |= (PR_MANA);
            }
        }
    }

    fear_recover_p();

    /*** Handle actual user input ***/

    /* Repeat until out of energy */
    while (p_ptr->energy_need <= 0)
    {
        p_ptr->sutemi = FALSE;
        p_ptr->counter = FALSE;

        p_ptr->turn++;

        /* Handle "p_ptr->notice" */
        notice_stuff();

        /* Handle "p_ptr->update" and "p_ptr->redraw" and "p_ptr->window" */
        handle_stuff();

        /* Place the cursor on the player */
        move_cursor_relative(p_ptr->pos);

        /* Refresh (optional) */
        if (fresh_before) Term_fresh();


        /* Hack -- Pack Overflow */
        pack_overflow();


        /* Hack -- cancel "lurking browse mode" */
        if (!command_new) command_see = FALSE;


        /* Assume free turn */
        energy_use = 0;

        if (plr_tim_find(T_PARALYZED))
        {
            energy_use = 100;
        }
        else if (plr_tim_amount(T_STUN) >= STUN_KNOCKED_OUT)
        {
            energy_use = 100;
            plr_tim_subtract(T_STUN, 25);
        }
        else if (p_ptr->action == ACTION_REST)
        {
            /* Timed rest */
            if (resting > 0)
            {
                /* Reduce rest count */
                resting--;

                if (!resting) set_action(ACTION_NONE);

                /* Redraw the state */
                p_ptr->redraw |= (PR_STATE);
            }

            /* Take a turn */
            energy_use = 100;

            if (p_ptr->csp < p_ptr->msp)
            {
                caster_info *caster_ptr = get_caster_info();
                if (caster_ptr && (caster_ptr->options & CASTER_SUPERCHARGE_MANA))
                {
                    msg_boundary();
                    cast_concentration();
                }
                else if (p_ptr->clear_mind)
                {
                    msg_boundary();
                    cast_clear_mind();
                }
            }
        }
        else if (p_ptr->action == ACTION_GLITTER)
        {
            energy_use = 100;
        }

        /* Running */
        else if (running)
        {
            /* Take a step */
            run_step(0);
        }

        /* Traveling */
        else if (travel.run)
        {
            /* Take a step */
            travel_step();
        }

        /* Repeated command */
        else if (command_rep)
        {
            /* Count this execution */
            command_rep--;

            /* Redraw the state */
            p_ptr->redraw |= (PR_STATE);

            /* Redraw stuff */
            redraw_stuff();

            /* Hack -- Assume messages were seen 
            msg_line_clear(); */

            /* Process the command */
            process_command();
        }

        /* Normal command */
        else
        {
            /* Place the cursor on the player */
            move_cursor_relative(p_ptr->pos);

            can_save = TRUE;
            /* Get a command (normal) */
            request_command(FALSE);
            can_save = FALSE;

            /* Process the command */
            process_command();
        }

        /* Hack -- Pack Overflow */
        pack_overflow();


        /*** Clean up ***/

        /* Significant */
        if (energy_use)
            plr_hook_player_action(); /* classes may grant a free move */
        if (energy_use)
        {
            plr_tim_fast_tick();  /* cf _poison_tick() if you move this */
            if (p_ptr->clp > 1000)
            {
                int x = p_ptr->clp - 1000;
                int dec = 1 + x/25 + (x/100)*(x/100);
                p_ptr->clp -= dec;
                if (p_ptr->clp <= 1000)
                {
                    p_ptr->clp = 1000; /* paranoia: assert(dec <= x); */
                    msg_print("You return to normal power.");
                }
                p_ptr->update |= PU_HP;
                p_ptr->redraw |= PR_EFFECTS;
            }

            if (world_player || energy_use > 400)
            {
                /* The Randomness is irrelevant */
                p_ptr->energy_need += energy_use * TURNS_PER_TICK / 10;
            }
            else
            {
                int amt = (s16b)((s32b)energy_use * ENERGY_NEED() / 100L);
                if (p_ptr->wizard || 0)
                {
                    rect_t r = ui_char_info_rect();
                    c_put_str(TERM_WHITE, format("E:%3d/%3d", amt, energy_use), r.y + r.cy - 2, r.x);
                }
                p_ptr->energy_need += amt;
            }

            /* Hack -- constant hallucination */
            if (plr_tim_find(T_HALLUCINATE)) p_ptr->redraw |= (PR_MAP);

            /* Shimmer monsters if needed */
            if (shimmer_monsters)
            {
                shimmer_monsters = FALSE;
                dun_iter_mon(cave, _shimmer_aux);
            }

            if (randint1(200) < energy_use)
                fear_process_p();

            if (repair_monsters)
            {
                repair_monsters = FALSE;
                dun_iter_mon(cave, _repair_aux);
            }

            if (p_ptr->action == ACTION_LEARN)
            {
                new_mane = FALSE;
                p_ptr->redraw |= (PR_STATE);
            }

            if (world_player && (p_ptr->energy_need > - 1000))
            {
                p_ptr->redraw |= (PR_MAP | PR_STATUS);
                p_ptr->update |= (PU_MONSTERS);
                p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);
                msg_print("You feel time flowing around you once more.");
                msg_print(NULL);
                world_player = FALSE;
                p_ptr->energy_need = ENERGY_NEED();
                handle_stuff();
            }
        }
        else p_ptr->turn--;

        if (!p_ptr->playing || p_ptr->is_dead)
        {
            world_player = FALSE;
            break;
        }

        /* Sniper */
        if (energy_use && reset_concent) reset_concentration(TRUE);

        if (p_ptr->leaving) break;
    }

    /* Update scent trail */
    update_smell();
}


/*
 * Load some "user pref files"
 *
 * Modified by Arcum Dagsson to support
 * separate macro files for different realms.
 */
static void load_all_pref_files(void)
{
    char buf[1024];

    /* Access the "user" pref file */
    sprintf(buf, "user.prf");

    /* Process that file */
    process_pref_file(buf);

    /* Access the "user" system pref file */
    sprintf(buf, "user-%s.prf", ANGBAND_SYS);

    /* Process that file */
    process_pref_file(buf);

    /* Access the "race" pref file */
    sprintf(buf, "%s.prf", get_true_race()->name);

    /* Process that file */
    process_pref_file(buf);

    /* Access the "class" pref file */
    sprintf(buf, "%s.prf", get_class()->name);

    /* Process that file */
    process_pref_file(buf);

    /* Access the "character" pref file */
    sprintf(buf, "%s.prf", player_base);

    /* Process that file */
    process_pref_file(buf);

    /* Access the "realm 1" pref file */
    if (p_ptr->realm1 != REALM_NONE)
    {
        sprintf(buf, "%s.prf", realm_names[p_ptr->realm1]);

        /* Process that file */
        process_pref_file(buf);
    }

    /* Access the "realm 2" pref file */
    if (p_ptr->realm2 != REALM_NONE)
    {
        sprintf(buf, "%s.prf", realm_names[p_ptr->realm2]);

        /* Process that file */
        process_pref_file(buf);
    }


    /* Load an autopick preference file */
    autopick_load_pref(FALSE);
}


/*
 * Extract option variables from bit sets
 */
void extract_option_vars(void)
{
    int i;

    for (i = 0; option_info[i].o_desc; i++)
    {
        int os = option_info[i].o_set;
        int ob = option_info[i].o_bit;

        /* Set the "default" options */
        if (option_info[i].o_var)
        {
            /* Set */
            if (option_flag[os] & (1L << ob))
            {
                /* Set */
                (*option_info[i].o_var) = TRUE;
            }

            /* Clear */
            else
            {
                /* Clear */
                (*option_info[i].o_var) = FALSE;
            }
        }
    }
}

/*
 * Determine today's bounty monster
 */
void determine_today_mon(void)
{
    int max_dl = MAX(3, plr_max_dun_lvl());

    while (1)
    {
        mon_race_ptr race = mon_alloc_choose_aux2(mon_alloc_tbl, max_dl, 0, GMN_NO_UNIQUES);
        if (!race) break;

        if (race->flags7 & (RF7_NAZGUL | RF7_UNIQUE2)) continue;
        if (race->flags2 & RF2_MULTIPLY) continue;
        if ((race->flags9 & (RF9_DROP_CORPSE | RF9_DROP_SKELETON)) != (RF9_DROP_CORPSE | RF9_DROP_SKELETON)) continue;
        if (race->level < MIN(max_dl / 2, 40)) continue;
        if (race->rarity > 10) continue;

        today_mon = race->id;
        break;
    }

    p_ptr->today_mon = 0;
}


/*
 * Actually play a game
 *
 * If the "new_game" parameter is true, then, after loading the
 * savefile, we will commit suicide, if necessary, to allow the
 * player to start a new game.
 */
void play_game(bool new_game)
{
    int i;
    bool load_game = TRUE;

    autosave_l = TRUE;

    /* Hack -- Character is "icky" */
    character_icky = TRUE;

    /* Make sure main term is active */
    Term_activate(angband_term[0]);

    /* Initialise the resize hooks */
    angband_term[0]->resize_hook = resize_map;

    for (i = 1; i < 8; i++)
    {
        /* Does the term exist? */
        if (angband_term[i])
        {
            /* Add the redraw on resize hook */
            angband_term[i]->resize_hook = redraw_window;
        }
    }

    /* The Windows port blocks until the user chooses a menu for a New game, or
       to load an existing game. Thus, it will display its own start screen ... */
    if (strcmp(ANGBAND_SYS, "win") != 0)
    {
        /* On X11, you need to flush() before Term->hgt is accurate! */
        Term_flush();
        display_news();
    }

    /* Hack -- turn off the cursor */
    (void)Term_set_cursor(0);

    /* Attempt to load */
    if (!load_player())
    {
        quit("broken savefile");
    }

    /* Extract the options */
    extract_option_vars();

    creating_savefile = new_game;

    /* Nothing loaded */
    if (!character_loaded)
    {
        /* Make new player */
        new_game = TRUE;

        /* Prepare to init the RNG */
        Rand_quick = TRUE;

    }

    /* Old game is loaded. But new game is requested. */
    else if (new_game)
    {
    }

    /* Process old character */
    if (!new_game)
    {
        /* Process the player name */
        process_player_name(FALSE);
    }

    /* Init the RNG */
    if (Rand_quick)
    {
        u32b seed;

        /* Basic seed */
        seed = (u32b)time(NULL);

#ifdef SET_UID

        /* Mutate the seed on Unix machines */
        seed = ((seed >> 3) * (getpid() << 1));

#endif

        /* Use the complex RNG */
        Rand_quick = FALSE;

        /* Seed the "complex" RNG */
        Rand_state_init(seed);
    }

    /* Roll new character */
    if (new_game)
    {
        /* Hack -- seed for flavors */
        seed_flavor = randint0(0x10000000);

        /* Roll up a new character */
        player_birth();

        counts_write(2,0);
        p_ptr->count = 0;

        load = FALSE;

        determine_today_mon();
    }

    plr_hook_register_timers();
    creating_savefile = FALSE;

    p_ptr->sutemi = FALSE;
    world_monster = FALSE;
    now_turn = dun_mgr()->turn;
    playtime_resume();

    /* TODO: py_skills_init() or some such ... w_max needs to be reset each time you play, 
     * not just on player birth */
    if (p_ptr->pclass == CLASS_WEAPONMASTER && !new_game)
        weaponmaster_adjust_skills();

    /* Flavor the objects */
    flavor_init();

    /* Flush the message */
    Term_fresh();

    /* Hack -- Enter wizard mode */
    if (arg_wizard)
    {
        if (enter_wizard_mode())
            p_ptr->wizard = TRUE;
        else if (p_ptr->is_dead)
            quit("Already dead.");
    }

    /* Initialize the town-buildings if necessary ... user restarted the
     * game while on the surface, possibly even inside a town */
    if (plr_on_surface())
        towns_init_buildings();

    /* Character is now "complete" */
    character_generated = TRUE;

    /* Hack -- Character is no longer "icky" */
    character_icky = FALSE;

    /* Start game */
    p_ptr->playing = TRUE;

    /* Reset the visual mappings */
    reset_visuals();

    /* Load the "pref" files */
    load_all_pref_files();

    Term_xtra(TERM_XTRA_REACT, 0);
    p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_SPELL);
    p_ptr->window |= (PW_MESSAGE | PW_OVERHEAD | PW_DUNGEON | PW_MONSTER_LIST | PW_OBJECT_LIST | PW_MONSTER | PW_OBJECT);
    window_stuff();
    viewport_verify_aux(p_ptr->pos, VIEWPORT_FORCE_CENTER);

    /* Give startup outfit (after loading pref files) */
    if (new_game)
    {
        dun_world_ptr world = dun_worlds_current();
        do_cmd_redraw(); /* needed for the msg_print(NULL) below */
        msg_format("<color:B>Welcome!</color> You have entered <color:r>%s</color>.", world->name);
        msg_boundary();
        msg_print(world->desc);
        msg_print(NULL);
        plr_hook_birth();
    }


    /* Set or clear "rogue_like_commands" if requested */
    if (arg_force_original) rogue_like_commands = FALSE;
    if (arg_force_roguelike) rogue_like_commands = TRUE;

    /* Hack -- Enforce "delayed death" */
    if (p_ptr->chp < 0) p_ptr->is_dead = TRUE;

    if (p_ptr->prace == RACE_ANDROID) android_calc_exp();

    if (new_game && (p_ptr->pclass == CLASS_CAVALRY || p_ptr->pclass == CLASS_BEASTMASTER))
    {
        point_t p = point_step(p_ptr->pos, 4); /* XXX was always hard coded! */
        monster_type *m_ptr;
        int pet_r_idx = ((p_ptr->pclass == CLASS_CAVALRY) ? MON_HORSE : MON_YASE_HORSE);
        monster_race *r_ptr = mon_race_lookup(pet_r_idx);
        m_ptr = place_monster_aux(0, p, pet_r_idx, (PM_FORCE_PET | PM_NO_KAGE));
        if (m_ptr)
        {
            m_ptr->mspeed = r_ptr->speed;
            m_ptr->maxhp = r_ptr->hdice*(r_ptr->hside+1)/2;
            m_ptr->max_maxhp = m_ptr->maxhp;
            m_ptr->hp = r_ptr->hdice*(r_ptr->hside+1)/2;
            m_ptr->energy_need = ENERGY_NEED() + ENERGY_NEED();
        }
    }

    viewport_verify();
    character_xtra = TRUE;
    p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_SPELL | PW_MONSTER_LIST | PW_OBJECT_LIST | PW_MONSTER | PW_OVERHEAD | PW_DUNGEON | PW_WORLD_MAP);
    p_ptr->redraw |= (PR_WIPE | PR_BASIC | PR_EXTRA | PR_EQUIPPY | PR_MSG_LINE);
    p_ptr->redraw |= (PR_MAP);
    p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);
    p_ptr->update |= (PU_VIEW | PU_LITE | PU_MON_LITE | PU_TORCH);
    p_ptr->update |= (PU_MONSTERS | PU_FLOW);
    handle_stuff();
    character_xtra = FALSE;

    p_ptr->update |= (PU_BONUS | PU_INNATE | PU_HP | PU_MANA | PU_SPELLS);
    p_ptr->notice |= (PN_OPTIMIZE_PACK | PN_OPTIMIZE_QUIVER);
    notice_stuff();
    handle_stuff();
    Term_fresh();
    if (!load_game && (p_ptr->special_defense & NINJA_S_STEALTH)) set_superstealth(FALSE);

    /* XXX Zap -Detection then quit game. On restart, player gains telepathy on detected
     * monsters since they never got "repaired". XXX */
    repair_monsters = TRUE;

    /* Process */
    dun_mgr_process();

    /* Close stuff */
    close_game();

    /* Quit */
    quit(NULL);
}

