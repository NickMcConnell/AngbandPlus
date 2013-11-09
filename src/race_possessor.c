#include "angband.h"

static int _calc_level(int l)
{
    return l + l*l*l/2500;
}

static cptr _mon_name(int r_idx)
{
    if (r_idx)
        return r_name + r_info[r_idx].name;
    return ""; /* Birth Menu */
}

static void _birth(void) 
{ 
    object_type forge;

    p_ptr->current_r_idx = MON_POSSESSOR_SOUL;
    equip_on_change_race();
    
    object_prep(&forge, lookup_kind(TV_WAND, SV_WAND_MAGIC_MISSILE));
    forge.number = 1;
    forge.pval = (byte)rand_range(25, 30);
    add_outfit(&forge);

    object_prep(&forge, lookup_kind(TV_RING, SV_RING_DAMAGE));
    forge.to_d = 3;
    add_outfit(&forge);
}

/**********************************************************************
 * Attacks
 * We could either write new attack code, or translate to existing innate
 * attack code (which also has nice display code already written for us!)
 **********************************************************************/
static bool _blow_is_masked(monster_blow *blow_ptr)
{
    switch (blow_ptr->effect)
    {
    case RBE_EAT_LITE:
        return TRUE;
    }

    switch (blow_ptr->method)
    {
    case 0:
    case RBM_EXPLODE:
    case RBM_SHOOT:
        return TRUE;

    case RBM_GAZE:
        if (p_ptr->blind)
            return TRUE;
        break;

    case RBM_HIT:
    case RBM_TOUCH:
    case RBM_PUNCH:
    case RBM_CLAW:
    case RBM_SLASH:
    case RBM_BEG:
        if (!equip_is_valid_hand(0)) /* No hands, so can't be blocked */
            return FALSE;

        if (p_ptr->weapon_ct > 0) /* Wielding a weapon blocks hand based attacks */
            return TRUE;

        if (!equip_find_empty_hand()) /* So does shield, capture ball, etc. */
            return TRUE;

        break;
    }

    return FALSE;
}

static void _calc_innate_attacks(void)
{
    monster_race *r_ptr = &r_info[p_ptr->current_r_idx];
    monster_blow  blows[4] = {0};
    int           cts[4] = {0};
    int           ct = 0;
    int           i, j;

    for (i = 0; i < 4; i++)
    {
        bool dup = FALSE;
        
        if (!r_ptr->blow[i].effect)
            continue;

        /* Look for a duplicate. TODO: Add monster_blow.num so monsters can get more than 4 blows :) */
        for (j = 0; j < ct; j++)
        {
            if ( blows[j].method == r_ptr->blow[i].method
              && blows[j].effect == r_ptr->blow[i].effect
              && blows[j].d_dice == r_ptr->blow[i].d_dice
              && blows[j].d_side == r_ptr->blow[i].d_side )
            {
                cts[j]++;
                dup = TRUE;
            }
        }
        if (!dup)
        {
            blows[ct] = r_ptr->blow[i];            
            cts[ct] = 1;
            ct++;
        }
    }

    for (i = 0; i < ct; i++)
    {
        monster_blow    *blow_ptr = &blows[i];
        innate_attack_t  a = {0};

        if (_blow_is_masked(blow_ptr))
            continue;

        a.dd = blow_ptr->d_dice;
        a.ds = blow_ptr->d_side;
        a.to_h += mbe_info[blow_ptr->effect].power / 3;
        a.to_h += r_ptr->level / 3;

        switch (blow_ptr->method)
        {
        case RBM_HIT:
            a.msg = "You hit %s.";
            a.name = "Hit";
            a.weight = MIN(r_ptr->weight / 2, 400);
            break;
        case RBM_TOUCH:
            a.msg = "You touch %s.";
            a.name = "Touch";
            a.weight = MIN(r_ptr->weight / 10, 100);
            break;
        case RBM_PUNCH:
            a.msg = "You punch %s.";
            a.name = "Punch";
            a.weight = MIN(r_ptr->weight / 2, 300);
            break;
        case RBM_KICK:
            a.msg = "You kick %s.";
            a.name = "Kick";
            a.weight = MIN(r_ptr->weight, 400);
            break;
        case RBM_CLAW:
            a.msg = "You claw %s.";
            a.name = "Claw";
            a.weight = MIN(r_ptr->weight / 2, 300);
            break;
        case RBM_BITE:
            a.msg = "You bite %s.";
            a.name = "Bite";
            a.weight = MIN(r_ptr->weight, 500);
            break;
        case RBM_STING:
            a.msg = "You sting %s.";
            a.name = "Sting";
            a.weight = MIN(r_ptr->weight, 250);
            break;
        case RBM_SLASH:
            a.msg = "You slash %s.";
            a.name = "Slash";
            a.weight = MIN(r_ptr->weight, 300);
            break;
        case RBM_BUTT:
            a.msg = "You butt %s.";
            a.name = "Butt"; /* :) */
            a.weight = MIN(r_ptr->weight * 2, 500);
            break;
        case RBM_CRUSH:
            a.msg = "You crush %s.";
            a.name = "Crush";
            a.weight = MIN(r_ptr->weight * 2, 500);
            break;
        case RBM_ENGULF:
            a.msg = "You engulf %s.";
            a.name = "Engulf";
            a.weight = MIN(r_ptr->weight * 2, 400);
            break;
        case RBM_CHARGE:
            a.msg = "You charge %s.";
            a.name = "Charge";
            a.weight = MIN(r_ptr->weight * 2, 500);
            break;
        case RBM_CRAWL:
            a.msg = "You crawl %s.";
            a.name = "Crawl";
            a.weight = MIN(r_ptr->weight, 400);
            break;
        case RBM_DROOL:
            a.msg = "You drool on %s.";
            a.name = "Drool";
            a.weight = MIN(r_ptr->weight / 10, 100);
            break;
        case RBM_SPIT:
            a.msg = "You spit %s.";
            a.name = "Spit";
            a.weight = MIN(r_ptr->weight / 20, 100);
            break;
        case RBM_GAZE:
            a.msg = "You gaze at %s.";
            a.name = "Gaze";
            a.weight = MIN(r_ptr->weight / 20, 100);
            break;
        case RBM_WAIL:
            a.msg = "You wail at %s.";
            a.name = "Wail";
            a.weight = MIN(r_ptr->weight / 20, 100);
            break;
        case RBM_SPORE:
            a.msg = "You release spores at %s.";
            a.name = "Spores";
            a.weight = MIN(r_ptr->weight / 20, 100);
            break;
        case RBM_BEG:
            a.msg = "You beg %s for money.";
            a.name = "Beg";
            a.weight = MIN(r_ptr->weight / 10, 100);
            break;
        case RBM_INSULT:
            a.msg = "You insult %s.";
            a.name = "Wit";
            a.weight = MIN(r_ptr->weight / 20, 100);
            break;
        case RBM_MOAN:
            a.msg = "You moan at %s.";
            a.name = "Moan";
            a.weight = MIN(r_ptr->weight / 20, 100);
            break;
        case RBM_SHOW:
            a.msg = "You sing to %s.";
            a.name = "Voice";
            a.weight = MIN(r_ptr->weight / 20, 100);
            break;
        }
        if (a.weight < 10) /* 1 lb minimum ... */
            a.weight = 10;

        /* Most early monsters with innate attacks aren't worth possessing as
           their damage is just too low ... Heck, a Mean Looking Mercenary with
           a good longsword is usually a much better option! */
        switch (blow_ptr->method)
        {
        case RBM_HIT:
        case RBM_PUNCH:
        case RBM_KICK:
        case RBM_CLAW:
        case RBM_BITE:
        case RBM_STING:
        case RBM_SLASH:
        case RBM_BUTT:
        case RBM_CRUSH:
            a.to_d += 2 + (r_ptr->level + 4) / 5;
            break;
        }

        switch (blow_ptr->effect)
        {
        case RBE_ACID:
            a.effect[1] = GF_ACID;
            a.dd = (a.dd + 1)/2;
            break;
        case RBE_ELEC:
            a.effect[1] = GF_ELEC;
            a.dd = (a.dd + 1)/2;
            break;
        case RBE_FIRE:
            a.effect[1] = GF_FIRE;
            a.dd = (a.dd + 1)/2;
            break;
        case RBE_COLD:
            a.effect[1] = GF_COLD;
            a.dd = (a.dd + 1)/2;
            break;
        case RBE_POISON:
            a.effect[0] = GF_POIS;
            a.dd = (a.dd + 1)/2;
            break;
        case RBE_DISEASE:
            a.effect[0] = GF_POIS;
            break;
        case RBE_CONFUSE:
            a.effect[1] = GF_OLD_CONF;
            break;
        case RBE_TERRIFY:
            a.effect[1] = GF_TURN_ALL;
            break;
        case RBE_PARALYZE:
            a.effect[1] = GF_STASIS;
            break;
        case RBE_SUPERHURT:
            a.to_d += a.dd * (a.ds + 1) / 4;
            break;
        case RBE_UN_BONUS:
        case RBE_UN_POWER:
            a.effect[0] = GF_DISENCHANT;
            break;
        case RBE_EAT_GOLD:
        case RBE_EAT_ITEM:
        case RBE_EAT_FOOD:
            a.effect[1] = GF_STEAL;
            break;
        case RBE_EXP_10:
        case RBE_EXP_20:
        case RBE_EXP_40:
        case RBE_EXP_80:
            a.effect[0] = GF_NETHER;
            break;
        case RBE_TIME:
            a.effect[0] = GF_TIME;
            break;
        case RBE_EXP_VAMP:
            a.effect[0] = GF_OLD_DRAIN;
            break;
        case RBE_DR_MANA:
            a.effect[1] = GF_DRAIN_MANA;
            break;
        case RBE_SHATTER:
            a.effect[1] = GF_QUAKE;
            break;
        }

        /* Monster Stunning is a bit obtuse ... Perhaps it could be made into an effect? */
        if ( (blow_ptr->method == RBM_KICK || blow_ptr->method == RBM_PUNCH || blow_ptr->method == RBM_BUTT || blow_ptr->method == RBM_CRUSH)
          && a.dd >= 15
          && a.ds <= 2 )
        {
            a.effect[2] = GF_STUN;
        }

        calc_innate_blows(&a, 100 * cts[i]);
        p_ptr->innate_attacks[p_ptr->innate_attack_ct++] = a;
    }
}

/**********************************************************************
 * Possession
 **********************************************************************/
static bool _obj_can_possess(object_type *o_ptr)
{
    return o_ptr->tval == TV_CORPSE && o_ptr->sval == SV_CORPSE;
}
static void _possess_spell(int cmd, variant *res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Possess");
        break;
    case SPELL_DESC:
        var_set_string(res, "Enter the corpse of a new body, gaining the powers and abilities of that form.");
        break;
    case SPELL_INFO:
        var_set_string(res, format("Lvl %d", _calc_level(p_ptr->max_plv) + 5));
        break;
    case SPELL_CAST:
    {
        int item;
        char o_name[MAX_NLEN];
        object_type *o_ptr;
        object_type copy;

        var_set_bool(res, FALSE);

        if ( p_ptr->current_r_idx != MON_POSSESSOR_SOUL 
          && !get_check("Your current body will be destroyed. Are you sure? ") )
        {
            return;
        }

        item_tester_hook = _obj_can_possess;
        if (!get_item(&item, "Possess which corpse? ", "You have nothing to possess.", (USE_INVEN | USE_FLOOR))) 
            return;

        if (item >= 0)
            o_ptr = &inventory[item];
        else
            o_ptr = &o_list[0 - item];

        object_copy(&copy, o_ptr);
        copy.number = 1;
        object_desc(o_name, &copy, OD_NAME_ONLY);

        if (r_info[copy.pval].level > _calc_level(p_ptr->max_plv) + 5)
        {
            msg_format("You are not powerful enough to possess %s (Lvl %d).", o_name, r_info[copy.pval].level);
            return;
        }
        else
            msg_format("You possess %s.", o_name);

        /* Order is important. Changing body forms may result in illegal
           equipment being placed in the pack, invalidating the item index.
           This is exacerbated by corpses sorting to the bottom :( */
        if (item >= 0)
        {
            inven_item_increase(item, -1);
            inven_item_describe(item);
            inven_item_optimize(item);
        }
        else
        {
            floor_item_increase(0 - item, -1);
            floor_item_describe(0 - item);
            floor_item_optimize(0 - item);
        }
        o_ptr = NULL;

        p_ptr->current_r_idx = copy.pval;
        if (p_ptr->exp > possessor_max_exp())
        {
            p_ptr->exp = possessor_max_exp();
            check_experience();
        }
        else
            restore_level();

        p_ptr->update |= PU_BONUS | PU_HP | PU_MANA;
        p_ptr->redraw |= PR_MAP | PR_BASIC | PR_MANA | PR_EXP | PR_EQUIPPY;

        /* Apply the new body type to our equipment */
        equip_on_change_race();

        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

/**********************************************************************
 * Innate Powers
 **********************************************************************/
static int _breath_amount(int type)
{
    int l = p_ptr->lev;
    switch (type)
    {
    case GF_ACID: case GF_ELEC: case GF_FIRE : case GF_COLD:
        return MAX(1, MIN(900, p_ptr->chp * (25 + l*l*l/2500) / 100));

    case GF_POIS: case GF_NUKE:
        return MAX(1, MIN(700, p_ptr->chp * (20 + l*l*l/2500) / 100));

    case GF_NETHER:
        return MAX(1, MIN(650, p_ptr->chp * (20 + l*l*l*30/125000) / 100));

    case GF_LITE: case GF_DARK:
        return MAX(1, MIN(350, p_ptr->chp * (20 + l*l*l*15/125000) / 100));

    case GF_CONFUSION: case GF_NEXUS: 
        return MAX(1, MIN(300, p_ptr->chp * (20 + l*l*l*15/125000) / 100));

    case GF_TIME: case GF_INERT: case GF_GRAVITY: case GF_DISINTEGRATE:
    case GF_PLASMA: case GF_FORCE:
        return MAX(1, MIN(250, p_ptr->chp * (20 + l*l*l*15/125000) / 100));

    case GF_SOUND:
        return MAX(1, MIN(400, p_ptr->chp * (20 + l*l*l*25/125000) / 100));

    case GF_STORM:
        return MAX(1, MIN(300, p_ptr->chp * (20 + l*l*l*20/125000) / 100));

    case GF_CHAOS: case GF_SHARDS:
        return MAX(1, MIN(500, p_ptr->chp * (20 + l*l*l*30/125000) / 100));

    case GF_MANA:
        return MAX(1, MIN(400, p_ptr->chp * (20 + l*l*l*25/125000) / 100));

    case GF_DISENCHANT:
        return MAX(1, MIN(450, p_ptr->chp * (20 + l*l*l*30/125000) / 100));
    }
    return 0;
}

static void _breathe_spell(int what, int cmd, variant *res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, format("Breathe %^s", gf_name(what)));
        break;
    case SPELL_DESC:
        var_set_string(res, format("Breathes %s at your opponent.", gf_name(what)));
        break;
    case SPELL_INFO:
        var_set_string(res, info_damage(0, 0, _breath_amount(what)));
        break;
    case SPELL_CAST:
    {
        int dir = 0;
        var_set_bool(res, FALSE);
        if (get_aim_dir(&dir))
        {
            if (p_ptr->current_r_idx == MON_BOTEI) 
                msg_print("'Botei-Build cutter!!!'");
            else
                msg_format("You breathe %s!", gf_name(what));
            fire_ball(what, dir, _breath_amount(what), -1 - (p_ptr->lev / 20));
            var_set_bool(res, TRUE);
        }
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _breathe_acid_spell(int cmd, variant *res) { _breathe_spell(GF_ACID, cmd, res); }
static void _breathe_elec_spell(int cmd, variant *res) { _breathe_spell(GF_ELEC, cmd, res); }
static void _breathe_fire_spell(int cmd, variant *res) { _breathe_spell(GF_FIRE, cmd, res); }
static void _breathe_cold_spell(int cmd, variant *res) { _breathe_spell(GF_COLD, cmd, res); }
static void _breathe_poison_spell(int cmd, variant *res) { _breathe_spell(GF_POIS, cmd, res); }
static void _breathe_nether_spell(int cmd, variant *res) { _breathe_spell(GF_NETHER, cmd, res); }
static void _breathe_light_spell(int cmd, variant *res) { _breathe_spell(GF_LITE, cmd, res); }
static void _breathe_dark_spell(int cmd, variant *res) { _breathe_spell(GF_DARK, cmd, res); }
static void _breathe_confusion_spell(int cmd, variant *res) { _breathe_spell(GF_CONFUSION, cmd, res); }
static void _breathe_sound_spell(int cmd, variant *res) { _breathe_spell(GF_SOUND, cmd, res); }
static void _breathe_chaos_spell(int cmd, variant *res) { _breathe_spell(GF_CHAOS, cmd, res); }
static void _breathe_disenchantment_spell(int cmd, variant *res) { _breathe_spell(GF_DISENCHANT, cmd, res); }
static void _breathe_nexus_spell(int cmd, variant *res) { _breathe_spell(GF_NEXUS, cmd, res); }
static void _breathe_storm_spell(int cmd, variant *res) { _breathe_spell(GF_STORM, cmd, res); }
static void _breathe_time_spell(int cmd, variant *res) { _breathe_spell(GF_TIME, cmd, res); }
static void _breathe_inertia_spell(int cmd, variant *res) { _breathe_spell(GF_INERT, cmd, res); }
static void _breathe_gravity_spell(int cmd, variant *res) { _breathe_spell(GF_GRAVITY, cmd, res); }
static void _breathe_shards_spell(int cmd, variant *res) { _breathe_spell(GF_SHARDS, cmd, res); }
static void _breathe_plasma_spell(int cmd, variant *res) { _breathe_spell(GF_PLASMA, cmd, res); }
static void _breathe_force_spell(int cmd, variant *res) { _breathe_spell(GF_FORCE, cmd, res); }
static void _breathe_mana_spell(int cmd, variant *res) { _breathe_spell(GF_MANA, cmd, res); }
static void _breathe_disintegration_spell(int cmd, variant *res) { _breathe_spell(GF_DISINTEGRATE, cmd, res); }
static void _breathe_nuke_spell(int cmd, variant *res) { _breathe_spell(GF_NUKE, cmd, res); }

static void _multiply_spell(int cmd, variant *res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Multiply");
        break;
    case SPELL_DESC:
        var_set_string(res, "Engage in breeding activities. No mate required!");
        break;
    case SPELL_CAST:
    {
        summon_named_creature(-1, py, px, p_ptr->current_r_idx, PM_FORCE_PET);
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _rocket_spell(int cmd, variant *res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Rocket");
        break;
    case SPELL_DESC:
        var_set_string(res, "Launches a powerful rocket at your opponent.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_damage(0, 0, p_ptr->chp / 3));
        break;
    case SPELL_CAST:
    {
        int dir = 0;
        var_set_bool(res, FALSE);
        if (!get_aim_dir(&dir)) return;

        msg_print("You launch a rocket.");
        fire_rocket(GF_ROCKET, dir, p_ptr->chp / 3, 2);

        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _shriek_spell(int cmd, variant *res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Shriek");
        break;
    case SPELL_DESC:
        var_set_string(res, "Make a very loud annoying sound hoping that nearby monsters will assist you.");
        break;
    case SPELL_CAST:
        msg_print("You make a high pitched shriek.");
        aggravate_monsters(0);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _add_power(spell_info* spell, int lvl, int cost, int fail, ang_spell fn, int stat_idx)
{
    spell->level = lvl;
    spell->cost = cost;
    spell->fail = calculate_fail_rate(lvl, fail, stat_idx); 
    spell->fn = fn;
}

static int _get_powers(spell_info* spells, int max)
{
    monster_race *r_ptr = &r_info[p_ptr->current_r_idx];
    int           ct = 0;

    if (ct < max)
        _add_power(&spells[ct++], 1, 0, 0, _possess_spell, p_ptr->stat_ind[A_DEX]);
    if (ct < max && (r_ptr->flags2 & RF2_MULTIPLY))
        _add_power(&spells[ct++], 1, 5, 40, _multiply_spell, p_ptr->stat_ind[A_CHR]);
    if (ct < max && (r_ptr->flags4 & RF4_SHRIEK))
        _add_power(&spells[ct++], 1, 1, 10, _shriek_spell, p_ptr->stat_ind[A_CON]);
    if (ct < max && (r_ptr->flags4 & RF4_SHOOT))
        _add_power(&spells[ct++], 2, 1, 15, shoot_arrow_spell, p_ptr->stat_ind[A_DEX]);
    if (ct < max && (r_ptr->flags4 & RF4_THROW))
        _add_power(&spells[ct++], 5, 0, 50, throw_boulder_spell, p_ptr->stat_ind[A_STR]);
    if (ct < max && (r_ptr->flags9 & RF9_POS_BERSERK))
        _add_power(&spells[ct++], 13, 9, 50, berserk_spell, p_ptr->stat_ind[A_STR]);
    if (ct < max && (r_ptr->flags4 & RF4_BR_ACID))
        _add_power(&spells[ct++], 20, 25, 55, _breathe_acid_spell, p_ptr->stat_ind[A_CON]);
    if (ct < max && (r_ptr->flags4 & RF4_BR_ELEC))
        _add_power(&spells[ct++], 20, 25, 55, _breathe_elec_spell, p_ptr->stat_ind[A_CON]);
    if (ct < max && (r_ptr->flags4 & RF4_BR_FIRE))
        _add_power(&spells[ct++], 20, 25, 55, _breathe_fire_spell, p_ptr->stat_ind[A_CON]);
    if (ct < max && (r_ptr->flags4 & RF4_BR_COLD))
        _add_power(&spells[ct++], 20, 25, 55, _breathe_cold_spell, p_ptr->stat_ind[A_CON]);
    if (ct < max && (r_ptr->flags4 & RF4_BR_POIS))
        _add_power(&spells[ct++], 20, 25, 55, _breathe_poison_spell, p_ptr->stat_ind[A_CON]);
    if (ct < max && (r_ptr->flags4 & RF4_BR_NETH))
        _add_power(&spells[ct++], 20, 25, 70, _breathe_nether_spell, p_ptr->stat_ind[A_CON]);
    if (ct < max && (r_ptr->flags4 & RF4_BR_LITE))
        _add_power(&spells[ct++], 20, 26, 70, _breathe_light_spell, p_ptr->stat_ind[A_CON]);
    if (ct < max && (r_ptr->flags4 & RF4_BR_DARK))
        _add_power(&spells[ct++], 20, 26, 70, _breathe_dark_spell, p_ptr->stat_ind[A_CON]);
    if (ct < max && (r_ptr->flags4 & RF4_BR_CONF))
        _add_power(&spells[ct++], 20, 30, 70, _breathe_confusion_spell, p_ptr->stat_ind[A_CON]);
    if (ct < max && (r_ptr->flags4 & RF4_BR_SOUN))
        _add_power(&spells[ct++], 20, 30, 70, _breathe_sound_spell, p_ptr->stat_ind[A_CON]);
    if (ct < max && (r_ptr->flags4 & RF4_BR_CHAO))
        _add_power(&spells[ct++], 20, 30, 70, _breathe_chaos_spell, p_ptr->stat_ind[A_CON]);
    if (ct < max && (r_ptr->flags4 & RF4_BR_DISE))
        _add_power(&spells[ct++], 20, 30, 70, _breathe_disenchantment_spell, p_ptr->stat_ind[A_CON]);
    if (ct < max && (r_ptr->flags4 & RF4_BR_NUKE))
        _add_power(&spells[ct++], 25, 30, 70, _breathe_nuke_spell, p_ptr->stat_ind[A_CON]);
    if (ct < max && (r_ptr->flags4 & RF4_BR_SHAR))
        _add_power(&spells[ct++], 25, 35, 70, _breathe_shards_spell, p_ptr->stat_ind[A_CON]);
    if (ct < max && (r_ptr->flags4 & RF4_BR_NEXU))
        _add_power(&spells[ct++], 30, 35, 80, _breathe_nexus_spell, p_ptr->stat_ind[A_CON]);
    if (ct < max && (r_ptr->flags4 & RF4_BR_INER))
        _add_power(&spells[ct++], 30, 35, 80, _breathe_inertia_spell, p_ptr->stat_ind[A_CON]);
    if (ct < max && (r_ptr->flags4 & RF4_BR_GRAV))
        _add_power(&spells[ct++], 30, 38, 90, _breathe_gravity_spell, p_ptr->stat_ind[A_CON]);
    if (ct < max && (r_ptr->flags4 & RF4_BR_WALL))
        _add_power(&spells[ct++], 30, 28, 80, _breathe_force_spell, p_ptr->stat_ind[A_CON]);
    if (ct < max && (r_ptr->flags4 & RF4_BR_MANA))
        _add_power(&spells[ct++], 30, 38, 80, _breathe_mana_spell, p_ptr->stat_ind[A_CON]);
    if (ct < max && (r_ptr->flags4 & RF4_BR_PLAS))
        _add_power(&spells[ct++], 35, 35, 80, _breathe_plasma_spell, p_ptr->stat_ind[A_CON]);
    if (ct < max && (r_ptr->flags4 & RF4_BR_STORM))
        _add_power(&spells[ct++], 35, 40, 80, _breathe_storm_spell, p_ptr->stat_ind[A_CON]);
    if (ct < max && (r_ptr->flags4 & RF4_BR_TIME))
        _add_power(&spells[ct++], 35, 30, 80, _breathe_time_spell, p_ptr->stat_ind[A_CON]);
    if (ct < max && (r_ptr->flags4 & RF4_BR_DISI))
        _add_power(&spells[ct++], 35, 50, 95, _breathe_disintegration_spell, p_ptr->stat_ind[A_CON]);
    if (ct < max && (r_ptr->flags4 & RF4_ROCKET))
        _add_power(&spells[ct++], 35, 30, 80, _rocket_spell, p_ptr->stat_ind[A_STR]);

    return ct;
}

/**********************************************************************
 * Spells
 **********************************************************************/
void _healing_spell(int cmd, variant *res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Healing");
        break;
    case SPELL_DESC:
        var_set_string(res, "Heals hitpoints, cuts and stun.");
        break;
    case SPELL_INFO:
    {
        monster_race *r_ptr = &r_info[p_ptr->current_r_idx];
        var_set_string(res, format("Heals %d", MIN(300, r_ptr->level * 5)));
        break;
    }
    case SPELL_CAST:
    {
        monster_race *r_ptr = &r_info[p_ptr->current_r_idx];
        hp_player(MIN(300, r_ptr->level * 5));
        set_stun(0, TRUE);
        set_cut(0, TRUE);
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _add_spell(spell_info* spell, int lvl, int cost, int fail, ang_spell fn, int stat_idx)
{
    spell->level = lvl;
    spell->cost = cost;
    spell->fail = calculate_fail_rate(lvl, fail, stat_idx); 
    spell->fn = fn;
}

static int _get_spells(spell_info* spells, int max) 
{
    monster_race *r_ptr = &r_info[p_ptr->current_r_idx];
    int           ct = 0;
    int           stat_idx = p_ptr->stat_ind[r_ptr->body.spell_stat];

    if (ct < max && (r_ptr->flags9 & RF9_POS_BLESSING))
        _add_spell(&spells[ct++], 1, 1, 30, bless_spell, stat_idx);
    if (ct < max && (r_ptr->flags5 & RF5_MISSILE))
        _add_spell(&spells[ct++], 1, 1, 30, magic_missile_spell, stat_idx);
    if (ct < max && (r_ptr->flags5 & RF5_CAUSE_1))
        _add_spell(&spells[ct++], 3, 1, 25, cause_wounds_I_spell, stat_idx);
    if (ct < max && (r_ptr->flags9 & RF9_POS_DETECT_TRAPS))
        _add_spell(&spells[ct++], 3, 2, 30, detect_traps_spell, stat_idx);
    if (ct < max && (r_ptr->flags9 & RF9_POS_DETECT_EVIL))
        _add_spell(&spells[ct++], 3, 2, 30, detect_evil_spell, stat_idx);
    if (ct < max && (r_ptr->flags6 & RF6_DARKNESS))
        _add_spell(&spells[ct++], 5, 1, 20, create_darkness_spell, stat_idx);
    if (ct < max && (r_ptr->flags6 & RF6_TRAPS))
        _add_spell(&spells[ct++], 5, 1, 20, create_minor_trap_spell, stat_idx);
    if (ct < max && (r_ptr->flags6 & RF6_BLINK))
        _add_spell(&spells[ct++], 5, 1, 30, phase_door_spell, stat_idx);
    if (ct < max && (r_ptr->flags5 & RF5_BA_POIS))
        _add_spell(&spells[ct++], 5, 3, 40, stinking_cloud_spell, stat_idx);
    if (ct < max && (r_ptr->flags5 & RF5_SCARE))
        _add_spell(&spells[ct++], 5, 3, 35, scare_monster_spell, stat_idx);
    if (ct < max && (r_ptr->flags9 & RF9_POS_DETECT_MONSTERS))
        _add_spell(&spells[ct++], 7, 3, 40, detect_monsters_spell, stat_idx);
    if (ct < max && (r_ptr->flags9 & RF9_POS_HEROISM))
        _add_spell(&spells[ct++], 8, 5, 40, heroism_spell, stat_idx);
    if (ct < max && (r_ptr->flags9 & RF9_POS_DETECT_OBJECTS))
        _add_spell(&spells[ct++], 9, 5, 40, detect_objects_spell, stat_idx);
    if (ct < max && (r_ptr->flags9 & RF9_POS_IDENTIFY))
        _add_spell(&spells[ct++], 10, 7, 50, identify_spell, stat_idx);
    if (ct < max && (r_ptr->flags5 & RF5_CONF))
        _add_spell(&spells[ct++], 10, 5, 40, confuse_spell, stat_idx);
    if (ct < max && (r_ptr->flags5 & RF5_SLOW))
        _add_spell(&spells[ct++], 10, 5, 40, slow_spell, stat_idx);
    if (ct < max && (r_ptr->flags5 & RF5_HOLD))
        _add_spell(&spells[ct++], 10, 5, 40, paralyze_spell, stat_idx);
    if (ct < max && (r_ptr->flags5 & RF5_DRAIN_MANA))
        _add_spell(&spells[ct++], 10, 5, 50, drain_mana_spell, stat_idx);
    if (ct < max && (r_ptr->flags5 & RF5_BO_ELEC))
        _add_spell(&spells[ct++], 10, 5, 35, lightning_bolt_spell, stat_idx);
    if (ct < max && (r_ptr->flags5 & RF5_CAUSE_2))
        _add_spell(&spells[ct++], 12, 2, 35, cause_wounds_II_spell, stat_idx);
    if (ct < max && (r_ptr->flags5 & RF5_BO_COLD))
        _add_spell(&spells[ct++], 12, 6, 35, frost_bolt_spell, stat_idx);
    if (ct < max && (r_ptr->flags5 & RF5_BO_ACID))
        _add_spell(&spells[ct++], 13, 7, 40, acid_bolt_spell, stat_idx);
    if (ct < max && (r_ptr->flags5 & RF5_BA_ELEC))
        _add_spell(&spells[ct++], 14, 10, 45, lightning_ball_spell, stat_idx);
    if (ct < max && (r_ptr->flags9 & RF9_POS_MAPPING))
        _add_spell(&spells[ct++], 15, 10, 50, magic_mapping_spell, stat_idx);
    if (ct < max && (r_ptr->flags6 & RF6_FORGET))
        _add_spell(&spells[ct++], 15, 3, 40, amnesia_spell, stat_idx);
    if (ct < max && (r_ptr->flags6 & RF6_HEAL) && r_ptr->level < 20)
        _add_spell(&spells[ct++], 15, 8, 50, cure_wounds_III_spell, stat_idx);
    if (ct < max && (r_ptr->flags6 & RF6_TPORT))
        _add_spell(&spells[ct++], 15, 8, 40, teleport_spell, stat_idx);
    if (ct < max && (r_ptr->flags6 & RF6_TELE_TO))
        _add_spell(&spells[ct++], 15, 8, 50, teleport_to_spell, stat_idx);
    if (ct < max && (r_ptr->flags5 & RF5_BO_FIRE))
        _add_spell(&spells[ct++], 15, 9, 50, fire_bolt_spell, stat_idx);
    if (ct < max && (r_ptr->flags5 & RF5_BA_COLD))
        _add_spell(&spells[ct++], 15, 11, 50, frost_ball_spell, stat_idx);
    if (ct < max && (r_ptr->flags5 & RF5_BA_ACID))
        _add_spell(&spells[ct++], 18, 13, 55, acid_ball_spell, stat_idx);
    if (ct < max && (r_ptr->flags6 & RF6_HASTE))
        _add_spell(&spells[ct++], 20, 10, 70, haste_self_spell, stat_idx);
    if (ct < max && (r_ptr->flags6 & RF6_TELE_AWAY))
        _add_spell(&spells[ct++], 20, 13, 80, teleport_other_spell, stat_idx);
    if (ct < max && (r_ptr->flags5 & RF5_BA_FIRE))
        _add_spell(&spells[ct++], 20, 14, 60, fire_ball_spell, stat_idx);
    if (ct < max && (r_ptr->flags5 & RF5_BO_MANA) && r_ptr->level < 25) /* DE Warlock */
        _add_spell(&spells[ct++], 20, 15, 60, mana_bolt_I_spell, stat_idx);
    if (ct < max && (r_ptr->flags6 & RF6_HEAL) && r_ptr->level >= 20)
        _add_spell(&spells[ct++], 20, 35, 70, _healing_spell, stat_idx);
    if (ct < max && (r_ptr->flags5 & RF5_CAUSE_3))
        _add_spell(&spells[ct++], 22, 6, 50, cause_wounds_III_spell, stat_idx);
    if (ct < max && (r_ptr->flags5 & RF5_MIND_BLAST))
        _add_spell(&spells[ct++], 25, 10, 60, mind_blast_spell, stat_idx);
    if (ct < max && (r_ptr->flags5 & RF5_BO_NETH))
        _add_spell(&spells[ct++], 25, 12, 60, nether_bolt_spell, stat_idx);
    if (ct < max && (r_ptr->flags5 & RF5_BO_ICEE))
        _add_spell(&spells[ct++], 25, 16, 60, ice_bolt_spell, stat_idx);
    if (ct < max && (r_ptr->flags5 & RF5_BA_NETH))
        _add_spell(&spells[ct++], 25, 18, 70, nether_ball_spell, stat_idx);
    if (ct < max && (r_ptr->flags4 & RF4_BA_NUKE))
        _add_spell(&spells[ct++], 25, 20, 95, radiation_ball_spell, stat_idx);
    if (ct < max && (r_ptr->flags5 & RF5_BO_WATE))
        _add_spell(&spells[ct++], 25, 20, 65, water_bolt_spell, stat_idx);
    if (ct < max && (r_ptr->flags5 & RF5_BO_PLAS))
        _add_spell(&spells[ct++], 25, 20, 80, plasma_bolt_spell, stat_idx);
    if (ct < max && (r_ptr->flags5 & RF5_BO_MANA) && r_ptr->level >= 25)
        _add_spell(&spells[ct++], 25, 24, 90, mana_bolt_II_spell, stat_idx);
    if (ct < max && (r_ptr->flags2 & RF2_THIEF))
        _add_spell(&spells[ct++], 30, 20, 60, panic_hit_spell, stat_idx);
    if (ct < max && (r_ptr->flags5 & RF5_BRAIN_SMASH))
        _add_spell(&spells[ct++], 30, 14, 65, brain_smash_spell, stat_idx);
    if (ct < max && (r_ptr->flags5 & RF5_BA_WATE))
        _add_spell(&spells[ct++], 30, 22, 75, water_ball_spell, stat_idx);
    if (ct < max && (r_ptr->flags4 & RF4_BA_CHAO))
        _add_spell(&spells[ct++], 30, 32, 85, invoke_logrus_spell, stat_idx);
    if (ct < max && (r_ptr->flags6 & RF6_RAISE_DEAD))
        _add_spell(&spells[ct++], 30, 30, 70, animate_dead_spell, stat_idx);
    if (ct < max && (r_ptr->flags6 & RF6_TELE_LEVEL))
        _add_spell(&spells[ct++], 30, 40, 95, teleport_level_spell, stat_idx);
    if (ct < max && (r_ptr->flags5 & RF5_CAUSE_4))
        _add_spell(&spells[ct++], 32, 10, 70, cause_wounds_IV_spell, stat_idx);
    if (ct < max && (r_ptr->flags5 & RF5_BA_DARK) && r_ptr->level < 43) /* Jack */
        _add_spell(&spells[ct++], 32, 20, 80, darkness_storm_I_spell, stat_idx);
    if (ct < max && (r_ptr->flags6 & RF6_PSY_SPEAR))
        _add_spell(&spells[ct++], 35, 30, 80, psycho_spear_spell, stat_idx);
    if (ct < max && (r_ptr->flags6 & RF6_TRAPS) && r_ptr->level >= 30)
        _add_spell(&spells[ct++], 37, 30, 80, create_major_trap_spell, stat_idx);
    if (ct < max && (r_ptr->flags4 & RF4_DISPEL))
        _add_spell(&spells[ct++], 40, 35, 85, dispel_magic_spell, stat_idx);
    if (ct < max && (r_ptr->flags5 & RF5_BA_DARK) && r_ptr->level >= 43) /* !Jack */
        _add_spell(&spells[ct++], 40, 42, 90, darkness_storm_II_spell, stat_idx);
    if (ct < max && (r_ptr->flags5 & RF5_BA_LITE))
        _add_spell(&spells[ct++], 40, 42, 90, starburst_II_spell, stat_idx);
    if (ct < max && (r_ptr->flags5 & RF5_BA_MANA))
        _add_spell(&spells[ct++], 44, 45, 85, mana_storm_II_spell, stat_idx);
    if (ct < max && (r_ptr->flags6 & RF6_HAND_DOOM))
        _add_spell(&spells[ct++], 45, 120, 95, hand_of_doom_spell, stat_idx);
    if (ct < max && (r_ptr->flags6 & RF6_INVULNER))
        _add_spell(&spells[ct++], 45, 65, 80, invulnerability_spell, stat_idx);
    if (ct < max && (r_ptr->flags6 & RF6_WORLD))
        _add_spell(&spells[ct++], 45, 150, 85, stop_time_spell, stat_idx);
    if (ct < max && (r_ptr->flags6 & RF6_TRAPS) && r_ptr->level >= 43) /* !Jack */
        _add_spell(&spells[ct++], 50, 100, 95, create_ultimate_trap_spell, stat_idx);

    /* I prefer summoning at the bottom ... */
    if (ct < max && (r_ptr->flags6 & RF6_S_MONSTER))
        _add_spell(&spells[ct++], 25, 20, 65, summon_monster_spell, stat_idx);
    if (ct < max && (r_ptr->flags6 & RF6_S_SPIDER))
        _add_spell(&spells[ct++], 25, 20, 60, summon_spiders_spell, stat_idx);
    if (ct < max && (r_ptr->flags6 & RF6_S_ANT))
        _add_spell(&spells[ct++], 25, 20, 65, summon_ants_spell, stat_idx);
    if (ct < max && (r_ptr->flags6 & RF6_S_HYDRA))
        _add_spell(&spells[ct++], 30, 23, 70, summon_hydras_spell, stat_idx);
    if (ct < max && (r_ptr->flags6 & RF6_S_UNDEAD))
        _add_spell(&spells[ct++], 30, 30, 75, summon_undead_spell, stat_idx);
    if (ct < max && (r_ptr->flags6 & RF6_S_HOUND))
        _add_spell(&spells[ct++], 35, 26, 75, summon_hounds_spell, stat_idx);
    if (ct < max && (r_ptr->flags6 & RF6_S_MONSTERS))
        _add_spell(&spells[ct++], 35, 30, 75, summon_monsters_spell, stat_idx);
    if (ct < max && (r_ptr->flags6 & RF6_S_DEMON))
        _add_spell(&spells[ct++], 35, 50, 80, summon_demon_II_spell, stat_idx);
    if (ct < max && (r_ptr->flags6 & RF6_S_DRAGON))
        _add_spell(&spells[ct++], 39, 70, 80, summon_dragon_spell, stat_idx);
    if (ct < max && (r_ptr->flags6 & RF6_S_ANGEL))
        _add_spell(&spells[ct++], 40, 50, 85, summon_angel_spell, stat_idx);
    if (ct < max && (r_ptr->flags6 & RF6_S_KIN))
        _add_spell(&spells[ct++], 40, 70, 85, summon_kin_spell, stat_idx);
    if (ct < max && (r_ptr->flags6 & RF6_S_HI_UNDEAD))
        _add_spell(&spells[ct++], 43, 85, 85, summon_hi_undead_spell, stat_idx);
    if (ct < max && (r_ptr->flags6 & RF6_S_CYBER))
        _add_spell(&spells[ct++], 45, 90, 90, summon_cyberdemon_spell, stat_idx);
    if (ct < max && (r_ptr->flags6 & RF6_S_HI_DRAGON))
        _add_spell(&spells[ct++], 46, 90, 85, summon_hi_dragon_spell, stat_idx);
    if (ct < max && (r_ptr->flags6 & RF6_S_AMBERITES))
        _add_spell(&spells[ct++], 48, 120, 90, summon_amberites_spell, stat_idx);
    if (ct < max && (r_ptr->flags6 & RF6_S_UNIQUE))
        _add_spell(&spells[ct++], 50, 150, 95, summon_uniques_spell, stat_idx);

    if (ct == 0)
        msg_print("You have no spells!");

    return ct;
}

static caster_info * _caster_info(void)
{
    static caster_info me = {0};
    monster_race *r_ptr = &r_info[p_ptr->current_r_idx];

    me.options = 0;
    me.which_stat = r_ptr->body.spell_stat;

    if (me.which_stat == A_INT)
    {
        me.magic_desc = "spell";
        me.options = CASTER_ALLOW_DEC_MANA;
        me.weight = 350;
    }
    else if (me.which_stat == A_WIS)
    {
        me.magic_desc = "prayer";
        me.weight = 450;
    }
    else
    {
        me.magic_desc = "power";
        me.weight = 450;
    }
    return &me;
}

/**********************************************************************
 * Bonuses
 * TODO: Someday, we should have a unified flag system ... sigh.
 **********************************************************************/
static int ac_percent;

static void _ac_bonus_imp(int slot)
{
    object_type *o_ptr = equip_obj(slot);
    if (o_ptr)
    {
        switch (equip_slot_type(slot))
        {
        case EQUIP_SLOT_BODY_ARMOR:
            ac_percent -= 25;
            break;
        case EQUIP_SLOT_CLOAK:
            ac_percent -= 5;
            break;
        case EQUIP_SLOT_WEAPON_SHIELD:
            if (object_is_shield(o_ptr))
                ac_percent -= 15;
            break;
        case EQUIP_SLOT_HELMET:
            ac_percent -= 7;
            break;
        case EQUIP_SLOT_GLOVES:
            ac_percent -= 5;
            break;
        case EQUIP_SLOT_BOOTS:
            ac_percent -= 5;
            break;
        }
    }
}

static void _calc_bonuses(void) 
{
    monster_race *r_ptr = &r_info[p_ptr->current_r_idx];
    int           r_lvl = MAX(1, r_ptr->level);
    int           p_lvl = _calc_level(p_ptr->lev);

    if (!p_ptr->current_r_idx) /* Birth hack ... we haven't been "born" yet! */
        return;

    if ((r_ptr->flags1 & RF1_FEMALE) && p_ptr->psex != SEX_FEMALE)
    {
        p_ptr->psex = SEX_FEMALE;
        sp_ptr = &sex_info[p_ptr->psex];
    }

    if ((r_ptr->flags1 & RF1_MALE) && p_ptr->psex != SEX_MALE)
    {
        p_ptr->psex = SEX_MALE;
        sp_ptr = &sex_info[p_ptr->psex];
    }

    if (!equip_can_wield_kind(TV_LITE, SV_LITE_FEANOR))
        p_ptr->see_nocto = TRUE;

    if (r_ptr->flags9 & RF9_POS_GAIN_AC)
    {
        int to_a = r_ptr->ac * MIN(p_lvl, r_lvl) / r_lvl;

        /* Reduce AC bonus a bit depending on what armor slots are available.
           For example, Wahha-man has AC200 yet can also wear a full complement of armor! */        
        ac_percent = 100;
        equip_for_each_slot(_ac_bonus_imp);
        to_a = to_a * ac_percent / 100;

        if (to_a > 0)
        {
            p_ptr->to_a += to_a;
            p_ptr->dis_to_a += to_a;
        }
    }

    /* Add possessor speed info to r_info? The problem is that many end game
       humanoid forms are too fast! Possessing Great Eagles should be encouraged,
       as they have severe equipment limitations to compensate. */
    if (r_ptr->speed != 110)
    {
        int sp = (int)r_ptr->speed - 110;

        if (sp < 0)
            p_ptr->pspeed += sp;
        else
        {
        int bonus;

            if (strchr("hkoOpPTVWz", r_ptr->d_char))
                sp /= 3;

            bonus = sp * MIN(p_lvl, r_lvl) / r_lvl;

            p_ptr->pspeed += bonus;
        }
    }

    if (r_ptr->flags3 & RF3_GOOD)
        p_ptr->align += 200;
    if (r_ptr->flags3 & RF3_EVIL)
        p_ptr->align -= 200;

    if (r_ptr->flags9 & RF9_POS_HOLD_LIFE)
        p_ptr->hold_life = TRUE;
    /*if (r_ptr->flags1 & (RF1_RAND_25 | RF1_RAND_50))
        p_ptr->move_random = TRUE;*/
    if (r_ptr->flags9 & RF9_POS_TELEPATHY)
        p_ptr->telepathy = TRUE;
    if (r_ptr->flags9 & RF9_POS_SEE_INVIS)
        p_ptr->see_inv = TRUE;

    if (r_ptr->flags9 & RF9_POS_SUST_STR)
        p_ptr->sustain_str = TRUE;
    if (r_ptr->flags9 & RF9_POS_SUST_INT)
        p_ptr->sustain_int = TRUE;
    if (r_ptr->flags9 & RF9_POS_SUST_WIS)
        p_ptr->sustain_wis = TRUE;
    if (r_ptr->flags9 & RF9_POS_SUST_DEX)
        p_ptr->sustain_dex = TRUE;
    if (r_ptr->flags9 & RF9_POS_SUST_CON)
        p_ptr->sustain_con = TRUE;
    if (r_ptr->flags9 & RF9_POS_SUST_CHR)
        p_ptr->sustain_chr = TRUE;

    if (r_ptr->flags2 & RF2_REFLECTING)
        p_ptr->reflect = TRUE;
    if (r_ptr->flags2 & RF2_REGENERATE)
        p_ptr->regenerate = TRUE;
    if ((r_ptr->flags2 & RF2_ELDRITCH_HORROR) || strchr("GLUVW", r_ptr->d_char))
        p_ptr->no_eldritch = TRUE;
    if (r_ptr->flags2 & RF2_AURA_FIRE)
        p_ptr->sh_fire = TRUE;
    if (r_ptr->flags2 & RF2_AURA_ELEC)
        p_ptr->sh_elec = TRUE;
    if (r_ptr->flags3 & RF3_AURA_COLD)
        p_ptr->sh_cold = TRUE;
    if (r_ptr->flags2 & RF2_PASS_WALL)
    {
        p_ptr->pass_wall = TRUE;
        p_ptr->no_passwall_dam = TRUE;
    }
    if (r_ptr->flags2 & RF2_KILL_WALL)
        p_ptr->kill_wall = TRUE;
    if (r_ptr->flags2 & RF2_AURA_REVENGE)
        p_ptr->sh_retaliation = TRUE;
    if (r_ptr->flags2 & RF2_AURA_FEAR)
        p_ptr->sh_fear = TRUE;

    if (r_ptr->flags3 & RF3_HURT_LITE)
        res_add_vuln(RES_LITE);
    if (r_ptr->flags3 & RF3_HURT_FIRE)
        res_add_vuln(RES_FIRE);
    if (r_ptr->flags3 & RF3_HURT_COLD)
        res_add_vuln(RES_COLD);
    if (r_ptr->flags3 & RF3_NO_FEAR)
        res_add(RES_FEAR);
    if (r_ptr->flags3 & RF3_NO_STUN)
        p_ptr->no_stun = TRUE;
    if (r_ptr->flags3 & RF3_NO_CONF)
        res_add(RES_CONF);
    if (r_ptr->flags3 & RF3_NO_SLEEP)
        p_ptr->free_act = TRUE;

    if (r_ptr->flags7 & RF7_CAN_FLY)
        p_ptr->levitation = TRUE;

    if (r_ptr->flagsr & RFR_IM_ACID)
        res_add_amt(RES_ACID, 3);
    if (r_ptr->flagsr & RFR_IM_ELEC)
        res_add_amt(RES_ELEC, 3);
    if (r_ptr->flagsr & RFR_IM_FIRE)
        res_add_amt(RES_FIRE, 3);
    if (r_ptr->flagsr & RFR_IM_COLD)
        res_add_amt(RES_COLD, 3);
    if (r_ptr->flagsr & RFR_IM_POIS)
        res_add_amt(RES_POIS, 3);
    if (r_ptr->flagsr & RFR_RES_LITE)
        res_add(RES_LITE);
    if (r_ptr->flagsr & RFR_RES_DARK)
        res_add(RES_DARK);
    if (r_ptr->flagsr & RFR_RES_NETH)
        res_add(RES_NETHER);
    if (r_ptr->flagsr & RFR_RES_SHAR)
        res_add(RES_SHARDS);
    if (r_ptr->flagsr & RFR_RES_SOUN)
        res_add(RES_SOUND);
    if (r_ptr->flagsr & RFR_RES_CHAO)
        res_add(RES_CHAOS);
    if (r_ptr->flagsr & RFR_RES_NEXU)
        res_add(RES_NEXUS);
    if (r_ptr->flagsr & RFR_RES_DISE)
        res_add(RES_DISEN);
    if (r_ptr->flagsr & RFR_RES_TIME)
        res_add(RES_TIME);
    if (r_ptr->flagsr & RFR_RES_TELE)
        res_add(RES_TELEPORT);
    if (r_ptr->flagsr & RFR_RES_ALL)
        res_add_all();
}

static void _get_flags(u32b flgs[TR_FLAG_SIZE]) 
{
    monster_race *r_ptr = &r_info[p_ptr->current_r_idx];

    if (r_ptr->speed != 110)
        add_flag(flgs, TR_SPEED);

    if (r_ptr->flags9 & RF9_POS_HOLD_LIFE)
        add_flag(flgs, TR_HOLD_LIFE);
    if (r_ptr->flags9 & RF9_POS_TELEPATHY)
        add_flag(flgs, TR_TELEPATHY);
    if (r_ptr->flags9 & RF9_POS_SEE_INVIS)
        add_flag(flgs, TR_SEE_INVIS);
    if (r_ptr->flags9 & RF9_POS_SUST_STR)
        add_flag(flgs, TR_SUST_STR);
    if (r_ptr->flags9 & RF9_POS_SUST_INT)
        add_flag(flgs, TR_SUST_INT);
    if (r_ptr->flags9 & RF9_POS_SUST_WIS)
        add_flag(flgs, TR_SUST_WIS);
    if (r_ptr->flags9 & RF9_POS_SUST_DEX)
        add_flag(flgs, TR_SUST_DEX);
    if (r_ptr->flags9 & RF9_POS_SUST_CON)
        add_flag(flgs, TR_SUST_CON);
    if (r_ptr->flags9 & RF9_POS_SUST_CHR)
        add_flag(flgs, TR_SUST_CHR);

    if (r_ptr->flags2 & RF2_REFLECTING)
        add_flag(flgs, TR_REFLECT);
    if (r_ptr->flags2 & RF2_REGENERATE)
        add_flag(flgs, TR_REGEN);
    if (r_ptr->flags2 & RF2_AURA_FIRE)
        add_flag(flgs, TR_SH_FIRE);
    if (r_ptr->flags2 & RF2_AURA_ELEC)
        add_flag(flgs, TR_SH_ELEC);

    if (r_ptr->flags3 & RF3_AURA_COLD)
        add_flag(flgs, TR_SH_COLD);
    if (r_ptr->flags3 & RF3_NO_FEAR)
        add_flag(flgs, TR_RES_FEAR);
    if (r_ptr->flags3 & RF3_NO_CONF)
        add_flag(flgs, TR_RES_CONF);
    if (r_ptr->flags3 & RF3_NO_SLEEP)
        add_flag(flgs, TR_FREE_ACT);

    if (r_ptr->flags7 & RF7_CAN_FLY)
        add_flag(flgs, TR_LEVITATION);

    if (r_ptr->flagsr & RFR_IM_ACID)
        add_flag(flgs, TR_RES_ACID);
    if (r_ptr->flagsr & RFR_IM_ELEC)
        add_flag(flgs, TR_RES_ELEC);
    if (r_ptr->flagsr & RFR_IM_FIRE)
        add_flag(flgs, TR_RES_FIRE);
    if (r_ptr->flagsr & RFR_IM_COLD)
        add_flag(flgs, TR_RES_COLD);
    if (r_ptr->flagsr & RFR_IM_POIS)
        add_flag(flgs, TR_RES_POIS);
    if (r_ptr->flagsr & RFR_RES_LITE)
        add_flag(flgs, TR_RES_LITE);
    if (r_ptr->flagsr & RFR_RES_DARK)
        add_flag(flgs, TR_RES_DARK);
    if (r_ptr->flagsr & RFR_RES_NETH)
        add_flag(flgs, TR_RES_NETHER);
    if (r_ptr->flagsr & RFR_RES_SHAR)
        add_flag(flgs, TR_RES_SHARDS);
    if (r_ptr->flagsr & RFR_RES_SOUN)
        add_flag(flgs, TR_RES_SOUND);
    if (r_ptr->flagsr & RFR_RES_CHAO)
        add_flag(flgs, TR_RES_CHAOS);
    if (r_ptr->flagsr & RFR_RES_NEXU)
        add_flag(flgs, TR_RES_NEXUS);
    if (r_ptr->flagsr & RFR_RES_DISE)
        add_flag(flgs, TR_RES_DISEN);
    if (r_ptr->flagsr & RFR_RES_TIME)
        add_flag(flgs, TR_RES_TIME);
    if (r_ptr->flagsr & RFR_RES_ALL)
    {
        add_flag(flgs, TR_RES_FIRE);
        add_flag(flgs, TR_RES_COLD);
        add_flag(flgs, TR_RES_ACID);
        add_flag(flgs, TR_RES_ELEC);
        add_flag(flgs, TR_RES_POIS);
        add_flag(flgs, TR_RES_LITE);
        add_flag(flgs, TR_RES_DARK);
        add_flag(flgs, TR_RES_CONF);
        add_flag(flgs, TR_RES_NETHER);
        add_flag(flgs, TR_RES_NEXUS);
        add_flag(flgs, TR_RES_SOUND);
        add_flag(flgs, TR_RES_SHARDS);
        add_flag(flgs, TR_RES_CHAOS);
        add_flag(flgs, TR_RES_DISEN);
        add_flag(flgs, TR_RES_TIME);
    }
}

static void _get_immunities(u32b flgs[TR_FLAG_SIZE]) 
{
    monster_race *r_ptr = &r_info[p_ptr->current_r_idx];
    /*if (r_ptr->flagsr & RFR_IM_ACID)
        add_flag(flgs, TR_RES_ACID);
    if (r_ptr->flagsr & RFR_IM_ELEC)
        add_flag(flgs, TR_RES_ELEC);
    if (r_ptr->flagsr & RFR_IM_FIRE)
        add_flag(flgs, TR_RES_FIRE);
    if (r_ptr->flagsr & RFR_IM_COLD)
        add_flag(flgs, TR_RES_COLD);
    if (r_ptr->flagsr & RFR_IM_POIS)
        add_flag(flgs, TR_RES_POIS);*/
}

static void _get_vulnerabilities(u32b flgs[TR_FLAG_SIZE]) 
{
    monster_race *r_ptr = &r_info[p_ptr->current_r_idx];

    if (r_ptr->flags3 & RF3_HURT_LITE)
        add_flag(flgs, TR_RES_LITE);
    if (r_ptr->flags3 & RF3_HURT_FIRE)
        add_flag(flgs, TR_RES_FIRE);
    if (r_ptr->flags3 & RF3_HURT_COLD)
        add_flag(flgs, TR_RES_COLD);
}

/**********************************************************************
 * Public
 **********************************************************************/
race_t *mon_possessor_get_race_t(void)
{
    static race_t me = {0};
    static bool   init = FALSE;
    static int    last_r_idx = -1;
    int           r_idx = p_ptr->current_r_idx, i;

    if (!init)
    {
        me.name = "Possessor";
        me.desc = "The Possessor is an odd creature, completely harmless in its natural form. However, they "
                    "are capable of possessing the corpses of monsters they have slain, and gain powers and "
                    "abilities based on their current body. As such, they can become quite powerful indeed! "
                    "Unfortunately, not every type of monster will drop a corpse, and getting suitable corspes "
                    "to inhabit can be difficult. If the possessor ever leaves their current body (and they "
                    "must do so to inhabit a new form) then all of their equipment will be removed (except a "
                    "light source) and they will temporarily be in their native, vulnerable state. Finally, "
                    "leaving their current body will destroy that corpse most of the time, so the possessor "
                    "should only do so if they have a better corpse on hand (and also only if there are no "
                    "monsters nearby!).\n \n"
                    "Possessors are monsters and do not choose a normal class. Their stats, skills, resistances "
                    "and spells are completely determined by the body they inhabit. Their current body also "
                    "determines their spell stat (e.g. a novice priest uses wisdom, a novice mage uses intelligence). "
                    "Their current body may offer innate powers (e.g. breath weapons or rockets) in addition to or in lieu "
                    "of magical powers (e.g. mana storms and frost bolts). Be sure to check both the racial power "
                    "command ('U') and the magic command ('m') after possessing a new body.";

        me.exp = 250;

        me.birth = _birth;

        me.get_powers = _get_powers;

        me.calc_bonuses = _calc_bonuses;
        me.get_flags = _get_flags;
        me.get_immunities = _get_immunities;
        me.get_vulnerabilities = _get_vulnerabilities;
        
        me.calc_innate_attacks = _calc_innate_attacks;

        me.flags = RACE_IS_MONSTER;
        init = TRUE;
    }

    if (!r_idx)
        r_idx = MON_POSSESSOR_SOUL; /* Birth hack ... */

    if (r_idx != last_r_idx)
    {
        monster_race *r_ptr;
    
        if (p_ptr->current_r_idx == r_idx) /* Birth hack ... */
            last_r_idx = r_idx;

        r_ptr = &r_info[r_idx];

        me.base_hp = 15;

        me.get_spells = NULL;
        me.caster_info = NULL;
        if (r_ptr->body.spell_stat != A_NONE)
        {
            me.get_spells = _get_spells;
            me.caster_info = _caster_info;
        }

        me.infra = r_ptr->body.infra;
        me.life = r_ptr->body.life;
        if (!me.life)
            me.life = 100;
    
        me.equip_template = mon_get_equip_template();

        for (i = 0; i < MAX_STATS; i++)
            me.stats[i] = r_ptr->body.stats[i];

        me.skills = r_ptr->body.skills;
        me.extra_skills = r_ptr->body.extra_skills;

        me.subname = _mon_name(r_idx);
    }
    return &me;
}

static int _max_lvl(void)
{
    monster_race *r_ptr = &r_info[p_ptr->current_r_idx];
    int           max_lvl = MAX(15, r_ptr->level + 5);

    return MIN(PY_MAX_LEVEL, max_lvl);
}

bool possessor_can_gain_exp(void)
{
    int max = _max_lvl();
    if (max < PY_MAX_LEVEL && p_ptr->lev >= max)
        return FALSE;
    return TRUE;
}

s32b possessor_max_exp(void)
{
    int max = _max_lvl();
    if (max < PY_MAX_LEVEL)
        return exp_requirement(max) - 1;
    else
        return 99999999;
}

void possessor_on_take_hit(void)
{
    /* Getting too wounded may eject the possessor! */
    if ( p_ptr->chp < p_ptr->mhp/4
      && p_ptr->current_r_idx != MON_POSSESSOR_SOUL )
    {
        if (one_in_(66))
        {
            int old_r_idx = p_ptr->current_r_idx;
            monster_race *old_r_ptr = &r_info[old_r_idx];

            msg_print("You can no longer maintain your current body!");
            if (one_in_(3))
            {
                object_type forge;
                object_prep(&forge, lookup_kind(TV_CORPSE, SV_CORPSE));
                apply_magic(&forge, object_level, AM_NO_FIXED_ART);
                forge.pval = old_r_idx;
                forge.weight = MIN(30*1000, MAX(40, old_r_ptr->weight * 10));
                drop_near(&forge, -1, py, px);
            }
            else
                msg_print("Your previous body quickly decays!");

            p_ptr->current_r_idx = MON_POSSESSOR_SOUL;
            p_ptr->chp = p_ptr->mhp;
            p_ptr->chp_frac = 0;
            if (p_ptr->exp > possessor_max_exp())
            {
                p_ptr->exp = possessor_max_exp();
                check_experience();
            }
            else
                restore_level();

            p_ptr->update |= PU_BONUS | PU_HP | PU_MANA;
            p_ptr->redraw |= PR_MAP | PR_BASIC | PR_MANA | PR_EXP | PR_EQUIPPY;
            equip_on_change_race();
        }
        else
        {
            msg_print("You struggle to maintain possession of your current body!");
        }
    }
}
