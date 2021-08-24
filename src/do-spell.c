/* File: do-spell.c */

/* Purpose: Do everything for each spell */

#include "angband.h"

/* Hack: Increase spell power! */
static int _current_realm_hack = 0;

int spell_power_aux(int amt, int bonus)
{
    return MAX(0, amt + amt*bonus/13);
}

int spell_power(int amt)
{
    int tmp = plr->spell_power;
/*  if (_current_realm_hack && _current_realm_hack == plr->easy_realm1)
        tmp += 2; */
    amt = spell_power_aux(amt, tmp);
    if (plr->clp > 1000)
        amt = amt * plr->clp / 1000;
    return amt;
}

int device_power_aux(int pow, int bonus)
{
    return MAX(0, pow + pow*bonus/20);
}

int device_power(int pow)
{
    return device_power_aux(pow, plr->device_power);
}

int spell_cap_aux(int cap, int bonus)
{
    return MAX(0, cap + cap*bonus/20);
}

int spell_cap(int cap)
{
    return spell_cap_aux(cap, plr->spell_cap);
}

/*
 * Generate dice info string such as "foo 2d10"
 */
static cptr info_string_dice(cptr str, int dice, int sides, int base)
{
    /* Fix value */
    if (!dice)
        return format("%s%d", str, base);

    /* Dice only */
    else if (!base)
        return format("%s%dd%d", str, dice, sides);

    /* Dice plus base value */
    else
        return format("%s%dd%d%+d", str, dice, sides, base);
}


/*
 * Generate damage-dice info string such as "dam 2d10"
 */
cptr info_damage(int dice, int sides, int base)
{
    return info_string_dice("dam ", dice, sides, base);
}


/*
 * Generate duration info string such as "dur 20+1d20"
 */
cptr info_duration(int base, int sides)
{
    return format("dur %d+1d%d", base, sides);
}


/*
 * Generate range info string such as "range 5"
 */
cptr info_range(int range)
{
    return format("range %d", range);
}


/*
 * Generate heal info string such as "heal 2d8"
 */
cptr info_heal(int dice, int sides, int base)
{
    if (plr->pclass == CLASS_BLOOD_KNIGHT)
    {
        sides /= 2;
        base /= 2;
    }

    return info_string_dice("heal ", dice, sides, base);
}


/*
 * Generate delay info string such as "delay 15+1d15"
 */
cptr info_delay(int base, int sides)
{
    return format("delay %d+1d%d", base, sides);
}


/*
 * Generate power info string such as "power 100"
 */
cptr info_power(int power)
{
    return format("power %d", power);
}


/*
 * Generate power info string such as "power 1d100"
 */
cptr info_power_dice(int dice, int sides)
{
    return format("power %dd%d", dice, sides);
}


/*
 * Generate radius info string such as "rad 100"
 */
cptr info_radius(int rad)
{
    return format("rad %d", rad);
}


/*
 * Generate weight info string such as "max wgt 15"
 */
cptr info_weight(int weight)
{
    return format("max wgt %d", weight/10);
}


/*
 * Prepare standard probability to become beam for fire_bolt_or_beam()
 */
int beam_chance(void)
{
    if (plr->pclass == CLASS_MAGE || plr->pclass == CLASS_NECROMANCER || plr->pclass == CLASS_YELLOW_MAGE || plr->pclass == CLASS_GRAY_MAGE)
        return plr->lev;
    if (plr->pclass == CLASS_HIGH_MAGE || plr->pclass == CLASS_SORCERER)
        return plr->lev + 10;

    return plr->lev / 2;
}


/*
 * Handle summoning and failure of trump spells
 */
bool trump_summoning(int num, bool pet, point_t pos, int lev, int type, u32b mode)
{
    int plev = plr->lev;

    who_t who = who_create_null();
    int i;
    bool success = FALSE;

    /* Default level */
    if (!lev) lev = spell_power(plev) + randint1(spell_power(plev * 2 / 3));

    if (pet)
    {
        /* Become pet */
        mode |= PM_FORCE_PET;

        /* Only sometimes allow unique monster */
        if (mode & PM_ALLOW_UNIQUE)
        {
            /* Forbid often */
            if (randint1(50 + plev) >= plev / 10)
                mode &= ~PM_ALLOW_UNIQUE;
        }

        /* Player is who summons */
        who = who_create_plr();
    }
    else
    {
        /* Prevent taming, allow unique monster */
        mode |= PM_NO_PET;
    }

    for (i = 0; i < num; i++)
    {
        if (summon_specific(who, pos, lev, type, mode))
            success = TRUE;
    }

    if (!success)
    {
        if (plr->pclass == CLASS_NECROMANCER)
            msg_print("Nobody answers to your foul summons.");
        else
            msg_print("Nobody answers to your Trump call.");
    }

    return success;
}


/*
 * This spell should become more useful (more controlled) as the
 * player gains experience levels. Thus, add 1/5 of the player's
 * level to the die roll. This eliminates the worst effects later on,
 * while keeping the results quite random. It also allows some potent
 * effects only at high level.
 */
static void _wonder(int die, point_t pos)
{
    if (die < 8) plr_bolt(pos, GF_OLD_CLONE, 0);
    else if (die < 14) plr_bolt(pos, GF_OLD_SPEED, plr->lev);
    else if (die < 26) plr_bolt(pos, GF_OLD_HEAL, _4d(6));
    else if (die < 31) plr_bolt(pos, GF_OLD_POLY, plr->lev); 
    else if (die < 36) {
        plr_bolt_or_beam(pos, GF_MISSILE, 
            damroll(3 + (plr->lev - 1)/5, 4) + plr->to_d_spell, 
            beam_chance() - 10); }
    else if (die < 41) plr_bolt(pos, GF_OLD_CONF, plr->lev);
    else if (die < 46) plr_ball(3, pos, GF_POIS, 20 + plr->lev/2 + plr->to_d_spell);
    else if (die < 51) plr_beam(pos, GF_LIGHT_WEAK, _6d(8));
    else if (die < 56) {
        plr_bolt_or_beam(pos, GF_ELEC,
            damroll(3 + (plr->lev - 5)/4, 8) + plr->to_d_spell,
            beam_chance() - 10); }
    else if (die < 61) {
        plr_bolt_or_beam(pos, GF_COLD,
            damroll(5 + (plr->lev - 5)/4, 8) + plr->to_d_spell,
            beam_chance() - 10); }
    else if (die < 66) {
        plr_bolt_or_beam(pos, GF_ACID,
            damroll(6 + (plr->lev - 5)/4, 8) + plr->to_d_spell,
            beam_chance()); }
    else if (die < 71) {
        plr_bolt_or_beam(pos, GF_FIRE,
            damroll(8 + (plr->lev - 5)/4, 8) + plr->to_d_spell,
            beam_chance()); }
    else if (die < 76) plr_bolt(pos, GF_OLD_DRAIN, 75 + plr->to_d_spell);
    else if (die < 81) plr_ball(2, pos, GF_ELEC, 30 + plr->lev/2 + plr->to_d_spell);
    else if (die < 86) plr_ball(2, pos, GF_ACID, 40 + plr->lev + plr->to_d_spell);
    else if (die < 91) plr_ball(3, pos, GF_ICE, 70 + plr->lev + plr->to_d_spell);
    else if (die < 96) plr_ball(3, pos, GF_FIRE, 80 + plr->lev + plr->to_d_spell);
    else if (die < 101) plr_bolt(pos, GF_OLD_DRAIN, 100 + plr->lev + plr->to_d_spell);
    else if (die < 104) earthquake(plr->pos, 12);
    else if (die < 106) destroy_area(plr->pos, 12 + _1d(4), 2*plr->lev);
    else if (die < 108) symbol_genocide(plr->lev + 50, TRUE);
    else if (die < 110) plr_project_los(GF_DISP_ALL, 120);
    else /* RARE */
    {
        plr_project_los(GF_DISP_ALL, 150 + plr->to_d_spell);
        plr_project_los(GF_SLOW, plr->lev);
        plr_project_los(GF_SLEEP, plr->lev);
        hp_player(300);
    }
}
bool cast_wonder(void)
{
    point_t pos;
    int die = randint1(100) + plr->lev/5;
    int vir = virtue_current(VIRTUE_CHANCE);

    /* always prompt for a target even if the effect doesn't need one */
    pos = get_fire_pos_aux(TARGET_KILL | TARGET_BALL); /* <== assume a ball effect (PROJECT_STOP on a 'dir' entry) */
    if (!dun_pos_interior(cave, pos)) return FALSE;

    if (vir > 0)
    {
        while (randint1(400) < vir) die++;
    }
    else if (vir < 0)
    {
        while (randint1(400) < -vir) die--;
    }

    if (plr->pclass == CLASS_WILD_TALENT)
        die += randint1(25 + plr->lev/2);

    if (die < 26)
        virtue_add(VIRTUE_CHANCE, 1);

    if (die > 100)
    {
        msg_print("You feel a surge of power!");
    }

    _wonder(die, pos);
    return TRUE;
}


static bool cast_invoke_spirits(void)
{
    point_t pos;
    int die = randint1(100) + plr->lev/5;
    int vir = virtue_current(VIRTUE_CHANCE);

    /* always prompt for a target even if the effect doesn't need one */
    pos = get_fire_pos_aux(TARGET_KILL | TARGET_BALL); /* <== assume a ball effect (PROJECT_STOP on a 'dir' entry) */
    if (!dun_pos_interior(cave, pos)) return FALSE;

    if (vir > 0)
    {
        while (randint1(400) < vir) die++;
    }
    else if (vir < 0)
    {
        while (randint1(400) < -vir) die--;
    }

    msg_print("You call on the power of the dead...");

    if (die < 26)
        virtue_add(VIRTUE_CHANCE, 1);

    if (die > 100)
    {
        msg_print("You feel a surge of eldritch force!");
    }


    if (die < 8)
    {
        msg_print("Oh no! Mouldering forms rise from the earth around you!");

        (void)summon_specific(who_create_null(), plr->pos, cave->dun_lvl, SUMMON_UNDEAD, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_NO_PET));
        virtue_add(VIRTUE_UNLIFE, 1);
    }
    else if (die < 14)
    {
        msg_print("An unnamable evil brushes against your mind...");

        fear_add_p(FEAR_TERRIFIED);
    }
    else if (die < 26)
    {
        msg_print("Your head is invaded by a horde of gibbering spectral voices...");

        plr_tim_add(T_CONFUSED, randint1(4) + 4);
    }
    else _wonder(die, pos);
    if (die < 31)
    {
        msg_print("Sepulchral voices chuckle. 'Soon you will join us, mortal.'");
    }
    return TRUE;
}


static void wild_magic(int spell)
{
    int counter = 0;
    int type = SUMMON_BIZARRE1 + randint0(6);

    if (type < SUMMON_BIZARRE1) type = SUMMON_BIZARRE1;
    else if (type > SUMMON_BIZARRE6) type = SUMMON_BIZARRE6;

    switch (randint1(spell) + randint1(8) + 1)
    {
    case 1:
    case 2:
    case 3:
        teleport_player(10, TELEPORT_PASSIVE);
        break;
    case 4:
    case 5:
    case 6:
        teleport_player(100, TELEPORT_PASSIVE);
        break;
    case 7:
    case 8:
        teleport_player(200, TELEPORT_PASSIVE);
        break;
    case 9:
    case 10:
    case 11:
        unlite_area(10, 3);
        break;
    case 12:
    case 13:
    case 14:
        lite_area(damroll(2, 3), 2);
        break;
    case 15:
        destroy_doors_touch();
        break;
    case 16: case 17:
        wall_breaker();
        break;
    case 18:
        plr_burst(1, GF_SLEEP, plr->lev);
        break;
    case 19:
    case 20:
        trap_creation(plr->pos);
        break;
    case 21:
    case 22:
        door_creation();
        break;
    case 23:
    case 24:
    case 25:
        aggravate_monsters(who_create_null());
        break;
    case 26:
        earthquake(plr->pos, 5);
        break;
    case 27:
    case 28:
        mut_gain_random(NULL);
        break;
    case 29:
    case 30:
        apply_disenchant(1);
        break;
    case 31:
        lose_all_info();
        break;
    case 32:
        plr_ball(1 + spell/10, plr->pos, GF_CHAOS, spell + 5);
        break;
    case 33:
        wall_stone();
        break;
    case 34:
    case 35:
        while (counter++ < 8)
        {
            (void)summon_specific(who_create_null(), plr->pos, (cave->dun_lvl * 3) / 2, type, (PM_ALLOW_GROUP | PM_NO_PET));
        }
        break;
    case 36:
    case 37:
        activate_hi_summon(plr->pos, FALSE);
        break;
    case 38:
        (void)summon_cyber(who_create_plr(), plr->pos);
        break;
    default:
        {
            int count = 0;
            (void)activate_ty_curse(FALSE, &count);
            break;
        }
    }
}


static void cast_shuffle(void)
{
    int plev = plr->lev;
    int die;
    int vir = virtue_current(VIRTUE_CHANCE);
    int i;

    /* Card sharks and high mages get a level bonus */
    if ((plr->pclass == CLASS_ROGUE) ||
        (plr->pclass == CLASS_HIGH_MAGE) ||
        (plr->pclass == CLASS_SORCERER))
        die = (randint1(110)) + plev / 5;
    else
        die = randint1(120);


    if (vir > 0)
    {
        while (randint1(400) < vir) die++;
    }
    else if (vir < 0)
    {
        while (randint1(400) < -vir) die--;
    }

    msg_print("You shuffle the deck and draw a card...");

    if (die < 30)
        virtue_add(VIRTUE_CHANCE, 1);

    if (die < 7)
    {
        msg_print("Oh no! It's Death!");

        for (i = 0; i < randint1(3); i++)
            activate_hi_summon(plr->pos, FALSE);
    }
    else if (die < 14)
    {
        msg_print("Oh no! It's the Devil!");

        summon_specific(who_create_null(), plr->pos, cave->dun_lvl, SUMMON_DEMON, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_NO_PET));
    }
    else if (die < 18)
    {
        int count = 0;
        msg_print("Oh no! It's the Hanged Man.");

        activate_ty_curse(FALSE, &count);
    }
    else if (die < 22)
    {
        msg_print("It's the swords of discord.");

        aggravate_monsters(who_create_null());
    }
    else if (die < 26)
    {
        msg_print("It's the Fool.");

        do_dec_stat(A_INT);
        do_dec_stat(A_WIS);
    }
    else if (die < 30)
    {
        msg_print("It's the picture of a strange monster.");

        trump_summoning(1, FALSE, plr->pos, (cave->dun_lvl * 3 / 2), (32 + randint1(6)), PM_ALLOW_GROUP | PM_ALLOW_UNIQUE);
    }
    else if (die < 33)
    {
        msg_print("It's the Moon.");

        unlite_area(10, 3);
    }
    else if (die < 38)
    {
        msg_print("It's the Wheel of Fortune.");

        wild_magic(randint0(32));
    }
    else if (die < 40)
    {
        msg_print("It's a teleport trump card.");

        teleport_player(10, TELEPORT_PASSIVE);
    }
    else if (die < 42)
    {
        msg_print("It's Justice.");

        plr_tim_add(T_BLESSED, plr->lev);
    }
    else if (die < 47)
    {
        msg_print("It's a teleport trump card.");

        teleport_player(100, TELEPORT_PASSIVE);
    }
    else if (die < 52)
    {
        msg_print("It's a teleport trump card.");

        teleport_player(200, TELEPORT_PASSIVE);
    }
    else if (die < 60)
    {
        msg_print("It's the Tower.");

        wall_breaker();
    }
    else if (die < 72)
    {
        msg_print("It's Temperance.");
        plr_burst(1, GF_SLEEP, plr->lev);
    }
    else if (die < 80)
    {
        msg_print("It's the Tower.");

        earthquake(plr->pos, 5);
    }
    else if (die < 82)
    {
        msg_print("It's the picture of a friendly monster.");

        trump_summoning(1, TRUE, plr->pos, (cave->dun_lvl * 3 / 2), SUMMON_BIZARRE1, 0L);
    }
    else if (die < 84)
    {
        msg_print("It's the picture of a friendly monster.");

        trump_summoning(1, TRUE, plr->pos, (cave->dun_lvl * 3 / 2), SUMMON_BIZARRE2, 0L);
    }
    else if (die < 86)
    {
        msg_print("It's the picture of a friendly monster.");

        trump_summoning(1, TRUE, plr->pos, (cave->dun_lvl * 3 / 2), SUMMON_BIZARRE4, 0L);
    }
    else if (die < 88)
    {
        msg_print("It's the picture of a friendly monster.");

        trump_summoning(1, TRUE, plr->pos, (cave->dun_lvl * 3 / 2), SUMMON_BIZARRE5, 0L);
    }
    else if (die < 96)
    {
        msg_print("It's the Lovers.");
        plr_cast_bolt(GF_CHARM, dice_create(0, 0, MIN(plr->lev, 20)));
    }
    else if (die < 101)
    {
        msg_print("It's the Hermit.");

        wall_stone();
    }
    else if (die < 111)
    {
        msg_print("It's the Judgement.");

        do_cmd_rerate(FALSE);
        mut_lose_all();
    }
    else if (die < 120)
    {
        msg_print("It's the Sun.");

        virtue_add(VIRTUE_KNOWLEDGE, 1);
        virtue_add(VIRTUE_ENLIGHTENMENT, 1);
        wiz_lite();
    }
    else
    {
        msg_print("It's the World.");

        if (plr->exp < PY_MAX_EXP)
        {
            s32b ee = (plr->exp / 25) + 1;
            if (ee > 5000) ee = 5000;
            msg_print("You feel more experienced.");

            gain_exp(ee);
        }
    }
}

/*
 * An "item_tester_hook" for offer
 */
static bool item_tester_offer(object_type *o_ptr)
{
    if (o_ptr->tval != TV_CORPSE) return FALSE;
    if (o_ptr->sval != SV_CORPSE) return FALSE;
    return corpse_race_is_char_ex(o_ptr, "pht");
}


/*
 * Daemon spell Summon Greater Demon
 */
bool cast_summon_greater_demon(void)
{
    obj_prompt_t prompt = {0};
    int plev = plr->lev;
    int summon_lev;

    prompt.prompt = "Sacrifice which corpse?";
    prompt.error = "You have nothing to sacrifice.";
    prompt.filter = item_tester_offer;
    prompt.where[0] = INV_PACK;
    prompt.where[1] = INV_FLOOR;

    obj_prompt(&prompt);
    if (!prompt.obj) return FALSE;

    summon_lev = plev * 2 / 3 + mon_race_lookup(prompt.obj->race_id)->alloc.lvl;

    if (summon_specific(who_create_plr(), plr->pos, summon_lev, SUMMON_HI_DEMON, (PM_ALLOW_GROUP | PM_FORCE_PET)))
    {
        msg_print("The area fills with a stench of sulphur and brimstone.");
        msg_print("'What is thy bidding... Master?'");

        prompt.obj->number--;
        obj_release(prompt.obj, 0);
    }
    else
    {
        msg_print("No Greater Demon arrives.");
    }

    return TRUE;
}



static cptr do_life_spell(int spell, int mode)
{
    bool name = (mode == SPELL_NAME) ? TRUE : FALSE;
    bool desc = (mode == SPELL_DESC) ? TRUE : FALSE;
    bool info = (mode == SPELL_INFO) ? TRUE : FALSE;
    bool cast = (mode == SPELL_CAST) ? TRUE : FALSE;

    int rad = 0;
    dice_t dice = {0};

    dice.scale = spell_power(1000);
    dice.scale += virtue_current(VIRTUE_FAITH)/4;
    dice.scale += virtue_current(VIRTUE_VITALITY)/4;
    dice.scale -= virtue_current(VIRTUE_UNLIFE)/2;

    switch (spell)
    {
    case 0:
        if (name) return "Cure Light Wounds";
        if (desc) return "Heals cut and HP a little.";
        dice.dd = 2;
        dice.ds = 10;
        if (info) return dice_info_heal(dice);
        if (cast) {
            hp_player(dice_roll(dice));
            plr_tim_subtract(T_CUT, 10);
        }
        break;
    case 1:
        if (name) return "Bless";
        if (desc) return "Gives bonus to hit and AC for a few turns.";
        dice.dd = 1;
        dice.ds = 12;
        dice.base = 12;
        if (info) return dice_info_dur(dice);
        if (cast) plr_tim_add(T_BLESSED, dice_roll(dice));
        break;
    case 2:
        if (name) return "Regeneration";
        if (desc) return "Gives regeneration ability for a while.";
        dice.dd = 1;
        dice.ds = 80;
        dice.base = 80;
        if (info) return dice_info_dur(dice);
        if (cast) plr_tim_add(T_REGEN, dice_roll(dice));
        break;
    case 3:
        if (name) return "Call Light";
        if (desc) return "Lights up nearby area and the inside of a room permanently.";
        dice.dd = 2;
        dice.ds = MAX(1, plr->lev/2);
        rad = 1 + plr->lev/10;
        if (info) return dice_info_dam(dice);
        if (cast) lite_area(dice_roll(dice), rad);
        break;
    case 4:
        if (name) return "Detect Doors & Traps";
        if (desc) return "Detects traps, doors, and stairs in your vicinity.";
        rad = DETECT_RAD_DEFAULT;
        if (info) return info_radius(rad);
        if (cast) {
            detect_traps(rad, TRUE);
            detect_doors(rad);
            detect_stairs(rad);
            detect_recall(rad);
        }
        break;
    case 5:
        if (name) return "Cure Medium Wounds";
        if (desc) return "Heals cut and HP more.";
        dice.dd = 6;
        dice.ds = 10;
        if (info) return dice_info_heal(dice);
        if (cast) {
            hp_player(dice_roll(dice));
            plr_tim_recover(T_CUT, 50, 0);
            plr_tim_subtract(T_CUT, 20);
        }
        break;
    case 6:
        if (name) return "Cure Poison";
        if (desc) return "Cure poison status.";
        if (cast) plr_tim_recover(T_POISON, 80, 100);
        break;
    case 7:
        if (name) return "Satisfy Hunger";
        if (desc) return "Satisfies hunger.";
        if (cast) set_food(PY_FOOD_MAX - 1);
        break;
    case 8:
        if (name) return "Remove Curse";
        if (desc) return "Removes normal curses from equipped items.";
        if (cast && remove_curse())
            msg_print("You feel as if someone is watching over you.");
        break;
    case 9:
        if (name) return "Fasting";
        if (desc) return "Begin a religious fast. In time, your god may restore you!";
        if (cast)
        {
            if (plr->fasting)
            {
                msg_print("You are already fasting. Perhaps you should pray as well?");
                return NULL;
            }
            msg_print("You begin to fast.");
            set_food(plr->food/2);
            plr->redraw |= PR_EFFECTS;
            plr->fasting = TRUE;
        }
        break;
    case 10:
        if (name) return "Cure Critical Wounds";
        if (desc) return "Heals cut, stun and HP greatly.";
        dice.dd = 12;
        dice.ds = 12;
        if (info) return dice_info_heal(dice);
        if (cast) {
            hp_player(dice_roll(dice));
            plr_tim_remove(T_STUN);
            plr_tim_remove(T_CUT);
        }
        break;
    case 11:
        if (name) return "Resist Heat and Cold";
        if (desc) return "Gives resistance to fire and cold.";
        dice.dd = 1;
        dice.ds = 20;
        dice.base = 20;
        if (info) return dice_info_dur(dice);
        if (cast) {
            plr_tim_add(T_RES_FIRE, dice_roll(dice));
            plr_tim_add(T_RES_COLD, dice_roll(dice));
        }
        break;
    case 12:
        if (name) return "Sense Surroundings";
        if (desc) return "Maps nearby area.";
        rad = DETECT_RAD_MAP;
        if (info) return info_radius(rad);
        if (cast) map_area(rad);
        break;
    case 13:
        dice.base = plr->lev;
        if (name) return "Turn Undead";
        if (desc) return "Attempts to scare undead monsters in sight.";
        if (info) return dice_info_power(dice);
        if (cast) plr_project_los(GF_TURN_UNDEAD, dice_roll(dice));
        break;
    case 14:
        if (name) return "Healing";
        if (desc) return "Very powerful healing magic that also cures cuts and stunning.";
        dice.base = 300;
        if (info) return dice_info_heal(dice);
        if (cast) {
            hp_player(dice_roll(dice));
            plr_tim_remove(T_STUN);
            plr_tim_remove(T_CUT);
        }
        break;
    case 15:
        if (name) return "Glyph of Warding";
        if (desc) return "Sets a glyph on the floor beneath you. Monsters cannot attack you if you are on a glyph, but can try to break glyph.";
        if (cast) warding_glyph();
        break;
    case 16:
        if (name) return "Dispel Curse";
        if (desc) return "Removes normal and heavy curse from equipped items.";
        if (cast && remove_all_curse())
            msg_print("You feel as if someone is watching over you.");
        break;
    case 17:
        if (name) return "Perception";
        if (desc) return "Identifies an item.";
        if (cast && !ident_spell(NULL)) return NULL;
        break;
    case 18:
        if (name) return "Dispel Undead";
        if (desc) return "Damages all undead monsters in sight.";
        dice.base = 3*plr->lev + plr->to_d_spell;
        if (info) return dice_info_dam(dice);
        if (cast) plr_project_los(GF_DISP_UNDEAD, dice_roll(dice));
        break;
    case 19:
        if (name) return "Sustaining";
        if (desc) return "Temporarily sustains your stats and life force.";
        dice.base = plr->lev;
        if (info) return dice_info_dur(dice);
        if (cast) plr_tim_add(T_SUSTAIN, dice_roll(dice));
        break;
    case 20:
        if (name) return "Cure Mutation";
        if (desc) return "Remove a random mutation.";
        if (cast) {
            if (one_in_(100/plr->lev))
                mut_lose_random(mut_bad_pred);
            else
                mut_lose_random(NULL);
        }
        break;
    case 21:
        if (name) return "Word of Recall";
        if (desc) return "Recalls player from dungeon to town, or from town to the deepest level of dungeon.";
        if (cast) {
            if (!get_check("Are you sure? (Recall) ")) return NULL;
            if (!dun_mgr_recall_plr()) return NULL;
        }
        break;
    case 22:
        if (name) return "*Regeneration*";
        if (desc) return "For a short while you will regain health before every action.";
        dice.dd = 1;
        dice.ds = 10;
        dice.base = 10;
        if (info) return dice_info_dur(dice);
        if (cast) plr_tim_add_aux(T_STAR_REGEN, dice_roll(dice), 25);
        break;
    case 23:
        if (name) return "Warding True";
        if (desc) return "Creates glyphs in all adjacent squares and under you.";
        if (cast) {
            warding_glyph();
            glyph_creation();
        }
        break;
    case 24:
        if (name) return "Sterilization";
        if (desc) return "Prevents any breeders on current level from breeding.";
        if (cast) cave->breed_ct += MAX_REPRO;
        break;
    case 25:
        if (name) return "Detection";
        if (desc) return "Detects all monsters, traps, doors, stairs, treasures and items in your vicinity.";
        rad = DETECT_RAD_DEFAULT;
        if (info) return info_radius(rad);
        if (cast) detect_all(rad);
        break;
    case 26:
        if (name) return "Annihilate Undead";
        if (desc) return "Eliminates all nearby undead monsters, exhausting you. Powerful or unique monsters may be able to resist.";
        dice.base = 50 + plr->lev;
        if (info) return dice_info_power(dice);
        if (cast) mass_genocide_undead(dice_roll(dice), TRUE);
        break;
    case 27:
        if (name) return "Clairvoyance";
        if (desc) return "Maps and lights whole dungeon level. Knows all objects location.";
        if (cast) wiz_lite();
        break;
    case 28:
        if (name) return "Restoration";
        if (desc) return "Restores all stats, life and experience.";
        if (cast) {
            do_res_stat(A_STR);
            do_res_stat(A_INT);
            do_res_stat(A_WIS);
            do_res_stat(A_DEX);
            do_res_stat(A_CON);
            do_res_stat(A_CHR);
            restore_level();
            plr_restore_life(1000);
        }
        break;
    case 29:
        if (name) return "Healing True";
        if (desc) return "The greatest healing magic. Heals all HP, cuts and stunning.";
        dice.base = 2000;
        if (info) return dice_info_heal(dice);
        if (cast) {
            hp_player(dice_roll(dice));
            plr_tim_remove(T_STUN);
            plr_tim_remove(T_CUT);
        }
        break;
    case 30:
        if (name) return "Holy Vision";
        if (desc) return "Fully identifies an item.";
        if (cast && !identify_fully(NULL)) return NULL;
        break;
    case 31:
        if (name) return "Ultimate Resistance";
        if (desc) return "Gives resistance to almost everything as well enhanced armor class.";
        dice.dd = 1;
        dice.ds = plr->lev/2;
        dice.base = plr->lev/2;
        if (info) return dice_info_dur(dice);
        if (cast) plr_tim_add(T_ULT_RES, dice_roll(dice));
        break;
    }
    return "";
}

static cptr do_sorcery_spell(int spell, int mode)
{
    bool name = (mode == SPELL_NAME) ? TRUE : FALSE;
    bool desc = (mode == SPELL_DESC) ? TRUE : FALSE;
    bool info = (mode == SPELL_INFO) ? TRUE : FALSE;
    bool cast = (mode == SPELL_CAST) ? TRUE : FALSE;

    int rad = 0;
    int rng = DUN_PATH_MAX;
    dice_t dice = {0};

    dice.scale = spell_power(1000);
    dice.scale += virtue_current(VIRTUE_KNOWLEDGE) / 2; /* roughly +/- 6% */

    switch (spell)
    {
    case 0:
        if (name) return "Detect Monsters";
        if (desc) return "Detects all monsters in your vicinity unless invisible.";
        rad = DETECT_RAD_DEFAULT;
        if (info) return info_radius(rad);
        if (cast) detect_monsters_normal(rad);
        break;
    case 1:
        if (name) return "Phase Door";
        if (desc) return "Teleport short distance.";
        rng = 10;
        if (info) return info_range(rng);
        if (cast) {
            if (mut_present(MUT_ASTRAL_GUIDE))
                energy_use /= 3;
            teleport_player(rng, 0);
        }
        break;
    case 2:
        if (name) return "Detect Doors and Traps";
        if (desc) return "Detects traps, doors, and stairs in your vicinity.";
        rad = DETECT_RAD_DEFAULT;
        if (info) return info_radius(rad);
        if (cast) {
            detect_traps(rad, TRUE);
            detect_doors(rad);
            detect_stairs(rad);
            detect_recall(rad);
        }
        break;
    case 3:
        if (name) return "Light Area";
        if (desc) return "Lights up nearby area and the inside of a room permanently.";
        dice.dd = 2;
        dice.ds = MAX(1, plr->lev/2);
        rad = 1 + plr->lev/10;
        if (info) return dice_info_dam(dice);
        if (cast) lite_area(dice_roll(dice), rad);
        break;
    case 4:
        if (name) return "Confuse Monster";
        if (desc) return "Attempts to confuse a monster.";
        dice.base = 3*plr->lev/2;
        if (info) return dice_info_power(dice);
        if (cast && !plr_cast_bolt(GF_OLD_CONF, dice)) return NULL;
        break;
    case 5:
        if (name) return "Teleport";
        if (desc) return "Teleport long distance.";
        rng = 5*plr->lev;
        if (info) return info_range(rng);
        if (cast) {
            if (mut_present(MUT_ASTRAL_GUIDE)) energy_use /= 3;
            teleport_player(rng, 0);
        }
        break;
    case 6:
        if (name) return "Sleep Monster";
        if (desc) return "Attempts to sleep a monster.";
        dice.base = 3*plr->lev/2;
        if (info) return dice_info_power(dice);
        if (cast && !plr_cast_bolt(GF_SLEEP, dice)) return NULL;
        break;
    case 7:
        if (name) return "Recharging";
        if (desc) return "It attempts to recharge a device using your mana for power.";
        dice.base = 30 + 120*plr->lev/50; /* cf _recharge_aux: spell should have some chance of working at CL5 */
        if (info) return dice_info_power(dice);
        if (cast && !recharge_from_player(dice_roll(dice))) return NULL;
        break;
    case 8:
        if (name) return "Magic Mapping";
        if (desc) return "Maps nearby area.";
        rad = DETECT_RAD_MAP;
        if (info) return info_radius(rad);
        if (cast) map_area(rad);
        break;
    case 9:
        if (name) return plr->lev < 30 ? "Identify" : "Mass Identify";
        if (desc) return plr->lev < 30 ? "Identifies an item." : "Identifies all items in your pack";
        if (cast) {
            if (plr->lev < 30) 
            {
                if (!ident_spell(NULL))
                    return NULL;
            }
            else mass_identify(FALSE);
        }
        break;
    case 10:
        if (name) return "Slow Monster";
        if (desc) return "Attempts to slow a monster.";
        dice.base = 2*plr->lev;
        if (info) return dice_info_power(dice);
        if (cast && !plr_cast_bolt(GF_SLOW, dice)) return NULL;
        break;
    case 11:
        if (name) return "Mass Sleep";
        if (desc) return "Attempts to sleep all monsters in sight.";
        dice.base = 4*plr->lev;
        if (info) return dice_info_power(dice);
        if (cast) plr_project_los(GF_SLEEP, dice_roll(dice));
        break;
    case 12:
        if (name) return "Teleport Away";
        if (desc) return "Teleports all monsters on the line away unless resisted.";
        dice.base = 2*plr->lev;
        if (info) return dice_info_power(dice);
        if (cast && !plr_cast_beam(GF_TELEPORT, dice)) return NULL;
        break;
    case 13:
        if (name) return "Haste Self";
        if (desc) return "Hastes you for a while.";
        dice.dd = 1;
        dice.ds = 20 + plr->lev;
        dice.base = plr->lev;
        if (info) return dice_info_dur(dice);
        if (cast) plr_tim_add(T_FAST, dice_roll(dice));
        break;
    case 14:
        if (name) return "Detection True";
        if (desc) return "Detects all monsters, traps, doors, stairs, treasures and items in your vicinity.";
        rad = DETECT_RAD_DEFAULT;
        if (info) return info_radius(rad);
        if (cast) detect_all(rad);
        break;
    case 15:
        if (name) return "Identify True";
        if (desc) return "*Identifies* an item.";
        if (cast && !identify_fully(NULL)) return NULL;
        break;
    case 16:
        if (name) return "Inventory Protection";
        if (desc) return "For a short while, items in your pack have a chance to resist destruction.";
        dice.dd = 1;
        dice.ds = 30;
        dice.base = 30;
        if (info) return dice_info_dur(dice);
        if (cast) plr_tim_add(T_INV_PROT, dice_roll(dice));
        break;
    case 17:
        if (name) return "Stair Creation";
        if (desc) return "Creates a stair which goes down or up.";
        if (cast) dun_create_stairs(cave, FALSE);
        break;
    case 18:
        if (name) return "Sense Minds";
        if (desc) return "Gives telepathy for a while.";
        dice.dd = 1;
        dice.ds = 30;
        dice.base = 25;
        if (info) return dice_info_dur(dice);
        if (cast) plr_tim_add(T_TELEPATHY, dice_roll(dice));
        break;
    case 19:
        if (name) return "Teleport to Town";
        if (desc) return "Teleport instantaneously to selected town.";
        if (cast && !dun_mgr_teleport_town(TF_SECRET | TF_VISITED)) return NULL;
        break;
    case 20:
        if (name) return "Self Knowledge";
        if (desc) return "Gives you useful info regarding your current resistances, the powers of your weapon and maximum limits of your stats.";
        if (cast) self_knowledge();
        break;
    case 21:
        if (name) return "Teleport Level";
        if (desc) return "Teleport to up or down stairs in a moment.";
        if (cast) {
            if (!get_check("Are you sure? (Teleport Level) ")) return NULL;
            dun_teleport_level_plr(cave);
        }
        break;
    case 22:
        if (name) return "Word of Recall";
        if (desc) return "Recalls player from dungeon to town, or from town to the deepest level of dungeon.";
        if (cast) {
            if (!get_check("Are you sure? (Recall) ")) return NULL;
            if (!dun_mgr_recall_plr()) return NULL;
        }
        break;
    case 23:
        if (name) return "Dimension Door";
        if (desc) return "Teleport to given location.";
        dice.base = 10 + plr->lev/2;
        if (info) return dice_info_range(dice);
        if (cast) {
            msg_print("You open a dimensional gate. Choose a destination.");
            if (!dimension_door(dice_roll(dice))) return NULL;
        }
        break;
    case 24:
        if (name) return "Probing";
        if (desc) return "Proves all monsters' alignment, HP, speed and their true character.";
        if (cast) probing();
        break;
    case 25:
        if (name) return "Door Creation";
        if (desc) return "Creates doors on all surrounding squares.";
        if (cast) plr_burst(1, GF_MAKE_DOOR, 0);
        break;
    case 26:
        if (name) return "Telekinesis";
        if (desc) return "Pulls a distant item close to you.";
        dice.base = 15*plr->lev;
        if (info) return info_weight(dice_roll(dice));
        if (cast) {
            int dir;
            if (!get_aim_dir(&dir)) return NULL;
            fetch(dir, dice_roll(dice), FALSE);
        }
        break;
    case 27:
        if (name) return "Clairvoyance";
        if (desc) return "Maps and lights whole dungeon level. Knows all objects location. And gives telepathy for a while.";
        dice.dd = 1;
        dice.ds = 30;
        dice.base = 25;
        if (info) return dice_info_dur(dice);
        if (cast) {
            virtue_add(VIRTUE_KNOWLEDGE, 1);
            virtue_add(VIRTUE_ENLIGHTENMENT, 1);
            wiz_lite();
            plr_tim_add(T_TELEPATHY, dice_roll(dice));
        }
        break;
    case 28:
        if (name) return "Device Mastery";
        if (desc) return "For a very short time, your magical devices are more powerful.";
        dice.dd = 1;
        dice.ds = plr->lev/10;
        dice.base = plr->lev/10;
        if (info) return dice_info_dur(dice);
        if (cast) plr_tim_add(T_DEVICE_POWER, dice_roll(dice));
        break;
    case 29:
        if (name) return "Alchemy";
        if (desc) return "Turns an item into 1/3 of its value in gold.";
        if (cast && !alchemy()) return NULL;
        break;
    case 30:
        if (name) return "Banishment";
        if (desc) return "Teleports all monsters in sight away unless resisted.";
        dice.base = 4*plr->lev;
        if (info) return dice_info_power(dice);
        if (cast) plr_project_los(GF_TELEPORT, dice_roll(dice));
        break;
    case 31:
        if (name) return "Globe of Invulnerability";
        if (desc) return "Generates barrier which completely protect you from almost all damages. Takes a few your turns when the barrier breaks or duration time is exceeded.";
        dice.dd = 1;
        dice.ds = 1000;
        dice.base = 500;
        if (cast) plr_tim_add(T_INVULN, dice_roll(dice));
        break;
    }
    return "";
}

static cptr do_nature_spell(int spell, int mode)
{
    bool name = (mode == SPELL_NAME) ? TRUE : FALSE;
    bool desc = (mode == SPELL_DESC) ? TRUE : FALSE;
    bool info = (mode == SPELL_INFO) ? TRUE : FALSE;
    bool cast = (mode == SPELL_CAST) ? TRUE : FALSE;

    int rad = 0;
    int rng = DUN_PATH_MAX;
    dice_t dice = {0};

    dice.scale = spell_power(1000);
    dice.scale += virtue_current(VIRTUE_NATURE) / 2; /* roughly +/- 6% */

    switch (spell)
    {
    case 0:
        rad = DETECT_RAD_DEFAULT;
        if (name) return "Detect Creatures";
        if (desc) return "Detects all monsters in your vicinity unless invisible.";
        if (info) return info_radius(rad);
        if (cast) detect_monsters_normal(rad);
        break;
    case 1:
        if (name) return "Lightning";
        if (desc) return "Fires a short beam of lightning.";
        dice.dd = 3 + (plr->lev - 1)/5;
        dice.ds = 4;
        dice.base = plr->to_d_spell;
        rng = spell_power(plr->lev/6 + 2);
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_beam_aux(GF_ELEC, dice, rng)) return NULL;
        break;
    case 2:
        if (name) return "Detect Doors and Traps";
        if (desc) return "Detects traps, doors, and stairs in your vicinity.";
        rad = DETECT_RAD_DEFAULT;
        if (info) return info_radius(rad);
        if (cast) {
            detect_traps(rad, TRUE);
            detect_doors(rad);
            detect_stairs(rad);
            detect_recall(rad);
        }
        break;
    case 3:
        if (name) return "Produce Food";
        if (desc) return "Produces a Ration of Food.";
        if (cast) {
            obj_t obj = {0};
            msg_print("A food ration is produced."); /* before "something rolls beneath your feet" */
            object_prep(&obj, lookup_kind(TV_FOOD, SV_FOOD_RATION));
            dun_drop_near(cave, &obj, plr->pos);
        }
        break;
    case 4:
        if (name) return "Daylight";
        if (desc) return "Lights up nearby area and the inside of a room permanently.";
        dice.dd = 2;
        dice.ds = plr->lev/2;
        rad = plr->lev/10 + 1;
        if (info) return dice_info_dam(dice);
        if (cast) {
            lite_area(dice_roll(dice), rad);
            if ( (prace_is_(RACE_VAMPIRE) || prace_is_(RACE_MON_VAMPIRE) || plr->mimic_form == MIMIC_VAMPIRE)
              && !res_save_default(GF_LIGHT) )
            {
                msg_print("The light scorches your flesh!");
                take_hit(DAMAGE_NOESCAPE, damroll(2, 2), "light");
            }
        }
        break;
    case 5:
        if (name) return "Wind Walker";
        if (desc) return "Grants temporary levitation.";
        dice.dd = 1;
        dice.ds = 30;
        dice.base = 30;
        if (info) return dice_info_dur(dice);
        if (cast) plr_tim_add(T_LEVITATION, dice_roll(dice));
        break;
    case 6:
        if (name) return "Resist Environment";
        if (desc) return "Gives resistance to fire, cold and electricity for a while.";
        dice.dd = 1;
        dice.ds = 20;
        dice.base = 20;
        if (info) return dice_info_dur(dice);
        if (cast) {
            plr_tim_add(T_RES_COLD, dice_roll(dice));
            plr_tim_add(T_RES_FIRE, dice_roll(dice));
            plr_tim_add(T_RES_ELEC, dice_roll(dice));
        }
        break;
    case 7:
        if (name) return "Cure Wounds & Poison";
        if (desc) return "Heals all cut and poison status. Heals HP a little.";
        dice.dd = 2;
        dice.ds = 8;
        if (info) return dice_info_heal(dice);
        if (cast) {
            hp_player(dice_roll(dice));
            plr_tim_remove(T_CUT);
            plr_tim_recover(T_POISON, 80, 100);
        }
        break;
    case 8:
        if (name) return "Stone to Mud";
        if (desc) return "Turns one rock square to mud.";
        dice.dd = 1;
        dice.ds = 30;
        dice.base = 20 + plr->to_d_spell;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_beam(GF_KILL_WALL, dice)) return NULL;
        break;
    case 9:
        if (name) return "Frost Bolt";
        if (desc) return "Fires a bolt or beam of cold.";
        dice.dd = 3 + (plr->lev - 5)/4;
        dice.ds = 8;
        dice.base = plr->to_d_spell;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_bolt_or_beam(GF_COLD, dice, beam_chance() - 10)) return NULL;
        break;
    case 10:
        if (name) return "Nature Awareness";
        if (desc) return "Maps nearby area. Detects all monsters, traps, doors and stairs.";
        rad = DETECT_RAD_DEFAULT;
        if (info) return info_radius(rad);
        if (cast) {
            map_area(rad);
            detect_traps(rad, TRUE);
            detect_doors(rad);
            detect_stairs(rad);
            detect_recall(rad);
            detect_monsters_normal(rad);
        }
        break;
    case 11:
        if (name) return "Fire Bolt";
        if (desc) return "Fires a bolt or beam of fire.";
        dice.dd = 5 + (plr->lev - 5)/4;
        dice.ds = 8;
        dice.base = plr->to_d_spell;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_bolt_or_beam(GF_FIRE, dice, beam_chance() - 10)) return NULL;
        break;
    case 12:
        if (name) return "Ray of Sunlight";
        if (desc) return "Fires a beam of light which damages to light-sensitive monsters.";
        dice.dd = 6;
        dice.ds = 8;
        dice.base = plr->to_d_spell;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_beam(GF_LIGHT_WEAK, dice)) return NULL;
        break;
    case 13:
        if (name) return "Entangle";
        if (desc) return "Attempts to slow all monsters in sight.";
        dice.base = 2*plr->lev;
        if (info) return dice_info_power(dice);
        if (cast) plr_project_los(GF_SLOW, dice_roll(dice));
        break;
    case 14:
        if (name) return "Nature's Gate";
        if (desc) return "Summons one or more animals. At higher levels, might summon hounds, reptiles or even an Ent!";
        if (cast)
        {
            bool success = FALSE;
            if (plr->lev < 30)
                success = trump_summoning(1, TRUE, plr->pos, 0, SUMMON_ANIMAL_RANGER, PM_ALLOW_GROUP);
            else if (plr->lev < 47)
            {
                switch (randint1(3))
                {
                case 1:
                    success = trump_summoning(1, TRUE, plr->pos, 0, SUMMON_HOUND, PM_ALLOW_GROUP);
                    break;
                case 2:
                    success = trump_summoning(1, TRUE, plr->pos, 0, SUMMON_HYDRA, PM_ALLOW_GROUP);
                    break;
                case 3:
                    success = trump_summoning((1 + (plr->lev - 15)/ 10), TRUE, plr->pos, 0, SUMMON_ANIMAL_RANGER, PM_ALLOW_GROUP);
                    break;
                }
            }
            else
            {
                if (one_in_(5))
                    success = trump_summoning(1, TRUE, plr->pos, 0, SUMMON_ENT, PM_ALLOW_GROUP);
            }
            if (!success)
                msg_print("No help arrives.");
        }
        break;
    case 15:
        if (name) return "Herbal Healing";
        if (desc) return "Heals HP greatly. And heals cut, stun and perhaps poison.";
        dice.base = 500;
        if (info) return dice_info_heal(dice);
        if (cast) {
            hp_player(dice_roll(dice));
            plr_tim_remove(T_STUN);
            plr_tim_remove(T_CUT);
            plr_tim_recover(T_POISON, 50, 300);
        }
        break;
    case 16:
        if (name) return "Stair Building";
        if (desc) return "Creates a stair which goes down or up.";
        if (cast) dun_create_stairs(cave, FALSE);
        break;
    case 17:
        if (name) return "Stone Skin";
        if (desc) return "Gives bonus to AC for a while.";
        dice.dd = 1;
        dice.ds = 30;
        dice.base = 20;
        if (info) return dice_info_dur(dice);
        if (cast) plr_tim_add(T_STONE_SKIN, dice_roll(dice));
        break;
    case 18:
        if (name) return "Resistance True";
        if (desc) return "Gives resistance to fire, cold, electricity, acid and poison for a while.";
        dice.dd = 1;
        dice.ds = 20;
        dice.base = 20;
        if (info) return dice_info_dur(dice);
        if (cast) {
            plr_tim_add(T_RES_ACID, dice_roll(dice));
            plr_tim_add(T_RES_ELEC, dice_roll(dice));
            plr_tim_add(T_RES_FIRE, dice_roll(dice));
            plr_tim_add(T_RES_COLD, dice_roll(dice));
            plr_tim_add(T_RES_POIS, dice_roll(dice));
        }
        break;
    case 19:
        if (name) return "Forest Creation";
        if (desc) return "Creates trees in all adjacent squares.";
        if (cast) plr_burst(1, GF_MAKE_TREE, 0);
        break;
    case 20:
        if (name) return "Stone Tell";
        if (desc) return "*Identifies* an item.";
        if (cast && !identify_fully(NULL)) return NULL;
        break;
    case 21:
        if (name) return "Wall of Stone";
        if (desc) return "Creates granite walls in all adjacent squares.";
        if (cast) plr_burst(1, GF_MAKE_WALL, 0);
        break;
    case 22:
        if (name) return "Protect from Corrosion";
        if (desc) return "Makes an equipment acid-proof.";
        if (cast && !rustproof()) return NULL;
        break;
    case 23:
        if (name) return "Call Sunlight";
        if (desc) return "Generates ball of light centered on you. Maps and lights whole dungeon level. Knows all objects location.";
        dice.base = 75 + plr->to_d_spell;
        rad = 8;
        if (info) return dice_info_dam(dice);
        if (cast) {
            plr_burst(rad, GF_LIGHT, dice_roll(dice));
            virtue_add(VIRTUE_KNOWLEDGE, 1);
            virtue_add(VIRTUE_ENLIGHTENMENT, 1);
            wiz_lite();

            if ( (prace_is_(RACE_VAMPIRE) || prace_is_(RACE_MON_VAMPIRE) || plr->mimic_form == MIMIC_VAMPIRE)
              && !res_save_default(GF_LIGHT) )
            {
                msg_print("The sunlight scorches your flesh!");
                take_hit(DAMAGE_NOESCAPE, 50, "sunlight");
            }
        }
        break;
    case 24:
        if (name) return "Earthquake";
        if (desc) return "Shakes dungeon structure, and results in random swapping of floors and walls.";
        rad = 10;
        if (info) return info_radius(rad);
        if (cast) earthquake(plr->pos, rad);
        break;
    case 25:
        if (name) return "Fire Storm";
        if (desc) return "Fires a huge ball of fire.";
        dice.base = 60 + 2*plr->lev + plr->to_d_spell;
        rad = 1 + plr->lev/12;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_ball(rad, GF_FIRE, dice)) return NULL;
        break;
    case 26:
        if (name) return "Blizzard";
        if (desc) return "Fires a huge ball of cold.";
        dice.base = 70 + 2*plr->lev + plr->to_d_spell;
        rad = 1 + plr->lev/12;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_ball(rad, GF_COLD, dice)) return NULL;
        break;
    case 27:
        if (name) return "Lightning Storm";
        if (desc) return "Fires a huge electric ball.";
        dice.base = 90 + 2*plr->lev + plr->to_d_spell;
        rad = 1 + plr->lev/12;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_ball(rad, GF_ELEC, dice)) return NULL;
        break;
    case 28:
        if (name) return "Whirlpool";
        if (desc) return "Fires a huge ball of water.";
        dice.base = 100 + 2*plr->lev + plr->to_d_spell;
        rad = 1 + plr->lev/12;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_ball(rad, GF_WATER, dice)) return NULL;
        break;
    case 29:
        if (name) return "Meteor";
        if (desc) return "Fires a meteor.";
        dice.base = 150 + 2*plr->lev + plr->to_d_spell;
        rad = 2;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_ball(rad, GF_METEOR, dice)) return NULL;
        break;

    case 30:
        if (name) return "Hurricane";
        if (desc) return "Summons a hurricane at chosen location with gale force winds.";
        dice.base = 125 + 2*plr->lev + plr->to_d_spell;
        rad = 1 + plr->lev/12;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_ball(rad, GF_STORM, dice)) return NULL;
        break;

    case 31:
        if (name) return "Nature's Wrath";
        if (desc) return "You unleash Nature's full fury, the exact consequences of which can't be predicted.";
        if (cast)
        {
            rad = 1 + plr->lev/12;
            switch (randint1(6))
            {
            case 1: /* The original effect: Line of Sight damage, earthquake, disintegration ball */
                msg_print("Nature's Fury is unleashed!");
                dice.base = 4*plr->lev + plr->to_d_spell;
                plr_project_los(GF_DISP_ALL, dice_roll(dice));
                earthquake(plr->pos, spell_power(20 + plr->lev / 2));
                dice.base = 100 + plr->lev + plr->to_d_spell;
                plr_burst(rad, GF_DISINTEGRATE, dice_roll(dice));
                break;

            case 2: /* Deadly bolt of lightning */
                msg_print("Your hands crackle with electricity!");
                dice.base = 8*plr->lev + plr->to_d_spell;
                plr_cast_bolt(GF_ELEC, dice); /* this spell cannot be cancelled */
                break;

            case 3: /* Immense thunderclap */
                msg_print("There is a large thunderclap!");
                dice.base = 5*plr->lev + plr->to_d_spell;
                plr_project_los(GF_SOUND, dice_roll(dice));
                break;

            case 4: /* Gravitational Wave */
                msg_print("Space warps around you!");
                dice.base = 4*plr->lev + plr->to_d_spell;
                plr_project_los(GF_GRAVITY, dice_roll(dice));
                break;

            case 5: /* Elemental Storm */
                msg_print("You unleash the elements!");
                dice.base = 120 + plr->lev + plr->to_d_spell;
                plr_burst(rad, GF_FIRE, dice_roll(dice));
                plr_burst(rad, GF_COLD, dice_roll(dice));
                plr_burst(rad, GF_ELEC, dice_roll(dice));
                break;

            case 6: {/* Rock Storm */
                point_t pos;
                msg_print("You fire a storm of boulders!");
                pos = get_fire_pos();
                if (dun_pos_interior(cave, pos)) /* this spell cannot be cancelled */
                {
                    int i;
                    dice.base = 70 + plr->lev + plr->to_d_spell;
                    for (i = 0; i < 3; i++)
                        plr_ball(1, pos, GF_SHARDS, dice_roll(dice));
                }
                break; }
            }
        }
        break;
    }
    return "";
}

static cptr do_chaos_spell(int spell, int mode)
{
    bool name = (mode == SPELL_NAME) ? TRUE : FALSE;
    bool desc = (mode == SPELL_DESC) ? TRUE : FALSE;
    bool info = (mode == SPELL_INFO) ? TRUE : FALSE;
    bool cast = (mode == SPELL_CAST) ? TRUE : FALSE;

    int rad = 0;
    int rng = DUN_PATH_MAX;
    dice_t dice = {0};

    dice.scale = spell_power(1000);
    dice.scale += virtue_current(VIRTUE_CHANCE) / 2; /* roughly +/- 6% */

    switch (spell)
    {
    case 0:
        if (name) return "Magic Missile";
        if (desc) return "Fires a weak bolt of magic.";
        dice.dd = 3 + (plr->lev - 1)/5;
        dice.ds = 4;
        dice.base = plr->to_d_spell;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_bolt_or_beam(GF_MISSILE, dice, beam_chance() - 10)) return NULL;
        break;
    case 1:
        if (name) return "Trap / Door Destruction";
        if (desc) return "Destroys all traps in adjacent squares.";
        rad = 1;
        if (info) return info_radius(rad);
        if (cast) plr_burst(1, GF_KILL_DOOR, 0);
        break;
    case 2:
        if (name) return "Flash of Light";
        if (desc) return "Lights up nearby area and the inside of a room permanently.";
        dice.dd = 2;
        dice.ds = MAX(1, plr->lev / 2);
        rad = 1 + plr->lev/10;
        if (info) return dice_info_dam(dice);
        if (cast) lite_area(dice_roll(dice), rad);
        break;
    case 3:
        if (name) return "Touch of Confusion";
        if (desc) return "Attempts to confuse the next monster that you hit.";
        if (cast)
        {
            if (!(plr->special_attack & ATTACK_CONFUSE))
            {
                msg_print("Your hands start glowing.");

                plr->special_attack |= ATTACK_CONFUSE;
                plr->redraw |= (PR_STATUS);
            }
        }
        break;
    case 4:
        if (name) return "Mana Burst";
        if (desc) return "Fires a ball of magic.";

        rad = 2 + plr->lev/30;
        dice.dd = 3;
        dice.ds = 5;
        dice.base = plr->to_d_spell;
        if (plr->pclass == CLASS_MAGE ||
            plr->pclass == CLASS_HIGH_MAGE ||
            plr->pclass == CLASS_SORCERER ||
            plr->pclass == CLASS_YELLOW_MAGE ||
            plr->pclass == CLASS_GRAY_MAGE)
            dice.base += plr->lev + plr->lev / 2;
        else
            dice.base += plr->lev + plr->lev / 4;

        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_ball(rad, GF_MISSILE, dice)) return NULL;
        break;
    case 5:
        if (name) return "Fire Bolt";
        if (desc) return "Fires a bolt or beam of fire.";

        dice.dd = 8 + (plr->lev - 5)/4;
        dice.ds = 8;
        dice.base = plr->to_d_spell;

        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_bolt_or_beam(GF_FIRE, dice, beam_chance())) return NULL;
        break;
    case 6:
        if (name) return "Fist of Force";
        if (desc) return "Fires a tiny ball of disintegration.";

        dice.dd = 8 + (plr->lev - 5)/4;
        dice.ds = 8;
        dice.base = plr->to_d_spell;

        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_ball(0, GF_DISINTEGRATE, dice)) return NULL;
        break;
    case 7:
        if (name) return "Teleport Self";
        if (desc) return "Teleport long distance.";
        rng = 5*plr->lev;
        if (info) return info_range(rng);
        if (cast) {
            if (mut_present(MUT_ASTRAL_GUIDE)) energy_use /= 3;
            teleport_player(rng, 0);
        }
        break;
    case 8:
        if (name) return "Wonder";
        if (desc) return "Fires something with random effects.";
        if (info) return "random";
        if (cast && !cast_wonder()) return NULL;
        break;
    case 9:
        if (name) return "Chaos Bolt";
        if (desc) return "Fires a bolt or ball of chaos.";

        dice.dd = 10 + (plr->lev - 5)/4;
        dice.ds = 8;
        dice.base = plr->to_d_spell;

        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_bolt_or_beam(GF_CHAOS, dice, beam_chance())) return NULL;
        break;
    case 10:
        if (name) return "Sonic Boom";
        if (desc) return "Generates a ball of sound centered on you.";

        dice.base = 50 + plr->lev + plr->to_d_spell;
        rad = 2 + plr->lev/10;

        if (info) return dice_info_dam(dice);
        if (cast) {
            msg_print("BOOM! Shake the room!");
            plr_burst(rad, GF_SOUND, dice_roll(dice));
        }
        break;
    case 11:
        if (name) return "Doom Bolt";
        if (desc) return "Fires a beam of pure mana.";

        dice.dd = 11 + (plr->lev - 5)/4;
        dice.ds = 8;
        dice.base = plr->to_d_spell;

        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_beam(GF_MANA, dice)) return NULL;
        break;
    case 12:
        if (name) return "Fire Ball";
        if (desc) return "Fires a ball of fire.";

        dice.base = 55 + plr->lev + plr->to_d_spell;
        rad = 2;

        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_ball(rad, GF_FIRE, dice)) return NULL;
        break;
    case 13:
        if (name) return "Teleport Other";
        if (desc) return "Teleports all monsters on the line away unless resisted.";

        dice.base = 2*plr->lev;
        if (info) return dice_info_power(dice);
        if (cast && !plr_cast_beam(GF_TELEPORT, dice)) return NULL;
        break;
    case 14:
        if (name) return "Word of Destruction";
        if (desc) return "Destroy everything in nearby area.";

        dice.base = 4*plr->lev;
        if (info) return dice_info_power(dice);
        rad = 12 + _1d(4);
        if (cast) destroy_area(plr->pos, rad, dice_roll(dice));
        break;

    case 15:
        if (name) return "Invoke Logrus";
        if (desc) return "Fires a huge ball of chaos.";

        dice.base = 99 + 2*plr->lev + plr->to_d_spell;
        rad = plr->lev/5;

        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_ball(rad, GF_CHAOS, dice)) return NULL;
        break;
    case 16:
        if (name) return "Polymorph Other";
        if (desc) return "Attempts to polymorph a monster.";
        dice.base = plr->lev;
        if (info) return dice_info_power(dice);
        if (cast && !plr_cast_bolt(GF_OLD_POLY, dice)) return NULL;
        break;
    case 17:
        if (name) return "Chain Lightning";
        if (desc) return "Fires lightning beams in all directions.";

        dice.dd = 5 + plr->lev/10;
        dice.ds = 8;
        dice.base = plr->to_d_spell;

        if (info) return dice_info_dam(dice);
        if (cast) {
            int i;
            for (i = 0; i < 8; i++)
            {
                point_t p = point_step(plr->pos, ddd[i]);
                plr_beam(p, GF_ELEC, dice_roll(dice));
            }
        }
        break;
    case 18:
        if (name) return "Arcane Binding";
        if (desc) return "It attempts to recharge a device using your mana for power.";

        dice.base = 90;
        if (info) return dice_info_power(dice);
        if (cast && !recharge_from_player(dice_roll(dice))) return NULL;
        break;
    case 19:
        if (name) return "Disintegrate";
        if (desc) return "Fires a huge ball of disintegration.";

        dice.base = 70 + plr->lev + plr->to_d_spell;
        rad = 3 + plr->lev/40;

        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_ball(rad, GF_DISINTEGRATE, dice)) return NULL;
        break;
    case 20:
        if (name) return "Alter Reality";
        if (desc) return "Recreates current dungeon level.";
        if (cast) alter_reality();
        break;
    case 21:
        if (name) return "Magic Rocket";
        if (desc) return "Fires a magic rocket.";

        dice.base = 50 + 4*plr->lev + plr->to_d_spell;
        rad = 2;

        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_rocket(rad, dice)) return NULL;
        break;
    case 22:
        if (name) return "Chaos Branding";
        if (desc) return "Makes current weapon a Chaotic weapon.";
        if (cast && !brand_weapon(EGO_WEAPON_CHAOS)) return NULL;
        break;
    case 23:
        if (name) return "Summon Demon";
        if (desc) return "Summons a demon.";
        if (cast)
        {
            u32b mode = 0;
            bool pet = !one_in_(3);
            who_t  who = pet ? who_create_plr() : who_create_null();

            if (pet) mode |= PM_FORCE_PET;
            else mode |= PM_NO_PET;
            if (!(pet && plr->lev < 50)) mode |= PM_ALLOW_GROUP;

            if (summon_specific(who, plr->pos, 3*plr->lev/2, SUMMON_DEMON, mode))
            {
                msg_print("The area fills with a stench of sulphur and brimstone.");
                if (pet)
                    msg_print("'What is thy bidding... Master?'");
                else
                    msg_print("'NON SERVIAM! Wretch! I shall feast on thy mortal soul!'");
            }
        }
        break;

    case 24:
        if (name) return "Beam of Gravity";
        if (desc) return "Fires a beam of gravity.";

        dice.dd = 9 + (plr->lev - 5)/4;
        dice.ds = 8;
        dice.base = plr->to_d_spell;

        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_beam(GF_GRAVITY, dice)) return NULL;
        break;
    case 25:
        if (name) return "Meteor Swarm";
        if (desc) return "Makes meteor balls fall down to nearby random locations.";

        dice.base = 2*plr->lev + plr->to_d_spell;
        rad = 2;

        if (info) return dice_info_dam_each(dice);
        if (cast) plr_meteor_shower(10 + _1d(10), rad, GF_METEOR, dice);
        break;
    case 26:
        if (name) return "Flame Strike";
        if (desc) return "Generate a huge ball of fire centered on you.";

        dice.base = 150 + 3*plr->lev/2 + plr->to_d_spell;
        rad = 8;

        if (info) return dice_info_dam(dice);
        if (cast) plr_burst(rad, GF_FIRE, dice_roll(dice));
        break;
    case 27:
        if (name) return "Call Chaos";
        if (desc) return "Generate random kind of balls or beams.";
        if (cast) call_chaos(100);
        break;
    case 28:
        if (name) return "Polymorph Self";
        if (desc) return "Polymorphs yourself into a new form.";
        if (cast)
        {
            int which;
            switch (randint1(50))
            {
            case 1: which = one_in_(10) ? MIMIC_DEMON_LORD : MIMIC_DEMON; break;
            case 2: which = MIMIC_VAMPIRE; break;
            default: which = plr_race_polymorph();
            }
            set_mimic(50 + randint1(50), which, FALSE);
        }
        break;
    case 29:
        if (name) return "Mana Storm";
        if (desc) return "Fires an extremely powerful huge ball of pure mana.";

        dice.base = 300 + 4*plr->lev + plr->to_d_spell;
        rad = 4;

        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_ball(rad, GF_MANA, dice)) return NULL;
        break;
    case 30:
        if (name) return "Breathe Logrus";
        if (desc) return "Fires an extremely powerful ball of chaos.";

        dice.base = 3*plr->chp/4 + plr->to_d_spell;
        rad = 2;

        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_ball(rad, GF_CHAOS, dice)) return NULL;
        break;
    case 31: /* XXX */
        if (name) return "Call the Void";
        if (desc) return "Fires rockets, mana balls and nuclear waste balls in all directions each unless you are not adjacent to any walls. Otherwise *destroys* huge area.";
        if (info) return "dam 3 * 175";
        if (cast) call_the_();
        break;
    }
    return "";
}

static cptr do_death_spell(int spell, int mode)
{
    bool name = (mode == SPELL_NAME) ? TRUE : FALSE;
    bool desc = (mode == SPELL_DESC) ? TRUE : FALSE;
    bool info = (mode == SPELL_INFO) ? TRUE : FALSE;
    bool cast = (mode == SPELL_CAST) ? TRUE : FALSE;

    int rad = 0;
    dice_t dice = {0};

    dice.scale = spell_power(1000);
    dice.scale += virtue_current(VIRTUE_UNLIFE) / 2; /* roughly +/- 6% */
    dice.scale -= virtue_current(VIRTUE_FAITH) / 2;

    switch (spell)
    {
    case 0:
        if (name) return "Detect Unlife";
        if (desc) return "Detects all nonliving monsters in your vicinity.";
        rad = DETECT_RAD_DEFAULT;
        if (info) return info_radius(rad);
        if (cast) detect_monsters_nonliving(rad);
        break;
    case 1:
        if (name) return "Malediction";
        if (desc) return "Fires a tiny ball of evil power which hurts good monsters greatly.";
        dice.dd = 3 + (plr->lev - 1)/5;
        dice.ds = 4;
        dice.base = plr->to_d_spell;
        if (info) return dice_info_dam(dice);
        if (cast) {
            point_t p = get_fire_pos_aux(TARGET_KILL | TARGET_BALL);
            if (!dun_pos_interior(plr_dun(), p)) return NULL;
            plr_ball(0, p, GF_HELL_FIRE, dice_roll(dice));
            if (one_in_(5))
            {
                int effect = randint1(1000);
                if (effect == 666)
                    plr_ball_hide(0, p, GF_DEATH_RAY, 200*plr->lev);
                else if (effect < 500)
                    plr_ball_hide(0, p, GF_FEAR, plr->lev);
                else if (effect < 800)
                    plr_ball_hide(0, p, GF_OLD_CONF, dice_roll(dice));
                else
                    plr_ball_hide(0, p, GF_STUN, dice_roll(dice));
            }
        }
        break;
    case 2:
        if (name) return "Detect Evil";
        if (desc) return "Detects all evil monsters in your vicinity.";
        rad = DETECT_RAD_DEFAULT;
        if (info) return info_radius(rad);
        if (cast) detect_monsters_evil(rad);
        break;
    case 3:
        if (name) return "Stinking Cloud";
        if (desc) return "Fires a ball of poison.";
        dice.base = 10 + plr->lev/2 + plr->to_d_spell;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_ball(2, GF_POIS, dice)) return NULL;
        break;
    case 4:
        if (name) return "Black Sleep";
        if (desc) return "Attempts to sleep a monster.";
        dice.base = 2*plr->lev;
        if (info) return dice_info_power(dice);
        if (cast && !plr_cast_bolt(GF_SLEEP, dice)) return NULL;
        break;
    case 5:
        if (name) return "Undead Resistance";
        if (desc) return "Gives resistance to poison and cold.";
        dice.dd = 1;
        dice.ds = 20;
        dice.base = 20;
        if (info) return dice_info_dur(dice);
        if (cast) {
            plr_tim_add(T_RES_COLD, dice_roll(dice));
            plr_tim_add(T_RES_POIS, dice_roll(dice));
        }
        break;
    case 6:
        if (name) return "Horrify";
        if (desc) return "Attempts to scare and stun a monster.";
        dice.base = 2*plr->lev;
        if (info) return dice_info_power(dice);
        if (cast) {
            point_t p = get_fire_pos_aux(TARGET_KILL | TARGET_BALL);
            if (!dun_pos_interior(plr_dun(), p)) return NULL;
            plr_bolt(p, GF_FEAR, dice_roll(dice));
            dice.base = 5 + plr->lev/5;
            plr_bolt(p, GF_STUN, dice_roll(dice));
        }
        break;
    case 7:
        if (name) return "Enslave Undead";
        if (desc) return "Attempts to charm an undead monster.";
        dice.base = 2*plr->lev;
        if (info) return dice_info_power(dice);
        if (cast && !plr_cast_bolt(GF_CONTROL_UNDEAD, dice)) return NULL;
        break;
    case 8:
        if (name) return "Orb of Entropy";
        if (desc) return "Fires a ball which damages living monsters.";
        dice.dd = 3;
        dice.ds = 6;
        dice.base = plr->to_d_spell;
        if (plr->pclass == CLASS_MAGE ||
            plr->pclass == CLASS_HIGH_MAGE ||
            plr->pclass == CLASS_SORCERER ||
            plr->pclass == CLASS_YELLOW_MAGE ||
            plr->pclass == CLASS_GRAY_MAGE)
            dice.base += plr->lev + plr->lev/2;
        else
            dice.base += plr->lev + plr->lev/4;
        rad = 2 + plr->lev/30;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_ball(rad, GF_OLD_DRAIN, dice)) return NULL;
        break;
    case 9:
        if (name) return "Nether Bolt";
        if (desc) return "Fires a bolt or beam of nether.";
        dice.dd = 5 + (plr->lev - 5)/4;
        dice.ds = 8;
        dice.base = plr->to_d_spell;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_bolt_or_beam(GF_NETHER, dice, beam_chance())) return NULL;
        break;
    case 10:
        if (name) return "Cloud kill";
        if (desc) return "Generate a ball of poison centered on you.";
        dice.base = 30 + plr->lev + plr->to_d_spell;
        rad = 2 + plr->lev/10;
        if (info) return dice_info_dam(dice);
        if (cast) plr_burst(rad, GF_POIS, dice_roll(dice));
        break;
    case 11:
        if (name) return "Genocide One";
        if (desc) return "Attempts to vanish a monster.";
        dice.base = 3*plr->lev;
        if (info) return dice_info_power(dice);
        if (cast && !plr_cast_ball(0, GF_GENOCIDE, dice)) return NULL;
        break;
    case 12:
        if (name) return "Poison Branding";
        if (desc) return "Makes current weapon poison branded.";
        if (cast) brand_weapon_slaying(OF_BRAND_POIS, OF_RES_(GF_POIS));
        break;
    case 13:
        if (name) return "Vampiric Drain";
        if (desc) return "Absorbs some HP from a monster and gives them to you. You will also gain nutritional sustenance from this.";
        dice.dd = 1;
        dice.ds = 2*plr->lev;
        dice.base = 2*plr->lev + plr->to_d_spell;
        if (info) return dice_info_dam(dice);
        if (cast) {
            point_t p = plr_get_target(GF_OLD_DRAIN);
            int d = dice_roll(dice);
            if (!dun_pos_interior(plr_dun(), p)) return NULL;
            if (plr_bolt(p, GF_OLD_DRAIN, d))
            {
                int food;

                hp_player(d);

                food = plr->food + MIN(5000, 100*d); /* Ration of Food = 5000 */
                food = MIN(PY_FOOD_MAX - 1, food);     /* don't gorge plr */
                set_food(food);

                virtue_add(VIRTUE_SACRIFICE, -1);
                virtue_add(VIRTUE_VITALITY, -1);
            }
        }
        break;
    case 14:
        if (name) return "Animate dead";
        if (desc) return "Resurrects nearby corpse and skeletons. And makes these your pets.";
        if (cast) plr_burst(5, GF_ANIM_DEAD, 0);
        break;
    case 15:
        if (name) return "Genocide";
        if (desc) return "Eliminates an entire class of monster, exhausting you. Powerful or unique monsters may resist.";
        dice.base = 3*plr->lev;
        if (info) return dice_info_power(dice);
        if (cast && !symbol_genocide(dice_roll(dice), TRUE)) return NULL;
        break;
    case 16:
        if (name) return "Berserk";
        if (desc) return "Gives bonus to hit and HP, immunity to fear for a while. But decreases AC.";
        dice.dd = 1;
        dice.ds = 25;
        dice.base = 25;
        if (info) return dice_info_dur(dice);
        if (cast) {
            bool heal = !plr_tim_find(T_BERSERK);
            plr_tim_add(T_BERSERK, dice_roll(dice));
            if (heal) hp_player(30);
        }
        break;
    case 17:
        if (name) return "Invoke Spirits";
        if (desc) return "Causes random effects.";
        if (info) return "random";
        if (cast && !cast_invoke_spirits()) return NULL;
        break;
    case 18:
        if (name) return "Dark Bolt";
        if (desc) return "Fires a bolt or beam of darkness.";
        dice.dd = 4 + (plr->lev - 5)/4;
        dice.ds = 8;
        dice.base = plr->to_d_spell;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_bolt_or_beam(GF_DARK, dice, beam_chance())) return NULL;
        break;
    case 19:
        if (name) return "Battle Frenzy";
        if (desc) return "Hastes you with greatly enhanced combat prowess.";
        dice.dd = 1;
        dice.ds = 25;
        dice.base = 25;
        if (info) return dice_info_dur(dice);
        if (cast) {
            plr_tim_add(T_HERO, dice_roll(dice));
            plr_tim_add(T_BLESSED, dice_roll(dice));
            plr_tim_add(T_FAST, dice_roll(dice));
        }
        break;
    case 20:
        if (name) return "Vampiric Branding";
        if (desc) return "Makes current weapon Vampiric.";
        if (cast) brand_weapon(EGO_WEAPON_DEATH);
        break;
    case 21:
        if (name) return "Vampirism True";
        if (desc) return "Fires 3 bolts. Each of the bolts absorbs some HP from a monster and gives them to you.";
        dice.base = 100 + plr->to_d_spell/3;
        if (info) return dice_info_dam_each(dice);
        if (cast) {
            point_t p = plr_get_target(GF_OLD_DRAIN);
            int i, d = dice_roll(dice);
            if (!dun_pos_interior(plr_dun(), p)) return NULL;
            for (i = 0; i < 3; i++)
            {
                if (plr_bolt(p, GF_OLD_DRAIN, d))
                {
                    vamp_player(d);
                    virtue_add(VIRTUE_SACRIFICE, -1);
                    virtue_add(VIRTUE_VITALITY, -1);
                }
            }
        }
        break;
    case 22:
        if (name) return "Nether Wave";
        if (desc) return "Damages all living monsters in sight.";
        dice.dd = 1;
        dice.ds = 3*plr->lev;
        dice.base = plr->to_d_spell;
        if (info) return dice_info_dam(dice);
        if (cast) plr_project_los(GF_DISP_LIVING, dice_roll(dice));
        break;
    case 23:
        if (name) return "Darkness Storm";
        if (desc) return "Fires a huge ball of darkness.";
        dice.base = 100 + plr_prorata_level_aux(200, 1, 1, 2) + plr->to_d_spell;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_ball(4, GF_DARK, dice)) return NULL;
        break;
    case 24:
        if (name) return "Death Ray";
        if (desc) return "Fires a beam of death.";
        dice.base = 200*plr->lev;
        if (info) return dice_info_power(dice);
        if (cast && !plr_cast_ball(0, GF_DEATH_RAY, dice)) return NULL;
        break;
    case 25:
        if (name) return "Raise the Dead";
        if (desc) return "Summons an undead monster.";
        if (cast) {
            int type = plr->lev > 47 ? SUMMON_HI_UNDEAD : SUMMON_UNDEAD;
            bool pet = !one_in_(3);
            who_t  who = pet ? who_create_plr() : who_create_null();
            u32b mode = 0;

            if (!pet || (pet && plr->lev > 24 && one_in_(3)))
                mode |= PM_ALLOW_GROUP;

            if (pet) mode |= PM_FORCE_PET;
            else mode |= (PM_ALLOW_UNIQUE | PM_NO_PET);

            if (summon_specific(who, plr->pos, plr->lev*3/2, type, mode))
            {
                msg_print("Cold winds begin to blow around you, carrying with them the stench of decay...");
                if (pet)
                    msg_print("Ancient, long-dead forms arise from the ground to serve you!");
                else
                    msg_print("'The dead arise... to punish you for disturbing them!'");
                virtue_add(VIRTUE_UNLIFE, 1);
            }
        }
        break;
    case 26:
        if (name) return "Esoteria";
        if (desc) return "Identifies an item. Or *identifies* an item at higher level.";
        if (cast) {
            if (randint1(50) > spell_power(plr->lev))
            {
                if (!ident_spell(NULL)) return NULL;
            }
            else
            {
                if (!identify_fully(NULL)) return NULL;
            }
        }
        break;
    case 27:
        if (name) return "Polymorph Vampire";
        if (desc) return "Mimic a vampire for a while. Loses abilities of original race and gets abilities as a vampire.";
        dice.dd = 1;
        dice.ds = 10 + plr->lev/2;
        dice.base = 10 + plr->lev/2;
        if (info) return dice_info_dur(dice);
        if (cast) set_mimic(dice_roll(dice), MIMIC_VAMPIRE, FALSE);
        break;
    case 28:
        if (name) return "Restore Life";
        if (desc) return "Restore lost life force and experience.";
        if (cast) {
            restore_level();
            plr_restore_life(1000);
        }
        break;
    case 29:
        if (name) return "Mass Genocide";
        if (desc) return "Eliminates all nearby monsters, exhausting you. Powerful or unique monsters may be able to resist.";
        dice.base = 3*plr->lev;
        if (info) return dice_info_power(dice);
        if (cast) mass_genocide(dice_roll(dice), TRUE);
        break;
    case 30:
        if (name) return "Nether Storm";
        if (desc) return "Generate a huge ball of nether.";
        dice.base = 10*plr->lev + plr->to_d_spell;
        rad = plr->lev/5;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_ball(rad, GF_NETHER, dice)) return NULL;
        break;
    case 31:
        if (name) return "Wraithform";
        if (desc) return "Becomes wraith form which gives ability to pass walls and makes all damages half.";
        dice.dd = 1;
        dice.ds = plr->lev/2;
        dice.base = plr->lev/2;
        if (info) return dice_info_dur(dice);
        if (cast) plr_tim_add(T_WRAITH, dice_roll(dice));
        break;
    }
    return "";
}

static cptr do_trump_spell(int spell, int mode)
{
    bool name = (mode == SPELL_NAME) ? TRUE : FALSE;
    bool desc = (mode == SPELL_DESC) ? TRUE : FALSE;
    bool info = (mode == SPELL_INFO) ? TRUE : FALSE;
    bool cast = (mode == SPELL_CAST) ? TRUE : FALSE;
    bool fail = (mode == SPELL_FAIL) ? TRUE : FALSE;

    int dir;
    int plev = plr->lev;
    point_t summon_pos = plr->pos;

    int rng = DUN_PATH_MAX;
    dice_t dice = {0};

    dice.scale = spell_power(1000);

    if (cast && use_old_target && target_okay() && plr_view(who_pos(plr->target)) && !one_in_(3))
        summon_pos = who_pos(plr->target);

    switch (spell)
    {
    case 0:
        if (name) return "Phase Door";
        if (desc) return "Teleport short distance.";
        rng = 10;
        if (info) return info_range(rng);
        if (cast) {
            if (mut_present(MUT_ASTRAL_GUIDE)) energy_use /= 3;
            teleport_player(rng, 0);
        }
        break;
    case 1:
        if (name) return "Trump Spiders";
        if (desc) return "Summons spiders.";
        if (cast || fail) {
            msg_print("You concentrate on the trump of an spider...");
            if (trump_summoning(1, !fail, summon_pos, 0, SUMMON_SPIDER, PM_ALLOW_GROUP))
            {
                if (fail) msg_print("The summoned spiders get angry!");
            }
        }
        break;
    case 2:
        if (name) return "Shuffle";
        if (desc) return "Causes random effects.";
        if (info) return "random";
        if (cast) {
            if (0 || get_check("Are you sure you wish to shuffle? "))
                cast_shuffle();
            else
                return NULL;
        }
        break;
    case 3:
        if (name) return "Reset Recall";
        if (desc) return "Resets the 'deepest' level for recall spell.";
        if (cast && !reset_recall()) return NULL;
        break;
    case 4:
        if (name) return "Teleport";
        if (desc) return "Teleport long distance.";
        rng = 4 * plr->lev;
        if (info) return info_range(rng);
        if (cast) {
            if (mut_present(MUT_ASTRAL_GUIDE)) energy_use /= 3;
            teleport_player(rng, 0);
        }
        break;
    case 5:
        if (name) return "Trump Spying";
        if (desc) return "Gives telepathy for a while.";
        dice.dd = 1;
        dice.ds = 30;
        dice.base = 25;
        if (info) return dice_info_dur(dice);
        if (cast) plr_tim_add(T_TELEPATHY, dice_roll(dice));
        break;
    case 6:
        if (name) return "Teleport Away";
        if (desc) return "Teleports all monsters on the line away unless resisted.";
        dice.base = 2*plr->lev;
        if (info) return dice_info_power(dice);
        if (cast && !plr_cast_beam(GF_TELEPORT, dice)) return NULL;
        break;
    case 7:
        if (name) return "Trump Animals";
        if (desc) return "Summons an animal.";
        if (cast || fail) {
            int type = (!fail ? SUMMON_ANIMAL_RANGER : SUMMON_ANIMAL);
            msg_print("You concentrate on the trump of an animal...");
            if (trump_summoning(1, !fail, summon_pos, 0, type, 0L))
            {
                if (fail) msg_print("The summoned animal gets angry!");
            }
        }
        break;
    case 8:
        if (name) return "Trump Reach";
        if (desc) return "Pulls a distant item close to you.";
        dice.base = 15*plr->lev;
        if (info) return info_weight(dice_roll(dice));
        if (cast) {
            int dir;
            if (!get_aim_dir(&dir)) return NULL;
            fetch(dir, dice_roll(dice), FALSE);
        }
        break;
    case 9:
        if (name) return "Trump Kamikaze";
        if (desc) return "Summons monsters which explode by itself.";
        if (cast || fail) {
            point_t where = plr->pos; /* at plr on failure */
            int type;

            if (cast) /* at chosen target (always!) on success */
            {
                if (!target_set(TARGET_KILL)) return NULL;
                where = who_pos(plr->target);
            }

            if (plr->pclass == CLASS_BEASTMASTER)
                type = SUMMON_KAMIKAZE_LIVING;
            else
                type = SUMMON_KAMIKAZE;

            msg_print("You concentrate on several trumps at once...");
            if (trump_summoning(2 + randint0(plr->lev / 7), !fail, where, 0, type, 0))
            {
                if (fail)
                {
                    msg_print("The summoned creatures get angry!");
                }
            }
        }
        break;

    case 10:
        if (name) return "Phantasmal Servant";
        if (desc) return "Summons a ghost.";
        /* Phantasmal Servant is not summoned as enemy when failed */
        if (cast) {
            int summon_lev = plr->lev * 2 / 3 + randint1(plr->lev / 2);
            if (trump_summoning(1, !fail, summon_pos, (summon_lev * 3 / 2), SUMMON_PHANTOM, 0L))
            {
                msg_print("'Your wish, master?'");
            }
        }
        break;

    case 11:
        if (name) return "Haste Monster";
        if (desc) return "Hastes a monster.";
        dice.base = plr->lev; /* currently unused */
        if (cast && !plr_cast_bolt(GF_OLD_SPEED, dice)) return NULL;
    case 12:
        if (name) return "Teleport Level";
        if (desc) return "Teleport to up or down stairs in a moment.";
        if (cast) {
            if (!get_check("Are you sure? (Teleport Level) ")) return NULL;
            dun_teleport_level_plr(cave);
        }
        break;
    case 13:
        if (name) return "Dimension Door";
        if (desc) return "Teleport to given location.";
        rng = 10 + plr->lev/2;
        if (info) return info_range(rng);
        if (cast) {
            msg_print("You open a dimensional gate. Choose a destination.");
            if (!dimension_door(rng)) return NULL;
        }
        break;
    case 14:
        if (name) return "Word of Recall";
        if (desc) return "Recalls player from dungeon to town, or from town to the deepest level of dungeon.";
        if (cast) {
            if (!get_check("Are you sure? (Recall) ")) return NULL;
            if (!dun_mgr_recall_plr()) return NULL;
        }
        break;
    case 15:
        if (name) return "Banish";
        if (desc) return "Teleports all monsters in sight away unless resisted.";
        dice.base = 4*plr->lev;
        if (info) return dice_info_power(dice);
        if (cast) plr_project_los(GF_TELEPORT, dice_roll(dice));
        break;
    case 16: /* XXX */
        if (name) return "Swap Position";
        if (desc) return "Swap positions of you and a monster.";

            if (cast)
            {
                bool result;

                /* HACK -- No range limit */
                project_length = -1;

                result = get_fire_dir(&dir);

                /* Restore range to default */
                project_length = 0;

                if (!result) return NULL;

                teleport_swap(dir);
            }
        break;
    case 17:
        if (name) return "Trump Undead";
        if (desc) return "Summons an undead monster.";
        if (cast || fail) {
            msg_print("You concentrate on the trump of an undead creature...");
            if (trump_summoning(1, !fail, summon_pos, 0, SUMMON_UNDEAD, 0L))
            {
                if (fail) msg_print("The summoned undead creature gets angry!");
            }
        }
        break;
    case 18:
        if (name) return "Trump Reptiles";
        if (desc) return "Summons a hydra.";
        if (cast || fail) {
            msg_print("You concentrate on the trump of a reptile...");
            if (trump_summoning(1, !fail, summon_pos, 0, SUMMON_HYDRA, 0L))
            {
                if (fail) msg_print("The summoned reptile gets angry!");
            }
        }
        break;
    case 19:
        if (name) return "Trump Monsters";
        if (desc) return "Summons some monsters.";
        if (cast || fail) {
            int type = 0;
            msg_print("You concentrate on several trumps at once...");
            if (plr->pclass == CLASS_BEASTMASTER)
                type = SUMMON_LIVING;
            if (trump_summoning((1 + (plev - 15)/ 10), !fail, summon_pos, 0, type, 0L))
            {
                if (fail) msg_print("The summoned creatures get angry!");
            }
        }
        break;
    case 20:
        if (name) return "Trump Hounds";
        if (desc) return "Summons a group of hounds.";
        if (cast || fail) {
            msg_print("You concentrate on the trump of a hound...");
            if (trump_summoning(1, !fail, summon_pos, 0, SUMMON_HOUND, PM_ALLOW_GROUP))
            {
                if (fail) msg_print("The summoned hounds get angry!");
            }
        }
        break;
    case 21:
        if (name) return "Trump Branding";
        if (desc) return "Makes current weapon a Trump weapon.";
        if (cast) brand_weapon(EGO_WEAPON_TRUMP);
        break;
    case 22:
        if (name) return "Living Trump";
        if (desc) return "Gives mutation which makes you teleport randomly or makes you able to teleport at will.";
        if (cast) {
            int which = MUT_TELEPORT_RND;
            if (!get_check("Are you sure? (Living Trump) ")) return NULL;
            if (one_in_(7) || cave->type->id == D_SURFACE)
                which = MUT_TELEPORT;
            if (mut_gain(which))
                msg_print("You have turned into a Living Trump.");
        }
        break;
    case 23:
        if (name) return "Trump Cyberdemon";
        if (desc) return "Summons a cyber demon.";
        if (cast || fail) {
            msg_print("You concentrate on the trump of a Cyberdemon...");
            if (trump_summoning(1, !fail, summon_pos, 0, SUMMON_CYBER, 0L))
            {
                if (fail) msg_print("The summoned Cyberdemon gets angry!");
            }
        }
        break;
    case 24:
        if (name) return "Trump Divination";
        if (desc) return "Detects all monsters, traps, doors, stairs, treasures and items in your vicinity.";
        if (cast) detect_all(DETECT_RAD_DEFAULT);
        break;
    case 25:
        if (name) return "Trump Lore";
        if (desc) return "*Identifies* an item.";
        if (cast && !identify_fully(NULL)) return NULL;
        break;
    case 26:
        if (name) return "Heal Monster";
        if (desc) return "Heal a monster.";
        dice.base = 200 + 10*plr->lev;
        if (info) return dice_info_heal(dice);
        if (cast && !plr_cast_bolt(GF_OLD_HEAL, dice)) return NULL;
        break;
    case 27:
        if (name) return "Trump Dragon";
        if (desc) return "Summons a dragon.";
        if (cast || fail) {
            msg_print("You concentrate on the trump of a dragon...");
            if (trump_summoning(1, !fail, summon_pos, 0, SUMMON_DRAGON, 0))
            {
                if (fail) msg_print("The summoned dragon gets angry!");
            }
        }
        break;
    case 28:
        if (name) return "Trump Meteor";
        if (desc) return "Makes meteor balls fall down to nearby random locations.";
        dice.base = 2*plr->lev + plr->to_d_spell;
        if (info) return dice_info_dam_each(dice);
        if (cast) plr_meteor_shower(10 + _1d(10), 2, GF_METEOR, dice);
        break;
    case 29:
        if (name) return "Trump Demon";
        if (desc) return "Summons a demon.";
        if (cast || fail) {
            msg_print("You concentrate on the trump of a demon...");
            if (trump_summoning(1, !fail, summon_pos, 0, SUMMON_DEMON, 0L))
            {
                if (fail) msg_print("The summoned demon gets angry!");
            }
        }
        break;
    case 30:
        if (name) return "Trump Greater Undead";
        if (desc) return "Summons a greater undead.";
        if (cast || fail) {
            msg_print("You concentrate on the trump of a greater undead being...");
            if (trump_summoning(1, !fail, summon_pos, 0, SUMMON_HI_UNDEAD, PM_ALLOW_UNIQUE))
            {
                if (fail) msg_print("The summoned greater undead creature gets angry!");
            }
        }
        break;
    case 31:
        if (name) return "Trump Ancient Dragon";
        if (desc) return "Summons an ancient dragon.";
        if (cast) {
            int type = SUMMON_HI_DRAGON;
            if (plr->pclass == CLASS_BEASTMASTER)
                type = SUMMON_HI_DRAGON_LIVING;
            msg_print("You concentrate on the trump of an ancient dragon...");
            if (trump_summoning(1, !fail, summon_pos, 0, type, PM_ALLOW_UNIQUE))
            {
                if (fail) msg_print("The summoned ancient dragon gets angry!");
            }
        }
        break;
    }
    return "";
}

static cptr do_arcane_spell(int spell, int mode)
{
    bool name = (mode == SPELL_NAME) ? TRUE : FALSE;
    bool desc = (mode == SPELL_DESC) ? TRUE : FALSE;
    bool info = (mode == SPELL_INFO) ? TRUE : FALSE;
    bool cast = (mode == SPELL_CAST) ? TRUE : FALSE;

    int rng = DUN_PATH_MAX;
    int rad = 0;
    dice_t dice = {0};

    dice.scale = spell_power(1000);

    switch (spell)
    {
    case 0:
        if (name) return "Zap";
        if (desc) return "Fires a bolt or beam of lightning.";
        dice.dd = 3 + (plr->lev - 1)/5;
        dice.ds = 3;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_bolt_or_beam(GF_ELEC, dice, beam_chance() - 10)) return NULL;
        break;
    case 1:
        if (name) return "Wizard Lock";
        if (desc) return "Locks a door.";
        dice.dd = 1;
        dice.ds = 30;
        dice.base = 20;
        if (cast && !plr_cast_beam(GF_JAM_DOOR, dice)) return NULL;
        break;
    case 2:
        if (name) return "Detect Invisibility";
        if (desc) return "Detects all invisible monsters in your vicinity.";
        if (cast) detect_monsters_invis(DETECT_RAD_DEFAULT);
        break;
    case 3:
        if (name) return "Detect Monsters";
        if (desc) return "Detects all monsters in your vicinity unless invisible.";
        if (cast) detect_monsters_normal(DETECT_RAD_DEFAULT);
        break;
    case 4:
        if (name) return "Blink";
        if (desc) return "Teleport short distance.";
        rng = 10;
        if (info) return info_range(rng);
        if (cast) {
            if (mut_present(MUT_ASTRAL_GUIDE))
                energy_use /= 3;
            teleport_player(rng, 0);
        }
        break;
    case 5:
        if (name) return "Light Area";
        if (desc) return "Lights up nearby area and the inside of a room permanently.";
        dice.dd = 2;
        dice.ds = MAX(1, plr->lev / 2);
        rad = 1 + plr->lev/10;
        if (info) return dice_info_dam(dice);
        if (cast) lite_area(dice_roll(dice), rad);
        break;
    case 6:
        if (name) return "Trap & Door Destruction";
        if (desc) return "Fires a beam which destroy traps and doors.";
        if (cast && !plr_cast_beam(GF_KILL_DOOR, dice)) return NULL;
        break;
    case 7:
        if (name) return "Cure Light Wounds";
        if (desc) return "Heals cut and HP a little.";
        dice.dd = 2;
        dice.ds = 8;
        if (info) return dice_info_heal(dice);
        if (cast) {
            hp_player(dice_roll(dice));
            plr_tim_subtract(T_CUT, 10);
        }
        break;
    case 8:
        if (name) return "Detect Doors & Traps";
        if (desc) return "Detects traps, doors, and stairs in your vicinity.";
        if (cast) {
            detect_traps(DETECT_RAD_DEFAULT, TRUE);
            detect_doors(DETECT_RAD_DEFAULT);
            detect_stairs(DETECT_RAD_DEFAULT);
            detect_recall(DETECT_RAD_DEFAULT);
        }
        break;
    case 9:
        if (name) return "Phlogiston";
        if (desc) return "Adds more turns of light to a lantern or torch.";
        if (cast) phlogiston();
        break;
    case 10:
        if (name) return "Detect Treasure";
        if (desc) return "Detects all treasures in your vicinity.";
        if (cast) {
            detect_treasure(DETECT_RAD_DEFAULT);
            detect_objects_gold(DETECT_RAD_DEFAULT);
        }
        break;
    case 11:
        if (name) return "Detect Enchantment";
        if (desc) return "Detects all magical items in your vicinity.";
        if (cast) detect_objects_magic(DETECT_RAD_DEFAULT);
        break;
    case 12:
        if (name) return "Detect Objects";
        if (desc) return "Detects all items in your vicinity.";
        if (cast) detect_objects_normal(DETECT_RAD_DEFAULT);
        break;
    case 13:
        if (name) return "Cure Poison";
        if (desc) return "Cures poison status.";
        if (cast) plr_tim_recover(T_POISON, 80, 100);
        break;
    case 14:
        if (name) return "Resist Cold";
        if (desc) return "Gives resistance to cold.";
        dice.dd = 1;
        dice.ds = 20;
        dice.base = 20;
        if (info) return dice_info_dur(dice);
        if (cast) plr_tim_add(T_RES_COLD, dice_roll(dice));
        break;
    case 15:
        if (name) return "Resist Fire";
        if (desc) return "Gives resistance to fire.";
        dice.dd = 1;
        dice.ds = 20;
        dice.base = 20;
        if (info) return dice_info_dur(dice);
        if (cast) plr_tim_add(T_RES_FIRE, dice_roll(dice));
        break;
    case 16:
        if (name) return "Resist Lightning";
        if (desc) return "Gives resistance to electricity.";
        dice.dd = 1;
        dice.ds = 20;
        dice.base = 20;
        if (info) return dice_info_dur(dice);
        if (cast) plr_tim_add(T_RES_ELEC, dice_roll(dice));
        break;
    case 17:
        if (name) return "Resist Acid";
        if (desc) return "Gives resistance to acid.";
        dice.dd = 1;
        dice.ds = 20;
        dice.base = 20;
        if (info) return dice_info_dur(dice);
        if (cast) plr_tim_add(T_RES_ACID, dice_roll(dice));
        break;
    case 18:
        if (name) return "Cure Medium Wounds";
        if (desc) return "Heals cut and HP more.";
        dice.dd = 4;
        dice.ds = 8;
        if (info) return dice_info_heal(dice);
        if (cast) {
            hp_player(dice_roll(dice));
            plr_tim_recover(T_CUT, 50, 0);
            plr_tim_subtract(T_CUT, 50);
        }
        break;
    case 19:
        if (name) return "Teleport";
        if (desc) return "Teleport long distance.";
        dice.base = 5 * plr->lev;
        if (info) return dice_info_range(dice);
        if (cast) {
            if (mut_present(MUT_ASTRAL_GUIDE))
                energy_use /= 3;
            teleport_player(dice_roll(dice), 0L);
        }
        break;
    case 20:
        if (name) return "Identify";
        if (desc) return "Identifies an item.";
        if (cast && !ident_spell(NULL)) return NULL;
        break;
    case 21:
        if (name) return "Stone to Mud";
        if (desc) return "Turns one rock square to mud.";
        dice.dd = 1;
        dice.ds = 30;
        dice.base = 20 + plr->to_d_spell;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_beam(GF_KILL_WALL, dice)) return NULL;
        break;
    case 22:
        if (name) return "Ray of Light";
        if (desc) return "Fires a beam of light which damages light-sensitive monsters.";
        dice.dd = 6;
        dice.ds = 8;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_beam(GF_LIGHT_WEAK, dice)) return NULL;
        break;
    case 23:
        if (name) return "Satisfy Hunger";
        if (desc) return "Satisfies hunger.";
        if (cast) set_food(PY_FOOD_MAX - 1);
        break;
    case 24:
        if (name) return "See Invisible";
        if (desc) return "Gives see invisible for a while.";
        dice.dd = 1;
        dice.ds = 25;
        dice.base = 25;
        if (info) return dice_info_dur(dice);
        if (cast) plr_tim_add(T_SEE_INVIS, dice_roll(dice));
        break;
    case 25:
        if (name) return "Resist Poison";
        if (desc) return "Gives resistance to poison.";
        dice.dd = 1;
        dice.ds = 20;
        dice.base = 20;
        if (info) return dice_info_dur(dice);
        if (cast) plr_tim_add(T_RES_POIS, dice_roll(dice));
        break;
    case 26:
        if (name) return "Teleport Level";
        if (desc) return "Teleport to up or down stairs in a moment.";
        if (cast) {
            if (!get_check("Are you sure? (Teleport Level) ")) return NULL;
            dun_teleport_level_plr(cave);
        }
        break;
    case 27:
        if (name) return "Teleport Away";
        if (desc) return "Teleports all monsters on the line away unless resisted.";
        dice.base = 2*plr->lev;
        if (info) return dice_info_power(dice);
        if (cast && !plr_cast_beam(GF_TELEPORT, dice)) return NULL;
        break;
    case 28:
        if (name) return "Recharging";
        if (desc) return "It attempts to recharge a device using your mana for power.";
        dice.base = 3*plr->lev/2;
        if (info) return dice_info_power(dice);
        if (cast && !recharge_from_player(dice_roll(dice))) return NULL;
        break;
    case 29:
        if (name) return "Detection";
        if (desc) return "Detects all monsters, traps, doors, stairs, treasures and items in your vicinity.";
        if (cast) detect_all(DETECT_RAD_DEFAULT);
        break;
    case 30:
        if (name) return "Word of Recall";
        if (desc) return "Recalls player from dungeon to town, or from town to the deepest level of dungeon.";
        if (cast) {
            if (!get_check("Are you sure? (Recall) ")) return NULL;
            if (!dun_mgr_recall_plr()) return NULL;
        }
        break;
    case 31:
        if (name) return "Clairvoyance";
        if (desc) return "Maps and lights whole dungeon level. Knows all objects location. And gives telepathy for a while.";
        dice.dd = 1;
        dice.ds = 30;
        dice.base = 25;
        if (info) return dice_info_dur(dice);
        if (cast) {
            virtue_add(VIRTUE_KNOWLEDGE, 1);
            virtue_add(VIRTUE_ENLIGHTENMENT, 1);
            wiz_lite();
            plr_tim_add(T_TELEPATHY, dice_roll(dice));
        }
        break;
    }
    return "";
}

static bool _can_enchant(obj_ptr obj) {
    if (object_is_(obj, TV_SWORD, SV_POISON_NEEDLE)) return FALSE;
    return object_is_weapon_armour_ammo(obj);
}
bool craft_enchant(int max, int inc)
{
    obj_prompt_t prompt = {0};
    char         o_name[MAX_NLEN];
    bool         improved = FALSE;

    prompt.prompt = "Enchant which item?";
    prompt.error = "You have nothing to enchant.";
    prompt.filter = _can_enchant;
    prompt.where[0] = INV_PACK;
    prompt.where[1] = INV_EQUIP;
    prompt.where[2] = INV_QUIVER;
    prompt.where[3] = INV_FLOOR;

    obj_prompt(&prompt);
    if (!prompt.obj) return FALSE;

    object_desc(o_name, prompt.obj, (OD_OMIT_PREFIX | OD_NAME_ONLY));

    /* Some objects cannot be enchanted */
    if (obj_has_flag(prompt.obj, OF_NO_ENCHANT))
        return FALSE;

    /* Enchanting is now automatic ... It was always possible to max
     * out enchanting quickly with skilled macro usage, but other players
     * are inviting carpal tunnel issues to no purpose. */
    if (obj_is_weapon_ammo(prompt.obj) || obj_is_bow(prompt.obj))
    {
        if (prompt.obj->to_h < max)
        {
            prompt.obj->to_h = MIN(max, prompt.obj->to_h + inc);
            if (prompt.obj->to_h >= 0)
                break_curse(prompt.obj);
            improved = TRUE;
        }
        if (prompt.obj->to_d < max)
        {
            prompt.obj->to_d = MIN(max, prompt.obj->to_d + inc);
            if (prompt.obj->to_d >= 0)
                break_curse(prompt.obj);
            improved = TRUE;
        }
    }
    else
    {
        if (prompt.obj->to_a < max)
        {
            prompt.obj->to_a = MIN(max, prompt.obj->to_a + inc);
            if (prompt.obj->to_a >= 0)
                break_curse(prompt.obj);
            improved = TRUE;
        }
    }

    msg_format("%s %s glow%s brightly!",
            (prompt.obj->loc.where != INV_FLOOR) ? "Your" : "The", o_name,
            (prompt.obj->number > 1) ? "" : "s");

    if (!improved)
    {
        msg_print("The enchantment failed.");
        if (one_in_(3) && virtue_current(VIRTUE_ENCHANTMENT) < 100)
            virtue_add(VIRTUE_ENCHANTMENT, -1);
    }
    else
    {
        virtue_add(VIRTUE_ENCHANTMENT, 1);
        /* Minor Enchantment should not allow gold farming ... */
        if (inc == 1 && object_is_nameless(prompt.obj))
            prompt.obj->discount = 99;
        obj_release(prompt.obj, OBJ_RELEASE_ENCHANT);
    }
    return TRUE;
}

static cptr do_craft_spell(int spell, int mode)
{
    bool name = (mode == SPELL_NAME) ? TRUE : FALSE;
    bool desc = (mode == SPELL_DESC) ? TRUE : FALSE;
    bool info = (mode == SPELL_INFO) ? TRUE : FALSE;
    bool cast = (mode == SPELL_CAST) ? TRUE : FALSE;

    int plev = plr->lev;
    dice_t dice = {0};

    dice.scale = spell_power(1000);
    dice.scale += virtue_current(VIRTUE_ENCHANTMENT) / 2; /* roughly +/- 6% */

    switch (spell)
    {
    case 0:
        if (name) return "Minor Enchantment";
        if (desc) return "Attempts to increase +to-hit, +to-dam of a weapon, or to increase +AC of armor.";
        if (cast && !craft_enchant(2 + plev/5, 1)) return NULL;
        break;
    case 1:
        if (name) return "Regeneration";
        if (desc) return "Gives regeneration ability for a while.";
        dice.dd = 1;
        dice.ds = 80;
        dice.base = 80;
        if (info) return dice_info_dur(dice);
        if (cast) plr_tim_add(T_REGEN, dice_roll(dice));
        break;
    case 2:
        if (name) return "Satisfy Hunger";
        if (desc) return "Satisfies hunger.";
        if (cast) set_food(PY_FOOD_MAX - 1);
        break;
    case 3:
        if (name) return "Resist Cold";
        if (desc) return "Gives resistance to cold.";
        dice.dd = 1;
        dice.ds = 20;
        dice.base = 20;
        if (info) return dice_info_dur(dice);
        if (cast) plr_tim_add(T_RES_COLD, dice_roll(dice));
        break;
    case 4:
        if (name) return "Resist Fire";
        if (desc) return "Gives resistance to fire.";
        dice.dd = 1;
        dice.ds = 20;
        dice.base = 20;
        if (info) return dice_info_dur(dice);
        if (cast) plr_tim_add(T_RES_FIRE, dice_roll(dice));
        break;
    case 5:
        if (name) return "Heroism";
        if (desc) return "Removes fear, and gives bonus to hit and 10 more HP for a while.";
        dice.dd = 1;
        dice.ds = 25;
        dice.base = 25;
        if (info) return dice_info_dur(dice);
        if (cast) plr_tim_add(T_HERO, dice_roll(dice));
        break;
    case 6:
        if (name) return "Resist Lightning";
        if (desc) return "Gives resistance to electricity.";
        dice.dd = 1;
        dice.ds = 20;
        dice.base = 20;
        if (info) return dice_info_dur(dice);
        if (cast) plr_tim_add(T_RES_ELEC, dice_roll(dice));
        break;
    case 7:
        if (name) return "Resist Acid";
        if (desc) return "Gives resistance to acid.";
        dice.dd = 1;
        dice.ds = 20;
        dice.base = 20;
        if (info) return dice_info_dur(dice);
        if (cast) plr_tim_add(T_RES_ACID, dice_roll(dice));
        break;
    case 8:
        if (name) return "See Invisibility";
        if (desc) return "Gives see invisible for a while.";
        dice.dd = 1;
        dice.ds = 25;
        dice.base = 25;
        if (info) return dice_info_dur(dice);
        if (cast) plr_tim_add(T_SEE_INVIS, dice_roll(dice));
        break;
    case 9:
        if (name) return "Remove Curse";
        if (desc) return "Removes normal curses from equipped items.";
        if (cast) {
            if (remove_curse())
                msg_print("You feel as if someone is watching over you.");
        }
        break;
    case 10:
        if (name) return "Resist Poison";
        if (desc) return "Gives resistance to poison.";
        dice.dd = 1;
        dice.ds = 20;
        dice.base = 20;
        if (info) return dice_info_dur(dice);
        if (cast) plr_tim_add(T_RES_POIS, dice_roll(dice));
        break;
    case 11:
        if (name) return "Berserk";
        if (desc) return "Gives bonus to hit and HP, immunity to fear for a while. But decreases AC.";
        dice.dd = 1;
        dice.ds = 25;
        dice.base = 25;
        if (info) return dice_info_dur(dice);
        if (cast) {
            bool heal = !plr_tim_find(T_BERSERK);
            plr_tim_add(T_BERSERK, dice_roll(dice));
            if (heal) hp_player(30);
        }
        break;
    case 12:
        if (name) return "Self Knowledge";
        if (desc) return "Gives you useful info regarding your current resistances, the powers of your weapon and maximum limits of your stats.";
        if (cast) self_knowledge();
        break;
    case 13:
        if (name) return "Identify";
        if (desc) return "Identifies an item.";
        if (cast && !ident_spell(NULL)) return NULL;
        break;
    case 14:
        if (name) return "Curing";
        if (desc) return "It cures what ails you including fear, poison, stunning, cuts and hallucination.";
        if (cast) {
            fear_clear_p();
            plr_tim_recover(T_POISON, 65, 150);
            plr_tim_remove(T_STUN);
            plr_tim_remove(T_CUT);
            plr_tim_remove(T_HALLUCINATE);
        }
        break;
    case 15:
        if (name) return "Elemental Branding";
        if (desc) return "Grants your attacks a temporary elemental brand of your choice.";
        dice.dd = 1;
        dice.ds = plr->lev/2;
        dice.base = plr->lev/2;
        if (info) return dice_info_dur(dice);
        if (cast && !choose_ele_attack()) return NULL;
        break;
    case 16:
        if (name) return "Telepathy";
        if (desc) return "Gives telepathy for a while.";
        dice.dd = 1;
        dice.ds = 30;
        dice.base = 25;
        if (info) return dice_info_dur(dice);
        if (cast) plr_tim_add(T_TELEPATHY, dice_roll(dice));
        break;
    case 17:
        if (name) return "Stone Skin";
        if (desc) return "Gives bonus to AC for a while.";
        dice.dd = 1;
        dice.ds = 20;
        dice.base = 30;
        if (info) return dice_info_dur(dice);
        if (cast) plr_tim_add(T_STONE_SKIN, dice_roll(dice));
        break;
    case 18:
        if (name) return "Resistance";
        if (desc) return "Gives resistance to fire, cold, electricity, acid and poison for a while.";
        dice.dd = 1;
        dice.ds = 20;
        dice.base = 20;
        if (info) return dice_info_dur(dice);
        if (cast) {
            plr_tim_add(T_RES_ACID, dice_roll(dice));
            plr_tim_add(T_RES_ELEC, dice_roll(dice));
            plr_tim_add(T_RES_FIRE, dice_roll(dice));
            plr_tim_add(T_RES_COLD, dice_roll(dice));
            plr_tim_add(T_RES_POIS, dice_roll(dice));
        }
        break;
    case 19:
        if (name) return "Haste Self";
        if (desc) return "Hastes you for a while.";
        dice.dd = 1;
        dice.ds = 20 + plr->lev;
        dice.base = plr->lev;
        if (info) return dice_info_dur(dice);
        if (cast) plr_tim_add(T_FAST, dice_roll(dice));
        break;
    case 20:
        if (name) return "Whirlwind Attack";
        if (desc) return "Attacks all adjacent monsters.";
        if (cast) {
            int dir;
            for (dir = 0; dir < 8; dir++)
            {
                point_t pos = point_step(plr->pos, ddd[dir]);
                dun_cell_ptr cell = dun_cell_at(cave, pos);
                mon_ptr mon = dun_mon_at(cave, pos);
                if (mon && (mon->ml || cell_project(cell)))
                    plr_attack_normal(pos);
            }
        }
        break;
    case 21:
        if (name) return "Polish Shield";
        if (desc) return "Makes your shield reflect missiles and bolt spells.";
        if (cast && !polish_shield()) return NULL;
        break;
    case 22:
        if (name) return "Weaponmastery";
        if (desc) return "For a short time, your melee weapon becomes more deadly.";
        dice.dd = 1;
        dice.ds = 3 + plr->lev/10;
        dice.base = 3 + plr->lev/10;
        if (info) return dice_info_dur(dice);
        if (cast) plr_tim_add(T_WEAPONMASTERY, dice_roll(dice));
        break;
    case 23:
        if (name) return "Magical Armor";
        if (desc) return "Gives resistance to magic, bonus to AC, resistance to confusion, blindness, reflection, free action and levitation for a while.";
        dice.dd = 1;
        dice.ds = 20;
        dice.base = 20;
        if (info) return dice_info_dur(dice);
        if (cast) plr_tim_add(T_MAGICAL_ARMOR, dice_roll(dice));
        break;
    case 24:
        if (name) return "Remove All Curse";
        if (desc) return "Removes normal and heavy curse from equipped items.";
        if (cast) {
            if (remove_all_curse())
                msg_print("You feel as if someone is watching over you.");
        }
        break;
    case 25:
        if (name) return "Walk through Wall";
        if (desc) return "Gives ability to pass walls for a while.";
        dice.dd = 1;
        dice.ds = plr->lev/2;
        dice.base = plr->lev/2;
        if (info) return dice_info_dur(dice);
        if (cast) plr_tim_add(T_PASSWALL, dice_roll(dice));
        break;
    case 26:
        if (name) return "Knowledge True";
        if (desc) return "*Identifies* an item.";
        if (cast && !identify_fully(NULL)) return NULL;
        break;
    case 27:
        if (name) return "Enchantment";
        if (desc) return "Attempts to increase +to-hit, +to-dam of a weapon, or to increase +AC of armor.";
        if (cast && !craft_enchant(15, 3)) return NULL;
        break;
    case 28:
        if (name) return "Crafting";
        if (desc) return "Makes chosen weapon, armor or ammo an ego item.";
        if (cast && !cast_crafting()) return NULL;
        break;
    case 29:
        if (name) return "Living Trump";
        if (desc) return "Gives mutation which makes you teleport randomly or makes you able to teleport at will.";
        if (cast) {
            int which = MUT_TELEPORT_RND;
            if (!get_check("Are you sure? (Living Trump) ")) return NULL;
            if (one_in_(7) || cave->type->id == D_SURFACE)
                which = MUT_TELEPORT;
            if (mut_gain(which))
                msg_print("You have turned into a Living Trump.");
        }
        break;
    case 30:
        if (name) return "Immunity";
        if (desc) return "Gives an immunity to fire, cold, electricity or acid for a while.";
        dice.dd = 1;
        dice.ds = 13;
        dice.base = 13;
        if (info) return dice_info_dur(dice);
        if (cast && !choose_ele_immune(dice_roll(dice))) return NULL;
        break;
    case 31:
        if (name) return "Mana Branding";
        if (desc) return "Temporarily brands your weapon with force.";
        dice.dd = 1;
        dice.ds = plr->lev/4;
        dice.base = plr->lev/4;
        if (info) return dice_info_dur(dice);
        if (cast) plr_tim_add(T_BRAND_MANA, dice_roll(dice));
        break;
    }
    return "";
}

static cptr do_daemon_spell(int spell, int mode)
{
    bool name = (mode == SPELL_NAME) ? TRUE : FALSE;
    bool desc = (mode == SPELL_DESC) ? TRUE : FALSE;
    bool info = (mode == SPELL_INFO) ? TRUE : FALSE;
    bool cast = (mode == SPELL_CAST) ? TRUE : FALSE;

    int rad = 0;
    dice_t dice = {0};

    dice.scale = spell_power(1000);
    dice.scale -= virtue_current(VIRTUE_JUSTICE) / 4;
    dice.scale -= virtue_current(VIRTUE_FAITH) / 4;
    dice.scale -= virtue_current(VIRTUE_HONOUR) / 4;

    switch (spell)
    {
    case 0:
        if (name) return "Magic Missile";
        if (desc) return "Fires a weak bolt of magic.";
        dice.dd = 3 + (plr->lev - 1)/5;
        dice.ds = 4;
        dice.base = plr->to_d_spell;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_bolt_or_beam(GF_MISSILE, dice, beam_chance() - 10)) return NULL;
        break;
    case 1:
        if (name) return "Detect Unlife";
        if (desc) return "Detects all nonliving monsters in your vicinity.";
        if (cast) detect_monsters_nonliving(DETECT_RAD_DEFAULT);
        break;
    case 2:
        if (name) return "Evil Bless";
        if (desc) return "Gives bonus to hit and AC for a few turns.";
        dice.dd = 1;
        dice.ds = 12;
        dice.base = 12;
        if (info) return dice_info_dur(dice);
        if (cast) plr_tim_add(T_BLESSED, dice_roll(dice));
        break;
    case 3:
        if (name) return "Resist Fire";
        if (desc) return "Gives resistance to fire for a while.";
        dice.dd = 1;
        dice.ds = 20;
        dice.base = 20;
        if (info) return dice_info_dur(dice);
        if (cast) plr_tim_add(T_RES_FIRE, dice_roll(dice));
        break;
    case 4:
        if (name) return "Horrify";
        if (desc) return "Attempts to scare and stun a monster.";
        dice.base = 2*plr->lev;
        if (info) return dice_info_dur(dice);
        if (cast) {
            point_t p = plr_get_target(GF_FEAR);
            if (!dun_pos_interior(cave, p)) return NULL;
            plr_bolt(p, GF_FEAR, dice_roll(dice));
            plr_bolt(p, GF_STUN, 5 + plr->lev/5);
        }
        break;
    case 5:
        if (name) return "Nether Bolt";
        if (desc) return "Fires a bolt or beam of nether.";
        dice.dd = 5 + (plr->lev - 5)/4;
        dice.ds = 8;
        dice.base = plr->to_d_spell;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_bolt_or_beam(GF_NETHER, dice, beam_chance())) return NULL;
        break;
    case 6:
        if (name) return "Summon Manes";
        if (desc) return "Summons a manes.";
        if (cast) {
            if (!summon_specific(who_create_plr(), plr->pos, spell_power(plr->lev * 3 / 2), SUMMON_MANES, (PM_ALLOW_GROUP | PM_FORCE_PET)))
                msg_print("No Manes arrive.");
        }
        break;
    case 7:
        if (name) return "Hellish Flame";
        if (desc) return "Fires a ball of evil power. Hurts good monsters greatly.";
        dice.dd = 3;
        dice.ds = 6;
        dice.base = plr->to_d_spell;
        if (plr->pclass == CLASS_MAGE ||
            plr->pclass == CLASS_HIGH_MAGE ||
            plr->pclass == CLASS_SORCERER ||
            plr->pclass == CLASS_YELLOW_MAGE ||
            plr->pclass == CLASS_GRAY_MAGE)
            dice.base += plr->lev + plr->lev / 2;
        else
            dice.base += plr->lev + plr->lev / 4;
        rad = 2 + plr->lev/30;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_ball(rad, GF_HELL_FIRE, dice)) return NULL;
        break;
    case 8:
        if (name) return "Dominate Demon";
        if (desc) return "Attempts to charm a demon.";
        dice.base = 2*plr->lev;
        if (info) return dice_info_power(dice);
        if (cast && !plr_cast_bolt(GF_CONTROL_DEMON, dice)) return NULL;
        break;
    case 9:
        if (name) return "Vision";
        if (desc) return "Maps nearby area.";
        if (cast) map_area(DETECT_RAD_MAP);
        break;
    case 10:
        if (name) return "Resist Nether";
        if (desc) return "Gives resistance to nether for a while.";
        dice.dd = 1;
        dice.ds = 20;
        dice.base = 20;
        if (info) return dice_info_dur(dice);
        if (cast) plr_tim_add(T_RES_NETHER, dice_roll(dice));
        break;
    case 11:
        if (name) return "Plasma Bolt";
        if (desc) return "Fires a bolt or beam of plasma.";
        dice.dd = 11 + (plr->lev - 5)/4;
        dice.ds = 8;
        dice.base = plr->to_d_spell;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_bolt_or_beam(GF_PLASMA, dice, beam_chance())) return NULL;
        break;
    case 12:
        if (name) return "Fire Ball";
        if (desc) return "Fires a ball of fire.";
        dice.base = 55 + plr->lev + plr->to_d_spell;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_ball(2, GF_FIRE, dice)) return NULL;
        break;
    case 13:
        if (name) return "Fire Branding";
        if (desc) return "Makes current weapon fire branded.";
        if (cast) brand_weapon_slaying(OF_BRAND_FIRE, OF_RES_(GF_FIRE));
        break;
    case 14:
        if (name) return "Nether Ball";
        if (desc) return "Fires a huge ball of nether.";
        dice.base = 75 + 3*plr->lev/2 + plr->to_d_spell;
        rad = 2 + plr->lev/20;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_ball(rad, GF_NETHER, dice)) return NULL;
        break;
    case 15:
        if (name) return "Summon Demon";
        if (desc) return "Summons a demon.";
        if (cast) {
            bool pet = !one_in_(3);
            u32b mode = 0L;
            who_t  who = pet ? who_create_plr() : who_create_null();

            if (pet) mode |= PM_FORCE_PET;
            else mode |= PM_NO_PET;
            if (!(pet && (plr->lev < 50))) mode |= PM_ALLOW_GROUP;

            if (summon_specific(who, plr->pos, spell_power(plr->lev*2/3+randint1(plr->lev/2)), SUMMON_DEMON, mode))
            {
                msg_print("The area fills with a stench of sulphur and brimstone.");
                if (pet)
                    msg_print("'What is thy bidding... Master?'");
                else
                    msg_print("'NON SERVIAM! Wretch! I shall feast on thy mortal soul!'");
            }
            else
                msg_print("No demons arrive.");
        }
        break;
    case 16:
        if (name) return "Devilish Eye";
        if (desc) return "Gives telepathy for a while.";
        dice.dd = 1;
        dice.ds = 25;
        dice.base = 30;
        if (info) return dice_info_dur(dice);
        if (cast) plr_tim_add(T_TELEPATHY, dice_roll(dice));
        break;
    case 17:
        if (name) return "Devilish Cloak";
        if (desc) return "Gives resistance to fire, acid and poison as well as an aura of fire.";
        dice.dd = 1;
        dice.ds = 20;
        dice.base = 20;
        if (info) return dice_info_dur(dice);
        if (cast) {
            plr_tim_add(T_RES_ACID, dice_roll(dice));
            plr_tim_add(T_RES_FIRE, dice_roll(dice));
            plr_tim_add(T_RES_POIS, dice_roll(dice));
            plr_tim_add(T_AURA_FIRE, dice_roll(dice));
        }
        break;
    case 18:
        if (name) return "The Flow of Lava";
        if (desc) return "Generates a ball of fire centered on you which transforms floors to magma.";
        dice.base = 55 + plr->lev + plr->to_d_spell;
        if (info) return dice_info_dam(dice);
        if (cast) {
            plr_burst(3, GF_FIRE, dice_roll(dice));
            plr_ball(3, plr->pos, GF_LAVA_FLOW, 2 + _1d(2));
        }
        break;
    case 19:
        if (name) return "Plasma Ball";
        if (desc) return "Fires a ball of plasma.";
        dice.base = 80 + 3*plr->lev/2 + plr->to_d_spell;
        rad = 2 + plr->lev/40;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_ball(rad, GF_PLASMA, dice)) return NULL;
        break;
    case 20:
        if (name) return "Polymorph Demon";
        if (desc) return "Mimic a demon for a while. Loses abilities of original race and gets abilities as a demon.";
        dice.dd = 1;
        dice.ds = 10 + plr->lev/2;
        dice.base = 10 + plr->lev/2;
        if (info) return dice_info_dur(dice);
        if (cast) set_mimic(dice_roll(dice), MIMIC_DEMON, FALSE);
        break;
    case 21:
        if (name) return "Nether Wave";
        if (desc) return "Damages all monsters in sight. Hurts good monsters greatly.";
        dice.dd = 1;
        dice.ds = 2*plr->lev;
        dice.base = plr->to_d_spell;
        if (info) return dice_info_dam(dice);
        if (cast) {
            plr_project_los(GF_DISP_ALL, dice_roll(dice));
            plr_project_los(GF_DISP_GOOD, dice_roll(dice));
        }
        break;
    case 22:
        if (name) return "Kiss of Succubus";
        if (desc) return "Fires a ball of nexus.";
        dice.base = 75 + 3*plr->lev + plr->to_d_spell;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_ball(4, GF_NEXUS, dice)) return NULL;
        break;
    case 23:
        if (name) return "Doom Hand";
        if (desc) return "Attempt to mortally wound a target monster, draining a large proportion of their remaining health.";
        dice.base = 3*plr->lev;
        if (cast && !plr_cast_ball(0, GF_HAND_DOOM, dice)) return NULL;
        break;
    case 24:
        if (name) return "Raise the Morale";
        if (desc) return "Gives bonus to hit and 10 more HP for a while.";
        dice.dd = 1;
        dice.ds = 25;
        dice.base = 25;
        if (info) return dice_info_dur(dice);
        if (cast) plr_tim_add(T_HERO, dice_roll(dice));
        break;
    case 25:
        if (name) return "Immortal Body";
        if (desc) return "Gives resistance to time for a while.";
        dice.dd = 1;
        dice.ds = 20;
        dice.base = 20;
        if (info) return dice_info_dur(dice);
        if (cast) plr_tim_add(T_RES_TIME, dice_roll(dice));
        break;
    case 26:
        if (name) return "Insanity Circle";
        if (desc) return "Generate balls of chaos, confusion and charm centered on you.";
        dice.base = 25 + plr->lev/2 + plr->to_d_spell;
        rad = 3 + plr->lev/20;
        if (info) return dice_info_dam(dice);
        if (cast) {
            plr_burst(rad, GF_CHAOS, dice_roll(dice));
            plr_burst(rad, GF_CONFUSION, dice_roll(dice));
            plr_burst(rad, GF_CHARM, 20 + plr->lev);
        }
        break;
    case 27:
        if (name) return "Explode Pets";
        if (desc) return "Makes all pets explode.";
        if (cast) discharge_minion();
        break;
    case 28:
        if (name) return "Summon Greater Demon";
        if (desc) return "Summons greater demon. It need to sacrifice a corpse of human ('p','h' or 't').";
        if (cast && !cast_summon_greater_demon()) return NULL;
        break;
    case 29:
        if (name) return "Hellfire";
        if (desc) return "Fires a powerful ball of evil power. Hurts good monsters greatly.";
        dice.base = 666 + plr->to_d_spell;
        if (info) return dice_info_dam(dice);
        if (cast) {
            if (!plr_cast_ball(3, GF_HELL_FIRE, dice)) return NULL;
            take_hit(DAMAGE_USELIFE, 20 + _1d(30), "the strain of casting Hellfire");
        }
        break;
    case 30:
        if (name) return "Send to Hell";
        if (desc) return "Attempts to send a single monster directly to hell.";
        if (cast) {
            mon_ptr mon = plr_target_mon();
            if (!mon) return NULL;
            if (genocide_aux(mon, 666, TRUE, (mon_lvl(mon) + 1) / 2, "Genocide One"))
            {
                char name[MAX_NLEN_MON];
                monster_desc(name, mon, 0);
                msg_format("%^s is sent directly to hell!", name);
                virtue_add(VIRTUE_VITALITY, -1);
            }
        }
        break;
    case 31:
        if (name) return "Polymorph Demonlord";
        if (desc) return "Mimic a demon lord for a while. Loses abilities of original race and gets great abilities as a demon lord. Even hard walls can't stop your walking.";
        dice.dd = 1;
        dice.ds = 15;
        dice.base = 15;
        if (info) return dice_info_dur(dice);
        if (cast) set_mimic(dice_roll(dice), MIMIC_DEMON_LORD, FALSE);
        break;
    }
    return "";
}


static cptr do_crusade_spell(int spell, int mode)
{
    bool name = (mode == SPELL_NAME) ? TRUE : FALSE;
    bool desc = (mode == SPELL_DESC) ? TRUE : FALSE;
    bool info = (mode == SPELL_INFO) ? TRUE : FALSE;
    bool cast = (mode == SPELL_CAST) ? TRUE : FALSE;

    int rng = DUN_PATH_MAX;
    int rad = 0;
    dice_t dice = {0};

    dice.scale = spell_power(1000);
    dice.scale += virtue_current(VIRTUE_JUSTICE) / 3;
    dice.scale += virtue_current(VIRTUE_HONOUR) / 3;

    switch (spell)
    {
    case 0:
        if (name) return "Punishment";
        if (desc) return "Fires a bolt or beam of lightning.";
        dice.dd = 3 + (plr->lev - 1)/5;
        dice.ds = 4;
        dice.base = plr->to_d_spell;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_bolt_or_beam(GF_ELEC, dice, beam_chance() - 10)) return NULL;
        break;
    case 1:
        if (name) return "Detect Evil";
        if (desc) return "Detects all evil monsters in your vicinity.";
        if (cast) detect_monsters_evil(DETECT_RAD_DEFAULT);
        break;
    case 2:
        if (name) return "Remove Fear";
        if (desc) return "Removes fear.";
        if (cast) fear_clear_p();
        break;
    case 3:
        if (name) return "Scare Monster";
        if (desc) return "Attempts to scare a monster.";
        dice.base = plr->lev;
        if (info) return dice_info_power(dice);
        if (cast && !plr_cast_bolt(GF_FEAR, dice)) return NULL;
        break;
    case 4:
        if (name) return "Sanctuary";
        if (desc) return "Attempts to sleep monsters in the adjacent squares.";
        dice.base = plr->lev;
        if (info) return dice_info_power(dice);
        if (cast) plr_burst(1, GF_SLEEP, dice_roll(dice));
        break;
    case 5:
        if (name) return "Portal";
        if (desc) return "Teleport medium distance.";
        rng = 25 + plr->lev/2;
        if (info) return info_range(rng);
        if (cast) {
            if (mut_present(MUT_ASTRAL_GUIDE))
                energy_use /= 3;
            teleport_player(rng, 0);
        }
        break;
    case 6:
        if (name) return "Star Dust";
        if (desc) return "Fires many bolts of light near the target.";
        dice.dd = 3 + (plr->lev - 1)/9;
        dice.ds = 2;
        dice.base = (plr->to_d_spell + dice.dd - 1)/dice.dd;
        if (info) return dice_info_dam_each(dice);
        if (cast && !plr_cast_star_dust(10, GF_LIGHT, dice)) return NULL;
        break;
    case 7:
        if (name) return "Purify";
        if (desc) return "Heals cuts and stuns as well as reducing poison.";
        if (cast) {
            plr_tim_remove(T_CUT);
            plr_tim_recover(T_POISON, 80, 50);
            plr_tim_remove(T_STUN);
        }
        break;
    case 8:
        if (name) return "Scatter Evil";
        if (desc) return "Attempts to teleport an evil monster away.";
        dice.base = 5*DUN_VIEW_MAX;
        if (info) return dice_info_power(dice);
        if (cast && !plr_cast_ball(0, GF_AWAY_EVIL, dice)) return NULL;
        break;
    case 9:
        if (name) return "Holy Orb";
        if (desc) return "Fires a ball with holy power. Hurts evil monsters greatly, but don't effect good monsters.";
        dice.dd = 3;
        dice.ds = 6;
        dice.base = plr->to_d_spell;
        rad = 2 + plr->lev/30;
        if (plr->pclass == CLASS_PRIEST ||
            plr->pclass == CLASS_HIGH_PRIEST ||
            plr->pclass == CLASS_HIGH_MAGE ||
            plr->pclass == CLASS_SORCERER)
            dice.base += plr->lev + plr->lev / 2;
        else
            dice.base += plr->lev + plr->lev / 4;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_ball(rad, GF_HOLY_FIRE, dice)) return NULL;
        break;
    case 10:
        if (name) return "Exorcism";
        if (desc) return "Damages all undead and demons in sight, and scares all evil monsters in sight.";
        dice.dd = 1;
        dice.ds = plr->lev;
        dice.base = plr->to_d_spell;
        if (info) return dice_info_dam(dice);
        if (cast) {
            plr_project_los(GF_DISP_UNDEAD, dice_roll(dice));
            plr_project_los(GF_DISP_DEMON, dice_roll(dice));
            plr_project_los(GF_TURN_EVIL, plr->lev);
        }
        break;
    case 11:
        if (name) return "Remove Curse";
        if (desc) return "Removes normal curses from equipped items.";
        if (cast) {
            if (remove_curse())
                msg_print("You feel as if someone is watching over you.");
        }
        break;
    case 12:
        if (name) return "Sense Unseen";
        if (desc) return "Gives see invisible for a while.";
        dice.dd = 1;
        dice.ds = 25;
        dice.base = 25;
        if (info) return dice_info_dur(dice);
        if (cast) plr_tim_add(T_SEE_INVIS, dice_roll(dice));
        break;
    case 13:
        if (name) return "Protection from Evil";
        if (desc) return "Gives aura which protect you from evil monster's physical attack.";
        dice.dd = 1;
        dice.ds = 25;
        dice.base = 3*plr->lev; /* seems high, but cf mon_hit_plr */
        if (info) return dice_info_dur(dice);
        if (cast) plr_tim_add(T_PROT_EVIL, dice_roll(dice));
        break;
    case 14:
        if (name) return "Judgment Thunder";
        if (desc) return "Fires a powerful bolt of lightning.";
        dice.base = 5*plr->lev + plr->to_d_spell;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_bolt(GF_ELEC, dice)) return NULL;
        break;
    case 15:
        if (name) return "Holy Word";
        if (desc) return "Damages all evil monsters in sight, heals HP, and completely heals stun and cut status.";
        dice.base = 3*plr->lev + plr->to_d_spell;
        if (info) return dice_info_dam(dice);
        if (cast) {
            plr_project_los(GF_DISP_EVIL, dice_roll(dice));
            dice.base = 100;
            hp_player(dice_roll(dice));
            plr_tim_remove(T_STUN);
            plr_tim_remove(T_CUT);
        }
        break;
    case 16:
        if (name) return "Unbarring Ways";
        if (desc) return "Fires a beam which destroy traps and doors.";
        if (cast && !plr_cast_beam(GF_KILL_DOOR, dice)) return NULL;
        break;
    case 17:
        if (name) return "Arrest";
        if (desc) return "Attempts to paralyze an evil monster.";
        dice.base = 2*plr->lev;
        if (info) return dice_info_power(dice);
        if (cast && !plr_cast_ball(0, GF_STASIS_EVIL, dice)) return NULL;
        break;
    case 18:
        if (name) return "Angelic Cloak";
        if (desc) return "Gives resistance to acid, cold and lightning. Gives aura of holy power which injures evil monsters which attacked you for a while.";
        dice.dd = 1;
        dice.ds = 20;
        dice.base = 20;
        if (info) return dice_info_dur(dice);
        if (cast) {
            plr_tim_add(T_RES_ACID, dice_roll(dice));
            plr_tim_add(T_RES_COLD, dice_roll(dice));
            plr_tim_add(T_RES_ELEC, dice_roll(dice));
            plr_tim_add(T_AURA_HOLY, dice_roll(dice));
        }
        break;
    case 19:
        if (name) return "Dispel Undead & Demons";
        if (desc) return "Damages all undead and demons in sight.";
        dice.base = 3*plr->lev + plr->to_d_spell;
        if (info) return dice_info_dam(dice);
        if (cast) {
            plr_project_los(GF_DISP_UNDEAD, dice_roll(dice));
            plr_project_los(GF_DISP_DEMON, dice_roll(dice));
        }
        break;
    case 20:
        if (name) return "Dispel Evil";
        if (desc) return "Damages all evil monsters in sight.";
        dice.base = 5*plr->lev/2 + plr->to_d_spell;
        if (info) return dice_info_dam(dice);
        if (cast) plr_project_los(GF_DISP_EVIL, dice_roll(dice));
        break;
    case 21:
        if (name) return "Holy Blade";
        if (desc) return "Makes current weapon especially deadly against evil monsters.";
        if (cast) brand_weapon_slaying(OF_SLAY_EVIL, OF_INVALID);
        break;
    case 22:
        if (name) return "Star Burst";
        if (desc) return "Fires a huge ball of powerful light.";
        dice.base = 100 + plr_prorata_level_aux(200, 1, 1, 2) + plr->to_d_spell;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_ball(4, GF_LIGHT, dice)) return NULL;
        break;
    case 23:
        if (name) return "Summon Angel";
        if (desc) return "Summons an angel.";
        if (cast) {
            bool pet = !one_in_(3);
            who_t  who = pet ? who_create_plr() : who_create_null();
            u32b mode = 0L;

            if (pet) mode |= PM_FORCE_PET;
            else mode |= PM_NO_PET;
            if (!(pet && (plr->lev < 50))) mode |= PM_ALLOW_GROUP;

            if (summon_specific(who, plr->pos, plr->lev*3/2, SUMMON_ANGEL, mode))
            {
                if (pet)
                    msg_print("'What is thy bidding... Master?'");
                else
                    msg_print("Mortal! Repent of thy impiousness.");
            }
        }
        break;
    case 24:
        if (name) return "Heroism";
        if (desc) return "Removes fear, and gives bonus to hit and 10 more HP for a while.";
        dice.dd = 1;
        dice.ds = 25;
        dice.base = 25;
        if (info) return dice_info_dur(dice);
        if (cast) {
            bool heal = !plr_tim_find(T_HERO);
            plr_tim_add(T_HERO, dice_roll(dice));
            if (heal) hp_player(10);
        }
        break;
    case 25:
        if (name) return "Dispel Curse";
        if (desc) return "Removes normal and heavy curse from equipped items.";
        if (cast) {
            if (remove_all_curse())
                msg_print("You feel as if someone is watching over you.");
        }
        break;
    case 26:
        if (name) return "Banish Evil";
        if (desc) return "Teleports all evil monsters in sight away unless resisted.";
        dice.base = 100;
        if (info) return dice_info_power(dice);
        if (cast) {
            if (plr_project_los(GF_AWAY_EVIL, dice_roll(dice)))
                msg_print("The holy power banishes evil!");
        }
        break;
    case 27:
        if (name) return "Armageddon";
        if (desc) return "Destroy everything in nearby area.";
        dice.base = 4*plr->lev;
        if (info) return dice_info_power(dice);
        if (cast) destroy_area(plr->pos, 12 + _1d(4), dice_roll(dice));
        break;
    case 28:
        if (name) return "An Eye for an Eye";
        if (desc) return "Gives special aura for a while. When you are attacked by a monster, the monster are injured with same amount of damage as you take.";
        dice.dd = 1;
        dice.ds = 10;
        dice.base = 10;
        if (info) return dice_info_dur(dice);
        if (cast) plr_tim_add(T_REVENGE, dice_roll(dice));
        break;
    case 29:
        if (name) return "Wrath of the God";
        if (desc) return "Drops many balls of disintegration near the target.";
        dice.base = 25 + 3*plr->lev + plr->to_d_spell;
        if (info) return dice_info_dam_each(dice);
        if (cast && !plr_cast_wrath_of_god(GF_DISINTEGRATE, dice)) return NULL; 
        break;
    case 30:
        if (name) return "Divine Intervention";
        if (desc) return "Damages all adjacent monsters with holy power. Damages and attempt to slow, stun, confuse, scare and freeze all monsters in sight. And heals HP.";

        {
            int b_dam = spell_power(plr->lev * 11/2);
            int d_dam = spell_power(plr->lev * 4 + plr->to_d_spell);
            int heal = spell_power(100);
            int power = spell_power(plr->lev * 4);

            if (info) return format("h%d/dm%d+%d", heal, d_dam, b_dam);

            if (cast)
            {
                plr_burst(1, GF_HOLY_FIRE, b_dam);
                plr_project_los(GF_DISP_ALL, d_dam);
                plr_project_los(GF_SLOW, power);
                plr_project_los(GF_STUN, 5 + plr->lev/5);
                plr_project_los(GF_OLD_CONF, power);
                plr_project_los(GF_FEAR, power);
                plr_project_los(GF_STASIS, power/3);
                hp_player(heal);
            }
        }
        break;

    case 31:
        if (name) return "Crusade";
        if (desc) return "Attempts to charm all good monsters in sight, and scare all non-charmed monsters, and summons great number of knights, and gives heroism, bless, speed and protection from evil.";
        if (cast) cast_crusade();
        break;
    }

    return "";
}

static cptr do_armageddon_spell(int spell, int mode)
{
    bool name = (mode == SPELL_NAME) ? TRUE : FALSE;
    bool desc = (mode == SPELL_DESC) ? TRUE : FALSE;
    bool info = (mode == SPELL_INFO) ? TRUE : FALSE;
    bool cast = (mode == SPELL_CAST) ? TRUE : FALSE;

    int rad = 0;
    dice_t dice = {0};

    dice.scale = spell_power(1000);

    switch (spell)
    {
    /* Book of Elements */
    case 0:
        if (name) return "Lightning Bolt";
        if (desc) return "Fires a bolt or beam of electricity.";
        dice.dd = 3 + plr->lev/4;
        dice.ds = 8;
        dice.base = plr->to_d_spell;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_bolt_or_beam(GF_ELEC, dice, beam_chance())) return NULL;
        break;
    case 1:
        if (name) return "Frost Bolt";
        if (desc) return "Fires a bolt or beam of cold.";
        dice.dd = 4 + plr->lev/4;
        dice.ds = 8;
        dice.base = plr->to_d_spell;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_bolt_or_beam(GF_COLD, dice, beam_chance())) return NULL;
        break;
    case 2:
        if (name) return "Fire Bolt";
        if (desc) return "Fires a bolt or beam of fire.";
        dice.dd = 5 + plr->lev/4;
        dice.ds = 8;
        dice.base = plr->to_d_spell;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_bolt_or_beam(GF_FIRE, dice, beam_chance())) return NULL;
        break;
    case 3:
        if (name) return "Acid Bolt";
        if (desc) return "Fires a bolt or beam of acid.";
        dice.dd = 5 + plr->lev/4;
        dice.ds = 8;
        dice.base = plr->to_d_spell;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_bolt_or_beam(GF_ACID, dice, beam_chance())) return NULL;
        break;
    case 4:
        if (name) return "Lightning Ball";
        if (desc) return "Fires a ball of electricity.";
        dice.base = 20 + 3*plr->lev/2 + plr->to_d_spell;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_ball(2, GF_ELEC, dice)) return NULL;
        break;
    case 5:
        if (name) return "Frost Ball";
        if (desc) return "Fires a ball of cold.";
        dice.base = 25 + 3*plr->lev/2 + plr->to_d_spell;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_ball(2, GF_COLD, dice)) return NULL;
        break;
    case 6:
        if (name) return "Fire Ball";
        if (desc) return "Fires a ball of fire.";
        dice.base = 30 + 3*plr->lev/2 + plr->to_d_spell;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_ball(2, GF_FIRE, dice)) return NULL;
        break;
    case 7:
        if (name) return "Acid Ball";
        if (desc) return "Fires a ball of acid.";
        dice.base = 35 + 3*plr->lev/2 + plr->to_d_spell;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_ball(2, GF_ACID, dice)) return NULL;
        break;
    /* Earth, Wind and Fire */
    case 8:
        if (name) return "Shard Bolt";
        if (desc) return "Fires a bolt or beam of shards.";
        dice.dd = 7 + plr->lev/4;
        dice.ds = 8;
        dice.base = plr->to_d_spell;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_bolt_or_beam(GF_SHARDS, dice, beam_chance())) return NULL;
        break;
    case 9:
        if (name) return "Gravity Bolt";
        if (desc) return "Fires a bolt or beam of gravity.";
        dice.dd = 5 + plr->lev/4;
        dice.ds = 8;
        dice.base = plr->to_d_spell;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_bolt_or_beam(GF_GRAVITY, dice, beam_chance())) return NULL;
        break;
    case 10:
        if (name) return "Plasma Bolt";
        if (desc) return "Fires a bolt or beam of plasma.";
        dice.dd = 11 + plr->lev/4;
        dice.ds = 8;
        dice.base = plr->to_d_spell;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_bolt_or_beam(GF_PLASMA, dice, beam_chance())) return NULL;
        break;
    case 11:
        if (name) return "Meteor";
        if (desc) return "Fires a meteor.";
        dice.base = 60 + plr->lev + plr->to_d_spell;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_ball(2, GF_METEOR, dice)) return NULL;
        break;
    case 12:
        if (name) return "Thunderclap";
        if (desc) return "Generates a ball of sound centered on you.";
        dice.base = 40 + plr->lev + plr->to_d_spell;
        rad = 2 + plr->lev/10;
        if (info) return dice_info_dam(dice);
        if (cast) {
            msg_print("BOOM!");
            plr_burst(rad, GF_SOUND, dice_roll(dice));
        }
        break;
    case 13:
        if (name) return "Windblast";
        if (desc) return "Fires a microburst of strong winds.";
        dice.base = 40 + plr->lev + plr->to_d_spell;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_ball(2, GF_STORM, dice)) return NULL;
        break;
    case 14:
        if (name) return "Hellstorm";
        if (desc) return "Generates a huge ball of fire centered on you.";
        dice.base = 6*plr->lev + plr->to_d_spell;
        if (info) return dice_info_dam(dice);
        if (cast) plr_burst(8, GF_FIRE, dice_roll(dice));
        break;
    case 15:
        if (name) return "Rocket";
        if (desc) return "Fires a rocket.";
        dice.base = 60 + 4*plr->lev + plr->to_d_spell;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_rocket(2, dice)) return NULL;
        break;
    /* Path of Destruction */
    case 16:
        if (name) return "Ice Bolt";
        if (desc) return "Fires a bolt of ice.";
        dice.dd = 5 + plr->lev/4;
        dice.ds = 15;
        dice.base = plr->to_d_spell;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_bolt(GF_ICE, dice)) return NULL;
        break;
    case 17:
        if (name) return "Water Ball";
        if (desc) return "Fires a ball of water.";
        dice.base = 30 + 2*plr->lev + plr->to_d_spell;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_ball(2, GF_WATER, dice)) return NULL;
        break;
    case 18:
        if (name) return "Breathe Lightning";
        if (desc) return "Breathes a cone of electricity at chosen target.";
        dice.base = 9*plr->lev/2 + plr->to_d_spell;
        rad = 2 + plr->lev/40;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_breath(rad, GF_ELEC, dice)) return NULL;
        break;
    case 19:
        if (name) return "Breathe Frost";
        if (desc) return "Breathes a cone of cold at chosen target.";
        dice.base = 9*plr->lev/2 + plr->to_d_spell;
        rad = 2 + plr->lev/40;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_breath(rad, GF_COLD, dice)) return NULL;
        break;
    case 20:
        if (name) return "Breathe Fire";
        if (desc) return "Breathes a cone of fire at chosen target.";
        dice.base = 5*plr->lev + plr->to_d_spell;
        rad = 2 + plr->lev/40;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_breath(rad, GF_FIRE, dice)) return NULL;
        break;
    case 21:
        if (name) return "Breathe Acid";
        if (desc) return "Breathes a cone of acid at chosen target.";
        dice.base = 5*plr->lev + plr->to_d_spell;
        rad = 2 + plr->lev/40;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_breath(rad, GF_ACID, dice)) return NULL;
        break;
    case 22:
        if (name) return "Breathe Plasma";
        if (desc) return "Breathes a cone of plasma at chosen target.";
        dice.base = 9*plr->lev/2 + plr->to_d_spell;
        rad = 2 + plr->lev/40;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_breath(rad, GF_PLASMA, dice)) return NULL;
        break;
    case 23:
        if (name) return "Breathe Gravity";
        if (desc) return "Breathes a cone of gravity at chosen target.";
        dice.base = 4*plr->lev + plr->to_d_spell;
        rad = 2 + plr->lev/40;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_breath(rad, GF_GRAVITY, dice)) return NULL;
        break;
    /* Day of Ragnarok */
    case 24:
        if (name) return "Mana Bolt";
        if (desc) return "Fires a bolt of mana.";
        dice.dd = 1;
        dice.ds = 5*plr->lev;
        dice.base = plr->to_d_spell;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_bolt(GF_MANA, dice)) return NULL;
        break;
    case 25:
        if (name) return "Plasma Ball";
        if (desc) return "Fires a ball of plasma.";
        dice.base = 90 + 2*plr->lev + plr->to_d_spell;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_ball(3, GF_PLASMA, dice)) return NULL;
        break;
    case 26:
        if (name) return "Mana Ball";
        if (desc) return "Fires a ball of pure mana.";
        dice.base = 100 + 4*plr->lev + plr->to_d_spell;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_ball(3, GF_MANA, dice)) return NULL;
        break;
    case 27:
        if (name) return "Breathe Sound";
        if (desc) return "Breathes a cone of sound at chosen target.";
        dice.base = 6*plr->lev + plr->to_d_spell;
        rad = 2 + plr->lev/40;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_breath(rad, GF_SOUND, dice)) return NULL;
        break;
    case 28:
        if (name) return "Breathe Inertia";
        if (desc) return "Breathes a cone of inertia at chosen target.";
        dice.base = 5*plr->lev + plr->to_d_spell;
        rad = 2 + plr->lev/40;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_breath(rad, GF_INERTIA, dice)) return NULL;
        break;
    case 29:
        if (name) return "Breathe Disintegration";
        if (desc) return "Breathes a cone of disintegration at chosen target.";
        dice.base = 7*plr->lev + plr->to_d_spell;
        rad = 2 + plr->lev/40;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_breath(rad, GF_DISINTEGRATE, dice)) return NULL;
        break;
    case 30:
        if (name) return "Breathe Mana";
        if (desc) return "Breathes a cone of mana at chosen target.";
        dice.base = 9*plr->lev + plr->to_d_spell;
        rad = 2 + plr->lev/40;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_breath(rad, GF_MANA, dice)) return NULL;
        break;
    case 31:
        if (name) return "Breathe Shards";
        if (desc) return "Breathes a cone of shards at chosen target.";
        dice.base = 10*plr->lev + plr->to_d_spell;
        rad = 2 + plr->lev/40;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_breath(rad, GF_SHARDS, dice)) return NULL;
        break;
    }
    return "";
}
/*
 * Do everything for each spell
 */
int current_spell_cost; /* For Music and Hex upkeep */
cptr do_spell(int realm, int spell, int mode)
{
    cptr result = NULL;
    if (mode == SPELL_ON_BROWSE && realm != REALM_HISSATSU) return NULL;

    _current_realm_hack = realm;

    switch (realm)
    {
    case REALM_LIFE:     result = do_life_spell(spell, mode); break;
    case REALM_SORCERY:  result = do_sorcery_spell(spell, mode); break;
    case REALM_NATURE:   result = do_nature_spell(spell, mode); break;
    case REALM_CHAOS:    result = do_chaos_spell(spell, mode); break;
    case REALM_DEATH:    result = do_death_spell(spell, mode); break;
    case REALM_TRUMP:    result = do_trump_spell(spell, mode); break;
    case REALM_ARCANE:   result = do_arcane_spell(spell, mode); break;
    case REALM_CRAFT:    result = do_craft_spell(spell, mode); break;
    case REALM_DAEMON:   result = do_daemon_spell(spell, mode); break;
    case REALM_ARMAGEDDON: result = do_armageddon_spell(spell, mode); break;
    case REALM_CRUSADE:  result = do_crusade_spell(spell, mode); break;
    case REALM_ILLUSION: result = do_illusion_spell(spell, mode); break;
    case REALM_MUSIC:    result = do_music_spell(spell, mode); break;
    case REALM_HEX:      result = do_hex_spell(spell, mode); break;
    case REALM_BLESS:    result = do_bless_spell(spell, mode); break;
    case REALM_NECROMANCY: result = do_necromancy_spell(spell, mode); break;
    case REALM_BURGLARY: result = do_burglary_spell(spell, mode); break;
    }

    _current_realm_hack = 0;
    return result;
}

int get_realm_idx(cptr name)
{
    int i;
    for (i = 0; i < MAX_REALM; i++)
    {
        if (strcmp(name, realm_names[i]) == 0)
            return i;
    }
    return -1;
}
