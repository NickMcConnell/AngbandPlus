/*
 * File: player-attack.c
 * Purpose: Attacks (both throwing and melee) by the player
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * Copyright (c) 2018 MAngband and PWMAngband Developers
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 2, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */


#include "s-angband.h"


/*
 * Returns percent chance of an object breaking after throwing or shooting.
 *
 * Artifacts will never break.
 *
 * Beyond that, each item kind has a percent chance to break (0-100). When the
 * object hits its target this chance is used.
 *
 * When an object misses it also has a chance to break. This is determined by
 * squaring the normaly breakage probability. So an item that breaks 100% of
 * the time on hit will also break 100% of the time on a miss, whereas a 50%
 * hit-breakage chance gives a 25% miss-breakage chance, and a 10% hit breakage
 * chance gives a 1% miss-breakage chance.
 */
int breakage_chance(const struct object *obj, bool hit_target)
{
    int perc = obj->kind->base->break_perc;

    if (obj->artifact) return 0;
    if (!hit_target) return (perc * perc) / 100;

    return perc;
}


/*
 * Determine if the player "hits" a monster with a missile.
 */
static int chance_of_missile_hit(struct player *p, struct object *missile,
    struct object *launcher, int y, int x)
{
    int bonus = p->state.to_h + missile->to_h;
    int chance;

    if (launcher)
    {
        s16b to_h;

        object_to_h(launcher, &to_h);
        bonus += to_h;
        chance = p->state.skills[SKILL_TO_HIT_BOW] + bonus * BTH_PLUS_ADJ;
    }
    else
        chance = p->state.skills[SKILL_TO_HIT_THROW] + bonus * BTH_PLUS_ADJ;

    return chance - distance(p->py, p->px, y, x);
}


/*
 * Determine if the player "hits" a monster.
 */
bool test_hit(int chance, int ac, int vis)
{
    int k = randint0(100);

    /* There is an automatic 12% chance to hit and 5% chance to miss */
    if (k < 17) return (k < 12);

    /* Penalize invisible targets */
    if (!vis) chance = chance / 2;

    /* Starting a bit higher up on the scale */
    if (chance < 9) chance = 9;

    /* Power competes against armor */
    return (randint0(chance) >= (ac * 2 / 3));
}


struct delayed_effects
{
    bool fear;
    bool poison;
    bool cut;
    bool stun;
    bool slow;
    bool conf;
    bool blind;
    bool para;
    bool stab_sleep;
    bool stab_flee;
};


/*
 * Determine standard melee damage.
 *
 * Factor in damage dice, to-dam and any brand or slay.
 */
static int melee_damage(struct player *p, struct object *obj, random_value dice, int best_mult,
    struct source *target, struct delayed_effects *effects, int *d_dam)
{
    int dmg = randcalc(dice, 0, RANDOMISE);
    s16b to_d;

    /* Base damage for Shadow touch and cuts/stuns */
    *d_dam = dmg;

    dmg *= best_mult;

    if (target->monster)
    {
        /* Stabbing attacks */
        if (effects->stab_sleep) dmg *= (3 + p->lev / 40);
        if (effects->stab_flee) dmg = dmg * 3 / 2;
    }

    object_to_d(obj, &to_d);
    dmg += to_d;

    return dmg;
}


/*
 * Determine standard ranged damage.
 *
 * Factor in damage dice, to-dam, multiplier and any brand or slay.
 */
static int ranged_damage(struct player *p, struct object *missile, struct object *launcher,
    int best_mult, int mult)
{
    int dam;

    /* If we have a slay or brand, modify the multiplier appropriately */
    if (best_mult > 1)
    {
        if (mult > 1) mult += best_mult;
        else mult = best_mult;
    }

    /* Apply damage: multiplier, slays, bonuses */
    dam = damroll(missile->dd, missile->ds);
    dam += missile->to_d;
    if (launcher)
    {
        s16b to_d;

        object_to_d(launcher, &to_d);
        dam += to_d;
    }
    dam *= mult;
    if (p->timed[TMD_BOWBRAND] && !p->brand.blast) dam += p->brand.dam;

    return dam;
}


/*
 * Check if a target is debuffed in such a way as to make a critical
 * hit more likely.
 */
static bool is_debuffed(struct source *target)
{
    if (target->monster)
    {
        return (target->monster->m_timed[MON_TMD_CONF] || target->monster->m_timed[MON_TMD_HOLD] ||
            target->monster->m_timed[MON_TMD_STUN] || target->monster->m_timed[MON_TMD_BLIND]);
    }
    if (target->player)
    {
        return (target->player->timed[TMD_CONFUSED] || target->player->timed[TMD_PARALYZED] ||
            target->player->timed[TMD_BLIND]);
    }
    return false;
}


/*
 * Determine damage for critical hits from shooting.
 *
 * Factor in item weight, total plusses, and player level.
 */
static int critical_shot(struct player *p, struct source *target, int weight, int plus, int dam,
    u32b *msg_type)
{
    int debuff_to_hit = (is_debuffed(target)? DEBUFF_CRITICAL_HIT: 0);
    int chance = weight + (p->state.to_h + plus + debuff_to_hit) * 4 + p->lev * 2;
    int power = weight + randint1(500);

    if (randint1(5000) > chance)
    {
        *msg_type = MSG_SHOOT_HIT;
        return dam;
    }
    if (power < 500)
    {
        *msg_type = MSG_HIT_GOOD;
        return 2 * dam + 5;
    }
    if (power < 1000)
    {
        *msg_type = MSG_HIT_GREAT;
        return 2 * dam + 10;
    }
    *msg_type = MSG_HIT_SUPERB;
    return 3 * dam + 15;
}


/*
 * Determine damage for critical hits from melee.
 *
 * Factor in weapon weight, total plusses, player level.
 */
static int critical_norm(struct player *p, struct source *target, int weight, int plus, int dam,
    u32b *msg_type)
{
    int debuff_to_hit = (is_debuffed(target)? DEBUFF_CRITICAL_HIT: 0);
    int chance = weight + (p->state.to_h + plus + debuff_to_hit) * 5 + p->lev * 3;
    int power = weight + randint1(650);

    /* Apply Touch of Death */
    if (p->timed[TMD_DEADLY] && magik(25))
    {
        *msg_type = MSG_HIT_HI_CRITICAL;
        return 4 * dam + 30;
    }

    if (randint1(5000) > chance)
    {
        *msg_type = MSG_HIT;
        return dam;
    }
    if (power < 400)
    {
        *msg_type = MSG_HIT_GOOD;
        return dam * 2 + 5;
    }
    if (power < 700)
    {
        *msg_type = MSG_HIT_GREAT;
        return dam * 2 + 10;
    }
    if (power < 900)
    {
        *msg_type = MSG_HIT_SUPERB;
        return dam * 3 + 15;
    }
    if (power < 1300)
    {
        *msg_type = MSG_HIT_HI_GREAT;
        return dam * 3 + 20;
    }
    *msg_type = MSG_HIT_HI_SUPERB;
    return 4 * dam + 20;
}


/*
 * Apply the player damage bonuses
 */
static int player_damage_bonus(struct player_state *state)
{
    return state->to_d;
}


/*
 * Apply blow side effects
 */
static void blow_side_effects(struct player *p, struct source *target,
    struct delayed_effects *effects, struct side_effects *seffects, bool do_conf,
    struct object *obj, char name[NORMAL_WID], bool do_blind, bool do_para, bool do_fear,
    bool *do_quake, int dmg, random_value dice, int d_dam, bool do_slow)
{
    /* Apply poison */
    if (seffects->do_poison)
    {
        if (target->monster)
        {
            if (mon_inc_timed(p, target->monster, MON_TMD_POIS, 5 + randint1(5),
                MON_TMD_FLG_NOMESSAGE))
            {
                effects->poison = true;
            }
        }
        else
            player_inc_timed(target->player, TMD_POISONED, randint1(p->lev) + 5, true, true);
    }

    /* Apply Shadow Touch */
    if (p->timed[TMD_TOUCH] && target->monster && !monster_is_nonliving(target->monster->race))
    {
        int drain = ((d_dam > target->monster->hp)? target->monster->hp: d_dam);

        hp_player_safe(p, 1 + drain / 2);
    }

    /* Confusion attack */
    if (p->confusing)
    {
        p->confusing = false;
        msg(p, "Your hands stop glowing.");
        do_conf = true;
    }

    /* Handle polymorphed players */
    if (p->poly_race && obj)
    {
        int m = randint0(z_info->mon_blows_max);

        /* Extract the attack infomation */
        struct blow_effect *effect = p->poly_race->blow[m].effect;
        struct blow_method *method = p->poly_race->blow[m].method;

        melee_effect_handler_context_t context;
        melee_effect_handler_f effect_handler;

        /* There must be an attack */
        if (method)
        {
            /* Describe the attack method */
            seffects->do_cut = method->cut;
            seffects->do_stun = method->stun;

            /* Initialize */
            context.p = p;
            context.target = target;
            context.ddesc = name;
            context.do_blind = do_blind;
            context.do_para = do_para;
            context.do_conf = do_conf;
            context.do_fear = do_fear;
            strnfmt(context.flav, sizeof(context.flav), "was killed by %s", name);
            context.do_quake = *do_quake;
            context.do_stun = seffects->do_stun;
            context.damage = dmg;
            context.style = TYPE_PVX;

            /* Perform the actual effect. */
            effect_handler = melee_handler_for_blow_effect(effect->name);

            if (effect_handler != NULL)
                effect_handler(&context);
            else
                plog_fmt("Effect handler not found for %s.", effect->name);

            /* Save any changes made in the handler for later use. */
            do_blind = context.do_blind;
            do_para = context.do_para;
            do_conf = context.do_conf;
            do_fear = context.do_fear;
            *do_quake = context.do_quake;
            seffects->do_stun = context.do_stun;
        }
    }

    /* Ghosts get fear attacks */
    if (p->ghost && !player_can_undead(p)) do_fear = true;

    /* Hack -- only one of cut or stun */
    if (seffects->do_cut && seffects->do_stun)
    {
        /* Cancel cut */
        if (magik(50))
            seffects->do_cut = 0;

        /* Cancel stun */
        else
            seffects->do_stun = 0;
    }

    /* Handle cut */
    if (seffects->do_cut)
    {
        /* PvP */
        if (target->player)
        {
            seffects->do_cut = get_cut(dice, d_dam);

            /* Apply the cut */
            if (seffects->do_cut)
                player_inc_timed(target->player, TMD_CUT, seffects->do_cut, true, true);
        }
        else if (mon_inc_timed(p, target->monster, MON_TMD_CUT, 5 + randint1(5),
            MON_TMD_FLG_NOMESSAGE))
        {
            effects->cut = true;
        }
    }

    /* Handle stun */
    if (seffects->do_stun)
    {
        /* PvP: stunning attack */
        if (target->player)
        {
            seffects->do_stun = get_stun(dice, d_dam);

            /* Apply the stun */
            if (seffects->do_stun)
                player_inc_timed(target->player, TMD_STUN, seffects->do_stun, true, true);
        }
        else if (mon_inc_timed(p, target->monster, MON_TMD_STUN, 5 + randint1(5),
            MON_TMD_FLG_NOMESSAGE))
        {
            effects->stun = true;
        }
    }

    /* Apply slowing */
    if (do_slow)
    {
        /* PvP: slowing attack */
        if (target->player)
            player_inc_timed(target->player, TMD_SLOW, randint0(4) + 4, true, true);
        else if (dmg && mon_inc_timed(p, target->monster, MON_TMD_SLOW, 20, MON_TMD_FLG_NOMESSAGE))
            effects->slow = true;
    }

    /* Apply fear */
    if (do_fear)
    {
        /* PvP: fear attack */
        if (target->player)
        {
            /* Player is terrified */
            if (magik(target->player->state.skills[SKILL_SAVE]))
                msg(p, "%s is unaffected.", name);
            else
                player_inc_timed(target->player, TMD_AFRAID, 3 + randint1(p->lev), true, true);
        }
        else if (mon_inc_timed(p, target->monster, MON_TMD_FEAR, 10 + randint1(10),
            MON_TMD_FLG_NOMESSAGE))
        {
            effects->fear = true;
        }
    }

    /* Apply confusion */
    if (do_conf)
    {
        /* PvP: confusing attack */
        if (target->player)
        {
            /* Player is confused */
            player_inc_timed(target->player, TMD_CONFUSED, 10 + randint0(p->lev) / 10, true, true);
        }
        else if (mon_inc_timed(p, target->monster, MON_TMD_CONF, 5 + randint1(5),
            MON_TMD_FLG_NOMESSAGE))
        {
            effects->conf = true;
        }
    }

    /* Apply blindness */
    if (do_blind)
    {
        /* PvP: blinding attack */
        if (target->player)
        {
            /* Player is blinded */
            player_inc_timed(target->player, TMD_BLIND, 10 + randint1(p->lev), true, true);
        }
        else if (mon_inc_timed(p, target->monster, MON_TMD_BLIND, 5 + randint1(5),
            MON_TMD_FLG_NOMESSAGE))
        {
            effects->blind = true;
        }
    }

    /* Handle paralysis */
    if (do_para)
    {
        /* PvP: paralyzing attack */
        if (target->player)
        {
            /* Player is paralyzed */
            player_inc_timed(target->player, TMD_PARALYZED, 3 + randint1(p->lev), true, true);
        }
        else if (mon_inc_timed(p, target->monster, MON_TMD_HOLD, 3 + randint1(5),
            MON_TMD_FLG_NOMESSAGE))
        {
            effects->para = true;
        }
    }
}


/*
 * Apply blow after effects
 */
static bool blow_after_effects(struct player *p, struct chunk *c, int y, int x, bool circle,
    int dmg, bool quake)
{
    bool stop = false;

    /* Apply circular kick: do damage to anything around the attacker */
    if (circle)
    {
        fire_ball(p, PROJ_MISSILE, 0, dmg, 1, false);
        show_monster_messages(p);

        /* Target may be dead */
        if (!c->squares[y][x].mon) stop = true;
    }

    /* Apply earthquake brand */
    if (quake)
    {
        struct source who_body;
        struct source *who = &who_body;

        source_player(who, get_player_index(get_connection(p->conn)), p);
        effect_simple(EF_EARTHQUAKE, who, "0", 0, 10, 0, NULL);

        /* Target may be dead or moved */
        if (!c->squares[y][x].mon) stop = true;
    }

    return stop;
}


/* Melee and throwing hit types */
static const struct hit_types melee_hit_types[] =
{
    {MSG_MISS, NULL},
    {MSG_HIT, NULL},
    {MSG_HIT_GOOD, "It was a good hit!"},
    {MSG_HIT_GREAT, "It was a great hit!"},
    {MSG_HIT_SUPERB, "It was a superb hit!"},
    {MSG_HIT_HI_GREAT, "It was a *GREAT* hit!"},
    {MSG_HIT_HI_SUPERB, "It was a *SUPERB* hit!"},
    {MSG_HIT_HI_CRITICAL, "It was a *CRITICAL* hit!"}
};


/*
 * Return the player's chance to hit with a particular weapon.
 */
int py_attack_hit_chance(struct player *p, const struct object *weapon)
{
    int chance, bonus = p->state.to_h;

    if (weapon)
    {
        s16b to_h;

        object_to_h(weapon, &to_h);
        bonus += to_h;
    }
    chance = p->state.skills[SKILL_TO_HIT_MELEE] + bonus * BTH_PLUS_ADJ;

    return chance;
}


/* Barehanded attack */
struct barehanded_attack
{
    const char *verb;   /* A verbose attack description */
    const char *hit_extra;
    int min_level;          /* Minimum level to use */
    int chance;             /* Chance of failure vs player level */
    int effect;             /* Special effects */
    bool racial;            /* true for dragons, false for monks */
};


/* Special effects for barehanded attacks */
enum
{
    MA_NONE,
    MA_SIDE,
    MA_DICE,
    MA_KNEE,
    MA_DAM,
    MA_SLOW,
    MA_STUN,
    MA_CUT,
    MA_JUMP,
    MA_CIRCLE,
    MA_CRUSH
};


/* Barehanded attacks */
#define MAX_MA 14
static struct barehanded_attack barehanded_attacks[MAX_MA] =
{
    /* Basic monk attack */
    {"punch", "", 1, 0, MA_NONE, false},

    /* Special monk attacks */
    {"kick", "", 2, 1, MA_SIDE, false},
    {"strike", " with your elbow", 3, 2, MA_DICE, false},
    {"ram", " with your knee", 5, 4, MA_KNEE, false},
    {"butt", "", 8, 7, MA_DAM, false},
    {"strike", "", 11, 10, MA_SLOW, false},
    {"uppercut", "", 15, 12, MA_STUN, false},
    {"hit", " with a Cat's Claw", 20, 15, MA_CUT, false},
    {"hit", " with a jump kick", 25, 20, MA_JUMP, false},
    {"hit", " with a circle kick", 35, 30, MA_CIRCLE, false},
    {"hit", " with a crushing blow", 45, 35, MA_CRUSH, false},

    /* Basic dragon attack */
    {"claw", "", 1, 0, MA_NONE, true},

    /* Special dragon attacks */
    {"bite", "", 5, 4, MA_DAM, true},
    {"crush", "", 45, 40, MA_CRUSH, true}
};


/*
 * Attack the monster at the given location with a single blow.
 */
static bool py_attack_real(struct player *p, struct chunk *c, int y, int x,
    struct delayed_effects *effects)
{
    size_t i;

    /* Information about the target of the attack */
    struct source target_body;
    struct source *target = &target_body;
    char target_name[NORMAL_WID];
    bool stop = false;
    bool visible;
    int ac;
    char name[NORMAL_WID];

    /* The weapon used */
    struct object *obj = equipped_item_by_slot_name(p, "weapon");

    /* Information about the attack */
    int chance = py_attack_hit_chance(p, obj);
    bool do_quake = false;
    bool success = false;

    /* Default to punching for one damage */
    char verb[30];
    int dmg = 1;
    u32b msg_type = MSG_HIT;

    /* Information about the attacker */
    char killer_name[NORMAL_WID];
    random_value dice;
    int show_mhit, show_mdam;
    int show_shit, show_sdam;
    int d_dam = 1;
    bool do_circle = false;
    const char *hit_extra = "";
    bool do_slow = false, do_fear = false, do_conf = false, do_blind = false, do_para = false;
    struct side_effects seffects;

    memset(&seffects, 0, sizeof(seffects));

    /* Default to punching for one damage */
    my_strcpy(verb, "punch", sizeof(verb));
    if (obj) my_strcpy(verb, "hit", sizeof(verb));

    /* Information about the target of the attack */
    square_actor(c, y, x, target);
    if (target->monster)
    {
        visible = monster_is_visible(p, target->idx);
        ac = target->monster->ac;
    }
    else
    {
        visible = player_is_visible(p, target->idx);
        ac = target->player->state.ac + target->player->state.to_a;
    }

    /* Extract target name */
    if (target->monster)
    {
        monster_desc(p, target_name, sizeof(target_name), target->monster,
            MDESC_OBJE | MDESC_IND_HID | MDESC_PRO_HID);
    }
    else
    {
        player_desc(p, target_name, sizeof(target_name), target->player, false);
        player_desc(p, name, sizeof(name), target->player, true);
    }

    /* Auto-Recall if possible and visible */
    if (target->monster && visible) monster_race_track(p->upkeep, target);

    /* Track a new monster */
    if (visible) health_track(p->upkeep, target);

    /* Handle player fear */
    if (player_of_has(p, OF_AFRAID))
    {
        equip_learn_flag(p, OF_AFRAID);
        msgt(p, MSG_AFRAID, "You are too afraid to attack %s!", target_name);
        return false;
    }

    /* Disturb the target */
    if (target->monster)
    {
        mon_clear_timed(p, target->monster, MON_TMD_SLEEP, MON_TMD_FLG_NOMESSAGE);
        mon_clear_timed(p, target->monster, MON_TMD_HOLD, MON_TMD_FLG_NOTIFY);
    }
    else
        disturb(target->player, 0);

    /* See if the player hit */
    success = test_hit(chance, ac, visible);

    /* Extract killer name */
    if (target->player)
        player_desc(target->player, killer_name, sizeof(killer_name), p, true);

    /* If a miss, skip this hit */
    if (!success)
    {
        effects->stab_sleep = false;
        msgt(p, MSG_MISS, "You miss %s.", target_name);
        if (target->player) msg(target->player, "%s misses you.", killer_name);
        return false;
    }

    /* Information about the attacker */
    memset(&dice, 0, sizeof(dice));
    get_plusses(p, &p->known_state, &dice.dice, &dice.sides, &show_mhit, &show_mdam, &show_shit,
        &show_sdam);

    /* Ghosts do barehanded damage relative to level */
    if (p->ghost && !player_can_undead(p))
        dmg = d_dam = randcalc(dice, 0, RANDOMISE);
    else
    {
        int best_mult = 1;

        /* Handle polymorphed players + temp branding */
        improve_attack_modifier(p, NULL, target, &best_mult, &seffects, verb, sizeof(verb), false);

        /*
         * Get the best attack from all slays or
         * brands on all non-launcher equipment
         */
        for (i = 2; i < (size_t)p->body.count; i++)
        {
            struct object *equipped = slot_object(p, i);

            if (equipped)
            {
                improve_attack_modifier(p, equipped, target, &best_mult, &seffects, verb,
                    sizeof(verb), false);
            }
        }

        /* Monks and dragons do barehanded damage with special attacks */
        if (player_has(p, PF_MARTIAL_ARTS) || player_has(p, PF_DRAGON))
        {
            struct barehanded_attack *ba_ptr;
            bool ok;

            /* Get an attack */
            do
            {
                ba_ptr = &barehanded_attacks[randint0(MAX_MA)];

                /* Monks: use monk attacks */
                if (player_has(p, PF_MARTIAL_ARTS))
                {
                    ok = !ba_ptr->racial;

                    /* Dragon monks do monk attacks 1/3 of the time */
                    if (player_has(p, PF_DRAGON) && magik(66))
                        ok = ba_ptr->racial;

                    /* Special monk attacks: only when not stunned, confused or encumbered */
                    else if (ok && (ba_ptr->effect != MA_NONE))
                        ok = !p->timed[TMD_STUN] && !p->timed[TMD_CONFUSED] && monk_armor_ok(p);
                }

                /* Dragons: use dragon attacks */
                else
                    ok = ba_ptr->racial;

                /* Apply minimum level */
                if (ok) ok = (ba_ptr->min_level <= p->lev);

                /* Chance of failure vs player level */
                if (ok) ok = !CHANCE(ba_ptr->chance, p->lev);
            }
            while (!ok);

            my_strcpy(verb, ba_ptr->verb, sizeof(verb));
            hit_extra = ba_ptr->hit_extra;

            /* Special effect: extra damage side */
            if (ba_ptr->effect == MA_SIDE) dice.sides++;

            /* Special effect: extra damage dice */
            if (ba_ptr->effect == MA_DICE) dice.dice++;

            /* Special effect: crushing attack */
            if (ba_ptr->effect == MA_CRUSH)
            {
                dice.dice += 2;
                dice.sides += 2;
                seffects.do_stun = 1;
            }

            /* Compute the damage */
            dmg = d_dam = randcalc(dice, 0, RANDOMISE);
            dmg *= best_mult;
            dmg = critical_norm(p, target, p->lev * randint1(10), p->lev, dmg, &msg_type);

            /* Special effect: knee attack */
            if (ba_ptr->effect == MA_KNEE)
            {
                bool male = false;

                /* Male target */
                if (target->monster)
                    male = rf_has(target->monster->race->flags, RF_MALE);
                else
                    male = (target->player->psex == SEX_MALE);

                /* Stuns male targets */
                if (male)
                {
                    hit_extra = " in the groin with your knee";
                    seffects.do_stun = 1;
                }
            }

            /* Special effect: extra damage */
            if (ba_ptr->effect == MA_DAM) dmg = dmg * 5 / 4;

            /* Special effect: slowing attack */
            if (ba_ptr->effect == MA_SLOW)
            {
                /* Slows some targets */
                if (target->monster && !rf_has(target->monster->race->flags, RF_NEVER_MOVE) &&
                    !monster_is_unique(target->monster->race) &&
                    (is_humanoid(target->monster->race) ||
                    rf_has(target->monster->race->flags, RF_HAS_LEGS)) &&
                    !CHANCE(target->monster->level - 10, (dmg < 11)? 1: (dmg - 10)))
                {
                    my_strcpy(verb, "kick", sizeof(verb));
                    hit_extra = " in the ankle";
                    do_slow = true;
                }
            }

            /* Special effect: stunning attack */
            if ((ba_ptr->effect == MA_STUN) || (ba_ptr->effect == MA_JUMP))
                seffects.do_stun = 1;

            /* Special effect: cutting attack */
            if ((ba_ptr->effect == MA_CUT) || (ba_ptr->effect == MA_JUMP))
                seffects.do_cut = 1;

            /* Special effect: circular attack */
            if (ba_ptr->effect == MA_CIRCLE)
                do_circle = true;

            /* Dragon monks do tail attacks */
            if (player_has(p, PF_DRAGON) && player_has(p, PF_MARTIAL_ARTS) && !ba_ptr->racial)
            {
                my_strcpy(verb, "hit", sizeof(verb));
                hit_extra = " with your tail";
            }
        }

        /* Handle normal weapon */
        else if (obj)
        {
            s16b to_h;

            /* Handle the weapon itself */
            improve_attack_modifier(p, obj, target, &best_mult, &seffects, verb, sizeof(verb),
                false);

            dmg = melee_damage(p, obj, dice, best_mult, target, effects, &d_dam);
            object_to_h(obj, &to_h);
            dmg = critical_norm(p, target, obj->weight, to_h, dmg, &msg_type);

            /* Learn by use for the weapon */
            object_notice_attack_plusses(p, obj);

            if (player_of_has(p, OF_IMPACT) && (dmg > 50))
            {
                do_quake = true;
                equip_learn_flag(p, OF_IMPACT);
            }
        }

        /* Default barehanded attack */
        else
        {
            /* Compute the damage */
            dmg = d_dam = randcalc(dice, 0, RANDOMISE);
            dmg *= best_mult;
        }
    }

    /* Learn by use for other equipped items */
    equip_learn_on_melee_attack(p);

    /* Apply the player damage bonuses */
    dmg += player_damage_bonus(&p->state);

    /* No negative damage; change verb if no damage done */
    if (dmg <= 0)
    {
        dmg = 0;
        msg_type = MSG_MISS;
        my_strcpy(verb, "fail to harm", sizeof(verb));
        hit_extra = "";
    }

    /* Special messages */
    if (target->monster)
    {
        /* Stabbing attacks */
        if (effects->stab_sleep)
            my_strcpy(verb, "cruelly stab", sizeof(verb));
        if (effects->stab_flee)
            my_strcpy(verb, "backstab", sizeof(verb));
    }
    else
    {
        /* Tell the target what happened */
        if (!dmg)
            msg(target->player, "%s fails to harm you.", killer_name);
        else
            msg(target->player, "%s hits you.", killer_name);
    }

    /* Tell the player what happened */
    for (i = 0; i < N_ELEMENTS(melee_hit_types); i++)
    {
        const char *dmg_text = "";

        if (msg_type != melee_hit_types[i].msg_type) continue;
        if (OPT(p, show_damage)) dmg_text = format(" (%d)", dmg);
        if (melee_hit_types[i].text)
        {
            msgt(p, msg_type, "You %s %s%s%s. %s", verb, target_name, hit_extra, dmg_text,
                melee_hit_types[i].text);
        }
        else
            msgt(p, msg_type, "You %s %s%s%s.", verb, target_name, hit_extra, dmg_text);
    }

    effects->stab_sleep = false;

    /* Pre-damage side effects */
    blow_side_effects(p, target, effects, &seffects, do_conf, obj, name, do_blind, do_para, do_fear,
        &do_quake, dmg, dice, d_dam, do_slow);

    /* Damage, check for fear and death */
    if (target->monster)
        stop = mon_take_hit(p, c, target->monster, dmg, &effects->fear, -2);
    else
    {
        char df[160];

        strnfmt(df, sizeof(df), "was brutally murdered by %s", p->name);
        stop = take_hit(target->player, dmg, p->name, false, df);

        /* Handle freezing aura */
        if (!stop && target->player->timed[TMD_ICY_AURA] && dmg)
        {
            if (magik(50))
                fire_ball(target->player, PROJ_ICE, 0, 1, 1, false);
            else
                fire_ball(target->player, PROJ_COLD, 0, 1 + target->player->lev / 5, 1, false);

            /* Stop if player is dead */
            if (p->is_dead) stop = true;

            /* Stop if player is stunned */
            if (p->timed[TMD_STUN]) stop = true;
        }
    }

    if (stop) memset(effects, 0, sizeof(struct delayed_effects));

    /* Post-damage effects */
    if (blow_after_effects(p, c, y, x, do_circle, dmg, do_quake))
        stop = true;

    return stop;
}


/*
 * Attack the monster at the given location
 *
 * We get blows until energy drops below that required for another blow, or
 * until the target monster dies. Each blow is handled by py_attack_real().
 * We don't allow @ to spend more than 100 energy in one go, to avoid slower
 * monsters getting double moves.
 */
void py_attack(struct player *p, struct chunk *c, int y, int x)
{
    int num_blows;
    bool stop = false;
    int blows = 0;
    struct delayed_effects effects;
    struct monster *mon = square_monster(c, y, x);
    bool visible = (mon && monster_is_visible(p, mon->midx));

    memset(&effects, 0, sizeof(effects));

    /* Handle polymorphed players */
    if (p->poly_race && rf_has(p->poly_race->flags, RF_NEVER_BLOW)) return;

    /* Unaware players attacking something reveal themselves */
    if (p->k_idx) aware_player(p, p);

    /* Rogues get stabbing attacks against sleeping and fleeing (visible) monsters */
    if (visible && player_has(p, PF_BACK_STAB))
    {
        if (mon->m_timed[MON_TMD_SLEEP]) effects.stab_sleep = true;
        else if (mon->m_timed[MON_TMD_FEAR]) effects.stab_flee = true;
    }

    /* Disturb the player */
    disturb(p, 0);

    /* Calculate number of blows */
    num_blows = (p->state.num_blows + p->state.frac_blow) / 100;

    /* Calculate remainder */
    p->state.frac_blow += (p->state.num_blows - num_blows * 100);

    /* Take blows until energy runs out or monster dies */
    while ((blows < num_blows) && !stop)
    {
        stop = py_attack_real(p, c, y, x, &effects);
        blows++;
    }

    /* Hack -- delay messages */
    if (visible && !stop)
    {
        if (effects.fear) add_monster_message(p, mon, MON_MSG_FLEE_IN_TERROR, true);
        if (effects.poison) add_monster_message(p, mon, MON_MSG_POISONED, true);
        if (effects.cut) add_monster_message(p, mon, MON_MSG_BLEED, true);
        if (effects.stun) add_monster_message(p, mon, MON_MSG_DAZED, true);
        if (effects.slow) add_monster_message(p, mon, MON_MSG_SLOWED, true);
        if (effects.conf) add_monster_message(p, mon, MON_MSG_CONFUSED, true);
        if (effects.blind) add_monster_message(p, mon, MON_MSG_BLIND, true);
        if (effects.para) add_monster_message(p, mon, MON_MSG_HELD, true);
    }

    /* Carry over the remaining energy to the next turn */
    p->state.frac_blow += (num_blows - blows) * 100;

    /* Hack -- limit to ONE turn */
    if (p->state.frac_blow > p->state.num_blows)
        p->state.frac_blow = p->state.num_blows;
}


void un_power(struct player *p, struct source *who, bool* obvious)
{
    struct object *obj;
    int tries;
    int unpower = 0, newcharge;
    int rlev;

    /* Get level */
    if (who->monster)
        rlev = ((who->monster->level >= 1)? who->monster->level: 1);
    else
        rlev = who->player->lev;

    /* Find an item */
    for (tries = 0; tries < 10; tries++)
    {
        /* Pick an item */
        obj = p->upkeep->inven[randint0(z_info->pack_size)];

        /* Skip non-objects */
        if (obj == NULL) continue;

        /* Drain charged wands/staves */
        if (tval_can_have_charges(obj))
        {
            /* Charged? */
            if (obj->pval)
            {
                /* Get number of charge to drain */
                unpower = (rlev / (obj->kind->level + 2)) + 1;

                /* Get new charge value, don't allow negative */
                newcharge = MAX((obj->pval - unpower), 0);

                /* Remove the charges */
                obj->pval = newcharge;
            }
        }

        if (unpower)
        {
            int heal = rlev * unpower;

            /* Message */
            msg(p, "Energy drains from your pack!");

            /* Obvious */
            *obvious = true;

            /* Heal */
            if (who->monster)
            {
                /* Don't heal more than max hp */
                heal = MIN(heal, who->monster->maxhp - who->monster->hp);

                /* Heal */
                who->monster->hp += heal;

                /* Redraw (later) if needed */
                update_health(who);
            }
            else
                hp_player_safe(who->player, heal);

            /* Combine the pack */
            p->upkeep->notice |= (PN_COMBINE);

            /* Redraw stuff */
            p->upkeep->redraw |= (PR_INVEN);

            /* Affect only a single inventory slot */
            break;
        }
    }
}


void eat_item(struct player *p, struct source *who, bool* obvious, int* blinked)
{
    int tries;

    /* Find an item */
    for (tries = 0; tries < 10; tries++)
    {
        struct object *obj, *stolen;
        char o_name[NORMAL_WID];
        bool split = false;
        bool none_left = false;

        /* Pick an item */
        int index = randint0(z_info->pack_size);

        /* Obtain the item */
        obj = p->upkeep->inven[index];

        /* Skip non-objects */
        if (obj == NULL) continue;

        /* Skip artifacts */
        if (obj->artifact) continue;

        /* Skip deeds of property */
        if (tval_is_deed(obj)) continue;

        /* PvP: can only steal items if they can be carried */
        if (who->player)
        {
            struct object *test = object_new();
            bool ok = true;

            /* Get a copy with the right "amt" */
            object_copy_amt(test, obj, 1);

            /* Note that the pack is too full */
            if (!inven_carry_okay(who->player, test)) ok = false;

            /* Note that the pack is too heavy */
            else if (!weight_okay(who->player, test)) ok = false;

            object_delete(&test);
            if (!ok) continue;
        }

        /* Get a description */
        object_desc(p, o_name, sizeof(o_name), obj, ODESC_FULL);

        /* Is it one of a stack being stolen? */
        if (obj->number > 1) split = true;

        /* Message */
        msg(p, "%s %s (%c) was stolen!", (split? "One of your": "Your"), o_name, I2A(index));

        /* Steal and carry */
        stolen = gear_object_for_use(p, obj, 1, false, &none_left);
        if (who->monster)
        {
            if (!monster_carry(who->monster, stolen, false))
                object_delete(&stolen);
        }
        else if (who->player)
            inven_carry(who->player, stolen, true, false);

        /* Obvious */
        *obvious = true;

        /* Blink away */
        *blinked = 2;

        /* Done */
        break;
    }
}


static void use_fud(struct player *p, struct object *obj)
{
    struct effect *effect;
    bool ident = false, used = false;

    /* The player is aware of the object's flavour */
    p->was_aware = object_flavor_is_aware(p, obj);

    /* Figure out effect to use */
    effect = object_effect(obj);

    /* Do effect */
    if (effect)
    {
        struct source who_body;
        struct source *who = &who_body;

        /* Make a noise! */
        sound(p, MSG_EAT);

        /* Do effect */
        if (effect->other_msg) msg_misc(p, effect->other_msg);
        source_player(who, get_player_index(get_connection(p->conn)), p);
        used = effect_do(effect, who, &ident, p->was_aware, 0, NULL, 0, 0, NULL);

        /* Quit if the item wasn't used and no knowledge was gained */
        if (!used && (p->was_aware || !ident)) return;
    }

    if (ident) object_notice_effect(p, obj);

    if (ident && !p->was_aware)
        object_learn_on_use(p, obj);
    else if (used)
        object_flavor_tried(p, obj);
}


void eat_fud(struct player *p, struct player *q, bool* obvious)
{
    int tries;

    /* Steal some food */
    for (tries = 0; tries < 10; tries++)
    {
        /* Pick an item from the pack */
        int index = randint0(z_info->pack_size);

        struct object *obj, *eaten;
        char o_name[NORMAL_WID];
        bool none_left = false;

        /* Get the item */
        obj = p->upkeep->inven[index];

        /* Skip non-objects */
        if (obj == NULL) continue;

        /* Only eat food */
        if (!tval_is_edible(obj)) continue;

        if (obj->number == 1)
        {
            object_desc(p, o_name, sizeof(o_name), obj, ODESC_BASE);
            msg(p, "Your %s (%c) was eaten!", o_name, I2A(index));
        }
        else
        {
            object_desc(p, o_name, sizeof(o_name), obj, ODESC_PREFIX | ODESC_BASE);
            msg(p, "One of your %s (%c) was eaten!", o_name, I2A(index));
        }

        /* PWMAngband: feed the offending player in PvP! */
        if (q) use_fud(q, obj);

        /* Steal and eat */
        eaten = gear_object_for_use(p, obj, 1, false, &none_left);
        object_delete(&eaten);

        /* Obvious */
        *obvious = true;

        /* Done */
        break;
    }
}


void drain_xp(struct player *p, int amt)
{
    int chance = 100 - (amt / 2) - (amt / 40) * 5;

    if (player_of_has(p, OF_HOLD_LIFE) && magik(chance))
        msg(p, "You keep hold of your life force!");
    else
    {
        s32b d = damroll(amt, 6) + (p->exp / 100) * z_info->life_drain_percent;
        if (player_of_has(p, OF_HOLD_LIFE))
        {
            msg(p, "You feel your life slipping away!");
            player_exp_lose(p, d / 10, false);
        }
        else
        {
            msg(p, "You feel your life draining away!");
            player_exp_lose(p, d, false);
        }
    }
}


void drop_weapon(struct player *p, int damage)
{
    int tmp;
    struct object *obj;

    /* This effect is *very* nasty - it should be very rare */
    /* So give a chance to avoid it to everyone */
    if (magik(50)) return;

    /* A high DEX will help a lot */
    tmp = adj_dex_safe[p->state.stat_ind[STAT_DEX]];
    if (tmp > 95) tmp = 95;
    if (magik(tmp)) return;

    /* Access the weapon */
    obj = equipped_item_by_slot_name(p, "weapon");

    /* No weapon used - bleeding effect instead */
    if (!obj)
    {
        player_inc_timed(p, TMD_CUT, 100, true, true);
        return;
    }

    /* Artifacts are safe */
    if (obj->artifact && magik(90)) return;

    /* Stuck weapons can't be removed */
    if (!obj_can_takeoff(obj)) return;

    /* Two-handed weapons are safe */
    if (kf_has(obj->kind->kind_flags, KF_TWO_HANDED) && magik(90)) return;

    /* Give an extra chance for comfortable weapons */
    if (magik(50) && !(p->state.heavy_wield || p->state.icky_wield || p->state.cumber_shield))
        return;

    /* Finally give an extra chance for weak blows */
    if (!magik(damage)) return;

    /* Really unlucky or really lousy fighters get disarmed */
    msg(p, "You lose grip of your weapon!");
    if (!inven_drop(p, obj, 1, true))
    {
        /* Protect true artifacts at shallow depths */
        msg(p, "You manage to catch your weapon before it falls to the ground.");
    }
    p->upkeep->update |= (PU_BONUS);
}


/*
 * Check for hostility (player vs target).
 */
static bool pvx_check(struct player *p, struct source *who, byte feat)
{
    /* Player here */
    if (who->player)
    {
        int mode = (target_equals(p, who)? PVP_DIRECT: PVP_INDIRECT);

        return pvp_check(p, who->player, mode, true, feat);
    }

    /* Monster here */
    if (who->monster) return pvm_check(p, who->monster);

    /* Nothing here */
    return false;
}


typedef struct delayed_ranged_effects
{
    struct monster *mon;
    int dmg;
    bool fear;
    bool poison;
    bool stun;
    bool cut;
    bool conf;
    struct delayed_ranged_effects *next;
} ranged_effects;


/*
 * This is a helper function to manage a linked list of delayed range effects.
 */
static ranged_effects *get_delayed_ranged_effects(ranged_effects **effects, struct monster *mon)
{
    ranged_effects *current = *effects;

    /* Walk through the list to get the corresponding monster */
    while (current)
    {
        /* Found a match */
        if (current->mon == mon) return current;

        current = current->next;
    }

    /* No match: create, assign and return */
    current = mem_zalloc(sizeof(ranged_effects));
    current->mon = mon;
    current->next = *effects;
    *effects = current;
    return current;
}


/*
 * Wipes a dead monster from the linked list of delayed range effects.
 */
static void wipe_delayed_ranged_effects(ranged_effects **effects, struct monster *mon)
{
    ranged_effects *current = *effects, *next;

    /* Empty list */
    if (!current) return;

    /* First element */
    if ((*effects)->mon == mon)
    {
        /* Wipe the dead monster */
        next = (*effects)->next;
        mem_free(*effects);
        *effects = next;
        return;
    }

    /* Walk through the list to get the corresponding monster */
    while (current)
    {
        next = current->next;

        /* End of list */
        if (!next) return;

        /* Found a match */
        if (next->mon == mon)
        {
            /* Wipe the dead monster */
            current->next = next->next;
            mem_free(next);
            return;
        }

        current = next;
    }
}


/*
 * Find the attr/char pair to use for a missile.
 *
 * It is moving (or has moved) from (x, y) to (nx, ny).
 */
static void missile_pict(struct player *p, const struct object *obj, int y, int x,
    int ny, int nx, byte *a, char *c)
{
    int arrow_type = (kf_has(obj->kind->kind_flags, KF_AMMO_NORMAL)? PROJ_ARROW_2: PROJ_ARROW_X);
    int bolt_type = (kf_has(obj->kind->kind_flags, KF_AMMO_NORMAL)? PROJ_ARROW_3: PROJ_ARROW_4);

    /* Get a nice missile picture for arrows and bolts */
    if (tval_is_arrow(obj))
        bolt_pict(p, y, x, ny, nx, arrow_type, a, c);
    else if (tval_is_bolt(obj))
        bolt_pict(p, y, x, ny, nx, bolt_type, a, c);
    else
    {
        /* Default to object picture */
        *a = object_attr(p, obj);
        *c = object_char(p, obj);
    }
}


/* Shooting hit types */
static const struct hit_types ranged_hit_types[] =
{
    {MSG_MISS, NULL},
    {MSG_SHOOT_HIT, NULL},
    {MSG_HIT_GOOD, "It was a good hit!"},
    {MSG_HIT_GREAT, "It was a great hit!"},
    {MSG_HIT_SUPERB, "It was a superb hit!"}
};


/*
 * This is a helper function used by do_cmd_throw and do_cmd_fire.
 *
 * It abstracts out the projectile path, display code, identify and clean up
 * logic, while using the 'attack' parameter to do work particular to each
 * kind of attack.
 */
static void ranged_helper(struct player *p, struct object *obj, int dir, int range, int shots,
    ranged_attack attack, const struct hit_types *hit_types, int num_types, bool magic, bool pierce,
    bool ranged_effect)
{
    int i, j;
    char o_name[NORMAL_WID];
    int path_n;
    struct loc path_g[256];

    /* Start at the player */
    int x = p->px;
    int y = p->py;

    /* Predict the "target" location */
    int ty = y + 99 * ddy[dir];
    int tx = x + 99 * ddx[dir];

    bool hit_target = false;
    struct object *missile;
    int num = 0;
    bool dead = false;
    ranged_effects *effects = NULL, *current;
    struct chunk *c = chunk_get(&p->wpos);

    /* Check for target validity */
    if ((dir == 5) && target_okay(p))
    {
        int taim;

        target_get(p, &tx, &ty);

        /* Check distance */
        taim = distance(y, x, ty, tx);
        if (taim > range)
        {
            msg(p, "Target out of range by %d squares.", taim - range);
            return;
        }
    }

    /* Sound */
    sound(p, MSG_SHOOT);

    /* Describe the object */
    object_desc(p, o_name, sizeof(o_name), obj, ODESC_FULL | ODESC_SINGULAR);

    /* Take a turn */
    use_energy(p);

    /* Attack once for each legal shot */
    while (num++ < shots)
    {
        int by = -1, bx = -1;
        struct source who_body;
        struct source *who = &who_body;
        bool none_left = false;

        /* Start at the player */
        y = p->py;
        x = p->px;

        /* Calculate the path */
        path_n = project_path(NULL, path_g, range, c, y, x, ty, tx, (pierce? PROJECT_THRU: 0));

        /* Hack -- handle stuff */
        handle_stuff(p);

        /* Project along the path */
        for (i = 0; i < path_n; ++i)
        {
            int ny = path_g[i].y;
            int nx = path_g[i].x;
            struct missile data;

            /* Hack -- disable throwing through open house door */
            if (square_home_isopendoor(c, ny, nx)) break;

            /* Hack -- stop before hitting walls */
            if (!square_ispassable(c, ny, nx) && !square_isprojectable(c, ny, nx))
            {
                /* Special case: potion VS house door */
                if (tval_is_potion(obj) && square_home_iscloseddoor(c, ny, nx))
                {
                    /* Break it */
                    hit_target = true;

                    /* Find suitable color */
                    colorize_door(p, obj->kind, c, ny, nx);
                }

                /* Done */
                break;
            }

            /* Get missile picture */
            missile_pict(p, obj, y, x, ny, nx, &data.mattr, &data.mchar);

            /* Advance */
            data.x = x = nx;
            data.y = y = ny;

            display_missile(c, &data);

            /* Don't allow if not hostile */
            square_actor(c, y, x, who);
            if (!pvx_check(p, who, c->squares[y][x].feat))
                memset(who, 0, sizeof(struct source));

            /* Try the attack on the target at (x, y) if any */
            if (!source_null(who))
            {
                bool visible;
                bool fear = false;
                char m_name[NORMAL_WID];
                int note_dies = MON_MSG_DIE;
                struct attack_result result = attack(p, obj, y, x);
                int dmg = result.dmg;
                u32b msg_type = result.msg_type;
                const char *verb = result.verb;
                bool mimicking;

                /* Target info */
                if (who->monster)
                {
                    visible = monster_is_visible(p, who->idx);
                    monster_desc(p, m_name, sizeof(m_name), who->monster, MDESC_OBJE);
                    if (monster_is_destroyed(who->monster->race))
                        note_dies = MON_MSG_DESTROYED;
                    mimicking = monster_is_camouflaged(who->monster);
                }
                else
                {
                    visible = player_is_visible(p, who->idx);
                    my_strcpy(m_name, who->player->name, sizeof(m_name));
                    mimicking = who->player->k_idx;
                }

                if (result.success)
                {
                    hit_target = true;

                    missile_learn_on_ranged_attack(p, obj);

                    /* Learn by use for other equipped items */
                    equip_learn_on_ranged_attack(p);

                    /* No negative damage; change verb if no damage done */
                    if (dmg <= 0)
                    {
                        dmg = 0;
                        msg_type = MSG_MISS;
                        verb = "fails to harm";
                    }

                    if (!visible)
                    {
                        /* Invisible monster/player */
                        msgt(p, MSG_SHOOT_HIT, "The %s finds a mark.", o_name);
                    }
                    else
                    {
                        int type;

                        /* Handle visible monster/player */
                        for (type = 0; type < num_types; type++)
                        {
                            const char *dmg_text = "";

                            if (msg_type != hit_types[type].msg_type) continue;
                            if (OPT(p, show_damage)) dmg_text = format(" (%d)", dmg);
                            if (hit_types[type].text)
                            {
                                msgt(p, msg_type, "Your %s %s %s%s. %s", o_name, verb, m_name,
                                    dmg_text, hit_types[type].text);
                            }
                            else
                            {
                                msgt(p, msg_type, "Your %s %s %s%s.", o_name, verb, m_name,
                                    dmg_text);
                            }
                        }

                        /* Track this target */
                        if (who->monster) monster_race_track(p->upkeep, who);
                        health_track(p->upkeep, who);
                    }

                    /* Message */
                    if (who->player)
                    {
                        char killer_name[NORMAL_WID];

                        /* Killer name */
                        player_desc(who->player, killer_name, sizeof(killer_name), p, true);

                        msg(who->player, "%s hits you with a %s.", killer_name, o_name);
                    }

                    /* Hit the target, check for death */
                    if (who->monster)
                    {
                        /* Hit the monster, check for death */
                        dead = mon_take_hit(p, c, who->monster, dmg, &fear, note_dies);
                    }
                    else
                    {
                        char df[160];

                        strnfmt(df, sizeof(df), "was shot to death with a %s by %s", o_name,
                            p->name);

                        /* Hit the player, check for death */
                        dead = take_hit(who->player, dmg, p->name, false, df);
                    }

                    /* Message */
                    if (!dead)
                    {
                        if (who->monster)
                        {
                            current = get_delayed_ranged_effects(&effects, who->monster);
                            current->dmg += dmg;
                        }
                        else
                            player_pain(p, who->player, dmg);
                    }
                    else if (who->monster)
                        wipe_delayed_ranged_effects(&effects, who->monster);

                    /* Apply poison */
                    if (result.effects.do_poison && !dead)
                    {
                        if (who->player)
                        {
                            player_inc_timed(who->player, TMD_POISONED, randint1(p->lev) + 5, true,
                                true);
                        }
                        else if (mon_inc_timed(p, who->monster, MON_TMD_POIS, 5 + randint1(5),
                            MON_TMD_FLG_NOMESSAGE))
                        {
                            current = get_delayed_ranged_effects(&effects, who->monster);
                            current->poison = true;
                        }
                    }

                    /* Apply stun */
                    if (result.effects.do_stun && !dead)
                    {
                        if (who->player)
                        {
                            player_inc_timed(who->player, TMD_STUN, randint1(p->lev) + 5, true,
                                true);
                        }
                        else if (mon_inc_timed(p, who->monster, MON_TMD_STUN, 5 + randint1(5),
                            MON_TMD_FLG_NOMESSAGE))
                        {
                            current = get_delayed_ranged_effects(&effects, who->monster);
                            current->stun = true;
                        }
                    }

                    /* Apply cut */
                    if (result.effects.do_cut && !dead)
                    {
                        if (who->player)
                        {
                            player_inc_timed(who->player, TMD_CUT, randint1(p->lev) + 5, true,
                                true);
                        }
                        else if (mon_inc_timed(p, who->monster, MON_TMD_CUT, 5 + randint1(5),
                            MON_TMD_FLG_NOMESSAGE))
                        {
                            current = get_delayed_ranged_effects(&effects, who->monster);
                            current->cut = true;
                        }
                    }

                    /* Apply archer confusion brand */
                    if (ranged_effect && has_bowbrand(p, PROJ_MON_CONF, false) && !dead)
                    {
                        if (who->player)
                        {
                            player_inc_timed(who->player, TMD_CONFUSED,
                                3 + randint1(10 + randint0(p->lev) / 10), true, true);
                        }
                        else if (mon_inc_timed(p, who->monster, MON_TMD_CONF, 5 + randint1(5),
                            MON_TMD_FLG_NOMESSAGE))
                        {
                            current = get_delayed_ranged_effects(&effects, who->monster);
                            current->conf = true;
                        }
                    }

                    /* Add a nice ball if needed */
                    if (ranged_effect && p->timed[TMD_BOWBRAND] && p->brand.blast)
                    {
                        bx = x;
                        by = y;
                    }

                    /* Take note */
                    if (!dead && fear)
                    {
                        current = get_delayed_ranged_effects(&effects, who->monster);
                        current->fear = true;
                    }
                }
                else if (visible && !mimicking)
                {
                    /* Handle visible monster/player */
                    msgt(p, MSG_MISS, "The %s misses %s.", o_name, m_name);

                    /* Track this target */
                    if (who->monster) monster_race_track(p->upkeep, who);
                    health_track(p->upkeep, who);
                }

                /* Stop the missile */
                if (!pierce) break;
            }

            /* Stop if non-projectable but passable */
            if (!square_isprojectable(c, ny, nx)) break;
        }

        /* Ball effect */
        if ((by >= 0) && (bx >= 0))
        {
            int p_flag = PROJECT_JUMP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | PROJECT_PLAY;
            struct source act_body;
            struct source *p_act = &act_body;

            source_player(p_act, get_player_index(get_connection(p->conn)), p);

            p->current_sound = -2;
            project(p_act, 2, c, by, bx, p->brand.dam, p->brand.type, p_flag, 0, 0, "killed");
            p->current_sound = -1;
        }

        /* Drop (or break) near that location */
        if (!magic)
        {
            /* Get the missile */
            if (object_is_carried(p, obj))
                missile = gear_object_for_use(p, obj, 1, true, &none_left);
            else
                missile = floor_object_for_use(p, c, obj, 1, true, &none_left);

            /* Chance of breakage (during attacks) */
            j = breakage_chance(missile, hit_target);

            /* Handle the newbies_cannot_drop option */
            if (newbies_cannot_drop(p)) j = 100;

            /* Drop (or break) near that location */
            drop_near(p, c, &missile, j, y, x, true, DROP_FADE);
        }

        /* Stop if dead */
        if (dead && !pierce) break;
    }

    /* Hack -- delay messages */
    while (effects)
    {
        /* Paranoia: only process living monsters */
        /* This is necessary to take into account monsters killed by ball effects */
        if (effects->mon->race && monster_is_visible(p, effects->mon->midx))
        {
            if (effects->dmg) message_pain(p, effects->mon, effects->dmg);
            if (effects->poison) add_monster_message(p, effects->mon, MON_MSG_POISONED, true);
            if (effects->cut) add_monster_message(p, effects->mon, MON_MSG_BLEED, true);
            if (effects->stun) add_monster_message(p, effects->mon, MON_MSG_DAZED, true);
            if (effects->conf) add_monster_message(p, effects->mon, MON_MSG_CONFUSED, true);
            if (effects->fear) add_monster_message(p, effects->mon, MON_MSG_FLEE_IN_TERROR, true);
        }

        current = effects->next;
        mem_free(effects);
        effects = current;
    }
}


/*
 * Helper function used with ranged_helper by do_cmd_fire.
 */
static struct attack_result make_ranged_shot(struct player *p, struct object *ammo, int y, int x)
{
    struct attack_result result;
    struct object *bow = equipped_item_by_slot_name(p, "shooting");
    int chance = chance_of_missile_hit(p, ammo, bow, y, x);
    int multiplier = p->state.ammo_mult;
    int best_mult = 1;
    struct chunk *c = chunk_get(&p->wpos);
    struct source target_body;
    struct source *target = &target_body;
    bool visible;
    int ac;

    memset(&result, 0, sizeof(result));
    my_strcpy(result.verb, "hits", sizeof(result.verb));

    /* Target info */
    square_actor(c, y, x, target);
    if (target->monster)
    {
        visible = monster_is_visible(p, target->idx);
        ac = target->monster->ac;
    }
    else
    {
        visible = player_is_visible(p, target->idx);
        ac = target->player->state.ac + target->player->state.to_a;
    }

    /* Did we hit it (penalize distance travelled) */
    if (!test_hit(chance, ac, visible)) return result;

    result.success = true;

    improve_attack_modifier(p, NULL, target, &best_mult, &result.effects, result.verb,
        sizeof(result.verb), true);
    improve_attack_modifier(p, ammo, target, &best_mult, &result.effects, result.verb,
        sizeof(result.verb), true);
    if (bow)
    {
        improve_attack_modifier(p, bow, target, &best_mult, &result.effects, result.verb,
            sizeof(result.verb), true);
    }

    result.dmg = ranged_damage(p, ammo, bow, best_mult, multiplier);
    result.dmg = critical_shot(p, target, ammo->weight, ammo->to_h, result.dmg, &result.msg_type);

    missile_learn_on_ranged_attack(p, bow);

    return result;
}


/*
 * Helper function used with ranged_helper by do_cmd_throw.
 */
static struct attack_result make_ranged_throw(struct player *p, struct object *obj, int y, int x)
{
    struct attack_result result;
    int chance = chance_of_missile_hit(p, obj, NULL, y, x);
    int multiplier = 1;
    int best_mult = 1;
    struct chunk *c = chunk_get(&p->wpos);
    struct source target_body;
    struct source *target = &target_body;
    bool visible;
    int ac;
    s16b to_h;

    memset(&result, 0, sizeof(result));
    my_strcpy(result.verb, "hits", sizeof(result.verb));

    /* Target info */
    square_actor(c, y, x, target);
    if (target->monster)
    {
        visible = monster_is_visible(p, target->idx);
        ac = target->monster->ac;
    }
    else
    {
        visible = player_is_visible(p, target->idx);
        ac = target->player->state.ac + target->player->state.to_a;
    }

    /* If we missed then we're done */
    if (!test_hit(chance, ac, visible)) return result;

    result.success = true;

    improve_attack_modifier(p, NULL, target, &best_mult, &result.effects, result.verb,
        sizeof(result.verb), true);
    improve_attack_modifier(p, obj, target, &best_mult, &result.effects, result.verb,
        sizeof(result.verb), true);

    result.dmg = ranged_damage(p, obj, NULL, best_mult, multiplier);
    object_to_h(obj, &to_h);
    result.dmg = critical_norm(p, target, obj->weight, to_h, result.dmg, &result.msg_type);

    /* Direct adjustment for exploding things (flasks of oil) */
    if (of_has(obj->flags, OF_EXPLODE)) result.dmg *= 3;

    return result;
}


/*
 * Fire an object from the quiver, pack or floor at a target.
 */
void do_cmd_fire(struct player *p, int dir, int item)
{
    int range = MIN(6 + 2 * p->state.ammo_mult, z_info->max_range);
    int shots = p->state.num_shots;
    ranged_attack attack = make_ranged_shot;
    struct object *obj = object_from_index(p, item, true, true);
    bool magic, pierce;

    /* Paranoia: requires an item */
    if (!obj) return;

    /* Restrict ghosts */
    if (p->ghost && !(p->dm_flags & DM_GHOST_HANDS))
    {
        msg(p, "You cannot fire missiles!");
        return;
    }

    /* Check preventive inscription '^f' */
    if (check_prevent_inscription(p, INSCRIPTION_FIRE))
    {
        msg(p, "The item's inscription prevents it.");
        return;
    }

    /* Make sure the player isn't firing wielded items */
    if (object_is_equipped(p->body, obj))
    {
        msg(p, "You cannot fire wielded items.");
        return;
    }

    /* Restricted by choice */
    if (!object_is_carried(p, obj) && !is_owner(p, obj))
    {
        msg(p, "This item belongs to someone else!");
        return;
    }

    /* Paranoia: requires a proper missile */
    if (obj->tval != p->state.ammo_tval) return;

    /* Check preventive inscription '!f' */
    if (object_prevent_inscription(p, obj, INSCRIPTION_FIRE, false))
    {
        msg(p, "The item's inscription prevents it.");
        return;
    }

    /* Restrict artifacts */
    if (obj->artifact && newbies_cannot_drop(p))
    {
        msg(p, "You cannot fire that!");
        return;
    }

    /* Never in wrong house */
    if (!check_store_drop(p, obj))
    {
        msg(p, "You cannot fire this here.");
        return;
    }

    /* Ensure "dir" is in ddx/ddy array bounds */
    if (!VALID_DIR(dir)) return;

    /* Apply confusion */
    player_confuse_dir(p, &dir);

    /* Only fire in direction 5 if we have a target */
    if ((dir == 5) && !target_okay(p)) return;

    magic = of_has(obj->flags, OF_AMMO_MAGIC);
    pierce = has_bowbrand(p, PROJ_ARROW_X, false);

    /* Temporary "Farsight" */
    if (p->timed[TMD_FARSIGHT]) range += (p->lev - 7) / 10;

    /* Check if we have enough missiles */
    if (!magic && (shots > obj->number)) shots = obj->number;

    ranged_helper(p, obj, dir, range, shots, attack, ranged_hit_types,
        (int)N_ELEMENTS(ranged_hit_types), magic, pierce, true);
}


/*
 * Throw an object from the quiver, pack or floor.
 */
void do_cmd_throw(struct player *p, int dir, int item)
{
    int shots = 1;
    int str = adj_str_blow[p->state.stat_ind[STAT_STR]];
    ranged_attack attack = make_ranged_throw;
    int weight;
    int range;
    struct object *obj = object_from_index(p, item, true, true);
    bool magic = false;

    /* Paranoia: requires an item */
    if (!obj) return;

    /* Restrict ghosts */
    if (p->ghost && !(p->dm_flags & DM_GHOST_HANDS))
    {
        msg(p, "You cannot throw items!");
        return;
    }

    /* Check preventive inscription '^v' */
    if (check_prevent_inscription(p, INSCRIPTION_THROW))
    {
        msg(p, "The item's inscription prevents it.");
        return;
    }

    /* Make sure the player isn't throwing wielded items */
    if (object_is_equipped(p->body, obj))
    {
        msg(p, "You cannot throw wielded items.");
        return;
    }

    /* Restricted by choice */
    if (!object_is_carried(p, obj) && !is_owner(p, obj))
    {
        msg(p, "This item belongs to someone else!");
        return;
    }

    /* Check preventive inscription '!v' */
    if (object_prevent_inscription(p, obj, INSCRIPTION_THROW, false))
    {
        msg(p, "The item's inscription prevents it.");
        return;
    }

    if (tval_is_ammo(obj)) magic = of_has(obj->flags, OF_AMMO_MAGIC);

    /* Restrict artifacts */
    if (obj->artifact && (!magic || newbies_cannot_drop(p)))
    {
        msg(p, "You cannot throw that!");
        return;
    }

    /* Never drop deeds of property */
    if (tval_is_deed(obj))
    {
        msg(p, "You cannot throw this.");
        return;
    }

    /* Never in wrong house */
    if (!check_store_drop(p, obj))
    {
        msg(p, "You cannot throw this here.");
        return;
    }

    weight = MAX(obj->weight, 10);
    range = MIN(((str + 20) * 10) / weight, 10);

    /* Apply confusion */
    player_confuse_dir(p, &dir);

    ranged_helper(p, obj, dir, range, shots, attack, melee_hit_types,
        (int)N_ELEMENTS(melee_hit_types), magic, false, false);
}


/*
 * Fire at nearest target
 */
void do_cmd_fire_at_nearest(struct player *p)
{
    /* The direction '5' means 'use the target' */
    int i, dir = 5;
    struct object *ammo = NULL;
    struct object *bow = equipped_item_by_slot_name(p, "shooting");

    /* Require a usable launcher */
    if (!bow && (p->state.ammo_tval != TV_ROCK))
    {
        msg(p, "You have nothing to fire with.");
        return;
    }

    /* Find first eligible ammo in the quiver */
    for (i = 0; i < z_info->quiver_size; i++)
    {
        if (!p->upkeep->quiver[i]) continue;
        if (p->upkeep->quiver[i]->tval != p->state.ammo_tval) continue;
        ammo = p->upkeep->quiver[i];
        break;
    }

    /* Require usable ammo */
    if (!ammo)
    {
        msg(p, "You have no ammunition in the quiver to fire.");
        return;
    }

    /* Require foe */
    if (!target_set_closest(p, TARGET_KILL | TARGET_QUIET)) return;

    /* Fire! */
    do_cmd_fire(p, dir, ammo->oidx);
}
