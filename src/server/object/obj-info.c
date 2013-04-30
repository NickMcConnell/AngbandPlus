/*
 * File: obj-info.c
 * Purpose: Object description code.
 *
 * Copyright (c) 2004 Robert Ruehlmann
 * Copyright (c) 2010 Andi Sidwell
 * Copyright (c) 2012 MAngband and PWMAngband Developers
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


#include "../s-angband.h"
#include "../../common/tvalsval.h"
#include "../attack.h"
#include "../effects.h"
#include "pval.h"
#include "slays.h"
#include "../s-spells.h"


/*
 * Describes a flag-name pair
 */
typedef struct
{
    int flag;
    const char *name;
} flag_type;


/*** Utility code ***/


/*
 * Given an array of strings, as so:
 *  { "intelligence", "fish", "lens", "prime", "number" },
 *
 * ... output a list like "intelligence, fish, lens, prime, number.\n".
 */
static void info_out_list(struct player *p, const char *list[], size_t count)
{
    size_t i;

    for (i = 0; i < count; i++)
    {
        text_out(p, list[i]);
        if (i != (count - 1)) text_out(p, ", ");
    }

    text_out(p, ".\n");
}


static size_t info_collect(const flag_type list[], size_t max,
    const bitflag flags[OF_SIZE], const char *recepticle[])
{
    size_t i, count = 0;

    for (i = 0; i < max; i++)
    {
        if (of_has(flags, list[i].flag)) recepticle[count++] = list[i].name;
    }

    return count;
}


/*** Big fat data tables ***/


static const flag_type pval_flags[] =
{
    {OF_STR, "strength"},
    {OF_INT, "intelligence"},
    {OF_WIS, "wisdom"},
    {OF_DEX, "dexterity"},
    {OF_CON, "constitution"},
    {OF_CHR, "charisma"},
    {OF_MANA, "mana capacity"},
    {OF_STEALTH, "stealth"},
    {OF_INFRA, "infravision"},
    {OF_TUNNEL, "tunneling"},
    {OF_SPEED, "speed"},
    {OF_BLOWS, "attack speed"},
    {OF_SHOTS, "shooting speed"},
    {OF_MIGHT, "shooting power"}
};


static const flag_type immunity_flags[] =
{
    {OF_IM_ACID, "acid"},
    {OF_IM_ELEC, "lightning"},
    {OF_IM_FIRE, "fire"},
    {OF_IM_COLD, "cold"}
};


static const flag_type vuln_flags[] =
{
    {OF_VULN_ACID, "acid"},
    {OF_VULN_ELEC, "lightning"},
    {OF_VULN_FIRE, "fire"},
    {OF_VULN_COLD, "cold"}
};


static const flag_type resist_flags[] =
{
    {OF_RES_ACID, "acid"},
    {OF_RES_ELEC, "lightning"},
    {OF_RES_FIRE, "fire"},
    {OF_RES_COLD, "cold"},
    {OF_RES_POIS, "poison"},
    {OF_RES_LIGHT, "light"},
    {OF_RES_DARK, "dark"},
    {OF_RES_SOUND, "sound"},
    {OF_RES_SHARD, "shards"},
    {OF_RES_NEXUS, "nexus"},
    {OF_RES_NETHR, "nether"},
    {OF_RES_CHAOS, "chaos"},
    {OF_RES_DISEN, "disenchantment"},
    {OF_RES_TIME, "time"},
    {OF_RES_MANA, "mana"}
};


static const flag_type protect_flags[] =
{
    {OF_RES_FEAR, "fear"},
    {OF_RES_BLIND, "blindness"},
    {OF_RES_CONFU, "confusion"},
    {OF_RES_STUN, "stunning"}
};


static const flag_type ignore_flags[] =
{
    {OF_IGNORE_ACID, "acid"},
    {OF_IGNORE_ELEC, "lightning"},
    {OF_IGNORE_FIRE, "fire"},
    {OF_IGNORE_COLD, "cold"}
};


static const flag_type hates_flags[] =
{
    {OF_HATES_ACID, "acid"},
    {OF_HATES_ELEC, "lightning"},
    {OF_HATES_FIRE, "fire"},
    {OF_HATES_COLD, "cold"}
};


static const flag_type sustain_flags[] =
{
    {OF_SUST_STR, "strength"},
    {OF_SUST_INT, "intelligence"},
    {OF_SUST_WIS, "wisdom"},
    {OF_SUST_DEX, "dexterity"},
    {OF_SUST_CON, "constitution"},
    {OF_SUST_CHR, "charisma"}
};


static const flag_type misc_flags[] =
{
    {OF_BLESSED, "Blessed by the gods"},
    {OF_SLOW_DIGEST, "Slows your metabolism"},
    {OF_IMPAIR_HP, "Impairs hitpoint recovery"},
    {OF_IMPAIR_MANA, "Impairs mana recovery"},
    {OF_AFRAID, "Makes you afraid of melee, and worse at shooting and casting spells"},
    {OF_FEATHER, "Makes you fall like a feather"},
    {OF_REGEN, "Speeds regeneration"},
    {OF_FREE_ACT, "Prevents paralysis"},
    {OF_HOLD_LIFE, "Sustains your life force"},
    {OF_SEE_INVIS, "Grants the ability to see invisible things"},
    {OF_KNOWLEDGE, "Identifies all items for you"},
    {OF_ANTI_MAGIC, "Creates an anti-magic field"},
    {OF_AGGRAVATE, "Aggravates nearby creatures"},
    {OF_DRAIN_EXP, "Drains experience"},
    {OF_TELEPORT, "Induces random teleportation"}
};


static const flag_type esp_flags[] =
{
    {OF_ESP_ALL, "Grants telepathy"},
    {OF_ESP_ANIMAL, "Grants the ability to sense animals"},
    {OF_ESP_EVIL, "Grants the ability to sense evil"},
    {OF_ESP_UNDEAD, "Grants the ability to sense undead"},
    {OF_ESP_DEMON, "Grants the ability to sense demons"},
    {OF_ESP_ORC, "Grants the ability to sense orcs"},
    {OF_ESP_TROLL, "Grants the ability to sense trolls"},
    {OF_ESP_GIANT, "Grants the ability to sense giants"},
    {OF_ESP_DRAGON, "Grants the ability to sense dragons"},
    {OF_ESP_RADIUS, "Grants telepathic awareness"}
};


/*** Code that makes use of the data tables ***/


/*
 * Describe an item's curses.
 */
static bool describe_curses(struct player *p, const object_type *o_ptr,
    const bitflag flags[OF_SIZE])
{
    if (of_has(flags, OF_PERMA_CURSE))
        text_out_c(p, TERM_L_RED, "Permanently cursed.\n");
    else if (of_has(flags, OF_HEAVY_CURSE))
        text_out_c(p, TERM_L_RED, "Heavily cursed.\n");
    else if (of_has(flags, OF_LIGHT_CURSE))
        text_out_c(p, TERM_L_RED, "Cursed.\n");
    else
        return FALSE;

    return TRUE;
}


/*
 * Describe stat modifications.
 */
static bool describe_stats(struct player *p, const object_type *o_ptr,
    const bitflag flags[MAX_PVALS][OF_SIZE], oinfo_detail_t mode)
{
    const char *descs[N_ELEMENTS(pval_flags)];
    size_t count, i;
    bool full = mode & OINFO_FULL;
    bool search = FALSE;
    s32b src = 0L;
    bool aware = object_flavor_is_aware(p, o_ptr);

    if (!o_ptr->num_pvals) return FALSE;

    for (i = 0; i < o_ptr->num_pvals; i++)
    {
        count = info_collect(pval_flags, N_ELEMENTS(pval_flags), flags[i], descs);

        if (count)
        {
            if (object_this_pval_is_visible(o_ptr, i, aware) || full)
                text_out_c(p, ((o_ptr->pval[i] > 0)? TERM_L_GREEN: TERM_RED), "%+i ",
                    o_ptr->pval[i]);
            else
                text_out(p, "Affects your ");
            info_out_list(p, descs, count);
        }
        if (of_has(flags[i], OF_SEARCH))
        {
            search = TRUE;
            if (object_this_pval_is_visible(o_ptr, i, aware) || full)
                src += o_ptr->pval[i];
        }
    }

    /* Special case: searching */
    if (search)
    {
        if (src)
        {
            text_out_c(p, ((src > 0)? TERM_L_GREEN: TERM_RED), "%+i%% ", src * 5);
            text_out(p, "to searching.\n");
        }
        else if (count)
            text_out(p, "Also affects your searching skill.\n");
        else
            text_out(p, "Affects your searching skill.\n");
    }

    return TRUE;
}


/*
 * Describe immunities granted by an object.
 */
static bool describe_immune(struct player *p, const bitflag flags[OF_SIZE])
{
    const char *i_descs[N_ELEMENTS(immunity_flags)];
    const char *r_descs[N_ELEMENTS(resist_flags)];
    const char *p_descs[N_ELEMENTS(protect_flags)];
    const char *v_descs[N_ELEMENTS(vuln_flags)];
    size_t count;
    bool prev = FALSE;

    /* Immunities */
    count = info_collect(immunity_flags, N_ELEMENTS(immunity_flags), flags, i_descs);
    if (count)
    {
        text_out(p, "Provides immunity to ");
        info_out_list(p, i_descs, count);
        prev = TRUE;
    }

    /* Resistances */
    count = info_collect(resist_flags, N_ELEMENTS(resist_flags), flags, r_descs);
    if (count)
    {
        text_out(p, "Provides resistance to ");
        info_out_list(p, r_descs, count);
        prev = TRUE;
    }

    /* Protections */
    count = info_collect(protect_flags, N_ELEMENTS(protect_flags), flags, p_descs);
    if (count)
    {
        text_out(p, "Provides protection from ");
        info_out_list(p, p_descs, count);
        prev = TRUE;
    }

    /* Vulnerabilities */
    count = info_collect(vuln_flags, N_ELEMENTS(vuln_flags), flags, v_descs);
    if (count)
    {
        text_out(p, "Makes you vulnerable to ");
        info_out_list(p, v_descs, count);
        prev = TRUE;
    }

    return prev;
}


/*
 * Describe IGNORE_ flags of an object.
 */
static bool describe_ignores(struct player *p, const bitflag flags[OF_SIZE])
{
    const char *descs[N_ELEMENTS(ignore_flags)];
    size_t count = info_collect(ignore_flags, N_ELEMENTS(ignore_flags), flags, descs);

    if (!count) return FALSE;

    text_out(p, "Cannot be harmed by ");
    info_out_list(p, descs, count);

    return TRUE;
}


/*
 * Describe HATES_ flags of an object.
 */
static bool describe_hates(struct player *p, const bitflag flags[OF_SIZE])
{
    const char *descs[N_ELEMENTS(hates_flags)];
    size_t count = info_collect(hates_flags, N_ELEMENTS(hates_flags), flags, descs);

    if (!count) return FALSE;

    text_out(p, "Can be destroyed by ");
    info_out_list(p, descs, count);

    return TRUE;
}


/*
 * Describe stat sustains.
 */
static bool describe_sustains(struct player *p, const bitflag flags[OF_SIZE])
{
    const char *descs[N_ELEMENTS(sustain_flags)];
    size_t count = info_collect(sustain_flags, N_ELEMENTS(sustain_flags), flags, descs);

    if (!count) return FALSE;

    text_out(p, "Sustains ");
    info_out_list(p, descs, count);

    return TRUE;
}


/*
 * Describe miscellaneous powers.
 */
static bool describe_misc_magic(struct player *p, const bitflag flags[OF_SIZE])
{
    size_t i;
    bool printed = FALSE;

    for (i = 0; i < N_ELEMENTS(misc_flags); i++)
    {
        if (of_has(flags, misc_flags[i].flag))
        {
            text_out(p, "%s. ", misc_flags[i].name);
            printed = TRUE;
        }
    }

    if (printed) text_out(p, "\n");

    return printed;
}


/*
 * Describe slays and brands granted by an object.
 */
static bool describe_slays(struct player *p, const bitflag flags[OF_SIZE], bool fulldesc)
{
    bool printed = FALSE;
    const char *slay_descs[SL_MAX] = { 0 };
    bitflag slay_mask[OF_SIZE], kill_mask[OF_SIZE], brand_mask[OF_SIZE];
    size_t count;

    create_mask(slay_mask, FALSE, OFT_SLAY, OFT_MAX);
    create_mask(kill_mask, FALSE, OFT_KILL, OFT_MAX);
    create_mask(brand_mask, FALSE, OFT_BRAND, OFT_MAX);

    /* Slays */
    count = list_slays(flags, slay_mask, slay_descs, NULL, NULL, TRUE);
    if (count)
    {
        if (fulldesc) text_out(p, "It causes your melee attacks to slay ");
        else text_out(p, "Slays ");
        info_out_list(p, slay_descs, count);
        printed = TRUE;
    }

    /* Kills */
    count = list_slays(flags, kill_mask, slay_descs, NULL, NULL, TRUE);
    if (count)
    {
        if (fulldesc) text_out(p, "It causes your melee attacks to *slay* ");
        else text_out(p, "*Slays* ");
        info_out_list(p, slay_descs, count);
        printed = TRUE;
    }

    /* Brands */
    count = list_slays(flags, brand_mask, NULL, slay_descs, NULL, TRUE);
    if (count)
    {
        if (fulldesc) text_out(p, "It brands your melee attacks with ");
        else text_out(p, "Branded with ");
        info_out_list(p, slay_descs, count);
        printed = TRUE;
    }

    return printed;
}


/*
 * Describe monster ESP.
 */
static bool describe_esp(struct player *p, const bitflag flags[OF_SIZE])
{
    size_t i;
    bool printed = FALSE;

    for (i = 0; i < N_ELEMENTS(esp_flags); i++)
    {
        if (of_has(flags, esp_flags[i].flag))
        {
            text_out(p, "%s.\n", esp_flags[i].name);
            printed = TRUE;
        }
    }

    return printed;
}


/*
 * Account for criticals in the calculation of melee prowess.
 */
static int calculate_melee_crits(struct player *p, player_state *state, int weight, int plus,
    int dam)
{
    int k, to_crit = weight + 5 * (state->to_h + plus) + 3 * p->lev;
    int crit_dam = 0;

    to_crit = MIN(5000, MAX(0, to_crit));

    /* Extract average critical damage */
    for (k = weight + 1; k <= weight + 650; k++)
    {
        if (k < 400) crit_dam += dam * 2 + 5;
        else if (k < 700) crit_dam += dam * 2 + 10;
        else if (k < 900) crit_dam += dam * 3 + 15;
        else if (k < 1300) crit_dam += dam * 3 + 20;
        else crit_dam += dam * 7 / 2 + 25;
    }
    crit_dam /= 650;

    /* Chance */
    crit_dam = (crit_dam * to_crit + dam * (5000 - to_crit)) / 5000;

    /* Apply Touch of Death */
    if (p->timed[TMD_DEADLY])
        crit_dam = (crit_dam * 3 + (dam * 7 / 2 + 30)) / 4;

    return crit_dam;
}


/*
 * Missile crits follow the same approach as melee crits.
 */
static int calculate_missile_crits(struct player *p, player_state *state, int weight, int plus,
    int dam)
{
    int k, to_crit = weight + 4 * (state->to_h + plus) + 2 * p->lev;
    int crit_dam = 0;

    to_crit = MIN(5000, MAX(0, to_crit));

    /* Extract average critical damage */
    for (k = weight + 1; k <= weight + 500; k++)
    {
        if (k < 500) crit_dam += dam * 2 + 5;
        else if (k < 1000) crit_dam += dam * 2 + 10;
        else crit_dam += dam * 3 + 15;
    }
    crit_dam /= 500;

    /* Chance */
    crit_dam = (crit_dam * to_crit + dam * (5000 - to_crit)) / 5000;

    return crit_dam;
}


/*
 * Describe blows.
 */
static void describe_blows(struct player *p, object_type inven[INVEN_TOTAL], player_state state)
{
    int str_plus, dex_plus, old_blows, new_blows;
    int str_faster = -1, str_done = -1;
    int dex_plus_bound;
    int str_plus_bound;

    dex_plus_bound = STAT_RANGE - state.stat_ind[A_DEX];
    str_plus_bound = STAT_RANGE - state.stat_ind[A_STR];

    text_out_c(p, TERM_L_GREEN, "%d.%d ", state.num_blows / 100, ((state.num_blows / 10) % 10));
    text_out(p, "blow%s/round.\n", ((state.num_blows > 100)? "s": ""));

    /* Check to see if extra STR or DEX would yield extra blows */
    for (dex_plus = 0; dex_plus < dex_plus_bound; dex_plus++)
    {
        for (str_plus = 0; str_plus < str_plus_bound; str_plus++)
        {
            player_state tmpstate;

            WIPE(&tmpstate, player_state);

            tmpstate.stat_add[A_STR] = str_plus;
            tmpstate.stat_add[A_DEX] = dex_plus;

            calc_bonuses(p, inven, &tmpstate, TRUE);

            old_blows = state.num_blows;
            new_blows = tmpstate.num_blows;

            /* Test to make sure that this extra blow is a new str/dex combination, not a repeat */
            if (((new_blows - new_blows % 10) > (old_blows - old_blows % 10)) &&
                ((str_plus < str_done) || (str_done == -1)))
            {
                text_out(p, "With +%d STR and +%d DEX you would get ", str_plus, dex_plus);
                text_out(p, "%d.%d ", new_blows / 100, (new_blows / 10) % 10);
                text_out(p, "blow%s/round.\n", ((new_blows > 100)? "s": ""));
                str_done = str_plus;
                break;
            }

            /*
             * If the combination doesn't increment the displayed blows number, it might still
             * take a little less energy
             */
            if ((new_blows > old_blows) && ((str_plus < str_faster) || (str_faster == -1)) &&
                ((str_plus < str_done) || (str_done == -1)))
            {
                text_out(p, "With +%d STR and +%d DEX you would attack a bit faster.\n", str_plus,
                    dex_plus);
                str_faster = str_plus;
                continue;
            }
        }
    }
}


static void display_damage(struct player *p, const object_type *o_ptr, const object_type *j_ptr,
    bool weapon, bool ammo, int mult, player_state *pstate, bool full)
{
    int dice, sides, dam, plus = 0;
    int multiplier = 1;

    /* Use displayed dice if real dice not known */
    if (full || object_attack_plusses_are_visible(p, o_ptr))
    {
        dice = o_ptr->dd;
        sides = o_ptr->ds;
    }
    else
    {
        dice = o_ptr->kind->dd;
        sides = o_ptr->kind->ds;
    }

    /* Calculate damage */
    dam = ((sides + 1) * dice * 5);

    /* Apply melee slays & brands */
    if (weapon) dam *= mult;

    /* Add object to-dam (x10) */
    if (object_attack_plusses_are_visible(p, o_ptr) || full)
        dam += o_ptr->to_d * 10;

    /* Apply melee critical hits */
    if (weapon)
    {
        if (object_attack_plusses_are_visible(p, o_ptr) || full)
            plus = o_ptr->to_h;
        dam = calculate_melee_crits(p, pstate, o_ptr->weight, plus, dam);
    }

    /* Add player to-dam (x10) for melee weapons */
    if (weapon) dam += pstate->dis_to_d * 10;

    /* Add shooter to-dam (x10) for missile weapons */
    if (ammo && j_ptr->kind && object_attack_plusses_are_visible(p, j_ptr))
        dam += j_ptr->to_d * 10;

    /* Calculate missile multiplier from launcher multiplier, slays & brands */
    if (ammo)
    {
        multiplier = p->state.ammo_mult;
        if (mult > 1)
        {
            if (multiplier > 1) multiplier += mult;
            else multiplier = mult;
        }
    }

    /* Apply missile multiplier */
    if (ammo) dam *= multiplier;

    /* Apply missile to-dam from temp branding (x10) */
    if (ammo && p->timed[TMD_BOWBRAND]) dam += p->bow_brand_d * 10;

    /* Apply missile critical hits */
    if (ammo)
    {
        if (object_attack_plusses_are_visible(p, o_ptr) || full)
            plus = o_ptr->to_h;
        dam = calculate_missile_crits(p, &p->state, o_ptr->weight, plus, dam);
    }

    /* Don't show negative damage */
    if (dam < 0) dam = 0;

    /* Apply number of blows/shots per round */
    if (weapon) dam = (dam * pstate->num_blows) / 100;
    else dam *= p->state.num_shots;

    /* Display the damage */
    if (!dam) text_out_c(p, TERM_L_RED, "0");
    else if (dam % 10)
        text_out_c(p, TERM_L_GREEN, "%d.%d", dam / 10, dam % 10);
    else
        text_out_c(p, TERM_L_GREEN, "%d", dam / 10);
}


/*
 * Describe damage.
 */
static void describe_damage(struct player *p, const object_type *o_ptr, player_state state,
    bitflag f[OF_SIZE], oinfo_detail_t mode)
{
    const char *desc[SL_MAX] = { 0 };
    int i;
    int mult[SL_MAX];
    int cnt;
    object_type *bow = &p->inventory[INVEN_BOW];
    bitflag tmp_f[OF_SIZE], mask[OF_SIZE];
    bool weapon = (wield_slot(p, o_ptr) == INVEN_WIELD);
    bool ammo = (p->state.ammo_tval == o_ptr->tval);

    /* Create the "all slays" mask */
    create_mask(mask, FALSE, OFT_SLAY, OFT_KILL, OFT_BRAND, OFT_MAX);

    /*
     * Apply brands/slays from the shooter to the ammo, but only if known.
     * Note that this is not dependent on mode, so that viewing shop-held
     * ammo (fully known) does not leak information about launcher
     */
    if (ammo && bow->kind)
    {
        object_flags_known(bow, tmp_f, object_flavor_is_aware(p, bow));
        of_union(f, tmp_f);
    }

    /* Collect slays */
    /* Melee weapons get slays and brands from other items now */
    if (weapon)
    {
        bool nonweap_slay = FALSE;

        for (i = INVEN_LEFT; i < INVEN_TOTAL; i++)
        {
            if (!p->inventory[i].kind) continue;

            object_flags_known(&p->inventory[i], tmp_f, object_flavor_is_aware(p, &p->inventory[i]));

            /* Strip out non-slays */
            of_inter(tmp_f, mask);

            if (of_union(f, tmp_f)) nonweap_slay = TRUE;
        }

        /* Apply slays and brands to the weapon */
        if (nonweap_slay)
            text_out(p, "This weapon may benefit from one or more off-weapon brands or slays.\n");
    }

    /* Handle polymorphed players */
    if (weapon && p->r_idx)
    {
        for (i = 0; i < MONSTER_BLOW_MAX; i++)
            apply_poly_brand(p->r_idx, i, f, tmp_f);
    }

    /* Hack -- Extract temp branding */
    if (ammo && p->timed[TMD_BOWBRAND])
        apply_bow_brand(p->bow_brand_t, f, tmp_f);

    /* Hack -- Extract temp branding */
    if (weapon && p->timed[TMD_SGRASP]) of_on(f, OF_BRAND_ELEC);

    text_out(p, "Average damage/round: ");

    /* Output damage for creatures effected by the brands or slays */
    cnt = list_slays(f, mask, desc, NULL, mult, TRUE);
    for (i = 0; i < cnt; i++)
    {
        /* Display damage */
        display_damage(p, o_ptr, bow, weapon, ammo, mult[i], &state, mode & OINFO_FULL);
        text_out(p, " vs. %s, ", desc[i]);
    }

    if (cnt) text_out(p, "and ");

    /* Normal damage, not considering brands or slays */
    display_damage(p, o_ptr, bow, weapon, ammo, 1, &state, mode & OINFO_FULL);
    if (cnt) text_out(p, " vs. others");
    text_out(p, ".\n");
}


/*
 * Describe combat advantages
 */
static bool describe_combat(struct player *p, const object_type *o_ptr, oinfo_detail_t mode)
{
    bool full = mode & OINFO_FULL;
    bitflag f[OF_SIZE];
    bool weapon = (wield_slot(p, o_ptr) == INVEN_WIELD);
    bool ammo = (p->state.ammo_tval == o_ptr->tval);

    /* The player's hypothetical state, were they to wield this item */
    player_state state;

    /* Abort if we've nothing to say */
    if (!weapon && !ammo)
    {
        /* Potions can have special text */
        if ((o_ptr->tval != TV_POTION) || !o_ptr->dd || !o_ptr->ds ||
            !object_flavor_is_aware(p, o_ptr))
        {
            return FALSE;
        }

        text_out(p, "It can be thrown at creatures with damaging effect.\n");
        return TRUE;
    }

    if (full) object_flags(o_ptr, f);
    else object_flags_known(o_ptr, f, object_flavor_is_aware(p, o_ptr));

    text_out_c(p, TERM_L_WHITE, "Combat info:\n");

    /* Weapon: blows/round */
    if (weapon)
    {
        object_type inven[INVEN_TOTAL];

        C_COPY(inven, p->inventory, INVEN_TOTAL, object_type);
        inven[INVEN_WIELD] = *o_ptr;

        if (full) object_know_all_flags(&inven[INVEN_WIELD]);

        /* Calculate the player's hypothetical state */
        WIPE(&state, player_state);
        calc_bonuses(p, inven, &state, TRUE);

        /* Special -- Two-handed weapons */
        if (of_has(f, OF_TWO_HANDED))
            text_out_c(p, TERM_L_RED, "This weapon should be wielded with both hands.\n");

        /* Warn about heavy weapons */
        if (state.heavy_wield)
            text_out_c(p, TERM_L_RED, "You are too weak to use this weapon.\n");

        /* Describe blows */
        describe_blows(p, inven, state);
    }

    /* Missile: shots/round and range */
    else
    {
        /* Range of the weapon */
        int tdis = 6 + 2 * p->state.ammo_mult;

        /* Temporary "Farsight" */
        if (p->timed[TMD_FARSIGHT]) tdis += (p->lev - 7) / 10;

        text_out_c(p, TERM_L_GREEN, "%d ", p->state.num_shots);
        text_out(p, "shot%s/round.\n", PLURAL(p->state.num_shots));
        text_out(p, "Hits targets up to ");
        text_out_c(p, TERM_L_GREEN, "%d", tdis * 10);
        text_out(p, " feet away.\n");
    }

    /* Describe damage */
    describe_damage(p, o_ptr, state, f, mode);

    /* Note the impact flag */
    if (of_has(f, OF_IMPACT))
        text_out(p, "Sometimes creates earthquakes on impact.\n");

    /* Add breakage chance */
    if (ammo && !magic_ammo_p(o_ptr) && !o_ptr->artifact)
    {
        text_out_c(p, TERM_L_GREEN, "%d%%", breakage_chance(o_ptr, TRUE));
        text_out(p, " chance of breaking upon contact.\n");
    }

    /* Something has been said */
    return TRUE;
}


/*
 * Describe objects that can be used for digging
 */
static bool describe_digger(struct player *p, const object_type *o_ptr, oinfo_detail_t mode)
{
    bool full = mode & OINFO_FULL;
    player_state st;
    object_type inven[INVEN_TOTAL];
    int sl = wield_slot(p, o_ptr);
    int i;
    bitflag f[MAX_PVALS][OF_SIZE];
    int chances[5];
    static const char *names[5] =
    {
        "vegetation", "rubble", "magma veins", "quartz veins", "granite"
    };
    bool has_tunnel = FALSE;

    if (full) object_pval_flags(o_ptr, f);
    else object_pval_flags_known(o_ptr, f, object_flavor_is_aware(p, o_ptr));

    for (i = 0; i < o_ptr->num_pvals; i++)
    {
        if (of_has(f[i], OF_TUNNEL)) has_tunnel = TRUE;
    }

    /* Abort if we've nothing to say */
    if ((sl < 0) || !has_tunnel) return FALSE;

    /* Get the player's hypothetical state, were they to be using this item */
    C_COPY(inven, p->inventory, INVEN_TOTAL, object_type);

    /*
     * Hack -- if we examine a ring that is worn on the right finger,
     * we shouldn't put a copy of it on the left finger before calculating
     * digging skills.
     */
    if (o_ptr != &p->inventory[INVEN_RIGHT]) inven[sl] = *o_ptr;

    WIPE(&st, player_state);
    calc_bonuses(p, inven, &st, TRUE);

    chances[0] = (st.skills[SKILL_DIGGING] + wielding_cut(p) * 10) * 4;
    chances[1] = st.skills[SKILL_DIGGING] * 8;
    chances[2] = (st.skills[SKILL_DIGGING] - 10) * 4;
    chances[3] = (st.skills[SKILL_DIGGING] - 20) * 2;
    chances[4] = st.skills[SKILL_DIGGING] - 40;

    for (i = 0; i < 5; i++)
    {
        int chance = MAX(0, MIN(1600, chances[i]));
        int decis = (chance? (16000 / chance): 0);

        if ((i == 0) && (chance > 0))
        {
            if (sl == INVEN_WIELD) text_out(p, "Clears ");
            else text_out(p, "With this item, your current weapon clears ");
        }
        if ((i == 4) || ((i != 0) && (chance == 0))) text_out(p, "and ");
        if (chance == 0)
        {
            text_out_c(p, TERM_L_RED, "doesn't affect ");
            text_out(p, "%s.\n", names[i]);
            break;
        }
        text_out(p, "%s in ", names[i]);
        if (chance == 1600) text_out_c(p, TERM_L_GREEN, "1 ");
        else if (decis < 100)
            text_out_c(p, TERM_GREEN, "%d.%d ", decis / 10, decis % 10);
        else
            text_out_c(p, ((decis < 1000)? TERM_YELLOW: TERM_RED), "%d ", (decis + 5) / 10);
        text_out(p, "turn%s%s", ((decis == 10)? "": "s"), ((i == 4)? ".\n": ", "));
    }

    /* You always have something to say... */
    return TRUE;
}


static int adjust_depth(struct player *p, int value)
{
    double adj_value;

    /* Correct for player depth */
    adj_value = (double)value * level_speed(p->depth) / 50000;
    return (int)adj_value;
}


/*
 * Describe boring bits.
 */
static bool describe_food(struct player *p, const object_type *o_ptr, bool full)
{
    /* Describe boring bits */
    if ((is_food(o_ptr) || (o_ptr->tval == TV_POTION)) && o_ptr->pval[DEFAULT_PVAL])
    {
        if (object_is_known(p, o_ptr) || full)
        {
            /* Correct for player depth */
            text_out(p, "Nourishes for around ");
            text_out_c(p, TERM_L_GREEN, "%d", adjust_depth(p, o_ptr->pval[DEFAULT_PVAL] / 2));
            text_out(p, " turns at your current depth.\n");
        }
        else
            text_out(p, "Provides some nourishment.\n");

        return TRUE;
    }

    return FALSE;
}


static int base_light(struct object_kind *k)
{
    int i;

    for (i = 0; i < k->num_pvals; i++)
    {
        if (of_has(k->pval_flags[i], OF_LIGHT)) return randcalc(k->pval[i], 0, MINIMISE);
    }

    return 0;
}


static int art_light(struct artifact *a)
{
    int i;

    for (i = 0; i < a->num_pvals; i++)
    {
        if (of_has(a->pval_flags[i], OF_LIGHT)) return a->pval[i];
    }

    return 0;
}


/*
 * Describe things that look like lights.
 */
static bool describe_light(struct player *p, const object_type *o_ptr, const bitflag flags[OF_SIZE])
{
    int rad = 0;
    bool artifact = (o_ptr->artifact? TRUE: FALSE);
    bool no_fuel = of_has(flags, OF_NO_FUEL);
    bool is_light = (o_ptr->tval == TV_LIGHT)? TRUE: FALSE;
    const char *intro, *outro = NULL;

    if (!is_light && !of_has(flags, OF_LIGHT)) return FALSE;

    /* Work out radius */
    if (!of_has(flags, OF_LIGHT))
    {
        /* Unidentified lights will give the base radius */
        intro = "Base radius ";
        if (artifact)
        {
            /* Use base radius of static artifact */
            rad = art_light(o_ptr->artifact);
        }
        else
        {
            /* Use base radius of kind */
            rad = base_light(o_ptr->kind);
            outro = " (when fueled)";
        }
    }
    else
    {
        /* Identified items will give the real radius */
        intro = "Radius ";
        rad = o_ptr->pval[which_pval(o_ptr, OF_LIGHT)];
        if (is_light && !no_fuel) outro = " (when fueled)";
    }

    /* Describe here */
    text_out(p, intro);
    text_out_c(p, TERM_L_GREEN, "%d", rad);
    text_out(p, " light");
    if (outro) text_out(p, outro);
    text_out(p, ".");

    /* Lamps can refill other lamps */
    if (is_light && is_lamp(o_ptr) && !no_fuel)
        text_out(p, " Refills other lamps, up to %d turns of fuel.", FUEL_LAMP);

    text_out(p, "\n");

    return TRUE;
}


static void describe_effect(struct player *p, const char *d)
{
    char desc[MSG_LEN];
    char *t;
    bool colored = FALSE;

    /* Print a colourised description */
    my_strcpy(desc, d, sizeof(desc));
    t = strtok(desc, "{}");
    while (t)
    {
        if (colored) text_out_c(p, TERM_L_GREEN, t);
        else text_out(p, t);
        colored = !colored;
        t = strtok(NULL, "{}");
    }
}


static void describe_turns(struct player *p, random_value v)
{
    int min_time, max_time;

    if (!randcalc(v, 0, MAXIMISE)) return;

    text_out(p, " every ");

    /* Correct for player depth */
    min_time = adjust_depth(p, randcalc(v, 0, MINIMISE));
    max_time = adjust_depth(p, randcalc(v, 0, MAXIMISE));

    text_out_c(p, TERM_L_GREEN, "%d", min_time);

    if (min_time != max_time)
    {
        text_out(p, " to ");
        text_out_c(p, TERM_L_GREEN, "%d", max_time);
    }

    text_out(p, " turns at your current depth");
}


/*
 * Describe an object's activation, if any.
 */
static bool describe_activation(struct player *p, const object_type *o_ptr, bool full)
{
    const char *desc;
    int effect = 0, fail;

    /* Get activation */
    if (object_activation(p, o_ptr, full, &effect))
    {
        if (effect_aim(effect))
            text_out(p, "It can be aimed.\n");
        else if (is_food(o_ptr))
            text_out(p, "It can be eaten.\n");
        else if (o_ptr->tval == TV_POTION)
            text_out(p, "It can be drunk.\n");
        else if (o_ptr->tval == TV_SCROLL)
            text_out(p, "It can be read.\n");
        else
            text_out(p, "It can be activated.\n");
        return TRUE;
    }

    /* Forget it without an effect */
    if (!effect) return FALSE;

    /* Obtain the descriptions */
    desc = effect_desc(effect);
    if (!desc) return FALSE;

    text_out(p, "When ");

    if (effect_aim(effect))
        text_out(p, "aimed");
    else if (is_food(o_ptr))
        text_out(p, "eaten");
    else if (o_ptr->tval == TV_POTION)
        text_out(p, "drunk");
    else if (o_ptr->tval == TV_SCROLL)
        text_out(p, "read");
    else
        text_out(p, "activated");

    text_out(p, ", it ");

    /* Print a colourised description */
    describe_effect(p, desc);

    describe_turns(p, o_ptr->time);

    text_out(p, ".\n");

    if (full && !is_food(o_ptr) && (o_ptr->tval != TV_POTION) &&
        (o_ptr->tval != TV_SCROLL))
    {
        fail = get_use_device_chance(p, o_ptr);
        text_out(p, "Your chance of success is %d.%d%%\n", (1000 - fail) / 10,
            (1000 - fail) % 10);
    }

    return TRUE;
}


static void describe_origin(struct player *p, const object_type *o_ptr)
{
    char origin_text[NORMAL_WID];

    if (o_ptr->origin_depth)
        strnfmt(origin_text, sizeof(origin_text), "%d feet (level %d)",
            o_ptr->origin_depth * 50, o_ptr->origin_depth);
    else
        my_strcpy(origin_text, "town", sizeof(origin_text));

    /* Display the origin */
    switch (o_ptr->origin)
    {
        case ORIGIN_NONE:
        case ORIGIN_MIXED:
        case ORIGIN_STOLEN:
            break;

        case ORIGIN_BIRTH:
        {
            text_out(p, "An inheritance from your family.\n");
            break;
        }

        case ORIGIN_STORE:
        {
            text_out(p, "Purchased from a store %s %s.\n",
                (o_ptr->origin_depth? "at": "in"), origin_text);
            break;
        }

        case ORIGIN_FLOOR:
        {
            text_out(p, "Found lying on the floor %s %s.\n",
                (o_ptr->origin_depth? "at": "in"), origin_text);
            break;
        }

        case ORIGIN_DROP:
        case ORIGIN_DROP_SPECIAL:
        case ORIGIN_DROP_PIT:
        case ORIGIN_DROP_VAULT:
        case ORIGIN_DROP_SUMMON:
        case ORIGIN_DROP_BREED:
        case ORIGIN_DROP_POLY:
        {
            monster_race *r_ptr = &r_info[o_ptr->origin_xtra];
            const char *name;

            if (r_ptr->ridx) name = r_ptr->name;
            else name = "monster lost to history";

            text_out(p, "Dropped by ");

            if (rf_has(r_ptr->flags, RF_UNIQUE))
                text_out(p, "%s", name);
            else
                text_out(p, "%s%s", is_a_vowel(name[0])? "an ": "a ", name);

            text_out(p, " %s %s.\n", (o_ptr->origin_depth? "at": "in"), origin_text);

            break;
        }

        case ORIGIN_DROP_UNKNOWN:
        {
            text_out(p, "Dropped by an unknown monster %s %s.\n",
                (o_ptr->origin_depth? "at": "in"), origin_text);
            break;
        }

        case ORIGIN_ACQUIRE:
        {
            text_out(p, "Conjured forth by magic %s %s.\n",
                (o_ptr->origin_depth? "at": "in"), origin_text);
            break;
        }

        case ORIGIN_CHEAT:
        {
            text_out(p, "Created by debug option.\n");
            break;
        }

        case ORIGIN_CHEST:
        {
            text_out(p, "Found in a chest from %s.\n", origin_text);
            break;
        }

        case ORIGIN_SPECIAL:
        {
            text_out(p, "Found lying on the floor of a special room at %s.\n", origin_text);
            break;
        }

        case ORIGIN_PIT:
        {
            text_out(p, "Found lying on the floor in a pit at %s.\n", origin_text);
            break;
        }

        case ORIGIN_VAULT:
        {
            text_out(p, "Found lying on the floor in a vault at %s.\n", origin_text);
            break;
        }

        case ORIGIN_LABYRINTH:
        {
            text_out(p, "Found lying on the floor of a labyrinth at %s.\n", origin_text);
            break;
        }

        case ORIGIN_CAVERN:
        {
            text_out(p, "Found lying on the floor of a cavern at %s.\n", origin_text);
            break;
        }

        case ORIGIN_RUBBLE:
        {
            text_out(p, "Found under some rubble at %s.\n", origin_text);
            break;
        }

        case ORIGIN_PLAYER:
        {
            text_out(p, "Purchased from a player %s %s.\n",
                (o_ptr->origin_depth? "at": "in"), origin_text);
            break;
        }

        case ORIGIN_FOUNTAIN:
        {
            text_out(p, "Obtained from a fountain at %s.\n", origin_text);
            break;
        }
    }

    text_out(p, "\n");
}


/*
 * Print an item's flavour text.
 *
 * o_ptr is the object we are describing
 */
static void describe_flavor_text(struct player *p, const object_type *o_ptr)
{
    /* Display the known artifact description */
    if (true_artifact_p(o_ptr) && object_is_known(p, o_ptr) && o_ptr->artifact->text)
        text_out(p, "%s\n\n", o_ptr->artifact->text);

    /* Display the known object description */
    else if (object_flavor_is_aware(p, o_ptr))
    {
        bool did_desc = FALSE;

        if (o_ptr->kind->text)
        {
            text_out(p, "%s", o_ptr->kind->text);
            did_desc = TRUE;
        }

        /* Display an additional ego-item description */
        if (object_ego_is_visible(o_ptr) && o_ptr->ego->text)
        {
            if (did_desc) text_out(p, " ");
            text_out(p, "%s", o_ptr->ego->text);
            did_desc = TRUE;
        }

        if (did_desc) text_out(p, "\n\n");
    }
}


/*
 * Output object information
 */
static void object_info_out(struct player *p, const object_type *o_ptr, oinfo_detail_t mode)
{
    bitflag flags[OF_SIZE];
    bitflag pval_flags[MAX_PVALS][OF_SIZE];
    bool something = FALSE;
    bool known = object_is_known(p, o_ptr);
    bool aware = object_flavor_is_aware(p, o_ptr);
    bool full = mode & OINFO_FULL;

    /* Hack - "wearable" items other than weapons/ammo add slays/brands to melee attacks */
    bool fulldesc = (wearable_p(o_ptr) && !wieldable_p(o_ptr));

    /* Grab the object flags */
    if (full)
    {
        object_flags(o_ptr, flags);
        object_pval_flags(o_ptr, pval_flags);
    }
    else
    {
        object_flags_known(o_ptr, flags, aware);
        object_pval_flags_known(o_ptr, pval_flags, aware);
    }

    /* Print origin and descriptive text for a given object */
    describe_origin(p, o_ptr);
    describe_flavor_text(p, o_ptr);

    /* Unidentified item */
    if (!full && !known)
    {
        text_out(p, "You do not know the full extent of this item's powers.\n");
        something = TRUE;
    }

    /* Describe bits */
    if (describe_curses(p, o_ptr, flags)) something = TRUE;
    if (describe_stats(p, o_ptr, pval_flags, mode)) something = TRUE;
    if (describe_slays(p, flags, fulldesc)) something = TRUE;
    if (describe_immune(p, flags)) something = TRUE;
    if (describe_ignores(p, flags)) something = TRUE;
    dedup_hates_flags(flags);
    if (describe_hates(p, flags)) something = TRUE;
    if (describe_sustains(p, flags)) something = TRUE;
    if (describe_misc_magic(p, flags)) something = TRUE;
    if (describe_esp(p, flags)) something = TRUE;
    if (something) text_out(p, "\n");

    if (describe_activation(p, o_ptr, full))
    {
        something = TRUE;
        text_out(p, "\n");
    }

    /* Describe combat bits */
    if (describe_combat(p, o_ptr, mode))
    {
        something = TRUE;
        text_out(p, "\n");
    }

    /* Describe boring bits */
    if (describe_food(p, o_ptr, full)) something = TRUE;
    if (describe_light(p, o_ptr, flags)) something = TRUE;
    if (describe_digger(p, o_ptr, mode)) something = TRUE;

    if (!something)
        text_out(p, "\n\nThis item does not seem to possess any special abilities.");
}


/*
 * Provide information on an item, including how it would affect the current
 * player's state.
 *
 * OINFO_FULL mode should be set if actual player knowledge should be ignored
 * in favour of full knowledge
 */
void object_info(struct player *p, const object_type *o_ptr, oinfo_detail_t mode)
{
    object_info_out(p, o_ptr, mode);
}
