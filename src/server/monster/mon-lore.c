/*
 * File: mon-lore.c
 * Purpose: Monster recall code.
 *
 * Copyright (c) 1997-2007 Ben Harrison, James E. Wilson, Robert A. Koeneke
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
#include "mon-lore.h"
#include "mon-spell.h"


/*
 * Monster genders
 */
enum monster_sex
{
    MON_SEX_NEUTER = 0,
    MON_SEX_MALE,
    MON_SEX_FEMALE
};


/*
 * Pronoun arrays, by gender.
 */
static const char *wd_he[3] = {"It", "He", "She"};
static const char *wd_helw[3] = {"it", "he", "she"};
static const char *wd_his[3] = {"its", "his", "her"};


/*
 * Pluralizer.  Args(count, singular, plural)
 */
#define plural(c, s, p) (((c) == 1)? (s): (p))


/*
 * Prints `num` elements from `list` using color attribute `attr`, and joins
 * them with the given conjunction ("and" or "or").
 */
static void output_list(struct player *p, const char *list[], int num, byte attr,
    const char *conjunction)
{
    int i;

    my_assert(num >= 0);

    for (i = 0; i < num; i++)
    {
        if (i)
        {
            if (num > 2)
                text_out(p, ", ");
            else
                text_out(p, " ");

            if (i == num - 1)
                text_out(p, conjunction);
        }

        text_out_c(p, attr, list[i]);
    }
}


/*
 * Prints `num` elements from `list` using the given colors and damage amounts,
 * and joins them with the given conjunction ("and" or "or").
 */
static void output_list_dam(struct player *p, const char *list[], int num, int col[], int dam[],
    const char *conjunction)
{
    int i;

    my_assert(num >= 0);

    for (i = 0; i < num; i++)
    {
        if (i)
        {
            if (num > 2)
                text_out(p, ", ");
            else
                text_out(p, " ");

            if (i == num - 1)
                text_out(p, conjunction);
        }

        text_out_c(p, col[i], list[i]);

        if (dam[i])
            text_out_c(p, col[i], " (%d)", dam[i]);
    }
}


/*
 * Prints "[pronoun] [verb] [list]", where the verb is given by `intro`.
 * The elements of the list are printed using the given color attribute.
 */
static void output_desc_list(struct player *p, enum monster_sex msex, const char *intro,
    const char *list[], int num, byte attr)
{
    my_assert(num >= 0);

    if (num != 0)
    {
        text_out(p, "%s %s ", wd_he[msex], intro);
        output_list(p, list, num, attr, "and ");
        text_out(p, ". ");
    }
}


/*
 * Initializes the color-coding of monster attacks / spells.
 *
 * This function assigns a color to each monster melee attack type and each
 * monster spell, depending on how dangerous the attack is to the player
 * given current gear and state. Attacks may be colored green (least
 * dangerous), yellow, orange, or red (most dangerous). The colors are stored
 * in `melee_colors` and `spell_colors`, which the calling function then
 * uses when printing the monster recall.
 */
static void get_attack_colors(struct player *p, int melee_colors[RBE_MAX], int spell_colors[RSF_MAX])
{
    int i;
    bool known;
    bitflag f[OF_SIZE];
    player_state st;
    int tmp_col;

    WIPE(&st, player_state);
    calc_bonuses(p, p->inventory, &st, TRUE);

    /* Initialize the colors to green */
    for (i = 0; i < RBE_MAX; i++) melee_colors[i] = TERM_L_GREEN;
    for (i = 0; i < RSF_MAX; i++) spell_colors[i] = TERM_L_GREEN;

    /* Scan the inventory for potentially vulnerable items */
    for (i = 0; i < INVEN_TOTAL; i++)
    {
        object_type *o_ptr = &p->inventory[i];

        /* Only occupied slots */
        if (!o_ptr->kind) continue;

        /* Extract the flags */
        object_flags_known(o_ptr, f, object_flavor_is_aware(p, o_ptr));

        /*
         * Don't reveal the nature of an object.
         * Assume the player is conservative with unknown items.
         */
        known = object_is_known(p, o_ptr);

        /* Drain charges - requires a charged item */
        if ((i < INVEN_PACK) && (!known || (o_ptr->pval[DEFAULT_PVAL] > 0)) &&
            ((o_ptr->tval == TV_STAFF) || (o_ptr->tval == TV_WAND)))
                melee_colors[RBE_UN_POWER] = TERM_L_RED;

        /* Steal item - requires non-artifacts */
        if ((i < INVEN_PACK) && (!known || !o_ptr->artifact) &&
            p->lev + adj_dex_safe[st.stat_ind[A_DEX]] < 100)
                melee_colors[RBE_EAT_ITEM] = TERM_L_RED;

        /* Eat food - requires food */
        if ((i < INVEN_PACK) && is_food(o_ptr))
            melee_colors[RBE_EAT_FOOD] = TERM_YELLOW;

        /* Eat light - requires a fueled light */
        if ((i == INVEN_LIGHT) && (!known || (!of_has(f, OF_NO_FUEL) && (o_ptr->timeout > 0))))
            melee_colors[RBE_EAT_LIGHT] = TERM_YELLOW;

        /* Disenchantment - requires an enchanted item */
        if ((i >= INVEN_WIELD) &&
            (!known || (o_ptr->to_a > 0) || (o_ptr->to_h > 0) || (o_ptr->to_d > 0)) &&
            !check_state_aux(p, st.flags, OF_RES_DISEN))
        {
            melee_colors[RBE_UN_BONUS] = TERM_L_RED;
            spell_colors[RSF_BR_DISE] = TERM_L_RED;
        }
    }

    /* Acid */
    if (check_state_aux(p, st.flags, OF_IM_ACID)) tmp_col = TERM_L_GREEN;
    else if (check_state_aux(p, st.flags, OF_RES_ACID)) tmp_col = TERM_YELLOW;
    else tmp_col = TERM_ORANGE;
    melee_colors[RBE_ACID] = tmp_col;
    spell_colors[RSF_BR_ACID] = tmp_col;
    spell_colors[RSF_BO_ACID] = tmp_col;
    spell_colors[RSF_BA_ACID] = tmp_col;

    /* Cold and ice */
    if (check_state_aux(p, st.flags, OF_IM_COLD)) tmp_col = TERM_L_GREEN;
    else if (check_state_aux(p, st.flags, OF_RES_COLD)) tmp_col = TERM_YELLOW;
    else tmp_col = TERM_ORANGE;
    melee_colors[RBE_COLD] = tmp_col;
    spell_colors[RSF_BR_COLD] = tmp_col;
    spell_colors[RSF_BO_COLD] = tmp_col;
    spell_colors[RSF_BA_COLD] = tmp_col;
    spell_colors[RSF_BO_ICEE] = tmp_col;

    /* Elec */
    if (check_state_aux(p, st.flags, OF_IM_ELEC)) tmp_col = TERM_L_GREEN;
    else if (check_state_aux(p, st.flags, OF_RES_ELEC)) tmp_col = TERM_YELLOW;
    else tmp_col = TERM_ORANGE;
    melee_colors[RBE_ELEC] = tmp_col;
    spell_colors[RSF_BR_ELEC] = tmp_col;
    spell_colors[RSF_BO_ELEC] = tmp_col;
    spell_colors[RSF_BA_ELEC] = tmp_col;

    /* Fire */
    if (check_state_aux(p, st.flags, OF_IM_FIRE)) tmp_col = TERM_L_GREEN;
    else if (check_state_aux(p, st.flags, OF_RES_FIRE)) tmp_col = TERM_YELLOW;
    else tmp_col = TERM_ORANGE;
    melee_colors[RBE_FIRE] = tmp_col;
    spell_colors[RSF_BR_FIRE] = tmp_col;
    spell_colors[RSF_BO_FIRE] = tmp_col;
    spell_colors[RSF_BA_FIRE] = tmp_col;

    /* Poison (and disease) */
    if (!check_state_aux(p, st.flags, OF_RES_POIS))
    {
        melee_colors[RBE_POISON] = TERM_ORANGE;
        spell_colors[RSF_BR_POIS] = TERM_ORANGE;
        spell_colors[RSF_BA_POIS] = TERM_ORANGE;
        melee_colors[RBE_DISEASE] = TERM_ORANGE;
    }

    /* Nexus */
    if (!check_state_aux(p, st.flags, OF_RES_NEXUS))
    {
        if (st.skills[SKILL_SAVE] < 100)
            spell_colors[RSF_BR_NEXU] = TERM_L_RED;
        else
            spell_colors[RSF_BR_NEXU] = TERM_YELLOW;
    }

    /* Nether */
    if (!check_state_aux(p, st.flags, OF_RES_NETHR))
    {
        spell_colors[RSF_BR_NETH] = TERM_ORANGE;
        spell_colors[RSF_BA_NETH] = TERM_ORANGE;
        spell_colors[RSF_BO_NETH] = TERM_ORANGE;
    }

    /* Inertia, gravity, and time */
    spell_colors[RSF_BR_INER] = TERM_ORANGE;
    spell_colors[RSF_BR_GRAV] = TERM_L_RED;
    if (check_state_aux(p, st.flags, OF_RES_TIME))
    {
        melee_colors[RBE_TIME] = TERM_YELLOW;
        spell_colors[RSF_BR_TIME] = TERM_ORANGE;
    }
    else
    {
        melee_colors[RBE_TIME] = TERM_ORANGE;
        spell_colors[RSF_BR_TIME] = TERM_L_RED;
    }

    /* Sound */
    if (!check_state_aux(p, st.flags, OF_RES_SOUND) || !check_state_aux(p, st.flags, OF_RES_STUN))
        spell_colors[RSF_BR_SOUN] = TERM_ORANGE;

    /* Force and plasma */
    if (!check_state_aux(p, st.flags, OF_RES_STUN))
    {
        spell_colors[RSF_BR_WALL] = TERM_YELLOW;

        spell_colors[RSF_BR_PLAS] = TERM_ORANGE;
        spell_colors[RSF_BO_PLAS] = TERM_ORANGE;
    }
    else
    {
        spell_colors[RSF_BR_PLAS] = TERM_YELLOW;
        spell_colors[RSF_BO_PLAS] = TERM_YELLOW;
    }

    /* Shards */
    if (!check_state_aux(p, st.flags, OF_RES_SHARD))
        spell_colors[RSF_BR_SHAR] = TERM_ORANGE;

    /* Confusion */
    if (!check_state_aux(p, st.flags, OF_RES_CONFU))
        melee_colors[RBE_CONFUSE] = TERM_ORANGE;

    /* Chaos */
    if (!check_state_aux(p, st.flags, OF_RES_CHAOS))
        spell_colors[RSF_BR_CHAO] = TERM_ORANGE;

    /* Light */
    if (!check_state_aux(p, st.flags, OF_RES_LIGHT))
        spell_colors[RSF_BR_LIGHT] = TERM_ORANGE;

    /* Darkness */
    if (!check_state_aux(p, st.flags, OF_RES_DARK))
    {
        spell_colors[RSF_BR_DARK] = TERM_ORANGE;
        spell_colors[RSF_BA_DARK] = TERM_L_RED;
    }

    /* Water */
    if (!check_state_aux(p, st.flags, OF_RES_CONFU) || !check_state_aux(p, st.flags, OF_RES_STUN))
    {
        spell_colors[RSF_BR_WATE] = TERM_L_RED;
        spell_colors[RSF_BA_WATE] = TERM_L_RED;
        spell_colors[RSF_BO_WATE] = TERM_L_RED;
    }
    else
    {
        spell_colors[RSF_BR_WATE] = TERM_ORANGE;
        spell_colors[RSF_BA_WATE] = TERM_ORANGE;
        spell_colors[RSF_BO_WATE] = TERM_ORANGE;
    }

    /* Mana */
    if (check_state_aux(p, st.flags, OF_RES_MANA))
    {
        spell_colors[RSF_BR_MANA] = TERM_ORANGE;
        spell_colors[RSF_BA_MANA] = TERM_ORANGE;
        spell_colors[RSF_BO_MANA] = TERM_ORANGE;
    }
    else
    {
        spell_colors[RSF_BR_MANA] = TERM_L_RED;
        spell_colors[RSF_BA_MANA] = TERM_L_RED;
        spell_colors[RSF_BO_MANA] = TERM_L_RED;
    }

    /* These attacks only apply without a perfect save */
    if (st.skills[SKILL_SAVE] < 100)
    {
        /* Amnesia */
        melee_colors[RBE_FORGET] = TERM_YELLOW;
        spell_colors[RSF_FORGET] = TERM_YELLOW;

        /* Fear */
        if (!check_state_aux(p, st.flags, OF_RES_FEAR))
        {
            melee_colors[RBE_TERRIFY] = TERM_YELLOW;
            spell_colors[RSF_SCARE] = TERM_YELLOW;
        }

        /* Paralysis and slow */
        if (!check_state_aux(p, st.flags, OF_FREE_ACT))
        {
            melee_colors[RBE_PARALYZE] = TERM_L_RED;
            spell_colors[RSF_HOLD] = TERM_L_RED;
            spell_colors[RSF_SLOW] = TERM_ORANGE;
        }

        /* Blind */
        if (!check_state_aux(p, st.flags, OF_RES_BLIND))
            spell_colors[RSF_BLIND] = TERM_ORANGE;

        /* Confusion */
        if (!check_state_aux(p, st.flags, OF_RES_CONFU))
            spell_colors[RSF_CONF] = TERM_ORANGE;

        /* Cause wounds */
        spell_colors[RSF_CAUSE_1] = TERM_YELLOW;
        spell_colors[RSF_CAUSE_2] = TERM_YELLOW;
        spell_colors[RSF_CAUSE_3] = TERM_YELLOW;
        spell_colors[RSF_CAUSE_4] = TERM_YELLOW;

        /* Mind blast */
        spell_colors[RSF_MIND_BLAST] =
            (check_state_aux(p, st.flags, OF_RES_CONFU)? TERM_YELLOW: TERM_ORANGE);

        /* Brain smash slows even when conf/blind resisted */
        spell_colors[RSF_BRAIN_SMASH] = ((check_state_aux(p, st.flags, OF_RES_BLIND) &&
            check_state_aux(p, st.flags, OF_FREE_ACT) &&
            check_state_aux(p, st.flags, OF_RES_CONFU))? TERM_ORANGE: TERM_L_RED);
    }

    /* Gold theft */
    if (p->lev + adj_dex_safe[st.stat_ind[A_DEX]] < 100 && p->au)
        melee_colors[RBE_EAT_GOLD] = TERM_YELLOW;

    /* Melee blindness and hallucinations */
    if (!check_state_aux(p, st.flags, OF_RES_BLIND))
        melee_colors[RBE_BLIND] = TERM_YELLOW;
    if (!check_state_aux(p, st.flags, OF_RES_CHAOS))
        melee_colors[RBE_HALLU] = TERM_YELLOW;

    /* Stat draining is bad */
    if (!check_state_aux(p, st.flags, OF_SUST_STR))
        melee_colors[RBE_LOSE_STR] = TERM_ORANGE;
    if (!check_state_aux(p, st.flags, OF_SUST_INT))
        melee_colors[RBE_LOSE_INT] = TERM_ORANGE;
    if (!check_state_aux(p, st.flags, OF_SUST_WIS))
        melee_colors[RBE_LOSE_WIS] = TERM_ORANGE;
    if (!check_state_aux(p, st.flags, OF_SUST_DEX))
        melee_colors[RBE_LOSE_DEX] = TERM_ORANGE;
    if (!check_state_aux(p, st.flags, OF_SUST_CON))
        melee_colors[RBE_LOSE_CON] = TERM_ORANGE;
    if (!check_state_aux(p, st.flags, OF_SUST_CHR))
        melee_colors[RBE_LOSE_CHR] = TERM_ORANGE;

    if (!check_state_aux(p, st.flags, OF_SUST_CON)) melee_colors[RBE_DISEASE] = TERM_ORANGE;

    /* Drain all gets a red warning */
    if (!check_state_aux(p, st.flags, OF_SUST_STR) || !check_state_aux(p, st.flags, OF_SUST_INT) ||
        !check_state_aux(p, st.flags, OF_SUST_WIS) || !check_state_aux(p, st.flags, OF_SUST_DEX) ||
        !check_state_aux(p, st.flags, OF_SUST_CON) || !check_state_aux(p, st.flags, OF_SUST_CHR))
            melee_colors[RBE_LOSE_ALL] = TERM_L_RED;

    /* Hold life isn't 100% effective */
    melee_colors[RBE_EXP_10] = melee_colors[RBE_EXP_20] =
        melee_colors[RBE_EXP_40] = melee_colors[RBE_EXP_80] =
            (check_state_aux(p, st.flags, OF_HOLD_LIFE)? TERM_YELLOW: TERM_ORANGE);

    /* Shatter is always noteworthy */
    melee_colors[RBE_SHATTER] = TERM_YELLOW;

    /* Heal (and drain mana) and haste are always noteworthy */
    spell_colors[RSF_HEAL] = TERM_YELLOW;
    spell_colors[RSF_DRAIN_MANA] = TERM_YELLOW;
    spell_colors[RSF_HASTE] = TERM_YELLOW;

    /* Player teleports and traps are annoying */
    spell_colors[RSF_TELE_TO] = TERM_YELLOW;
    spell_colors[RSF_TELE_AWAY] = TERM_YELLOW;
    if (!check_state_aux(p, st.flags, OF_RES_NEXUS) && (st.skills[SKILL_SAVE] < 100))
        spell_colors[RSF_TELE_LEVEL] = TERM_YELLOW;
    spell_colors[RSF_TRAPS] = TERM_YELLOW;

    /* Summons are potentially dangerous */
    spell_colors[RSF_S_MONSTER] = TERM_ORANGE;
    spell_colors[RSF_S_MONSTERS] = TERM_ORANGE;
    spell_colors[RSF_S_KIN] = TERM_ORANGE;
    spell_colors[RSF_S_ANIMAL] = TERM_ORANGE;
    spell_colors[RSF_S_SPIDER] = TERM_ORANGE;
    spell_colors[RSF_S_HOUND] = TERM_ORANGE;
    spell_colors[RSF_S_HYDRA] = TERM_ORANGE;
    spell_colors[RSF_S_AINU] = TERM_ORANGE;
    spell_colors[RSF_S_DEMON] = TERM_ORANGE;
    spell_colors[RSF_S_DRAGON] = TERM_ORANGE;
    spell_colors[RSF_S_UNDEAD] = TERM_ORANGE;

    /* High level summons are very dangerous */
    spell_colors[RSF_S_HI_DEMON] = TERM_L_RED;
    spell_colors[RSF_S_HI_DRAGON] = TERM_L_RED;
    spell_colors[RSF_S_HI_UNDEAD] = TERM_L_RED;
    spell_colors[RSF_S_UNIQUE] = TERM_L_RED;
    spell_colors[RSF_S_WRAITH] = TERM_L_RED;

    /* Shrieking can lead to bad combos */
    spell_colors[RSF_SHRIEK] = TERM_ORANGE;

    /*
     * Ranged attacks can't be resisted (only mitigated by accuracy)
     * They are colored yellow to indicate the damage is a hard value
     */
    spell_colors[RSF_ARROW_X] = TERM_YELLOW;
    spell_colors[RSF_ARROW_1] = TERM_YELLOW;
    spell_colors[RSF_ARROW_2] = TERM_YELLOW;
    spell_colors[RSF_ARROW_3] = TERM_YELLOW;
    spell_colors[RSF_ARROW_4] = TERM_YELLOW;
    spell_colors[RSF_BOULDER] = TERM_YELLOW;

    /* PWMAngband */
    melee_colors[RBE_DISARM] = TERM_YELLOW;
    if (!p->ghost) melee_colors[RBE_FAMINE] = TERM_YELLOW;
    spell_colors[RSF_ANIM_DEAD] = TERM_ORANGE;
}


/*
 * Determine if the player knows the AC of the given monster.
 *
 * In order for the player to know the AC of a monster, the number of total
 * kills (this life + all past lives) must be high enough. For high-level
 * monsters, fewer kills are needed. Uniques also require far fewer kills.
 */
static bool know_armour(const monster_race *r_ptr, const monster_lore *l_ptr)
{
    s32b level;
    s32b kills;

    my_assert(r_ptr && l_ptr);

    level = r_ptr->level;
    kills = l_ptr->pkills;

    if (kills > 304 / (4 + level)) return TRUE;
    if (rf_has(r_ptr->flags, RF_UNIQUE) && (kills > 304 / (38 + (5 * level) / 4))) return TRUE;
    return FALSE;
}


/*
 * Determine if the player knows the damage of the given attack.
 *
 * In order for the player to know how much damage an attack does, the monster
 * must use the attack against the player enough times. Fewer attacks are
 * necessary for higher-level monsters and fewer still for unique monsters.
 * More attacks are necessary for blows that deal a lot of damage.
 */
static bool know_damage(const monster_race *r_ptr, const monster_lore *l_ptr, int blow_num)
{
    s32b level, attacks, d1, d2, max_damage;

    my_assert(r_ptr && l_ptr);

    level = r_ptr->level;
    attacks = l_ptr->blows[blow_num];
    d1 = r_ptr->blow[blow_num].d_dice;
    d2 = r_ptr->blow[blow_num].d_side;
    max_damage = d1 * d2;

    if ((4 + level) * attacks >= 80 * max_damage) return TRUE;
    if (rf_has(r_ptr->flags, RF_UNIQUE) && ((4 + level) * (2 * attacks) > 80 * max_damage))
        return TRUE;
    return FALSE;
}


/*
 * Prints the flavour text of a monster.
 */
static void describe_monster_desc(struct player *p, const monster_race *r_ptr)
{
    my_assert(r_ptr);

    text_out(p, "%s\n\n", r_ptr->text);
}


/*
 * Prints a colorized description of what spells a monster can cast.
 *
 * Using the colors in `colors`, this function prints out a full list of the
 * spells that the player knows a given monster can cast, including the
 * maximum damage of each spell.
 */
static void describe_monster_spells(struct player *p, const monster_race *r_ptr,
    const monster_lore *l_ptr, const int colors[RSF_MAX])
{
    bitflag f[RF_SIZE];
    int m, n;
    enum monster_sex msex = MON_SEX_NEUTER;
    bool breath = FALSE;
    bool magic = FALSE;
    int vn; /* list size */
    const char *names[64]; /* list item names */
    int cols[64]; /* list colors */
    int dams[64]; /* list avg damage values */
    int known_hp;

    my_assert(r_ptr && l_ptr);

    /* Get the known monster flags */
    monster_flags_known(r_ptr, l_ptr, f);

    /* Extract a gender (if applicable) */
    if (rf_has(r_ptr->flags, RF_FEMALE)) msex = MON_SEX_FEMALE;
    else if (rf_has(r_ptr->flags, RF_MALE)) msex = MON_SEX_MALE;

    /* Collect innate attacks */
    vn = 0;
    for (m = 0; m < 64; m++)
    {
        dams[m] = 0;
        cols[m] = TERM_WHITE;
    }
    if (rsf_has(l_ptr->spell_flags, RSF_SHRIEK))
    {
        names[vn] = "shriek for help";
        cols[vn++] = colors[RSF_SHRIEK];
    }
    if (rsf_has(l_ptr->spell_flags, RSF_ARROW_X))
    {
        names[vn] = "fire a seeker arrow";
        cols[vn] = colors[RSF_ARROW_X];
        dams[vn++] = ARROWX_DMG(r_ptr->level, MAXIMISE);
    }
    if (rsf_has(l_ptr->spell_flags, RSF_ARROW_1))
    {
        names[vn] = "fire a shot";
        cols[vn] = colors[RSF_ARROW_1];
        dams[vn++] = ARROW1_DMG(r_ptr->level, MAXIMISE);
    }
    if (rsf_has(l_ptr->spell_flags, RSF_ARROW_2))
    {
        names[vn] = "fire an arrow";
        cols[vn] = colors[RSF_ARROW_2];
        dams[vn++] = ARROW2_DMG(r_ptr->level, MAXIMISE);
    }
    if (rsf_has(l_ptr->spell_flags, RSF_ARROW_3))
    {
        names[vn] = "fire a bolt";
        cols[vn] = colors[RSF_ARROW_3];
        dams[vn++] = ARROW3_DMG(r_ptr->level, MAXIMISE);
    }
    if (rsf_has(l_ptr->spell_flags, RSF_ARROW_4))
    {
        names[vn] = "fire a missile";
        cols[vn] = colors[RSF_ARROW_4];
        dams[vn++] = ARROW4_DMG(r_ptr->level, MAXIMISE);
    }
    if (rsf_has(l_ptr->spell_flags, RSF_BOULDER))
    {
        names[vn] = "throw boulders";
        cols[vn] = colors[RSF_BOULDER];
        dams[vn++] = BOULDER_DMG(r_ptr->level, MAXIMISE);
    }

    /* Describe innate attacks */
    if (vn)
    {
        text_out(p, "%s may ", wd_he[msex]);
        output_list_dam(p, names, vn, cols, dams, "or ");
        text_out(p, ". ");
    }

    /* Collect breaths */
    vn = 0;
    for (m = 0; m < 64; m++)
    {
        dams[m] = 0;
        cols[m] = TERM_WHITE;
    }

    known_hp = know_armour(r_ptr, l_ptr)? r_ptr->avg_hp: 0;

    if (rsf_has(l_ptr->spell_flags, RSF_BR_ACID))
    {
        names[vn] = "acid";
        cols[vn] = colors[RSF_BR_ACID];
        dams[vn++] = MIN(known_hp / BR_ACID_DIVISOR, BR_ACID_MAX);
    }
    if (rsf_has(l_ptr->spell_flags, RSF_BR_ELEC))
    {
        names[vn] = "lightning";
        cols[vn] = colors[RSF_BR_ELEC];
        dams[vn++] = MIN(known_hp / BR_ELEC_DIVISOR, BR_ELEC_MAX);
    }
    if (rsf_has(l_ptr->spell_flags, RSF_BR_FIRE))
    {
        names[vn] = "fire";
        cols[vn] = colors[RSF_BR_FIRE];
        dams[vn++] = MIN(known_hp / BR_FIRE_DIVISOR, BR_FIRE_MAX);
    }
    if (rsf_has(l_ptr->spell_flags, RSF_BR_COLD))
    {
        names[vn] = "frost";
        cols[vn] = colors[RSF_BR_COLD];
        dams[vn++] = MIN(known_hp / BR_COLD_DIVISOR, BR_COLD_MAX);
    }
    if (rsf_has(l_ptr->spell_flags, RSF_BR_POIS))
    {
        names[vn] = "poison";
        cols[vn] = colors[RSF_BR_POIS];
        dams[vn++] = MIN(known_hp / BR_POIS_DIVISOR, BR_POIS_MAX);
    }
    if (rsf_has(l_ptr->spell_flags, RSF_BR_NETH))
    {
        names[vn] = "nether";
        cols[vn] = colors[RSF_BR_NETH];
        dams[vn++] = MIN(known_hp / BR_NETH_DIVISOR, BR_NETH_MAX);
    }
    if (rsf_has(l_ptr->spell_flags, RSF_BR_LIGHT))
    {
        names[vn] = "light";
        cols[vn] = colors[RSF_BR_LIGHT];
        dams[vn++] = MIN(known_hp / BR_LIGHT_DIVISOR, BR_LIGHT_MAX);
    }
    if (rsf_has(l_ptr->spell_flags, RSF_BR_DARK))
    {
        names[vn] = "darkness";
        cols[vn] = colors[RSF_BR_DARK];
        dams[vn++] = MIN(known_hp / BR_DARK_DIVISOR, BR_DARK_MAX);
    }
    if (rsf_has(l_ptr->spell_flags, RSF_BR_SOUN))
    {
        names[vn] = "sound";
        cols[vn] = colors[RSF_BR_SOUN];
        dams[vn++] = MIN(known_hp / BR_SOUN_DIVISOR, BR_SOUN_MAX);
    }
    if (rsf_has(l_ptr->spell_flags, RSF_BR_CHAO))
    {
        names[vn] = "chaos";
        cols[vn] = colors[RSF_BR_CHAO];
        dams[vn++] = MIN(known_hp / BR_CHAO_DIVISOR, BR_CHAO_MAX);
    }
    if (rsf_has(l_ptr->spell_flags, RSF_BR_DISE))
    {
        names[vn] = "disenchantment";
        cols[vn] = colors[RSF_BR_DISE];
        dams[vn++] = MIN(known_hp / BR_DISE_DIVISOR, BR_DISE_MAX);
    }
    if (rsf_has(l_ptr->spell_flags, RSF_BR_NEXU))
    {
        names[vn] = "nexus";
        cols[vn] = colors[RSF_BR_NEXU];
        dams[vn++] = MIN(known_hp / BR_NEXU_DIVISOR, BR_NEXU_MAX);
    }
    if (rsf_has(l_ptr->spell_flags, RSF_BR_TIME))
    {
        names[vn] = "time";
        cols[vn] = colors[RSF_BR_TIME];
        dams[vn++] = MIN(known_hp / BR_TIME_DIVISOR, BR_TIME_MAX);
    }
    if (rsf_has(l_ptr->spell_flags, RSF_BR_INER))
    {
        names[vn] = "inertia";
        cols[vn] = colors[RSF_BR_INER];
        dams[vn++] = MIN(known_hp / BR_INER_DIVISOR, BR_INER_MAX);
    }
    if (rsf_has(l_ptr->spell_flags, RSF_BR_GRAV))
    {
        names[vn] = "gravity";
        cols[vn] = colors[RSF_BR_GRAV];
        dams[vn++] = MIN(known_hp / BR_GRAV_DIVISOR, BR_GRAV_MAX);
    }
    if (rsf_has(l_ptr->spell_flags, RSF_BR_SHAR))
    {
        names[vn] = "shards";
        cols[vn] = colors[RSF_BR_SHAR];
        dams[vn++] = MIN(known_hp / BR_SHAR_DIVISOR, BR_SHAR_MAX);
    }
    if (rsf_has(l_ptr->spell_flags, RSF_BR_PLAS))
    {
        names[vn] = "plasma";
        cols[vn] = colors[RSF_BR_PLAS];
        dams[vn++] = MIN(known_hp / BR_PLAS_DIVISOR, BR_PLAS_MAX);
    }
    if (rsf_has(l_ptr->spell_flags, RSF_BR_WALL))
    {
        names[vn] = "force";
        cols[vn] = colors[RSF_BR_WALL];
        dams[vn++] = MIN(known_hp / BR_FORC_DIVISOR, BR_FORC_MAX);
    }
    if (rsf_has(l_ptr->spell_flags, RSF_BR_MANA))
    {
        names[vn] = "mana";
        cols[vn] = colors[RSF_BR_MANA];
        dams[vn++] = MIN(known_hp / BR_MANA_DIVISOR, BR_MANA_MAX);
    }
    if (rsf_has(l_ptr->spell_flags, RSF_BR_WATE))
    {
        names[vn] = "water";
        cols[vn] = colors[RSF_BR_WATE];
        dams[vn++] = MIN(known_hp / BR_WATE_DIVISOR, BR_WATE_MAX);
    }

    /* Describe breaths */
    if (vn)
    {
        /* Note breath */
        breath = TRUE;

        /* Display */
        text_out(p, "%s may ", wd_he[msex]);
        text_out_c(p, TERM_L_RED, "breathe ");
        output_list_dam(p, names, vn, cols, dams, "or ");
    }

    /* Collect spell information */
    vn = 0;
    for (m = 0; m < 64; m++)
    {
        dams[m] = 0;
        cols[m] = TERM_WHITE;
    }

    /* Ball spells */
    if (rsf_has(l_ptr->spell_flags, RSF_BA_MANA))
    {
        names[vn] = "invoke mana storms";
        cols[vn] = colors[RSF_BA_MANA];
        dams[vn++] = BA_MANA_DMG(r_ptr->level, MAXIMISE);
    }
    if (rsf_has(l_ptr->spell_flags, RSF_BA_DARK))
    {
        names[vn] = "invoke darkness storms";
        cols[vn] = colors[RSF_BA_DARK];
        dams[vn++] = BA_DARK_DMG(r_ptr->level, MAXIMISE);
    }
    if (rsf_has(l_ptr->spell_flags, RSF_BA_WATE))
    {
        names[vn] = "produce water balls";
        cols[vn] = colors[RSF_BA_WATE];
        dams[vn++] = BA_WATE_DMG(r_ptr->level, MAXIMISE);
    }
    if (rsf_has(l_ptr->spell_flags, RSF_BA_NETH))
    {
        names[vn] = "produce nether balls";
        cols[vn] = colors[RSF_BA_NETH];
        dams[vn++] = BA_NETH_DMG(r_ptr->level, MAXIMISE);
    }
    if (rsf_has(l_ptr->spell_flags, RSF_BA_FIRE))
    {
        names[vn] = "produce fire balls";
        cols[vn] = colors[RSF_BA_FIRE];
        dams[vn++] = BA_FIRE_DMG(r_ptr->level, MAXIMISE);
    }
    if (rsf_has(l_ptr->spell_flags, RSF_BA_ACID))
    {
        names[vn] = "produce acid balls";
        cols[vn] = colors[RSF_BA_ACID];
        dams[vn++] = BA_ACID_DMG(r_ptr->level, MAXIMISE);
    }
    if (rsf_has(l_ptr->spell_flags, RSF_BA_COLD))
    {
        names[vn] = "produce frost balls";
        cols[vn] = colors[RSF_BA_COLD];
        dams[vn++] = BA_COLD_DMG(r_ptr->level, MAXIMISE);
    }
    if (rsf_has(l_ptr->spell_flags, RSF_BA_ELEC))
    {
        names[vn] = "produce lightning balls";
        cols[vn] = colors[RSF_BA_ELEC];
        dams[vn++] = BA_ELEC_DMG(r_ptr->level, MAXIMISE);
    }
    if (rsf_has(l_ptr->spell_flags, RSF_BA_POIS))
    {
        names[vn] = "produce poison balls";
        cols[vn] = colors[RSF_BA_POIS];
        dams[vn++] = BA_POIS_DMG(r_ptr->level, MAXIMISE);
    }

    /* Bolt spells */
    if (rsf_has(l_ptr->spell_flags, RSF_BO_MANA))
    {
        names[vn] = "produce mana bolts";
        cols[vn] = colors[RSF_BO_MANA];
        dams[vn++] = BO_MANA_DMG(r_ptr->level, MAXIMISE);
    }
    if (rsf_has(l_ptr->spell_flags, RSF_BO_PLAS))
    {
        names[vn] = "produce plasma bolts";
        cols[vn] = colors[RSF_BO_PLAS];
        dams[vn++] = BO_PLAS_DMG(r_ptr->level, MAXIMISE);
    }
    if (rsf_has(l_ptr->spell_flags, RSF_BO_ICEE))
    {
        names[vn] = "produce ice bolts";
        cols[vn] = colors[RSF_BO_ICEE];
        dams[vn++] = BO_ICEE_DMG(r_ptr->level, MAXIMISE);
    }
    if (rsf_has(l_ptr->spell_flags, RSF_BO_WATE))
    {
        names[vn] = "produce water bolts";
        cols[vn] = colors[RSF_BO_WATE];
        dams[vn++] = BO_WATE_DMG(r_ptr->level, MAXIMISE);
    }
    if (rsf_has(l_ptr->spell_flags, RSF_BO_NETH))
    {
        names[vn] = "produce nether bolts";
        cols[vn] = colors[RSF_BO_NETH];
        dams[vn++] = BO_NETH_DMG(r_ptr->level, MAXIMISE);
    }
    if (rsf_has(l_ptr->spell_flags, RSF_BO_FIRE))
    {
        names[vn] = "produce fire bolts";
        cols[vn] = colors[RSF_BO_FIRE];
        dams[vn++] = BO_FIRE_DMG(r_ptr->level, MAXIMISE);
    }
    if (rsf_has(l_ptr->spell_flags, RSF_BO_ACID))
    {
        names[vn] = "produce acid bolts";
        cols[vn] = colors[RSF_BO_ACID];
        dams[vn++] = BO_ACID_DMG(r_ptr->level, MAXIMISE);
    }
    if (rsf_has(l_ptr->spell_flags, RSF_BO_COLD))
    {
        names[vn] = "produce frost bolts";
        cols[vn] = colors[RSF_BO_COLD];
        dams[vn++] = BO_COLD_DMG(r_ptr->level, MAXIMISE);
    }
    if (rsf_has(l_ptr->spell_flags, RSF_BO_ELEC))
    {
        names[vn] = "produce lightning bolts";
        cols[vn] = colors[RSF_BO_ELEC];
        dams[vn++] = BO_ELEC_DMG(r_ptr->level, MAXIMISE);
    }
    if (rsf_has(l_ptr->spell_flags, RSF_MISSILE))
    {
        names[vn] = "produce magic missiles";
        cols[vn] = colors[RSF_MISSILE];
        dams[vn++] = MISSILE_DMG(r_ptr->level, MAXIMISE);
    }

    /* Curses */
    if (rsf_has(l_ptr->spell_flags, RSF_BRAIN_SMASH))
    {
        names[vn] = "cause brain smashing";
        cols[vn] = colors[RSF_BRAIN_SMASH];
        dams[vn++] = BRAIN_SMASH_DMG(r_ptr->level, MAXIMISE);
    }
    if (rsf_has(l_ptr->spell_flags, RSF_MIND_BLAST))
    {
        names[vn] = "cause mind blasting";
        cols[vn] = colors[RSF_MIND_BLAST];
        dams[vn++] = MIND_BLAST_DMG(r_ptr->level, MAXIMISE);
    }
    if (rsf_has(l_ptr->spell_flags, RSF_CAUSE_4))
    {
        names[vn] = "cause mortal wounds";
        cols[vn] = colors[RSF_CAUSE_4];
        dams[vn++] = CAUSE_4_DMG(r_ptr->level, MAXIMISE);
    }
    if (rsf_has(l_ptr->spell_flags, RSF_CAUSE_3))
    {
        names[vn] = "cause critical wounds";
        cols[vn] = colors[RSF_CAUSE_3];
        dams[vn++] = CAUSE_3_DMG(r_ptr->level, MAXIMISE);
    }
    if (rsf_has(l_ptr->spell_flags, RSF_CAUSE_2))
    {
        names[vn] = "cause serious wounds";
        cols[vn] = colors[RSF_CAUSE_2];
        dams[vn++] = CAUSE_2_DMG(r_ptr->level, MAXIMISE);
    }
    if (rsf_has(l_ptr->spell_flags, RSF_CAUSE_1))
    {
        names[vn] = "cause light wounds";
        cols[vn] = colors[RSF_CAUSE_1];
        dams[vn++] = CAUSE_1_DMG(r_ptr->level, MAXIMISE);
    }
    if (rsf_has(l_ptr->spell_flags, RSF_FORGET))
    {
        names[vn] = "cause amnesia";
        cols[vn++] = colors[RSF_FORGET];
    }
    if (rsf_has(l_ptr->spell_flags, RSF_SCARE))
    {
        names[vn] = "terrify";
        cols[vn++] = colors[RSF_SCARE];
    }
    if (rsf_has(l_ptr->spell_flags, RSF_BLIND))
    {
        names[vn] = "blind";
        cols[vn++] = colors[RSF_BLIND];
    }
    if (rsf_has(l_ptr->spell_flags, RSF_CONF))
    {
        names[vn] = "confuse";
        cols[vn++] = colors[RSF_CONF];
    }
    if (rsf_has(l_ptr->spell_flags, RSF_SLOW))
    {
        names[vn] = "slow";
        cols[vn++] = colors[RSF_SLOW];
    }
    if (rsf_has(l_ptr->spell_flags, RSF_HOLD))
    {
        names[vn] = "paralyze";
        cols[vn++] = colors[RSF_HOLD];
    }

    /* Healing and haste */
    if (rsf_has(l_ptr->spell_flags, RSF_DRAIN_MANA))
    {
        names[vn] = "drain mana";
        cols[vn++] = colors[RSF_DRAIN_MANA];
    }
    if (rsf_has(l_ptr->spell_flags, RSF_HEAL))
    {
        names[vn] = "heal-self";
        cols[vn++] = colors[RSF_HEAL];
    }
    if (rsf_has(l_ptr->spell_flags, RSF_HASTE))
    {
        names[vn] = "haste-self";
        cols[vn++] = colors[RSF_HASTE];
    }

    /* Teleports */
    if (rsf_has(l_ptr->spell_flags, RSF_BLINK))
    {
        names[vn] = "blink-self";
        cols[vn++] = colors[RSF_BLINK];
    }
    if (rsf_has(l_ptr->spell_flags, RSF_TPORT))
    {
        names[vn] = "teleport-self";
        cols[vn++] = colors[RSF_TPORT];
    }
    if (rsf_has(l_ptr->spell_flags, RSF_TELE_TO))
    {
        names[vn] = "teleport to";
        cols[vn++] = colors[RSF_TELE_TO];
    }
    if (rsf_has(l_ptr->spell_flags, RSF_TELE_AWAY))
    {
        names[vn] = "teleport away";
        cols[vn++] = colors[RSF_TELE_AWAY];
    }
    if (rsf_has(l_ptr->spell_flags, RSF_TELE_LEVEL))
    {
        names[vn] = "teleport level";
        cols[vn++] = colors[RSF_TELE_LEVEL];
    }

    /* Annoyances */
    if (rsf_has(l_ptr->spell_flags, RSF_DARKNESS))
    {
        names[vn] = "create darkness";
        cols[vn++] = colors[RSF_DARKNESS];
    }
    if (rsf_has(l_ptr->spell_flags, RSF_TRAPS))
    {
        names[vn] = "create traps";
        cols[vn++] = colors[RSF_TRAPS];
    }

    /* Summoning */
    if (rsf_has(l_ptr->spell_flags, RSF_S_KIN))
    {
        names[vn] = "summon similar monsters";
        cols[vn++] = colors[RSF_S_KIN];
    }
    if (rsf_has(l_ptr->spell_flags, RSF_S_MONSTER))
    {
        names[vn] = "summon a monster";
        cols[vn++] = colors[RSF_S_MONSTER];
    }
    if (rsf_has(l_ptr->spell_flags, RSF_S_MONSTERS))
    {
        names[vn] = "summon monsters";
        cols[vn++] = colors[RSF_S_MONSTERS];
    }
    if (rsf_has(l_ptr->spell_flags, RSF_S_ANIMAL))
    {
        names[vn] = "summon animals";
        cols[vn++] = colors[RSF_S_ANIMAL];
    }
    if (rsf_has(l_ptr->spell_flags, RSF_S_SPIDER))
    {
        names[vn] = "summon spiders";
        cols[vn++] = colors[RSF_S_SPIDER];
    }
    if (rsf_has(l_ptr->spell_flags, RSF_S_HOUND))
    {
        names[vn] = "summon hounds";
        cols[vn++] = colors[RSF_S_HOUND];
    }
    if (rsf_has(l_ptr->spell_flags, RSF_S_HYDRA))
    {
        names[vn] = "summon hydras";
        cols[vn++] = colors[RSF_S_HYDRA];
    }
    if (rsf_has(l_ptr->spell_flags, RSF_S_AINU))
    {
        names[vn] = "summon an ainu";
        cols[vn++] = colors[RSF_S_AINU];
    }
    if (rsf_has(l_ptr->spell_flags, RSF_S_DEMON))
    {
        names[vn] = "summon a demon";
        cols[vn++] = colors[RSF_S_DEMON];
    }
    if (rsf_has(l_ptr->spell_flags, RSF_S_UNDEAD))
    {
        names[vn] = "summon an undead";
        cols[vn++] = colors[RSF_S_UNDEAD];
    }
    if (rsf_has(l_ptr->spell_flags, RSF_S_DRAGON))
    {
        names[vn] = "summon a dragon";
        cols[vn++] = colors[RSF_S_DRAGON];
    }
    if (rsf_has(l_ptr->spell_flags, RSF_S_HI_UNDEAD))
    {
        names[vn] = "summon greater undead";
        cols[vn++] = colors[RSF_S_HI_UNDEAD];
    }
    if (rsf_has(l_ptr->spell_flags, RSF_S_HI_DRAGON))
    {
        names[vn] = "summon ancient dragons";
        cols[vn++] = colors[RSF_S_HI_DRAGON];
    }
    if (rsf_has(l_ptr->spell_flags, RSF_S_HI_DEMON))
    {
        names[vn] = "summon greater demons";
        cols[vn++] = colors[RSF_S_HI_DEMON];
    }
    if (rsf_has(l_ptr->spell_flags, RSF_S_WRAITH))
    {
        names[vn] = "summon ringwraiths";
        cols[vn++] = colors[RSF_S_WRAITH];
    }
    if (rsf_has(l_ptr->spell_flags, RSF_S_UNIQUE))
    {
        names[vn] = "summon uniques";
        cols[vn++] = colors[RSF_S_UNIQUE];
    }

    /* PWMAngband */
    if (rsf_has(l_ptr->spell_flags, RSF_ANIM_DEAD))
    {
        names[vn] = "animate the dead";
        cols[vn++] = colors[RSF_ANIM_DEAD];
    }

    /* Describe spells */
    if (vn)
    {
        /* Note magic */
        magic = TRUE;

        /* Intro */
        if (breath)
            text_out(p, ", and may ");
        else
            text_out(p, "%s may ", wd_he[msex]);

        /* Verb Phrase */
        text_out_c(p, TERM_L_RED, "cast spells");

        /* Adverb */
        if (rf_has(f, RF_SMART)) text_out(p, " intelligently");

        /* List */
        text_out(p, " which ");
        output_list_dam(p, names, vn, cols, dams, "or ");
    }

    /* End the sentence about innate/other spells */
    if (breath || magic)
    {
        /* Total casting */
        m = l_ptr->cast_innate + l_ptr->cast_spell;

        /* Average frequency */
        n = r_ptr->freq_spell;

        /* Describe the spell frequency */
        if (m > 100)
        {
            text_out(p, "; ");
            text_out_c(p, TERM_L_GREEN, "1");
            text_out(p, " time in ");
            text_out_c(p, TERM_L_GREEN, "%d", 100 / n);
        }

        /* Guess at the frequency */
        else if (m)
        {
            n = ((n + 9) / 10) * 10;
            text_out(p, "; about ");
            text_out_c(p, TERM_L_GREEN, "1");
            text_out(p, " time in ");
            text_out_c(p, TERM_L_GREEN, "%d", 100 / n);
        }

        /* End this sentence */
        text_out(p, ". ");
    }
}


/*
 * Prints a description of what a monster can drop.
 *
 * This function prints information about a monster's drop based on what
 * the player has observed, including number of drops, quality of drops, and
 * whether the monster drops items and/or gold.
 */
static void describe_monster_drop(struct player *p, const monster_race *r_ptr,
    const monster_lore *l_ptr)
{
    bitflag f[RF_SIZE];
    int n;
    enum monster_sex msex = MON_SEX_NEUTER;

    my_assert(r_ptr && l_ptr);

    /* Get the known monster flags */
    monster_flags_known(r_ptr, l_ptr, f);

    /* Extract a gender (if applicable) */
    if (rf_has(r_ptr->flags, RF_FEMALE)) msex = MON_SEX_FEMALE;
    else if (rf_has(r_ptr->flags, RF_MALE)) msex = MON_SEX_MALE;

    /* Drops gold and/or items */
    if (l_ptr->drop_gold || l_ptr->drop_item)
    {
        /* Intro */
        text_out(p, "%s may carry", wd_he[msex]);

        /* Count maximum drop */
        n = MAX(l_ptr->drop_gold, l_ptr->drop_item);

        /* Count drops */
        if (n == 1) text_out_c(p, TERM_BLUE, " a single ");
        else if (n == 2) text_out_c(p, TERM_BLUE, " one or two ");
        else
        {
            text_out(p, " up to ");
            text_out_c(p, TERM_BLUE, "%d ", n);
        }

        /* Quality */
        if (rf_has(f, RF_DROP_GREAT)) text_out_c(p, TERM_BLUE, "exceptional ");
        else if (rf_has(f, RF_DROP_GOOD)) text_out_c(p, TERM_BLUE, "good ");

        /* Objects */
        if (l_ptr->drop_item)
        {
            /* Dump "object(s)" */
            text_out_c(p, TERM_BLUE, "object%s", PLURAL(n));

            /* Add conjunction if also dropping gold */
            if (l_ptr->drop_gold) text_out_c(p, TERM_BLUE, " or ");
        }

        /* Treasures */
        if (l_ptr->drop_gold)
        {
            /* Dump "treasure(s)" */
            text_out_c(p, TERM_BLUE, "treasure%s", PLURAL(n));
        }

        /* End this sentence */
        text_out(p, ". ");
    }
}


/*
 * Prints a colorized description of what attacks a monster has.
 *
 * Using the colors in `colors`, this function prints out a full list of the
 * attacks that the player knows a given monster has, including damage if
 * the player knows that.
 */
static void describe_monster_attack(struct player *p, const monster_race *r_ptr,
    const monster_lore *l_ptr, const int colors[RBE_MAX])
{
    bitflag f[RF_SIZE];
    int m, n, r;
    enum monster_sex msex = MON_SEX_NEUTER;

    my_assert(r_ptr && l_ptr);

    /* Get the known monster flags */
    monster_flags_known(r_ptr, l_ptr, f);

    /* Extract a gender (if applicable) */
    if (rf_has(r_ptr->flags, RF_FEMALE)) msex = MON_SEX_FEMALE;
    else if (rf_has(r_ptr->flags, RF_MALE)) msex = MON_SEX_MALE;

    /* Count the number of "known" attacks */
    for (n = 0, m = 0; m < MONSTER_BLOW_MAX; m++)
    {
        /* Skip non-attacks */
        if (!r_ptr->blow[m].method) continue;

        /* Count known attacks */
        if (l_ptr->blows[m]) n++;
    }

    /* Examine (and count) the actual attacks */
    for (r = 0, m = 0; m < MONSTER_BLOW_MAX; m++)
    {
        int method, effect, d1, d2;
        const char *method_str = NULL;
        const char *effect_str = NULL;

        /* Skip unknown and undefined attacks */
        if (!r_ptr->blow[m].method || !l_ptr->blows[m]) continue;

        /* Extract the attack info */
        method = r_ptr->blow[m].method;
        effect = r_ptr->blow[m].effect;
        d1 = r_ptr->blow[m].d_dice;
        d2 = r_ptr->blow[m].d_side;

        /* Get the method */
        switch (method)
        {
            case RBM_HIT:       method_str = "hit"; break;
            case RBM_TOUCH:     method_str = "touch"; break;
            case RBM_PUNCH:     method_str = "punch"; break;
            case RBM_KICK:      method_str = "kick"; break;
            case RBM_CLAW:      method_str = "claw"; break;
            case RBM_BITE:      method_str = "bite"; break;
            case RBM_STING:     method_str = "sting"; break;
            case RBM_BUTT:      method_str = "butt"; break;
            case RBM_CRUSH:     method_str = "crush"; break;
            case RBM_ENGULF:    method_str = "engulf"; break;
            case RBM_CRAWL:     method_str = "crawl on you"; break;
            case RBM_DROOL:     method_str = "drool on you"; break;
            case RBM_SPIT:      method_str = "spit"; break;
            case RBM_GAZE:      method_str = "gaze"; break;
            case RBM_WAIL:      method_str = "wail"; break;
            case RBM_SPORE:     method_str = "release spores"; break;
            case RBM_BEG:       method_str = "beg"; break;
            case RBM_INSULT:    method_str = "insult"; break;
            case RBM_MOAN:      method_str = "moan"; break;
            default:            method_str = "do something weird"; break;
        }

        /* Get the effect */
        switch (effect)
        {
            case RBE_HURT:      effect_str = "attack"; break;
            case RBE_POISON:    effect_str = "poison"; break;
            case RBE_UN_BONUS:  effect_str = "disenchant"; break;
            case RBE_UN_POWER:  effect_str = "drain charges"; break;
            case RBE_EAT_GOLD:  effect_str = "steal gold"; break;
            case RBE_EAT_ITEM:  effect_str = "steal items"; break;
            case RBE_EAT_FOOD:  effect_str = "eat your food"; break;
            case RBE_EAT_LIGHT: effect_str = "absorb light"; break;
            case RBE_ACID:      effect_str = "shoot acid"; break;
            case RBE_ELEC:      effect_str = "electrify"; break;
            case RBE_FIRE:      effect_str = "burn"; break;
            case RBE_COLD:      effect_str = "freeze"; break;
            case RBE_BLIND:     effect_str = "blind"; break;
            case RBE_CONFUSE:   effect_str = "confuse"; break;
            case RBE_TERRIFY:   effect_str = "terrify"; break;
            case RBE_PARALYZE:  effect_str = "paralyze"; break;
            case RBE_LOSE_STR:  effect_str = "reduce strength"; break;
            case RBE_LOSE_INT:  effect_str = "reduce intelligence"; break;
            case RBE_LOSE_WIS:  effect_str = "reduce wisdom"; break;
            case RBE_LOSE_DEX:  effect_str = "reduce dexterity"; break;
            case RBE_LOSE_CON:  effect_str = "reduce constitution"; break;
            case RBE_LOSE_CHR:  effect_str = "reduce charisma"; break;
            case RBE_LOSE_ALL:  effect_str = "reduce all stats"; break;
            case RBE_SHATTER:   effect_str = "shatter"; break;
            case RBE_EXP_10:    effect_str = "lower experience"; break;
            case RBE_EXP_20:    effect_str = "lower experience"; break;
            case RBE_EXP_40:    effect_str = "lower experience"; break;
            case RBE_EXP_80:    effect_str = "lower experience"; break;
            case RBE_HALLU:     effect_str = "cause hallucinations"; break;
            case RBE_FORGET:    effect_str = "cause amnesia"; break;
            case RBE_DISEASE:   effect_str = "cause disease"; break;
            case RBE_TIME:      effect_str = "produce time effects"; break;
            case RBE_DISARM:    effect_str = "disarm"; break;
            case RBE_FAMINE:    effect_str = "cause hunger"; break;
        }

        /* Introduce the attack description */
        if (!r)
            text_out(p, "%s can ", wd_he[msex]);
        else if (r < n - 1)
            text_out(p, ", ");
        else
            text_out(p, ", and ");

        /* Describe the method */
        text_out(p, method_str);

        /* Describe the effect (if any) */
        if (effect_str)
        {
            /* Describe the attack type */
            text_out(p, " to ");
            text_out_c(p, colors[effect], "%s", effect_str);

            /* Describe damage (if known) */
            if (d1 && d2 && know_damage(r_ptr, l_ptr, m))
            {
                text_out(p, " with damage ");
                text_out_c(p, TERM_L_GREEN, "%dd%d", d1, d2);
            }
        }

        /* Count the attacks as printed */
        r++;
    }

    /* Finish sentence above */
    if (r)
        text_out(p, ". ");

    /* Notice lack of attacks */
    else if (rf_has(f, RF_NEVER_BLOW))
        text_out(p, "%s has no physical attacks. ", wd_he[msex]);

    /* Or describe the lack of knowledge */
    else
        text_out(p, "Nothing is known about %s attack. ", wd_his[msex]);
}


/*
 * Describes special abilities of monsters.
 *
 * Based on what the player has observed, prints a description of various
 * monster abilities -- can it open doors, pass through walls, etc.
 * Also prints out a list of monster immunities
 */
static void describe_monster_abilities(struct player *p, const monster_race *r_ptr,
    const monster_lore *l_ptr)
{
    bitflag f[RF_SIZE];
    int vn;
    const char *descs[64];
    bool prev = FALSE;
    enum monster_sex msex = MON_SEX_NEUTER;

    my_assert(r_ptr && l_ptr);

    /* Get the known monster flags */
    monster_flags_known(r_ptr, l_ptr, f);

    /* Extract a gender (if applicable) */
    if (rf_has(r_ptr->flags, RF_FEMALE)) msex = MON_SEX_FEMALE;
    else if (rf_has(r_ptr->flags, RF_MALE)) msex = MON_SEX_MALE;

    /* Collect special abilities. */
    vn = 0;
    if (rf_has(f, RF_OPEN_DOOR)) descs[vn++] = "open doors";
    if (rf_has(f, RF_BASH_DOOR)) descs[vn++] = "bash down doors";
    if (rf_has(f, RF_PASS_WALL)) descs[vn++] = "pass through walls";
    if (rf_has(f, RF_KILL_WALL)) descs[vn++] = "bore through walls";
    if (rf_has(f, RF_MOVE_BODY)) descs[vn++] = "push past weaker monsters";
    if (rf_has(f, RF_KILL_BODY)) descs[vn++] = "destroy weaker monsters";
    if (rf_has(f, RF_TAKE_ITEM)) descs[vn++] = "pick up objects";
    if (rf_has(f, RF_KILL_ITEM)) descs[vn++] = "destroy objects";

    /* Describe special abilities. */
    output_desc_list(p, msex, "can", descs, vn, TERM_WHITE);

    /* Collect detection traits */
    vn = 0;
    if (rf_has(f, RF_INVISIBLE))  descs[vn++] = "invisible";
    if (rf_has(f, RF_COLD_BLOOD)) descs[vn++] = "cold blooded";
    if (rf_has(f, RF_EMPTY_MIND)) descs[vn++] = "not detected by telepathy";
    if (rf_has(f, RF_WEIRD_MIND)) descs[vn++] = "rarely detected by telepathy";

    /* Describe detection traits */
    output_desc_list(p, msex, "is", descs, vn, TERM_WHITE);

    /* Describe special things */
    if (rf_has(f, RF_UNAWARE))
        text_out(p, "%s disguises itself to look like something else. ", wd_he[msex]);
    if (rf_has(f, RF_MULTIPLY))
        text_out_c(p, TERM_ORANGE, "%s breeds explosively. ", wd_he[msex]);
    if (rf_has(f, RF_REGENERATE))
        text_out(p, "%s regenerates quickly. ", wd_he[msex]);
    if (rf_has(f, RF_HAS_LIGHT))
        text_out(p, "%s illuminates %s surroundings. ", wd_he[msex], wd_his[msex]);
    if (rf_has(f, RF_ANTI_MAGIC))
        text_out(p, "%s is surrounded by an anti-magic field. ", wd_he[msex]);

    /* Collect susceptibilities */
    vn = 0;
    if (rf_has(f, RF_HURT_ROCK)) descs[vn++] = "rock remover";
    if (rf_has(f, RF_HURT_LIGHT)) descs[vn++] = "bright light";
    if (rf_has(f, RF_HURT_FIRE)) descs[vn++] = "fire";
    if (rf_has(f, RF_HURT_COLD)) descs[vn++] = "cold";

    /* Describe susceptibilities */
    if (vn)
    {
        /* Output connecting text */
        text_out(p, "%s is hurt by ", wd_he[msex]);
        output_list(p, descs, vn, TERM_VIOLET, "and ");
        prev = TRUE;
    }

    /* Collect immunities and resistances */
    vn = 0;
    if (rf_has(f, RF_IM_ACID))   descs[vn++] = "acid";
    if (rf_has(f, RF_IM_ELEC))   descs[vn++] = "lightning";
    if (rf_has(f, RF_IM_FIRE))   descs[vn++] = "fire";
    if (rf_has(f, RF_IM_COLD))   descs[vn++] = "cold";
    if (rf_has(f, RF_IM_POIS))   descs[vn++] = "poison";
    if (rf_has(f, RF_IM_WATER))  descs[vn++] = "water";
    if (rf_has(f, RF_RES_NETH))  descs[vn++] = "nether";
    if (rf_has(f, RF_RES_PLAS))  descs[vn++] = "plasma";
    if (rf_has(f, RF_RES_NEXUS)) descs[vn++] = "nexus";
    if (rf_has(f, RF_RES_DISE))  descs[vn++] = "disenchantment";

    /* Note lack of vulnerability as a resistance */
    if (rf_has(l_ptr->flags, RF_HURT_LIGHT) && !rf_has(f, RF_HURT_LIGHT))
        descs[vn++] = "bright light";
    if (rf_has(l_ptr->flags, RF_HURT_ROCK) && !rf_has(f, RF_HURT_ROCK))
        descs[vn++] = "rock remover";

    /* Describe immunities and resistances */
    if (vn)
    {
        /* Output connecting text */
        if (prev)
            text_out(p, ", but resists ");
        else
            text_out(p, "%s resists ", wd_he[msex]);

        /* Write the text */
        output_list(p, descs, vn, TERM_L_UMBER, "and ");
        prev = TRUE;
    }

    /* Collect known but average susceptibilities */
    vn = 0;
    if (rf_has(l_ptr->flags, RF_IM_ACID) && !rf_has(f, RF_IM_ACID))
        descs[vn++] = "acid";
    if (rf_has(l_ptr->flags, RF_IM_ELEC) && !rf_has(f, RF_IM_ELEC))
        descs[vn++] = "lightning";
    if (rf_has(l_ptr->flags, RF_IM_FIRE) && !rf_has(f, RF_IM_FIRE) && !rf_has(f, RF_HURT_FIRE))
        descs[vn++] = "fire";
    if (rf_has(l_ptr->flags, RF_IM_COLD) && !rf_has(f, RF_IM_COLD) && !rf_has(f, RF_HURT_COLD))
        descs[vn++] = "cold";
    if (rf_has(l_ptr->flags, RF_IM_POIS) && !rf_has(f, RF_IM_POIS))
        descs[vn++] = "poison";
    if (rf_has(l_ptr->flags, RF_IM_WATER) && !rf_has(f, RF_IM_WATER))
        descs[vn++] = "water";
    if (rf_has(l_ptr->flags, RF_RES_NETH) && !rf_has(f, RF_RES_NETH))
        descs[vn++] = "nether";
    if (rf_has(l_ptr->flags, RF_RES_PLAS) && !rf_has(f, RF_RES_PLAS))
        descs[vn++] = "plasma";
    if (rf_has(l_ptr->flags, RF_RES_NEXUS) && !rf_has(f, RF_RES_NEXUS))
        descs[vn++] = "nexus";
    if (rf_has(l_ptr->flags, RF_RES_DISE) && !rf_has(f, RF_RES_DISE))
        descs[vn++] = "disenchantment";

    /* Describe */
    if (vn)
    {
        /* Output connecting text */
        if (prev)
            text_out(p, ", and does not resist ");
        else
            text_out(p, "%s does not resist ", wd_he[msex]);

        /* Write the text */
        output_list(p, descs, vn, TERM_L_UMBER, "or ");
        prev = TRUE;
    }

    /* Collect non-effects */
    vn = 0;
    if (rf_has(f, RF_NO_STUN)) descs[vn++] = "stunned";
    if (rf_has(f, RF_NO_FEAR)) descs[vn++] = "frightened";
    if (rf_has(f, RF_NO_CONF)) descs[vn++] = "confused";
    if (rf_has(f, RF_NO_SLEEP)) descs[vn++] = "slept";

    /* Describe non-effects */
    if (vn)
    {
        /* Output connecting text */
        if (prev)
            text_out(p, ", and cannot be ");
        else
            text_out(p, "%s cannot be ", wd_he[msex]);

        output_list(p, descs, vn, TERM_L_UMBER, "or ");
        prev = TRUE;
    }

    /* Full stop. */
    if (prev) text_out(p, ". ");

    /* Do we know how aware it is? */
    if ((((int)l_ptr->wake * (int)l_ptr->wake) > r_ptr->sleep) || (l_ptr->ignore == MAX_UCHAR) ||
        ((r_ptr->sleep == 0) && (l_ptr->pkills >= 10)))
    {
        const char *act;

        if (r_ptr->sleep > 200) act = "prefers to ignore";
        else if (r_ptr->sleep > 95) act = "pays very little attention to";
        else if (r_ptr->sleep > 75) act = "pays little attention to";
        else if (r_ptr->sleep > 45) act = "tends to overlook";
        else if (r_ptr->sleep > 25) act = "takes quite a while to see";
        else if (r_ptr->sleep > 10) act = "takes a while to see";
        else if (r_ptr->sleep > 5) act = "is fairly observant of";
        else if (r_ptr->sleep > 3) act = "is observant of";
        else if (r_ptr->sleep > 1) act = "is very observant of";
        else if (r_ptr->sleep > 0) act = "is vigilant for";
        else act = "is ever vigilant for";

        text_out(p, "%s %s intruders, which %s may notice from ", wd_he[msex], act, wd_helw[msex]);
        text_out_c(p, TERM_L_BLUE, "%d", (cfg_small_range? 5: 10) * r_ptr->aaf);
        text_out(p, " feet. ");
    }

    /* Describe escorts */
    if (flags_test(f, RF_SIZE, RF_ESCORT, RF_ESCORTS, FLAG_END))
        text_out(p, "%s usually appears with escorts. ", wd_he[msex]);

    /* Describe friends */
    else if (flags_test(f, RF_SIZE, RF_FRIEND, RF_FRIENDS, FLAG_END))
        text_out(p, "%s usually appears in groups. ", wd_he[msex]);
}


/*
 * Describes how often the monster has killed/been killed.
 */
static void describe_monster_kills(struct player *p, const monster_race *r_ptr,
    const monster_lore *l_ptr)
{
    bitflag f[RF_SIZE];
    enum monster_sex msex = MON_SEX_NEUTER;
    bool out = TRUE;

    my_assert(r_ptr && l_ptr);

    /* Get the known monster flags */
    monster_flags_known(r_ptr, l_ptr, f);

    /* Extract a gender (if applicable) */
    if (rf_has(r_ptr->flags, RF_FEMALE)) msex = MON_SEX_FEMALE;
    else if (rf_has(r_ptr->flags, RF_MALE)) msex = MON_SEX_MALE;

    /* Treat uniques differently */
    if (rf_has(f, RF_UNIQUE))
    {
        /* A killer... */
        if (l_ptr->tdeaths)
        {
            /* Killed players */
            text_out(p, "%s has slain %d player%s", wd_he[msex], l_ptr->tdeaths,
                PLURAL(l_ptr->tdeaths));

            /* Did we get killed too? */
            if (l_ptr->pdeaths)
                text_out(p, " (including you %d time%s)", l_ptr->pdeaths, PLURAL(l_ptr->pdeaths));

            /* But we've also killed it */
            if (l_ptr->pkills)
                text_out(p, ", but you have taken revenge! ");

            /* Someone else has killed it */
            else if (l_ptr->tkills)
                text_out(p, ", but someone has taken revenge! ");

            /* Unavenged */
            else
                text_out(p, ", who remain%s unavenged. ", plural(l_ptr->tdeaths, "s", ""));
        }

        /* Dead unique (killed by the player) who never killed anyone */
        else if (l_ptr->pkills)
            text_out(p, "You have slain this foe. ");    

        /* Dead unique (killed by someone else) who never killed anyone */
        else if (l_ptr->tkills)
            text_out(p, "Someone has slain this foe. ");

        /* Alive and never killed anyone */
        else
            out = FALSE;
    }

    /* Not unique, but killed players */
    else if (l_ptr->tdeaths)
    {
        /* Killed players */
        text_out(p, "%d %s been killed by this creature",
            l_ptr->tdeaths, plural(l_ptr->tdeaths, "player has", "players have"));

        /* Did we get killed too? */
        if (l_ptr->pdeaths)
            text_out(p, " (including you %d time%s)", l_ptr->pdeaths, PLURAL(l_ptr->pdeaths));

        /* Some kills by the player */
        if (l_ptr->pkills)
            text_out(p, ", and you have exterminated at least %d of the creatures. ", l_ptr->pkills);

        /* Some kills by other players */
        else if (l_ptr->tkills)
        {
            text_out(p, ", and players have exterminated at least %d of the creatures. ",
                l_ptr->tkills);
        }

        /* No kills */
        else
        {
            text_out_c(p, TERM_RED, ", and %s is not ever known to have been defeated. ",
                wd_helw[msex]);
        }
    }

    /* Normal monsters */
    else
    {
        /* Some kills by the player */
        if (l_ptr->pkills)
            text_out(p, "You have killed at least %d of these creatures. ", l_ptr->pkills);

        /* Some kills by other players */
        else if (l_ptr->tkills)
            text_out(p, "Players have killed at least %d of these creatures. ", l_ptr->tkills);

        /* Killed none */
        else
            text_out(p, "No battles to the death are recalled. ");
    }

    /* Separate */
    if (out) text_out(p, "\n\n");
}


/*
 * Describes the AC, HP, and player's chance to hit a monster.
 *
 * Once a player knows the AC of a monster (see know_armour()), he or she
 * also knows the HP and the chance to-hit.
 */
static void describe_monster_toughness(struct player *p, const monster_race *r_ptr,
    const monster_lore *l_ptr)
{
    bitflag f[RF_SIZE];
    enum monster_sex msex = MON_SEX_NEUTER;
    long chance = 0, chance2 = 0;

    my_assert(r_ptr && l_ptr);

    /* Get the known monster flags */
    monster_flags_known(r_ptr, l_ptr, f);

    /* Extract a gender (if applicable) */
    if (rf_has(r_ptr->flags, RF_FEMALE)) msex = MON_SEX_FEMALE;
    else if (rf_has(r_ptr->flags, RF_MALE)) msex = MON_SEX_MALE;

    /* Describe monster "toughness" */
    if (know_armour(r_ptr, l_ptr))
    {
        /* Armor */
        text_out(p, "%s has an armor rating of ", wd_he[msex]);
        text_out_c(p, TERM_L_BLUE, "%d", r_ptr->ac);

        /* Hitpoints */
        text_out(p, ", and a");
        if (!rf_has(f, RF_UNIQUE)) text_out(p, "n average");
        text_out(p, " life rating of ");
        text_out_c(p, TERM_L_BLUE, "%d", r_ptr->avg_hp);
        text_out(p, ". ");

        /*
         * Player's chance to hit it - this code is duplicated in
         * py_attack_real() and test_hit() and must be kept in sync
         */
        chance = (p->state.skills[SKILL_TO_HIT_MELEE] +
            ((p->state.to_h + p->inventory[INVEN_WIELD].to_h) * BTH_PLUS_ADJ));

        /* Avoid division by zero errors, and starting higher on the scale */
        if (chance < 9) chance = 9;

        chance2 = 90 * (chance - (r_ptr->ac * 2 / 3)) / chance + 5;

        /* There is always a 12 percent chance to hit */
        if (chance2 < 12) chance2 = 12;

        text_out(p, "You have a");
        if ((chance2 / 10) == 8) text_out(p, "n");
        text_out_c(p, TERM_L_BLUE, " %d", chance2);
        text_out(p, " percent chance to hit such a creature in melee (if you can see it). ");
    }
}


/*
 * Describes how much experience the player gets for killing this monster,
 * taking the player's level into account.
 */
static void describe_monster_exp(struct player *p, const monster_race *r_ptr,
    const monster_lore *l_ptr)
{
    bitflag f[RF_SIZE];
    const char *pm, *q;
    long i, j;
    char buf[20] = "";

    my_assert(r_ptr && l_ptr);

    /* Get the known monster flags */
    monster_flags_known(r_ptr, l_ptr, f);

    /* Introduction */
    if (rf_has(f, RF_UNIQUE))
        text_out(p, "Killing");
    else
        text_out(p, "A kill of");

    text_out(p, " this creature");

    /* Calculate the integer exp part */
    i = (long)r_ptr->mexp * r_ptr->level / p->lev;

    /*
     * Calculate the fractional exp part scaled by 100, must use long arithmetic
     * to avoid overflow
     */
    j = ((((long)r_ptr->mexp * r_ptr->level % p->lev) * (long)1000 / p->lev + 5) / 10);

    /* Calculate textual representation */
    strnfmt(buf, sizeof(buf), "%ld", (long)i);
    if (j) my_strcat(buf, format(".%02ld", (long)j), sizeof(buf));

    /* Mention the experience */
    text_out(p, " is worth ");
    text_out_c(p, TERM_BLUE, "%s point%s", buf, PLURAL((i == 1) && (j == 0)));

    /* Take account of annoying English */
    pm = "th";
    i = p->lev % 10;
    if ((p->lev / 10) == 1) /* nothing */;
    else if (i == 1) pm = "st";
    else if (i == 2) pm = "nd";
    else if (i == 3) pm = "rd";

    /* Take account of "leading vowels" in numbers */
    q = "";
    i = p->lev;
    if ((i == 8) || (i == 11) || (i == 18)) q = "n";

    /* Mention the dependance on the player's level */
    text_out(p, " for a%s %lu%s level character. ", q, i, pm);
}


/*
 * Describes the type of monster (undead, dragon, etc.) and how quickly
 * and erratically it moves.
 */
static void describe_monster_movement(struct player *p, const monster_race *r_ptr,
    const monster_lore *l_ptr)
{
    bitflag f[RF_SIZE];

    my_assert(r_ptr && l_ptr);

    /* Get the known monster flags */
    monster_flags_known(r_ptr, l_ptr, f);

    text_out(p, "This");

    if (rf_has(r_ptr->flags, RF_ANIMAL)) text_out_c(p, TERM_L_BLUE, " natural");
    if (rf_has(r_ptr->flags, RF_EVIL)) text_out_c(p, TERM_L_BLUE, " evil");
    if (rf_has(r_ptr->flags, RF_UNDEAD)) text_out_c(p, TERM_L_BLUE, " undead");
    if (rf_has(r_ptr->flags, RF_NONLIVING)) text_out_c(p, TERM_L_BLUE, " nonliving");
    if (rf_has(r_ptr->flags, RF_METAL)) text_out_c(p, TERM_L_BLUE, " metal");

    if (rf_has(r_ptr->flags, RF_DRAGON)) text_out_c(p, TERM_L_BLUE, " dragon");
    else if (rf_has(r_ptr->flags, RF_DEMON)) text_out_c(p, TERM_L_BLUE, " demon");
    else if (rf_has(r_ptr->flags, RF_GIANT)) text_out_c(p, TERM_L_BLUE, " giant");
    else if (rf_has(r_ptr->flags, RF_TROLL)) text_out_c(p, TERM_L_BLUE, " troll");
    else if (rf_has(r_ptr->flags, RF_ORC)) text_out_c(p, TERM_L_BLUE, " orc");
    else text_out_c(p, TERM_L_BLUE, " creature");

    /* Describe location */
    if (r_ptr->level == 0) text_out(p, " lives in the town");
    else
    {
        byte colour = (r_ptr->level > p->max_depth)? TERM_RED: TERM_L_BLUE;

        if (rf_has(f, RF_FORCE_DEPTH))
            text_out(p, " is found ");
        else
            text_out(p, " is normally found ");

        text_out(p, "at depths of ");
        text_out_c(p, colour, "%d", r_ptr->level * 50);
        text_out(p, " feet (level ");
        text_out_c(p, colour, "%d", r_ptr->level);
        text_out(p, ")");
    }

    text_out(p, ", and moves");

    /* Random-ness */
    if (flags_test(f, RF_SIZE, RF_RAND_50, RF_RAND_25, FLAG_END))
    {
        /* Adverb */
        if (rf_has(f, RF_RAND_50) && rf_has(f, RF_RAND_25))
            text_out(p, " extremely");
        else if (rf_has(f, RF_RAND_50))
            text_out(p, " somewhat");
        else if (rf_has(f, RF_RAND_25))
            text_out(p, " a bit");

        /* Adjective */
        text_out(p, " erratically");

        /* Hack -- Occasional conjunction */
        if (r_ptr->speed != 110) text_out(p, ", and");
    }

    /* Speed */
    if (r_ptr->speed > 110)
    {
        if (r_ptr->speed > 130) text_out_c(p, TERM_GREEN, " incredibly");
        else if (r_ptr->speed > 120) text_out_c(p, TERM_GREEN, " very");
        text_out_c(p, TERM_GREEN, " quickly");
    }
    else if (r_ptr->speed < 110)
    {
        if (r_ptr->speed < 90) text_out_c(p, TERM_GREEN, " incredibly");
        else if (r_ptr->speed < 100) text_out_c(p, TERM_GREEN, " very");
        text_out_c(p, TERM_GREEN, " slowly");
    }
    else
    {
        text_out(p, " at ");
        text_out_c(p, TERM_GREEN, "normal speed");
    }

    /* The code above includes "attack speed" */
    if (rf_has(f, RF_NEVER_MOVE))
    {
        text_out(p, ", but ");
        text_out_c(p, TERM_L_GREEN, "does not deign to chase intruders");
    }

    /* End this sentence */
    text_out(p, ". ");
}


/*
 * Learn everything about a monster (by cheating).
 *
 * Sets the number of total kills of a monster to MAX_SHORT, so that the
 * player knows the armor etc. of the monster. Sets the number of observed
 * blows to MAX_UCHAR for each blow. Sets the number of observed drops
 * to the maximum possible. The player also automatically learns every
 * monster flag.
 */
static void cheat_monster_lore(const monster_race *r_ptr, monster_lore *l_ptr)
{
    int i;

    my_assert(r_ptr);
    my_assert(l_ptr);

    /* Hack -- Maximal kills */
    l_ptr->tkills = MAX_SHORT;

    /* Hack -- Maximal info */
    l_ptr->wake = l_ptr->ignore = MAX_UCHAR;

    /* Observe "maximal" attacks */
    for (i = 0; i < MONSTER_BLOW_MAX; i++)
    {
        /* Examine "actual" blows */
        if (r_ptr->blow[i].effect || r_ptr->blow[i].method)
        {
            /* Hack -- Maximal observations */
            l_ptr->blows[i] = MAX_UCHAR;
        }
    }

    /* Hack -- Maximal drops */
    l_ptr->drop_item = 0;
    if (rf_has(r_ptr->flags, RF_DROP_20)) l_ptr->drop_item++;
    if (rf_has(r_ptr->flags, RF_DROP_40)) l_ptr->drop_item++;
    if (rf_has(r_ptr->flags, RF_DROP_60)) l_ptr->drop_item++;

    if (rf_has(r_ptr->flags, RF_DROP_4)) l_ptr->drop_item += 6;
    if (rf_has(r_ptr->flags, RF_DROP_3)) l_ptr->drop_item += 4;
    if (rf_has(r_ptr->flags, RF_DROP_2)) l_ptr->drop_item += 3;
    if (rf_has(r_ptr->flags, RF_DROP_1)) l_ptr->drop_item++;

    l_ptr->drop_gold = l_ptr->drop_item;

    /* Hack -- But only "valid" drops */
    if (rf_has(r_ptr->flags, RF_ONLY_GOLD)) l_ptr->drop_item = 0;
    if (rf_has(r_ptr->flags, RF_ONLY_ITEM)) l_ptr->drop_gold = 0;

    /* Hack -- Observe many spells */
    l_ptr->cast_innate = MAX_UCHAR;
    l_ptr->cast_spell = MAX_UCHAR;

    /* Hack -- Know all the flags */
    rf_setall(l_ptr->flags);
    rsf_copy(l_ptr->spell_flags, r_ptr->spell_flags);
}


/*
 * Learn everything about a monster
 */
void get_global_lore(struct player *p, int r_idx, monster_lore* l_ptr)
{
    /* Get the race and lores (player + global) */
    const monster_race *r_ptr = &r_info[r_idx];
    monster_lore *lp_ptr = &p->lore[r_idx];
    const monster_lore *lm_ptr = &r_ptr->lore;

    /* Hack -- Create a copy of the monster memory (player) */
    COPY(l_ptr, lp_ptr, monster_lore);

    /* Add the global monster memory */
    l_ptr->spawned = lm_ptr->spawned;
    l_ptr->seen = lm_ptr->seen;
    l_ptr->tdeaths = lm_ptr->tdeaths;
    l_ptr->tkills = lm_ptr->tkills;

    /* Assume some "obvious" flags */
    flags_set(l_ptr->flags, RF_SIZE, RF_OBVIOUS_MASK, FLAG_END);

    /* Killing a monster reveals some properties */
    if (l_ptr->pkills)
    {
        /* Know "race" flags */
        flags_set(l_ptr->flags, RF_SIZE, RF_RACE_MASK, FLAG_END);

        /* Know "forced" flags */
        rf_on(l_ptr->flags, RF_FORCE_DEPTH);
    }

    /* DM has perfect monster lore */
    if (is_dm_p(p)) cheat_monster_lore(r_ptr, l_ptr);
}


static void describe_monster_name(struct player *p, const monster_race *r_ptr)
{
    byte a1, a2;
    char c1, c2;

    my_assert(r_ptr);

    /* Get the chars */
    c1 = r_ptr->d_char;
    c2 = r_ptr->x_char;

    /* Get the attrs */
    a1 = r_ptr->d_attr;
    a2 = r_ptr->x_attr;

    /* A title (use "The" for non-uniques) */
    if (!rf_has(r_ptr->flags, RF_UNIQUE)) text_out_c(p, TERM_WHITE, "The ");
    else if (OPT_P(p, purple_uniques))
    {
        a1 = TERM_VIOLET;
        if (!(a2 & 0x80)) a2 = TERM_VIOLET;
    }

    /* Dump the name */
    text_out_c(p, TERM_WHITE, r_ptr->name);

    /* Append the "standard" attr/char info */
    text_out_c(p, TERM_WHITE, " ('");
    text_out_c(p, a1, "%c", c1);
    text_out_c(p, TERM_WHITE, "')");

    /* Append the "optional" attr/char info */
    if ((a2 != a1) || (c2 != c1))
    {
        text_out_c(p, TERM_WHITE, "/('");
        text_out_c(p, a2, "%c", c2);
        text_out_c(p, TERM_WHITE, "'):");
    }

    text_out(p, "\n\n");
}


void monster_info_screen(struct player *p, int r_idx)
{
    monster_lore lore;
    bitflag f[RF_SIZE];
    int melee_colors[RBE_MAX], spell_colors[RSF_MAX];
    const monster_race *r_ptr;

    /* Paranoia */
    if (!r_idx) return;

    /* Get the race */
    r_ptr = &r_info[r_idx];

    /* Determine the special attack colors */
    get_attack_colors(p, melee_colors, spell_colors);

    /* Get the lores (player + global) */
    get_global_lore(p, r_idx, &lore);

    /* Now get the known monster flags */
    monster_flags_known(r_ptr, &lore, f);

    /* Monster name */
    describe_monster_name(p, r_ptr);

    /* Show kills of monster vs. player(s) */
    describe_monster_kills(p, r_ptr, &lore);

    /* Monster description */
    describe_monster_desc(p, r_ptr);

    /* Describe the monster type, speed, life, and armor */
    describe_monster_movement(p, r_ptr, &lore);
    describe_monster_toughness(p, r_ptr, &lore);

    /* Describe the experience and item reward when killed */
    describe_monster_exp(p, r_ptr, &lore);
    describe_monster_drop(p, r_ptr, &lore);

    /* Describe the special properties of the monster */
    describe_monster_abilities(p, r_ptr, &lore);

    /* Describe the spells, spell-like abilities and melee attacks */
    describe_monster_spells(p, r_ptr, &lore, spell_colors);
    describe_monster_attack(p, r_ptr, &lore, melee_colors);

    /* Notice "Quest" monsters */
    if (rf_has(r_ptr->flags, RF_QUESTOR))
        text_out(p, "You feel an intense desire to kill this monster... ");
}


/*
 * Display monster information, using text_out()
 */
void describe_monster(struct player *p, int r_idx)
{
    /* Let the player scroll through this info */
    p->special_file_type = SPECIAL_FILE_OTHER;

    /* Prepare player structure for text */
    text_out_init(p);

    /* Dump info into player */
    monster_info_screen(p, r_idx);

    /* Restore height and width of current dungeon level */
    text_out_done(p);
}


/*
 * Learn about a monster (by "probing" it)
 */
void lore_do_probe(struct player *p, int m_idx)
{
    monster_type *m_ptr;
    monster_race *r_ptr;
    monster_lore *l_ptr;
    unsigned i;

    my_assert(m_idx > 0);

    m_ptr = cave_monster(cave_get(p->depth), m_idx);
    r_ptr = &r_info[m_ptr->r_idx];
    l_ptr = &p->lore[m_ptr->r_idx];

    /* Know various things */
    rf_setall(l_ptr->flags);
    rsf_copy(l_ptr->spell_flags, r_ptr->spell_flags);
    for (i = 0; i < MONSTER_BLOW_MAX; i++) l_ptr->blows[i] = MAX_UCHAR;

    /* Update monster recall window */
    if (p->monster_race_idx == m_ptr->r_idx) p->redraw |= (PR_MONSTER);
}


/*
 * Take note that the given monster just dropped some treasure
 *
 * Note that learning the "GOOD"/"GREAT" flags gives information
 * about the treasure (even when the monster is killed for the first
 * time, such as uniques, and the treasure has not been examined yet).
 *
 * This "indirect" method is used to prevent the player from learning
 * exactly how much treasure a monster can drop from observing only
 * a single example of a drop.  This method actually observes how much
 * gold and items are dropped, and remembers that information to be
 * described later by the monster recall code.
 */
void lore_treasure(int Ind, struct monster *m_ptr, int num_item, int num_gold)
{
    player_type *p_ptr = player_get(Ind);
    monster_lore *l_ptr = &p_ptr->lore[m_ptr->r_idx];

    my_assert(num_item >= 0);
    my_assert(num_gold >= 0);

    /* Note the number of things dropped */
    if (num_item > l_ptr->drop_item) l_ptr->drop_item = num_item;
    if (num_gold > l_ptr->drop_gold) l_ptr->drop_gold = num_gold;

    /* Learn about drop quality */
    rf_on(l_ptr->flags, RF_DROP_GOOD);
    rf_on(l_ptr->flags, RF_DROP_GREAT);

    /* Update monster recall window */
    if (p_ptr->monster_race_idx == m_ptr->r_idx)
    {
        /* Redraw */
        p_ptr->redraw |= (PR_MONSTER);
    }
}


/*
 * Copies into `flags` the flags of the given monster race that are known
 * to the given lore structure (usually the player's knowledge).
 *
 * Known flags will be 1 for present, or 0 for not present. Unknown flags
 * will always be 0.
 */
void monster_flags_known(const monster_race *r_ptr, const monster_lore *l_ptr, bitflag flags[RF_SIZE])
{
    rf_copy(flags, r_ptr->flags);
    rf_inter(flags, l_ptr->flags);
}