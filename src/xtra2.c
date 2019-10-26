/* File: xtra2.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies. Other copyrights may also apply.
 */

/* Purpose: effects of various "objects" */

#include "angband.h"

#include <assert.h>

#define REWARD_CHANCE 10
#define OLYMPIAN_CHANCE 20 /* Olympians are now a bit easier */

 /* Experience required to advance from level to level + 1
    Note the table is off by 1, so we encapsulate that fact.
    (e.g. table[0] gives xp needed to go from L1 to L2!)
    Also, the old code (cut and pasted multiple places) would
    overflow once expfact got too high which is easily avoided.
    Finally, Androids do it all differently!

    To be sure everybody is doing it correctly, I moved the tables
    here and renamed them!
 */
static s32b _player_exp[PY_MAX_LEVEL] =
{
    10,
    25,
    45,
    70,
    100,
    140,
    200,
    280,
    380,/*10*/
    500,
    650,
    850,
    1100,
    1400,
    1800,
    2300,
    2900,
    3600,
    4400,/*20*/
    5400,
    6800,
    8400,
    10200,
    12500,
    17500,
    25000,
    35000,
    50000,
    75000,/*30*/
    100000,
    150000,
    200000,
    275000,
    350000,
    450000,
    550000,
    700000,
    850000,
    1000000,/*40*/
    1250000,
    1500000,
    1800000,
    2100000,
    2400000,
    2700000,
    3000000,
    3500000,
    4000000,
    4500000,/*50*/
    5000000
};


static s32b _player_exp_a[PY_MAX_LEVEL] =
{
    20,
    50,
    100,
    170,
    280,
    430,
    650,
    950,
    1400,/*10*/
    1850,
    2300,
    2900,
    3600,
    4400,
    5400,
    6800,
    8400,
    10400,
    12500,/*20*/
    17500,
    25000,
    35000,
    50000,
    75000,
    100000,
    150000,
    200000,
    275000,
    350000,/*30*/
    450000,
    550000,
    650000,
    800000,
    950000,
    1100000,
    1250000,
    1400000,
    1550000,
    1700000,/*40*/
    1900000,
    2100000,
    2300000,
    2550000,
    2800000,
    3050000,
    3300000,
    3700000,
    4100000,
    4500000,/*50*/
    5000000
};

void export_exp_table(FILE* fp)
{
    int i;
    fputs("Lvl,Exp,Android\n", fp);
    fputs("1,0,0\n", fp);
    for (i = 0; i <= 48; i++) /* entry i is experience required for level i+2 */
    {
        fprintf(fp, "%d,%d,%d\n", i+2, _player_exp[i], _player_exp_a[i]);
    }
}

int exp_requirement(int level) /* Ugh ... return experience required to reach level + 1, which is table[level - 1]! */
{
    bool android = (p_ptr->prace == RACE_ANDROID ? TRUE : FALSE);
    int base = (android ? _player_exp_a : _player_exp)[level-1];
    if (base % 100 == 0)
        return base / 100 * p_ptr->expfact;
    else
        return base * p_ptr->expfact / 100;
}

void gain_chosen_stat(void)
{
    int choice;
    if (statistics_hack)
        return;
    screen_save();
    while(1)
    {
        int n;
        char tmp[32];

        cnv_stat(p_ptr->stat_max[0], tmp);
        put_str(format("        a) Str (cur %6.6s)              ", tmp), 2, 14);
        cnv_stat(p_ptr->stat_max[1], tmp);
        put_str(format("        b) Int (cur %6.6s)              ", tmp), 3, 14);
        cnv_stat(p_ptr->stat_max[2], tmp);
        put_str(format("        c) Wis (cur %6.6s)              ", tmp), 4, 14);
        cnv_stat(p_ptr->stat_max[3], tmp);
        put_str(format("        d) Dex (cur %6.6s)              ", tmp), 5, 14);
        cnv_stat(p_ptr->stat_max[4], tmp);
        put_str(format("        e) Con (cur %6.6s)              ", tmp), 6, 14);
        cnv_stat(p_ptr->stat_max[5], tmp);
        put_str(format("        f) Chr (cur %6.6s)              ", tmp), 7, 14);
        put_str("                                         ", 8, 14);
        c_put_str(TERM_YELLOW, "        Which stat do you want to raise? ", 1, 14);

        while(1)
        {
            choice = inkey();
            if ((choice >= 'a') && (choice <= 'f')) break;
        }
        for(n = 0; n < 6; n++)
        {
            if (n != choice - 'a')
                put_str("                                         ", n+2, 14);
        }
        if (get_check("Are you sure? ")) break;
    }
    do_inc_stat(choice - 'a');
    screen_load();
}

/*
 * Advance experience levels and print experience
 */

void check_experience(void)
{
    int  old_lev = p_ptr->lev;

    /* Hack -- lower limit */
    if (p_ptr->exp < 0) p_ptr->exp = 0;
    if (p_ptr->max_exp < 0) p_ptr->max_exp = 0;
    if (p_ptr->max_max_exp < 0) p_ptr->max_max_exp = 0;

    /* Hack -- upper limit */
    if (p_ptr->exp > PY_MAX_EXP) p_ptr->exp = PY_MAX_EXP;
    if (p_ptr->max_exp > PY_MAX_EXP) p_ptr->max_exp = PY_MAX_EXP;
    if (p_ptr->max_max_exp > PY_MAX_EXP) p_ptr->max_max_exp = PY_MAX_EXP;

    /* Hack -- maintain "max" experience */
    if (p_ptr->exp > p_ptr->max_exp) p_ptr->max_exp = p_ptr->exp;

    /* Hack -- maintain "max max" experience */
    if (p_ptr->max_exp > p_ptr->max_max_exp) p_ptr->max_max_exp = p_ptr->max_exp;

    /* Redraw experience */
    p_ptr->redraw |= (PR_EXP);

    /* Lose levels while possible */
    while ((p_ptr->lev > 1) &&
           (p_ptr->exp < exp_requirement(p_ptr->lev - 1)))
    {
        p_ptr->lev--;
        p_ptr->update |= (PU_BONUS | PU_INNATE | PU_HP | PU_MANA | PU_SPELLS);
        p_ptr->redraw |= (PR_LEV);
    }

    /* Gain levels while possible */
    while ((p_ptr->lev < PY_MAX_LEVEL) &&
           (p_ptr->exp >= exp_requirement(p_ptr->lev)))
    {
        p_ptr->lev++;

        if (p_ptr->pclass == CLASS_WILD_TALENT) wild_talent_fix_up();

        /* Save the highest level */
        if (p_ptr->lev > p_ptr->max_plv)
        {
            class_t *class_ptr = get_class();

            p_ptr->max_plv = p_ptr->lev;

            sound(SOUND_LEVEL);
            cmsg_format(TERM_L_GREEN, "Welcome to level %d.", p_ptr->lev);

            if (class_ptr->hooks.gain_level != NULL)
                (class_ptr->hooks.gain_level)(p_ptr->lev);

            if (mut_present(MUT_CHAOS_GIFT))
                chaos_warrior_reward();

            /* N.B. The class hook or the Chaos Gift mutation may result in a race
               change (stupid Chaos-Warriors), so we better always requery the player's
               race to make sure the correct racial hook is called. */
            {
                race_t *race_ptr = get_true_race(); /* So players don't miss if they Polymorph Demon, etc */

                if (p_ptr->prace == RACE_DOPPELGANGER) /* But a doppelganger should use the mimicked race! */
                    race_ptr = get_race();

                if (race_ptr->hooks.gain_level != NULL)
                    (race_ptr->hooks.gain_level)(p_ptr->lev);
            }
            if(p_ptr->max_plv % 5 == 0)
                gain_chosen_stat();
        }
        p_ptr->update |= (PU_BONUS | PU_INNATE | PU_HP | PU_MANA | PU_SPELLS);
        p_ptr->redraw |= (PR_LEV);
        p_ptr->window |= (PW_SPELL);
    }

    if (old_lev != p_ptr->lev)
    {
        race_t *race_ptr = get_true_race(); /* So players don't miss if they Polymorph Demon, etc */

        if (p_ptr->prace == RACE_DOPPELGANGER) /* But a doppelganger should use the mimicked race! */
            race_ptr = get_race();

        if (race_ptr->hooks.change_level)
            race_ptr->hooks.change_level(old_lev, p_ptr->lev);

        autopick_load_pref(FALSE);
    }
    handle_stuff(); /* XXX only do this once at the end */
}


/*
 * Return monster death string
 */
cptr extract_note_dies(monster_race *r_ptr)
{
    if (!monster_living(r_ptr))
    {
        if (mon_blows_find(r_ptr->blows, RBM_EXPLODE))
            return " explodes into tiny shreds.";
        return " is destroyed.";
    }
    return " dies.";
}

static bool _mon_is_wanted(mon_ptr mon)
{
    mon_race_ptr race = mon_race(mon);
    if ((race->flags1 & RF1_UNIQUE) && !mon_is_cloned(mon))
    {
        int i;
        for (i = 0; i < MAX_KUBI; i++)
        {
            if (kubi_r_idx[i] == mon->r_idx && !(mon->mflag2 & MFLAG2_CHAMELEON))
            {
                return TRUE;
            }
        }
    }
    return FALSE;
}

static bool _kind_is_basic(int k_idx)
{
    object_kind *k_ptr = &k_info[k_idx];
    switch (k_ptr->tval)
    {
    case TV_FOOD:
        switch (k_ptr->sval)
        {
        case SV_FOOD_RATION:
        case SV_FOOD_WAYBREAD:
            return TRUE;
        }
        break;

    case TV_SCROLL:
        switch (k_ptr->sval)
        {
        case SV_SCROLL_TELEPORT:
        case SV_SCROLL_WORD_OF_RECALL:
            return TRUE;
        }
        break;

    case TV_FLASK:
        switch (k_ptr->sval)
        {
        case SV_FLASK_OIL:
            return (cave->dun_lvl < 15) ? TRUE : FALSE;
        }
        break;

    case TV_LITE:
        switch (k_ptr->sval)
        {
        case SV_LITE_TORCH:
        case SV_LITE_LANTERN:
            return (cave->dun_lvl < 15) ? TRUE : FALSE;
        }
        break;

    case TV_DIGGING:
        switch (k_ptr->sval)
        {
        case SV_SHOVEL:
        case SV_PICK:
            return (cave->dun_lvl < 15) ? TRUE : FALSE;
        }
        break;
    }
    return FALSE;
}

static bool _kind_is_stat_potion(int k_idx)
{
    object_kind *k_ptr = &k_info[k_idx];
    switch (k_ptr->tval)
    {
    case TV_POTION:
        switch (k_ptr->sval)
        {
        case SV_POTION_INC_STR:
        case SV_POTION_INC_INT:
        case SV_POTION_INC_WIS:
        case SV_POTION_INC_DEX:
        case SV_POTION_INC_CON:
        case SV_POTION_INC_CHR:
            return TRUE;
        }
        break;
    }
    return FALSE;
}

static bool _kind_is_utility(int k_idx)
{
    object_kind *k_ptr = &k_info[k_idx];
    switch (k_ptr->tval)
    {
    case TV_SCROLL:
        switch (k_ptr->sval)
        {
        case SV_SCROLL_TELEPORT:
        case SV_SCROLL_PHASE_DOOR:
        case SV_SCROLL_WORD_OF_RECALL:
        case SV_SCROLL_IDENTIFY:
        case SV_SCROLL_REMOVE_CURSE:
        case SV_SCROLL_STAR_REMOVE_CURSE:
        case SV_SCROLL_MAPPING:
        /* case SV_SCROLL_PROTECTION_FROM_EVIL: XXX This was a bad idea! */
        case SV_SCROLL_DETECT_MONSTERS:
            return TRUE;
        case SV_SCROLL_STAR_IDENTIFY:
            return TRUE;
        }
        break;

    case TV_POTION:
        switch (k_ptr->sval)
        {
        case SV_POTION_SPEED:
        case SV_POTION_RESIST_HEAT:
        case SV_POTION_RESIST_COLD:
        case SV_POTION_RESISTANCE:
        case SV_POTION_HEROISM:
        case SV_POTION_CURE_SERIOUS:
        case SV_POTION_CURE_CRITICAL:
        case SV_POTION_CURING:
        case SV_POTION_RESTORE_EXP:
        case SV_POTION_RES_STR:
        case SV_POTION_RES_INT:
        case SV_POTION_RES_WIS:
        case SV_POTION_RES_DEX:
        case SV_POTION_RES_CON:
        case SV_POTION_RES_CHR:
        case SV_POTION_CLARITY:
            return TRUE;
        }
        break;
    }
    return FALSE;
}


/*
 * Handle the "death" of a monster.
 *
 * Disperse treasures centered at the monster location based on the
 * various flags contained in the monster flags fields.
 *
 * Check for "Quest" completion when a quest monster is killed.
 *
 * Note that only the player can induce "monster_death()" on Uniques.
 * Thus (for now) all Quest monsters should be Uniques.
 *
 * Note that monsters can now carry objects, and when a monster dies,
 * it drops all of its objects, which may disappear in crowded rooms.
 */
void monster_death(mon_ptr m_ptr, bool drop_item)
{
    int i;

    int dump_item = 0;
    int dump_gold = 0;

    monster_race *r_ptr = mon_race(m_ptr);

    bool visible = ((m_ptr->ml && !plr_tim_find(T_HALLUCINATE)) || (r_ptr->flags1 & RF1_UNIQUE));

    bool cloned = (m_ptr->smart & (1U << SM_CLONED)) ? TRUE : FALSE;
    bool do_vampire_servant = FALSE;
    char m_name[MAX_NLEN];
    int corpse_chance = 3;

    object_type forge;
    object_type *q_ptr;

    bool drop_chosen_item = drop_item && !cloned && !is_pet(m_ptr);


    monster_desc(m_name, m_ptr, MD_TRUE_NAME);

    /* The caster is dead? */
    if (world_monster && world_monster == m_ptr->id) world_monster = 0;

    /* Notice changes in view */
    if (r_ptr->flags7 & (RF7_LITE_MASK | RF7_DARK_MASK))
    {
        p_ptr->update |= (PU_MON_LITE);
    }

    if (mut_present(MUT_INFERNAL_DEAL) && plr_los(m_ptr->pos) && !is_pet(m_ptr))
    {
        if ( p_ptr->msp > 0
          && p_ptr->pclass != CLASS_RUNE_KNIGHT
          && p_ptr->pclass != CLASS_SAMURAI
          && p_ptr->pclass != CLASS_MYSTIC )
        {
            hp_player_aux(10);
            sp_player(5);
        }
        else
            hp_player_aux(15);
    }

    if (r_ptr->flags2 & RF2_MULTIPLY)
        cave->breed_kill_ct++;

    /* Let monsters explode! */
    {mon_blow_ptr blow = mon_blows_find(r_ptr->blows, RBM_EXPLODE);
    if (blow && blow->effect_ct)
    {
        int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;
        int typ = blow->effects[0].effect;
        int damage = dice_roll(blow->effects[0].dice);

        project(m_ptr->id, 3, m_ptr->pos.y, m_ptr->pos.x, damage, typ, flg);
    }}

    if (m_ptr->mflag2 & MFLAG2_CHAMELEON)
    {
        choose_new_monster(m_ptr, TRUE, MON_CHAMELEON);
        r_ptr = mon_race(m_ptr);
    }

    mon_tim_clear(m_ptr);
    quests_on_kill_mon(m_ptr);
    plr_hook_kill_monster(m_ptr);

    if (m_ptr->id == p_ptr->riding)
    {
        if (rakuba(-1, FALSE))
        {
            msg_print("You have fallen from your riding pet.");
        }
    }

    if (p_ptr->prace == RACE_MON_MIMIC && !(m_ptr->smart & SM_CLONED))
        mimic_on_kill_monster(m_ptr->r_idx);

    if ( vampiric_drain_hack
      && (r_ptr->flags2 & RF2_HUMAN)
      && !is_pet(m_ptr)
      && randint1(p_ptr->lev) >= 15 )
    {
        do_vampire_servant = TRUE;
    }

    /* Drop a dead corpse? */
    if (p_ptr->prace == RACE_MON_POSSESSOR && p_ptr->current_r_idx == MON_POSSESSOR_SOUL)
        corpse_chance = 2;

    if ( (_mon_is_wanted(m_ptr) || (one_in_(corpse_chance) && !do_vampire_servant))
      && (r_ptr->flags9 & (RF9_DROP_CORPSE | RF9_DROP_SKELETON))
      && !(cloned || (m_ptr->r_idx == today_mon && is_pet(m_ptr))))
    {
        /* Assume skeleton */
        bool corpse = FALSE;

        do_vampire_servant = FALSE;

        /*
         * We cannot drop a skeleton? Note, if we are in this check,
         * we *know* we can drop at least a corpse or a skeleton
         */
        if (!(r_ptr->flags9 & RF9_DROP_SKELETON))
            corpse = TRUE;
        else if ((r_ptr->flags9 & RF9_DROP_CORPSE) && _mon_is_wanted(m_ptr))
            corpse = TRUE;
        else if ( (r_ptr->flags9 & RF9_DROP_CORPSE)
               && p_ptr->prace == RACE_MON_POSSESSOR
               && p_ptr->current_r_idx == MON_POSSESSOR_SOUL )
        {
            corpse = TRUE;
        }
        /* Else, a corpse is more likely unless we did a "lot" of damage */
        else if (r_ptr->flags9 & RF9_DROP_CORPSE)
        {
            /* Lots of damage in one blow */
            if ((0 - ((m_ptr->maxhp) / 4)) > m_ptr->hp)
            {
                if (one_in_(4)) corpse = TRUE;
            }
            else
            {
                if (!one_in_(5)) corpse = TRUE;
            }
        }

        /* Get local object */
        q_ptr = &forge;

        /* Prepare to make an object
         * XXX don't apply_magic ... we know which monster to use. cf a_m_aux_4 */
        object_prep(q_ptr, lookup_kind(TV_CORPSE, (corpse ? SV_CORPSE : SV_SKELETON)));
        q_ptr->pval = m_ptr->r_idx;
        q_ptr->ident |= IDENT_KNOWN;
        if (r_ptr->weight && p_ptr->prace == RACE_MON_POSSESSOR)
        {
            /* Note: object_type.weight is an s16b and stores decipounds.
                     monster_race.weight is an s16b and stores pounds.
                     Thus, we might overflow on coversion! */
            if (corpse)
                q_ptr->weight = MIN(500*10, r_ptr->weight * 10);
            else
                q_ptr->weight = MIN(500*10, r_ptr->weight * 10 / 3);
        }

        /* Drop it in the dungeon */
        drop_near(q_ptr, m_ptr->pos, -1);
    }

    /* Drop objects being carried */
    dun_mon_drop_carried_obj(cave, m_ptr);

    switch (m_ptr->r_idx)
    {
    case MON_PINK_HORROR:
        /* Pink horrors are replaced with 2 Blue horrors */
        {
            bool notice = FALSE;

            for (i = 0; i < 2; i++)
            {
                bool pet = is_pet(m_ptr);
                u32b mode = 0L;

                if (pet) mode |= PM_FORCE_PET;

                if (summon_specific((pet ? -1 : m_ptr->id), m_ptr->pos, 20, SUMMON_BLUE_HORROR, mode))
                {
                    if (plr_can_see(m_ptr->pos))
                        notice = TRUE;
                }
            }

            if (notice)
                msg_print("The Pink horror divides!");
        }
        break;

    case MON_VARIANT_MAINTAINER:
    {
        bool notice = FALSE;

        for (i = 0; i < 4; i++)
        {
            if (summon_specific(m_ptr->id, m_ptr->pos, 14, SUMMON_SOFTWARE_BUG, 0))
            {
                if (plr_can_see(m_ptr->pos))
                    notice = TRUE;
            }
        }

        if (notice)
            msg_print("The Variant Maintainer is dead, but his crappy code remains!");
        break;
    }
    case MON_DAWN:
        /*
         * Mega^3-hack: killing a 'Warrior of the Dawn' is likely to
         * spawn another in the fallen one's place!
         */
        if (!one_in_(7))
        {
            point_t p;
            int attempts = 100;
            bool pet = is_pet(m_ptr);

            do
            {
                p = scatter(m_ptr->pos, 20);
            }
            while (!(dun_pos_interior(cave, p) && cave_empty_at(p)) && --attempts);

            if (attempts > 0)
            {
                u32b mode = 0L;
                if (pet) mode |= PM_FORCE_PET;

                if (summon_specific((pet ? -1 : m_ptr->id), p, 40, SUMMON_DAWN, mode))
                {
                    if (plr_can_see(p)) msg_print("A new warrior steps forth!");
                }
            }
        }
        break;

    case MON_UNMAKER:
        /* One more ultra-hack: An Unmaker goes out with a big bang! */
        {
            int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;
            project(m_ptr->id, 6, m_ptr->pos.y, m_ptr->pos.x, 100, GF_CHAOS, flg);
        }
        break;

    case MON_A_SILVER:
        /* XXX O:20%:OBJ(can of toys)
         * XXX No way to do exactly 1 in 5, though ... */
        if (drop_chosen_item && r_ptr->r_akills % 5 == 0)
        {
            /* Get local object */
            q_ptr = &forge;

            /* Prepare to make a Can of Toys */
            object_prep(q_ptr, lookup_kind(TV_CHEST, SV_CHEST_KANDUME));

            apply_magic(q_ptr, cave->difficulty, AM_NO_FIXED_ART);

            /* Drop it in the dungeon */
            drop_near(q_ptr, m_ptr->pos, -1);
        }
        break;

    case MON_ROLENTO:
        {
            int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;
            project(m_ptr->id, 3, m_ptr->pos.y, m_ptr->pos.x, damroll(20, 10), GF_FIRE, flg);
        }
        break;
    }

    if (drop_chosen_item && (m_ptr->mflag2 & MFLAG2_DROP_MASK))
    {
        int k_idx;
        int mode = 0;
        object_type forge;

        if (m_ptr->mflag2 & MFLAG2_DROP_PRIZE)
        {
            if (cave->dun_lvl >= 30 && cave->dun_lvl <= 60  && one_in_(3))
                get_obj_num_hook = _kind_is_stat_potion;
            else
            {
                if (cave->dun_lvl >= 20 && one_in_(3))
                    mode |= AM_GREAT;
                else
                    mode |= AM_GOOD;

                if (one_in_(3))
                    mode |= AM_TAILORED;
            }
        }
        else if (m_ptr->mflag2 & MFLAG2_DROP_UTILITY)
            get_obj_num_hook = _kind_is_utility;
        else if (m_ptr->mflag2 & MFLAG2_DROP_BASIC)
            get_obj_num_hook = _kind_is_basic;

        if (get_obj_num_hook) get_obj_num_prep();
        k_idx = get_obj_num(cave->difficulty);
        if (get_obj_num_hook)
        {
            get_obj_num_hook = NULL;
            get_obj_num_prep();
        }

        object_prep(&forge, k_idx);
        if (!apply_magic(&forge, cave->difficulty, mode) && obj_is_device(&forge))
            apply_magic(&forge, cave->difficulty, 0);
        obj_make_pile(&forge);
        drop_near(&forge, m_ptr->pos, -1);
    }

    /* Mega-Hack -- drop fixed items
     * XXX These are now specified directly in ../lib/edit/r_info.txt
     * XXX We still need to grant dungeon guardian rewards, though. */
    if (drop_chosen_item)
    {
        race_t *race_ptr = get_race();

        if (race_ptr->boss_r_idx && race_ptr->boss_r_idx == m_ptr->r_idx && p_ptr->pclass == CLASS_MONSTER)
        {
            msg_print("Congratulations! You have killed the boss of your race!");
            p_ptr->fame += 10;
            p_ptr->update |= PU_BONUS; /* Player is now a "Hero" */
            p_ptr->redraw |= PR_STATUS;

            /* Centipedes can only take the final evolutionary step if the boss is dead */
            if (p_ptr->prace == RACE_MON_CENTIPEDE && p_ptr->lev >= 35)
                race_ptr->hooks.gain_level(p_ptr->lev);

            msg_add_tiny_screenshot(50, 24);
        }
    }

    if (drop_item || r_ptr->d_char == '$')
    {
        vec_ptr drops = mon_drop_make(m_ptr);
        int     i;

        for (i = 0; i < vec_length(drops); i++)
        {
            obj_ptr drop = vec_get(drops, i);
            if (!drop) continue; /* paranoia */
            if (drop->tval == TV_GOLD)
                dump_gold++;
            else
                dump_item++;
            drop_near(drop, m_ptr->pos, -1);
        }

        vec_free(drops);
    }

    if (visible && (dump_item || dump_gold))
        lore_treasure(m_ptr->id, dump_item, dump_gold);

    if (do_vampire_servant)
    {
        int r_idx = MON_VAMPIRE;
        int r_lvl = r_ptr->level;
        int mode = PM_FORCE_PET;

        if (!one_in_(3))
        {
            mode = PM_FORCE_FRIENDLY;
            cmsg_format(TERM_RED, "%^s is transformed in undeath!", m_name);
        }
        else
            cmsg_format(TERM_RED, "%^s rises to serve you!", m_name);

        if (r_ptr->flags1 & RF1_UNIQUE)
            r_lvl += 10;

        if (r_lvl >= 30)
            r_idx = MON_MASTER_VAMPIRE;
        if (r_lvl >= 40 && one_in_(2))
            r_idx = MON_VAMPIRE_LORD;
        if (r_lvl >= 60 && one_in_(3))
            r_idx = MON_ELDER_VAMPIRE;
        if (r_lvl >= 80)
            r_idx = MON_ELDER_VAMPIRE;

        summon_named_creature(0, m_ptr->pos, r_idx, mode);
    }
}

/*
 * Modify the physical damage done to the monster.
 * (for example when it's invulnerable or shielded)
 *
 * ToDo: Accept a damage-type to calculate the modified damage from
 * things like fire, frost, lightning, poison, ... attacks.
 *
 * "type" is not yet used and should be 0.
 */
 /* I had to split this into 2 routines:
    [1] Player is damaging monster */
int mon_damage_mod(monster_type *m_ptr, int dam, bool is_psy_spear)
{
    monster_race *r_ptr = mon_race(m_ptr);

    if (mon_tim_find(m_ptr, T_INVULN))
    {
        if (is_psy_spear)
        {
            if (mon_show_msg(m_ptr))
                msg_print("The barrier is penetrated!");
        }
        else if (!one_in_(PENETRATE_INVULNERABILITY))
        {
            return (0);
        }
    }

    /* Hack: Pact monsters have special resistance to all damage from the player
       I'm not sure if this is the correct spot for this code ...*/
    if ( p_ptr->pclass == CLASS_WARLOCK
      && warlock_is_pact_monster(r_ptr)
      && dam )
    {
        /* Let the player notice this for this monster race only the first time */
        if (!(r_ptr->r_flagsr & RFR_PACT_MONSTER))
        {
            msg_print("<color:v>You are less effective against monsters you have made a pact with.</color>");
            r_ptr->r_flagsr |= (RFR_PACT_MONSTER);
        }
        dam = dam/2;
    }

    return (dam);
}

/* [2] Another monster is damaging monster */
int mon_damage_mod_mon(monster_type *m_ptr, int dam, bool is_psy_spear)
{
    if (mon_tim_find(m_ptr, T_INVULN))
    {
        if (is_psy_spear)
        {
            if (mon_show_msg(m_ptr))
                msg_print("The barrier is penetrated!");
        }
        else if (!one_in_(PENETRATE_INVULNERABILITY))
        {
            return (0);
        }
    }

    return (dam);
}


/*
 * Calculate experience point to be get
 *
 * Even the 64 bit operation is not big enough to avoid overflaw
 * unless we carefully choose orders of multiplication and division.
 *
 * Get the coefficient first, and multiply (potentially huge) base
 * experience point of a monster later.
 */
static void get_exp_from_mon(int dam, monster_type *m_ptr)
{
    monster_race *r_ptr = mon_race(m_ptr);

    s32b new_exp;
    u32b new_exp_frac;
    s32b div_h;
    u32b div_l;

    if (is_pet(m_ptr)) return;

    /*
     * - Ratio of monster's level to player's level effects
     * - Varying speed effects (Skipped for Breeders)
     * - Get a fraction in proportion of damage point
     */
    if (r_ptr->flags2 & RF2_MULTIPLY)
    {
        dam = dam * (r_ptr->hdice * (r_ptr->hside + 1) / 2) / m_ptr->maxhp;
        new_exp = r_ptr->level * SPEED_TO_ENERGY(r_ptr->speed) * dam;
    }
    else
        new_exp = r_ptr->level * SPEED_TO_ENERGY(m_ptr->mspeed) * dam;

    new_exp_frac = 0;
    div_h = 0L;
    div_l = (p_ptr->max_plv+2) * SPEED_TO_ENERGY(r_ptr->speed);

    /* Use (average maxhp * 2) as a denominator */
    if (!(r_ptr->flags1 & RF1_FORCE_MAXHP))
        s64b_mul(&div_h, &div_l, 0, r_ptr->hdice * (r_ptr->hside + 1));
    else
        s64b_mul(&div_h, &div_l, 0, r_ptr->hdice * r_ptr->hside * 2);

    /* Do division first to prevent overflaw */
    s64b_div(&new_exp, &new_exp_frac, div_h, div_l);

    /* Special penalty for mutiply-monster    */
    if ((r_ptr->flags2 & RF2_MULTIPLY) || (m_ptr->r_idx == MON_DAWN))
    {
        int biff = r_ptr->r_akills / 400;
        if (biff > 8) biff = 8;
        if (biff) s64b_RSHIFT(new_exp, new_exp_frac, biff);
    }

    /* Farming Summoners for xp is now biffed! For example, farming
       The Queen Ant for infinite Giant Fire Ants and a quick CL50. */
    if (m_ptr->parent_m_idx && !(r_ptr->flags1 & RF1_UNIQUE))
    {
        monster_type *pm_ptr = dun_mon(cave, m_ptr->parent_m_idx);

        if (pm_ptr)
        {
            monster_race *pr_ptr = mon_race(pm_ptr);
            int           biff;

            if (pr_ptr->flags1 & RF1_UNIQUE)
                biff = pr_ptr->r_skills / 100;
            else /* Draconic Q's? */
                biff = pr_ptr->r_skills / 250;

            if (biff > 8) biff = 8;
            if (biff) s64b_RSHIFT(new_exp, new_exp_frac, biff);
        }
    }

    /* Finally multiply base experience point of the monster */
    s64b_mul(&new_exp, &new_exp_frac, 0, r_ptr->mexp);

    /* Limit the amount of Exp gained from a single monster to remove
       player exploits that deliberately allow monsters to rest (= infinite exp) */
    {
        s32b n_h, d_h;
        u32b n_l, d_l;
        s32b pexp, mexp;

        /* Figure out the max experience to gain from this monster */
        if (r_ptr->flags2 & RF2_MULTIPLY)
            n_h = r_ptr->level * SPEED_TO_ENERGY(r_ptr->speed);
        else
            n_h = r_ptr->level * SPEED_TO_ENERGY(m_ptr->mspeed);
        n_l = 0;
        d_h = 0;
        d_l = (p_ptr->max_plv+2) * SPEED_TO_ENERGY(r_ptr->speed);

        s64b_div(&n_h, &n_l, d_h, d_l);
        s64b_mul(&n_h, &n_l, 0, r_ptr->mexp);

        mexp = n_h * 100;
        n_h = 0;
        s64b_mul(&n_h, &n_l, 0, 100);
        mexp += n_h;

        /* Figure out how much we are gaining */
        pexp = new_exp * 100;
        n_h = 0;
        n_l = new_exp_frac;
        s64b_mul(&n_h, &n_l, 0, 100);
        pexp += n_h;

        if (m_ptr->pexp + pexp > mexp)
        {
            pexp = MAX(0, mexp - m_ptr->pexp);
            new_exp = pexp / 100;
            n_h = pexp % 100;
            new_exp_frac = 0;
            s64b_div(&n_h, &new_exp_frac, 0, 100);
        }
        m_ptr->pexp += pexp;
#if 0
        msg_format(
            "Gain %d.%2.2d XP (Max %d.%2.2d, Mon %d.%2.2d, Actual %d %u)",
            pexp/100, pexp%100,
            mexp/100, mexp%100,
            m_ptr->pexp/100, m_ptr->pexp%100,
            new_exp, new_exp_frac
        );
#endif
    }

    if (mut_present(MUT_FAST_LEARNER))
    {
        s64b_mul(&new_exp, &new_exp_frac, 0, 6);
        s64b_div(&new_exp, &new_exp_frac, 0, 5);
    }

    /* Intelligence affects learning! */
    s64b_mul(&new_exp, &new_exp_frac, 0, adj_exp_gain[p_ptr->stat_ind[A_INT]]);
    s64b_div(&new_exp, &new_exp_frac, 0, 100);

    /* Gain experience */
    gain_exp_64(new_exp, new_exp_frac);
}

/* Hack for Quylthulgs. Their pets are allowed to kill uniques! */
void mon_check_kill_unique(int m_idx)
{
    monster_type    *m_ptr = dun_mon(cave, m_idx);
    monster_race    *r_ptr = mon_race(m_ptr);

    if (!(m_ptr->smart & (1U << SM_CLONED)))
    {
        /* When the player kills a Unique, it stays dead */
        if (r_ptr->flags1 & RF1_UNIQUE)
        {
            r_ptr->max_num = 0;

            if (one_in_(3) || r_ptr->level >= 80)
            {
                p_ptr->fame++;
                if (r_ptr->level >= 90)
                    p_ptr->fame++;
            }

            /* Mega-Hack -- Banor & Lupart */
            if ((m_ptr->r_idx == MON_BANOR) || (m_ptr->r_idx == MON_LUPART))
            {
                mon_race_ptr race = mon_race_lookup(MON_BANORLUPART);
                race->max_num = 0;
                race->r_pkills++;
                race->r_akills++;
                if (race->r_tkills < MAX_SHORT) race->r_tkills++;
            }
            else if (m_ptr->r_idx == MON_BANORLUPART)
            {
                mon_race_ptr race = mon_race_lookup(MON_BANOR);
                race->max_num = 0;
                race->r_pkills++;
                race->r_akills++;
                if (race->r_tkills < MAX_SHORT) race->r_tkills++;

                race = mon_race_lookup(MON_LUPART);
                race->max_num = 0;
                race->r_pkills++;
                race->r_akills++;
                if (race->r_tkills < MAX_SHORT) race->r_tkills++;
            }
        }

        /* When the player kills a Nazgul, it stays dead */
        else if (r_ptr->flags7 & RF7_NAZGUL) r_ptr->max_num--;
        else if (m_ptr->r_idx == MON_CAMELOT_KNIGHT)
        {
            if (r_ptr->max_num)
                r_ptr->max_num--;
        }
    }
}

/*
 * Decreases monsters hit points, handling monster death.
 *
 * We return TRUE if the monster has been killed (and deleted).
 *
 * We announce monster death (using an optional "death message"
 * if given, and a otherwise a generic killed/destroyed message).
 *
 * Only "physical attacks" can induce the "You have slain" message.
 * Missile and Spell attacks will induce the "dies" message, or
 * various "specialized" messages. Note that "You have destroyed"
 * and "is destroyed" are synonyms for "You have slain" and "dies".
 *
 * Hack -- unseen monsters yield "You have killed it." message.
 *
 * Added fear (DGK) and check whether to print fear messages -CWS
 *
 * Made name, sex, and capitalization generic -BEN-
 *
 * As always, the "ghost" processing is a total hack.
 *
 * Hack -- we "delay" fear messages by passing around a "fear" flag.
 *
 * XXX XXX XXX Consider decreasing monster experience over time, say,
 * by using "(m_exp * m_lev * (m_lev)) / (p_lev * (m_lev + n_killed))"
 * instead of simply "(m_exp * m_lev) / (p_lev)", to make the first
 * monster worth more than subsequent monsters. This would also need
 * to induce changes in the monster recall code.
 */
static void _ring_revealed(int id, mon_ptr mon)
{
    if (is_aware(mon)) return;
    if (mon_tim_find(mon, MT_SLEEP)) return;
    if (!plr_los(mon->pos)) return;
    mon->mflag2 |= MFLAG2_AWARE;
}
bool mon_take_hit(int m_idx, int dam, bool *fear, cptr note)
{
    monster_type    *m_ptr = dun_mon(cave, m_idx);
    monster_race    *r_ptr = &r_info[m_ptr->r_idx];

    /* Innocent until proven otherwise */
    bool        innocent = TRUE, thief = FALSE;
    int         i, j;
    int         expdam;

    set_sanctuary(FALSE);

    /* Hack: Player mimic has revealed itself! */
    if (p_ptr->prace == RACE_MON_RING && !p_ptr->riding)
    {
        m_ptr->mflag2 |= MFLAG2_AWARE;
        dun_iter_mon(cave, _ring_revealed);
    }

    if (!(r_ptr->flags7 & RF7_KILL_EXP))
    {
        expdam = (m_ptr->hp > dam) ? dam : m_ptr->hp;
        if (mon_race_has_healing(r_ptr)) expdam = (expdam+1) * 2 / 3;

        get_exp_from_mon(expdam, m_ptr);

        /* Genocided by chaos patron */
        if (mon_is_dead(m_ptr)) m_idx = 0;
    }

    /* Redraw (later) if needed */
    check_mon_health_redraw(m_idx);

    /* Wake it up */
    if (shoot_hack != SHOOT_TRANQUILIZE)
        mon_tim_delete(m_ptr, MT_SLEEP);

    /* Hack - Cancel any special player stealth magics. -LM- */
    if (p_ptr->special_defense & NINJA_S_STEALTH)
    {
        set_superstealth(FALSE);
    }

    /* Genocided by chaos patron */
    if (!m_idx) return TRUE;

    if (dam > 0 && p_ptr->wizard)
        msg_format("You do %d damage.", dam);

    if ( p_ptr->melt_armor
      && note == NULL /* Hack: Trying to just get melee and shooting */
      && (-m_ptr->ac_adj) < r_ptr->ac/2
      && !mon_save_p(m_ptr->r_idx, A_NONE) )
    {
        char m_name[MAX_NLEN];
        monster_desc(m_name, m_ptr, MD_PRON_VISIBLE | MD_POSSESSIVE);
        msg_format("%^s armor melts.", m_name);
        m_ptr->ac_adj -= randint1(2);
        if (p_ptr->wizard)
            msg_format("Melt Armor: AC is now %d", mon_ac(m_ptr));
    }

    /* Rage Mage: "Blood Lust" */
    if (p_ptr->pclass == CLASS_RAGE_MAGE && dam > 0)
    {
        rage_mage_blood_lust(dam);
    }

    /* Hurt it */
    m_ptr->hp -= dam;
    if (m_ptr->hp >= 0)
        pack_on_damage_monster(m_idx);

    /* It is dead now */
    if (m_ptr->hp < 0)
    {
        char         m_name[MAX_NLEN];
        monster_type exp_mon = *m_ptr; /* Copy since we will delete_monster before granting experience */

        monster_desc(m_name, m_ptr, MD_TRUE_NAME);

        if (r_info[m_ptr->r_idx].flags7 & RF7_TANUKI)
        {
            /* You might have unmasked Tanuki first time */
            r_ptr = &r_info[m_ptr->r_idx];
            m_ptr->ap_r_idx = m_ptr->r_idx;
            if (r_ptr->r_sights < MAX_SHORT) r_ptr->r_sights++;
        }

        if (m_ptr->mflag2 & MFLAG2_CHAMELEON)
        {
            /* You might have unmasked Chameleon first time */
            r_ptr = real_r_ptr(m_ptr);
            if (r_ptr->r_sights < MAX_SHORT) r_ptr->r_sights++;
        }

        if (!(m_ptr->smart & (1U << SM_CLONED)))
        {
            /* When the player kills a Unique, it stays dead */
            if (r_ptr->flags1 & RF1_UNIQUE)
            {
                if (m_ptr->r_idx == MON_PHOENIX && one_in_(3))
                {
                    m_ptr->hp = m_ptr->maxhp;
                    mon_tim_delete(m_ptr, T_FEAR);
                    msg_print("The Phoenix rises again!");
                    return FALSE;
                }

                r_ptr->max_num = 0;

                if (one_in_(3) || r_ptr->level >= 80)
                {
                    p_ptr->fame++;
                    if (r_ptr->level >= 90)
                        p_ptr->fame++;
                }

                /* Mega-Hack -- Banor & Lupart */
                if ((m_ptr->r_idx == MON_BANOR) || (m_ptr->r_idx == MON_LUPART))
                {
                    r_info[MON_BANORLUPART].max_num = 0;
                    r_info[MON_BANORLUPART].r_pkills++;
                    r_info[MON_BANORLUPART].r_akills++;
                    if (r_info[MON_BANORLUPART].r_tkills < MAX_SHORT) r_info[MON_BANORLUPART].r_tkills++;
                }
                else if (m_ptr->r_idx == MON_BANORLUPART)
                {
                    r_info[MON_BANOR].max_num = 0;
                    r_info[MON_BANOR].r_pkills++;
                    r_info[MON_BANOR].r_akills++;
                    if (r_info[MON_BANOR].r_tkills < MAX_SHORT) r_info[MON_BANOR].r_tkills++;
                    r_info[MON_LUPART].max_num = 0;
                    r_info[MON_LUPART].r_pkills++;
                    r_info[MON_LUPART].r_akills++;
                    if (r_info[MON_LUPART].r_tkills < MAX_SHORT) r_info[MON_LUPART].r_tkills++;
                }
            }

            /* When the player kills a Nazgul, it stays dead */
            else if (r_ptr->flags7 & RF7_NAZGUL) r_ptr->max_num--;
            else if (m_ptr->r_idx == MON_CAMELOT_KNIGHT)
            {
                if (r_ptr->max_num)
                    r_ptr->max_num--;
            }
        }

        /* Handle Packs ... Check Morale */
        pack_on_slay_monster(m_idx);

        /* Count all monsters killed */
        if (r_ptr->r_akills < MAX_SHORT) r_ptr->r_akills++;

        /* Count all summons killed */
        if (m_ptr->parent_m_idx)
        {
            monster_type *pm_ptr = dun_mon(cave, m_ptr->parent_m_idx);
            if (pm_ptr)
            {
                monster_race *pr_ptr = &r_info[pm_ptr->r_idx];
                if (pr_ptr->r_skills < MAX_SHORT)
                    pr_ptr->r_skills++;
            }
        }

        /* Recall even invisible uniques or winners (or statistics gathering runs :) */
        if ((m_ptr->ml && !plr_tim_find(T_HALLUCINATE)) || (r_ptr->flags1 & RF1_UNIQUE) || statistics_hack)
        {
            /* Count kills this life */
            if ((m_ptr->mflag2 & MFLAG2_KAGE) && (r_info[MON_KAGE].r_pkills < MAX_SHORT)) r_info[MON_KAGE].r_pkills++;
            else if (r_ptr->r_pkills < MAX_SHORT) r_ptr->r_pkills++;

            /* Count kills in all lives */
            if ((m_ptr->mflag2 & MFLAG2_KAGE) && (r_info[MON_KAGE].r_tkills < MAX_SHORT)) r_info[MON_KAGE].r_tkills++;
            else if (r_ptr->r_tkills < MAX_SHORT) r_ptr->r_tkills++;

            /* Hack -- Auto-recall */
            mon_track(m_ptr);
        }

        /* Don't kill Amberites */
        if ((r_ptr->flags3 & RF3_AMBERITE) && one_in_(2))
        {
            int curses = 1 + randint1(3);
            bool stop_ty = FALSE;
            int count = 0;

            cmsg_format(TERM_VIOLET, "%^s puts a terrible blood curse on you!", m_name);

            curse_equipment(100, 50);

            do
            {
                stop_ty = activate_ty_curse(stop_ty, &count);
            }
            while (--curses);
        }

        if (r_ptr->flags2 & RF2_CAN_SPEAK)
        {
            char line_got[1024];

            /* Dump a message */
            if (!get_rnd_line("mondeath.txt", m_ptr->r_idx, line_got))

                msg_format("%^s %s", m_name, line_got);
        }

        if (r_ptr->level > cave->dun_lvl)
        {
            if (randint1(10) <= (r_ptr->level - cave->dun_lvl))
                virtue_add(VIRTUE_VALOUR, 1);
        }
        if (r_ptr->level > 60)
        {
            virtue_add(VIRTUE_VALOUR, 1);
        }
        if (r_ptr->level >= 2 * (p_ptr->lev+1))
            virtue_add(VIRTUE_VALOUR, 2);

        if (r_ptr->flags1 & RF1_UNIQUE)
        {
            if (r_ptr->flags3 & (RF3_EVIL | RF3_GOOD)) virtue_add(VIRTUE_HARMONY, 2);

            if (r_ptr->flags3 & RF3_GOOD)
            {
                virtue_add(VIRTUE_UNLIFE, 2);
                virtue_add(VIRTUE_VITALITY, -2);
            }

            if (one_in_(3)) virtue_add(VIRTUE_INDIVIDUALISM, -1);
        }

        if (m_ptr->r_idx == MON_BEGGAR || m_ptr->r_idx == MON_LEPER)
        {
            virtue_add(VIRTUE_COMPASSION, -1);
        }

        if ((r_ptr->flags3 & RF3_GOOD) &&
            ((r_ptr->level) / 10 + (3 * cave->dun_lvl) >= randint1(100)))
            virtue_add(VIRTUE_UNLIFE, 1);

        if (r_ptr->d_char == 'A')
        {
            if (r_ptr->flags1 & RF1_UNIQUE)
                virtue_add(VIRTUE_FAITH, -2);
            else if ((r_ptr->level) / 10 + (3 * cave->dun_lvl) >= randint1(100))
            {
                if (r_ptr->flags3 & RF3_GOOD) virtue_add(VIRTUE_FAITH, -1);
                else virtue_add(VIRTUE_FAITH, 1);
            }
        }
        else if (r_ptr->flags3 & RF3_DEMON)
        {
            if (r_ptr->flags1 & RF1_UNIQUE)
                virtue_add(VIRTUE_FAITH, 2);
            else if ((r_ptr->level) / 10 + (3 * cave->dun_lvl) >= randint1(100))
                virtue_add(VIRTUE_FAITH, 1);
        }

        if ((r_ptr->flags3 & RF3_UNDEAD) && (r_ptr->flags1 & RF1_UNIQUE))
            virtue_add(VIRTUE_VITALITY, 2);

        if (r_ptr->r_deaths)
        {
            if (r_ptr->flags1 & RF1_UNIQUE)
            {
                virtue_add(VIRTUE_HONOUR, 10);
            }
            else if ((r_ptr->level) / 10 + (2 * cave->dun_lvl) >= randint1(100))
            {
                virtue_add(VIRTUE_HONOUR, 1);
            }
        }
        if ((r_ptr->flags2 & RF2_MULTIPLY) && (r_ptr->r_akills > 1000) && one_in_(10))
        {
            virtue_add(VIRTUE_VALOUR, -1);
        }

        for (i = 0; i < vec_length(r_ptr->blows); i++)
        {
            mon_blow_ptr blow = vec_get(r_ptr->blows, i);
            /* XXX RF2_THIEF could be used, but not every RBE_EAT_FOO monster is so flagged */
            for (j = 0; j < blow->effect_ct; j++)
            {
                int effect = blow->effects[j].effect;
                if (j == 0 && blow->effects[j].dice.dd != 0) innocent = FALSE; /* Murderer! XXX This makes no sense ... */
                if (effect == RBE_EAT_GOLD || effect == RBE_EAT_ITEM)
                    thief = TRUE;
            }
        }

        /* The new law says it is illegal to live in the dungeon */
        if (r_ptr->level != 0) innocent = FALSE;

        if (thief)
        {
            if (r_ptr->flags1 & RF1_UNIQUE)
                virtue_add(VIRTUE_JUSTICE, 3);
            else if (1 + r_ptr->level/10 + 2*cave->dun_lvl >= randint1(100))
                virtue_add(VIRTUE_JUSTICE, 1);
        }
        else if (innocent)
        {
            virtue_add (VIRTUE_JUSTICE, -1);
        }

        if ((r_ptr->flags3 & RF3_ANIMAL) && !(r_ptr->flags3 & RF3_EVIL) && !mon_race_is_magical(r_ptr))
        {
            if (one_in_(4)) virtue_add(VIRTUE_NATURE, -1);
        }

        /* Make a sound */
        sound(SOUND_KILL);

        /* Death by Missile/Spell attack */
        if (note)
        {
            msg_format("%^s%s", m_name, note);
        }

        /* Death by physical attack -- invisible monster */
        else if (!m_ptr->ml)
        {
            cmsg_format(TERM_L_RED, "You have killed %s.", m_name);
        }

        /* Death by Physical attack -- non-living monster */
        else if (!monster_living(r_ptr))
        {
            if (mon_blows_find(r_ptr->blows, RBM_EXPLODE))
                cmsg_format(TERM_L_RED, "%^s explodes into tiny shreds.", m_name);
            else
                cmsg_format(TERM_L_RED, "You have destroyed %s.", m_name);
        }

        /* Death by Physical attack -- living monster */
        else
            cmsg_format(TERM_L_RED, "You have slain %s.", m_name);

        if (_mon_is_wanted(m_ptr))
        {
            msg_format("There is a price on %s's head.", m_name);
        }

        /* Generate treasure */
        monster_death(m_ptr, TRUE);

        /* Mega hack : replace IKETA to BIKETAL */
        if (m_ptr->r_idx == MON_IKETA)
        {
            point_t pos = m_ptr->pos;
            u32b mode = 0L;

            if (is_pet(m_ptr)) mode |= PM_FORCE_PET;

            delete_monster(m_ptr);

            if (summon_named_creature(0, pos, MON_BIKETAL, mode))
                msg_print("Uwa-hahaha!  *I* am Biketal!");
        }
        else
        {
            delete_monster(m_ptr);
        }

        /* Prevent bug of chaos patron's reward */
        if (r_ptr->flags7 & RF7_KILL_EXP)
            get_exp_from_mon((long)exp_mon.max_maxhp*2, &exp_mon);
        else
            get_exp_from_mon(((long)exp_mon.max_maxhp+1L) * 9L / 10L, &exp_mon);

        /* Not afraid */
        (*fear) = FALSE;

        /* Monster is dead */
        return (TRUE);
    }

    /* Mega-Hack -- Pain cancels fear ... later (fear_process_m) */
    if (mon_tim_find(m_ptr, T_FEAR)) m_ptr->pain += dam;
    if (fear_p_hurt_m(m_idx, dam))
        (*fear) = TRUE;

    /* Not dead yet */
    return (FALSE);
}

/*
 * Map resizing whenever the main term changes size
 */
void resize_map(void)
{
    /* Only if the dungeon exists */
    if (!cave) return;
    if (!(cave->flags & DF_GENERATED)) return;

    viewport_verify();
    msg_line_clear();

    /* Update stuff */
    p_ptr->update |= (PU_TORCH | PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);

    /* Forget lite/view */
    p_ptr->update |= (PU_UN_VIEW | PU_UN_LITE);

    /* Update lite/view */
    p_ptr->update |= (PU_VIEW | PU_LITE | PU_MON_LITE);

    /* Update monsters */
    p_ptr->update |= (PU_MONSTERS);

    /* Redraw everything */
    p_ptr->redraw |= (PR_WIPE | PR_BASIC | PR_EXTRA | PR_MAP | PR_EQUIPPY);

    /* Hack -- update */
    handle_stuff();

    /* Redraw */
    Term_redraw();

    /*
     * Waiting command;
     * Place the cursor on the player
     */
    if (can_save) move_cursor_relative(p_ptr->pos);

    msg_line_init(ui_msg_rect());

    /* Refresh */
    Term_fresh();
}

/*
 * Redraw a term when it is resized
 */
void redraw_window(void)
{
    /* Only if the dungeon exists */
    if (!(cave->flags & DF_GENERATED)) return;

    /* Window stuff */
    p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_SPELL);

    /* Window stuff */
    p_ptr->window |= (PW_MESSAGE | PW_OVERHEAD | PW_DUNGEON | PW_MONSTER | PW_OBJECT);

    /* Hack -- update */
    handle_stuff();

    /* Redraw */
    Term_redraw();
}


point_t ui_pt_to_cave_pt(point_t pt)
{
    rect_t  r = ui_map_rect();
    point_t v = point_subtract(pt, rect_top_left(r));
    return point_add(viewport_origin, v);
}

point_t ui_xy_to_cave_pt(int x, int y)
{
    return ui_pt_to_cave_pt(point_create(x, y));
}

point_t cave_pt_to_ui_pt(point_t pt)
{
    rect_t  r = ui_map_rect();
    point_t v = point_subtract(pt, viewport_origin);
    return point_add(rect_top_left(r), v);
}

point_t cave_xy_to_ui_pt(int x, int y)
{
    return cave_pt_to_ui_pt(point_create(x, y));
}

bool cave_pt_is_visible(point_t pt)
{
    point_t ui = cave_pt_to_ui_pt(pt);
    rect_t  r = ui_map_rect();
    return rect_contains_point(r, ui);
}

bool cave_xy_is_visible(int x, int y)
{
    return in_bounds2(y, x) /* This is for legacy code ... */
        && cave_pt_is_visible(point_create(x, y));
}

bool ui_pt_is_visible(point_t pt)
{
    rect_t r = ui_map_rect();
    return rect_contains_point(r, pt);
}

bool ui_xy_is_visible(int x, int y)
{
    rect_t r = ui_map_rect();
    return rect_contains_xy(r, x, y);
}

rect_t ui_map_rect(void)
{
    return rect_create(
        0,
        1,
        Term->wid - 12 - 1,
        Term->hgt - 1 - 1
    );
}

rect_t ui_menu_rect(void) { return ui_map_rect(); }
rect_t ui_doc_menu_rect(void)
{
    return rect_create(
        0,
        0,
        Term->wid - 12 - 1,
        Term->hgt
    );
}

rect_t ui_shop_msg_rect(void)
{
    return rect_create(0, 0, 80, 3);
}

rect_t ui_msg_rect(void)
{
    return rect_create(0, 0, MIN(72, Term->wid - 13), 10);
}

rect_t ui_shop_rect(void)
{
    return rect_create(
        0,
        3,
        Term->wid,
        Term->hgt - 3
    );
}

rect_t ui_screen_rect(void)
{
    return rect_create(0, 0, Term->wid, Term->hgt);
}

rect_t ui_char_info_rect(void)
{
    return rect_create(
        Term->wid - 12,
        1,
        12,
        Term->hgt - 2
    );
}

/*
 * Handle a request to change the current viewport
 *
 * Return TRUE if the panel was changed.
 *
 * Also used in do_cmd_locate
 */
bool viewport_scroll(int dy, int dx)
{
    int y, x;
    rect_t r = ui_map_rect();
    point_t min = rect_top_left(cave->rect);
    point_t max = rect_bottom_right(cave->rect);

    /* Apply the motion */
    y = viewport_origin.y + dy * r.cy / 2;
    x = viewport_origin.x + dx * r.cx / 2;

    /* Verify the row */
    if (y > max.y - r.cy + 1) y = max.y - r.cy + 1;
    if (y < min.y) y = min.y;

    /* Verify the col */
    if (x > max.x - r.cx + 1) x = max.x - r.cx + 1;
    if (x < min.x) x = min.x;

    /* Handle "changes" */
    if ((y != viewport_origin.y) || (x != viewport_origin.x))
    {
        viewport_origin.y = y;
        viewport_origin.x = x;

        p_ptr->update |= (PU_MONSTERS); /* XXX Why? */
        p_ptr->redraw |= (PR_MAP);
        redraw_hack = TRUE;
        handle_stuff();
        redraw_hack = FALSE;
        return TRUE;
    }

    /* No change */
    return FALSE;
}

/*
 * Given an row (y) and col (x), this routine detects when a move
 * off the screen has occurred and figures new borders. -RAK-
 *
 * "Update" forces a "full update" to take place.
 *
 * The map is reprinted if necessary.
 */
void viewport_verify_aux(point_t pos, u32b options)
{
    point_t p = cave_pt_to_ui_pt(pos);
    rect_t  r = ui_map_rect();
    point_t o = viewport_origin;
    point_t min = rect_top_left(cave->rect);
    point_t max = rect_bottom_right(cave->rect);

    if ((options & VIEWPORT_FORCE_CENTER) || !rect_contains_point(r, p))
    {
        point_t c = rect_center(r);
        point_t d = point_subtract(p, c);
        o = point_add(o, d);
    }
    else
    {
        if (p.y > r.y + r.cy - 2)
            o.y += r.cy/2;
        else if (p.y < r.y + 2)
            o.y -= r.cy/2;
        if (p.x > r.x + r.cx - 4)
            o.x += r.cx/2;
        else if (p.x < r.x + 4)
            o.x -= r.cx/2;
    }
    if (o.x > max.x - 3*r.cx/4) o.x = max.x - 3*r.cx/4;
    if (o.y > max.y - 3*r.cy/4) o.y = max.y - 3*r.cy/4;
    if (o.x < min.x - r.cx/4) o.x = min.x - r.cx/4;
    if (o.y < min.y - r.cy/4) o.y = min.y - r.cy/4;
    if (point_compare(viewport_origin, o) != 0)
    {
        viewport_origin = o;
        if (disturb_panel && !center_player) disturb(0, 0);
        p_ptr->update |= PU_MONSTERS;
        p_ptr->redraw |= PR_MAP;
        p_ptr->window |= PW_OVERHEAD | PW_DUNGEON;
    }
}

void viewport_verify(void)
{
    int options = 0;
    if (center_player && (center_running || (!running && !travel.run)))
        options |= VIEWPORT_FORCE_CENTER;
    viewport_verify_aux(p_ptr->pos, options);
}


cptr mon_health_desc(monster_type *m_ptr)
{
    monster_race *ap_r_ptr = mon_apparent_race(m_ptr);
    bool          living = monster_living(ap_r_ptr);
    int           perc = 100 * m_ptr->hp / m_ptr->maxhp;

    if (m_ptr->hp >= m_ptr->maxhp)
        return living ? "unhurt" : "undamaged";
    else if (perc >= 60)
        return living ? "somewhat wounded" : "somewhat damaged";
    else if (perc >= 25)
        return living ? "wounded" : "damaged";
    else if (perc >= 10)
        return living ? "badly wounded" : "badly damaged";

    return living ? "almost dead" : "almost destroyed";
}

cptr mon_allegiance_desc(monster_type *m_ptr)
{
    if (is_pet(m_ptr))
        return "pet";
    else if (is_friendly(m_ptr))
        return "friendly";
    return ""; /* Hostile is to be assumed! */
}

/*
 * Angband sorting algorithm -- quick sort in place
 *
 * Note that the details of the data we are sorting is hidden,
 * and we rely on the "ang_sort_comp()" and "ang_sort_swap()"
 * function hooks to interact with the data, which is given as
 * two pointers, and which may have any user-defined form.
 */
void ang_sort_aux(vptr u, vptr v, int p, int q)
{
    int z, a, b;

    /* Done sort */
    if (p >= q) return;

    /* Pivot */
    z = p;

    /* Begin */
    a = p;
    b = q;

    /* Partition */
    while (TRUE)
    {
        /* Slide i2 */
        while (!(*ang_sort_comp)(u, v, b, z)) b--;

        /* Slide i1 */
        while (!(*ang_sort_comp)(u, v, z, a)) a++;

        /* Done partition */
        if (a >= b) break;

        /* Swap */
        (*ang_sort_swap)(u, v, a, b);

        /* Advance */
        a++, b--;
    }

    /* Recurse left side */
    ang_sort_aux(u, v, p, b);

    /* Recurse right side */
    ang_sort_aux(u, v, b+1, q);
}


/*
 * Angband sorting algorithm -- quick sort in place
 *
 * Note that the details of the data we are sorting is hidden,
 * and we rely on the "ang_sort_comp()" and "ang_sort_swap()"
 * function hooks to interact with the data, which is given as
 * two pointers, and which may have any user-defined form.
 */
void ang_sort(vptr u, vptr v, int n)
{
    /* Sort the array */
    ang_sort_aux(u, v, 0, n-1);
}



/*** Targeting Code ***/


/*
 * Determine is a monster makes a reasonable target
 *
 * The concept of "targeting" was stolen from "Morgul" (?)
 *
 * The player can target any location, or any "target-able" monster.
 *
 * Currently, a monster is "target_able" if it is visible, and if
 * the player can hit it with a projection, and the player is not
 * hallucinating. This allows use of "use closest target" macros.
 *
 * Future versions may restrict the ability to target "trappers"
 * and "mimics", but the semantics is a little bit weird.
 */
bool target_able(int m_idx) { return target_able_aux(m_idx, TARGET_KILL); }
bool target_able_aux(int m_idx, int mode)
{
    monster_type *m_ptr = dun_mon(cave, m_idx);

    /* Monster must be alive */
    if (mon_is_dead(m_ptr)) return (FALSE);

    /* Hack -- no targeting hallucinations */
    if (plr_tim_find(T_HALLUCINATE)) return (FALSE);

    /* Monster must be visible */
    if (!m_ptr->ml) return (FALSE);

    if (p_ptr->riding && (p_ptr->riding == m_idx)) return (TRUE);

    /* Monster must be projectable */
    if (mode != TARGET_DISI && !point_project(p_ptr->pos, m_ptr->pos)) return FALSE;

    /* XXX XXX XXX Hack -- Never target trappers */
    /* if (CLEAR_ATTR && (CLEAR_CHAR)) return (FALSE); */

    /* Assume okay */
    return (TRUE);
}

/*
 * Update (if necessary) and verify (if possible) the target.
 *
 * We return TRUE if the target is "okay" and FALSE otherwise.
 */
bool target_okay(void) { return target_okay_aux(TARGET_KILL); }
bool target_okay_aux(int mode)
{
    /* Accept stationary targets ... but cf move_player_effect
     * in cmd1.c. We will dismiss a non-projectable positional
     * target the next time the player moves. */
    if (target_who < 0) return TRUE;

    /* Check moving targets */
    if (target_who > 0)
    {
        /* Accept reasonable targets */
        if (target_able_aux(target_who, mode))
        {
            monster_type *m_ptr = dun_mon(cave, target_who);

            /* Acquire monster location */
            target_row = m_ptr->pos.y;
            target_col = m_ptr->pos.x;

            /* Good target */
            return (TRUE);
        }
    }

    /* Assume no target */
    return (FALSE);
}


/*
 * Sorting hook -- comp function -- by "distance to player"
 *
 * We use "u" and "v" to point to arrays of "x" and "y" positions,
 * and sort the arrays by double-distance to the player.
 */
static bool ang_sort_comp_distance(vptr u, vptr v, int a, int b)
{
    s16b *x = (s16b*)(u);
    s16b *y = (s16b*)(v);

    int da, db, kx, ky;

    /* Absolute distance components */
    kx = x[a]; kx -= p_ptr->pos.x; kx = ABS(kx);
    ky = y[a]; ky -= p_ptr->pos.y; ky = ABS(ky);

    /* Approximate Double Distance to the first point */
    da = ((kx > ky) ? (kx + kx + ky) : (ky + ky + kx));

    /* Absolute distance components */
    kx = x[b]; kx -= p_ptr->pos.x; kx = ABS(kx);
    ky = y[b]; ky -= p_ptr->pos.y; ky = ABS(ky);

    /* Approximate Double Distance to the first point */
    db = ((kx > ky) ? (kx + kx + ky) : (ky + ky + kx));

    /* Compare the distances */
    return (da <= db);
}

/*
 * Sorting hook -- comp function -- by importance level of grids
 *
 * We use "u" and "v" to point to arrays of "x" and "y" positions,
 * and sort the arrays by level of monster
 */
static bool ang_sort_comp_importance(vptr u, vptr v, int a, int b)
{
    s16b *x = (s16b*)(u);
    s16b *y = (s16b*)(v);
    cave_type *ca_ptr = cave_at_xy(x[a], y[a]);
    cave_type *cb_ptr = cave_at_xy(x[b], y[b]);
    monster_type *ma_ptr = mon_at_xy(x[a], y[a]);
    monster_type *mb_ptr = mon_at_xy(x[b], y[b]);
    monster_race *ap_ra_ptr, *ap_rb_ptr;
    obj_ptr oa_ptr = obj_at_xy(x[a], y[a]);
    obj_ptr ob_ptr = obj_at_xy(x[b], y[b]);

    /* The player grid */
    if (y[a] == p_ptr->pos.y && x[a] == p_ptr->pos.x) return TRUE;
    if (y[b] == p_ptr->pos.y && x[b] == p_ptr->pos.x) return FALSE;

    /* Extract monster race */
    if (ma_ptr && ma_ptr->ml) ap_ra_ptr = &r_info[ma_ptr->ap_r_idx];
    else ap_ra_ptr = NULL;
    if (mb_ptr && mb_ptr->ml) ap_rb_ptr = &r_info[mb_ptr->ap_r_idx];
    else ap_rb_ptr = NULL;

    if (ap_ra_ptr && !ap_rb_ptr) return TRUE;
    if (!ap_ra_ptr && ap_rb_ptr) return FALSE;

    /* Compare two monsters */
    if (ap_ra_ptr && ap_rb_ptr)
    {
        /* Unique monsters first */
        if ((ap_ra_ptr->flags1 & RF1_UNIQUE) && !(ap_rb_ptr->flags1 & RF1_UNIQUE)) return TRUE;
        if (!(ap_ra_ptr->flags1 & RF1_UNIQUE) && (ap_rb_ptr->flags1 & RF1_UNIQUE)) return FALSE;

        /* Shadowers first */
        if ((ma_ptr->mflag2 & MFLAG2_KAGE) && !(mb_ptr->mflag2 & MFLAG2_KAGE)) return TRUE;
        if (!(ma_ptr->mflag2 & MFLAG2_KAGE) && (mb_ptr->mflag2 & MFLAG2_KAGE)) return FALSE;

         /* Unknown monsters first */
        if (!ap_ra_ptr->r_tkills && ap_rb_ptr->r_tkills) return TRUE;
        if (ap_ra_ptr->r_tkills && !ap_rb_ptr->r_tkills) return FALSE;

        /* Higher level monsters first (if known) */
        if (ap_ra_ptr->r_tkills && ap_rb_ptr->r_tkills)
        {
            if (ap_ra_ptr->level > ap_rb_ptr->level) return TRUE;
            if (ap_ra_ptr->level < ap_rb_ptr->level) return FALSE;
        }

        /* Sort by index if all conditions are same */
        if (ma_ptr->ap_r_idx > mb_ptr->ap_r_idx) return TRUE;
        if (ma_ptr->ap_r_idx < mb_ptr->ap_r_idx) return FALSE;
    }

    /* An object get higher priority */
    if (oa_ptr && !ob_ptr) return TRUE;
    if (!oa_ptr && ob_ptr) return FALSE;

    /* Priority from the terrain */
    if (f_info[ca_ptr->feat].priority > f_info[cb_ptr->feat].priority) return TRUE;
    if (f_info[ca_ptr->feat].priority < f_info[cb_ptr->feat].priority) return FALSE;

    /* If all conditions are same, compare distance */
    return ang_sort_comp_distance(u, v, a, b);
}


/*
 * Sorting hook -- swap function -- by "distance to player"
 *
 * We use "u" and "v" to point to arrays of "x" and "y" positions,
 * and sort the arrays by distance to the player.
 */
static void ang_sort_swap_distance(vptr u, vptr v, int a, int b)
{
    s16b *x = (s16b*)(u);
    s16b *y = (s16b*)(v);

    s16b temp;

    /* Swap "x" */
    temp = x[a];
    x[a] = x[b];
    x[b] = temp;

    /* Swap "y" */
    temp = y[a];
    y[a] = y[b];
    y[b] = temp;
}



/*
 * Hack -- help "select" a location (see below)
 */
static s16b target_pick(int y1, int x1, int dy, int dx)
{
    int i, v;

    int x2, y2, x3, y3, x4, y4;

    int b_i = -1, b_v = 9999;


    /* Scan the locations */
    for (i = 0; i < temp_n; i++)
    {
        /* Point 2 */
        x2 = temp_x[i];
        y2 = temp_y[i];

        /* Directed distance */
        x3 = (x2 - x1);
        y3 = (y2 - y1);

        /* Verify quadrant */
        if (dx && (x3 * dx <= 0)) continue;
        if (dy && (y3 * dy <= 0)) continue;

        /* Absolute distance */
        x4 = ABS(x3);
        y4 = ABS(y3);

        /* Verify quadrant */
        if (dy && !dx && (x4 > y4)) continue;
        if (dx && !dy && (y4 > x4)) continue;

        /* Approximate Double Distance */
        v = ((x4 > y4) ? (x4 + x4 + y4) : (y4 + y4 + x4));

        /* XXX XXX XXX Penalize location */

        /* Track best */
        if ((b_i >= 0) && (v >= b_v)) continue;

        /* Track best */
        b_i = i; b_v = v;
    }

    /* Result */
    return (b_i);
}


/*
 * Hack -- determine if a given location is "interesting"
 */
static bool target_set_accept(point_t pos)
{
    dun_grid_ex_t grid;
    obj_ptr obj;

    if (!dun_pos_interior(cave, pos)) return FALSE;
    if (plr_at(pos)) return TRUE;
    if (plr_tim_find(T_HALLUCINATE)) return FALSE;

    grid = dun_grid_ex_at(cave, pos);
    if (grid.mon && grid.mon->ml) return TRUE;

    for (obj = grid.obj; obj; obj = obj->next)
        if (obj->marked & OM_FOUND) return TRUE;

    if (grid.grid->info & CAVE_MARK)
    {
        if (grid.grid->info & CAVE_OBJECT) return TRUE;
        if (have_flag(grid.feat_mimic->flags, FF_NOTICE)) return TRUE;
    }

    return FALSE;
}


/*
 * Prepare the "temp" array for "target_set"
 *
 * Return the number of target_able monsters in the set.
 */
static void target_set_prepare(int mode)
{
    rect_t map_rect = ui_map_rect();
    point_t uip;

    /* Reset "temp" array */
    temp_n = 0;

    /* Scan the current panel */
    for (uip.y = map_rect.y; uip.y < map_rect.y + map_rect.cy; uip.y++)
    {
        for (uip.x = map_rect.x; uip.x < map_rect.x + map_rect.cx; uip.x++)
        {
            point_t cp = ui_pt_to_cave_pt(uip);
            mon_ptr mon;

            if (!target_set_accept(cp)) continue;

            mon = mon_at(cp);

            if (mode & TARGET_KILL)
                if (!mon || !target_able(mon->id)) continue;

            if ((mode & (TARGET_KILL | TARGET_MARK)) && !target_pet)
                if (mon && is_pet(mon)) continue;

            if (mode & (TARGET_MARK | TARGET_DISI))
                if (!mon || !mon->ml) continue; /* XXX duelist required visible monsters */

            temp_x[temp_n] = cp.x;
            temp_y[temp_n] = cp.y;
            temp_n++;
        }
    }

    /* Set the sort hooks */
    if ((mode & TARGET_KILL) || (mode & TARGET_MARK) || (mode & TARGET_DISI))
    {
        /* Target the nearest monster for shooting */
        ang_sort_comp = ang_sort_comp_distance;
        ang_sort_swap = ang_sort_swap_distance;
    }
    else
    {
        /* Look important grids first in Look command */
        ang_sort_comp = ang_sort_comp_importance;
        ang_sort_swap = ang_sort_swap_distance;
    }

    /* Sort the positions */
    ang_sort(temp_x, temp_y, temp_n);

    if (p_ptr->riding && target_pet && (temp_n > 1) && (mode & (TARGET_KILL)))
    {
        s16b tmp;

        tmp = temp_y[0];
        temp_y[0] = temp_y[1];
        temp_y[1] = tmp;
        tmp = temp_x[0];
        temp_x[0] = temp_x[1];
        temp_x[1] = tmp;
    }
}


bool show_gold_on_floor = FALSE;

/*
 * Examine a grid, return a keypress.
 *
 * The "mode" argument contains the "TARGET_LOOK" bit flag, which
 * indicates that the "space" key should scan through the contents
 * of the grid, instead of simply returning immediately. This lets
 * the "look" command get complete information, without making the
 * "target" command annoying.
 *
 * The "info" argument contains the "commands" which should be shown
 * inside the "[xxx]" text. This string must never be empty, or grids
 * containing monsters will be displayed with an extra comma.
 *
 * Note that if a monster is in the grid, we update both the monster
 * recall info and the health bar info to track that monster.
 *
 * Eventually, we may allow multiple objects per grid, or objects
 * and terrain features in the same grid. XXX XXX XXX
 *
 * This function must handle blindness/hallucination.
 */
static int target_set_aux(int y, int x, int mode, cptr info)
{
    cave_type *c_ptr = cave_at_xy(x, y);
    obj_ptr obj;
    cptr s1 = "", s2 = "", s3 = "", x_info = "";
    bool boring = TRUE;
    s16b feat;
    feature_type *f_ptr;
    int query = '\001';
    char out_val[MAX_NLEN+80];
    inv_ptr inv = NULL;
    int obj_ct = 0;
    mon_ptr mon;

    /* Hack -- under the player */
    if (player_bold(y, x))
    {
        /* Description */
        s1 = "You are ";

        /* Preposition */
        s2 = "on ";
    }
    else
    {
        s1 = "Target: ";
        /* Hack: I get confused about whether I am selecting a monster,
           or the position that the monster happens to currently occupy.
           This happens after a fat finger incident or a failed <dir>
           navigation that really should have worked.*/
        if (strstr(info, "o,"))
            s1 = (mode == TARGET_LOOK) ? "Feature: " : "Monster: ";
        else if (strstr(info, "m,"))
            s1 = "Position: ";
    }

    /* Hack -- hallucination */
    if (plr_tim_find(T_HALLUCINATE))
    {
        cptr name = "something strange";


        /* Display a message */
        sprintf(out_val, "%s%s%s%s [%s]", s1, s2, s3, name, info);

        prt(out_val, 0, 0);
        move_cursor_relative(point_create(x, y));
        query = inkey();

        /* Stop on everything but "return" */
        if ((query != '\r') && (query != '\n')) return query;

        /* Repeat forever */
        return 0;
    }

    inv = inv_filter_floor(point_create(x, y), obj_is_found);
    obj_ct = inv_count_slots(inv, obj_exists);
    if (obj_ct)
        x_info = "x,";

    /* Actual monsters */
    mon = mon_at_xy(x, y);
    if (mon && mon->ml)
    {
        bool          fuzzy = BOOL(mon->mflag2 & MFLAG2_FUZZY);
        monster_race *ap_r_ptr = mon_apparent_race(mon);
        char m_name[80];
        bool recall = FALSE;

        boring = FALSE;

        if (fuzzy)
            strcpy(m_name, "Monster");
        else
        {
            monster_desc(m_name, mon, MD_INDEF_VISIBLE);
            mon_track(mon);
            health_track(mon->id);
            handle_stuff();
        }

        /* Interact */
        while (1)
        {
            /* Recall */
            if (recall && !fuzzy)
            {
                doc_ptr doc = doc_alloc(72);

                /* Save */
                screen_save();

                /* Recall on screen */
                mon_display_doc(ap_r_ptr, doc);
                doc_sync_term(doc, doc_range_all(doc), doc_pos_create(0, 1));
                doc_free(doc);

                /* Hack -- Complete the prompt (again)
                Term_addstr(-1, TERM_WHITE, format("  [r,%s%s]", x_info, info));*/

                /* Command */
                query = inkey();

                /* Restore */
                screen_load();

                /* Normal commands */
                if (query != 'r') break;

                /* Toggle recall */
                recall = FALSE;

                /* Cleare recall text and repeat */
                continue;
            }

            /*** Normal ***/

            /* Describe, and prompt for recall */
            sprintf(out_val, "%s%s%s%s ", s1, s2, s3, m_name);
            if (is_pet(mon))
                strcat(out_val, "(Pet) ");
            else if (!fuzzy)
            {
                if (is_friendly(mon))
                    strcat(out_val, "(Friendly) ");
                else if (mon->smart & (1U << SM_CLONED))
                    strcat(out_val, "(Clone) ");
            }
            if (display_distance)
                sprintf(out_val + strlen(out_val), "(Rng %d) ", mon->cdis);
            sprintf(out_val + strlen(out_val), "[r,%s%s]", x_info, info);

            prt(out_val, 0, 0);

            /* Place cursor */
            move_cursor_relative(point_create(x, y));

            /* Command */
            query = inkey();

            /* Normal commands */
            if (query != 'r') break;

            /* Toggle recall */
            recall = TRUE;
        }

        /* Always stop at "normal" keys */
        if ((query != '\r') && (query != '\n') && (query != ' ') && (query != 'x'))
        {
            inv_free(inv);
            return query;
        }

        /* Sometimes stop at "space" key */
        if ((query == ' ') && !(mode & (TARGET_LOOK)))
        {
            inv_free(inv);
            return query;
        }

        /* Change the intro */
        s1 = "It is ";


        /* Hack -- take account of gender */
        if (ap_r_ptr->flags1 & (RF1_FEMALE)) s1 = "She is ";

        else if (ap_r_ptr->flags1 & (RF1_MALE)) s1 = "He is ";


        /* Use a preposition */
        s2 = "carrying ";


        /* Scan all objects being carried */
        for (obj = mon->obj; obj; obj = obj->next)
        {
            char o_name[MAX_NLEN];

            /* Obtain an object description */
            object_desc(o_name, obj, 0);

            /* Describe the object */
            sprintf(out_val, "%s%s%s%s [%s]", s1, s2, s3, o_name, info);

            prt(out_val, 0, 0);
            move_cursor_relative(point_create(x, y));
            query = inkey();

            /* Always stop at "normal" keys */
            if ((query != '\r') && (query != '\n') && (query != ' ') && (query != 'x'))
            {
                inv_free(inv);
                return query;
            }

            /* Sometimes stop at "space" key */
            if ((query == ' ') && !(mode & (TARGET_LOOK)))
            {
                inv_free(inv);
                return query;
            }

            /* Change the intro */
            s2 = "also carrying ";
        }

        /* Use a preposition */
        s2 = "on ";
    }

    /* Show objects on this grid. If multiple, show a list. If the
     * list won't fit on the screen, <CR> scrolls the list */
    if (obj_ct == 1)
    {
        obj_ptr obj = inv_obj(inv, 1);
        char name[MAX_NLEN];

        object_desc(name, obj, 0);

        sprintf(out_val, "%s%s%s%s [%s]",
            s1, s2, s3, name, info);

        prt(out_val, 0, 0);
        move_cursor_relative(point_create(x, y));

        query = inkey();
        inv_free(inv);
        return query;
    }
    else if (obj_ct > 1)
    {
        doc_ptr doc = NULL;
        rect_t  r = ui_map_rect();
        int     top = 0, lines = r.cy - 1;
        if (boring)
        {
            /* Display rough information about items */
            sprintf(out_val, "%s%s%sa pile of %d items [x,%s]",
                s1, s2, s3, obj_ct, info);

            prt(out_val, 0, 0);
            move_cursor_relative(point_create(x, y));

            query = inkey();

            if (query != 'x' && query != ' ')
            {
                inv_free(inv);
                return query;
            }
        }
        doc = doc_alloc(72);
        inv_display(inv, 1, 0, obj_exists, doc, INV_SHOW_SLOT);
        for (;;)
        {
            screen_save();
            doc_sync_term(doc,
                doc_range_middle_lines(doc, top, top + lines),
                doc_pos_create(r.x, r.y));

            sprintf(out_val, "%s%s%sa pile of %d items [Enter,%s]",
                s1, s2, s3, obj_ct, info);
            prt(out_val, 0, 0);
            query = inkey();
            screen_load();

            if (query != '\\' && query != '\r')
            {
                doc_free(doc);
                inv_free(inv);
                return query;
            }
            if (query == '\r' && top + lines < obj_ct) top++;
            if (query == '\\' && top > 0) top--;
        }
        /* unreachable */
    }
    assert(obj_ct == 0);
    inv_free(inv);

    /* Feature code (applying "mimic" field) */
    feat = get_feat_mimic(c_ptr);

    /* Require knowledge about grid, or ability to see grid */
    if (!(c_ptr->info & CAVE_MARK) && !player_can_see_bold(y, x))
    {
        /* Forget feature */
        feat = feat_none;
    }

    f_ptr = &f_info[feat];

    /* Terrain feature if needed */
    if (boring || have_flag(f_ptr->flags, FF_REMEMBER))
    {
        cptr name;

        /* Hack -- special handling for quest entrances */
        if (have_flag(f_ptr->flags, FF_QUEST_ENTER))
        {
            quest_ptr q = quests_get(c_ptr->special);
            name = format("the entrance to the quest '%s'(level %d)", q->name, q->level);
        }
        else if (have_flag(f_ptr->flags, FF_ENTRANCE))
        {
            dun_type_ptr type = dun_types_lookup(c_ptr->special);
            if (type->flags & DF_RANDOM)
                name = format("%s (level ?)", type->name);
            else
                name = format("%s (level %d)", type->name, type->min_dun_lvl);
        }
        else if (have_flag(f_ptr->flags, FF_TOWN))
        {
            name = town_name(c_ptr->special);
        }
        else
        {
            name = f_name + f_ptr->name;
        }


        /* Pick a prefix */
        if (*s2 &&
            ((!have_flag(f_ptr->flags, FF_MOVE) && !have_flag(f_ptr->flags, FF_CAN_FLY)) ||
             (!have_flag(f_ptr->flags, FF_LOS) && !have_flag(f_ptr->flags, FF_TREE)) ||
             have_flag(f_ptr->flags, FF_TOWN)))
        {
            s2 = "in ";
        }

        /* Hack -- special introduction for store & building doors -KMW- */
        if (have_flag(f_ptr->flags, FF_STORE) ||
            have_flag(f_ptr->flags, FF_QUEST_ENTER) ||
            have_flag(f_ptr->flags, FF_BLDG) ||
            have_flag(f_ptr->flags, FF_ENTRANCE))
        {
            s3 = "";
        }
        else if (have_flag(f_ptr->flags, FF_FLOOR) ||
             have_flag(f_ptr->flags, FF_TOWN) ||
             have_flag(f_ptr->flags, FF_SHALLOW) ||
             have_flag(f_ptr->flags, FF_DEEP))
        {
            s3 ="";
        }
        else
        {
            /* Pick proper indefinite article */
            s3 = (is_a_vowel(name[0])) ? "an " : "a ";
        }

        if (p_ptr->wizard)
        {
            char f_idx_str[32];
            if (c_ptr->mimic) sprintf(f_idx_str, "%d/%d", c_ptr->feat, c_ptr->mimic);
            else sprintf(f_idx_str, "%d", c_ptr->feat);
            sprintf(out_val, "%s%s%s%s [%s] %x %s (%d,%d)", s1, s2, s3, name, info, c_ptr->info, f_idx_str, y, x);
        }
        else if (display_distance)
        {
            /* Note: c_ptr->dist != m_ptr->cdis. The cave distance is not the range as diagonals count as 1, not 1.5
               Use distance calculation from update_mon, which sets m_ptr->cdis.*/
            int d = point_fast_distance(p_ptr->pos, point_create(x, y));
            sprintf(out_val, "%s%s%s%s [%s] (Rng %d)", s1, s2, s3, name, info, d);
        }
        else
            sprintf(out_val, "%s%s%s%s [%s]", s1, s2, s3, name, info);

        prt(out_val, 0, 0);
        move_cursor_relative(point_create(x, y));
        query = inkey();

        /* Always stop at "normal" keys */
        if ((query != '\r') && (query != '\n') && (query != ' ')) return query;
    }

    /* Stop on everything but "return" */
    if ((query != '\r') && (query != '\n')) return query;

    /* Repeat forever */
    return 0;
}


/*
 * Handle "target" and "look".
 *
 * Note that this code can be called from "get_fire_dir()".
 *
 * All locations must be on the current panel. Consider the use of
 * "panel_bounds()" to allow "off-panel" targets, perhaps by using
 * some form of "scrolling" the map around the cursor. XXX XXX XXX
 * That is, consider the possibility of "auto-scrolling" the screen
 * while the cursor moves around. This may require changes in the
 * "update_mon()" code to allow "visibility" even if off panel, and
 * may require dynamic recalculation of the "temp" grid set.
 *
 * Hack -- targeting/observing an "outer border grid" may induce
 * problems, so this is not currently allowed.
 *
 * The player can use the direction keys to move among "interesting"
 * grids in a heuristic manner, or the "space", "+", and "-" keys to
 * move through the "interesting" grids in a sequential manner, or
 * can enter "location" mode, and use the direction keys to move one
 * grid at a time in any direction. The "t" (set target) command will
 * only target a monster (as opposed to a location) if the monster is
 * target_able and the "interesting" mode is being used.
 *
 * The current grid is described using the "look" method above, and
 * a new command may be entered at any time, but note that if the
 * "TARGET_LOOK" bit flag is set (or if we are in "location" mode,
 * where "space" has no obvious meaning) then "space" will scan
 * through the description of the current grid until done, instead
 * of immediately jumping to the next "interesting" grid. This
 * allows the "target" command to retain its old semantics.
 *
 * The "*", "+", and "-" keys may always be used to jump immediately
 * to the next (or previous) interesting grid, in the proper mode.
 *
 * The "return" key may always be used to scan through a complete
 * grid description (forever).
 *
 * This command will cancel any old target, even if used from
 * inside the "look" command.
 */
bool target_set(int mode)
{
    int        i, d, m, t, bd;
    int        y = p_ptr->pos.y;
    int        x = p_ptr->pos.x;
    bool       done = FALSE;
    bool       flag = TRUE;
    char       query;
    char       info[80];

    mon_ptr    mon;
    rect_t     map_rect = ui_map_rect();
    point_t    min_pos = rect_top_left(cave->rect);
    point_t    max_pos = rect_bottom_right(cave->rect);


    /* Cancel target */
    target_who = 0;


    /* Cancel tracking */
    /* health_track(0); */


    /* Prepare the "temp" array */
    target_set_prepare(mode);

    /* Start near the player */
    m = 0;

    /* Interact */
    while (!done)
    {
        /* Interesting grids */
        if (flag && temp_n)
        {
            y = temp_y[m];
            x = temp_x[m];

            if ( !(mode & TARGET_LOOK)
              && !(mode & TARGET_MARK) )
            {
                prt_path(y, x, (mode & TARGET_DISI) ? PROJECT_DISI : 0);
            }

            /* Access */
            mon = mon_at_xy(x, y);

            /* Allow target */
            if ( (mon && target_able(mon->id))
              || ((mode & (TARGET_MARK|TARGET_DISI)) && mon && mon->ml) )
            {
                strcpy(info, "q,t,p,o,+,-,?,<dir>");
            }

            /* Dis-allow target */
            else
            {
                strcpy(info, "q,p,o,+,-,?,<dir>");
            }

            /* Describe and Prompt */
            while (!(query = target_set_aux(y, x, mode, info)));

            /* Cancel tracking */
            /* health_track(0); */

            /* Assume no "direction" */
            d = 0;

            if (use_menu)
            {
                if (query == '\r') query = 't';
            }

            /* Analyze */
            switch (query)
            {
                case '?':
                    screen_save();
                    show_file(TRUE, "context_targetting.txt", NULL, 0, 0);
                    screen_load();
                    break;
                case ESCAPE:
                case 'q':
                {
                    done = TRUE;
                    break;
                }

                case 't':
                case '.':
                case '5':
                case '0':
                {
                    if ( (mon && target_able(mon->id))
                      || ((mode & (TARGET_MARK|TARGET_DISI)) && mon && mon->ml) )
                    {
                        health_track(mon->id);
                        target_who = mon->id;
                        target_row = y;
                        target_col = x;
                        done = TRUE;
                    }
                    else
                    {
                        bell();
                    }
                    break;
                }

                case ' ':
                case '*':
                case '+':
                {
                    if (++m == temp_n)
                    {
                        m = 0;
                        if (!expand_list) done = TRUE;
                    }
                    break;
                }

                case '-':
                {
                    if (m-- == 0)
                    {
                        m = temp_n - 1;
                        if (!expand_list) done = TRUE;
                    }
                    break;
                }

                case 'p':
                {
                    /* Recenter the map around the player */
                    viewport_verify();

                    /* Update stuff */
                    p_ptr->update |= (PU_MONSTERS);

                    /* Redraw map */
                    p_ptr->redraw |= (PR_MAP);

                    /* Window stuff */
                    p_ptr->window |= (PW_OVERHEAD);

                    /* Handle stuff */
                    handle_stuff();

                    /* Recalculate interesting grids */
                    target_set_prepare(mode);

                    y = p_ptr->pos.y;
                    x = p_ptr->pos.x;
                }

                case 'o':
                {
                    flag = FALSE;
                    break;
                }

                case 'm':
                {
                    break;
                }

                default:
                {
                    /* Extract the action (if any) */
                    d = get_keymap_dir(query);

                    if (!d) bell();
                    break;
                }
            }

            /* Hack -- move around */
            if (d)
            {
                /* Modified to scroll to monster */
                int y2 = viewport_origin.y;
                int x2 = viewport_origin.x;

                /* Find a new monster */
                i = target_pick(temp_y[m], temp_x[m], ddy[d], ddx[d]);

                /* Request to target past last interesting grid */
                while (flag && (i < 0))
                {
                    /* Note the change */
                    if (viewport_scroll(ddy[d], ddx[d]))
                    {
                        int v = temp_y[m];
                        int u = temp_x[m];

                        /* Recalculate interesting grids */
                        target_set_prepare(mode);

                        /* Look at interesting grids */
                        flag = TRUE;

                        /* Find a new monster */
                        i = target_pick(v, u, ddy[d], ddx[d]);

                        /* Use that grid */
                        if (i >= 0) m = i;
                    }

                    /* Nothing interesting */
                    else
                    {
                        int dx = ddx[d];
                        int dy = ddy[d];

                        /* Restore previous position */
                        viewport_origin.y = y2;
                        viewport_origin.x = x2;

                        /* Update stuff */
                        p_ptr->update |= (PU_MONSTERS); /* XXX Why? */

                        /* Redraw map */
                        p_ptr->redraw |= (PR_MAP);

                        /* Window stuff */
                        p_ptr->window |= (PW_OVERHEAD);

                        /* Handle stuff */
                        redraw_hack = TRUE;
                        handle_stuff();
                        redraw_hack = FALSE;

                        /* Recalculate interesting grids */
                        target_set_prepare(mode);

                        /* Look at boring grids */
                        flag = FALSE;

                        /* Move */
                        x += dx;
                        y += dy;

                        /* Do not move horizontally if unnecessary */
                        if (((x < viewport_origin.x + map_rect.cx / 2) && (dx > 0)) ||
                             ((x > viewport_origin.x + map_rect.cx / 2) && (dx < 0)))
                        {
                            dx = 0;
                        }

                        /* Do not move vertically if unnecessary */
                        if (((y < viewport_origin.y + map_rect.cy / 2) && (dy > 0)) ||
                             ((y > viewport_origin.y + map_rect.cy / 2) && (dy < 0)))
                        {
                            dy = 0;
                        }

                        /* Apply the motion */
                        if (!cave_xy_is_visible(x, y))
                        {
                            if (viewport_scroll(dy, dx)) target_set_prepare(mode);
                        }

                        /* Slide into legality */
                        if (x >= max_pos.x) x = max_pos.x - 1;
                        else if (x <= min_pos.x) x = min_pos.x + 1;

                        if (y >= max_pos.y) y = max_pos.y - 1;
                        else if (y <= min_pos.y) y = min_pos.y + 1;
                    }
                }

                /* Use that grid */
                m = i;
            }
        }

        /* Arbitrary grids */
        else
        {
            bool move_fast = FALSE;

            if ( !(mode & TARGET_LOOK)
              && !(mode & TARGET_MARK) )
            {
                prt_path(y, x, (mode & TARGET_DISI) ? PROJECT_DISI : 0);
            }

            /* Access */
            mon = mon_at_xy(x, y);

            if ((mode & TARGET_MARK) && (!mon || !mon->ml))
                strcpy(info, "q,p,o,+,-,?,<dir>");
            else
                strcpy(info, "q,t,p,m,+,-,?,<dir>");


            /* Describe and Prompt (enable "TARGET_LOOK") */
            while (!(query = target_set_aux(y, x, mode | TARGET_LOOK, info)));

            /* Cancel tracking */
            /* health_track(0); */

            /* Assume no direction */
            d = 0;

            if (use_menu)
            {
                if (query == '\r') query = 't';
            }

            /* Analyze the keypress */
            switch (query)
            {
                case '?':
                    screen_save();
                    show_file(TRUE, "context_targetting.txt", NULL, 0, 0);
                    screen_load();
                    break;
                case ESCAPE:
                case 'q':
                {
                    done = TRUE;
                    break;
                }

                case 't':
                case '.':
                case '5':
                case '0':
                if ( !(mode & TARGET_MARK)
                  || (mon && mon->ml) )
                {
                    if (mode & TARGET_MARK)
                        target_who = mon->id;
                    else
                        target_who = -1; /* position target */
                    target_row = y;
                    target_col = x;
                    done = TRUE;
                }
                else
                {
                    bell();
                }
                break;

                case 'p':
                {
                    /* Recenter the map around the player */
                    viewport_verify();

                    /* Update stuff */
                    p_ptr->update |= (PU_MONSTERS);

                    /* Redraw map */
                    p_ptr->redraw |= (PR_MAP);

                    /* Window stuff */
                    p_ptr->window |= (PW_OVERHEAD);

                    /* Handle stuff */
                    handle_stuff();

                    /* Recalculate interesting grids */
                    target_set_prepare(mode);

                    y = p_ptr->pos.y;
                    x = p_ptr->pos.x;
                }

                case 'o':
                {
                    break;
                }

                case ' ':
                case '*':
                case '+':
                case '-':
                case 'm':
                {
                    flag = TRUE;

                    m = 0;
                    bd = 999;

                    /* Pick a nearby monster */
                    for (i = 0; i < temp_n; i++)
                    {
                        t = distance(y, x, temp_y[i], temp_x[i]);

                        /* Pick closest */
                        if (t < bd)
                        {
                            m = i;
                            bd = t;
                        }
                    }

                    /* Nothing interesting */
                    if (bd == 999) flag = FALSE;

                    break;
                }

                default:
                {
                    /* Extract the action (if any) */
                    d = get_keymap_dir(query);

                    /* XTRA HACK MOVEFAST */
                    if (isupper(query)) move_fast = TRUE;

                    if (!d) bell();
                    break;
                }
            }

            /* Handle "direction" */
            if (d)
            {
                int dx = ddx[d];
                int dy = ddy[d];

                /* XTRA HACK MOVEFAST */
                if (move_fast)
                {
                    int mag = MIN(map_rect.cx / 2, map_rect.cy / 2);
                    x += dx * mag;
                    y += dy * mag;
                }
                else
                {
                    x += dx;
                    y += dy;
                }

                /* Do not move horizontally if unnecessary */
                if (((x < viewport_origin.x + map_rect.cx / 2) && (dx > 0)) ||
                     ((x > viewport_origin.x + map_rect.cx / 2) && (dx < 0)))
                {
                    dx = 0;
                }

                /* Do not move vertically if unnecessary */
                if (((y < viewport_origin.y + map_rect.cy / 2) && (dy > 0)) ||
                     ((y > viewport_origin.y + map_rect.cy / 2) && (dy < 0)))
                {
                    dy = 0;
                }

                /* Apply the motion */
                if (!cave_xy_is_visible(x, y))
                {
                    if (viewport_scroll(dy, dx)) target_set_prepare(mode);
                }

                /* Slide into legality */
                if (x >= max_pos.x) x = max_pos.x - 1;
                else if (x <= min_pos.x) x = min_pos.x + 1;

                if (y >= max_pos.y) y = max_pos.y - 1;
                else if (y <= min_pos.y) y = min_pos.y + 1;
            }
        }
    }

    /* Forget */
    temp_n = 0;

    msg_line_clear();
    prt("", 0, 0);

    /* Recenter the map around the player */
    viewport_verify();

    /* Update stuff */
    p_ptr->update |= (PU_MONSTERS);

    /* Redraw map */
    p_ptr->redraw |= (PR_MAP | PR_HEALTH_BARS);

    /* Window stuff */
    p_ptr->window |= (PW_OVERHEAD | PW_MONSTER_LIST);

    /* Prevent losing visibility on newly acquired target (PU_MONSTERS above) */
    redraw_hack = TRUE;
    handle_stuff();
    redraw_hack = FALSE;

    /* Failure to set target */
    if (!target_who) return (FALSE);

    /* Success */
    return (TRUE);
}


/*
 * Get an "aiming direction" from the user.
 *
 * The "dir" is loaded with 1,2,3,4,6,7,8,9 for "actual direction", and
 * "0" for "current target", and "-1" for "entry aborted".
 *
 * Note that "Force Target", if set, will pre-empt user interaction,
 * if there is a usable target already set.
 *
 * Note that confusion over-rides any (explicit?) user choice.
 *
 */
bool get_fire_dir(int *dp) { return get_fire_dir_aux(dp, TARGET_KILL); }
bool get_fire_dir_aux(int *dp, int target_mode)
{
    bool valid_target = FALSE;
    if (use_old_target && target_okay_aux(target_mode))
        valid_target = TRUE;
    /* auto_target the closest monster if no valid target is selected up front */
    if (!valid_target && auto_target && !plr_tim_find(T_CONFUSED) && !plr_tim_find(T_HALLUCINATE))
    {
        int best_m_id = 0, best_dis = 9999;

        int_map_iter_ptr iter;
        for (iter = int_map_iter_alloc(cave->mon);
                int_map_iter_is_valid(iter);
                int_map_iter_next(iter))
        {
            mon_ptr mon = int_map_iter_current(iter);
            if (!mon->ml) continue;
            if (!target_pet && is_pet(mon)) continue;
            if (target_mode != TARGET_DISI && !plr_project_mon(mon)) continue;
            if (mon->cdis < best_dis)
            {
                best_dis = mon->cdis;
                best_m_id = mon->id;
            }
        }
        int_map_iter_free(iter);

        if (best_m_id)
        {
            target_who = best_m_id;
            target_row = dun_mon(cave, best_m_id)->pos.y; /* XXX */
            target_col = dun_mon(cave, best_m_id)->pos.x; /* XXX */
            *dp = 5;
            p_ptr->redraw |= PR_HEALTH_BARS;
            return TRUE;
        }
    }
    /* fall back on normal target selection */
    return get_aim_dir_aux(dp, target_mode);
}

bool get_aim_dir(int *dp) { return get_aim_dir_aux(dp, TARGET_KILL); }
bool get_aim_dir_aux(int *dp, int target_mode)
{
    int        dir;

    char    command;

    cptr    p;

    /* Initialize */
    (*dp) = 0;

    /* Global direction */
    dir = command_dir;

    /* Hack -- auto-target if requested */
    if (use_old_target && target_okay_aux(target_mode)) dir = 5;

#ifdef ALLOW_REPEAT /* TNB */

    if (repeat_pull(dp))
    {
        /* Confusion? */

        /* Verify */
        if (!(*dp == 5 && !target_okay_aux(target_mode)))
        {
/*            return (TRUE); */
            dir = *dp;
        }
    }

#endif /* ALLOW_REPEAT -- TNB */

    /* Ask until satisfied */
    while (!dir)
    {
        /* Choose a prompt */
        if (!target_okay_aux(target_mode))
        {
            p = "Direction ('*' to choose a target, Escape to cancel)? ";

        }
        else
        {
            p = "Direction ('5' for target, '*' to re-target, Escape to cancel)? ";

        }

        /* Get a command (or Cancel) */
        if (!get_com(p, &command, TRUE)) break;

        if (use_menu)
        {
            if (command == '\r') command = 't';
        }

        /* Convert various keys to "standard" keys */
        switch (command)
        {
            /* Use current target */
            case 'T':
            case 't':
            case '.':
            case '5':
            case '0':
            {
                dir = 5;
                break;
            }

            /* Set new target */
            case '*':
            case ' ':
            case '\r':
            {
                if (target_set(target_mode)) dir = 5;
                break;
            }

            default:
            {
                /* Extract the action (if any) */
                dir = get_keymap_dir(command);

                break;
            }
        }

        /* Verify requested targets */
        if ((dir == 5) && !target_okay_aux(target_mode)) dir = 0;

        /* Error */
        if (!dir) bell();
    }

    /* No direction */
    if (!dir)
    {
        project_length = 0; /* reset to default */
        return (FALSE);
    }

    /* Save the direction */
    command_dir = dir;

    /* Check for confusion */
    if (plr_tim_find(T_CONFUSED))
    {
        /* XXX XXX XXX */
        /* Random direction */
        dir = ddd[randint0(8)];
    }

    /* Notice confusion */
    if (command_dir != dir)
    {
        /* Warn the user */
        msg_print("You are confused.");

    }

    /* Save direction */
    (*dp) = dir;

#ifdef ALLOW_REPEAT /* TNB */

/*    repeat_push(dir); */
    repeat_push(command_dir);

#endif /* ALLOW_REPEAT -- TNB */

    /* A "valid" direction was entered */
    return (TRUE);
}



/*
 * Request a "movement" direction (1,2,3,4,6,7,8,9) from the user,
 * and place it into "command_dir", unless we already have one.
 *
 * This function should be used for all "repeatable" commands, such as
 * run, walk, open, close, bash, disarm, spike, tunnel, etc, as well
 * as all commands which must reference a grid adjacent to the player,
 * and which may not reference the grid under the player. Note that,
 * for example, it is no longer possible to "disarm" or "open" chests
 * in the same grid as the player.
 *
 * Direction "5" is illegal and will (cleanly) abort the command.
 *
 * This function tracks and uses the "global direction", and uses
 * that as the "desired direction", to which "confusion" is applied.
 */
int get_rep_dir(int *dp, bool under)
{
    int result = GET_DIR_OK;
    int dir;

    /* Initialize */
    (*dp) = 0;

    /* Global direction */
    dir = command_dir;

#ifdef ALLOW_REPEAT /* TNB */

    if (repeat_pull(dp))
    {
        dir = *dp;
/*        return (TRUE); */
    }

#endif /* ALLOW_REPEAT -- TNB */

    /* Get a direction */
    while (!dir)
    {
        char ch;

        /* Get a command (or Cancel) */
        if (!get_com("Direction (Escape to cancel)? ", &ch, TRUE)) break;


        /* Look up the direction */
        dir = get_keymap_dir(ch);

        /* Oops */
        if (!dir) bell();
    }

    /* Prevent weirdness */
    if ((dir == 5) && (!under)) dir = 0;

    /* Aborted */
    if (!dir) return (FALSE);

    /* Save desired direction */
    command_dir = dir;

    /* Apply "confusion" */
    if (plr_tim_find(T_CONFUSED))
    {
        /* Standard confusion */
        if (randint0(100) < 75)
        {
            /* Random direction */
            dir = ddd[randint0(8)];
        }
    }
    else if (p_ptr->riding)
    {
        monster_type *m_ptr = dun_mon(cave, p_ptr->riding);
        monster_race *r_ptr = &r_info[m_ptr->r_idx];

        if (mon_tim_find(m_ptr, T_CONFUSED))
        {
            /* Standard confusion */
            if (randint0(100) < 75)
            {
                /* Random direction */
                dir = ddd[randint0(8)];
            }
        }
        else if ((r_ptr->flags1 & RF1_RAND_50) && (r_ptr->flags1 & RF1_RAND_25) && (randint0(100) < 50))
        {
            /* Random direction */
            dir = ddd[randint0(8)];
        }
        else if ((r_ptr->flags1 & RF1_RAND_50) && (randint0(100) < 25))
        {
            /* Random direction */
            dir = ddd[randint0(8)];
        }
    }
    else if (p_ptr->move_random)
    {
        if (one_in_(66))
        {
            dir = ddd[randint0(8)];
        }
    }

    /* Notice confusion */
    if (command_dir != dir)
    {
        if (plr_tim_find(T_CONFUSED))
            msg_print("You are confused.");
        else if (p_ptr->move_random)
            cmsg_print(TERM_YELLOW, "You are moving erratically.");
        else
        {
            char m_name[80];
            monster_type *m_ptr = dun_mon(cave, p_ptr->riding);

            monster_desc(m_name, m_ptr, 0);
            if (mon_tim_find(m_ptr, T_CONFUSED))
                msg_format("%^s is confused.", m_name);
            else
                msg_format("You cannot control %s.", m_name);
        }
        /* Block running in random directions */
        result = GET_DIR_RANDOM;
    }

    /* Save direction */
    (*dp) = dir;

#ifdef ALLOW_REPEAT /* TNB */

/*    repeat_push(dir); */
    repeat_push(command_dir);

#endif /* ALLOW_REPEAT -- TNB */

    /* Success */
    return result;
}


bool get_rep_dir2(int *dp)
{
    int dir;

    /* Initialize */
    (*dp) = 0;

    /* Global direction */
    dir = command_dir;

#ifdef ALLOW_REPEAT /* TNB */

    if (repeat_pull(dp))
    {
        dir = *dp;
/*        return (TRUE); */
    }

#endif /* ALLOW_REPEAT -- TNB */

    /* Get a direction */
    while (!dir)
    {
        char ch;

        /* Get a command (or Cancel) */
        if (!get_com("Direction (Escape to cancel)? ", &ch, TRUE)) break;


        /* Look up the direction */
        dir = get_keymap_dir(ch);

        /* Oops */
        if (!dir) bell();
    }

    /* Prevent weirdness */
    if (dir == 5) dir = 0;

    /* Aborted */
    if (!dir) return (FALSE);

    /* Save desired direction */
    command_dir = dir;

    /* Apply "confusion" */
    if (plr_tim_find(T_CONFUSED))
    {
        /* Standard confusion */
        if (randint0(100) < 75)
        {
            /* Random direction */
            dir = ddd[randint0(8)];
        }
    }

    /* Notice confusion */
    if (command_dir != dir)
    {
        /* Warn the user */
        msg_print("You are confused.");

    }

    /* Save direction */
    (*dp) = dir;

#ifdef ALLOW_REPEAT /* TNB */

/*    repeat_push(dir); */
    repeat_push(command_dir);

#endif /* ALLOW_REPEAT -- TNB */

    /* Success */
    return (TRUE);
}



/*
 * XAngband: Prepare the "temp" array for "tget_pt"
 * based on target_set_prepare funciton.
 */
static void tgt_pt_prepare(void)
{
    if (!expand_list) return;

    temp_n = 0;

    temp_x[temp_n] = p_ptr->pos.x;
    temp_y[temp_n++] = p_ptr->pos.y;

    if (dun_pos_interior(cave, travel.pos))
    {
        temp_x[temp_n] = travel.pos.x;
        temp_y[temp_n++] = travel.pos.y;
    }

    if (!plr_tim_find(T_HALLUCINATE))
    {
        dun_stairs_ptr stairs;

        for (stairs = cave->stairs; stairs; stairs = stairs->next)
        {
            if (cave_at(stairs->pos_here)->info & CAVE_MARK)
            {
                temp_x[temp_n] = stairs->pos_here.x;
                temp_y[temp_n++] = stairs->pos_here.y;
            }
        }
    }

    ang_sort_comp = ang_sort_comp_distance;
    ang_sort_swap = ang_sort_swap_distance;

    ang_sort(temp_x, temp_y, temp_n);
}

/*
 * old -- from PsiAngband.
 */
bool tgt_pt(int *x_ptr, int *y_ptr, int rng)
{
    char ch = 0;
    int d, x, y, n = 0;
    bool success = FALSE;
    rect_t map_rect = ui_map_rect();
    point_t min_pos = rect_top_left(cave->rect);
    point_t max_pos = rect_bottom_right(cave->rect);

    x = p_ptr->pos.x;
    y = p_ptr->pos.y;

    if (expand_list)
    {
        tgt_pt_prepare();
        n = 0;
    }

    msg_print("Select a point and press <color:y>space</color>.");

    while ((ch != ESCAPE) && !success)
    {
        bool move_fast = FALSE;

        move_cursor_relative(point_create(x, y));
        ch = inkey();
        switch (ch)
        {
        case ESCAPE:
            break;
        case ' ':
        case 't':
        case '.':
        case '5':
        case '0':
            /* illegal place */
            if (player_bold(y, x)) ch = 0;

            /* okay place */
            else success = TRUE;

            break;

        case '`':
            if (expand_list && temp_n)
            {
                n++;

                while(n < temp_n)
                {
                    point_t p = point_create(temp_x[n], temp_y[n]);
                    if (point_equals(p, travel.pos)) break;
                    n++;
                }

                if (n == temp_n)    /* Loop out taget list */
                {
                    n = 0;
                    y = p_ptr->pos.y;
                    x = p_ptr->pos.x;
                    viewport_verify();    /* Move cursor to player */

                    /* Update stuff */
                    p_ptr->update |= (PU_MONSTERS);

                    /* Redraw map */
                    p_ptr->redraw |= (PR_MAP);

                    /* Window stuff */
                    p_ptr->window |= (PW_OVERHEAD);

                    /* Handle stuff */
                    handle_stuff();
                }
                else 
                {
                    y = temp_y[n];
                    x = temp_x[n];
                    viewport_verify_aux(point_create(x, y), VIEWPORT_FORCE_CENTER);
                    p_ptr->redraw |= (PR_MAP);
                    redraw_hack = TRUE;
                    handle_stuff();
                    redraw_hack = FALSE;
                }
            }
            break;

        /* XAngband: Move cursor to stairs */
        case '>':
        case '<':
            if (expand_list && temp_n)
            {
                n++;

                while(n < temp_n)    /* Skip stairs which have defferent distance */
                {
                    cave_type *c_ptr = cave_at_xy(temp_x[n], temp_y[n]);

                    if (ch == '>')
                    {
                        if (!cave_have_flag_grid(c_ptr, FF_MORE))
                            n++;
                        else
                            break;
                    }
                    else /* if (ch == '<') */
                    {
                        if (!cave_have_flag_grid(c_ptr, FF_LESS))
                            n++;
                        else
                            break;
                    }
                }

                if (n == temp_n)    /* Loop out taget list */
                {
                    n = 0;
                    y = p_ptr->pos.y;
                    x = p_ptr->pos.x;
                    viewport_verify();    /* Move cursor to player */

                    /* Update stuff */
                    p_ptr->update |= (PU_MONSTERS);

                    /* Redraw map */
                    p_ptr->redraw |= (PR_MAP);

                    /* Window stuff */
                    p_ptr->window |= (PW_OVERHEAD);

                    /* Handle stuff */
                    handle_stuff();
                }
                else    /* move cursor to next stair and change panel */
                {
                    y = temp_y[n];
                    x = temp_x[n];
                    viewport_verify_aux(point_create(x, y), VIEWPORT_FORCE_CENTER);
                    p_ptr->redraw |= (PR_MAP);
                    redraw_hack = TRUE;
                    handle_stuff();
                    redraw_hack = FALSE;
                }
            }
            break;

        default:
            /* Look up the direction */
            d = get_keymap_dir(ch);

            /* XTRA HACK MOVEFAST */
            if (isupper(ch)) move_fast = TRUE;

            /* Handle "direction" */
            if (d)
            {
                int dx = ddx[d];
                int dy = ddy[d];
                int old_x = x;
                int old_y = y;

                /* XTRA HACK MOVEFAST */
                if (move_fast)
                {
                    int mag = MIN(map_rect.cx / 2, map_rect.cy / 2);
                    x += dx * mag;
                    y += dy * mag;
                }
                else
                {
                    x += dx;
                    y += dy;
                }

                if (rng > 0 && distance(p_ptr->pos.y, p_ptr->pos.x, y, x) > rng)
                {
                    bell();
                    x = old_x;
                    y = old_y;
                    continue;
                }

                /* Do not move horizontally if unnecessary */
                if (((x < viewport_origin.x + map_rect.cx / 2) && (dx > 0)) ||
                     ((x > viewport_origin.x + map_rect.cx / 2) && (dx < 0)))
                {
                    dx = 0;
                }

                /* Do not move vertically if unnecessary */
                if (((y < viewport_origin.y + map_rect.cy / 2) && (dy > 0)) ||
                     ((y > viewport_origin.y + map_rect.cy / 2) && (dy < 0)))
                {
                    dy = 0;
                }

                /* Apply the motion */
                if (!cave_xy_is_visible(x, y))
                {
                    /* if (change_panel(dy, dx)) target_set_prepare(mode); */
                    viewport_scroll(dy, dx);
                }

                /* Slide into legality */
                if (x >= max_pos.x) x = max_pos.x - 1;
                else if (x <= min_pos.x) x = min_pos.x + 1;

                if (y >= max_pos.y) y = max_pos.y - 1;
                else if (y <= min_pos.y) y = min_pos.y + 1;
            }
            break;
        }
    }

    msg_line_clear();

    /* Recenter the map around the player */
    viewport_verify();

    /* Update stuff */
    p_ptr->update |= (PU_MONSTERS);

    /* Redraw map */
    p_ptr->redraw |= (PR_MAP);

    /* Window stuff */
    p_ptr->window |= (PW_OVERHEAD);

    /* Handle stuff */
    handle_stuff();

    *x_ptr = x;
    *y_ptr = y;
    return success;
}


bool get_hack_dir(int *dp)
{
    int        dir;
    cptr    p;
    char    command;


    /* Initialize */
    (*dp) = 0;

    /* Global direction */
    dir = 0;

    /* (No auto-targeting) */

    /* Ask until satisfied */
    while (!dir)
    {
        /* Choose a prompt */
        if (!target_okay())
        {
            p = "Direction ('*' to choose a target, Escape to cancel)? ";

        }
        else
        {
            p = "Direction ('5' for target, '*' to re-target, Escape to cancel)? ";

        }

        /* Get a command (or Cancel) */
        if (!get_com(p, &command, TRUE)) break;

        if (use_menu)
        {
            if (command == '\r') command = 't';
        }

        /* Convert various keys to "standard" keys */
        switch (command)
        {
            /* Use current target */
            case 'T':
            case 't':
            case '.':
            case '5':
            case '0':
            {
                dir = 5;
                break;
            }

            /* Set new target */
            case '*':
            case ' ':
            case '\r':
            {
                if (target_set(TARGET_KILL)) dir = 5;
                break;
            }

            default:
            {
                /* Look up the direction */
                dir = get_keymap_dir(command);

                break;
            }
        }

        /* Verify requested targets */
        if ((dir == 5) && !target_okay()) dir = 0;

        /* Error */
        if (!dir) bell();
    }

    /* No direction */
    if (!dir) return (FALSE);

    /* Save the direction */
    command_dir = dir;

    /* Check for confusion */
    if (plr_tim_find(T_CONFUSED))
    {
        /* XXX XXX XXX */
        /* Random direction */
        dir = ddd[randint0(8)];
    }

    /* Notice confusion */
    if (command_dir != dir)
    {
        /* Warn the user */
        msg_print("You are confused.");

    }

    /* Save direction */
    (*dp) = dir;

    /* A "valid" direction was entered */
    return (TRUE);
}



#define Go_no_JuuJou 5*5*5*5*5*5*5*5*5*5

s16b gain_energy(void)
{
    int i;
    s32b energy_result = 10;
    s32b tmp;

    tmp = randint0(Go_no_JuuJou);

    for (i = 0; i < 9; i ++){
        energy_result += tmp % 5;
        tmp /= 5;
    }

    return (s16b)(energy_result + tmp);
}


/*
 * Return bow energy
 */
int bow_energy(int sval)
{
    int energy = 10000;

    /* Analyze the launcher */
    switch (sval)
    {
        /* Sling and ammo */
        case SV_SLING:
        {
            /*energy = 8000;*/
            energy = 7150;
            break;
        }

        /* Short Bow and Arrow */
        case SV_SHORT_BOW:
        {
            energy = 8888;
            break;
        }

        /* Long Bow and Arrow */
        case SV_LONG_BOW:
        case SV_GREAT_BOW:
        case SV_CRIMSON:
        case SV_RAILGUN:
        case SV_HARP:
        {
            energy = 10000;
            break;
        }

        /* Light Crossbow and Bolt */
        case SV_LIGHT_XBOW:
        {
            energy = 12000;
            break;
        }

        /* Heavy Crossbow and Bolt */
        case SV_HEAVY_XBOW:
        {
            energy = 13333;
            break;
        }
    }

    return (energy);
}

/*
 * Return alignment title
 */
cptr your_alignment(void)
{
    if (p_ptr->align > 150) return "Lawful";
    else if (p_ptr->align > 50) return "Good";
    else if (p_ptr->align > 10) return "Neutral Good";
    else if (p_ptr->align > -11) return "Neutral";
    else if (p_ptr->align > -51) return "Neutral Evil";
    else if (p_ptr->align > -151) return "Evil";
    else return "Chaotic";
}


/*
 * Return proficiency level of weapons and misc. skills (except riding)
 */
int weapon_exp_level(int weapon_exp)
{
    if (weapon_exp < WEAPON_EXP_BEGINNER) return EXP_LEVEL_UNSKILLED;
    else if (weapon_exp < WEAPON_EXP_SKILLED) return EXP_LEVEL_BEGINNER;
    else if (weapon_exp < WEAPON_EXP_EXPERT) return EXP_LEVEL_SKILLED;
    else if (weapon_exp < WEAPON_EXP_MASTER) return EXP_LEVEL_EXPERT;
    else return EXP_LEVEL_MASTER;
}


/*
 * Return proficiency level of riding
 */
int riding_exp_level(int riding_exp)
{
    if (riding_exp < RIDING_EXP_BEGINNER) return EXP_LEVEL_UNSKILLED;
    else if (riding_exp < RIDING_EXP_SKILLED) return EXP_LEVEL_BEGINNER;
    else if (riding_exp < RIDING_EXP_EXPERT) return EXP_LEVEL_SKILLED;
    else if (riding_exp < RIDING_EXP_MASTER) return EXP_LEVEL_EXPERT;
    else return EXP_LEVEL_MASTER;
}


/*
 * Return proficiency level of spells
 */
int spell_exp_level(int spell_exp)
{
    if (spell_exp < SPELL_EXP_BEGINNER) return EXP_LEVEL_UNSKILLED;
    else if (spell_exp < SPELL_EXP_SKILLED) return EXP_LEVEL_BEGINNER;
    else if (spell_exp < SPELL_EXP_EXPERT) return EXP_LEVEL_SKILLED;
    else if (spell_exp < SPELL_EXP_MASTER) return EXP_LEVEL_EXPERT;
    else return EXP_LEVEL_MASTER;
}
