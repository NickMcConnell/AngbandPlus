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
    bool android = (plr->prace == RACE_ANDROID ? TRUE : FALSE);
    int base = (android ? _player_exp_a : _player_exp)[level-1];
    if (base % 100 == 0)
        return base / 100 * plr->expfact;
    else
        return base * plr->expfact / 100;
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

        cnv_stat(plr->stat_max[0], tmp);
        put_str(format("        a) Str (cur %6.6s)              ", tmp), 2, 14);
        cnv_stat(plr->stat_max[1], tmp);
        put_str(format("        b) Int (cur %6.6s)              ", tmp), 3, 14);
        cnv_stat(plr->stat_max[2], tmp);
        put_str(format("        c) Wis (cur %6.6s)              ", tmp), 4, 14);
        cnv_stat(plr->stat_max[3], tmp);
        put_str(format("        d) Dex (cur %6.6s)              ", tmp), 5, 14);
        cnv_stat(plr->stat_max[4], tmp);
        put_str(format("        e) Con (cur %6.6s)              ", tmp), 6, 14);
        cnv_stat(plr->stat_max[5], tmp);
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
    int  old_lev = plr->lev;

    /* Hack -- lower limit */
    if (plr->exp < 0) plr->exp = 0;
    if (plr->max_exp < 0) plr->max_exp = 0;
    if (plr->max_max_exp < 0) plr->max_max_exp = 0;

    /* Hack -- upper limit */
    if (plr->exp > PY_MAX_EXP) plr->exp = PY_MAX_EXP;
    if (plr->max_exp > PY_MAX_EXP) plr->max_exp = PY_MAX_EXP;
    if (plr->max_max_exp > PY_MAX_EXP) plr->max_max_exp = PY_MAX_EXP;

    /* Hack -- maintain "max" experience */
    if (plr->exp > plr->max_exp) plr->max_exp = plr->exp;

    /* Hack -- maintain "max max" experience */
    if (plr->max_exp > plr->max_max_exp) plr->max_max_exp = plr->max_exp;

    /* Redraw experience */
    plr->redraw |= (PR_EXP);

    /* Lose levels while possible */
    while ((plr->lev > 1) &&
           (plr->exp < exp_requirement(plr->lev - 1)))
    {
        plr->lev--;
        plr->update |= (PU_BONUS | PU_INNATE | PU_HP | PU_MANA | PU_SPELLS);
        plr->redraw |= (PR_LEV);
    }

    /* Gain levels while possible */
    while ((plr->lev < PY_MAX_LEVEL) &&
           (plr->exp >= exp_requirement(plr->lev)))
    {
        plr->lev++;

        if (plr->pclass == CLASS_WILD_TALENT) wild_talent_fix_up();

        /* Save the highest level */
        if (plr->lev > plr->max_plv)
        {
            class_t *class_ptr = get_class();

            plr->max_plv = plr->lev;

            sound(SOUND_LEVEL);
            cmsg_format(TERM_L_GREEN, "Welcome to level %d.", plr->lev);

            if (class_ptr->hooks.gain_level != NULL)
                (class_ptr->hooks.gain_level)(plr->lev);

            if (mut_present(MUT_CHAOS_GIFT))
                chaos_warrior_reward();

            /* N.B. The class hook or the Chaos Gift mutation may result in a race
               change (stupid Chaos-Warriors), so we better always requery the player's
               race to make sure the correct racial hook is called. */
            {
                race_t *race_ptr = get_true_race(); /* So players don't miss if they Polymorph Demon, etc */

                if (plr->prace == RACE_DOPPELGANGER) /* But a doppelganger should use the mimicked race! */
                    race_ptr = get_race();

                if (race_ptr->hooks.gain_level != NULL)
                    (race_ptr->hooks.gain_level)(plr->lev);
            }
            if(plr->max_plv % 5 == 0)
                gain_chosen_stat();
        }
        plr->update |= (PU_BONUS | PU_INNATE | PU_HP | PU_MANA | PU_SPELLS);
        plr->redraw |= (PR_LEV);
        plr->window |= (PW_SPELL);
    }

    if (old_lev != plr->lev)
    {
        race_t *race_ptr = get_true_race(); /* So players don't miss if they Polymorph Demon, etc */

        if (plr->prace == RACE_DOPPELGANGER) /* But a doppelganger should use the mimicked race! */
            race_ptr = get_race();

        if (race_ptr->hooks.change_level)
            race_ptr->hooks.change_level(old_lev, plr->lev);

        autopick_load_pref(FALSE);
    }
    handle_stuff(); /* XXX only do this once at the end */
}


/*
 * Return monster death string
 */
cptr extract_note_dies(monster_race *r_ptr)
{
    if (!mon_race_is_living(r_ptr))
    {
        if (mon_blows_find(r_ptr->blows, RBM_EXPLODE))
            return " explodes into tiny shreds.";
        return " is destroyed.";
    }
    return " dies.";
}

static bool _mon_is_wanted(mon_ptr mon)
{
    if (mon_is_unique(mon) && !mon_is_cloned(mon))
    {
        int i;
        for (i = 0; i < MAX_KUBI; i++)
        {
            if (kubi_r_idx[i] == mon->race->id && !(mon->mflag2 & MFLAG2_CHAMELEON))
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

    case TV_LIGHT:
        switch (k_ptr->sval)
        {
        case SV_LIGHT_TORCH:
        case SV_LIGHT_LANTERN:
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

    monster_race *r_ptr = m_ptr->race;

    bool visible = ((m_ptr->ml && !plr_tim_find(T_HALLUCINATE)) || mon_race_is_unique(r_ptr));

    bool cloned = have_flag(m_ptr->smart, SM_CLONED);
    bool do_vampire_servant = FALSE;
    char m_name[MAX_NLEN];
    int corpse_chance = 3;

    object_type forge;
    object_type *q_ptr;

    bool drop_chosen_item = drop_item && !cloned && !mon_is_pet(m_ptr);


    monster_desc(m_name, m_ptr, MD_TRUE_NAME);

    /* The caster is dead? */
    if (world_monster == m_ptr) world_monster = 0;

    /* Notice changes in view */
    if (r_ptr->light || r_ptr->lantern)
    {
        plr->update |= PU_LIGHT; /* XXX dun_update_mon_lite optimization won't find a dead monster XXX */
    }

    if (mut_present(MUT_INFERNAL_DEAL) && plr_view(m_ptr->pos) && !mon_is_pet(m_ptr))
    {
        if ( plr->msp > 0
          && plr->pclass != CLASS_RUNE_KNIGHT
          && plr->pclass != CLASS_SAMURAI
          && plr->pclass != CLASS_MYSTIC )
        {
            hp_player_aux(10);
            sp_player(5);
        }
        else
            hp_player_aux(15);
    }

    if (mon_can_multiply(m_ptr))
        cave->breed_kill_ct++;

    /* Let monsters explode! */
    {mon_blow_ptr blow = mon_blows_find(r_ptr->blows, RBM_EXPLODE);
    if (blow && blow->effect_ct)
    {
        int gf = blow->effects[0].type;
        int damage = dice_roll(blow->effects[0].dice);

        mon_burst(m_ptr, 3, gf, damage);
    }}

    if (m_ptr->mflag2 & MFLAG2_CHAMELEON)
    {
        choose_new_monster(m_ptr, TRUE, mon_race_parse("R.chameleon"));
        r_ptr = m_ptr->race;
    }

    mon_tim_clear(m_ptr);
    quests_on_kill_mon(m_ptr);
    plr_hook_kill_monster(m_ptr);

    if (m_ptr->id == plr->riding)
    {
        if (rakuba(-1, FALSE))
        {
            msg_print("You have fallen from your riding pet.");
        }
    }

    if (plr->prace == RACE_MON_MIMIC && !have_flag(m_ptr->smart, SM_CLONED))
        mimic_on_kill_monster(m_ptr->race->id);

    if ( vampiric_drain_hack
      && mon_is_human(m_ptr)
      && !mon_is_pet(m_ptr)
      && randint1(plr->lev) >= 15 )
    {
        do_vampire_servant = TRUE;
    }

    /* Drop a dead corpse? */
    if (plr->prace == RACE_MON_POSSESSOR && plr_mon_race_is_("@.soul"))
        corpse_chance = 2;

    if ( (_mon_is_wanted(m_ptr) || (one_in_(corpse_chance) && !do_vampire_servant))
      && (r_ptr->body.flags & (RF_DROP_CORPSE | RF_DROP_SKELETON))
      && !(cloned || (m_ptr->race->id == today_mon && mon_is_pet(m_ptr))))
    {
        /* Assume skeleton */
        bool corpse = FALSE;

        do_vampire_servant = FALSE;

        /*
         * We cannot drop a skeleton? Note, if we are in this check,
         * we *know* we can drop at least a corpse or a skeleton
         */
        if (!(r_ptr->body.flags & RF_DROP_SKELETON))
            corpse = TRUE;
        else if ((r_ptr->body.flags & RF_DROP_CORPSE) && _mon_is_wanted(m_ptr))
            corpse = TRUE;
        else if ( (r_ptr->body.flags & RF_DROP_CORPSE)
               && plr->prace == RACE_MON_POSSESSOR
               && plr_mon_race_is_("@.soul") )
        {
            corpse = TRUE;
        }
        /* Else, a corpse is more likely unless we did a "lot" of damage */
        else if (r_ptr->body.flags & RF_DROP_CORPSE)
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
        q_ptr->race_id = m_ptr->race->id;
        q_ptr->ident |= IDENT_KNOWN;
        if (r_ptr->weight && plr->prace == RACE_MON_POSSESSOR)
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

    if (mon_race_is_(m_ptr->race, "u.horror.pink"))
    {
        bool notice = FALSE;
        for (i = 0; i < 2; i++)
        {
            bool pet = mon_is_pet(m_ptr);
            who_t who = pet ? who_create_plr() : who_create_mon(m_ptr);
            u32b mode = 0;

            if (pet) mode |= PM_FORCE_PET;

            if (summon_specific(who, m_ptr->pos, 20, SUMMON_BLUE_HORROR, mode))
            {
                if (plr_can_see(m_ptr->pos))
                    notice = TRUE;
            }
        }
        if (notice)
            msg_print("The Pink horror divides!");
    }
    else if (mon_race_is_(m_ptr->race, "p.VM"))
    {
        bool notice = FALSE;
        for (i = 0; i < 4; i++)
        {
            if (summon_specific(who_create_mon(m_ptr), m_ptr->pos, 14, SUMMON_SOFTWARE_BUG, 0))
            {
                if (plr_can_see(m_ptr->pos))
                    notice = TRUE;
            }
        }
        if (notice)
            msg_print("The Variant Maintainer is dead, but his crappy code remains!");
    }
    else if (mon_race_is_(m_ptr->race, "p.dawn"))
    {
        if (!one_in_(7))
        {
            point_t p;
            int attempts = 100;
            bool pet = mon_is_pet(m_ptr);
            who_t who = pet ? who_create_plr() : who_create_mon(m_ptr);

            do
            {
                p = scatter(m_ptr->pos, 20);
            }
            while (!dun_allow_mon_at(cave, p) && --attempts);

            if (attempts > 0)
            {
                u32b mode = 0L;
                if (pet) mode |= PM_FORCE_PET;

                if (summon_specific(who, p, 50, SUMMON_DAWN, mode))
                {
                    if (plr_can_see(p)) msg_print("A new warrior steps forth!");
                }
            }
        }
    }
    else if (mon_race_is_(m_ptr->race, "E.unmaker"))
    {
        mon_burst(m_ptr, 6, GF_CHAOS, 100);
    }
    else if (mon_race_is_(m_ptr->race, "A.silver"))
    {
        /* XXX O:20%:OBJ(can of toys)
         * XXX No way to do exactly 1 in 5, though ... */
        if (drop_chosen_item && r_ptr->lore.kills.current % 5 == 0)
        {
            /* Get local object */
            q_ptr = &forge;

            /* Prepare to make a Can of Toys */
            object_prep(q_ptr, lookup_kind(TV_CHEST, SV_CHEST_KANDUME));

            apply_magic(q_ptr, cave->difficulty, AM_NO_FIXED_ART);

            /* Drop it in the dungeon */
            drop_near(q_ptr, m_ptr->pos, -1);
        }
    }
    else if (mon_race_is_(m_ptr->race, "p.Rolento"))
    {
        mon_burst(m_ptr, 3, GF_FIRE, damroll(20, 10));
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

        if (race_ptr->boss_r_idx && race_ptr->boss_r_idx == m_ptr->race->id && plr->pclass == CLASS_MONSTER)
        {
            msg_print("Congratulations! You have killed the boss of your race!");
            plr->fame += 10;
            plr->update |= PU_BONUS; /* Player is now a "Hero" */
            plr->redraw |= PR_STATUS;

            /* Centipedes can only take the final evolutionary step if the boss is dead */
            if (plr->prace == RACE_MON_CENTIPEDE && plr->lev >= 35)
                race_ptr->hooks.gain_level(plr->lev);

            msg_add_tiny_screenshot(50, 24);
        }
    }

    if (drop_item || mon_race_is_char(r_ptr, '$'))
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
        mon_race_ptr race = NULL;
        int r_lvl = r_ptr->alloc.lvl;
        int mode = PM_FORCE_PET;

        if (!one_in_(3))
        {
            mode = PM_FORCE_FRIENDLY;
            cmsg_format(TERM_RED, "%^s is transformed in undeath!", m_name);
        }
        else
            cmsg_format(TERM_RED, "%^s rises to serve you!", m_name);

        if (mon_race_is_unique(r_ptr))
            r_lvl += 10;

        if (r_lvl >= 80)
            race = mon_race_parse("V.elder");
        else if (r_lvl >= 60 && one_in_(3))
            race = mon_race_parse("V.elder");
        else if (r_lvl >= 40 && one_in_(2))
            race = mon_race_parse("V.lord");
        else if (r_lvl >= 30)
            race = mon_race_parse("V.master");
        else
            race = mon_race_parse("V.vampire");

        summon_named_creature(who_create_null(), m_ptr->pos, race, mode);
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
static int _mon_apply_invuln(mon_ptr mon, int dam, bool is_psy_spear)
{
    int invuln = mon_tim_amount(mon, T_INVULN);
    if (invuln)
    {
        if (is_psy_spear || one_in_(PENETRATE_INVULNERABILITY))
        {
            if (mon_show_msg(mon))
                msg_print("The barrier is penetrated!");
        }
        else if (dam <= invuln)
        {
            mon_tim_subtract(mon, T_INVULN, dam);
            return 0;
        }
        else
        {
            dam -= invuln;
            mon_tim_remove(mon, T_INVULN);
        }
    }
    return dam;
}
 /* I had to split this into 2 routines:
    [1] Player is damaging monster */
int mon_damage_mod(mon_ptr mon, int dam, bool is_psy_spear)
{
    dam = _mon_apply_invuln(mon, dam, is_psy_spear);
    if (dam <= 0) return 0;

    /* Hack: Pact monsters have special resistance to all damage from the player
       I'm not sure if this is the correct spot for this code ...*/
    if ( plr->pclass == CLASS_WARLOCK
      && warlock_is_pact_monster(mon->race)
      && dam )
    {
        /* Let the player notice this for this monster race only the first time */
        if (!(mon->race->lore.flags & RFL_PACT))
        {
            msg_print("<color:v>You are less effective against monsters you have made a pact with.</color>");
            mon->race->lore.flags |= RFL_PACT;
        }
        dam = dam/2;
    }

    return dam;
}

/* [2] Another monster is damaging monster */
int mon_damage_mod_mon(mon_ptr mon, int dam, bool is_psy_spear)
{
    dam = _mon_apply_invuln(mon, dam, is_psy_spear);
    if (dam <= 0) return 0;
    return dam;
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
    monster_race *r_ptr = m_ptr->race;

    s32b new_exp;
    u32b new_exp_frac;
    s32b div_h;
    u32b div_l;

    if (mon_is_pet(m_ptr)) return;

    /*
     * - Ratio of monster's level to player's level effects
     * - Varying speed effects (Skipped for Breeders)
     * - Get a fraction in proportion of damage point
     */
    if (mon_can_multiply(m_ptr))
    {
        dam = dam * dice_avg_roll(r_ptr->hp) / m_ptr->maxhp;
        new_exp = r_ptr->alloc.lvl * speed_to_energy(r_ptr->move.speed) * dam;
    }
    else
        new_exp = r_ptr->alloc.lvl * speed_to_energy(m_ptr->mspeed) * dam;

    new_exp_frac = 0;
    div_h = 0L;
    div_l = (plr->max_plv+2) * speed_to_energy(r_ptr->move.speed);

    /* Use (average maxhp * 2) as a denominator */
    s64b_mul(&div_h, &div_l, 0, dice_avg_roll(r_ptr->hp)*2);

    /* Do division first to prevent overflaw */
    s64b_div(&new_exp, &new_exp_frac, div_h, div_l);

    /* Finally multiply base experience point of the monster */
    s64b_mul(&new_exp, &new_exp_frac, 0, r_ptr->mexp);

    if (mut_present(MUT_FAST_LEARNER))
    {
        s64b_mul(&new_exp, &new_exp_frac, 0, 6);
        s64b_div(&new_exp, &new_exp_frac, 0, 5);
    }

    /* Intelligence affects learning! */
    s64b_mul(&new_exp, &new_exp_frac, 0, adj_exp_gain[plr->stat_ind[A_INT]]);
    s64b_div(&new_exp, &new_exp_frac, 0, 100);

    /* Gain experience */
    gain_exp_64(new_exp, new_exp_frac);
}

/* Hack for Quylthulgs. Their pets are allowed to kill uniques! */
void mon_check_kill_unique(mon_ptr mon)
{
    if (!have_flag(mon->smart, SM_CLONED))
    {
        /* When the player kills a Unique, it stays dead */
        if (mon_race_is_fixed_unique(mon->race))
        {
            assert(mon->race->alloc.max_max_num == 1);
            mon->race->alloc.max_num = 0;

            if (one_in_(3) || mon->race->alloc.lvl >= 80)
            {
                plr->fame++;
                if (mon->race->alloc.lvl >= 90)
                    plr->fame++;
            }

            /* Mega-Hack -- Banor=Rupart is a 'compound' monster, often
             * splitting in two before re-combining */
            /* kill a part, and the whole can no longer be created ... */
            if ( mon_race_is_(mon->race, "p.Banor")
              || mon_race_is_(mon->race, "p.Rupart") )
            {
                mon_race_ptr r = mon_race_parse("p.Banor=Rupart");
                r->alloc.max_num = 0;
                r->lore.kills.current++;
                if (r->lore.kills.total < MAX_SHORT) r->lore.kills.total++;
            }
            /* kill the whole and the parts can no longer be created ... */
            else if (mon_race_is_(mon->race, "p.Banor=Rupart"))
            {
                mon_race_ptr r = mon_race_parse("p.Banor");
                r->alloc.max_num = 0;
                r->lore.kills.current++;
                if (r->lore.kills.total < MAX_SHORT) r->lore.kills.total++;

                r = mon_race_parse("p.Rupart");
                r->alloc.max_num = 0;
                r->lore.kills.current++;
                if (r->lore.kills.total < MAX_SHORT) r->lore.kills.total++;
            }
        }

        /* When the player kills a Nazgul, it stays dead */
        else if (mon->race->alloc.max_max_num)
            mon->race->alloc.max_num = MAX(0, mon->race->alloc.max_num - 1);
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
    if (!plr_view(mon->pos)) return;
    mon->mflag2 |= MFLAG2_AWARE;
}
bool tranquilize_hack;
bool mon_take_hit(mon_ptr mon, int dam, bool *fear, cptr note)
{
    /* Innocent until proven otherwise */
    bool        innocent = TRUE, thief = FALSE;
    int         i, j;
    int         expdam;

    set_sanctuary(FALSE);
    /* plr->innocence blocks monster hostility, allowing the plr to pass unscathed ... */
    if (plr->innocence)
    {
        /* ... so attacking is a very evil action! */
        virtue_add(VIRTUE_HONOUR, -5);
        virtue_add(VIRTUE_COMPASSION, -5);
        virtue_add(VIRTUE_JUSTICE, -5);
        plr_tim_remove(T_CLOAK_INNOCENCE);
        plr_tim_remove(T_BLESS_PEACE);
        plr->innocence = FALSE; /* XXX only trash virtues once ... PU_BONUS is pending end of melee attacks */
    }

    /* Hack: Player mimic has revealed itself! */
    if (plr->prace == RACE_MON_RING && !plr->riding)
    {
        mon->mflag2 |= MFLAG2_AWARE;
        dun_iter_mon(cave, _ring_revealed);
    }

    if (!(mon->race->attributes & RF_KILL_EXP)) /* XXX only "E.caaws" has this ... */
    {
        expdam = (mon->hp > dam) ? dam : mon->hp;
        if (mon_race_has_healing(mon->race)) expdam = (expdam+1) * 2 / 3;

        get_exp_from_mon(expdam, mon);

        /* Genocided by chaos patron */
        if (!mon_is_valid(mon)) return TRUE;
    }

    /* Redraw (later) if needed */
    check_mon_health_redraw(mon);

    /* Wake it up */
    if (!tranquilize_hack) /* no clean way to do this ... */
        mon_tim_delete(mon, MT_SLEEP);

    /* Hack - Cancel any special player stealth magics. -LM- */
    if (plr->special_defense & NINJA_S_STEALTH)
        set_superstealth(FALSE);
    if (plr->special_defense & DEFENSE_INVISIBLE)
        set_invisible(FALSE);

    if (dam > 0 && (plr->wizard || 0))
        msg_format("You do %d damage.", dam);

    if ( plr->melt_armor
      && note == NULL /* Hack: Trying to just get melee and shooting */
      && (-mon->ac_adj) < mon->race->ac/2
      && !mon_save_p(mon, A_NONE) )
    {
        char m_name[MAX_NLEN];
        monster_desc(m_name, mon, MD_PRON_VISIBLE | MD_POSSESSIVE);
        msg_format("%^s armor melts.", m_name);
        mon->ac_adj -= randint1(2);
        if (plr->wizard)
            msg_format("Melt Armor: AC is now %d", mon_ac(mon));
    }

    /* Rage Mage: "Blood Lust" */
    if (plr->pclass == CLASS_RAGE_MAGE && dam > 0)
    {
        rage_mage_blood_lust(dam);
    }

    /* Hurt it */
    mon->hp -= dam;
    if (mon->hp >= 0)
        mon_packs_on_damage(mon);

    /* It is dead now */
    if (mon->hp < 0)
    {
        char         m_name[MAX_NLEN];
        monster_type exp_mon = *mon; /* Copy since we will delete_monster before granting experience */

        monster_desc(m_name, mon, MD_TRUE_NAME);

        if (mon_race_is_(mon->race, "q.tanuki"))
        {
            mon->apparent_race = mon->race;
            mon_lore_sighting(mon);
        }
        if (mon->mflag2 & MFLAG2_CHAMELEON)
        {
            mon->race = mon_true_race(mon);
            mon_lore_sighting(mon);
        }

        if (!have_flag(mon->smart, SM_CLONED))
        {
            /* When the player kills a Unique, it stays dead */
            if (mon_race_is_fixed_unique(mon->race))
            {
                if (mon_race_is_(mon->race, "B.Phoenix") && one_in_(3))
                {
                    mon->hp = mon->maxhp;
                    mon_tim_delete(mon, T_FEAR);
                    msg_print("The Phoenix rises again!");
                    return FALSE;
                }

                assert(mon->race->alloc.max_max_num == 1);
                mon->race->alloc.max_num = 0;

                if (one_in_(3) || mon->race->alloc.lvl >= 80)
                {
                    plr->fame++;
                    if (mon->race->alloc.lvl >= 90)
                        plr->fame++;
                }

                /* Mega-Hack -- Banor=Rupart is a 'compound' monster, often
                 * splitting in two before re-combining */
                /* kill a part, and the whole can no longer be created ... */
                if ( mon_race_is_(mon->race, "p.Banor")
                  || mon_race_is_(mon->race, "p.Rupart") )
                {
                    mon_race_ptr r = mon_race_parse("p.Banor=Rupart");
                    r->alloc.max_num = 0;
                    r->lore.kills.current++;
                    if (r->lore.kills.total < MAX_SHORT) r->lore.kills.total++;
                }
                /* kill the whole and the parts can no longer be created ... */
                else if (mon_race_is_(mon->race, "p.Banor=Rupart"))
                {
                    mon_race_ptr r = mon_race_parse("p.Banor");
                    r->alloc.max_num = 0;
                    r->lore.kills.current++;
                    if (r->lore.kills.total < MAX_SHORT) r->lore.kills.total++;

                    r = mon_race_parse("p.Rupart");
                    r->alloc.max_num = 0;
                    r->lore.kills.current++;
                    if (r->lore.kills.total < MAX_SHORT) r->lore.kills.total++;
                }
            }

            /* When the player kills a Nazgul, it stays dead */
            else if (mon->race->alloc.max_max_num)
                mon->race->alloc.max_num = MAX(0, mon->race->alloc.max_num - 1);
        }

        mon_packs_on_death(mon);
        mon_lore_death(mon);

        if ( (mon->ml && !plr_tim_find(T_HALLUCINATE))
          || mon_race_is_unique(mon->race)
          || statistics_hack )
        {
            mon_track(mon);
        }

        /* Don't kill Amberites */
        if (mon_race_is_amberite(mon->race) && one_in_(2))
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

        if (mon_can_speak(mon))
        {
            char line_got[1024];

            /* Dump a message */
            if (!get_rnd_line("mondeath.txt", sym_str(mon->race->id), line_got))

                msg_format("%^s %s", m_name, line_got);
        }

        if (mon->race->alloc.lvl > cave->dun_lvl)
        {
            if (randint1(10) <= (mon->race->alloc.lvl - cave->dun_lvl))
                virtue_add(VIRTUE_VALOUR, 1);
        }
        if (mon->race->alloc.lvl > 60)
        {
            virtue_add(VIRTUE_VALOUR, 1);
        }
        if (mon->race->alloc.lvl >= 2 * (plr->lev+1))
            virtue_add(VIRTUE_VALOUR, 2);

        if (mon_race_is_unique(mon->race))
        {
            if (mon->race->align) virtue_add(VIRTUE_HARMONY, 2);

            if (mon_race_is_good(mon->race))
            {
                virtue_add(VIRTUE_UNLIFE, 2);
                virtue_add(VIRTUE_VITALITY, -2);
            }

            if (one_in_(3)) virtue_add(VIRTUE_INDIVIDUALISM, -1);
        }

        if (mon_race_is_(mon->race, "t.beggar") || mon_race_is_(mon->race, "t.leper"))
        {
            virtue_add(VIRTUE_COMPASSION, -1);
        }

        if (mon_race_is_good(mon->race) &&
            ((mon->race->alloc.lvl) / 10 + (3 * cave->dun_lvl) >= randint1(100)))
            virtue_add(VIRTUE_UNLIFE, 1);

        if (mon_race_is_char(mon->race, 'A'))
        {
            if (mon_race_is_unique(mon->race))
                virtue_add(VIRTUE_FAITH, -2);
            else if ((mon->race->alloc.lvl) / 10 + (3 * cave->dun_lvl) >= randint1(100))
            {
                if (mon_race_is_good(mon->race)) virtue_add(VIRTUE_FAITH, -1);
                else virtue_add(VIRTUE_FAITH, 1);
            }
        }
        else if (mon_race_is_demon(mon->race))
        {
            if (mon_race_is_unique(mon->race))
                virtue_add(VIRTUE_FAITH, 2);
            else if ((mon->race->alloc.lvl) / 10 + (3 * cave->dun_lvl) >= randint1(100))
                virtue_add(VIRTUE_FAITH, 1);
        }

        if (mon_race_is_undead(mon->race) && mon_race_is_unique(mon->race))
            virtue_add(VIRTUE_VITALITY, 2);

        if (mon->race->lore.deaths)
        {
            if (mon_race_is_unique(mon->race))
            {
                virtue_add(VIRTUE_HONOUR, 10);
            }
            else if ((mon->race->alloc.lvl) / 10 + (2 * cave->dun_lvl) >= randint1(100))
            {
                virtue_add(VIRTUE_HONOUR, 1);
            }
        }
        if (mon_race_can_multiply(mon->race) && mon->race->lore.kills.current > 1000 && one_in_(10))
        {
            virtue_add(VIRTUE_VALOUR, -1);
        }

        for (i = 0; i < vec_length(mon->race->blows); i++)
        {
            mon_blow_ptr blow = vec_get(mon->race->blows, i);
            /* XXX RF2_THIEF could be used, but not every RBE_EAT_FOO monster is so flagged */
            for (j = 0; j < blow->effect_ct; j++)
            {
                int effect = blow->effects[j].type;
                if (j == 0 && blow->effects[j].dice.dd != 0) innocent = FALSE; /* Murderer! XXX This makes no sense ... */
                if (effect == RBE_EAT_GOLD || effect == RBE_EAT_ITEM)
                    thief = TRUE;
            }
        }

        /* The new law says it is illegal to live in the dungeon */
        if (mon->race->alloc.lvl != 0) innocent = FALSE;

        if (thief)
        {
            if (mon_race_is_unique(mon->race))
                virtue_add(VIRTUE_JUSTICE, 3);
            else if (1 + mon->race->alloc.lvl/10 + 2*cave->dun_lvl >= randint1(100))
                virtue_add(VIRTUE_JUSTICE, 1);
        }
        else if (innocent)
        {
            virtue_add (VIRTUE_JUSTICE, -1);
        }

        if (mon_race_is_animal(mon->race) && !mon_race_is_evil(mon->race) && !mon_race_is_magical(mon->race))
        {
            if (one_in_(4)) virtue_add(VIRTUE_NATURE, -1);
        }

        /* Make a sound */
        sound(SOUND_KILL);

        if (mon->mflag2 & MFLAG2_ILLUSION)
            msg_format("%^s is dispelled.", m_name);

        /* Death by Missile/Spell attack */
        else if (note)
        {
            msg_format("%^s%s", m_name, note);
        }

        /* Death by physical attack -- invisible monster */
        else if (!mon->ml)
        {
            cmsg_format(TERM_L_RED, "You have killed %s.", m_name);
        }

        /* Death by Physical attack -- non-living monster */
        else if (!mon_race_is_living(mon->race))
        {
            if (mon_blows_find(mon->race->blows, RBM_EXPLODE))
                cmsg_format(TERM_L_RED, "%^s explodes into tiny shreds.", m_name);
            else
                cmsg_format(TERM_L_RED, "You have destroyed %s.", m_name);
        }

        /* Death by Physical attack -- living monster */
        else
            cmsg_format(TERM_L_RED, "You have slain %s.", m_name);

        if (_mon_is_wanted(mon))
        {
            msg_format("There is a price on %s's head.", m_name);
        }

        /* Generate treasure */
        monster_death(mon, TRUE);

        /* Mega hack : replace IKETA to BIKETAL */
        if (mon_race_is_(mon->race, "p.Iketa"))
        {
            point_t pos = mon->pos;
            u32b mode = 0L;

            if (mon_is_pet(mon)) mode |= PM_FORCE_PET;

            delete_monster(mon);

            if (summon_named_creature(who_create_null(), pos, mon_race_parse("U.Biketal"), mode))
                msg_print("Uwa-hahaha!  *I* am Biketal!");
        }
        else
        {
            delete_monster(mon);
        }

        /* Prevent bug of chaos patron's reward */
        if (mon->race->attributes & RF_KILL_EXP)
            get_exp_from_mon((long)exp_mon.max_maxhp*2, &exp_mon);
        else
            get_exp_from_mon(((long)exp_mon.max_maxhp+1L) * 9L / 10L, &exp_mon);

        /* Not afraid */
        (*fear) = FALSE;

        /* Monster is dead */
        return (TRUE);
    }

    /* Mega-Hack -- Pain cancels fear ... later (fear_process_m) */
    if (mon_tim_find(mon, T_FEAR)) mon->pain += dam;
    if (fear_p_hurt_m(mon, dam))
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
    plr->update |= (PU_TORCH | PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);

    /* Forget light/view */
    plr->update |= (PU_UN_VIEW | PU_UN_LIGHT);

    /* Update light/view */
    plr->update |= (PU_VIEW | PU_LIGHT | PU_MON_LIGHT);

    /* Update monsters */
    plr->update |= (PU_MONSTERS);

    /* Redraw everything */
    plr->redraw |= (PR_WIPE | PR_BASIC | PR_EXTRA | PR_MAP | PR_EQUIPPY);

    /* Hack -- update */
    handle_stuff();

    /* Redraw */
    Term_redraw();

    /*
     * Waiting command;
     * Place the cursor on the player
     */
    if (can_save) move_cursor_relative(plr->pos);

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
    plr->window |= (PW_INVEN | PW_EQUIP | PW_SPELL);

    /* Window stuff */
    plr->window |= (PW_MESSAGE | PW_OVERHEAD | PW_DUNGEON | PW_MONSTER | PW_OBJECT);

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
    point_t p = point_create(x, y);
    return dun_pos_valid(cave, p)
        && cave_pt_is_visible(p);
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

rect_t ui_prompt_rect(void) /* XXX Prompt no longer owns the entire top line! */
{
    return rect_create(
        0,
        0,
        Term->wid - 12 - 1,
        1
    );
}

rect_t ui_shop_msg_rect(void)
{
    return rect_create(0, 0, 80, 3);
}

rect_t ui_msg_rect(void)
{
    int cx = ui_map_rect().cx; /* msg_line never overwrites ui_char_info_rect ... */
    return rect_create(0, 0, MIN(72, cx), 10);
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

        plr->update |= (PU_MONSTERS); /* XXX Why? */
        plr->redraw |= (PR_MAP);
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
        plr->update |= PU_MONSTERS;
        plr->redraw |= PR_MAP;
        plr->window |= PW_OVERHEAD | PW_DUNGEON;
    }
}

void viewport_verify(void)
{
    int options = 0;
    if (center_player && (center_running || (!running && !travel.run)))
        options |= VIEWPORT_FORCE_CENTER;
    viewport_verify_aux(plr->pos, options);
}


cptr mon_health_desc(monster_type *m_ptr)
{
    monster_race *ap_r_ptr = m_ptr->apparent_race;
    bool          living = mon_race_is_living(ap_r_ptr); /* apparent! */
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
    if (mon_is_pet(m_ptr))
        return "pet";
    else if (mon_is_friendly(m_ptr))
        return "friendly";
    return ""; /* Hostile is to be assumed! */
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
static bool _target_project(point_t src, point_t target, int mode)
{
    int rng = project_length > 0 ? project_length : DUN_PATH_MAX;
    u32b flgs = 0;
    if (mode & TARGET_LOS) flgs |= PROJECT_LOS;
    if (mode & TARGET_DISI) flgs |= PROJECT_DISI;
    return dun_project_aux(cave, src, target, flgs, rng);
}

bool target_able(mon_ptr mon) { return target_able_aux(mon, TARGET_KILL); }
bool target_able_aux(mon_ptr mon, int mode)
{
    if (!mon_is_valid(mon)) return FALSE; /* cf _mon_free */

    if (plr_tim_find(T_HALLUCINATE)) return FALSE;
    if (!mon->ml) return FALSE;
    if (mon->id == plr->riding) return TRUE;

    if (!(mode & TARGET_DISI) && !_target_project(plr->pos, mon->pos, mode))
        return FALSE;

    return TRUE;
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
    if (who_is_pos(plr->target)) return TRUE;

    if (who_is_mon(plr->target))
        return target_able_aux(who_mon(plr->target), mode);

    /* Assume no target */
    return FALSE;
}

/*
 * Hack -- help "select" a location (see below)
 */
static s16b target_pick(point_t pos, point_t v)
{
    int i, dis;
    int b_i = -1, b_dis = 9999;

    for (i = 0; i < point_vec_length(temp_pts); i++)
    {
        point_t pos2 = point_vec_get(temp_pts, i);
        point_t v2 = point_subtract(pos2, pos);
        point_t a2;

        /* Verify quadrant */
        if (v.x && v2.x * v.x <= 0) continue;
        if (v.y && v2.y * v.y <= 0) continue;

        /* Absolute distance */
        a2 = point_abs(v2);

        /* Verify quadrant */
        if (v.y && !v.x && a2.x > a2.y) continue;
        if (v.x && !v.y && a2.y > a2.x) continue;

        /* cf point_fast_distance */
        dis = (a2.y > a2.x) ? (a2.y + (a2.x>>1)) : (a2.x + (a2.y>>1));

        /* Track best */
        if (b_i >= 0 && dis >= b_dis) continue;

        b_i = i;
        b_dis = dis;
    }

    return b_i;
}


/*
 * Hack -- determine if a given location is "interesting"
 */
static bool target_set_accept(point_t pos)
{
    dun_grid_ex_t grid;
    obj_ptr obj;

    if (!dun_pos_interior(cave, pos)) return FALSE;
    if (dun_plr_at(cave, pos)) return TRUE;
    if (plr_tim_find(T_HALLUCINATE)) return FALSE;

    grid = dun_grid_ex_at(cave, pos);
    if (grid.mon && grid.mon->ml) return TRUE;

    for (obj = grid.obj; obj; obj = obj->next)
        if (obj->marked & OM_FOUND) return TRUE;

    if (grid.grid->flags & CELL_MAP)
    {
        if (cell_notice(grid.grid)) return TRUE;
    }

    return FALSE;
}


static int _cmp_distance(point_t p1, point_t p2)
{
    int d1 = point_fast_distance(plr->pos, p1);
    int d2 = point_fast_distance(plr->pos, p2);
    if (d1 < d2) return -1;
    if (d1 > d2) return 1;
    return 0;
}
static int _cmp_mon(point_t p1, point_t p2)
{
    mon_ptr m1 = dun_mon_at(cave, p1);
    mon_ptr m2 = dun_mon_at(cave, p2);
    bool    l1 = m1 && m1->ml;
    bool    l2 = m2 && m2->ml;
    mon_race_ptr r1, r2;

    if (l1 && !l2) return -1;
    if (!l1 && l2) return 1;
    if (!l1 && !l2) return 0;
    assert(m1 && m2);
    assert(m1->ml && m2->ml);

    /* uniques first */
    if (mon_is_unique(m1) && !mon_is_unique(m2)) return -1;
    if (!mon_is_unique(m1) && mon_is_unique(m2)) return 1;

    /* shadowers first */
    if ((m1->mflag2 & MFLAG2_KAGE) && !(m2->mflag2 & MFLAG2_KAGE)) return -1;
    if (!(m1->mflag2 & MFLAG2_KAGE) && (m2->mflag2 & MFLAG2_KAGE)) return 1;

    r1 = m1->apparent_race;
    r2 = m2->apparent_race;

    /* unknown monsters first */
    if (!r1->lore.kills.total && r2->lore.kills.total) return -1;
    if (r1->lore.kills.total && !r2->lore.kills.total) return 1;

    /* higher level first (if known) */
    if (r1->lore.kills.total && r2->lore.kills.total)
    {
        if (r1->alloc.lvl > r2->alloc.lvl) return -1;
        if (r1->alloc.lvl < r2->alloc.lvl) return 1;
    }

    /* sort by name */
    return strcmp(r1->name, r2->name);
}
static int _cmp_obj(point_t p1, point_t p2)
{
    obj_ptr o1 = dun_obj_at(cave, p1);
    obj_ptr o2 = dun_obj_at(cave, p2);

    if (o1 && !o2) return -1;
    if (!o1 && o2) return 1;
    /* XXX could consult auto-picker; could use known value */
    return 0;
}
static int _cmp_feat(point_t p1, point_t p2)
{
    dun_cell_ptr c1 = dun_cell_at(cave, p1);
    dun_cell_ptr c2 = dun_cell_at(cave, p2);
    int v1 = cell_priority(c1);
    int v2 = cell_priority(c2);

    if (v1 > v2) return -1;
    if (v1 < v2) return 1;
    return 0;
}
static int _cmp_importance(point_t p1, point_t p2)
{
    int cmp;

    if (point_equals(p1, p2)) return 0; /* paranoia */

    if (point_equals(plr->pos, p1)) return -1;
    if (point_equals(plr->pos, p2)) return 1;

    cmp = _cmp_mon(p1, p2);
    if (cmp) return cmp;

    cmp = _cmp_obj(p1, p2);
    if (cmp) return cmp;

    cmp = _cmp_feat(p1, p2);
    if (cmp) return cmp;

    return _cmp_distance(p1, p2);
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
    point_vec_clear(temp_pts);

    /* Scan the current panel */
    for (uip = rect_first(map_rect);
            rect_contains_point(map_rect, uip);
            uip = rect_next(map_rect, uip) )
    {
        point_t cp = ui_pt_to_cave_pt(uip);
        mon_ptr mon;

        if (!target_set_accept(cp)) continue;

        mon = dun_mon_at(cave, cp);

        if (mode & TARGET_KILL)
            if (!mon || !target_able_aux(mon, mode)) continue;

        if ((mode & (TARGET_KILL | TARGET_MARK)) && !target_pet)
            if (mon && mon_is_pet(mon)) continue;

        if (mode & (TARGET_MARK | TARGET_DISI))
            if (!mon || !mon->ml) continue; /* XXX duelist required visible monsters */

        point_vec_add(temp_pts, cp);
    }

    if (mode & (TARGET_KILL | TARGET_MARK | TARGET_DISI))
        point_vec_sort(temp_pts, _cmp_distance);
    else
        point_vec_sort(temp_pts, _cmp_importance);

    /* XXX */
    #if 0
    if (plr->riding && target_pet && (temp_n > 1) && (mode & (TARGET_KILL)))
    {
        s16b tmp;

        tmp = temp_y[0];
        temp_y[0] = temp_y[1];
        temp_y[1] = tmp;
        tmp = temp_x[0];
        temp_x[0] = temp_x[1];
        temp_x[1] = tmp;
    }
    #endif
}

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
 *
 */
static int target_set_aux(point_t pos, int mode, cptr info)
{
    dun_cell_ptr cell = dun_cell_at(cave, pos);
    dun_cell_t nothing = {FEAT_FLOOR, 0}; /* clean floor for un-seen|known grids */
    obj_ptr obj;
    cptr s1 = "", s2 = "", s3 = "", x_info = "";
    bool boring = TRUE;
    int query = '\001';
    doc_ptr prompt = doc_alloc(ui_prompt_rect().cx); /* XXX Term should understand color directives */
    inv_ptr inv = NULL; /* XXX Note all the effort required not to leak memory ... REWRITE!!! */
    int obj_ct = 0;
    mon_ptr mon;

    doc_insert(prompt, "<style:table>");

    /* Hack -- under the player */
    if (dun_plr_at(cave, pos))
    {
        /* Description */
        s1 = "You are ";

        /* Preposition */
        s2 = "on ";
    }
    else if (mode == TARGET_LOOK) /* do_cmd_look */
    {
        s1 = "You see ";
        s2 = "";
    }
    else
    {
        s1 = "Target: ";
        /* Hack: I get confused about whether I am selecting a monster,
           or the position that the monster happens to currently occupy.
           This happens after a fat finger incident or a failed <dir>
           navigation that really should have worked.*/
        if (strstr(info, "o,"))
            s1 = "Monster: ";
        else if (strstr(info, "m,"))
            s1 = "Position: ";
    }

    /* Hack -- hallucination */
    if (plr_tim_find(T_HALLUCINATE))
    {
        cptr name = "something strange";

        /* Display a message */
        doc_printf(prompt, "%s%s%s%s [%s]", s1, s2, s3, name, info);
        doc_sync_prompt(prompt);

        move_cursor_relative(pos);
        query = inkey();

        /* Stop on everything but "return" */
        if ((query != '\r') && (query != '\n')) { doc_free(prompt); return query; }

        /* Repeat forever */
        doc_free(prompt);
        return 0;
    }

    inv = inv_filter_floor(pos, obj_is_found);
    obj_ct = inv_count_slots(inv, obj_exists);
    if (obj_ct)
        x_info = "x,";

/* BLOCK: Monster */
    /* Actual monsters */
    mon = dun_mon_at(cave, pos);
    if (mon && mon->ml)
    {
        bool          fuzzy = BOOL(mon->mflag2 & MFLAG2_FUZZY);
        monster_race *ap_r_ptr = mon->apparent_race;
        char m_name[80];
        bool recall = FALSE;

        boring = FALSE;

        if (fuzzy)
            strcpy(m_name, "Monster");
        else
        {
            monster_desc(m_name, mon, MD_INDEF_VISIBLE);
            mon_track(mon);
            health_track(mon);
            handle_stuff();
        }

        /* Interact */
        while (1)
        {
            /* Recall */
            #ifdef DEVELOPER
            if (recall)
            #else
            if (recall && !fuzzy)
            #endif
            {
                doc_ptr doc = doc_alloc(72);

                /* Save */
                screen_save();

                /* Recall on screen */
                #ifdef DEVELOPER
                mon_wizard_doc(mon, doc);
                #else
                mon_display_doc(ap_r_ptr, doc);
                #endif
                doc_sync_term(doc, doc_range_all(doc), doc_pos_create(0, 1));
                doc_free(doc);

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
            doc_clear(prompt);
            doc_printf(prompt, "%s%s%s%s ", s1, s2, s3, m_name);
            if (mon_is_pet(mon))
                doc_insert(prompt, "(<color:o>Pet</color>) ");
            else if (!fuzzy)
            {
                if (mon_is_friendly(mon))
                    doc_insert(prompt, "(<color:G>Friendly</color>) ");
                else if (have_flag(mon->smart, SM_CLONED))
                    doc_insert(prompt, "(<color:B>Clone</color>) ");
            }
            if (display_distance)
                doc_printf(prompt, "(Rng %d) ", mon->cdis);
            if (plr->wizard)
                doc_printf(prompt, "(L=%d) ", plr_light(pos));
            doc_printf(prompt, "[r,%s%s]", x_info, info);

            doc_sync_prompt(prompt);

            /* Place cursor */
            move_cursor_relative(pos);

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
            doc_free(prompt);
            return query;
        }

        /* Sometimes stop at "space" key */
        if ((query == ' ') && !(mode & (TARGET_LOOK)))
        {
            inv_free(inv);
            doc_free(prompt);
            return query;
        }
/* BLOCK: Monster Carried Objects */
        /* Change the intro */
        s1 = "It is ";

        /* Hack -- take account of gender */
        if (mon_race_is_female(ap_r_ptr)) s1 = "She is ";

        else if (mon_race_is_male(ap_r_ptr)) s1 = "He is ";


        /* Use a preposition */
        s2 = "carrying ";

        /* Scan all objects being carried */
        for (obj = mon->obj; obj; obj = obj->next)
        {
            char o_name[MAX_NLEN];

            /* Obtain an object description */
            object_desc(o_name, obj, 0);

            /* Describe the object */
            doc_clear(prompt);
            doc_printf(prompt, "%s%s%s%s [%s]", s1, s2, s3, o_name, info);
            doc_sync_prompt(prompt);

            move_cursor_relative(pos);
            query = inkey();

            /* Always stop at "normal" keys */
            if ((query != '\r') && (query != '\n') && (query != ' ') && (query != 'x'))
            {
                inv_free(inv);
                doc_free(prompt);
                return query;
            }

            /* Sometimes stop at "space" key */
            if ((query == ' ') && !(mode & (TARGET_LOOK)))
            {
                inv_free(inv);
                doc_free(prompt);
                return query;
            }

            /* Change the intro */
            s2 = "also carrying ";
        }

        /* Use a preposition */
        s2 = "on ";
    }
/* BLOCK: Floor Objects */
    /* Show objects on this grid. If multiple, show a list. If the
     * list won't fit on the screen, <CR> scrolls the list */
    if (obj_ct == 1)
    {
        obj_ptr obj = inv_obj(inv, 1);
        char name[MAX_NLEN];

        object_desc(name, obj, 0);

        doc_clear(prompt);
        doc_printf(prompt, "%s%s%s%s [%s]", s1, s2, s3, name, info);
        doc_sync_prompt(prompt);

        move_cursor_relative(pos);

        query = inkey();
        inv_free(inv);
        doc_free(prompt);
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
            doc_clear(prompt);
            doc_printf(prompt, "%s%s%sa pile of %d items [x,%s]",
                s1, s2, s3, obj_ct, info);
            doc_sync_prompt(prompt);

            move_cursor_relative(pos);

            query = inkey();

            if (query != 'x' && query != ' ')
            {
                inv_free(inv);
                doc_free(prompt);
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

            doc_clear(prompt);
            doc_printf(prompt, "%s%s%sa pile of %d items [Enter,%s]",
                s1, s2, s3, obj_ct, info);
            doc_sync_prompt(prompt);
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

/* BLOCK: Terrain */

    /* Hack: 'Forget' the true cell if unseen and unknown */
    if (!(cell->flags & CELL_MAP) && !plr_can_see(pos))
        cell = &nothing; /* XXX hack to prevent code restructure */

    /* Terrain feature if needed */
    if (boring || !cell_is_boring(cell))
    {
        cptr name;

        /* Hack -- special handling for quest entrances */
        if (stairs_enter_quest(cell))
        {
            quest_ptr q = quests_get(stairs_quest_id(cell));
            name = format("entrance to the quest '%s'(level %d)", q->name, q->level);
        }
        else if (stairs_enter_dungeon(cell))
        {
            dun_type_ptr type = dun_types_lookup(stairs_dun_type_id(cell));
            if (type->flags.info & DF_RANDOM)
                name = format("entrance to <color:R>%s</color> (level ?)", type->name);
            else
                name = format("entrance to <color:R>%s</color> (level %d)", type->name, type->min_dun_lvl);
        }
        else
        {
            if (cell == &nothing) /* XXX */
                name = "an unknown grid"; /* XXX */
            else
                name = cell_desc(cell);
        }


        /* Pick a prefix */
        if (*s2 /* you see a ghost. it is in a wall. */
          && ( (cell_is_wall(cell) && !wall_is_mountain(cell))
            || (!cell_los(cell) && !cell_is_tree(cell)) ) )
        {
            s2 = "in ";
            if (floor_has_web(cell))
                s3 = "a ";
        }
        else if (!floor_is_clean(cell))
        {
            s3 = (is_a_vowel(name[0])) ? "an " : "a ";
        }
        else if ( cell_is_bldg(cell) /* you see the General Store */
               || stairs_enter_quest(cell)
               || stairs_enter_dungeon(cell) /* you see the entrance to ... */
               || floor_is_road(cell) ) /* you see the road */
        {
            s3 = "the ";
        }
        else if ( floor_is_floor(cell) /* 'you see floor|dirt|grass' but not 'you see flower|brake' */
               || floor_is_dirt(cell) 
               || floor_is_grass(cell)
               || cell_is_lava(cell)
               || cell_is_water(cell) ) /* you see deep water */
        {
            s3 = "";
        }
        else /* you see an open door ... you see a closed door */
        {
            /* Pick proper indefinite article */
            s3 = (is_a_vowel(name[0])) ? "an " : "a ";
        }

        doc_clear(prompt);
        if (plr->wizard)
        {
            line_t line = line_create(plr->pos, pos);
            if (line_is_valid(line))
            {
                dun_line_gen_t gen;
                dun_line_gen_create(&gen, line);
                doc_printf(prompt, "%s%s%s (%d,%d) (%d,%d) %d=%d*%d+%d (L=%d,F=%d)", s2, s3, name, pos.x, pos.y,
                    pos.x - plr->pos.x, pos.y - plr->pos.y,
                    gen.major_length + 1,
                    gen.segment_count,        /* N */
                    gen.base_segment_length,
                    gen.segment_remainder,    /* choose k */
                    plr_light(line.b),
                    dun_flow_at(cave->flow, line.b)
                );
                if (cell->flags & CELL_LIT)
                    doc_insert_char(prompt, TERM_YELLOW, 'L');
                dun_line_gen_destroy(&gen);
            }
            else
            {
                int flow = dun_flow_at(cave->flow, pos);
                doc_printf(prompt, "%s%s%s%s [%s] %x (%d,%d) %d", s1, s2, s3, name, info, cell->flags, pos.x, pos.y, flow);
                if (plr->wizard)
                    doc_printf(prompt, " (L=%d)", plr_light(pos));
            }
        }
        else if (display_distance)
        {
            /* Note: c_ptr->dist != m_ptr->cdis. The cave distance is not the range as diagonals count as 1, not 1.5
               Use distance calculation from update_mon, which sets m_ptr->cdis.*/
            int d = point_fast_distance(plr->pos, pos);
            doc_printf(prompt, "%s%s%s%s [%s] (Rng %d)", s1, s2, s3, name, info, d);
        }
        else
            doc_printf(prompt, "%s%s%s%s [%s]", s1, s2, s3, name, info);

        doc_sync_prompt(prompt);
        move_cursor_relative(pos);
        query = inkey();

        /* Always stop at "normal" keys */
        if ((query != '\r') && (query != '\n') && (query != ' ')) { doc_free(prompt); return query; }
    }

    /* Stop on everything but "return" */
    if ((query != '\r') && (query != '\n')) { doc_free(prompt); return query; }

    /* Repeat forever */
    doc_free(prompt);
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
    int        i, d, m, bd;
    point_t    pos = plr->pos;
    bool       done = FALSE;
    bool       flag = TRUE;
    char       query;
    char       info[80];

    mon_ptr    mon;
    rect_t     map_rect = ui_map_rect();
    point_t    min_pos = rect_top_left(cave->rect);
    point_t    max_pos = rect_bottom_right(cave->rect);


    /* Cancel target */
    plr->target = who_create_null();

    /* Cancel tracking */
    /* health_track(NULL); */


    /* Prepare the "temp" array */
    target_set_prepare(mode);

    /* Start near the player */
    m = 0;

    /* Interact */
    while (!done)
    {
        /* Interesting grids */
        if (flag && point_vec_length(temp_pts))
        {
            pos = point_vec_get(temp_pts, m);

            if (!(mode & TARGET_LOOK) && !(mode & TARGET_MARK))
                prt_path(pos.y, pos.x, (mode & TARGET_DISI) ? PROJECT_DISI : 0);

            mon = dun_mon_at(cave, pos);
            if ( (mon && target_able_aux(mon, mode))
              || ((mode & (TARGET_MARK|TARGET_DISI)) && mon && mon->ml) )
            {
                strcpy(info, "q,t,p,o,+,-,<dir>");
            }
            else /* not targetable */
            {
                strcpy(info, "q,p,o,+,-,<dir>");
            }

            /* Describe and Prompt */
            while (!(query = target_set_aux(pos, mode, info)));

            /* Assume no "direction" */
            d = 0;

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
                done = TRUE;
                break;
            case 't':
            case '.':
            case '5':
            case '0':
                if ( (mon && target_able_aux(mon, mode))
                  || ((mode & (TARGET_MARK|TARGET_DISI)) && mon && mon->ml) )
                {
                    health_track(mon);
                    plr->target = who_create_mon(mon);
                    done = TRUE;
                }
                else bell();
                break;
            case ' ':
            case '*':
            case '+':
                if (++m == point_vec_length(temp_pts))
                {
                    m = 0;
                    if (!expand_list) done = TRUE;
                }
                break;
            case '-':
                if (m-- == 0)
                {
                    m = point_vec_length(temp_pts) - 1;
                    if (!expand_list) done = TRUE;
                }
                break;
            case 'p':
                viewport_verify();
                plr->update |= (PU_MONSTERS);
                plr->redraw |= (PR_MAP);
                plr->window |= (PW_OVERHEAD);
                handle_stuff();

                /* Recalculate interesting grids */
                target_set_prepare(mode);
                pos = plr->pos;
                flag = FALSE; /* XXX XXX XXX */
                break;
            case 'o':
                flag = FALSE;
                break;
            case 'm':
                break;
            default:
                /* Extract the action (if any) */
                d = get_keymap_dir(query);
                if (!d) bell();
            }

            /* Hack -- move around */
            if (d)
            {
                /* Modified to scroll to monster */
                point_t pos2 = viewport_origin;
                point_t v = point_create(ddx[d], ddy[d]);

                /* Find a new monster */
                i = target_pick(pos, v);

                /* Request to target past last interesting grid */
                while (flag && (i < 0))
                {
                    /* Note the change */
                    if (viewport_scroll(v.y, v.x)) /* XXX */
                    {
                        /* Recalculate interesting grids */
                        target_set_prepare(mode);

                        /* Look at interesting grids */
                        flag = TRUE;

                        /* Find a new monster */
                        i = target_pick(pos, v);

                        /* Use that grid */
                        if (i >= 0) m = i;
                    }

                    /* Nothing interesting */
                    else
                    {
                        int dx = v.x;
                        int dy = v.y;

                        /* Restore previous position */
                        viewport_origin = pos2;

                        plr->update |= (PU_MONSTERS); /* XXX Why? */
                        plr->redraw |= (PR_MAP);
                        plr->window |= (PW_OVERHEAD);
                        redraw_hack = TRUE;
                        handle_stuff();
                        redraw_hack = FALSE;

                        target_set_prepare(mode);

                        /* Look at boring grids */
                        flag = FALSE;

                        /* Move */
                        pos = point_add(pos, v);

                        /* Do not move horizontally if unnecessary */
                        if (((pos.x < viewport_origin.x + map_rect.cx / 2) && (dx > 0)) ||
                             ((pos.x > viewport_origin.x + map_rect.cx / 2) && (dx < 0)))
                        {
                            dx = 0;
                        }

                        /* Do not move vertically if unnecessary */
                        if (((pos.y < viewport_origin.y + map_rect.cy / 2) && (dy > 0)) ||
                             ((pos.y > viewport_origin.y + map_rect.cy / 2) && (dy < 0)))
                        {
                            dy = 0;
                        }

                        /* Apply the motion */
                        if (!cave_pt_is_visible(pos))
                        {
                            if (viewport_scroll(dy, dx))
                            {
                                target_set_prepare(mode);
                                m = 0; /* XXX used to read out of bounds */
                            }
                        }

                        /* Slide into legality */
                        if (pos.x >= max_pos.x) pos.x = max_pos.x - 1;
                        else if (pos.x <= min_pos.x) pos.x = min_pos.x + 1;

                        if (pos.y >= max_pos.y) pos.y = max_pos.y - 1;
                        else if (pos.y <= min_pos.y) pos.y = min_pos.y + 1;
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

            if (!(mode & TARGET_LOOK) && !(mode & TARGET_MARK))
                prt_path(pos.y, pos.x, (mode & TARGET_DISI) ? PROJECT_DISI : 0);

            mon = dun_mon_at(cave, pos);

            if ((mode & TARGET_MARK) && (!mon || !mon->ml))
                strcpy(info, "q,p,o,+,-,?,<dir>");
            else
                strcpy(info, "q,t,p,m,+,-,?,<dir>");


            /* Describe and Prompt (enable "TARGET_LOOK") */
            while (!(query = target_set_aux(pos, mode | TARGET_LOOK, info)));

            /* Assume no direction */
            d = 0;

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
                done = TRUE;
                break;
            case 't':
            case '.':
            case '5':
            case '0':
                if (!(mode & TARGET_MARK) || (mon && mon->ml))
                {
                    if (mode & TARGET_MARK)
                        plr->target = who_create_mon(mon);
                    else
                        plr->target = who_create_pos(pos);
                    done = TRUE;
                }
                else bell();
                break;
            case 'p':
                viewport_verify();
                plr->update |= (PU_MONSTERS);
                plr->redraw |= (PR_MAP);
                plr->window |= (PW_OVERHEAD);
                handle_stuff();
                target_set_prepare(mode);
                pos = plr->pos;
                break;
            case 'o':
                break;
            case ' ':
            case '*':
            case '+':
            case '-':
            case 'm':
                flag = TRUE;

                m = 0;
                bd = 999;

                /* Pick a nearby monster */
                for (i = 0; i < point_vec_length(temp_pts); i++)
                {
                    point_t p = point_vec_get(temp_pts, i);
                    int t = point_fast_distance(pos, p);

                    if (t < bd)
                    {
                        m = i;
                        bd = t;
                    }
                }

                /* Nothing interesting */
                if (bd == 999) flag = FALSE;
                break;
            default:
                /* Extract the action (if any) */
                d = get_keymap_dir(query);

                /* XTRA HACK MOVEFAST */
                if (isupper(query)) move_fast = TRUE;

                if (!d) bell();
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
                    pos = point_jump(pos, d, mag);
                }
                else
                {
                    pos = point_step(pos, d);
                }

                /* Do not move horizontally if unnecessary */
                if (((pos.x < viewport_origin.x + map_rect.cx / 2) && (dx > 0)) ||
                     ((pos.x > viewport_origin.x + map_rect.cx / 2) && (dx < 0)))
                {
                    dx = 0;
                }

                /* Do not move vertically if unnecessary */
                if (((pos.y < viewport_origin.y + map_rect.cy / 2) && (dy > 0)) ||
                     ((pos.y > viewport_origin.y + map_rect.cy / 2) && (dy < 0)))
                {
                    dy = 0;
                }

                /* Apply the motion */
                if (!cave_pt_is_visible(pos))
                {
                    if (viewport_scroll(dy, dx)) target_set_prepare(mode);
                }

                /* Slide into legality */
                if (pos.x >= max_pos.x) pos.x = max_pos.x - 1;
                else if (pos.x <= min_pos.x) pos.x = min_pos.x + 1;

                if (pos.y >= max_pos.y) pos.y = max_pos.y - 1;
                else if (pos.y <= min_pos.y) pos.y = min_pos.y + 1;
            }
        }
    }

    /* Forget */
    point_vec_clear(temp_pts);

    msg_line_clear();
    prt("", 0, 0);

    /* Recenter the map around the player */
    viewport_verify();

    /* Update stuff */
    plr->update |= (PU_MONSTERS);

    /* Redraw map */
    plr->redraw |= (PR_MAP | PR_HEALTH_BARS);

    /* Window stuff */
    plr->window |= (PW_OVERHEAD | PW_MONSTER_LIST);

    /* Prevent losing visibility on newly acquired target (PU_MONSTERS above) */
    redraw_hack = TRUE;
    handle_stuff();
    redraw_hack = FALSE;

    return !who_is_null(plr->target);
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
point_t get_fire_pos(void) { return get_fire_pos_aux(TARGET_KILL); }
point_t get_fire_pos_aux(int mode)
{
    point_t pos = point_create(-1, -1);
    int dir = 5;
    if (get_fire_dir_aux(&dir, mode))
    {
        if (dir == 5)
        {
            pos = who_pos(plr->target);
            /* verify positional targets for LoS (or RACE_MON_BEHOLDER can gaze thru curtains) */
            if (who_is_pos(plr->target) && (mode & TARGET_LOS))
            {
                u32b flgs = PROJECT_LOS;
                int rng = project_length > 0 ? project_length : DUN_PATH_MAX;
                dun_path_ptr path;

                if (!(mode & TARGET_BALL)) flgs |= PROJECT_STOP;
                path = dun_path_alloc_aux(cave, plr->pos, pos, flgs, rng);
                pos = path->stop;
                dun_path_free(path);
            }
        }
        else
        {
            rect_t r = rect_interior(plr_dun()->rect);
            pos = point_jump_clipped(plr->pos, dir, DUN_PATH_MAX, r);
            /* a ball in a given direction will target first monster along path */
            if (mode & TARGET_BALL)
            {
                u32b flgs = PROJECT_STOP;
                int rng = project_length > 0 ? project_length : DUN_PATH_MAX;
                dun_path_ptr path;

                if (mode & TARGET_DISI) flgs |= PROJECT_DISI;
                if (mode & TARGET_LOS) flgs |= PROJECT_LOS;
                path = dun_path_alloc_aux(cave, plr->pos, pos, flgs, rng);
                pos = path->stop;
                dun_path_free(path);
            }
        }
    }
    return pos;
}
bool get_fire_dir(int *dp) { return get_fire_dir_aux(dp, TARGET_KILL); }
bool get_fire_dir_aux(int *dp, int mode)
{
    bool valid_target = FALSE;
    if (use_old_target && target_okay_aux(mode))
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
            if (!target_pet && mon_is_pet(mon)) continue;
            if (!_target_project(plr->pos, mon->pos, mode)) continue;
            if (mon->cdis < best_dis)
            {
                best_dis = mon->cdis;
                best_m_id = mon->id;
            }
        }
        int_map_iter_free(iter);

        if (best_m_id)
        {
            plr->target = who_create_mon(dun_mon(cave, best_m_id));
            *dp = 5;
            plr->redraw |= PR_HEALTH_BARS;
            return TRUE;
        }
    }
    /* fall back on normal target selection */
    return get_aim_dir_aux(dp, mode);
}

point_t get_aim_pos(void) { return get_aim_pos_aux(TARGET_KILL); }
point_t get_aim_pos_aux(int mode)
{
    point_t pos = point_create(-1, -1);
    int dir = 5;
    if (get_aim_dir_aux(&dir, mode))
    {
        if (dir == 5)
            pos = who_pos(plr->target);
        else
        {
            rect_t r = rect_interior(plr_dun()->rect);
            pos = point_jump_clipped(plr->pos, dir, DUN_PATH_MAX, r);
        }
    }
    return pos;
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
    else if (plr->riding)
    {
        monster_type *m_ptr = dun_mon(cave, plr->riding);
        monster_race *r_ptr = m_ptr->race;

        if (mon_tim_find(m_ptr, T_CONFUSED))
        {
            /* Standard confusion */
            if (randint0(100) < 75)
            {
                /* Random direction */
                dir = ddd[randint0(8)];
            }
        }
        else if (r_ptr->move.random && randint0(100) < r_ptr->move.random)
        {
            dir = ddd[randint0(8)];
        }
    }
    else if (plr->move_random)
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
        else if (plr->move_random)
            cmsg_print(TERM_YELLOW, "You are moving erratically.");
        else
        {
            char m_name[80];
            monster_type *m_ptr = dun_mon(cave, plr->riding);

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
static void target_pos_prepare(void)
{
    if (!expand_list) return;

    point_vec_clear(temp_pts);

    point_vec_add(temp_pts, plr->pos);

    if (dun_pos_interior(cave, travel.pos))
        point_vec_add(temp_pts, travel.pos);
    else
        travel.pos = point_create(0, 0);

    if (dun_pos_interior(cave, travel.last_pos))
        point_vec_add(temp_pts, travel.last_pos);
    else
        travel.last_pos = point_create(0, 0);

    if (!plr_tim_find(T_HALLUCINATE))
    {
        dun_stairs_ptr stairs;

        for (stairs = cave->stairs; stairs; stairs = stairs->next)
        {
            if (dun_cell_at(cave, stairs->pos_here)->flags & CELL_MAP)
                point_vec_add(temp_pts, stairs->pos_here);
        }
    }
    point_vec_sort(temp_pts, _cmp_distance);
}

/*
 * old -- from PsiAngband.
 */
point_t target_pos(int rng)
{
    point_t pos = plr->pos;
    char ch = 0;
    int d, n = 0;
    bool success = FALSE;
    rect_t map_rect = ui_map_rect();
    point_t min_pos = rect_top_left(cave->rect);
    point_t max_pos = rect_bottom_right(cave->rect);

    if (expand_list)
    {
        target_pos_prepare();
        n = 0;
    }

    msg_print("Select a point and press <color:y>space</color>.");

    while (ch != ESCAPE && !success)
    {
        bool move_fast = FALSE;

        move_cursor_relative(pos);
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
            if (point_equals(pos, plr->pos)) ch = 0;

            /* okay place */
            else success = TRUE;

            break;

        case '`':
            if ( expand_list
              && point_vec_length(temp_pts)
              && ( dun_pos_interior(cave, travel.pos)
                || dun_pos_interior(cave, travel.last_pos) ) )
            {
                n++;

                while (n < point_vec_length(temp_pts))
                {
                    point_t p = point_vec_get(temp_pts, n);
                    if (!point_equals(pos, p))
                    {
                        if (point_equals(p, travel.pos)) break;
                        if (point_equals(p, travel.last_pos)) break;
                    }
                    n++;
                }

                if (n == point_vec_length(temp_pts))    /* Loop out taget list */
                {
                    n = 0;
                    pos = plr->pos;
                    viewport_verify();    /* Move cursor to player */
                    plr->update |= (PU_MONSTERS);
                    plr->redraw |= (PR_MAP);
                    plr->window |= (PW_OVERHEAD);
                    handle_stuff();
                }
                else 
                {
                    pos = point_vec_get(temp_pts, n);
                    viewport_verify_aux(pos, VIEWPORT_FORCE_CENTER);
                    plr->redraw |= (PR_MAP);
                    redraw_hack = TRUE;
                    handle_stuff();
                    redraw_hack = FALSE;
                }
            }
            break;

        /* XAngband: Move cursor to stairs */
        case '>':
        case '<':
            if (expand_list && point_vec_length(temp_pts))
            {
                n++;
                while (n < point_vec_length(temp_pts))
                {
                    point_t temp = point_vec_get(temp_pts, n);
                    dun_cell_ptr cell = dun_cell_at(cave, temp);

                    if (ch == '>')
                    {
                        if (!stairs_go_down(cell))
                            n++;
                        else
                            break;
                    }
                    else /* if (ch == '<') */
                    {
                        if (!stairs_go_up(cell))
                            n++;
                        else
                            break;
                    }
                }

                if (n == point_vec_length(temp_pts))    /* Loop out taget list */
                {
                    n = 0;
                    pos = plr->pos;
                    viewport_verify();    /* Move cursor to player */
                    plr->update |= (PU_MONSTERS);
                    plr->redraw |= (PR_MAP);
                    plr->window |= (PW_OVERHEAD);
                    handle_stuff();
                }
                else    /* move cursor to next stair and change panel */
                {
                    pos = point_vec_get(temp_pts, n);
                    viewport_verify_aux(pos, VIEWPORT_FORCE_CENTER);
                    plr->redraw |= (PR_MAP);
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
                point_t old = pos;

                /* XTRA HACK MOVEFAST */
                if (move_fast)
                {
                    int mag = MIN(map_rect.cx / 2, map_rect.cy / 2);
                    pos = point_jump(pos, d, mag);
                }
                else
                {
                    pos = point_step(pos, d);
                }

                if (rng > 0 && point_distance(plr->pos, pos) > rng)
                {
                    bell();
                    pos = old;
                    continue;
                }

                /* Do not move horizontally if unnecessary */
                if (((pos.x < viewport_origin.x + map_rect.cx / 2) && (dx > 0)) ||
                     ((pos.x > viewport_origin.x + map_rect.cx / 2) && (dx < 0)))
                {
                    dx = 0;
                }

                /* Do not move vertically if unnecessary */
                if (((pos.y < viewport_origin.y + map_rect.cy / 2) && (dy > 0)) ||
                     ((pos.y > viewport_origin.y + map_rect.cy / 2) && (dy < 0)))
                {
                    dy = 0;
                }

                /* Apply the motion */
                if (!cave_pt_is_visible(pos))
                {
                    /* if (change_panel(dy, dx)) target_set_prepare(mode); */
                    viewport_scroll(dy, dx);
                }

                /* Slide into legality */
                if (pos.x >= max_pos.x) pos.x = max_pos.x - 1;
                else if (pos.x <= min_pos.x) pos.x = min_pos.x + 1;

                if (pos.y >= max_pos.y) pos.y = max_pos.y - 1;
                else if (pos.y <= min_pos.y) pos.y = min_pos.y + 1;
            }
            break;
        }
    }
    point_vec_clear(temp_pts);

    msg_line_clear();

    /* Recenter the map around the player */
    viewport_verify();

    /* Update stuff */
    plr->update |= (PU_MONSTERS);

    /* Redraw map */
    plr->redraw |= (PR_MAP);

    /* Window stuff */
    plr->window |= (PW_OVERHEAD);

    /* Handle stuff */
    handle_stuff();

    if (success) return pos;
    return point_create(-1, -1);
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
    return align_desc(plr->align);
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
