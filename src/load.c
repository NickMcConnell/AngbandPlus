/* File: load.c */

/*
 * Copyright (c) 1997 Ben Harrison, and others
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies. Other copyrights may also apply.
 */

/* Purpose: support for loading savefiles -BEN- */

#include "angband.h"
#include "dun.h"

#include <assert.h>

/*
 * Hack -- Show information on the screen, one line at a time.
 *
 * Avoid the top two lines, to avoid interference with "msg_print()".
 */
static void note(cptr msg)
{
    static int y = 2;

    /* Draw the message */
    prt(msg, y, 0);

    /* Advance one line (wrap if needed) */
    if (++y >= 24) y = 2;

    /* Flush it */
    Term_fresh();
}

void rd_item(savefile_ptr file, object_type *o_ptr)
{
    obj_load(o_ptr, file);
}



static void rd_lore_aux(savefile_ptr file, mon_race_ptr race)
{
    bool pact = FALSE;
    int  i, j, ct_blows, ct_effects, ct_auras;

    race->r_sights = savefile_read_s16b(file);
    race->r_deaths = savefile_read_s16b(file);
    race->r_pkills = savefile_read_s16b(file);
    race->r_akills = savefile_read_s16b(file);
    race->r_skills = savefile_read_s16b(file);
    race->r_tkills = savefile_read_s16b(file);
    race->r_wake = savefile_read_byte(file);
    race->r_ignore = savefile_read_byte(file);
    race->r_xtra1 = savefile_read_byte(file);
    race->r_xtra2 = savefile_read_byte(file);
    race->r_drop_gold = savefile_read_byte(file);
    race->r_drop_item = savefile_read_byte(file);
    race->r_spell_turns = savefile_read_u32b(file);
    race->r_move_turns = savefile_read_u32b(file);

    race->r_flags1 = savefile_read_u32b(file);
    race->r_flags2 = savefile_read_u32b(file);
    race->r_flags3 = savefile_read_u32b(file);
    race->r_flagsr = savefile_read_u32b(file);

    mon_spells_load(race->spells, file);
    ct_blows = savefile_read_byte(file);
    for (i = 0; i < ct_blows; i++)
    {
        mon_blow_ptr blow;
        if (i >= vec_length(race->blows))
        {
            savefile_read_s16b(file);
            ct_effects = savefile_read_byte(file);
            for (j = 0; j < ct_effects; j++)
                savefile_read_s16b(file);
            continue;
        }
        blow = vec_get(race->blows, i);
        blow->lore = savefile_read_s16b(file);
        ct_effects = savefile_read_byte(file);
        for (j = 0; j < ct_effects; j++)
        {
            if (j >= blow->effect_ct)
                savefile_read_s16b(file);
            else
                blow->effects[j].lore = savefile_read_s16b(file);
        }
    }
    ct_auras = savefile_read_byte(file);
    for (i = 0; i < ct_auras; i++)
    {
        mon_effect_ptr aura = &race->auras[i];
        aura->lore = savefile_read_s16b(file);
    }

    if (race->r_flagsr & (RFR_PACT_MONSTER)) pact = TRUE;

    /* Repair the lore flags */
    race->r_flags1 &= race->flags1;
    race->r_flags2 &= race->flags2;
    race->r_flags3 &= race->flags3;
    race->r_flagsr &= race->flagsr;

    if (pact)
        race->r_flagsr |= RFR_PACT_MONSTER;
}
static void rd_randomizer(savefile_ptr file)
{
    int i;
    Rand_place = savefile_read_u16b(file);

    for (i = 0; i < RAND_DEG; i++)
        Rand_state[i] = savefile_read_u32b(file);

    Rand_quick = FALSE;
}


/*
 * Read options (ignore most pre-2.8.0 options)
 *
 * Note that the normal options are now stored as a set of 256 bit flags,
 * plus a set of 256 bit masks to indicate which bit flags were defined
 * at the time the savefile was created. This will allow new options
 * to be added, and old options to be removed, at any time, without
 * hurting old savefiles.
 *
 * The window options are stored in the same way, but note that each
 * window gets 32 options, and their order is fixed by certain defines.
 */
static void rd_options(savefile_ptr file)
{
    int i, n;
    u16b c;
    u32b flag[8];
    u32b mask[8];

    if (savefile_is_older_than(file, 7, 1, 0, 8))
        savefile_read_byte(file);
    else
    {
        delay_animation = savefile_read_s16b(file);
        delay_run = savefile_read_s16b(file);
    }
    hitpoint_warn = savefile_read_byte(file);
    mana_warn = savefile_read_byte(file);
    random_artifact_pct = savefile_read_byte(file);

    /*** Cheating options ***/
    c = savefile_read_u16b(file);
    if (c & 0x0002) p_ptr->wizard = TRUE;

    autosave_l = savefile_read_byte(file);
    autosave_t = savefile_read_byte(file);
    autosave_freq = savefile_read_s16b(file);


    /*** Normal Options ***/
    for (n = 0; n < 8; n++) flag[n] = savefile_read_u32b(file);
    for (n = 0; n < 8; n++) mask[n] = savefile_read_u32b(file);

    for (n = 0; n < 8; n++)
    {
        for (i = 0; i < 32; i++)
        {
            if (mask[n] & (1L << i))
            {
                if (option_mask[n] & (1L << i))
                {
                    if (flag[n] & (1L << i))
                        option_flag[n] |= (1L << i);
                    else
                        option_flag[n] &= ~(1L << i);
                }
            }
        }
    }

    /* Extract the options */
    extract_option_vars();

    /*** Window Options ***/
    for (n = 0; n < 8; n++) flag[n] = savefile_read_u32b(file);
    for (n = 0; n < 8; n++) mask[n] = savefile_read_u32b(file);

    for (n = 0; n < 8; n++)
    {
        for (i = 0; i < 32; i++)
        {
            if (mask[n] & (1L << i))
            {
                if (window_mask[n] & (1L << i))
                {
                    if (flag[n] & (1L << i))
                        window_flag[n] |= (1L << i);
                    else
                        window_flag[n] &= ~(1L << i);
                }
            }
        }
    }
}

static void rd_quick_start(savefile_ptr file)
{
    int i;

    previous_char.game_mode = savefile_read_byte(file);
    previous_char.psex = savefile_read_byte(file);
    previous_char.prace = savefile_read_s16b(file);
    previous_char.psubrace = savefile_read_byte(file);
    previous_char.pclass = savefile_read_byte(file);
    previous_char.psubclass = savefile_read_byte(file);
    previous_char.personality = savefile_read_byte(file);
    previous_char.realm1 = savefile_read_byte(file);
    previous_char.realm2 = savefile_read_byte(file);
    previous_char.dragon_realm = savefile_read_byte(file);
    previous_char.au = savefile_read_s32b(file);

    for (i = 0; i < 6; i++)
        previous_char.stat_max[i] = savefile_read_s16b(file);
    previous_char.quick_ok = savefile_read_byte(file);
}

static void rd_extra(savefile_ptr file)
{
    int i,j;
    char buf[1024];

    p_ptr->id = savefile_read_s32b(file);
    savefile_read_cptr(file, player_name, sizeof(player_name));
    savefile_read_cptr(file, p_ptr->died_from, sizeof(p_ptr->died_from));

    savefile_read_cptr(file, buf, sizeof buf);
    if (buf[0]) p_ptr->last_message = z_string_make(buf);

    rd_quick_start(file);

    game_mode = savefile_read_s32b(file);

    p_ptr->prace = savefile_read_s16b(file);
    p_ptr->pclass = savefile_read_byte(file);
    p_ptr->personality = savefile_read_byte(file);
    p_ptr->psex = savefile_read_byte(file);
    p_ptr->realm1 = savefile_read_byte(file);
    p_ptr->realm2 = savefile_read_byte(file);
    p_ptr->dragon_realm = savefile_read_byte(file);
    p_ptr->psubclass = savefile_read_byte(file);
    p_ptr->psubrace = savefile_read_byte(file);
    p_ptr->current_r_idx = savefile_read_s16b(file);
    p_ptr->expfact = savefile_read_u16b(file);

    for (i = 0; i < 6; i++) p_ptr->stat_max[i] = savefile_read_s16b(file);
    for (i = 0; i < 6; i++) p_ptr->stat_max_max[i] = savefile_read_s16b(file);
    for (i = 0; i < 6; i++) p_ptr->stat_cur[i] = savefile_read_s16b(file);

    p_ptr->au = savefile_read_s32b(file);
    p_ptr->fame = savefile_read_s16b(file);
    p_ptr->max_exp = savefile_read_s32b(file);
    p_ptr->max_max_exp = savefile_read_s32b(file);
    p_ptr->exp = savefile_read_s32b(file);
    p_ptr->exp_frac = savefile_read_u32b(file);
    p_ptr->lev = savefile_read_s16b(file);

    for (i = 0; i < 64; i++) p_ptr->spell_exp[i] = savefile_read_s16b(file);
    for (i = 0; i < 5; i++) for (j = 0; j < 64; j++) p_ptr->weapon_exp[i][j] = savefile_read_s16b(file);
    for (i = 0; i < 10; i++) p_ptr->skill_exp[i] = savefile_read_s16b(file);
    for (i = 0; i < MAX_MAGIC_NUM; i++) p_ptr->magic_num1[i] = savefile_read_s32b(file);
    for (i = 0; i < MAX_MAGIC_NUM; i++) p_ptr->magic_num2[i] = savefile_read_byte(file);
    if (music_singing_any()) p_ptr->action = ACTION_SING;

    p_ptr->start_race = savefile_read_s16b(file);
    p_ptr->old_race1 = savefile_read_s32b(file);
    p_ptr->old_race2 = savefile_read_s32b(file);
    p_ptr->old_realm = savefile_read_s16b(file);

    for (i = 0; i < MAX_KUBI; i++)
        kubi_r_idx[i] = savefile_read_s16b(file);

    p_ptr->mmhp = savefile_read_s32b(file);
    p_ptr->mhp = savefile_read_s32b(file);
    p_ptr->chp = savefile_read_s32b(file);
    p_ptr->chp_frac = savefile_read_u32b(file);
    p_ptr->msp = savefile_read_s32b(file);
    p_ptr->csp = savefile_read_s32b(file);
    p_ptr->csp_frac = savefile_read_u32b(file);
    p_ptr->clp = savefile_read_s16b(file);
    p_ptr->max_plv = savefile_read_s16b(file);

    if (p_ptr->max_plv < p_ptr->lev) p_ptr->max_plv = p_ptr->lev;

    p_ptr->concent = savefile_read_s16b(file);
    p_ptr->food = savefile_read_s16b(file);
    p_ptr->energy_need = savefile_read_s16b(file);
    plr_tim_load(file);
    p_ptr->afraid = savefile_read_s16b(file);
    p_ptr->see_infra = savefile_read_s16b(file);
    p_ptr->mimic_form = savefile_read_s16b(file);
    p_ptr->tim_mimic = savefile_read_s16b(file);
    {
        int i, ct;
        monster_race *r_ptr = &r_info[MON_MONKEY_CLONE];
        r_ptr->cur_num = savefile_read_byte(file);
        if (r_ptr->cur_num)
        {
            r_ptr->hdice = savefile_read_byte(file);
            r_ptr->hside = savefile_read_byte(file);
            r_ptr->ac = savefile_read_s16b(file);
            r_ptr->speed = savefile_read_byte(file);
            ct = savefile_read_byte(file);
            vec_clear(r_ptr->blows);
            for (i = 0; i < ct; i++)
            {
                int method = savefile_read_byte(file);
                mon_blow_ptr blow = mon_blow_alloc(method);
                int blows = savefile_read_s16b(file);
                int e = savefile_read_s16b(file);
                int dd = savefile_read_s16b(file);
                int ds = savefile_read_s16b(file);
                int base = savefile_read_s16b(file);
                blow->blows = blows;
                mon_blow_push_effect(blow, e, dice_create(dd, ds, base));
                vec_add(r_ptr->blows, blow);
            }
            r_ptr->flags3 = savefile_read_u32b(file);
            r_ptr->flagsr = savefile_read_u32b(file);
            r_ptr->flags2 = savefile_read_u32b(file);
            r_ptr->flags7 = savefile_read_u32b(file);
        }
    }

    p_ptr->entrench_x = savefile_read_s16b(file);
    p_ptr->entrench_y = savefile_read_s16b(file);
    p_ptr->entrench_ct = savefile_read_s16b(file);
    p_ptr->sense_artifact = savefile_read_byte(file);
    p_ptr->duelist_target_idx = savefile_read_s16b(file);
    p_ptr->free_turns = savefile_read_s16b(file);
    p_ptr->chaos_patron= savefile_read_s16b(file);

    for (i = 0; i < MUT_FLAG_SIZE; ++i)
        p_ptr->muta[i] = savefile_read_u32b(file);

    for (i = 0; i < MUT_FLAG_SIZE; ++i)
        p_ptr->muta_lock[i] = savefile_read_u32b(file);

    for (i = 0; i < MAX_DEMIGOD_POWERS; ++i)
        p_ptr->demigod_power[i] = savefile_read_s16b(file);

    p_ptr->draconian_power = savefile_read_s16b(file);

    for (i = 0; i < 8; i++)
        p_ptr->virtues[i] = savefile_read_s16b(file);
    for (i = 0; i < 8; i++)
        p_ptr->vir_types[i] = savefile_read_s16b(file);

    mutant_regenerate_mod = mut_regenerate_mod();

    p_ptr->special_attack = savefile_read_u32b(file);

    if (p_ptr->special_attack & KAMAE_MASK) p_ptr->action = ACTION_KAMAE;
    else if (p_ptr->special_attack & KATA_MASK) p_ptr->action = ACTION_KATA;

    p_ptr->special_defense = savefile_read_u32b(file);
    p_ptr->knowledge = savefile_read_byte(file);

    p_ptr->autopick_autoregister = savefile_read_byte(file) ? TRUE: FALSE;
    p_ptr->action = savefile_read_byte(file);

    seed_flavor = savefile_read_u32b(file);
    p_ptr->panic_save = savefile_read_u16b(file);
    p_ptr->total_winner = savefile_read_u16b(file);
    p_ptr->noscore = savefile_read_u16b(file);
    p_ptr->is_dead = savefile_read_byte(file) ? TRUE : FALSE;

    today_mon = savefile_read_s16b(file);
    p_ptr->today_mon = savefile_read_s16b(file);
    p_ptr->riding = savefile_read_u16b(file);

    playtime = savefile_read_u32b(file);
    p_ptr->count = savefile_read_u32b(file);

    plr_hook_load(file);
}

static errr rd_savefile_new_aux(savefile_ptr file)
{
    int i;

    u16b tmp16u;

#ifdef VERIFY_CHECKSUMS
    u32b n_x_check, n_v_check;
    u32b o_x_check, o_v_check;
#endif

    /* Mention the savefile version */
    note(format(
             "Loading a %d.%d.%d savefile...",
             (z_major > 9) ? z_major - 10 : z_major, z_minor, z_patch));

    /* hard limit due to cave re-write */
    if (savefile_is_older_than(file, 7, 1, 0, 0))
        note("Savefiles older than 7.1.0 are not supported!");

    /* Savefiles break iff VER_MAJOR bumps */
    if (savefile_is_older_than(file, VER_MAJOR, 0, 0, 0))
    {
        note(format("Savefiles older than %d.0.0 are not supported!", VER_MAJOR));
        return 1;
    }

    sf_system = savefile_read_u32b(file);
    sf_when = savefile_read_u32b(file);
    sf_lives = savefile_read_u16b(file);
    sf_saves = savefile_read_u16b(file);

    rd_randomizer(file);
    if (arg_fiddle) note("Loaded Randomizer Info");

    rd_options(file);
    if (arg_fiddle) note("Loaded Option Flags");

    msg_on_load(file);
    if (arg_fiddle) note("Loaded Messages");

    for (i = 0; i < max_r_idx; i++)
    {
        /* Access that monster */
        monster_race *r_ptr = &r_info[i];

        /* Hack -- Reset the death counter */
        r_ptr->max_num = 30000;

        if (r_ptr->flags1 & RF1_UNIQUE) r_ptr->max_num = 1;

        /* Hack -- Non-unique Nazguls are semi-unique */
        else if (r_ptr->flags7 & RF7_NAZGUL) r_ptr->max_num = MAX_NAZGUL_NUM;
        else if (i == MON_CAMELOT_KNIGHT)
            r_ptr->max_num = MAX_CAMELOT_KNIGHT_NUM;
    }

    /* Monster Memory */
    tmp16u = savefile_read_u16b(file);

    if (tmp16u > max_r_idx)
    {
        note(format("Too many (%u) monster races!", tmp16u));
        return 22;
    }
    for (i = 0; i < tmp16u; i++)
    {
        mon_race_ptr race = &r_info[i];
        byte header = savefile_read_byte(file);

        race->max_num = savefile_read_s16b(file);
        race->stolen_ct = savefile_read_byte(file);
        if (header & 0x01)
            race->flagsx = savefile_read_u32b(file);
        if (header & 0x02)
            rd_lore_aux(file, race);
    }
    if (arg_fiddle) note("Loaded Monster Memory");


    /* Object Memory */
    tmp16u = savefile_read_u16b(file);

    if (tmp16u > max_k_idx)
    {
        note(format("Too many (%u) object kinds!", tmp16u));
        return (22);
    }
    for (i = 0; i < tmp16u; i++)
    {
        byte tmp8u;
        object_kind *k_ptr = &k_info[i];

        tmp8u = savefile_read_byte(file);

        k_ptr->aware = (tmp8u & 0x01) ? TRUE: FALSE;
        k_ptr->tried = (tmp8u & 0x02) ? TRUE: FALSE;

        if (tmp8u & 0x04)
        {
            k_ptr->counts.generated = savefile_read_s16b(file);
            k_ptr->counts.found = savefile_read_s16b(file);
            k_ptr->counts.bought = savefile_read_s16b(file);
            k_ptr->counts.used = savefile_read_s16b(file);
            k_ptr->counts.destroyed = savefile_read_s16b(file);
        }
    }

    for (;;)
    {
        int e_idx = savefile_read_s16b(file);
        ego_ptr ego;
        if (e_idx < 0) break;
        if (e_idx > max_e_idx)
        {
            note(format("Ego (%d) out of range!", e_idx));
            return (22);
        }
        ego = &e_info[e_idx];
        for (;;)
        {
            byte b = savefile_read_byte(file);
            if (b == 0xFF) break;
            assert(/*0 <= b &&*/ b < OF_ARRAY_SIZE);
            ego->known_flags[b] = savefile_read_u32b(file);
        }
        for (;;)
        {
            byte b = savefile_read_byte(file);
            if (b == 0xFF) break;
            assert(/*0 <= b &&*/ b < OF_ARRAY_SIZE);
            ego->xtra_flags[b] = savefile_read_u32b(file);
        }
        ego->counts.generated = savefile_read_s16b(file);
        ego->counts.found = savefile_read_s16b(file);
        ego->counts.bought = savefile_read_s16b(file);
        /*ego->counts.used = savefile_read_s16b(file);*/
        ego->counts.destroyed = savefile_read_s16b(file);
    }
    for (;;)
    {
        int a_idx = savefile_read_s16b(file);
        art_ptr art;
        if (a_idx < 0) break;
        if (a_idx > max_a_idx)
        {
            note(format("Art (%d) out of range!", a_idx));
            return (22);
        }
        art = &a_info[a_idx];
        for (;;)
        {
            byte b = savefile_read_byte(file);
            if (b == 0xFF) break;
            assert(/*0 <= b &&*/ b < OF_ARRAY_SIZE);
            art->known_flags[b] = savefile_read_u32b(file);
        }
    }
    if (arg_fiddle) note("Loaded Object Memory");

    quests_load(file); /* quests must load after monster lore ... see above */
    if (arg_fiddle) note("Loaded Quests");

    /* Load the Artifacts */
    tmp16u = savefile_read_u16b(file);
    if (tmp16u > max_a_idx)
    {
        note(format("Too many (%u) artifacts!", tmp16u));
        return (24);
    }
    for (i = 0; i < tmp16u; i++)
    {
        artifact_type *a_ptr = &a_info[i];
        a_ptr->generated = savefile_read_byte(file);
        a_ptr->found = savefile_read_byte(file);
    }
    if (arg_fiddle) note("Loaded Artifacts");


    rd_extra(file);

    if (p_ptr->energy_need < -999) world_player = TRUE;

    if (arg_fiddle) note("Loaded extra information");

    /* Read the player_hp array */
    tmp16u = savefile_read_u16b(file);
    if (tmp16u > PY_MAX_LEVEL)
    {
        note(format("Too many (%u) hitpoint entries!", tmp16u));
        return (25);
    }
    for (i = 0; i < tmp16u; i++)
        p_ptr->player_hp[i] = savefile_read_s16b(file);

    /* Important -- Initialize stuff */
    mp_ptr = &m_info[p_ptr->pclass];
    /* Hack ... external files always make easy stuff hard ... Burglary is natural for rogues!!!*/
    /* Note: Class and Realm are read in in rd_extra a few lines back */
    if (p_ptr->pclass == CLASS_ROGUE)
    {
        if (p_ptr->realm1 == REALM_BURGLARY)
            mp_ptr->spell_first = 1;
        else
            mp_ptr->spell_first = 5;
    }

    /* Read spell info */
    p_ptr->spell_learned1 = savefile_read_u32b(file);
    p_ptr->spell_learned2 = savefile_read_u32b(file);
    p_ptr->spell_worked1 = savefile_read_u32b(file);
    p_ptr->spell_worked2 = savefile_read_u32b(file);
    p_ptr->spell_forgotten1 = savefile_read_u32b(file);
    p_ptr->spell_forgotten2 = savefile_read_u32b(file);
    p_ptr->learned_spells = savefile_read_s16b(file);
    p_ptr->add_spells = savefile_read_s16b(file);
    if (p_ptr->pclass == CLASS_MINDCRAFTER) p_ptr->add_spells = 0;

    for (i = 0; i < 64; i++)
        p_ptr->spell_order[i] = savefile_read_byte(file);


    /* Read the inventory */
    equip_init();
    pack_init();
    quiver_init();
    home_init();

    art_load(file);
    equip_load(file);
    pack_load(file);
    quiver_load(file);
    towns_load(file);
    home_load(file);

    p_ptr->pet_follow_distance = savefile_read_s16b(file);
    p_ptr->pet_extra_flags = savefile_read_s16b(file);

    spell_stats_on_load(file);
    skills_on_load(file);
    stats_on_load(file);

    /* I'm not dead yet... */
    if (!p_ptr->is_dead)
    {
        int tmp_ix = p_ptr->duelist_target_idx;
        note("Restoring Dungeon...");
        dun_mgr_load(file);
        p_ptr->duelist_target_idx = tmp_ix;
    }

#ifdef VERIFY_CHECKSUMS

    n_v_check = file->v_check;
    o_v_check = savefile_read_u32b(file);
    if (o_v_check != n_v_check)
    {
        note("Invalid checksum");
        return (11);
    }
    n_x_check = file->x_check;
    o_x_check = savefile_read_u32b(file);
    if (o_x_check != n_x_check)
    {
        note("Invalid encoded checksum");
        return (11);
    }

#endif

    return 0;
}


/*
 * Actually read the savefile
 */
errr rd_savefile_new(void)
{
    errr err;
    savefile_ptr file = savefile_open_read(savefile);

    if (!file) return -1;

    err = rd_savefile_new_aux(file);
    if (ferror(file->file)) err = -1;

    savefile_close(file);
    return err;
}


