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

    delay_animation = savefile_read_s16b(file);
    delay_run = savefile_read_s16b(file);
    delay_rest = savefile_read_s16b(file);
    hitpoint_warn = savefile_read_byte(file);
    mana_warn = savefile_read_byte(file);
    random_artifact_pct = savefile_read_byte(file);

    /*** Cheating options ***/
    c = savefile_read_u16b(file);
    if (c & 0x0002) plr->wizard = TRUE;

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

    previous_char.initial_world_id = savefile_read_s16b(file);
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

    plr->id = savefile_read_s32b(file);
    savefile_read_cptr(file, player_name, sizeof(player_name));
    savefile_read_cptr(file, plr->died_from, sizeof(plr->died_from));

    savefile_read_cptr(file, buf, sizeof buf);
    if (buf[0]) plr->last_message = z_string_make(buf);

    rd_quick_start(file);

    game_mode = savefile_read_s32b(file);

    plr->prace = savefile_read_s16b(file);
    plr->pclass = savefile_read_byte(file);
    plr->personality = savefile_read_byte(file);
    plr->psex = savefile_read_byte(file);
    plr->realm1 = savefile_read_byte(file);
    plr->realm2 = savefile_read_byte(file);
    plr->dragon_realm = savefile_read_byte(file);
    plr->psubclass = savefile_read_byte(file);
    plr->psubrace = savefile_read_byte(file);
    plr->current_r_idx = savefile_read_sym(file);
    plr->expfact = savefile_read_u16b(file);

    for (i = 0; i < 6; i++) plr->stat_max[i] = savefile_read_s16b(file);
    for (i = 0; i < 6; i++) plr->stat_max_max[i] = savefile_read_s16b(file);
    for (i = 0; i < 6; i++) plr->stat_cur[i] = savefile_read_s16b(file);

    plr->au = savefile_read_s32b(file);
    plr->fame = savefile_read_s16b(file);
    plr->max_exp = savefile_read_s32b(file);
    plr->max_max_exp = savefile_read_s32b(file);
    plr->exp = savefile_read_s32b(file);
    plr->exp_frac = savefile_read_u32b(file);
    plr->lev = savefile_read_s16b(file);

    for (i = 0; i < 64; i++) plr->spell_exp[i] = savefile_read_s16b(file);
    for (i = 0; i < 5; i++) for (j = 0; j < 64; j++) plr->weapon_exp[i][j] = savefile_read_s16b(file);
    for (i = 0; i < 10; i++) plr->skill_exp[i] = savefile_read_s16b(file);
    for (i = 0; i < MAX_MAGIC_NUM; i++) plr->magic_num1[i] = savefile_read_s32b(file);
    for (i = 0; i < MAX_MAGIC_NUM; i++) plr->magic_num2[i] = savefile_read_byte(file);

    plr->start_race = savefile_read_s16b(file);
    plr->old_race1 = savefile_read_s32b(file);
    plr->old_race2 = savefile_read_s32b(file);
    plr->old_realm = savefile_read_s16b(file);

    for (i = 0; i < MAX_KUBI; i++)
        kubi_r_idx[i] = savefile_read_sym(file);

    plr->mmhp = savefile_read_s32b(file);
    plr->mhp = savefile_read_s32b(file);
    plr->chp = savefile_read_s32b(file);
    plr->chp_frac = savefile_read_u32b(file);
    plr->msp = savefile_read_s32b(file);
    plr->csp = savefile_read_s32b(file);
    plr->csp_frac = savefile_read_u32b(file);
    plr->clp = savefile_read_s16b(file);
    plr->max_plv = savefile_read_s16b(file);

    if (plr->max_plv < plr->lev) plr->max_plv = plr->lev;

    plr->concent = savefile_read_s16b(file);
    plr->food = savefile_read_s16b(file);
    plr->energy_need = savefile_read_s16b(file);
    plr_tim_load(file);
    plr->afraid = savefile_read_s16b(file);
    plr->see_infra = savefile_read_s16b(file);
    plr->mimic_form = savefile_read_s16b(file);
    plr->tim_mimic = savefile_read_s16b(file);
    {
        int i, ct;
        monster_race *r_ptr = mon_race_parse("@.clone");
        r_ptr->alloc.cur_num = savefile_read_byte(file);
        if (r_ptr->alloc.cur_num)
        {
            r_ptr->alloc.lvl = savefile_read_byte(file);
            r_ptr->hp = dice_load(file);
            r_ptr->ac = savefile_read_s16b(file);
            r_ptr->move.speed = savefile_read_s16b(file);
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
            r_ptr->resist = savefile_read_u32b(file);
            r_ptr->immune = savefile_read_u32b(file);
            r_ptr->vuln = savefile_read_u32b(file);
            r_ptr->abilities = savefile_read_u32b(file);
            r_ptr->move.flags = savefile_read_u16b(file);
        }
    }

    plr->entrench_x = savefile_read_s16b(file);
    plr->entrench_y = savefile_read_s16b(file);
    plr->entrench_ct = savefile_read_s16b(file);
    plr->sense_artifact = savefile_read_byte(file);
    plr->duelist_target_idx = savefile_read_u32b(file);
    plr->free_turns = savefile_read_s16b(file);
    plr->chaos_patron= savefile_read_s16b(file);

    for (i = 0; i < MUT_FLAG_SIZE; ++i)
        plr->muta[i] = savefile_read_u32b(file);

    for (i = 0; i < MUT_FLAG_SIZE; ++i)
        plr->muta_lock[i] = savefile_read_u32b(file);

    for (i = 0; i < MAX_DEMIGOD_POWERS; ++i)
        plr->demigod_power[i] = savefile_read_s16b(file);

    plr->draconian_power = savefile_read_s16b(file);

    for (i = 0; i < 8; i++)
        plr->virtues[i] = savefile_read_s16b(file);
    for (i = 0; i < 8; i++)
        plr->vir_types[i] = savefile_read_s16b(file);

    mutant_regenerate_mod = mut_regenerate_mod();

    plr->special_attack = savefile_read_u32b(file);

    if (plr->special_attack & KAMAE_MASK) plr->action = ACTION_KAMAE;
    else if (plr->special_attack & KATA_MASK) plr->action = ACTION_KATA;

    plr->special_defense = savefile_read_u32b(file);
    plr->knowledge = savefile_read_byte(file);

    plr->autopick_autoregister = savefile_read_byte(file) ? TRUE: FALSE;
    plr->action = savefile_read_byte(file);

    seed_flavor = savefile_read_u32b(file);
    plr->panic_save = savefile_read_u16b(file);
    plr->total_winner = savefile_read_u16b(file);
    plr->noscore = savefile_read_u16b(file);
    plr->is_dead = savefile_read_byte(file) ? TRUE : FALSE;

    today_mon = savefile_read_sym(file);
    plr->today_mon = savefile_read_sym(file);
    plr->riding = savefile_read_u32b(file);

    playtime = savefile_read_u32b(file);
    plr->count = savefile_read_u32b(file);

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

    note(format( "Loading a %d.%d.%d savefile...", z_major, z_minor, z_patch));
    if (savefile_is_older_than(file, MIN_VER_MAJOR, MIN_VER_MINOR, 0, 0))
    {
        note(format("Savefiles older than %d.%d.0 are not supported!", MIN_VER_MAJOR, MIN_VER_MINOR));
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

    msg_load(file);
    if (arg_fiddle) note("Loaded Messages");

    /* Monster Memory */
    mon_race_load(file);
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
    arts_load(file);
    if (arg_fiddle) note("Loaded Object Memory");

    quests_load(file); /* quests must load after monster lore ... see above */
    if (arg_fiddle) note("Loaded Quests");

    rd_extra(file);

    if (plr->energy_need < -999) world_player = TRUE;

    if (arg_fiddle) note("Loaded extra information");

    /* Read the player_hp array */
    tmp16u = savefile_read_u16b(file);
    if (tmp16u > PY_MAX_LEVEL)
    {
        note(format("Too many (%u) hitpoint entries!", tmp16u));
        return (25);
    }
    for (i = 0; i < tmp16u; i++)
        plr->player_hp[i] = savefile_read_s16b(file);

    /* Important -- Initialize stuff */
    mp_ptr = &m_info[plr->pclass];
    /* Hack ... external files always make easy stuff hard ... Burglary is natural for rogues!!!*/
    /* Note: Class and Realm are read in in rd_extra a few lines back */
    if (plr->pclass == CLASS_ROGUE)
    {
        if (plr->realm1 == REALM_BURGLARY)
            mp_ptr->spell_first = 1;
        else
            mp_ptr->spell_first = 5;
    }

    /* Read spell info */
    plr->spell_learned1 = savefile_read_u32b(file);
    plr->spell_learned2 = savefile_read_u32b(file);
    plr->spell_worked1 = savefile_read_u32b(file);
    plr->spell_worked2 = savefile_read_u32b(file);
    plr->spell_forgotten1 = savefile_read_u32b(file);
    plr->spell_forgotten2 = savefile_read_u32b(file);
    plr->learned_spells = savefile_read_s16b(file);
    plr->add_spells = savefile_read_s16b(file);
    if (plr->pclass == CLASS_MINDCRAFTER) plr->add_spells = 0;

    for (i = 0; i < 64; i++)
        plr->spell_order[i] = savefile_read_byte(file);


    /* Read the inventory */
    equip_init();
    pack_init();
    quiver_init();
    home_init();

    equip_load(file);
    pack_load(file);
    quiver_load(file);
    towns_load(file);
    home_load(file);

    plr->pet_follow_distance = savefile_read_s16b(file);
    plr->pet_extra_flags = savefile_read_s16b(file);

    spell_stats_on_load(file);
    skills_on_load(file);
    stats_on_load(file);

    /* I'm not dead yet... */
    if (!plr->is_dead)
    {
        note("Restoring Dungeon...");
        dun_mgr_load(file);
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

    plr->panic_save = 0;
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


