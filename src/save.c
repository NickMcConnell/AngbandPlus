/* File: save.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies. Other copyrights may also apply.
 */

/* Purpose: interact with savefiles */

#include "angband.h"
#include "dun.h"
#include <assert.h>

void wr_item(savefile_ptr file, object_type *o_ptr)
{
    obj_save(o_ptr, file);
}

static void wr_race_lore(savefile_ptr file, mon_race_ptr race)
{
    int i, j, ct_auras = 0;
    savefile_write_s16b(file, race->r_sights);
    savefile_write_s16b(file, race->r_deaths);
    savefile_write_s16b(file, race->r_pkills);
    savefile_write_s16b(file, race->r_akills);
    savefile_write_s16b(file, race->r_skills);
    savefile_write_s16b(file, race->r_tkills);
    savefile_write_byte(file, race->r_wake);
    savefile_write_byte(file, race->r_ignore);
    savefile_write_byte(file, race->r_xtra1);
    savefile_write_byte(file, race->r_xtra2);
    savefile_write_byte(file, race->r_drop_gold);
    savefile_write_byte(file, race->r_drop_item);
    savefile_write_u32b(file, race->r_spell_turns);
    savefile_write_u32b(file, race->r_move_turns);
    savefile_write_u32b(file, race->r_flags1);
    savefile_write_u32b(file, race->r_flags2);
    savefile_write_u32b(file, race->r_flags3);
    savefile_write_u32b(file, race->r_flagsr);
    mon_spells_save(race->spells, file); /* 2 + 5S' bytes where S' is a seen spell */
    savefile_write_byte(file, vec_length(race->blows));
    for (i = 0; i < vec_length(race->blows); i++)
    {
        mon_blow_ptr blow = vec_get(race->blows, i);
        savefile_write_s16b(file, blow->lore);
        savefile_write_byte(file, blow->effect_ct);
        for (j = 0; j < blow->effect_ct; j++)
        {
            mon_effect_ptr effect = &blow->effects[j];
            savefile_write_s16b(file, effect->lore);
        }
    }
    for (i = 0; i < MAX_MON_AURAS; i++) /* was 6 bytes ... very very slight optimization */
    {                                   /* but most monsters have no A:* auras (save 6k perhaps)*/
        mon_effect_ptr aura = &race->auras[i];
        if (!aura->effect) break;
        ct_auras++;
    }
    savefile_write_byte(file, ct_auras);
    for (i = 0; i < ct_auras; i++)
    {
        mon_effect_ptr aura = &race->auras[i];
        savefile_write_s16b(file, aura->lore);
    }
}
static bool _race_has_lore(mon_race_ptr race)
{
    return race->r_sights || race->r_tkills; /* XXX */
}
static void wr_r_info(savefile_ptr file)
{
    int i;
    savefile_write_u16b(file, max_r_idx);
    for (i = 0; i < max_r_idx; i++)
    {
        mon_race_ptr race = &r_info[i];
        byte         header = 0;

        if (race->flagsx) header |= 0x01;
        if (_race_has_lore(race)) header |= 0x02;

        savefile_write_byte(file, header);
        savefile_write_s16b(file, race->max_num);
        savefile_write_byte(file, race->stolen_ct);

        if (race->flagsx)
            savefile_write_u32b(file, race->flagsx);
        if (_race_has_lore(race))
            wr_race_lore(file, race);
    }
}

static bool _have_counts(counts_ptr counts)
{
    return counts->found || counts->bought || counts->used || counts->destroyed;
}

static void wr_xtra_kind(savefile_ptr file, int k_idx)
{
    byte tmp8u = 0;

    object_kind *k_ptr = &k_info[k_idx];

    if (k_ptr->aware) tmp8u |= 0x01;
    if (k_ptr->tried) tmp8u |= 0x02;
    if (_have_counts(&k_ptr->counts)) tmp8u |= 0x04;

    savefile_write_byte(file, tmp8u);
    if (_have_counts(&k_ptr->counts))
    {
        savefile_write_s16b(file, k_ptr->counts.generated);
        savefile_write_s16b(file, k_ptr->counts.found);
        savefile_write_s16b(file, k_ptr->counts.bought);
        savefile_write_s16b(file, k_ptr->counts.used);
        savefile_write_s16b(file, k_ptr->counts.destroyed);
    }
}

static bool _ego_has_lore(ego_ptr ego)
{
    int i;
    if (_have_counts(&ego->counts)) return TRUE;
    for (i = 0; i < OF_ARRAY_SIZE; i++)
    {
        if (ego->known_flags[i]) return TRUE;
        if (ego->xtra_flags[i]) return TRUE;
    }
    return FALSE;
}
static void wr_xtra_ego_aux(savefile_ptr file, ego_ptr ego)
{
    int i;
    for (i = 0; i < OF_ARRAY_SIZE; i++)
    {
        if (!ego->known_flags[i]) continue;
        savefile_write_byte(file, i);
        savefile_write_u32b(file, ego->known_flags[i]);
    }
    savefile_write_byte(file, 0xFF);
    for (i = 0; i < OF_ARRAY_SIZE; i++)
    {
        if (!ego->xtra_flags[i]) continue;
        savefile_write_byte(file, i);
        savefile_write_u32b(file, ego->xtra_flags[i]);
    }
    savefile_write_byte(file, 0xFF);

    savefile_write_s16b(file, ego->counts.generated);
    savefile_write_s16b(file, ego->counts.found);
    savefile_write_s16b(file, ego->counts.bought);
    savefile_write_s16b(file, ego->counts.destroyed);
}
static void wr_xtra_ego(savefile_ptr file)
{
    int i;
    for (i = 0; i < max_e_idx; i++)
    {
        ego_ptr ego = &e_info[i];
        if (!ego->name) continue;
        if (!_ego_has_lore(ego)) continue;
        savefile_write_s16b(file, i);
        wr_xtra_ego_aux(file, ego);
    }
    savefile_write_s16b(file, -1);
}

static bool _art_has_lore(art_ptr art)
{
    int i;
    for (i = 0; i < OF_ARRAY_SIZE; i++)
    {
        if (art->known_flags[i]) return TRUE;
    }
    return FALSE;
}
static void wr_xtra_art_aux(savefile_ptr file, art_ptr art)
{
    int i;
    for (i = 0; i < OF_ARRAY_SIZE; i++)
    {
        if (!art->known_flags[i]) continue;
        savefile_write_byte(file, i);
        savefile_write_u32b(file, art->known_flags[i]);
    }
    savefile_write_byte(file, 0xFF);
}
static void wr_xtra_art(savefile_ptr file)
{
    int i;
    for (i = 0; i < max_a_idx; i++)
    {
        art_ptr art = &a_info[i];
        if (!art->name) continue;
        if (!_art_has_lore(art)) continue;
        savefile_write_s16b(file, i);
        wr_xtra_art_aux(file, art);
    }
    savefile_write_s16b(file, -1);
}

static void wr_randomizer(savefile_ptr file)
{
    int i;

    savefile_write_u16b(file, Rand_place);

    for (i = 0; i < RAND_DEG; i++)
        savefile_write_u32b(file, Rand_state[i]);
}

static void wr_options(savefile_ptr file)
{
    int i;
    u16b c;

    savefile_write_s16b(file, delay_animation);
    savefile_write_s16b(file, delay_run);
    savefile_write_byte(file, hitpoint_warn);
    savefile_write_byte(file, mana_warn);
    savefile_write_byte(file, random_artifact_pct);

    /*** Cheating options ***/
    c = 0;
    if (p_ptr->wizard) c |= 0x0002;
    savefile_write_u16b(file, c);

    /* Autosave info */
    savefile_write_byte(file, autosave_l);
    savefile_write_byte(file, autosave_t);
    savefile_write_s16b(file, autosave_freq);

    /*** Extract options ***/
    for (i = 0; option_info[i].o_desc; i++)
    {
        int os = option_info[i].o_set;
        int ob = option_info[i].o_bit;

        if (option_info[i].o_var)
        {
            if (*option_info[i].o_var)
                option_flag[os] |= (1L << ob);
            else
                option_flag[os] &= ~(1L << ob);
        }
    }

    for (i = 0; i < 8; i++) savefile_write_u32b(file, option_flag[i]);
    for (i = 0; i < 8; i++) savefile_write_u32b(file, option_mask[i]);


    /*** Window options ***/
    for (i = 0; i < 8; i++) savefile_write_u32b(file, window_flag[i]);
    for (i = 0; i < 8; i++) savefile_write_u32b(file, window_mask[i]);
}


static void wr_quick_start(savefile_ptr file)
{
    int i;

    savefile_write_byte(file, previous_char.game_mode);
    savefile_write_byte(file, previous_char.psex);
    savefile_write_s16b(file, previous_char.prace);
    savefile_write_byte(file, previous_char.psubrace);
    savefile_write_byte(file, previous_char.pclass);
    savefile_write_byte(file, previous_char.psubclass);
    savefile_write_byte(file, previous_char.personality);
    savefile_write_byte(file, previous_char.realm1);
    savefile_write_byte(file, previous_char.realm2);
    savefile_write_byte(file, previous_char.dragon_realm);
    savefile_write_s32b(file, previous_char.au);

    for (i = 0; i < 6; i++)
        savefile_write_s16b(file, previous_char.stat_max[i]);

    savefile_write_byte(file, previous_char.quick_ok);
}

static void wr_extra(savefile_ptr file)
{
    int i,j;

    savefile_write_s32b(file, p_ptr->id);
    savefile_write_cptr(file, player_name);
    savefile_write_cptr(file, p_ptr->died_from);
    savefile_write_cptr(file, p_ptr->last_message ? p_ptr->last_message : "");
    wr_quick_start(file);

    savefile_write_s32b(file, game_mode);
    savefile_write_s16b(file, p_ptr->prace);
    savefile_write_byte(file, p_ptr->pclass);
    savefile_write_byte(file, p_ptr->personality);
    savefile_write_byte(file, p_ptr->psex);
    savefile_write_byte(file, p_ptr->realm1);
    savefile_write_byte(file, p_ptr->realm2);
    savefile_write_byte(file, p_ptr->dragon_realm);
    savefile_write_byte(file, p_ptr->psubclass);
    savefile_write_byte(file, p_ptr->psubrace);
    savefile_write_s16b(file, p_ptr->current_r_idx);
    savefile_write_u16b(file, p_ptr->expfact);

    for (i = 0; i < 6; ++i) savefile_write_s16b(file, p_ptr->stat_max[i]);
    for (i = 0; i < 6; ++i) savefile_write_s16b(file, p_ptr->stat_max_max[i]);
    for (i = 0; i < 6; ++i) savefile_write_s16b(file, p_ptr->stat_cur[i]);

    savefile_write_u32b(file, p_ptr->au);
    savefile_write_s16b(file, p_ptr->fame);
    savefile_write_u32b(file, p_ptr->max_exp);
    savefile_write_u32b(file, p_ptr->max_max_exp);
    savefile_write_u32b(file, p_ptr->exp);
    savefile_write_u32b(file, p_ptr->exp_frac);
    savefile_write_s16b(file, p_ptr->lev);

    for (i = 0; i < 64; i++) savefile_write_s16b(file, p_ptr->spell_exp[i]);
    for (i = 0; i < 5; i++) for (j = 0; j < 64; j++) savefile_write_s16b(file, p_ptr->weapon_exp[i][j]);
    for (i = 0; i < 10; i++) savefile_write_s16b(file, p_ptr->skill_exp[i]);
    for (i = 0; i < MAX_MAGIC_NUM; i++) savefile_write_s32b(file, p_ptr->magic_num1[i]);
    for (i = 0; i < MAX_MAGIC_NUM; i++) savefile_write_byte(file, p_ptr->magic_num2[i]);

    savefile_write_s16b(file, p_ptr->start_race);
    savefile_write_s32b(file, p_ptr->old_race1);
    savefile_write_s32b(file, p_ptr->old_race2);
    savefile_write_s16b(file, p_ptr->old_realm);

    for (i = 0; i < MAX_KUBI; i++)
        savefile_write_s16b(file, kubi_r_idx[i]);

    savefile_write_s32b(file, p_ptr->mmhp);
    savefile_write_s32b(file, p_ptr->mhp);
    savefile_write_s32b(file, p_ptr->chp);
    savefile_write_u32b(file, p_ptr->chp_frac);
    savefile_write_s32b(file, p_ptr->msp);
    savefile_write_s32b(file, p_ptr->csp);
    savefile_write_u32b(file, p_ptr->csp_frac);
    savefile_write_s16b(file, p_ptr->clp);
    savefile_write_s16b(file, p_ptr->max_plv);

    savefile_write_s16b(file, p_ptr->concent);
    savefile_write_s16b(file, p_ptr->food);
    savefile_write_s16b(file, p_ptr->energy_need);
    plr_tim_save(file);
    savefile_write_s16b(file, p_ptr->afraid);
    savefile_write_s16b(file, p_ptr->see_infra);
    savefile_write_s16b(file, p_ptr->mimic_form);
    savefile_write_s16b(file, p_ptr->tim_mimic);
    /* Remember the Monkey Clone */
    {
        int i;
        monster_race *r_ptr = &r_info[MON_MONKEY_CLONE];
        savefile_write_byte(file, r_ptr->cur_num);
        if (r_ptr->cur_num)
        {
            savefile_write_byte(file, r_ptr->hdice); /* Probably not required ... */
            savefile_write_byte(file, r_ptr->hside); /* Probably not required ... */
            savefile_write_s16b(file, r_ptr->ac);
            savefile_write_byte(file, r_ptr->speed); /* Probably not required ... */
            savefile_write_byte(file, vec_length(r_ptr->blows));
            for (i = 0; i < vec_length(r_ptr->blows); i++)
            {
                mon_blow_ptr blow = vec_get(r_ptr->blows, i);
                savefile_write_byte(file, blow->method);
                savefile_write_s16b(file, blow->blows);
                assert(blow->effect_ct);
                savefile_write_s16b(file, blow->effects[0].effect); /* RBE_HURT = 5000 */
                savefile_write_s16b(file, blow->effects[0].dice.dd);
                savefile_write_s16b(file, blow->effects[0].dice.ds);
                savefile_write_s16b(file, blow->effects[0].dice.base);
            }
            savefile_write_u32b(file, r_ptr->flags3);
            savefile_write_u32b(file, r_ptr->flagsr);
            savefile_write_u32b(file, r_ptr->flags2);
            savefile_write_u32b(file, r_ptr->flags7);
        }
    }
    savefile_write_s16b(file, p_ptr->entrench_x);
    savefile_write_s16b(file, p_ptr->entrench_y);
    savefile_write_s16b(file, p_ptr->entrench_ct);
    savefile_write_byte(file, p_ptr->sense_artifact);
    savefile_write_s16b(file, p_ptr->duelist_target_idx);
    savefile_write_s16b(file, p_ptr->free_turns);

    savefile_write_s16b(file, p_ptr->chaos_patron);
    for (i = 0; i < MUT_FLAG_SIZE; ++i)
        savefile_write_u32b(file, p_ptr->muta[i]);
    for (i = 0; i < MUT_FLAG_SIZE; ++i)
        savefile_write_u32b(file, p_ptr->muta_lock[i]);
    for (i = 0; i < MAX_DEMIGOD_POWERS; ++i)
        savefile_write_s16b(file, p_ptr->demigod_power[i]);
    savefile_write_s16b(file, p_ptr->draconian_power);

    for (i = 0; i<8; i++)
        savefile_write_s16b(file, p_ptr->virtues[i]);
    for (i = 0; i<8; i++)
        savefile_write_s16b(file, p_ptr->vir_types[i]);

    savefile_write_u32b(file, p_ptr->special_attack);
    savefile_write_u32b(file, p_ptr->special_defense);
    savefile_write_byte(file, p_ptr->knowledge);
    savefile_write_byte(file, p_ptr->autopick_autoregister);
    savefile_write_byte(file, p_ptr->action);
    savefile_write_u32b(file, seed_flavor);
    savefile_write_u16b(file, p_ptr->panic_save);
    savefile_write_u16b(file, p_ptr->total_winner);
    savefile_write_u16b(file, p_ptr->noscore);
    savefile_write_byte(file, p_ptr->is_dead);
    savefile_write_s16b(file, today_mon);
    savefile_write_s16b(file, p_ptr->today_mon);
    savefile_write_u16b(file, p_ptr->riding);
    savefile_write_u32b(file, playtime);
    savefile_write_u32b(file, p_ptr->count);

    plr_hook_save(file);
}

static bool wr_savefile_new(savefile_ptr file)
{
    int        i;

    u32b              now;
    u16b            tmp16u;

    now = (u32b)time(0);
    sf_system = 0L;
    sf_when = now;
    sf_saves++;

    /*** Actually write the file ***/
    savefile_write_u32b(file, sf_system);
    savefile_write_u32b(file, sf_when);
    savefile_write_u16b(file, sf_lives);
    savefile_write_u16b(file, sf_saves);

    wr_randomizer(file);
    wr_options(file);

    msg_on_save(file);

    wr_r_info(file);

    tmp16u = max_k_idx;
    savefile_write_u16b(file, tmp16u);
    for (i = 0; i < tmp16u; i++) wr_xtra_kind(file, i);

    wr_xtra_ego(file);
    wr_xtra_art(file);

    quests_save(file);

    tmp16u = max_a_idx;
    savefile_write_u16b(file, tmp16u);
    for (i = 0; i < tmp16u; i++)
    {
        artifact_type *a_ptr = &a_info[i];
        savefile_write_byte(file, a_ptr->generated);
        savefile_write_byte(file, a_ptr->found);
    }

    wr_extra(file);

    tmp16u = PY_MAX_LEVEL;
    savefile_write_u16b(file, tmp16u);
    for (i = 0; i < tmp16u; i++)
        savefile_write_s16b(file, p_ptr->player_hp[i]);

    savefile_write_u32b(file, p_ptr->spell_learned1);
    savefile_write_u32b(file, p_ptr->spell_learned2);
    savefile_write_u32b(file, p_ptr->spell_worked1);
    savefile_write_u32b(file, p_ptr->spell_worked2);
    savefile_write_u32b(file, p_ptr->spell_forgotten1);
    savefile_write_u32b(file, p_ptr->spell_forgotten2);
    savefile_write_s16b(file, p_ptr->learned_spells);
    savefile_write_s16b(file, p_ptr->add_spells);

    for (i = 0; i < 64; i++)
        savefile_write_byte(file, p_ptr->spell_order[i]);

    art_save(file);
    equip_save(file);
    pack_save(file);
    quiver_save(file);
    towns_save(file);
    home_save(file);

    savefile_write_s16b(file, p_ptr->pet_follow_distance);
    savefile_write_s16b(file, p_ptr->pet_extra_flags);

    spell_stats_on_save(file);
    skills_on_save(file);
    stats_on_save(file);

    if (!p_ptr->is_dead)
    {
        dun_mgr_save(file);
    }
    savefile_write_u32b(file, file->v_check);
    savefile_write_u32b(file, file->x_check);

    if (ferror(file->file) || (fflush(file->file) == EOF)) return FALSE;
    return TRUE;
}


static bool save_player_aux(char *name)
{
    bool ok = FALSE;
    savefile_ptr file = savefile_open_write(name);

    if (file)
    {
        /* Hack: Wiping the monster list clears the current duel! */
        int tmp_ix = p_ptr->duelist_target_idx;

        ok = wr_savefile_new(file);
        if (tmp_ix)
        {
            p_ptr->duelist_target_idx = tmp_ix;
            p_ptr->redraw |= PR_STATUS;
        }

        if (!savefile_close(file)) ok = FALSE;
    }

    /* Remove "broken" files */
    if (!ok)
    {
        safe_setuid_grab();
        fd_kill(name);
        safe_setuid_drop();
    }

    if (!ok) return FALSE;

    counts_write(0, playtime);
    character_saved = TRUE;
    return TRUE;
}



/*
 * Attempt to save the player in a savefile
 */
bool save_player(void)
{
    int             result = FALSE;

    char    safe[1024];


#ifdef SET_UID

# ifdef SECURE

    /* Get "games" permissions */
    beGames();

# endif

#endif


    /* New savefile */
    strcpy(safe, savefile);
    strcat(safe, ".new");

#ifdef VM
    /* Hack -- support "flat directory" usage on VM/ESA */
    strcpy(safe, savefile);
    strcat(safe, "n");
#endif /* VM */

    /* Grab permissions */
    safe_setuid_grab();

    /* Remove it */
    fd_kill(safe);

    /* Drop permissions */
    safe_setuid_drop();

    playtime_update();

    /* Attempt to save the player */
    if (save_player_aux(safe))
    {
        char temp[1024];

        /* Old savefile */
        strcpy(temp, savefile);
        strcat(temp, ".old");

#ifdef VM
        /* Hack -- support "flat directory" usage on VM/ESA */
        strcpy(temp, savefile);
        strcat(temp, "o");
#endif /* VM */

        /* Grab permissions */
        safe_setuid_grab();

        /* Remove it */
        fd_kill(temp);

        /* Preserve old savefile */
        fd_move(savefile, temp);

        /* Activate new savefile */
        fd_move(safe, savefile);

        /* Remove preserved savefile */
        fd_kill(temp);

        /* Drop permissions */
        safe_setuid_drop();

        /* Hack -- Pretend the character was loaded */
        character_loaded = TRUE;

#ifdef VERIFY_SAVEFILE

        /* Lock on savefile */
        strcpy(temp, savefile);
        strcat(temp, ".lok");

        /* Grab permissions */
        safe_setuid_grab();

        /* Remove lock file */
        fd_kill(temp);

        /* Drop permissions */
        safe_setuid_drop();

#endif

        /* Success */
        result = TRUE;
    }


#ifdef SET_UID

# ifdef SECURE

    /* Drop "games" permissions */
    bePlayer();

# endif

#endif

    /* Return the result */
    return (result);
}



/*
 * Attempt to Load a "savefile"
 *
 * Version 2.7.0 introduced a slightly different "savefile" format from
 * older versions, requiring a completely different parsing method.
 *
 * Note that savefiles from 2.7.0 - 2.7.2 are completely obsolete.
 *
 * Pre-2.8.0 savefiles lose some data, see "load2.c" for info.
 *
 * Pre-2.7.0 savefiles lose a lot of things, see "load1.c" for info.
 *
 * On multi-user systems, you may only "read" a savefile if you will be
 * allowed to "write" it later, this prevents painful situations in which
 * the player loads a savefile belonging to someone else, and then is not
 * allowed to save his game when he quits.
 *
 * We return "TRUE" if the savefile was usable, and we set the global
 * flag "character_loaded" if a real, living, character was loaded.
 *
 * Note that we always try to load the "current" savefile, even if
 * there is no such file, so we must check for "empty" savefile names.
 */
bool load_player(void)
{
    int             fd = -1;

    errr    err = 0;

    byte    vvv[4];

#ifdef VERIFY_TIMESTAMP
    struct stat     statbuf;
#endif

    cptr    what = "generic";


    /* Paranoia */
    p_ptr->is_dead = FALSE;


    /* Allow empty savefile name */
    if (!savefile[0]) return (TRUE);


#if !defined(MACINTOSH) && !defined(WINDOWS) && !defined(VM)

    /* XXX XXX XXX Fix this */

    /* Verify the existance of the savefile */
    if (access(savefile, 0) < 0)
    {
        /* Give a message */
        msg_print("Savefile does not exist.");

        msg_print(NULL);

        /* Allow this */
        return (TRUE);
    }

#endif


#ifdef VERIFY_SAVEFILE

    /* Verify savefile usage */
    if (!err)
    {
        FILE *fkk;

        char temp[1024];

        /* Extract name of lock file */
        strcpy(temp, savefile);
        strcat(temp, ".lok");

        /* Check for lock */
        fkk = my_fopen(temp, "r");

        /* Oops, lock exists */
        if (fkk)
        {
            /* Close the file */
            my_fclose(fkk);

            /* Message */
            msg_print("Savefile is currently in use.");

            msg_print(NULL);

            /* Oops */
            return (FALSE);
        }

        /* Create a lock file */
        fkk = my_fopen(temp, "w");

        /* Dump a line of info */
        fprintf(fkk, "Lock file for savefile '%s'\n", savefile);

        /* Close the lock file */
        my_fclose(fkk);
    }

#endif


    /* Okay */
    if (!err)
    {
        /* Open the savefile */
        fd = fd_open(savefile, O_RDONLY);

        /* No file */
        if (fd < 0) err = -1;

        /* Message (below) */
        if (err) what = "Cannot open savefile";

    }

    /* Process file */
    if (!err)
    {

#ifdef VERIFY_TIMESTAMP
        /* Get the timestamp */
        (void)fstat(fd, &statbuf);
#endif

        /* Read the first four bytes */
        if (fd_read(fd, (char*)(vvv), 4)) err = -1;

        /* What */
        if (err) what = "Cannot read savefile";


        /* Close the file */
        (void)fd_close(fd);
    }

    /* Process file */
    if (!err)
    {

        /* Extract version */
        z_major = vvv[0];
        z_minor = vvv[1];
        z_patch = vvv[2];
        sf_extra = vvv[3];


        /* Clear screen */
        Term_clear();

        /* Attempt to load */
        err = rd_savefile_new();

        /* Message (below) */
        if (err) what = "Cannot parse savefile";

    }

#ifdef VERIFY_TIMESTAMP
    /* Verify timestamp */
    if (!err && !arg_wizard)
    {
        /* Hack -- Verify the timestamp */
        if (sf_when > (statbuf.st_ctime + 100) ||
            sf_when < (statbuf.st_ctime - 100))
        {
            /* Message */
            what = "Invalid timestamp";


            /* Oops */
            err = -1;
        }
    }
#endif


    /* Okay */
    if (!err)
    {
        /* Give a conversion warning */
        if ((VER_MAJOR != z_major) ||
            (VER_MINOR != z_minor) ||
            (VER_PATCH != z_patch))
        {
            msg_format("Converted a %d.%d.%d savefile.",
                (z_major > 9) ? z_major-10 : z_major , z_minor, z_patch);
            msg_print(NULL);
        }

        /* Player is dead */
        if (p_ptr->is_dead)
        {
            /* Cheat death */
            if (arg_wizard)
            {
                /* A character was loaded */
                character_loaded = TRUE;

                /* Done */
                return (TRUE);
            }

            /* Player is no longer "dead" */
            p_ptr->is_dead = FALSE;

            /* Count lives */
            sf_lives++;

            /* Done */
            return (TRUE);
        }

        /* A character was loaded */
        character_loaded = TRUE;

        {
            u32b tmp = counts_read(2);
            if (tmp > p_ptr->count)
                p_ptr->count = tmp;
            if (counts_read(0) > playtime || counts_read(1) == playtime)
                counts_write(2, ++p_ptr->count);
            counts_write(1, playtime);
        }

        /* Success */
        return (TRUE);
    }


#ifdef VERIFY_SAVEFILE

    /* Verify savefile usage */
    if (TRUE)
    {
        char temp[1024];

        /* Extract name of lock file */
        strcpy(temp, savefile);
        strcat(temp, ".lok");

        /* Remove lock */
        fd_kill(temp);
    }

#endif


    /* Message */
    msg_format("Error (%s) reading %d.%d.%d savefile.",
           what, (z_major>9) ? z_major - 10 : z_major, z_minor, z_patch);
    msg_print(NULL);

    /* Oops */
    return (FALSE);
}

