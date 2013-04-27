// File: save.c
// Purpose: save the current game into a savefile

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "utumno.h"
// Current savefile
static FILE *fff;
char savefile[1024];


/*
 * These functions place information into a savefile a byte at a time
 */
static void sf_put(byte v)
{
    // Write a character
    putc(v, fff);
}

static void wr_byte(byte v)
{
    sf_put(v);
}

static void wr_u16b(u16b v)
{
    sf_put(v & 0xFF);
    sf_put((v >> 8) & 0xFF);
}

static void wr_s16b(s16b v)
{
    wr_u16b((u16b)v);
}

static void wr_u32b(u32b v)
{
    sf_put(v & 0xFF);
    sf_put((v >> 8) & 0xFF);
    sf_put((v >> 16) & 0xFF);
    sf_put((v >> 24) & 0xFF);
}

static void wr_s32b(s32b v)
{
    wr_u32b((u32b)v);
}

static void wr_string(char *str)
{
    while (*str)
    {
        wr_byte(*str);
        str++;
    }
    wr_byte(*str);
}


/*
 * These functions write info in larger logical records
 */


/*
 * Write an "item" record
 */
static void wr_item(CItem *i_ptr)
{
    wr_s16b(i_ptr->GetKIdx());

    wr_s16b(i_ptr->GetX());
    wr_s16b(i_ptr->GetY());

    wr_s16b(i_ptr->GetPval());

    wr_byte(i_ptr->GetDiscount());
    wr_byte(i_ptr->GetNumber());

    wr_byte(i_ptr->GetName1());
    wr_byte(i_ptr->GetName2());
    wr_s16b(i_ptr->GetTimeout());

    wr_s16b(i_ptr->GetToH());
    wr_s16b(i_ptr->GetToD());
    wr_s16b(i_ptr->GetToA());
    wr_s16b(i_ptr->GetAC());

    wr_byte(i_ptr->GetDD());
    wr_byte(i_ptr->GetDS());

    wr_byte(i_ptr->GetIdent());

    wr_byte(i_ptr->GetMarked());

    wr_byte(i_ptr->GetXtra1());
    wr_byte(i_ptr->GetXtra2());

    /* Save the inscription (if any) */
    if (i_ptr->GetNote()) {
        wr_string(i_ptr->GetNote());
    }
    else {
        wr_string("");
    }
}


/*
 * Write a "monster" record
 */
static void wr_monster(CMonster *m_ptr)
{
    wr_s16b(m_ptr->get_r_idx());
    wr_s16b(m_ptr->GetX());
    wr_s16b(m_ptr->GetY());
    wr_s16b(m_ptr->GetMHP());
    wr_s16b(m_ptr->GetCHP());
    wr_u16b(m_ptr->GetCHPFrac());
    wr_s16b(m_ptr->get_csleep());
    wr_s16b(m_ptr->get_busy());
    wr_s16b(m_ptr->get_fast());
    wr_s16b(m_ptr->get_slow());
    wr_s16b(m_ptr->get_confused());
    wr_s16b(m_ptr->get_afraid());
    wr_s16b(m_ptr->get_stun());
    wr_byte(m_ptr->get_spawned());
    wr_byte(m_ptr->detect);
    wr_byte(0);
    wr_s16b(m_ptr->action);
}


/*
 * Write a "lore" record
 */
static void wr_lore(int r_idx)
{
    CMonsterRace *r_ptr = &r_info[r_idx];

    /* Count sights/deaths/kills */
    wr_s16b(r_ptr->r_sights);
    wr_s16b(r_ptr->r_deaths);
    wr_s16b(r_ptr->r_pkills);
    wr_s16b(r_ptr->r_tkills);

    /* Count wakes and ignores */
    wr_byte(r_ptr->r_wake);
    wr_byte(r_ptr->r_ignore);

    /* Extra stuff */
    wr_byte(r_ptr->r_xtra1);
    wr_byte(r_ptr->r_xtra2);

    /* Count drops */
    wr_byte(r_ptr->r_drop_gold);
    wr_byte(r_ptr->r_drop_item);

    /* Count spells */
    wr_byte(r_ptr->r_cast_inate);
    wr_byte(r_ptr->r_cast_spell);

    /* Count blows of each type */
    wr_byte(r_ptr->r_blows[0]);
    wr_byte(r_ptr->r_blows[1]);
    wr_byte(r_ptr->r_blows[2]);
    wr_byte(r_ptr->r_blows[3]);

    /* Memorize flags */
    wr_u32b(r_ptr->r_flags1);
    wr_u32b(r_ptr->r_flags2);
    wr_u32b(r_ptr->r_flags3);
    wr_u32b(r_ptr->r_flags4);
    wr_u32b(r_ptr->r_flags5);
    wr_u32b(r_ptr->r_flags6);


    /* Monster limit per level */
    wr_byte(r_ptr->max_num);

    /* Later (?) */
    wr_byte(0);
    wr_byte(0);
    wr_byte(0);
}


/*
 * Write an "xtra" record
 */
static void wr_xtra(int k_idx)
{
    byte tmp8u = 0;

    CObjectKind *k_ptr = &k_info[k_idx];

    if (k_ptr->aware) tmp8u |= 0x01;
    if (k_ptr->tried) tmp8u |= 0x02;

    wr_byte(tmp8u);
}


/*
 * Write a "store" record
 */
static void wr_store(store_type *st_ptr)
{
    int j;

    /* Save the "open" counter */
    wr_u32b(st_ptr->store_open);

    /* Save the "insults" */
    wr_s16b(st_ptr->insult_cur);

    /* Save the current owner */
    wr_byte(st_ptr->owner);

    /* Save the stock size */
    wr_byte(st_ptr->stock_num);

    /* Save the "haggle" info */
    wr_s16b(st_ptr->good_buy);
    wr_s16b(st_ptr->bad_buy);

    /* Save the stock */
    for (j = 0; j < st_ptr->stock_num; j++)
    {
        /* Save each item in stock */
        wr_item(&st_ptr->stock[j]);
    }
}


/*
 * Write some "extra" info
 */
static void wr_extra()
{
    int i;

    /* Race/Class/Gender/Experience Factor */
    wr_byte(p_ptr->GetRace());
    wr_byte(p_ptr->GetClass());
    wr_byte(p_ptr->GetMale());

    wr_string(player_name);

    wr_string(died_from);

    /* Dump the stats (maximum and current) */
    for (i = 0; i < 6; i++) wr_s16b(p_ptr->GetStatMax(i));
    for (i = 0; i < 6; i++) wr_s16b(p_ptr->GetStatCur(i));

    wr_u32b(p_ptr->GetGold());

    wr_u32b(p_ptr->GetMaxExp());
    wr_u32b(p_ptr->GetExp());
    wr_u16b(p_ptr->GetExpFrac());

    wr_s16b(p_ptr->GetLev());

    wr_s16b(p_ptr->GetMHP());
    wr_s16b(p_ptr->GetCHP());
    wr_u16b(p_ptr->GetCHPFrac());

    wr_s16b(p_ptr->GetMSP());
    wr_s16b(p_ptr->GetCSP());
    wr_u16b(p_ptr->GetCSPFrac());

    /* Max Player and Dungeon Levels */
    wr_s16b(p_ptr->GetMaxPlv());
    wr_s16b(p_ptr->GetMaxDlv());

    wr_s16b(p_ptr->GetBlind());
    wr_s16b(p_ptr->GetParalyzed());
    wr_s16b(p_ptr->GetConfused());
    wr_s16b(p_ptr->GetFood());
    wr_s16b(p_ptr->GetBusy());
    wr_s16b(p_ptr->GetFast());
    wr_s16b(p_ptr->GetSlow());
    wr_s16b(p_ptr->GetAfraid());
    wr_s16b(p_ptr->GetCut());
    wr_s16b(p_ptr->GetStun());
    wr_s16b(p_ptr->GetPoisoned());
    wr_s16b(p_ptr->GetProtevil());
    wr_s16b(p_ptr->GetShadowform());
    wr_s16b(p_ptr->GetHero());
    wr_s16b(p_ptr->GetSHero());
    wr_s16b(p_ptr->GetShield());
    wr_s16b(p_ptr->GetBlessed());
    wr_s16b(p_ptr->GetTimInvis());
    wr_s16b(p_ptr->GetWordRecall());
    wr_s16b(p_ptr->get_see_infra());
    wr_s16b(p_ptr->GetTimInfra());
    wr_s16b(p_ptr->GetOpposeFire());
    wr_s16b(p_ptr->GetOpposeCold());
    wr_s16b(p_ptr->GetOpposeAcid());
    wr_s16b(p_ptr->GetOpposeElec());
    wr_s16b(p_ptr->GetOpposePois());

    wr_byte(p_ptr->GetConfusing());


    // Write the "object seeds"
    wr_u32b(seed_flavor);
    wr_u32b(seed_town);


    /* Special stuff */
    wr_u16b(panic_save);
    wr_u16b(total_winner);
    wr_byte(p_ptr->GetNoScore());
    wr_s16b(p_ptr->action);
    wr_s16b(p_ptr->last_move);


    /* Write death */
    wr_byte(death);

    /* Current turn */
    wr_s32b(game_turn);
}



/*
 * Write an arrow to the savefile
 */
void CArrow::Write(void)
{
    fwrite(&x, sizeof(x), 1, fff);
    fwrite(&y, sizeof(y), 1, fff);
    fwrite(&z, sizeof(z), 1, fff);
    fwrite(&vx, sizeof(vx), 1, fff);
    fwrite(&vy, sizeof(vy), 1, fff);
    fwrite(&vz, sizeof(vz), 1, fff);
    wr_s16b(chance);
    wr_s16b(damage);
    wr_byte(purge);
    if (who == p_ptr) wr_byte(0);
    else {
        wr_byte(1);
        wr_s16b(who->GetX());
        wr_s16b(who->GetY());
    }
    wr_item(i_ptr);
}


/*
 * Write the current dungeon
 *
 * XXX XXX XXX Mega-Hack -- convert new "terrain feature" info back
 * into standard Angband 2.7.9 savefile format using "fake" objects,
 * so that I can use the new "terrain features" even though the new
 * savefile format is not ready yet.
 */
static void wr_dungeon()
{
    int x, y;
    CGrid *g_ptr;


    // Dungeon specific info follows
    wr_u16b(dun_level);
    wr_u16b(num_repro);
    wr_s16b(p_ptr->GetX());
    wr_s16b(p_ptr->GetY());
    wr_u16b(cur_hgt);
    wr_u16b(cur_wid);


    /* Dump the cave */
    for (x = 0; x < cur_wid; x++) {
        for (y = 0; y < cur_hgt; y++) {
            /* Get the cave */
            g_ptr = &cave[y][x];

            /* Paranoia */
            if (g_ptr->i_ptr) {
                g_ptr->i_ptr->SetLocation(x, y);
            }

            /* Paranoia */
            if (g_ptr->m_ptr) {
                g_ptr->m_ptr->SetLocation(x, y);
            }

            /* Write features/flags */
            wr_s16b(g_ptr->get_feat());
            wr_u16b(g_ptr->flags);
            wr_byte(g_ptr->variant);
        }
    }


    // Dump the items
    for (x = 0; x < cur_wid; x++) {
        for (y = 0; y < cur_hgt; y++) {
            CItem *i_ptr = cave[y][x].i_ptr;
            if (!i_ptr) continue;
            wr_byte(1);
            wr_item(i_ptr);
            i_ptr = i_ptr->next_i_ptr;
            while (i_ptr) {
                wr_byte(2);
                wr_item(i_ptr);
                i_ptr = i_ptr->next_i_ptr;
            }
        }
    }
    wr_byte(0);


    // Dump the "real" monsters
    for (x = 0; x < cur_wid; x++) {
        for (y = 0; y < cur_hgt; y++) {
            CMonster *m_ptr = cave[y][x].m_ptr;
            if (!m_ptr) continue;
            wr_byte(1);
            wr_monster(m_ptr);
            CItem *i_ptr = m_ptr->i_ptr;
            while (i_ptr) {
                wr_byte(2);
                wr_item(i_ptr);
                i_ptr = i_ptr->next_i_ptr;
            }
        }
    }
    wr_byte(0);

    
    // Dump the projectiles
    for (int i = 0; i < MAX_PROJECTILES; i++) {
        // No arrow, or arrow to be purged
        if (!arrows[i]) continue;
        if (arrows[i]->ToBePurged()) continue;

        // Write the data out
        wr_byte(1);
        arrows[i]->Write();
    }
    wr_byte(0);
}



/*
 * Actually write a save-file
 */
static bool wr_savefile_new(void)
{
    int i;
    u16b tmp16u;


    /*** Actually write the file ***/

    // Dump the file header
    wr_byte(VERSION_MAJOR);
    wr_byte(VERSION_MINOR);
    wr_byte(VERSION_PATCH);
    wr_byte(VERSION_TYPE);


    // Write the "extra" information
    wr_extra();


    // Wizard mode
    wr_byte(wizard);


    // Dump the monster lore
    tmp16u = MAX_R_IDX;
    wr_u16b(tmp16u);
    for (i = 0; i < tmp16u; i++) wr_lore(i);


    // Dump the object memory
    tmp16u = MAX_K_IDX;
    wr_u16b(tmp16u);
    for (i = 0; i < tmp16u; i++) wr_xtra(i);


    // Hack -- Dump the quests
    tmp16u = MAX_Q_IDX;
    wr_u16b(tmp16u);
    for (i = 0; i < tmp16u; i++) {
        wr_byte(q_list[i].level);
        wr_byte(0);
        wr_byte(0);
        wr_byte(0);
    }

    /* Hack -- Dump the artifacts */
    tmp16u = MAX_A_IDX;
    wr_u16b(tmp16u);
    for (i = 0; i < tmp16u; i++) {
        artifact_type *a_ptr = &a_info[i];
        wr_byte(a_ptr->cur_num);
        wr_byte(0);
        wr_byte(0);
        wr_byte(0);
    }



    // Dump the "player hp" entries
    tmp16u = PY_MAX_LEVEL;
    wr_u16b(tmp16u);
    for (i = 0; i < tmp16u; i++) {
        wr_s16b(p_ptr->player_hp[i]);
    }


    // Write spell data
    for (i = 0; i < 64; i++) {
        wr_byte(spell_learned[i]);
        wr_byte(spell_worked[i]);
        wr_byte(spell_forgotten[i]);
    }

    // Dump the ordered spells
    for (i = 0; i < 64; i++) {
        wr_byte(spell_order[i]);
    }


    /* Write the inventory */
    for (i = 0; i < INVEN_TOTAL; i++) {
        if (inventory[i].exists()) {
            wr_u16b(i);
            wr_item(&inventory[i]);
        }
    }

    /* Add a sentinel */
    wr_u16b(0xFFFF);


    /* Note the stores */
    tmp16u = MAX_STORES;
    wr_u16b(tmp16u);

    /* Dump the stores */
    for (i = 0; i < tmp16u; i++) wr_store(&store[i]);


    /* Player is not dead, write the dungeon */
    if (!death) {
        /* Dump the dungeon */
        wr_dungeon();
    }


    /* Error in save */
    if (ferror(fff) || (fflush(fff) == EOF)) return FALSE;

    /* Successful save */
    return TRUE;
}


/*
 * Medium level player saver
 *
 * XXX XXX XXX Angband 2.8.0 will use "fd" instead of "fff" if possible
 */
static bool save_player_aux(char *name)
{
    bool ok = FALSE;
    int fd = -1;
    int mode = 0644;


    /* No file yet */
    fff = NULL;


    /* Create the savefile */
    fd = fd_make(name, mode);

    /* File is okay */
    if (fd >= 0) {
        /* Close the "fd" */
        (void)fd_close(fd);

        /* Open the savefile */
        fff = my_fopen(name, "wb");

        /* Successful open */
        if (fff) {
            /* Write the savefile */
            if (wr_savefile_new()) ok = TRUE;

            /* Attempt to close it */
            if (fclose(fff) == EOF) ok = FALSE;
        }

        /* Remove "broken" files */
        if (!ok) remove(name);
    }


    /* Failure */
    if (!ok) return (FALSE);

    /* Successful save */
    character_saved = TRUE;

    /* Success */
    return (TRUE);
}



/*
 * Attempt to save the player in a savefile
 */
bool save_player()
{
    int result = FALSE;
    char safe[1024];


    // Savefile name
    sprintf(savefile, "dat/save/save%d", char_idx);

    // New savefile
    sprintf(safe, "dat/save/save%d.new", char_idx);

    // Remove it
    fd_kill(safe);

    // Attempt to save the player
    if (save_player_aux(safe)) {
        char temp[1024];

        // Old savefile
        strcpy(temp, savefile);
        strcat(temp, ".old");

        // Remove it
        fd_kill(temp);

        /* Preserve old savefile */
        fd_move(savefile, temp);

        /* Activate new savefile */
        fd_move(safe, savefile);

        /* Remove preserved savefile */
        fd_kill(temp);

        /* Hack -- Pretend the character was loaded */
        character_loaded = TRUE;

        /* Success */
        result = TRUE;
    }


    // Return the result
    return result;
}



/*
 * Attempt to Load a "savefile"
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
    int fd = -1;
    errr err = 0;
    byte vvv[4];
    char *what = "generic";


    sprintf(savefile, "dat/save/save%d", char_idx);

    // Paranoia
    game_turn = 0;

    // Paranoia
    death = FALSE;


    // Okay
    if (!err) {
        /* Open the savefile */
        fd = fd_open(savefile, O_RDONLY);

        /* No file */
        if (fd < 0) err = -1;

        /* Message (below) */
        if (err) what = "Cannot open savefile";
    }

    /* Process file */
    if (!err) {
        /* Read the first four bytes */
        if (read(fd, (char *)(vvv), 4) != 4) err = -1;

        /* What */
        if (err) what = "Cannot read savefile";

        /* Close the file */
        (void)fd_close(fd);
    }

    /* Process file */
    if (!err) {
        /* Extract version */
        sf_major = vvv[0];
        sf_minor = vvv[1];
        sf_patch = vvv[2];
        sf_extra = vvv[3];

        // Clear screen
        blank_screen(COLOR_BLACK);

        /* Check version */
        if ((sf_major != VERSION_MAJOR) ||
            (sf_minor != VERSION_MINOR) ||
            (sf_patch != VERSION_PATCH) ||
            (sf_extra != VERSION_TYPE)) {
            err = -1;
        }

        /* Parse modern savefiles */
        else {
            /* Attempt to load */
            err = rd_savefile_new();
        }

        /* Message (below) */
        if (err) what = "Cannot parse savefile";
    }

    /* Paranoia */
    if (!err) {
        /* Invalid turn */
        if (!game_turn) err = -1;

        /* Message (below) */
        if (err) what = "Broken savefile";
    }

    /* Okay */
    if (!err) {
        /* Player is dead */
        if (death) {
            /* Player is no longer "dead" */
            death = FALSE;

            /* Forget the turn */
            game_turn = 0;

            /* Done */
            return (TRUE);
        }

        /* A character was loaded */
        character_loaded = TRUE;

        /* Still alive */
        if (p_ptr->GetCHP() >= 0) {
            /* Reset cause of death */
            strcpy(died_from, "(alive and well)");
        }

        /* Success */
        return TRUE;
    }


    /* Message */
    if (sf_extra == 0) {
        mini_message_box("Error", format("Error (%s) reading v%d.%da%d savefile.", what,
            sf_major, sf_minor, sf_patch));
    }
    else if (sf_extra == 1) {
        mini_message_box("Error", format("Error (%s) reading v%d.%db%d savefile.", what,
            sf_major, sf_minor, sf_patch));
    }
    else {
        mini_message_box("Error", format("Error (%s) reading v%d.%d savefile.", what,
            sf_major, sf_minor));
    }

    /* Oops */
    return FALSE;
}


