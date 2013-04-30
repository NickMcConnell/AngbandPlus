/*
 * File: savefile.c
 * Purpose: Savefile loading and saving main routines
 *
 * Copyright (c) 2009 Andi Sidwell <andi@takkaria.org>
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


#include "s-angband.h"
#include "../common/md5.h"
#include "files.h"
#include "netserver.h"
#include "savefile.h"


/*
 * The savefile code.
 *
 * Savefiles since ~3.1 have used a block-based system.  Each savefile
 * consists of an 8-byte header, the first four bytes of which mark this
 * as a savefile, the second four bytes provide a variant ID.
 *
 * After that, each block has the format:
 * - 16-byte string giving the type of block
 * - 4-byte block version
 * - 4-byte block size
 * - 4-byte block checksum
 * ... data ...
 * padding so that block is a multiple of 4 bytes
 *
 * The savefile doesn't contain the version number of that game that saved it;
 * versioning is left at the individual block level.  The current code
 * keeps a list of savefile blocks to save in savers[] below, along with
 * their current versions.
 *
 * For each block type and version, there is a loading function to load that
 * type/version combination.  For example, there may be a loader for v1
 * and v2 of the RNG block; these must be different functions.  It has been
 * done this way since it allows easier maintenance; after each release, you
 * need simply remove old loaders and you will not have to disentangle
 * lots of code with "if (version > 3)" and its like everywhere.
 *
 * Savefile loading and saving is done by keeping the current block in
 * memory, which is accessed using the wr_* and rd_* functions.  This is
 * then written out, whole, to disk, with the appropriate header.
 *
 *
 * So, if you want to make a savefile compat-breaking change, then there are
 * a few things you should do:
 *
 * - increment the version in 'savers' below
 * - add a loading function that accepts the new version (in addition to
 *   the previous loading function) to 'loaders'
 * - and watch the magic happen.
 */


/* Magic bits at beginning of savefile */
static const byte savefile_magic[4] = {1, 1, 9, 6};
static const byte savefile_name[4] = "PWMG";


/*
 * Savefile saver
 */
typedef struct
{
    char name[16];
    void (*save)(int);
    u32b version;
} savefile_saver;


/*
 * Savefile saving functions (player)
 */
static const savefile_saver player_savers[] =
{
    /* Hack - Save basic player info */
    {"header", wr_header, 1},

    {"monster memory", wr_monster_memory, 1},
    {"object memory", wr_object_memory, 1},
    {"artifacts", wr_player_artifacts, 1},
    {"player", wr_player, 1},
    {"misc", wr_player_misc, 1},
    {"player hp", wr_player_hp, 1},
    {"player spells", wr_player_spells, 1},
    {"inventory", wr_inventory, 1},
    {"dungeon", wr_player_dungeon, 1},
    {"history", wr_history, 1},

    /* PWMAngband */
    {"wild map", wr_wild_map, 1}
};


/*
 * Savefile saving functions (server)
 */
static const savefile_saver server_savers[] =
{
    {"monster memory", wr_monster_memory, 1},
    {"artifacts", wr_artifacts, 1},
    {"misc", wr_misc, 1},
    {"stores", wr_stores, 1},
    {"dungeons", wr_dungeon, 1},
    {"objects", wr_objects, 1},
    {"monsters", wr_monsters, 1},

    /* PWMAngband */
    {"parties", wr_parties, 1},
    {"houses", wr_houses, 1},
    {"arenas", wr_arenas, 1},
    {"wilderness", wr_wilderness, 1},
    {"player_names", wr_player_names, 1}
};


/* Hack */
static const savefile_saver special_savers[] =
{
    {"dungeon", wr_depth_dungeon, 1}
};


/*
 * Savefile loader
 */
typedef struct
{
    char name[16];
    int (*load)(struct player *);
    u32b version;
} savefile_loader;


/*
 * Savefile loading functions (player)
 */
static const savefile_loader player_loaders[] =
{
    /* Hack - Save basic player info */
    {"header", rd_header, 1},

    {"monster memory", rd_monster_memory, 1},
    {"object memory", rd_object_memory, 1},
    {"artifacts", rd_player_artifacts, 1},
    {"player", rd_player, 1},
    {"misc", rd_player_misc, 1},
    {"player hp", rd_player_hp, 1},
    {"player spells", rd_player_spells, 1},
    {"inventory", rd_inventory, 1},
    {"dungeon", rd_player_dungeon, 1},
    {"history", rd_history, 1},

    /* PWMAngband */
    {"wild map", rd_wild_map, 1}
};


/*
 * Savefile loading functions (server)
 */
static const savefile_loader server_loaders[] =
{
    {"monster memory", rd_monster_memory, 1},
    {"artifacts", rd_artifacts, 1},
    {"misc", rd_misc, 1},
    {"stores", rd_stores, 1},
    {"dungeons", rd_dungeon, 1},
    {"objects", rd_objects, 1},
    {"monsters", rd_monsters, 1},

    /* PWMAngband */
    {"parties", rd_parties, 1},
    {"houses", rd_houses, 1},
    {"arenas", rd_arenas, 1},
    {"wilderness", rd_wilderness, 1},
    {"player_names", rd_player_names, 1}
};


/* Hack */
static const savefile_loader special_loaders[] =
{
    {"dungeon", rd_depth_dungeon, 1}
};
static bool load_dungeon_special(void);


/* Buffer bits */
static byte *buffer;
static u32b buffer_size;
static u32b buffer_pos;
static u32b buffer_check;


#define BUFFER_INITIAL_SIZE     1024
#define BUFFER_BLOCK_INCREMENT  1024
#define SAVEFILE_HEAD_SIZE      28


/** Base put/get **/


static void sf_put(byte v)
{
    my_assert(buffer != NULL);
    my_assert(buffer_size > 0);

    if (buffer_size == buffer_pos)
    {
        buffer_size += BUFFER_BLOCK_INCREMENT;
        buffer = mem_realloc(buffer, buffer_size);
    }

    my_assert(buffer_pos < buffer_size);
    buffer[buffer_pos++] = v;
    buffer_check += v;
}


static byte sf_get(void)
{
    my_assert(buffer != NULL);
    my_assert(buffer_size > 0);
    my_assert(buffer_pos < buffer_size);

    buffer_check += buffer[buffer_pos];

    return buffer[buffer_pos++];
}


/** Writing bits **/


void wr_byte(byte v)
{
    sf_put(v);
}


void wr_u16b(u16b v)
{
    sf_put((byte)(v & 0xFF));
    sf_put((byte)((v >> 8) & 0xFF));
}


void wr_s16b(s16b v)
{
    wr_u16b((u16b)v);
}


void wr_u32b(u32b v)
{
    sf_put((byte)(v & 0xFF));
    sf_put((byte)((v >> 8) & 0xFF));
    sf_put((byte)((v >> 16) & 0xFF));
    sf_put((byte)((v >> 24) & 0xFF));
}


void wr_s32b(s32b v)
{
    wr_u32b((u32b)v);
}


void wr_hturn(hturn* pv)
{
    wr_u32b(pv->era);
    wr_u32b(pv->turn);
}


void wr_string(const char *str)
{
    while (*str)
    {
        wr_byte(*str);
        str++;
    }
    wr_byte(*str);
}


/** Reading bits **/


void rd_byte(byte *ip)
{
    *ip = sf_get();
}


void rd_bool(bool *ip)
{
    byte tmp8u;

    rd_byte(&tmp8u);
    *ip = tmp8u;
}


void rd_u16b(u16b *ip)
{
    (*ip) = sf_get();
    (*ip) |= ((u16b)(sf_get()) << 8);
}


void rd_s16b(s16b *ip)
{
    rd_u16b((u16b*)ip);
}


void rd_u32b(u32b *ip)
{
    (*ip) = sf_get();
    (*ip) |= ((u32b)(sf_get()) << 8);
    (*ip) |= ((u32b)(sf_get()) << 16);
    (*ip) |= ((u32b)(sf_get()) << 24);
}


void rd_s32b(s32b *ip)
{
    rd_u32b((u32b*)ip);
}


void rd_hturn(hturn *ip)
{
    u32b scan_era, scan_turn;

    rd_u32b(&scan_era);
    rd_u32b(&scan_turn);

    ht_reset(ip);
    ip->era = scan_era;
    ht_add(ip, scan_turn);
}


void rd_string(char *str, int max)
{
    byte tmp8u;
    int i = 0;

    do
    {
        rd_byte(&tmp8u);

        if (i < max) str[i] = tmp8u;
        if (!tmp8u) break;
    }
    while (++i);

    str[max - 1] = '\0';
}


void strip_bytes(int n)
{
    byte tmp8u;

    while (n--) rd_byte(&tmp8u);
}


void strip_string(int max)
{
    char *dummy;

    dummy = C_ZNEW(max, char);
    rd_string(dummy, max);
    mem_free(dummy);
}


/*** Savefile saving functions ***/


static bool try_save(int Ind, ang_file *file, savefile_saver *savers, size_t n_savers)
{
    byte savefile_head[SAVEFILE_HEAD_SIZE];
    size_t i, pos;

    /* Start off the buffer */
    buffer = mem_alloc(BUFFER_INITIAL_SIZE);
    buffer_size = BUFFER_INITIAL_SIZE;

    for (i = 0; i < n_savers; i++)
    {
        buffer_pos = 0;
        buffer_check = 0;

        savers[i].save(Ind);

        /* 16-byte block name */
        pos = my_strcpy((char *)savefile_head, savers[i].name, sizeof(savefile_head));
        while (pos < 16) savefile_head[pos++] = 0;

#define SAVE_U32B(v) \
        savefile_head[pos++] = (v & 0xFF); \
        savefile_head[pos++] = ((v >> 8) & 0xFF); \
        savefile_head[pos++] = ((v >> 16) & 0xFF); \
        savefile_head[pos++] = ((v >> 24) & 0xFF);

        SAVE_U32B(savers[i].version);
        SAVE_U32B(buffer_pos);
        SAVE_U32B(buffer_check);

        my_assert(pos == SAVEFILE_HEAD_SIZE);

        file_write(file, (char *)savefile_head, SAVEFILE_HEAD_SIZE);
        file_write(file, (char *)buffer, buffer_pos);

        /* Pad to 4 byte multiples */
        if (buffer_pos % 4) file_write(file, "xxx", 4 - (buffer_pos % 4));
    }

    mem_free(buffer);
    buffer = NULL;
    return TRUE;
}


/*
 * Attempt to save the player in a savefile
 */
bool save_player(int Ind)
{
    player_type *p_ptr = player_get(Ind);
    ang_file *file;
    int count = 0;
    char new_savefile[MSG_LEN];
    char old_savefile[MSG_LEN];
    bool character_saved = FALSE;

    /* New savefile */
    strnfmt(old_savefile, sizeof(old_savefile), "%s%u.old", p_ptr->savefile, Rand_simple(1000000));
    while (file_exists(old_savefile) && (count++ < 100))
        strnfmt(old_savefile, sizeof(old_savefile), "%s%u%u.old", p_ptr->savefile, Rand_simple(1000000), count);
    count = 0;

    /* Open the savefile */
    strnfmt(new_savefile, sizeof(new_savefile), "%s%u.new", p_ptr->savefile, Rand_simple(1000000));
    while (file_exists(new_savefile) && (count++ < 100))
        strnfmt(new_savefile, sizeof(new_savefile), "%s%u%u.new", p_ptr->savefile, Rand_simple(1000000), count);
    file = file_open(new_savefile, MODE_WRITE, FTYPE_SAVE);

    if (file)
    {
        file_write(file, (char *)&savefile_magic, 4);
        file_write(file, (char *)&savefile_name, 4);

        character_saved = try_save(Ind, file, (savefile_saver *)player_savers,
            N_ELEMENTS(player_savers));
        file_close(file);
    }

    /* Attempt to save the player */
    if (character_saved)
    {
        bool err = FALSE;

        if (file_exists(p_ptr->savefile) && !file_move(p_ptr->savefile, old_savefile))
            err = TRUE;

        if (!err)
        {
            if (!file_move(new_savefile, p_ptr->savefile)) err = TRUE;

            if (err) file_move(old_savefile, p_ptr->savefile);
            else file_delete(old_savefile);
        }

        return !err;
    }

    /* Delete temp file if the save failed */
    if (file) file_delete(new_savefile);

    return FALSE;
}


/*
 * Save special manually-designed dungeon levels
 */
void save_dungeon_special(int depth)
{
    char filename[MSG_LEN];
    char levelname[32];
    ang_file *file;
    int j = 0, k = 0;

    /* Build a file name */
    strnfmt(levelname, sizeof(levelname), "server.level.%d.%d.%d", k, j, depth);
    path_build(filename, MSG_LEN, ANGBAND_DIR_SAVE, levelname);

    /* Open the savefile */
    file = file_open(filename, MODE_WRITE, FTYPE_RAW);
    if (file)
    {
        /* Save the level */
        plog_fmt("Saving special level for level %d...", depth);
        try_save(depth, file, (savefile_saver *)special_savers, N_ELEMENTS(special_savers));
        file_close(file);
    }
}


/*
 * Save the server state to a "server" savefile.
 */
bool save_server_info(void)
{
    ang_file *file;
    int count = 0;
    char new_savefile[MSG_LEN], new_name[MSG_LEN];
    char old_savefile[MSG_LEN], old_name[MSG_LEN];
    bool server_saved = FALSE;

    /* New savefile */
    strnfmt(old_name, sizeof(old_name), "server%u.old", Rand_simple(1000000));
    path_build(old_savefile, MSG_LEN, ANGBAND_DIR_SAVE, old_name);
    while (file_exists(old_savefile) && (count++ < 100))
    {
        strnfmt(old_name, sizeof(old_name), "server%u%u.old", Rand_simple(1000000), count);
        path_build(old_savefile, MSG_LEN, ANGBAND_DIR_SAVE, old_name);
    }
    count = 0;

    /* Open the savefile */
    strnfmt(new_name, sizeof(new_name), "server%u.new", Rand_simple(1000000));
    path_build(new_savefile, MSG_LEN, ANGBAND_DIR_SAVE, new_name);
    while (file_exists(new_savefile) && (count++ < 100))
    {
        strnfmt(new_name, sizeof(new_name), "server%u%u.new", Rand_simple(1000000), count);
        path_build(new_savefile, MSG_LEN, ANGBAND_DIR_SAVE, new_name);
    }
    file = file_open(new_savefile, MODE_WRITE, FTYPE_SAVE);

    if (file)
    {
        file_write(file, (char *)&savefile_magic, 4);
        file_write(file, (char *)&savefile_name, 4);

        server_saved = try_save(0, file, (savefile_saver *)server_savers,
            N_ELEMENTS(server_savers));
        file_close(file);
    }

    /* Attempt to save the server state */
    if (server_saved)
    {
        char savefile[MSG_LEN];
        bool err = FALSE;

        path_build(savefile, MSG_LEN, ANGBAND_DIR_SAVE, "server");

        if (file_exists(savefile) && !file_move(savefile, old_savefile))
            err = TRUE;

        if (!err)
        {
            if (!file_move(new_savefile, savefile)) err = TRUE;

            if (err) file_move(old_savefile, savefile);
            else file_delete(old_savefile);
        }

        return !err;
    }

    /* Delete temp file if the save failed */
    if (file) file_delete(new_savefile);

    return FALSE;
}


/*** Savefile loading functions ***/


static bool try_load(struct player *p, ang_file *f, savefile_loader *loaders, size_t n_loaders)
{
    byte savefile_head[SAVEFILE_HEAD_SIZE];
    u32b block_version, block_size;
    char *block_name;

    while (TRUE)
    {
        size_t i;
        int (*loader)(struct player *) = NULL;

        /* Load in the next header */
        size_t size = file_read(f, (char *)savefile_head, SAVEFILE_HEAD_SIZE);

        if (!size) break;

        if ((size != SAVEFILE_HEAD_SIZE) || (savefile_head[15] != 0))
        {
            plog("Savefile is corrupted -- block header mangled.");
            return FALSE;
        }

#define RECONSTRUCT_U32B(from) \
        ((u32b)savefile_head[from]) | \
        ((u32b)savefile_head[from + 1] << 8) | \
        ((u32b)savefile_head[from + 2] << 16) | \
        ((u32b)savefile_head[from + 3] << 24);

        block_name = (char *)savefile_head;
        block_version = RECONSTRUCT_U32B(16);
        block_size = RECONSTRUCT_U32B(20);

        /* Pad to 4 bytes */
        if (block_size % 4) block_size += 4 - (block_size % 4);

        /* Find the right loader */
        for (i = 0; i < n_loaders; i++)
        {
            if (streq(block_name, loaders[i].name) && (block_version == loaders[i].version))
                loader = loaders[i].load;
        }

        /* No loader found */
        if (!loader)
        {
            plog("Savefile too old -- no loader found.");
            return FALSE;
        }

        /* Allocate space for the buffer */
        buffer = mem_alloc(block_size);
        buffer_pos = 0;
        buffer_check = 0;

        buffer_size = file_read(f, (char *)buffer, block_size);
        if (buffer_size != block_size)
        {
            plog("Savefile is corrupted -- not enough bytes.");
            mem_free(buffer);
            return FALSE;
        }

        /* Try loading */
        if (loader(p) != 0)
        {
            plog("Savefile is corrupted.");
            mem_free(buffer);
            return FALSE;
        }

        mem_free(buffer);

        /* Hack -- load any special static levels */
        if (streq(block_name, "dungeons"))
        {
            if (!load_dungeon_special()) return FALSE;
        }
    }

    /* Success */
    return TRUE;
}


static int try_scoop(ang_file *f, char *pass_word, byte *pridx, byte *pcidx, byte *psex)
{
    byte savefile_head[SAVEFILE_HEAD_SIZE];
    u32b block_version, block_size;
    char *block_name;
    const char *header = "header";
    char pass[NORMAL_WID];
    char stored_pass[NORMAL_WID];
    char client_pass[NORMAL_WID];
    int err = 0;

    /* Load in the next header */
    size_t size = file_read(f, (char *)savefile_head, SAVEFILE_HEAD_SIZE);

    if (!size) return -1;
    if ((size != SAVEFILE_HEAD_SIZE) || (savefile_head[15] != 0)) return -1;

    /* Determine the block ID */
    if (strncmp((char *)savefile_head, header, sizeof(header)) != 0) return -1;

#define RECONSTRUCT_U32B(from) \
    ((u32b)savefile_head[from]) | \
    ((u32b)savefile_head[from + 1] << 8) | \
    ((u32b)savefile_head[from + 2] << 16) | \
    ((u32b)savefile_head[from + 3] << 24);

    block_name = (char *)savefile_head;
    block_version = RECONSTRUCT_U32B(16);
    block_size = RECONSTRUCT_U32B(20);

    /* Pad to 4 bytes */
    if (block_size % 4) block_size += 4 - (block_size % 4);

    /* Allocate space for the buffer */
    buffer = mem_alloc(block_size);
    buffer_pos = 0;
    buffer_check = 0;

    buffer_size = file_read(f, (char *)buffer, block_size);
    if (buffer_size != block_size)
    {
        mem_free(buffer);
        return -1;
    }

    /* Try to fetch the data */
    strip_string(NORMAL_WID);
    rd_string(pass, NORMAL_WID);
    rd_byte(pridx);
    rd_byte(pcidx);
    rd_byte(psex);

    /* Here's where we do our password encryption handling */
    my_strcpy(stored_pass, (const char *)pass, sizeof(stored_pass));
    MD5Password(stored_pass); /* The hashed version of our stored password */
    my_strcpy(client_pass, (const char *)pass_word, sizeof(client_pass));
    MD5Password(client_pass); /* The hashed version of password from client */

    if (strstr(pass, "$1$"))
    {
        /* Most likely an MD5 hashed password saved */
        if (strcmp(pass, pass_word))
        {
            /* No match, might be clear text from client */
            if (strcmp(pass, client_pass))
            {
                /* No, it's not correct */
                err = -2;
            }

            /* Old style client, but OK otherwise */
        }
    }
    else
    {
        /* Most likely clear text password saved */
        if (strstr(pass_word, "$1$"))
        {
            /* Most likely hashed password from new client */
            if (strcmp(stored_pass, pass_word))
            {
                /* No, it doesn't match hashed */
                err = -2;
            }
        }
        else
        {
            /* Most likely clear text from client as well */
            if (strcmp(pass, pass_word))
            {
                /* No, it's not correct */
                err = -2;
            }
        }

        /* Good match with clear text, save the hashed */
        my_strcpy(pass_word, (const char *)stored_pass, sizeof(pass_word));
    }

    mem_free(buffer);

    /* Result */
    return (err);
}


/*
 * Load a savefile.
 */
bool load_player(struct player *p)
{
    byte head[8];
    bool ok = TRUE;
    ang_file *f = file_open(p->savefile, MODE_READ, FTYPE_RAW);
    char buf[NORMAL_WID];

    if (f)
    {
        if ((file_read(f, (char *)&head, 8) == 8) && (memcmp(&head[0], savefile_magic, 4) == 0) &&
            (memcmp(&head[4], savefile_name, 4) == 0))
        {
            if (!try_load(p, f, (savefile_loader *)player_loaders, N_ELEMENTS(player_loaders)))
            {
                ok = FALSE;
                my_strcpy(buf, "Failed loading savefile.", sizeof(buf));
            }
        }
        else
        {
            ok = FALSE;
            my_strcpy(buf, "Savefile is corrupted or too old -- incorrect file header.", sizeof(buf));
        }

        file_close(f);
    }
    else
    {
        ok = FALSE;
        my_strcpy(buf, "Couldn't open savefile.", sizeof(buf));
    }

    if (!ok)
    {
        plog(buf);
        Destroy_connection(p->conn, buf);
    }

    return ok;
}


/*
 * Similarly to "load_player", reads a part of player savefile and report the results.
 *
 * This is used because we need the password information early on in the connection stage
 * (before the player structure is allocated) and the only way
 * to get it is to read the save file. The file will be read again when it is time
 * to allocate player information and start game play.
 *
 * The actual read is performed by "try_scoop", which is a simplified code
 * duplication from "try_load".
 */
int scoop_player(char *nick, char *pass, byte *pridx, byte *pcidx, byte *psex)
{
    byte head[8];
    int err = 0;
    ang_file *f;
    char tmp[MSG_LEN];

    my_strcpy(tmp, nick, sizeof(tmp));

    if (process_player_name_aux(nick, NULL, tmp, TRUE) < 0)
    {
        /* Error already! */
        plog_fmt("Incorrect player name %s.", nick);
        return -1;
    }

    /* No file */
    if (!file_exists(tmp))
    {
        /* Give a message */
        plog_fmt("Savefile does not exist for player %s.", nick);

        /* Inform caller */
        return 1;
    }

    /* Open savefile */
    f = file_open(tmp, MODE_READ, FTYPE_RAW);
    if (f)
    {
        if ((file_read(f, (char *)&head, 8) == 8) &&
            (memcmp(&head[0], savefile_magic, 4) == 0) &&
            (memcmp(&head[4], savefile_name, 4) == 0))
        {
            err = try_scoop(f, pass, pridx, pcidx, psex);
            if (err == -2) plog("Incorrect password");
            else if (err) plog("Failed loading savefile.");
        }
        else
        {
            err = -1;
            plog("Savefile is corrupted or too old -- incorrect file header.");
        }

        file_close(f);
    }
    else
    {
        err = -1;
        plog("Couldn't open savefile.");
    }

    /* Oops */
    return (err);
}


/*
 * Read special static pre-designed dungeon levels
 *
 * Special pre-designed levels are stored in separate files with
 * the filename "server.level.<wild_x>.<wild_y>.<depth>".
 * Level files are searched for at runtime and loaded if present.
 */
static bool load_dungeon_special(void)
{
    char filename[MSG_LEN];
    char levelname[32];
    ang_file *fhandle;
    int i, num_levels;
    int j = 0, k = 0;

    /* Clear all the special levels */
    for (i = 0; i < MAX_SPECIAL_LEVELS; i++) special_levels[i] = -999;

    /*
     * Special static pre-designed levels are only used on no_recall or
     * more_towns servers
     */
    if (!cfg_no_recall && !cfg_more_towns) return TRUE;

    /* k = E/W, j = N/S for wilderness towns */
    num_levels = 0;
    for (i = 0; i < MAX_DEPTH; i++)
    {
        bool ok;

        /* No special "quest" levels */
        if (is_quest(i)) continue;

        /* Build a file name */
        strnfmt(levelname, sizeof(levelname), "server.level.%d.%d.%d", k, j, i);
        path_build(filename, MSG_LEN, ANGBAND_DIR_SAVE, levelname);

        /* Open the file if it exists */
        fhandle = file_open(filename, MODE_READ, FTYPE_RAW);
        if (fhandle)
        {
            /* Load the level */
            plog_fmt("Loading special level for level %d...", i);
            ok = try_load(NULL, fhandle, (savefile_loader *)special_loaders,
                N_ELEMENTS(special_loaders));

            /* Close the level file */
            file_close(fhandle);

            if (!ok) return FALSE;

            /* Add this depth to the special level list */
            special_levels[num_levels++] = i;

            /* We have an arbitrary max number of levels */
            if (num_levels > MAX_SPECIAL_LEVELS) break;
        }
    }

    /* Success */
    return TRUE;
}


/*
 * Load the server info (artifacts created and uniques killed)
 * from a special savefile.
 */
bool load_server_info(void)
{
    byte head[8];
    bool ok = TRUE;
    ang_file *f;
    char buf[MSG_LEN];

    path_build(buf, MSG_LEN, ANGBAND_DIR_SAVE, "server");

    /* No file */
    if (!file_exists(buf))
    {
        /* Give message */
        plog("Server savefile does not exist.");

        /* No-recall/more_towns modes: read the special levels */
        if (cfg_no_recall || cfg_more_towns)
        {
            if (!load_dungeon_special())
            {
                plog("Cannot read special levels.");
                return FALSE;
            }
        }

        /* Allow this */
        return TRUE;
    }

    /* Open savefile */
    f = file_open(buf, MODE_READ, FTYPE_RAW);
    if (f)
    {
        if ((file_read(f, (char *)&head, 8) == 8) && (memcmp(&head[0], savefile_magic, 4) == 0) &&
            (memcmp(&head[4], savefile_name, 4) == 0))
        {
            if (!try_load(NULL, f, (savefile_loader *)server_loaders, N_ELEMENTS(server_loaders)))
            {
                ok = FALSE;
                plog("Failed loading server savefile.");
            }
        }
        else
        {
            ok = FALSE;
            plog("Server savefile is corrupted or too old -- incorrect file header.");
        }

        file_close(f);
    }
    else
    {
        ok = FALSE;
        plog("Couldn't open server savefile.");
    }

    /* Okay */
    if (ok)
    {
        /* The server state was loaded */
        server_state_loaded = TRUE;

        /* Success */
        return TRUE;
    }

    /* Oops */
    return FALSE;
}
