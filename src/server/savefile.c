/*
 * File: savefile.c
 * Purpose: Savefile loading and saving main routines
 *
 * Copyright (c) 2009 Andi Sidwell <andi@takkaria.org>
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


/*
 * Magic bits at beginning of savefile
 */
static const byte savefile_magic[4] = {1, 1, 12, 3};
static const byte savefile_name[4] = "PWMG";


/* Some useful types */
typedef int (*loader_t)(struct player *);


struct blockheader
{
    char name[16];
    u32b version;
    u32b size;
};


struct blockinfo
{
    char name[16];
    loader_t loader;
    u32b version;
};


/*
 * Savefile saver
 */
typedef struct
{
    char name[16];
    void (*save)(void *);
    u32b version;
} savefile_saver;


/*
 * Savefile saving functions (player)
 */
static const savefile_saver player_savers[] =
{
    /* Hack -- save basic player info */
    {"header", wr_header, 1},

    {"description", wr_description, 1},
    {"monster memory", wr_monster_memory, 1},
    {"object memory", wr_object_memory, 1},
    {"player", wr_player, 1},
    {"ignore", wr_ignore, 1},
    {"misc", wr_player_misc, 1},
    {"artifacts", wr_player_artifacts, 1},
    {"player hp", wr_player_hp, 1},
    {"player spells", wr_player_spells, 1},
    {"gear", wr_gear, 1},
    {"dungeon", wr_player_dungeon, 1},
    {"objects", wr_player_objects, 1},
    {"traps", wr_player_traps, 1},
    {"history", wr_history, 1},

    /* PWMAngband */
    {"wild map", wr_wild_map, 1},
    {"home", wr_home, 1}
};


/*
 * Savefile saving functions (server)
 */
static const savefile_saver server_savers[] =
{
    {"monster memory", wr_monster_memory, 1},
    {"object memory", wr_object_memory, 1},
    {"misc", wr_misc, 1},
    {"artifacts", wr_artifacts, 1},
    {"stores", wr_stores, 1},
    {"dungeons", wr_dungeon, 1},
    {"objects", wr_objects, 1},
    {"monsters", wr_monsters, 1},
    {"traps", wr_traps, 1},

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
    {"dungeon", wr_level, 1}
};


/*
 * Savefile loading functions (player)
 */
static const struct blockinfo player_loaders[] =
{
    /* Hack -- save basic player info */
    {"header", rd_header, 1},

    {"description", rd_null, 1},
    {"monster memory", rd_monster_memory, 1},
    {"object memory", rd_object_memory, 1},
    {"player", rd_player, 1},
    {"ignore", rd_ignore, 1},
    {"misc", rd_player_misc, 1},
    {"artifacts", rd_player_artifacts, 1},
    {"player hp", rd_player_hp, 1},
    {"player spells", rd_player_spells, 1},
    {"gear", rd_gear, 1},
    {"dungeon", rd_player_dungeon, 1},
    {"objects", rd_player_objects, 1},
    {"traps", rd_player_traps, 1},
    {"history", rd_history, 1},

    /* PWMAngband */
    {"wild map", rd_wild_map, 1},
    {"home", rd_home, 1}
};


/*
 * Savefile loading functions (server)
 */
static const struct blockinfo server_loaders[] =
{
    {"monster memory", rd_monster_memory, 1},
    {"object memory", rd_object_memory, 1},
    {"misc", rd_misc, 1},
    {"artifacts", rd_artifacts, 1},
    {"stores", rd_stores, 1},
    {"dungeons", rd_dungeon, 1},
    {"objects", rd_objects, 1},
    {"monsters", rd_monsters, 1},
    {"traps", rd_traps, 1},

    /* PWMAngband */
    {"parties", rd_parties, 1},
    {"houses", rd_houses, 1},
    {"arenas", rd_arenas, 1},
    {"wilderness", rd_wilderness, 1},
    {"player_names", rd_player_names, 1}
};


/* Hack */
static const struct blockinfo special_loaders[] =
{
    {"dungeon", rd_level, 1}
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


/*
 * Base put/get
 */


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
    if ((buffer == NULL) || (buffer_size <= 0) || (buffer_pos >= buffer_size))
        quit("Broken savefile - probably from a development version");

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


void wr_loc(struct loc l)
{
    wr_byte((byte)l.y);
    wr_byte((byte)l.x);
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


void rd_loc(struct loc *l)
{
    byte tmp8u;

    rd_byte(&tmp8u);
    l->y = tmp8u;
    rd_byte(&tmp8u);
    l->x = tmp8u;
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

    dummy = mem_zalloc(max * sizeof(char));
    rd_string(dummy, max);
    mem_free(dummy);
}


/*
 * Savefile saving functions
 */


static bool try_save(void *data, ang_file *file, savefile_saver *savers, size_t n_savers)
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

        savers[i].save(data);

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
    return true;
}


/*
 * Attempt to save the player in a savefile
 */
bool save_player(struct player *p)
{
    ang_file *file;
    int count = 0;
    char new_savefile[MSG_LEN];
    char old_savefile[MSG_LEN];
    bool character_saved = false;

    /* New savefile */
    strnfmt(old_savefile, sizeof(old_savefile), "%s%u.old", p->savefile, Rand_simple(1000000));
    while (file_exists(old_savefile) && (count++ < 100))
    {
        strnfmt(old_savefile, sizeof(old_savefile), "%s%u%u.old", p->savefile,
            Rand_simple(1000000), count);
    }
    count = 0;

    /* Open the savefile */
    strnfmt(new_savefile, sizeof(new_savefile), "%s%u.new", p->savefile, Rand_simple(1000000));
    while (file_exists(new_savefile) && (count++ < 100))
    {
        strnfmt(new_savefile, sizeof(new_savefile), "%s%u%u.new", p->savefile,
            Rand_simple(1000000), count);
    }
    file = file_open(new_savefile, MODE_WRITE, FTYPE_SAVE);

    if (file)
    {
        file_write(file, (char *)&savefile_magic, 4);
        file_write(file, (char *)&savefile_name, 4);

        character_saved = try_save((void *)p, file, (savefile_saver *)player_savers,
            N_ELEMENTS(player_savers));
        file_close(file);
    }

    /* Attempt to save the player */
    if (character_saved)
    {
        bool err = false;

        if (file_exists(p->savefile) && !file_move(p->savefile, old_savefile))
            err = true;

        if (!err)
        {
            if (!file_move(new_savefile, p->savefile)) err = true;

            if (err) file_move(old_savefile, p->savefile);
            else file_delete(old_savefile);
        }

        return !err;
    }

    /* Delete temp file if the save failed */
    if (file) file_delete(new_savefile);

    return false;
}


/*
 * Save special manually-designed dungeon levels
 */
void save_dungeon_special(struct worldpos *wpos, bool town)
{
    char filename[MSG_LEN];
    char lvlname[32];
    ang_file *file;

    /* Build a file name */
    if (town)
        strnfmt(lvlname, sizeof(lvlname), "server.town.%d.%d.%d", wpos->wx, wpos->wy, wpos->depth);
    else
        strnfmt(lvlname, sizeof(lvlname), "server.level.%d.%d.%d", wpos->wx, wpos->wy, wpos->depth);
    path_build(filename, sizeof(filename), ANGBAND_DIR_SAVE, lvlname);

    /* Open the savefile */
    file = file_open(filename, MODE_WRITE, FTYPE_RAW);
    if (file)
    {
        /* Save the level */
        plog_fmt("Saving special file: %s", lvlname);
        try_save((void *)wpos, file, (savefile_saver *)special_savers, N_ELEMENTS(special_savers));
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
    bool server_saved = false;

    /* New savefile */
    strnfmt(old_name, sizeof(old_name), "server%u.old", Rand_simple(1000000));
    path_build(old_savefile, sizeof(old_savefile), ANGBAND_DIR_SAVE, old_name);
    while (file_exists(old_savefile) && (count++ < 100))
    {
        strnfmt(old_name, sizeof(old_name), "server%u%u.old", Rand_simple(1000000), count);
        path_build(old_savefile, sizeof(old_savefile), ANGBAND_DIR_SAVE, old_name);
    }
    count = 0;

    /* Open the savefile */
    strnfmt(new_name, sizeof(new_name), "server%u.new", Rand_simple(1000000));
    path_build(new_savefile, sizeof(new_savefile), ANGBAND_DIR_SAVE, new_name);
    while (file_exists(new_savefile) && (count++ < 100))
    {
        strnfmt(new_name, sizeof(new_name), "server%u%u.new", Rand_simple(1000000), count);
        path_build(new_savefile, sizeof(new_savefile), ANGBAND_DIR_SAVE, new_name);
    }
    file = file_open(new_savefile, MODE_WRITE, FTYPE_SAVE);

    if (file)
    {
        file_write(file, (char *)&savefile_magic, 4);
        file_write(file, (char *)&savefile_name, 4);

        server_saved = try_save(NULL, file, (savefile_saver *)server_savers,
            N_ELEMENTS(server_savers));
        file_close(file);
    }

    /* Attempt to save the server state */
    if (server_saved)
    {
        char savefile[MSG_LEN];
        bool err = false;

        path_build(savefile, sizeof(savefile), ANGBAND_DIR_SAVE, "server");

        if (file_exists(savefile) && !file_move(savefile, old_savefile))
            err = true;

        if (!err)
        {
            if (!file_move(new_savefile, savefile)) err = true;

            if (err) file_move(old_savefile, savefile);
            else file_delete(old_savefile);
        }

        return !err;
    }

    /* Delete temp file if the save failed */
    if (file) file_delete(new_savefile);

    return false;
}


/*
 * Savefile loading functions
 */


/*
 * Check the savefile header file clearly indicates that it's a savefile
 */
static bool check_header(ang_file *f)
{
    byte head[8];

    if ((file_read(f, (char *)&head, 8) == 8) && (memcmp(&head[0], savefile_magic, 4) == 0) &&
        (memcmp(&head[4], savefile_name, 4) == 0))
    {
        return true;
    }

    return false;
}


static void throw_err(struct player *p, const char *str)
{
    plog(str);
    if (p) Destroy_connection(p->conn, (char *)str);
}


/*
 * Get the next block header from the savefile
 */
static errr next_blockheader(ang_file *f, struct blockheader *b, bool scoop)
{
    byte savefile_head[SAVEFILE_HEAD_SIZE];
    size_t len;
    const char *header = "header";

    len = file_read(f, (char *)savefile_head, SAVEFILE_HEAD_SIZE);

    /* No more blocks */
    if (len == 0) return 1;

    if ((len != SAVEFILE_HEAD_SIZE) || (savefile_head[15] != 0))
        return -1;

    /* Determine the block ID */
    if (scoop && (strncmp((char *)savefile_head, header, sizeof(header)) != 0))
        return -1;

#define RECONSTRUCT_U32B(from) \
    ((u32b)savefile_head[from]) | \
    ((u32b)savefile_head[from + 1] << 8) | \
    ((u32b)savefile_head[from + 2] << 16) | \
    ((u32b)savefile_head[from + 3] << 24);

    my_strcpy(b->name, (char *)&savefile_head, sizeof(b->name));
    b->version = RECONSTRUCT_U32B(16);
    b->size = RECONSTRUCT_U32B(20);

    /* Pad to 4 bytes */
    if (b->size % 4) b->size += 4 - (b->size % 4);

    return 0;
}


/*
 * Find the right loader for this block, return it
 */
static loader_t find_loader(struct blockheader *b, const struct blockinfo *loaders, size_t n_loaders)
{
    size_t i = 0;

    /* Find the right loader */
    for (i = 0; i < n_loaders; i++)
    {
        if (!streq(b->name, loaders[i].name)) continue;
        if (b->version != loaders[i].version) continue;

        return loaders[i].loader;
    }

    return NULL;
}


/*
 * Load a given block with the given loader
 */
static bool load_block(struct player *p, ang_file *f, struct blockheader *b, loader_t loader)
{
    /* Allocate space for the buffer */
    buffer = mem_alloc(b->size);
    buffer_pos = 0;
    buffer_check = 0;

    buffer_size = file_read(f, (char *)buffer, b->size);
    if ((buffer_size != b->size) || (loader(p) != 0))
    {
        mem_free(buffer);
        return false;
    }

    mem_free(buffer);
    return true;
}


/*
 * Skip a block
 */
static void skip_block(ang_file *f, struct blockheader *b)
{
    file_skip(f, b->size);
}


/*
 * Try to load a savefile
 */
static bool try_load(struct player *p, ang_file *f, const struct blockinfo *loaders,
    size_t n_loaders, bool with_header)
{
    struct blockheader b;
    errr err;

    if (with_header && !check_header(f))
    {
        throw_err(p, "Savefile is corrupted or too old -- incorrect file header.");
        return false;
    }

    /* Get the next block header */
    while ((err = next_blockheader(f, &b, false)) == 0)
    {
        loader_t loader = find_loader(&b, loaders, n_loaders);

        /* No loader found */
        if (!loader)
        {
            throw_err(p, "Savefile block can't be read -- probably an old savefile.");
            return false;
        }

        if (!load_block(p, f, &b, loader))
        {
            throw_err(p,
                format("Savefile is corrupted or too old -- couldn't load block %s", b.name));
            return false;
        }

        /* Hack -- load any special static levels */
        if (streq(b.name, "dungeons"))
        {
            if (!load_dungeon_special()) return false;
        }
    }

    if (err == -1)
    {
        throw_err(p, "Savefile is corrupted or too old -- block header mangled.");
        return false;
    }

    return true;
}


/* XXX this isn't nice but it'll have to do */
static char savefile_desc[120];


static int get_desc(struct player *unused)
{
    rd_string(savefile_desc, sizeof(savefile_desc));
    return 0;
}


/*
 * Try to get the 'description' block from a savefile. Fail gracefully.
 */
const char *savefile_get_description(const char *path)
{
    struct blockheader b;
    ang_file *f = file_open(path, MODE_READ, FTYPE_RAW);

    if (!f) return NULL;

    /* Blank the description */
    savefile_desc[0] = 0;

    if (!check_header(f))
        my_strcpy(savefile_desc, "Invalid savefile", sizeof(savefile_desc));
    else
    {
        while (!next_blockheader(f, &b, false))
        {
            if (!streq(b.name, "description"))
            {
                skip_block(f, &b);
                continue;
            }

            load_block(NULL, f, &b, get_desc);
            break;
        }
    }

    file_close(f);

    return savefile_desc;
}


static int try_scoop(ang_file *f, char *pass_word, byte *pridx, byte *pcidx, byte *psex)
{
    struct blockheader b;
    errr err;
    char pass[NORMAL_WID];
    char stored_pass[NORMAL_WID];
    char client_pass[NORMAL_WID];
    char buf[NORMAL_WID];
    struct player_race *r;
    struct player_class *c;

    if (!check_header(f))
    {
        plog("Savefile is corrupted or too old -- incorrect file header.");
        return -1;
    }

    /* Get the next block header */
    err = next_blockheader(f, &b, true);
    if (err == -1)
    {
        plog("Savefile is corrupted or too old -- block header mangled.");
        return -1;
    }

    /* There should be at least one block */
    if (err == 1)
    {
        plog("Cannot read savefile -- no block of data found.");
        return -1;
    }

    /* Allocate space for the buffer */
    buffer = mem_alloc(b.size);
    buffer_pos = 0;
    buffer_check = 0;

    buffer_size = file_read(f, (char *)buffer, b.size);
    if (buffer_size != b.size)
    {
        plog("Savefile is corrupted or too old -- block too short.");
        mem_free(buffer);
        return -1;
    }

    /* Try to fetch the data */
    strip_string(NORMAL_WID);
    rd_string(pass, NORMAL_WID);
    rd_string(buf, sizeof(buf));
    r = lookup_player_race(buf);
    if (!r)
    {
        plog("Savefile is corrupted or too old -- invalid player race.");
        mem_free(buffer);
        return -1;
    }
    *pridx = r->ridx;
    rd_string(buf, sizeof(buf));
    c = lookup_player_class(buf);
    if (!c)
    {
        plog("Savefile is corrupted or too old -- invalid player class.");
        mem_free(buffer);
        return -1;
    }
    *pcidx = c->cidx;
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
                plog("Incorrect password");
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
                plog("Incorrect password");
                err = -2;
            }
        }
        else
        {
            /* Most likely clear text from client as well */
            if (strcmp(pass, pass_word))
            {
                /* No, it's not correct */
                plog("Incorrect password");
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
    bool ok;
    ang_file *f = file_open(p->savefile, MODE_READ, FTYPE_RAW);

    if (!f)
    {
        throw_err(p, "Couldn't open savefile.");
        return false;
    }

    ok = try_load(p, f, player_loaders, N_ELEMENTS(player_loaders), true);
    file_close(f);

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
    int err;
    ang_file *f;
    char tmp[MSG_LEN];

    my_strcpy(tmp, nick, sizeof(tmp));

    if (!savefile_set_name(NULL, tmp, nick))
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
    if (!f)
    {
        plog("Couldn't open savefile.");
        return -1;
    }

    err = try_scoop(f, pass, pridx, pcidx, psex);
    file_close(f);

    return err;
}


/*
 * Maximum number of special pre-designed static levels.
 */
#define MAX_SPECIAL_LEVELS 10


/* List of coordinates which are special static levels */
static struct worldpos special_levels[MAX_SPECIAL_LEVELS];


/* List of coordinates which are special static towns */
static struct worldpos special_towns[MAX_SPECIAL_LEVELS];


/*
 * Read special static pre-designed dungeon levels
 *
 * Special pre-designed levels are stored in separate files with
 * the filename "server.level.<wild_x>.<wild_y>.<depth>".
 *
 * Special pre-designed towns are stored in separate files with
 * the filename "server.town.<wild_x>.<wild_y>.<depth>".
 *
 * Level files are searched for at runtime and loaded if present.
 *
 * On no_recall or more_towns servers, we first search for a special town.
 * If not found, we search for a special level instead. On other servers,
 * we only search for special levels.
 */
static bool load_dungeon_special(void)
{
    char filename[MSG_LEN];
    char levelname[32];
    ang_file *fhandle;
    int i, x, y, num_levels = 0, num_towns = 0;

    /* Clear all the special levels and towns */
    for (i = 0; i < MAX_SPECIAL_LEVELS; i++)
    {
        COORDS_SET(&special_levels[i], 0, 0, -1);
        COORDS_SET(&special_towns[i], 0, 0, -1);
    }

    for (y = radius_wild; y >= 0 - radius_wild; y--)
    {
        for (x = 0 - radius_wild; x <= radius_wild; x++)
        {
            struct wild_type *w_ptr = get_wt_info_at(y, x);

            /* Don't load special wilderness levels if no wilderness */
            if (cfg_diving_mode && !((y == base_wpos()->wy) && (x == base_wpos()->wx))) continue;

            for (i = 0; i < w_ptr->max_depth; i++)
            {
                bool ok, town = false;

                /* Paranoia */
                if ((i > 0) && (i < w_ptr->min_depth)) continue;

                /* No special "quest" levels */
                if (is_quest(i)) continue;

                /* Special static pre-designed towns are only used on no_recall or more_towns servers */
                if ((cfg_diving_mode == 2) || cfg_more_towns)
                {
                    /* Build a file name */
                    strnfmt(levelname, sizeof(levelname), "server.town.%d.%d.%d", x, y, i);
                    path_build(filename, sizeof(filename), ANGBAND_DIR_SAVE, levelname);

                    if (file_exists(filename))
                        town = true;
                    else
                    {
                        /* If no special town is found, check for special level */
                        strnfmt(levelname, sizeof(levelname), "server.level.%d.%d.%d", x, y, i);
                        path_build(filename, sizeof(filename), ANGBAND_DIR_SAVE, levelname);
                    }
                }

                /* Special static pre-designed levels can be used on other servers */
                else
                {
                    /* Build a file name */
                    strnfmt(levelname, sizeof(levelname), "server.level.%d.%d.%d", x, y, i);
                    path_build(filename, sizeof(filename), ANGBAND_DIR_SAVE, levelname);
                }

                /* Open the file if it exists */
                fhandle = file_open(filename, MODE_READ, FTYPE_RAW);
                if (fhandle)
                {
                    /* Load the level */
                    plog_fmt("Loading special file: %s", levelname);
                    ok = try_load(NULL, fhandle, special_loaders, N_ELEMENTS(special_loaders), false);

                    /* Close the level file */
                    file_close(fhandle);

                    if (!ok) return false;

                    if (town)
                    {
                        /* We have an arbitrary max number of towns */
                        if (num_towns + 1 > MAX_SPECIAL_LEVELS) break;

                        /* Add this depth to the special town list */
                        COORDS_SET(&special_towns[num_towns], y, x, i);
                        num_towns++;
                    }
                    else
                    {
                        /* We have an arbitrary max number of levels */
                        if (num_levels + 1 > MAX_SPECIAL_LEVELS) break;

                        /* Add this depth to the special level list */
                        COORDS_SET(&special_levels[num_levels], y, x, i);
                        num_levels++;
                    }
                }
            }
        }
    }

    /* Success */
    return true;
}


/*
 * Load the server info (artifacts created and uniques killed)
 * from a special savefile.
 */
bool load_server_info(void)
{
    bool ok;
    ang_file *f;
    char buf[MSG_LEN];

    path_build(buf, sizeof(buf), ANGBAND_DIR_SAVE, "server");

    /* No file */
    if (!file_exists(buf))
    {
        /* Give message */
        plog("Server savefile does not exist.");

        /* Read the special levels */
        if (!load_dungeon_special())
        {
            plog("Cannot read special levels.");
            return false;
        }

        /* Allow this */
        return true;
    }

    /* Open savefile */
    f = file_open(buf, MODE_READ, FTYPE_RAW);

    if (!f)
    {
        plog("Couldn't open server savefile.");
        return false;
    }

    ok = try_load(NULL, f, server_loaders, N_ELEMENTS(server_loaders), true);
    file_close(f);

    /* Okay */
    if (ok)
    {
        /* The server state was loaded */
        server_state_loaded = true;

        /* Success */
        return true;
    }

    /* Oops */
    return false;
}


/*
 * Return true if the given level is a special static level, i.e. a hand designed level.
 */
bool special_level(struct worldpos *wpos)
{
    int i;

    for (i = 0; i < MAX_SPECIAL_LEVELS; i++)
    {
        if (COORDS_EQUAL(wpos, &special_levels[i]) || COORDS_EQUAL(wpos, &special_towns[i]))
            return true;
    }

    return false;
}


/*
 * Return true if the given depth is a special static town.
 */
bool special_town(struct worldpos *wpos)
{
    int i;

    for (i = 0; i < MAX_SPECIAL_LEVELS; i++)
    {
        if (COORDS_EQUAL(wpos, &special_towns[i])) return true;
    }

    return false;
}


/*
 * Forbid in the towns or on special levels.
 */
bool forbid_special(struct worldpos *wpos)
{
    if (special_level(wpos)) return true;
    if (in_town(wpos)) return true;
    return false;
}


/*
 * Forbid in the towns.
 */
bool forbid_town(struct worldpos *wpos)
{
    if (special_town(wpos)) return true;
    if (in_town(wpos)) return true;
    return false;
}


/*
 * Returns whether "wpos" corresponds to a randomly generated level.
 */
bool random_level(struct worldpos *wpos)
{
    return ((wpos->depth > 0) && !special_level(wpos));
}


/*
 * Return true if the given level is a dynamically generated town.
 */
bool dynamic_town(struct worldpos *wpos)
{
    /* Only on no_recall servers if there is no static pre-designed dungeon town loaded */
    if (special_town(wpos) || (cfg_diving_mode < 2)) return false;

    /* Not in wilderness dungeons */
    if ((wpos->wy != base_wpos()->wy) || (wpos->wx != base_wpos()->wx)) return false;

    /* Every 1000ft */
    return ((wpos->depth == 20) || (wpos->depth == 40) || (wpos->depth == 60) ||
        (wpos->depth == 80));
}
