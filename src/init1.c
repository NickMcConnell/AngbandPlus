/* File: init1.c */

/*
 * Copyright (c) 1997 Ben Harrison
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies. Other copyrights may also apply.
 */

/* Purpose: Initialization (part 1) -BEN- */

#include "angband.h"
#include "rooms.h"

#include <assert.h>
/*
 * This file is used to initialize various variables and arrays for the
 * Angband game. Note the use of "fd_read()" and "fd_write()" to bypass
 * the common limitation of "read()" and "write()" to only 32767 bytes
 * at a time.
 *
 * Several of the arrays for Angband are built from "template" files in
 * the "lib/file" directory, from which quick-load binary "image" files
 * are constructed whenever they are not present in the "lib/data"
 * directory, or if those files become obsolete, if we are allowed.
 *
 * Warning -- the "ascii" file parsers use a minor hack to collect the
 * name and text information in a single pass. Thus, the game will not
 * be able to load any template file with more than 20K of names or 60K
 * of text, even though technically, up to 64K should be legal.
 *
 * XXX ALLOW_TEMPLATES has been removed. The game no longer supports
 * .raw files and will not run if the parsing code is removed. XXX
 */


#include "init.h"


/*** Helper arrays for parsing ascii template files ***/
static int _get_gf_type(cptr which)
{
    gf_info_ptr gf = gf_parse_name(which);
    if (gf) return gf->id;
    return GF_NONE;
}

static int _get_r_blow_method(cptr name)
{
    mon_blow_info_ptr info = mon_blow_info_parse(name);
    if (info) return info->id;
    return RBM_NONE;
}

static int _get_r_blow_effect(cptr which)
{
    int i;
    struct { cptr name; int id; } _table[] = {
        {"HURT", RBE_HURT},
        {"SHATTER", RBE_SHATTER},
        {"EAT_GOLD", RBE_EAT_GOLD},
        {"EAT_ITEM", RBE_EAT_ITEM},
        {"EAT_FOOD", RBE_EAT_FOOD},
        {"EAT_LITE", RBE_EAT_LITE},
        {"LOSE_STR", RBE_LOSE_STR},
        {"LOSE_INT", RBE_LOSE_INT},
        {"LOSE_WIS", RBE_LOSE_WIS},
        {"LOSE_DEX", RBE_LOSE_DEX},
        {"LOSE_CON", RBE_LOSE_CON},
        {"LOSE_CHR", RBE_LOSE_CHR},
        {"LOSE_ALL", RBE_LOSE_ALL},
        {"DISEASE", RBE_DISEASE},
        {"DRAIN_CHARGES", RBE_DRAIN_CHARGES},
        {"DRAIN_EXP", RBE_DRAIN_EXP},
        {"VAMP", RBE_VAMP},
        {"CUT", RBE_CUT},
        {"STUN_MALE", RBE_STUN_MALE},
        {"SLOW_ANKLE", RBE_SLOW_ANKLE},
        {0}};
    for (i = 0;; i++)
    {
        if (!_table[i].name) break;
        if (streq(_table[i].name, which)) return _table[i].id;
    }
    return _get_gf_type(which);
}


/*
 * Feature info flags
 */
static cptr f_info_flags[] =
{
    "LOS",
    "PROJECT",
    "MOVE",
    "PLACE",
    "DROP",
    "SECRET",
    "NOTICE",
    "REMEMBER",
    "OPEN",
    "CLOSE",
    "BASH",
    "SPIKE",
    "DISARM",
    "STORE",
    "TUNNEL",
    "MAY_HAVE_GOLD",
    "HAS_GOLD",
    "HAS_ITEM",
    "DOOR",
    "TRAP",
    "STAIRS",
    "GLYPH",
    "LESS",
    "MORE",
    "AVOID_RUN",
    "FLOOR",
    "WALL",
    "PERMANENT",
    "XXX00",
    "XXX01",
    "XXX02",
    "HIT_TRAP",

    "BRIDGE",
    "RIVER",
    "LAKE",
    "BRIDGED",
    "COVERED",
    "GLOW",
    "ENSECRET",
    "WATER",
    "LAVA",
    "SHALLOW",
    "DEEP",
    "FILLED",
    "HURT_ROCK",
    "HURT_FIRE",
    "HURT_COLD",
    "HURT_ACID",
    "ICE",
    "ACID",
    "OIL",
    "XXX04",
    "CAN_CLIMB",
    "CAN_FLY",
    "CAN_SWIM",
    "CAN_PASS",
    "CAN_OOZE",
    "CAN_DIG",
    "HIDE_ITEM",
    "HIDE_SNEAK",
    "HIDE_SWIM",
    "HIDE_DIG",
    "KILL_HUGE",
    "KILL_MOVE",

    "PICK_TRAP",
    "PICK_DOOR",
    "ALLOC",
    "CHEST",
    "DROP_1D2",
    "DROP_2D2",
    "DROP_GOOD",
    "DROP_GREAT",
    "HURT_POIS",
    "HURT_ELEC",
    "HURT_WATER",
    "HURT_BWATER",
    "USE_FEAT",
    "GET_FEAT",
    "GROUND",
    "OUTSIDE",
    "EASY_HIDE",
    "EASY_CLIMB",
    "MUST_CLIMB",
    "TREE",
    "NEED_TREE",
    "BLOOD",
    "DUST",
    "SLIME",
    "PLANT",
    "XXX2",
    "INSTANT",
    "EXPLODE",
    "TIMED",
    "ERUPT",
    "STRIKE",
    "SPREAD",

    "SPECIAL",
    "HURT_DISI",
    "QUEST_ENTER",
    "QUEST_EXIT",
    "QUEST",
    "SHAFT",
    "MOUNTAIN",
    "BLDG",
    "MON_TRAP",
    "PATTERN",
    "TOWN",
    "ENTRANCE",
    "MIRROR",
    "UNPERM",
    "TELEPORTABLE",
    "CONVERT",
    "GLASS",
    "ROGUE_TRAP_1",
    "ROGUE_TRAP_2",
    "ROGUE_TRAP_3",
    "WEB",
    "RECALL",
    "TRAVEL",
};


/*
 * Monster race flags
 */
static cptr r_info_flags1[] =
{
    "UNIQUE",
    "FIXED_UNIQUE",
    "MALE",
    "FEMALE",
    "CHAR_CLEAR",
    "SHAPECHANGER",
    "ATTR_CLEAR",
    "ATTR_MULTI",
    "FORCE_DEPTH",
    "FORCE_MAXHP",
    "FORCE_SLEEP",
    "FORCE_EXTRA",
    "ATTR_SEMIRAND",
    "FRIENDS",
    "ESCORT",
    "NO_SUMMON",
    "NEVER_BLOW",
    "NEVER_MOVE",
    "RAND_25",
    "RAND_50",
    "XXX21",
    "XXX22",
    "XXX23",
    "XXX24",
    "XXX25",
    "XXX26",
    "XXX27",
    "XXX28",
    "XXX29",
    "XXX30",
    "TRUMP",
    "NO_QUEST"
};

/*
 * Monster race flags
 */
static cptr r_info_flags2[] =
{
    "STUPID",
    "SMART",
    "CAN_SPEAK",
    "REFLECTING",
    "INVISIBLE",
    "COLD_BLOOD",
    "EMPTY_MIND",
    "WEIRD_MIND",
    "MULTIPLY",
    "REGENERATE",
    "CHAR_MULTI",
    "ATTR_ANY",
    "XXX",
    "ELDRITCH_HORROR",
    "XXX",
    "XXX",
    "OPEN_DOOR",
    "BASH_DOOR",
    "PASS_WALL",
    "KILL_WALL",
    "MOVE_BODY",
    "KILL_BODY",
    "TAKE_ITEM",
    "KILL_ITEM",
    "AURA_REVENGE",
    "THIEF",
    "AURA_FEAR",
    "XXX",
    "KNIGHT",
    "XXX",
    "HUMAN",
    "QUANTUM"
};

/*
 * Monster race flags
 */
static cptr r_info_flags3[] =
{
    "ORC",
    "TROLL",
    "GIANT",
    "DRAGON",
    "DEMON",
    "UNDEAD",
    "EVIL",
    "ANIMAL",
    "AMBERITE",
    "GOOD",
    "XXX",
    "NONLIVING",
    "HURT_LITE",
    "HURT_ROCK",
    "HURT_FIRE",
    "HURT_COLD",
    "OLYMPIAN",
    "XXX",
    "XXX",
    "XXX",
    "XXX",
    "XXX",
    "XXX",
    "XXX",
    "XXX",
    "XXX",
    "XXX",
    "XXX",
    "NO_FEAR",
    "NO_STUN",
    "NO_CONF",
    "NO_SLEEP"
};

/*
 * Monster race flags
 */
static cptr r_info_flags7[] =
{
    "AQUATIC",
    "CAN_SWIM",
    "CAN_FLY",
    "FRIENDLY",
    "NAZGUL",
    "UNIQUE2",
    "RIDING",
    "KAGE",
    "HAS_LITE_1",
    "SELF_LITE_1",
    "HAS_LITE_2",
    "SELF_LITE_2",
    "GUARDIAN",
    "CHAMELEON",
    "KILL_EXP",
    "TANUKI",
    "HAS_DARK_1",
    "SELF_DARK_1",
    "HAS_DARK_2",
    "SELF_DARK_2",
    "CAN_CLIMB",
    "XXX7X21",
    "XXX7X22",
    "XXX7X23",
    "XXX7X24",
    "XXX7X25",
    "XXX7X26",
    "XXX7X27",
    "XXX7X28",
    "XXX7X29",
    "XXX7X30",
    "XXX7X31",
};

/*
 * Monster race flags
 */
static cptr r_info_flags8[] =
{
    "WILD_ONLY",
    "WILD_TOWN",
    "XXX8X02",
    "WILD_SHORE",
    "WILD_OCEAN",
    "WILD_WASTE",
    "WILD_WOOD",
    "WILD_VOLCANO",
    "XXX8X08",
    "WILD_MOUNTAIN",
    "WILD_GRASS",
    "XXX8X11",
    "XXX8X12",
    "XXX8X13",
    "XXX8X14",
    "XXX8X15",
    "XXX8X16",
    "XXX8X17",
    "XXX8X18",
    "XXX8X19",
    "XXX8X20",
    "XXX8X21",
    "XXX8X22",
    "XXX8X23",
    "XXX8X24",
    "XXX8X25",
    "XXX8X26",
    "XXX8X27",
    "XXX8X28",
    "XXX8X29",
    "WILD_SWAMP",    /* ToDo: Implement Swamp */
    "WILD_ALL",
};


/*
 * Monster race flags - Drops
 */
static cptr r_info_flags9[] =
{
    "DROP_CORPSE",
    "DROP_SKELETON",
    "POS_GAIN_AC",
    "POS_TELEPATHY",
    "POS_SEE_INVIS",
    "POS_HOLD_LIFE",
    "POS_SUST_STR",
    "POS_SUST_INT",
    "POS_SUST_WIS",
    "POS_SUST_DEX",
    "POS_SUST_CON",
    "POS_SUST_CHR",
    "XXX13",
    "XXX14",
    "XXX15",
    "XXX16",
    "XXX17",
    "XXX18",
    "XXX19",
    "XXX20",
    "XXX21",
    "XXX22",
    "XXX23",
    "XXX24",
    "POS_BACKSTAB",
    "XXX26",
    "XXX27",
    "XXX28",
    "XXX29",
    "XXX30",
    "XXX31",
    "DEPRECATED",
};

/*
 * Monster race flags - Resistances
 */
static cptr r_info_flagsr[] =
{
    "IM_ACID",
    "IM_ELEC",
    "IM_FIRE",
    "IM_COLD",
    "IM_POIS",
    "RES_LITE",
    "RES_DARK",
    "RES_NETH",
    "RES_WATE",
    "RES_PLAS",
    "RES_SHAR",
    "RES_SOUN",
    "RES_CHAO",
    "RES_NEXU",
    "RES_DISE",
    "RES_WALL",
    "RES_INER",
    "RES_TIME",
    "RES_GRAV",
    "RES_ALL",
    "RES_TELE",
    "XXX",
    "RES_ACID",
    "RES_ELEC",
    "RES_FIRE",
    "RES_COLD",
    "RES_POIS",
    "XXX27",
    "XXX28",
    "XXX29",
    "XXX30",
    "XXX31",
};

static cptr k_info_gen_flags[] =
{
    "INSTA_ART",
    "QUESTITEM",
    "XTRA_POWER",
    "ONE_SUSTAIN",
    "XTRA_RES_OR_POWER",
    "XTRA_H_RES",
    "XTRA_E_RES",
    "XTRA_L_RES",
    "XTRA_D_RES",
    "XTRA_RES",
    "CURSED",
    "HEAVY_CURSE",
    "PERMA_CURSE",
    "RANDOM_CURSE0",
    "RANDOM_CURSE1",
    "RANDOM_CURSE2",
    "AWARE",
    "TOWN",
    "FIXED_ART",
    "FIXED_ACT",
    "XXX",
    "XXX",
    "XXX",
    "XXX",
    "XXX",
    "XXX",
    "XXX",
    "XXX",
    "XXX",
    "XXX",
    "XXX",
    "XXX",
};

/*
 * Add a text to the text-storage and store offset to it.
 *
 * Returns FALSE when there isn't enough space available to store
 * the text.
 */
static bool add_text(u32b *offset, header *head, cptr buf, bool normal_text)
{
    /* Hack -- Verify space */
    if (head->text_size + strlen(buf) + 8 > FAKE_TEXT_SIZE)
        return (FALSE);

    /* New text? */
    if (*offset == 0)
    {
        /* Advance and save the text index */
        *offset = ++head->text_size;
    }

    /* Additional text */
    else if (normal_text)
    {
        /*
         * If neither the end of the last line nor
         * the beginning of current line is not a space,
         * fill up a space as a correct separator of two words.
         */
        if (head->text_size > 0 &&
            (*(head->text_ptr + head->text_size - 1) != ' ') &&
            (buf[0] != ' ')
            )
        {
            /* Append a space */
            *(head->text_ptr + head->text_size) = ' ';

            /* Advance the index */
            head->text_size++;
        }
    }

    /* Append chars to the text */
    strcpy(head->text_ptr + head->text_size, buf);

    /* Advance the index */
    head->text_size += strlen(buf);

    /* Success */
    return (TRUE);
}


/*
 * Add a name to the name-storage and return an offset to it.
 *
 * Returns FALSE when there isn't enough space available to store
 * the name.
 */
static bool add_name(u32b *offset, header *head, cptr buf)
{
    /* Hack -- Verify space */
    if (head->name_size + strlen(buf) + 8 > FAKE_NAME_SIZE)
        return (FALSE);

    /* New name? */
    if (*offset == 0)
    {
        /* Advance and save the name index */
        *offset = ++head->name_size;
    }

    /* Append chars to the names */
    strcpy(head->name_ptr + head->name_size, buf);

    /* Advance the index */
    head->name_size += strlen(buf);

    /* Success */
    return (TRUE);
}


/*
 * Add a tag to the tag-storage and return an offset to it.
 *
 * Returns FALSE when there isn't enough space available to store
 * the name.
 */
static bool add_tag(s16b *offset, header *head, cptr buf)
{
    u32b i;

    /* Search for an existing (fake) tag */
    for (i = 1; i < head->tag_size; i += strlen(&head->tag_ptr[i]) + 1)
    {
        /* Found it */
        if (streq(&head->tag_ptr[i], buf)) break;
    }

    /* There was no existing tag */
    if (i >= head->tag_size)
    {
        /* Hack -- Verify space */
        if (head->tag_size + strlen(buf) + 8 > FAKE_TAG_SIZE)
            return FALSE;

        /* Append chars to the tags */
        strcpy(head->tag_ptr + head->tag_size, buf);

        /* Point the new tag */
        i = head->tag_size;

        /* Advance the index */
        head->tag_size += strlen(buf) + 1;
    }

    /* Return offset of the tag */
    *offset = (s16b)i;

    /* Success */
    return TRUE;
}


/*
 * Convert a "color letter" into an "actual" color
 * The colors are: dwsorgbuDWvyRGBU, as shown below
 */
byte color_char_to_attr(char c)
{
    switch (c)
    {
        case 'd': return (TERM_DARK);
        case 'w': return (TERM_WHITE);
        case 's': return (TERM_SLATE);
        case 'o': return (TERM_ORANGE);
        case 'r': return (TERM_RED);
        case 'g': return (TERM_GREEN);
        case 'b': return (TERM_BLUE);
        case 'u': return (TERM_UMBER);

        case 'D': return (TERM_L_DARK);
        case 'W': return (TERM_L_WHITE);
        case 'v': return (TERM_VIOLET);
        case 'y': return (TERM_YELLOW);
        case 'R': return (TERM_L_RED);
        case 'G': return (TERM_L_GREEN);
        case 'B': return (TERM_L_BLUE);
        case 'U': return (TERM_L_UMBER);
    }

    return (255);
}



/*** Initialize from ascii template files ***/


/*
 * Initialize an "*_info" array, by parsing an ascii "template" file
 */
errr init_info_txt(FILE *fp, char *buf, header *head,
           parse_info_txt_func parse_info_txt_line)
{
    errr err;

    /* Just before the first record */
    error_idx = -1;

    /* Just before the first line */
    error_line = 0;


    /* Prepare the "fake" stuff */
    head->name_size = 0;
    head->text_size = 0;
    head->tag_size = 0;

    /* Parse */
    while (0 == my_fgets(fp, buf, 1024))
    {
        /* Advance the line number */
        error_line++;

        /* Skip comments and blank lines */
        if (!buf[0] || (buf[0] == '#')) continue;

        /* Verify correct "colon" format */
        if (buf[1] != ':') return (PARSE_ERROR_GENERIC);


        /* Hack -- Process 'V' for "Version" */
        if (buf[0] == 'V')
        {
            /* ignore */
            continue;
        }

        /* Mega Hack -- Calculate Check Sum */
        if (buf[0] != 'N' && buf[0] != 'D')
        {
            int i;
            for (i = 0; buf[i]; i++)
            {
                head->v_extra += (byte)buf[i];
                head->v_extra ^= (1 << (i % 8));
            }
        }

        /* Parse the line */
        if ((err = (*parse_info_txt_line)(buf, head)) != 0)
            return (err);
    }


    /* Complete the "name" and "text" sizes */
    if (head->name_size) head->name_size++;
    if (head->text_size) head->text_size++;

    /* Success */
    return (0);
}

static bool _is_digit(char c)
{
    if ('0' <= c && c <= '9')
        return TRUE;
    return FALSE;
}

static bool _is_numeric(const char *token) /* [0-9]+ */
{
    const char *c = token;
    if (!*c) return FALSE;
    if (!_is_digit(*c)) return FALSE;
    for (c++; *c; c++)
    {
        if (!_is_digit(*c)) return FALSE;
    }
    return TRUE;
}

static bool _is_d_char(const char *token)
{
    int i;
    if (strlen(token) != 1) return FALSE;
    for (i = 0; i < max_r_idx; i++)
    {
        if (token[0] == r_info[i].d_char) return TRUE;
    }
    return FALSE;
}

/* this is almost strip_name_aux ... but I want to support
 * partial matches (e.g. EGO(speed) to match 'of Speed' and
 * EGO(pattern) to match '(Pattern)' (Note: EGO((Pattern)) 
 * would sadly confuse our parser ...) Also, MON(raal's)
 * rather than MON(raal's tome of destruction) will keep
 * my fingers happy. */
static void _prep_name_aux(char *dest, const char *src)
{
    char *t;

    while (*src == ' ' || *src == '&' || *src == '[' || *src == '(')
        src++;

    for (t = dest; *src; src++)
    {
        char c = *src;
        if (c != '~' && c != ']' && c != ')')
        {
            if (isupper(c)) c = tolower(c);
            *t++ = c;
        }
    }

    *t = '\0';
}

void parse_prep_name(char *dest, const char *src)
{
    strcpy(dest, "^"); /* OBJ(^dagger) matches ^dagger$ not ^broken dagger$ */
    _prep_name_aux(dest + strlen(dest), src);
    strcat(dest, "$"); /* MON(ent$) matches ^ent$ not ^agent of the black market$ */
}

/* Same order as summon_specific_e in defines.h
   These are legal monster types for the MON() directive when
   specifying room_grid_t
 */
static parse_tbl_t _summon_type_tbl[] = {
    { SUMMON_MONSTER, "Monsters", TERM_WHITE, "", "MONSTER", 10 },
    { SUMMON_ANT, "Ants", TERM_WHITE, "", "ANT", 10 },
    { SUMMON_SPIDER, "Spiders", TERM_WHITE, "", "SPIDER", 10 },
    { SUMMON_HOUND, "Hounds", TERM_WHITE, "", "HOUND", 15 },
    { SUMMON_HYDRA, "Hydras", TERM_WHITE, "", "HYDRA", 20 },
    { SUMMON_ANGEL, "Angels", TERM_WHITE, "", "ANGEL", 60 },
    { SUMMON_DEMON, "Demons", TERM_WHITE, "", "DEMON", 40 },
    { SUMMON_UNDEAD, "Undead", TERM_WHITE, "", "UNDEAD", 40 },
    { SUMMON_DRAGON, "Dragons", TERM_WHITE, "", "DRAGON", 40 },
    { SUMMON_HI_UNDEAD, "Mighty Undead", TERM_L_DARK, "", "HI_UNDEAD", 60 },
    { SUMMON_HI_DRAGON, "Ancient Dragons", TERM_RED, "", "HI_DRAGON", 70 },
    { SUMMON_HI_DEMON, "Foul Demons", TERM_RED, "", "HI_DEMON", 66 },
    { SUMMON_AMBERITE, "Amberites", TERM_WHITE, "", "AMBERITE", 100 },
    { SUMMON_UNIQUE, "Uniques", TERM_VIOLET, "", "UNIQUE", 125 },
    { SUMMON_BIZARRE1, "Mold", TERM_WHITE, "", "BIZARRE1", 15 },
    { SUMMON_BIZARRE2, "Bats", TERM_WHITE, "", "BIZARRE2", 5 },
    { SUMMON_BIZARRE3, "Quylthulgs", TERM_WHITE, "", "BIZARRE3", 20 },
    { SUMMON_BIZARRE4, "Vortices", TERM_WHITE, "", "BIZARRE4", 20 },
    { SUMMON_BIZARRE5, "Creeping Coins", TERM_WHITE, "", "BIZARRE5", 5 },
    { SUMMON_BIZARRE6, "Mimics", TERM_WHITE, "", "BIZARRE6", 10 },
    { SUMMON_CYBER, "Cyberdemons", TERM_WHITE, "", "CYBER", 80 },
    { SUMMON_KIN, "Kin", TERM_WHITE, "", "KIN", 35 },
    { SUMMON_DAWN, "Warriors of the Dawn", TERM_WHITE, "", "DAWN", 30 },
    { SUMMON_ANIMAL, "Animals", TERM_WHITE, "", "ANIMAL", 15 },
    { SUMMON_ANIMAL_RANGER, "Animals", TERM_WHITE, "", "ANIMAL_RANGER", 15 },
    { SUMMON_PHANTOM, "Phantom Beasts", TERM_WHITE, "", "PHANTOM", 20 },
    { SUMMON_BLUE_HORROR, "Blue Horrors", TERM_WHITE, "", "BLUE_HORROR", 5 },
    { SUMMON_LIVING, "Life", TERM_WHITE, "", "LIVING", 10 },
    { SUMMON_HI_DRAGON_LIVING, "Dragons", TERM_WHITE, "", "HI_DRAGON_LIVING", 70 },
    { SUMMON_GOLEM, "Golems", TERM_WHITE, "", "GOLEM", 20 },
    { SUMMON_ELEMENTAL, "Elementals", TERM_WHITE, "", "ELEMENTAL", 15 },
    { SUMMON_VORTEX, "Vortices", TERM_WHITE, "", "VORTEX", 20 },
    { SUMMON_HYBRID, "Abominations", TERM_WHITE, "", "HYBRID", 15 },
    { SUMMON_BIRD, "Birds", TERM_WHITE, "", "BIRD", 20 },
    { SUMMON_KAMIKAZE, "Fanatics", TERM_WHITE, "", "KAMIKAZE", 10 },
    { SUMMON_KAMIKAZE_LIVING, "Fanatics", TERM_WHITE, "", "KAMIKAZE_LIVING", 15 },
    { SUMMON_MANES, "Manes", TERM_WHITE, "", "MANES", 5 },
    { SUMMON_LOUSE, "Lice", TERM_WHITE, "", "LOUSE", 1 },
    { SUMMON_GUARDIAN, "Dungeon Guardians", TERM_VIOLET, "", "GUARDIAN", 150 },
    { SUMMON_KNIGHT, "Knights", TERM_WHITE, "", "KNIGHT", 20 },
    { SUMMON_EAGLE, "Eagles", TERM_WHITE, "", "EAGLE", 25 },
    { SUMMON_PIRANHA, "Piranhas", TERM_WHITE, "", "PIRANHA", 5 },
    { SUMMON_ARMAGE_GOOD, "Holy Monsters", TERM_WHITE, "", "ARMAGE_GOOD", 40 },
    { SUMMON_ARMAGE_EVIL, "Foul Monsters", TERM_WHITE, "", "ARMAGE_EVIL", 40 },
    { SUMMON_SOFTWARE_BUG, "Software Bugs", TERM_WHITE, "", "SOFTWARE_BUG", 1 },
    { SUMMON_OLYMPIAN, "Olympians", TERM_WHITE, "", "OLYMPIAN", 150 },
    { SUMMON_RAT, "Rats", TERM_WHITE, "", "RAT", 2 },
    { SUMMON_BAT, "Bats", TERM_WHITE, "", "BAT", 5 },
    { SUMMON_WOLF, "Wolves", TERM_WHITE, "", "WOLF", 7 },
    { SUMMON_DREAD, "Dread", TERM_WHITE, "", "DREAD", 20 },
    { SUMMON_ZOMBIE, "Zombies", TERM_WHITE, "", "ZOMBIE", 10 },
    { SUMMON_SKELETON, "Skeletons", TERM_WHITE, "", "SKELETON", 20 },
    { SUMMON_GHOST, "Ghosts", TERM_WHITE, "", "GHOST", 20 },
    { SUMMON_VAMPIRE, "Vampires", TERM_WHITE, "", "VAMPIRE", 25 },
    { SUMMON_WIGHT, "Wights", TERM_WHITE, "", "WIGHT", 25 },
    { SUMMON_LICH, "Liches", TERM_WHITE, "", "LICH", 30 },
    { SUMMON_KRAKEN, "Kraken", TERM_WHITE, "", "KRAKEN", 50 },
    { SUMMON_THIEF, "Thieves", TERM_WHITE, "", "THIEF", 15 },
    { SUMMON_ENT, "Ents", TERM_WHITE, "", "ENT", 50 },
    { SUMMON_CAMELOT, "Camelot Knights", TERM_WHITE, "", "CAMELOT", 25 },
    { SUMMON_NIGHTMARE, "Nightmares", TERM_WHITE, "", "NIGHTMARE", 20 },
    { SUMMON_YEEK, "Yeeks", TERM_WHITE, "", "YEEK", 2 },
    { SUMMON_ORC, "Orcs", TERM_WHITE, "", "ORC", 5 },
    { SUMMON_DARK_ELF, "Dark Elves", TERM_WHITE, "", "DARK_ELF", 15 },
    { SUMMON_GIANT, "Giants", TERM_WHITE, "", "GIANT", 20 },
    { SUMMON_UNDEAD_SUMMONER, "Undead Summoners", TERM_WHITE, "", "UNDEAD_SUMMONER", 25 },
    { SUMMON_MATURE_DRAGON, "Mature Dragons", TERM_WHITE, "", "MATURE_DRAGON", 15 },
    { SUMMON_DRAGON_SUMMONER, "Dragon Summoners", TERM_WHITE, "", "DRAGON_SUMMONER", 25 },
    { SUMMON_CLUBBER_DEMON, "Clubber Demons", TERM_WHITE, "", "CLUBBER_DEMON", 20 },
    { SUMMON_BALROG, "Balrogs", TERM_WHITE, "", "BALROG", 35 },
    { SUMMON_DEMON_SUMMONER, "Demon Summoners", TERM_WHITE, "", "DEMON_SUMMONER", 35 },
    { SUMMON_ULTIMATE, "Ultimate", TERM_WHITE, "", "ULTIMATE", 100 },
    { SUMMON_HUMAN, "Human", TERM_WHITE, "", "HUMAN", 10 },
    { SUMMON_HORSE, "Horsies", TERM_WHITE, "", "HORSE", 10 },
    { SUMMON_MAGICAL, "Magical Monsters", TERM_WHITE, "", "MAGICAL", 15 },
    { SUMMON_TROLL, "Trolls", TERM_WHITE, "", "TROLL", 10 },
    { SUMMON_CHAPEL_GOOD, "Good Monsters", TERM_WHITE, "", "CHAPEL_GOOD", 25 },
    { SUMMON_CHAPEL_EVIL, "Evil Monsters", TERM_WHITE, "", "CHAPEL_EVIL", 25 },
    { SUMMON_RING_BEARER, "Ring Bearers", TERM_WHITE, "", "RING_BEARER", 20 },
    { SUMMON_ARCHER, "Archers", TERM_WHITE, "", "ARCHER", 10 },
    { SUMMON_MONK, "Monks", TERM_WHITE, "", "MONK", 20 },
    { SUMMON_MAGE, "Mages", TERM_WHITE, "", "MAGE", 20 },
    { SUMMON_SPECIAL, "Special", TERM_WHITE, "", "SPECIAL", 30 },
    { 0 }
};

parse_tbl_ptr parse_tbl_parse(parse_tbl_ptr tbl, cptr token)
{
    int i;
    for (i = 0;; i++)
    {
        parse_tbl_ptr p = &tbl[i];
        if (!p->name) return NULL;
        if (strcmp(p->parse, token) == 0) return p;
    }
}

parse_tbl_ptr parse_tbl_lookup(parse_tbl_ptr tbl, int id)
{
    int i;
    for (i = 0;; i++)
    {
        parse_tbl_ptr p = &tbl[i];
        if (!p->name) return NULL;
        if (p->id == id) return p;
    }
}

parse_tbl_ptr summon_type_parse(cptr token)
{
    return parse_tbl_parse(_summon_type_tbl, token);
}
        
parse_tbl_ptr summon_type_lookup(int id)
{
    return parse_tbl_lookup(_summon_type_tbl, id);
}

int parse_lookup_monster(cptr name, int options)
{
    int i;
    for (i = 1; i < max_r_idx; i++)
    {
        monster_race *r_ptr = &r_info[i];
        char buf[255];
        if (!r_ptr->name) continue;
        parse_prep_name(buf, r_name + r_ptr->name);
        if (strstr(buf, name))
        {
            if (trace_doc)
                doc_printf(trace_doc, "Mapping <color:B>%s</color> to <color:R>%s</color> (%d).\n", name, buf, i);
            return i;
        }
    }
    return 0;
}

/* MON(DRAGON, DEPTH+20)          Any dragon, 20 levels OoD
   MON(*, DEPTH+40)               Any monster, 40 levels OoD
   MON(ORC, NO_GROUP | HASTE)     A hasted orc loner at current depth
   MON(o, NO_GROUP | HASTE)       Ditto: You can use any valid d_char
   MON(black knight)              A Black Knight
   MON(^ent$)                     An Ent (not an Agent of the black market) */
static errr _parse_room_grid_monster(char **args, int arg_ct, room_grid_ptr grid, int options)
{
    if (arg_ct < 1 || arg_ct > 2)
    {
        msg_print("Invalid MON() directive: Syntax: MON(<which> [,<options>]).");
        return PARSE_ERROR_TOO_FEW_ARGUMENTS;
    }

    /* Which monster? Can be random, by index, by display character, or by summoning type.*/
    if (streq(args[0], "*"))
    {
        grid->flags |= ROOM_GRID_MON_RANDOM;
    }
    else if (_is_numeric(args[0]))
    {
        grid->monster = atoi(args[0]);
        if (!grid->monster)
        {
            msg_format("Error: %d is not a valid monster index (See r_idx.txt).", grid->monster);
            return PARSE_ERROR_GENERIC;
        }
    }
    else if (_is_d_char(args[0]))
    {
        grid->flags |= ROOM_GRID_MON_CHAR;
        grid->monster = args[0][0];
    }
    else
    {
        parse_tbl_ptr p = summon_type_parse(args[0]);
        if (p)
        {
            grid->flags |= ROOM_GRID_MON_TYPE;
            grid->monster = p->id;
        }
        else
        {
            grid->monster = parse_lookup_monster(args[0], options);
            if (!grid->monster)
            {
                msg_format("Error: Invalid monster specifier %s.", args[0]);
                return PARSE_ERROR_GENERIC;
            }
        }
    }

    /* Options */
    if (arg_ct >= 2)
    {
        char *flags[10];
        int   flag_ct = z_string_split(args[1], flags, 10, "|");
        int   i, n;

        for (i = 0; i < flag_ct; i++)
        {
            char* flag = flags[i];
            if (streq(flag, "NO_GROUP"))
            {
                grid->flags |= ROOM_GRID_MON_NO_GROUP;
            }
            else if (streq(flag, "NO_SLEEP"))
            {
                grid->flags |= ROOM_GRID_MON_NO_SLEEP;
            }
            else if (streq(flag, "NO_UNIQUE"))
            {
                grid->flags |= ROOM_GRID_MON_NO_UNIQUE;
            }
            else if (streq(flag, "FRIENDLY"))
            {
                grid->flags |= ROOM_GRID_MON_FRIENDLY;
            }
            else if (streq(flag, "HASTE"))
            {
                grid->flags |= ROOM_GRID_MON_HASTE;
            }
            else if (streq(flag, "CLONE"))
            {
                grid->flags |= ROOM_GRID_MON_CLONED;
            }
            else if (sscanf(flag, "DEPTH+%d", &n) == 1)
            {
                grid->monster_level = n;
            }
            else if (sscanf(flag, "%d%%", &n) == 1)
            {
                grid->mon_pct = n;
            }
            else
            {
                msg_format("Error: Invalid monster option %s.", flag);
                return PARSE_ERROR_GENERIC;
            }
        }
    }
    return 0;
}

int parse_lookup_kind(cptr name, int options)
{
    int i;
    for (i = 1; i < max_k_idx; i++)
    {
        object_kind *k_ptr = &k_info[i];
        char         buf[255];

        if (k_ptr->tval == TV_FOOD && k_ptr->sval <= SV_FOOD_MAX_MUSHROOM)
            strcpy(buf, "^mushroom of ");
        else if (k_ptr->tval == TV_POTION)
            strcpy(buf, "^potion of ");
        else if (k_ptr->tval == TV_SCROLL)
            strcpy(buf, "^scroll of ");
        else
            strcpy(buf, "^");

        _prep_name_aux(buf + strlen(buf), k_name + k_ptr->name);
        strcat(buf, "$");
        if (strstr(buf, name))
        {
            if (trace_doc)
                doc_printf(trace_doc, "Mapping kind <color:B>%s</color> to <color:R>%s</color> (%d).\n", name, buf, i);
            return i;
        }
    }
    return 0;
}

int parse_lookup_artifact(cptr name, int options)
{
    int i;
    for (i = 1; i < max_a_idx; i++)
    {
        artifact_type *a_ptr = &a_info[i];
        char           buf[255];
        if (!a_ptr->name) continue;
        if (have_flag(a_ptr->flags, OF_FULL_NAME))
            parse_prep_name(buf, a_name + a_ptr->name);
        else /* ART(bow of bard) matches "long bow of bard" */
        {    /* not "black arrow of bard" or "soft leather boots of bard" */
            int          k_idx = lookup_kind(a_ptr->tval, a_ptr->sval);
            object_kind *k_ptr = &k_info[k_idx];
            strcpy(buf, "^");
            _prep_name_aux(buf + strlen(buf), k_name + k_ptr->name);
            strcat(buf, " ");
            _prep_name_aux(buf + strlen(buf), a_name + a_ptr->name);
            strcat(buf, "$");
        }
        if (strstr(buf, name))
        {
            if (trace_doc)
                doc_printf(trace_doc, "Matching artifact <color:B>%s</color> to <color:R>%s</color> (%d).\n", name, buf, i);
            return i;
        }
    }
    return 0;
}

static errr _parse_room_grid_trap(char **args, int arg_ct, room_grid_ptr grid)
{
    int n;
    switch (arg_ct)
    {
    case 2:
        if (sscanf(args[1], "%d%%", &n) == 1)
            grid->trap_pct = n;
        /* vvvvvvvvv Fall Through vvvvvvvvvvvv */
    case 1:
        if (streq(args[0], "*"))
        {
            grid->flags |= ROOM_GRID_TRAP_RANDOM;
        }
        else
        {
            s16b trap = f_tag_to_index(args[0]);
            if (trap < 0)
            {
                msg_format("Error: Unknown Trap %s.", args[0]);
                return PARSE_ERROR_GENERIC;
            }
            grid->cave_trap = trap;
        }
        break;

    default:
        msg_print("Error: Invalid TRAP() directive. Syntax: TRAP(<which> [,<pct odds>]).");
        return PARSE_ERROR_TOO_FEW_ARGUMENTS;
    }
    return 0;
}

/* GRANITE
   FLOOR(ROOM | ICKY | GLOW) */
static errr _parse_room_grid_feature(char* name, char **args, int arg_ct, room_grid_ptr grid)
{
    s16b feat = f_tag_to_index(name);

    if (feat < 0)
    {
        msg_format("Error: Unknown Feature %s.", name);
        return PARSE_ERROR_GENERIC;
    }
    grid->cave_feat = feat;

    if (arg_ct > 2)
    {
        msg_print("Error: Invalid feature directive. Syntax: <Name>[(<flags> [,<special info>])].");
        return PARSE_ERROR_TOO_FEW_ARGUMENTS;
    }
    if (arg_ct >= 1)
    {
        char *flags[10];
        int   flag_ct = z_string_split(args[0], flags, 10, "|");
        int   i;

        for (i = 0; i < flag_ct; i++)
        {
            char* flag = flags[i];

            if (streq(flag, "ROOM"))
                grid->cave_info |= CAVE_ROOM;
            else if (streq(flag, "ICKY"))
                grid->cave_info |= CAVE_ICKY;
            else if (streq(flag, "GLOW"))
                grid->cave_info |= CAVE_GLOW;
            else if (streq(flag, "MARK"))
                grid->cave_info |= CAVE_MARK | CAVE_AWARE;
            else if (streq(flag, "INNER"))
                grid->cave_info |= CAVE_INNER;
            else if (streq(flag, "OUTER"))
                grid->cave_info |= CAVE_OUTER;
            else if (streq(flag, "TOWN"))
                grid->cave_info |= CAVE_TOWN;
            else if (streq(flag, "DUNGEON"))
                grid->cave_info |= CAVE_DUNGEON;
            else if (streq(flag, "ROAD"))
                grid->cave_info |= CAVE_ROAD;
            else if (streq(flag, "RIVER"))
                grid->cave_info |= CAVE_RIVER;
            else if (_is_numeric(flag)) /* QUEST_ENTER(1) or QUEST_ENTER(GLOW, 1) */
            {
                grid->flags |= ROOM_GRID_SPECIAL;
                grid->extra = atoi(flag);
            }
            else
            {
                msg_format("Error: Unknown Feature Option %s.", flag);
                return PARSE_ERROR_INVALID_FLAG;
            }
        }
    }
    if (arg_ct >= 2)
    {
        grid->flags |= ROOM_GRID_SPECIAL;
        if (_is_numeric(args[1]))
            grid->extra = atoi(args[1]);
        else if (grid->cave_info & CAVE_TOWN)
        {
            int id = towns_parse(args[1]);
            if (id)
                grid->extra = id;
            else
            {
                msg_format("Error: Unkown Town '%s'.", args[1]);
                return PARSE_ERROR_UNDEFINED_DIRECTIVE;
            }
        }
        else if (grid->cave_info & CAVE_QUEST)
        {
            quest_ptr quest = quests_parse(args[1]);
            if (quest)
                grid->extra = quest->id;
            else
            {
                msg_format("Error: Unkown Quest '%s'.", args[1]);
                return PARSE_ERROR_UNDEFINED_DIRECTIVE;
            }
        }
        else if (grid->cave_info & CAVE_DUNGEON)
        {
            grid->extra = dun_types_parse(args[1]);
            if (!grid->extra)
            {
                msg_format("Error: Unkown Dungeon '%s'.", args[1]);
                return PARSE_ERROR_UNDEFINED_DIRECTIVE;
            }
        }
        else
        {
            msg_format("Error: Unkown Feature Special '%s'.", args[1]);
            return PARSE_ERROR_UNDEFINED_DIRECTIVE;
        }
    }
    return 0;
}

/*     v---------- buf
   L:.:FLOOR(ROOM|ICKY):MON(DRAGON, 20):EGO(*)
   R:ART(ringil)
     ^-----buf
*/
errr parse_room_grid(char *buf, room_grid_ptr grid, int options)
{
    errr  result = 0;
    char *commands[10];
    int   command_ct = z_string_split(buf, commands, 10, ":");
    int   i;
    bool  found_feature = FALSE;

    for (i = 0; i < command_ct; i++)
    {
        char *command = commands[i];
        char *name;
        char *args[10];
        int   arg_ct = parse_args(command, &name, args, 10);

        if (arg_ct < 0)
        {
            msg_format("Error: Malformed argument %s. Missing )?", name);
            return PARSE_ERROR_GENERIC;
        }

        if (streq(name, "MON"))
        {
            result = _parse_room_grid_monster(args, arg_ct, grid, options);
            if (result) break;
        }
        else if (streq(name, "OBJ"))
        {
            result = obj_drop_parse_obj(args, arg_ct, &grid->object, options);
            if (result) break;
        }
        else if (streq(name, "EGO"))
        {
            result = obj_drop_parse_ego(args, arg_ct, &grid->object, options);
            if (result) break;
        }
        else if (streq(name, "ART"))
        {
            result = obj_drop_parse_art(args, arg_ct, &grid->object, options);
            if (result) break;
        }
        else if (streq(name, "TRAP") || streq(name, "SECRET"))
        {
            result = _parse_room_grid_trap(args, arg_ct, grid);
            if (result) break;
        }
        else
        {
            if (found_feature)
            {
                msg_format("Error: Unkown %s directive.", name);
                return PARSE_ERROR_GENERIC;
            }

            result = _parse_room_grid_feature(name, args, arg_ct, grid);
            if (result) break;
            found_feature = TRUE;
        }
    }

    return result;
}

static errr _parse_room_flags(char* buf, room_ptr room)
{
    char *flags[10];
    int   flag_ct = z_string_split(buf, flags, 10, "|");
    int   i;

    for (i = 0; i < flag_ct; i++)
    {
        char* flag = flags[i];

        if (streq(flag, "GOOD"))
            room->flags |= ROOM_THEME_GOOD;
        else if (streq(flag, "EVIL"))
            room->flags |= ROOM_THEME_EVIL;
        else if (streq(flag, "FRIENDLY"))
            room->flags |= ROOM_THEME_FRIENDLY;
        else if (streq(flag, "NIGHT"))
            room->flags |= ROOM_THEME_NIGHT;
        else if (streq(flag, "DAY"))
            room->flags |= ROOM_THEME_DAY;
        else if (streq(flag, "DEBUG"))
            room->flags |= ROOM_DEBUG;
        else if (streq(flag, "NO_ROTATE"))
            room->flags |= ROOM_NO_ROTATE;
        else if (streq(flag, "FORMATION"))
            room->flags |= ROOM_THEME_FORMATION;
        else if (streq(flag, "THEME_OBJECT"))
            room->flags |= ROOM_THEME_OBJECT;
        else
        {
            msg_format("Error: Invalid room flag %s.", flag);
            return PARSE_ERROR_INVALID_FLAG;
        }
    }
    return 0;
}

static errr _parse_room_type(char *buf, room_ptr room)
{
    char *zz[10];
    int   num = tokenize(buf, 10, zz, 0);

    if (num < 2 || num > 3)
    {
        msg_print("Error: Invalid T: line. Syntax is T:<Type>:<Subtype>[:<Flags>].");
        return PARSE_ERROR_TOO_FEW_ARGUMENTS;
    }

    if (streq(zz[0], "VAULT"))
    {
        room->type = ROOM_VAULT;
        if (streq(zz[1], "LESSER"))
            room->subtype = VAULT_LESSER;
        else if (streq(zz[1], "GREATER"))
            room->subtype = VAULT_GREATER;

        if (!room->subtype)
        {
            msg_format("Error: Unknown vault type %s.", zz[1]);
            return PARSE_ERROR_GENERIC;
        }
    }
    else if (streq(zz[0], "ROOM"))
    {
        room->type = ROOM_ROOM;
        if (streq(zz[1], "NORMAL"))
            room->subtype = ROOM_NORMAL;
        else if (streq(zz[1], "SHOP"))
            room->subtype = ROOM_SHOP;
        else if (streq(zz[1], "RECALL"))
            room->subtype = ROOM_RECALL;
        else if (streq(zz[1], "TRAVEL"))
            room->subtype = ROOM_TRAVEL;

        if (!room->subtype)
        {
            msg_format("Error: Unknown room type %s.", zz[1]);
            return PARSE_ERROR_GENERIC;
        }
    }
    else if (streq(zz[0], "QUEST"))
    {
        room->type = ROOM_QUEST;
        room->subtype = 0; /* TODO */
    }
    else if (streq(zz[0], "TOWN"))
    {
        room->type = ROOM_TOWN;
        room->subtype = 0; /* TODO */
    }
    else if (streq(zz[0], "WORLD"))
    {
        room->type = ROOM_WORLD;
        room->subtype = 0; /* TODO */
    }
    else if (streq(zz[0], "WILD") || streq(zz[0], "AMBUSH"))
    {
        static struct { cptr name; int type; } types[] = {
            {"WATER",    TERRAIN_DEEP_WATER}, /* TERRAIN_SHALLOW_WATER */
            {"SWAMP",    TERRAIN_SWAMP},
            {"GRASS",    TERRAIN_GRASS},      /* TERRAIN_DIRT, TERRAIN_DESERT */
            {"TREES",    TERRAIN_TREES},
            {"LAVA",     TERRAIN_DEEP_LAVA},  /* TERRAIN_SHALLOW_LAVA */
            {"MOUNTAIN", TERRAIN_MOUNTAIN},
            { 0, 0 }
        };
        int j;
        if (streq(zz[0], "AMBUSH"))
            room->type = ROOM_AMBUSH;
        else
            room->type = ROOM_WILDERNESS;
        for (j = 0; ; j++)
        {
            if (!types[j].name) break;
            if (streq(types[j].name, zz[1]))
            {
                room->subtype = types[j].type;
                break;
            }
        }

        if (!room->subtype)
        {
            msg_format("Error: Unknown wilderness type %s.", zz[1]);
            return PARSE_ERROR_GENERIC;
        }
    }

    if (!room->type)
    {
        msg_format("Error: Unknown room type %s.", zz[0]);
        return PARSE_ERROR_GENERIC;
    }

    if (num == 3)
    {
        errr rc = _parse_room_flags(zz[2], room);
        if (rc) return rc;
    }
    return 0;
}

static errr parse_v_info(char *buf, int options)
{
    /* Current entry */
    static room_ptr room = NULL;

    /* Default letters for all rooms and vaults */
    if (buf[0] == 'L' && buf[1] == ':' && !room)
    {
        int rc;
        room_grid_ptr letter = malloc(sizeof(room_grid_t));
        memset(letter, 0, sizeof(room_grid_t));
        letter->letter = buf[2];
        rc = parse_room_grid(buf + 4, letter, options);
        if (!rc) int_map_add(room_letters, letter->letter, letter);
        else free(letter);
        if (rc) return rc;
    }
    /* N:Name */
    else if (buf[0] == 'N')
    {
        char *zz[10];
        int   num = tokenize(buf + 2, 10, zz, 0);

        if (num != 1 || !*zz[0])
        {
            msg_print("Error: Invalid N: line. Syntax: N:<Name>.");
            return PARSE_ERROR_TOO_FEW_ARGUMENTS;
        }

        error_idx = vec_length(room_info);
        if (options & INIT_DEBUG)
        {
            room_free(room);
            room = room_alloc(zz[0]);
        }
        else
        {
            room = room_alloc(zz[0]);
            vec_push(room_info, room);
        }
        room->id = error_idx;
    }

    /* There better be a current room */
    else if (!room)
    {
        msg_print("Error: Missing N: line for new room template.");
        return PARSE_ERROR_MISSING_RECORD_HEADER;
    }
    else
        return parse_room_line(room, buf, options);

    /* Success */
    return 0;
}

errr init_v_info(int options)
{
    if (room_info) vec_free(room_info); /* double initialization?? */
    room_info = vec_alloc((vec_free_f)room_free);
    if (room_letters) int_map_free(room_letters);
    room_letters = int_map_alloc(free);
    return parse_edit_file("v_info.txt", parse_v_info, options);
}

/*
 * Initialize the "s_info" array, by parsing an ascii "template" file
 */
errr parse_s_info(char *buf, header *head)
{
    int i;

    /* Current entry */
    static skill_table *s_ptr = NULL;


    /* Process 'N' for "New/Number/Name" */
    if (buf[0] == 'N')
    {
        /* Get the index */
        if (_is_numeric(buf + 2))
            i = atoi(buf+2);
        else
        {
            i = plr_class_parse(buf+2);
            if (i == CLASS_NONE)
            {
                msg_format("Unkown class: %s", buf+2);
                return PARSE_ERROR_UNDEFINED_DIRECTIVE;
            }
        }

        /* Verify information */
        if (i >= head->info_num) return (2);

        /* Save the index */
        error_idx = i;

        /* Point at the "info" */
        s_ptr = &s_info[i];
    }

    /* There better be a current s_ptr */
    else if (!s_ptr) return (3);

    /* Process 'W' for "Weapon exp" */
    else if (buf[0] == 'W')
    {
        int tval, sval, start, max;
        const s16b exp_conv_table[] =
        {
            WEAPON_EXP_UNSKILLED, WEAPON_EXP_BEGINNER, WEAPON_EXP_SKILLED,
            WEAPON_EXP_EXPERT, WEAPON_EXP_MASTER
        };

        /* Scan for the values */
        if (4 != sscanf(buf+2, "%d:%d:%d:%d",
                &tval, &sval, &start, &max)) return (1);

        if (start < EXP_LEVEL_UNSKILLED || start > EXP_LEVEL_MASTER
            || max < EXP_LEVEL_UNSKILLED || max > EXP_LEVEL_MASTER) return (8);

        /* Save the values */
        s_ptr->w_start[tval][sval] = exp_conv_table[start];
        s_ptr->w_max[tval][sval] = exp_conv_table[max];
    }

    /* Process 'S' for "Skill exp" */
    else if (buf[0] == 'S')
    {
        int num, start, max;

        /* Scan for the values */
        if (3 != sscanf(buf+2, "%d:%d:%d",
                &num, &start, &max)) return (1);

        if (start < WEAPON_EXP_UNSKILLED || start > WEAPON_EXP_MASTER
            || max < WEAPON_EXP_UNSKILLED || max > WEAPON_EXP_MASTER) return (8);

        /* Save the values */
        s_ptr->s_start[num] = start;
        s_ptr->s_max[num] = max;
    }


    /* Oops */
    else return (6);

    /* Success */
    return (0);
}


/*
 * Initialize the "m_info" array, by parsing an ascii "template" file
 */
errr parse_m_info(char *buf, header *head)
{
    int i;

    char *s;

    /* Current entry */
    static player_magic *m_ptr = NULL;

    /* ---Hack--- */
    static int realm, magic_idx = 0, readable = 0;


    /* Process 'N' for "New/Number/Name" */
    if (buf[0] == 'N')
    {
        /* Get the index */
        if (_is_numeric(buf + 2))
            i = atoi(buf+2);
        else
        {
            i = plr_class_parse(buf+2);
            if (i == CLASS_NONE)
            {
                msg_format("Unkown class: %s", buf+2);
                return PARSE_ERROR_UNDEFINED_DIRECTIVE;
            }
        }

        /* Verify information */
        if (i >= head->info_num) return (2);

        /* Save the index */
        error_idx = i;

        /* Point at the "info" */
        m_ptr = &m_info[i];
    }

    /* There better be a current m_ptr */
    else if (!m_ptr) return (3);

    /* Process 'I' for "Info" (one line only) */
    else if (buf[0] == 'I')
    {
        char *book, *stat;
        int xtra, type, first, weight;

        /* Find the colon before the name */
        s = my_strchr(buf+2, ':');

        /* Verify that colon */
        if (!s) return (1);

        /* Nuke the colon, advance to the name */
        *s++ = '\0';

        book = buf+2;

        if (streq(book, "SORCERY")) m_ptr->spell_book = TV_SORCERY_BOOK;
        else if (streq(book, "LIFE")) m_ptr->spell_book = TV_LIFE_BOOK;
        else if (streq(book, "NECROMANCY")) m_ptr->spell_book = TV_NECROMANCY_BOOK;
        else if (streq(book, "MUSIC")) m_ptr->spell_book = TV_MUSIC_BOOK;
        else if (streq(book, "HISSATSU")) m_ptr->spell_book = TV_HISSATSU_BOOK;
        else if (streq(book, "RAGE")) m_ptr->spell_book = TV_RAGE_BOOK;
        else if (streq(book, "NONE")) m_ptr->spell_book = 0;
        else return (5);

        stat = s;

        /* Find the colon before the name */
        s = my_strchr(s, ':');

        /* Verify that colon */
        if (!s) return (1);

        /* Nuke the colon, advance to the name */
        *s++ = '\0';

        if (streq(stat, "STR")) m_ptr->spell_stat = A_STR;
        else if (streq(stat, "INT")) m_ptr->spell_stat = A_INT;
        else if (streq(stat, "WIS")) m_ptr->spell_stat = A_WIS;
        else if (streq(stat, "DEX")) m_ptr->spell_stat = A_DEX;
        else if (streq(stat, "CON")) m_ptr->spell_stat = A_CON;
        else if (streq(stat, "CHR")) m_ptr->spell_stat = A_CHR;
        else return (5);


        /* Scan for the values */
        if (4 != sscanf(s, "%x:%d:%d:%d",
                (uint *)&xtra, &type, &first, &weight))    return (1);

        m_ptr->spell_xtra = xtra;
        m_ptr->spell_type = type;
        m_ptr->spell_first = first;
        m_ptr->spell_weight = weight;
    }


    /* Process 'R' for "Realm" (one line only) */
    else if (buf[0] == 'R')
    {
        /* Scan for the values */
        if (2 != sscanf(buf+2, "%d:%d",
                &realm, &readable)) return (1);

        magic_idx = 0;
    }

    else if (buf[0] == 'T')
    {
        int level, mana, fail, exp;

        if (!readable) return (1);
        /* Scan for the values */
        if (4 != sscanf(buf+2, "%d:%d:%d:%d",
                &level, &mana, &fail, &exp)) return (1);

        m_ptr->info[realm][magic_idx].slevel = level;
        m_ptr->info[realm][magic_idx].smana = mana;
        m_ptr->info[realm][magic_idx].sfail = fail;
        m_ptr->info[realm][magic_idx].sexp = exp;
        magic_idx ++;
    }


    /* Oops */
    else return (6);

    /* Success */
    return (0);
}


/*
 * Grab one flag from a textual string
 */
static errr grab_one_flag(u32b *flags, cptr names[], cptr what)
{
    int i;

    /* Check flags */
    for (i = 0; i < 32; i++)
    {
        if (streq(what, names[i]))
        {
            *flags |= (1L << i);
            return 0;
        }
    }

    return -1;
}


/*
 * Grab one flag in an feature_type from a textual string
 */
static errr grab_one_feat_flag(feature_type *f_ptr, cptr what)
{
    int i;

    /* Check flags */
    for (i = 0; i < FF_FLAG_MAX; i++)
    {
        if (streq(what, f_info_flags[i]))
        {
            add_flag(f_ptr->flags, i);
            return 0;
        }
    }

    /* Oops */
    msg_format("Unknown feature flag '%s'.", what);

    /* Error */
    return PARSE_ERROR_GENERIC;
}


/*
 * Grab an action in an feature_type from a textual string
 */
static errr grab_one_feat_action(feature_type *f_ptr, cptr what, int count)
{
    int i;

    /* Check flags */
    for (i = 0; i < FF_FLAG_MAX; i++)
    {
        if (streq(what, f_info_flags[i]))
        {
            f_ptr->state[count].action = i;
            return 0;
        }
    }

    /* Oops */
    msg_format("Unknown feature action '%s'.", what);

    /* Error */
    return PARSE_ERROR_GENERIC;
}


/*
 * Initialize the "f_info" array, by parsing an ascii "template" file
 */
errr parse_f_info(char *buf, header *head)
{
    int i;

    char *s, *t;

    /* Current entry */
    static feature_type *f_ptr = NULL;


    /* Process 'N' for "New/Number/Name" */
    if (buf[0] == 'N')
    {
        /* Find the colon before the name */
        s = my_strchr(buf+2, ':');

        if (s)
        {
            /* Nuke the colon, advance to the name */
            *s++ = '\0';
        }

        /* Get the index */
        i = atoi(buf+2);

        /* Verify information */
        if (i <= error_idx) return (4);

        /* Verify information */
        if (i >= head->info_num) return (2);

        /* Save the index */
        error_idx = i;

        /* Point at the "info" */
        f_ptr = &f_info[i];

        /* Tag name is given */
        if (s)
        {
            /* Store the tag */
            if (!add_tag(&f_ptr->tag, head, s)) return (7);
        }

        /* Default "mimic" */
        f_ptr->mimic = i;

        /* Default "destroyed state" -- if not specified */
        f_ptr->destroyed = i;

        /* Default "states" */
        for (i = 0; i < MAX_FEAT_STATES; i++) f_ptr->state[i].action = FF_FLAG_MAX;
    }

    /* There better be a current f_ptr */
    else if (!f_ptr) return (3);

    else if (buf[0] == 'E')
    {
        /* Acquire the Text */
        s = buf+2;

        /* Store the name */
        if (!add_name(&f_ptr->name, head, s)) return (7);
    }


    /* Process 'M' for "Mimic" (one line only) */
    else if (buf[0] == 'M')
    {
        s16b offset;

        if (!add_tag(&offset, head, buf + 2)) return PARSE_ERROR_OUT_OF_MEMORY;

        /* Record a fake tag index */
        f_ptr->mimic = -offset;
    }


    /* Process 'G' for "Graphics" (one line only) */
    else if (buf[0] == 'G')
    {
        int j;
        byte s_attr;
        char char_tmp[F_LIT_MAX];

        /* Paranoia */
        if (buf[1] != ':') return (1);
        if (!buf[2]) return (1);
        if (buf[3] != ':') return (1);
        if (!buf[4]) return (1);

        /* Extract the char */
        char_tmp[F_LIT_STANDARD] = buf[2];

        /* Extract the color */
        s_attr = color_char_to_attr(buf[4]);

        /* Paranoia */
        if (s_attr > 127) return (1);

        /* Save the standard values */
        f_ptr->d_attr[F_LIT_STANDARD] = s_attr;
        f_ptr->d_char[F_LIT_STANDARD] = char_tmp[F_LIT_STANDARD];

        /* Is this feature supports lighting? */
        if (buf[5] == ':')
        {
            /* G:c:a:LIT (default) */
            apply_default_feat_lighting(f_ptr->d_attr, f_ptr->d_char);

            /* G:c:a:lc:la:dc:da */
            if (!streq(buf + 6, "LIT"))
            {
                char attr_lite_tmp[F_LIT_MAX - F_LIT_NS_BEGIN];

                if ((F_LIT_MAX - F_LIT_NS_BEGIN) * 2 != sscanf(buf + 6, "%c:%c:%c:%c",
                    &char_tmp[F_LIT_LITE], &attr_lite_tmp[F_LIT_LITE - F_LIT_NS_BEGIN],
                    &char_tmp[F_LIT_DARK], &attr_lite_tmp[F_LIT_DARK - F_LIT_NS_BEGIN])) return 1;
                if (buf[F_LIT_MAX * 4 + 1]) return 1;

                for (j = F_LIT_NS_BEGIN; j < F_LIT_MAX; j++)
                {
                    switch (attr_lite_tmp[j - F_LIT_NS_BEGIN])
                    {
                    case '*':
                        /* Use default lighting */
                        break;
                    case '-':
                        /* No lighting support */
                        f_ptr->d_attr[j] = f_ptr->d_attr[F_LIT_STANDARD];
                        break;
                    default:
                        /* Extract the color */
                        f_ptr->d_attr[j] = color_char_to_attr(attr_lite_tmp[j - F_LIT_NS_BEGIN]);
                        if (f_ptr->d_attr[j] > 127) return 1;
                        break;
                    }
                    f_ptr->d_char[j] = char_tmp[j];
                }
            }
        }
        else if (!buf[5])
        {
            for (j = F_LIT_NS_BEGIN; j < F_LIT_MAX; j++)
            {
                f_ptr->d_attr[j] = s_attr;
                f_ptr->d_char[j] = char_tmp[F_LIT_STANDARD];
            }
        }
        else return 1;
    }

    /* Hack -- Process 'F' for flags */
    else if (buf[0] == 'F')
    {
        /* Parse every entry textually */
        for (s = buf + 2; *s; )
        {
            /* Find the end of this entry */
            for (t = s; *t && (*t != ' ') && (*t != '|'); ++t) /* loop */;

            /* Nuke and skip any dividers */
            if (*t)
            {
                *t++ = '\0';
                while (*t == ' ' || *t == '|') t++;
            }

            /* XXX XXX XXX Hack -- Read feature subtype */
            if (1 == sscanf(s, "SUBTYPE_%d", &i))
            {
                /* Extract a "subtype" */
                f_ptr->subtype =  i;

                /* Start at next entry */
                s = t;

                /* Continue */
                continue;
            }

            /* XXX XXX XXX Hack -- Read feature power */
            if (1 == sscanf(s, "POWER_%d", &i))
            {
                /* Extract a "power" */
                f_ptr->power =  i;

                /* Start at next entry */
                s = t;

                /* Continue */
                continue;
            }

            /* Parse this entry */
            if (0 != grab_one_feat_flag(f_ptr, s)) return (PARSE_ERROR_INVALID_FLAG);

            /* Start the next entry */
            s = t;
        }
    }

    /* Process 'W' for "More Info" (one line only) */
    else if (buf[0] == 'W')
    {
        int priority;

        /* Scan for the value */
        if (1 != sscanf(buf+2, "%d", &priority)) return (PARSE_ERROR_GENERIC);

        /* Save the value */
        f_ptr->priority = priority;
    }

    /* Process 'K' for "States" (up to four lines + default (which cannot be last)) */
    else if (buf[0] == 'K')
    {
        s16b offset;

        /* Find the next empty state slot (if any) */
        for (i = 0; i < MAX_FEAT_STATES; i++) if (f_ptr->state[i].action == FF_FLAG_MAX) break;

        /* Oops, no more slots */
        if (i == MAX_FEAT_STATES) return PARSE_ERROR_GENERIC;

        /* Analyze the first field */
        for (s = t = buf+2; *t && (*t != ':'); t++) /* loop */;

        /* Terminate the field (if necessary) */
        if (*t == ':') *t++ = '\0';

        /* Is this default entry? */
        if (streq(s, "DESTROYED"))
        {
            if (!add_tag(&offset, head, t)) return PARSE_ERROR_OUT_OF_MEMORY;

            /* Record a fake tag index */
            f_ptr->destroyed = -offset;
        }
        else
        {
            /* Reset */
            f_ptr->state[i].action = 0;

            /* Parse this entry */
            if (0 != grab_one_feat_action(f_ptr, s, i)) return PARSE_ERROR_INVALID_FLAG;

            if (!add_tag(&offset, head, t)) return PARSE_ERROR_OUT_OF_MEMORY;

            /* Record a fake tag index */
            f_ptr->state[i].result = -offset;
        }
    }

    /* Oops */
    else    return (6);

    /* Success */
    return (0);
}


/*
 * Convert a fake tag to a real feat index
 */
s16b f_tag_to_index(cptr str)
{
    u16b i;

    /* Search for real index corresponding to this fake tag */
    for (i = 0; i < f_head.info_num; i++)
    {
        if (streq(f_tag + f_info[i].tag, str))
        {
            /* Return the index */
            return (s16b)i;
        }
    }

    /* Not found */
    return -1;
}


/*
 * Search for real index corresponding to this fake tag
 */
static void search_real_feat(s16b *feat)
{
    int i;

    /* Don't convert non-fake tag */
    if (*feat >= 0) return;

    /* Search for real index corresponding to this fake tag */
    for (i = 0; i < f_head.info_num; i++)
    {
        if ((-(*feat)) == f_info[i].tag)
        {
            /* Record real index */
            *feat = (s16b)i;
            return;
        }
    }

    /* Undefined tag */
    msg_format("%s is undefined.", f_tag + (-(*feat)));
}


/*
 * Retouch fake tags of f_info
 */
void retouch_f_info(header *head)
{
    int i;

    /* Convert fake tags to real feat indices */
    for (i = 0; i < head->info_num; i++)
    {
        feature_type *f_ptr = &f_info[i];
        int j;

        search_real_feat(&f_ptr->mimic);

        search_real_feat(&f_ptr->destroyed);

        for (j = 0; j < MAX_FEAT_STATES; j++) search_real_feat(&f_ptr->state[j].result);
    }
}


/*
 * Grab one flag in an object_kind from a textual string
 */
static errr grab_one_kind_flag(object_kind *k_ptr, cptr what)
{
    of_info_ptr info = of_parse_name(what);
    if (info)
    {
        add_flag(k_ptr->flags, info->id);
        return 0;
    }

    if (grab_one_flag(&k_ptr->gen_flags, k_info_gen_flags, what) == 0)
        return 0;

    /* Oops */
    msg_format("Unknown object flag '%s'.", what);


    /* Error */
    return (1);
}


/*
 * Initialize the "k_info" array, by parsing an ascii "template" file
 */
errr parse_k_info(char *buf, header *head)
{
    char *s, *t;

    /* Current entry */
    static object_kind *k_ptr = NULL;
    static int k_idx = 0;

    /* Process 'N' for "New/Number/Name" */
    if (buf[0] == 'N')
    {
        char *flavor;

        /* Find the colon before the name */
        s = my_strchr(buf+2, ':');

        /* Verify that colon */
        if (!s) return (1);

        /* Nuke the colon, advance to the name */
        *s++ = '\0';

        /* Get the index */
        if (strcmp(buf+2, "*") == 0)
            k_idx++;
        else
            k_idx = atoi(buf+2);

        /* Verify information */
        if (k_idx <= error_idx) return (4);

        /* Verify information */
        if (k_idx >= head->info_num) return (2);

        /* Save the index */
        error_idx = k_idx;

        /* Point at the "info" */
        k_ptr = &k_info[k_idx];
        k_ptr->idx = k_idx;

        /* Paranoia -- require a name */
        if (!*s) return (1);

        /* Find the colon before the flavor */
        flavor = my_strchr(s, ':');

        /* Verify that colon */
        if (flavor)
        {
            /* Nuke the colon, advance to the flavor */
            *flavor++ = '\0';

            /* Store the flavor */
            if (!add_name(&k_ptr->flavor_name, head, flavor)) return (7);
        }

        /* Store the name */
        if (!add_name(&k_ptr->name, head, s)) return (7);
    }

    /* There better be a current k_ptr */
    else if (!k_ptr) return (3);
    /* From Vanilla: M:P:XdY to control object stacks. P is
       the probabilty (1 to 100) for a stack and XdY is how
       many get rolled up. Replaces TRG_STACK and obj_make_pile(). */
    else if (buf[0] == 'M')
    {
        int p, x, y;

        if (3 != sscanf(buf+2, "%d:%dd%d",
                &p, &x, &y)) return (1);

        k_ptr->stack_chance = p;
        k_ptr->stack_dice = x;
        k_ptr->stack_sides = y;
    }
    else if (buf[0] == 'E')
    {
        /* First E: line is required and defines the activation. */
        if (!k_ptr->activation.type)
        {
            errr rc = effect_parse(buf + 2, &k_ptr->activation);
            if (rc) return rc;
            add_flag(k_ptr->flags, OF_ACTIVATE); /* for object lore */
        }
        /* Second E: line is optional and describes the activation. */
        else if (!k_ptr->activation_msg)
        {
            s = buf+2;
            if (!add_text(&k_ptr->activation_msg, head, s, FALSE)) return (7);
        }
        else
            return 1;
    }

    /* Process 'D' for "Description" */
    else if (buf[0] == 'D')
    {
        /* Acquire the text */
        s = buf+2;

        /* Store the text */
        if (!add_text(&k_ptr->text, head, s, TRUE)) return (7);
    }

    /* Process 'G' for "Graphics" (one line only) */
    else if (buf[0] == 'G')
    {
        char sym;
        byte tmp;

        /* Paranoia */
        if (buf[1] != ':') return (1);
        if (!buf[2]) return (1);
        if (buf[3] != ':') return (1);
        if (!buf[4]) return (1);

        /* Extract the char */
        sym = buf[2];

        /* Extract the attr */
        tmp = color_char_to_attr(buf[4]);

        /* Paranoia */
        if (tmp > 127) return (1);

        /* Save the values */
        k_ptr->d_attr = tmp;
        k_ptr->d_char = sym;
    }

    /* Process 'I' for "Info" (one line only) */
    else if (buf[0] == 'I')
    {
        char *zz[4];
        int   num = tokenize(buf + 2, 4, zz, TOKENIZE_CHECKQUOTE | TOKENIZE_NO_SLASH | TOKENIZE_NO_ESCAPE);

        if (num < 2) return PARSE_ERROR_TOO_FEW_ARGUMENTS;
        if (_is_numeric(zz[0]))
            k_ptr->tval = atoi(zz[0]);
        else
        {
            tv_info_ptr info = tv_parse_name(zz[0]);
            if (!info)
            {
                msg_format("Unknown tval=%s", zz[0]);
                return 1;
            }
            k_ptr->tval = info->id;
        }
        k_ptr->sval = atoi(zz[1]);
        if (num >= 3)
            k_ptr->pval = atoi(zz[2]);
        if (num >= 4)
            k_ptr->stack = atoi(zz[3]);
    }

    /* Process 'W' for "More Info" (one line only) */
    else if (buf[0] == 'W')
    {
        int level, wgt, max_level;
        int cost;

        /* Scan for the values */
        if (4 != sscanf(buf+2, "%d:%d:%d:%d",
                &level, &max_level, &wgt, &cost)) return (1);

        /* Save the values */
        k_ptr->level = level;
        k_ptr->max_level = max_level;
        k_ptr->weight = wgt;
        k_ptr->cost = cost;
    }

    /* Process 'A' for "Allocation" (one line only) */
    else if (buf[0] == 'A')
    {
        int i;

        /* XXX XXX XXX Simply read each number following a colon */
        for (i = 0, s = buf+1; s && (s[0] == ':') && s[1]; ++i)
        {
                /* Default chance */
            k_ptr->chance[i] = 1;

                /* Store the attack damage index */
            k_ptr->locale[i] = atoi(s+1);

                /* Find the slash */
            t = my_strchr(s+1, '/');

                /* Find the next colon */
            s = my_strchr(s+1, ':');

                /* If the slash is "nearby", use it */
            if (t && (!s || t < s))
            {
                int chance = atoi(t+1);
                if (chance > 0) k_ptr->chance[i] = chance;
            }
        }
    }

    /* Hack -- Process 'P' for "power" and such */
    else if (buf[0] == 'P')
    {
        int ac, hd1, hd2, th, td, ta, mult = 0;

        if (k_ptr->tval == TV_BOW)
        {
            if (6 != sscanf(buf+2, "%d:x%d.%d:%d:%d:%d",
                    &ac, &hd1, &hd2, &th, &td, &ta)) return (1);
            mult = hd1 * 100 + hd2; /* x3.25 -> 325 (alas, x3.2 -> 302 so use x3.20 instead) */
            hd1 = 0;
            hd2 = 0;
        }
        else
        {
            if (6 != sscanf(buf+2, "%d:%dd%d:%d:%d:%d",
                    &ac, &hd1, &hd2, &th, &td, &ta)) return (1);
        }
        k_ptr->ac = ac;
        k_ptr->dd = hd1;
        k_ptr->ds = hd2;
        k_ptr->mult = mult;
        k_ptr->to_h = th;
        k_ptr->to_d = td;
        k_ptr->to_a =  ta;
    }

    /* Hack -- Process 'F' for flags */
    else if (buf[0] == 'F')
    {
        /* Parse every entry textually */
        for (s = buf + 2; *s; )
        {
                /* Find the end of this entry */
            for (t = s; *t && (*t != ' ') && (*t != '|'); ++t) /* loop */;

                /* Nuke and skip any dividers */
            if (*t)
            {
                *t++ = '\0';
                while (*t == ' ' || *t == '|') t++;
            }

                /* Parse this entry */
            if (0 != grab_one_kind_flag(k_ptr, s)) return (5);

                /* Start the next entry */
            s = t;
        }
    }


    /* Oops */
    else return (6);


    /* Success */
    return (0);
}


/*
 * Grab one flag in an artifact_type from a textual string
 */
static errr grab_one_artifact_flag(artifact_type *a_ptr, cptr what)
{
    of_info_ptr info = of_parse_name(what);
    if (info)
    {
        add_flag(a_ptr->flags, info->id);
        return 0;
    }

    if (grab_one_flag(&a_ptr->gen_flags, k_info_gen_flags, what) == 0)
        return 0;

    /* Oops */
    msg_format("Unknown artifact flag '%s'.", what);


    /* Error */
    return (1);
}




/*
 * Initialize the "a_info" array, by parsing an ascii "template" file
 */
errr parse_a_info(char *buf, header *head)
{
    int i;

    char *s, *t;

    /* Current entry */
    static artifact_type *a_ptr = NULL;


    /* Process 'N' for "New/Number/Name" */
    if (buf[0] == 'N')
    {
        /* Find the colon before the name */
        s = my_strchr(buf+2, ':');

            /* Verify that colon */
        if (!s) return (1);

        /* Nuke the colon, advance to the name */
        *s++ = '\0';

        /* Paranoia -- require a name */
        if (!*s) return (1);

        /* Get the index */
        i = atoi(buf+2);

        /* Verify information */
        if (i < error_idx) return (4);

        /* Verify information */
        if (i >= head->info_num) return (2);

        /* Save the index */
        error_idx = i;

        /* Point at the "info" */
        a_ptr = &a_info[i];

        /* Ignore everything */
        add_flag(a_ptr->flags, OF_IGNORE_ACID);
        add_flag(a_ptr->flags, OF_IGNORE_ELEC);
        add_flag(a_ptr->flags, OF_IGNORE_FIRE);
        add_flag(a_ptr->flags, OF_IGNORE_COLD);

        /* Store the name */
        if (!add_name(&a_ptr->name, head, s)) return (7);
    }

    /* There better be a current a_ptr */
    else if (!a_ptr) return (3);

    else if (buf[0] == 'E')
    {
        /* First E: line is required and defines the activation. */
        if (!a_ptr->activation.type)
        {
            errr rc = effect_parse(buf + 2, &a_ptr->activation);
            if (rc) return rc;
            add_flag(a_ptr->flags, OF_ACTIVATE); /* for object lore */
        }
        /* Second E: line is optional and describes the activation. */
        else if (!a_ptr->activation_msg)
        {
            s = buf+2;
            if (!add_text(&a_ptr->activation_msg, head, s, FALSE)) return (7);
        }
        else
            return 1;
    }

    /* Process 'D' for "Description" */
    else if (buf[0] == 'D')
    {
        /* Acquire the text */
        s = buf+2;

        /* Store the text */
        if (!add_text(&a_ptr->text, head, s, TRUE)) return (7);
    }


    /* Process 'I' for "Info" (one line only) */
    else if (buf[0] == 'I')
    {
        char *zz[3];
        int   num = tokenize(buf + 2, 3, zz, TOKENIZE_CHECKQUOTE | TOKENIZE_NO_SLASH | TOKENIZE_NO_ESCAPE);

        if (num < 2) return PARSE_ERROR_TOO_FEW_ARGUMENTS;
        if (_is_numeric(zz[0]))
            a_ptr->tval = atoi(zz[0]);
        else
        {
            tv_info_ptr info = tv_parse_name(zz[0]);
            if (!info)
            {
                msg_format("Unknown tval=%s", zz[0]);
                return 1;
            }
            a_ptr->tval = info->id;
        }
        a_ptr->sval = atoi(zz[1]);
        if (num >= 3)
            a_ptr->pval = atoi(zz[2]);
    }

    /* Process 'W' for "More Info" (one line only) */
    else if (buf[0] == 'W')
    {
        int level, rarity, wgt;
        int cost;

        /* Scan for the values */
        if (4 != sscanf(buf+2, "%d:%d:%d:%d",
                &level, &rarity, &wgt, &cost)) return (1);

        /* Save the values */
        a_ptr->level = level;
        a_ptr->rarity = rarity;
        a_ptr->weight = wgt;
        a_ptr->cost = cost;
    }

    /* Hack -- Process 'P' for "power" and such */
    else if (buf[0] == 'P')
    {
        int ac, hd1, hd2, th, td, ta, mult = 0;

        if (a_ptr->tval == TV_BOW)
        {
            if (6 != sscanf(buf+2, "%d:x%d.%d:%d:%d:%d",
                    &ac, &hd1, &hd2, &th, &td, &ta)) return (1);
            mult = hd1 * 100 + hd2; /* x3.25 -> 325 (alas, x3.2 -> 302 so use x3.20 instead) */
            hd1 = 0;
            hd2 = 0;
        }
        else
        {
            if (6 != sscanf(buf+2, "%d:%dd%d:%d:%d:%d",
                    &ac, &hd1, &hd2, &th, &td, &ta)) return (1);
        }
        a_ptr->ac = ac;
        a_ptr->dd = hd1;
        a_ptr->ds = hd2;
        a_ptr->mult = mult;
        a_ptr->to_h = th;
        a_ptr->to_d = td;
        a_ptr->to_a =  ta;
    }

    /* Hack -- Process 'F' for flags */
    else if (buf[0] == 'F')
    {
        /* Parse every entry textually */
        for (s = buf + 2; *s; )
        {
                /* Find the end of this entry */
            for (t = s; *t && (*t != ' ') && (*t != '|'); ++t) /* loop */;

                /* Nuke and skip any dividers */
            if (*t)
            {
                *t++ = '\0';
                while ((*t == ' ') || (*t == '|')) t++;
            }

                /* Parse this entry */
            if (0 != grab_one_artifact_flag(a_ptr, s)) return (5);

                /* Start the next entry */
            s = t;
        }
    }


    /* Oops */
    else return (6);


    /* Success */
    return (0);
}


/*
 * Grab one flag in a ego-item_type from a textual string
 */
static bool grab_one_ego_item_flag(ego_type *e_ptr, cptr what)
{
    of_info_ptr info = of_parse_name(what);
    if (info)
    {
        add_flag(e_ptr->flags, info->id);
        return 0;
    }

    if (grab_one_flag(&e_ptr->gen_flags, k_info_gen_flags, what) == 0)
        return 0;

    /* Oops */
    msg_format("Unknown ego-item flag '%s'.", what);


    /* Error */
    return (1);
}

static bool grab_one_ego_type_flag(ego_type *e_ptr, cptr what)
{
    if (streq(what, "AMMO")) e_ptr->type |= EGO_TYPE_AMMO;
    else if (streq(what, "WEAPON")) e_ptr->type |= EGO_TYPE_WEAPON;
    else if (streq(what, "SHIELD")) e_ptr->type |= EGO_TYPE_SHIELD;
    else if (streq(what, "BOW")) e_ptr->type |= EGO_TYPE_BOW;
    else if (streq(what, "RING")) e_ptr->type |= EGO_TYPE_RING;
    else if (streq(what, "AMULET")) e_ptr->type |= EGO_TYPE_AMULET;
    else if (streq(what, "LITE")) e_ptr->type |= EGO_TYPE_LITE;
    else if (streq(what, "BODY_ARMOR")) e_ptr->type |= EGO_TYPE_BODY_ARMOR;
    else if (streq(what, "CLOAK")) e_ptr->type |= EGO_TYPE_CLOAK;
    else if (streq(what, "HELMET")) e_ptr->type |= EGO_TYPE_HELMET;
    else if (streq(what, "GLOVES")) e_ptr->type |= EGO_TYPE_GLOVES;
    else if (streq(what, "BOOTS")) e_ptr->type |= EGO_TYPE_BOOTS;
    else if (streq(what, "DIGGER")) e_ptr->type |= EGO_TYPE_DIGGER;
    else if (streq(what, "CROWN")) e_ptr->type |= EGO_TYPE_CROWN;
    else if (streq(what, "HARP")) e_ptr->type |= EGO_TYPE_HARP;
    else if (streq(what, "ROBE")) e_ptr->type |= EGO_TYPE_ROBE;
    else if (streq(what, "SPECIAL")) e_ptr->type |= EGO_TYPE_SPECIAL;
    else if (streq(what, "DEVICE")) e_ptr->type |= EGO_TYPE_DEVICE;
    else if (streq(what, "DRAGON_ARMOR")) e_ptr->type |= EGO_TYPE_DRAGON_ARMOR;
    else if (streq(what, "QUIVER")) e_ptr->type |= EGO_TYPE_QUIVER;
    else
    {
        msg_format("Unknown ego type flag: '%s'.", what);
        return ERROR_UNKOWN_FAILURE;
    }
    return ERROR_SUCCESS;
}

/*
 * Initialize the "e_info" array, by parsing an ascii "template" file
 */
errr parse_e_info(char *buf, header *head)
{
    int i;

    char *s, *t;

    /* Current entry */
    static ego_type *e_ptr = NULL;


    /* Just before the first record */
    error_idx = -1;

    /* Just before the first line */
    error_line = -1;


    /* N:1:of Free Action */
    if (buf[0] == 'N')
    {
        char *zz[3];
        int   num = tokenize(buf + 2, 2, zz, 0);

        if (num != 2) return PARSE_ERROR_TOO_FEW_ARGUMENTS;

        /* Unique Index */
        i = atoi(zz[0]);
        if (i < error_idx) return 4;
        if (i >= head->info_num) return 2;

        error_idx = i;
        e_ptr = &e_info[i];
        e_ptr->id = i;

        /* Description */
        if (!add_name(&e_ptr->name, head, zz[1])) return 7;
    }

    /* There better be a current e_ptr */
    else if (!e_ptr) return (3);
    else if (buf[0] == 'E')
    {
        errr rc = effect_parse(buf + 2, &e_ptr->activation);
        if (rc) return rc;
        add_flag(e_ptr->flags, OF_ACTIVATE); /* for object lore */
    }
    /* A:Name | Name | ... names go in art_names_high (The 'A' list)
     * B:Name | Name | ...  names go in art_names     (The 'B' list) */
    else if (buf[0] == 'A')
    {
        char *names[20];
        int   ct = z_string_split(buf + 2, names, 20, "|");
        int   i;
        if (!e_ptr->art_names_high)
            e_ptr->art_names_high = vec_alloc(free);
        for (i = 0; i < ct; i++)
        {
            cptr  name = names[i];
            int   len = strlen(name);

            if (len)
            {
                char *copy = malloc(len + 1);
                strcpy(copy, name);
                vec_add(e_ptr->art_names_high, copy);
            }
        }
    }
    else if (buf[0] == 'B')
    {
        char *names[20];
        int   ct = z_string_split(buf + 2, names, 20, "|");
        int   i;
        if (!e_ptr->art_names)
            e_ptr->art_names = vec_alloc(free);
        for (i = 0; i < ct; i++)
        {
            cptr  name = names[i];
            int   len = strlen(name);

            if (len)
            {
                char *copy = malloc(len + 1);
                strcpy(copy, name);
                vec_add(e_ptr->art_names, copy);
            }
        }
    }
    /* W:MinDepth:MaxDepth:Rarity
       W:30:*:32                  */
    else if (buf[0] == 'W')
    {
        char *zz[4];
        int   num = tokenize(buf + 2, 4, zz, 0);

        if (num != 3) return PARSE_ERROR_TOO_FEW_ARGUMENTS;

        e_ptr->level = atoi(zz[0]);
        if (strcmp(zz[1], "*") == 0)
            e_ptr->max_level = 0;
        else
            e_ptr->max_level = atoi(zz[1]);
        e_ptr->rarity = atoi(zz[2]);
    }

    /* Hack -- Process 'C' for "creation" */
    else if (buf[0] == 'C')
    {
        int th, td, ta, pv;

        /* Scan for the values */
        if (4 != sscanf(buf+2, "%d:%d:%d:%d",
                &th, &td, &ta, &pv)) return (1);

        e_ptr->max_to_h = th;
        e_ptr->max_to_d = td;
        e_ptr->max_to_a = ta;
        e_ptr->max_pval = pv;
    }
    /* T:HELMET | SHIELD | BODY_ARMOR | CLOAK
       T:WEAPON | AMMO
       T:RING etc. */
    else if (buf[0] == 'T')
    {
        /* Parse every entry textually */
        for (s = buf + 2; *s; )
        {
                /* Find the end of this entry */
            for (t = s; *t && (*t != ' ') && (*t != '|'); ++t) /* loop */;

                /* Nuke and skip any dividers */
            if (*t)
            {
                *t++ = '\0';
                while ((*t == ' ') || (*t == '|')) t++;
            }

                /* Parse this entry */
            if (0 != grab_one_ego_type_flag(e_ptr, s)) return (5);

                /* Start the next entry */
            s = t;
        }
    }

    /* Hack -- Process 'F' for flags */
    else if (buf[0] == 'F')
    {
        /* Parse every entry textually */
        for (s = buf + 2; *s; )
        {
                /* Find the end of this entry */
            for (t = s; *t && (*t != ' ') && (*t != '|'); ++t) /* loop */;

                /* Nuke and skip any dividers */
            if (*t)
            {
                *t++ = '\0';
                while ((*t == ' ') || (*t == '|')) t++;
            }

                /* Parse this entry */
            if (0 != grab_one_ego_item_flag(e_ptr, s)) return (5);

                /* Start the next entry */
            s = t;
        }
    }
    /* Process 'D' for "Description" */
    else if (buf[0] == 'D')
    {
        /* Acquire the text */
        s = buf+2;

        /* Store the text */
        if (!add_text(&e_ptr->text, head, s, TRUE)) return (7);
    }

    /* Oops */
    else return (6);

    /* Success */
    return (0);
}


/*
 * Grab one (basic) flag in a monster_race from a textual string
 */
static errr grab_one_basic_flag(monster_race *r_ptr, cptr what)
{
    if (grab_one_flag(&r_ptr->flags1, r_info_flags1, what) == 0)
        return 0;

    if (grab_one_flag(&r_ptr->flags2, r_info_flags2, what) == 0)
        return 0;

    if (grab_one_flag(&r_ptr->flags3, r_info_flags3, what) == 0)
        return 0;

    if (grab_one_flag(&r_ptr->flags7, r_info_flags7, what) == 0)
        return 0;

    if (grab_one_flag(&r_ptr->flags8, r_info_flags8, what) == 0)
        return 0;

    if (grab_one_flag(&r_ptr->flags9, r_info_flags9, what) == 0)
        return 0;

    if (grab_one_flag(&r_ptr->flagsr, r_info_flagsr, what) == 0)
        return 0;

    /* Oops */
    msg_format("Unknown monster flag '%s'.", what);


    /* Failure */
    return (1);
}

/*
 * b_info for monster body types
 */
static cptr b_info_slots[] =
{
    "NONE",
    "GLOVES",
    "WEAPON_SHIELD",
    "RING",
    "BOW",
    "AMULET",
    "LITE",
    "BODY_ARMOR",
    "CLOAK",
    "BOOTS",
    "HELMET",
    "ANY",
    "WEAPON",
    "CAPTURE_BALL",
    "QUIVER",
};

errr parse_b_info(char *buf, header *head)
{
    int i;
    char *s, *t;

    /* Current entry */
    static equip_template_ptr b_ptr = NULL;


    /* Process 'N' for "New/Number/Name" */
    if (buf[0] == 'N')
    {
        /* Find the colon before the name */
        s = my_strchr(buf+2, ':');

        /* Verify that colon */
        if (!s) return (1);

        /* Nuke the colon, advance to the name */
        *s++ = '\0';

        /* Paranoia -- require a name */
        if (!*s) return (1);

        /* Get the index */
        i = atoi(buf+2);

        /* Verify information */
        if (i < error_idx) return (4);

        /* Verify information */
        if (i >= head->info_num) return (2);

        /* Save the index */
        error_idx = i;

        /* Point at the "info" */
        b_ptr = &b_info[i];

        /* Store the name */
        if (!add_name(&b_ptr->name, head, s)) return (7);
    }

    /* There better be a current r_ptr */
    else if (!b_ptr) return (3);

    else if (buf[0] == 'S')
    {
        int n1;

        i = ++b_ptr->max;
        if (i > EQUIP_MAX) return PARSE_ERROR_OUT_OF_BOUNDS;

        /* Analyze the first field */
        for (s = t = buf+2; *t && (*t != ':'); t++) /* loop */;

        /* Terminate the field (if necessary) */
        if (*t == ':') *t++ = '\0';

        /* Analyze the slot type */
        for (n1 = 0; n1 < EQUIP_SLOT_MAX; n1++)
        {
            if (streq(s, b_info_slots[n1]))
            {
                b_ptr->slots[i].type = n1;
                break;
            }
        }

        /* Invalid slot type */
        if (!b_ptr->slots[i].type) return (1);

        /* Analyze the second field */
        for (s = t; *t && (*t != ':'); t++) /* loop */;

        /* Terminate the field (if necessary) */
        if (*t == ':') *t++ = '\0';

        if (!add_tag(&b_ptr->slots[i].tag, head, s)) return (7);

        /* Analyze the third field */
        if (*t)
            b_ptr->slots[i].hand = atoi(t);
    }
    /* Oops */
    else return 6;


    /* Success */
    return 0;
}

/* BITE(60) or perhaps just BITE
 * Also: BITE(60, x3) or BITE(x5) form multiple blows per round. */
static errr parse_mon_blow_method(char *command, mon_blow_ptr blow)
{
    char *name;
    char *args[10];
    int   arg_ct = parse_args(command, &name, args, 10);
    int   i;
    mon_blow_info_ptr info;

    if (arg_ct < 0)
    {
        msg_format("Error: Malformed argument %s. Missing )?", name);
        return PARSE_ERROR_GENERIC;
    }

    blow->method = _get_r_blow_method(name);
    if (!blow->method)
    {
        msg_format("Error: Unknown monster blow method %s.", name);
        return PARSE_ERROR_UNDEFINED_DIRECTIVE;
    }
    info = mon_blow_info_lookup(blow->method);
    if (!info) /* paranoia */
    {
        msg_format("Error: _mon_blow_info table is missing this method: %s.", name);
        return PARSE_ERROR_UNDEFINED_DIRECTIVE;
    }
    blow->flags = info->flags;
    blow->blows = 100;

    for (i = 0; i < arg_ct; i++)
    {
        char arg[100], sentinel = '~', check;
        int  n;
        char x;

        sprintf(arg, "%s%c", args[i], sentinel);
        if (2 == sscanf(arg, "x%d%c", &n, &check) && check == sentinel)
        {
            blow->blows = n*100;
        }
        else if (3 == sscanf(arg, "x%d.%c%c", &n, &x, &check) && check == sentinel)
        {
            /* stupid parser for now: x1.2->120; x3.9->390; x1.25->unsupported */
            blow->blows = n*100;
            blow->blows += (x - '0')*10;
        }
        else if (2 == sscanf(arg, "x%d%%%c", &n, &check) && check == sentinel)
        {
            /* 80% can just store 80 */
            blow->blows = n;
        }
        else if (2 == sscanf(arg, "%dlbs%c", &n, &check) && check == sentinel)
        {
            blow->weight = n*10;
        }
        else if (2 == sscanf(arg, "%d%c", &n, &check) && check == sentinel)
        {
            blow->power = n;
        }
        else
        {
            blow->name = monk_verify_table(args[i]);
            if (!blow->name)
            {
                msg_format("Error: Unknown argument %s.", args[i]);
                return PARSE_ERROR_GENERIC;
            }
        }
    }
    return 0;
}

/* historically in mbe_info ... */
static int _default_blow_power(int effect)
{
    switch (effect)
    {
    case RBE_HURT:
    case RBE_SHATTER:
        return 60;
    case RBE_LOSE_STR:
    case RBE_LOSE_INT:
    case RBE_LOSE_WIS:
    case RBE_LOSE_DEX:
    case RBE_LOSE_CON:
    case RBE_LOSE_CHR:
    case GF_ACID:
        return 0;
    case GF_ELEC:
    case GF_FIRE:
    case GF_COLD:
        return 10;
    case RBE_LOSE_ALL:
        return 2;
    case GF_DISENCHANT:
        return 20;
    case RBE_DRAIN_CHARGES:
        return 15;
    }
    return 5;
}

/*   V------------------- buf
 * B:BITE:SUPERHURT:15d10   <===== The old syntax (no longer supported)
 * B:BITE(60):HURT(15d10):HURT(15d10, 20%):STUN(5d5, 10%)  <=== New syntax, multiple effects
 *   ^------------------- buf
 */
errr parse_mon_blow(char *buf, mon_blow_ptr blow)
{
    errr  rc = 0;
    char *commands[10];
    int   command_ct = z_string_split(buf, commands, 10, ":");
    int   i, j, dd, ds, xtra, pct; /* sscanf probably wants int*, not byte* */

    if (command_ct < 1)
        return PARSE_ERROR_TOO_FEW_ARGUMENTS;

    rc = parse_mon_blow_method(commands[0], blow);
    if (rc) return rc;

    for (i = 1; i < command_ct; i++)
    {
        char *command = commands[i];
        char *name;
        char *args[10];
        int   arg_ct = parse_args(command, &name, args, 10);
        mon_effect_t effect = {0};

        if (arg_ct < 0)
        {
            msg_format("Error: Malformed argument %s. Missing )?", name);
            return PARSE_ERROR_GENERIC;
        }

        effect.effect = _get_r_blow_effect(name);
        if (!effect.effect)
        {
            msg_format("Error: Unknown monster blow effect %s.", name);
            return PARSE_ERROR_UNDEFINED_DIRECTIVE;
        }
        if (arg_ct > 2)
            return PARSE_ERROR_TOO_FEW_ARGUMENTS;
        for (j = 0; j < arg_ct; j++)
        {
            char arg[100], sentinel = '~', check;
            /* sscanf tricks learned from vanilla ... */
            sprintf(arg, "%s%c", args[j], sentinel);
            
            if (2 == sscanf(arg, "%d%%%c", &pct, &check) && check == sentinel)
                effect.pct = MAX(0, MIN(100, pct));
            else if (4 == sscanf(arg, "%dd%d+%d%c", &dd, &ds, &xtra, &check) && check == sentinel)
            {
                effect.dice.dd = MAX(0, MIN(100, dd)); /* 100d100+255 max */
                effect.dice.ds = MAX(0, MIN(100, ds));
                effect.dice.base = MAX(0, MIN(255, xtra));
            }
            else if (3 == sscanf(arg, "%dd%d%c", &dd, &ds, &check) && check == sentinel)
            {
                effect.dice.dd = MAX(0, MIN(100, dd)); /* 100d100 max */
                effect.dice.ds = MAX(0, MIN(100, ds));
                effect.dice.base = 0;
            }
            else if (2 == sscanf(arg, "%d%c", &xtra, &check) && check == sentinel)
            {
                effect.dice.dd = 0;
                effect.dice.ds = 0;
                effect.dice.base = MAX(0, MIN(255, xtra));
            }
            else
            {
                msg_format("Error: Unknown argument %s.", args[j]);
                return PARSE_ERROR_GENERIC;
            }
        }
        /* For convenience, use the implied power on the first effect for the
         * overall blow power. For example: B:BITE:HURT(7d7) == B:BITE(60):HURT(7d7) */
        if (i == 1 && !blow->power)
            blow->power = _default_blow_power(effect.effect);
        mon_blow_push_effect_aux(blow, &effect);
    }

    return rc;
}
/*   V---buf
 * A:INERT(3d4, 20%):FIRE(3d4):GRAVITY(3d4, 25%)
 * Multiple auras on a single line. Assume all only a single A: line per monster.
 * Up to MAX_MON_AURAS allowed. Effects are GF_**** (see list above).
 */
static errr parse_mon_auras(char *buf, mon_race_ptr r_ptr)
{
    errr  rc = 0;
    char *commands[10];
    int   command_ct = z_string_split(buf, commands, 10, ":");
    int   i, j, dd, ds, xtra, pct; /* sscanf probably wants int*, not byte* */

    if (command_ct > MAX_MON_AURAS)
        return PARSE_ERROR_TOO_FEW_ARGUMENTS;

    for (i = 0; i < command_ct; i++)
    {
        char *command = commands[i];
        char *name;
        char *args[10];
        int   arg_ct = parse_args(command, &name, args, 10);
        mon_effect_ptr aura = &r_ptr->auras[i];

        if (arg_ct < 0)
        {
            msg_format("Error: Malformed argument %s. Missing )?", name);
            return PARSE_ERROR_GENERIC;
        }
        if (aura->effect)
        {
            msg_print("Duplicate A: line. Put all auras on a single line, please!");
            return PARSE_ERROR_GENERIC;
        }

        aura->effect = _get_gf_type(name);
        if (!aura->effect)
        {
            msg_format("Error: Unknown monster aura effect %s.", name);
            return PARSE_ERROR_UNDEFINED_DIRECTIVE;
        }
        if (arg_ct > 2)
            return PARSE_ERROR_TOO_FEW_ARGUMENTS;
        for (j = 0; j < arg_ct; j++)
        {
            char arg[100], sentinel = '~', check;
            /* sscanf tricks learned from vanilla ... */
            sprintf(arg, "%s%c", args[j], sentinel);
            
            if (2 == sscanf(arg, "%d%%%c", &pct, &check) && check == sentinel)
                aura->pct = MAX(0, MIN(100, pct));
            else if (4 == sscanf(arg, "%dd%d+%d%c", &dd, &ds, &xtra, &check) && check == sentinel)
            {
                aura->dice.dd = MAX(0, MIN(100, dd)); /* 100d100+255 max */
                aura->dice.ds = MAX(0, MIN(100, ds));
                aura->dice.base = MAX(0, MIN(255, xtra));
            }
            else if (3 == sscanf(arg, "%dd%d%c", &dd, &ds, &check) && check == sentinel)
            {
                aura->dice.dd = MAX(0, MIN(100, dd)); /* 100d100 max */
                aura->dice.ds = MAX(0, MIN(100, ds));
                aura->dice.base = 0;
            }
            else if (2 == sscanf(arg, "%d%c", &xtra, &check) && check == sentinel)
            {
                aura->dice.dd = 0;
                aura->dice.ds = 0;
                aura->dice.base = MAX(0, MIN(255, xtra));
            }
            else
            {
                msg_format("Error: Unknown argument %s.", args[j]);
                return PARSE_ERROR_GENERIC;
            }
        }
    }

    return rc;
}
static errr parse_mon_flags(char *buf, mon_race_ptr race)
{
    errr  rc = 0;
    char *tokens[20];
    int   token_ct = z_string_split(buf, tokens, 20, "|");
    int   i, j;

    for (i = 0; i < token_ct; i++)
    {
        char *token = tokens[i];

        if (!strlen(token)) continue;
        if (prefix(token, "FRIENDS"))
        {
            char *name;
            char *args[10];
            int   arg_ct = parse_args(token, &name, args, 10);
            race->flags1 |= RF1_FRIENDS;
            if (!streq(name, "FRIENDS")) return 5; /* eg FRIENDS_I_LIKE s/b an error */
            if (arg_ct > 2)
                return PARSE_ERROR_TOO_FEW_ARGUMENTS; /* s/FEW/MANY */
            for (j = 0; j < arg_ct; j++)  /* XXX parsing logic duplicated above ... */
            {
                char arg[100], sentinel = '~', check;
                int  dd, ds, pct;
                sprintf(arg, "%s%c", args[j], sentinel);
                
                if (2 == sscanf(arg, "%d%%%c", &pct, &check) && check == sentinel)
                    race->pack_pct = MAX(0, MIN(100, pct));
                else if (3 == sscanf(arg, "%dd%d%c", &dd, &ds, &check) && check == sentinel)
                {
                    race->pack_dice = MAX(0, MIN(100, dd)); /* 100d100 max */
                    race->pack_sides = MAX(0, MIN(100, ds));
                }
                else
                {
                    msg_format("Error: Unknown argument %s.", args[j]);
                    return PARSE_ERROR_GENERIC;
                }
            }
        }
        else if (0 != grab_one_basic_flag(race, token)) return 5;
    }
    return rc;
}
static errr parse_mon_spells(char *buf, mon_race_ptr race)
{
    errr  rc = 0;
    char *tokens[10];
    int   token_ct = z_string_split(buf, tokens, 10, "|");
    int   i, n;

    assert(race->spells);

    for (i = 0; i < token_ct; i++)
    {
        char *token = tokens[i];

        if (!strlen(token)) continue;

        if (1 == sscanf(token, "1_IN_%d", &n))
            race->spells->freq = 100 / n;
        else if (1 == sscanf(token, "FREQ_%d", &n))
            race->spells->freq = n;
        else
        {
            rc = mon_spells_parse(race->spells, race->level, token);
            if (rc)
                return rc;
        }
    }
    return rc;
}
/*
 * Initialize the "r_info" array, by parsing an ascii "template" file
 */
errr parse_r_info(char *buf, header *head)
{
    int i;

    char *s;

    /* Current entry */
    static monster_race *r_ptr = NULL;


    /* Process 'N' for "New/Number/Name" */
    if (buf[0] == 'N')
    {
        /* Find the colon before the name */
        s = my_strchr(buf+2, ':');

            /* Verify that colon */
        if (!s) return (1);

        /* Nuke the colon, advance to the name */
        *s++ = '\0';

        /* Paranoia -- require a name */
        if (!*s) return (1);

        /* Get the index */
        i = atoi(buf+2);

        /* Verify information */
        if (i < error_idx) return (4);

        /* Verify information */
        if (i >= head->info_num) return (2);

        /* Save the index */
        error_idx = i;

        /* Point at the "info" */
        r_ptr = &r_info[i];
        r_ptr->id = i;
        r_ptr->blows = vec_alloc((vec_free_f)mon_blow_free);

        /* Store the name */
        if (!add_name(&r_ptr->name, head, s)) return (7);
    }

    /* There better be a current r_ptr */
    else if (!r_ptr) return (3);

    /* Process 'D' for "Description" */
    else if (buf[0] == 'D')
    {
        /* Acquire the text */
        s = buf+2;

        /* Store the text */
        if (!add_text(&r_ptr->text, head, s, TRUE)) return (7);
    }

    /* Process 'G' for "Graphics" (one line only) */
    else if (buf[0] == 'G')
    {
        char sym;
        byte tmp;

        /* Paranoia */
        if (buf[1] != ':') return (1);
        if (!buf[2]) return (1);
        if (buf[3] != ':') return (1);
        if (!buf[4]) return (1);

        /* Extract the char */
        sym = buf[2];

        /* Extract the attr */
        tmp = color_char_to_attr(buf[4]);

        /* Paranoia */
        if (tmp > 127) return (1);

        /* Save the values */
        r_ptr->d_char = sym;
        r_ptr->d_attr = tmp;
    }

    /* Process 'I' for "Info" (one line only) */
    else if (buf[0] == 'I')
    {
        int spd, hp1, hp2, aaf, ac, slp, wgt;

        /* Scan for the other values */
        if (7 != sscanf(buf+2, "%d:%dd%d:%d:%d:%d:%d",
                &spd, &hp1, &hp2, &aaf, &ac, &slp, &wgt)) return (1);

        /* Save the values */
        r_ptr->speed = spd;
        r_ptr->hdice = MAX(hp1, 1);
        r_ptr->hside = MAX(hp2, 1);
        r_ptr->aaf = aaf;
        r_ptr->ac = ac;
        r_ptr->sleep = slp;
        r_ptr->weight = wgt;
    }
    /* P:<Code>:... for The Possessor */
    else if (buf[0] == 'P')
    {
        char *zz[33];
        int   num = tokenize(buf + 2, 33, zz, 0);

        if (num < 2) return PARSE_ERROR_TOO_FEW_ARGUMENTS;

        if (strcmp(zz[0], "Copy") == 0)
        {
            int idx, i;
            if (num < 2) return PARSE_ERROR_TOO_FEW_ARGUMENTS;

            /* P:Copy:<r_idx> */
            if (num == 2)
            {
                idx = atoi(zz[1]);
                r_ptr->body = r_info[idx].body;
            }
            /* P:Copy:Yeek:Tourist */
            else if (num == 3)
            {
                idx = plr_race_parse(zz[1]);
                if (0 <= idx && idx < MAX_RACES) /* XXX don't lookup monsters or mimic forms */
                {
                    race_t *race_ptr = get_race_aux(idx, 0);
                    for (i = 0; i < MAX_STATS; i++)
                        r_ptr->body.stats[i] = race_ptr->stats[i];
                    r_ptr->body.skills = race_ptr->skills;
                    r_ptr->body.extra_skills = race_ptr->extra_skills;
                    r_ptr->body.life = race_ptr->life;
                    r_ptr->body.infra = race_ptr->infra;
                }
                else
                    return PARSE_ERROR_OUT_OF_BOUNDS;

                idx = plr_class_parse(zz[2]);
                if (idx != CLASS_NONE)
                {
                    plr_class_ptr pc = plr_class_aux(idx, 0);
                    r_ptr->body.class_idx = idx;
                    for (i = 0; i < MAX_STATS; i++)
                        r_ptr->body.stats[i] += pc->stats[i];
                    skills_add(&r_ptr->body.skills, &pc->skills);
                    skills_add(&r_ptr->body.extra_skills, &pc->extra_skills);
                    r_ptr->body.life = r_ptr->body.life * pc->life / 100;
                    if (pc->hooks.caster_info)
                        r_ptr->body.spell_stat = (s16b)pc->hooks.caster_info()->which_stat;
                    else
                        r_ptr->body.spell_stat = A_NONE;
                }
                else
                    return PARSE_ERROR_OUT_OF_BOUNDS;
            }
            else
                return PARSE_ERROR_TOO_FEW_ARGUMENTS;
        }
        else if (strcmp(zz[0], "Speed") == 0)
        {
            int sp = atoi(zz[1]);
            if (num != 2) return PARSE_ERROR_TOO_FEW_ARGUMENTS;
            if (sp > 90) /* Hack: 110 -> +0, 100 -> -10, etc. This is old school.*/
                sp = sp - 110;
            r_ptr->body.speed = sp;
        }
        else if (strcmp(zz[0], "SpellStat") == 0)
        {
            if (num != 2) return PARSE_ERROR_TOO_FEW_ARGUMENTS;
            if (streq(zz[1], "Str")) r_ptr->body.spell_stat = A_STR;
            else if (streq(zz[1], "Int")) r_ptr->body.spell_stat = A_INT;
            else if (streq(zz[1], "Wis")) r_ptr->body.spell_stat = A_WIS;
            else if (streq(zz[1], "Dex")) r_ptr->body.spell_stat = A_DEX;
            else if (streq(zz[1], "Con")) r_ptr->body.spell_stat = A_CON;
            else if (streq(zz[1], "Chr")) r_ptr->body.spell_stat = A_CHR;
            else return PARSE_ERROR_OUT_OF_BOUNDS;
        }
        /* P:Stats:<Str>:<Int>:<Wis>:<Dex>:<Con>:<Chr> */
        else if (strcmp(zz[0], "Stats") == 0)
        {
            if (num < 7) return PARSE_ERROR_TOO_FEW_ARGUMENTS;
            r_ptr->body.stats[A_STR] += atoi(zz[1]);
            r_ptr->body.stats[A_INT] += atoi(zz[2]);
            r_ptr->body.stats[A_WIS] += atoi(zz[3]);
            r_ptr->body.stats[A_DEX] += atoi(zz[4]);
            r_ptr->body.stats[A_CON] += atoi(zz[5]);
            r_ptr->body.stats[A_CHR] += atoi(zz[6]);
        }
        /* P:Skills:<Dis>:<Dev>:<Sav>:<Stl>:<Srh>:<Fos>:<Thn>:<Thb> */
        else if (strcmp(zz[0], "Skills") == 0)
        {
            if (num < 9) return PARSE_ERROR_TOO_FEW_ARGUMENTS;
            r_ptr->body.skills.dis += atoi(zz[1]);
            r_ptr->body.skills.dev += atoi(zz[2]);
            r_ptr->body.skills.sav += atoi(zz[3]);
            r_ptr->body.skills.stl += atoi(zz[4]);
            r_ptr->body.skills.srh += atoi(zz[5]);
            r_ptr->body.skills.fos += atoi(zz[6]);
            r_ptr->body.skills.thn += atoi(zz[7]);
            r_ptr->body.skills.thb += atoi(zz[8]);
        }
        else if (strcmp(zz[0], "ExtraSkills") == 0)
        {
            if (num < 9) return PARSE_ERROR_TOO_FEW_ARGUMENTS;
            r_ptr->body.extra_skills.dis += atoi(zz[1]);
            r_ptr->body.extra_skills.dev += atoi(zz[2]);
            r_ptr->body.extra_skills.sav += atoi(zz[3]);
            r_ptr->body.extra_skills.stl += atoi(zz[4]);
            r_ptr->body.extra_skills.srh += atoi(zz[5]);
            r_ptr->body.extra_skills.fos += atoi(zz[6]);
            r_ptr->body.extra_skills.thn += atoi(zz[7]);
            r_ptr->body.extra_skills.thb += atoi(zz[8]);
        }
        else if (strcmp(zz[0], "Life") == 0)
        {
            int life;
            if (num < 2) return PARSE_ERROR_TOO_FEW_ARGUMENTS;
            life = atoi(zz[1]);
            if (!r_ptr->body.life)
                r_ptr->body.life = life;
            else
                r_ptr->body.life = r_ptr->body.life * life / 100;
        }
        else if (strcmp(zz[0], "Infra") == 0)
        {
            if (num < 2) return PARSE_ERROR_TOO_FEW_ARGUMENTS;
            r_ptr->body.infra += atoi(zz[1]);
        }
        else if (strcmp(zz[0], "Body") == 0)
        {
            int i;
            bool found = FALSE;
            if (num < 2) return PARSE_ERROR_TOO_FEW_ARGUMENTS;

            for (i = 0; i < max_b_idx; i++)
            {
                if (streq(b_name + b_info[i].name, zz[1]))
                {
                    r_ptr->body.body_idx = i;
                    found = TRUE;
                    break;
                }
            }

            if (!found) return PARSE_ERROR_OUT_OF_BOUNDS;
        }
        else if (strcmp(zz[0], "Blows") == 0)
        {
            if (num < 2) return PARSE_ERROR_TOO_FEW_ARGUMENTS;
            if (num > 4) return PARSE_ERROR_TOO_FEW_ARGUMENTS; /* s/FEW/MANY */
            switch (num)
            {
            case 4: r_ptr->body.blows_calc.mult = atoi(zz[3]);
            case 3: r_ptr->body.blows_calc.wgt = atoi(zz[2]);
            case 2: r_ptr->body.blows_calc.max = atoi(zz[1]);
            }
        }
        else
            return PARSE_ERROR_UNDEFINED_DIRECTIVE;
    }
    /* Process 'W' for "More Info" (one line only) */
    else if (buf[0] == 'W')
    {
        char *commands[10];
        int   command_ct = z_string_split(buf + 2, commands, 10, ":");

        if (command_ct < 3) return 1;
        switch (command_ct)
        {
        case 5: r_ptr->next_r_idx = atoi(commands[4]);
        case 4: r_ptr->next_exp = atoi(commands[3]);
        case 3: r_ptr->mexp = atoi(commands[2]);
        case 2: r_ptr->rarity = atoi(commands[1]);
        case 1:
            if (_is_numeric(commands[0])) r_ptr->level = atoi(commands[0]);
            else
            {
                char *command = commands[0];
                char *name;
                char *args[10];
                int   arg_ct = parse_args(command, &name, args, 10);
                int   lvl, max_lvl;

                /* "Angband(1)" or "Angband(10 to 20)" */
                if (arg_ct == 1)
                {
                    r_ptr->dun_type_id = dun_types_parse(name);
                    if (!r_ptr->dun_type_id)
                    {
                        msg_format("<color:r>Error:</color> Unknown Dungeon '%s' (Case Sensitive)", name);
                        return 1;
                    }
                    if (_is_numeric(args[0]))
                        r_ptr->level = atoi(args[0]);
                    else if (sscanf(args[0], "%d to %d", &lvl, &max_lvl) == 2)
                    {
                        r_ptr->level = lvl;
                        r_ptr->max_level = max_lvl;
                    }
                    else return 1;
                }
                /* "1" or "10 to 20" */
                else
                {
                    if (_is_numeric(name))
                        r_ptr->level = atoi(name);
                    else if (sscanf(name, "%d to %d", &lvl, &max_lvl) == 2)
                    {
                        r_ptr->level = lvl;
                        r_ptr->max_level = max_lvl;
                    }
                    else return 1;
                }
            }
            break;
        }
    }

    /* Process 'B' for "Blows" */
    else if (buf[0] == 'B')
    {
        errr rc = 0;
        mon_blow_ptr blow = mon_blow_alloc(RBM_NONE);

        rc = parse_mon_blow(buf + 2, blow);
        if (rc) { mon_blow_free(blow); return rc; }
        vec_add(r_ptr->blows, blow);
    }

    /* Process 'A' for "Auras" (up to MAX_MON_AURAS, all on a single line) */
    else if (buf[0] == 'A')
    {
        return parse_mon_auras(buf + 2, r_ptr);
    }
    /* Process 'F' for "Basic Flags" (multiple lines) */
    else if (buf[0] == 'F')
    {
        return parse_mon_flags(buf + 2, r_ptr);
    }

    /* Process 'S' for "Spell Flags" (multiple lines) */
    else if (buf[0] == 'S')
    {
        if (!r_ptr->spells)
            r_ptr->spells = mon_spells_alloc();
        return parse_mon_spells(buf + 2, r_ptr);
    }
    /* O:DROP_WARRIOR */
    else if (buf[0] == 'O')
    {
        return mon_drop_parse(buf + 2, r_ptr, 0);
    }

    /* Oops */
    else return PARSE_ERROR_UNDEFINED_DIRECTIVE;


    /* Success */
    return (0);
}


errr parse_room_line(room_ptr room, char *line, int options)
{
    /* T:Type:Subtype[:Flags] */
    if (line[0] == 'T' && line[1] == ':')
        return _parse_room_type(line + 2, room);

    /* W:Level:MaxLevel:Rarity */
    else if (line[0] == 'W' && line[1] == ':')
    {
        char *zz[10];
        int   num = tokenize(line + 2, 10, zz, 0);
        int   tmp;

        if (num != 3)
        {
            msg_print("Error: Invalid W: line. Syntax: W:<Level>:<MaxLevel>:<Rarity>.");
            return PARSE_ERROR_TOO_FEW_ARGUMENTS;
        }
        room->level = atoi(zz[0]);
        if (streq(zz[1], "*"))
            room->max_level = 0;
        else
            room->max_level = atoi(zz[1]);
        tmp = atoi(zz[2]);
        if (tmp < 0 || tmp > 255) /* room_t.rarity is a byte */
        {
            msg_format("Error: Invalid rarity %d. Enter a value between 1 and 255.", tmp);
            return PARSE_ERROR_OUT_OF_BOUNDS;
        }
        room->rarity = tmp;
    }

    /* L:X:... */
    else if (line[0] == 'L' && line[1] == ':')
    {
        int rc;
        room_grid_ptr letter = malloc(sizeof(room_grid_t));
        memset(letter, 0, sizeof(room_grid_t));
        letter->letter = line[2];
        rc = parse_room_grid(line + 4, letter, options);
        if (!rc) int_map_add(room->letters, letter->letter, letter);
        else free(letter);
        if (rc) return rc;
    }

    /* Process 'M'ap lines */
    else if (line[0] == 'M' && line[1] == ':')
    {
        /* Acquire the text */
        char *s = line+2;

        /* Calculate room dimensions automagically */
        room->height++;
        if (!room->width)
            room->width = strlen(s);
        else if (strlen(s) != room->width)
        {
            msg_format(
                "Error: Inconsistent map widths. Room width auto-calculated to %d but "
                "current line is %d. Please make all map lines the same length.",
                room->width, strlen(s)
            );
            return PARSE_ERROR_GENERIC;
        }
        vec_push(room->map, (vptr)z_string_make(s));
    }
    /* !:SCRAMBLE(a,b,c,d) */
    else if (line[0] == '!' && line[1] == ':')
    {
        char *name;
        char *args[10];
        int   arg_ct = parse_args(line + 2, &name, args, 10);

        if (arg_ct < 0)
        {
            msg_format("Error: Malformed argument %s. Missing )?", name);
            return PARSE_ERROR_GENERIC;
        }

        if (streq(name, "SCRAMBLE") || streq(name, "SHUFFLE"))
        {
            #define _max_scramble 20 /* Windows */
            char letters[_max_scramble], scrambles[_max_scramble];
            int  i;
            if (arg_ct > _max_scramble)
            {
                msg_format("I can only scramble %d letters.", _max_scramble);
                return PARSE_ERROR_GENERIC;
            }
            for (i = 0; i < arg_ct; i++)
            {
                letters[i] = args[i][0];
                scrambles[i] = letters[i];
            }
            for (i = 0; i < arg_ct - 1; i++) /* Skiena: _Algorithm_Design_Manual_ p248 */
            {
                int j = rand_range(i, arg_ct - 1);
                char t = scrambles[i];
                scrambles[i] = scrambles[j];
                scrambles[j] = t;
            }
            for (i = 0; i < arg_ct; i++)
            {
                char letter = letters[i];
                char scramble = scrambles[i];
                room_grid_ptr grid = int_map_find(room->letters, letter);
                if (!grid)
                {
                    msg_format("Error: Undefined letter: %c", letter);
                    return PARSE_ERROR_GENERIC;
                }
                grid->scramble = scramble; /* cf _find_room_grid (rooms.c) */
                grid = int_map_find(room->letters, scramble);
                if (!grid)
                {
                    msg_format("Error: Undefined scramble: %c", scramble);
                    return PARSE_ERROR_GENERIC;
                }
                if (trace_doc)
                    doc_printf(trace_doc, "Scrambled <color:R>%c</color> to <color:B>%c</color>.\n", letter, scramble);
            }
        }
        else
        {
            msg_format("Error: Unknown directive: %s", name);
            return PARSE_ERROR_GENERIC;
        }
        return 0;
    }
    return 0;
}

/*
 * Parse a sub-file of the "extra info"
 */
static errr process_dungeon_file_aux(char *buf, int options)
{
    /* Skip "empty" lines */
    if (!buf[0]) return (0);

    /* Skip "blank" lines */
    if (isspace(buf[0])) return (0);

    /* Skip comments */
    if (buf[0] == '#') return (0);

    /* Require "?:*" format */
    if (buf[1] != ':') return (1);


    /* Process "%:<fname>" */
    if (buf[0] == '%')
    {
        return process_dungeon_file(buf + 2, options);
    }

    /* Failure */
    return (1);
}


static char tmp[255];
static cptr variant_name = "POSCHENGBAND";

/*
 * Helper function for "process_dungeon_file()"
 */
static cptr process_dungeon_file_expr(char **sp, char *fp)
{
    cptr v;

    char *b;
    char *s;

    char b1 = '[';
    char b2 = ']';

    char f = ' ';

    /* Initial */
    s = (*sp);

    /* Skip spaces */
    while (isspace(*s)) s++;

    /* Save start */
    b = s;

    /* Default */
    v = "?o?o?";

    /* Analyze */
    if (*s == b1)
    {
        const char *p;
        const char *t;

        /* Skip b1 */
        s++;

        /* First */
        t = process_dungeon_file_expr(&s, &f);

        /* Oops */
        if (!*t)
        {
            /* Nothing */
        }

        /* Function: IOR */
        else if (streq(t, "IOR") || streq(t, "OR"))
        {
            v = "0";
            while (*s && (f != b2))
            {
                t = process_dungeon_file_expr(&s, &f);
                if (*t && !streq(t, "0")) v = "1";
            }
        }

        /* Function: AND */
        else if (streq(t, "AND"))
        {
            v = "1";
            while (*s && (f != b2))
            {
                t = process_dungeon_file_expr(&s, &f);
                if (*t && streq(t, "0")) v = "0";
            }
        }

        /* Function: NOT */
        else if (streq(t, "NOT"))
        {
            v = "1";
            while (*s && (f != b2))
            {
                t = process_dungeon_file_expr(&s, &f);
                if (*t && streq(t, "1")) v = "0";
            }
        }

        /* Function: EQU */
        else if (streq(t, "EQU"))
        {
            v = "0";
            if (*s && (f != b2))
            {
                t = process_dungeon_file_expr(&s, &f);
            }
            while (*s && (f != b2))
            {
                p = process_dungeon_file_expr(&s, &f);
                if (streq(t, p)) v = "1";
            }
        }
        /* Function: MOD */
        else if (streq(t, "MOD"))
        {
            int x = 0;
            int y = 0;
            v = "0";
            if (*s && (f != b2))
            {
                x = atoi(process_dungeon_file_expr(&s, &f));
            }
            if(*s && (f != b2))
            {
                y = atoi(process_dungeon_file_expr(&s, &f));
                sprintf(tmp, "%d", x%y);
                v = tmp;
            }
        }

        /* Function: LEQ */
        else if (streq(t, "LEQ"))
        {
            v = "1";
            if (*s && (f != b2))
            {
                t = process_dungeon_file_expr(&s, &f);
            }
            while (*s && (f != b2))
            {
                p = t;
                t = process_dungeon_file_expr(&s, &f);
                if (*t && atoi(p) > atoi(t)) v = "0";
            }
        }

        /* Function: GEQ */
        else if (streq(t, "GEQ"))
        {
            v = "1";
            if (*s && (f != b2))
            {
                t = process_dungeon_file_expr(&s, &f);
            }
            while (*s && (f != b2))
            {
                p = t;
                t = process_dungeon_file_expr(&s, &f);

                /* Compare two numbers instead of string */
                if (*t && atoi(p) < atoi(t)) v = "0";
            }
        }

        /* Oops */
        else
        {
            while (*s && (f != b2))
            {
                t = process_dungeon_file_expr(&s, &f);
            }
        }

        /* Verify ending */
        if (f != b2) v = "?x?x?";

        /* Extract final and Terminate */
        if ((f = *s) != '\0') *s++ = '\0';
    }

    /* Other */
    else
    {
        /* Accept all printables except spaces and brackets */
        while (isprint(*s) && !my_strchr(" []", *s)) ++s;

        /* Extract final and Terminate */
        if ((f = *s) != '\0') *s++ = '\0';

        /* Variable */
        if (*b == '$')
        {
            /* System */
            if (streq(b+1, "SYS"))
            {
                v = ANGBAND_SYS;
            }

            /* Graphics */
            else if (streq(b+1, "GRAF"))
            {
                v = ANGBAND_GRAF;
            }
            else if (streq(b+1, "MONOCHROME"))
            {
                if (arg_monochrome)
                    v = "ON";
                else
                    v = "OFF";
            }
            /* Race */
            else if (streq(b+1, "RACE"))
            {
                v = get_true_race()->name;
            }
            else if (streq(b+1, "SUBRACE"))
            {
                v = get_true_race()->subname;
                if (!v) v = "none";
            }
            /* Class */
            else if (streq(b+1, "CLASS"))
            {
                v = get_class()->name;
            }
            else if (streq(b+1, "SUBCLASS"))
            {
                v = get_class()->subname;
                if (!v) v = "none";
            }
            /* Realms */
            else if (streq(b+1, "REALM1"))
            {
                v = realm_names[p_ptr->realm1];
            }
            else if (streq(b+1, "REALM2"))
            {
                v = realm_names[p_ptr->realm2];
            }
            /* Player name */
            else if (streq(b+1, "PLAYER"))
            {
                static char tmp_player_name[32];
                char *pn, *tpn;
                for (pn = player_name, tpn = tmp_player_name; *pn; pn++, tpn++)
                {
                    *tpn = my_strchr(" []", *pn) ? '_' : *pn;
                }
                *tpn = '\0';
                v = tmp_player_name;
            }

            /* Town */
            else if (streq(b+1, "TOWN"))
            {
                sprintf(tmp, "%d", dun_world_town_id());
                v = tmp;
            }

            /* Level */
            else if (streq(b+1, "LEVEL"))
            {
                sprintf(tmp, "%d", p_ptr->lev);
                v = tmp;
            }

            /* Quest status */
            else if (prefix(b+1, "QUEST"))
            {
                cptr _status[] = { "Untaken", "Taken", "InProgress", "Completed", "Finished", "Failed", "FailedDone" };
                int  which = atoi(b+6);
                quest_ptr q = quests_get(which);
                if (q) sprintf(tmp, "%s", _status[q->status]);
                else sprintf(tmp, "Unknown");
                v = tmp;
            }
            else if (prefix(b+1, "RANDOM"))
            {
                int q_idx = atoi(b+7);
                /* "RANDOM" uses a special parameter to determine the number of the quest */
                sprintf(tmp, "%d", quests_get(q_idx)->seed);
                v = tmp;
            }
            else if (streq(b+1, "VARIANT"))
                v = variant_name;
            else if (streq(b+1, "WORLD"))
            {
                sprintf(tmp, "%d", p_ptr->world_id);
                v = tmp;
            }
            else if (streq(b+1, "SPECIALITY"))
            {
                if (p_ptr->pclass == CLASS_WEAPONMASTER)
                    sprintf(tmp, "%s", weaponmaster_speciality_name(p_ptr->psubclass));
                else if (p_ptr->pclass == CLASS_DEVICEMASTER)
                    sprintf(tmp, "%s", devicemaster_speciality_name(p_ptr->psubclass));
                else
                    sprintf(tmp, "None");
                v = tmp;
            }
        }

        /* Constant */
        else
        {
            v = b;
        }
    }

    /* Save */
    (*fp) = f;

    /* Save */
    (*sp) = s;

    /* Result */
    return (v);
}

/* XXX Replace with parse_edit_file ... */
errr process_dungeon_file(cptr name, int options)
{
    FILE      *fp;
    char       buf[1024];
    int        num = 0;
    errr       err = 0;
    bool       bypass = FALSE;

    /* Build the filename */
    path_build(buf, sizeof(buf), ANGBAND_DIR_EDIT, name);

    /* Open the file */
    fp = my_fopen(buf, "r");

    /* No such file */
    if (!fp) return (-1);

    /* Process the file */
    while (0 == my_fgets(fp, buf, sizeof(buf)))
    {
        /* Count lines */
        num++;


        /* Skip "empty" lines */
        if (!buf[0]) continue;

        /* Skip "blank" lines */
        if (isspace(buf[0])) continue;

        /* Skip comments */
        if (buf[0] == '#') continue;


        /* Process "?:<expr>" */
        if ((buf[0] == '?') && (buf[1] == ':'))
        {
            char f;
            cptr v;
            char *s;

            /* Start */
            s = buf + 2;

            /* Parse the expr */
            v = process_dungeon_file_expr(&s, &f);

            /* Set flag */
            bypass = (streq(v, "0") ? TRUE : FALSE);

            /* Continue */
            continue;
        }

        /* Apply conditionals ... INIT_DEBUG is for testing purposes */
        if (!(options & INIT_DEBUG) && bypass) continue;

        err = process_dungeon_file_aux(buf, options);

        /* Oops */
        if (err) break;
    }

    /* Errors */
    if (err)
    {
        cptr oops;

        /* Error string */
        oops = (((err > 0) && (err < PARSE_ERROR_MAX)) ? err_str[err] : "unknown");

        /* Oops */
        msg_boundary();
        msg_format("<color:v>Error</color> %d (%s) at line %d of '%s'.", err, oops, num, name);
        msg_format("Parsing '%s'.", buf);

        msg_print(NULL);
    }

    /* Close the file */
    my_fclose(fp);

    /* Result */
    return (err);
}

errr parse_edit_file(cptr name, parser_f parser, int options)
{
    FILE *fp;
    char  buf[1024];
    int   line_num = 0;
    errr  err = 0;
    bool  bypass = FALSE;

    assert(parser);

    path_build(buf, sizeof(buf), ANGBAND_DIR_EDIT, name);
    fp = my_fopen(buf, "r");
    if (!fp) return (-1);

    while (!err && 0 == my_fgets(fp, buf, sizeof(buf)))
    {
        line_num++;

        if (!buf[0]) continue; /* empty line */
        if (isspace(buf[0])) continue; /* blank line */
        if (buf[0] == '#') continue; /* comment */
        if (buf[0] == '?' && buf[1] == ':') /* ?:<exp> conditional controls subsequent parsing */
        {
            char f;
            cptr v;
            char *s;

            s = buf + 2;
            v = process_dungeon_file_expr(&s, &f);
            bypass = streq(v, "0"); /* skip until subsequent ?: returns true */
            continue;
        }
        if (!(options & INIT_DEBUG) && bypass) continue; /* apply skip unless debugging */

        if (buf[0] == '%' && buf[1] == ':') /* %:file.txt */
            err = parse_edit_file(buf + 2, parser, options);
        else
        {
            if (trace_doc && (buf[0] == 'L' || buf[0] == 'R'))
            {
                doc_printf(trace_doc, "<color:R>%s:%d</color> <indent>%s</indent>\n",
                    name, line_num, buf);
            }
            err = parser(buf, options);
            if (err) /* report now for recursion */
            {
                cptr oops = (err > 0 && err < PARSE_ERROR_MAX) ? err_str[err] : "unknown";

                msg_boundary();
                msg_format("<color:v>Error</color> %d (%s) at line %d of '%s'.", err, oops, line_num, name);
                msg_format("Parsing '%s'.", buf);
                msg_print(NULL); /* quit() during initialization ... */
            }
        }
    }

    my_fclose(fp);
    return err;
}


