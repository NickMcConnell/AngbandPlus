// File: init1.c
// Purpose: Initialization (part 1) -BEN-

#include "utumno.h"


/*
 * This file is used to initialize various variables and arrays for the
 * Angband game.  Note the use of "fd_read()" and "fd_write()" to bypass
 * the common limitation of "read()" and "write()" to only 32767 bytes
 * at a time.
 *
 * Several of the arrays for Angband are built from "template" files in
 * the "dat" directory, from which quick-load binary "image" files
 * are constructed whenever they are not present in the "lib/data"
 * directory, or if those files become obsolete, if we are allowed.
 *
 * Warning -- the "ascii" file parsers use a minor hack to collect the
 * name and text information in a single pass.  Thus, the game will not
 * be able to load any template file with more than 20K of names or 60K
 * of text, even though technically, up to 64K should be legal.
 *
 * The code could actually be removed and placed into a "stand-alone"
 * program, but that feels a little silly, especially considering some
 * of the platforms that we currently support.
 */


/*
 * Hack -- error tracking
 */
extern s16b error_idx;
extern s16b error_line;


/*
 * Hack -- size of the "fake" arrays
 */
extern u16b fake_name_size;
extern u16b fake_text_size;



/*** Helper arrays for parsing ascii template files ***/

/*
 * Monster types
 */

static char *r_info_type[] = {
    "",
    "TOWN",
    "HUMAN_WARRIOR",
    "HUMAN_MAGE",
    "HUMAN_PRIEST",
    "HUMAN_ROGUE",
    "HUMAN_RANGER",
    "HUMAN_MISC",
    "HUMANOID_D_ELF",
    "HUMANOID_ORC",
    "HUMANOID_TROLL",
    "HUMANOID_OGRE",
    "HUMANOID_GIANT",
    "HUMANOID_TITAN",
    "HUMANOID_YEEK",
    "HUMANOID_KOBOLD",
    "HUMANOID_MISC",
    "MAMMAL_QUAD",
    "MAMMAL_YETI",
    "MAMMAL_CAT",
    "MAMMAL_DOG",
    "MAMMAL_RODENT",
    "MAMMAL_BAT",
    "VERT_HARPY",
    "VERT_BIRD",
    "VERT_AMPHIB",
    "VERT_NAGA",
    "VERT_SNAKE",
    "VERT_REPTILE",
    "VERT_HYDRA",
    "VERT_HYBRID",
    "INSECT_SPIDER",
    "INSECT_SCORPION",
    "INSECT_ANT",
    "INSECT_BEETLE",
    "INSECT_CENTIPEDE",
    "INSECT_TICK",
    "INSECT_LOUSE",
    "INSECT_DRAG_FLY",
    "INSECT_FLY",
    "LIFE_FUNGI",
    "LIFE_ICKY_THING",
    "LIFE_JELLY_IMMOB",
    "LIFE_JELLY_MOB",
    "LIFE_OOZE",
    "LIFE_WORM",
    "LIFE_EYE",
    "LIFE_BEHOLDER",
    "ELEMENT_SPIRIT",
    "ELEMENT_HOUND",
    "ELEMENT_VORTEX",
    "DRAGON_BABY",
    "DRAGON_YOUNG",
    "DRAGON_MATURE",
    "DRAGON_ANCIENT",
    "DRAGON_GREAT_WYRM",
    "DRAGON_UNUSUAL",
    "DRAGON_UNIQUE",
    "DEMON_MINOR",
    "DEMON_MAJOR",
    "UNDEAD_SKELETON",
    "UNDEAD_DRUJ",
    "UNDEAD_ZOMBIE",
    "UNDEAD_MUMMY",
    "UNDEAD_GHOST",
    "UNDEAD_WIGHT",
    "UNDEAD_NAZGUL",
    "UNDEAD_VAMPIRE",
    "UNDEAD_LICH",
    "ANGEL",
    "MIMIC",
    "GOLEM",
    "COINS",
    "QUYLTHULG",
    "MISC",
    "QUESTOR",
    NULL
};


/*
 * Monster Blow Methods
 */
static char *r_info_blow_method[] = {
    "",
    "HIT",
    "TOUCH",
    "PUNCH",
    "KICK",
    "CLAW",
    "BITE",
    "STING",
    "XXX1",
    "BUTT",
    "CRUSH",
    "ENGULF",
    "XXX2",
    "CRAWL",
    "DROOL",
    "SPIT",
    "XXX3",
    "GAZE",
    "WAIL",
    "SPORE",
    "XXX4",
    "BEG",
    "INSULT",
    "MOAN",
    "XXX5",
    NULL
};


/*
 * Monster Blow Effects
 */
static char *r_info_blow_effect[] = {
    "",
    "HURT",
    "POISON",
    "UN_BONUS",
    "UN_POWER",
    "EAT_GOLD",
    "EAT_ITEM",
    "EAT_FOOD",
    "EAT_LITE",
    "ACID",
    "ELEC",
    "FIRE",
    "COLD",
    "BLIND",
    "CONFUSE",
    "TERRIFY",
    "PARALYZE",
    "LOSE_STR",
    "LOSE_INT",
    "LOSE_WIS",
    "LOSE_DEX",
    "LOSE_CON",
    "LOSE_CHR",
    "LOSE_ALL",
    "SHATTER",
    "EXP_10",
    "EXP_20",
    "EXP_40",
    "EXP_80",
    NULL
};


/*
 * Monster race flags
 */
static char *r_info_flags1[] = {
    "UNIQUE",
    "QUESTOR",
    "MALE",
    "FEMALE",
    "CHAR_CLEAR",
    "CHAR_MULTI",
    "ATTR_CLEAR",
    "ATTR_MULTI",
    "FORCE_DEPTH",
    "FORCE_MAXHP",
    "FORCE_SLEEP",
    "FORCE_EXTRA",
    "FRIEND",
    "FRIENDS",
    "ESCORT",
    "ESCORTS",
    "NEVER_BLOW",
    "NEVER_MOVE",
    "RAND_25",
    "RAND_50",
    "ONLY_GOLD",
    "ONLY_ITEM",
    "DROP_60",
    "DROP_90",
    "DROP_1D2",
    "DROP_2D2",
    "DROP_3D2",
    "DROP_4D2",
    "DROP_GOOD",
    "DROP_GREAT",
    "DROP_USEFUL",
    "DROP_CHOSEN"
};

/*
 * Monster race flags
 */
static char *r_info_flags2[] = {
    "STUPID",
    "SMART",
    "XXX1X2",
    "XXX2X2",
    "INVISIBLE",
    "COLD_BLOOD",
    "EMPTY_MIND",
    "WEIRD_MIND",
    "MULTIPLY",
    "REGENERATE",
    "XXX3X2",
    "XXX4X2",
    "POWERFUL",
    "XXX5X2",
    "XXX7X2",
    "XXX6X2",
    "OPEN_DOOR",
    "BASH_DOOR",
    "PASS_WALL",
    "KILL_WALL",
    "MOVE_BODY",
    "KILL_BODY",
    "TAKE_ITEM",
    "KILL_ITEM",
    "BRAIN_1",
    "BRAIN_2",
    "BRAIN_3",
    "BRAIN_4",
    "BRAIN_5",
    "BRAIN_6",
    "BRAIN_7",
    "BRAIN_8"
};

/*
 * Monster race flags
 */
static char *r_info_flags3[] = {
    "ORC",
    "TROLL",
    "GIANT",
    "DRAGON",
    "DEMON",
    "UNDEAD",
    "EVIL",
    "ANIMAL",
    "XXX1X3",
    "XXX2X3",
    "XXX3X3",
    "XXX4X3",
    "HURT_LITE",
    "HURT_ROCK",
    "HURT_FIRE",
    "HURT_COLD",
    "IM_ACID",
    "IM_ELEC",
    "IM_FIRE",
    "IM_COLD",
    "IM_POIS",
    "XXX5X3",
    "RES_NETHER",
    "RES_WATER",
    "RES_PLASMA",
    "RES_NEXUS",
    "RES_DISEN",
    "RES_MAGIC",
    "NO_FEAR",
    "NO_STUN",
    "NO_CONF",
    "NO_SLEEP"
};

/*
 * Monster race flags
 */
static char *r_info_flags4[] = {
    "SHRIEK",
    "XXX2X4",
    "XXX3X4",
    "XXX4X4",
    "ARROW_1",
    "ARROW_2",
    "ARROW_3",
    "ARROW_4",
    "BR_ACID",
    "BR_ELEC",
    "BR_FIRE",
    "BR_COLD",
    "BR_POIS",
    "BR_NETH",
    "BR_LITE",
    "BR_DARK",
    "BR_CONF",
    "BR_SOUN",
    "BR_CHAO",
    "BR_DISE",
    "BR_NEXU",
    "BR_TIME",
    "BR_INER",
    "BR_GRAV",
    "BR_SHAR",
    "BR_PLAS",
    "BR_WALL",
    "BR_MANA",
    "XXX5X4",
    "XXX6X4",
    "XXX7X4",
    "XXX8X4"
};

/*
 * Monster race flags
 */
static char *r_info_flags5[] = {
    "BA_ACID",
    "BA_ELEC",
    "BA_FIRE",
    "BA_COLD",
    "BA_POIS",
    "BA_NETH",
    "BA_WATE",
    "BA_MANA",
    "BA_DARK",
    "DRAIN_MANA",
    "MIND_BLAST",
    "BRAIN_SMASH",
    "CAUSE_1",
    "CAUSE_2",
    "CAUSE_3",
    "CAUSE_4",
    "BO_ACID",
    "BO_ELEC",
    "BO_FIRE",
    "BO_COLD",
    "BO_POIS",
    "BO_NETH",
    "BO_WATE",
    "BO_MANA",
    "BO_PLAS",
    "BO_ICEE",
    "MISSILE",
    "SCARE",
    "BLIND",
    "CONF",
    "SLOW",
    "HOLD"
};

// Monster race flags
static char *r_info_flags6[] = {
    "HASTE",
    "XXX1X6",
    "HEAL",
    "XXX2X6",
    "BLINK",
    "TPORT",
    "XXX3X6",
    "XXX4X6",
    "TELE_TO",
    "TELE_AWAY",
    "TELE_LEVEL",
    "XXX5",
    "DARKNESS",
    "TRAPS",
    "FORGET",
    "XXX6X6",
    "XXX7X6",
    "XXX8X6",
    "S_MONSTER",
    "S_MONSTERS",
    "S_ANT",
    "S_SPIDER",
    "S_HOUND",
    "S_HYDRA",
    "S_ANGEL",
    "S_DEMON",
    "S_UNDEAD",
    "S_DRAGON",
    "S_HI_UNDEAD",
    "S_HI_DRAGON",
    "S_WRAITH",
    "S_UNIQUE"
};


// Item flags
static char *k_info_flags1[] = {
    "STR",
    "INT",
    "WIS",
    "DEX",
    "CON",
    "CHR",
    "XXX1",
    "XXX2",
    "STEALTH",
    "SEARCH",
    "INFRA",
    "TUNNEL",
    "SPEED",
    "BLOWS",
    "XXX3",
    "XXX4",
    "SLAY_ANIMAL",
    "SLAY_EVIL",
    "SLAY_UNDEAD",
    "SLAY_DEMON",
    "SLAY_ORC",
    "SLAY_TROLL",
    "SLAY_GIANT",
    "SLAY_DRAGON",
    "KILL_DRAGON",
    "XXX5",
    "IMPACT",
    "BRAND_POIS",
    "BRAND_ACID",
    "BRAND_ELEC",
    "BRAND_FIRE",
    "BRAND_COLD"
};

// Item flags
static char *k_info_flags2[] = {
    "SUST_STR",
    "SUST_INT",
    "SUST_WIS",
    "SUST_DEX",
    "SUST_CON",
    "SUST_CHR",
    "XXX1",
    "XXX2",
    "IM_ACID",
    "IM_ELEC",
    "IM_FIRE",
    "IM_COLD",
    "XXX3",
    "XXX4",
    "FREE_ACT",
    "HOLD_LIFE",
    "RES_ACID",
    "RES_ELEC",
    "RES_FIRE",
    "RES_COLD",
    "RES_POIS",
    "XXX5",
    "RES_LITE",
    "RES_DARK",
    "RES_BLIND",
    "RES_CONF",
    "RES_SOUND",
    "RES_SHARDS",
    "RES_NETHER",
    "RES_NEXUS",
    "RES_CHAOS",
    "RES_DISEN"
};

// Item flags
static char *k_info_flags3[] = {
    "XXX1",
    "XXX2",
    "XXX3",
    "XXX4",
    "XXX5",
    "XXX6",
    "XXX7",
    "XXX8",
    "EASY_KNOW",
    "HIDE_TYPE",
    "SHOW_MODS",
    "INSTA_ART",
    "FEATHER",
    "LITE",
    "SEE_INVIS",
    "TELEPATHY",
    "SLOW_DIGEST",
    "REGEN",
    "XTRA_MIGHT",
    "XTRA_SHOTS",
    "IGNORE_ACID",
    "IGNORE_ELEC",
    "IGNORE_FIRE",
    "IGNORE_COLD",
    "ACTIVATE",
    "DRAIN_EXP",
    "TELEPORT",
    "AGGRAVATE",
    "BLESSED",
    "CURSED",
    "HEAVY_CURSE",
    "PERMA_CURSE"
};


/*** Initialize from ascii template files ***/


/*
 * Initialize the "v_info" array, by parsing an ascii "template" file
 */
errr init_v_info_txt(FILE *fp, char *buf)
{
    int i;

    char *s;

    /* Not ready yet */
    bool okay = FALSE;

    /* Current entry */
    vault_type *v_ptr = NULL;


    /* Just before the first record */
    error_idx = -1;

    /* Just before the first line */
    error_line = -1;


    /* Prepare the "fake" stuff */
    v_head->name_size = 0;
    v_head->text_size = 0;

    /* Parse */
    while (0 == my_fgets(fp, buf, 1024)) {
        /* Advance the line number */
        error_line++;

        /* Skip comments and blank lines */
        if (!buf[0] || (buf[0] == '#')) continue;

        /* Verify correct "colon" format */
        if (buf[1] != ':') return (1);


        /* Hack -- Process 'V' for "Version" */
        if (buf[0] == 'V') {
            /* Verify the string */
            if (strcmp(buf+2, VERSION_STRING)) {
                return 2;
            }

            /* Okay to proceed */
            okay = TRUE;

            /* Continue */
            continue;
        }

        /* No version yet */
        if (!okay) return (2);


        /* Process 'N' for "New/Number/Name" */
        if (buf[0] == 'N') {
            /* Find the colon before the name */
            s = strchr(buf+2, ':');

            /* Verify that colon */
            if (!s) return (1);

            /* Nuke the colon, advance to the name */
            *s++ = '\0';

            /* Paranoia -- require a name */
            if (!*s) return (1);

            /* Get the index */
            i = atoi(buf+2);

            /* Verify information */
            if (i <= error_idx) return (4);

            /* Verify information */
            if (i >= MAX_V_IDX) return (2);

            /* Save the index */
            error_idx = i;

            /* Point at the "info" */
            v_ptr = &v_info[i];

            /* Hack -- Verify space */
	// MV -- Inserted a casting to u16b in order to avoid a compile warning
            if (v_head->name_size + (u16b) strlen(s) > fake_name_size - 8) return (7);

            /* Advance and Save the name index */
            if (!v_ptr->name) v_ptr->name = ++v_head->name_size;

            /* Append chars to the name */
            strcpy(v_name + v_head->name_size, s);

            /* Advance the index */
            v_head->name_size += strlen(s);

            /* Next... */
            continue;
        }

        /* There better be a current v_ptr */
        if (!v_ptr) return (3);


        /* Process 'D' for "Description" */
        if (buf[0] == 'D')
        {
            /* Acquire the text */
            s = buf+2;

            /* Hack -- Verify space */
	// MV -- Inserted a casting to u16b in order to avoid a compile warning
            if (v_head->text_size + (u16b) strlen(s) > fake_text_size -  8) return (7);

            /* Advance and Save the text index */
            if (!v_ptr->text) v_ptr->text = ++v_head->text_size;

            /* Append chars to the name */
            strcpy(v_text + v_head->text_size, s);

            /* Advance the index */
            v_head->text_size += strlen(s);

            /* Next... */
            continue;
        }


        /* Process 'X' for "Extra info" (one line only) */
        if (buf[0] == 'X')
        {
            int typ, rat, hgt, wid;

            /* Scan for the values */
            if (4 != sscanf(buf+2, "%d:%d:%d:%d",
                &typ, &rat, &hgt, &wid)) return (1);

            /* Save the values */
            v_ptr->typ = typ;
            v_ptr->rat = rat;
            v_ptr->hgt = hgt;
            v_ptr->wid = wid;

            /* Next... */
            continue;
        }


        /* Oops */
        return (6);
    }


    /* Complete the "name" and "text" sizes */
    ++v_head->name_size;
    ++v_head->text_size;


    /* No version yet */
    if (!okay) return (2);


    /* Success */
    return (0);
}



/*
 * Initialize the "f_info" array, by parsing an ascii "template" file
 */
errr init_f_info_txt(FILE *fp, char *buf)
{
    int i;

    char *s;

    /* Not ready yet */
    bool okay = FALSE;

    /* Current entry */
    feature_type *f_ptr = NULL;


    /* Just before the first record */
    error_idx = -1;

    /* Just before the first line */
    error_line = -1;


    /* Prepare the "fake" stuff */
    f_head->name_size = 0;
    f_head->text_size = 0;

    /* Parse */
    while (0 == my_fgets(fp, buf, 1024))
    {
        /* Advance the line number */
        error_line++;

        /* Skip comments and blank lines */
        if (!buf[0] || (buf[0] == '#')) continue;

        /* Verify correct "colon" format */
        if (buf[1] != ':') return (1);


        /* Hack -- Process 'V' for "Version" */
        if (buf[0] == 'V') {
            /* Verify the string */
            if (strcmp(buf+2, VERSION_STRING)) {
                return 2;
            }

            /* Okay to proceed */
            okay = TRUE;

            /* Continue */
            continue;
        }

        /* No version yet */
        if (!okay) return (2);


        /* Process 'N' for "New/Number/Name" */
        if (buf[0] == 'N')
        {
            /* Find the colon before the name */
            s = strchr(buf+2, ':');

            /* Verify that colon */
            if (!s) return (1);

            /* Nuke the colon, advance to the name */
            *s++ = '\0';

            /* Paranoia -- require a name */
            if (!*s) return (1);

            /* Get the index */
            i = atoi(buf+2);

            /* Verify information */
            if (i <= error_idx) return (4);

            /* Verify information */
            if (i >= MAX_F_IDX) return (2);

            /* Save the index */
            error_idx = i;

            /* Point at the "info" */
            f_ptr = &f_info[i];

            /* Hack -- Verify space */
	// MV -- Inserted a casting to u16b in order to avoid a compile warning
            if (f_head->name_size + (u16b) strlen(s) > fake_name_size - 8) return (7);

            /* Advance and Save the name index */
            if (!f_ptr->name) f_ptr->name = ++f_head->name_size;

            /* Append chars to the name */
            strcpy(f_name + f_head->name_size, s);

            /* Advance the index */
            f_head->name_size += strlen(s);

            /* Next... */
            continue;
        }

        /* There better be a current f_ptr */
        if (!f_ptr) return (3);


        /* Oops */
        return (6);
    }


    /* Complete the "name" and "text" sizes */
    ++f_head->name_size;
    ++f_head->text_size;


    /* No version yet */
    if (!okay) return (2);


    /* Success */
    return (0);
}



/*
 * Grab one flag in an CObjectKind from a textual string
 */
static errr grab_one_kind_flag(CObjectKind *k_ptr, char *what)
{
    int i;

    /* Check flags1 */
    for (i = 0; i < 32; i++) {
        if (streq(what, k_info_flags1[i])) {
            k_ptr->flags1 |= (1L << i);
            return (0);
        }
    }

    /* Check flags2 */
    for (i = 0; i < 32; i++) {
        if (streq(what, k_info_flags2[i])) {
            k_ptr->flags2 |= (1L << i);
            return (0);
        }
    }

    /* Check flags3 */
    for (i = 0; i < 32; i++) {
        if (streq(what, k_info_flags3[i])) {
            k_ptr->flags3 |= (1L << i);
            return (0);
        }
    }

    /* Oops */
    quit(format("Unknown object flag '%s'.", what));

    /* Error */
    return (1);
}



/*
 * Initialize the "k_info" array, by parsing an ascii "template" file
 */
errr init_k_info_txt(FILE *fp, char *buf)
{
    int i;

    char *s, *t;

    /* Not ready yet */
    bool okay = FALSE;

    /* Current entry */
    CObjectKind *k_ptr = NULL;


    /* Just before the first record */
    error_idx = -1;

    /* Just before the first line */
    error_line = -1;


    /* Parse */
    while (0 == my_fgets(fp, buf, 1024)) {
        /* Advance the line number */
        error_line++;

        /* Skip comments and blank lines */
        if (!buf[0] || (buf[0] == '#')) continue;

        /* Verify correct "colon" format */
        if (buf[1] != ':') return (1);


        /* Hack -- Process 'V' for "Version" */
        if (buf[0] == 'V') {
            /* Verify the string */
            if (strcmp(buf+2, VERSION_STRING)) {
                return 2;
            }

            /* Okay to proceed */
            okay = TRUE;

            /* Continue */
            continue;
        }

        /* No version yet */
        if (!okay) return (2);


        /* Process 'N' for "New/Number/Name" */
        if (buf[0] == 'N') {
            /* Find the colon before the name */
            s = strchr(buf+2, ':');

            /* Verify that colon */
            if (!s) return (1);

            /* Nuke the colon, advance to the name */
            *s++ = '\0';

            /* Paranoia -- require a name */
            if (!*s) return (1);

            /* Get the index */
            i = atoi(buf+2);

            /* Verify information */
            if (i <= error_idx) return (4);

            /* Verify information */
            if (i >= MAX_K_IDX) return (2);

            /* Save the index */
            error_idx = i;

            /* Point at the "info" */
            k_ptr = &k_info[i];

            // Get the name
            k_ptr->name = new char[strlen(s)+1];
            strcpy(k_ptr->name, s);

            /* Next... */
            continue;
        }

        /* There better be a current k_ptr */
        if (!k_ptr) return (3);


        // Process 'G' for "Graphics" (one line only)
        if (buf[0] == 'G') {
            k_ptr->tile = locate_tile(buf+2);
            if (k_ptr->tile == -1) quit(format("Bad k_info tile: %s", buf+2));
            continue;
        }
        
        
        /* Process 'I' for "Info" (one line only) */
        if (buf[0] == 'I') {
            int tval, sval, pval;

            /* Scan for the values */
            if (3 != sscanf(buf+2, "%d:%d:%d",
                &tval, &sval, &pval)) return (1);

            /* Save the values */
            k_ptr->tval = tval;
            k_ptr->sval = sval;
            k_ptr->pval = pval;

            /* Next... */
            continue;
        }

        /* Process 'W' for "More Info" (one line only) */
        if (buf[0] == 'W') {
            int level, extra, wgt;
            long cost;

            /* Scan for the values */
            if (4 != sscanf(buf+2, "%d:%d:%d:%ld",
                &level, &extra, &wgt, &cost)) return (1);

            /* Save the values */
            k_ptr->level = level;
            k_ptr->extra = extra;
            k_ptr->weight = wgt;
            k_ptr->cost = cost;

            /* Next... */
            continue;
        }

        /* Process 'A' for "Allocation" (one line only) */
        if (buf[0] == 'A')
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
                t = strchr(s+1, '/');

                /* Find the next colon */
                s = strchr(s+1, ':');

                /* If the slash is "nearby", use it */
                if (t && (!s || t < s))
                {
                    int chance = atoi(t+1);
                    if (chance > 0) k_ptr->chance[i] = chance;
                }
            }

            /* Next... */
            continue;
        }

        /* Hack -- Process 'P' for "power" and such */
        if (buf[0] == 'P') {
            int ac, hd1, hd2, th, td, ta;

            /* Scan for the values */
            if (6 != sscanf(buf+2, "%d:%dd%d:%d:%d:%d",
                &ac, &hd1, &hd2, &th, &td, &ta)) return (1);

            k_ptr->ac = ac;
            k_ptr->dd = hd1;
            k_ptr->ds = hd2;
            k_ptr->to_h = th;
            k_ptr->to_d = td;
            k_ptr->to_a =  ta;

            /* Next... */
            continue;
        }

        /* Hack -- Process 'F' for flags */
        if (buf[0] == 'F') {
            /* Parse every entry textually */
            for (s = buf + 2; *s; ) {
                /* Find the end of this entry */
                for (t = s; *t && (*t != ' ') && (*t != '|'); ++t) ;

                /* Nuke and skip any dividers */
                if (*t) {
                    *t++ = '\0';
                    while (*t == ' ' || *t == '|') t++;
                }

                /* Parse this entry */
                if (0 != grab_one_kind_flag(k_ptr, s)) return (5);

                /* Start the next entry */
                s = t;
            }

            /* Next... */
            continue;
        }


        /* Oops */
        return (6);
    }


    /* No version yet */
    if (!okay) return (2);


    /* Success */
    return (0);
}


/*
 * Grab one flag in an artifact_type from a textual string
 */
static errr grab_one_artifact_flag(artifact_type *a_ptr, char *what)
{
    int i;

    /* Check flags1 */
    for (i = 0; i < 32; i++)
    {
        if (streq(what, k_info_flags1[i]))
        {
            a_ptr->flags1 |= (1L << i);
            return (0);
        }
    }

    /* Check flags2 */
    for (i = 0; i < 32; i++)
    {
        if (streq(what, k_info_flags2[i]))
        {
            a_ptr->flags2 |= (1L << i);
            return (0);
        }
    }

    /* Check flags3 */
    for (i = 0; i < 32; i++)
    {
        if (streq(what, k_info_flags3[i]))
        {
            a_ptr->flags3 |= (1L << i);
            return (0);
        }
    }

    /* Oops */
    quit(format("Unknown artifact flag '%s'.", what));

    /* Error */
    return (1);
}




/*
 * Initialize the "a_info" array, by parsing an ascii "template" file
 */
errr init_a_info_txt(FILE *fp, char *buf)
{
    int i;

    char *s, *t;

    /* Not ready yet */
    bool okay = FALSE;

    /* Current entry */
    artifact_type *a_ptr = NULL;


    /* Just before the first record */
    error_idx = -1;

    /* Just before the first line */
    error_line = -1;


    /* Parse */
    while (0 == my_fgets(fp, buf, 1024)) {
        /* Advance the line number */
        error_line++;

        /* Skip comments and blank lines */
        if (!buf[0] || (buf[0] == '#')) continue;

        /* Verify correct "colon" format */
        if (buf[1] != ':') return (1);


        /* Hack -- Process 'V' for "Version" */
        if (buf[0] == 'V') {
            /* Verify the string */
            if (strcmp(buf+2, VERSION_STRING)) {
                return 2;
            }

            /* Okay to proceed */
            okay = TRUE;

            /* Continue */
            continue;
        }

        /* No version yet */
        if (!okay) return (2);


        /* Process 'N' for "New/Number/Name" */
        if (buf[0] == 'N')
        {
            /* Find the colon before the name */
            s = strchr(buf+2, ':');

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
            if (i >= MAX_A_IDX) return (2);

            /* Save the index */
            error_idx = i;

            /* Point at the "info" */
            a_ptr = &a_info[i];

            /* Hack -- Verify space */
	// MV -- Inserted a casting to u16b in order to avoid a compile warning
            if (a_head->name_size + (u16b) strlen(s) > fake_name_size - 8) return (7);

            /* Advance and Save the name index */
            if (!a_ptr->name) a_ptr->name = ++a_head->name_size;

            /* Append chars to the name */
            strcpy(a_name + a_head->name_size, s);

            /* Advance the index */
            a_head->name_size += strlen(s);

            /* Next... */
            continue;
        }

        /* There better be a current a_ptr */
        if (!a_ptr) return (3);


        /* Process 'I' for "Info" (one line only) */
        if (buf[0] == 'I') {
            int tval, sval, pval;

            /* Scan for the values */
            if (3 != sscanf(buf+2, "%d:%d:%d",
                &tval, &sval, &pval)) return (1);

            /* Save the values */
            a_ptr->tval = tval;
            a_ptr->sval = sval;
            a_ptr->pval = pval;

            /* Next... */
            continue;
        }

        /* Process 'W' for "More Info" (one line only) */
        if (buf[0] == 'W') {
            int level, rarity, wgt;
            long cost;

            /* Scan for the values */
            if (4 != sscanf(buf+2, "%d:%d:%d:%ld",
                &level, &rarity, &wgt, &cost)) return (1);

            /* Save the values */
            a_ptr->level = level;
            a_ptr->rarity = rarity;
            a_ptr->weight = wgt;
            a_ptr->cost = cost;

            /* Next... */
            continue;
        }

        /* Hack -- Process 'P' for "power" and such */
        if (buf[0] == 'P') {
            int ac, hd1, hd2, th, td, ta;

            /* Scan for the values */
            if (6 != sscanf(buf+2, "%d:%dd%d:%d:%d:%d",
                &ac, &hd1, &hd2, &th, &td, &ta)) return (1);

            a_ptr->ac = ac;
            a_ptr->dd = hd1;
            a_ptr->ds = hd2;
            a_ptr->to_h = th;
            a_ptr->to_d = td;
            a_ptr->to_a =  ta;

            /* Next... */
            continue;
        }

        /* Hack -- Process 'F' for flags */
        if (buf[0] == 'F')
        {
            /* Parse every entry textually */
            for (s = buf + 2; *s; )
            {
                /* Find the end of this entry */
                for (t = s; *t && (*t != ' ') && (*t != '|'); ++t) ;

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

            /* Next... */
            continue;
        }


        /* Oops */
        return (6);
    }


    /* Complete the "name" and "text" sizes */
    ++a_head->name_size;
    ++a_head->text_size;


    /* Hack -- extract the "ignore" flags */
    for (i = 0; i < MAX_A_IDX; i++) {
        a_ptr = &a_info[i];

        /* Skip non-artifacts */
        if (!a_ptr->name) continue;

        /* Ignore everything */
        a_ptr->flags3 |= TR3_IGNORE_ACID;
        a_ptr->flags3 |= TR3_IGNORE_ELEC;
        a_ptr->flags3 |= TR3_IGNORE_FIRE;
        a_ptr->flags3 |= TR3_IGNORE_COLD;
    }


    /* No version yet */
    if (!okay) return (2);


    /* Success */
    return (0);
}


/*
 * Grab one flag in a ego-item_type from a textual string
 */
static bool grab_one_ego_item_flag(ego_item_type *e_ptr, char *what)
{
    int i;

    /* Check flags1 */
    for (i = 0; i < 32; i++)
    {
        if (streq(what, k_info_flags1[i]))
        {
            e_ptr->flags1 |= (1L << i);
            return (0);
        }
    }

    /* Check flags2 */
    for (i = 0; i < 32; i++)
    {
        if (streq(what, k_info_flags2[i]))
        {
            e_ptr->flags2 |= (1L << i);
            return (0);
        }
    }

    /* Check flags3 */
    for (i = 0; i < 32; i++)
    {
        if (streq(what, k_info_flags3[i]))
        {
            e_ptr->flags3 |= (1L << i);
            return (0);
        }
    }

    /* Oops */
    quit(format("Unknown ego-item flag '%s'.", what));

    /* Error */
    return (1);
}




/*
 * Initialize the "e_info" array, by parsing an ascii "template" file
 */
errr init_e_info_txt(FILE *fp, char *buf)
{
    int i;

    char *s, *t;

    /* Not ready yet */
    bool okay = FALSE;

    /* Current entry */
    ego_item_type *e_ptr = NULL;


    /* Just before the first record */
    error_idx = -1;

    /* Just before the first line */
    error_line = -1;


    /* Parse */
    while (0 == my_fgets(fp, buf, 1024))
    {
        /* Advance the line number */
        error_line++;

        /* Skip comments and blank lines */
        if (!buf[0] || (buf[0] == '#')) continue;

        /* Verify correct "colon" format */
        if (buf[1] != ':') return (1);


        /* Hack -- Process 'V' for "Version" */
        if (buf[0] == 'V') {
            /* Verify the string */
            if (strcmp(buf+2, VERSION_STRING)) {
                return 2;
            }

            /* Okay to proceed */
            okay = TRUE;

            /* Continue */
            continue;
        }

        /* No version yet */
        if (!okay) return (2);


        /* Process 'N' for "New/Number/Name" */
        if (buf[0] == 'N')
        {
            /* Find the colon before the name */
            s = strchr(buf+2, ':');

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
            if (i >= MAX_E_IDX) return (2);

            /* Save the index */
            error_idx = i;

            /* Point at the "info" */
            e_ptr = &e_info[i];

            /* Hack -- Verify space */
	// MV -- Inserted a casting to u16b in order to avoid a compile warning
            if (e_head->name_size + (u16b) strlen(s) > fake_name_size - 8) return (7);

            /* Advance and Save the name index */
            if (!e_ptr->name) e_ptr->name = ++e_head->name_size;

            /* Append chars to the name */
            strcpy(e_name + e_head->name_size, s);

            /* Advance the index */
            e_head->name_size += strlen(s);

            /* Next... */
            continue;
        }

        /* There better be a current e_ptr */
        if (!e_ptr) return (3);


        /* Process 'X' for "Xtra" (one line only) */
        if (buf[0] == 'X')
        {
            int slot, rating;

            /* Scan for the values */
            if (2 != sscanf(buf+2, "%d:%d",
                &slot, &rating)) return (1);

            /* Save the values */
            e_ptr->slot = slot;
            e_ptr->rating = rating;

            /* Next... */
            continue;
        }

        /* Process 'W' for "More Info" (one line only) */
        if (buf[0] == 'W')
        {
            int level, rarity, pad2;
            long cost;

            /* Scan for the values */
            if (4 != sscanf(buf+2, "%d:%d:%d:%ld",
                &level, &rarity, &pad2, &cost)) return (1);

            /* Save the values */
            e_ptr->level = level;
            e_ptr->rarity = rarity;
            /* e_ptr->weight = wgt; */
            e_ptr->cost = cost;

            /* Next... */
            continue;
        }

        /* Hack -- Process 'C' for "creation" */
        if (buf[0] == 'C')
        {
            int th, td, ta, pv;

            /* Scan for the values */
            if (4 != sscanf(buf+2, "%d:%d:%d:%d",
                &th, &td, &ta, &pv)) return (1);

            e_ptr->max_to_h = th;
            e_ptr->max_to_d = td;
            e_ptr->max_to_a = ta;
            e_ptr->max_pval = pv;

            /* Next... */
            continue;
        }

        /* Hack -- Process 'F' for flags */
        if (buf[0] == 'F')
        {
            /* Parse every entry textually */
            for (s = buf + 2; *s; )
            {
                /* Find the end of this entry */
                for (t = s; *t && (*t != ' ') && (*t != '|'); ++t) ;

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

            /* Next... */
            continue;
        }

        /* Oops */
        return (6);
    }


    /* Complete the "name" and "text" sizes */
    ++e_head->name_size;
    ++e_head->text_size;


#if 0

    /* Hack -- extract the "ignore" flags */
    for (i = 0; i < MAX_E_IDX; i++)
    {
        e_ptr = &e_info[i];

        /* Skip non-artifacts */
        if (!e_ptr->name) continue;

        /* Ignore when resistant */
        if (e_ptr->flags2 & TR2_RES_ACID) e_ptr->flags3 |= (TR3_IGNORE_ACID);
        if (e_ptr->flags2 & TR2_RES_ELEC) e_ptr->flags3 |= (TR3_IGNORE_ELEC);
        if (e_ptr->flags2 & TR2_RES_FIRE) e_ptr->flags3 |= (TR3_IGNORE_FIRE);
        if (e_ptr->flags2 & TR2_RES_COLD) e_ptr->flags3 |= (TR3_IGNORE_COLD);

        /* Ignore when immune */
        if (e_ptr->flags2 & TR2_IM_ACID) e_ptr->flags3 |= (TR3_IGNORE_ACID);
        if (e_ptr->flags2 & TR2_IM_ELEC) e_ptr->flags3 |= (TR3_IGNORE_ELEC);
        if (e_ptr->flags2 & TR2_IM_FIRE) e_ptr->flags3 |= (TR3_IGNORE_FIRE);
        if (e_ptr->flags2 & TR2_IM_COLD) e_ptr->flags3 |= (TR3_IGNORE_COLD);
    }

#endif


    /* No version yet */
    if (!okay) return (2);


    /* Success */
    return (0);
}


/*
 * Grab one (basic) flag in a CMonsterRace from a textual string
 */
static errr grab_one_basic_flag(CMonsterRace *r_ptr, char *what)
{
    int i;

    /* Scan flags1 */
    for (i = 0; i < 32; i++) {
        if (streq(what, r_info_flags1[i])) {
            r_ptr->flags1 |= (1L << i);
            return (0);
        }
    }

    /* Scan flags2 */
    for (i = 0; i < 32; i++) {
        if (streq(what, r_info_flags2[i])) {
            r_ptr->flags2 |= (1L << i);
            return (0);
        }
    }

    /* Scan flags1 */
    for (i = 0; i < 32; i++)
    {
        if (streq(what, r_info_flags3[i]))
        {
            r_ptr->flags3 |= (1L << i);
            return (0);
        }
    }

    /* Oops */
    quit(format("Unknown monster flag '%s'.", what));

    /* Failure */
    return (1);
}


/*
 * Grab one (spell) flag in a CMonsterRace from a textual string
 */
static errr grab_one_spell_flag(CMonsterRace *r_ptr, char *what)
{
    int i;

    /* Scan flags4 */
    for (i = 0; i < 32; i++)
    {
        if (streq(what, r_info_flags4[i]))
        {
            r_ptr->flags4 |= (1L << i);
            return (0);
        }
    }

    /* Scan flags5 */
    for (i = 0; i < 32; i++)
    {
        if (streq(what, r_info_flags5[i]))
        {
            r_ptr->flags5 |= (1L << i);
            return (0);
        }
    }

    /* Scan flags6 */
    for (i = 0; i < 32; i++)
    {
        if (streq(what, r_info_flags6[i]))
        {
            r_ptr->flags6 |= (1L << i);
            return (0);
        }
    }

    /* Oops */
    quit(format("Unknown monster flag '%s'.", what));

    /* Failure */
    return (1);
}




/*
 * Initialize the "r_info" array, by parsing an ascii "template" file
 */
errr init_r_info_txt(FILE *fp, char *buf)
{
    int i;

    char *s, *t;

    /* Not ready yet */
    bool okay = FALSE;

    /* Current entry */
    CMonsterRace *r_ptr = NULL;


    /* Just before the first record */
    error_idx = -1;

    /* Just before the first line */
    error_line = -1;


    /* Start the "fake" stuff */
    r_head->name_size = 0;
    r_head->text_size = 0;

    /* Parse */
    while (0 == my_fgets(fp, buf, 1024)) {
        /* Advance the line number */
        error_line++;

        /* Skip comments and blank lines */
        if (!buf[0] || (buf[0] == '#')) continue;

        /* Verify correct "colon" format */
        if (buf[1] != ':') return (1);


        /* Hack -- Process 'V' for "Version" */
        if (buf[0] == 'V') {
            /* Verify the string */
            if (strcmp(buf+2, VERSION_STRING)) {
                return 2;
            }

            /* Okay to proceed */
            okay = TRUE;

            /* Continue */
            continue;
        }

        /* No version yet */
        if (!okay) return (2);


        /* Process 'N' for "New/Number/Name" */
        if (buf[0] == 'N') {
            /* Find the colon before the name */
            s = strchr(buf+2, ':');

            /* Verify that colon */
            if (!s) return (1);

            /* Nuke the colon, advance to the name */
            *s++ = '\0';

            /* Paranoia -- require a name */
            if (!*s) return (1);

            /* Get the index */
            i = atoi(buf+2);

            /* Verify information */
            if (i < error_idx) return 4;

            /* Verify information */
            if (i >= MAX_R_IDX) return 2;

            /* Save the index */
            error_idx = i;

            /* Point at the "info" */
            r_ptr = &r_info[i];

            /* Hack -- Verify space */
	// MV -- Inserted a casting to u16b in order to avoid a compile warning
            if (r_head->name_size + (u16b) strlen(s) > fake_name_size - 8) return (7);

            /* Advance and Save the name index */
            if (!r_ptr->name) r_ptr->name = ++r_head->name_size;

            /* Append chars to the name */
            strcpy(r_name + r_head->name_size, s);

            /* Advance the index */
            r_head->name_size += strlen(s);

            /* Next... */
            continue;
        }

        /* There better be a current r_ptr */
        if (!r_ptr) return (3);


        // Process 'G' for "Graphics" (one line only)
        if (buf[0] == 'G') {
            r_ptr->tile = locate_tile(buf+2);
            if (r_ptr->tile == -1) quit(format("Bad r_info tile: %s", buf+2));
            continue;
        }
        
        
        /* Process 'D' for "Description" */
        if (buf[0] == 'D') {
            /* Acquire the text */
            s = buf+2;

            /* Hack -- Verify space */
	// MV -- Inserted a casting to u16b in order to avoid a compile warning
            if (r_head->text_size + (u16b) strlen(s) > fake_text_size - 8) return (7);

            /* Advance and Save the text index */
            if (!r_ptr->text) r_ptr->text = ++r_head->text_size;

            /* Append chars to the name */
            strcpy(r_text + r_head->text_size, s);

            /* Advance the index */
            r_head->text_size += strlen(s);

            /* Next... */
            continue;
        }

        /* Process 'I' for "Info" (one line only) */
        if (buf[0] == 'I') {
            int spd, hp1, hp2, aaf, ac, slp;

            /* Scan for the other values */
            if (6 != sscanf(buf+2, "%d:%dd%d:%d:%d:%d",
                &spd, &hp1, &hp2, &aaf, &ac, &slp)) return (1);

            /* Save the values */
            r_ptr->speed = spd;
            r_ptr->hdice = hp1;
            r_ptr->hside = hp2;
            r_ptr->aaf = aaf;
            r_ptr->ac = ac;
            r_ptr->sleep = slp;

            /* Next... */
            continue;
        }

        /* Process 'W' for "More Info" (one line only) */
        if (buf[0] == 'W')
        {
            int lev, rar, pad;
            long exp;

            /* Scan for the values */
            if (4 != sscanf(buf+2, "%d:%d:%d:%ld",
                &lev, &rar, &pad, &exp)) return (1);

            /* Save the values */
            r_ptr->level = lev;
            r_ptr->rarity = rar;
            r_ptr->extra = pad;
            r_ptr->mexp = exp;

            /* Next... */
            continue;
        }

        /* Process 'B' for "Blows" (up to four lines) */
        if (buf[0] == 'B') {
            int n1, n2;

            /* Find the next empty blow slot (if any) */
            for (i = 0; i < 4; i++) if (!r_ptr->blow[i].method) break;

            /* Oops, no more slots */
            if (i == 4) return (1);

            /* Analyze the first field */
            for (s = t = buf+2; *t && (*t != ':'); t++) ;

            /* Terminate the field (if necessary) */
            if (*t == ':') *t++ = '\0';

            /* Analyze the method */
            for (n1 = 0; r_info_blow_method[n1]; n1++)
            {
                if (streq(s, r_info_blow_method[n1])) break;
            }

            /* Invalid method */
            if (!r_info_blow_method[n1]) return (1);

            /* Analyze the second field */
            for (s = t; *t && (*t != ':'); t++) ;

            /* Terminate the field (if necessary) */
            if (*t == ':') *t++ = '\0';

            /* Analyze effect */
            for (n2 = 0; r_info_blow_effect[n2]; n2++)
            {
                if (streq(s, r_info_blow_effect[n2])) break;
            }

            /* Invalid effect */
            if (!r_info_blow_effect[n2]) return (1);

            /* Analyze the third field */
            for (s = t; *t && (*t != 'd'); t++) ;

            /* Terminate the field (if necessary) */
            if (*t == 'd') *t++ = '\0';

            /* Save the method */
            r_ptr->blow[i].method = n1;

            /* Save the effect */
            r_ptr->blow[i].effect = n2;

            /* Extract the damage dice and sides */
            r_ptr->blow[i].d_dice = atoi(s);
            r_ptr->blow[i].d_side = atoi(t);

            /* Next... */
            continue;
        }

        /* Process 'F' for "Basic Flags" (multiple lines) */
        if (buf[0] == 'F') {
            /* Parse every entry */
            for (s = buf + 2; *s; ) {
                /* Find the end of this entry */
                for (t = s; *t && (*t != ' ') && (*t != '|'); ++t) ;

                /* Nuke and skip any dividers */
                if (*t) {
                    *t++ = '\0';
                    while (*t == ' ' || *t == '|') t++;
                }

                /* Parse this entry */
                if (0 != grab_one_basic_flag(r_ptr, s)) return (5);

                /* Start the next entry */
                s = t;
            }

            /* Next... */
            continue;
        }

        /* Process 'S' for "Spell Flags" (multiple lines) */
        if (buf[0] == 'S') {
            /* Parse every entry */
            for (s = buf + 2; *s; ) {
                /* Find the end of this entry */
                for (t = s; *t && (*t != ' ') && (*t != '|'); ++t) ;

                /* Nuke and skip any dividers */
                if (*t) {
                    *t++ = '\0';
                    while ((*t == ' ') || (*t == '|')) t++;
                }

                /* XXX XXX XXX Hack -- Read spell frequency */
                if (1 == sscanf(s, "1_IN_%d", &i)) {
                    /* Extract a "frequency" */
                    r_ptr->freq_spell = r_ptr->freq_inate = 100 / i;

                    /* Start at next entry */
                    s = t;

                    /* Continue */
                    continue;
                }

                /* Parse this entry */
                if (0 != grab_one_spell_flag(r_ptr, s)) return (5);

                /* Start the next entry */
                s = t;
            }

            /* Next... */
            continue;
        }

        if (buf[0] == 'T') {
            int n;

            // Search for the monster type
            for (n = 0; r_info_type[n]; n++) {
                if (streq(r_info_type[n], buf+2)) break;
            }

            // If not found, error
            if (!r_info_type[n]) return 1;

            // Assign it
            r_ptr->type = n;

            // Next...
            continue;
        }

        /* Oops */
        return (6);
    }


    /* Complete the "name" and "text" sizes */
    ++r_head->name_size;
    ++r_head->text_size;


    /* No version yet */
    if (!okay) return (2);


    /* Success */
    return (0);
}
