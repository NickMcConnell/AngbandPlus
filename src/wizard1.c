/* File: wizard1.c */

/*
 * Copyright (c) 1997 Ben Harrison, and others
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies. Other copyrights may also apply.
 */

/* Purpose: Spoiler generation -BEN- */

#include "angband.h"


#ifdef ALLOW_SPOILERS


/*
 * The spoiler file being created
 */
static FILE *fff = NULL;



/*
 * Extract a textual representation of an attribute
 */
static cptr attr_to_text(monster_race *r_ptr)
{
    if (r_ptr->flags1 & RF1_ATTR_CLEAR)    return "Clear";
    if (r_ptr->flags1 & RF1_ATTR_MULTI)    return "Multi";
    if (r_ptr->flags1 & RF1_ATTR_SEMIRAND) return "S.Rand";

    switch (r_ptr->d_attr)
    {
    case TERM_DARK:    return "xxx";
    case TERM_WHITE:   return "White";
    case TERM_SLATE:   return "Slate";
    case TERM_ORANGE:  return "Orange";
    case TERM_RED:     return "Red";
    case TERM_GREEN:   return "Green";
    case TERM_BLUE:    return "Blue";
    case TERM_UMBER:   return "Umber";
    case TERM_L_DARK:  return "L.Dark";
    case TERM_L_WHITE: return "L.Slate";
    case TERM_VIOLET:  return "Violet";
    case TERM_YELLOW:  return "Yellow";
    case TERM_L_RED:   return "L.Red";
    case TERM_L_GREEN: return "L.Green";
    case TERM_L_BLUE:  return "L.Blue";
    case TERM_L_UMBER: return "L.Umber";
    }

    /* Oops */
    return "Icky";
}



/*
 * A tval grouper
 */
typedef struct
{
    byte tval;
    cptr name;
} grouper;



/*
 * Item Spoilers by: benh@phial.com (Ben Harrison)
 */


/*
 * The basic items categorized by type
 */
static grouper group_item[] =
{
    { TV_SHOT,          "Ammo" },

    { TV_ARROW,         NULL },
    { TV_BOLT,          NULL },

    { TV_BOW,           "Bows" },

    { TV_DIGGING,       "Weapons" },

    { TV_POLEARM,       NULL },
    { TV_HAFTED,        NULL },
    { TV_SWORD,         NULL },

    { TV_SOFT_ARMOR,    "Armour (Body)" },

    { TV_HARD_ARMOR,    NULL },
    { TV_DRAG_ARMOR,    NULL },

    { TV_BOOTS,         "Armour (Misc)" },

    { TV_GLOVES,        NULL },
    { TV_HELM,          NULL },
    { TV_CROWN,         NULL },
    { TV_SHIELD,        NULL },
    { TV_CLOAK,         NULL },

    { TV_LITE,          "Light Sources" },
    { TV_AMULET,        "Amulets" },
    { TV_RING,          "Rings" },

    { TV_STAFF,         "Staffs" },
    { TV_WAND,          "Wands" },
    { TV_ROD,           "Rods" },

    { TV_SCROLL,        "Scrolls" },
    { TV_POTION,        "Potions" },
    { TV_FOOD,          "Food" },

    { TV_LIFE_BOOK,     "Books (Life)" },
    { TV_SORCERY_BOOK,  "Books (Sorcery)" },
    { TV_NATURE_BOOK,   "Books (Nature)" },
    { TV_CHAOS_BOOK,    "Books (Chaos)" },
    { TV_DEATH_BOOK,    "Books (Death)" },
    { TV_TRUMP_BOOK,    "Books (Trump)" },
    { TV_ARCANE_BOOK,   "Books (Arcane)" },
    { TV_CRAFT_BOOK,    "Books (Craft)" },
    { TV_DAEMON_BOOK,   "Books (Daemon)" },
    { TV_CRUSADE_BOOK,  "Books (Crusade)" },
    { TV_NECROMANCY_BOOK, "Books (Necromancy)" },
    { TV_ARMAGEDDON_BOOK, "Books (Armageddon)" },
    { TV_MUSIC_BOOK,    "Song Books" },
    { TV_HISSATSU_BOOK, "Books (Kendo)" },
    { TV_HEX_BOOK,      "Books (Hex)" },
    { TV_RAGE_BOOK,      "Books (Rage)" },

    { TV_WHISTLE,       "Whistle" },
    { TV_CAPTURE,       "Capture Ball" },
    { TV_CARD,          "Express Card" },

    { TV_CHEST,         "Chests" },

    { TV_FIGURINE,      "Magical Figurines" },
    { TV_STATUE,        "Statues" },
    { TV_CORPSE,        "Corpses" },

    { TV_SKELETON,      "Misc" },

    { TV_BOTTLE,        NULL },
    { TV_JUNK,          NULL },
    { TV_SPIKE,         NULL },
    { TV_FLASK,         NULL },
    { TV_PARCHMENT,     NULL },

    { 0, "" }
};


/*
 * Describe the kind
 */
static void kind_info(char *buf, char *dam, char *wgt, int *lev, s32b *val, int k)
{
    object_type forge;
    object_type *q_ptr;


    /* Get local object */
    q_ptr = &forge;

    /* Prepare a fake item */
    object_prep(q_ptr, k);

    /* It is known */
    q_ptr->ident |= (IDENT_KNOWN);

    /* Cancel bonuses */
    q_ptr->pval = 0;
    q_ptr->to_a = 0;
    q_ptr->to_h = 0;
    q_ptr->to_d = 0;


    /* Level */
    (*lev) = k_info[q_ptr->k_idx].level;

    /* Value */
    (*val) = object_value(q_ptr);


    /* Hack */
    if (!buf || !dam || !wgt) return;


    /* Description (too brief) */
    object_desc(buf, q_ptr, (OD_NAME_ONLY | OD_STORE));


    /* Misc info */
    strcpy(dam, "");

    /* Damage */
    switch (q_ptr->tval)
    {
        /* Bows */
        case TV_BOW:
        {
            break;
        }

        /* Ammo */
        case TV_SHOT:
        case TV_BOLT:
        case TV_ARROW:
        {
            sprintf(dam, "%dd%d", q_ptr->dd, q_ptr->ds);
            break;
        }

        /* Weapons */
        case TV_HAFTED:
        case TV_POLEARM:
        case TV_SWORD:
        case TV_DIGGING:
        {
            sprintf(dam, "%dd%d", q_ptr->dd, q_ptr->ds);
            break;
        }

        /* Armour */
        case TV_BOOTS:
        case TV_GLOVES:
        case TV_CLOAK:
        case TV_CROWN:
        case TV_HELM:
        case TV_SHIELD:
        case TV_SOFT_ARMOR:
        case TV_HARD_ARMOR:
        case TV_DRAG_ARMOR:
        {
            sprintf(dam, "%d", q_ptr->ac);
            break;
        }
    }


    /* Weight */
    sprintf(wgt, "%3d.%d", q_ptr->weight / 10, q_ptr->weight % 10);
}


/*
 * Create a spoiler file for items
 */
static void spoil_obj_desc(cptr fname)
{
    int i, k, s, t, n = 0, group_start = 0;

    u16b who[200];

    char buf[1024];

    char wgt[80];
    char dam[80];


    /* Build the filename */
    path_build(buf, sizeof(buf), ANGBAND_DIR_USER, fname);

    /* File type is "TEXT" */
    FILE_TYPE(FILE_TYPE_TEXT);

    /* Open the file */
    fff = my_fopen(buf, "w");

    /* Oops */
    if (!fff)
    {
        msg_print("Cannot create spoiler file.");
        return;
    }


    /* Header */
    fprintf(fff, "Spoiler File -- Basic Items (PosChengband %d.%d.%d)\n\n\n",
        VER_MAJOR, VER_MINOR, VER_PATCH);

    /* More Header */
    fprintf(fff, "%-45s     %8s%7s%5s%9s\n",
        "Description", "Dam/AC", "Wgt", "Lev", "Cost");
    fprintf(fff, "%-45s     %8s%7s%5s%9s\n",
        "----------------------------------------",
        "------", "---", "---", "----");

    /* List the groups */
    for (i = 0; TRUE; i++)
    {
        /* Write out the group title */
        if (group_item[i].name)
        {
            if (n)
            {
                /* Hack -- bubble-sort by cost and then level */
                for (s = 0; s < n - 1; s++)
                {
                    for (t = 0; t < n - 1; t++)
                    {
                        int i1 = t;
                        int i2 = t + 1;

                        int e1;
                        int e2;

                        s32b t1;
                        s32b t2;

                        kind_info(NULL, NULL, NULL, &e1, &t1, who[i1]);
                        kind_info(NULL, NULL, NULL, &e2, &t2, who[i2]);

                        if ((t1 > t2) || ((t1 == t2) && (e1 > e2)))
                        {
                            int tmp = who[i1];
                            who[i1] = who[i2];
                            who[i2] = tmp;
                        }
                    }
                }

                fprintf(fff, "\n\n%s\n\n", group_item[group_start].name);

                /* Spoil each item */
                for (s = 0; s < n; s++)
                {
                    int e;
                    s32b v;

                    /* Describe the kind */
                    kind_info(buf, dam, wgt, &e, &v, who[s]);

                    /* Dump it */
                    fprintf(fff, "     %-45s%8s%7s%5d%9d\n",
                        buf, dam, wgt, e, v);
                }

                /* Start a new set */
                n = 0;
            }

            /* Notice the end */
            if (!group_item[i].tval) break;

            /* Start a new set */
            group_start = i;
        }

        /* Acquire legal item types */
        for (k = 1; k < max_k_idx; k++)
        {
            object_kind *k_ptr = &k_info[k];

            /* Skip wrong tval's */
            if (k_ptr->tval != group_item[i].tval) continue;

            /* Hack -- Skip instant-artifacts */
            if (k_ptr->gen_flags & (TRG_INSTA_ART)) continue;

            /* Save the index */
            who[n++] = k;
        }
    }


    /* Check for errors */
    if (ferror(fff) || my_fclose(fff))
    {
        msg_print("Cannot close spoiler file.");
        return;
    }

    /* Message */
    msg_print("Successfully created a spoiler file.");
}

/*
 * The artifacts categorized by type
 */
static grouper group_artifact[] =
{
    { TV_SWORD,             "Edged-Weapons" },
    { TV_POLEARM,           "Polearms" },
    { TV_HAFTED,            "Hafted-Weapons" },
    { TV_DIGGING,           "Diggers" },
    { TV_BOW,               "Bows" },
    { TV_ARROW,             "Ammo" },

    { TV_SOFT_ARMOR,        "Body-Armor" },
    { TV_HARD_ARMOR,        NULL },
    { TV_DRAG_ARMOR,        NULL },

    { TV_CLOAK,             "Cloaks" },
    { TV_SHIELD,            "Shields" },
    { TV_CARD,              NULL },
    { TV_HELM,              "Headgear" },
    { TV_CROWN,             NULL },
    { TV_GLOVES,            "Gloves" },
    { TV_BOOTS,             "Boots" },

    { TV_LITE,              "Light-Sources" },
    { TV_AMULET,            "Amulets" },
    { TV_RING,              "Rings" },

    { 0, NULL }
};

/*
 * Hack -- Create a "forged" artifact
 * Compare to create_named_art_aux() in artifact.c
 */
static bool make_fake_artifact(object_type *o_ptr, int name1)
{
    int i;

    artifact_type *a_ptr = &a_info[name1];


    /* Ignore "empty" artifacts */
    if (!a_ptr->name) return FALSE;

    /* Acquire the "kind" index */
    i = lookup_kind(a_ptr->tval, a_ptr->sval);

    /* Oops */
    if (!i) return (FALSE);

    /* Create the artifact */
    object_prep(o_ptr, i);

    /* Save the name */
    o_ptr->name1 = name1;

    /* Extract the fields */
    o_ptr->pval = a_ptr->pval;
    o_ptr->ac = a_ptr->ac;
    o_ptr->dd = a_ptr->dd;
    o_ptr->ds = a_ptr->ds;
    o_ptr->to_a = a_ptr->to_a;
    o_ptr->to_h = a_ptr->to_h;
    o_ptr->to_d = a_ptr->to_d;
    o_ptr->weight = a_ptr->weight;

    /* Success */
    return (TRUE);
}

static void spoil_artifact_doc(void)
{
    int i,j;
    doc_ptr doc = doc_alloc(80);

    spoiler_hack = TRUE;
    for (i = 0; group_artifact[i].tval; i++)
    {
        if (group_artifact[i].name)
        {
            if (i) doc_insert(doc, "</indent>\n");
            doc_printf(doc, "<topic:%s><style:heading>%s</style>\n  <indent>\n",
                group_artifact[i].name, group_artifact[i].name);
        }

        for (j = 1; j < max_a_idx; ++j)
        {
            artifact_type *a_ptr = &a_info[j];
            object_type    forge = {0};

            if (a_ptr->tval != group_artifact[i].tval) continue;

            if (!make_fake_artifact(&forge, j)) continue;

            identify_item(&forge);
            forge.ident |= IDENT_FULL;

            obj_display_doc(&forge, doc);
            doc_newline(doc);
        }
    }

    doc_display(doc, "Artifact Spoilers", 0);
    doc_free(doc);
    spoiler_hack = FALSE;
}

/*
 * Create a spoiler file for monsters   -BEN-
 */
static void spoil_mon_desc(cptr fname)
{
    int i, n = 0;

    u16b why = 2;
    s16b *who;

    char buf[1024];

    char nam[80];
    char lev[80];
    char rar[80];
    char spd[80];
    char ac[80];
    char hp[80];
    char exp[80];

    /* Build the filename */
    path_build(buf, sizeof(buf), ANGBAND_DIR_USER, fname);

    /* File type is "TEXT" */
    FILE_TYPE(FILE_TYPE_TEXT);

    /* Open the file */
    fff = my_fopen(buf, "w");

    /* Oops */
    if (!fff)
    {
        msg_print("Cannot create spoiler file.");
        return;
    }

    /* Allocate the "who" array */
    C_MAKE(who, max_r_idx, s16b);

    /* Dump the header */
    fprintf(fff, "Spoiler File -- Monsters (PosChengband %d.%d.%d)\n\n\n",
        VER_MAJOR, VER_MINOR, VER_PATCH);
    fprintf(fff, "------------------------------------------\n\n");

    /* Dump the header */
    fprintf(fff, "    %-38.38s%4s%4s%4s%7s%5s  %11.11s\n",
        "Name", "Lev", "Rar", "Spd", "Hp", "Ac", "Visual Info");
    fprintf(fff, "%-42.42s%4s%4s%4s%7s%5s  %11.11s\n",
        "--------", "---", "---", "---", "--", "--", "-----------");


    /* Scan the monsters */
    for (i = 1; i < max_r_idx; i++)
    {
        monster_race *r_ptr = &r_info[i];

        /* Use that monster */
        if (r_ptr->name) who[n++] = i;
    }

    /* Select the sort method */
    ang_sort_comp = ang_sort_comp_hook;
    ang_sort_swap = ang_sort_swap_hook;

    /* Sort the array by dungeon depth of monsters */
    ang_sort(who, &why, n);

    /* Scan again */
    for (i = 0; i < n; i++)
    {
        monster_race *r_ptr = &r_info[who[i]];

        cptr name = (r_name + r_ptr->name);
        if (r_ptr->flags7 & (RF7_KAGE)) continue;

        /* Get the "name" */
        /*
        else if (r_ptr->flags1 & (RF1_QUESTOR))
        {
            sprintf(nam, "[Q] %s", name);
        }
        */
        else if (r_ptr->flags1 & (RF1_UNIQUE))
        {
            sprintf(nam, "[U] %s", name);
        }
        else
        {
            sprintf(nam, "The %s", name);
        }


        /* Level */
        sprintf(lev, "%d", r_ptr->level);

        /* Rarity */
        sprintf(rar, "%d", r_ptr->rarity);

        /* Speed */
        if (r_ptr->speed >= 110)
        {
            sprintf(spd, "+%d", (r_ptr->speed - 110));
        }
        else
        {
            sprintf(spd, "-%d", (110 - r_ptr->speed));
        }

        /* Armor Class */
        sprintf(ac, "%d", r_ptr->ac);

        /* Hitpoints */
        if ((r_ptr->flags1 & (RF1_FORCE_MAXHP)) || (r_ptr->hside == 1))
        {
            sprintf(hp, "%d", r_ptr->hdice * r_ptr->hside);
        }
        else
        {
            sprintf(hp, "%dd%d", r_ptr->hdice, r_ptr->hside);
        }


        /* Experience */
        sprintf(exp, "%d", r_ptr->mexp);

        /* Hack -- use visual instead */
        sprintf(exp, "%s '%c'", attr_to_text(r_ptr), r_ptr->d_char);

        /* Dump the info */
        fprintf(fff, "%-42.42s%4s%4s%4s%7s%5s  %11.11s\n",
            nam, lev, rar, spd, hp, ac, exp);
    }

    /* End it */
    fprintf(fff, "\n");


    /* Free the "who" array */
    C_KILL(who, max_r_idx, s16b);

    /* Check for errors */
    if (ferror(fff) || my_fclose(fff))
    {
        msg_print("Cannot close spoiler file.");
        return;
    }

    /* Worked */
    msg_print("Successfully created a spoiler file.");
}




/*
 * Monster spoilers by: smchorse@ringer.cs.utsa.edu (Shawn McHorse)
 *
 * Primarily based on code already in mon-desc.c, mostly by -BEN-
 */



/*
 * Buffer text to the given file. (-SHAWN-)
 * This is basically c_roff() from mon-desc.c with a few changes.
 */
static void spoil_out(cptr str)
{
    cptr r;

    /* Line buffer */
    static char roff_buf[256];

    /* Delay buffer */
    static char roff_waiting_buf[256];

    /* Current pointer into line roff_buf */
    static char *roff_p = roff_buf;

    /* Last space saved into roff_buf */
    static char *roff_s = NULL;

    /* Mega-Hack -- Delayed output */
    static bool waiting_output = FALSE;

    /* Special handling for "new sequence" */
    if (!str)
    {
        if (waiting_output)
        {
            fputs(roff_waiting_buf, fff);
            waiting_output = FALSE;
        }

        if (roff_p != roff_buf) roff_p--;
        while (*roff_p == ' ' && roff_p != roff_buf) roff_p--;

        if (roff_p == roff_buf) fprintf(fff, "\n");
        else
        {
            *(roff_p + 1) = '\0';
            fprintf(fff, "%s\n\n", roff_buf);
        }

        roff_p = roff_buf;
        roff_s = NULL;
        roff_buf[0] = '\0';
        return;
    }

    /* Scan the given string, character at a time */
    for (; *str; str++)
    {
        char ch = *str;
        bool wrap = (ch == '\n');

        if (!isprint(ch)) ch = ' ';

        if (waiting_output)
        {
            fputs(roff_waiting_buf, fff);
            if (!wrap) fputc('\n', fff);
            waiting_output = FALSE;
        }

        if (!wrap)
        {
            if (roff_p >= roff_buf + 75) wrap = TRUE;
            else if ((ch == ' ') && (roff_p >= roff_buf + 73)) wrap = TRUE;

            if (wrap)
            {
                cptr tail = str + 1;

                for (; *tail; tail++)
                {
                    if (*tail == ' ') continue;

                    if (isprint(*tail)) break;
                }

                if (!*tail) waiting_output = TRUE;
            }
        }

        /* Handle line-wrap */
        if (wrap)
        {
            *roff_p = '\0';
            r = roff_p;
            if (roff_s && (ch != ' '))
            {
                *roff_s = '\0';
                r = roff_s + 1;
            }
            if (!waiting_output) fprintf(fff, "%s\n", roff_buf);
            else strcpy(roff_waiting_buf, roff_buf);
            roff_s = NULL;
            roff_p = roff_buf;
            while (*r) *roff_p++ = *r++;
        }

        /* Save the char */
        if ((roff_p > roff_buf) || (ch != ' '))
        {
            if (ch == ' ') roff_s = roff_p;

            *roff_p++ = ch;
        }
    }
}



/*
 *  Hook function used in spoil_mon_info()
 */
static void roff_func(byte attr, cptr str)
{
    /* Unused */
    (void)attr;

    spoil_out(str);
}


/*
 * Create a spoiler file for monsters (-SHAWN-)
 */
static void spoil_mon_info(cptr fname)
{
    char buf[1024];
    int i, l, n = 0;
    u32b flags1;

    u16b why = 2;
    s16b *who;

    /* Build the filename */
    path_build(buf, sizeof(buf), ANGBAND_DIR_USER, fname);

    /* File type is "TEXT" */
    FILE_TYPE(FILE_TYPE_TEXT);

    /* Open the file */
    fff = my_fopen(buf, "w");

    /* Oops */
    if (!fff)
    {
        msg_print("Cannot create spoiler file.");
        return;
    }


    /* Dump the header */
    sprintf(buf, "Spoiler File -- Monsters (PosChengband %d.%d.%d)\n\n\n",
        VER_MAJOR, VER_MINOR, VER_PATCH);

    spoil_out(buf);
    spoil_out("------------------------------------------\n\n");

    /* Allocate the "who" array */
    C_MAKE(who, max_r_idx, s16b);

    /* Scan the monsters */
    for (i = 1; i < max_r_idx; i++)
    {
        monster_race *r_ptr = &r_info[i];

        /* Use that monster */
        if (r_ptr->name) who[n++] = i;
    }

    /* Select the sort method */
    ang_sort_comp = ang_sort_comp_hook;
    ang_sort_swap = ang_sort_swap_hook;

    /* Sort the array by dungeon depth of monsters */
    ang_sort(who, &why, n);


    /*
     * List all monsters in order
     */
    for (l = 0; l < n; l++)
    {
        monster_race *r_ptr = &r_info[who[l]];

        /* Extract the flags */
        flags1 = r_ptr->flags1;

        /* Prefix */
        /*
        if (flags1 & (RF1_QUESTOR))
        {
            spoil_out("[Q] ");
        }
        else
        */
        if (flags1 & (RF1_UNIQUE))
        {
            spoil_out("[U] ");
        }
        else
        {
            spoil_out("The ");
        }

        /* Name */
        sprintf(buf, "%s  (", (r_name + r_ptr->name));  /* ---)--- */

        spoil_out(buf);

        /* Color */
        spoil_out(attr_to_text(r_ptr));

        /* Symbol --(-- */
        sprintf(buf, " '%c')\n", r_ptr->d_char);
        spoil_out(buf);


        /* Indent */
        sprintf(buf, "=== ");
        spoil_out(buf);

        /* Number */
        sprintf(buf, "Num:%d  ", who[l]);
        spoil_out(buf);

        /* Level */
        sprintf(buf, "Lev:%d  ", r_ptr->level);
        spoil_out(buf);

        /* Rarity */
        sprintf(buf, "Rar:%d  ", r_ptr->rarity);
        spoil_out(buf);

        /* Speed */
        if (r_ptr->speed >= 110)
        {
            sprintf(buf, "Spd:+%d  ", (r_ptr->speed - 110));
        }
        else
        {
            sprintf(buf, "Spd:-%d  ", (110 - r_ptr->speed));
        }
        spoil_out(buf);

        /* Hitpoints */
        if ((flags1 & (RF1_FORCE_MAXHP)) || (r_ptr->hside == 1))
        {
            sprintf(buf, "Hp:%d  ", r_ptr->hdice * r_ptr->hside);
        }
        else
        {
            sprintf(buf, "Hp:%dd%d  ", r_ptr->hdice, r_ptr->hside);
        }
        spoil_out(buf);

        /* Armor Class */
        sprintf(buf, "Ac:%d  ", r_ptr->ac);
        spoil_out(buf);

        /* Experience */
        sprintf(buf, "Exp:%d\n", r_ptr->mexp);
        spoil_out(buf);

        /* Reuse the code of monster recall. */
        output_monster_spoiler(who[l], roff_func);

        spoil_out(NULL);
    }

    /* Free the "who" array */
    C_KILL(who, max_r_idx, s16b);

    /* Check for errors */
    if (ferror(fff) || my_fclose(fff))
    {
        msg_print("Cannot close spoiler file.");
        return;
    }

    msg_print("Successfully created a spoiler file.");
}



#define MAX_EVOL_DEPTH 64


/*
 * Compare two int-type array like strncmp() and return TRUE if equals
 */
static bool int_n_cmp(int *a, int *b, int length)
{
    /* Null-string comparation is always TRUE */
    if (!length) return TRUE;

    do
    {
        if (*a != *(b++)) return FALSE;
        if (!(*(a++))) break;
    }
    while (--length);

    return TRUE;
}


/*
 * Returns TRUE if an evolution tree is "partial tree"
 */
static bool is_partial_tree(int *tree, int *partial_tree)
{
    int pt_head = *(partial_tree++);
    int pt_len = 0;

    while (partial_tree[pt_len]) pt_len++;

    while (*tree)
    {
        if (*(tree++) == pt_head)
        {
            if (int_n_cmp(tree, partial_tree, pt_len)) return TRUE;
        }
    }

    return FALSE;
}


/*
 * Sorting hook -- Comp function
 */
static bool ang_sort_comp_evol_tree(vptr u, vptr v, int a, int b)
{
    int **evol_tree = (int **)u;

    int w1 = evol_tree[a][0];
    int w2 = evol_tree[b][0];
    monster_race *r1_ptr = &r_info[w1];
    monster_race *r2_ptr = &r_info[w2];

    /* Unused */
    (void)v;

    /* Used tree first */
    if (w1 && !w2) return TRUE;
    if (!w1 && w2) return FALSE;

    /* Sort by monster level */
    if (r1_ptr->level < r2_ptr->level) return TRUE;
    if (r1_ptr->level > r2_ptr->level) return FALSE;

    /* Sort by monster experience */
    if (r1_ptr->mexp < r2_ptr->mexp) return TRUE;
    if (r1_ptr->mexp > r2_ptr->mexp) return FALSE;

    /* Compare indexes */
    return w1 <= w2;
}


/*
 * Sorting hook -- Swap function
 */
static void ang_sort_swap_evol_tree(vptr u, vptr v, int a, int b)
{
    int **evol_tree = (int **)u;
    int *holder;

    /* Unused */
    (void)v;

    /* Swap */
    holder = evol_tree[a];
    evol_tree[a] = evol_tree[b];
    evol_tree[b] = holder;
}


/*
 * Print monsters' evolution information to file
 */
static void spoil_mon_evol(cptr fname)
{
    char buf[1024];
    monster_race *r_ptr;
    int **evol_tree, i, j, n, r_idx;
    int *evol_tree_zero; /* For C_KILL() */

    /* Build the filename */
    path_build(buf, sizeof buf, ANGBAND_DIR_USER, fname);

    /* File type is "TEXT" */
    FILE_TYPE(FILE_TYPE_TEXT);

    /* Open the file */
    fff = my_fopen(buf, "w");

    /* Oops */
    if (!fff)
    {
        msg_print("Cannot create spoiler file.");
        return;
    }

    /* Dump the header */
    sprintf(buf, "Monster Spoilers for PosChengband Version %d.%d.%d\n",
         VER_MAJOR, VER_MINOR, VER_PATCH);

    spoil_out(buf);
    spoil_out("------------------------------------------\n\n");

    /* Allocate the "evol_tree" array (2-dimension) */
    C_MAKE(evol_tree, max_r_idx, int *);
    C_MAKE(*evol_tree, max_r_idx * (MAX_EVOL_DEPTH + 1), int);
    for (i = 1; i < max_r_idx; i++) evol_tree[i] = *evol_tree + i * (MAX_EVOL_DEPTH + 1);
    evol_tree_zero = *evol_tree;

    /* Step 1: Build the evolution tree */
    for (i = 1; i < max_r_idx; i++)
    {
        r_ptr = &r_info[i];

        /* No evolution */
        if (!r_ptr->next_exp) continue;

        /* Trace evolution */
        n = 0;
        evol_tree[i][n++] = i;
        do
        {
            evol_tree[i][n++] = r_ptr->next_r_idx;
            r_ptr = &r_info[r_ptr->next_r_idx];
        }
        while (r_ptr->next_exp && (n < MAX_EVOL_DEPTH));
    }

    /* Step 2: Scan the evolution trees and remove "partial tree" */
    for (i = 1; i < max_r_idx; i++)
    {
        /* Not evolution tree */
        if (!evol_tree[i][0]) continue;

        for (j = 1; j < max_r_idx; j++)
        {
            /* Same tree */
            if (i == j) continue;

            /* Not evolution tree */
            if (!evol_tree[j][0]) continue;

            /* Is evolution tree[i] is part of [j]? */
            if (is_partial_tree(evol_tree[j], evol_tree[i]))
            {
                /* Remove this evolution tree */
                evol_tree[i][0] = 0;
                break;
            }
        }
    }

    /* Step 3: Sort the evolution trees */

    /* Select the sort method */
    ang_sort_comp = ang_sort_comp_evol_tree;
    ang_sort_swap = ang_sort_swap_evol_tree;

    /* Sort the array */
    ang_sort(evol_tree, NULL, max_r_idx);

    /* Step 4: Print the evolution trees */
    for (i = 0; i < max_r_idx; i++)
    {
        r_idx = evol_tree[i][0];

        /* No evolution or removed evolution tree */
        if (!r_idx) continue;

        /* Trace the evolution tree */
        r_ptr = &r_info[r_idx];
        fprintf(fff, "[%d]: %s (Level %d, '%c')\n", r_idx,
            r_name + r_ptr->name, r_ptr->level, r_ptr->d_char);
        for (n = 1; r_ptr->next_exp; n++)
        {
            fprintf(fff, "%*s-(%d)-> ", n * 2, "", r_ptr->next_exp);
            fprintf(fff, "[%d]: ", r_ptr->next_r_idx);
            r_ptr = &r_info[r_ptr->next_r_idx];
            fprintf(fff, "%s (Level %d, '%c')\n",
                r_name + r_ptr->name, r_ptr->level, r_ptr->d_char);
        }

        /* End of evolution tree */
        fputc('\n', fff);
    }

    /* Free the "evol_tree" array (2-dimension) */
    C_KILL(evol_tree_zero, max_r_idx * (MAX_EVOL_DEPTH + 1), int);
    C_KILL(evol_tree, max_r_idx, int *);

    /* Check for errors */
    if (ferror(fff) || my_fclose(fff))
    {
        msg_print("Cannot close spoiler file.");
        return;
    }

    /* Message */
    msg_print("Successfully created a spoiler file.");
}



/*
 * Forward declare
 */
extern void do_cmd_spoilers(void);

/*
 * Create Spoiler files -BEN-
 * Converted from File Creation to Document Creation, with online
 * viewing (as well as the option for HTML output).  -CHRIS-
 */
void do_cmd_spoilers(void)
{
    /* Save the screen */
    screen_save();

    /* Interact */
    while (1)
    {
        /* Clear screen */
        Term_clear();

        /* Info */
        prt("View Spoilers", 2, 0);

        /* Prompt for a file */
        prt("(1) Brief Object Info (obj-desc.spo)", 5, 5);
        prt("(2) Brief Artifact Info", 6, 5);
        prt("(3) Brief Monster Info (mon-desc.spo)", 7, 5);
        prt("(4) Full Monster Info (mon-info.spo)", 8, 5);
        prt("(5) Monster Evolution Info (mon-evol.spo)", 9, 5);

        /* Prompt */
        prt("Command: ", 12, 0);

        /* Get a choice */
        switch (inkey())
        {
        /* Escape */
        case ESCAPE:
            /* Restore the screen */
            screen_load();
            return;

        /* Option (1) */
        case '1':
            spoil_obj_desc("obj-desc.spo");
            break;

        /* Option (2) */
        case '2':
            spoil_artifact_doc();
            break;

        /* Option (3) */
        case '3':
            spoil_mon_desc("mon-desc.spo");
            break;

        /* Option (4) */
        case '4':
            spoil_mon_info("mon-info.spo");
            break;

        /* Option (5) */
        case '5':
            spoil_mon_evol("mon-evol.spo");
            break;

        /* Oops */
        default:
            bell();
            break;
        }

        /* Flush messages */
        msg_print(NULL);
    }
}


#else

#ifdef MACINTOSH
static int i = 0;
#endif

#endif
