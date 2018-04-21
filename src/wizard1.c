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
    (*val) = obj_value(q_ptr);


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
            if (k_ptr->gen_flags & (OFG_INSTA_ART)) continue;

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
    o_ptr->mult = a_ptr->mult;
    o_ptr->to_a = a_ptr->to_a;
    o_ptr->to_h = a_ptr->to_h;
    o_ptr->to_d = a_ptr->to_d;
    o_ptr->weight = a_ptr->weight;

    /* Success */
    return (TRUE);
}

static void spoil_artifact_desc(void)
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

            obj_identify_fully(&forge);

            obj_display_doc(&forge, doc);
            doc_newline(doc);
        }
    }

    doc_display(doc, "Artifact Spoilers", 0);
    doc_free(doc);
    spoiler_hack = FALSE;
}

#define ART_RANDOM -1
#define ART_EGO    -2
#define ART_STD_BEGIN 1

typedef struct {
    int  id;
    char name[MAX_NLEN];
    int  score;
    int  k_idx; /* For rand-arts and egos ... */
} _art_info_t, *_art_info_ptr;
static int _art_score_cmp(_art_info_ptr l, _art_info_ptr r)
{
    if (l->score > r->score)
        return -1;
    if (l->score < r->score)
        return 1;
    return 0;
}

static int _term_width(void)
{
    int cx, cy;
    Term_get_size(&cx, &cy);
    return cx;
}

typedef bool (*_obj_p)(object_type *o_ptr);

#define _SPOIL_ARTS      0x01
#define _SPOIL_RAND_ARTS 0x02
#define _SPOIL_EGOS      0x04
static void _spoil_table_aux(doc_ptr doc, cptr title, _obj_p pred, int options)
{
    int i;
    vec_ptr entries = vec_alloc(free);
    int     ct_std = 0, ct_rnd = 0, ct_ego = 0;
    int     score_std = 0, score_rnd = 0, score_ego = 0;
    int     max_score_std = 0, max_score_rnd = 0, max_score_ego = 0;

    if ((options & _SPOIL_ARTS) && !random_artifacts)
    {
        for (i = 1; i < max_a_idx; ++i)
        {
            object_type    forge = {0};
            _art_info_ptr  entry;

            if (!p_ptr->wizard && (a_info[i].gen_flags & OFG_QUESTITEM)) continue;
            if (!create_named_art_aux(i, &forge)) continue;
            if ((options & _SPOIL_EGOS) && !a_info[i].found) continue; /* Hack */
            if (pred && !pred(&forge)) continue;

            obj_identify_fully(&forge);

            entry = malloc(sizeof(_art_info_t));
            entry->id = i;
            if (p_ptr->prace == RACE_ANDROID)
            {
                entry->score = android_obj_exp(&forge);
                if (!entry->score)
                    entry->score = obj_value_real(&forge);
            }
            else
                entry->score = obj_value_real(&forge);
            object_desc(entry->name, &forge, OD_COLOR_CODED);
            entry->k_idx = forge.k_idx;
            vec_add(entries, entry);

            if (a_info[entry->id].found)
            {
                ct_std++;
                score_std += entry->score;
                if (entry->score > max_score_std)
                    max_score_std = entry->score;
            }
        }
    }
    if (options & _SPOIL_RAND_ARTS)
    {
        vec_ptr v = stats_rand_arts();
        for (i = 0; i < vec_length(v); i++)
        {
            object_type   *o_ptr = vec_get(v, i);
            _art_info_ptr  entry;

            if (pred && !pred(o_ptr)) continue;

            entry = malloc(sizeof(_art_info_t));
            entry->id = ART_RANDOM;
            if (p_ptr->prace == RACE_ANDROID)
            {
                entry->score = android_obj_exp(o_ptr);
                if (!entry->score)
                    entry->score = obj_value_real(o_ptr);
            }
            else
                entry->score = obj_value_real(o_ptr);
            object_desc(entry->name, o_ptr, OD_COLOR_CODED);
            entry->k_idx = o_ptr->k_idx;
            vec_add(entries, entry);

            ct_rnd++;
            score_rnd += entry->score;
            if (entry->score > max_score_rnd)
                max_score_rnd = entry->score;
        }
    }
    if (options & _SPOIL_EGOS)
    {
        vec_ptr v = stats_egos();
        for (i = 0; i < vec_length(v); i++)
        {
            object_type   *o_ptr = vec_get(v, i);
            _art_info_ptr  entry;

            if (pred && !pred(o_ptr)) continue;

            entry = malloc(sizeof(_art_info_t));
            entry->id = ART_EGO;
            if (p_ptr->prace == RACE_ANDROID)
            {
                entry->score = android_obj_exp(o_ptr);
                if (!entry->score)
                    entry->score = obj_value_real(o_ptr);
            }
            else
                entry->score = obj_value_real(o_ptr);
            object_desc(entry->name, o_ptr, OD_COLOR_CODED);
            entry->k_idx = o_ptr->k_idx;
            vec_add(entries, entry);

            ct_ego++;
            score_ego += entry->score;
            if (entry->score > max_score_ego)
                max_score_ego = entry->score;
        }
    }
    if (vec_length(entries))
    {
        vec_sort(entries, (vec_cmp_f)_art_score_cmp);

        doc_printf(doc, "<topic:%s><style:heading>%s</style>\n\n", title, title);
        doc_insert(doc, "<style:wide>     <color:G>  Score Lvl Rty Cts Object Description</color>\n");
        for (i = 0; i < vec_length(entries); i++)
        {
            _art_info_ptr  entry = vec_get(entries, i);

            if (entry->id == ART_RANDOM)
            {
                doc_printf(doc, "<color:v>%3d) %7d</color>         %3d ", i+1, entry->score, k_info[entry->k_idx].counts.found);
                doc_printf(doc, "<indent><style:indent>%s</style></indent>\n", entry->name);
            }
            else if (entry->id == ART_EGO)
            {
                doc_printf(doc, "<color:B>%3d) %7d</color>         %3d ", i+1, entry->score, k_info[entry->k_idx].counts.found);
                doc_printf(doc, "<indent><style:indent>%s</style></indent>\n", entry->name);
            }
            else
            {
            artifact_type *a_ptr = &a_info[entry->id];

                doc_printf(doc, "<color:%c>%3d) %7d</color> %3d %3d ",
                    (a_ptr->found) ? 'y' : 'w',
                    i+1, entry->score, a_ptr->level, a_ptr->rarity);

                if (a_ptr->gen_flags & OFG_INSTA_ART)
                    doc_insert(doc, "    ");
                else
                    doc_printf(doc, "%3d ", k_info[entry->k_idx].counts.found);
                doc_printf(doc, "<indent><style:indent>%s <color:D>#%d</color></style></indent>\n", entry->name, entry->id);
            }
        }

        if (ct_std || ct_rnd || ct_ego)
        {
            doc_printf(doc, "\n<color:G>%20.20s   Ct Average    Best</color>\n", "");
            if (ct_std)
            {
                doc_printf(doc, "<color:B>%20.20s</color> %4d %7d %7d\n",
                    "Stand Arts", ct_std, score_std/ct_std, max_score_std);
            }
            if (ct_rnd)
            {
                doc_printf(doc, "<color:B>%20.20s</color> %4d %7d %7d\n",
                    "Rand Arts", ct_rnd, score_rnd/ct_rnd, max_score_rnd);
            }
            if (ct_ego)
            {
                doc_printf(doc, "<color:B>%20.20s</color> %4d %7d %7d\n",
                    "Egos", ct_ego, score_ego/ct_ego, max_score_ego);
            }
        }
        doc_insert(doc, "</style>\n\n");
    }
    vec_free(entries);
}

static void _spoil_artifact_table_aux(doc_ptr doc, cptr title, _obj_p pred)
{
    _spoil_table_aux(doc, title, pred, _SPOIL_ARTS | _SPOIL_RAND_ARTS);
}

static void _spoil_object_table_aux(doc_ptr doc, cptr title, _obj_p pred)
{
    _spoil_table_aux(doc, title, pred, _SPOIL_ARTS | _SPOIL_RAND_ARTS | _SPOIL_EGOS);
}

static void spoil_artifact_tables(void)
{
    doc_ptr doc = doc_alloc(MIN(100, _term_width()));

    spoiler_hack = TRUE;
    _spoil_artifact_table_aux(doc, "All Artifacts", NULL);
    _spoil_artifact_table_aux(doc, "Weapons", object_is_melee_weapon);
    _spoil_artifact_table_aux(doc, "Shields", object_is_shield);
    _spoil_artifact_table_aux(doc, "Bows", object_is_bow);
    _spoil_artifact_table_aux(doc, "Rings", object_is_ring);
    _spoil_artifact_table_aux(doc, "Amulets", object_is_amulet);
    _spoil_artifact_table_aux(doc, "Lights", object_is_lite);
    _spoil_artifact_table_aux(doc, "Body Armor", object_is_body_armour);
    _spoil_artifact_table_aux(doc, "Cloaks", object_is_cloak);
    _spoil_artifact_table_aux(doc, "Helmets", object_is_helmet);
    _spoil_artifact_table_aux(doc, "Gloves", object_is_gloves);
    _spoil_artifact_table_aux(doc, "Boots", object_is_boots);
    spoiler_hack = FALSE;

    doc_display(doc, "Artifact Tables", 0);
    doc_free(doc);
}

static void spoil_object_tables(void)
{
    doc_ptr doc = doc_alloc(MIN(100, _term_width()));

    spoiler_hack = TRUE;
    _spoil_object_table_aux(doc, "All Objects", NULL);
    _spoil_object_table_aux(doc, "Weapons", object_is_melee_weapon);
    _spoil_object_table_aux(doc, "Shields", object_is_shield);
    _spoil_object_table_aux(doc, "Bows", object_is_bow);
    _spoil_object_table_aux(doc, "Rings", object_is_ring);
    _spoil_object_table_aux(doc, "Amulets", object_is_amulet);
    _spoil_object_table_aux(doc, "Lights", object_is_lite);
    _spoil_object_table_aux(doc, "Body Armor", object_is_body_armour);
    _spoil_object_table_aux(doc, "Cloaks", object_is_cloak);
    _spoil_object_table_aux(doc, "Helmets", object_is_helmet);
    _spoil_object_table_aux(doc, "Gloves", object_is_gloves);
    _spoil_object_table_aux(doc, "Boots", object_is_boots);
    spoiler_hack = FALSE;

    doc_display(doc, "Object Tables", 0);
    doc_free(doc);
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



static int _compare_r_level(monster_race *l, monster_race *r)
{
    if (l->level < r->level) return -1;
    if (l->level > r->level) return 1;
    if (l->mexp < r->mexp) return -1;
    if (l->mexp > r->mexp) return 1;
    if (l->id < r->id) return -1;
    if (l->id > r->id) return 1;
    return 0;
}
static int _compare_r_level_desc(monster_race *l, monster_race *r)
{
    return -_compare_r_level(l, r);
}
static void spoil_mon_info()
{
    int     i;
    vec_ptr v = vec_alloc(NULL);
    doc_ptr doc = doc_alloc(80);

    spoiler_hack = TRUE;

    for (i = 1; i < max_r_idx; i++)
    {
        monster_race *r_ptr = &r_info[i];
        if (!r_ptr->name) continue;
        if (r_ptr->id == MON_MONKEY_CLONE) continue;
        if (r_ptr->id == MON_KAGE) continue;
        vec_add(v, r_ptr);
    }
    vec_sort(v, (vec_cmp_f)_compare_r_level_desc);

    for (i = 0; i < vec_length(v); i++)
    {
        monster_race *r_ptr = vec_get(v, i);
        doc_printf(doc, "<topic:%s><color:r>=====================================================================</color>\n", r_name + r_ptr->name);
        mon_display_doc(r_ptr, doc);
        doc_newline(doc);
    }
    vec_free(v);

    doc_display(doc, "Monster Spoilers", 0);
    doc_free(doc);

    spoiler_hack = FALSE;
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

static bool _check_realm(int class_idx, int realm_idx)
{
    int bit = (1 << (realm_idx-1)); /* cf CH_LIFE and REALM_LIFE (etc) in defines.h */
    if (realm_choices1[class_idx] & bit)
        return TRUE;
    if (realm_choices2[class_idx] & bit)
        return TRUE;
    if (class_idx == CLASS_SORCERER || class_idx == CLASS_RED_MAGE)
    {
        if (is_magic(realm_idx) && realm_idx != REALM_NECROMANCY)
            return TRUE;
    }
    return FALSE;
}

static void _spoil_spell_book(doc_ptr doc, int class_idx, int realm_idx, int book_idx)
{
    int           spell_idx;
    int           k_idx = lookup_kind(realm2tval(realm_idx), book_idx);
    player_magic *magic_ptr = &m_info[class_idx];

    doc_printf(doc, "<color:o>%-25.25s</color><color:G> Lvl Cst Fail  </color>\n", k_name + k_info[k_idx].name);
    for (spell_idx = book_idx*8; spell_idx < (book_idx+1)*8; spell_idx++)
    {
        magic_type *spell_ptr = NULL;
        if (is_magic(realm_idx))
            spell_ptr = &magic_ptr->info[realm_idx - 1][spell_idx];
        else
            spell_ptr = &technic_info[realm_idx - MIN_TECHNIC][spell_idx];

        if (0 < spell_ptr->slevel && spell_ptr->slevel <= PY_MAX_LEVEL)
        {
            doc_printf(doc, "%-25.25s %3d %3d %3d%%\n",
                do_spell(realm_idx, spell_idx, SPELL_NAME),
                spell_ptr->slevel,
                spell_ptr->smana,
                spell_ptr->sfail
            );
        }
        else
        {
            doc_printf(doc, "<color:D>%-26.26s</color>\n", "Illegible");
        }
    }
    doc_newline(doc);
}

static int  _cmp_class_name(int left_idx, int right_idx)
{
    class_t *l = get_class_aux(left_idx, 0);
    class_t *r = get_class_aux(right_idx, 0);
    return strcmp(l->name, r->name);
}

static void spoil_spells_by_class(void)
{
    int i, realm_idx;
    doc_ptr doc = doc_alloc(80);
    vec_ptr vec = vec_alloc(NULL);

    for (i = 0; i < MAX_CLASS; i++)
        vec_add_int(vec, i);

    vec_sort(vec, (vec_cmp_f)_cmp_class_name);

    for (i = 0; i < vec_length(vec); i++)
    {
        int           class_idx = vec_get_int(vec, i);
        class_t      *class_ptr = get_class_aux(class_idx, 0);
        bool          class_heading = FALSE;

        if (class_idx == CLASS_RAGE_MAGE) continue; /* broken */

        for (realm_idx = REALM_LIFE; realm_idx <= MAX_REALM; realm_idx++)
        {
            if (_check_realm(class_idx, realm_idx))
            {
                doc_ptr cols[2];

                cols[0] = doc_alloc(40);
                cols[1] = doc_alloc(40);

                _spoil_spell_book(cols[0], class_idx, realm_idx, 0);
                _spoil_spell_book(cols[1], class_idx, realm_idx, 1);
                if (class_idx != CLASS_RED_MAGE || realm_idx == REALM_ARCANE)
                {
                    _spoil_spell_book(cols[0], class_idx, realm_idx, 2);
                    _spoil_spell_book(cols[1], class_idx, realm_idx, 3);
                }

                if (!class_heading)
                {
                    doc_printf(doc, "<topic:%s><color:r>%s</color>\n", class_ptr->name, class_ptr->name);
                    doc_printf(doc, "%s\n\n", class_ptr->desc);
                    class_heading = TRUE;
                }
                doc_printf(doc, "<color:B>%s</color>\n", realm_names[realm_idx]);

                doc_insert_cols(doc, cols, 2, 0);

                doc_free(cols[0]);
                doc_free(cols[1]);
            }
        }
    }

    doc_display(doc, "Spells by Class", 0);
    doc_free(doc);
    vec_free(vec);
}

static void _spoil_spell_book2(doc_ptr doc, int class1_idx, int class2_idx, int realm_idx, int book_idx)
{
    int           spell_idx;
    int           k_idx = lookup_kind(realm2tval(realm_idx), book_idx);
    player_magic *magic1_ptr = &m_info[class1_idx];
    player_magic *magic2_ptr = &m_info[class2_idx];

    doc_printf(doc, "%-25.25s <color:G>%-12.12s</color>  <color:R>%-12.12s</color>\n",
        "", get_class_aux(class1_idx, 0)->name, get_class_aux(class2_idx, 0)->name);
    doc_printf(doc, "<color:o>%-25.25s</color><color:G> Lvl Cst Fail</color>  <color:R>Lvl Cst Fail</color>\n", k_name + k_info[k_idx].name);
    for (spell_idx = book_idx*8; spell_idx < (book_idx+1)*8; spell_idx++)
    {
        magic_type *spell1_ptr = &magic1_ptr->info[realm_idx - 1][spell_idx];
        magic_type *spell2_ptr = &magic2_ptr->info[realm_idx - 1][spell_idx];

        doc_printf(doc, "%-25.25s ", do_spell(realm_idx, spell_idx, SPELL_NAME));
        if (1 <= spell1_ptr->slevel && spell1_ptr->slevel <= PY_MAX_LEVEL)
            doc_printf(doc, "%3d %3d %3d%%  ", spell1_ptr->slevel, spell1_ptr->smana, spell1_ptr->sfail);
        else
            doc_insert(doc, "<color:D>Illegible</color>     ");
        if (1 <= spell2_ptr->slevel && spell2_ptr->slevel <= PY_MAX_LEVEL)
            doc_printf(doc, "%3d %3d %3d%%  ", spell2_ptr->slevel, spell2_ptr->smana, spell2_ptr->sfail);
        else
            doc_insert(doc, "<color:D>Illegible</color>     ");
        doc_newline(doc);
    }
    doc_newline(doc);
}

static void _spoil_spells_by_realm_aux3(int realm_idx, int class1_idx, int class2_idx)
{
    int i;
    doc_ptr doc = doc_alloc(80);

    for (i = 0; i < 4; i++)
        _spoil_spell_book2(doc, class1_idx, class2_idx, realm_idx, i);

    doc_display(doc, "Spells by Realm", 0);
    doc_free(doc);
}

static void _spoil_spells_by_realm_aux2(int realm_idx, int class1_idx)
{
    int i, row, col, class_idx, choice;
    vec_ptr vec = vec_alloc(NULL);

    for (class_idx = 0; class_idx < MAX_CLASS; class_idx++)
    {
        if (_check_realm(class_idx, realm_idx))
            vec_add_int(vec, class_idx);
    }

    vec_sort(vec, (vec_cmp_f)_cmp_class_name);

    while (1)
    {
        Term_clear();

        c_prt(TERM_L_BLUE, format("%s", realm_names[realm_idx]), 2, 0);
        c_prt(TERM_L_BLUE, format("First Class: %s", get_class_aux(class1_idx, 0)->name), 3, 0);

        /* Classes */
        row = 4;
        col = 2;
        c_prt(TERM_RED, "Second Class", row++, col - 2);

        for (i = 0; i < vec_length(vec); i++)
        {
            int      class_idx = vec_get_int(vec, i);
            class_t *class_ptr = get_class_aux(class_idx, 0);

            prt(format("(%c) %s", 'a' + i, class_ptr->name), row++, col);
        }

        i = inkey();
        if (i == ESCAPE) break;
        choice = i - 'a';

        if (0 <= choice && choice < vec_length(vec))
        {
            class_idx = vec_get_int(vec, choice);
            _spoil_spells_by_realm_aux3(realm_idx, class1_idx, class_idx);
        }
     }

    vec_free(vec);
}

static void _spoil_spells_by_realm_aux1(int realm_idx)
{
    int i, row, col, class_idx, choice;
    vec_ptr vec = vec_alloc(NULL);

    for (class_idx = 0; class_idx < MAX_CLASS; class_idx++)
    {
        if (_check_realm(class_idx, realm_idx))
            vec_add_int(vec, class_idx);
    }

    vec_sort(vec, (vec_cmp_f)_cmp_class_name);

    while (1)
    {
        Term_clear();

        c_prt(TERM_L_BLUE, format("%s", realm_names[realm_idx]), 2, 0);

        /* Classes */
        row = 4;
        col = 2;
        c_prt(TERM_RED, "First Class", row++, col - 2);

        for (i = 0; i < vec_length(vec); i++)
        {
            int      class_idx = vec_get_int(vec, i);
            class_t *class_ptr = get_class_aux(class_idx, 0);

            prt(format("(%c) %s", 'a' + i, class_ptr->name), row++, col);
        }

        i = inkey();
        if (i == ESCAPE) break;
        choice = i - 'a';

        if (0 <= choice && choice < vec_length(vec))
        {
            class_idx = vec_get_int(vec, choice);
            _spoil_spells_by_realm_aux2(realm_idx, class_idx);
        }
     }

    vec_free(vec);
}

static void spoil_spells_by_realm(void)
{
    int i, row, col, realm_idx;
    while (1)
    {
        Term_clear();

        prt("Realm Spoilers", 2, 0);

        /* Realms */
        row = 4;
        col = 2;
        c_prt(TERM_RED, "Realms", row++, col - 2);

        for (realm_idx = REALM_LIFE; realm_idx <= MAX_MAGIC; realm_idx++)
        {
            prt(format("(%c) %s", 'a' + realm_idx - 1, realm_names[realm_idx]), row++, col);
        }

        i = inkey();
        if (i == ESCAPE) break;
        realm_idx = i - 'a' + 1;

        if (REALM_LIFE <= realm_idx && realm_idx <= MAX_MAGIC)
            _spoil_spells_by_realm_aux1(realm_idx);
     }
}

/*
 * Create Spoiler files -BEN-
 * Converted from File Creation to Document Creation, with online
 * viewing (as well as the option for HTML output).  -CHRIS-
 */
void do_cmd_spoilers(void)
{
    int i, row, col;

    screen_save();

    while (1)
    {
        Term_clear();

        prt("View Spoilers", 2, 0);

        /* Give some choices */
        row = 4;
        col = 2;
        c_prt(TERM_RED, "Object Spoilers", row++, col - 2);
        prt("(a) Artifact Descriptions", row++, col);
        prt("(A) Artifact Tables", row++, col);
        prt("(o) Objects", row++, col);
        prt("(O) Object Tables", row++, col);
        row++;

        c_prt(TERM_RED, "Monster Spoilers", row++, col - 2);
        prt("(m) Brief Descriptions", row++, col);
        prt("(M) Full Descriptions", row++, col);
        prt("(e) Evolution", row++, col);
        row++;

        c_prt(TERM_RED, "Class Spoilers", row++, col - 2);
        prt("(s) Spells by Class", row++, col);
        prt("(r) Spells by Realm", row++, col);
        row++;

        /* Prompt */
        prt("ESC) Exit menu", 21, 1);
        prt("Command: ", 20, 0);

        /* Prompt */
        i = inkey();

        /* Done */
        if (i == ESCAPE) break;
        switch (i)
        {
        /* Object Spoilers */
        case 'a':
            spoil_artifact_desc();
            break;
        case 'A':
            spoil_artifact_tables();
            break;
        case 'O':
            spoil_object_tables();
            break;
        case 'o':
            spoil_obj_desc("obj-desc.spo");
            break;

        /* Monster Spoilers */
        case 'm':
            spoil_mon_desc("mon-desc.spo");
            break;
        case 'M':
            spoil_mon_info();
            break;
        case 'e':
            spoil_mon_evol("mon-evol.spo");
            break;

        /* Class Spoilers */
        case 's':
            spoil_spells_by_class();
            break;
        case 'r':
            spoil_spells_by_realm();
            break;

        /* Oops */
        default:
            bell();
            break;
        }

        /* Flush messages */
        msg_print(NULL);
    }
    screen_load();
}


#else

#ifdef MACINTOSH
static int i = 0;
#endif

#endif
