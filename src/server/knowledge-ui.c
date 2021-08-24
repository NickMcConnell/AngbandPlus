/*
 * File: knowledge-ui.c
 * Purpose: Various kinds of browsing functions
 *
 * Copyright (c) 1997-2007 Robert A. Koeneke, James E. Wilson, Ben Harrison,
 * Eytan Zweig, Andrew Doull, Pete Mack.
 * Copyright (c) 2004 DarkGod (HTML dump code)
 * Copyright (c) 2016 MAngband and PWMAngband Developers
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
 * The first part of this file contains the knowledge menus. Generic display
 * routines are followed by sections which implement "subclasses" of the
 * abstract classes represented by member_funcs and group_funcs.
 *
 * After the knowledge menus are various knowledge functions - message review;
 * inventory, equipment, monster and object lists; symbol lookup; and the
 * "locate" command which scrolls the screen around the current dungeon level.
 */


/*
 * Helper class for generating joins
 */
typedef struct join
{
    int oid;
    int gid;
} join_t;


/*
 * A default group-by
 */
static join_t *default_join;


/*
 * Knowledge menu utilities
 */


/*
 * Return a specific ordering for the features
 */
static int feat_order(int feat)
{
    struct feature *f = &f_info[feat];
    int special = feat_order_special(feat);

    if (special != -1)
        return special;

    switch (f->d_char)
    {
        case '.': return 0;
        case '\'':
        case '+': return 1;
        case '<':
        case '>': return 2;
        case '#': return 3;
        case '*':
        case '%': return 4;
        case ';':
        case ':': return 5;
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9': return 6;
        default: return 7;
    }
}


/*
 * Return a specific ordering for the traps
 */
static int trap_order(int trap)
{
    const struct trap_kind *t = &trap_info[trap];

    if (trf_has(t->flags, TRF_RUNE)) return 0;
    if (trf_has(t->flags, TRF_LOCK)) return 1;
    if (trf_has(t->flags, TRF_TRAP)) return 2;
    return 3;
}


/*
 * MONSTERS
 */


/*
 * Description of each monster group.
 */
static struct
{
    const char *chars;
    const char *name;
} monster_group[] =
{
    {(const char *)-1,  "Uniques"},
    {"A",               "Ainur"},
    {"a",               "Ants"},
    {"b",               "Bats"},
    {"B",               "Birds"},
    {"C",               "Canines"},
    {"c",               "Centipedes"},
    {"uU",              "Demons"},
    {"dD",              "Dragons"},
    {"vE",              "Elementals/Vortices"},
    {"e",               "Eyes"},
    {"f",               "Felines"},
    {"G",               "Ghosts"},
    {"OP",              "Giants/Ogres"},
    {"g",               "Golems"},
    {"h",               "Humanoids"},
    {"tp",              "Humans"},
    {"H",               "Hybrids"},
    {"M",               "Hydras"},
    {"i",               "Icky Things"},
    {"lFI",             "Insects"},
    {"j",               "Jellies"},
    {"K",               "Killer Beetles"},
    {"k",               "Kobolds"},
    {"L",               "Lichs"},
    {".$|(!+x?><=~",    "Mimics"},
    {"m",               "Molds"},
    {",",               "Mushroom Patches"},
    {"n",               "Nagas"},
    {"o",               "Orcs"},
    {"q",               "Quadrupeds"},
    {"Q",               "Quylthulgs"},
    {"R",               "Reptiles/Amphibians"},
    {"r",               "Rodents"},
    {"S",               "Scorpions/Spiders"},
    {"s",               "Skeletons"},
    {"J",               "Snakes"},
    {"T",               "Trolls"},
    {"V",               "Vampires"},
    {"W",               "Wights/Wraiths"},
    {"w",               "Worms/Worm Masses"},
    {"X",               "Xorns/Xarens"},
    {"y",               "Yeeks"},
    {"Y",               "Yeti"},
    {"Z",               "Zephyr Hound"},
    {"z",               "Zombies"},
    {NULL,              NULL}
};


static int m_cmp_race(const void *a, const void *b)
{
    const struct monster_race *r_a = &r_info[default_join[*(const int *)a].oid];
    const struct monster_race *r_b = &r_info[default_join[*(const int *)b].oid];
    int gid = default_join[*(const int *)a].gid;

    /* Group by */
    int c = gid - default_join[*(const int *)b].gid;

    if (c) return c;

    /* Order results */
    c = r_a->d_char - r_b->d_char;
    if (c && gid != 0)
    {
        /* UNIQUE group is ordered by level & name only */
        /* Others by order they appear in the group symbols */
        return strchr(monster_group[gid].chars, r_a->d_char) -
            strchr(monster_group[gid].chars, r_b->d_char);
    }
    c = r_a->level - r_b->level;
    if (c) return c;

    return strcmp(r_a->name, r_b->name);
}


/*
 * Display known monsters
 */
static void do_cmd_knowledge_monsters(struct player *p, int line)
{
    int *monsters;
    int m_count = 0;
    int i;
    size_t j;
    char file_name[MSG_LEN];
    ang_file *fff;
    int m_group = -1;

    /* Temporary file */
    fff = file_temp(file_name, sizeof(file_name));
    if (!fff) return;

    for (i = 1; i < z_info->r_max; i++)
    {
        struct monster_race *race = &r_info[i];
        struct monster_lore *lore = get_lore(p, race);

        if (!lore->pseen) continue;
        if (!race->name) continue;

        if (rf_has(race->flags, RF_UNIQUE)) m_count++;

        for (j = 1; j < N_ELEMENTS(monster_group) - 1; j++)
        {
            const char *pat = monster_group[j].chars;

            if (strchr(pat, race->d_char)) m_count++;
        }
    }

    default_join = mem_zalloc(m_count * sizeof(join_t));
    monsters = mem_zalloc(m_count * sizeof(int));

    /* Collect matching monsters */
    m_count = 0;
    for (i = 1; i < z_info->r_max; i++)
    {
        struct monster_race *race = &r_info[i];
        struct monster_lore *lore = get_lore(p, race);

        if (!lore->pseen) continue;
        if (!race->name) continue;

        for (j = 0; j < N_ELEMENTS(monster_group) - 1; j++)
        {
            const char *pat = monster_group[j].chars;

            if ((j == 0) && !rf_has(race->flags, RF_UNIQUE)) continue;
            else if ((j > 0) && !strchr(pat, race->d_char)) continue;

            monsters[m_count] = m_count;
            default_join[m_count].oid = i;
            default_join[m_count++].gid = j;
        }
    }

    /* Sort by kills (and level) */
    sort(monsters, m_count, sizeof(*monsters), m_cmp_race);

    /* Print the monsters */
    for (i = 0; i < m_count; i++)
    {
        int oid = default_join[monsters[i]].oid;
        int gid = default_join[monsters[i]].gid;
        char kills[6];

        /* Access the race */
        struct monster_race *race = &r_info[oid];
        struct monster_lore *lore = get_lore(p, race);

        /* Find graphics bits */
        byte a = p->r_attr[oid];
        char c = p->r_char[oid];

        /* Paranoia */
        if (gid == -1) continue;

        /* Print group */
        if (gid != m_group)
        {
            m_group = gid;
            file_putf(fff, "w%s\n", monster_group[m_group].name);
        }

        /* If uniques are purple, make it so */
        if (OPT_P(p, purple_uniques) && rf_has(race->flags, RF_UNIQUE) && !(a & 0x80))
            a = COLOUR_VIOLET;

        /* Display kills */
        if (rf_has(race->flags, RF_UNIQUE))
            my_strcpy(kills, (lore->pkills? " dead": "alive"), sizeof(kills));
        else
            strnfmt(kills, sizeof(kills), "%5d", lore->pkills);

        /* Print a message */
        if (p->tile_distorted)
            file_putf(fff, "w     %-40s  %s\n", race->name, kills);
        else
        {
            file_putf(fff, "d%c%cw%-40s  %s\n", ((a & 0x80)? a: color_attr_to_char(a)), c,
                race->name, kills);
        }
    }

    mem_free(default_join);
    mem_free(monsters);

    /* Close the file */
    file_close(fff);

    /* Display the file contents */
    show_file(p, file_name, "Monsters", line, 1);

    /* Remove the file */
    file_delete(file_name);
}


/*
 * ARTIFACTS
 */


/*
 * These are used for all the object sections
 */
static const grouper object_text_order[] =
{
    {TV_RING, "Ring"},
    {TV_AMULET, "Amulet"},
    {TV_POTION, "Potion"},
    {TV_SCROLL, "Scroll"},
    {TV_WAND, "Wand"},
    {TV_STAFF, "Staff"},
    {TV_ROD, "Rod"},
    {TV_FOOD, "Food"},
    {TV_MUSHROOM, "Mushroom"},
    {TV_CROP, "Crop"},
    {TV_MAGIC_BOOK, "Magic Book"},
    {TV_PRAYER_BOOK, "Prayer Book"},
    {TV_SORCERY_BOOK, "Sorcery Book"},
    {TV_SHADOW_BOOK, "Shadow Book"},
    {TV_HUNT_BOOK, "Hunt Book"},
    {TV_PSI_BOOK, "Psi Book"},
    {TV_DEATH_BOOK, "Death Book"},
    {TV_ELEM_BOOK, "Elemental Book"},
    {TV_SUMMON_BOOK, "Summoning book"},
    {TV_LIGHT, "Light"},
    {TV_SWORD, "Sword"},
    {TV_POLEARM, "Polearm"},
    {TV_HAFTED, "Hafted"},
    {TV_MSTAFF, "Mage Staff"},
    {TV_BOW, "Shooter"},
    {TV_ARROW, "Ammunition (firing)"},
    {TV_BOLT, NULL},
    {TV_SHOT, NULL},
    {TV_ROCK, "Ammunition (throwing)"},
    {TV_SHIELD, "Shield"},
    {TV_CROWN, "Crown"},
    {TV_HELM, "Helm"},
    {TV_GLOVES, "Gloves"},
    {TV_BOOTS, "Boots"},
    {TV_CLOAK, "Cloak"},
    {TV_DRAG_ARMOR, "Dragon Armour"},
    {TV_HARD_ARMOR, "Hard Armour"},
    {TV_SOFT_ARMOR, "Soft Armour"},
    {TV_HORN, "Horn"},
    {TV_DIGGING, "Digger"},
    {TV_CHEST, "Chest"},
    {TV_SKELETON, "Junk/Other"},
    {TV_BOTTLE, NULL},
    {TV_STONE, NULL},
    {TV_CORPSE, NULL},
    {TV_DEED, NULL},
    {TV_FLASK, NULL},
    {0, NULL}
};


static int *obj_group_order = NULL;


struct cmp_art
{
    const struct artifact *artifact;
    char highlight;
    char owner[NORMAL_WID];
};


static int a_cmp_tval(const void *a, const void *b)
{
    const struct cmp_art *pa = a;
    const struct cmp_art *pb = b;
    const struct artifact *a_a = pa->artifact;
    const struct artifact *a_b = pb->artifact;

    /* Group by */
    int ta = obj_group_order[a_a->tval];
    int tb = obj_group_order[a_b->tval];
    int c = ta - tb;

    if (c) return c;

    /* Order by */
    c = a_a->sval - a_b->sval;
    if (c) return c;
    return strcmp(a_a->name, a_b->name);
}


static char highlight_unknown(struct player *p, int a_idx)
{
    /* Artifact is unknown */
    if (p->art_info[a_idx] < ARTS_FOUND) return 'd';

    /* Artifact is discarded and unfindable again */
    if (p->art_info[a_idx] > cfg_preserve_artifacts) return 'r';

    /* Artifact is discarded and findable again */
    return 'g';
}


/*
 * Collects the list of known artifacts
 *
 * Highlighting:
 *    'w': artifact is owned by the player
 *    'r': artifact is discarded and unfindable again
 *    'g': artifact is discarded and findable again
 *    'D': artifact is not owned or owned by someone else
 */
static int collect_known_artifacts(struct player *p, struct cmp_art *artifacts)
{
    int a_count = 0;
    int i, y, x;
    struct object *obj;
    char *highlights = mem_zalloc(z_info->a_max * sizeof(char));
    char *owners = mem_zalloc(z_info->a_max * NORMAL_WID * sizeof(char));

    /* Check inventory and equipment of players */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *player = player_get(i);

        /* Check this guy */
        for (obj = player->gear; obj; obj = obj->next)
        {
            /* Ignore non-artifacts */
            if (!true_artifact_p(obj)) continue;

            /* Note owner */
            strnfmt(&owners[obj->artifact->aidx * NORMAL_WID], NORMAL_WID, " (%s)", player->name);

            /* Artifact is owned by the player */
            if (player == p) highlights[obj->artifact->aidx] = 'w';

            /* Artifact is owned by someone else */
            else highlights[obj->artifact->aidx] = 'D';
        }
    }

    /* Check the dungeon */
    for (i = 0 - MAX_WILD; i < z_info->max_depth; i++)
    {
        struct chunk *c = chunk_get(i);

        if (!c) continue;

        for (y = 0; y < c->height; y++)
        {
            for (x = 0; x < c->width; x++)
            {
                for (obj = square_object(c, y, x); obj; obj = obj->next)
                {
                    /* Ignore non-artifacts */
                    if (!true_artifact_p(obj)) continue;

                    /* Note location */
                    strnfmt(&owners[obj->artifact->aidx * NORMAL_WID], NORMAL_WID, " (%d ft)",
                        c->depth * 50);

                    /* Artifact is owned by the player */
                    if (obj->owner == p->id) highlights[obj->artifact->aidx] = 'w';

                    /* Artifact is not owned or owned by someone else */
                    else if (object_is_known(p, obj)) highlights[obj->artifact->aidx] = 'D';

                    /* Artifact is unknown */
                    else
                        highlights[obj->artifact->aidx] = highlight_unknown(p, obj->artifact->aidx);
                }
            }
        }
    }

    /* Collect matching artifacts */
    for (i = 0; i < z_info->a_max; i++)
    {
        const struct artifact *art = &a_info[i];
        char h = highlights[i];

        /* Skip "empty" artifacts */
        if (!art->name) continue;

        /* Skip "uncreated + unfound" artifacts */
        if ((p->art_info[i] < ARTS_FOUND) && !art->created) continue;

        /* Artifact is "uncreated" */
        if (!art->created) h = highlight_unknown(p, i);

        /* Special case: artifact is owned by a non-connected player */
        if (!h)
        {
            /* Artifact is owned */
            if (art->owned) h = 'D';

            /* Artifact is unknown */
            else h = highlight_unknown(p, i);
        }

        /* Artifact is known */
        if (h != 'd')
        {
            int c = obj_group_order[art->tval];

            if (c >= 0)
            {
                artifacts[a_count].artifact = art;
                artifacts[a_count].highlight = h;
                my_strcpy(artifacts[a_count].owner, &owners[i * NORMAL_WID], NORMAL_WID);
                a_count++;
            }
        }
    }

    /* Free memory */
    mem_free(highlights);
    mem_free(owners);

    return a_count;
}


/*
 * Display known artifacts
 */
static void do_cmd_knowledge_artifacts(struct player *p, int line)
{
    struct cmp_art *artifacts;
    int a_count = 0;
    int i;
    char file_name[MSG_LEN];
    ang_file *fff;
    int a_group = -1;

    /* Temporary file */
    fff = file_temp(file_name, sizeof(file_name));
    if (!fff) return;

    artifacts = mem_zalloc(z_info->a_max * sizeof(struct cmp_art));

    /* Initialize other static variables */
    if (!obj_group_order)
    {
        int gid = -1;

        obj_group_order = mem_zalloc(TV_MAX * sizeof(int));

        /* Allow for missing values */
        for (i = 0; i < TV_MAX; i++) obj_group_order[i] = -1;

        for (i = 0; 0 != object_text_order[i].tval; i++)
        {
            if (object_text_order[i].name) gid = i;
            obj_group_order[object_text_order[i].tval] = gid;
        }
    }

    /* Collect valid artifacts */
    a_count = collect_known_artifacts(p, artifacts);

    /* Sort */
    sort(artifacts, a_count, sizeof(*artifacts), a_cmp_tval);

    /* Print the artifacts */
    for (i = 0; i < a_count; i++)
    {
        const struct artifact *art = artifacts[i].artifact;
        char o_name[NORMAL_WID];
        int gid = obj_group_order[art->tval];
        struct object *fake = object_new();

        /* Paranoia */
        if (gid == -1) continue;
        if (!make_fake_artifact(fake, (struct artifact *)art)) continue;

        /* Print group */
        if (gid != a_group)
        {
            a_group = gid;
            file_putf(fff, "w%s\n", object_text_order[a_group].name);
        }

        /* Describe the artifact */
        object_desc(p, o_name, sizeof(o_name), fake, ODESC_BASE | ODESC_ARTIFACT);

        /* Print a message */
        /* Dungeon Masters see extra info */
        if (is_dm_p(p))
            file_putf(fff, "%c     The %s%s\n", artifacts[i].highlight, o_name, artifacts[i].owner);
        else
            file_putf(fff, "%c     The %s\n", artifacts[i].highlight, o_name);

        object_delete(&fake);
    }

    mem_free(artifacts);
    mem_free(obj_group_order);
    obj_group_order = NULL;

    /* Close the file */
    file_close(fff);

    /* Display the file contents */
    show_file(p, file_name, "Artifacts", line, 1);

    /* Remove the file */
    file_delete(file_name);
}


/*
 * EGO ITEMS
 */


static int e_cmp_tval(const void *a, const void *b)
{
    const struct ego_item *ea = &e_info[default_join[*(const int *)a].oid];
    const struct ego_item *eb = &e_info[default_join[*(const int *)b].oid];

    /* Group by */
    int c = default_join[*(const int *)a].gid - default_join[*(const int *)b].gid;

    if (c) return c;

    /* Order by */
    return strcmp(ea->name, eb->name);
}


/*
 * Display known ego items
 */
static void do_cmd_knowledge_ego_items(struct player *p, int line)
{
    int *egoitems;
    int e_count = 0;
    int i;
    int max_pairs = z_info->e_max * N_ELEMENTS(object_text_order);
    char file_name[MSG_LEN];
    ang_file *fff;
    int e_group = -1;

    /* Temporary file */
    fff = file_temp(file_name, sizeof(file_name));
    if (!fff) return;

    egoitems = mem_zalloc(max_pairs * sizeof(int));
    default_join = mem_zalloc(max_pairs * sizeof(join_t));

    /* Initialize other static variables */
    if (!obj_group_order)
    {
        int gid = -1;

        obj_group_order = mem_zalloc(TV_MAX * sizeof(int));

        /* Allow for missing values */
        for (i = 0; i < TV_MAX; i++) obj_group_order[i] = -1;

        for (i = 0; 0 != object_text_order[i].tval; i++)
        {
            if (object_text_order[i].name) gid = i;
            obj_group_order[object_text_order[i].tval] = gid;
        }
    }

    /* Look at all the ego items */
    for (i = 1; i < z_info->e_max; i++)
    {
        struct ego_item *ego = &e_info[i];
        size_t j;
        int *tval;
        struct ego_poss_item *poss;

        /* Ignore non-egos && unknown egos */
        if (!(ego->name && p->ego_everseen[i])) continue;

        tval = mem_zalloc(N_ELEMENTS(object_text_order) * sizeof(int));

        /* Note the tvals which are possible for this ego */
        for (poss = ego->poss_items; poss; poss = poss->next)
        {
            struct object_kind *kind = &k_info[poss->kidx];

            tval[obj_group_order[kind->tval]]++;
        }

        /* Count and put into the list */
        for (j = 0; j < N_ELEMENTS(object_text_order); j++)
        {
            int gid = obj_group_order[j];

            /* Ignore duplicate gids */
            if ((j > 0) && (gid == default_join[e_count - 1].gid)) continue;

            if (tval[gid])
            {
                egoitems[e_count] = e_count;
                default_join[e_count].oid = i;
                default_join[e_count++].gid = gid;
            }
        }

        mem_free(tval);
    }

    /* Sort */
    sort(egoitems, e_count, sizeof(*egoitems), e_cmp_tval);

    /* Print the ego items */
    for (i = 0; i < e_count; i++)
    {
        struct ego_item *ego = &e_info[default_join[egoitems[i]].oid];
        int gid = default_join[egoitems[i]].gid;

        /* Paranoia */
        if (gid == -1) continue;

        /* Print group */
        if (gid != e_group)
        {
            e_group = gid;
            file_putf(fff, "%s\n", object_text_order[e_group].name);
        }

        /* Print a message */
        file_putf(fff, "     %s\n", ego->name);
    }

    mem_free(default_join);
    mem_free(egoitems);
    mem_free(obj_group_order);
    obj_group_order = NULL;

    /* Close the file */
    file_close(fff);

    /* Display the file contents */
    show_file(p, file_name, "Ego Items", line, 0);

    /* Remove the file */
    file_delete(file_name);
}


/*
 * ORDINARY OBJECTS
 */


struct cmp_obj
{
    struct player *player;
    const struct object_kind *kind;
};


static int o_cmp_tval(const void *a, const void *b)
{
    const struct cmp_obj *pa = a;
    const struct cmp_obj *pb = b;
    const struct object_kind *k_a = pa->kind;
    const struct object_kind *k_b = pb->kind;

    /* Group by */
    int ta = obj_group_order[k_a->tval];
    int tb = obj_group_order[k_b->tval];
    int c = ta - tb;

    if (c) return c;

    /* Order by */
    c = pa->player->obj_aware[k_a->kidx] - pb->player->obj_aware[k_b->kidx];
    if (c) return -c;

    switch (k_a->tval)
    {
        case TV_LIGHT:
        case TV_MAGIC_BOOK:
        case TV_PRAYER_BOOK:
        case TV_SORCERY_BOOK:
        case TV_SHADOW_BOOK:
        case TV_HUNT_BOOK:
        case TV_PSI_BOOK:
        case TV_DEATH_BOOK:
        case TV_ELEM_BOOK:
        case TV_SUMMON_BOOK:
        {
            /* Leave sorted by sval */
            break;
        }

        default:
        {
            if (pa->player->obj_aware[k_a->kidx]) return strcmp(k_a->name, k_b->name);

            /* Then in tried order */
            c = pa->player->obj_tried[k_a->kidx] - pb->player->obj_tried[k_b->kidx];
            if (c) return -c;

            return strcmp(k_a->flavor->text, k_b->flavor->text);
        }
    }

    return k_a->sval - k_b->sval;
}


/*
 * Display known objects
 */
static void do_cmd_knowledge_objects(struct player *p, int line)
{
    struct cmp_obj *objects;
    int o_count = 0;
    int i;
    char file_name[MSG_LEN];
    ang_file *fff;
    int o_group = -1;

    /* Temporary file */
    fff = file_temp(file_name, sizeof(file_name));
    if (!fff) return;

    objects = mem_zalloc(z_info->k_max * sizeof(struct cmp_obj));

    /* Initialize other static variables */
    if (!obj_group_order)
    {
        int gid = -1;

        obj_group_order = mem_zalloc(TV_MAX * sizeof(int));

        /* Allow for missing values */
        for (i = 0; i < TV_MAX; i++) obj_group_order[i] = -1;

        for (i = 0; 0 != object_text_order[i].tval; i++)
        {
            if (object_text_order[i].name) gid = i;
            obj_group_order[object_text_order[i].tval] = gid;
        }
    }

    /* Collect matching objects */
    for (i = 1; i < z_info->k_max; i++)
    {
        struct object_kind *kind = &k_info[i];

        /*
         * It's in the list if we've ever seen it, or it has a flavour,
         * and either it's not one of the special artifacts, or if it is,
         * we're not aware of it yet. This way the flavour appears in the list
         * until it is found.
         */
        if ((p->kind_everseen[i] || kind->flavor) &&
            (!kf_has(kind->kind_flags, KF_INSTA_ART) || !p->obj_aware[i]))
        {
            int c = obj_group_order[kind->tval];

            if (c >= 0)
            {
                objects[o_count].player = p;
                objects[o_count++].kind = kind;
            }
        }
    }

    /* Sort */
    sort(objects, o_count, sizeof(*objects), o_cmp_tval);

    /* Print the objects */
    for (i = 0; i < o_count; i++)
    {
        const struct object_kind *kind = objects[i].kind;
        char o_name[NORMAL_WID];
        int gid = obj_group_order[kind->tval];

        /* Choose a color */
        bool aware = p->obj_aware[kind->kidx];
        byte attr = (aware? COLOUR_WHITE: COLOUR_SLATE);

        /* Find graphics bits -- versions of the object_char and object_attr defines */
        byte a = object_kind_attr(p, kind);
        char c = object_kind_char(p, kind);

        /* Paranoia */
        if (gid == -1) continue;

        /* Print group */
        if (gid != o_group)
        {
            o_group = gid;
            file_putf(fff, "w%s\n", object_text_order[o_group].name);
        }

        object_kind_name(o_name, sizeof(o_name), kind, aware);

        /* If the type is "tried", display that */
        if (p->obj_tried[kind->kidx] && !aware) my_strcat(o_name, " {tried}", sizeof(o_name));

        /* Append flavour if desired */
        else if (OPT_P(p, show_flavors) && kind->flavor && aware)
        {
            my_strcat(o_name, " (", sizeof(o_name));
            my_strcat(o_name, kind->flavor->text, sizeof(o_name));
            my_strcat(o_name, ")", sizeof(o_name));
        }

        /* Print a message */
        if (p->tile_distorted)
            file_putf(fff, "%c     %s\n", color_attr_to_char(attr), o_name);
        else
        {
            file_putf(fff, "d%c%c%c%s\n", ((a & 0x80)? a: color_attr_to_char(a)), c,
                color_attr_to_char(attr), o_name);
        }
    }

    mem_free(objects);
    mem_free(obj_group_order);
    obj_group_order = NULL;

    /* Close the file */
    file_close(fff);

    /* Display the file contents */
    show_file(p, file_name, "Known Objects", line, 1);

    /* Remove the file */
    file_delete(file_name);
}


/*
 * TERRAIN FEATURES
 */


/*
 * Description of each feature group
 */
static const char *feature_group_text[] =
{
    "Floors",
    "Doors",
    "Stairs",
    "Walls",
    "Streamers",
    "Obstructions",
    "Stores",
    "Other",
    NULL
};


static int f_cmp_fkind(const void *a, const void *b)
{
    const struct feature *fa = &f_info[*(const int *)a];
    const struct feature *fb = &f_info[*(const int *)b];

    /* Group by */
    int c = feat_order(*(const int *)a) - feat_order(*(const int *)b);

    if (c) return c;

    /* Order by feature name */
    return strcmp(fa->name, fb->name);
}


/*
 * Interact with feature visuals
 */
static void do_cmd_knowledge_features(struct player *p, int line)
{
    int *features;
    int f_count = 0;
    int i;
    char file_name[MSG_LEN];
    ang_file *fff;
    int f_group = -1;

    /* Temporary file */
    fff = file_temp(file_name, sizeof(file_name));
    if (!fff) return;

    features = mem_zalloc(z_info->f_max * sizeof(int));

    /* Collect features */
    for (i = 0; i < z_info->f_max; i++)
    {
        /* Ignore non-features and mimics */
        if (!f_info[i].name || (f_info[i].mimic != i)) continue;

        features[f_count++] = i;
    }

    /* Sort */
    sort(features, f_count, sizeof(*features), f_cmp_fkind);

    /* Print the features */
    for (i = 0; i < f_count; i++)
    {
        const struct feature *feat = &f_info[features[i]];
        int gid = feat_order(features[i]);

        /* Find graphics bits */
        byte a = p->f_attr[features[i]][LIGHTING_LIT];
        char c = p->f_char[features[i]][LIGHTING_LIT];

        /* Paranoia */
        if (gid == -1) continue;

        /* Print group */
        if (gid != f_group)
        {
            f_group = gid;
            file_putf(fff, "w%s\n", feature_group_text[f_group]);
        }

        /* Print a message */
        if (p->tile_distorted)
            file_putf(fff, "w     %s\n", feat->name);
        else
            file_putf(fff, "d%c%cw%s\n", ((a & 0x80)? a: color_attr_to_char(a)), c, feat->name);
    }

    mem_free(features);

    /* Close the file */
    file_close(fff);

    /* Display the file contents */
    show_file(p, file_name, "Features", line, 1);

    /* Remove the file */
    file_delete(file_name);
}


/*
 * TRAPS
 */


/*
 * Description of each trap group
 */
static const char *trap_group_text[] =
{
    "Runes",
    "Locks",
    "Traps",
    "Other",
    NULL
};


static int t_cmp_tkind(const void *a, const void *b)
{
    const int a_val = *(const int *)a;
    const int b_val = *(const int *)b;
    const struct trap_kind *ta = &trap_info[a_val];
    const struct trap_kind *tb = &trap_info[b_val];

    /* Group by */
    int c = trap_order(a_val) - trap_order(b_val);

    if (c) return c;

    /* Order by name */
    if (ta->name)
    {
        if (tb->name) return strcmp(ta->name, tb->name);
        return 1;
    }
    if (tb->name) return -1;

    return 0;
}


/*
 * Interact with trap visuals
 */
static void do_cmd_knowledge_traps(struct player *p, int line)
{
    int *traps;
    int t_count = 0;
    int i;
    char file_name[MSG_LEN];
    ang_file *fff;
    int t_group = -1;

    /* Temporary file */
    fff = file_temp(file_name, sizeof(file_name));
    if (!fff) return;

    traps = mem_zalloc(z_info->trap_max * sizeof(int));

    /* Collect traps */
    for (i = 0; i < z_info->trap_max; i++)
    {
        /* Ignore non-traps */
        if (!trap_info[i].name) continue;

        traps[t_count++] = i;
    }

    /* Sort */
    sort(traps, t_count, sizeof(*traps), t_cmp_tkind);

    /* Print the traps */
    for (i = 0; i < t_count; i++)
    {
        const struct trap_kind *trap = &trap_info[traps[i]];
        int gid = trap_order(traps[i]);

        /* Find graphics bits */
        byte a = p->t_attr[traps[i]][LIGHTING_LIT];
        char c = p->t_char[traps[i]][LIGHTING_LIT];

        /* Paranoia */
        if (gid == -1) continue;

        /* Print group */
        if (gid != t_group)
        {
            t_group = gid;
            file_putf(fff, "w%s\n", trap_group_text[t_group]);
        }

        /* Print a message */
        if (p->tile_distorted)
            file_putf(fff, "w     %s\n", trap->desc);
        else
            file_putf(fff, "d%c%cw%s\n", ((a & 0x80)? a: color_attr_to_char(a)), c, trap->desc);
    }

    mem_free(traps);

    /* Close the file */
    file_close(fff);

    /* Display the file contents */
    show_file(p, file_name, "Traps", line, 1);

    /* Remove the file */
    file_delete(file_name);
}


/*
 * Main knowledge menus
 */


static void do_cmd_knowledge_history(struct player *p, int line)
{
    ang_file *fff;
    char file_name[MSG_LEN];

    /* Temporary file */
    fff = file_temp(file_name, sizeof(file_name));
    if (!fff) return;

    /* Dump character history */
    dump_history(p, fff);

    /* Close the file */
    file_close(fff);

    /* Display the file contents */
    show_file(p, file_name, "Character History", line, 0);

    /* Remove the file */
    file_delete(file_name);
}


/*
 * Display known uniques
 */
static void do_cmd_knowledge_uniques(struct player *p, int line)
{
    int k, l, i, space, namelen, total = 0, width = NORMAL_WID - 2;
    ang_file *fff;
    char file_name[MSG_LEN], buf[MSG_LEN];
    int* idx;
    struct monster_race *race, *curr_race;
    struct monster_lore *lore;

    /* Temporary file */
    fff = file_temp(file_name, sizeof(file_name));
    if (!fff) return;

    idx = mem_zalloc(z_info->r_max * sizeof(int));

    /* Scan the monster races */
    for (k = 1; k < z_info->r_max; k++)
    {
        race = &r_info[k];

        /* Only print Uniques */
        if (rf_has(race->flags, RF_UNIQUE))
        {
            /* Only display "known" uniques */
            if (race->lore.seen)
            {
                l = 0;
                while (l < total)
                {
                    curr_race = &r_info[idx[l]];
                    if (race->level > curr_race->level) break;
                    l++;
                }
                for (i = total; i > l; i--) idx[i] = idx[i - 1];
                idx[l] = k;
                total++;
            }
        }
    }

    if (total)
    {
        /* For each unique */
        for (l = total - 1; l >= 0; l--)
        {
            byte ok = false;
            char highlight = 'D';

            race = &r_info[idx[l]];
            strnfmt(buf, sizeof(buf), "%s has been killed by: ", race->name);
            space = width - strlen(buf);

            /* Do we need to highlight this unique? */
            lore = get_lore(p, race);
            if (lore->pkills) highlight = 'w';

            /* Append all players who killed this unique */
            k = 0;
            for (i = 1; i <= NumPlayers; i++)
            {
                struct player *q = player_get(i);

                /* Skip the DM... except for the DM */
                if ((q->dm_flags & DM_SECRET_PRESENCE) && !is_dm_p(p)) continue;

                lore = get_lore(q, race);
                if (lore->pkills)
                {
                    ok = true;
                    namelen = strlen(q->name) + 2;
                    if (space - namelen < 0)
                    {
                        /* Out of space, flush the line */
                        file_putf(fff, "%c%s\n", highlight, buf);
                        my_strcpy(buf, "  ", sizeof(buf));
                        k = 0;
                        space = width;
                    }
                    if (k++) my_strcat(buf, ", ", sizeof(buf));
                    my_strcat(buf, q->name, sizeof(buf));
                    space -= namelen;
                }
            }

            if (ok)
                file_putf(fff, "%c%s\n", highlight, buf);
            else if (race->lore.tkills)
                file_putf(fff, "D%s has been killed by somebody.\n", race->name);
            else
                file_putf(fff, "D%s has never been killed!\n", race->name);
        }
    }
    else file_put(fff, "wNo uniques are witnessed so far.\n");

    /* Close the file */
    file_close(fff);

    /* Display the file contents */
    show_file(p, file_name, "Known Uniques", line, 1);

    /* Remove the file */
    file_delete(file_name);

    mem_free(idx);
}


/*
 * Display party gear
 */
static void do_cmd_knowledge_gear(struct player *p, int line)
{
    int k, i;
    ang_file *fff;
    char file_name[MSG_LEN];
    struct object *obj;
    char o_name[NORMAL_WID];
    byte color;

    /* Temporary file */
    fff = file_temp(file_name, sizeof(file_name));
    if (!fff) return;

    /* Scan the players */
    for (k = 1; k <= NumPlayers; k++)
    {
        struct player *q = player_get(k);

        /* Only print connected players XXX */
        if (q->conn == -1) continue;

        /* Don't display self */
        if (p == q) continue;

        /* DM sees everybody - others see party members */
        if ((!p->party || (p->party != q->party)) && !(p->dm_flags & DM_SEE_PLAYERS))
            continue;

        /* Print a message */
        if (q->total_winner)
        {
            file_putf(fff, "G     %s the %s %s (%s, Level %d) at %d ft\n", q->name,
                q->race->name, q->clazz->name, q->sex->winner, q->lev,
                q->depth * 50);
        }
        else
        {
            file_putf(fff, "G     %s the %s %s (Level %d) at %d ft\n", q->name,
                q->race->name, q->clazz->name, q->lev, q->depth * 50);
        }

        /* Display the equipment */
        for (i = 0; i < q->body.count; i++)
        {
            obj = slot_object(q, i);

            /* We need an item */
            if (!obj) continue;

            /* Obtain an item description */
            object_desc(p, o_name, sizeof(o_name), obj, ODESC_PREFIX | ODESC_FULL);

            /* Get the color */
            color = obj->kind->base->attr;

            /* Display item description */
            file_putf(fff, "%c         %s\n", color_attr_to_char(color), o_name);
        }

        /* Next */
        file_put(fff, "w\n");
    }

    /* Close the file */
    file_close(fff);

    /* Display the file contents */
    if (p->dm_flags & DM_SEE_PLAYERS)
        show_file(p, file_name, "Player List", line, 1);
    else
        show_file(p, file_name, "Party Members", line, 1);

    /* Remove the file */
    file_delete(file_name);
}


/*
 * Display owned houses
 */
static void do_cmd_knowledge_houses(struct player *p, int line)
{
    char file_name[MSG_LEN];
    ang_file *fff;

    /* Temporary file */
    fff = file_temp(file_name, sizeof(file_name));
    if (!fff) return;

    /* List owned houses */
    house_list(p, fff);

    /* Close the file */
    file_close(fff);

    /* Display the file contents */
    show_file(p, file_name, "Owned Houses", line, 0);

    /* Remove the file */
    file_delete(file_name);
}


/*
 * Display the "player knowledge" menu.
 */
void do_cmd_knowledge(struct player *p, char type, int line)
{
    switch (type)
    {
        /* Display object knowledge */
        case SPECIAL_FILE_OBJECT:
            Send_term_info(p, NTERM_ACTIVATE, NTERM_WIN_SPECIAL);
            do_cmd_knowledge_objects(p, line);
            Send_term_info(p, NTERM_ACTIVATE, NTERM_WIN_OVERHEAD);
            break;

        /* Display artifact knowledge */
        case SPECIAL_FILE_ARTIFACT:
            Send_term_info(p, NTERM_ACTIVATE, NTERM_WIN_SPECIAL);
            do_cmd_knowledge_artifacts(p, line);
            Send_term_info(p, NTERM_ACTIVATE, NTERM_WIN_OVERHEAD);
            break;

        /* Display ego item knowledge */
        case SPECIAL_FILE_EGO:
            Send_term_info(p, NTERM_ACTIVATE, NTERM_WIN_SPECIAL);
            do_cmd_knowledge_ego_items(p, line);
            Send_term_info(p, NTERM_ACTIVATE, NTERM_WIN_OVERHEAD);
            break;

        /* Display monster knowledge */
        case SPECIAL_FILE_KILL:
            Send_term_info(p, NTERM_ACTIVATE, NTERM_WIN_SPECIAL);
            do_cmd_knowledge_monsters(p, line);
            Send_term_info(p, NTERM_ACTIVATE, NTERM_WIN_OVERHEAD);
            break;

        /* Display feature knowledge */
        case SPECIAL_FILE_FEATURE:
            Send_term_info(p, NTERM_ACTIVATE, NTERM_WIN_SPECIAL);
            do_cmd_knowledge_features(p, line);
            Send_term_info(p, NTERM_ACTIVATE, NTERM_WIN_OVERHEAD);
            break;

        /* Display trap knowledge */
        case SPECIAL_FILE_TRAP:
            Send_term_info(p, NTERM_ACTIVATE, NTERM_WIN_SPECIAL);
            do_cmd_knowledge_traps(p, line);
            Send_term_info(p, NTERM_ACTIVATE, NTERM_WIN_OVERHEAD);
            break;

        /* Display hall of fame */
        case SPECIAL_FILE_SCORES:
            Send_term_info(p, NTERM_ACTIVATE, NTERM_WIN_SPECIAL);
            do_cmd_knowledge_scores(p, line);
            Send_term_info(p, NTERM_ACTIVATE, NTERM_WIN_OVERHEAD);
            break;

        /* Display character history */
        case SPECIAL_FILE_HISTORY:
            Send_term_info(p, NTERM_ACTIVATE, NTERM_WIN_SPECIAL);
            do_cmd_knowledge_history(p, line);
            Send_term_info(p, NTERM_ACTIVATE, NTERM_WIN_OVERHEAD);
            break;

        /* Display known uniques */
        case SPECIAL_FILE_UNIQUE:
            Send_term_info(p, NTERM_ACTIVATE, NTERM_WIN_SPECIAL);
            do_cmd_knowledge_uniques(p, line);
            Send_term_info(p, NTERM_ACTIVATE, NTERM_WIN_OVERHEAD);
            break;

        /* Display party gear */
        case SPECIAL_FILE_GEAR:
            Send_term_info(p, NTERM_ACTIVATE, NTERM_WIN_SPECIAL);
            do_cmd_knowledge_gear(p, line);
            Send_term_info(p, NTERM_ACTIVATE, NTERM_WIN_OVERHEAD);
            break;

        /* Display owned houses */
        case SPECIAL_FILE_HOUSES:
            Send_term_info(p, NTERM_ACTIVATE, NTERM_WIN_SPECIAL);
            do_cmd_knowledge_houses(p, line);
            Send_term_info(p, NTERM_ACTIVATE, NTERM_WIN_OVERHEAD);
            break;
    }
}


/*
 * Other knowledge functions
 */


/*
 * Hack -- redraw the screen
 *
 * This command performs various low level updates, clears all the "extra"
 * windows, does a total redraw of the main window, and requests all of the
 * interesting updates and redraws that I can think of.
 */
void do_cmd_redraw(struct player *p)
{
    /* Not while shopping */
    if (in_store(p)) return;

    verify_panel(p);

    /* Combine the pack (later) */
    p->upkeep->notice |= (PN_COMBINE);

    /* Update stuff */
    p->upkeep->update |= (PU_BONUS | PU_SPELLS | PU_INVEN);

    /* Fully update the visuals */
    p->upkeep->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);

    /* Redraw */
    p->upkeep->redraw |= (PR_BASIC | PR_EXTRA | PR_MAP | PR_FLOOR | PR_INVEN |
        PR_SPELL | PR_MONSTER | PR_OBJECT | PR_MONLIST | PR_ITEMLIST);
}


/*
 * Drop some gold
 */
void do_cmd_drop_gold(struct player *p, s32b amt)
{
    struct object *obj;

    /* Restrict ghosts */
    if (p->ghost && !(p->dm_flags & DM_GHOST_BODY))
    {
        msg(p, "You need a tangible body to drop gold!");
        return;
    }

    /* Handle the newbies_cannot_drop option */
    if (newbies_cannot_drop(p))
    {
        msg(p, "You are not experienced enough to drop gold.");
        return;
    }

    /* Error checks */
    if (amt <= 0) return;
    if (amt > p->au)
    {
        msg(p, "You do not have that much gold.");
        return;
    }

    /* Take a turn */
    use_energy(p);

    /* Substract from the player's gold */
    p->au -= amt;

    /* Message */
    msg(p, "You drop %ld gold pieces.", amt);

    /* Redraw gold */
    p->upkeep->redraw |= (PR_GOLD);

    /* Setup the object */
    obj = object_new();
    object_prep(p, obj, money_kind("gold", amt), 0, MINIMISE);

    /* Setup the "worth" */
    obj->pval = amt;

    /* Pile of gold is now owned */
    obj->owner = p->id;

    /* Drop it */
    drop_near(p, chunk_get(p->depth), obj, 0, p->py, p->px, false, DROP_FADE);
}


/*
 * Attempt to steal from another player
 */
void do_cmd_steal(struct player *p, int dir)
{
    struct chunk *c = chunk_get(p->depth);
    int y, x;
    struct actor who_body;
    struct actor *who = &who_body;
    struct player *target;
    int chance, notice;
    bool success = false;

    /* Paranoia */
    if ((dir == 5) || !dir) return;

    /* Ensure "dir" is in ddx/ddy array bounds */
    if (!VALID_DIR(dir)) return;

    y = p->py + ddy[dir];
    x = p->px + ddx[dir];
    square_actor(c, y, x, who);

    /* Restrict ghosts */
    if (p->ghost && !(p->dm_flags & DM_GHOST_BODY))
    {
        msg(p, "You need a tangible body to steal items!");
        return;
    }

    /* Not when confused */
    if (p->timed[TMD_CONFUSED])
    {
        msg(p, "You are too confused!");
        return;
    }

    /* Restricted by choice */
    if (OPT_P(p, birth_no_stores))
    {
        msg(p, "You cannot steal from players.");
        return;
    }

    /* Check preventive inscription '^J' */
    if (check_prevent_inscription(p, INSCRIPTION_STEAL))
    {
        msg(p, "The item's inscription prevents it.");
        return;
    }

    /* May only steal from visible players */
    if (!who->player || !mflag_has(p->pflag[who->idx], MFLAG_VISIBLE))
    {
        /* Message */
        msg(p, "You see nothing there to steal from.");
        return;
    }

    /* Examine target */
    target = who->player;

    /* May not steal if hostile */
    if (pvp_check(p, target, PVP_CHECK_ONE, true, 0x00))
    {
        /* Message */
        msg(p, "%s is on guard against you.", target->name);
        return;
    }

    /* May not steal if the target cannot retaliate */
    if ((cfg_pvp_hostility >= PVP_SAFE) || in_party(target, p->party) ||
        square_issafefloor(c, y, x))
    {
        /* Message */
        msg(p, "You cannot steal from that player.");
        return;
    }

    /* Compute chance of success */
    chance = 3 * (adj_dex_safe[p->state.stat_ind[STAT_DEX]] -
        adj_dex_safe[target->state.stat_ind[STAT_DEX]]);

    /* Compute base chance of being noticed */
    notice = 5 * (adj_mag_stat[target->state.stat_ind[STAT_INT]] -
        p->state.skills[SKILL_STEALTH]);

    /* Hack -- rogues get bonuses to chances */
    if (player_has(p, PF_STEALING_IMPROV))
    {
        /* Increase chance by level */
        chance += 3 * p->lev;
        notice -= 3 * p->lev;
    }

    /* Hack -- always small chance to succeed */
    if (chance < 2) chance = 2;

    /* Check for success */
    if (magik(chance))
    {
        /* Steal gold 25% of the time */
        if (magik(25))
        {
            int amt = target->au / 10;

            /* Transfer gold */
            if (amt)
            {
                /* Move from target to thief */
                target->au -= amt;
                p->au += amt;

                /* Redraw */
                p->upkeep->redraw |= (PR_GOLD);
                target->upkeep->redraw |= (PR_GOLD);

                /* Tell thief */
                msg(p, "You steal %d gold from %s.", amt, target->name);

                /* Check for target noticing */
                if (magik(notice))
                {
                    /* Make target hostile */
                    pvp_check(target, p, PVP_ADD, true, 0x00);

                    /* Message */
                    msg(target, "You notice %s stealing %d gold!", p->name, amt);
                }

                /* Done */
                success = true;
            }
        }

        /* Steal an item */
        else
        {
            int tries;

            /* Find an item */
            for (tries = 0; tries < 100; tries++)
            {
                struct object *obj, *stolen, *test;
                char o_name[NORMAL_WID], t_name[NORMAL_WID];
                bool ok = true;
                bool none_left = false;

                /* Pick an item */
                int index = randint0(z_info->pack_size);

                /* Obtain the item */
                obj = target->upkeep->inven[index];

                /* Skip non-objects */
                if (obj == NULL) continue;

                /* Skip artifacts */
                if (obj->artifact) continue;

                /* Skip deeds of property */
                if (tval_is_deed(obj)) continue;

                /* Get a copy with the right "amt" */
                test = object_new();
                object_copy_amt(test, obj, 1);

                /* Note that the pack is too full */
                if (!inven_carry_okay(p, test)) ok = false;

                /* Note that the pack is too heavy */
                else if (!weight_okay(p, test)) ok = false;

                /* Can only steal items if they can be carried */
                object_delete(&test);
                if (!ok) continue;

                /* Steal and carry, easier to notice heavier objects */
                stolen = gear_object_for_use(target, obj, 1, false, &none_left);
                object_desc(p, o_name, sizeof(o_name), stolen, ODESC_PREFIX | ODESC_FULL);
                object_desc(target, t_name, sizeof(t_name), stolen, ODESC_PREFIX | ODESC_FULL);
                notice += stolen->weight;
                inven_carry(p, stolen, true, false);

                /* Tell thief what he got */
                msg(p, "You steal %s from %s.", o_name, target->name);

                /* Check for target noticing */
                if (magik(notice))
                {
                    /* Make target hostile */
                    pvp_check(target, p, PVP_ADD, true, 0x00);

                    /* Message */
                    msg(target, "You notice %s stealing %s!", p->name, t_name);
                }

                /* Done */
                success = true;
                break;
            }
        }
    }
    
    /* Failure */
    if (!success)
    {
        /* Message */
        msg(p, "You fail to steal anything from %s.", target->name);

        /* Easier to notice a failed attempt */
        if (magik(notice + 50))
        {
            /* Make target hostile */
            pvp_check(target, p, PVP_ADD, true, 0x00);

            /* Message */
            msg(target, "You notice %s trying to steal from you!", p->name);
        }
    }

    /* Take a turn */
    use_energy(p);
}


/*
 * Display known info about a monster or group of monsters specified by name or symbol
 */
void do_cmd_query_symbol(struct player *p, const char *buf)
{
    int i;
    bool found = false;
    char* str;

    /* Let the player scroll through this info */
    p->special_file_type = SPECIAL_FILE_OTHER;

    /* Prepare player structure for text */
    text_out_init(p);

    /* Lowercase our search string */
    if (strlen(buf) > 1)
    {
        for (str = (char*)buf; *str; str++) *str = tolower((unsigned char)*str);
    }

    /* Collect matching monsters */
    for (i = 1; i < z_info->r_max; i++)
    {
        struct monster_race *race = &r_info[i];
        bool ok;

        /* Skip non-entries */
        if (!race->name) continue;

        /* Check if the input is a symbol */
        if (strlen(buf) == 1)
            ok = (race->d_char == buf[0]);

        /* The input is a partial monster name */
        else
        {
            char monster[NORMAL_WID];

            /* Clean up monster name */
            clean_name(monster, race->name);

            /* Check if cleaned name matches our search string */
            ok = (strstr(monster, buf) != NULL);
        }

        /* Dump info */
        if (ok)
        {
            /* Describe it */
            if (found) text_out(p, "\n\n\n");
            lore_description(p, race);

            found = true;
        }
    }
    if (!found) text_out(p, "You fail to remember any monsters of this kind.");

    /* Restore height and width of current dungeon level */
    text_out_done(p);

    /* Notify player */
    notify_player(p, format("Monster Recall ('%s')", buf), NTERM_WIN_MONSTER, false);
}


/*
 * Display known info about a monster/player
 */
void do_cmd_describe(struct player *p)
{
    struct actor *cursor_who = &p->cursor_who;

    /* We need something */
    if (ACTOR_NULL(cursor_who)) return;

    /* Describe it */
    if (cursor_who->player) describe_player(p, cursor_who->player);
    else describe_monster(p, cursor_who->mon->race);

    /* Notify player */
    notify_player(p, "Monster Recall", NTERM_WIN_MONSTER, false);
}


/*
 * Allow the player to examine other sectors on the map
 */
void do_cmd_locate(struct player *p, int dir)
{
    int y1, x1, y2, x2;
    char tmp_val[NORMAL_WID];
    char out_val[160];
    int panel_wid, panel_hgt;

    panel_wid = PANEL_SIZE / p->tile_wid;
    panel_hgt = PANEL_SIZE / p->tile_hgt;

    /* No direction, recenter */
    if (!dir)
    {
        /* Forget current panel */
        p->offset_y_old = p->offset_x_old = -1;

        /* Recenter map around the player */
        verify_panel(p);

        return;
    }

    /* Ensure "dir" is in ddx/ddy array bounds */
    if (!VALID_DIR(dir)) return;

    /* Initialize */
    if (dir == 5)
    {
        /* Remember current panel */
        p->offset_y_old = p->offset_y;
        p->offset_x_old = p->offset_x;
    }

    /* Start at current panel */
    y1 = p->offset_y_old;
    x1 = p->offset_x_old;

    /* Apply the motion */
    change_panel(p, dir);

    /* Handle stuff */
    handle_stuff(p);

    /* Get the current panel */
    y2 = p->offset_y;
    x2 = p->offset_x;

    /* Describe the location */
    if ((y2 == y1) && (x2 == x1))
        tmp_val[0] = '\0';
    else
    {
        strnfmt(tmp_val, sizeof(tmp_val), "%s%s of", ((y2 < y1)? " north": (y2 > y1)? " south": ""),
            ((x2 < x1)? " west": (x2 > x1)? " east": ""));
    }

    /* Prepare to ask which way to look */
    strnfmt(out_val, sizeof(out_val), "Map sector [%d,%d], which is%s your sector.  Direction?",
        (y2 / panel_hgt), (x2 / panel_wid), tmp_val);

    /* More detail */
    if (OPT_P(p, center_player))
    {
        strnfmt(out_val, sizeof(out_val),
            "Map sector [%d(%02d),%d(%02d)], which is%s your sector.  Direction?",
            (y2 / panel_hgt), (y2 % panel_hgt), (x2 / panel_wid), (x2 % panel_wid), tmp_val);
    }

    msg(p, out_val);
}


static void drink_fountain(struct player *p, struct object *obj)
{
    struct effect *effect;
    bool ident = false, used = false;

    /* The player is aware of the object's flavour */
    p->was_aware = object_flavor_is_aware(p, obj);

    /* Figure out effect to use */
    effect = object_effect(obj);

    /* Do effect */
    if (effect)
    {
        /* Do effect */
        if (effect->other_msg) msg_misc(p, effect->other_msg);
        used = effect_do(p, effect, &ident, p->was_aware, 0, NULL, 0, 0, NULL, NULL);

        /* Quit if the item wasn't used and no knowledge was gained */
        if (!used && (p->was_aware || !ident)) return;
    }

    if (ident) object_notice_effect(p, obj);
}


/*
 * Drink/fill an empty bottle from a fountain
 */
void do_cmd_fountain(struct player *p, int item)
{
    struct object *obj;
    struct object_kind *kind;
    struct chunk *c = chunk_get(p->depth);

    /* Restrict ghosts */
    if (p->ghost && !(p->dm_flags & DM_GHOST_BODY))
    {
        msg(p, "You need a tangible body to interact with fountains!");
        return;
    }

    /* Check preventive inscription '^_' */
    if (check_prevent_inscription(p, INSCRIPTION_FOUNTAIN))
    {
        msg(p, "The item's inscription prevents it.");
        return;
    }

    /* We must stand on a fountain */
    if (!square_isfountain(c, p->py, p->px))
    {
        msg(p, "There is no fountain here.");
        return;
    }

    /* Dried out */
    if (square_isdryfountain(c, p->py, p->px))
    {
        msg(p, "The fountain is dried out.");
        return;
    }

    /* Take a turn */
    use_energy(p);

    /* Fruit bat! */
    if (item == -1)
    {
        struct monster_race *race_fruit_bat = get_race("Fruit bat");
        bool poly = false;

        /* Try (very rarely) to turn the player into a fruit bat */
        if ((p->poly_race != race_fruit_bat) && one_in_(200)) poly = true;

        /* Try (very hard) to restore characters back to normal */
        if ((p->poly_race == race_fruit_bat) && one_in_(3)) poly = true;

        if (poly)
        {
            msg(p, "You drink from the fountain.");
            poly_bat(p, 100, NULL);

            /* Done */
            return;
        }
    }

    /* Summon water creature */
    if (one_in_(20))
    {
        static const struct summon_chance_t
        {
            const char *race;
            byte minlev;
            byte chance;
        } summon_chance[] =
        {
            {"Giant green frog", 0, 100},
            {"Giant red frog", 7, 90},
            {"Water spirit", 17, 80},
            {"Water vortex", 21, 70},
            {"Water elemental", 33, 60},
            {"Water hound", 43, 50}
        };
        int i;

        msg(p, "Something pops out of the water!");
        do {i = randint0(N_ELEMENTS(summon_chance));}
        while ((p->depth < summon_chance[i].minlev) || !magik(summon_chance[i].chance));
        summon_specific_race(p, c, p->py, p->px, get_race(summon_chance[i].race), 1);

        /* Done */
        return;
    }

    /* Fall in */
    if (one_in_(20))
    {
        msg(p, "You slip and fall in the water.");
        if (!player_passwall(p) && !can_swim(p))
        {
            my_strcpy(p->died_flavor, "slipped and fell in a fountain",
                sizeof(p->died_flavor));
            take_hit(p, damroll(4, 5), "drowning", false);
        }

        /* Done */
        return;
    }

    /* Message */
    if (item == -1) msg(p, "You drink from the fountain.");

    /* Ale */
    if (one_in_(10))
    {
        msg(p, "Wow! Pure dwarven ale!");
        kind = lookup_kind_by_name(TV_FOOD, "Pint of Fine Ale");
    }

    /* Plain water */
    else if (one_in_(2))
    {
        msg(p, "The water is clear and fresh.");
        kind = lookup_kind_by_name(TV_POTION, "Water");
    }

    /* Random potion effect */
    else
    {
        int sval;

        /* Generate a potion */
        while (true)
        {
            /* Pick a random potion */
            sval = randint1(kb_info[TV_POTION].num_svals);

            /* Access the kind index */
            kind = lookup_kind_silent(TV_POTION, sval);
            if (!kind) continue;

            /* No out of depth effect */
            if (p->depth < kind->alloc_min) continue;

            /* Less chance to get the effect deeper */
            if ((p->depth > kind->alloc_max) && magik(50)) continue;

            /* Apply rarity */
            if (!magik(kind->alloc_prob)) continue;

            /* Success */
            break;
        }

        /* Message */
        if (!kind->cost)
            msg(p, "The water is dark and muddy.");
        else
            msg(p, "The water sparkles gently.");
    }

    /* Prepare the object */
    obj = object_new();
    object_prep(p, obj, kind, p->depth, RANDOMISE);

    /* Set origin */
    set_origin(obj, ORIGIN_FOUNTAIN, p->depth, 0);

    /* Get an empty bottle */
    if (item >= 0)
    {
        struct object *bottle = object_from_index(p, item, true, true), *used;
        bool none_left = false;

        /* Paranoia: requires a bottle */
        if (!bottle || !tval_is_bottle(bottle)) return;

        /* Check preventive inscription '!_' */
        if (object_prevent_inscription(p, bottle, INSCRIPTION_FOUNTAIN, false))
        {
            msg(p, "The item's inscription prevents it.");
            return;
        }

        /* Eliminate the item */
        used = gear_object_for_use(p, bottle, 1, false, &none_left);
        object_delete(&used);

        /* Create the object */
        apply_magic(p, c, obj, p->depth, false, false, false, false);

        /* Drop it in the dungeon */
        drop_near(p, c, obj, 0, p->py, p->px, true, DROP_FADE);
    }

    /* Drink from a fountain */
    else
    {
        drink_fountain(p, obj);
        object_delete(&obj);
    }

    /* Fountain dries out */
    if (one_in_(3))
    {
        msg(p, "The fountain suddenly dries up.");
        square_dry_fountain(c, p->py, p->px);
    }
}


/*
 * Centers the map on the player
 */
void do_cmd_center_map(struct player *p)
{
    /* Not while shopping */
    if (in_store(p)) return;

    center_panel(p);
}


/*
 * Display the main-screen monster list.
 */
void do_cmd_monlist(struct player *p)
{
    /* Display visible monsters */
    monster_list_show_interactive(p, p->max_hgt, NORMAL_WID - 5);

    /* Notify player */
    notify_player(p, "Monster List", NTERM_WIN_MONLIST, true);
}


/*
 * Display the main-screen item list.
 */
void do_cmd_itemlist(struct player *p)
{
    /* Display visible objects */
    object_list_show_interactive(p, p->max_hgt, NORMAL_WID - 5);

    /* Notify player */
    notify_player(p, "Object List", NTERM_WIN_OBJLIST, true);
}


/*
 * Check the status of "players"
 *
 * The player's name, race, class, and experience level are shown.
 */
void do_cmd_check_players(struct player *p, int line)
{
    int k;
    ang_file *fff;
    char file_name[MSG_LEN];

    /* Temporary file */
    fff = file_temp(file_name, sizeof(file_name));
    if (!fff) return;

    /* Scan the player races */
    for (k = 1; k <= NumPlayers; k++)
    {
        struct player *q = player_get(k);
        byte attr = 'w';
        char brave[30];
        char winner[20];
        const char *batty = "";

        /* Only print connected players XXX */
        if (q->conn == -1) continue;

        /*
         * Don't display the dungeon master if the secret_dungeon_master
         * option is set (unless you're a DM yourself)
         */
        if ((q->dm_flags & DM_SECRET_PRESENCE) && !(p->dm_flags & DM_SEE_PLAYERS)) continue;

        /*** Determine color ***/

        /* Print self in green */
        if (p == q) attr = 'G';

        /* Print party members in blue */
        else if (p->party && p->party == q->party) attr = 'B';

        /* Print hostile players in red */
        else if (pvp_check(p, q, PVP_CHECK_BOTH, true, 0x00)) attr = 'r';

        /* Print potential hostile players in yellow */
        else if (pvp_check(p, q, PVP_CHECK_ONE, true, 0x00)) attr = 'y';

        /* Output color byte */
        file_putf(fff, "%c", attr);

        /* Challenge options */
        strnfmt(brave, sizeof(brave), "the%s%s%s",
            OPT_P(q, birth_no_ghost)? " brave": "",
            OPT_P(q, birth_no_recall)? " hardcore": "",
            OPT_P(q, birth_force_descend)? " diving": "");

        winner[0] = '\0';
        if (q->total_winner) strnfmt(winner, sizeof(winner), "%s, ", q->sex->winner);
        if (OPT_P(q, birth_fruit_bat)) batty = "batty, ";

        /* Print a message */
        file_putf(fff, "     %s %s %s %s (%s%sLevel %d, %s)", q->name, brave, q->race->name,
            q->clazz->name, winner, batty, q->lev, parties[q->party].name);

        /* Print extra info if these people are not 'red' aka hostile */
        /* Hack -- always show extra info to dungeon master */
        if ((attr != 'r') || (p->dm_flags & DM_SEE_PLAYERS))
            file_putf(fff, " at %d ft", q->depth * 50);

        /* Newline */
        file_put(fff, "\n");
        file_putf(fff, "U         %s@%s\n", q->other.full_name, q->hostname);
    }

    /* Close the file */
    file_close(fff);

    /* Display the file contents */
    show_file(p, file_name, "Player List", line, 1);

    /* Remove the file */
    file_delete(file_name);
}


/*
 * Scroll through information.
 */
void do_cmd_check_other(struct player *p, int line)
{
    char buf[NORMAL_WID + 1];
    int i, j;
    s16b last_info_line = get_last_info_line(p);

    /* Make sure the player is allowed to */
    if (!p->special_file_type) return;

    /* Dump the next 20 lines of the file */
    for (i = 0; i < 20; i++)
    {
        byte attr = COLOUR_WHITE;

        /* We're done */
        if (line + i > MAX_TXT_INFO) break;
        if (line + i > last_info_line) break;

        /* Extract string */
        for (j = 0; j < NORMAL_WID; j++)
            buf[j] = get_info(p, line + i, j)->c;
        attr = get_info(p, line + i, 0)->a;
        buf[j] = '\0';

        /* Dump the line */
        Send_special_line(p, last_info_line, last_info_line - line, i, attr, &buf[0]);
    }
}


/*
 * Returns the "affinity" of the player to the monster race.
 */
static int affinity(struct player *p, struct monster_race *race)
{
    size_t i, kills = 0, count = 0;

    /* Calculate monster race affinity */
    for (i = 1; i < (size_t)z_info->r_max; i++)
    {
        struct monster_race *r = &r_info[i];
        struct monster_lore *il_ptr = get_lore(p, r);

        /* Skip non-entries */
        if (!r->name) continue;

        /* Skip uniques */
        if (rf_has(r->flags, RF_UNIQUE)) continue;

        /* Skip different symbol */
        if (r->d_char != race->d_char) continue;

        /* Skip current */
        if (r == race) continue;

        /* Skip stronger */
        if (r->level > race->level) continue;

        /* Affinity with that monster race */
        kills += MIN(il_ptr->pkills, MAX(1, r->level));
        count += MAX(1, r->level);
    }

    return (count? ((100 * kills) / count): 0);
}


void do_cmd_poly(struct player *p, struct monster_race *race, bool check_kills, bool domsg)
{
    const char *prefix = "";

    /* Restrict ghosts */
    if (p->ghost)
    {
        if (domsg)
            msg(p, "You need a tangible body to polymorph!");
        else
            plog("You need a tangible body to polymorph!");
        return;
    }

    /* Nothing to do */
    if (race == p->poly_race)
    {
        if (domsg)
            msg(p, "You are already using that form.");
        else
            plog("You are already using that form.");
        return;
    }

    /* Polymorph into normal form */
    if (!race)
    {
        if (domsg)
            msg(p, "You polymorph back into your normal form.");
        else
            plog("You polymorph back into your normal form.");

        /* Wraithform */
        player_clear_timed(p, TMD_WRAITHFORM, true);

        /* Invisibility */
        player_clear_timed(p, TMD_INVIS, true);

        /* Normal form */
        p->poly_race = NULL;
        p->k_idx = 0;
        if (domsg) Send_poly(p, 0);

        /* Notice */
        p->upkeep->update |= (PU_BONUS | PU_MONSTERS);

        /* Redraw */
        p->upkeep->redraw |= (PR_MAP | PR_EQUIP | PR_SPELL);

        return;
    }

    /* Skip non-entries */
    if (!race->name)
    {
        if (domsg)
            msg(p, "This monster race doesn't exist.");
        else
            plog("This monster race doesn't exist.");
        return;
    }

    /* Must not be unique (allow it to the DM for debug purposes) */
    if (rf_has(race->flags, RF_UNIQUE) && !is_dm_p(p))
    {
        if (domsg)
            msg(p, "This monster race is unique.");
        else
            plog("This monster race is unique.");
        return;
    }

    /* Check required kill count */
    if (check_kills)
    {
        struct monster_lore *lore = get_lore(p, race);
        int rkills = 1;

        /* Perfect affinity lowers the requirement to half of the required kills */
        if (lore->pkills) rkills = ((race->level * (200 - affinity(p, race))) / 200);

        if (lore->pkills < rkills)
        {
            if (domsg)
                msg(p, "You have not learned that form yet.");
            else
                plog("You have not learned that form yet.");
            return;
        }
    }

    /* Polymorph into that monster */
    if (!rf_has(race->flags, RF_UNIQUE))
        prefix = (is_a_vowel(tolower(race->name[0]))? "an ": "a ");
    if (domsg)
        msg(p, "You polymorph into %s%s.", prefix, race->name);
    else
        plog_fmt("You polymorph into %s%s.", prefix, race->name);

    /* Wraithform */
    if (rf_has(race->flags, RF_PASS_WALL))
    {
        p->timed[TMD_WRAITHFORM] = -1;
        p->upkeep->redraw |= PR_STATUS;
        handle_stuff(p);
    }
    else
        player_clear_timed(p, TMD_WRAITHFORM, true);

    /* Invisibility */
    if (rf_has(race->flags, RF_INVISIBLE))
    {
        p->timed[TMD_INVIS] = -1;
        p->upkeep->update |= PU_MONSTERS;
        p->upkeep->redraw |= PR_STATUS;
        handle_stuff(p);
    }
    else
        player_clear_timed(p, TMD_INVIS, true);

    /* New form */
    p->poly_race = race;
    if (domsg) Send_poly(p, race->ridx);

    /* Unaware players */
    p->k_idx = 0;
    if (rf_has(race->flags, RF_UNAWARE)) p->k_idx = -1;

    /* Hack -- random mimics */
    if (race->base == lookup_monster_base("random mimic"))
    {
        /* Random symbol from object set */
        while (1)
        {
            /* Select a random object */
            p->k_idx = randint0(z_info->k_max - 1) + 1;

            /* Skip non-entries */
            if (!k_info[p->k_idx].name) continue;

            /* Skip empty entries */
            if (!k_info[p->k_idx].d_attr || !k_info[p->k_idx].d_char)
                continue;

            /* Force race attr */
            if (k_info[p->k_idx].d_attr != race->d_attr) continue;

            /* Success */
            break;
        }
    }

    /* Hack -- object mimics */
    else if (race->mimic_kinds)
    {
        struct monster_mimic *mimic_kind;
        int i = 1;

        /* Pick a random object kind to mimic */
        for (mimic_kind = race->mimic_kinds; mimic_kind; mimic_kind = mimic_kind->next, i++)
        {
            if (one_in_(i)) p->k_idx = mimic_kind->kind->kidx;
        }
    }

    /* Notice */
    p->upkeep->update |= (PU_BONUS | PU_MONSTERS);

    /* Redraw */
    p->upkeep->redraw |= (PR_MAP | PR_EQUIP | PR_SPELL);
}


/*
 * Show every monster race with kill count/required kill count for polymorphing
 */
void do_cmd_check_poly(struct player *p, int line)
{
    char file_name[MSG_LEN];
    ang_file *fff;
    int k, total = 0;
    struct monster_race *race;
    struct monster_lore *lore;
    int aff, rkills;
    char* str;

    /* Temporary file */
    fff = file_temp(file_name, sizeof(file_name));
    if (!fff) return;

    /* Lowercase our search string */
    if (strlen(p->tempbuf) > 1)
    {
        for (str = p->tempbuf; *str; str++) *str = tolower((unsigned char)*str);
    }

    /* Scan the monster races */
    for (k = 1; k < z_info->r_max; k++)
    {
        bool ok;

        race = &r_info[k];
        lore = get_lore(p, race);

        /* Skip non-entries */
        if (!race->name) continue;

        /* Only print non uniques */
        if (rf_has(race->flags, RF_UNIQUE)) continue;

        /* Only display "known" races */
        if (!lore->pkills) continue;

        /* Check if the input is a symbol */
        if (strlen(p->tempbuf) == 1)
        {
            if (p->tempbuf[0] == '*')
                ok = true;
            else
                ok = (race->d_char == p->tempbuf[0]);
        }

        /* The input is a partial monster name */
        else
        {
            char monster[NORMAL_WID];

            /* Clean up monster name */
            clean_name(monster, race->name);

            /* Check if cleaned name matches our search string */
            ok = (strstr(monster, p->tempbuf) != NULL);
        }

        if (!ok) continue;

        /* Perfect affinity lowers the requirement to half of the required kills */
        aff = affinity(p, race);
        rkills = ((race->level * (200 - aff)) / 200);

        /* Check required kill count */
        if (lore->pkills >= rkills)
            file_putf(fff, "G[%d] %s: %d (learnt)\n", k, race->name, lore->pkills);
        else
        {
            file_putf(fff, "w[%d] %s: %d (%d more to go, affinity = %d%%)\n", k, race->name,
                lore->pkills, rkills - lore->pkills, aff);
        }

        total++;
    }

    if (!total) file_put(fff, "wNothing so far.\n");

    /* Close the file */
    file_close(fff);

    /* Display the file contents */
    show_file(p, file_name, "Killed List", line, 1);

    /* Remove the file */
    file_delete(file_name);
}   


/*
 * Show socials
 */
void do_cmd_check_socials(struct player *p, int line)
{
    char file_name[MSG_LEN];
    ang_file *fff;
    int k;
    struct social *s;

    /* Temporary file */
    fff = file_temp(file_name, sizeof(file_name));
    if (!fff) return;

    /* Scan the socials */
    for (k = 0; k < z_info->soc_max; k++)
    {
        s = &soc_info[k];

        /* Print the socials */
        file_putf(fff, "w%s\n", s->name);
    }

    /* Close the file */
    file_close(fff);

    /* Display the file contents */
    show_file(p, file_name, "Socials", line, 1);

    /* Remove the file */
    file_delete(file_name);
}


void do_cmd_interactive(struct player *p, int type, u32b query)
{
    /* Hack -- use special term */
    Send_term_info(p, NTERM_ACTIVATE, NTERM_WIN_SPECIAL);

    /* Let the player scroll through this info */
    p->special_file_type = type;

    /* Perform action */
    switch (type)
    {
        /* Help file */
        case SPECIAL_FILE_HELP:
            common_file_peruse(p, query);
            do_cmd_check_other(p, p->interactive_line - p->interactive_next);
            break;
    }

    /* Hack -- return to main term */
    Send_term_info(p, NTERM_ACTIVATE, NTERM_WIN_OVERHEAD);
}
