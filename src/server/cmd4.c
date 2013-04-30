/*
 * File: cmd4.c
 * Purpose: Interface commands
 *
 * Copyright (c) 1997-2007 Robert A. Koeneke, James E. Wilson, Ben Harrison,
 * Eytan Zweig, Andrew Doull, Pete Mack.
 * Copyright (c) 2004 DarkGod (HTML dump code)
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
#include "../common/tvalsval.h"
#include "cmds.h"
#include "files.h"
#include "history.h"
#include "netserver.h"
#include "wilderness.h"


/*
 * Array of feeling strings for object feelings.
 * Keep strings at 36 or less characters to keep the
 * combined feeling on one row.
 */
static const char *obj_feeling_text[][2] =
{
    {"this looks like any other level.", "this place looks familiar."},
    {"you sense an item of wondrous power!", "there was an item of wondrous power here!"},
    {"there are superb treasures here.", "there were superb treasures here."},
    {"there are excellent treasures here.", "there were excellent treasures here."},
    {"there are very good treasures here.", "there were very good treasures here."},
    {"there are good treasures here.", "there were good treasures here."},
    {"there may be something worthwhile here.", "there was something worthwhile here."},
    {"there may not be much interesting here.", "there was nothing interesting here."},
    {"there aren't many treasures here.", "there weren't many treasures here."},
    {"there are only scraps of junk here.", "there were only scraps of junk here."},
    {"there are naught but cobwebs here.", "there were naught but cobwebs here."}
};


/*
 * Array of feeling strings for monster feelings.
 * Keep strings at 36 or less characters to keep the
 * combined feeling on one row.
 */
static const char *mon_feeling_text[][2] =
{
    {"You are still uncertain about this place", "You are still uncertain about this place"},
    {"Omens of death haunt this place", "Omens of death haunted this place"},
    {"This place seems murderous", "This place seemed murderous"},
    {"This place seems terribly dangerous", "This place seemed terribly dangerous"},
    {"You feel anxious about this place", "You were anxious about this place"},
    {"You feel nervous about this place", "You were nervous about this place"},
    {"This place does not seem too risky", "This place did not seem too risky"},
    {"This place seems reasonably safe", "This place seemed reasonably safe"},
    {"This seems a tame, sheltered place", "This seemed a tame, sheltered place"},
    {"This seems a quiet, peaceful place", "This seemed a quiet, peaceful place"}
};


/*
 * Display the feeling. Players always get a monster feeling.
 * Object feelings are delayed until the player has explored some
 * of the level.
 *
 * Players entering a static level will get a different message, since
 * the feeling may not be accurate anymore.
 */
void display_feeling(struct player *p, bool obj_only)
{
    int obj_feeling;
    int mon_feeling;
    const char *join;
    byte set = 0;

    /* Don't show feelings for cold-hearted characters */
    if (OPT_P(p, birth_no_feelings)) return;

    /* No feeling in the town or on special levels */
    if (forbid_special(p->depth))
    {
        msg(p, "Looks like a typical town.");
        return;
    }

    /* No feeling in the wilderness */
    if (p->depth < 0)
    {
        msg(p, "Looks like a typical wilderness level.");
        return;
    }

    /* Hack -- Player entering a static level */
    if (p->feeling < 0)
    {
        int i, obj_f, mon_f;

        obj_feeling = 0;
        mon_feeling = 0;

        /* Get a feeling from other players */
        for (i = 1; i <= NumPlayers; i++)
        {
            player_type *q_ptr = player_get(i);

            if (q_ptr == p) continue;
            if (q_ptr->depth != p->depth) continue;
            if (q_ptr->feeling < 0) continue;

            obj_f = q_ptr->feeling / 10;
            mon_f = q_ptr->feeling - (10 * obj_f);

            if (obj_f > obj_feeling) obj_feeling = obj_f;
            if (mon_f > mon_feeling) mon_feeling = mon_f;
        }

        /* Display a different message */
        set = 1;
    }
    else
    {
        obj_feeling = p->feeling / 10;
        mon_feeling = p->feeling - (10 * obj_feeling);
    }

    /* Verify the feeling */
    if (obj_feeling >= N_ELEMENTS(obj_feeling_text))
        obj_feeling = N_ELEMENTS(obj_feeling_text) - 1;

    /* Display only the object feeling when it's first discovered. */
    if (obj_only)
    {
        msg(p, "You feel that %s", obj_feeling_text[obj_feeling][set]);
        return;
    }

    /* Verify the feeling */
    if (mon_feeling >= N_ELEMENTS(mon_feeling_text))
        mon_feeling = N_ELEMENTS(mon_feeling_text) - 1;

    /* Players automatically get a monster feeling. */
    if (p->cave->feeling_squares < FEELING1)
    {
        msg(p, "%s.", mon_feeling_text[mon_feeling][set]);
        return;
    }

    /* Decide the conjunction */
    if (((mon_feeling <= 5) && (obj_feeling > 6)) || ((mon_feeling > 5) && (obj_feeling <= 6)))
        join = ", yet";
    else
        join = ", and";

    /* Display the feeling */
    msg(p, "%s%s %s", mon_feeling_text[mon_feeling][set], join, obj_feeling_text[obj_feeling][set]);
}


/***** Knowledge screen *****/


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
    {TV_SPIKE, NULL},
    {TV_FLASK, NULL},
    {0, NULL}
};


/* Helper class for generating joins */
typedef struct join
{
    int oid;
    int gid;
} join_t;


/* A default group-by */
static join_t *default_join;


static int *obj_group_order = NULL;


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


/*
 * Description of each feature group
 */
const char *feature_group_text[] =
{
    "Floors",
    "Traps",
    "Doors",
    "Stairs",
    "Walls",
    "Streamers",
    "Obstructions",
    "Stores",
    "Other",
    NULL
};


/*
 * Return a specific ordering for the features
 */
static int feat_order(int feat)
{
    feature_type *f_ptr = &f_info[feat];

    switch (feat)
    {
        case FEAT_WATER:
        case FEAT_MUD:
        case FEAT_DRAWBRIDGE:
        case FEAT_LOOSE_DIRT:
        case FEAT_CROP:
        case FEAT_MOUNTAIN: return 0;
        case FEAT_TREE:
        case FEAT_EVIL_TREE: return 6;
        case FEAT_SWAMP: return 8;
    }

    switch (f_ptr->d_char)
    {
        case '.': return 0;
        case '^': return 1;
        case '\'':
        case '+': return 2;
        case '<':
        case '>': return 3;
        case '#': return 4;
        case '*':
        case '%': return 5;
        case ';':
        case ':': return 6;
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9': return 7;
        default: return 8;
    }
}


/*** Objects ***/


struct cmp_obj
{
    struct player *p;
    const object_kind *kind;
};


static int o_cmp_tval(const void *a, const void *b)
{
    const struct cmp_obj *pa = a;
    const struct cmp_obj *pb = b;
    const object_kind *k_a = pa->kind;
    const object_kind *k_b = pb->kind;

    /* Group by */
    int ta = obj_group_order[k_a->tval];
    int tb = obj_group_order[k_b->tval];
    int c = ta - tb;

    if (c) return c;

    /* Order by */
    c = pa->p->obj_aware[k_a->kidx] - pb->p->obj_aware[k_b->kidx];
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
            if (pa->p->obj_aware[k_a->kidx]) return strcmp(k_a->name, k_b->name);

            /* Then in tried order */
            c = pa->p->obj_tried[k_a->kidx] - pb->p->obj_tried[k_b->kidx];
            if (c) return -c;

            return strcmp(k_a->flavor->text, k_b->flavor->text);
        }
    }

    return k_a->sval - k_b->sval;
}


/*
 * Display known objects
 */
static void do_cmd_knowledge_objects(int Ind, int line)
{
    player_type *p_ptr = player_get(Ind);
    struct cmp_obj *objects;
    int o_count = 0;
    int i;
    char file_name[MSG_LEN];
    ang_file *fff;
    int o_group = -1;

    /* Temporary file */
    fff = file_temp(file_name, sizeof(file_name));
    if (!fff) return;

    objects = C_ZNEW(z_info->k_max, struct cmp_obj);

    /* Initialize other static variables */
    if (!obj_group_order)
    {
        int gid = -1;

        obj_group_order = C_ZNEW(TV_MAX, int);

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
        object_kind *kind = &k_info[i];

        /*
         * It's in the list if we've ever seen it, or it has a flavour,
         * and either it's not one of the special artifacts, or if it is,
         * we're not aware of it yet. This way the flavour appears in the list
         * until it is found.
         */
        if ((p_ptr->kind_everseen[i] || kind->flavor) &&
            (!of_has(kind->flags, OF_INSTA_ART) || !p_ptr->obj_aware[i]))
        {
            int c = obj_group_order[kind->tval];

            if (c >= 0)
            {
                objects[o_count].p = p_ptr;
                objects[o_count++].kind = kind;
            }
        }
    }

    /* Sort */
    sort(objects, o_count, sizeof(*objects), o_cmp_tval);

    /* Print the objects */
    for (i = 0; i < o_count; i++)
    {
        const object_kind *kind = objects[i].kind;
        char o_name[NORMAL_WID];
        int gid = obj_group_order[kind->tval];

        /* Choose a color */
        bool aware = p_ptr->obj_aware[kind->kidx];
        byte attr = (aware? TERM_WHITE: TERM_SLATE);

        /* Find graphics bits -- Versions of the object_char and object_attr defines */
        byte a = object_kind_attr(p_ptr, kind);
        char c = object_kind_char(p_ptr, kind);

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
        if (p_ptr->obj_tried[kind->kidx] && !aware) my_strcat(o_name, " {tried}", sizeof(o_name));

        /* Append flavour if desired */
        else if (OPT_P(p_ptr, show_flavors) && kind->flavor && aware)
        {
            my_strcat(o_name, " (", sizeof(o_name));
            my_strcat(o_name, kind->flavor->text, sizeof(o_name));
            my_strcat(o_name, ")", sizeof(o_name));
        }

        /* Print a message */
        if (p_ptr->tile_distorted)
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
    show_file(Ind, file_name, "Known Objects", line, 1);

    /* Remove the file */
    file_delete(file_name);
}


/*** Artifacts ***/


struct cmp_art
{
    const artifact_type *artifact;
    char highlight;
};


static int a_cmp_tval(const void *a, const void *b)
{
    const struct cmp_art *pa = a;
    const struct cmp_art *pb = b;
    const artifact_type *a_a = pa->artifact;
    const artifact_type *a_b = pb->artifact;

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
    int i, j;
    char *highlight = C_ZNEW(z_info->a_max, char);

    /* Check inventory and equipment of players */
    for (i = 1; i <= NumPlayers; i++)
    {
        player_type *p_ptr = player_get(i);

        /* Check this guy */
        for (j = 0; j < ALL_INVEN_TOTAL; j++)
        {
            object_type *o_ptr;

            /* Get the object */
            o_ptr = &p_ptr->inventory[j];

            /* Ignore non-objects */
            if (!o_ptr->kind) continue;

            /* Ignore non-artifacts */
            if (!true_artifact_p(o_ptr)) continue;

            /* Artifact is owned by the player */
            if (p_ptr == p) highlight[o_ptr->artifact->aidx] = 'w';

            /* Artifact is owned by someone else */
            else highlight[o_ptr->artifact->aidx] = 'D';
        }
    }

    /* Check the dungeon */
    for (i = 1; i < o_max; i++)
    {
        object_type *o_ptr;

        /* Get the object */
        o_ptr = object_byid(i);

        /* Skip "dead" objects */
        if (!o_ptr->kind) continue;

        /* Ignore non-artifacts */
        if (!true_artifact_p(o_ptr)) continue;

        /* Artifact is owned by the player */
        if (o_ptr->owner == p->id) highlight[o_ptr->artifact->aidx] = 'w';

        /* Artifact is not owned or owned by someone else */
        else if (object_is_known(p, o_ptr)) highlight[o_ptr->artifact->aidx] = 'D';

        /* Artifact is unknown */
        else highlight[o_ptr->artifact->aidx] = highlight_unknown(p, o_ptr->artifact->aidx);
    }

    /* Collect matching artifacts */
    for (i = 0; i < z_info->a_max; i++)
    {
        const artifact_type *a_ptr = &a_info[i];
        char h = highlight[i];

        /* Skip "empty" artifacts */
        if (!a_ptr->name) continue;

        /* Skip "uncreated + unfound" artifacts */
        if ((p->art_info[i] < ARTS_FOUND) && !a_ptr->created) continue;

        /* Artifact is "uncreated" */
        if (!a_ptr->created) h = highlight_unknown(p, i);

        /* Special case: artifact is owned by a non-connected player */
        if (!h)
        {
            /* Artifact is owned */
            if (a_ptr->owned) h = 'D';

            /* Artifact is unknown */
            else h = highlight_unknown(p, i);
        }

        /* Artifact is known */
        if (h != 'd')
        {
            int c = obj_group_order[a_ptr->tval];

            if (c >= 0)
            {
                artifacts[a_count].artifact = a_ptr;
                artifacts[a_count++].highlight = h;
            }
        }
    }

    /* Free memory */
    mem_free(highlight);

    return a_count;
}


/*
 * Display known artifacts
 */
static void do_cmd_knowledge_artifacts(int Ind, int line)
{
    player_type *p_ptr = player_get(Ind);
    struct cmp_art *artifacts;
    int a_count = 0;
    int i;
    char file_name[MSG_LEN];
    ang_file *fff;
    int a_group = -1;

    /* Temporary file */
    fff = file_temp(file_name, sizeof(file_name));
    if (!fff) return;

    artifacts = C_ZNEW(z_info->a_max, struct cmp_art);

    /* Initialize other static variables */
    if (!obj_group_order)
    {
        int gid = -1;

        obj_group_order = C_ZNEW(TV_MAX, int);

        /* Allow for missing values */
        for (i = 0; i < TV_MAX; i++) obj_group_order[i] = -1;

        for (i = 0; 0 != object_text_order[i].tval; i++)
        {
            if (object_text_order[i].name) gid = i;
            obj_group_order[object_text_order[i].tval] = gid;
        }
    }

    /* Collect valid artifacts */
    a_count = collect_known_artifacts(p_ptr, artifacts);

    /* Sort */
    sort(artifacts, a_count, sizeof(*artifacts), a_cmp_tval);

    /* Print the artifacts */
    for (i = 0; i < a_count; i++)
    {
        const artifact_type *a_ptr = artifacts[i].artifact;
        char o_name[NORMAL_WID];
        int gid = obj_group_order[a_ptr->tval];
        object_type object_type_body;
        object_type *o_ptr = &object_type_body;

        /* Paranoia */
        if (gid == -1) continue;
        if (!make_fake_artifact(o_ptr, (artifact_type *)a_ptr)) continue;

        /* Print group */
        if (gid != a_group)
        {
            a_group = gid;
            file_putf(fff, "w%s\n", object_text_order[a_group].name);
        }

        /* Describe the artifact */
        object_desc(p_ptr, o_name, sizeof(o_name), o_ptr, ODESC_ARTIFACT);

        /* Print a message */
        file_putf(fff, "%c     The %s\n", artifacts[i].highlight, o_name);
    }

    mem_free(artifacts);
    mem_free(obj_group_order);
    obj_group_order = NULL;

    /* Close the file */
    file_close(fff);

    /* Display the file contents */
    show_file(Ind, file_name, "Artifacts", line, 1);

    /* Remove the file */
    file_delete(file_name);
}


/*** Ego items ***/


static int e_cmp_tval(const void *a, const void *b)
{
    const ego_item_type *ea = &e_info[default_join[*(const int *)a].oid];
    const ego_item_type *eb = &e_info[default_join[*(const int *)b].oid];

    /* Group by */
    int c = default_join[*(const int *)a].gid - default_join[*(const int *)b].gid;

    if (c) return c;

    /* Order by */
    return strcmp(ea->name, eb->name);
}


/*
 * Display known ego items
 */
static void do_cmd_knowledge_ego_items(int Ind, int line)
{
    player_type *p_ptr = player_get(Ind);
    int *egoitems;
    int e_count = 0;
    int i, j;
    char file_name[MSG_LEN];
    ang_file *fff;
    int e_group = -1;

    /* Temporary file */
    fff = file_temp(file_name, sizeof(file_name));
    if (!fff) return;

    egoitems = C_ZNEW(z_info->e_max * EGO_TVALS_MAX, int);
    default_join = C_ZNEW(z_info->e_max * EGO_TVALS_MAX, join_t);

    /* Initialize other static variables */
    if (!obj_group_order)
    {
        int gid = -1;

        obj_group_order = C_ZNEW(TV_MAX, int);

        /* Allow for missing values */
        for (i = 0; i < TV_MAX; i++) obj_group_order[i] = -1;

        for (i = 0; 0 != object_text_order[i].tval; i++)
        {
            if (object_text_order[i].name) gid = i;
            obj_group_order[object_text_order[i].tval] = gid;
        }
    }

    /* Collect matching ego items */
    for (i = 1; i < z_info->e_max; i++)
    {
        /* Ignore non-egos && unknown egos */
        if (!(e_info[i].name && p_ptr->ego_everseen[i])) continue;

        /* Collect ego items */
        for (j = 0; j < EGO_TVALS_MAX && e_info[i].tval[j]; j++)
        {
            int gid = obj_group_order[e_info[i].tval[j]];

            /* Ignore duplicate gids */
            if ((j > 0) && (gid == default_join[e_count - 1].gid)) continue;

            egoitems[e_count] = e_count;
            default_join[e_count].oid = i;
            default_join[e_count++].gid = gid;
        }
    }

    /* Sort */
    sort(egoitems, e_count, sizeof(*egoitems), e_cmp_tval);

    /* Print the ego items */
    for (i = 0; i < e_count; i++)
    {
        ego_item_type *e_ptr = &e_info[default_join[egoitems[i]].oid];
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
        file_putf(fff, "     %s\n", e_ptr->name);
    }

    mem_free(default_join);
    mem_free(egoitems);
    mem_free(obj_group_order);
    obj_group_order = NULL;

    /* Close the file */
    file_close(fff);

    /* Display the file contents */
    show_file(Ind, file_name, "Ego Items", line, 0);

    /* Remove the file */
    file_delete(file_name);
}


/*** Monsters ***/


static int m_cmp_race(const void *a, const void *b)
{
    const monster_race *r_a = &r_info[default_join[*(const int *)a].oid];
    const monster_race *r_b = &r_info[default_join[*(const int *)b].oid];
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
static void do_cmd_knowledge_monsters(int Ind, int line)
{
    player_type *p_ptr = player_get(Ind);
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
        monster_race *r_ptr = &r_info[i];
        monster_lore *l_ptr = &p_ptr->lore[i];

        if (!l_ptr->pseen) continue;
        if (!r_ptr->name) continue;

        if (rf_has(r_ptr->flags, RF_UNIQUE)) m_count++;

        for (j = 1; j < N_ELEMENTS(monster_group) - 1; j++)
        {
            const char *pat = monster_group[j].chars;

            if (strchr(pat, r_ptr->d_char)) m_count++;
        }
    }

    default_join = C_ZNEW(m_count, join_t);
    monsters = C_ZNEW(m_count, int);

    /* Collect matching monsters */
    m_count = 0;
    for (i = 1; i < z_info->r_max; i++)
    {
        monster_race *r_ptr = &r_info[i];
        monster_lore *l_ptr = &p_ptr->lore[i];

        if (!l_ptr->pseen) continue;
        if (!r_ptr->name) continue;

        for (j = 0; j < N_ELEMENTS(monster_group) - 1; j++)
        {
            const char *pat = monster_group[j].chars;

            if ((j == 0) && !rf_has(r_ptr->flags, RF_UNIQUE)) continue;
            else if ((j > 0) && !strchr(pat, r_ptr->d_char)) continue;

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
        int r_idx = default_join[monsters[i]].oid;
        int gid = default_join[monsters[i]].gid;
        char kills[6];

        /* Access the race */
        monster_race *r_ptr = &r_info[r_idx];
        monster_lore *l_ptr = &p_ptr->lore[r_idx];

        /* Find graphics bits */
        byte a = p_ptr->r_attr[r_idx];
        char c = p_ptr->r_char[r_idx];

        /* Paranoia */
        if (gid == -1) continue;

        /* Print group */
        if (gid != m_group)
        {
            m_group = gid;
            file_putf(fff, "w%s\n", monster_group[m_group].name);
        }

        /* If uniques are purple, make it so */
        if (OPT_P(p_ptr, purple_uniques) && rf_has(r_ptr->flags, RF_UNIQUE) && !(a & 0x80))
            a = TERM_VIOLET;

        /* Display kills */
        if (rf_has(r_ptr->flags, RF_UNIQUE))
            my_strcpy(kills, (l_ptr->pkills? " dead": "alive"), sizeof(kills));
        else
            strnfmt(kills, sizeof(kills), "%5d", l_ptr->pkills);

        /* Print a message */
        if (p_ptr->tile_distorted)
            file_putf(fff, "w     %-40s  %s\n", r_ptr->name, kills);
        else
        {
            file_putf(fff, "d%c%cw%-40s  %s\n", ((a & 0x80)? a: color_attr_to_char(a)), c,
                r_ptr->name, kills);
        }
    }

    mem_free(default_join);
    mem_free(monsters);

    /* Close the file */
    file_close(fff);

    /* Display the file contents */
    show_file(Ind, file_name, "Monsters", line, 1);

    /* Remove the file */
    file_delete(file_name);
}


/*** Features ***/


static int f_cmp_fkind(const void *a, const void *b)
{
    const feature_type *fa = &f_info[*(const int *)a];
    const feature_type *fb = &f_info[*(const int *)b];

    /* Group by */
    int c = feat_order(*(const int *)a) - feat_order(*(const int *)b);

    if (c) return c;

    /* Order by feature name */
    return strcmp(fa->name, fb->name);
}


/*
 * Interact with feature visuals
 */
static void do_cmd_knowledge_features(int Ind, int line)
{
    player_type *p_ptr = player_get(Ind);
    int *features;
    int f_count = 0;
    int i;
    char file_name[MSG_LEN];
    ang_file *fff;
    int f_group = -1;

    /* Temporary file */
    fff = file_temp(file_name, sizeof(file_name));
    if (!fff) return;

    features = C_ZNEW(z_info->f_max, int);

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
        const feature_type *f_ptr = &f_info[features[i]];
        int gid = feat_order(features[i]);

        /* Find graphics bits */
        byte a = p_ptr->f_attr[features[i]][FEAT_LIGHTING_LIT];
        char c = p_ptr->f_char[features[i]][FEAT_LIGHTING_LIT];

        /* Paranoia */
        if (gid == -1) continue;

        /* Print group */
        if (gid != f_group)
        {
            f_group = gid;
            file_putf(fff, "w%s\n", feature_group_text[f_group]);
        }

        /* Print a message */
        if (p_ptr->tile_distorted)
            file_putf(fff, "w     %s\n", f_ptr->name);
        else
            file_putf(fff, "d%c%cw%s\n", ((a & 0x80)? a: color_attr_to_char(a)), c, f_ptr->name);
    }

    mem_free(features);

    /* Close the file */
    file_close(fff);

    /* Display the file contents */
    show_file(Ind, file_name, "Features", line, 1);

    /* Remove the file */
    file_delete(file_name);
}


/*** Character history ***/


static void do_cmd_knowledge_history(int Ind, int line)
{
    player_type *p_ptr = player_get(Ind);
    ang_file *fff;
    char file_name[MSG_LEN];

    /* Temporary file */
    fff = file_temp(file_name, sizeof(file_name));
    if (!fff) return;

    /* Dump character history */
    dump_history(p_ptr, fff);

    /* Close the file */
    file_close(fff);

    /* Display the file contents */
    show_file(Ind, file_name, "Character History", line, 0);

    /* Remove the file */
    file_delete(file_name);
}


/*** Unique monsters ***/


/*
 * Display known uniques
 */
static void do_cmd_knowledge_uniques(int Ind, int line)
{
    player_type *p_ptr = player_get(Ind);
    int k, l, i, space, namelen, total = 0, width = NORMAL_WID - 2;
    ang_file *fff;
    char file_name[MSG_LEN], buf[MSG_LEN];
    int* idx;
    monster_race *r_ptr, *curr_ptr;
    monster_lore *l_ptr;

    /* Temporary file */
    fff = file_temp(file_name, sizeof(file_name));
    if (!fff) return;

    idx = C_ZNEW(z_info->r_max, int);

    /* Scan the monster races */
    for (k = 1; k < z_info->r_max; k++)
    {
        r_ptr = &r_info[k];

        /* Only print Uniques */
        if (rf_has(r_ptr->flags, RF_UNIQUE))
        {
            /* Only display "known" uniques */
            if (r_ptr->lore.seen)
            {
                l = 0;
                while (l < total)
                {
                    curr_ptr = &r_info[idx[l]];
                    if (r_ptr->level > curr_ptr->level) break;
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
            byte ok = FALSE;
            char highlight = 'D';

            r_ptr = &r_info[idx[l]];
            strnfmt(buf, sizeof(buf), "%s has been killed by: ", r_ptr->name);
            space = width - strlen(buf);

            /* Do we need to highlight this unique? */
            l_ptr = &p_ptr->lore[idx[l]];
            if (l_ptr->pkills) highlight = 'w';

            /* Append all players who killed this unique */
            k = 0;
            for (i = 1; i <= NumPlayers; i++)
            {
                player_type *q_ptr = player_get(i);

                /* Skip the DM... except for the DM */
                if ((q_ptr->dm_flags & DM_SECRET_PRESENCE) && !is_dm_p(p_ptr)) continue;

                l_ptr = &q_ptr->lore[idx[l]];
                if (l_ptr->pkills)
                {
                    ok = TRUE;
                    namelen = strlen(q_ptr->name) + 2;
                    if (space - namelen < 0)
                    {
                        /* Out of space, flush the line */
                        file_putf(fff, "%c%s\n", highlight, buf);
                        my_strcpy(buf, "  ", sizeof(buf));
                        k = 0;
                        space = width;
                    }
                    if (k++) my_strcat(buf, ", ", sizeof(buf));
                    my_strcat(buf, q_ptr->name, sizeof(buf));
                    space -= namelen;
                }
            }

            if (ok)
                file_putf(fff, "%c%s\n", highlight, buf);
            else if (r_ptr->lore.tkills)
                file_putf(fff, "D%s has been killed by somebody.\n", r_ptr->name);
            else
                file_putf(fff, "D%s has never been killed!\n", r_ptr->name);
        }
    }
    else file_put(fff, "wNo uniques are witnessed so far.\n");

    /* Close the file */
    file_close(fff);

    /* Display the file contents */
    show_file(Ind, file_name, "Known Uniques", line, 1);

    /* Remove the file */
    file_delete(file_name);

    mem_free(idx);
}


/*** Party gear ***/


/*
 * Display party gear
 */
static void do_cmd_knowledge_gear(int Ind, int line)
{
    player_type *p_ptr = player_get(Ind);
    int k, i;
    ang_file *fff;
    char file_name[MSG_LEN];
    object_type *o_ptr;
    char o_name[NORMAL_WID];
    byte color;

    /* Temporary file */
    fff = file_temp(file_name, sizeof(file_name));
    if (!fff) return;

    /* Scan the players */
    for (k = 1; k < NumPlayers + 1; k++)
    {
        player_type *q_ptr = player_get(k);

        /* Only print connected players */
        if (q_ptr->conn == NOT_CONNECTED) continue;

        /* Don't display self */
        if (Ind == k) continue;

        /* DM sees everybody - others see party members */
        if ((!p_ptr->party || (p_ptr->party != q_ptr->party)) && !(p_ptr->dm_flags & DM_SEE_PLAYERS))
            continue;

        /* Print a message */
        if (q_ptr->total_winner)
        {
            file_putf(fff, "G     %s the %s %s (%s, Level %d) at %d ft\n", q_ptr->name,
                q_ptr->race->name, q_ptr->clazz->name, q_ptr->sex->winner, q_ptr->lev,
                q_ptr->depth * 50);
        }
        else
        {
            file_putf(fff, "G     %s the %s %s (Level %d) at %d ft\n", q_ptr->name,
                q_ptr->race->name, q_ptr->clazz->name, q_ptr->lev, q_ptr->depth * 50);
        }

        /* Display the equipment */
        for (i = INVEN_WIELD; i < ALL_INVEN_TOTAL; i++)
        {
            o_ptr = &q_ptr->inventory[i];

            /* We need an item */
            if (!o_ptr || !o_ptr->kind) continue;

            /* Obtain an item description */
            object_desc(p_ptr, o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_FULL);

            /* Get the color */
            color = p_ptr->tval_attr[o_ptr->tval % N_ELEMENTS(tval_to_attr)];

            /* Display item description */
            file_putf(fff, "%c         %s\n", color_attr_to_char(color), o_name);
        }

        /* Next */
        file_putf(fff, "w\n");
    }

    /* Close the file */
    file_close(fff);

    /* Display the file contents */
    if (p_ptr->dm_flags & DM_SEE_PLAYERS)
        show_file(Ind, file_name, "Player List", line, 1);
    else
        show_file(Ind, file_name, "Party Members", line, 1);

    /* Remove the file */
    file_delete(file_name);
}


/*** Houses ***/


/*
 * Verify the current panel (relative to a location).
 */
static void verify_panel_loc(int Ind, int py, int px, s16b *sy, s16b *sx)
{
    player_type *p_ptr = player_get(Ind);
    int screen_hgt, screen_wid;
    int panel_wid, panel_hgt;

    screen_hgt = p_ptr->screen_rows / p_ptr->tile_hgt;
    screen_wid = p_ptr->screen_cols / p_ptr->tile_wid;

    panel_wid = screen_wid / 2;
    panel_hgt = screen_hgt / 2;

    /* Scroll screen vertically when 3 grids from top/bottom edge */
    if ((py < *sy + 3) || (py >= *sy + screen_hgt - 3))
        *sy = py - panel_hgt;

    /* Scroll screen horizontally when 3 grids from left/right edge */
    if ((px < *sx + 3) || (px >= *sx + screen_wid - 3))
        *sx = px - panel_wid;

    /* Verify wy, adjust if needed */
    if (*sy > DUNGEON_HGT - screen_hgt) *sy = DUNGEON_HGT - screen_hgt;
    if (*sy < 0) *sy = 0;

    /* Verify wx, adjust if needed */
    if (*sx > DUNGEON_WID - screen_wid) *sx = DUNGEON_WID - screen_wid;
    if (*sx < 0) *sx = 0;
}


/*
 * Display owned houses
 */
static void do_cmd_knowledge_houses(int Ind, int line)
{
    player_type *p_ptr = player_get(Ind);
    char file_name[MSG_LEN];
    ang_file *fff;
    int i, j = 0;
    char buf[160];
    s16b sy, sx;
    char dpt[8];
    int panel_wid, panel_hgt;

    panel_wid = BLOCK_WID / p_ptr->tile_wid;
    panel_hgt = BLOCK_HGT / p_ptr->tile_hgt;

    /* Temporary file */
    fff = file_temp(file_name, sizeof(file_name));
    if (!fff) return;

    for (i = 0; i < num_houses; i++)
    {
        if (house_owned_by(p_ptr, i))
        {
            j++;

            dpt[0] = '\0';
            wild_cat_depth(houses[i].depth, dpt, sizeof(dpt));

            verify_panel_loc(Ind, houses[i].y_1, houses[i].x_1, &sy, &sx);

            strnfmt(buf, sizeof(buf), "  %c) House %d %s %s, sector [%d,%d]\n",
                index_to_label(j - 1), j, (!houses[i].depth? "in": "at"), dpt, (sy / panel_hgt),
                (sx / panel_wid));
            file_put(fff, buf);
        }
    }
    if (!j) file_put(fff, "You do not own any house.\n");

    /* Close the file */
    file_close(fff);

    /* Display the file contents */
    show_file(Ind, file_name, "Owned Houses", line, 0);

    /* Remove the file */
    file_delete(file_name);
}


/*
 * Display the "player knowledge" menu.
 */
void do_cmd_knowledge(int Ind, char type, int line)
{
    player_type *p_ptr = player_get(Ind);

    switch (type)
    {
        /* Display object knowledge */
        case SPECIAL_FILE_OBJECT:
            Send_term_info(p_ptr, NTERM_ACTIVATE, NTERM_WIN_SPECIAL);
            do_cmd_knowledge_objects(Ind, line);
            Send_term_info(p_ptr, NTERM_ACTIVATE, NTERM_WIN_OVERHEAD);
            break;

        /* Display artifact knowledge */
        case SPECIAL_FILE_ARTIFACT:
            Send_term_info(p_ptr, NTERM_ACTIVATE, NTERM_WIN_SPECIAL);
            do_cmd_knowledge_artifacts(Ind, line);
            Send_term_info(p_ptr, NTERM_ACTIVATE, NTERM_WIN_OVERHEAD);
            break;

        /* Display ego item knowledge */
        case SPECIAL_FILE_EGO:
            Send_term_info(p_ptr, NTERM_ACTIVATE, NTERM_WIN_SPECIAL);
            do_cmd_knowledge_ego_items(Ind, line);
            Send_term_info(p_ptr, NTERM_ACTIVATE, NTERM_WIN_OVERHEAD);
            break;

        /* Display monster knowledge */
        case SPECIAL_FILE_KILL:
            Send_term_info(p_ptr, NTERM_ACTIVATE, NTERM_WIN_SPECIAL);
            do_cmd_knowledge_monsters(Ind, line);
            Send_term_info(p_ptr, NTERM_ACTIVATE, NTERM_WIN_OVERHEAD);
            break;

        /* Display feature knowledge */
        case SPECIAL_FILE_FEATURE:
            Send_term_info(p_ptr, NTERM_ACTIVATE, NTERM_WIN_SPECIAL);
            do_cmd_knowledge_features(Ind, line);
            Send_term_info(p_ptr, NTERM_ACTIVATE, NTERM_WIN_OVERHEAD);
            break;

        /* Display hall of fame */
        case SPECIAL_FILE_SCORES:
            Send_term_info(p_ptr, NTERM_ACTIVATE, NTERM_WIN_SPECIAL);
            do_cmd_knowledge_scores(Ind, line);
            Send_term_info(p_ptr, NTERM_ACTIVATE, NTERM_WIN_OVERHEAD);
            break;

        /* Display character history */
        case SPECIAL_FILE_HISTORY:
            Send_term_info(p_ptr, NTERM_ACTIVATE, NTERM_WIN_SPECIAL);
            do_cmd_knowledge_history(Ind, line);
            Send_term_info(p_ptr, NTERM_ACTIVATE, NTERM_WIN_OVERHEAD);
            break;

        /* Display known uniques */
        case SPECIAL_FILE_UNIQUE:
            Send_term_info(p_ptr, NTERM_ACTIVATE, NTERM_WIN_SPECIAL);
            do_cmd_knowledge_uniques(Ind, line);
            Send_term_info(p_ptr, NTERM_ACTIVATE, NTERM_WIN_OVERHEAD);
            break;

        /* Display party gear */
        case SPECIAL_FILE_GEAR:
            Send_term_info(p_ptr, NTERM_ACTIVATE, NTERM_WIN_SPECIAL);
            do_cmd_knowledge_gear(Ind, line);
            Send_term_info(p_ptr, NTERM_ACTIVATE, NTERM_WIN_OVERHEAD);
            break;

        /* Display owned houses */
        case SPECIAL_FILE_HOUSES:
            Send_term_info(p_ptr, NTERM_ACTIVATE, NTERM_WIN_SPECIAL);
            do_cmd_knowledge_houses(Ind, line);
            Send_term_info(p_ptr, NTERM_ACTIVATE, NTERM_WIN_OVERHEAD);
            break;
    }
}


/*
 * Check the status of "players"
 *
 * The player's name, race, class, and experience level are shown.
 */
void do_cmd_check_players(int Ind, int line)
{
    int k;
    ang_file *fff;
    char file_name[MSG_LEN];
    player_type *p_ptr = player_get(Ind);

    /* Temporary file */
    fff = file_temp(file_name, sizeof(file_name));
    if (!fff) return;

    /* Scan the player races */
    for (k = 1; k < NumPlayers + 1; k++)
    {
        player_type *q_ptr = player_get(k);
        byte attr = 'w';
        const char *brave = "the";
        char winner[20];
        const char *batty = "";

        /* Only print connected players */
        if (q_ptr->conn == NOT_CONNECTED) continue;

        /*
         * Don't display the dungeon master if the secret_dungeon_master
         * option is set (unless you're a DM yourself)
         */
        if ((q_ptr->dm_flags & DM_SECRET_PRESENCE) && !(p_ptr->dm_flags & DM_SEE_PLAYERS)) continue;

        /*** Determine color ***/

        /* Print self in green */
        if (Ind == k) attr = 'G';

        /* Print party members in blue */
        else if (p_ptr->party && p_ptr->party == q_ptr->party) attr = 'B';

        /* Print hostile players in red */
        else if (pvp_check(p_ptr, q_ptr, PVP_CHECK_BOTH, TRUE, FEAT_NONE)) attr = 'r';

        /* Print potential hostile players in yellow */
        else if (pvp_check(p_ptr, q_ptr, PVP_CHECK_ONE, TRUE, FEAT_NONE)) attr = 'y';

        /* Output color byte */
        file_putf(fff, "%c", attr);

        if (OPT_P(q_ptr, birth_no_ghost))
        {
            if (OPT_P(q_ptr, birth_ironman)) brave = "the iron";
            else brave = "the brave";
        }
        else if (OPT_P(q_ptr, birth_ironman)) brave = "the hardcore";
        winner[0] = '\0';
        if (q_ptr->total_winner) strnfmt(winner, sizeof(winner), "%s, ", q_ptr->sex->winner);
        if (OPT_P(q_ptr, birth_fruit_bat)) batty = "batty, ";

        /* Print a message */
        file_putf(fff, "     %s %s %s %s (%s%sLevel %d, %s)", q_ptr->name, brave, q_ptr->race->name,
            q_ptr->clazz->name, winner, batty, q_ptr->lev, parties[q_ptr->party].name);

        /* Print extra info if these people are not 'red' aka hostile */
        /* Hack -- Always show extra info to dungeon master */
        if ((attr != 'r') || (p_ptr->dm_flags & DM_SEE_PLAYERS))
            file_putf(fff, " at %d ft", q_ptr->depth * 50);

        /* Newline */
        file_put(fff, "\n");
        file_putf(fff, "U         %s@%s\n", q_ptr->other.full_name, q_ptr->hostname);
    }

    /* Close the file */
    file_close(fff);

    /* Display the file contents */
    show_file(Ind, file_name, "Player List", line, 1);

    /* Remove the file */
    file_delete(file_name);
}


/*
 * Scroll through information.
 */
void do_cmd_check_other(int Ind, int line)
{
    player_type *p_ptr = player_get(Ind);
    char buf[NORMAL_WID + 1];
    int i, j;
    s16b last_info_line = get_last_info_line(Ind);

    /* Make sure the player is allowed to */
    if (!p_ptr->special_file_type) return;

    /* Dump the next 20 lines of the file */
    for (i = 0; i < 20; i++)
    {
        byte attr = TERM_WHITE;

        /* We're done */
        if (line + i > MAX_TXT_INFO) break;
        if (line + i > last_info_line) break;

        /* Extract string */
        for (j = 0; j < NORMAL_WID; j++)
            buf[j] = get_info(Ind, line + i, j)->c;
        attr = get_info(Ind, line + i, 0)->a;
        buf[j] = '\0';

        /* Dump the line */
        Send_special_line(Ind, last_info_line, last_info_line - line, i, attr, &buf[0]);
    }
}


/*
 * Show every monster race with kill count/required kill count for polymorphing
 */
void do_cmd_check_poly(int Ind, int line)
{
    char file_name[MSG_LEN];
    ang_file *fff;
    int k, total = 0;
    monster_race *r_ptr;
    monster_lore *l_ptr;
    player_type *p_ptr = player_get(Ind);

    /* Temporary file */
    fff = file_temp(file_name, sizeof(file_name));
    if (!fff) return;

    /* Scan the monster races */
    for (k = 1; k < z_info->r_max; k++)
    {
        r_ptr = &r_info[k];
        l_ptr = &p_ptr->lore[k];

        /* Skip non-entries */
        if (!r_ptr->name) continue;

        /* Only print non uniques */
        if (rf_has(r_ptr->flags, RF_UNIQUE)) continue;

        /* Only display "known" races */
        if (!l_ptr->pkills) continue;

        /* Check required kill count */
        if (l_ptr->pkills >= r_ptr->level)
            file_putf(fff, "G[%d] %s: %d (learnt)\n", k, r_ptr->name, l_ptr->pkills);
        else
        {
            file_putf(fff, "w[%d] %s: %d (%d more to go)\n", k, r_ptr->name, l_ptr->pkills,
                r_ptr->level - l_ptr->pkills);
        }

        total++;
    }

    if (!total) file_put(fff, "wNothing so far.\n");

    /* Close the file */
    file_close(fff);

    /* Display the file contents */
    show_file(Ind, file_name, "Killed List", line, 1);

    /* Remove the file */
    file_delete(file_name);
}   


/*
 * Show socials
 */
void do_cmd_check_socials(int Ind, int line)
{
    char file_name[MSG_LEN];
    ang_file *fff;
    int k;
    social_type *s_ptr;

    /* Temporary file */
    fff = file_temp(file_name, sizeof(file_name));
    if (!fff) return;

    /* Scan the socials */
    for (k = 0; k < z_info->soc_max; k++)
    {
        s_ptr = &soc_info[k];

        /* Print the socials */
        file_putf(fff, "w%s\n", s_ptr->name);
    }

    /* Close the file */
    file_close(fff);

    /* Display the file contents */
    show_file(Ind, file_name, "Socials", line, 1);

    /* Remove the file */
    file_delete(file_name);
}


/*
 * Hack -- redraw the screen
 *
 * This command performs various low level updates, clears all the "extra"
 * windows, does a total redraw of the main window, and requests all of the
 * interesting updates and redraws that I can think of.
 */
void do_cmd_redraw(int Ind)
{
    player_type *p_ptr = player_get(Ind);

    /* Not while shopping */
    if (in_store(p_ptr)) return;

    verify_panel(p_ptr);

    /* Combine and Reorder the pack (later) */
    p_ptr->notice |= (PN_COMBINE | PN_REORDER | PN_SORT_QUIVER);

    /* Update torch */
    p_ptr->update |= (PU_TORCH);

    /* Update stuff */
    p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);

    /* Fully update the visuals */
    p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);

    /* Redraw */
    p_ptr->redraw |= (PR_BASIC | PR_EXTRA | PR_MAP | PR_FLOOR | PR_INVEN |
        PR_EQUIP | PR_SPELL | PR_MONSTER | PR_MONLIST | PR_ITEMLIST);
}


void do_cmd_interactive(int Ind, int type, u32b query)
{
    player_type *p_ptr = player_get(Ind);

    /* Hack -- Use special term */
    Send_term_info(p_ptr, NTERM_ACTIVATE, NTERM_WIN_SPECIAL);

    /* Let the player scroll through this info */
    p_ptr->special_file_type = type;

    /* Perform action */
    switch (type)
    {
        /* Help file */
        case SPECIAL_FILE_HELP:
            common_file_peruse(Ind, query);
            do_cmd_check_other(Ind, p_ptr->interactive_line - p_ptr->interactive_next);
            break;
    }

    /* Hack -- Return to main term */
    Send_term_info(p_ptr, NTERM_ACTIVATE, NTERM_WIN_OVERHEAD);
}
