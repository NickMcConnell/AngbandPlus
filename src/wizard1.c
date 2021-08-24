/* File: wizard1.c */

/*
 * Copyright (c) 1997 Ben Harrison, and others
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies. Other copyrights may also apply.
 */

/* Purpose: Spoiler generation -BEN-
 * This file has been mostly rewritten. If something is broke,
 * don't blame -BEN- :) */

#include "angband.h"
#include <math.h>

#ifdef ALLOW_SPOILERS


/*
 * A tval grouper
 */
typedef struct
{
    byte tval;
    cptr name;
} grouper;

/************************************************************************
 * Object and Artifact Tables
 * Note: The Object Tables are designed to be used with the wizard
 *       commands for gathering statistics. Create a new character,
 *       gather statistics and then display the Object tables. Egos
 *       and rand-arts are forgotten when you restart the game.
 ************************************************************************/
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

    { TV_LIGHT,              "Light-Sources" },
    { TV_AMULET,            "Amulets" },
    { TV_RING,              "Rings" },

    { 0, NULL }
};

static int _art_tval;
static bool _art_filter(int id, art_ptr art)
{
    if (_art_tval && art->tval != _art_tval) return FALSE;
    return TRUE;
}

static void spoil_artifact_desc(void)
{
    int i,j;
    doc_ptr doc = doc_alloc(80);

    spoiler_hack = TRUE;
    for (i = 0; group_artifact[i].tval; i++)
    {
        vec_ptr v;
        if (group_artifact[i].name)
        {
            if (i) doc_insert(doc, "</indent>\n");
            doc_printf(doc, "<topic:%s><style:heading>%s</style>\n  <indent>\n",
                group_artifact[i].name, group_artifact[i].name);
        }
        _art_tval = group_artifact[i].tval;
        v = arts_filter_ex(_art_filter);
        for (j = 0; j < vec_length(v); j++)
        {
            art_ptr art = vec_get(v, j);
            obj_t   forge;

            if (!art_create_std(&forge, art, AM_DEBUG | AM_NO_DROP)) continue;

            obj_identify_fully(&forge);
            obj_display_doc(&forge, doc);
            doc_newline(doc);
        }
        vec_free(v);
    }

    doc_display(doc, "Artifact Spoilers", 0);
    doc_free(doc);
    spoiler_hack = FALSE;
}

#define ART_RANDOM -1
#define ART_EGO    -2

typedef struct {
    int  id;
    char name[MAX_NLEN];
    int  score;
    int  k_idx; /* For rand-arts and egos ... */
    int  level;
    int  pct;   /* replacement arts */
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
#define _SPOIL_TRIES     0x08

static void _spoil_table_aux(doc_ptr doc, cptr title, _obj_p pred, int options)
{
    int i;
    vec_ptr entries = vec_alloc(free);
    int     ct_std = 0, ct_rnd = 0, ct_ego = 0;
    int     score_std = 0, score_rnd = 0, score_ego = 0;
    int     max_score_std = 0, max_score_rnd = 0, max_score_ego = 0;

    if ((options & _SPOIL_ARTS) /* FIXED_ART ... && (!random_artifacts || random_artifact_pct < 100)*/)
    {
        vec_ptr arts;
        _art_tval = 0;
        arts = arts_filter_ex(_art_filter);
        for (i = 0; i < vec_length(arts); i++)
        {
            art_ptr        art = vec_get(arts, i);
            object_type    forge = {0};
            _art_info_ptr  entry;

            if (!plr->wizard && (art->gen_flags & OFG_QUESTITEM)) continue;
            if (!art_create_std(&forge, art, AM_DEBUG | AM_NO_DROP)) continue;
            if ((options & _SPOIL_EGOS) && !art->found) continue; /* Hack */
            if (pred && !pred(&forge)) continue;

            obj_identify_fully(&forge);

            entry = malloc(sizeof(_art_info_t));
            entry->id = art->id;
            if (plr->prace == RACE_ANDROID)
            {
                entry->score = android_obj_exp(&forge);
                if (!entry->score)
                    entry->score = obj_value_real(&forge);
            }
            else
                entry->score = obj_value_real(&forge);
            object_desc(entry->name, &forge, OD_COLOR_CODED);
            entry->k_idx = forge.k_idx;
            entry->level = art->level;
            vec_add(entries, entry);

            if (art->found)
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
            if (plr->prace == RACE_ANDROID)
            {
                entry->score = android_obj_exp(o_ptr);
                if (!entry->score)
                    entry->score = obj_value_real(o_ptr);
            }
            else
                entry->score = obj_value_real(o_ptr);
            object_desc(entry->name, o_ptr, OD_COLOR_CODED);
            entry->k_idx = o_ptr->k_idx;
            entry->level = o_ptr->level;
            entry->pct = 0;
            if (o_ptr->replacement_art_id)
            {
                obj_t forge = {0};
                if (art_create_std(&forge, arts_lookup(o_ptr->replacement_art_id), AM_NO_DROP))
                {
                    int base_score;
                    if (obj_is_weapon_ammo(&forge))
                    {
                        forge.to_h = MAX(10, forge.to_h);
                        forge.to_d = MAX(10, forge.to_d);
                    }
                    if (obj_is_armor(&forge))
                    {
                        forge.to_a = MAX(10, forge.to_a);
                    }
                    base_score = obj_value_real(&forge);
                    entry->pct = entry->score * 100 / base_score;
                }
            }
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
            if (plr->prace == RACE_ANDROID)
            {
                entry->score = android_obj_exp(o_ptr);
                if (!entry->score)
                    entry->score = obj_value_real(o_ptr);
            }
            else
                entry->score = obj_value_real(o_ptr);
            object_desc(entry->name, o_ptr, OD_COLOR_CODED);
            entry->k_idx = o_ptr->k_idx;
            entry->level = o_ptr->level;
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
        doc_insert(doc, "<style:wide>     <color:G>  Score Lvl Rty Cts");
        if (options & _SPOIL_TRIES)
            doc_insert(doc, " Try");
        doc_insert(doc, " Object Description</color>\n");
        for (i = 0; i < vec_length(entries); i++)
        {
            _art_info_ptr  entry = vec_get(entries, i);

            if (entry->id == ART_RANDOM)
            {
                if (entry->pct)
                    doc_printf(doc, "<color:v>%3d) %7d</color> %3d %3d %3d ", i+1, entry->score, entry->level, entry->pct, k_info[entry->k_idx].counts.found);
                else
                    doc_printf(doc, "<color:v>%3d) %7d</color> %3d     %3d ", i+1, entry->score, entry->level, k_info[entry->k_idx].counts.found);
                if (options & _SPOIL_TRIES)
                    doc_insert(doc, "    ");
                doc_printf(doc, "<indent><style:indent>%s</style></indent>\n", entry->name);
            }
            else if (entry->id == ART_EGO)
            {
                doc_printf(doc, "<color:B>%3d) %7d</color> %3d     %3d ", i+1, entry->score, entry->level, k_info[entry->k_idx].counts.found);
                if (options & _SPOIL_TRIES)
                    doc_insert(doc, "    ");
                doc_printf(doc, "<indent><style:indent>%s</style></indent>\n", entry->name);
            }
            else
            {
                art_ptr art = arts_lookup(entry->id);

                doc_printf(doc, "<color:%c>%3d) %7d</color> %3d %3d ",
                    (art->found) ? 'y' : 'w',
                    i+1, entry->score, art->level, art->rarity);

                if (art->gen_flags & OFG_INSTA_ART)
                    doc_insert(doc, "    ");
                else
                    doc_printf(doc, "%3d ", k_info[entry->k_idx].counts.found);
                if (options & _SPOIL_TRIES)
                    doc_printf(doc, "%3d ", art->tries);
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
    _spoil_table_aux(doc, title, pred, _SPOIL_ARTS | _SPOIL_RAND_ARTS | _SPOIL_TRIES);
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
    _spoil_artifact_table_aux(doc, "Weapons", obj_is_weapon);
    _spoil_artifact_table_aux(doc, "Shields", obj_is_shield);
    _spoil_artifact_table_aux(doc, "Bows", obj_is_bow);
    _spoil_artifact_table_aux(doc, "Rings", obj_is_ring);
    _spoil_artifact_table_aux(doc, "Amulets", obj_is_amulet);
    _spoil_artifact_table_aux(doc, "Lights", obj_is_light);
    _spoil_artifact_table_aux(doc, "Body Armor", obj_is_body_armor);
    _spoil_artifact_table_aux(doc, "Cloaks", obj_is_cloak);
    _spoil_artifact_table_aux(doc, "Helmets", obj_is_helmet);
    _spoil_artifact_table_aux(doc, "Gloves", obj_is_gloves);
    _spoil_artifact_table_aux(doc, "Boots", obj_is_boots);
    spoiler_hack = FALSE;

    doc_display(doc, "Artifact Tables", 0);
    doc_free(doc);
}

static void spoil_object_tables(void)
{
    doc_ptr doc = doc_alloc(MIN(100, _term_width()));

    spoiler_hack = TRUE;
    _spoil_object_table_aux(doc, "All Objects", NULL);
    _spoil_object_table_aux(doc, "Weapons", obj_is_weapon);
    _spoil_object_table_aux(doc, "Shields", obj_is_shield);
    _spoil_object_table_aux(doc, "Bows", obj_is_bow);
    _spoil_object_table_aux(doc, "Rings", obj_is_ring);
    _spoil_object_table_aux(doc, "Amulets", obj_is_amulet);
    _spoil_object_table_aux(doc, "Lights", obj_is_light);
    _spoil_object_table_aux(doc, "Body Armor", obj_is_body_armor);
    _spoil_object_table_aux(doc, "Cloaks", obj_is_cloak);
    _spoil_object_table_aux(doc, "Helmets", obj_is_helmet);
    _spoil_object_table_aux(doc, "Gloves", obj_is_gloves);
    _spoil_object_table_aux(doc, "Boots", obj_is_boots);
    _spoil_object_table_aux(doc, "Ammo", obj_is_ammo);
    _spoil_object_table_aux(doc, "Quivers", obj_is_quiver);
    spoiler_hack = FALSE;

    doc_display(doc, "Object Tables", 0);
    doc_free(doc);
}

/************************************************************************
 * Monster Tables
 ************************************************************************/
static bool _is_nonunique(mon_race_ptr r) { return !mon_race_is_unique(r); }

static int _compare_r_level(mon_race_ptr l, mon_race_ptr r)
{
    if (l->alloc.lvl < r->alloc.lvl) return -1;
    if (l->alloc.lvl > r->alloc.lvl) return 1;
    if (l->mexp < r->mexp) return -1;
    if (l->mexp > r->mexp) return 1;
    if (l->id < r->id) return -1;
    if (l->id > r->id) return 1;
    return 0;
}
static int _compare_r_level_desc(mon_race_ptr l, mon_race_ptr r)
{
    return -_compare_r_level(l, r);
}

static vec_ptr _mon_table(mon_race_p p)
{
    vec_ptr v = vec_alloc(NULL);
    int     i;

    for (i = 0; i < vec_length(mon_alloc_tbl); i++)
    {
        mon_race_ptr r = vec_get(mon_alloc_tbl, i);
        if (p && !p(r)) continue;
        vec_add(v, r);
    }

    vec_sort(v, (vec_cmp_f)_compare_r_level);
    return v;
}

static void _spoil_mon_table(doc_ptr doc, cptr heading, mon_race_p p)
{
    vec_ptr v = _mon_table(p);
    int     i;

    doc_printf(doc, "<topic:%s><color:G>%-38.38s Lvl Rar Spd     HP    AC Display</color>\n", heading, heading);
    for (i = 0; i < vec_length(v); i++)
    {
        mon_race_ptr r = vec_get(v, i);
        term_char_t  ac = mon_race_visual_ascii(r);
        term_char_t  gc = mon_race_visual(r);

        if (mon_race_is_unique(r))
            doc_printf(doc, "<color:%c>%-38.38s</color> ", p == mon_race_is_unique ? 'w' : 'v', r->name);
        else
            doc_printf(doc, "The %-34.34s ", r->name);

        doc_printf(doc, "%3d %3d %+3d", r->alloc.lvl, r->alloc.rarity, r->move.speed);
        doc_printf(doc, "%7d ", dice_avg_roll(r->hp));
        doc_printf(doc, "%5d ", r->ac);
        doc_printf(doc, "  <color:%c>%c</color>", attr_to_attr_char(ac.a), ac.c);
        if (use_graphics && (gc.c != ac.c || gc.a != ac.a))
        {
            doc_insert(doc, " / ");
            doc_insert_term_char(doc, gc);
        }
        doc_newline(doc);
    }
    doc_newline(doc);

    vec_free(v);
}

static void spoil_mon_desc(void)
{
    doc_ptr doc = doc_alloc(80);

    doc_change_name(doc, "mon-desc.html");
    doc_printf(doc, "<color:heading>Monster Tables for %s Version %d.%d.%d</color>\n\n",
                     VERSION_NAME, VER_MAJOR, VER_MINOR, VER_PATCH);
    doc_insert(doc, "<style:table>");

    _spoil_mon_table(doc, "All Monsters", NULL);
    _spoil_mon_table(doc, "Uniques", mon_race_is_unique);
    _spoil_mon_table(doc, "Non-uniques", _is_nonunique);

    doc_insert(doc, "</style>");
    doc_display(doc, "Monster Tables", 0);
    doc_free(doc);
}

/******************************************************************************
 * Spoilers: Monster max damage by resistance type
 * The goal here is to not list all possible damage types. Rather, the goal
 * is to give the player an idea of when certain resistances are desirable
 * as well as how to play with resistance holes
 ******************************************************************************/
static void _display_res(doc_ptr doc, int res)
{
    int pct = res_pct_known(res);
    char color = 'D';
    if (pct == 100)
        color = 'v';
    else if (pct < 0)
        color = 'D';
    else if (res_is_low(res))
    {
        if (pct >= 72)
            color =  'r';
        else if (pct >= 65)
            color =  'R';
        else if (pct >= 50)
            color =  'y';
    }
    else
    {
        if (pct >= 45)
            color =  'r';
        else if (pct >= 40)
            color =  'R';
        else if (pct >= 30)
            color =  'y';
    }
    doc_printf(doc, " <color:%c>%3d</color>", color, pct);
}
static void _display_dam(doc_ptr doc, int dam)
{
    int ratio = dam * 100 / MAX(1, plr->chp);
    char color;
    if (ratio > 100) color = 'v';
    else if (ratio > 75) color = 'r';
    else if (ratio > 50) color = 'R';
    else if (ratio > 35) color = 'o';
    else if (ratio > 20) color = 'y';
    else if (ratio > 10) color = 'U';
    else if (dam == 0) color = 'D';
    else color = 'w';
    doc_printf(doc, " <color:%c>%3d</color>", color, dam);
}

#if 0
static char _melee_dam_color(int dam)
{
    int ratio = dam * 100 / plr->chp;
    if (ratio > 40) return 'v';
    else if (ratio > 25) return 'r';
    else if (ratio > 15) return 'R';
    else if (ratio > 10) return 'o';
    else if (ratio >  5) return 'y';
    else if (ratio >  3) return 'U';
    else if (dam == 0) return 'D';
    return 'w';
}
#endif

static void _display_speed(doc_ptr doc, int speed)
{
    char color;
    if (speed >= 30) color = 'v';
    else if (speed >= 20) color = 'r';
    else if (speed >= 10) color = 'R';
    else if (speed >=  5) color = 'y';
    else color = 'w';
    doc_printf(doc, " <color:%c>%5d</color>", color, speed);
}

static int _avg_dam_roll(int dd, int ds) { return dd * (ds + 1) / 2; }
static int _mon_hp(mon_race_ptr r)
{
    return dice_avg_roll(r->hp);
}

typedef struct {
    int dam[GF_RES_COUNT];
    int unresist;
    int total;
    int count;
} _spell_dam_info_t, *_spell_dam_info_ptr;

static _spell_dam_info_ptr _spell_dam_info_alloc(void)
{
    _spell_dam_info_ptr info = malloc(sizeof(_spell_dam_info_t));
    memset(info, 0, sizeof(_spell_dam_info_t));
    return info;
}
static void _add_spell_dam(_spell_dam_info_ptr info, int which, int amt)
{
    if (which == GF_NONE)
    {
        if (info->unresist < amt)
            info->unresist = amt;
    }
    else
    {
        if (info->dam[which] < amt)
            info->dam[which] = amt;
    }
    info->total += amt;
    info->count++;
}

static void _add_bolt_dam(_spell_dam_info_ptr info, int which, int amt)
{
    if (plr->reflect)
        _add_spell_dam(info, which, amt/7);
    else
        _add_spell_dam(info, which, amt);
}

static void _add_curse_dam(_spell_dam_info_ptr info, mon_race_ptr race, int amt)
{
    int roll = 100 + race->alloc.lvl/2;
    int sav = plr->skills.sav;
    int success = sav * 100 / roll;
    int fail = 100 - success;
    int dam = amt * fail / 100;
    if (mon_race_is_(race, "p.Kenshirou"))
        dam = amt; /* his curses never fail */
    _add_spell_dam(info, GF_NONE, dam);
}
static bool _is_gf_spell(mon_spell_ptr spell)
{
    switch (spell->id.type)
    {
    case MST_BREATH: case MST_BALL: case MST_BOLT: case MST_BEAM:
        return TRUE;
    }
    return FALSE;
}
static int _spell_res(mon_spell_ptr spell)
{
    if (_is_gf_spell(spell))
        return gf_resist(spell->id.effect);
    return GF_NONE;
}
static _spell_dam_info_ptr _calc_spell_dam_info(mon_race_ptr r)
{
    int                 i;
    _spell_dam_info_ptr info = _spell_dam_info_alloc();
    mon_spells_ptr      spells = r->spells;
    mon_spell_group_ptr group;

    if (!spells) return info;
    group = spells->groups[MST_BREATH];
    if (group)
    {
        for (i = 0; i < group->count; i++)
        {
            mon_spell_ptr spell = &group->spells[i];
            int           dam = mon_spell_avg_dam(spell, r, TRUE);
            _add_spell_dam(info, _spell_res(spell), dam);
        }
    }
    group = spells->groups[MST_BALL];
    if (group)
    {
        for (i = 0; i < group->count; i++)
        {
            mon_spell_ptr spell = &group->spells[i];
            int           dam = mon_spell_avg_dam(spell, r, TRUE);

            if ( spell->id.effect == GF_MIND_BLAST
              || spell->id.effect == GF_BRAIN_SMASH )
            {
                _add_curse_dam(info, r, dam);
            }
            else
                _add_spell_dam(info, _spell_res(spell), dam);
        }
    }
    group = spells->groups[MST_BOLT];
    if (group)
    {
        for (i = 0; i < group->count; i++)
        {
            mon_spell_ptr spell = &group->spells[i];
            int           dam = mon_spell_avg_dam(spell, r, TRUE);
            _add_bolt_dam(info, _spell_res(spell), dam);
        }
    }
    group = spells->groups[MST_BEAM];
    if (group)
    {
        for (i = 0; i < group->count; i++)
        {
            mon_spell_ptr spell = &group->spells[i];
            int           dam = mon_spell_avg_dam(spell, r, TRUE);
            _add_spell_dam(info, _spell_res(spell), dam);
        }
    }
    group = spells->groups[MST_LOS];
    if (group)
    {
        for (i = 0; i < group->count; i++)
        {
            mon_spell_ptr spell = &group->spells[i];
            if (spell->id.effect != GF_ATTACK)
            {
                int dam = mon_spell_avg_dam(spell, r, TRUE);
                _add_curse_dam(info, r, dam);
            }
        }
    }
    return info;
}

static void _spoil_mon_spell_dam_aux(doc_ptr doc, vec_ptr v)
{
    int i, j;
    for (i = 0; i < vec_length(v); i++)
    {
        mon_race_ptr        r = vec_get(v, i);
        _spell_dam_info_ptr info = _calc_spell_dam_info(r);
        int                 hp = dice_avg_roll(r->hp);
        char                color = 'w';

        if (i%25 == 0)
        {
            doc_printf(doc, "\n<tab:21><color:B>%3d %5d     </color>", plr->lev, plr->chp);
            for (j = GF_ACID; j <= GF_DISEN; j++)
                _display_res(doc, j);
            doc_printf(doc, "\n<color:G>%-20.20s Lvl    HP Freq  Ac  El  Fi  Co  Po  Li  Dk  Cf  Nt  Nx  So  Sh  Ca  Di  Un</color>\n", "Name");
        }

        if (r->attributes & RF_DEPRECATED)
            color = 'D';
        else if (mon_race_is_olympian(r))
            color = 'U';
        else if (mon_race_is_unique(r))
            color = 'B';
        doc_printf(doc, "<color:%c>%-20.20s</color> %3d %5d", color, r->name, r->alloc.lvl, hp);
        if (r->spells)
        {
            char color;
            if (r->spells->freq > 70) color = 'v';
            else if (r->spells->freq >= 50) color = 'r';
            else if (r->spells->freq >= 40) color = 'R';
            else if (r->spells->freq >= 30) color = 'o';
            else if (r->spells->freq >= 20) color = 'y';
            else if (r->spells->freq >= 10) color = 'U';
            else color = 'w';
            doc_printf(doc, " <color:%c>%3d%%</color>", color, r->spells->freq);
        }
        else
            doc_insert(doc, "     ");
        for (j = GF_ACID; j <= GF_DISEN; j++)
            _display_dam(doc, info->dam[j]);
        _display_dam(doc, info->unresist);
        if (info->count)
            _display_dam(doc, info->total / info->count);
        doc_newline(doc);
        free(info);
    }
}
typedef struct {
    int raw;
    int reduced;  /* HURT, SUPERHURT, SHATTER reduce by player's AC */
    int effective;/* account for 'dodge rate' (ie melee accuracy) */
} _melee_dam_t;

typedef struct {
    mon_race_ptr race;
    _spell_dam_info_ptr spells;
    _melee_dam_t melee;
    int          hits;
    int          auras;
    int          retaliation;
    int          nasty1;
    int          nasty2;
} _mon_dam_info_t, *_mon_dam_info_ptr;

static int _gf_resist(int which, int dam)
{
    switch (which)
    {
    case GF_HOLY_FIRE:
        dam = plr_holy_dam(dam);
        break;
    case GF_HELL_FIRE:
        dam = plr_hell_dam(dam);
        break;
    default:
        if (GF_RES_MIN <= which && which <= GF_RES_MAX)
        {
            int pct = res_pct_known(which);
            dam -= dam * pct / 100;
        }
    }
    return dam;
}

static int _calc_py_hits(mon_race_ptr r) /* scaled by 100 */
{
    int i;
    int hits = 0;
    if (plr->prace == RACE_MON_RING) return 0;
    for (i = 0; i < MAX_HANDS; i++)
    {
        switch (plr->attack_info[i].type)
        {
        case PAT_MONK: {
            int blows = NUM_BLOWS(i);
            int chance = hit_chance(0, 0, r->ac);
            hits += blows * chance / 100;
            break; }
        case PAT_WEAPON: {
            int blows = NUM_BLOWS(i);
            obj_ptr obj = equip_obj(plr->attack_info[i].slot);
            int chance = hit_chance(i, obj->to_h, r->ac);
            hits += blows * chance / 100;
            break; }
        }
    }
    #if 0
    for (i = 0; i < plr->innate_attack_ct; i++)
    {
        innate_attack_ptr a = &plr->innate_attacks[i];
        int to_h = plr->to_h_m + a->to_h;
        int chance = XXXhit_chance_innate(to_h, r->ac);
        int blows = a->blows;
        if (i == 0)
            blows += plr->innate_attack_info.xtra_blow;
        hits += blows * chance / 100;
    }
    #endif
    return hits;
}
typedef struct {
    int raw;
    int reduced;
} _blow_stats_t, *_blow_stats_ptr;
static _blow_stats_t _analyze_blow(mon_blow_ptr blow, mon_race_ptr race)
{
    _blow_stats_t stats = {0};
    int ac = plr->ac + plr->to_a;
    int k;

    for (k = 0; k < blow->effect_ct; k++)
    {
        mon_effect_ptr effect = &blow->effects[k];
        int            effect_dam;

        if (!effect->type) continue;
        /* skip non-damaging effects */
        if (effect->type == RBE_CUT) continue;
        if (effect->type == RBE_DRAIN_EXP) continue;
        if (effect->type == GF_FEAR) continue;
        if (effect->type == GF_STUN) continue;
        if (effect->type == GF_PARALYSIS) continue;
        if (effect->type == GF_DRAIN_MANA) continue;
        if (effect->type == GF_UNLIFE) continue;
        /* XXX Delayed damage: if (effect->effect == GF_POIS) continue;*/

        effect_dam = _avg_dam_roll(effect->dice.dd, effect->dice.ds);
        if (k == 0 && mon_blow_allow_crit(blow))
        {
            effect_dam = effect_dam * mon_crit_avg_mul(race, blow) / 100;
        }
        effect_dam += effect->dice.base;
        if (0 && mon_race_has_spell(race, MST_BUFF, 2)) /* XXX T_BERSERK */
            effect_dam = effect_dam * 5 / 4;
        effect_dam = _gf_resist(effect->type, effect_dam);
        if (effect->pct)
            effect_dam = effect_dam * effect->pct / 100;

        /* reduce for player AC */
        stats.raw += effect_dam;
        switch (effect->type)
        {
        case RBE_HURT: case RBE_SHATTER: case RBE_VAMP:
            stats.reduced += effect_dam * ac_melee_pct(ac) / 100;
            break;
        default:
            stats.reduced += effect_dam;
        }
    }
    return stats;
}
static _blow_stats_t _sample_monk(cptr tbl_name, mon_race_ptr race, int count)
{
    _blow_stats_t stats = {0};
    mon_t         mon = {0};
    int           i;

    mon.race = race; /* monk_choose depends on confused and stunned status */
    for (i = 0; i < count; i++)
    {
        mon_blow_ptr blow = monk_choose_attack_mon(tbl_name, &mon);
        _blow_stats_t s = _analyze_blow(blow, race);

        stats.raw += s.raw;
        stats.reduced += s.reduced;
    }
    stats.raw /= count;
    stats.reduced /= count;
    return stats;
}
static bool _skip_aura(int effect)
{
    if (GF_RES_MIN <= effect && effect <= GF_RES_MAX)
    {
        if (plr->resist[effect] > 2)
            return TRUE;
    }
    return FALSE;
}
static _mon_dam_info_ptr _mon_dam_info_alloc(mon_race_ptr r)
{
    int j, blows = 0;
    int ac = plr->ac + plr->to_a;
    int ac2 = 3*ac/4;
    int freq = r->spells ? r->spells->freq : 0;
    _mon_dam_info_ptr info = malloc(sizeof(_mon_dam_info_t));
    memset(info, 0, sizeof(_mon_dam_info_t));

    info->race = r;
    info->spells = _calc_spell_dam_info(r);

    for (j = 0; j < vec_length(r->blows); j++)
    {
        _blow_stats_t stats = {0};
        mon_blow_ptr blow = vec_get(r->blows, j);
        int skill, chance;

        if (freq == 100) break;
        if (blow->method == RBM_EXPLODE) continue;
        if (blow->method == RBM_MONK)
            stats = _sample_monk(blow->name, r, 100);
        else
            stats = _analyze_blow(blow, r);
        skill = mon_race_skill_thn(r);
        skill += blow->power; /* not quite right for _sample_monk, but all powers s/b 0 atm */
        if (0 && mon_race_has_spell(r, MST_BUFF, 2)) /* XXX T_BERSERK */
            skill = skill * 5 / 4;
        if (skill > ac2)
            chance = 50 + 19*(1000 - ac2*1000/skill)/20;
        else
            chance = 50;

        /* now keep totals */
        stats.raw = stats.raw * blow->blows / 100;
        stats.reduced = stats.reduced * blow->blows / 100;

        info->melee.raw += stats.raw;
        info->melee.reduced += stats.reduced;
        info->melee.effective += chance * stats.reduced / 1000;

        blows += blow->blows;
    }
    if (!plr->lightning_reflexes)
    {
        mon_aura_ptr aura;
        if (mon_race_can_retaliate(r) && blows > 0)
            info->retaliation += info->melee.effective * 100/blows; /* dam per strike */
        for (aura = r->auras; aura; aura = aura->next)
        {
            int dam;
            if (_skip_aura(aura->gf)) continue;
            dam = dice_avg_roll(aura->dam);
            dam = _gf_resist(aura->gf, dam);
            if (aura->pct)
                dam = dam * aura->pct / 100;
            info->auras += dam;
        }
    }
    if (info->spells->count)
        info->nasty1 += (info->spells->total / info->spells->count) * freq/100;
    info->nasty1 += info->melee.effective * (100 - freq)/100;

    if (info->spells->count)
        info->nasty2 += (info->spells->total / info->spells->count) * MIN(100, freq + 12)/100;

    if (1)
    {
        int mf = speed_to_energy(r->move.speed);
        int pf = speed_to_energy(plr->pspeed);
        info->nasty1 = info->nasty1 * mf / pf;
        info->nasty2 = info->nasty2 * mf / pf;
    }

    info->hits = _calc_py_hits(r);
    info->nasty1 += info->auras * info->hits / 100;
    if (info->retaliation)
    {
        int chance = r->alloc.lvl * 100 / 150;
        int returns = info->hits * chance / 100;
        /* XXX if (returns > blows) returns = blows; cf plr_on_hit_mon */
        info->nasty1 += info->retaliation * returns/100;
    }
    return info;
}
static void _mon_dam_info_free(_mon_dam_info_ptr info)
{
    free(info->spells);
    free(info);
}

static int _cmp_info1(_mon_dam_info_ptr left, _mon_dam_info_ptr right)
{
    if (left->race->alloc.lvl < right->race->alloc.lvl) return 1;
    if (left->race->alloc.lvl > right->race->alloc.lvl) return -1;
    return 0;
}
static int _cmp_info2(_mon_dam_info_ptr left, _mon_dam_info_ptr right)
{
    #if 1
    if (left->nasty1 < right->nasty1) return 1;
    if (left->nasty1 > right->nasty1) return -1;
    #else
    if (left->melee1.raw < right->melee1.raw) return 1;
    if (left->melee1.raw > right->melee1.raw) return -1;
    #endif
    return 0;
}
static int _cmp_info3(_mon_dam_info_ptr left, _mon_dam_info_ptr right)
{
    if (left->nasty2 < right->nasty2) return 1;
    if (left->nasty2 > right->nasty2) return -1;
    return 0;
}
void mon_spoil_nastiness(mon_race_ptr race, doc_ptr doc)
{
    _mon_dam_info_ptr info = _mon_dam_info_alloc(race);

    doc_insert(doc, "<color:G>Melee Dam:</color>");
    _display_dam(doc, info->melee.raw);
    _display_dam(doc, info->melee.reduced);
    _display_dam(doc, info->melee.effective);
    doc_insert(doc, "\n<color:G>Nastiness:</color>");
    _display_dam(doc, info->nasty1);
    _display_dam(doc, info->nasty2);
    doc_newline(doc);
    doc_newline(doc);

    _mon_dam_info_free(info);
}
static void _spoil_mon_melee_dam_aux_aux(doc_ptr doc, vec_ptr v)
{
    int i;
    for (i = 0; i < vec_length(v); i++)
    {
        _mon_dam_info_ptr info = vec_get(v, i);
        int               hp = _mon_hp(info->race);
        char              color = 'w';
        int               xp, nasty;

        if (i%25 == 0)
            doc_printf(doc, "\n<color:G>%-30.30s Lvl    HP Speed  AC   Exp Dam         Auras Nastiness</color>\n", "Name");

        if (info->race->attributes & RF_DEPRECATED)
            color = 'D';
        else if (mon_race_is_olympian(info->race))
            color = 'U';
        else if (mon_race_is_unique(info->race))
            color = 'B';
        doc_printf(doc, "<color:%c>%-30.30s</color>", color, info->race->name);
        color = (info->race->alloc.flags & RFA_FORCE_DEPTH) ? 'r' : 'w';
        doc_printf(doc, " <color:%c>%3d</color> %5d", color, info->race->alloc.lvl, hp);
        _display_speed(doc, info->race->move.speed);
        if (info->race->ac < 999)
            doc_printf(doc, " %3d", info->race->ac);
        else
            doc_insert(doc, " <color:y>***</color>"); /* metal babble */
        {
            int  plev = spoiler_hack ? 50 : plr->lev;
            char buf[10];

            xp = info->race->mexp * info->race->alloc.lvl / (plev + 2);
            big_num_display(xp, buf);
            doc_printf(doc, " %5.5s", buf);
        }
        _display_dam(doc, info->melee.raw);
        _display_dam(doc, info->melee.reduced);
        _display_dam(doc, info->melee.effective);

        if (info->auras + info->retaliation)
            doc_printf(doc, " %5d", info->auras + info->retaliation);
        else
            doc_insert(doc, "      ");

        _display_dam(doc, info->nasty1);
        _display_dam(doc, info->nasty2);
        /*doc_printf(doc, " %d", info->hits);*/
        nasty = MAX(info->nasty1, info->nasty2);
        if (nasty && hp > 10) /* skip babbles */
            doc_printf(doc, "<tab:91>%9.2f", (double)xp * 1000.0 / (nasty * hp));
        doc_newline(doc);
    }
}

static void _spoil_mon_melee_dam_aux(doc_ptr doc, vec_ptr v)
{
    int i;
    vec_ptr v2 = vec_alloc((vec_free_f)_mon_dam_info_free);

    for (i = 0; i < vec_length(v); i++)
    {
        mon_race_ptr r = vec_get(v, i);
        vec_add(v2, _mon_dam_info_alloc(r));
    }

    vec_sort(v2, (vec_cmp_f)_cmp_info1);
    doc_insert(doc, "<topic:ByLevel><style:heading>Monster Damage by Level</style>\n");
    _spoil_mon_melee_dam_aux_aux(doc, v2);

    vec_sort(v2, (vec_cmp_f)_cmp_info2);
    doc_insert(doc, "\n<topic:ByNasty1><style:heading>Monster Damage by Melee Nastiness</style>\n");
    _spoil_mon_melee_dam_aux_aux(doc, v2);

    vec_sort(v2, (vec_cmp_f)_cmp_info3);
    doc_insert(doc, "\n<topic:ByNasty2><style:heading>Monster Damage by Distance Nastiness</style>\n");
    _spoil_mon_melee_dam_aux_aux(doc, v2);

    vec_free(v2);
}

typedef bool (_blow_p)(mon_blow_ptr blow);
static bool _martial_arts(mon_blow_ptr blow)
{
    return blow->method == RBM_KICK || blow->method == RBM_PUNCH;
}
static bool _paralysis(mon_blow_ptr blow)
{
    int i;
    for (i = 0; i < blow->effect_ct; i++)
    {
        if (blow->effects[i].type == GF_PARALYSIS)
            return TRUE;
    }
    return FALSE;
}

static bool _has_blow(mon_race_ptr r, _blow_p p)
{
    int i;
    for (i = 0; i < vec_length(r->blows); i++)
    {
        mon_blow_ptr blow = vec_get(r->blows, i);
        if (p(blow)) return TRUE;
    }
    return FALSE;
}

static bool _is_monk(mon_race_ptr r)
{
    if (!mon_race_is_char(r, 'p')) return FALSE;
    return _has_blow(r, _martial_arts);
}

static bool _summon_spell_only(mon_race_ptr r)
{
    int i;
    if (!r->spells) return FALSE;
    if (!r->spells->groups[MST_SUMMON]) return FALSE;

    for (i = 0; i < MST_COUNT; i++)
    {
        if (i == MST_SUMMON) continue;
        if (r->spells->groups[i]) return FALSE;
    }
    return TRUE;
}
static bool _ooops(skills_t s)
{
    return s.dis == 0
        && s.dev == 0
        && s.sav == 0
        && s.stl == 0
        && s.srh == 0
        && s.fos == 0
        && s.thn == 0
        && s.thb == 0;
}
static bool _blue_mage(mon_race_ptr r)
{
    if (!mon_race_has_noninnate_spell(r)) return FALSE;
    switch (r->body.class_id)
    {
    case CLASS_MAGE:
    case CLASS_HIGH_MAGE:
    case CLASS_SORCERER:
    case CLASS_PRIEST:
        return TRUE;
    }
    return FALSE;
}
static bool _mon_dam_p(mon_race_ptr r)
{
    return _blue_mage(r);
    return r->spells != NULL;
    return _ooops(r->body.skills);
    return TRUE;
    return mon_is_type(r, SUMMON_ANIMAL_RANGER);
    return mon_race_can_passweb(r);
    return !mon_race_ignore_webs(r);
    return mon_race_immune(r, GF_SLOW);
    return BOOL(r->alloc.flags & RFA_WILD_OCEAN);
    return _has_blow(r, _paralysis);
    return _summon_spell_only(r);
    return r->alloc.lvl <= mimic_max_lvl();
    return _is_monk(r);
}

static void spoil_mon_spell_dam(void)
{
    doc_ptr doc = doc_alloc(120);
    vec_ptr v = _mon_table(_mon_dam_p); 

    doc_change_name(doc, "mon-spells.html");
    doc_insert(doc, "<style:table>");

    _spoil_mon_spell_dam_aux(doc, v);

    doc_insert(doc, "</style>");
    doc_printf(doc, "\n<color:D>Generated for %s Version %d.%d.%d</color>\n\n",
                     VERSION_NAME, VER_MAJOR, VER_MINOR, VER_PATCH);
    doc_display(doc, "Monster Tables", 0);
    doc_free(doc);
    vec_free(v);
}

static void spoil_mon_melee_dam(void)
{
    doc_ptr doc = doc_alloc(120);
    vec_ptr v = _mon_table(_mon_dam_p); 

    doc_change_name(doc, "mon-melee.html");
    doc_insert(doc, "<style:table>");

    _spoil_mon_melee_dam_aux(doc, v);

    doc_insert(doc, "</style>");
    doc_printf(doc, "\n<color:D>Generated for %s Version %d.%d.%d</color>\n\n",
                     VERSION_NAME, VER_MAJOR, VER_MINOR, VER_PATCH);
    doc_display(doc, "Monster Tables", 0);
    doc_free(doc);
    vec_free(v);
}

static void _display_mon_resist(doc_ptr doc, mon_race_ptr race, int gf)
{
    if (mon_race_immune(race, gf))
        doc_insert(doc, " <color:v>*</color>");
    else if (mon_race_vuln(race, gf))
        doc_insert(doc, " <color:y>v</color>");
    else if (mon_race_resist(race, gf))
        doc_insert(doc, " <color:r>+</color>");
    else
        doc_insert(doc, " <color:D>-</color>");
}

static void _spoil_mon_resist_aux(doc_ptr doc, vec_ptr v)
{
    int i;
    for (i = 0; i < vec_length(v); i++)
    {
        mon_race_ptr race = vec_get(v, i);
        int          hp = dice_avg_roll(race->hp);
        char         color = 'w';

        if (i%10 == 0)
        {
            doc_printf(doc, "\n<color:G>%-30.30s Lvl    HP AcElFiCoPo LiDkCfNtNx SoShCaDiTm</color>\n", "Name");
        }

        if (race->attributes & RF_DEPRECATED)
            color = 'D';
        else if (mon_race_is_olympian(race))
            color = 'U';
        else if (race->id > 1132)
            color = 'B';
        doc_printf(doc, "<color:%c>%-30.30s</color> %3d %5d ", color, race->name, race->alloc.lvl, hp);
        _display_mon_resist(doc, race, GF_ACID);
        _display_mon_resist(doc, race, GF_ELEC);
        _display_mon_resist(doc, race, GF_FIRE);
        _display_mon_resist(doc, race, GF_COLD);
        _display_mon_resist(doc, race, GF_POIS);
        doc_insert(doc, " ");
        _display_mon_resist(doc, race, GF_LIGHT);
        _display_mon_resist(doc, race, GF_DARK);
        _display_mon_resist(doc, race, GF_CONFUSION);
        _display_mon_resist(doc, race, GF_NETHER);
        _display_mon_resist(doc, race, GF_NEXUS);
        doc_insert(doc, " ");
        _display_mon_resist(doc, race, GF_SOUND);
        _display_mon_resist(doc, race, GF_SHARDS);
        _display_mon_resist(doc, race, GF_CHAOS);
        _display_mon_resist(doc, race, GF_DISENCHANT);
        _display_mon_resist(doc, race, GF_TIME);
        doc_newline(doc);
    }
}
static void spoil_mon_resist(void)
{
    doc_ptr doc = doc_alloc(120);
    vec_ptr v = _mon_table(_mon_dam_p); 

    doc_change_name(doc, "mon-resist.html");
    doc_insert(doc, "<style:table>");

    _spoil_mon_resist_aux(doc, v);

    doc_insert(doc, "</style>");
    doc_printf(doc, "\n<color:D>Generated for %s Version %d.%d.%d</color>\n\n",
                     VERSION_NAME, VER_MAJOR, VER_MINOR, VER_PATCH);
    doc_display(doc, "Monster Tables", 0);
    doc_free(doc);
    vec_free(v);
}

static void spoil_mon_anger(void)
{
    /* I tried to spreadsheet this, but I must be too dumb to get accurate results! */
    doc_ptr doc = doc_alloc(80);
    doc_ptr cols[2];
    int boosts[] = { 0, 5, 7, 10, 12, 15, -1 };
    int freqs[] = { 10, 15, 20, 25, 30, 33, 35, 40, 50, -1 };
    int doc_idx = 1, boost_idx, freq_idx;
    cols[0] = doc_alloc(36);
    cols[1] = doc_alloc(36);
    for (boost_idx = 0;; boost_idx++)
    {
        int boost = boosts[boost_idx];
        if (boost < 0) break;
        doc_idx = !doc_idx;
        doc_printf(cols[doc_idx], "<color:R>Anger Boost: <color:B>%d</color></color>\n", boost);
        doc_insert(cols[doc_idx], "<color:G>Base Actual Mean  Std Max</color>\n");
        for (freq_idx = 0;; freq_idx++)
        {
            vec_ptr runs;
            int_stat_t stat;
            int i, j, cast = 0, total = 0;
            int freq = freqs[freq_idx];
            if (freq < 0) break;
            runs = vec_alloc(NULL);
            for (i = 0; i < 10000; i++)
            {
                int a = 0;    /* anger */
                for (j = 0; ; j++) /* loop until we cast a spell */
                {
                    total++;
                    if (randint0(100) < freq + a)
                    {
                        vec_add_int(runs, j+1);
                        cast++;
                        break;
                    }
                    /* angering each turn ... there are lots of ways to do
                     * this, but the following vastly decreases stat.max while
                     * only slightly affecting stat.mean and stat.sigma */
                    a += boost + a/2;
                }
            }
            stat = int_calc_stats(runs);
            doc_printf(cols[doc_idx], "%4d  %2d.%d%% %.2f %.2f %3d\n",
                freq, cast*100/total, (cast*1000/total)%10, stat.mean, stat.sigma, stat.max);
            vec_free(runs);
        }
        doc_newline(cols[doc_idx]);
    }
    doc_insert_cols(doc, cols, 2, 0);
    doc_insert(doc, "The stats are on the number of turns it takes to actually cast a spell. "
                    "This is very important for mage-like distance tactics where very long runs "
                    "of no player damage are to be avoided.\n");
    doc_display(doc, "Dynamic Spell Frequencies", 0);
    doc_free(cols[0]);
    doc_free(cols[1]);
    doc_free(doc);
}
typedef bool (*_cast_simulator)(int freq, int mana);
static bool _baseline_caster(int freq, int mana) { return randint0(100) < freq; }
static bool _adaptive_caster1(int freq, int mana) {
    int f = freq;
    f -= MAX(1, freq/5) * mana;
    if (f < 1) f = 1;
    return randint0(100) < f;
}
static bool _adaptive_caster2(int freq, int mana) { return randint0(100) < freq && one_in_(1+mana); }
static void _spoil_mon_spell_freq_aux(doc_ptr doc, int freq, _cast_simulator caster)
{
    vec_ptr runs;
    int_stat_t stat;
    int i, mana = 0, run = 0, total = 0, cast = 0;
    runs = vec_alloc(NULL);
    for (i = 0; i < 100 * 1000; i++)
    {
        total++;
        if (caster(freq, mana))
        {
            cast++;
            run++;
            mana++;
        }
        else
        {
            if (mana) mana--;
            if (run)
            {
                vec_add_int(runs, run);
                run = 0;
            }
        }
    }
    stat = int_calc_stats(runs);
    doc_printf(doc, " %2d.%d%% %.2f %.2f %3d  ",
        cast*100/total, (cast*1000/total)%10, stat.mean, stat.sigma, stat.max);
    vec_free(runs);
}
static void spoil_mon_spell_freq(void)
{
    doc_ptr doc = doc_alloc(80);
    int     freqs[] = { 10, 15, 20, 25, 30, 33, 35, 40, 50, -1 };
    int     freq_idx;

    doc_insert(doc, "<color:R>     ------Baseline------  ------Approach I----  ----Approach II-----</color>\n");
    doc_insert(doc, "<color:G>Freq Actual Mean  Std Max  Actual Mean  Std Max  Actual Mean  Std Max</color>\n");
    for (freq_idx = 0;; freq_idx++)
    {
        int freq = freqs[freq_idx];
        if (freq < 0) break;
        doc_printf(doc, "%4d ", freq);
        _spoil_mon_spell_freq_aux(doc, freq, _baseline_caster);
        doc_insert(doc, "<color:U>");
        _spoil_mon_spell_freq_aux(doc, freq, _adaptive_caster1);
        doc_insert(doc, "</color>");
        _spoil_mon_spell_freq_aux(doc, freq, _adaptive_caster2);
        doc_newline(doc);
    }
    doc_insert(doc, "\n\nThe stats are on the number of casts in a row. This if very important for "
                    "melee characters where long runs of monster spell casting are to be avoided.\n");
    doc_newline(doc);
    doc_display(doc, "Dynamic Spell Frequencies in Melee", 0);
    doc_free(doc);
}

/************************************************************************
 * Devices
 ************************************************************************/
static char _fail_color(int fail)
{
    if (fail < 30) return 'w';
    if (fail < 80) return 'W';
    if (fail < 150) return 'U';
    if (fail < 250) return 'y';
    if (fail < 350) return 'o';
    if (fail < 500) return 'R';
    if (fail < 750) return 'r';
    return 'v';
}
static void spoil_device_fail()
{
    doc_ptr doc = doc_alloc(80);
    int     d, s;

    doc_change_name(doc, "device_fail.html");
    doc_insert(doc, "<style:table>");
    doc_insert(doc, "<topic:d_vs_s><color:r>Difficulty vs Skill</color>\n");
    for (d = 1; d <= 100; d++)
    {
        if (d%25 == 1)
        {
            doc_insert(doc, "\n<color:G>Dev ");
            for (s = 20; s <= 160; s+=10)
                doc_printf(doc, "%4d ", s);
            doc_insert(doc, "</color>\n");
        }
        doc_printf(doc, "<color:B>%3d</color> ", d);
        for (s = 20; s <= 160; s+=10)
        {
            int fail = device_calc_fail_rate_aux(s, d);
            doc_printf(doc, "<color:%c>%2d.%d</color> ", _fail_color(fail), fail/10, fail%10);
        }
        doc_newline(doc);
    }

    doc_insert(doc, "\n<topic:s_vs_d><color:r>Skill vs Difficulty</color>\n");
    for (s = 26; s <= 160; s++)
    {
        if (s%25 == 1)
        {
            doc_insert(doc, "\n<color:G>Skl ");
            for (d = 30; d <= 100; d+=5)
                doc_printf(doc, "%4d ", d);
            doc_insert(doc, "</color>\n");
        }
        doc_printf(doc, "<color:B>%3d</color> ", s);
        for (d = 30; d <= 100; d+=5)
        {
            int fail = device_calc_fail_rate_aux(s, d);
            doc_printf(doc, "<color:%c>%2d.%d</color> ", _fail_color(fail), fail/10, fail%10);
        }
        doc_newline(doc);
    }
    doc_insert(doc, "</style>");
    doc_printf(doc, "\n<color:D>Generated for %s %d.%d.%d</color>\n",
                     VERSION_NAME, VER_MAJOR, VER_MINOR, VER_PATCH);
    doc_display(doc, "Device Faile Rates", 0);
    doc_free(doc);
}
static char _effect_color(int which)
{
    effect_t e = {0};
    int      a;
    e.type = which;
    a = atoi(do_effect(&e, SPELL_COLOR, 0));
    if (a)
        return attr_to_attr_char(a);
    return 'w';
}
static void _display_device_power(doc_ptr doc, effect_t *effect)
{
    cptr s = do_effect(effect, SPELL_INFO, 0);
    int  dd, ds, base, amt = 0;

    if (!s || !strlen(s))
    {
        doc_insert(doc, "    ");
        return;
    }
    if (sscanf(s, "dam %dd%d+%d", &dd, &ds, &base) == 3)
        amt = dd*(ds+1)/2+base;
    else if (sscanf(s, "dam %dd%d", &dd, &ds) == 2)
        amt = dd*(ds+1)/2;
    else if (sscanf(s, "dam %d", &base) == 1)
        amt = base;
    else if (sscanf(s, "heal %dd%d+%d", &dd, &ds, &base) == 3)
        amt = dd*(ds+1)/2+base;
    else if (sscanf(s, "heal %dd%d", &dd, &ds) == 2)
        amt = dd*(ds+1)/2;
    else if (sscanf(s, "heal %d", &base) == 1)
        amt = base;
    else if (sscanf(s, "Power %d", &base) == 1)
        amt = base;
    if (amt)
        doc_printf(doc, " %3d", amt);
    else
        doc_insert(doc, "    ");
}
static void _spoil_device_table_aux(doc_ptr doc, device_effect_info_ptr table, cptr heading)
{
    int lvl, row;
    doc_printf(doc, "<topic:%s>", heading);
    for (row = 0; table->type; table++, row++)
    {
        effect_t e = {0};
        int      max_depth = table->max_depth ? table->max_depth : 100;
        e.type = table->type;
        if (row % 25 == 0)
            doc_printf(doc, "\n<style:heading>%-15.15s</style><color:G> Min Max   5  10  15  20  25  30  35  40  45  50  55  60  65  70  75  80  85  90  95 100</color>\n", heading);
        doc_printf(doc, "<color:%c>", _effect_color(table->type));
        doc_printf(doc, "%-15.15s", do_effect(&e, SPELL_NAME, 0));
        doc_printf(doc, " %3d %3d", table->level, max_depth);
        for (lvl = 5; lvl <= 100; lvl += 5)
        {
            e.power = lvl;
            e.difficulty = e.power;
            if (table->level <= lvl && lvl <= max_depth)
                _display_device_power(doc, &e);
            else
                doc_insert(doc, "    ");
        }
        doc_insert(doc, "</color>\n");
    }
}
static void spoil_device_tables()
{
    doc_ptr doc = doc_alloc(120);

    doc_change_name(doc, "devices.html");
    doc_insert(doc, "<style:table>");

    _spoil_device_table_aux(doc, wand_effect_table, "Wands");
    _spoil_device_table_aux(doc, staff_effect_table, "Staves");
    _spoil_device_table_aux(doc, rod_effect_table, "Rods");

    doc_insert(doc, "</style>");
    doc_printf(doc, "\n<color:D>Generated for %s %d.%d.%d</color>\n",
                     VERSION_NAME, VER_MAJOR, VER_MINOR, VER_PATCH);
    doc_display(doc, "Device Faile Rates", 0);
    doc_free(doc);
}
/************************************************************************
 * Monster Lore
 ************************************************************************/
static void spoil_mon_info(void)
{
    int     i;
    vec_ptr v = vec_alloc(NULL);
    doc_ptr doc = doc_alloc(80);

    spoiler_hack = TRUE;

    /* XXX vec_copy */
    for (i = 0; i < vec_length(mon_alloc_tbl); i++)
    {
        mon_race_ptr r = vec_get(mon_alloc_tbl, i);
        vec_add(v, r);
    }
    vec_sort(v, (vec_cmp_f)_compare_r_level_desc);

    for (i = 0; i < vec_length(v); i++)
    {
        mon_race_ptr r = vec_get(v, i);
        doc_printf(doc, "<topic:%s><color:r>=====================================================================</color>\n", r->name);
        mon_display_doc(r, doc);
        doc_newline(doc);
    }
    vec_free(v);

    doc_display(doc, "Monster Spoilers", 0);
    doc_free(doc);

    spoiler_hack = FALSE;
}
static void spoil_possessor(void)
{
    int     i;
    vec_ptr v = vec_alloc(NULL);

    spoiler_hack = TRUE;

    /* XXX vec_copy */
    for (i = 0; i < vec_length(mon_alloc_tbl); i++)
    {
        mon_race_ptr r = vec_get(mon_alloc_tbl, i);
        vec_add(v, r);
    }
    vec_sort(v, (vec_cmp_f)_compare_r_level_desc);
    possessor_wizard(v);
    vec_free(v);

    spoiler_hack = FALSE;
}

/************************************************************************
 * Monster Evolution
 ************************************************************************/
static vec_ptr _evol_roots(void)
{
    vec_ptr     roots = vec_alloc(NULL);
    int_map_ptr targets = int_map_alloc(NULL);
    int         i;

    /* Find all the targets (monsters that may
     * be evolved into) */
    for (i = 0; i < vec_length(mon_alloc_tbl); i++)
    {
        mon_race_ptr r = vec_get(mon_alloc_tbl, i);
        if (!r->evolution.exp) continue;
        int_map_add(targets, r->evolution.id, NULL);
    }

    /* Now, the roots are simply monsters that
     * may evolve, but aren't themselves targets */
    for (i = 0; i < vec_length(mon_alloc_tbl); i++)
    {
        mon_race_ptr r = vec_get(mon_alloc_tbl, i);
        if (!r->evolution.exp) continue;
        if (int_map_contains(targets, r->id)) continue;
        vec_add(roots, r);
    }
    int_map_free(targets);

    vec_sort(roots, (vec_cmp_f)_compare_r_level);
    return roots;
}

static void _evol_mon_line(doc_ptr doc, monster_race *r_ptr)
{
    term_char_t  ac = mon_race_visual_ascii(r_ptr);
    term_char_t  gc = mon_race_visual(r_ptr);
    doc_printf(doc, "[%s]: <color:B>%s</color> (Level <color:G>%d</color>, ", sym_str(r_ptr->id), r_ptr->name, r_ptr->alloc.lvl);
    doc_printf(doc, "<color:%c>%c</color>", attr_to_attr_char(ac.a), ac.c);
    if (use_graphics && (gc.c != ac.c || gc.a != ac.a))
    {
        doc_insert(doc, "/");
        doc_insert_term_char(doc, gc);
    }
    doc_insert(doc, ")\n");
}

static void spoil_mon_evol(void)
{
    int     i, j;
    vec_ptr roots = _evol_roots();
    doc_ptr doc = doc_alloc(100);

    doc_change_name(doc, "mon-evol.html");
    doc_printf(doc, "<color:heading>Monster Evolution for %s Version %d.%d.%d</color>\n",
                     VERSION_NAME, VER_MAJOR, VER_MINOR, VER_PATCH);
    doc_insert(doc, "<style:table>");

    for (i = 0; i < vec_length(roots); i++)
    {
        monster_race *r_ptr = vec_get(roots, i);

        _evol_mon_line(doc, r_ptr);
        for (j = 1; r_ptr->evolution.exp; j++)
        {
            doc_printf(doc, "%*s<color:y>-<color:R>%d</color>-></color> ", j * 2, "", r_ptr->evolution.exp);
            r_ptr = mon_race_lookup(r_ptr->evolution.id);
            _evol_mon_line(doc, r_ptr);
        }
        doc_newline(doc);
    }
    vec_free(roots);

    doc_insert(doc, "</style>");
    doc_display(doc, "Monster Evolution", 0);
    doc_free(doc);
}

/************************************************************************
 * Skills
 ************************************************************************/
static int _pers_cmp(personality_ptr l, personality_ptr r)
{
    return strcmp(l->name, r->name);
}
static vec_ptr _get_personalities(void)
{
    vec_ptr v = vec_alloc(NULL);
    int     i;

    for (i = 0; i < MAX_PERSONALITIES; i++)
    {
        personality_ptr p = get_personality_aux(i);
        if (p->flags & DEPRECATED) continue;
        vec_add(v, p);
    }
    vec_sort(v, (vec_cmp_f)_pers_cmp);
    return v;
}
static int _race_cmp(race_ptr l, race_ptr r)
{
    return strcmp(l->name, r->name);
}
static vec_ptr _get_races(void)
{
    vec_ptr v = vec_alloc(NULL);
    int     i;

    for (i = 0; i < MAX_RACES; i++)
    {
        race_ptr r = get_race_aux(i, 0);
        if (r->flags & DEPRECATED) continue;
        if (r->flags & RACE_IS_MONSTER) continue;
        vec_add(v, r);
    }
    vec_sort(v, (vec_cmp_f)_race_cmp);
    return v;
}
static void _print_skills1(doc_ptr doc, skills_ptr skills)
{
    doc_printf(doc, " %4d %4d %4d %4d %4d %4d %4d %4d",
        skills->dis, skills->dev, skills->sav, skills->stl,
        skills->srh, skills->fos, skills->thn, skills->thb);
}
static void _print_race_skills(doc_ptr doc, race_ptr race)
{
    if (race->id == RACE_DEMIGOD || race->id == RACE_DRACONIAN)
    {
        char buf[100];
        sprintf(buf, "%s: %s", race->name, race->subname);
        doc_printf(doc, "%-20.20s", buf);
    }
    else
        doc_printf(doc, "%-20.20s", race->name);
    _print_skills1(doc, &race->skills);
    doc_newline(doc);
}
static void spoil_skills()
{
    doc_ptr doc = doc_alloc(120);
    vec_ptr v;
    int     i, j;

    doc_change_name(doc, "skills.html");
    doc_insert(doc, "<style:table>");

    /* Personalities */
    v = _get_personalities();
    doc_insert(doc, "<topic:Personalities><style:heading>Personalities</style>\n");
    doc_printf(doc, "<color:G>%-20.20s  Dis  Dev  Sav  Stl  Srh  Fos  Thn  Thb</color>\n", "Name");
    for (i = 0; i < vec_length(v); i++)
    {
        personality_ptr p = vec_get(v, i);
        doc_printf(doc, "%-20.20s", p->name);
        _print_skills1(doc, &p->skills);
        doc_newline(doc);
    }
    doc_newline(doc);
    vec_free(v);

    /* Races */
    v = _get_races();
    doc_insert(doc, "<topic:Races><style:heading>Races</style>\n");
    doc_printf(doc, "<color:G>%-20.20s  Dis  Dev  Sav  Stl  Srh  Fos  Thn  Thb</color>\n", "Name");
    for (i = 0; i < vec_length(v); i++)
    {
        race_ptr r = vec_get(v, i);
        if (r->id == RACE_DEMIGOD)
        {
            for (j = 0; j < DEMIGOD_MAX; j++)
            {
                r = get_race_aux(RACE_DEMIGOD, j);
                _print_race_skills(doc, r);
            }
        }
        else if (r->id == RACE_DRACONIAN)
        {
            for (j = 0; j < DRACONIAN_MAX; j++)
            {
                r = get_race_aux(RACE_DRACONIAN, j);
                _print_race_skills(doc, r);
            }
        }
        else
            _print_race_skills(doc, r);
    }
    doc_newline(doc);
    vec_free(v);

    doc_insert(doc, "</style>");
    doc_printf(doc, "\n<color:D>Generated for %s %d.%d.%d</color>\n",
                     VERSION_NAME, VER_MAJOR, VER_MINOR, VER_PATCH);
    doc_display(doc, "Skills", 0);
    doc_free(doc);
}

/************************************************************************
 * Magic Realms
 ************************************************************************/
static bool _check_realm(int class_idx, int realm_idx)
{
    int      bit = (1 << (realm_idx-1)); /* cf CH_LIFE and REALM_LIFE (etc) in defines.h */
    class_t *class_ptr = plr_class_aux(class_idx, 0);

    if (class_idx == CLASS_SORCERER || class_idx == CLASS_RED_MAGE || class_idx == CLASS_GRAY_MAGE || class_idx == CLASS_SKILLMASTER)
    {
        if (is_magic(realm_idx) && realm_idx != REALM_NECROMANCY)
            return TRUE;
    }
    if (class_idx == CLASS_SKILLMASTER && realm_idx == REALM_BURGLARY)
        return TRUE;
    if (class_ptr->hooks.caster_info)
    {
        caster_info *caster_ptr = class_ptr->hooks.caster_info();
        if (!caster_ptr) return FALSE; /* skillmaster */
        if (caster_ptr->realm1_choices & bit) return TRUE;
        if (caster_ptr->realm2_choices & bit) return TRUE;
    }
    return FALSE;
}

static void _spoil_spell_book(doc_ptr doc, int class_idx, int realm_idx, int book_idx)
{
    int           spell_idx;
    int           k_idx = lookup_kind(realm2tval(realm_idx), book_idx);
    player_magic *magic_ptr = &m_info[class_idx];

    doc_printf(doc, "<color:o>%-25.25s</color><color:G> Lvl Cst Fail  </color>\n", k_info[k_idx].name);
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
    {
        vec_add_int(vec, i);
    }

    vec_sort(vec, (vec_cmp_f)_cmp_class_name);

    for (i = 0; i < vec_length(vec); i++)
    {
        int           class_idx = vec_get_int(vec, i);
        class_t      *class_ptr = get_class_aux(class_idx, 0);
        bool          class_heading = FALSE;

        if (class_idx == CLASS_RAGE_MAGE) continue; /* broken */
        if (class_idx == CLASS_SAMURAI) continue; /* broken */

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
    doc_printf(doc, "<color:o>%-25.25s</color><color:G> Lvl Cst Fail</color>  <color:R>Lvl Cst Fail</color>\n", k_info[k_idx].name);
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

/************************************************************************
 * Miscellaneous
 ************************************************************************/
static void spoil_option_bits(void)
{
    int     set, bit, seek;
    doc_ptr doc = doc_alloc(80);
    doc_ptr cols[2];

    doc_insert(doc, "Options are stored in the savefile using hard coded "
        "bits. When adding new options, it is hard to locate a free slot. "
        "Perhaps this will help?\n\n");
    cols[0] = doc_alloc(36);
    cols[1] = doc_alloc(36);
    for (set = 0; set < 8; set++) /* 8 is hardcoded ... */
    {
        doc_ptr col = cols[set < 4 ? 0 : 1];
        for (bit = 0; bit < 32; bit++) /* u32b ... */
        {
            doc_printf(col, "<color:R>%d.%2d:</color> ", set, bit);
            for (seek = 0; option_info[seek].o_desc; seek++)
            {
                if ( option_info[seek].o_set == set
                  && option_info[seek].o_bit == bit )
                {
                    doc_printf(col, "%s", option_info[seek].o_text);
                    break;
                }
            }
            doc_newline(col);
        }
        doc_newline(col);
    }
    doc_insert_cols(doc, cols, 2, 0);
    doc_free(cols[0]);
    doc_free(cols[1]);
    doc_display(doc, "Option Bits", 0);
    doc_free(doc);
}

/************************************************************************
 * Public
 ************************************************************************/
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
        prt("(O) Object Tables", row++, col);
        row++;

        c_prt(TERM_RED, "Monster Spoilers", row++, col - 2);
        prt("(m) Brief Descriptions", row++, col);
        prt("(M) Full Descriptions", row++, col);
        prt("(p) Possessor Info", row++, col);
        prt("(e) Evolution", row++, col);
        prt("(d) Damage by Resistance", row++, col);
        prt("(D) Damage by Melee", row++, col);
        prt("(R) Resistance", row++, col);
        prt("(f) Spell Frequency (Anger)", row++, col);
        prt("(F) Spell Frequency (Melee)", row++, col);
        row++;

        c_prt(TERM_RED, "Class Spoilers", row++, col - 2);
        prt("(s) Spells by Class", row++, col);
        prt("(r) Spells by Realm", row++, col);
        prt("(S) Skills", row++, col);
        row++;

        row = 4;
        col = 40;

        c_prt(TERM_RED, "Miscellaneous", row++, col - 2);
        prt("(1) Option Bits", row++, col);
        prt("(2) Device Fail Rates", row++, col);
        prt("(3) Device Tables", row++, col);
        row++;

        /* Prompt */
        prt("ESC) Exit menu", 27, 1);
        prt("Command: ", 26, 0);

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

        /* Monster Spoilers */
        case 'm':
            spoil_mon_desc();
            break;
        case 'M':
            spoil_mon_info();
            break;
        case 'p':
            spoil_possessor();
            break;
        case 'e':
            spoil_mon_evol();
            break;
        case 'd':
            spoil_mon_spell_dam();
            break;
        case 'D':
            spoil_mon_melee_dam();
            break;
        case 'R':
            spoil_mon_resist();
            break;
        case 'f':
            spoil_mon_anger();
            break;
        case 'F':
            spoil_mon_spell_freq();
            break;

        /* Class Spoilers */
        case 's':
            spoil_spells_by_class();
            break;
        case 'r':
            spoil_spells_by_realm();
            break;
        case 'S':
            spoil_skills();
            break;

        /* Miscellaneous */
        case '1':
            spoil_option_bits();
            break;
        case '2':
            spoil_device_fail();
            break;
        case '3':
            spoil_device_tables();
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
