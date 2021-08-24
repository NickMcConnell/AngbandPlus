/* File: wizard2.c */

/*
 * Copyright (c) 1997 Ben Harrison, and others
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies. Other copyrights may also apply.
 */

/* Purpose: Wizard commands */

#include "angband.h"
#include "dun_gen.h"
#include <assert.h>

/* Statistics: Use the wizard commands '-' and '=' to gather statistics.
   The Wizard command '"' and 'A' will then show all found artifacts,
   including rand-arts. The character sheet will show statistics on object
   distributions and key resources. The object info commands '~' for egos 'e'
   and objects 'o' are also useful. Be sure to begin each statistics run
   with a fresh, newly created character.*/
bool statistics_hack = FALSE;
static vec_ptr _rand_arts = NULL;
static vec_ptr _egos = NULL;

vec_ptr stats_rand_arts(void)
{
    if (!_rand_arts)
        _rand_arts = vec_alloc(free);
    return _rand_arts;
}

vec_ptr stats_egos(void)
{
    if (!_egos)
        _egos = vec_alloc(free);
    return _egos;
}

void stats_add_rand_art(object_type *o_ptr)
{
    if (o_ptr->art_name)
    {
        object_type *copy = malloc(sizeof(object_type));
        *copy = *o_ptr;
        obj_identify_fully(copy);
        vec_add(stats_rand_arts(), copy);
    }
}

void stats_add_ego(object_type *o_ptr)
{
    if (o_ptr->name2)
    {
        object_type *copy = malloc(sizeof(object_type));
        *copy = *o_ptr;
        obj_identify_fully(copy);
        vec_add(stats_egos(), copy);
    }
}

typedef struct {
    int total;
    int count;
} _tally_t;
static _tally_t _monster_levels[MAX_DEPTH];
static _tally_t _object_levels[MAX_DEPTH];
static int      _object_histogram[MAX_DEPTH];

static void _stats_reset_monster_levels(void)
{
    int i;
    for (i = 0; i < MAX_DEPTH; i++)
    {
        _monster_levels[i].total = 0;
        _monster_levels[i].count = 0;
    }
    #if 1
    /* This is useful for testing custom dun_type_s.mon_alloc_f's */
    for (i = 1; i < max_r_idx; i++)
    {
        r_info[i].r_pkills = 0;
        r_info[i].r_akills = 0;
    }
    #endif
}

static void _stats_note_monster_level(int dlvl, int mlvl)
{
    if (0 <= dlvl && dlvl < MAX_DEPTH)
    {
        _monster_levels[dlvl].total += mlvl;
        _monster_levels[dlvl].count++;
    }
}

static void _stats_reset_object_levels(void)
{
    int i;
    for (i = 0; i < MAX_DEPTH; i++)
    {
        _object_levels[i].total = 0;
        _object_levels[i].count = 0;

        _object_histogram[i] = 0;
    }
}

static void _stats_note_object_level(int dlvl, int olvl)
{
    if (0 <= dlvl && dlvl < MAX_DEPTH)
    {
        _object_levels[dlvl].total += olvl;
        _object_levels[dlvl].count++;
    }

    if (0 <= olvl && olvl < MAX_DEPTH)
        _object_histogram[olvl]++;
}


/*
 * Strip an "object name" into a buffer
 */
void strip_name_aux(char *dest, const char *src)
{
    char *t;

    /* Skip past leading characters */
    while (*src == ' ' || *src == '&' || *src == '[')
        src++;

    /* Copy useful chars */
    for (t = dest; *src; src++)
    {
        if (*src != '~' && *src != ']')
            *t++ = *src;
    }

    *t = '\0';
}

void strip_name(char *buf, int k_idx)
{
    strip_name_aux(buf, k_name + k_info[k_idx].name);
}

int _life_rating_aux(int lvl)
{
    return (p_ptr->player_hp[lvl-1]-100) * 100 / (50*(lvl-1));
}

int life_rating(void)
{
    return _life_rating_aux(PY_MAX_LEVEL);
}

void do_cmd_rerate_aux(void)
{
    for(;;)
    {
        int i, pct;
        p_ptr->player_hp[0] = 100;

        for (i = 1; i < PY_MAX_LEVEL; i++)
            p_ptr->player_hp[i] = p_ptr->player_hp[i - 1] + randint1(100);

        /* These extra early checks give a slight boost to average life ratings (~102%) */
        pct = _life_rating_aux(5);
        if (pct < 87) continue;

        pct = _life_rating_aux(10);
        if (pct < 87) continue;

        pct = _life_rating_aux(25);
        if (pct < 87) continue;

        pct = life_rating();
        if (87 <= pct && pct <= 117) break;
    }
}

void do_cmd_rerate(bool display)
{
    do_cmd_rerate_aux();

    p_ptr->update |= (PU_HP);
    p_ptr->redraw |= (PR_HP);
    handle_stuff();

    if (display)
    {
        msg_format("Your life rate is %d/100 now.", life_rating());
        p_ptr->knowledge |= KNOW_HPRATE;
    }
    else
    {
        msg_print("Life rate is changed.");
        p_ptr->knowledge &= ~(KNOW_HPRATE);
    }
}


#ifdef ALLOW_WIZARD

/*
 * Dimension Door
 */
static bool wiz_dimension_door(void)
{
    int    x = 0, y = 0;

    if (!tgt_pt(&x, &y, -1)) return FALSE;

    teleport_player_to(y, x, TELEPORT_NONMAGICAL);

    return (TRUE);
}


/*
 * Create the artifact of the specified number -- DAN
 *
 */
static void wiz_create_named_art(int a_idx)
{
    if (create_named_art(a_idx, p_ptr->pos))
        a_info[a_idx].generated = TRUE;
}

#ifdef MONSTER_HORDES

/* Summon a horde of monsters */
static void do_cmd_summon_horde(void)
{
    point_t pos;
    int attempts = 1000;

    while (--attempts)
    {
        pos = scatter(p_ptr->pos, 3);
        if (cave_empty_at(pos)) break;
    }

    alloc_horde(pos, cave->difficulty);
}

#endif /* MONSTER_HORDES */

/*
 * Hack -- Teleport to the target
 */
static void do_cmd_wiz_bamf(void)
{
    /* Must have a target */
    if (!target_who) return;

    /* Teleport to the target */
    teleport_player_to(target_row, target_col, TELEPORT_NONMAGICAL);
}


/*
 * Aux function for "do_cmd_wiz_change()".   -RAK-
 */
static void do_cmd_wiz_change_aux(void)
{
    int i, j;
    int tmp_int;
    long tmp_long;
    s16b tmp_s16b;
    char tmp_val[160];
    char ppp[80];


    /* Query the stats */
    for (i = 0; i < 6; i++)
    {
        /* Prompt */
        sprintf(ppp, "%s (3-%d): ", stat_names[i], p_ptr->stat_max_max[i]);

        /* Default */
        sprintf(tmp_val, "%d", p_ptr->stat_max[i]);

        /* Query */
        if (!get_string(ppp, tmp_val, 3)) return;

        /* Extract */
        tmp_int = atoi(tmp_val);

        /* Verify */
        if (tmp_int > p_ptr->stat_max_max[i]) tmp_int = p_ptr->stat_max_max[i];
        else if (tmp_int < 3) tmp_int = 3;

        /* Save it */
        p_ptr->stat_cur[i] = p_ptr->stat_max[i] = tmp_int;
    }


    /* Default */
    sprintf(tmp_val, "%d", WEAPON_EXP_MASTER);

    /* Query */
    if (!get_string("Proficiency: ", tmp_val, 9)) return;

    /* Extract */
    tmp_s16b = atoi(tmp_val);

    /* Verify */
    if (tmp_s16b < WEAPON_EXP_UNSKILLED) tmp_s16b = WEAPON_EXP_UNSKILLED;
    if (tmp_s16b > WEAPON_EXP_MASTER) tmp_s16b = WEAPON_EXP_MASTER;

    for (j = 0; j <= TV_WEAPON_END - TV_WEAPON_BEGIN; j++)
    {
        for (i = 0;i < 64;i++)
        {
            int max = skills_weapon_max(TV_WEAPON_BEGIN + j, i);
            p_ptr->weapon_exp[j][i] = tmp_s16b;
            if (p_ptr->weapon_exp[j][i] > max) p_ptr->weapon_exp[j][i] = max;
        }
    }

    for (j = 0; j < 10; j++)
    {
        p_ptr->skill_exp[j] = tmp_s16b;
        if (p_ptr->skill_exp[j] > s_info[p_ptr->pclass].s_max[j]) p_ptr->skill_exp[j] = s_info[p_ptr->pclass].s_max[j];
    }

    /* Hack for WARLOCK_DRAGONS. Of course, reading skill tables directly is forbidden, so this code is inherently wrong! */
    p_ptr->skill_exp[SKILL_RIDING] = MIN(skills_riding_max(), tmp_s16b);

    for (j = 0; j < 32; j++)
        p_ptr->spell_exp[j] = (tmp_s16b > SPELL_EXP_MASTER ? SPELL_EXP_MASTER : tmp_s16b);
    for (; j < 64; j++)
        p_ptr->spell_exp[j] = (tmp_s16b > SPELL_EXP_EXPERT ? SPELL_EXP_EXPERT : tmp_s16b);

    /* Default */
    sprintf(tmp_val, "%d", p_ptr->au);

    /* Query */
    if (!get_string("Gold: ", tmp_val, 9)) return;

    /* Extract */
    tmp_long = atol(tmp_val);

    /* Verify */
    if (tmp_long < 0) tmp_long = 0L;

    /* Save */
    p_ptr->au = tmp_long;


    /* Default */
    sprintf(tmp_val, "%d", p_ptr->max_exp);

    /* Query */
    if (!get_string("Experience: ", tmp_val, 9)) return;

    /* Extract */
    tmp_long = atol(tmp_val);

    /* Verify */
    if (tmp_long < 0) tmp_long = 0L;

    if (p_ptr->prace != RACE_ANDROID)
    {
        /* Save */
        p_ptr->max_exp = tmp_long;
        p_ptr->exp = tmp_long;

        /* Update */
        check_experience();
    }

    sprintf(tmp_val, "%d", p_ptr->fame);
    if (!get_string("Fame: ", tmp_val, 3)) return;
    tmp_long = atol(tmp_val);
    if (tmp_long < 0) tmp_long = 0L;
    p_ptr->fame = (s16b)tmp_long;
}


/*
 * Change various "permanent" player variables.
 */
static void do_cmd_wiz_change(void)
{
    /* Interact */
    do_cmd_wiz_change_aux();

    /* Redraw everything */
    do_cmd_redraw();
}

/*
 * A structure to hold a tval and its description
 */
typedef struct tval_desc
{
    int        tval;
    cptr       desc;
} tval_desc;

/*
 * A list of tvals and their textual names
 */
static tval_desc tvals[] =
{
    { TV_SWORD,             "Sword"                },
    { TV_POLEARM,           "Polearm"              },
    { TV_HAFTED,            "Hafted Weapon"        },
    { TV_BOW,               "Bow"                  },
    { TV_ARROW,             "Arrows"               },
    { TV_BOLT,              "Bolts"                },
    { TV_SHOT,              "Shots"                },
    { TV_SHIELD,            "Shield"               },
    { TV_CROWN,             "Crown"                },
    { TV_HELM,              "Helm"                 },
    { TV_GLOVES,            "Gloves"               },
    { TV_BOOTS,             "Boots"                },
    { TV_CLOAK,             "Cloak"                },
    { TV_DRAG_ARMOR,        "Dragon Scale Mail"    },
    { TV_HARD_ARMOR,        "Hard Armor"           },
    { TV_SOFT_ARMOR,        "Soft Armor"           },
    { TV_RING,              "Ring"                 },
    { TV_AMULET,            "Amulet"               },
    { TV_LITE,              "Lite"                 },
    { TV_POTION,            "Potion"               },
    { TV_SCROLL,            "Scroll"               },
    { TV_WAND,              "Wand"                 },
    { TV_STAFF,             "Staff"                },
    { TV_ROD,               "Rod"                  },
    { TV_LIFE_BOOK,         "Life Spellbook"       },
    { TV_SORCERY_BOOK,      "Sorcery Spellbook"    },
    { TV_NATURE_BOOK,       "Nature Spellbook"     },
    { TV_CHAOS_BOOK,        "Chaos Spellbook"      },
    { TV_DEATH_BOOK,        "Death Spellbook"      },
    { TV_TRUMP_BOOK,        "Trump Spellbook"      },
    { TV_ARCANE_BOOK,       "Arcane Spellbook"     },
    { TV_CRAFT_BOOK,        "Craft Spellbook"},
    { TV_DAEMON_BOOK,       "Daemon Spellbook"},
    { TV_CRUSADE_BOOK,      "Crusade Spellbook"},
    { TV_NECROMANCY_BOOK,   "Necromancy Spellbook"},
    { TV_ARMAGEDDON_BOOK,   "Armageddon Spellbook"},
    { TV_MUSIC_BOOK,        "Music Spellbook"      },
    { TV_HISSATSU_BOOK,     "Book of Kendo"        },
    { TV_HEX_BOOK,          "Hex Spellbook"        },
    { TV_RAGE_BOOK,         "Rage Spellbook"       },
    { TV_BURGLARY_BOOK,     "Thieve's Guide"       },
    { TV_PARCHMENT,         "Parchment" },
    { TV_WHISTLE,           "Whistle"    },
    { TV_SPIKE,             "Spikes"               },
    { TV_DIGGING,           "Digger"               },
    { TV_CHEST,             "Chest"                },
    { TV_CAPTURE,           "Capture Ball"         },
    { TV_CARD,              "Express Card"         },
    { TV_FIGURINE,          "Magical Figurine"     },
    { TV_STATUE,            "Statue"               },
    { TV_CORPSE,            "Corpse"               },
    { TV_FOOD,              "Food"                 },
    { TV_FLASK,             "Flask"                },
    { TV_JUNK,              "Junk"                 },
    { TV_SKELETON,          "Skeleton"             },
    { TV_QUIVER,            "Quiver"               },
    { 0,                    NULL                   }
};



/*
 * Specify tval and sval (type and subtype of object) originally
 * by RAK, heavily modified by -Bernd-
 *
 * This function returns the k_idx of an object type, or zero if failed
 *
 * List up to 50 choices in three columns
 */
static int wiz_create_itemtype(void)
{
    int i, num, max_num, lvl;
    int col, row;
    int tval;

    cptr tval_desc;
    char ch;

    int choice[120];

    char buf[160];


    /* Clear screen */
    Term_clear();

    /* Print all tval's and their descriptions */
    for (num = 0; (num < 80) && tvals[num].tval; num++)
    {
        row = 2 + (num % 30);
        col = 30 * (num / 30);
        ch = listsym[num];
        prt(format("[%c] %s", ch, tvals[num].desc), row, col);
    }

    /* Me need to know the maximal possible tval_index */
    max_num = num;

    /* Choose! */
    if (!get_com("Get what type of object? ", &ch, FALSE)) return (0);

    /* Analyze choice */
    for (num = 0; num < max_num; num++)
    {
        if (listsym[num] == ch) break;
    }

    /* Bail out if choice is illegal */
    if ((num < 0) || (num >= max_num)) return (0);

    /* Base object type chosen, fill in tval */
    tval = tvals[num].tval;
    tval_desc = tvals[num].desc;


    /*** And now we go for k_idx ***/

    /* Clear screen */
    Term_clear();

    /* We have to search the whole itemlist. */
    num = 0;
    for (lvl = 0; lvl <= 120 && num < 120; lvl++) /* Who cares if this is slow. But order the choices please!! */
    {
        for (i = 1; i < max_k_idx && num < 120; i++)
        {
            object_kind *k_ptr = &k_info[i];

            /* Analyze matching items */
            if (k_ptr->tval == tval && k_ptr->level == lvl)
            {
                /* Prepare it */
                row = 2 + (num % 30);
                col = 30 * (num / 30);
                ch = listsym[num];
                strcpy(buf,"                    ");

                /* Acquire the "name" of object "i" */
                strip_name(buf, i);

                /* Print it */
                if (k_ptr->max_level)
                    prt(format("[%c] %s (L%d-%d)", ch, buf, lvl, k_ptr->max_level), row, col);
                else
                    prt(format("[%c] %s (L%d-*)", ch, buf, lvl), row, col);

                /* Remember the object index */
                choice[num++] = i;
            }
        }
    }

    /* Me need to know the maximal possible remembered object_index */
    max_num = num;

    /* Choose! */
    if (!get_com(format("What Kind of %s? ", tval_desc), &ch, FALSE)) return (0);

    /* Analyze choice */
    for (num = 0; num < max_num; num++)
    {
        if (listsym[num] == ch) break;
    }

    /* Bail out if choice is "illegal" */
    if ((num < 0) || (num >= max_num)) return (0);

    /* And return successful */
    return (choice[num]);
}

/*
 * Wizard routine for creating objects        -RAK-
 * Heavily modified to allow magification and artifactification  -Bernd-
 *
 * Note that wizards cannot create objects on top of other objects.
 *
 * Hack -- this routine always makes a "dungeon object", and applies
 * magic to it, and attempts to decline cursed items.
 */
static void wiz_create_item(void)
{
    object_type    forge;
    object_type *q_ptr;
    int n = 1;

    int k_idx;


    /* Save the screen */
    screen_save();

    /* Get object base type */
    k_idx = wiz_create_itemtype();

    /* Restore the screen */
    screen_load();


    /* Return if failed */
    if (!k_idx) return;

    if (k_info[k_idx].gen_flags & OFG_INSTA_ART)
    {
        int i;

        /* Artifactify */
        for (i = 1; i < max_a_idx; i++)
        {
            /* Ignore incorrect tval */
            if (a_info[i].tval != k_info[k_idx].tval) continue;

            /* Ignore incorrect sval */
            if (a_info[i].sval != k_info[k_idx].sval) continue;

            /* Create this artifact */
            if (create_named_art(i, p_ptr->pos))
                a_info[i].generated = TRUE;

            /* All done */
            msg_print("Allocated(INSTA_ART).");

            return;
        }
    }
    else if (k_info[k_idx].tval == TV_CORPSE) /* Possessor Testing! */
    {
        char buf[81];
        buf[0] = 0;
        if (msg_input("Which monster? ", buf, 80))
        {
            n = parse_lookup_monster(buf, 0);
            if (!n) n = atoi(buf);
        }
    }
    else
    {
        switch (k_info[k_idx].tval)
        {
        case TV_WAND: case TV_ROD: case TV_STAFF:
            n = 1;
            break;
        default:
            n = get_quantity("How many? ", 99);
        }
    }

    /* Get local object */
    q_ptr = &forge;

    /* Create the item */
    object_prep(q_ptr, k_idx);

    /* Apply magic */
    apply_magic(q_ptr, cave->dun_lvl, AM_NO_FIXED_ART);
    if (k_info[k_idx].tval == TV_CORPSE)
    {
        if (n) q_ptr->pval = n;
    }
    else
        q_ptr->number = n;

    /* Drop the object from heaven */
    (void)drop_near(q_ptr, p_ptr->pos, -1);

    /* All done */
    msg_print("Allocated.");
}


/*
 * Cure everything instantly
 */
static void do_cmd_wiz_cure_all(void)
{
    /* Restore stats */
    (void)res_stat(A_STR);
    (void)res_stat(A_INT);
    (void)res_stat(A_WIS);
    (void)res_stat(A_CON);
    (void)res_stat(A_DEX);
    (void)res_stat(A_CHR);

    /* Restore the level */
    (void)restore_level();

    /* Heal the player */
    if (p_ptr->chp < p_ptr->mhp)
    {
        p_ptr->chp = p_ptr->mhp;
        p_ptr->chp_frac = 0;

        /* Redraw */
        p_ptr->redraw |= (PR_HP);
    }

    /* Restore mana */
    if (p_ptr->csp < p_ptr->msp)
    {
        p_ptr->csp = p_ptr->msp;
        p_ptr->csp_frac = 0;

        p_ptr->redraw |= (PR_MANA);
        p_ptr->window |= (PW_SPELL);
    }

    /* Cure stuff */
    plr_tim_remove(T_BLIND);
    plr_tim_remove(T_CONFUSED);
    plr_tim_remove(T_POISON);
    fear_clear_p();
    plr_tim_remove(T_PARALYZED);
    plr_tim_remove(T_HALLUCINATE);
    plr_tim_remove(T_STUN);
    plr_tim_remove(T_CUT);
    plr_tim_remove(T_SLOW);

    /* No longer hungry
    (void)set_food(PY_FOOD_MAX - 1);*/
}


/*
 * Go to any level
 */
static void do_cmd_wiz_jump(void)
{
    dun_type_ptr type = dun_types_choose("Jump to Which Dungeon?", TRUE);
    int          dun_lvl;

    if (!type) return;
    if (cave->dun_type_id == type->id)
        dun_lvl = cave->dun_lvl;
    else
        dun_lvl = MAX(type->plr_max_lvl, type->min_dun_lvl);

    if (msg_input_num("Level", &dun_lvl, type->min_dun_lvl, type->max_dun_lvl))
        dun_mgr_wizard_jump(type->id, dun_lvl);
}


/*
 * Become aware of a lot of objects
 */
static void do_cmd_wiz_learn(void)
{
    int i;

    object_type forge;
    object_type *q_ptr;

    /* Scan every object */
    for (i = 1; i < max_k_idx; i++)
    {
        object_kind *k_ptr = &k_info[i];

        /* Induce awareness */
        if (k_ptr->level <= command_arg)
        {
            /* Get local object */
            q_ptr = &forge;

            /* Prepare object */
            object_prep(q_ptr, i);

            /* Awareness */
            object_aware(q_ptr);
        }
    }
}


/*
 * Summon some creatures
 */
static void do_cmd_wiz_summon(int num)
{
    int i;

    for (i = 0; i < num; i++)
    {
        (void)summon_specific(0, p_ptr->pos, cave->dun_lvl, 0, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE));
    }
}


/*
 * Summon a creature of the specified type
 *
 * XXX XXX XXX This function is rather dangerous
 */
static void do_cmd_wiz_named(int r_idx)
{
    int x = p_ptr->pos.x;
    int y = p_ptr->pos.y;

    if (target_who < 0)
    {
        x = target_col;
        y = target_row;
    }

    {
        monster_race *r_ptr = mon_race_lookup(r_idx);
        if (((r_ptr->flags1 & (RF1_UNIQUE)) ||
                (r_ptr->flags7 & (RF7_NAZGUL))) &&
            (r_ptr->cur_num >= r_ptr->max_num))
        {
            r_ptr->cur_num = 0;
            r_ptr->max_num = 1;
        }
    }

    (void)summon_named_creature(0, point_create(x, y), r_idx, (PM_ALLOW_SLEEP | PM_ALLOW_GROUP));
}


/*
 * Summon a creature of the specified type
 *
 * XXX XXX XXX This function is rather dangerous
 */
static void do_cmd_wiz_named_friendly(int r_idx)
{
    (void)summon_named_creature(0, p_ptr->pos, r_idx, (PM_ALLOW_SLEEP | PM_ALLOW_GROUP | PM_FORCE_PET));
}



/*
 * Hack -- Delete all nearby monsters
 */
static void do_cmd_wiz_zap(void)
{
    vec_ptr v = dun_filter_mon(cave, NULL);
    int     i;
    for (i = 0; i < vec_length(v); i++)
    {
        mon_ptr mon = vec_get(v, i);
        bool    fear = FALSE;

        if (mon_is_dead(mon)) continue; /* killed by nearby exploding monster */
        if (mon->id == p_ptr->riding) continue;
        if (mon->cdis > MAX_SIGHT) continue;
        mon_take_hit(mon->id, mon->hp + 1, &fear, NULL);
    }
    vec_free(v);
}


/*
 * Hack -- Delete all monsters
 */
static void do_cmd_wiz_zap_all(void)
{
    vec_ptr v = dun_filter_mon(cave, NULL);
    int     i;
    for (i = 0; i < vec_length(v); i++)
    {
        mon_ptr mon = vec_get(v, i);
        if (mon_is_dead(mon)) continue; /* killed by nearby exploding monster */
        if (mon->id == p_ptr->riding) continue;
        delete_monster(mon);
    }
    vec_free(v);
}


/*
 * Create desired feature
 */
static void do_cmd_wiz_create_feature(void)
{
    static int   prev_feat = 0;
    static int   prev_mimic = 0;
    cave_type    *c_ptr;
    feature_type *f_ptr;
    char         tmp_val[160];
    int          tmp_feat, tmp_mimic;
    int          y, x;

    if (!tgt_pt(&x, &y, -1)) return;

    c_ptr = cave_at_xy(x, y);

    /* Default */
    sprintf(tmp_val, "%d", prev_feat);

    /* Query */
    if (!get_string("Feature: ", tmp_val, 3)) return;

    /* Extract */
    tmp_feat = atoi(tmp_val);
    if (tmp_feat < 0) tmp_feat = 0;
    else if (tmp_feat >= max_f_idx) tmp_feat = max_f_idx - 1;

    /* Default */
    sprintf(tmp_val, "%d", prev_mimic);

    /* Query */
    if (!get_string("Feature (mimic): ", tmp_val, 3)) return;

    /* Extract */
    tmp_mimic = atoi(tmp_val);
    if (tmp_mimic < 0) tmp_mimic = 0;
    else if (tmp_mimic >= max_f_idx) tmp_mimic = max_f_idx - 1;

    cave_set_feat(y, x, tmp_feat);
    c_ptr->mimic = tmp_mimic;

    f_ptr = &f_info[get_feat_mimic(c_ptr)];

    if (have_flag(f_ptr->flags, FF_GLYPH) ||
        have_flag(f_ptr->flags, FF_MON_TRAP))
        c_ptr->info |= (CAVE_OBJECT);
    else if (have_flag(f_ptr->flags, FF_MIRROR))
        c_ptr->info |= (CAVE_GLOW | CAVE_OBJECT);

    /* Notice */
    note_spot(y, x);

    /* Redraw */
    lite_spot(y, x);

    /* Update some things */
    p_ptr->update |= (PU_FLOW);

    prev_feat = tmp_feat;
    prev_mimic = tmp_mimic;
}

/*************************************************************************
 * Wizard Stats
 ************************************************************************/
static doc_ptr _wiz_doc = NULL;
static bool    _wiz_show_scores = TRUE;
static int     _wiz_obj_count = 0;
static int     _wiz_obj_score = 0;

static void _wiz_stats_begin(void)
{
    _wiz_doc = doc_alloc(120);
    doc_insert(_wiz_doc, "<style:table>");
    _wiz_obj_count = 0;
    _wiz_obj_score = 0;
    wiz_obj_stat_reset();
    statistics_hack = TRUE;
}

static void _wiz_stats_end(void)
{
    doc_insert(_wiz_doc, "</style>");
    if (_wiz_obj_count)
    {
        doc_printf(_wiz_doc, "\n\n<color:R>%d</color> objects. <color:R>%d</color> average score.\n",
            _wiz_obj_count, _wiz_obj_score / _wiz_obj_count);
    }
    if (original_score)
        doc_printf(_wiz_doc, "<color:R>%d%%</color> replacement power.\n", replacement_score * 100 / original_score);

    doc_newline(_wiz_doc);
    wiz_obj_stat_calc();
    wiz_obj_stat_report(_wiz_doc, FALSE);
    wiz_obj_stat_reset();

    statistics_hack = FALSE;
}

static void _wiz_stats_free(void)
{
    doc_free(_wiz_doc);
    _wiz_doc = NULL;
}

static void _wiz_stats_display(void)
{
    if (doc_line_count(_wiz_doc))
        doc_display(_wiz_doc, "Statistics", 0);

    viewport_verify();
    do_cmd_redraw();
}

static char _score_color(int score)
{
    if (score < 1000)
        return 'D';
    if (score < 10000)
        return 'w';
    if (score < 20000)
        return 'W';
    if (score < 40000)
        return 'u';
    if (score < 60000)
        return 'y';
    if (score < 80000)
        return 'o';
    if (score < 100000)
        return 'R';
    if (score < 150000)
        return 'r';
    return 'v';
}

#if 0
static void _wiz_stats_log_android(int level, object_type *o_ptr)
{
    int  score = obj_value_real(o_ptr);
    int  exp   = android_obj_exp(o_ptr);
    char name[MAX_NLEN];
    char buf[10];

    if (!_wiz_doc) return;
    if (!exp) return;

    object_desc(name, o_ptr, OD_COLOR_CODED);

    big_num_display(score, buf);
    doc_printf(_wiz_doc, "<color:%c>%s</color> ", _score_color(score), buf);

    big_num_display(exp, buf);
    doc_printf(_wiz_doc, "<color:%c>%s</color>:", _score_color(exp/10), buf);

    doc_printf(_wiz_doc, " <indent><style:indent>%s</style></indent>\n", name);
}
#endif

static void _wiz_stats_log_device(int level, object_type *o_ptr)
{
    char buf[MAX_NLEN];
    if (!_wiz_doc) return;
    object_desc(buf, o_ptr, OD_COLOR_CODED);
    _wiz_obj_count++;
    doc_printf(_wiz_doc, "C%2d D%2d O%2d P%2d D%2d: <indent><style:indent>%s</style></indent>\n",
        p_ptr->lev, level, o_ptr->level, o_ptr->activation.power, o_ptr->activation.difficulty, buf);
}
static void _wiz_stats_log_obj(int level, object_type *o_ptr)
{
    char buf[MAX_NLEN];
    if (!_wiz_doc) return;
    object_desc(buf, o_ptr, OD_COLOR_CODED);
    _wiz_obj_count++;
    if (_wiz_show_scores)
    {
        int  score;
        score = obj_value_real(o_ptr);
        _wiz_obj_score += score;
        doc_printf(_wiz_doc, "C%2d D%2d O%2d <color:%c>%6d</color>: <indent><style:indent>%s</style></indent>\n",
            p_ptr->lev, level, o_ptr->level, _score_color(score), score, buf);
    }
    else
        doc_printf(_wiz_doc, "C%2d D%2d O%2d: <indent><style:indent>%s</style></indent>\n", p_ptr->lev, level, o_ptr->level, buf);
}
static void _wiz_stats_log_speed(int level, object_type *o_ptr)
{
    if (obj_has_flag(o_ptr, OF_SPEED) && !obj_is_art(o_ptr))
        _wiz_stats_log_obj(level, o_ptr);
}
static void _wiz_stats_log_books(int level, object_type *o_ptr, int max3, int max4)
{
    if (obj_is_spellbook(o_ptr) && o_ptr->tval != TV_ARCANE_BOOK)
    {
            if ( (o_ptr->sval == 2 && k_info[o_ptr->k_idx].counts.found < max3)
              || (o_ptr->sval == 3 && k_info[o_ptr->k_idx].counts.found < max4) )
        {
            if (check_book_realm(o_ptr->tval, o_ptr->sval))
                _wiz_stats_log_obj(level, o_ptr);
        }
    }
}
static void _wiz_stats_log_devices(int level, object_type *o_ptr)
{
    #if 0
    if (o_ptr->tval == TV_WAND)
    {
        switch (o_ptr->activation.type)
        {
        case EFFECT_BALL_DISINTEGRATE:
        case EFFECT_BALL_WATER:
        case EFFECT_ROCKET:
            _wiz_stats_log_device(level, o_ptr);
            break;
        }
    }
    #endif
    #if 0
    if (o_ptr->tval == TV_STAFF)
    {
        switch (o_ptr->activation.type)
        {
        case EFFECT_HEAL_CURING:
            _wiz_stats_log_device(level, o_ptr);
            break;
        }
    }
    #endif

    #if 1
    if (obj_is_device(o_ptr))
        _wiz_stats_log_device(level, o_ptr);
    #endif
}
static void _wiz_stats_log_arts(int level, object_type *o_ptr)
{
    if (o_ptr->name1)
        _wiz_stats_log_obj(level, o_ptr);
}
static void _wiz_stats_log_rand_arts(int level, object_type *o_ptr)
{
    if (o_ptr->art_name)
        _wiz_stats_log_obj(level, o_ptr);
}
static bool _wiz_stats_skip(point_t pt)
{
    if (0 && (cave_at_xy(pt.x, pt.y)->info & CAVE_ICKY)) return TRUE;
    return FALSE;
}
static void _wiz_stats_kill(int level)
{
    vec_ptr v = dun_filter_mon(cave, NULL);
    int     i;

    for (i = 0; i < vec_length(v); i++)
    {
        mon_ptr mon = vec_get(v, i);
        mon_race_ptr race = mon_race(mon);
        bool    fear = FALSE;

        if (mon_is_dead(mon)) continue;
        if (mon->id == p_ptr->riding) continue;
        if (mon->r_idx == MON_DAWN) continue; /* inflates pct of humans */
        if (_wiz_stats_skip(mon->pos)) continue;

        race->r_sights++; /* lore */
        _stats_note_monster_level(level, race->level);
        mon_take_hit(mon->id, mon->hp + 1, &fear, NULL); /* drops and experience */
    }
    vec_free(v);
}
static bool _is_stat_potion(obj_ptr obj)
{
    if (obj->tval != TV_POTION) return FALSE;
    switch (obj->sval)
    {
    case SV_POTION_INC_STR:
    case SV_POTION_INC_INT:
    case SV_POTION_INC_WIS:
    case SV_POTION_INC_DEX:
    case SV_POTION_INC_CON:
    case SV_POTION_INC_CHR: return TRUE;
    }
    return FALSE;
}

static bool _wiz_improve_gear_aux(obj_ptr obj, slot_t slot)
{
    obj_ptr old = equip_obj(slot);
    if (old)
    {
        int score, old_score;
        if (obj_is_weapon(old) && !obj_is_weapon(obj)) return FALSE;
        if (obj_is_shield(old) && !obj_is_shield(obj)) return FALSE;
        score = obj_value_real(obj);
        old_score = obj_value_real(old);
        if (score > old_score)
        {
            home_carry(old);
            equip_remove(slot);
            equip_wield(obj, slot);
            return TRUE;
        }
    }
    else
    {
        equip_wield(obj, slot);
        return TRUE;
    }
    return FALSE;
}

static void _wiz_improve_gear(obj_ptr obj)
{
    slot_t slot;

    if (p_ptr->prace == RACE_MON_SWORD || p_ptr->prace == RACE_MON_RING) return;
    /* XXX this is tedious ... */
    if (obj_is_gloves(obj))
    {
        class_t *class_ptr = get_class();
        caster_info *caster = NULL;

        if (class_ptr->hooks.caster_info)
            caster = class_ptr->hooks.caster_info();

        if (caster && caster->options & CASTER_GLOVE_ENCUMBRANCE)
        {
            u32b flags[OF_ARRAY_SIZE];
            obj_flags(obj, flags);
            if ( !have_flag(flags, OF_FREE_ACT)
              && !have_flag(flags, OF_DEX)
              && !have_flag(flags, OF_MAGIC_MASTERY) )
            {
                return;
            }
        }
    }
    if (obj_is_weapon(obj) && skills_weapon_is_icky(obj->tval, obj->sval)) return;

    if (obj->name1 == ART_POWER || obj->name1 == ART_STONEMASK || have_flag(obj->flags, OF_NO_SUMMON)) return;
    if (obj->name2 == EGO_RING_NAZGUL) return;
    if (obj_is_(obj, TV_POLEARM, SV_DEATH_SCYTHE)) return;
    /* hydras have many heads ... */
    for (slot = equip_first_slot(obj); slot; slot = equip_next_slot(obj, slot))
    {
        if (_wiz_improve_gear_aux(obj, slot))
            break;
    }
}

static obj_ptr _pack_obj = NULL;
static bool _improve_pack_p(obj_ptr obj)
{
    assert(_pack_obj);
    return obj->tval == _pack_obj->tval && obj->sval == _pack_obj->sval;
}

static void _wiz_improve_pack(obj_ptr obj)
{
    if (obj->tval == TV_POTION || obj->tval == TV_SCROLL)
    {
        int ct;
        _pack_obj = obj;
        ct = pack_count(_improve_pack_p);
        if (ct + obj->number < 30)
            pack_carry_aux(obj);
        else
            home_carry(obj);
    }
    else if (obj_is_device(obj))
    {
        int slot = pack_find_device(obj->activation.type);

        if (!slot)
            pack_carry_aux(obj);
        else
        {
            obj_ptr old = pack_obj(slot);
            int score, old_score;
            score = obj_value_real(obj);
            old_score = obj_value_real(old);
            if (score > old_score)
            {
                home_carry(old);
                pack_remove(slot);
                pack_carry_aux(obj);
            }
        }
    }
    else if (obj_is_readable_book(obj))
    {
        int ct;
        _pack_obj = obj;
        ct = pack_count(_improve_pack_p);
        if (ct < 2)
            pack_carry_aux(obj);
    }
    else
    {
        pack_carry_aux(obj);
    }
}

static bool _device_is_(obj_ptr obj, int tval, int effect)
{
    return obj->tval == tval
        && obj->activation.type == effect;
}

static void _wiz_stats_inspect_obj(obj_ptr pile)
{
    obj_ptr obj;
    int level = cave->dun_lvl;
    for (obj = pile; obj; obj = obj->next)
    {
        if (obj->tval == TV_GOLD)
        {
            pack_get(obj);
            continue;
        }
        if (obj->marked & OM_COUNTED) continue; /* skip player drops */

        obj_identify_fully(obj);
        stats_on_identify(obj);
        if (obj->level)
            _stats_note_object_level(level, obj->level);

        #if 1
        if (obj->art_name)
            stats_add_rand_art(obj);

        if (obj->name2)
            stats_add_ego(obj);
        #endif

        /* Logging: I simply hand-edit this and recompile as desired */
        if (0 && obj_is_weapon(obj) && (obj->name1 || obj->name3 || obj->art_name))
            _wiz_stats_log_obj(level, obj);

        if (0) _wiz_stats_log_speed(level, obj);
        if (0) _wiz_stats_log_books(level, obj, 20, 20);
        if (0) _wiz_stats_log_devices(level, obj);
        if (0) _wiz_stats_log_arts(level, obj);
        if (0) _wiz_stats_log_rand_arts(level, obj);

        if (1 && !object_is_nameless(obj) && weaponmaster_is_favorite(obj))
            _wiz_stats_log_obj(level, obj);

        if (0 && obj->name2 == EGO_BOOTS_SPEED)
            _wiz_stats_log_obj(level, obj);

        if (0 && obj->name3)
            _wiz_stats_log_obj(level, obj);

        if (0 && obj->name2 && !obj_is_device(obj) && !obj_is_ammo(obj))
            _wiz_stats_log_obj(level, obj);

        if (0 && !object_is_nameless(obj) && obj->tval == TV_BOW)
            _wiz_stats_log_obj(level, obj);

        if (0 && !object_is_nameless(obj) && obj->tval == TV_QUIVER)
            _wiz_stats_log_obj(level, obj);

        if (0 && !object_is_nameless(obj) && obj_is_ammo(obj))
            _wiz_stats_log_obj(level, obj);

        if (0 && obj_is_dragon_armor(obj))
            _wiz_stats_log_obj(level, obj);

        if (0 && _device_is_(obj, TV_WAND, EFFECT_BALL_WATER))
            _wiz_stats_log_obj(level, obj);

        /* Use Resources: Quaff stat potions and improve equipment (mindlessly).
         * This makes it easier for me to poke around a bit after a stat run.
         * Destroying objects is for Death-swords or other race/classes that
         * gain powers that way. */
        if (_is_stat_potion(obj))
            do_device(obj, SPELL_CAST, 0);

        #if 1
        /* XXX Yes ... another stat module. But a better one! */
        if (/*!object_is_nameless(obj) &&*/ obj_is_wearable(obj))
            wiz_obj_stat_add(obj);
        #endif

        /* Use the autopicker to 'improve' this character. For example, you can
         * conditionally have rogues only use weapons less than a certain weight
         * and only shoot slings. */
        if (obj->number)
        {
            int auto_pick_idx = is_autopick(obj);
            if (auto_pick_idx >= 0 && autopick_list[auto_pick_idx].action & DO_AUTOPICK)
            {
                if (obj_is_wearable(obj))
                    _wiz_improve_gear(obj);
                else
                    _wiz_improve_pack(obj);
            }
            else if ( (auto_pick_idx < 0 || !(autopick_list[auto_pick_idx].action & DO_AUTODESTROY))
                   && obj_is_wearable(obj) )
            {
                _wiz_improve_gear(obj);
            }
        }

        if (obj->number)
            plr_hook_destroy_object(obj);
        obj_release(obj, OBJ_RELEASE_QUIET);
    }
}
static void _wiz_stats_inspect(int level)
{
    vec_ptr objects = dun_filter_obj(cave, NULL);
    int     i;
    for (i = 0; i < vec_length(objects); i++)
    {
        obj_ptr obj = vec_get(objects, i);
        _wiz_stats_inspect_obj(obj);
    }
    vec_free(objects);
    pack_overflow();
    home_optimize();
    if (p_ptr->cursed) remove_all_curse();
}
static void _wiz_stats_gather(int which_dungeon, int level, int reps)
{
    int i;
    for (i = 0; i < reps; i++)
    {
        energy_use = 0;
        p_ptr->energy_need = 0;
        dun_mgr_wizard_jump(which_dungeon, level);
        _wiz_stats_kill(level);
        _wiz_stats_inspect(level);
    }
}
static bool _wiz_dun_skip(int id)
{
    switch (id)
    {
    case D_OLYMPUS:
    case D_CAMELOT:
    case D_ICKY_CAVE:
    case D_MOUNTAIN:
    case D_CASTLE:
    case D_MOUNT_DOOM: return TRUE;
    }
    return FALSE;
}
static void _wiz_stats_world(void)
{
    dun_world_ptr world = dun_worlds_current();
    vec_ptr v = world_dun_types();
    int     i, j;
    for (i = 0; i < vec_length(v); i++)
    {
        dun_type_ptr type = vec_get(v, i);
        if (_wiz_dun_skip(type->id)) continue;
        for (j = type->min_dun_lvl; j <= type->max_dun_lvl; j++)
        {
            dun_mgr_wizard_jump(type->id, j);
            _wiz_stats_kill(j);
            _wiz_stats_inspect(j);

            vec_clear(cave->graveyard);
            vec_clear(cave->junkpile);
        }
    }
    vec_free(v);

    if ((world->plr_flags & WFP_COMPLETED) && world->next_world_id)
    {
        dun_worlds_wizard(world->next_world_id);
        _wiz_stats_world();
    }
}
static void _obj_id_full(point_t pos, obj_ptr pile)
{
    obj_ptr obj;
    for (obj = pile; obj; obj = obj->next)
    {
        if (obj->tval == TV_GOLD) continue;
        identify_item(obj);
        obj_identify_fully(obj);
    }
}
static void _obj_id_full_desc(point_t pos, obj_ptr pile)
{
    obj_ptr obj;
    for (obj = pile; obj; obj = obj->next)
    {
        if (obj->tval == TV_GOLD) continue;
        identify_item(obj);
        obj_identify_fully(obj);
        if (obj->name1 || obj->name2)
        {
            char name[MAX_NLEN_OBJ];
            object_desc(name, obj, OD_COLOR_CODED);
            msg_print(name);
        }
    }
}
static void _wiz_glow(point_t pos, cave_ptr grid) { grid->info |= CAVE_GLOW | CAVE_MARK | CAVE_AWARE; }
static void _world_glow(point_t pos, dun_grid_ptr grid) { grid->info |= CAVE_MARK | CAVE_GLOW; }
/*************************************************************************
 * Handle the ^A wizard commands. Perhaps there should be a UI for this?
 ************************************************************************/
extern void do_cmd_debug(void);
void do_cmd_debug(void)
{
    int     n, repeat;
    char    cmd;

    if (REPEAT_PULL(&repeat))
        cmd = repeat;
    else
    {
        get_com("Debug Command: ", &cmd, FALSE);
        REPEAT_PUSH(cmd);
    }

    /* Analyze the command */
    switch (cmd)
    {
    /* Nothing */
    case ESCAPE:
    case ' ':
    case '\n':
    case '\r':
        break;

#ifdef ALLOW_SPOILERS

    /* Hack -- Generate Spoilers */
    case '"':
        do_cmd_spoilers();
        break;

#endif /* ALLOW_SPOILERS */

    /* Hack -- Help */
    case '?':
        do_cmd_help();
        break;

    /* Cure all maladies */
    case 'a':
        do_cmd_wiz_cure_all();
        break;

    /* Know alignment */
    case 'A':
        msg_format("Your alignment is %d.", p_ptr->align);
        break;

    /* Teleport to target */
    case 'b':
        do_cmd_wiz_bamf();
        break;

    /* Create any object */
    case 'c':
        wiz_create_item();
        break;

    /* Create a named artifact */
    case 'C':
    {
        char buf[81];
        buf[0] = 0;
        if (msg_input("Which artifact? ", buf, 80))
        {
            int idx = parse_lookup_artifact(buf, 0);
            if (!idx) idx = atoi(buf);
            wiz_create_named_art(idx);
        }
        break;
    }
    /* Detect everything */
    case 'd':
        detect_all(DETECT_RAD_ALL * 3);
        break;

    /* Dimension_door */
    case 'D':
        wiz_dimension_door();
        break;

    /* Edit character */
    case 'e':
        do_cmd_wiz_change();
        break;

    case 'f': /* debug surface fractals */
        if (cave->dun_type_id == D_SURFACE)
        {
            #if 0
            dun_mgr_ptr dm = dun_mgr();
            dm->world_seed = randint0(0x10000000);
            dun_world_reseed(dm->world_seed);
            dun_regen_surface(cave);
            do_cmd_redraw();
            #endif
            #if 0
            dun_world_dump_frac(cave);
            #endif
        }
        else
        {
            dun_gen_world_wizard();
            /*dun_gen_cave_wizard();*/
        }
        break;

    /* Create desired feature */
    case 'F':
        do_cmd_wiz_create_feature();
        break;

    /* Good Objects */
    case 'g':
#if 1
    {
        object_type forge;
        int num = 10;

        while (num--)
        {
            object_wipe(&forge);
            if (!make_object(&forge, cave->difficulty, AM_GOOD)) continue;
            drop_near(&forge, p_ptr->pos, -1);
        }
    }
#else
        if (command_arg <= 0) command_arg = 10;
        acquirement(p_ptr->pos.y, p_ptr->pos.x, command_arg, FALSE, TRUE);
#endif
        break;

    case 'G':
        if (cave->dun_type_id == D_SURFACE)
            dun_iter_grids(dun_mgr()->world, _world_glow);
        break;

    /* Hitpoint rerating */
    case 'h':
    {
        int i, r;
        int tot = 0, min = 0, max = 0;

        for (i = 0; i < 100; i++)
        {
            do_cmd_rerate_aux();
            r = life_rating();
            tot += r;
            if (!min) min = r;
            else min = MIN(min, r);
            max = MAX(max, r);
        }
        msg_format("Life Ratings: %d%% (%d%%-%d%%)", tot/100, min, max);

    /*    do_cmd_rerate(TRUE); */

        break;
    }
#ifdef MONSTER_HORDES
    case 'H':
        do_cmd_summon_horde();
        break;
#endif /* MONSTER_HORDES */

    /* Identify */
    case 'i':
        (void)identify_fully(NULL);
        break;

    case 'I':
        dun_iter_floor_obj(cave, _obj_id_full_desc);
        break;

    /* Go up or down in the dungeon */
    case 'j':
        do_cmd_wiz_jump();
        break;

    /* Self-Knowledge */
    case 'k':
        self_knowledge();
        break;

    /* Learn about objects */
    case 'l':
        do_cmd_wiz_learn();
        break;

    case 'L': { /* los check */
        point_t uip;
        rect_t  map_rect = ui_map_rect();

        msg_line_clear();
        msg_print("<color:B> Current Line of Sight (<color:R>CAVE_VIEW</color>)</color>. Hit Any Key.");
        for (uip.y = map_rect.y; uip.y < map_rect.y + map_rect.cy; uip.y++)
        {
            for (uip.x = map_rect.x; uip.x < map_rect.x + map_rect.cx; uip.x++)
            {
                point_t cp = ui_pt_to_cave_pt(uip);

                if (msg_line_contains(uip.y, uip.x)) continue;
                if (!dun_pos_valid(cave, cp)) continue;

                if (plr_los(cp))
                {
                    if (use_graphics)
                    {
                        dun_feat_ptr feat = dun_feat_at(cave, cp);
                        Term_queue_bigchar(uip.x, uip.y, feat->d_attr[F_LIT_STANDARD], feat->d_char[F_LIT_STANDARD], 0, 0);
                    }
                    else
                        Term_queue_bigchar(uip.x, uip.y, TERM_WHITE, '*', 0, 0);
                }
            }
        }
        inkey();
        msg_line_clear();
        prt_map();
        break;}

    /* Magic Mapping */
    case 'm':
        map_area(DETECT_RAD_ALL * 3);
        /*(void)detect_monsters_invis(255);
        (void)detect_monsters_normal(255);*/
        break;

    /* Mutation */
    case 'M':
    {
    /*
        for (n = 0; n < 120; n++)
            mut_gain_random(NULL);*/
    /*  mut_gain_choice(mut_demigod_pred);*/
    /*  mut_gain_choice(mut_draconian_pred); */

      n = get_quantity("Which One? ", 500);
        if (n == 500)
        {
            int i;
            for (i = 0; i < 32; ++i)
                mut_gain(i);
        }
        else
            mut_gain(n);
        break;
    }

    /* Summon _friendly_ named monster */
    case 'N':
        do_cmd_wiz_named_friendly(command_arg);
        break;

    /* Summon Named Monster */
    case 'n':
    {
        char buf[81];
        buf[0] = 0;
        if (msg_input("Which monster? ", buf, 80))
        {
            int idx = parse_lookup_monster(buf, 0);
            if (!idx) idx = atoi(buf);
            do_cmd_wiz_named(idx);
        }
        break;
    }

    /* Object playing routines */
    case 'o':
        wiz_obj_smith();
        break;

    case 'O':
        if (target_who > 0)
        {
            mon_ptr mon = dun_mon(cave, target_who);
            doc_ptr doc = doc_alloc(120);
            int i, j;

            wiz_obj_stat_reset();
            statistics_hack = TRUE;
            for (i = 0; i < 1000; i++)
            {
                vec_ptr v = mon_drop_make(mon);
                for (j = 0; j < vec_length(v); j++)
                {
                    obj_ptr obj = vec_get(v, j);
                    if (obj->tval == TV_GOLD) continue;
                    obj_identify_fully(obj);
                    wiz_obj_stat_add(obj);
                }
                vec_free(v);
            }
            wiz_obj_stat_calc();
            wiz_obj_stat_report(doc, TRUE);
            statistics_hack = FALSE;

            doc_display(doc, "Drops", 0);
            doc_free(doc);
            do_cmd_redraw();
        }
        break;
    /* Phase Door */
    case 'p':
        teleport_player(10, 0L);
        break;

    /* Wizard Probe */
    case 'P':
        if (target_who > 0)
        {
            mon_ptr mon = dun_mon(cave, target_who);
            doc_ptr doc = doc_alloc(80);
            mon_spell_wizard(mon, NULL, doc);
            doc_display(doc, "Spells", 0);
            doc_free(doc);
            do_cmd_redraw();
        }
        break;
    case 'q':
    {
        quests_wizard();
        break;
    }
    case 'R': {
        doc_ptr doc = doc_alloc(120);
        obj_t   forge = {0};
        int     i, j, k_idx = lookup_kind(TV_SWORD, SV_EXECUTIONERS_SWORD);

        wiz_obj_stat_reset();
        statistics_hack = TRUE;
        for (i = 1; i < 100;)
        {
            for (j = 0; j < 100; j++)
            {
                object_prep(&forge, k_idx);
                apply_magic_ego = EGO_WEAPON_MORGUL;
                apply_magic(&forge, 100, AM_NO_FIXED_ART | AM_GOOD | AM_FORCE_EGO);
                if (!forge.art_name)
                    art_create_ego(&forge, 100, 0);
                if (obj_value_real(&forge) > 60000) break;
            }
            obj_identify_fully(&forge);
            wiz_obj_stat_add(&forge);
            i++;
        }
        wiz_obj_stat_calc();
        wiz_obj_stat_report(doc, TRUE);
        statistics_hack = FALSE;

        doc_display(doc, "Objects", 0);

        doc_free(doc);
        wiz_obj_stat_reset();
        do_cmd_redraw();
        break; }
    /* Summon Random Monster(s) */
    case 's':
        if (command_arg <= 0) command_arg = 1;
        do_cmd_wiz_summon(command_arg);
        break;

    case 'S':
#ifdef ALLOW_SPOILERS
        generate_spoilers();
#endif
        break;

    /* Teleport */
    case 't':
        dimension_door(255);
        /*teleport_player(100, 0L);*/
        break;
    /* Make every dungeon square "known" to test streamers -KMW- */
    case 'u':
        dun_iter_grids(cave, _wiz_glow);
        wiz_lite();
        if (0) detect_treasure(255);
        if (1) dun_iter_floor_obj(cave, _obj_id_full);
        break;


    /* Very Good Objects */
    case 'v':
        if (command_arg <= 0) command_arg = 1;
        acquirement(p_ptr->pos.y, p_ptr->pos.x, command_arg, TRUE, TRUE);
        break;

    case 'V':
        msg_print("You receive an equalization ritual.");
        virtue_init();
        p_ptr->update |= PU_BONUS;
        break;

    /* Wizard Light the Level */
    case 'w':
        wiz_lite();
        break;

    /* Increase Experience */
    case 'x':
        gain_exp(command_arg ? command_arg : (p_ptr->exp + 1));
        break;

    /* Zap Monsters (Genocide) */
    case 'z':
        do_cmd_wiz_zap();
        break;

    /* Zap Monsters (Omnicide) */
    case 'Z':
        do_cmd_wiz_zap_all();
        break;

    case '-':
    {
        /* Generate Statistics on object/monster distributions. Create a new
           character, run this command, then create a character dump
           or browse the object knowledge command (~2). Wizard commands "A and "O
           are also useful.*/

        _wiz_stats_begin();
        _stats_reset_monster_levels();
        _stats_reset_object_levels();
        _wiz_stats_world();
#if 0
        {
            _tally_t mon_total_tally = {0};
            _tally_t obj_total_tally = {0};
            int      obj_total = 0;
            int      obj_running = 0;
            int      last_lev = 0, lev;

            for (lev = 0; lev < MAX_DEPTH; lev++)
                obj_total += _object_histogram[lev];

            doc_newline(_wiz_doc);
            doc_insert(_wiz_doc, "<color:G>Depth   Monster Level    Object Level   Object Counts</color>\n");
            for (lev = 0; lev < MAX_DEPTH; lev++)
            {
                _tally_t mon_tally = _monster_levels[lev];
                _tally_t obj_tally = _object_levels[lev];
                int      j, obj_ct = 0;

                if (!mon_tally.count || !obj_tally.count) continue;

                mon_total_tally.total += mon_tally.total;
                mon_total_tally.count += mon_tally.count;

                obj_total_tally.total += obj_tally.total;
                obj_total_tally.count += obj_tally.count;

                for (j = last_lev + 1; j <= lev; j++)
                {
                    obj_ct += _object_histogram[j];
                }
                last_lev = lev;
                obj_running += obj_ct;

                doc_printf(_wiz_doc, "%5d   %3d.%02d (%4d)    %3d.%02d (%4d)        %3d.%02d%%  %3d.%02d%%\n",
                    lev,
                    mon_tally.total / mon_tally.count,
                    (mon_tally.total*100 / mon_tally.count) % 100,
                    mon_tally.count,
                    obj_tally.total / obj_tally.count,
                    (obj_tally.total*100 / obj_tally.count) % 100,
                    obj_tally.count,
                    obj_ct*100 / obj_total,
                    (obj_ct*10000 / obj_total) % 100,
                    obj_running*100 / obj_total,
                    (obj_running*10000 / obj_total) % 100
                );
            }

            if (mon_total_tally.count && obj_total_tally.count)
            {
                doc_printf(_wiz_doc, "<color:R>        %3d.%02d (%5d)   %3d.%02d (%5d)</color>\n",
                    mon_total_tally.total / mon_total_tally.count,
                    (mon_total_tally.total*100 / mon_total_tally.count) % 100,
                    mon_total_tally.count,
                    obj_total_tally.total / obj_total_tally.count,
                    (obj_total_tally.total*100 / obj_total_tally.count) % 100,
                    obj_total_tally.count
                );
            }
            doc_newline(_wiz_doc);
        }
#endif
        _wiz_stats_end();
        _wiz_stats_display();
        _wiz_stats_free();

        break;
    }
    case '=':
    {
        /* In this version, we gather statistics on the current level of the
           current dungeon. You still want to start with a fresh character. */
        int reps = get_quantity("How many reps? ", 100);

        _wiz_stats_begin();
        _stats_reset_monster_levels(); /* XXX */
        _wiz_stats_gather(cave->dun_type_id, cave->dun_lvl, reps);
        _wiz_stats_end();
        _wiz_stats_display();
        _wiz_stats_free();

        break;
    }
    case '_': {
        #if 1
        int id = get_quantity("Which? ", 100);
        dun_worlds_wizard(id);
        #else
        int curses = 1 + randint1(3);
        bool stop_ty = FALSE;
        int count = 0;
        do
        {
            stop_ty = activate_ty_curse(stop_ty, &count);
        }
        while (--curses);
        #endif
        break; }
    default:
        msg_print("That is not a valid debug command.");
        break;
    }
}

#else

#ifdef MACINTOSH
static int i = 0;
#endif

#endif


