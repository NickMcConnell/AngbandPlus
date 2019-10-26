/* File: spells2.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies. Other copyrights may also apply.
 */

/* Purpose: Spell code (part 2) */

#include "angband.h"
#include <assert.h>

/*
 * self-knowledge... idea from nethack. Useful for determining powers and
 * resistences of items. It saves the screen, clears it, then starts listing
 * attributes, a screenful at a time. (There are a LOT of attributes to
 * list. It will probably take 2 or 3 screens for a powerful character whose
 * using several artifacts...) -CFT
 *
 * It is now a lot more efficient. -BEN-
 *
 * See also "identify_fully()".
 *
 * XXX XXX XXX Use the "show_file()" method, perhaps.
 */
void self_knowledge(void)
{
    doc_ptr doc = doc_alloc(80);
    int i = 0, j;
    u32b flgs[OF_ARRAY_SIZE];


    /* Acquire item flags from equipment */
    for (i = 0; i < OF_ARRAY_SIZE; i++)
        flgs[i] = 0;
    for (i = 1; i <= equip_max(); i++)
    {
        u32b tflgs[OF_ARRAY_SIZE];
        obj_ptr obj = equip_obj(i);
        if (!obj) continue;

        obj_flags(obj, tflgs);
        for (j = 0; j < OF_ARRAY_SIZE; j++)
            flgs[j] |= tflgs[j];
    }

    p_ptr->knowledge |= (KNOW_STAT | KNOW_HPRATE);
    virtue_add(VIRTUE_KNOWLEDGE, 1);
    virtue_add(VIRTUE_ENLIGHTENMENT, 1);

    doc_printf(doc, "Your current Life Rating is <color:G>%d%%</color>.\n\n", life_rating());
    doc_insert(doc, "<color:r>Limits of maximum stats</color>\n");
    for (i = 0; i < MAX_STATS; i++)
    {
        doc_printf(doc, "%s <color:G>18/%d</color>\n", stat_names[i], p_ptr->stat_max_max[i]-18);
    }
    doc_insert(doc, "\n\n");

    virtue_display(doc, TRUE);
    doc_insert(doc, "\n\n");

    doc_insert(doc, "<color:G>Resistances:</color>\n");
    for (i = 0; i < RES_MAX; i++)
    {
        int pct = res_pct(i);
        if (pct != 0)
        {
            if (j == RES_FEAR)
                doc_printf(doc, "You are %s to %s.\n", pct > 0 ? "resistant" : "vulnerable", res_name(i));
            else
                doc_printf(doc, "You are %d%% %s to %s.\n", ABS(pct), pct > 0 ? "resistant" : "vulnerable", res_name(i));
        }
    }
    doc_insert(doc, "\n\n");

    if (p_ptr->afraid || plr_tim_count())
    {
        doc_insert(doc, "<color:G>Timed Effects:</color>\n");
        if (p_ptr->afraid)
            doc_insert(doc, "You are terrified.\n");
        plr_tim_self_knowledge(doc);
        doc_insert(doc, "\n\n");
    }

    if (p_ptr->cursed)
    {
        doc_insert(doc, "<color:G>Curses:</color>\n");
        if (p_ptr->cursed & OFC_TY_CURSE)
            doc_insert(doc, "You carry an ancient foul curse.\n");
        if (p_ptr->cursed & OFC_AGGRAVATE)
            doc_insert(doc, "You aggravate monsters.\n");
        if (p_ptr->cursed & OFC_DRAIN_EXP)
            doc_insert(doc, "You are drained.\n");
        if (p_ptr->cursed & OFC_SLOW_REGEN)
            doc_insert(doc, "You regenerate slowly.\n");
        if (p_ptr->cursed & OFC_ADD_L_CURSE)
            doc_insert(doc, "Your weak curses multiply.\n");
        if (p_ptr->cursed & OFC_ADD_H_CURSE)
            doc_insert(doc, "Your heavy curses multiply.\n");
        if (p_ptr->cursed & OFC_CALL_ANIMAL)
            doc_insert(doc, "You attract animals.\n");
        if (p_ptr->cursed & OFC_CALL_DEMON)
            doc_insert(doc, "You attract demons.\n");
        if (p_ptr->cursed & OFC_CALL_DRAGON)
            doc_insert(doc, "You attract dragons.\n");
        if (p_ptr->cursed & OFC_COWARDICE)
            doc_insert(doc, "You are subject to cowardice.\n");
        if (p_ptr->cursed & OFC_TELEPORT)
            doc_insert(doc, "Your position is very uncertain.\n");
        if (p_ptr->cursed & OFC_LOW_MELEE)
            doc_insert(doc, "Your weapon causes you to miss blows.\n");
        if (p_ptr->cursed & OFC_LOW_AC)
            doc_insert(doc, "You are subject to be hit.\n");
        if (p_ptr->cursed & OFC_LOW_MAGIC)
            doc_insert(doc, "You are subject to fail spellcasting.\n");
        if (p_ptr->cursed & OFC_FAST_DIGEST)
            doc_insert(doc, "You have a good appetite.\n");
        if (p_ptr->cursed & OFC_DRAIN_HP)
            doc_insert(doc, "You are drained.\n");
        if (p_ptr->cursed & OFC_DRAIN_MANA)
            doc_insert(doc, "You brain is drained.\n");
        doc_insert(doc, "\n\n");
    }

    doc_insert(doc, "<color:G>Abilities:</color>\n");
    if (p_ptr->special_attack & ATTACK_CONFUSE)
        doc_insert(doc, "Your hands are glowing dull red.\n");
    if (p_ptr->action == ACTION_SEARCH)
        doc_insert(doc, "You are looking around very carefully.\n");
    if (p_ptr->new_spells)
        doc_insert(doc, "You can learn some spells/prayers.\n");
    if (p_ptr->see_infra)
        doc_insert(doc, "Your eyes are sensitive to infrared light.\n");
    if (p_ptr->see_inv)
        doc_insert(doc, "You can see invisible creatures.\n");
    if (p_ptr->levitation)
        doc_insert(doc, "You can fly.\n");
    if (p_ptr->free_act)
        doc_insert(doc, "You have free action.\n");
    if (p_ptr->regen > 100)
        doc_insert(doc, "You regenerate quickly.\n");
    if (p_ptr->slow_digest)
        doc_insert(doc, "Your appetite is small.\n");
    if (p_ptr->telepathy)
        doc_insert(doc, "You have ESP.\n");
    if (p_ptr->esp_animal)
        doc_insert(doc, "You sense natural creatures.\n");
    if (p_ptr->esp_undead)
        doc_insert(doc, "You sense undead.\n");
    if (p_ptr->esp_demon)
        doc_insert(doc, "You sense demons.\n");
    if (p_ptr->esp_orc)
        doc_insert(doc, "You sense orcs.\n");
    if (p_ptr->esp_troll)
        doc_insert(doc, "You sense trolls.\n");
    if (p_ptr->esp_giant)
        doc_insert(doc, "You sense giants.\n");
    if (p_ptr->esp_dragon)
        doc_insert(doc, "You sense dragons.\n");
    if (p_ptr->esp_human)
        doc_insert(doc, "You sense humans.\n");
    if (p_ptr->esp_evil)
        doc_insert(doc, "You sense evil creatures.\n");
    if (p_ptr->esp_good)
        doc_insert(doc, "You sense good creatures.\n");
    if (p_ptr->esp_nonliving)
        doc_insert(doc, "You sense non-living creatures.\n");
    if (p_ptr->esp_unique)
        doc_insert(doc, "You sense unique monsters.\n");
    if (p_ptr->hold_life)
        doc_insert(doc, "You have a firm hold on your life force.\n");
    if (p_ptr->reflect)
        doc_insert(doc, "You reflect arrows and bolts.\n");
    if (p_ptr->sh_fire)
        doc_insert(doc, "You are surrounded with a fiery aura.\n");
    if (p_ptr->sh_shards)
        doc_insert(doc, "You are surrounded with a shard aura.\n");
    if (p_ptr->sh_elec)
        doc_insert(doc, "You are surrounded with electricity.\n");
    if (p_ptr->sh_cold)
        doc_insert(doc, "You are surrounded with an aura of coldness.\n");
    if (p_ptr->anti_magic)
        doc_insert(doc, "You are surrounded by an anti-magic shell.\n");
    if (p_ptr->anti_tele)
        doc_insert(doc, "You cannot teleport.\n");
    if (p_ptr->lite)
        doc_insert(doc, "You are carrying a permanent light.\n");
    if (p_ptr->warning)
        doc_insert(doc, "You will be warned before dangerous actions.\n");
    if (p_ptr->dec_mana)
        doc_insert(doc, "You can cast spells with fewer mana points.\n");
    if (p_ptr->easy_spell)
        doc_insert(doc, "Fail rate of your magic is decreased.\n");
    if (p_ptr->heavy_spell)
        doc_insert(doc, "Fail rate of your magic is increased.\n");
    if (p_ptr->mighty_throw)
        doc_insert(doc, "You can throw objects powerfully.\n");

    if (p_ptr->sustain_str)
        doc_insert(doc, "Your strength is sustained.\n");
    if (p_ptr->sustain_int)
        doc_insert(doc, "Your intelligence is sustained.\n");
    if (p_ptr->sustain_wis)
        doc_insert(doc, "Your wisdom is sustained.\n");
    if (p_ptr->sustain_con)
        doc_insert(doc, "Your constitution is sustained.\n");
    if (p_ptr->sustain_dex)
        doc_insert(doc, "Your dexterity is sustained.\n");
    if (p_ptr->sustain_chr)
        doc_insert(doc, "Your charisma is sustained.\n");
    if (have_flag(flgs, OF_STR))
        doc_insert(doc, "Your strength is affected by your equipment.\n");
    if (have_flag(flgs, OF_INT))
        doc_insert(doc, "Your intelligence is affected by your equipment.\n");
    if (have_flag(flgs, OF_WIS))
        doc_insert(doc, "Your wisdom is affected by your equipment.\n");
    if (have_flag(flgs, OF_DEX))
        doc_insert(doc, "Your dexterity is affected by your equipment.\n");
    if (have_flag(flgs, OF_CON))
        doc_insert(doc, "Your constitution is affected by your equipment.\n");
    if (have_flag(flgs, OF_CHR))
        doc_insert(doc, "Your charisma is affected by your equipment.\n");
    if (have_flag(flgs, OF_STEALTH))
        doc_insert(doc, "Your stealth is affected by your equipment.\n");
    if (have_flag(flgs, OF_SEARCH))
        doc_insert(doc, "Your searching ability is affected by your equipment.\n");
    if (have_flag(flgs, OF_INFRA))
        doc_insert(doc, "Your infravision is affected by your equipment.\n");
    if (have_flag(flgs, OF_TUNNEL))
        doc_insert(doc, "Your digging ability is affected by your equipment.\n");
    if (have_flag(flgs, OF_SPEED))
        doc_insert(doc, "Your speed is affected by your equipment.\n");
    if (have_flag(flgs, OF_BLOWS))
        doc_insert(doc, "Your attack speed is affected by your equipment.\n");
    if (p_ptr->no_eldritch)
        doc_insert(doc, "You are unaffected by the Eldritch Horror.\n");
    if (p_ptr->no_cut)
        doc_insert(doc, "You cannot be cut.\n");
    if (p_ptr->no_stun)
        doc_insert(doc, "You cannot be stunned.\n");
    if (p_ptr->no_charge_drain)
        doc_insert(doc, "You are immune to charge draining attacks.\n");
    if (p_ptr->auto_id)
        doc_insert(doc, "Objects are automatically identified as you pass over them.\n");
    else if (p_ptr->auto_pseudo_id)
        doc_insert(doc, "Objects are automatically pseudo-identified as you pass over them.\n");
    if (p_ptr->cult_of_personality)
        doc_insert(doc, "Summoned monsters sometimes switch their allegiance.\n");

    /* XXX: We used to spoil your first weapon, and ignore any alternate weapons. Rethink ... */

    screen_save();
    doc_display(doc, "Self Knowledge", 0);
    screen_load();
    doc_free(doc);
}


/*
 * Report all currently active magical effects.
 */
void report_magics(void)
{
    doc_ptr doc = doc_alloc(80);

    if (p_ptr->afraid)
        doc_printf(doc, "<color:R>%-18.18s</color> %3d <indent>%s</indent>\n", "Fear", p_ptr->afraid, "You are afraid.");
    if (p_ptr->special_attack & ATTACK_CONFUSE)
        doc_printf(doc, "<color:R>%-18.18s</color>     <indent>%s</indent>\n", "Confusing Touch", "Your hands are glowing a dull red.");
    plr_tim_weigh_magic(doc);

    screen_save();
    doc_display(doc, "Current Magic", 0);
    screen_load();
    doc_free(doc);
}

/************************************************************************
 * Detection: _detect and _detect_range are shared by feature, object
 * and monster detection. In each case there is a low-level function
 * to perform iteration and high-level functions to give the flavor
 * message.
 ************************************************************************/
static bool _detect;
static int _detect_range;

/* Feature Detection: */
static int _detect_flag;
static bool _detect_known;
static void _detect_feat_grid(point_t pos, cave_ptr grid)
{
    feat_ptr feat;
    int      dist = plr_distance(pos);

    if (dist > _detect_range) return;

    /* Detect Traps marks every grid as detected for display purposes.
     * The player is also warned when leaving a trap detected region */
    if (_detect_flag == FF_TRAP && _detect_known)
    {
        if (dist <= _detect_range - 1)
            grid->info |= CAVE_IN_DETECT;

        grid->info &= ~CAVE_UNSAFE;
        lite_pos(pos);
    }

    feat = dun_grid_feat(grid);
    if (!have_flag(feat->flags, _detect_flag)) return;

    disclose_grid(pos.y, pos.x);
    grid->info |= (CAVE_MARK | CAVE_AWARE);
    lite_pos(pos);
    _detect = TRUE;
}
static bool detect_feat_flag(int range, int flag, bool known)
{
    rect_t r = rect_create_centered(p_ptr->pos, range, range);

    _detect = FALSE;
    _detect_range = range;
    _detect_flag = flag;
    _detect_known = known;

    dun_iter_rect(cave, r, _detect_feat_grid);

    if (flag == FF_TRAP && !view_unsafe_grids)
        p_ptr->redraw |= PR_STATUS;

    return _detect;
}
bool detect_traps(int range, bool known)
{
    bool detect = detect_feat_flag(range, FF_TRAP, known);
    if (music_singing(MUSIC_DETECT) && p_ptr->magic_num1[2] > 0) detect = FALSE;
    if (detect)
        msg_print("You sense the presence of traps!");
    return detect;
}
bool detect_doors(int range)
{
    bool detect = detect_feat_flag(range, FF_DOOR, TRUE);
    if (music_singing(MUSIC_DETECT) && p_ptr->magic_num1[2] > 0) detect = FALSE;
    if (detect)
        msg_print("You sense the presence of doors!");
    return detect;
}
bool detect_stairs(int range)
{
    bool detect = detect_feat_flag(range, FF_STAIRS, TRUE);
    if (music_singing(MUSIC_DETECT) && p_ptr->magic_num1[2] > 0) detect = FALSE;
    if (detect)
        msg_print("You sense the presence of stairs!");
    return detect;
}
bool detect_recall(int range)
{
    bool detect = detect_feat_flag(range, FF_RECALL, TRUE);
    if (music_singing(MUSIC_DETECT) && p_ptr->magic_num1[2] > 0) detect = FALSE;
    if (detect)
        msg_print("You sense the presence of a way back home!");
    return detect;
}
bool detect_treasure(int range)
{
    bool detect = detect_feat_flag(range, FF_HAS_GOLD, TRUE);
    if (music_singing(MUSIC_DETECT) && p_ptr->magic_num1[2] > 6) detect = FALSE;
    if (detect)
        msg_print("You sense the presence of buried treasure!");
    return detect;
}

/* Object Detection: */
static obj_p _detect_obj_filter;
static void _detect_pile(point_t pos, obj_ptr pile)
{
    obj_ptr obj;
    if (point_distance(pos, p_ptr->pos) > _detect_range) return;
    for (obj = pile; obj; obj = obj->next)
    {
        if (_detect_obj_filter && !_detect_obj_filter(obj)) continue;
        obj->marked |= OM_FOUND;
        lite_pos(pos);
        _detect = TRUE;
    }
}
bool detect_obj_aux(obj_p p, int range)
{
    _detect = FALSE;
    _detect_range = range;
    _detect_obj_filter = p;
    dun_iter_floor_obj(cave, _detect_pile);
    return _detect;
}
bool detect_objects_gold(int range)
{
    bool detect = FALSE;
    if (detect_obj_aux(obj_is_gold, range))
    {
        if (music_singing(MUSIC_DETECT) && p_ptr->magic_num1[2] > 6) {}
        else msg_print("You sense the presence of treasure!");
        detect = TRUE;
    }

    if (detect_monsters_string(range, "$"))
        detect = TRUE;

    return detect;
}
bool detect_objects_normal(int range)
{
    bool detect = FALSE;
    if (detect_obj_aux(obj_is_not_gold, range))
    {
        if (music_singing(MUSIC_DETECT) && p_ptr->magic_num1[2] > 6) {}
        else msg_print("You sense the presence of objects!");
        detect = TRUE;
    }
    if (detect_monsters_string(range, "!=?|/`"))
        detect = TRUE;
    return detect;
}
static bool _obj_is_magic(obj_ptr obj)
{
    if (obj_is_art(obj) || obj_is_ego(obj)) return TRUE;
    if (obj_is_jewelry(obj)) return TRUE;
    if (obj_is_spellbook(obj)) return TRUE;
    if (obj_is_device(obj)) return TRUE;
    if (obj->to_a > 0 || obj->to_h + obj->to_d > 0) return TRUE;

    switch (obj->tval)
    {
    case TV_WHISTLE:
    case TV_SCROLL:
    case TV_POTION: return TRUE;
    }
    return FALSE;
}
bool detect_objects_magic(int range)
{
    bool detect = FALSE;
    if (detect_obj_aux(_obj_is_magic, range))
    {
        msg_print("You sense the presence of magic objects!");
        detect = TRUE;
    }
    return detect;
}

/* Monster Detection: */
static mon_p _detect_mon_filter;
static void _detect_mon(int id, mon_ptr mon)
{
    if (point_distance(mon->pos, p_ptr->pos) > _detect_range) return;
    if (_detect_mon_filter && !_detect_mon_filter(mon)) return;
    if (p_ptr->monster_race_idx == mon->r_idx)
        p_ptr->window |= (PW_MONSTER);
    repair_monsters = TRUE;
    mon->mflag2 |= MFLAG2_MARK | MFLAG2_SHOW;
    update_mon(mon, FALSE);
    _detect = TRUE;
}
bool detect_mon_aux(mon_p p, int range)
{
    _detect = FALSE;
    _detect_range = range;
    _detect_mon_filter = p;
    dun_iter_mon(cave, _detect_mon);
    return _detect;
}
static bool _mon_normal_p(mon_ptr mon)
{
    return !(mon_race(mon)->flags2 & RF2_INVISIBLE) || p_ptr->see_inv;
}
bool detect_monsters_normal(int range)
{
    bool detect = FALSE;
    if (detect_mon_aux(_mon_normal_p, range))
    {
        if (music_singing(MUSIC_DETECT) && p_ptr->magic_num1[2] > 6) {}
        else msg_print("You sense the presence of monsters!");
        detect = TRUE;
    }
    return detect;
}
static bool _mon_invis_p(mon_ptr mon)
{
    return BOOL(mon_race(mon)->flags2 & RF2_INVISIBLE);
}
bool detect_monsters_invis(int range)
{
    bool detect = FALSE;
    if (detect_mon_aux(_mon_invis_p, range))
    {
        if (music_singing(MUSIC_DETECT) && p_ptr->magic_num1[2] > 3) {}
        else msg_print("You sense the presence of invisible creatures!");
        detect = TRUE;
    }
    return detect;
}
static bool _mon_evil_p(mon_ptr mon)
{
    mon_race_ptr race = mon_race(mon);
    if (race->flags3 & RF3_EVIL)
    {
        if (is_original_ap(mon))
            mon_lore_aux_3(race, RF3_EVIL);
        return TRUE;
    }
    return FALSE;
}
bool detect_monsters_evil(int range)
{
    bool detect = FALSE;
    if (detect_mon_aux(_mon_evil_p, range))
    {
        msg_print("You sense the presence of evil creatures!");
        detect = TRUE;
    }
    return detect;
}
static bool _mon_nonliving_p(mon_ptr mon)
{
    return !mon_is_living(mon);
}
bool detect_monsters_nonliving(int range)
{
    bool detect = FALSE;
    if (detect_mon_aux(_mon_nonliving_p, range))
    {
        msg_print("You sense the presence of unnatural beings!");
        detect = TRUE;
    }
    return detect;
}
bool detect_monsters_living(int range, cptr msg)
{
    bool detect = FALSE;
    if (detect_mon_aux(mon_is_living, range))
    {
        if (msg) msg_print(msg);
        detect = TRUE;
    }
    return detect;
}
bool detect_monsters_magical(int range)
{
    bool detect = FALSE;
    if (detect_mon_aux(mon_is_magical, range))
    {
        msg_print("You sense magical foes!");
        detect = TRUE;
    }
    return detect;
}
static bool _mon_mind_p(mon_ptr mon)
{
    return !(mon_race(mon)->flags2 & RF2_EMPTY_MIND);
}
bool detect_monsters_mind(int range)
{
    bool detect = FALSE;
    if (detect_mon_aux(_mon_mind_p, range))
    {
        msg_print("You sense the presence of someone's mind!");
        detect = TRUE;
    }
    return detect;
}
static cptr _mon_match_string;
static bool _mon_match_p(mon_ptr mon)
{
    assert(_mon_match_string);
    return strchr(_mon_match_string, mon_race(mon)->d_char) != NULL;
}
bool detect_monsters_string(int range, cptr match)
{
    bool detect = FALSE;
    _mon_match_string = match;
    if (detect_mon_aux(_mon_match_p, range))
    {
        if (music_singing(MUSIC_DETECT) && p_ptr->magic_num1[2] > 3) {}
        else msg_print("You sense the presence of monsters!");
        detect = TRUE;
    }
    return detect;
}

/*
 * Detect everything
 */
bool detect_all(int range)
{
    bool detect = FALSE;

    /* Detect everything */
    if (detect_traps(range, TRUE)) detect = TRUE;
    if (detect_doors(range)) detect = TRUE;
    if (detect_stairs(range)) detect = TRUE;
    if (detect_recall(range)) detect = TRUE;

    /* There are too many hidden treasure. So... */
    /* if (detect_treasure(range)) detect = TRUE; */

    if (detect_objects_gold(range)) detect = TRUE;
    if (detect_objects_normal(range)) detect = TRUE;
    if (detect_monsters_invis(range)) detect = TRUE;
    if (detect_monsters_normal(range)) detect = TRUE;

    /* Result */
    return (detect);
}


/*
 * Apply a "project()" directly to all viewable monsters
 *
 * Note that affected monsters are NOT auto-tracked by this usage.
 *
 * To avoid misbehavior when monster deaths have side-effects,
 * this is done in two passes. -- JDL
 */
static bool _project_mon_p(mon_ptr mon)
{
    if (!plr_los(mon->pos)) return FALSE;
    if (!plr_project_mon(mon)) return FALSE;
    return TRUE;
}
bool project_los(int gf, int dam)
{
    vec_ptr v = dun_filter_mon(cave, _project_mon_p);
    bool    obvious = FALSE;
    int     i;

    for (i = 0; i < vec_length(v); i++)
    {
        mon_ptr mon = vec_get(v, i);
        if (mon_is_dead(mon)) continue; /* killed by an exploding monster already processed */
        if (gf_affect_m(GF_WHO_PLAYER, mon, gf, dam, GF_AFFECT_SPELL)) obvious = TRUE;
    }

    vec_free(v);
    return obvious;
}

/*
 * Speed monsters
 */
bool speed_monsters(void)
{
    return (project_los(GF_OLD_SPEED, p_ptr->lev));
}

/*
 * Slow monsters
 */
bool slow_monsters(int power)
{
    return (project_los(GF_OLD_SLOW, power));
}

/*
 * Sleep monsters
 */
bool sleep_monsters(int power)
{
    return (project_los(GF_OLD_SLEEP, power));
}


/*
 * Banish evil monsters
 */
bool banish_evil(int dist)
{
    return (project_los(GF_AWAY_EVIL, dist));
}


/*
 * Turn undead
 */
bool turn_undead(void)
{
    bool tester = (project_los(GF_TURN_UNDEAD, p_ptr->lev));
    if (tester)
        virtue_add(VIRTUE_UNLIFE, -1);
    return tester;
}


/*
 * Dispel undead monsters
 */
bool dispel_undead(int dam)
{
    bool tester = (project_los(GF_DISP_UNDEAD, dam));
    if (tester)
        virtue_add(VIRTUE_UNLIFE, -2);
    return tester;
}

/*
 * Dispel evil monsters
 */
bool dispel_evil(int dam)
{
    return (project_los(GF_DISP_EVIL, dam));
}

/*
 * Dispel good monsters
 */
bool dispel_good(int dam)
{
    return (project_los(GF_DISP_GOOD, dam));
}

/*
 * Dispel all monsters
 */
bool dispel_monsters(int dam)
{
    return (project_los(GF_DISP_ALL, dam));
}

/*
 * Dispel 'living' monsters
 */
bool dispel_living(int dam)
{
    return (project_los(GF_DISP_LIVING, dam));
}

/*
 * Dispel demons
 */
bool dispel_demons(int dam)
{
    return (project_los(GF_DISP_DEMON, dam));
}


/*
 * Wake up all monsters, and speed up "los" monsters.
 */
void aggravate_monsters(int who)
{
    point_map_iter_ptr iter;
    bool    sleep = FALSE;
    bool    speed = FALSE;

    /* Aggravate everyone nearby */
    for (iter = point_map_iter_alloc(cave->mon_pos);
            point_map_iter_is_valid(iter);
            point_map_iter_next(iter))
    {
        mon_ptr mon = point_map_iter_current(iter);

        /* Skip aggravating monster (or player) */
        if (mon->id == who) continue;

        /* Wake up nearby sleeping monsters */
        if (mon->cdis < MAX_SIGHT * 2)
        {
            if (mon_tim_find(mon, MT_SLEEP))
            {
                mon_tim_remove(mon, MT_SLEEP);
                sleep = TRUE;
            }
            if (!is_pet(mon)) mon->mflag2 |= MFLAG2_NOPET;
        }

        /* Speed up monsters in line of sight */
        if (plr_los(mon->pos))
        {
            if (!is_pet(mon))
            {
                mon_tim_add(mon, T_FAST, 100);
                speed = TRUE;
            }
        }
    }
    point_map_iter_free(iter);

    /* Messages */
    if (speed) msg_print("You feel a sudden stirring nearby!");
    else if (sleep) msg_print("You hear a sudden stirring in the distance!");
    if (p_ptr->riding) p_ptr->update |= PU_BONUS;
}


/*
 * Delete a non-unique/non-quest monster
 */
bool genocide_aux(mon_ptr m_ptr, int power, bool player_cast, int dam_side, cptr spell_name)
{
    monster_race *r_ptr = mon_race(m_ptr);
    bool         resist = FALSE;

    if (is_pet(m_ptr) && !player_cast) return FALSE;

    /* Hack -- Skip Unique Monsters or Quest Monsters */
    if (r_ptr->flags1 & RF1_UNIQUE) resist = TRUE;
    else if (m_ptr->mflag2 & MFLAG2_QUESTOR) resist = TRUE;
    else if (r_ptr->flags7 & RF7_UNIQUE2) resist = TRUE;
    else if (m_ptr->id == p_ptr->riding) resist = TRUE;
    else if (cave->flags & DF_NO_GENOCIDE) resist = TRUE;
    else if (player_cast && (r_ptr->level > randint0(power))) resist = TRUE;
    else if (player_cast && (m_ptr->mflag2 & MFLAG2_NOGENO)) resist = TRUE;
    else if (player_cast && (m_ptr->mflag2 & MFLAG2_VAULT) && !one_in_(3)) resist = TRUE;

    /* Delete the monster */
    else
    {
        delete_monster(m_ptr);
    }

    if (resist && player_cast)
    {
        bool see_m = mon_show_msg(m_ptr);
        char m_name[80];

        monster_desc(m_name, m_ptr, 0);
        if (see_m)
        {
            msg_format("%^s is unaffected.", m_name);
        }
        mon_tim_remove(m_ptr, MT_SLEEP);
        if (is_friendly(m_ptr) && !is_pet(m_ptr))
        {
            if (see_m)
            {
                msg_format("%^s gets angry!", m_name);
            }
            set_hostile(m_ptr);
        }
        if (one_in_(13)) m_ptr->mflag2 |= MFLAG2_NOGENO;
    }

    if (player_cast)
    {
        /* Take damage */
        take_hit(DAMAGE_GENO, randint1(dam_side), format("the strain of casting %^s", spell_name));
    }

    /* Visual feedback */
    move_cursor_relative(p_ptr->pos);

    /* Redraw */
    p_ptr->redraw |= (PR_HP);

    /* Handle */
    handle_stuff();

    /* Fresh */
    Term_fresh();

    /* Delay */
    Term_xtra(TERM_XTRA_DELAY, delay_animation);

    return !resist;
}


/*
 * Delete all non-unique/non-quest monsters of a given "type" from the level
 */
static char _genocide_char;
static bool _mon_genocide(mon_ptr mon) { return mon_race(mon)->d_char == _genocide_char; }
bool symbol_genocide(int power, bool player_cast)
{
    vec_ptr v;
    int  i;
    bool do_virtue = FALSE;

    /* Prevent genocide in quest levels */
    if (cave->flags & DF_NO_GENOCIDE)
        return TRUE; /* But charge the player for the (stupid) action! */

    /* Mega-Hack -- Get a monster symbol */
    if (!get_com("Choose a monster race (by symbol) to genocide: ", &_genocide_char, FALSE))
        return FALSE;

    /* Delete the monsters of that "type" */
    v = dun_filter_mon(cave, _mon_genocide);
    for (i = 0; i < vec_length(v); i++)
    {
        mon_ptr mon = vec_get(v, i);
        if (genocide_aux(mon, power, player_cast, 4, "Genocide"))
            do_virtue = TRUE;
    }

    if (do_virtue)
    {
        virtue_add(VIRTUE_VITALITY, -2);
        virtue_add(VIRTUE_CHANCE, -1);
    }

    return TRUE;
}


/*
 * Delete all nearby (non-unique) monsters
 */
static bool _mass_genocide_p(mon_ptr mon) { return mon->cdis <= MAX_SIGHT; }
bool mass_genocide(int power, bool player_cast)
{
    vec_ptr v;
    int     i;
    bool    result = FALSE;

    /* Prevent mass genocide in quest levels */
    if (cave->flags & DF_NO_GENOCIDE) return FALSE;

    /* Delete the (nearby) monsters */
    v = dun_filter_mon(cave, _mass_genocide_p);
    for (i = 0; i < vec_length(v); i++)
    {
        mon_ptr mon = vec_get(v, i);
        result |= genocide_aux(mon, power, player_cast, 3, "Mass Genocide");
    }
    vec_free(v);

    if (result)
    {
        virtue_add(VIRTUE_VITALITY, -2);
        virtue_add(VIRTUE_CHANCE, -1);
    }

    return result;
}



/*
 * Delete all nearby (non-unique) undead
 */
static bool _mass_genocide_undead_p(mon_ptr mon) { return mon_is_undead(mon) && mon->cdis <= MAX_SIGHT; }
bool mass_genocide_undead(int power, bool player_cast)
{
    vec_ptr v;
    int     i;
    bool    result = FALSE;

    /* Prevent mass genocide in quest levels */
    if (cave->flags & DF_NO_GENOCIDE) return FALSE;

    /* Delete the (nearby) monsters */
    v = dun_filter_mon(cave, _mass_genocide_undead_p);
    for (i = 0; i < vec_length(v); i++)
    {
        mon_ptr mon = vec_get(v, i);
        result |= genocide_aux(mon, power, player_cast, 3, "Annihilate Undead");
    }
    vec_free(v);

    if (result)
    {
        virtue_add(VIRTUE_UNLIFE, -2);
        virtue_add(VIRTUE_CHANCE, -1);
    }

    return result;
}



/*
 * Probe nearby monsters
 */
bool probing(void)
{
    do_cmd_list_monsters(MON_LIST_PROBING);
    return TRUE; /*?? */
}



/*
 * The spell of destruction
 *
 * This spell "deletes" monsters (instead of "killing" them).
 */
bool destroy_area(int y1, int x1, int r, int power)
{
    point_t center = point_create(x1, y1);
    point_t d;
    bool    flag = FALSE;
    bool    in_generate = power < 0;
    obj_ptr obj;

    /* Prevent destruction of quest levels and town */
    if (cave->flags & DF_NO_DESTRUCT) return FALSE;

    /* Lose monster light */
    if (!in_generate) clear_mon_lite();

    /* Big area of affect */
    for (d.y = -r; d.y <= r; d.y++)
    {
        for (d.x = -r; d.x <= r; d.x++)
        {
            point_t p = point_add(center, d);
            cave_type *c_ptr;
            mon_ptr mon;

            if (!dun_pos_interior(cave, p)) continue;
            if (point_distance(center, p) > r) continue;

            /* Access the grid */
            c_ptr = cave_at(p);

            /* Lose room and vault */
            c_ptr->info &= ~(CAVE_ROOM | CAVE_ICKY);

            /* Lose light and knowledge */
            c_ptr->info &= ~(CAVE_MARK | CAVE_GLOW);

            if (!in_generate) /* Normal */
            {
                /* Lose unsafety */
                c_ptr->info &= ~(CAVE_UNSAFE);

                /* Hack -- Notice player affect */
                if (plr_at(p))
                {
                    /* Hurt the player later */
                    flag = TRUE;

                    /* Do not hurt this grid */
                    continue;
                }
            }

            /* Hack -- Skip the epicenter */
            if (point_equals(p, center)) continue;

            mon = mon_at(p);
            if (mon)
            {
                if (in_generate) delete_monster_at(p);
                else
                {
                    mon_race_ptr race = mon_race(mon);
                    bool resist = FALSE;

                    if (mon->smart & (1U << SM_SUMMONED))
                    {
                        /* XXX Redress the game balance wrt to summoning. We might consider
                         * some sort of odds here, but let's revert Destruction completely
                         * to the way it worked in Hengband. Note that Genocide always had
                         * a save in heng ... I copied this mechanic for Destruction. */
                        resist = FALSE;
                    }
                    else if (mon->mflag2 & MFLAG2_NODESTRUCT) resist = TRUE;
                    else if ((mon->mflag2 & MFLAG2_VAULT) && !one_in_(3)) resist = TRUE;
                    else if (race->level > randint0(power)) resist = TRUE;

                    if (resist)
                    {
                        bool see_m = mon_show_msg(mon);
                        char m_name[80];

                        monster_desc(m_name, mon, 0);

                        if (see_m)
                            msg_format("%^s is unaffected.", m_name);

                        mon_tim_remove(mon, MT_SLEEP);
                        if (is_friendly(mon) && !is_pet(mon))
                        {
                            if (see_m)
                                msg_format("%^s gets angry!", m_name);
                            set_hostile(mon);
                        }

                        if ( !(mon->mflag2 & MFLAG2_QUESTOR) /* Questors becoming immune to *destruct* can be advantageous! */
                          && !(race->flags2 & RF2_MULTIPLY)  /* Unmakers ... *shudder* */
                          && one_in_(13) )
                        {
                            mon->mflag2 |= MFLAG2_NODESTRUCT;
                        }
                        continue;
                    }
                    else
                    {
                        if (mon->mflag2 & MFLAG2_QUESTOR)
                        {
                            /* Heal the monster */
                            mon->hp = mon->maxhp;

                            /* Try to teleport away quest monsters */
                            if (!teleport_away(mon->id, (r * 2) + 1, TELEPORT_DEC_VALOUR)) continue;
                        }
                        else
                        {
                            delete_monster_at(p);
                        }
                    }
                }
            }

            for (obj = obj_at(p); obj; obj = obj->next)
            {
                if (obj_is_std_art(obj) && (!obj_is_known(obj) || in_generate))
                {
                    a_info[obj->name1].generated = FALSE;
                }
                else if (random_artifacts && obj->name3 && (!obj_is_known(obj) || in_generate))
                {
                    a_info[obj->name3].generated = FALSE;
                }
            }

            /* Delete objects */
            delete_object(p.y, p.x);

            /* Destroy "non-permanent" grids */
            if (!cave_perma_grid(c_ptr))
            {
                /* Wall (or floor) type */
                int t = randint0(200);

                if (!in_generate) /* Normal */
                {
                    if (t < 20)
                    {
                        /* Create granite wall */
                        cave_set_feat(p.y, p.x, feat_granite);
                    }
                    else if (t < 70)
                    {
                        /* Create quartz vein */
                        cave_set_feat(p.y, p.x, feat_quartz_vein);
                    }
                    else if (t < 100)
                    {
                        /* Create magma vein */
                        cave_set_feat(p.y, p.x, feat_magma_vein);
                    }
                    else
                    {
                        /* Create floor */
                        cave_set_feat(p.y, p.x, dun_type()->floor_type[randint0(100)]);
                    }
                }
                else /* In generation */
                {
                    if (t < 20)
                    {
                        /* Create granite wall */
                        place_extra_grid(p, c_ptr);
                    }
                    else if (t < 70)
                    {
                        /* Create quartz vein */
                        c_ptr->feat = feat_quartz_vein;
                    }
                    else if (t < 100)
                    {
                        /* Create magma vein */
                        c_ptr->feat = feat_magma_vein;
                    }
                    else
                    {
                        /* Create floor */
                        place_floor_grid(p, c_ptr);
                    }

                    /* Clear garbage of hidden trap or door */
                    c_ptr->mimic = 0;
                }
            }
        }
    }

    if (!in_generate)
    {
        for (d.y = -r; d.y <= r; d.y++)
        {
            for (d.x = -r; d.x <= r; d.x++)
            {
                point_t p = point_add(center, d);
                cave_type *c_ptr;

                if (!dun_pos_interior(cave, p)) continue;
                if (point_distance(center, p) > r) continue;

                /* Access the grid */
                c_ptr = cave_at(p);

                if (is_mirror_grid(c_ptr)) c_ptr->info |= CAVE_GLOW;
                else
                {
                    int i;

                    for (i = 0; i < 9; i++)
                    {
                        point_t q = point_step(p, ddd[i]);
                        cave_type *cc_ptr;
                        if (!dun_pos_interior(cave, q)) continue;
                        cc_ptr = cave_at(q);
                        if (have_flag(f_info[get_feat_mimic(cc_ptr)].flags, FF_GLOW))
                        {
                            c_ptr->info |= CAVE_GLOW;
                            break;
                        }
                    }
                }
            }
        }

        /* Hack -- Affect player */
        if (flag)
        {
            /* Message */
            msg_print("There is a searing blast of light!");

            /* Blind the player */
            if (!res_save_default(RES_BLIND) && !res_save_default(RES_LITE))
            {
                /* Become blind */
                plr_tim_add(T_BLIND, 10 + randint1(10));
            }
        }

        dun_forget_flow(cave);

        /* Mega-Hack -- Forget the view and lite */
        p_ptr->update |= (PU_UN_VIEW | PU_UN_LITE);

        /* Update stuff */
        p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_MON_LITE | PU_MONSTERS);

        if (weaponmaster_is_(WEAPONMASTER_DIGGERS))
            p_ptr->update |= PU_BONUS;

        /* Redraw map */
        p_ptr->redraw |= (PR_MAP);

        /* Window stuff */
        p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);

        if (p_ptr->special_defense & NINJA_S_STEALTH)
        {
            if (cave_at(p_ptr->pos)->info & CAVE_GLOW) set_superstealth(FALSE);
        }
    }

    /* Success */
    return (TRUE);
}


/*
 * Induce an "earthquake" of the given radius at the given location.
 *
 * This will turn some walls into floors and some floors into walls.
 *
 * The player will take damage and "jump" into a safe grid if possible,
 * otherwise, he will "tunnel" through the rubble instantaneously.
 *
 * Monsters will take damage, and "jump" into a safe grid if possible,
 * otherwise they will be "buried" in the rubble, disappearing from
 * the level in the same way that they do when genocided.
 *
 * Note that thus the player and monsters (except eaters of walls and
 * passers through walls) will never occupy the same grid as a wall.
 * Note that as of now (2.7.8) no monster may occupy a "wall" grid, even
 * for a single turn, unless that monster can pass_walls or kill_walls.
 * This has allowed massive simplification of the "monster" code.
 */
bool earthquake_aux(point_t pos, int r, int m_idx)
{
    int cx = pos.x, cy = pos.y; /* XXX */
    int             i, t, y, x, yy, xx, dy, dx;
    int             damage = 0;
    int             sn = 0, sy = 0, sx = 0;
    bool            hurt = FALSE;
    cave_type       *c_ptr;
    mon_ptr         m_ptr;
    bool            map[32][32];


    /* Prevent destruction of quest levels and town */
    if (cave->flags & DF_NO_QUAKE) return FALSE;

    /* Paranoia -- Enforce maximum range */
    if (r > 12) r = 12;

    /* Clear the "maximal blast" area */
    for (y = 0; y < 32; y++)
    {
        for (x = 0; x < 32; x++)
        {
            map[y][x] = FALSE;
        }
    }

    /* Check around the epicenter */
    for (dy = -r; dy <= r; dy++)
    {
        for (dx = -r; dx <= r; dx++)
        {
            /* Extract the location */
            yy = cy + dy;
            xx = cx + dx;

            /* Skip illegal grids */
            if (!in_bounds(yy, xx)) continue;

            /* Skip distant grids */
            if (distance(cy, cx, yy, xx) > r) continue;

            /* Access the grid */
            c_ptr = cave_at_xy(xx, yy);

            /* Lose room and vault */
            c_ptr->info &= ~(CAVE_ROOM | CAVE_ICKY | CAVE_UNSAFE);

            /* Lose light and knowledge */
            c_ptr->info &= ~(CAVE_GLOW | CAVE_MARK);

            /* Skip the epicenter */
            if (!dx && !dy) continue;

            /* Skip most grids */
            if (randint0(100) < 85) continue;

            /* Damage this grid */
            map[16+yy-cy][16+xx-cx] = TRUE;

            /* Hack -- Take note of player damage */
            if (player_bold(yy, xx)) hurt = TRUE;
        }
    }

    /* First, affect the player (if necessary) */
    if (hurt && !p_ptr->pass_wall && !p_ptr->kill_wall)
    {
        /* Check around the player */
        for (i = 0; i < 8; i++)
        {
            /* Access the location */
            y = p_ptr->pos.y + ddy_ddd[i];
            x = p_ptr->pos.x + ddx_ddd[i];

            /* Skip non-empty grids */
            if (!cave_empty_bold(y, x)) continue;

            /* Important -- Skip "quake" grids */
            if (map[16+y-cy][16+x-cx]) continue;

            if (mon_at_xy(x, y)) continue;

            /* Count "safe" grids */
            sn++;

            /* Randomize choice */
            if (randint0(sn) > 0) continue;

            /* Save the safe location */
            sy = y; sx = x;
        }

        /* Random message */
        switch (randint1(3))
        {
            case 1:
            {
                msg_print("The cave ceiling collapses!");
                break;
            }
            case 2:
            {
                msg_print("The cave floor twists in an unnatural way!");
                break;
            }
            default:
            {
                msg_print("The cave quakes! You are pummeled with debris!");
                break;
            }
        }

        /* Hurt the player a lot */
        damage = 0;
        if (!sn)
        {
            if (!mut_present(MUT_EVASION) || one_in_(2))
            {
                msg_print("You are <color:v>severely crushed</color>!");
                damage = 50 + damroll(3, 50);
                if (!p_ptr->no_stun) plr_tim_add(T_STUN, damroll(7, 7));
            }
        }

        /* Destroy the grid, and push the player to safety */
        else
        {
            /* Calculate results */
            switch (randint1(3))
            {
                case 1:
                {
                    msg_print("You nimbly dodge the blast!");
                    break;
                }
                case 2:
                {
                    if (!mut_present(MUT_EVASION) || one_in_(2))
                    {
                        msg_print("You are <color:R>bashed by rubble</color>!");
                        damage = damroll(10, 4);
                        if (!p_ptr->no_stun) plr_tim_add(T_STUN, damroll(5, 5));
                    }
                    break;
                }
                case 3:
                {
                    if (!mut_present(MUT_EVASION) || one_in_(2))
                    {
                        msg_print("You are <color:R>crushed between the floor and ceiling</color>!");
                        damage = damroll(10, 4);
                        if (!p_ptr->no_stun) plr_tim_add(T_STUN, damroll(5, 5));
                    }
                    break;
                }
            }

            /* Move the player to the safe location */
            (void)move_player_effect(point_create(sx, sy), MPE_DONT_PICKUP);
        }

        /* Important -- no wall on player */
        map[16+p_ptr->pos.y-cy][16+p_ptr->pos.x-cx] = FALSE;

        /* Take some damage */
        if (damage)
        {
            char *killer;

            if (m_idx)
            {
                char m_name[80];
                monster_type *m_ptr = dun_mon(cave, m_idx);

                /* Get the monster's real name */
                monster_desc(m_name, m_ptr, MD_IGNORE_HALLU | MD_ASSUME_VISIBLE | MD_INDEF_VISIBLE);

                killer = format("an earthquake caused by %s", m_name);
            }
            else
            {
                killer = "an earthquake";
            }

            take_hit(DAMAGE_ATTACK, damage, killer);
        }
    }

    /* Examine the quaked region */
    for (dy = -r; dy <= r; dy++)
    {
        for (dx = -r; dx <= r; dx++)
        {
            /* Extract the location */
            yy = cy + dy;
            xx = cx + dx;

            /* Skip unaffected grids */
            if (!map[16+yy-cy][16+xx-cx]) continue;

            /* Access the grid */
            c_ptr = cave_at_xy(xx, yy);

            /* Process monsters */
            m_ptr = mon_at_xy(xx, yy);
            if (m_ptr)
            {
                monster_race *r_ptr = mon_race(m_ptr);

                if (m_ptr->id == p_ptr->riding) continue;
                
                /* Quest monsters and Amberites (Blood Curse). Note that dun_delete_mon()
                 * can only be called once per monster, and mon_take_hit() will do this 
                 * in a bit for Amberites (after activate_ty_curse). */
                if ((m_ptr->mflag2 & MFLAG2_QUESTOR) || mon_is_dead(m_ptr))
                {
                    map[16+yy-cy][16+xx-cx] = FALSE;
                    continue;
                }

                /* Most monsters cannot co-exist with rock */
                if (!(r_ptr->flags2 & (RF2_KILL_WALL)) &&
                    !(r_ptr->flags2 & (RF2_PASS_WALL)))
                {
                    char m_name[80];

                    /* Assume not safe */
                    sn = 0;

                    /* Monster can move to escape the wall */
                    if (!(r_ptr->flags1 & (RF1_NEVER_MOVE)))
                    {
                        /* Look for safety */
                        for (i = 0; i < 8; i++)
                        {
                            /* Access the grid */
                            y = yy + ddy_ddd[i];
                            x = xx + ddx_ddd[i];

                            /* Skip non-empty grids */
                            if (!cave_empty_bold(y, x)) continue;

                            /* Hack -- no safety on glyph of warding */
                            if (is_glyph_grid(cave_at_xy(x, y))) continue;
                            if (is_mon_trap_grid(cave_at_xy(x, y))) continue;

                            /* ... nor on the Pattern */
                            if (pattern_tile(y, x)) continue;

                            /* Important -- Skip "quake" grids */
                            if (map[16+y-cy][16+x-cx]) continue;

                            if (mon_at_xy(x, y)) continue;
                            if (player_bold(y, x)) continue;

                            /* Count "safe" grids */
                            sn++;

                            /* Randomize choice */
                            if (randint0(sn) > 0) continue;

                            /* Save the safe grid */
                            sy = y; sx = x;
                        }
                    }

                    /* Describe the monster */
                    monster_desc(m_name, m_ptr, 0);

                    /* Scream in pain */
                    if (mon_show_msg(m_ptr)) msg_format("%^s wails out in pain!", m_name);

                    /* Take damage from the quake */
                    damage = (sn ? damroll(4, 8) : (m_ptr->hp + 1));

                    /* Monster is certainly awake */
                    mon_tim_delete(m_ptr, MT_SLEEP);

                    /* Apply damage directly */
                    m_ptr->hp -= damage;

                    /* Delete (not kill) "dead" monsters */
                    if (m_ptr->hp < 0)
                    {
                        if (mon_show_msg(m_ptr)) msg_format("%^s is embedded in the rock!", m_name);

                        /* Delete the monster */
                        delete_monster_at(point_create(xx, yy));

                        /* No longer safe */
                        sn = 0;
                    }

                    /* Hack -- Escape from the rock */
                    if (sn)
                        dun_move_mon(cave, m_ptr, point_create(sx, sy));
                }
            }
        }
    }

    /* Lose monster light */
    clear_mon_lite();

    /* Examine the quaked region */
    for (dy = -r; dy <= r; dy++)
    {
        for (dx = -r; dx <= r; dx++)
        {
            /* Extract the location */
            yy = cy + dy;
            xx = cx + dx;

            /* Skip unaffected grids */
            if (!map[16+yy-cy][16+xx-cx]) continue;

            /* Access the cave grid */
            c_ptr = cave_at_xy(xx, yy);

            /* Paranoia -- never affect player */
/*            if (player_bold(yy, xx)) continue; */

            /* Destroy location (if valid) */
            if (cave_valid_bold(yy, xx))
            {
                /* Delete objects */
                delete_object(yy, xx);

                /* Wall (or floor) type */
                t = cave_have_flag_bold(yy, xx, FF_PROJECT) ? randint0(100) : 200;

                /* Granite */
                if (t < 20)
                {
                    /* Create granite wall */
                    cave_set_feat(yy, xx, feat_granite);
                }

                /* Quartz */
                else if (t < 70)
                {
                    /* Create quartz vein */
                    cave_set_feat(yy, xx, feat_quartz_vein);
                }

                /* Magma */
                else if (t < 100)
                {
                    /* Create magma vein */
                    cave_set_feat(yy, xx, feat_magma_vein);
                }

                /* Floor */
                else
                {
                    /* Create floor */
                    cave_set_feat(yy, xx, dun_type()->floor_type[randint0(100)]);
                }
            }
        }
    }


    /* Process "re-glowing" */
    for (dy = -r; dy <= r; dy++)
    {
        for (dx = -r; dx <= r; dx++)
        {
            /* Extract the location */
            yy = cy + dy;
            xx = cx + dx;

            /* Skip illegal grids */
            if (!in_bounds(yy, xx)) continue;

            /* Skip distant grids */
            if (distance(cy, cx, yy, xx) > r) continue;

            /* Access the grid */
            c_ptr = cave_at_xy(xx, yy);

            if (is_mirror_grid(c_ptr)) c_ptr->info |= CAVE_GLOW;
            else
            {
                int ii, yyy, xxx;
                cave_type *cc_ptr;

                for (ii = 0; ii < 9; ii++)
                {
                    yyy = yy + ddy_ddd[ii];
                    xxx = xx + ddx_ddd[ii];
                    if (!in_bounds2(yyy, xxx)) continue;
                    cc_ptr = cave_at_xy(xxx, yyy);
                    if (have_flag(f_info[get_feat_mimic(cc_ptr)].flags, FF_GLOW))
                    {
                        c_ptr->info |= CAVE_GLOW;
                        break;
                    }
                }
            }
        }
    }


    /* Mega-Hack -- Forget the view and lite */
    p_ptr->update |= (PU_UN_VIEW | PU_UN_LITE);

    /* Update stuff */
    p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_MON_LITE | PU_MONSTERS);

    if (weaponmaster_is_(WEAPONMASTER_DIGGERS))
        p_ptr->update |= PU_BONUS;

    /* Update the health bar */
    p_ptr->redraw |= PR_HEALTH_BARS;

    /* Redraw map */
    p_ptr->redraw |= (PR_MAP);

    /* Window stuff */
    p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);

    if (p_ptr->special_defense & NINJA_S_STEALTH)
    {
        if (cave_at(p_ptr->pos)->info & CAVE_GLOW) set_superstealth(FALSE);
    }

    /* Success */
    return (TRUE);
}

bool earthquake(point_t pos, int r)
{
    return earthquake_aux(pos, r, 0);
}


void discharge_minion(void)
{
    vec_ptr pets = plr_pets_for_dismiss();
    int     i;
    bool    okay = !p_ptr->riding;

    for (i = 0; i < vec_length(pets); i++)
    {
        mon_ptr pet = vec_get(pets, i);
        if (pet->nickname) okay = FALSE;
    }
    if (!okay && get_check("You will blast all pets. Are you sure? "))
        okay = TRUE;

    for (i = 0; okay && i < vec_length(pets); i++)
    {
        mon_ptr pet = vec_get(pets, i);
        int     dam;
        int     typ = GF_PLASMA;
        int     rad = 2 + mon_race(pet)->level/20;

        if (p_ptr->pclass == CLASS_NECROMANCER)
        {
            switch (randint1(4))
            {
            case 1: typ = GF_POIS; break;
            case 2: typ = GF_NETHER; break;
            case 3: typ = GF_DISENCHANT; break;
            case 4: typ = GF_MANA; break;
            }
        }
        if (mon_race(pet)->flags1 & RF1_UNIQUE)
        {
            char m_name[80];
            monster_desc(m_name, pet, 0x00);
            msg_format("%^s resists being blasted, and runs away.", m_name);
            delete_monster(pet);
            continue;
        }
        dam = pet->maxhp / 2;
        if (dam > 100) dam = (dam-100)/2 + 100;
        if (dam > 400) dam = (dam-400)/2 + 400;
        if (dam > 800) dam = 800;
        project(pet->id, rad, pet->pos.y, pet->pos.x, dam, typ,
            PROJECT_STOP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL);

        delete_monster(pet);
    }
    vec_free(pets);
}


/*
 * This routine clears the entire "temp" set.
 *
 * This routine will Perma-Lite all "temp" grids.
 *
 * This routine is used (only) by "lite_room()"
 *
 * Dark grids are illuminated.
 *
 * Also, process all affected monsters.
 *
 * SMART monsters always wake up when illuminated
 * NORMAL monsters wake up 1/4 the time when illuminated
 * STUPID monsters wake up 1/10 the time when illuminated
 */
static void cave_temp_room_lite(void)
{
    int i;

    /* Clear them all */
    for (i = 0; i < temp_n; i++)
    {
        int y = temp_y[i];
        int x = temp_x[i];

        cave_type *c_ptr = cave_at_xy(x, y);
        mon_ptr m_ptr = mon_at_xy(x, y);

        /* No longer in the array */
        c_ptr->info &= ~(CAVE_TEMP);

        /* Update only non-CAVE_GLOW grids */
        /* if (c_ptr->info & (CAVE_GLOW)) continue; */

        /* Perma-Lite */
        c_ptr->info |= (CAVE_GLOW);

        /* Process affected monsters */
        if (m_ptr)
        {
            int          chance = 25;
            mon_race_ptr r_ptr = mon_race(m_ptr);

            /* Update the monster */
            update_mon(m_ptr, FALSE);

            /* Stupid monsters rarely wake up */
            if (r_ptr->flags2 & (RF2_STUPID)) chance = 10;

            /* Smart monsters always wake up */
            if (r_ptr->flags2 & (RF2_SMART)) chance = 100;

            /* Sometimes monsters wake up */
            if (mon_tim_find(m_ptr, MT_SLEEP) && randint0(100) < chance)
                mon_tim_remove(m_ptr, MT_SLEEP);
        }

        /* Note */
        note_spot(y, x);

        /* Redraw */
        lite_spot(y, x);

        update_local_illumination(y, x);
    }

    /* None left */
    temp_n = 0;
}



/*
 * This routine clears the entire "temp" set.
 *
 * This routine will "darken" all "temp" grids.
 *
 * In addition, some of these grids will be "unmarked".
 *
 * This routine is used (only) by "unlite_room()"
 *
 * Also, process all affected monsters
 */
static void cave_temp_room_unlite(void)
{
    int i;

    /* Clear them all */
    for (i = 0; i < temp_n; i++)
    {
        int y = temp_y[i];
        int x = temp_x[i];
        int j;

        cave_type *c_ptr = cave_at_xy(x, y);
        bool do_dark = !is_mirror_grid(c_ptr);

        /* No longer in the array */
        c_ptr->info &= ~(CAVE_TEMP);

        /* Darken the grid */
        if (do_dark)
        {
            mon_ptr m_ptr;
            if (cave->dun_type_id == D_SURFACE || !is_daytime())
            {
                for (j = 0; j < 9; j++)
                {
                    int by = y + ddy_ddd[j];
                    int bx = x + ddx_ddd[j];

                    if (in_bounds2(by, bx))
                    {
                        cave_type *cc_ptr = cave_at_xy(bx, by);

                        if (have_flag(f_info[get_feat_mimic(cc_ptr)].flags, FF_GLOW))
                        {
                            do_dark = FALSE;
                            break;
                        }
                    }
                }

                if (!do_dark) continue;
            }

            c_ptr->info &= ~(CAVE_GLOW);

            /* Hack -- Forget "boring" grids */
            if (!have_flag(f_info[get_feat_mimic(c_ptr)].flags, FF_REMEMBER))
            {
                /* Forget the grid */
                if (!view_torch_grids) c_ptr->info &= ~(CAVE_MARK);

                /* Notice */
                note_spot(y, x);
            }

            /* Process affected monsters */
            m_ptr = mon_at_xy(x, y);
            if (m_ptr) update_mon(m_ptr, FALSE);

            /* Redraw */
            lite_spot(y, x);

            update_local_illumination(y, x);
        }
    }

    /* None left */
    temp_n = 0;
}


/*
 * Determine how much contiguous open space this grid is next to
 */
static int next_to_open(int cy, int cx, bool (*pass_bold)(int, int))
{
    int i;

    int y, x;

    int len = 0;
    int blen = 0;

    for (i = 0; i < 16; i++)
    {
        y = cy + ddy_cdd[i % 8];
        x = cx + ddx_cdd[i % 8];

        /* Found a wall, break the length */
        if (!pass_bold(y, x))
        {
            /* Track best length */
            if (len > blen)
            {
                blen = len;
            }

            len = 0;
        }
        else
        {
            len++;
        }
    }

    return (MAX(len, blen));
}


static int next_to_walls_adj(int cy, int cx, bool (*pass_bold)(int, int))
{
    int i;

    int y, x;

    int c = 0;

    for (i = 0; i < 8; i++)
    {
        y = cy + ddy_ddd[i];
        x = cx + ddx_ddd[i];

        if (!pass_bold(y, x)) c++;
    }

    return c;
}


/*
 * Aux function -- see below
 */
static void cave_temp_room_aux(int y, int x, bool only_room, bool (*pass_bold)(int, int))
{
    cave_type *c_ptr;

    /* Get the grid */
    c_ptr = cave_at_xy(x, y);

    /* Avoid infinite recursion */
    if (c_ptr->info & (CAVE_TEMP)) return;

    /* Do not "leave" the current room */
    if (!(c_ptr->info & (CAVE_ROOM)))
    {
        if (only_room) return;

        /* Verify */
        if (!in_bounds2(y, x)) return;

        /* Do not exceed the maximum spell range ...
         * ... x2 for quests. When designing quests, it is a bit
         * tedious to set the ROOM flag correctly (unless the entire
         * quest is a single room). Much easier to just have light
         * area work a bit farther than normal.*/
        if (distance(p_ptr->pos.y, p_ptr->pos.x, y, x) > MAX_RANGE*2) return;

        /* Verify this grid */
        /*
         * The reason why it is ==6 instead of >5 is that 8 is impossible
         * due to the check for cave_bold above.
         * 7 lights dead-end corridors (you need to do this for the
         * checkboard interesting rooms, so that the boundary is lit
         * properly.
         * This leaves only a check for 6 bounding walls!
         */
        if (in_bounds(y, x) && pass_bold(y, x) &&
            (next_to_walls_adj(y, x, pass_bold) == 6) && (next_to_open(y, x, pass_bold) <= 1)) return;
    }

    /* Paranoia -- verify space */
    if (temp_n == TEMP_MAX) return;

    /* Mark the grid as "seen" */
    c_ptr->info |= (CAVE_TEMP);

    /* Add it to the "seen" set */
    temp_y[temp_n] = y;
    temp_x[temp_n] = x;
    temp_n++;
}

/*
 * Aux function -- see below
 */
static bool cave_pass_lite_bold(int y, int x)
{
    return cave_los_bold(y, x);
}

/*
 * Aux function -- see below
 */
static void cave_temp_lite_room_aux(int y, int x)
{
    cave_temp_room_aux(y, x, FALSE, cave_pass_lite_bold);
}

/*
 * Aux function -- see below
 */
static bool cave_pass_dark_bold(int y, int x)
{
    return cave_have_flag_bold(y, x, FF_PROJECT);
}

/*
 * Aux function -- see below
 */
static void cave_temp_unlite_room_aux(int y, int x)
{
    cave_temp_room_aux(y, x, TRUE, cave_pass_dark_bold);
}




/*
 * Illuminate any room containing the given location.
 */
void lite_room(int y1, int x1)
{
    int i, x, y;

    /* Add the initial grid */
    cave_temp_lite_room_aux(y1, x1);

    /* While grids are in the queue, add their neighbors */
    for (i = 0; i < temp_n; i++)
    {
        x = temp_x[i], y = temp_y[i];

        /* Walls get lit, but stop light */
        if (!cave_pass_lite_bold(y, x)) continue;

        /* Spread adjacent */
        cave_temp_lite_room_aux(y + 1, x);
        cave_temp_lite_room_aux(y - 1, x);
        cave_temp_lite_room_aux(y, x + 1);
        cave_temp_lite_room_aux(y, x - 1);

        /* Spread diagonal */
        cave_temp_lite_room_aux(y + 1, x + 1);
        cave_temp_lite_room_aux(y - 1, x - 1);
        cave_temp_lite_room_aux(y - 1, x + 1);
        cave_temp_lite_room_aux(y + 1, x - 1);
    }

    /* Now, lite them all up at once */
    cave_temp_room_lite();

    if (p_ptr->special_defense & NINJA_S_STEALTH)
    {
        if (cave_at(p_ptr->pos)->info & CAVE_GLOW) set_superstealth(FALSE);
    }
}


/*
 * Darken all rooms containing the given location
 */
void unlite_room(int y1, int x1)
{
    int i, x, y;

    /* Add the initial grid */
    cave_temp_unlite_room_aux(y1, x1);

    /* Spread, breadth first */
    for (i = 0; i < temp_n; i++)
    {
        x = temp_x[i], y = temp_y[i];

        /* Walls get dark, but stop darkness */
        if (!cave_pass_dark_bold(y, x)) continue;

        /* Spread adjacent */
        cave_temp_unlite_room_aux(y + 1, x);
        cave_temp_unlite_room_aux(y - 1, x);
        cave_temp_unlite_room_aux(y, x + 1);
        cave_temp_unlite_room_aux(y, x - 1);

        /* Spread diagonal */
        cave_temp_unlite_room_aux(y + 1, x + 1);
        cave_temp_unlite_room_aux(y - 1, x - 1);
        cave_temp_unlite_room_aux(y - 1, x + 1);
        cave_temp_unlite_room_aux(y + 1, x - 1);
    }

    /* Now, darken them all at once */
    cave_temp_room_unlite();
}



/*
 * Hack -- call light around the player
 * Affect all monsters in the projection radius
 */
bool lite_area(int dam, int rad)
{
    int flg = PROJECT_GRID | PROJECT_KILL;

    /* Hack -- Message */
    if (!plr_tim_find(T_BLIND))
    {
        msg_print("You are surrounded by a white light.");

    }

    /* Hook into the "project()" function */
    (void)project(0, rad, p_ptr->pos.y, p_ptr->pos.x, dam, GF_LITE_WEAK, flg);

    /* Lite up the room */
    lite_room(p_ptr->pos.y, p_ptr->pos.x);

    /* Assume seen */
    return (TRUE);
}


/*
 * Hack -- call darkness around the player
 * Affect all monsters in the projection radius
 */
bool unlite_area(int dam, int rad)
{
    int flg = PROJECT_GRID | PROJECT_KILL;

    /* Hack -- Message */
    if (!plr_tim_find(T_BLIND))
    {
        msg_print("Darkness surrounds you.");

    }

    /* Hook into the "project()" function */
    (void)project(0, rad, p_ptr->pos.y, p_ptr->pos.x, dam, GF_DARK_WEAK, flg);

    /* Lite up the room */
    unlite_room(p_ptr->pos.y, p_ptr->pos.x);

    /* Assume seen */
    return (TRUE);
}



/*
 * Cast a ball spell
 * Stop if we hit a monster, act as a "ball"
 * Allow "target" mode to pass over monsters
 * Affect grids, objects, and monsters
 */
bool fire_ball(int typ, int dir, int dam, int rad)
{
    return fire_ball_aux(typ, dir, dam, rad, 0);
}

bool fire_ball_aux(int typ, int dir, int dam, int rad, int xtra_flgs)
{
    int tx, ty;

    int flg = PROJECT_STOP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | xtra_flgs;

    if (typ == GF_CONTROL_LIVING) flg|= PROJECT_HIDE;
    /* Use the given direction */
    tx = p_ptr->pos.x + 99 * ddx[dir];
    ty = p_ptr->pos.y + 99 * ddy[dir];

    /* Hack -- Use an actual "target"
     * Note: target_okay() will check target_able() which requires projectable() 
     * This requirement is *wrong* in certain situations (e.g. Breathe Disintegration) 
     * but it is unclear how to best communicate that fact. However, what would we do
     * at this point anyway? Project() at the players feet? That is just silly ... */
    if (dir == 5 /* && target_okay() */)
    {
        flg &= ~(PROJECT_STOP);
        tx = target_col;
        ty = target_row;
    }

    /* Analyze the "dir" and the "target". Hurt items on floor. */
    return (project(0, rad, ty, tx, dam, typ, flg));
}


/*
 * Cast a ball spell
 * Stop if we hit a monster, act as a "ball"
 * Allow "target" mode to pass over monsters
 * Affect grids, objects, and monsters
 */
bool fire_rocket(int typ, int dir, int dam, int rad)
{
    int tx, ty;

    int flg = PROJECT_STOP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

    /* Use the given direction */
    tx = p_ptr->pos.x + 99 * ddx[dir];
    ty = p_ptr->pos.y + 99 * ddy[dir];

    /* Hack -- Use an actual "target" */
    if ((dir == 5) && target_okay())
    {
        tx = target_col;
        ty = target_row;
    }

    /* Analyze the "dir" and the "target". Hurt items on floor. */
    return (project(0, rad, ty, tx, dam, typ, flg));
}


/*
 * Cast a ball spell
 * Stop if we hit a monster, act as a "ball"
 * Allow "target" mode to pass over monsters
 * Affect grids, objects, and monsters
 */
bool fire_ball_hide(int typ, int dir, int dam, int rad)
{
    int tx, ty;

    int flg = PROJECT_STOP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | PROJECT_HIDE;

    /* Use the given direction */
    tx = p_ptr->pos.x + 99 * ddx[dir];
    ty = p_ptr->pos.y + 99 * ddy[dir];

    /* Hack -- Use an actual "target" */
    if ((dir == 5) && target_okay())
    {
        flg &= ~(PROJECT_STOP);
        tx = target_col;
        ty = target_row;
    }

    /* Analyze the "dir" and the "target". Hurt items on floor. */
    return (project(0, rad, ty, tx, dam, typ, flg));
}


/*
 * Cast a meteor spell, defined as a ball spell cast by an arbitary monster,
 * player, or outside source, that starts out at an arbitrary location, and
 * leaving no trail from the "caster" to the target. This function is
 * especially useful for bombardments and similar. -LM-
 *
 * Option to hurt the player.
 */
bool fire_meteor(int who, int typ, int y, int x, int dam, int rad)
{
    int flg = PROJECT_STOP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

    /* Analyze the "target" and the caster. */
    return (project(who, rad, y, x, dam, typ, flg));
}


bool fire_blast(int typ, int dir, int dd, int ds, int num, int dev)
{
    int ly, lx, ld;
    int ty, tx, y, x;
    int i;

    int flg = PROJECT_FAST | PROJECT_THRU | PROJECT_STOP | PROJECT_KILL | PROJECT_REFLECTABLE | PROJECT_GRID;

    /* Assume okay */
    bool result = TRUE;

    /* Use the given direction */
    if (dir != 5)
    {
        ly = ty = p_ptr->pos.y + 20 * ddy[dir];
        lx = tx = p_ptr->pos.x + 20 * ddx[dir];
    }

    /* Use an actual "target" */
    else /* if (dir == 5) */
    {
        tx = target_col;
        ty = target_row;

        lx = 20 * (tx - p_ptr->pos.x) + p_ptr->pos.x;
        ly = 20 * (ty - p_ptr->pos.y) + p_ptr->pos.y;
    }

    ld = distance(p_ptr->pos.y, p_ptr->pos.x, ly, lx);

    /* Blast */
    for (i = 0; i < num; i++)
    {
        while (1)
        {
            /* Get targets for some bolts */
            y = rand_spread(ly, ld * dev / 20);
            x = rand_spread(lx, ld * dev / 20);

            if (distance(ly, lx, y, x) <= ld * dev / 20) break;
        }

        /* Analyze the "dir" and the "target". */
        if (!project(0, 0, y, x, damroll(dd, ds), typ, flg))
        {
            result = FALSE;
        }
    }

    return (result);
}


/*
 * Switch position with a monster.
 */
bool teleport_swap(int dir)
{
    point_t tgt;
    cave_type * c_ptr;
    monster_type * m_ptr;
    monster_race * r_ptr;

    if ((dir == 5) && target_okay())
        tgt = point_create(target_col, target_row);
    else
        tgt = point_step(p_ptr->pos, dir);

    if (p_ptr->anti_tele)
    {
        msg_print("A mysterious force prevents you from teleporting!");
        equip_learn_flag(OF_NO_TELE);
        return FALSE;
    }

    c_ptr = cave_at(tgt);
    m_ptr = mon_at(tgt);
    if (!m_ptr || (m_ptr->id == p_ptr->riding))
    {
        msg_print("You can't trade places with that!");
        return FALSE;
    }

    if ((c_ptr->info & CAVE_ICKY) || point_distance(p_ptr->pos, tgt) > p_ptr->lev*3/2 + 10)
    {
        msg_print("Failed to swap.");
        return FALSE;
    }

    r_ptr = mon_race(m_ptr);

    mon_tim_delete(m_ptr, MT_SLEEP);

    if (r_ptr->flagsr & RFR_RES_TELE)
    {
        msg_print("Your teleportation is blocked!");
        mon_lore_r(m_ptr, RFR_RES_TELE);
        return FALSE;
    }

    sound(SOUND_TELEPORT);

    /* Swap the player and monster */
    move_player_effect(tgt, MPE_FORGET_FLOW | MPE_HANDLE_STUFF | MPE_DONT_PICKUP);

    /* Success */
    return TRUE;
}


/*
 * Hack -- apply a "projection()" in a direction (or at the target)
 */
bool project_hook(int typ, int dir, int dam, int flg)
{
    int tx, ty;

    /* Pass through the target if needed */
    flg |= (PROJECT_THRU);

    /* Use the given direction */
    tx = p_ptr->pos.x + ddx[dir];
    ty = p_ptr->pos.y + ddy[dir];

    /* Hack -- Use an actual "target"
     * Note: target_okay() will check target_able() which requires projectable() 
     * This requirement is *wrong* in certain situations (e.g. Beam of Disintegration) 
     * but it is unclear how to best communicate that fact. However, what would we do
     * at this point anyway? Project() at the players feet? That is just silly ... */
    if (dir == 5 /*&& target_okay()*/)
    {
        tx = target_col;
        ty = target_row;
    }

    /* Analyze the "dir" and the "target", do NOT explode */
    return (project(0, 0, ty, tx, dam, typ, flg));
}


/*
 * Cast a bolt spell.
 * Stop if we hit a monster, as a "bolt".
 * Affect monsters and grids (not objects).
 */
bool fire_bolt(int typ, int dir, int dam)
{
    int flg = PROJECT_STOP | PROJECT_KILL | PROJECT_REFLECTABLE | PROJECT_GRID;
    return (project_hook(typ, dir, dam, flg));
}


/*
 * Cast a beam spell.
 * Pass through monsters, as a "beam".
 * Affect monsters, grids and objects.
 */
bool fire_beam(int typ, int dir, int dam)
{
    int flg = PROJECT_BEAM | PROJECT_KILL | PROJECT_GRID | PROJECT_ITEM;
    return (project_hook(typ, dir, dam, flg));
}


/*
 * Cast a bolt spell, or rarely, a beam spell
 */
bool fire_bolt_or_beam(int prob, int typ, int dir, int dam)
{
    if (randint0(100) < prob)
    {
        return (fire_beam(typ, dir, dam));
    }
    else
    {
        return (fire_bolt(typ, dir, dam));
    }
}


/*
 * Some of the old functions
 */
bool lite_line(int dir)
{
    int flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_KILL;
    return (project_hook(GF_LITE_WEAK, dir, damroll(6, 8), flg));
}


bool drain_life(int dir, int dam)
{
    int flg = PROJECT_STOP | PROJECT_KILL | PROJECT_REFLECTABLE;
    return (project_hook(GF_OLD_DRAIN, dir, dam, flg));
}


bool wall_to_mud(int dir)
{
    int flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;
    return (project_hook(GF_KILL_WALL, dir, 20 + randint1(30), flg));
}


bool wizard_lock(int dir)
{
    int flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;
    return (project_hook(GF_JAM_DOOR, dir, 20 + randint1(30), flg));
}


bool destroy_door(int dir)
{
    int flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM;
    return (project_hook(GF_KILL_DOOR, dir, 0, flg));
}


bool disarm_trap(int dir)
{
    int flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM;
    return (project_hook(GF_KILL_TRAP, dir, 0, flg));
}


bool heal_monster(int dir, int dam)
{
    int flg = PROJECT_STOP | PROJECT_KILL | PROJECT_REFLECTABLE;
    return (project_hook(GF_OLD_HEAL, dir, dam, flg));
}


bool speed_monster(int dir)
{
    int flg = PROJECT_STOP | PROJECT_KILL | PROJECT_REFLECTABLE;
    return (project_hook(GF_OLD_SPEED, dir, p_ptr->lev, flg));
}


bool slow_monster(int dir)
{
    int flg = PROJECT_STOP | PROJECT_KILL | PROJECT_REFLECTABLE;
    return (project_hook(GF_OLD_SLOW, dir, p_ptr->lev, flg));
}


bool sleep_monster(int dir, int power)
{
    int flg = PROJECT_STOP | PROJECT_KILL | PROJECT_REFLECTABLE;
    return (project_hook(GF_OLD_SLEEP, dir, power, flg));
}


bool stasis_monster(int dir)
{
    return (fire_ball_hide(GF_STASIS, dir, p_ptr->lev*2, 0));
}


bool stasis_evil(int dir)
{
    return (fire_ball_hide(GF_STASIS_EVIL, dir, p_ptr->lev*2, 0));
}


bool confuse_monster(int dir, int plev)
{
    int flg = PROJECT_STOP | PROJECT_KILL | PROJECT_REFLECTABLE;
    return (project_hook(GF_OLD_CONF, dir, plev, flg));
}


bool stun_monster(int dir, int plev)
{
    int flg = PROJECT_STOP | PROJECT_KILL | PROJECT_REFLECTABLE;
    return (project_hook(GF_STUN, dir, 5 + plev/5, flg));
}


bool poly_monster(int dir)
{
    int flg = PROJECT_STOP | PROJECT_KILL | PROJECT_REFLECTABLE;
    bool tester = (project_hook(GF_OLD_POLY, dir, p_ptr->lev, flg));
    if (tester)
        virtue_add(VIRTUE_CHANCE, 1);
    return(tester);
}


bool clone_monster(int dir)
{
    int flg = PROJECT_STOP | PROJECT_KILL | PROJECT_REFLECTABLE;
    return (project_hook(GF_OLD_CLONE, dir, 0, flg));
}


bool fear_monster(int dir, int plev)
{
    int flg = PROJECT_STOP | PROJECT_KILL | PROJECT_REFLECTABLE;
    return (project_hook(GF_TURN_ALL, dir, plev, flg));
}


bool teleport_monster(int dir)
{
    int flg = PROJECT_BEAM | PROJECT_KILL;
    return (project_hook(GF_AWAY_ALL, dir, MAX_SIGHT * 5, flg));
}

/*
 * Hooks -- affect adjacent grids (radius 1 ball attack)
 */
bool door_creation(void)
{
    int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;
    return (project(0, 1, p_ptr->pos.y, p_ptr->pos.x, 0, GF_MAKE_DOOR, flg));
}


bool trap_creation(int y, int x)
{
    int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;
    return (project(0, 1, y, x, 0, GF_MAKE_TRAP, flg));
}


bool tree_creation(void)
{
    int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;
    return (project(0, 1, p_ptr->pos.y, p_ptr->pos.x, 0, GF_MAKE_TREE, flg));
}


bool glyph_creation(void)
{
    int flg = PROJECT_GRID | PROJECT_ITEM;
    return (project(0, 1, p_ptr->pos.y, p_ptr->pos.x, 0, GF_MAKE_GLYPH, flg));
}


bool wall_stone(void)
{
    int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;

    bool dummy = (project(0, 1, p_ptr->pos.y, p_ptr->pos.x, 0, GF_MAKE_WALL, flg));

    /* Update stuff */
    p_ptr->update |= (PU_FLOW);

    /* Redraw map */
    p_ptr->redraw |= (PR_MAP);

    return dummy;
}


bool destroy_doors_touch(void)
{
    int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;
    return (project(0, 1, p_ptr->pos.y, p_ptr->pos.x, 0, GF_KILL_DOOR, flg));
}


bool sleep_monsters_touch(void)
{
    int flg = PROJECT_KILL | PROJECT_HIDE;
    return (project(0, 1, p_ptr->pos.y, p_ptr->pos.x, p_ptr->lev, GF_OLD_SLEEP, flg));
}


bool animate_dead(int who, int y, int x)
{
    int flg = PROJECT_ITEM | PROJECT_HIDE;
    return (project(who, 5, y, x, 0, GF_ANIM_DEAD, flg));
}


void call_chaos(int pct)
{
    int chaos_type, dummy, dir;
    int plev = p_ptr->lev;
    bool line_chaos = FALSE;

    int hurt_types[31] =
    {
        GF_ELEC,      GF_POIS,    GF_ACID,    GF_COLD,
        GF_FIRE,      GF_MISSILE, GF_ARROW,   GF_PLASMA,
        GF_HOLY_FIRE, GF_WATER,   GF_LITE,    GF_DARK,
        GF_FORCE,     GF_INERT, GF_MANA,    GF_METEOR,
        GF_ICE,       GF_CHAOS,   GF_NETHER,  GF_DISENCHANT,
        GF_SHARDS,    GF_SOUND,   GF_NEXUS,   GF_CONFUSION,
        GF_TIME,      GF_GRAVITY, GF_ROCKET,  GF_NUKE,
        GF_HELL_FIRE, GF_DISINTEGRATE, GF_PSY_SPEAR
    };

    chaos_type = hurt_types[randint0(31)];
    if (one_in_(4)) line_chaos = TRUE;

    if (one_in_(6))
    {
        for (dummy = 1; dummy < 10; dummy++)
        {
            if (dummy - 5)
            {
                if (line_chaos)
                    fire_beam(chaos_type, dummy, 150*pct/100);
                else
                    fire_ball(chaos_type, dummy, 150*pct/100, 2);
            }
        }
    }
    else if (one_in_(3))
    {
        fire_ball(chaos_type, 0, 500*pct/100, 8);
    }
    else
    {
        if (!get_fire_dir(&dir)) return;
        if (line_chaos)
            fire_beam(chaos_type, dir, 250*pct/100);
        else
            fire_ball(chaos_type, dir, 250*pct/100, 3 + (plev / 35));
    }
}


/*
 * Activate the evil Topi Ylinen curse
 * rr9: Stop the nasty things when a Cyberdemon is summoned
 * or the player gets paralyzed.
 */
bool activate_ty_curse(bool stop_ty, int *count)
{
    int     i = 0;

    int flg = (PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | PROJECT_JUMP);

    if (statistics_hack) return TRUE;

    do
    {
        switch (randint1(34))
        {
        case 28: case 29:
            if (!(*count))
            {
                msg_print("The ground trembles...");

                earthquake(p_ptr->pos, 5 + randint0(10));
                if (!one_in_(6)) break;
            }
        case 30: case 31:
            if (!(*count))
            {
                int dam = damroll(10, 10);
                msg_print("A portal opens to a plane of raw mana!");

                project(0, 8, p_ptr->pos.y, p_ptr->pos.x, dam, GF_MANA, flg);
                take_hit(DAMAGE_NOESCAPE, dam, "released pure mana");
                if (!one_in_(6)) break;
            }
        case 32: case 33:
            if (!(*count))
            {
                msg_print("Space warps about you!");

                teleport_player(damroll(10, 10), TELEPORT_PASSIVE);
                if (randint0(13)) (*count) += activate_hi_summon(p_ptr->pos.y, p_ptr->pos.x, FALSE);
                if (!one_in_(6)) break;
            }
        case 34:
            msg_print("You feel a surge of energy!");

            wall_breaker();
            if (!randint0(7))
            {
                project(0, 7, p_ptr->pos.y, p_ptr->pos.x, 50, GF_KILL_WALL, flg);
                take_hit(DAMAGE_NOESCAPE, 50, "surge of energy");
            }
            if (!one_in_(6)) break;
        case 1: case 2: case 3: case 16: case 17:
            aggravate_monsters(0);
            if (!one_in_(6)) break;
        case 4: case 5: case 6:
            (*count) += activate_hi_summon(p_ptr->pos.y, p_ptr->pos.x, FALSE);
            if (!one_in_(6)) break;
        case 7: case 8: case 9: case 18:
            (*count) += summon_specific(0, p_ptr->pos, cave->difficulty, 0, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_NO_PET));
            if (!one_in_(6)) break;
        case 10: case 11: case 12:
            msg_print("You feel your life draining away...");

            lose_exp(p_ptr->exp / 16);
            if (!one_in_(6)) break;
        case 13: case 14: case 15: case 19: case 20:
            if (stop_ty || (p_ptr->free_act && randint1(125) < p_ptr->skills.sav))
            {
                /* Do nothing */ ;
            }
            else
            {
                msg_print("You feel like a statue!");

                if (p_ptr->free_act)
                {
                    plr_tim_add(T_PARALYZED, randint1(2));
                    equip_learn_flag(OF_FREE_ACT);
                }
                else
                    plr_tim_add(T_PARALYZED, randint1(13));
                stop_ty = TRUE;
            }
            if (!one_in_(6)) break;
        case 21: case 22: case 23:
            (void)do_dec_stat(randint0(6));
            if (!one_in_(6)) break;
        case 24:
            msg_print("Huh? Who am I? What am I doing here?");

            lose_all_info();
            if (!one_in_(6)) break;
        case 25:
            /*
             * Only summon Cyberdemons deep in the dungeon.
             */
            if ((cave->difficulty > 65) && !stop_ty)
            {
                (*count) += summon_cyber(-1, p_ptr->pos.y, p_ptr->pos.x);
                stop_ty = TRUE;
                break;
            }
            if (!one_in_(6)) break;
        default:
            while (i < 6)
            {
                do
                {
                    (void)do_dec_stat(i);
                }
                while (one_in_(2));

                i++;
            }
        }
    }
    while (one_in_(3) && !stop_ty);

    return stop_ty;
}


int activate_hi_summon(int y, int x, bool can_pet)
{
    point_t pos = point_create(x, y);
    int i;
    int count = 0;
    int summon_lev;
    u32b mode = PM_ALLOW_GROUP;
    bool pet = FALSE;

    if (can_pet)
    {
        if (one_in_(4))
        {
            mode |= PM_FORCE_FRIENDLY;
        }
        else
        {
            mode |= PM_FORCE_PET;
            pet = TRUE;
        }
    }

    if (!pet) mode |= PM_NO_PET;

    summon_lev = (pet ? p_ptr->lev * 2 / 3 + randint1(p_ptr->lev / 2) : cave->difficulty);

    for (i = 0; i < (randint1(7) + (cave->difficulty / 40)); i++)
    {
        int j, ct = 0;
        /* Make sure we get something on each roll ... */
        for (j = 0; j < 1000 && !ct; j++)
        {
            switch (randint1(25) + (cave->difficulty / 20))
            {
                case 1: case 2:
                    ct = summon_specific((pet ? -1 : 0), pos, summon_lev, SUMMON_ANT, mode);
                    break;
                case 3: case 4:
                    ct = summon_specific((pet ? -1 : 0), pos, summon_lev, SUMMON_SPIDER, mode);
                    break;
                case 5: case 6:
                    ct = summon_specific((pet ? -1 : 0), pos, summon_lev, SUMMON_HOUND, mode);
                    break;
                case 7: case 8:
                    ct = summon_specific((pet ? -1 : 0), pos, summon_lev, SUMMON_HYDRA, mode);
                    break;
                case 9: case 10:
                    ct = summon_specific((pet ? -1 : 0), pos, summon_lev, SUMMON_ANGEL, mode);
                    break;
                case 11: case 12:
                    ct = summon_specific((pet ? -1 : 0), pos, summon_lev, SUMMON_UNDEAD, mode);
                    break;
                case 13: case 14:
                    ct = summon_specific((pet ? -1 : 0), pos, summon_lev, SUMMON_DRAGON, mode);
                    break;
                case 15: case 16:
                    ct = summon_specific((pet ? -1 : 0), pos, summon_lev, SUMMON_DEMON, mode);
                    break;
                case 17:
                    if (can_pet) break;
                    ct = summon_specific((pet ? -1 : 0), pos, summon_lev, SUMMON_AMBERITE, (mode | PM_ALLOW_UNIQUE));
                    break;
                case 18: case 19:
                    if (can_pet) break;
                    ct = summon_specific((pet ? -1 : 0), pos, summon_lev, SUMMON_UNIQUE, (mode | PM_ALLOW_UNIQUE));
                    break;
                case 20: case 21:
                    if (!can_pet) mode |= PM_ALLOW_UNIQUE;
                    ct = summon_specific((pet ? -1 : 0), pos, summon_lev, SUMMON_HI_UNDEAD, mode);
                    break;
                case 22: case 23:
                    if (!can_pet) mode |= PM_ALLOW_UNIQUE;
                    ct = summon_specific((pet ? -1 : 0), pos, summon_lev, SUMMON_HI_DRAGON, mode);
                    break;
                case 24:
                    ct = summon_specific((pet ? -1 : 0), pos, 100, SUMMON_CYBER, mode);
                    break;
                default:
                    if (!can_pet) mode |= PM_ALLOW_UNIQUE;
                    ct = summon_specific((pet ? -1 : 0), pos,pet ? summon_lev : (((summon_lev * 3) / 2) + 5), 0, mode);
            }
        }
        count += ct;
    }

    return count;
}


/* ToDo: check */
int summon_cyber(int who, int y, int x)
{
    point_t pos = point_create(x, y);
    int i;
    int max_cyber = cave->difficulty / 50 + randint1(2);
    int count = 0;
    u32b mode = PM_ALLOW_GROUP;

    /* Summoned by a monster */
    if (who > 0)
    {
        monster_type *m_ptr = dun_mon(cave, who);
        if (is_pet(m_ptr)) mode |= PM_FORCE_PET;
    }

    if (max_cyber > 4) max_cyber = 4;

    for (i = 0; i < max_cyber; i++)
    {
        count += summon_specific(who, pos, 100, SUMMON_CYBER, mode);
    }

    return count;
}


void wall_breaker(void)
{
    if (randint1(80 + p_ptr->lev) < 70)
    {
        point_t pos;
        int attempts = 1000;
        while (attempts--)
        {
            pos = scatter(p_ptr->pos, 4);
            if (!cave_have_flag_at(pos, FF_PROJECT)) continue;
            if (!plr_at(pos)) break;
        }
        project(0, 0, pos.y, pos.x, 20 + randint1(30), GF_KILL_WALL,
                  (PROJECT_BEAM | PROJECT_THRU | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL));
    }
    else if (randint1(100) > 30)
    {
        earthquake(p_ptr->pos, 1);
    }
    else
    {
        int num = damroll(5, 3);
        int i;

        for (i = 0; i < num; i++)
        {
            point_t pos;
            while (1)
            {
                pos = scatter(p_ptr->pos, 10);
                if (!plr_at(pos)) break;
            }
            project(0, 0, pos.y, pos.x, 20 + randint1(30), GF_KILL_WALL,
                      (PROJECT_BEAM | PROJECT_THRU | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL));
        }
    }
}


/*
 * Confuse monsters
 */
bool confuse_monsters(int dam)
{
    return (project_los(GF_OLD_CONF, dam));
}


/*
 * Charm monsters
 */
bool charm_monsters(int dam)
{
    return (project_los(GF_CHARM, dam));
}


/*
 * Charm animals
 */
bool charm_animals(int dam)
{
    return (project_los(GF_CONTROL_ANIMAL, dam));
}


/*
 * Stun monsters
 */
bool stun_monsters(int dam)
{
    return (project_los(GF_STUN, dam));
}


/*
 * Stasis monsters
 */
bool stasis_monsters(int dam)
{
    return (project_los(GF_STASIS, dam));
}


/*
 * Mindblast monsters
 */
bool mindblast_monsters(int dam)
{
    return (project_los(GF_PSI, dam));
}


/*
 * Banish all monsters
 */
bool banish_monsters(int dist)
{
    return (project_los(GF_AWAY_ALL, dist));
}


/*
 * Turn evil
 */
bool turn_evil(int dam)
{
    return (project_los(GF_TURN_EVIL, dam));
}


/*
 * Turn everyone
 */
bool turn_monsters(int dam)
{
    return (project_los(GF_TURN_ALL, dam));
}


/*
 * Death-ray all monsters (note: OBSCENELY powerful)
 */
bool deathray_monsters(void)
{
    return (project_los(GF_DEATH_RAY, p_ptr->lev * 200));
}


bool charm_monster(int dir, int plev)
{
    int flg = PROJECT_STOP | PROJECT_KILL;
    return (project_hook(GF_CHARM, dir, plev, flg));
}


bool control_one_undead(int dir, int plev)
{
    int flg = PROJECT_STOP | PROJECT_KILL;
    return (project_hook(GF_CONTROL_UNDEAD, dir, plev, flg));
}


bool control_one_demon(int dir, int plev)
{
    int flg = PROJECT_STOP | PROJECT_KILL;
    return (project_hook(GF_CONTROL_DEMON, dir, plev, flg));
}


bool charm_animal(int dir, int plev)
{
    int flg = PROJECT_STOP | PROJECT_KILL;
    return (project_hook(GF_CONTROL_ANIMAL, dir, plev, flg));
}


bool charm_living(int dir, int plev)
{
    int flg = PROJECT_STOP | PROJECT_KILL;
    return (project_hook(GF_CONTROL_LIVING, dir, plev, flg));
}


bool kawarimi(bool success)
{
    int y, x;

    if (p_ptr->is_dead) return FALSE;
    if (plr_tim_find(T_CONFUSED) || plr_tim_find(T_BLIND) || plr_tim_find(T_PARALYZED) || plr_tim_find(T_HALLUCINATE)) return FALSE;
    if (randint0(200) < plr_tim_amount(T_STUN)) return FALSE;

    if (!success && one_in_(3))
    {
        msg_print("Failed! You could not escape.");
        p_ptr->special_defense &= ~(NINJA_KAWARIMI);
        p_ptr->redraw |= (PR_STATUS);
        return FALSE;
    }

    y = p_ptr->pos.y;
    x = p_ptr->pos.x;

    teleport_player(10 + randint1(90), 0L);

    if (p_ptr->pclass == CLASS_NINJA)
    {
        object_type forge;
        object_wipe(&forge);
        object_prep(&forge, lookup_kind(TV_STATUE, SV_WOODEN_STATUE));
        forge.pval = MON_NINJA;
        drop_near(&forge, point_create(x, y), -1);
    }

    if (success) msg_print("You have escaped just before the attack hit you.");
    else msg_print("Failed! You are hit by the attack.");

    p_ptr->special_defense &= ~(NINJA_KAWARIMI);
    p_ptr->redraw |= (PR_STATUS);

    return TRUE;
}


/*
 * "Rush Attack" routine for Samurai or Ninja
 * Return value is for checking "done"
 * Hacked up for Duelist as well.
 */
bool rush_attack(int rng, bool *mdeath)
{
    int dir;
    int tx, ty;
    int tm_idx = 0;
    point_t path[100];
    int path_n, i;
    bool tmp_mdeath = FALSE;
    bool moved = FALSE;

    if (mdeath) *mdeath = FALSE;

    project_length = rng;

    /* Mega Hack for the Duelist */
    if (p_ptr->pclass == CLASS_DUELIST)
    {
        /* assert(p_ptr->duelist_target_idx); */
        tm_idx = p_ptr->duelist_target_idx;
        tx = dun_mon(cave, tm_idx)->pos.x;
        ty = dun_mon(cave, tm_idx)->pos.y;

        if (!los(ty, tx, p_ptr->pos.y, p_ptr->pos.x))
        {
            msg_format("%^s is not in your line of sight.", duelist_current_challenge());
            return FALSE;
        }
    }
    else
    {
        if (!get_fire_dir(&dir)) return FALSE;

        /* Use the given direction */
        tx = p_ptr->pos.x + project_length * ddx[dir];
        ty = p_ptr->pos.y + project_length * ddy[dir];

        /* Hack -- Use an actual "target" */
        if ((dir == 5) && target_okay())
        {
            tx = target_col;
            ty = target_row;
        }

        if (in_bounds(ty, tx))
        {
            mon_ptr mon = mon_at_xy(tx, ty);
            if (mon) tm_idx = mon->id;
        }
    }

    path_n = project_path(path, project_length, p_ptr->pos, point_create(tx, ty), PROJECT_STOP | PROJECT_KILL);
    project_length = 0;

    /* No need to move */
    if (!path_n) return TRUE;

    /* Use ty and tx as to-move point */
    ty = p_ptr->pos.y;
    tx = p_ptr->pos.x;

    /* Project along the path */
    for (i = 0; i < path_n; i++)
    {
        monster_type *m_ptr;

        int ny = path[i].y;
        int nx = path[i].x;

        if (cave_empty_bold(ny, nx) && player_can_enter(cave_at_xy(nx, ny)->feat, 0))
        {
            ty = ny;
            tx = nx;

            /* Go to next grid */
            continue;
        }

        m_ptr = mon_at_xy(nx, ny);
        if (!m_ptr)
        {
            if (tm_idx)
            {
                msg_print("Failed!");
            }
            else
            {
                msg_print("You can't move to that place.");
            }

            /* Exit loop */
            break;
        }

        /* Move player before updating the monster */
        if (!player_bold(ty, tx)) teleport_player_to(ty, tx, TELEPORT_NONMAGICAL);

        /* Update the monster */
        update_mon(m_ptr, TRUE);

        if (tm_idx != m_ptr->id)
        {
            msg_format("There is %s in the way!", m_ptr->ml ? (tm_idx ? "another monster" : "a monster") : "someone");
        }
        else if (!player_bold(ty, tx))
        {
            char m_name[80];
            monster_desc(m_name, m_ptr, 0);
            msg_format("You quickly jump in and attack %s!", m_name);
        }

        if (!player_bold(ty, tx)) teleport_player_to(ty, tx, TELEPORT_NONMAGICAL);
        moved = TRUE;
        {
            plr_attack_t ctx = {0};
            ctx.mode = PLR_HIT_RUSH_ATTACK;
            plr_attack(&ctx, point_create(nx, ny));
            tmp_mdeath = BOOL(ctx.stop == STOP_MON_DEAD);
        }
        break;
    }

    if (!moved && !player_bold(ty, tx)) teleport_player_to(ty, tx, TELEPORT_NONMAGICAL);
    if (mdeath) *mdeath = tmp_mdeath;
    return TRUE;
}


