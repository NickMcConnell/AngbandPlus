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

    plr->knowledge |= (KNOW_STAT | KNOW_HPRATE);
    virtue_add(VIRTUE_KNOWLEDGE, 1);
    virtue_add(VIRTUE_ENLIGHTENMENT, 1);

    doc_printf(doc, "Your current Life Rating is <color:G>%d%%</color>.\n\n", life_rating());
    doc_insert(doc, "<color:r>Limits of maximum stats</color>\n");
    for (i = 0; i < MAX_STATS; i++)
    {
        doc_printf(doc, "%s <color:G>18/%d</color>\n", stat_names[i], plr->stat_max_max[i]-18);
    }
    doc_insert(doc, "\n\n");

    virtue_display(doc, TRUE);
    doc_insert(doc, "\n\n");

    doc_insert(doc, "<color:G>Resistances:</color>\n");
    for (i = GF_RES_MIN; i <= GF_RES_MAX; i++)
    {
        int pct = res_pct(i);
        if (pct != 0)
        {
            if (i == GF_FEAR)
                doc_printf(doc, "You are %s to %s.\n", pct > 0 ? "resistant" : "vulnerable", res_name(i));
            else
                doc_printf(doc, "You are %d%% %s to %s.\n", ABS(pct), pct > 0 ? "resistant" : "vulnerable", res_name(i));
        }
    }
    doc_insert(doc, "\n\n");

    if (plr->afraid || plr_tim_count())
    {
        doc_insert(doc, "<color:G>Timed Effects:</color>\n");
        if (plr->afraid)
            doc_insert(doc, "You are terrified.\n");
        plr_tim_self_knowledge(doc);
        doc_insert(doc, "\n\n");
    }

    if (plr->cursed)
    {
        doc_insert(doc, "<color:G>Curses:</color>\n");
        if (plr->cursed & OFC_TY_CURSE)
            doc_insert(doc, "You carry an ancient foul curse.\n");
        if (plr->cursed & OFC_AGGRAVATE)
            doc_insert(doc, "You aggravate monsters.\n");
        if (plr->cursed & OFC_DRAIN_EXP)
            doc_insert(doc, "You are drained.\n");
        if (plr->cursed & OFC_SLOW_REGEN)
            doc_insert(doc, "You regenerate slowly.\n");
        if (plr->cursed & OFC_ADD_L_CURSE)
            doc_insert(doc, "Your weak curses multiply.\n");
        if (plr->cursed & OFC_ADD_H_CURSE)
            doc_insert(doc, "Your heavy curses multiply.\n");
        if (plr->cursed & OFC_CALL_ANIMAL)
            doc_insert(doc, "You attract animals.\n");
        if (plr->cursed & OFC_CALL_DEMON)
            doc_insert(doc, "You attract demons.\n");
        if (plr->cursed & OFC_CALL_DRAGON)
            doc_insert(doc, "You attract dragons.\n");
        if (plr->cursed & OFC_COWARDICE)
            doc_insert(doc, "You are subject to cowardice.\n");
        if (plr->cursed & OFC_TELEPORT)
            doc_insert(doc, "Your position is very uncertain.\n");
        if (plr->cursed & OFC_LOW_MELEE)
            doc_insert(doc, "Your weapon causes you to miss blows.\n");
        if (plr->cursed & OFC_LOW_AC)
            doc_insert(doc, "You are subject to be hit.\n");
        if (plr->cursed & OFC_LOW_MAGIC)
            doc_insert(doc, "You are subject to fail spellcasting.\n");
        if (plr->cursed & OFC_FAST_DIGEST)
            doc_insert(doc, "You have a good appetite.\n");
        if (plr->cursed & OFC_DRAIN_HP)
            doc_insert(doc, "You are drained.\n");
        if (plr->cursed & OFC_DRAIN_MANA)
            doc_insert(doc, "You brain is drained.\n");
        doc_insert(doc, "\n\n");
    }

    doc_insert(doc, "<color:G>Abilities:</color>\n");
    if (plr->special_attack & ATTACK_CONFUSE)
        doc_insert(doc, "Your hands are glowing dull red.\n");
    if (plr->action == ACTION_SEARCH)
        doc_insert(doc, "You are looking around very carefully.\n");
    if (plr->new_spells)
        doc_insert(doc, "You can learn some spells/prayers.\n");
    if (plr->see_infra)
        doc_insert(doc, "Your eyes are sensitive to infrared light.\n");
    if (plr->see_nocto == DUN_VIEW_MAX)
        doc_insert(doc, "You can see in the dark.\n");
    else if (plr->see_nocto)
        doc_printf(doc, "You can see in the dark (%d').\n", plr->see_nocto * 10);
    if (plr->see_inv)
        doc_insert(doc, "You can see invisible creatures.\n");
    if (plr->levitation)
        doc_insert(doc, "You can fly.\n");
    if (plr->free_act)
        doc_insert(doc, "You have free action.\n");
    if (plr->pass_web)
        doc_insert(doc, "You are unhampered by sticky webs.\n");
    if (plr->pass_tree)
        doc_insert(doc, "You are unhampered by thick foliage.\n");
    if (plr->regen > 100)
        doc_insert(doc, "You regenerate quickly.\n");
    if (plr->slow_digest)
        doc_insert(doc, "Your appetite is small.\n");
    if (plr->telepathy)
        doc_insert(doc, "You have ESP.\n");
    if (plr->esp_animal)
        doc_insert(doc, "You sense natural creatures.\n");
    if (plr->esp_undead)
        doc_insert(doc, "You sense undead.\n");
    if (plr->esp_demon)
        doc_insert(doc, "You sense demons.\n");
    if (plr->esp_orc)
        doc_insert(doc, "You sense orcs.\n");
    if (plr->esp_troll)
        doc_insert(doc, "You sense trolls.\n");
    if (plr->esp_giant)
        doc_insert(doc, "You sense giants.\n");
    if (plr->esp_dragon)
        doc_insert(doc, "You sense dragons.\n");
    if (plr->esp_human)
        doc_insert(doc, "You sense humans.\n");
    if (plr->esp_evil)
        doc_insert(doc, "You sense evil creatures.\n");
    if (plr->esp_good)
        doc_insert(doc, "You sense good creatures.\n");
    if (plr->esp_nonliving)
        doc_insert(doc, "You sense non-living creatures.\n");
    if (plr->esp_unique)
        doc_insert(doc, "You sense unique monsters.\n");
    if (plr->hold_life)
        doc_insert(doc, "You have a firm hold on your life force.\n");
    if (plr->reflect)
        doc_insert(doc, "You reflect arrows and bolts.\n");
    if (plr->sh_fire)
        doc_insert(doc, "You are surrounded with a fiery aura.\n");
    if (plr->sh_shards)
        doc_insert(doc, "You are surrounded with a shard aura.\n");
    if (plr->sh_elec)
        doc_insert(doc, "You are surrounded with electricity.\n");
    if (plr->sh_cold)
        doc_insert(doc, "You are surrounded with an aura of coldness.\n");
    if (plr->anti_magic)
        doc_insert(doc, "You are surrounded by an anti-magic shell.\n");
    if (plr->anti_tele)
        doc_insert(doc, "You cannot teleport.\n");
    if (plr->warning)
        doc_insert(doc, "You will be warned before dangerous actions.\n");
    if (plr->dec_mana)
        doc_insert(doc, "You can cast spells with fewer mana points.\n");
    if (plr->easy_spell)
        doc_insert(doc, "Fail rate of your magic is decreased.\n");
    if (plr->heavy_spell)
        doc_insert(doc, "Fail rate of your magic is increased.\n");
    if (plr->mighty_throw)
        doc_insert(doc, "You can throw objects powerfully.\n");

    if (plr->sustain_str)
        doc_insert(doc, "Your strength is sustained.\n");
    if (plr->sustain_int)
        doc_insert(doc, "Your intelligence is sustained.\n");
    if (plr->sustain_wis)
        doc_insert(doc, "Your wisdom is sustained.\n");
    if (plr->sustain_con)
        doc_insert(doc, "Your constitution is sustained.\n");
    if (plr->sustain_dex)
        doc_insert(doc, "Your dexterity is sustained.\n");
    if (plr->sustain_chr)
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
    if (plr->no_eldritch)
        doc_insert(doc, "You are unaffected by the Eldritch Horror.\n");
    if (plr->no_cut)
        doc_insert(doc, "You cannot be cut.\n");
    if (plr->no_charge_drain)
        doc_insert(doc, "You are immune to charge draining attacks.\n");
    if (plr->auto_id)
        doc_insert(doc, "Objects are automatically identified as you pass over them.\n");
    else if (plr->auto_pseudo_id)
        doc_insert(doc, "Objects are automatically pseudo-identified as you pass over them.\n");
    if (plr->cult_of_personality)
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

    if (plr->afraid)
        doc_printf(doc, "<color:R>%-18.18s</color> %3d <indent>%s</indent>\n", "Fear", plr->afraid, "You are afraid.");
    if (plr->special_attack & ATTACK_CONFUSE)
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
static cell_p _detect_p;
static bool _detect_known; /* XXX using a known device; don't leak -DetectTraps via CELL_UNSAFE XXX */
static void _detect_feat_grid(point_t pos, dun_grid_ptr grid)
{
    int dist = plr_distance(pos);

    if (dist > _detect_range) return;

    /* Detect Traps marks every grid as detected for display purposes.
     * The player is also warned when leaving a trap detected region */
    if ( (_detect_p == floor_has_trap || _detect_p == floor_has_secret_trap)
      && _detect_known )
    {
        if (dist <= _detect_range - 1)
            grid->flags |= CELL_DETECT;

        grid->flags &= ~CELL_UNSAFE;
        draw_pos(pos);
    }

    if (_detect_p && !_detect_p(grid)) return;

    cell_detect(cave, pos, grid);
    grid->flags |= (CELL_MAP | CELL_AWARE);
    draw_pos(pos);
    _detect = TRUE;
}
static bool detect_feat_p(int range, cell_p p, bool known)
{
    rect_t r = rect_create_centered(plr->pos, range, range);

    _detect = FALSE;
    _detect_range = range;
    _detect_p = p;
    _detect_known = known;

    dun_iter_rect(cave, r, _detect_feat_grid);

    if (p == floor_has_trap && !view_unsafe_grids)
        plr->redraw |= PR_STATUS;

    return _detect;
}
static bool _music_check(int threshold)
{
    /* this check prevents multiple messages while maintaining the song
     * of detection (Clairaudience) ... only msg on the first turn */
    return (plr_tim_find(T_SONG_DETECT) && music_duration() > threshold)
        || (plr_tim_find(T_BLESS_REVELATION) && plr_tim_amount(T_BLESS_REVELATION) - 1 > threshold);
}
bool detect_traps(int range, bool known)
{
    bool detect = detect_feat_p(range, floor_has_trap, known);
    if (_music_check(0)) detect = FALSE;
    if (detect)
        msg_print("You sense the presence of traps!");
    return detect;
}
static bool _cell_is_door(dun_cell_ptr cell) { return cell_is_door(cell) || wall_has_secret_door(cell); }
bool detect_doors(int range)
{
    bool detect = detect_feat_p(range, _cell_is_door, TRUE);
    if (_music_check(0)) detect = FALSE;
    if (detect)
        msg_print("You sense the presence of doors!");
    return detect;
}
bool detect_stairs(int range)
{
    bool detect = detect_feat_p(range, cell_is_stairs, TRUE);
    if (_music_check(0)) detect = FALSE;
    if (detect)
        msg_print("You sense the presence of stairs!");
    return detect;
}
bool detect_recall(int range)
{
    bool detect = detect_feat_p(range, portal_is_recall, TRUE);
    if (_music_check(0)) detect = FALSE;
    if (detect)
        msg_print("You sense the presence of a way back home!");
    return detect;
}
bool detect_treasure(int range)
{
    bool detect = detect_feat_p(range, wall_has_treasure, TRUE);
    if (_music_check(6)) detect = FALSE;
    if (detect)
        msg_print("You sense the presence of buried treasure!");
    return detect;
}
bool detect_secret_doors(int range)
{
    bool detect = detect_feat_p(range, wall_has_secret_door, TRUE);
    if (detect)
        msg_print("You sense the presence of secret doors!");
    return detect;
}
bool detect_secret_traps(int range)
{
    bool detect = detect_feat_p(range, floor_has_secret_trap, TRUE);
    if (detect)
        msg_print("You sense the presence of secret traps!");
    return detect;
}

/* Object Detection: */
static obj_p _detect_obj_filter;
static void _detect_pile(point_t pos, obj_ptr pile)
{
    obj_ptr obj;
    if (point_distance(pos, plr->pos) > _detect_range) return;
    for (obj = pile; obj; obj = obj->next)
    {
        if (_detect_obj_filter && !_detect_obj_filter(obj)) continue;
        obj->marked |= OM_FOUND;
        draw_pos(pos);
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
        if (_music_check(6)) {}
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
        if (_music_check(6)) {}
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
    if (point_distance(mon->pos, plr->pos) > _detect_range) return;
    if (_detect_mon_filter && !_detect_mon_filter(mon)) return;
    if (plr->monster_race_idx == mon->race->id)
        plr->window |= (PW_MONSTER);
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
    return !mon_is_invisible(mon) || plr->see_inv;
}
bool detect_monsters_normal(int range)
{
    bool detect = FALSE;
    if (detect_mon_aux(_mon_normal_p, range))
    {
        if (_music_check(3)) {}
        else msg_print("You sense the presence of monsters!");
        detect = TRUE;
    }
    return detect;
}
bool detect_monsters_invis(int range)
{
    bool detect = FALSE;
    if (detect_mon_aux(mon_is_invisible, range))
    {
        if (_music_check(3)) {}
        else msg_print("You sense the presence of invisible creatures!");
        detect = TRUE;
    }
    return detect;
}
static bool _mon_evil_p(mon_ptr mon)
{
    if (mon_is_evil(mon))
    {
        mon_lore_align(mon);
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
    return !mon_has_empty_mind(mon);
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
    return mon_is_char_ex(mon, _mon_match_string);
}
bool detect_monsters_string(int range, cptr match)
{
    bool detect = FALSE;
    _mon_match_string = match;
    if (detect_mon_aux(_mon_match_p, range))
    {
        if (_music_check(3)) {}
        else msg_print("You sense the presence of monsters!");
        detect = TRUE;
    }
    return detect;
}
bool detect_monsters_mimic(int range)
{
    bool detect = FALSE;
    _mon_match_string = "!=?|/`";
    if (detect_mon_aux(_mon_match_p, range))
    {
        if (_music_check(3)) {}
        else msg_print("You sense the presence of mimics!");
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
 * Wake up all monsters, and speed up "los" monsters.
 */
void aggravate_monsters(who_t who)
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
        if (mon == who_mon(who)) continue;

        /* Wake up nearby sleeping monsters */
        if (mon->cdis < MAX_SIGHT * 2)
        {
            if (mon_tim_find(mon, MT_SLEEP))
            {
                mon_tim_remove(mon, MT_SLEEP);
                sleep = TRUE;
            }
            if (!mon_is_pet(mon)) mon->mflag2 |= MFLAG2_NOPET;
        }

        /* Speed up monsters in line of sight */
        if (plr_view(mon->pos))
        {
            if (!mon_is_pet(mon))
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
    if (plr->riding) plr->update |= PU_BONUS;
}


/*
 * Delete a non-unique/non-quest monster
 */
bool genocide_aux(mon_ptr m_ptr, int power, bool player_cast, int dam_side, cptr spell_name)
{
    monster_race *r_ptr = m_ptr->race;
    bool         resist = FALSE;

    if (mon_is_pet(m_ptr) && !player_cast) return FALSE;

    /* Hack -- Skip Unique Monsters or Quest Monsters */
    if (mon_race_is_unique(r_ptr)) resist = TRUE;
    else if (m_ptr->mflag2 & MFLAG2_QUESTOR) resist = TRUE;
    else if (m_ptr->id == plr->riding) resist = TRUE;
    else if (cave->flags & DF_NO_GENOCIDE) resist = TRUE;
    else if (player_cast && (r_ptr->alloc.lvl > randint0(power))) resist = TRUE;
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
        if (mon_is_friendly(m_ptr) && !mon_is_pet(m_ptr))
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
    move_cursor_relative(plr->pos);

    /* Redraw */
    plr->redraw |= (PR_HP);

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
static bool _mon_genocide(mon_ptr mon) { return mon_is_char(mon, _genocide_char); }
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
bool destroy_area(point_t center, int r, int power)
{
    point_t d;
    bool    flag = FALSE;
    bool    in_generate = power < 0;
    obj_ptr obj;

    /* Prevent destruction of quest levels and town */
    if (cave->flags & DF_NO_DESTRUCT) return FALSE;

    /* Big area of affect */
    for (d.y = -r; d.y <= r; d.y++)
    {
        for (d.x = -r; d.x <= r; d.x++)
        {
            point_t p = point_add(center, d);
            dun_cell_ptr cell;
            mon_ptr mon;

            if (!dun_pos_interior(cave, p)) continue;
            if (point_distance(center, p) > r) continue;

            cell = dun_cell_at(cave, p);
            cell->flags &= ~(CELL_ROOM | CELL_VAULT);
            cell->flags &= ~(CELL_MAP | CELL_LIT);

            if (!in_generate) /* Normal */
            {
                cell->flags &= ~(CELL_UNSAFE | CELL_AWARE);
                if (dun_plr_at(cave, p))
                {
                    /* Hurt the player later */
                    flag = TRUE;

                    /* Do not hurt this grid */
                    continue;
                }
            }

            /* Hack -- Skip the epicenter */
            if (point_equals(p, center)) continue;

            mon = dun_mon_at(cave, p);
            if (mon)
            {
                if (in_generate) delete_monster_at(p);
                else
                {
                    bool resist = FALSE;

                    if (have_flag(mon->smart, SM_SUMMONED))
                    {
                        /* XXX Redress the game balance wrt to summoning. We might consider
                         * some sort of odds here, but let's revert Destruction completely
                         * to the way it worked in Hengband. Note that Genocide always had
                         * a save in heng ... I copied this mechanic for Destruction. */
                        resist = FALSE;
                    }
                    else if (mon->mflag2 & MFLAG2_NODESTRUCT) resist = TRUE;
                    else if ((mon->mflag2 & MFLAG2_VAULT) && !one_in_(3)) resist = TRUE;
                    else if (mon_lvl(mon) > randint0(power)) resist = TRUE;

                    if (resist)
                    {
                        bool see_m = mon_show_msg(mon);
                        char m_name[80];

                        monster_desc(m_name, mon, 0);

                        if (see_m)
                            msg_format("%^s is unaffected.", m_name);

                        mon_tim_remove(mon, MT_SLEEP);
                        if (mon_is_friendly(mon) && !mon_is_pet(mon))
                        {
                            if (see_m)
                                msg_format("%^s gets angry!", m_name);
                            set_hostile(mon);
                        }

                        if ( !(mon->mflag2 & MFLAG2_QUESTOR) /* Questors becoming immune to *destruct* can be advantageous! */
                          && !mon_can_multiply(mon)  /* Unmakers ... *shudder* */
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
                            if (!teleport_away(mon, (r * 2) + 1, TELEPORT_DEC_VALOUR)) continue;
                        }
                        else
                        {
                            delete_monster_at(p);
                        }
                    }
                }
            }

            for (obj = dun_obj_at(cave, p); obj; obj = obj->next)
            {
                if (!obj_is_known(obj) || in_generate)
                {
                    if (obj->art_id)
                        arts_lookup(obj->art_id)->generated = FALSE;
                    else if (obj->replacement_art_id)
                        arts_lookup(obj->replacement_art_id)->generated = FALSE;
                }
            }

            /* Delete objects */
            delete_object(p);

            /* Destroy "non-permanent" grids */
            if (!(cell->flags & CELL_PERM))
            {
                /* Wall (or floor) type */
                int t = randint0(200);

                if (!in_generate) /* Normal */
                {
                    if (t < 20)
                        dun_place_granite(cave, p);
                    else if (t < 70)
                        dun_place_quartz(cave, p);
                    else if (t < 100)
                        dun_place_magma(cave, p);
                    else
                        cave->type->place_floor(cave, p);
                }
                else /* In generation */
                {
                    if (t < 20)
                    {
                        cave->type->place_wall(cave, p);
                        cell->flags &= ~CELL_GEN_MASK;
                        cell->flags |= CELL_EXTRA;
                    }
                    else if (t < 70)
                        dun_place_quartz(cave, p);
                    else if (t < 100)
                        dun_place_magma(cave, p);
                    else
                        cave->type->place_floor(cave, p);
                }
            }
        }
    }

    if (!in_generate)
    {
        /* Hack -- Affect player */
        if (flag)
        {
            msg_print("There is a searing blast of light!");
            if (!res_save_default(GF_BLIND) && !res_save_default(GF_LIGHT))
                plr_tim_add(T_BLIND, 10 + _1d(10));
        }

        dun_forget_flow(cave);
        plr->update |= (PU_UN_VIEW | PU_UN_LIGHT);
        plr->update |= (PU_VIEW | PU_LIGHT | PU_FLOW | PU_MON_FLOW | PU_MON_LIGHT | PU_MONSTERS);

        if (weaponmaster_is_(WEAPONMASTER_DIGGERS))
            plr->update |= PU_BONUS;

        plr->redraw |= (PR_MAP);
        plr->window |= (PW_OVERHEAD | PW_DUNGEON);
    }

    return TRUE;
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
bool earthquake_aux(point_t center, int rad, int m_idx)
{
    point_t     p;
    dun_bmp_ptr bmp;
    rect_t      rect;

    /* Prevent destruction of quest levels and town */
    if (cave->flags & DF_NO_QUAKE) return FALSE;

    rad = MIN(12, rad);
    rect = rect_create_centered(center, rad, rad);
    rect = rect_intersect(cave->rect, rect);
    bmp = dun_bmp_alloc(cave, rect);

    /* randomly choose affected grids */
    for (p = rect_first(rect); rect_contains_point(rect, p); p = rect_next(rect, p))
    {
        dun_cell_ptr cell;
        if (!dun_pos_interior(cave, p)) continue;
        if (point_distance(center, p) > rad) continue;

        cell = dun_cell_at(cave, p);
        cell->flags &= ~(CELL_ROOM | CELL_VAULT | CELL_UNSAFE | CELL_AWARE);
        cell->flags &= ~(CELL_LIT | CELL_MAP);

        if (point_equals(p, center)) continue; /* skip epicenter */
        if (_1d(100) <= 85) continue;

        dun_bmp_set(bmp, p);
    }

    /* hurt and move plr (XXX if plr is actually on this level! XXX) */
    if (plr->dun_id == cave->id && dun_bmp_test(bmp, plr->pos) && !plr->pass_wall && !plr->kill_wall)
    {
        int i, sn = 0, damage = 0;
        point_t sp;

        for (i = 0; i < 8; i++) /* look for safety */
        {
            point_t p = point_step(plr->pos, ddd[i]);

            if (!dun_allow_plr_at(cave, p)) continue;
            if (dun_bmp_test(bmp, p)) continue;

            sn++;
            if (one_in_(sn)) sp = p;
        }

        switch (_1d(3))
        {
        case 1:
            msg_print("The cave ceiling collapses!");
            break;
        case 2:
            msg_print("The cave floor twists in an unnatural way!");
            break;
        default:
            msg_print("The cave quakes! You are pummeled with debris!");
        }

        if (!sn) /* crush plr if no safe pos */
        {
            if (!mut_present(MUT_EVASION) || one_in_(2))
            {
                msg_print("You are <color:v>severely crushed</color>!");
                damage = 50 + _3d(50);
                if (!res_save(GF_STUN, 100))
                    plr_tim_add(T_STUN, _7d(7));
            }
        }
        else
        {
            switch (_1d(3))
            {
            case 1:
                msg_print("You nimbly dodge the blast!");
                break;
            case 2:
                if (!mut_present(MUT_EVASION) || one_in_(2))
                {
                    msg_print("You are <color:R>bashed by rubble</color>!");
                    damage = _10d(4);
                    if (!res_save(GF_STUN, 100))
                        plr_tim_add(T_STUN, _5d(5));
                }
                break;
            default:
                if (!mut_present(MUT_EVASION) || one_in_(2))
                {
                    msg_print("You are <color:R>crushed between the floor and ceiling</color>!");
                    damage = _10d(4);
                    if (!res_save(GF_STUN, 100))
                        plr_tim_add(T_STUN, _5d(5));
                }
            }
            move_player_effect(sp, MPE_DONT_PICKUP);
        }

        /* Important -- no wall on player */
        dun_bmp_unset(bmp, plr->pos);

        if (damage)
        {
            cptr killer = "an earthquake";

            if (m_idx) /* XXX Morgoth has RBE_SHATTER and should get credit for the kill! */
            {
                char m_name[80];
                mon_ptr mon = dun_mon(cave, m_idx);

                monster_desc(m_name, mon, MD_IGNORE_HALLU | MD_ASSUME_VISIBLE | MD_INDEF_VISIBLE);
                killer = format("an earthquake caused by %s", m_name);
            }
            take_hit(DAMAGE_ATTACK, damage, killer);
        }
    }

    /* hurt and move monsters */
    for (p = rect_first(rect); rect_contains_point(rect, p); p = rect_next(rect, p))
    {
        mon_ptr mon;

        if (!dun_bmp_test(bmp, p)) continue;

        mon = dun_mon_at(cave, p);
        if (!mon || mon->id == plr->riding) continue;
        
        /* Quest monsters and Amberites (Blood Curse). Note that dun_delete_mon()
         * can only be called once per monster, and mon_take_hit() will do this 
         * in a bit for Amberites (after activate_ty_curse). */
        if ((mon->mflag2 & MFLAG2_QUESTOR) || !mon_is_valid(mon))
        {
            dun_bmp_unset(bmp, p);
            continue;
        }

        /* Most monsters cannot co-exist with rock */
        if (!mon_can_tunnel(mon) && !mon_can_passwall(mon))
        {
            char m_name[80];
            int sn = 0, damage = 0;
            point_t sp;

            /* Monster can move to escape the wall */
            if (!mon_never_move(mon))
            {
                int i;
                /* Look for safety */
                for (i = 0; i < 8; i++)
                {
                    point_t p2 = point_step(p, ddd[i]);
                    dun_cell_ptr c2;

                    if (dun_bmp_test(bmp, p2)) continue; /* important: looking for unaffected grids */
                    if (!dun_allow_mon_at(cave, p2)) continue;

                    c2 = dun_cell_at(cave, p2);
                    if (floor_has_glyph_of_warding(c2)) continue;
                    if (floor_has_plr_trap(c2)) continue;
                    if (c2->type == FEAT_PATTERN) continue;

                    sn++;
                    if (one_in_(sn)) sp = p2;
                }
            }

            monster_desc(m_name, mon, 0);
            if (mon_show_msg(mon)) msg_format("%^s wails out in pain!", m_name);
            if (sn) damage = _4d(8);
            else damage = mon->hp + 1; /* buried alive! */

            mon_tim_delete(mon, MT_SLEEP);
            mon->hp -= damage;
            if (mon->hp < 0)
            {
                if (mon_show_msg(mon)) msg_format("%^s is embedded in the rock!", m_name);
                delete_monster(mon); /* no monster_death ... uniques can come back and no drops */
            }

            if (sn && mon_is_valid(mon))
                dun_move_mon(cave, mon, sp);
        }
    }

    /* hurt terrain */
    for (p = rect_first(rect); rect_contains_point(rect, p); p = rect_next(rect, p))
    {
        dun_cell_ptr cell;

        if (!dun_bmp_test(bmp, p)) continue;

        cell = dun_cell_at(cave, p);
        assert(!dun_plr_at(cave, p) || plr->pass_wall || plr->kill_wall); /* we unset this bit above ... */

        if (dun_can_destroy(cave, p)) /* blocked by artifacts and CELL_PERM */
        {
            int roll = cell_project(cell) ? _1d(100) : 200; /* force WALL->FLOOR */

            delete_object(p);

            if (roll <= 20)
                dun_place_granite(cave, p);
            else if (roll < 70)
                dun_place_quartz(cave, p);
            else if (roll < 100)
                dun_place_magma(cave, p);
            else
                cave->type->place_floor(cave, p);
        }
    }

    plr->update |= (PU_UN_VIEW | PU_UN_LIGHT);
    plr->update |= (PU_VIEW | PU_LIGHT | PU_FLOW | PU_MON_FLOW | PU_MON_LIGHT | PU_MONSTERS);

    if (weaponmaster_is_(WEAPONMASTER_DIGGERS))
        plr->update |= PU_BONUS;

    plr->redraw |= PR_HEALTH_BARS | PR_MAP;
    plr->window |= (PW_OVERHEAD | PW_DUNGEON);

    dun_bmp_free(bmp);
    return TRUE;
}

bool earthquake(point_t pos, int rad)
{
    return earthquake_aux(pos, rad, 0);
}

void discharge_minion(void)
{
    vec_ptr pets = plr_pets_for_dismiss();
    int     i;
    bool    okay = !plr->riding;

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
        int     rad = 2 + pet->race->alloc.lvl/20;

        if (!mon_is_valid(pet)) continue; /* Killed by previous pets exploding */
        if (pet->dun != cave) continue;
        if (pet->mflag2 & MFLAG2_ILLUSION)
        {
            delete_monster(pet);
            continue;
        }

        if (plr->pclass == CLASS_NECROMANCER)
        {
            switch (randint1(4))
            {
            case 1: typ = GF_POIS; break;
            case 2: typ = GF_NETHER; break;
            case 3: typ = GF_DISENCHANT; break;
            case 4: typ = GF_MANA; break;
            }
        }
        if (mon_is_unique(pet))
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
        mon_burst(pet, rad, typ, dam);
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

    for (i = 0; i < point_vec_length(temp_pts); i++)
    {
        point_t p = point_vec_get(temp_pts, i);
        dun_cell_ptr cell = dun_cell_at(cave, p);
        mon_ptr mon = dun_mon_at(cave, p);

        cell->flags &= ~CELL_TEMP;

        if (!(cell->flags & CELL_DARK))
            cell->flags |= CELL_LIT;

        if (mon)
        {
            int chance = 25;

            if (mon_is_stupid(mon)) chance = 10;
            if (mon_is_smart(mon)) chance = 100;

            if (mon_tim_find(mon, MT_SLEEP) && randint0(100) < chance)
                mon_tim_remove(mon, MT_SLEEP);
        }
    }
    point_vec_clear(temp_pts);
    plr->update |= PU_LIGHT;
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

    for (i = 0; i < point_vec_length(temp_pts); i++)
    {
        point_t p = point_vec_get(temp_pts, i);
        dun_cell_ptr cell = dun_cell_at(cave, p);

        cell->flags &= ~CELL_TEMP;

        if (!(cell->flags & CELL_LIGHT))
        {
            cell->flags &= ~CELL_LIT;
            if (cell_is_boring(cell))
                cell->flags &= ~CELL_MAP;
        }
    }
    point_vec_clear(temp_pts);
    plr->update |= PU_LIGHT;
}

/*
 * Determine how much contiguous open space this grid is next to
 */
static int next_to_open(point_t pos, cell_p filter)
{
    int i;
    int len = 0;
    int blen = 0;
    
    assert(dun_pos_interior(cave, pos));

    for (i = 0; i < 16; i++) /* 2 passes to find longest run */
    {
        point_t p = point_step(pos, cdd[i % 8]);
        dun_cell_ptr cell = dun_cell_at(cave, p);

        if (!filter(cell))
        {
            if (len > blen)
                blen = len;
            len = 0;
        }
        else
            len++;
    }

    return MAX(len, blen);
}

static int next_to_walls_adj(point_t pos, cell_p filter)
{
    int i;
    int ct = 0;

    assert(dun_pos_interior(cave, pos));

    for (i = 0; i < 8; i++)
    {
        point_t p = point_step(pos, ddd[i]);
        dun_cell_ptr cell = dun_cell_at(cave, p);
        if (!filter(cell))
            ct++;
    }
    return ct;
}


/*
 * Aux function -- see below
 */
static void cave_temp_room_aux(point_t pos, bool only_room, cell_p filter)
{
    dun_cell_ptr cell = dun_cell_at(cave, pos);

    if (cell->flags & CELL_TEMP) return; /* already marked */

    /* Do not "leave" the current room */
    if (!(cell->flags & CELL_ROOM))
    {
        if (only_room) return;
        if (!dun_pos_valid(cave, pos)) return;

        /* Do not exceed the maximum spell range */
        if (plr_distance(pos) > MAX_RANGE) return;

        /* Verify this grid */
        /*
         * The reason why it is ==6 instead of >5 is that 8 is impossible
         * due to the check for cave_bold above.
         * 7 lights dead-end corridors (you need to do this for the
         * checkboard interesting rooms, so that the boundary is lit
         * properly.
         * This leaves only a check for 6 bounding walls!
         */
        if (dun_pos_interior(cave, pos) && filter(cell))
        {
            if ( next_to_walls_adj(pos, filter) == 6
              && next_to_open(pos, filter) <= 1 )
            {
                return;
            }
        }
    }

    cell->flags |= CELL_TEMP;     /* mark */
    point_vec_add(temp_pts, pos);
}

static bool _cell_pass_lite(dun_cell_ptr cell)
{
    return cell_los(cell) && !(cell->flags & CELL_DARK);
}
static void cave_temp_lite_room_aux(point_t pos)
{
    cave_temp_room_aux(pos, FALSE, _cell_pass_lite);
}

static bool _cell_pass_dark(dun_cell_ptr cell)
{
    return cell_los(cell) && !(cell->flags & CELL_LIGHT);
    /* XXX was cell_project() ... closed curtains should stop darkness imo */
}

static void cave_temp_unlite_room_aux(point_t pos)
{
    /* XXX Formerly, this would restrict to "only_room", but with 
     * ninjas this gives bad mechanics. Every closed door or curtain
     * and even the boundary walls are not part of the room, so they
     * won't get darkened. Open the door, and you get a 'disturbing'
     * square of light that interrupts the player. (Note: The 'burst'
     * will darken walls and doors ... there is only a problem with 
     * the propagation). cf view_gridlight
     * I simply removed the 'only_room' restriction for darkness. XXX */
    cave_temp_room_aux(pos, FALSE, _cell_pass_dark);
}

/*
 * Illuminate any room containing the given location.
 */
void lite_room(point_t pos)
{
    int i;
    
    point_vec_clear(temp_pts);

    /* Add the initial grid */
    cave_temp_lite_room_aux(pos);

    /* While grids are in the queue, add their neighbors */
    for (i = 0; i < point_vec_length(temp_pts); i++)
    {
        point_t p = point_vec_get(temp_pts, i);
        dun_cell_ptr cell = dun_cell_at(cave, p);

        /* Walls get lit, but stop light */
        if (!_cell_pass_lite(cell)) continue;

        /* Spread adjacent */
        cave_temp_lite_room_aux(point_step(p, 2));
        cave_temp_lite_room_aux(point_step(p, 8));
        cave_temp_lite_room_aux(point_step(p, 6));
        cave_temp_lite_room_aux(point_step(p, 4));

        /* Spread diagonal */
        cave_temp_lite_room_aux(point_step(p, 3));
        cave_temp_lite_room_aux(point_step(p, 7));
        cave_temp_lite_room_aux(point_step(p, 9));
        cave_temp_lite_room_aux(point_step(p, 1));
    }

    /* Now, lite them all up at once */
    cave_temp_room_lite();
}


/*
 * Darken all rooms containing the given location
 */
void unlite_room(point_t pos)
{
    int i;

    point_vec_clear(temp_pts);

    /* Add the initial grid */
    cave_temp_unlite_room_aux(pos);

    /* Spread, breadth first */
    for (i = 0; i < point_vec_length(temp_pts); i++)
    {
        point_t p = point_vec_get(temp_pts, i);
        dun_cell_ptr cell = dun_cell_at(cave, p);

        /* Walls get lit, but stop light */
        if (!_cell_pass_dark(cell)) continue;

        /* Spread adjacent */
        cave_temp_unlite_room_aux(point_step(p, 2));
        cave_temp_unlite_room_aux(point_step(p, 8));
        cave_temp_unlite_room_aux(point_step(p, 6));
        cave_temp_unlite_room_aux(point_step(p, 4));

        /* Spread diagonal */
        cave_temp_unlite_room_aux(point_step(p, 3));
        cave_temp_unlite_room_aux(point_step(p, 7));
        cave_temp_unlite_room_aux(point_step(p, 9));
        cave_temp_unlite_room_aux(point_step(p, 1));
    }

    /* Now, darken them all at once */
    cave_temp_room_unlite();
}



/*
 * Call light around the player
 * Affect all monsters in the projection radius
 */
bool lite_area(int dam, int rad)
{
    if (!plr_tim_find(T_BLIND))
        msg_print("You are surrounded by a white light.");

    plr_burst(rad, GF_LIGHT_WEAK, dam);
    lite_room(plr->pos);
    return TRUE;
}


/*
 * Call darkness around the player
 * Affect all monsters in the projection radius
 */
bool unlite_area(int dam, int rad)
{
    if (!plr_tim_find(T_BLIND))
        msg_print("Darkness surrounds you.");

    plr_burst(rad, GF_DARK_WEAK, dam);
    unlite_room(plr->pos);
    if (cave->type->id == D_SURFACE && cave->ambient_light > 0)
        msg_print("The sun shines brightly and your darkness dissipates!");
    return TRUE;
}

/*
 * Switch position with a monster.
 */
bool teleport_swap(int dir)
{
    point_t pos;
    dun_cell_ptr cell;
    mon_ptr mon;
    int rng = 10 + 3*plr->lev/2;

    if (dir == 5 && target_okay())
        pos = who_pos(plr->target);
    else
        pos = point_step(plr->pos, dir);

    if (plr->anti_tele)
    {
        msg_print("A mysterious force prevents you from teleporting!");
        equip_learn_flag(OF_NO_TELE);
        return FALSE;
    }

    cell = dun_cell_at(cave, pos);
    mon = dun_mon_at(cave, pos);
    if (!mon || (mon->id == plr->riding))
    {
        msg_print("You can't trade places with that!");
        return FALSE;
    }

    if ((cell->flags & CELL_VAULT) || plr_distance(pos) > rng)
    {
        msg_print("Failed to swap.");
        return FALSE;
    }

    mon_tim_delete(mon, MT_SLEEP);

    if (_1d(100) <= mon_res_pct(mon, GF_TELEPORT))
    {
        msg_print("Your teleportation is blocked!");
        mon_lore_resist(mon, GF_TELEPORT);
        return FALSE;
    }

    move_player_effect(pos, MPE_FORGET_FLOW | MPE_HANDLE_STUFF | MPE_DONT_PICKUP);
    return TRUE;
}

/* affect adjacent grids only (_o and _f) */
static bool _gf_adjacent(point_t pos, int gf, int power)
{
    bool notice = FALSE;
    int i;
    who_t who = who_create_plr(); /* doesn't really matter for us, but trap_creation might be monster src'd */
    for (i = 0; i < 8; i++)
    {
        point_t p = point_step(pos, ddd[i]);
        if (!dun_pos_interior(cave, p)) continue;
        if (gf_affect_o(who, p, gf, power, GF_AFFECT_SPELL)) notice = TRUE;
        if (gf_affect_f(who, p, gf, power, GF_AFFECT_SPELL)) notice = TRUE;
    }

    return notice;
}
bool door_creation(void)             { return _gf_adjacent(plr->pos, GF_MAKE_DOOR, 0); }
bool trap_creation(point_t pos)      { return _gf_adjacent(pos,      GF_MAKE_TRAP, 0); }
bool tree_creation(void)             { return _gf_adjacent(plr->pos, GF_MAKE_TREE, 0); }
bool glyph_creation(void)            { return _gf_adjacent(plr->pos, GF_MAKE_GLYPH, 0); }
bool wall_stone(void)                { return _gf_adjacent(plr->pos, GF_MAKE_WALL, 0); }
bool destroy_doors_touch(void)       { return _gf_adjacent(plr->pos, GF_KILL_DOOR, 0); }

bool plr_animate_dead(void)          { return plr_burst(5, GF_ANIM_DEAD, 0); }
bool mon_animate_dead(mon_ptr mon)   { return mon_burst(mon, 5, GF_ANIM_DEAD, 0); }

static int _call_chaos_gf(void)
{
    static int hurt_types[31] =
    {
        GF_ELEC,      GF_POIS,    GF_ACID,    GF_COLD,
        GF_FIRE,      GF_MISSILE, GF_ARROW,   GF_PLASMA,
        GF_HOLY_FIRE, GF_WATER,   GF_LIGHT,    GF_DARK,
        GF_FORCE,     GF_INERTIA, GF_MANA,    GF_METEOR,
        GF_ICE,       GF_CHAOS,   GF_NETHER,  GF_DISENCHANT,
        GF_SHARDS,    GF_SOUND,   GF_NEXUS,   GF_CONFUSION,
        GF_TIME,      GF_GRAVITY, GF_ROCKET,  GF_NUKE,
        GF_HELL_FIRE, GF_DISINTEGRATE, GF_PSY_SPEAR
    };

    return hurt_types[randint0(31)];
}
void call_chaos(int pct)
{
    bool beam = one_in_(4);
    if (one_in_(6))
    {
        int i;
        dice_t dice = spell_dam_dice(0, 0, 150*pct/100);
        for (i = 0; i < 8; i++)
        {
            point_t pos = point_jump(plr->pos, cdd[i], DUN_PATH_MAX);
            int gf = _call_chaos_gf();
            if (beam)
                plr_beam(pos, gf, dice_roll(dice));
            else
                plr_ball(2, pos, gf, dice_roll(dice));
        }
    }
    else if (one_in_(3))
    {
        dice_t dice = spell_dam_dice(0, 0, 250*pct/100);
        plr_burst(8, _call_chaos_gf(), dice_roll(dice));
    }
    else
    {
        dice_t dice = spell_dam_dice(0, 0, 250*pct/100);
        int gf = _call_chaos_gf();
        if (beam)
            plr_cast_beam(gf, dice);
        else
            plr_cast_ball(3 + plr->lev/35, gf, dice);
    }
}


/*
 * Activate the evil Topi Ylinen curse
 * rr9: Stop the nasty things when a Cyberdemon is summoned
 * or the player gets paralyzed.
 */
bool activate_ty_curse(bool stop_ty, int *count)
{
    int i = 0;

    if (statistics_hack) return TRUE;

    do
    {
        switch (randint1(34))
        {
        case 28: case 29:
            if (!(*count))
            {
                msg_print("The ground trembles...");

                earthquake(plr->pos, 5 + randint0(10));
                if (!one_in_(6)) break;
            }
        case 30: case 31:
            if (!(*count))
            {
                int dam = _10d(10);
                msg_print("A portal opens to a plane of raw mana!");
                plr_burst(8, GF_MANA, dam);
                take_hit(DAMAGE_NOESCAPE, dam, "released pure mana");
                if (!one_in_(6)) break;
            }
        case 32: case 33:
            if (!(*count))
            {
                msg_print("Space warps about you!");

                teleport_player(damroll(10, 10), TELEPORT_PASSIVE);
                if (randint0(13)) (*count) += activate_hi_summon(plr->pos, FALSE);
                if (!one_in_(6)) break;
            }
        case 34:
            msg_print("You feel a surge of energy!");

            wall_breaker();
            if (!randint0(7))
            {
                plr_burst(7, GF_KILL_WALL, 50);
                take_hit(DAMAGE_NOESCAPE, 50, "surge of energy");
            }
            if (!one_in_(6)) break;
        case 1: case 2: case 3: case 16: case 17:
            aggravate_monsters(who_create_null());
            if (!one_in_(6)) break;
        case 4: case 5: case 6:
            (*count) += activate_hi_summon(plr->pos, FALSE);
            if (!one_in_(6)) break;
        case 7: case 8: case 9: case 18:
            (*count) += summon_specific(who_create_null(), plr->pos, cave->difficulty, 0, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_NO_PET));
            if (!one_in_(6)) break;
        case 10: case 11: case 12:
            msg_print("You feel your life draining away...");

            lose_exp(plr->exp / 16);
            if (!one_in_(6)) break;
        case 13: case 14: case 15: case 19: case 20:
            if (stop_ty || (plr->free_act && randint1(125) < plr->skills.sav))
            {
                /* Do nothing */ ;
            }
            else
            {
                msg_print("You feel like a statue!");

                if (plr->free_act)
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
             * XXX Yes, but see activate_hi_summon which can summon cyberdemons
             * on *any* dungeon level!
             */
            if ((cave->difficulty > 65) && !stop_ty)
            {
                (*count) += summon_cyber(who_create_plr(), plr->pos);
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


int activate_hi_summon(point_t pos, bool can_pet)
{
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

    summon_lev = (pet ? plr->lev * 2 / 3 + randint1(plr->lev / 2) : cave->difficulty);

    for (i = 0; i < (randint1(7) + (cave->difficulty / 40)); i++)
    {
        int j, ct = 0;
        /* Make sure we get something on each roll ... */
        for (j = 0; j < 1000 && !ct; j++)
        {
            who_t who = pet ? who_create_plr() : who_create_null();
            switch (randint1(25) + (cave->difficulty / 20))
            {
                case 1: case 2:
                    ct = summon_specific(who, pos, summon_lev, SUMMON_ANT, mode);
                    break;
                case 3: case 4:
                    ct = summon_specific(who, pos, summon_lev, SUMMON_SPIDER, mode);
                    break;
                case 5: case 6:
                    ct = summon_specific(who, pos, summon_lev, SUMMON_HOUND, mode);
                    break;
                case 7: case 8:
                    ct = summon_specific(who, pos, summon_lev, SUMMON_HYDRA, mode);
                    break;
                case 9: case 10:
                    ct = summon_specific(who, pos, summon_lev, SUMMON_ANGEL, mode);
                    break;
                case 11: case 12:
                    ct = summon_specific(who, pos, summon_lev, SUMMON_UNDEAD, mode);
                    break;
                case 13: case 14:
                    ct = summon_specific(who, pos, summon_lev, SUMMON_DRAGON, mode);
                    break;
                case 15: case 16:
                    ct = summon_specific(who, pos, summon_lev, SUMMON_DEMON, mode);
                    break;
                case 17:
                    if (can_pet) break;
                    ct = summon_specific(who, pos, summon_lev, SUMMON_AMBERITE, (mode | PM_ALLOW_UNIQUE));
                    break;
                case 18: case 19:
                    if (can_pet) break;
                    ct = summon_specific(who, pos, summon_lev, SUMMON_UNIQUE, (mode | PM_ALLOW_UNIQUE));
                    break;
                case 20: case 21:
                    if (!can_pet) mode |= PM_ALLOW_UNIQUE;
                    ct = summon_specific(who, pos, summon_lev, SUMMON_HI_UNDEAD, mode);
                    break;
                case 22: case 23:
                    if (!can_pet) mode |= PM_ALLOW_UNIQUE;
                    ct = summon_specific(who, pos, summon_lev, SUMMON_HI_DRAGON, mode);
                    break;
                case 24:
                    ct = summon_specific(who, pos, 100, SUMMON_CYBER, mode);
                    break;
                default:
                    if (!can_pet) mode |= PM_ALLOW_UNIQUE;
                    ct = summon_specific(who, pos,pet ? summon_lev : (((summon_lev * 3) / 2) + 5), 0, mode);
            }
        }
        count += ct;
    }

    return count;
}


int summon_cyber(who_t who, point_t pos)
{
    int i;
    int max_cyber = cave->difficulty / 50 + randint1(2);
    int count = 0;
    u32b mode = PM_ALLOW_GROUP;

    if (who_is_pet(who)) mode |= PM_FORCE_PET;

    if (max_cyber > 4) max_cyber = 4;

    for (i = 0; i < max_cyber; i++)
        count += summon_specific(who, pos, 100, SUMMON_CYBER, mode);

    return count;
}


void wall_breaker(void)
{
    if (randint1(80 + plr->lev) < 70)
    {
        point_t pos;
        int attempts = 1000;
        while (attempts--)
        {
            pos = scatter(plr->pos, 4);

            if (!dun_allow_project_at(cave, pos)) continue;
            if (dun_plr_at(cave, pos)) continue;
            break;
        }
        plr_beam(pos, GF_KILL_WALL, 20 + _1d(30));
    }
    else if (randint1(100) > 30)
    {
        earthquake(plr->pos, 1);
    }
    else
    {
        int num = _5d(3);
        int i;

        for (i = 0; i < num; i++)
        {
            point_t pos;
            while (1)
            {
                pos = scatter(plr->pos, 10);
                if (dun_plr_at(cave, pos)) continue;
                break;
            }
            plr_beam(pos, GF_KILL_WALL, 20 + _1d(30));
        }
    }
}

/*
 * "Rush Attack" routine for Samurai or Ninja
 * Return value is for checking "done"
 * Hacked up for Duelist as well.
 */
bool rush_attack(int rng, bool *mdeath)
{
    int dir;
    point_t tgt, pos;
    mon_ptr tgt_mon = NULL;
    point_t path[100];
    int path_n, i;
    bool tmp_mdeath = FALSE;
    bool moved = FALSE;

    if (mdeath) *mdeath = FALSE;

    project_length = rng;

    /* Mega Hack for the Duelist */
    if (plr->pclass == CLASS_DUELIST)
    {
        tgt_mon = who_mon(plr->duelist_target);

        if (!tgt_mon)
        {
            msg_print("Failed!");
            return FALSE;
        }
        tgt = tgt_mon->pos;
        if (!plr_view(tgt))
        {
            msg_format("%^s is not in your line of sight.", duelist_current_challenge());
            return FALSE;
        }
    }
    else
    {
        if (!get_fire_dir(&dir)) return FALSE;
        tgt = point_jump(plr->pos, dir, project_length);
        if (dir == 5 && target_okay())
            tgt = who_pos(plr->target);

        if (dun_pos_interior(cave, tgt))
            tgt_mon = dun_mon_at(cave, tgt);
    }

    path_n = project_path(path, project_length, plr->pos, tgt, PROJECT_STOP | PROJECT_KILL);
    project_length = 0;

    /* No need to move */
    if (!path_n) return TRUE;

    pos = plr->pos;

    /* Project along the path */
    for (i = 0; i < path_n; i++)
    {
        point_t next = path[i];
        mon_ptr mon;

        if (dun_allow_plr_at(cave, next) && cell_allow_plr(dun_cell_at(cave, next)))
        {
            pos = next;
            continue;
        }

        mon = dun_mon_at(cave, next);
        if (!mon)
        {
            if (tgt_mon)
                msg_print("Failed!");
            else
                msg_print("You can't move to that place.");

            break;
        }

        /* Move player before updating the monster */
        if (!dun_plr_at(cave, pos)) teleport_player_to(pos, TELEPORT_NONMAGICAL);
        update_mon(mon, TRUE);

        if (tgt_mon != mon)
        {
            msg_format("There is %s in the way!", mon->ml ? (tgt_mon ? "another monster" : "a monster") : "someone");
        }
        else if (!dun_plr_at(cave, pos))
        {
            char m_name[80];
            monster_desc(m_name, mon, 0);
            msg_format("You quickly jump in and attack %s!", m_name);
        }

        if (!dun_plr_at(cave, pos)) teleport_player_to(pos, TELEPORT_NONMAGICAL); /* XXX why 2x? */
        moved = TRUE;
        {
            plr_attack_t ctx = {0};
            ctx.mode = PLR_HIT_RUSH_ATTACK;
            plr_attack(&ctx, next);
            tmp_mdeath = BOOL(ctx.stop == STOP_MON_DEAD);
        }
        break;
    }

    if (!moved && !dun_plr_at(cave, pos)) teleport_player_to(pos, TELEPORT_NONMAGICAL);
    if (mdeath) *mdeath = tmp_mdeath;
    return TRUE;
}


