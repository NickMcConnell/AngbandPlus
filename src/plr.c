#include "angband.h"

/************************************************************************
 * Player
 ************************************************************************/
plr_ptr p_ptr = NULL;
static void _plr_alloc(void)
{
    int i;
    p_ptr = malloc(sizeof(player_type));

    memset(p_ptr, 0, sizeof(player_type));
    p_ptr->innate_blows = vec_alloc((vec_free_f)mon_blow_free);
    plr_tim_clear();

    /* setup non-zero defaults */
    p_ptr->mimic_form = MIMIC_NONE;
    p_ptr->food = PY_FOOD_FULL - 1;
    p_ptr->pet_follow_distance = PET_FOLLOW_DIST;
    p_ptr->pet_extra_flags = (PF_TELEPORT | PF_ATTACK_SPELL | PF_SUMMON_SPELL);
    p_ptr->max_plv = p_ptr->lev = 1;
    p_ptr->clp = 1000;
    for (i = 0; i < MAX_DEMIGOD_POWERS; ++i)
        p_ptr->demigod_power[i] = -1;
    p_ptr->draconian_power = -1;
    for (i = 0; i < 64; i++)
        p_ptr->spell_order[i] = 99;
}
static void _plr_free(void)
{
    if (p_ptr)
    {
        if (p_ptr->last_message) z_string_free(p_ptr->last_message);
        if (p_ptr->los) dun_bmp_free(p_ptr->los);
        vec_free(p_ptr->innate_blows);
        free(p_ptr);
        p_ptr = NULL;
    }
}
void plr_startup(void)
{
    _plr_alloc();
}
void plr_shutdown(void)
{
    _plr_free();
}
void plr_wipe(void)
{
    _plr_free();
    _plr_alloc();
}
dun_ptr plr_dun(void)
{
    return dun_mgr_dun(p_ptr->dun_id);
}
dun_type_ptr plr_dun_type(void)
{
    dun_ptr dun = plr_dun();
    return dun_types_lookup(dun->dun_type_id);
}
void plr_hook_birth(void)
{
    plr_race_ptr r = plr_race();
    plr_class_ptr c = plr_class();
    personality_ptr p = get_personality();

    /* Hack: Skills must init before racial birth for monster race innate proficiency! */
    skills_on_birth();   

    /* Hack: Personality goes first for the Sexy Whip! */
    if (p->birth) p->birth();

    /* Note: The class birth function should give starting
     * equipment and spellbooks while the race birth function
     * should give starting food and light (in general) */
    if (c->hooks.birth) c->hooks.birth();
    if (r->hooks.birth) r->hooks.birth();
    else
    {
        /* most races won't need a special birth function, so
         * give standard food and light by default */
        plr_birth_food();
        plr_birth_light();
    }

    spell_stats_on_birth();

    stats_on_gold_find(p_ptr->au); /* Found? Inherited? What's the difference? */

    /* Gain CL1 (e.g. Chaos Warriors) */
    if (c->hooks.gain_level) c->hooks.gain_level(p_ptr->lev);
    if (r->hooks.gain_level) r->hooks.gain_level(p_ptr->lev);
}
void plr_hook_calc_bonuses(void)
{
    plr_race_ptr r = plr_race();
    plr_class_ptr c = plr_class();
    if (r->hooks.calc_bonuses) r->hooks.calc_bonuses();
    if (c->hooks.calc_bonuses) c->hooks.calc_bonuses();
}
void plr_hook_calc_innate_attacks(void)
{
    plr_race_ptr r = plr_race();
    plr_class_ptr c = plr_class();
    if (r->hooks.calc_innate_attacks) r->hooks.calc_innate_attacks();
    if (c->hooks.calc_innate_attacks) c->hooks.calc_innate_attacks();
}
void plr_hook_calc_stats(s16b stats[MAX_STATS])
{
    plr_race_ptr r = plr_race();
    plr_class_ptr c = plr_class();
    if (r->hooks.calc_stats) r->hooks.calc_stats(stats);
    if (c->hooks.calc_stats) c->hooks.calc_stats(stats);
}
void plr_hook_character_dump(doc_ptr doc)
{
    plr_race_ptr r = plr_race();
    plr_class_ptr c = plr_class();
    if (r->hooks.character_dump) r->hooks.character_dump(doc);
    if (c->hooks.character_dump) c->hooks.character_dump(doc);
}
bool plr_hook_destroy_object(obj_ptr obj)
{
    plr_race_ptr r = plr_race();
    plr_class_ptr c = plr_class();
    bool b = FALSE;
    if (r->hooks.destroy_object) b = r->hooks.destroy_object(obj);
    if (!b && c->hooks.destroy_object) b = c->hooks.destroy_object(obj);
    return b;
}
void plr_hook_get_object(obj_ptr obj)
{
    plr_race_ptr r = plr_race();
    plr_class_ptr c = plr_class();
    if (r->hooks.get_object) r->hooks.get_object(obj);
    if (c->hooks.get_object) c->hooks.get_object(obj);
}
void plr_hook_kill_monster(mon_ptr mon)
{
    plr_race_ptr r = plr_race();
    plr_class_ptr c = plr_class();
    dun_type_ptr dt = dun_type();
    dun_world_ptr w = dun_worlds_current();
    town_ptr t = towns_current_town();

    /* world goes first to drop a world travel tile if needed */
    if (w->kill_mon_f) w->kill_mon_f(w, mon);

    /* dungeon type goes next to drop stairs for the next guardian if needed */
    if (dt->kill_mon_f) dt->kill_mon_f(dt, mon);

    if (t && t->kill_mon_f) t->kill_mon_f(mon);

    /* plr goes last ... */
    if (r->hooks.kill_monster) r->hooks.kill_monster(mon);
    if (c->hooks.kill_monster) c->hooks.kill_monster(mon);
}
void plr_hook_load(savefile_ptr file)
{
    plr_race_ptr r = plr_true_race();
    plr_class_ptr c = plr_class();
    if (r->hooks.load_player) r->hooks.load_player(file);
    if (c->hooks.load_player) c->hooks.load_player(file);
}
void plr_hook_move_monster(mon_ptr mon)
{
    plr_race_ptr r = plr_race();
    plr_class_ptr c = plr_class();
    if (r->hooks.move_monster) r->hooks.move_monster(mon);
    if (c->hooks.move_monster) c->hooks.move_monster(mon);
}
void plr_hook_move_player(void)
{
    plr_race_ptr r = plr_race();
    plr_class_ptr c = plr_class();
    if (r->hooks.move_player) r->hooks.move_player();
    if (c->hooks.move_player) c->hooks.move_player();
}
void plr_hook_player_action(void)
{
    plr_race_ptr r = plr_race();
    plr_class_ptr c = plr_class();
    if (r->hooks.player_action) r->hooks.player_action();
    if (c->hooks.player_action) c->hooks.player_action();
}
void plr_hook_process_player(void)
{
    plr_race_ptr r = plr_race();
    plr_class_ptr c = plr_class();
    if (r->hooks.process_player) r->hooks.process_player();
    if (c->hooks.process_player) c->hooks.process_player();
}
void plr_hook_process_world(void)
{
    plr_race_ptr r = plr_race();
    plr_class_ptr c = plr_class();
    if (r->hooks.process_world) r->hooks.process_world();
    if (c->hooks.process_world) c->hooks.process_world();
}
void plr_hook_prt_effects(doc_ptr doc)
{
    plr_race_ptr r = plr_race();
    plr_class_ptr c = plr_class();
    if (r->hooks.prt_effects) r->hooks.prt_effects(doc);
    if (c->hooks.prt_effects) c->hooks.prt_effects(doc);
}
void plr_hook_register_timers(void)
{
    plr_class_ptr c = plr_class();
    /* only classes can register custom timers ... but in monster mode,
     * the race is treated as if it were a class */
    if (c->id == CLASS_MONSTER)
    {
        plr_race_ptr r = plr_true_race();
        if (r->hooks.register_timers) r->hooks.register_timers();
    }
    else if (c->hooks.register_timers) c->hooks.register_timers();
}
void plr_hook_save(savefile_ptr file)
{
    plr_race_ptr r = plr_true_race();
    plr_class_ptr c = plr_class();
    if (r->hooks.save_player) r->hooks.save_player(file);
    if (c->hooks.save_player) c->hooks.save_player(file);
}
bool plr_mage_bonus(void)
{
    plr_race_ptr r = plr_race();
    plr_class_ptr c = plr_class();
    return (r->flags & RACE_MAGE_BONUS) || (c->flags & CLASS_MAGE_BONUS);
}
int plr_skill_sav(int m_idx)
{
    int sav = p_ptr->skills.sav;
    if (p_ptr->pclass == CLASS_DUELIST && m_idx && m_idx == p_ptr->duelist_target_idx)
        sav += 15 + p_ptr->lev;
    return sav;
}
mon_ptr plr_target_mon(void)
{
    mon_ptr mon;
    if (use_old_target && target_who > 0 && target_okay_aux(TARGET_KILL))
    {
    }
    else if (!target_set(TARGET_KILL)) return NULL;
    mon = mon_at_xy(target_col, target_row);
    if (target_who < 0 || !mon || !target_able(mon->id))
    {
        msg_print("You need to target a monster.");
        return NULL;
    }
    return mon;
}
mon_ptr plr_target_adjacent_mon(void)
{
    int dir = 0;
    point_t pos;
    mon_ptr mon = NULL;

    if (use_old_target && target_okay())
    {
        pos = point_create(target_col, target_row);
        mon = mon_at(pos);
        if (mon && mon->cdis > 1)
            mon = NULL;
    }

    if (!mon && auto_target)
    {
        /* XXX auto target ... XXX look at plr_attack.c for this */
    }

    if (!mon)
    {
        if (!get_rep_dir2(&dir)) return NULL;
        pos = point_step(p_ptr->pos, dir);
        mon = mon_at(pos);
        if (!mon) msg_print("There is no monster there.");
    }
    return mon;
}
bool plr_touch_mon(mon_ptr mon, int gf, int dam)
{
    bool b;
    if (!mon || mon_is_dead(mon)) return FALSE; /* paranoia */
    if (cave->flags & DF_NO_MELEE) /* XXX */
    {
        msg_print("Something prevent you from attacking.");
        return FALSE;
    }
    if ( !is_hostile(mon)
      && !( plr_tim_find(T_STUN)
         || plr_tim_find(T_CONFUSED)
         || plr_tim_find(T_HALLUCINATE)
         || plr_tim_find(T_BERSERK)
         || !mon->ml) )
    {
        if (!get_check("Really hit it? "))
            return FALSE;
    }
    b = gf_affect_m(GF_WHO_PLAYER, mon, gf, dam, GF_AFFECT_ATTACK);
    if (!mon_is_dead(mon))
        plr_on_touch_mon(mon);
    return b;
}
mon_ptr plr_riding_mon(void)
{
    if (!p_ptr->riding) return NULL;
    return dun_mon(plr_dun(), p_ptr->riding);
}
mon_race_ptr plr_riding_race(void)
{
    mon_ptr mount = plr_riding_mon();
    if (!mount) return NULL;
    return mon_race(mount);
}
int plr_riding_lvl(void)
{
    mon_race_ptr r = plr_riding_race();
    if (!r) return 0;
    return r->level;
}

static int _pet_dismiss_cmp_f(mon_ptr m_ptr1, mon_ptr m_ptr2)
{
    monster_race *r_ptr1 = mon_race(m_ptr1);
    monster_race *r_ptr2 = mon_race(m_ptr2);

    /* mount goes first */
    if (p_ptr->riding == m_ptr1->id) return -1;
    if (p_ptr->riding == m_ptr2->id) return 1;

    /* followed by named pets */
    if (m_ptr1->nickname && !m_ptr2->nickname) return -1;
    if (m_ptr2->nickname && !m_ptr1->nickname) return 1;

    /* followed by summoned monsters, which will disappear if you dismiss the summoner */
    if (!m_ptr1->parent_m_idx && m_ptr2->parent_m_idx) return -1;
    if (!m_ptr2->parent_m_idx && m_ptr1->parent_m_idx) return 1;

    /* followed by uniques? I guess they cost alot to keep */
    if ((r_ptr1->flags1 & RF1_UNIQUE) && !(r_ptr2->flags1 & RF1_UNIQUE)) return -1;
    if ((r_ptr2->flags1 & RF1_UNIQUE) && !(r_ptr1->flags1 & RF1_UNIQUE)) return 1;

    /* descending on level XXX */
    if (r_ptr1->level > r_ptr2->level) return -1;
    if (r_ptr1->level < r_ptr2->level) return 1;

    /* descending on health XXX */
    if (m_ptr1->hp > m_ptr2->hp) return -1;
    if (m_ptr1->hp < m_ptr2->hp) return 1;

    return 0;
}
static int _pet_cmp_f(mon_ptr m_ptr1, mon_ptr m_ptr2)
{
    monster_race *r_ptr1 = mon_race(m_ptr1);
    monster_race *r_ptr2 = mon_race(m_ptr2);

    if (m_ptr1->nickname && !m_ptr2->nickname) return -1;
    if (m_ptr2->nickname && !m_ptr1->nickname) return 1;

    if ((r_ptr1->flags1 & RF1_UNIQUE) && !(r_ptr2->flags1 & RF1_UNIQUE)) return -1;
    if ((r_ptr2->flags1 & RF1_UNIQUE) && !(r_ptr1->flags1 & RF1_UNIQUE)) return 1;

    if (r_ptr1->level > r_ptr2->level) return -1;
    if (r_ptr1->level < r_ptr2->level) return 1;

    if (m_ptr1->hp > m_ptr2->hp) return -1;
    if (m_ptr1->hp < m_ptr2->hp) return 1;

    return 0;
}

vec_ptr plr_pets(void)
{
    vec_ptr pets = dun_filter_mon(cave, mon_is_pet);
    vec_sort(pets, (vec_cmp_f)_pet_cmp_f);
    return pets;
}
vec_ptr plr_pets_for_dismiss(void)
{
    vec_ptr pets = dun_filter_mon(cave, mon_is_pet);
    vec_sort(pets, (vec_cmp_f)_pet_dismiss_cmp_f);
    return pets;
}
int plr_feeling_delay(dun_ptr dun)
{
    int delay = MAX(10, 150 - p_ptr->skills.fos) * (150 - dun->dun_lvl) * TURNS_PER_TICK / 100;

    delay = delay * adj_pseudo_id[p_ptr->stat_ind[A_WIS]] / 100;
    delay = delay * (625 - virtue_current(VIRTUE_ENLIGHTENMENT)) / 625;

    return delay;
}
