#include "angband.h"

#include <assert.h>

/************************************************************************
 * Player
 ************************************************************************/
plr_ptr plr = NULL;
static void _plr_alloc(void)
{
    int i;
    plr = malloc(sizeof(player_type));

    memset(plr, 0, sizeof(player_type));
    plr->innate_blows = vec_alloc((vec_free_f)mon_blow_free);
    plr_tim_clear();

    /* setup non-zero defaults */
    plr->mimic_form = MIMIC_NONE;
    plr->food = PY_FOOD_FULL - 1;
    plr->pet_follow_distance = PET_FOLLOW_DIST;
    plr->pet_extra_flags = (PF_TELEPORT | PF_ATTACK_SPELL | PF_SUMMON_SPELL);
    plr->max_plv = plr->lev = 1;
    plr->clp = 1000;
    for (i = 0; i < MAX_DEMIGOD_POWERS; ++i)
        plr->demigod_power[i] = -1;
    plr->draconian_power = -1;
    for (i = 0; i < 64; i++)
        plr->spell_order[i] = 99;

}
static void _plr_free(void)
{
    if (plr)
    {
        if (plr->last_message) z_string_free(plr->last_message);
        vec_free(plr->innate_blows);
        free(plr);
        plr = NULL;
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
    return dun_mgr_dun(plr->dun_id);
}
dun_type_ptr plr_dun_type(void)
{
    return plr_dun()->type;
}
static mon_race_ptr _mon_race_parse(cptr which)
{
    mon_race_ptr r = mon_race_parse(which);
    if (!r)
    {
        msg_format("<color:v>Error</color>: <color:r>%s</color> is not a valid monster race.", which);
        return NULL;
    }
    return r;
}
bool plr_mon_race_is_(cptr which)
{
    assert(_mon_race_parse(which)); /* paranoia wrt my fat fingers ... */
    return sym_equals(plr->current_r_idx, which);
}
static void _plr_mon_race_set(mon_race_ptr r)
{
    if (!r) return;
    plr->current_r_idx = r->id;
    equip_on_change_race();
    plr->update |= PU_BONUS | PU_INNATE;
    plr->redraw |= PR_MAP | PR_BASIC;
}
void plr_mon_race_set(cptr which)
{
    _plr_mon_race_set(_mon_race_parse(which));
}
void plr_mon_race_evolve(cptr which)
{
    mon_race_ptr r = mon_race_parse(which);
    if (!r) return;
    if (mon_race_is_unique(r)) /* c.Multi-hued */
        msg_format("You have evolved into %s. There can be only one, you know ...", r->name);
    else
    {
        msg_format("You have evolved into %s %s.",
            is_a_vowel(r->name[0]) ? "an" : "a", r->name);
    }
    if (mon_race_is_female(r))
        plr->psex = SEX_FEMALE;
    if (mon_race_is_male(r))
        plr->psex = SEX_MALE;
    _plr_mon_race_set(r);
}
mon_race_ptr plr_mon_race(void)
{
    if (!plr->current_r_idx) return mon_race_parse("@.player");
    return mon_race_lookup(plr->current_r_idx);
}
mon_race_ptr plr_boss_race(void)
{
    plr_race_ptr r = plr_race();
    if (!r->boss_r_idx) return NULL;
    return mon_race_lookup(r->boss_r_idx);
}
equip_template_ptr plr_equip_template(void)
{
    mon_race_ptr race = plr_mon_race();
    return equip_template_lookup(race->body.body_id);
}
bool plr_hook_auto_id(obj_ptr obj)
{
    plr_race_ptr r = plr_race();
    plr_class_ptr c = plr_class();
    if (r->hooks.auto_id && r->hooks.auto_id(obj))
        return TRUE;
    if (c->hooks.auto_id && c->hooks.auto_id(obj))
        return TRUE;
    return FALSE;
}
bool plr_hook_auto_detect(void)
{
    plr_race_ptr r = plr_race();
    plr_class_ptr c = plr_class();
    if (r->hooks.auto_detect && r->hooks.auto_detect())
        return TRUE;
    if (c->hooks.auto_detect && c->hooks.auto_detect())
        return TRUE;
    return FALSE;
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

    stats_on_gold_find(plr->au); /* Found? Inherited? What's the difference? */

    /* Gain CL1 (e.g. Chaos Warriors) */
    if (c->hooks.gain_level) c->hooks.gain_level(plr->lev);
    if (r->hooks.gain_level) r->hooks.gain_level(plr->lev);
}
void plr_hook_startup(void)
{
    hack_mind = TRUE; /* we are playing now ... cf do_cmd_save_game */
    if (plr->pflag & PFLAG_BIRTH)
    {
        dun_world_ptr world = dun_worlds_current();

        /* give starting gear w/o message spam */
        plr_hook_birth();
        plr->pflag &= ~PFLAG_BIRTH;

        msg_format("<color:B>Welcome!</color> You have entered <color:r>%s</color>.", world->name);
        msg_boundary();
        msg_print(world->desc);
        msg_print(NULL);
    }
    /* XXX hooks? */
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
    dun_type_ptr dt = cave->type;
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
    hex_load(file);
}
void plr_hook_save(savefile_ptr file)
{
    plr_race_ptr r = plr_true_race();
    plr_class_ptr c = plr_class();
    if (r->hooks.save_player) r->hooks.save_player(file);
    if (c->hooks.save_player) c->hooks.save_player(file);
    hex_save(file);
}
plr_magic_ptr plr_magic(void)
{
    plr_race_ptr r = plr_true_race();
    plr_class_ptr c = plr_class();
    if (r->hooks.caster_info) return r->hooks.caster_info();
    if (c->hooks.caster_info) return c->hooks.caster_info();
    return NULL;
}
int plr_skill(int base)
{
    int skill = base;
    if (plr_tim_find(T_BLIND) || no_light()) skill /= 10;
    if (plr_tim_find(T_CONFUSED) || plr_tim_find(T_HALLUCINATE)) skill /= 10;
    return skill;
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
void plr_hook_update_light(void)
{
    plr_race_ptr r = plr_race();
    plr_class_ptr c = plr_class();
    static int old_light = 255;
    int light;

    if (r->hooks.update_light) r->hooks.update_light();
    if (c->hooks.update_light) c->hooks.update_light();

    light = plr_light(plr->pos);
    if (light != old_light)
    {
        old_light = light;
        if (display_light_bar)
            plr->redraw |= PR_HEALTH_BARS;
    }
}
bool plr_mage_bonus(void)
{
    plr_race_ptr r = plr_race();
    plr_class_ptr c = plr_class();
    return (r->flags & RACE_MAGE_BONUS) || (c->flags & CLASS_MAGE_BONUS);
}
bool plr_allow_mage_quiver(void)
{
    if (plr->pclass == CLASS_DEVICEMASTER) return TRUE;
    return plr_mage_bonus();
}
int plr_skill_sav(who_t who)
{
    int sav = plr->skills.sav;
    if (who_equals(who, plr->duelist_target))
        sav += 15 + plr->lev;
    return sav;
}
int plr_ac(mon_ptr who)
{
    int ac = plr->ac + plr->to_a;
    if (who == who_mon(plr->duelist_target))
    {
        int penalty = 20 + 30*plr->lev/50;
        int bonus = 2*penalty;
        bonus -= plr->ac_adj; /* XXX AC should go from -50->+50, not 0->+100! */
        ac += bonus;
    }
    return ac;
}
mon_ptr plr_target_mon(void)
{
    if (use_old_target && who_is_mon(plr->target) && target_okay_aux(TARGET_KILL))
    {
    }
    else if (!target_set(TARGET_KILL)) return NULL;

    if (!who_is_mon(plr->target))
    {
        msg_print("You need to target a monster.");
        return NULL;
    }
    if (!target_able(who_mon(plr->target)))
    {
        msg_print("You cannot target that monster.");
        return NULL;
    }
    return who_mon(plr->target);
}
static void _set_target(mon_ptr mon)
{
    plr->target = who_create_mon(mon);
    plr->redraw |= PR_HEALTH_BARS;
}
static mon_ptr _get_adjacent_target(bool allow_pet)
{
    mon_ptr result = NULL;
    int i;
    for (i = 0; i < 8; i++)
    {
        point_t p = point_step(plr->pos, cdd[i]);
        mon_ptr mon = dun_mon_at(cave, p);
        if (!mon || !mon->ml) continue;
        if (!allow_pet && !mon_is_hostile(mon)) continue; /* XXX skip friends as well */
        /* XXX only auto-target an adjacent monster if the choice
         * is not ambiguous XXX */
        if (result) return NULL;
        result = mon;
    }
    if (result)
        _set_target(result);
    return result;
}
static mon_ptr _target_adjacent_mon(bool pet)
{
    int dir = 0;
    point_t pos;
    mon_ptr mon = NULL;

    if (use_old_target && target_okay())
    {
        pos = who_pos(plr->target);
        mon = dun_mon_at(cave, pos);
        if (mon && mon->cdis > 1)
            mon = NULL;
    }

    if (!mon && auto_target && !plr_tim_find(T_CONFUSED))
        mon = _get_adjacent_target(pet);

    if (!mon)
    {
        if (!get_rep_dir2(&dir)) return NULL;
        pos = point_step(plr->pos, dir);
        mon = dun_mon_at(cave, pos);
        if (!mon) msg_print("There is no monster there.");
    }
    return mon;
}
mon_ptr plr_target_adjacent_mon(void)
{
    return _target_adjacent_mon(FALSE);
}
mon_ptr plr_target_adjacent_pet(void)
{
    return _target_adjacent_mon(TRUE);
}
bool plr_touch_mon(mon_ptr mon, int gf, int dam)
{
    bool b;
    if (!mon || !mon_is_valid(mon)) return FALSE; /* paranoia */
    if (cave->flags & DF_NO_MELEE) /* XXX */
    {
        msg_print("Something prevent you from attacking.");
        return FALSE;
    }
    if ( !mon_is_hostile(mon)
      && !( plr_tim_find(T_STUN)
         || plr_tim_find(T_CONFUSED)
         || plr_tim_find(T_HALLUCINATE)
         || plr_tim_find(T_BERSERK)
         || !mon->ml) )
    {
        if (!get_check("Really hit it? "))
            return FALSE;
    }
    b = gf_affect_m(who_create_plr(), mon, gf, dam, GF_AFFECT_ATTACK);
    mon_set_target(mon, plr->pos);
    if (mon_is_valid(mon))
        plr_on_touch_mon(mon);
    return b;
}
mon_ptr plr_riding_mon(void)
{
    if (!plr->riding) return NULL;
    return dun_mon(plr_dun(), plr->riding);
}
bool plr_is_riding_(mon_ptr mon)
{
    return mon->id == plr->riding;
}
mon_race_ptr plr_riding_race(void)
{
    mon_ptr mount = plr_riding_mon();
    if (!mount) return NULL;
    return mount->race;
}
int plr_riding_lvl(void)
{
    mon_race_ptr r = plr_riding_race();
    if (!r) return 0;
    return r->alloc.lvl;
}
bool plr_see_nocto(point_t pos)
{
    if (!plr->see_nocto) return FALSE;
    if (!plr_view(pos)) return FALSE;
    if (plr->see_nocto >= DUN_VIEW_MAX) return TRUE;
    return point_fast_distance(plr->pos, pos) <= plr->see_nocto;
}
static int _pet_dismiss_cmp_f(mon_ptr m_ptr1, mon_ptr m_ptr2)
{
    monster_race *r_ptr1 = m_ptr1->race;
    monster_race *r_ptr2 = m_ptr2->race;

    /* mount goes first */
    if (plr->riding == m_ptr1->id) return -1;
    if (plr->riding == m_ptr2->id) return 1;

    /* followed by named pets */
    if (m_ptr1->nickname && !m_ptr2->nickname) return -1;
    if (m_ptr2->nickname && !m_ptr1->nickname) return 1;

    /* followed by summoned monsters, which will disappear if you dismiss the summoner */
    if (!m_ptr1->parent_id && m_ptr2->parent_id) return -1;
    if (!m_ptr2->parent_id && m_ptr1->parent_id) return 1;

    /* followed by uniques? I guess they cost alot to keep */
    if (mon_race_is_unique(r_ptr1) && !mon_race_is_unique(r_ptr2)) return -1;
    if (mon_race_is_unique(r_ptr2) && !mon_race_is_unique(r_ptr1)) return 1;

    /* descending on level XXX */
    if (r_ptr1->alloc.lvl > r_ptr2->alloc.lvl) return -1;
    if (r_ptr1->alloc.lvl < r_ptr2->alloc.lvl) return 1;

    /* descending on health XXX */
    if (m_ptr1->hp > m_ptr2->hp) return -1;
    if (m_ptr1->hp < m_ptr2->hp) return 1;

    return 0;
}
static int _pet_cmp_f(mon_ptr m_ptr1, mon_ptr m_ptr2)
{
    monster_race *r_ptr1 = m_ptr1->race;
    monster_race *r_ptr2 = m_ptr2->race;

    if (m_ptr1->nickname && !m_ptr2->nickname) return -1;
    if (m_ptr2->nickname && !m_ptr1->nickname) return 1;

    if (mon_race_is_unique(r_ptr1) && !mon_race_is_unique(r_ptr2)) return -1;
    if (mon_race_is_unique(r_ptr2) && !mon_race_is_unique(r_ptr1)) return 1;

    if (r_ptr1->alloc.lvl > r_ptr2->alloc.lvl) return -1;
    if (r_ptr1->alloc.lvl < r_ptr2->alloc.lvl) return 1;

    if (m_ptr1->hp > m_ptr2->hp) return -1;
    if (m_ptr1->hp < m_ptr2->hp) return 1;

    return 0;
}

int plr_pet_count(void)
{
    return mon_pack_count(plr_pack());
}
mon_pack_ptr plr_pets(void)
{
    /* this is for UI display. no need to copy pets->members, but we do
     * sort the list for cosmetic purposes. */
    mon_pack_ptr pets = plr_pack();
    vec_sort(pets->members, (vec_cmp_f)_pet_cmp_f);
    return pets;
}
static bool _can_dismiss(mon_ptr mon)
{
    assert(mon_is_pet(mon));
    /* Temp pets come from the "Mask of Command" and refuse to be dismissed.
     * When this mask wears off, they will resume hostility! */
    return !mon_has_smart_flag(mon, SM_TEMP_PET);
}
vec_ptr plr_pets_for_dismiss(void)
{
    /* this is for pet deletion and requires we copy plr_pack()->members */
    mon_pack_ptr pack = plr_pack();
    vec_ptr      pets = vec_filter(pack->members, (vec_item_p)_can_dismiss);
    vec_sort(pets, (vec_cmp_f)_pet_dismiss_cmp_f);
    return pets;
}
int plr_feeling_delay(dun_ptr dun)
{
    int delay = MAX(10, 150 - plr->skills.fos) * (150 - dun->dun_lvl) * TURNS_PER_TICK / 100;

    delay = delay * adj_pseudo_id[plr->stat_ind[A_WIS]] / 100;
    delay = delay * (625 - virtue_current(VIRTUE_ENLIGHTENMENT)) / 625;

    return delay;
}
/************************************************************************
 * Player Spells
 ************************************************************************/
static bool _known_spell(int realm, int spell)
{
    if (plr->realm1 == realm)
        return BOOL(plr->spell_learned1 & (1U << spell));
    else if (plr->realm2 == realm)
        return BOOL(plr->spell_learned2 & (1U << spell));
    return FALSE;
}
/* Helper for Auto-ID and Auto-Detect. Plrs with reliable Identify spells,
 * for example, should not want to also carry _Identify. This is for the '?'
 * auto-picker directive. This helper is for book-based magic only. */
int plr_can_auto_cast(int realm, int spell)
{
    caster_info *caster_ptr = get_caster_info();
    int          tval = realm2tval(realm);
    int          sval = spell/8;
    magic_type  *info;
    int          cost, fail;

    /* assume normal book based magic */
    assert(is_magic(realm));

    /* never automatically hurt the plr ... mana only! */
    if (caster_ptr && (caster_ptr->options & CASTER_USE_HP)) return 0;

    /* spell must be learned and the plr must have the required book */
    if (!_known_spell(realm, spell)) return 0;
    if (!pack_find_obj(tval, sval)) return 0;

    /* need the mana */
    info = &mp_ptr->info[realm - 1][spell];
    cost = mod_need_mana(info->smana, spell, realm);
    assert(cost); /* s/b always at least 1sp ... we are using cost as a boolean */
    if (plr->csp < cost) return 0;

    /* need reliable fail rates ... we won't roll for
     * success since that would be "disturbing". */
    fail = spell_chance(spell, realm);
    if (fail > 15) return 0;

    return cost;
}
bool plr_auto_cast(int realm, int spell)
{
    int cost = plr_can_auto_cast(realm, spell);
    if (cost && do_spell(realm, spell, SPELL_CAST))
    {
        sp_player(-cost);
        spell_stats_on_cast_old(realm, spell);
        return TRUE;
    }
    return FALSE;
}

/* targetting */
static point_t _get_pet_target(u32b mode)
{
    point_t pos = {0};
    bool old_target_pet = target_pet;

    target_pet = TRUE;
    pos = get_fire_pos_aux(mode);
    target_pet = old_target_pet;

    return pos;
}
static point_t _get_target(int gf)
{
    gf_info_ptr gfi = gf_lookup(gf);
    u32b mode = TARGET_KILL;

    if (gf == GF_LIGHT || gf == GF_DARK)
        mode |= TARGET_LOS;

    if (gfi->flags & GFF_TERRAIN) /* e.g. Stone to Mud */
        return get_aim_pos_aux(mode); /* skip auto_target */
    if (gfi->flags & GFF_TARGET_PET) /* heal|haste monster */
        return _get_pet_target(mode);
    return get_fire_pos_aux(mode);
}
point_t plr_get_target(int gf)
{
    return plr_get_target_aux(gf, DUN_PATH_MAX);
}
point_t plr_get_target_aux(int gf, int rng)
{
    point_t pos;
    project_length = rng; /* XXX */
    pos = _get_target(gf);
    project_length = 0;
    return pos;
}
point_t plr_get_beam_target(int gf)
{
    return plr_get_beam_target_aux(gf, DUN_PATH_MAX);
}
point_t plr_get_beam_target_aux(int gf, int rng)
{
    return plr_get_target_aux(gf, rng);
}
point_t plr_get_ball_target(int gf)
{
    return plr_get_ball_target_aux(gf, DUN_PATH_MAX);
}
point_t plr_get_ball_target_aux(int gf, int rng)
{
    point_t pos;
    project_length = rng; /* XXX */
    pos = get_fire_pos_aux(TARGET_KILL | TARGET_BALL);
    project_length = 0;
    return pos;
}
point_t plr_get_breath_target(int gf)
{
    return plr_get_breath_target_aux(gf, DUN_PATH_MAX);
}
point_t plr_get_breath_target_aux(int gf, int rng)
{
    point_t pos;
    u32b mode = TARGET_KILL | TARGET_BALL;

    if (gf == GF_LIGHT || gf == GF_DARK)
        mode |= TARGET_LOS;
    else if (gf == GF_DISINTEGRATE)
        mode |= TARGET_DISI;

    project_length = rng; /* XXX */
    pos = get_fire_pos_aux(mode);
    project_length = 0;
    return pos;
}

/* spell casting */
static void _bolt_msg(int gf)
{
    switch (gf)
    {
    case GF_ROCK: msg_print("You throw a huge boulder."); break;
    case GF_MANA: msg_print("You cast a mana bolt."); break;
    }
}
bool plr_cast_bolt(int gf, dice_t dice)
{
    return plr_cast_bolt_aux(gf, dice, DUN_PATH_MAX);
}
bool plr_cast_bolt_aux(int gf, dice_t dice, int rng)
{
    point_t p = plr_get_target_aux(gf, rng);
    if (!dun_pos_interior(plr_dun(), p)) return FALSE;
    _bolt_msg(gf);
    plr_bolt_aux(p, gf, dice_roll(dice), rng);
    return TRUE;
}
bool plr_cast_star_dust(int count, int gf, dice_t dice)
{
    int dir, n;
    point_t p;
    if (!get_fire_dir(&dir)) return FALSE;
    if (dir != 5)
        p = point_jump(plr->pos, dir, 20);
    else
    {
        point_t v;
        p = who_pos(plr->target);
        v = point_subtract(p, plr->pos); /* vector from plr to desired target */
        n = point_norm(v);
        if (!n) return FALSE;
        v = point_scale_Q(v, 20, n);       /* scale it */
        p = point_add(plr->pos, v);      /* new target (s/b distance of 20 from plr, modulo rounding errors) */
    }
    /* What's going on? @ is fighting a death mold, which he either
     * targetted or used dir of '6'. Either way, we compute p as 
     * indicated. For each "bolt", plr_star_dust computes a random
     * point within a ball of given spread (3). I've illustrated a
     * miss with randomly chosen q (note q + (1,0) would hit m, as
     * would most radius 2 or less points):
                     ****q.
                *****  ..... 
          ******      .......  
     @**** m          ...p...  
                      .......                               
                       .....
                        ...
     * Accuracy improves as m slides closer to @ since p remains fixed:
     * More line projections from @ to ball surrounding p go thru m.
     * At rng 4, all projections hit.
     * At rng 5, 4 miss points of 37. And so on. */
    /*msg_format("<color:R>(%d,%d)</color>", p.x - plr->pos.x, p.y - plr->pos.y);*/
    return plr_star_dust(count, 3, p, gf, dice);
}
bool plr_cast_beam(int gf, dice_t dice)
{
    return plr_cast_beam_aux(gf, dice, DUN_PATH_MAX);
}
bool plr_cast_beam_aux(int gf, dice_t dice, int rng)
{
    point_t p = plr_get_beam_target_aux(gf, rng);
    if (!dun_pos_interior(plr_dun(), p)) return FALSE;
    switch (gf) {
    case GF_LIGHT_WEAK: msg_print("A line of sunlight appears."); break; }
    plr_beam_aux(p, gf, dice_roll(dice), rng);
    return TRUE;
}
bool plr_cast_bolt_or_beam(int gf, dice_t dice, int beam_chance)
{
    point_t p = plr_get_target(gf);
    if (!dun_pos_interior(plr_dun(), p)) return FALSE;
    if (randint0(100) < beam_chance)
        plr_beam(p, gf, dice_roll(dice));
    else
        plr_bolt(p, gf, dice_roll(dice));
    return TRUE;
}
static void _ball_msg(int gf, int dam)
{
    if (gf == GF_HAND_DOOM) msg_print("You invoke the Hand of Doom!");
    else if (gf == GF_WATER) msg_print("You gesture fluidly.");
    else if (dam >= 400)
    {
        switch (gf) {
        case GF_MANA: msg_print("You invoke a mana storm!"); break;
        case GF_NETHER: msg_print("You invoke a nether storm!"); break;
        }
    }
    else if (dam >= 250)
    {
        switch (gf) {
        case GF_LIGHT: msg_print("You invoke a starburst!"); break;
        case GF_DARK: msg_print("You invoke a darkness storm!"); break;
        case GF_PSI_STORM: msg_print("You invoke a psionic storm!"); break; }
    }
}
bool plr_cast_ball(int rad, int gf, dice_t dice)
{
    return plr_cast_ball_aux(rad, gf, dice, DUN_PATH_MAX);
}
bool plr_cast_ball_aux(int rad, int gf, dice_t dice, int rng)
{
    int     dam;
    point_t p = plr_get_ball_target_aux(gf, rng);

    if (!dun_pos_interior(plr_dun(), p)) return FALSE;

    dam = dice_roll(dice);
    _ball_msg(gf, dam);
    plr_ball_aux(rad, p, gf, dam, rng);

    return TRUE;
}
bool plr_cast_wrath_of_god(int gf, dice_t dice)
{
    point_t p = get_fire_pos();
    if (!dun_pos_interior(plr_dun(), p)) return FALSE;
    return plr_wrath_of_god(p, gf, dice);
}
bool plr_cast_breath(int rad, int gf, dice_t dice)
{
    return plr_cast_breath_aux(rad, gf, dice, DUN_PATH_MAX);
}
bool plr_cast_breath_aux(int rad, int gf, dice_t dice, int rng)
{
    point_t p = plr_get_breath_target_aux(gf, rng);
    if (!dun_pos_interior(plr_dun(), p)) return FALSE;
    msg_format("You breathe %s.", gf_name(gf));
    plr_breath_aux(rad, p, gf, dice_roll(dice), rng);
    return TRUE;
}
bool plr_cast_burst(int rad, int gf, dice_t dice)
{
    plr_burst(rad, gf, dice_roll(dice));
    return TRUE;
}
bool plr_cast_rocket(int rad, dice_t dice)
{
    point_t p = get_fire_pos();
    if (!dun_pos_interior(plr_dun(), p)) return FALSE;
    msg_format("You launch a %s.", gf_name(GF_ROCKET));
    plr_rocket(rad, p, GF_ROCKET, dice_roll(dice));
    return TRUE;
}
bool plr_cast_direct(int gf, dice_t dice)
{
    mon_ptr mon = plr_target_mon();
    if (!mon) return FALSE;
    gf_affect_m(who_create_plr(), mon, gf, dice_roll(dice), GF_AFFECT_SPELL);
    return TRUE;
}

static bool _block_check(mon_ptr mon)
{
    if (mon->dun->id != plr->dun_id) return FALSE;
    if (mon->cdis > DUN_VIEW_MAX) return FALSE;
    return 3*plr->lev/2 >= _1d(mon_lvl(mon));
}
bool plr_block_magic(mon_ptr mon)
{
    if (plr->block_magic)
        return _block_check(mon);
    return FALSE;
}
bool plr_block_multiply(mon_ptr mon)
{
    if (plr->block_multiply)
        return _block_check(mon);
    return FALSE;
}
bool plr_block_steal(mon_ptr mon)
{
    if (plr->block_steal)
        return _block_check(mon);
    return FALSE;
}
bool plr_block_summon(mon_ptr mon)
{
    if (plr->block_summon)
        return _block_check(mon);
    return FALSE;
}
bool plr_block_teleport(mon_ptr mon)
{
    if (plr->block_teleport)
        return _block_check(mon);
    return FALSE;
}

