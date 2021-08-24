#ifndef INCLUDED_PLR_H
#define INCLUDED_PLR_H

#include "plr_race.h"
#include "plr_class.h"

struct caster_info; /* XXX */
extern plr_ptr plr;

extern void    plr_startup(void);
extern void    plr_shutdown(void);
extern void    plr_wipe(void);

extern dun_ptr plr_dun(void);
extern dun_type_ptr plr_dun_type(void);

extern bool    plr_hook_auto_id(obj_ptr obj);
extern bool    plr_hook_auto_detect(void);
extern void    plr_hook_birth(void);
extern void    plr_hook_calc_bonuses(void);
extern void    plr_hook_calc_innate_attacks(void);
extern void    plr_hook_calc_stats(s16b stats[MAX_STATS]);
extern void    plr_hook_character_dump(doc_ptr doc);
extern bool    plr_hook_destroy_object(obj_ptr obj);
extern void    plr_hook_get_object(obj_ptr obj);
extern void    plr_hook_kill_monster(mon_ptr mon);
extern void    plr_hook_load(savefile_ptr file);
extern void    plr_hook_move_monster(mon_ptr mon);
extern void    plr_hook_move_player(void);
extern void    plr_hook_player_action(void);
extern void    plr_hook_process_player(void);
extern void    plr_hook_process_world(void);
extern void    plr_hook_prt_effects(doc_ptr doc);
extern void    plr_hook_register_timers(void);
extern void    plr_hook_startup(void);
extern void    plr_hook_save(savefile_ptr file);
extern void    plr_hook_update_light(void);

/* helpers for 'Monster Mode' */
extern bool    plr_mon_race_is_(cptr which);
extern void    plr_mon_race_set(cptr which);
extern void    plr_mon_race_evolve(cptr which);
extern mon_race_ptr plr_mon_race(void);
extern mon_race_ptr plr_boss_race(void);
extern equip_template_ptr plr_equip_template(void);

extern int     plr_can_auto_cast(int realm, int spell); /* return cost of spell (0 => FALSE) */
extern bool    plr_auto_cast(int realm, int spell); /* helper for Auto-ID and Auto-Detect (Spellbook Magic Only) */
extern bool    plr_mage_bonus(void);
extern bool    plr_allow_mage_quiver(void);
extern bool    plr_see_nocto(point_t pos);
extern int     plr_skill_sav(who_t who);
extern int     plr_ac(mon_ptr who);
extern plr_magic_ptr plr_magic(void);
extern int     plr_skill(int base); /* blind or hallucinating reduce skill */

extern mon_ptr plr_riding_mon(void);
extern mon_race_ptr plr_riding_race(void);
extern int     plr_riding_lvl(void);
extern void    plr_process_riding(void);
extern bool    plr_is_riding_(mon_ptr mon);

extern mon_ptr plr_target_mon(void);
extern mon_ptr plr_target_adjacent_mon(void);
extern mon_ptr plr_target_adjacent_pet(void);
extern bool    plr_touch_mon(mon_ptr mon, int gf, int dam); /* touch based attacks */

extern mon_pack_ptr plr_pets(void);
extern vec_ptr plr_pets_for_dismiss(void);
extern int     plr_pet_count(void);

extern int     plr_feeling_delay(dun_ptr dun);

/* plr_spells: provides a layer of target selection on top of dun_project */
extern point_t plr_get_target(int gf);
extern point_t plr_get_target_aux(int gf, int rng);
extern point_t plr_get_beam_target(int gf);
extern point_t plr_get_beam_target_aux(int gf, int rng);
extern point_t plr_get_ball_target(int gf);
extern point_t plr_get_ball_target_aux(int gf, int rng);
extern point_t plr_get_breath_target(int gf);
extern point_t plr_get_breath_target_aux(int gf, int rng);

extern bool    plr_cast_bolt(int gf, dice_t dice);
extern bool    plr_cast_bolt_aux(int gf, dice_t dice, int rng);
extern bool    plr_cast_beam(int gf, dice_t dice);
extern bool    plr_cast_beam_aux(int gf, dice_t dice, int rng);
extern bool    plr_cast_bolt_or_beam(int gf, dice_t dice, int beam_chance);
extern bool    plr_cast_ball(int rad, int gf, dice_t dice);
extern bool    plr_cast_ball_aux(int rad, int gf, dice_t dice, int rng);
extern bool    plr_cast_breath(int rad, int gf, dice_t dice);
extern bool    plr_cast_breath_aux(int rad, int gf, dice_t dice, int rng);
extern bool    plr_cast_burst(int rad, int gf, dice_t dice);
extern bool    plr_cast_direct(int gf, dice_t dice);
extern bool    plr_cast_rocket(int rad, dice_t dice);
extern bool    plr_cast_star_dust(int count, int gf, dice_t dice);
extern bool    plr_cast_wrath_of_god(int gf, dice_t dice);

extern bool    plr_block_magic(mon_ptr mon);
extern bool    plr_block_multiply(mon_ptr mon);
extern bool    plr_block_steal(mon_ptr mon);
extern bool    plr_block_summon(mon_ptr mon);
extern bool    plr_block_teleport(mon_ptr mon);
#endif
