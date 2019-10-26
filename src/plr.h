#ifndef INCLUDED_PLR_H
#define INCLUDED_PLR_H

#include "plr_race.h"
#include "plr_class.h"

struct player_type; /* XXX define this here */
typedef struct player_type plr_t, *plr_ptr;
extern plr_ptr p_ptr;
typedef struct plr_attack_info_s plr_attack_info_t, *plr_attack_info_ptr;

extern void    plr_startup(void);
extern void    plr_shutdown(void);
extern void    plr_wipe(void);

extern dun_ptr plr_dun(void);
extern dun_type_ptr plr_dun_type(void);

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
extern void    plr_hook_save(savefile_ptr file);

extern bool    plr_mage_bonus(void);
extern int     plr_skill_sav(int m_idx);

extern mon_ptr plr_riding_mon(void);
extern mon_race_ptr plr_riding_race(void);
extern int     plr_riding_lvl(void);

extern mon_ptr plr_target_mon(void);
extern mon_ptr plr_target_adjacent_mon(void);
extern bool    plr_touch_mon(mon_ptr mon, int gf, int dam); /* touch based attacks */

extern vec_ptr plr_pets(void);
extern vec_ptr plr_pets_for_dismiss(void);

extern int     plr_feeling_delay(dun_ptr dun);

#endif
