#ifndef INCLUDED_MON_AI_H
#define INCLUDED_MON_AI_H

extern void mon_process(mon_ptr mon); 
extern void mon_process_aux(mon_ptr mon); 
extern mon_ptr mon_process_current(void);
extern u32b mon_process_current_id(void);

extern void mon_set_target(mon_ptr mon, point_t pos);
extern void mon_clear_target(mon_ptr mon);
extern bool mon_has_valid_target(mon_ptr mon);
extern point_t mon_fuzzy_pos(mon_ptr mon, point_t pos);

/* Precompute distance offsets for optimization. This is used by _find_safety,
 * _find_hiding and dun_blast_ball.
 * The MAX_PRECOMPUTE_DISTANCE restricts the max radius supported by spell projection */
#define MAX_PRECOMPUTE_DISTANCE 10
extern point_vec_ptr distance_offsets(int dis);
#endif
