#ifndef INCLUDED_OPTIONS_H
#define INCLUDED_OPTIONS_H


const char *option_name(int opt);
const char *option_desc(int opt);

void option_set(int opt, bool on);
void option_set_defaults(void);


/*** Option display definitions ***/

/*
 * Information for "do_cmd_options()".
 */
#define OPT_PAGE_MAX				7
#define OPT_PAGE_PER				20

/* The option data structures */
extern const byte option_page[OPT_PAGE_MAX][OPT_PAGE_PER];



/*** Option definitions ***/

/*
 * Option indexes (offsets)
 *
 * These values are hard-coded by savefiles (and various pieces of code).  Ick.
 */
#define OPT_BIRTH					128
#define OPT_CHEAT					160
#define OPT_ADULT					192
#define OPT_SCORE					224
#define OPT_NONE					255
#define OPT_MAX						256


/*
 * Option indexes (hard-coded by savefiles)
 */
#define OPT_rogue_like_commands		0
#define OPT_quick_messages			1
#define OPT_floor_query_flag		2
#define OPT_carry_query_flag		3
#define OPT_use_old_target			4
#define OPT_always_pickup			5
#define OPT_always_repeat			6
#define OPT_depth_in_feet			7
#define OPT_stack_force_notes		8
/* xxx OPT_stack_force_costs */
#define OPT_show_labels				10
#define OPT_show_weights			11
#define OPT_show_choices			12
#define OPT_show_details			13
#define OPT_ring_bell				14
#define OPT_show_flavors			15
#define OPT_run_ignore_stairs		16
#define OPT_run_ignore_doors		17
#define OPT_run_cut_corners			18
#define OPT_run_use_corners			19
#define OPT_disturb_move			20
#define OPT_disturb_near			21
#define OPT_disturb_panel			22
#define OPT_disturb_state			23
#define OPT_disturb_minor			24
/* xxx OPT_disturb_other */
/* xxx OPT_alert_hitpoint */
/* xxx OPT_alert_failure */
#define OPT_verify_destroy			28
/* xxx OPT_verify_special */
/* xxx OPT_allow_quantity */
/* xxx */
/* xxx OPT_auto_haggle */
#define OPT_auto_scum				33
/* xxx testing_stack */
/* xxx testing_carry */
#define OPT_expand_look				36
/* xxx OPT_expand_list */
#define OPT_view_perma_grids		38
#define OPT_view_torch_grids		39
#define OPT_dungeon_align			40
#define OPT_dungeon_stair			41
#define OPT_flow_by_sound			42
#define OPT_flow_by_smell			43
/* xxx track_follow */
/* xxx track_target */
#define OPT_smart_learn				46
#define OPT_smart_cheat				47
#define OPT_view_reduce_lite		48
#define OPT_hidden_player			49
#define OPT_avoid_abort				50
#define OPT_avoid_other				51
#define OPT_flush_failure			52
#define OPT_flush_disturb			53

#define OPT_compress_savefile		58
#define OPT_hilite_player			59
#define OPT_view_yellow_lite		60
#define OPT_view_bright_lite		61
#define OPT_view_granite_lite		62
#define OPT_view_special_lite		63
#define OPT_easy_open 				64
#define OPT_easy_alter 				65
#define OPT_easy_floor 				66
#define OPT_show_piles				67
#define OPT_center_player			68
#define OPT_run_avoid_center		69

#define OPT_auto_more				71
#define OPT_smart_monsters			72
#define OPT_smart_packs				73

#define OPT_birth_point_based       (OPT_BIRTH+0)
#define OPT_birth_auto_roller       (OPT_BIRTH+1)
#define OPT_birth_maximize          (OPT_BIRTH+2)
#define OPT_birth_preserve          (OPT_BIRTH+3)
#define OPT_birth_ironman           (OPT_BIRTH+4)
#define OPT_birth_no_stores         (OPT_BIRTH+5)
#define OPT_birth_no_artifacts      (OPT_BIRTH+6)
#define OPT_birth_rand_artifacts    (OPT_BIRTH+7)
#define OPT_birth_no_stacking       (OPT_BIRTH+8)

#define OPT_cheat_peek				(OPT_CHEAT+0)
#define OPT_cheat_hear				(OPT_CHEAT+1)
#define OPT_cheat_room				(OPT_CHEAT+2)
#define OPT_cheat_xtra				(OPT_CHEAT+3)
#define OPT_cheat_know				(OPT_CHEAT+4)
#define OPT_cheat_live				(OPT_CHEAT+5)

#define OPT_adult_point_based		(OPT_ADULT+0)
#define OPT_adult_auto_roller		(OPT_ADULT+1)
#define OPT_adult_maximize			(OPT_ADULT+2)
#define OPT_adult_preserve			(OPT_ADULT+3)
#define OPT_adult_ironman			(OPT_ADULT+4)
#define OPT_adult_no_stores			(OPT_ADULT+5)
#define OPT_adult_no_artifacts		(OPT_ADULT+6)
#define OPT_adult_rand_artifacts	(OPT_ADULT+7)
#define OPT_adult_no_stacking		(OPT_ADULT+8)

#define OPT_score_peek				(OPT_SCORE+0)
#define OPT_score_hear				(OPT_SCORE+1)
#define OPT_score_room				(OPT_SCORE+2)
#define OPT_score_xtra				(OPT_SCORE+3)
#define OPT_score_know				(OPT_SCORE+4)
#define OPT_score_live				(OPT_SCORE+5)


/*
 * Hack -- Option symbols
 *
 * These shouldn't even be here.
 */
#define OPT(opt_name)	op_ptr->opt[OPT_##opt_name]

#define rogue_like_commands		OPT(rogue_like_commands)
#define quick_messages			OPT(quick_messages)
#define floor_query_flag		OPT(floor_query_flag)
#define carry_query_flag		OPT(carry_query_flag)
#define use_old_target			OPT(use_old_target)
#define always_pickup			OPT(always_pickup)
#define always_repeat			OPT(always_repeat)
#define depth_in_feet			OPT(depth_in_feet)
#define stack_force_notes		OPT(stack_force_notes)
/* #define stack_force_costs		OPT(stack_force_costs) */
#define show_labels				OPT(show_labels)
#define show_weights			OPT(show_weights)
#define show_choices			OPT(show_choices)
#define show_details			OPT(show_details)
#define ring_bell				OPT(ring_bell)
#define show_flavors			OPT(show_flavors)
#define run_ignore_stairs		OPT(run_ignore_stairs)
#define run_ignore_doors		OPT(run_ignore_doors)
#define run_cut_corners			OPT(run_cut_corners)
#define run_use_corners			OPT(run_use_corners)
#define disturb_move			OPT(disturb_move)
#define disturb_near			OPT(disturb_near)
#define disturb_panel			OPT(disturb_panel)
#define disturb_state			OPT(disturb_state)
#define disturb_minor			OPT(disturb_minor)
/* xxx disturb_other */
/* xxx alert_failure */
#define verify_destroy			OPT(verify_destroy)
/* #define verify_special			OPT(verify_special) */
/* #define allow_quantity			OPT(allow_quantity) */

/* auto_haggle */
#define auto_scum				OPT(auto_scum)

#define expand_look				OPT(expand_look)
/* #define expand_list				OPT(expand_list) */
#define view_perma_grids		OPT(view_perma_grids)
#define view_torch_grids		OPT(view_torch_grids)
#define dungeon_align			OPT(dungeon_align)
#define dungeon_stair			OPT(dungeon_stair)
#define flow_by_sound			OPT(flow_by_sound)
#define flow_by_smell			OPT(flow_by_smell)

#define smart_learn				OPT(smart_learn)
#define smart_cheat				OPT(smart_cheat)
#define view_reduce_lite		OPT(view_reduce_lite)
#define hidden_player			OPT(hidden_player)
#define avoid_abort				OPT(avoid_abort)
#define avoid_other				OPT(avoid_other)
#define flush_failure			OPT(flush_failure)
#define flush_disturb			OPT(flush_disturb)

#define compress_savefile		OPT(compress_savefile)
#define hilite_player			OPT(hilite_player)
#define view_yellow_lite		OPT(view_yellow_lite)
#define view_bright_lite		OPT(view_bright_lite)
#define view_granite_lite		OPT(view_granite_lite)
#define view_special_lite		OPT(view_special_lite)
#define easy_open				OPT(easy_open)
#define easy_alter				OPT(easy_alter)
#define easy_floor				OPT(easy_floor)
#define show_piles				OPT(show_piles)
#define center_player			OPT(center_player)
#define run_avoid_center		OPT(run_avoid_center)

#define auto_more				OPT(auto_more)
#define smart_monsters			OPT(smart_monsters)
#define smart_packs				OPT(smart_packs)

#define birth_point_based		OPT(birth_point_based)
#define birth_auto_roller		OPT(birth_auto_roller)
#define birth_maximize			OPT(birth_maximize)
#define birth_preserve			OPT(birth_preserve)
#define birth_ironman			OPT(birth_ironman)
#define birth_no_stores			OPT(birth_no_stores)
#define birth_no_artifacts		OPT(birth_no_artifacts)
#define birth_rand_artifacts	OPT(birth_rand_artifacts)
#define birth_no_stacking       OPT(birth_no_stacking)

#define cheat_peek				OPT(cheat_peek)
#define cheat_hear				OPT(cheat_hear)
#define cheat_room				OPT(cheat_room)
#define cheat_xtra				OPT(cheat_xtra)
#define cheat_know				OPT(cheat_know)
#define cheat_live				OPT(cheat_live)

#define adult_point_based		OPT(adult_point_based)
#define adult_auto_roller		OPT(adult_auto_roller)
#define adult_maximize			OPT(adult_maximize)
#define adult_preserve			OPT(adult_preserve)
#define adult_ironman			OPT(adult_ironman)
#define adult_no_stores			OPT(adult_no_stores)
#define adult_no_artifacts		OPT(adult_no_artifacts)
#define adult_rand_artifacts	OPT(adult_rand_artifacts)
#define adult_no_stacking		OPT(adult_no_stacking)

#define score_peek				OPT(score_peek)
#define score_hear				OPT(score_hear)
#define score_room				OPT(score_room)
#define score_xtra				OPT(score_xtra)
#define score_know				OPT(score_know)
#define score_live				OPT(score_live)


#endif /* !INCLUDED_OPTIONS_H */
