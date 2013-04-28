
/* PosBand -- A variant of Angband roguelike
 *
 * Copyright (c) 2004 Ben Harrison, Robert Ruehlmann and others
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 * 
 * NPPAngband Copyright (c) 2003-2004 Jeff Greene
 * PosBand Copyright (c) 2004-2005 Alexander Ulyanov
 */

/* options.h: options definitions */

#ifndef OPTIONS_H_INCLUDED
#define OPTIONS_H_INCLUDED

/*
 * Option indexes (offsets)
 *
 * These values are hard-coded by savefiles (and various pieces of code).
 */
#define OPT_USER_INTERFACE	0x00
#define OPT_DISTURBANCE		0x10
#define OPT_GAME_PLAY		0x20
#define OPT_EFFICIENCY		0x30
#define OPT_BIRTH		0x80
#define OPT_CHEAT		0xA0
#define OPT_ADULT		0xC0
#define OPT_SCORE		0xE0
#define OPT_NONE		0xFF
#define OPT_MAX			0x100

/*
 * Option indexes (normal)
 *
 * These values are hard-coded by savefiles.
 */
#define OPT_rogue_like_commands		0
#define OPT_quick_messages		1
#define OPT_floor_query_flag		2
#define OPT_carry_query_flag		3
#define OPT_use_old_target		4
#define OPT_always_pickup		5
#define OPT_always_repeat		6
#define OPT_depth_in_feet		7
#define OPT_stack_force_notes		8
#define OPT_stack_force_costs		9
#define OPT_show_labels			10
#define OPT_show_weights		11
#define OPT_show_choices		12
#define OPT_show_details		13
#define OPT_ring_bell			14
#define OPT_show_flavors		15
#define OPT_run_ignore_stairs		16
#define OPT_run_ignore_doors		17
#define OPT_run_cut_corners		18
#define OPT_run_use_corners		19
#define OPT_disturb_move		20
#define OPT_disturb_near		21
#define OPT_disturb_panel		22
#define OPT_disturb_state		23
#define OPT_disturb_minor		24
/* xxx OPT_disturb_other */
/* xxx OPT_alert_hitpoint */
/* xxx OPT_alert_failure */
#define OPT_verify_destroy		28
#define OPT_verify_special		29
#define OPT_allow_quantity		30
#define OPT_low_hp_careful		31
#define OPT_auto_haggle			32
#define OPT_auto_scum			33
/* xxx testing_stack */
/* xxx testing_carry */
#define OPT_expand_look			36
#define OPT_expand_list			37
#define OPT_view_perma_grids		38
#define OPT_view_torch_grids		39
#define OPT_dungeon_align		40
#define OPT_dungeon_stair		41
/* xxx track_follow */
/* xxx track_target */
#define OPT_smart_cheat			47
#define OPT_view_reduce_lite		48
#define OPT_hidden_player		49
#define OPT_avoid_abort			50
#define OPT_avoid_other			51
#define OPT_flush_failure		52
#define OPT_flush_disturb		53
/* xxx */
#define OPT_fresh_before		55
#define OPT_fresh_after			56
/* xxx */
#define OPT_compress_savefile		58
#define OPT_hilite_player		59
#define OPT_view_yellow_lite		60
#define OPT_view_bright_lite		61
#define OPT_view_granite_lite		62
#define OPT_view_special_lite		63
#define OPT_easy_open 			64
#define OPT_easy_alter 			65
#define OPT_easy_floor 			66
#define OPT_show_piles			67
#define OPT_center_player		68
#define OPT_run_avoid_center		69
#define OPT_scroll_target		70
#define OPT_auto_more			71
#define OPT_no_race_char		72
/* xxx */
#define OPT_hp_changes_color  		74
#define OPT_verify_leave_quest		75
#define OPT_mark_squelch_items		76
/* xxx */
/* xxx */
/* xxx */
/* xxx xxx */
#define OPT_birth_point_based		(OPT_BIRTH+0)
#define OPT_birth_auto_roller		(OPT_BIRTH+1)
#define OPT_birth_maximize		(OPT_BIRTH+2)
#define OPT_birth_preserve		(OPT_BIRTH+3)
#define OPT_birth_ironman		(OPT_BIRTH+4)
#define OPT_birth_no_stores		(OPT_BIRTH+5)
#define OPT_birth_no_artifacts		(OPT_BIRTH+6)
#define OPT_birth_rand_artifacts	(OPT_BIRTH+7)
#define OPT_birth_no_stacking		(OPT_BIRTH+8)
/* xxx OPT_birth_take_notes */
#define OPT_birth_force_small_lev	(OPT_BIRTH+10)
#define OPT_birth_retain_squelch	(OPT_BIRTH+11)
#define OPT_birth_themed_levels		(OPT_BIRTH+12)

/* xxx xxx */
#define OPT_cheat_peek			(OPT_CHEAT+0)
#define OPT_cheat_hear			(OPT_CHEAT+1)
#define OPT_cheat_room			(OPT_CHEAT+2)
#define OPT_cheat_xtra			(OPT_CHEAT+3)
#define OPT_cheat_know			(OPT_CHEAT+4)
#define OPT_cheat_live			(OPT_CHEAT+5)
/* xxx xxx */
#define OPT_adult_point_based		(OPT_ADULT+0)
#define OPT_adult_auto_roller		(OPT_ADULT+1)
#define OPT_adult_maximize		(OPT_ADULT+2)
#define OPT_adult_preserve		(OPT_ADULT+3)
#define OPT_adult_ironman		(OPT_ADULT+4)
#define OPT_adult_no_stores		(OPT_ADULT+5)
#define OPT_adult_no_artifacts		(OPT_ADULT+6)
#define OPT_adult_rand_artifacts	(OPT_ADULT+7)
#define OPT_adult_no_stacking		(OPT_ADULT+8)
/* xxx OPT_adult_take_notes */
#define OPT_adult_force_small_lev   	(OPT_ADULT+10)
#define OPT_adult_retain_squelch	(OPT_ADULT+11)
#define OPT_adult_themed_levels		(OPT_ADULT+12)
/* xxx xxx */
#define OPT_score_peek			(OPT_SCORE+0)
#define OPT_score_hear			(OPT_SCORE+1)
#define OPT_score_room			(OPT_SCORE+2)
#define OPT_score_xtra			(OPT_SCORE+3)
#define OPT_score_know			(OPT_SCORE+4)
#define OPT_score_live			(OPT_SCORE+5)
/* xxx xxx */

/*
 * Hack -- Option symbols
 */
#define rogue_like_commands		op_ptr->opt[OPT_rogue_like_commands]
#define quick_messages			op_ptr->opt[OPT_quick_messages]
#define floor_query_flag		op_ptr->opt[OPT_floor_query_flag]
#define carry_query_flag		op_ptr->opt[OPT_carry_query_flag]
#define use_old_target			op_ptr->opt[OPT_use_old_target]
#define always_pickup			op_ptr->opt[OPT_always_pickup]
#define always_repeat			op_ptr->opt[OPT_always_repeat]
#define depth_in_feet			op_ptr->opt[OPT_depth_in_feet]
#define stack_force_notes		op_ptr->opt[OPT_stack_force_notes]
#define stack_force_costs		op_ptr->opt[OPT_stack_force_costs]
#define show_labels			op_ptr->opt[OPT_show_labels]
#define show_weights			op_ptr->opt[OPT_show_weights]
#define show_choices			op_ptr->opt[OPT_show_choices]
#define show_details			op_ptr->opt[OPT_show_details]
#define ring_bell			op_ptr->opt[OPT_ring_bell]
#define show_flavors			op_ptr->opt[OPT_show_flavors]
#define run_ignore_stairs		op_ptr->opt[OPT_run_ignore_stairs]
#define run_ignore_doors		op_ptr->opt[OPT_run_ignore_doors]
#define run_cut_corners			op_ptr->opt[OPT_run_cut_corners]
#define run_use_corners			op_ptr->opt[OPT_run_use_corners]
#define disturb_move			op_ptr->opt[OPT_disturb_move]
#define disturb_near			op_ptr->opt[OPT_disturb_near]
#define disturb_panel			op_ptr->opt[OPT_disturb_panel]
#define disturb_state			op_ptr->opt[OPT_disturb_state]
#define disturb_minor			op_ptr->opt[OPT_disturb_minor]
/* xxx disturb_other */
/* xxx */
/* xxx alert_failure */
#define verify_destroy			op_ptr->opt[OPT_verify_destroy]
#define verify_special			op_ptr->opt[OPT_verify_special]
#define allow_quantity			op_ptr->opt[OPT_allow_quantity]
#define low_hp_careful			op_ptr->opt[OPT_low_hp_careful]
#define auto_haggle			op_ptr->opt[OPT_auto_haggle]
#define auto_scum			op_ptr->opt[OPT_auto_scum]
/* xxx testing_stack */
/* xxx testing_carry */
#define expand_look			op_ptr->opt[OPT_expand_look]
#define expand_list			op_ptr->opt[OPT_expand_list]
#define view_perma_grids		op_ptr->opt[OPT_view_perma_grids]
#define view_torch_grids		op_ptr->opt[OPT_view_torch_grids]
#define dungeon_align			op_ptr->opt[OPT_dungeon_align]
#define dungeon_stair			op_ptr->opt[OPT_dungeon_stair]
/* xxx track_follow */
/* xxx track_target */
#define smart_cheat			op_ptr->opt[OPT_smart_cheat]
#define view_reduce_lite		op_ptr->opt[OPT_view_reduce_lite]
#define hidden_player			op_ptr->opt[OPT_hidden_player]
#define avoid_abort			op_ptr->opt[OPT_avoid_abort]
#define avoid_other			op_ptr->opt[OPT_avoid_other]
#define flush_failure			op_ptr->opt[OPT_flush_failure]
#define flush_disturb			op_ptr->opt[OPT_flush_disturb]
/* xxx */
#define fresh_before			op_ptr->opt[OPT_fresh_before]
#define fresh_after			op_ptr->opt[OPT_fresh_after]
/* xxx */
#define compress_savefile		op_ptr->opt[OPT_compress_savefile]
#define hilite_player			op_ptr->opt[OPT_hilite_player]
#define view_yellow_lite		op_ptr->opt[OPT_view_yellow_lite]
#define view_bright_lite		op_ptr->opt[OPT_view_bright_lite]
#define view_granite_lite		op_ptr->opt[OPT_view_granite_lite]
#define view_special_lite		op_ptr->opt[OPT_view_special_lite]
#define easy_open			op_ptr->opt[OPT_easy_open]
#define easy_alter			op_ptr->opt[OPT_easy_alter]
#define easy_floor			op_ptr->opt[OPT_easy_floor]
#define show_piles			op_ptr->opt[OPT_show_piles]
#define center_player			op_ptr->opt[OPT_center_player]
#define run_avoid_center		op_ptr->opt[OPT_run_avoid_center]
#define scroll_target			op_ptr->opt[OPT_scroll_target]
#define auto_more			op_ptr->opt[OPT_auto_more]
#define no_race_char			op_ptr->opt[OPT_no_race_char]
/* xxx */
/* xxx */
/* xxx */
/* xxx */
/* xxx */
/* xxx xxx */
#define birth_point_based		op_ptr->opt[OPT_birth_point_based]
#define birth_auto_roller		op_ptr->opt[OPT_birth_auto_roller]
#define birth_maximize			op_ptr->opt[OPT_birth_maximize]
#define birth_preserve			op_ptr->opt[OPT_birth_preserve]
#define birth_ironman			op_ptr->opt[OPT_birth_ironman]
#define birth_no_stores			op_ptr->opt[OPT_birth_no_stores]
#define birth_no_artifacts		op_ptr->opt[OPT_birth_no_artifacts]
#define birth_rand_artifacts		op_ptr->opt[OPT_birth_rand_artifacts]
#define birth_no_stacking    		op_ptr->opt[OPT_birth_no_stacking]
/* xxx birth_take_notes */
#define	birth_force_small_lev		op_ptr->opt[OPT_birth_force_small_lev]
#define birth_retain_squelch		op_ptr->opt[OPT_birth_retain_squelch]
#define birth_themed_levels		op_ptr->opt[OPT_birth_themed_levels]
/* xxx xxx */
#define cheat_peek			op_ptr->opt[OPT_cheat_peek]
#define cheat_hear			op_ptr->opt[OPT_cheat_hear]
#define cheat_room			op_ptr->opt[OPT_cheat_room]
#define cheat_xtra			op_ptr->opt[OPT_cheat_xtra]
#define cheat_know			op_ptr->opt[OPT_cheat_know]
#define cheat_live			op_ptr->opt[OPT_cheat_live]
/* xxx xxx */
#define adult_point_based		op_ptr->opt[OPT_adult_point_based]
#define adult_auto_roller		op_ptr->opt[OPT_adult_auto_roller]
#define adult_maximize			op_ptr->opt[OPT_adult_maximize]
#define adult_preserve			op_ptr->opt[OPT_adult_preserve]
#define adult_ironman			op_ptr->opt[OPT_adult_ironman]
#define adult_no_stores			op_ptr->opt[OPT_adult_no_stores]
#define adult_no_artifacts		op_ptr->opt[OPT_adult_no_artifacts]
#define adult_rand_artifacts		op_ptr->opt[OPT_adult_rand_artifacts]
#define adult_no_stacking		op_ptr->opt[OPT_adult_no_stacking]
/* xxx adult_take_notes */
#define	adult_force_small_lev		op_ptr->opt[OPT_adult_force_small_lev]
#define adult_retain_squelch		op_ptr->opt[OPT_adult_retain_squelch]
#define adult_themed_levels		op_ptr->opt[OPT_adult_themed_levels]
#define hp_changes_color  		op_ptr->opt[OPT_hp_changes_color]
#define verify_leave_quest 		op_ptr->opt[OPT_verify_leave_quest]
#define mark_squelch_items		op_ptr->opt[OPT_mark_squelch_items]
#define score_peek			op_ptr->opt[OPT_score_peek]
#define score_hear			op_ptr->opt[OPT_score_hear]
#define score_room			op_ptr->opt[OPT_score_room]
#define score_xtra			op_ptr->opt[OPT_score_xtra]
#define score_know			op_ptr->opt[OPT_score_know]
#define score_live			op_ptr->opt[OPT_score_live]
/* xxx xxx */

/*
 * Information for "do_cmd_options()".
 */
#define OPT_PAGE_MAX			7
#define OPT_PAGE_PER			20

extern cptr option_text[OPT_MAX];
extern cptr option_desc[OPT_MAX];
extern const bool option_norm[OPT_MAX];
extern const byte option_page[OPT_PAGE_MAX][OPT_PAGE_PER];

#endif /* OPTIONS_H_INCLUDED */
