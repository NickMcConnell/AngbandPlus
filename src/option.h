/* option.h */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 2, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */

/* KBB: various assumptions about these are checked by static asserts in birth.c */

/*** Option Definitions ***/

/*
 * Option indexes (offsets)
 *
 * These values are hard-coded by savefiles (and various pieces of code).
 */
#define OPT_BIRTH					128
#define OPT_CHEAT					160
#define OPT_ADULT					192
#define OPT_SCORE					224
#define OPT_NONE					255
#define OPT_MAX						256


/*
 * Option indexes (normal)
 *
 * These values are hard-coded by savefiles.
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
/* xxx OPT_disturb_minor */
/* xxx OPT_disturb_other */
/* xxx OPT_alert_hitpoint */
/* xxx OPT_alert_failure */
#define OPT_verify_destroy			28
/* xxx OPT_verify_special */
/* xxx OPT_allow_quantity */
/* xxx */
/* xxx OPT_auto_haggle */
/* xxx */
/* xxx testing_stack */
/* xxx testing_carry */
#define OPT_expand_look				36
/* xxx OPT_expand_list */
#define OPT_view_perma_grids		38
#define OPT_view_torch_grids		39
/* xxx */
/* xxx */
/* xxx */
/* xxx */
/* xxx track_follow */
/* xxx track_target */
/* xxx */
/* xxx */
#define OPT_view_reduce_lite		48
#define OPT_hidden_player			49
#define OPT_avoid_abort				50
#define OPT_avoid_other				51
#define OPT_flush_failure			52
#define OPT_flush_disturb			53
/* xxx */
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
/* xxx */
#define OPT_auto_more				71
/* xxx */
/* xxx */
/* xxx */
/* xxx */
/* xxx */
/* xxx */
/* xxx */
/* xxx */
/* xxx xxx */
#define OPT_birth_point_based       (OPT_BIRTH+0)
#define OPT_birth_auto_roller       (OPT_BIRTH+1)
#define OPT_birth_maximize          (OPT_BIRTH+2)
#define OPT_birth_preserve          (OPT_BIRTH+3)
#define OPT_birth_ironman           (OPT_BIRTH+4)
#define OPT_birth_no_stores         (OPT_BIRTH+5)
#define OPT_birth_no_artifacts      (OPT_BIRTH+6)
#define OPT_birth_rand_artifacts    (OPT_BIRTH+7)
#define OPT_birth_no_stacking       (OPT_BIRTH+8)
#define OPT_birth_auto_scum			(OPT_BIRTH+9)
#define OPT_birth_dungeon_align		(OPT_BIRTH+10)
#define OPT_birth_dungeon_stair		(OPT_BIRTH+11)
#define OPT_birth_flow_by_sound		(OPT_BIRTH+12)
#define OPT_birth_flow_by_smell		(OPT_BIRTH+13)
#define OPT_birth_smart_monsters	(OPT_BIRTH+14)
#define OPT_birth_smart_packs		(OPT_BIRTH+15)
#define OPT_birth_smart_learn		(OPT_BIRTH+16)
#define OPT_birth_smart_cheat		(OPT_BIRTH+17)

/* xxx xxx */
#define OPT_cheat_peek				(OPT_CHEAT+0)
#define OPT_cheat_hear				(OPT_CHEAT+1)
#define OPT_cheat_room				(OPT_CHEAT+2)
#define OPT_cheat_xtra				(OPT_CHEAT+3)
#define OPT_cheat_know				(OPT_CHEAT+4)
#define OPT_cheat_live				(OPT_CHEAT+5)

/* adult and score options are mirrors of birth and cheat options, respectively.  They must be synchronized. */
/* xxx xxx */
#define OPT_adult_point_based		(OPT_ADULT+0)
#define OPT_adult_auto_roller		(OPT_ADULT+1)
#define OPT_adult_maximize			(OPT_ADULT+2)
#define OPT_adult_preserve			(OPT_ADULT+3)
#define OPT_adult_ironman			(OPT_ADULT+4)
#define OPT_adult_no_stores			(OPT_ADULT+5)
#define OPT_adult_no_artifacts		(OPT_ADULT+6)
#define OPT_adult_rand_artifacts	(OPT_ADULT+7)
#define OPT_adult_no_stacking		(OPT_ADULT+8)
#define OPT_adult_auto_scum			(OPT_ADULT+9)
#define OPT_adult_dungeon_align		(OPT_ADULT+10)
#define OPT_adult_dungeon_stair		(OPT_ADULT+11)
#define OPT_adult_flow_by_sound		(OPT_ADULT+12)
#define OPT_adult_flow_by_smell		(OPT_ADULT+13)
#define OPT_adult_smart_monsters	(OPT_ADULT+14)
#define OPT_adult_smart_packs		(OPT_ADULT+15)
#define OPT_adult_smart_learn		(OPT_ADULT+16)
#define OPT_adult_smart_cheat		(OPT_ADULT+17)
/* xxx xxx */
#define OPT_score_peek				(OPT_SCORE+0)
#define OPT_score_hear				(OPT_SCORE+1)
#define OPT_score_room				(OPT_SCORE+2)
#define OPT_score_xtra				(OPT_SCORE+3)
#define OPT_score_know				(OPT_SCORE+4)
#define OPT_score_live				(OPT_SCORE+5)
/* xxx xxx */

#ifdef NDEBUG
#define OPTION(A) (op_ptr->opt[OPT_##A])
#else
#define OPTION(A) (assert(NULL!=options[OPT_##A].text && !strcmp(options[OPT_##A].text,#A)),op_ptr->opt[OPT_##A])
#endif


/*
 * Information for "do_cmd_options()".
 */
#define OPT_PAGE_MAX				7
#define OPT_PAGE_PER				20

#ifndef __cplusplus
typedef struct option_def option_def;
#endif

struct option_def
{
	const char* text;
	const char* desc;
	bool norm;
};

/* option.c */
extern const option_def options[OPT_MAX];
extern const unsigned char option_page[OPT_PAGE_MAX][OPT_PAGE_PER];

