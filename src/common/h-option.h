/*
 * File: h-options.h
 * Purpose: Options table and definitions.
 */


#ifndef INCLUDED_H_OPTIONS_H
#define INCLUDED_H_OPTIONS_H


/*** Option definitions ***/


/*
 * Option indexes (offsets)
 *
 * These values are hard-coded by savefiles (and various pieces of code).
 */
#define OPT_NONE    59
#define OPT_MAX     60


/*
 * Option indexes (normal)
 *
 * These values are hard-coded by savefiles.
 */
#define OPT_use_sound               0
#define OPT_rogue_like_commands     1
#define OPT_use_old_target          2
#define OPT_pickup_always           3
#define OPT_pickup_inven            4
#define OPT_easy_open               5
#define OPT_active_auto_retaliator  6
#define OPT_pause_after_detect      7

#define OPT_hp_changes_color        15
#define OPT_center_player           16
#define OPT_show_flavors            17
#define OPT_view_yellow_light       18
#define OPT_animate_flicker         19
#define OPT_purple_uniques          20
#define OPT_view_orange_light       21
#define OPT_highlight_leader        22

#define OPT_disturb_move            30
#define OPT_disturb_near            31
#define OPT_disturb_detect          32
#define OPT_disturb_state           33
#define OPT_notify_recharge         34
#define OPT_disturb_panel           35
#define OPT_auto_accept             36
#define OPT_disturb_icky            37

#define OPT_birth_ironman           45
#define OPT_birth_no_stores         46
#define OPT_birth_no_artifacts      47
#define OPT_birth_no_feelings       48
#define OPT_birth_no_selling        49
#define OPT_birth_no_ghost          50
#define OPT_birth_fruit_bat         51


#define OPT(opt_name) Client_setup.options[OPT_##opt_name]
#define OPT_P(P, opt_name) (P)->other.opt[OPT_##opt_name]


#endif /* INCLUDED_H_OPTIONS_H */
