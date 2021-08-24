/*
 * File: player-calcs.h
 * Purpose: Player status calculation, signalling ui events based on status changes.
 */

#ifndef PLAYER_CALCS_H
#define PLAYER_CALCS_H

/*
 * Bit flags for the "player->upkeep->notice" variable
 */
#define PN_COMBINE      0x00000001L /* Combine the pack */
#define PN_IGNORE       0x00000008L /* Ignore stuff */
#define PN_MON_MESSAGE  0x00000010L /* Flush monster pain messages */
#define PN_WAIT         0x00000020L /* Wait (item request is pending) */

/*
 * Bit flags for the "player->upkeep->update" variable
 */
#define PU_BONUS        0x00000001L /* Calculate bonuses */
/* Calculate torch radius (PU_TORCH -- obsolete) */
/* Calculate chp and mhp (PU_HP -- obsolete) */
/* Calculate csp and msp (PU_MANA -- obsolete) */
#define PU_SPELLS       0x00000010L /* Calculate spells */
#define PU_FORGET_VIEW  0x00000020L /* Forget field of view */
#define PU_UPDATE_VIEW  0x00000040L /* Update field of view */
#define PU_FORGET_FLOW  0x00000080L /* Forget flow data */
#define PU_UPDATE_FLOW  0x00000100L /* Update flow data */
#define PU_MONSTERS     0x00000200L /* Update monsters */
#define PU_DISTANCE     0x00000400L /* Update distances */
#define PU_INVEN        0x00001000L /* Update inventory */

extern const byte adj_str_hold[STAT_RANGE];
extern const byte adj_str_blow[STAT_RANGE];
extern const byte adj_dex_safe[STAT_RANGE];
extern const byte adj_con_fix[STAT_RANGE];

extern bool obj_can_browse(struct player *p, const struct object *obj);
extern bool earlier_object(struct player *p, struct object *orig, struct object *newobj,
    bool store);
extern void calc_inventory(struct player *p);
extern int weight_limit(struct player_state *state);
extern int weight_remaining(struct player *p);
extern void calc_bonuses(struct player *p, struct player_state *state, bool known_only, bool update);
extern void calc_digging_chances(struct player *p, struct player_state *state,
    int chances[DIGGING_MAX]);
extern void health_track(struct player_upkeep *upkeep, struct actor *who);
extern void monster_race_track(struct player_upkeep *upkeep, struct actor *who);
extern void track_object(struct player_upkeep *upkeep, struct object *obj);
extern bool tracked_object_is(struct player_upkeep *upkeep, struct object *obj);
extern void cursor_track(struct player *p, struct actor *who);
extern void notice_stuff(struct player *p);
extern void update_stuff(struct player *p, struct chunk *c);
extern void handle_stuff(struct player *p);
extern void refresh_stuff(struct player *p);
extern bool monk_armor_ok(struct player *p);

#endif /* PLAYER_CALCS_H */
