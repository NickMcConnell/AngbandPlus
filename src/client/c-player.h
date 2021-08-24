/*
 * File: c-player.h
 * Purpose: Player information (client side)
 */

#ifndef C_PLAYER_H
#define C_PLAYER_H

extern struct player *player;
extern char title[NORMAL_WID];
extern char party_info[160];
extern bool map_active;
extern s16b last_line_info;
extern int special_line_type;
extern char special_line_header[ANGBAND_TERM_MAX][NORMAL_WID];
extern s16b stat_roll[STAT_MAX + 1];
extern bool party_mode;

#endif /* C_PLAYER_H */
