/*
 * File: history.h
 * Purpose: Player history tracking
 */

#ifndef HISTORY_H
#define HISTORY_H

extern void history_init(player_type *p_ptr);
extern void history_clear(player_type *p_ptr);
extern void history_wipe(player_type *p_ptr);
extern bool history_lose_artifact(player_type *p_ptr, const object_type *o_ptr);
extern void history_add(player_type *p_ptr, const char *event, u16b type, const object_type *o_ptr);
extern void history_add_unique(player_type *p_ptr, const char *event, u16b type);
extern bool history_add_artifact(player_type *p_ptr, const object_type *o_ptr, bool found);
extern void history_unmask_unknown(player_type *p_ptr);
extern void get_real_time(hturn *pturn, int* pd, int* ph, int* pm);
extern void dump_history(player_type *p_ptr, ang_file *file);

#endif /* HISTORY_H */