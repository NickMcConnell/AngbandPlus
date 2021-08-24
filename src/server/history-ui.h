/*
 * File: history-ui.h
 * Purpose: Character auto-history display
 */

#ifndef UI_HISTORY_H
#define UI_HISTORY_H

extern void get_real_time(hturn *pturn, int* pd, int* ph, int* pm);
extern void dump_history(struct player *p, ang_file *file);

#endif /* UI_HISTORY_H */
