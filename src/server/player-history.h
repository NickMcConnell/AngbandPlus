/*
 * File: player-history.h
 * Purpose: Character auto-history creation and management
 */

#ifndef HISTORY_H
#define HISTORY_H

extern void history_clear(struct player *p);
extern void history_add_full(struct player *p, struct history_info *entry);
extern void history_add(struct player *p, const char *text, int type);
extern void history_find_artifact(struct player *p, const struct object *obj);
extern void history_generate_artifact(struct player *p, const struct object *obj);
extern void history_lose_artifact(struct player *p, const struct object *obj);
extern void history_add_unique(struct player *p, const char *event, int type);
extern void history_unmask_unknown(struct player *p);

#endif /* HISTORY_H */