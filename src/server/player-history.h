/*
 * File: player-history.h
 * Purpose: Character auto-history creation and management
 */

#ifndef HISTORY_H
#define HISTORY_H

extern void history_init(struct player *p);
extern void history_clear(struct player *p);
extern void history_wipe(struct player *p);
extern bool history_lose_artifact(struct player *p, const struct object *obj);
extern void history_add(struct player *p, const char *event, int type, const struct object *obj);
extern void history_add_unique(struct player *p, const char *event, int type);
extern bool history_add_artifact(struct player *p, const struct object *obj, bool found);
extern void history_unmask(struct player *p);

#endif /* HISTORY_H */