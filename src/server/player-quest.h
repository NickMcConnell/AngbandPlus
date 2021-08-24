/*
 * File: player-quest.h
 * Purpose: All quest-related code
 */

#ifndef QUEST_H
#define QUEST_H

/* Quest list */
extern struct quest *quests;

/* Functions */
extern bool is_quest(int level);
extern int quest_check(struct player *p, struct chunk *c, const struct monster *m);
extern bool is_quest_active(struct player *p, int level);
extern void start_quest(struct player *p);
extern void process_quest(struct player *p);
extern void end_quest(struct player *p, struct chunk *c, const struct monster *m);

extern struct file_parser quests_parser;

#endif /* QUEST_H */
