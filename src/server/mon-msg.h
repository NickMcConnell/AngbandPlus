/*
 * File: mon-msg.h
 * Purpose: Structures and functions for monster messages.
 */

#ifndef MONSTER_MESSAGE_H
#define MONSTER_MESSAGE_H

/*
 * Monster message constants
 */
enum mon_messages
{
    #define MON_MSG(x, t, o, s) MON_MSG_##x,
    #include "../common/list-mon-message.h"
    #undef MON_MSG
    MON_MSG_MAX
};

extern void message_pain(struct player *p, struct monster *mon, int dam);
extern bool add_monster_message(struct player *p, struct monster *mon, int msg_code, bool delay);
extern void show_monster_messages(struct player *p);
extern void player_pain(struct player *p, struct player *who, int dam);
extern void monmsg_init(struct player *p);
extern void monmsg_cleanup(struct player *p);

#endif /* MONSTER_MESSAGE_H */
