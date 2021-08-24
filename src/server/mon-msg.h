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
    #define MON_MSG(x, s) MON_MSG_##x,
    #include "../common/list-mon-message.h"
    #undef MON_MSG
    MON_MSG_MAX
};

enum
{
    MON_DELAY_TAG_DEFAULT = 0,
    MON_DELAY_TAG_DEATH
};

/*
 * Maximum number of stacked monster messages
 */
#define MAX_STORED_MON_MSG      200
#define MAX_STORED_MON_CODES    400

enum mon_msg_flags
{
    MON_MSG_FLAG_HIDDEN = 0x01,
    MON_MSG_FLAG_OFFSCREEN = 0x02,
    MON_MSG_FLAG_INVISIBLE = 0x04
};

extern void player_pain(struct player *p, struct player *who, int dam);
extern void message_pain(struct player *p, struct monster *mon, int dam);
extern bool add_monster_message(struct player *p, const char *mon_name, struct monster *mon,
    int msg_code, bool delay);
extern void flush_all_monster_messages(struct player *p);

#endif /* MONSTER_MESSAGE_H */
