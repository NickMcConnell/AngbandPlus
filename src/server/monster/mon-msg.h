/*
 * File: mon-msg.h
 * Purpose: Structures and functions for monster messages.
 */

#ifndef MONSTER_MESSAGE_H
#define MONSTER_MESSAGE_H

/** Constants **/

/* The codified monster messages */
enum mon_messages
{
    MON_MSG_NONE = 0,

    /* project_m */
    MON_MSG_DIE,
    MON_MSG_DESTROYED,
    MON_MSG_RESIST_A_LOT,
    MON_MSG_HIT_HARD,
    MON_MSG_RESIST,
    MON_MSG_IMMUNE,
    MON_MSG_RESIST_SOMEWHAT,
    MON_MSG_UNAFFECTED,
    MON_MSG_SPAWN,
    MON_MSG_HEALTHIER,
    MON_MSG_FALL_ASLEEP,
    MON_MSG_WAKES_UP,
    MON_MSG_CRINGE_LIGHT,
    MON_MSG_SHRIVEL_LIGHT,
    MON_MSG_LOSE_SKIN,
    MON_MSG_DISSOLVE,
    MON_MSG_CATCH_FIRE,
    MON_MSG_BADLY_FROZEN,
    MON_MSG_SHUDDER,
    MON_MSG_CHANGE,
    MON_MSG_DISAPPEAR,
    MON_MSG_MORE_DAZED,
    MON_MSG_DAZED,
    MON_MSG_NOT_DAZED,
    MON_MSG_MORE_CONFUSED,
    MON_MSG_CONFUSED,
    MON_MSG_NOT_CONFUSED,
    MON_MSG_MORE_SLOWED,
    MON_MSG_SLOWED,
    MON_MSG_NOT_SLOWED,
    MON_MSG_MORE_HASTED,
    MON_MSG_HASTED,
    MON_MSG_NOT_HASTED,
    MON_MSG_MORE_AFRAID,
    MON_MSG_FLEE_IN_TERROR,
    MON_MSG_NOT_AFRAID,
    MON_MSG_MORIA_DEATH,
    MON_MSG_DISENTEGRATES,
    MON_MSG_FREEZE_SHATTER,
    MON_MSG_MANA_DRAIN,
    MON_MSG_BRIEF_PUZZLE,
    MON_MSG_MAINTAIN_SHAPE,

    /* message_pain */
    MON_MSG_UNHARMED,
    MON_MSG_95,
    MON_MSG_75,
    MON_MSG_50,
    MON_MSG_35,
    MON_MSG_20,
    MON_MSG_10,
    MON_MSG_0,

    /* PWMAngband */
    MON_MSG_BLEED,
    MON_MSG_NOT_BLEEDING,
    MON_MSG_MORE_BLEEDING,
    MON_MSG_POISONED,
    MON_MSG_NOT_POISONED,
    MON_MSG_MORE_POISONED,
    MON_MSG_BLIND,
    MON_MSG_NOT_BLIND,
    MON_MSG_MORE_BLIND,
    MON_MSG_PARALYZED,
    MON_MSG_NOT_PARALYZED,
    MON_MSG_MORE_PARALYZED,
    MON_MSG_DROP_DEAD,
    MON_MSG_HATE,
    MON_MSG_REACT,
    MON_MSG_RETURN,
    MON_MSG_TORN,
    MON_MSG_ICE,
    MON_MSG_EMBEDDED,
    MON_MSG_WAIL,
    MON_MSG_CROAK,

    /* Always leave this at the end */
    MAX_MON_MSG
};

/* Maxinum number of stacked monster messages */
#define MAX_STORED_MON_MSG      200
#define MAX_STORED_MON_CODES    400

/** Macros **/

/** Structures **/

/** Variables **/

/** Functions **/
extern void player_pain(int Ind, int who, int dam);
extern void message_pain(int Ind, struct monster *m_ptr, int dam);
extern bool add_monster_message(struct player *p, const char *mon_name, struct monster *m_ptr,
    int msg_code, bool delay);
extern void flush_all_monster_messages(struct player *p);

#endif /* MONSTER_MESSAGE_H */
