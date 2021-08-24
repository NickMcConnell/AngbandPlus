/*
 * File: list-mon-timed.h
 * Purpose: Timed monster properties
 *
 * Fields:
 * name - the index name for this timed flag
 * message_begin - the argument to the message code when the effect begins
 * message_end - the argument to the message code when the effect ends
 * message_increase - the argument to the message code when the effect increases
 * resist_flag - monsters with this monster race flag will resist this effect
 * max_timer - maximum that the timer for this effect can reach
 */

/* name  message_begin  message_end  message_increase  flag_resist  max_timer */
MON_TMD(SLEEP, MON_MSG_FALL_ASLEEP, MON_MSG_WAKES_UP, 0, RF_NO_SLEEP, 10000)
MON_TMD(STUN, MON_MSG_DAZED, MON_MSG_NOT_DAZED, MON_MSG_MORE_DAZED, RF_NO_STUN, 200)
MON_TMD(CONF, MON_MSG_CONFUSED, MON_MSG_NOT_CONFUSED, MON_MSG_MORE_CONFUSED, RF_NO_CONF, 200)
MON_TMD(FEAR, MON_MSG_FLEE_IN_TERROR, MON_MSG_NOT_AFRAID, MON_MSG_MORE_AFRAID, RF_NO_FEAR, 10000)
MON_TMD(SLOW, MON_MSG_SLOWED, MON_MSG_NOT_SLOWED, MON_MSG_MORE_SLOWED, 0, 50)
MON_TMD(FAST, MON_MSG_HASTED, MON_MSG_NOT_HASTED, MON_MSG_MORE_HASTED, 0, 50)
MON_TMD(POIS, MON_MSG_POISONED, MON_MSG_NOT_POISONED, MON_MSG_MORE_POISONED, RF_IM_POIS, 10000)
MON_TMD(CUT, MON_MSG_BLEED, MON_MSG_NOT_BLEEDING, MON_MSG_MORE_BLEEDING, 0, 10000)
MON_TMD(BLIND, MON_MSG_BLIND, MON_MSG_NOT_BLIND, MON_MSG_MORE_BLIND, RF_NO_SLEEP, 200)
MON_TMD(HOLD, MON_MSG_PARALYZED, MON_MSG_NOT_PARALYZED, MON_MSG_MORE_PARALYZED, RF_NO_STUN, 200)
