/*
 * File: channel.h
 * Purpose: Chat channels
 */

#ifndef CHANNEL_H
#define CHANNEL_H

/*
 * Chat Channels defines
 */

/* Channel modes */
#define CM_KEYLOCK      0x01 /* +k More similar to IRC's +A */
#define CM_SECRET       0x02 /* +s */
#define CM_MODERATE     0x04 /* +m */
#define CM_SERVICE      0x08 /* +! service channel */
#define CM_PLOG         0x10 /* +p log to plog */

/* User-channel modes */
#define UCM_EAR         0x01
#define UCM_VOICE       0x02
#define UCM_OPER        0x04
#define UCM_BAN         0x08

#define UCM_LEAVE       (UCM_EAR | UCM_VOICE | UCM_OPER)

#define VIRTUAL_CHANNELS 8

#define on_channel(P, I) ((P)->on_channel[(I)] & UCM_EAR)

/* can_talk(p, channel_index) test:
 * Line 1. Present on channel
 * Line 2. Not banned
 * Line 3. Not moderated
 * Line 4. Moderated, BUT
 * Line 5.  user has Voice or Op */
#define can_talk(P, I) \
    (on_channel((P),(I)) && \
    !((P)->on_channel[(I)] & UCM_BAN) && \
    (!(channels[(I)].mode & CM_MODERATE) || ( \
    (channels[(I)].mode & CM_MODERATE) && \
    ((P)->on_channel[(I)] & UCM_VOICE || (P)->on_channel[(I)] & UCM_OPER) \
    )))

#define clog(C, M) if (chan_ ## C) msg_channel(chan_ ## C, (M))
#define audit(M) clog(audit, (M))
#define debug(M) clog(debug, (M))
#define cheat(M) clog(cheat, (M))

/*
 * Default chat channel
 */
#define DEFAULT_CHANNEL "#public"

extern int chan_audit;
extern int chan_debug;
extern int chan_cheat;

extern void channel_join(struct player *p, const char *channel, bool quiet);
extern void channel_leave(struct player *p, const char *channel);
extern void channels_leave(struct player *p);

#endif /* CHANNEL_H */
