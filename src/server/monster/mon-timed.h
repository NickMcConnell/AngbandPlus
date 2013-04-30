/*
 * File: mon-timed.h
 * Purpose: Structures and functions for monster timed effects.
 */

#ifndef MONSTER_TIMED_H
#define MONSTER_TIMED_H

/** Constants **/

/** Macros **/

/* Flags for the monster timed functions */
#define MON_TMD_FLG_NOTIFY      0x01    /* Give notification */
#define MON_TMD_MON_SOURCE      0x02    /* Monster is causing the damage */
#define MON_TMD_FLG_NOMESSAGE   0x04    /* Never show a message */
#define MON_TMD_FLG_NOFAIL      0x08    /* Never fail the tests. */

/** Structures **/

/** Variables **/

/** Functions **/
extern bool mon_inc_timed(struct player *p, struct monster *m_ptr, int ef_idx, int timer, u16b flag,
    bool id);
extern bool mon_dec_timed(struct player *p, struct monster *m_ptr, int ef_idx, int timer, u16b flag,
    bool id);
extern bool mon_clear_timed(struct player *p, struct monster *m_ptr, int ef_idx, u16b flag, bool id);

#endif /* MONSTER_TIMED_H */
