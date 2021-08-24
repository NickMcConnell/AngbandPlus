/*
 * File: list-mon-temp-flags.h
 * Purpose: Temporary monster flags
 */

MFLAG(NONE, "")
MFLAG(VIEW, "Monster is in line of sight")                          /* player PoV */
MFLAG(ACTIVE, "Monster is in active mode")                          /* monster PoV */
MFLAG(VISIBLE, "Monster is visible")                                /* player PoV */
MFLAG(CAMOUFLAGE, "Player doesn't know this is a monster")          /* monster PoV */
MFLAG(AWARE, "Monster is aware of the player")                      /* monster PoV */
MFLAG(HANDLED, "Monster has been processed this turn")              /* monster PoV */
MFLAG(TRACKING, "Monster is tracking the player by sound or scent") /* monster PoV */
MFLAG(HURT, "Monster is hurt")                                      /* player PoV */
