/*
 * File: player-common-calcs.h
 * Purpose: Player temporary status structures.
 */

#ifndef INCLUDED_PLAYER_COMMON_CALCS_H
#define INCLUDED_PLAYER_COMMON_CALCS_H

/*
 * Bit flags for the "player->upkeep->redraw" variable
 */
#define PR_MISC             0x00000001L /* Display Race/Class */
#define PR_TITLE            0x00000002L /* Display Title */
#define PR_LEV              0x00000004L /* Display Level */
#define PR_EXP              0x00000008L /* Display Experience */
#define PR_STATS            0x00000010L /* Display Stats */
#define PR_ARMOR            0x00000020L /* Display Armor */
#define PR_HP               0x00000040L /* Display Hitpoints */
#define PR_MANA             0x00000080L /* Display Mana */
#define PR_GOLD             0x00000100L /* Display Gold */
#define PR_HEALTH           0x00000200L /* Display Health Bar */
#define PR_SPEED            0x00000400L /* Display Extra (Speed) */
#define PR_STUDY            0x00000800L /* Display Extra (Study) */
#define PR_DEPTH            0x00001000L /* Display Depth */
#define PR_STATUS           0x00002000L
#define PR_DTRAP            0x00004000L /* Display Extra (Trap detection) */
#define PR_STATE            0x00008000L /* Display Extra (State) */
#define PR_MAP              0x00010000L /* Redraw whole map */
#define PR_INVEN            0x00020000L /* Display inven/equip */
#define PR_EQUIP            0x00040000L /* Display equip/inven */
#define PR_MESSAGE          0x00080000L /* Display messages */
#define PR_MONSTER          0x00100000L /* Display monster recall */
#define PR_OBJECT           0x00200000L /* Display object recall */
#define PR_MONLIST          0x00400000L /* Display monster list */
#define PR_ITEMLIST         0x00800000L /* Display item list */
/* PWMAngband */
#define PR_OTHER            0x01000000L /* Display other info */
#define PR_MESSAGE_CHAT     0x02000000L /* Display chat messages */
#define PR_SPELL            0x04000000L /* Display spell list */
#define PR_SPECIAL_INFO     0x08000000L /* Display special info */
#define PR_LAG              0x10000000L /* Display Lag Bar */
#define PR_PLUSSES          0x20000000L /* Display Plusses to Hit/Damage */
#define PR_CURSOR           0x40000000L /* Display Cursor */
#define PR_FLOOR            0x80000000L /* Display floor object */

/*
 * Display Basic Info
 */
#define PR_BASIC \
    (PR_MISC | PR_TITLE | PR_LEV | PR_EXP | PR_STATS | PR_ARMOR | PR_HP | \
     PR_MANA | PR_GOLD | PR_DEPTH | PR_EQUIP | PR_HEALTH | PR_SPEED)

/*
 * Display Extra Info
 */
#define PR_EXTRA \
    (PR_STATUS | PR_STATE | PR_STUDY)

/*
 * Display Subwindow Info
 */
#define PR_SUBWINDOW \
    (PR_MONSTER | PR_OBJECT | PR_MONLIST | PR_ITEMLIST | PR_MESSAGE)

#endif /* INCLUDED_PLAYER_COMMON_CALCS_H */
