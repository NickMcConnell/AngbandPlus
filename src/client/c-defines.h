/*
 * File: c-defines.h
 * Purpose: Client constants and macro definitions
 */


#ifndef INCLUDED_C_DEFINES_H
#define INCLUDED_C_DEFINES_H      


/*
 * Maximum amount of Angband windows.
 */
#define ANGBAND_TERM_MAX 8


/*
 * Bit flags for the "get_item" function
 */
#define USE_EQUIP       0x01    /* Allow equip items */
#define USE_INVEN       0x02    /* Allow inven items */
#define USE_FLOOR       0x04    /* Allow floor items */
#define START_EQUIP     0x08    /* Start in equipment mode */
#define SHOW_PRICES     0x20    /* Show item prices in item lists */
#define SHOW_FAIL       0x40    /* Show device failure in item lists */
#define QUIVER_TAGS     0x80    /* 0-4 are quiver slots when selecting */


/*** Macro Definitions ***/


/*
 * Hack -- The main "screen"
 */
#define term_screen (angband_term[0])


#define SCAN_INSTANT    -1
#define SCAN_OFF        0


/*** PWMAngband ***/


/*
 * Maximum number of spells per realm
 */
#define BOOKS_PER_REALM 9


#define PMSG_TERM   4


#define term_chat (angband_term[PMSG_TERM])


typedef bool (*keypress_handler)(char *buf, size_t buflen, size_t *curs, size_t *len,
    struct keypress keypress, bool firsttime);


#endif /* INCLUDED_C_DEFINES_H */
