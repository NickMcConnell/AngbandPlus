/*
 * File: ui-store.h
 * Purpose: Store UI
 */

#ifndef INCLUDED_UI_STORE_H
#define INCLUDED_UI_STORE_H

/* Easy names for the elements of the 'scr_places' arrays. */
enum
{
    LOC_PRICE = 0,
    LOC_OWNER,
    LOC_HEADER,
    LOC_MORE,
    LOC_HELP_CLEAR,
    LOC_HELP_PROMPT,
    LOC_AU,
    LOC_WEIGHT,

    LOC_MAX
};

struct store_context
{
    struct menu menu;       /* Menu instance */
    struct store *store;    /* Pointer to store */
    struct object *list;    /* List of objects */
    int flags;              /* Display flags */

    /* Places for the various things displayed onscreen */
    unsigned int scr_places_x[LOC_MAX];
    unsigned int scr_places_y[LOC_MAX];
};

typedef char store_name[NORMAL_WID];

extern struct store_context *store_ctx;
extern struct store current_store;
extern store_name *store_names;

extern void store_enter(void);
extern void store_prt_gold(void);
extern void store_prt_frame(void);
extern void store_sell_accept(s32b price, s16b reset);
extern void store_purchase_end(void);
extern void store_sell_end(void);
extern void store_leave(void);
extern bool check_store_leave(bool refresh);

#endif /* INCLUDED_UI_STORE_H */
