/*
 * File: store.h
 * Purpose: Store stocking and UI (server side)
 */

#ifndef INCLUDED_STORE_H
#define INCLUDED_STORE_H

/*** Constants ***/

#define STORE_TURNS     250 /* Number of turns between turnovers */
#define STORE_SHUFFLE   25  /* 1/Chance (per day) of an owner changing */
#define STORE_MIN_KEEP  6   /* Min slots to "always" keep full */
#define STORE_MAX_KEEP  18  /* Max slots to "always" keep full */

#define PLAYER_STORE_GENERAL    10
#define PLAYER_STORE_ARMOURY    8
#define PLAYER_STORE_SMITH      9
#define PLAYER_STORE_TEMPLE     5
#define PLAYER_STORE_ALCHEMIST  4
#define PLAYER_STORE_MAGIC      6
#define PLAYER_STORE_LIBRARY    7
#define PLAYER_STORE_BM         1
#define PLAYER_STORE_XBM        3
#define PLAYER_STORE_TAVERN     2

/*** Variables ***/

/* Store orders */
extern char store_orders[STORE_MIN_KEEP][NORMAL_WID];

/*** Functions ***/

extern void store_init(void);
extern void free_stores(void);
extern void store_reset(void);
extern void store_shuffle(struct store *store, bool force);
extern void store_maint(struct store *store, bool force);
extern s32b price_item(struct player *p, object_type *o_ptr, bool store_buying, int qty);
extern struct owner *store_ownerbyidx(struct store *s, unsigned int idx);
extern void store_purchase(int Ind, int item, int amt);
extern void store_examine(int Ind, int item);
extern void store_order(int Ind, const char *buf);
extern void store_sell(int Ind, int item, int amt);
extern void store_confirm(int Ind);
extern void do_cmd_store(int Ind, int pstore);
extern bool check_store_drop(struct player *p, object_type *o_ptr);
extern bool check_store_drop_color(struct player *p, object_type *o_ptr, byte color);
extern bool get_player_store_name(int num, char *name, int len);
extern s32b player_price_item(int Ind, object_type *o_ptr);
extern void store_cancel_order(int order);
extern void store_get_order(int order, char *desc, int len);

#endif /* INCLUDED_STORE_H */
