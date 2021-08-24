/*
 * File: store.h
 * Purpose: Store stocking
 */

#ifndef INCLUDED_STORE_H
#define INCLUDED_STORE_H

/*** Constants ***/

#define STORE_ORDERS    6   /* Number of store orders allowed (should be equal to min XBM slots) */

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

#define in_store(P) ((P)->store_num != -1)

/* Randomly select one of the entries in an array */
#define ONE_OF(x) x[randint0(N_ELEMENTS(x))]

/*** Variables ***/

extern struct store *stores;

/* Store orders */
extern char store_orders[STORE_ORDERS][NORMAL_WID];

/*** Functions ***/

extern void store_reset(void);
extern void store_shuffle(struct store *store, bool force);
extern void store_update(void);
extern s32b price_item(struct player *p, struct object *obj, bool store_buying, int qty);
extern struct object *store_carry(struct store *store, struct object *obj);
extern struct owner *store_ownerbyidx(struct store *s, unsigned int idx);
extern void do_cmd_buy(struct player *p, int item, int amt);
extern void store_examine(struct player *p, int item);
extern void store_order(struct player *p, const char *buf);
extern void do_cmd_sell(struct player *p, int item, int amt);
extern void store_confirm(struct player *p);
extern void do_cmd_store(struct player *p, int pstore);
extern bool check_store_drop(struct player *p, struct object *obj);
extern bool check_store_drop_color(struct player *p, struct object *obj, byte color);
extern bool get_player_store_name(int num, char *name, int len);
extern s32b player_price_item(struct player *p, struct object *obj);
extern void store_cancel_order(int order);
extern void store_get_order(int order, char *desc, int len);
extern bool store_will_buy_tester(struct player *p, const struct object *obj);

#endif /* INCLUDED_STORE_H */
