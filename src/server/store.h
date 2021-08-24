/*
 * File: store.h
 * Purpose: Store stocking
 */

#ifndef INCLUDED_STORE_H
#define INCLUDED_STORE_H

/*** Constants ***/

#define STORE_ORDERS    8   /* Number of store orders allowed (should be equal to min XBM slots) */

#define in_store(P) ((P)->store_num != -1)

/* Randomly select one of the entries in an array */
#define ONE_OF(x) x[randint0(N_ELEMENTS(x))]

/*** Variables ***/

extern int store_max;
extern struct store *stores;

/* Store orders */
struct store_order
{
    char order[NORMAL_WID];
    hturn turn;
};
extern struct store_order store_orders[STORE_ORDERS];

/*** Functions ***/

extern struct store *store_at(struct player *p);
extern void store_reset(void);
extern void store_shuffle(struct store *store, bool force);
extern void store_update(void);
extern s32b price_item(struct player *p, struct object *obj, bool store_buying, int qty);
extern void store_stock_list(struct player *p, struct store *store, struct object **list, int n);
extern struct object *home_carry(struct player *p, struct store *store, struct object *obj);
extern struct object *store_carry(struct player *p, struct store *store, struct object *obj);
extern struct owner *store_ownerbyidx(struct store *s, unsigned int idx);
extern void do_cmd_buy(struct player *p, int item, int amt);
extern void do_cmd_retrieve(struct player *p, int item, int amt);
extern void store_examine(struct player *p, int item, bool describe);
extern void store_order(struct player *p, const char *buf);
extern void do_cmd_sell(struct player *p, int item, int amt);
extern void do_cmd_stash(struct player *p, int item, int amt);
extern void store_confirm(struct player *p);
extern void do_cmd_store(struct player *p, int pstore);
extern bool check_store_drop(struct player *p);
extern bool get_player_store_name(int num, char *name, int len);
extern s32b player_price_item(struct player *p, struct object *obj);
extern void store_cancel_order(int order);
extern void store_get_order(int order, char *desc, int len);
extern bool store_will_buy_tester(struct player *p, const struct object *obj);

#endif /* INCLUDED_STORE_H */
