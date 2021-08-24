#ifndef INCLUDED_SHOP_H
#define INCLUDED_SHOP_H

#include "inv.h"

extern bool store_hack;

/************************************************************************
 * Shops
 ***********************************************************************/
struct shop_s;
typedef struct shop_s shop_t, *shop_ptr;

enum /* shared with FEAT_BLDG implementation */
{
    SHOP_NONE = 0,
    SHOP_GENERAL,
    SHOP_ARMORY,
    SHOP_WEAPON,
    SHOP_TEMPLE,
    SHOP_ALCHEMIST,
    SHOP_MAGIC,
    SHOP_BLACK_MARKET,
    SHOP_HOME,
    SHOP_BOOK,
    SHOP_MUSEUM,
    SHOP_JEWELER,
    SHOP_COUNT
};

extern shop_ptr shop_load(savefile_ptr file);
extern void     shop_free(shop_ptr shop);

extern void     shop_ui(shop_ptr shop);

extern void     shop_reset(shop_ptr shop); /* for dungeon shops */
extern bool     shop_common_cmd_handler(int cmd); /* shared with home_ui */
extern void     shop_display_inv(doc_ptr doc, inv_ptr inv, slot_t top, int page_size);
extern void     shop_save(shop_ptr shop, savefile_ptr file);

/************************************************************************
 * Buildings
 ***********************************************************************/
struct bldg_s;
typedef struct bldg_s bldg_t, *bldg_ptr;

enum /* shared with FEAT_BLDG implementation */
{
    BLDG_NONE = 0,
    BLDG_INN,
    BLDG_CASTLE,
    BLDG_FIGHTERS_GUILD,
    BLDG_ARCHERS_GUILD,
    BLDG_THIEVES_GUILD,
    BLDG_WIZARDS_GUILD,
    BLDG_PRIESTS_GUILD,
    BLDG_HUNTERS_OFFICE,
};

extern bldg_ptr bldg_load(savefile_ptr file);
extern void     bldg_save(bldg_ptr bldg, savefile_ptr file);
extern void     bldg_free(bldg_ptr bldg);
extern void     bldg_ui(bldg_ptr bldg);

extern void     display_wanted_uniques(void);

/************************************************************************
 * Towns
 ***********************************************************************/
typedef struct town_s town_t, *town_ptr;
struct town_s
{
   int         id;
   int         level;
   char       *name;
   char       *file;
   int       (*mon_alloc_f)(mon_race_ptr race, int prob);
   bool      (*owner_race_p)(int race_id);
   void      (*kill_mon_f)(mon_ptr mon);
   void      (*populate_f)(dun_ptr dun, rect_t rect);
   point_t     world_pos;
   int_map_ptr shops;
   int_map_ptr bldgs;
   u32b        flags;
};
#define TF_VISITED  0x0001
#define TF_SECRET   0x0002
#define TF_FRIENDLY 0x0004

extern void town_reset(town_ptr town);

enum
{
    TOWN_NONE = 0,
    TOWN_RANDOM,
    /* Middle Earth */
    TOWN_RIVENDELL,
    TOWN_BEORN,
    TOWN_LAKETOWN,
    TOWN_EDORAS,
    TOWN_MINAS_TIRITH,
    TOWN_OSGILIATH,
    TOWN_MORANNON,
    /* Amber */
    TOWN_OUTPOST,
    TOWN_ANGWIL,
    TOWN_TELMORA,
    TOWN_MORIVANT,
    TOWN_ZUL,
};

extern void     towns_reset_world(void);

extern int      towns_parse(cptr name);
extern town_ptr towns_lookup(int which);

extern town_ptr towns_current_town(void);
extern town_ptr towns_choose(u32b flags);
extern void     towns_save(savefile_ptr file);
extern void     towns_load(savefile_ptr file);

extern room_ptr towns_get_map(int town_id);

extern town_ptr town_alloc(int which, cptr name);
extern void     town_free(town_ptr town);

extern shop_ptr town_get_shop(town_ptr town, int which);
extern bldg_ptr town_get_bldg(town_ptr town, int which);

extern void     town_on_visit(int which);
extern cptr     town_name(int which);
extern void     town_save(town_ptr town, savefile_ptr file);
extern void     town_load(town_ptr town, savefile_ptr file);

#endif
