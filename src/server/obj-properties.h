/*
 * File: obj-properties.h
 * Purpose: Definitions and functions for object flags and modifiers
 */

#ifndef INCLUDED_OBJPROPERTIES_H
#define INCLUDED_OBJPROPERTIES_H

/*
 * Constants
 */

/*
 * The object flag types
 */
enum object_flag_type
{
    OFT_NONE = 0,   /* placeholder flag */
    OFT_SUST,       /* sustains a stat */
    OFT_PROT,       /* protection from an effect */
    OFT_MISC,       /* a good property, suitable for ego items */
    OFT_LIGHT,      /* applicable only to light sources */
    OFT_MELEE,      /* applicable only to melee weapons */
    OFT_CURSE,      /* a "sticky" curse */
    OFT_BAD,        /* an undesirable flag that isn't a curse */
    OFT_ESP,        /* an ESP flag */
    OFT_KNOW,       /* affects knowledge */

    OFT_MAX
};

/*
 * How object flags are IDd
 */
enum object_flag_id
{
    OFID_NONE,      /* never shown */
    OFID_NORMAL,    /* normal ID on use */
    OFID_TIMED,     /* obvious after time */
    OFID_WIELD      /* obvious on wield */
};

/*
 * Structures
 */

/*
 * The object flag structure
 */
struct object_flag
{
    u16b index;             /* the OF_ index */
    u16b id;                /* how is it identified */
    u16b type;              /* OFT_ category */
    s16b power;             /* base power rating */
    const char *message;    /* id message */
};

/*
 * The object modifier structure
 */
struct object_mod
{
    u16b index;             /* the OBJ_MOD_ index */
    s16b power;             /* base power rating */
    s16b mod_mult;          /* modifier weight rating */
    const char *name;       /* modifier name */
};

/*
 * Functions
 */

extern int sustain_flag(int stat);
extern void create_mask(bitflag *f, bool id, ...);
extern bool cursed_p(const bitflag *f);
extern s16b flag_slot_mult(struct player *p, int flag, int slot);
extern s32b flag_power(int flag);
extern int obj_flag_type(int flag);
extern void flag_message(struct player *p, int flag, char *name);
extern s32b mod_power(int mod);
extern int mod_mult(int mod);
extern s16b mod_slot_mult(struct player *p, int mod, int slot);

#endif /* INCLUDED_OBJPROPERTIES_H */
