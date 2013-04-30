/*
 * File: obj-flag.h
 * Purpose: Definitions and functions for object flags
 */

#ifndef INCLUDED_OBJFLAG_H
#define INCLUDED_OBJFLAG_H

/*** Constants ***/

/* The object flag types */
enum object_flag_type
{
    OFT_NONE,
    OFT_PVAL,   /* pval-related but not to a stat */
    OFT_STAT,   /* affects a stat */
    OFT_SUST,   /* sustains a stat */
    OFT_SLAY,   /* a "normal" creature-type slay */
    OFT_BRAND,  /* a brand against monsters lacking the resist */
    OFT_KILL,   /* a powerful creature-type slay */
    OFT_VULN,   /* lowers resistance to an element */
    OFT_IMM,    /* offers immunity to an element */
    OFT_LRES,   /* a "base" elemental resistance */
    OFT_HRES,   /* a "high" elemental resistance */
    OFT_IGNORE, /* object ignores an element */
    OFT_HATES,  /* object can be destroyed by element */
    OFT_PROT,   /* protection from an effect */
    OFT_MISC,   /* a good property, suitable for ego items */
    OFT_LIGHT,  /* applicable only to light sources */
    OFT_MELEE,  /* applicable only to melee weapons */
    OFT_CURSE,  /* a "sticky" curse */
    OFT_BAD,    /* an undesirable flag that isn't a curse */
    OFT_INT,    /* an internal flag, not shown in the game */
    OFT_ESP,    /* an ESP flag */
    OFT_XRES,   /* an extra "high" elemental resistance */
    OFT_KNOW,   /* affects knowledge */

    OFT_MAX
};

/* How object flags are IDd */
enum object_flag_id
{
    OFID_NONE,      /* never shown */
    OFID_NORMAL,    /* normal ID on use */
    OFID_TIMED,     /* obvious after time */
    OFID_WIELD      /* obvious on wield */
};

/* Hack -- Special "xtra" object flag info (type) */
#define OBJECT_XTRA_TYPE_NONE       0
#define OBJECT_XTRA_TYPE_SUSTAIN    1
#define OBJECT_XTRA_TYPE_RESIST     2
#define OBJECT_XTRA_TYPE_POWER      3
#define OBJECT_XTRA_RESIST_POWER    4
#define OBJECT_XTRA_TYPE_ESP        5

/*** Structures ***/

/*
 * The object flag structure
 */
struct object_flag
{
    u16b index;     /* the OF_ index */
    bool pval;      /* is it granular (TRUE) or binary (FALSE) */
    u16b timed;     /* the corresponding TMD_ flag */
    u16b id;        /* how is it identified */
    u16b type;      /* OFT_ category */
    s16b power;     /* base power rating */
    s16b pval_mult; /* pval weight rating */
    s16b weapon;    /* power mult for melee weapon */
    s16b bow;       /* power mult for launcher */
    s16b ring;      /* etc. ... */
    s16b amulet;
    s16b light;
    s16b body;
    s16b cloak;
    s16b shield;
    s16b hat;
    s16b gloves;
    s16b boots;
    const char *message;    /* id message */
};

/*** Functions ***/

extern void create_mask(bitflag *f, bool id, ...);
extern void flag_message(struct player *p, int flag, char *name);
extern bool cursed_p(bitflag *f);
extern bool check_state_aux(struct player *p, bitflag f[OF_SIZE], int flag);
extern bool check_state(struct player *p, int flag);
extern s16b slot_mult(int flag, int slot);
extern s32b flag_power(int flag);
extern bool flag_uses_pval(int flag);
extern int obj_flag_type(int flag);
extern int pval_mult(int flag);

#endif /* INCLUDED_OBJFLAG_H */
