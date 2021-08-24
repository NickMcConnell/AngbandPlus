/*
 * File: obj-properties.h
 * Purpose: Definitions and functions for object properties
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
    OFT_BAD,        /* an undesirable flag */
    OFT_DIG,        /* applicable only to diggers */
    OFT_THROW,      /* applicable only to throwables */
    OFT_ESP,        /* an ESP flag */
    OFT_OTHER,      /* other flags (auto-id, magic) */

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
 * The object property types
 */
enum obj_property_type
{
    OBJ_PROPERTY_NONE = 0,
    OBJ_PROPERTY_STAT,
    OBJ_PROPERTY_MOD,
    OBJ_PROPERTY_FLAG,
    OBJ_PROPERTY_IGNORE,
    OBJ_PROPERTY_RESIST,
    OBJ_PROPERTY_VULN,
    OBJ_PROPERTY_IMM,
    OBJ_PROPERTY_MAX
};

/*
 * Structures
 */

/*
 * The object property structure
 */
struct obj_property
{
    struct obj_property *next;
    int type;               /* type of property */
    int subtype;            /* subtype of property */
    int id_type;            /* how the property is identified (flags only?) */
    int index;              /* index of the property for its type */
    int power;              /* base power rating */
    int mult;               /* relative weight rating */
    int type_mult[TV_MAX];  /* relative weight rating specific to object type */
    char *name;             /* property name */
    char *adjective;        /* adjective for property */
    char *neg_adj;          /* adjective for negative of property */
    char *msg;              /* message on noticing property */
    char *desc;             /* extra text for object info */
    char *short_desc;       /* short description for random powers/resists */
};

extern struct obj_property *obj_properties;

/*
 * Functions
 */

extern struct obj_property *lookup_obj_property(int type, int index);
extern void create_obj_flag_mask(bitflag *f, int id, ...);
extern void flag_message(struct player *p, int flag, char *name);
extern int sustain_flag(int stat);

#endif /* INCLUDED_OBJPROPERTIES_H */
