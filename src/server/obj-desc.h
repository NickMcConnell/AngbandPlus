/*
 * File: obj-desc.h
 * Purpose: Create object name descriptions
 */

#ifndef OBJECT_DESC_H
#define OBJECT_DESC_H

/*
 * Modes for object_desc().
 */
enum
{
    ODESC_BASE      = 0x00,     /* Only describe the base name */
    ODESC_COMBAT    = 0x01,     /* Also show combat bonuses */
    ODESC_EXTRA     = 0x02,     /* Show charges/inscriptions/pvals */

    ODESC_FULL      = ODESC_COMBAT | ODESC_EXTRA, /* Show entire description */

    ODESC_STORE     = 0x04,     /* This is an in-store description */
    ODESC_PLURAL    = 0x08,     /* Always pluralise */
    ODESC_SINGULAR  = 0x10,     /* Always singular */
    ODESC_ARTIFACT  = 0x20,     /* Describe the base name for artifacts */
    ODESC_PREFIX    = 0x40,     /* Show prefix */
    ODESC_SALE      = 0x80,     /* Describe the base name for items purchased from floor */
    ODESC_TERSE     = 0x100,    /* Make terse names */
    ODESC_FLAVOR    = 0x200     /* Show flavor */
};

extern size_t object_desc(struct player *p, char *buf, size_t max, const struct object *obj,
    int mode);

#endif /* OBJECT_DESC_H */
