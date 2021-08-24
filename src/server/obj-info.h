/*
 * File: obj-info.h
 * Purpose: Object description code.
 */

#ifndef OBJECT_INFO_H
#define OBJECT_INFO_H

/*
 * Modes for object_info()
 */
enum
{
    OINFO_NONE  = 0x00, /* No options */
    OINFO_TERSE = 0x01 /* Keep descriptions brief, e.g. for dumps */
};

/*
 * Flags for effect descriptions
 */
enum
{
    EFINFO_NONE,
    EFINFO_HEAL,
    EFINFO_CONST,
    EFINFO_CURE,
    EFINFO_TIMED,
    EFINFO_STAT,
    EFINFO_SEEN,
    EFINFO_SUMM,
    EFINFO_TELE,
    EFINFO_QUAKE,
    EFINFO_BALL,
    EFINFO_BREATH,
    EFINFO_BOLT,
    EFINFO_BOLTD,
    EFINFO_TOUCH,
    EFINFO_TAP,
    EFINFO_MANA,
    EFINFO_ENCHANT,
    EFINFO_FOOD
};

extern void object_info(struct player *p, const struct object *obj, int mode);
extern void object_info_chardump(struct player *p, ang_file *f, const struct object *obj);

#endif /* OBJECT_INFO_H */
