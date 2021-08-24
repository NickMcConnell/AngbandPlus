#ifndef SQUELCH_H
#define SQUELCH_H

#include <src/structures.h>

/*
 * These are the base types for quelching on identification.
 * Some of the tvls are combined by ini_tv_to_type to make this
 * list a little more reasonable.
 */

enum
{
    TYPE_AMMO = 1,
    TYPE_BOW,
    TYPE_WEAPON1,
    TYPE_WEAPON2,
    TYPE_BODY,
    TYPE_CLOAK,
    TYPE_SHIELD,
    TYPE_HELM,
    TYPE_GLOVES,
    TYPE_BOOTS,
    TYPE_RING,
    TYPE_STAFF,
    TYPE_WAND,
    TYPE_ROD,
    TYPE_SCROLL,
    TYPE_POTION,
    TYPE_AMULET,
    TYPE_BOOK,
    TYPE_FOOD,
    TYPE_MISC,
    TYPE_MAX,
};


/*
 * List of kinds of item, for pseudo-id squelch.
 */
enum
{
    PS_TYPE_WEAPON_SHARP = 0,
    PS_TYPE_WEAPON_BLUNT,
    PS_TYPE_EQUIP_RARE,
    PS_TYPE_BOW,
    PS_TYPE_MISSILE_SLING,
    PS_TYPE_MISSILE_BOW,
    PS_TYPE_MISSILE_XBOW,
    PS_TYPE_ARMOR_ROBE,
    PS_TYPE_ARMOR_BODY,
    PS_TYPE_ARMOR_DRAGON,
    PS_TYPE_CLOAK,
    PS_TYPE_SHIELD,
    PS_TYPE_HELMS,
    PS_TYPE_CROWNS,
    PS_TYPE_GLOVES,
    PS_TYPE_BOOTS,
    PS_TYPE_DIGGER,
    PS_TYPE_RING,
    PS_TYPE_AMULET,
    PS_TYPE_LIGHT,
    PS_TYPE_MAX,
};

class quality_squelch_struct
{
public:
    byte squelch_type;
    int tval;
    int min_sval;
    int max_sval;
};


class quality_name_struct
{
public:
    int enum_val;
    QString name;
};

/*
 * Class for differentiating aware from unaware squelch
 */
class squelch_choice
{
public:
    s16b idx;
    bool aware;
};

/*
 * A class to hold a tval and its description
 */
class  tval_desc
{
public:
    int tval;
    QString desc;
};

// These numbers need to be changed when the tables are modified
#define QUALITY_MAPPING_MAX 28
#define RAW_TVALS_MAX       36

extern quality_squelch_struct quality_mapping[QUALITY_MAPPING_MAX];
extern quality_name_struct quality_choices[PS_TYPE_MAX];
extern quality_name_struct quality_values[SQUELCH_MAX];
extern tval_desc raw_tvals[RAW_TVALS_MAX];





#endif // SQUELCH_H
