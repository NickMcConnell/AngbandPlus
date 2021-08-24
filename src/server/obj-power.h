/*
 * File: obj-power.h
 * Purpose: Calculation of object power
 */

#ifndef OBJECT_POWER_H
#define OBJECT_POWER_H

/*
 * Some constants used in randart generation and power calculation
 * - thresholds for limiting to_hit, to_dam and to_ac
 * - fudge factor for rescaling ammo cost
 * (a stack of this many equals a weapon of the same damage output)
 */
#define INHIBIT_POWER   20000
#define INHIBIT_BLOWS   3   /* PWMAngband: limit extra blows/might to +2 */
#define INHIBIT_MIGHT   3
#define INHIBIT_SHOTS   11
#define HIGH_TO_AC      26
#define VERYHIGH_TO_AC  36
#define INHIBIT_AC      56
#define HIGH_TO_HIT     16
#define VERYHIGH_TO_HIT 26
#define INHIBIT_TO_HIT  41
#define HIGH_TO_DAM     16
#define VERYHIGH_TO_DAM 26
#define INHIBIT_TO_DAM  41
#define AMMO_RESCALER   20

enum power_calc_operation
{
    POWER_CALC_NONE,
    POWER_CALC_ADD,
    POWER_CALC_ADD_IF_POSITIVE,
    POWER_CALC_SQUARE_ADD_IF_POSITIVE,
    POWER_CALC_MULTIPLY,
    POWER_CALC_DIVIDE,
    POWER_CALC_MAX
};

struct iterate
{
    int property_type;
    int max;
};

struct power_calc
{
    struct power_calc *next;
    char *name;                     /* Name of the calculation */
    struct poss_item *poss_items;
    dice_t *dice;                   /* Dice expression used in the calculation */
    int operation;                  /* How the calculation operates on power */
    struct iterate iterate;         /* What the calculation iterates over */
    char *apply_to;                 /* What the calculation is applied to */
};

extern struct power_calc *calculations;

extern expression_base_value_f power_calculation_by_name(const char *name);

extern int object_power(struct player *p, const struct object* obj);
extern int object_value_real(struct player *p, const struct object *obj, int qty);
extern int object_value(struct player *p, const struct object *obj, int qty);

#endif /* OBJECT_POWER_H */
