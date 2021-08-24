/*
 * File: obj-power.h
 * Purpose: Calculation of object power
 */

#ifndef OBJECT_POWER_H
#define OBJECT_POWER_H

/*
 * Constants for the power algorithm:
 * - fudge factor for extra damage from rings etc. (used if extra blows)
 * - assumed damage for off-weapon brands
 * - base power for jewelry
 * - base power for armour items (for halving acid damage)
 * - power per point of damage
 * - power per point of +to_hit
 * - power per point of base AC
 * - power per point of +to_ac
 * (these four are all halved in the algorithm)
 * - assumed max blows
 * - inhibiting values for +blows/might/shots/immunities (max is one less)
 */
#define NONWEAP_DAMAGE          15  /* Fudge to boost extra blows */
#define WEAP_DAMAGE             12  /* And for off-weapon combat flags */
#define BASE_JEWELRY_POWER      4
#define BASE_ARMOUR_POWER       1
#define BASE_TOOL_POWER         5   /* PWMAngband: adjust this if necessary */
#define DAMAGE_POWER            5
#define TO_HIT_POWER            3
#define BASE_AC_POWER           2
#define TO_AC_POWER             2
#define MAX_BLOWS               5

/*
 * Some constants used in randart generation and power calculation
 * - thresholds for limiting to_hit, to_dam and to_ac
 * - fudge factor for rescaling ammo cost
 * (a stack of this many equals a weapon of the same damage output)
 */
#define INHIBIT_POWER   20000
#define INHIBIT_BLOWS   3   /* PWMAngband: limit extra blows/shots/might to +2 */
#define INHIBIT_MIGHT   3
#define INHIBIT_SHOTS   3
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

/* PWMAngband: limit damage power on artifact ammo */
#define HIGH_TO_DAM_AMMO     6
#define VERYHIGH_TO_DAM_AMMO 11
#define INHIBIT_TO_DAM_AMMO  16

extern s32b object_power(struct player *p, const struct object* obj);
extern s32b object_value_real(struct player *p, const struct object *obj, int qty);
extern s32b object_value(struct player *p, const struct object *obj, int qty);

#endif /* OBJECT_POWER_H */
