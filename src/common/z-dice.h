/*
 * File: z-dice.h
 * Purpose: Represent more complex dice than random_value
 */

#ifndef INCLUDED_Z_DICE_H
#define INCLUDED_Z_DICE_H

typedef struct dice_s dice_t;

extern dice_t *dice_new(void);
extern void dice_free(dice_t *dice);
extern bool dice_parse_string(dice_t *dice, const char *string);
extern void dice_random_value(dice_t *dice, void *data, random_value *v);
extern int dice_bind_expression(dice_t *dice, const char *name, const expression_t *expression);
extern int dice_evaluate(dice_t *dice, int level, aspect aspect, void *data, random_value *v);
extern int dice_roll(dice_t *dice, void *data, random_value *v);
extern bool dice_test_values(dice_t *dice, int base, int dice_count, int sides, int bonus);
extern bool dice_test_variables(dice_t *dice, const char *base, const char *dice_name,
    const char *sides, const char *bonus);

#endif /* INCLUDED_Z_DICE_H */