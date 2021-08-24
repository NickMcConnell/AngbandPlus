                                                                                                                                                /*
 * File: z-expression.h
 * Purpose: Creating, storing, and deserializing simple math expressions
 */

#ifndef INCLUDED_Z_EXPRESSION_H
#define INCLUDED_Z_EXPRESSION_H

enum expression_err_e
{
    EXPRESSION_ERR_GENERIC = -1,
    EXPRESSION_ERR_INVALID_OPERATOR = -2,
    EXPRESSION_ERR_EXPECTED_OPERATOR = -3,
    EXPRESSION_ERR_EXPECTED_OPERAND = -4,
    EXPRESSION_ERR_DIVIDE_BY_ZERO = -5
};

typedef struct expression_operation_s expression_operation_t;
typedef struct expression_s expression_t;
typedef s32b (*expression_base_value_f)(void *);

extern expression_t *expression_new(void);
extern void expression_free(expression_t *expression);
extern expression_t *expression_copy(const expression_t *source);
extern void expression_set_base_value(expression_t *expression, expression_base_value_f function);
extern s32b expression_evaluate(expression_t const * const expression, void *data);
extern s16b expression_add_operations_string(expression_t *expression, const char *string);
extern bool expression_test_copy(const expression_t *a, const expression_t *b);

#endif /* INCLUDED_Z_EXPRESSION_H */