#ifndef INCLUDED_VARIANT_H
#define INCLUDED_VARIANT_H

/*
 * A simple variant type (Keep it that way!)
 */

#define VAR_NULL            0
#define VAR_STRING_INTERNAL    1
#define VAR_STRING_ALLOC    2
#define VAR_INT                3
#define VAR_BOOL            4

#define VAR_INTERNAL_STRING_SIZE  100

typedef struct {
    s16b tag;
    union {
       int n;
       cptr pc;
       char buf[VAR_INTERNAL_STRING_SIZE];
       bool b;
    } data;
} variant;

/* variant.c */
extern void var_init(variant *var);
extern void var_clear(variant *var);
/*extern void var_copy(variant *src, variant *dest);*/
extern bool var_is_null(variant *var);

extern void var_set_int(variant *var, int n);
extern void var_set_string(variant *var, cptr pc);
extern void var_set_bool(variant *var, bool b);

extern int var_get_int(variant *var);
extern cptr var_get_string(variant *var);
extern bool var_get_bool(variant *var);

#endif
