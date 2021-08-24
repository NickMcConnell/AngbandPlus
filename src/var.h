#ifndef INCLUDED_VAR_H
#define INCLUDED_VAR_H

/* Simple variant type for menus and spells.*/
enum {
    VAR_NULL = 0,
    VAR_STRING_INTERNAL,
    VAR_STRING,
    VAR_INT,
    VAR_BOOL
};

#define VAR_INTERNAL_STRING_SIZE  30

struct var_s {
    s16b tag;
    union {
       int   n;
       char *pc;
       char  buf[VAR_INTERNAL_STRING_SIZE];
       bool  b;
    } data;
};

typedef struct var_s var_t, *var_ptr;
typedef struct var_s variant; /* XXX */

extern var_t var_create(void); /* create #null ... same as var_t v = {0}; */
extern void  var_clear(var_ptr var); /* reset to #null */
extern void  var_destroy(var_ptr var); /* release dynamic memory ... always destroy your variants! */

extern bool  var_is_null(var_ptr var);

extern void  var_set_int(var_ptr var, int n);
extern void  var_set_string(var_ptr var, cptr pc);
extern void  var_set_bool(var_ptr var, bool b);
extern void  var_printf(var_ptr var, const char *fmt, ...);

extern int   var_get_int(var_ptr var);
extern cptr  var_get_string(var_ptr var);
extern bool  var_get_bool(var_ptr var);

#endif
