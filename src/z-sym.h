#ifndef INCLUDED_Z_SYM_H
#define INCLUDED_Z_SYM_H

#include "h-basic.h"

/* symbols are constant, unique strings 
 *
 * The sym_t for a string will potentially change each time you run the program,
 * as it depends on the ordering of calls to sym_add. But it will not change while
 * the program runs. A unique mapping of string->sym_t is maintained, so duplicate
 * adds return the same symbol (ie. sym_add is a lookup that creates a new symbol
 * each time it encounters a new string). So savefiles should write out strings,
 * not sym_t values.
 *
 * XXX sym_t is a sequential counter. 0 is sym_null so this type behaves as a "bool".
 * You, if desired, may use u16b for symbols, provided you don't need more than
 * 64k unique strings. Also, the null symbol 0 maps to the empty string "", which is
 * useful as a sentinel in savefiles. XXX
 * 
 * You can compare sym_t values: a == b <==> strcmp(sym_str(a), sym_str(b)) == 0
 */
typedef u16b sym_t;

extern void  sym_startup(void);
extern void  sym_shutdown(void);

extern sym_t sym_add(cptr str);   /* add new or return symbol for existing */
extern sym_t sym_find(cptr str);  /* return existing or sym_null */
extern cptr  sym_str(sym_t sym);  /* "dereference" a non-null sym */
extern bool  sym_equals(sym_t left, cptr right);

extern void  sym_doc(doc_ptr doc);
extern void  sym_wizard(void);
extern void  sym_test(void);

#endif
