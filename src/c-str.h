#ifndef INCLUDED_C_STRING_H
#define INCLUDED_C_STRING_H

#include <stdio.h>
#include "c-vec.h"

typedef struct str_s str_t, *str_ptr;

/* XXX support stack objects (cf poschengband.h discussion) */
struct str_s
{
    int   size;
    int   len;
    char *buf;
};

extern void str_create(str_ptr str, int size);
extern void str_destroy(str_ptr str);

/* Create a new str object (You must str_free()) */
extern str_ptr str_alloc(void);
extern str_ptr str_alloc_format(const char *fmt, ...);
extern str_ptr str_alloc_size(int size);

extern str_ptr str_copy(str_ptr str);
extern str_ptr str_copy_s(const char *val);
extern str_ptr str_copy_sn(const char *val, int cb);

extern str_ptr str_read_file(FILE *fp);

/* Destroy a str object */
extern void str_free(str_ptr str);

extern void str_clear(str_ptr str);

/* String Building */
extern void str_append(str_ptr str, str_ptr to_append);
extern void str_append_c(str_ptr str, char ch);
extern void str_append_s(str_ptr str, const char *val);
extern void str_append_sn(str_ptr str, const char *val, int cb);
extern void str_append_file(str_ptr str, FILE *fp);
extern void str_printf(str_ptr str, const char *fmt, ...);
extern void str_vprintf(str_ptr str, const char *fmt, va_list vp);

extern void str_read_line(str_ptr str, FILE *fp);
extern void str_write_file(str_ptr str, FILE *fp);

extern void str_strip(str_ptr str);

extern int str_compare(const str_ptr left, const str_ptr right);
extern int str_hash(str_ptr str);
extern int str_hash_imp(const char *str);

extern void str_grow(str_ptr str, int size);
extern void str_shrink(str_ptr str, int size);
extern void str_trim(str_ptr str);

extern int str_length(str_ptr str);
extern const char *str_buffer(str_ptr str);
extern char        str_get(str_ptr str, int pos);
extern char        str_get_last(str_ptr str);

extern vec_ptr    str_split(str_ptr str, char sep);
extern str_ptr str_join(vec_ptr vec, char sep);

struct substr_s
{
    str_ptr str;
    int     pos;
    int     len;
};
typedef struct substr_s substr_t, *substr_ptr;

extern int str_chr(str_ptr str, int start, char ch);
extern int str_last_chr(str_ptr str, char ch);
extern int str_count_chr(str_ptr str, char ch);
extern substr_t str_left(str_ptr str, int length);
extern substr_t str_right(str_ptr str, int length);

extern str_ptr  substr_copy(substr_ptr ss);
extern const char *substr_buffer(substr_ptr ss);

#endif
