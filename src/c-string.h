#ifndef INCLUDED_C_STRING_H
#define INCLUDED_C_STRING_H

#include <stdio.h>
#include "c-vec.h"

typedef struct string_s string_t;
typedef string_t *string_ptr;

/* Create a new string object (You must string_free()) */
extern string_ptr string_alloc(void);
extern string_ptr string_alloc_format(const char *fmt, ...);
extern string_ptr string_alloc_size(int size);

extern string_ptr string_copy(string_ptr str);
extern string_ptr string_copy_s(const char *val);
extern string_ptr string_copy_sn(const char *val, int cb);

extern string_ptr string_read_file(FILE *fp);

/* Destroy a string object */
extern void string_free(string_ptr str);


extern void string_clear(string_ptr str);

/* String Building */
extern void string_append(string_ptr str, string_ptr to_append);
extern void string_append_c(string_ptr str, char ch);
extern void string_append_s(string_ptr str, const char *val);
extern void string_append_sn(string_ptr str, const char *val, int cb);
extern void string_append_file(string_ptr str, FILE *fp);
extern void string_printf(string_ptr str, const char *fmt, ...);
extern void string_vprintf(string_ptr str, const char *fmt, va_list vp);

extern void string_read_line(string_ptr str, FILE *fp);
extern void string_write_file(string_ptr str, FILE *fp);

extern void string_strip(string_ptr str);

extern int string_compare(const string_ptr left, const string_ptr right);
extern int string_hash(string_ptr str);
extern int string_hash_imp(const char *str);

extern void string_grow(string_ptr str, int size);
extern void string_shrink(string_ptr str, int size);
extern void string_trim(string_ptr str);

extern int string_length(string_ptr str);
extern const char *string_buffer(string_ptr str);

extern vec_ptr    string_split(string_ptr str, char sep);
extern string_ptr string_join(vec_ptr vec, char sep);

struct substring_s
{
    string_ptr str;
    int        pos;
    int        len;
};
typedef struct substring_s substring_t, *substring_ptr;

extern int string_chr(string_ptr str, int start, char ch);
extern int string_last_chr(string_ptr str, char ch);
extern int string_count_chr(string_ptr str, char ch);
extern substring_t string_left(string_ptr str, int length);
extern substring_t string_right(string_ptr str, int length);

extern string_ptr  substring_copy(substring_ptr ss);
extern const char *substring_buffer(substring_ptr ss);

#endif
