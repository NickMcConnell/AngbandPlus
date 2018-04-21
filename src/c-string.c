#include "c-string.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#ifdef _MSC_VER
#   pragma warning(disable:4996)
    /* Release code is 4x slower than debug code without the following! */
#   pragma function(memset, strlen)
    /* From latest Vanilla:
     * "MSVC doesn't have va_copy (which is C99) or an alternative, so we'll just
     *  copy the SRC pointer. In other cases we'll use va_copy() as we should." */
#   define va_copy(DST, SRC) (DST) = (SRC)
#   define vsnprintf _vsnprintf
#endif

struct string_s
{
    int   size;
    int   len;
    char *buf;
};

string_ptr string_alloc(void)
{
    return string_copy_sn("", 0);
}

string_ptr string_alloc_format(const char *fmt, ...)
{
    string_ptr res = string_alloc_size(128);
    va_list vp;

    va_start(vp, fmt);
    string_vprintf(res, fmt, vp);
    va_end(vp);

    return res;
}

string_ptr string_alloc_size(int size)
{
    string_ptr res = malloc(sizeof(string_t));

    res->buf = malloc(size + 1);
    res->len = 0;
    res->buf[0] = '\0';
    res->size = size + 1;

    return res;
}

string_ptr string_copy(string_ptr str)
{
    return string_copy_sn(str->buf, str->len);
}

string_ptr string_copy_s(const char *val)
{
    if (!val)
        val = "";
    return string_copy_sn(val, strlen(val));
}

string_ptr string_copy_sn(const char *val, int cb)
{
    string_ptr res = malloc(sizeof(string_t));

    assert(val);
    res->buf = malloc(cb + 1);
    memcpy(res->buf, val, cb);
    res->len = cb;
    res->buf[res->len] = '\0';
    res->size = cb + 1;
    return res;
}

void string_free(string_ptr str)
{
    if (str)
    {
        free(str->buf);
        free(str);
    }
}

void string_append(string_ptr str, string_ptr to_append)
{
    string_append_sn(str, to_append->buf, to_append->len);
}

void string_append_c(string_ptr str, char ch)
{
    string_grow(str, str->len + 2);
    str->buf[str->len++] = ch;
    str->buf[str->len] = '\0';
}

void string_append_s(string_ptr str, const char *val)
{
    if (!val)
        return;

    string_append_sn(str, val, strlen(val));
}

void string_append_sn(string_ptr str, const char *val, int cb)
{
    int cbl;  /* left += right */

    if (!cb)
        return;

    cbl = str->len;

    string_grow(str, cbl + cb + 1);
    memcpy(str->buf + cbl, val, cb);
    str->len += cb;
    str->buf[str->len] = '\0';
}

void string_read_line(string_ptr str, FILE *fp)
{
    string_clear(str);
    for (;;)
    {
        int c = fgetc(fp);
        if (c == EOF) break;
        if (c == '\n') break;
        if (str->len >= str->size - 1)
            string_grow(str, str->size * 2);
        str->buf[str->len++] = c;
    }
    str->buf[str->len] = '\0';
}

string_ptr string_read_file(FILE *fp)
{
    string_ptr result = string_alloc();
    string_append_file(result, fp);
    return result;
}

void string_append_file(string_ptr str, FILE *fp)
{
    for (;;)
    {
        int c = fgetc(fp);
        if (c == EOF) break;
        if (c == '\r') continue; /* \r\n -> \n */
        if (str->len >= str->size - 1)
            string_grow(str, str->size * 2);
        str->buf[str->len++] = c;
    }
    str->buf[str->len] = '\0';
}

void string_write_file(string_ptr str, FILE *fp)
{
    int i;
    for (i = 0; i < str->len; i++)
        fputc(str->buf[i], fp);
}

int string_compare(const string_ptr left, const string_ptr right)
{
    return strcmp(left->buf, right->buf);
}

void string_printf(string_ptr str, const char *fmt, ...)
{
    va_list vp;
    va_start(vp, fmt);
    string_vprintf(str, fmt, vp);
    va_end(vp);
}

void string_vprintf(string_ptr str, const char *fmt, va_list vp)
{
    for (;;)
    {
        va_list args;
        int     cb = str->len;
        int     res;

        /* Note: va_copy allows vsnprintf to work on linux. Otherwise,
           we would have to va_start before each call, forcing the size
           growing code from string_vprintf() to string_printf(fmt, ...).
           Now consider something like doc_printf(fmt, ...) that wants
           to reuse this, but can't call string_printf(fmt, ...)!
           va_copy is C99 so won't work on M$. Yet, _vsnprintf doesn't
           mutate the va_list arg the way linux does, so it just works.*/
        va_copy(args, vp);
        res = vsnprintf(str->buf + cb, str->size - cb, fmt, args);
        va_end(args);

        if (res >= str->size - cb)
        {
            str->buf[cb] = '\0';
            string_grow(str, cb + res + 1);
        }
        else if (res < 0)
        {
            str->buf[cb] = '\0';
            string_grow(str, str->size * 2);
        }
        else
        {
            str->len += res;
            break;
        }
    }
}

void string_grow(string_ptr str, int size)
{
    if (size > str->size)
    {
        int   new_size;
        char *buf;

        new_size = str->size * 2;
        if (new_size < size)
            new_size = size;

        buf = malloc(new_size);
        memcpy(buf, str->buf, str->size);
        free(str->buf);

        str->size = new_size;
        str->buf = buf;
    }
}

/* <quibble> These aren't really hash functions. The hash function
   is the composite string->int->int where the second arrow
   is modulo some well chosen prime. This "hash" is just mapping
   from strings to integers in preparation for the real hashing.
   Still, we need to be as "injective" as possible in the first
   arrow. </quibble> */
int string_hash_imp(const char *str) /* djb2 hash algorithm */
{
    int hash = 5381;
    int c;

    while ((c = *str++))
        hash = ((hash << 5) + hash) + c; /* hash * 33 + c */

    return hash;
}

int string_hash(string_ptr str)
{
    return string_hash_imp(str->buf);
}

void string_clear(string_ptr str)
{
    str->len = 0;
    str->buf[str->len] = '\0';
}

void string_shrink(string_ptr str, int size)
{
    if (size < str->size)
    {
        int   cb = str->len + 1;
        char *buf;

        /* Never shrink below the current strlen() */
        if (size < cb)
            size = cb;

        buf = malloc(size);
        memcpy(buf, str->buf, cb);
        free(str->buf);

        str->size = size;
        str->buf = buf;
    }
}

void string_strip(string_ptr str)
{
    int i, j, k;
    for (i = 0; i < str->len; i++)
    {
        if (str->buf[i] != ' ') break;
    }
    for (j = str->len - 1; j > i; j--)
    {
        if (str->buf[j] != ' ') break;
    }
    if (0 < i || j < str->len - 1)
    {
        str->len = j - i + 1;
        for (k = 0; k < str->len; k++)
            str->buf[k] = str->buf[i+k];
        str->buf[str->len] = '\0';
    }
}

void string_trim(string_ptr str)
{
    string_shrink(str, 0);
}


int string_length(string_ptr str)
{
    return str->len;
}

const char *string_buffer(string_ptr str)
{
    if (str)
        return str->buf;
    return NULL;
}

int string_chr(string_ptr str, int start, char ch)
{
    if (start < str->len)
    {
        const char *pos = strchr(str->buf + start, ch);
        if (pos)
            return pos - str->buf;
    }
    return -1;
}

int string_count_chr(string_ptr str, char ch)
{
    int ct = 0;
    int pos = 0;
    for (;;)
    {
        pos = string_chr(str, pos, ch);
        if (pos == -1) break;
        pos++;
        ct++;
    }
    return ct;
}

int string_last_chr(string_ptr str, char ch)
{
    int pos = 0;
    int result = -1;
    for (;;)
    {
        pos = string_chr(str, pos, ch);
        if (pos >= 0)
        {
            result = pos;
            pos++;
        }
        else
            break;
    }
    return result;
}

substring_t string_left(string_ptr str, int len)
{
    substring_t result;

    result.str = str;
    result.pos = 0;
    if (len <= str->len)
        result.len = len;
    else
        result.len = str->len;

    return result;
}

substring_t string_right(string_ptr str, int len)
{
    substring_t result;

    result.str = str;
    if (len <= str->len)
    {
        result.pos = str->len - len;
        result.len = len;
    }
    else
    {
        result.pos = 0;
        result.len = str->len;
    }

    return result;
}

const char *substring_buffer(substring_ptr ss)
{
    assert(ss->pos + ss->len <= ss->str->len);
    return ss->str->buf + ss->pos;
}

string_ptr substring_copy(substring_ptr ss)
{
    return string_copy_sn(substring_buffer(ss), ss->len);
}

vec_ptr string_split(string_ptr str, char sep)
{
    vec_ptr     v = vec_alloc((vec_free_f)string_free);
    const char *pos = str->buf;
    int         done = 0;

    while (!done && *pos)
    {
        const char *next = strchr(pos, sep);
        string_ptr  s;

        if (!next && *pos)
        {
            next = strchr(pos, '\0');
            assert(next);
            done = 1;
        }

        s = string_copy_sn(pos, next - pos);
        vec_add(v, s);
        pos = next + 1;
    }
    return v;
}

string_ptr string_join(vec_ptr vec, char sep)
{
    int        i;
    string_ptr result = string_alloc();

    for (i = 0; i < vec_length(vec); i++)
    {
        string_ptr s = vec_get(vec, i);
        if (i > 0)
            string_append_c(result, sep);
        string_append(result, s);
    }
    return result;
}
