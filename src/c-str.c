#include "c-str.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

str_ptr str_alloc(void)
{
    return str_copy_sn("", 0);
}

str_ptr str_alloc_format(const char *fmt, ...)
{
    str_ptr res = str_alloc_size(128);
    va_list vp;

    va_start(vp, fmt);
    str_vprintf(res, fmt, vp);
    va_end(vp);

    return res;
}

str_ptr str_alloc_size(int size)
{
    str_ptr res = malloc(sizeof(str_t));
    str_create(res, size);
    return res;
}

void str_create(str_ptr str, int size)
{
    str->buf = malloc(size + 1);
    str->buf[0] = '\0';
    str->len = 0;
    str->size = size + 1;
}

str_ptr str_copy(str_ptr str)
{
    return str_copy_sn(str->buf, str->len);
}

str_ptr str_copy_s(const char *val)
{
    if (!val)
        val = "";
    return str_copy_sn(val, strlen(val));
}

str_ptr str_copy_sn(const char *val, int cb)
{
    str_ptr res = malloc(sizeof(str_t));

    assert(val);
    res->buf = malloc(cb + 1);
    memcpy(res->buf, val, cb);
    res->len = cb;
    res->buf[res->len] = '\0';
    res->size = cb + 1;
    return res;
}

void str_destroy(str_ptr str)
{
    assert(str);
    free(str->buf);
    free(str);
}

void str_free(str_ptr str)
{
    if (str)
        str_destroy(str);
}

void str_append(str_ptr str, str_ptr to_append)
{
    str_append_sn(str, to_append->buf, to_append->len);
}

void str_append_c(str_ptr str, char ch)
{
    str_grow(str, str->len + 2);
    str->buf[str->len++] = ch;
    str->buf[str->len] = '\0';
}

void str_append_s(str_ptr str, const char *val)
{
    if (!val)
        return;

    str_append_sn(str, val, strlen(val));
}

void str_append_sn(str_ptr str, const char *val, int cb)
{
    int cbl;  /* left += right */

    if (!cb)
        return;

    cbl = str->len;

    str_grow(str, cbl + cb + 1);
    memcpy(str->buf + cbl, val, cb);
    str->len += cb;
    str->buf[str->len] = '\0';
}

void str_read_line(str_ptr str, FILE *fp)
{
    str_clear(str);
    for (;;)
    {
        int c = fgetc(fp);
        if (c == EOF) break;
        if (c == '\n') break;
        if (str->len >= str->size - 1)
            str_grow(str, str->size * 2);
        str->buf[str->len++] = c;
    }
    str->buf[str->len] = '\0';
}

str_ptr str_read_file(FILE *fp)
{
    str_ptr result = str_alloc();
    str_append_file(result, fp);
    return result;
}

void str_append_file(str_ptr str, FILE *fp)
{
    for (;;)
    {
        int c = fgetc(fp);
        if (c == EOF) break;
        if (c == '\r') continue; /* \r\n -> \n */
        if (str->len >= str->size - 1)
            str_grow(str, str->size * 2);
        str->buf[str->len++] = c;
    }
    str->buf[str->len] = '\0';
}

void str_write_file(str_ptr str, FILE *fp)
{
    int i;
    for (i = 0; i < str->len; i++)
        fputc(str->buf[i], fp);
}

int str_compare(const str_ptr left, const str_ptr right)
{
    return strcmp(left->buf, right->buf);
}

void str_printf(str_ptr str, const char *fmt, ...)
{
    va_list vp;
    va_start(vp, fmt);
    str_vprintf(str, fmt, vp);
    va_end(vp);
}

void str_vprintf(str_ptr str, const char *fmt, va_list vp)
{
    for (;;)
    {
        va_list args;
        int     cb = str->len;
        int     res;

        /* Note: va_copy allows vsnprintf to work on linux. Otherwise,
           we would have to va_start before each call, forcing the size
           growing code from str_vprintf() to str_printf(fmt, ...).
           Now consider something like doc_printf(fmt, ...) that wants
           to reuse this, but can't call str_printf(fmt, ...)!
           va_copy is C99 so won't work on M$. Yet, _vsnprintf doesn't
           mutate the va_list arg the way linux does, so it just works.*/
        va_copy(args, vp);
        res = vsnprintf(str->buf + cb, str->size - cb, fmt, args);
        va_end(args);

        if (res >= str->size - cb)
        {
            str->buf[cb] = '\0';
            str_grow(str, cb + res + 1);
        }
        else if (res < 0)
        {
            str->buf[cb] = '\0';
            str_grow(str, str->size * 2);
        }
        else
        {
            str->len += res;
            break;
        }
    }
}

void str_grow(str_ptr str, int size)
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
   is the composite str->int->int where the second arrow
   is modulo some well chosen prime. This "hash" is just mapping
   from strs to integers in preparation for the real hashing.
   Still, we need to be as "injective" as possible in the first
   arrow. </quibble> */
int str_hash_imp(const char *str) /* djb2 hash algorithm */
{
    int hash = 5381;
    int c;

    while ((c = *str++))
        hash = ((hash << 5) + hash) + c; /* hash * 33 + c */

    return hash;
}

int str_hash(str_ptr str)
{
    return str_hash_imp(str->buf);
}

void str_clear(str_ptr str)
{
    str->len = 0;
    str->buf[str->len] = '\0';
}

void str_shrink(str_ptr str, int size)
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

void str_strip(str_ptr str)
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

void str_trim(str_ptr str)
{
    str_shrink(str, 0);
}


int str_length(str_ptr str)
{
    return str->len;
}

const char *str_buffer(str_ptr str)
{
    if (str)
        return str->buf;
    return NULL;
}

char str_get(str_ptr str, int pos)
{
    assert(0 <= pos && pos < str->len);
    return str->buf[pos];
}

char str_get_last(str_ptr str)
{
    char c = '\0';
    if (str->len)
        c = str->buf[str->len - 1];
    return c;
}

int str_chr(str_ptr str, int start, char ch)
{
    if (start < str->len)
    {
        const char *pos = strchr(str->buf + start, ch);
        if (pos)
            return pos - str->buf;
    }
    return -1;
}

int str_count_chr(str_ptr str, char ch)
{
    int ct = 0;
    int pos = 0;
    for (;;)
    {
        pos = str_chr(str, pos, ch);
        if (pos == -1) break;
        pos++;
        ct++;
    }
    return ct;
}

int str_last_chr(str_ptr str, char ch)
{
    int pos = 0;
    int result = -1;
    for (;;)
    {
        pos = str_chr(str, pos, ch);
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

substr_t str_left(str_ptr str, int len)
{
    substr_t result;

    result.str = str;
    result.pos = 0;
    if (len <= str->len)
        result.len = len;
    else
        result.len = str->len;

    return result;
}

substr_t str_right(str_ptr str, int len)
{
    substr_t result;

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

const char *substr_buffer(substr_ptr ss)
{
    assert(ss->pos + ss->len <= ss->str->len);
    return ss->str->buf + ss->pos;
}

str_ptr substr_copy(substr_ptr ss)
{
    return str_copy_sn(substr_buffer(ss), ss->len);
}

vec_ptr str_split(str_ptr str, char sep)
{
    vec_ptr     v = vec_alloc((vec_free_f)str_free);
    const char *pos = str->buf;
    int         done = 0;

    while (!done && *pos)
    {
        const char *next = strchr(pos, sep);
        str_ptr  s;

        if (!next && *pos)
        {
            next = strchr(pos, '\0');
            assert(next);
            done = 1;
        }

        s = str_copy_sn(pos, next - pos);
        vec_add(v, s);
        pos = next + 1;
    }
    return v;
}

str_ptr str_join(vec_ptr vec, char sep)
{
    int        i;
    str_ptr result = str_alloc();

    for (i = 0; i < vec_length(vec); i++)
    {
        str_ptr s = vec_get(vec, i);
        if (i > 0)
            str_append_c(result, sep);
        str_append(result, s);
    }
    return result;
}
