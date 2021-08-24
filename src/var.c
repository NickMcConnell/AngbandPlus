#include "angband.h"

var_t var_create(void)
{
    var_t v = {0};
    v.tag = VAR_NULL;
    return v;
}

void var_clear(var_ptr var)
{
    if (var->tag == VAR_STRING)
        free(var->data.pc);
    var->tag = VAR_NULL;
}

void var_destroy(var_ptr var)
{
    var_clear(var);
}

bool var_is_null(var_ptr var)
{
    return var->tag == VAR_NULL;
}

void var_set_int(var_ptr var, int n)
{
    var_clear(var);
    var->tag = VAR_INT;
    var->data.n = n;
}

void var_set_string(var_ptr var, cptr pc)
{
    var_clear(var);

    if (pc != NULL)
    {
        int sz = strlen(pc) + 1;

        if (sz <= VAR_INTERNAL_STRING_SIZE)
        {
            strcpy(var->data.buf, pc);
            var->tag = VAR_STRING_INTERNAL;
        }
        else
        {
        char *buf = malloc(sz + 1);
        
            strcpy(buf, pc);

            var->tag = VAR_STRING;
            var->data.pc = buf;
        }
    }
}

void var_printf(var_ptr var, const char *fmt, ...)
{
    va_list vp;
    str_t s;

    str_create(&s, 20);

    va_start(vp, fmt);
    str_vprintf(&s, fmt, vp);
    va_end(vp);

    /* steal ownership of s.buf. do not str_destroy(&s) */
    var_clear(var);
    var->tag = VAR_STRING;
    var->data.pc = s.buf;
}

void var_set_bool(var_ptr var, bool b)
{
    var_clear(var);
    var->tag = VAR_BOOL;
    if (b)
        var->data.b = TRUE;
    else
        var->data.b = FALSE;
}

int var_get_int(var_ptr var)
{
    switch (var->tag)
    {
    case VAR_BOOL: return var->data.b ? 1 : 0;
    case VAR_INT: return var->data.n;
    case VAR_STRING: return atoi(var->data.pc);
    case VAR_STRING_INTERNAL: return atoi(var->data.buf);
    }
    return 0;
}

cptr var_get_string(var_ptr var)
{
    switch (var->tag)
    {
    case VAR_STRING: return var->data.pc;
    case VAR_STRING_INTERNAL: return var->data.buf;
    }
    return "";
}

bool var_get_bool(var_ptr var)
{
    if (var_get_int(var)) return TRUE;
    return FALSE;
}

