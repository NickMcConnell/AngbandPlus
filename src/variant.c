#include "angband.h"

/*
 * A simple variant type
 * Why does angband avoid malloc and free?
 */

void var_init(variant *var)
{
    var->tag = VAR_NULL;
}

void var_clear(variant *var)
{
    if (var->tag == VAR_STRING_ALLOC)
        free((void*)(var->data.pc));

    var->tag = VAR_NULL;
}

bool var_is_null(variant *var)
{
    if (var->tag == VAR_NULL)
        return TRUE;
    return FALSE;
}

void var_set_int(variant *var, int n)
{
    var_clear(var);
    var->tag = VAR_INT;
    var->data.n = n;
}

void var_set_string(variant *var, cptr pc)
{
    var_clear(var);

    if (pc != NULL)
    {
        int sz = strlen(pc) + 1;

        if (sz <= VAR_INTERNAL_STRING_SIZE)
        {
            memcpy((void*)(var->data.buf), pc, sz);
            var->tag = VAR_STRING_INTERNAL;
        }
        else
        {
        char *buf = (char *)malloc(sz + 1);
        
            memcpy((void*)buf, pc, sz);

            var->tag = VAR_STRING_ALLOC;
            var->data.pc = buf;
        }
    }
}

void var_set_bool(variant *var, bool b)
{
    var_clear(var);
    var->tag = VAR_BOOL;
    if (b)
        var->data.b = TRUE;
    else
        var->data.b = FALSE;
}

int var_get_int(variant *var)
{
    int n = 0;

    switch (var->tag)
    {
    case VAR_BOOL:
        n = (var->data.b == TRUE) ? 1 : 0;
        break;

    case VAR_INT:
        n = var->data.n;
        break;

    case VAR_STRING_ALLOC:
        n = atoi(var->data.pc);
        break;

    case VAR_STRING_INTERNAL:
        n = atoi(var->data.buf);
        break;
    }

    return n;
}

cptr var_get_string(variant *var)
{
    cptr p = "";  /* Can one pass null string pointers to sprintf, et. al.? */
    switch (var->tag)
    {
    case VAR_STRING_ALLOC:
        p = var->data.pc;
        break;

    case VAR_STRING_INTERNAL:
        p = var->data.buf;
        break;
    }
    return p;
}

bool var_get_bool(variant *var)
{
    bool b = FALSE;
    if (var_get_int(var) != 0)
        b = TRUE;
    return b;
}

