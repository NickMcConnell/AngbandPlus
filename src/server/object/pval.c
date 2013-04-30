/*
 * File: pval.c
 * Purpose: Functions for handling object pvals.
 *
 * Copyright (c) 2011 Chris Carr
 * Copyright (c) 2012 MAngband and PWMAngband Developers
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 2, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */


#include "../s-angband.h"
#include "../../common/tvalsval.h"
#include "pval.h"


/*
 * Return the pval of an object which is closest to value. Returns -1 if
 * o_ptr has no pvals.
 */
static int object_closest_pval(object_type *o_ptr, int value)
{
    int i, best_diff, best_pval = 0;

    if (!o_ptr->num_pvals) return -1;

    best_diff = value;

    for (i = 0; i < o_ptr->num_pvals; i++)
    {
        if (abs(o_ptr->pval[i] - value) < best_diff)
        {
            best_diff = abs(o_ptr->pval[i] - value);
            best_pval = i;
        }
    }

    return best_pval;
}


static void object_shift_pval(object_type *o_ptr, int j)
{
    int k;

    /* Move any remaining pvals down one to fill the void */
    for (k = j + 1; k < o_ptr->num_pvals; k++)
    {
        of_copy(o_ptr->pval_flags[k - 1], o_ptr->pval_flags[k]);
        of_wipe(o_ptr->pval_flags[k]);
        o_ptr->pval[k - 1] = o_ptr->pval[k];
        o_ptr->pval[k] = 0;
    }

    /* We now have one fewer pval */
    o_ptr->num_pvals--;
}


/*
 * Add a pval to an object, rearranging flags as necessary. Returns TRUE
 * if the number of pvals is now different, FALSE if it is the same.
 *
 * o_ptr is the object we're adjusting
 * pval is the pval we are adding
 * flag is the flag to which we are adding the pval
 */
bool object_add_pval(object_type *o_ptr, int pval, int flag)
{
    bitflag f[OF_SIZE];
    int a = -1, best_pval;

    /* Sanity check (we may be called with 0) */
    if (!pval) return FALSE;

    create_mask(f, FALSE, OFT_PVAL, OFT_STAT, OFT_MAX);

    if (of_has(o_ptr->flags, flag))
    {
        /* See if any other flags are associated with this pval */
        a = which_pval(o_ptr, flag);
        of_off(f, flag);
        of_inter(f, o_ptr->pval_flags[a]);
        if (of_is_empty(f))
        {
            /* Safe to increment and finish */
            o_ptr->pval[a] += pval;
            if (o_ptr->pval[a] == 0)
            {
                /* Remove the flag */
                of_off(o_ptr->flags, flag);
                of_off(o_ptr->pval_flags[a], flag);

                object_shift_pval(o_ptr, a);
            }
            return object_dedup_pvals(o_ptr);
        }
    }

    /* So it doesn't have the flag, or it does but that pval also has others */

    /* Create a new pval if we can */
    if (o_ptr->num_pvals < MAX_PVALS)
    {
        o_ptr->pval[o_ptr->num_pvals] = pval;
        of_on(o_ptr->pval_flags[o_ptr->num_pvals], flag);

        /* We need to move the flag to the new pval */
        if (a != -1)
        {
            o_ptr->pval[o_ptr->num_pvals] += o_ptr->pval[a];
            of_off(o_ptr->pval_flags[a], flag);
        }

        /* We need to add it to object_flags */
        else
            of_on(o_ptr->flags, flag);

        /* We do this last because pvals start from zero */
        o_ptr->num_pvals++;

        /* We invert the logic because we've already added a pval */
        return (!object_dedup_pvals(o_ptr));
    }

    /* We use the closest existing pval */
    best_pval = object_closest_pval(o_ptr, pval + ((a == -1)? 0: o_ptr->pval[a]));

    /* Turn on the flag on the new pval */
    if (best_pval != a)
    {
        of_on(o_ptr->pval_flags[best_pval], flag);

        /* Turn it off on its old pval */
        if (a != -1) of_off(o_ptr->pval_flags[a], flag);

        /* Add it to object_flags */
        else
            of_on(o_ptr->flags, flag);
    }

    /* We haven't changed any pvals, so no need to de-dup */
    return FALSE;
}


/*
 * Returns whether a specific pval is known
 */
bool object_this_pval_is_visible(const object_type *o_ptr, int pval, bool aware)
{
    bitflag f[MAX_PVALS][OF_SIZE], f2[OF_SIZE];

    my_assert(o_ptr->kind);

    /* Aware jewelry with non-variable pval (except artifacts) */
    if (object_is_jewelry(o_ptr) && !o_ptr->artifact && aware &&
        !randcalc_varies(o_ptr->kind->pval[pval]))
    {
        return TRUE;
    }

    if (object_was_worn(o_ptr))
    {
        object_pval_flags_known(o_ptr, f, aware);

        /* Create the mask for pval-related flags */
        create_mask(f2, FALSE, OFT_STAT, OFT_PVAL, OFT_MAX);

        if (of_is_inter(f[pval], f2)) return TRUE;
    }

    return FALSE;
}


/*
 * Work out which pval governs a particular flag.
 * We assume that we are only called if this pval and flag exist and are known.
 */
int which_pval(const object_type *o_ptr, const int flag)
{
    int i;
    bitflag f[MAX_PVALS][OF_SIZE];

    object_pval_flags(o_ptr, f);

    for (i = 0; i < MAX_PVALS; i++)
    {
        /* Flag should be unique across pvals */
        if (of_has(f[i], flag)) return i;
    }

    if (o_ptr->kind)
        quit_fmt("Bad call to which_pval: flag is %d, kidx is %d", flag, o_ptr->kind->kidx);
    else
        quit_fmt("Bad call to which_pval: flag is %d, kind is NULL", flag);
    return -1;
}


/*
 * Return the pval_flags for an item
 */
void object_pval_flags(const object_type *o_ptr, bitflag flags[MAX_PVALS][OF_SIZE])
{
    size_t i;

    for (i = 0; i < MAX_PVALS; i++) of_wipe(flags[i]);

    if (!o_ptr->kind) return;

    for (i = 0; i < o_ptr->num_pvals; i++) of_copy(flags[i], o_ptr->pval_flags[i]);
}


/*
 * Obtain the pval_flags for an item which are known to the player
 */
void object_pval_flags_known(const object_type *o_ptr, bitflag flags[MAX_PVALS][OF_SIZE],
    bool aware)
{
    int i, j, flag;
    bitflag f[MAX_PVALS][OF_SIZE];

    object_pval_flags(o_ptr, flags);
    if (!o_ptr->kind) return;

    object_pval_flags(o_ptr, f);

    for (i = 0; i < o_ptr->num_pvals; i++) of_inter(flags[i], o_ptr->known_flags);

    /* Kind and ego pval_flags may have shifted pvals or gone so we iterate */
    for (i = 0; aware && (i < o_ptr->kind->num_pvals); i++)
    {
        for (flag = of_next(o_ptr->kind->pval_flags[i], FLAG_START); flag != FLAG_END;
            flag = of_next(o_ptr->kind->pval_flags[i], flag + 1))
        {
            for (j = 0; j < MAX_PVALS; j++)
            {
                if (of_has(f[j], flag))
                {
                    of_on(flags[j], flag);
                    break;
                }
            }
        }
    }

    for (i = 0; o_ptr->ego && easy_know(o_ptr, aware) && (i < o_ptr->ego->num_pvals); i++)
    {
        for (flag = of_next(o_ptr->ego->pval_flags[i], FLAG_START); flag != FLAG_END;
            flag = of_next(o_ptr->ego->pval_flags[i], flag + 1))
        {
            for (j = 0; j < MAX_PVALS; j++)
            {
                if (of_has(f[j], flag))
                {
                    of_on(flags[j], flag);
                    break;
                }
            }
        }
    }
}


/*
 * Combine two pvals of the same value on an object. Returns TRUE if changes
 * were made, i.e. o_ptr->num_pvals has decreased by one.
 */
bool object_dedup_pvals(object_type *o_ptr)
{
    int i, j;

    /* Abort if there can be no duplicates */
    if (o_ptr->num_pvals < 2) return FALSE;

    /* Find the first pair of pvals which have the same value */
    for (i = 0; i < o_ptr->num_pvals; i++)
    {
        for (j = i + 1; j < o_ptr->num_pvals; j++)
        {
            if (o_ptr->pval[i] == o_ptr->pval[j])
            {
                /* Nuke the j pval and its flags, combining them with i's */
                of_union(o_ptr->pval_flags[i], o_ptr->pval_flags[j]);
                of_wipe(o_ptr->pval_flags[j]);
                o_ptr->pval[j] = 0;

                /* Move any remaining pvals down one to fill the void */
                object_shift_pval(o_ptr, j);
                return TRUE;
            }
        }
    }

    /* No two pvals are identical */
    return FALSE;
}
