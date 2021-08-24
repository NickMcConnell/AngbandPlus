#include "angband.h"

#include <assert.h>

/****************************************************************
 * Public Entrypoints
 ****************************************************************/
bool prace_is_(int which)
{
    if (plr->mimic_form == which)
        return TRUE;
    else if (plr->mimic_form == MIMIC_NONE && plr->prace == which)
        return TRUE;

    return FALSE;
}

race_t *get_race_aux(int prace, int psubrace)
{
    return plr_race_aux(prace, psubrace);
}

race_t *get_true_race(void)
{
    return plr_true_race();
}

race_t *get_race(void)
{
    return plr_race();
}

