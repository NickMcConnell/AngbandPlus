#include "angband.h"

race_t *get_race_t_aux(int prace)
{
	return NULL;
}

race_t *get_race_t(void)
{
	return get_race_t_aux(p_ptr->prace);
}
