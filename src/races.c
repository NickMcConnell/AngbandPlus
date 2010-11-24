#include "angband.h"

race_t *get_race_t_aux(int prace)
{
	return NULL;
}

race_t *get_race_t(void)
{
	return get_race_t_aux(p_ptr->prace);
}

/* HACK: This should be handled by the race_t entry point ...
   This is just here while I am refactoring code!!! */
int get_racial_powers(spell_info* spells, int max)
{
	int ct = 0;

	return ct;
}