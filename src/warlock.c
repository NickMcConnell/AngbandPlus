/****************************************************************
 * The Warlock
 *
 * TODO: This class is still mostly implemented in mind.c
 *       Please move it over here!
 ****************************************************************/

#include "angband.h"

bool warlock_is_pact_monster(monster_race *r_ptr)
{
	bool is_pact = FALSE;
	/* First, we list symbols for the alliance */
	char* pc = my_strchr(pact_info[p_ptr->psubclass].alliance, r_ptr->d_char);
	if (pc != NULL)
	{
		is_pact = TRUE;
	}
	else
	{
		/* If that fails, we check flags ... I'd prefer to only check flags
			but I'm not sure how accurate the beastiary is ... */
		switch (p_ptr->psubclass)
		{
		case PACT_UNDEAD:
			if (r_ptr->flags3 & RF3_UNDEAD)
				is_pact = TRUE;
			break;

		case PACT_DRAGON:
			if (r_ptr->flags3 & RF3_DRAGON)
				is_pact = TRUE;
			break;

		case PACT_ANGEL:
			/* Angel pact is now all good monsters!!! */
			if (r_ptr->flags3 & RF3_GOOD)
				is_pact = TRUE;
			break;
				
		case PACT_DEMON:
			if (r_ptr->flags3 & RF3_DEMON)
				is_pact = TRUE;
			break;
				
		case PACT_ABERRATION:
			if (r_ptr->flags2 & RF2_HUMAN)
				is_pact = TRUE;				
			break;
		}
	}

	return is_pact;
}

