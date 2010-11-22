#include "angband.h"

typedef struct {
	mutation_rating rating;
	u16b type;
	byte stat;
	byte prob;
} mutation_info;

/*
 * Table of mutations.  Activatable mutations have level, cost, fail rates, etc.
 * Passive and bonus mutations just have spell objects.
 */
static spell_info _mutation_spells[MAX_MUTATIONS] = 
{
    /*lvl cst fail  spell */
	{   9,  9,  30, spit_acid_spell},
	{  20,  0,  40, breathe_fire_spell},
	{  12, 12,  40, hypnotic_gaze_spell},
	{   9,  9,  40, telekinesis_spell},
	{   7,  7,  30, teleport_spell},
	{   5,  3,  30, mind_blast_spell},
	{  15, 15,  30, radiation_spell},
	{   2,  1,  30, vampirism_spell},
	{   3,  2,  30, detect_treasure_spell},
	{   5,  4,  30, detect_monsters_spell},
	{   3,  3,  25, phase_door_spell},
};

/*
 * Table describing the available mutations, used internally for allocation
 * of random mutations, and various processing callbacks
 */
static mutation_info _mutations[MAX_MUTATIONS] = 
{
	{ MUT_RATING_GOOD, MUT_TYPE_ACTIVATION, A_DEX, 4 },	/* MUT_SPIT_ACID */
	{ MUT_RATING_GOOD, MUT_TYPE_ACTIVATION, A_CON, 3 },	/* MUT_BR_FIRE */
	{ MUT_RATING_GOOD, MUT_TYPE_ACTIVATION, A_CHR, 2 },	/* MUT_HYPN_GAZE */
	{ MUT_RATING_GOOD, MUT_TYPE_ACTIVATION, A_WIS, 2 },	/* MUT_TELEKINESIS */
	{ MUT_RATING_GOOD, MUT_TYPE_ACTIVATION, A_WIS, 3 },	/* MUT_TELEPORT */
	{ MUT_RATING_GOOD, MUT_TYPE_ACTIVATION, A_WIS, 2 },	/* MUT_MIND_BLAST */
	{ MUT_RATING_GOOD, MUT_TYPE_ACTIVATION, A_CON, 2 }, /* MUT_RADIATION */
	{ MUT_RATING_GOOD, MUT_TYPE_ACTIVATION, A_CON, 2 }, /* MUT_VAMPIRISM */
	{ MUT_RATING_GOOD, MUT_TYPE_ACTIVATION, A_INT, 3 }, /* MUT_SMELL_METAL */
	{ MUT_RATING_GOOD, MUT_TYPE_ACTIVATION, A_INT, 4 }, /* MUT_SMELL_MONSTERS */
	{ MUT_RATING_GOOD, MUT_TYPE_ACTIVATION, A_WIS, 3 }, /* MUT_BLINK */
};

bool mut_berserker_pred(int mut_idx)
{
	if (mut_type(mut_idx) & MUT_TYPE_ACTIVATION) return FALSE;
	return TRUE;
}

void mut_calc_bonuses(void)
{
	int i;
	variant v;
	var_init(&v);

	for (i = 0; i < MAX_MUTATIONS; i++)
	{
		if ( mut_present(i)
		  && (_mutations[i].type & MUT_TYPE_BONUS) )
		{
			(_mutation_spells[i].fn)(SPELL_CALC_BONUS, &v);
		}
	}

	var_clear(&v);
}

void mut_gain(int mut_idx)
{
	variant v;
	if (mut_present(mut_idx)) return;
	
	var_init(&v);
	add_flag(p_ptr->muta, mut_idx);
	(_mutation_spells[mut_idx].fn)(SPELL_GAIN_MUT, &v);
	var_clear(&v);
}

bool mut_gain_random(mut_pred pred)
{
	int prob[MAX_MUTATIONS];
	int i;
	int tot = 0;

	/* build probability table */
	for (i = 0; i < MAX_MUTATIONS; i++)
	{
		int cur = 0;
		if (!mut_present(i))
		{
			if (pred == NULL || (pred(i)))
				cur = _mutations[i].prob;
		}
		tot += cur;
		prob[i] = tot;
	}

	/* any left? */
	if (tot > 0)
	{
		int j = randint0(tot);
		int which = -1;
		
		/* scan the probability table for the correct mutation */
		for (i = 0; i < MAX_MUTATIONS; i++)
		{
			if (j < prob[i])
			{
				which = i;
				break;
			}
		}

		if (which >= 0 && !mut_present(which)) /* paranoid checks ... should always pass */
		{
			mut_gain(which);
			return TRUE;
		}
	}

	msg_print("You feel normal.");
	return FALSE;
}

int mut_get_spells(spell_info* spells, int max)
{
	int i;
	int ct = 0;
	
	for (i = 0; i < MAX_MUTATIONS; i++)
	{
		if ( mut_present(i)
		  && (_mutations[i].type & MUT_TYPE_ACTIVATION) )
		{
			spell_info *base = &_mutation_spells[i];
			spell_info* current = NULL;
			int stat_idx = p_ptr->stat_ind[_mutations[i].stat];

			if (ct >= max) break;

			current = &spells[ct];
			current->fn = base->fn;
			current->level = base->level;
			current->cost = base->cost;

			current->fail = calculate_fail_rate(base, stat_idx);			
			ct++;
		}
	}
	return ct;
}

bool mut_good_pred(int mut_idx)
{
	if (mut_rating(mut_idx) < MUT_RATING_GOOD) return FALSE;
	return TRUE;
}

void mut_lock(int mut_idx)
{
	if (mut_locked(mut_idx)) return;
	add_flag(p_ptr->muta_lock, mut_idx);
}

bool mut_locked(int mut_idx)
{
	return have_flag(p_ptr->muta_lock, mut_idx);
}

void mut_lose(int mut_idx)
{
	variant v;
	if (!mut_present(mut_idx)) return;
	if (mut_locked(mut_idx)) return;
	
	var_init(&v);
	remove_flag(p_ptr->muta, mut_idx);
	(_mutation_spells[mut_idx].fn)(SPELL_LOSE_MUT, &v);
	var_clear(&v);
}

bool mut_lose_random(mut_pred pred)
{
	int prob[MAX_MUTATIONS];
	int i;
	int tot = 0;

	/* build probability table */
	for (i = 0; i < MAX_MUTATIONS; i++)
	{
		int cur = 0;
		if (mut_present(i))
		{
			if (pred == NULL || (pred(i)))
				cur = _mutations[i].prob;
		}
		tot += cur;
		prob[i] = tot;
	}

	/* any left? */
	if (tot > 0)
	{
		int j = randint0(tot);
		int which = -1;
		
		/* scan the probability table for the correct mutation */
		for (i = 0; i < MAX_MUTATIONS; i++)
		{
			if (j < prob[i])
			{
				which = i;
				break;
			}
		}

		if (which >= 0 && mut_present(which)) /* paranoid checks ... should always pass */
		{
			mut_lose(which);
			return TRUE;
		}
	}

	msg_print("You feel strange.");
	return FALSE;
}

bool mut_present(int mut_idx)
{
	return have_flag(p_ptr->muta, mut_idx);
}

void mut_process(void)
{
	int i;
	variant v;
	var_init(&v);

	for (i = 0; i < MAX_MUTATIONS; i++)
	{
		if ( mut_present(i)
		  && (_mutations[i].type & MUT_TYPE_EFFECT) )
		{
			(_mutation_spells[i].fn)(SPELL_PROCESS, &v);
		}
	}

	var_clear(&v);
}

int mut_rating(int mut_idx)
{
	return _mutations[mut_idx].rating;
}

int mut_type(int mut_idx)
{
	return _mutations[mut_idx].type;
}

void mut_unlock(int mut_idx)
{
	if (!mut_locked(mut_idx)) return;
	remove_flag(p_ptr->muta_lock, mut_idx);
}

