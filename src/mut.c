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
	{   3,  2,  30, smell_metal_spell},
	{   5,  4,  30, smell_monsters_spell},
	{   3,  3,  30, phase_door_spell},
	{   8, 12,  40, eat_rock_spell},
	{  15, 12,  40, swap_pos_spell},
	{  20, 14,  40, shriek_spell},
	{   3,  2,  30, light_area_spell},
	{   7, 14,  30, detect_curses_spell},
	{   8,  8,  50, berserk_spell},
	{  18, 20,  50, polymorph_self_spell},
	{  10,  5,  70, alchemy_spell},
	{   1,  6,  60, grow_mold_spell},
	{  10, 12,  50, resist_elements_spell},
	{  12, 12,  50, earthquake_spell},
	{  17,  1,  80, eat_magic_spell},
	{   6,  6,  50, weigh_magic_spell},
	{  12, 23,  70, sterility_spell},
	{  10, 12,  60, panic_hit_spell},
	{   7, 15,  60, dazzle_spell},
	{   7, 10,  50, laser_eye_spell},
	{  17, 50,  70, recall_spell},
	{  25, 25,  70, banish_evil_spell},
	{   2,  2,  30, cold_touch_spell},
	{   1,  0,  40, power_throw_spell},
	{   0,  0,   0, berserk_rage_mut},
	{   0,  0,   0, cowardice_mut},
	{   0,  0,   0, random_teleport_mut},
	{   0,  0,   0, alcohol_mut},
	{   0,  0,   0, hallucination_mut},
	{   0,  0,   0, flatulence_mut},
	{   0,  0,   0, scorpion_tail_mut},
	{   0,  0,   0, horns_mut},
	{   0,  0,   0, beak_mut},
	{   0,  0,   0, attract_demon_mut},
	{   0,  0,   0, produce_mana_mut},
	{   0,  0,   0, speed_flux_mut},
	{   0,  0,   0, random_banish_mut},
	{   0,  0,   0, eat_light_mut},
	{   0,  0,   0, trunk_mut},
};

/*
 * Table describing the available mutations, used internally for allocation
 * of random mutations, and various processing callbacks
 */
static mutation_info _mutations[MAX_MUTATIONS] = 
{
	{ MUT_RATING_GOOD,		MUT_TYPE_ACTIVATION, A_DEX, 8 },	/* MUT_SPIT_ACID */
	{ MUT_RATING_GOOD,		MUT_TYPE_ACTIVATION, A_CON, 6 },	/* MUT_BR_FIRE */
	{ MUT_RATING_AVERAGE,	MUT_TYPE_ACTIVATION, A_CHR, 4 },	/* MUT_HYPN_GAZE */
	{ MUT_RATING_AVERAGE,	MUT_TYPE_ACTIVATION, A_WIS, 4 },	/* MUT_TELEKINESIS */
	{ MUT_RATING_GOOD,		MUT_TYPE_ACTIVATION, A_WIS, 6 },	/* MUT_TELEPORT */
	{ MUT_RATING_GOOD,		MUT_TYPE_ACTIVATION, A_WIS, 4 },	/* MUT_MIND_BLAST */
	{ MUT_RATING_GOOD,		MUT_TYPE_ACTIVATION, A_CON, 4 },	/* MUT_RADIATION */
	{ MUT_RATING_GOOD,		MUT_TYPE_ACTIVATION, A_CON, 4 },	/* MUT_VAMPIRISM */
	{ MUT_RATING_GOOD,		MUT_TYPE_ACTIVATION, A_INT, 6 },	/* MUT_SMELL_METAL */
	{ MUT_RATING_GOOD,		MUT_TYPE_ACTIVATION, A_INT, 8 },	/* MUT_SMELL_MONSTERS */
	{ MUT_RATING_GOOD,		MUT_TYPE_ACTIVATION, A_WIS, 6 },	/* MUT_BLINK */
	{ MUT_RATING_AVERAGE,	MUT_TYPE_ACTIVATION, A_CON, 4 },	/* MUT_EAT_ROCK */
	{ MUT_RATING_GOOD,		MUT_TYPE_ACTIVATION, A_DEX, 4 },	/* MUT_SWAP_POS */
	{ MUT_RATING_GOOD,		MUT_TYPE_ACTIVATION, A_CON, 6 },	/* MUT_SHRIEK */
	{ MUT_RATING_AVERAGE,	MUT_TYPE_ACTIVATION, A_INT, 6 },	/* MUT_ILLUMINE */
	{ MUT_RATING_AVERAGE,	MUT_TYPE_ACTIVATION, A_WIS, 4 },	/* MUT_DET_CURSE */
	{ MUT_RATING_GREAT,		MUT_TYPE_ACTIVATION, A_STR, 6 },	/* MUT_BERSERK */
	{ MUT_RATING_AVERAGE,	MUT_TYPE_ACTIVATION, A_CON, 2 },	/* MUT_POLYMORPH */
	{ MUT_RATING_AVERAGE,	MUT_TYPE_ACTIVATION, A_INT, 4 },	/* MUT_MIDAS_TOUCH */
	{ MUT_RATING_GOOD,		MUT_TYPE_ACTIVATION, A_CON, 2 },	/* MUT_SUMMON_MOLD */
	{ MUT_RATING_GREAT,		MUT_TYPE_ACTIVATION, A_CON, 6 },	/* MUT_RESIST */
	{ MUT_RATING_GOOD,		MUT_TYPE_ACTIVATION, A_STR, 6 },	/* MUT_EARTHQUAKE */
	{ MUT_RATING_GREAT,		MUT_TYPE_ACTIVATION, A_WIS, 2 },	/* MUT_EAT_MAGIC */
	{ MUT_RATING_AVERAGE,	MUT_TYPE_ACTIVATION, A_INT, 4 },	/* MUT_WEIGH_MAGIC */
	{ MUT_RATING_GREAT,		MUT_TYPE_ACTIVATION, A_CHR, 2 },	/* MUT_STERILITY */
	{ MUT_RATING_GOOD,		MUT_TYPE_ACTIVATION, A_DEX, 4 },	/* MUT_PANIC_HIT */
	{ MUT_RATING_GOOD,		MUT_TYPE_ACTIVATION, A_CHR, 6 },	/* MUT_DAZZLE */
	{ MUT_RATING_GOOD,		MUT_TYPE_ACTIVATION, A_WIS, 6 },	/* MUT_LASER_EYE */
	{ MUT_RATING_GOOD,		MUT_TYPE_ACTIVATION, A_INT, 4 },	/* MUT_RECALL */
	{ MUT_RATING_GOOD,		MUT_TYPE_ACTIVATION, A_WIS, 2 },	/* MUT_BANISH */
	{ MUT_RATING_AVERAGE,	MUT_TYPE_ACTIVATION, A_CON, 4 },	/* MUT_COLD_TOUCH */
	{ MUT_RATING_AVERAGE,	MUT_TYPE_ACTIVATION, A_STR, 4 },	/* MUT_LAUNCHER */
	{ MUT_RATING_BAD,		MUT_TYPE_EFFECT,	     0, 2 },	/* MUT_BERS_RAGE */
	{ MUT_RATING_BAD,		MUT_TYPE_EFFECT,	     0, 2 },	/* MUT_COWARDICE */
	{ MUT_RATING_BAD,		MUT_TYPE_EFFECT,	     0, 2 },	/* MUT_TELEPORT_RND */
	{ MUT_RATING_BAD,		MUT_TYPE_EFFECT,	     0, 2 },	/* MUT_ALCOHOL */
	{ MUT_RATING_BAD,		MUT_TYPE_EFFECT,	     0, 2 },	/* MUT_HALLUCINATION */
	{ MUT_RATING_AVERAGE,	MUT_TYPE_EFFECT,	     0, 2 },	/* MUT_FLATULENT */
	{ MUT_RATING_GOOD,		              0,	     0, 4 },	/* MUT_SCORPION_TAIL */
	{ MUT_RATING_GOOD,		              0,	     0, 4 },	/* MUT_HORNS */
	{ MUT_RATING_GOOD,		              0,	     0, 4 },	/* MUT_BEAK */
	{ MUT_RATING_BAD,		MUT_TYPE_EFFECT,	     0, 4 },	/* MUT_ATTRACT_DEMON */
	{ MUT_RATING_AVERAGE,	MUT_TYPE_EFFECT,	     0, 2 },	/* MUT_PRODUCE_MANA */
	{ MUT_RATING_BAD,		MUT_TYPE_EFFECT,	     0, 4 },	/* MUT_SPEED_FLUX */
	{ MUT_RATING_AVERAGE,	MUT_TYPE_EFFECT,	     0, 4 },	/* MUT_BANISH_ALL_RND */
	{ MUT_RATING_AVERAGE,	MUT_TYPE_EFFECT,	     0, 2 },	/* MUT_BANISH_EAT_LIGHT */
	{ MUT_RATING_GOOD,		              0,	     0, 4 },	/* MUT_TRUNK */
};

int _mut_prob_gain(int i)
{
	int result = _mutations[i].prob;

	/* TODO: Tweak probabilities for various races ... */

	if ( _mutations[i].rating < MUT_RATING_AVERAGE
	  && mut_present(MUT_GOOD_LUCK) )
	{
		result = 1;
	}

	if ( _mutations[i].rating > MUT_RATING_AVERAGE
	  && mut_present(MUT_BAD_LUCK) )
	{
		result = 1;
	}

	return result;
}

int _mut_prob_lose(int i)
{
	int result = _mutations[i].prob;

	/* TODO: Tweak probabilities for various races ... */

	if ( _mutations[i].rating > MUT_RATING_AVERAGE
	  && mut_present(MUT_GOOD_LUCK) )
	{
		result = 1;
	}

	if ( _mutations[i].rating < MUT_RATING_AVERAGE
	  && mut_present(MUT_BAD_LUCK) )
	{
		result = 1;
	}

	return result;
}

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
				cur = _mut_prob_gain(i);
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

int mut_get_powers(spell_info* spells, int max)
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
		if (mut_present(i) && !mut_locked(i))
		{
			if (pred == NULL || (pred(i)))
				cur = cur = _mut_prob_lose(i);
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

