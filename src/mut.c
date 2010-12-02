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
	{  20,  0,  40, breathe_fire_I_spell},
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
	{  25, 30,  70, resist_elements_spell},
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
	{   0,  0,   0, attract_animal_mut},
	{   0,  0,   0, tentacles_mut},
	{   0,  0,   0, raw_chaos_mut},
	{   0,  0,   0, normality_mut},
	{   0,  0,   0, wraith_mut},
	{   0,  0,   0, polymorph_wounds_mut},
	{   0,  0,   0, wasting_mut},
	{   0,  0,   0, attract_dragon_mut},
	{   0,  0,   0, weird_mind_mut},
	{   0,  0,   0, nausea_mut},
	{   0,  0,   0, chaos_deity_mut},
	{   0,  0,   0, shadow_walk_mut},
	{   0,  0,   0, warning_mut},
	{   0,  0,   0, invulnerability_mut},
	{   0,  0,   0, sp_to_hp_spell},
	{   0,  0,   0, hp_to_sp_spell},
	{   0,  0,   0, fumbling_mut},
	{   0,  0,   0, he_man_mut},
	{   0,  0,   0, puny_mut},
	{   0,  0,   0, einstein_mut},
	{   0,  0,   0, moron_mut},
	{   0,  0,   0, resilient_mut},
	{   0,  0,   0, fat_mut},
	{   0,  0,   0, albino_mut},
	{   0,  0,   0, rotting_flesh_mut},
	{   0,  0,   0, silly_voice_mut},
	{   0,  0,   0, blank_face_mut},
	{   0,  0,   0, illusion_normal_mut},
	{   0,  0,   0, extra_eyes_mut},
	{   0,  0,   0, magic_resistance_mut},
	{   0,  0,   0, extra_noise_mut},
	{   0,  0,   0, infravision_mut},
	{   0,  0,   0, extra_legs_mut},
	{   0,  0,   0, short_legs_mut},
	{   0,  0,   0, elec_aura_mut},
	{   0,  0,   0, fire_aura_mut},
	{   0,  0,   0, warts_mut},
	{   0,  0,   0, scales_mut},
	{   0,  0,   0, steel_skin_mut},
	{   0,  0,   0, wings_mut},
	{   0,  0,   0, fearless_mut},
	{   0,  0,   0, regeneration_mut},
	{   0,  0,   0, telepathy_mut},
	{   0,  0,   0, limber_mut},
	{   0,  0,   0, arthritis_mut},
	{   0,  0,   0, bad_luck_mut},
	{   0,  0,   0, vulnerability_mut},
	{   0,  0,   0, motion_mut},
	{   0,  0,   0, good_luck_mut},
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
	{ MUT_RATING_AVERAGE,	MUT_TYPE_EFFECT,	     0, 2 },	/* MUT_EAT_LIGHT */
	{ MUT_RATING_GOOD,		              0,	     0, 4 },	/* MUT_TRUNK */
	{ MUT_RATING_BAD,		MUT_TYPE_EFFECT,	     0, 2 },	/* MUT_ATTRACT_ANIMAL */
	{ MUT_RATING_GOOD,		              0,	     0, 2 },	/* MUT_TENTACLES */
	{ MUT_RATING_AVERAGE,	MUT_TYPE_EFFECT,	     0, 2 },	/* MUT_RAW_CHAOS */
	{ MUT_RATING_BAD,		MUT_TYPE_EFFECT,	     0, 6 },	/* MUT_NORMALITY */
	{ MUT_RATING_GOOD,		MUT_TYPE_EFFECT,	     0, 2 },	/* MUT_WRAITH */
	{ MUT_RATING_AVERAGE,	MUT_TYPE_EFFECT,	     0, 2 },	/* MUT_POLY_WOUND */
	{ MUT_RATING_AWFUL,		MUT_TYPE_EFFECT,	     0, 2 },	/* MUT_WASTING */
	{ MUT_RATING_BAD,		MUT_TYPE_EFFECT,	     0, 2 },	/* MUT_ATTRACT_DRAGON */
	{ MUT_RATING_AVERAGE,	MUT_TYPE_EFFECT,	     0, 4 },	/* MUT_WEIRD_MIND */
	{ MUT_RATING_BAD,		MUT_TYPE_EFFECT,	     0, 2 },	/* MUT_NAUSEA */
	{ MUT_RATING_GREAT,		              0,	     0, 4 },	/* MUT_CHAOS_GIFT */
	{ MUT_RATING_BAD,		MUT_TYPE_EFFECT,	     0, 2 },	/* MUT_SHADOW_WALK */
	{ MUT_RATING_AVERAGE,	MUT_TYPE_EFFECT,	     0, 2 },	/* MUT_WARNING */
	{ MUT_RATING_GOOD,		MUT_TYPE_EFFECT,	     0, 2 },	/* MUT_INVULN */
	{ MUT_RATING_AVERAGE,	MUT_TYPE_EFFECT,	     0, 4 },	/* MUT_SP_TO_HP */
	{ MUT_RATING_AVERAGE,	MUT_TYPE_EFFECT,	     0, 2 },	/* MUT_HP_TO_SP */
	{ MUT_RATING_BAD,		MUT_TYPE_EFFECT,	     0, 2 },	/* MUT_FUMBLING */
	{ MUT_RATING_GREAT,		MUT_TYPE_BONUS,			 0, 6 },    /* MUT_HYPER_STR */
	{ MUT_RATING_AWFUL,		MUT_TYPE_BONUS,			 0, 6 },    /* MUT_PUNY */
	{ MUT_RATING_GREAT,		MUT_TYPE_BONUS,			 0, 6 },    /* MUT_HYPER_INT */
	{ MUT_RATING_AWFUL,		MUT_TYPE_BONUS,			 0, 6 },    /* MUT_MORON */
	{ MUT_RATING_GREAT,		MUT_TYPE_BONUS,			 0, 4 },    /* MUT_RESILIENT */
	{ MUT_RATING_AVERAGE,	MUT_TYPE_BONUS,			 0, 4 },    /* MUT_XTRA_FAT */
	{ MUT_RATING_AWFUL,		MUT_TYPE_BONUS,			 0, 4 },    /* MUT_ALBINO */
	{ MUT_RATING_AWFUL,		MUT_TYPE_BONUS,			 0, 6 },    /* MUT_FLESH_ROT */
	{ MUT_RATING_BAD,		MUT_TYPE_BONUS,			 0, 4 },    /* MUT_SILLY_VOICE */
	{ MUT_RATING_BAD,		MUT_TYPE_BONUS,			 0, 4 },    /* MUT_BLANK_FACE */
	{ MUT_RATING_AVERAGE,	             0,			 0, 2 },    /* MUT_ILLUS_NORM */
	{ MUT_RATING_AVERAGE,	MUT_TYPE_BONUS,			 0, 6 },    /* MUT_XTRA_EYES */
	{ MUT_RATING_GOOD,		MUT_TYPE_BONUS,			 0, 4 },    /* MUT_MAGIC_RES */
	{ MUT_RATING_BAD,		MUT_TYPE_BONUS,			 0, 6 },    /* MUT_XTRA_NOISE */
	{ MUT_RATING_AVERAGE,	MUT_TYPE_BONUS,			 0, 6 },    /* MUT_INFRAVISION */
	{ MUT_RATING_GREAT,		MUT_TYPE_BONUS,			 0, 4 },    /* MUT_XTRA_LEGS */
	{ MUT_RATING_AWFUL,		MUT_TYPE_BONUS,			 0, 4 },    /* MUT_SHORT_LEGS */
	{ MUT_RATING_GOOD,		MUT_TYPE_BONUS,			 0, 4 },    /* MUT_ELEC_AURA */
	{ MUT_RATING_GOOD,		MUT_TYPE_BONUS,			 0, 4 },    /* MUT_FIRE_AURA */
	{ MUT_RATING_AVERAGE,	MUT_TYPE_BONUS,			 0, 6 },    /* MUT_WARTS */
	{ MUT_RATING_GOOD,		MUT_TYPE_BONUS,			 0, 6 },    /* MUT_SCALES */
	{ MUT_RATING_GREAT,		MUT_TYPE_BONUS,			 0, 4 },    /* MUT_STEEL_SKIN */
	{ MUT_RATING_AVERAGE,	MUT_TYPE_BONUS,			 0, 4 },    /* MUT_WINGS */
	{ MUT_RATING_AVERAGE,	MUT_TYPE_BONUS,			 0, 6 },    /* MUT_FEARLESS */
	{ MUT_RATING_GOOD,		MUT_TYPE_BONUS,			 0, 4 },    /* MUT_REGEN */
	{ MUT_RATING_GREAT,		MUT_TYPE_BONUS,			 0, 4 },    /* MUT_ESP */
	{ MUT_RATING_GOOD,		MUT_TYPE_BONUS,			 0, 6 },    /* MUT_LIMBER */
	{ MUT_RATING_BAD,		MUT_TYPE_BONUS,			 0, 6 },    /* MUT_ARTHRITIS */
	{ MUT_RATING_AWFUL,		             0,			 0, 2 },    /* MUT_BAD_LUCK */
	{ MUT_RATING_AWFUL,		             0,			 0, 2 },    /* MUT_VULN_ELEM */
	{ MUT_RATING_GOOD,		MUT_TYPE_BONUS,			 0, 6 },    /* MUT_MOTION */
	{ MUT_RATING_GREAT,				     0,			 0, 2 },    /* MUT_GOOD_LUCK */
};

int _mut_prob_gain(int i)
{
	int result = _mutations[i].prob;
	const int racial_odds = 50;

	switch (i)
	{
	case MUT_CHAOS_GIFT:
		/* TODO: Birth Chaos Warriors with this mutation */
		if (p_ptr->pclass == CLASS_CHAOS_WARRIOR)
			return 0;
		break;

	case MUT_BAD_LUCK:
		if (mut_locked(MUT_GOOD_LUCK))
			return 0;
		break;

	case MUT_HYPN_GAZE:
		if (p_ptr->prace == RACE_VAMPIRE)
			return racial_odds;
		break;

	case MUT_HORNS:
		if (p_ptr->prace == RACE_IMP)
			return racial_odds;
		break;

	case MUT_SHRIEK:
		if (p_ptr->prace == RACE_YEEK)
			return racial_odds;
		break;

	case MUT_POLYMORPH:
		if (p_ptr->prace == RACE_BEASTMAN)
			return racial_odds;
		break;

	case MUT_TENTACLES:
		if (p_ptr->prace == RACE_MIND_FLAYER)
			return racial_odds;
		break;
	}

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

void _mut_refresh(void)
{
	mutant_regenerate_mod = mut_regenerate_mod();
	p_ptr->update |= PU_BONUS;
	handle_stuff();
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

int mut_count(mut_pred pred)
{
	int i;
	int count = 0;
	for (i = 0; i < MAX_MUTATIONS; ++i)
	{
		if (mut_present(i))
		{
			if (pred == NULL || (pred)(i))
				++count;
		}
	}

	return count;
}

void mut_do_cmd_knowledge(void)
{
	FILE *fff;
	char file_name[1024];

	/* Open a new file */
	fff = my_fopen_temp(file_name, 1024);

	/* Dump the mutations to file */
	if (fff) mut_dump_file(fff);

	/* Close the file */
	my_fclose(fff);

	/* Display the file contents */
	show_file(TRUE, file_name, T("Mutations", "突然変異"), 0, 0);

	/* Remove the file */
	fd_kill(file_name);
}

void mut_dump_file(FILE* file)
{
	int i;
	variant desc;
	var_init(&desc);
	for (i = 0; i < MAX_MUTATIONS; ++i)
	{
		if (mut_present(i))
		{
			(_mutation_spells[i].fn)(SPELL_MUT_DESC, &desc);
			fprintf(file, "%s\n", var_get_string(&desc));
		}
	}
	var_clear(&desc);
}

bool mut_gain(int mut_idx)
{
	variant v;

	if (mut_idx < 0 || mut_idx >= MAX_MUTATIONS) return FALSE;
	if (mut_present(mut_idx)) return FALSE;
	
	var_init(&v);
	add_flag(p_ptr->muta, mut_idx);
	(_mutation_spells[mut_idx].fn)(SPELL_GAIN_MUT, &v);
	var_clear(&v);

	_mut_refresh();
	return TRUE;
}

int mut_gain_random_aux(mut_pred pred)
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
		
		/* scan the probability table for the correct mutation */
		for (i = 0; i < MAX_MUTATIONS; i++)
		{
			if (j < prob[i])
			{
				return i;
			}
		}
	}

	return -1;
}

bool mut_gain_random(mut_pred pred)
{
	int which = mut_gain_random_aux(pred);
	if (which >= 0 && !mut_present(which))
	{
		chg_virtue(V_CHANCE, 1);
		return mut_gain(which);
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

			current->fail = calculate_fail_rate(base->level, base->fail, stat_idx);			
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
	if (mut_idx < 0 || mut_idx >= MAX_MUTATIONS) return;
	if (mut_locked(mut_idx)) return;
	add_flag(p_ptr->muta_lock, mut_idx);
}

bool mut_locked(int mut_idx)
{
	if (mut_idx < 0 || mut_idx >= MAX_MUTATIONS) return FALSE;
	return have_flag(p_ptr->muta_lock, mut_idx);
}

bool mut_lose(int mut_idx)
{
	variant v;
	if (mut_idx < 0 || mut_idx >= MAX_MUTATIONS) return FALSE;
	if (!mut_present(mut_idx)) return FALSE;
	if (mut_locked(mut_idx)) return FALSE;
	
	var_init(&v);
	remove_flag(p_ptr->muta, mut_idx);
	(_mutation_spells[mut_idx].fn)(SPELL_LOSE_MUT, &v);
	var_clear(&v);

	_mut_refresh();
	return TRUE;
}

void mut_lose_all(void)
{
	if (mut_count(mut_unlocked_pred))
	{
		int i;
		chg_virtue(V_CHANCE, -5);
		msg_print(T("You are cured of all mutations.", "全ての突然変異が治った。"));

		for (i = 0; i < MUT_FLAG_SIZE; ++i)
			p_ptr->muta[i] = p_ptr->muta_lock[i];

		_mut_refresh();
	}
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
			return mut_lose(which);
		}
	}

	msg_print("You feel strange.");
	return FALSE;
}

void mut_name(int i, char* buf)
{
	variant v;
	var_init(&v);

	if (i >= 0 && i < MAX_MUTATIONS)
		(_mutation_spells[i].fn)(SPELL_NAME, &v);
	else
		var_set_string(&v, "None");

	sprintf(buf, "%s", var_get_string(&v));
	var_clear(&v);
}

bool mut_present(int mut_idx)
{
	if (mut_idx < 0 || mut_idx >= MAX_MUTATIONS) return FALSE;
	return have_flag(p_ptr->muta, mut_idx);
}

void mut_process(void)
{
	int i;
	variant v;

	/* No effect on monster arena */
	if (p_ptr->inside_battle) return;

	/* No effect on the global map */
	if (p_ptr->wild_mode) return;

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
	if (mut_idx < 0 || mut_idx >= MAX_MUTATIONS) return MUT_RATING_AWFUL;
	return _mutations[mut_idx].rating;
}

int mut_regenerate_mod(void)
{
	int regen;
	int mod = 10;
	int count = mut_count(mut_unlocked_pred);

	/*
	 * Beastman get 10 "free" mutations and
	 * only 5% decrease per additional mutation
	 */

	if (p_ptr->prace == RACE_BEASTMAN)
	{
		count -= 10;
		mod = 5;
	}

	/* No negative modifier */
	if (count <= 0) return 100;

	regen = 100 - count * mod;

	/* Max. 90% decrease in regeneration speed */
	if (regen < 10) regen = 10;

	return regen;
}

int mut_type(int mut_idx)
{
	if (mut_idx < 0 || mut_idx >= MAX_MUTATIONS) return 0;
	return _mutations[mut_idx].type;
}

void mut_unlock(int mut_idx)
{
	if (mut_idx < 0 || mut_idx >= MAX_MUTATIONS) return;
	if (!mut_locked(mut_idx)) return;
	remove_flag(p_ptr->muta_lock, mut_idx);
}

bool mut_unlocked_pred(int mut_idx)
{
	return !mut_locked(mut_idx);
}