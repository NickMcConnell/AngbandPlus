#include "angband.h"

/*
 * We are using p_ptr->magic_num to remember talents.
 * 0 indicates no talent for this group, so we always subtract
 * 1 before indexing into an array.
 */

typedef struct {
int			stat;
cptr		gain_desc;
spell_info	spell;
} talent_t;

/*
 * Talents are grouped.  Each group of talent will contain abilities
 * of a similar kind, such as offense, defense, detection.
 */
#define _MAX_TALENTS 23
#define _MAX_TALENTS_PER_GROUP 10

static talent_t _talents[_MAX_TALENTS][_MAX_TALENTS_PER_GROUP] = 
{
	/* CL1: Weak offense */
	{
		{ A_INT, "like a Dark Elf", {1, 1, 30, magic_missile_spell}},
		{ A_STR, "like an Android", {1, 1, 30, android_ray_gun_spell}},
		{ A_CON, "like a Vampire", {1, 1, 70, vampirism_spell}},
		{ -1, NULL, {0, 0, 0, NULL}},
	},
	/* CL3: Weak utility */
	{
		{ A_WIS, "like a Yeek", {3, 5, 30, scare_monster_spell}},
		{ A_WIS, "like a Half Orc", {3, 5, 50, remove_fear_spell}},
		{ A_CHR, "like a Warlock", {3, 5, 60, satisfy_hunger_spell}},
		{ A_CHR, "like a Warlock", {3, 5, 40, light_area_spell}},
		{ A_CHR, "like a Mutant", {3, 12, 40, hypnotic_gaze_spell}},		
		{ -1, NULL, {0, 0, 0, NULL}},
	},
	/* CL5: Middle Utility */
	{
		{ A_INT, "like a Gnome", {3, 5, 50, phase_door_spell}},
		{ A_DEX, "like an Archer", {1, 0, 0, create_ammo_spell}},
		{ A_WIS, "like a Dwarf", {5, 5, 50, detect_doors_stairs_traps_spell}},
		{ -1, NULL, {0, 0, 0, NULL}},
	},

	/* CL7: Middle Utility */
	{
		{ A_INT, "like a Mutant", {3, 2, 30, smell_metal_spell}},
		{ A_CHR, "like a Warlock", {5, 5, 40, detect_monsters_spell}},
		{ A_CHR, "like a Warlock", {5, 5, 50, detect_objects_spell}},
		{ A_CON, "like a Mutant", {7, 12, 40, eat_rock_spell}},
		{ -1, NULL, {0, 0, 0, NULL}},
	},
	/* CL9: Middle Offense */
	{
		{ A_CHR, "like a Beastmaster", {1, 0, 30, dominate_living_I_spell}},
		{ A_WIS, "like an Imp", {9, 15, 50, imp_fire_spell}},
		{ A_STR, "like an Android", {9, 13, 30, android_blaster_spell}},
		{ A_INT, "like a Sprite", {9, 12, 50, sleeping_dust_spell}},
		{ A_DEX, "like a Kobold", {9, 8, 50, poison_dart_spell}},
		{ -1, NULL, {0, 0, 0, NULL}},
	},
	/* CL11: Middle Offense */
	{
		{ A_CON, "like a Mutant", {11, 15, 30, radiation_spell}},
		{ A_CON, "like a Mutant", {11, 14, 40, shriek_spell}},
		{ A_WIS, "like a Mutant", {7, 10, 50, laser_eye_spell}},
		{ A_CON, "like a Mutant", {2, 2, 30, cold_touch_spell}},
		{ A_STR, "like a Mutant", {1, 0, 40, power_throw_spell}},
		{ -1, NULL, {0, 0, 0, NULL}},
	},
	/* CL13: Good Utility */
	{
		{ A_DEX, "like a Berserker", {10, 10, 50, recall_spell}},
		{ A_INT, "like a Hobbit", {13, 10, 50, create_food_spell}},
		{ A_CHR, "like an Ent", {10, 20, 50, summon_tree_spell}},
		{ -1, NULL, {0, 0, 0, NULL}},
	},
	/* CL15: Middle Offense */
	{
		{ A_INT, "like a Mindflayer", {15, 12, 50, mind_blast_spell}},
		{ A_CON, "like a Balrog", {15, 10, 50, demon_breath_spell}},
		{ A_CHR, "like a Beastmaster", {15, 0, 50, dominate_living_II_spell}},
		{ -1, NULL, {0, 0, 0, NULL}},
	},
	/* CL17: Middle Offense */
	{
		{ A_STR, "like a Cyclops", {17, 15, 50, throw_boulder_spell}},
		{ A_STR, "like an Android", {17, 26, 40, android_bazooka_spell}},
		{ -1, NULL, {0, 0, 0, NULL}},
	},
	/* CL19: Good Utility */
	{
		{ A_INT, "like a Half Titan", {15, 10, 40, probing_spell}},
		{ A_STR, "like a Half Giant", {19, 10, 40, stone_to_mud_spell}},
		{ A_CHR, "like a Warlock", {19, 20, 50, identify_spell}},
		{ A_CHR, "like a Warlock", {19, 10, 40, teleport_spell}},
		{ A_INT, "like a Mutant", {15, 12, 40, swap_pos_spell}},
		{ -1, NULL, {0, 0, 0, NULL}},
	},
	/* CL21: Good Buff*/
	{
        { A_STR, "like a Half Troll", {10, 12, 50, berserk_spell}},
		{ A_CON, "like a Golem", {20, 15, 50, stone_skin_spell}},		
		{ A_CHR, "like a Kutar", {20, 15, 40, kutar_expand_spell}},
		{ A_CHR, "like a Warlock", {15, 10, 30, heroism_spell}},
		{ A_CHR, "like a Warlock", {21, 40, 40, protection_from_evil_spell}},
		{ -1, NULL, {0, 0, 0, NULL}},
	},
	/* CL23: Good Recovery */
	{
        { A_WIS, "like a Zombie", {23, 30, 50, restore_life_spell}},
		{ A_WIS, "like a Force-Trainer", {15, 0, 30, clear_mind_spell}},
		{ A_CHR, "like a Warlock", {20, 20, 50, remove_curse_I_spell}},
		{ -1, NULL, {0, 0, 0, NULL}},
	},
	/* CL25: Good Offense */
	{
		{ A_CON, "like a Draconian", {1, 0, 40, draconian_breath_spell}},
		{ A_DEX, "like a Klackon", {9, 9, 40, spit_acid_spell}},
		{ A_CON, "like a Mutant", {20,  0,  40, breathe_fire_spell}},
		{ -1, NULL, {0, 0, 0, NULL}},
	},
	/* CL27: Good Utility */
	{
		{ A_DEX, "like a Rogue", {8, 12, 50, panic_hit_spell}},
		{ A_INT, "like a Half Ogre", {25, 35, 40, explosive_rune_spell}},
		{ A_CHR, "like a Warlock", {25, 20, 50, magic_mapping_spell}},
		{ A_CHR, "like a Warlock", {25, 40, 50, recharging_spell}},
		{ A_INT, "like a Mutant", {10, 5, 50, alchemy_spell}},
		{ A_WIS, "like a Mutant", {9, 9, 40, telekinesis_spell}},
		{ -1, NULL, {0, 0, 0, NULL}},
	},
	/* CL29: Good Recovery */
	{
		{ A_INT, "like a Warrior-Mage", {25, 0, 50, hp_to_sp_spell}},
		{ A_INT, "like a Warrior-Mage", {25, 0, 50, sp_to_hp_spell}},
		{ A_INT, "like a Mage", {25, 1, 70, eat_magic_spell}},
		{ -1, NULL, {0, 0, 0, NULL}},
	},
	/* CL31: Good Offense */
	{
		{ A_WIS, "like a Good Paladin", {30, 30, 40, holy_lance_spell}},
		{ A_WIS, "like an Evil Paladin", {30, 30, 40, hell_lance_spell}},
		{ A_STR, "like an Android", {30, 40, 50, android_beam_cannon_spell}},
		{ -1, NULL, {0, 0, 0, NULL}},
	},
	/* CL33: Good Utility */
	{
		{ A_INT, "like an Amberite", {30, 50, 50, shadow_shifting_spell}},
		{ A_INT, "like a Tourist", {25, 20, 30, identify_fully_spell}},
		{ A_CHR, "like a Warlock", {30, 10, 40, earthquake_spell}},
		{ A_CHR, "like a Warlock", {30, 20, 40, teleport_level_spell}},
		{ -1, NULL, {0, 0, 0, NULL}},
	},
	/* CL35: Good Offense/Ability */
	{
		{ A_DEX, "like a Warrior", {33, 75, 60, sword_dance_spell}},
		{ A_DEX, "like a Monk", {30, 30, 60, monk_double_attack_spell}},
		{ A_DEX, "like a Ninja", {20, 0, 0, quick_walk_spell}},
		{ -1, NULL, {0, 0, 0, NULL}},
	},
	/* CL37: Great Buff */
	{
		{ A_DEX, "like a Monk", {25, 0, 0, monk_posture_spell}},
		{ A_DEX, "like a Samurai", {25, 0, 0, samurai_posture_spell}},
		{ A_CON, "like a Mutant", {25, 30, 50, resist_elements_spell}},
		{ -1, NULL, {0, 0, 0, NULL}},
	},
	/* CL39: Great Utility */
	{
		{ A_CHR, "like a Mutant", {12, 23, 70, sterility_spell}},
		{ A_CHR, "like a Mutant", {7, 15, 60, dazzle_spell}},
		{ A_WIS, "like a Mutant", {25, 25, 70, banish_evil_spell}},
		{ A_CON, "like a Mutant", {1, 6, 60, grow_mold_spell}},
		{ -1, NULL, {0, 0, 0, NULL}},
	},
	/* CL41: Great Utility */
	{
		{ A_WIS, "like an Amberite", {40, 75, 75, pattern_mindwalk_spell}},
		{ A_CHR, "like a Warlock", {35, 70, 60, destruction_spell}},
		{ A_CHR, "like a Warlock", {46, 20, 65, dimension_door_spell}},
		{ -1, NULL, {0, 0, 0, NULL}},
	},
	/* CL43: Great Utility */
	{
		{ A_INT, "like a Chaos Warrior", {40, 50, 80, confusing_lights_spell}},
		{ A_WIS, "like an Evil Priest", {42, 40, 80, evocation_spell}},
		{ -1, NULL, {0, 0, 0, NULL}},
	},
	/* CL45: Great Offense */
	{
		{ A_STR, "like an Android", {43, 60, 70, android_rocket_spell}},
		{ A_WIS, "like an Mindcrafter", {43, 60, 70, psycho_spear_spell}},
		{ -1, NULL, {0, 0, 0, NULL}},
	},
};

static int _group_size(int i)
{
	int result = 0;
	if (i >= 0 && i < _MAX_TALENTS)
	{
		int j;
		for (j = 0; j < _MAX_TALENTS_PER_GROUP ; ++j)
		{
			talent_t *talent_ptr = &_talents[i][j];
			if (talent_ptr->stat == -1)
			{
				result = j;
				break;
			}
		}
	}
	return result;
}

static int _get_powers(spell_info* spells, int max)
{
	int ct = 0, i;

	for (i = 0; i < _MAX_TALENTS; ++i)
	{
		int idx = p_ptr->magic_num1[i] - 1;	/* Magic Numbers are base 1, Table indices base 0 */
		if (idx >= 0 && idx < _group_size(i))
		{
			talent_t *talent = &_talents[i][idx];
			spell_info* spell = &spells[ct++];
			spell->level = talent->spell.level;
			spell->cost = talent->spell.cost;
			spell->fail = calculate_fail_rate(
				talent->spell.level, 
				talent->spell.fail, 
				p_ptr->stat_ind[talent->stat]
			);
			spell->fn = talent->spell.fn;
		}
	}

	return ct;
}

void _gain_power(int level)
{
int group_idx = -1;

	/* Powers on every odd level from 1 to 45 */
	if (level <= 45 && level % 2 == 1)
		group_idx = level/2;

	if (group_idx >= 0 && group_idx < _MAX_TALENTS)
	{
		variant name;
		int idx = randint0(_group_size(group_idx));
		talent_t *talent = &_talents[group_idx][idx];

		if (!talent->spell.fn)
		{
			msg_print("BUG: Where is your talent?!");
			return;
		}

		var_init(&name);
		(talent->spell.fn)(SPELL_NAME, &name);

		msg_format("You gain the power of '%s' %s.", var_get_string(&name), talent->gain_desc);
		p_ptr->magic_num1[group_idx] = idx + 1;

		var_clear(&name);
	}
}

bool _mut_avg_pred(int mut_idx)
{
	if (mut_type(mut_idx) == MUT_TYPE_ACTIVATION) return FALSE;
	if (mut_rating(mut_idx) == MUT_RATING_AVERAGE) return TRUE;
	return FALSE;
}

bool _mut_avg_plus_pred(int mut_idx)
{
	if (mut_type(mut_idx) == MUT_TYPE_ACTIVATION) return FALSE;
	if (mut_rating(mut_idx) >= MUT_RATING_AVERAGE) return TRUE;
	return FALSE;
}

bool _mut_good_pred(int mut_idx)
{
	if (mut_type(mut_idx) == MUT_TYPE_ACTIVATION) return FALSE;
	if (mut_rating(mut_idx) == MUT_RATING_GOOD) return TRUE;
	return FALSE;
}

bool _mut_good_plus_pred(int mut_idx)
{
	if (mut_type(mut_idx) == MUT_TYPE_ACTIVATION) return FALSE;
	if (mut_rating(mut_idx) >= MUT_RATING_GOOD) return TRUE;
	return FALSE;
}

bool _mut_great_pred(int mut_idx)
{
	if (mut_type(mut_idx) == MUT_TYPE_ACTIVATION) return FALSE;
	if (mut_rating(mut_idx) == MUT_RATING_GREAT) return TRUE;
	return FALSE;
}

void _gain_mutation(int level)
{
	switch (level)
	{
	case 10:
		mut_gain_random(_mut_avg_plus_pred);
		break;
	case 20:
		mut_gain_random(_mut_good_pred);
		break;
	case 30:
		mut_gain_random(_mut_good_pred);
		break;
	case 40:
		mut_gain_random(_mut_good_plus_pred);
		break;
	case 47:
		mut_gain_random(_mut_good_plus_pred);
		break;
	case 49:
		mut_gain_random(_mut_great_pred);
		break;
	case 50:
		mut_gain_random(_mut_great_pred);
		break;
	}
}

void _gain_level(int new_level)
{
	_gain_power(new_level);
	_gain_mutation(new_level);
}

void wild_talent_new_life(void)
{
	int i;

	if (p_ptr->pclass != CLASS_WILD_TALENT) return;

	/* Forget old talents ... mutations have already been cleared by !New Life */
	for (i = 0; i <= _MAX_TALENTS; ++i)
		p_ptr->magic_num1[i] = 0;

	msg_print("You feel new forces of randomness enter your body!");

	/* Regain new talents and mutations */
	for (i = 1; i <= p_ptr->lev; ++i)
		_gain_level(i);
}

class_t *wild_talent_get_class_t(void)
{
	static class_t me = {0};
	static bool init = FALSE;

	/* static info never changes */
	if (!init)
	{           /* dis, dev, sav, stl, srh, fos, thn, thb */
	skills_t bs = { 30,  40,  31,   1,  24,  16,  68,  50 };
	skills_t xs = {  8,  15,  10,   0,   0,   0,  18,  20 };

		me.name = "Wild-Talent";
		me.desc = "The Wild-Talent gains random talents and abilities as they "
		          "level up.  They are good fighters, and decent with magical devices, "
				  "but their true forte is their vast array of potential random "
				  "powers.  Except you never know what those might be!";
		me.stats[A_STR] = -1;
		me.stats[A_INT] =  2;
		me.stats[A_WIS] = -1;
		me.stats[A_DEX] =  2;
		me.stats[A_CON] = -2;
		me.stats[A_CHR] = +3;
		me.base_skills = bs;
		me.extra_skills = xs;
		me.get_powers = _get_powers;
		me.gain_level = _gain_level;
		init = TRUE;
	}

	return &me;
}
