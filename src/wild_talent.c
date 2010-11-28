#include "angband.h"

/*
 * We are using p_ptr->magic_num to remember talents.
 * 0 indicates no talent for this group, so we always subtract
 * 1 before indexing into an array.
 */

typedef struct {
byte		stat;
cptr		gain_desc;
spell_info	spell;
} talent_t;

/*
 * Talents are grouped.  Each group of talent will contain abilities
 * of a similar kind, such as offense, defense, detection.
 */
#define _MAX_TALENTS 13
#define _MAX_TALENTS_PER_GROUP 10

/* Sorry, but please keep this table of groups sizes up to date! */
static int _group_sizes[_MAX_TALENTS] = { 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};

static talent_t _talents[_MAX_TALENTS][_MAX_TALENTS_PER_GROUP] = 
{
	/* CL1: Weak offense */
	{
		{ A_INT, "like a Dark Elf", {1, 1, 30, magic_missile_spell}},
		{ A_STR, "like an Android", {1, 1, 30, android_ray_gun_spell}},
		{ A_CON, "like a Vampire", {1, 1, 70, vampirism_spell}},
		{ A_WIS, "like a Yeek", {1, 5, 30, scare_monster_spell}},
	},
};


/* Available Powers: Note, I played with level, cost, fail, stat, etc.
   OFFENSE
		{ A_INT, "like a Dark Elf", {1, 1, 30, magic_missile_spell}},
		{ A_STR, "like an Android", {1, 1, 30, android_ray_gun_spell}},
		{ A_CON, "like a Vampire", {1, 1, 70, vampirism_spell}},
		{ A_WIS, "like a Yeek", {1, 5, 30, scare_monster_spell}},

		{ A_CHR, "like a Beastmaster", {1, 0, 70, dominate_living_I_spell}},
		{ A_CON, "like a Draconian", {1, 0, 70, draconian_breath_spell}},
		{ A_WIS, "like an Imp", {9, 15, 50, imp_fire_spell}},
		{ A_DEX, "like a Klackon", {9, 9, 50, spit_acid_spell}},
		{ A_STR, "like an Android", {10, 13, 30, android_blaster_spell}},
		{ A_INT, "like a Sprite", {12, 12, 50, sleeping_dust_spell}},
		{ A_DEX, "like a Kobold", {12, 8, 50, poison_dart_spell}},

		{ A_INT, "like a Mindflayer", {15, 12, 50, mind_blast_spell}},
		{ A_CON, "like a Balrog", {15, 10, 70, demon_breath_spell}},
		{ A_STR, "like a Cyclops", {20, 15, 50, throw_boulder_spell}},
		{ A_STR, "like an Android", {25, 26, 40, android_bazooka_spell}},
		{ A_CHR, "like a Beastmaster", {30, 0, 70, dominate_living_II_spell}},

		{ A_WIS, "like a Good Paladin", {30, 30, 70, holy_lance_spell}},
		{ A_WIS, "like an Evil Paladin", {30, 30, 70, hell_lance_spell}},
		{ A_STR, "like an Android", {35, 40, 50, android_beam_cannon_spell}},
		{ A_DEX, "like a Warrior", {40, 75, 80, sword_dance_spell}},
		{ A_DEX, "like a Monk", {30, 30, 80, monk_double_attack_spell}},

		{ A_INT, "like a Chaos Warrior", {40, 50, 80, confusing_lights_spell}},
		{ A_WIS, "like an Evil Priest", {42, 40, 80, evocation_spell}},
		{ A_STR, "like an Android", {45, 60, 70, android_rocket_spell}},


   BUFF
        { A_STR, "like a Half Troll", {10, 12, 50, berserk_spell}},
		{ A_CON, "like a Golem", {20, 15, 50, stone_skin_spell}},		
		{ A_CHR, "like a Kutar", {20, 15, 70, kutar_expand_spell}},
		{ A_CHR, "like a Warlock", {15, 10, 60, heroism_spell}},

		{ A_DEX, "like a Monk", {25, 0, 0, monk_posture_spell}},
		{ A_DEX, "like a Samurai", {25, 0, 0, samurai_posture_spell}},
		{ A_CHR, "like a Warlock", {35, 40, 60, protection_from_evil_spell}},
		{ A_DEX, "like a Ninja", {20, 0, 0, quick_walk_spell}},


   RECOVERY
        { A_WIS, "like a Zombie", {30, 30, 70, restore_life_spell}},
		{ A_INT, "like a Warrior-Mage", {25, 0, 50, hp_to_sp_spell}},
		{ A_INT, "like a Warrior-Mage", {25, 0, 50, sp_to_hp_spell}},
		{ A_WIS, "like a Force-Trainer", {15, 0, 30, clear_mind_spell}},
		{ A_INT, "like a Mage", {25, 1, 90, eat_magic_spell}},
		{ A_CHR, "like a Warlock", {20, 20, 60, remove_curse_I_spell}},

   UTILITY
		{ A_WIS, "like a Half Orc", {3, 5, 50, remove_fear_spell}},
		{ A_INT, "like a Gnome", {5, 5, 50, phase_door_spell}},
		{ A_DEX, "like an Archer", {1, 0, 0, create_ammo_spell}},
		{ A_CHR, "like a Warlock", {5, 5, 60, satisfy_hunger_spell}},

		{ A_WIS, "like a Dwarf", {5, 5, 50, detect_doors_stairs_traps_spell}},
		{ A_CHR, "like a Warlock", {5, 5, 60, detect_objects_spell}},
		{ A_CHR, "like a Warlock", {5, 5, 40, detect_monsters_spell}},
		{ A_CHR, "like a Warlock", {5, 5, 40, light_area_spell}},
		
		{ A_DEX, "like a Berserker", {10, 10, 70, recall_spell}},
		{ A_INT, "like a Hobbit", {15, 10, 50, create_food_spell}},
		{ A_CHR, "like an Ent", {10, 20, 70, summon_tree_spell}},
		{ A_INT, "like a Half Titan", {15, 10, 60, probing_spell}},
		{ A_STR, "like a Half Giant", {20, 10, 70, stone_to_mud_spell}},
		{ A_CHR, "like a Warlock", {20, 20, 70, identify_spell}},
		{ A_CHR, "like a Warlock", {20, 10, 50, teleport_spell}},

		{ A_DEX, "like a Rogue", {8, 12, 80, panic_hit_spell}},
		{ A_INT, "like a Half Ogre", {25, 35, 70, explosive_rune_spell}},
		{ A_INT, "like an Amberite", {30, 50, 70, shadow_shifting_spell}},
		{ A_INT, "like a Tourist", {25, 20, 30, identify_fully_spell}},
		{ A_CHR, "like a Warlock", {30, 10, 60, earthquake_spell}},
		{ A_CHR, "like a Warlock", {30, 20, 60, teleport_level_spell}},
		{ A_CHR, "like a Warlock", {35, 20, 50, magic_mapping_spell}},
		{ A_CHR, "like a Warlock", {35, 40, 70, recharging_spell}},

		{ A_WIS, "like an Amberite", {40, 75, 75, pattern_mindwalk_spell}},
		{ A_INT, "like a Red-Mage", {48, 20, 0, double_magic_spell}},
		{ A_CHR, "like a Warlock", {35, 70, 60, destruction_spell}},
		{ A_CHR, "like a Warlock", {50, 20, 65, dimension_door_spell}},
*/

static int _get_powers(spell_info* spells, int max)
{
	int ct = 0, i;

	for (i = 0; i < _MAX_TALENTS; ++i)
	{
		int idx = p_ptr->magic_num1[i] - 1;	/* Magic Numbers are base 1, Table indices base 0 */
		if (idx >= 0 && idx < _group_sizes[i])
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

	/* Of course, exactly when we gain powers is liable to change ... 
	   So, I just hardcode the levels for now. */
	switch (level)
	{
	case 1: group_idx = 0; break;
	case 7: group_idx = 1; break;
	case 10: group_idx = 2; break;
	case 13: group_idx = 3; break;
	case 19: group_idx = 4; break;
	case 25: group_idx = 5; break;
	case 28: group_idx = 6; break;
	case 34: group_idx = 7; break;
	case 37: group_idx = 8; break;
	case 43: group_idx = 9; break;
	case 46: group_idx = 10; break;
	case 49: group_idx = 11; break;
	case 50: group_idx = 12; break;
	}

	if (group_idx >= 0 && group_idx < _MAX_TALENTS)
	{
		variant name;
		int idx = randint0(_group_sizes[group_idx]);
		talent_t *talent = &_talents[group_idx][idx];

		var_init(&name);
		(talent->spell.fn)(SPELL_NAME, &name);

		msg_format("You gain the power of '%s' %s.", var_get_string(&name), talent->gain_desc);

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
	case 4:
		mut_gain_random(_mut_avg_pred);
		break;
	case 16:
		mut_gain_random(_mut_good_pred);
		break;
	case 22:
		mut_gain_random(_mut_good_pred);
		break;
	case 31:
		mut_gain_random(_mut_good_plus_pred);
		break;
	case 40:
		mut_gain_random(_mut_great_pred);
		break;
	}
}

void _gain_level(int new_level)
{
	_gain_power(new_level);
	_gain_mutation(new_level);
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
