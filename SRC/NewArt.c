/* File: NewArt.c */


/*
 * Copyright (c) 1997 Ben Harrison
 * Copyright (c) 2002 - 2003 Alexander Pirozhkov (pirozh@sci.lebedev.ru)
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"

#include "init.h"

int last_power = 0;
int names_list_len = 0;
char* new_art_names = 0;

/* Alex: Constants for new artifacts construction */
enum
{
	/*Constants for choosing rarity of new ability, see add_ability()*/
	ART_R_1 = 25,
	ART_R_2 = 50,
	ART_R_3 = 100,
	/*art power = depth / ART_STORM_DIV + Rand_normal(0, ART_RND),
	 *double depth for Mana Storm, see generate_storm_artifacts()*/
	/* ART_STORM_DIV = 2, */
	/*Maximum possible a_ptr->ac increase*/
	ART_MAX_EXTRA_AC = 10,
	/*Determine weapon artifact power increase with to_hit, to_dam, see artifact_power()*/
	ART_W_1 = 10,
	ART_W_2 = 20,
	ART_W_3 = 30,/*Maximum possible for new weapon artifacts*/
	/*Determine armor artifact power increase with to_ac, see artifact_power()*/
	ART_A_1 = 20,
	ART_A_2 = 30,
	ART_A_3 = 40,/*Maximum possible for new armor artifacts*/
	/*Determine artifact power increase for:
	* non-weapon with to_hit, to_dam, and non-armor for to_ac, see artifact_power()*/
	ART_1 = 5,
	ART_2 = 10,
	ART_3 = 15,/*Maximum possible*/
	ART_WEIGHT_DIV = 10, /*+1 power for 10 weight decrease; remember, weight is in 0.1 lb. units*/
	ART_ACT_CHANCE = 4, /* Activations are proposed with 1/ART_ACT_CHANCE probability */
	ART_ACT_TIME_CHANGE_CHANCE = 3	/* Determine increase/decrease chance of recharge time - see add_activation() */
};
/* Abilities and activations rarity */
enum Rarity{r_common, r_uncommon, r_rare, r_vrare, r_last};

cptr rarity_name[r_last] = {"Common", "Uncommon", "Rare", "Very Rare"};

/* Power increase for activation - also depends on recharge time, see art_power() */
int activation_power[r_last] = {2, 5, 10, 20};

/* Constants for recherge time */
int act_recharge_mul[r_last] = {5, 10, 25, 50};

const enum Rarity act_rarity[ACT_MAX] =
{
	r_common,	/*ACT_ILLUMINATION        0*/
	r_uncommon,	/*ACT_MAGIC_MAP           1*/
	r_rare,		/*ACT_CLAIRVOYANCE        2*/
	r_uncommon,	/*ACT_PROT_EVIL           3*/
	r_rare,		/*ACT_DISP_EVIL           4*/
	r_rare,		/*ACT_HEAL1               5*/
	r_vrare,	/*ACT_HEAL2               6*/
	r_common,	/*ACT_CURE_WOUNDS         7*/
	r_uncommon,	/*ACT_HASTE1              8*/
	r_rare,		/*ACT_HASTE2              9*/
	r_common,	/*ACT_FIRE1               10*/
	r_uncommon,	/*ACT_FIRE2               11*/
	r_rare,		/*ACT_FIRE3               12*/
	r_common,	/*ACT_FROST1              13*/
	r_uncommon,	/*ACT_FROST2              14*/
	r_rare,		/*ACT_FROST3              15*/
	r_rare,		/*ACT_FROST4              16*/
	r_vrare,	/*ACT_FROST5              17*/
	r_common,	/*ACT_ACID1               18*/
	r_uncommon,	/*ACT_RECHARGE1           19*/
	r_uncommon,	/*ACT_SLEEP               20*/
	r_common,	/*ACT_LIGHTNING_BOLT      21*/
	r_vrare,	/*ACT_ELEC2               22*/
	r_rare,		/*ACT_BANISHMENT          23*/
	r_vrare,	/*ACT_MASS_BANISHMENT     24*/
	r_uncommon,	/*ACT_IDENTIFY            25*/
	r_uncommon,	/*ACT_DRAIN_LIFE1         26*/
	r_rare,		/*ACT_DRAIN_LIFE2         27*/
	r_vrare,		/*ACT_BIZZARE             28*/
	r_vrare,	/*ACT_STAR_BALL           29*/
	r_vrare,	/*ACT_RAGE_BLESS_RESIST   30*/
	r_common,	/*ACT_PHASE               31*/
	r_uncommon,	/*ACT_TRAP_DOOR_DEST      32*/
	r_uncommon,	/*ACT_DETECT              33*/
	r_uncommon,	/*ACT_RESIST              34*/
	r_uncommon,	/*ACT_TELEPORT            35*/
	r_uncommon,	/*ACT_RESTORE_LIFE        36*/
	r_common,	/*ACT_MISSILE             37*/
	r_rare,		/*ACT_ARROW               38*/
	r_uncommon,	/*ACT_REM_FEAR_POIS       39*/
	r_common,	/*ACT_STINKING_CLOUD      40*/
	r_uncommon,	/*ACT_STONE_TO_MUD        41*/
	r_rare,		/*ACT_TELE_AWAY           42*/
	r_uncommon,	/*ACT_WOR                 43*/
	r_uncommon,	/*ACT_CONFUSE             44*/
	r_rare,		/*ACT_PROBE               45*/
	r_vrare,	/*ACT_FIREBRAND           46*/
	r_uncommon,	/*ACT_STARLIGHT           47*/
	r_uncommon,	/*ACT_MANA_BOLT           48*/
	r_uncommon	/*ACT_BERSERKER           49*/
};

#define sign(x) ((x) > 0 ? 1 : ((x) < 0 ? -1 : 0))


bool is_armour(byte tval)
{
	switch (tval)
	{
		case TV_BOOTS:
		case TV_GLOVES:
		case TV_CROWN:
		case TV_HELM:
		case TV_SHIELD:
		case TV_CLOAK:
		case TV_SOFT_ARMOR:
		case TV_HARD_ARMOR:
		case TV_DRAG_ARMOR:
			return TRUE;
		default:
			return (FALSE);
	}
}
bool is_hand_weapon(byte tval)
{
	switch (tval)
	{
		case TV_DIGGING:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
			return TRUE;
		default:
			return (FALSE);
	}
}
bool is_weapon(byte tval)
{
	switch (tval)
	{
		case TV_DIGGING:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_BOW:
			return TRUE;
		default:
			return (FALSE);
	}
}
/*
 * Evaluate the artifact's overall power level.
 * Based on artifact_power(int a_idx) from randart.c
 * Provide k_idx for speed (if known).
 * k_idx = 0 means function calls lookup_kind().
 */
s32b art_power(const artifact_type *a_ptr, int k_idx)
{
	s32b p = 0;
	object_kind *k_ptr = 0;
	int immunities = 0;

	/* Lookup the item if not yet cached */
	if (!k_idx)
	{
		k_idx = lookup_kind(a_ptr->tval, a_ptr->sval);

		/* Paranoia */
		if (!k_idx)
			quit_fmt("Illegal artifact tval/sval value:  %d/%d!", a_ptr->tval, a_ptr->sval);
	}

	k_ptr = &k_info[k_idx];

	/* Evaluate certain abilities based on type of object. */
	switch (a_ptr->tval)
	{
		case TV_BOW:
		{
			int mult;

			/* Alex: begin with average damage of the best ammo */
			switch (a_ptr->sval)
			{
				case SV_SLING: p += 5; /* Mithril Shot: average of 2d4 */
				case SV_SHORT_BOW:
				case SV_LONG_BOW:
					p += 10; /* Seeker Arrow: average of 4d4 */
				case SV_LIGHT_XBOW:
				case SV_HEAVY_XBOW:
					p += 12; /* Seeker Bolt: average of 5d4 */
			}

			p += (a_ptr->to_d + sign(a_ptr->to_d)) / 2;
			mult = bow_multiplier(a_ptr->sval);
			if (a_ptr->flags1 & TR1_MIGHT)
			{
				if (a_ptr->pval > 3)
				{
					p += 20000;	/* inhibit */
					mult = 1;	/* don't overflow */
				}
				else
					mult += a_ptr->pval;
			}
			p *= mult;

			if (a_ptr->to_d > ART_W_1) p += (a_ptr->to_d - ART_W_1 + 1) / 2;/*Alex*/
			if (a_ptr->to_d > ART_W_2) p += (a_ptr->to_d - ART_W_2 + 1) / 2;/*Alex*/
			if (a_ptr->to_d > ART_W_3) p += 20000;/*Alex*/

			if (a_ptr->flags1 & TR1_SHOTS)
			{
				if (a_ptr->pval > 3)
					p += 20000;	/* inhibit */
				else if (a_ptr->pval > 0)
					/* Alex: was: p *= (2 * a_ptr->pval); */
					p *= (1 + a_ptr->pval);
			}

			p += (a_ptr->to_h + 3 * sign(a_ptr->to_h)) / 4;
			if (a_ptr->to_h > ART_W_1) p += (a_ptr->to_h - ART_W_1 + 1) / 2;/*Alex*/
			if (a_ptr->to_h > ART_W_2) p += (a_ptr->to_h - ART_W_2 + 1) / 2;/*Alex*/
			if (a_ptr->to_h > ART_W_3) p += 20000;/*Alex*/
			/* Alex - see later*/
			/* (a_ptr->weight < k_ptr->weight) p++; */
			break;
		}
		case TV_DIGGING:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		{
			/*p += (a_ptr->dd * a_ptr->ds + 1) / 2; - wrong*/
			/* Alex: this is right (according to average damage value) */
			p += a_ptr->dd * (a_ptr->ds + 1) / 2;
			if (a_ptr->flags1 & TR1_SLAY_EVIL) p = (p * 3) / 2;
			if (a_ptr->flags1 & TR1_KILL_DRAGON) p = (p * 3) / 2;
			if (a_ptr->flags1 & TR1_KILL_DEMON) p = (p * 3) / 2;
			if (a_ptr->flags1 & TR1_KILL_UNDEAD) p = (p * 3) / 2;
			if (a_ptr->flags1 & TR1_SLAY_ANIMAL) p = (p * 4) / 3;
			if (a_ptr->flags1 & TR1_SLAY_UNDEAD) p = (p * 4) / 3;
			if (a_ptr->flags1 & TR1_SLAY_DRAGON) p = (p * 4) / 3;
			if (a_ptr->flags1 & TR1_SLAY_DEMON) p = (p * 5) / 4;
			if (a_ptr->flags1 & TR1_SLAY_TROLL) p = (p * 6) / 5;/*Alex: was 5/4*/
			if (a_ptr->flags1 & TR1_SLAY_ORC) p = (p * 6) / 5;/*Alex: was 5/4*/
			if (a_ptr->flags1 & TR1_SLAY_GIANT) p = (p * 7) / 6;/*Alex: was 6/5*/

			if (a_ptr->flags1 & TR1_BRAND_ACID) p = p * 2;
			if (a_ptr->flags1 & TR1_BRAND_ELEC) p = (p * 3) / 2;
			if (a_ptr->flags1 & TR1_BRAND_FIRE) p = (p * 4) / 3;
			if (a_ptr->flags1 & TR1_BRAND_COLD) p = (p * 4) / 3;

			p += (a_ptr->to_d + 2 * sign(a_ptr->to_d)) / 3;
			if (a_ptr->to_d > ART_W_1) p += (a_ptr->to_d - ART_W_1 + 1) / 2;
			if (a_ptr->to_d > ART_W_2) p += (a_ptr->to_d - ART_W_2 + 1) / 2;/*Alex*/
			if (a_ptr->to_d > ART_W_3) p += 20000;/*Alex*/

			if (a_ptr->flags1 & TR1_BLOWS)
			{
				if (a_ptr->pval > 3)
					p += 20000;	/* inhibit */
				else if (a_ptr->pval > 0)
					p = (p * 6) / (4 - a_ptr->pval);
				else
					p += 20*a_ptr->pval;
			}

			if ((a_ptr->flags1 & TR1_TUNNEL) &&
			    (a_ptr->tval != TV_DIGGING))
				p += a_ptr->pval * 3;

			p += (a_ptr->to_h + 3 * sign(a_ptr->to_h)) / 4;
			if (a_ptr->to_h > ART_W_1) p += (a_ptr->to_h - ART_W_1 + 1) / 2;/*Alex*/
			if (a_ptr->to_h > ART_W_2) p += (a_ptr->to_h - ART_W_2 + 1) / 2;/*Alex*/
			if (a_ptr->to_h > ART_W_3) p += 20000;/*Alex*/

			/* Remember, weight is in 0.1 lb. units. */
			/* Alex: see later*/
			/* if (a_ptr->weight != k_ptr->weight)
				p += (k_ptr->weight - a_ptr->weight) / 20;*/

			break;
		}
		case TV_BOOTS:
		case TV_GLOVES:
		case TV_HELM:
		case TV_CROWN:
		case TV_SHIELD:
		case TV_CLOAK:
		case TV_SOFT_ARMOR:
		case TV_HARD_ARMOR:
		case TV_DRAG_ARMOR:
		{
			int delta_ac = a_ptr->ac - k_ptr->ac;
			/* Alex: see later */
			/* int delta_w = k_ptr->weight - a_ptr->weight; */
			p += (a_ptr->ac + 4 * sign(a_ptr->ac)) / 5;
			if (delta_ac>ART_MAX_EXTRA_AC)
				p+= 20000;
			else
				p += delta_ac;
			/* Alex: see later */
			/* if (delta_w)
				p += (delta_w) / 30; */
			if (a_ptr->to_a > ART_A_1) p += (a_ptr->to_a - ART_A_1 + 1) / 2;
			if (a_ptr->to_a > ART_A_2) p += (a_ptr->to_a - ART_A_2 + 1) / 2;
			if (a_ptr->to_a > ART_A_3) p += 20000;	/* inhibit */
			break;
		}
		/*Alex: it was p + for nothing
		case TV_LITE:
		{
			p += 10;
			break;
		}
		case TV_RING:
		case TV_AMULET:
		{
			p += 20;
			break;
		}*/
	}

	/*Alex*/
	if (!is_hand_weapon(a_ptr->tval))
	{
		if (a_ptr->flags1 & TR1_BLOWS)
		{
			if (a_ptr->pval > 3)		
				p += 20000;	/* inhibit */		
			else if (a_ptr->pval > 0)		
				p +=  10 + 10 * a_ptr->pval * a_ptr->pval;/*20 - 50 - 100*/
			else
				p += 30*a_ptr->pval;
		}
		p += (a_ptr->to_h + sign(a_ptr->to_h)) / 2;
	}

	if (!is_weapon(a_ptr->tval))
	{
		if (a_ptr->to_h > ART_1) p += (a_ptr->to_h - ART_1);
		if (a_ptr->to_h > ART_2) p += (a_ptr->to_h - ART_2);
		if (a_ptr->to_h > ART_3) p += 20000;
			p += (a_ptr->to_d + sign(a_ptr->to_d)) / 2;
		if (a_ptr->to_d > ART_1) p += (a_ptr->to_d - ART_1);
		if (a_ptr->to_d > ART_2) p += (a_ptr->to_d - ART_2);
		if (a_ptr->to_d > ART_3) p += 20000;
	}

	if (!is_armour(a_ptr->tval))
	{
		if (a_ptr->to_a > ART_1) p += (a_ptr->to_a - ART_1);
		if (a_ptr->to_a > ART_2) p += (a_ptr->to_a - ART_2);
		if (a_ptr->to_a > ART_3) p += 20000;	/* inhibit */
	}
	/* Other abilities are evaluated independent of the object type. */
	p += (a_ptr->to_a + 3 * sign(a_ptr->to_a)) / 4;
	if (a_ptr->weight != k_ptr->weight)
		p -= (a_ptr->weight - k_ptr->weight)/ART_WEIGHT_DIV;

	if (a_ptr->pval > 0)
	{
		if (a_ptr->flags1 & TR1_STR) p += a_ptr->pval * a_ptr->pval;
		if (a_ptr->flags1 & TR1_INT) p += a_ptr->pval * a_ptr->pval;
		if (a_ptr->flags1 & TR1_WIS) p += a_ptr->pval * a_ptr->pval;
		if (a_ptr->flags1 & TR1_DEX) p += a_ptr->pval * a_ptr->pval;
		if (a_ptr->flags1 & TR1_CON) p += a_ptr->pval * a_ptr->pval;
		if (a_ptr->flags1 & TR1_STEALTH) p += a_ptr->pval * a_ptr->pval;
	}
	else if (a_ptr->pval < 0)	/* hack: don't give large negatives */
	{
		if (a_ptr->flags1 & TR1_STR) p += a_ptr->pval;
		if (a_ptr->flags1 & TR1_INT) p += a_ptr->pval;
		if (a_ptr->flags1 & TR1_WIS) p += a_ptr->pval;
		if (a_ptr->flags1 & TR1_DEX) p += a_ptr->pval;
		if (a_ptr->flags1 & TR1_CON) p += a_ptr->pval;
		if (a_ptr->flags1 & TR1_STEALTH) p += a_ptr->pval;
	}
	if (a_ptr->flags1 & TR1_CHR) p += a_ptr->pval;
	if (a_ptr->flags1 & TR1_INFRA) p += (a_ptr->pval + sign(a_ptr->pval)) / 2;
	if (a_ptr->flags1 & TR1_SPEED) p += (a_ptr->pval * 3) / 2;
	if (a_ptr->flags1 & TR1_SEARCH) p += (a_ptr->pval + 4*sign(a_ptr->pval)) / 5;

	if (a_ptr->flags2 & TR2_SUST_STR) p += 6;
	if (a_ptr->flags2 & TR2_SUST_INT) p += 4;
	if (a_ptr->flags2 & TR2_SUST_WIS) p += 4;
	if (a_ptr->flags2 & TR2_SUST_DEX) p += 4;
	if (a_ptr->flags2 & TR2_SUST_CON) p += 4;
	if (a_ptr->flags2 & TR2_SUST_CHR) p += 1;
	if (a_ptr->flags2 & TR2_IM_ACID)
	{
		p += 20;
		immunities++;
	}
	if (a_ptr->flags2 & TR2_IM_ELEC)
	{
		p += 24;
		immunities++;
	}
	if (a_ptr->flags2 & TR2_IM_FIRE)
	{
		p += 36;
		immunities++;
	}
	if (a_ptr->flags2 & TR2_IM_COLD)
	{
		p += 24;
		immunities++;
	}
	if (immunities > 1) p += 16;
	if (immunities > 2) p += 16;
	if (immunities > 3) p += 20000; 	/* inhibit */
	if (a_ptr->flags3 & TR3_FREE_ACT) p += 8;
	if (a_ptr->flags3 & TR3_HOLD_LIFE) p += 10;
	if (a_ptr->flags2 & TR2_RES_FEAR) p += 1;/*Alex*/
	if (a_ptr->flags2 & TR2_RES_ACID) p += 6;
	if (a_ptr->flags2 & TR2_RES_ELEC) p += 6;
	if (a_ptr->flags2 & TR2_RES_FIRE) p += 6;
	if (a_ptr->flags2 & TR2_RES_COLD) p += 6;
	if (a_ptr->flags2 & TR2_RES_POIS) p += 12;
	if (a_ptr->flags2 & TR2_RES_LITE) p += 8;
	if (a_ptr->flags2 & TR2_RES_DARK) p += 10;
	if (a_ptr->flags2 & TR2_RES_BLIND) p += 10;
	if (a_ptr->flags2 & TR2_RES_CONFU) p += 8;
	if (a_ptr->flags2 & TR2_RES_SOUND) p += 10;
	if (a_ptr->flags2 & TR2_RES_SHARD) p += 8;
	if (a_ptr->flags2 & TR2_RES_NETHR) p += 12;
	if (a_ptr->flags2 & TR2_RES_NEXUS) p += 10;
	if (a_ptr->flags2 & TR2_RES_CHAOS) p += 12;
	if (a_ptr->flags2 & TR2_RES_DISEN) p += 12;

	if (a_ptr->flags3 & TR3_FEATHER) p += 2;
	if (a_ptr->flags3 & TR3_LITE) p += 2;
	if (a_ptr->flags3 & TR3_SEE_INVIS) p += 8;
	if (a_ptr->flags3 & TR3_TELEPATHY) p += 20;
	if (a_ptr->flags3 & TR3_SLOW_DIGEST) p += 4;
	if (a_ptr->flags3 & TR3_REGEN) p += 8;
	if (a_ptr->flags3 & TR3_TELEPORT) p -= 20;
	if (a_ptr->flags3 & TR3_DRAIN_EXP) p -= 16;
	if (a_ptr->flags3 & TR3_AGGRAVATE) p -= 8;
	if (a_ptr->flags3 & TR3_BLESSED) p += 4;
	if (a_ptr->flags3 & TR3_LIGHT_CURSE) p -= 4;
	if (a_ptr->flags3 & TR3_HEAVY_CURSE) p -= 20;
	if (a_ptr->flags3 & TR3_PERMA_CURSE) p -= 40;

	/* Alex: activation */
	if (a_ptr->flags3 & TR3_ACTIVATE)
	{
		enum Rarity r = act_rarity[a_ptr->activation];
		/* Not exactly, but near */
		u16b rarity_average_time = (u16b)(4*act_recharge_mul[r]);
		u16b this_average_time = (u16b)(a_ptr->time + (a_ptr->randtime + 1)/2);
		p += activation_power[r] * rarity_average_time / this_average_time;
	}
	
	return (p);
}

/* Alex: add_pval can increase pval<0 when cursed_art = FALSE */
bool cursed_art = FALSE;

/* Alex: string with new artifact property */
enum {LPROP = 200};
char new_property[LPROP];

/* Alex: string concatenation with range checking */
static void strcat_check(char* s, const char* t, int max_length){
	char* max_chr = &(s[max_length-1]);
	while (*s && s!= max_chr) s++;/* Now s points to '\0' or to last buffer symbol */
	while (*t && s != max_chr) *s++ = *t++;
	*s = '\0';
}

/* Last new ability */
enum Art_Abil last_abil = a_last;
/* Rarity of ability added by add_ability() */
enum Rarity last_rarity;
/* Flag for show plus */
bool show_plus = FALSE;
s16b plus;
/* Flag for show pval */
bool show_pval = FALSE;

/* Abilities that was disabled by player */
bool disabled[a_last];
/* Artifact abilities that can/cannot be disabled by player */
const bool can_disable[a_last] =
{
/* General abilities */
	/* Common */
	TRUE /*a_pval*/,	FALSE /*a_comm_act*/,
	TRUE,	TRUE,	TRUE,	TRUE,	TRUE,	TRUE,	TRUE,
	TRUE,	TRUE,	TRUE,	TRUE,	TRUE,	TRUE,
	TRUE,	TRUE,	TRUE,	TRUE,
	FALSE,	FALSE,	FALSE,
	/*a_drain_exp,	a_teleport,	a_aggr,*/
	/* Uncommon */
	FALSE /*a_uncomm_act*/,
	TRUE,	TRUE,	TRUE,
	TRUE,	TRUE,	TRUE,	FALSE /*a_heavy_weight*/,	TRUE,
	TRUE,	TRUE,	TRUE,	TRUE,	TRUE,	TRUE,
	TRUE,	TRUE,	TRUE,	TRUE,
	TRUE,
	/* Rare */
	FALSE /*a_rare_act*/,
	TRUE,	TRUE,	TRUE,
	TRUE,	TRUE,	TRUE,	TRUE,	TRUE,
	TRUE,
	/* V. Rare */
	FALSE /*a_vrare_act*/,
	TRUE,	TRUE,	TRUE,	TRUE,
	TRUE,	TRUE,
	TRUE,	TRUE,
	FALSE /*a_to_h_to_d*/,
	TRUE,
/* Specific abilities: weapon */
	/* Common */
	FALSE,	FALSE,	FALSE,
	/*a_to_h,	a_to_d, a_to_a,*/
	TRUE,	TRUE,	TRUE,
	TRUE,
	/* Uncommon */
	TRUE,
	TRUE,	TRUE,	TRUE,	TRUE,	TRUE,
	TRUE,	TRUE,	TRUE,	TRUE,	TRUE,
	/* Rare */
	TRUE,	TRUE,	TRUE,
	TRUE,	TRUE,	TRUE,
	/* V. Rare */
	TRUE,
	TRUE,
	TRUE,
/* Specific abilities: heavy armour with (-x) */
	/* Uncommon */
	TRUE
};

/* Names of artifact abilities */
/* NULL means name set in add_xxx function */
cptr abil_name[a_last] =
{
/* General abilities */
	/* Common */
	"Plus", /* a_pval */	NULL /*a_comm_act*/,
	"Searching",	"Resist Fear",	"Infravision",	"Feather Falling",	"Lite", "See Invisible",	"Slow Digestion",
	"Sust Str",	"Sust Int",	"Sust Wis",	"Sust Dex",	"Sust Con",	"Sust Chr",
	"Res Acid",	"Res Electricity",	"Res Fire",	"Res Cold",
	"Drain Exp",	"Random Teleportation", "Aggravation",
	/* Uncommon */
	NULL /*a_uncomm_act*/,
	"Remove Drain Exp",	"Remove Random Teleportation",	"Remove Aggravation",
	"Blessed",	"Stealth",	"Low Weight", "Heavy Weight",	"Regeneration",
	"Str",	"Int",	"Wis",	"Dex",	"Con",	"Chr",
	"Res Lite",	"Res Dark",	"Res Blind",	"Res Pois",
	"Sust All",
	/* Rare */
	NULL /*a_rare_act*/,
	"Speed",	"Free Action",	"Hold Life",
	"Res Confusion",	"Res Sound",	"Res Shards",	"Res Nexus",	"Res Chaos",
	"Resistance (incl. Res Poison)",
	/* V. Rare */
	NULL /*a_vrare_act*/,
	"Immune Acid",	"Immune Electricity",	"Immune Fire",	"Immune Cold",
	"Res Nether",	"Res Disenchantment",
	"Telepathy",	"Blows",
	"To Hit, To Dam",
	"All Stats",
/* Specific abilities: weapon */
	/* Common */
	"To Hit",	"To Dam",	"To AC",
	"Slay Orcs",	"Slay Trolls",	"Slay Giants",
	"Tunnelling",
	/* Uncommon */
	"Might",
	"Acid Brand", "Lightning Brand",	"Fire Brand", "Cold Brand",	"Poison Brand",
	"Slay Dragons", "Slay Evil",	"Slay Animals", "Slay Undeads", "Slay Demons",
	/* Rare */
	"Shots",	"Damage Dices",	"Base AC",
	"Kill Dragons", "Kill Undeads", "Kill Demons",
	/* V. Rare */
	"Might and Shots",
	"Earthquake",
	"Kill Dragons, Undeads, and Demons",
/* Specific abilities: heavy armour with (-x) */
	/* Uncommon */
	"Less to hit pen"
};
struct SimpleArtAbil
{
	/* 0 - ability not simple,
	 * 1,2,3 - flags1,2,3
	 * -1,-2,-3 - flags1,2,3 and check for pval; if pval is zero, set it to 1
	 */
	s16b f;
	u32b mask;
};
/* Artifact abilities */
const struct SimpleArtAbil simple_abil[a_last]=
{
/* General abilities */
	/* Common */
	{0,0},/* a_pval */	{0,0} /*a_comm_act*/,
	{-1,TR1_SEARCH},{2,TR2_RES_FEAR},{-1,TR1_INFRA},{3,TR3_FEATHER},{3,TR3_LITE},{3,TR3_SEE_INVIS},{3,TR3_SLOW_DIGEST},
	{2,TR2_SUST_STR},{2,TR2_SUST_INT},{2,TR2_SUST_WIS},{2,TR2_SUST_DEX},{2,TR2_SUST_CON},{2,TR2_SUST_CHR},
	{2,TR2_RES_ACID},{2,TR2_RES_ELEC},{2,TR2_RES_FIRE},{2,TR2_RES_COLD},
	{3,TR3_DRAIN_EXP},{3,TR3_TELEPORT},{3,TR3_AGGRAVATE},
	/* Uncommon */
	{0,0} /*a_uncomm_act*/,
	{0,0}/*a_remove_drain_exp*/,{0,0}/*a_remove_teleport*/,{0,0}/*a_remove_aggr*/,
	{3,TR3_BLESSED},{-1,TR1_STEALTH},{0,0}/*a_low_weight*/,{0,0}/*a_heavy_weight*/,{3,TR3_REGEN},
	{-1,TR1_STR},{-1,TR1_INT},{-1,TR1_WIS},{-1,TR1_DEX},{-1,TR1_CON},{-1,TR1_CHR},
	{2,TR2_RES_LITE},{2,TR2_RES_DARK},{2,TR2_RES_BLIND},{2,TR2_RES_POIS},
	{2,TR2_SUST_STR | TR2_SUST_INT | TR2_SUST_WIS | TR2_SUST_DEX | TR2_SUST_CON | TR2_SUST_CHR}/*a_sust_all*/,
	/* Rare */
	{0,0} /*a_rare_act*/,
	{-1,TR1_SPEED},{3,TR3_FREE_ACT},{3,TR3_HOLD_LIFE},
	{2,TR2_RES_CONFU},{2,TR2_RES_SOUND},{2,TR2_RES_SHARD},{2,TR2_RES_NEXUS},{2,TR2_RES_CHAOS},
	{2,TR2_RES_ACID | TR2_RES_ELEC | TR2_RES_FIRE | TR2_RES_COLD | TR2_RES_POIS}/*a_resistance*/,
	/* V. Rare */
	{0,0} /*a_vrare_act*/,
	{2,TR2_IM_ACID},{2,TR2_IM_ELEC},{2,TR2_IM_FIRE},{2,TR2_IM_COLD},
	{2,TR2_RES_NETHR},{2,TR2_RES_DISEN},
	{3,TR3_TELEPATHY},{-1,TR1_BLOWS},
	{0,0}/*a_to_h_to_d*/,
	{-1,TR1_STR | TR1_INT | TR1_WIS | TR1_DEX | TR1_CON | TR1_CHR}/*a_all_stats*/,
/* Specific abilities: weapon */
	/* Common */
	{0,0},{0,0},{0,0},
	/*a_to_h,	a_to_d, a_to_a,*/
	{1,TR1_SLAY_ORC},{1,TR1_SLAY_TROLL},{1,TR1_SLAY_GIANT},
	{-1,TR1_TUNNEL},
	/* Uncommon */
	{-1,TR1_MIGHT},
	{1,TR1_BRAND_ACID},{1,TR1_BRAND_ELEC},{1,TR1_BRAND_FIRE},{1,TR1_BRAND_COLD},{1,TR1_BRAND_POIS},
	{1,TR1_SLAY_DRAGON},{1,TR1_SLAY_EVIL},{1,TR1_SLAY_ANIMAL},{1,TR1_SLAY_UNDEAD},{1,TR1_SLAY_DEMON},
	/* Rare */
	{-1,TR1_SHOTS},{0,0}/*a_dd*/,{0,0}/*a_ac*/,
	{1,TR1_KILL_DRAGON},{1,TR1_KILL_UNDEAD},{1,TR1_KILL_DEMON},
	/* V. Rare */
	{-1,TR1_MIGHT | TR1_SHOTS},
	{3,TR3_IMPACT},
	{1,TR1_KILL_DRAGON | TR1_KILL_UNDEAD | TR1_KILL_DEMON},
/* Specific abilities: heavy armour with (-x) */
	/* Uncommon */
	{0,0}/* a_remove_to_h_pen */
};
/* General abilities */
	/* Common */
/*
 * Alex: based on do_pval(artifact_type *a_ptr) from randart.c
 */
static bool add_pval(artifact_type *a_ptr)
{
	last_abil = a_pval;
	last_rarity = r_common;
	show_plus = TRUE;
	plus = 0;

	if (!a_ptr->pval)
		plus = (s16b)(1 + rand_int(3));
	else if (a_ptr->pval < 0 && cursed_art && !rand_int(2))
		plus = -1;
	else if (!rand_int(3))
	{
		if (a_ptr->pval != -1)
			plus = 1;
		else
			/* Alex: make sure it's not zero. */
			plus = 2;
	}
	a_ptr->pval += plus;
	return (plus != 0);
}
static bool remove_drain_exp(artifact_type *a_ptr)
{
	last_abil = a_remove_drain_exp;
	if (!(a_ptr->flags3 & TR3_DRAIN_EXP)) return FALSE;
	a_ptr->flags3 &= ~(TR3_DRAIN_EXP);
	return TRUE;
}
static bool remove_teleport(artifact_type *a_ptr)
{
	last_abil = a_remove_teleport;
	if (!(a_ptr->flags3 & TR3_TELEPORT)) return FALSE;
	a_ptr->flags3 &= ~(TR3_TELEPORT);
	return TRUE;
}
static bool remove_aggr(artifact_type *a_ptr)
{
	last_abil = a_remove_aggr;
	if (!(a_ptr->flags3 & TR3_AGGRAVATE)) return FALSE;
	a_ptr->flags3 &= ~(TR3_AGGRAVATE);
	return TRUE;
}
static void weight_aux(const artifact_type *a_ptr)
{
	char buf[80];
	char *t = buf;

	strcat_check(new_property, " (", LPROP);
	object_desc_s_int_macro(t, a_ptr->weight/10);
	object_desc_chr_macro(t, '.');
	object_desc_s_int_macro(t, a_ptr->weight%10);
	object_desc_chr_macro(t, ')');
	*t = '\0';
	strcat_check(new_property, buf, LPROP);
}
static bool add_heavy_weight(artifact_type *a_ptr)
{
	int mul = 11 + rand_int(7);
	last_abil = a_heavy_weight;
	a_ptr->weight = (a_ptr->weight * mul) / 10;
	weight_aux(a_ptr);
	return TRUE;
}
	/* Uncommon */
static bool add_low_weight(artifact_type *a_ptr)
{
	int mul = 9 - rand_int(7);
	last_abil = a_low_weight;
	a_ptr->weight = (a_ptr->weight * mul) / 10;
	weight_aux(a_ptr);
return TRUE;
}

static bool add_to_h_to_d(artifact_type *a_ptr)
{
	last_abil = a_to_h_to_d;
	show_plus = TRUE;
	a_ptr->flags3 |= TR3_SHOW_MODS;
	if (r_vrare == last_rarity)
		plus = (s16b)(2 + rand_int(2));
	else
		plus = 1;

	a_ptr->to_d += plus;
	a_ptr->to_h += plus;
	return TRUE;
}
static bool add_to_h(artifact_type *a_ptr)
{
	last_abil = a_to_h;
	show_plus = TRUE;
	a_ptr->flags3 |= TR3_SHOW_MODS;
	switch (last_rarity)
	{
		case r_common: plus = (s16b)(1 + rand_int(2)); break;
		case r_uncommon: plus = (s16b)(2 + rand_int(3)); break;
		case r_rare: plus = 5; break;
		case r_vrare: plus = 10; break;
	}
	a_ptr->to_h += plus;
	return TRUE;
}
static bool add_to_d(artifact_type *a_ptr)
{
	last_abil = a_to_d;
	show_plus = TRUE;
	a_ptr->flags3 |= TR3_SHOW_MODS;
	switch (last_rarity)
	{
		case r_common: plus = (s16b)(1 + rand_int(2)); break;
		case r_uncommon: plus = (s16b)(2 + rand_int(3)); break;
		case r_rare: plus = 5; break;
		case r_vrare: plus = 10; break;
	}
	a_ptr->to_d += plus;
	return TRUE;
}
static bool add_to_a(artifact_type *a_ptr)
{
	last_abil = a_to_a;
	show_plus = TRUE;
	switch (last_rarity)
	{
		case r_common: plus = (s16b)(1 + rand_int(2)); break;
		case r_uncommon: plus = (s16b)(2 + rand_int(3)); break;
		case r_rare: plus = 5; break;
		case r_vrare: plus = 10; break;
	}
	a_ptr->to_a += plus;
	return TRUE;
}
static bool add_dd(artifact_type *a_ptr)
{
	byte old_dd = a_ptr->dd;
	last_abil = a_dd;
	show_plus = TRUE;
	if (r_vrare == last_rarity) plus = (s16b)(2 + rand_int(2));
	else plus = 1;
	a_ptr->dd += (byte)plus;
	if (a_ptr->dd > 9) a_ptr->dd = 9;
	plus = a_ptr->dd - old_dd;
	return (plus != 0);
}
static bool add_ac(artifact_type *a_ptr)
{
	last_abil = a_ac;
	show_plus = TRUE;
	if (r_vrare == last_rarity) plus = (s16b)(2 + rand_int(2));
	else plus = 1;
	a_ptr->ac += plus;
	return TRUE;
}
static bool remove_to_h_pen(artifact_type *a_ptr)
{
	int old = a_ptr->to_h;
	last_abil = a_remove_to_h_pen;
	show_plus = TRUE;
	if (a_ptr->to_h<0) a_ptr->to_h++;
	while(rand_int(2) && a_ptr->to_h<0) a_ptr->to_h++;
	plus = a_ptr->to_h - old;
	return (plus != 0);
}

/*
 * Returns activation description in descr.
 * If time == 0 and randtime == 0, no time information included.
 */
static bool describe_act(byte act, u16b time, u16b randtime, char* descr, int max_length)
{
	int i;
	int a_idx = 0;

	/*Try to find artifact with the given activation*/
	for (i = 1; i < z_info->a_max; i++)
		if (a_info[i].activation == act)
		{
			a_idx = i;
			break;
		}

	if (a_idx)
	{
		/* This code is complicated, but 
		 * I do not know the other way to get description of activation. */
		cptr act_desc = NULL;
		object_type obj;
		artifact_type* _a = &a_info[a_idx];
		u16b original_time = _a->time;
		u16b original_randtime = _a->randtime;
		bool ok = FALSE;

		obj.name1 = a_idx;
		if (time || randtime)
		{
			_a->time = time;
			_a->randtime = randtime;
		}
		else
		{
			_a->time = 1;
			_a->randtime = 0;
		}
		ok = call_hook("describe_item_activation", "(O)", "s", &obj, &act_desc);
		_a->time = original_time;
		_a->randtime = original_randtime;
		if (ok)
		{
			if (time || randtime)
				strcat_check(descr, act_desc, max_length);
			else
			{
				/* find the word " every" */
				char* every = strstr(act_desc, " every");
				*every = '\0';
				strcat_check(descr, act_desc, max_length);
				*every = ' ';
			}
		}
		return TRUE;
	}
	else
		/* No description */
		return FALSE;
}

static bool add_activation(artifact_type *a_ptr, enum Rarity rarity)
{
	int i;
	int act = -1;
	/* Alex: it is impossible to overflow 200-char array by activation */
	char buf[200];
	char *t = buf;
	u16b time_base;

	if (a_ptr->flags3 & TR3_ACTIVATE)
		return FALSE;/* Already has acivation */

	for (i = 0; i < 100*ACT_MAX; i++)
	{
		int act_try = rand_int(ACT_MAX);/* One simple if() works bad. I don't know why. */
		if (act_rarity[act_try] == rarity)
		{
			act = act_try;
			break;
		}
		else if (act_rarity[ACT_MAX - 1 - act_try] == rarity)
		{
			act = ACT_MAX - 1 - act_try;
			break;
		}

		act_try = act_try + ACT_MAX/2;
		if (act_try >= ACT_MAX) act_try -= ACT_MAX;

		if (act_rarity[act_try] == rarity)
		{
			act = act_try;
			break;
		}
		else if (act_rarity[ACT_MAX - 1 - act_try] == rarity)
		{
			act = ACT_MAX - 1 - act_try;
			break;
		}
	}
	if (act < 0)
		return FALSE;

	a_ptr->flags3 |= TR3_ACTIVATE;
	a_ptr->activation = (byte)(act);
	time_base = (u16b)(1 + rand_int(5));
	if (rand_int(2))
		/* 50% time goes up */
		while (!rand_int(ART_ACT_TIME_CHANGE_CHANCE)) {time_base *= 3; time_base /= 2;}
	else
		/* 50% time goes down */
		while (!rand_int(ART_ACT_TIME_CHANGE_CHANCE) && time_base > 1) {time_base /= 2;}

	a_ptr->time = time_base * act_recharge_mul[rarity];
	a_ptr->randtime = rand_int(time_base) * act_recharge_mul[rarity];

	if (describe_act(a_ptr->activation, a_ptr->time, a_ptr->randtime, new_property, LPROP))
		return TRUE;

	object_desc_str_macro(t, "Activation number ");
	object_desc_num_macro(t, a_ptr->activation);
	*t = '\0';
	strcat_check(new_property, buf, LPROP);
	return TRUE;
}
static bool add_common_act(artifact_type *a_ptr)
{
	last_abil = a_comm_act;
	return add_activation(a_ptr, r_common);
}
static bool add_uncommon_act(artifact_type *a_ptr)
{
	last_abil = a_uncomm_act;
	return add_activation(a_ptr, r_uncommon);
}
static bool add_rare_act(artifact_type *a_ptr)
{
	last_abil = a_rare_act;
	return add_activation(a_ptr, r_rare);
}
static bool add_vrare_act(artifact_type *a_ptr)
{
	last_abil = a_vrare_act;
	return add_activation(a_ptr, r_vrare);
}

/* Functions that add abilities to the Artifact */
typedef bool (*add_abil_t)(artifact_type *a_ptr);
const add_abil_t _add_abil[a_last] =
{
/* General abilities */
	/* Common */
	add_pval,	add_common_act,
	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,
	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,
	NULL,	NULL,	NULL,	NULL,
	NULL,	NULL,	NULL,
	/* Uncommon */
	add_uncommon_act,
	remove_drain_exp,	remove_teleport,	remove_aggr,
	NULL,	NULL,	add_low_weight, add_heavy_weight, NULL,
	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,
	NULL,	NULL,	NULL,	NULL,
	NULL,
	/* Rare */
	add_rare_act,
	NULL,	NULL,	NULL,
	NULL,	NULL,	NULL,	NULL,	NULL,
	NULL,
	/* V. Rare */
	add_vrare_act,
	NULL,	NULL,	NULL,	NULL,
	NULL,	NULL,
	NULL,	NULL,
	add_to_h_to_d,
	NULL,
/* Specific abilities: weapon */
	/* Common */
	add_to_h,	add_to_d,	add_to_a,
	NULL,	NULL,	NULL,
	NULL,
	/* Uncommon */
	NULL,
	NULL,	NULL,	NULL,	NULL,	NULL,
	NULL,	NULL,	NULL,	NULL,	NULL,
	/* Rare */
	NULL,	add_dd, add_ac,
	NULL,	NULL,	NULL,
	/* V. Rare */
	NULL,
	NULL,
	NULL,
/* Specific abilities: heavy armour with (-x) */
	/* Uncommon */
	remove_to_h_pen
};

/* Return TRUE if actually adds ability */
static bool add_abil_aux(artifact_type *a_ptr, enum Art_Abil abil)
{
	add_abil_t _abil_func = _add_abil[abil];
	cptr _abil_name = abil_name[abil];
	last_abil = abil;

	if (_abil_name)
		strcat_check(new_property, _abil_name, LPROP);

	if (_abil_func)
	{
		/* _abil_func() could add some to new_property - see e.g. add_low_weight() */
		bool ok = _abil_func(a_ptr);
		if (!ok)
			strcat_check(new_property, "", LPROP);
		return ok;
	}
	else
	{
		u32b* flags = 0;
		struct SimpleArtAbil aa = simple_abil[abil];

		if (aa.f < 0)
			show_pval = TRUE;

		switch (aa.f)
		{
		case -1:
		case 1: flags = &a_ptr->flags1; break;
		case -2:
		case 2: flags = &a_ptr->flags2;break;
		case -3:
		case 3: flags = &a_ptr->flags3;break;
		default:
			{
				if (_abil_name)
					msg_format("Error: Ability %s is not simple.", _abil_name);
				else
					msg_format("Error: Ability %d is not simple.", abil);
				return FALSE;
			}
		}
		if (((*flags) & aa.mask) == aa.mask) return FALSE;/* Artifact already has this ability */
		*flags |= aa.mask;/* Add */
		if (aa.f < 0 && !a_ptr->pval)
			a_ptr->pval = 1;
		return TRUE;
	}
}

static bool add_common_ability(artifact_type *a_ptr)
{
	int r = rand_int(20);
	if (!rand_int(ART_ACT_CHANCE)) return add_common_act(a_ptr);
	switch (r)
		{
			case 0: return add_abil_aux(a_ptr, a_search);
			case 1: return add_abil_aux(a_ptr, a_infra);

			case 2: return add_abil_aux(a_ptr, a_sust_str);
			case 3: return add_abil_aux(a_ptr, a_sust_int);
			case 4: return add_abil_aux(a_ptr, a_sust_wis);
			case 5: return add_abil_aux(a_ptr, a_sust_dex);
			case 6: return add_abil_aux(a_ptr, a_sust_con);
			case 7: return add_abil_aux(a_ptr, a_sust_chr);

			case 8: return add_abil_aux(a_ptr, a_res_acid);
			case 9: return add_abil_aux(a_ptr, a_res_elec);
			case 10: return add_abil_aux(a_ptr, a_res_fire);
			case 11: return add_abil_aux(a_ptr, a_res_cold);

			case 12: return add_abil_aux(a_ptr, a_feather);
			case 13: return add_abil_aux(a_ptr, a_lite);
			case 14: return add_abil_aux(a_ptr, a_see_invis);
			case 15: return add_abil_aux(a_ptr, a_slow_digest);
			case 16: return add_abil_aux(a_ptr, a_res_fear);

			case 17: if (!rand_int(150)) return add_abil_aux(a_ptr, a_drain_exp); else return FALSE;
			case 18: if (!rand_int(200)) return add_abil_aux(a_ptr, a_teleport); else return FALSE;
			case 19: if (!rand_int(100)) return add_abil_aux(a_ptr, a_aggr); else return FALSE;
		}/*switch (r)*/
	return FALSE;
}

static bool add_uncommon_ability(artifact_type *a_ptr)
{
	int r = rand_int(18);
	if (!rand_int(ART_ACT_CHANCE)) return add_uncommon_act(a_ptr);
		switch (r)
		{
			case 0: return add_abil_aux(a_ptr, a_str);
			case 1: return add_abil_aux(a_ptr, a_int);
			case 2: return add_abil_aux(a_ptr, a_wis);
			case 3: return add_abil_aux(a_ptr, a_dex);
			case 4: return add_abil_aux(a_ptr, a_con);
			case 5: return add_abil_aux(a_ptr, a_chr);

			case 6: return add_abil_aux(a_ptr, a_stealth);
			case 7: return add_abil_aux(a_ptr, a_res_pois);
			case 8: return add_abil_aux(a_ptr, a_res_lite);
			case 9: return add_abil_aux(a_ptr, a_res_dark);
			case 10: return add_abil_aux(a_ptr, a_res_blind);
			case 11: return add_abil_aux(a_ptr, a_regen);
			case 12: return add_abil_aux(a_ptr, a_sust_all);
			case 13: return add_abil_aux(a_ptr, a_low_weight);
			case 14: return add_abil_aux(a_ptr, a_heavy_weight);

			case 15: return add_abil_aux(a_ptr, a_remove_drain_exp);
			case 16: return add_abil_aux(a_ptr, a_remove_teleport);
			case 17: return add_abil_aux(a_ptr, a_remove_aggr);
		}/*switch (r)*/
	return FALSE;
}

static bool add_rare_ability(artifact_type *a_ptr)
{
	int r = rand_int(9);
	if (!rand_int(ART_ACT_CHANCE)) return add_rare_act(a_ptr);
		switch (r)
		{
			case 0: return add_abil_aux(a_ptr, a_speed);
			case 1: return add_abil_aux(a_ptr, a_free_act);
			case 2: return add_abil_aux(a_ptr, a_hold_life);
			case 3: return add_abil_aux(a_ptr, a_res_conf);
			case 4: return add_abil_aux(a_ptr, a_res_sound);
			case 5: return add_abil_aux(a_ptr, a_res_shard);
			case 6: return add_abil_aux(a_ptr, a_res_nexus);
			case 7: return add_abil_aux(a_ptr, a_res_chaos);
			case 8: return add_abil_aux(a_ptr, a_resistance);
		}/*switch (r)*/
	return FALSE;
}

static bool add_very_rare_ability(artifact_type *a_ptr)
{
	int r = rand_int(10);
	if (!rand_int(ART_ACT_CHANCE)) return add_vrare_act(a_ptr);
		switch (r)
		{
			case 0: return add_abil_aux(a_ptr, a_im_acid);
			case 1: return add_abil_aux(a_ptr, a_im_elec);
			case 2: return add_abil_aux(a_ptr, a_im_fire);
			case 3: return add_abil_aux(a_ptr, a_im_cold);
			case 4: return add_abil_aux(a_ptr, a_res_nether);
			case 5: return add_abil_aux(a_ptr, a_res_disen);
			case 6: return add_abil_aux(a_ptr, a_telepathy);
			case 7: if (!rand_int(3)) return add_abil_aux(a_ptr, a_blow); else return FALSE;
			case 8: if (!rand_int(3)) return add_abil_aux(a_ptr, a_to_h_to_d); else return FALSE;
			case 9: return add_abil_aux(a_ptr, a_all_stats);
		}/*switch (r)*/
	return FALSE;
}

static bool add_common_specific(artifact_type *a_ptr)
{
		int r = rand_int(100);
		switch (a_ptr->tval)
		{
			case TV_BOW:
			{
				if (r < 45) return add_abil_aux(a_ptr, a_to_h);
				else if (r<90) return add_abil_aux(a_ptr, a_to_d);
				else return add_abil_aux(a_ptr, a_low_weight);
			}
			case TV_DIGGING:
			case TV_HAFTED:
			case TV_POLEARM:
			case TV_SWORD:
			{
				if (r < 6) return add_abil_aux(a_ptr, a_blessed);
				else if (r < 19) return add_abil_aux(a_ptr, a_slay_orc);
				else if (r < 32) return add_abil_aux(a_ptr, a_slay_troll);
				else if (r < 45) return add_abil_aux(a_ptr, a_slay_giant);
				else if (r < 60) return add_abil_aux(a_ptr, a_to_h);
				else if (r < 80) return add_abil_aux(a_ptr, a_to_d);
				else if (r < 90) return add_abil_aux(a_ptr, a_low_weight);
				else if (r < 92) return add_abil_aux(a_ptr, a_heavy_weight);
				else return add_abil_aux(a_ptr, a_tunnel);
			}
			case TV_BOOTS:
			{
				if (r < 35) return add_abil_aux(a_ptr, a_feather);
				else if (r < 85) return add_abil_aux(a_ptr, a_to_a);
				else return add_abil_aux(a_ptr, a_low_weight);
			}
			case TV_GLOVES:
			{
				if (r<80) return add_abil_aux(a_ptr, a_to_a);
				else return add_abil_aux(a_ptr, a_low_weight);
			}
			case TV_HELM:
			case TV_CROWN:
			{
				if (r < 20) return add_abil_aux(a_ptr, a_res_blind);
				else if (r < 50) return add_abil_aux(a_ptr, a_see_invis);
				else if (r<90) return add_abil_aux(a_ptr, a_to_a);
				else return add_abil_aux(a_ptr, a_low_weight);
			}
			case TV_SHIELD:
			{
				if (r < 15) return add_abil_aux(a_ptr, a_res_acid);
				else if (r < 30) return add_abil_aux(a_ptr, a_res_elec);
				else if (r < 45) return add_abil_aux(a_ptr, a_res_fire);
				else if (r < 60) return add_abil_aux(a_ptr, a_res_cold);
				else if (r<90) return add_abil_aux(a_ptr, a_to_a);
				else return add_abil_aux(a_ptr, a_low_weight);
			}
			case TV_CLOAK:
			{
				if (r<90) return add_abil_aux(a_ptr, a_to_a);
				else return add_abil_aux(a_ptr, a_low_weight);
			}
			case TV_SOFT_ARMOR:
			case TV_HARD_ARMOR:
			case TV_DRAG_ARMOR:
			{
				if (r < 15) return add_abil_aux(a_ptr, a_res_acid);
				else if (r < 30) return add_abil_aux(a_ptr, a_res_elec);
				else if (r < 45) return add_abil_aux(a_ptr, a_res_fire);
				else if (r < 60) return add_abil_aux(a_ptr, a_res_cold);
				else if (r < 70) return add_abil_aux(a_ptr, a_low_weight);
				else return add_abil_aux(a_ptr, a_to_a);
			}
			case TV_RING:
			case TV_AMULET:
			{
				if (r < 10) return add_abil_aux(a_ptr, a_res_acid);
				else if (r < 20) return add_abil_aux(a_ptr, a_res_elec);
				else if (r < 30) return add_abil_aux(a_ptr, a_res_fire);
				else if (r < 40) return add_abil_aux(a_ptr, a_res_cold);
				else if (r < 50) return add_abil_aux(a_ptr, a_sust_str);
				else if (r < 60) return add_abil_aux(a_ptr, a_sust_int);
				else if (r < 70) return add_abil_aux(a_ptr, a_sust_wis);
				else if (r < 80) return add_abil_aux(a_ptr, a_sust_dex);
				else if (r < 90) return add_abil_aux(a_ptr, a_sust_con);
				else return add_abil_aux(a_ptr, a_sust_chr);
			}
			case TV_LITE:
			{
				if (r<40) return add_abil_aux(a_ptr, a_lite);
				else if (r<70) return add_abil_aux(a_ptr, a_res_lite);
				else return add_abil_aux(a_ptr, a_res_dark);
			}
		}/*switch (a_ptr->tval)*/
	return FALSE;
}
static bool add_uncommon_specific(artifact_type *a_ptr)
{
		int r = rand_int(100);
		switch (a_ptr->tval)
		{
			case TV_BOW:
			{
				return add_abil_aux(a_ptr, a_might);
			}
			case TV_DIGGING:
			case TV_HAFTED:
			case TV_POLEARM:
			case TV_SWORD:
			{
				if (r < 5) return add_abil_aux(a_ptr, a_str);
				else if (r < 10) return add_abil_aux(a_ptr, a_dex);
				else if (r < 15) return add_abil_aux(a_ptr, a_acid);
				else if (r < 20) return add_abil_aux(a_ptr, a_elec);
				else if (r < 30) return add_abil_aux(a_ptr, a_fire);
				else if (r < 40) return add_abil_aux(a_ptr, a_cold);
				else if (r < 48) return add_abil_aux(a_ptr, a_pois);
				else if (r < 56) return add_abil_aux(a_ptr, a_slay_dragon);
				else if (r < 66) return add_abil_aux(a_ptr, a_slay_evil);
				else if (r < 72) return add_abil_aux(a_ptr, a_slay_animal);
				else if (r < 82) return add_abil_aux(a_ptr, a_slay_undead);
				else if (r < 87) return add_abil_aux(a_ptr, a_slay_demon);
				else if (r < 92) return add_abil_aux(a_ptr, a_see_invis);
				else return add_abil_aux(a_ptr, a_to_a);
			}
			case TV_BOOTS:
			{
				return add_abil_aux(a_ptr, a_stealth);
			}
			case TV_GLOVES:
			{
				if (r < 25) return add_abil_aux(a_ptr, a_free_act);
				else if (r < 50) return add_abil_aux(a_ptr, a_dex);
				else return add_abil_aux(a_ptr, a_to_h_to_d);
			}
			case TV_HELM:
			case TV_CROWN:
			{
				if (r < 40) return add_abil_aux(a_ptr, a_wis);
				else if (r < 80) return add_abil_aux(a_ptr, a_int);
				else return add_abil_aux(a_ptr, a_chr);
			}
			case TV_SHIELD:
			{
				return add_abil_aux(a_ptr, a_to_a);
			}
			case TV_CLOAK:
			{
				return add_abil_aux(a_ptr, a_stealth);
			}
			case TV_SOFT_ARMOR:
			case TV_HARD_ARMOR:
			case TV_DRAG_ARMOR:
			{
				if (r < 40) return add_abil_aux(a_ptr, a_stealth);
				else return add_abil_aux(a_ptr, a_con);
			}
			case TV_RING:
			case TV_AMULET:
			case TV_LITE:
			{
				if (r < 60) return add_abil_aux(a_ptr, a_to_a);
				else return add_abil_aux(a_ptr, a_to_h_to_d);
			}
		}/*switch (a_ptr->tval)*/
	return FALSE;
}
static bool add_rare_specific(artifact_type *a_ptr)
{
		int r = rand_int(100);
		switch (a_ptr->tval)
		{
			case TV_BOW:
			{
				return add_abil_aux(a_ptr, a_shots);
			}
			case TV_DIGGING:
			case TV_HAFTED:
			case TV_POLEARM:
			case TV_SWORD:
			{
				if (r < 30) return add_abil_aux(a_ptr, a_dd);
				else if (r < 55) return add_abil_aux(a_ptr, a_kill_dragon);
				else if (r < 80) return add_abil_aux(a_ptr, a_kill_undead);
				else return add_abil_aux(a_ptr, a_kill_demon);
			}
			case TV_BOOTS:
			{
				if (r<10) return add_abil_aux(a_ptr, a_ac);
				else return add_abil_aux(a_ptr, a_speed);
			}
			case TV_GLOVES:
			{
				if (r<10) return add_abil_aux(a_ptr, a_ac);
				else if (r<70) return add_abil_aux(a_ptr, a_to_a);
				else return add_abil_aux(a_ptr, a_to_h_to_d);
			}
			case TV_HELM:
			case TV_CROWN:
			{
				if (r<10) return add_abil_aux(a_ptr, a_ac);
				else if (r < 50) return add_abil_aux(a_ptr, a_telepathy);
				else return add_abil_aux(a_ptr, a_to_a);
			}
			case TV_SHIELD:
			case TV_CLOAK:
			case TV_SOFT_ARMOR:
			case TV_HARD_ARMOR:
			case TV_DRAG_ARMOR:
			{
				if (r<10) return add_abil_aux(a_ptr, a_ac);
				else if (r<18) return add_abil_aux(a_ptr, a_hold_life);
				else if (r<26) return add_abil_aux(a_ptr, a_res_conf);
				else if (r<34) return add_abil_aux(a_ptr, a_res_sound);
				else if (r<42) return add_abil_aux(a_ptr, a_res_shard);
				else if (r<50) return add_abil_aux(a_ptr, a_res_nexus);
				else if (r<58) return add_abil_aux(a_ptr, a_res_chaos);
				else if (r<66) return add_abil_aux(a_ptr, a_resistance);
				else if (r<74) return add_abil_aux(a_ptr, a_sust_all);
				else if (r<80) return add_abil_aux(a_ptr, a_remove_to_h_pen);
				else return add_abil_aux(a_ptr, a_to_a);
			}
			case TV_RING:
			case TV_AMULET:
			{
				if (r<8) return add_abil_aux(a_ptr, a_hold_life);
				else if (r<16) return add_abil_aux(a_ptr, a_res_conf);
				else if (r<24) return add_abil_aux(a_ptr, a_res_sound);
				else if (r<32) return add_abil_aux(a_ptr, a_res_shard);
				else if (r<40) return add_abil_aux(a_ptr, a_res_nexus);
				else if (r<48) return add_abil_aux(a_ptr, a_res_chaos);
				else if (r<56) return add_abil_aux(a_ptr, a_resistance);
				else if (r<64) return add_abil_aux(a_ptr, a_sust_all);
				else if (r<82) return add_abil_aux(a_ptr, a_to_a);
				else return add_abil_aux(a_ptr, a_to_h_to_d);
			}
			case TV_LITE:
			{
				if (r < 50) return add_abil_aux(a_ptr, a_telepathy);
				else return add_abil_aux(a_ptr, a_to_a);
			}
		}/*switch (a_ptr->tval)*/
	return FALSE;
}
static bool add_very_rare_specific(artifact_type *a_ptr)
{
		int r = rand_int(100);
		switch (a_ptr->tval)
		{
			case TV_BOW:
			{
				if (r<50) return add_abil_aux(a_ptr, a_might_shots);
				else return add_abil_aux(a_ptr, a_to_h_to_d);
			}
			case TV_DIGGING:
			case TV_HAFTED:
			case TV_POLEARM:
			case TV_SWORD:
			{
				if (r<10 && a_ptr->tval == TV_DIGGING) return add_abil_aux(a_ptr, a_impact);
				else if (r< 20) return add_abil_aux(a_ptr, a_dd);
				else if (r < 50) return add_abil_aux(a_ptr, a_kill_all);
				else if (r < 80) return add_abil_aux(a_ptr, a_blow);
				else if (r < 90) return add_abil_aux(a_ptr, a_to_h_to_d);
				else return add_abil_aux(a_ptr, a_to_a);
			}
			case TV_BOOTS:
			{
				if (r < 70)
				{
					if (a_ptr->pval < 0) return FALSE;
					return add_abil_aux(a_ptr, a_speed);
				}
				else return add_abil_aux(a_ptr, a_to_a);
			}
			case TV_GLOVES:
			{
				if (r < 20)
				{
					if (a_ptr->pval < 0) return FALSE;
					return add_abil_aux(a_ptr, a_speed);
				}
				else return add_abil_aux(a_ptr, a_to_a);
			}
			case TV_HELM:
			case TV_CROWN:
			case TV_SHIELD:
			case TV_CLOAK:
			case TV_SOFT_ARMOR:
			case TV_HARD_ARMOR:
			case TV_DRAG_ARMOR:
			case TV_RING:
			case TV_AMULET:
			case TV_LITE:
			{
				if (r < 10) return add_abil_aux(a_ptr, a_im_acid);
				else if (r < 20) return add_abil_aux(a_ptr, a_im_elec);
				else if (r < 30) return add_abil_aux(a_ptr, a_im_fire);
				else if (r < 40) return add_abil_aux(a_ptr, a_im_cold);
				else if (r < 50) return add_abil_aux(a_ptr, a_res_nether);
				else if (r < 60) return add_abil_aux(a_ptr, a_res_disen);
				else return add_abil_aux(a_ptr, a_to_a);
			}
		}/*switch (a_ptr->tval)*/
	return FALSE;
}
/*
 * Alex: based on remove_contradictory(artifact_type *a_ptr) from randart.c
 * Returns TRUE if new ability is not removed. Assumed that we remove no more than one ability.
 */
static bool  remove_contradictory(artifact_type *a_ptr)
{
	if (a_ptr->flags3 & TR3_AGGRAVATE)
	{
		a_ptr->flags1 &= ~(TR1_STEALTH);
		if (last_abil == a_stealth) return FALSE;
	}
	if (a_ptr->flags2 & TR2_IM_ACID)
	{
		a_ptr->flags2 &= ~(TR2_RES_ACID);
		if (last_abil == a_res_acid) return FALSE;
	}
	if (a_ptr->flags2 & TR2_IM_ELEC)
	{
		a_ptr->flags2 &= ~(TR2_RES_ELEC);
		if (last_abil == a_res_elec) return FALSE;
	}
	if (a_ptr->flags2 & TR2_IM_FIRE)
	{
		a_ptr->flags2 &= ~(TR2_RES_FIRE);
		if (last_abil == a_res_fire) return FALSE;
	}
	if (a_ptr->flags2 & TR2_IM_COLD)
	{
		a_ptr->flags2 &= ~(TR2_RES_COLD);
		if (last_abil == a_res_cold) return FALSE;
	}

	/* if (a_ptr->pval < 0)
	{
		if (a_ptr->flags1 & TR1_STR) a_ptr->flags2 &= ~(TR2_SUST_STR);
		if (a_ptr->flags1 & TR1_INT) a_ptr->flags2 &= ~(TR2_SUST_INT);
		if (a_ptr->flags1 & TR1_WIS) a_ptr->flags2 &= ~(TR2_SUST_WIS);
		if (a_ptr->flags1 & TR1_DEX) a_ptr->flags2 &= ~(TR2_SUST_DEX);
		if (a_ptr->flags1 & TR1_CON) a_ptr->flags2 &= ~(TR2_SUST_CON);
		if (a_ptr->flags1 & TR1_CHR) a_ptr->flags2 &= ~(TR2_SUST_CHR);
		a_ptr->flags1 &= ~(TR1_BLOWS);
	} */

	if (a_ptr->flags3 & TR3_LIGHT_CURSE)
	{
		a_ptr->flags3 &= ~(TR3_BLESSED);
		if (last_abil == a_blessed) return FALSE;
	}
	if (a_ptr->flags1 & TR1_KILL_DRAGON)
	{
		a_ptr->flags1 &= ~(TR1_SLAY_DRAGON);
		if (last_abil == a_slay_dragon) return FALSE;
	}
	if (a_ptr->flags1 & TR1_KILL_DEMON)
	{
		a_ptr->flags1 &= ~(TR1_SLAY_DEMON);
		if (last_abil == a_slay_demon) return FALSE;
	}
	if (a_ptr->flags1 & TR1_KILL_UNDEAD)
	{
		a_ptr->flags1 &= ~(TR1_SLAY_UNDEAD);
		if (last_abil == a_slay_undead) return FALSE;
	}
	/* New ability is not removed */
	return TRUE;
}
/* Returns TRUE if actually adds new ability.
 */
static bool add_ability(artifact_type *a_ptr, int delta)
{
	int r, limit;
	bool ok = FALSE;

	last_abil = a_last;
	last_rarity = r_last;
	show_plus = FALSE;
	show_pval = FALSE;
	strcpy(new_property, "");
	plus = 0;

	if (!rand_int(10) && a_ptr->pval)
		return add_abil_aux(a_ptr, a_pval);

	/* Alex: weapon should have more specific abilities */
	switch (a_ptr->tval)
	{
		case TV_DIGGING:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_BOW:
			limit = 8;
			break;
		default:
			limit = 6;
	}
	r = rand_int(10);
	if (delta<ART_R_1)
	{
		if (r<6) last_rarity = 0;/*The most probable*/
		else last_rarity = 1;
	}
	else if (delta <ART_R_2)
	{
		if (r<1) last_rarity = 0;
		else if (r<7) last_rarity = 1;/*The most probable*/
		else last_rarity = 2;
	}
	else if (delta <ART_R_3)
	{
		if (r<1) last_rarity = 1;
		else if (r<7) last_rarity = 2;/*The most probable*/
		else last_rarity = 3;
	}
	else
	{
		if (r<2) last_rarity = 2;
		else last_rarity = 3;/*The most probable*/
	}

	r = rand_int(10);
	if (r < limit)		/* Pick something dependent on item type. */
	{
		switch (last_rarity)
		{
			case 0: ok = add_common_specific(a_ptr); break;
			case 1: ok = add_uncommon_specific(a_ptr); break;
			case 2: ok = add_rare_specific(a_ptr); break;
			case 3: ok = add_very_rare_specific(a_ptr); break;
		}
	}
	else			/* Pick something universally useful. */
	{
		switch (last_rarity)
		{
			case 0: ok = add_common_ability(a_ptr); break;
			case 1: ok = add_uncommon_ability(a_ptr); break;
			case 2: ok = add_rare_ability(a_ptr); break;
			case 3: ok = add_very_rare_ability(a_ptr); break;
		}
	}

//	if (last_abil == a_last) msg_print("add_ability() error: no new ability.");

	if (ok)
		/* Now remove contradictory or redundant powers. */
		/* Here we can remove new ability */
		return remove_contradictory(a_ptr);
	else return FALSE;
}

/* 
 * Memory allocated for art_capacity number of artifact.
 * art_capacity can be > z_info->a_max.
 */
static int art_capacity = 0;

void init_new_art(){
	art_capacity = z_info->a_max;

	if (!kinds)
	{
		/* Allocate the "kinds" array */
		/* FREE in done_randart() */
		C_MAKE(kinds, z_info->a_max, s16b);
	}

	if (!names_list_len) names_list_len = strlen(names_list);

	if (!new_art_names)
	{
		char* cp;
		new_art_names = malloc(names_list_len+1);
		strcpy(new_art_names, names_list);
		cp = new_art_names;
		*cp = toupper(*cp);
		while (*cp)
		{
			if (*cp == '\n')
			{
				*cp = 0;
				cp++;
				if (*cp)
				*cp = toupper(*cp);
			}
			else
				cp++;
		}
	}
}

void done_new_art()
{
	if (kinds)
	{
		FREE(kinds);
		kinds = NULL;
	}
	if (new_art_names)
	{
		free(new_art_names);
		new_art_names = NULL;
	}
}

/*base must not be NULL*/
static void make_base(artifact_type *a_ptr, const object_type* base)
{
	s16b k_idx;
	object_kind *k_ptr;

	k_idx = lookup_kind(base->tval, base->sval);
	k_ptr = &k_info[k_idx];

	/* Create artifact based on base */
	a_ptr->tval = base->tval;
	a_ptr->sval = base->sval;
	a_ptr->pval = base->pval;
	a_ptr->to_h = base->to_h;
	a_ptr->to_d = base->to_d;
	a_ptr->to_a = base->to_a;
	a_ptr->ac = base->ac;
	a_ptr->dd = base->dd;
	a_ptr->ds = base->ds;
	a_ptr->weight = base->weight;
	a_ptr->flags1 = k_ptr->flags1;
	a_ptr->flags2 = k_ptr->flags2;
	a_ptr->flags3 = k_ptr->flags3;

	switch (base->xtra1)
	{
		case OBJECT_XTRA_TYPE_SUSTAIN:
		{
			/* OBJECT_XTRA_WHAT_SUSTAIN == 2 */
			a_ptr->flags2 |= (OBJECT_XTRA_BASE_SUSTAIN << base->xtra2);
			break;
		}

		case OBJECT_XTRA_TYPE_RESIST:
		{
			/* OBJECT_XTRA_WHAT_RESIST == 2 */
			a_ptr->flags2 |= (OBJECT_XTRA_BASE_RESIST << base->xtra2);
			break;
		}

		case OBJECT_XTRA_TYPE_POWER:
		{
			/* OBJECT_XTRA_WHAT_POWER == 3 */
			a_ptr->flags3 |= (OBJECT_XTRA_BASE_POWER << base->xtra2);
			break;
		}
	}

	if (base->name2)
	{
		ego_item_type* e_ptr = &e_info[base->name2];
		a_ptr->level = e_ptr->level;
		a_ptr->rarity = e_ptr->rarity;
		a_ptr->flags1 |= e_ptr->flags1;
		a_ptr->flags2 |= e_ptr->flags2;
		a_ptr->flags3 |= e_ptr->flags3;
	}
	else
	{
		a_ptr->level = k_ptr->level;
			a_ptr->rarity = k_ptr->chance[0];
	}

	if (k_ptr->flags3 & TR3_ACTIVATE)
	{
		/* Activation as base item */
		a_ptr->activation = ACT_MAX;
		a_ptr->time = 1;
		a_ptr->randtime = 0;
	}

	if (a_ptr->to_d || (a_ptr->to_h && !is_armour(a_ptr->tval)))
		a_ptr->flags3 |= TR3_SHOW_MODS;

	/* Artifacts ignore everything */
	a_ptr->flags3 |= TR3_IGNORE_MASK;

	/* New artifacts must not be too often */
	if (a_ptr->rarity < 5)
		a_ptr->rarity = 5;
}

/* Adds memory for n artifacts; memory is not wiped */
/* Returns TRUE if success */
bool realloc_art(int n){
	int new_size = z_info->a_max + n;
	int try_alloc = art_capacity;
	artifact_type* a = 0;
	s16b* k =0;

	if (new_size > art_capacity)
	/* Need more memory */
	{
		int i;

		if (n < 20)
			try_alloc += 20;
		else
			try_alloc += n;

		a = realloc(a_info, sizeof(artifact_type)*try_alloc);
		k = realloc(kinds, sizeof(s16b)*try_alloc);

		if (!a || !k)
			return FALSE;

		a_head.info_ptr = a;
		a_info = a;
		kinds = k;

		/* Initialize all new kinds[] */
		for (i = art_capacity; i < try_alloc; i++)
			kinds[i] = 0;
	}

	/* Enough memory or just allocated */
	art_capacity = try_alloc;
	z_info->a_max = new_size;
	a_head.info_num = z_info->a_max;
	a_head.info_size = z_info->a_max * sizeof(artifact_type);
	
	return TRUE;
}

/* Returns TRUE if success */
static bool add_artifact(const artifact_type* a_ptr)
{
	int name_index = 0;
	int text_index = rand_int(z_info->a_max-1) + 1;
	artifact_type* a;

	if (!a_ptr->tval)
		message_format(MSG_KILL, 0, "ZERO ARTIFACT TVAL!");

	if (!realloc_art(1))
		return FALSE;

	a = &a_info[z_info->a_max-1];
	*a = *a_ptr;
	name_index = rand_int(names_list_len - 10);

	while(names_list[name_index] != '\n')
		name_index++;
	if (name_index == names_list_len - 1)
		/*It is impossible - "Yavanna" will be the last name*/
		a->name = 0;
	else
		a->name = name_index + 1;

	a->text = a_info[text_index].text;

	a->cur_num = 0;
	/* a->force_depth = 0; */

	return TRUE;
}

/* Alex: adds extra random artifact begining from base object.
 * Base object does not change - assign o_ptr->name1 = z_info->a_max-1; then apply_magic(o_ptr, -1, TRUE, TRUE, TRUE);
*/
bool add_new_art(int power, const object_type* base, bool player_choose, int choices, int tries)
{
	artifact_type art_body;
	artifact_type* a_ptr = &art_body;
	int res_power;

	last_power = 0;

	if (!base)
		return FALSE;

	/* Paranoia -- no "plural" artifacts */
	if (base->number != 1) return (FALSE);

	if (!item_tester_hook_for_create_artifact(base))
		return FALSE;

	make_base(a_ptr, base);
	
	if (!add_artifact(a_ptr))
	{
		if (player_choose) msg_print("Not enough memory for new Artifact.");
		return FALSE;
	}

	if (!enchant_artifact(z_info->a_max-1, power, player_choose, choices, tries))
	{
		kinds[z_info->a_max - 1] = 0;
		z_info->a_max --;
		return FALSE;
	}
	a_ptr = &a_info[z_info->a_max - 1];

	res_power = art_power(a_ptr, 0);
	a_ptr->level += res_power/5;
	a_ptr->cost = res_power * 1000L;
	if (a_ptr->cost < 0) a_ptr->cost = 0;

	if (is_special(a_ptr->tval))
		a_ptr->flags3 |= TR3_INSTA_ART;

	a_ptr->cur_num = 0;
	/* a_ptr->force_depth = 0; */
	a_ptr->max_num = 1;
	
	return TRUE;
}

/* 
 * Alex: enchant an existing artifact number a_idx. Player has no choice.
 * power is a maximum possible artifact power.
 * If power >= 0, function adds random abilities. Every time new ability is too powerfull,
 * tries decremented by 1. When tries == 0, function returns result.
 * If power < 0, function generates cursed artifact.
 */
bool enchant_art_auto(int a_idx, int power, int tries, int k_idx)
{
	artifact_type* a_ptr = &a_info[a_idx];
	artifact_type art_try;
	int original_power = 0;
	int try_power = 0;

	last_power = original_power = art_power(a_ptr, k_idx);

	if (power >= 0)
	{
		while (tries > 0)
		{
			while(1)
			{
				art_try = *a_ptr;
				if (a_ptr->pval && !rand_int(8))
				{
					add_pval(&art_try);
					break;/*while(1)*/
				}
				else if (add_ability(&art_try, power - last_power) && last_abil != a_last)
					break;/*while(1)*/
			}/*while(1)*/

			try_power = art_power(&art_try, k_idx);
			if (try_power <= power)
			{
				/* Okay, accept new ability */
				*a_ptr = art_try;
				last_power = try_power;
				if (last_rarity != r_last)
					a_ptr->rarity += last_rarity;
			}
			else
				tries--;
			if (last_power == power)
				break;
		}
	}
	else
	/* Cursed artifact */
	{
		while(last_power > power)
		{
			add_ability(a_ptr, power - last_power);
			do_curse(a_ptr);
			remove_contradictory(a_ptr);
			last_power = art_power(a_ptr, k_idx);
		}
		if (!rand_int(3))
		{
			a_ptr->flags3 |= TR3_HEAVY_CURSE;
			if (!rand_int(5))
				a_ptr->flags3 |= TR3_PERMA_CURSE;
		}
	}

	if (original_power == last_power)
		return FALSE;

	return TRUE;
}

/* Show artifact info */
static void show_art(int a_idx)
{
	char o_name[80];

	object_type obj;
	object_type* o_ptr = &obj;

	object_wipe(o_ptr);
	if (!make_fake_artifact(o_ptr, a_idx))
	{
		prt("Error: cannot describe artifact!", 16, 0);
		return;
	}
	o_ptr->ident |= IDENT_MENTAL;

	/* clear_from(16); */
	/* Place cursor */
	move_cursor(16, 0);

	/* Describe object: code from screen_out_head(), object_info_screen() */

	/* Description */
	object_desc_spoil(o_name, 80, o_ptr, TRUE, 3);

	/* Print, in colour */
	text_out_c(TERM_YELLOW, format("%^s\n", o_name));

	/* Display the object description */
	if (k_info[o_ptr->k_idx].text)
	{
		text_out(k_text + k_info[o_ptr->k_idx].text);
		text_out("\n");
	}

	object_info_out(o_ptr);
}

enum {MAX_CHOICES = 10};
enum Art_Abil proposed_abil[MAX_CHOICES];
s32b proposed_power[MAX_CHOICES];
enum Rarity proposed_rarity[MAX_CHOICES];
artifact_type proposed_art[MAX_CHOICES];

/* Parse string e.g. "BDEc" to disable b), d), e) and accept and return c)."
 * Returns -1 if there is no small letters.
 */
static int parse_command(cptr cmd, int choices)
{
	const char* t = cmd;
	int a;
	int abil_choosen = -1;

	while (*t)
	{
		if (isalpha(*t))
		{
			if (islower(*t))
			{
				a = A2I(*t);
				if (a >= 0 && a < choices)
					abil_choosen = a;
			}
			else
			{
				a = A2I(tolower(*t));
				if (a >= 0 && a < choices && can_disable[proposed_abil[a]])
					disabled[proposed_abil[a]] = TRUE;
			}
		}
		t++;
	}
	return abil_choosen;
}

/* Probabilities of fail for abilities. unity_prob is 100%. */
u16b abil_fail[a_last];
/* Probabilities of fail for activations. unity_prob is 100%. */
u16b act_fail[ACT_MAX];

static int success_percent(int fail)
{
	return ((unity_prob - fail) * 100) / unity_prob;
}
bool is_activation(enum Art_Abil a)
{
	return (a_comm_act == a || a_uncomm_act == a|| a_rare_act == a|| a_vrare_act == a);
}
bool is_bad(enum Art_Abil a)
{
	return (a_drain_exp == a|| a_teleport == a|| a_aggr ==a || a_heavy_weight == a);
}
bool is_dependent(enum Art_Abil a)
{
	return (a_pval == a || a_sust_all == a || a_resistance == a || a_all_stats == a || a_kill_all == a);
}
static void show_choice(int c)
{
	const artifact_type* a_ptr = &proposed_art[c];
	char buf[200];
	char* t = buf;
	int succ;

	if (a_last == last_abil)
	{
		c_prt(TERM_L_BLUE, "", c + 5, 0);
		return;
	}

	if (can_disable[last_abil])
		object_desc_chr_macro(t, toupper(I2A(c)));
	else
		object_desc_chr_macro(t, I2A(c));

	object_desc_str_macro(t, ") ");

	if (new_property[0] != '\0')
	{
		object_desc_str_macro(t, new_property);

		if (show_plus || show_pval)
		{
			object_desc_str_macro(t, " (");
			if (show_pval) object_desc_int_macro(t, a_ptr->pval);
			else object_desc_int_macro(t, plus);
			object_desc_chr_macro(t, ')');
		}
		object_desc_str_macro(t, " <");
		object_desc_s_int_macro(t, proposed_power[c]);
		object_desc_str_macro(t, "> (");
		object_desc_str_macro(t, rarity_name[last_rarity]);
		object_desc_chr_macro(t, ')');
	}
	else
		msg_format("Error: ability %d without description!", last_abil);

	if (is_activation(last_abil))
		succ = success_percent(act_fail[proposed_art[c].activation]);
	else
		succ = success_percent(abil_fail[last_abil]);
	object_desc_str_macro(t, " (");
	object_desc_s_int_macro(t, succ);
	object_desc_str_macro(t, "%)");

	*t = '\0';
	c_prt(TERM_L_BLUE, buf, c + 5, 0);
}

/* 
 * Alex: enchant an existing artifact number a_idx.
 * power is maximum possible artifact power,
 * player_choose = true if player can choose new abilityes,
 * choices is a number of proposed abilities per one try (disregarded if player_choose = false),
 * choices must be <= MAX_CHOICES,
 * tries is a number of proposals.
 */
bool enchant_artifact(int a_idx, int power, bool player_choose, int choices, int tries)
{
	artifact_type* a_ptr = &a_info[a_idx];
	int original_power = 0;
	int k_idx = 0;

	if (!a_idx)
		return FALSE;

	/* Try to use the cache */
	k_idx = kinds[a_idx];

	/* Lookup the item if not yet cached */
	if (!k_idx)
	{
		k_idx = lookup_kind(a_ptr->tval, a_ptr->sval);

		/* Cache the object index */
		kinds[a_idx] = k_idx;

		/* Paranoia */
		if (!k_idx)
		{
			quit_fmt("Illegal tval/sval value for artifact %d!", a_idx);
		}
	}

	if (power >= 0)
	{
		cursed_art = FALSE;
		while (!rand_int(GREAT_OBJ))
			power += 50;
	}
	else
		cursed_art = TRUE;

	last_power = original_power = art_power(a_ptr, k_idx);

	if (power >= 0 && original_power >= power)
		return FALSE;

	if (!player_choose || power < 0)
	{
		if (!enchant_art_auto(a_idx, power, tries, k_idx))
			return FALSE;
	}
	else
	{
		int i;
		cptr report_result = "";/* "Success!" or "Failure!" */

		if (choices > MAX_CHOICES)
			choices = MAX_CHOICES;
		else if (choices < 1)
			choices = 1;
		msg_format("Enchanting artifact number %d of %d.", a_idx, z_info->a_max - 1);
		msg_format("Base power: %d.", original_power);

		/* Redirect output to the screen */
		text_out_hook = text_out_to_screen;
		/* Save screen */
		screen_save();

		for (i = 0; i < a_last; i++)
			disabled[i] = FALSE;

		clear_from(0);

		prt("Capital letters to disable, one small letter to accept, ESC to next.", 1, 0);
		prt("Only Abilities with capital letters can be disabled.", 2, 0);
		prt("Example: BDEc to disable b), d), e) and accept c).", 3, 0);

		/* Set object_info_out() hook */
		object_info_out_flags = object_flags;

		for (i = 1; i <= tries; i++)
		{
			char msg[80];
			char* t = msg;
			char out_val[MAX_CHOICES + 1] = "";

			int abil_choosen = 0;
			int min_power = 0;
			int c;

			object_desc_str_macro(t, report_result);
			object_desc_str_macro(t, "Try ");
			object_desc_num_macro(t, i);
			object_desc_str_macro(t, " of ");
			object_desc_s_int_macro(t, tries);
			object_desc_str_macro(t, ". Power ");
			object_desc_s_int_macro(t, last_power);
			object_desc_str_macro(t, " of ");
			object_desc_s_int_macro(t, power);
			object_desc_str_macro(t, ". ");

			show_art(a_idx);

			prt("", 22, 0);
			for (c = 0; c < choices; c++)
			{
				int c1;
				int tr = 0;

				while(tr < 10000)
				{
					tr++;

					proposed_art[c] = *a_ptr;
					if (!add_ability(&proposed_art[c], power - last_power))
						continue;/*while*/

					if (disabled[last_abil])
						continue;/*while*/

					/* Check previous abilities */
					for (c1 = 0; c1 < c; c1++)
						if (proposed_abil[c1] == last_abil
						/* && proposed_power[c1] == proposed_power[c] */)
						{
							last_abil = a_last;
							break;/* for (c1) */
						}
					if (last_abil == a_last)
						continue;/*while*/
					if (is_activation(last_abil) &&
						success_percent(act_fail[proposed_art[c].activation]) == 0)
					{
						/*byte act = proposed_art[c].activation;
						prt(format("Skip activation #%d (fail: %d).", act, act_fail[act]), 22, 0);
						*/
						continue;/*while*/
					}
					else if (success_percent(abil_fail[last_abil]) == 0)
						continue;/*while*/

					proposed_power[c] = art_power(&proposed_art[c], k_idx);
					if (proposed_power[c] > power)
					{
						last_abil = a_last;
						continue;/*while*/
					}

					proposed_abil[c] = last_abil;
					proposed_rarity[c] = last_rarity;
					break;/* while */
				}
				if (tr >= 10000)
				{
					last_abil = a_last;
					proposed_power[c] = 20000;
					proposed_abil[c] = a_last;
					proposed_rarity[c] = r_last;
				}
				show_choice(c);
			}

			min_power = proposed_power[0];
			for (c = 1; c < choices; c++)
				if (min_power > proposed_power[c])
				{
					abil_choosen = c;
					min_power = proposed_power[c];
				}
			if (min_power < last_power && p_ptr->lev < PY_MAX_LEVEL && !rand_int(1 + p_ptr->lev/5))
			{
				/* Automatically accept bad change */
				object_desc_str_macro(t, "Oops!.. Accepted ability ");
				object_desc_chr_macro(t, I2A(abil_choosen));
				object_desc_str_macro(t, "). ");
				*t = '\0';
				get_string(msg, out_val, 2);
			}
			else
			{
				object_desc_str_macro(t, "Command: ");
				*t = '\0';

				abil_choosen = -1;
				if (get_string(msg, out_val, choices + 1))
					abil_choosen = parse_command(out_val, choices);
			}

			if (abil_choosen >= 0)
			{
				/* Okay, try new ability */
				int fail;
				enum Art_Abil abil = proposed_abil[abil_choosen];

				if (is_activation(abil))
					fail = act_fail[proposed_art[abil_choosen].activation];
				else
					fail = abil_fail[abil];

				if (rand_int(unity_prob) >= fail)
				{
					/* Success */
					last_rarity = proposed_rarity[abil_choosen];
					*a_ptr = proposed_art[abil_choosen];
					last_power = proposed_power[abil_choosen];
					if (last_rarity != r_last)
						a_ptr->rarity += last_rarity;
					else
						if (player_choose) msg_print("Error: incorrect ability rarity.");

					fail = 100;
					if (!is_bad(abil))
						report_result = "Success! ";
					else
						report_result = "";
				}
				else
				{
					fail = 10;
					report_result = "Failure! ";
				}

				/* Update fails: more for success, less for fail */
				if (is_activation(abil))
					update_act_fail(proposed_art[abil_choosen].activation, fail);
				else
					update_abil_fail(abil, fail);
			}
			else
				report_result = "";

			if (last_power >= power)
				break;
		}/* for i */

		/* Load screen */
		screen_load();
	}


	if (original_power == last_power)
		return FALSE;

	a_ptr->cost = last_power * 1000L;

	if (a_ptr->cost < 0) a_ptr->cost = 0;

	if (abs(last_power / 8) > a_ptr->rarity)
		a_ptr->rarity = abs(last_power / 8);

	/*
	 * Add TR3_HIDE_TYPE to all artifacts with nonzero pval because we're
	 * too lazy to find out which ones need it and which ones don't.
	 */
	if (a_ptr->pval)
		a_ptr->flags3 |= TR3_HIDE_TYPE;

	return TRUE;
}

/* Make random artifact without player. Object becomes artifact */
bool make_rnd_art(object_type* base)
{
	int r, power;

	if (rand_int(RANDOM_ART_CHANCE) || rand_int(RANDOM_ART_CHANCE))
		return FALSE;

	r = rand_int(100);
	if (r < 66) power = Rand_normal(10, 10);
	else if (r < 99) power = Rand_normal(20, 20);
	else power = Rand_normal(50, 30);

	if (!add_new_art(power, base, FALSE, 0, 50))
		return FALSE;

	base->name1 = z_info->a_max-1;
	apply_magic(base, -1, TRUE, TRUE, TRUE);
	return TRUE;
}

/* Returns 0 if cancel/bad enter */
int get_levels_sacrifice()
{
	char out_val[4];
        char def[4];
        char msg[80];
	int sacr;

	strcpy(msg, "Enter how many experience levels you will sacrifice (10-");
        itoa(p_ptr->lev - 1, def, 10);

	strcat(msg, def);
        strcat(msg, "): ");

	/* Default */
        strcpy(out_val, def);

	/* Ask for sacrifice */
	if (!get_string(msg, out_val, 3)) return 0;

	sacr = atoi(out_val);
	if (sacr < 10 || sacr >= p_ptr->lev)
		return 0;

	return sacr;
}
void update_pval_fail()
{
	abil_fail[a_pval] = (PY_MAX_LEVEL - p_ptr->lev) * unity_prob / PY_MAX_LEVEL;
}
/* 100% fail probability to add artifact abilities and activations (except bad abilities) */
void wipe_fails()
{
	int a;

	for (a = 0; a < a_last; a++)
		abil_fail[a] = unity_prob;

	abil_fail[a_drain_exp] = 0;
	abil_fail[a_teleport] = 0;
	abil_fail[a_aggr] = 0;
	abil_fail[a_heavy_weight] = 0;
	update_pval_fail();

	for (a = 0; a < ACT_MAX; a++)
		act_fail[a] = unity_prob;
}

/* decrease ability fail by p */
void update_abil_fail(enum Art_Abil a, int p)
{
	u32b fail = abil_fail[a];
	if (!fail || p<=0) return;
	if (p < unity_prob)
		abil_fail[a] = (u16b)(fail * (unity_prob - p) / unity_prob);
	else
		abil_fail[a] = 0;

	if (a >= a_str && a <= a_chr)
	{
		u32b succ = unity_prob - abil_fail[a_str];
		succ *= (unity_prob - abil_fail[a_int]) / unity_prob;
		succ *= (unity_prob - abil_fail[a_wis]) / unity_prob;
		succ *= (unity_prob - abil_fail[a_dex]) / unity_prob;
		succ *= (unity_prob - abil_fail[a_con])/ unity_prob;
		succ *= (unity_prob - abil_fail[a_chr])/ unity_prob;
		abil_fail[a_all_stats] = (u16b)(unity_prob - succ);
	}
	else if (a >= a_sust_str && a <= a_sust_chr)
	{
		u32b succ = unity_prob - abil_fail[a_sust_str];
		succ *= (unity_prob - abil_fail[a_sust_int]) / unity_prob;
		succ *= (unity_prob - abil_fail[a_sust_wis]) / unity_prob;
		succ *= (unity_prob - abil_fail[a_sust_dex]) / unity_prob;
		succ *= (unity_prob - abil_fail[a_sust_con])/ unity_prob;
		succ *= (unity_prob - abil_fail[a_sust_chr])/ unity_prob;
		abil_fail[a_sust_all] = (u16b)(unity_prob - succ);
	}
	else if ((a >= a_res_acid && a <= a_res_cold) || a_res_pois == a)
	{
		u32b succ = unity_prob - abil_fail[a_res_acid];
		succ *= (unity_prob - abil_fail[a_res_elec]) / unity_prob;
		succ *= (unity_prob - abil_fail[a_res_fire]) / unity_prob;
		succ *= (unity_prob - abil_fail[a_res_cold]) / unity_prob;
		succ *= (unity_prob - abil_fail[a_res_pois])/ unity_prob;
		abil_fail[a_resistance] = (u16b)(unity_prob - succ);
	}
	else if (a_kill_dragon == a || a_kill_undead == a || a_kill_demon == a)
	{
		u32b succ = unity_prob - abil_fail[a_kill_dragon];
		succ *= (unity_prob - abil_fail[a_kill_undead]) / unity_prob;
		succ *= (unity_prob - abil_fail[a_kill_demon]) / unity_prob;
		abil_fail[a_kill_all] = (u16b)(unity_prob - succ);
	}
}
/* decrease activation fail by p */
void update_act_fail(byte a, int p)
{
	u32b fail = act_fail[a];
	if (!fail || p<=0) return;
	if (rand_int(act_rarity[a] + 1)) return;
	if (p < unity_prob)
		act_fail[a] = (u16b)(fail * (unity_prob - p) / unity_prob);
	else
		act_fail[a] = 0;
}
enum {dam_div = 20, dam_dam_div = 2500};
static int fail_dam(int dam)
{
	if (dam >= dam_div)
		return dam/dam_div + dam*dam/dam_dam_div;
	else if (dam > rand_int(dam_div)) return 1;
	return 0;
}
/* decrease ability fail due to damage */
void update_abil_fail_dam(enum Art_Abil a, int dam)
{
	update_abil_fail(a, fail_dam(dam));
}
/* decrease ability fail due to damage */
void update_act_fail_dam(byte a, int dam)
{
	update_act_fail(a, fail_dam(dam));
}
/* Halves some abilities/activation probs of success */
void forget_abil_act()
{
	int i;
	msg_print("Forgotten abilities: ");
	for (i = 0; i < 10; i++)
	{
		enum Art_Abil a = rand_int(a_last);
		int success;
		/* Ignore activations, bad abilities and
		 * abilities that have special fail calculation functions */
		if (is_activation(a) || is_bad(a) || is_dependent(a))
			continue;
		success = unity_prob - abil_fail[a];
		abil_fail[a] = unity_prob - success / 2;
		msg_format("%s. ", abil_name[a]);
	}

	msg_print("Forgotten activations: ");
	for (i = 0; i < 5; i++)
	{
		char descr[70] = "";
		byte a = (byte)rand_int(ACT_MAX);
		int success = unity_prob - act_fail[a];
		act_fail[a] = unity_prob - success / 2;
		if (describe_act(a, 0, 0, descr, 70))
			msg_format("%s. ", descr);
		else
			msg_format("Activation number %d. ", a);
	}
}
/*
 * Alex: display known abilities
 */
void do_cmd_knowledge_abils()
{
	int a;

	FILE *fff;

	char file_name[1024];

	/* Temporary file */
	fff = my_fopen_temp(file_name, sizeof(file_name));

	/* Failure */
	if (!fff) return;

	/* Scan the abilities */
	for (a = 0; a < a_last; a++)
		if (!is_activation(a) && !is_bad(a))
		{
			int succ = success_percent(abil_fail[a]);
			if (succ) fprintf(fff, "     %s: %d%%\n", abil_name[a], succ);
		}

	/* Close the file */
	my_fclose(fff);

	/* Display the file contents */
	show_file(file_name, "Activation Success", 0, 0);

	/* Remove the file */
	fd_kill(file_name);
}

/*
 * Alex: display known activation
 */
void do_cmd_knowledge_acts()
{
	byte act;

	FILE *fff;

	char file_name[1024];

	/* Temporary file */
	fff = my_fopen_temp(file_name, sizeof(file_name));

	/* Failure */
	if (!fff) return;

	/* Scan the activations */
	for (act = 0; act < ACT_MAX; act++)
	{
		char descr[70] = "";
		int succ = success_percent(act_fail[act]);
		if (succ)
		{
			if (describe_act(act, 0, 0, descr, 70))
				fprintf(fff, "     %s: %d%%\n", descr, succ);
			else
				fprintf(fff, "     Activation number %d: %d%%\n", act, succ);
		}
	}

	/* Close the file */
	my_fclose(fff);

	/* Display the file contents */
	show_file(file_name, "Activation Success", 0, 0);

	/* Remove the file */
	fd_kill(file_name);
}
