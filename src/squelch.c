/*
 * File: squelch.c
 * Purpose: Item destruction
 *
 * Copyright (c) 2007 David T. Blackston, Iain McFall, DarkGod, Jeff Greene,
 * David Vestal, Pete Mack, Andrew Sidwell.
#ifdef EFG
 * Copyright (c) 2008 Edward F. Grove.
#endif
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 2, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */
#include "angband.h"
#include "cmds.h"


/*
 * The squelch code has a long history.  Originally it started out as a simple
 * sval-dependent item destroyer, but then ego-item and quality squelch was
 * added too, and then squelched items on the dungeon floor were marked by
 * purple dots, and by this time the code was quite unmaintainable and pretty
 * much impossible to work with.
 *
 * Luckily, though, it's been cleaned up.  There is now only sval-dependent
 * squelch and quality-based squelch, and the two don't interact -- quality-based
 * is for items that get pseudo-id'd and sval-dependent is for potions and the
 * like.
 *
 * The squelch code figures most things out itself.  Simply do:
 *     p_ptr->notice |= PN_SQUELCH;
 * whenever you want to make the game check for squelched items.
 *
 * The quality-dependent squelch is much reduced in scope from how it used to
 * be.  If less "general" settings are desired, they can be added easily enough
 * by changing entries in type_tvals[][], adding more TYPE_* constants, and
 * updating type_names.  Savefile compatibility is automatically ensured.
 *
 *
 * The UI code is much cleaner than it was before, but the interface itself
 * still needs some design.  XXX
 */

#ifdef EFG

/* EFGchange notice obvious effects */
typedef struct {enum object_flag flag_idx; char *desc; int format;} obvious_struct;
static obvious_struct obvious_list[] =
{
	{ LITE, "%s provides permanent light.", OBVIOUS_FORMAT_NAME },
	{ REGEN, "%s speeds your regeneration.", OBVIOUS_FORMAT_NAME },
	{ TELEPATHY, "%s provides ESP.", OBVIOUS_FORMAT_NAME },
/* The "random" powers from xtra==3 must come first, and we need the count */
#define OBVIOUS_XTRA 3
	{ ACTIVATE, "%s can be activated.", OBVIOUS_FORMAT_NAME },
	{ DRAIN_EXP, "%s drains experience.", OBVIOUS_FORMAT_NAME },
	{ STR,      "%s %s your strength by %d", OBVIOUS_FORMAT_BOOST },
	{ INT,      "%s %s your intelligence by %d", OBVIOUS_FORMAT_BOOST },
	{ WIS,      "%s %s your wisdom by %d", OBVIOUS_FORMAT_BOOST },
	{ DEX,      "%s %s your dexterity by %d", OBVIOUS_FORMAT_BOOST },
	{ CON,      "%s %s your constitution by %d", OBVIOUS_FORMAT_BOOST },
	{ CHR,      "%s %s your charisma by %d", OBVIOUS_FORMAT_BOOST },
	{ STEALTH,  "%s %s your stealth by %d", OBVIOUS_FORMAT_BOOST },
	{ SPEED,    "%s %s your speed by %d", OBVIOUS_FORMAT_BOOST },
	{ BLOWS,    "%s %s your blows by %d", OBVIOUS_FORMAT_BOOST },
	{ SHOTS,    "%s %s your shots by %d", OBVIOUS_FORMAT_BOOST },
	{ INFRA,    "%s %s your infravision by %d", OBVIOUS_FORMAT_BOOST },
	{ BRAND_POIS, "%s is branded with poison", OBVIOUS_FORMAT_NAME },
	{ BRAND_ACID, "%s is branded with acid", OBVIOUS_FORMAT_NAME },
	{ BRAND_ELEC, "%s is branded with electricity", OBVIOUS_FORMAT_NAME },
	{ BRAND_FIRE, "%s is branded with fire", OBVIOUS_FORMAT_NAME },
	{ BRAND_COLD, "%s is branded with cold", OBVIOUS_FORMAT_NAME },
};

static enum object_flag excellent_list[] =
{
/* currently considered average
SEARCH,
TUNNEL,
*/
MIGHT,
SLAY_ANIMAL,
SLAY_EVIL,
SLAY_UNDEAD,
SLAY_DEMON,
SLAY_ORC,
SLAY_TROLL,
SLAY_GIANT,
SLAY_DRAGON,
KILL_DRAGON,
KILL_DEMON,
KILL_UNDEAD,
/* currently considered average
SUST_STR,
SUST_INT,
SUST_WIS,
SUST_DEX,
SUST_CON,
SUST_CHR,
*/
IM_ACID,
IM_ELEC,
IM_FIRE,
IM_COLD,
RES_ACID,
RES_ELEC,
RES_FIRE,
RES_COLD,
RES_POIS,
RES_FEAR,
RES_LITE,
RES_DARK,
RES_BLIND,
RES_CONFU,
RES_SOUND,
RES_SHARD,
RES_NEXUS,
RES_NETHR,
RES_CHAOS,
RES_DISEN,
/* currently considered average
SLOW_DIGEST,
FEATHER,
*/
SEE_INVIS,
FREE_ACT,
HOLD_LIFE,
NO_FUEL,
IMPACT,
/* currently considered cursed
TELEPORT,
AGGRAVATE,
DRAIN_EXP,
*/
/* not clear if these should be excellent, since some base armor has ignore_acid
IGNORE_ACID,
*/
IGNORE_ELEC,
IGNORE_FIRE,
IGNORE_COLD,
BLESSED,
};



void recognize_artifact(object_type *o_ptr)
{
	if (!object_is_known(o_ptr))
	{
		/* ??? do we need a sound */
		char o_name[80];
		object_known(o_ptr);
		object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 0);
	
		msg_format("You recognize the legendary %s.", o_name);
	}
}

bool object_flavor_is_fixed(const object_type *o_ptr)
{
	switch(o_ptr->tval)
	{
		case TV_SCROLL:
if (o_ptr->pval) printf ("anomalous pval %d on object kind %d\n", o_ptr->pval, o_ptr->k_idx);
		case TV_FLASK: /* pval light */
		case TV_POTION: /* pval = nourishment */
		case TV_FOOD:  /* pval = nourishment */
		case TV_WAND:  /* pval = charges */
		case TV_STAFF: /* pval = charges */
		case TV_ROD:   /* pval = timeout */
if (o_ptr->to_h) printf ("anomalous to_h %d on object kind %d\n", o_ptr->to_h, o_ptr->k_idx);
if (o_ptr->to_d) printf ("anomalous to_d %d on object kind %d\n", o_ptr->to_d, o_ptr->k_idx);
if (o_ptr->to_a) printf ("anomalous to_a %d on object kind %d\n", o_ptr->to_a, o_ptr->k_idx);
			return TRUE;
		default:
			if ((o_ptr->pval) || (o_ptr->to_h) || (o_ptr->to_d) || (o_ptr->to_a))
				return FALSE;
			else
				return TRUE;
	}
}

bool object_is_known_excellent(const object_type *o_ptr)
{
        if (object_is_known(o_ptr))
        {
                printf("*** excellence of known objects not implemented\n");
                return FALSE;
        }
        else
        {
                switch(o_ptr->pseudo)
                {
                        case INSCRIP_TRIED:
                                /* ??? this should go away */
                                return object_is_splendid(o_ptr);
                        case INSCRIP_SPLENDID:
                        case INSCRIP_EXCELLENT:
                                return TRUE;
                        default:
                                return FALSE;
                }
        }
}


bool object_is_known_splendid(const object_type *o_ptr)
{
	if (object_is_known(o_ptr))
	{
		printf("*** splendidness of known objects not implemented\n");
		return FALSE;
	}
	else
	{
		switch(o_ptr->pseudo)
		{
			case INSCRIP_TRIED:
				/* ??? this should go away */
				return object_is_splendid(o_ptr);
			case INSCRIP_SPLENDID:
				return TRUE;
			default:
				return FALSE;
		}
	}
}

bool object_is_known_bad(const object_type *o_ptr)
{
	if (object_is_known(o_ptr))
	{
		if ((object_is_cursed(o_ptr)) || (o_ptr->pseudo == INSCRIP_UNCURSED))
			return TRUE;
		else
			return FALSE;
	}
	else
	{
		switch(o_ptr->pseudo)
		{
			case INSCRIP_CURSED:
			case INSCRIP_UNCURSED:
			case INSCRIP_WORTHLESS:
			case INSCRIP_TERRIBLE:
				return TRUE;
			default:
				return FALSE;
		}
	}
}

bool object_known_never_cursed(const object_type *o_ptr)
{
	if (object_is_known(o_ptr))
	{
		if (o_ptr->pseudo == INSCRIP_UNCURSED)
			return TRUE;
		else
			return object_is_cursed(o_ptr);
	}
	else
	{
		switch (o_ptr->pseudo)
		{
			case INSCRIP_TRIED:
			case INSCRIP_AVERAGE:
			case INSCRIP_GOOD:
			case INSCRIP_EXCELLENT:
			case INSCRIP_SPLENDID:
				return TRUE;
			default:
				return FALSE;
		}
	}
}

/* ??? compare to squelch_wield_checkable ??? */

bool object_is_safe_to_wield(const object_type *o_ptr)
{
	/* ??? maybe should answer TRUE even if cursed if already being wielded */
	if ((object_known_never_cursed(o_ptr)) || 
	    (o_ptr->pseudo == INSCRIP_UNCURSED))
		return TRUE;
	else
		return FALSE;
}

void object_auto_id(object_type *o_ptr)
{
	if (!object_is_safe_to_wield(o_ptr))
		return;

	if ((o_ptr->tval == TV_RING) || (o_ptr->tval == TV_AMULET))
	{
		if ((!object_aware_p(o_ptr)) && (obvious_kind(o_ptr)))
			object_aware(o_ptr);
		if (object_aware_p(o_ptr))
		{
			/* ??? this is how it should be tested, but generation is annoyingly hard-coded
			object_kind *k_ptr = &k_info[o_ptr->k_idx];
			if ((k_ptr->to_h == 0) && (k_ptr->to_d == 0) && (k_ptr->to_a == 0))
			*/
			/* hack -- assume values of 0 can never be different */
			if ((o_ptr->to_h == 0) && (o_ptr->to_d == 0) && (o_ptr->to_a == 0))
				object_known(o_ptr);
		}
	}
}


void object_flags_all(const object_type *o_ptr, flag_block_type *F)
{
	/* conversion to current method */
	u32b *G = (u32b *)F;
	object_flags(o_ptr, &G[0], &G[1], &G[2]);
}

void object_flags_all_known(const object_type *o_ptr, flag_block_type *F)
{
	/* conversion to current method */
	u32b *G = (u32b *)F;
	object_flags_known(o_ptr, &G[0], &G[1], &G[2]);
}

bool extract_flag(const flag_block_type *F, int flag_idx)
{
	int idx = flag_idx / OBJECT_FLAGS_PER_BLOCK;
	flag_block_type mask = 1 << (flag_idx % OBJECT_FLAGS_PER_BLOCK);
	return ((F[idx] & mask) > 0);
}

bool object_is_melee(const object_type *o_ptr)
{
	switch(o_ptr->tval)
	{
		case TV_SWORD:
		case TV_POLEARM:
		case TV_HAFTED:
		case TV_DIGGING:
			return TRUE;
		default:
			return FALSE;
	}
}

#define MAX_ENCHANT 15
inscrip_type object_quality(const object_type *o_ptr, const flag_block_type *F)
{
	int i;

	/* ??? this is iffy, but needed to avoid listing artifacts as
		possible jewelry types */
	if (extract_flag(F, INSTA_ART))
		return INSCRIP_SPLENDID;

	if (o_ptr != NULL)
	{
		if (object_is_splendid(o_ptr))
			return INSCRIP_SPLENDID;

		if ((o_ptr->to_a > MAX_ENCHANT) || (o_ptr->to_h > MAX_ENCHANT) || (o_ptr->to_d > MAX_ENCHANT))
			return INSCRIP_EXCELLENT;

		/* this routine fails to notice body armor whose hit penalty is better than the base type. */
		/* any armor with +hit or +dam is considered excellent */
		switch(o_ptr->tval)
		{
			case TV_HARD_ARMOR:
			case TV_SOFT_ARMOR:
			case TV_CLOAK:
			case TV_SHIELD:
			case TV_HELM:
			case TV_CROWN:
			case TV_GLOVES:
			case TV_BOOTS:
				if ((o_ptr->to_h > 0) || (o_ptr->to_d > 0))
					return INSCRIP_EXCELLENT;
		}
	}

	for (i = 0; i < N_ELEMENTS(excellent_list); i++)
	{
		if (extract_flag(F, excellent_list[i]))
		{
			return INSCRIP_EXCELLENT;
		}
	}

	if (o_ptr != NULL)
	{
		switch(o_ptr->pseudo)
		{
			case INSCRIP_EXCELLENT:
			case INSCRIP_GOOD:
				return o_ptr->pseudo;
		}

		if ((o_ptr->to_h > 0) || (o_ptr->to_d > 0) || (o_ptr->to_a > 0))
			return INSCRIP_GOOD;
	}

	return INSCRIP_AVERAGE;
}

bool squelch_wield_checkable(const object_type *o_ptr)
{
	if (!cursed_p(o_ptr))
	{
		if (object_known_p(o_ptr))
			return TRUE;
		switch(o_ptr->pseudo)
		{
			case INSCRIP_UNCURSED:
			case INSCRIP_TRIED:
			case INSCRIP_AVERAGE:
			case INSCRIP_GOOD:
			case INSCRIP_EXCELLENT:
			case INSCRIP_SPLENDID:
				return TRUE;
		}
	}
	return FALSE;
}


/* tailor squelching to blows */
int num_blows(const object_type *o_ptr, int str_mod, int dex_mod)
{
	/* ??? should make sure it is a wieldable weapon */
	/* most of this stolen from xtra1.c, should combine??? */

	int str_index, dex_index;
	int str_by_ten, dex_by_ten;
	int div;
	int blows;
	int max_index;


	int extra_blows = 0;
	u32b F[3];

	object_type *c_ptr = &inventory[INVEN_WIELD]; /* current weapon */
	if (c_ptr->k_idx) /* ??? is this the way to check if the slot is not empty? */
	{
		/* need to subtract off value of current weapon, hope not buggy
		   ??? if current weapon pushes stats over the upper limits */
		object_flags_all(c_ptr, F);
		if (extract_flag(F, STR))
			str_mod -= c_ptr->pval;
		if (extract_flag(F, DEX))
			dex_mod -= c_ptr->pval;
	}

	object_flags_all(o_ptr, F);
	if (extract_flag(F, STR))
		str_mod += o_ptr->pval;
	if (extract_flag(F, DEX))
		dex_mod += o_ptr->pval;
	if (extract_flag(F, BLOWS))
		extra_blows += o_ptr->pval;

	/* Enforce a minimum "weight" (tenth pounds) */
	div = ((o_ptr->weight < cp_ptr->min_weight) ? cp_ptr->min_weight : o_ptr->weight);

	/* Get the strength vs weight */
	str_by_ten = p_ptr->stat_use[A_STR];
	if (str_by_ten > 18)
		str_by_ten = 18 + (str_by_ten-18)/10;
	str_by_ten -= 3;
	str_mod += str_by_ten;
	max_index = adj_str_blow_size() - 1;
	if (str_mod > max_index)
		str_mod = max_index;
	if (str_mod < 0)
		str_mod = 0;
	str_index = (adj_str_blow[str_mod] * cp_ptr->att_multiply / div);

	/* Maximal value */
	if (str_index > 11) str_index = 11;

	/* Index by dexterity */
	dex_by_ten = p_ptr->stat_use[A_DEX];
	if (dex_by_ten > 18)
		dex_by_ten = 18 + (dex_by_ten-18)/10;
	dex_by_ten -= 3;
	dex_mod += dex_by_ten;
	max_index = adj_dex_blow_size() - 1;
	if (dex_mod > max_index)
		dex_mod = max_index;
	if (dex_mod < 0)
		dex_mod = 0;
	dex_index = (adj_dex_blow[dex_mod]);

	/* Maximal value */
	if (dex_index > 11) dex_index = 11;

	/* Use the blows table */
	blows = blows_table[str_index][dex_index];

	/* NPP style additional fighter blows should go here */

	/* Maximal value */
	if (blows > cp_ptr->max_attacks)
		blows = cp_ptr->max_attacks;

	/* Add in the "bonus blows" */
	blows += extra_blows;

	/* Require at least one blow */
	if (blows < 1)
		blows = 1;

	return blows;
}

/* EFGchange allow squelching unaware objects */

/* aware flag must be 1 to match legacy savefiles */
#define SQUELCH_AWARE_FLAG	0x01
#define SQUELCH_UNAWARE_FLAG	0x02

typedef struct
{
	s16b idx;
	bool aware;
} squelch_choice;
#endif

/*
 * List of kinds of item, for pseudo-id squelch.
 */
enum
{
#ifdef EFG
	/* EFGchange differentiate squelch into subcategories */
	TYPE_WEAPON_POINTY,
	TYPE_WEAPON_BLUNT,
	TYPE_SHOOTER,
	TYPE_MISSILE_SLING,
	TYPE_MISSILE_BOW,
	TYPE_MISSILE_XBOW,
	TYPE_ARMOR_ROBE,
	TYPE_ARMOR_BODY,
	TYPE_ARMOR_CLOAK,
	TYPE_ARMOR_SHIELD,
	TYPE_ARMOR_HEAD,
	TYPE_ARMOR_HANDS,
	TYPE_ARMOR_FEET,
	TYPE_DIGGER,
	/* EFGchange add lights to quality squelch menus */
	TYPE_LITE,
	/* jewelry should come last because dropdown menu is shortest */
	TYPE_RING,
	TYPE_AMULET,
#else
	TYPE_WEAPON,
	TYPE_SHOOTER,
	TYPE_MISSILE,
	TYPE_ARMOR,
	TYPE_JEWELRY,
	TYPE_DIGGER,
#endif

	TYPE_MAX
};

/*
 * Names of categories.
 */
static const char *type_names[TYPE_MAX] =
{
#ifdef EFG
/* ??? need redundant check for right TYPE order */
/* ??? should combine this into mapping_struct below */
	"Pointy Melee Weapons",
	"Blunt Melee Weapons",
	"Missile weapons",
	/* EFGchange differentiate squelch into subcategories */
	"Shots and Pebbles",
	"Arrows",
	"Bolts",
	"Robes",
	"Body Armor",
	"Cloaks",
	"Shields",
	"Headgear",
	"Handgear",
	"Footgear",
	"Diggers",
	/* EFGchange add lights to quality squelch menus */
	"Lights",
	"Rings",
	"Amulets",
#else
	"Melee weapons",
	"Missile weapons",
	"Ammunition",
	"Armor",
	"Jewelry",
	"Diggers",
#endif
};

#ifdef EFG
/* Mapping of tval -> type */
typedef struct
{
	int squelch_type;
	int tval;
	int min_sval;
	int max_sval;
} mapping_struct;
static mapping_struct type_tvals[] =
{
	/* EFGchange differentiate squelch into subcategories */
	{ TYPE_WEAPON_POINTY,	TV_SWORD,	0,		SV_UNKNOWN },
	{ TYPE_WEAPON_POINTY,	TV_POLEARM,	0,		SV_UNKNOWN },
	{ TYPE_WEAPON_BLUNT,	TV_HAFTED,	0,		SV_UNKNOWN },
	{ TYPE_SHOOTER,		TV_BOW,		0,		SV_UNKNOWN },
	{ TYPE_MISSILE_SLING,	TV_SHOT,	0,		SV_UNKNOWN },
	{ TYPE_MISSILE_BOW,	TV_ARROW,	0,		SV_UNKNOWN },
	{ TYPE_MISSILE_XBOW,	TV_BOLT,	0,		SV_UNKNOWN },
	{ TYPE_ARMOR_ROBE,	TV_SOFT_ARMOR,	SV_ROBE,	SV_ROBE },
	{ TYPE_ARMOR_BODY,	TV_DRAG_ARMOR,	0,		SV_UNKNOWN },
	{ TYPE_ARMOR_BODY,	TV_HARD_ARMOR,	0,		SV_UNKNOWN },
	{ TYPE_ARMOR_BODY,	TV_SOFT_ARMOR,	0,		SV_UNKNOWN },
	{ TYPE_ARMOR_CLOAK,	TV_CLOAK,	0,		SV_UNKNOWN },
	{ TYPE_ARMOR_SHIELD,	TV_SHIELD,	0,		SV_UNKNOWN },
	{ TYPE_ARMOR_HEAD,	TV_HELM,	0,		SV_UNKNOWN },
	{ TYPE_ARMOR_HEAD,	TV_CROWN,	0,		SV_UNKNOWN },
	{ TYPE_ARMOR_HANDS,	TV_GLOVES,	0,		SV_UNKNOWN },
	{ TYPE_ARMOR_FEET,	TV_BOOTS,	0,		SV_UNKNOWN },
	{ TYPE_RING,		TV_RING,	0,		SV_UNKNOWN },
	{ TYPE_AMULET,		TV_AMULET,	0,		SV_UNKNOWN },
	{ TYPE_DIGGER,		TV_DIGGING,	0,		SV_UNKNOWN },
	/* EFGchange add lights to quality squelch menus */
	{ TYPE_LITE,		TV_LITE,	0,		SV_UNKNOWN },
#else
/* Mapping of tval -> type */
static int type_tvals[][2] =
{
	{ TYPE_WEAPON,	TV_SWORD },
	{ TYPE_WEAPON,	TV_POLEARM },
	{ TYPE_WEAPON,	TV_HAFTED },
	{ TYPE_SHOOTER,	TV_BOW },
	{ TYPE_MISSILE, TV_ARROW },
	{ TYPE_MISSILE,	TV_BOLT },
	{ TYPE_MISSILE,	TV_SHOT },
	{ TYPE_ARMOR,	TV_SHIELD },
	{ TYPE_ARMOR,	TV_HELM },
	{ TYPE_ARMOR,	TV_GLOVES },
	{ TYPE_ARMOR,	TV_BOOTS },
	{ TYPE_ARMOR,	TV_DRAG_ARMOR },
	{ TYPE_ARMOR,	TV_HARD_ARMOR },
	{ TYPE_ARMOR,	TV_SOFT_ARMOR },
	{ TYPE_ARMOR,	TV_CLOAK },
	{ TYPE_ARMOR,	TV_CROWN },
	{ TYPE_JEWELRY,	TV_RING },
	{ TYPE_JEWELRY,	TV_AMULET },
	{ TYPE_DIGGER,	TV_DIGGING },
#endif
};

byte squelch_level[TYPE_MAX];
size_t squelch_size = TYPE_MAX;

#ifdef EFG
/* EFGchange generalized squelch */

#define MAX_SQUELCH_LEN 1000
object_type gen_squelch_list[TYPE_MAX][MAX_SQUELCH_LEN];
int	gen_squelch_used[TYPE_MAX];

static int get_squelch_type(const object_type *o_ptr, bool combine_weapons)
{
	int i;
	int num = -1;
	int tval = o_ptr->tval;
	/* if no pointy penalty, do not separate maces from swords */
	if (combine_weapons)
	{
		if ((!(cp_ptr->flags & CF_BLESS_WEAPON)) && (tval == TV_HAFTED))
			tval = TV_SWORD;
	}

        /* Find the appropriate squelch group */
        for (i = 0; i < N_ELEMENTS(type_tvals); i++)
        {
                if ((type_tvals[i].tval == tval) && (type_tvals[i].min_sval <= o_ptr->sval)
			&& (type_tvals[i].max_sval >= o_ptr->sval))
                {
                        num = type_tvals[i].squelch_type;
                        break;
                }
        }
	return num;
}

bool dominates(const object_type *s1, const object_type *s2)
/* IMPORTANT for purposes of dealing with weak pseudo,
   an unknown s1 is considered to have strong psuedo and 
   s2 is considered to have weak pseudo, so a good weapon does NOT dominate itself
   for a char with weak pseudo
*/
{
	int t1 = get_squelch_type(s1, TRUE);
	int t2 = get_squelch_type(s2, TRUE);
	int d1;
	int d2;
	int plus_d1 = s1->to_d;
	int plus_h1 = s1->to_h;
	int plus_a1 = s1->to_a;
	int plus_d2 = s2->to_d;
	int plus_h2 = s2->to_h;
	int plus_a2 = s2->to_a;

/* this is for convenience, not quite perfect */
/* possibly +10 should merit pseudo "VERY_GOOD" and +16 should merit pseudo "EXCELLENT" */
#define BEST_GOOD_BONUS 25

	if (t1 != t2)
		return FALSE;
	if (!object_known_p(s2))
		switch(s2->pseudo)
		{
			case INSCRIP_TRIED:
				/* ??? if !splendid */
				break;
			case INSCRIP_NULL:
			case INSCRIP_SPLENDID:
	        		return FALSE;
		}
	/* ??? what about uncursed s1 */
	if (object_is_cursed(s1) && !object_is_cursed(s2))
		return FALSE;
	if (s2->name1)
	{
		return FALSE;
	}
	if ((s2->name2) || /* ??? what about average or good egos? */
	    ((!(cp_ptr->flags & CF_PSEUDO_ID_HEAVY)) && (!object_is_known(s2)) && (!object_is_known_bad(s2))))
	{
		if (object_is_splendid(s2))
			return FALSE;
		if (object_is_known(s2))
		{
			/* ??? for now, both known requires same ego */
			if (object_is_known(s1))
			{
				if (s1->name2 != s2->name2)
					return FALSE;
			}
			else
			{
				switch(s1->pseudo)
				{
					case INSCRIP_TRIED:
					case INSCRIP_EXCELLENT:
						break;
					default:
						return FALSE;
				}
			}
		}
		else
		{
			if (!object_is_known_bad(s2))
			{
				switch(s1->pseudo)
				{
					case INSCRIP_TRIED:
					case INSCRIP_EXCELLENT:
						break;
					default:
						return FALSE;
				}
			}
		}
	}
	/* it appears that we do not have to multiply by number */
	if (s1->weight > s2->weight)
		return FALSE;
	switch (t1)
	{
	    case TYPE_SHOOTER:
		/* treat all shooters differently in case of class or racial preferences */
		if (s1->sval != s2->sval)
		    return FALSE;
	    case TYPE_DIGGER:
		/* ??? should also compare digging pval */
	    case TYPE_MISSILE_SLING:
	    case TYPE_MISSILE_BOW:
	    case TYPE_MISSILE_XBOW:
	    case TYPE_WEAPON_POINTY:
	    case TYPE_WEAPON_BLUNT:
		if (!object_known_p(s1))
			/* only compare base attributes */
			plus_h1 = plus_h2 = plus_d1 = plus_d2 = 0;
		else if (!object_known_p(s2))
			/* should have pseudo of some sort by here */
			plus_h2 = plus_d2 = BEST_GOOD_BONUS;
		d1 = s1->dd * (s1->ds + 1) + 2 * plus_d1;
		d2 = s2->dd * (s2->ds + 1) + 2 * plus_d2;
		if (plus_h1 < plus_h2)
		    /* ??? should actually calculate damage per round */
		    return FALSE;
		if (d1 < d2)
		    return FALSE;
		/* now assume enchantment to +9 from scrolls */
		if (plus_d1 < 9)
		    d1 = s1->dd * (s1->ds + 1) + 2 * 9;
		if (plus_d2 < 9)
		    d2 = s2->dd * (s2->ds + 1) + 2 * 9;
		if (d1 < d2)
		    return FALSE;
		return TRUE;
		break;

	    case TYPE_ARMOR_BODY:
		if (s1->to_h < s2->to_h)
			/* only visible good and unknown for bodyarmor */
			return FALSE; 
	    case TYPE_ARMOR_ROBE:
	    case TYPE_ARMOR_CLOAK:
	    case TYPE_ARMOR_SHIELD:
	    case TYPE_ARMOR_HEAD:
	    case TYPE_ARMOR_HANDS:
	    case TYPE_ARMOR_FEET:
		if (!object_known_p(s1))
			/* only compare base attributes */
			plus_a1 = plus_a2 = 0;
		else if (!object_known_p(s2))
			/* should know not excellent by here */
			plus_a2 = BEST_GOOD_BONUS;
		if (s1->ac + plus_a1 >= s2->ac + plus_a2)
		    return TRUE;
		break;
		
		
	    case TYPE_LITE:
		/* treat all light types differently */
		if (s1->sval != s2->sval)
		    return FALSE;
		/* squelch by pval, good for eliminating empty torches */
		if (s1->pval < s2->pval)
		    return FALSE;
		/* ??? should check for flags instead */
		break;

	    case TYPE_AMULET:
		/* mega-hack -- this stuff should be generalized to checking flags */
		switch(s1->sval)
		{
		    case SV_AMULET_THE_MAGI:
		    case SV_AMULET_WEAPONMASTERY:
			if ((s1->to_h < s2->to_h) || (s1->to_d < s2->to_d) || (s1->to_a < s2->to_a))
			    return FALSE;
		    case SV_AMULET_SEARCHING:
		    case SV_AMULET_WISDOM:
		    case SV_AMULET_CHARISMA:
		    case SV_AMULET_DEVOTION:
		    case SV_AMULET_TRICKERY:
		    case SV_AMULET_INFRAVISION:
			if (s1->pval < s2->pval)
			    return FALSE;

		    case SV_AMULET_DOOM:
		    case SV_AMULET_TELEPORT:
		    case SV_AMULET_ADORNMENT:
		    case SV_AMULET_SLOW_DIGEST:
		    case SV_AMULET_RESIST_ACID:
		    case SV_AMULET_SUSTENANCE:
		    case SV_AMULET_CARLAMMAS:
		    case SV_AMULET_INGWE:
		    case SV_AMULET_DWARVES:
		    case SV_AMULET_ESP:
		    case SV_AMULET_RESIST:
		    case SV_AMULET_REGEN:
		    case SV_AMULET_ELESSAR:
		    case SV_AMULET_EVENSTAR:
		    case SV_AMULET_RESIST_LIGHTNING:
			if (s1->sval != s2->sval)
			    	return FALSE;
			else
			{
				if (!object_is_known(s1))
					/* this should never happen */
					printf("unknown amulet %d used to dominate\n", s1->sval);
			    		return FALSE;
				if (!object_is_known(s2))
					/* ??? should allow perhaps if s2 is known to be cursed */
			    		return FALSE;
			    	return TRUE;
			}
		    default:
			printf("cannot do domination of amulet %d\n", s1->sval);
		}
		break;

	    case TYPE_RING:
		/* mega-hack -- this stuff should be generalized to checking flags */
		switch(s1->sval)
		{
		    case SV_RING_PROTECTION:
		    case SV_RING_ACID:
		    case SV_RING_FLAMES:
		    case SV_RING_ICE:
		    case SV_RING_LIGHTNING:
			if (s1->to_a < s2->to_a)
			    return FALSE;

		    case SV_RING_ACCURACY:
		    case SV_RING_DAMAGE:
		    case SV_RING_SLAYING:
			if ((s1->to_h < s2->to_h) || (s1->to_d < s2->to_d))
			    return FALSE;

		    case SV_RING_SEARCHING:
		    case SV_RING_STR:
		    case SV_RING_INT:
		    case SV_RING_DEX:
		    case SV_RING_CON:
		    case SV_RING_SPEED:
			if (s1->pval < s2->pval)
			    return FALSE;

		    case SV_RING_BARAHIR:
		    case SV_RING_TULKAS:
		    case SV_RING_NARYA:
		    case SV_RING_NENYA:
		    case SV_RING_VILYA:
		    case SV_RING_POWER:
			/* The above block should also get =rPoison, =con+4, =rFire, =dam+6 etc */
		    case SV_RING_WOE:
		    case SV_RING_AGGRAVATION:
		    case SV_RING_WEAKNESS:
		    case SV_RING_STUPIDITY:
		    case SV_RING_TELEPORTATION:
                    /* xxx */
		    case SV_RING_SLOW_DIGESTION:
		    case SV_RING_FEATHER_FALL:
		    case SV_RING_RESIST_FIRE:
		    case SV_RING_RESIST_COLD:
		    case SV_RING_SUSTAIN_STR:
		    case SV_RING_SUSTAIN_INT:
		    case SV_RING_SUSTAIN_WIS:
		    case SV_RING_SUSTAIN_DEX:
		    case SV_RING_SUSTAIN_CON:
		    case SV_RING_SUSTAIN_CHR:
		    case SV_RING_RESIST_POIS:
		    case SV_RING_FREE_ACTION:
		    case SV_RING_SEE_INVIS:

                        if (s1->sval != s2->sval)
                                return FALSE;
                        else
                        {
                                if (!object_is_known(s1))
				{
                                        /* this should never happen */
                                        printf("unknown ring %d used to dominate\n", s1->sval);
                                        return FALSE;
				}
                                if (!object_is_known(s2))
				{
                                        /* ??? should allow perhaps if s2 is known to be cursed */
                                        return FALSE;
				}
                        }
			return TRUE;

		    default:
			printf("cannot do domination of ring %d\n", s1->sval);
		}
		break;

	    default:
		printf("squelch type %d domination not implemented\n", t1);
		break;
	}

	return FALSE;;
}

void squelch_object(const object_type *o_ptr)
{
	/* get the list */
	int l = get_squelch_type(o_ptr, TRUE);

	if (l < 0)
	{
		printf ("oops type %d is out of bounds in squelch_object\n", l);
		return;
	}
	if (gen_squelch_used[l] < MAX_SQUELCH_LEN)
	{
		/* remove items on the squelch list dominated by new item */
		int i = 0;
		while (i < gen_squelch_used[l])
		{
			if (dominates(o_ptr, &gen_squelch_list[l][i]))
{
				gen_squelch_list[l][i] = gen_squelch_list[l][--gen_squelch_used[l]];
printf("removed item %d from squelch for type %d\n", i, l);
}
			else
				i++;
		}

		/* add the new item to the list */
        	gen_squelch_list[l][gen_squelch_used[l]++] = *o_ptr;
		printf ("squelch list %d now has size %d\n", l, gen_squelch_used[l]);
	}
	else
	{
		/* ??? */
		printf (" should be increasing size of squelch list\n");
	}
}
#endif

/*
 * The different kinds of quality squelch
 */
enum
{
#ifdef EFG
	/* EFGchange new pseudo level SPLENDID */
	SQUELCH_NONE,
	SQUELCH_CURSED,
	SQUELCH_AVERAGE,
	SQUELCH_GOOD_STRONG,
	SQUELCH_BORING,
	SQUELCH_NONEGO,
	SQUELCH_UNSPLENDID,
/* ??? should allow squelching of splendid ammo */
#else
	SQUELCH_NONE,
	SQUELCH_CURSED,
	SQUELCH_AVERAGE,
	SQUELCH_GOOD_STRONG,
	SQUELCH_GOOD_WEAK,
	SQUELCH_ALL,
#endif

	SQUELCH_MAX
};
#ifdef EFG
/* EFGchange add lights to quality squelch menus */
enum
{
	SQUELCH_LITE_NONE,
	SQUELCH_LITE_AVERAGE_TORCHES,
	SQUELCH_LITE_AVERAGE_LANTERNS,
	SQUELCH_LITE_AVERAGE_ALL,
	SQUELCH_LITE_EXCELLENT,

	SQUELCH_LITE_MAX
};

enum
{
	SQUELCH_JEWELRY_NONE,
	SQUELCH_JEWELRY_CURSED_AWARE,
	SQUELCH_JEWELRY_CURSED,

	SQUELCH_JEWELRY_MAX,
};
#endif

/*
 * The names for the various kinds of quality
 */
static const char *quality_names[SQUELCH_MAX] =
{
	"none",							/* SQUELCH_NONE */
	"cursed",						/* SQUELCH_CURSED */
	"average",						/* SQUELCH_AVERAGE */
#ifdef EFG
	/* EFGchange new pseudo level SPLENDID */
	"good",					/* SQUELCH_GOOD_STRONG */
	"average (keep worthless)",		/* SQUELCH_BORING */
	"good    (keep worthless)",		/* SQUELCH_NONEGO */
	"all but splendid",			/* SQUELCH_UNSPLENDID */
};

/* EFGchange add lights to quality squelch menus */
static const char *quality_lights[SQUELCH_LITE_MAX] =
{
	"none",					/* SQUELCH_LITE_NONE */
	"average torches",			/* SQUELCH_LITE_AVERAGE_TORCHES */
	"average lanterns",			/* SQUELCH_LITE_AVERAGE_LANTERNS */
	"all average lights",			/* SQUELCH_LITE_AVERAGE_ALL */
	"all but artifacts",			/* SQUELCH_LITE_EXCELLENT */
};
static const char *quality_jewelry[SQUELCH_JEWELRY_MAX] =
{
	"none",					/* SQUELCH_JEWELRY_NONE */
	"cursed when aware",			/* SQUELCH_JEWELRY_CURSED_AWARE */
	"all cursed",				/* SQUELCH_JEWELRY_CURSED */
};
#else
        "good (strong pseudo-ID)",              /* SQUELCH_GOOD_STRONG */
        "good (weak pseudo-ID)",                /* SQUELCH_GOOD_WEAK */
        "everything except artifacts",  /* SQUELCH_ALL */
};
#endif


/* Structure to describe tval/description pairings. */
typedef struct
{
	int tval;
	const char *desc;
} tval_desc;

/* Categories for sval-dependent squelch. */
static tval_desc sval_dependent[] =
{
	{ TV_STAFF,			"Staffs" },
	{ TV_WAND,			"Wands" },
	{ TV_ROD,			"Rods" },
	{ TV_SCROLL,		"Scrolls" },
	{ TV_POTION,		"Potions" },
	{ TV_RING,			"Rings" },
	{ TV_AMULET,		"Amulets" },
	{ TV_FOOD,			"Food" },
	{ TV_MAGIC_BOOK,	"Magic books" },
	{ TV_PRAYER_BOOK,	"Prayer books" },
	{ TV_SPIKE,			"Spikes" },
#ifdef EFG
	/* EFGchange squelch oil like spikes */
	/* ??? we need some sort of misc option */
	{ TV_FLASK,			"Oil" },
	{ TV_DRAG_ARMOR,		"Dragon Armor" },
#else
	{ TV_LITE,			"Lights" },
#endif
};


/*** Autoinscription stuff ***/

/*
 * This code needs documenting.
 */
int get_autoinscription_index(s16b k_idx)
{
	int i;

	for (i = 0; i < inscriptions_count; i++)
	{
		if (k_idx == inscriptions[i].kind_idx)
			return i;
	}

	return -1;
}

/*
 * DOCUMENT ME!
 */
const char *get_autoinscription(s16b kind_idx)
{
	int i;

	for (i = 0; i < inscriptions_count; i++)
	{
		if (kind_idx == inscriptions[i].kind_idx)
			return quark_str(inscriptions[i].inscription_idx);
	}

	return 0;
}

/* Put the autoinscription on an object */
int apply_autoinscription(object_type *o_ptr)
{
	char o_name[80];
	cptr note = get_autoinscription(o_ptr->k_idx);
	cptr existing_inscription = quark_str(o_ptr->note);

	/* Don't inscribe unaware objects */
	if (!note || !object_aware_p(o_ptr))
		return 0;

	/* Don't re-inscribe if it's already correctly inscribed */
	if (existing_inscription && streq(note, existing_inscription))
		return 0;

	/* Get an object description */
	object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

	if (note[0] != 0)
		o_ptr->note = quark_add(note);
	else
		o_ptr->note = 0;

	msg_format("You autoinscribe %s.", o_name);

	return 1;
}


int remove_autoinscription(s16b kind)
{
	int i = get_autoinscription_index(kind);

	/* It's not here. */
	if (i == -1) return 0;

	while (i < inscriptions_count - 1)
	{
		inscriptions[i] = inscriptions[i+1];
		i++;
	}

	inscriptions_count--;

	return 1;
}


int add_autoinscription(s16b kind, cptr inscription)
{
	int index;

	/* Paranoia */
	if (kind == 0) return 0;

	/* If there's no inscription, remove it */
	if (!inscription || (inscription[0] == 0))
		return remove_autoinscription(kind);

	index = get_autoinscription_index(kind);

	if (index == -1)
		index = inscriptions_count;

	if (index >= AUTOINSCRIPTIONS_MAX)
	{
		msg_format("This inscription (%s) cannot be added because the inscription array is full!", inscription);
		return 0;
	}

	inscriptions[index].kind_idx = kind;
	inscriptions[index].inscription_idx = quark_add(inscription);

	/* Only increment count if inscription added to end of array */
	if (index == inscriptions_count)
		inscriptions_count++;

	return 1;
}


void autoinscribe_ground(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;
	s16b this_o_idx, next_o_idx = 0;

	/* Scan the pile of objects */
	for (this_o_idx = cave_o_idx[py][px]; this_o_idx; this_o_idx = next_o_idx)
	{
		/* Get the next object */
		next_o_idx = o_list[this_o_idx].next_o_idx;

		/* Apply an autoinscription */
		apply_autoinscription(&o_list[this_o_idx]);
	}
}

void autoinscribe_pack(void)
{
	int i;

	/* Cycle through the inventory */
	for (i = INVEN_PACK; i > 0; i--)
	{
		/* Skip empty items */
		if (!inventory[i].k_idx) continue;

		/* Apply the inscription */
		apply_autoinscription(&inventory[i]);
	}

	return;
}




/*** Squelch code ***/

/*
 * Determines whether a tval is eligable for sval-squelch.
 */
bool squelchable_tval(int tval)
{
	size_t i;

	/* Only squelch if the tval's allowed */
	for (i = 0; i < N_ELEMENTS(sval_dependent); i++)
	{
		if (tval == sval_dependent[i].tval)
			return TRUE;
	}

	return FALSE;
}


/*
 * Determines if an object is eligable for squelching.
 */
bool squelch_item_ok(const object_type *o_ptr)
{
	size_t i;
	int num = -1;

	object_kind *k_ptr = &k_info[o_ptr->k_idx];
	bool fullid = object_known_p(o_ptr);
	bool sensed = (o_ptr->ident & IDENT_SENSE) || fullid;
	byte feel   = fullid ? value_check_aux1(o_ptr) : o_ptr->pseudo;
#ifdef EFG
	if (check_for_inscrip(o_ptr, "!k") || check_for_inscrip(o_ptr, "!*"))
		return FALSE;
	(void) i; /* avoid a compiler warning */
	/* EFGchange generalized squelch */
	int idx;
	int l = get_squelch_type(o_ptr, TRUE); /* combine for domination */
	for (idx = 0; idx < gen_squelch_used[l]; idx++)
		if (dominates(&gen_squelch_list[l][idx], o_ptr))
			return TRUE;
	l = get_squelch_type(o_ptr, FALSE); /* separate for quality squelch */

	/* EFGchange treat big 3 weapons as excellent when squelching */
	switch(o_ptr->tval)
	{
		/* uncommenting means weapons with base damage >= 15 should be considered excellent
		*/
        	case TV_SWORD:
        	case TV_POLEARM:
        	case TV_HAFTED:
			if (o_ptr->dd * o_ptr->ds < 30)
				break;
			if ((feel == INSCRIP_BROKEN) || (feel == INSCRIP_CURSED))
				feel = INSCRIP_WORTHLESS;
			else if ((feel == INSCRIP_WORTHLESS) || (feel == INSCRIP_TERRIBLE))
				feel = feel;
			else if ((feel == INSCRIP_AVERAGE) || (feel == INSCRIP_GOOD))
				/* ??? should we allow uncursed? */
				feel = INSCRIP_EXCELLENT;
	}
#endif


	/* Don't squelch artifacts */
#ifdef EFG
/* ??? why not?  If squelching non-SPLENDID shields, Celegorm is considered boring */
/* ??? Need to mark preserved artifacts not to be regenerated after pseudoed or tried on */
#endif
	if (artifact_p(o_ptr)) return FALSE;

#ifdef EFG
	/* ??? move this up to the top? */
#endif
	/* Don't squelch stuff inscribed not to be destroyed (!k) */
	if (check_for_inscrip(o_ptr, "!k") || check_for_inscrip(o_ptr, "!*"))
	{
		return FALSE;
	}

	/* Auto-squelch dead chests */
	if (o_ptr->tval == TV_CHEST && o_ptr->pval == 0)
		return TRUE;

	/* Do squelching by sval, if we 'know' the flavour. */
#ifdef EFG
/* ???  awareness should cancel SQUELCH_UNAWARE, set SQUELCH_AWARE */
/* ???  buying squelched item should be inscribed !d!k */
/* ??? what is this k_ptr->flavor of 0 ? */
	if (squelchable_tval(k_info[o_ptr->k_idx].tval))
	{
		if ((k_ptr->squelch & SQUELCH_AWARE_FLAG) && (k_ptr->aware))
			return TRUE;
		if ((k_ptr->squelch & SQUELCH_UNAWARE_FLAG) && (!k_ptr->aware))
			return TRUE;
	}
#else
	if (k_ptr->squelch && (k_ptr->flavor == 0 || k_ptr->aware))
	{
		if (squelchable_tval(k_info[o_ptr->k_idx].tval))
			return TRUE;
	}
#endif


#ifdef EFG
	/* EFGchange remember when you have "tried" on a wieldable */
#else
	/* Don't check pseudo-ID for nonsensed things */
	if (!sensed) return FALSE;
#endif



#ifdef EFG
	num = get_squelch_type(o_ptr, FALSE);
#else
	/* Find the appropriate squelch group */
	for (i = 0; i < N_ELEMENTS(type_tvals); i++)
	{
		if (type_tvals[i][1] == o_ptr->tval)
		{
			num = type_tvals[i][0];
			break;
		}
	}
#endif

	/* Never squelched */
	if (num == -1)
		return FALSE;


#ifdef EFG
	/* EFGchange new pseudo level SPLENDID */
	if (feel == INSCRIP_TRIED)
	{
		sensed = TRUE;
		if ((o_ptr->tval != TV_LITE) && (o_ptr->tval != TV_RING) && (o_ptr->tval != TV_AMULET) &&
		    (squelch_level[num] == SQUELCH_UNSPLENDID) && (!object_is_splendid(o_ptr)))
			/* if we have tried it on, we can be sure it is not splendid */
			/* ??? only problem is we can squelch artifacts, might not be a bug */
			feel = INSCRIP_GOOD;
	}


	if (o_ptr->tval == TV_LITE) switch (squelch_level[num])
	{
		case SQUELCH_LITE_AVERAGE_TORCHES:
		{
			if ((o_ptr->sval == SV_LITE_TORCH) && (feel == INSCRIP_AVERAGE))
				return TRUE;
			break;
		}

		case SQUELCH_LITE_AVERAGE_LANTERNS:
		{
			if ((o_ptr->sval == SV_LITE_LANTERN) && (feel == INSCRIP_AVERAGE))
				return TRUE;
			break;
		}

		case SQUELCH_LITE_AVERAGE_ALL:
		{
			if (feel == INSCRIP_AVERAGE)
				return TRUE;
			break;
		}

		case SQUELCH_LITE_EXCELLENT:
		{
			if ((feel == INSCRIP_AVERAGE) ||
			    (feel == INSCRIP_SPLENDID && !o_ptr->name1) ||
			    (feel == INSCRIP_EXCELLENT) || (feel == INSCRIP_WORTHLESS))
				return TRUE;
		}
	}
	else if ((o_ptr->tval == TV_RING) || (o_ptr->tval == TV_AMULET)) switch (squelch_level[num])
	{
		case SQUELCH_JEWELRY_CURSED_AWARE:
			if (!object_aware_p(o_ptr))
				break;
			if ((extract_flag(&k_info[o_ptr->k_idx].flags1, LIGHT_CURSE)) ||
			    (o_ptr->pseudo == INSCRIP_UNCURSED))
				return TRUE;
		case SQUELCH_JEWELRY_CURSED:
			if ((feel == INSCRIP_BROKEN) || (feel == INSCRIP_TERRIBLE) ||
			    (feel == INSCRIP_UNCURSED) ||
			    (feel == INSCRIP_WORTHLESS) || (feel == INSCRIP_CURSED))
				return TRUE;
			break;
	}
	else if (!sensed) return FALSE;
	else switch (squelch_level[num])
#else
	/* Get result based on the feeling and the squelch_level */
	switch (squelch_level[num])
#endif
	{
		case SQUELCH_CURSED:
		{
			if ((feel == INSCRIP_BROKEN) || (feel == INSCRIP_TERRIBLE) ||
			    (feel == INSCRIP_UNCURSED) ||
			    (feel == INSCRIP_WORTHLESS) || (feel == INSCRIP_CURSED))
			{
				return TRUE;
			}

			break;
		}

		case SQUELCH_AVERAGE:
		{
			if ((feel == INSCRIP_BROKEN) || (feel == INSCRIP_TERRIBLE) ||
#ifdef EFG
			    /* EFGchange treat UNCURSED same as CURSED for squelch purposes */
			    (feel == INSCRIP_UNCURSED) ||
#endif
			    (feel == INSCRIP_WORTHLESS) || (feel == INSCRIP_CURSED) ||
			    (feel == INSCRIP_AVERAGE))
			{
				return TRUE;
			}

			break;
		}

#ifdef EFG
/* ??? BORING has to go, alternate method to specify each possibility */
		case SQUELCH_BORING:
		{
			if ((feel == INSCRIP_BROKEN) ||
			    (((fullid) || (cp_ptr->flags & CF_PSEUDO_ID_HEAVY)) &&
			     ((feel == INSCRIP_CURSED) || (feel == INSCRIP_UNCURSED) ||
			      (feel == INSCRIP_AVERAGE))))
			{
				return TRUE;
			}

			break;
		}

		case SQUELCH_NONEGO:
		{
			if ((feel == INSCRIP_BROKEN) ||
			    (((fullid) || (cp_ptr->flags & CF_PSEUDO_ID_HEAVY)) &&
			     ((feel == INSCRIP_CURSED) || (feel == INSCRIP_UNCURSED) ||
			      (feel == INSCRIP_AVERAGE) || (feel == INSCRIP_GOOD))))
			{
				return TRUE;
			}

			break;
		}

		/* EFGchange new pseudo level SPLENDID */
		case SQUELCH_UNSPLENDID:
		{
			if ((feel == INSCRIP_BROKEN) || (feel == INSCRIP_TERRIBLE) ||
			    (feel == INSCRIP_CURSED) || (feel == INSCRIP_UNCURSED) ||
			    (feel == INSCRIP_AVERAGE) || (feel == INSCRIP_GOOD) ||
			    (feel == INSCRIP_EXCELLENT) || (feel == INSCRIP_WORTHLESS))
			{
				return TRUE;
			}

			break;
		}
#else
		case SQUELCH_GOOD_WEAK:
		{
			if ((feel == INSCRIP_BROKEN) || (feel == INSCRIP_TERRIBLE) ||
			    (feel == INSCRIP_WORTHLESS) || (feel == INSCRIP_CURSED) ||
			    (feel == INSCRIP_AVERAGE) || (feel == INSCRIP_GOOD))
			{
				return TRUE;
			}

			break;
		}
#endif

		case SQUELCH_GOOD_STRONG:
		{
			if ((feel == INSCRIP_BROKEN) || (feel == INSCRIP_TERRIBLE) ||
			    (feel == INSCRIP_WORTHLESS) || (feel == INSCRIP_CURSED) ||
			    (feel == INSCRIP_AVERAGE) ||
#ifdef EFG
			    /* EFGchange treat UNCURSED same as CURSED for squelch purposes */
			    (feel == INSCRIP_UNCURSED) ||
#endif
			    ((feel == INSCRIP_GOOD) &&
			     ((fullid) || (cp_ptr->flags & CF_PSEUDO_ID_HEAVY))))
			{
				return TRUE;
			}

			break;
		}

#ifdef EFG
#else
		case SQUELCH_ALL:
		{
			return TRUE;
			break;
		}
#endif
	}

	/* Failure */
	return FALSE;
}


/* 
 * Returns TRUE if an item should be hidden due to the player's
 * current settings.
 */
bool squelch_hide_item(object_type *o_ptr)
{
	return (hide_squelchable ? squelch_item_ok(o_ptr) : FALSE);
}


/*
 * Destroy all {squelch}able items.
 *
 * Imported, with thanks, from Ey... much cleaner than the original.
 */
void squelch_items(void)
{
	int floor_list[MAX_FLOOR_STACK];
	int floor_num, n;
	int count = 0;

	object_type *o_ptr;

	/* Set the hook and scan the floor */
	item_tester_hook = squelch_item_ok;
	(void)scan_floor(floor_list, &floor_num, p_ptr->py, p_ptr->px, 0x01);

	if (floor_num)
	{
		for (n = 0; n < floor_num; n++)
		{
			o_ptr = &o_list[floor_list[n]];

			/* Avoid artifacts */
			if (artifact_p(o_ptr)) continue;

			if (item_tester_okay(o_ptr))
			{
				/* Destroy item */
				floor_item_increase(floor_list[n], -o_ptr->number);
				floor_item_optimize(floor_list[n]);
				count++;
			}
		}
	}

	/* Scan through the slots backwards */
	for (n = INVEN_PACK - 1; n >= 0; n--)
	{
		o_ptr = &inventory[n];

		/* Skip non-objects and artifacts */
		if (!o_ptr->k_idx) continue;
		if (artifact_p(o_ptr)) continue;

		if (item_tester_okay(o_ptr))
		{
			/* Destroy item */
			inven_item_increase(n, -o_ptr->number);
			inven_item_optimize(n);
			count++;
		}
	}

	item_tester_hook = NULL;

	/* Mention casualties */
	if (count > 0)
	{
		message_format(MSG_DESTROY, 0, "%d item%s squelched.",
		               count, ((count > 1) ? "s" : ""));

		/* Combine/reorder the pack */
		p_ptr->notice |= (PN_COMBINE | PN_REORDER);
	}
}


/*
 * Drop all {squelch}able items.
 */
void squelch_drop(void)
{
	int n;

	/* Scan through the slots backwards */
	for (n = INVEN_PACK - 1; n >= 0; n--)
	{
		object_type *o_ptr = &inventory[n];

		/* Skip non-objects and unsquelchable objects */
		if (!o_ptr->k_idx) continue;
		if (!squelch_item_ok(o_ptr)) continue;

		/* Check for !d (no drop) inscription */
		if (!check_for_inscrip(o_ptr, "!d") && !check_for_inscrip(o_ptr, "!*"))
		{
			/* We're allowed to drop it. */
			inven_drop(n, o_ptr->number);
		}
	}

	/* Combine/reorder the pack */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);
}



/*** Quality-squelch menu ***/
#ifdef EFG
/* EFGchange add lights to quality squelch menus */
/* ??? need to redo submenus to do this consistently */
/* We need to keep track for different entries in the lights submenu. */
static bool doing_lights = FALSE;
static bool doing_jewelry = FALSE;
#endif

/*
 * Display an entry in the menu.
 */
static void quality_display(menu_type *menu, int oid, bool cursor, int row, int col, int width)
{
	const char *name = type_names[oid];

	byte level = squelch_level[oid];
	const char *level_name = quality_names[level];
#ifdef EFG
	/* EFGchange add lights to quality squelch menus */
	if (oid == TYPE_LITE)
	{
		level_name = quality_lights[level];
	}
	if ((oid == TYPE_RING) || (oid == TYPE_AMULET))
	{
		level_name = quality_jewelry[level];
	}
	if (cursor)
	{
		doing_lights = (oid == TYPE_LITE);
		doing_jewelry = (oid == TYPE_AMULET) || (oid == TYPE_RING);
	}
#endif

	byte attr = (cursor ? TERM_L_BLUE : TERM_WHITE);


	c_put_str(attr, format("%-20s : %s", name, level_name), row, col);
}


/*
 * Display the quality squelch subtypes.
 */
static void quality_subdisplay(menu_type *menu, int oid, bool cursor, int row, int col, int width)
{
	const char *name = quality_names[oid];
#ifdef EFG
	/* EFGchange add lights to quality squelch menus */
	if (doing_lights)
	{
		name = quality_lights[oid];
	}
	if (doing_jewelry) /* ??? these are exclusive */
	{
		name = quality_jewelry[oid];
	}
#endif
	byte attr = (cursor ? TERM_L_BLUE : TERM_WHITE);

	c_put_str(attr, name, row, col);
}

/*
 * Handle "Enter".  :(
 */
static bool quality_subaction(char cmd, void *db, int oid)
{
	return TRUE;
}


/*
 * Handle keypresses.
 */
static bool quality_action(char cmd, void *db, int oid)
{
	menu_type menu;
	menu_iter menu_f = { 0, 0, 0, quality_subdisplay, quality_subaction };
	region area = { 24, 5, 26, SQUELCH_MAX };
	event_type evt;
	int cursor;

	/* Display at the right point */
	area.row += oid;
	cursor = squelch_level[oid];

	/* Save */
	screen_save();

	/* Run menu */
	WIPE(&menu, menu);
	menu.cmd_keys = "\n\r";
	menu.count = SQUELCH_MAX;
#ifdef EFG
	/* EFGchange add lights to quality squelch menus */
	if (oid == TYPE_LITE)
		menu.count = area.page_rows = SQUELCH_LITE_MAX;
	if ((oid == TYPE_RING) || (oid == TYPE_AMULET))
		menu.count = area.page_rows = SQUELCH_JEWELRY_MAX;
#else
	if (oid == TYPE_JEWELRY)
		menu.count = area.page_rows = SQUELCH_CURSED + 1;
#endif

	menu_init2(&menu, find_menu_skin(MN_SCROLL), &menu_f, &area);
	window_make(area.col - 2, area.row - 1, area.col + area.width + 2, area.row + area.page_rows);

	evt = menu_select(&menu, &cursor, 0);

	/* Set the new value appropriately */
	if (evt.key != ESCAPE && evt.type != EVT_BACK)
		squelch_level[oid] = cursor;

	/* Load and finish */
	screen_load();
	return TRUE;
}

/*
 * Display quality squelch menu.
 */
static void quality_menu(void *unused, const char *also_unused)
{
	menu_type menu;
	menu_iter menu_f = { 0, 0, 0, quality_display, quality_action };
	region area = { 1, 5, -1, -1 };
	event_type evt = EVENT_EMPTY;
	int cursor = 0;

	/* Save screen */
	screen_save();
	clear_from(0);

	/* Help text */
	prt("Quality squelch menu", 0, 0);

	Term_gotoxy(1, 1);
	text_out_to_screen(TERM_L_RED, "Use the movement keys to navigate, and Enter to change settings.");

	/* Set up the menu */
	WIPE(&menu, menu);
	menu.cmd_keys = " \n\r";
	menu.count = TYPE_MAX;
	menu_init2(&menu, find_menu_skin(MN_SCROLL), &menu_f, &area);

	/* Select an entry */
	while (evt.key != ESCAPE)
		evt = menu_select(&menu, &cursor, 0);

	/* Load screen */
	screen_load();
	return;
}

#ifdef EFG

/*** Sval-dependent menu ***/

/*
 * Display an entry on the sval menu
 */
static void gen_squelch_display(menu_type *menu, int oid, bool cursor, int row, int col, int width)
{
	const object_type *o_list = menu->menu_data;
	const object_type *o_ptr = &o_list[oid];

	byte attr = (cursor ? TERM_L_BLUE : TERM_WHITE);


	char o_name[80];
        object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

	/* Print it */
	c_put_str(attr, format("[ ] %s", o_name), row, col);
/*
	if (((k_info[idx].squelch & SQUELCH_AWARE_FLAG) && choice[oid].aware) ||
	    ((k_info[idx].squelch & SQUELCH_UNAWARE_FLAG) && !choice[oid].aware))
		c_put_str(TERM_L_RED, "*", row, col + 1);
*/
}
#endif


/*** Sval-dependent menu ***/

/*
 * Display an entry on the sval menu
 */
static void sval_display(menu_type *menu, int oid, bool cursor, int row, int col, int width)
{
	char buf[80];
#ifdef EFG
	const squelch_choice *choice = menu->menu_data;
	int idx = choice[oid].idx;
#else
	const u16b *choice = menu->menu_data;
	int idx = choice[oid];
#endif

	byte attr = (cursor ? TERM_L_BLUE : TERM_WHITE);


	/* Acquire the "name" of object "i" */
#ifdef EFG
	object_kind_name(buf, sizeof(buf), idx, choice[oid].aware);
	if (choice[oid].aware)
	{
		if (!k_info[idx].aware)
		{
                	int l = strlen(buf);
                	if (l + 10 < sizeof(buf))
                        	sprintf (&buf[l], " (unaware)");
		}
	}
	else
	{
		if (k_info[idx].tried)
		{
                	int l = strlen(buf);
                	if (l + 8 < sizeof(buf))
                        	sprintf (&buf[l], " (tried)");
		}
	}
#else
	object_kind_name(buf, sizeof(buf), idx, TRUE);
#endif

	/* Print it */
	c_put_str(attr, format("[ ] %s", buf), row, col);
#ifdef EFG
	if (((k_info[idx].squelch & SQUELCH_AWARE_FLAG) && choice[oid].aware) ||
	    ((k_info[idx].squelch & SQUELCH_UNAWARE_FLAG) && !choice[oid].aware))
#else
	if (k_info[idx].squelch)
#endif
		c_put_str(TERM_L_RED, "*", row, col + 1);
}

/*
 * Deal with events on the sval menu
 */
static bool sval_action(char cmd, void *db, int oid)
{
#ifdef EFG
/* ??? are object_kinds u16b ro s16b? */
	const squelch_choice *choice = db;
#else
	u16b *choice = db;
#endif

	/* Toggle */
	if (cmd == '\n' || cmd == '\r')
	{
#ifdef EFG
		int idx = choice[oid].idx;
		if (choice[oid].aware)
			k_info[idx].squelch ^= SQUELCH_AWARE_FLAG;
		else
			k_info[idx].squelch ^= SQUELCH_UNAWARE_FLAG;
#else
		int idx = choice[oid];
		k_info[idx].squelch = !k_info[idx].squelch;
#endif

		return TRUE;
	}

	return FALSE;
}

#ifdef EFG

/*
 * Deal with events on the sval menu
 */
static bool gen_squelch_action(char cmd, void *db, int oid)
{

	/* Toggle */
	if (cmd == '\n' || cmd == '\r')
	{
		return TRUE;
	}

	return FALSE;
}


/* EFGchange sort by name in some squelch menus */
static void ang_sort_swap_hook_squelch_choices(void *u, void *v, int a, int b)
{
        squelch_choice temp;
        squelch_choice *x = (squelch_choice*) u;
        (void)v; /* unused */
        temp = x[a];
        x[a] = x[b];
        x[b] = temp;
}

static bool ang_sort_comp_hook_squelch_choices(const void *u, const void *v, int a, int b)
{
        char bufa[80];
        char bufb[80];
        squelch_choice *x = (squelch_choice*) u;
        (void)v; /* unused */

	if (x[a].aware && !x[b].aware)
		return TRUE;
	if (!x[a].aware && x[b].aware)
		return FALSE;
	object_kind_name(bufa, sizeof(bufa), x[a].idx, x[a].aware);
	object_kind_name(bufb, sizeof(bufb), x[b].idx, x[b].aware);

        return strcmp(bufa, bufb) <= 0;
        /* the = is crucial, inf loop in sort if use < rather than <= */
}


/*
 * Display list of generalized squelched objects of particular tval.
 */
static bool gen_squelch_menu(int tval_ty, const char *desc)
{
	menu_type menu;
	menu_iter menu_f = { 0, 0, 0, gen_squelch_display, gen_squelch_action };
	region area = { 1, 5, -1, -1 };
	event_type evt = { EVT_NONE, 0, 0, 0, 0 };
	int cursor = 0;

	/*
	int num = 0;
	size_t i;
	*/


	/* EFGchange sort by name in some squelch menus */
/* ??? not clear how to sort */
	/*
			ang_sort_comp = ang_sort_comp_hook_squelch_choices;
			ang_sort_swap = ang_sort_swap_hook_squelch_choices;
                        ang_sort((void*)choice, NULL, num);
	*/

	/* Save the screen and clear it */
	screen_save();
	clear_from(0);

	/* Help text */

	/* Output to the screen */
	text_out_hook = text_out_to_screen;

	/* Indent output */
	text_out_indent = 1;
	text_out_wrap = 79;
	Term_gotoxy(1, 0);

	/* Display some helpful information */
	text_out("Use the ");
	text_out_c(TERM_L_GREEN, "movement keys");
	text_out(" to scroll the list or ");
	text_out_c(TERM_L_GREEN, "ESC");
	text_out(" to return to the previous menu.  ");
	text_out_c(TERM_L_BLUE, "Enter");
	text_out(" toggles the current setting.");

	text_out_indent = 0;

	/* Set up the menu */
	WIPE(&menu, menu);
	menu.cmd_keys = " \n\r";
	menu.count = gen_squelch_used[tval_ty];
	menu.menu_data = gen_squelch_list[tval_ty];
	menu_init2(&menu, find_menu_skin(MN_SCROLL), &menu_f, &area);

	/* Select an entry */
	while (evt.key != ESCAPE)
		evt = menu_select(&menu, &cursor, 0);

	/* Load screen */
	screen_load();
	return TRUE;
}
#endif

/*
 * Display list of svals to be squelched.
 */
static bool sval_menu(int tval, const char *desc)
{
	menu_type menu;
	menu_iter menu_f = { 0, 0, 0, sval_display, sval_action };
	region area = { 1, 5, -1, -1 };
	event_type evt = { EVT_NONE, 0, 0, 0, 0 };
	int cursor = 0;

	int num = 0;
	size_t i;

#ifdef EFG
	squelch_choice *choice;

	/* Create the array */
	C_MAKE(choice, 2 * z_info->k_max, u16b); /* two options possible per kind */
#else
	u16b *choice;


	/* Create the array */
	C_MAKE(choice, z_info->k_max, u16b);
#endif

	/* Iterate over all possible object kinds, finding ones which can be squelched */
	for (i = 1; i < z_info->k_max; i++)
	{
		object_kind *k_ptr = &k_info[i];

		/* Skip empty objects, unseen objects, and incorrect tvals */
		if (!k_ptr->name) continue;
#ifdef EFG
		/* EFGchange allow squelch of unaware objects */
		if (k_ptr->tval != tval) continue;
		/* EFGchange allow squelching flavors without everseen */
		/* if (k_ptr->everseen) */
		if (!(k_ptr->flags3 & TR3_INSTA_ART))
		{
			choice[num].idx = i;
			choice[num].aware = TRUE;
			num++;
		}
		if (!k_ptr->aware)
		{
			choice[num].idx = i;
			choice[num].aware = FALSE;
			num++;
		}
#else
		if (!k_ptr->everseen) continue;
		if (k_ptr->tval != tval) continue;

		/* Add this item to our possibles list */
		choice[num++] = i;
#endif
	}

	/* Return here if there are no objects */
	if (!num)
	{
		FREE(choice);
		return FALSE;
	}

#ifdef EFG
	/* EFGchange sort by name in some squelch menus */
        switch(tval)
        {
		case TV_LITE:
                case TV_MAGIC_BOOK:
                case TV_PRAYER_BOOK:
                case TV_DRAG_ARMOR:
                        /* leave sorted by sval */
                        break;
                default:
                        /* sort by name */
			ang_sort_comp = ang_sort_comp_hook_squelch_choices;
			ang_sort_swap = ang_sort_swap_hook_squelch_choices;
                        ang_sort((void*)choice, NULL, num);
        }
#endif

	/* Save the screen and clear it */
	screen_save();
	clear_from(0);

	/* Help text */

	/* Output to the screen */
	text_out_hook = text_out_to_screen;

	/* Indent output */
	text_out_indent = 1;
	text_out_wrap = 79;
	Term_gotoxy(1, 0);

	/* Display some helpful information */
	text_out("Use the ");
	text_out_c(TERM_L_GREEN, "movement keys");
	text_out(" to scroll the list or ");
	text_out_c(TERM_L_GREEN, "ESC");
	text_out(" to return to the previous menu.  ");
	text_out_c(TERM_L_BLUE, "Enter");
	text_out(" toggles the current setting.");

	text_out_indent = 0;

	/* Set up the menu */
	WIPE(&menu, menu);
	menu.cmd_keys = " \n\r";
	menu.count = num;
	menu.menu_data = choice;
	menu_init2(&menu, find_menu_skin(MN_SCROLL), &menu_f, &area);

	/* Select an entry */
	while (evt.key != ESCAPE)
		evt = menu_select(&menu, &cursor, 0);

	/* Free memory */
	FREE(choice);

	/* Load screen */
	screen_load();
	return TRUE;
}


/* Returns TRUE if there's anything to display a menu of */
static bool seen_tval(int tval)
{
	int i;

	for (i = 1; i < z_info->k_max; i++)
	{
		object_kind *k_ptr = &k_info[i];

		/* Skip empty objects, unseen objects, and incorrect tvals */
		if (!k_ptr->name) continue;
		if (!k_ptr->everseen) continue;
		if (k_ptr->tval != tval) continue;

		 return TRUE;
	}


	return FALSE;
}


/* Extra options on the "item options" menu */
struct
{
	char tag;
	char *name;
	void (*action)(void *unused, const char *also_unused);
} extra_item_options[] =
{
	{ 'Q', "Quality squelching options", quality_menu },
	{ '{', "Autoinscription setup", do_cmd_knowledge_objects },
};

static char tag_options_item(menu_type *menu, int oid)
{
	size_t line = (size_t) oid;

	if (line < N_ELEMENTS(sval_dependent))
		return I2A(oid);

	/* Separator - blank line. */
	if (line == N_ELEMENTS(sval_dependent))
		return 0;

	line = line - N_ELEMENTS(sval_dependent) - 1;

	if (line < N_ELEMENTS(extra_item_options))
		return extra_item_options[line].tag;

	return 0;
}

static int valid_options_item(menu_type *menu, int oid)
{
	size_t line = (size_t) oid;

	if (line < N_ELEMENTS(sval_dependent))
		return 1;

	/* Separator - blank line. */
	if (line == N_ELEMENTS(sval_dependent))
		return 0;

	line = line - N_ELEMENTS(sval_dependent) - 1;

	if (line < N_ELEMENTS(extra_item_options))
		return 1;

	return 0;
}

static void display_options_item(menu_type *menu, int oid, bool cursor, int row, int col, int width)
{
	size_t line = (size_t) oid;

	/* First section of menu - the svals */
	if (line < N_ELEMENTS(sval_dependent))
	{
		bool known = seen_tval(sval_dependent[line].tval);
		byte attr = curs_attrs[known ? CURS_KNOWN: CURS_UNKNOWN][(int)cursor];

		c_prt(attr, sval_dependent[line].desc, row, col);
	}
	/* Second section - the "extra options" */
	else
	{
		byte attr = curs_attrs[CURS_KNOWN][(int)cursor];

		line = line - N_ELEMENTS(sval_dependent) - 1;
    
		if (line < N_ELEMENTS(extra_item_options))
			c_prt(attr, extra_item_options[line].name, row, col);
	}
}

#ifdef EFG

static char tag_options_gen_squelch(menu_type *menu, int oid)
{
	size_t line = (size_t) oid;

	if (line < N_ELEMENTS(type_names))
		return I2A(oid);

	/* Separator - blank line. */
	if (line == N_ELEMENTS(type_names))
		return 0;

	line = line - N_ELEMENTS(type_names) - 1;

	if (line < N_ELEMENTS(extra_item_options))
		return extra_item_options[line].tag;

	return 0;
}

static int valid_options_gen_squelch(menu_type *menu, int oid)
{
	size_t line = (size_t) oid;

	if (line < N_ELEMENTS(type_names))
		return 1;

	/* Separator - blank line. */
	if (line == N_ELEMENTS(type_names))
		return 0;

	line = line - N_ELEMENTS(type_names) - 1;

	if (line < N_ELEMENTS(extra_item_options))
		return 1;

	return 0;
}

static void display_options_gen_squelch(menu_type *menu, int oid, bool cursor, int row, int col, int width)
{
	size_t line = (size_t) oid;
	byte attr = curs_attrs[CURS_KNOWN][(int)cursor];

	/* First section of menu - the svals */
	if (line < N_ELEMENTS(type_names))
	{
		c_prt(attr, type_names[line], row, col);
	}
	/* Second section - the "extra options" */
	else
	{
		line = line - N_ELEMENTS(type_names) - 1;
    
		if (line < N_ELEMENTS(extra_item_options))
			c_prt(attr, extra_item_options[line].name, row, col);
	}
}

static const menu_iter options_gen_squelch_iter =
{
	0,
	tag_options_gen_squelch,
	valid_options_gen_squelch,
	display_options_gen_squelch,
	NULL
};

#endif

static const menu_iter options_item_iter =
{
	0,
	tag_options_item,
	valid_options_item,
	display_options_item,
	NULL
};


/*
 * Display and handle the main squelching menu.
 */
void do_cmd_options_item(void *unused, cptr title)
{
	int cursor = 0;
	event_type c = EVENT_EMPTY;
	const char cmd_keys[] = { ARROW_LEFT, ARROW_RIGHT, '\0' };

	menu_type menu;

	WIPE(&menu, menu_type);
	menu.title = title;
        menu.cmd_keys = cmd_keys;
	menu.count = N_ELEMENTS(sval_dependent) + N_ELEMENTS(extra_item_options) + 1;
	menu_init2(&menu, find_menu_skin(MN_SCROLL), &options_item_iter, &SCREEN_REGION);

	menu_layout(&menu, &SCREEN_REGION);

	/* Save and clear screen */
	screen_save();
	clear_from(0);

	while (c.key != ESCAPE)
	{
		clear_from(0);
		c = menu_select(&menu, &cursor, 0);

		if (c.type == EVT_SELECT)
		{
			if ((size_t) cursor < N_ELEMENTS(sval_dependent))
			{
				sval_menu(sval_dependent[cursor].tval, sval_dependent[cursor].desc);
			}
			else
			{
				cursor = cursor - N_ELEMENTS(sval_dependent) - 1;
				if ((size_t) cursor < N_ELEMENTS(extra_item_options))
					extra_item_options[cursor].action(NULL, NULL);
			}
		}
	}

#ifdef EFG
	/* EFGchange bugfix */
	p_ptr->notice |= PN_SQUELCH;

#endif
	/* Load screen and finish */
	screen_load();
	return;
}

#ifdef EFG
void do_cmd_options_squelch_generalized(void *unused, cptr title)
{
	int cursor = 0;
	event_type c = EVENT_EMPTY;
	const char cmd_keys[] = { ARROW_LEFT, ARROW_RIGHT, '\0' };

	menu_type menu;

	WIPE(&menu, menu_type);
	menu.title = title;
        menu.cmd_keys = cmd_keys;
	menu.count = N_ELEMENTS(type_names) + N_ELEMENTS(extra_item_options) + 1;
	menu_init2(&menu, find_menu_skin(MN_SCROLL), &options_gen_squelch_iter, &SCREEN_REGION);

	menu_layout(&menu, &SCREEN_REGION);

	/* Save and clear screen */
	screen_save();
	clear_from(0);

	while (c.key != ESCAPE)
	{
		clear_from(0);
		c = menu_select(&menu, &cursor, 0);

		if (c.type == EVT_SELECT)
		{
			if ((size_t) cursor < N_ELEMENTS(type_names))
			{
				gen_squelch_menu(cursor, type_names[cursor]);
			}
			else
			{
				cursor = cursor - N_ELEMENTS(type_names) - 1;
				if ((size_t) cursor < N_ELEMENTS(extra_item_options))
					extra_item_options[cursor].action(NULL, NULL);
			}
		}
	}

	p_ptr->notice |= PN_SQUELCH;

	/* Load screen and finish */
	screen_load();
	return;
}

void squelch_clear(s16b k_idx)
{
	/* ??? should be testing if k_idx is valid */
	k_info[k_idx].squelch = 0;
}


/* EFGchange allow squelching unaware objects */
void squelch_kind(s16b k_idx, bool aware)
{
	/* ??? should be testing if k_idx is valid */
	object_kind *k_ptr = &k_info[k_idx];

	if (aware)
		k_ptr->squelch |= SQUELCH_AWARE_FLAG;
	else
		k_ptr->squelch |= SQUELCH_UNAWARE_FLAG;
}

/* EFGchange generalized squelch */
/* do not use rd_object since we may make screwy objects and
   do not want the savefile reader to try to correct them */
void rd_squelch_gen(void)
{
        byte numarrays;
	u32b len;
        int i, j, k;

        savefile_rd_byte(&numarrays);
	/* ??? assert numarrays == TYPE_MAX else flush */
        for (i = 0; i < numarrays; i++)
        {
                savefile_rd_u32b(&len);
		gen_squelch_used[i] = len;
		for (j = 0; j < len; j++)
		{
			object_type *o_ptr = &gen_squelch_list[i][j];
			byte *b_ptr = (byte *) o_ptr;
			for (k = 0; k < sizeof(object_type); k++)
			{
				savefile_rd_byte(b_ptr);
				b_ptr++;
			}
		}
		if (numarrays != TYPE_MAX)
			gen_squelch_used[i] = 0;
        }
}

void wr_squelch_gen(void)
{
        int i, j, k;
	u32b len;
	/* ??? somewhere should assert that TYPE_MAX fits into a byte */
	savefile_wr_byte(TYPE_MAX);
        for (i = 0; i < TYPE_MAX; i++)
        {
		len = gen_squelch_used[i];
                savefile_wr_u32b(len);
		for (j = 0; j < len; j++)
                {
                        object_type *o_ptr = &gen_squelch_list[i][j];
                        byte *b_ptr = (byte *) o_ptr;
                        for (k = 0; k < sizeof(object_type); k++)
			{
                                savefile_wr_byte(*b_ptr);
				b_ptr++;
			}
                }

        }
}

bool squelch_object_interactive(const object_type *i_ptr)
{
        int t = get_squelch_type(i_ptr, FALSE);

	char prompt[160];
	object_type o = *i_ptr;
	object_type *o_ptr = &o;

	/* ask about quality squelch */
	char *query = NULL;
	int lev = SQUELCH_NONE;
	bool ret = FALSE;

	char o_name[80];
        object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);
	char *tval_name = NULL;

	/* ??? should check if already squelched, say "press any key to continue */

	/* if known, can squelch precise object */
	if (object_known_p(o_ptr) || object_is_known_splendid(o_ptr))
	{
		sprintf(prompt, "Squelch %s? ", o_name);
               	if (!get_check(prompt))
			return FALSE;

		/* for fixed objects, no point in further queries */
		if (squelchable_tval(o_ptr->tval))
		{
			if (!object_flavor_is_fixed(o_ptr))
			{
				squelch_object(o_ptr);
				/* ??? need to get a real prompt here, editing o_name to remove pval */
				sprintf(prompt, "Squelch all pvals for %s? ", o_name);
               			if (get_check(prompt))
				{
					bool is_aware = TRUE;
                       			squelch_kind(o_ptr->k_idx, is_aware);
					/* ??? this does not allow quality squelch query on rings of weakness */
					return TRUE;
				}
				else
				{
					/* for cursed objects, will still want to inquire about quality squelch */
					if (object_is_cursed(o_ptr) || (o_ptr->pseudo == INSCRIP_UNCURSED))
						ret = TRUE;
					else
						return TRUE;
				}
			}
			else
			{
				bool is_aware = TRUE;
                       		squelch_kind(o_ptr->k_idx, is_aware);
               			switch(o_ptr->tval)
               			{
                       			case TV_MAGIC_BOOK: tval_name = "magic books"; break;
                       			case TV_PRAYER_BOOK: tval_name = "prayer books"; break;
               			}
               			if (tval_name != NULL)
               			{
                       			sprintf(prompt, "Squelch all %s? ", tval_name);
                       			if (get_check(prompt))
                       			{
						int idx;
                        			for (idx = 0; idx < z_info->k_max; idx++)
                                			if (k_info[idx].tval == o_ptr->tval)
                                                        	k_info[idx].squelch |= SQUELCH_AWARE_FLAG;
                       			}
               			}
				if (object_is_cursed(o_ptr))
					/* might still want to expand to quality squelch */
					ret = TRUE;
				else
					return TRUE;
			}
		}
		else
		{
			squelch_object(o_ptr);
		}
		ret = TRUE;
	}
	else if (squelchable_tval(o_ptr->tval))
	{
		sprintf(prompt, "Squelch %s? ", o_name);
               	if (!get_check(prompt))
			return FALSE;

		if (object_is_aware(o_ptr))
		{
			if (object_is_known_bad(o_ptr))
			{
				/* if you squelch a cursed ring of damage you are done */
				squelch_object(o_ptr);
				ret = TRUE;
			}
			else
			{
				/* if you squelch a good unknown ring of damage you are done */
				/* ??? might be complicated if =dam+16 counts as excellent */
				k_info[o_ptr->k_idx].squelch |= SQUELCH_AWARE_FLAG;
				return TRUE;
			}
		}
		else
		{
			k_info[o_ptr->k_idx].squelch |= SQUELCH_UNAWARE_FLAG;
			ret = TRUE;
               		if (!k_info[o_ptr->k_idx].tried)
               		{
                       		switch(o_ptr->tval)
                       		{
					/* These may need separate treatment because they can have quality */
                               		case TV_RING: tval_name = "rings"; break;
                               		case TV_AMULET: tval_name = "amulets"; break;

					/* Some potions have fixed flavors.  Squelching them does not suggest further interest */
                               		case TV_POTION:
                                       		tval_name = "potions";
                                       		switch(o_ptr->sval) /* perhaps this should be a function ??? */
                                       		{
                                               		case SV_POTION_WATER:
                                               		case SV_POTION_APPLE_JUICE:
                                               		case SV_POTION_SLIME_MOLD:
                                                       		tval_name = NULL;
                                       		}
                                       		break;
                               		case TV_FOOD: tval_name = "food"; break;
                               		case TV_WAND: tval_name = "wands"; break;
                               		case TV_STAFF: tval_name = "staves"; break;
                               		case TV_ROD: tval_name = "rods"; break;
                               		case TV_SCROLL: tval_name = "scrolls"; break;
                       		}
               		}
		}
	}


/* ??? need to consider squelching all items of a given ego before moving on to all excellent types */

	int qual = INSCRIP_NULL;
	/* maybe can squelch based upon quality */
	if (object_is_known(o_ptr) || (o_ptr->pseudo))
	{
		flag_block_type F[OBJECT_FLAG_BLOCKS];
		object_flags_all_known(o_ptr, F);
		qual = object_quality(o_ptr, F);
		/* this is screwed up for excellent object weak pseudo should be good */
		char *qual_string;
		if (!object_is_known(o_ptr)) switch(o_ptr->pseudo)
		{
			case INSCRIP_CURSED:
			case INSCRIP_UNCURSED:
			case INSCRIP_WORTHLESS:
				qual = INSCRIP_CURSED;
				break;
			case INSCRIP_GOOD:
	    			if (cp_ptr->flags & CF_PSEUDO_ID_HEAVY)
					break;
			case INSCRIP_TRIED:
			/* ??? maybe best is to use TRIED, works for weak too */
				qual = INSCRIP_EXCELLENT;
				break;
		}
		switch(qual)
		{
			case INSCRIP_CURSED:
				qual_string = "cursed"; break;
			case INSCRIP_AVERAGE:
				qual_string = "average"; break;
			case INSCRIP_GOOD:
				qual_string = "good"; break;
			case INSCRIP_EXCELLENT:
				qual_string = "excellent"; break;
			default:
				qual_string = NULL; break;
		}
		if (qual_string != NULL) 
		{
			if (qual != INSCRIP_AVERAGE)
			{
				bool specifically_squelchable = TRUE;
				switch(t)
				{
					case TYPE_ARMOR_BODY:
					case TYPE_ARMOR_CLOAK:
					case TYPE_ARMOR_SHIELD:
					case TYPE_ARMOR_HEAD:
					case TYPE_ARMOR_HANDS:
					case TYPE_ARMOR_FEET:
						sprintf(prompt, "Squelch all %s %s >=%2.1f lb, base AC %d? ", qual_string, type_names[t], o_ptr->weight/10.0, o_ptr->ac);
						break;
					case TYPE_WEAPON_POINTY:
					case TYPE_WEAPON_BLUNT:
						sprintf(prompt, "Squelch all %s weapons >= %2.1f lbs with base damage <= %dd%d? ", qual_string, o_ptr->weight/10.0, o_ptr->dd, o_ptr->ds);
						break;
					case TYPE_SHOOTER:
                				switch(o_ptr->sval)
                				{
                        				case SV_SLING:
                                				sprintf(prompt, "Squelch all %s slings? ", qual_string);
                                				break;
                        				case SV_SHORT_BOW:
                                				sprintf(prompt, "Squelch all %s shortbows? ", qual_string);
                                				break;
                        				case SV_LONG_BOW:
                                				sprintf(prompt, "Squelch all %s longbows? ", qual_string);
                                				break;
                        				case SV_LIGHT_XBOW:
                                				sprintf(prompt, "Squelch all %s light xbows? ", qual_string);
                                				break;
                        				case SV_HEAVY_XBOW:
                                				sprintf(prompt, "Squelch all %s heavy xbows? ", qual_string);
                                				break;
                				}
						break;
					case TYPE_MISSILE_SLING:
                                		sprintf(prompt, "Squelch all %s %dd%d sling ammo? ", qual_string, o_ptr->dd, o_ptr->ds);
                                		break;
					case TYPE_MISSILE_BOW:
                                		sprintf(prompt, "Squelch all %s %dd%d arrows? ", qual_string, o_ptr->dd, o_ptr->ds);
                                		break;
					case TYPE_MISSILE_XBOW:
                                		sprintf(prompt, "Squelch all %s %dd%d bolts? ", qual_string, o_ptr->dd, o_ptr->ds);
                                		break;
					case TYPE_DIGGER:
						sprintf(prompt, "Squelch all %s diggers >= %2.1f lbs with base damage <= %dd%d? ", qual_string, o_ptr->weight/10.0, o_ptr->dd, o_ptr->ds);
                                		break;
					default:
						/* This is stuff like jewelry that has no base qualities */
						specifically_squelchable = FALSE;
						break;
				}
               			if (specifically_squelchable)
				{
					if (get_check(prompt))
					{
						ret = TRUE;
						/* ??? if known, have to make o_ptr unknown first */
						object_type u;
						object_type *u_ptr = &u;
						*u_ptr = *o_ptr;
						/* ??? should remove any inscription */
						if (object_is_known(u_ptr))
						{
							u_ptr->ident &= ~(IDENT_KNOWN);
						}
						u_ptr->pseudo = qual;
						if (((o_ptr->pseudo == INSCRIP_GOOD) || (o_ptr->pseudo == INSCRIP_TRIED)) && (qual == INSCRIP_EXCELLENT))
							u_ptr->pseudo = INSCRIP_TRIED; /* this avoids inconsistent object, same effect */
						squelch_object(u_ptr);
					}
					else
						return ret;
				}
			}
		}
		
	}

	/* now for quality squelch menu which is irrespective of base values or weight */

	/* There is an extra subset of cursed squelching to check first */
	char *jewelry_name = NULL;
	int  jewelry_type;
	if (o_ptr->tval == TV_RING)
	{
		jewelry_name = "rings";
		jewelry_type = TYPE_RING;
	}
	if (o_ptr->tval == TV_AMULET)
	{
		jewelry_name = "amulets";
		jewelry_type = TYPE_AMULET;
	}
	if (jewelry_name != NULL)
	{
		if ((object_known_p(o_ptr) && cursed_p(o_ptr)) ||
		    (((o_ptr->pseudo == INSCRIP_CURSED) || (o_ptr->pseudo == INSCRIP_UNCURSED)) &&
		    (object_aware_p(o_ptr))))
		{
			sprintf(prompt, "Squelch all cursed %s when aware of the flavor? ", jewelry_name);
			if (get_check(prompt))
			{
				squelch_level[jewelry_type] = SQUELCH_JEWELRY_CURSED_AWARE;
				ret = TRUE; /* probably has to be just to get here */
			}
		}
	}


	int quality = o_ptr->pseudo;
	if (object_is_known(o_ptr) && !quality)
		quality = value_check_aux1(o_ptr);
	switch(quality)
	{
		case INSCRIP_CURSED:
		case INSCRIP_UNCURSED:
		case INSCRIP_WORTHLESS:
			query = "Squelch all cursed"; lev = SQUELCH_CURSED; break;
		case INSCRIP_GOOD:
			query = "Squelch all good"; lev = SQUELCH_GOOD_STRONG;
			if ((cp_ptr->flags & CF_PSEUDO_ID_HEAVY) || (object_is_known(o_ptr)))
				break;
			/* otherwise with weak pseudo squelching good means all but splendid */
		case INSCRIP_TRIED:
		case INSCRIP_EXCELLENT:
			query = "Squelch all but splendid"; lev = SQUELCH_UNSPLENDID; break;

		case INSCRIP_AVERAGE:
			query = "Squelch all average";
			lev = SQUELCH_AVERAGE;
			if (o_ptr->tval == TV_LITE)
                        {
                                switch(o_ptr->sval)
                                {
                                        case SV_LITE_TORCH:
                                                if (get_check("Squelch all average torches? "))
                                                        squelch_level[TYPE_LITE] = SQUELCH_LITE_AVERAGE_TORCHES;
                                                else
                                                        return FALSE; /* ??? this assumes that squelch by pval does not work */
                                                break;
                                        case SV_LITE_LANTERN:
                                                if (get_check("Squelch all average lanterns? "))
                                                        squelch_level[TYPE_LITE] = SQUELCH_LITE_AVERAGE_LANTERNS;
                                                else
                                                        return FALSE; /* ??? this assumes that squelch by pval does not work */
                                                break;
                                }
                                lev = SQUELCH_LITE_AVERAGE_ALL;
                        }

			break;
		default:
			query = NULL; break;
	}

	if (query != NULL)
	{
		sprintf(prompt, "%s %s? ", query, type_names[t]);
               	if (!get_check(prompt))
			return ret;

		squelch_level[t] = lev;
		ret = TRUE;
	}


        if (squelchable_tval(o_ptr->tval))
        {
		if ((tval_name != NULL) && (!object_is_aware(o_ptr)))
        	{
			sprintf(prompt, "Squelch all unaware %s? ", tval_name);
			if (!get_check(prompt))
				return ret;

                	int idx;
                	for (idx = 0; idx < z_info->k_max; idx++)
                	{
                        	if (k_info[idx].tval == o_ptr->tval)
                        	{
					/*
                                	if (k_info[idx].aware)
                                	{
                                        	if (squelch_aware)
                                                	k_info[idx].squelch |= SQUELCH_AWARE_FLAG;
                                	}
                                	else
                                	{
					*/
                                        	k_info[idx].squelch |= SQUELCH_UNAWARE_FLAG;
					/*
                                	}
					*/
	
                        	}
                	}
                	return TRUE;
        	}

#ifdef GFE
		/* clear extraneous info from query */
		int i;
		for (i = 0; o_name[i] != '\0'; i++)
			switch(o_name[i+1])
			{
				case '[': /* have to be careful about spellbooks */
					if (!o_ptr->ac)
						break;
				case '(':
				case '{':
					o_name[i] = '\0';
					break;
			}
#endif

        }
        else if (o_ptr->tval == TV_LITE)
        {
		/* ??? should also do this for lanterns */
		/* ??? these prompts should be in a table indexed by squelch level */
		if (o_ptr->sval == SV_LITE_TORCH)
		{
			/* ??? should we allow squelch by pval? */
			if (o_ptr->name2)
			{
				if (get_check("Squelch all excellent lights? "))
				{
					squelch_level[TYPE_LITE] = SQUELCH_LITE_EXCELLENT;
					return TRUE;
				}
			}
			else if (squelch_level[TYPE_LITE] == SQUELCH_LITE_NONE)
			{
				if (get_check("Squelch all average torches? "))
				{
					squelch_level[TYPE_LITE] = SQUELCH_LITE_AVERAGE_TORCHES;
					return TRUE;
				}
			}
			else if (squelch_level[TYPE_LITE] == SQUELCH_LITE_AVERAGE_LANTERNS)
			{
				if (get_check("Squelch all average lights? "))
				{
					squelch_level[TYPE_LITE] = SQUELCH_LITE_AVERAGE_ALL;
					return TRUE;
				}
			}
		}
        }
	else if ((t == TYPE_SHOOTER) && (o_ptr->pseudo == INSCRIP_GOOD)) /* ??? weak pseudo? */
	{
		char prompt[160];
		switch(o_ptr->sval)
		{
			case SV_SLING:
				sprintf(prompt, "Squelch all good slings? ");
				break;
			case SV_SHORT_BOW:
				sprintf(prompt, "Squelch all good shortbows? ");
				break;
			case SV_LONG_BOW:
				sprintf(prompt, "Squelch all good longbows? ");
				break;
			case SV_LIGHT_XBOW:
				sprintf(prompt, "Squelch all good light xbows? ");
				break;
			case SV_HEAVY_XBOW:
				sprintf(prompt, "Squelch all good heavy xbows? ");
				break;
		}
                if (get_check(prompt))
		{
			squelch_object(o_ptr);
			return TRUE;
		}
	}
        else if (object_known_p(o_ptr) && !o_ptr->name1 && !o_ptr->name2)
	{
		char prompt[160];
		char *name = NULL;
		int mult;
		switch(t)
		{
			case TYPE_WEAPON_POINTY:
			case TYPE_WEAPON_BLUNT:
				sprintf(prompt, "Squelch weapons >= %2.1f lb, base damage <= %dd%d, +hit <= %d, +dam <= %d? ", o_ptr->weight/10.0, o_ptr->dd, o_ptr->ds, o_ptr->to_h, o_ptr->to_d);
				break;
			case TYPE_MISSILE_SLING:
				sprintf(prompt, "Squelch sling ammo >= %0.1f lb, damage <= %dd%d, +hit <= %d, +dam <= %d? ", o_ptr->weight/10.0, o_ptr->dd, o_ptr->ds, o_ptr->to_h, o_ptr->to_d);
				break;
			case TYPE_MISSILE_BOW:
				sprintf(prompt, "Squelch arrows >= %0.1f lb, base damage <= %dd%d, +hit <= %d, +dam <= %d? ", o_ptr->weight/10.0, o_ptr->dd, o_ptr->ds, o_ptr->to_h, o_ptr->to_d);
				break;
			case TYPE_MISSILE_XBOW:
				sprintf(prompt, "Squelch bolts >= %0.1f lb, base damage <= %dd%d, +hit <= %d, +dam <= %d? ", o_ptr->weight/10.0, o_ptr->dd, o_ptr->ds, o_ptr->to_h, o_ptr->to_d);
				break;
			case TYPE_ARMOR_BODY:
			case TYPE_ARMOR_CLOAK:
			case TYPE_ARMOR_SHIELD:
			case TYPE_ARMOR_HEAD:
			case TYPE_ARMOR_HANDS:
			case TYPE_ARMOR_FEET:
				sprintf(prompt, "Squelch %s >=%2.1f lb, base AC %d, +ac<=%d, +hit<=%d, +dam<=%d? ", type_names[t], o_ptr->weight/10.0, o_ptr->ac, o_ptr->to_a, o_ptr->to_h, o_ptr->to_d);
				break;
			case TYPE_SHOOTER:
				switch(o_ptr->sval)
				{
					case SV_SLING:
						name = "sling";
						mult = 2;
						break;
					case SV_SHORT_BOW:
						name = "bow";
						mult = 2;
						break;
					case SV_LONG_BOW:
						name = "bow";
						mult = 3;
						break;
					case SV_LIGHT_XBOW:
						name = "crossbow";
						mult = 3;
						break;
					case SV_HEAVY_XBOW:
						name = "crossbow";
						mult = 4;
						break;
				}
				if (name != NULL)
				{
					sprintf(prompt, "Squelch every (x%d) %s with +hit<=%d and +dam<=%d? ", mult, name, o_ptr->to_h, o_ptr->to_d);
					break;
				}
			default:
                		sprintf(prompt, "[maybe unimplemented?] squelch dominated by ...?");
				break;
		}
	}
	return ret;
}

bool obviously_excellent(const object_type *o_ptr, bool to_print, char *o_name)
{
	bool ret = FALSE;

	/* the player should be informed of items that obviously boost */
	/* ??? should check for tried, print this when "I"nspecting a tried object */

	int i;
	u32b f1, f2, f3;
	u32b F[3];

/*
	switch(o_ptr->pseudo)
	{
		case INSCRIP_EXCELLENT:
		case INSCRIP_SPLENDID:
			return TRUE;
	}
*/

	object_flags(o_ptr, &f1, &f2, &f3);
	object_flags_all(o_ptr, F);

	if (cursed_p(o_ptr))
		return FALSE;

	for (i = 0; i < N_ELEMENTS(obvious_list); i++)
	{
		if (extract_flag(F, obvious_list[i].flag_idx))
		{
			ret = TRUE;
			if (to_print)
				switch(obvious_list[i].format)
				{
					case OBVIOUS_FORMAT_NAME:
						msg_format(obvious_list[i].desc, o_name);
						break;
					case OBVIOUS_FORMAT_BOOST:
						msg_format(obvious_list[i].desc, o_name, (o_ptr->pval > 0) ? "boosts" : "reduces", abs(o_ptr->pval));
						break;
				}
		}
	}
	return ret;
}

int next_matching_unaware_kind(const object_type *o_ptr, int f)
/* f for flavor, maybe should be k for kind */
{
	/* assuming can wield to get obvious info, and that object is not aware */
        u32b F[3];
	int i;

	/* This is not necessary, just an optimization. */
	if ((o_ptr->tval != TV_RING) && (o_ptr->tval != TV_AMULET))
		return 0;

        object_flags_all_known(o_ptr, F);

        for (f++; f < z_info->k_max; f++)
	{
                object_kind *k_ptr = &k_info[f];
		if (k_ptr->aware)
			continue;
		if (o_ptr->tval != k_ptr->tval)
			continue;

		if (object_is_safe_to_wield(o_ptr))
		{
			/* ??? do we need to check pval is visible, e.g. on ego of searching? */
			if ((k_ptr->flags3 & (TR3_LIGHT_CURSE | TR3_HEAVY_CURSE | TR3_PERMA_CURSE)) &&
			    (o_ptr->pval > 0))
				continue;
/* should we check range of negative values for cursed stuff? */
/* ??? pvals are hardcoded!?! */
/*
		if (o_ptr->pval > k_ptr->pval)
			continue;
*/
/* ??? assuming can wield to check flags here */

			if (extract_flag(F, ACTIVATE))
			{
				/* assuming activations are unique */
				if (f != o_ptr->k_idx)
					continue;
			}
			for (i = 0; i < N_ELEMENTS(obvious_list); i++)
			{
/*
if (obvious_list[i].flag_idx == STR)
{
	printf("i is %d, F[0] is %d, kf1 is %d, kflag is %d, oflag is %d\n", i, F[0], k_ptr->flags1, extract_flag(&k_ptr->flags1, obvious_list[i].flag_idx),
extract_flag(F, obvious_list[i].flag_idx));
}
*/
				if (extract_flag(&k_ptr->flags1, obvious_list[i].flag_idx))
				{
					if (!extract_flag(F, obvious_list[i].flag_idx))
						break;
				}
				else 
				{
					if (extract_flag(F, obvious_list[i].flag_idx))
						break;
				}
			}
			if (i != N_ELEMENTS(obvious_list))
				continue;

			return f;
		}
	}
	return 0;
}

int next_matching_ego(const object_type *o_ptr, int e)
{
	/* assuming can wield to get obvious info */
	if (!o_ptr->name2)
		return 0;

        int i, j;
	bool found_extra;
        u32b F[3];
        object_flags_all_known(o_ptr, F);

        for (e++; e < z_info->e_max; e++)
        {
                /* Get the i'th ego item */
                ego_item_type *e_ptr = &e_info[e];
                /* Legal items */
                if (e_ptr->rarity)
		{
                	for (j = 0; j < EGO_TVALS_MAX; j++)
				if ((o_ptr->tval == e_ptr->tval[j]) && (o_ptr->sval >= e_ptr->min_sval[j])
                                     && (o_ptr->sval <= e_ptr->max_sval[j]))
				{
					if (e_ptr->flags3 & (TR3_LIGHT_CURSE | TR3_HEAVY_CURSE | TR3_PERMA_CURSE))
					{
						if (object_known_never_cursed(o_ptr))
							continue;
						/*
						switch (o_ptr->pseudo)
						{
							case INSCRIP_TRIED:
							case INSCRIP_AVERAGE:
							case INSCRIP_GOOD:
							case INSCRIP_EXCELLENT:
							case INSCRIP_SPLENDID:
								continue;
						}
						if (o_ptr->pseudo != INSCRIP_UNCURSED)
							return(e);
						*/
					}
					else
					{
						if (object_is_known_bad(o_ptr))
							continue;
					}

					/* ??? should add a check so that cursed items do not match good egos */
					/* ??? except maybe new pseudo TAINTED for nazgul etc, or zang-style */
					if (object_is_safe_to_wield(o_ptr))
					{
						found_extra = FALSE;
						for (i = 0; i < N_ELEMENTS(obvious_list); i++)
						{
							if (extract_flag(&e_ptr->flags1, obvious_list[i].flag_idx))
							{
								if (!extract_flag(F, obvious_list[i].flag_idx))
									break;
							}
							else if (extract_flag(F, obvious_list[i].flag_idx))
							{
								if ((e_ptr->xtra == OBJECT_XTRA_TYPE_POWER) && (i < OBVIOUS_XTRA) && (!found_extra))
									found_extra = TRUE;
								else
                                                               		break;
							}
						}
						if (i != N_ELEMENTS(obvious_list))
							continue;
					}

					return(e);
				}
		}
	}
	return 0;
}

int num_matching_unaware_kinds(const object_type *o_ptr)
{
	int num = 0;
	int e = 0;
	while ((e = next_matching_unaware_kind(o_ptr, e)))
		num++;
	return num;
}

bool obvious_kind(const object_type *o_ptr)
{
	return (object_aware_p(o_ptr)) || (num_matching_unaware_kinds(o_ptr) == 1);
}

int num_matching_egos(const object_type *o_ptr)
{
	int num = 0;
	int e = 0;
	while ((e = next_matching_ego(o_ptr, e)))
		num++;
	return num;
}

bool obvious_ego(const object_type *o_ptr)
{
	return (num_matching_egos(o_ptr) == 1);
}

#endif
