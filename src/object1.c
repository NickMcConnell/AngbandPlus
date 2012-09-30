/* File: object1.c */

/* Purpose: Object code, part 1 */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"


/*
 * Reset the "visual" lists
 *
 * This involves resetting various things to their "default" state.
 *
 * If the "prefs" flag is TRUE, then we will also load the appropriate
 * "user pref file" based on the current setting of the "use_graphics"
 * flag.  This is useful for switching "graphics" on/off.
 *
 * The features, objects, and monsters, should all be encoded in the
 * relevant "font.pref" and/or "graf.prf" files.  XXX XXX XXX
 *
 * The "prefs" parameter is no longer meaningful.  XXX XXX XXX
 */
void reset_visuals(void)
{
	int i;

	/* Extract some info about terrain features */
	for (i = 0; i < z_info->f_max; i++)
	{
		feature_type *f_ptr = &f_info[i];

		/* Assume we will use the underlying values */
		f_ptr->x_attr = f_ptr->d_attr;
		f_ptr->x_char = f_ptr->d_char;

		/* No extra information */
		f_ptr->w_attr = 0;
		f_ptr->w_char = 0;
	}

	/* Extract default attr/char code for objects */
	for (i = 0; i < z_info->k_max; i++)
	{
		object_kind *k_ptr = &k_info[i];

		/* Default attr/char */
		k_ptr->x_attr = k_ptr->d_attr;
		k_ptr->x_char = k_ptr->d_char;
	}

	/* Extract default attr/char code for monsters */
	for (i = 0; i < z_info->r_max; i++)
	{
		monster_race *r_ptr = &r_info[i];

		/* Default attr/char */
		r_ptr->x_attr = r_ptr->d_attr;
		r_ptr->x_char = r_ptr->d_char;
	}

	/* Extract default attr/char code for fields */
	for (i = 0; i < z_info->t_max; i++)
	{
		field_thaum *t_ptr = &t_info[i];

		/* Default attr/char */
		t_ptr->f_attr = t_ptr->d_attr;
		t_ptr->f_char = t_ptr->d_char;
	}


	if (use_graphics)
	{
		/* Process "graf.prf" */
		(void)process_pref_file("graf.prf");
	}

	/* Normal symbols */
	else
	{
		/* Process "font.prf" */
		(void)process_pref_file("font.prf");
	}

	/* Reset the fake monochrome flag */
	fake_monochrome = (!use_graphics
					   || streq(ANGBAND_SYS, "ibm")) ? TRUE : FALSE;

	/* Fields have to notice the change of visuals. */
	init_fields();
}


/*
 * Obtain the "flags" for an item
 */
void object_flags(const object_type *o_ptr, u32b *f1, u32b *f2, u32b *f3)
{
	const object_kind *k_ptr = &k_info[o_ptr->k_idx];

	/* Base object */
	(*f1) = k_ptr->flags1 | o_ptr->flags1;
	(*f2) = k_ptr->flags2 | o_ptr->flags2;
	(*f3) = k_ptr->flags3 | o_ptr->flags3;

	/* Remove the Moria flags */
	if (ironman_moria)
	{
		(*f1) &= TR1_MORIA_MASK;
		(*f2) &= TR2_MORIA_MASK;
		(*f3) &= TR3_MORIA_MASK;
	}
}


/*
 * Obtain the "flags" for an item which are known to the player
 */
void object_flags_known(const object_type *o_ptr, u32b *f1, u32b *f2, u32b *f3)
{
	const object_kind *k_ptr = &k_info[o_ptr->k_idx];

	bool known = object_known_p(o_ptr);

	/* Clear */
	(*f1) = (*f2) = (*f3) = 0L;

	if (cursed_p(o_ptr) && (known || (o_ptr->info & (OB_SENSE))))
	{
		(*f3) |= TR3_CURSED;
	}

	/* Must be identified */
	if (!known) return;

	/* Base object */
	(*f1) = k_ptr->flags1;
	(*f2) = k_ptr->flags2;
	(*f3) = k_ptr->flags3;

	/* Show modifications to stats */
	(*f1) |= (o_ptr->flags1 &
			  (TR1_STR | TR1_INT | TR1_WIS | TR1_DEX | TR1_CON | TR1_CHR));

	/* 
	 * *Identify* sets these flags,
	 * and ego items have some set on creation.
	 */
	(*f1) |= o_ptr->kn_flags1;
	(*f2) |= o_ptr->kn_flags2;
	(*f3) |= o_ptr->kn_flags3;

	/* We now now whether or not it is an artifact */
	if (o_ptr->flags3 & TR3_INSTA_ART)
	{
		(*f3) |= TR3_INSTA_ART;
	}

	/* Remove the Moria flags */
	if (ironman_moria)
	{
		(*f1) &= TR1_MORIA_MASK;
		(*f2) &= TR2_MORIA_MASK;
		(*f3) &= TR3_MORIA_MASK;
	}
}


/*
 * Determine the "Activation" (if any) for an artifact
 * Return a string, or NULL for "no activation"
 */
cptr item_activation(const object_type *o_ptr)
{
	u32b f1, f2, f3;

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* Require activation ability */
	if (!(f3 & (TR3_ACTIVATE))) return ("nothing");

	if (o_ptr->activate < 128)
	{
		switch (o_ptr->activate)
		{
			case ACT_SUNLIGHT:
			{
				return "beam of sunlight every 10 turns";
			}
			case ACT_BO_MISS_1:
			{
				return "magic missile (3d6) every 2 turns";
			}
			case ACT_BA_POIS_1:
			{
				return "stinking cloud (25), rad. 3, every 4+d4 turns";
			}
			case ACT_BO_ELEC_1:
			{
				return "lightning bolt (6d8) every 6+d6 turns";
			}
			case ACT_BO_ACID_1:
			{
				return "acid bolt (8d8) every 5+d5 turns";
			}
			case ACT_BO_COLD_1:
			{
				return "frost bolt (9d8) every 7+d7 turns";
			}
			case ACT_BO_FIRE_1:
			{
				return "fire bolt (11d8) every 8+d8 turns";
			}
			case ACT_BA_COLD_1:
			{
				return "ball of cold (100) every 400 turns";
			}
			case ACT_BA_FIRE_1:
			{
				return "ball of fire (150) every 400 turns";
			}
			case ACT_DRAIN_1:
			{
				return "drain life (200) every 100+d100 turns";
			}
			case ACT_BA_COLD_2:
			{
				return "ball of cold (200) every 300 turns";
			}
			case ACT_BA_ELEC_2:
			{
				return "ball of lightning (200) every 500 turns";
			}
			case ACT_DRAIN_2:
			{
				return "drain life (250) every 400 turns";
			}
			case ACT_VAMPIRE_1:
			{
				return "vampiric drain (3*100) every 400 turns";
			}
			case ACT_BO_MISS_2:
			{
				return "arrows (250) every 90+d90 turns";
			}
			case ACT_BA_FIRE_2:
			{
				return "fire ball (250) every 225+d225 turns";
			}
			case ACT_BA_COLD_3:
			{
				return "ball of cold (400) every 325+d325 turns";
			}
			case ACT_BA_ELEC_3:
			{
				return "ball of lightning (500) every 425+d425 turns";
			}
			case ACT_WHIRLWIND:
			{
				return "whirlwind attack every 250 turns";
			}
			case ACT_VAMPIRE_2:
			{
				return "vampiric drain (3*200) every 400 turns";
			}
			case ACT_CALL_CHAOS:
			{
				return "call chaos every 350 turns";
			}
			case ACT_ROCKET:
			{
				return "launch rocket (300+level) every 400 turns";
			}
			case ACT_DISP_EVIL:
			{
				return "dispel evil (level*5) every 300+d300 turns";
			}
			case ACT_BA_MISS_3:
			{
				return "elemental breath (500) every 500 turns";
			}
			case ACT_DISP_GOOD:
			{
				return "dispel good (level*5) every 300+d300 turns";
			}
			case ACT_CONFUSE:
			{
				return "confuse monster every 15 turns";
			}
			case ACT_SLEEP:
			{
				return "sleep nearby monsters every 55 turns";
			}
			case ACT_QUAKE:
			{
				return "earthquake (rad 10) every 50 turns";
			}
			case ACT_TERROR:
			{
				return "terror every 3 * (level+10) turns";
			}
			case ACT_TELE_AWAY:
			{
				return "teleport away every 200 turns";
			}
			case ACT_BANISH_EVIL:
			{
				return "banish evil every 250+d250 turns";
			}
			case ACT_GENOCIDE:
			{
				return "genocide every 500 turns";
			}
			case ACT_MASS_GENO:
			{
				return "mass genocide every 1000 turns";
			}
			case ACT_CHARM_ANIMAL:
			{
				return "charm animal every 300 turns";
			}
			case ACT_CHARM_UNDEAD:
			{
				return "enslave undead every 333 turns";
			}
			case ACT_CHARM_OTHER:
			{
				return "charm monster every 400 turns";
			}
			case ACT_CHARM_ANIMALS:
			{
				return "animal friendship every 500 turns";
			}
			case ACT_CHARM_OTHERS:
			{
				return "mass charm every 750 turns";
			}
			case ACT_SUMMON_ANIMAL:
			{
				return "summon animal every 200+d300 turns";
			}
			case ACT_SUMMON_PHANTOM:
			{
				return "summon phantasmal servant every 200+d200 turns";
			}
			case ACT_SUMMON_ELEMENTAL:
			{
				return "summon elemental every 750 turns";
			}
			case ACT_SUMMON_DEMON:
			{
				return "summon demon every 666+d333 turns";
			}
			case ACT_SUMMON_UNDEAD:
			{
				return "summon undead every 666+d333 turns";
			}
			case ACT_CURE_LW:
			{
				return "remove fear & heal 30 hp every 10 turns";
			}
			case ACT_CURE_MW:
			{
				return "heal 75 hp & wounds every 3+d3 turns";
			}
			case ACT_CURE_POISON:
			{
				return "remove fear and cure poison every 5 turns";
			}
			case ACT_REST_LIFE:
			{
				return "restore life levels every 450 turns";
			}
			case ACT_REST_ALL:
			{
				return "restore stats and life levels every 750 turns";
			}
			case ACT_CURE_700:
			{
				return "heal 700 hit points every 250 turns";
			}
			case ACT_CURE_1000:
			{
				return "heal 1000 hit points every 888 turns";
			}
			case ACT_ESP:
			{
				return "temporary ESP (dur 25+d30) every 200 turns";
			}
			case ACT_BERSERK:
			{
				return "heroism and berserk (dur 50+d50) every 100+d100 turns";
			}
			case ACT_PROT_EVIL:
			{
				return "protect evil (dur level*3 + d25) every 225+d225 turns";
			}
			case ACT_RESIST_ALL:
			{
				return "resist elements (dur 40+d40) every 200 turns";
			}
			case ACT_SPEED:
			{
				return "speed (dur 20+d20) every 250 turns";
			}
			case ACT_XTRA_SPEED:
			{
				return "speed (dur 75+d75) every 200+d200 turns";
			}
			case ACT_WRAITH:
			{
				return "wraith form (level/2 + d(level/2)) every 1000 turns";
			}
			case ACT_INVULN:
			{
				return "invulnerability (dur 8+d8) every 1000 turns";
			}
			case ACT_LIGHT:
			{
				return "light area (dam 2d15) every 10+d10 turns";
			}
			case ACT_MAP_LIGHT:
			{
				return "light (dam 2d15) & map area every 50+d50 turns";
			}
			case ACT_DETECT_ALL:
			{
				return "detection every 55+d55 turns";
			}
			case ACT_DETECT_XTRA:
			{
				return "detection, probing and identify true every 1000 turns";
			}
			case ACT_ID_FULL:
			{
				return "identify true every 750 turns";
			}
			case ACT_ID_PLAIN:
			{
				return "identify spell every 10 turns";
			}
			case ACT_RUNE_EXPLO:
			{
				return "explosive rune every 200 turns";
			}
			case ACT_RUNE_PROT:
			{
				return "rune of protection every 400 turns";
			}
			case ACT_SATIATE:
			{
				return "satisfy hunger every 200 turns";
			}
			case ACT_DEST_DOOR:
			{
				return "destroy doors every 10 turns";
			}
			case ACT_STONE_MUD:
			{
				return "stone to mud every 5 turns";
			}
			case ACT_RECHARGE:
			{
				return "recharging every 70 turns";
			}
			case ACT_ALCHEMY:
			{
				return "alchemy every 500 turns";
			}
			case ACT_DIM_DOOR:
			{
				return "dimension door every 100 turns";
			}
			case ACT_TELEPORT_1:
			{
				return "teleport (range 100) every 50+d50 turns";
			}
			case ACT_TELEPORT_2:
			{
				return "teleport (range 100) every 45 turns";
			}
			case ACT_RECALL:
			{
				return "word of recall every 200 turns";
			}
			default:
			{
				/* No randart activation */
				break;
			}
		}
	}
	else
	{
		/* Some artifacts can be activated */
		switch (o_ptr->activate - 128)
		{
			case ART_NARTHANC:
			{
				return "fire bolt (11d8) every 8+d8 turns";
			}
			case ART_NIMTHANC:
			{
				return "frost bolt (8d8) every 7+d7 turns";
			}
			case ART_DETHANC:
			{
				return "lightning bolt (6d8) every 6+d6 turns";
			}
			case ART_RILIA:
			{
				return "stinking cloud (25) every 4+d4 turns";
			}
			case ART_BELANGIL:
			{
				return "frost ball (100) every 5+d5 turns";
			}
			case ART_DAL:
			{
				return "remove fear and cure poison every 5 turns";
			}
			case ART_RINGIL:
			{
				return "frost ball (200) every 300 turns";
			}
			case ART_DAWN:
			{
				return "summon the Legion of the Dawn every 500+d500 turns";
			}
			case ART_ANDURIL:
			{
				return "fire ball (150) every 400 turns";
			}
			case ART_FIRESTAR:
			{
				return "large fire ball (200) every 100 turns";
			}
			case ART_FEANOR:
			{
				return "haste self (20+d20 turns) every 200 turns";
			}
			case ART_THEODEN:
			{
				return "drain life (200) every 400 turns";
			}
			case ART_TURMIL:
			{
				return "drain life (200) every 70 turns";
			}
			case ART_CASPANION:
			{
				return "door and trap destruction every 10 turns";
			}
			case ART_AVAVIR:
			{
				return "word of recall every 200 turns";
			}
			case ART_WHIRLWIND:
			{
				return "whirlwind every 50+d50 turns";
			}
			case ART_ENERGY:
			{
				return "haste self (20+d20 turns) every 100+d100 turns";
			}
			case ART_ERIRIL:
			{
				return "identify every 10 turns";
			}
			case ART_OLORIN:
			{
				return "probing, detection and full id every 1000 turns";
			}
			case ART_EONWE:
			{
				return "mass genocide every 1000 turns";
			}
			case ART_LOTHARANG:
			{
				return "cure wounds (100) every 3+d3 turns";
			}
			case ART_CATAPULT:
			{
				return "heal (45) every 10 turns";
			}
			case ART_BRAND:
			{
				return "fire branding of bolts every 999 turns";
			}
			case ART_ANGUIREL:
			{
				return "a getaway every 35 turns";
			}
			case ART_AEGLOS:
			{
				return "lightning ball (200) every 500 turns";
			}
			case ART_OROME:
			{
				return "stone to mud every 5 turns";
			}
			case ART_SOULKEEPER:
			{
				return "heal (1000) every 888 turns";
			}
			case ART_BELEGENNON:
			{
				return ("heal (777), curing and heroism every 300 turns");
			}
			case ART_CELEBORN:
			{
				return "genocide every 500 turns";
			}
			case ART_LUTHIEN:
			{
				return "restore life levels every 450 turns";
			}
			case ART_ULMO:
			{
				return "teleport away every 150 turns";
			}
			case ART_KERI:
			{
				return "create food every 100 turns";
			}
			case ART_COLLUIN:
			{
				return "resistance (20+d20 turns) every 111 turns";
			}
			case ART_HOLCOLLETH:
			{
				return "Sleep II every 55 turns";
			}
			case ART_THINGOL:
			{
				return "recharge item every 70 turns";
			}
			case ART_COLANNON:
			{
				return "teleport every 45 turns";
			}
			case ART_TOTILA:
			{
				return "confuse monster every 15 turns";
			}
			case ART_CAMMITHRIM:
			{
				return "magic missile (3d6) every 2 turns";
			}
			case ART_PAURHACH:
			{
				return "fire bolt (11d8) every 8+d8 turns";
			}
			case ART_CORWIN:
			{
				return "frost bolt (8d8) every 7+d7 turns";
			}
			case ART_PAURAEGEN:
			{
				return "lightning bolt (6d8) every 6+d6 turns";
			}
			case ART_PAURNEN:
			{
				return "acid bolt (8d8) every 5+d5 turns";
			}
			case ART_FINGOLFIN:
			{
				return "a magical arrow (250) every 90+d90 turns";
			}
			case ART_HOLHENNETH:
			{
				return "detection every 55+d55 turns";
			}
			case ART_GONDOR:
			{
				return "heal (700) every 250 turns";
			}
			case ART_RAZORBACK:
			{
				return "star ball (1500) every 100 turns";
			}
			case ART_BLADETURNER:
			{
				return
					"breathe elements (1500), berserk rage, bless, and resistance";
			}
			case ART_GALADRIEL:
			{
				return "illumination every 10+d10 turns";
			}
			case ART_ELENDIL:
			{
				return "magic mapping and light every 50+d50 turns";
			}
			case ART_THRAIN:
			{
				return "clairvoyance and recall, draining you";
			}
			case ART_INGWE:
			{
				return "dispel evil (x5) every 300+d300 turns";
			}
			case ART_CARLAMMAS:
			{
				return "protection from evil every 225+d225 turns";
			}
			case ART_BARAHIR:
			{
				return "a strangling attack (200) every 100+d100 turns";
			}
			case ART_TULKAS:
			{
				return "haste self (75+d75 turns) every 150+d150 turns";
			}
			case ART_NARYA:
			{
				return "large fire ball (250) every 225+d225 turns";
			}
			case ART_NENYA:
			{
				return "large frost ball (400) every 325+d325 turns";
			}
			case ART_VILYA:
			{
				return "large lightning ball (500) every 425+d425 turns";
			}
			case ART_POWER:
			{
				return "bizarre things every 450+d450 turns";
			}
			case ART_ELEMENTS:
			{
				return "the elements (800) every 250+d250 turns";
			}
			case ART_DOR:  case ART_TERROR:
			{
				return "rays of fear in every direction";
			}
		}
	}

	if (o_ptr->tval == TV_RING)
	{
		switch (o_ptr->sval)
		{
			case SV_RING_FLAMES:
				return "ball of fire and resist fire";
			case SV_RING_ICE:
				return "ball of cold and resist cold";
			case SV_RING_ACID:
				return "ball of acid and resist acid";
			default:
				return NULL;
		}
	}

	/* Require dragon scale mail */
	if (o_ptr->tval != TV_DRAG_ARMOR) return ("a strange glow");

	/* Branch on the sub-type */
	switch (o_ptr->sval)
	{
		case SV_DRAGON_BLUE:
		{
			return "breathe lightning (330) every 50+d50 turns";
		}
		case SV_DRAGON_WHITE:
		{
			return "breathe frost (370) every 50+d50 turns";
		}
		case SV_DRAGON_BLACK:
		{
			return "breathe acid (430) every 50+d50 turns";
		}
		case SV_DRAGON_GREEN:
		{
			return "breathe poison gas (500) every 50+d50 turns";
		}
		case SV_DRAGON_RED:
		{
			return "breathe fire (670) every 50+d50 turns";
		}
		case SV_DRAGON_MULTIHUED:
		{
			return "breathe multi-hued (840) every 25+d25 turns";
		}
		case SV_DRAGON_BRONZE:
		{
			return "breathe confusion (400) every 50+d50 turns";
		}
		case SV_DRAGON_GOLD:
		{
			return "breathe sound (430) every 50+d50 turns";
		}
		case SV_DRAGON_CHAOS:
		{
			return "breathe chaos/disenchant (740) every 30+d30 turns";
		}
		case SV_DRAGON_LAW:
		{
			return "breathe sound/shards (750) every 30+d30 turns";
		}
		case SV_DRAGON_BALANCE:
		{
			return "breathe balance (850) every 30+d30 turns";
		}
		case SV_DRAGON_SHINING:
		{
			return "breathe light/darkness (670) every 30+d30 turns";
		}
		case SV_DRAGON_POWER:
		{
			return "breathe the elements (1000) every 30+d30 turns";
		}
	}

	/* Oops */
	return "breathe air";
}


/*
 * Fully describe the known information about an item
 */
bool identify_fully_aux(const object_type *o_ptr)
{
	int i = 0, j, k;

	u32b f1, f2, f3;

	cptr info[128], reclaim[128], temp;
	int num_reclaim = 0;
	
	int wid, hgt;

	/* Extract the flags */
	object_flags_known(o_ptr, &f1, &f2, &f3);

	/* Indicate if fully known */
	if (object_known_full(o_ptr))
	{
		info[i++] = "You have full knowledge of this item.";
	}

	/* Mega-Hack -- describe activation if item is identified */
	if ((o_ptr->flags3 & (TR3_ACTIVATE)) && object_known_p(o_ptr))
	{
		info[i++] = "It can be activated for...";
		info[i++] = item_activation(o_ptr);
		info[i++] = "...if it is being worn.";
	}

	/* Books, a hack */
	if ((o_ptr->tval >= TV_BOOKS_MIN) && (o_ptr->tval <= TV_BOOKS_MAX))
	{
		do_cmd_browse_aux(o_ptr);
		return (TRUE);
	}

	/* Figurines, a hack */
	if (o_ptr->tval == TV_FIGURINE)
	{
		info[i++] = "It will transform into a pet when thrown.";
	}

	/* Hack -- describe lite's */
	if (o_ptr->tval == TV_LITE)
	{
		if (o_ptr->flags3 & TR3_INSTA_ART)
		{
			info[i++] = "It provides light (radius 3).";
		}
		else if (o_ptr->sval == SV_LITE_LANTERN)
		{
			info[i++] = "It provides light (radius 2) when fueled.";
		}
		else
		{
			info[i++] = "It provides light (radius 1) when fueled.";
		}
	}


	/* And then describe it fully */

	if (f1 & (TR1_STR))
	{
		if (o_ptr->pval > 0)
		{
			temp = string_make(format("It increases your strength by %+i.",
									  o_ptr->pval));
		}
		else
		{
			temp = string_make(format("It decreases your strength by %+i.",
									  o_ptr->pval));
		}

		info[i++] = temp;
		reclaim[num_reclaim++] = temp;
	}
	if (f1 & (TR1_INT))
	{
		if (o_ptr->pval > 0)
		{
			temp = string_make(format("It increases your intelligence by %+i.",
									  o_ptr->pval));
		}
		else
		{
			temp = string_make(format("It decreases your intelligence by %+i.",
									  o_ptr->pval));
		}

		info[i++] = temp;
		reclaim[num_reclaim++] = temp;
	}
	if (f1 & (TR1_WIS))
	{
		if (o_ptr->pval > 0)
		{
			temp = string_make(format("It increases your wisdom by %+i.",
									  o_ptr->pval));
		}
		else
		{
			temp = string_make(format("It decreases your wisdom by %+i.",
									  o_ptr->pval));
		}

		info[i++] = temp;
		reclaim[num_reclaim++] = temp;
	}
	if (f1 & (TR1_DEX))
	{
		if (o_ptr->pval > 0)
		{
			temp = string_make(format("It increases your dexterity by %+i.",
									  o_ptr->pval));
		}
		else
		{
			temp = string_make(format("It decreases your dexterity by %+i.",
									  o_ptr->pval));
		}

		info[i++] = temp;
		reclaim[num_reclaim++] = temp;
	}
	if (f1 & (TR1_CON))
	{
		if (o_ptr->pval > 0)
		{
			temp = string_make(format("It increases your constitution by %+i.",
									  o_ptr->pval));
		}
		else
		{
			temp = string_make(format("It decreases your constitution by %+i.",
									  o_ptr->pval));
		}

		info[i++] = temp;
		reclaim[num_reclaim++] = temp;
	}
	if (f1 & (TR1_CHR))
	{
		if (o_ptr->pval > 0)
		{
			temp = string_make(format("It increases your charisma by %+i.",
									  o_ptr->pval));
		}
		else
		{
			temp = string_make(format("It decreases your charisma by %+i.",
									  o_ptr->pval));
		}

		info[i++] = temp;
		reclaim[num_reclaim++] = temp;
	}

	if (f1 & (TR1_STEALTH))
	{
		if (o_ptr->pval > 0)
		{
			temp = string_make(format("It increases your stealth by %+i.",
									  o_ptr->pval));
		}
		else
		{
			temp = string_make(format("It decreases your stealth by %+i.",
									  o_ptr->pval));
		}

		info[i++] = temp;
		reclaim[num_reclaim++] = temp;
	}
	if (f1 & (TR1_SEARCH))
	{
		if (o_ptr->pval > 0)
		{
			temp =
				string_make(format
							("It increases your searching ability by %+i.",
							 o_ptr->pval));
		}
		else
		{
			temp =
				string_make(format
							("It decreases your searching ability by %+i.",
							 o_ptr->pval));
		}

		info[i++] = temp;
		reclaim[num_reclaim++] = temp;
	}
	if (f1 & (TR1_INFRA))
	{
		if (o_ptr->pval > 0)
		{
			temp =
				string_make(format
							("It increases your infravision by %i feet.",
							 o_ptr->pval * 10));
		}
		else
		{
			temp =
				string_make(format
							("It decreases your infravision by %i feet.",
							 -o_ptr->pval * 10));
		}

		info[i++] = temp;
		reclaim[num_reclaim++] = temp;
	}
	if (f1 & (TR1_TUNNEL))
	{
		if (o_ptr->pval > 0)
		{
			temp =
				string_make(format
							("It increases your ability to dig by %+i.",
							 o_ptr->pval));
		}
		else
		{
			temp =
				string_make(format
							("It decreases your ability to dig by %+i.",
							 o_ptr->pval));
		}

		info[i++] = temp;
		reclaim[num_reclaim++] = temp;
	}
	if (f1 & (TR1_SPEED))
	{
		if (o_ptr->pval > 0)
		{
			temp = string_make(format("It increases your speed by %+i.",
									  o_ptr->pval));
		}
		else
		{
			temp = string_make(format("It decreases your speed by %+i.",
									  o_ptr->pval));
		}

		info[i++] = temp;
		reclaim[num_reclaim++] = temp;
	}
	if (f1 & (TR1_BLOWS))
	{
		if (o_ptr->pval > 0)
		{
			temp = string_make(format("It provides %i extra blows per turn.",
									  o_ptr->pval));
		}
		else
		{
			temp = string_make(format("It provides %i fewer blows per turn.",
									  -o_ptr->pval));
		}

		info[i++] = temp;
		reclaim[num_reclaim++] = temp;
	}

	if (f1 & (TR1_BRAND_ACID))
	{
		info[i++] = "It does extra damage from acid.";
	}
	if (f1 & (TR1_BRAND_ELEC))
	{
		info[i++] = "It does extra damage from electricity.";
	}
	if (f1 & (TR1_BRAND_FIRE))
	{
		info[i++] = "It does extra damage from fire.";
	}
	if (f1 & (TR1_BRAND_COLD))
	{
		info[i++] = "It does extra damage from frost.";
	}

	if (f1 & (TR1_BRAND_POIS))
	{
		info[i++] = "It poisons your foes.";
	}

	if (f1 & (TR1_CHAOTIC))
	{
		info[i++] = "It produces chaotic effects.";
	}

	if (f1 & (TR1_VAMPIRIC))
	{
		info[i++] = "It drains life from your foes.";
	}

	if (f1 & (TR1_IMPACT))
	{
		info[i++] = "It can cause earthquakes.";
	}

	if (f1 & (TR1_VORPAL))
	{
		info[i++] = "It is very sharp and can cut your foes.";
	}

	if (f1 & (TR1_KILL_DRAGON))
	{
		info[i++] = "It is a great bane of dragons.";
	}
	else if (f1 & (TR1_SLAY_DRAGON))
	{
		info[i++] = "It is especially deadly against dragons.";
	}
	if (f1 & (TR1_SLAY_ORC))
	{
		info[i++] = "It is especially deadly against orcs.";
	}
	if (f1 & (TR1_SLAY_TROLL))
	{
		info[i++] = "It is especially deadly against trolls.";
	}
	if (f1 & (TR1_SLAY_GIANT))
	{
		info[i++] = "It is especially deadly against giants.";
	}
	if (f1 & (TR1_SLAY_DEMON))
	{
		info[i++] = "It strikes at demons with holy wrath.";
	}
	if (f1 & (TR1_SLAY_UNDEAD))
	{
		info[i++] = "It strikes at undead with holy wrath.";
	}
	if (f1 & (TR1_SLAY_EVIL))
	{
		info[i++] = "It fights against evil with holy fury.";
	}
	if (f1 & (TR1_SLAY_ANIMAL))
	{
		info[i++] = "It is especially deadly against natural creatures.";
	}

	if (f2 & (TR2_SUST_STR))
	{
		info[i++] = "It sustains your strength.";
	}
	if (f2 & (TR2_SUST_INT))
	{
		info[i++] = "It sustains your intelligence.";
	}
	if (f2 & (TR2_SUST_WIS))
	{
		info[i++] = "It sustains your wisdom.";
	}
	if (f2 & (TR2_SUST_DEX))
	{
		info[i++] = "It sustains your dexterity.";
	}
	if (f2 & (TR2_SUST_CON))
	{
		info[i++] = "It sustains your constitution.";
	}
	if (f2 & (TR2_SUST_CHR))
	{
		info[i++] = "It sustains your charisma.";
	}

	if (f2 & (TR2_IM_ACID))
	{
		info[i++] = "It provides immunity to acid.";
	}
	if (f2 & (TR2_IM_ELEC))
	{
		info[i++] = "It provides immunity to electricity.";
	}
	if (f2 & (TR2_IM_FIRE))
	{
		info[i++] = "It provides immunity to fire.";
	}
	if (f2 & (TR2_IM_COLD))
	{
		info[i++] = "It provides immunity to cold.";
	}

	if (f2 & (TR2_THROW))
	{
		info[i++] = "It is perfectly balanced for throwing.";
	}

	if (f2 & (TR2_FREE_ACT))
	{
		info[i++] = "It provides immunity to paralysis.";
	}
	if (f2 & (TR2_HOLD_LIFE))
	{
		info[i++] = "It provides resistance to life draining.";
	}
	if (f2 & (TR2_RES_FEAR))
	{
		info[i++] = "It makes you completely fearless.";
	}
	if (f2 & (TR2_RES_ACID))
	{
		info[i++] = "It provides resistance to acid.";
	}
	if (f2 & (TR2_RES_ELEC))
	{
		info[i++] = "It provides resistance to electricity.";
	}
	if (f2 & (TR2_RES_FIRE))
	{
		info[i++] = "It provides resistance to fire.";
	}
	if (f2 & (TR2_RES_COLD))
	{
		info[i++] = "It provides resistance to cold.";
	}
	if (f2 & (TR2_RES_POIS))
	{
		info[i++] = "It provides resistance to poison.";
	}

	if (f2 & (TR2_RES_LITE))
	{
		info[i++] = "It provides resistance to light.";
	}
	if (f2 & (TR2_RES_DARK))
	{
		info[i++] = "It provides resistance to dark.";
	}

	if (f2 & (TR2_RES_BLIND))
	{
		info[i++] = "It provides resistance to blindness.";
	}
	if (f2 & (TR2_RES_CONF))
	{
		info[i++] = "It provides resistance to confusion.";
	}
	if (f2 & (TR2_RES_SOUND))
	{
		info[i++] = "It provides resistance to sound.";
	}
	if (f2 & (TR2_RES_SHARDS))
	{
		info[i++] = "It provides resistance to shards.";
	}

	if (f2 & (TR2_RES_NETHER))
	{
		info[i++] = "It provides resistance to nether.";
	}
	if (f2 & (TR2_RES_NEXUS))
	{
		info[i++] = "It provides resistance to nexus.";
	}
	if (f2 & (TR2_RES_CHAOS))
	{
		info[i++] = "It provides resistance to chaos.";
	}
	if (f2 & (TR2_RES_DISEN))
	{
		info[i++] = "It provides resistance to disenchantment.";
	}

	if (f3 & (TR3_XXX7))
	{
		info[i++] = "It renders you XXX7'ed.";
	}
	if (f3 & (TR3_FEATHER))
	{
		info[i++] = "It allows you to levitate.";
	}
	if (f3 & (TR3_LITE))
	{
		info[i++] = "It provides permanent light.";
	}
	if (f3 & (TR3_SEE_INVIS))
	{
		info[i++] = "It allows you to see invisible monsters.";
	}
	if (f3 & (TR3_TELEPATHY))
	{
		info[i++] = "It gives telepathic powers.";
	}
	if (f3 & (TR3_SLOW_DIGEST))
	{
		info[i++] = "It slows your metabolism.";
	}
	if (f3 & (TR3_REGEN))
	{
		info[i++] = "It speeds your regenerative powers.";
	}
	if (f2 & (TR2_REFLECT))
	{
		info[i++] = "It reflects bolts and arrows.";
	}
	if (f3 & (TR3_SH_FIRE))
	{
		info[i++] = "It produces a fiery sheath.";
	}
	if (f3 & (TR3_SH_ELEC))
	{
		info[i++] = "It produces an electric sheath.";
	}
	if (f3 & (TR3_NO_MAGIC))
	{
		info[i++] = "It produces an anti-magic shell.";
	}
	if (f3 & (TR3_NO_TELE))
	{
		info[i++] = "It prevents teleportation.";
	}
	if (f3 & (TR3_XTRA_MIGHT))
	{
		info[i++] = "It fires missiles with extra might.";
	}
	if (f3 & (TR3_XTRA_SHOTS))
	{
		info[i++] = "It fires missiles excessively fast.";
	}

	if (f3 & (TR3_DRAIN_EXP))
	{
		info[i++] = "It drains your experience.";
	}
	if (f3 & (TR3_TELEPORT))
	{
		info[i++] = "It induces random teleportation.";
	}
	if (f3 & TR3_AGGRAVATE)
	{
		info[i++] = "It aggravates nearby creatures.";
	}

	if (f3 & TR3_BLESSED)
	{
		info[i++] = "It has been blessed by the gods.";
	}

	if (cursed_p(o_ptr))
	{
		if (f3 & TR3_PERMA_CURSE)
		{
			info[i++] = "It is permanently cursed.";
		}
		else if (f3 & TR3_HEAVY_CURSE)
		{
			info[i++] = "It is heavily cursed.";
		}
		else
		{
			info[i++] = "It is cursed.";
		}
	}

	if (f3 & TR3_TY_CURSE)
	{
		info[i++] = "It carries an ancient foul curse.";
	}

	if (f3 & (TR3_IGNORE_ACID))
	{
		info[i++] = "It cannot be harmed by acid.";
	}
	if (f3 & (TR3_IGNORE_ELEC))
	{
		info[i++] = "It cannot be harmed by electricity.";
	}
	if (f3 & (TR3_IGNORE_FIRE))
	{
		info[i++] = "It cannot be harmed by fire.";
	}
	if (f3 & (TR3_IGNORE_COLD))
	{
		info[i++] = "It cannot be harmed by cold.";
	}


	/* No special effects */
	if (!i) return (FALSE);
	
	/* Get size */
	Term_get_size(&wid, &hgt);

	/* Save the screen */
	screen_save();

	/* Erase the screen */
    clear_region(13, 1, hgt);

	/* Label the information */
	prtf(15, 1, "     Item Attributes:");

	/* We will print on top of the map (column 13) */
	for (k = 2, j = 0; j < i; j++)
	{
		/* Show the info */
		prtf(15, k++, info[j]);

		/* Every hgt-2 entries (lines 2 to hgt-3), start over */
		if ((k == hgt - 2) && (j + 1 < i))
		{
			prtf(15, k, "-- more --");
			(void)inkey();
			for (; k > 2; k--) prtf(15, k, "");
		}
	}

	/* Wait for it */
	prtf(13, k, "[Press any key to continue]");
	(void)inkey();

	/* Restore the screen */
	screen_load();

	/* Reclaim the used memory */
	for (i = 0; i < num_reclaim; i++) string_free(reclaim[i]);

	/* Gave knowledge */
	return (TRUE);
}

/*
 * Convert a label into the object pointer
 *  to an item in a list.
 * Return NULL if the label does not indicate a real item.
 */
static object_type *label_to_list(int c, s16b list_start)
{
	int i;

	/* Convert */
	i = (islower(c) ? A2I(c) : -1);

	/* Return the item */
	return (get_list_item(list_start, i));
}

/*
 * Convert a label into the object pointer
 *  to an item in the equipment.
 * Return NULL if the label does not indicate a real item.
 */
static object_type *label_to_equip(int c)
{
	object_type *o_ptr;

	int i;

	/* Convert */
	i = (islower(c) ? A2I(c) : -1);

	/* Verify the index */
	if ((i < 0) || (i >= EQUIP_MAX)) return (NULL);

	/* Get the item */
	o_ptr = &p_ptr->equipment[i];

	/* Empty slots can never be chosen */
	if (!o_ptr->k_idx) return (NULL);

	/* Return the item */
	return (o_ptr);
}

/*
 * The the "choosable" range of objects from the list.
 */
static void get_label_bounds(s16b list, int *n1, int *n2)
{
	object_type *o_ptr;

	int i = -1;

	*n1 = -1;
	*n2 = -1;

	OBJ_ITT_START (list, o_ptr)
	{
		i++;

		if (item_tester_okay(o_ptr))
		{
			/* Get lower bounds */
			if (*n1 == -1) *n1 = i;

			/* Get higher bounds */
			*n2 = i;
		}
	}
	OBJ_ITT_END;
}


/*
 * Determine which equipment slot (if any) an item likes
 */
s16b wield_slot(const object_type *o_ptr)
{
	/* Slot for equipment */
	switch (o_ptr->tval)
	{
		case TV_DIGGING:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		{
			return (EQUIP_WIELD);
		}

		case TV_BOW:
		{
			return (EQUIP_BOW);
		}

		case TV_RING:
		{
			/* Use the right hand first */
			if (!p_ptr->equipment[EQUIP_RIGHT].k_idx) return (EQUIP_RIGHT);

			/* Use the left hand for swapping (by default) */
			return (EQUIP_LEFT);
		}

		case TV_AMULET:
		{
			return (EQUIP_NECK);
		}

		case TV_LITE:
		{
			return (EQUIP_LITE);
		}

		case TV_DRAG_ARMOR:
		case TV_HARD_ARMOR:
		case TV_SOFT_ARMOR:
		{
			return (EQUIP_BODY);
		}

		case TV_CLOAK:
		{
			return (EQUIP_OUTER);
		}

		case TV_SHIELD:
		{
			return (EQUIP_ARM);
		}

		case TV_CROWN:
		case TV_HELM:
		{
			return (EQUIP_HEAD);
		}

		case TV_GLOVES:
		{
			return (EQUIP_HANDS);
		}

		case TV_BOOTS:
		{
			return (EQUIP_FEET);
		}
	}

	/* No slot available */
	return (-1);
}


/*
 * Return a string mentioning how a given item is carried
 */
cptr mention_use(int i)
{
	cptr p;

	/* Examine the location */
	switch (i)
	{
		case EQUIP_WIELD:
		{
			p = "Wielding";
			break;
		}
		case EQUIP_BOW:
		{
			p = "Shooting";
			break;
		}
		case EQUIP_LEFT:
		{
			p = "On left hand";
			break;
		}
		case EQUIP_RIGHT:
		{
			p = "On right hand";
			break;
		}
		case EQUIP_NECK:
		{
			p = "Around neck";
			break;
		}
		case EQUIP_LITE:
		{
			p = "Light source";
			break;
		}
		case EQUIP_BODY:
		{
			p = "On body";
			break;
		}
		case EQUIP_OUTER:
		{
			p = "About body";
			break;
		}
		case EQUIP_ARM:
		{
			p = "On arm";
			break;
		}
		case EQUIP_HEAD:
		{
			p = "On head";
			break;
		}
		case EQUIP_HANDS:
		{
			p = "On hands";
			break;
		}
		case EQUIP_FEET:
		{
			p = "On feet";
			break;
		}
		default:
		{
			p = "In pack";
			break;
		}
	}

	/* Hack -- Heavy weapon */
	if (i == EQUIP_WIELD)
	{
		object_type *o_ptr;
		o_ptr = &p_ptr->equipment[i];
		if (adj_str_hold[p_ptr->stat_ind[A_STR]] < o_ptr->weight / 10)
		{
			p = "Just lifting";
		}
	}

	/* Hack -- Heavy bow */
	if (i == EQUIP_BOW)
	{
		object_type *o_ptr;
		o_ptr = &p_ptr->equipment[i];
		if (adj_str_hold[p_ptr->stat_ind[A_STR]] < o_ptr->weight / 10)
		{
			p = "Just holding";
		}
	}

	/* Return the result */
	return (p);
}


/*
 * Return a string describing how a given item is being worn.
 * Currently, only used for items in the equipment, not inventory.
 */
cptr describe_use(int i)
{
	cptr p;

	switch (i)
	{
		case EQUIP_WIELD:
		{
			p = "attacking monsters with";
			break;
		}
		case EQUIP_BOW:
		{
			p = "shooting missiles with";
			break;
		}
		case EQUIP_LEFT:
		{
			p = "wearing on your left hand";
			break;
		}
		case EQUIP_RIGHT:
		{
			p = "wearing on your right hand";
			break;
		}
		case EQUIP_NECK:
		{
			p = "wearing around your neck";
			break;
		}
		case EQUIP_LITE:
		{
			p = "using to light the way";
			break;
		}
		case EQUIP_BODY:
		{
			p = "wearing on your body";
			break;
		}
		case EQUIP_OUTER:
		{
			p = "wearing on your back";
			break;
		}
		case EQUIP_ARM:
		{
			p = "wearing on your arm";
			break;
		}
		case EQUIP_HEAD:
		{
			p = "wearing on your head";
			break;
		}
		case EQUIP_HANDS:
		{
			p = "wearing on your hands";
			break;
		}
		case EQUIP_FEET:
		{
			p = "wearing on your feet";
			break;
		}
		default:
		{
			p = "invalid item to describe_use()!";
			break;
		}
	}

	/* Hack -- Heavy weapon */
	if (i == EQUIP_WIELD)
	{
		object_type *o_ptr = &p_ptr->equipment[i];
		if (adj_str_hold[p_ptr->stat_ind[A_STR]] < o_ptr->weight / 10)
		{
			p = "just lifting";
		}
	}

	/* Hack -- Heavy bow */
	if (i == EQUIP_BOW)
	{
		object_type *o_ptr = &p_ptr->equipment[i];
		if (adj_str_hold[p_ptr->stat_ind[A_STR]] < o_ptr->weight / 10)
		{
			p = "just holding";
		}
	}

	/* Return the result */
	return (p);
}


/*
 * Hook to specify "weapon"
 */
bool item_tester_hook_weapon(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_SWORD:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_DIGGING:
		case TV_BOW:
		case TV_BOLT:
		case TV_ARROW:
		case TV_SHOT:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}

/*
 * Hook to specify "melee weapon"
 */
bool item_tester_hook_melee_weapon(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_SWORD:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_DIGGING:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}

/*
 * Hook to specify "non-sword melee"
 */
bool item_tester_hook_nonsword(const object_type *o_ptr)
{
	if ((o_ptr->tval == TV_HAFTED) || (o_ptr->tval == TV_POLEARM))
	{
		return (TRUE);
	}

	return (FALSE);
}



/*
 * Hook to specify "ammo"
 */
bool item_tester_hook_ammo(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_SHOT:
		case TV_ARROW:
		case TV_BOLT:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}

/*
 * Hook to specify "Bows+arrows"
 */
bool item_tester_hook_fletcher(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_SHOT:
		case TV_ARROW:
		case TV_BOLT:
		case TV_BOW:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}


/*
 * Hook to specify "armour"
 */
bool item_tester_hook_armour(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_DRAG_ARMOR:
		case TV_HARD_ARMOR:
		case TV_SOFT_ARMOR:
		case TV_SHIELD:
		case TV_CLOAK:
		case TV_CROWN:
		case TV_HELM:
		case TV_BOOTS:
		case TV_GLOVES:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}

/*
 * Hook to specify "soft armour"
 */
bool item_tester_hook_soft_armour(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_SOFT_ARMOR:
		case TV_CLOAK:
		case TV_BOOTS:
		case TV_GLOVES:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}

/*
 * Hook to specify "hard armour"
 */
bool item_tester_hook_hard_armour(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_DRAG_ARMOR:
		case TV_HARD_ARMOR:
		case TV_SHIELD:
		case TV_CROWN:
		case TV_HELM:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}


/*
 * Hook to specify "helm or crown"
 */
bool item_tester_hook_helm(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_CROWN:
		case TV_HELM:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}

/*
 * Hook to specify "pure hard armour"
 */
bool item_tester_hook_pure_hard_armour(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_DRAG_ARMOR:
		case TV_HARD_ARMOR:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}



/*
 * Check if an object is weapon or armour (but not arrow, bolt, or shot)
 */
bool item_tester_hook_weapon_armour(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_SWORD:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_BOW:
		case TV_DRAG_ARMOR:
		case TV_HARD_ARMOR:
		case TV_SOFT_ARMOR:
		case TV_SHIELD:
		case TV_CLOAK:
		case TV_CROWN:
		case TV_HELM:
		case TV_BOOTS:
		case TV_GLOVES:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}


/*
 * The "wearable" tester
 */
bool item_tester_hook_wear(const object_type *o_ptr)
{
	/* Check for a usable slot */
	if (wield_slot(o_ptr) >= EQUIP_WIELD) return (TRUE);

	/* Assume not wearable */
	return (FALSE);
}


/*
 * Determine if something is rechargable.
 */
bool item_tester_hook_recharge(const object_type *o_ptr)
{
	/* Staffs */
	if (o_ptr->tval == TV_STAFF) return (TRUE);

	/* Wands */
	if (o_ptr->tval == TV_WAND) return (TRUE);

	/* Rods */
	if (o_ptr->tval == TV_ROD) return (TRUE);

	/* Nope */
	return (FALSE);
}

/*
 * Determine if something is a 'jewel'
 */
bool item_tester_hook_jewel(const object_type *o_ptr)
{
	/* Rings */
	if (o_ptr->tval == TV_RING) return (TRUE);

	/* Amulets */
	if (o_ptr->tval == TV_AMULET) return (TRUE);

	/* Nope */
	return (FALSE);
}



/* Hack - match item_tester_tval */
bool item_tester_hook_tval(const object_type *o_ptr)
{
	/* A match? */
	if (o_ptr->tval == item_tester_tval) return (TRUE);

	/* Nope */
	return (FALSE);
}


bool item_tester_hook_is_blessed(const object_type *o_ptr)
{
	u32b f1, f2, f3;
	object_flags_known(o_ptr, &f1, &f2, &f3);

	/* Is it blessed? */
	if (f3 & TR3_BLESSED) return (TRUE);

	/* Check for unallowable weapons */
	if ((o_ptr->tval == TV_SWORD)
		|| (o_ptr->tval == TV_POLEARM)) return (FALSE);

	/* Everthing else is ok */
	return (TRUE);
}

bool item_tester_hook_is_good(const object_type *o_ptr)
{
	if (!object_known_p(o_ptr)) return (FALSE);

	if (cursed_p(o_ptr)) return (FALSE);

	/* Ego item or artifact */
	if (o_ptr->xtra_name) return (TRUE);

	/* Positve AC bonus */
	if (o_ptr->to_a > 0) return (TRUE);

	/* Good attack + defence */
	if (o_ptr->to_h + o_ptr->to_d > 0) return (TRUE);

	/* Everthing else isn't good */
	return (FALSE);
}


bool item_tester_hook_is_great(const object_type *o_ptr)
{
	if (!object_known_p(o_ptr)) return (FALSE);

	if (cursed_p(o_ptr)) return (FALSE);

	/* Ego item or artifact */
	if (o_ptr->xtra_name) return (TRUE);

	/* Everthing else isn't great */
	return (FALSE);
}


bool item_tester_hook_is_book(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_SORCERY_BOOK:
		case TV_NATURE_BOOK:
		case TV_CHAOS_BOOK:
		case TV_DEATH_BOOK:
		case TV_TRUMP_BOOK:
		case TV_ARCANE_BOOK:
		case TV_LIFE_BOOK:

			/* It is a book */
			return (TRUE);
	}

	/* It isn't a book */
	return (FALSE);
}


/* Hack: Check if a spellbook is one of the realms we can use. -- TY */

static bool check_book_realm(const byte book_tval)
{
	return (REALM1_BOOK == book_tval || REALM2_BOOK == book_tval);
}


/*
 * Check an item against the item tester info
 */
bool item_tester_okay(const object_type *o_ptr)
{
	/* Paranoia */
	if (!o_ptr) return (FALSE);

	/* Hack -- allow listing empty slots */
	if (item_tester_full) return (TRUE);

	/* Require an item */
	if (!o_ptr->k_idx) return (FALSE);

	/* Hack -- ignore "gold" */
	if (o_ptr->tval == TV_GOLD) return (FALSE);

	/* Check the tval */
	if (item_tester_tval)
	{
		/* Is it a spellbook? If so, we need a hack -- TY */
		if ((item_tester_tval <= TV_DEATH_BOOK) &&
			(item_tester_tval >= TV_LIFE_BOOK))
			return check_book_realm(o_ptr->tval);
		else if (item_tester_tval != o_ptr->tval) return (FALSE);
	}

	/* Check the hook */
	if (item_tester_hook)
	{
		if (!(*item_tester_hook) (o_ptr)) return (FALSE);
	}

	/* Assume okay */
	return (TRUE);
}

static bool item_is_recharging(object_type *o_ptr)
{
	object_kind *k_ptr = &k_info[o_ptr->k_idx];
	int power;

	/* Is the item recharging? */
	if (o_ptr->timeout &&
		((o_ptr->tval != TV_LITE) || (o_ptr->flags3 & TR3_INSTA_ART)))
	{
		if (o_ptr->tval == TV_ROD)
		{
			/* Paranoia. */
			if (!k_ptr->pval) return (FALSE);

			/*
			 * Find out how many rods are charging, by dividing
			 * current timeout by each rod's maximum timeout.
			 */
			power = (o_ptr->timeout + (k_ptr->pval - 1)) / k_ptr->pval;

			/* Only darken fully-charging stacks. */
			if (power >= o_ptr->number) return (TRUE);
		}
		else
		{
			return (TRUE);
		}
	}

	return (FALSE);
}


/*
 * Choice window "shadow" of the "show_inven()" function
 */
void display_inven(void)
{
	int i = 0;
	object_type *o_ptr;
	cptr attr;

	char tmp_val[80];

	int wid, hgt;

	/* Get size */
	Term_get_size(&wid, &hgt);

	/* Display the pack */
	OBJ_ITT_START (p_ptr->inventory, o_ptr)
	{
		/* Start with an empty "index" */
		tmp_val[0] = tmp_val[1] = ' ';
		tmp_val[2] = 0;

		/* Is this item "acceptable"? */
		if (item_tester_okay(o_ptr))
		{
			/* Prepare an "index" */
			tmp_val[0] = I2A(i);

			/* Bracket the "index" --(-- */
			tmp_val[1] = ')';
		}

		/* Display the index (or blank space) */
		prtf(0, i, tmp_val);

		/* Get a color */
		attr = color_seq[tval_to_attr[o_ptr->tval % 128]];

		/* Grey out charging items */
		if (item_is_recharging(o_ptr)) attr = CLR_L_DARK;

		/* Display the entry itself */
		put_fstr(3, i, "%s" CLR_SET_DEFAULT "%v", attr, OBJECT_FMT(o_ptr, TRUE, 3));

		/* Display the weight if needed */
		if (show_weights && o_ptr->weight)
		{
			int wgt = o_ptr->weight * o_ptr->number;
			prtf(wid - 9, i, "%3d.%1d lb", wgt / 10, wgt % 10);
		}

		/* Count items in inventory */
		i++;
	}
	OBJ_ITT_END;

	/* Erase the rest of the window */
    clear_from(i);
}


/*
 * Choice window "shadow" of the "show_equip()" function
 */
void display_equip(void)
{
	int i, n;
	object_type *o_ptr;
	cptr attr;
	char tmp_val[80];
	char o_name[256];


	int wid, hgt;

	/* Get size */
	Term_get_size(&wid, &hgt);

	/* Display the equipment */
	for (i = 0; i < EQUIP_MAX; i++)
	{
		/* Examine the item */
		o_ptr = &p_ptr->equipment[i];

		/* Start with an empty "index" */
		tmp_val[0] = tmp_val[1] = ' ';
		tmp_val[2] = 0;

		/* Is this item "acceptable"? */
		if (item_tester_okay(o_ptr))
		{
			/* Prepare an "index" */
			tmp_val[0] = I2A(i);

			/* Bracket the "index" --(-- */
			tmp_val[1] = ')';
		}

		/* Display the index (or blank space) */
		prtf(0, i, tmp_val);

		/* Obtain the length of the description */
		n = strlen(o_name);

		/* Get the color */
		attr = color_seq[tval_to_attr[o_ptr->tval % 128]];

		/* Grey out charging items */
		if (item_is_recharging(o_ptr)) attr = CLR_L_DARK;

		/* Display the entry itself */
		put_fstr(3, i, "%s" CLR_SET_DEFAULT "%v", attr, OBJECT_FMT(o_ptr, TRUE, 3));

		/* Display the slot description (if needed) */
		if (show_labels)
		{
			prtf(wid - 19, i, "<--");
			put_fstr(wid - 15, i, mention_use(i));
		}

		/* Display the weight (if needed) */
		if (show_weights && o_ptr->weight)
		{
			int wgt = o_ptr->weight * o_ptr->number;
			int col = (show_labels ? wid - 28 : wid - 9);
			put_fstr(col, i, "%3d.%1d lb", wgt / 10, wgt % 10);
		}
	}

	/* Erase the rest of the window */
    clear_from(EQUIP_MAX);
}


/*
 * Display a list of objects.
 * This can be used to show the player's inventory,
 * or to show a list of floor items.
 *
 * Hack -- do not display "trailing" empty slots
 */
void show_list(s16b o_list_ptr)
{
	int i, j;
	int k, l;

	int col, len, lim;
	object_type *o_ptr;

	char o_name[256];

	object_type *out_object[23];
	int out_index[23];
	cptr out_color[23];
	char out_desc[23][256];

	byte a;
	char c;

	int wid, hgt;

	/* Get size */
	Term_get_size(&wid, &hgt);

	/* Default "max-length" */
	len = wid - 51;

	/* Maximum space allowed for descriptions */
	lim = wid - 4;

	/* Require space for weight (if needed) */
	if (show_weights) lim -= 9;

	/* Require space for icon */
	lim -= 2;

	/* Initialise counters */
	i = -1;
	j = 0;
	k = -1;

	/* Display the inventory */
	OBJ_ITT_START (o_list_ptr, o_ptr)
	{
		i++;

		/* Is this item acceptable? */
		if (!item_tester_okay(o_ptr)) continue;

		/* Advance to next "line" */
		k++;

		/* Describe the object */
		object_desc(o_name, o_ptr, TRUE, 3, 256);

		/* Hack -- enforce max length */
		o_name[lim] = '\0';

		/* Save the object index, color, and description */
		out_index[k] = i;
		out_object[k] = o_ptr;
		out_color[k] = color_seq[tval_to_attr[o_ptr->tval % 128]];

		/* Grey out charging items */
		if (item_is_recharging(o_ptr)) out_color[k] = CLR_L_DARK;

		(void)strcpy(out_desc[k], o_name);

		/* Find the predicted "line length" */
		l = strlen(out_desc[k]) + 5;

		/* Be sure to account for the weight */
		if (show_weights) l += 9;

		/* Account for icon if displayed */
		l += 2;

		/* Maintain the maximum length */
		if (l > len) len = l;
	}
	OBJ_ITT_END;

	/* Hack -- Find a column to start in and to put weights */
	if (len > wid - 4)
	{
		col = 0;
		lim = wid - 9;
	}
	else
	{
		col = (wid - len - 1) / 2;
		lim = col + len - 9;
	}

	/* Output each entry */
	for (j = 0; j <= k; j++)
	{
		/* Get the item */
		o_ptr = out_object[j];

		/* Clear the line */
		prtf(col ? col - 2 : col, j + 1, "");

		/* Clear the line with the (possibly indented) index */
		put_fstr(col, j + 1, "%c)", I2A(out_index[j]));

		/* Display graphics for object, if desired */
		a = object_attr(o_ptr);
		c = object_char(o_ptr);

		/* Fake monochrome */
		if (!use_color || ironman_moria)
		{
			/* Hack - no equippy char */
			a = TERM_WHITE;
			c = ' ';
		}

		Term_draw(col + 3, j + 1, a, c);

		/* Display the entry itself */
		put_fstr(col + 5, j + 1, "%s" CLR_SET_DEFAULT "%s",
				 out_color[j], out_desc[j]);

		/* Display the weight if needed */
		if (show_weights)
		{
			int wgt = o_ptr->weight * o_ptr->number;
			put_fstr(lim, j + 1, "%3d.%1d lb", wgt / 10, wgt % 10);
		}
	}

	/* Make a "shadow" below the list (only if needed) */
	if (j && (j < hgt - 2)) prtf(col ? col - 2 : col, j + 1, "");
}


/*
 * Display the equipment.
 */
void show_equip(void)
{
	int i, j, k, l;
	int col, len, lim;

	object_type *o_ptr;

	char o_name[256];
	int out_index[23];
	cptr out_color[23];
	char out_desc[23][256];

	byte a;
	char c;

	int wid, hgt;

	/* Get size */
	Term_get_size(&wid, &hgt);


	/* Maximal length */
	len = wid - 51;

	/* Maximum space allowed for descriptions */
	lim = wid - 4;

	/* Require space for labels (if needed) */
	if (show_labels) lim -= (14 + 2);

	/* Require space for weight (if needed) */
	if (show_weights) lim -= 9;

	/* Old show_equip_graph option Perm. on. */
	lim -= 2;

	/* Scan the equipment list */
	for (k = 0, i = 0; i < EQUIP_MAX; i++)
	{
		o_ptr = &p_ptr->equipment[i];

		/* Is this item acceptable? */
		if (!item_tester_okay(o_ptr)) continue;

		/* Description */
		object_desc(o_name, o_ptr, TRUE, 3, 256);

		/* Truncate the description */
		o_name[lim] = '\0';

		/* Save the color */
		out_index[k] = i;
		out_color[k] = color_seq[tval_to_attr[o_ptr->tval % 128]];

		/* Grey out charging items */
		if (item_is_recharging(o_ptr)) out_color[k] = CLR_L_DARK;

		(void)strcpy(out_desc[k], o_name);

		/* Extract the maximal length (see below) */
		l = strlen(out_desc[k]) + (2 + 3);

		/* Increase length for labels (if needed) */
		if (show_labels) l += (14 + 2);

		/* Increase length for weight (if needed) */
		if (show_weights) l += 9;

		/* old show_equip_graph option perm. on. */
		l += 2;

		/* Maintain the max-length */
		if (l > len) len = l;

		/* Advance the entry */
		k++;
	}

	/* Hack -- Find a column to start in and to put weights */
	if (len > wid - 4)
	{
		col = 0;
		lim = wid - 9;
	}
	else
	{
		col = (wid - len - 1) / 2;
		lim = col + len - 9;
	}

	/* Output each entry */
	for (j = 0; j < k; j++)
	{
		/* Get the index */
		i = out_index[j];

		/* Get the item */
		o_ptr = &p_ptr->equipment[i];

		/* Clear the line */
		prtf(col ? col - 2 : col, j + 1, "");

		/* Clear the line with the (possibly indented) index */
		put_fstr(col, j + 1,"%c)", I2A(i));

		/* Show_equip_graph perm. on. */
		a = object_attr(o_ptr);
		c = object_char(o_ptr);

		/* Hack - if no object, don't display the 'nothing' symbol */
		if (!o_ptr->number) c = ' ';

		/* Fake monochrome */
		if (!use_color || ironman_moria)
		{
			/* Hack - no equippy char */
			a = TERM_WHITE;
			c = ' ';
		}

		Term_draw(col + 3, j + 1, a, c);


		/* Use labels */
		if (show_labels)
		{
			/* Mention the use */
			put_fstr(col + 5, j + 1, "%-14s: ", mention_use(i));

			/* Display the entry itself */
			put_fstr(col + 21, j + 1, "%s" CLR_SET_DEFAULT "%s",
					 out_color[j], out_desc[j]);
		}

		/* No labels */
		else
		{
			/* Display the entry itself */
			put_fstr(col + 5, j + 1, "%s" CLR_SET_DEFAULT "%s",
					 out_color[j], out_desc[j]);
		}

		/* Display the weight if needed */
		if (show_weights && o_ptr->number)
		{
			int wgt = o_ptr->weight * o_ptr->number;
			put_fstr(lim, j + 1, "%3d.%d lb", wgt / 10, wgt % 10);
		}
	}

	/* Make a "shadow" below the list (only if needed) */
	if (j && (j < hgt - 2)) prtf(col ? col - 2 : col, j + 1, "");
}


/*
 * Flip "inven" and "equip" in any sub-windows
 */
void toggle_inven_equip(void)
{
	int j;

	/* Scan windows */
	for (j = 0; j < ANGBAND_TERM_MAX; j++)
	{
		/* Unused */
		if (!angband_term[j]) continue;

		/* Flip inven to equip */
		if (window_flag[j] & (PW_INVEN))
		{
			/* Flip flags */
			window_flag[j] &= ~(PW_INVEN);
			window_flag[j] |= (PW_EQUIP);

			/* Window stuff */
			p_ptr->window |= (PW_EQUIP);
		}

		/* Flip inven to equip */
		else if (window_flag[j] & (PW_EQUIP))
		{
			/* Flip flags */
			window_flag[j] &= ~(PW_EQUIP);
			window_flag[j] |= (PW_INVEN);

			/* Window stuff */
			p_ptr->window |= (PW_INVEN);
		}
	}
}



/*
 * Verify the choice of an item.
 *
 * The item can be negative to mean "item on floor".
 */
static bool verify(cptr prompt, object_type *o_ptr)
{
	/* Query */
	return (get_check("%s %v? ", prompt, OBJECT_FMT(o_ptr, TRUE, 3)));
}


/*
 * Hack -- allow user to "prevent" certain choices
 */
static bool get_item_allow(object_type *o_ptr)
{
	cptr s;

	/* No inscription */
	if (!o_ptr->inscription) return (TRUE);

	/* Find a '!' */
	s = strchr(quark_str(o_ptr->inscription), '!');

	/* Process preventions */
	while (s)
	{
		/* Check the "restriction" */
		if ((s[1] == p_ptr->command_cmd) || (s[1] == '*'))
		{
			/* Verify the choice */
			if (!verify("Really try", o_ptr)) return (FALSE);
		}

		/* Find another '!' */
		s = strchr(s + 1, '!');
	}

	/* Allow it */
	return (TRUE);
}


/*
 * Find the "first" inventory object with the given "tag".
 *
 * A "tag" is a char "n" appearing as "@n" anywhere in the
 * inscription of an object.
 *
 * Also, the tag "@xn" will work as well, where "n" is a tag-char,
 * and "x" is the "current" command_cmd code.
 */
static object_type *get_tag(bool *inven, char tag)
{
	int i;

	cptr s;

	object_type *o_ptr;


	/* Check inventory */
	OBJ_ITT_START (p_ptr->inventory, o_ptr)
	{
		/* Skip empty inscriptions */
		if (!o_ptr->inscription) continue;

		/* Find a '@' */
		s = strchr(quark_str(o_ptr->inscription), '@');

		/* Process all tags */
		while (s)
		{
			/* Check the normal tags */
			if (s[1] == tag)
			{
				/* Save the actual inventory ID */
				*inven = TRUE;

				/* Success */
				return (o_ptr);
			}

			/* Check the special tags */
			if ((s[1] == p_ptr->command_cmd) && (s[2] == tag))
			{
				/* Save the actual inventory ID */
				*inven = TRUE;

				/* Success */
				return (o_ptr);
			}

			/* Find another '@' */
			s = strchr(s + 1, '@');
		}
	}
	OBJ_ITT_END;

	/* Check equipment */
	for (i = 0; i < EQUIP_MAX; i++)
	{
		object_type *o_ptr = &p_ptr->equipment[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Skip empty inscriptions */
		if (!o_ptr->inscription) continue;

		/* Find a '@' */
		s = strchr(quark_str(o_ptr->inscription), '@');

		/* Process all tags */
		while (s)
		{
			/* Check the normal tags */
			if (s[1] == tag)
			{
				/* Save the actual inventory ID */
				*inven = FALSE;

				/* Success */
				return (o_ptr);
			}

			/* Check the special tags */
			if ((s[1] == p_ptr->command_cmd) && (s[2] == tag))
			{
				/* Save the actual inventory ID */
				*inven = FALSE;

				/* Success */
				return (o_ptr);
			}

			/* Find another '@' */
			s = strchr(s + 1, '@');
		}
	}

	/* No such tag */
	return (NULL);
}

/*
 * test_floor --
 *
 * Return the first valid item at a location +
 * Return the number of valid items at the location.
 *
 * Valid flags are:
 *
 *		mode & 0x01 -- Item tester
 *		mode & 0x02 -- Marked items only
 */
object_type *test_floor(int *num, cave_type *c_ptr, int mode)
{
	object_type *q_ptr = NULL;
	object_type *o_ptr;

	/* No items yet */
	*num = 0;

	/* Scan all objects in the grid */
	OBJ_ITT_START (c_ptr->o_idx, o_ptr)
	{
		/* Item tester */
		if ((mode & 0x01) && !item_tester_okay(o_ptr)) continue;

		/* Marked */
		if ((mode & 0x02) && !(o_ptr->info & OB_SEEN)) continue;

		/* Save first item */
		if (!q_ptr) q_ptr = o_ptr;

		/* Count objects */
		(*num)++;
	}
	OBJ_ITT_END;

	/* First item in list, if it exists */
	return (q_ptr);
}

/*
 * Toggle the inventory and equipment terms when needed
 */
static bool toggle_windows(bool toggle, int command_wrk)
{
	bool ni = FALSE;
	bool ne = FALSE;

	int j;

	/* Scan windows */
	for (j = 0; j < ANGBAND_TERM_MAX; j++)
	{
		/* Unused */
		if (!angband_term[j]) continue;

		/* Count windows displaying inven */
		if (window_flag[j] & (PW_INVEN)) ni = TRUE;

		/* Count windows displaying equip */
		if (window_flag[j] & (PW_EQUIP)) ne = TRUE;
	}

	/* Toggle if needed */
	if ((command_wrk == (USE_EQUIP) && ni && !ne) ||
		(command_wrk == (USE_INVEN) && !ni && ne))
	{
		/* Toggle */
		toggle_inven_equip();

		/* Track toggles */
		toggle = !toggle;
	}

	/* Update */
	p_ptr->window |= (PW_INVEN | PW_EQUIP);

	/* Redraw windows */
	window_stuff();

	return (toggle);
}


/*
 * Show the prompt for items
 */
static void show_item_prompt(bool inven, bool equip, bool floor, cptr pmt,
                             int command_wrk)
{
	int i;

	int n1, n2;

	char out_val[160];

	object_type *eo_ptr;
	
	int len = 0;

	switch (command_wrk)
	{
		case USE_INVEN:
		{
			/* Extract the legal requests */
			get_label_bounds(p_ptr->inventory, &n1, &n2);

			/* Redraw */
			show_list(p_ptr->inventory);

			/* Begin the prompt */
			len = strnfmt(out_val, 160, "Inven:");

			/* Append */
			if (equip) strnfcat(out_val, 160, &len, " / for Equip,");

			/* Append */
			if (floor) strnfcat(out_val, 160, &len, " - for floor,");

			break;
		}

		case USE_EQUIP:
		{
			/* Nothing yet */
			n1 = -1;
			n2 = -1;

			/* Test for usable equipment */
			for (i = 0; i < EQUIP_MAX; i++)
			{
				eo_ptr = &p_ptr->equipment[i];

				if (item_tester_okay(eo_ptr))
				{
					/* Get lower bound */
					if (n1 == -1) n1 = i;

					/* Get higher bound */
					n2 = i;
				}
			}

			/* Redraw */
			show_equip();

			/* Begin the prompt */
			len = strnfmt(out_val, 160, "Equip:");

			/* Append */
			if (inven) strnfcat(out_val, 160, &len, " / for Inven,");

			/* Append */
			if (floor) strnfcat(out_val, 160, &len, " - for floor,");

			break;
		}

		case USE_FLOOR:
		{
			cave_type *c_ptr = area(p_ptr->px, p_ptr->py);

			/* Extract the legal requests */
			get_label_bounds(p_ptr->inventory, &n1, &n2);

			if (easy_floor)
			{
				/* Redraw  */
				show_list(c_ptr->o_idx);

				/* Begin the prompt */
				len = strnfmt(out_val, 160, "Floor:");

				/* Append */
				if (inven)
				{
					strnfcat(out_val, 160, &len, " / for Inven,");
				}
				else if (equip)
				{
					strnfcat(out_val, 160, &len, " / for Equip,");
				}
			}
			else
			{
				/* Begin the prompt */
				len = strnfmt(out_val, 160, "Top item on floor: '-'");
			}

			break;
		}
	}

	/* Show the prompt */
	prtf(0, 0, "(%s ESC) %s", out_val, pmt);
}

/*
 * Remember the object we selected so that
 * we can repeat the action if required.
 */
static void save_object_choice(object_type *o_ptr, int command_wrk)
{
	int index = -1;

	cave_type *c_ptr;

	/* Paranoia */
	if (!o_ptr) return;

	/* Save type of prompt */
	repeat_push(command_wrk);

	switch (command_wrk)
	{
		case USE_INVEN:
		{
			index = get_item_position(p_ptr->inventory, o_ptr);
			break;
		}

		case USE_EQUIP:
		{
			index = GET_ARRAY_INDEX(p_ptr->equipment, o_ptr);
			break;
		}

		case USE_FLOOR:
		{
			c_ptr = area(p_ptr->px, p_ptr->py);

			index = get_item_position(c_ptr->o_idx, o_ptr);
			break;
		}
	}

	/* Save the index */
	repeat_push(index);
}

/*
 * Recall which object we have previously used.
 */
static object_type *recall_object_choice(int *command_wrk)
{
	int type;
	int index;

	cave_type *c_ptr;
	object_type *o_ptr = NULL;

	/* Get type of prompt */
	if (!repeat_pull(&type))
	{
		/* Not a valid repeat - return invalid object */
		return (NULL);
	}

	/* Paranoia */
	if (type == -1)
	{
		/* Invalid repeat - reset it */
		repeat_clear();
		return (NULL);
	}

	/* Set type of prompt */
	*command_wrk = type;

	/* Get index */
	if (!repeat_pull(&index))
	{
		/* Not a valid repeat - return invalid object */
		return (NULL);
	}

	/* Paranoia */
	if (index == -1)
	{
		/* Invalid repeat - reset it */
		repeat_clear();
		return (NULL);
	}

	/* Get item */
	switch (type)
	{
		case USE_INVEN:
		{
			o_ptr = get_list_item(p_ptr->inventory, index);
			break;
		}

		case USE_EQUIP:
		{
			if ((index < 0) || (index >= EQUIP_MAX))
			{
				o_ptr = NULL;
			}
			else
			{
				o_ptr = &p_ptr->equipment[index];
			}

			break;
		}

		case USE_FLOOR:
		{
			c_ptr = area(p_ptr->px, p_ptr->py);

			o_ptr = get_list_item(c_ptr->o_idx, index);
			break;
		}

		default:
		{
			/* Invalid repeat - reset it */
			repeat_clear();
			return (NULL);
		}
	}


	/* Validate the item */
	if (item_tester_okay(o_ptr))
	{
		/* Forget the item_tester_tval restriction */
		item_tester_tval = 0;

		/* Forget the item_tester_hook restriction */
		item_tester_hook = NULL;

		/* Success */
		return (o_ptr);
	}

	/* Invalid repeat - reset it */
	repeat_clear();
	return (NULL);
}


/*
 * Let the user select an item and return a pointer to it.
 *
 * The selected item must satisfy the "item_tester_hook()" function,
 * if that hook is set, and the "item_tester_tval", if that value is set.
 *
 * All "item_tester" restrictions are cleared before this function returns.
 *
 * The user is allowed to choose acceptable items from the equipment,
 * inventory, or floor, respectively, if the proper flag was given,
 * and there are any acceptable items in that location.
 *
 * The equipment or inventory are displayed (even if no acceptable
 * items are in that location) if the proper flag was given.
 *
 * If there are no acceptable items available anywhere, and "str" is
 * not NULL, then it will be used as the text of a warning message
 * before the function returns.
 *
 * Note that the user must press "-" to specify the item on the floor,
 * and there is no way to "examine" the item on the floor, while the
 * use of "capital" letters will "examine" an inventory/equipment item,
 * and prompt for its use.
 *
 * Global "p_ptr->command_new" is used when viewing the inventory or equipment
 * to allow the user to enter a command while viewing those screens, and
 * also to induce "auto-enter" of stores, and other such stuff.
 *
 * We always erase the prompt when we are done, leaving a blank line,
 * or a warning message, if appropriate, if no items are available.
 *
 * This version of get_item() includes the modifications due to the
 * easy_floor flag.  This flag changes how items on the floor are treated.
 */
object_type *get_item(cptr pmt, cptr str, int mode)
{
	cave_type *c_ptr = area(p_ptr->px, p_ptr->py);

	char which;

	int i;

	bool done = FALSE;

	bool equip = FALSE;
	bool inven = FALSE;
	bool floor = FALSE;

	bool allow_equip = FALSE;
	bool allow_inven = FALSE;
	bool allow_floor = FALSE;

	int command_wrk;

	bool toggle = FALSE;

	int floor_num;

	/* First Floor item */
	object_type *fo_ptr;

	/* Temp item */
	object_type *q_ptr;

	object_type *o_ptr;

	/* Extract args */
	if (mode & (USE_EQUIP)) equip = TRUE;
	if (mode & (USE_INVEN)) inven = TRUE;
	if (mode & (USE_FLOOR)) floor = TRUE;


	/* Paranoia XXX XXX XXX */
	message_flush();

	/* Not done */
	done = FALSE;

	/* Test for equipment */
	if (equip)
	{
		for (i = 0; i < EQUIP_MAX; i++)
		{
			q_ptr = &p_ptr->equipment[i];

			/* Only want valid items */
			if (q_ptr->k_idx && item_tester_okay(q_ptr))
			{
				allow_equip = TRUE;

				break;
			}
		}
	}

	/* Scan all objects in the grid */
	fo_ptr = test_floor(&floor_num, c_ptr, 0x01);

	/* Accept floor */
	if (floor_num && floor) allow_floor = TRUE;

	/* Scan inventory */
	if (inven)
	{
		OBJ_ITT_START (p_ptr->inventory, q_ptr)
		{
			/* Only want valid items */
			if (item_tester_okay(q_ptr))
			{
				allow_inven = TRUE;

				break;
			}
		}
		OBJ_ITT_END;
	}

	/* Require at least one legal choice */
	if (!allow_inven && !allow_equip && !allow_floor)
	{
		/* Warning if needed */
		if (str) msgf(str);

		/* Forget the item_tester_tval restriction */
		item_tester_tval = 0;

		/* Forget the item_tester_hook restriction */
		item_tester_hook = NULL;

		/* Done */
		return (FALSE);
	}

	/* Analyze choices */

	/* Use inventory if allowed */
	if (allow_inven)
	{
		command_wrk = (USE_INVEN);
	}

	/* Use equipment if allowed */
	else if (allow_equip)
	{
		command_wrk = (USE_EQUIP);
	}

	/* Use floor if allowed */
	else if (allow_floor)
	{
		command_wrk = (USE_FLOOR);
	}

	/* Get the saved item index */
	o_ptr = recall_object_choice(&command_wrk);

	if (o_ptr)
	{
		/* Forget the item_tester_tval restriction */
		item_tester_tval = 0;

		/* Forget the item_tester_hook restriction */
		item_tester_hook = NULL;

		/* Done */
		return (o_ptr);
	}

	/* Hack -- start out in "display" mode */

	/* Save screen */
	screen_save();

	/* Repeat until done */
	while (!done)
	{
		/* Make sure object is in the unselected state */
		o_ptr = NULL;
		
		/* Activate the correct term info */
		toggle = toggle_windows(toggle, command_wrk);

		/* Display the prompt */
		show_item_prompt(allow_inven, allow_equip, allow_floor, pmt,
						 command_wrk);

		/* Get a key */
		which = inkey();

		/* Parse it */
		switch (which)
		{
			case ESCAPE:
			{
				done = TRUE;
				break;
			}

			case '/':
			{
				if (command_wrk == (USE_INVEN))
				{
					if (!allow_equip)
					{
						bell("Cannot switch item selector!");
						break;
					}
					command_wrk = (USE_EQUIP);
				}
				else if (command_wrk == (USE_EQUIP))
				{
					if (!allow_inven)
					{
						bell("Cannot switch item selector!");
						break;
					}
					command_wrk = (USE_INVEN);
				}
				else if (command_wrk == (USE_FLOOR))
				{
					if (allow_inven)
					{
						command_wrk = (USE_INVEN);
					}
					else if (allow_equip)
					{
						command_wrk = (USE_EQUIP);
					}
					else
					{
						bell("Cannot switch item selector!");
						break;
					}
				}

				/* Hack -- Fix screen */

				/* Load screen */
				screen_load();

				/* Save screen */
				screen_save();

				/* Need to redraw */
				break;
			}

			case '-':
			{
				if (!allow_floor)
				{
					bell("Cannot select floor!");
					break;
				}

				if (!easy_floor)
				{
					/* Scan all objects in the grid */
					OBJ_ITT_START (c_ptr->o_idx, o_ptr)
					{
						/* Valid items only */
						if (!item_tester_okay(o_ptr)) continue;
					
						/* Allow player to "refuse" certain actions */
						if (!get_item_allow(o_ptr)) continue;

						/* Accept that choice */
						done = TRUE;
						break;
					}
					OBJ_ITT_END;
				}

				/*
				 * If we are already examining the floor, and there
				 * is only one item, we will always select it.
				 * If we aren't examining the floor and there is only
				 * one item, we will select it if floor_query_flag
				 * is FALSE.
				 */
				else if (floor_num == 1)
				{
					if ((command_wrk == (USE_FLOOR)) || (!carry_query_flag))
					{
						/* Allow player to "refuse" certain actions */
						if (!get_item_allow(fo_ptr)) continue;

						/* We use the first floor item */
						o_ptr = fo_ptr;

						/* Accept that choice */
						done = TRUE;

						break;
					}
				}

				/* Hack -- Fix screen */

				/* Load screen */
				screen_load();

				/* Save screen */
				screen_save();

				command_wrk = (USE_FLOOR);

				break;
			}

			case '0':
			case '1':  case '2':  case '3':
			case '4':  case '5':  case '6':
			case '7':  case '8':  case '9':
			{
				bool was_inven;

				/* Look up the tag */
				o_ptr = get_tag(&was_inven, which);

				if (!o_ptr)
				{
					bell("Illegal object choice (tag)!");
					break;
				}

				/* Hack -- Validate the item */
				if (was_inven ? !inven : !equip)
				{
					bell("Illegal object choice (tag)!");

					/* Invalid item */
					o_ptr = NULL;
					break;
				}

				/* Validate the item */
				if (!item_tester_okay(o_ptr))
				{
					bell("Illegal object choice (tag)!");

					/* Invalid item */
					o_ptr = NULL;
					break;
				}

				/* Allow player to "refuse" certain actions */
				if (!get_item_allow(o_ptr)) continue;

				/* Accept that choice */
				done = TRUE;
				break;
			}

			case '\n':
			case '\r':
			{
				/* No object selected yet */
				o_ptr = NULL;

				/* Choose "default" floor item */
				if (command_wrk == (USE_FLOOR))
				{
					if (floor_num == 1)
					{
						o_ptr = fo_ptr;
					}
				}

				/* Validate the item */
				if (!o_ptr || !item_tester_okay(o_ptr))
				{
					bell("Illegal object choice (default)!");

					/* Invalid item */
					o_ptr = NULL;
					break;
				}

				/* Allow player to "refuse" certain actions */
				if (!get_item_allow(o_ptr)) continue;

				/* Accept that choice */
				done = TRUE;
				break;
			}

			default:
			{
				int ver;

				/* Extract "query" setting */
				ver = isupper(which);
				which = tolower(which);

				/* Convert letter to inventory index */
				if (command_wrk == (USE_INVEN))
				{
					o_ptr = label_to_list(which, p_ptr->inventory);
				}

				/* Convert letter to equipment index */
				else if (command_wrk == (USE_EQUIP))
				{
					o_ptr = label_to_equip(which);
				}

				/* Convert letter to floor index */
				else if (command_wrk == USE_FLOOR)
				{
					o_ptr = label_to_list(which, c_ptr->o_idx);
				}

				/* Make sure selection is in bounds */
				if (!o_ptr)
				{
					bell("Illegal object choice (bounds)!");
					break;
				}

				/* Validate the item */
				if (!item_tester_okay(o_ptr))
				{
					bell("Illegal object choice (normal)!");

					/* Invalid item */
					o_ptr = NULL;
					break;
				}

				/* Verify the item */
				if (ver && !verify("Try", o_ptr))
				{
					done = TRUE;
					o_ptr = NULL;
					break;
				}

				/* Allow player to "refuse" certain actions */
				if (!get_item_allow(o_ptr)) continue;
					

				/* Accept that choice */
				done = TRUE;
				break;
			}
		}
	}

	/* Fix the screen */

	/* Load screen */
	screen_load();

	/* Forget the item_tester_tval restriction */
	item_tester_tval = 0;

	/* Forget the item_tester_hook restriction */
	item_tester_hook = NULL;


	/* Clean up */

	/* Toggle again if needed */
	if (toggle) toggle_inven_equip();

	/* Update */
	p_ptr->window |= (PW_INVEN | PW_EQUIP);

	/* Window stuff */
	window_stuff();

	/* Clear the prompt line */
	clear_msg();

	/* Save this object */
	save_object_choice(o_ptr, command_wrk);

	/* Done */
	return (o_ptr);
}
