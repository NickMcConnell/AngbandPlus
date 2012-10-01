/* File: cmd-book.c */

/*
 * Commands and routines that have to do with spellbooks. Combined from old cmd5.c and book.c
 *
 * Some of the code originally based on Oangband's info.c
 *
 * Copyright (c) 1999 Leon Marrick, Ben Harrison, James E. Wilson, 
 * Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"


typedef struct spell_tip
{
	int		index;
	cptr	desc;
} spell_tip;

/*
 * Prints "User's tips" for various spells.  User's tips only appear after 
 * a learnt spell is browsed. -LM-
 */
static spell_tip spell_tips[SPELLS_TOTAL] = 
{
	{0, NULL},
	{SP_HEAL_1,				"Reduces cuts and heals you a little."},
    {SP_HEAL_2A,			"Reduces cuts and heals you a moderate amount."},
    {SP_HEAL_2B,			"Eliminates cuts and heals you a moderate amount."},
	{SP_HEAL_3,				"Reduces cuts and heals you a large amount."},
	{SP_HEAL_4,				"Eliminates cuts and heals you a very large amount."},
	{SP_HEAL_5,				"A large amount of healing, eliminates cuts and stunning."},	
	{SP_HEAL_6,				"An extremely strong healing spell.  Removes cuts and stuns."},
	{SP_RESTORE_STATS,		"Restores all stats."},
	{SP_RESTORE_LEVEL,		"Restores experience level."},
	{SP_CURE_FEAR,			"Removes any fear you currently feel."},
	{SP_CURE_DISEASE,		"Rids your body of all disease."},
	{SP_CURE_POISON_1,		"Reduces the amount of poison in your system."},
	{SP_CURE_POISON_2,		"Removes all poison from your body."},
	{SP_CURE_POIS_DISE,		"Removes all poison and disease from your body."},
	{SP_CURE_BODY,			"Restores all stats, cures poison, and fully feeds you."},
	{SP_CLEAR_MIND,			"Rids your mind of confusion and fear, cures blindness."},
	{SP_TELE_10,			"Random minor displacement."}, 
	{SP_TELE_MINOR,			"Random medium-range displacement."},
	{SP_TELE_MAJOR,			"Random major displacement."},
	{SP_TELE_OTHER,			"Teleports a line of opponents away."},
	{SP_TELE_LEVEL,			"Immediately takes you to the next level up or down."},
	{SP_TELE_CONTROL,		"Controlled displacement. Press 'p' when you enter targeting mode."},
	{SP_WORD_RECALL,		"Recalls you to the town, or back into the dungeon."},
	{SP_ALTER_REALITY,		"Regenerates the dungeon level."},
	{SP_BOLT_MISSILE,		"Fires a bolt of mana."},
	{SP_BOLT_ELEC,			"Fires a bolt or beam of lightning."},	
	{SP_BOLT_FROST,			"Fires a bolt or beam of cold."},
	{SP_BOLT_ACID,			"Fires a bolt or beam of acid."},
	{SP_BOLT_FIRE,			"Fires a bolt or beam of fire."},
	{SP_BOLT_SOUND,			"Fires a bolt or beam of sound."},
	{SP_BOLT_FORCE,			"Fires a bolt or beam of force."},
	{SP_BOLT_MANA,			"Fires a bolt or beam of pure mana."},
	{SP_BEAM_WEAK_LITE,		"Fires a line of light.  Effects light-hating creatures."},
	{SP_BEAM_NETHER,		"Fires an ultra-powerful beam of nether. Does not affect undead."},
	{SP_BALL_POISON_1,		"Fires a ball of poison."},
	{SP_BALL_POISON_2,		"Fires a large poison ball."},
	{SP_BALL_ACID,			"Fires a large acid ball."},
	{SP_BALL_FIRE,			"Fires a ball of fire."},
	{SP_BALL_FROST_1,		"Fires a ball of frost."},
	{SP_BALL_FROST_2,		"Fires a large frost ball."},
	{SP_BALL_SOUND,			"Fires a ball of sound."},
	{SP_BALL_METEOR,		"Fires a large, unresistable ball of magic."},
	{SP_BALL_MANA,			"Fires a large, very powerful mana ball."},
	{SP_BALL_HOLY,			"Fires a ball of holy force."},
	{SP_BANISH,				"Teleports away all evil monsters in line of sight."},
	{SP_ANNIHILATION,		"A powerful drain life attack."},
	{SP_DISPEL_UNDEAD_1,	"Dispels all undead in line of sight."},
	{SP_DISPEL_UNDEAD_2,	"Dispels all undead in line of sight."},
	{SP_DISPEL_NON_EVIL,	"Dispels all non-evil monsters in line of sight."},
	{SP_DISPEL_EVIL_1,		"Dispels all evil monsters in line of sight."},
	{SP_DISPEL_EVIL_2,		"Dispels all evil monsters in line of sight."},
	{SP_WORD_HOLY,			"Strong dispel evil and healing."},
	{SP_GENOCIDE,			"Removes all monsters of the symbol you choose from the level."},
	{SP_MASS_GENOCIDE,		"Removes nearby monsters except uniques."},
	{SP_EARTHQUAKE,			"Shakes the nearby dungeon, randomly swapping walls and floors."},
	{SP_WORD_DESTRUCTION,	"Destroys objects and monsters, and banishes uniques."},
	{SP_LIGHT_AREA,			"Permanently light up the nearby area."},
	{SP_DARK_AREA,			"Darken the area."},
	{SP_DETECT_MONSTERS,	"Detects monsters on the current screen that are not invisible."},
	{SP_DETECT_EVIL,		"Detects all evil monsters, even invisible ones."},
	{SP_DETECT_TRAP,		"Detects all traps on the current panel."},
	{SP_DETECT_DOOR_STAIR,	"Detects all doors and stairs on the current panel."},
	{SP_DETECT_TRAP_DOOR,	"Detects hidden traps and doors on the current screen."},
	{SP_DETECT_ENCHANT,		"Detects magical objects on the current panel."},
	{SP_DETECT_ALL,			"Detects everything of interest on the panel."},
	{SP_ABSORB_HIT,			"Reverses the affect of damage (i.e. taking hits heal you)."},
	{SP_BLESS,				"Short-duration bonus to fighting ability and armour class."},
	{SP_CHANT,				"Medium-duration bonus to fighting ability and armour class."},
	{SP_PRAYER,				"Long-duration bonus to fighting ability and armour class."},
	{SP_HEROISM,			"Temporary heroism."},
	{SP_RAGE,				"Temporary berserk rage."},
	{SP_SHIELD,				"Temporarily increases armour class by 50."},
	{SP_INVIS,				"Temporarily turns you invisible."},
	{SP_RESILIENCE,			"Temporarily raises your AC by 100 and reduces all damage by 66%."},
	{SP_SEE_INVIS,			"Temporary see invisible."},
	{SP_PROT_EVIL,			"Temporary protection from lesser evil creatures."},
	{SP_HASTE_SELF_1,		"Temporarily hasten yourself."},
	{SP_HASTE_SELF_2,		"Long-duration haste spell."},
	{SP_DESTROY_TRAP_DOOR,	"Destroys all doors and traps next to you."},
	{SP_STONE_TO_MUD,		"Melts a wall square to floor."},
	{SP_CREATE_DOOR,		"Creates a barrier of doors around you."},
	{SP_CREATE_STAIR,		"Creates a randomly oriented staircase nearby."},
	{SP_AGGRAVATE,			"Aggravates nearby monsters."},
	{SP_CONFUSE_MONSTER,	"Attempts to confuse one monster."},
	{SP_CONFUSE_ALL,		"Attempts to confuse all monsters in line of sight."},
	{SP_SLEEP_MONSTER,		"Attempts to put a monster to sleep."},
	{SP_SLEEP_ADJACENT,		"Attempts to put all adjacent monsters to sleep."},
	{SP_SLEEP_ALL,			"Attempts to put all monsters in line of sight to sleep."},
	{SP_SLOW_MONSTER,		"Attempts to slow a monster down."},
	{SP_SLOW_ALL,			"Attempts to slow all monsters in line of sight."},
	{SP_CALM_MONSTER,		"Attempts to calm a monster."},
	{SP_CALM_ANIMALS,		"Attempts to calm all natural creatures in line of sight."},
	{SP_CALM_NON_EVIL,		"Attempts to calm all non-evil creatures in line of sight."},
	{SP_CALM_ALL,			"Attempts to calm all creatures in line of sight."},
	{SP_BLIND_MONSTER,		"Attempts to blind a monster."},
	{SP_SCARE_MONSTER,		"Attempts to frighten one monster."},
	{SP_SCARE_UNDEAD,		"Attempts to make all undead monsters in line of sight flee."},
	{SP_SCARE_ALL,			"Attempts to make all monsters in line of sight flee."},
	{SP_POLY_MONSTER,		"Attempts to change a monster."},
	{SP_SATISFY_HUNGER,		"Fully feeds you."},
	{SP_RECHARGE_1,			"Minor recharging."},
	{SP_RECHARGE_2,			"Medium strength recharging spell."},
	{SP_RECHARGE_3,			"Medium-strength recharging spell."},
	{SP_RECHARGE_4,			"Powerful recharging spell."},
	{SP_IDENTIFY,			"Standard identification of an object."},
	{SP_IDENTIFY_FULL,		"Reveals all information about a specific object."},
	{SP_RES_FIRE,			"Opposition to fire.  Cumulative with equipment."},
	{SP_RES_COLD,			"Opposition to cold.  Cumulative with equipment."},
	{SP_RES_FIRE_COLD,		"Opposition to fire and frost.  Cumulative with equipment."},
	{SP_RES_ACID_ELEC,		"Opposition to acid & electricity.  Cumulative with equipment."},
	{SP_RES_POISON,			"Opposition to poison. Cumulative with equipment."},
	{SP_RES_SOUND,			"Opposition to sound."},
	{SP_RES_ELEMENTS,		"Opposition to all elements. Cumulative with equipment."},
	{SP_RES_GREATER,		"Resist many things. Poison resistance cumulative with equipment."},
	{SP_RESISTANCE,			"Opposition to all elements + poison.  Cumulative with equipment."},
	{SP_GLYPH_WARDING,		"Places a glyph on the floor that monsters cannot pass over."},
	{SP_REMOVE_CURSE_1,		"Removes standard curses."},
	{SP_REMOVE_CURSE_2,		"Removes both normal and heavy curses."},
	{SP_MAP_1,				"Maps the local area."},
	{SP_MAP_2,				"Permanently light and detect objects on the entire level."},
	{SP_PROBE,				"Learns about a monster's attributes and resistances."},
	{SP_SELF_KNOW,			"Reveals all the magics that affect you."},
	{SP_ENCHANT_WEAP,		"Adds plusses to Hit and Damage to weapons."},
	{SP_ENCHANT_ARMR,		"Adds plusses to armour class to armour."},
	{SP_BRAND_AMMO_ANIMAL,	"Makes arrows and bolts extra powerful against animals."},
	{SP_BRAND_AMMO_WOUND,	"Strengthens arrows and bolts."},
	{SP_BRAND_AMMO_ELEMNT,	"Imbues arrows and bolts with elemental power."},
	{SP_BRAND_SHOT_HOLY,	"Makes your shots powerful against evil creatures."}
};

/* 
 * Checks to see if the character has access to any spell books 
 */
bool literate(void)
{
	int j;

	for (j = 0; j < SV_MAX_BOOKS; j++)
	{
		if (cp_ptr->spell_book[j]) return TRUE;  
	}
	return FALSE;
}

/* 
 * Checks to see if the character can cast spells
 */
bool spellcaster(void)
{
	if ((cp_ptr->flags & CF_MUSIC) || literate()) return TRUE;

	return FALSE;
}

/*
 * Returns mana cost for a spell
 */
static s16b spell_mana(int book, int spell, bool music)
{
	int mana, handicap;

	magic_type *s_ptr;

	/* Get the spell */
	if (!music) s_ptr = &books[book].contents[spell];
	else s_ptr = &instruments[book].contents[spell];

	/* Extract the base spell mana */
	mana = s_ptr->smana;

	/* Extract the base handicap */
	if (music) handicap = 0;
	else handicap = (cp_ptr->spell_handicap[book] - 1);

	/* Modify for handicap */
	mana += ((s_ptr->slevel/6) * handicap);

	return mana;
}

/*
 * Returns chance of failure for a spell
 */
static s16b spell_chance(int book, int spell, bool music)
{
	int chance, minfail, stat_factor;
	int handicap;

	magic_type *s_ptr;

	/* Paranoia -- must be able to cast spells */
	if (!spellcaster()) return (100);

	/* Get the spell */
	if (!music) s_ptr = &books[book].contents[spell];
	else s_ptr = &instruments[book].contents[spell];

	/* Extract the base spell failure rate */
	chance = s_ptr->sfail;

	/* Extract handicap */
	if (!music) handicap = cp_ptr->spell_handicap[book]-1;
	else handicap = 0;
	
	/* Reduce failure rate by "effective" level adjustment */
	chance -= 3 * (p_ptr->lev - (s_ptr->slevel+handicap));
	
	/* Reduce failure rate by INT/WIS adjustment */
	stat_factor = (p_ptr->stat_ind[cp_ptr->spell_stat1] + p_ptr->stat_ind[cp_ptr->spell_stat2])/2;
	chance -= 3 * (adj_mag_stat[stat_factor] - 1);

	/* Not enough mana to cast */
	if (spell_mana(book, spell, music) > p_ptr->csp)
	{
		chance += 5 * (spell_mana(book, spell, music) - p_ptr->csp);
	}

	/* Extract the minimum failure rate */
	minfail = adj_mag_fail[stat_factor];

	/* Non mage/priest/mystic characters never get better than 5 percent */
	if (!cp_ptr->flags & CF_ZERO_FAIL)
	{
		int threshold;

		if (!music) threshold = (cp_ptr->spell_handicap[book]+2);
		else threshold = 5;

		if (minfail < threshold) minfail = threshold;
	}

	/* Priest prayer penalty for "edged" weapons (before minfail) */
	if ((cp_ptr->flags & CF_BLESS_WEAPON) && (p_ptr->icky_wield))
	{
		chance += 25;
	}

	/* Minimum failure rate */
	if (chance < minfail) chance = minfail;

	/* Stunning makes spells harder (after minfail) */
	if (p_ptr->stun > 50) chance += 25;
	else if (p_ptr->stun) chance += 15;

	/* Always a 5 percent chance of working */
	if (chance > 95) chance = 95;

	/* Return the chance */
	return (chance);
}

/*
 * Determine if a spell is "okay" for the player to cast or study
 * The spell must be legible, not forgotten, and also, to cast,
 * it must be known, and to study, it must not be known.
 */
static bool spell_okay(int book, int spell, bool known)
{
	magic_type *s_ptr;

	/* Get the spell */
	s_ptr = &books[book].contents[spell];

	/* Spell is illegal */
	if ((s_ptr->slevel+(cp_ptr->spell_handicap[book]-1)) > p_ptr->lev) return (FALSE);

	/* Spell is forgotten */
	if (p_ptr->spell_forgotten[book] & (1L << spell))
	{
		/* Never okay */
		return (FALSE);
	}

	/* Spell is learned */
	if (p_ptr->spell_learned[book] & (1L << spell))
		/* Okay to cast, not to study */
	{
		return (known);
	}

	/* Okay to study, not to cast */
	return (!known);
}

/*
 * Determine if a tune is "okay" for the player to cast.
 */
static bool tune_okay(int instrument, int lev, int tune)
{
	magic_type *s_ptr;

	/* Get the spell */
	s_ptr = &instruments[instrument].contents[tune];

	/* Spell is illegal */
	if (s_ptr->slevel > p_ptr->lev) return (FALSE);

	if (s_ptr->slevel > (lev * 10)) return (FALSE);

	return (TRUE);
}

/*
 * Extra information on a spell		-DRS-
 *
 * We can use up to 20 characters of the buffer 'p'
 *
 * The strings in this function were extracted from the code in the
 * functions "do_cmd_cast()" and "do_cmd_pray()" and are up to date 
 * (as of 0.4.0). -LM-
 */
static void spell_info(char *p, int spell_index)
{
	int plev = ((cp_ptr->flags & CF_POWER) 
		? p_ptr->lev + (p_ptr->lev / 2) : p_ptr->lev);

	int beam = ((cp_ptr->flags & CF_BEAM) 
		? plev : (plev / 2));
	int beam_low = (beam - 10 > 0 ? beam - 10 : 0);

	/* Default */
	strcpy(p, "");

	/* Analyze the spell */
	switch (spell_index)
	{
		case SP_HEAL_1:
			strcpy(p, " heal 2d10"); break;
		case SP_HEAL_2A	:
			strcpy(p, " heal 4d10"); break;
		case SP_HEAL_2B:
			strcpy(p, " heal 4d10, any cut"); break;
		case SP_HEAL_3:
			strcpy(p, " heal 6d10"); break;
		case SP_HEAL_4:
			strcpy(p, " heal 8d10, any cut"); break;
		case SP_HEAL_5:
			strcpy(p, " heal 300, any cut"); break;
		case SP_HEAL_6:
			strcpy(p, " heal 2000, any cut"); break;
		case SP_CURE_POISON_1:
			strcpy(p, " halve poison"); break;
		case SP_TELE_10: 
			strcpy(p, " range 10"); break;
		case SP_TELE_MINOR:
			sprintf(p, " range %d", 3 * plev); break;
		case SP_TELE_CONTROL: 
			strcpy(p, " range 20"); break;
		case SP_TELE_MAJOR: 
			sprintf(p, " range %d", plev * 5); break;
		case SP_BOLT_MISSILE: 
			sprintf(p, " dam %dd4, beam %d%%", 3+((plev-1)/5), beam_low); break;
		case SP_BOLT_ELEC: 
			sprintf(p, " dam %dd8, beam %d%%", (3+((plev-5)/4)), beam_low); break;
		case SP_BOLT_FROST: 
			sprintf(p, " dam %dd8, beam %d%%", (5+((plev-5)/4)), beam_low); break;
		case SP_BOLT_ACID: 
			sprintf(p, " dam %dd8, beam %d%%", (6+((plev-5)/4)), beam); break;
		case SP_BOLT_FIRE: 
			sprintf(p, " dam %dd8, beam %d%%", (8+((plev-5)/4)), beam); break;
		case SP_BOLT_SOUND: 
			sprintf(p, " dam %dd4, beam %d%%", 3+((plev-1)/5), beam_low); break;
		case SP_BOLT_FORCE: 
			sprintf(p, " dam %dd8, beam %d%%", (2+((plev-5)/4)), beam_low); break;
		case SP_BOLT_MANA: 
			sprintf(p, " dam %dd8, beam %d%%", (6+((plev-5)/4)), beam); break;
		case SP_BEAM_WEAK_LITE: 
			strcpy(p, " dam 9d8"); break;
		case SP_BEAM_NETHER:
			sprintf(p, " dam %dd4", 8 * plev); break;
		case SP_BALL_POISON_1:
			sprintf(p, " dam %d, rad 2", 10 + (plev / 2)); break;
		case SP_BALL_POISON_2: 
			sprintf(p, " dam %d, rad 3", 20 + (plev/2)); break;
		case SP_BALL_ACID: 
			sprintf(p, " dam %d, rad 3", 40 + (plev)); break;
		case SP_BALL_FIRE: 
			sprintf(p, " dam %d, rad 2", 55 + plev); break;
		case SP_BALL_FROST_1: 
			sprintf(p, " dam %d, rad 2", 30 + plev); break;
		case SP_BALL_FROST_2: 
			sprintf(p, " dam %d, rad 3", 70 + (plev)); break;
		case SP_BALL_SOUND: 
			sprintf(p, " dam %d, rad 2", 30 + plev); break;
		case SP_BALL_METEOR: 
			sprintf(p, " dam %d, rad 3", 65 + (plev)); break;
		case SP_BALL_MANA: 
			sprintf(p, " dam %d, rad 3", 300 + (plev * 2)); break;
		case SP_BALL_HOLY:
			sprintf(p, " dam %d+3d6, rad %d", 
				plev + (plev / ((cp_ptr->flags & CF_BLESS_WEAPON) ? 2 : 4)), 
				((plev >= 30) && (cp_ptr->flags & CF_BLESS_WEAPON)) ? 3 : 2); break;
		case SP_ANNIHILATION	:
			strcpy(p, " dam 200"); break;
		case SP_DISPEL_UNDEAD_1:
			sprintf(p, " dam d%d", 3 * plev); break;
		case SP_DISPEL_UNDEAD_2:
			sprintf(p, " dam d%d", 4 * plev); break;
		case SP_DISPEL_NON_EVIL:
			sprintf(p, " dam d%d", 5 * plev); break;
		case SP_DISPEL_EVIL_1:
			sprintf(p, " dam d%d", 3 * plev); break;
		case SP_DISPEL_EVIL_2:
			sprintf(p, " dam d%d", 4 * plev); break;
		case SP_WORD_HOLY:
			sprintf(p, " dam d%d, heal 1000", plev * 4); break;
		case SP_GENOCIDE: 
			strcpy(p, " hurt 1d4 per kill"); break;
		case SP_MASS_GENOCIDE: 
			strcpy(p, " hurt 1d3 per kill"); break;
		case SP_EARTHQUAKE: 
			strcpy(p, " rad 10"); break;
		case SP_WORD_DESTRUCTION: 
			strcpy(p, " rad 15"); break;
		case SP_LIGHT_AREA:
			sprintf(p, " dam 2d%d, rad %d", (plev/2), ( plev / 10) + 1); break;
		case SP_DARK_AREA:
			sprintf(p, " dam 2d%d, rad %d", (plev/2), ( plev / 10) + 1); break;
		case SP_ABSORB_HIT: 
			sprintf(p, " dur %d+d25", plev); break;
		case SP_BLESS:
			strcpy(p, " dur 12+d12"); break;
		case SP_CHANT:
			strcpy(p, " dur 24+d24"); break;
		case SP_PRAYER:
			strcpy(p, " dur 48+d48"); break;
		case SP_HEROISM: 
			strcpy(p, " dur 25+d25"); break;
		case SP_RAGE: 
			strcpy(p, " dur 25+d25"); break;
		case SP_SHIELD: 
			strcpy(p, " dur 30+d20"); break;
		case SP_INVIS: 
			strcpy(p, " dur 25+d25"); break;
		case SP_RESILIENCE: 
			strcpy(p, " dur 8+d8"); break;
		case SP_SEE_INVIS:
			strcpy(p, " dur 24+d24"); break;
		case SP_PROT_EVIL:
			sprintf(p, " dur %d+d25", 3*plev); break;
		case SP_HASTE_SELF_1: 
			sprintf(p, " dur %d+d20", plev); break;
		case SP_HASTE_SELF_2	: 
			sprintf(p, " dur %d+d30", 30+plev); break;
		case SP_RES_FIRE: 
			strcpy(p, " dur 20+d20"); break;
		case SP_RES_COLD: 
			strcpy(p, " dur 20+d20"); break;
		case SP_RES_FIRE_COLD:
			strcpy(p, " dur 10+d10"); break;
		case SP_RES_ACID_ELEC: 
			strcpy(p, " dur 20+d20"); break;
		case SP_RES_POISON: 
			strcpy(p, " dur 20+d20"); break;
		case SP_RES_SOUND: 
			strcpy(p, " dur 40+d40"); break;
		case SP_RES_ELEMENTS:
			sprintf(p, " dur %d+d%d", plev/2, plev/2); break;
		case SP_RES_GREATER:
			sprintf(p, " dur %d+d%d", plev/3, plev/3); break;
		case SP_RESISTANCE: 
			strcpy(p, " dur 20+d20"); break;
	}
}

/*
 * Print out a list of available spells for any spellbook given.
 * Revised by -LM-
 *
 * Input y controls lines from top for list, and input x controls columns 
 * from left. 
 *
 */
void print_spells(int book, bool music, int lev, int y, int x)
{
	int i, left_justi;
	int j = 0;
	int handicap;

	magic_type *s_ptr;

	byte attr_book, attr_name;

	cptr comment;
	char info[80];
	char out_val[160];

	object_kind *k_ptr;
	cptr basenm;

	if (!music)
	{
		k_ptr = &k_info[lookup_kind(TV_MAGIC_BOOK, book)];
		basenm = (k_name + k_ptr->name);
	}
	else
	{
		k_ptr = &k_info[lookup_kind(TV_MUSIC, book)];
	}

	/* Choose appropriate spellbook color. */
	attr_book = k_ptr->x_attr;

	if (!music)
	{
		/* Choose a left margin for the spellbook name. */
		left_justi = ((80 - x) - strlen(basenm)) / 2;

		/* Center the spellbook name */
		prt("", y, x);
		c_put_str(attr_book, format("%s", basenm), y, x + left_justi);
	}

	/* Title the list */
	prt("", y + 1, x);
	put_str("Name", y + 1, x + 5);
	put_str("Lv Mana Fail Info", y + 1, x + 35);

	/* Calculate handicap */
	if (!music) handicap = cp_ptr->spell_handicap[book]-1;
	else handicap = 0;

	/* Dump the spells in the book. */
	for (i = 0; i < MAX_BOOK_SPELLS; i++)
	{
		/* Get the spell */
		if (!music) s_ptr = &books[book].contents[i];
		else s_ptr = &instruments[book].contents[i];

		if (s_ptr->index == 0) continue;

		/* Skip tunes if higher than the instrument level */
		if ((music) && (s_ptr->slevel > (lev * 10)))	continue;

		/* Increment the current line */
		j++;

		/* Skip illegible spells. */
		if ((s_ptr->slevel+handicap) >= 51)
		{
			sprintf(out_val, "  %c) %-30s", I2A(i), "(illegible)");
			c_prt(TERM_L_DARK, out_val, y + j + 1, x);
			continue;
		}

		/* Get extra info */
		spell_info(info, s_ptr->index);

		/* Use that info */
		comment = info;

		/* Vivid color for known, cast spells */
		attr_name = attr_book;

		/* Analyze the spell */
		if (!music)
		{
			if (p_ptr->spell_forgotten[book] & (1L << i))
			{
				comment = " forgotten";
				attr_name = TERM_L_WHITE;
			}
			else if (!(p_ptr->spell_learned[book] & (1L << i)))
			{
				if ((s_ptr->slevel+(cp_ptr->spell_handicap[book]-1)) <= p_ptr->lev)
				{
					comment = " unknown";
					attr_name = TERM_SLATE;
				}
				else
				{
					comment = " too high";
					attr_name = TERM_L_DARK;
				}
			}
			else if (!(p_ptr->spell_worked[book] & (1L << i)))
			{
				comment = " untried";
				attr_name = TERM_WHITE;
			}
		}
		else
		{
			if (s_ptr->slevel > p_ptr->lev)
			{
				comment = " too high";
				attr_name = TERM_L_DARK;
			}
		}

		/* Clear line */
		prt("", y + j + 1, x);

		/* Print out (colored) information about a single spell. */
		put_str(format("  %c) ", I2A(i)), y + j + 1, x);
		c_put_str(attr_name, format("%-30s", s_ptr->sname), 
			y + j + 1, x + 5);
		c_put_str(attr_name, format("%2d %4d %3d%%", (s_ptr->slevel+handicap), 
			spell_mana(book, i, music), spell_chance(book, i, music)), y + j + 1, x + 35);
		c_put_str(attr_name, format("%s", comment), y + j + 1, x + 47);
	}

	/* Clear the bottom line */
	prt("", y + j + 2, x);
}

/* Count how many spells in spell book */
static byte count_tunes(int instrument, int lev)
{
	int j, count;
	magic_type *s_ptr;

	count = 0;

	for (j=0; j < MAX_BOOK_SPELLS; j++)
	{
		s_ptr = &instruments[instrument].contents[j];
		if ((s_ptr->index > 0) && (s_ptr->slevel <= lev * 10)) count++;
	}

	return count;
}

/* Count how many spells in spell book */
byte count_spells(int book)
{
	int j, count;
	magic_type *s_ptr;

	count = 0;

	for (j=0; j < MAX_BOOK_SPELLS; j++)
	{
		s_ptr = &books[book].contents[j];
		if (s_ptr->index > 0) count++;
	}

	return count;
}

/*
 * Get a spell out of a book/instrument
 */
static int get_spell(int *sn, cptr prompt, int book, bool known)
{
	int i;

	int spell = -1;

	int ver;

	bool flag, redraw, okay;
	char choice;

	magic_type *s_ptr;

	char out_val[160];

#ifdef ALLOW_REPEAT /* TNB */

	/* Get the spell, if available */
	if (repeat_pull(sn)) 
	{
		/* Verify the spell is okay */
		if (spell_okay(book, *sn, known)) 
		{
			/* Success */
			return (TRUE);
		}
	}

#endif /* ALLOW_REPEAT */

	okay = FALSE;

	/* Assume no spells available */
	(*sn) = -2;

	/* Check for "okay" spells */
	for (i = 0; i < MAX_BOOK_SPELLS; i++)
	{
		/* Look for "okay" spells */
		if (spell_okay(book, i, known)) okay = TRUE;
	}

	/* No "okay" spells */
	if (!okay) return (FALSE);

	/* Assume cancelled */
	*sn = (-1);

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	/* Build a prompt (accept all spells) */
	strnfmt(out_val, 78, "(spells %c-%c, *=List, ESC=exit) %^s which spell? ",
		I2A(0), I2A(count_spells(book)-1), prompt);

	/* Get a spell from the user */
	while (!flag && get_com(out_val, &choice))
	{
		/* Request redraw */
		if ((choice == ' ') || (choice == '*') || (choice == '?'))
		{
			/* Hide the list */
			if (redraw)
			{
				/* Load screen */
				screen_load();

				/* Hide list */
				redraw = FALSE;
			}

			/* Show the list */
			else
			{
				/* Show list */
				redraw = TRUE;

				/* Save screen */
				screen_save();

				/* Display a list of spells */
				print_spells(book, FALSE, 0, 1, 14);
			}

			/* Ask again */
			continue;
		}

		/* Note verify */
		ver = (isupper(choice));

		/* Lowercase */
		choice = tolower(choice);

		/* Extract request */
		i = (islower(choice) ? A2I(choice) : -1);

		/* Totally Illegal */
		if ((i < 0) || (i >= MAX_BOOK_SPELLS))
		{
			bell("Illegal spell choice!");
			continue;
		}

		/* Convert spellbook number to spell index. */
		spell = i;

		/* Require "okay" spells */
		if (!spell_okay(book, spell, known))
		{
			bell("Illegal spell choice!");
			msg_format("You may not %s that spell.", prompt);
			continue;
		}

		/* Verify it */
		if (ver)
		{
			char tmp_val[160];

			/* Get the spell */
			s_ptr = &books[book].contents[i];

			/* Prompt */
			strnfmt(tmp_val, 78, "%^s %s (%d mana, %d%% fail)? ",
			        prompt, s_ptr->sname,
			        spell_mana(book, spell, FALSE), spell_chance(book, spell, FALSE));

			/* Belay that order */
			if (!get_check(tmp_val)) continue;
		}

		/* Stop the loop */
		flag = TRUE;
	}

	/* Restore the screen */
	if (redraw)
	{
		/* Load screen */
		screen_load();
	}

	/* Abort if needed */
	if (!flag) return (FALSE);

	/* Save the choice */
	(*sn) = spell;

#ifdef ALLOW_REPEAT /* TNB */
	repeat_push(*sn);

#endif /* ALLOW_REPEAT */

	/* Success */
	return (TRUE);
}

/*
 * Get a spell out of a book/instrument
 */
static int get_tune(int *sn, int instrument, int lev)
{
	int i;

	int tune = -1;

	int ver;

	bool flag, redraw, okay;
	char choice;

	magic_type *s_ptr;

	char out_val[160];

#ifdef ALLOW_REPEAT /* TNB */

	/* Get the spell, if available */
	if (repeat_pull(sn)) 
	{
		/* Verify the spell is okay */
		if (tune_okay(instrument, lev, *sn)) 
		{
			/* Success */
			return (TRUE);
		}
	}

#endif /* ALLOW_REPEAT */

	okay = FALSE;

	/* Assume no spells available */
	(*sn) = -2;

	/* Check for "okay" spells */
	for (i = 0; i < MAX_BOOK_SPELLS; i++)
	{
		/* Look for "okay" spells */
		if (tune_okay(instrument, lev, i)) okay = TRUE;
	}

	/* No "okay" spells */
	if (!okay) return (FALSE);

	/* Assume cancelled */
	*sn = (-1);

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	/* Build a prompt (accept all spells) */
	strnfmt(out_val, 78, "(spells %c-%c, *=List, ESC=exit) play which tune? ",
		I2A(0), I2A(count_tunes(instrument, lev)-1));

	/* Get a spell from the user */
	while (!flag && get_com(out_val, &choice))
	{
		/* Request redraw */
		if ((choice == ' ') || (choice == '*') || (choice == '?'))
		{
			/* Hide the list */
			if (redraw)
			{
				/* Load screen */
				screen_load();

				/* Hide list */
				redraw = FALSE;
			}

			/* Show the list */
			else
			{
				/* Show list */
				redraw = TRUE;

				/* Save screen */
				screen_save();

				/* Display a list of spells */
				print_spells(instrument, TRUE, lev, 1, 14);
			}

			/* Ask again */
			continue;
		}

		/* Note verify */
		ver = (isupper(choice));

		/* Lowercase */
		choice = tolower(choice);

		/* Extract request */
		i = (islower(choice) ? A2I(choice) : -1);

		/* Totally Illegal */
		if ((i < 0) || (i >= MAX_BOOK_SPELLS))
		{
			bell("Illegal tune choice!");
			continue;
		}

		/* Convert spellbook number to spell index. */
		tune = i;

		if (!tune_okay(instrument, lev, tune))
		{
			bell("Illegal tune choice!");
			msg_print("You may not play that tune.");
			continue;
		}

		/* Verify it */
		if (ver)
		{
			char tmp_val[160];

			/* Get the spell */
			s_ptr = &instruments[instrument].contents[i];

			/* Prompt */
			strnfmt(tmp_val, 78, "Play %s (%d mana, %d%% fail)? ", s_ptr->sname,
			        spell_mana(instrument, tune, TRUE), spell_chance(instrument, tune, TRUE));

			/* Belay that order */
			if (!get_check(tmp_val)) continue;
		}

		/* Stop the loop */
		flag = TRUE;
	}

	/* Restore the screen */
	if (redraw)
	{
		/* Load screen */
		screen_load();
	}

	/* Abort if needed */
	if (!flag) return (FALSE);

	/* Save the choice */
	(*sn) = tune;

#ifdef ALLOW_REPEAT /* TNB */
	repeat_push(*sn);
#endif /* ALLOW_REPEAT */

	/* Success */
	return (TRUE);
}

/*
 *  Actually browse a spellbook
 */
static void do_browse_instrument(int instrument, int lev)
{
	int tune, lines;

	magic_type *s_ptr;

	int j;

	/* Display the spells */
	print_spells(instrument, TRUE, lev, 1, 14);

	/* Prompt for a command */
	put_str("(Browsing) Choose a tune, or ESC:", 0, 0);

	/* Hack - Determine how far from the top of the screen the spell list 
	 * extends by counting spells, and adding space for name, etc.
	 */
	lines = count_tunes(instrument, lev);

	/* Keep browsing spells.  Exit browsing on cancel. */
	while(TRUE)
	{
		/* Ask for a spell, allow cancel */
		if (!get_tune(&tune, instrument, lev))
		{
			/* If cancelled, leave immediately. */
			if (tune == -1) break;

			/* Notify that there's nothing to see, and wait. */
			c_put_str(TERM_SLATE, "No tunes to browse     ", 0, 11);

			/* Any key cancels if no spells are available. */
			if (inkey()) break;
		}				  

		/* Clear lines, position cursor  (really should use strlen here) */
		Term_erase(14, lines + 3, 255);

		/* Access the spell */
		s_ptr = &instruments[instrument].contents[tune];

		for (j = (SPELLS_TOTAL-1);j > 0; j--) 
		{
			if (spell_tips[j].index == s_ptr->index) break;
		}

		/* Display that spell's information. */
		if (spell_tips[j].desc != NULL) c_roff(TERM_L_BLUE, spell_tips[j].desc);
	}
}

/*
 *  Actually browse a spellbook
 */
static void do_browse_book(int book)
{
	int spell, lines;

	magic_type *s_ptr;

	int j;

	/* Display the spells */
	print_spells(book, FALSE, 0, 1, 14);

	/* Prompt for a command */
	put_str("(Browsing) Choose a spell, or ESC:", 0, 0);

	/* Hack - Determine how far from the top of the screen the spell list 
	 * extends by counting spells, and adding space for name, etc.
	 */
	lines = count_spells(book);

	/* Keep browsing spells.  Exit browsing on cancel. */
	while(TRUE)
	{
		/* Ask for a spell, allow cancel */
		if (!get_spell(&spell, "browse", book, TRUE))
		{
			/* If cancelled, leave immediately. */
			if (spell == -1) break;

			/* Notify that there's nothing to see, and wait. */
			c_put_str(TERM_SLATE, "No spells to browse     ", 0, 11);

			/* Any key cancels if no spells are available. */
			if (inkey()) break;
		}				  

		/* Clear lines, position cursor  (really should use strlen here) */
		Term_erase(14, lines + 3, 255);

		/* Access the spell */
		s_ptr = &books[book].contents[spell];

		for (j = (SPELLS_TOTAL-1);j > 0; j--) 
		{
			if (spell_tips[j].index == s_ptr->index) break;
		}

		/* Display that spell's information. */
		if (spell_tips[j].desc != NULL) c_roff(TERM_L_BLUE, spell_tips[j].desc);
	}
}

/*
 * Peruse the spells/prayers in a Book/Instrument
 *
 * Note that browsing is allowed while confused or blind,
 * and in the dark, primarily to allow browsing in stores.
 *
 * Some code taken from Oangband 0.4.1 
 *
 */
void do_cmd_browse(void)
{
	int item;

	object_type *o_ptr;

	if (!spellcaster())
	{
		msg_print("You know no magic!");
		return;
	}

	item_tester_hook = item_tester_hook_bookmusic;

	/* Get an item */

	/* Can read books, can't use instruments */
	if (literate() && (!(cp_ptr->flags & CF_MUSIC))) 
		if (!get_item(&item, "Browse which book? ", "You have no books that you can browse.", 
		(USE_INVEN | USE_FLOOR))) return;

	/* Can use both instruments and books */
	if (literate() && ((cp_ptr->flags & CF_MUSIC))) 
		if (!get_item(&item, "Browse which book or musical instrument? ", 
			"You have no books or musical instruments that you can browse.", 
			(USE_INVEN | USE_FLOOR | USE_EQUIP))) return;

	/* Can use instruments, can't use books */
	if (!literate() && ((cp_ptr->flags & CF_MUSIC))) 
		if (!get_item(&item, "Browse which musical instrument? ", 
			"You have no musical instruments that you can browse.", 
			(USE_INVEN | USE_FLOOR | USE_EQUIP))) return;

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}

	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item];
	}
	
	/* Track the object kind */
	object_kind_track(o_ptr->k_idx, o_ptr->pval);

	/* Hack -- Handle stuff */
	handle_stuff();

	/* Save screen */
	screen_save();
	
	if (o_ptr->tval == TV_MAGIC_BOOK) do_browse_book(o_ptr->sval);
	else do_browse_instrument(o_ptr->sval, o_ptr->pval);

	/* Load screen */
	screen_load();

	/* Hack -- Process "Escape" */
	if (p_ptr->command_new == ESCAPE)
	{
		/* Reset stuff */
		p_ptr->command_new = 0;
	}
}

/*
 * Study a book to gain a new spell/prayer
 */
void do_cmd_study(void)
{
	int i, item;

	magic_type *s_ptr;

	int spell = -1;

	object_type *o_ptr;

	/* Forbid illiterates to read spellbooks. */
	if (!literate())
	{
		msg_print("You cannot learn magic!");
		return;
	}

	if (p_ptr->blind || no_lite())
	{
		msg_print("You cannot see!");
		return;
	}

	if (p_ptr->confused)
	{
		msg_print("You are too confused!");
		return;
	}

	if (!(p_ptr->new_spells))
	{
		msg_print("You cannot learn any new spells.");
		return;
	}

	/* Restrict choices to "useful" books */
	item_tester_hook = item_tester_hook_spellbooks;

	/* Get an item */
	if (!get_item(&item, "Study which book? ", "You have no books that you can read."
		, (USE_INVEN | USE_FLOOR))) return;

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}

	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item];
	}

	/* Track the object kind */
	object_kind_track(o_ptr->k_idx, o_ptr->pval);

	/* Hack -- Handle stuff */
	handle_stuff();

	/* All but Priests -- Learn a selected spell */
	if (cp_ptr->flags & CF_CHOOSE_SPELLS)
	{
		/* Ask for a spell, allow cancel */
		if (!get_spell(&spell, "study", o_ptr->sval, FALSE) && (spell == -1)) return;
	}
	/* Priest -- Learn a random prayer */
	else
	{
		int k = 0;

		int gift = -1;

		/* Pick an legal, unknown prayer at random. */
		for (spell = 0; spell < MAX_BOOK_SPELLS; spell++)
		{
			/* Skip non "okay" prayers */
			if (!spell_okay(o_ptr->sval, spell, FALSE)) continue;

			/* Apply the randomizer */
			if ((++k > 1) && (rand_int(k) != 0)) continue;

			/* Track it */
			gift = spell;
		}

		/* Accept gift */
		spell = gift;
	}

	/* Nothing to study */
	if (spell < 0)
	{
		/* Message */
		msg_print("You cannot learn any spells in that book.");

		/* Abort */
		return;
	}

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Learn the spell */
	p_ptr->spell_learned[o_ptr->sval] |= (1L << spell);

	/* Find the next open entry in "spell_order[]" */
	for (i = 0; i < (SV_MAX_BOOKS * MAX_BOOK_SPELLS); i++)
	{
		/* Stop at the first empty space */
		if (p_ptr->spell_order[i][1] == 99) break;
	}

	/* Add the spell to the known list */
	p_ptr->spell_order[i][0] = o_ptr->sval;
	p_ptr->spell_order[i][1] = spell;

	/* Access the spell */
	s_ptr = &books[o_ptr->sval].contents[spell];

	/* Mention the result */
	msg_format("You have learned the spell of %s.",
	           s_ptr->sname);

	/* Sound */
	sound(SOUND_STUDY);

	/* One less spell available */
	p_ptr->new_spells--;

	/* Message if needed */
	if (p_ptr->new_spells)
	{
		/* Message */
		msg_format("You can learn %d more spell%s.", p_ptr->new_spells, 
			(p_ptr->new_spells != 1)  ? "s" : "");
	}

	/* Save the new_spells value */
	p_ptr->old_spells = p_ptr->new_spells;

	/* Redraw Study Status */
	p_ptr->redraw |= (PR_STUDY);

}

/*
 * Actual spell effect 
 */
static void aux_spell_cast(int index, int plev, int beam)
{
	int dir;
	int durat;

	switch (index)
	{
		case SP_HEAL_1:
		{
			(void)hp_player(damroll(2, 10));
			(void)set_cut(p_ptr->cut - 10);
			break;
		}
		case SP_HEAL_2A:
		{
			(void)hp_player(damroll(4, 10));
			(void)set_cut((p_ptr->cut / 2) - 20);
			break;
		}
		case SP_HEAL_2B:
		{
			(void)hp_player(damroll(4, 10));
			(void)set_cut(0);
			break;
		}
		case SP_HEAL_3:
		{
			(void)hp_player(damroll(6, 10));
			(void)set_cut(0);
			break;
		}
		case SP_HEAL_4:
		{
			(void)hp_player(damroll(8, 10));
			(void)set_stun(0);
			(void)set_cut(0);
			break;
		}
		case SP_HEAL_5:
		{
			(void)hp_player(300);
			(void)set_stun(0);
			(void)set_cut(0);
			break;
		}
		case SP_HEAL_6:
		{
			(void)hp_player(2000);
			(void)set_stun(0);
			(void)set_cut(0);
			break;
		}
		case SP_RESTORE_STATS:
		{
			(void)do_res_stat(A_STR);
			(void)do_res_stat(A_INT);
			(void)do_res_stat(A_WIS);
			(void)do_res_stat(A_DEX);
			(void)do_res_stat(A_CON);
			(void)do_res_stat(A_CHR);
			break;
		}
		case SP_RESTORE_LEVEL:
		{
			(void)restore_exp();
			break;
		}
		case SP_CURE_FEAR:
		{
			(void)set_afraid(0);
			break;
		}
		case SP_CURE_DISEASE:
		{
			(void)set_diseased(0);
			break;
		}
		case SP_CURE_POISON_1:
		{
			(void)set_poisoned(p_ptr->poisoned / 2);
			break;
		}
		case SP_CURE_POISON_2:
		{
			(void)set_poisoned(0);
			break;
		}
		case SP_CURE_POIS_DISE:
		{
			(void)set_poisoned(0);
			(void)set_diseased(0);
			break;
		}
		case SP_CURE_BODY:
		{
			(void)set_poisoned(0);
			(void)set_food(PY_FOOD_MAX - 1);
			(void)do_res_stat(A_STR);
			(void)do_res_stat(A_INT);
			(void)do_res_stat(A_WIS);
			(void)do_res_stat(A_DEX);
			(void)do_res_stat(A_CON);
			(void)do_res_stat(A_CHR);
			break;
		}
		case SP_CLEAR_MIND:
		{
			(void)set_stun(0);
			(void)set_blind(0);
			(void)set_afraid(0);
			(void)set_confused(0);
			break;
		}
		case SP_TELE_10: 
		{
			teleport_player(10);
			break;
		}
		case SP_TELE_MINOR:
		{
			teleport_player(plev * 3);
			break;
		}
		case SP_TELE_MAJOR:
		{
			teleport_player(plev * 5);
			break;
		}
		case SP_TELE_OTHER:
		{
			if (!get_aim_dir(&dir)) return;
			(void)teleport_monster(dir);
			break;
		}
		case SP_TELE_LEVEL:
		{
			(void)teleport_player_level();
			break;
		}
		case SP_TELE_CONTROL:
		{
			msg_print("Choose a location to teleport to.");
			msg_print(NULL);
			dimen_door();
			break;
		}
		case SP_WORD_RECALL:
		{
			set_recall();
			break;
		}
		case SP_ALTER_REALITY:
		{
			msg_print("The world changes!");

			/* Leaving */
			p_ptr->leaving = TRUE;

			break;
		}
		case SP_BOLT_MISSILE:
		{
			if (!get_aim_dir(&dir)) return;
			fire_bolt_or_beam(beam-10, GF_MISSILE, dir,
				              damroll(3 + ((plev - 1) / 5), 4));
			break;
		}
		case SP_BOLT_ELEC:
		{
			if (!get_aim_dir(&dir)) return;
			fire_bolt_or_beam(beam-10, GF_ELEC, dir,
				              damroll(3+((plev-5)/4), 8));
			break;
		}
		case SP_BOLT_FROST:
		{
			if (!get_aim_dir(&dir)) return;
			fire_bolt_or_beam(beam-10, GF_COLD, dir,
				              damroll(5+((plev-5)/4), 8));
			break;
		}
		case SP_BOLT_ACID:
		{
			if (!get_aim_dir(&dir)) return;
			fire_bolt_or_beam(beam, GF_ACID, dir,
				              damroll(6+((plev-5)/4), 8));
			break;
		}
		case SP_BOLT_FIRE:
		{
			if (!get_aim_dir(&dir)) return;
			fire_bolt_or_beam(beam, GF_FIRE, dir,
				              damroll(8+((plev-5)/4), 8));
			break;
		}
		case SP_BOLT_SOUND:
		{
			if (!get_aim_dir(&dir)) return;
			fire_bolt_or_beam(beam-10, GF_SOUND, dir,
				              damroll(3 + ((plev - 1) / 5), 4));
			break;
		}
		case SP_BOLT_FORCE:
		{
			if (!get_aim_dir(&dir)) return;
			fire_bolt_or_beam(beam-10, GF_FORCE, dir,
				              damroll(2+((plev-5)/4), 8));
			break;
		}
		case SP_BOLT_MANA:
		{
			if (!get_aim_dir(&dir)) return;
			fire_bolt_or_beam(beam, GF_MANA, dir,
				              damroll(6+((plev-5)/4), 8));
			break;
		}
		case SP_BEAM_WEAK_LITE:
		{ 
			if (!get_aim_dir(&dir)) return;
			msg_print("A line of blue shimmering light appears.");
			lite_line(dir,damroll(9,8));
			break;
		}
		case SP_BEAM_NETHER:
		{
			if (!get_aim_dir(&dir)) return;
			fire_bolt_or_beam(100, GF_NETHER, dir,
				              damroll((8 * plev), 4));				
			break;
		}
		case SP_BALL_POISON_1:
		{
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_POIS, dir, 10 + (plev / 2), 2);
			break;
		}
		case SP_BALL_POISON_2:
		{
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_POIS, dir, 20 + (plev / 2), 3);
			break;
		}
		case SP_BALL_ACID:
		{
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_ACID, dir, 40 + (plev), 2);
			break;
		}
		case SP_BALL_FIRE:
		{
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_FIRE, dir, 55 + (plev), 2);
			break;
		}
		case SP_BALL_FROST_1:
		{
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_COLD, dir, 30 + (plev), 2);
			break;
		}
		case SP_BALL_FROST_2:
		{
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_COLD, dir, 70 + (plev), 3);
			break;
		}
		case SP_BALL_SOUND:
		{
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_SOUND, dir, 30 + (plev), 2);
			break;
		}
		case SP_BALL_METEOR:
		{
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_METEOR, dir, 65 + (plev), 3);
			break;
		}
		case SP_BALL_MANA:
		{
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_MANA, dir, 300 + (plev * 2), 3);
			break;
		}
		case SP_BALL_HOLY:
		{
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_HOLY_ORB, dir, (damroll(3, 6) + plev +
				       (plev / ((cp_ptr->flags & CF_BLESS_WEAPON) ? 2 : 4))),
				      (((plev >= 30) && (cp_ptr->flags & CF_BLESS_WEAPON)) ? 3 : 2));
			break;
		}
		case SP_BANISH:
		{
			if (banish_evil(100))
			{
				msg_print("The power of your god banishes evil!");
			}
			break;
		}
		case SP_ANNIHILATION:
		{
			if (!get_aim_dir(&dir)) return;
			drain_life(dir, 200);
			break;
		}
		case SP_DISPEL_UNDEAD_1:
		{
			(void)dispel_undead(randint(plev * 3));
			break;
		}
		case SP_DISPEL_UNDEAD_2:
		{
			(void)dispel_undead(randint(plev * 4));
			break;
		}
		case SP_DISPEL_NON_EVIL:
		{
			dispel_non_evil(randint(plev * 5));
			break;
		}
		case SP_DISPEL_EVIL_1:
		{
			(void)dispel_evil(randint(plev * 3));
			break;
		}
		case SP_DISPEL_EVIL_2:
		{
			(void)dispel_evil(randint(plev * 4));
			break;
		}
		case SP_WORD_HOLY:
		{
			(void)dispel_evil(randint(plev * 4));
			(void)hp_player(1000);
			(void)set_afraid(0);
			(void)set_poisoned(0);
			(void)set_stun(0);
			(void)set_cut(0);
			break;
		}
		case SP_GENOCIDE:
		{
			(void)genocide();
			break;
		}
		case SP_MASS_GENOCIDE:
		{
			(void)mass_genocide();
			break;
		}
		case SP_EARTHQUAKE:
		{
			earthquake(p_ptr->py, p_ptr->px, 10);
			break;
		}
		case SP_WORD_DESTRUCTION:
		{
			destroy_area(p_ptr->py, p_ptr->px, 15, TRUE);
			break;
		}
		case SP_LIGHT_AREA: 
		{
			(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
			break;
		}
		case SP_DARK_AREA:
		{
			if (!p_ptr->no_blind)
			{
				(void)set_blind(p_ptr->blind + 3 + randint(5));
			}
			(void)unlite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
			break;
		}
		case SP_DETECT_MONSTERS:
		{
			(void)detect_monsters_normal();
			break;
		}
		case SP_DETECT_EVIL:
		{
			(void)detect_monsters_evil();
			break;
		}
		case SP_DETECT_TRAP:
		{
			(void)detect_traps();
			break;
		}
		case SP_DETECT_TRAP_DOOR:
		{
			(void)detect_traps();
			(void)detect_doors();
			(void)detect_stairs();
			break;
		}
		case SP_DETECT_DOOR_STAIR:
		{
			(void)detect_doors();
			(void)detect_stairs();
			break;
		}
		case SP_DETECT_ENCHANT:
		{
			(void)detect_objects_magic();
			break;
		}
		case SP_DETECT_ALL:
		{
			(void)detect_all();
			break;
		}
		case SP_ABSORB_HIT:
		{
			(void)set_absorb(p_ptr->absorb + randint(25) + plev);
			break;
		}
		case SP_BLESS:
		{
			(void)set_blessed(p_ptr->blessed + randint(12) + 12);
			break;
		}
		case SP_CHANT:
		{
			(void)set_blessed(p_ptr->blessed + randint(24) + 24);
			break;
		}
		case SP_PRAYER:
		{
			(void)set_blessed(p_ptr->blessed + randint(48) + 48);
			break;
		}
		case SP_HEROISM:
		{
			(void)hp_player(10);
			(void)set_hero(p_ptr->hero + randint(25) + 25);
			(void)set_afraid(0);
			break;
		}
		case SP_RAGE:
		{
			(void)(hp_player(10));
			(void)(set_afraid(0));
			(void)(set_rage(p_ptr->rage + randint(25) + 25));
			break;
		}
		case SP_SHIELD:
		{
			(void)set_shield(p_ptr->shield + randint(20) + 30);
			break;
		}
		case SP_INVIS:
		{
			if (!p_ptr->tim_invis)
			{
				(void)set_tim_invis(randint(25) + 25 + plev);
			}
			else
			{
				(void)set_tim_invis(p_ptr->tim_invis + randint(20));
			}
			break;
			break;
		}
		case SP_RESILIENCE:
		{
			(void)set_resilient(p_ptr->resilient + randint(8) + 8);
			break;
		}
		case SP_SEE_INVIS:
		{
			(void)set_tim_see_invis(p_ptr->tim_see_invis + randint(24) + 24);
			break;
		}
		case SP_PROT_EVIL:
		{
			(void)set_protevil(p_ptr->protevil + randint(25) + 3 * plev);
			break;
		}
		case SP_HASTE_SELF_1:
		{
			if (!p_ptr->fast)
			{
				(void)set_fast(randint(20) + plev);
			}
			else
			{
				(void)set_fast(p_ptr->fast + randint(5));
			}
			break;
		}
		case SP_HASTE_SELF_2:
		{
			if (!p_ptr->fast)
			{
				(void)set_fast(randint(30) + 30 + plev);
			}
			else
			{
				(void)set_fast(p_ptr->fast + randint(10));
			}
			break;
		}
		case SP_DESTROY_TRAP_DOOR:
		{
			(void)destroy_doors_touch();
			break;
		}
		case SP_STONE_TO_MUD:
		{
			if (!get_aim_dir(&dir)) return;
			(void)wall_to_mud(dir);
			break;
		}
		case SP_CREATE_DOOR:
		{
			(void)door_creation();
			break;
		}
		case SP_CREATE_STAIR:
		{
			(void)stair_creation();
			break;
		}
		case SP_AGGRAVATE:
		{
			aggravate_monsters(0);
		}
		case SP_CONFUSE_MONSTER:
		{
			if (!get_aim_dir(&dir)) return;
			(void)confuse_monster(dir, plev);
			break;
		}
		case SP_CONFUSE_ALL:
		{
			(void)confuse_monsters();
			break;
		}
		case SP_SLEEP_MONSTER:
		{
			if (!get_aim_dir(&dir)) return;
			(void)sleep_monster(dir, plev);
			break;
		}
		case SP_SLEEP_ADJACENT:
		{
			(void)sleep_monsters_touch();
			break;
		}
		case SP_SLEEP_ALL:
		{
			(void)sleep_monsters();
			break;
		}
		case SP_SLOW_MONSTER:
		{
			if (!get_aim_dir(&dir)) return;
			(void)slow_monster(dir,plev);
			break;
		}
		case SP_SLOW_ALL:
		{
			(void)slow_monsters();
			break;
		}
		case SP_CALM_MONSTER:
		{
			if (!get_aim_dir(&dir)) return;
			(void)calm_monster(dir,plev);
			break;
		}
		case SP_CALM_ANIMALS:
		{
			(void)calm_animals();
			break;
		}
		case SP_CALM_NON_EVIL:
		{
			(void)calm_non_evil();
			break;
		}
		case SP_CALM_ALL:
		{
			(void)calm_monsters();
			break;
		}
		case SP_BLIND_MONSTER:
		{
			if (!get_aim_dir(&dir)) return;
			blind_monster(dir,plev);
			break;
		}
		case SP_SCARE_MONSTER:
		{
			if (!get_aim_dir(&dir)) return;
			(void)fear_monster(dir, plev);
			break;
		}
		case SP_SCARE_UNDEAD:
		{
			(void)turn_undead();
			break;
		}
		case SP_SCARE_ALL:
		{
			(void)scare_monsters();
			break;
		}
		case SP_POLY_MONSTER:
		{
			if (!get_aim_dir(&dir)) return;
			(void)poly_monster(dir);
			break;
		}
		case SP_SATISFY_HUNGER:
		{
			(void)set_food(PY_FOOD_MAX - 1);
			break;
		}
		case SP_RECHARGE_1:
		{
			(void)recharge(5);
			break;
		}
		case SP_RECHARGE_2:
		{
			(void)recharge(15);
			break;
		}
		case SP_RECHARGE_3:
		{
			(void)recharge(40);
			break;
		}
		case SP_RECHARGE_4:
		{
			recharge(100);
			break;
		}
		case SP_IDENTIFY:
		{
			(void)ident_spell();
			break;
		}
		case SP_IDENTIFY_FULL:
		{
			identify_fully();
			break;
		}
		case SP_RES_FIRE:
		{
			(void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20);
			break;
		}
		case SP_RES_COLD:
		{
			(void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + 20);
			break;
		}
		case SP_RES_FIRE_COLD:
		{
			durat = randint(10) + 10;
			(void)set_oppose_fire(p_ptr->oppose_fire + durat);
			(void)set_oppose_cold(p_ptr->oppose_cold + durat);
			break;
		}
		case SP_RES_ACID_ELEC:
		{
			durat = randint(20) + 20;
			(void)set_oppose_acid(p_ptr->oppose_acid + durat);
			(void)set_oppose_elec(p_ptr->oppose_elec + durat);
			break;
		}
		case SP_RES_POISON:
		{
			(void)set_oppose_pois(p_ptr->oppose_pois + randint(20) + 20);
			break;
		}
		case SP_RES_SOUND:
		{
			(void)(set_tim_res_sound(p_ptr->tim_res_sound + randint(40) + 40));
			break;
		}
		case SP_RES_ELEMENTS:
		{
			durat = randint(plev/2) + plev/2;
			(void)set_oppose_acid(p_ptr->oppose_acid + durat);
			(void)set_oppose_elec(p_ptr->oppose_elec + durat);
			(void)set_oppose_fire(p_ptr->oppose_fire + durat);
			(void)set_oppose_cold(p_ptr->oppose_cold + durat);
			break;
		}
		case SP_RES_GREATER:
		{
			durat = randint(plev/3) + plev/3;
			(void)(set_oppose_pois(p_ptr->oppose_pois + durat));
			(void)(set_tim_res_lite(p_ptr->tim_res_lite + durat));
			(void)(set_tim_res_dark(p_ptr->tim_res_dark + durat));
			(void)(set_tim_res_confu(p_ptr->tim_res_confu + durat));
			(void)(set_tim_res_sound(p_ptr->tim_res_sound + durat));
			(void)(set_tim_res_shard(p_ptr->tim_res_shard + durat));
			(void)(set_tim_res_nexus(p_ptr->tim_res_nexus + durat));
			(void)(set_tim_res_nethr(p_ptr->tim_res_nethr + durat));
			(void)(set_tim_res_chaos(p_ptr->tim_res_chaos + durat)); 
			(void)(set_tim_res_disease(p_ptr->tim_res_disease + durat));
			(void)(set_tim_res_water(p_ptr->tim_res_water + durat));
			break;
		}
		case SP_RESISTANCE:
		{
			durat = randint(20) + 20;
			(void)set_oppose_acid(p_ptr->oppose_acid + durat);
			(void)set_oppose_elec(p_ptr->oppose_elec + durat);
			(void)set_oppose_fire(p_ptr->oppose_fire + durat);
			(void)set_oppose_cold(p_ptr->oppose_cold + durat);
			(void)set_oppose_pois(p_ptr->oppose_pois + durat);
			break;
		}
		case SP_GLYPH_WARDING:
		{
			warding_glyph();
			break;
		}
		case SP_REMOVE_CURSE_1:
		{
			remove_curse();
			break;
		}
		case SP_REMOVE_CURSE_2:
		{
			(void)remove_all_curse();
			break;
		}
		case SP_MAP_1:
		{
			map_area();
			break;
		}
		case SP_MAP_2:
		{
			wiz_lite();
			break;
		}
		case SP_PROBE:
		{
			(void)probing();
			break;
		}
		case SP_SELF_KNOW:
		{
			msg_print("You begin to know yourself a little better...");
			msg_print(NULL);
			self_knowledge();
			break;
		}
		case SP_ENCHANT_WEAP:
		{
			(void)enchant_spell(rand_int(4) + 1, rand_int(4) + 1, 0);
			break;
		}
		case SP_ENCHANT_ARMR:
		{
			(void)enchant_spell(0, 0, rand_int(3) + 2);
			break;
		}
		case SP_BRAND_AMMO_ANIMAL:
		{	
			(void)brand_weapon(TV_ARROW,EGO_HURT_ANIMAL,FALSE);
			break;
		}
		case SP_BRAND_AMMO_WOUND:
		{	
			(void)brand_weapon(TV_ARROW,EGO_WOUNDING,TRUE);
			break;
		}
		case SP_BRAND_AMMO_ELEMNT:
		{	
			/* 
			 * Hack - choose random brand 
			 * Uses the "durat" variable to avoid declaring a new one.
			 */

			durat = rand_int(4);
			(void)brand_weapon(TV_ARROW,EGO_AMMO_ACID+durat,TRUE);
			break;
		}
		case SP_BRAND_SHOT_HOLY:
		{
			(void)brand_weapon(TV_SHOT,EGO_HURT_EVIL,FALSE);
			break;
		}
	}
}

/*
 * Cast a spell or pray a prayer.
 */
static void do_cast_or_pray(int book)
{
	int spell;
	int chance, beam;
	int shape = 0;

	int plev = ((cp_ptr->flags & CF_POWER) 
		? p_ptr->lev + (p_ptr->lev / 2) : p_ptr->lev);

	magic_type *s_ptr;

	/* Ask for a spell */
	if (!get_spell(&spell, "cast", book, TRUE))
	{
		if (spell == -2) 
		{
			msg_print("You don't know any spells in that books.");
		}
		return;
	}

	/* Access the spell */
	s_ptr = &books[book].contents[spell];

	/* Verify "dangerous" spells */
	if (spell_mana(book, spell, FALSE) > p_ptr->csp)
	{
		/* Warning */
		msg_print("You do not have enough mana to cast this spell.");

		/* Verify */
		if (!get_check("Attempt it anyway? ")) return;
	}


	/* Spell failure chance */
	chance = spell_chance(book, spell, FALSE);

	/* Failed spell */
	if (rand_int(100) < chance)
	{
		if (flush_failure) flush();
		switch (books[book].flags & SBF_TYPE_MASK) 
		{
			case SBF_MAGIC:
			{
				msg_print("You fail to tap onto the necessary magical forces!");
				break;
			}
			case SBF_PRAYER:
			{
				msg_print("Your prayer was left unanswered!");
				break;
			}
			case SBF_MYSTIC:
			{
				msg_print("You lose your concentration!");
				break;
			}
			case SBF_CODEX:
			{
				msg_print("Your mind is overwhelmed by the magnitute of ancient mystery!");
				/* Lose your spell-casting stats */
				if (rand_int(100) < chance) 
				{ 
					if (rand_int(2) == 0) do_dec_stat(cp_ptr->spell_stat1, rand_int(10)+20, TRUE, FALSE);
					else do_dec_stat(cp_ptr->spell_stat2, rand_int(10)+20, TRUE, FALSE);
				}
				/* Lose your memories */
				lose_all_info();
				break;
			}
			case SBF_NECRONOM:
			{
				msg_print("You lost your grasp on the evil powers that you had sought to control!");
				/* Summon some horrors */
				summon_specific(p_ptr->py, p_ptr->px, p_ptr->depth+10, SUMMON_HORROR);
				/* Darkness */
				if (!p_ptr->no_blind)
				{
					set_blind(p_ptr->blind + 3 + randint(5));
				}
				unlite_area(10, 3);
				/* Lose EXP */
				if (p_ptr->hold_life && (rand_int(100) < (100-chance)))
				{
					msg_print("You keep hold of your life force!");
				}
				else if (p_ptr->hold_life)
				{
					msg_print("You feel your life slipping away!");
					lose_exp(200 + (p_ptr->exp/250));
				}
				else
				{
					msg_print("You feel your life draining away!");
					lose_exp(200 + (p_ptr->exp/25));
				}				
				break;
			}
		}

	}

	/* Process spell */
	else
	{
		/* Hack -- higher chance of "beam" instead of "bolt" for mages */
		beam = ((cp_ptr->flags & CF_BEAM) ? plev : (plev / 2));

		aux_spell_cast(s_ptr->index,plev,beam);

		/* A spell was cast or a prayer prayed */
		if (!(p_ptr->spell_worked[book] & (1L << spell)))
		{
			int e = s_ptr->sexp;

			/* The spell or prayer worked */
			p_ptr->spell_worked[book] |= (1L << spell);

			/* Gain experience */
			gain_exp(e * (s_ptr->slevel+(cp_ptr->spell_handicap[book]-1)));
		}
	}

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Sufficient mana */
	if (spell_mana(book, spell, FALSE) <= p_ptr->csp)
	{
		/* Use some mana */
		p_ptr->csp -= spell_mana(book, spell, FALSE);
	}

	/* Over-exert the player */
	else
	{
		int oops = spell_mana(book, spell, FALSE) - p_ptr->csp;

		/* No mana left */
		p_ptr->csp = 0;
		p_ptr->csp_frac = 0;

		/* Message */
		msg_print("You faint from the effort!");

		/* Hack -- Bypass free action */
		(void)set_paralyzed(p_ptr->paralyzed + randint(5 * oops + 1));

		/* Damage CON (possibly permanently) */
		if (rand_int(100) < 50)
		{
			bool perm = (rand_int(100) < 25);

			/* Message */
			msg_print("You have damaged your health!");

			/* Reduce constitution */
			(void)do_dec_stat(A_CON, 15 + randint(10), perm, FALSE);
		}
	}

	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
}

/*
 * Play a tune
 */
static void do_play(int instrument, int lev)
{
	int tune;
	int chance;
	int shape = 0;

	magic_type *s_ptr;

	/* Ask for a spell */
	if (!get_tune(&tune, instrument, lev))
	{
		if (tune == -2) 
		{
			msg_print("You can't play any tunes with this instrument.");
		}
		return;
	}

	/* Access the spell */
	s_ptr = &instruments[instrument].contents[tune];

	/* Verify "dangerous" spells */
	if (spell_mana(instrument, tune, TRUE) > p_ptr->csp)
	{
		/* Warning */
		msg_print("You do not have enough mana to play this tune.");

		/* Verify */
		if (!get_check("Attempt it anyway? ")) return;
	}

	/* Spell failure chance */
	chance = spell_chance(instrument, tune, TRUE);

	/* Failed spell */
	if (rand_int(100) < chance)
	{
		if (flush_failure) flush();
		msg_print("Your tune falls flat!");
	}

	/* Process spell */
	else aux_spell_cast(s_ptr->index,p_ptr->lev,(p_ptr->lev / 2));

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Sufficient mana */
	if (spell_mana(instrument, tune, TRUE) <= p_ptr->csp)
	{
		/* Use some mana */
		p_ptr->csp -= spell_mana(instrument, tune, TRUE);
	}

	/* Over-exert the player */
	else
	{
		int oops = spell_mana(instrument, tune, TRUE) - p_ptr->csp;

		/* No mana left */
		p_ptr->csp = 0;
		p_ptr->csp_frac = 0;

		/* Message */
		msg_print("You faint from the effort!");

		/* Hack -- Bypass free action */
		(void)set_paralyzed(p_ptr->paralyzed + randint(5 * oops + 1));

		/* Damage DEX (possibly permanently) */
		if (rand_int(100) < 50)
		{
			bool perm = (rand_int(100) < 25);

			/* Message */
			msg_print("You overtax your muscles!");

			/* Reduce constitution */
			(void)do_dec_stat(A_DEX, 15 + randint(10), perm, FALSE);
		}
	}

	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
}

/*
 * Cast a spell or pray a prayer.
 */
void do_cmd_magic(void)
{
	int item;

	object_type *o_ptr;

	if (!spellcaster())
	{
		msg_print("You know no magic!");
		return;
	}

	item_tester_hook = item_tester_hook_bookmusic;

	if (p_ptr->confused)
	{
		if (!(cp_ptr->flags & CF_MYSTIC_CAST))
		{
			msg_print("You are too confused!");
			return;
		}
		else item_tester_hook = item_tester_hook_bookmusic;
	}

	if (p_ptr->blind || no_lite())
	{
		if ((!(cp_ptr->flags & CF_MYSTIC_CAST)) && (!(cp_ptr->flags & CF_MUSIC)))
		{
			msg_print("You cannot see!");
			return;
		}
		else item_tester_hook = item_tester_hook_bookmusic;
	}

	/* Get an item */

	/* Can use instruments, can't use books */
	if (!literate() && ((cp_ptr->flags & CF_MUSIC))) 
	{
		o_ptr = &inventory[INVEN_MUSIC];
		if (!o_ptr->tval)
		{
			msg_print("You have nothing to play tunes with.");
			return;
		}

		/* Forget the item_tester_hook restriction */
		item_tester_hook = NULL;
	}
	else
	{
		/* Can read books, can't use instruments */
		if (literate() && (!(cp_ptr->flags & CF_MUSIC))) 
			if (!get_item(&item, "Use which book? ", "You have no books that you can use.", 
			(USE_INVEN | USE_FLOOR))) return;

		/* Can use both instruments and books */
		if (literate() && ((cp_ptr->flags & CF_MUSIC))) 
			if (!get_item(&item, "Use which book or musical instrument? ", 
				"You have no books or musical instruments that you can use.", 
				(USE_INVEN | USE_FLOOR | USE_EQUIP))) return;

		/* Get the item (in the pack) */
		if (item >= 0)
		{
			o_ptr = &inventory[item];
		}

		/* Get the item (on the floor) */
		else
		{
			o_ptr = &o_list[0 - item];
		}
	}

	/* Track the object kind */
	object_kind_track(o_ptr->k_idx, o_ptr->pval);

	/* Hack -- Handle stuff */
	handle_stuff();

	if (o_ptr->tval == TV_MAGIC_BOOK) do_cast_or_pray(o_ptr->sval);
	else do_play(o_ptr->sval, o_ptr->pval);
}
