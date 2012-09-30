/* File: zbmagic3.c */
/* Purpose: Medium level stuff for the Borg -BEN- */


#include "angband.h"

#ifdef ALLOW_BORG

#include "zborg1.h"
#include "zborg2.h"
#include "zborg3.h"
#include "zborg4.h"
#include "zborg5.h"
#include "zborg6.h"
#include "zbmagic.h"


/*
 *
 * There are several types of setup moves:
 *
 *   Temporary speed
 *   Protect From Evil
 *   Bless\Prayer
 *   Berserk\Heroism
 *   Temp Resist (either all or just cold/fire?)
 *   Shield
 *   Teleport away
 *   Glyph of Warding
 *   See inviso
 *
 * * and many others
 *
 */

enum
{
	BD_SPEED,
	BD_PROT_FROM_EVIL,
	BD_BLESS,
	BD_HERO_BERSERK,
	BD_RESIST_FCE,
	BD_RESIST_FECAP,
	BD_RESIST_F,
	BD_RESIST_C,
	BD_RESIST_A,
	BD_RESIST_P,
	BD_SHIELD,
	BD_GOI,
	BD_GOI_POT,
	BD_GLYPH,
	BD_WARDING,
	BD_TELL_AWAY,
	BD_CREATE_WALLS,
	BD_MASS_GENOCIDE,
	BD_GENOCIDE,
	BD_GENOCIDE_HOUNDS,
	BD_EARTHQUAKE,
	BD_DESTRUCTION,
	BD_BANISHMENT,
	BD_DETECT_INVISO,
	BD_TELEPATHY,
	BD_LIGHT_BEAM,
	BD_TRUMP_SERVANT,

	BD_MAX
};


/* basic method to calulate what fail_rate is allowed */
static int borg_fail_allowed(int p1)
{
	int fail_allowed = 39;

	/* If very scary, do not allow for much chance of fail */
	if (p1 > avoidance)
	{
		fail_allowed -= 19;
	}
	/* a little scary */
	else if (p1 > (avoidance * 2) / 3)
	{
		fail_allowed -= 10;
	}
	/* not very scary, allow lots of fail */
	else if (p1 < avoidance / 3)
	{
		fail_allowed += 10;
	}

	/* Give it back */
	return (fail_allowed);
}


/*
 * Bless/Prayer to prepare for battle
 */
static int borg_defend_aux_bless(int p1)
{
	int fail_allowed = 10;

	if (borg_simulate)
	{
		/* already blessed */
		if (borg_bless) return (0);

		/* Check if the borg be blessed (weakest last) */
		if (!borg_spell_okay_fail(REALM_LIFE, 3, 1, fail_allowed) &&
			!borg_spell_okay_fail(REALM_LIFE, 0, 2, fail_allowed) &&
			!borg_activate_fail(BORG_ACT_BLESS) &&
			!borg_read_scroll_fail(SV_SCROLL_HOLY_PRAYER) &&
			!borg_read_scroll_fail(SV_SCROLL_HOLY_CHANT) &&
			!borg_read_scroll_fail(SV_SCROLL_BLESSING))
			return (0);

		/* if we are in some danger but not much, go for a quick bless */
		if (p1 > avoidance / 12 && p1 < avoidance / 2)
		{
			/* bless is a low priority */
			return (1);
		}

		/* Don't do it */
		return (0);
	}

	/* do it! */
	return (borg_activate_fail(BORG_ACT_BLESS) ||
		borg_spell(REALM_LIFE, 3, 1) ||
		borg_spell(REALM_LIFE, 0, 2) ||
		borg_read_scroll(SV_SCROLL_BLESSING) ||
		borg_read_scroll(SV_SCROLL_HOLY_CHANT) ||
		borg_read_scroll(SV_SCROLL_HOLY_PRAYER));
}

/*
 * Speed to prepare for battle
 */
static int borg_defend_aux_speed(int p1)
{
	int p2;
	bool good_speed = FALSE;
	int fail_allowed;

	if (borg_simulate)
	{
		/* Already fast */
		if (borg_speed) return (0);

		/* Get the allowed fail_rate */
		fail_allowed = borg_fail_allowed(p1);

		/* Only cast defence spells if fail rate is not too high */
		if (borg_spell_okay_fail(REALM_SORCERY, 1, 5, fail_allowed) ||
			borg_spell_okay_fail(REALM_DEATH, 2, 3, fail_allowed) ||
			borg_mindcr_okay_fail(MIND_ADRENALINE, 35, fail_allowed) ||
			borg_equips_staff_fail(SV_STAFF_SPEED) ||
			borg_equips_rod(SV_ROD_SPEED))
		{
			/* Recheargable speeds */
			good_speed = TRUE;
		}

		/* If there is no speed source */
		if (!good_speed && !borg_slot(TV_POTION, SV_POTION_SPEED))
		{
			/* Give up */
			return (0);
		}

		/* pretend we are sped up and look again */
		borg_speed = TRUE;
		p2 = borg_danger(c_x, c_y, 1, TRUE);
		borg_speed = FALSE;

		/* if we are fighting a unique cast it. */
		if (borg_fighting_unique)
		{
			/* HACK pretend that it was scary and will be safer */
			p2 = p2 * 7 / 10;
		}
		/* if the unique is Oberon cast it */
		if (bp_ptr->depth == 99 && borg_fighting_unique >= BORG_QUESTOR)
		{
			p2 = p2 * 6 / 10;
		}

		/* if the unique is The Serpent cast it */
		if (bp_ptr->depth == 100 && borg_fighting_unique >= BORG_QUESTOR)
		{
			p2 = p2 * 5 / 10;
		}

		/*
		 * If this is an improvement and we may
		 * not avoid monster now and we may have before
		 */
		if (((p1 > p2) &&
			 p2 <= (borg_fighting_unique ? ((avoidance * 2) / 3) : (avoidance / 2))
			 && (p1 > (avoidance / 5)) && good_speed) ||
			((p1 > p2) &&
			 p2 <= (borg_fighting_unique ? ((avoidance * 2) / 3) :
					(avoidance / 3)) && (p1 > (avoidance / 7))))
		{
			/* Simulation */
			return (p1 - p2 + (borg_goi / 100) * 50);

		}

		/* default to can't do it. */
		return (0);
	}

	/* Do it! */
	return (borg_spell(REALM_SORCERY, 1, 5) ||
		borg_spell(REALM_DEATH, 2, 3) ||
		borg_mindcr(MIND_ADRENALINE, 35) ||
		borg_zap_rod(SV_ROD_SPEED) ||
		borg_use_staff(SV_STAFF_SPEED) ||
		borg_quaff_potion(SV_POTION_SPEED));
}

/*
 * Globe of Invulnurability
 */
static int borg_defend_aux_goi(int p1)
{
	int p2;
	int fail_allowed;

	if (borg_simulate)
	{
		/* does the borg have the spell up already? */
		if (borg_goi) return (0);

		/* Get the allowed fail_rate */
		fail_allowed = borg_fail_allowed(p1);

		/* If fighting regular unique boost the fail rate */
		if (borg_fighting_unique >= 1) fail_allowed = MAX(25, fail_allowed);

		/* If fighting Questor */
		if (borg_fighting_unique >= BORG_QUESTOR)
		{
			/* boost the fail rate */
			fail_allowed = MAX(33, fail_allowed);
		}

		/* Do we have the spell? */
		if (!borg_activate_fail(BORG_ACT_INVULNERABILITY) &&
			!borg_spell_okay_fail(REALM_SORCERY, 3, 7, fail_allowed) &&
			!borg_spell_okay_fail(REALM_LIFE, 3, 7, fail_allowed))
			return (0);

		/* pretend we are protected and look again */
		borg_goi = 100;
		p2 = borg_danger(c_x, c_y, 1, TRUE);
		borg_goi = 0;

		/*  if we are fighting a unique enhance the value by reducing p2 */
		if (borg_fighting_unique)
		{
			p2 = p2 / 2;
		}

		/* if the unique is Oberon cast it */
		if (bp_ptr->depth == 99 && borg_fighting_unique >= BORG_QUESTOR)
		{
			p2 = p2 * 4 / 10;
		}

		/* if the unique is the Serpent cast it */
		if (bp_ptr->depth == 100 && borg_fighting_unique >= BORG_QUESTOR)
		{
			p2 = 0;
		}

		/* if this is an improvement and we may not avoid monster now and */
		/* we may have before */
		if (p1 > p2 &&
			p1 > (avoidance / 7) &&
			p2 <= (borg_fighting_unique) ? ((avoidance * 2) / 3) : (avoidance / 2))
		{
			/* Simulation */
			return (p1 - p2);
		}

		/* default to can't do it. */
		return (0);
	}

	/* do it! */
	return (borg_activate(BORG_ACT_INVULNERABILITY) ||
		borg_spell(REALM_SORCERY, 3, 7) ||
		borg_spell(REALM_LIFE, 3, 7));
}

/*
 * Globe of Invulnurability Potion
 */
static int borg_defend_aux_goi_pot(int p1)
{
	int p2;

	if (borg_simulate)
	{
		if (borg_goi) return (0);

		/* Save for fighting uniques */
		if (!borg_fighting_unique) return (0);

		/* have some in inven? */
		if (!borg_slot(TV_POTION, SV_POTION_INVULNERABILITY)) return (0);

		/* pretend we are protected and look again */
		borg_goi = 100;
		p2 = borg_danger(c_x, c_y, 1, TRUE);
		borg_goi = 0;

		/*  Fighting a unique, enhance the value by reducing p2 */
		p2 = p2 / 2;

		/* if the unique is Oberon cast it */
		if (bp_ptr->depth == 99 && borg_fighting_unique >= BORG_QUESTOR)
		{
			p2 = p2 * 4 / 10;
		}

		/* if the unique is The Serpent cast it */
		if (bp_ptr->depth == 100 && borg_fighting_unique >= BORG_QUESTOR)
		{
			p2 = 0;
		}

		/* if this is an improvement and we may not avoid monster now and */
		/* we may have before */
		if (p1 > p2 &&
			p1 > (avoidance / 7) &&
			p2 <= (borg_fighting_unique) ? ((avoidance * 2) / 3) : (avoidance / 2))
		{
			/* Simulation */
			return (p1 - p2);
		}

		/* default to can't do it. */
		return (0);
	}

	/* do it! */
	return (borg_quaff_potion(SV_POTION_INVULNERABILITY));
}

/* cold/fire */
static int borg_defend_aux_resist_fce(int p1)
{
	int p2;
	int fail_allowed;
	bool save_fire, save_cold, save_elec;

	if (borg_simulate)
	{
		if (my_oppose_fire && my_oppose_cold && my_oppose_elec) return (0);

		/* Get the allowed fail_rate */
		fail_allowed = borg_fail_allowed(p1);

		/* Does the borg have the spell? */
		if (!borg_spell_okay_fail(REALM_NATURE, 0, 6, fail_allowed) &&
			!borg_activate_fail(BORG_ACT_RESISTANCE)) return (0);

		/* pretend we are protected and look again */
		save_fire = my_oppose_fire;
		save_cold = my_oppose_cold;
		save_elec = my_oppose_elec;

		my_oppose_fire = TRUE;
		my_oppose_cold = TRUE;
		my_oppose_elec = TRUE;
		p2 = borg_danger(c_x, c_y, 1, FALSE);
		my_oppose_fire = save_fire;
		my_oppose_cold = save_cold;
		my_oppose_elec = save_elec;

		/* if this is an improvement and we may not avoid monster now and */
		/* we may have before */
		if (p1 > p2 &&
			p2 <= (borg_fighting_unique ? ((avoidance * 2) / 3) : (avoidance / 2))
			&& p1 > (avoidance / 7))
		{
			/* Simulation */
			return (p1 - p2);
		}

		/* default to can't do it. */
		return (0);
	}

	/* do it! */
	return (borg_activate(BORG_ACT_RESISTANCE) ||
		borg_spell(REALM_NATURE, 0, 6));
}

/* all resists */
static int borg_defend_aux_resist_fecap(int p1)
{
	int p2;
	int fail_allowed;
	bool save_fire, save_acid, save_poison, save_elec, save_cold;

	if (borg_simulate)
	{
		/* Does the borg already have al the resists? */
		if (my_oppose_fire && my_oppose_acid && my_oppose_pois &&
			my_oppose_elec && my_oppose_cold) return (0);

		/* Get the allowed fail_rate */
		fail_allowed = borg_fail_allowed(p1);

		/*
		 * How about adding the potion of Resistance?
		 * Won't want to cast it though if only one element is
		 * down.  Ought to at least wait until 3 of the 4 are down.
		 */
		if (!borg_spell_okay_fail(REALM_NATURE, 2, 3, fail_allowed) &&
			!borg_mindcr_okay_fail(MIND_CHAR_ARMOUR, 33, fail_allowed) &&
			!borg_activate_fail(BORG_ACT_RESISTANCE) &&
			!borg_mutation_check(MUT1_RESIST, TRUE) &&
			!borg_slot(TV_POTION, SV_POTION_RESISTANCE))
			return (0);

		/* pretend we are protected and look again */
		save_fire = my_oppose_fire;
		save_acid = my_oppose_acid;
		save_poison = my_oppose_pois;
		save_elec = my_oppose_elec;
		save_cold = my_oppose_cold;
		my_oppose_fire = TRUE;
		my_oppose_cold = TRUE;
		my_oppose_acid = TRUE;
		my_oppose_pois = TRUE;
		my_oppose_elec = TRUE;
		p2 = borg_danger(c_x, c_y, 1, FALSE);
		my_oppose_fire = save_fire;
		my_oppose_cold = save_cold;
		my_oppose_acid = save_acid;
		my_oppose_pois = save_poison;
		my_oppose_elec = save_elec;

		/* if this is an improvement and we may not avoid monster now and */
		/* we may have before */
		if (p1 > p2 &&
			p2 <= (borg_fighting_unique ? ((avoidance * 2) / 3) : (avoidance / 2))
			&& p1 > (avoidance / 7))
		{
			/* Simulation */
			return ((p1 - p2) - 1);
		}

		/* default to can't do it. */
		return (0);
	}

	/* do it! */
	return (borg_activate(BORG_ACT_RESISTANCE) ||
		borg_spell(REALM_NATURE, 2, 3) ||
		borg_mindcr(MIND_CHAR_ARMOUR, 33) ||
		borg_mutation(MUT1_RESIST) ||
		borg_quaff_potion(SV_POTION_RESISTANCE));
}

/* fire */
static int borg_defend_aux_resist_f(int p1)
{
	int p2;
	int fail_allowed;
	bool save_fire;

	if (borg_simulate)
	{
		if (my_oppose_fire) return (0);

		/* Get the allowed fail_rate */
		fail_allowed = borg_fail_allowed(p1);

		if (!borg_spell_okay_fail(REALM_ARCANE, 1, 7, fail_allowed) &&
			!borg_mindcr_okay_fail(MIND_CHAR_ARMOUR, 21, fail_allowed) &&
			!borg_spell_okay_fail(REALM_NATURE, 0, 6, fail_allowed) &&
			!borg_spell_okay_fail(REALM_NATURE, 2, 3, fail_allowed) &&
			!borg_activate_fail(BORG_ACT_RESIST_FIRE) &&
			!borg_activate_fail(BORG_ACT_RESISTANCE) &&
			!borg_slot(TV_POTION, SV_POTION_RESIST_HEAT))
			return (0);

		/* pretend we are protected and look again */
		save_fire = my_oppose_fire;
		my_oppose_fire = TRUE;
		p2 = borg_danger(c_x, c_y, 1, FALSE);
		my_oppose_fire = save_fire;

		/* if this is an improvement and we may not avoid monster now and */
		/* we may have before */
		if (p1 > p2 &&
			p2 <= (borg_fighting_unique ? ((avoidance * 2) / 3) : (avoidance / 2))
			&& p1 > (avoidance / 7))
		{
			/* Simulation */
			return (p1 - p2);
		}

		/* default to can't do it. */
		return (0);
	}

	/* do it! */
	return (borg_activate(BORG_ACT_RESIST_FIRE) ||
		borg_activate(BORG_ACT_RESISTANCE) ||
		borg_spell(REALM_NATURE, 0, 6) ||
		borg_spell(REALM_ARCANE, 1, 7) ||
		borg_spell(REALM_NATURE, 2, 3) ||
		borg_mindcr(MIND_CHAR_ARMOUR, 21) ||
		borg_quaff_potion(SV_POTION_RESIST_HEAT));
}

 /* cold */
static int borg_defend_aux_resist_c(int p1)
{

	int p2;
	int fail_allowed;
	bool save_cold;

	if (borg_simulate)
	{
		if (my_oppose_cold) return (0);

		/* Get the allowed fail_rate */
		fail_allowed = borg_fail_allowed(p1);

		if (!borg_spell_okay_fail(REALM_ARCANE, 1, 7, fail_allowed) &&
			!borg_mindcr_okay_fail(MIND_CHAR_ARMOUR, 25, fail_allowed) &&
			!borg_activate_fail(BORG_ACT_RESIST_COLD) &&
			!borg_activate_fail(BORG_ACT_RESISTANCE) &&
			!borg_slot(TV_POTION, SV_POTION_RESIST_COLD))
			return (0);

		/* pretend we are protected and look again */
		save_cold = my_oppose_cold;
		my_oppose_cold = TRUE;
		p2 = borg_danger(c_x, c_y, 1, FALSE);
		my_oppose_cold = save_cold;

		/* if this is an improvement and we may not avoid monster now and */
		/* we may have before */
		if (p1 > p2 &&
			p2 <= (borg_fighting_unique ? ((avoidance * 2) / 3) : (avoidance / 2))
			&& p1 > (avoidance / 7))
		{
			/* Simulation */
			return (p1 - p2);
		}

		/* default to can't do it. */
		return (0);
	}

	/* do it! */
	return (borg_activate(BORG_ACT_RESIST_COLD) ||
		borg_activate(BORG_ACT_RESISTANCE) ||
		borg_spell(REALM_ARCANE, 1, 7) ||
		borg_mindcr(MIND_CHAR_ARMOUR, 25) ||
		borg_quaff_potion(SV_POTION_RESIST_COLD));
}

/* acid */
static int borg_defend_aux_resist_a(int p1)
{

	int p2;
	int fail_allowed;
	bool save_acid;

	if (borg_simulate)
	{
		if (my_oppose_acid) return (0);

		/* Get the allowed fail_rate */
		fail_allowed = borg_fail_allowed(p1);

		if (!borg_spell_okay_fail(REALM_ARCANE, 2, 1, fail_allowed) &&
			!borg_activate_fail(BORG_ACT_RESIST_ACID) &&
			!borg_activate_fail(BORG_ACT_RESISTANCE) &&
			!borg_mindcr_okay_fail(MIND_CHAR_ARMOUR, 17, fail_allowed))
			return (0);

		/* pretend we are protected and look again */
		save_acid = my_oppose_acid;
		my_oppose_acid = TRUE;
		p2 = borg_danger(c_x, c_y, 1, FALSE);
		my_oppose_acid = save_acid;

		/* if this is an improvement and we may not avoid monster now and */
		/* we may have before */
		if (p1 > p2 &&
			p2 <= (borg_fighting_unique ? ((avoidance * 2) / 3) : (avoidance / 2))
			&& p1 > (avoidance / 7))
		{
			/* Simulation */
			return (p1 - p2);
		}
		/* default to can't do it. */
		return (0);
	}

	/* do it! */
	return (borg_activate(BORG_ACT_RESIST_ACID) ||
		borg_activate(BORG_ACT_RESISTANCE) ||
		borg_mindcr(MIND_CHAR_ARMOUR, 15) ||
		borg_spell(REALM_ARCANE, 2, 1));
}

/* poison */
static int borg_defend_aux_resist_p(int p1)
{
	int p2;
	int fail_allowed;
	bool save_poison;

	if (borg_simulate)
	{
		if (my_oppose_pois) return (0);

		/* Get the allowed fail_rate */
		fail_allowed = borg_fail_allowed(p1);

		if (!borg_spell_okay_fail(REALM_DEATH, 0, 5, fail_allowed) &&
			!borg_mindcr_okay_fail(MIND_CHAR_ARMOUR, 33, fail_allowed) &&
			!borg_activate_fail(BORG_ACT_RESIST_POISON) &&
			!borg_activate_fail(BORG_ACT_RESISTANCE))
			return (0);

		/* pretend we are protected and look again */
		save_poison = my_oppose_pois;
		my_oppose_pois = TRUE;
		p2 = borg_danger(c_x, c_y, 1, FALSE);
		my_oppose_pois = save_poison;

		/* if this is an improvement and we may not avoid monster now and */
		/* we may have before */
		if (p1 > p2 &&
			p2 <= (borg_fighting_unique ? ((avoidance * 2) / 3) : (avoidance / 2))
			&& p1 > (avoidance / 7))
		{
			/* Simulation */
			return (p1 - p2);
		}

		/* default to can't do it. */
		return (0);
	}

	/* do it! */
	return (borg_activate(BORG_ACT_RESIST_POISON) ||
		borg_activate(BORG_ACT_RESISTANCE) ||
		borg_mindcr(MIND_CHAR_ARMOUR, 33) ||
		borg_spell(REALM_DEATH, 0, 5));
}

static int borg_defend_aux_prot_evil(int p1)
{
	int p2;
	int fail_allowed;

	if (borg_simulate)
	{
		/* if already protected */
		if (borg_prot_from_evil || FLAG(bp_ptr, TR_SLAY_EVIL)) return (0);

		/* Get the allowed fail_rate */
		fail_allowed = borg_fail_allowed(p1);

		/* Is the spell available? */
		if (!borg_spell_okay_fail(REALM_LIFE, 1, 5, fail_allowed) &&
			!borg_activate_fail(BORG_ACT_PROT_EVIL) &&
			!borg_read_scroll_fail(SV_SCROLL_PROTECTION_FROM_EVIL)) return (0);

		/* pretend we are protected and look again */
		borg_prot_from_evil = TRUE;
		p2 = borg_danger(c_x, c_y, 1, FALSE);
		borg_prot_from_evil = FALSE;

		/* if this is an improvement and we may not avoid monster now and */
		/* we may have before */

		if (p1 > p2 &&
			p2 <= (borg_fighting_unique ? ((avoidance * 2) / 3) : (avoidance / 2))
			&& p1 > (avoidance / 7))
		{
			/* Simulation */
			return (p1 - p2);
		}

		/* default to can't do it. */
		return (0);
	}

	/* do it! */
	return (borg_spell(REALM_LIFE, 1, 5) ||
		borg_activate(BORG_ACT_PROT_EVIL) ||
		borg_read_scroll(SV_SCROLL_PROTECTION_FROM_EVIL));
}

static int borg_defend_aux_shield(int p1)
{
	int p2;
	int fail_allowed;

	if (borg_simulate)
	{
		/* if already protected */
		if (borg_shield || borg_goi) return (0);

		/* Get the allowed fail_rate */
		fail_allowed = borg_fail_allowed(p1);

		if (!borg_spell_okay_fail(REALM_NATURE, 2, 2, fail_allowed) &&
			!borg_mindcr_okay_fail(MIND_CHAR_ARMOUR, 13, fail_allowed) &&
			!borg_racial_check(RACE_GOLEM, TRUE))
			return (0);

		/* pretend we are protected and look again */
		borg_shield = TRUE;
		p2 = borg_danger(c_x, c_y, 1, TRUE);
		borg_shield = FALSE;

		/* slightly enhance the value if fighting a unique */
		if (borg_fighting_unique) p2 = (p2 * 7 / 10);

		/* if this is an improvement and we may not avoid monster now and */
		/* we may have before */
		if (p1 > p2 &&
			p2 <= (borg_fighting_unique ? ((avoidance * 2) / 3) : (avoidance / 2))
			&& p1 > (avoidance / 7))
		{
			/* Simulation */
			return (p1 - p2);
		}

		/* default to can't do it. */
		return (0);
	}

	/* do it! */
	return (borg_spell(REALM_NATURE, 2, 2) ||
		borg_mindcr(MIND_CHAR_ARMOUR, 13) ||
		borg_racial(RACE_GOLEM));
}

/*
 * Try to get rid of all of the non-uniques around so you can go at it
 * 'mano-e-mano' with the unique.
 */
static int borg_defend_aux_tell_away(int p1)
{
	int p2, b_n;
	int fail_allowed = 30;

	if (borg_simulate)
	{
		/* Only tell away if scared */
		if (p1 < avoidance) return (0);

		/* if very scary, do not allow for much chance of fail */
		if (p1 > avoidance * 4)
		{
			fail_allowed -= 18;
		}
		/* scary */
		else if (p1 > avoidance * 3)
		{
			fail_allowed -= 12;
		}
		/* a little scary */
		else if (p1 > (avoidance * 5) / 2)
		{
			fail_allowed += 5;
		}

		if (!borg_spell_okay_fail(REALM_ARCANE, 3, 3, fail_allowed) &&
			!borg_spell_okay_fail(REALM_SORCERY, 1, 4, fail_allowed) &&
			!borg_spell_okay_fail(REALM_CHAOS, 1, 5, fail_allowed) &&
			!borg_activate_fail(BORG_ACT_TELEPORT_AWAY) &&
			!borg_equips_rod_fail(SV_ROD_TELEPORT_AWAY) &&
			!borg_equips_wand_fail(SV_WAND_TELEPORT_AWAY))
			return (0);

		/* Try all monsters for the best shot */
		b_n = borg_launch_beam(50, GF_AWAY_ALL, MAX_RANGE);

		/* normalize the value */
		p2 = MAX(p1 - b_n, 0);

		/* check to see if I am left better off */
		if (p1 > p2 &&
			p2 <= (borg_fighting_unique ? ((avoidance * 2) / 3) : (avoidance / 2))
			&& p1 > (avoidance / 7))
		{
			/* Simulation */
			return (b_n);
		}

		/* Oh well */
		return (0);
	}

	/* Set the target */
	borg_target(g_x, g_y);

	/* Set our shooting flag */
	successful_target = BORG_FRESH_TARGET;

	/* Cast the spell */
	return (borg_spell(REALM_SORCERY, 1, 4) ||
		borg_spell(REALM_ARCANE, 3, 3) ||
		borg_spell(REALM_CHAOS, 1, 5) ||
		borg_activate(BORG_ACT_TELEPORT_AWAY) ||
		borg_zap_rod(SV_ROD_TELEPORT_AWAY) ||
		borg_aim_wand(SV_WAND_TELEPORT_AWAY));
}

/*
 * Hero to prepare for battle
 */
static int borg_defend_aux_hero(int p1)
{
	int fail_allowed = 10;

	if (borg_simulate)
	{
		/* already hero */
		if (borg_hero || borg_berserk) return (0);

		/* Is there some way to berserk? */
		if (!borg_spell_okay_fail(REALM_LIFE, 3, 0, fail_allowed) &&
			!borg_spell_okay_fail(REALM_DEATH, 2, 0, fail_allowed) &&
			!borg_mindcr_okay_fail(MIND_ADRENALINE, 23, fail_allowed) &&
			!borg_activate_fail(BORG_ACT_HEROISM) &&
			!borg_activate_fail(BORG_ACT_BERSERKER) &&
			!borg_racial_check(RACE_HALF_TROLL, TRUE) &&
			!borg_racial_check(RACE_BARBARIAN, TRUE) &&
			!borg_mutation_check(MUT1_BERSERK, TRUE) &&
			!borg_slot(TV_POTION, SV_POTION_BERSERK_STRENGTH) &&
			!borg_slot(TV_POTION, SV_POTION_HEROISM)) return (0);

		/* if we are in some danger but not much, go for a quick heroism */
		if (borg_goi || (p1 > avoidance / 12 && p1 < avoidance / 2) ||
			(borg_fighting_unique && p1 < avoidance * 13 / 10))
		{
			/* Simulation */
			return (1);
		}

		/* Never mind */
		return (0);
	}

	/* do it! */
	return (borg_spell(REALM_LIFE, 3, 0) ||
		borg_spell(REALM_DEATH, 2, 0) ||
		borg_mindcr(MIND_ADRENALINE, 23) ||
		borg_activate(BORG_ACT_HEROISM) ||
		borg_activate(BORG_ACT_BERSERKER) ||
		borg_racial(RACE_HALF_TROLL) ||
		borg_racial(RACE_BARBARIAN) ||
		borg_mutation(MUT1_BERSERK) ||
		borg_quaff_potion(SV_POTION_BERSERK_STRENGTH) ||
		borg_quaff_potion(SV_POTION_HEROISM));
}


/* Glyph of Warding and Rune of Protection */
static int borg_defend_aux_glyph(int p1)
{
	int p2, i;
	int fail_allowed;
	map_block *mb_ptr = map_loc(c_x, c_y);

	if (borg_simulate)
	{
		/* He should not cast it while on an object.
		 * I have addressed this inadequately in borg9.c when dealing with
		 * messages.  The message "the object resists" will delete the glyph
		 * from the array.  Then I set a broken door on that spot, the borg ignores
		 * broken doors, so he won't loop.
		 */

		if ((mb_ptr->object) || (mb_ptr->m_effect) || (mb_ptr->trap) ||
			(mb_ptr->feat == FEAT_LESS) || (mb_ptr->feat == FEAT_MORE) ||
			(mb_ptr->feat == FEAT_CLOSED) || (mb_ptr->feat == FEAT_OPEN) ||
			(mb_ptr->feat == FEAT_BROKEN))
		{
			/* Something is in the way */
			return (0);
		}

		/* The Serpent breaks these in one try so its a waste of mana against him */
		if (borg_fighting_unique >= BORG_QUESTOR) return (0);

		/* Get the allowed fail_rate */
		fail_allowed = borg_fail_allowed(p1);

		if (!borg_spell_okay_fail(REALM_LIFE, 1, 7, fail_allowed) &&
			!borg_read_scroll_fail(SV_SCROLL_RUNE_OF_PROTECTION)) return (0);

		/* pretend we are protected and look again */
		borg_on_glyph = TRUE;
		p2 = borg_danger(c_x, c_y, 1, TRUE);
		borg_on_glyph = FALSE;

		/* if this is an improvement and we may not avoid monster now and */
		/* we may have before */
		if (p1 > p2 &&
			p2 <= (borg_fighting_unique ? ((avoidance * 2) / 3) : (avoidance / 2))
			&& p1 > (avoidance / 7))
		{
			/* Simulation */
			return (p1 - p2);
		}

		/* default to can't do it. */
		return (0);
	}

	/* Check for an existing glyph */
	for (i = 0; i < track_glyph_num; i++)
	{
		/* Stop if we already knew about this glyph */
		if (track_glyph_x[i] == c_x &&
			track_glyph_y[i] == c_y) break;
	}

	/* Track the newly discovered glyph */
	if (i == track_glyph_num)
	{
		/* If the borg makes too many glyphs */
		if (track_glyph_size == track_glyph_num)
		{
			/* Please recompile with a higher track_glyph_size value */
			borg_oops("Borg makes too many glyphs.  Increase track_glyph_num");
		}

		/* Keep track of the existing glyphs */
		borg_note("# Noting the creation of a glyph.");
		track_glyph_num++;
		track_glyph_x[i] = c_x;
		track_glyph_y[i] = c_y;
	}

	/* do it! */
	return (borg_spell(REALM_LIFE, 1, 7) ||
		borg_read_scroll(SV_SCROLL_RUNE_OF_PROTECTION));
}

/* True Warding */
static int borg_defend_aux_true_warding(int p1)
{
	/* Ignore parameter */
	(void)p1;
#if 0
	int p2;
	int fail_allowed;
	int glyph_bad = 0;
	int glyph_x, glyph_y, x, y;

	map_block *mb_ptr;

	/* any summoners near? */
	if (!borg_fighting_summoner) return (0);

	/* Get the allowed fail_rate */
	fail_allowed = borg_fail_allowed(p1);

	if (!borg_spell_okay_fail(REALM_LIFE, 2, 7, fail_allowed)) return (0);

	/* Do not cast if surounded by doors or something */
	/* Get grid */
	for (glyph_x = -1; glyph_x <= 1; glyph_x++)
	{
		for (glyph_y = -1; glyph_y <= 1; glyph_y++)
		{
			/* Acquire location */
			x = glyph_x + c_x;
			y = glyph_y + c_y;

			/* Bounds checking */
			if (!map_in_bounds(x, y)) continue;

			mb_ptr = map_loc(x, y);

			/* track spaces already protected */
			if ((mb_ptr->feat >= FEAT_CLOSED) &&
				(mb_ptr->feat <= FEAT_PERM_SOLID))
			{
				glyph_bad++;
			}

			/* track spaces that cannot be protected */
			if ((mb_ptr->object)	/* ||
									   ((mb_ptr->feat >= FEAT_TRAP_TRAPDOOR) && (mb_ptr->feat <= FEAT_TRAP_SLEEP)) */
				||
				(mb_ptr->feat == FEAT_LESS) || (mb_ptr->feat == FEAT_MORE)
				|| (mb_ptr->feat == FEAT_OPEN) ||
				(mb_ptr->feat == FEAT_BROKEN) || (mb_ptr->monster))
			{
				glyph_bad++;
			}
		}
	}


	/* Track it */
	/* lets make sure that we going to be benifited */
	if (glyph_bad >= 6)
	{
		/* not really worth it.  Only 2 spaces protected */
		return (0);
	}

	/* pretend we are protected and look again (use the door code) */
	borg_create_door = TRUE;
	p2 = borg_danger(c_x, c_y, 1, TRUE);
	borg_create_door = FALSE;

	/* if this is an improvement and we may not avoid monster now and */
	/* we may have before */
	if (p1 > p2 &&
		p2 <= (borg_fighting_unique ? ((avoidance * 2) / 3) : (avoidance / 2))
		&& p1 > (avoidance / 7))
	{
		/* Simulation */
		if (borg_simulate) return (p1 - p2);

		/* do it! */
		if (borg_spell_fail(REALM_LIFE, 2, 7, fail_allowed))
		{
			/* Set the breeder flag to keep doors closed. Avoid summons */
			breeder_level = TRUE;

			/* Value */
			return (p1 - p2);
		}
	}
#endif /* 0 */

	/* default to can't do it. */
	return (0);
}


/* Create Granite Walls-- Nature spell */
static int borg_defend_aux_create_walls(int p1)
{
	/* Ignore parameter */
	(void)p1;
#if 0
	int p2 = 0;
	int fail_allowed = 99;
	int wall_bad = 0;
	int wall_x, wall_y, x, y;

	map_block *mb_ptr;

	/* any summoners near? */
	if (!borg_fighting_summoner) return (0);

	/* Get the allowed fail_rate */
	fail_allowed = borg_fail_allowed(p1);

	if (!borg_spell_okay_fail(REALM_NATURE, 2, 6, fail_allowed) &&
		!borg_spell_okay_fail(REALM_NATURE, 2, 0, fail_allowed))
		return (0);

	/* Do not cast if surounded by doors or something */
	/* Get grid */
	for (wall_x = -1; wall_x <= 1; wall_x++)
	{
		for (wall_y = -1; wall_y <= 1; wall_y++)
		{
			/* Acquire location */
			x = wall_x + c_x;
			y = wall_y + c_y;

			/* Bounds checking */
			if (!map_in_bounds(x, y)) continue;

			mb_ptr = map_loc(x, y);

			/* track spaces already protected */
			if (				/* (mb_ptr->feat == FEAT_GLYPH) ||
								   (mb_ptr->feat == FEAT_MINOR_GLYPH) || */ mb_ptr->
				   monster ||
				   ((mb_ptr->feat >= FEAT_CLOSED) &&
					(mb_ptr->feat <= FEAT_PERM_SOLID)))
			{
				wall_bad++;
			}

			/* track spaces that cannot be protected */
			if ((mb_ptr->object)	/*||
									   ((mb_ptr->feat >= FEAT_TRAP_TRAPDOOR) && (mb_ptr->feat <= FEAT_TRAP_SLEEP)) */
				||
				(mb_ptr->feat == FEAT_LESS) || (mb_ptr->feat == FEAT_MORE)
				|| (mb_ptr->feat == FEAT_OPEN) ||
				(mb_ptr->feat == FEAT_BROKEN) || (mb_ptr->monster))
			{
				wall_bad++;
			}
		}
	}


	/* Track it */
	/* lets make sure that we going to be benifited */
	if (wall_bad >= 6)
	{
		/* not really worth it.  Only 2 spaces protected */
		return (0);
	}

	/* pretend we are protected and look again */
	borg_create_door = TRUE;
	p2 = borg_danger(c_x, c_y, 1, TRUE);
	borg_create_door = FALSE;

	/* if this is an improvement and we may not avoid monster now and */
	/* we may have before */
	if (p1 > p2 &&
		p2 <= (borg_fighting_unique ? ((avoidance * 2) / 3) : (avoidance / 2))
		&& p1 > (avoidance / 7))
	{
		/* Simulation */
		if (borg_simulate) return (p1 - p2);

		/* do it! */
		if (borg_spell_fail(REALM_NATURE, 2, 0, fail_allowed) ||
			borg_spell_fail(REALM_NATURE, 2, 6, fail_allowed))
		{
			/* Set the breeder flag to keep doors closed. Avoid summons */
			breeder_level = TRUE;

			/* Value */
			return (p1 - p2);
		}
	}
#endif /* 0 */

	/* default to can't do it. */
	return (0);
}



/* This will simulate and cast the mass genocide spell.
 */
static int borg_defend_aux_mass_genocide(int p1)
{
	int hit = 0, i = 0, p2;
	int b_p = 0, p;

	borg_kill *kill;
	monster_race *r_ptr;

	if (borg_simulate)
	{
		/* see if prayer is legal */
		if (!borg_spell_okay_fail(REALM_DEATH, 2, 7, 40) &&
			!borg_spell_okay_fail(REALM_DEATH, 3, 6, 40) &&
			!borg_activate_fail(BORG_ACT_MASS_GENOCIDE) &&
			!borg_read_scroll_fail(SV_SCROLL_MASS_GENOCIDE)) return (0);

		/* See if he is in real danger */
		if (p1 < avoidance * 12 / 10) return (0);

		/* Find a monster and calculate its danger */
		for (i = 0; i < borg_kills_nxt; i++)
		{
			/* Monster */
			kill = &borg_kills[i];
			r_ptr = &r_info[kill->r_idx];

			/* Skip dead monsters */
			if (!kill->r_idx) continue;

			/* Check the distance */
			if (distance(c_y, c_x, kill->y, kill->x) > 20) continue;

			/* we try not to genocide uniques */
			if (FLAG(r_ptr, RF_UNIQUE)) continue;

			/* Calculate danger */
			borg_full_damage = TRUE;
			p = borg_danger_aux(c_x, c_y, 1, i, TRUE);
			borg_full_damage = FALSE;

			/* store the danger for this type of monster */
			b_p = b_p + p;
			hit = hit + 4;
		}

		/* normalize the value */
		p2 = MAX(p1 - b_p, 0);

		/* if strain (plus a pad incase we did not know about some monsters)
		 * is greater than hp, don't cast it
		 */
		if ((hit * 11 / 10) >= bp_ptr->chp) return (0);

		/* Penalize the strain from casting the spell */
		p2 = p2 + hit;

		/* Be more likely to use this if fighting The Serpent */
		if (borg_fighting_unique >= BORG_QUESTOR && (hit / 3 > 8))
		{
			p2 = p2 * 6 / 10;
		}

		/* if this is an improvement and we may not avoid monster now and */
		/* we may have before */
		if (p1 > p2 &&
			p2 <= (borg_fighting_unique ? (avoidance * 2 / 3) : (avoidance / 2)))
		{
			/* Simulation */
			return (p1 - p2);
		}
		/* Not worth it */
		return (0);
	}

	/* Cast the spell */
	return (borg_spell(REALM_DEATH, 2, 7) ||
		borg_spell(REALM_DEATH, 3, 6) ||
		borg_activate(BORG_ACT_MASS_GENOCIDE) ||
		borg_read_scroll(SV_SCROLL_MASS_GENOCIDE));
}

/* This will simulate and cast the genocide spell.
 * There are two seperate functions happening here.
 * 1. will genocide the race which is immediately threatening me.
 * 2. will genocide the race which is most dangerous on the level.  Though it may not be
 *    threatening the borg right now.  It was considered to nuke the escorts of a unique.
 *    But it could also be used to nuke a race if it becomes too dangerous, for example
 *    a summoner called up 15-20 hounds, and they must be dealt with.
 * The first option may be called at any time.  While the 2nd option is only called when the
 * borg is in relatively good health.
 */
static int borg_defend_aux_genocide(int p1, int *genocide_target)
{
	int i, p, u, b_i = 0;
	int p2 = 0;
	int threat = 0;
	int max = 1;

	int b_p[256];
	int b_num[256];
	int b_threat[256];
	int b_threat_num[256];

	int b_threat_id = (char)0;

	int fail_allowed;

	if (borg_simulate)
	{
		/* Set default target */
		*genocide_target = 0;

		/* Get the allowed fail_rate */
		fail_allowed = borg_fail_allowed(p1);

		/* Is genocide available at all? */
		if (!borg_spell_okay_fail(REALM_DEATH, 1, 6, fail_allowed) &&
			!borg_equips_staff_fail(SV_STAFF_GENOCIDE) &&
			!borg_activate_fail(BORG_ACT_GENOCIDE) &&
			!borg_read_scroll_fail(SV_SCROLL_GENOCIDE)) return (0);

		/* Don't try it if really weak */
		if (bp_ptr->chp <= 75) return (0);

		/* two methods to calculate the threat:
		 *1. cycle each character of monsters on screen
		 *   collect collective threat of each char
		 *2 select race of most dangerous guy, and choose him.
		 * Method 2 is cheaper and faster.
		 *
		 * The borg uses method #1
		 */

		/* Clear previous dangers */
		for (i = 0; i < 256; i++)
		{
			b_p[i] = 0;
			b_num[i] = 0;
			b_threat[i] = 0;
			b_threat_num[i] = 0;
		}

		/* Find a monster and calculate its danger */
		for (i = 0; i < borg_kills_nxt; i++)
		{
			borg_kill *kill;
			monster_race *r_ptr;

			/* Monster */
			kill = &borg_kills[i];
			r_ptr = &r_info[kill->r_idx];

			/* Our char of the monster */
			u = r_ptr->d_char;

			/* Skip dead monsters */
			if (!kill->r_idx) continue;

			/* we try not to genocide uniques */
			if (FLAG(r_ptr, RF_UNIQUE)) continue;

			/* Calculate danger */
			borg_full_damage = TRUE;
			p = borg_danger_aux(c_x, c_y, 1, i, TRUE);
			threat = borg_danger_aux(kill->x, kill->y, 1, i, TRUE);
			borg_full_damage = FALSE;

			/* store the danger for this type of monster */
			b_p[u] = b_p[u] + p;
			b_threat[u] = b_threat[u] + threat;

			/* Store the number of this type of monster */
			b_num[u]++;
			b_threat_num[u]++;
		}

		/* Now, see which race contributes the most danger */
		for (i = 0; i < 256; i++)
		{

			/* Skip this one if empty */
			if (!b_p[i]) continue;

			/* for the race threatening me right now */
			if (b_p[i] > max)
			{
				/* track the race */
				max = b_p[i];
				b_i = i;

				/* note the danger with this race gone */
				p2 = p1 - b_p[b_i];
			}

			/* for this race on the whole level */
			if (b_threat[i] > max)
			{
				/* track the race */
				max = b_threat[i];
				b_threat_id = i;

				/* Asses the danger on the level */
				p2 = MAX(p1 - b_threat[i], 0);
			}

		}

		/*
		 * This will track and decide if it is worth genociding this dangerous
		 * race for the level
		 */
		if (b_threat_id)
		{
			/* Not if I am weak (Have to watch out for monster pits) */
			if (bp_ptr->chp < bp_ptr->mhp ||
				bp_ptr->chp < 375) b_threat_id = 0;

			/* Do not perform in Danger */
			if (borg_danger(c_x, c_y, 1, TRUE) > avoidance/5) b_threat_id = 0;

			/* The threat must be real */
			if (b_threat[b_threat_id] < bp_ptr->mhp * 10) b_threat_id = 0;

			/* Too painful to cast it (padded to be safe) */
			if (b_num[b_threat_id] * 44 / 10 >= bp_ptr->chp) b_threat_id = 0;

			/* report the danger and most dangerous race */
			if (b_threat_id)
			{
				borg_note
					("# Race '%c' is a real threat with total danger %d from %d individuals.",
					 b_threat_id, b_threat[b_threat_id], b_threat_num[b_threat_id]);
			}

			/* Genociding this race would reduce the danger of the level */
			*genocide_target = b_threat_id;
		}

		/* Consider the immediate threat genocide */
		if (b_i)
		{
			/* Too painful to cast it (padded to be safe incase of unknown monsters) */
			if (b_num[b_i] * 44 / 10 >= bp_ptr->chp) b_i = 0;

			/* See if he is in real danger, generally,
			 * or deeper in the dungeon, conservatively,
			 */
			if (p1 < avoidance * 12 / 10 ||
				(bp_ptr->depth > 75 && p1 < avoidance * 7 / 10)) b_i = 0;

			/* Did this help improve my situation? */
			if (p1 < p2 && p2 >= (avoidance / 2)) b_i = 0;

			/* Genociding this race would help me immediately */
			*genocide_target = b_i;
		}

		/* Complete the genocide routine */
		if (*genocide_target)
		{
			/* Simulation */
			return (p1 - p2);
		}

		/* default to can't do it. */
		return (0);
	}

	borg_note
		("# Genociding race '%c' (%d)", *genocide_target, *genocide_target);

	/* do it! ---use scrolls first since they clutter inventory */
	if (borg_read_scroll(SV_SCROLL_GENOCIDE) ||
		borg_spell(REALM_DEATH, 1, 6) ||
		borg_activate(BORG_ACT_GENOCIDE) ||
		borg_use_staff(SV_STAFF_GENOCIDE))
	{
		/* and the winner is..... */
		borg_keypress((char)*genocide_target);

		/* Remove this race from the borg_kill */
		for (i = 0; i < borg_kills_nxt; i++)
		{
			borg_kill *kill;
			monster_race *r_ptr;

			/* Monster */
			kill = &borg_kills[i];
			r_ptr = &r_info[kill->r_idx];

			/* Our char of the monster */
			if (r_ptr->d_char != *genocide_target) continue;

			/* remove this monster */
			borg_delete_kill(i, "genocided");
		}

		return (TRUE);
	}

	/* Inconceivable */
	borg_oops("Decided to genocide without having genocide!");
	return (FALSE);
}


/* This will cast the genocide spell on Hounds at the beginning of each level.
 */
static int borg_defend_aux_genocide_hounds(int p1)
{
	int i = 0;
	char genocide_target = 'Z';

	if (borg_simulate)
	{
		/* Not if I am weak */
		if (bp_ptr->chp < bp_ptr->mhp || bp_ptr->chp < 350) return (0);

		/* only do it when deep, */
		if (bp_ptr->depth < 50) return (0);

		/* Do not perform in Danger */
		if (p1 > avoidance / 3) return (0);

		/* Is the spell available? */
		if (!borg_spell_okay_fail(REALM_DEATH, 1, 6, 35) &&
			!borg_activate_fail(BORG_ACT_GENOCIDE) &&
			!borg_equips_staff_fail(SV_STAFF_GENOCIDE)) return (0);

		return (1);
	}

	borg_note("# Genociding Hounds at Start of DLevel");

	if (borg_spell(REALM_DEATH, 1, 6) ||
		borg_activate(BORG_ACT_GENOCIDE) ||
		borg_use_staff(SV_STAFF_GENOCIDE))
	{
		/* and the winner is..... */
		borg_keypress(genocide_target);

		/* Remove this race from the borg_kill */
		for (i = 0; i < borg_kills_nxt; i++)
		{
			monster_race *r_ptr;

			/* Monster */
			r_ptr = &r_info[borg_kills[i].r_idx];

			/* Our char of the monster */
			if (r_ptr->d_char != genocide_target) continue;

			/* remove this monster */
			borg_delete_kill(i, "genocided");
		}

		return (1);
	}

	/* default to can't do it. */
	return (0);
}

/* Earthquake, priest and mage spells. */
static int borg_defend_aux_earthquake(int p1)
{
	int p2 = 0;
	int door_bad = 0;
	int door_x, door_y, x, y;

	map_block *mb_ptr;

	if (borg_simulate)
	{
		if (!borg_spell_okay_fail(REALM_NATURE, 3, 0, 35) &&
			!borg_equips_staff_fail(SV_STAFF_EARTHQUAKES)) return (0);

		/* See if he is in real danger or fighting summoner */
		if (p1 < avoidance) return (0);

		/* Do not cast if surounded by doors or something */
		/* Get grid */
		for (door_x = -1; door_x <= 1; door_x++)
		{
			for (door_y = -1; door_y <= 1; door_y++)
			{
				/* Acquire location */
				x = door_x + c_x;
				y = door_y + c_y;

				/* Bounds checking */
				if (!map_in_bounds(x, y)) continue;

				mb_ptr = map_loc(x, y);

				/* track spaces already protected */
				if ( /*(mb_ptr->feat == FEAT_GLYPH) || */
					(mb_ptr->feat >= FEAT_CLOSED &&
						mb_ptr->feat <= FEAT_PERM_SOLID) ||
					mb_ptr->object ||
					mb_ptr->feat == FEAT_LESS ||
					mb_ptr->feat == FEAT_MORE ||
					mb_ptr->feat == FEAT_OPEN ||
					mb_ptr->feat == FEAT_BROKEN ||
					mb_ptr->monster)
				{
					door_bad++;
				}
			}
		}


		/* If there are too many bad spots don't bother */
		if (door_bad >= 6) return (0);

		/* What effect is there? */
		borg_create_door = TRUE;
		p2 = borg_danger(c_x, c_y, 1, TRUE);
		borg_create_door = FALSE;

		if (p1 > p2 &&
			p2 <= (borg_fighting_unique ? ((avoidance * 2) / 3) : (avoidance / 2))
			&& p1 > (avoidance / 5))
		{
			/* Simulation */
			return (p2);
		}

		/* I guess not */
		return (0);
	}

	/* Cast the spell */
	return (borg_spell(REALM_NATURE, 3, 0) ||
			borg_use_staff(SV_STAFF_EARTHQUAKES));
}

/* Word of Destruction, priest and mage spells.  Death is right around the
 *  corner, so kill everything.
 */
static int borg_defend_aux_destruction(int p1)
{
	if (borg_simulate)
	{
		/* Borg_defend() is called before borg_escape().  He may have some
		 * easy ways to escape (teleport scroll) but he may attempt this spell
		 * instead of using the scrolls.
		 */
		/* Use teleport scrolls instead of WoD */
		if (bp_ptr->able.escape && !bp_ptr->status.blind &&
			!bp_ptr->status.confused) return (0);

		if (!borg_spell_okay_fail(REALM_CHAOS, 1, 6, 55) &&
			!borg_read_scroll_fail(SV_SCROLL_STAR_DESTRUCTION) &&
			!borg_equips_staff_fail(SV_STAFF_DESTRUCTION) &&
			!(p1 > (avoidance * 4) && borg_equips_staff(SV_STAFF_DESTRUCTION)))
			return (0);

		/* See if he is in real danger */
		if (p1 < avoidance * 2) return (0);

		/* Try not to cast this against uniques */
		/* Don't cast it on a quest level */
		if ((borg_fighting_unique && p1 < avoidance * 5) ||
			borg_fighting_unique >= BORG_QUESTOR) return (0);

		/* Simulation */
		return (p1);
	}

	/* Cast the spell */
	return (borg_spell(REALM_CHAOS, 1, 6) ||
		borg_use_staff(SV_STAFF_DESTRUCTION) ||
		borg_read_scroll(SV_SCROLL_STAR_DESTRUCTION));
}

static int borg_defend_aux_banishment(int p1)
{
	int p2 = 1;
	int fail_allowed = 15;
	int i;

	if (borg_simulate)
	{
		/* Only tell away if scared */
		if (p1 < avoidance * 12 / 10) return (0);

		/* if very scary, do not allow for much chance of fail */
		if (p1 > avoidance * 4) fail_allowed -= 10;

		if (!borg_spell_okay_fail(REALM_LIFE, 2, 5, fail_allowed) &&
			!borg_spell_okay_fail(REALM_TRUMP, 1, 7, fail_allowed)) return (0);

		/* reset initial danger */
		p1 = 1;

		/* Two passes to determine exact danger */
		for (i = 0; i < borg_beam_n; i++)
		{
			int x = borg_beam_x[i];
			int y = borg_beam_y[i];

			/* Make sure to be on the map */
			if (!map_in_bounds(x, y)) continue;

			/* Calculate danger of who is left over */
			borg_full_damage = TRUE;
			p1 += borg_danger_aux(c_x, c_y, 1, map_loc(x, y)->kill, TRUE);
			borg_full_damage = FALSE;
		}

		/* Pass two -- Find a monster and calculate its danger */
		for (i = 0; i < borg_beam_n; i++)
		{
			int x = borg_beam_x[i];
			int y = borg_beam_y[i];
			int idx;

			monster_race *r_ptr;

			/* Make sure to be on the map */
			if (!map_in_bounds(x, y)) continue;

			/* Which monster is this in the kill_list. */
			idx = map_loc(x, y)->kill;

			/* Get the monster */
			r_ptr = &r_info[borg_kills[idx].r_idx];

			/* Get rid of evil monsters */
			if (FLAG(r_ptr, RF_EVIL)) continue;

			/* Calculate danger of who is left over */
			borg_full_damage = TRUE;
			p2 += borg_danger_aux(c_x, c_y, 1, idx, TRUE);
			borg_full_damage = FALSE;
		}

		/* no negatives */
		p2 = MAX(p2, 0);

		/* Try not to cast this against Serpent/Oberon */
		if (borg_fighting_unique >= BORG_QUESTOR &&
			((bp_ptr->chp > 250 && bp_ptr->depth == 99) ||
			(bp_ptr->chp > 350 && bp_ptr->depth == 100))) p2 = 9999;

		/* check to see if I am left better off */
		if (p1 > p2 &&
			p2 <= (borg_fighting_unique ? ((avoidance * 2) / 3) : (avoidance / 2))
			&& p1 > (avoidance * 2))
		{
			/* Simulation */
			return (p2);
		}

		/* I guess not */
		return (0);
	}

	/* Cast the spell */
	return (borg_spell_fail(REALM_LIFE, 2, 5, fail_allowed) ||
		borg_spell_fail(REALM_TRUMP, 1, 7, fail_allowed));
}



/*
 * Detect Inviso/Monsters
 * Used only if I am hit by an unseen guy.
 * Casts detect invis.
 */
static int borg_defend_aux_inviso(int p1)
{
	int fail_allowed = 40;

	if (borg_simulate)
	{
		/* Can the borg see invis already? */
		if (borg_see_inv || FLAG(bp_ptr, TR_SEE_INVIS)) return (0);

		/* not recent */
		if (borg_t > need_see_inviso + 5) return (0);

		/* too dangerous to cast */
		if (p1 > avoidance * 7) return (0);

		/* Do I have anything that will work? */
		if (!borg_slot(TV_POTION, SV_POTION_DETECT_INVIS) &&
			!borg_read_scroll_fail(SV_SCROLL_DETECT_INVIS) &&
			!borg_equips_staff_fail(SV_STAFF_DETECT_INVIS) &&
			!borg_activate_fail(BORG_ACT_DETECT_EVIL) &&
			!borg_equips_staff_fail(SV_STAFF_DETECT_EVIL) &&
			!borg_spell_okay_fail(REALM_LIFE, 1, 3, fail_allowed) &&
			!borg_spell_okay_fail(REALM_ARCANE, 0, 2, fail_allowed))
			return (0);

		/* No real value known, but lets cast it to find the bad guys. */
		return (10);
	}

	/* smoke em if you got em */
	/* short time */
	if (borg_quaff_potion(SV_POTION_DETECT_INVIS))
	{
		borg_see_inv = 18000;
		return (10);
	}
	/* long time */
	if (borg_spell(REALM_LIFE, 1, 3) ||
		borg_spell(REALM_ARCANE, 0, 2))
	{
		borg_see_inv = 20000;
		return (10);
	}
	/* snap shot */
	if (borg_activate(BORG_ACT_DETECT_EVIL) ||
		borg_read_scroll(SV_SCROLL_DETECT_INVIS) ||
		borg_use_staff(SV_STAFF_DETECT_INVIS) ||
		borg_use_staff(SV_STAFF_DETECT_EVIL))
	{
		borg_see_inv = 3000;	/* hack, actually a snap shot, no ignition message */
		return (10);
	}

	borg_oops("How did this happen?");

	/* ah crap, I guess I wont be able to see them */
	return (0);

}

/*
 * Use temp esp.
 * Used only if the borg is hit by an unseen guy.
 */
static int borg_defend_aux_esp(int p1)
{
	int fail_allowed = 40;

	if (borg_simulate)
	{
		/* Has the borg esp already? */
		if (borg_esp || FLAG(bp_ptr, TR_TELEPATHY)) return (0);

		/* not recent */
		if (borg_t > need_see_inviso + 5) return (0);

		/* too dangerous to cast */
		if (p1 > avoidance * 7) return (0);

		/* Do I have anything that will work? */
		if (!borg_spell_okay_fail(REALM_SORCERY, 2, 4, fail_allowed) &&
			!borg_spell_okay_fail(REALM_ARCANE, 3, 7, fail_allowed) &&
			!borg_activate_fail(BORG_ACT_TELEPATHY) &&
			!borg_mindcr_okay_fail(MIND_PRECOGNIT, 24, fail_allowed))
			return (0);

		/* No real value known, but lets cast it to find the bad guys. */
		return (10);
	}

	/* long time */
	return (borg_activate(BORG_ACT_TELEPATHY) ||
		borg_spell(REALM_SORCERY, 2, 4) ||
		borg_spell(REALM_ARCANE, 3, 7) ||
		borg_mindcr(MIND_PRECOGNIT, 24));
}

/*
 * Light Beam to spot lurkers
 * Used only if I am hit by an unseen guy.
 * Lights up a hallway.
 */
static int borg_defend_aux_lbeam(int *key)
{
	bool hallway = FALSE;
	int x = c_x;
	int y = c_y;

	if (borg_simulate)
	{
		/* no need */
		if (bp_ptr->status.blind) return (0);

		/* Light Beam section to spot non seen guys */
		/* not recent, dont bother */
		if (borg_t > (need_see_inviso + 2)) return (0);

		/* Check to see if I am in a hallway */
		/* Case 1a: north-south corridor */
		if (borg_cave_floor_bold(y - 1, x) &&
			borg_cave_floor_bold(y + 1, x) &&
			!borg_cave_floor_bold(y, x - 1) &&
			!borg_cave_floor_bold(y, x + 1) &&
			!borg_cave_floor_bold(y + 1, x - 1) &&
			!borg_cave_floor_bold(y + 1, x + 1) &&
			!borg_cave_floor_bold(y - 1, x - 1) &&
			!borg_cave_floor_bold(y - 1, x + 1))
		{
			/* ok to light up */
			hallway = TRUE;
		}

		/* Case 1b: east-west corridor */
		if (borg_cave_floor_bold(y, x - 1) &&
			borg_cave_floor_bold(y, x + 1) &&
			!borg_cave_floor_bold(y - 1, x) &&
			!borg_cave_floor_bold(y + 1, x) &&
			!borg_cave_floor_bold(y + 1, x - 1) &&
			!borg_cave_floor_bold(y + 1, x + 1) &&
			!borg_cave_floor_bold(y - 1, x - 1) &&
			!borg_cave_floor_bold(y - 1, x + 1))
		{
			/* ok to light up */
			hallway = TRUE;
		}

		/* Case 1aa: north-south doorway */
		if (borg_cave_floor_bold(y - 1, x) &&
			borg_cave_floor_bold(y + 1, x) &&
			!borg_cave_floor_bold(y, x - 1) &&
			!borg_cave_floor_bold(y, x + 1))
		{
			/* ok to light up */
			hallway = TRUE;
		}

		/* Case 1ba: east-west doorway */
		if (borg_cave_floor_bold(y, x - 1) &&
			borg_cave_floor_bold(y, x + 1) &&
			!borg_cave_floor_bold(y - 1, x) &&
			!borg_cave_floor_bold(y + 1, x))
		{
			/* ok to light up */
			hallway = TRUE;
		}

		/* not in a hallway */
		if (!hallway) return (0);

		/* Make sure I am not in too much danger */
		/* if (borg_simulate && p1 > avoidance * 3 / 4) return (0); */

		/* test the beam function */
		if (borg_lite_beam(borg_simulate, key)) return (10);

		/* Never mind */
		return (0);
	}

	/* if in a hallway call the Light Beam routine */
	return (borg_lite_beam(borg_simulate, key));
}


/*
 * Phantasmal Servant.
 * If I dont have one, then get one.
 */
static int borg_defend_aux_servant(int p1)
{
	int fail_allowed = 15;
	int i;
	int friendlies = 0;

	if (borg_simulate)
	{
		/* must have the ability */
		if (!borg_spell_okay_fail(REALM_TRUMP, 1, 2, fail_allowed)) return (0);

		/* reset initial danger */
		p1 = 1;

		/* Two passes to determine exact danger */
		for (i = 0; i < borg_kills_nxt; i++)
		{
			borg_kill *kill;

			/* Monster */
			kill = &borg_kills[i];

			/* Skip dead monsters */
			if (!kill->r_idx) continue;

			/* Skip non Friendly */
			if (!(kill->m_flags & MONST_PET)) continue;

			/* Count Friendly */
			friendlies++;
		}

		/* check to see if I am left better off */
		if (friendlies < 5 && p1 < (avoidance / 3))
		{
			/* Simulation */
			return (5);
		}

		/* I guess not */
		return (0);
	}

	/* Cast the spell */
	return (borg_spell(REALM_TRUMP, 1, 2));
}


/*
 * Simulate/Apply the optimal result of using the given "type" of defence
 * p1 is the current danger level (passed in for effiency)
 */
static int borg_defend_aux(int what, int p1, int *key)
{
	/* Analyze */
	switch (what)
	{
		case BD_SPEED:
		{
			return (borg_defend_aux_speed(p1));
		}
		case BD_PROT_FROM_EVIL:
		{
			return (borg_defend_aux_prot_evil(p1));
		}
		case BD_BLESS:
		{
			return (borg_defend_aux_bless(p1));
		}
		case BD_HERO_BERSERK:
		{
			return (borg_defend_aux_hero(p1));
		}
		case BD_RESIST_FCE:
		{
			return (borg_defend_aux_resist_fce(p1));
		}
		case BD_RESIST_FECAP:
		{
			return (borg_defend_aux_resist_fecap(p1));
		}
		case BD_RESIST_F:
		{
			return (borg_defend_aux_resist_f(p1));
		}
		case BD_RESIST_C:
		{
			return (borg_defend_aux_resist_c(p1));
		}
		case BD_RESIST_A:
		{
			return (borg_defend_aux_resist_a(p1));
		}
		case BD_RESIST_P:
		{
			return (borg_defend_aux_resist_p(p1));
		}
		case BD_SHIELD:
		{
			return (borg_defend_aux_shield(p1));
		}
		case BD_GOI:
		{
			return (borg_defend_aux_goi(p1));
		}
		case BD_GOI_POT:
		{
			return (borg_defend_aux_goi_pot(p1));
		}
		case BD_GLYPH:
		{
			return (borg_defend_aux_glyph(p1));
		}
		case BD_WARDING:
		{
			return (borg_defend_aux_true_warding(p1));
		}
		case BD_TELL_AWAY:
		{
			return (borg_defend_aux_tell_away(p1));
		}
		case BD_CREATE_WALLS:
		{
			return (borg_defend_aux_create_walls(p1));
		}
		case BD_MASS_GENOCIDE:
		{
			return (borg_defend_aux_mass_genocide(p1));
		}
		case BD_GENOCIDE:
		{
			return (borg_defend_aux_genocide(p1, key));
		}
		case BD_GENOCIDE_HOUNDS:
		{
			return (borg_defend_aux_genocide_hounds(p1));
		}
		case BD_EARTHQUAKE:
		{
			return (borg_defend_aux_earthquake(p1));
		}
		case BD_DESTRUCTION:
		{
			return (borg_defend_aux_destruction(p1));
		}
		case BD_BANISHMENT:
		{
			return (borg_defend_aux_banishment(p1));
		}
		case BD_DETECT_INVISO:
		{
			return (borg_defend_aux_inviso(p1));
		}
		case BD_TELEPATHY:
		{
			return (borg_defend_aux_esp(p1));
		}
		case BD_LIGHT_BEAM:
		{
			return (borg_defend_aux_lbeam(key));
		}
		case BD_TRUMP_SERVANT:
		{
			return (borg_defend_aux_servant(p1));
		}
	}

	borg_oops("# Trying invalid BD type. (%d)", what);

	return (0);
}

static bool borg_refresh_goi(void)
{
	int p;
	map_block *mb_ptr = map_loc(c_x, c_y);

	/* if you have a globe up and it is about to drop, */
	if (borg_goi && borg_goi < (borg_game_ratio * 2))
	{
		/* check 'true' danger. This will make sure we do not */
		/* refresh our GOI if no-one is around */
		borg_attacking = TRUE;
		p = borg_danger(c_x, c_y, 1, TRUE);
		borg_attacking = FALSE;

		/* Is it a good idea to keep up the Globe? */
		if ((p > mb_ptr->fear) || borg_fighting_unique)
		{
			/* If you can cast the spell */
			if (borg_spell(REALM_SORCERY, 3, 7) ||
				borg_spell(REALM_LIFE, 3, 7))
			{
				/* Make a note */
				borg_note("# refreshing GOI.  borg_goi = %d", borg_goi);
				borg_note("# p_ptr->invuln = %d, (ratio = %d)",
					p_ptr->tim.invuln, borg_game_ratio);

				/* Declare success */
				return (TRUE);
			}
		}
	}

	/* No Globe */
	return (FALSE);
}


/*
 * prepare to attack... this is setup for a battle.
 */
bool borg_defend(int p1)
{
	int n, b_n = 0;
	int key, b_key;
	int g, b_g = -1;

	/* Simulate */
	borg_simulate = TRUE;

	/* refresh Globe of Invulnerablity (if you can) */
	if (borg_refresh_goi()) return (TRUE);

	/* Make sure you have the monsters lined correctly */
	borg_temp_fill();

	/* Analyze the possible setup moves */
	for (g = 0; g < BD_MAX; g++)
	{
		/* Simulate */
		n = borg_defend_aux(g, p1, &key);

		/* Track "best" attack */
		if (n <= b_n) continue;

		/* Track best */
		b_g = g;
		b_n = n;
		b_key = key;
	}

	/* Nothing good */
	if (b_n <= 0)
	{
		return (FALSE);
	}

	/* Note */
	borg_note("# Performing defence type %d with value %d", b_g, b_n);

	/* Instantiate */
	borg_simulate = FALSE;

	/* Instantiate */
	(void)borg_defend_aux(b_g, p1, &b_key);

	/* Success */
	return (TRUE);

}

/*
 * Perma spells.  Some are cool to have on all the time, so long as their
 * mana cost is not too much.
 * There are several types of setup moves:
 *
 *   Temporary speed
 *   Protect From Evil
 *   Prayer
 *   Temp Resist (either all or just cold/fire?)
 *   Shield
 *
 */
enum
{
	BP_SPEED,
	BP_PROT_FROM_EVIL,
	BP_BLESS,
	BP_TELEPATHY,
	BP_SEE_INVIS,

	BP_RESIST_ALL,
	BP_RESIST_F,
	BP_RESIST_C,
	BP_RESIST_A,
	BP_RESIST_E,
	BP_RESIST_P,
	BP_RESIST_FCE,

	BP_GOI,
	BP_SHIELD,
	BP_HERO_BERSERK,
	BP_BERSERK_POTION,

	BP_GLYPH,

	BP_MAX
};

/*
 * Bless/Prayer to prepare for battle
 */
static int borg_perma_aux_bless(void)
{
	int fail_allowed = 5, cost;

	if (borg_simulate)
	{
		/* increase the threshold */
		if (unique_on_level) fail_allowed = 10;
		if (borg_fighting_unique) fail_allowed = 15;

		/* already blessed */
		if (borg_bless) return (0);

		/* Is the bless activation available? */
		if (borg_activate_fail(BORG_ACT_BLESS))
		{
			cost = 0;
		}
		/* Is the life prayer spell available? */
		else if (borg_spell_okay_fail(REALM_LIFE, 3, 1, fail_allowed))
		{
			/* Obtain the cost of the spell */
			cost = borg_spell_mana(REALM_LIFE, 3, 1);
		}
		/* Is the life bless spell available? */
		else if (borg_spell_okay_fail(REALM_LIFE, 0, 2, fail_allowed))
		{
			/* Obtain the cost of the spell */
			cost = borg_spell_mana(REALM_LIFE, 0, 2);
		}
		else
		{
			/* No bless available */
			return (0);
		}

		/* If its cheap, go ahead */
		if (cost >= bp_ptr->csp / (unique_on_level ? 7 : 10)) return (0);

		/* Simulation */
		/* bless is a low priority */
		return (1);
	}

	/* do it! */
	return (borg_spell(REALM_LIFE, 3, 1) ||
			borg_spell(REALM_LIFE, 0, 2));
}

/* all resists */
static int borg_perma_aux_resist(void)
{
	int cost = 0;
	int fail_allowed = 5;

	if (borg_simulate)
	{
		/* increase the threshold */
		if (unique_on_level) fail_allowed = 10;
		if (borg_fighting_unique) fail_allowed = 15;

		if (my_oppose_fire + my_oppose_acid + my_oppose_pois +
			my_oppose_elec + my_oppose_cold >= 3)
			return (0);

		/* Not needed if GOI is on */
		if (borg_goi) return (0);

		if (borg_spell_okay_fail(REALM_NATURE, 2, 3, fail_allowed))
		{
			/* Obtain the cost of the spell */
			cost = borg_spell_mana(REALM_NATURE, 2, 3);
		}
		else if (borg_mindcr_okay_fail(MIND_CHAR_ARMOUR, 35, fail_allowed))
		{
			/* Obtain the cost of the spell */
			cost = borg_minds[MIND_CHAR_ARMOUR].power;
		}
		else
		{
			/* No resistance available */
			return (0);
		}

		/* If its cheap, go ahead */
		if (cost >= bp_ptr->csp / (unique_on_level ? 7 : 10)) return (0);

		/* Simulation */
		return (2);
	}

	/* do it! */
	return (borg_spell(REALM_NATURE, 2, 3) ||
		borg_mindcr(MIND_CHAR_ARMOUR, 35));
}


/* resists--- Only bother if a Unique is on the level.*/
static int borg_perma_aux_resist_f(void)
{
	int cost = 0;
	int fail_allowed = 5;

	if (borg_simulate)
	{
		/* increase the threshold */
		if (unique_on_level) fail_allowed = 10;
		if (borg_fighting_unique) fail_allowed = 15;

		if (my_oppose_fire || !unique_on_level) return (0);

		/* Not needed if GOI is on */
		if (borg_goi) return (0);

		/* No need if the borg is immune to fire */
		if (FLAG(bp_ptr, TR_IM_FIRE)) return (0);

		/* Is the spell available? */
		if (borg_spell_okay_fail(REALM_ARCANE, 1, 6, fail_allowed))
		{
			/* Obtain the cost of the spell */
			cost = borg_spell_mana(REALM_ARCANE, 1, 6);
		}
		else if (borg_mindcr_okay_fail(MIND_CHAR_ARMOUR, 20, fail_allowed))
		{
			/* Obtain the cost of the spell */
			cost = borg_minds[MIND_CHAR_ARMOUR].power;
		}
		else
		{
			/* No resistance to fire available */
			return (0);
		}

		/* If its cheap, go ahead */
		if (cost >= bp_ptr->csp / 20) return (0);

		/* Simulation */
		return (1);
	}

	/* do it! */
	return (borg_spell(REALM_ARCANE, 1, 6) ||
		borg_mindcr(MIND_CHAR_ARMOUR, 20));
}

/* resists--- Only bother if a Unique is on the level.*/
static int borg_perma_aux_resist_c(void)
{
	int cost = 0;
	int fail_allowed = 5;

	if (borg_simulate)
	{
		/* increase the threshold */
		if (unique_on_level) fail_allowed = 10;
		if (borg_fighting_unique) fail_allowed = 15;

		if (my_oppose_cold || !unique_on_level) return (0);

		/* No need if the borg is immune to Cold */
		if (FLAG(bp_ptr, TR_IM_COLD)) return (0);

		/* Not needed if GOI is on */
		if (borg_goi) return (0);

		if (borg_spell_okay_fail(REALM_ARCANE, 1, 7, fail_allowed))
		{
			/* Obtain the cost of the spell */
			cost = borg_spell_mana(REALM_ARCANE, 1, 7);
		}
		else if (borg_mindcr_okay_fail(MIND_CHAR_ARMOUR, 25, fail_allowed))
		{
			/* Obtain the cost of the spell */
			cost = borg_minds[MIND_CHAR_ARMOUR].power;
		}
		else
		{
			/* No resistance to cold available */
			return (0);
		}

		/* If its cheap, go ahead */
		if (cost >= bp_ptr->csp / 20) return (0);

		/* Simulation */
		return (1);
	}

	/* do it! */
	return (borg_spell(REALM_ARCANE, 1, 7) ||
		borg_mindcr(MIND_CHAR_ARMOUR, 25));
}

/* resists--- Only bother if a Unique is on the level.*/
static int borg_perma_aux_resist_a(void)
{
	int cost = 0;
	int fail_allowed = 5;

	if (borg_simulate)
	{
		/* increase the threshold */
		if (unique_on_level) fail_allowed = 10;
		if (borg_fighting_unique) fail_allowed = 15;

		if (my_oppose_acid || !unique_on_level) return (0);

		/* No need if the borg is immune to Acid */
		if (FLAG(bp_ptr, TR_IM_ACID)) return (0);

		/* Not needed if GOI is on */
		if (borg_goi) return (0);

		if (borg_spell_okay_fail(REALM_ARCANE, 2, 1, fail_allowed))
		{
			/* Obtain the cost of the spell */
			cost = borg_spell_mana(REALM_ARCANE, 2, 1);
		}
		else if (borg_mindcr_okay_fail(MIND_CHAR_ARMOUR, 15, fail_allowed))
		{
			/* Obtain the cost of the spell */
			cost = borg_minds[MIND_CHAR_ARMOUR].power;
		}
		else
		{
			/* No resistance to acid available */
			return (0);
		}

		/* If its cheap, go ahead */
		if (cost >= bp_ptr->csp / 20) return (0);

		/* Simulation */
		return (1);
	}

	/* do it! */
	return (borg_spell(REALM_ARCANE, 2, 1) ||
		borg_mindcr(MIND_CHAR_ARMOUR, 15));
}

/* resists--- Only bother if a Unique is on the level.*/
static int borg_perma_aux_resist_e(void)
{
	int cost = 0;
	int fail_allowed = 5;

	if (borg_simulate)
	{
		/* increase the threshold */
		if (unique_on_level) fail_allowed = 10;
		if (borg_fighting_unique) fail_allowed = 15;

		if (my_oppose_elec || !unique_on_level) return (0);

		/* No need if the borg is immune to Acid */
		if (FLAG(bp_ptr, TR_IM_ACID)) return (0);

		/* Not needed if GOI is on */
		if (borg_goi) return (0);

		if (borg_spell_okay_fail(REALM_ARCANE, 2, 0, fail_allowed))
		{
			/* Obtain the cost of the spell */
			cost = borg_spell_mana(REALM_ARCANE, 2, 0);
		}
		else if (borg_mindcr_okay_fail(MIND_CHAR_ARMOUR, 30, fail_allowed))
		{
			/* Obtain the cost of the spell */
			cost = borg_minds[MIND_CHAR_ARMOUR].power;
		}
		else
		{
			/* No resistance to elec available */
			return (0);
		}

		/* If its cheap, go ahead */
		if (cost >= bp_ptr->csp / 20) return (0);

		/* Simulation */
		return (1);
	}

	/* do it! */
	return (borg_spell(REALM_ARCANE, 2, 1) ||
		borg_mindcr(MIND_CHAR_ARMOUR, 30));
}

/* resists--- Only bother if a Unique is on the level.*/
static int borg_perma_aux_resist_p(void)
{
	int cost = 0;
	int fail_allowed = 5;

	if (borg_simulate)
	{
		/* increase the threshold */
		if (unique_on_level) fail_allowed = 10;
		if (borg_fighting_unique) fail_allowed = 15;

		if (my_oppose_pois || !unique_on_level) return (0);

		/* Not needed if GOI is on */
		if (borg_goi) return (0);

		/* No need if the borg is immune to Poison */
		if (FLAG(bp_ptr, TR_IM_POIS)) return (0);

		if (!borg_spell_okay_fail(REALM_DEATH, 0, 5, fail_allowed)) return (0);

		/* Obtain the cost of the spell */
		cost = borg_spell_mana(REALM_DEATH, 0, 5);

		/* If its cheap, go ahead */
		if (cost >= bp_ptr->csp / 20) return (0);

		/* Simulation */
		return (1);
	}

	/* do it! */
	return (borg_spell(REALM_DEATH, 0, 5));
}

/* resist fire and cold for priests */
static int borg_perma_aux_resist_fce(void)
{
	int cost = 0;
	int fail_allowed = 5;

	if (borg_simulate)
	{
		/* increase the threshold */
		if (unique_on_level) fail_allowed = 10;
		if (borg_fighting_unique) fail_allowed = 15;

		/* cast if one drops and unique is near */
		if (borg_fighting_unique &&
			(my_oppose_fire || FLAG(bp_ptr, TR_IM_FIRE)) &&
			(my_oppose_elec || FLAG(bp_ptr, TR_IM_ELEC)) &&
			(my_oppose_cold || FLAG(bp_ptr, TR_IM_COLD))) return (0);

		/* cast if both drop and no unique is near */
		if (!borg_fighting_unique && (my_oppose_fire || my_oppose_cold)) return (0);

		/* no need if immune */
		if (FLAG(bp_ptr, TR_IM_FIRE) &&
			FLAG(bp_ptr, TR_IM_COLD) &&
			FLAG(bp_ptr, TR_IM_ELEC)) return (0);

		/* Not needed if GOI is on */
		if (borg_goi) return (0);

		if (!borg_spell_okay_fail(REALM_NATURE, 0, 6, fail_allowed)) return (0);

		/* Obtain the cost of the spell */
		cost = borg_spell_mana(REALM_NATURE, 0, 6);

		/* If its cheap, go ahead */
		if (cost >= bp_ptr->csp / (unique_on_level ? 7 : 10)) return (0);

		/* Simulation */
		return (2);
	}

	/* do it! */
	return (borg_spell(REALM_NATURE, 0, 6));
}


/*
 * Speed to prepare for battle
 */
static int borg_perma_aux_speed(void)
{
	int fail_allowed = 7;
	int cost;

	if (borg_simulate)
	{
		/* increase the threshold */
		if (unique_on_level) fail_allowed = 10;
		if (borg_fighting_unique) fail_allowed = 15;

		/* already fast */
		if (borg_speed) return (0);

		/* Is the sorcery speed spell available? */
		if (borg_spell_okay_fail(REALM_SORCERY, 1, 5, fail_allowed))
		{
			/* Obtain the cost of the spell */
			cost = borg_spell_mana(REALM_SORCERY, 1, 5);
		}
		/* Is the death speed spell available? */
		else if (borg_spell_okay_fail(REALM_DEATH, 2, 3, fail_allowed))
		{
			/* Obtain the cost of the spell */
			cost = borg_spell_mana(REALM_DEATH, 2, 3);
		}
		/* Is the mindcrafter speed spell available? */
		else if (borg_mindcr_okay_fail(MIND_ADRENALINE, 23, fail_allowed))
		{
			/* Obtain the cost of the spell */
			cost = borg_minds[MIND_ADRENALINE].power;
		}
		else
		{
			/* speed spell not available */
			return (0);
		}

		/* If its cheap, go ahead */
		if (cost >= bp_ptr->csp / (unique_on_level ? 7 : 10)) return (0);

		/* Simulation */
		return (5);
	}

	/* do it! */
	return (borg_spell(REALM_SORCERY, 1, 5) ||
		borg_mindcr(MIND_ADRENALINE, 23) ||
		borg_spell(REALM_DEATH, 2, 3));
}


static int borg_perma_aux_goi(void)
{
	int fail_allowed = 5;
	int cost;

	if (borg_simulate)
	{
		/* increase the threshold */
		if (unique_on_level) fail_allowed = 10;
		if (borg_fighting_unique) fail_allowed = 15;

		/* if already protected */
		if (borg_shield || borg_goi) return (0);

		/* only when a unique is near */
		if (!unique_on_level) return (0);

		/* Is the Life GoI spell available? */
		if (borg_spell_okay_fail(REALM_LIFE, 3, 7, fail_allowed))
		{
			/* Obtain the cost of the spell */
			cost = borg_spell_mana(REALM_LIFE, 3, 7);
		}
		/* Is the Death GoI spell available? */
		else if (borg_spell_okay_fail(REALM_SORCERY, 3, 7, fail_allowed))
		{
			/* Obtain the cost of the spell */
			cost = borg_spell_mana(REALM_SORCERY, 3, 7);
		}
		else
		{
			/* No GoI available */
			return (0);
		}

		/* If its cheap, go ahead */
		if (cost >= bp_ptr->csp / (unique_on_level ? 7 : 10)) return (0);

		/* Simulation */
		return (3);
	}

	/* do it! */
	return (borg_spell_fail(REALM_SORCERY, 3, 7, fail_allowed) ||
		borg_spell_fail(REALM_LIFE, 3, 7, fail_allowed));
}

/*
 * Telepathy
 */
static int borg_perma_aux_telepathy(void)
{
	int fail_allowed = 5, cost;

	if (borg_simulate)
	{
		/* increase the threshold */
		if (unique_on_level) fail_allowed = 10;
		if (borg_fighting_unique) fail_allowed = 15;

		/* already telepathic */
		if (borg_esp || (FLAG(bp_ptr, TR_TELEPATHY))) return (0);

		/* ESP from an artifact is for free */
		if (borg_activate_fail(BORG_ACT_TELEPATHY))
		{
			cost = 0;
		}
		/* Is the Arcane telepathy spell available? */
		else if (borg_spell_okay_fail(REALM_ARCANE, 3, 7, fail_allowed))
		{
			/* Obtain the cost of the spell */
			cost = borg_spell_mana(REALM_ARCANE, 3, 7);
		}
		/* Is the Sorcery telepathy spell available? */
		else if (borg_spell_okay_fail(REALM_SORCERY, 2, 4, fail_allowed))
		{
			/* Obtain the cost of the spell */
			cost = borg_spell_mana(REALM_SORCERY, 2, 4);
		}
		else if (borg_mindcr_okay_fail(MIND_PRECOGNIT, 24, fail_allowed) &&
				bp_ptr->lev < 40)
		{
			/* Obtain the cost of the spell */
			cost = borg_minds[MIND_PRECOGNIT].power;
		}
		else
		{
			/* Telepathy, what is that? */
			return (0);
		}

		/* If its cheap, go ahead */
		if (cost >= bp_ptr->csp / (unique_on_level ? 7 : 10)) return (0);

		/* Simulation */
		return (1);
	}

	/* do it! */
	return (borg_activate(BORG_ACT_TELEPATHY) ||
		borg_spell(REALM_ARCANE, 3, 7) ||
		borg_spell(REALM_SORCERY, 2, 4) ||
		borg_mindcr(MIND_PRECOGNIT, 24));
}

/* See invisible */
static int borg_perma_aux_see_invis(void)
{
	int fail_allowed = 5, cost;

	if (borg_simulate)
	{
		/* increase the threshold */
		if (unique_on_level) fail_allowed = 10;
		if (borg_fighting_unique) fail_allowed = 15;

		/* already seeing invisible */
		if (borg_inviso || FLAG(bp_ptr, TR_SEE_INVIS)) return (0);

		/* Is the Arcane see invisible spell available? */
		if (borg_spell_okay_fail(REALM_ARCANE, 2, 7, fail_allowed))
		{
			/* Obtain the cost of the spell */
			cost = borg_spell_mana(REALM_ARCANE, 2, 7);
		}
		/* Is the Life see invisible spell available? */
		else if (borg_spell_okay_fail(REALM_LIFE, 1, 3, fail_allowed))
		{
			/* Obtain the cost of the spell */
			cost = borg_spell_mana(REALM_LIFE, 1, 3);
		}
		else
		{
			/* See invisible, what is that? */
			return (0);
		}

		/* If its cheap, go ahead */
		if (cost >= bp_ptr->csp / (unique_on_level ? 7 : 10)) return (0);

		/* Simulation */
		return (1);
	}

	/* do it! */
	return (borg_spell(REALM_ARCANE, 2, 7) ||
		borg_spell(REALM_LIFE, 1, 3));
}

/* Shield for high AC */
static int borg_perma_aux_shield(void)
{
	int fail_allowed = 5;
	int cost;

	if (borg_simulate)
	{
		/* increase the threshold */
		if (unique_on_level) fail_allowed = 10;
		if (borg_fighting_unique) fail_allowed = 15;

		/* if already protected */
		if (borg_shield || borg_goi) return (0);

		/* Is the nature shield spell available? */
		if (borg_spell_okay_fail(REALM_NATURE, 2, 2, fail_allowed))
		{
			/* Obtain the cost of the spell */
			cost = borg_spell_mana(REALM_NATURE, 2, 2);
		}
		/* Is the mindcrafter shield spell available? */
		else if (borg_mindcr_okay_fail(MIND_CHAR_ARMOUR, 13, fail_allowed))
		{
			/* Obtain the cost of the spell */
			cost = borg_minds[MIND_CHAR_ARMOUR].power;
		}
		else
		{
			/* No shield available */
			return (0);
		}

		/* If its cheap, go ahead */
		if (cost >= bp_ptr->csp / (unique_on_level ? 7 : 10)) return (0);

		/* Simulation */
		return (2);
	}

	/* do it! */
	return (borg_spell(REALM_NATURE, 2, 2) ||
		borg_mindcr(MIND_CHAR_ARMOUR, 13));
}

static int borg_perma_aux_prot_evil(void)
{
	int cost = 0;
	int fail_allowed = 5;

	if (borg_simulate)
	{
		/* if already protected */
		if (borg_prot_from_evil || FLAG(bp_ptr, TR_SLAY_EVIL)) return (0);

		/* increase the threshold */
		if (unique_on_level) fail_allowed = 10;
		if (borg_fighting_unique) fail_allowed = 15;

		if (!borg_spell_okay_fail(REALM_LIFE, 1, 5, fail_allowed) &&
			!borg_activate_fail(BORG_ACT_PROT_EVIL)) return (0);

		/* Obtain the cost of the spell */
		cost = borg_spell_mana(REALM_LIFE, 1, 5);

		/* If its cheap, go ahead */
		if (cost >= bp_ptr->csp / (unique_on_level ? 7 : 10)) return (0);

		/* Simulation */
		return (3);
	}

	/* do it! */
	return (borg_activate(BORG_ACT_PROT_EVIL) ||
		borg_spell_fail(REALM_LIFE, 1, 5, fail_allowed));
}

/*
 * Hero/Berserk to prepare for battle
 */
static int borg_perma_aux_hero(void)
{
	int fail_allowed = 5;
	int priority = 2, cost;

	/* Is this for real */
	if (borg_simulate)
	{
		/* increase the threshold */
		if (unique_on_level) fail_allowed = 10;
		if (borg_fighting_unique) fail_allowed = 15;

		/* already heroed */
		if (borg_hero || borg_berserk) return (0);

		/* Is the hero or berserk activation spell available? */
		if (borg_activate_fail(BORG_ACT_HEROISM) ||
			borg_activate_fail(BORG_ACT_BERSERKER))
		{
			cost = 0;
		}
		/* Can the borg cast the death berserk spell? */
		else if (borg_spell_okay_fail(REALM_DEATH, 2, 0, fail_allowed))
		{
			/* Obtain the cost of the spell */
			cost = borg_spell_mana(REALM_DEATH, 2, 0);
		}
		/* Can the borg cast the mindcrafter adrenaline spell? */
		else if (borg_mindcr_okay_fail(MIND_ADRENALINE, 23, fail_allowed))
		{
			/* Obtain the cost of the spell */
			cost = borg_minds[MIND_ADRENALINE].power;
		}
		/* Can the borg cast the life hero spell? */
		else if (borg_spell_okay_fail(REALM_LIFE, 3, 0, fail_allowed))
		{
			/* Obtain the cost of the spell */
			cost = borg_spell_mana(REALM_LIFE, 3, 0);

			/* Reassign the importance */
			priority = 1;
		}
		else
		{
			/* No hero or berserk available */
			return (0);
		}

		/* If its cheap, go ahead */
		if (cost >= bp_ptr->csp / (unique_on_level ? 7 : 10)) return (0);

		/* hero/berserk has a low priority */
		return (priority);
	}

	/* Do it!  (We know one of these will succeed) */
	return (borg_activate(BORG_ACT_HEROISM) ||
		borg_activate(BORG_ACT_BERSERKER) ||
		borg_spell(REALM_DEATH, 2, 0) ||
		borg_mindcr(MIND_ADRENALINE, 23) ||
		borg_spell(REALM_LIFE, 3, 0));
}


/*
 * Berserk to prepare for battle
 */
static int borg_perma_aux_berserk_potion(void)
{
	if (borg_simulate)
	{
		/* Save the potions */
		if (!borg_fighting_unique) return (0);

		/* already blessed */
		if (borg_hero || borg_berserk)return (0);

		/* do I have any? */
		if (!borg_slot(TV_POTION, SV_POTION_BERSERK_STRENGTH) ||
			borg_mutation_check(MUT1_BERSERK, TRUE))
		{
			/* No dice */
			return (0);
		}

		/* Simulation */
		return (2);
	}

	/* do it! */
	return (borg_quaff_potion(SV_POTION_BERSERK_STRENGTH) ||
		borg_mutation(MUT1_BERSERK));
}


/* Glyph of Warding in a a-s corridor */
static int borg_perma_aux_glyph(void)
{
#if 0
	int i, wall_y, wall_x, wall_count = 0, y, x;
	int fail_allowed = 20;

	map_block *mb_ptr = map_loc(c_x, c_y);

	/* check to make sure a summoner is near */
	if (borg_kills_summoner == -1) return (0);

	/* make sure I have the spell */
	if (!borg_spell_okay_fail(REALM_LIFE, 1, 7, fail_allowed)) return (0);


	/* He should not cast it while on an object.
	 * I have addressed this inadequately in borg9.c when dealing with
	 * messages.  The message "the object resists" will delete the glyph
	 * from the array.  Then I set a broken door on that spot, the borg ignores
	 * broken doors, so he won't loop.
	 */
	if ((mb_ptr->object)		/*||
								   (mb_ptr->feat == FEAT_GLYPH) ||
								   ((mb_ptr->feat >= FEAT_TRAP_TRAPDOOR) && (mb_ptr->feat <= FEAT_TRAP_SLEEP)) */
		||
		(mb_ptr->feat == FEAT_CLOSED) || (mb_ptr->feat == FEAT_LESS) ||
		(mb_ptr->feat == FEAT_MORE) || (mb_ptr->feat == FEAT_OPEN) ||
		(mb_ptr->feat == FEAT_BROKEN))
	{
		return (0);
	}

	/* This spell is cast while he is digging and AS Corridor */
	/* Get grid */
	for (wall_x = -1; wall_x <= 1; wall_x++)
	{
		for (wall_y = -1; wall_y <= 1; wall_y++)
		{
			/* Acquire location */
			x = wall_x + c_x;
			y = wall_y + c_y;

			/* Bounds checking */
			if (!map_in_bounds(x, y)) continue;

			mb_ptr = map_loc(x, y);

			/* track adjacent walls */
			if (				/* (mb_ptr->feat == FEAT_GLYPH) || */
				   (mb_ptr->feat == FEAT_PILLAR) ||
				   ((mb_ptr->feat >= FEAT_MAGMA) &&
					(mb_ptr->feat <= FEAT_WALL_SOLID)))
			{
				wall_count++;
			}
		}
	}

	/* must be in a corridor */
	if (wall_count < 7) return (0);

	/* Simulation */
	if (borg_simulate) return (10);

	/* do it! */
	if (borg_spell_fail(REALM_LIFE, 1, 7, fail_allowed) ||
		borg_read_scroll(SV_SCROLL_RUNE_OF_PROTECTION))
	{
		/* Check for an existing glyph */
		for (i = 0; i < track_glyph_num; i++)
		{
			/* Stop if we already new about this glyph */
			if ((track_glyph_x[i] == c_x) &&
				(track_glyph_y[i] == c_y)) return (2);
		}

		/* Track the newly discovered glyph */
		if ((i == track_glyph_num) && (track_glyph_size))
		{
			borg_note("# Noting the creation of a corridor glyph.");
			track_glyph_num++;
			track_glyph_x[i] = c_x;
			track_glyph_y[i] = c_y;
		}
		return (2);
	}
#endif /* 0 */

	/* default to can't do it. */
	return (0);
}



/*
 * Simulate/Apply the optimal result of using the given "type" of set-up
 */
static int borg_perma_aux(int what)
{

	/* Analyze */
	switch (what)
	{
		case BP_SPEED:
		{
			return (borg_perma_aux_speed());
		}
		case BP_TELEPATHY:
		{
			return (borg_perma_aux_telepathy());
		}
		case BP_SEE_INVIS:
		{
			return (borg_perma_aux_see_invis());
		}

		case BP_PROT_FROM_EVIL:
		{
			return (borg_perma_aux_prot_evil());
		}
		case BP_RESIST_ALL:
		{
			return (borg_perma_aux_resist());
		}
		case BP_RESIST_F:
		{
			return (borg_perma_aux_resist_f());
		}
		case BP_RESIST_C:
		{
			return (borg_perma_aux_resist_c());
		}
		case BP_RESIST_A:
		{
			return (borg_perma_aux_resist_a());
		}
		case BP_RESIST_E:
		{
			return (borg_perma_aux_resist_e());
		}
		case BP_RESIST_P:
		{
			return (borg_perma_aux_resist_p());
		}
		case BP_RESIST_FCE:
		{
			return (borg_perma_aux_resist_fce());
		}
		case BP_BLESS:
		{
			return (borg_perma_aux_bless());
		}
		case BP_HERO_BERSERK:
		{
			return (borg_perma_aux_hero());
		}
		case BP_BERSERK_POTION:
		{
			return (borg_perma_aux_berserk_potion());
		}
		case BP_GOI:
		{
			return (borg_perma_aux_goi());
		}

		case BP_SHIELD:
		{
			return (borg_perma_aux_shield());
		}
		case BP_GLYPH:
		{
			return (borg_perma_aux_glyph());
		}
	}
	return (0);
}


/*
 * prepare to attack... this is setup for a battle.
 */
bool borg_perma_spell()
{
	int n, b_n = 0;
	int g, b_g = -1;


	/* Simulate */
	borg_simulate = TRUE;

	/* Not in town */
	if (!bp_ptr->depth) return (FALSE);

	/* No perma-spells until clevel 30 or the borg has to rest too much */
	if (bp_ptr->lev < 30) return (FALSE);

	/* Analyze the possible setup moves */
	for (g = 0; g < BP_MAX; g++)
	{
		/* Simulate */
		n = borg_perma_aux(g);

		/* Track "best" move */
		if (n <= b_n) continue;

		/* Track best */
		b_g = g;
		b_n = n;
	}

	/* Nothing good */
	if (b_n <= 0) return (FALSE);

	/* Note */
	borg_note("# Performing perma-spell type %d with value %d", b_g, b_n);

	/* Instantiate */
	borg_simulate = FALSE;

	/* Instantiate */
	(void)borg_perma_aux(b_g);

	/* Success */
	return (TRUE);
}

/*
 * check to make sure there are no monsters around
 * that should prevent resting also make sure the ground
 * is safe for us.
 */
bool borg_check_rest(void)
{
	int i;

	if (FLAG(bp_ptr, TR_HURT_LITE) && !FLAG(bp_ptr, TR_RES_LITE))
	{
		list_item *l_ptr = look_up_equip_slot(EQUIP_LITE);

		/* Do not rest with an artifact lite */
		if (l_ptr && KN_FLAG(l_ptr, TR_INSTA_ART)) return (FALSE);

		/* Do not rest in Sunlight */
		if (!bp_ptr->depth &&
			bp_ptr->hour >= 5 &&
			bp_ptr->hour <= 18) return (FALSE);
	}

	/* Now check the ground to see if safe. */
	if (!borg_on_safe_feat(map_loc(c_x, c_y)->feat)) return (FALSE);

	/* Examine all the monsters */
	for (i = 1; i < borg_kills_nxt; i++)
	{
		borg_kill *kill = &borg_kills[i];
		monster_race *r_ptr = &r_info[kill->r_idx];

		int x9 = kill->x;
		int y9 = kill->y;
		int ax, ay, d;
		int p = 0;

		/* Skip dead monsters */
		if (!kill->r_idx) continue;

		/* Distance components */
		ax = (x9 > c_x) ? (x9 - c_x) : (c_x - x9);
		ay = (y9 > c_y) ? (y9 - c_y) : (c_y - y9);

		/* Distance */
		d = MAX(ax, ay);

		/* Minimal distance */
		if (d > 16) continue;

		/* if too close, don't rest */
		if (d < 2) return (FALSE);

		/* If too close, don't rest */
		if (d < 3 && !(FLAG(r_ptr, RF_NEVER_MOVE))) return (FALSE);

		/* one call for dangers */
		borg_full_damage = TRUE;
		p = borg_danger_aux(x9, y9, 1, i, TRUE);
		borg_full_damage = FALSE;


		/* Real scary guys pretty close */
		if (d < 5 && (p > avoidance / 3)) return (FALSE);

		/* should check LOS... monster to me */
		if (borg_los(x9, y9, c_x, c_y)) return FALSE;

		/* should check LOS... me to monster */
		if (borg_los(c_x, c_y, x9, y9)) return FALSE;

		/* Perhaps borg should check and see if the previous grid was los */

		/* if absorbs mana, not safe */
		if ((FLAG(r_ptr, RF_DRAIN_MANA)) && (bp_ptr->msp > 1)) return FALSE;

		/* if it walks through walls, not safe */
		if (FLAG(r_ptr, RF_PASS_WALL)) return FALSE;
		if (FLAG(r_ptr, RF_KILL_WALL)) return FALSE;
	}

	/* Otherwise ok */
	return TRUE;
}


#else

#ifdef MACINTOSH
static int HACK = 0;
#endif

#endif
