/* File: spells2.c */

/* Purpose: Spell code (part 2) */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

/*
 * Increase players hit points, notice effects
 */
bool hp_player(int num)
{
	/* Healing needed */
	if (p_ptr->chp < p_ptr->mhp)
	{
		/* Gain hitpoints */
		p_ptr->chp += num;

		/* Enforce maximum */
		if (p_ptr->chp >= p_ptr->mhp)
		{
			p_ptr->chp = p_ptr->mhp;
			p_ptr->chp_frac = 0;
		}

		/* Redraw */
		p_ptr->redraw |= (PR_HP);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER);

		/* Heal 0-4 */
		if (num < 5)
		{
			msg_print("You feel a little better.");
		}

		/* Heal 5-14 */
		else if (num < 15)
		{
			msg_print("You feel better.");
		}

		/* Heal 15-34 */
		else if (num < 35)
		{
			msg_print("You feel much better.");
		}

		/* Heal 35+ */
		else
		{
			msg_print("You feel very good.");
		}

		/* Notice */
		return (TRUE);
	}

	/* Ignore */
	return (FALSE);
}


/*
 * Array of stat "descriptions"
 */
static cptr desc_stat_pos[] =
{
	"strong",
	"smart",
	"wise",
	"dextrous",
	"healthy",
	"cute"
};


/*
 * Array of stat "descriptions"
 */
static cptr desc_stat_neg[] =
{
	"weak",
	"stupid",
	"naive",
	"clumsy",
	"sickly",
	"ugly"
};


/*
 * Lose a "point"
 */
bool do_dec_stat(int stat)
{
	bool sust = FALSE;

	/* Access the "sustain" */
	switch (stat)
	{
		case A_STR: if (p_ptr->sustain_str) sust = TRUE; break;
		case A_INT: if (p_ptr->sustain_int) sust = TRUE; break;
		case A_WIS: if (p_ptr->sustain_wis) sust = TRUE; break;
		case A_DEX: if (p_ptr->sustain_dex) sust = TRUE; break;
		case A_CON: if (p_ptr->sustain_con) sust = TRUE; break;
		case A_CHR: if (p_ptr->sustain_chr) sust = TRUE; break;
	}

	/* Sustain */
	if (sust)
	{
		/* Message */
		msg_format("You feel %s for a moment, but the feeling passes.",
			   desc_stat_neg[stat]);

		/* Notice effect */
		return (TRUE);
	}

	/* Attempt to reduce the stat */
	if (dec_stat(stat, 10, FALSE))
	{
		/* Message */
		msg_format("You feel very %s.", desc_stat_neg[stat]);

		/* Notice effect */
		return (TRUE);
	}

	/* Nothing obvious */
	return (FALSE);
}


/*
 * Restore lost "points" in a stat
 */
bool do_res_stat(int stat)
{
	/* Attempt to increase */
	if (res_stat(stat))
	{
		/* Message */
		msg_format("You feel less %s.", desc_stat_neg[stat]);

		/* Notice */
		return (TRUE);
	}

	/* Nothing obvious */
	return (FALSE);
}


/*
 * Gain a "point" in a stat
 */
bool do_inc_stat(int stat)
{
	bool res;

	/* Restore strength */
	res = res_stat(stat);

	/* Attempt to increase */
	if (inc_stat(stat))
	{
		/* Message */
		msg_format("Wow!  You feel very %s!", desc_stat_pos[stat]);

		/* Notice */
		return (TRUE);
	}

	/* Restoration worked */
	if (res)
	{
		/* Message */
		msg_format("You feel less %s.", desc_stat_neg[stat]);

		/* Notice */
		return (TRUE);
	}

	/* Nothing obvious */
	return (FALSE);
}


/*
 * Removes curses from items in inventory
 *
 * Note that Items which are "Perma-Cursed" (The One Ring,
 * The Crown of Morgoth) can NEVER be uncursed.
 *
 * Note that if "all" is FALSE, then Items which are
 * "Heavy-Cursed" (Mormegil, Calris, and Weapons of Morgul)
 * will not be uncursed.
 */
static int remove_curse_aux(int all)
{
	int i, cnt = 0;
/*	s32b curse_turn; */


	/* Attempt to uncurse items being worn */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		u32b f1, f2, f3;

		object_type *o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Uncursed already */
		if (!cursed_p(o_ptr)) continue;

		/* Extract the flags */
		object_flags(o_ptr, &f1, &f2, &f3);

		/* Heavily Cursed Items need a special spell */
		if (!all && (f3 & (TR3_HEAVY_CURSE))) continue;

		/* Perma-Cursed Items can NEVER be uncursed */
		if (f3 & (TR3_PERMA_CURSE)) continue;

		/* Uncurse it */
		o_ptr->ident &= ~(IDENT_CURSED);

		/* Hack -- Assume felt */
		o_ptr->ident |= (IDENT_SENSE);

		if (o_ptr->art_flags3 & (TR3_CURSED))
			o_ptr->art_flags3 &= ~(TR3_CURSED);

		if (o_ptr->art_flags3 & (TR3_HEAVY_CURSE))
			o_ptr->art_flags3 &= ~(TR3_HEAVY_CURSE);

		/* Take note */
		o_ptr->note = quark_add("uncursed");

		/* Recalculate the bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Window stuff */
		p_ptr->window |= (PW_EQUIP);

 		/* Set auto_curse timeout (was 5) */
 		curse_turn = turn + 50;

		/* Count the uncursings */
		cnt++;
	}

	/* Return "something uncursed" */
	return (cnt);
}


/*
 * Remove most curses
 */
bool remove_curse(void)
{
	return (remove_curse_aux(FALSE));
}

/*
 * Remove all curses
 */
bool remove_all_curse(void)
{
	return (remove_curse_aux(TRUE));
}


/*
 * self-knowledge... idea from nethack.  Useful for determining powers and
 * resistences of items.  It saves the screen, clears it, then starts listing
 * attributes, a screenful at a time.  (There are a LOT of attributes to
 * list.  It will probably take 2 or 3 screens for a powerful character whose
 * using several artifacts...) -CFT
 *
 * It is now a lot more efficient. -BEN-
 *
 * See also "identify_fully()".
 *
 * XXX XXX XXX Use the "show_file()" method, perhaps.
 */
void self_knowledge(void)
{
	int		i = 0, j, k;
	u32b		f1 = 0L, f2 = 0L, f3 = 0L;
	object_type	*o_ptr;
	char		Dummy[80];
	cptr		info[128];
	int		plev = p_ptr->lev;

	strcpy (Dummy, "");

	/* Acquire item flags from equipment */
	for (k = INVEN_WIELD; k < INVEN_TOTAL; k++)
	{
		u32b t1, t2, t3;

		o_ptr = &inventory[k];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Extract the flags */
		object_flags(o_ptr, &t1, &t2, &t3);

		/* Extract flags */
		f1 |= t1;
		f2 |= t2;
		f3 |= t3;
	}

	/* Racial powers... */
	switch (p_ptr->prace)
	{
		case RACE_HUMAN:
			if (plev > 14)
				info[i++] = "You have an understanding of your abilities (cost 10).";
			break;
		case RACE_ELF:
			if (plev > 9)
				info[i++] = "You can sense natural creatures (cost 5).";
			break;
		case RACE_DWARF:
			if (plev > 4)
				info[i++] = "You can find traps, doors and stairs (cost 5).";
			break;
		case RACE_HOBBIT:
			if (plev > 14)
				info[i++] = "You can produce food (cost 10).";
			break;
		case RACE_GNOME:
			if (plev > 4)
			{
				sprintf(Dummy, "You can teleport, range %d (cost %d).",
				    (1+plev), (5 + (plev/5)));
				info[i++] = Dummy;
			}
			break;
		case RACE_HALF_ORC:
			if (plev > 2)
				info[i++] = "You can remove fear (cost 5).";
			break;
		case RACE_HALF_TROLL:
			if (plev > 9)
				info[i++] = "You can go berserk (cost 12).";
			break;
		case RACE_GAMBOLT:
			if (plev > 19)
				info[i++] = "You can be charming (cost lvl).";
			break;
		case RACE_BARBARIAN:
			if (plev > 7)
				info[i++] = "You can go berserk (cost 10).";
			break;
		case RACE_HALF_GIANT:
			if (plev > 19)
				info[i++] = "You can break stone walls (cost 10).";
			break;
		case RACE_HALF_TITAN:
			if (plev > 34)
				info[i++] = "You can probe monsters (cost 20).";
			break;
		case RACE_KLACKON:
			if (plev > 8)
			{
				sprintf(Dummy, "You can spit acid, dam. %d (cost 9).", plev * 2);
				info[i++] = Dummy;
			}
			break;
		case RACE_KOBOLD:
			if (plev > 11)
			{
				sprintf(Dummy,
				    "You can throw a dart of poison, dam. %d (cost 8).", plev * 2);
				info[i++] = Dummy;
			}
			break;
		case RACE_DRACONIAN:
			sprintf(Dummy, "You can breathe, dam. %d (cost %d).", (plev * 3), plev);
			info[i++] = Dummy;
			break;
		case RACE_MIND_FLAYER:
			if (plev > 14)
				sprintf(Dummy, "You can mind blast your enemies, dam %d (cost 12).", plev * 2);
			info[i++] = Dummy;
			break;
		case RACE_GOLEM:
			if (plev > 19)
				info[i++] = "You can turn your skin to stone, dur d20+30 (cost 15).";
			break;
		case RACE_VAMPIRE:
			if (plev > 1)
			{
				sprintf(Dummy, "You can steal life from a foe, dam. %d-%d (cost %d).",
				    plev+MAX(1, plev/10), plev+plev*MAX(1, plev/10), 1+(plev/3));
				info[i++] = Dummy;
			}
			break;
		case RACE_SPECTRE:
			if (plev > 3)
			{
				info[i++] = "You can wail to terrify your enemies (cost 3).";
			}
			break;
		case RACE_YEEK:
			info[i++] = "You can flee in terror (cost 2).";
			break;
		case RACE_MELNIBONEAN:
			if (plev > 29)
			{
				info[i++] = "You can summon demons (cost 40).";
			}
			break;
		case RACE_VADHAGH:
			if (plev > 29)
			{
				info[i++] = "You can travel the planes of the Earth (cost 25).";
			}
			break;
		default:
			break;
	}

	/* Handle mutations */
	if (p_ptr->muta1)
	{
		if (p_ptr->muta1 & MUT1_SPIT_ACID)
		{
			info[i++] = "You can spit acid (dam lvl*2).";
		}
		if (p_ptr->muta1 & MUT1_BR_FIRE)
		{
			info[i++] = "You can breathe fire (dam lvl*3).";
		}
		if (p_ptr->muta1 & MUT1_HYPN_GAZE)
		{
			info[i++] = "Your gaze is hypnotic.";
		}
		if (p_ptr->muta1 & MUT1_APPORTATION)
		{
			info[i++] = "You can teleport objects.";
		}
		if (p_ptr->muta1 & MUT1_VTELEPORT)
		{
			info[i++] = "You can teleport at will.";
		}
		if (p_ptr->muta1 & MUT1_MIND_BLST)
		{
			info[i++] = "You can Mind Blast your enemies.";
		}
		if (p_ptr->muta1 & MUT1_RADIATION)
		{
			info[i++] = "You can emit hard radiation at will (dam lvl*3).";
		}
		if (p_ptr->muta1 & MUT1_VAMPIRISM)
		{
			info[i++] = "You can drain life from a foe like a vampire.";
		}
		if (p_ptr->muta1 & MUT1_BLINK)
		{
			info[i++] = "You can teleport yourself short distances.";
		}
		if (p_ptr->muta1 & MUT1_EAT_ROCK)
		{
			info[i++] = "You can consume solid rock.";
		}
		if (p_ptr->muta1 & MUT1_SHRIEK)
		{
			info[i++] = "You can emit a horrible shriek (dam lvl*3).";
		}
		if (p_ptr->muta1 & MUT1_ILLUMINE)
		{
			info[i++] = "You can emit bright light.";
		}
		if (p_ptr->muta1 & MUT1_DET_CURSE)
		{
			info[i++] = "You can feel the danger of evil magic.";
		}
		if (p_ptr->muta1 & MUT1_BERSERK)
		{
			info[i++] = "You can drive yourself into a berserk frenzy.";
		}
		if (p_ptr->muta1 & MUT1_POLYMORPH)
		{
			info[i++] = "You can polymorph yourself at will.";
		}
		if (p_ptr->muta1 & MUT1_MIDAS_TCH)
		{
			info[i++] = "You can turn ordinary items to gold.";
		}
		if (p_ptr->muta1 & MUT1_GROW_MOLD)
		{
			info[i++] = "You can cause mold to grow near you.";
		}
		if (p_ptr->muta1 & MUT1_RESIST)
		{
			info[i++] = "You can harden yourself to the ravages of the elements.";
		}
		if (p_ptr->muta1 & MUT1_EARTHQUAKE)
		{
			info[i++] = "You can bring down the dungeon around your ears.";
		}
		if (p_ptr->muta1 & MUT1_DAZZLE)
		{
			info[i++] = "You can emit confusing, blinding radiation.";
		}
		if (p_ptr->muta1 & MUT1_RECALL)
		{
			info[i++] = "You can travel between town and the depths.";
		}
		if (p_ptr->muta1 & MUT1_BANISH)
		{
			info[i++] = "You can send evil creatures directly to Hell.";
		}
		if (p_ptr->muta1 & MUT1_COLD_TOUCH)
		{
			info[i++] = "You can freeze things with a touch (dam lvl*3).";
		}
		if (p_ptr->muta1 & MUT1_MISSILE)
		{
			info[i++] = "You can cast magic bolts (dam ((3 + lvl - 1)/3)d4).";
		}
		if (p_ptr->muta1 & MUT1_SHARD_BOLT)
		{
			info[i++] = "You can cast shards (dam ((3 + lvl)/5)d5).";
		}
		if (p_ptr->muta1 & MUT1_SHARD_BLAST)
		{
			info[i++] = "You can cast volleys of shards (dam ((2 + lvl)/5)d4).";
		}
		if (p_ptr->muta1 & MUT1_DSHARD_BLAST)
		{
			info[i++] = "You can cast large volleys of shards (dam ((2 + lvl)/5)d4).";
		}
		if (p_ptr->muta1 & MUT1_CHAIN_SHARDS)
		{
			info[i++] = "You can cast shards rapidly (dam ((3 + lvl)/5)d5).";
		}
		if (p_ptr->muta1 & MUT1_ROCKET)
		{
			info[i++] = "You can fire rockets (dam lvl*4).";
		}
		if (p_ptr->muta1 & MUT1_GRAV_BEAM)
		{
			info[i++] = "You can shoot a beam of gravity.";
		}
	}

	if (p_ptr->muta2)
	{
		if (p_ptr->muta2 & MUT2_BERS_RAGE)
		{
			info[i++] = "You are subject to berserker fits.";
		}
		if (p_ptr->muta2 & MUT2_COWARDICE)
		{
			info[i++] = "You are subject to cowardice.";
		}
		if (p_ptr->muta2 & MUT2_RTELEPORT)
		{
			info[i++] = "You are teleporting randomly.";
		}
		if (p_ptr->muta2 & MUT2_ALCOHOL)
		{
			info[i++] = "Your body produces alcohol.";
		}
		if (p_ptr->muta2 & MUT2_HALLU)
		{
			info[i++] = "You have a hallucinatory insanity.";
		}
		if (p_ptr->muta2 & MUT2_FLATULENT)
		{
			info[i++] = "You are subject to uncontrollable flatulence.";
		}
		if (p_ptr->muta2 & MUT2_PROD_MANA)
		{
			info[i++] = "You are producing magical energy uncontrollably.";
		}
		if (p_ptr->muta2 & MUT2_ATT_ANIMAL)
		{
			info[i++] = "You attract animals.";
		}
		if (p_ptr->muta2 & MUT2_ATT_DEMON)
		{
			info[i++] = "You attract demons.";
		}
		if (p_ptr->muta2 & MUT2_ATT_DRAGON)
		{
			info[i++] = "You attract dragons.";
		}
		if (p_ptr->muta2 & MUT2_WOUND)
		{
			info[i++] = "Your flesh is very delicate.";
		}
		if (p_ptr->muta2 & MUT2_SCOR_TAIL)
		{
			info[i++] = "You have a scorpion tail (poison, 3d7).";
		}
		if (p_ptr->muta2 & MUT2_HORNS)
		{
			info[i++] = "You have horns (dam. 2d6).";
		}
		if (p_ptr->muta2 & MUT2_BEAK)
		{
			info[i++] = "You have a beak (dam. 2d4).";
		}
		if (p_ptr->muta2 & MUT2_TUSKS)
		{
			info[i++] = "You have tusks (dam. 2d6).";
		}
		if (p_ptr->muta2 & MUT2_CLAWS)
		{
			info[i++] = "You have claws (dam. 2d3).";
		}
		if (p_ptr->muta2 & MUT2_DISPEL_ALL)
		{
			info[i++] = "You are shrouded in evil.";
		}
		if (p_ptr->muta2 & MUT2_EAT_LIGHT)
		{
			info[i++] = "You sometimes feed off of the light around you.";
		}
		if (p_ptr->muta2 & MUT2_RAW_CHAOS)
		{
			info[i++] = "You occasionally are surrounded with raw chaos.";
		}
		if (p_ptr->muta2 & MUT2_WRAITH)
		{
			info[i++] = "You fade in and out of physical reality.";
		}
		if (p_ptr->muta2 & MUT2_POLY_WOUND)
		{
			info[i++] = "Your health is subject to chaotic forces.";
		}
		if (p_ptr->muta2 & MUT2_WASTING)
		{
			info[i++] = "You have a horrible wasting disease.";
		}
		if (p_ptr->muta2 & MUT2_WEIRD_MIND)
		{
			info[i++] = "Your mind randomly expands and contracts.";
		}
		if (p_ptr->muta2 & MUT2_NAUSEA)
		{
			info[i++] = "You have a seriously upset stomach.";
		}
		if (p_ptr->muta2 & MUT2_CHAOS_GIFT)
		{
			info[i++] = "A Chaos Lord has taken notice of you.";
		}
		if (p_ptr->muta2 & MUT2_WALK_SHAD)
		{
			info[i++] = "You occasionally stumble into other shadows.";
		}
		if (p_ptr->muta2 & MUT2_WARNING)
		{
			info[i++] = "You receive warnings about your foes.";
		}
		if (p_ptr->muta2 & MUT2_INVULN)
		{
			info[i++] = "You occasionally feel resilient.";
		}
		if (p_ptr->muta2 & MUT2_SP_TO_HP)
		{
			info[i++] = "Your blood sometimes rushes to your muscles.";
		}
		if (p_ptr->muta2 & MUT2_HP_TO_SP)
		{
			info[i++] = "Your blood sometimes rushes to your head.";
		}
		if (p_ptr->muta2 & MUT2_DISARM)
		{
			info[i++] = "You occasionally stumble and drop things.";
		}
		if (p_ptr->muta2 & MUT2_TENTACLES)
		{
			info[i++] = "You have tentacles (slow, 3d3).";
		}
	}

	if (p_ptr->muta3)
	{
		if (p_ptr->muta3 & MUT3_HYPER_STR)
		{
			info[i++] = "You are superhumanly strong (+4 STR).";
		}
		if (p_ptr->muta3 & MUT3_PUNY)
		{
			info[i++] = "You are puny (-4 STR).";
		}
		if (p_ptr->muta3 & MUT3_HYPER_INT)
		{
			info[i++] = "Your brain is a living computer (+4 INT/WIS).";
		}
		if (p_ptr->muta3 & MUT3_MORONIC)
		{
			info[i++] = "You are moronic (-4 INT/WIS).";
		}
		if (p_ptr->muta3 & MUT3_RESILIENT)
		{
			info[i++] = "You are very tough (+4 CON).";
		}
		if (p_ptr->muta3 & MUT3_XTRA_FAT)
		{
			info[i++] = "You are extremely fat (+2 CON, -2 speed).";
		}
		if (p_ptr->muta3 & MUT3_ALBINO)
		{
			info[i++] = "You are albino (-4 CON).";
		}
		if (p_ptr->muta3 & MUT3_FLESH_ROT)
		{
			info[i++] = "Your flesh is rotting (-2 CON, -1 CHR).";
		}
		if (p_ptr->muta3 & MUT3_SILLY_VOI)
		{
			info[i++] = "Your voice is a silly squeak (-4 CHR).";
		}
		if (p_ptr->muta3 & MUT3_BLANK_FAC)
		{
			info[i++] = "Your face is featureless (-1 CHR).";
		}
		if (p_ptr->muta3 & MUT3_ILL_NORM)
		{
			info[i++] = "Your appearance is masked with illusion.";
		}
		if (p_ptr->muta3 & MUT3_XTRA_EYES)
		{
			info[i++] = "You have an extra pair of eyes (+15 search).";
		}
		if (p_ptr->muta3 & MUT3_MAGIC_RES)
		{
			info[i++] = "You are resistant to magic.";
		}
		if (p_ptr->muta3 & MUT3_XTRA_NOIS)
		{
			info[i++] = "You make a lot of strange noise (-3 stealth).";
		}
		if (p_ptr->muta3 & MUT3_INFRAVIS)
		{
			info[i++] = "You have remarkable infravision (+3).";
		}
		if (p_ptr->muta3 & MUT3_XTRA_LEGS)
		{
			info[i++] = "You have an extra pair of legs (+3 speed).";
		}
		if (p_ptr->muta3 & MUT3_SHORT_LEG)
		{
			info[i++] = "Your legs are short stubs (-3 speed).";
		}
		if (p_ptr->muta3 & MUT3_ELEC_TOUC)
		{
			info[i++] = "Electricity is running through your veins.";
		}
		if (p_ptr->muta3 & MUT3_FIRE_BODY)
		{
			info[i++] = "Your body is enveloped in flames.";
		}
		if (p_ptr->muta3 & MUT3_SPINES)
		{
			info[i++] = "Your body is covered with sharp spines.";
		}
		if (p_ptr->muta3 & MUT3_WART_SKIN)
		{
			info[i++] = "Your skin is covered with warts (-2 CHR, +5 AC).";
		}
		if (p_ptr->muta3 & MUT3_SCALES)
		{
			info[i++] = "Your skin has turned into scales (-1 CHR, +10 AC).";
		}
		if (p_ptr->muta3 & MUT3_IRON_SKIN)
		{
			info[i++] = "Your skin is made of iron (-1 DEX, +25 AC).";
		}
		if (p_ptr->muta3 & MUT3_WINGS)
		{
			info[i++] = "You have wings.";
		}
		if (p_ptr->muta3 & MUT3_FEARLESS)
		{
			/* Unnecessary */
		}
		if (p_ptr->muta3 & MUT3_REGEN)
		{
			/* Unnecessary */
		}
		if (p_ptr->muta3 & MUT3_ESP)
		{
			/* Unnecessary */
		}
		if (p_ptr->muta3 & MUT3_TWISTED)
		{
			info[i++] = "Your frame is unnaturally twisted.";
		}
		if (p_ptr->muta3 & MUT3_LIMBER)
		{
			info[i++] = "Your body is very limber (+3 DEX).";
		}
		if (p_ptr->muta3 & MUT3_ARTHRITIS)
		{
			info[i++] = "Your joints ache constantly (-3 DEX).";
		}
		if (p_ptr->muta3 & MUT3_VULN_ELEM)
		{
			info[i++] = "You are susceptible to damage from the elements.";
		}
		if (p_ptr->muta3 & MUT3_GLOW)
		{
			info[i++] = "Your body is glowing brightly.";
		}
	}

	if (p_ptr->astral)
	{
		info[i++] = "You are currently an astral being.";
	}
	if (p_ptr->blind)
	{
		info[i++] = "You cannot see.";
	}
	if (p_ptr->confused)
	{
		info[i++] = "You are confused.";
	}
	if (p_ptr->afraid)
	{
		info[i++] = "You are terrified.";
	}
	if (p_ptr->cut)
	{
		info[i++] = "You are bleeding.";
	}
	if (p_ptr->stun)
	{
		info[i++] = "You are stunned.";
	}
	if (p_ptr->poisoned)
	{
		info[i++] = "You are poisoned.";
	}
	if (p_ptr->image)
	{
		info[i++] = "You are hallucinating.";
	}
	if (p_ptr->aggravate)
	{
		info[i++] = "You aggravate monsters.";
	}
	if (p_ptr->teleport)
	{
		info[i++] = "Your position is very uncertain.";
	}
	if (p_ptr->blessed)
	{
		info[i++] = "You feel righteous.";
	}
	if (p_ptr->hero)
	{
		info[i++] = "You feel heroic.";
	}
	if (p_ptr->shero)
	{
		info[i++] = "You are in a battle rage.";
	}
	if (p_ptr->protevil)
	{
		info[i++] = "You are protected from evil.";
	}
	if (p_ptr->shield)
	{
		info[i++] = "You are protected by a mystic shield.";
	}
	if (p_ptr->invuln)
	{
		info[i++] = "You are temporarily resilient.";
	}
	if (p_ptr->wraith_form)
	{
		info[i++] = "You are temporarily incorporeal.";
	}
	if (p_ptr->confusing)
	{
		info[i++] = "Your hands are glowing dull red.";
	}
	if (p_ptr->searching)
	{
		info[i++] = "You are looking around very carefully.";
	}
	if (p_ptr->new_spells)
	{
		info[i++] = "You can learn some spells/prayers.";
	}
	if (p_ptr->word_recall)
	{
		info[i++] = "You will soon be recalled.";
	}
	if (p_ptr->see_infra)
	{
		info[i++] = "Your eyes are sensitive to infrared light.";
	}
	if (p_ptr->see_inv)
	{
		info[i++] = "You can see invisible creatures.";
	}
	if (p_ptr->ffall)
	{
		info[i++] = "You can fly.";
	}
	if (p_ptr->free_act)
	{
		info[i++] = "You have free action.";
	}
	if (p_ptr->regenerate)
	{
		info[i++] = "You regenerate quickly.";
	}
	if (p_ptr->slow_digest)
	{
		info[i++] = "Your appetite is small.";
	}
	if (p_ptr->telepathy)
	{
		info[i++] = "You have ESP.";
	}
	if (p_ptr->hold_life)
	{
		info[i++] = "You have a firm hold on your life force.";
	}
	if (p_ptr->reflect)
	{
		info[i++] = "You reflect arrows and bolts.";
	}
	if (p_ptr->sh_fire)
	{
		info[i++] = "You are surrounded with a fiery aura.";
	}
	if (p_ptr->sh_elec)
	{
		info[i++] = "You are surrounded with electricity.";
	}
	if (p_ptr->sh_spine)
	{
		info[i++] = "You are covered with sharp spines.";
	}
	if (p_ptr->devices)
	{
		info[i++] = "You understand magical devices.";
	}
	if (p_ptr->anti_magic)
	{
		info[i++] = "You are surrounded by an anti-magic shell.";
	}
	if (p_ptr->anti_tele)
	{
		info[i++] = "You cannot teleport.";
	}
	if (p_ptr->lite)
	{
		info[i++] = "You are carrying a permanent light.";
	}

	if (p_ptr->immune_acid)
	{
		info[i++] = "You are completely immune to acid.";
	}
	else if ((p_ptr->resist_acid) && (p_ptr->oppose_acid))
	{
		info[i++] = "You resist acid exceptionally well.";
	}
	else if ((p_ptr->resist_acid) || (p_ptr->oppose_acid))
	{
		info[i++] = "You are resistant to acid.";
	}

	if (p_ptr->immune_elec)
	{
		info[i++] = "You are completely immune to lightning.";
	}
	else if ((p_ptr->resist_elec) && (p_ptr->oppose_elec))
	{
		info[i++] = "You resist lightning exceptionally well.";
	}
	else if ((p_ptr->resist_elec) || (p_ptr->oppose_elec))
	{
		info[i++] = "You are resistant to lightning.";
	}

	if (p_ptr->immune_fire)
	{
		info[i++] = "You are completely immune to fire.";
	}
	else if ((p_ptr->resist_fire) && (p_ptr->oppose_fire))
	{
		info[i++] = "You resist fire exceptionally well.";
	}
	else if ((p_ptr->resist_fire) || (p_ptr->oppose_fire))
	{
		info[i++] = "You are resistant to fire.";
	}

	if (p_ptr->immune_cold)
	{
		info[i++] = "You are completely immune to cold.";
	}
	else if ((p_ptr->resist_cold) && (p_ptr->oppose_cold))
	{
		info[i++] = "You resist cold exceptionally well.";
	}
	else if ((p_ptr->resist_cold) || (p_ptr->oppose_cold))
	{
		info[i++] = "You are resistant to cold.";
	}

	if ((p_ptr->resist_pois) && (p_ptr->oppose_pois))
	{
		info[i++] = "You resist poison exceptionally well.";
	}
	else if ((p_ptr->resist_pois) || (p_ptr->oppose_pois))
	{
		info[i++] = "You are resistant to poison.";
	}

	if (p_ptr->resist_lite)
	{
		info[i++] = "You are resistant to bright light.";
	}
	if (p_ptr->resist_dark)
	{
		info[i++] = "You are resistant to darkness.";
	}
	if (p_ptr->resist_conf)
	{
		info[i++] = "You are resistant to confusion.";
	}
	if (p_ptr->resist_sound)
	{
		info[i++] = "You are resistant to sonic attacks.";
	}
	if (p_ptr->resist_disen)
	{
		info[i++] = "You are resistant to disenchantment.";
	}
	if (p_ptr->resist_chaos)
	{
		info[i++] = "You are resistant to chaos.";
	}
	if (p_ptr->resist_shard)
	{
		info[i++] = "You are resistant to blasts of shards.";
	}
	if (p_ptr->resist_nexus)
	{
		info[i++] = "You are resistant to nexus attacks.";
	}
	if (p_ptr->resist_neth)
	{
		info[i++] = "You are resistant to nether forces.";
	}
	if (p_ptr->resist_fear)
	{
		info[i++] = "You are completely fearless.";
	}
	if (p_ptr->resist_blind)
	{
		info[i++] = "Your eyes are resistant to blindness.";
	}

	if (p_ptr->sustain_str && p_ptr->sustain_int && p_ptr->sustain_wis &&
	    p_ptr->sustain_dex && p_ptr->sustain_con && p_ptr->sustain_chr)
	{
		info[i++] = "All of your attributes are sustained.";
	}
	else
	{
		if (p_ptr->sustain_str)
		{
			info[i++] = "Your strength is sustained.";
		}
		if (p_ptr->sustain_int)
		{
			info[i++] = "Your intelligence is sustained.";
		}
		if (p_ptr->sustain_wis)
		{
			info[i++] = "Your wisdom is sustained.";
		}
		if (p_ptr->sustain_con)
		{
			info[i++] = "Your constitution is sustained.";
		}
		if (p_ptr->sustain_dex)
		{
			info[i++] = "Your dexterity is sustained.";
		}
		if (p_ptr->sustain_chr)
		{
			info[i++] = "Your charisma is sustained.";
		}
	}

	if ((f1 & (TR1_STR)) && (f1 & (TR1_INT)) && (f1 & (TR1_WIS)) &&
	    (f1 & (TR1_DEX)) && (f1 & (TR1_CON)) && (f1 & (TR1_STR)))
	{
		info[i++] = "All of your stats are affected by equipment.";
	}
	else
	{
		if (f1 & (TR1_STR))
		{
			info[i++] = "Your strength is affected by your equipment.";
		}
		if (f1 & (TR1_INT))
		{
			info[i++] = "Your intelligence is affected by your equipment.";
		}
		if (f1 & (TR1_WIS))
		{
			info[i++] = "Your wisdom is affected by your equipment.";
		}
		if (f1 & (TR1_DEX))
		{
			info[i++] = "Your dexterity is affected by your equipment.";
		}
		if (f1 & (TR1_CON))
		{
			info[i++] = "Your constitution is affected by your equipment.";
		}
		if (f1 & (TR1_CHR))
		{
			info[i++] = "Your charisma is affected by your equipment.";
		}
	}

	if (f1 & (TR1_STEALTH))
	{
		info[i++] = "Your stealth is affected by your equipment.";
	}
	if (f1 & (TR1_SEARCH))
	{
		info[i++] = "Your searching ability is affected by your equipment.";
	}
	if (f1 & (TR1_INFRA))
	{
		info[i++] = "Your infravision is affected by your equipment.";
	}
	if (f1 & (TR1_TUNNEL))
	{
		info[i++] = "Your digging ability is affected by your equipment.";
	}
	if (f1 & (TR1_SPEED))
	{
		info[i++] = "Your speed is affected by your equipment.";
	}
	if (f1 & (TR1_BLOWS))
	{
		info[i++] = "Your attack speed is affected by your equipment.";
	}


	/* Access the current weapon */
	o_ptr = &inventory[INVEN_WIELD];

	/* Analyze the weapon */
	if (o_ptr->k_idx)
	{
		/* Indicate Blessing */
		if (f3 & (TR3_BLESSED))
		{
			info[i++] = "Your weapon has been blessed by the gods.";
		}

		if (f1 & (TR1_CHAOTIC))
		{
			info[i++] = "Your weapon is marked with the Arms of Chaos.";
		}

		/* Hack */
		if (f1 & (TR1_IMPACT))
		{
			info[i++] = "The impact of your weapon can cause earthquakes.";
		}

		if (f1 & (TR1_VORPAL))
		{
			info[i++] = "Your weapon is very sharp.";
		}

		if (f1 & (TR1_VAMPIRIC))
		{
			info[i++] = "Your weapon drains life from your foes.";
		}

		/* Special "Attack Bonuses" */
		if (f1 & (TR1_BRAND_ACID))
		{
			info[i++] = "Your weapon melts your foes.";
		}
		if (f1 & (TR1_BRAND_ELEC))
		{
			info[i++] = "Your weapon shocks your foes.";
		}
		if (f1 & (TR1_BRAND_FIRE))
		{
			info[i++] = "Your weapon burns your foes.";
		}
		if (f1 & (TR1_BRAND_COLD))
		{
			info[i++] = "Your weapon freezes your foes.";
		}
		if (f1 & (TR1_BRAND_POIS))
		{
			info[i++] = "Your weapon poisons your foes.";
		}

		/* Special "slay" flags */
		if (f1 & (TR1_SLAY_HUMANOID))
		{
			info[i++] = "Your weapon strikes at humanoids with extra force.";
		}
		if (f1 & (TR1_SLAY_ELEMENTAL))
		{
			info[i++] = "Your weapon strikes at elementals with extra force.";
		}
		if (f1 & (TR1_SLAY_ANIMAL))
		{
			info[i++] = "Your weapon strikes at animals with extra force.";
		}
		if (f1 & (TR1_SLAY_EVIL))
		{
			info[i++] = "Your weapon strikes at evil with extra force.";
		}
		if (f1 & (TR1_SLAY_UNDEAD))
		{
			info[i++] = "Your weapon strikes at undead with holy wrath.";
		}
		if (f1 & (TR1_SLAY_DEMON))
		{
			info[i++] = "Your weapon strikes at demons with holy wrath.";
		}
		if (f1 & (TR1_SLAY_ORC))
		{
			info[i++] = "Your weapon is especially deadly against orcs.";
		}
		if (f1 & (TR1_SLAY_TROLL))
		{
			info[i++] = "Your weapon is especially deadly against trolls.";
		}
		if (f1 & (TR1_SLAY_GIANT))
		{
			info[i++] = "Your weapon is especially deadly against giants.";
		}
		if (f1 & (TR1_SLAY_DRAGON))
		{
			info[i++] = "Your weapon is especially deadly against dragons.";
		}
		if (f1 & (TR1_CHAOTIC))
		{
			info[i++] = "Your weapon is especially deadly against creatures of chaos.";
		}

		/* Special "kill" flags */
		if (f1 & (TR1_KILL_DRAGON))
		{
			info[i++] = "Your weapon is a great bane of dragons.";
		}
	}


	/* Save the screen */
	Term_save();

	/* Erase the screen */
	for (k = 1; k < 24; k++) prt("", k, 13);

	/* Label the information */
	prt("     Your Attributes:", 1, 15);

	/* We will print on top of the map (column 13) */
	for (k = 2, j = 0; j < i; j++)
	{
		/* Show the info */
		prt(info[j], k++, 15);

		/* Every 20 entries (lines 2 to 21), start over */
		if ((k == 22) && (j+1 < i))
		{
			prt("-- more --", k, 15);
			inkey();
			for (; k > 2; k--) prt("", k, 15);
		}
	}

	/* Pause */
	prt("[Press any key to continue]", k, 13);
	inkey();

	/* Restore the screen */
	Term_load();
}


/*
 * Forget everything
 */
bool lose_all_info(void)
{
	int i;

	/* Forget info about objects */
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		object_type *o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Allow "protection" by the MENTAL flag */
		if (o_ptr->ident & (IDENT_MENTAL)) continue;

		/* Remove "default inscriptions" */
		if (o_ptr->note && (o_ptr->ident & (IDENT_SENSE)))
		{
			/* Access the inscription */
			cptr q = quark_str(o_ptr->note);

			/* Hack -- Remove auto-inscriptions */
			if ((streq(q, "cursed")) ||
			    (streq(q, "broken")) ||
			    (streq(q, "good")) ||
			    (streq(q, "average")) ||
			    (streq(q, "excellent")) ||
			    (streq(q, "worthless")) ||
			    (streq(q, "special")) ||
			    (streq(q, "terrible")))
			{
				/* Forget the inscription */
				o_ptr->note = 0;
			}
		}

		/* Hack -- Clear the "empty" flag */
		o_ptr->ident &= ~(IDENT_EMPTY);

		/* Hack -- Clear the "known" flag */
		o_ptr->ident &= ~(IDENT_KNOWN);

		/* Hack -- Clear the "felt" flag */
		o_ptr->ident &= ~(IDENT_SENSE);
	}

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);

	wiz_dark();

	/* It worked */
	return (TRUE);
}


/*
 * Detect random things on the current panel, the Chaos detect spell -- Gumby
 */
bool detect_random(void)
{
	int		i, y, x;
	bool		flag = FALSE;
	cave_type	*c_ptr;
	int		chance = 50 + (p_ptr->lev / 2);

	/* Scan monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];

		/* Skip random monsters */
		if (randint(100) >= chance) continue;

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;

		/* Only detect nearby monsters */
		if (!panel_contains(y, x)) continue;
		
		/* Repair visibility later */
		repair_monsters = TRUE;

		/* Hack -- Detect monster */
		m_ptr->mflag |= (MFLAG_MARK | MFLAG_SHOW);

		/* Hack -- See monster */
		m_ptr->ml = TRUE;
			
		/* Redraw */
		lite_spot(y, x);

		/* Detect */
		flag = TRUE;
	}

	/* Scan objects */
	for (i = 1; i < o_max; i++)
	{
		object_type *o_ptr = &o_list[i];

		/* Skip random objects */
		if (randint(100) >= chance) continue;

		/* Skip dead objects */
		if (!o_ptr->k_idx) continue;

		/* Skip held objects */
		if (o_ptr->held_m_idx) continue;

		/* Location */
		y = o_ptr->iy;
		x = o_ptr->ix;

		/* Only detect nearby objects */
		if (!panel_contains(y, x)) continue;
		
		/* Hack -- memorize it */
		o_ptr->marked = TRUE;

		/* Redraw */
		lite_spot(y, x);

		/* Detect */
		flag = TRUE;
	}

	/* Scan the panel for buried treasure, traps, doors, stairs */
	for (y = panel_row_min; y <= panel_row_max; y++)
	{
		for (x = panel_col_min; x <= panel_col_max; x++)
		{
			c_ptr = &cave[y][x];

			/* Notice embedded gold */
			if (((c_ptr->feat == FEAT_MAGMA_H) ||
			    (c_ptr->feat == FEAT_QUARTZ_H)) &&
			    (randint(100) <= chance))
			{
				/* Expose the gold */
				c_ptr->feat += 0x02;
			}

			/* Detect secret doors */
			if ((c_ptr->feat == FEAT_SECRET) &&
			    (randint(100) <= chance))
			{
				/* Pick a door */
				cave_set_feat(y, x, FEAT_DOOR_HEAD + 0x00);
			}

			/* Detect invisible traps */
			if ((c_ptr->feat == FEAT_INVIS) &&
			    (randint(100) <= chance))
			{
				/* Pick a trap */
				pick_trap(y, x);
			}

			/* Detect the stuff... */
			if (((c_ptr->feat == FEAT_MAGMA_K) ||
			    (c_ptr->feat == FEAT_QUARTZ_K) ||
			    ((c_ptr->feat >= FEAT_DOOR_HEAD) &&
			     (c_ptr->feat <= FEAT_DOOR_TAIL)) ||
			    (c_ptr->feat == FEAT_OPEN) ||
			    (c_ptr->feat == FEAT_BROKEN) ||
			    (c_ptr->feat == FEAT_LESS) ||
			    (c_ptr->feat == FEAT_MORE) ||
			    ((c_ptr->feat >= FEAT_TRAP_HEAD) &&
			     (c_ptr->feat <= FEAT_TRAP_TAIL))) &&
			    (randint(100) <= chance))
			{
				/* Hack -- Memorize */
				c_ptr->info |= (CAVE_MARK);

				/* Redraw */
				lite_spot(y, x);

				/* Detect */
				flag = TRUE;
			}
		}
	}

	/* Possible chance of getting the panel mapped -- arch */
	if(randint(100) <= chance / 2)
	{
		map_area();
		flag = TRUE;
	}

	/* Describe */
	if (flag)
	{
		/* Describe result */
		msg_print("You sense the presense of random things!");
	}

	/* Result */
	return (flag);
}


/*
 * Detect all traps on current panel
 */
bool detect_traps(void)
{
	int             x, y;
	bool            detect = FALSE;
	cave_type       *c_ptr;


	/* Scan the current panel */
	for (y = panel_row_min; y <= panel_row_max; y++)
	{
		for (x = panel_col_min; x <= panel_col_max; x++)
		{
			/* Access the grid */
			c_ptr = &cave[y][x];

			/* Detect invisible traps */
			if (c_ptr->feat == FEAT_INVIS)
			{
				/* Pick a trap */
				pick_trap(y, x);
			}
			
			/* Detect traps */
			if ((c_ptr->feat >= FEAT_TRAP_HEAD) &&
			    (c_ptr->feat <= FEAT_TRAP_TAIL))
			{
				/* Hack -- Memorize */
				c_ptr->info |= (CAVE_MARK);

				/* Redraw */
				lite_spot(y, x);

				/* Obvious */
				detect = TRUE;
			}
		}
	}

	/* Describe */
	if (detect)
	{
		msg_print("You sense the presence of traps!");
	}

	/* Result */
	return (detect);
}



/*
 * Detect all doors on current panel
 */
bool detect_doors(void)
{
	int y, x;

	bool detect = FALSE;

	cave_type *c_ptr;


	/* Scan the panel */
	for (y = panel_row_min; y <= panel_row_max; y++)
	{
		for (x = panel_col_min; x <= panel_col_max; x++)
		{
			c_ptr = &cave[y][x];

			/* Detect secret doors */
			if (c_ptr->feat == FEAT_SECRET)
			{
				/* Pick a door XXX XXX XXX */
				cave_set_feat(y, x, FEAT_DOOR_HEAD + 0x00);
			}

			/* Detect doors */
			if (((c_ptr->feat >= FEAT_DOOR_HEAD) &&
			     (c_ptr->feat <= FEAT_DOOR_TAIL)) ||
			    ((c_ptr->feat == FEAT_OPEN) ||
			     (c_ptr->feat == FEAT_BROKEN)))
			{
				/* Hack -- Memorize */
				c_ptr->info |= (CAVE_MARK);

				/* Redraw */
				lite_spot(y, x);

				/* Obvious */
				detect = TRUE;
			}
		}
	}

	/* Describe */
	if (detect)
	{
		msg_print("You sense the presence of doors!");
	}

	/* Result */
	return (detect);
}


/*
 * Detect all stairs on current panel
 */
bool detect_stairs(void)
{
	int y, x;

	bool detect = FALSE;

	cave_type *c_ptr;


	/* Scan the panel */
	for (y = panel_row_min; y <= panel_row_max; y++)
	{
		for (x = panel_col_min; x <= panel_col_max; x++)
		{
			c_ptr = &cave[y][x];

			/* Detect stairs */
			if ((c_ptr->feat == FEAT_LESS) ||
			    (c_ptr->feat == FEAT_MORE))
			{
				/* Hack -- Memorize */
				c_ptr->info |= (CAVE_MARK);

				/* Redraw */
				lite_spot(y, x);

				/* Obvious */
				detect = TRUE;
			}
		}
	}

	/* Describe */
	if (detect_monsters_string("<>"))
	{
		detect = TRUE;
	}
	else if (detect)
	{
		msg_print("You sense the presence of stairs!");
	}

	/* Result */
	return (detect);
}


/*
 * Detect any treasure on the current panel
 */
bool detect_treasure(void)
{
	int y, x;

	bool detect = FALSE;

	cave_type *c_ptr;


	/* Scan the current panel */
	for (y = panel_row_min; y <= panel_row_max; y++)
	{
		for (x = panel_col_min; x <= panel_col_max; x++)
		{
			c_ptr = &cave[y][x];

			/* Notice embedded gold */
			if ((c_ptr->feat == FEAT_MAGMA_H) ||
			    (c_ptr->feat == FEAT_QUARTZ_H))
			{
				/* Expose the gold */
				c_ptr->feat += 0x02;
			}

			/* Magma/Quartz + Known Gold */
			if ((c_ptr->feat == FEAT_MAGMA_K) ||
			    (c_ptr->feat == FEAT_QUARTZ_K))
			{
				/* Hack -- Memorize */
				c_ptr->info |= (CAVE_MARK);

				/* Redraw */
				lite_spot(y, x);

				/* Detect */
				detect = TRUE;
			}
		}
	}

	/* Describe */
	if (detect_monsters_string("*"))
	{
		detect = TRUE;
	}
	else if (detect)
	{
		msg_print("You sense the presence of buried treasure!");
	}

	/* Result */
	return (detect);
}



/*
 * Detect all "gold" objects on the current panel
 */
bool detect_objects_gold(void)
{
	int i, y, x;

	bool detect = FALSE;


	/* Scan objects */
	for (i = 1; i < o_max; i++)
	{
		object_type *o_ptr = &o_list[i];

		/* Skip dead objects */
		if (!o_ptr->k_idx) continue;

		/* Skip held objects */
		if (o_ptr->held_m_idx) continue;

		/* Location */
		y = o_ptr->iy;
		x = o_ptr->ix;

		/* Only detect nearby objects */
		if (!panel_contains(y, x)) continue;
		
		/* Detect "gold" objects */
		if (o_ptr->tval == TV_GOLD)
		{
			/* Hack -- memorize it */
			o_ptr->marked = TRUE;

			/* Redraw */
			lite_spot(y, x);

			/* Detect */
			detect = TRUE;
		}
	}

	/* Describe */
	if (detect_monsters_string("$"))
	{
		detect = TRUE;
	}
	else if (detect)
	{
		msg_print("You sense the presence of treasure!");
	}

	/* Result */
	return (detect);
}


/*
 * Detect all "normal" objects on the current panel
 */
bool detect_objects_normal(void)
{
	int i, y, x;

	bool detect = FALSE;


	/* Scan objects */
	for (i = 1; i < o_max; i++)
	{
		object_type *o_ptr = &o_list[i];

		/* Skip dead objects */
		if (!o_ptr->k_idx) continue;

		/* Skip held objects */
		if (o_ptr->held_m_idx) continue;

		/* Location */
		y = o_ptr->iy;
		x = o_ptr->ix;

		/* Only detect nearby objects */
		if (!panel_contains(y, x)) continue;
		
		/* Detect "real" objects */
		if (o_ptr->tval != TV_GOLD)
		{
			/* Hack -- memorize it */
			o_ptr->marked = TRUE;

			/* Redraw */
			lite_spot(y, x);

			/* Detect */
			detect = TRUE;
		}
	}

	/* Describe */
	if (detect_monsters_string(",!=?|"))
	{
		detect = TRUE;
	}
	else if (detect)
	{
		msg_print("You sense the presence of objects!");
	}

	/* Result */
	return (detect);
}


/*
 * Detect all "magic" objects on the current panel.
 *
 * This will light up all spaces with "magic" items, including artifacts,
 * ego-items, potions, scrolls, books, rods, wands, staves, amulets, rings,
 * and "enchanted" items of the "good" variety.
 *
 * It can probably be argued that this function is now too powerful.
 */
bool detect_objects_magic(void)
{
	int i, y, x, tv;
	bool detect = FALSE;

	/* Scan all objects */
	for (i = 1; i < o_max; i++)
	{
		object_type *o_ptr = &o_list[i];
		
		/* Skip dead objects */
		if (!o_ptr->k_idx) continue;

		/* Skip held objects */
		if (o_ptr->held_m_idx) continue;

		/* Location */
		y = o_ptr->iy;
		x = o_ptr->ix;

		/* Only detect nearby objects */
		if (!panel_contains(y,x)) continue;
		
		/* Examine the tval */
		tv = o_ptr->tval;

		/* Artifacts, misc magic items, or enchanted wearables */
		if (artifact_p(o_ptr) || ego_item_p(o_ptr) || o_ptr->art_name ||
		    (tv == TV_AMULET) || (tv == TV_RING) ||
		    (tv == TV_STAFF) || (tv == TV_WAND) || (tv == TV_ROD) ||
		    (tv == TV_SCROLL) || (tv == TV_POTION) ||
		    (tv == TV_LIFE_BOOK) || (tv == TV_SORCERY_BOOK) ||
		    (tv == TV_NATURE_BOOK) || (tv == TV_CHAOS_BOOK) ||
		    (tv == TV_DEATH_BOOK) ||
		    (tv == TV_TRUMP_BOOK) || (tv == TV_ARCANE_BOOK) ||
		    ((o_ptr->to_a > 0) || (o_ptr->to_h + o_ptr->to_d > 0)))
		{
			/* Memorize the item */
			o_ptr->marked = TRUE;

			/* Redraw */
			lite_spot(y, x);

			/* Detect */
			detect = TRUE;
		}
	}

	/* Describe */
	if (detect_monsters_string(",!=?|"))
	{
		detect = TRUE;
	}
	else if (detect)
	{
		msg_print("You sense the presence of magic objects!");
	}

	/* Return result */
	return (detect);
}


/*
 * Detect all "normal" monsters on the current panel
 */
bool detect_monsters_normal(void)
{
	int i, y, x;
	bool flag = FALSE;


	/* Scan monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Skip object mimics */
		if ((r_ptr->d_char == '<') || (r_ptr->d_char == '>') ||
		    (r_ptr->d_char == '$') || (r_ptr->d_char == '*') ||
		    (r_ptr->d_char == ',') ||
		    (r_ptr->d_char == '!') || (r_ptr->d_char == '=') ||
		    (r_ptr->d_char == '?') || (r_ptr->d_char == '|')) continue;

		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;

		/* Only detect nearby monsters */
		if (!panel_contains(y, x)) continue;
		
		/* Detect all non-invisible monsters */
		if ((!(r_ptr->flags2 & (RF2_INVISIBLE))) ||
		    p_ptr->see_inv || p_ptr->tim_invis)
		{
			/* Repair visibility later */
			repair_monsters = TRUE;

			/* Hack -- Detect monster */
			m_ptr->mflag |= (MFLAG_MARK | MFLAG_SHOW);

			/* Hack -- See monster */
			m_ptr->ml = TRUE;
			
			/* Redraw */
			lite_spot(y, x);

			/* Detect */
			flag = TRUE;
		}
	}

	/* Describe */
	if (flag)
	{
		/* Describe result */
		msg_print("You sense the presence of monsters!");
	}

	/* Result */
	return (flag);
}


/*
 * Detect all monsters with minds on current panel -- Gumby
 */
bool detect_monsters_mental(void)
{
	int i, y, x;
	bool flag = FALSE;

	/* Scan monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/*
		 * Skip dead and mindless monsters. Only sometimes skip
		 * those with erratic minds or those who are stupid.
		 */
		if (!m_ptr->r_idx)			continue;
		if (r_ptr->flags2 & (RF2_EMPTY_MIND))	continue;
		if ((r_ptr->flags2 & (RF2_STUPID)) &&
		    (randint(100) > (p_ptr->lev * 2)))	continue;
		if ((r_ptr->flags2 & (RF2_WEIRD_MIND)) &&
		    (randint(100) > (p_ptr->lev * 2)))	continue;

		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;

		/* Only detect nearby monsters */
		if (!panel_contains(y, x)) continue;

		/* Repair visibility later */
		repair_monsters = TRUE;

		/* Hack -- Detect monster */
		m_ptr->mflag |= (MFLAG_MARK | MFLAG_SHOW);

		/* Hack -- See monster */
		m_ptr->ml = TRUE;
			
		/* Redraw */
		lite_spot(y, x);

		/* Detect */
		flag = TRUE;
	}

	/* Describe */
	if (flag)
	{
		/* Describe result */
		msg_print("You sense the presence of mental energy!");
	}

	/* Result */
	return (flag);
}


/*
 * Detect all "invisible" monsters on current panel
 */
bool detect_monsters_invis(void)
{
	int i, y, x;
	bool flag = FALSE;

	/* Scan monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Skip object mimics */
		if ((r_ptr->d_char == '<') || (r_ptr->d_char == '>') ||
		    (r_ptr->d_char == '$') || (r_ptr->d_char == '*') ||
		    (r_ptr->d_char == ',') ||
		    (r_ptr->d_char == '!') || (r_ptr->d_char == '=') ||
		    (r_ptr->d_char == '?') || (r_ptr->d_char == '|')) continue;

		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;

		/* Only detect nearby monsters */
		if (!panel_contains(y, x)) continue;
		
		/* Detect invisible monsters */
		if (r_ptr->flags2 & (RF2_INVISIBLE))
		{
			/* Take note that they are invisible */
			r_ptr->r_flags2 |= (RF2_INVISIBLE);

			/* Update monster recall window */
			if (monster_race_idx == m_ptr->r_idx)
			{
				/* Window stuff */
				p_ptr->window |= (PW_MONSTER);
			}

			/* Repair visibility later */
			repair_monsters = TRUE;

			/* Hack -- Detect monster */
			m_ptr->mflag |= (MFLAG_MARK | MFLAG_SHOW);

			/* Hack -- See monster */
			m_ptr->ml = TRUE;
			
			/* Redraw */
			lite_spot(y, x);

			/* Detect */
			flag = TRUE;
		}
	}

	/* Describe */
	if (flag)
	{
		/* Describe result */
		msg_print("You sense the presence of invisible creatures!");
	}

	/* Result */
	return (flag);
}


/*
 * Detect all "evil" monsters on current panel
 */
bool detect_monsters_evil(void)
{
	int i, y, x;
	bool flag = FALSE;


	/* Scan monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Skip object mimics */
		if ((r_ptr->d_char == '<') || (r_ptr->d_char == '>') ||
		    (r_ptr->d_char == '$') || (r_ptr->d_char == '*') ||
		    (r_ptr->d_char == ',') ||
		    (r_ptr->d_char == '!') || (r_ptr->d_char == '=') ||
		    (r_ptr->d_char == '?') || (r_ptr->d_char == '|')) continue;

		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;

		/* Only detect nearby monsters */
		if (!panel_contains(y, x)) continue;
		
		/* Detect evil monsters */
		if (r_ptr->flags3 & (RF3_EVIL))
		{
			/* Take note that they are evil */
			r_ptr->r_flags3 |= (RF3_EVIL);

			/* Update monster recall window */
			if (monster_race_idx == m_ptr->r_idx)
			{
				/* Window stuff */
				p_ptr->window |= (PW_MONSTER);
			}

			/* Repair visibility later */
			repair_monsters = TRUE;

			/* Hack -- Detect monster */
			m_ptr->mflag |= (MFLAG_MARK | MFLAG_SHOW);

			/* Hack -- See monster */
			m_ptr->ml = TRUE;
			
			/* Redraw */
			lite_spot(y, x);

			/* Detect */
			flag = TRUE;
		}
	}

	/* Describe */
	if (flag)
	{
		/* Describe result */
		msg_print("You sense the presence of evil creatures!");
	}

	/* Result */
	return (flag);
}




/*
 * Detect all (string) monsters on current panel
 */
bool detect_monsters_string(cptr Match)
{
	int i, y, x;

	bool flag = FALSE;
	cptr desc_monsters = "weird monsters";

	/* Scan monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;

		/* Only detect nearby monsters */
		if (!panel_contains(y, x)) continue;
		
		/* Detect evil monsters */
		if (strchr(Match, r_ptr->d_char))
		{

			/* Update monster recall window */
			if (monster_race_idx == m_ptr->r_idx)
			{
				/* Window stuff */
				p_ptr->window |= (PW_MONSTER);
			}

			/* Repair visibility later */
			repair_monsters = TRUE;

			/* Hack -- Detect monster */
			m_ptr->mflag |= (MFLAG_MARK | MFLAG_SHOW);

			/* Hack -- See monster */
			m_ptr->ml = TRUE;
			
			/* Redraw */
			lite_spot(y, x);

			/* Detect */
			if ((r_ptr->d_char == '<') || (r_ptr->d_char == '>'))
			{
				desc_monsters = "stairs";
				flag = TRUE;
			}
			else if (r_ptr->d_char == '$')
			{
				desc_monsters = "treasure";
				flag = TRUE;
			}
			else if (r_ptr->d_char == '*')
			{
				desc_monsters = "buried treasure";
				flag = TRUE;
			}
			else if ((r_ptr->d_char == ',') ||
				 (r_ptr->d_char == '!') ||
				 (r_ptr->d_char == '=') ||
				 (r_ptr->d_char == '?') ||
				 (r_ptr->d_char == '|'))
			{
				desc_monsters = "objects";
				flag = TRUE;
			}
			else
			{
				desc_monsters = "monsters";
				flag = TRUE;
			}
		}
	}

	/* Describe */
	if (flag)
	{
		msg_format("You sense the presence of %s!", desc_monsters);
	}

	/* Result */
	return (flag);
}


/*
 * A "generic" detect monsters routine, tagged to flags3
 */
bool detect_monsters_xxx(u32b match_flag)
{
	int  i, y, x;
	bool flag = FALSE;
	cptr desc_monsters = "weird monsters";


	/* Scan monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Skip object mimics */
		if ((r_ptr->d_char == '<') || (r_ptr->d_char == '>') ||
		    (r_ptr->d_char == '$') || (r_ptr->d_char == '*') ||
		    (r_ptr->d_char == ',') ||
		    (r_ptr->d_char == '!') || (r_ptr->d_char == '=') ||
		    (r_ptr->d_char == '?') || (r_ptr->d_char == '|')) continue;

		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;

		/* Only detect nearby monsters */
		if (!panel_contains(y, x)) continue;
		
		/* Detect evil monsters */
		if (r_ptr->flags3 & (match_flag))
		{
			/* Take note that they are something */
			r_ptr->r_flags3 |= (match_flag);

			/* Update monster recall window */
			if (monster_race_idx == m_ptr->r_idx)
			{
				/* Window stuff */
				p_ptr->window |= (PW_MONSTER);
			}

			/* Repair visibility later */
			repair_monsters = TRUE;

			/* Hack -- Detect monster */
			m_ptr->mflag |= (MFLAG_MARK | MFLAG_SHOW);

			/* Hack -- See monster */
			m_ptr->ml = TRUE;
			
			/* Redraw */
			lite_spot(y, x);

			/* Detect */
			flag = TRUE;
		}
	}

	/* Describe */
	if (flag)
	{
		switch (match_flag)
		{
			case RF3_ORC:
				desc_monsters = "orcs";
				break;
			case RF3_TROLL:
				desc_monsters = "trolls";
				break;
			case RF3_GIANT:
				desc_monsters = "giants";
				break;
			case RF3_DRAGON:
				desc_monsters = "dragons";
				break;
			case RF3_DEMON:
				desc_monsters = "demons";
				break;
			case RF3_UNDEAD:
				desc_monsters = "the undead";
				break;
			case RF3_ANIMAL:
				desc_monsters = "animals";
				break;
			case RF3_ELEMENTAL:
				desc_monsters = "elementals";
				break;
			case RF3_GOOD:
				desc_monsters = "nice creatures";
				break;
		}

		/* Describe result */
		msg_format("You sense the presence of %s!", desc_monsters);
		msg_print(NULL);
	}

	/* Result */
	return (flag);
}


/*
 * Detect everything
 */
bool detect_all(void)
{
	bool detect = FALSE;

	/* Detect everything */
	if (detect_traps()) detect = TRUE;
	if (detect_doors()) detect = TRUE;
	if (detect_stairs()) detect = TRUE;
	if (detect_treasure()) detect = TRUE;
	if (detect_objects_gold()) detect = TRUE;
	if (detect_objects_normal()) detect = TRUE;
	if (detect_monsters_invis()) detect = TRUE;
	if (detect_monsters_normal()) detect = TRUE;
	
	/* Result */
	return (detect);
}


/*
 * Create stairs at the player location
 */
void stair_creation(void)
{
	/* XXX XXX XXX */
	if (!cave_valid_bold(py, px))
	{
		msg_print("The object resists the spell.");
		return;
	}

	/* XXX XXX XXX */
	delete_object(py, px);

	/* Create a staircase */
	if (!dun_level)
	{
		cave_set_feat(py, px, FEAT_MORE);
	}
	else if (is_quest(dun_level, FALSE) || (dun_level >= MAX_DEPTH-1))
	{
		cave_set_feat(py, px, FEAT_LESS);
	}
	else if (p_ptr->astral && (dun_level == 97))
	{
		cave_set_feat(py, px, FEAT_LESS);
	}
	else if (rand_int(100) < 50)
	{
		cave_set_feat(py, px, FEAT_MORE);
	}
	else
	{
		cave_set_feat(py, px, FEAT_LESS);
	}
}


/*
 * Apply a "project()" directly to all viewable monsters
 *
 * Note that affected monsters are NOT auto-tracked by this usage.
 */
static bool project_hack(int typ, int dam)
{
	int     i, x, y;
	int     flg = PROJECT_JUMP | PROJECT_KILL | PROJECT_HIDE;
	bool    obvious = FALSE;

	/* Affect all (nearby) monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;

		/* Require line of sight */
		if (!player_has_los_bold(y, x)) continue;

		/* Jump directly to the target monster */
		if (project(0, FALSE, 0, y, x, dam, typ, flg, FALSE))
			obvious = TRUE;
	}

	/* Result */
	return (obvious);
}


/* Speed monsters */
bool speed_monsters(void)
{
	return (project_hack(GF_OLD_SPEED, p_ptr->lev));
}

/* Slow monsters */
bool slow_monsters(void)
{
	return (project_hack(GF_OLD_SLOW, p_ptr->lev));
}

/* Sleep monsters */
bool sleep_monsters(void)
{
	return (project_hack(GF_OLD_SLEEP, p_ptr->lev));
}

/* Teleport away all evil monsters */
bool banish_evil(int dist)
{
	return (project_hack(GF_AWAY_EVIL, dist));
}

/* Turn (scare) undead */
bool turn_undead(void)
{
	return (project_hack(GF_TURN_UNDEAD, p_ptr->lev));
}

/* Dispel undead monsters */
bool dispel_undead(int dam)
{
	return (project_hack(GF_DISP_UNDEAD, dam));
}

/* Dispel evil monsters */
bool dispel_evil(int dam)
{
	return (project_hack(GF_DISP_EVIL, dam));
}

/* Dispel good monsters */
bool dispel_good(int dam)
{
    return (project_hack(GF_DISP_GOOD, dam));
}

/* Dispel all monsters */
bool dispel_monsters(int dam)
{
	return (project_hack(GF_DISP_ALL, dam));
}

/* Dispel 'living' monsters */
bool dispel_living(int dam)
{
	return (project_hack(GF_DISP_LIVING, dam));
}

/* Dispel demons */
bool dispel_demons(int dam)
{
	return (project_hack(GF_DISP_DEMON, dam));
}

/* Dispel animals */
bool dispel_animals(int dam)
{
	return (project_hack(GF_DISP_ANIMAL, dam));
}

/* Polymorph All */
bool poly_all(void)
{
	return (project_hack(GF_OLD_POLY, p_ptr->lev));
}

/* Wake up all monsters, and speed up "los" monsters. */
void aggravate_monsters(int who, bool the_entire_level)
{
	int i;

	bool sleep = FALSE;
	bool speed = FALSE;

	/* Aggravate everyone nearby */
	for (i = 1; i < m_max; i++)
	{
		monster_type    *m_ptr = &m_list[i];
		monster_race    *r_ptr = &r_info[m_ptr->r_idx];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Skip aggravating monster (or player) */
		if (i == who) continue;

		/* Wake up and hasten all monsters. No additional messages. -LM- */
		if (the_entire_level)
		{
			/* Wake up */
			if (m_ptr->csleep)
			{
				/* Wake up */
				m_ptr->csleep = 0;
			}

			/* Get mad. */
			if (m_ptr->mspeed < r_ptr->speed + 10)
			{
				m_ptr->mspeed = r_ptr->speed + 10;
			}

			if (randint(2)==1)
			{
				m_ptr->smart &= ~SM_FRIEND;
			}
			else
			{
				p_ptr->pet_follow_distance = 255;
			}
		}
		else
		{
			/* Wake up nearby sleeping monsters */
			if (m_ptr->cdis < MAX_SIGHT * 2)
			{
				/* Wake up */
				if (m_ptr->csleep)
				{
					/* Wake up */
					m_ptr->csleep = 0;
					sleep = TRUE;
				}
			}

			/* Speed up monsters in line of sight */
			if (player_has_los_bold(m_ptr->fy, m_ptr->fx))
			{
				/* Speed up (instantly) to racial base + 10 */
				if (m_ptr->mspeed < r_ptr->speed + 10)
				{
					/* Speed up */
					m_ptr->mspeed = r_ptr->speed + 10;
					speed = TRUE;
				}

				if (m_ptr->smart & SM_FRIEND)
				{
					if (randint(2)==1)
					{
						m_ptr->smart &= ~SM_FRIEND;
					}
					else
					{
						p_ptr->pet_follow_distance = 255;
					}
				}
			}
		}
	}

	/* Messages */
	if (speed) msg_print("You feel a sudden stirring nearby!");
	else if (sleep) msg_print("You hear a sudden stirring in the distance!");
}


/*
 * The spell of destruction
 *
 * This spell "deletes" monsters (instead of "killing" them).
 *
 * Later we may use one function for both "destruction" and
 * "earthquake" by using the "full" to select "destruction".
 */
void destroy_area(int y1, int x1, int r, bool full)
{
	int y, x, k, t;
	cave_type *c_ptr;
	bool flag = FALSE;

	/* XXX XXX */
	full = full ? full : 0;

	/* Big area of affect */
	for (y = (y1 - r); y <= (y1 + r); y++)
	{
		for (x = (x1 - r); x <= (x1 + r); x++)
		{
			/* Skip illegal grids */
			if (!in_bounds(y, x)) continue;

			/* Extract the distance */
			k = distance(y1, x1, y, x);

			/* Stay in the circle of death */
			if (k > r) continue;

			/* Access the grid */
			c_ptr = &cave[y][x];

			/* Lose room and vault */
			c_ptr->info &= ~(CAVE_ROOM | CAVE_ICKY);

			/* Hack -- Notice player affect */
			if ((x == px) && (y == py))
			{
				/* Hurt the player later - was TRUE */
				flag = TRUE;

				/* Do not hurt this grid */
				continue;
			}

			/* Hack -- Skip the epicenter */
			if ((y == y1) && (x == x1)) continue;

			/* Delete the monster (if any) */
			delete_monster(y, x);

			/* Destroy "valid" grids */
			if (cave_valid_bold(y, x))
			{
				/* Delete objects */
				delete_object(y, x);

				/* Wall (or floor) type */
				t = rand_int(200);

				/* Granite */
				if (t < 20)
				{
					/* Create granite wall */
					c_ptr->feat = FEAT_WALL_EXTRA;
				}

				/* Quartz */
				else if (t < 70)
				{
					/* Create quartz vein */
					c_ptr->feat = FEAT_QUARTZ;
				}

				/* Magma */
				else if (t < 100)
				{
					/* Create magma vein */
					c_ptr->feat = FEAT_MAGMA;
				}

				/* Floor */
				else
				{
					/* Create floor */
					c_ptr->feat = FEAT_FLOOR;
				}
			}
		}
	}


	/* Hack -- Affect player */
	if (flag)
	{
		/* Message */
		msg_print("There is a searing blast of light!");

		/* Blind the player */
		if (!p_ptr->resist_blind && !p_ptr->resist_lite)
		{
			/* Become blind */
			(void)set_blind(p_ptr->blind + 10 + randint(10));
		}
	}


	/* Mega-Hack -- Forget the view and lite */
	p_ptr->update |= (PU_UN_VIEW | PU_UN_LITE);

	/* Update stuff */
	p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW);

	/* Update the monsters */
	p_ptr->update |= (PU_MONSTERS);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD);
}


/*
 * Induce an "earthquake" of the given radius at the given location.
 *
 * This will turn some walls into floors and some floors into walls.
 *
 * The player will take damage and "jump" into a safe grid if possible,
 * otherwise, he will "tunnel" through the rubble instantaneously.
 *
 * Monsters will take damage, and "jump" into a safe grid if possible,
 * otherwise they will be "buried" in the rubble, disappearing from
 * the level in the same way that they do when genocided.
 *
 * Note that thus the player and monsters (except eaters of walls and
 * passers through walls) will never occupy the same grid as a wall.
 * Note that as of now (2.7.8) no monster may occupy a "wall" grid, even
 * for a single turn, unless that monster can pass_walls or kill_walls.
 * This has allowed massive simplification of the "monster" code.
 */
void earthquake(int cy, int cx, int r)
{
	int             i, t, y, x, yy, xx, dy, dx, oy, ox;
	int             damage = 0;
	int             sn = 0, sy = 0, sx = 0;
	bool            hurt = FALSE;
	cave_type       *c_ptr;
	bool            map[32][32];


	/* Paranoia -- Enforce maximum range */
	if (r > 12) r = 12;

	/* Clear the "maximal blast" area */
	for (y = 0; y < 32; y++)
	{
		for (x = 0; x < 32; x++)
		{
			map[y][x] = FALSE;
		}
	}

	/* Check around the epicenter */
	for (dy = -r; dy <= r; dy++)
	{
		for (dx = -r; dx <= r; dx++)
		{
			/* Extract the location */
			yy = cy + dy;
			xx = cx + dx;

			/* Skip illegal grids */
			if (!in_bounds(yy, xx)) continue;

			/* Skip distant grids */
			if (distance(cy, cx, yy, xx) > r) continue;

			/* Access the grid */
			c_ptr = &cave[yy][xx];

			/* Lose room and vault */
			c_ptr->info &= ~(CAVE_ROOM | CAVE_ICKY);

			/* Skip the epicenter */
			if (!dx && !dy) continue;

			/* Skip most grids */
			if (rand_int(100) < 85) continue;

			/* Damage this grid */
			map[16+yy-cy][16+xx-cx] = TRUE;

			/* Hack -- Take note of player damage */
			if ((yy == py) && (xx == px)) hurt = TRUE;
		}
	}

	/* First, affect the player (if necessary) */
	if (hurt)
	{
		/* Check around the player */
		for (i = 0; i < 8; i++)
		{
			/* Access the location */
			y = py + ddy[i];
			x = px + ddx[i];

			/* Skip non-empty grids */
			if (!cave_empty_bold(y, x)) continue;

			/* Important -- Skip "quake" grids */
			if (map[16+y-cy][16+x-cx]) continue;

			/* Count "safe" grids */
			sn++;

			/* Randomize choice */
			if (rand_int(sn) > 0) continue;

			/* Save the safe location */
			sy = y; sx = x;
		}

		/* Random message */
		switch (randint(3))
		{
			case 1:
			{
				msg_print("The cave ceiling collapses!");
				break;
			}
			case 2:
			{
				msg_print("The cave floor twists in an unnatural way!");
				break;
			}
			default:
			{
				msg_print("The cave quakes!  You are pummeled with debris!");
				break;
			}
		}

		/* Hurt the player a lot */
		if (!sn)
		{
			/* Message and damage */
			msg_print("You are severely crushed!");
			damage = 300;
		}

		/* Destroy the grid, and push the player to safety */
		else
		{
			/* Calculate results */
			switch (randint(3))
			{
				case 1:
				{
					msg_print("You nimbly dodge the blast!");
					damage = 0;
					break;
				}
				case 2:
				{
					msg_print("You are bashed by rubble!");
					damage = damroll(10, 4);
					(void)set_stun(p_ptr->stun + randint(50));
					break;
				}
				case 3:
				{
					msg_print("You are crushed between the floor and ceiling!");
					damage = damroll(10, 4);
					(void)set_stun(p_ptr->stun + randint(50));
					break;
				}
			}

			/* Save the old location */
			oy = py;
			ox = px;

			/* Move the player to the safe location */
			py = sy;
			px = sx;

			/* Redraw the old spot */
			lite_spot(oy, ox);

			/* Redraw the new spot */
			lite_spot(py, px);

			/* Check for new panel */
			verify_panel();
		}

		/* Important -- no wall on player */
		map[16+py-cy][16+px-cx] = FALSE;

		/* Take some damage */
		if (damage) take_hit(damage, "an earthquake");
	}


	/* Examine the quaked region */
	for (dy = -r; dy <= r; dy++)
	{
		for (dx = -r; dx <= r; dx++)
		{
			/* Extract the location */
			yy = cy + dy;
			xx = cx + dx;

			/* Skip unaffected grids */
			if (!map[16+yy-cy][16+xx-cx]) continue;

			/* Access the grid */
			c_ptr = &cave[yy][xx];

			/* Process monsters */
			if (c_ptr->m_idx)
			{
				monster_type *m_ptr = &m_list[c_ptr->m_idx];
				monster_race *r_ptr = &r_info[m_ptr->r_idx];

				/* Most monsters cannot co-exist with rock */
				if (!(r_ptr->flags2 & (RF2_KILL_WALL)) &&
				    !(r_ptr->flags2 & (RF2_PASS_WALL)))
				{
					char m_name[80];

					/* Assume not safe */
					sn = 0;

					/* Monster can move to escape the wall */
					if (!(r_ptr->flags1 & (RF1_NEVER_MOVE)))
					{
						/* Look for safety */
						for (i = 0; i < 8; i++)
						{
							/* Access the grid */
							y = yy + ddy[i];
							x = xx + ddx[i];

							/* Skip non-empty grids */
							if (!cave_empty_bold(y, x)) continue;

							/* Hack -- no safety on glyph of warding */
							if (cave[y][x].feat == FEAT_GLYPH) continue;
							if (cave[y][x].feat == FEAT_MINOR_GLYPH) continue;

							/* Important -- Skip "quake" grids */
							if (map[16+y-cy][16+x-cx]) continue;

							/* Count "safe" grids */
							sn++;

							/* Randomize choice */
							if (rand_int(sn) > 0) continue;

							/* Save the safe grid */
							sy = y; sx = x;
						}
					}

					/* Describe the monster */
					monster_desc(m_name, m_ptr, 0);

					/* Scream in pain */
					msg_format("%^s wails out in pain!", m_name);

					/* Take damage from the quake */
					damage = (sn ? damroll(4, 8) : 200);

					/* Monster is certainly awake */
					m_ptr->csleep = 0;

					/* Apply damage directly */
					m_ptr->hp -= damage;

					/* Delete (not kill) "dead" monsters */
					if (m_ptr->hp < 0)
					{
						/* Message */
						msg_format("%^s is embedded in the rock!", m_name);

						/* Delete the monster */
						delete_monster(yy, xx);

						/* No longer safe */
						sn = 0;
					}

					/* Hack -- Escape from the rock */
					if (sn)
					{
						int m_idx = cave[yy][xx].m_idx;

						/* Update the new location */
						cave[sy][sx].m_idx = m_idx;

						/* Update the old location */
						cave[yy][xx].m_idx = 0;

						/* Move the monster */
						m_ptr->fy = sy;
						m_ptr->fx = sx;

						/* Update the monster (new location) */
						update_mon(m_idx, TRUE);

						/* Redraw the old grid */
						lite_spot(yy, xx);

						/* Redraw the new grid */
						lite_spot(sy, sx);
					}
				}
			}
		}
	}


	/* Examine the quaked region */
	for (dy = -r; dy <= r; dy++)
	{
		for (dx = -r; dx <= r; dx++)
		{
			/* Extract the location */
			yy = cy + dy;
			xx = cx + dx;

			/* Skip unaffected grids */
			if (!map[16+yy-cy][16+xx-cx]) continue;

			/* Access the cave grid */
			c_ptr = &cave[yy][xx];

			/* Paranoia -- never affect player */
			if ((yy == py) && (xx == px)) continue;

			/* Destroy location (if valid) */
			if (cave_valid_bold(yy, xx))
			{
				bool floor = cave_floor_bold(yy, xx);

				/* Delete objects */
				delete_object(yy, xx);

				/* Wall (or floor) type */
				t = (floor ? rand_int(100) : 200);

				/* Granite */
				if (t < 20)
				{
					/* Create granite wall */
					c_ptr->feat = FEAT_WALL_EXTRA;
				}

				/* Quartz */
				else if (t < 70)
				{
					/* Create quartz vein */
					c_ptr->feat = FEAT_QUARTZ;
				}

				/* Magma */
				else if (t < 100)
				{
					/* Create magma vein */
					c_ptr->feat = FEAT_MAGMA;
				}

				/* Floor */
				else
				{
					/* Create floor */
					c_ptr->feat = FEAT_FLOOR;
				}
			}
		}
	}


	/* Mega-Hack -- Forget the view and lite */
	p_ptr->update |= (PU_UN_VIEW | PU_UN_LITE);

	/* Update stuff */
	p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW);

	/* Update the monsters */
	p_ptr->update |= (PU_DISTANCE);

	/* Update the health bar */
	p_ptr->redraw |= (PR_HEALTH);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD);
}



/*
 * This routine clears the entire "temp" set.
 *
 * This routine will Perma-Lite all "temp" grids.
 *
 * This routine is used (only) by "lite_room()"
 *
 * Dark grids are illuminated.
 *
 * Also, process all affected monsters.
 *
 * SMART monsters always wake up when illuminated
 * NORMAL monsters wake up 1/4 the time when illuminated
 * STUPID monsters wake up 1/10 the time when illuminated
 */
static void cave_temp_room_lite(void)
{
	int i;

	/* Clear them all */
	for (i = 0; i < temp_n; i++)
	{
		int y = temp_y[i];
		int x = temp_x[i];

		cave_type *c_ptr = &cave[y][x];

		/* No longer in the array */
		c_ptr->info &= ~(CAVE_TEMP);

		/* Update only non-CAVE_GLOW grids */
		/* if (c_ptr->info & (CAVE_GLOW)) continue; */

		/* Perma-Lite */
		c_ptr->info |= (CAVE_GLOW);

		/* Process affected monsters */
		if (c_ptr->m_idx)
		{
			int chance = 25;

			monster_type    *m_ptr = &m_list[c_ptr->m_idx];

			monster_race    *r_ptr = &r_info[m_ptr->r_idx];

			/* Update the monster */
			update_mon(c_ptr->m_idx, FALSE);

			/* Stupid monsters rarely wake up */
			if (r_ptr->flags2 & (RF2_STUPID)) chance = 10;

			/* Smart monsters always wake up */
			if (r_ptr->flags2 & (RF2_SMART)) chance = 100;

			/* Sometimes monsters wake up */
			if (m_ptr->csleep && (rand_int(100) < chance))
			{
				/* Wake up! */
				m_ptr->csleep = 0;

				/* Notice the "waking up" */
				if (m_ptr->ml)
				{
					char m_name[80];

					/* Acquire the monster name */
					monster_desc(m_name, m_ptr, 0);

					/* Dump a message */
					msg_format("%^s wakes up.", m_name);
				}
			}
		}

		/* Note */
		note_spot(y, x);

		/* Redraw */
		lite_spot(y, x);
	}

	/* None left */
	temp_n = 0;
}



/*
 * This routine clears the entire "temp" set.
 *
 * This routine will "darken" all "temp" grids.
 *
 * In addition, some of these grids will be "unmarked".
 *
 * This routine is used (only) by "unlite_room()"
 *
 * Also, process all affected monsters
 */
static void cave_temp_room_unlite(void)
{
	int i;

	/* Clear them all */
	for (i = 0; i < temp_n; i++)
	{
		int y = temp_y[i];
		int x = temp_x[i];

		cave_type *c_ptr = &cave[y][x];

		/* No longer in the array */
		c_ptr->info &= ~(CAVE_TEMP);

		/* Darken the grid */
		c_ptr->info &= ~(CAVE_GLOW);

		/* Hack -- Forget "boring" grids */
		if (c_ptr->feat <= FEAT_INVIS)
		{
			/* Forget the grid */
			c_ptr->info &= ~(CAVE_MARK);

			/* Notice */
			note_spot(y, x);
		}

		/* Process affected monsters */
		if (c_ptr->m_idx)
		{
			/* Update the monster */
			update_mon(c_ptr->m_idx, FALSE);
		}

		/* Redraw */
		lite_spot(y, x);
	}

	/* None left */
	temp_n = 0;
}




/*
 * Aux function -- see below
 */
static void cave_temp_room_aux(int y, int x)
{
	cave_type *c_ptr = &cave[y][x];

	/* Avoid infinite recursion */
	if (c_ptr->info & (CAVE_TEMP)) return;

	/* Do not "leave" the current room */
	if (!(c_ptr->info & (CAVE_ROOM))) return;

	/* Paranoia -- verify space */
	if (temp_n == TEMP_MAX) return;

	/* Mark the grid as "seen" */
	c_ptr->info |= (CAVE_TEMP);

	/* Add it to the "seen" set */
	temp_y[temp_n] = y;
	temp_x[temp_n] = x;
	temp_n++;
}




/*
 * Illuminate any room containing the given location.
 */
void lite_room(int y1, int x1)
{
	int i, x, y;

	/* Add the initial grid */
	cave_temp_room_aux(y1, x1);

	/* While grids are in the queue, add their neighbors */
	for (i = 0; i < temp_n; i++)
	{
		x = temp_x[i], y = temp_y[i];

		/* Walls get lit, but stop light */
		if (!cave_floor_bold(y, x)) continue;

		/* Spread adjacent */
		cave_temp_room_aux(y + 1, x);
		cave_temp_room_aux(y - 1, x);
		cave_temp_room_aux(y, x + 1);
		cave_temp_room_aux(y, x - 1);

		/* Spread diagonal */
		cave_temp_room_aux(y + 1, x + 1);
		cave_temp_room_aux(y - 1, x - 1);
		cave_temp_room_aux(y - 1, x + 1);
		cave_temp_room_aux(y + 1, x - 1);
	}

	/* Now, lite them all up at once */
	cave_temp_room_lite();
}


/*
 * Darken all rooms containing the given location
 */
void unlite_room(int y1, int x1)
{
	int i, x, y;

	/* Add the initial grid */
	cave_temp_room_aux(y1, x1);

	/* Spread, breadth first */
	for (i = 0; i < temp_n; i++)
	{
		x = temp_x[i], y = temp_y[i];

		/* Walls get dark, but stop darkness */
		if (!cave_floor_bold(y, x)) continue;

		/* Spread adjacent */
		cave_temp_room_aux(y + 1, x);
		cave_temp_room_aux(y - 1, x);
		cave_temp_room_aux(y, x + 1);
		cave_temp_room_aux(y, x - 1);

		/* Spread diagonal */
		cave_temp_room_aux(y + 1, x + 1);
		cave_temp_room_aux(y - 1, x - 1);
		cave_temp_room_aux(y - 1, x + 1);
		cave_temp_room_aux(y + 1, x - 1);
	}

	/* Now, darken them all at once */
	cave_temp_room_unlite();
}



/*
 * Hack -- call light around the player
 * Affect all monsters in the projection radius
 */
bool lite_area(int dam, int rad)
{
	int flg = PROJECT_GRID | PROJECT_KILL;

	/* Hack -- Message */
	if (!p_ptr->blind)
	{
		msg_print("You are surrounded by a white light.");
	}

	/* Hook into the "project()" function */
	(void)project(0, FALSE, rad, py, px, dam, GF_LITE_WEAK, flg, FALSE);

	/* Lite up the room */
	lite_room(py, px);

	/* Assume seen */
	return (TRUE);
}


/*
 * Hack -- call darkness around the player
 * Affect all monsters in the projection radius
 */
bool unlite_area(int dam, int rad)
{
	int flg = PROJECT_GRID | PROJECT_KILL;

	/* Hack -- Message */
	if (!p_ptr->blind)
	{
		msg_print("Darkness surrounds you.");
	}

	/* Hook into the "project()" function */
	(void)project(0, FALSE, rad, py, px, dam, GF_DARK_WEAK, flg, FALSE);

	/* Lite up the room */
	unlite_room(py, px);

	/* Assume seen */
	return (TRUE);
}



/*
 * Cast a ball spell
 * Stop if we hit a monster, act as a "ball"
 * Allow "target" mode to pass over monsters
 * Affect grids, objects, and monsters
 */
bool fire_ball(int typ, int dir, int dam, int rad)
{
	int tx, ty;

	int flg = PROJECT_STOP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

	/* Use the given direction */
	tx = px + 99 * ddx[dir];
	ty = py + 99 * ddy[dir];

	/* Hack -- Use an actual "target" */
	if ((dir == 5) && target_okay())
	{
		flg &= ~(PROJECT_STOP);
		tx = target_col;
		ty = target_row;
	}

	/* Analyze the "dir" and the "target".  Hurt items on floor. */
	return (project(0, FALSE, rad, ty, tx, dam, typ, flg, FALSE));
}


/*
 * Cast a shower of ball spells
 * Scatter "balls" around the target or first obstacle reached.
 * Affect grids, objects, and monsters
 */
bool fire_shower(int typ, int dir, int dam, int rad, int num)
{
	int ty, tx, y, x, dist;
	int i;

	int flg = PROJECT_JUMP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

	/* Assume okay */
	bool result = TRUE;

	/* Use the given direction */
	ty = py + 20 * ddy[dir];
	tx = px + 20 * ddx[dir];

	y = py;
	x = px;

	/* Hack -- Use an actual "target" */
	if ((dir == 5) && target_okay())
	{
		tx = target_col;
		ty = target_row;
	}
	else
	{
		cave_type *c_ptr;

		/* Find the REAL target :) */
		for (dist = 0; dist <= MAX_RANGE; dist++)
		{
			c_ptr = &cave[y][x];

			/* Never pass through walls */
			if (dist && !cave_floor_bold(y, x)) break;

			/* Never pass through monsters */
			if (dist && c_ptr->m_idx > 0) break;

			/* Check for arrival at "final target" */
			if ((x == tx) && (y == ty)) break;

			/* Calculate the new location */
			mmove2(&y, &x, py, px, ty, tx);
		}

		/* Update the target */
		ty = y;
		tx = x;
	}

	/* Meteor shower */
	for (i = 0; i < num; i++)
	{
		/* Get targets for some meteors */
		scatter(&y, &x, ty, tx, 3, 0);

		/*
		 * Analyze the "dir" and the "target".  Hurt items on floor.
		 * And don't stand too close ;)
		 */
		if (!project(0, FALSE, rad, y, x, dam, typ, flg, FALSE))
		{
			result = FALSE;
		}
	}

	return (result);
}


/*
 * Cast a volley of bolt spells
 * Scatter "bolts" around the target or first obstacle reached.
 * Affect grids, objects, and monsters
 *
 * dd - bolt damage dice
 * ds - bolt damage sides
 * num - number of bolts in volley
 * dev - maximum distance to spread
 */
bool fire_blast(int typ, int dir, int dd, int ds, int num, int dev)
{
	int ly, lx, ld;
	int ty, tx, y, x, dist;
	int i;

	int flg = PROJECT_THRU | PROJECT_STOP | PROJECT_KILL;

	/* Assume okay */
	bool result = TRUE;

	/* Use the given direction */
	ly = ty = py + 20 * ddy[dir];
	lx = tx = px + 20 * ddx[dir];
	ld = 20;

	y = py;
	x = px;

	/* Hack -- Use an actual "target" */
	if ((dir == 5) && target_okay())
	{
		tx = target_col;
		ty = target_row;

		lx = 20 * (tx - px) + px;
		ly = 20 * (ty - py) + py;
		ld = distance(py, px, ly, lx);
	}
	else
	{
		cave_type *c_ptr;

		/* Find the REAL target :) */
		for (dist = 0; dist <= MAX_RANGE; dist++)
		{
			c_ptr = &cave[y][x];

			/* Never pass through walls */
			if (dist && !cave_floor_bold(y, x)) break;

			/* Never pass through monsters */
			if (dist && c_ptr->m_idx > 0) break;


			/* Check for arrival at "final target" */
			if ((x == tx) && (y == ty)) break;

			/* Calculate the new location */
			mmove2(&y, &x, py, px, ty, tx);
		}

		/* Update the target */
		ty = y;
		tx = x;
	}

	/* Blast */
	for (i = 0; i < num; i++)
	{
		while (1)
		{
			/* Get targets for some bolts */
			y = rand_spread(ly, ld * dev / 20);
			x = rand_spread(lx, ld * dev / 20);

			if (distance(ly, lx, y, x) <= ld * dev / 20) break;
		}

		/* Analyze the "dir" and the "target". */
		if (!project(0, FALSE, 0, y, x, damroll(dd, ds), typ, flg, TRUE))
		{
			result = FALSE;
		}
	}

	return (result);
}


/*
 * Hack -- apply a "projection()" in a direction (or at the target)
 */
static bool project_hook(int typ, int dir, int dam, int flg, bool bolt)
{
	int tx, ty;

	/* Pass through the target if needed */
	flg |= (PROJECT_THRU);

	/* Use the given direction */
	tx = px + ddx[dir];
	ty = py + ddy[dir];

	/* Hack -- Use an actual "target" */
	if ((dir == 5) && target_okay())
	{
		tx = target_col;
		ty = target_row;
	}

	/* Analyze the "dir" and the "target", do NOT explode */
	return (project(0, FALSE, 0, ty, tx, dam, typ, flg, bolt));
}


/*
 * Cast a bolt spell
 * Stop if we hit a monster, as a "bolt"
 * Affect monsters (not grids or objects)
 */
bool fire_bolt(int typ, int dir, int dam)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(typ, dir, dam, flg, TRUE));
}


/*
 * Cast a beam spell
 * Pass through monsters, as a "beam"
 * Affect monsters (not grids or objects)
 */
bool fire_beam(int typ, int dir, int dam)
{
	int flg = PROJECT_BEAM | PROJECT_KILL;
	return (project_hook(typ, dir, dam, flg, FALSE));
}


/*
 * Cast a bolt spell, or rarely, a beam spell
 */
bool fire_bolt_or_beam(int prob, int typ, int dir, int dam)
{
	if (rand_int(100) < prob)
	{
		return (fire_beam(typ, dir, dam));
	}
	else
	{
		return (fire_bolt(typ, dir, dam));
	}
}


/*
 * Some of the old functions
 */
bool lite_line(int dir)
{
	int flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_KILL;
	return (project_hook(GF_LITE_WEAK, dir, damroll(6, 8), flg, FALSE));
}

bool drain_life(int dir, int dam)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_OLD_DRAIN, dir, dam, flg, TRUE));
}

bool wall_to_mud(int dir)
{
	int flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;
	return (project_hook(GF_KILL_WALL, dir, 20 + randint(30), flg, FALSE));
}

bool wizard_lock(int dir)
{
	int flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;
	return (project_hook(GF_JAM_DOOR, dir, 20 + randint(30), flg, FALSE));
}

bool destroy_door(int dir)
{
	int flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM;
	return (project_hook(GF_KILL_DOOR, dir, 0, flg, FALSE));
}

bool disarm_trap(int dir)
{
	int flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM;
	return (project_hook(GF_KILL_TRAP, dir, 0, flg, FALSE));
}

bool heal_monster(int dir)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_OLD_HEAL, dir, damroll(4, 6), flg, TRUE));
}

bool speed_monster(int dir)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_OLD_SPEED, dir, p_ptr->lev, flg, TRUE));
}

bool slow_monster(int dir)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_OLD_SLOW, dir, p_ptr->lev, flg, TRUE));
}

bool sleep_monster(int dir)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_OLD_SLEEP, dir, p_ptr->lev, flg, TRUE));
}

bool stasis_monster(int dir)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_STASIS, dir, p_ptr->lev, flg, TRUE));
}

bool confuse_monster(int dir, int plev)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_OLD_CONF, dir, plev, flg, TRUE));
}

bool stun_monster(int dir, int plev)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_STUN, dir, plev, flg, TRUE));
}

bool poly_monster(int dir)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_OLD_POLY, dir, p_ptr->lev, flg, TRUE));
}

bool clone_monster(int dir)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_OLD_CLONE, dir, 0, flg, TRUE));
}

bool fear_monster(int dir, int plev)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_TURN_ALL, dir, plev, flg, TRUE));
}

bool death_ray(int dir, int plev)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_DEATH_RAY, dir, (plev * 3) / 2, flg, TRUE));
}

bool teleport_monster(int dir)
{
	int flg = PROJECT_BEAM | PROJECT_KILL;
	return (project_hook(GF_AWAY_ALL, dir, MAX_SIGHT * 5, flg, FALSE));
}

/*
 * Hooks -- affect adjacent grids (radius 1 ball attack)
 */
bool door_creation(void)
{
	int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;
	return (project(0, FALSE, 1, py, px, 0, GF_MAKE_DOOR, flg, FALSE));
}

bool trap_creation(void)
{
	int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;
	return (project(0, FALSE, 1, py, px, 0, GF_MAKE_TRAP, flg, FALSE));
}


bool glyph_creation(void)
{
	int flg = PROJECT_GRID | PROJECT_ITEM;
	return (project(0, FALSE, 2, py, px, 0, GF_MAKE_GLYPH, flg, FALSE));
}


bool wall_stone(void)
{
	int flg = PROJECT_GRID | PROJECT_ITEM;

	bool dummy = (project(0, FALSE, 1, py, px, 0, GF_STONE_WALL, flg, FALSE));

	/* Update stuff */
	p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW);

	/* Update the monsters */
	p_ptr->update |= (PU_MONSTERS);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD);

	return dummy;
}


bool destroy_doors_touch(void)
{
	int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;
	return (project(0, FALSE, 1, py, px, 0, GF_KILL_DOOR, flg, FALSE));
}

/* Changed for the sake of the Shadow Cloak of Luthien. -- Gumby */
bool sleep_monsters_touch(void)
{
	int flg = PROJECT_KILL | PROJECT_HIDE;
	return (project(0, FALSE, 1, py, px, p_ptr->lev, GF_STASIS, flg, FALSE));
}


void call_chaos(void)
{
	int Chaos_type, dummy, dir;
	int plev = p_ptr->lev;
	bool line_chaos = FALSE;

	int hurt_types[30] =
	{
		GF_ELEC,      GF_POIS,    GF_ACID,    GF_COLD,
		GF_FIRE,      GF_MISSILE, GF_ARROW,   GF_PLASMA,
		GF_HOLY_FIRE, GF_WATER,   GF_LITE,    GF_DARK,
		GF_FORCE,     GF_INERTIA, GF_MANA,    GF_METEOR,
		GF_ICE,       GF_CHAOS,   GF_NETHER,  GF_DISENCHANT,
		GF_SHARDS,    GF_SOUND,   GF_NEXUS,   GF_CONFUSION,
		GF_TIME,      GF_GRAVITY, GF_ROCKET,  GF_NUKE,
		GF_HELL_FIRE, GF_DISINTEGRATE
	};

	Chaos_type = hurt_types[((randint (30))-1)];
	if (randint(4)==1) line_chaos = TRUE;

#if 0
	/* Probably a meaningless line, a remnant from earlier code */
	while (Chaos_type > GF_GRAVITY && Chaos_type < GF_ROCKET);
#endif

	if (randint(6)==1)
	{
		for (dummy = 1; dummy<10; dummy++)
		{
			if (dummy-5)
			{
				if (line_chaos)
					fire_beam(Chaos_type, dummy, 150);
				else
					fire_ball(Chaos_type, dummy, 150, 2);
			}
		}
	}
	else if (randint(3)==1)
	{
		fire_ball(Chaos_type, 0, 300, 8);
	}
	else
	{
		if (!get_aim_dir(&dir)) return;
		if (line_chaos)
			fire_beam(Chaos_type, dir, 300);
		else
			fire_ball(Chaos_type, dir, 300, 3 + (plev/35));
	}
}


/*
 * Activate the evil Topi Ylinen curse
 * rr9: Stop the nasty things when a Cyberdemon is summoned
 * or the player gets paralyzed.
 */
void activate_ty_curse()
{
	int i = 0;
	bool stop_ty = FALSE;

	do
	{
		switch(randint(27))
		{
			case 1: case 2: case 3: case 16:
				aggravate_monsters(1, FALSE);
				if (randint(6)!=1) break;
			case 17:
				aggravate_monsters(1, TRUE);
				if (randint(6)!=1) break;
			case 4: case 5: case 6:
				activate_hi_summon();
				if (randint(6)!=1) break;
			case 7: case 8: case 9: case 18:
				(void) summon_specific(py, px, dun_level, 0);
				if (randint(6)!=1) break;
			case 10: case 11: case 12:
			{
				msg_print("You feel your life draining away...");
				lose_exp(p_ptr->exp / 15);
				if (randint(6)!=1) break;
			}
			case 13: case 14: case 15: case 19: case 20:
				if (p_ptr->free_act && (randint(100) < p_ptr->skill_sav))
				{
					/* Do nothing */ ;
				}
				else
				{
					msg_print("You feel like a statue!");
					if (p_ptr->free_act)
						set_paralyzed (p_ptr->paralyzed + randint(5));
					else
						set_paralyzed (p_ptr->paralyzed + randint(15));
					stop_ty = TRUE;
				}
				if (randint(6)!=1) break;
			case 21: case 22: case 23:
				(void)do_dec_stat((randint(6))-1);
				if (randint(6)!=1) break;
			case 24:
				msg_print("Huh? Who am I? What am I doing here?");
				lose_all_info();
				break;
			case 25:
				if ((dun_level > 65) && !stop_ty)
				{
					msg_print("Oshitoshitoshit, you're gonna die you're gonna die you're gonna DIE!");
					summon_cyber();
					stop_ty = TRUE;
				}
				if (randint(6)!=1) break;
			default:
				while (i<6)
				{
					do
					{
						(void)do_dec_stat(i);
					}
					while (randint(2)==1);

					i++;
				}
		}
	}
	while ((randint(3)==1) && !stop_ty);
}


void activate_hi_summon(void)
{
	int i = 0;

	for (i = 0; i < (randint(9) + (dun_level / 40)); i++)
	{
		switch(randint(26) + (dun_level / 20) )
		{
			case 1: case 2:
				(void) summon_specific(py, px, dun_level, SUMMON_ANT);
				break;
			case 3: case 4:
				(void) summon_specific(py, px, dun_level, SUMMON_SPIDER);
				break;
			case 5: case 6:
				(void) summon_specific(py, px, dun_level, SUMMON_HOUND);
				break;
			case 7: case 8:
				(void) summon_specific(py, px, dun_level, SUMMON_HYDRA);
				break;
			case 9: case 10:
				(void) summon_specific(py, px, dun_level, SUMMON_ANGEL);
				break;
			case 11: case 12:
				(void) summon_specific(py, px, dun_level, SUMMON_UNDEAD);
				break;
			case 13: case 14:
				(void) summon_specific(py, px, dun_level, SUMMON_DRAGON);
				break;
			case 15: case 16:
				(void) summon_specific(py, px, dun_level, SUMMON_DEMON);
				break;
			case 17:
				(void) summon_specific(py, px, dun_level, SUMMON_WRAITH);
				break;
			case 18: case 19:
				(void) summon_specific(py, px, dun_level, SUMMON_UNIQUE);
				break;
			case 20: case 21:
				(void) summon_specific(py, px, dun_level, SUMMON_HI_UNDEAD);
				break;
			case 22: case 23:
				(void) summon_specific(py, px, dun_level, SUMMON_HI_DRAGON);
				break;
			case 24: case 25:
				(void) summon_specific(py, px, 100, SUMMON_CYBER);
				break;
			default:
				(void) summon_specific(py, px,(((dun_level * 3) / 2) + 5), 0);
		}
	}
}


void summon_cyber(void)
{
	int i = 0;
	int max_cyber = (dun_level / 50) + randint(6);

	for (i = 0; i < max_cyber; i++)
	{
		(void)summon_specific(py, px, 100, SUMMON_CYBER);
	}
}


void wall_breaker(void)
{
	int dummy = 5;

	if (randint(80+(p_ptr->lev))<70)
	{
		do
		{
			dummy = randint(9);
		}
		while ((dummy == 5) || (dummy == 0));

		wall_to_mud (dummy);
	}
	else if (randint(100)>30)
	{
		earthquake(py,px,1);
	}
	else
	{
		for (dummy = 1; dummy<10; dummy++)
		{
			if (dummy-5) wall_to_mud(dummy);
		}
	}
}


/* Add the BLESSED flag to a weapon so a Priest can use it */
bool bless_weapon(void)
{
	int             item;
	object_type     *o_ptr;
	u32b            f1, f2, f3;
	char            o_name[80];


	/* Assume enchant weapon */
	item_tester_hook = item_tester_hook_weapon;

	/* Get an item (from equip or inven or floor) */
	if (!get_item(&item, "Bless which weapon? ", TRUE, TRUE, TRUE))
	{
		if (item == -2) msg_print("You have no weapon to bless.");
		return (FALSE);
	}

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


	/* Description */
	object_desc(o_name, o_ptr, FALSE, 0);

        /* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);

	if (o_ptr->ident & (IDENT_CURSED))
	{

		if (((f3 & (TR3_HEAVY_CURSE)) && (randint (100) < 33)) ||
		    (f3 & (TR3_PERMA_CURSE)))
		{

			msg_format("The black aura on %s %s disrupts the blessing!",
			    ((item >= 0) ? "your" : "the"), o_name);
			return (TRUE);
		}

		msg_format("A malignant aura leaves %s %s.",
		    ((item>=0)? "your" : "the"), o_name);

		/* Uncurse it */
		o_ptr->ident &= ~(IDENT_CURSED);

		/* Hack -- Assume felt */
		o_ptr->ident |= (IDENT_SENSE);

		/* Take note */
		o_ptr->note = quark_add("uncursed");

		/* Recalculate the bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Window stuff */
		p_ptr->window |= (PW_EQUIP);
	}

/* Next, we try to bless it. Artifacts have a 1/3 chance of being blessed,
otherwise, the operation simply disenchants them, godly power negating the
magic. Ok, the explanation is silly, but otherwise priests would always
bless every artifact weapon they find.
Ego weapons and normal weapons can be blessed automatically. */

	if (f3 & TR3_BLESSED)
	{
		msg_format("%s %s %s blessed already.",
		    ((item >= 0) ? "Your" : "The"), o_name,
		    ((o_ptr->number > 1) ? "were" : "was"));
		return (TRUE);
	}

	if (!(o_ptr->art_name || o_ptr->name1) || (randint(3)==1))
	{
		/* Describe */
		msg_format("%s %s shine%s!",
		    ((item >= 0) ? "Your" : "The"), o_name,
		    ((o_ptr->number > 1) ? "" : "s"));
		o_ptr->art_flags3 |= TR3_BLESSED;
	}
	else
	{

		bool dis_happened = FALSE;

		msg_print("The artifact resists your blessing!");

		/* Disenchant tohit */
		if (o_ptr->to_h > 0)
		{
			o_ptr->to_h--;
			dis_happened = TRUE;
		}

		if ((o_ptr->to_h > 5) && (rand_int(100) < 33)) o_ptr->to_h--;

		/* Disenchant todam */
		if (o_ptr->to_d > 0)
		{
			o_ptr->to_d--;
			dis_happened = TRUE;
		}

		if ((o_ptr->to_d > 5) && (rand_int(100) < 33)) o_ptr->to_d--;

		/* Disenchant toac */
		if (o_ptr->to_a > 0)
		{
			o_ptr->to_a--;
			dis_happened = TRUE;
		}

		if ((o_ptr->to_a > 5) && (rand_int(100) < 33)) o_ptr->to_a--;

		if (dis_happened)
		{
			msg_print("There is a static feeling in the air...");
			msg_format("%s %s %s disenchanted!",
			    ((item >= 0) ? "Your" : "The"), o_name,
			    ((o_ptr->number > 1) ? "were" : "was"));
		}
	}

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Window stuff */
	p_ptr->window |= (PW_EQUIP | PW_PLAYER);

	return (TRUE);
}


/*
 * Detect all "nonliving", "undead" or "demonic" monsters on current panel
 */
bool detect_monsters_nonliving(void)
{
	int i, y, x;
	bool flag = FALSE;

	/* Scan monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Skip object mimics */
		if ((r_ptr->d_char == '<') || (r_ptr->d_char == '>') ||
		    (r_ptr->d_char == '$') || (r_ptr->d_char == '*') ||
		    (r_ptr->d_char == ',') ||
		    (r_ptr->d_char == '!') || (r_ptr->d_char == '=') ||
		    (r_ptr->d_char == '?') || (r_ptr->d_char == '|')) continue;

		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;

		/* Only detect nearby monsters */
		if (!panel_contains(y, x)) continue;

		/* Detect evil monsters */
		if ((r_ptr->flags3 & (RF3_NONLIVING)) ||
		    (r_ptr->flags3 & (RF3_UNDEAD)))
		{
			/* Update monster recall window */
			if (monster_race_idx == m_ptr->r_idx)
			{
				/* Window stuff */
				p_ptr->window |= (PW_MONSTER);
			}

			/* Repair visibility later */
			repair_monsters = TRUE;

			/* Hack -- Detect monster */
			m_ptr->mflag |= (MFLAG_MARK | MFLAG_SHOW);

			/* Hack -- See monster */
			m_ptr->ml = TRUE;

			/* Redraw */
			lite_spot(y, x);

			/* Detect */
			flag = TRUE;
		}
	}

	/* Describe */
	if (flag)
	{
		/* Describe result */
		msg_print("You sense the presence of unnatural beings!");
	}

	/* Result */
	return (flag);
}


/*
 * Confuse monsters
 */
bool confuse_monsters(int dam)
{
	return (project_hack(GF_OLD_CONF, dam));
}

/*
 * Charm monsters
 */
bool charm_monsters(int dam)
{
	return (project_hack(GF_CHARM, dam));
}

/*
 * Charm animals
 */
bool charm_animals(int dam)
{
	return (project_hack(GF_CONTROL_ANIMAL, dam));
}

/*
 * Stun monsters
 */
bool stun_monsters(int dam)
{
	return (project_hack(GF_STUN, dam));
}

/*
 * Stasis monsters
 */
bool stasis_monsters(int dam)
{
	return (project_hack(GF_STASIS, dam));
}

/*
 * Mindblast monsters
 */
bool mindblast_monsters(int dam)
{
	return (project_hack(GF_PSI, dam));
}

/*
 * Banish all monsters
 */
bool banish_monsters(int dist)
{
	return (project_hack(GF_AWAY_ALL, dist));
}

/*
 * Turn evil
 */
bool turn_evil(int dam)
{
	return (project_hack(GF_TURN_EVIL, dam));
}

/*
 * Turn everyone
 */
bool turn_monsters(int dam)
{
	return (project_hack(GF_TURN_ALL, dam));
}

/*
 * Death-ray all monsters (note: OBSCENELY powerful)
 */
bool deathray_monsters(void)
{
	return (project_hack(GF_DEATH_RAY, p_ptr->lev * 3));
}

bool charm_monster(int dir, int plev)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_CHARM, dir, plev, flg, TRUE));
}

bool control_one_undead(int dir, int plev)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_CONTROL_UNDEAD, dir, plev, flg, TRUE));
}


bool charm_animal(int dir, int plev)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_CONTROL_ANIMAL, dir, plev, flg, TRUE));
}
