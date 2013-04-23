#define SPELLS2_C
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

/* Chance of using syllables to form the name instead of the "template" files */
#define TABLE_NAME      45
#define A_CURSED        13
#define WEIRD_LUCK      12
#define BIAS_LUCK       20
/* Bias luck needs to be higher than weird luck, since it is usually tested
several times... */
#define ACTIVATION_CHANCE 3
/* Time between an item becoming uncursed, and cursing itself again. */
#define CURSE_TIMEOUT 30

static bool detect_monsters_string(cptr Match);

/*
 * Return TRUE if the player can teleport to (x,y) with dimension door.
 */
static int mvd_dimension_door_success(int y, int x, int plev)
{
	cave_type *c_ptr = &cave[y][x];

	/* Monster or LOS-blocking feature present. */
	if (!cave_empty_grid(c_ptr)) return MVD_STOP_BEFORE_HERE;

	/* No teleporting into vaults. */
	if (c_ptr->info & CAVE_ICKY) return MVD_STOP_BEFORE_HERE;

	/* Too far. */
	if (distance(py, px, y, x) > plev+2) return MVD_STOP_BEFORE_HERE;

	/* Success. */
	return MVD_CONTINUE;
}

/*
 * Teleport the player to a nearby square of the player's choice.
 * Returns FALSE if aborted, TRUE otherwise.
 */
bool dimension_door(int plev, int fail_dis)
{
	int dir, x, y;

	msg_print("You open a dimensional gate. Choose a destination.");
	msg_print(NULL);

	/* Hack - start with the "pick a location" display, but allow directions. */
	set_gnext("*op");

	/* Find the location. */
	if (!get_aim_dir(&dir)) return FALSE;

	/* Extract the location. */
	get_dir_target(&x, &y, dir, mvd_dimension_door_success);

	energy_use += 6*TURN_ENERGY/10 - plev*TURN_ENERGY/100;

	/* Bad target or bad luck. */
	if (one_in(plev*plev/2) || !mvd_dimension_door_success(y, x, plev))
	{
		msg_print("You fail to exit the astral plane correctly!");
		energy_use += TURN_ENERGY;
		teleport_player(fail_dis);
	}
	else /* Success */
	{
		teleport_player_to(y,x);
	}
	return TRUE;
}

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
 * Leave a "glyph of warding" which prevents monster movement
 */
void warding_glyph(void)
{
	/* XXX XXX XXX */
	if (!cave_clean_bold(py, px))
	{
		msg_print("The object resists the spell.");
		return;
	}

	/* Create a glyph */
	cave_set_feat(py, px, FEAT_GLYPH);
}

void explosive_rune(void)
{
	/* XXX XXX XXX */
	if (!cave_clean_bold(py, px))
	{
		msg_print("The object resists the spell.");
		return;
	}

	/* Create a glyph */
	cave_set_feat(py, px, FEAT_MINOR_GLYPH);
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
	/* Sustain */
	if (p_ptr->sustain[stat])
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
 * Lose a "point" (time attack version)
 */
void do_dec_stat_time(int stat, bool msg)
{
	if (msg) msg_format("You're not as %s as you used to be...",
		desc_stat_pos[stat]);

	p_ptr->stat_cur[stat] = MAX(3, p_ptr->stat_cur[stat]*3/4);

	p_ptr->update |= (PU_BONUS);
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
 * Try to restore all stats. Return TRUE if any were restored.
 */
bool do_res_stats(void)
{
	bool b;
	int i;
	for (b = FALSE, i = 0; i < A_MAX; i++)
	{
		b |= do_res_stat(i);
	}
	return b;
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
 * Identify everything being carried.
 * Done by a potion of "self knowledge".
 */
void identify_pack(void)
{
	int                 i;

	/* Simply identify and know every item */
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		object_type *o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Aware and Known */
		object_aware(o_ptr);
		object_known(o_ptr);
	}
}






/*
 * Used by the "enchant" function (chance of failure)
 * (modified for Zangband, we need better stuff there...) -- TY
 */
static int enchant_table[16] =
{
	0, 10,  50, 100, 200,
	300, 400, 500, 650, 800,
	950, 987, 993, 995, 998,
	1000
};


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
	int             i, feel, cnt = 0;

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

		/* Hack - as "cursed" items can become average, good or messageless
		when they are uncursed, don't leave value information behind */
		feel = find_feeling(o_ptr);

		if (feel == SENSE_C_OBJ || feel == SENSE_CP_OBJ)
			o_ptr->ident &= ~(IDENT_SENSE_VALUE | IDENT_SENSE_HEAVY);

		/* Uncurse it */
		o_ptr->ident &= ~(IDENT_CURSED);

		/* Hack -- Assume felt */
		o_ptr->ident |= (IDENT_SENSE_CURSED);

		if (o_ptr->flags3 & (TR3_CURSED))
			o_ptr->flags3 &= ~(TR3_CURSED);

		if (o_ptr->flags3 & (TR3_HEAVY_CURSE))
			o_ptr->flags3 &= ~(TR3_HEAVY_CURSE);

		/* You can't palm that ring of speed (-20) off quite so easily... */
		if (broken_p(o_ptr))
			o_ptr->ident |= (IDENT_SENSE);

		/* Recalculate the bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Window stuff */
		p_ptr->window |= (PW_EQUIP);

		/* Set auto_curse timeout */
		curse_turn = turn + CURSE_TIMEOUT;

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
 * Restores any drained experience
 */
bool restore_level(void)
{
	int i,did_it;

	/* Assume nothing happened */
	did_it = FALSE;

	for (i=0;i<MAX_SKILLS;i++)
	{
		/* Restore experience */
		if (skill_set[i].value < skill_set[i].max_value)
		{
			skill_set[i].value = skill_set[i].max_value;
			did_it = TRUE;
		}
	}

	if (did_it)
	{
		/* Hack - prevent the update of arbitrary things (I haven't checked
		 * that this is necessary, but it may be). */
		u32b hack_update = p_ptr->update;

		/* Message */
		msg_print("You feel your life energies returning.");
		/* Did something */

		/* Recalculate hit points, mana and spells, but nothing else. */
		p_ptr->update = PU_HP | PU_MANA | PU_SPELLS;
		update_stuff();

		p_ptr->update = hack_update;

		return (TRUE);
	}

	/* No effect */
	return (FALSE);
}


/*
 * Turns an object into gold, gain some of its value in a shop
 * Return TRUE if an item is destroyed.
 */
bool alchemy(void)
{
	s32b price;
	object_type q_ptr[1], *o_ptr;
	errr err;

	/* Restrict the choices */
	item_tester_hook = item_tester_hook_destroy;

	/* Get an item (from equip or inven or floor) */
	o_ptr = get_item(&err, "Turn which item to gold? ", TRUE, TRUE, TRUE);
	if (!o_ptr)
	{
		if (err == -2) msg_print("You have nothing to turn to gold.");
		return FALSE;
	}

	if (do_cmd_destroy_aux("turn", " to gold", q_ptr, o_ptr)) return FALSE;

	/* Find the value for alchemy. */
	price = object_value(q_ptr, TRUE) * q_ptr->number;

	if (price <= 0)
	{
		/* Message */
		msg_format("You have turned %v to fool's gold.",
			object_desc_f3, q_ptr, TRUE, 3);
	}
	else
	{
		price /= 3;

		if (price > 30000) price = 30000;
		msg_format("You have turned %v to %ld coins worth of gold.",
			object_desc_f3, q_ptr, TRUE, 3, price);
		p_ptr->au += price;

		/* Redraw gold */
		p_ptr->redraw |= (PR_GOLD);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER);
	}

	return TRUE;
}




/*
 * Hack - determine if a chaos feature duplicates anothe ability of the player.
 * This should consider what each feature actually does...
 */
static bool chaos_reject_dup(int i)
{
	switch (i)
	{
		case MUT_FIRE_BODY:
		case MUT_FEARLESS:
		case MUT_REGEN:
		case MUT_ESP:
		case MUT_SUS_STATS:
			return TRUE;
		default:
			return FALSE;
	}
}

/*
 * Add a list of the player's racial powers to info.
 */
static int add_race_powers(cptr *info)
{
	int i;
	const int plev = MAX(1, skill_set[SKILL_RACIAL].value/2);

	for (i = 0; i < rp_ptr->powers; i++)
	{
		cptr str = describe_power(rp_ptr->power[i].idx, plev);

		int cost = power_cost(&rp_ptr->power[i], plev);

		/* Describe the power. */
		info[i] = safe_string_make(format("You have a power which %v "
			"(cost %d).", evaluate_text_f3, str, "LEV", plev, cost));
	}
	return rp_ptr->powers;
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
	int             i = 0, j, k;

	u32b    f1 = 0L, f2 = 0L, f3 = 0L;

	object_type     *o_ptr;

	cptr    info[128];

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

	/* Handle racial powers. */
	i += add_race_powers(info+i);

	/* Handle chaos features */
	i += add_chaos_features(info+i, chaos_reject_dup);

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
		info[i++] = "You feel rightous.";
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
		info[i++] = "You are temporarily invulnerable.";
	}
	if (p_ptr->wraith_form)
	{
		info[i++] = "You are temporarily incorporeal.";
	}
	if (p_ptr->confusing)
	{
		info[i++] = "Your hands are glowing dull red.";
	}
	if (p_ptr->sneaking)
	{
		info[i++] = "You are sneaking around very carefully.";
	}
	if (p_ptr->new_spells)
	{
		info[i++] = "You can learn some spells.";
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


	if (p_ptr->sustain[A_STR])
	{
		info[i++] = "Your strength is sustained.";
	}
	if (p_ptr->sustain[A_INT])
	{
		info[i++] = "Your intelligence is sustained.";
	}
	if (p_ptr->sustain[A_WIS])
	{
		info[i++] = "Your wisdom is sustained.";
	}
	if (p_ptr->sustain[A_CON])
	{
		info[i++] = "Your constitution is sustained.";
	}
	if (p_ptr->sustain[A_DEX])
	{
		info[i++] = "Your dexterity is sustained.";
	}
	if (p_ptr->sustain[A_CHR])
	{
		info[i++] = "Your charisma is sustained.";
	}

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
		info[i++] = "Your weapon is branded with the Yellow Sign.";
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
		switch (f1 & TR1_ALL_SLAY_DRAGON)
		{
			case TR1_SLAY_DRAGON:
			info[i++] = "Your weapon is especially deadly against dragons.";
			break;
			case TR1_KILL_DRAGON:
			info[i++] = "Your weapon is a great bane of dragons.";
			break;
			case TR1_X15_DRAGON:
			info[i++] = "Your weapon is incredibly deadly against dragons.";
			break;
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

	/* Free any allocated strings. */
	while (i--) safe_free(info+i);

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
	int                 i;

	/* Forget info about objects */
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		object_type *o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Allow "protection" by the MENTAL flag */
		if (o_ptr->ident & (IDENT_MENTAL)) continue;

		/* Hack -- Clear the "empty" flag */
		o_ptr->ident &= ~(IDENT_EMPTY);

		/* Hack -- Clear the "known" flag */
		o_ptr->ident &= ~(IDENT_KNOWN);

		/* Items still not forgotten (i.e. have easy_know) keep feelings */
		if (object_known_p(o_ptr)) continue;

		/* Hack -- Clear the "felt" flags */
		o_ptr->ident &= ~(IDENT_SENSE_VALUE);

		/* Hack -- Clear the "felt curses" flag for unworn items */
		if (i < INVEN_WIELD) o_ptr->ident &= ~(IDENT_SENSE_CURSED);

		/* Hack -- Clear the "heavily felt" flag */
		o_ptr->ident &= ~(IDENT_SENSE_HEAVY);

		/* Hack - Clear the "powerful" flags */
		o_ptr->ident &= ~(IDENT_POWER_ALL);
	}


	/* Recalculate/redraw stuff (later) */
	update_objects(OUP_CARRIED_MASK);

	/* Mega-Hack -- Forget the map */
	wiz_dark();

	/* It worked */
	return (TRUE);
}



/*
 * Check whether the current panel contains traps.
 */
PURE bool detect_traps_p(void)
{
	int x,y;

	/* Scan the current panel */
	for (y = panel_row_min; y <= panel_row_max; y++)
	{
		for (x = panel_col_min; x <= panel_col_max; x++)
		{
			/* Access the grid */
			const cave_type *c_ptr = &cave[y][x];

			/* Detected a trap. */
			if (c_ptr->feat == FEAT_INVIS ||
				((c_ptr->feat >= FEAT_TRAP_HEAD) &&
				(c_ptr->feat <= FEAT_TRAP_TAIL)))
			{
				return TRUE;
			}
		}
	}
	return FALSE;
}



/*
 * Detect all traps on current panel
 */
bool detect_traps(void)
{
	int y, x;

	bool detect = FALSE;


	/* Scan the current panel */
	for (y = panel_row_min; y <= panel_row_max; y++)
	{
		for (x = panel_col_min; x <= panel_col_max; x++)
		{
			/* Access the grid */
			cave_type *c_ptr = &cave[y][x];

			const u16b old_info = c_ptr->info;

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

			/* Remember that this square has been checked at some point. */
			c_ptr->info |= (CAVE_TRAP);

			/* Redraw if necessary. */
			if (c_ptr->info != old_info) lite_spot(y, x);
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
				replace_secret_door(y,x);
			}

			/* Detect doors */
			if (((c_ptr->feat >= FEAT_DOOR_HEAD) &&
				(c_ptr->feat <= FEAT_DOOR_HEAD)) ||
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
	if (detect)
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
	if (detect)
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
	if (detect)
	{
		msg_print("You sense the presence of treasure!");
	}

	if (detect_monsters_string("$*"))
	{
		detect = TRUE;
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
	if (detect)
	{
		msg_print("You sense the presence of objects!");
	}

	if (detect_monsters_string("!=?|"))
	{
		detect = TRUE;
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
	if (allart_p(o_ptr) || ego_item_p(o_ptr) ||
			(tv == TV_AMULET) || (tv == TV_RING) ||
			(tv == TV_STAFF) || (tv == TV_WAND) || (tv == TV_ROD) ||
			(tv == TV_SCROLL) || (tv == TV_POTION) || (tv == TV_BOOK) ||
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
	if (detect)
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
	int             i, y, x;

	bool    flag = FALSE;


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

		/* Detect all non-invisible monsters */
	if ((!(r_ptr->flags2 & (RF2_INVISIBLE)))
		|| p_ptr->see_inv || p_ptr->tim_invis)
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

	/* Update window */
	p_ptr->window |= PW_VISIBLE;

	/* Result */
	return (flag);
}


/*
 * Detect all "invisible" monsters on current panel
 */
bool detect_monsters_invis(void)
{
	int             i, y, x;

	bool    flag = FALSE;


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

	/* Update window */
	p_ptr->window |= PW_VISIBLE;

	/* Result */
	return (flag);
}



/*
 * Detect all "evil" monsters on current panel
 */
bool detect_monsters_evil(void)
{
	int             i, y, x;

	bool    flag = FALSE;


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

	/* Update window */
	p_ptr->window |= PW_VISIBLE;

	/* Result */
	return (flag);
}




/*
 * Detect all (string) monsters on current panel
 */
static bool detect_monsters_string(cptr Match)
{
	int             i, y, x;

	bool    flag = FALSE;


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
		if (strchr(Match, r_ptr->gfx.dc))

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
		msg_print("You sense the presence of monsters!");
	}

	/* Update window */
	p_ptr->window |= PW_VISIBLE;

	/* Result */
	return (flag);
}


/*
 * Detect everything.
 * This function assumes that it is expected to check for traps, and so
 * should not be called for items which may remain unidentified.
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
	if (dun_level <= 0)
	{
		return;
	}
	else if (is_quest(dun_level) ||
		(dun_level >= dun_defs[cur_dungeon].max_level))
	{
		if(dun_defs[cur_dungeon].flags & DF_TOWER)
		{
			cave_set_feat(py, px, FEAT_MORE);
		}
		else
		{
			cave_set_feat(py, px, FEAT_LESS);
		}
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
 * Hook to specify "weapon"
 */
static bool PURE item_tester_hook_weapon(object_ctype *o_ptr)
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
 * Hook to specify "armour"
 */
bool PURE item_tester_hook_armour(object_ctype *o_ptr)
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

static bool PURE item_tester_unknown(object_ctype *o_ptr)
{
	if (object_known_p(o_ptr))
		return FALSE;
	else
		return TRUE;
}


/*
 * Enchants a plus onto an item.                        -RAK-
 *
 * Revamped!  Now takes item pointer, number of times to try enchanting,
 * and a flag of what to try enchanting.  Artifacts resist enchantment
 * some of the time, and successful enchantment to at least +0 might
 * break a curse on the item.  -CFT
 *
 * Note that an item can technically be enchanted all the way to +15 if
 * you wait a very, very, long time.  Going from +9 to +10 only works
 * about 5% of the time, and from +10 to +11 only about 1% of the time.
 *
 * Note that this function can now be used on "piles" of items, and
 * the larger the pile, the lower the chance of success.
 */
bool enchant(object_type *o_ptr, int n, int eflag)
{
	int i, chance, prob;

	bool res = FALSE;

	bool a = (allart_p(o_ptr));

	u32b f1, f2, f3;

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);


	/* Large piles resist enchantment */
	prob = o_ptr->number * 100;

	/* Missiles are easy to enchant */
	if ((o_ptr->tval == TV_BOLT) ||
		(o_ptr->tval == TV_ARROW) ||
		(o_ptr->tval == TV_SHOT))
	{
		prob = prob / 20;
	}

	/* Try "n" times */
	for (i=0; i<n; i++)
	{
		/* Hack -- Roll for pile resistance */
		if (rand_int(prob) >= 100) continue;

		/* Enchant to hit */
		if (eflag & (ENCH_TOHIT))
		{
			if (o_ptr->to_h < 0) chance = 0;
			else if (o_ptr->to_h > 15) chance = 1000;
			else chance = enchant_table[o_ptr->to_h];

			if ((randint(1000) > chance) && (!a || (rand_int(100) < 50)))
			{
				o_ptr->to_h++;
				res = TRUE;

				/* only when you get it above -1 -CFT */
				if (cursed_p(o_ptr) &&
					(!(f3 & (TR3_PERMA_CURSE))) &&
					(o_ptr->to_h >= 0) && (rand_int(100) < 25))
				{
					msg_print("The curse is broken!");
					o_ptr->ident &= ~(IDENT_CURSED);
					o_ptr->ident |= (IDENT_SENSE_CURSED);

					if (o_ptr->flags3 & (TR3_CURSED))
						o_ptr->flags3 &= ~(TR3_CURSED);
					if (o_ptr->flags3 & (TR3_HEAVY_CURSE))
						o_ptr->flags3 &= ~(TR3_HEAVY_CURSE);

				}
			}
		}

		/* Enchant to damage */
		if (eflag & (ENCH_TODAM))
		{
			if (o_ptr->to_d < 0) chance = 0;
			else if (o_ptr->to_d > 15) chance = 1000;
			else chance = enchant_table[o_ptr->to_d];

			if ((randint(1000) > chance) && (!a || (rand_int(100) < 50)))
			{
				o_ptr->to_d++;
				res = TRUE;

				/* only when you get it above -1 -CFT */
				if (cursed_p(o_ptr) &&
					(!(f3 & (TR3_PERMA_CURSE))) &&
					(o_ptr->to_d >= 0) && (rand_int(100) < 25))
				{
					msg_print("The curse is broken!");
					o_ptr->ident &= ~(IDENT_CURSED);
					o_ptr->ident |= (IDENT_SENSE_CURSED);

					if (o_ptr->flags3 & (TR3_CURSED))
						o_ptr->flags3 &= ~(TR3_CURSED);
					if (o_ptr->flags3 & (TR3_HEAVY_CURSE))
						o_ptr->flags3 &= ~(TR3_HEAVY_CURSE);

				}
			}
		}

		/* Enchant to armor class */
		if (eflag & (ENCH_TOAC))
		{
			if (o_ptr->to_a < 0) chance = 0;
			else if (o_ptr->to_a > 15) chance = 1000;
			else chance = enchant_table[o_ptr->to_a];

			if ((randint(1000) > chance) && (!a || (rand_int(100) < 50)))
			{
				o_ptr->to_a++;
				res = TRUE;

				/* only when you get it above -1 -CFT */
				if (cursed_p(o_ptr) &&
					(!(f3 & (TR3_PERMA_CURSE))) &&
					(o_ptr->to_a >= 0) && (rand_int(100) < 25))
				{
					msg_print("The curse is broken!");
					o_ptr->ident &= ~(IDENT_CURSED);
					o_ptr->ident |= (IDENT_SENSE_CURSED);

					if (o_ptr->flags3 & (TR3_CURSED))
						o_ptr->flags3 &= ~(TR3_CURSED);
					if (o_ptr->flags3 & (TR3_HEAVY_CURSE))
						o_ptr->flags3 &= ~(TR3_HEAVY_CURSE);
				}
			}
		}
	}

	/* Failure */
	if (!res) return (FALSE);


	/* Recalculate/redraw stuff (later) */
	update_object(o_ptr);

	/* Success */
	return (TRUE);
}

/* Just in case */
#ifndef allart_p
#define allart_p(T) \
	((artifact_p(T) || (T)->art_name) ? TRUE : FALSE)
#endif

/*
 * Enchant an item (in the inventory or on the floor)
 * Note that "num_ac" requires armour, else weapon
 * Returns TRUE if attempted, FALSE if cancelled
 */
bool enchant_spell(int num_hit, int num_dam, int num_ac)
{
	errr err;
	bool            okay = FALSE;

	object_type             *o_ptr;


	/* Assume enchant weapon */
	item_tester_hook = item_tester_hook_weapon;

	/* Enchant armor if requested */
	if (num_ac) item_tester_hook = item_tester_hook_armour;

	/* Get an item (from equip or inven or floor) */
	if (!((o_ptr = get_item(&err, "Enchant which item? ", TRUE, TRUE, TRUE))))
	{
		if (err == -2) msg_print("You have nothing to enchant.");
		return (FALSE);
	}


	/* Describe */
	msg_format("%s %v glow%s brightly!",
			((is_inventory_p(o_ptr) && !allart_p(o_ptr)) ? "Your" : "The"),
			object_desc_f3, o_ptr, FALSE, 0, ((o_ptr->number > 1) ? "" : "s"));

	/* Enchant */
	if (enchant(o_ptr, num_hit, ENCH_TOHIT)) okay = TRUE;
	if (enchant(o_ptr, num_dam, ENCH_TODAM)) okay = TRUE;
	if (enchant(o_ptr, num_ac, ENCH_TOAC)) okay = TRUE;

	/* Failure */
	if (!okay)
	{
		/* Flush */
		if (flush_failure) flush();

		/* Message */
		msg_print("The enchantment failed.");
	}
	else
	{
		/* Describe again */
		msg_format("You now %s %v", (is_inventory_p(o_ptr)) ? "have" : "see",
			object_desc_f3, o_ptr, TRUE, 1);

		/* Forget obsolete stacking information. */
		set_stack_number(o_ptr);
	}

	/* Something happened */
	return (TRUE);
}


/*
 * Make it bad, or if it's already bad, make it worse!
 */
static void curse_artifact (object_type * o_ptr)
{
	if (o_ptr->pval) o_ptr->pval = 0 - ((o_ptr->pval) + randint(4));
	if (o_ptr->to_a) o_ptr->to_a = 0 - ((o_ptr->to_a) + randint(4));
	if (o_ptr->to_h) o_ptr->to_h = 0 - ((o_ptr->to_h) + randint(4));
	if (o_ptr->to_d) o_ptr->to_d = 0 - ((o_ptr->to_d) + randint(4));
	o_ptr->flags3 |= ( TR3_HEAVY_CURSE | TR3_CURSED );

	/* Perma-cursed objects which cannot be instantly recognised are bad. */
/* if (randint(4)==1) o_ptr-> flags3 |= TR3_PERMA_CURSE; */

	if (randint(3)==1) o_ptr-> flags3 |= TR3_TY_CURSE;
	if (randint(2)==1) o_ptr-> flags3 |= TR3_AGGRAVATE;
	if (randint(3)==1) o_ptr-> flags3 |= TR3_DRAIN_EXP;
	if (randint(2)==1) o_ptr-> flags3 |= TR3_TELEPORT;
	else if (randint(3)==1) o_ptr->flags3 |= TR3_NO_TELE;
	if (randint(3)==1) o_ptr->flags3 |= TR3_NO_MAGIC;
	o_ptr->ident |= IDENT_CURSED;
}

/*
 * Various flags to add to an object using an appropriate function.
 */
typedef const struct biased_flag_type biased_flag_type;
struct biased_flag_type
{
	byte bias;
	byte chance;
	byte set;
	u32b flag;
	bool (*test)(object_ctype *);
};

typedef struct bias_type bias_type;
struct bias_type
{
	byte bias;
	byte chance;
};

typedef const struct unbiased_flag_type unbiased_flag_type;
struct unbiased_flag_type
{
	byte chance;
	byte set;
	u32b flag;
	bool (*test)(object_ctype *);
	bias_type bias[4];
};

typedef struct biased_activation_type biased_activation_type;
struct biased_activation_type
{
	byte bias;
	s16b chance;
	byte activation;
};

/* Fake flags for apply_special_bonus below. */
#define TR0_RANDART_DEFEND 1
#define TR0_RANDART_ATTACK 2

/*
 * Hack - add in a bonus for a randart which doesn't set a flag.
 */ 
static void apply_special_bonus(object_type *o_ptr, u32b flag)
{
	switch (flag)
	{
		case TR0_RANDART_DEFEND:
		{
			o_ptr->flags3 |= TR3_SHOW_MODS;
			o_ptr->to_a = rand_range(5, 15);
			break;
		}
		case TR0_RANDART_ATTACK:
		{
			o_ptr->flags3 |= TR3_SHOW_MODS;
			o_ptr->to_h += rand_range(5, 15);
			o_ptr->to_d += rand_range(5, 15);
			break;
		}
	}
}

/*
 * Return TRUE if this object should be used as a "weapon-style" randart rather
 * than an "armour-style" one.
 */
static bool PURE is_weapon(object_ctype *o_ptr)
{
	switch (wield_slot(o_ptr))
	{
		case INVEN_LEFT:
		case INVEN_RIGHT:
		case INVEN_NECK:
		case INVEN_LITE:
		case INVEN_BODY:
		case INVEN_OUTER:
		case INVEN_ARM:
		case INVEN_HEAD:
		case INVEN_HANDS:
		case INVEN_FEET:
			return FALSE;
		default:
			return TRUE;
	}
}

/*
 * Return TRUE if this is a melee weapon.
 */
static bool PURE melee_weapon(object_ctype *o_ptr)
{
	return (wield_slot(o_ptr) == INVEN_WIELD);
}

/*
 * Return TRUE if this is an "edged" weapon.
 */
static bool PURE edged_weapon(object_ctype *o_ptr)
{
	return (melee_weapon(o_ptr) && o_ptr->tval != TV_HAFTED);
}

/*
 * Return TRUE if this is a "sword".
 */
static bool PURE is_sword(object_ctype *o_ptr)
{
	return (o_ptr->tval == TV_SWORD);
}

/*
 * Return TRUE if a magical sheath can be added to a new randart.
 */
static bool PURE sheath_possible(object_ctype *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_CLOAK:
		case TV_SOFT_ARMOR:
		case TV_HARD_ARMOR:
			return TRUE;
		default:
			return FALSE;
	}
}

/*
 * Return TRUE if a randart can gain the REFLECT flag.
 */
static bool PURE reflect_possible(object_ctype *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_SHIELD:
		case TV_CLOAK:
		case TV_HELM:
		case TV_HARD_ARMOR:
			return TRUE;
		default:
			return FALSE;
	}
}

/*
 * Hack - handle WEIRD_LUCK as an abort request rather than by making the
 * choice less likely as this gives an extra chance to roll for the biased
 * resistances.
 */
static bool PURE weird_test(object_ctype UNUSED *o_ptr)
{
	return one_in(WEIRD_LUCK);
}

#define NONE {0, 0} /* Make empty bias_types more obvious. */

static biased_flag_type biased_pval_flags[] =
{
	{BIAS_WARRIOR, 1, TR1, TR1_STR, NULL},
	{BIAS_WARRIOR, 1, TR1, TR1_CON, NULL},
	{BIAS_WARRIOR, 1, TR1, TR1_DEX, NULL},
	{BIAS_MAGE, 1, TR1, TR1_INT, NULL},
	{BIAS_PRIESTLY, 1, TR1, TR1_WIS, NULL},
	{BIAS_RANGER, 1, TR1, TR1_CON, NULL},
	{BIAS_RANGER, 1, TR1, TR1_DEX, NULL},
	{BIAS_RANGER, 1, TR1, TR1_STR, NULL},
	{BIAS_ROGUE, 1, TR1, TR1_STEALTH, NULL},
	{BIAS_ROGUE, 1, TR1, TR1_SEARCH, NULL},
	{BIAS_STR, 1, TR1, TR1_STR, NULL},
	{BIAS_INT, 1, TR1, TR1_INT, NULL},
	{BIAS_WIS, 1, TR1, TR1_WIS, NULL},
	{BIAS_DEX, 1, TR1, TR1_DEX, NULL},
	{BIAS_CON, 1, TR1, TR1_CON, NULL},
	{BIAS_CHR, 1, TR1, TR1_CHR, NULL},
};

static unbiased_flag_type unbiased_pval_flags[] =
{
	{2, TR1, TR1_STR, NULL, {{BIAS_STR, 84}, {BIAS_WARRIOR, 1}, {0, 6}, NONE}},
	{2, TR1, TR1_INT, NULL, {{BIAS_INT, 84}, {BIAS_MAGE, 1}, {0, 6}, NONE}},
	{2, TR1, TR1_WIS, NULL, {{BIAS_WIS, 84}, {BIAS_PRIESTLY, 1}, {0, 6}, NONE}},
	{2, TR1, TR1_DEX, NULL, {{BIAS_DEX, 84}, {BIAS_ROGUE, 1}, {0, 6}, NONE}},
	{2, TR1, TR1_CON, NULL, {{BIAS_CON, 108}, {BIAS_RANGER, 1}, {0, 8}, NONE}},
	{2, TR1, TR1_CHR, NULL, {{BIAS_CHR, 12}, {0, 1}, NONE, NONE}},
	{2, TR1, TR1_STEALTH, NULL, {{BIAS_ROGUE, 1}, {0, 2}, NONE, NONE}},
	{2, TR1, TR1_SEARCH, NULL, {{BIAS_RANGER, 1}, {0, 8}, NONE, NONE}},
	{2, TR1, TR1_INFRA, NULL, {{0, 1}, NONE, NONE, NONE}},
	{1, TR1, TR1_SPEED, NULL, {{BIAS_ROGUE, 1}, {0, 10}, NONE, NONE}},
	{2, TR1, TR1_TUNNEL, is_weapon, {{0, 1}, NONE, NONE, NONE}},
	{2, TR1, TR1_BLOWS, melee_weapon, {{BIAS_WARRIOR, 1}, {0, 10}, NONE, NONE}},
};

static biased_flag_type biased_resistance_list[] =
{
	{BIAS_ACID, 1, TR2, TR2_RES_ACID, NULL},
	{BIAS_ACID, BIAS_LUCK, TR2, TR2_IM_ACID, NULL},
	{BIAS_ELEC, 1, TR2, TR2_RES_ELEC, NULL},
	{BIAS_ELEC, 1, TR3, TR3_SH_ELEC, sheath_possible},
	{BIAS_ELEC, BIAS_LUCK, TR2, TR2_IM_ELEC, NULL},
	{BIAS_FIRE, 1, TR2, TR2_RES_FIRE, NULL},
	{BIAS_FIRE, 1, TR3, TR3_SH_FIRE, sheath_possible},
	{BIAS_FIRE, BIAS_LUCK, TR2, TR2_IM_FIRE, NULL},
	{BIAS_COLD, 1, TR2, TR2_RES_COLD, NULL},
	{BIAS_COLD, BIAS_LUCK, TR2, TR2_IM_COLD, NULL},
	{BIAS_POIS, 1, TR2, TR2_RES_POIS, NULL},
	{BIAS_WARRIOR, 3, TR2, TR2_RES_FEAR, NULL},
	{BIAS_WARRIOR, 3, TR3, TR3_NO_MAGIC, NULL},
	{BIAS_NECROMANTIC, 1, TR2, TR2_RES_NETHER, NULL},
	{BIAS_NECROMANTIC, 1, TR2, TR2_RES_POIS, NULL},
	{BIAS_NECROMANTIC, 1, TR2, TR2_RES_DARK, NULL},
	{BIAS_CHAOS, 1, TR2, TR2_RES_CHAOS, NULL},
	{BIAS_CHAOS, 1, TR2, TR2_RES_CONF, NULL},
	{BIAS_CHAOS, 1, TR2, TR2_RES_DISEN, NULL},
};

/*
 * Hack - add_resistance() depends on knowing various ranges within this table,
 * and so there are comments to indicate the significant ones.
 */
static unbiased_flag_type unbiased_resistance_list[] =
{
	{1, TR2, TR2_IM_ACID, weird_test, {{BIAS_ACID, 1}, NONE, NONE, NONE}},
	{1, TR2, TR2_IM_ELEC, weird_test, {{BIAS_ELEC, 1}, NONE, NONE, NONE}},
	{1, TR2, TR2_IM_COLD, weird_test, {{BIAS_COLD, 1}, NONE, NONE, NONE}},
	{1, TR2, TR2_IM_FIRE, weird_test, {{BIAS_FIRE, 1}, NONE, NONE, NONE}},
	/* 4 */
	{3, TR2, TR2_RES_ACID, NULL, {{BIAS_ACID, 1}, NONE, NONE, NONE}},
	{3, TR2, TR2_RES_ELEC, NULL, {{BIAS_ELEC, 1}, NONE, NONE, NONE}},
	{3, TR2, TR2_RES_COLD, NULL, {{BIAS_COLD, 1}, NONE, NONE, NONE}},
	{3, TR2, TR2_RES_FIRE, NULL, {{BIAS_FIRE, 1}, NONE, NONE, NONE}},
	/* 16 */
	{2, TR2, TR2_RES_POIS, NULL,
		{{BIAS_POIS, 12}, {BIAS_NECROMANTIC, 2}, {BIAS_ROGUE, 1}, {0, 1}}},
	/* 18 */
	{2, TR2, TR2_RES_FEAR, NULL, {{BIAS_WARRIOR, 1}, {0, 2}, NONE, NONE}},
	{1, TR2, TR2_RES_LITE, NULL, {{0, 1}, NONE, NONE, NONE}},
	{1, TR2, TR2_RES_DARK, NULL, {{0, 1}, NONE, NONE, NONE}},
	{2, TR2, TR2_RES_BLIND, NULL, {{0, 1}, NONE, NONE, NONE}},
	{2, TR2, TR2_RES_CONF, NULL, {{BIAS_CHAOS, 1}, {0, 5}, NONE, NONE}},
	{2, TR2, TR2_RES_SOUND, NULL, {{0, 1}, NONE, NONE, NONE}},
	{2, TR2, TR2_RES_SHARDS, NULL, {{0, 1}, NONE, NONE, NONE}},
	{2, TR2, TR2_RES_NETHER, NULL, {{BIAS_NECROMANTIC, 1}, {0, 2}, NONE, NONE}},
	{2, TR2, TR2_RES_NEXUS, NULL, {{0, 1}, NONE, NONE, NONE}},
	{2, TR2, TR2_RES_CHAOS, NULL, {{BIAS_CHAOS, 1}, {0, 1}, NONE, NONE}},
	{2, TR2, TR2_RES_DISEN, NULL, {{0, 1}, NONE, NONE, NONE}},
	/* 38 */
	{1, TR3, TR3_SH_ELEC, sheath_possible, {{BIAS_ELEC, 1}, NONE, NONE, NONE}},
	{1, TR3, TR3_SH_FIRE, sheath_possible, {{BIAS_FIRE, 1}, NONE, NONE, NONE}},
	{1, TR2, TR2_REFLECT, reflect_possible, {{0, 1}, NONE, NONE, NONE}},
	/* 41 */
};

static biased_flag_type biased_misc_flags[] =
{
	{BIAS_RANGER, 1, TR2, TR2_SUST_CON, NULL},
	{BIAS_STR, 1, TR2, TR2_SUST_STR, NULL},
	{BIAS_WIS, 1, TR2, TR2_SUST_WIS, NULL},
	{BIAS_INT, 1, TR2, TR2_SUST_INT, NULL},
	{BIAS_DEX, 1, TR2, TR2_SUST_DEX, NULL},
	{BIAS_CON, 1, TR2, TR2_SUST_CON, NULL},
	{BIAS_CHR, 1, TR2, TR2_SUST_CHR, NULL},
	{BIAS_CHAOS, 1, TR3, TR3_TELEPORT, NULL},
	{BIAS_FIRE, 3, TR3, TR3_LITE, NULL},
};

static unbiased_flag_type unbiased_misc_flags[] =
{
	{1, TR2, TR2_SUST_STR, NULL, {{BIAS_STR, 1}, NONE, NONE, NONE}},
	{1, TR2, TR2_SUST_INT, NULL, {{BIAS_INT, 1}, NONE, NONE, NONE}},
	{1, TR2, TR2_SUST_WIS, NULL, {{BIAS_WIS, 1}, NONE, NONE, NONE}},
	{1, TR2, TR2_SUST_DEX, NULL, {{BIAS_DEX, 1}, NONE, NONE, NONE}},
	{1, TR2, TR2_SUST_CON, NULL, {{BIAS_CON, 1}, NONE, NONE, NONE}},
	{1, TR2, TR2_SUST_CHR, NULL, {{BIAS_CHR, 1}, NONE, NONE, NONE}},
	{3, TR2, TR2_FREE_ACT, NULL, {{0, 1}, NONE, NONE, NONE}},
	{1, TR2, TR2_HOLD_LIFE, NULL,
		{{BIAS_PRIESTLY, 3}, {BIAS_NECROMANTIC, 2}, {0, 10}, NONE}},
	{2, TR3, TR3_LITE, NULL, {{0, 1}, NONE, NONE, NONE}},
	{2, TR3, TR3_FEATHER, NULL, {{0, 1}, NONE, NONE, NONE}},
	{3, TR3, TR3_SEE_INVIS, NULL, {{0, 1}, NONE, NONE, NONE}},
	{1, TR3, TR3_TELEPATHY, NULL, {{BIAS_MAGE, 1}, {0, 8}, NONE, NONE}},
	{2, TR3, TR3_SLOW_DIGEST, NULL, {{0, 1}, NONE, NONE, NONE}},
	{2, TR3, TR3_REGEN, NULL, {{0, 1}, NONE, NONE, NONE}},
	{1, TR3, TR3_TELEPORT, NULL, {{0, 1}, NONE, NONE, NONE}},

	{3, TR0, TR0_RANDART_DEFEND, is_weapon, {{0, 1}, NONE, NONE, NONE}},
	{3, TR0, TR0_RANDART_ATTACK, NULL, {{0, 1}, NONE, NONE, NONE}},

	{1, TR3, TR3_NO_MAGIC, NULL, {{0, 1}, NONE, NONE, NONE}},
	{1, TR3, TR3_NO_TELE, NULL, {{0, 1}, NONE, NONE, NONE}},
};

static biased_flag_type biased_slay_flags[] =
{
	{BIAS_CHAOS, 1, TR1, TR1_CHAOTIC, NULL},
	{BIAS_PRIESTLY, 1, TR3, TR3_BLESSED, edged_weapon},
	{BIAS_NECROMANTIC, 1, TR1, TR1_VAMPIRIC, NULL},
	{BIAS_NECROMANTIC, 2, TR1, TR1_BRAND_POIS, NULL},
	{BIAS_RANGER, 1, TR1, TR1_SLAY_ANIMAL, NULL},
	{BIAS_ROGUE, 1, TR1, TR1_BRAND_POIS, NULL},
	{BIAS_POIS, 1, TR1, TR1_BRAND_POIS, NULL},
	{BIAS_FIRE, 1, TR1, TR1_BRAND_FIRE, NULL},
	{BIAS_COLD, 1, TR1, TR1_BRAND_COLD, NULL},
	{BIAS_ELEC, 1, TR1, TR1_BRAND_ELEC, NULL},
	{BIAS_ACID, 1, TR1, TR1_BRAND_ACID, NULL},
	{BIAS_LAW, 1, TR1, TR1_SLAY_EVIL, NULL},
	{BIAS_LAW, 1, TR1, TR1_SLAY_UNDEAD, NULL},
	{BIAS_LAW, 1, TR1, TR1_SLAY_DEMON, NULL},
};

static unbiased_flag_type unbiased_slay_flags[] =
{
	{2, TR1, TR1_SLAY_ANIMAL, NULL, {{0, 1}, NONE, NONE, NONE}},
	{2, TR1, TR1_SLAY_EVIL, NULL,
		{{BIAS_LAW, 9}, {BIAS_PRIESTLY, 1}, {0, 8}, NONE}},
	{2, TR1, TR1_SLAY_UNDEAD, NULL, {{BIAS_PRIESTLY, 1}, {0, 8}, NONE, NONE}},
	{2, TR1, TR1_SLAY_DEMON, NULL, {{BIAS_PRIESTLY, 1}, {0, 8}, NONE, NONE}},
	{2, TR1, TR1_SLAY_ORC, NULL, {{0, 1}, NONE, NONE, NONE}},
	{2, TR1, TR1_SLAY_TROLL, NULL, {{0, 1}, NONE, NONE, NONE}},
	{2, TR1, TR1_SLAY_GIANT, NULL, {{0, 1}, NONE, NONE, NONE}},
	{2, TR1, TR1_SLAY_DRAGON, NULL, {{0, 1}, NONE, NONE, NONE}},
	{1, TR1, TR1_KILL_DRAGON, NULL, {{0, 1}, NONE, NONE, NONE}},
	{2, TR1, TR1_VORPAL, is_sword, {{BIAS_WARRIOR, 1}, {0, 8}, NONE, NONE}},
	{2, TR1, TR1_IMPACT, NULL, {{0, 1}, NONE, NONE, NONE}},
	{2, TR1, TR1_BRAND_ACID, NULL, {{BIAS_ACID, 1}, NONE, NONE, NONE}},
	{2, TR1, TR1_BRAND_ELEC, NULL, {{BIAS_ELEC, 1}, NONE, NONE, NONE}},
	{2, TR1, TR1_BRAND_COLD, NULL, {{BIAS_COLD, 1}, NONE, NONE, NONE}},
	{2, TR1, TR1_BRAND_FIRE, NULL, {{BIAS_FIRE, 1}, NONE, NONE, NONE}},
	{2, TR1, TR1_BRAND_POIS, NULL,
		{{BIAS_POIS, 12}, {BIAS_NECROMANTIC, 1}, {BIAS_ROGUE, 5}, NONE}},
	{2, TR1, TR1_VAMPIRIC, NULL, {{BIAS_NECROMANTIC, 1}, NONE, NONE, NONE}},
	{2, TR1, TR1_CHAOTIC, NULL, {{BIAS_CHAOS, 1}, NONE, NONE, NONE}},
};

static unbiased_flag_type unbiased_bow_flags[] =
{
	{1, TR3, TR3_XTRA_MIGHT, NULL, {{BIAS_RANGER, 1}, {0, 9}, NONE, NONE}},
	{1, TR3, TR3_XTRA_SHOTS, NULL, {{BIAS_RANGER, 1}, {0, 9}, NONE, NONE}},
};

static biased_activation_type biased_activations[] =
{
	{BIAS_ELEC, 10, ACT_BO_ELEC_1},
	{BIAS_ELEC, 4, ACT_BA_ELEC_2},
	{BIAS_ELEC, 1, ACT_BA_ELEC_3},
	{BIAS_POIS, 1, ACT_BA_POIS_1},
	{BIAS_FIRE, 10, ACT_BO_FIRE_1},
	{BIAS_FIRE, 4, ACT_BA_FIRE_1},
	{BIAS_FIRE, 1, ACT_BA_FIRE_2},
	{BIAS_COLD, 18, ACT_BO_COLD_1},
	{BIAS_COLD, 6, ACT_BA_COLD_1},
	{BIAS_COLD, 2, ACT_BA_COLD_2},
	{BIAS_COLD, 1, ACT_BA_COLD_3},
	{BIAS_CHAOS, 6, 0},
	{BIAS_CHAOS, 5, ACT_CALL_CHAOS},
	{BIAS_CHAOS, 1, ACT_SUMMON_DEMON},
	{BIAS_PRIESTLY, 5, ACT_CURE_MW},
	{BIAS_PRIESTLY, 1, ACT_CHARM_UNDEAD},
	{BIAS_PRIESTLY, 1, ACT_BANISH_EVIL},
	{BIAS_PRIESTLY, 1, ACT_DISP_EVIL},
	{BIAS_PRIESTLY, 1, ACT_PROT_EVIL},
	{BIAS_PRIESTLY, 1, ACT_CURE_1000},
	{BIAS_PRIESTLY, 1, ACT_CURE_700},
	{BIAS_PRIESTLY, 1, ACT_REST_ALL},
	{BIAS_PRIESTLY, 1, ACT_REST_LIFE},
	{BIAS_NECROMANTIC, 11200, ACT_VAMPIRE_1},
	{BIAS_NECROMANTIC, 2340, ACT_MASS_GENO},
	{BIAS_NECROMANTIC, 2340, ACT_GENOCIDE},
	{BIAS_NECROMANTIC, 2240, ACT_CHARM_UNDEAD},
	{BIAS_NECROMANTIC, 1755, ACT_DISP_GOOD},
	{BIAS_NECROMANTIC, 1680, ACT_VAMPIRE_2},
	{BIAS_NECROMANTIC, 1260, ACT_SUMMON_UNDEAD},
	{BIAS_NECROMANTIC, 351, ACT_WRAITH},
	{BIAS_LAW, 21, ACT_PROT_EVIL},
	{BIAS_LAW, 7, ACT_DISP_EVIL},
	{BIAS_LAW, 4, ACT_BANISH_EVIL},
	{BIAS_ROGUE, 343, ACT_ID_PLAIN},
	{BIAS_ROGUE, 196, ACT_SLEEP},
	{BIAS_ROGUE, 196, ACT_DETECT_ALL},
	{BIAS_ROGUE, 49, ACT_ID_FULL},
	{BIAS_ROGUE, 16, ACT_SPEED},
	{BIAS_MAGE, 22572, ACT_ESP},
	{BIAS_MAGE, 17000, 0},
	{BIAS_MAGE, 5643, ACT_RUNE_EXPLO},
	{BIAS_MAGE, 3135, ACT_SUMMON_PHANTOM},
	{BIAS_MAGE, 1650, ACT_SUMMON_ELEMENTAL},
	{BIAS_WARRIOR, 396, ACT_BERSERK},
	{BIAS_WARRIOR, 100, 0},
	{BIAS_WARRIOR, 4, ACT_INVULN},
	{BIAS_RANGER, 570, ACT_CURE_POISON},
	{BIAS_RANGER, 285, ACT_RESIST_ALL},
	{BIAS_RANGER, 285, ACT_SATIATE},
	{BIAS_RANGER, 228, ACT_SUMMON_ANIMAL},
	{BIAS_RANGER, 228, ACT_CHARM_ANIMAL},
	{BIAS_RANGER, 84, ACT_CHARM_ANIMALS},
};

static byte unbiased_activations[][2] =
{
	{100, ACT_SUNLIGHT},
	{100, ACT_BO_MISS_1},
	{100, ACT_BA_POIS_1},
	{100, ACT_BO_ELEC_1},
	{100, ACT_BO_ACID_1},
	{100, ACT_BO_COLD_1},
	{100, ACT_BO_FIRE_1},
	{100, ACT_CONFUSE},
	{100, ACT_SLEEP},
	{100, ACT_QUAKE},
	{100, ACT_CURE_LW},
	{100, ACT_CURE_MW},
	{100, ACT_CURE_POISON},
	{100, ACT_BERSERK},
	{100, ACT_LIGHT},
	{100, ACT_MAP_LIGHT},
	{100, ACT_DEST_DOOR},
	{100, ACT_STONE_MUD},
	{100, ACT_TELEPORT},
	{84, ACT_BA_COLD_1},
	{84, ACT_BA_FIRE_1},
	{84, ACT_DRAIN_1},
	{84, ACT_TELE_AWAY},
	{84, ACT_ESP},
	{84, ACT_RESIST_ALL},
	{84, ACT_DETECT_ALL},
	{84, ACT_RECALL},
	{84, ACT_SATIATE},
	{84, ACT_RECHARGE},
	{74, ACT_TERROR},
	{74, ACT_PROT_EVIL},
	{74, ACT_ID_PLAIN},
	{65, ACT_DRAIN_2},
	{65, ACT_VAMPIRE_1},
	{65, ACT_BO_MISS_2},
	{65, ACT_BA_FIRE_2},
	{65, ACT_REST_LIFE},
	{49, ACT_BA_COLD_3},
	{49, ACT_BA_ELEC_3},
	{49, ACT_WHIRLWIND},
	{49, ACT_VAMPIRE_2},
	{49, ACT_CHARM_ANIMAL},
	{39, ACT_SUMMON_ANIMAL},
	{32, ACT_DISP_EVIL},
	{32, ACT_BA_MISS_3},
	{32, ACT_DISP_GOOD},
	{32, ACT_BANISH_EVIL},
	{32, ACT_GENOCIDE},
	{32, ACT_MASS_GENO},
	{32, ACT_CHARM_UNDEAD},
	{32, ACT_CHARM_OTHER},
	{32, ACT_SUMMON_PHANTOM},
	{32, ACT_REST_ALL},
	{32, ACT_RUNE_EXPLO},
	{24, ACT_CALL_CHAOS},
	{24, ACT_SHARD},
	{24, ACT_CHARM_ANIMALS},
	{24, ACT_CHARM_OTHERS},
	{24, ACT_SUMMON_ELEMENTAL},
	{24, ACT_CURE_700},
	{24, ACT_SPEED},
	{24, ACT_ID_FULL},
	{24, ACT_RUNE_PROT},
	{9, ACT_CURE_1000},
	{9, ACT_XTRA_SPEED},
	{9, ACT_DETECT_XTRA},
	{9, ACT_DIM_DOOR},
	{4, ACT_SUMMON_UNDEAD},
	{4, ACT_SUMMON_DEMON},
	{4, ACT_WRAITH},
	{4, ACT_INVULN},
	{4, ACT_ALCHEMY},
};

/*
 * Give a random bonus to an object. Return FALSE if the object chosen is
 * ineligible for this bonus.
 * This isn't actually possible at present, but the code looks at the return.
 */
static void give_activation_power(object_type *o_ptr, int *bias)
{
	int type;
	long max = 0;
	biased_activation_type *ptr;

	/* Count up the chance in this array for this bias. */
	FOR_ALL_IN(biased_activations, ptr)
	{
		if (ptr->bias == *bias) max += ptr->chance;
	}

	/* Found some to choose between. */
	if (max)
	{
		/* Pick one element. */
		max = rand_int(max);

		/* Find it. */
		FOR_ALL_IN(biased_activations, ptr)
		{
			if (ptr->bias != *bias) continue;
			max -= ptr->chance;
			if (max < 0) break;
		}

		/* Remember the choice (which may be 0). */
		type = ptr->activation;
	}
	else
	{
		/* No choice yet. */
		type = 0;
	}

	/* Pick an unbiased power at random. */
	if (!type)
	{
		byte (*this)[2];
		FOR_ALL_IN(unbiased_activations, this) max += **this;
		max = rand_int(max);
		FOR_ALL_IN(unbiased_activations, this)
		{
			max -= **this;
			if (max < 0) break;
		}

		type = (*this)[1];
	}

	/* A type was chosen... */
	o_ptr->activation = type;
	o_ptr->flags3 |= TR3_ACTIVATE;
	o_ptr->timeout = 0;
}

/*
 * Create a name from syllables in one of two files.
 */
static void get_table_name(char * out_string)
{
	/* Set up the parameters for the name set to use. */
	bool set2 = !rand_int(3);
	cptr str = "", file = (set2) ? "scroll.txt" : "elvish.txt";
	int num = (set2) ? rand_range(2,4) : rand_range(2,3);

	/* Build up the name. */
	while (num--)
	{
		str = format("%s%v", str, get_rnd_line_f1, file);
	}

	/* Copy the name across, obeying the length limit set in defines.h
	 * and the size of new_name.
	 */
	strnfmt(out_string, 80, "'%.*^s'", MAX_TABLE_LEN-2, str);

	return;
}

static void get_random_name(char * return_name, bool armour, int power)
{
	if (randint(100)<=TABLE_NAME)
	get_table_name(return_name);
	else
	{
	char NameFile[16];
	switch (armour)
	{
	case 1:
		switch(power)
		{
	case 0:
	strcpy(NameFile, "a_cursed.txt");
	break;
		case 1:
		strcpy(NameFile, "a_low.txt");
		break;
		case 2:
		strcpy(NameFile, "a_med.txt");
		break;
		default:
		strcpy(NameFile, "a_high.txt");
		}
	break;
	default:
		switch(power)
		{
	case 0:
	strcpy(NameFile, "w_cursed.txt");
	break;
		case 1:
		strcpy(NameFile, "w_low.txt");
		break;
		case 2:
		strcpy(NameFile, "w_med.txt");
		break;
		default:
		strcpy(NameFile, "w_high.txt");
		}

	}

	strnfmt(return_name, 80, "%v", get_rnd_line_f1, NameFile);
	}
}

/*
 * Find the flag set which the numerical flag set requests refers to.
 */
static u32b *set_to_flag(object_type *o_ptr, int set)
{
	switch (set)
	{
		case TR1: return &o_ptr->flags1;
		case TR2: return &o_ptr->flags2;
		case TR3: return &o_ptr->flags3;
		default: assert(!"Unknown flag set"); return NULL;
	}
}	

/*
 * Add the specified resistance, expressed as a random element from
 * unbiased_resistance_list.
 */
void add_resistance(object_type *o_ptr, int min, int max)
{
	unbiased_flag_type *ptr;

	assert(min > 0 && max <= 41 && min <= max); /* Caller */

	/* Pick an element in the range. */
	max = rand_range(min-1, max-1);

	/* Find it. */
	for (ptr = unbiased_resistance_list; max >= 0; ptr++) max -= ptr->chance;

	/* ptr is always incremented before the final check. */
	ptr--;

	/* Paranoia - the parameters chosen should exclude unsuitable flags. */
	if (ptr->test && !(ptr->test)(o_ptr)) return;

	if (ptr->set == TR0)
	{
		apply_special_bonus(o_ptr, ptr->flag);
	}
	else
	{
		/* Find the flag. */
		u32b *flag = set_to_flag(o_ptr, ptr->set);

		/* Set it, if necessary. */
		*flag |= ptr->flag;
	}
}

/*
 * Add one or more flags to an artefact based on the given bias.
 * Return TRUE if no more resistances should be added in this step (see below).
 */
static bool biased_bonus(object_type *o_ptr, int bias,
	biased_flag_type *array, int num)
{
	biased_flag_type *ptr;

	for (ptr = array; ptr < array+num; ptr++)
	{
		/* Wrong bias. */
		if (ptr->bias != bias) continue;

		/* Not a suitable flag to set. */
		if (ptr->test && !(ptr->test)(o_ptr)) continue;

		/* Random failure. */
		if (!one_in(ptr->chance)) continue;

		if (ptr->set == TR0)
		{
			apply_special_bonus(o_ptr, ptr->flag);
		}
		else
		{
			/* Find the flag. */
			u32b *flag = set_to_flag(o_ptr, ptr->set);

			/* Flag already fully set. */
			if ((*flag & ptr->flag) == ptr->flag) continue;

			/* Set it. */
			*flag |= ptr->flag;
		}

		/* Give a 50% chance of finishing at each such flag addition. */
		if (one_in(2)) return TRUE;
	}

	/* Continue to the next step. */
	return FALSE;
}

/*
 * Add one or more flags to an artefact without reference to the bias. This
 * may set the bias to a value, so it is still needed.
 */
static bool unbiased_bonus(object_type *o_ptr, int *bias,
	unbiased_flag_type *array, int num)
{
	unbiased_flag_type *ptr;
	int i, max;

	/* Count up the chance for this array. */
	for (ptr = array, max = 0; ptr < array+num; ptr++) max += ptr->chance;

	/* Pick one element. */
	max = rand_int(max);

	/* Find it. */
	for (ptr = array; ptr < array+num && max >= 0; ptr++) max -= ptr->chance;

	/* ptr is always incremented before the final check. */
	ptr--;

	/* Not a suitable flag to set. */
	if (ptr->test && !(ptr->test)(o_ptr)) return FALSE;

	if (ptr->set == TR0)
	{
		apply_special_bonus(o_ptr, ptr->flag);
	}
	else
	{
		/* Find the flag. */
		u32b *flag = set_to_flag(o_ptr, ptr->set);

		/* Set it, if necessary. */
		*flag |= ptr->flag;
	}

	/* Pick a bias, if necessary. */
	if (!*bias)
	{
		int max = 0;
		int num = N_ELEMENTS(ptr->bias);

		/* Pick a random element from ptr->bias given their relative chances. */
		for (i = 0; i < num; i++) max += ptr->bias[i].chance;
		max = rand_int(max);
		for (i = 0; i < num && max >= 0; i++) max -= ptr->bias[i].chance;

		/* Set the current bias appropriately. */
		*bias = ptr->bias[i-1].bias;
	}

	/* Finish here. */
	return TRUE;
}

/*
 * Apply any biased bonuses which are appropriate, and add an unbiased bonus
 * if the biased ones do not contain a "finish" request.
 * Return FALSE if the latter fails.
 */
static bool combined_bonus(object_type *o_ptr, int *bias,
	biased_flag_type *barray, int bnum, unbiased_flag_type *uarray, int unum)
{
	return ((bnum && biased_bonus(o_ptr, *bias, barray, bnum)) ||
		(unum && unbiased_bonus(o_ptr, bias, uarray, unum)));
}

bool create_artifact(object_type *o_ptr, bool a_scroll)
{
	char new_name[80] = "";
	int artifact_bias, has_pval = 0;
	int powers = rand_range(2, 6);
	int max_type = is_weapon(o_ptr) ? 7 : 5;
	int power_level;
	s32b total_flags;
	bool a_cursed = FALSE;

	if (!a_scroll || !one_in(4))
	{
		artifact_bias = 0;
	}
	else if (percent(cp_ptr->art2_chance))
	{
		artifact_bias = cp_ptr->art2_bias;
	}
	else
	{
		artifact_bias = cp_ptr->art1_bias;
	}

	if (!a_scroll && one_in(A_CURSED)) a_cursed = TRUE;

	while (one_in(powers) || one_in(7) || one_in(10)) powers++;

	if ((!a_cursed) && (randint(WEIRD_LUCK)==1)) powers *= 2;

	if (a_cursed) powers /= 2;

	/* Main loop */

	while(powers--)
	{
		switch (randint(max_type))
		{
			case 1: case 2:
				while (!combined_bonus(o_ptr, &artifact_bias,
					ARRAY(biased_pval_flags), ARRAY(unbiased_pval_flags))) ;
				has_pval = TRUE;
				break;
			case 3: case 4:
				while (!combined_bonus(o_ptr, &artifact_bias,
					ARRAY(biased_resistance_list),
					ARRAY(unbiased_resistance_list))) ;
				break;
			case 5:
				while (!combined_bonus(o_ptr, &artifact_bias,
					ARRAY(biased_misc_flags), ARRAY(unbiased_misc_flags))) ;
				break;
			case 6: case 7:
				if (o_ptr->tval == TV_BOW)
					while (!combined_bonus(o_ptr, &artifact_bias,
						NULL, 0, ARRAY(unbiased_bow_flags))) ;
				else
					while (!combined_bonus(o_ptr, &artifact_bias,
						ARRAY(biased_slay_flags), ARRAY(unbiased_slay_flags))) ;
				break;
			default:
				if(cheat_wzrd) msg_print ("Switch error in create_artifact!");
				powers++;
		}
	}

	if (has_pval)
	{
		if (o_ptr->flags1 & TR1_BLOWS)
			o_ptr->pval = randint(2) + 1;
		else
		{
			do
			{ o_ptr->pval++; }
			while (o_ptr->pval<randint(5) || randint(o_ptr->pval)==1);
		}
		if (o_ptr->pval > 4 && (randint(WEIRD_LUCK)!=1))
			o_ptr->pval = 4;
	}

	/* give it some plusses... */
	if (!is_weapon(o_ptr))
		o_ptr->to_a += randint(o_ptr->to_a>19?1:20-o_ptr->to_a);
	else
	{
		o_ptr->to_h += randint(o_ptr->to_h>19?1:20-o_ptr->to_h);
		o_ptr->to_d += randint(o_ptr->to_d>19?1:20-o_ptr->to_d);
	}
	o_ptr->flags3 |= (TR3_IGNORE_ALL); /* Just to be sure */
	total_flags = flag_cost(o_ptr, TRUE);
	if (cheat_peek) msg_format("%ld", total_flags);

	if (a_cursed) curse_artifact(o_ptr);

	if (!a_cursed && one_in(!is_weapon(o_ptr) ? ACTIVATION_CHANCE * 2
		: ACTIVATION_CHANCE))
	{
		o_ptr->activation = 0;
		give_activation_power(o_ptr, &artifact_bias);
	}


	if (!is_weapon(o_ptr))
	{
		if (a_cursed) power_level = 0;
		else if (total_flags<10000) power_level = 1;
		else if (total_flags<20000) power_level = 2;
		else power_level = 3;
	}
	else
	{
		if (a_cursed) power_level = 0;
		else if (total_flags<15000) power_level = 1;
		else if (total_flags<30000) power_level = 2;
		else power_level = 3;
	}

	if (a_scroll)
	{
		char dummy_name[80];
		strcpy(dummy_name, "");
		identify_fully_aux(o_ptr, FALSE);
		o_ptr->ident |= IDENT_STOREB; /* This will be used later on... */
		if (!(get_string("What do you want to call the artifact? ",
			dummy_name, 80)))
			strcpy(new_name,"(a DIY Artifact)");
		else
		{
			strcpy(new_name,"called '");
			strcat(new_name,dummy_name);
			strcat(new_name,"'");
		}
		/* Identify it fully */
		object_aware(o_ptr);
		object_known(o_ptr);

		/* Mark the item as fully known */
		o_ptr->ident |= (IDENT_MENTAL);

	}

	else

		get_random_name(new_name, !is_weapon(o_ptr), power_level);

	if (cheat_xtra)
	{
		if (artifact_bias)
			msg_format("Biased artifact: %d.", artifact_bias);
		else
			msg_print("No bias in artifact.");
	}

	/* Save the inscription */
	o_ptr->art_name = quark_add(new_name);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP);
	return TRUE;
}


bool artifact_scroll(void)
{
	errr err;
	bool            okay = FALSE;

	object_type             *o_ptr;

	C_TNEW(o_name, ONAME_MAX, char);


	/* Assume enchant weapon */
	item_tester_hook = item_tester_hook_weapon;

	/* Get an item (from equip or inven or floor) */
	if (!((o_ptr = get_item(&err, "Enchant which item? ", TRUE, TRUE, TRUE))))
	{
		if (err == -2) msg_print("You have nothing to enchant.");
		TFREE(o_name);
		return (FALSE);
	}


	/* Description */
	strnfmt(o_name, ONAME_MAX, "%v", object_desc_f3, o_ptr, FALSE, 0);

	/* Describe */
	msg_format("%s %s radiate%s a blinding light!",
			(is_inventory_p(o_ptr)) ? "Your" : "The", o_name,
			((o_ptr->number > 1) ? "" : "s"));

	if (o_ptr->name1 || o_ptr->art_name)
	{
		msg_format("The %s %s already %s!",
			o_name, ((o_ptr->number > 1) ? "are" : "is"),
			((o_ptr->number > 1) ? "artifacts" : "an artifact"));
		okay = FALSE;
	}

	else if (o_ptr->name2)
	{
		msg_format("The %s %s already %s!",
			o_name, ((o_ptr->number > 1) ? "are" : "is"),
			((o_ptr->number > 1) ? "ego items" : "an ego item"));
		okay = FALSE;
	}

	else
	{
		if (o_ptr->number > 1)
		{
			msg_print("Not enough enough energy to enchant more than one "
				"object!");
			msg_format("%d of your %s %s destroyed!", (o_ptr->number)-1,
				o_name, (o_ptr->number>2?"were":"was"));
			o_ptr->number = 1;
		}
		okay = create_artifact(o_ptr, TRUE);
	}

	/* Failure */
	if (!okay)
	{
		/* Flush */
		if (flush_failure) flush();

		/* Message */
		msg_print("The enchantment failed.");
	}
	else
	{
		/* Forget obsolete stacking information. */
		set_stack_number(o_ptr);
	}

	TFREE(o_name);
	/* Something happened */
	return (TRUE);
}

/*
 * Describe an item, giving its location.
 */
static void ident_describe(object_type *o_ptr)
{
	/* Describe */
	switch (find_object(o_ptr))
	{
		case OUP_EQUIP: case OUP_POUCH:
		{
			msg_format("%^s: %v (%c).", describe_use(o_ptr),
				object_desc_f3, o_ptr, TRUE, 3, index_to_label(o_ptr));
			break;
		}
		/* Remove the verb from describe_use() for inventory. */
		case OUP_INVEN:
		{
			msg_format("In your pack: %v (%c).",
				object_desc_f3, o_ptr, TRUE, 3, index_to_label(o_ptr));
			break;
		}
		case OUP_FLOOR:
		{
			msg_format("On the ground: %v.", object_desc_f3, o_ptr, TRUE, 3);
			break;
		}
	}
}

/*
 * Identify an object in the inventory (or on the floor)
 * This routine does *not* automatically combine objects.
 * Returns TRUE if something was identified, else FALSE.
 */
bool ident_spell(void)
{
	errr                     err;

	object_type             *o_ptr;

	/* Only unidentified items */
	item_tester_hook = item_tester_unknown;

	/* Get an item (from equip or inven or floor) */
	if (!((o_ptr = get_item(&err, "Identify which item? ", TRUE, TRUE, TRUE))))
	{
		if (err == -2) msg_print("You have nothing to identify.");
		return (FALSE);
	}


	/* Identify it fully */
	object_aware(o_ptr);
	object_known(o_ptr);

	/* Recalculate/redraw stuff (later) */
	update_object(o_ptr);

	/* Describe the object. */
	ident_describe(o_ptr);

	/* Something happened */
	return (TRUE);
}



void do_identify_fully(object_type *o_ptr)
{
	/* Identify it fully */
	object_aware(o_ptr);
	object_known(o_ptr);

	/* Mark the item as fully known */
	o_ptr->ident |= (IDENT_MENTAL);

	/* Recalculate/redraw stuff (later) */
	update_object(o_ptr);

	/* Handle stuff */
	handle_stuff();

	/* Describe the object. */
	ident_describe(o_ptr);

	/* Describe it fully */
	identify_fully_aux(o_ptr, FALSE);
}

/*
 * Fully "identify" an object in the inventory  -BEN-
 * This routine returns TRUE if an item was identified.
 */
bool identify_fully(void)
{
	object_type *o_ptr = get_object_from_function(do_identify_fully);

	if (!o_ptr) return FALSE;

	do_identify_fully(o_ptr);

	return TRUE;
}




/*
 * Hook for "get_item()".  Determine if something is rechargable.
 */
bool PURE item_tester_hook_recharge(object_ctype *o_ptr)
{
	/* Recharge staffs */
	if (o_ptr->tval == TV_STAFF) return (TRUE);

	/* Recharge wands */
	if (o_ptr->tval == TV_WAND) return (TRUE);

	/* Hack -- Recharge rods */
	if (o_ptr->tval == TV_ROD) return (TRUE);

	/* Nope */
	return (FALSE);
}


/*
 * Recharge a wand/staff/rod from the pack or on the floor.
 *
 * Scroll of recharging --> recharge(60)
 *
 * recharge(20) = 1/6 failure for empty 10th level wand
 * recharge(60) = 1/10 failure for empty 10th level wand
 *
 * It is harder to recharge high level, and highly charged wands.
 *
 * XXX XXX XXX Beware of "sliding index errors".
 *
 * Should probably not "destroy" over-charged items, unless we
 * "replace" them by, say, a broken stick or some such.  The only
 * reason this is okay is because "scrolls of recharging" appear
 * BEFORE all staffs/wands/rods in the inventory.  Note that the
 * new "auto_sort_pack" option would correctly handle replacing
 * the "broken" wand with any other item (i.e. a broken stick).
 *
 * XXX XXX XXX Perhaps we should auto-unstack recharging stacks.
 */
bool recharge(int num)
{
	errr err;
	int                 i, t,  lev;

	object_type             *o_ptr;


	/* Only accept legal items */
	item_tester_hook = item_tester_hook_recharge;

	/* Get an item (from inven or floor) */
	if (!((o_ptr = get_item(&err, "Recharge which item? ", TRUE, TRUE, TRUE))))
	{
		if (err == -2) msg_print("You have nothing to recharge.");
		return (FALSE);
	}


	/* Extract the object "level" */
	lev = k_info[o_ptr->k_idx].extra;

	/* Recharge a rod */
	if (o_ptr->tval == TV_ROD)
	{
		/* Extract a recharge power */
		i = (100 - lev + num) / 5;

		/* Paranoia -- prevent crashes */
		if (i < 1) i = 1;

		/* Back-fire */
		if (rand_int(i) == 0)
		{
			/* Hack -- backfire */
			msg_print("The recharge backfires, draining the rod further!");

			/* Hack -- decharge the rod */
			if (o_ptr->timeout < 10000)
				o_ptr->timeout = (o_ptr->timeout + 100) * 2;
		}

		/* Recharge */
		else
		{
			/* Rechange amount */
			t = (num * damroll(2, 4));

			/* Recharge by that amount */
			if (o_ptr->timeout > t)
			{
				o_ptr->timeout -= t;
			}

			/* Fully recharged */
			else
			{
				o_ptr->timeout = 0;
			}
		}
	}

	/* Recharge wand/staff */
	else
	{
		/* Recharge power */
		i = (num + 100 - lev - (10 * o_ptr->pval)) / 15;

		/* Paranoia -- prevent crashes */
		if (i < 1) i = 1;

		/* Back-fire XXX XXX XXX */
		if (rand_int(i) == 0)
		{
			/* Dangerous Hack -- Destroy the item */
			msg_print("There is a bright flash of light.");

			/* Reduce and describe item */
			item_increase(o_ptr, -999);
			item_describe(o_ptr);
			item_optimize(o_ptr);
		}

		/* Recharge */
		else
		{
			/* Extract a "power" */
			t = (num / (lev + 2)) + 1;

			/* Recharge based on the power */
			if (t > 0) o_ptr->pval += 2 + randint(t);

			/* Hack -- we no longer "know" the item */
			o_ptr->ident &= ~(IDENT_KNOWN);

			/* Hack -- we no longer think the item is empty */
			o_ptr->ident &= ~(IDENT_EMPTY);
		}
	}

	/* Recalculate/redraw stuff (later) */
	update_object(o_ptr);

	/* Forget obsolete stacking information. */
	set_stack_number(o_ptr);

	/* Something was done */
	return (TRUE);
}








/*
 * Apply a "project()" directly to all viewable monsters
 *
 * Note that affected monsters are NOT auto-tracked by this usage.
 */
static bool project_hack(int typ, int dam)
{
	int             i, x, y;

	int             flg = PROJECT_JUMP | PROJECT_KILL | PROJECT_HIDE;

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
		if (project(0, 0, y, x, dam, typ, flg)) obvious = TRUE;
	}

	/* Result */
	return (obvious);
}


/*
 * Speed monsters
 */
bool speed_monsters(void)
{
	return (project_hack(GF_OLD_SPEED, (skill_set[SKILL_DEVICE].value/2)));
}

/*
 * Slow monsters
 */
bool slow_monsters(int dam)
{
	return (project_hack(GF_OLD_SLOW, dam));
}

/*
 * Sleep monsters
 */
bool sleep_monsters(int dam)
{
	return (project_hack(GF_OLD_SLEEP, dam));
}


/*
 * Banish evil monsters
 */
bool banish_evil(int dist)
{
	return (project_hack(GF_AWAY_EVIL, dist));
}


/*
 * Dispel undead monsters
 */
bool dispel_undead(int dam)
{
	return (project_hack(GF_DISP_UNDEAD, dam));
}

/*
 * Dispel evil monsters
 */
bool dispel_evil(int dam)
{
	return (project_hack(GF_DISP_EVIL, dam));
}

/*
 * Dispel good monsters
 */
bool dispel_good(int dam)
{
	return (project_hack(GF_DISP_GOOD, dam));
}

/*
 * Dispel all monsters
 */
bool dispel_monsters(int dam)
{
	return (project_hack(GF_DISP_ALL, dam));
}

/*
 * Dispel 'living' monsters
 */
bool dispel_living(int dam)
{
	return (project_hack(GF_DISP_LIVING, dam));
}

/*
 * Dispel demons
 */
bool dispel_demons(int dam)
{
	return (project_hack(GF_DISP_DEMON, dam));
}


/*
 * Wake up all monsters, and speed up "los" monsters.
 * mw_ptr is only used for a comparison, so both NULL and m_list aggravate
 * every monster.
 */
void aggravate_monsters(monster_type *mw_ptr)
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
		if (m_ptr == mw_ptr) continue;

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
			if (m_ptr->smart & SM_ALLY)
			{
				if (randint(2)==1)
				{
					m_ptr->smart &= ~SM_ALLY;
				}
			}
		}
	}

	/* Messages */
	if (speed) msg_print("You feel a sudden stirring nearby!");
	else if (sleep) msg_print("You hear a sudden stirring in the distance!");
}



/*
 * Delete all non-unique monsters of a given "type" from the level
 * Return POWER_ERROR_* value.
 */
errr genocide(bool player_cast)
{
	int             i;

	char    typ;

	errr result = POWER_ERROR_FAIL;

	/* Mega-Hack -- Get a monster symbol */
	if (!get_com(&typ, "Choose a monster race (by symbol) to genocide: "))
		return POWER_ERROR_ABORT;

	/* Delete the monsters of that "type" */
	for (i = 1; i < m_max; i++)
	{
		monster_type    *m_ptr = &m_list[i];
		monster_race    *r_ptr = &r_info[m_ptr->r_idx];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Hack -- Skip Unique Monsters */
		if (r_ptr->flags1 & (RF1_UNIQUE)) continue;

		/* Skip "wrong" monsters */
		if (r_ptr->gfx.dc != typ) continue;

		/* Skip Quest Monsters - Dean Anderson */
		if (r_ptr->flags1 & RF1_GUARDIAN) continue;

		/* Delete the monster */
		delete_monster_idx(i,TRUE);

		if (player_cast)
		{
			/* Take damage */
			take_hit(randint(4), "the strain of casting Genocide",
				MON_CASTING_GENOCIDE);
		}

		/* Visual feedback */
		move_cursor_relative(py, px);

		/* Redraw */
		p_ptr->redraw |= (PR_HP);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER);

		/* Handle */
		handle_stuff();

		/* Fresh */
		Term_fresh();

		/* Delay */
		Term_xtra(TERM_XTRA_DELAY, delay_factor);

		/* Take note */
		result = SUCCESS;
	}

	return (result);
}


/*
 * Delete all nearby (non-unique) monsters
 */
errr mass_genocide(bool player_cast)
{
	int             i;

	errr result = POWER_ERROR_FAIL;


	/* Delete the (nearby) monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type    *m_ptr = &m_list[i];
		monster_race    *r_ptr = &r_info[m_ptr->r_idx];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Hack -- Skip unique monsters */
		if (r_ptr->flags1 & (RF1_UNIQUE)) continue;

		/* Skip Quest Monsters - Dean Anderson */
		if (r_ptr->flags1 & RF1_GUARDIAN) continue;

		/* Skip distant monsters */
		if (m_ptr->cdis > MAX_SIGHT) continue;

		/* Delete the monster */
		delete_monster_idx(i,TRUE);

		if (player_cast)
		{
			/* Hack -- visual feedback */
			take_hit(randint(3), "the strain of casting Mass Genocide",
				MON_CASTING_MASS_GENOCIDE);
		}

		move_cursor_relative(py, px);

		/* Redraw */
		p_ptr->redraw |= (PR_HP);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER);

		/* Handle */
		handle_stuff();

		/* Fresh */
		Term_fresh();

		/* Delay */
		Term_xtra(TERM_XTRA_DELAY, delay_factor);

		/* Note effect */
		result = SUCCESS;
	}

	return (result);
}



/*
 * Probe nearby monsters
 */
bool probing(void)
{
	int            i;

	bool    probe = FALSE;


	/* Probe all (nearby) monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Require line of sight */
		if (!player_has_los_bold(m_ptr->fy, m_ptr->fx)) continue;

		/* Probe visible monsters */
		if (m_ptr->ml)
		{
			/* Start the message */
			if (!probe) msg_print("Probing...");

			/* Describe the monster */
			msg_format("%^v has %d hit points.", monster_desc_f2, m_ptr, 0x04,
				m_ptr->hp);

			/* Learn all of the non-spell, non-treasure flags */
			lore_do_probe(m_ptr->r_idx);

			/* Probe worked */
			probe = TRUE;
		}
	}

	/* Done */
	if (probe)
	{
		msg_print("That's all.");
	}

	/* Result */
	return (probe);
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
			c_ptr->info &= ~(CAVE_ICKY);

			/* Lose light and knowledge */
			c_ptr->info &= ~(CAVE_MARK | CAVE_GLOW | CAVE_TRAP);

			/* Hack -- Notice player affect */
			if ((x == px) && (y == py))
			{
				/* Hurt the player later */
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
			(void)add_flag(TIMED_BLIND, 10 + randint(10));
		}
	}


	/* Mega-Hack -- Forget the view and lite */
	p_ptr->update |= (PU_UN_VIEW | PU_UN_LITE);

	/* Update stuff */
	p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_ROOM);

	/* Update the monsters */
	p_ptr->update |= (PU_MONSTERS);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);
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
	int             i, t, y, x, yy, xx, dy, dx;

	int             damage = 0;

	int             sn = 0, sy = 0, sx = 0;

	bool    hurt = FALSE;

	cave_type       *c_ptr;

	bool    map[32][32];


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
			c_ptr->info &= ~(CAVE_ICKY);

			/* Lose light and knowledge */
			c_ptr->info &= ~(CAVE_GLOW | CAVE_MARK | CAVE_TRAP);

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
			y = py + ddy_ddd[i];
			x = px + ddx_ddd[i];

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
					(void)add_flag(TIMED_STUN, randint(50));
					break;
				}
				case 3:
				{
					msg_print("You are crushed between the floor and ceiling!");
					damage = damroll(10, 4);
					(void)add_flag(TIMED_STUN, randint(50));
					break;
				}
			}

			/* Move the player to the safe location */
			move_to(sy, sx);
		}

		/* Important -- no wall on player */
		map[16+py-cy][16+px-cx] = FALSE;

		/* Take some damage */
		if (damage) take_hit(damage, "an earthquake", MON_EARTHQUAKE);
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
					/* Assume not safe */
					sn = 0;

					/* Monster can move to escape the wall */
					if (!(r_ptr->flags1 & (RF1_NEVER_MOVE)))
					{
						/* Look for safety */
						for (i = 0; i < 8; i++)
						{
							/* Access the grid */
							y = yy + ddy_ddd[i];
							x = xx + ddx_ddd[i];

							/* Skip non-empty grids */
							if (!cave_empty_bold(y, x)) continue;

							/* Hack -- no safety on glyph of warding */
							if (cave[y][x].feat == FEAT_GLYPH) continue;
							if (cave[y][x].feat == FEAT_MINOR_GLYPH) continue;

							/* ... nor on the Pattern */
							if ((cave[y][x].feat <= FEAT_PATTERN_XTRA2) &&
								(cave[y][x].feat >= FEAT_PATTERN_START))
								continue;

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

					/* Scream in pain */
					msg_format("%^v wails out in pain!",
						monster_desc_f2, m_ptr, 0);

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
						msg_format("%^v is embedded in the rock!",
							monster_desc_f2, m_ptr, 0);

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
	p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_ROOM);

	/* Monster may be able to be created in the floor squares created. */
	full_grid = MAX(full_grid, 2*r);

	/* Update the monsters */
	p_ptr->update |= (PU_DISTANCE);

	/* Update the health bar */
	p_ptr->redraw |= (PR_HEALTH);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);
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
					/* Dump a message */
					msg_format("%^v wakes up.", monster_desc_f2, m_ptr, 0);
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
 * Mark area to be lit by lite_room() with CAVE_TEMP.
 *
 * This works by lighting the central 9 squares unconditionally, and then
 * lighting others if:
 * 1. Two adjacent non-wall squares are lit.
 * 2. Three adjacent squares which touch one another are lit, the middle one
 * of which is not a wall. This is provided solely to light walls.
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
static void lite_room(int y1, int x1)
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
	(void)project(0, rad, py, px, dam, GF_LITE_WEAK, flg);

	/* Hack - don't light up the town. */
	if (dun_level || !is_town_p(wildy,wildx))
	{
		/* Lite up the room */
		lite_room(py, px);
	}

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
	(void)project(0, rad, py, px, dam, GF_DARK_WEAK, flg);

	/* Hack - don't darken the town. */
	if (dun_level || !is_town_p(wildy,wildx))
	{
		/* Darken the room */
		unlite_room(py, px);
	}

	/* Assume seen */
	return (TRUE);
}

/*
 * MVD hook for ball spells.
 */
static int PURE mvd_no_visible_monster(int y, int x, int UNUSED d)
{
	/* Stop before the first wall. */
	if (!cave_floor_bold(y, x)) return MVD_STOP_BEFORE_HERE;

	/* Stop at the first monster. */
	if (cave[y][x].m_idx && m_list[cave[y][x].m_idx].ml) return MVD_STOP_HERE;

	/* Don't stop at all otherwise. */
	return MVD_CONTINUE;
}


/*
 * Cast a ball spell
 * Can be aimed at any projectable() monster.
 * If aimed in a direction, it will hit the first wall or visible monster along
 * its path.
 * 
 * Affect grids, objects, and monsters in its blast radius.
 */
bool fire_ball(int typ, int dir, int dam, int rad)
{
	int x, y, flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

	/* Extract the target. */
	get_dir_target(&x, &y, dir, mvd_no_visible_monster);

	/* Analyze the "dir" and the "target".  Hurt items on floor. */
	return (project(0, rad, y, x, dam, typ, flg));
}


/*
 * Hack -- apply a "projection()" in a direction (or at the target)
 */
static bool project_hook(int typ, int dir, int dam, int flg)
{
	int tx, ty;

	/* Pass through the target if needed */
	flg |= (PROJECT_THRU);

	/* Find the target co-ordinates. */
	get_dir_target(&tx, &ty, dir, NULL);

	/* Analyze the "dir" and the "target", do NOT explode */
	return (project(0, 0, ty, tx, dam, typ, flg));
}


/*
 * Cast a bolt spell
 * Stop if we hit a monster, as a "bolt"
 * Affect monsters (not grids or objects)
 */
bool fire_bolt(int typ, int dir, int dam)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(typ, dir, dam, flg));
}

/*
 * Cast a beam spell
 * Pass through monsters, as a "beam"
 * Affect monsters (not grids or objects)
 */
bool fire_beam(int typ, int dir, int dam)
{
	int flg = PROJECT_BEAM | PROJECT_KILL;
	return (project_hook(typ, dir, dam, flg));
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
	return (project_hook(GF_LITE_WEAK, dir, damroll(6, 8), flg));
}

bool wall_to_mud(int dir)
{
	int flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;
	return (project_hook(GF_KILL_WALL, dir, 20 + randint(30), flg));
}

bool wizard_lock(int dir)
{
	int flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;
	return (project_hook(GF_JAM_DOOR, dir, 20 + randint(30), flg));
}

bool destroy_door(int dir)
{
	int flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM;
	return (project_hook(GF_KILL_DOOR, dir, 0, flg));
}

bool disarm_trap(int dir)
{
	int flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM;
	return (project_hook(GF_KILL_TRAP, dir, 0, flg));
}

/*
 * Hooks -- affect adjacent grids (radius 1 ball attack)
 */

bool door_creation(void)
{
	int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;
	return (project(0, 1, py, px, 0, GF_MAKE_DOOR, flg));
}

bool trap_creation(void)
{
	int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;
	return (project(0, 1, py, px, 0, GF_MAKE_TRAP, flg));
}

bool glyph_creation(void)
{   int flg = PROJECT_GRID | PROJECT_ITEM;
	return (project(0, 1, py, px, 0, GF_MAKE_GLYPH, flg));
}

bool wall_stone(void)
{
	int flg = PROJECT_GRID | PROJECT_ITEM;

	bool dummy = (project(0, 1, py, px, 0, GF_STONE_WALL, flg));

	/* Update stuff */
	p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW);

	/* Update the monsters */
	p_ptr->update |= (PU_MONSTERS);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	return dummy;
}


bool destroy_doors_touch(void)
{
	int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;
	return (project(0, 1, py, px, 0, GF_KILL_DOOR, flg));
}

bool sleep_monsters_touch(int dam)
{
	int flg = PROJECT_KILL | PROJECT_HIDE;
	return (project(0, 1, py, px, dam, GF_OLD_SLEEP, flg));
}







void call_chaos(int plev)
{
	int Chaos_type, dummy, dir;
	bool line_chaos = FALSE;

	int hurt_types[30] =
	{ GF_ELEC,      GF_POIS,    GF_ACID,    GF_COLD,
		GF_FIRE,      GF_MISSILE, GF_ARROW,   GF_PLASMA,
		GF_HOLY_FIRE, GF_WATER,   GF_LITE,    GF_DARK,
		GF_FORCE,     GF_INERTIA, GF_MANA,    GF_METEOR,
		GF_ICE,       GF_CHAOS,   GF_NETHER,  GF_DISENCHANT,
		GF_SHARDS,    GF_SOUND,   GF_NEXUS,   GF_CONFUSION,
		GF_TIME,      GF_GRAVITY, GF_SHARD,  GF_NUKE,
		GF_HELL_FIRE, GF_DISINTEGRATE };

			Chaos_type = hurt_types[((randint (30))-1)];
		if (randint(4)==1) line_chaos = TRUE;

			if (randint(6)==1)
		{
				for (dummy = 1; dummy<10; dummy++)

				{
		if (dummy-5)
			{
			if (line_chaos)
					fire_beam(Chaos_type, dummy, 75);
			else
				fire_ball(Chaos_type, dummy,
				75, 2);
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
					fire_beam(Chaos_type, dir, 150);
			else
				fire_ball(Chaos_type, dir,
					150, 3 + (plev/35));
			}
}

void activate_ty_curse(void)
{
	int i = 0;
	do
	{
	switch(randint(27))
	{
		case 1: case 2: case 3: case 16: case 17:
		aggravate_monsters(NULL);
		if (randint(6)!=1) break;
		case 4: case 5: case 6:
		activate_hi_summon();
		if (randint(6)!=1) break;
		case 7: case 8: case 9: case 18:
		(void) summon_specific(py, px, (dun_depth), 0);
		if (randint(6)!=1) break;
		case 10: case 11: case 12:
		msg_print("You feel your life draining away...");
		lose_skills(10);
		if (randint(6)!=1) break;
		case 13: case 14: case 15: case 19: case 20:
		if (p_ptr->free_act && (randint(100) < p_ptr->skill_sav))
		{
			skill_exp(SKILL_SAVE);
			/* Do nothing */ ;
		}
		else
		{
			msg_print("You feel like a statue!");
			if (p_ptr->free_act)
				add_flag(TIMED_PARALYZED, randint(3));
			else
				add_flag(TIMED_PARALYZED, randint(13));
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
		summon_reaver();
		break;
		default:
		while (i<6)
		{
		do { (void)do_dec_stat(i); } while (randint(2)==1);
		i++;
		}
	}
	}   while (randint(3)==1);

}

void activate_hi_summon(void)
{
int i = 0;

	for (i = 0; i < (randint(9) + ((dun_depth) / 40)); i++)
	{
	switch(randint(26) + ((dun_depth) / 20) )
	{
	case 1: case 2:
		(void) summon_specific(py, px, (dun_depth), SUMMON_ANT);
		break;
	case 3: case 4:
		(void) summon_specific(py, px, (dun_depth), SUMMON_SPIDER);
		break;
	case 5: case 6:
		(void) summon_specific(py, px, (dun_depth), SUMMON_HOUND);
		break;
	case 7: case 8:
		(void) summon_specific(py, px, (dun_depth), SUMMON_HYDRA);
		break;
	case 9: case 10:
		(void) summon_specific(py, px, (dun_depth), SUMMON_CTHULOID);
		break;
	case 11: case 12:
		(void) summon_specific(py, px, (dun_depth), SUMMON_UNDEAD);
		break;
	case 13: case 14:
	(void) summon_specific(py, px, (dun_depth), SUMMON_DRAGON);
		break;
	case 15: case 16:
	(void) summon_specific(py, px, (dun_depth), SUMMON_DEMON);
		break;
	case 17:
	(void) summon_specific(py, px, (dun_depth), SUMMON_GOO);
		break;
	case 18: case 19:
	(void) summon_specific(py, px, (dun_depth), SUMMON_UNIQUE);
		break;
	case 20: case 21:
	(void) summon_specific(py, px, (dun_depth), SUMMON_HI_UNDEAD);
		break;
	case 22: case 23:
	(void) summon_specific(py, px, (dun_depth), SUMMON_HI_DRAGON);
		break;
	case 24: case 25:
	(void) summon_specific(py, px, 100, SUMMON_REAVER);
		break;
	default:
		summon_specific(py, px,( ( ( (dun_depth) * 3) / 2 ) + 5 ), 0);
	}
	}
}

void summon_reaver(void)
{
	int i = 0;
	int max_reaver = ((dun_depth) / 50) + randint(6);

		for (i = 0; i < max_reaver; i++)
			{
			(void)summon_specific(py, px, 100, SUMMON_REAVER);
			}
}


void wall_breaker(int plev)
{

	int dummy = 5;

	if (randint(80+(plev))<70)
	{
	do { dummy = randint(9); }
	while ((dummy == 5) || (dummy == 0));
	wall_to_mud (dummy);
	}
	else if (randint(100)>30)
	{
	earthquake(py,px,1);
	}
	else
	{           for (dummy = 1; dummy<10; dummy++)

				{
		if (dummy-5)
			{
			wall_to_mud(dummy);

			}
				}
	}


}


void bless_weapon(void)
{
	errr err;
	object_type             *o_ptr;
	u32b f1, f2, f3;

	C_TNEW(o_name, ONAME_MAX, char);


	/* Assume enchant weapon */
	item_tester_hook = item_tester_hook_weapon;


	/* Get an item (from equip or inven or floor) */
	if (!((o_ptr = get_item(&err, "Bless which weapon? ", TRUE, TRUE, TRUE))))
	{
		if (err == -2) msg_print("You have weapon to bless.");
		TFREE(o_name);
		return;
	}


	/* Description */
	strnfmt(o_name, ONAME_MAX, "%v", object_desc_f3, o_ptr, FALSE, 0);

		/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);

	if (o_ptr->ident & (IDENT_CURSED))
	{

		if ( ( (f3 &  (TR3_HEAVY_CURSE)) && ( randint (100) < 33 ) )
			|| (f3 & (TR3_PERMA_CURSE)) )
		{

			msg_format("The black aura on %s %s disrupts the blessing!",
					(is_inventory_p(o_ptr) ? "your" : "the"), o_name);
			TFREE(o_name);
			return;
		}

		msg_format("A malignant aura leaves %s %s.",
			(is_inventory_p(o_ptr) ? "your" : "the"), o_name);

		/* Uncurse it */
		o_ptr->ident &= ~(IDENT_CURSED);

		/* Hack -- Assume felt */
		o_ptr->ident |= (IDENT_SENSE_CURSED);

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
					(is_inventory_p(o_ptr) ? "Your" : "The"), o_name,
					((o_ptr->number > 1) ? "were" : "was"));
			TFREE(o_name);
			return;
	}

	if (!(o_ptr->art_name || o_ptr->name1) || (randint(3)==1))
	{
	/* Describe */
	msg_format("%s %s shine%s!",
			(is_inventory_p(o_ptr) ? "Your" : "The"), o_name,
			((o_ptr->number > 1) ? "" : "s"));
	o_ptr->flags3 |= TR3_BLESSED;
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
					(is_inventory_p(o_ptr) ? "Your" : "The"), o_name,
					((o_ptr->number > 1) ? "were" : "was"));
		}

	}
	/* Forget obsolete stacking information. */
	set_stack_number(o_ptr);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Window stuff */
	p_ptr->window |= (PW_EQUIP | PW_PLAYER);

	TFREE(o_name);
}

/*
 * Detect all "nonliving", "undead" or "demonic" monsters on current panel
 */
bool detect_monsters_nonliving(void)
{
	int             i, y, x;

	bool    flag = FALSE;


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
			if (!live_monster_p(r_ptr))
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

	/* Update window */
	p_ptr->window |= PW_VISIBLE;
	}

	/* Result */
	return (flag);
}


/*
 * Prepare to recall to/from the dungeon after a few turns.
 *
 * The "spell" parameter increased the time to wait slightly (by an average
 * of half a turn). I don't know why...
 */
void set_recall(bool spell)
{
	if (dun_level && (p_ptr->max_dlv > dun_level) &&
		(cur_dungeon == recall_dungeon))
	{
		if (get_check("Reset recall depth? "))
		p_ptr->max_dlv = dun_level;
	}
	if (p_ptr->word_recall == 0)
	{
		p_ptr->word_recall = randint(spell ? 21 : 20) + 15;
		if (dun_level > 0)
		{
			recall_dungeon = cur_dungeon;
		}
		else
		{
			cur_dungeon = recall_dungeon;
		}
		msg_print("The air about you becomes charged...");
	}
	else
	{
		p_ptr->word_recall = 0;
		msg_print("A tension leaves the air around you...");
	}
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


bool charm_monster(int dir, int plev)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_CHARM, dir, plev, flg));
}

bool control_one_undead(int dir, int plev)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_CONTROL_UNDEAD, dir, plev, flg));
}

bool charm_animal(int dir, int plev)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_CONTROL_ANIMAL, dir, plev, flg));
}


static int report_magics_aux(int dur)
{
	if (dur <= 5)
	{
		return 0;
	}
	else if (dur <= 10)
	{
		return 1;
	}
	else if (dur <= 20)
	{
		return 2;
	}
	else if (dur <= 50)
	{
		return 3;
	}
	else if (dur <= 100)
	{
		return 4;
	}
	else if (dur <= 200)
	{
		return 5;
	}
	else
	{
		return 6;
	}
}

static cptr report_magic_durations[] =
{
	"for a short time",
	"for a little while",
	"for a while",
	"for a long while",
	"for a long time",
	"for a very long time",
	"for an incredibly long time",
	"until you hit a monster"
};


void report_magics(void)
{
	int             i = 0, j, k;

	char Dummy[80];

	cptr    info[128];
	int     info2[128];

	if (p_ptr->blind)
	{
		info2[i]  = report_magics_aux(p_ptr->blind);
		info[i++] = "You cannot see";
	}
	if (p_ptr->confused)
	{
		info2[i]  = report_magics_aux(p_ptr->confused);
		info[i++] = "You are confused";
	}
	if (p_ptr->afraid)
	{
		info2[i]  = report_magics_aux(p_ptr->afraid);
		info[i++] = "You are terrified";
	}
	if (p_ptr->poisoned)
	{
		info2[i]  = report_magics_aux(p_ptr->poisoned);
		info[i++] = "You are poisoned";
	}
	if (p_ptr->image)
	{
		info2[i]  = report_magics_aux(p_ptr->image);
		info[i++] = "You are hallucinating";
	}

	if (p_ptr->blessed)
	{
		info2[i]  = report_magics_aux(p_ptr->blessed);
		info[i++] = "You feel rightous";
	}
	if (p_ptr->hero)
	{
		info2[i]  = report_magics_aux(p_ptr->hero);
		info[i++] = "You feel heroic";
	}
	if (p_ptr->shero)
	{
		info2[i]  = report_magics_aux(p_ptr->shero);
		info[i++] = "You are in a battle rage";
	}
	if (p_ptr->protevil)
	{
		info2[i]  = report_magics_aux(p_ptr->protevil);
		info[i++] = "You are protected from evil";
	}
	if (p_ptr->shield)
	{
		info2[i]  = report_magics_aux(p_ptr->shield);
		info[i++] = "You are protected by a mystic shield";
	}
	if (p_ptr->invuln)
	{
		info2[i]  = report_magics_aux(p_ptr->invuln);
		info[i++] = "You are invulnerable";
	}
	if (p_ptr->wraith_form)
	{
		info2[i]  = report_magics_aux(p_ptr->wraith_form);
		info[i++] = "You are incorporeal";
	}
	if (p_ptr->confusing)
	{
		info2[i]  = 7;
		info[i++] = "Your hands are glowing dull red.";
	}
	if (p_ptr->word_recall)
	{
		info2[i]  = report_magics_aux(p_ptr->word_recall);
		info[i++] = "You waiting to be recalled";
	}
	if (p_ptr->oppose_acid)
	{
		info2[i]  = report_magics_aux(p_ptr->oppose_acid);
		info[i++] = "You are resistant to acid";
	}
	if (p_ptr->oppose_elec)
	{
		info2[i]  = report_magics_aux(p_ptr->oppose_elec);
		info[i++] = "You are resistant to lightning";
	}
	if (p_ptr->oppose_fire)
	{
		info2[i]  = report_magics_aux(p_ptr->oppose_fire);
		info[i++] = "You are resistant to fire";
	}
	if (p_ptr->oppose_cold)
	{
		info2[i]  = report_magics_aux(p_ptr->oppose_cold);
		info[i++] = "You are resistant to cold";
	}
	if (p_ptr->oppose_pois)
	{
		info2[i]  = report_magics_aux(p_ptr->oppose_pois);
		info[i++] = "You are resistant to poison";
	}

	/* Save the screen */
	Term_save();

	/* Erase the screen */
	for (k = 1; k < 24; k++) prt("", k, 13);

	/* Label the information */
	prt("     Your Current Magic:", 1, 15);

	/* We will print on top of the map (column 13) */
	for (k = 2, j = 0; j < i; j++)
	{
		/* Show the info */
		sprintf( Dummy, "%s %s.", info[j],
			report_magic_durations[info2[j]] );
		prt(Dummy, k++, 15);

		/* Every 20 entries (lines 2 to 21), start over */
		if ((k == 22) && (j + 1 < i))
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
 * move_in_direction hook for teleport_swap.
 */
static int PURE mvd_one_step(int UNUSED y, int UNUSED x, int d)
{
	switch (d)
	{
		case 0: return MVD_CONTINUE;
		case 1: return MVD_STOP_HERE;
		default: return MVD_STOP_BEFORE_HERE;
	}
}

void teleport_swap(int dir)
{
	int tx, ty;
	cave_type * c_ptr;
	monster_type * m_ptr;
	monster_race * r_ptr;

	/* A direction always refers to an adjacent square. */
	get_dir_target(&tx, &ty, dir, mvd_one_step);

	c_ptr = &cave[ty][tx];

	if (!c_ptr->m_idx)
	{
		msg_print("You can't trade places with that!");
	}
	else
	{
		m_ptr = &m_list[c_ptr->m_idx];
		r_ptr = &r_info[m_ptr->r_idx];

		if (r_ptr->flags3 & RF3_RES_TELE)
		{
			msg_print("Your teleportation is blocked!");
		}
		else
		{
			sound(SOUND_TELEPORT);

			cave[py][px].m_idx = c_ptr->m_idx;

			/* Update the old location */
			c_ptr->m_idx = 0;

			/* Move the monster */
			m_ptr->fy = (byte)py;
			m_ptr->fx = (byte)px;

			/* Move the player */
			move_to(ty, tx);

			/* Update the monster (new location) */
			update_mon(cave[ty][tx].m_idx, TRUE);

			/* Handle stuff XXX XXX XXX */
			handle_stuff();
		}
	}
}

void alter_reality(void)
{
	msg_print("The world changes!");

	/* Leaving */
	change_level(dun_level, START_RANDOM);
}

/*
 * Hack -- Rerate Hitpoints
 */
void do_cmd_rerate(void)
{
	int i;
	int lastroll,j;

	/* Pre-calculate level 1 hitdice */
	player_hp[0] = p_ptr->hitdie;

	/* Roll out the hitpoints */

	/* 'Roll' the hitpoint values */
	lastroll = p_ptr->hitdie;
	for (i = 1; i < 100; i++)
	{
		player_hp[i]=lastroll;
		lastroll--;
		if(lastroll<1) lastroll = p_ptr->hitdie;
	}
	/* Now shuffle them */
	for(i=1;i<100;i++)
	{
		j=randint(99);
		lastroll=player_hp[i];
		player_hp[i]=player_hp[j];
		player_hp[j]=lastroll;
	}
	/* Make each a cumulative score */
	for(i=1;i<100;i++)
	{
	player_hp[i] = player_hp[i-1] +player_hp[i];
	}

	/* Update and redraw hitpoints */
	p_ptr->update |= (PU_HP);
	p_ptr->redraw |= (PR_HP);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);

	/* Handle stuff */
	handle_stuff();
}
