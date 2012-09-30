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
	int i = 0, j, k, x, height;
	int res;

	int v_nr;
	char v_string[8][128];

	u32b ff[4] = {0, 0, 0, 0};

	object_type *o_ptr;
	const mutation_type *mut_ptr;

	char Dummy[80], Liferating[80];

	cptr info[220];

	int plev = p_ptr->lev;

	int percent;

	Dummy[0] = 0;
	Liferating[0] = 0;

	percent = (int)(((long)p_ptr->player_hp[PY_MAX_LEVEL - 1] * 200L) /
					(2 * p_ptr->rp.hitdie +
					 ((PY_MAX_LEVEL - 1) * (p_ptr->rp.hitdie + 1))));

	strnfmt(Liferating, 80, "Your current Life Rating is %d/100.", percent);
	info[i++] = Liferating;

	chg_virtue(V_KNOWLEDGE, 1);
	chg_virtue(V_ENLIGHTEN, 1);

	/* Acquire item flags from equipment */
	for (k = 0; k < EQUIP_MAX; k++)
	{
		o_ptr = &p_ptr->equipment[k];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Extract flags */
		ff[0] |= o_ptr->flags[0];
		ff[1] |= o_ptr->flags[1];
		ff[2] |= o_ptr->flags[2];
		ff[3] |= o_ptr->flags[3];
	}

	for (v_nr = 0; v_nr < MAX_PLAYER_VIRTUES; v_nr++)
	{
		char virt_name[20];
		char vir_desc[80];
		int tester = p_ptr->virtues[v_nr];

		strcpy(virt_name, virtue[(p_ptr->vir_types[v_nr]) - 1]);

		strnfmt(vir_desc, 80, "Oops. No info about %s.", virt_name);
		if (tester < -100)
			strnfmt(vir_desc, 80, "You are the polar opposite of %s (%d).",
					virt_name, tester);
		else if (tester < -80)
			strnfmt(vir_desc, 80, "You are an arch-enemy of %s (%d).",
					virt_name, tester);
		else if (tester < -60)
			strnfmt(vir_desc, 80, "You are a bitter enemy of %s (%d).",
					virt_name, tester);
		else if (tester < -40)
			strnfmt(vir_desc, 80, "You are an enemy of %s (%d).",
					virt_name, tester);
		else if (tester < -20)
			strnfmt(vir_desc, 80, "You have sinned against %s (%d).",
					virt_name, tester);
		else if (tester < 0)
			strnfmt(vir_desc, 80, "You have strayed from the path of %s (%d).",
					virt_name, tester);
		else if (tester == 0)
			strnfmt(vir_desc, 80, "You are neutral to %s (%d).", virt_name, tester);
		else if (tester < 20)
			strnfmt(vir_desc, 80, "You are somewhat virtuous in %s (%d).",
					virt_name, tester);
		else if (tester < 40)
			strnfmt(vir_desc, 80, "You are virtuous in %s (%d).",
					virt_name, tester);
		else if (tester < 60)
			strnfmt(vir_desc, 80, "You are very virtuous in %s (%d).",
					virt_name, tester);
		else if (tester < 80)
			strnfmt(vir_desc, 80, "You are a champion of %s (%d).",
					virt_name, tester);
		else if (tester < 100)
			strnfmt(vir_desc, 80, "You are a great champion of %s (%d).",
					virt_name, tester);
		else
			strnfmt(vir_desc, 80, "You are the living embodiment of %s (%d).",
					virt_name, tester);

		strcpy(v_string[v_nr], vir_desc);

		info[i++] = v_string[v_nr];
	}

	/* Racial powers... */
	for (x = 0; x < MAX_RACE_POWERS; x++)
	{
		mut_ptr = &race_powers[x];

		if ((mut_ptr->which == p_ptr->rp.prace) && (plev >= mut_ptr->level))
		{
			info[i++] = mut_ptr->desc_text;
		}
	}

	/* Activatble mutations */
	for (x = 0; x < MUT_PER_SET * 3; x++)
	{
		mut_ptr = &mutations[x];

		/*
		 * Only show activatable mutations if we
		 * are of sufficiently high level
		 */
		if ((x < MUT_PER_SET) && (mut_ptr->level > plev)) continue;

		/* Do we have the mutation? */
		if (player_has_mut(x))
		{
			info[i++] = mut_ptr->desc_text;
		}
	}

	if (p_ptr->tim.blind)
	{
		info[i++] = "You cannot see.";
	}
	if (p_ptr->tim.confused)
	{
		info[i++] = "You are confused.";
	}
	if (p_ptr->tim.afraid)
	{
		info[i++] = "You are terrified.";
	}
	if (p_ptr->tim.cut)
	{
		info[i++] = "You are bleeding.";
	}
	if (p_ptr->tim.stun)
	{
		info[i++] = "You are stunned.";
	}
	if (p_ptr->tim.poisoned)
	{
		info[i++] = "You are poisoned.";
	}
	if (p_ptr->tim.image)
	{
		info[i++] = "You are hallucinating.";
	}
	if (FLAG(p_ptr, TR_AGGRAVATE))
	{
		info[i++] = "You aggravate monsters.";
	}
	if (FLAG(p_ptr, TR_TELEPORT))
	{
		info[i++] = "Your position is very uncertain.";
	}
	if (FLAG(p_ptr, TR_CANT_EAT))
	{
		info[i++] = "You cannot survive on normal food.";
	}
	if (p_ptr->tim.blessed)
	{
		info[i++] = "You feel righteous.";
	}
	if (p_ptr->tim.hero)
	{
		info[i++] = "You feel heroic.";
	}
	if (p_ptr->tim.shero)
	{
		info[i++] = "You are in a battle rage.";
	}
	if (p_ptr->tim.protevil || (FLAG(p_ptr, TR_SLAY_EVIL)))
	{
		info[i++] = "You are protected from evil.";
	}
	if (FLAG(p_ptr, TR_SLAY_ANIMAL))
	{
		info[i++] = "You are protected from animals.";
	}
	if (FLAG(p_ptr, TR_SLAY_UNDEAD))
	{
		info[i++] = "You are protected from undead.";
	}
	if (FLAG(p_ptr, TR_SLAY_DEMON))
	{
		info[i++] = "You are protected from demons.";
	}
	if (FLAG(p_ptr, TR_SLAY_ORC))
	{
		info[i++] = "You are protected from orcs.";
	}
	if (FLAG(p_ptr, TR_SLAY_TROLL))
	{
		info[i++] = "You are protected from trolls.";
	}
	if (FLAG(p_ptr, TR_SLAY_GIANT))
	{
		info[i++] = "You are protected from giants.";
	}
	if (FLAG(p_ptr, TR_SLAY_DRAGON))
	{
		info[i++] = "You are protected from dragons.";
	}
	if (p_ptr->tim.shield)
	{
		info[i++] = "You are protected by a mystic shield.";
	}
	if (p_ptr->tim.invuln)
	{
		info[i++] = "You are temporarily invulnerable.";
	}
	if (p_ptr->tim.wraith_form)
	{
		info[i++] = "You are temporarily incorporeal.";
	}
	if (p_ptr->state.confusing)
	{
		info[i++] = "Your hands are glowing dull red.";
	}
	if (p_ptr->state.searching)
	{
		info[i++] = "You are looking around very carefully.";
	}
	if (p_ptr->new_spells)
	{
		info[i++] = "You can learn some spells/prayers.";
	}
	if (p_ptr->tim.word_recall)
	{
		info[i++] = "You will soon be recalled.";
	}
	if (p_ptr->see_infra)
	{
		info[i++] = "Your eyes are sensitive to infrared light.";
	}
	if (FLAG(p_ptr, TR_SEE_INVIS))
	{
		info[i++] = "You can see invisible creatures.";
	}
	if (FLAG(p_ptr, TR_FEATHER))
	{
		info[i++] = "You can fly.";
	}
	if (FLAG(p_ptr, TR_FREE_ACT))
	{
		info[i++] = "You have free action.";
	}
	if (FLAG(p_ptr, TR_MUTATE))
	{
		info[i++] = "You mutate spontaneously.";
	}
	if (FLAG(p_ptr, TR_PATRON))
	{
		info[i++] = "You have a chaos patron.";
	}
	if (FLAG(p_ptr, TR_STRANGE_LUCK))
	{
		info[i++] = "Chance is warped around you.";
	}
	if (FLAG(p_ptr, TR_PASS_WALL))
	{
		info[i++] = "You can pass through solid rock.";
	}

	if ((FLAG(p_ptr, TR_REGEN)) && (!(p_ptr->muta3 & MUT3_REGEN)))
	{
		if (FLAG(p_ptr, TR_SLOW_HEAL))
			info[i++] = "You regenerate slowly.";
		else
			info[i++] = "You regenerate quickly.";
	}
	else if (FLAG(p_ptr, TR_SLOW_HEAL))
	{
		info[i++] = "You regenerate very slowly.";
	}
	
	if ((FLAG(p_ptr, TR_SLOW_DIGEST)) &&
		(FLAG(p_ptr, TR_SLOW_HEAL)))
	{
		info[i++] = "Your appetite is very small.";
	}
	else if ((FLAG(p_ptr, TR_SLOW_DIGEST)) ||
		(FLAG(p_ptr, TR_SLOW_HEAL)))
	{
		info[i++] = "Your appetite is small.";
	}
	
	if ((FLAG(p_ptr, TR_TELEPATHY)) && (!(p_ptr->muta3 & MUT3_ESP)))
	{
		info[i++] = "You have ESP.";
	}
	if (FLAG(p_ptr, TR_HOLD_LIFE))
	{
		info[i++] = "You have a firm hold on your life force.";
	}
	if (FLAG(p_ptr, TR_REFLECT))
	{
		info[i++] = "You reflect arrows and bolts.";
	}
	if (FLAG(p_ptr, TR_SH_FIRE))
	{
		info[i++] = "You are surrounded with a fiery aura.";
	}
	if (FLAG(p_ptr, TR_SH_ELEC))
	{
		info[i++] = "You are surrounded with electricity.";
	}
	if (FLAG(p_ptr, TR_SH_ACID))
	{
		info[i++] = "You are surrounded by an acidic cloud.";
	}
	if (FLAG(p_ptr, TR_SH_COLD))
	{
		info[i++] = "You are surrounded by a freezing aura.";
	}
	if (FLAG(p_ptr, TR_NO_MAGIC))
	{
		info[i++] = "You are surrounded by an anti-magic shell.";
	}
	if (FLAG(p_ptr, TR_NO_TELE))
	{
		info[i++] = "You cannot teleport.";
	}
	if (FLAG(p_ptr, TR_LITE))
	{
		info[i++] = "You have a source of permanent light.";
	}

	res = res_acid_lvl();
	if (res == 0)
	{
		info[i++] = "You are completely immune to acid.";
	}
	else if (res <= 25)
	{
		info[i++] = "You resist acid exceptionally well.";
	}
	else if (res <= 50)
	{
		info[i++] = "You are resistant to acid.";
	}
	else if (res < 100)
	{
		info[i++] = "You are somewhat resistant to acid.";
	}
	else if (res > 100)
	{
		info[i++] = "You are vulnerable to acid.";
	}

	res = res_elec_lvl();
	if (res == 0)
	{
		info[i++] = "You are completely immune to lightning.";
	}
	else if (res <= 25)
	{
		info[i++] = "You resist lightning exceptionally well.";
	}
	else if (res <= 50)
	{
		info[i++] = "You are resistant to lightning.";
	}
	else if (res < 100)
	{
		info[i++] = "You are somewhat resistant to lightning.";
	}
	else if (res > 100)
	{
		info[i++] = "You are vulnerable to lightning.";
	}

	res = res_fire_lvl();
	if (res == 0)
	{
		info[i++] = "You are completely immune to fire.";
	}
	else if (res <= 25)
	{
		info[i++] = "You resist fire exceptionally well.";
	}
	else if (res <= 50)
	{
		info[i++] = "You are resistant to fire.";
	}
	else if (res < 100)
	{
		info[i++] = "You are somewhat resistant to fire.";
	}
	else if (res > 100)
	{
		info[i++] = "You are vulnerable to fire.";
	}

	res = res_cold_lvl();
	if (res == 0)
	{
		info[i++] = "You are completely immune to cold.";
	}
	else if (res <= 25)
	{
		info[i++] = "You resist cold exceptionally well.";
	}
	else if (res <= 50)
	{
		info[i++] = "You are resistant to cold.";
	}
	else if (res < 100)
	{
		info[i++] = "You are somewhat resistant to cold.";
	}
	else if (res > 100)
	{
		info[i++] = "You are vulnerable to cold.";
	}

	res = res_pois_lvl();
	if (res == 0)
	{
		info[i++] = "You are completely immune to poison.";
	}
	else if (res <= 25)
	{
		info[i++] = "You resist poison exceptionally well.";
	}
	else if (res <= 50)
	{
		info[i++] = "You are resistant to poison.";
	}
	else if (res < 100)
	{
		info[i++] = "You are somewhat resistant to poison.";
	}
	else if (res > 100)
	{
		info[i++] = "You are vulnerable to poison.";
	}

	if (FLAG(p_ptr, TR_IM_LITE))
	{
		info[i++] = "You are completely immune to bright light.";
	}
	else if (FLAG(p_ptr, TR_RES_LITE))
	{
		if (FLAG(p_ptr, TR_HURT_LITE))
			info[i++] = "You are somewhat resistant to bright light.";
		else
			info[i++] = "You are resistant to bright light.";
	}
	else if (FLAG(p_ptr, TR_HURT_LITE))
	{
		info[i++] = "You are vulnerable to bright light.";
	}

	if (FLAG(p_ptr, TR_IM_DARK))
	{
		info[i++] = "You are completely immune to darkness.";
	}
	else if (FLAG(p_ptr, TR_RES_DARK))
	{
		if (FLAG(p_ptr, TR_HURT_DARK))
			info[i++] = "You are somewhat resistant to darkness.";
		else
			info[i++] = "You are resistant to darkness.";
	}
	else if (FLAG(p_ptr, TR_HURT_DARK))
	{
		info[i++] = "You are vulnerable to darkness.";
	}

	if (FLAG(p_ptr, TR_RES_CONF))
	{
		info[i++] = "You are resistant to confusion.";
	}
	if (FLAG(p_ptr, TR_RES_SOUND))
	{
		info[i++] = "You are resistant to sonic attacks.";
	}
	if (FLAG(p_ptr, TR_RES_DISEN))
	{
		info[i++] = "You are resistant to disenchantment.";
	}
	if (FLAG(p_ptr, TR_RES_CHAOS))
	{
		info[i++] = "You are resistant to chaos.";
	}
	if (FLAG(p_ptr, TR_RES_SHARDS))
	{
		info[i++] = "You are resistant to blasts of shards.";
	}
	if (FLAG(p_ptr, TR_RES_NEXUS))
	{
		info[i++] = "You are resistant to nexus attacks.";
	}
	if (FLAG(p_ptr, TR_RES_NETHER))
	{
		info[i++] = "You are resistant to nether forces.";
	}
	if ((FLAG(p_ptr, TR_RES_FEAR)) && (!(p_ptr->muta3 & MUT3_ESP)))
	{
		info[i++] = "You are completely fearless.";
	}
	if (FLAG(p_ptr, TR_RES_BLIND))
	{
		info[i++] = "Your eyes are resistant to blindness.";
	}

	if (FLAG(p_ptr, TR_SUST_STR))
	{
		info[i++] = "Your strength is sustained.";
	}
	if (FLAG(p_ptr, TR_SUST_INT))
	{
		info[i++] = "Your intelligence is sustained.";
	}
	if (FLAG(p_ptr, TR_SUST_WIS))
	{
		info[i++] = "Your wisdom is sustained.";
	}
	if (FLAG(p_ptr, TR_SUST_CON))
	{
		info[i++] = "Your constitution is sustained.";
	}
	if (FLAG(p_ptr, TR_SUST_DEX))
	{
		info[i++] = "Your dexterity is sustained.";
	}
	if (FLAG(p_ptr, TR_SUST_CHR))
	{
		info[i++] = "Your charisma is sustained.";
	}

	if (FLAG(p_ptr, TR_GHOUL_TOUCH))
	{
		info[i++] = "Your touch paralyzes your foes.";
	}

	if (FLAG(p_ptr, TR_WILD_SHOT))
	{
		info[i++] = "Your shots are not affected by forest.";
	}
	if (FLAG(p_ptr, TR_WILD_WALK))
	{
		info[i++] = "You are not hindered by natural terrain.";
	}

	if (ff[0] & (TR0_STR))
	{
		info[i++] = "Your strength is affected by your equipment.";
	}
	if (ff[0] & (TR0_INT))
	{
		info[i++] = "Your intelligence is affected by your equipment.";
	}
	if (ff[0] & (TR0_WIS))
	{
		info[i++] = "Your wisdom is affected by your equipment.";
	}
	if (ff[0] & (TR0_DEX))
	{
		info[i++] = "Your dexterity is affected by your equipment.";
	}
	if (ff[0] & (TR0_CON))
	{
		info[i++] = "Your constitution is affected by your equipment.";
	}
	if (ff[0] & (TR0_CHR))
	{
		info[i++] = "Your charisma is affected by your equipment.";
	}

	if (ff[0] & (TR0_STEALTH))
	{
		info[i++] = "Your stealth is affected by your equipment.";
	}
	if (ff[0] & (TR0_SEARCH))
	{
		info[i++] = "Your searching ability is affected by your equipment.";
	}
	if (ff[0] & (TR0_INFRA))
	{
		info[i++] = "Your infravision is affected by your equipment.";
	}
	if (ff[0] & (TR0_TUNNEL))
	{
		info[i++] = "Your digging ability is affected by your equipment.";
	}
	if (ff[0] & (TR0_SPEED))
	{
		info[i++] = "Your speed is affected by your equipment.";
	}
	if (ff[0] & (TR0_BLOWS))
	{
		info[i++] = "Your attack speed is affected by your equipment.";
	}


	/* Access the current weapon */
	o_ptr = &p_ptr->equipment[EQUIP_WIELD];

	/* Analyze the weapon */
	if (o_ptr->k_idx)
	{
		/* Indicate Blessing */
		if (FLAG(o_ptr, TR_BLESSED))
		{
			info[i++] = "Your weapon has been blessed by the gods.";
		}

		if (FLAG(o_ptr, TR_CHAOTIC))
		{
			info[i++] = "Your weapon is branded with the Sign of Logrus.";
		}

		/* Hack */
		if (FLAG(o_ptr, TR_IMPACT))
		{
			info[i++] = "The impact of your weapon can cause earthquakes.";
		}

		if (FLAG(o_ptr, TR_VORPAL))
		{
			info[i++] = "Your weapon is very sharp.";
		}

		if (FLAG(o_ptr, TR_VAMPIRIC))
		{
			info[i++] = "Your weapon drains life from your foes.";
		}

		/* Special "Attack Bonuses" */
		if (ff[0] & (TR0_BRAND_ACID))
		{
			info[i++] = "Your weapon melts your foes.";
		}
		if (ff[0] & (TR0_BRAND_ELEC))
		{
			info[i++] = "Your weapon shocks your foes.";
		}
		if (ff[0] & (TR0_BRAND_FIRE))
		{
			info[i++] = "Your weapon burns your foes.";
		}
		if (ff[0] & (TR0_BRAND_COLD))
		{
			info[i++] = "Your weapon freezes your foes.";
		}
		if (ff[0] & (TR0_BRAND_POIS))
		{
			info[i++] = "Your weapon poisons your foes.";
		}

		/* Special "slay" flags */
		if (FLAG(o_ptr, TR_SLAY_ANIMAL))
		{
			info[i++] = "Your weapon strikes at animals with extra force.";
		}
		if (FLAG(o_ptr, TR_SLAY_EVIL))
		{
			info[i++] = "Your weapon strikes at evil with extra force.";
		}
		if (FLAG(o_ptr, TR_SLAY_UNDEAD))
		{
			info[i++] = "Your weapon strikes at undead with holy wrath.";
		}
		if (FLAG(o_ptr, TR_SLAY_DEMON))
		{
			info[i++] = "Your weapon strikes at demons with holy wrath.";
		}
		if (FLAG(o_ptr, TR_SLAY_ORC))
		{
			info[i++] = "Your weapon is especially deadly against orcs.";
		}
		if (FLAG(o_ptr, TR_SLAY_TROLL))
		{
			info[i++] = "Your weapon is especially deadly against trolls.";
		}
		if (FLAG(o_ptr, TR_SLAY_GIANT))
		{
			info[i++] = "Your weapon is especially deadly against giants.";
		}
		if (FLAG(o_ptr, TR_SLAY_DRAGON))
		{
			info[i++] = "Your weapon is especially deadly against dragons.";
		}

		/* Special "kill" flags */
		if (ff[0] & (TR0_KILL_DRAGON))
		{
			info[i++] = "Your weapon is a great bane of dragons.";
		}

		if (ff[1] & (TR1_THROW))
		{
			info[i++] = "Your weapon can be thrown well.";
		}

		if (ff[3] & (TR3_PSI_CRIT))
		{
			info[i++] = "Your weapon uses magical power to strike great blows.";
		}
	}


	/* Save the screen */
	screen_save();

	/* Calculate how much lines we can put on a page */
	height = MIN(Term->hgt - 3, i + 2);

	/* Erase the screen */
    clear_region(13, 1, height + 1);

	/* Label the information */
	prtf(15, 1, "     Your Attributes:");

	/* We will print on top of the map (column 13) */
	for (k = 2, j = 0; j < i; j++)
	{
		/* Show the info */
		prtf(15, k++, info[j]);

		/* Every heightth entry, start a new page */
		if ((k == height) && (j + 1 < i))
		{
			prtf(15, k, "-- more --");
			(void)inkey();
			for (; k > 2; k--) prtf(15, k, "");
		}
	}

	/* Pause */
	prtf(13, k, "[Press any key to continue]");
	(void)inkey();

	/* Restore the screen */
	screen_load();
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


/*
 * Report all currently active magical effects.
 */
void report_magics(void)
{
	int i = 0, j, k;
	cptr info[128];
	int info2[128];


	if (p_ptr->tim.blind)
	{
		info2[i] = report_magics_aux(p_ptr->tim.blind);
		info[i++] = "You cannot see";
	}
	if (p_ptr->tim.confused)
	{
		info2[i] = report_magics_aux(p_ptr->tim.confused);
		info[i++] = "You are confused";
	}
	if (p_ptr->tim.afraid)
	{
		info2[i] = report_magics_aux(p_ptr->tim.afraid);
		info[i++] = "You are terrified";
	}
	if (p_ptr->tim.poisoned)
	{
		info2[i] = report_magics_aux(p_ptr->tim.poisoned);
		info[i++] = "You are poisoned";
	}
	if (p_ptr->tim.image)
	{
		info2[i] = report_magics_aux(p_ptr->tim.image);
		info[i++] = "You are hallucinating";
	}
	if (p_ptr->tim.blessed)
	{
		info2[i] = report_magics_aux(p_ptr->tim.blessed);
		info[i++] = "You feel righteous";
	}
	if (p_ptr->tim.hero)
	{
		info2[i] = report_magics_aux(p_ptr->tim.hero);
		info[i++] = "You feel heroic";
	}
	if (p_ptr->tim.shero)
	{
		info2[i] = report_magics_aux(p_ptr->tim.shero);
		info[i++] = "You are in a battle rage";
	}
	if (p_ptr->tim.protevil)
	{
		info2[i] = report_magics_aux(p_ptr->tim.protevil);
		info[i++] = "You are protected from evil";
	}
	if (p_ptr->tim.shield)
	{
		info2[i] = report_magics_aux(p_ptr->tim.shield);
		info[i++] = "You are protected by a mystic shield";
	}
	if (p_ptr->tim.invuln)
	{
		info2[i] = report_magics_aux(p_ptr->tim.invuln);
		info[i++] = "You are invulnerable";
	}
	if (p_ptr->tim.wraith_form)
	{
		info2[i] = report_magics_aux(p_ptr->tim.wraith_form);
		info[i++] = "You are incorporeal";
	}
	if (p_ptr->state.confusing)
	{
		info2[i] = 7;
		info[i++] = "Your hands are glowing dull red.";
	}
	if (p_ptr->tim.word_recall)
	{
		info2[i] = report_magics_aux(p_ptr->tim.word_recall);
		info[i++] = "You are waiting to be recalled";
	}
	if (p_ptr->tim.oppose_acid)
	{
		info2[i] = report_magics_aux(p_ptr->tim.oppose_acid);
		info[i++] = "You are resistant to acid";
	}
	if (p_ptr->tim.oppose_elec)
	{
		info2[i] = report_magics_aux(p_ptr->tim.oppose_elec);
		info[i++] = "You are resistant to lightning";
	}
	if (p_ptr->tim.oppose_fire)
	{
		info2[i] = report_magics_aux(p_ptr->tim.oppose_fire);
		info[i++] = "You are resistant to fire";
	}
	if (p_ptr->tim.oppose_cold)
	{
		info2[i] = report_magics_aux(p_ptr->tim.oppose_cold);
		info[i++] = "You are resistant to cold";
	}
	if (p_ptr->tim.oppose_pois)
	{
		info2[i] = report_magics_aux(p_ptr->tim.oppose_pois);
		info[i++] = "You are resistant to poison";
	}

	/* Save the screen */
	screen_save();

	/* Erase the screen */
    clear_region(13, 1, 23);

	/* Label the information */
	prtf(15, 1, "     Your Current Magic:");

	/* We will print on top of the map (column 13) */
	for (k = 2, j = 0; j < i; j++)
	{
		/* Show the info */
		prtf(15, k++, "%s %s.", info[j], report_magic_durations[info2[j]]);

		/* Every 20 entries (lines 2 to 21), start over */
		if ((k == 22) && (j + 1 < i))
		{
			prtf(15, k, "-- more --");
			(void)inkey();
			for (; k > 2; k--) prtf(15, k, "");
		}
	}

	/* Pause */
	prtf(13, k, "[Press any key to continue]");
	(void)inkey();

	/* Restore the screen */
	screen_load();
}



/*
 * Detect things on a square.
 *
 * The location (x,y) is passed to the tester function pointer.
 *
 * This is an "engine" function that does all the work, and
 * prevents a huge amount of code duplication.
 */
static bool detect_sq_aux(bool tester(int x, int y), cptr msg)
{
	int px = p_ptr->px;
	int py = p_ptr->py;
	
	int x, y;
	
	bool detect = FALSE;
	
	/* Scan a radius MAX_DETECT circle */
	for (y = py - MAX_DETECT; y <= py + MAX_DETECT; y++)
	{
		for (x = px - MAX_DETECT; x <= px + MAX_DETECT; x++)
		{
			if (!in_bounds2(x, y)) continue;

			if (distance(px, py, x, y) > MAX_DETECT) continue;
			
			/* Detect something? */
			if (tester(x, y)) detect = TRUE;
		}
	}
	
	if (detect)
	{
		/* Describe */
		if (msg) msgf(msg);
		
		/* Window stuff */
		p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);
	}
	
	return (detect);
}


/* Test for the existance of traps here */
static bool trap_tester(int x, int y)
{
	cave_type *c_ptr = area(x, y);
	pcave_type *pc_ptr = parea(x, y);
	
	/* Save the 'detected' status for this square */
	pc_ptr->player |= GRID_DTCT;

	/* Detect traps */
	return (field_detect_type(c_ptr, FTYPE_TRAP));
}

/* Test for the existance of traps here */
static bool trap_ident_tester(int x, int y)
{
	cave_type *c_ptr = area(x, y);
	
	/* Detect traps */
	return (field_detect_type(c_ptr, FTYPE_TRAP));
}


/*
 * Detect all traps in range
 */
bool detect_traps(bool ident)
{
	/* The source is identified? */
	if (ident || detect_sq_aux(trap_ident_tester, NULL))
	{
		/* Have detected traps on this level */
		p_ptr->state.detected = TRUE;
	
		/* Detect them properly now */
		return(detect_sq_aux(trap_tester, "You sense the presence of traps!"));
	}
	
	return (FALSE);
}


/*
 * Place a random type of normal door at the given location.
 * Use this in-game
 */
void create_closed_door(int x, int y)
{
	int tmp;

	/* Invisible wall */
	if (ironman_nightmare && one_in_(666))
	{
		/* Create invisible wall */
		cave_set_feat(x, y, the_floor());
		(void)place_field(x, y, FT_WALL_INVIS);
		return;
	}

	/* Choose an object */
	tmp = randint0(400);

	/* Closed doors (300/400) */
	if (tmp < 300)
	{
		/* Create closed door */
		cave_set_feat(x, y, FEAT_CLOSED);
	}

	/* Locked doors (99/400) */
	else if (tmp < 399)
	{
		/* Create locked door */
		make_lockjam_door(x, y, randint1(10) + p_ptr->depth / 10, FALSE);
	}

	/* Stuck doors (1/400) */
	else
	{
		/* Create jammed door */
		make_lockjam_door(x, y, randint1(5) + p_ptr->depth / 10, TRUE);
	}
}

/* Test for the existance of doors here */
static bool door_tester(int x, int y)
{
	cave_type *c_ptr = area(x, y);
	pcave_type *pc_ptr = parea(x, y);
	
	/* Detect secret doors */
	if (c_ptr->feat == FEAT_SECRET)
	{
		/* Pick a door */
		create_closed_door(x, y);
	}

	/* Detect doors */
	if ((c_ptr->feat == FEAT_CLOSED) || (c_ptr->feat == FEAT_OPEN) ||
		(c_ptr->feat == FEAT_BROKEN))
	{
		/* Hack -- Memorize */
		remember_grid(c_ptr, pc_ptr);

		/* Redraw */
		lite_spot(x, y);

		/* Obvious */
		return (TRUE);
	}
	
	return (FALSE);
}


/*
 * Detect all doors in range
 */
bool detect_doors(void)
{
	return (detect_sq_aux(door_tester, "You sense the presence of doors!"));
}



/* Test for the existance of stairs here */
static bool stair_tester(int x, int y)
{
	cave_type *c_ptr = area(x, y);
	pcave_type *pc_ptr = parea(x, y);
	
	/* Detect stairs */
	if ((c_ptr->feat == FEAT_LESS) || (c_ptr->feat == FEAT_MORE))
	{
		/* Hack -- Memorize */
		remember_grid(c_ptr, pc_ptr);

		/* Redraw */
		lite_spot(x, y);

		/* Obvious */
		return (TRUE);
	}

	return (FALSE);
}

/*
 * Detect all stairs in range
 */
bool detect_stairs(void)
{
	return (detect_sq_aux(stair_tester, "You sense the presence of stairs!"));
}


/* Test for the existance of treasure here */
static bool treasure_tester(int x, int y)
{
	cave_type *c_ptr = area(x, y);
	pcave_type *pc_ptr = parea(x, y);
	
	/* Magma/Quartz + Known Gold */
	if ((c_ptr->feat == FEAT_MAGMA_K) || (c_ptr->feat == FEAT_QUARTZ_K))
	{
		/* Hack -- Memorize */
		remember_grid(c_ptr, pc_ptr);

		/* Redraw */
		lite_spot(x, y);

		/* Detect */
		return (TRUE);
	}

	return (FALSE);
}


/*
 * Detect any treasure in range
 */
bool detect_treasure(void)
{
	return (detect_sq_aux(treasure_tester,
			"You sense the presence of buried treasure!"));
}


/*
 * Detect objects with a given property.
 *
 * The object o_ptr is passed to the tester function pointer.
 *
 * This is an "engine" function that does all the work, and
 * prevents a huge amount of code duplication.
 */
static bool detect_obj_aux(bool tester(const object_type *o_ptr), cptr msg)
{
	int px = p_ptr->px;
	int py = p_ptr->py;
	
	int x, y, i;
	
	bool detect = FALSE;
	
	/* Scan objects */
	for (i = 1; i < o_max; i++)
	{
		object_type *o_ptr = &o_list[i];

		/* Skip dead objects */
		if (!o_ptr->k_idx) continue;

		/* Skip held objects */
		if (!(o_ptr->ix || o_ptr->iy)) continue;

		/* Location */
		y = o_ptr->iy;
		x = o_ptr->ix;

		if (distance(px, py, x, y) > MAX_DETECT) continue;

		/* Detect */
		if (tester(o_ptr))
		{
			/* Detect */
			detect = TRUE;
			
			/* Hack -- memorize it */
			o_ptr->info |= OB_SEEN;

			/* Redraw */
			lite_spot(x, y);
		}
	}
	
	if (detect)
	{
		/* Describe */
		msgf(msg);
		
		/* Window stuff */
		p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);
	}
	
	return (detect);
}

/*  Gold item? */ 
static bool gold_tester(const object_type *o_ptr)
{
	/* Detect "gold" objects */
	return (o_ptr->tval == TV_GOLD);
}


/*
 * Detect all "gold" objects in range
 */
bool detect_objects_gold(void)
{
	return (detect_obj_aux(gold_tester,
				"You sense the presence of treasure!") ||
			detect_monsters_string("$"));
}

/* Real nongold item? */
static bool nongold_tester(const object_type *o_ptr)
{
	return (o_ptr->tval != TV_GOLD);
}

/*
 * Detect all "normal" objects in range
 */
bool detect_objects_normal(void)
{
	return (detect_obj_aux(nongold_tester,
				"You sense the presence of objects!") ||
			detect_monsters_string("!=?|"));
}


static bool magic_tester(const object_type *o_ptr)
{
	/* Examine the tval */
	int	tv = o_ptr->tval;

	/* Artifacts, misc magic items, or enchanted wearables */
	return	(o_ptr->xtra_name ||
			(tv == TV_AMULET) ||
			(tv == TV_RING) ||
			(tv == TV_STAFF) ||
			(tv == TV_WAND) ||
			(tv == TV_ROD) ||
			(tv == TV_SCROLL) ||
			(tv == TV_POTION) ||
			(tv == TV_LIFE_BOOK) ||
			(tv == TV_SORCERY_BOOK) ||
			(tv == TV_NATURE_BOOK) ||
			(tv == TV_CHAOS_BOOK) ||
			(tv == TV_DEATH_BOOK) ||
			(tv == TV_TRUMP_BOOK) ||
			(tv == TV_ARCANE_BOOK) ||
			((o_ptr->to_a > 0) || (o_ptr->to_h + o_ptr->to_d > 0)));
}

/*
 * Detect all "magic" objects in range.
 *
 * This will light up all spaces with "magic" items, including artifacts,
 * ego-items, potions, scrolls, books, rods, wands, staves, amulets, rings,
 * and "enchanted" items of the "good" variety.
 *
 * It can probably be argued that this function is now too powerful.
 */
bool detect_objects_magic(void)
{
	return (detect_obj_aux(magic_tester,
			"You sense the presence of magic objects!"));
}


/*
 * Detect monster with a given property.
 *
 * The monster m_ptr is passed to the tester function pointer,
 * along with the pointer to the extra parameters.
 *
 * This is an "engine" function that does all the work, and
 * prevents a huge amount of code duplication.
 */
static bool detect_mon_aux(bool tester(const monster_type *m_ptr, const vptr data), cptr msg, const vptr data)
{
	int px = p_ptr->px;
	int py = p_ptr->py;
	
	int x, y, i;
	
	bool detect = FALSE;
	
	/* Scan monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;

		if (distance(px, py, x, y) > MAX_DETECT) continue;

		/* Do not detect mimics */
		if (m_ptr->smart & (SM_MIMIC)) continue;
		
		/* Detect monsters satisfying restriction */
		if (tester(m_ptr, data))
		{
			/* Update monster recall window */
			if (p_ptr->monster_race_idx == m_ptr->r_idx)
			{
				/* Window stuff */
				p_ptr->window |= (PW_MONSTER);
			}
			
			/* Repair visibility later */
			p_ptr->change |= (PC_REPAIR);

			/* Hack -- Detect monster */
			m_ptr->mflag |= (MFLAG_MARK | MFLAG_SHOW);

			/* Update the monster */
			update_mon(i, FALSE);

			/* Detect */
			detect = TRUE;
		}
	}
	
	if (detect)
	{
		/* Describe */
		msgf(msg);
		
		/* Window stuff */
		p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);
	}
	
	return (detect);
}


/* Normal monster? */
static bool norm_mon_tester(const monster_type *m_ptr, vptr data)
{
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	
	/* Ignore parameter */
	(void) data;

	/* Do not detect mimics */
	if (m_ptr->smart & (SM_MIMIC)) return (FALSE);
	
	/* Detect all non-invisible monsters */
	if (!FLAG(r_ptr, RF_INVISIBLE) ||
		FLAG(p_ptr, TR_SEE_INVIS) || p_ptr->tim.invis) return (TRUE);
		
	return (FALSE);
}


/*
 * Detect all "normal" monsters in range
 */
bool detect_monsters_normal(void)
{
	return (detect_mon_aux(norm_mon_tester,
			"You sense the presence of monsters!", NULL));
}


/* Invisible monster? */
static bool invis_mon_tester(const monster_type *m_ptr, vptr data)
{
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	
	/* Ignore parameter */
	(void) data;
	
	/* Detect invisible monsters */
	return (FLAG(r_ptr, RF_INVISIBLE));
}

/*
 * Detect all "invisible" monsters in range
 */
bool detect_monsters_invis(void)
{
	return (detect_mon_aux(invis_mon_tester,
			"You sense the presence of invisible creatures!", NULL));
}


/* Evil monster? */
static bool evil_mon_tester(const monster_type *m_ptr, vptr data)
{
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	
	/* Ignore parameter */
	(void) data;
	
	/* Detect evil monsters */
	if (FLAG(r_ptr, RF_EVIL))
	{
		/* Take note that they are evil */
		r_ptr->r_flags[2] |= (RF2_EVIL);
		
		return (TRUE);
	}
	
	return (FALSE);
}

/*
 * Detect all "evil" monsters in range
 */
bool detect_monsters_evil(void)
{
	return (detect_mon_aux(evil_mon_tester,
			"You sense the presence of evil creatures!", NULL));
}

/* Nonliving monster? */
static bool nonlive_mon_tester(const monster_type *m_ptr, vptr data)
{
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	
	/* Ignore parameter */
	(void) data;
	
	/* Not living? */
	return (!monster_living(r_ptr));
}

/*
 * Detect all "nonliving", "undead" or "demonic" monsters in range
 */
bool detect_monsters_nonliving(void)
{
	return (detect_mon_aux(nonlive_mon_tester,
			"You sense the presence of unnatural beings!", NULL));
}

/* Living monster? */
static bool live_mon_tester(const monster_type *m_ptr, vptr data)
{
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	
	/* Ignore parameter */
	(void) data;
	
	/* Not living? */
	return (monster_living(r_ptr));
}

/*
 * Detect all "living" monsters in range
 */
bool detect_monsters_living(void)
{
	return (detect_mon_aux(live_mon_tester,
			"You sense the presence of natural beings!", NULL));
}


/* Is a monster in the list of races? */
static bool race_mon_tester(const monster_type *m_ptr, vptr data)
{
	cptr match;

	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	
	/* Get string to compare with */
	match = (cptr) data;
	
	/* Detect monsters with the same symbol */
	return(strchr(match, r_ptr->d_char) ? TRUE : FALSE);
}

/*
 * Detect all (string) monsters in range
 */
bool detect_monsters_string(cptr match)
{
	return (detect_mon_aux(race_mon_tester,
			"You sense the presence of monsters!", (vptr) match));
}

/* Generic monster tester */
static bool flag_mon_tester(const monster_type *m_ptr, vptr data)
{
	/* Get flags to compare with */
	const u32b flag = *((const u32b *) data);

	monster_race *r_ptr = &r_info[m_ptr->r_idx];
		
	if (r_ptr->flags[2] & flag)
	{
		/* Take note that they are something */
		r_ptr->r_flags[2] |= (flag);
		
		return (TRUE);
	}
	
	return (FALSE);
}


/*
 * A "generic" detect monsters routine, tagged to flags3
 */
bool detect_monsters_xxx(u32b match_flag)
{
	cptr desc_monsters;

	/* Describe */
	switch (match_flag)
	{
		case RF2_DEMON:
			desc_monsters = "You sense the presence of demons!";
			break;
		case RF2_UNDEAD:
			desc_monsters = "You sense the presence of the undead!";
			break;
		default:
			desc_monsters = "You sense the presence of weird monsters!";
	}
	
	/* Result (note that ints might only be 16bit...) */
	return (detect_mon_aux(flag_mon_tester, desc_monsters, (vptr) &match_flag));
}


/*
 * Detect everything
 */
bool detect_all(void)
{
	bool detect = FALSE;

	/* Detect everything */
	if (detect_traps(TRUE)) detect = TRUE;
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
 * Apply a "project()" directly to all viewable monsters
 *
 * Note that affected monsters are NOT auto-tracked by this usage.
 *
 * To avoid misbehavior when monster deaths have side-effects,
 * this is done in two passes. -- JDL
 */
bool project_hack(int typ, int dam)
{
	int i, x, y;
	u16b flg = PROJECT_JUMP | PROJECT_KILL | PROJECT_HIDE;
	bool obvious = FALSE;

	/* Mark all (nearby) monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;
		
		/* Paranoia */
		if (!in_boundsp(x, y)) continue;

		/* Require line of sight */
		if (!player_has_los_grid(parea(x, y))) continue;

		/* Mark the monster */
		m_ptr->mflag |= (MFLAG_TEMP);
	}

	/* Affect all marked monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];

		/* Skip unmarked monsters */
		if (!(m_ptr->mflag & (MFLAG_TEMP))) continue;

		/* Remove mark */
		m_ptr->mflag &= ~(MFLAG_TEMP);

		/* Jump directly to the target monster */
		if (project(0, 0, m_ptr->fx, m_ptr->fy, dam, typ, flg)) obvious = TRUE;
	}

	/* Result */
	return (obvious);
}


/*
 * Speed monsters
 */
bool speed_monsters(void)
{
	return (project_hack(GF_OLD_SPEED, p_ptr->lev));
}

/*
 * Slow monsters
 */
bool slow_monsters(void)
{
	return (project_hack(GF_OLD_SLOW, p_ptr->lev));
}

/*
 * Sleep monsters
 */
bool sleep_monsters(void)
{
	return (project_hack(GF_OLD_SLEEP, p_ptr->lev));
}


/*
 * Banish evil monsters
 */
bool banish_evil(int dist)
{
	return (project_hack(GF_AWAY_EVIL, dist));
}


/*
 * Turn undead
 */
bool turn_undead(void)
{
	bool tester = (project_hack(GF_TURN_UNDEAD, p_ptr->lev));

	if (tester)
		chg_virtue(V_UNLIFE, -1);

	return tester;
}


/*
 * Dispel undead monsters
 */
bool dispel_undead(int dam)
{
	bool tester = (project_hack(GF_DISP_UNDEAD, dam));

	if (tester)
		chg_virtue(V_UNLIFE, -2);

	return tester;
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
 * Raise the dead
 */
bool raise_dead(int x, int y, bool pet)
{
	s16b i;
	int fx, fy;

	bool obvious = FALSE;

	cave_type *c_ptr;

	/* Check all (nearby) fields */
	for (i = 1; i < fld_max; i++)
	{
		field_type *f_ptr = &fld_list[i];

		/* Paranoia -- Skip missing objects */
		if (!f_ptr->t_idx) continue;

		/* Want a corpse / skeleton */
		if (!(f_ptr->t_idx == FT_CORPSE ||
			  f_ptr->t_idx == FT_SKELETON)) continue;

		/* Location */
		fy = f_ptr->fy;
		fx = f_ptr->fx;

		/* Require line of sight */
		if (!los(fx, fy, x, y)) continue;

		c_ptr = area(fx, fy);

		/* Raise Corpses / Skeletons */
		if (field_script_special(c_ptr, FTYPE_CORPSE, "i", LUA_VAR(pet)))
		{
			if (player_has_los_grid(parea(fx, fy))) obvious = TRUE;
		}
	}

	/* Result */
	return (obvious);
}



/*
 * Wake up all monsters, and speed up "los" monsters.
 */
void aggravate_monsters(int who)
{
	int i;
	bool sleep = FALSE;
	bool speed = FALSE;

	/* Aggravate everyone nearby */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Skip aggravating monster (or player) */
		if (i == who) continue;

		/* Wake up nearby sleeping monsters */
		if (m_ptr->cdis < MAX_SIGHT * 2)
		{
			/* Wake up */
			if (m_ptr->csleep)
			{
				/* Wake up */
				m_ptr->csleep = 0;
				sleep = TRUE;

				/* Redraw (later) if needed */
				if (p_ptr->health_who == i)
					p_ptr->redraw |= (PR_HEALTH);
			}
		}

		/* Speed up monsters in line of sight */
		if (player_has_los_grid(parea(m_ptr->fx, m_ptr->fy)))
		{
			/* Speed up (instantly) to racial base + 10 */
			if (m_ptr->mspeed < r_ptr->speed + 10)
			{
				/* Speed up */
				m_ptr->mspeed = r_ptr->speed + 10;
				speed = TRUE;
			}
		}
	}

	/* Messages */
	if (speed) msgf("You feel a sudden stirring nearby!");
	else if (sleep) msgf("You hear a sudden stirring in the distance!");
}



/*
 * Delete all non-unique/non-quest monsters of a given "type" from the level
 */
bool genocide(int player_cast)
{
	int i;
	char typ;
	bool result = FALSE;
	int msec = delay_factor * delay_factor * delay_factor;

	/* Mega-Hack -- Get a monster symbol */
	(void)(get_com("Choose a monster race (by symbol) to genocide: ", &typ));

	/* Delete the monsters of that "type" */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Hack -- Skip Unique Monsters */
		if (FLAG(r_ptr, RF_UNIQUE)) continue;

		/* Hack -- Skip Quest Monsters */
		if (FLAG(r_ptr, RF_QUESTOR)) continue;

		/* Skip "wrong" monsters */
		if (r_ptr->d_char != typ) continue;

		/* Notice changes in view */
		if (FLAG(r_ptr, RF_LITE_1) || FLAG(r_ptr, RF_LITE_2))
		{
			/* Update some things */
			p_ptr->update |= (PU_MON_LITE);
		}

		/* Delete the monster */
		delete_monster_idx(i);

		if (player_cast)
		{
			/* Take damage */
			take_hit(randint1(4), "the strain of casting Genocide");
		}

		/* Visual feedback */
		move_cursor_relative(p_ptr->px, p_ptr->py);

		/* Redraw */
		p_ptr->redraw |= (PR_HP);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER);

		/* Handle */
		handle_stuff();

		/* Fresh */
		Term_fresh();

		/* Delay */
		Term_xtra(TERM_XTRA_DELAY, msec);

		/* Take note */
		result = TRUE;
	}

	if (result)
		chg_virtue(V_VITALITY, -2);

	return (result);
}


/*
 * Delete all nearby (non-unique) monsters
 */
bool mass_genocide(int player_cast)
{
	int i;
	bool result = FALSE;
	int msec = delay_factor * delay_factor * delay_factor;

	/* This needs to be rethought - the wilderness is a problem... */

	/* Delete the (nearby) monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Hack -- Skip unique monsters */
		if (FLAG(r_ptr, RF_UNIQUE)) continue;

		/* Hack -- Skip Quest Monsters */
		if (FLAG(r_ptr, RF_QUESTOR)) continue;

		/* Skip distant monsters */
		if (m_ptr->cdis > MAX_SIGHT) continue;

		/* Notice changes in view */
		if (FLAG(r_ptr, RF_LITE_1) || FLAG(r_ptr, RF_LITE_2))
		{
			/* Update some things */
			p_ptr->update |= (PU_MON_LITE);
		}

		/* Delete the monster */
		delete_monster_idx(i);

		if (player_cast)
		{
			/* Hack -- visual feedback */
			take_hit(randint1(3), "the strain of casting Mass Genocide");
		}

		move_cursor_relative(p_ptr->px, p_ptr->py);

		/* Redraw */
		p_ptr->redraw |= (PR_HP);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER);

		/* Handle */
		handle_stuff();

		/* Fresh */
		Term_fresh();

		/* Delay */
		Term_xtra(TERM_XTRA_DELAY, msec);

		/* Note effect */
		result = TRUE;
	}

	if (result)
		chg_virtue(V_VITALITY, -2);

	return (result);
}



/*
 * Probe nearby monsters
 */
bool probing(void)
{
	int i;
	bool probe = FALSE;

	/* Probe all (nearby) monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Require line of sight */
		if (!player_has_los_grid(parea(m_ptr->fx, m_ptr->fy))) continue;

		/* Probe visible monsters */
		if (m_ptr->ml)
		{
			/* Start the message */
			if (!probe) msgf("Probing...");

			/* Describe the monster */
			msgf("%^v has %d hit points.", MONSTER_FMT(m_ptr, 0x04), m_ptr->hp);

			/* Learn all of the non-spell, non-treasure flags */
			lore_do_probe(i);

			/* Probe worked */
			probe = TRUE;
		}
	}

	/* Done */
	if (probe)
	{
		if (probe)
			chg_virtue(V_KNOWLEDGE, 1);

		msgf("That's all.");
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
 * "earthquake" by using the (removed) "full" parameter 
 * to select "destruction".
 */
bool destroy_area(int x1, int y1, int r)
{
	int y, x, k, t;
	bool flag = FALSE;

	cave_type *c_ptr;
	pcave_type *pc_ptr;

	/* Prevent destruction of town and wilderness */
	if (!p_ptr->depth)
	{
		return (FALSE);
	}

	/* Big area of affect */
	for (y = (y1 - r); y <= (y1 + r); y++)
	{
		for (x = (x1 - r); x <= (x1 + r); x++)
		{
			/* Skip illegal grids */
			if (!in_boundsp(x, y)) continue;

			/* Extract the distance */
			k = distance(x1, y1, x, y);

			/* Stay in the circle of death */
			if (k > r) continue;

			/* Access the grid */
			c_ptr = area(x, y);
			pc_ptr = parea(x, y);

			/* Lose room and vault */
			c_ptr->info &= ~(CAVE_ROOM | CAVE_ICKY);

			/* Lose light */
			c_ptr->info &= ~(CAVE_GLOW);

			/* Hack -- Notice player affect */
			if ((x == p_ptr->px) && (y == p_ptr->py))
			{
				/* Hurt the player later */
				flag = TRUE;

				/* Do not hurt this grid */
				continue;
			}

			/* Hack -- Skip the epicenter */
			if ((y == y1) && (x == x1)) continue;

			if (FLAG(&r_info[m_list[c_ptr->m_idx].r_idx], RF_QUESTOR))
			{
				/* Heal the monster */
				m_list[c_ptr->m_idx].hp = m_list[c_ptr->m_idx].maxhp;

				/* Try to teleport away quest monsters */
				if (!teleport_away(c_ptr->m_idx, (r * 2) + 1)) continue;
			}
			else
			{
				/* Delete the monster (if any) */
				delete_monster(x, y);
			}

			/* Fields can block destruction */
			if (fields_have_flags(c_ptr, FIELD_INFO_PERM)) continue;

			/* Destroy the fields on the square */
			delete_field(x, y);

			/* Destroy "valid" grids */
			if (cave_valid_grid(c_ptr))
			{
				/* Delete objects */
				delete_object(x, y);

				/* Wall (or floor) type */
				t = randint0(200);

				/* Granite */
				if (t < 20)
				{
					/* Create granite wall */
					cave_set_feat(x, y, FEAT_WALL_EXTRA);
				}

				/* Quartz */
				else if (t < 70)
				{
					/* Create quartz vein */
					cave_set_feat(x, y, FEAT_QUARTZ);
				}

				/* Magma */
				else if (t < 100)
				{
					/* Create magma vein */
					cave_set_feat(x, y, FEAT_MAGMA);
				}

				/* Floor */
				else
				{
					/* Create floor */
					cave_set_feat(x, y, the_floor());
				}
			}

			/* Hack - forget the square */
			forget_grid(pc_ptr);
		}
	}


	/* Hack -- Affect player */
	if (flag)
	{
		/* Message */
		msgf("There is a searing blast of light!");

		/* Blind the player */
		if (!(FLAG(p_ptr, TR_RES_BLIND)) &&
			!(FLAG(p_ptr, TR_RES_LITE)) &&
			!(FLAG(p_ptr, TR_IM_LITE)))
		{
			/* Become blind */
			(void)inc_blind(rand_range(10, 20));
		}
	}

	/* Success */
	return (TRUE);
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
bool earthquake(int cx, int cy, int r)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int i, t, y, x, yy, xx, dy, dx, oy, ox;
	int damage = 0;
	int sn = 0, sy = 0, sx = 0;
	bool hurt = FALSE;

	cave_type *c_ptr;
	pcave_type *pc_ptr;

	bool map[32][32];

	/* Prevent destruction of town and wilderness */
	if (!p_ptr->depth)
	{
		return (FALSE);
	}

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
			if (!in_boundsp(xx, yy)) continue;

			/* Skip distant grids */
			if (distance(cx, cy, xx, yy) > r) continue;

			/* Access the grid */
			c_ptr = area(xx, yy);
			pc_ptr = parea(xx, yy);

			/* Lose room and vault */
			c_ptr->info &= ~(CAVE_ROOM | CAVE_ICKY);

			/* Lose light */
			c_ptr->info &= ~(CAVE_GLOW);

			/* Skip the epicenter */
			if (!dx && !dy) continue;

			/* Skip most grids */
			if (randint0(100) < 85) continue;

			/* Damage this grid */
			map[16 + yy - cy][16 + xx - cx] = TRUE;

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

			if (!in_bounds2(x, y)) continue;

			/* Access the grid */
			c_ptr = area(x, y);

			/* Skip non-empty grids */
			if (!cave_empty_grid(c_ptr)) continue;

			/* Important -- Skip "quake" grids */
			if (map[16 + y - cy][16 + x - cx]) continue;

			/* Check for a field that blocks movement */
			if (fields_have_flags(c_ptr, FIELD_INFO_NO_ENTER))
			{
				continue;
			}

			/* Count "safe" grids */
			sn++;

			/* Randomize choice */
			if (randint0(sn) > 0) continue;

			/* Save the safe location */
			sy = y;
			sx = x;
		}

		/* Random message */
		switch (randint1(3))
		{
			case 1:
			{
				msgf("The cave ceiling collapses!");
				break;
			}
			case 2:
			{
				msgf("The cave floor twists in an unnatural way!");
				break;
			}
			default:
			{
				msgf("The cave quakes!  You are pummeled with debris!");
				break;
			}
		}

		/* Hurt the player a lot */
		if (!sn)
		{
			/* Message and damage */
			msgf("You are severely crushed!");
			damage = 300;
		}

		/* Destroy the grid, and push the player to safety */
		else
		{
			/* Calculate results */
			switch (randint1(3))
			{
				case 1:
				{
					msgf("You nimbly dodge the blast!");
					damage = 0;
					break;
				}
				case 2:
				{
					msgf("You are bashed by rubble!");
					damage = damroll(10, 4);
					(void)inc_stun(randint1(50));
					break;
				}
				case 3:
				{
					msgf("You are crushed between the floor and ceiling!");
					damage = damroll(10, 4);
					(void)inc_stun(randint1(50));
					break;
				}
			}

			/* Save old location */
			oy = py;
			ox = px;

			/* Move the player */
			py = sy;
			px = sx;

			/* Move the player */
			p_ptr->py = sy;
			p_ptr->px = sx;

			/* Notice movement */
			Term_move_player();

			if (!p_ptr->depth)
			{
				/* Scroll wilderness */
				p_ptr->wilderness_x = px;
				p_ptr->wilderness_y = py;
				move_wild();
			}

			/* Redraw the old spot */
			lite_spot(ox, oy);

			/* Redraw the new spot */
			lite_spot(px, py);

			/* Process fields under the player. */
			field_script(area(px, py), FIELD_ACT_PLAYER_ENTER, "");

			/* Check for new panel */
			verify_panel();
		}

		/* Important -- no wall on player */
		map[16 + py - cy][16 + px - cx] = FALSE;

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
			if (!map[16 + yy - cy][16 + xx - cx]) continue;

			if (!in_bounds2(xx, yy)) continue;

			/* Access the grid */
			c_ptr = area(xx, yy);

			/* Process monsters */
			if (c_ptr->m_idx)
			{
				monster_type *m_ptr = &m_list[c_ptr->m_idx];
				monster_race *r_ptr = &r_info[m_ptr->r_idx];

				/* Quest monsters */
				if (FLAG(r_ptr, RF_QUESTOR))
				{
					/* No wall on quest monsters */
					map[16 + yy - cy][16 + xx - cx] = FALSE;

					continue;
				}

				/* Most monsters cannot co-exist with rock */
				if (!FLAG(r_ptr, RF_KILL_WALL) &&
					!FLAG(r_ptr, RF_PASS_WALL))
				{
					/* Assume not safe */
					sn = 0;

					/* Monster can move to escape the wall */
					if (!FLAG(r_ptr, RF_NEVER_MOVE))
					{
						/* Look for safety */
						for (i = 0; i < 8; i++)
						{
							/* Access the grid */
							y = yy + ddy_ddd[i];
							x = xx + ddx_ddd[i];

							if (!in_bounds2(x, y)) continue;

							/* Access the grid */
							c_ptr = area(x, y);

							/* Skip non-empty grids */
							if (!cave_empty_grid(c_ptr)) continue;

							/* Not on player */
							if ((y == py) && (x == px)) continue;

							/* Check for a field that blocks movement */
							if (fields_have_flags(c_ptr, FIELD_INFO_NO_ENTER))
								continue;

							/* 
							 * Test for fields that will not allow monsters to
							 * be generated on them.  (i.e. Glyph of warding)
							 */
							if (fields_have_flags(c_ptr, FIELD_INFO_NO_MPLACE)) continue;

							/* ... nor on the Pattern */
							if (cave_pattern_grid(c_ptr))
							{
								continue;
							}

							/* Important -- Skip "quake" grids */
							if (map[16 + y - cy][16 + x - cx]) continue;

							/* Count "safe" grids */
							sn++;

							/* Randomize choice */
							if (randint0(sn) > 0) continue;

							/* Save the safe grid */
							sy = y;
							sx = x;
						}
					}

					/* Scream in pain */
					msgf("%^v wails out in pain!", MONSTER_FMT(m_ptr, 0));

					/* Take damage from the quake */
					damage = (sn ? damroll(4, 8) : (m_ptr->hp + 1));

					/* Monster is certainly awake */
					m_ptr->csleep = 0;

					/* Apply damage directly */
					m_ptr->hp -= damage;

					/* Delete (not kill) "dead" monsters */
					if (m_ptr->hp < 0)
					{
						/* Message */
						msgf("%^v is embedded in the rock!", MONSTER_FMT(m_ptr, 0));

						/* Delete the monster */
						delete_monster(xx, yy);

						/* No longer safe */
						sn = 0;
					}

					/* Hack -- Escape from the rock */
					if (sn)
					{
						int m_idx = area(xx, yy)->m_idx;

						/* Update the new location */
						area(sx, sy)->m_idx = m_idx;

						/* Update the old location */
						area(xx, yy)->m_idx = 0;

						/* Move the monster */
						m_ptr->fy = sy;
						m_ptr->fx = sx;

						/* Update the monster (new location) */
						update_mon(m_idx, TRUE);

						/* Redraw the old grid */
						lite_spot(xx, yy);

						/* Redraw the new grid */
						lite_spot(sx, sy);
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
			if (!map[16 + yy - cy][16 + xx - cx]) continue;

			if (!in_bounds2(xx, yy)) continue;

			/* Access the cave grid */
			c_ptr = area(xx, yy);
			pc_ptr = parea(xx, yy);

			/* Paranoia -- never affect player */
			if ((yy == py) && (xx == px)) continue;

			/* Fields can block destruction */
			if (fields_have_flags(c_ptr, FIELD_INFO_PERM)) continue;

			/* Destroy the fields on the square */
			delete_field(xx, yy);

			/* Destroy location (if valid) */
			if (cave_valid_grid(c_ptr))
			{
				bool floor = cave_floor_grid(c_ptr);

				/* Delete objects */
				delete_object(xx, yy);

				/* Wall (or floor) type */
				t = (floor ? randint0(100) : 200);

				/* Granite */
				if (t < 20)
				{
					/* Create granite wall */
					cave_set_feat(xx, yy, FEAT_WALL_EXTRA);
				}

				/* Quartz */
				else if (t < 70)
				{
					/* Create quartz vein */
					cave_set_feat(xx, yy, FEAT_QUARTZ);
				}

				/* Magma */
				else if (t < 100)
				{
					/* Create magma vein */
					cave_set_feat(xx, yy, FEAT_MAGMA);
				}

				/* Floor */
				else
				{
					/* Create floor */
					cave_set_feat(xx, yy, the_floor());
				}
			}

			/* Hack - forget square */
			forget_grid(pc_ptr);
		}
	}

	/* Update the health bar */
	p_ptr->redraw |= (PR_HEALTH);

	/* Success */
	return (TRUE);
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
	int i, j;

	/* Clear them all */
	for (i = 0; i < temp_n; i++)
	{
		for (j = 0; j < 8; j++)
		{
			int y = temp_y[i] + ddy_ddd[j];
			int x = temp_x[i] + ddx_ddd[j];

			cave_type *c_ptr;

			/* Verify */
			if (!in_bounds2(x, y)) continue;
			
			c_ptr = area(x, y);

			/* No longer in the array */
			c_ptr->info &= ~(CAVE_TEMP);

			/* Update only non-CAVE_GLOW grids */
			if (c_ptr->info & (CAVE_GLOW)) continue;

			/* Perma-Lite */
			c_ptr->info |= (CAVE_GLOW);

			/* Process affected monsters */
			if (c_ptr->m_idx)
			{
				int chance = 25;

				monster_type *m_ptr = &m_list[c_ptr->m_idx];

				monster_race *r_ptr = &r_info[m_ptr->r_idx];

				/* Stupid monsters rarely wake up */
				if (FLAG(r_ptr, RF_STUPID)) chance = 10;

				/* Smart monsters always wake up */
				if (FLAG(r_ptr, RF_SMART)) chance = 100;

				/* Sometimes monsters wake up */
				if (m_ptr->csleep && (randint0(100) < chance))
				{
					/* Wake up! */
					m_ptr->csleep = 0;

					/* Notice the "waking up" */
					if (m_ptr->ml)
					{
						/* Dump a message */
						msgf("%^v wakes up.", MONSTER_FMT(m_ptr, 0));

						/* Redraw the health bar */
						if (p_ptr->health_who == c_ptr->m_idx)
							p_ptr->redraw |= (PR_HEALTH);
					}
				}
			}

			/* Note + Redraw */
			note_spot(x, y);
		}
	}

	/*
	 * Hack - recalculate the view
	 *
	 * The view must be redone afterwards because of the way
	 * light affects walls.  We simply don't know which squares
	 * are lit until the effect has completed
	 */
	p_ptr->update |= PU_VIEW | PU_MONSTERS;


	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);

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
	int i, j;

	/* Clear them all */
	for (i = 0; i < temp_n; i++)
	{
		for (j = 0; j < 8; j++)
		{
			int y = temp_y[i] + ddy_cdd[j];
			int x = temp_x[i] + ddx_cdd[j];

			cave_type *c_ptr;
			pcave_type *pc_ptr;

			/* Verify */
			if (!in_boundsp(x, y)) continue;

			c_ptr = area(x, y);
			pc_ptr = parea(x, y);

			/* No longer in the array */
			c_ptr->info &= ~(CAVE_TEMP);

			/* Darken the grid */
			c_ptr->info &= ~(CAVE_GLOW);
			
			/* Notice + Redraw */
			note_spot(x, y);
		}
	}

	/*
	 * Hack - recalculate the view
	 *
	 * The view must be redone afterwards because of the way
	 * light affects walls.  We simply don't know which squares
	 * are unlit until the effect has completed.
	 */
	p_ptr->update |= PU_VIEW | PU_MONSTERS;


	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);

	/* None left */
	temp_n = 0;

	/* None left */
	temp_n = 0;
}


/*
 * Determine how much contiguous open space this grid is next to
 */
static int next_to_open(int cx, int cy)
{
	int i;

	int y, x;

	int len = 0;
	int blen = 0;

	cave_type *c_ptr;

	for (i = 0; i < 16; i++)
	{
		y = cy + ddy_cdd[i % 8];
		x = cx + ddx_cdd[i % 8];

		if (!in_bounds2(x, y)) continue;

		c_ptr = area(x, y);

		/* Found a wall, break the length */
		if (cave_wall_grid(c_ptr))
		{
			/* Track best length */
			if (len > blen)
			{
				blen = len;
			}

			len = 0;
		}
		else
		{
			len++;
		}
	}

	return (MAX(len, blen));
}


static int next_to_walls_adj(int cx, int cy)
{
	int i;

	int y, x;

	int c = 0;

	cave_type *c_ptr;

	for (i = 0; i < 8; i++)
	{
		y = cy + ddy_ddd[i];
		x = cx + ddx_ddd[i];

		if (!in_bounds2(x, y)) continue;

		c_ptr = area(x, y);

		if (cave_wall_grid(c_ptr)) c++;
	}

	return c;
}


/*
 * Aux function -- see below
 */
static void cave_temp_room_aux(int x, int y)
{
	cave_type *c_ptr;

	/* Verify */
	if (!in_bounds2(x, y)) return;

	/* Get the grid */
	c_ptr = area(x, y);

	/* Avoid infinite recursion */
	if (c_ptr->info & (CAVE_TEMP)) return;

	/* If a wall, exit */
	if (cave_wall_grid(c_ptr)) return;

	/* Do not exceed the maximum spell range */
	if (distance(p_ptr->px, p_ptr->py, x, y) > MAX_RANGE) return;


	/* Verify this grid
	 *
	 * The reason why it is ==6 instead of >5 is that 8 is impossible
	 * due to the check for cave_floor_grid above.
	 * 7 lights dead-end corridors (you need to do this for the
	 * checkboard interesting rooms, so that the boundary is lit
	 * properly.
	 * This leaves only a check for 6 bounding walls!
	 */
	if ((next_to_walls_adj(x, y) == 6) && (next_to_open(x, y) <= 1)) return;

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
void lite_room(int x1, int y1)
{
	int i, x, y;

	cave_type *c_ptr;

	/* Add the initial grid */
	cave_temp_room_aux(x1, y1);

	/* While grids are in the queue, add their neighbors */
	for (i = 0; i < temp_n; i++)
	{
		x = temp_x[i], y = temp_y[i];

		c_ptr = area(x, y);

		/* Walls get lit, but stop light */
		if (cave_wall_grid(c_ptr)) continue;

		/* Spread adjacent */
		cave_temp_room_aux(x + 1, y);
		cave_temp_room_aux(x - 1, y);
		cave_temp_room_aux(x, y + 1);
		cave_temp_room_aux(x, y - 1);

		/* Spread diagonal */
		cave_temp_room_aux(x + 1, y + 1);
		cave_temp_room_aux(x - 1, y - 1);
		cave_temp_room_aux(x - 1, y + 1);
		cave_temp_room_aux(x + 1, y - 1);
	}

	/* Now, lite them all up at once */
	cave_temp_room_lite();
}


/*
 * Darken all rooms containing the given location
 */
void unlite_room(int x1, int y1)
{
	int i, x, y;

	cave_type *c_ptr;

	/* Add the initial grid */
	cave_temp_room_aux(x1, y1);

	/* Spread, breadth first */
	for (i = 0; i < temp_n; i++)
	{
		x = temp_x[i], y = temp_y[i];

		c_ptr = area(x, y);

		/* Walls get dark, but stop darkness */
		if (cave_wall_grid(c_ptr)) continue;

		/* Spread adjacent */
		cave_temp_room_aux(x + 1, y);
		cave_temp_room_aux(x - 1, y);
		cave_temp_room_aux(x, y + 1);
		cave_temp_room_aux(x, y - 1);

		/* Spread diagonal */
		cave_temp_room_aux(x + 1, y + 1);
		cave_temp_room_aux(x - 1, y - 1);
		cave_temp_room_aux(x - 1, y + 1);
		cave_temp_room_aux(x + 1, y - 1);
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
	u16b flg = PROJECT_GRID | PROJECT_KILL;

	/* Hack -- Message */
	if (!p_ptr->tim.blind)
	{
		msgf("You are surrounded by a white light.");
	}

	/* Hook into the "project()" function */
	(void)project(0, rad, p_ptr->px, p_ptr->py, dam, GF_LITE_WEAK, flg);

	/* Lite up the room */
	lite_room(p_ptr->px, p_ptr->py);

	/* Assume seen */
	return (TRUE);
}


/*
 * Hack -- call darkness around the player
 * Affect all monsters in the projection radius
 */
bool unlite_area(int dam, int rad)
{
	u16b flg = PROJECT_GRID | PROJECT_KILL;

	/* Hack -- Message */
	if (!p_ptr->tim.blind)
	{
		msgf("Darkness surrounds you.");
	}

	/* Hook into the "project()" function */
	(void)project(0, rad, p_ptr->px, p_ptr->py, dam, GF_DARK_WEAK, flg);

	/* Lite up the room */
	unlite_room(p_ptr->px, p_ptr->py);

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

	u16b flg = PROJECT_STOP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

	/* Use the given direction */
	tx = p_ptr->px + 99 * ddx[dir];
	ty = p_ptr->py + 99 * ddy[dir];

	/* Hack -- Use an actual "target" */
	if ((dir == 5) && target_okay())
	{
		flg &= ~(PROJECT_STOP);
		tx = p_ptr->target_col;
		ty = p_ptr->target_row;
	}

	/* Analyze the "dir" and the "target".  Hurt items on floor. */
	return (project(0, rad, tx, ty, dam, typ, flg));
}


/*
 * Switch position with a monster.
 *
 * This function allows the player to teleport into a wall!
 */
bool teleport_swap(int dir)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

	int tx, ty;
	cave_type *c_ptr;
	monster_type *m_ptr;
	monster_race *r_ptr;

	if ((dir == 5) && target_okay())
	{
		tx = p_ptr->target_col;
		ty = p_ptr->target_row;
	}
	else
	{
		tx = px + ddx[dir];
		ty = py + ddy[dir];
	}

	if (!in_bounds2(tx, ty))
	{
		msgf("You can't trade places with that!");

		/* Failure */
		return FALSE;
	}

	c_ptr = area(tx, ty);

	if (!c_ptr->m_idx)
	{
		msgf("You can't trade places with that!");

		/* Failure */
		return FALSE;
	}

	m_ptr = &m_list[c_ptr->m_idx];
	r_ptr = &r_info[m_ptr->r_idx];

	if (FLAG(r_ptr, RF_RES_TELE))
	{
		msgf("Your teleportation is blocked!");

		/* Failure */
		return FALSE;
	}

	sound(SOUND_TELEPORT);

	/* Move monster */
	area(px, py)->m_idx = c_ptr->m_idx;

	/* Update the old location */
	c_ptr->m_idx = 0;

	/* Move the monster */
	m_ptr->fy = py;
	m_ptr->fx = px;

	/* Move the player */
	px = tx;
	py = ty;

	/* Move the player */
	p_ptr->px = tx;
	p_ptr->py = ty;

	/* Notice movement */
	Term_move_player();

	tx = m_ptr->fx;
	ty = m_ptr->fy;

	if (!p_ptr->depth)
	{
		/* Scroll wilderness */
		p_ptr->wilderness_x = px;
		p_ptr->wilderness_y = py;
		move_wild();
	}

	/* Update the monster (new location) */
	update_mon(area(tx, ty)->m_idx, TRUE);

	/* Redraw the old grid */
	lite_spot(tx, ty);

	/* Redraw the new grid */
	lite_spot(px, py);

	/* Process fields under the player. */
	field_script(area(px, py), FIELD_ACT_PLAYER_ENTER, "");

	/* Process fields under the monster. */
	field_script(area(m_ptr->fx, m_ptr->fy),
			   FIELD_ACT_MONSTER_ENTER, "");

	/* Check for new panel (redraw map) */
	verify_panel();

	/* Update stuff */
	p_ptr->update |= (PU_VIEW | PU_FLOW);

	/* Notice changes in view */
	if (FLAG(r_ptr, RF_LITE_1) || FLAG(r_ptr, RF_LITE_2))
	{
		/* Update some things */
		p_ptr->update |= (PU_MON_LITE);
	}

	/* Update the monsters */
	p_ptr->update |= (PU_DISTANCE);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);

	/* Handle stuff XXX XXX XXX */
	handle_stuff();

	/* Success */
	return TRUE;
}


/*
 * Hack -- apply a "projection()" in a direction (or at the target)
 */
bool project_hook(int typ, int dir, int dam, u16b flg)
{
	int tx, ty;

	/* Pass through the target if needed */
	flg |= (PROJECT_THRU);

	/* Use the given direction */
	tx = p_ptr->px + 99 * ddx[dir];
	ty = p_ptr->py + 99 * ddy[dir];

	/* Hack -- Use an actual "target" */
	if ((dir == 5) && target_okay())
	{
		tx = p_ptr->target_col;
		ty = p_ptr->target_row;
	}

	/* Analyze the "dir" and the "target", do NOT explode */
	return (project(0, 0, tx, ty, dam, typ, flg));
}


/*
 * Cast a bolt spell
 * Stop if we hit a monster, as a "bolt"
 * Affect monsters (not grids or objects)
 */
bool fire_bolt(int typ, int dir, int dam)
{
	u16b flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(typ, dir, dam, flg));
}


/*
 * Cast a beam spell
 * Pass through monsters, as a "beam"
 * Affect monsters (not grids or objects)
 */
bool fire_beam(int typ, int dir, int dam)
{
	u16b flg = PROJECT_BEAM | PROJECT_KILL;
	return (project_hook(typ, dir, dam, flg));
}


/*
 * Cast a bolt spell, or rarely, a beam spell
 */
bool fire_bolt_or_beam(int prob, int typ, int dir, int dam)
{
	if (randint0(100) < prob)
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
bool lite_line(int dir, int dam)
{
	u16b flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_KILL;
	return (project_hook(GF_LITE_WEAK, dir, dam, flg));
}

/* Drain life from monster, and do not give it to the player */
bool drain_life(int dir, int dam)
{
	u16b flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_OLD_DRAIN, dir, dam, flg));
}

/* Drain life from monster, and give it to the player */
bool drain_gain_life(int dir, int dam)
{
	u16b flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_NEW_DRAIN, dir, dam, flg));
}

bool wall_to_mud(int dir)
{
	u16b flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;
	return (project_hook(GF_KILL_WALL, dir, rand_range(20, 50), flg));
}


bool wizard_lock(int dir)
{
	u16b flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;
	return (project_hook(GF_JAM_DOOR, dir, rand_range(20, 50), flg));
}


bool destroy_door(int dir)
{
	u16b flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM;
	return (project_hook(GF_KILL_DOOR, dir, 60, flg));
}


bool disarm_trap(int dir)
{
	u16b flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM;
	return (project_hook(GF_KILL_TRAP, dir, 60, flg));
}


bool heal_monster(int dir)
{
	u16b flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_OLD_HEAL, dir, damroll(4, 6), flg));
}


bool speed_monster(int dir)
{
	u16b flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_OLD_SPEED, dir, p_ptr->lev, flg));
}


bool slow_monster(int dir)
{
	u16b flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_OLD_SLOW, dir, p_ptr->lev, flg));
}


bool sleep_monster(int dir)
{
	u16b flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_OLD_SLEEP, dir, p_ptr->lev, flg));
}


bool stasis_monster(int dir)
{
	u16b flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_STASIS, dir, p_ptr->lev, flg));
}


bool confuse_monster(int dir, int plev)
{
	u16b flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_OLD_CONF, dir, plev, flg));
}


bool stun_monster(int dir, int plev)
{
	u16b flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_STUN, dir, plev, flg));
}


bool poly_monster(int dir)
{
	u16b flg = PROJECT_STOP | PROJECT_KILL;
	bool tester = (project_hook(GF_OLD_POLY, dir, p_ptr->lev, flg));

	if (tester)
		chg_virtue(V_CHANCE, 1);

	return (tester);
}


bool clone_monster(int dir)
{
	u16b flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_OLD_CLONE, dir, 0, flg));
}


bool fear_monster(int dir, int plev)
{
	u16b flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_TURN_ALL, dir, plev, flg));
}


bool death_ray(int dir, int plev)
{
	u16b flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_DEATH_RAY, dir, plev * 50, flg));
}


bool teleport_monster(int dir)
{
	u16b flg = PROJECT_BEAM | PROJECT_KILL;
	return (project_hook(GF_AWAY_ALL, dir, MAX_SIGHT * 5, flg));
}


/*
 * Hooks -- affect adjacent grids (radius 1 ball attack)
 */
bool door_creation(void)
{
	u16b flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;
	return (project(0, 1, p_ptr->px, p_ptr->py, 0, GF_MAKE_DOOR, flg));
}


bool trap_creation(void)
{
	u16b flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;
	return (project(0, 1, p_ptr->px, p_ptr->py, 0, GF_MAKE_TRAP, flg));
}


bool glyph_creation(void)
{
	u16b flg = PROJECT_GRID | PROJECT_ITEM;
	return (project(0, 1, p_ptr->px, p_ptr->py, 0, GF_MAKE_GLYPH, flg));
}


bool wall_stone(void)
{
	u16b flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;

	bool dummy = (project(0, 1, p_ptr->px, p_ptr->py, 0, GF_STONE_WALL, flg));

	/* Update stuff */
	p_ptr->update |= (PU_VIEW | PU_FLOW);

	/* Update the monsters */
	p_ptr->update |= (PU_MONSTERS);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);

	return dummy;
}


bool destroy_doors_touch(void)
{
	u16b flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;
	return (project(0, 1, p_ptr->px, p_ptr->py, 60, GF_KILL_DOOR, flg));
}


bool sleep_monsters_touch(void)
{
	u16b flg = PROJECT_KILL | PROJECT_HIDE;
	return (project(0, 1, p_ptr->px, p_ptr->py, p_ptr->lev, GF_OLD_SLEEP, flg));
}


void call_chaos(void)
{
	int Chaos_type, dummy, dir;
	int plev = p_ptr->lev;
	bool line_chaos = FALSE;

	int hurt_types[30] =
	{
	GF_ELEC, GF_POIS, GF_ACID, GF_COLD,
	GF_FIRE, GF_MISSILE, GF_ARROW, GF_PLASMA,
	GF_HOLY_FIRE, GF_WATER, GF_LITE, GF_DARK,
	GF_FORCE, GF_INERTIA, GF_MANA, GF_METEOR,
	GF_ICE, GF_CHAOS, GF_NETHER, GF_DISENCHANT,
	GF_SHARDS, GF_SOUND, GF_NEXUS, GF_CONFUSION,
	GF_TIME, GF_GRAVITY, GF_ROCKET, GF_NUKE,
	GF_HELL_FIRE, GF_DISINTEGRATE
	};

	Chaos_type = hurt_types[randint0(30)];
	if (one_in_(4)) line_chaos = TRUE;

	if (one_in_(6))
	{
		for (dummy = 1; dummy < 10; dummy++)
		{
			if (dummy - 5)
			{
				if (line_chaos)
					(void)fire_beam(Chaos_type, dummy, 75);
				else
					(void)fire_ball(Chaos_type, dummy, 75, 2);
			}
		}
	}
	else if (one_in_(3))
	{
		(void)fire_ball(Chaos_type, 0, 300, 8);
	}
	else
	{
		if (!get_aim_dir(&dir)) return;
		if (line_chaos)
			(void)fire_beam(Chaos_type, dir, 150);
		else
			(void)fire_ball(Chaos_type, dir, 150, 3 + (plev / 35));
	}
}


/*
 * Activate the evil Topi Ylinen curse
 * rr9: Stop the nasty things when a Cyberdemon is summoned
 * or the player gets paralyzed.
 */
bool activate_ty_curse(bool stop_ty, int *count)
{
	int px = p_ptr->px;
	int py = p_ptr->py;
	u16b flg = (PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | PROJECT_JUMP);


	do
	{
		switch (randint1(34))
		{
			case 28:  case 29:
			{
				if (!(*count))
				{
					msgf("The ground trembles...");
					(void)earthquake(px, py, rand_range(5, 15));
					if (!one_in_(6)) break;
				}

				/* Fall through */
			}
			case 30:  case 31:
			{
				if (!(*count))
				{
					msgf("A portal opens to a plane of raw mana!");
					(void)destroy_area(px, py, 20);
					project(1, 3, px, py, damroll(10, 5), GF_MANA, flg);
					if (!one_in_(6)) break;
				}

				/* Fall through */
			}
			case 32:  case 33:
			{
				if (!(*count))
				{
					msgf("Space warps about you!");
					teleport_player(damroll(10, 10));
					if (!one_in_(13)) (*count) += activate_hi_summon();
					if (!one_in_(6)) break;
				}

				/* Fall through */
			}
			case 34:
			{
				msgf("You feel a surge of energy!");
				wall_breaker();
				if (one_in_(7))
				{
					(void)project(1, 7, px, py, 50, GF_KILL_WALL, flg);
				}
				if (!one_in_(6)) break;

				/* Fall through */
			}
			case 1:  case 2:  case 3:  case 16:  case 17:
			{
				aggravate_monsters(0);
				if (!one_in_(6)) break;

				/* Fall through */
			}
			case 4:  case 5:  case 6:
			{
				(*count) += activate_hi_summon();
				if (!one_in_(6)) break;

				/* Fall through */
			}
			case 7:  case 8:  case 9:  case 18:
			{
				(*count) +=
					summon_specific(0, px, py, p_ptr->depth, 0, TRUE, FALSE,
									FALSE);
				if (!one_in_(6)) break;

				/* Fall through */
			}
			case 10:  case 11:  case 12:
			{
				msgf("You feel your life draining away...");
				lose_exp(p_ptr->exp / 16);
				if (!one_in_(6)) break;

				/* Fall through */
			}
			case 13:  case 14:  case 15:  case 19:  case 20:
			{
				/* The TY_CURSE is effectively a level 80 monster */
				if (stop_ty ||
					((FLAG(p_ptr, TR_FREE_ACT)) &&
					 (player_save(80))))
				{
					/* Do nothing */ ;
				}
				else
				{
					msgf("You feel like a statue!");
					if (FLAG(p_ptr, TR_FREE_ACT))
					{
						(void)inc_paralyzed(randint1(3));
					}
					else
					{
						(void)inc_paralyzed(randint1(13));
					}
					stop_ty = TRUE;
				}
				if (!one_in_(6)) break;

				/* Fall through */
			}
			case 21:  case 22:  case 23:
			{
				(void)do_dec_stat(randint0(6));
				if (!one_in_(6)) break;

				/* Fall through */
			}
			case 24:
			{
				msgf("Huh? Who am I? What am I doing here?");
				(void)lose_all_info();
				if (!one_in_(6)) break;

				/* Fall through */
			}
			case 25:
			{
				/*
				 * Only summon Cyberdemons deep in the dungeon.
				 */
				if ((p_ptr->depth > 65) && !stop_ty)
				{
					(*count) += summon_cyber(-1, px, py);
					stop_ty = TRUE;
					break;
				}
				if (!one_in_(6)) break;

				/* Fall through */
			}
			default:
			{
				int stat = 0;

				while (stat < A_MAX)
				{
					do
					{
						(void)do_dec_stat(stat);
					}
					while (one_in_(2));

					stat++;
				}
			}
		}
	}
	while (one_in_(3) && !stop_ty);

	return stop_ty;
}


int activate_hi_summon(void)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

	int i;
	int count = 0;

	for (i = 0; i < (randint1(9) + (p_ptr->depth / 40)); i++)
	{
		switch ((randint1(26) + (p_ptr->depth / 20)) / 2)
		{
			case 1:
			{
				count +=
					summon_specific(0, px, py, p_ptr->depth, SUMMON_ANT, TRUE,
									FALSE, FALSE);
				break;
			}
			case 2:
			{
				count +=
					summon_specific(0, px, py, p_ptr->depth, SUMMON_SPIDER,
									TRUE, FALSE, FALSE);
				break;
			}
			case 3:
			{
				count +=
					summon_specific(0, px, py, p_ptr->depth, SUMMON_HOUND, TRUE,
									FALSE, FALSE);
				break;
			}
			case 4:
			{
				count +=
					summon_specific(0, px, py, p_ptr->depth, SUMMON_HYDRA, TRUE,
									FALSE, FALSE);
				break;
			}
			case 5:
			{
				count +=
					summon_specific(0, px, py, p_ptr->depth, SUMMON_ANGEL, TRUE,
									FALSE, FALSE);
				break;
			}
			case 6:
			{
				count +=
					summon_specific(0, px, py, p_ptr->depth, SUMMON_UNDEAD,
									TRUE, FALSE, FALSE);
				break;
			}
			case 7:
			{
				count +=
					summon_specific(0, px, py, p_ptr->depth, SUMMON_DRAGON,
									TRUE, FALSE, FALSE);
				break;
			}
			case 8:
			{
				count +=
					summon_specific(0, px, py, p_ptr->depth, SUMMON_DEMON, TRUE,
									FALSE, FALSE);
				break;
			}
			case 9:
			{
				count +=
					summon_specific(0, px, py, p_ptr->depth, SUMMON_AMBERITES,
									TRUE, FALSE, FALSE);
				break;
			}
			case 10:
			{
				count +=
					summon_specific(0, px, py, p_ptr->depth, SUMMON_UNIQUE,
									TRUE, FALSE, FALSE);
				break;
			}
			case 11:
			{
				count +=
					summon_specific(0, px, py, p_ptr->depth, SUMMON_HI_UNDEAD,
									TRUE, FALSE, FALSE);
				break;
			}
			case 12:
			{
				count +=
					summon_specific(0, px, py, p_ptr->depth, SUMMON_HI_DRAGON,
									TRUE, FALSE, FALSE);
				break;
			}
			case 13:
			{
				count +=
					summon_specific(0, px, py, 100, SUMMON_CYBER, TRUE, FALSE,
									FALSE);
				break;
			}
			default:
			{
				count +=
					summon_specific(0, px, py, (((p_ptr->depth * 3) / 2) + 5),
									0, TRUE, FALSE, FALSE);
			}
		}
	}

	return count;
}


/* ToDo: check */
int summon_cyber(int who, int x, int y)
{
	int i;
	int max_cyber = (p_ptr->depth / 50) + randint1(6);
	int count = 0;

	bool friendly = FALSE;
	bool pet = FALSE;

	/* Summoned by a monster */
	if (who > 0)
	{
		monster_type *m_ptr = &m_list[who];
		friendly = is_friendly(m_ptr);
		pet = is_pet(m_ptr);
	}

	for (i = 0; i < max_cyber; i++)
	{
		count +=
			summon_specific(who, x, y, 100, SUMMON_CYBER, FALSE, friendly, pet);
	}

	return count;
}


void wall_breaker(void)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

	int i;
	int y, x;
	int attempts = 1000;

	if (randint1(80 + p_ptr->lev) < 70)
	{
		while (attempts--)
		{
			scatter(&x, &y, px, py, 4);

			if ((y != py) || (x != px)) break;
		}

		(void)project(0, 0, x, y, rand_range(20, 50), GF_KILL_WALL,
					  (PROJECT_BEAM | PROJECT_THRU | PROJECT_GRID |
					   PROJECT_ITEM | PROJECT_KILL));
	}
	else if (randint1(100) > 30)
	{
		(void)earthquake(px, py, 1);
	}
	else
	{
		int num = damroll(5, 3);

		for (i = 0; i < num; i++)
		{
			while (1)
			{
				scatter(&x, &y, px, py, 4);

				if ((y != py) || (x != px)) break;
			}

			(void)project(0, 0, x, y, rand_range(20, 50), GF_KILL_WALL,
						  (PROJECT_BEAM | PROJECT_THRU | PROJECT_GRID |
						   PROJECT_ITEM | PROJECT_KILL));
		}
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


/*
 * Death-ray all monsters (note: OBSCENELY powerful)
 */
bool deathray_monsters(void)
{
	return (project_hack(GF_DEATH_RAY, p_ptr->lev * 50));
}


bool charm_monster(int dir, int plev)
{
	u16b flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_CHARM, dir, plev, flg));
}


bool control_one_undead(int dir, int plev)
{
	u16b flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_CONTROL_UNDEAD, dir, plev, flg));
}


bool charm_animal(int dir, int plev)
{
	u16b flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_CONTROL_ANIMAL, dir, plev, flg));
}


bool starlite(void)
{
	int k;
	int num = damroll(5, 3);
	int y, x;
	int attempts;

	cave_type *c_ptr;

	for (k = 0; k < num; k++)
	{
		attempts = 1000;

		while (attempts--)
		{
			scatter(&x, &y, p_ptr->px, p_ptr->py, 4);

			/* paranoia */
			if (!in_bounds2(x, y)) continue;

			c_ptr = area(x, y);

			if (cave_wall_grid(c_ptr)) continue;

			if ((y != p_ptr->py) || (x != p_ptr->px)) break;
		}

		(void)project(0, 0, x, y, damroll(6, 8), GF_LITE_WEAK,
					  (PROJECT_BEAM | PROJECT_THRU | PROJECT_GRID |
					   PROJECT_KILL));
	}

	return (TRUE);
}


bool scatter_ball(int num, int type, int dam, int rad)
{
	int k;
	int y, x;
	int attempts;

	cave_type *c_ptr;

	for (k = 0; k < num; k++)
	{
		attempts = 1000;

		while (attempts--)
		{
			scatter(&x, &y, p_ptr->px, p_ptr->py, 4);

			/* paranoia */
			if (!in_bounds2(x, y)) continue;

			c_ptr = area(x, y);

			if (cave_wall_grid(c_ptr)) continue;

			if ((y != p_ptr->py) || (x != p_ptr->px)) break;
		}

		project(0, rad, x, y, dam, type,
				(PROJECT_THRU | PROJECT_STOP | PROJECT_GRID |
				 PROJECT_ITEM | PROJECT_KILL));
	}

	return (TRUE);
}


void create_food()
{
	object_type *q_ptr;

	/* Hack - create a food ration */
	q_ptr = object_prep(lookup_kind(TV_FOOD, SV_FOOD_RATION));

	/* Drop the object from heaven */
	drop_near(q_ptr, -1, p_ptr->px, p_ptr->py);
}

void whirlwind_attack()
{
	int y = 0, x = 0, dir;
	cave_type *c_ptr;
	monster_type *m_ptr;

	for (dir = 0; dir <= 9; dir++)
	{
		y = p_ptr->py + ddy[dir];
		x = p_ptr->px + ddx[dir];

		/* paranoia */
		if (!in_bounds2(x, y)) return;
		c_ptr = area(x, y);

		/* Get the monster */
		m_ptr = &m_list[c_ptr->m_idx];

		/* Hack -- attack monsters */
		if (c_ptr->m_idx && (m_ptr->ml || cave_floor_grid(c_ptr)))
			py_attack(x, y);
	}
}
