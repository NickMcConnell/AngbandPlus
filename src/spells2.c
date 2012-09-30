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
	int i = 0, j, k, x;

	int v_nr;
	char v_string[8][128];

	u32b f1 = 0L, f2 = 0L, f3 = 0L;

	object_type *o_ptr;
	const mutation_type *mut_ptr;

	char Dummy[80], Liferating[80];

	cptr info[220];

	int plev = p_ptr->lev;

	int percent;

	strcpy(Dummy, "");
	strcpy(Liferating, "");

	percent = (int)(((long)p_ptr->player_hp[PY_MAX_LEVEL - 1] * 200L) /
		(2 * p_ptr->hitdie +
		((PY_MAX_LEVEL - 1) * (p_ptr->hitdie + 1))));

	sprintf(Liferating, "Your current Life Rating is %d/100.", percent);
	info[i++] = Liferating;

	chg_virtue(V_KNOWLEDGE, 1);
	chg_virtue(V_ENLIGHTEN, 1);

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

	for (v_nr = 0; v_nr < MAX_PLAYER_VIRTUES; v_nr++)
	{
		char v_name[20];
		char vir_desc[80];
		int tester = p_ptr->virtues[v_nr];

		strcpy(v_name, virtue[(p_ptr->vir_types[v_nr]) - 1]);

		sprintf(vir_desc, "Oops. No info about %s.", v_name);
		if (tester < -100)
			sprintf(vir_desc, "You are the polar opposite of %s (%d).",
				v_name, tester);
		else if (tester < -80)
			sprintf(vir_desc, "You are an arch-enemy of %s (%d).",
				v_name, tester);
		else if (tester < -60)
			sprintf(vir_desc, "You are a bitter enemy of %s (%d).",
				v_name, tester);
		else if (tester < -40)
			sprintf(vir_desc, "You are an enemy of %s (%d).",
				v_name, tester);
		else if (tester < -20)
			sprintf(vir_desc, "You have sinned against %s (%d).",
				v_name, tester);
		else if (tester < 0)
			sprintf(vir_desc, "You have strayed from the path of %s (%d).",
				v_name, tester);
		else if (tester == 0)
			sprintf(vir_desc,"You are neutral to %s (%d).",
				v_name, tester);
		else if (tester < 20)
			sprintf(vir_desc,"You are somewhat virtuous in %s (%d).",
				v_name, tester);
		else if (tester < 40)
			sprintf(vir_desc,"You are virtuous in %s (%d).",
				v_name, tester);
		else if (tester < 60)
			sprintf(vir_desc,"You are very virtuous in %s (%d).",
				v_name, tester);
		else if (tester < 80)
			sprintf(vir_desc,"You are a champion of %s (%d).",
				v_name, tester);
		else if (tester < 100)
			sprintf(vir_desc,"You are a great champion of %s (%d).",
				v_name, tester);
		else
			sprintf(vir_desc,"You are the living embodiment of %s (%d).",
		v_name, tester);

		strcpy(v_string[v_nr], vir_desc);

		info[i++] = v_string[v_nr];
	}

	/* Racial powers... */
	for (x = 0; x < MAX_RACE_POWERS; x++)
	{
		mut_ptr = &race_powers[x];

		if ((mut_ptr->which == p_ptr->prace) &&	(plev >= mut_ptr->level))
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
	if ((p_ptr->regenerate) && (!(p_ptr->muta3 & MUT3_REGEN)))
	{
		info[i++] = "You regenerate quickly.";
	}
	if (p_ptr->slow_digest)
	{
		info[i++] = "Your appetite is small.";
	}
	if ((p_ptr->telepathy) && (!(p_ptr->muta3 & MUT3_ESP)))
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
	if (p_ptr->resist_confu)
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
	if (p_ptr->resist_nethr)
	{
		info[i++] = "You are resistant to nether forces.";
	}
	if ((p_ptr->resist_fear) && (!(p_ptr->muta3 & MUT3_ESP)))
	{
		info[i++] = "You are completely fearless.";
	}
	if (p_ptr->resist_blind)
	{
		info[i++] = "Your eyes are resistant to blindness.";
	}

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
			info[i++] = "Your weapon is branded with the Sign of Logrus.";
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
		if (f1 & (TR1_SLAY_DRAGON))
		{
			info[i++] = "Your weapon is especially deadly against dragons.";
		}

		/* Special "kill" flags */
		if (f1 & (TR1_KILL_DRAGON))
		{
			info[i++] = "Your weapon is a great bane of dragons.";
		}

		if (f2 & (TR2_THROW))
		{
			info[i++] = "Your weapon can be thrown well.";
		}
	}


	/* Save the screen */
	screen_save();

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
	int     i = 0, j, k;
	char    Dummy[80];
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
		info[i++] = "You feel righteous";
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
		info[i++] = "You are waiting to be recalled";
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
	screen_save();

	/* Erase the screen */
	for (k = 1; k < 24; k++) prt("", k, 13);

	/* Label the information */
	prt("     Your Current Magic:", 1, 15);

	/* We will print on top of the map (column 13) */
	for (k = 2, j = 0; j < i; j++)
	{
		/* Show the info */
		sprintf(Dummy, "%s %s.", info[j],
			report_magic_durations[info2[j]]);
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
	screen_load();
}


/*
 * Detect all traps in range
 */
bool detect_traps(void)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

	int             x, y;
	bool            detect = FALSE;
	cave_type       *c_ptr;

	/* Save center of detection radius */
	p_ptr->detectx = px;
	p_ptr->detecty = py;

	/* Have detected traps on this level */
	p_ptr->detected = TRUE;


	/* Scan a radius MAX_DETECT circle */
	for (y = py - MAX_DETECT; y <= py + MAX_DETECT; y++)
	{
		for (x = px - MAX_DETECT; x <= px + MAX_DETECT; x++)
		{
			if (!in_bounds2(y, x)) continue;

			if (distance(px, py, x, y) > MAX_DETECT) continue;

			/* Access the grid */
			c_ptr = area(y,x);

			/* Detect traps */
			if (field_detect_type(c_ptr->fld_idx, FTYPE_TRAP))
			{
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
 * Detect all doors in range
 */
bool detect_doors(void)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

	int y, x;

	bool detect = FALSE;

	cave_type *c_ptr;
	monster_type *m_ptr;

	/* Scan a radius MAX_DETECT circle */
	for (y = py - MAX_DETECT; y <= py + MAX_DETECT; y++)
	{
		for (x = px - MAX_DETECT; x <= px + MAX_DETECT; x++)
		{
			if (!in_bounds2(y, x)) continue;

			if (distance(px, py, x, y) > MAX_DETECT) continue;

			c_ptr = area(y,x);

			/* Detect secret doors */
			if (c_ptr->feat == FEAT_SECRET)
			{
				/* Pick a door */
				place_closed_door(y, x);
			}

			/* Detect doors */
			if ((c_ptr->feat == FEAT_CLOSED) ||
			    (c_ptr->feat == FEAT_OPEN) ||
			     (c_ptr->feat == FEAT_BROKEN))
			{
				/* Hack -- Memorize */
				c_ptr->info |= (CAVE_MARK);

				/* Redraw */
				lite_spot(y, x);

				/* Obvious */
				detect = TRUE;
			}
			
			/* Hack - look for door mimics */
			if (c_ptr->m_idx)
			{
				m_ptr = &m_list[c_ptr->m_idx];
				
				/* Door mimic? */
				if ((m_ptr->smart & SM_MIMIC) &&
					 (r_info[m_ptr->r_idx].d_char == '+'))
				{
					/* Clear the mimic flag */
					m_ptr->smart &= ~(SM_MIMIC);
					
					/* It is in the monster list now */
					update_mon_vis(m_ptr->r_idx, 1);
					
					/* Hack -- Detect monster */
					m_ptr->mflag |= (MFLAG_MARK | MFLAG_SHOW);
					
					/* Update the monster */
					update_mon(m_ptr->r_idx, FALSE);
					
					detect = TRUE;
				}
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
 * Detect all stairs in range
 */
bool detect_stairs(void)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

	int y, x;

	bool detect = FALSE;

	cave_type *c_ptr;
	monster_type *m_ptr;

	/* Scan a radiuc MAX_DETECT circle */
	for (y = py - MAX_DETECT; y <= py + MAX_DETECT; y++)
	{
		for (x = px - MAX_DETECT; x <= px + MAX_DETECT; x++)
		{
			if (!in_bounds2(y, x)) continue;

			if (distance(px, py, x, y) > MAX_DETECT) continue;

			c_ptr = area(y,x);

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
			
			/* Hack - look for stair mimics */
			if (c_ptr->m_idx)
			{
				m_ptr = &m_list[c_ptr->m_idx];
				
				/* Stair mimic? */
				if ((m_ptr->smart & SM_MIMIC) &&
					 (r_info[m_ptr->r_idx].d_char == '>'))
				{
					/* Clear the mimic flag */
					m_ptr->smart &= ~(SM_MIMIC);
					
					/* It is in the monster list now */
					update_mon_vis(m_ptr->r_idx, 1);
					
					/* Hack -- Detect monster */
					m_ptr->mflag |= (MFLAG_MARK | MFLAG_SHOW);
					
					/* Update the monster */
					update_mon(m_ptr->r_idx, FALSE);
					
					detect = TRUE;
				}
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
 * Detect any treasure in range
 */
bool detect_treasure(void)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

	int y, x;

	bool detect = FALSE;

	cave_type *c_ptr;


	/* Scan a radius MAX_DETECT circle */
	for (y = py - MAX_DETECT; y <= py + MAX_DETECT; y++)
	{
		for (x = px - MAX_DETECT; x <= px + MAX_DETECT; x++)
		{
			if (!in_bounds2(y, x)) continue;

			if (distance(px, py, x, y) > MAX_DETECT) continue;

			c_ptr = area(y,x);

			/* Notice embedded gold */

			/* Does "embeded" gold really exist??? */
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
 * Detect all "gold" objects in range
 */
bool detect_objects_gold(void)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

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

		if (distance(px, py, x, y) > MAX_DETECT) continue;

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
#if 0		
		/* Hack - look for coins mimics */
		if (c_ptr->m_idx)
		{
			m_ptr = &m_list[m_idx];
				
			/* Coins mimic? */
			if ((m_ptr->smart & SM_MIMIC) &&
				 (r_info[m_ptr->r_idx].d_char == '$'))
			{
				/* Clear the mimic flag */
				m_ptr->smart &= ~(SM_MIMIC);
				
				/* It is in the monster list now */
				update_mon_vis(m_ptr->r_idx, 1);
					
				/* Hack -- Detect monster */
				m_ptr->mflag |= (MFLAG_MARK | MFLAG_SHOW);
					
				/* Update the monster */
				update_mon(i, FALSE);
					
				detect = TRUE;
			}
		}
#endif /* 0 */
	}

	/* Describe */
	if (detect)
	{
		msg_print("You sense the presence of treasure!");
	}

	if (detect_monsters_string("$"))
	{
		detect = TRUE;
	}

	/* Result */
	return (detect);
}


/*
 * Detect all "normal" objects in range
 */
bool detect_objects_normal(void)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

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

		if (distance(px, py, x, y) > MAX_DETECT) continue;

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
#if 0		
		/* Hack - look for object mimics */
		if (c_ptr->m_idx)
		{
			m_ptr = &m_list[m_idx];
				
			/* Mimic? */
			if ((m_ptr->smart & SM_MIMIC) &&
				 (strchr("$(=?!|", r_info[m_ptr->r_idx].d_char)))
			{
				/* Clear the mimic flag */
				m_ptr->smart &= ~(SM_MIMIC);
				
				/* It is in the monster list now */
				update_mon_vis(m_ptr->r_idx, 1);
					
				/* Hack -- Detect monster */
				m_ptr->mflag |= (MFLAG_MARK | MFLAG_SHOW);
					
				/* Update the monster */
				update_mon(i, FALSE);
					
				detect = TRUE;
			}
		}
#endif /* 0 */

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
	int px = p_ptr->px;
	int py = p_ptr->py;

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

		if (distance(px, py, x, y) > MAX_DETECT) continue;

		/* Examine the tval */
		tv = o_ptr->tval;

		/* Artifacts, misc magic items, or enchanted wearables */
		if (o_ptr->xtra_name ||
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
		    ((o_ptr->to_a > 0) || (o_ptr->to_h + o_ptr->to_d > 0)))
		{
			/* Memorize the item */
			o_ptr->marked = TRUE;

			/* Redraw */
			lite_spot(y, x);

			/* Detect */
			detect = TRUE;
		}
#if 0
		
		/* Hack - look for object mimics */
		if (c_ptr->m_idx)
		{
			m_ptr = &m_list[m_idx];
				
			/* Mimic? */
			if ((m_ptr->smart & SM_MIMIC) &&
				 (strchr("=?!", r_info[m_ptr->r_idx].d_char)))
			{
				/* Clear the mimic flag */
				m_ptr->smart &= ~(SM_MIMIC);
				
				/* It is in the monster list now */
				update_mon_vis(m_ptr->r_idx, 1);
					
				/* Hack -- Detect monster */
				m_ptr->mflag |= (MFLAG_MARK | MFLAG_SHOW);
				
				/* Update the monster */
				update_mon(i, FALSE);
					
				detect = TRUE;
			}
		}
#endif /* 0 */
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
 * Detect all "normal" monsters in range
 */
bool detect_monsters_normal(void)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

	int i, y, x;

	bool flag = FALSE;


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

		if (distance(px, py, x, y) > MAX_DETECT) continue;
		
		/* Not mimics */
		if (m_ptr->smart & SM_MIMIC) continue;

		/* Detect all non-invisible monsters */
		if ((!(r_ptr->flags2 & RF2_INVISIBLE)) ||
		    p_ptr->see_inv || p_ptr->tim_invis)
		{
			/* Repair visibility later */
			repair_monsters = TRUE;

			/* Hack -- Detect monster */
			m_ptr->mflag |= (MFLAG_MARK | MFLAG_SHOW);

			/* Update the monster */
			update_mon(i, FALSE);

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
 * Detect all "invisible" monsters in range
 */
bool detect_monsters_invis(void)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

	int i, y, x;
	bool flag = FALSE;

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

		if (distance(px, py, x, y) > MAX_DETECT) continue;

		/* Detect invisible monsters */
		if (r_ptr->flags2 & RF2_INVISIBLE)
		{
			/* Update monster recall window */
			if (p_ptr->monster_race_idx == m_ptr->r_idx)
			{
				/* Window stuff */
				p_ptr->window |= (PW_MONSTER);
			}

			/* Repair visibility later */
			repair_monsters = TRUE;

			/* Hack -- Detect monster */
			m_ptr->mflag |= (MFLAG_MARK | MFLAG_SHOW);

			/* Update the monster */
			update_mon(i, FALSE);

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
 * Detect all "evil" monsters in range
 */
bool detect_monsters_evil(void)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

	int i, y, x;
	bool flag = FALSE;


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

		if (distance(px, py, x, y) > MAX_DETECT) continue;

		/* Detect evil monsters */
		if (r_ptr->flags3 & RF3_EVIL)
		{
			/* Take note that they are evil */
			r_ptr->r_flags3 |= (RF3_EVIL);

			/* Update monster recall window */
			if (p_ptr->monster_race_idx == m_ptr->r_idx)
			{
				/* Window stuff */
				p_ptr->window |= (PW_MONSTER);
			}

			/* Repair visibility later */
			repair_monsters = TRUE;

			/* Hack -- Detect monster */
			m_ptr->mflag |= (MFLAG_MARK | MFLAG_SHOW);

			/* Update the monster */
			update_mon(i, FALSE);

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
 * Detect all (string) monsters in range
 */
bool detect_monsters_string(cptr Match)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

	int i, y, x;
	bool flag = FALSE;


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

		if (distance(px, py, x, y) > MAX_DETECT) continue;

		/* Detect monsters with the same symbol */
		if (strchr(Match, r_ptr->d_char))
		{
			/* Update monster recall window */
			if (p_ptr->monster_race_idx == m_ptr->r_idx)
			{
				/* Window stuff */
				p_ptr->window |= (PW_MONSTER);
			}

			/* Repair visibility later */
			repair_monsters = TRUE;

			/* Hack -- Detect monster */
			m_ptr->mflag |= (MFLAG_MARK | MFLAG_SHOW);

			/* Update the monster */
			update_mon(i, FALSE);

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
 * A "generic" detect monsters routine, tagged to flags3
 */
bool detect_monsters_xxx(u32b match_flag)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

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

		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;

		if (distance(px, py, x, y) > MAX_DETECT) continue;

		/* Detect evil monsters */
		if (r_ptr->flags3 & (match_flag))
		{
			/* Take note that they are something */
			r_ptr->r_flags3 |= (match_flag);

			/* Update monster recall window */
			if (p_ptr->monster_race_idx == m_ptr->r_idx)
			{
				/* Window stuff */
				p_ptr->window |= (PW_MONSTER);
			}

			/* Repair visibility later */
			repair_monsters = TRUE;

			/* Hack -- Detect monster */
			m_ptr->mflag |= (MFLAG_MARK | MFLAG_SHOW);

			/* Update the monster */
			update_mon(i, FALSE);

			/* Detect */
			flag = TRUE;
		}
	}

	/* Describe */
	if (flag)
	{
		switch (match_flag)
		{
			case RF3_DEMON:
				desc_monsters = "demons";
				break;
			case RF3_UNDEAD:
				desc_monsters = "the undead";
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
 * Apply a "project()" directly to all viewable monsters
 *
 * Note that affected monsters are NOT auto-tracked by this usage.
 *
 * To avoid misbehavior when monster deaths have side-effects,
 * this is done in two passes. -- JDL
 */
bool project_hack(int typ, int dam)
{
	int     i, x, y;
	u16b    flg = PROJECT_JUMP | PROJECT_KILL | PROJECT_HIDE;
	bool    obvious = FALSE;

	cave_type *c_ptr;

	/* Mark all (nearby) monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;

		c_ptr = area(y, x);

		/* Require line of sight */
		if (!player_has_los_grid(c_ptr)) continue;

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

		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;

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
bool raise_dead(int y, int x, bool pet)
{
	s16b i;
	int fx, fy;

	bool    obvious = FALSE;

	cave_type *c_ptr;

	/* Check all (nearby) fields */
	for (i = 1; i < fld_max; i++)
	{
		field_type *f_ptr = &fld_list[i];

		/* Paranoia -- Skip missing objects */
		if (!f_ptr->t_idx) continue;

		/* Want a corpse / skeleton */
		if (!(f_ptr->t_idx == FT_CORPSE) ||
		     (f_ptr->t_idx == FT_SKELETON)) continue;

		/* Location */
		fy = f_ptr->fy;
		fx = f_ptr->fx;

		/* Require line of sight */
		if (!los(fy, fx, y, x)) continue;

		c_ptr = area(fy, fx);

		/* Raise Corpses / Skeletons */
		if (field_hook_special(&c_ptr->fld_idx, FTYPE_CORPSE, (vptr) &pet))
		{
			if (player_has_los_grid(c_ptr)) obvious = TRUE;
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
	int     i;
	bool    sleep = FALSE;
	bool    speed = FALSE;
	cave_type *c_ptr;

	/* Aggravate everyone nearby */
	for (i = 1; i < m_max; i++)
	{
		monster_type    *m_ptr = &m_list[i];
		monster_race    *r_ptr = &r_info[m_ptr->r_idx];

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

		c_ptr = area(m_ptr->fy, m_ptr->fx);

		/* Speed up monsters in line of sight */
		if (player_has_los_grid(c_ptr))
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
	if (speed) msg_print("You feel a sudden stirring nearby!");
	else if (sleep) msg_print("You hear a sudden stirring in the distance!");
}



/*
 * Delete all non-unique/non-quest monsters of a given "type" from the level
 */
bool genocide(int player_cast)
{
	int     i;
	char    typ;
	bool    result = FALSE;
	int     msec = delay_factor * delay_factor * delay_factor;

	/* Prevent genocide in quest levels */
	if (p_ptr->inside_quest)
	{
		return (FALSE);
	}

	/* Mega-Hack -- Get a monster symbol */
	(void)(get_com("Choose a monster race (by symbol) to genocide: ", &typ));

	/* Delete the monsters of that "type" */
	for (i = 1; i < m_max; i++)
	{
		monster_type    *m_ptr = &m_list[i];
		monster_race    *r_ptr = &r_info[m_ptr->r_idx];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Hack -- Skip Unique Monsters */
		if (r_ptr->flags1 & (RF1_UNIQUE)) continue;

		/* Hack -- Skip Quest Monsters */
		if (r_ptr->flags1 & RF1_QUESTOR) continue;

		/* Skip "wrong" monsters */
		if (r_ptr->d_char != typ) continue;

		/* Notice changes in view */
		if (r_ptr->flags7 & (RF7_LITE_1 | RF7_LITE_2))
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
		move_cursor_relative(p_ptr->py, p_ptr->px);

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
	int     i;
	bool    result = FALSE;
	int     msec = delay_factor * delay_factor * delay_factor;


	/* Prevent mass genocide in quest levels */
	if (p_ptr->inside_quest)
	{
		return (FALSE);
	}

	/* Delete the (nearby) monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type    *m_ptr = &m_list[i];
		monster_race    *r_ptr = &r_info[m_ptr->r_idx];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Hack -- Skip unique monsters */
		if (r_ptr->flags1 & (RF1_UNIQUE)) continue;

		/* Hack -- Skip Quest Monsters */
		if (r_ptr->flags1 & RF1_QUESTOR) continue;

		/* Skip distant monsters */
		if (m_ptr->cdis > MAX_SIGHT) continue;

		/* Notice changes in view */
		if (r_ptr->flags7 & (RF7_LITE_1 | RF7_LITE_2))
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

		move_cursor_relative(p_ptr->py, p_ptr->px);

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
	int     i;
	bool    probe = FALSE;
	cave_type *c_ptr;

	/* Probe all (nearby) monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		c_ptr = area(m_ptr->fy, m_ptr->fx);

		/* Require line of sight */
		if (!player_has_los_grid(c_ptr)) continue;

		/* Probe visible monsters */
		if (m_ptr->ml)
		{
			char m_name[80];

			/* Start the message */
			if (!probe) msg_print("Probing...");

			/* Get "the monster" or "something" */
			monster_desc(m_name, m_ptr, 0x04);

			/* Describe the monster */
			msg_format("%^s has %d hit points.", m_name, m_ptr->hp);

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
 * "earthquake" by using the (removed) "full" parameter 
 * to select "destruction".
 */
bool destroy_area(int y1, int x1, int r)
{
	int       y, x, k, t;
	cave_type *c_ptr;
	bool      flag = FALSE;

	/* Prevent destruction of quest levels and town */
	if (p_ptr->inside_quest || !p_ptr->depth)
	{
		return (FALSE);
	}

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
			c_ptr = area(y, x);

			/* Lose room and vault */
			c_ptr->info &= ~(CAVE_ROOM | CAVE_ICKY);

			/* Lose light and knowledge */
			c_ptr->info &= ~(CAVE_MARK | CAVE_GLOW);

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

			if (r_info[m_list[c_ptr->m_idx].r_idx].flags1 & RF1_QUESTOR)
			{
				/* Heal the monster */
				m_list[c_ptr->m_idx].hp = m_list[c_ptr->m_idx].maxhp;

				/* Try to teleport away quest monsters */
				if (!teleport_away(c_ptr->m_idx, (r * 2) + 1)) continue;
			}
			else
			{
				/* Delete the monster (if any) */
				delete_monster(y, x);
			}

			/* Fields can block destruction */
			if (fields_have_flags(c_ptr->fld_idx, FIELD_INFO_PERM)) continue;

			/* Destroy the fields on the square */
			delete_field(y, x);

			/* Destroy "valid" grids */
			if (cave_valid_grid(c_ptr))
			{
				/* Delete objects */
				delete_object(y, x);

				/* Wall (or floor) type */
				t = randint0(200);

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
			(void)set_blind(p_ptr->blind + rand_range(10, 20));
		}
	}


	/* Mega-Hack -- Forget the view */
	p_ptr->update |= (PU_UN_VIEW);

	/* Update stuff */
	p_ptr->update |= (PU_VIEW | PU_FLOW | PU_MON_LITE);

	/* Update the monsters */
	p_ptr->update |= (PU_MONSTERS);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);

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
bool earthquake(int cy, int cx, int r)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int             i, t, y, x, yy, xx, dy, dx, oy, ox;
	int             damage = 0;
	int             sn = 0, sy = 0, sx = 0;
	bool            hurt = FALSE;
	cave_type       *c_ptr;
	bool            map[32][32];
	field_mon_test	mon_enter_test;

	/* Prevent destruction of quest levels and town */
	if (p_ptr->inside_quest || !p_ptr->depth)
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
			if (!in_bounds(yy, xx)) continue;

			/* Skip distant grids */
			if (distance(cy, cx, yy, xx) > r) continue;

			/* Access the grid */
			c_ptr = area(yy,xx);

			/* Lose room and vault */
			c_ptr->info &= ~(CAVE_ROOM | CAVE_ICKY);

			/* Lose light and knowledge */
			c_ptr->info &= ~(CAVE_GLOW | CAVE_MARK);

			/* Skip the epicenter */
			if (!dx && !dy) continue;

			/* Skip most grids */
			if (randint0(100) < 85) continue;

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

			if (!in_bounds2(y, x)) continue;

			/* Access the grid */
			c_ptr = area(y, x);

			/* Skip non-empty grids */
			if (!cave_empty_grid(c_ptr)) continue;

			/* Important -- Skip "quake" grids */
			if (map[16+y-cy][16+x-cx]) continue;

			/* Check for a field that blocks movement */
			if (fields_have_flags(c_ptr->fld_idx, FIELD_INFO_NO_ENTER))
			{
				continue;
			}

			/* Count "safe" grids */
			sn++;

			/* Randomize choice */
			if (randint0(sn) > 0) continue;

			/* Save the safe location */
			sy = y; sx = x;
		}

		/* Random message */
		switch (randint1(3))
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
			switch (randint1(3))
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
					(void)set_stun(p_ptr->stun + randint1(50));
					break;
				}
				case 3:
				{
					msg_print("You are crushed between the floor and ceiling!");
					damage = damroll(10, 4);
					(void)set_stun(p_ptr->stun + randint1(50));
					break;
				}
			}

			/* Save old location */
			oy = py;
			ox = px;

			/* Process fields under the player. */
			field_hook(&area(py, px)->fld_idx,
				 FIELD_ACT_PLAYER_LEAVE, NULL);

			/* Move the player */
			py = sy;
			px = sx;

			/* Move the player */
			p_ptr->py = sy;
			p_ptr->px = sx;

			if (!p_ptr->depth)
			{
				/* Scroll wilderness */
				p_ptr->wilderness_x = px;
				p_ptr->wilderness_y = py;
				move_wild();
			}

			/* Process fields under the player. */
			field_hook(&area(py, px)->fld_idx,
				 FIELD_ACT_PLAYER_ENTER, NULL);

			/* Redraw the old spot */
			lite_spot(oy, ox);

			/* Redraw the new spot */
			lite_spot(py, px);

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

			if (!in_bounds2(yy, xx)) continue;

			/* Access the grid */
			c_ptr = area(yy, xx);

			/* Process monsters */
			if (c_ptr->m_idx)
			{
				monster_type *m_ptr = &m_list[c_ptr->m_idx];
				monster_race *r_ptr = &r_info[m_ptr->r_idx];

				/* Quest monsters */
				if (r_ptr->flags1 & RF1_QUESTOR)
				{
					/* No wall on quest monsters */
					map[16+yy-cy][16+xx-cx] = FALSE;

					continue;
				}

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
							y = yy + ddy_ddd[i];
							x = xx + ddx_ddd[i];

							if (!in_bounds2(y, x)) continue;

							/* Access the grid */
							c_ptr = area(y, x);

							/* Skip non-empty grids */
							if (!cave_empty_grid(c_ptr)) continue;

							/* Check for a field that blocks movement */
							if (fields_have_flags(c_ptr->fld_idx,
									 FIELD_INFO_NO_ENTER)) continue;

							/*
							 * Test for fields that will not allow this
							 * specific monster to pass. (i.e. Glyph of warding)
							 */

							/* Initialise info to pass to action functions */
							mon_enter_test.m_ptr = NULL;
							mon_enter_test.do_move = TRUE;

							/* Call the hook */
							field_hook(&c_ptr->fld_idx,
								 FIELD_ACT_MON_ENTER_TEST,
								 (vptr) &mon_enter_test);

							/* Get result */
							if (!mon_enter_test.do_move) continue;

							/* ... nor on the Pattern */
							if ((c_ptr->feat <= FEAT_PATTERN_XTRA2) &&
							    (c_ptr->feat >= FEAT_PATTERN_START))
								continue;

							/* Important -- Skip "quake" grids */
							if (map[16+y-cy][16+x-cx]) continue;

							/* Count "safe" grids */
							sn++;

							/* Randomize choice */
							if (randint0(sn) > 0) continue;

							/* Save the safe grid */
							sy = y; sx = x;
						}
					}

					/* Describe the monster */
					monster_desc(m_name, m_ptr, 0);

					/* Scream in pain */
					msg_format("%^s wails out in pain!", m_name);

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
						msg_format("%^s is embedded in the rock!", m_name);

						/* Delete the monster */
						delete_monster(yy, xx);

						/* No longer safe */
						sn = 0;
					}

					/* Hack -- Escape from the rock */
					if (sn)
					{
						int m_idx = area(yy,xx)->m_idx;

						/* Update the new location */
						area(sy,sx)->m_idx = m_idx;

						/* Update the old location */
						area(yy,xx)->m_idx = 0;

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

			if (!in_bounds2(yy, xx)) continue;

			/* Access the cave grid */
			c_ptr = area(yy, xx);

			/* Paranoia -- never affect player */
			if ((yy == py) && (xx == px)) continue;

			/* Fields can block destruction */
			if (fields_have_flags(c_ptr->fld_idx, FIELD_INFO_PERM)) continue;

			/* Destroy the fields on the square */
			delete_field(y, x);
			
			/* Destroy location (if valid) */
			if (cave_valid_grid(c_ptr))
			{
				bool floor = cave_floor_grid(c_ptr);

				/* Delete objects */
				delete_object(yy, xx);

				/* Wall (or floor) type */
				t = (floor ? randint0(100) : 200);

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


	/* Mega-Hack -- Forget the view */
	p_ptr->update |= (PU_UN_VIEW);

	/* Update stuff */
	p_ptr->update |= (PU_VIEW | PU_FLOW | PU_MON_LITE);

	/* Update the monsters */
	p_ptr->update |= (PU_DISTANCE);

	/* Update the health bar */
	p_ptr->redraw |= (PR_HEALTH);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);

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

			cave_type *c_ptr = area(y,x);

			/* Verify */
			if (!in_bounds2(y, x)) continue;

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

				monster_type    *m_ptr = &m_list[c_ptr->m_idx];

				monster_race    *r_ptr = &r_info[m_ptr->r_idx];

				/* Update the monster */
				update_mon(c_ptr->m_idx, FALSE);

				/* Stupid monsters rarely wake up */
				if (r_ptr->flags2 & (RF2_STUPID)) chance = 10;

				/* Smart monsters always wake up */
				if (r_ptr->flags2 & (RF2_SMART)) chance = 100;

				/* Sometimes monsters wake up */
				if (m_ptr->csleep && (randint0(100) < chance))
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

						/* Redraw the health bar */
						if (p_ptr->health_who == c_ptr->m_idx)
							p_ptr->redraw |= (PR_HEALTH);
					}
				}
			}

			/* Note + Redraw */
			note_spot(y, x);
		}
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
	int i, j;

	/* Clear them all */
	for (i = 0; i < temp_n; i++)
	{
		for (j = 0; j < 8; j++)
		{
			int y = temp_y[i] + ddy_cdd[j];
			int x = temp_x[i] + ddx_cdd[j];

			cave_type *c_ptr = area(y,x);

			/* Verify */
			if (!in_bounds2(y, x)) continue;

			/* No longer in the array */
			c_ptr->info &= ~(CAVE_TEMP);

			/* Darken the grid */
			c_ptr->info &= ~(CAVE_GLOW);

			/* Process affected monsters */
			if (c_ptr->m_idx)
			{
				/* Update the monster */
				update_mon(c_ptr->m_idx, FALSE);
			}

			/* Hack -- Forget "boring" grids */
			if (c_ptr->feat == FEAT_FLOOR)
			{
				/* Forget the grid */
				c_ptr->info &= ~(CAVE_MARK);

				/* Notice + Redraw */
				note_spot(y, x);
			}
			else
			{
				/* Redraw */
				lite_spot(y, x);
			}
		}
	}

	/* None left */
	temp_n = 0;
}


/*
 * Determine how much contiguous open space this grid is next to
 */
static int next_to_open(int cy, int cx)
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

		if (!in_bounds2(y, x)) continue;

		c_ptr = area(y, x);

		/* Found a wall, break the length */
		if (!cave_floor_grid(c_ptr))
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


static int next_to_walls_adj(int cy, int cx)
{
	int i;

	int y, x;

	int c = 0;

	cave_type *c_ptr;

	for (i = 0; i < 8; i++)
	{
		y = cy + ddy_ddd[i];
		x = cx + ddx_ddd[i];

		if (!in_bounds2(y, x)) continue;

		c_ptr = area(y, x);

		if (!cave_floor_grid(c_ptr)) c++;
	}

	return c;
}


/*
 * Aux function -- see below
 */
static void cave_temp_room_aux(int y, int x)
{
	cave_type *c_ptr;

	/* Verify */
	if (!in_bounds(y, x)) return;

	/* Get the grid */
	c_ptr = area(y, x);

	/* Avoid infinite recursion */
	if (c_ptr->info & (CAVE_TEMP)) return;

	/* If a wall, exit */
	if (!cave_floor_grid(c_ptr)) return;

	/* Do not exceed the maximum spell range */
	if (distance(p_ptr->py, p_ptr->px, y, x) > MAX_RANGE) return;


	/* Verify this grid */
	/*
	* The reason why it is ==6 instead of >5 is that 8 is impossible
	* due to the check for cave_floor_grid above.
	* 7 lights dead-end corridors (you need to do this for the
	* checkboard interesting rooms, so that the boundary is lit
	* properly.
	* This leaves only a check for 6 bounding walls!
	*/
	if ((next_to_walls_adj(y, x) == 6) && (next_to_open(y, x) <= 1)) return;

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

	cave_type *c_ptr;

	/* Add the initial grid */
	cave_temp_room_aux(y1, x1);

	/* While grids are in the queue, add their neighbors */
	for (i = 0; i < temp_n; i++)
	{
		x = temp_x[i], y = temp_y[i];

		c_ptr = area(y, x);

		/* Walls get lit, but stop light */
		if (!cave_floor_grid(c_ptr)) continue;

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

	cave_type *c_ptr;

	/* Add the initial grid */
	cave_temp_room_aux(y1, x1);

	/* Spread, breadth first */
	for (i = 0; i < temp_n; i++)
	{
		x = temp_x[i], y = temp_y[i];

		c_ptr = area(y, x);

		/* Walls get dark, but stop darkness */
		if (!cave_floor_grid(c_ptr)) continue;

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
	u16b flg = PROJECT_GRID | PROJECT_KILL;

	/* Hack -- Message */
	if (!p_ptr->blind)
	{
		msg_print("You are surrounded by a white light.");
	}

	/* Hook into the "project()" function */
	(void)project(0, rad, p_ptr->py, p_ptr->px, dam, GF_LITE_WEAK, flg);

	/* Lite up the room */
	lite_room(p_ptr->py, p_ptr->px);

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
	if (!p_ptr->blind)
	{
		msg_print("Darkness surrounds you.");
	}

	/* Hook into the "project()" function */
	(void)project(0, rad, p_ptr->py, p_ptr->px, dam, GF_DARK_WEAK, flg);

	/* Lite up the room */
	unlite_room(p_ptr->py, p_ptr->px);

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
	if (!ironman_moria && (dir == 5) && target_okay())
	{
		flg &= ~(PROJECT_STOP);
		tx = p_ptr->target_col;
		ty = p_ptr->target_row;
	}

	/* Analyze the "dir" and the "target".  Hurt items on floor. */
	return (project(0, rad, ty, tx, dam, typ, flg));
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
	cave_type * c_ptr;
	monster_type * m_ptr;
	monster_race * r_ptr;

	if (!ironman_moria && (dir == 5) && target_okay())
	{
		tx = p_ptr->target_col;
		ty = p_ptr->target_row;
	}
	else
	{
		tx = px + ddx[dir];
		ty = py + ddy[dir];
	}

	if (!in_bounds2(ty, tx))
	{
		msg_print("You can't trade places with that!");

		/* Failure */
		return FALSE;
	}

	c_ptr = area(ty,tx);

	if (!c_ptr->m_idx)
	{
		msg_print("You can't trade places with that!");

		/* Failure */
		return FALSE;
	}

	m_ptr = &m_list[c_ptr->m_idx];
	r_ptr = &r_info[m_ptr->r_idx];

	if (r_ptr->flags3 & RF3_RES_TELE)
	{
		msg_print("Your teleportation is blocked!");

		/* Failure */
		return FALSE;
	}

	sound(SOUND_TELEPORT);

	/* Process fields under the player. */
	field_hook(&area(py, px)->fld_idx, FIELD_ACT_PLAYER_LEAVE, NULL);

	/* Process fields under the monster. */
	field_hook(&area(m_ptr->fy, m_ptr->fx)->fld_idx,
		 FIELD_ACT_MONSTER_LEAVE, m_ptr);

	/* Move monster */
	area(py, px)->m_idx = c_ptr->m_idx;

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

	tx = m_ptr->fx;
	ty = m_ptr->fy;

	if (!p_ptr->depth)
	{
		/* Scroll wilderness */
		p_ptr->wilderness_x = px;
		p_ptr->wilderness_y = py;
		move_wild();
	}

	/* Process fields under the player. */
	field_hook(&area(py, px)->fld_idx,
		FIELD_ACT_PLAYER_ENTER, NULL);

	/* Update the monster (new location) */
	update_mon(area(ty, tx)->m_idx, TRUE);

	/* Process fields under the monster. */
	field_hook(&area(m_ptr->fy, m_ptr->fx)->fld_idx,
		FIELD_ACT_MONSTER_ENTER, (vptr) m_ptr);

	/* Redraw the old grid */
	lite_spot(ty, tx);

	/* Redraw the new grid */
	lite_spot(py, px);

	/* Check for new panel (redraw map) */
	verify_panel();

	/* Update stuff */
	p_ptr->update |= (PU_VIEW | PU_FLOW);

	/* Notice changes in view */
	if (r_ptr->flags7 & (RF7_LITE_1 | RF7_LITE_2))
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
	tx = p_ptr->px + ddx[dir];
	ty = p_ptr->py + ddy[dir];

	/* Hack -- Use an actual "target" */
	if (!ironman_moria && (dir == 5) && target_okay())
	{
		tx = p_ptr->target_col;
		ty = p_ptr->target_row;
	}

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
bool lite_line(int dir)
{
	u16b flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_KILL;
	return (project_hook(GF_LITE_WEAK, dir, damroll(6, 8), flg));
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
	return (project(0, 1, p_ptr->py, p_ptr->px, 0, GF_MAKE_DOOR, flg));
}


bool trap_creation(void)
{
	u16b flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;
	return (project(0, 1, p_ptr->py, p_ptr->px, 0, GF_MAKE_TRAP, flg));
}


bool glyph_creation(void)
{
	u16b flg = PROJECT_GRID | PROJECT_ITEM;
	return (project(0, 1, p_ptr->py, p_ptr->px, 0, GF_MAKE_GLYPH, flg));
}


bool wall_stone(void)
{
	u16b flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;

	bool dummy = (project(0, 1, p_ptr->py, p_ptr->px, 0, GF_STONE_WALL, flg));

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
	return (project(0, 1, p_ptr->py, p_ptr->px, 0, GF_KILL_DOOR, flg));
}


bool sleep_monsters_touch(void)
{
	u16b flg = PROJECT_KILL | PROJECT_HIDE;
	return (project(0, 1, p_ptr->py, p_ptr->px, p_ptr->lev, GF_OLD_SLEEP, flg));
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

	Chaos_type = hurt_types[randint0(30)];
	if (one_in_(4)) line_chaos = TRUE;

	if (one_in_(6))
	{
		for (dummy = 1; dummy < 10; dummy++)
		{
			if (dummy - 5)
			{
				if (line_chaos)
					fire_beam(Chaos_type, dummy, 75);
				else
					fire_ball(Chaos_type, dummy, 75, 2);
			}
		}
	}
	else if (one_in_(3))
	{
		fire_ball(Chaos_type, 0, 300, 8);
	}
	else
	{
		if (!get_aim_dir(&dir)) return;
		if (line_chaos)
			fire_beam(Chaos_type, dir, 150);
		else
			fire_ball(Chaos_type, dir, 150, 3 + (plev / 35));
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

	int stat = 0;

	u16b flg = (PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | PROJECT_JUMP);

	do
	{
		switch (randint1(34))
		{
			case 28: case 29:
			{
				if (!(*count))
				{
					msg_print("The ground trembles...");
					earthquake(py, px, rand_range(5, 15));
					if (!one_in_(6)) break;
				}
			}
			case 30: case 31:
			{
				if (!(*count))
				{
					msg_print("A portal opens to a plane of raw mana!");
					destroy_area(py, px, 20);
					project(1, 3, py, px, damroll(10, 5), GF_MANA, flg);
					if (!one_in_(6)) break;
				}
			}
			case 32: case 33:
			{
				if (!(*count))
				{
					msg_print("Space warps about you!");
					teleport_player(damroll(10, 10));
					if (!one_in_(13)) (*count) += activate_hi_summon();
					if (!one_in_(6)) break;
				}
			}
			case 34:
			{
				msg_print("You feel a surge of energy!");
				wall_breaker();
				if (one_in_(7))
				{
					project(1, 7, py, px, 50, GF_KILL_WALL, flg);
				}
				if (!one_in_(6)) break;
			}
			case 1: case 2: case 3: case 16: case 17:
			{
				aggravate_monsters(0);
				if (!one_in_(6)) break;
			}
			case 4: case 5: case 6:
			{
				(*count) += activate_hi_summon();
				if (!one_in_(6)) break;
			}
			case 7: case 8: case 9: case 18:
			{
				(*count) += summon_specific(0, py, px, p_ptr->depth, 0, TRUE, FALSE, FALSE);
				if (!one_in_(6)) break;
			}
			case 10: case 11: case 12:
			{
				msg_print("You feel your life draining away...");
				lose_exp(p_ptr->exp / 16);
				if (!one_in_(6)) break;
			}
			case 13: case 14: case 15: case 19: case 20:
			{
				if (stop_ty || (p_ptr->free_act && (randint1(100) < p_ptr->skill_sav)))
				{
					/* Do nothing */ ;
				}
				else
				{
					msg_print("You feel like a statue!");
					if (p_ptr->free_act)
					{
						set_paralyzed(p_ptr->paralyzed + randint1(3));
					}
					else
					{
						set_paralyzed(p_ptr->paralyzed + randint1(13));
					}
					stop_ty = TRUE;
				}
				if (!one_in_(6)) break;
			}
			case 21: case 22: case 23:
			{
				(void)do_dec_stat(randint0(6));
				if (!one_in_(6)) break;
			}
			case 24:
			{
				msg_print("Huh? Who am I? What am I doing here?");
				lose_all_info();
				if (!one_in_(6)) break;
			}
			case 25:
			{
				/*
				 * Only summon Cyberdemons deep in the dungeon.
				 */
				if ((p_ptr->depth > 65) && !stop_ty)
				{
					(*count) += summon_cyber(-1, py, px);
					stop_ty = TRUE;
					break;
				}
				if (!one_in_(6)) break;
			}
			default:
			{
				while (stat < 6)
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
		switch (randint1(26) + (p_ptr->depth / 20))
		{
			case 1: case 2:
				count += summon_specific(0, py, px, p_ptr->depth, SUMMON_ANT, TRUE, FALSE, FALSE);
				break;
			case 3: case 4:
				count += summon_specific(0, py, px, p_ptr->depth, SUMMON_SPIDER, TRUE, FALSE, FALSE);
				break;
			case 5: case 6:
				count += summon_specific(0, py, px, p_ptr->depth, SUMMON_HOUND, TRUE, FALSE, FALSE);
				break;
			case 7: case 8:
				count += summon_specific(0, py, px, p_ptr->depth, SUMMON_HYDRA, TRUE, FALSE, FALSE);
				break;
			case 9: case 10:
				count += summon_specific(0, py, px, p_ptr->depth, SUMMON_ANGEL, TRUE, FALSE, FALSE);
				break;
			case 11: case 12:
				count += summon_specific(0, py, px, p_ptr->depth, SUMMON_UNDEAD, TRUE, FALSE, FALSE);
				break;
			case 13: case 14:
				count += summon_specific(0, py, px, p_ptr->depth, SUMMON_DRAGON, TRUE, FALSE, FALSE);
				break;
			case 15: case 16:
				count += summon_specific(0, py, px, p_ptr->depth, SUMMON_DEMON, TRUE, FALSE, FALSE);
				break;
			case 17:
				count += summon_specific(0, py, px, p_ptr->depth, SUMMON_AMBERITES, TRUE, FALSE, FALSE);
				break;
			case 18: case 19:
				count += summon_specific(0, py, px, p_ptr->depth, SUMMON_UNIQUE, TRUE, FALSE, FALSE);
				break;
			case 20: case 21:
				count += summon_specific(0, py, px, p_ptr->depth, SUMMON_HI_UNDEAD, TRUE, FALSE, FALSE);
				break;
			case 22: case 23:
				count += summon_specific(0, py, px, p_ptr->depth, SUMMON_HI_DRAGON, TRUE, FALSE, FALSE);
				break;
			case 24: case 25:
				count += summon_specific(0, py, px, 100, SUMMON_CYBER, TRUE, FALSE, FALSE);
				break;
			default:
				count += summon_specific(0, py, px, (((p_ptr->depth * 3) / 2) + 5), 0, TRUE, FALSE, FALSE);
		}
	}

	return count;
}


/* ToDo: check */
int summon_cyber(int who, int y, int x)
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
		count += summon_specific(who, y, x, 100, SUMMON_CYBER, FALSE, friendly, pet);
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
			scatter(&y, &x, py, px, 4);

			if ((y != py) || (x != px)) break;
		}

		project(0, 0, y, x, rand_range(20, 50), GF_KILL_WALL,
				  (PROJECT_BEAM | PROJECT_THRU | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL));
	}
	else if (randint1(100) > 30)
	{
		earthquake(py, px, 1);
	}
	else
	{
		int num = damroll(5, 3);

		for (i = 0; i < num; i++)
		{
			while (1)
			{
				scatter(&y, &x, py, px, 4);

				if ((y != py) || (x != px)) break;
			}

			project(0, 0, y, x, rand_range(20, 50), GF_KILL_WALL,
					  (PROJECT_BEAM | PROJECT_THRU | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL));
		}
	}
}


/*
 * Detect all "nonliving", "undead" or "demonic" monsters in range
 */
bool detect_monsters_nonliving(void)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

	int     i, y, x;
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

		/* Only detect monsters in range */
		if (distance(px, py, x, y) > MAX_DETECT) continue;

		/* Detect non-living monsters */
		if (!monster_living(r_ptr))
		{
			/* Update monster recall window */
			if (p_ptr->monster_race_idx == m_ptr->r_idx)
			{
				/* Window stuff */
				p_ptr->window |= (PW_MONSTER);
			}

			/* Repair visibility later */
			repair_monsters = TRUE;

			/* Hack -- Detect monster */
			m_ptr->mflag |= (MFLAG_MARK | MFLAG_SHOW);

			/* Update the monster */
			update_mon(i, FALSE);

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
