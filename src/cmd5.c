/* File: cmd5.c */

/*
 * Spell browsing, learning, and casting.  Effects of all spells.
 *
 * Copyright (c) 2007
 * Leon Marrick, Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation, version 2.  Parts may also be available under the
 * terms of the Moria license.  For more details, see "/docs/copying.txt".
 */

#include "angband.h"


/*
 * Local variable:  mana cost of spell
 */
static int mana_cost = 0;


/*
 * Get the "name" of spells (ie, "spell", "prayer", or "technique")
 */
cptr spell_type(void)
{
	switch (p_ptr->realm)
	{
		case PRIEST: return ("prayer");
		case DRUID:  return ("technique");
		case NECRO:  return ("ritual");
		default:     return ("spell");
	}
}



/*
 * Get a color depending on character realm and degree of specialization.
 *
 * Priests and necromancers use different colors in 128-color mode.
 */
byte realm_color(void)
{
	byte attr = TERM_WHITE;

	switch (p_ptr->realm)
	{
		case MAGE:
		{
			if (p_ptr->oath & (OATH_OF_SORCERY)) attr = TERM_RED;
			else attr = TERM_L_RED;
			break;
		}
		case PRIEST:
		{
			if (p_ptr->oath & (COVENANT_OF_FAITH)) attr = TERM_BLUE;
			else if (max_system_colors > 16) attr = TERM_DEEP_L_BLUE;
			else attr = TERM_L_BLUE;
			break;
		}
		case DRUID:
		{
			if (p_ptr->oath & (YAVANNAS_FELLOWSHIP)) attr = TERM_GREEN;
			else attr = TERM_L_GREEN;
			break;
		}
		case NECRO:
		{
			if (p_ptr->oath & (BLACK_MYSTERY)) attr = TERM_PURPLE;
			else if (max_system_colors > 16) attr = TERM_L_PURPLE;
			else attr = TERM_SLATE;
			break;
		}
		case NONE:
		{
			if (p_ptr->oath & (OATH_OF_IRON)) attr = TERM_UMBER;
			else if (p_ptr->oath & (BURGLARS_GUILD)) attr = TERM_SLATE;
			else attr = TERM_L_UMBER;
			break;
		}
	}

	/* Return */
	return (attr);
}



/*
 * Fires an arc of the chosen element.  Return TRUE if character cast the spell.
 */
static bool chromatic_burst(int dam, int dir)
{
	int typ = GF_FIRE;
	char ch;

	/* Ask until done */
	while (TRUE)
	{
		/* Ask */
		if (!get_com("Invoke which element  (F=Fire, C=Frost, E=Lightning, A=Acid) ?", &ch)) return (FALSE);

		/* Make request lowercase */
		ch = tolower(ch);

		/* Get the projection type */
		if      (ch == 'f') typ = GF_FIRE;
		else if (ch == 'c') typ = GF_COLD;
		else if (ch == 'e') typ = GF_ELEC;
		else if (ch == 'a') typ = GF_ACID;
		else
		{
			bell("Unknown element.");
			continue;
		}

		if (typ == GF_FIRE && p_ptr->aura_fire) dam = dam * 3 /2;
		if (typ == GF_COLD && p_ptr->aura_cold) dam = dam * 3 /2;

		/* Fire the arc */
		fire_arc(typ, dir, dam, 6, rand_range(50, 70));

		/* Done */
		break;
	}

	/* Done */
	return (TRUE);

}

/*
 * Reveals mimics in line of sight, detects all nearby monsters,
 * grants temporary see invisible.
 */
static void eyes_undeceivable(int dur)
{
	int y, x, i;

	/* Temporary see invisible */
	set_detect_inv(p_ptr->detect_inv + dur);

	/* Scan monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;

		/* Only detect monsters in view */
		if (!player_has_los_bold(y,x)) continue;

		/* Monster is a mimic */
		if (m_ptr->mflag & (MFLAG_MIME))
		{
			/* Reveal mimic */
			m_ptr->mflag &= ~(MFLAG_MIME);

			/* Special alert message */
			msg_print("You see a mimic!");
		}
	}

	/* Detect all monsters nearby (reveals mimics, but without messages) */
	detect_all_monsters(FALSE, TRUE);
}


/*
 * Wonder spell.  Gives Wizards access to some cool magics at
 * fairly low level.  A mage prepared to roll the dice can deal
 * serious damage.
 */
static void wonder(int dir)
{
	int typ, dam;
	cptr typ_desc = "";

	int skill = get_skill(S_WIZARDRY, 0, 100);

	/* Power and safety increase with Wizardry skill */
	int die = rand_range(skill / 15, 20 + 3 * skill / 10);

	/* Miscellaneous magics */
	if      (die <= 3) clone_monster(dir);
	else if (die <= 4) heal_monster(dir, 100);
	else if (die <= 5) speed_monster(dir);
	else if (die <= 7) poly_monster(dir, 70 + 3 * skill / 4);
	else if (die <= 9) confuse_monster(dir, 25 + 2 * skill / 3);

	/* Chance for really weird magic */
	else if (one_in_(15))
	{
		msg_print("The spell alters the dungeon around you!");
		if (one_in_(2)) fire_star(GF_FORCE_DOOR,   0, 20,    8);
		else            fire_star(GF_DISINTEGRATE, 0, skill, 6);
	}

	/* Attack spells */
	else
	{
		int choice = randint(6);

		if (choice == 1)
		{
			typ = GF_CONFUSION;
			typ_desc = "confusion";
		}
		else if (choice == 2)
		{
			typ = GF_GRAVITY;
			typ_desc = "gravity";
		}
		else if (choice == 3)
		{
			typ = GF_NEXUS;
			typ_desc = "nexus";
		}
		else if (choice == 4)
		{
			typ = GF_LITE;
			typ_desc = "light";
		}
		else if (choice == 5)
		{
			typ = GF_DARK;
			typ_desc = "darkness";
		}
		else
		{
			typ = GF_MANA;
			typ_desc = "raw magic";
		}


		/* Damage increases rapidly with skill (max of 225) */
		dam = skill + (skill * skill / 80);

		/* But you need to take the Oath of Sorcery for full effect */
		if (!(p_ptr->oath & (OATH_OF_SORCERY))) dam = 2 * dam / 3;

		/* Fire a bolt */
		if (die < 20)
		{
			if (strlen(typ_desc))
				msg_format("You cast a bolt of %s.", typ_desc);
			fire_bolt(typ, dir, dam);
		}

		/* Fire a beam */
		else if (die < 24)
		{
			if (strlen(typ_desc))
				msg_format("You cast a beam of %s.", typ_desc);
			fire_beam(typ, dir, dam);
		}

		/* Fire a ball */
		else if (rand_range(36, 49) >= die)
		{
			if (strlen(typ_desc))
				msg_format("You cast a ball of %s.", typ_desc);
			fire_ball(typ, dir, dam, 2);
		}

		/* Fire a great star */
		else
		{
			if (strlen(typ_desc))
				msg_format("You invoke an explosion of %s.", typ_desc);
			fire_star(typ, dir, dam, 5);
		}
	}
}

/*
 * Invoke spirits necro spell.  Originally from Zangband.
 */
static void invoke_spirits(int reliability)
{
	int i;
	int dir = 5;

	int py = p_ptr->py;
	int px = p_ptr->px;
	TARGET_DECLARE

	int typ;
	cptr typ_desc;


	/* Roll a die.  Ranges from 0 - 40 to 8 - 80 */
	int die = rand_range(reliability / 10, 40 + reliability / 2);

	/* Save old target */
	TARGET_PRESERVE

	msg_print("You call on the power of the dead...");


	/* The spirits have decided to be unhelpful */
	if (die <= 10)
	{
		/* Kill the light */
		if (one_in_(3)) unlite_area(0, 0);

		/* Summon 1-4 undead (scattered) */
		if (die == 0)
		{
			msg_print("Oh no! Mouldering forms rise from the earth around you!");
			(void)summon_specific(py, px, TRUE, MAX(5, p_ptr->max_depth),
				SUMMON_UNDEAD, randint(4));
		}

		/* Suck away a little exp */
		else if (die < 2)
		{
			/* Hold life protects from exp loss */
			if (p_ptr->hold_life)
			{
				msg_print("Claws tear at your lifeforce, but you resist!");
			}
			else
			{
				msg_print("Claws tear at your lifeforce!");
				lose_exp(calc_spent_exp() / 100, FALSE);
			}
		}

		/* Usual case -- blast the character (but don't kill him - normally) */
		else if ((die < 7) && (p_ptr->chp > (10 + p_ptr->depth * 2)))
		{
			/* Damage depends on character depth, randomized slightly */
			int power = (5 + p_ptr->depth);
			power = rand_spread(power, power / 4);

			/* Get damage type */
			if (power < 40)
			{
				typ_desc = "darkness";
				typ = GF_DARK;
			}
			else if (power < 80)
			{
				typ_desc = "poison";
				typ = GF_POIS;
			}
			else if (power < 100)
			{
				typ_desc = "nether";
				typ = GF_NETHER;
			}
			else
			{
				typ_desc = "Morgul-darkness";
				typ = GF_MORGUL_DARK;
			}

			/* Cast a ball at the character */
			msg_format("The spirits cast %s at you!", typ_desc);
			(void)explosion(0, 1, p_ptr->py, p_ptr->px, p_ptr->power * 2, typ);
		}
		else if (die == 10)
		{
			fire_ball(GF_MAKE_TRAP, 0, 0, 1);
			if (!one_in_(3)) msg_print("You hear cackling.");
		}

		else if (one_in_(2))
		{
			msg_print("Your head is invaded by a horde of gibbering spectral voices...");
			if (!p_ptr->resist_confu)
				set_confused(p_ptr->confused + rand_range(3, 6));
			else msg_print("But you ignore them!");
		}
		else
		{
			msg_print("An unnamable evil brushes against your mind...");
			if (!p_ptr->resist_fear)
				set_afraid(p_ptr->afraid + rand_range(15, 30));
			else msg_print("But you resist!");
		}

		/* Warning */
		if (die < 2)
		{
			msg_print("Sepulchral voices chuckle. 'Soon you will join us, mortal.'");
		}
	}

	/* The spirits have decided to be helpful */
	else
	{
		int best_dist = MAX_SIGHT+1;

		/* Cool! */
		if (die >= 60) msg_print("You feel a surge of eldritch force!");

		/* Find and target a monster */
		for (i = 1; i < m_max; i++)
		{
			monster_type *m_ptr = &m_list[i];

			/* Paranoia -- Skip dead monsters */
			if (!m_ptr->r_idx) continue;

			/* Ignore monsters out of LOF */
			if (!(cave_info[m_ptr->fy][m_ptr->fx] & (CAVE_FIRE)))
				continue;

			/* Accept monster if closer */
			if (m_ptr->cdis < best_dist)
			{
				p_ptr->target_set = TRUE;
				p_ptr->target_who = i;
				p_ptr->target_row = m_ptr->fy;
				p_ptr->target_col = m_ptr->fx;

				best_dist = m_ptr->cdis;
			}
		}

		/* There is a monster to wreck destruction on */
		if (best_dist <= MAX_SIGHT)
		{
			int projection, dam, rad;
			int choice = rand_int(100);

			/* Determine projection shape */
			if      (choice < 25) projection = 1;    /* Bolt */
			else if (choice < 50) projection = 2;    /* Short beam */
			else if (choice < 75) projection = 3;    /* Arc */
			else                  projection = 4;    /* Ball */

			/* Determine spell type */
			i = rand_int(20 + die);

			if (i < 12)
			{
				typ_desc = "darkness";
				typ = GF_DARK;
			}
			else if (i < 24)
			{
				typ_desc = "numbing cold";
				typ = GF_COLD;
			}
			else if (i < 36)
			{
				typ_desc = "foul curses";
				typ = GF_CURSE;
			}
			else if (i < 48)
			{
				typ_desc = "venom";
				typ = GF_POIS;
			}
			else if (i < 60)
			{
				typ_desc = "death";
				typ = GF_DEATH;
			}
			else if (i < 80)
			{
				typ_desc = "necromantic magics";
				typ = GF_NETHER;
			}
			else
			{
				typ_desc = "raw mana";
				typ = GF_MANA;
			}

			/* Determine damage - can average as high as 175 */
			dam = 15 + rand_range(3 * reliability / 2, 5 * reliability / 2);

			/* Determine radius */
			if (projection == 2) rad = 5 + reliability / 15;
			if (projection == 3) rad = 3 + reliability / 15;
			else                 rad = 1 + reliability / 30;

			/* Message */
			if ((projection == 1) || (projection == 2))
			{
				msg_format("The spirits hurl %s%c",
					typ_desc, (die >= 60 ? '!' : '.'));
			}
			else
			{
				msg_format("The spirits call down %s%c",
					typ_desc, (die >= 60 ? '!' : '.'));
			}

			/* Cast the spell */
			if (projection == 1) fire_bolt(typ, dir, dam);
			if (projection == 2) fire_arc(typ, dir, dam, rad, 0);
			if (projection == 3) fire_arc(typ, dir, dam, rad, 45);
			if (projection == 4) fire_ball(typ, dir, dam, rad);
		}

		/* No monster found */
		else
		{
			if (die < 15)
			{
				/* Blow up doors */
				msg_print("The spirits are looking for doors to destroy...");
				fire_star(GF_KILL_DOOR, 0, 0, 6);
			}
			else if (die < 30)
			{
				/* Disarm traps */
				msg_print("The spirits are looking for traps to destroy...");
				fire_star(GF_KILL_TRAP, 0, 0, 6);
			}
			else if (die < 45)
			{
				/* Detect undead -- extended range */
				(void)detect_undead(TRUE, TRUE);
			}
			else if (die < 60)
			{
				/* Detect everything -- normal range */
				(void)detect_all(FALSE, TRUE);
			}
			else if (one_in_(2))
			{
				/* Heal character */
				msg_print("The spirits feel benevolent.");
				(void)explosion(0, 0, p_ptr->py, p_ptr->px, rand_range(150, 300),
				                GF_DO_HEAL);
			}
			else
			{
				/* Genocide monsters */
				msg_print("The spirits call down annihilation...");
				(void)genocide(0);
			}
		}
	}

	/* Restore old target */
	TARGET_RESTORE
}


/*
 * Turn curses on equipped items into starbursts of nether.  -LM-
 *
 * Idea by Mikko Lehtinen.
 */
static bool thaumacurse(bool verbose, int power)
{
	int i, dam;
	int curse_count = 0;
	object_type *o_ptr;
	bool notice = FALSE;


	/* Count curses */
	for (i = INVEN_WIELD; i < INVEN_SUBTOTAL; i++)
	{
		o_ptr = &inventory[i];

		/* All cursed objects have TR3_LIGHT_CURSE.  Perma-cursed objects count double. */
		if (o_ptr->flags3 & (TR3_LIGHT_CURSE | TR3_HEAVY_CURSE)) curse_count++;
		if (o_ptr->flags3 & (TR3_PERMA_CURSE)) curse_count++;
	}

	/* There are no curses to use */
	if (curse_count == 0)
	{
		if (verbose) msg_print("Your magic fails to find a focus ... and dissipates harmlessly.");
		return (FALSE);
	}


	/* Calculate damage for 1 cursed item (between about 61 and 139) (10x inflation) */
	dam = power * 6;

	/*
	 * Increase damage for each curse found (10x inflation).
	 * Diminishing effect for each, but damage can potentially be very high indeed.
	 */
	dam *= rsqrt(curse_count * 100);

	/* Deflate */
	dam = div_round(dam, 100);

	/* Fire an explosion of nether */
	notice = fire_star(GF_NETHER, 0, dam, 3 + curse_count / 2);


	/* Break light curses */
	for (i = INVEN_WIELD; i < INVEN_SUBTOTAL; i++)
	{
		o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Object isn't cursed -- ignore */
		if (!cursed_p(o_ptr)) continue;

		/* Ignore heavy and permanent curses */
		if (o_ptr->flags3 & (TR3_HEAVY_CURSE | TR3_PERMA_CURSE)) continue;

		/* Uncurse the object 1 time in 3 */
		if (one_in_(3)) uncurse_object(o_ptr);

		/* Hack -- Assume felt */
		o_ptr->ident |= (IDENT_SENSE);

		/* Recalculate the bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Window stuff */
		p_ptr->window |= (PW_EQUIP);

		/* Notice */
		notice = TRUE;
	}

	/* Return "something was noticed" */
	return (notice);
}


/*
 * Wild magic effects for sorcery spells.  Idea from Zangband.
 *
 * Make sure that values for "spell" do not exceed the number of spells
 * sorcerers get.
 */
static void wild_magic_sorcery(int spell)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int i, y, x;
	cptr typ_desc;

	/* Greatest possible failure */
	int oops_max = mp_ptr->spell_number + 10;

	/* Size of this failure (from 1 to 100) */
	int oops = randint((spell + 10) * 100 / oops_max);


	/* Light and darken room */
	if ((oops <= 10) || (spell < 5))
	{
		if (cave_info[py][px] & (CAVE_GLOW)) unlite_area(0, 4);
		else lite_area(0, 3);
	}

	/* Teleport character (unsafely!) */
	else if ((oops < 15) || (one_in_(4)))
	{
		msg_print("The wild magic hurls you around!");
		teleport_player(oops, FALSE, FALSE);
	}

	/* Dungeon-alteration */
	else if ((oops < 20) || (one_in_(6)))
	{
		/* Fall of rubble */
		if (oops < 35)
		{
			bool flag = FALSE;

			for (i = 0; i < 5; i++)
			{
				scatter(&y, &x, py, px, 3, 0);

				if (cave_floor_bold(y, x))
				{
					cave_set_feat(y, x, FEAT_RUBBLE);
					flag = TRUE;
				}
			}

			/* Fully update the visuals */
			p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);

			/* Redraw map */
			p_ptr->redraw |= (PR_MAP);

			/* Window stuff */
			p_ptr->window |= (PW_OVERHEAD);

			if (flag) msg_print("The ground is torn up!");
		}

		/* Earthquake */
		else if ((oops < 60) || (one_in_(2)))
		{
			msg_print("An earthquake hits!");
			earthquake(py, px, (oops / 3 - 8));
		}

		/* Collapse ceiling */
		else if ((oops < 80) || (one_in_(2)))
		{
			msg_print("The ceiling caves in on you!");
			(void)collapse_ceiling(py, px, (oops * 2 - 80));
		}

		/* Destroy nearby dungeon (one epicenter) */
		else if (oops != 100)
		{
			msg_print("The magic goes berserk!");
			destroy_area(py, px, (oops / 2 - 25), TRUE, FALSE);
		}

		/* Totally demolish the dungeon, then build a new one! */
		else
		{
			msg_print("The entire dungeon is being destroyed!");
			for (i = 0; i < 8; i++)
			{
				destroy_area(rand_range(10, dungeon_hgt - 10),
					rand_range(10, dungeon_wid - 10), 30, TRUE, FALSE);

				pause_for(100);
			}

			/* Oh, no */
			msg_print("The magic cannot be contained!  You are hurled out of the dungeon!");

			/* Stun/confuse */
			set_stun(p_ptr->stun + 50);
			set_confused(p_ptr->stun + 200);

			/* All over, man */
			p_ptr->leaving = TRUE;
		}
	}

	/* Vortices are about the only thing that Wizard magic summons */
	else if ((oops >= 20) && (one_in_(12)))
	{
		msg_print("Vortices materialize around you!");

		/* This can be very dangerous... */
		summon_specific(py, px, TRUE, oops, SUMMON_VORTEX, oops / 20);
	}

	/* Random magic (most of the time) */
	else
	{
		int typ, dam;
		int old_hp;

		/* Determine spell type */
		i = randint(20 + oops);

		if (i < 10)
		{
			typ_desc = "";
			typ = GF_LITE_WEAK;
		}
		else if (i < 20)
		{
			typ_desc = "frost";
			typ = GF_COLD;
		}
		else if (i < 35)
		{
			typ_desc = "fire";
			typ = GF_FIRE;
		}
		else if (i < 40)
		{
			typ_desc = "acid";
			typ = GF_ACID;
		}
		else if (i < 45)
		{
			typ_desc = "electricity";
			typ = GF_ELEC;
		}
		else if (i < 50)
		{
			typ_desc = "poison";
			typ = GF_POIS;
		}
		else if (i < 60)
		{
			typ_desc = "mana";
			typ = GF_MANA;
		}
		else if (i < 70)
		{
			typ_desc = "perplexity";
			typ = GF_CONFUSION;
		}
		else if (i < 81)
		{
			typ_desc = "light";
			typ = GF_LITE;
		}
		else if (i < 92)
		{
			typ_desc = "nexus";
			typ = GF_NEXUS;
		}
		else
		{
			typ_desc = "chaos";
			typ = GF_CHAOS;
		}

		/* Determine damage */
		dam = oops;

		/* Remember player HPs */
		old_hp = p_ptr->chp;

		/* Message */
		if (strlen(typ_desc) > 1)
			msg_format("The wild magic creates explosions of %s!", typ_desc);
		else if (typ == GF_LITE_WEAK)
			msg_print("Sparkles of light appear around you.");

		/* Fire some meteors */
		for (i = 0; i < 1 + oops / 30; i++)
		{
			scatter(&y, &x, py, px, 6, 0);

			/* Fire a meteor, let the player get hurt */
			(void)explosion(0, 1, y, x, dam, typ);

			/* Don't get too nasty (enclosed spaces are rough!). */
			if ((p_ptr->chp < p_ptr->mhp / 3) ||
				 ((p_ptr->chp < old_hp) && (one_in_(2))))
			{
				break;
			}
		}
	}
}



/*
 * Perilous side-effects for necromantic spells.  From Zangband.
 *
 * Make sure that values for "spell" do not exceed the number of spells
 * necromancers get.
 */
static void perilous_effect_necro(int spell, bool necronomicon)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int i, y, x;

	/* Greatest possible failure */
	int oops_max = mp_ptr->spell_number + 10;

	/* Size of this failure (from 1 to 100) */
	int oops = randint((spell + 10) * 100 / oops_max);


	/* Darken room */
	if (oops < 15)
	{
		if (cave_info[py][px] & (CAVE_GLOW)) unlite_area(0, 5);
	}

	else if (!one_in_(3))
	{
		(void)take_hit(1 + oops / 2, 0, "It hurts!", "a miscast Death spell");
	}

	/* The undead not infrequently appear */
	if ((oops >= 20) && (one_in_(8)))
	{
		msg_print("The undead appear and call your name!");

		/* This can be very dangerous... */
		summon_specific(py, px, TRUE, oops, SUMMON_UNDEAD, oops / 33);

		/* That's enough for one mistake */
		return;
	}

	/* Random magic (sometimes) */
	if (!one_in_(3))
	{
		int typ, dam;
		int old_hp;

		msg_print("Foul curses rain down upon you!");

		/* Determine spell type */
		i = randint(20 + oops);

		if      (i <  40) typ = GF_DARK;
		else if (i <  75) typ = GF_POIS;
		else if (i < 110) typ = GF_NETHER;
		else              typ = GF_MORGUL_DARK;

		/* Determine damage */
		dam = oops;

		/* Remember player HPs */
		old_hp = p_ptr->chp;

		/* Fire some meteors */
		for (i = 0; i < 1 + oops / 30; i++)
		{
			scatter(&y, &x, py, px, 7, 0);

			/* Fire a meteor, let the player get hurt */
			(void)explosion(0, 1, y, x, dam, typ);

			/* Don't get too nasty (enclosed spaces are rough!). */
			if ((p_ptr->chp < p_ptr->mhp / 3) ||
				 ((p_ptr->chp < old_hp) && (one_in_(2))))
			{
				break;
			}
		}
	}

	/* The Necronomicon is particularly dangerous */
	if ((necronomicon) && (!check_save(100)))
	{
		int choice = randint(6);

		/* Mind blast */
		if (choice <= 3)
		{
			if (!p_ptr->resist_confu)
			{
				(void)set_confused(p_ptr->confused + rand_range(5, 10));
			}
			if ((!p_ptr->free_act) && (!check_save(100)))
			{
				(void)set_paralyzed(p_ptr->paralyzed + rand_range(2, 3));
			}
			if ((!p_ptr->resist_chaos) && (one_in_(3)))
			{
				(void)set_image(p_ptr->image + rand_range(15, 20));
			}

			/* Message */
			if ((p_ptr->confused) || (p_ptr->paralyzed) || (p_ptr->image))
			{
				msg_print("Your mind is blasted by reading the Necronomicon!");
			}
		}

		/* Lose int */
		else if (choice >= 4)
		{
			do_dec_stat(A_INT, rand_range(2, 4), FALSE, "Your mind is shattered by reading the Necronomicon!", NULL);
		}

		/* Amnesia */
		else
		{
			if (!p_ptr->resist_fear)
			{
				(void)lose_all_info("You forget everything in your utmost terror!");
			}
			else if (one_in_(3))
			{
				(void)lose_all_info("Your mind goes blank!");
			}
		}
	}
}


/*
 * Calculates the effect of weather on Druidic spell damages.
 *
 * Fire:  Up to dam * 1.5 in hot weather, as little as no damage when cold.
 * Cold:  Up to dam * 1.5 in cold weather, as little as no damage when hot.
 * Acid:  Up to dam * 1.5 when wet, as little as no damage when dry.
 * Elec:  Up to dam * 1.5 when dry, as little as no damage when wet.
 * Pois:  Up to dam * 1.5 when still, as little as no damage when windy.
 * Wind:  Up to dam * 1.5 when windy, as little as no damage when still.
 */
static s16b weather_dam(int typ, int base_dam)
{
	int dam = 0;
	s16b humid, wind, temp;

	/* Values for the three components of weather */
	humid = p_ptr->humid;
	wind = p_ptr->wind;
	temp = p_ptr->temp;

	/* Paranoia. */
	if (base_dam > 1000) base_dam = 1000;

	/* Find type of projection, calculate effects of weather. */
	switch (typ)
	{
		case GF_FIRE:
		{
			dam = div_round(base_dam * (temp + MAX_WEATHER), MAX_WEATHER);
			break;
		}
		case GF_COLD:
		{
			dam = div_round(base_dam * (MAX_WEATHER - temp), MAX_WEATHER);
			break;
		}
		case GF_ELEC:
		{
			dam = div_round(base_dam * (MAX_WEATHER - humid), MAX_WEATHER);
			break;
		}
		case GF_ACID:
		{
			dam = div_round(base_dam * (humid + MAX_WEATHER), MAX_WEATHER);
			break;
		}
		case GF_POIS:
		{
			dam = div_round(base_dam * (MAX_WEATHER - wind), MAX_WEATHER);
			break;
		}
		case GF_WIND:
		{
			dam = div_round(base_dam * (wind + MAX_WEATHER), MAX_WEATHER);
			break;
		}
		default:
		{
			dam = base_dam;
			break;
		}
	}

	/* Hack - Halve benefits of favorable weather. */
	if (dam > base_dam)
	{
		dam -= (dam - base_dam) / 2;
	}

	/* Return modified damage. */
	return (dam);
}


/*
 * Hack -- The Athelas-creation code.
 */
static void create_athelas(void)
{
	object_type *i_ptr;
	object_type object_type_body;

	/* Get local object */
	i_ptr = &object_type_body;

	/* Make some Athelas */
	object_prep(i_ptr, lookup_kind(TV_FOOD, SV_FOOD_ATHELAS));

	/* Identify */
	object_aware(i_ptr);
	object_known(i_ptr);

	/* Prevent money-making. */
	i_ptr->cost_adjust = 20;

	/* Give it to the character */
	give_object(i_ptr, FALSE);
}



/*
 * Some spells expand to fill the available open space, and so need
 * more mana to cover large areas.
 *
 * Calculate mana expenditure based on space the spell must fill.
 */
static void calc_mana_aux(int min, int max, int rad)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x;
	int grids, max_grids;

	u32b factor, max_factor;


	/* Very small radii require special cases */
	if (rad <= 2) max_grids = grids_in_radius[rad];

	/* A single rule can be applied to most radii */
	else
	{
		/* W/o axis grids, they have about 2/3rds as many as a square */
		max_grids = 2 * (rad * 2) * (rad * 2) / 3;

		/* Add in axis grids */
		max_grids += rad * 4 + 1;
	}

	/* Scan all the grids within the radius */
	for (grids = 0, y = py - rad; y <= py + rad; y++)
	{
		for (x = px - rad; x <= px + rad; x++)
		{
			/* Must be within the outer walls */
			if (!in_bounds_fully(y, x)) continue;

			/* Require that grid be viewable XXX XXX */
			if (!(cave_info[y][x] & (CAVE_VIEW))) continue;

			/* Check distance */
			if (distance(py, px, y, x) > rad) continue;

			/* If this grid is passable, count it */
			if (cave_passable_bold(y, x)) grids++;
		}
	}

	/* If count is less than 2, use the minimum mana */
	if (grids < 2)
	{
		mana_cost = min;
	}
	else
	{
		/* The larger the number of grids, the less rapidly mana increases */
		factor = (u32b)(rsqrt(grids));
		max_factor = (u32b)(rsqrt(max_grids));

		/* Calculate the amount of mana required */
		mana_cost = min + ((max - min) * factor / max_factor);
	}
}


/*
 * Returns chance of failure for a spell
 *
 * There are two sorts of penalties
 */
static s16b spell_chance(int spell)
{
	int fail, skill;

	const magic_type *s_ptr;

	/* Paranoia -- must be literate */
	if (p_ptr->realm == NONE) return (100);

	/* Get the spell */
	s_ptr = &mp_ptr->info[spell];

	/* Extract the base spell failure rate */
	fail = s_ptr->sfail;

	/* Get effective level */
	skill = get_skill(mp_ptr->spell_skill, 0, 100);

	/* If wielding an unsanctified weapon, skill is reduced */
	if (p_ptr->icky_wield) skill = 2 * skill / 3;

	/* If wearing cumbersome gloves, skill is reduced */
	if (p_ptr->cumber_glove) skill = 2 * skill / 3;

	/* Reduce failure rate by "effective" level adjustment */
	fail -= (skill - s_ptr->slevel);

	/* Adjust failure rate according to spell stat */
	fail += ((int)(adj_mag_fail[p_ptr->stat_ind[mp_ptr->spell_stat]]) - 128);

	/* Get mana cost of spell */
	(void)do_spell(SPELL_MANA, spell);

	/* Not enough mana to cast */
	if (mana_cost > p_ptr->csp) fail += 5 * (mana_cost - p_ptr->csp);

	/* Stunning makes spells harder */
	if (p_ptr->stun >= HVY_STUN) fail += 25;
	else if (p_ptr->stun)        fail += 10;

	/* Fear makes spells a little harder */
	if (p_ptr->afraid) fail += 10;

	/* Darkness makes if more difficult to cast */
	if (no_light()) fail += 10;

	/* Always a 5 percent chance of working */
	if (fail > 95) fail = 95;
	if (fail <  0) fail =  0;

	/* Return the failure percentage */
	return (fail);
}

/*
 * Determine if we can cast or study, output messages.
 */
static bool can_study_or_cast(void)
{
	cptr note = "";

	/* Get failure note */
	if (p_ptr->realm == NONE) note = "You know no magical realm.";
	else if (p_ptr->berserk)  note = "You are too berserk!";
	else if (p_ptr->blind)    note = "You are blind!";
	else if (p_ptr->confused) note = "You are too confused!";
	else if (p_ptr->image)    note = "You are hallucinating!";

	/* Handle failure */
	if (strlen(note) >= 1)
	{
		if (flush_failure) flush();
		msg_format("%s", note);
		return (FALSE);
	}

	return (TRUE);
}


/*
 * Determine if a spell is legal for the character to cast or browse.
 * The character must have high enough skill (and spell stat) for the spell.
 */
static bool spell_okay(int spell)
{
	/* Get the spell */
	const magic_type *s_ptr = &mp_ptr->info[spell];

	/* Accept if not too high level */
	return (s_ptr->slevel <= p_ptr->spell_level);
}


/*
 * Print out a list of available spells for any spellbook given.
 *
 * Input y controls lines from top for list, and input x controls columns
 * from left.
 */
void print_spells(int tval, int sval, int y, int x)
{
	int i;
	int j = 0;
	int first_spell, after_last_spell;

	const magic_type *s_ptr;

	byte attr_book, attr_name, attr_extra;

	char comment[DESC_LEN];
	char out_val[DESC_LEN];

	object_kind *k_ptr = &k_info[lookup_kind(tval, sval)];
	cptr basenm = (k_name + k_ptr->name);


	/* Currently, must be a legal spellbook of the correct realm. */
	if ((tval != mp_ptr->spell_book) || (sval > SV_BOOK_MAX))
	{
		return;
	}


	/* Choose appropriate spellbook color. */
	if (tval == TV_MAGIC_BOOK)
	{
		if (sval < SV_BOOK_MIN_GOOD) attr_book = TERM_L_RED;
		else attr_book = TERM_RED;
	}
	else if (tval == TV_PRAYER_BOOK)
	{
		if (sval < SV_BOOK_MIN_GOOD) attr_book = TERM_L_BLUE;
		else attr_book = TERM_BLUE;
	}
	else if (tval == TV_NATURE_BOOK)
	{
		if (sval < SV_BOOK_MIN_GOOD) attr_book = TERM_L_GREEN;
		else attr_book = TERM_GREEN;
	}
	else if (tval == TV_DARK_BOOK)
	{
		attr_book = TERM_PURPLE;
	}
	else attr_book = TERM_WHITE;


	/* Find the array index of the spellbook's first spell. */
	first_spell = mp_ptr->book_start_index[sval];

	/* Find the first spell in the next book. */
	after_last_spell = mp_ptr->book_start_index[sval+1];


	/* Center the spellbook name */
	center_string(out_val, sizeof(out_val), basenm, 65);

	/* Print it out */
	c_put_str(attr_book, format("%s", out_val), y, x);


	/* Title the list */
	clear_space(y + 1, x, 65);
	put_str("Name", y + 1, x + 5);
	put_str("Lv Mana Fail Info", y + 1, x + 35);

	/* Dump the spells in the book. */
	for (i = first_spell; i < after_last_spell; i++)
	{
		/* Access the spell */
		s_ptr = &mp_ptr->info[i];

		/* Increment the current line */
		j++;

		/* Skip illegible spells.  This should actually never appear. */
		if (s_ptr->slevel > PY_MAX_POWER)
		{
			(void)strnfmt(out_val, sizeof(out_val), "  %c) %-30s", I2A(i - first_spell),
				"(illegible)");
			c_prt(TERM_SLATE, out_val, y + j + 1, x);
			continue;
		}

		/* Analyze the spell */
		if (p_ptr->spell_flags[i] & (PY_SPELL_FORGOTTEN))
		{
			strcpy(comment, "forgotten");
			attr_name  = TERM_L_WHITE;
			attr_extra = TERM_L_WHITE;
		}
		else if (s_ptr->slevel > p_ptr->spell_level)
		{
			strcpy(comment, "unknown");

			attr_name  = TERM_L_WHITE;
			attr_extra = TERM_L_WHITE;
		}
		else if (!(p_ptr->spell_flags[i] & (PY_SPELL_WORKED)))
		{
			strcpy(comment, "untried");
			attr_name  = TERM_WHITE;
			attr_extra = TERM_WHITE;
		}
		else
		{
			/* Get extra spell info */
			strcpy(comment, do_spell(SPELL_INFO, i));

			/* Vivid color for known, cast spells */
			attr_name = attr_book;
			attr_extra = TERM_L_BLUE;
		}

		/* Spell index */
		put_str(format(" %c)  ", I2A(i - first_spell)), y + j + 1, x);

		/* Spell name */
		c_put_str(attr_name, format("%-30s", s_ptr->sname), y + j + 1, x + 5);

		/* Get spell mana cost */
		(void)do_spell(SPELL_MANA, i);

		/* Spell level, mana, chance */
		put_str(format("%2d %4d %3d%%", s_ptr->slevel, mana_cost,
		   spell_chance(i)), y + j + 1, x + 35);

		/* Spell information */
		c_put_str(attr_extra, format(" %-*s", 17, comment), y + j + 1, x + 47);
	}

	/* Clear the bottom line */
	clear_space(y + j + 2, x, 65);
}


/*
 * Hack -- display an object kind in the current window
 *
 * Include list of usable spells for readable books
 */
void display_koff(int k_idx)
{
	object_type *i_ptr;
	object_type object_type_body;

	char o_name[DESC_LEN];


	/* Erase the window */
	(void)Term_clear();

	/* No info */
	if (!k_idx) return;


	/* Get local object */
	i_ptr = &object_type_body;

	/* Prepare the object */
	object_prep(i_ptr, k_idx);


	/* Describe */
	object_desc_store(o_name, sizeof(o_name), i_ptr, FALSE, 0);

	/* Mention the object name */
	(void)Term_putstr(0, 0, -1, TERM_WHITE, o_name);


	/* Warriors are illiterate */
	if (p_ptr->realm == NONE) return;


	/* Display spells in readable books */
	if (i_ptr->tval == mp_ptr->spell_book)
	{
		/* Print spells */
		print_spells(i_ptr->tval, i_ptr->sval, 1, 0);
	}
}

/*
 * Allow user to choose a spell/prayer from the given book.
 *
 * If a valid spell is chosen, saves it in '*sn' and returns TRUE
 * If the user hits escape, returns FALSE, and set '*sn' to -1
 * If there are no legal choices, returns FALSE, and sets '*sn' to -2
 *
 * The "prompt" should be "cast", "recite", or "study"
 */
static int get_spell(int *sn, cptr prompt, int tval, int sval)
{
	int i;

	int spell = -1;

	int first_spell, after_last_spell;

	int verify;

	bool flag, redraw, okay;
	char choice;

	/* If we have already saved the screen, we need to keep the spell list shown */
	bool always_show = (screen_depth != 0);

	const magic_type *s_ptr;

	char out_val[DESC_LEN];

	cptr p = "";


	/* Get the spell, if available */
	if (repeat_pull(sn))
	{
		/* Find the array index of the spellbook's first spell. */
		first_spell = mp_ptr->book_start_index[sval];

		/* Find the first spell in the next book. */
		after_last_spell = mp_ptr->book_start_index[sval+1];

		/* Verify that the spell is in this book. */
		if (((*sn) >= first_spell) && ((*sn) < after_last_spell))
		{
			/* Verify that the spell is okay */
			if (spell_okay(*sn))
			{
				/* Success */
				return (TRUE);
			}
			else
			{
				/* Invalid repeat - reset it */
				repeat_clear();
			}
		}
	}


	/* Determine the magic description */
	p = spell_type();

	/* Assume no usable spells */
	okay = FALSE;

	/* Assume no spells available */
	(*sn) = -2;


	/* Find the array index of the spellbook's first spell. */
	first_spell = mp_ptr->book_start_index[sval];

	/* Find the first spell in the next book. */
	after_last_spell = mp_ptr->book_start_index[sval+1];

	/* Check for "okay" spells */
	for (i = first_spell; i < after_last_spell; i++)
	{
		/* Look for "okay" spells */
		if (spell_okay(i)) okay = TRUE;
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
	(void)strnfmt(out_val, sizeof(out_val), "(%^ss %c-%c, *=List, ESC=exit) %^s which %s?",
		p, I2A(0), I2A(after_last_spell - first_spell - 1), prompt, p);

	/* Option -- automatically show lists */
	if ((always_show_list) || (always_show))
	{
		/* Show list */
		redraw = TRUE;

		/* Save screen */
		if (!always_show) screen_save(FALSE);

		/* Display a list of spells */
		print_spells(tval, sval, 1, (Term->cols - 40) / 3);
	}

	/* Get a spell from the user */
	while (!flag && get_com(out_val, &choice))
	{
		/* Request redraw */
		if ((choice == ' ') || (choice == '*') || (choice == '?'))
		{
			/* Allow us to keep the list up */
			if (always_show) continue;

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
				screen_save(FALSE);

				/* Display a list of spells */
				print_spells(tval, sval, 1, (Term->cols - 40) / 3);
			}

			/* Ask again */
			continue;
		}


		/* Note verify */
		verify = (my_isupper(choice));

		/* Lowercase */
		choice = my_tolower(choice);

		/* Extract request */
		i = (islower(choice) ? A2I(choice) : -1);

		/* Totally Illegal */
		if ((i < 0) || (i >= after_last_spell - first_spell))
		{
			bell(format("Illegal choice of %ss!", spell_type()));
			continue;
		}

		/* Convert spellbook number to spell index. */
		spell = i + first_spell;

		/* Require "okay" spells */
		if (!spell_okay(spell))
		{
			bell(format("Illegal choice of %ss!", spell_type()));
			msg_format("You may not %s that %s.", prompt, p);
			continue;
		}

		/* Verify it */
		if (verify)
		{
			char tmp_val[DESC_LEN];

			/* Get the spell */
			s_ptr = &mp_ptr->info[spell];

			/* Special case -- spell's cost can vary */
			(void)do_spell(SPELL_MANA, spell);

			/* Prompt */
			(void)strnfmt(tmp_val, sizeof(tmp_val), "%^s %s (%d mana, %d%% fail)?",
				prompt, s_ptr->sname, mana_cost, spell_chance(spell));

			/* Belay that order */
			if (!get_check(tmp_val)) continue;
		}

		/* Stop the loop */
		flag = TRUE;
	}


	/* Restore the screen */
	if ((redraw) && (!always_show)) screen_load();


	/* Abort if needed */
	if (!flag) return (FALSE);

	/* Save the choice */
	(*sn) = spell;

	/* Remember command */
	repeat_push(*sn);

	/* Success */
	return (TRUE);
}


/*
 * Peruse the spells/prayers in a book.
 */
void do_cmd_browse_aux(object_type *o_ptr)
{
	int spell, lines;
	int i;

	const magic_type *s_ptr;


	/* Save screen */
	screen_save(FALSE);

	/* Display the spells */
	print_spells(o_ptr->tval, o_ptr->sval, 1, (Term->cols - 40) / 3);

	/* Prompt for a command */
	prt("", 0, 0);
	put_str(format("(Browsing) Choose a %s, or ESC: ", spell_type()), 0, 0);

	/*
	 * Hack - Determine how far from the top of the screen the spell list
	 * extends by counting spells, and adding space for name, etc.
	 */
	lines = mp_ptr->book_start_index[o_ptr->sval + 1] -
		mp_ptr->book_start_index[o_ptr->sval] + 3;


	/* Keep browsing spells.  Exit browsing on cancel. */
	while (TRUE)
	{
		/* Ask for a spell, allow cancel */
		if (!get_spell(&spell, "browse", o_ptr->tval, o_ptr->sval))
		{
			/* If cancelled, leave immediately. */
			if (spell == -1) break;

			/* Notify that there's nothing to see, and wait. */
			c_put_str(TERM_SLATE,
				format("No %ss to browse (press ESC to cancel)", spell_type()),
				0, 11);

			/* Any key cancels if no spells are available. */
			if (inkey(ALLOW_CLICK)) break;
		}

		/* Clear lines (Hack -- leave lots of space  XXX) */
		for (i = lines; i < lines + 6; i++)
		{
			clear_space(i, (Term->cols - 40) / 3, 65);
		}

		/* Move cursor */
		move_cursor(lines + 1, (Term->cols - 40) / 3 + 6);

		/* Get the spell */
		s_ptr = &mp_ptr->info[spell];

		/* Display that spell's information. */
		c_roff(TERM_L_BLUE, do_spell(SPELL_DESC, spell),
			(Term->cols - 40) / 3 + 1, (Term->cols - 40) / 3 + 64);
	}

	/* Clear the message line */
	prt("", 0, 0);

	/* Load screen */
	screen_load();
}



/*
 * Peruse the spells/prayers in a book, showing "spell tips" as
 * requested.  -LM-
 *
 * Note that browsing is allowed while confused or blind,
 * and in the dark, primarily to allow browsing in stores.
 */
void do_cmd_browse(void)
{
	int item;

	object_type *o_ptr;

	cptr q = "";
	cptr s = "";


	/* Can we browse books? */
	if (!can_study_or_cast()) return;


	/* Restrict choices to "useful" books */
	item_tester_tval = mp_ptr->spell_book;

	/* Get a realm-flavored description. */
	if (p_ptr->realm == MAGE)
	{
		q = "Browse which magic book?";
		s = "You have no magic books that you can read.";
	}
	if (p_ptr->realm == PRIEST)
	{
		q = "Browse which holy book?";
		s = "You have no holy books that you can read.";
	}
	if (p_ptr->realm == DRUID)
	{
		q = "Browse which stone of lore?";
		s = "You have no stones that you can read.";
	}
	if (p_ptr->realm == NECRO)
	{
		q = "Browse which tome?";
		s = "You have no tomes that you can read.";
	}

	/* Get an item */
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;
	item_to_object(o_ptr, item);

	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();

	/* Hack -- Update the screen */
	(void)Term_fresh();

	/* Browse this book */
	do_cmd_browse_aux(o_ptr);
}

/*
 * Validate book selection
 *  (note some of the restriction is handled by item_tester_tval)
 */
bool item_tester_hook_book(const object_type *o_ptr)
{
	/* Require either light or a book with glowing words */
	if (!no_light() || (o_ptr->flags2 & TR2_GLOW_WORDS))
 	{
		return (TRUE);
	}
	return (FALSE);
}


/*
 * Cast a spell, or output spell info, description, or calculate mana.
 */
cptr do_spell(int mode, int spell)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	bool okay = TRUE;

	/* Function modes */
	bool cast = (mode == SPELL_CAST);
	bool info = (mode == SPELL_INFO);
	bool desc = (mode == SPELL_DESC);
	bool mana = (mode == SPELL_MANA);
	bool fail = (mode == SPELL_FAIL);

	/* Casting variables */
	int item, chance, dir, beam;
	int spower, xtra_spower, reliability;
	int do_shapechange = 0;
	int spell_cost = 0;

	/* Spell-specific variables */
	int dam, dam1, dam2;
	int dur, dur1, dur2;
	int pow, pow1, pow2;
	int dice, sides;
	int rad, i;

	char dummy[DESC_LEN];

	object_type *o_ptr;

	const magic_type *s_ptr;

	cptr extra = "";
	cptr p = "";
	cptr r = "";
	cptr t = "";

	cptr q = "";
	cptr s = "";

	/* Get the spell */
	s_ptr = &mp_ptr->info[spell];


	/* Check mana cost */
	if (mana)
	{
		/* Get mana needed to cast spell */
		mana_cost = s_ptr->smana;

		/* Usually, we can just use the "book price" */
		if (mana_cost != 255) return ("");
	}

	/* We are casting a spell */
	if (cast)
	{
		/* Can we cast spells? */
		if (!can_study_or_cast()) return ("");

		/* Magic is blocked */
		if (p_ptr->nomagic)
		{
			msg_print("Your magical abilities are being blocked!");
			if (flush_failure) flush();
			return (FALSE);
		}

		/* Determine magic description. */
		p = spell_type();

		/* Determine spellbook description. */
		if (p_ptr->realm == MAGE)   r = "magic book";
		if (p_ptr->realm == PRIEST) r = "holy book";
		if (p_ptr->realm == DRUID)  r = "stone";
		if (p_ptr->realm == NECRO)  r = "tome";

		/* Determine method description. */
		if (p_ptr->realm == MAGE)   t = "cast";
		if (p_ptr->realm == PRIEST) t = "chant";
		if (p_ptr->realm == DRUID)  t = "evoke";
		if (p_ptr->realm == NECRO)  t = "perform";


		/* Restrict choices to spell books */
		item_tester_tval = mp_ptr->spell_book;
		item_tester_hook = item_tester_hook_book;


		/* Get a realm-flavored description. */
		if (p_ptr->realm == MAGE)
		{
			q = "Use which magic book?";
			if (no_light()) s = "You have no magic books that you can use. (check your light?)";
			else            s = "You have no magic books that you can use.";
		}
		if (p_ptr->realm == PRIEST)
		{
			q = "Use which holy book?";
			if (no_light()) s = "You have no holy books that you can use. (check your light?)";
			else            s = "You have no holy books that you can use.";
		}
		if (p_ptr->realm == DRUID)
		{
			q = "Use which stone of lore?";
			if (no_light()) s = "You have no stones that you can use. (check your light?)";
			else            s = "You have no stones that you can use.";
		}
		if (p_ptr->realm == NECRO)
		{
			q = "Use which tome?";
			if (no_light()) s = "You have no tomes that you can use. (check your light?)";
			else            s = "You have no tomes that you can use.";
		}

		/* Get an item */
		if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return ("");
		item_to_object(o_ptr, item);


		/* Track the object kind */
		object_kind_track(o_ptr->k_idx);

		/* Hack -- Handle stuff */
		handle_stuff();


		/* Ask for a spell */
		if (!get_spell(&spell, t, o_ptr->tval, o_ptr->sval))
		{
			if (spell == -2)
			{
				msg_format("You don't know any %ss in that %s.", p, r);
			}
			return ("");
		}


		/* Get the spell */
		s_ptr = &mp_ptr->info[spell];

		/* Get mana needed to cast spell */
		(void)do_spell(SPELL_MANA, spell);

		/* Save this spell cost (to avoid possible problems) */
		spell_cost = mana_cost;

		/* Verify "dangerous" spells */
		if (spell_cost > p_ptr->csp)
		{
			/* Warning */
			msg_format("You do not have enough mana to %s this %s.",
				t, p);

			/* Flush input */
			flush();

			/* Verify */
			if (!get_check("Attempt it anyway?")) return ("");
		}

		/* Make a sound */
		if (p_ptr->realm != PRIEST) sound(MSG_SPELL);
		else                        sound(MSG_PRAYER);

		/* Note action in saved messages */
		msg_add(format("You %s %s.", t, s_ptr->sname));


		/* Spell failure chance */
		chance = spell_chance(spell);

		/* Failed spell */
		if (rand_int(100) < chance)
		{
			if (flush_failure) flush();

			/* Message */
			if (p_ptr->realm == MAGE)
				msg_print("You failed to get the spell off!");
			if (p_ptr->realm == PRIEST)
				msg_print("You lost your concentration!");
			if (p_ptr->realm == DRUID)
				msg_print("You lost your concentration!");
			if (p_ptr->realm == NECRO)
				msg_print("You perform the ritual incorrectly!");

			/* Optional delay */
			if (delay_failure) pause_for(250);

			/* Wizard magic is extremely temperamental */
			if (p_ptr->realm == MAGE)
			{
				/* Special fail code for Call Destruction */
				if (spell == 48)
				{
					(void)do_spell(SPELL_FAIL, spell);
				}
				else if (rand_int(100) < chance - 3)
				{
					msg_print("You produce a chaotic effect!");
					wild_magic_sorcery(spell % 64);
				}
			}

			/* Dark powers don't like having their name taken in vain */
			else if (p_ptr->realm == NECRO)
			{
				if (rand_int(100) < chance - 3)
				{
					perilous_effect_necro(spell % 64,
						(o_ptr->sval == SV_DARK_BOOK_NECRONOMICON));
				}
			}

			/* Some druid techniques have special failure effects */
			else if (p_ptr->realm == DRUID)
			{
				if (rand_int(100) < chance - 3)
				{
					(void)do_spell(SPELL_FAIL, spell);
				}
			}

			/* All over, man */
			if (p_ptr->leaving) return (NULL);

			/* Do not process spell */
			okay = FALSE;
		}
	}


	/* Process spell */
	if (okay)
	{
		/* Assume normal power */
		bool limit_power = FALSE;

		/* Non-specialists have limited power */
		if ((p_ptr->realm == MAGE)   && (!(p_ptr->oath & (OATH_OF_SORCERY))))
			limit_power = TRUE;
		if ((p_ptr->realm == PRIEST) && (!(p_ptr->oath & (COVENANT_OF_FAITH))))
			limit_power = TRUE;
		if ((p_ptr->realm == DRUID)  && (!(p_ptr->oath & (YAVANNAS_FELLOWSHIP))))
			limit_power = TRUE;
		if ((p_ptr->realm == NECRO)  && (!(p_ptr->oath & (BLACK_MYSTERY))))
			limit_power = TRUE;

		/* Get spell power (up to 100, or 75 for non-specialists) */
		/* Low level casters non-oath casters gain power faster, as they cannot take the oath */
		if (!limit_power) spower = get_skill(S_MAGIC, 0, 100);
		else if (get_skill(S_MAGIC, 0, 100) < 20)  spower = get_skill(S_MAGIC, 0,  95);
		else spower = 5 + get_skill(S_MAGIC, 0,  70);

		/* Necromancers cast stronger spells in the darkness */
		if (p_ptr->realm == NECRO) spower += darkness_ratio(3) / 20;

		/* Priests dislike the darkness */
		if (p_ptr->realm == PRIEST) spower -= darkness_ratio(3) / 20;

		/* Get extra power (over and above spell level */
		xtra_spower = MAX(1, spower - s_ptr->slevel);

		/* Get reliability factor (effective level - spell difficulty) */
		reliability = MAX(0,
			get_skill(mp_ptr->spell_skill, 0, 100) - s_ptr->slevel);

		/* Hack -- chance of "beam" instead of "bolt" */
		beam = get_skill(mp_ptr->spell_skill, 0, 50);

		/* Casting a spell -- Practice the spell level skill */
		if (cast) skill_being_used = S_MAGIC;


		/* Spells */
		switch (s_ptr->index)
		{
/*** Wizard's Spells ***/

			/* Magic Missile */
			case 0:
			{
				dice = 2;    sides = 5 + spower / 8;

				if (info) return (format("dam %dd%d", dice, sides));
				if (desc) return ("Fires a bolt of mana.");
				if (cast)
				{
					if (!get_aim_dir(&dir)) return (NULL);
					(void)fire_bolt(GF_MANA, dir, damroll(dice, sides));
				}
				break;
			}

			/* Detect Monsters */
			case 1:
			{
				if (info) return ("");
				if (desc) return ("Detects nearby monsters that are not invisible.");
				if (cast)
				{
					(void)detect_monsters_normal(FALSE, TRUE);
				}
				break;
			}

			/* Phase Door */
			case 2:
			{
				if (info) return ("range 10");
				if (desc) return ("Random minor displacement.");
				if (cast)
				{
					teleport_player(10, TRUE, FALSE);
				}
				break;
			}

			/* Light Area */
			case 3:
			{
				dice = 2;     sides = spower / 4;

				if (info) return (format("dam %dd%d", dice, sides));
				if (desc) return ("Permanently lights up a room or the area lit by your light source.");
				if (cast)
				{
					(void)lite_area(damroll(dice, sides), MAX(2, p_ptr->cur_lite));
				}
				break;
			}

			/* Bar Door */
			case 4:
			{
				if (info) return ("");
				if (desc) return ("Bars a door, with power dependant on character skill.  Repeated casts strengthen the door.");
				if (cast)
				{
					if (!get_aim_dir(&dir)) return (NULL);
					jam_door(dir);
				}
				break;
			}

			/* Cure Light Wounds */
			case 5:
			{
				pow = 5;

				if (info) return (format("heal %d%%", pow));
				if (desc) return ("Reduces cuts and heals you a little.");
				if (cast)
				{
					(void)heal_player(pow, pow * 2);
					set_cut(p_ptr->cut - (5 + spower / 4));
				}
				break;
			}

			/* Detect Doors and Stairs */
			case 6:
			{
				if (info) return ("");
				if (desc) return ("Detects nearby doors and stairs.");
				if (cast)
				{
					(void)detect_doors(FALSE);
					(void)detect_stairs(FALSE);
				}
				break;
			}

			/* Stinking Cloud */
			case 7:
			{
				dam = 5 + (spower / 8);
				rad = (spower < 45 ? 2 : 3);

				if (info) return (format("dam %d, rad = %d", dam, rad));
				if (desc) return ("Fires a cloud of poison.");
				if (cast)
				{
					if (!get_aim_dir(&dir)) return (NULL);
					(void)fire_orb(GF_POIS, dir, dam, rad);
				}
				break;
			}

			/* Confuse Monster */
			case 8:
			{
				if (info) return ("");
				if (desc) return ("Attempts to confuse one monster.");
				if (cast)
				{
					if (!get_aim_dir(&dir)) return (NULL);
					(void)confuse_monster(dir, 30 + 2 * spower / 3);
				}
				break;
			}

			/* Spear of Light */
			case 9:
			{
				if (info) return ("dam 4d5");
				if (desc) return ("Fires a line of weak light.  Affects light-hating creatures.");
				if (cast)
				{
					if (!get_aim_dir(&dir)) return (NULL);
					(void)lite_line(dir);
				}
				break;
			}

			/* Wonder */
			case 10:
			{
				if (info) return ("");
				if (mana)
				{
					/* Cost increases with Wizardry skill */
					mana_cost = 4 + reliability / 7;
					return ("");
				}
				if (desc) return ("Invokes strange magics, which might conjure up bolts, beams, or balls of esoteric forces, or affect your foes in ways pleasing and displeasing.  You can do great damage with this spell, especially if your Wizardry skill is high.");
				if (cast)
				{
					if (!get_aim_dir(&dir)) return ("");
					wonder(dir);
				}
				break;
			}

			/* Teleport Self */
			case 11:
			{
				if (info) return ("range 100");
				if (desc) return ("Random major displacement.");
				if (cast)
				{
					teleport_player(100, TRUE, FALSE);
				}
				break;
			}

			/* Blink Away */
			case 12:
			{
				if (info) return ("");
				if (desc) return ("Whenever you are hit by a melee blow, you attempt to blink away (this does not always work, though).  This spell can be dangerous if activated near hostile terrain.");
				if (cast)
				{
					(void)set_blink_away(reliability / 15 + rand_range(3, 6));
				}
				break;
			}

			/* Sleep Monster */
			case 13:
			{
				if (info) return ("");
				if (desc) return ("Attempts to put a monster to sleep.");
				if (cast)
				{
					if (!get_aim_dir(&dir)) return (NULL);
					(void)sleep_monster(dir, 20 + 2 * spower / 3);
				}
				break;
			}

			/* Sound Blast */
			case 14:
			{
				dam = 12 + spower / 3;
				rad = 3 + spower / 33;

				if (info) return (format("dam %d, rad %d", dam, rad));
				if (desc) return ("Fires an arc of sound.");
				if (cast)
				{
					if (!get_aim_dir(&dir)) return (NULL);
					(void)fire_arc(GF_SOUND, dir, dam, rad, 60);
				}
				break;
			}

			/* Fireflash */
			case 15:
			{
				dam = 10 + spower / 5;
				if (p_ptr->aura_fire) dam = 3 * dam / 2;

				if (info) return (format("dam %d", dam));
				if (desc) return ("Conjures up a great flash of fire around you.");
				if (cast)
				{
					(void)fire_star(GF_FIRE, 0, dam, 7);
				}
				break;
			}

			/* Magic Disarm */
			case 16:
			{
				if (info) return ("");
				if (desc) return ("Attempts to disarm a trap adjacent to you.");
				if (cast)
				{
					if (!get_aim_dir(&dir)) return (NULL);
					(void)disarm_trap(dir);
				}
				break;
			}

			/* Slow Monster */
			case 17:
			{
				if (info) return ("");
				if (desc) return ("Attempts to slow a monster.");
				if (cast)
				{
					if (!get_aim_dir(&dir)) return (NULL);
					(void)slow_monster(dir, 20 + 2 * spower / 3);
				}
				break;
			}

			/* Fire Bolt */
			case 18:
			{
				dice = spower / 6;
				sides = (p_ptr->aura_fire ? 12 : 8);

				if (info) return (format("dam %dd%d", dice, sides));
				if (desc) return ("Fires a bolt or beam of fire.");
				if (cast)
				{
					if (!get_aim_dir(&dir)) return (NULL);
					(void)fire_bolt_or_beam(beam + 5, GF_FIRE, dir,
						damroll(dice, sides));
				}
				break;
			}

			/* Polymorph */
			case 19:
			{
				if (info) return ("");
				if (desc) return ("Attempts to change a monster.");
				if (cast)
				{
					if (!get_aim_dir(&dir)) return (NULL);
					(void)poly_monster(dir, 30 + 3 * spower / 4);
				}
				break;
			}

			/* Identify */
			case 20:
			{
				if (info) return ("");
				if (desc) return ("Identifies an object.");
				if (cast)
				{
					if (!ident_spell()) return (NULL);
				}
				break;
			}

			/* Evasion */
			case 21:
			{
				dur1 = (p_ptr->evasion ?  1 :      spower / 4);
				dur2 = (p_ptr->evasion ? 20 : 30 + spower / 4);
				if (p_ptr->evasion) extra = "longer";

				if (info) return (format("dur %d-%d %s", dur1, dur2, extra));
				if (desc) return ("Improves your dodging ability temporarily.  Effects increase with Wizardry skill.  This spell is most effective when combined with reasonably good dodging.");
				if (cast)
				{
					(void)set_evasion(p_ptr->evasion +
						rand_range(dur1, dur2));
				}
				break;
			}

			/* Tap Magical Energy */
			case 22:
			{
				if (info) return ("");
				if (desc) return ("Turn rod, wand, or staff energy into mana.  The higher-level the item, and the more charges it has, the more magical energy it provides.  Rods have little usable energy, and staffs quite a bit.");
				if (cast)
				{
					if (!tap_magical_energy()) return (NULL);
				}
				break;
			}

			/* Elemental Protection */
			case 23:
			{
				dur2 = 30 + reliability / 5;  dur1 = dur2 - 15;

				if (info) return (format("dur %d-%d", dur1, dur2));
				if (desc) return ("Temporary opposition to fire and frost with a Wizardry skill of less than 60, and all the four elements afterwards.  Effects are cumulative with equipment resists.");
				if (cast)
				{
					dur = rand_range(dur1, dur2);

					if ((p_ptr->oppose_acid) || (p_ptr->oppose_elec) ||
					    (p_ptr->oppose_fire) || (p_ptr->oppose_cold))
					{
						dur /= 2;
					}

					if (get_skill(mp_ptr->spell_skill, 0, 100) >= 60)
						(void)set_oppose_acid(p_ptr->oppose_acid + dur);
					if (get_skill(mp_ptr->spell_skill, 0, 100) >= 60)
						(void)set_oppose_elec(p_ptr->oppose_elec + dur);
					(void)set_oppose_fire(p_ptr->oppose_fire + dur);
					(void)set_oppose_cold(p_ptr->oppose_cold + dur);

					/* Auras of fire and frost double duration */
					if (p_ptr->aura_fire)
						(void)set_oppose_fire(p_ptr->oppose_fire + dur);
					if (p_ptr->aura_cold)
						(void)set_oppose_cold(p_ptr->oppose_cold + dur);
				}
				break;
			}

			/* Cone of Cold  */
			case 24:
			{
				dam = 5 * spower / 4;
				rad = 3 + spower / 24;
				if (p_ptr->aura_cold) dam = 3 * dam / 2;

				if (info) return (format("dam %d, rad %d", dam, rad));
				if (desc) return ("Fires an arc of cold.");
				if (cast)
				{
					if (!get_aim_dir(&dir)) return (NULL);
					(void)fire_arc(GF_COLD, dir, dam, rad, 45);
				}
				break;
			}

			/* Detect Enchantment */
			case 25:
			{
				if (info) return ("");
				if (desc) return ("Detects nearby enchanted objects.");
				if (cast)
				{
					(void)detect_objects_magic(FALSE);
				}
				break;
			}

			/* Haste Self */
			case 26:
			{
				dur1 = (p_ptr->fast ? 1 :      spower / 4);
				dur2 = (p_ptr->fast ? 5 : 20 + spower / 4);
				if (p_ptr->fast) extra = "longer";

				if (info) return (format("dur %d-%d %s", dur1, dur2, extra));
				if (desc) return ("Temporarily hasten yourself.");
				if (cast)
				{
					(void)set_fast(p_ptr->fast + rand_range(dur1, dur2));
				}
				break;
			}

			/* Wall of Fire */
			case 27:
			{
				dam = (5 * spower / 4) - 10;
				if (p_ptr->aura_fire) dam = 3 * dam / 2;

				if (info) return (format("dam %d", dam));
				if (desc) return ("Fires a wall of fire that advances towards your target.  Most effective in open areas.");
				if (cast)
				{
					int ty, tx;

					/* Get a new effect index */
					i = effect_prep();

					/* Note failure XXX */
					if (i < 0) break;

					/* Get a direction */
					if (!get_aim_dir(&dir)) return (NULL);

					/* Use the given direction */
					ty = py + MAX_RANGE * ddy[dir];
					tx = px + MAX_RANGE * ddx[dir];

					/* Hack -- Use an actual "target" */
					if ((dir == 5) && target_okay())
					{
						ty = p_ptr->target_row;
						tx = p_ptr->target_col;
					}

					/* We want an advancing wall, */
					x_list[i].index = EFFECT_WALL;

					/* Of fire */
					x_list[i].type = GF_FIRE;

					/* That starts at the character location. */
					x_list[i].y0 = py;
					x_list[i].x0 = px;

					/* Practices the spellcasting skill */
					x_list[i].practice_skill = S_MAGIC;

					/* It advances one grid every six -> three game turns, */
					x_list[i].time_delay = 5 - (spower - 60) / 15;

					/* Heads for the target grid, */
					x_list[i].y1 = ty;
					x_list[i].x1 = tx;

					/* Does damage, */
					x_list[i].power = dam;

					/* Created by the character and capable of hurting him, */
					x_list[i].flags |= (EF1_HURT_PLAY | EF1_CHARACTER);

					/* Is self-illuminating, */
					x_list[i].flags |= (EF1_SHINING);

					/* Spreads out for three or four grids each way, */
					x_list[i].power2 = (spower >= 75 ? 4 : 3);

					/* And lasts for a certain period of time. */
					x_list[i].lifespan = MAX_RANGE;
				}
				break;
			}

			/* Chromatic Burst */
			case 28:
			{
				dam1 = 3 * spower / 2;
				dam2 = 5 * spower / 2;

				if (info)
				{
					if (p_ptr->aura_cold || p_ptr->aura_fire)
						return (format("dam %d-%d or %d-%d", dam1, dam2, dam1 * 3 / 2, dam2 * 3 / 2));
					else
						return (format("dam %d-%d", dam1, dam2));
				}
				if (desc) return ("Fires a burst of any of the four elements.");
				if (cast)
				{
					if (!get_aim_dir(&dir)) return (NULL);
					if (!chromatic_burst(rand_range(dam1, dam2), dir)) return (NULL);
				}
				break;
			}

			/* Frighten Monster */
			case 29:
			{
				if (info) return ("");
				if (desc) return ("Attempts to panic a monster.");
				if (cast)
				{
					if (!get_aim_dir(&dir)) return (NULL);
					(void)fear_monster(dir, 20 + 2 * spower / 3);
				}
				break;
			}

			/* Cloaking */
			case 30:
			{
				dur1 = (p_ptr->tim_invis ?  5 : 20 +     spower / 2);
				dur2 = (p_ptr->tim_invis ? 20 : 20 + 3 * spower / 2);

				if (info) return (format("dur %d-%d", dur1, dur2));
				if (desc) return ("Makes you partially invisible.  This spell becomes more effective with skill, but high-level monsters are also more perceptive.");
				if (cast)
				{
					(void)set_invis(p_ptr->tim_invis + rand_range(dur1, dur2),
						7 + spower / 3);
				}
				break;
			}

			/* Mass Confusion */
			case 31:
			{
				if (info) return ("");
				if (desc) return ("Attempts to confuse all viewable monsters.");
				if (cast)
				{
					(void)confu_monsters(23 + 2 * spower / 3);
				}
				break;
			}

			/* Chaos Cloud */
			case 32:
			{
				dam = 2 * (spower - 20);

				if (info) return (format("dam %d", dam));
				if (desc) return ("Fires a ball of chaos.");
				if (cast)
				{
					if (!get_aim_dir(&dir)) return (NULL);
					(void)fire_ball(GF_CHAOS, dir, dam, 2);
				}
				break;
			}

			/* Blinding and Befuddlement */
			case 33:
			{
				dam = 4 * spower / 3;
				rad = 6 + reliability / 8;

				if (info) return (format("dam %d/ea, rad %d", dam, rad));
				if (desc) return ("Fires a swirl of sorcerous missiles; light, darkness, and confusion.  This spell is much more effective, but also somewhat more costly, when cast out in the open.");
				if (mana)
				{
					/* Mana cost depends on area covered. */
					calc_mana_aux(15, 25, rad);
					return ("");
				}
				if (cast)
				{
					/* Lots and lots of bolts:  use lingering graphics */
					fire_storm(-1, -2, py, px, dam, rad, 12, 0, TRUE);
				}
				break;
			}

			/* Door Creation */
			case 34:
			{
				if (info) return ("");
				if (desc) return ("Creates a barrier of doors around you.");
				if (cast)
				{
					(void)door_creation();
				}
				break;
			}

			/* Stair Creation */
			case 35:
			{
				if (info) return ("");
				if (desc) return ("Creates a staircase nearby.  Random choice between up or down, except on quest levels.");
				if (cast)
				{
					(void)stair_creation();
				}
				break;
			}

			/* Dancing Feet */
			case 36:
			{
				dur1 = (p_ptr->dancing_feet ? 1 :  6);
				dur2 = (p_ptr->dancing_feet ? 4 : 12);
				if (p_ptr->dancing_feet) extra = "longer";

				if (info) return (format("dur %d-%d %s", dur1, dur2, extra));
				if (desc) return ("Blinks you every turn until it wears off.  Will not teleport you into lava, etc.");
				if (cast)
				{
					(void)set_dancing_feet(p_ptr->dancing_feet +
						rand_range(dur1, dur2), NULL, TRUE);
				}
				break;
			}

			/* Teleport Other */
			case 37:
			{
				if (info) return ("");
				if (desc) return ("Attempts to teleport a monster away.");
				if (cast)
				{
					if (!get_aim_dir(&dir)) return (NULL);
					(void)teleport_monster(dir);
				}
				break;
			}

			/* Teleport Level */
			case 38:
			{
				if (info) return ("");
				if (desc) return ("Immediately takes you to the next level up or down.");
				if (cast)
				{
					teleport_player_level();
				}
				break;
			}

			/* Phase Warp */
			case 39:
			{
				pow1 = 10 + (reliability / 5);
				pow2 = 8 - (reliability / 5);

				if (info) return (format("range %d, varies %d", pow1, pow2));
				if (desc) return ("Semi-controlled teleportation.");
				if (cast)
				{
					if (!phase_warp(pow1, pow2, FALSE)) return (NULL);
				}
				break;
			}

			/* Word of Recall */
			case 40:
			{
				if (info) return ("");
				if (desc) return ("Recalls you to the town, or as deep in the dungeon as you have ever gone.");
				if (cast)
				{
					recall_player();
				}
				break;
			}

			/* Detection */
			case 41:
			{
				if (info) return ("");
				if (desc) return ("Detects traps, doors, stairs, treasure, objects, and monsters nearby.");
				if (cast)
				{
					(void)detect_all(FALSE, TRUE);
				}
				break;
			}

			/* Magic Shield */
			case 42:
			{
				dur1 = (p_ptr->shield ?          1 : spower / 4);
				dur2 = (p_ptr->shield ? spower / 4 : spower / 2);
				if (p_ptr->shield) extra = "longer";

				if (info) return (format("dur %d-%d %s", dur1, dur2, extra));
				if (desc) return ("Temporarily increases armor class by 50.");
				if (cast)
				{
					(void)set_shield(p_ptr->shield + rand_range(dur1, dur2),
						"A magical shield forms around your body!");
				}
				break;
			}

			/* Aura of Frost */
			case 43:
			{
				dur1 = (p_ptr->aura_cold ?          1 : spower / 2);
				dur2 = (p_ptr->aura_cold ? spower / 2 : spower    );
				if (p_ptr->aura_cold) extra = "longer";

				if (info) return (format("dur %d-%d %s", dur1, dur2, extra));
				if (desc) return ("Surrounds you with an aura of frost that freezes creatures that attack you, protects you from cold, and enhances wizard spells that include cold.  You cannot have both an aura of frost and an aura of fire active at the same time; in fact, you can cancel one by casting its opposite.");
				if (cast)
				{
					(void)set_aura_cold(p_ptr->aura_cold +
						rand_range(dur1, dur2));
				}
				break;
			}

			/* Aura of Fire */
			case 44:
			{
				dur1 = (p_ptr->aura_fire ?          1 : spower / 2);
				dur2 = (p_ptr->aura_fire ? spower / 2 : spower    );
				if (p_ptr->aura_fire) extra = "longer";

				if (info) return (format("dur %d-%d %s", dur1, dur2, extra));
				if (desc) return ("Surrounds you with an aura of fire that burns creatures that attack you, protects you from fire, and enhances wizard spells that include fire.  You cannot have both an aura of frost and an aura of fire active at the same time; in fact, you can cancel one by casting its opposite.");
				if (cast)
				{
					(void)set_aura_fire(p_ptr->aura_fire +
						rand_range(dur1, dur2));
				}
				break;
			}

			/* Foes a-Phasing */
			case 45:
			{
				dur1 = (p_ptr->phasing_foes ? 3 : 3 + spower / 10);
				dur2 = (p_ptr->phasing_foes ? 6 : 6 + spower /  5);
				if (p_ptr->phasing_foes) extra = "longer";

				if (info) return (format("dur %d-%d %s", dur1, dur2, extra));
				if (desc) return ("Attempts to blink every opponent in line of sight every turn until it wears off.");
				if (cast)
				{
					(void)set_phasing_foes(p_ptr->phasing_foes +
						rand_range(dur1, dur2), NULL);
				}
				break;
			}

			/* Wizardly Enhancement */
			case 46:
			{
				dur1 = (p_ptr->wiz_prot ? spower / 3 : 2 * spower / 3);
				dur2 = (p_ptr->wiz_prot ? spower / 2 : 4 * spower / 3);
				if (p_ptr->wiz_prot) extra = "longer";

				if (info) return (format("dur %d-%d %s", dur1, dur2, extra));
				if (desc) return ("Provides resistance to disenchantment and mana loss, gives you a better saving throw, and sustains your intelligence.  Also acts as a haste spell.");
				if (cast)
				{
					(void)set_wiz_prot(p_ptr->wiz_prot + rand_range(dur1, dur2));
					(void)set_fast(p_ptr->fast + rand_range(dur1, dur2));
				}
				break;
			}

			/* Arc of Force */
			case 47:
			{
				dam = 3 * spower / 2;
				rad = 3 + spower / 20;

				if (info) return (format("dam %d, rad %d", dam, rad));
				if (desc) return ("Fires an arc of force.");
				if (cast)
				{
					if (!get_aim_dir(&dir)) return (NULL);
					(void)fire_arc(GF_FORCE, dir, dam, rad, 90);
				}
				break;
			}

			/* Call Destruction */
			case 48:
			{
				if (info) return ("");
				if (desc) return ("Smashes the dungeon around any target in line of fire.  Destroys almost all objects nearby, deletes ordinary monsters, and banishes uniques from the level.  Be warned:  Casting this spell incorrectly can cause serious side-effects!");
				if (fail)
				{
					call_destruction(FALSE);
				}
				if (cast)
				{
					call_destruction(TRUE);
				}
				break;
			}

			/* Plasma Vortex */
			case 49:
			{
				dam = (spower - 15) * 3;

				if (info) return (format("dam %d", dam));
				if (desc) return ("Fires an intense vortex of electric fire. \n(This spell is not affected by auras of fire or frost.)");
				if (cast)
				{
					if (!get_aim_dir(&dir)) return (NULL);

					(void)fire_ball_special(GF_PLASMA, dir, dam, 1,
						PROJECT_NO_TRAIL, 0);
				}
				break;
			}

			/* Rift */
			case 50:
			{
				dam = 3 * spower / 2;

				if (info) return (format("dam %d", dam));
				if (desc) return ("Fires a beam of gravity.");
				if (cast)
				{
					if (!get_aim_dir(&dir)) return (NULL);
					(void)fire_beam(GF_GRAVITY, dir, dam);
				}
				break;
			}

			/* Mana Storm */
			case 51:
			{
				dam = (spower - 20) * 3;

				if (info) return (format("dam %d", dam));
				if (desc) return ("Fire a large storm of mana.");
				if (cast)
				{
					if (!get_aim_dir(&dir)) return (NULL);
					(void)fire_ball_special(GF_MANA, dir, dam, 4,
						PROJECT_NO_TRAIL, 15);
				}
				break;
			}

			/* Prismatic Armageddon */
			case 52:
			{
				dam = 3 * spower / 2;
				rad = 16;

				if (info) return (format("dam %d each hit, rad %d", dam, rad));
				if (desc) return ("Fires a massive storm of wizard-magics.  This spell is much more effective, but also somewhat more costly, when cast out in the open.");
				if (mana)
				{
					/* Mana cost depends on area covered. */
					calc_mana_aux(20, 35, rad);
					return ("");
				}
				if (cast)
				{
					/* Lots and lots of arcs of all sorts of types */
					fire_storm(-1, -1, py, px, dam, rad, 4, 2, FALSE);
				}
				break;
			}

			case 53:
			case 54:
			case 55:
			case 56:
			case 57:
			case 58:
			case 59:
			case 60:
			case 61:
			case 62:
			case 63:
			{
				break;
			}


/*** Priestly Prayers ***/

			/* Detect Evil */
			case 64:
			{
				if (info) return ("");
				if (desc) return ("Detects all nearby evil monsters, even invisible ones.");
				if (cast)
				{
					(void)detect_evil(FALSE, TRUE);
				}
				break;
			}

			/* Cure Minor Wounds */
			case 65:
			{
				pow = 5;

				if (info) return (format("heal %d%%", pow));
				if (desc) return ("Reduces cuts and heals you a little.");
				if (cast)
				{
					(void)heal_player(pow, pow * 2);
					(void)set_cut(p_ptr->cut - 10);
				}
				break;
			}

			/* Bless */
			case 66:
			{
				dur1 = (p_ptr->blessed ?  1 : 12);
				dur2 = (p_ptr->blessed ? 12 : 24);
				if (p_ptr->blessed) extra = "longer";

				if (info) return (format("dur %d-%d %s", dur1, dur2, extra));
				if (desc) return ("Short-duration bonus to fighting ability and armor class.");
				if (cast)
				{
					(void)set_blessed(p_ptr->blessed + rand_range(dur1, dur2), NULL);
				}
				break;
			}

			/* Boldness */
			case 67:
			{
				dur1 = (p_ptr->bold ?  1 : 12);
				dur2 = (p_ptr->bold ? 12 : 24);
				if (p_ptr->bold) extra = "longer";

				if (info) return (format("dur %d-%d %s", dur1, dur2, extra));
				if (desc) return ("Removes fear, provides short-tern protection from fear.");
				if (cast)
				{
					(void)set_bold(p_ptr->bold + rand_range(dur1, dur2));
				}
				break;
			}

			/* Radiance */
			case 68:
			{
				dice = 2;     sides = spower / 4;

				if (info) return (format("dam %dd%d", dice, sides));
				if (desc) return ("Permanently lights up a room or the area lit by your light source.");
				if (cast)
				{
					(void)lite_area(damroll(dice, sides), MAX(2, p_ptr->cur_lite));
				}
				break;
			}

			/* Spiritual Hammer */
			case 69:
			{
				dice = 2;    sides = 7 + spower / 10;

				if (info) return (format("dam %dd%d", dice, sides));
				if (desc) return ("Fires a bolt of holy force that does extra damage to evil creatures.");
				if (cast)
				{
					if (!get_aim_dir(&dir)) return (NULL);
					(void)fire_bolt(GF_HOLY_ORB, dir, damroll(dice, sides));
				}
				break;
			}

			/* Detect Doors and Stairs */
			case 70:
			{
				if (info) return ("");
				if (desc) return ("Detects all nearby doors and stairs.");
				if (cast)
				{
					(void)detect_doors(FALSE);
					(void)detect_stairs(FALSE);
				}

				break;
			}

			/* Scare Monster */
			case 71:
			{
				if (info) return ("");
				if (desc) return ("Attempts to panic a monster.");
				if (cast)
				{
					if (!get_aim_dir(&dir)) return (NULL);
					(void)fear_monster(dir, 30 + 2 * spower / 3);
				}
				break;
			}

			/* Dispel Poison */
			case 72:
			{
				if (info) return ("");
				if (desc) return ("Purges all poison from your system.  Reduces disease a little.");
				if (cast)
				{
					(void)set_poisoned(0);
					(void)set_diseased(p_ptr->diseased - (2 + spower / 10), NULL);
				}
				break;
			}

			/* Blink */
			case 73:
			{
				if (info) return ("");
				if (desc) return ("Random minor teleportation.");
				if (cast)
				{
					teleport_player(10, TRUE, FALSE);
				}
				break;
			}

			/* Sense Invisible */
			case 74:
			{
				dur1 = (p_ptr->detect_inv ?  1 : 24);
				dur2 = (p_ptr->detect_inv ? 12 : 48);
				if (p_ptr->detect_inv) extra = "longer";

				if (info) return (format("dur %d-%d %s", dur1, dur2, extra));
				if (desc) return ("Temporary see invisible.");
				if (cast)
				{
					(void)set_detect_inv(p_ptr->detect_inv +
						rand_range(dur1, dur2));
				}
				break;
			}

			/* Shining Spear */
			case 75:
			{
				if (info) return ("dam 4d5");
				if (desc) return ("Fires a line of weak light.  Affects light-hating creatures.");
				if (cast)
				{
					if (!get_aim_dir(&dir)) return (NULL);
					(void)lite_line(dir);
				}
				break;
			}

			/* Scan Object */
			case 76:
			{
				/* Full power with a Piety skill of 45 or above */
				bool full = (get_skill(mp_ptr->spell_skill, 0, 100) >= 45);

				if (info) return (full ? "(full)" : "");
				if (desc) return ("Instantly tells if objects are blessed or cursed and reveals artifacts.  As your piety grows, you can also learn to distinguish objects with special (even hidden) qualities from more ordinary blessed and cursed items, and identify rings and amulets.");
				if (cast)
				{
					/* Scan the object */
					scan_object_priest(full);
				}
				break;
			}

			/* Sustenance */
			case 77:
			{
				if (info) return ("");
				if (desc) return ("Fully feeds you.");
				if (cast)
				{
					(void)set_food(p_ptr->food_bloated - 50);
				}
				break;
			}

			/* Orb of Draining */
			case 78:
			{
				dam1 = spower / 2 - 2;
				dam2 = spower / 2 + 8;
				rad = ((spower < 50) ? 1 : 2);

				if (info) return (format("dam %d-%d, rad %d", dam1, dam2, rad));
				if (desc) return ("Fires an orb of holy force that does extra damage to evil creatures, and does full damage to everything caught in the blast.");
				if (cast)
				{
					if (!get_aim_dir(&dir)) return (NULL);
					(void)fire_orb(GF_HOLY_ORB, dir, rand_range(dam1, dam2), rad);
				}

				break;
			}

			/* Sense Surroundings */
			case 79:
			{
				if (info) return ("");
				if (desc) return ("Maps the local area, reveals doors and stairs.");
				if (cast)
				{
					(void)map_area(0, 0, FALSE);
				}
				break;
			}

			/* Remove Curse */
			case 80:
			{
				if (info) return ("");
				if (desc) return ("Removes standard curses.");
				if (cast)
				{
					int cnt = remove_curse();
					if (cnt)
					{
						msg_format("You glow for a moment; %s broken.",
							cnt > 1 ? "several curses are" : "a curse is");
					}
				}
				break;
			}

			/* Resist Heat and Cold */
			case 81:
			{
				dur1 = 10 + spower / 5;     dur2 = 25 + spower / 5;

				if (info) return (format("dur %d-%d", dur1, dur2));
				if (desc) return ("Temporary opposition to fire and frost.  Cumulative with equipment resistances.");
				if (cast)
				{
					dur = rand_range(dur1, dur2);

					if ((p_ptr->oppose_fire) || (p_ptr->oppose_cold))
					{
						dur /= 2;
					}

					(void)set_oppose_fire(p_ptr->oppose_fire + dur);
					(void)set_oppose_cold(p_ptr->oppose_cold + dur);
				}
				break;
			}

			/* Turn Evil */
			case 82:
			{
				if (info) return ("");
				if (desc) return ("Attempts to make all evil monsters in line of sight turn and run.  Most effective against undead and demons.");
				if (cast)
				{
					turn_evil_priest(20 + spower / 2);
				}
				break;
			}

			/* Cure Major Wounds */
			case 83:
			{
				pow = 20;

				if (info) return (format("heal %d%%", pow));
				if (desc) return ("Reduces cuts and heals you a fair amount.");
				if (cast)
				{
					(void)heal_player(pow, pow * 2);
					(void)set_cut(p_ptr->cut / 2 - 50);
				}
				break;
			}

			/* Prayer */
			case 84:
			{
				dur1 = (p_ptr->blessed ?  1 : 60);
				dur2 = (p_ptr->blessed ? 45 : 90);
				if (p_ptr->blessed) extra = "longer";

				if (info) return (format("dur %d-%d %s", dur1, dur2, extra));
				if (desc) return ("Long-duration bonus to fighting ability and armor class.");
				if (cast)
				{
					(void)set_blessed(p_ptr->blessed + rand_range(dur1, dur2), NULL);
				}
				break;
			}

			/* Protection from Evil */
			case 85:
			{
				dur1 = (p_ptr->protevil ?          1 : 20 + spower / 2);
				dur2 = (p_ptr->protevil ? spower / 2 : 40 + spower / 2);
				if (p_ptr->protevil) extra = "longer";

				if (info) return (format("dur %d-%d %s", dur1, dur2, extra));
				if (desc) return ("Temporary protection from lesser evil creatures.");
				if (cast)
				{
					(void)set_protevil(p_ptr->protevil + rand_range(dur1, dur2));
				}
				break;
			}

			/* Exorcism */
			case 86:
			{
				dam1 = 3 * xtra_spower / 2;
				dam2 = 3 * spower / 2;

				if (info) return (format("dam %d-%d", dam1, dam2));
				if (desc) return ("Dispels an undead or demonic adversary.  Also attempts to panic any evil one.");
				if (cast)
				{
					if (!get_aim_dir(&dir)) return (NULL);
					(void)exorcise_monster(dir, rand_range(dam1, dam2));
				}
				break;
			}

			/* Dispel Evil */
			case 87:
			{
				dam1 = xtra_spower;     dam2 = spower;

				if (info) return (format("dam %d-%d", dam1, dam2));
				if (desc) return ("Dispels all evil monsters in line of sight.");
				if (cast)
				{
					(void)dispel_evil(rand_range(dam1, dam2));
				}
				break;
			}

			/* Divine Light */
			case 88:
			{
				dice = spower / 4;
				sides = 8;
				rad = 4 + reliability / 20;

				if (info) return (format("dam %dd%d, rng %d", dice, sides, rad));
				if (desc) return ("Fires a strong bolt of light with limited range.");
				if (cast)
				{
					if (!get_aim_dir(&dir)) return (NULL);
					(void)fire_bolt_beam_special(GF_LITE, dir,
						damroll(dice, sides), rad, 0L);
				}
				break;
			}

			/* Meditation */
			case 89:
			{
				if (info) return ("");
				if (desc) return ("Recover from stuns, wounds, poison, disease, and fear.  Usually needs to be repeated several times; assign this prayer to a macro.");
				if (cast)
				{
					(void)set_stun(p_ptr->stun - 4);
					(void)set_cut(p_ptr->cut - 6);
					(void)set_poisoned(p_ptr->poisoned - 6);
					(void)set_diseased(p_ptr->diseased - 2, NULL);
					(void)set_afraid(p_ptr->afraid - 8);
				}
				break;
			}

			/* Portal */
			case 90:
			{
				pow = spower;

				if (info) return (format("dist %d", pow));
				if (desc) return ("Medium range random teleportation.");
				if (cast)
				{
					teleport_player(pow, TRUE, FALSE);
				}
				break;
			}

			/* Heal */
			case 91:
			{
				pow = 40;

				if (info) return (format("heal %d%%", pow));
				if (desc) return ("A large amount of healing, eliminates cuts and stunning.");
				if (cast)
				{
					(void)heal_player(pow, pow * 3);
					(void)set_stun(0);
					(void)set_cut(0);
				}
				break;
			}

			/* Glyph of Warding */
			case 92:
			{
				if (info) return (format("glyphs: %d out of %d",
					num_glyph_on_level, MAX_GLYPHS));
				if (desc) return (format("Places a glyph on the floor that monsters cannot pass over or be summoned on until broken.  You may set a maximum of %d glyphs on a level.  Glyphs can be desanctified by disarming.", MAX_GLYPHS));
				if (cast)
				{
					warding_glyph(py, px);
				}
				break;
			}

			/* Holy Word */
			case 93:
			{
				dam1 = 25 + 2 * xtra_spower;
				dam2 = 2 * spower;

				if (info) return (format("dam %d-%d, heal 500", dam1, dam2));
				if (desc) return ("Strong dispel evil, healing, and remove poisoning, fear, stunning, and cuts.");
				if (cast)
				{
					(void)dispel_evil(rand_range(dam1, dam2));
					(void)hp_player(500);
					(void)set_afraid(0);
					(void)set_poisoned(0);
					(void)set_diseased(p_ptr->diseased / 2 - 10, NULL);
					(void)set_stun(0);
					(void)set_cut(0);
				}
				break;
			}

			/* Unbarring Ways */
			case 94:
			{
				if (info) return ("");
				if (desc) return ("Destroys all doors next to you.");
				if (cast)
				{
					(void)destroy_doors_touch();
				}
				break;
			}

			/* Untrapping Chant */
			case 95:
			{
				if (info) return ("");
				if (desc) return ("Attempts to disarm a trap adjacent to you.");
				if (cast)
				{
					if (!get_aim_dir(&dir)) return (NULL);
					(void)disarm_trap(dir);
				}
				break;
			}

			/* Eyes Undeceivable */
			case 96:
			{
				dur1 = (p_ptr->detect_inv ?  1 : 32);
				dur2 = (p_ptr->detect_inv ? 16 : 48);
				if (p_ptr->detect_inv) extra = "longer";

				if (info) return (format("dur %d-%d %s", dur1, dur2, extra));
				if (desc) return ("Reveals mimics in line of sight, detects all nearby monsters, grants temporary see invisible.");
				if (cast)
				{
					eyes_undeceivable(rand_range(dur1, dur2));
				}
				break;
			}

			/* Unveil Traps */
			case 97:
			{
				if (info) return ("");
				if (desc) return ("Detects all nearby traps.");
				if (cast)
				{
					(void)detect_traps(FALSE, TRUE);
				}
				break;
			}

			/* Light Chambers */
			case 98:
			{
				if (info) return ("");
				if (desc) return ("Magically lights every non-vault room on the current level.");
				if (cast)
				{
					int x, y;

					for (y = 0; y < dungeon_hgt; y++)
					{
						for (x = 0; x < dungeon_wid; x++)
						{
							/* Light all room grids, but not vaults */
							if ((cave_info[y][x] & (CAVE_ROOM)) &&
							   !(cave_info[y][x] & (CAVE_ICKY)))
							{
								cave_info[y][x] |= (CAVE_GLOW);
							}
						}
					}

					/* Fully update the visuals */
					p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);
				}
				break;
			}

			/* Clairvoyance */
			case 99:
			{
				if (info) return ("");
				if (desc) return ("Permanently lights up the entire dungeon level, except for vaults, and detects everything nearby.");
				if (cast)
				{
					wiz_lite(FALSE, TRUE);
					(void)detect_all(FALSE, TRUE);
				}
				break;
			}

			/* Holy Lance */
			case 100:
			{
				dam = 5 * spower / 4;
				rad = 2 + spower / 25;

				if (info) return (format("dam %d, rad %d", dam, rad));
				if (desc) return ("Casts a short beam of holy force.");
				if (cast)
				{
					if (!get_aim_dir(&dir)) return (NULL);
					(void)fire_arc(GF_HOLY_ORB, dir, dam, rad, 0);
				}
				break;
			}

			/* Concentrate Light */
			case 101:
			{
				/* The (approximate) maximal damages with various radii */
				/* 5  => 108     6  =>  151     7  => 198     8  => 248 */

				rad = 5 + xtra_spower / 13;

				/* Check grids available to tap */
				dam = 5 * concentrate_light(-1, py, px, rad, dummy, sizeof(dummy), FALSE) / 4;

				if (info)
				{
					/* Hack -- Use the "simple" RNG */
					Rand_quick = TRUE;

					/* Seed it with the actual damage */
					Rand_value = dam;

					/* Do not give overly accurate damage ratings */
					dam = rand_spread(dam, dam / 8);

					/* Hack -- Use the "complex" RNG */
					Rand_quick = FALSE;

					/* Round off */
					dam = (int)round_it(dam, 8);

					return (format("dam ~%d", dam));
				}
				if (desc) return ("Concentrates light into a bolt.  The more light available nearby, the stronger the bolt will be.");
				if (cast)
				{
					/* Drain light */
					dam = concentrate_light(-1, py, px, rad, dummy, sizeof(dummy), TRUE);

					/* No effect */
					if (!dam)
					{
						msg_print("Your light bolt fizzles out.");
						break;
					}

					/* Message */
					msg_print("You concentrate light...");

					/* Must cast the bolt */
					if (!get_aim_dir(&dir)) dir = randint(9);
					msg_format("And release it %s", dummy);

					/* Fire a bolt of light */
					fire_bolt(GF_LITE, dir, dam);
				}
				break;
			}

			/* Banishment */
			case 102:
			{
				if (info) return ("");
				if (desc) return ("Teleports away evil, undead, and demonic monsters in line of sight.");
				if (cast)
				{
					(void)banishment((RF3_EVIL | RF3_UNDEAD | RF3_DEMON),
						spower);
				}

				break;
			}

			/* Judgment */
			case 103:
			{
				pow = 10 + 4 * spower / 5;

				if (info) return (format("power %d", pow));
				if (desc) return ("Attempts to drive all monsters in line of sight insane.");
				if (cast)
				{
					judgement(pow);
				}
				break;
			}

			/* Starburst */
			case 104:
			{
				dam = 2 * (spower - 10);
				rad = 4 + xtra_spower / 7;

				if (info) return (format("dam %d, rad ~%d", dam, rad));
				if (desc) return ("Creates a great starburst of light around you.");
				if (cast)
				{
					(void)fire_star(GF_LITE, 0, dam, rad);
				}
				break;
			}

			/* Resist Ethereal */
			case 105:
			{
				dur1 = (p_ptr->oppose_ethereal ?  1 : 36);
				dur2 = (p_ptr->oppose_ethereal ? 12 : 48);
				if (p_ptr->oppose_ethereal) extra = "longer";

				if (info) return (format("dur %d-%d %s", dur1, dur2, extra));
				if (desc) return ("Temporary resistance to light and darkness, and opposition to electricity.  Electricity opposition is cumulative with equipment resists.");
				if (cast)
				{
					(void)set_oppose_ethereal(p_ptr->oppose_ethereal +
						rand_range(dur1, dur2));
				}
				break;
			}

			/* Perception */
			case 106:
			{
				if (info) return ("");
				if (desc) return ("Standard identification of an object.");
				if (cast)
				{
					if (!ident_spell()) return (NULL);
				}
				break;
			}

			/* Self Knowledge */
			case 107:
			{
				if (info) return ("");
				if (desc) return ("Gives you detailed information about yourself.");
				if (cast)
				{
					/* No lingering effect */
					self_knowledge(TRUE);
				}
				break;
			}

			/* Dispel Curse */
			case 108:
			{
				if (info) return ("");
				if (desc) return ("Removes both normal and heavy curses.");
				if (cast)
				{
					int cnt = remove_all_curse();
					if (cnt)
					{
						msg_format("You glow brilliantly; %s broken!",
							cnt > 1 ? "several curses are" : "a curse is");
					}
				}
				break;
			}

			/* Holy Aura */
			case 109:
			{
				dur1 = (p_ptr->holy ?  1 : 45);
				dur2 = (p_ptr->holy ? 30 : 60);
				if (p_ptr->holy) extra = "longer";

				if (info) return (format("dur %d-%d %s", dur1, dur2, extra));
				if (desc) return ("Temporarily grants all the benefits of Blessing, also increases light radius and provides hold life.");
				if (cast)
				{
					(void)set_holy(p_ptr->holy + rand_range(dur1, dur2));
				}
				break;
			}

			/* Restoration */
			case 110:
			{
				if (info) return ("");
				if (desc) return ("Restores all stats.");
				if (cast)
				{
					(void)restore_stats();
				}
				break;
			}

			/* Remembrance */
			case 111:
			{
				if (info) return ("");
				if (desc) return ("Restores experience level.");
				if (cast)
				{
					(void)restore_level();
				}
				break;
			}

			/* Sacred Shield */
			case 112:
			{
				dur1 = (p_ptr->shield ?          1 : spower / 4);
				dur2 = (p_ptr->shield ? spower / 4 : spower / 2);
				if (p_ptr->shield) extra = "longer";

				if (info) return (format("dur %d-%d %s", dur1, dur2, extra));
				if (desc) return ("Temporarily increases armor class by 50.");
				if (cast)
				{
					(void)set_shield(p_ptr->shield + rand_range(dur1, dur2),
						"A divine shield surrounds you!");
				}
				break;
			}

			/* Life */
			case 113:
			{
				if (info) return ("heal 1000");
				if (desc) return ("An extremely strong healing spell.  Also removes cuts and stuns.");
				if (cast)
				{
					(void)hp_player(1000);
					(void)set_stun(0);
					(void)set_cut(0);
				}
				break;
			}

			/* Word of Destruction */
			case 114:
			{
				if (info) return ("");
				if (desc) return ("Destroys almost all objects nearby, deletes ordinary monsters, and banishes uniques from the level.");
				if (cast)
				{
					destroy_area(py, px, 15, TRUE, FALSE);
				}

				break;
			}

			/* Smite Evil */
			case 115:
			{
				dam1 = 3 * spower / 2;     dam2 = 2 * spower;

				if (info) return (format("dam %d-%d", dam1, dam2));
				if (desc) return ("Dispels an evil creature, then attempts to stun, slow, and confuse it.  It is possible (although not easy) to hinder even the most powerful of foes.");
				if (cast)
				{
					if (!get_aim_dir(&dir)) return (NULL);
					(void)smite_evil(dir, rand_range(dam1, dam2));
				}
				break;
			}

			/* Annihilation */
			case 116:
			{
				dam1 = 3 * spower / 2;     dam2 = dam1 + spower;

				if (info) return (format("dam %d-%d", dam1, dam2));
				if (desc) return ("Strikes at the soul of a living monster.");
				if (cast)
				{
					if (!get_aim_dir(&dir)) return (NULL);
					(void)drain_life(dir, rand_range(dam1, dam2));
				}
				break;
			}

			/* Call on Varda */
			case 117:
			{
				dam = 2 * spower;

				if (info) return (format("dam %d, heal 500", dam));
				if (desc) return ("Large ball of light centered on you, healing, and a powerful panic monsters spell.");
				if (cast)
				{
					message(MSG_YELLOW, 0, "Gilthoniel A Elbereth!");
					(void)fire_ball_special(GF_LITE, 0, dam, spower / 14 + 2, 0L, 20);
					(void)fear_monsters(spower);
					(void)hp_player(500);
				}
				break;
			}

			/*  */
			case 118:
			case 119:
			case 120:
			case 121:
			case 122:
			case 123:
			case 124:
			case 125:
			case 126:
			case 127:
			{
				break;
			}


/*** Druidic Lore ***/

			/* Lightning Spark */
			case 128:
			{
				/* This is the one druid spell that ignores weather */
				dice = 2 + spower / 16;     sides = 6;
				rad = 2 + (spower + 10) / 15;

				if (info) return (format("dam %dd%d, rad %d", dice, sides, rad));
				if (desc) return ("Short-range beam of lightning.  Not affected by weather.");
				if (cast)
				{
					p_ptr->max_dist = rad;
					if (!get_aim_dir(&dir)) return (NULL);
					(void)fire_arc(GF_ELEC, dir, damroll(dice, sides), rad, 0);
				}
				break;
			}

			/* Sense Life */
			case 129:
			{
				if (info) return ("");
				if (desc) return ("Detects all nearby living monsters, even invisible ones.  Demons and the undead are non-living.");
				if (cast)
				{
					(void)detect_life(FALSE, TRUE);
				}
				break;
			}

			/* Minor Healing */
			case 130:
			{
				pow = 5;

				if (info) return (format("heal %d%%", pow));
				if (desc) return ("Reduces cuts and heals you a little.");
				if (cast)
				{
					(void)heal_player(pow, pow * 2);
					(void)set_cut(p_ptr->cut - 5);
				}
				break;
			}

			/* Song of Morning */
			case 131:
			{
				dice = 2;     sides = spower / 4;

				if (info) return (format("dam %dd%d", dice, sides));
				if (desc) return ("Permanently lights up a room or the area lit by your light source.");
				if (cast)
				{
					(void)lite_area(damroll(dice, sides), MAX(2, p_ptr->cur_lite));
				}
				break;
			}

			/* Sustain Self */
			case 132:
			{
				if (info) return ("");
				if (desc) return ("Fully feeds you.");
				if (cast)
				{
					(void)set_food(p_ptr->food_bloated - 50);
				}
				break;
			}

			/* Blink */
			case 133:
			{
				if (info) return ("");
				if (desc) return ("Minor random displacement.");
				if (cast)
				{
					teleport_player(10, TRUE, FALSE);
				}
				break;
			}

			/* Regain Health */
			case 134:
			{
				pow = 7 + spower / 3;

				if (info) return ("");
				if (desc) return ("Reduce poison and disease.");
				if (cast)
				{
					(void)set_poisoned(p_ptr->poisoned - pow);
					(void)set_diseased(p_ptr->diseased - pow / 2, NULL);
				}
				break;
			}

			/* Frost Bolt */
			case 135:
			{
				dice = 2 + spower / 10;
				sides = (reliability > 4 ? 8 : 6);

				if (info) return (format("dam %dd%d", dice, sides));
				if (desc) return ("Fires a bolt or beam of frost.  Affected by weather.");
				if (cast)
				{
					if (!get_aim_dir(&dir)) return (NULL);

					dam = weather_dam(GF_COLD, damroll(dice, sides));
					if (!dam) msg_print("The ice melts in the heat!");

					else (void)fire_bolt_or_beam(beam - 10, GF_COLD, dir, dam);
				}
				break;
			}

			/* Ray of Sunlight */
			case 136:
			{
				if (info) return ("dam 4d5");
				if (desc) return ("Fires a line of weak light.  Affects light-hating creatures.");
				if (cast)
				{
					if (!get_aim_dir(&dir)) return (NULL);
					(void)lite_line(dir);
				}
				break;
			}

			/* Turn Stone to Mud */
			case 137:
			{
				dam1 = 10 + spower / 5;     dam2 = dam1 + 10 + spower / 5;

				if (info) return (format("dam %d-%d", dam1, dam2));
				if (desc) return ("Melts stone.");
				if (cast)
				{
					if (!get_aim_dir(&dir)) return (NULL);
					(void)wall_to_mud(dir, rand_range(dam1, dam2));
				}
				break;
			}

			/* Sleep Creature */
			case 138:
			{
				if (info) return ("");
				if (desc) return ("Attempts to put a monster to sleep.");
				if (cast)
				{
					if (!get_aim_dir(&dir)) return (NULL);
					(void)sleep_monster(dir, 30 + 2 * spower / 3);
				}
				break;
			}

			/* Counter-Poison */
			case 139:
			{
				dur1 = p_ptr->oppose_pois ?         1 : spower / 4;
				dur2 = p_ptr->oppose_pois? spower / 4 : spower / 2;

				if (info) return (format("dur %d-%d%s", dur1, dur2,
					p_ptr->oppose_pois ? " longer" : ""));
				if (desc) return ("Purges all poison from your system and temporarily renders you resistant to it.  Resistance is in addition to equipment resists.");
				if (cast)
				{
					(void)set_poisoned(0);
					(void)set_oppose_pois(p_ptr->oppose_pois +
						rand_range(dur1, dur2));
				}
				break;
			}

			/* Detect Traps/Doors */
			case 140:
			{
				if (info) return ("");
				if (desc) return ("Detects nearby traps, doors, and stairs.");
				if (cast)
				{
					(void)detect_traps(FALSE, TRUE);
					(void)detect_doors(FALSE);
					(void)detect_stairs(FALSE);
				}
				break;
			}

			/* Snuff Small Life */
			case 141:
			{
				dam = 5 + spower / 4;

				if (info) return (format("dam %d", dam));
				if (desc) return ("Kills all weak monsters in line of sight; spell has no effect on stronger creatures.");
				if (cast)
				{
					(void)dispel_small_monsters(dam);
				}
				break;
			}

			/* Ignite Blaze */
			case 142:
			{
				dam = spower / 2;

				if (info) return (format("dam %d", dam));
				if (desc) return ("Creates a fire that dances about, burning everything it touches.  The hotter the weather is, the fiercer the blaze.  The stronger the wind, the faster it moves.  In open places, it actively seeks out monsters.");
				if (cast)
				{
					/* Get a new effect index */
					i = effect_prep();

					/* Note failure XXX */
					if (i < 0) break;

					/* We want a patch, */
					x_list[i].index = EFFECT_SEEKER_VORTEX;

					/* Of fire */
					x_list[i].type = GF_FIRE;

					/* That starts at the character location. */
					x_list[i].y0 = py;
					x_list[i].x0 = px;

					/* Practices the spellcasting skill */
					x_list[i].practice_skill = S_MAGIC;

					/* Moves with a speed that depends on the wind, */
					if (TRUE)
					{
						/* Delay ranges from 14 down to 3.5 */
						int delay = MAX_WEATHER - p_ptr->wind;
						if (delay < MAX_WEATHER)
							delay = div_round((delay + MAX_WEATHER), 2);

						x_list[i].time_delay = delay;
					}

					/* Does damage, */
					x_list[i].power = weather_dam(GF_FIRE, dam);

					/* Created by the character and capable of hurting him, */
					x_list[i].flags |= (EF1_HURT_PLAY | EF1_CHARACTER);

					/* Is self-illuminating, */
					x_list[i].flags |= (EF1_SHINING);

					/* And lasts for a certain period of time. */
					x_list[i].lifespan = (50 + spower / 4) /
					                     x_list[i].time_delay;
				}
				break;
			}

			/* Change Weather */
			case 143:
			{
				/* Usually less accurate than the talent */
				int accur = get_skill(S_NATURE, 20, 180) - MIN(100, p_ptr->depth);
				if (accur > 100) accur = 100;

				if (info) return (format("accuracy %d%%", accur));
				if (desc) return ("Randomly change weather, then report it.  Accuracy of weather report improves with Nature Lore skill, but is usually less then that of your special talent.");
				if (cast)
				{
					/* Change weather randomly, then predict it */
					if (change_weather(0, 0, 0))
					{
						msg_print("The weather has changed.");
						predict_weather(accur);
					}
				}
				break;
			}

			/* Form of the Goat */
			case 144:
			{
				if (info) return ("");
				if (desc) return ("Turns you into a goat.  Goats can eat almost anything.");
				if (cast)
				{
					do_shapechange = SHAPE_GOAT;
				}
				break;
			}

			/* Sense Area */
			case 145:
			{
				if (info) return ("");
				if (desc) return ("Maps the local area, reveals doors and stairs.");
				if (cast)
				{
					(void)map_area(0, 0, FALSE);
				}
				break;
			}

			/* Poison Bolt */
			case 146:
			{
				dice = 5 + spower / 8;     sides = 8;

				if (info) return (format("dam %dd%d", dice, sides));
				if (desc) return ("Fires a bolt of poison.  Affected by weather.");
				if (cast)
				{
					if (!get_aim_dir(&dir)) return (NULL);

					dam = weather_dam(GF_POIS, damroll(dice, sides));
					if (!dam) msg_print("The vapors blow away in the wind!");

					else (void)fire_bolt(GF_POIS, dir, dam);
				}
				break;
			}

			/* Resist Elements */
			case 147:
			{
				dur2 = 30 + reliability / 4;  dur1 = dur2 - 15;

				if (info) return (format("dur %d-%d", dur1, dur2));
				if (desc) return ("Temporary opposition to the four elements, cumulative with equipment resists.");
				if (cast)
				{
					dur = rand_range(dur1, dur2);

					if ((p_ptr->oppose_acid) || (p_ptr->oppose_elec) ||
					    (p_ptr->oppose_fire) || (p_ptr->oppose_cold))
					{
						dur /= 2;
					}

					(void)set_oppose_acid(p_ptr->oppose_acid + dur);
					(void)set_oppose_elec(p_ptr->oppose_elec + dur);
					(void)set_oppose_fire(p_ptr->oppose_fire + dur);
					(void)set_oppose_cold(p_ptr->oppose_cold + dur);
				}
				break;
			}

			/* Acid Blast */
			case 148:
			{
				dam = spower - 5;
				rad = 5 + xtra_spower / 20;

				if (info) return (format("dam %d, rad %d", dam, rad));
				if (desc) return ("Fires an arc of acid.  Affected by weather.");
				if (cast)
				{
					if (!get_aim_dir(&dir)) return (NULL);

					dam = weather_dam(GF_ACID, dam);
					if (!dam) msg_print("It is bone-dry; acid requires water!");

					else (void)fire_arc(GF_ACID, dir, dam, rad, 45);
				}
				break;
			}

			/* Wither Foe */
			case 149:
			{
				TARGET_DECLARE
				dice = 1 + spower / 12;     sides = 8;

				if (info) return ("");
				if (desc) return ("Hurts a monster, then attempts to slow and confuse it.");
				if (cast)
				{
					if (!get_aim_dir(&dir)) return (NULL);

					TARGET_PRESERVE
					fire_bolt(GF_MANA, dir, damroll(dice, sides));

					/* Only if specific target monster not dead */
					if (!save_target_set || p_ptr->target_set)
					{
						p_ptr->proj_temp_flags = PROJECT_HIDE;
						(void)confuse_monster(dir, 15 + 3 * spower / 5);
						p_ptr->proj_temp_flags = PROJECT_HIDE;
						(void)slow_monster(dir, 15 + 3 * spower / 5);
					}
				}
				break;
			}

			/* Blizzard */
			case 150:
			{
				dam1 = (spower - 15);
				dam2 = 2 * (spower - 15);

				if (info) return (format("dam %d-%d", dam1, dam2));
				if (desc) return ("Casts a ball of frost.  Affected by weather.");
				if (mana)
				{
					/* Mana cost increases (slowly) with power */
					mana_cost = 10 + xtra_spower / 10;
					return ("");
				}
				if (cast)
				{
					if (!get_aim_dir(&dir)) return (NULL);

					dam = rand_range(dam1, dam2);
					dam = weather_dam(GF_COLD, dam);
					if (!dam) msg_print("The snow melts!");

					else (void)fire_ball(GF_COLD, dir, dam, 1);
				}
				break;
			}

			/* Fireball */
			case 151:
			{
				dam1 = (spower - 15);
				dam2 = 2 * (spower - 15);

				if (info) return (format("dam %d-%d", dam1, dam2));
				if (desc) return ("Casts a ball of fire.  Affected by weather.");
				if (mana)
				{
					/* Mana cost increases (slowly) with power */
					mana_cost = 10 + (xtra_spower+2) / 10;
					return ("");
				}
				if (cast)
				{
					if (!get_aim_dir(&dir)) return (NULL);

					dam = rand_range(dam1, dam2);
					dam = weather_dam(GF_FIRE, dam);
					if (!dam) msg_print("The fire is extinguished!");

					else (void)fire_ball(GF_FIRE, dir, dam, 1);
				}
				break;
			}

			/* Form of the Bear */
			case 152:
			{
				if (info) return ("");
				if (desc) return ("Turns you into a bear, tough and strong.");
				if (cast)
				{
					do_shapechange = SHAPE_BEAR;
				}
				break;
			}

			/* Natural Vitality */
			case 153:
			{
				pow = spower / 10;

				if (info) return (format("heal 2d%d, pois/cut", pow));
				if (desc) return ("Recovers from wounds, poison, disease, and physical damage far more rapidly than normal resting.  This is a good spell to assign to a macro.");
				if (cast)
				{
					(void)hp_player(damroll(2, pow));
					(void)set_poisoned((3 * p_ptr->poisoned / 4) - 5);
					(void)set_diseased(p_ptr->diseased - 2, NULL);
					(void)set_cut(p_ptr->cut - spower / 4);
				}
				break;
			}

			/* Chain Lightning */
			case 154:
			{
				dam = 2 * spower / 3;
				rad = 8;

				if (info) return (format("dam %d, rad %d", dam, rad));
				if (desc) return ("When cast in a direction, fires a bolt of electricity (for double damage).  When cast at yourself, calls up a storm of lightning centered on you.  Storms are much more effective, but also somewhat more costly, when cast out in the open.  Affected by weather.");
				if (mana)
				{
					/* Mana cost depends on area covered. */
					calc_mana_aux(15, 25, rad);
					return ("");
				}
				if (cast)
				{
					dam = weather_dam(GF_ELEC, dam);

					/* Get a target */
					if (!get_aim_dir(&dir)) return (NULL);

					/* Cast a storm */
					if ((dir == 5) && (p_ptr->target_row == p_ptr->py)
					               && (p_ptr->target_col == p_ptr->px))
					{
						fire_storm(-1, GF_ELEC, py, px, dam, rad, 4, 1, FALSE);
					}

					/* Cast a bolt (always costs 15 mana) */
					else
					{
						fire_bolt_beam_special(GF_ELEC, dir, dam*2, rad, 0L);
						spell_cost = 15;
					}
				}
				break;
			}

			/* Detect All */
			case 155:
			{
				if (info) return ("");
				if (desc) return ("Detects all nearby monsters, traps, doors, stairs, gold seams, and objects.");
				if (cast)
				{
					(void)detect_all(FALSE, TRUE);
				}
				break;
			}

			/* Identify */
			case 156:
			{
				if (info) return ("");
				if (desc) return ("Standard identification of an object.");
				if (cast)
				{
					if (!ident_spell()) return (NULL);
				}
				break;
			}

			/* Thunderclap */
			case 157:
			{
				dam1 = 5 * spower / 4;
				dam2 = 5 * spower / 2;

				if (info) return (format("dam %d-%d", dam1, dam2));
				if (desc) return ("Casts a very large sound ball centered on you.");
				if (cast)
				{
					msg_print("Boom!");
					(void)fire_ball_special(GF_SOUND, 0, rand_range(dam1, dam2),
						2 + spower / 20, 0L, 20);
				}
				break;
			}

			/* Form of the Mouse */
			case 158:
			{
				if (info) return ("");
				if (desc) return ("Turns you into a silent mouse.");
				if (cast)
				{
					do_shapechange = SHAPE_MOUSE;
				}
				break;
			}

			/* Form of the Hound */
			case 159:
			{
				if (info) return ("");
				if (desc) return ("Turns you into a hound, always aware of its surroundings.");
				if (cast)
				{
					do_shapechange = SHAPE_HOUND;
				}
				break;
			}

			/* Form of the Cheetah */
			case 160:
			{
				if (info) return ("");
				if (desc) return ("Turns you into a speedy cheetah.");
				if (cast)
				{
					do_shapechange = SHAPE_CHEETAH;
				}
				break;
			}

			/* Form of the Lion */
			case 161:
			{
				if (info) return ("");
				if (desc) return ("Turns you into a dauntless lion, master of unarmed combat.");
				if (cast)
				{
					do_shapechange = SHAPE_LION;
				}
				break;
			}

			/* Form of the Dragon */
			case 162:
			{
				if (info) return ("");
				if (desc) return ("Turns you into a mighty dragon, whose breath is death.");
				if (cast)
				{
					do_shapechange = SHAPE_DRAGON;
				}
				break;
			}

			/* Hold Weather */
			case 163:
			{
				pow1 = 60;     pow2 = 90;

				if (info) return (format("dur %d-%d", pow1, pow2));
				if (desc) return ("Temporarily fixes current weather patterns in place (not cumulative).");
				if (cast)
				{
					/* Cancel previous weather hold, hold weather */
					(void)set_hold_weather(rand_range(pow1, pow2));
				}
				if (fail)
				{
					msg_print("The weather changes over and over again!");
					for (i = 0; i < 10; i++) (void)change_weather(0, 0, 0);
				}

				break;
			}

			/* Summer's Gaze */
			case 164:
			{
				if (info) return ("");
				if (desc) return ("Increases temperature.");
				if (cast)
				{
					if (change_weather(0, 0, 1))
					msg_print("The air grows warmer.");
				}
				if (fail)
				{
					msg_print("The weather changes unpredictably!");
					(void)change_weather(0, 0, 0);
				}
				break;
			}

			/* Winter's Grip */
			case 165:
			{
				if (info) return ("");
				if (desc) return ("Decreases temperature.");
				if (cast)
				{
					if (change_weather(0, 0, -1))
					msg_print("You feel the air grow cooler.");
				}
				if (fail)
				{
					msg_print("The weather changes unpredictably!");
					(void)change_weather(0, 0, 0);
				}
				break;
			}

			/* Drought */
			case 166:
			{
				if (info) return ("");
				if (desc) return ("Decreases humidity.");
				if (cast)
				{
					if (change_weather(-1, 0, 0))
					msg_print("The air seems drier.");
				}
				if (fail)
				{
					msg_print("The weather changes unpredictably!");
					(void)change_weather(0, 0, 0);
				}
				break;
			}

			/* Deluge */
			case 167:
			{
				if (info) return ("");
				if (desc) return ("Increases humidity.");
				if (cast)
				{
					if (change_weather(1, 0, 0))
					msg_print("The air seems damper.");
				}
				if (fail)
				{
					msg_print("The weather changes unpredictably!");
					(void)change_weather(0, 0, 0);
				}
				break;
			}

			/* Wind Songs */
			case 168:
			{
				if (info) return ("");
				if (desc) return ("Increases windiness.");
				if (cast)
				{
					if (change_weather(0, 1, 0))
					msg_print("You feel the wind pick up.");
				}
				if (fail)
				{
					msg_print("The weather changes unpredictably!");
					(void)change_weather(0, 0, 0);
				}
				break;
			}

			/* Still Winds */
			case 169:
			{
				if (info) return ("");
				if (desc) return ("Decreases windiness.");
				if (cast)
				{
					if (change_weather(0, -1, 0))
					msg_print("You feel the wind die down.");
				}
				if (fail)
				{
					msg_print("The weather changes unpredictably!");
					(void)change_weather(0, 0, 0);
				}
				break;
			}

			/* Ice Storm */
			case 170:
			{
				dam1 = 3 * (spower - 20) - 80;
				dam2 = 3 * (spower - 20);

				if (info) return (format("dam %d-%d", dam1, dam2));
				if (desc) return ("Casts a large explosion of cold.  Affected by weather.");
				if (cast)
				{
					if (!get_aim_dir(&dir)) return (NULL);

					dam = weather_dam(GF_COLD, rand_range(dam1, dam2));
					if (!dam) msg_print("The heat cancels your attack!");

					else (void)fire_ball_special(GF_COLD, dir, dam, 3,
						PROJECT_NO_TRAIL, 0);
				}
				break;
			}

			/* Essence of Flame */
			case 171:
			{
				dam1 = 3 * (spower - 20) - 80;
				dam2 = 3 * (spower - 20);

				if (info) return (format("dam %d-%d", dam1, dam2));
				if (desc) return ("Casts a large explosion of fire.  Affected by weather.");
				if (cast)
				{
					if (!get_aim_dir(&dir)) return (NULL);

					dam = weather_dam(GF_FIRE, rand_range(dam1, dam2));
					if (!dam) msg_print("The flames are extinguished!");

					else (void)fire_ball_special(GF_FIRE, dir, dam, 3,
						PROJECT_NO_TRAIL, 0);
				}
				break;
			}

			/* Lethal Plague */
			case 172:
			{
				dam1 = 3 * (spower - 20) - 80;
				dam2 = 3 * (spower - 20);

				if (info) return (format("dam %d-%d", dam1, dam2));
				if (desc) return ("Casts a large explosion of poison.  Affected by weather.");
				if (cast)
				{
					if (!get_aim_dir(&dir)) return (NULL);

					dam = weather_dam(GF_POIS, rand_range(dam1, dam2));
					if (!dam) msg_print("The wind wafts the plague away!");

					else (void)fire_ball_special(GF_POIS, dir, dam, 3,
						PROJECT_NO_TRAIL, 0);
				}
				break;
			}

			/* Cyclone */
			case 173:
			{
				dam1 = 5 * (spower - 20) / 2 - 80;
				dam2 = 5 * (spower - 20) / 2;

				if (info) return (format("dam %d-%d", dam1, dam2));
				if (desc) return ("Casts a large whirlwind.  Affected by weather.");
				if (cast)
				{
					if (!get_aim_dir(&dir)) return (NULL);

					dam = weather_dam(GF_WIND, rand_range(dam1, dam2));
					if (!dam) msg_print("The winds die away almost instantly!");

					else (void)fire_ball_special(GF_WIND, dir, dam, 3,
						PROJECT_NO_TRAIL, 0);
				}
				break;
			}

			/* Hurricane of Acid */
			case 174:
			{
				dam1 = 3 * (spower - 20) - 80;
				dam2 = 3 * (spower - 20);

				if (info) return (format("dam %d-%d", dam1, dam2));
				if (desc) return ("Casts a large explosion of acid.  Affected by weather.");
				if (cast)
				{
					if (!get_aim_dir(&dir)) return (NULL);

					dam = weather_dam(GF_ACID, rand_range(dam1, dam2));
					if (!dam) msg_print("It is bone-dry; acid needs water!");

					else (void)fire_ball_special(GF_ACID, dir, dam, 3,
						PROJECT_NO_TRAIL, 0);
				}
				break;
			}

			/* Thunder Strike */
			case 175:
			{
				dam1 = 3 * (spower - 20) - 80;
				dam2 = 3 * (spower - 20);

				if (info) return (format("dam %d-%d", dam1, dam2));
				if (desc) return ("Casts a large explosion of lightning.  Affected by weather.");
				if (cast)
				{
					if (!get_aim_dir(&dir)) return (NULL);

					dam = weather_dam(GF_ELEC, rand_range(dam1, dam2));
					if (!dam) msg_print("The lightning arcs away!");

					else (void)fire_ball_special(GF_ELEC, dir, dam, 3,
						PROJECT_NO_TRAIL, 0);
				}
				break;
			}

			/* Song of Protection */
			case 176:
			{
				dur1 = (p_ptr->shield ?  1 : 15);
				dur2 = (p_ptr->shield ? 15 : 30);
				if (p_ptr->shield) extra = "longer";

				if (info) return (format("dur %d-%d %s", dur1, dur2, extra));
				if (desc) return ("Temporarily increases armor class by 50.");
				if (cast)
				{
					(void)set_shield(p_ptr->shield + rand_range(dur1, dur2),
						"Your song creates a mystic shield.");
				}
				break;
			}

			/* Essence of Speed */
			case 177:
			{
				dur1 = (p_ptr->fast ?  1 : 15);
				dur2 = (p_ptr->fast ?  5 : 30);
				if (p_ptr->fast) extra = "longer";

				if (info) return (format("dur %d-%d %s", dur1, dur2, extra));
				if (desc) return ("Temporarily hastens you.");
				if (cast)
				{
					(void)set_fast(p_ptr->fast + rand_range(dur1, dur2));
				}
				break;
			}

			/* Herbal Healing */
			case 178:
			{
				pow1 = spower * 3;   pow2 = spower * 4;

				if (info) return (format("heal %d~%d", pow1, pow2));
				if (desc) return ("Healing, plus removal of all cuts, poison, and disease.");
				if (cast)
				{
					(void)hp_player(rand_range(pow1, pow2));
					(void)set_cut(0);
					(void)set_poisoned(0);
					(void)set_poisoned(0);
				}
				break;
			}

			/* Create Athelas */
			case 179:
			{
				if (info) return ("");
				if (desc) return ("Creates Athelas.");
				if (cast)
				{
					create_athelas();
				}
				break;
			}

			/* Form of the Ent */
			case 180:
			{
				if (info) return ("");
				if (desc) return ("Turns you into a wise and powerful Ent.  Use potions or spells to protect yourself from fire!");
				if (cast)
				{
					do_shapechange = SHAPE_ENT;
				}
				break;
			}

			/* Intervention of Yavanna */
			case 181:
			{
				if (info) return ("dam 150, heal 500");
				if (desc) return ("Dispels evil monsters, heals you, blesses you, and removes fear, poisoning, stunning, and cuts.");
				if (cast)
				{
					(void)dispel_evil(150);
					(void)hp_player(500);
					(void)set_blessed(p_ptr->blessed + rand_range(100, 200), NULL);
					(void)set_afraid(0);
					(void)set_poisoned(0);
					(void)set_stun(0);
					(void)set_cut(0);
				}
				break;
			}

			/*  */
			case 182:
			case 183:
			case 184:
			case 185:
			case 186:
			case 187:
			case 188:
			case 189:
			case 190:
			case 191:


/*** Necromantic Curses ***/

			/* Magic Bolt */
			case 192:
			{
				dice = 2;     sides = 5 + spower / 7;

				if (info) return (format("dam %dd%d", dice, sides));
				if (desc) return ("Fires a mana bolt.");
				if (cast)
				{
					if (!get_aim_dir(&dir)) return (NULL);
					(void)fire_bolt(GF_MANA, dir, damroll(dice, sides));
				}
				break;
			}

			/* Detect Evil */
			case 193:
			{
				if (info) return ("");
				if (desc) return ("Detects all nearby evil monsters, even invisible ones.");
				if (cast)
				{
					(void)detect_evil(FALSE, TRUE);
				}
				break;
			}

			/* Sense Blood */
			case 194:
			{
				dur1 = (p_ptr->tim_infra ?  1 : 50);
				dur2 = (p_ptr->tim_infra ? 50 : 75);
				if (p_ptr->tim_infra) extra = "longer";

				if (info) return (format("dur %d-%d %s", dur1, dur2, extra));
				if (desc) return ("Increases the range at which you can see warm-blooded creatures by 5.");
				if (cast)
				{
					(void)set_tim_infra(p_ptr->tim_infra +
						rand_range(dur1, dur2));
				}
				break;
			}

			/* Break Curse */
			case 195:
			{
				if (info) return ("");
				if (desc) return ("Removes ordinary curses.");
				if (cast)
				{
					if (remove_curse())
						msg_print("You feel mighty hands aiding you.");
				}
				break;
			}

			/* Black Blessing */
			case 196:
			{
				dur1 = (p_ptr->blessed ?  0 : 12);
				dur2 = (p_ptr->blessed ? 12 : 24);
				if (p_ptr->blessed) extra = "longer";

				if (info) return (format("dur %d-%d %s", dur1, dur2, extra));
				if (desc) return ("Short-duration bonus to fighting ability and armor class.");
				if (cast)
				{
					(void)set_blessed(p_ptr->blessed + rand_range(dur1, dur2),
						"You feel yourself grow stronger!");
				}
				break;
			}

			/* Malediction */
			case 197:
			{
				if (info) return ("");
				if (desc) return ("Curses a monster; can slow, daze, confuse, or panic it, or strip away various temporary bonuses (like haste spells).");
				if (cast)
				{
					if (!get_aim_dir(&dir)) return (NULL);
					(void)curse_monster(dir, 35 + 3 * spower / 4);
				}
				break;
			}

			/* Horrify */
			case 198:
			{
				if (info) return ("");
				if (desc) return ("Attempts to panic a monster.");
				if (cast)
				{
					if (!get_aim_dir(&dir)) return (NULL);
					(void)fear_monster(dir, 30 + 2 * spower / 3);
				}
				break;
			}

			/* Become Bat */
			case 199:
			{
				if (info) return ("");
				if (desc) return ("Turns you into a fast-fluttering bat.");
				if (cast)
				{
					do_shapechange = SHAPE_BAT;
				}
				break;
			}

			/* Dark Bolt */
			case 200:
			{
				dice = 2 + spower / 15;     sides = 8;

				if (info) return (format("dam %dd%d", dice, sides));
				if (desc) return ("Fires a bolt or beam of darkness.");
				if (cast)
				{
					if (!get_aim_dir(&dir)) return (NULL);
					(void)fire_bolt_or_beam(beam - 15, GF_DARK, dir,
						damroll(dice, sides));
				}
				break;
			}

			/* Noxious Fumes */
			case 201:
			{
				dam = 8 + (spower / 10) + (MIN(6, p_ptr->pois_power) * 5);
				rad = 2 + p_ptr->pois_power / 2;
				dur = MIN(p_ptr->pois_power_dur + 2, 9);

				if (info) return (format("dam %d, rad %d", dam, rad));
				if (desc) return ("Surrounds you with a cloud of poison.  Grows more powerful with repeated castings.");
				if (cast)
				{
					/* Allow a little weakening near the edges of the cloud */
					(void)fire_ball_special(GF_POIS, 0, dam, rad, 0L, 10 + rad * 5);

					/* Increase poison power */
					(void)set_pois_power(p_ptr->pois_power + 1, dur);

					/* Poisoning -- increases with spell strength */
					if (!(p_ptr->resist_pois || p_ptr->oppose_pois))
					{
						(void)set_poisoned(p_ptr->poisoned +
							randint(p_ptr->pois_power * 2));
					}

					/* This spell really wants to poison you */
					else if (!(p_ptr->resist_pois && p_ptr->oppose_pois))
					{
						(void)set_poisoned(p_ptr->poisoned +
							randint(p_ptr->pois_power / 3));
					}
				}
				break;
			}

			/* Cure Poison */
			case 202:
			{
				if (info) return ("");
				if (desc) return ("Removes all poison from your system.");
				if (cast)
				{
					(void)set_poisoned(0);
				}
				break;
			}

			/* Invoke Spirits */
			case 203:
			{
				if (info) return ("rng ~5");
				if (desc) return ("Calls dark spirits to attack a nearby monster or, if no monster is in sight, aid you in various ways.  Be warned!  The spirits are powerful but capricious; only one skilled in Blood Dominion can rely very much on them.");
				if (cast)
				{
					invoke_spirits(reliability);
				}
				break;
			}

			/* Dispel Evil */
			case 204:
			{
				dam1 = 3 * xtra_spower / 4;
				dam2 = 3 * spower / 4;

				if (info) return (format("dam %d-%d", dam1, dam2));
				if (desc) return ("Dispels all evil creatures in line of sight.");
				if (cast)
				{
					(void)dispel_evil(rand_range(dam1, dam2));
				}
				break;
			}

			/* Eyes of the Night */
			case 205:
			{
				dur1 = (p_ptr->detect_inv ?  1 : 16);
				dur2 = (p_ptr->detect_inv ? 12 : 32);
				if (p_ptr->detect_inv) extra = "longer";

				if (info) return (format("dur %d-%d %s", dur1, dur2, extra));
				if (desc) return ("Temporary see invisible.");
				if (cast)
				{
					(void)set_detect_inv(p_ptr->detect_inv +
						rand_range(dur1, dur2));
				}
				break;
			}

			/* Shadow Shifting */
			case 206:
			{
				if (info) return ("range 10");
				if (desc) return ("Random minor displacement.");
				if (cast)
				{
					teleport_player(10, TRUE, FALSE);
				}
				break;
			}

			/* Detect Traps */
			case 207:
			{
				if (info) return ("");
				if (desc) return ("Detects all nearby traps.");
				if (cast)
				{
					(void)detect_traps(FALSE, TRUE);
				}
				break;
			}

			/* Detect Doors/Stairs */
			case 208:
			{
				if (info) return ("");
				if (desc) return ("Detects all nearby doors and stairs.");
				if (cast)
				{
					(void)detect_doors(FALSE);
					(void)detect_stairs(FALSE);
				}
				break;
			}

			/* Death Claw */
			case 209:
			{
				dice = 2 + spower / 5;     sides = 8;

				if (info) return (format("dam %dd%d, rng 5", dice, sides));
				if (desc) return ("Fires a short-range bolt of death magic.  Becomes a beam with a high Blood Dominion skill.");
				if (cast)
				{
					if (!get_aim_dir(&dir)) return (NULL);
					(void)take_hit(damroll(2, 3), 0, "Your life is drained.",
						"the dark arts");

					/* Beams above 40 BD skill, always above 55 */
					if ((beam * 11) - 200 >= randint(100))
					{
						(void)fire_bolt_beam_special(GF_DEATH, dir,
							damroll(dice, sides), 5, PROJECT_BEAM);
					}
					else
					{
						(void)fire_bolt_beam_special(GF_DEATH, dir,
							damroll(dice, sides), 5, 0L);
					}
				}
				break;
			}

			/* Mental Awareness */
			case 210:
			{
				dur1 = 10 + spower / 5;     dur2 = 20 + spower / 5;

				if (info) return (format("dur %d-%d", dur1, dur2));
				if (desc) return ("Temporary telepathy.");
				if (cast)
				{
					set_tim_esp(p_ptr->tim_esp + rand_range(dur1, dur2));
				}
				break;
			}

			/* Mass Panic */
			case 211:
			{
				if (info) return ("");
				if (desc) return ("Attempts to panic all monsters in line of sight.");
				if (cast)
				{
					(void)fear_monsters(30 + 2 * spower / 3);
				}
				break;
			}

			/* Mists of Lethargy */
			case 212:
			{
				if (info) return ("");
				if (desc) return ("Attempts to slow all monsters in line of sight.");
				if (cast)
				{
					(void)slow_monsters(15 + 2 * spower / 3);
				}
				break;
			}

			/* Spear of Ice */
			case 213:
			{
				dam = spower - 10;

				if (info) return (format("dam %d", dam));
				if (desc) return ("Fires a spear of ice.");
				if (cast)
				{
					if (!get_aim_dir(&dir)) return (NULL);
					(void)fire_arc(GF_ICE, dir, dam, 0, 0);
				}
				break;
			}

			/* Doom Bolt */
			case 214:
			{
				dice = spower / 4;     sides = 8;

				if (info) return (format("dam %dd%d", dice, sides));
				if (desc) return ("Fires a heavy mana bolt.");
				if (cast)
				{
					if (!get_aim_dir(&dir)) return (NULL);
					(void)fire_bolt(GF_MANA, dir, damroll(dice, sides));
				}
				break;
			}

			/* Shadow Warping */
			case 215:
			{
				if (info) return ("");
				if (desc) return ("Random large-range displacement.");
				if (cast)
				{
					(void)take_hit(damroll(2, 6), 0, "Ouch!", "shadow dislocation");
					teleport_player(100, TRUE, FALSE);
				}
				break;
			}

			/* Feed off Light */
			case 216:
			{
				rad = 5 + xtra_spower / 20;

				if (info)
				{
					/* Check grids available to tap */
					pow = concentrate_light(-1, py, px, rad, dummy, sizeof(dummy), FALSE) * 50;

					/* Hack -- Use the "simple" RNG */
					Rand_quick = TRUE;

					/* Seed it with the actual power */
					Rand_value = pow;

					/* Do not give overly accurate power ratings */
					pow = rand_spread(pow, pow / 8);

					/* Round off */
					pow = (int)round_it(pow, 8);

					/* Hack -- Use the "complex" RNG */
					Rand_quick = FALSE;

					return (format("food ~%d", pow));
				}
				if (desc) return ("Drains away light, and turns it into sustenance.  The more light surrounds you, the more nutrition you will gain.");
				if (cast)
				{
					/* Drain light */
					pow = concentrate_light(-1, py, px, rad, dummy, sizeof(dummy), TRUE) * 50;

					/* Message */
					if (pow > 500)
						msg_print("You suck in light to feed yourself.");
					else
						msg_print("There was little or no light to feed on.");

					/* Add to food.  No protection against bloating. */
					set_food(p_ptr->food + pow);
				}
				break;
			}

			/* Dark Ball */
			case 217:
			{
				dam = 5 * (spower - 15) / 3;

				if (info) return (format("dam %d", dam));
				if (desc) return ("Casts a ball of darkness.");
				if (mana)
				{
					/* Mana cost increases (slowly) with power */
					mana_cost = 11 + xtra_spower / 13;
					return ("");
				}
				if (cast)
				{
					if (!get_aim_dir(&dir)) return (NULL);
					(void)fire_ball(GF_DARK, dir, dam, 1);
				}
				break;
			}

			/* Thaumacurse */
			case 218:
			{
				if (info) return ("");
				if (desc) return ("Taps into curses on equipped items to devastate nearby foes.  Each equipped cursed item (quiver doesn't count) boosts the spell's strength.  Light curses are one-use and broken, but heavy curses are a permanent source of power.");
				if (cast)
				{
					(void)thaumacurse(TRUE, (spower + spower * spower / 80));
				}
				break;
			}

			/* Exorcism */
			case 219:
			{
				dam1 = 2 * xtra_spower;
				dam2 = 2 * spower;

				if (info) return (format("dam %d-%d", dam1, dam2));
				if (desc) return ("Dispels all undead and demons, powerfully panics all evil monsters in line of sight.");
				if (cast)
				{
					(void)exorcise_monsters(rand_range(dam1, dam2));
				}
				break;
			}

			/* Mists of Death */
			case 220:
			{
				/* Note that the damage is done _every game turn_ */
				dam = 2 + spower / 10;
				dur = 3 * spower / 2;

				if (info) return (format("dam d%d/game turn, dur %d", dam, dur/10));
				if (desc) return ("Creates a cloud that attacks all living creatures near you every game turn.  If you move more than one grid at a time, you weaken the cloud, and it may even dissipate.");
				if (cast)
				{
					bool cloud_exists = FALSE;

					/* Try to find an existing cloud of death */
					for (i = 0; i < z_info->x_max; i++)
					{
						/* Require a cloud of death */
						if (x_list[i].index == EFFECT_DEATH_CLOUD)
						{
							msg_print("You renew the cloud of death surrounding you.");
							cloud_exists = TRUE;
							break;
						}
					}

					/* Get a new effect index if necessary */
					if (!cloud_exists)
					{
						i = effect_prep();

						/* Note failure XXX */
						if (i < 0) break;

						/* Message */
						msg_print("You cast a cloud of death.");
					}

					/* We want a cloud, */
					x_list[i].index = EFFECT_DEATH_CLOUD;

					/* Of death */
					x_list[i].type = GF_DEATH_CLOUD;

					/* That starts at the character location. */
					x_list[i].y0 = py;
					x_list[i].x0 = px;

					/* Practices the spellcasting skill */
					x_list[i].practice_skill = S_MAGIC;

					/* Moves every game turn, */
					x_list[i].time_delay = 0;

					/* Does damage, affects a fairly large area */
					x_list[i].power = dam;
					x_list[i].power2 = 1 + dam / 3;

					/* Created by the character, */
					x_list[i].flags |= (EF1_CHARACTER);

					/* And lasts for a certain period of time. */
					x_list[i].lifespan = dur;

					/* Allow clouds to be renewed */
					if (cloud_exists)
					{
						/* Renew cloud, but never allow age to go negative */
						x_list[i].age -= MIN(x_list[i].age, 20 + spower / 2);
					}
				}
				break;
			}

			/* Corpse Light */
			case 221:
			{
				if (info) return ("");
				if (desc) return ("Permanently lights up a room or the area lit by your light source.");
				if (cast)
				{
					(void)lite_area(0, MAX(2, p_ptr->cur_lite));
				}
				break;
			}

			/* Probing */
			case 222:
			{
				if (info) return ("");
				if (desc) return ("Learns most attributes of all monsters in sight.");
				if (cast)
				{
					(void)probing();
				}
				break;
			}

			/* Shadow Mapping */
			case 223:
			{
				if (info) return ("");
				if (desc) return ("Maps the immediate area, reveals doors and stairs.");
				if (cast)
				{
					map_area(0, 0, FALSE);
					(void)detect_doors(FALSE);
					(void)detect_stairs(FALSE);
				}
				break;
			}

			/* Interrogate */
			case 224:
			{
				if (info) return ("");
				if (desc) return ("Identifies an object.");
				if (cast)
				{
					if (!ident_spell()) return (NULL);
				}
				break;
			}

			/* Shadow Shield */
			case 225:
			{
				dur1 = (p_ptr->shield ?          1 : spower / 4);
				dur2 = (p_ptr->shield ? spower / 4 : spower / 2);
				if (p_ptr->shield) extra = "longer";

				if (info) return (format("dur %d-%d %s", dur1, dur2, extra));
				if (desc) return ("Temporarily increases armor class by 50.");
				if (cast)
				{
					(void)set_shield(p_ptr->shield + rand_range(dur1, dur2),
						"A dark shield forms around your body!");
				}
				break;
			}

			/* Wraithform */
			case 226:
			{
				dur1 = (p_ptr->wraithform ?          1 : spower / 2);
				dur2 = (p_ptr->wraithform ? spower / 4 : spower);
				if (p_ptr->wraithform) extra = "longer";

				if (info) return (format("dur %d-%d %s", dur1, dur2, extra));
				if (desc) return ("Enables you to go through walls.");
				if (cast)
				{
					(void)set_wraithform(p_ptr->wraithform +
						rand_range(dur1, dur2));
				}
				break;
			}

			/* Darkfire */
			case 227:
			{
				dam = 4 * spower / 3;

				if (info) return (format("dam %d", dam));
				if (desc) return ("Fires a large orb of necromantic fire.");
				if (cast)
				{
					if (!get_aim_dir(&dir)) return (NULL);
					(void)fire_orb(GF_HELLFIRE, dir, dam, 2);
				}
				break;
			}

			/* Vampiric Drain */
			case 228:
			{
				dam = 2 * spower / 3;

				if (info) return (format("dam %d, heal/feed", dam));
				if (desc) return ("Sucks life away from a warm-blooded, living monster to heal and feed you.");
				if (cast)
				{
					if (!get_aim_dir(&dir)) return (NULL);
					(void)fire_bolt(GF_VAMP_DRAIN, dir, dam);
				}
				break;
			}

			/* Blight Upon Nature */
			case 229:
			{
				dam1 = 2 * xtra_spower;
				dam2 = 2 * spower;

				if (info) return (format("dam %d-%d", dam1, dam2));
				if (desc) return ("Dispels all animals in line of sight.");
				if (cast)
				{
					(void)dispel_animals(rand_range(dam1, dam2));
				}
				break;
			}

			/* Smash Undead */
			case 230:
			{
				dam1 = 5 * xtra_spower / 2;
				dam2 = 5 * spower / 2;

				if (info) return (format("dam %d-%d", dam1, dam2));
				if (desc) return ("Dispels all undead in line of sight.");
				if (cast)
				{
					(void)dispel_undead(rand_range(dam1, dam2));
				}
				break;
			}

			/* Destroy Cavern */
			case 231:
			{
				if (info) return ("");
				if (desc) return ("Destroys almost all objects nearby, deletes ordinary monsters, and banishes uniques from the level.");
				if (cast)
				{
					(void)take_hit(damroll(6, 5), 0, "You are hit!",
						"incautious casting of the dark arts");
					destroy_area(py, px, 15, TRUE, FALSE);
				}
				break;
			}

			/* Annihilation */
			case 232:
			{
				dam = 3 * (spower - 10);

				if (info) return (format("dam %d", dam));
				if (desc) return ("Strikes at the soul of a living monster.");
				if (cast)
				{
					if (!get_aim_dir(&dir)) return (NULL);
					(void)drain_life(dir, dam);
				}
				break;
			}

			/* Mind over Body */
			case 233:
			{
				pow = get_heal_amount(20, 50);

				if (info) return (format("heal %d-%d", pow / 2, 3 * pow / 2));
				if (desc) return ("Mentally force your body to heal wounds and recover HPs.");
				if (cast)
				{
					if ((p_ptr->chp < p_ptr->mhp) || (p_ptr->cut))
					{
						msg_print("You force your body to heal.");
					}
					(void)hp_player(rand_range(pow / 2, 3 * pow / 2));
					(void)set_cut(p_ptr->cut - randint(50));
				}
				break;
			}

			/* Ward Against Evil */
			case 234:
			{
				dur1 = (p_ptr->protevil ?  1 : 30);
				dur2 = (p_ptr->protevil ? 20 : 40);
				if (p_ptr->protevil) extra = "longer";

				if (info) return (format("dur %d-%d %s", dur1, dur2, extra));
				if (desc) return ("Temporary protection from lesser evil creatures.");
				if (cast)
				{
					(void)set_protevil(p_ptr->protevil + rand_range(dur1, dur2));
				}
				break;
			}

			/* Necromantic Rage */
			case 235:
			{
				dur1 = (p_ptr->necro_rage ?  1 : 30);
				dur2 = (p_ptr->necro_rage ? 15 : 50);
				if (p_ptr->necro_rage) extra = "longer";

				if (info) return (format("dur %d-%d %s", dur1, dur2, extra));
				if (desc) return ("A raging Necromancer is terribly fast, feels no wounds, and heals unnaturally quickly.  However, no mana is recovered during the rage, and it fades away into great weakness and lassitude.");
				if (cast)
				{
					/* Allow for the weakness portion of the rage */
					dur = rand_range(dur1, dur2);
					if (p_ptr->necro_rage) dur += NECRO_WEAKNESS_LENGTH;

					set_necro_rage(p_ptr->necro_rage + dur);
				}
				break;
			}

			/* Dispel Curse */
			case 236:
			{
				if (info) return ("");
				if (desc) return ("Removes both normal and heavy curses.");
				if (cast)
				{
					(void)remove_all_curse();
				}
				break;
			}

			/* The Overmind */
			case 237:
			{
				pow = (p_ptr->mental_barrier ? 3 * spower / 2 : spower);

				if (info) return (format("power %d", pow));
				if (desc) return ("Attempts to smash the minds of all creatures in line of sight.  This spell is extremely risky, but also very powerful.  A high saving throw helps.");
				if (cast)
				{
					/* Message */
					msg_print("Mind-warping forces emanate from your brain!");

					/* Engage all viewable monsters in mental combat */
					project_los(p_ptr->py, p_ptr->px, pow, GF_PSI);
				}
				break;
			}

			/* Ritual of Blood */
			case 238:
			{
				if (info) return ("");   /* Deliberate lack of information */
				if (desc) return ("Trade hitpoints for mana.  Becomes more reliable with increases in Blood Dominion.");
				if (cast)
				{
					int diff = MIN(75, p_ptr->msp - p_ptr->csp);

					if (diff)
					{
						/* Recover some mana */
						pow = rand_range(MIN(reliability, diff), diff);
						(void)sp_player(pow, NULL);

						/* It hurts! */
						(void)take_hit(diff, 0,
							"Your lifeforce becomes magical power.",
							"the dark arts");
					}
				}
				break;
			}

			/* Mental Barrier */
			case 239:
			{
				dur1 = (p_ptr->mental_barrier ?  1 : 20);
				dur2 = (p_ptr->mental_barrier ? 20 : 40);
				if (p_ptr->mental_barrier) extra = "longer";

				if (info) return (format("dur %d-%d %s", dur1, dur2, extra));
				if (desc) return ("Increases your saving throw, makes the Overmind spell more powerful.");
				if (cast)
				{
					set_mental_barrier(p_ptr->mental_barrier +
						rand_range(dur1, dur2));
				}
				break;
			}

			/* Teleport Other */
			case 240:
			{
				if (info) return ("");
				if (desc) return ("Attempts to teleport a monster away.");
				if (cast)
				{
					if (!get_aim_dir(&dir)) return (NULL);
					(void)teleport_monster(dir);
				}
				break;
			}

			/* Forbid Summoning */
			case 241:
			{
				dur = spower / 4;

				if (info) return (format("dur %d", dur));
				if (desc) return ("Temporarily makes it much harder to summon monsters.");
				if (cast)
				{
					set_forbid_summoning(p_ptr->forbid_summoning + dur);
				}
				break;
			}

			/* Lich Powers */
			case 242:
			{
				dur1 = spower / 2;     dur2 = spower;

				if (info) return (format("dur %d-%d", dur1, dur2));
				if (desc) return ("Grants you many of the powers of liches, invisible undead who move with speed, but vulnerable to fire.");
				if (cast)
				{
					dur = rand_range(dur1, dur2);
					shapechange_temp(dur, SHAPE_LICH);
				}
				break;
			}

			/* Mass Genocide */
			case 243:
			{
				if (info) return ("");
				if (desc) return ("Deletes all monsters nearby, except uniques and special quest monsters.");
				if (cast)
				{
					(void)mass_genocide(py, px);
				}
				break;
			}

			/* Darkness Storm */
			case 244:
			{
				dam = 3 * (spower - 10);

				if (info) return (format("dam %d", dam));
				if (desc) return ("Casts a powerful storm of darkness.");
				if (cast)
				{
					if (!get_aim_dir(&dir)) return (NULL);
					(void)fire_ball_special(GF_DARK, dir, dam, 4,
						PROJECT_NO_TRAIL, 15);
				}
				break;
			}

			/* Mana Frenzy */
			case 245:
			{
				dam = 3 * spower / 2;
				rad = 20;

				if (info) return (format("dam %d each hit, rad %d", dam, rad));
				if (desc) return ("Invokes a storm of mana beams centered on you.  This spell is much more effective, but also somewhat more costly, when cast out in the open.");
				if (mana)
				{
					/* Mana cost depends on area covered. */
					calc_mana_aux(18, 30, rad);
					return ("");
				}
				if (cast)
				{
					/* Lots and lots of mana beams */
					fire_storm(-1, GF_MANA, py, px, dam, rad, 5, 1, FALSE);
				}
				break;
			}

			/*  */
			case 246:
			case 247:
			case 248:
			case 249:
			case 250:
			case 251:
			case 252:
			case 253:
			case 254:
			case 255:
			{
				break;
			}

			default:
			{
				msg_format("You have cast a spell (%d) that has not yet been defined.", s_ptr->index);
				cast = FALSE;
				break;
			}
		}
	}

	/* Did not cast a spell */
	if (!cast) return ("");


	/* If a spell was cast, take a turn */
	p_ptr->energy_use = 100;

	/* A spell was cast for the first time */
	if ((okay) && !(p_ptr->spell_flags[spell] & (PY_SPELL_WORKED)))
	{
		/* No longer gain experience (assign to spell level) - JM */
		/* gain_exp(MAX(1, s_ptr->sexp * s_ptr->slevel), S_MAGIC); */

		/* The spell worked */
		p_ptr->spell_flags[spell] |= (PY_SPELL_WORKED);

		/* Redraw object recall (later!) */
		p_ptr->window |= (PW_OBJECT);
	}

	/* Sufficient mana */
	if (spell_cost <= p_ptr->csp)
	{
		/* Use some mana */
		p_ptr->csp -= spell_cost;
	}

	/* Over-exert the player */
	else
	{
		int oops = spell_cost - p_ptr->csp;

		/* No mana left */
		p_ptr->csp = 0;
		p_ptr->csp_frac = 0;

		/* Message */
		if (p_ptr->realm == NECRO)
			msg_print("You collapse after the ritual!");
		else
			msg_print("You faint from the effort!");

		/* Paralyze the character (ignore Free Action) */
		if (oops > randint(15))
		{
			(void)set_paralyzed(p_ptr->paralyzed + randint(2 * oops + 1));
		}

		/* Damage CON (ignore sustain) */
		if (one_in_(2))
		{
			/* High-level spells may inflict permanent damage */
			bool perm = FALSE;
			if (s_ptr->slevel >= rand_range(40, 200)) perm = TRUE;

			/* Message */
			msg_print("You have damaged your health!");

			/* Reduce constitution */
			(void)dec_stat(A_CON, 1, perm);
		}
	}

	/* Shapechange after mana is deducted -JM */
	if (do_shapechange)
	{
		shapechange_perm(do_shapechange);
		p_ptr->schange_skill = mp_ptr->spell_skill;
		p_ptr->schange_min_skill = s_ptr->slevel;
	}


	/* Update spells */
	p_ptr->update |= (PU_SPELLS);

	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);

	/* Refresh the windows and update stuff */
	handle_stuff();

	/* Return */
	return ("");
}
