/* File: d11adds2.c
 *
 * Copyright (c) 2001 Stefan "Dunkelelf" Jurisch
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.
 *
 * Purpose: Additions for the special D11-Angband Version
 *
 * IMPORTANT NOTICE:
 * Some parts of the following source code are not really coded by myself!
 * I had to steel some parts of the original Vanilla Code and copy it into my own code
 * and then modified it to fit my own routines because some parts are not
 * coded as seperate routines and i did not get an entry point.
 * This is especially for the spell casting/praying.
 *
 * The Quick Spells / Spell memorizing is an idea of Björn Flender, who wanted
 * to fire some spells quick without pressing three key all time for his
 * standard spells. Thanks for that idea.
 */

#include "angband.h"


/*
 * Static Variables
 */

/* For the user commands menu */
char *user_commands_menu[7] =
{
	" 1) Term User                      ",
	" 2) Get single Line from Pref File ",
	" 3) Interact with Visuals          ",
	" 4) Interact with Colors           ",
	" 5) Load Screen Dump               ",
	" 6) Save Screen Dump               ",
	" 7) Identify Symbol                ",
};

/*
 * Configure and use Quick Spells
 */
extern void configure_quick_spells(void)
{
	int i;
	int item, spell, sval, choice;
	char in;
	char buf[80];
	cptr q, s;
	object_type *o_ptr;

	/* Save the Screen */
	screen_save();

	for (i = 1; i < 10; i++)
	{
		if (p_ptr->quick_spells[i] == -1)
			sprintf(buf, " %2d) %-30s ", i, "No Spell memorized ");
		else
		    sprintf(buf, " %2d) %-30s ", i, spell_names[cp_ptr->spell_type][p_ptr->quick_spells[i]]);
		c_put_str(TERM_WHITE, buf, i, 20);
	}
	c_put_str(TERM_WHITE, "Memorize which Spell? (1-9):", 0, 0);

	/* Key-Loop. do not end, as long as the key is not between a - f or ESC */
	while (TRUE)
	{
		in = inkey(); /* Wait for keypress */
		if ((in < '1' || in > '9' || in == '\0') && in != ESCAPE)
			continue;
		else
		    break;
		;
	}

	/* Restore Screen */
	screen_load();

	if (in == ESCAPE)
		return;

	choice = in - '1' + 1;

	/* Get an item */
	q = "Memorize Spell from which book? ";
	s = "You have no books that you can read.";
	if (!get_item(&item, q, s, (USE_INVEN))) return;

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}

	/* Get the item's sval */
	sval = o_ptr->sval;

	/* Ask for a spell, allow cancel */
	if (!get_spell(&spell, "memorize", sval, TRUE) && (spell == -1)) return;

	p_ptr->quick_spells[choice] = spell;
}

/*
 * Use the memorized quick spells
 */
extern void use_quick_spells(int qsnr)
{
	/* Sorry - I had to steel the variables to handle the spells as quick spells!!! */
	int py = p_ptr->py;
	int px = p_ptr->px;
	const magic_type *s_ptr;
	int item, sval, spell, dir;
	int chance, beam;
	int plev = p_ptr->lev;

	if (p_ptr->quick_spells[qsnr] == -1)
		return;

	spell = p_ptr->quick_spells[qsnr];

	/* Mage Spells */
	if (cp_ptr->spell_type == 0)
	{
		/* Here follows the handling of the Mage spells. Sorry!!! I had to steel from the original
		* source, because there was no entry point into the routine and i was too lazy to
		* code it all new!!!
		*/
		/* Get the spell */
		s_ptr = &mp_ptr->info[spell];


		/* Verify "dangerous" spells */
		if (s_ptr->smana > p_ptr->csp)
		{
			/* Warning */
			msg_print("You do not have enough mana to cast this spell.");

			/* Flush input */
			flush();

			/* Verify */
			if (!get_check("Attempt it anyway? ")) return;
		}


		/* Spell failure chance */
		chance = spell_chance(spell);

		/* Failed spell */
		if (rand_int(100) < chance)
		{
			if (flush_failure) flush();
			msg_print("You failed to get the spell off!");
		}

		/* Process spell */
		else
		    {
			/* Hack -- chance of "beam" instead of "bolt" */
			beam = ((cp_ptr->flags & CF_BEAM) ? plev : (plev / 2));

			/* Spells. */
			switch (spell)
			{
			case SPELL_MAGIC_MISSILE:
				{
					if (!get_aim_dir(&dir)) return;
					fire_bolt_or_beam(beam-10, GF_MISSILE, dir,
						damroll(3 + ((plev - 1) / 5), 4));
					break;
				}

			case SPELL_DETECT_MONSTERS:
				{
					(void)detect_monsters_normal();
					break;
				}

			case SPELL_PHASE_DOOR:
				{
					teleport_player(10);
					break;
				}

			case SPELL_LIGHT_AREA:
				{
					(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
					break;
				}

			case SPELL_TREASURE_DETECTION:
				{
					(void)detect_treasure();
					(void)detect_objects_gold();
					break;
				}

			case SPELL_CURE_LIGHT_WOUNDS:
				{
					(void)hp_player(damroll(2, 8));
					(void)set_cut(p_ptr->cut - 15);
					break;
				}

			case SPELL_OBJECT_DETECTION:
				{
					(void)detect_objects_normal();
					break;
				}

			case SPELL_FIND_TRAPS_DOORS:
				{
					(void)detect_traps();
					(void)detect_doors();
					(void)detect_stairs();
					break;
				}

			case SPELL_STINKING_CLOUD:
				{
					if (!get_aim_dir(&dir)) return;
					fire_ball(GF_POIS, dir,
						10 + (plev / 2), 2);
					break;
				}

			case SPELL_CONFUSE_MONSTER:
				{
					if (!get_aim_dir(&dir)) return;
					(void)confuse_monster(dir, plev);
					break;
				}

			case SPELL_LIGHTNING_BOLT:
				{
					if (!get_aim_dir(&dir)) return;
					fire_bolt_or_beam(beam-10, GF_ELEC, dir,
						damroll(3+((plev-5)/4), 8));
					break;
				}

			case SPELL_TRAP_DOOR_DESTRUCTION:
				{
					(void)destroy_doors_touch();
					break;
				}

			case SPELL_SLEEP_I:
				{
					if (!get_aim_dir(&dir)) return;
					(void)sleep_monster(dir);
					break;
				}

			case SPELL_CURE_POISON:
				{
					(void)set_poisoned(0);
					break;
				}

			case SPELL_TELEPORT_SELF:
				{
					teleport_player(plev * 5);
					break;
				}

			case SPELL_SPEAR_OF_LIGHT:
				{
					if (!get_aim_dir(&dir)) return;
					msg_print("A line of blue shimmering light appears.");
					lite_line(dir);
					break;
				}

			case SPELL_FROST_BOLT:
				{
					if (!get_aim_dir(&dir)) return;
					fire_bolt_or_beam(beam-10, GF_COLD, dir,
						damroll(5+((plev-5)/4), 8));
					break;
				}

			case SPELL_TURN_STONE_TO_MUD:
				{
					if (!get_aim_dir(&dir)) return;
					(void)wall_to_mud(dir);
					break;
				}

			case SPELL_SATISFY_HUNGER:
				{
					(void)set_food(PY_FOOD_MAX - 1);
					break;
				}

			case SPELL_RECHARGE_ITEM_I:
				{
					(void)recharge(5);
					break;
				}

			case SPELL_SLEEP_II:
				{
					(void)sleep_monsters_touch();
					break;
				}

			case SPELL_POLYMORPH_OTHER:
				{
					if (!get_aim_dir(&dir)) return;
					(void)poly_monster(dir);
					break;
				}

			case SPELL_IDENTIFY:
				{
					(void)ident_spell();
					break;
				}

			case SPELL_SLEEP_III:
				{
					(void)sleep_monsters();
					break;
				}

			case SPELL_FIRE_BOLT:
				{
					if (!get_aim_dir(&dir)) return;
					fire_bolt_or_beam(beam, GF_FIRE, dir,
						damroll(8+((plev-5)/4), 8));
					break;
				}

			case SPELL_SLOW_MONSTER:
				{
					if (!get_aim_dir(&dir)) return;
					(void)slow_monster(dir);
					break;
				}

			case SPELL_FROST_BALL:
				{
					if (!get_aim_dir(&dir)) return;
					fire_ball(GF_COLD, dir,
						30 + (plev), 2);
					break;
				}

			case SPELL_RECHARGE_ITEM_II:
				{
					(void)recharge(40);
					break;
				}

			case SPELL_TELEPORT_OTHER:
				{
					if (!get_aim_dir(&dir)) return;
					(void)teleport_monster(dir);
					break;
				}

			case SPELL_HASTE_SELF:
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

			case SPELL_FIRE_BALL:
				{
					if (!get_aim_dir(&dir)) return;
					fire_ball(GF_FIRE, dir,
						55 + (plev), 2);
					break;
				}

			case SPELL_WORD_OF_DESTRUCTION:
				{
					destroy_area(py, px, 15, TRUE);
					break;
				}

			case SPELL_GENOCIDE:
				{
					(void)genocide();
					break;
				}

			case SPELL_DOOR_CREATION:
				{
					(void)door_creation();
					break;
				}

			case SPELL_STAIR_CREATION:
				{
					(void)stair_creation();
					break;
				}

			case SPELL_TELEPORT_LEVEL:
				{
					(void)teleport_player_level();
					break;
				}

			case SPELL_EARTHQUAKE:
				{
					earthquake(py, px, 10);
					break;
				}

			case SPELL_WORD_OF_RECALL:
				{
					set_recall();
					break;
				}

			case SPELL_ACID_BOLT:
				{
					if (!get_aim_dir(&dir)) return;
					fire_bolt_or_beam(beam, GF_ACID, dir,
						damroll(6+((plev-5)/4), 8));
					break;
				}

			case SPELL_CLOUD_KILL:
				{
					if (!get_aim_dir(&dir)) return;
					fire_ball(GF_POIS, dir,
						20 + (plev / 2), 3);
					break;
				}

			case SPELL_ACID_BALL:
				{
					if (!get_aim_dir(&dir)) return;
					fire_ball(GF_ACID, dir,
						40 + (plev), 2);
					break;
				}

			case SPELL_ICE_STORM:
				{
					if (!get_aim_dir(&dir)) return;
					fire_ball(GF_COLD, dir,
						70 + (plev), 3);
					break;
				}

			case SPELL_METEOR_SWARM:
				{
					if (!get_aim_dir(&dir)) return;
					fire_ball(GF_METEOR, dir,
						65 + (plev), 3);
					break;
				}

			case SPELL_MANA_STORM:
				{
					if (!get_aim_dir(&dir)) return;
					fire_ball(GF_MANA, dir,
						300 + (plev * 2), 3);
					break;
				}

			case SPELL_DETECT_EVIL:
				{
					(void)detect_monsters_evil();
					break;
				}

			case SPELL_DETECT_ENCHANTMENT:
				{
					(void)detect_objects_magic();
					break;
				}

			case SPELL_RECHARGE_ITEM_III:
				{
					recharge(100);
					break;
				}

			case SPELL_GENOCIDE2:
				{
					(void)genocide();
					break;
				}

			case SPELL_MASS_GENOCIDE:
				{
					(void)mass_genocide();
					break;
				}

			case SPELL_RESIST_FIRE:
				{
					(void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + 20);
					break;
				}

			case SPELL_RESIST_COLD:
				{
					(void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20);
					break;
				}

			case SPELL_RESIST_ACID:
				{
					(void)set_oppose_acid(p_ptr->oppose_acid + randint(20) + 20);
					break;
				}

			case SPELL_RESIST_POISON:
				{
					(void)set_oppose_pois(p_ptr->oppose_pois + randint(20) + 20);
					break;
				}

			case SPELL_RESISTANCE:
				{
					int time = randint(20) + 20;
					(void)set_oppose_acid(p_ptr->oppose_acid + time);
					(void)set_oppose_elec(p_ptr->oppose_elec + time);
					(void)set_oppose_fire(p_ptr->oppose_fire + time);
					(void)set_oppose_cold(p_ptr->oppose_cold + time);
					(void)set_oppose_pois(p_ptr->oppose_pois + time);
					break;
				}

			case SPELL_HEROISM:
				{
					(void)hp_player(10);
					(void)set_hero(p_ptr->hero + randint(25) + 25);
					(void)set_afraid(0);
					break;
				}

			case SPELL_SHIELD:
				{
					(void)set_shield(p_ptr->shield + randint(20) + 30);
					break;
				}

			case SPELL_BERSERKER:
				{
					(void)hp_player(30);
					(void)set_shero(p_ptr->shero + randint(25) + 25);
					(void)set_afraid(0);
					break;
				}

			case SPELL_ESSENCE_OF_SPEED:
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

			case SPELL_GLOBE_OF_INVULNERABILITY:
				{
					(void)set_invuln(p_ptr->invuln + randint(8) + 8);
					break;
				}
			}

			/* A spell was cast */
			if (!((spell < 32) ?
				(p_ptr->spell_worked1 & (1L << spell)) :
				(p_ptr->spell_worked2 & (1L << (spell - 32)))))
			{
				int e = s_ptr->sexp;

				/* The spell worked */
				if (spell < 32)
				{
					p_ptr->spell_worked1 |= (1L << spell);
				}
				else
				    {
					p_ptr->spell_worked2 |= (1L << (spell - 32));
				}

				/* Gain experience */
				gain_exp(e * s_ptr->slevel);

				/* Redraw object recall */
				p_ptr->window |= (PW_OBJECT);
			}
		}

		/* Take a turn */
		p_ptr->energy_use = 100;

		/* Sufficient mana */
		if (s_ptr->smana <= p_ptr->csp)
		{
			/* Use some mana */
			p_ptr->csp -= s_ptr->smana;
		}

		/* Over-exert the player */
		else
		    {
			int oops = s_ptr->smana - p_ptr->csp;

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
				(void)dec_stat(A_CON, 15 + randint(10), perm);
			}
		}
	}

	/* Priest/Shamane Spells */
	else
	    {
		/* Here follows the handling of the Priest spells. Sorry!!! I had to steel from the original
		* source, because there was no entry point into the routine and i was too lazy to
		* code it all new!!!
		*/
		/* Get the spell */
		s_ptr = &mp_ptr->info[spell];


		/* Verify "dangerous" prayers */
		if (s_ptr->smana > p_ptr->csp)
		{
			/* Warning */
			msg_print("You do not have enough mana to recite this prayer.");

			/* Flush input */
			flush();

			/* Verify */
			if (!get_check("Attempt it anyway? ")) return;
		}


		/* Spell failure chance */
		chance = spell_chance(spell);

		/* Check for failure */
		if (rand_int(100) < chance)
		{
			if (flush_failure) flush();
			msg_print("You failed to concentrate hard enough!");
		}

		/* Success */
		else
		    {
			switch (spell)
			{
			case PRAYER_DETECT_EVIL:
				{
					(void)detect_monsters_evil();
					break;
				}

			case PRAYER_CURE_LIGHT_WOUNDS:
				{
					(void)hp_player(damroll(2, 10));
					(void)set_cut(p_ptr->cut - 10);
					break;
				}

			case PRAYER_BLESS:
				{
					(void)set_blessed(p_ptr->blessed + randint(12) + 12);
					break;
				}

			case PRAYER_REMOVE_FEAR:
				{
					(void)set_afraid(0);
					break;
				}

			case PRAYER_CALL_LIGHT:
				{
					(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
					break;
				}

			case PRAYER_FIND_TRAPS:
				{
					(void)detect_traps();
					break;
				}

			case PRAYER_DETECT_DOORS_STAIRS:
				{
					(void)detect_doors();
					(void)detect_stairs();
					break;
				}

			case PRAYER_SLOW_POISON:
				{
					(void)set_poisoned(p_ptr->poisoned / 2);
					break;
				}

			case PRAYER_SCARE_MONSTER:
				{
					if (!get_aim_dir(&dir)) return;
					(void)fear_monster(dir, plev);
					break;
				}

			case PRAYER_PORTAL:
				{
					teleport_player(plev * 3);
					break;
				}

			case PRAYER_CURE_SERIOUS_WOUNDS:
				{
					(void)hp_player(damroll(4, 10));
					(void)set_cut((p_ptr->cut / 2) - 20);
					break;
				}

			case PRAYER_CHANT:
				{
					(void)set_blessed(p_ptr->blessed + randint(24) + 24);
					break;
				}

			case PRAYER_SANCTUARY:
				{
					(void)sleep_monsters_touch();
					break;
				}

			case PRAYER_SATISFY_HUNGER:
				{
					(void)set_food(PY_FOOD_MAX - 1);
					break;
				}

			case PRAYER_REMOVE_CURSE:
				{
					remove_curse();
					break;
				}

			case PRAYER_RESIST_HEAT_COLD:
				{
					(void)set_oppose_fire(p_ptr->oppose_fire + randint(10) + 10);
					(void)set_oppose_cold(p_ptr->oppose_cold + randint(10) + 10);
					break;
				}

			case PRAYER_NEUTRALIZE_POISON:
				{
					(void)set_poisoned(0);
					break;
				}

			case PRAYER_ORB_OF_DRAINING:
				{
					if (!get_aim_dir(&dir)) return;
					fire_ball(GF_HOLY_ORB, dir,
						(damroll(3, 6) + plev +
						    (plev / ((cp_ptr->flags & CF_BLESS_WEAPON) ? 2 : 4))),
						((plev < 30) ? 2 : 3));
					break;
				}

			case PRAYER_CURE_CRITICAL_WOUNDS:
				{
					(void)hp_player(damroll(6, 10));
					(void)set_cut(0);
					break;
				}

			case PRAYER_SENSE_INVISIBLE:
				{
					(void)set_tim_invis(p_ptr->tim_invis + randint(24) + 24);
					break;
				}

			case PRAYER_PROTECTION_FROM_EVIL:
				{
					(void)set_protevil(p_ptr->protevil + randint(25) + 3 * p_ptr->lev);
					break;
				}

			case PRAYER_EARTHQUAKE:
				{
					earthquake(py, px, 10);
					break;
				}

			case PRAYER_SENSE_SURROUNDINGS:
				{
					map_area();
					break;
				}

			case PRAYER_CURE_MORTAL_WOUNDS:
				{
					(void)hp_player(damroll(8, 10));
					(void)set_stun(0);
					(void)set_cut(0);
					break;
				}

			case PRAYER_TURN_UNDEAD:
				{
					(void)turn_undead();
					break;
				}

			case PRAYER_PRAYER:
				{
					(void)set_blessed(p_ptr->blessed + randint(48) + 48);
					break;
				}

			case PRAYER_DISPEL_UNDEAD:
				{
					(void)dispel_undead(randint(plev * 3));
					break;
				}

			case PRAYER_HEAL:
				{
					(void)hp_player(300);
					(void)set_stun(0);
					(void)set_cut(0);
					break;
				}

			case PRAYER_DISPEL_EVIL:
				{
					(void)dispel_evil(randint(plev * 3));
					break;
				}

			case PRAYER_GLYPH_OF_WARDING:
				{
					warding_glyph();
					break;
				}

			case PRAYER_HOLY_WORD:
				{
					(void)dispel_evil(randint(plev * 4));
					(void)hp_player(1000);
					(void)set_afraid(0);
					(void)set_poisoned(0);
					(void)set_stun(0);
					(void)set_cut(0);
					break;
				}

			case PRAYER_DETECT_MONSTERS:
				{
					(void)detect_monsters_normal();
					break;
				}

			case PRAYER_DETECTION:
				{
					(void)detect_all();
					break;
				}

			case PRAYER_PERCEPTION:
				{
					(void)ident_spell();
					break;
				}

			case PRAYER_PROBING:
				{
					(void)probing();
					break;
				}

			case PRAYER_CLAIRVOYANCE:
				{
					wiz_lite();
					break;
				}

			case PRAYER_CURE_SERIOUS_WOUNDS2:
				{
					(void)hp_player(damroll(4, 10));
					(void)set_cut(0);
					break;
				}

			case PRAYER_CURE_MORTAL_WOUNDS2:
				{
					(void)hp_player(damroll(8, 10));
					(void)set_stun(0);
					(void)set_cut(0);
					break;
				}

			case PRAYER_HEALING:
				{
					(void)hp_player(2000);
					(void)set_stun(0);
					(void)set_cut(0);
					break;
				}

			case PRAYER_RESTORATION:
				{
					(void)do_res_stat(A_STR);
					(void)do_res_stat(A_INT);
					(void)do_res_stat(A_WIS);
					(void)do_res_stat(A_DEX);
					(void)do_res_stat(A_CON);
					(void)do_res_stat(A_CHR);
					break;
				}

			case PRAYER_REMEMBRANCE:
				{
					(void)restore_level();
					break;
				}

			case PRAYER_DISPEL_UNDEAD2:
				{
					(void)dispel_undead(randint(plev * 4));
					break;
				}

			case PRAYER_DISPEL_EVIL2:
				{
					(void)dispel_evil(randint(plev * 4));
					break;
				}

			case PRAYER_BANISHMENT:
				{
					if (banish_evil(100))
					{
						msg_print("The power of your god banishes evil!");
					}
					break;
				}

			case PRAYER_WORD_OF_DESTRUCTION:
				{
					destroy_area(py, px, 15, TRUE);
					break;
				}

			case PRAYER_ANNIHILATION:
				{
					if (!get_aim_dir(&dir)) return;
					drain_life(dir, 200);
					break;
				}

			case PRAYER_UNBARRING_WAYS:
				{
					(void)destroy_doors_touch();
					break;
				}

			case PRAYER_RECHARGING:
				{
					(void)recharge(15);
					break;
				}

			case PRAYER_DISPEL_CURSE:
				{
					(void)remove_all_curse();
					break;
				}

			case PRAYER_ENCHANT_WEAPON:
				{
					(void)enchant_spell(rand_int(4) + 1, rand_int(4) + 1, 0);
					break;
				}

			case PRAYER_ENCHANT_ARMOUR:
				{
					(void)enchant_spell(0, 0, rand_int(3) + 2);
					break;
				}

			case PRAYER_ELEMENTAL_BRAND:
				{
					brand_weapon();
					break;
				}

			case PRAYER_BLINK:
				{
					teleport_player(10);
					break;
				}

			case PRAYER_TELEPORT_SELF:
				{
					teleport_player(plev * 8);
					break;
				}

			case PRAYER_TELEPORT_OTHER:
				{
					if (!get_aim_dir(&dir)) return;
					(void)teleport_monster(dir);
					break;
				}

			case PRAYER_TELEPORT_LEVEL:
				{
					(void)teleport_player_level();
					break;
				}

			case PRAYER_WORD_OF_RECALL:
				{
					set_recall();
					break;
				}

			case PRAYER_ALTER_REALITY:
				{
					msg_print("The world changes!");

					/* Leaving */
					p_ptr->leaving = TRUE;

					break;
				}
			}

			/* A prayer was prayed */
			if (!((spell < 32) ?
				(p_ptr->spell_worked1 & (1L << spell)) :
				(p_ptr->spell_worked2 & (1L << (spell - 32)))))
			{
				int e = s_ptr->sexp;

				/* The spell worked */
				if (spell < 32)
				{
					p_ptr->spell_worked1 |= (1L << spell);
				}
				else
				    {
					p_ptr->spell_worked2 |= (1L << (spell - 32));
				}

				/* Gain experience */
				gain_exp(e * s_ptr->slevel);

				/* Redraw object recall */
				p_ptr->window |= (PW_OBJECT);
			}
		}

		/* Take a turn */
		p_ptr->energy_use = 100;

		/* Sufficient mana */
		if (s_ptr->smana <= p_ptr->csp)
		{
			/* Use some mana */
			p_ptr->csp -= s_ptr->smana;
		}

		/* Over-exert the player */
		else
		    {
			int oops = s_ptr->smana - p_ptr->csp;

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
				(void)dec_stat(A_CON, 15 + randint(10), perm);
			}
		}
	}

	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
}

/*
 * User Commands
 */
void user_commands(void)
{
	int i;
	char in;

	/* Save the Screen */
	screen_save();

	for (i = 0; i < 7; i++)
		c_put_str(TERM_WHITE, user_commands_menu[i], i + 1, 20);
	c_put_str(TERM_WHITE, "Wich User command? (1-7):", 0, 0);

	/* Key-Loop. do not end, as long as the key is not between a - f or ESC */
	while (TRUE)
	{
		in = inkey(); /* Wait for keypress */
		if ((in < '1' || in > '7' || in == '\0') && in != ESCAPE)
			continue;
		else
		    break;
		;
	}

	/* Restore Screen */
	screen_load();

	if (in == ESCAPE)
		return;

	switch (in)
	{
	case '1':
		/* Term User */
		{
			(void)Term_user(0);
			break;
		}

		/* Single line from a pref file */
	case '2':
		{
			do_cmd_pref();
			break;
		}

		/* Interact with visuals */
	case '3':
		{
			do_cmd_visuals();
			break;
		}

		/* Interact with colors */
	case '4':
		{
			do_cmd_colors();
			break;
		}

		/* Load "screen dump" */
	case '5':
		{
			do_cmd_load_screen();
			break;
		}

		/* Save "screen dump" */
	case '6':
		{
			do_cmd_save_screen();
			break;
		}

		/* Identify symbol */
	case '7':
		{
			do_cmd_query_symbol();
			break;
		}
	}
}
