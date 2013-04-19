/* File: nmagic2.c */

/* 
 * Purpose: Code for the new magic system: the actual spells. 
 * By Benjamin Mann, although large chunks are copied from elsewhere. 
 */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

#ifdef USE_NEW_MAGIC
/* Decide whether to do a failed version of a spell */
static bool failed_spell_okay(int spell)
{
	return FALSE;
}

bool cast_spell_new(int spell, bool success) 
{
	int 		px = p_ptr->px;
	int 		py = p_ptr->py;
	int		plev = p_ptr->lev;
	int		dir, i, dummy, beam;
	cave_type 	*c_ptr;
	bool 		no_summon = FALSE;

	if (!success && !failed_spell_okay(spell)) return FALSE;
	if (p_ptr->pclass == CLASS_MAGE) beam = plev;
	else beam = plev / 2;

	switch (spell)
	{
	case 1 : /* Detect Invisibility */
		(void)detect_monsters_invis();
		break;
	case 2 : /* Detect Creatures */
		(void)detect_monsters_normal();
		break;
	case 3 : /* Detect Undead and Demons */
		(void)detect_monsters_nonliving();
		break;
	case 4 : /* Detect Evil */
		(void)detect_monsters_evil();
		break;
	case 5 : /* Detect Monsters */
		(void)detect_monsters_normal();
		break;
	case 6 : /* Detect Traps, Doors and Stairs */
		(void)detect_traps();
		(void)detect_doors();
		(void)detect_stairs();
		break;
	case 7 : /* Detect Valuables */
		(void)detect_objects_normal();
		(void)detect_treasure();
		(void)detect_objects_gold();
		break;
	case 8 : /* Detect Enchantment */
		(void)detect_objects_magic();
		break;
	case 9 : /* Detect Artifacts */
		(void)detect_objects_artifacts();
		break;
	case 10 : /* Greater Detection */
		(void)detect_all();
		break;
	case 11 : /* Magic Mapping */
		map_area();
		break;
	case 12 : /* Awareness */
		map_area();
		/*(void)detect_traps();
		(void)detect_doors();
		(void)detect_stairs();
		(void)detect_monsters_normal();*/
		(void)detect_all();
		break;
	case 13 : /* Absolute Awareness */
		map_area();
		(void)detect_all();
		fully_identify_everything(MAX_DETECT);
		break;
	case 14 : /* Clairvoyance */
		wiz_lite();
		if (!(p_ptr->telepathy))
		{
			(void)set_tim_esp(p_ptr->tim_esp + rand_range(25, 55));
		}
		break;
	case 15 : /* Call Sunlight */
		(void)fire_ball(GF_LITE, 0, 150, 8);
		wiz_lite();
		if ((p_ptr->prace == RACE_VAMPIRE) && !p_ptr->resist_lite)
		{
			msg_print("The sunlight scorches your flesh!");
			take_hit(50, "sunlight");
		}
		break;
	case 16 : /* Telepathy */
		(void)set_tim_esp(p_ptr->tim_esp + rand_range(25, 55));
		break;
	case 17 : /* Psychometry */
		return psychometry();
	case 18 : /* Identify */
		return ident_spell();
	case 19 : /* General Identify */
		identify_pack();
		break;
	case 20 : /* Area Identify */
		identify_everything(-2);
		break;
	case 21 : /* Cave Identify */
		identify_everything(-1);
		break;
	case 22 : /* Greater Identify */
		return identify_fully();
	case 23 : /* Complete Item Knowledge */
		fully_identify_pack();
		break;
	case 24 : /* Greater Area Identify */
		fully_identify_everything(-2);
		break;
	case 25 : /* Absolute Identify */
		fully_identify_everything(-1);
		break;
	case 26 : /* Self Knowledge */
		(void)self_knowledge();
		break;
	case 27 : /* Absolute Knowledge */
		wiz_lite();
		if (!(p_ptr->telepathy))
		{
			(void)set_tim_esp(p_ptr->tim_esp + rand_range(25, 55));
		}
		fully_identify_everything(-1);
		(void)self_knowledge();
		break;
	case 28 : /* Cure Light Wounds */
		(void)hp_player(damroll(2, 10));
		(void)set_cut(p_ptr->cut - 10);
		break;
	case 29 : /* Cure Normal Wounds */
		(void)hp_player(damroll(4, 10));
		(void)set_cut((p_ptr->cut / 2) - 20);
		break;
	case 30 : /* Cure Serious Wounds */
		(void)hp_player(damroll(8, 10));
		(void)set_stun(0);
		(void)set_cut(0);
		break;
	case 31 : /* Cure Critical Wounds */
		(void)hp_player(damroll(20, 10));
		(void)set_stun(0);
		(void)set_cut(0);
		break;
	case 32 : /* Healing */
		(void)hp_player(300);
		(void)set_stun(0);
		(void)set_cut(0);
		break;
	case 33 : /* Greater Healing */
		(void)hp_player(2000);
		(void)set_afraid(0);
		(void)set_poisoned(0);
		(void)set_stun(0);
		(void)set_cut(0);
		break;
	case 34 : /* Restore Life */
		(void)restore_level();
		break;
	case 35 : /* Restoration */
		(void)do_res_stat(A_STR);
		(void)do_res_stat(A_INT);
		(void)do_res_stat(A_WIS);
		(void)do_res_stat(A_DEX);
		(void)do_res_stat(A_CON);
		(void)do_res_stat(A_CHR);
		(void)restore_level();
		break;
	case 36 : /* Neutralise Poison */
		(void)set_poisoned(0);
		break;
	case 37 : /* Cure Wounds and Poison */
		(void)set_cut(0);
		(void)set_poisoned(0);
		break;
	case 38 : /* Remove Fear */
		(void)set_afraid(0);
		break;
	case 39 : /* Remove Curse */
		(void)remove_curse();
		break;
	case 40 : /* Dispel Curse */
		(void)remove_all_curse();
		break;
	case 41 : /* Bless */
		(void)set_blessed(p_ptr->blessed + rand_range(12, 24));
		break;
	case 42 : /* Prayer */
		(void)set_blessed(p_ptr->blessed + rand_range(50, 100));
		break;
	case 43 : /* Heroism */
		(void)set_hero(p_ptr->hero + rand_range(25, 50));
		(void)hp_player(10);
		(void)set_afraid(0);
		break;
	case 44 : /* Haste */
		if (!p_ptr->fast)
		{
			(void)set_fast(randint1(20 + plev) + plev);
		}
		else
		{
			(void)set_fast(p_ptr->fast + randint1(5));
		}
		break;
	case 45 : /* Berserk Strength */
		(void)set_shero(p_ptr->shero + rand_range(25, 50));
		(void)hp_player(30);
		(void)set_afraid(0);
		break;
	case 46 : /* Battle Frenzy */
		(void)set_shero(p_ptr->shero + rand_range(25, 50));
		(void)hp_player(30);
		(void)set_afraid(0);
		if (!p_ptr->fast)
		{
			(void)set_fast(rand_range(plev / 2, 20 + plev));
		}
		else
		{
			(void)set_fast(p_ptr->fast + randint1(5));
		}
		break;
	case 47 : /* Protection from Corrosion */
		return rustproof();
	case 48 : /* Resist Cold */
		(void)set_oppose_cold(p_ptr->oppose_cold + rand_range(20, 40));
		break;
	case 49 : /* Resist Fire */
		(void)set_oppose_fire(p_ptr->oppose_fire + rand_range(20, 40));
		break;
	case 50 : /* Resist Lightning */
		(void)set_oppose_elec(p_ptr->oppose_elec + rand_range(20, 40));
		break;
	case 51 : /* Resist Acid */
		(void)set_oppose_acid(p_ptr->oppose_acid + rand_range(20, 40));
		break;
	case 52 : /* Resist Poison */
		(void)set_oppose_pois(p_ptr->oppose_pois + rand_range(20, 40));
		break;
	case 53 : /* Resist Environment */
		{
			int dur = rand_range(20, 40);
			(void)set_oppose_cold(p_ptr->oppose_cold + dur);
			(void)set_oppose_fire(p_ptr->oppose_fire + dur);
			(void)set_oppose_elec(p_ptr->oppose_elec + dur);
		}
		break;
	case 54 : /* Resistance */
		{
			int dur = rand_range(20, 40);
			(void)set_oppose_acid(p_ptr->oppose_acid + dur);
			(void)set_oppose_elec(p_ptr->oppose_elec + dur);
			(void)set_oppose_fire(p_ptr->oppose_fire + dur);
			(void)set_oppose_cold(p_ptr->oppose_cold + dur);
			(void)set_oppose_pois(p_ptr->oppose_pois + dur);
		}
		break;
	case 55 : /* Stoneskin */
		(void)set_shield(p_ptr->shield + rand_range(30, 50));
		break;
	case 56 : /* Globe of Invulnerability */
		(void)set_invuln(p_ptr->invuln + rand_range(8, 16));
		break;
	case 57 : /* Wraithform */
		(void)set_wraith_form(p_ptr->wraith_form + rand_range(8, 16));
		break;
	case 58 : /* Light */
		(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
		break;
	case 59 : /* Spear of Light */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)lite_line(dir);
		break;
	case 60 : /* Create Food */
		(void)set_food(PY_FOOD_MAX - 1);
		break;
	case 61 : /* See Invisible */
		(void)set_tim_invis(p_ptr->tim_invis + rand_range(24, 48));
		break;
	case 62 : /* Glyph of Warding */
		(void)warding_glyph();
		break;
	case 63 : /* Greater Warding */
		(void)warding_glyph();
		(void)glyph_creation();
		break;
	case 64 : /* Explosive Rune */
		(void)explosive_rune();
		break;
	case 65 : /* Touch of Confusion */
		if (!p_ptr->confusing)
		{
			msg_print("Your hands start glowing.");
			p_ptr->confusing = TRUE;
			p_ptr->redraw |= (PR_STATUS);
		}
		break;
	case 66 : /* Confuse Monster */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)confuse_monster(dir, (plev * 3) / 2);
		break;
	case 67 : /* Sleep */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)sleep_monster(dir);
		break;
	case 68 : /* Mass Sleep */
		(void)sleep_monsters();
		break;
	case 69 : /* Slow Monster */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)slow_monster(dir);
		break;
	case 70 : /* Entangle */
		(void)slow_monsters();
		break;
	case 71 : /* Scare Monster */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fear_monster(dir, plev);
		break;
	case 72 : /* Terror */
		(void)turn_monsters(30 + plev);
		break;
	case 73 : /* Horrify */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fear_monster(dir, plev);
		(void)stun_monster(dir, plev);
		break;
	case 74 : /* Hold Monster */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)stasis_monster(dir);
		break;
	case 75 : /* Protection from Evil */
		(void)set_protevil(p_ptr->protevil + randint1(25) + 3 * p_ptr->lev);
		break;
	case 76 : /* Mind Blast */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_bolt_or_beam(beam - 10, GF_PSI, dir,
		                  damroll(3 + ((plev - 1) / 5), 3));
		break;
	case 77 : /* Magic Missile */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fire_bolt_or_beam(beam - 10, GF_MISSILE, dir,
			damroll(3 + ((plev - 1) / 5), 3));
		break;
	case 78 : /* Mana Bolt */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_bolt(GF_MISSILE, dir, 75 + randint1(350 + (3*plev)));
		break;
	case 79 : /* Mana Storm */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fire_ball(GF_MANA, dir, 300 + (plev * 2), 4);
		break;
	case 80 : /* Hellfire */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fire_ball(GF_HELL_FIRE, dir, 666, 3);
		take_hit(rand_range(50, 100), "the strain of casting Hellfire");
		break;
	case 81 : /* Malediction */
		if (!get_aim_dir(&dir)) return FALSE;
		/* A radius-0 ball may (1) be aimed at objects etc.,
		 * and will affect them; (2) may be aimed at ANY
		 * visible monster, unlike a 'bolt' which must travel
		 * to the monster. */

		(void)fire_ball(GF_HELL_FIRE, dir,
			damroll(3 + ((plev - 1) / 5), 3), 0);

		if (one_in_(5))
		{
			/* Special effect first */
			dummy = randint1(1000);
			if (dummy == 666)
				(void)fire_bolt(GF_DEATH_RAY, dir, plev * 50);
			else if (dummy < 500)
				(void)fire_bolt(GF_TURN_ALL, dir, plev);
			else if (dummy < 800)
				(void)fire_bolt(GF_OLD_CONF, dir, plev);
			else
				(void)fire_bolt(GF_STUN, dir, plev);
		}
		break;
	case 82 : /* Orb of Entropy */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fire_ball(GF_OLD_DRAIN, dir,
			(damroll(3, 6) + plev +
			(plev / (((p_ptr->pclass == CLASS_MAGE) ||
			(p_ptr->pclass == CLASS_HIGH_MAGE)) ? 2 : 4))),
			((plev < 30) ? 2 : 3));
		break;
	case 83 : /* Orb of Draining */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fire_ball(GF_HOLY_FIRE, dir,
			(damroll(3, 6) + plev +
			(plev / (p_ptr->pclass == CLASS_PRIEST ? 2 : 4))),
			((plev < 30) ? 2 : 3));

		break;
	case 84 : /* Electrical Charge */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_bolt_or_beam(beam - 10, GF_ELEC, dir,
			damroll(3 + ((plev - 1) / 5), 4));
		break;
	case 85 : /* Lightning Bolt */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_bolt_or_beam(beam - 10, GF_ELEC, dir,
			damroll(3 + ((plev - 5) / 4), 8));
		break;
	case 86 : /* Chain Lightning */
		for (dir = 0; dir <= 9; dir++)
			(void)fire_beam(GF_ELEC, dir, damroll(5 + (plev / 10), 8));
		break;
	case 87 : /* Ball Lightning */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fire_ball(GF_ELEC, dir, plev + 15, 2);
		break;
	case 88 : /* Lightning Storm */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_ELEC, dir, 70 + plev, (plev / 5) + 1);
		break;
	case 89 : /* Electrocution */
		(void)fire_ball(GF_ELEC, 0, 250 + (4 * plev), 1);
		break;
	case 90 : /* Absolute Lightning */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_ELEC, dir, 250 + (4 * plev), 2);
		break;
	case 91 : /* Freeze */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_bolt_or_beam(beam - 10, GF_COLD, dir,
			damroll(3 + ((plev - 1) / 5), 4));
		break;
	case 92 : /* Frost Bolt */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_bolt_or_beam(beam - 10, GF_COLD, dir,
			damroll(5 + ((plev - 5) / 4), 8));
		break;
	case 93 : /* Cold Ball */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fire_ball(GF_COLD, dir, plev + 30, 2);
		break;
	case 94 : /* Ice Storm */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_COLD, dir, 70 + plev, (plev / 12) + 1);
		break;
	case 95 : /* Absolute Zero */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_COLD, dir, 300 + (6 * plev), 2);
		break;
	case 96 : /* Shoot Acid */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_bolt_or_beam(beam - 10, GF_ACID, dir,
			damroll(3 + ((plev - 1) / 5), 4));
		break;
	case 97 : /* Acid Bolt */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_bolt_or_beam(beam - 10, GF_ACID, dir,
			damroll(6 + ((plev - 5) / 4), 8));
		break;
	case 98 : /* Acid Ball */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fire_ball(GF_ACID, dir, plev + 40, 2);
		break;
	case 99 : /* Mass Corrosion */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_ACID, dir, 90 + plev, (plev / 12) + 1);
		break;
	case 100 : /* Absolute Acid */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_ACID, dir, 400 + (7 * plev), 2);
		break;
	case 101 : /* Flame */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_bolt_or_beam(beam - 5, GF_FIRE, dir,
			damroll(3 + ((plev + 2) / 4), 4));
		break;
	case 102 : /* Fire Bolt */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fire_bolt_or_beam(beam, GF_FIRE, dir,
			damroll(8 + ((plev - 5) / 4), 8));
		break;
	case 103 : /* Fireball */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fire_ball(GF_FIRE, dir, plev + 55, 2);
		break;
	case 104 : /* Firestorm */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_FIRE, dir, 150 + (plev * 2), (plev / 12) + 1);
		break;
	case 105 : /* Sunfire */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_FIRE, dir, 450 + (9 * plev), 2);
		break;
	case 106 : /* Flame Strike */
		(void)fire_ball(GF_FIRE, 0, 150 + (2 * plev), 8);
		break;
	case 107 : /* Meteor Swarm */
		{
			int x, y;
			int b = rand_range(10, 20);
			for (i = 0; i < b; i++)
			{
				int count = 0;

				while (count < 1000)
				{
					count++;
					
					x = px - 5 + randint1(10);
					y = py - 5 + randint1(10);

					/* paranoia */
					if (!in_bounds(y, x)) continue;

					c_ptr = area(y, x);

					/* keep going if not in LOS */
					if (!player_has_los_grid(c_ptr)) continue;

					/* if close enough - exit */
					if (distance(py, px, y, x) < 6) break;
				}

				if (count >= 1000) break;

				(void)project(0, 2, y, x, (plev * 3) / 2, GF_METEOR, PROJECT_KILL | PROJECT_JUMP | PROJECT_ITEM);
			}
		}
		break;
	case 108 : /* Stinking Cloud */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fire_ball(GF_POIS, dir, 8 + (plev / 2), 2);
		break;
	case 109 : /* Cloudkill */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_POIS, dir, 40 + plev, (plev / 12) + 1);
		break;
	case 110 : /* Absolute Poison */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_POIS, dir, 150 + (3 * plev), 2);
		break;
	case 111 : /* Whirlpool */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_WATER, dir, 100 + plev, (plev / 12) + 1);
		break;
	case 112 : /* Magic Rocket */
		if (!get_aim_dir(&dir)) return FALSE;

		msg_print("You launch a rocket!");
		(void)fire_ball(GF_ROCKET, dir, 120 + (plev * 2), 2);
		break;
	case 113 : /* Sonic Boom */
		msg_print("BOOM! Shake the room!");
		(void)project(0, plev / 10 + 2, py, px,
			45 + plev, GF_SOUND, PROJECT_KILL | PROJECT_ITEM);
		break;
	case 114 : /* Chaos Bolt */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fire_bolt_or_beam(beam, GF_CHAOS, dir,
		                  damroll(10 + ((plev - 5) / 4), 8));
		break;
	case 115 : /* Chaos Spread */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fire_ball(GF_CHAOS, dir, plev + 66, plev / 5);
		break;
	case 116 : /* Breathe Chaos */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fire_ball(GF_CHAOS, dir, p_ptr->chp, 2);
		break;
	case 117 : /* Call Chaos */
		call_chaos();
		break;
	case 118 : /* Fist of Force */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fire_ball(GF_DISINTEGRATE, dir,
			damroll(8 + ((plev - 5) / 4), 8), 0);
		break;
	case 119 : /* Disintegration */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fire_ball(GF_DISINTEGRATE, dir, plev + 80, 3 + plev / 40);
		break;
	case 120 : /* Beam of Gravity */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fire_beam(GF_GRAVITY, dir, damroll(9 + ((plev - 5) / 4), 8));
		break;
	case 121 : /* Gravitic Wave */
		(void)fire_ball(GF_GRAVITY, 0, 25 + (plev / 2), 4);
		break;
	case 122 : /* Doom Bolt */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fire_beam(GF_MANA, dir, damroll(11 + ((plev - 5) / 4), 8));
		break;
	case 123 : /* Darkness Bolt */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fire_bolt_or_beam(beam, GF_DARK, dir,
			damroll(4 + ((plev - 5) / 4), 8));
		break;
	case 124 : /* Darkness Storm */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fire_ball(GF_DARK, dir, (150 + (plev * 2)), 4);
		break;
	case 125 : /* Nether Bolt */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fire_bolt_or_beam(beam, GF_NETHER, dir,
		                  damroll(7 + ((plev - 5) / 4), 8));
		break;
	case 126 : /* Nether Ball */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_NETHER, dir, 50 + plev, 2);
		break;
	case 127 : /* Project Void */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fire_bolt_or_beam(beam, GF_MANA, dir,
		                  damroll(5 + ((plev - 5) / 4), 8));
		break;
	case 128 : /* Call the Void */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_MANA, dir, 250 + (2 * plev), 8);
		break;
	case 129 : /* Breathe Power */
		i = randint1(100);
		if (!get_aim_dir(&dir)) return FALSE;

		if (one_in_(2))
		{
		do
		{
		i = randint1(100);
		if (i < 5)
		{
			msg_print("You breathe light.");
			(void)fire_ball(GF_LITE, dir, 10 + (plev / 5), 2);			
		}
		else if (i < 10)
		{
			msg_print("You breathe darkness.");
			(void)fire_ball(GF_DARK, dir, 15 + (plev / 5), 2);
		}
		else if (i < 14)
		{
			msg_print("You project a missile.");
			(void)fire_bolt_or_beam(beam, GF_MISSILE, dir,
		damroll(3 + ((plev - 1) / 5), 4));
		}
		else if (i < 19)
		{
			msg_print("You breathe nexus.");
			(void)fire_ball(GF_NEXUS, dir, 10 + (plev / 2), 2);
		}
		else if (i < 23)
		{
			msg_print("You breathe time.");
			(void)fire_ball(GF_TIME, dir, 15 + (plev / 2), 2);
		}
		else if (i < 28)
		{
			msg_print("You breathe inertia.");
			(void)fire_ball(GF_INERTIA, dir, 20 + (plev / 2), 2);
		}
		else if (i < 32)
		{
			msg_print("You breathe frost.");
			(void)fire_ball(GF_COLD, dir, 15 + ((3 * plev) / 4), 2);
		}
		else if (i < 37)
		{
			msg_print("You breathe acid.");
			(void)fire_beam(GF_ACID, dir, damroll(4 + ((plev - 5) / 4), 8));
		}
		else if (i < 41)
		{
			msg_print("You breathe fire.");
			(void)fire_ball(GF_FIRE, dir, 40 + ((3 * plev) / 4), 2);
		}
		else if (i < 45)
		{
			msg_print("You breathe lightning.");
			(void)fire_beam(GF_ELEC, dir, damroll(9 + ((plev - 5) / 4), 8));
		}
		else if (i < 49)
		{
			msg_print("You breathe confusion.");
			(void)fire_ball(GF_CONFUSION, dir, 50 + plev, 2);
		}
		else if (i < 53)
		{
			msg_print("You breathe disenchantment.");
			(void)fire_ball(GF_DISENCHANT, dir, 55 + plev, 2);
		}
		else if (i < 57)
		{
			msg_print("You breathe shards.");
			(void)fire_ball(GF_SHARDS, dir, 60 + plev, 2);
		}
		else if (i < 62)
		{
			msg_print("You breathe gravity.");
			(void)fire_ball(GF_GRAVITY, dir, 60 + (2 * plev), 2);
		}
		else if (i < 66)
		{
			msg_print("You breathe force.");
			(void)fire_ball(GF_FORCE, dir, 85 + (2 * plev), 2);
		}
		else if (i < 71)
		{
			msg_print("You breathe poison.");
			(void)fire_ball(GF_POIS, dir, 100 + (6 * plev), 2);
		}
		else if (i < 75)
		{
			msg_print("You breathe plasma.");
			(void)fire_ball(GF_PLASMA, dir, 150 + (6 * plev), 2);
		}
		else if (i < 79)
		{
			msg_print("You breathe toxic waste.");
			(void)fire_ball(GF_NUKE, dir, 200 + (4 * plev), 2);
		}
		else if (i < 83)
		{
			msg_print("You breathe sound.");
			(void)fire_ball(GF_SOUND, dir, 250 + (5 * plev), 2);
		}
		else if (i == 83)
		{
			msg_print("Everything comes out!!!");
			(void)fire_ball(GF_LITE, dir, 2, 2);
			(void)fire_ball(GF_DARK, dir, 2, 2);
			(void)fire_bolt_or_beam(beam, GF_MISSILE, dir, 3);
			(void)fire_ball(GF_NEXUS, dir, 3, 2);
			(void)fire_ball(GF_TIME, dir, 4, 2);
			(void)fire_ball(GF_INERTIA, dir, 4, 2);
			(void)fire_ball(GF_COLD, dir, 5, 2);
			(void)fire_beam(GF_ACID, dir, 7);
			(void)fire_ball(GF_FIRE, dir, 8, 2);
			(void)fire_beam(GF_ELEC, dir, 9);
			(void)fire_ball(GF_CONFUSION, dir, 5 + (plev / 10), 2);
			(void)fire_ball(GF_DISENCHANT, dir, 6 + (plev / 10), 2);
			(void)fire_ball(GF_SHARDS, dir, 6 + (plev / 10), 2);
			(void)fire_ball(GF_GRAVITY, dir, 60 + (plev / 5), 2);
			(void)fire_ball(GF_FORCE, dir, 9 + (plev / 5), 2);
			(void)fire_ball(GF_POIS, dir, 10 + (plev / 5), 2);
			(void)fire_ball(GF_PLASMA, dir, 20 + (plev / 5), 2);
			(void)fire_ball(GF_NUKE, dir, 15 + (plev / 2), 2);
			(void)fire_ball(GF_SOUND, dir, 25 + (plev / 2), 2);
			(void)fire_ball(GF_NETHER, dir, 35 + (plev / 2), 2);
			(void)fire_ball(GF_CHAOS, dir, 40 + (plev / 2), 2);
			(void)fire_ball(GF_DISINTEGRATE, dir, 45 + (plev / 2), 2);
			(void)fire_ball(GF_MANA, dir, 25 + plev, 2);
		}
		else if (i < 88)
		{
			msg_print("You breathe nether.");
			(void)fire_ball(GF_NETHER, dir, 300 + (12 * plev), 2);
		}
		else if (i < 92)
		{
			msg_print("You breathe chaos.");
			(void)fire_ball(GF_CHAOS, dir, 300 + (7 * plev), 2);
		}
		else if (i < 97)
		{
			msg_print("You breathe disintegration.");
			(void)fire_ball(GF_DISINTEGRATE, dir, 350 + (14 * plev), 2);
		}
		else
		{
			msg_print("You breathe mana.");
			(void)fire_ball(GF_MANA, dir, 350 + (15 * plev), 2);
		}
		}
		while (one_in_(2));
		}
		else msg_print("Nothing seems to come.");
		break;
	case 130 : /* Power */
		call_the_();
		break;
	case 131 : /* Death Ray */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)death_ray(dir, plev);
		break;
	case 132 : /* Wonder */
		{
		/*
		 * This spell should become more useful (more
		 * controlled) as the player gains experience levels.
		 * Thus, add 1/5 of the player's level to the i roll.
		 * This eliminates the worst effects later on, while
		 * keeping the results quite random.  It also allows
		 * some potent effects only at high level.
		 */
			i = randint1(100) + plev / 5;

			if (i < 26)
				chg_virtue(V_CHANCE, 1);

			if (!get_aim_dir(&dir)) return FALSE;
			if (i > 100)
				msg_print("You feel a surge of power!");
			if (i < 8) (void)clone_monster(dir);
			else if (i < 14) (void)speed_monster(dir);
			else if (i < 26) (void)heal_monster(dir);
			else if (i < 31) (void)poly_monster(dir);
			else if (i < 36)
				(void)fire_bolt_or_beam(beam - 10, GF_MISSILE, dir,
				                  damroll(3 + ((plev - 1) / 5), 4));
			else if (i < 41) (void)confuse_monster(dir, plev);
			else if (i < 46) (void)fire_ball(GF_POIS, dir, 20 + (plev / 2), 3);
			else if (i < 51) (void)lite_line(dir);
			else if (i < 56)
				(void)fire_bolt_or_beam(beam - 10, GF_ELEC, dir,
				                  damroll(3 + ((plev - 5) / 4), 8));
			else if (i < 61)
				(void)fire_bolt_or_beam(beam - 10, GF_COLD, dir,
				                  damroll(5 + ((plev - 5) / 4), 8));
			else if (i < 66)
				(void)fire_bolt_or_beam(beam, GF_ACID, dir,
				                  damroll(6 + ((plev - 5) / 4), 8));
			else if (i < 71)
				(void)fire_bolt_or_beam(beam, GF_FIRE, dir,
				                  damroll(8 + ((plev - 5) / 4), 8));
			else if (i < 76) (void)drain_life(dir, 75);
			else if (i < 81) (void)fire_ball(GF_ELEC, dir, 30 + plev / 2, 2);
			else if (i < 86) (void)fire_ball(GF_ACID, dir, 40 + plev, 2);
			else if (i < 91) (void)fire_ball(GF_ICE, dir, 70 + plev, 3);
			else if (i < 96) (void)fire_ball(GF_FIRE, dir, 80 + plev, 3);
			else if (i < 101) (void)drain_life(dir, 100 + plev);
			else if (i < 104)
			{
				(void)earthquake(py, px, 12);
			}
			else if (i < 106)
			{
				(void)destroy_area(py, px, 15);
			}
			else if (i < 108)
			{
				(void)genocide(TRUE);
			}
			else if (i < 110) (void)dispel_monsters(120);
			else /* RARE */
			{
				(void)dispel_monsters(150);
				(void)slow_monsters();
				(void)sleep_monsters();
				(void)hp_player(300);
			}
			break;
		}
	case 133 : /* Invoke Spirits */
		{
			i = randint1(100) + plev / 5;
			if (!get_aim_dir(&dir)) return FALSE;

			msg_print("You call on the power of the dead...");

			if (i < 26)
				chg_virtue(V_CHANCE, 1);

			if (i > 100)
				msg_print("You feel a surge of eldritch force!");

			if (i < 8)
			{
				msg_print("Oh no! Mouldering forms rise from the earth around you!");
				(void)summon_specific(0, py, px, p_ptr->depth, SUMMON_UNDEAD, 
						      TRUE, FALSE, FALSE, GP_FIXATED_ON_PLAYER, 0);

				chg_virtue(V_UNLIFE, 1);
			}
			else if (i < 14)
			{
				msg_print("An unnamable evil brushes against your mind...");
				(void)set_afraid(p_ptr->afraid + rand_range(4, 8));
			}
			else if (i < 26)
			{
				msg_print("Your head is invaded by a horde of gibbering spectral voices...");
				(void)set_confused(p_ptr->confused + rand_range(4, 8));
			}
			else if (i < 31)
			{
				(void)poly_monster(dir);
			}
			else if (i < 36)
			{
				(void)fire_bolt_or_beam(beam - 10, GF_MISSILE, dir,
					damroll(3 + ((plev - 1) / 5), 4));
			}
			else if (i < 41)
			{
				(void)confuse_monster (dir, plev);
			}
			else if (i < 46)
			{
				(void)fire_ball(GF_POIS, dir, 20 + (plev / 2), 3);
			}
			else if (i < 51)
			{
				(void)lite_line(dir);
			}
			else if (i < 56)
			{
				(void)fire_bolt_or_beam(beam - 10, GF_ELEC, dir,
					damroll(3 + ((plev - 5) / 4), 8));
			}
			else if (i < 61)
			{
				(void)fire_bolt_or_beam(beam - 10, GF_COLD, dir,
					damroll(5 + ((plev - 5) / 4), 8));
			}
			else if (i < 66)
			{
				(void)fire_bolt_or_beam(beam, GF_ACID, dir,
					damroll(6 + ((plev - 5) / 4), 8));
			}
			else if (i < 71)
			{
				(void)fire_bolt_or_beam(beam, GF_FIRE, dir,
					damroll(8 + ((plev - 5) / 4), 8));
			}
			else if (i < 76)
			{
				(void)drain_life(dir, 75);
			}
			else if (i < 81)
			{
				(void)fire_ball(GF_ELEC, dir, 30 + plev / 2, 2);
			}
			else if (i < 86)
			{
				(void)fire_ball(GF_ACID, dir, 40 + plev, 2);
			}
			else if (i < 91)
			{
				(void)fire_ball(GF_ICE, dir, 70 + plev, 3);
			}
			else if (i < 96)
			{
				(void)fire_ball(GF_FIRE, dir, 80 + plev, 3);
			}
			else if (i < 101)
			{
				(void)drain_life(dir, 100 + plev);
			}
			else if (i < 104)
			{
				(void)earthquake(py, px, 12);
			}
			else if (i < 106)
			{
				(void)destroy_area(py, px, 15);
			}
			else if (i < 108)
			{
				(void)genocide(TRUE);
			}
			else if (i < 110)
			{
				(void)dispel_monsters(120);
			}
			else
			{
				(void)dispel_monsters(150);
				(void)slow_monsters();
				(void)sleep_monsters();
				(void)hp_player(300);
			}

			if (i < 31)
				msg_print("Sepulchral voices chuckle. 'Soon you will join us, mortal.'");
			break;
		}
	case 134 : /* Vampiric Drain */
		if (!get_aim_dir(&dir)) return FALSE;

		dummy = plev + randint1(plev) * MAX(1, plev / 10);   /* Dmg */
		if (drain_gain_life(dir, dummy))
		{
			/*
			 * Hack - this only happens when monster is seen to
			 * be hit.
			 */
			chg_virtue(V_SACRIFICE, -1);
			chg_virtue(V_VITALITY, -1);

			/* Gain nutritional sustenance: 150/hp drained */
			/* A Food ration gives 5000 food points (by contrast) */
			/* Don't ever get more than "Full" this way */
			/* But if we ARE Gorged, it won't cure us */
			dummy = p_ptr->food + MIN(5000, 100 * dummy);
			if (p_ptr->food < PY_FOOD_MAX)   /* Not gorged already */
				(void)set_food(dummy >= PY_FOOD_MAX ? PY_FOOD_MAX - 1 : dummy);
		}
		break;
	case 135 : /* Drain Life */
		if (!get_aim_dir(&dir)) return FALSE;

		chg_virtue(V_SACRIFICE, -1);
		chg_virtue(V_VITALITY, -1);

		for (dummy = 0; dummy < 3; dummy++)
		{
			(void)drain_gain_life(dir, 100);
		}
		break;
	case 136 : /* Exorcism */
		(void)dispel_undead(plev);
		(void)dispel_demons(plev);
		(void)turn_evil(plev);
		break;
	case 137 : /* Dispel Undead and Demons */
		(void)dispel_undead(plev * 3);
		(void)dispel_demons(plev * 3);
		break;
	case 138 : /* Dispel Evil */
		(void)dispel_evil(plev * 4);
		break;
	case 139 : /* Word of Death */
		(void)dispel_living(plev * 3);
		break;
	case 140 : /* Evocation */
		(void)dispel_monsters(plev * 4);
		(void)turn_monsters(plev * 4);
		(void)banish_monsters(plev * 4);
		break;
	case 141 : /* Purge */
		(void)genocide(TRUE);
		break;
	case 142 : /* Mass Purge */
		(void)mass_genocide(TRUE);
		break;
	case 143 : /* Omnicide */
		p_ptr->csp -= 100;
		
		/* Display doesn't show mana cost (100)
		 * as deleted until the spell has finished. This gives a
		 * false impression of how high your mana is climbing.
		 * Therefore, 'deduct' the cost temporarily before entering the
		 * loop, then add it back at the end so that the rest of the
		 * program can deduct it properly
		 */
		for (i = 1; i < m_max; i++)
		{
			monster_type *m_ptr = &m_list[i];
			monster_race *r_ptr = &r_info[m_ptr->r_idx];

			/* Paranoia -- Skip dead monsters */
			if (!m_ptr->r_idx) continue;

			/* Hack -- Skip Unique Monsters */
			if (r_ptr->flags1 & (RF1_UNIQUE)) continue;

			/* Hack -- Skip Quest Monsters */
			if (r_ptr->flags1 & RF1_QUESTOR) continue;

			/* Notice changes in view */
			if (r_ptr->flags7 & (RF7_LITE_1 | RF7_LITE_2))
			{
				/* Update some things */
				p_ptr->update |= (PU_MON_LITE);
			}
			
			/* Delete the monster */
			delete_monster_idx(i);

			/* Take damage */
			take_hit(randint1(4), "the strain of casting Omnicide");

			/* Absorb power of dead soul - up to twice max. mana */
			if (p_ptr->csp < (p_ptr->msp * 2))
				p_ptr->csp++;

			/* Visual feedback */
			move_cursor_relative(py, px);

			/* Redraw */
			p_ptr->redraw |= (PR_HP | PR_MANA);

			/* Window stuff */
			p_ptr->window |= (PW_PLAYER);
			p_ptr->window |= (PW_SPELL);

			/* Handle */
			handle_stuff();

			/* Fresh */
			Term_fresh();

			/* Delay */
			Term_xtra(TERM_XTRA_DELAY,
				delay_factor * delay_factor * delay_factor);
		}

		/* Restore, ready to be deducted properly */
		p_ptr->csp += 100;

		break;
	case 144 : /* Polymorph Other */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)poly_monster(dir);
		break;
	case 145 : /* Charm Animal */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)charm_animal(dir, plev);
		break;
	case 146 : /* Enslave the Undead */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)control_one_undead(dir, plev);
		break;
	case 147 : /* Charm Monster */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)charm_monster(dir, plev);
		break;
	case 148 : /* Animal Friendship */
		(void)charm_animals(plev * 2);
		break;
	case 149 : /* Day of the Dove */
		(void)charm_monsters(plev * 2);
		break;
	case 150 : /* Summon Monster */
		if (summon_specific((success ? -1 : 0), py, px, (plev * 3) / 2, (success ? SUMMON_NO_UNIQUES : 0),
					(success ? FALSE : TRUE), FALSE, success, GP_COPY, 0))
		{
			if (!success) msg_print("An angry monster appears!");
		}
		else
		{
			no_summon = TRUE;
		}

		break;
	case 151 : /* Phantasmal Servant */
		if (summon_specific(-1, py, px, (plev * 3) / 2, SUMMON_PHANTOM, 
					FALSE, TRUE, TRUE, GP_ALLY, 0))
		{
			msg_print("'Your wish, master?'");
		}
		else
		{
			no_summon = TRUE;
		}
		break;
	case 152 : /* Conjure Elemental */
		if (summon_specific((success ? -1 : 0), py, px, (plev * 3) / 2, SUMMON_ELEMENTAL, 
					(success ? FALSE : TRUE), FALSE, success, GP_COPY, 0))
		{
			if (!success) msg_print("An angry elemental appears!");
		}
		else
		{
			no_summon = TRUE;
		}

		break;
	case 153 : /* Summon Spiders */
		if (summon_specific((success ? -1 : 0), py, px, plev, SUMMON_SPIDER,
					(success ? FALSE : TRUE), FALSE, success, GP_COPY, 0))
		{
			if (!success) msg_print("An angry spider appears!");
		}
		else
		{
			no_summon = TRUE;
		}

		break;
	case 154 : /* Summon Hydrae */
		if (summon_specific((success ? -1 : 0), py, px, (plev * 3) / 2, SUMMON_HYDRA, 
					(success ? FALSE : TRUE), FALSE, success, GP_COPY, 0))
		{
			if (!success) msg_print("An angry reptile appears!");
		}
		else
		{
			no_summon = TRUE;
		}
		
		break;
	case 155 : /* Summon Hounds */
		if (summon_specific((success ? -1 : 0), py, px, plev, SUMMON_HOUND, TRUE, FALSE, success, GP_COPY, 0))
		{
			if (!success) msg_print("Angry barking surrounds you!");
		}
		else
		{
			no_summon = TRUE;
		}

		break;
	case 156 : /* Summon Animal */
		if (summon_specific((success ? -1 : 0), py, px, plev, (success ? SUMMON_ANIMAL_RANGER : SUMMON_ANIMAL),
					(success ? FALSE : TRUE), FALSE, success, GP_COPY, 0))
		{
			if (!success) msg_print("An angry animal appears!");
		}
		else
		{
			no_summon = TRUE;
		}

		break;
	case 157 : /* Summon Animals */
		if (!(summon_specific(-1, py, px, plev, SUMMON_ANIMAL_RANGER, 
						TRUE, TRUE, TRUE, GP_ALLY, 0)))
			no_summon = TRUE;
		break;
	case 158 : /* Summon Demon */
		{
			bool pet = (!one_in_(3));
			bool group = !(pet && (plev < 50));

			if (summon_specific((pet ? -1 : 0), py, px, (plev * 3) / 2, SUMMON_DEMON, 
						group, FALSE, pet, GP_COPY, 0))
			{
				msg_print("The area fills with a stench of sulphur and brimstone.");

				if (pet)
					msg_print("'What is thy bidding... Master?'");
				else
					msg_print("'NON SERVIAM! Wretch! I shall feast on thy mortal soul!'");
			}
			break;
		}
	case 159 : /* Summon Greater Demon */
		if (summon_specific((success ? -1 : 0), py, px, plev * 2, SUMMON_HI_DEMON, 
					(success ? FALSE : TRUE), FALSE, success, GP_COPY, 0))
		{
			if (!success) msg_print("An angry demon appears!");
		}
		else
		{
			no_summon = TRUE;
		}

		break;
	case 160 : /* Summon Undead */
		if (summon_specific((success ? -1 : 0), py, px, plev * 2, SUMMON_UNDEAD, 
					(success ? FALSE : TRUE), FALSE, success, GP_COPY, 0))
		{
			if (!success) msg_print("An angry creature appears!");
		}
		else
		{
			no_summon = TRUE;
		}

		break;
	case 161 : /* Summon Greater Undead */
		if (summon_specific((success ? -1 : 0), py, px, plev * 2, 
					(success ? SUMMON_HI_UNDEAD_NO_UNIQUES : SUMMON_HI_UNDEAD), 
					(success ? FALSE : TRUE), FALSE, success, GP_COPY, 0))
		{
			if (!success) msg_print("An angry creature appears!");
		}
		else
		{
			no_summon = TRUE;
		}
		
		break;
	case 162 : /* Summon Dragon */
		if (summon_specific((success ? -1 : 0), py, px, plev * 2, SUMMON_DRAGON, 
					(success ? FALSE : TRUE), FALSE, success, GP_COPY, 0))
		{
			if (!success) msg_print("An angry dragon appears!");
		}
		else
		{
			no_summon = TRUE;
		}
		
		break;
	case 163 : /* Summon Ancient Dragon */
		if (summon_specific((success ? -1 : 0), py, px, plev * 2, 
					(success ? SUMMON_HI_DRAGON_NO_UNIQUES : SUMMON_HI_DRAGON), 
					(success ? FALSE : TRUE), FALSE, success, GP_COPY, 0))
		{
			if (!success) msg_print("An angry dragon appears!");
		}
		else
		{
			no_summon = TRUE;
		}

		break;
	case 164 : /* Summon Cyberdemon */
		if (summon_specific((success ? -1 : 0), py, px, (plev * 3) / 2, SUMMON_CYBER, 
					FALSE, FALSE, success, GP_COPY, 0))
		{
			if (!success) msg_print("An angry cyberdemon appears!");
		}
		else
		{
				no_summon = TRUE;
		}

		break;
	case 165 : /* Strange Summoning */
		i = randint0(4);
		if (i == 0) dummy = SUMMON_BIZARRE1;
		else if (i == 1) dummy = SUMMON_BIZARRE2;
		else if (i == 2) dummy = SUMMON_BIZARRE4;
		else dummy = SUMMON_BIZARRE5;
		
		if (summon_specific((success ? -1 : 0), py, px, (plev * 3) / 2, dummy,
					(success ? FALSE : TRUE), FALSE, success, GP_COPY, 0))
		{
			if (!success) msg_print("An angry creature appears!");
		}
		else
		{
			no_summon = TRUE;
		}
		break;
	case 166 : /* Mass Summoning */
		no_summon = TRUE;

		for (i = 0; i < 3 + (plev / 10); ++i)
		{
			if (summon_specific((success ? -1 : 0), py, px, (plev * 3) / 2, (success ? SUMMON_NO_UNIQUES : 0), 
						(success ? FALSE : TRUE), FALSE, success, GP_COPY, 0))
			{
				if (!success) msg_print("An angry creature appears!");
				no_summon = FALSE;
			}
		}
		break;
	case 167 : /* Raise the Dead */
		{
			if (raise_dead(py, px, FALSE, (bool)(!one_in_(3)), GP_COPY, 0))
			{
				msg_print("Cold winds begin to blow around you, carrying with them the stench of decay...");
				chg_virtue(V_UNLIFE, 1);
			}
			else
			{
				msg_print("Nothing happens.");
			}	
			break;
		}
	case 168 : /* Banishment */
		(void)banish_monsters(plev * 4);
		break;
	case 169 : /* Banish Evil */
		if (banish_evil(100))
		{
			msg_print("The power of your god banishes evil!");
		}
		break;
	case 170 : /* Holy Word */
		(void)dispel_evil(plev * 4);
		(void)hp_player(1000);
		(void)set_afraid(0);
		(void)set_poisoned(0);
		(void)set_stun(0);
		(void)set_cut(0);
		break;
	case 171 : /* Divine Intervention */
		(void)project(0, 1, py, px, 777, GF_HOLY_FIRE, PROJECT_KILL);
		(void)dispel_monsters(plev * 4);
		(void)slow_monsters();
		(void)stun_monsters(plev * 4);
		(void)confuse_monsters(plev * 4);
		(void)turn_monsters(plev * 4);
		(void)stasis_monsters(plev * 4);
		(void)summon_specific(-1, py, px, plev, SUMMON_ANGEL, TRUE, TRUE, TRUE, GP_ALLY, 0);
		(void)set_shero(p_ptr->shero + rand_range(25, 50));
		(void)hp_player(300);

		if (!p_ptr->fast)
		{
			(void)set_fast(randint1(20 + plev) + plev);
		}
		else
		{
			(void)set_fast(p_ptr->fast + randint1(5));
		}

		(void)set_afraid(0);
		break;
	case 172 : /* Nature's Wrath */
		(void)dispel_monsters(plev * 4);
		(void)earthquake(py, px, 20 + (plev / 2));
		(void)project(0, 1 + plev / 12, py, px,
			100 + plev, GF_DISINTEGRATE, PROJECT_KILL | PROJECT_ITEM);
		break;
	case 173 : /* Recharging */
		return recharge(plev * 4);
	case 174 : /* Bless Weapon */
		return bless_weapon();
	case 175 : /* Enchant Weapon */
		return enchant_spell(randint1(4), randint1(4), 0);
	case 176 : /* Enchant Armour */
		return enchant_spell(0, 0, rand_range(2, 5));
	case 177 : /* Elemental Brand */
		brand_weapon(0);
		break;
	case 178 : /* Poison Branding */
		brand_weapon(2);
		break;
	case 179 : /* Chaos Branding */
		brand_weapon(1);
		break;
	case 180 : /* Vampiric Branding */
		brand_weapon(3);
		break;
	case 181 : /* Trump Branding */
		brand_weapon(4);
		break;
	case 182 : /* Teleport Away */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)fire_beam(GF_AWAY_ALL, dir, plev);
		break;
	case 183 : /* Phase Door */
		teleport_player(10);
		break;
	case 184 : /* Teleport */
		teleport_player(plev * 5);
		break;
	case 185 : /* Teleport Level */
		(void)teleport_player_level();
		break;
	case 186 : /* Dimension Door */
		msg_print("You open a dimensional gate. Choose a destination.");
		return dimension_door();
	case 187 : /* Word of Recall */
		word_of_recall();
		break;
	case 188 : /* Reset Recall */
		{
			char    ppp[80];
			char    tmp_val[160];
		
			/* Prompt */
			sprintf(ppp, "Reset to which level (1-%d): ", p_ptr->max_depth);

			/* Default */
			sprintf(tmp_val, "%d", MAX(p_ptr->depth, 1));
	
			/* Ask for a level */
			if (get_string(ppp, tmp_val, 10))			
			{
				/* Extract request */
				dummy = atoi(tmp_val);
	
				/* Paranoia */
				if (dummy < 1) dummy = 1;

				/* Paranoia */
				if (dummy > p_ptr->max_depth) dummy = p_ptr->max_depth;
	
				p_ptr->max_depth = dummy;
	
				/* Accept request */
				msg_format("Recall depth set to level %d (%d').", dummy, dummy * 50);
			}
			else
			{
				return FALSE;
			}
			break;
		}
	case 189 : /* Free Path */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)destroy_door(dir);
		break;
	case 190 : /* Destroy Barriers */
		(void)destroy_doors_touch();
		break;
	case 191 : /* Telekinesis */
		if (!get_aim_dir(&dir)) return FALSE;

		fetch(dir, plev * 15, FALSE);
		break;
	case 192 : /* Alchemy */
		return alchemy();
	case 193 : /* Stone to Mud */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)wall_to_mud(dir);
		break;
	case 194 : /* Door Building */
		(void)door_creation();
		break;
	case 195 : /* Stair Building */
		(void)stair_creation();
		break;
	case 196 : /* Wall of Stone */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)wall_stone(dir);
		break;
	case 197 : /* Create Rock */
		(void)wall_stone(-2);
		break;
	case 198 : /* Earthquake */
		(void)earthquake(py, px, 10);
		break;
	case 199 : /* Word of Destruction */
		(void)destroy_area(py, px, 15);
		break;
	case 200 : /* Whirlwind Attack */
		{
			int y = 0, x = 0;
			cave_type *c_ptr;
			monster_type *m_ptr;

			for (dir = 0; dir <= 9; dir++)
			{
				y = py + ddy[dir];
				x = px + ddx[dir];

				/* paranoia */
				if (!in_bounds2(y, x)) continue;
				c_ptr = area(y, x);

				/* Get the monster */
				m_ptr = &m_list[c_ptr->m_idx];

				/* Hack -- attack monsters */
				if (c_ptr->m_idx && (m_ptr->ml || cave_floor_grid(c_ptr)))
					py_attack(y, x);
			}
		}
		break;
	case 201 : /* Alter Reality */
		alter_reality();
		break;
	case 202 : /* Polymorph Self */
		do_poly_self();
		break;
	case 203 : /* Lock */
		if (!get_aim_dir(&dir)) return FALSE;

		(void)wizard_lock(dir);
		break;
	case 204 : /* Phlogiston */
		phlogiston();
		break;
	default:
		msg_print("You cast a nonexistent or unprepared spell");
		msg_print(NULL);
	}

	if (no_summon) msg_print("Your summoning attempt failed");
	return TRUE;
}
#endif /* USE_NEW_MAGIC */

