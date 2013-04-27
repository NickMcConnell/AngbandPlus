/* File: artifact.c */

/* Purpose: Artifact code */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

static void give_activation_power(object_type *o_ptr)
{
	int type = 0, chance = 0;

	while (!type || (randint1(100) >= chance))
	{
		type = randint1(255);
		switch (type)
		{
			case ACT_SUNLIGHT:
			case ACT_BO_MISS_1:
			case ACT_BA_POIS_1:
			case ACT_BO_ELEC_1:
			case ACT_BO_ACID_1:
			case ACT_BO_COLD_1:
			case ACT_BO_FIRE_1:
			case ACT_CONFUSE:
			case ACT_SLEEP:
			case ACT_QUAKE:
			case ACT_CURE_LW:
			case ACT_CURE_MW:
			case ACT_CURE_POISON:
			case ACT_BERSERK:
			case ACT_LIGHT:
			case ACT_MAP_LIGHT:
			case ACT_DEST_DOOR:
			case ACT_STONE_MUD:
			case ACT_TELEPORT_1:
			case ACT_TELEPORT_2:
				chance = 101;
				break;
			case ACT_BA_COLD_1:
			case ACT_BA_FIRE_1:
			case ACT_DRAIN_1:
			case ACT_TELE_AWAY:
			case ACT_ESP:
			case ACT_RESIST_ALL:
			case ACT_DETECT_ALL:
			case ACT_RECALL:
			case ACT_SATIATE:
			case ACT_RECHARGE:
				chance = 85;
				break;
			case ACT_TERROR:
			case ACT_PROT_EVIL:
			case ACT_ID_PLAIN:
				chance = 75;
				break;
			case ACT_DRAIN_2:
			case ACT_VAMPIRE_1:
			case ACT_BO_MISS_2:
			case ACT_BA_FIRE_2:
			case ACT_REST_LIFE:
				chance = 66;
				break;
			case ACT_BA_COLD_3:
			case ACT_BA_ELEC_3:
			case ACT_WHIRLWIND:
			case ACT_VAMPIRE_2:
			case ACT_CHARM_ANIMAL:
				chance = 50;
				break;
			case ACT_SUMMON_ANIMAL:
				chance = 40;
				break;
			case ACT_DISP_EVIL:
			case ACT_BA_MISS_3:
			case ACT_DISP_GOOD:
			case ACT_BANISH_EVIL:
			case ACT_GENOCIDE:
			case ACT_MASS_GENO:
			case ACT_CHARM_UNDEAD:
			case ACT_CHARM_OTHER:
			case ACT_SUMMON_PHANTOM:
			case ACT_REST_ALL:
			case ACT_RUNE_EXPLO:
				chance = 33;
				break;
			case ACT_CALL_CHAOS:
			case ACT_ROCKET:
			case ACT_CHARM_ANIMALS:
			case ACT_CHARM_OTHERS:
			case ACT_SUMMON_ELEMENTAL:
			case ACT_CURE_700:
			case ACT_SPEED:
			case ACT_ID_FULL:
			case ACT_RUNE_PROT:
				chance = 25;
				break;
			case ACT_CURE_1000:
			case ACT_XTRA_SPEED:
			case ACT_DETECT_XTRA:
			case ACT_DIM_DOOR:
				chance = 10;
				break;
			case ACT_SUMMON_UNDEAD:
			case ACT_SUMMON_DEMON:
			case ACT_WRAITH:
			case ACT_INVULN:
			case ACT_ALCHEMY:
				chance = 5;
				break;
			default:
				chance = 0;
		}
	}

	/* A type was chosen... */
	o_ptr->activate = type;
	o_ptr->flags3 |= TR3_ACTIVATE;
	o_ptr->timeout = 0;
}

void random_low_resist(object_type *o_ptr)
{
	switch (randint1(4))
	{
		case 1:
			o_ptr->flags2 |= TR2_RES_ACID;
			break;
		case 2:
			o_ptr->flags2 |= TR2_RES_ELEC;
			break;
		case 3:
			o_ptr->flags2 |= TR2_RES_COLD;
			break;
		case 4:
			o_ptr->flags2 |= TR2_RES_FIRE;
			break;
	}
}

bool create_randart(object_type *o_ptr, bool a_scroll)
{
	char new_name[1024];
	u32b oldFlag1;
	u32b oldFlag2;
	u32b oldFlag3;
	int numTries, rand_range, numEgo;

	//wrong kind of item
	if ( (o_ptr->tval < TV_BOW) || (o_ptr->tval > TV_LITE) )	return FALSE;

	//already an artifact
	if (o_ptr->flags3 & TR3_INSTA_ART) return FALSE;

	numTries = 0;
	numEgo   = 0;

	/* give all artifact lites permanent light, this prevents wierd interactions between
	   refueling timeouts and activation timeouts */
	if (o_ptr->tval == TV_LITE)
	{
		o_ptr->timeout = 0;
		o_ptr->flags3 |= TR3_LITE;
	}

	//if the second apply magic didn't give us anything new, try again
	while ( numEgo < 2 )
	{
		numTries++;

		oldFlag1 = o_ptr->flags1;
		oldFlag2 = o_ptr->flags2;
		oldFlag3 = o_ptr->flags3;

		/* try to give it another ego type */
		apply_magic(o_ptr, p_ptr->depth, 30, OC_FORCE_GOOD);

		if ( (o_ptr->flags1 != oldFlag1) || (o_ptr->flags2 != oldFlag2) || (o_ptr->flags3 != oldFlag3) )
		{
			numEgo++;
		}

		//prevent infinite loops
		if (numTries>1000) break;
	}

	if ( (o_ptr->tval >= TV_DIGGING) && (o_ptr->tval <= TV_SWORD) )
	{
		//it's a weapon
		rand_range = 4;
	}
	else
	{
		//it's not
		rand_range = 3;
	}

	// give it one extra thing on top
	switch (randint0(rand_range))
	{
		case 0:
			add_ego_power(EGO_XTRA_RESIST,  o_ptr);
			break;
		case 1:
			add_ego_power(EGO_XTRA_SUSTAIN, o_ptr);
			break;
		case 2:
			add_ego_power(EGO_XTRA_ABILITY, o_ptr);
			break;
		case 3:
			add_ego_power(EGO_XTRA_SLAY,		o_ptr);
			break;
	}

	/* give one in three randarts an activation */
	if (one_in_(3)) give_activation_power(o_ptr);

	// if this doesn't have a pval, give it something.
	if (o_ptr->pval == 0)
	{
		switch (randint(20))
		{
		case 1: case 2:
			o_ptr->flags1 |= TR1_STR;
			o_ptr->pval = randint(3);
			break;
		case 3: case 4:
			o_ptr->flags1 |= TR1_CON;
			o_ptr->pval = randint(3);
			break;
		case 5: case 6:
			o_ptr->flags1 |= TR1_DEX;
			o_ptr->pval = randint(3);
			break;
		case 7: case 8:
			o_ptr->flags1 |= TR1_INT;
			o_ptr->pval = randint(3);
			break;
		case 9: case 10:
			o_ptr->flags1 |= TR1_WIS;
			o_ptr->pval = randint(3);
			break;
		case 11: case 12: case 13: case 14:
			o_ptr->flags1 |= TR1_CHR;
			o_ptr->pval =  randint(6);
			break;
		case 15:
			o_ptr->flags1 |= TR1_SPEED;
			o_ptr->pval = randint(6);
			break;
		case 16:
			o_ptr->flags1 |= TR1_BLOWS;
			o_ptr->pval = 1;
			break;
		case 17: case 18: case 19: case 20:
			o_ptr->flags1 |= TR1_STEALTH;
			o_ptr->pval = randint(6);
			break;
		}
	}

	/* take away the easy know flag, if it has it */
	o_ptr->flags3 &= ~TR3_EASY_KNOW;

	/* Apply the artifact flags */
	o_ptr->flags3 |= (TR3_IGNORE_ELEM | TR3_INSTA_ART);

	/* start off not knowing anything about it */
	o_ptr->kn_flags1 = 0;
	o_ptr->kn_flags2 = 0;
	o_ptr->kn_flags3 = 0;

	new_name[0] = 0;

	/* give it some plusses... */
	if ( (o_ptr->tval >= TV_BOOTS) && (o_ptr->tval <= TV_DRAG_ARMOR) )
	{
		o_ptr->to_a = 5 + randint1(15);
	}
	else if ( (o_ptr->tval >= TV_BOW) && (o_ptr->tval <= TV_SWORD) )
	{
		o_ptr->to_h = 5 + randint1(15);
		o_ptr->to_d = 5 + randint1(15);
	}

	if (a_scroll)
	{
		char dummy_name[80];
		dummy_name[0] = 0;

		/* Identify it fully */
		object_aware(o_ptr);
		object_known(o_ptr);
		object_mental(o_ptr);

		/* Save all the known flags */
		o_ptr->kn_flags1 = o_ptr->flags1;
		o_ptr->kn_flags2 = o_ptr->flags2;
		o_ptr->kn_flags3 = o_ptr->flags3;

		(void)identify_fully_aux(o_ptr);
		o_ptr->info |= OB_STOREB;

		if (!(get_string(dummy_name, 80,
        				 "What do you want to call the artifact? ")))
		{
			if ( (o_ptr->tval >= TV_BOW) && (o_ptr->tval <= TV_SWORD) )	get_randart_name_weapon(new_name);
				else get_randart_name_other(new_name);
		}
		else
		{
			strnfmt(new_name, 1024, "'%s'", dummy_name);
		}
	}
	else
	{
		if ( (o_ptr->tval >= TV_BOW) && (o_ptr->tval <= TV_SWORD) )	get_randart_name_weapon(new_name);
			else get_randart_name_other(new_name);
	}

	/* Save the inscription */
	o_ptr->xtra_name = quark_add(new_name);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP);

	/* Set the cost */
	o_ptr->cost = k_info[o_ptr->k_idx].cost + flag_cost(o_ptr, o_ptr->pval);

	return TRUE;
}

/*
 * Activate an artifact / random artifact or ego item
 */
bool activate_effect(object_type *o_ptr)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

	int plev = p_ptr->lev;
	int k, dir, dummy;
	byte activate;

	char o_name[256];

	/* Get the basic name of the object */
	object_desc(o_name, o_ptr, FALSE, 0, 256);

	/* Get activation */
	activate = o_ptr->activate;

	/* Normal artifacts */
	if (activate > 127)
	{
		switch (activate - 128)
		{
			case ART_GALADRIEL:
			{
				msgf("The phial wells with clear light...");
				(void)lite_area(damroll(2, 15), 3);
				o_ptr->timeout = (s16b)rand_range(10, 20);
				break;
			}

			case ART_ELENDIL:
			{
				msgf("The star shines brightly...");
				map_area();
				(void)lite_area(damroll(2, 15), 3);
				o_ptr->timeout = (s16b)rand_range(50, 100);
				break;
			}

			case ART_THRAIN:
			{
				msgf("The Jewel flashes bright red!");
				wiz_lite();
				msgf("The Jewel drains your vitality...");
				take_hit(damroll(3, 8), "the Jewel of Judgement");
				(void)detect_traps();
				(void)detect_doors();
				(void)detect_stairs();

				if (get_check("Activate recall? "))
				{
					word_of_recall();
				}

				o_ptr->timeout = (s16b)rand_range(20, 40);
				break;
			}

			case ART_RAZORBACK:
			{
				int num = damroll(5, 3);
				int y, x;
				int attempts;
				cave_type *c_ptr;

				msgf("Your armor is surrounded by lightning...");

				for (k = 0; k < num; k++)
				{
					attempts = 1000;

					while (attempts--)
					{
						scatter(&x, &y, px, py, 4);

						/* paranoia */
						if (!in_bounds2(x, y)) continue;

						c_ptr = area(x, y);
						if (cave_wall_grid(c_ptr)) continue;

						if ((y != py) || (x != px)) break;
					}

					(void)project(0, 3, x, y, 1000, GF_ELEC,
								  (PROJECT_THRU | PROJECT_STOP | PROJECT_GRID |
								   PROJECT_ITEM | PROJECT_KILL));
				}

				o_ptr->timeout = 100;
				break;
			}

			case ART_BLADETURNER:
			{
				if (!get_aim_dir(&dir)) return FALSE;
				msgf("You breathe the elements.");
				(void)fire_ball(GF_MISSILE, dir, 1000, 4);
				msgf("Your armor glows many colours...");
				(void)set_afraid(0);
				(void)set_shero(p_ptr->shero + rand_range(50, 100));
				(void)hp_player(30);
				(void)set_blessed(p_ptr->blessed + rand_range(50, 100));
				(void)set_oppose_acid(p_ptr->oppose_acid + rand_range(50, 100));
				(void)set_oppose_elec(p_ptr->oppose_elec + rand_range(50, 100));
				(void)set_oppose_fire(p_ptr->oppose_fire + rand_range(50, 100));
				(void)set_oppose_cold(p_ptr->oppose_cold + rand_range(50, 100));
				(void)set_oppose_pois(p_ptr->oppose_pois + rand_range(50, 100));
				o_ptr->timeout = 100;
				break;
			}

			case ART_SOULKEEPER:
			{
				msgf("Your armor glows a bright white...");
				msgf("You feel much better...");
				(void)hp_player(1000);
				(void)set_cut(0);
				o_ptr->timeout = 888;
				break;
			}

			case ART_BELEGENNON:
			{
				msgf("A heavenly choir sings...");
				(void)set_poisoned(0);
				(void)set_cut(0);
				(void)set_stun(0);
				(void)set_confused(0);
				(void)set_blind(0);
				(void)set_hero(p_ptr->hero + rand_range(25, 50));
				(void)hp_player(777);
				o_ptr->timeout = 300;
				break;
			}

			case ART_CELEBORN:
			{
				msgf("Your armor glows deep blue...");
				(void)genocide(TRUE);
				o_ptr->timeout = 500;
				break;
			}

			case ART_CASPANION:
			{
				msgf("Your armor glows bright red...");
				(void)destroy_doors_touch();
				o_ptr->timeout = 10;
				break;
			}

			case ART_DOR:
			case ART_TERROR:
			{
				(void)turn_monsters(40 + p_ptr->lev);
				o_ptr->timeout = 3 * (p_ptr->lev + 10);
				break;
			}

			case ART_HOLHENNETH:
			{
				msgf("Your helm glows bright white...");
				msgf("An image forms in your mind...");
				(void)detect_all();
				o_ptr->timeout = (s16b)rand_range(55, 110);
				break;
			}

			case ART_GONDOR:
			{
				msgf("Your crown glows deep blue...");
				msgf("You feel a warm tingling inside...");
				(void)hp_player(700);
				(void)set_cut(0);
				o_ptr->timeout = 250;
				break;
			}

			case ART_KERI:
			{
				object_type *q_ptr;

				msgf("Your rag feels warm for a moment...");

				/* Hack - Create the food ration */
				q_ptr = object_prep(lookup_kind(TV_FOOD, SV_FOOD_RATION));

				/* Drop the object from heaven */
				drop_near(q_ptr, -1, p_ptr->px, p_ptr->py);

				o_ptr->timeout = 100;

				break;
			}

			case ART_COLLUIN:
			{
				msgf("Your cloak glows many colours...");
				(void)set_oppose_acid(p_ptr->oppose_acid + rand_range(20, 40));
				(void)set_oppose_elec(p_ptr->oppose_elec + rand_range(20, 40));
				(void)set_oppose_fire(p_ptr->oppose_fire + rand_range(20, 40));
				(void)set_oppose_cold(p_ptr->oppose_cold + rand_range(20, 40));
				(void)set_oppose_pois(p_ptr->oppose_pois + rand_range(20, 40));
				o_ptr->timeout = 111;
				break;
			}

			case ART_HOLCOLLETH:
			{
				msgf("Your cloak glows deep blue...");
				(void)sleep_monsters_touch();
				o_ptr->timeout = 55;
				break;
			}

			case ART_THINGOL:
			{
				msgf("Your cloak glows bright yellow...");
				(void)recharge(130);
				o_ptr->timeout = 70;
				break;
			}

			case ART_COLANNON:
			{
				msgf("Your cloak twists space around you...");
				teleport_player(100);
				o_ptr->timeout = 45;
				break;
			}

			case ART_LUTHIEN:
			{
				msgf("Your cloak glows a deep red...");
				(void)restore_level();
				o_ptr->timeout = 450;
				break;
			}

			case ART_CAMMITHRIM:
			{
				msgf("Your gloves glow extremely brightly...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)fire_bolt(GF_MISSILE, dir, damroll(3, 6));
				o_ptr->timeout = 2;
				break;
			}

			case ART_PAURHACH:
			{
				msgf("Your gauntlets are covered in fire...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)fire_bolt(GF_FIRE, dir, damroll(11, 8));
				o_ptr->timeout = (s16b)rand_range(8, 16);
				break;
			}

			case ART_CORWIN:
			{
				msgf("Your gauntlets are covered in frost...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)fire_bolt(GF_COLD, dir, damroll(8, 8));
				o_ptr->timeout = (s16b)rand_range(7, 14);
				break;
			}

			case ART_PAURAEGEN:
			{
				msgf("Your gauntlets are covered in sparks...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)fire_bolt(GF_ELEC, dir, damroll(6, 8));
				o_ptr->timeout = (s16b)rand_range(6, 12);
				break;
			}

			case ART_PAURNEN:
			{
				msgf("Your gauntlets are covered in acid...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)fire_bolt(GF_ACID, dir, damroll(8, 8));
				o_ptr->timeout = (s16b)rand_range(5, 10);
				break;
			}

			case ART_FINGOLFIN:
			{
				msgf("Your cesti grows magical spikes...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)fire_bolt(GF_ARROW, dir, 250);
				o_ptr->timeout = (s16b)rand_range(90, 180);
				break;
			}

			case ART_FEANOR:
			{
				msgf("Your boots glow bright green...");
				if (!p_ptr->fast)
				{
					(void)set_fast(rand_range(20, 40));
				}
				else
				{
					(void)set_fast(p_ptr->fast + 5);
				}
				o_ptr->timeout = 200;
				break;
			}

			case ART_DAL:
			{
				msgf("Your boots glow deep blue...");
				(void)set_afraid(0);
				(void)set_poisoned(0);
				o_ptr->timeout = 5;
				break;
			}

			case ART_NARTHANC:
			{
				msgf("Your dagger is covered in fire...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)fire_bolt(GF_FIRE, dir, damroll(11, 8));
				o_ptr->timeout = (s16b)rand_range(8, 16);
				break;
			}

			case ART_NIMTHANC:
			{
				msgf("Your dagger is covered in frost...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)fire_bolt(GF_COLD, dir, damroll(8, 8));
				o_ptr->timeout = (s16b)rand_range(7, 14);
				break;
			}

			case ART_DETHANC:
			{
				msgf("Your dagger is covered in sparks...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)fire_bolt(GF_ELEC, dir, damroll(6, 8));
				o_ptr->timeout = (s16b)rand_range(6, 12);
				break;
			}

			case ART_RILIA:
			{
				msgf("Your dagger throbs deep green...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)fire_ball(GF_POIS, dir, 25, 3);
				o_ptr->timeout = (s16b)rand_range(4, 8);
				break;
			}

			case ART_BELANGIL:
			{
				msgf("Your dagger is covered in frost...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)fire_ball(GF_COLD, dir, 100, 2);
				o_ptr->timeout = (s16b)rand_range(5, 10);
				break;
			}

			case ART_ANGUIREL:
			{
				switch (randint1(13))
				{
					case 1:  case 2:  case 3:  case 4:  case 5:
						teleport_player(10);
						break;
					case 6:  case 7:  case 8:  case 9:  case 10:
						teleport_player(222);
						break;
					case 11:  case 12:
						(void)stair_creation();
						break;
					default:
						if (get_check("Leave this level? "))
						{
							if (autosave_l) do_cmd_save_game(TRUE);

							/* Leaving */
							p_ptr->leaving = TRUE;
						}
				}
				o_ptr->timeout = 35;
				break;
			}

			case ART_RINGIL:
			{
				msgf("Your sword glows an intense blue...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)fire_ball(GF_COLD, dir, 200, 2);
				o_ptr->timeout = 300;
				break;
			}

			case ART_DAWN:
			{
				msgf("You summon the Legion of the Dawn.");
				(void)summon_specific(-1, px, py, p_ptr->depth, SUMMON_DAWN,
									  TRUE, TRUE, TRUE);
				o_ptr->timeout = (s16b)rand_range(500, 1000);
				break;
			}

			case ART_ANDURIL:
			{
				msgf("Your sword glows an intense red...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)fire_ball(GF_FIRE, dir, 150, 2);
				o_ptr->timeout = 400;
				break;
			}

			case ART_THEODEN:
			{
				msgf("Your axe blade glows black...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)drain_life(dir, 200);
				o_ptr->timeout = 400;
				break;
			}

			case ART_AEGLOS:
			{
				msgf("Your spear crackles with electricity...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)fire_ball(GF_ELEC, dir, 200, 3);
				o_ptr->timeout = 500;
				break;
			}

			case ART_OROME:
			{
				msgf("Your spear pulsates...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)wall_to_mud(dir);
				o_ptr->timeout = 5;
				break;
			}

			case ART_EONWE:
			{
				msgf("Your axe lets out a long, shrill note...");
				(void)mass_genocide(TRUE);
				o_ptr->timeout = 1000;
				break;
			}

			case ART_LOTHARANG:
			{
				msgf("Your battle axe radiates deep purple...");
				(void)hp_player(100);
				(void)set_cut((p_ptr->cut / 2) - 50);
				o_ptr->timeout = (s16b)rand_range(3, 6);
				break;
			}

			case ART_ULMO:
			{
				msgf("Your trident glows deep red...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)teleport_monster(dir);
				o_ptr->timeout = 150;
				break;
			}

			case ART_AVAVIR:
			{
				msgf("Your scythe glows soft white...");

				word_of_recall();

				o_ptr->timeout = 200;
				break;
			}

			case ART_TOTILA:
			{
				msgf("Your flail glows in scintillating colours...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)confuse_monster(dir, 50);
				o_ptr->timeout = 15;
				break;
			}

			case ART_WHIRLWIND:
			{
				int y, x;
				cave_type *c_ptr;
				monster_type *m_ptr;

				msgf("Your ball and chain swings through the air...");

				for (dir = 0; dir <= 9; dir++)
				{
					y = py + ddy[dir];
					x = px + ddx[dir];

					/* paranoia */
					if (!in_bounds2(x, y)) continue;

					c_ptr = area(x, y);

					/* Get the monster */
					m_ptr = &m_list[c_ptr->m_idx];

					/* Hack -- attack monsters */
					if (c_ptr->m_idx && (m_ptr->ml || cave_floor_grid(c_ptr)))
						py_attack(x, y);
				}
				o_ptr->timeout = rand_range(50, 100);
				break;
			}

			case ART_FIRESTAR:
			{
				msgf("Your morning star rages in fire...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)fire_ball(GF_FIRE, dir, 200, 3);
				o_ptr->timeout = 100;
				break;
			}

			case ART_ENERGY:
			{
				msgf("Your scythe glows bright green...");
				if (!p_ptr->fast)
				{
					(void)set_fast(rand_range(20, 40));
				}
				else
				{
					(void)set_fast(p_ptr->fast + 5);
				}
				o_ptr->timeout = (s16b)rand_range(100, 200);
				break;
			}

			case ART_ERIRIL:
			{
				msgf("Your quarterstaff glows yellow...");
				if (!ident_spell()) return FALSE;
				o_ptr->timeout = 10;
				break;
			}

			case ART_OLORIN:
			{
				msgf("Your quarterstaff glows brightly...");
				(void)detect_all();
				(void)probing();
				(void)identify_fully();
				o_ptr->timeout = 1000;
				break;
			}

			case ART_TURMIL:
			{
				msgf("Your hammer glows white...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)drain_life(dir, 200);
				o_ptr->timeout = 70;
				break;
			}

			case ART_CATAPULT:
			{
				msgf("Your sling hums...");
				(void)set_afraid(0);
				(void)hp_player(45);
				o_ptr->timeout = 10;
				break;
			}

			case ART_BRAND:
			{
				msgf("Your crossbow glows deep red...");
				(void)brand_bolts();
				o_ptr->timeout = 999;
				break;
			}
		}

		/* Done */
		return TRUE;
	}

	/* Activate random artifacts and ego items */
	switch (o_ptr->activate)
	{
		case ACT_SUNLIGHT:
		{
			if (!get_aim_dir(&dir)) return FALSE;
			msgf("A line of sunlight appears.");
			(void)lite_line(dir);
			o_ptr->timeout = 10;
			break;
		}

		case ACT_BO_MISS_1:
		{
			msgf("The %s glows extremely brightly...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_bolt(GF_MISSILE, dir, damroll(3, 6));
			o_ptr->timeout = 2;
			break;
		}

		case ACT_BA_POIS_1:
		{
			msgf("The %s throbs deep green...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_ball(GF_POIS, dir, 25, 3);
			o_ptr->timeout = (s16b)rand_range(4, 8);
			break;
		}

		case ACT_BO_ELEC_1:
		{
			msgf("The %s is covered in sparks...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_bolt(GF_ELEC, dir, damroll(6, 8));
			o_ptr->timeout = (s16b)rand_range(6, 12);
			break;
		}

		case ACT_BO_ACID_1:
		{
			msgf("The %s is covered in acid...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_bolt(GF_ACID, dir, damroll(8, 8));
			o_ptr->timeout = (s16b)rand_range(5, 10);
			break;
		}

		case ACT_BO_COLD_1:
		{
			msgf("The %s is covered in frost...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_bolt(GF_COLD, dir, damroll(9, 8));
			o_ptr->timeout = (s16b)rand_range(7, 14);
			break;
		}

		case ACT_BO_FIRE_1:
		{
			msgf("The %s is covered in fire...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_bolt(GF_FIRE, dir, damroll(11, 8));
			o_ptr->timeout = (s16b)rand_range(8, 16);
			break;
		}

		case ACT_BA_COLD_1:
		{
			msgf("The %s is covered in frost...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_ball(GF_COLD, dir, 100, 2);
			o_ptr->timeout = 400;
			break;
		}

		case ACT_BA_FIRE_1:
		{
			msgf("The %s glows an intense red...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_ball(GF_FIRE, dir, 150, 2);
			o_ptr->timeout = 400;
			break;
		}

		case ACT_DRAIN_1:
		{
			msgf("The %s glows black...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			if (drain_life(dir, 200))
				o_ptr->timeout = (s16b)rand_range(100, 200);
			break;
		}

		case ACT_BA_COLD_2:
		{
			msgf("The %s glows an intense blue...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_ball(GF_COLD, dir, 200, 2);
			o_ptr->timeout = 300;
			break;
		}

		case ACT_BA_ELEC_2:
		{
			msgf("The crackles with electricity...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_ball(GF_ELEC, dir, 200, 3);
			o_ptr->timeout = 500;
			break;
		}

		case ACT_DRAIN_2:
		{
			msgf("The %s glows black...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			(void)drain_life(dir, 250);
			o_ptr->timeout = 400;
			break;
		}

		case ACT_VAMPIRE_1:
		{
			if (!get_aim_dir(&dir)) return FALSE;

			msgf("The %s throbs red...", o_name);
			for (dummy = 0; dummy < 3; dummy++)
			{
				(void)drain_gain_life(dir, 100);
			}
			o_ptr->timeout = 400;
			break;
		}

		case ACT_BO_MISS_2:
		{
			msgf("The %s grows magical spikes...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_bolt(GF_ARROW, dir, 250);
			o_ptr->timeout = (s16b)rand_range(90, 180);
			break;
		}

		case ACT_BA_FIRE_2:
		{
			msgf("The %s glows deep red...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_ball(GF_FIRE, dir, 250, 3);
			o_ptr->timeout = (s16b)rand_range(225, 450);
			break;
		}

		case ACT_BA_COLD_3:
		{
			msgf("The %s glows bright white...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_ball(GF_COLD, dir, 400, 3);
			o_ptr->timeout = (s16b)rand_range(325, 650);
			break;
		}

		case ACT_BA_ELEC_3:
		{
			msgf("The %s glows deep blue...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_ball(GF_ELEC, dir, 500, 3);
			o_ptr->timeout = (s16b)rand_range(425, 850);
			break;
		}

		case ACT_WHIRLWIND:
		{
			int y, x;
			cave_type *c_ptr;
			monster_type *m_ptr;

			msgf("The %s emits a blast of air...", o_name);

			for (dir = 0; dir <= 9; dir++)
			{
				y = py + ddy[dir];
				x = px + ddx[dir];

				/* paranoia */
				if (!in_bounds2(x, y)) continue;

				c_ptr = area(x, y);

				/* Get the monster */
				m_ptr = &m_list[c_ptr->m_idx];

				/* Hack -- attack monsters */
				if (c_ptr->m_idx && (m_ptr->ml || cave_floor_grid(c_ptr)))
					py_attack(x, y);
			}
			o_ptr->timeout = 250;
			break;
		}

		case ACT_VAMPIRE_2:
		{
			if (!get_aim_dir(&dir)) return FALSE;

			msgf("The %s throbs red...", o_name);

			for (dummy = 0; dummy < 3; dummy++)
			{
				(void)drain_gain_life(dir, 200);
			}

			o_ptr->timeout = 400;
			break;
		}


		case ACT_CALL_CHAOS:
		{
			msgf("The %s glows in scintillating colours...", o_name);
			call_chaos();
			o_ptr->timeout = 350;
			break;
		}

		case ACT_ROCKET:
		{
			if (!get_aim_dir(&dir)) return FALSE;
			msgf("You launch a rocket!");
			(void)fire_ball(GF_ROCKET, dir, 300 + plev, 2);
			o_ptr->timeout = 400;
			break;
		}

		case ACT_DISP_EVIL:
		{
			msgf("The %s floods the area with goodness...", o_name);
			(void)dispel_evil(p_ptr->lev * 5);
			o_ptr->timeout = (s16b)rand_range(300, 600);
			break;
		}

		case ACT_DISP_GOOD:
		{
			msgf("The %s floods the area with evil...", o_name);
			(void)dispel_good(p_ptr->lev * 5);
			o_ptr->timeout = (s16b)rand_range(300, 600);
			break;
		}

		case ACT_BA_MISS_3:
		{
			if (!get_aim_dir(&dir)) return FALSE;
			msgf("You breathe the elements.");
			(void)fire_ball(GF_MISSILE, dir, 600, 4);
			o_ptr->timeout = 500;
			break;
		}

			/* Activate for other offensive action */

		case ACT_CONFUSE:
		{
			msgf("The %s glows in scintillating colours...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			(void)confuse_monster(dir, 50);
			o_ptr->timeout = 15;
			break;
		}

		case ACT_SLEEP:
		{
			msgf("The %s glows deep blue...", o_name);
			(void)sleep_monsters_touch();
			o_ptr->timeout = 55;
			break;
		}

		case ACT_QUAKE:
		{
			msgf("The %s vibrates...", o_name);

			(void)earthquake(px, py, 10);
			o_ptr->timeout = 50;
			break;
		}

		case ACT_TERROR:
		{
			msgf("The %s emits a loud blast...", o_name);

			(void)turn_monsters(40 + p_ptr->lev);
			o_ptr->timeout = 3 * (p_ptr->lev + 10);
			break;
		}

		case ACT_TELE_AWAY:
		{
			msgf("The %s glows violet...", o_name);

			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_beam(GF_AWAY_ALL, dir, plev);
			o_ptr->timeout = 200;
			break;
		}

		case ACT_BANISH_EVIL:
		{
			if (banish_evil(200))
			{
				msgf("The power of the artifact banishes evil!");
			}
			o_ptr->timeout = (s16b)rand_range(250, 500);
			break;
		}

		case ACT_GENOCIDE:
		{
			msgf("The %s glows deep blue...", o_name);
			(void)genocide(TRUE);
			o_ptr->timeout = 500;
			break;
		}

		case ACT_MASS_GENO:
		{
			msgf("The %s lets out a long, shrill note...", o_name);
			(void)mass_genocide(TRUE);
			o_ptr->timeout = 1000;
			break;
		}

			/* Activate for summoning / charming */

		case ACT_CHARM_ANIMAL:
		{
			msgf("The %s twists in your hands...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			(void)charm_animal(dir, plev);
			o_ptr->timeout = 300;
			break;
		}

		case ACT_CHARM_UNDEAD:
		{
			msgf("The %s shudders...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			(void)control_one_undead(dir, plev);
			o_ptr->timeout = 333;
			break;
		}

		case ACT_CHARM_OTHER:
		{
			msgf("The %s fades in and out...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			(void)charm_monster(dir, plev);
			o_ptr->timeout = 400;
			break;
		}

		case ACT_CHARM_ANIMALS:
		{
			msgf("The %s hums softly...", o_name);
			(void)charm_animals(plev * 2);
			o_ptr->timeout = 500;
			break;
		}

		case ACT_CHARM_OTHERS:
		{
			msgf("The %s blinks in and out...", o_name);
			(void)charm_monsters(plev * 2);
			o_ptr->timeout = 750;
			break;
		}

		case ACT_SUMMON_ANIMAL:
		{
			msgf("You summon a beast.");
			(void)summon_specific(-1, px, py, plev, SUMMON_ANIMAL_RANGER, TRUE,
								  TRUE, TRUE);
			o_ptr->timeout = (s16b)rand_range(200, 500);
			break;
		}

		case ACT_SUMMON_PHANTOM:
		{
			msgf("You summon a phantasmal servant.");
			(void)summon_specific(-1, px, py, p_ptr->depth, SUMMON_PHANTOM,
								  TRUE, TRUE, TRUE);
			o_ptr->timeout = (s16b)rand_range(200, 400);
			break;
		}

		case ACT_SUMMON_ELEMENTAL:
		{
			bool pet = !one_in_(3);
			bool group = !(pet && (plev < 50));

			if (summon_specific
				((pet ? -1 : 0), px, py, ((plev * 3) / 2), SUMMON_ELEMENTAL,
				 group, FALSE, pet))
			{
				msgf("An elemental materializes...");

				if (pet)
					msgf("It seems obedient to you.");
				else
					msgf("You fail to control it!");
			}

			o_ptr->timeout = 750;
			break;
		}

		case ACT_SUMMON_DEMON:
		{
			bool pet = !one_in_(3);
			bool group = !(pet && (plev < 50));

			if (summon_specific
				((pet ? -1 : 0), px, py, ((plev * 3) / 2), SUMMON_DEMON, group,
				 FALSE, pet))
			{
				msgf
					("The area fills with a stench of sulphur and brimstone.");
				if (pet)
					msgf("'What is thy bidding... Master?'");
				else
					msgf
						("'NON SERVIAM! Wretch! I shall feast on thy mortal soul!'");
			}

			o_ptr->timeout = (s16b)rand_range(666, 1000);
			break;
		}

		case ACT_SUMMON_UNDEAD:
		{
			bool pet = !one_in_(3);
			bool group;
			int type;

			if (pet)
			{
				type = (plev > 47 ? SUMMON_HI_UNDEAD : SUMMON_UNDEAD);
				group = (((plev > 24) && one_in_(3)) ? TRUE : FALSE);
			}
			else
			{
				type =
					(plev > 47 ? SUMMON_HI_UNDEAD_NO_UNIQUES : SUMMON_UNDEAD);
				group = TRUE;
			}

			if (summon_specific((pet ? -1 : 0), px, py, ((plev * 3) / 2), type,
								group, FALSE, pet))
			{
				msgf
					("Cold winds begin to blow around you, carrying with them the stench of decay...");
				if (pet)
					msgf
						("Ancient, long-dead forms arise from the ground to serve you!");
				else
					msgf
						("'The dead arise... to punish you for disturbing them!'");
			}

			o_ptr->timeout = (s16b)rand_range(666, 1000);
			break;
		}

			/* Activate for healing */

		case ACT_CURE_LW:
		{
			msgf("The %s radiates light blue...", o_name);
			(void)set_afraid(0);
			(void)hp_player(30);
			o_ptr->timeout = 10;
			break;
		}

		case ACT_CURE_MW:
		{
			msgf("The %s radiates deep purple...", o_name);
			(void)hp_player(75);
			(void)set_cut((p_ptr->cut / 2) - 50);
			o_ptr->timeout = (s16b)rand_range(3, 6);
			break;
		}

		case ACT_CURE_POISON:
		{
			msgf("The %s glows deep blue...", o_name);
			(void)set_afraid(0);
			(void)set_poisoned(0);
			o_ptr->timeout = 5;
			break;
		}

		case ACT_REST_LIFE:
		{
			msgf("The %s glows a deep red...", o_name);
			(void)restore_level();
			o_ptr->timeout = 450;
			break;
		}

		case ACT_REST_ALL:
		{
			msgf("The %s glows a deep green...", o_name);
			(void)do_res_stat(A_STR);
			(void)do_res_stat(A_INT);
			(void)do_res_stat(A_WIS);
			(void)do_res_stat(A_DEX);
			(void)do_res_stat(A_CON);
			(void)do_res_stat(A_CHR);
			(void)restore_level();
			o_ptr->timeout = 750;
			break;
		}

		case ACT_CURE_700:
		{
			msgf("The %s glows deep blue...", o_name);
			msgf("You feel a warm tingling inside...");
			(void)hp_player(700);
			(void)set_cut(0);
			o_ptr->timeout = 250;
			break;
		}

		case ACT_CURE_1000:
		{
			msgf("The %s glows a bright white...", o_name);
			msgf("You feel much better...");
			(void)hp_player(1000);
			(void)set_cut(0);
			o_ptr->timeout = 888;
			break;
		}

			/* Activate for timed effect */

		case ACT_ESP:
		{
			msgf("The %s enters your thoughts...", o_name);
			(void)set_tim_esp(p_ptr->tim_esp + rand_range(25, 55));
			o_ptr->timeout = 200;
			break;
		}

		case ACT_BERSERK:
		{
			msgf("The %s angers you...", o_name);
			(void)set_shero(p_ptr->shero + rand_range(50, 100));
			(void)set_blessed(p_ptr->blessed + rand_range(50, 100));
			o_ptr->timeout = (s16b)rand_range(100, 200);
			break;
		}

		case ACT_PROT_EVIL:
		{
			msgf("The %s lets out a shrill wail...", o_name);
			k = 3 * p_ptr->lev;
			(void)set_protevil(p_ptr->protevil + randint1(25) + k);
			o_ptr->timeout = (s16b)rand_range(225, 450);
			break;
		}

		case ACT_RESIST_ALL:
		{
			msgf("The %s glows many colours...", o_name);
			(void)set_oppose_acid(p_ptr->oppose_acid + rand_range(40, 80));
			(void)set_oppose_elec(p_ptr->oppose_elec + rand_range(40, 80));
			(void)set_oppose_fire(p_ptr->oppose_fire + rand_range(40, 80));
			(void)set_oppose_cold(p_ptr->oppose_cold + rand_range(40, 80));
			(void)set_oppose_pois(p_ptr->oppose_pois + rand_range(40, 80));
			o_ptr->timeout = 200;
			break;
		}

		case ACT_SPEED:
		{
			msgf("The %s glows bright green...", o_name);
			if (!p_ptr->fast)
			{
				(void)set_fast(rand_range(20, 40));
			}
			else
			{
				(void)set_fast(p_ptr->fast + 5);
			}
			o_ptr->timeout = 250;
			break;
		}

		case ACT_XTRA_SPEED:
		{
			msgf("The %s glows brightly...", o_name);
			if (!p_ptr->fast)
			{
				(void)set_fast(rand_range(75, 150));
			}
			else
			{
				(void)set_fast(p_ptr->fast + 5);
			}
			o_ptr->timeout = (s16b)rand_range(200, 400);
			break;
		}

		case ACT_WRAITH:
		{
			msgf("The %s fades out...", o_name);
			(void)set_wraith_form(p_ptr->wraith_form +
								  rand_range(plev / 2, plev));
			o_ptr->timeout = 1000;
			break;
		}

		case ACT_INVULN:
		{
			msgf("The %s fires a beam of bright white light at you...",
					   o_name);
			(void)set_invuln(p_ptr->invuln + rand_range(8, 16));
			o_ptr->timeout = 1000;
			break;
		}

		case ACT_TELEPORT_1:
		{
			msgf("The %s twists space around you...", o_name);
			teleport_player(100);
			o_ptr->timeout = (s16b)rand_range(50, 100);
			break;
		}

			/* Activate for general purpose effect (detection etc.) */

		case ACT_LIGHT:
		{
			msgf("The %s wells with clear light...", o_name);
			(void)lite_area(damroll(2, 15), 3);
			o_ptr->timeout = (s16b)rand_range(10, 20);
			break;
		}

		case ACT_MAP_LIGHT:
		{
			msgf("The %s shines brightly...", o_name);
			map_area();
			(void)lite_area(damroll(2, 15), 3);
			o_ptr->timeout = (s16b)rand_range(50, 100);
			break;
		}

		case ACT_DETECT_ALL:
		{
			msgf("The %s glows bright white...", o_name);
			msgf("An image forms in your mind...");
			(void)detect_all();
			o_ptr->timeout = (s16b)rand_range(55, 110);
			break;
		}

		case ACT_DETECT_XTRA:
		{
			msgf("The %s glows brightly...", o_name);
			(void)detect_all();
			(void)probing();
			(void)identify_fully();
			o_ptr->timeout = 1000;
			break;
		}

		case ACT_ID_FULL:
		{
			msgf("The %s glows yellow...", o_name);
			(void)identify_fully();
			o_ptr->timeout = 750;
			break;
		}

		case ACT_ID_PLAIN:
		{
			if (!ident_spell()) return FALSE;
			o_ptr->timeout = 10;
			break;
		}

		case ACT_RUNE_EXPLO:
		{
			msgf("The %s glows bright red...", o_name);
			(void)explosive_rune();
			o_ptr->timeout = 200;
			break;
		}

		case ACT_RUNE_PROT:
		{
			msgf("The %s glows light blue...", o_name);
			(void)warding_glyph();
			o_ptr->timeout = 400;
			break;
		}

		case ACT_SATIATE:
		{
			msgf("The %s glows brown...", o_name);
			(void)set_food(PY_FOOD_MAX - 1);
			o_ptr->timeout = 200;
			break;
		}

		case ACT_DEST_DOOR:
		{
			msgf("The %s glows bright red...", o_name);
			(void)destroy_doors_touch();
			o_ptr->timeout = 10;
			break;
		}

		case ACT_STONE_MUD:
		{
			msgf("The %s pulsates...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			(void)wall_to_mud(dir);
			o_ptr->timeout = 5;
			break;
		}

		case ACT_RECHARGE:
		{
			msgf("The %s hums...", o_name);
			(void)recharge(130);
			o_ptr->timeout = 70;
			break;
		}

		case ACT_ALCHEMY:
		{
			msgf("The %s glows bright yellow...", o_name);
			(void)alchemy();
			o_ptr->timeout = 500;
			break;
		}

		case ACT_DIM_DOOR:
		{
			msgf("You open a dimensional gate. Choose a destination.");
			if (!dimension_door()) return FALSE;
			o_ptr->timeout = 100;
			break;
		}


		case ACT_TELEPORT_2:
		{
			msgf("The %s twists space around you...", o_name);
			teleport_player(100);
			o_ptr->timeout = 45;
			break;
		}

		case ACT_RECALL:
		{
			word_of_recall();

			o_ptr->timeout = 200;
			break;
		}

		default:
		{
			msgf("Unknown activation effect: %d.", o_ptr->activate);
			return FALSE;
		}
	}

	return TRUE;
}


void random_artifact_resistance(object_type *o_ptr)
{
	bool give_resistance = FALSE, give_power = FALSE;

	/* Terror Mask is for warriors... */
	if (o_ptr->activate == ART_TERROR + 128)
	{
		if (p_ptr->pclass == CLASS_WARRIOR)
		{
			give_power = TRUE;
			give_resistance = TRUE;
		}
		else
		{
			o_ptr->flags3 |=
				(TR3_CURSED | TR3_HEAVY_CURSE | TR3_AGGRAVATE | TR3_TY_CURSE);
			return;
		}
	}

	switch (o_ptr->activate - 128)
	{
		case ART_CELEBORN:
		case ART_ARVEDUI:
		case ART_CASPANION:
		case ART_HITHLOMIR:
		case ART_ROHIRRIM:
		case ART_CELEGORM:
		case ART_ANARION:
		case ART_THRANDUIL:
		case ART_LUTHIEN:
		case ART_THROR:
		case ART_THORIN:
		case ART_NIMTHANC:
		case ART_DETHANC:
		case ART_NARTHANC:
		case ART_STING:
		case ART_TURMIL:
		case ART_THALKETTOTH:
		{
			/* Give a resistance */
			give_resistance = TRUE;
		}
			break;
		case ART_MAEDHROS:
		case ART_GLAMDRING:
		case ART_ORCRIST:
		case ART_ANDURIL:
		case ART_ZARCUTHRA:
		case ART_GURTHANG:
		case ART_HARADEKKET:
		case ART_BRAND:
		case ART_DAWN:
		{
			/* Give a resistance OR a power */
			if (one_in_(2)) give_resistance = TRUE;
			else
				give_power = TRUE;
		}
			break;
		case ART_BERUTHIEL:
		case ART_FINGOLFIN:
		case ART_THINGOL:
		case ART_ULMO:
		case ART_OLORIN:
		{
			/* Give a power */
			give_power = TRUE;
		}
			break;
		case ART_GONDOR:
		case ART_AULE:
		{
			/* Give both */
			give_power = TRUE;
			give_resistance = TRUE;
		}
			break;
	}

	if (give_power)
	{
		add_ego_power(EGO_XTRA_ABILITY, o_ptr);
	}

	if (give_resistance)
	{
		add_ego_power(EGO_XTRA_RESIST, o_ptr);
	}
}


/*
 * Create the artifact of the specified number
 */
void create_named_art(int a_idx, int x, int y)
{
	object_type *q_ptr;
	int i;

	artifact_type *a_ptr = &a_info[a_idx];

	/* Ignore "empty" artifacts */
	if (!a_ptr->name) return;

	/* Acquire the "kind" index */
	i = lookup_kind(a_ptr->tval, a_ptr->sval);

	/* Oops */
	if (!i) return;

	/* Create the artifact */
	q_ptr = object_prep(i);

	/* Set the activation */
	q_ptr->activate = a_idx + 128;

	/* Do not make another one */
	a_ptr->cur_num = 1;

	/* Save the artifact flags */
	q_ptr->flags1 |= a_ptr->flags1;
	q_ptr->flags2 |= a_ptr->flags2;
	q_ptr->flags3 |= a_ptr->flags3;

	/* Extract the fields */
	q_ptr->pval = a_ptr->pval;
	q_ptr->ac = a_ptr->ac;
	q_ptr->dd = a_ptr->dd;
	q_ptr->ds = a_ptr->ds;
	q_ptr->to_a = a_ptr->to_a;
	q_ptr->to_h = a_ptr->to_h;
	q_ptr->to_d = a_ptr->to_d;
	q_ptr->weight = a_ptr->weight;

	/* Save the inscription */
	q_ptr->xtra_name = quark_add(a_name + a_ptr->name);

	random_artifact_resistance(q_ptr);

	if (!a_ptr->cost)
	{
		/* Hack -- "worthless" artifacts */
		q_ptr->cost = 0L;
	}
	else
	{
		/* Hack - use the artifact price */
		q_ptr->cost = k_info[q_ptr->k_idx].cost + a_ptr->cost;
	}

	/* Drop the artifact from heaven */
	drop_near(q_ptr, -1, x, y);
}
