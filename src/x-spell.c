/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * 						Jeff Greene, Diego Gonzalez, Anssi Ramela
 *
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 2, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */
#include "angband.h"



/*
 * Helper function for casting the "Prismatic Spray" spell.
 */
static void cast_prismatic_spray(int dir, int dam)
{
	int gftype;

	switch (randint(p_ptr->lev + 25) / 5)
	{
		/*
		 * Should only be possible for level 50 and even then odds
		 * should be 1/75 of this hapening. An easter egg type of
		 * effect. :) -AR
		 */
		case (15):
		{
			gftype = GF_CHAOS;
			dam *= 12;
			msg_print("You conjure forth a massive torrent of raw chaos!");
			break;
		}
		case (14):
		{
			gftype = GF_INERTIA;
			dam *= 2;
			msg_print("You conjure forth a torrent of inertia.");
			break;
		}
		case (13):
		{
			gftype = GF_GRAVITY;
			dam *= 2;
			msg_print("You conjure forth a torrent of gravity.");
			break;
		}
		case (12):
		{
			gftype = GF_CONFUSION;
			dam *= 3;
			msg_print("You conjure forth a torrent of confusion.");
			break;
		}
		case (11):
		{
			gftype = GF_FORCE;
			dam *= 3;
			msg_print("You conjure forth a torrent of pure force.");
			break;
		}
		case (10):
		{
			gftype = GF_TIME;
			dam *= 3;
			msg_print("You conjure forth a torrent of time.");
			break;
		}
		case (9):
		{
			gftype = GF_STATIC;
			dam *= 4;
			msg_print("You conjure forth a torrent of anti-magic static.");
			break;
		}
		case (8):
		{
			gftype = GF_NEXUS;
			dam *= 4;
			msg_print("You conjure forth a torrent of dimensional energy.");
			break;
		}
		case (7):
		{
			gftype = GF_DISENCHANT;
			dam *= 4;
			msg_print("You conjure forth a torrent of disenchantment.");
			break;
		}
		case (6):
		{
			gftype = GF_SHARD;
			dam *= 4;
			msg_print("You conjure forth a torrent of shrapnel.");
			break;
		}
		case (5):
		{
			gftype = GF_NETHER;
			dam *= 5;
			msg_print("You conjure forth a torrent of nether.");
			break;
		}
		case (4):
		{
			gftype = GF_COLD;
			dam *= 5;
			msg_print("You conjure forth a torrent of frost.");
			break;
		}
		case (3):
		{
			gftype = GF_FIRE;
			dam *= 5;
			msg_print("You conjure forth a torrent of flames.");
			break;
		}
		case (2):
		{
			gftype = GF_ELEC;
			dam *= 5;
			msg_print("You conjure forth a torrent of electricity.");
			break;
		}
		case (1):
		{
			gftype = GF_ACID;
			dam *= 5;
			msg_print("You conjure forth a torrent of acid.");
			break;
		}
		default:
		{
			gftype = GF_POIS;
			dam *= 5;
			msg_print("You conjure forth a torrent of poison.");
			break;
		}
	}

	fire_arc(gftype, dir, dam, 0, 60);
}



static int beam_chance(void)
{
	int plev = p_ptr->lev;
	return ((cp_ptr->flags & CF_BEAM) ? plev : (plev / 2));
}




/* Report if a spell needs a target*/
bool spell_needs_aim(int tval, int spell)
{
	if (tval == TV_MAGIC_BOOK)
	{
		switch (spell)
		{
			case SPELL_MAGIC_MISSILE:
			case SPELL_STINKING_CLOUD:
			case SPELL_CONFUSE_MONSTER:
			case SPELL_SHOCK_WAVE:
			case SPELL_LIGHTNING_BOLT:
			case SPELL_FROST_BOLT:
			case SPELL_FROST_BALL:
			case SPELL_FIRE_BOLT:
			case SPELL_FIRE_BALL:
			case SPELL_TRAP_DOOR_DESTRUCTION:
			case SPELL_SLEEP_MONSTER:
			case SPELL_SPEAR_OF_LIGHT:
			case SPELL_ICE_BOLT:
			case SPELL_TURN_STONE_TO_MUD:
			case SPELL_PRISMATIC_SPRAY:
			case SPELL_POLYMORPH_OTHER:
			case SPELL_SHARD_STORM:
			case SPELL_SLOW_MONSTER:
			case SPELL_CALL_LIGHTNING:
			case SPELL_TELEPORT_OTHER:
			case SPELL_BEDLAM:
			case SPELL_WATER_BOLT:
			case SPELL_HURRICANE:
			case SPELL_CLOUD_KILL:
			case SPELL_ICE_STORM:
			case SPELL_PLASMA_BOLT:
			case SPELL_MANA_STORM:
			case SPELL_NOVA:
			case SPELL_REND_SOUL:
			case SPELL_RIFT:
			case SPELL_DARKNESS_STORM:
			case SPELL_METEOR_STORM:
			case SPELL_MANA_BOLT:
			case SPELL_WAIL_OF_THE_BANSHEE:
			{
				return TRUE;
			}

			default: return FALSE;
		}
	}

	else if (tval == TV_DRUID_BOOK)
	{
		switch (spell)
		{
			case DRUID_ACID_BOLT:
			case DRUID_POISON_CLOUD:
			case DRUID_TURN_STONE_TO_MUD:
			case DRUID_FROST_BEAM:
			case DRUID_TRAP_DOOR_DESTRUCTION:
			case DRUID_SPEAR_OF_LIGHT:
			case DRUID_FIRE_BEAM:
			case DRUID_STERILIZE:
			case DRUID_LIFE_DRAIN_BURST:
			case DRUID_FROST_BALL:
			case DRUID_FIRE_BALL:
			case DRUID_DRAIN_LIFE_ARC:
			case DRUID_SANDSTORM:
			case DRUID_CHANNEL_LIGHTNING:
			case DRUID_MASTER_ELEMENTS:
			case DRUID_STEAL_POWERS:
			{
				return TRUE;
			}
			default: return FALSE;
		}


	}
	else if (tval == TV_PRAYER_BOOK)
	{
		switch (spell)
		{
			case PRAYER_SHOCK_BOLT:
			case PRAYER_SCARE_MONSTER:
			case PRAYER_CONFUSE_MONSTER:
			case PRAYER_SUN_BEAM:
			case PRAYER_ORB_OF_DRAINING:
			case PRAYER_SUN_BURST:
			case PRAYER_UNBARRING_WAYS:
			case PRAYER_TELEPORT_OTHER:
			{
				return TRUE;
			}

			default: return FALSE;
		}
	}

	/*OOPS*/
	else return (FALSE);
}


/*
 * Cast a spell, or output spell info, description.
 */
cptr do_mage_spell(int mode, int spell, int dir)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int plev = p_ptr->lev;

	/* Spell-specific variables */
	int dam, dam1;
	int dur, dur1, dur2;
	int dice, sides;
	int rad, beam;

	/* Function modes */
	bool cast = (mode == MODE_SPELL_CAST);
	bool name = (mode == MODE_SPELL_NAME);
	bool desc = (mode == MODE_SPELL_DESC);
	bool desc_short = (mode == MODE_SPELL_DESC_SHORT);

	/* Return the spell type name if that is what is being asked*/
	if (mode == MODE_SPELL_NOUN) return "spell";
	if (mode == MODE_SPELL_VERB) return "cast";

	/* Hack -- chance of "beam" instead of "bolt" */
	beam = beam_chance();

	/* Spells. */
	switch (spell)
	{
		case SPELL_MAGIC_MISSILE:
		{

			dice = 3 + ((plev - 1) / 5);
			sides = 4;

			if (game_mode == GAME_NPPMORIA)
			{
				dice = 2;
				sides = 6;
			}

			if (name) return ("Magic Missile");
			if (desc) return (format("Fires a magic missile for %dd%d hp damage.", dice, sides));
			if (desc_short) return (format("dam %dd%d", dice, sides));
			if (cast)
			{
				fire_bolt_or_beam(beam-10, GF_MISSILE, dir, damroll(dice, sides));
			}

			break;
		}

		case SPELL_DETECT_MONSTERS:
		{
			if (name) return ("Detect Monsters");
			if (desc) return ("Detects nearby monsters that are not invisible.");
			if (desc_short) return (format("rad %d", DETECT_RADIUS));
			if (cast)
			{
				(void)detect(DETECT_RADIUS, DETECT_MONSTERS);
			}

			break;
		}

		case SPELL_PHASE_DOOR:
		{
			dam = 10;

			if (name) return ("Phase Door");
			if (desc) return (format("A range %d random minor displacement.", dam));
			if (desc_short) return (format("range %d", dam));
			if (cast)
			{
				teleport_player(dam, FALSE);
			}

			break;
		}

		case SPELL_LIGHT_AREA:
		{
			dice = 2;
			sides = (plev / 2);
			rad = (plev / 10) + 1;

			if (game_mode == GAME_NPPMORIA) sides = (plev / 4);

			if (name) return ("Light Area");
			if (desc) return (format("Permanently lights up a room or a radius %d area.", rad));
			if (desc_short) return (format("radius %d", rad));
			if (cast)
			{
				(void)light_area(damroll(dice, sides), rad);
			}

			break;
		}

		case SPELL_TREASURE_DETECTION:
		{
			if (name) return ("Detect Treasure");
			if (desc) return ("Detects nearby treasure.");
			if (desc_short) return (format("rad %d", DETECT_RADIUS));
			if (cast)
			{
				(void)detect(DETECT_RADIUS, DETECT_ALL_TREASURE);
			}

			break;
		}

		case SPELL_CURE_LIGHT_WOUNDS:
		{
			dice = 2;
			sides = 10;

			if (game_mode == GAME_NPPMORIA)
			{
				dice = 4;
				sides = 4;
			}

			if (name) return ("Cure Light Wounds");
			if (desc) return (format("Reduces cuts and heals %dd%d hp", dice, sides));
			if (desc_short) return (format("heal %dd%d", dice, sides));
			if (cast)
			{
				(void)hp_player(damroll(dice, sides));
				(void)set_cut(p_ptr->timed[TMD_CUT] - 15);
			}
			break;
		}

		case SPELL_OBJECT_DETECTION:
		{
			if (name) return ("Detect Objects");
			if (desc) return ("Detects nearby objects.");
			if (desc_short) return (format("rad %d", DETECT_RADIUS));
			if (cast)
			{
				(void)(void)detect(DETECT_RADIUS, DETECT_OBJECTS);
			}

			break;
		}

		case SPELL_FIND_TRAPS_DOORS:
		{
			if (name) return ("Find Hidden Traps/Doors");
			if (desc) return ("Detects nearby traps, doors and stairs.");
			if (desc_short) return (format("rad %d", DETECT_RADIUS));
			if (cast)
			{
				(void)detect(DETECT_RADIUS, DETECT_DOORS_STAIRS_TRAPS);
			}

			break;
		}

		case SPELL_STINKING_CLOUD:
		{
			rad = 2;
			dam = 10 + (plev / 2);

			if (game_mode == GAME_NPPMORIA) dam = 12;

			if (name) return ("Stinking Cloud");
			if (desc) return (format("Fires a radius %d cloud of poison for %d hp damage.", rad, dam));
			if (desc_short) return (format("dam %d", dam));
			if (cast)
			{
				fire_ball(GF_POIS, dir, dam, rad);
			}

			break;
		}

		case SPELL_CONFUSE_MONSTER:
		{
			if (name) return ("Confuse Monster");
			if (desc) return ("Attempts to confuse one monster.");
			if (desc_short) return (format(""));
			if (cast)
			{
				(void)confuse_monster(dir, plev);
			}

			break;
		}

		/*
		This spell is meant to be very strong at point blank range but then weaken fast as the range extends.
		As such, it might make sense to boost the damage and broaden the arc to even wider than 60 degrees to
		achieve the desired result. Considering how sound behaves in real life it might be fun to make the arc
	   270 degrees and call it something like "Wail of the Banshee" or "War Cry" or something. -AR
		*/

		case SPELL_SHOCK_WAVE:
		{
			dam = 20;
			dice = 1 + ((plev - 1 ) / 5); /*Reaches max damage (20+10d11, average 80) at plev 46. */
			sides = 11;

			if (name) return ("Shock Wave");
			if (desc) return (format("Fires an arc of sonic energy for %d+%dd%d hp damage.", dam, dice, sides));
			if (desc_short) return (format("dam %d+%dd%d", dam, dice, sides));
			if (cast)
			{
				fire_arc(GF_SOUND, dir, dam + damroll(dice, sides), 0, 60);
			}

			break;
		}

		case SPELL_TRAP_DOOR_DESTRUCTION:
		{
			if (name) return ("Trap/Door Destruction");
			if (desc) return ("Fires a beam that disarms traps and chests and destroys doors.");
			if (desc_short) return (format(""));
			if (cast)
			{
				destroy_door(dir);
			}

			break;
		}

		case SPELL_SLEEP_MONSTER:
		{
			if (name) return ("Sleep Monster");
			if (desc) return ("Attempts to put a monster to sleep.");
			if (desc_short) return (format(""));
			if (cast)
			{
				(void)sleep_monster(dir);
			}

			break;
		}

		case SPELL_CURE_POISON:
		{
			if (name) return ("Cure Poison");
			if (desc) return ("Cures the player of poison.");
			if (desc_short) return (format(""));
			if (cast)
			{
				(void)clear_timed(TMD_POISONED, TRUE);
			}

			break;
		}

		case SPELL_TELEPORT_SELF:
		{
			dam = (plev * 5);

			if (name) return ("Teleport Self");
			if (desc) return (format("Random major displacement up to %d squares.", dam));
			if (desc_short) return (format(""));
			if (cast)
			{
				teleport_player(dam, FALSE);
			}

			break;
		}

		case SPELL_SPEAR_OF_LIGHT:
		{
			dice = 6;
			sides = 8;

			if (name) return ("Spear of Light");
			if (desc) return (format("Fires a line of weak light.  %dd%d hp damage for light-hating creatures.", dice, sides));
			if (desc_short) return (format("dam %dd%d", dice, sides));
			if (cast)
			{
				msg_print("A line of blue shimmering light appears.");
				(void)light_line(dir, damroll(dice, sides));
			}

			break;
		}

		/*
		Magic Missile is average of 30 damage for 1 at level 50.

		I'm powering up the Mage's bolt spells significantly, since I see them as a key element
		for Mages due to the BEAM flag and the Druid's almost total reliance on non-bolt projections.
		Non- Magic Missile bolts will follow a guide line of 20 damage per mana average on plev 50,
		which is slightly less than Orb of Drainings average of 23 damage per mana at plev 50. This is
		done to lessen the reliance on Magic Missile. The uniformity of damage to mana ratios needs to
		be counteracted somehow in order to minimize the amount of spells that become obsolete as the
		game progresses. The HURT_FIRE, HURT_COLD, RES_WATER, RES_PLAS, EVIL, RES_NETHR, RES_COLD
		(partial resistance to ice) and RES_FIRE (partial resistance to plasma) could be used for that.
		In many cases, by accident they already are. For example HURT_COLD keeps ICE bolt useful even
		after Water Bolt has achieved 0% fail rate by offering a better damage per mana output against
		Fire based mosters that are cold sensitive, most notably red dragons.

		Overall I hope to make Mages better than Druids at confonting single monsters while Druid
		out perform Mages at crowd control.

		The fact that Orb does only 11.5 damage on average to non-evil is trivial due to the high % of
	    evil in the dungeon. (As it should be, since it is Morgoth's dungeon, after all) -AR
		*/

		case SPELL_ICE_BOLT:
		{
			dice = 5 + ((plev - 3) / 3);
			sides = 9;

			if (name) return ("Ice Bolt");
			if (desc) return (format("Fires a bolt or beam of ice for %dd%d hp damage.", dice, sides));
			if (desc_short) return (format("dam %dd%d", dice, sides));
			if (cast)
			{
						fire_bolt_or_beam(beam, GF_ICE, dir, damroll(dice, sides));
			}

			break;
		}

		case SPELL_TURN_STONE_TO_MUD:
		{
			dam = 20 + randint(30);

			if (name) return ("Turn Stone to Mud");
			if (desc) return ("Removes one section of a normal wall to floor.");
			if (desc_short) return ("");
			if (cast)
			{
				(void)wall_to_mud(dir, dam);
			}

			break;
		}

		case SPELL_SATISFY_HUNGER:
		{
			if (name) return ("Satisfy Hunger");
			if (desc) return ("Magically renders the player well-fed.");
			if (desc_short) return ("");
			if (cast)
			{
				(void)set_food(PY_FOOD_MAX - 50);
			}

			break;
		}

		case SPELL_RECHARGE_ITEM_I:
		{
			if (name) return ("Lesser Recharging");
			if (desc) return ("Attempts to recharge a wand, staff, or rod.");
			if (desc_short) return ("");
			if (cast)
			{
				(void)recharge(2 + (plev / 5), FALSE, 15 + (plev / 5));
			}

			break;
		}

		case SPELL_PRISMATIC_SPRAY: /* Replaces Wonder with something more consistently usefull. -AR*/
		{
			dam = 25 + 2 * plev;

			if (name) return ("Prismatic Spray");
			if (desc) return (format("Invokes a cone of a random element with a "
				"damage between %d and %d, depending on the element.", 2 * dam, 5 * dam));
			if (desc_short) return (format("dam range %d - %d", 2 * dam, 5 * dam));
			if (cast)
			{
				cast_prismatic_spray(dir, dam);
			}

			break;
		}

		case SPELL_POLYMORPH_OTHER:
		{
			if (name) return ("Polymorph Other");
			if (desc) return ("Attempts to change a monster into a different monster race.");
			if (desc_short) return ("");
			if (cast)
			{
				(void)poly_monster(dir);
			}

			break;
		}

		case SPELL_IDENTIFY:
		{
			if (name) return ("Identify");
			if (desc) return ("Identifies an object.");
			if (desc_short) return ("");
			if (cast)
			{
				if (!ident_spell()) return (NULL);
			}

			break;

		}

		case SPELL_MASS_SLEEP:
		{
			if (name)
			{
				if (game_mode == GAME_NPPMORIA) return ("Sleep III");
				else return ("Mass Sleep");
			}
			if (desc) return ("Attempts to sleep all monsters in LOS.");
			if (desc_short) return ("");
			if (cast)
			{
				(void)sleep_monsters(damroll (3, p_ptr->lev));
			}

			break;
		}

		case SPELL_SHARD_STORM:
		{
			rad = 4;

			/*
			* Note the damage of the final shard cloud is 15% of the
			* damage listed below, according to terrain.txt
			*
			* I gave this a weird damage progression to in an attempt to
			* make it more usefull for levels 30+ while keeping it at
			* roughly the same powerlevel in the early game.
			*/

			dam = 20 + (plev * 6);

			if (plev>30)
			   dam+=(plev-30)*9;


			dam1 = (dam * f_info[FEAT_SHARD].x_damage) / 100;

			if (name) return ("Shard Storm");
			if (desc) return (format("Creates a radius %d cloud of shards that causes %d hp damage.", rad, dam1));
			if (desc_short) return (format("rad %d dam %d", rad, dam1));
			if (cast)
			{
				fire_effect_orb(GF_SHARD, dir, dam, rad);
			}


			break;
		}

		case SPELL_SLOW_MONSTER:
		{
			if (name) return ("Slow Monster");
			if (desc) return ("Attempts to slow a monster.");
			if (desc_short) return ("");
			if (cast)
			{
				(void)slow_monster(dir);
			}

			break;
		}

		case SPELL_CALL_LIGHTNING:
		{
			rad = 5;

			/*
			 * Note the damage of the final static is 10% of the
			 * damage listed below, according to terrain.txt
			 * Final damage is double the damage od Drain Life Bursts
			 * at double the mana cost, a big bang for bick bucks theme
			 * I have for Mages. Maybe cut it down to 150% damage for 150%
			 * mana later if it is too fast a damage dealing rate. -AR
			 */
			dam = 2400 + (plev * 40);  /*240 + plev times 4 damage*/
			dam1 = (dam * f_info[FEAT_SPARKS].x_damage) / 100;

			if (name) return ("Call Lightning");
			if (desc) return (format("Creates a radius %d orb of time released lightning strikes that cause %d hp damage each.", rad, dam1));
			if (desc_short) return (format("rad %d dam %d", rad, dam1));
			if (cast)
			{
				fire_effect_orb(GF_ELEC_BURST, dir, dam, rad);
			}

			break;
		}

		case SPELL_RECHARGE_ITEM_II:
		{
			rad = 50 + plev;
			if (game_mode == GAME_NPPMORIA) rad = 60;

			if (name) return ("Greater Recharging");
			if (desc) return ("Attempts to recharge a wand, staff, or rod.");
			if (desc_short) return ("");
			if (cast)
			{
				(void)recharge(rad, FALSE, 25 + (plev / 2));
			}

			break;
		}

		case SPELL_TELEPORT_OTHER:
		{
			if (name) return ("Teleport Other");
			if (desc) return ("Attempts to teleport a monster away.");
			if (desc_short) return ("");
			if (cast)
			{
				(void)teleport_monster(dir);
			}

			break;
		}

		case SPELL_BEDLAM:
		{
			rad = 4;

			/*
			* Note the damage of the final confusion cloud is 15% of the
			* damage listed below, according to terrain.txt
			*
			* Bedlam is now a cloud of confusion -AR
			*/
			dam = 120 + (plev * 8);
			dam1 = (dam * f_info[FEAT_CONFUSION].x_damage) / 100;

			if (name) return ("Bedlam");
			if (desc) return (format("Creates a radius %d cloud of confusion that causes %d hp damage.", rad, dam1));
			if (desc_short) return (format("rad %d dam %d", rad, dam1));
			if (cast)
			{
				fire_effect_orb(GF_CONFUSION, dir, dam, rad);
			}


			break;
		}

		case SPELL_WATER_BOLT: /*The culmination of a mages pre-Raal's arsenal -AR*/
		{
			dice =  3 + (plev + 3) / 3;
			sides = 19;

			if (name) return ("Water Bolt");
			if (desc) return (format("Fires a bolt or beam of water for %dd%d hp damage.", dice, sides));
			if (desc_short) return (format("dam %dd%d", dice, sides));
			if (cast)
			{
				fire_bolt_or_beam(beam, GF_WATER, dir, damroll(dice, sides));
			}

			break;
		}

		case SPELL_WORD_OF_DESTRUCTION:
		{
			rad = 15;

			if (name) return ("Word of Destruction");
			if (desc) return (format("Creates a radius %d earthquake.  Deletes all non-quest monsters.", rad));
			if (desc_short) return (format("rad %d", rad));
			if (cast)
			{
				destroy_area(py, px, rad);
			}

			break;
		}

		case SPELL_BANISHMENT:
		{
			if (name) return ("Banishment");
			if (desc) return ("Banishes one type of monster.  Uniques and quest monsters are unaffected");
			if (desc_short) return ("");
			if (cast)
			{
				(void)banishment();
			}

			break;
		}

		case SPELL_DOOR_CREATION:
		{
			if (name) return ("Door Creation");
			if (desc) return ("Creates a barrier of doors around you.");
			if (desc_short) return ("");
			if (cast)
			{
				(void)door_creation();
			}

			break;
		}

		case SPELL_STAIR_CREATION:
		{
			if (name) return ("Stair Creation");
			if (desc) return ("Creates a staircase nearby.  Random choice between up or down, except on quest levels.");
			if (desc_short) return ("");
			if (cast)
			{
				(void)stair_creation();
			}

			break;
		}

		case SPELL_TELEPORT_LEVEL:
		{
			if (name) return ("Teleport Level");
			if (desc) return ("Immediately takes you to the next level up or down.");
			if (desc_short) return ("");
			if (cast)
			{
				if(!teleport_player_level(SOURCE_PLAYER)) return (NULL);
			}

			break;
		}

		case SPELL_EARTHQUAKE:
		{
			rad = 10;

			if (name) return ("Earthquake");
			if (desc) return (format("Creates a radius %d earthquake centered on the player.", rad));
			if (desc_short) return (format("rad %d", rad));
			if (cast)
			{
				earthquake(py, px, rad, FALSE);
			}

			break;
		}

		case SPELL_WORD_OF_RECALL:
		{
			if (name) return ("Word of Recall");
			if (desc) return ("Recalls you to the town, or to the recall level in the dungeon.");
			if (desc_short) return ("");
			if (cast)
			{
				if (!set_recall()) return (NULL);
			}

			break;
		}

		case SPELL_HURRICANE:
		{
			dam = 50 + (plev * 4);
			rad = 2;
			/*Big damage for big mana. Final damage will be 12.5 points per mana,
		    * a bit better than Druid Fire Ball, Druid Frost Ball is almost
			* exactly 12.9. -AR
		    */


			if (name) return ("Hurricane");
			if (desc) return (format("Conjures forth a radius %d storm of wind and water for %d hp damage.", rad, dam));
			if (desc_short) return (format("rad %d dam %d", rad, dam));
			if (cast)
			{
				fire_ball(GF_WATER, dir, dam, rad);
			}

			break;
		}

		case SPELL_CLOUD_KILL:
		{
			rad = 4;

			/*
			* Note the damage of the final Nether cloud is 30% of the
			* damage listed below, according to terrain.txt
			*
			* Cloudkill is now a cloud of nether, double damage and cost
			* of Shard Storm. It now reminds it's D&D 3.5 counterpart. -AR
			*/
			dam = 60 + (plev * 4);

			if (plev > 40) dam += (plev - 40) * 24;

			dam1 = (dam * f_info[FEAT_NETHER].x_damage) / 100;

			if (name) return ("Cloudkill");
			if (desc) return (format("Creates a radius %d cloud of nether that causes %d hp damage.", rad, dam1));
			if (desc_short) return (format("rad %d dam %d", rad, dam1));
			if (cast)
			{
				fire_effect_orb(GF_NETHER, dir, dam, rad);
			}


			break;
		}

		case SPELL_ICE_STORM: /* Moved down to make room for more powerful spells. Final damage ratio is 12 damage for 1 mana. -AR*/
		{
			dam = 160 + (plev * 4);
			rad = 3;

			if (name) return ("Ice Storm");
			if (desc) return (format("Fires a radius %d ball of ice for %d hp damage.", rad, dam));
			if (desc_short) return (format("rad %d dam %d", rad, dam));
			if (cast)
			{
				fire_ball(GF_ICE, dir, dam, rad);
			}

			break;
		}

		case SPELL_PLASMA_BOLT: /*Most powerful bolt for pre-Kelek's Mages. -AR */
		{
			dice =  15 + (plev / 2);
			sides = 19;

			if (name) return ("Plasma Bolt");
			if (desc) return (format("Fires a bolt or beam of plasma for %dd%d hp damage.", dice, sides));
			if (desc_short) return (format("dam %dd%d", dice, sides));
			if (cast)
			{
				fire_bolt_or_beam(beam, GF_PLASMA, dir, damroll(dice, sides));
			}

			break;
		}

		case SPELL_METEOR_STORM:
		{
			rad = 5;

			/*
			 * Note the damage of the final static is 30% of the
			 * damage listed below, according to terrain.txt
			 * Final damage is 990 hp per hit! A monumental mana cost is
			 * required of course, at least 60 or 75 mana. -AR
			 */
			dam = 1800 + (plev * 30);  /*540 + plev times 9 damage for each meteor*/
			dam1 = (dam * f_info[FEAT_METEOR_BURST].x_damage) / 100;

			if (name) return ("Meteor Storm");
			if (desc) return (format("Creates a radius %d meteor strike that causes %d hp damage.", rad, dam1));
			if (desc_short) return (format("rad %d dam %d", rad, dam1));
			if (cast)
			{
				fire_effect_orb(GF_METEOR, dir, dam, rad);
			}

			break;
		}

		case SPELL_MANA_STORM:
		{
			dam = 160 + (plev * 10);
			rad = 3;

			if (name) return ("Mana Storm");
			if (desc) return (format("Fire a radius %d storm of mana for %d hp damage.", rad, dam));
			if (desc_short) return (format("rad %d dam %d", rad, dam));
			if (cast)
			{
				fire_ball(GF_MANA, dir, dam, rad);
			}

			break;
		}
		case SPELL_DETECT_INVISIBLE:
		{
			if (name) return ("Detect Invisible");
			if (desc) return ("Detects invisible monsters.");
			if (desc_short) return (format("rad %d", DETECT_RADIUS));
			if (cast)
			{
				(void)detect(DETECT_RADIUS, DETECT_INVISIBLE);
			}

			break;

		}

		case SPELL_DETECT_ENCHANTMENT:
		{
			if (name) return ("Detect Enchantment");
			if (desc) return ("Detects nearby enchanted objects.");
			if (desc_short) return (format("rad %d", DETECT_RADIUS));
			if (cast)
			{
				(void)detect(DETECT_RADIUS, DETECT_ENCHANTMENT);
			}

			break;
		}

		case SPELL_NOVA:
			{
			/* A ball of GF_LIGHT allows a Mage to light up a distant area and hit
		    * vulnerable creatures for good damage. 8/1 final damage/mana for
			* normals, 20/1 for vulnerable creatures. Might be out of flavour for
			* mages though, so considering GF_PLASMA or possibly something else
			* with a different spell name. -AR
			*/
			dam = 60 + ((plev * 6) / 5);
			rad = 2;

			if (name) return ("Nova");
			if (desc) return (format("Fires a radius %d explosion of powerful light for %d hp damage.", rad, dam));
			if (desc_short) return (format("rad %d dam %d", rad, dam));
			if (cast)
			{
				fire_ball(GF_LIGHT, dir, dam, rad);
			}

			break;
		}

		case SPELL_REND_SOUL:
		{
			dice =  10 + ((plev * 2) / 5);
			sides = 19;

			/*Has greater than 20/1 final damage ratio, 25/1 but
			* balanced by the fact that EVIL monsters resist. Take
		    * that Hydras! ;) Might tone down damage and mana costs
			* both. -AR*/

			if (name) return ("Rend Soul");
			if (desc) return (format("Fires a bolt or beam of nether for %dd%d hp damage.", dice, sides));
			if (desc_short) return (format("dam %dd%d", dice, sides));
			if (cast)
			{
				fire_bolt_or_beam(beam, GF_NETHER, dir, damroll(dice, sides));
			}

			break;
		}

		case SPELL_MASS_BANISHMENT:
		{
			if (name) return ("Mass Banishment");
			if (desc) return ("Banishes nearby monsters.  Uniques and quest monsters are unaffected.");
			if (desc_short) return ("");
			if (cast)
			{
				(void)mass_banishment();
			}

			break;
		}

		case SPELL_RESIST_FIRE:
		{
			dur = 20;

			if (name) return ("Resist Fire");
			if (desc) return (format("Temporary opposition to fire for %d+1d%d turns.  Cumulative with equipment resistances.", dur, dur));
			if (desc_short) return (format("%d+1d%d turns", dur, dur));
			if (cast)
			{
				(void)inc_timed(TMD_OPP_FIRE, randint(dur) + dur, TRUE);
			}

			break;
		}

		case SPELL_RESIST_COLD:
		{
			dur = 20;

			if (name) return ("Resist Cold");
			if (desc) return (format("Temporary opposition to cold for %d+1d%d turns.  Cumulative with equipment resistances.", dur, dur));
			if (desc_short) return (format("%d+1d%d turns", dur, dur));
			if (cast)
			{
				(void)inc_timed(TMD_OPP_COLD, randint(dur) + dur, TRUE);
			}

			break;
		}

		case SPELL_ELEMENTAL_BRAND: /* elemental brand */
		{
			if (name) return ("Elemental Brand");
			if (desc) return ("Attempts to brand one set of ammunition.");
			if (desc_short) return ("");
			if (cast)
			{
				(void)brand_ammo(TRUE);
			}

			break;
		}

		case SPELL_RESIST_POISON:
		{
			dur = 20;

			if (name) return ("Resist Poison");
			if (desc) return (format("Temporary resist to poison for %d+1d%d turns.  Cumulative with equipment resistances.", dur, dur));
			if (desc_short) return (format("%d+1d%d turns", dur, dur));
			if (cast)
			{
				(void)inc_timed(TMD_OPP_POIS, randint(dur) + dur, TRUE);
			}

			break;
		}

		case SPELL_RESISTANCE:
		{
			dur = 20;

			if (name) return ("Resistance");
			if (desc) return (format("Temporary resist to the 5 elements for %d+1d%d turns.  Cumulative with equipment resistances.", dur, dur));
			if (desc_short) return (format("%d+1d%d turns", dur, dur));
			if (cast)
			{
				int time_2 = randint(dur) + dur;
				(void)inc_timed(TMD_OPP_ACID, time_2, TRUE);
				(void)inc_timed(TMD_OPP_ELEC, time_2, TRUE);
				(void)inc_timed(TMD_OPP_FIRE, time_2, TRUE);
				(void)inc_timed(TMD_OPP_COLD, time_2, TRUE);
				(void)inc_timed(TMD_OPP_POIS, time_2, TRUE);
			}

			break;
		}

		case SPELL_FLIGHT:
		{
			dur = 25 + plev / 2;
			dur1 = 10;

			if (name) return ("Flight");
			if (desc) return (format("Causes the player to fly for %d turns, or %d more turns if already flying.",
											dur, dur1));
			if (desc_short) return (format("%d turns", dur));
			if (cast)
			{
				if (!p_ptr->timed[TMD_FLYING])
				{
					(void)inc_timed(TMD_FLYING, dur, TRUE);
				}
				else
				{
					(void)inc_timed(TMD_FLYING, dur1, TRUE);
				}
			}

			break;
		}

		case SPELL_SHIELD:
		{
			dur1 = 20;
			dur2 = 30;

			if (name) return ("Shield");
			if (desc) return (format("Temporarily increases armour class by 50 for %d+1d%d turns.", dur2, dur1));
			if (desc_short) return (format("%d+1d%d turns", dur2, dur1));
			if (cast)
			{
				(void)inc_timed(TMD_SHIELD, randint(dur1) + dur2, TRUE);
			}

			break;
		}

		case SPELL_BERSERKER:
		{
			dur = 25;
			dam = 30;

			if (name) return ("Berserker");
			if (desc) return (format("Heals player a little.  Removes fear.  Makes player berzerk for %d+1d%d turns.", dur, dur));
			if (desc_short) return (format("%d+1d%d turns", dur, dur));
			if (cast)
			{
				(void)hp_player(dam);
				(void)inc_timed(TMD_SHERO, randint(dur) + dur, TRUE);
				(void)clear_timed(TMD_AFRAID, TRUE);
			}

			break;
		}

		case SPELL_HASTE_SELF:
		{
			dur1 = 5;
			dur = 20;

			if (name) return ("Haste Self");
			if (desc) return (format("Temporarily hasten yourself for %d+1d%d turns, or %d more turns if already hasted.",
										plev, dur, dur1));
			if (desc_short) return (format("%d+1d%d turns", plev, dur));
			if (cast)
			{
				if (!p_ptr->timed[TMD_FAST])
				{
					(void)set_timed(TMD_FAST, randint(dur) + plev, TRUE);
				}
				else
				{
					(void)inc_timed(TMD_FAST, dur1, FALSE);
				}
			}

			break;
		}

		case SPELL_RIFT:
		{
			dam1 = 40;
			dice = plev;
			sides = 7;

			if (name) return ("Rift");
			if (desc) return (format("Fires a beam of gravity for %d+%dd%d hp damage.", dam1, dice, sides));
			if (desc_short) return (format("dam %d+%dd%d", dam1, dice, sides));
			if (cast)
			{
				fire_beam(GF_GRAVITY, dir,	dam1 + damroll(dice, sides), 0L);
			}

			break;
		}

		case SPELL_DARKNESS_STORM:
		{
			/*
			* A Storm type spell while waiting for Mana Storm, picked GF_DARK
			* because I wanted to mimic monster spells. 12 for 1 final ratio.
			* -AR
			*/

			dam = 280 + (plev * 4);
			rad = 3;

			if (name) return ("Darkness Storm");
			if (desc) return (format("Fires a radius %d explosion of powerful darkness for %d hp damage.", rad, dam));
			if (desc_short) return (format("rad %d dam %d", rad, dam));
			if (cast)
			{
				fire_ball(GF_DARK, dir, dam, rad);
			}

			break;
		}

		case SPELL_MANA_BOLT:
		{
			dice = 20 + (plev * 2);
			sides = 9;

			if (name) return ("Mana Bolt");
			if (desc) return (format("Fires a bolt or beam of pure mana for %dd%d hp damage.", dice, sides));
			if (desc_short) return (format("dam %dd%d", dice, sides));
			if (cast)
			{
				fire_bolt_or_beam(beam, GF_MANA, dir, damroll(dice, sides));
			}

			break;
		}

		case SPELL_RUNE_OF_PROTECTION:
		{
			if (name) return ("Rune of Protection");
			if (desc) return ("Creates a rune of protection beneath you.");
			if (desc_short) return ("");
			if (cast)
			{
				(void)warding_glyph();
			}

			break;
		}

		case SPELL_ENCHANT_ARMOR: /* enchant armor */
		{
			if (name) return ("Enchant Armor");
			if (desc) return ("Attempts to enchant one piece of armor.");
			if (desc_short) return ("");
			if (cast)
			{
				(void)enchant_spell(0, 0, rand_int(3) + plev / 20);
			}

			break;
		}

		case SPELL_ENCHANT_WEAPON: /* enchant weapon */
		{
			if (name) return ("Enchant Weapon");
			if (desc) return ("Attempts to enchant one weapon.");
			if (desc_short) return ("");
			if (cast)
			{
				(void)enchant_spell(rand_int(4) + plev / 20,
			                     rand_int(4) + plev / 20, 0);
			}

			break;
		}

		case SPELL_MASS_IDENTIFY:
		{
			rad = 3;

			if (name) return ("Mass Identify");
			if (desc) return ("Identifies all nearby objects, including player equipment and inventory.");
			if (desc_short) return (format("rad %d", rad));
			if (cast)
			{
				mass_identify(rad);
			}

			break;
		}

 		case SPELL_WAIL_OF_THE_BANSHEE:
		{
			if (name) return ("Wail of the Banshee");
			if (desc) return ("Creates a wide cone of fear.");
			if (desc_short) return ("");
			if (cast)
			{
				fire_arc(GF_TURN_ALL, dir, plev, 0, 60);
			}

			break;
		}

 		case SPELL_LIGHTNING_BOLT:
 		{
 			dice =  4;
 			sides = 8;

 			if (name) return ("Lightning Bolt");
 			if (desc) return (format("Fires a bolt or beam of lightning for %dd%d hp damage.", dice, sides));
 			if (desc_short) return (format("dam %dd%d", dice, sides));
 			if (cast)
 			{
 				fire_bolt_or_beam(beam, GF_ELEC, dir, damroll(dice, sides));
 			}

 			break;
 		}

 		case SPELL_REMOVE_CURSE:
 		{
 			if (name) return ("Remove Curse");
 			if (desc) return ("Removes standard curses.");
 			if (desc_short) return ("");
 			if (cast)
 			{
 				remove_curse(FALSE);
 			}

			break;
		}
 		case SPELL_SLEEP_II:
 		{

 			if (name) return ("Sleep II");
 			if (desc) return ("Attempts to sleep all adjacent monsters.");
 			if (desc_short) return ("");
 			if (cast)
 			{
 				(void)sleep_monsters_touch();
 			}

 			break;
 		}

 		case SPELL_FROST_BOLT:
 		 {
 		 	dice =  6;
 		 	sides = 8;

 		 	if (name) return ("Frost Bolt");
 		 	if (desc) return (format("Fires a bolt or beam of cold for %dd%d hp damage.", dice, sides));
 		 	if (desc_short) return (format("dam %dd%d", dice, sides));
 		 	if (cast)
 		 	{
 		 		fire_bolt_or_beam(beam, GF_COLD, dir, damroll(dice, sides));
 		 	}

 		 	break;
 		 }

 		case SPELL_CREATE_FOOD:
 		{
 			if (name) return ("Create Food");
 			if (desc) return ("Magically creates one ration of food.");
 			if (desc_short) return ("");
 			if (cast)
 			{
 				create_food();
 			}
			break;
		}

 		case SPELL_FIRE_BOLT:
 		{
 		 	dice =  9;
 		 	sides = 8;

 		 	if (name) return ("Fire Bolt");
 		 	if (desc) return (format("Fires a bolt or beam of fire for %dd%d hp damage.", dice, sides));
 		 	if (desc_short) return (format("dam %dd%d", dice, sides));
 		 	if (cast)
 		 	{
 		 		fire_bolt_or_beam(beam, GF_FIRE, dir, damroll(dice, sides));
 		 	}

 		 	break;
 		}
 		case SPELL_FROST_BALL:
 		{
 			dam = 48;
 			rad = 2;

 			if (name) return ("Frost Ball");
 			if (desc) return (format("Fire a radius %d ball of frost for %d hp damage.", rad, dam));
 			if (desc_short) return (format("rad %d dam %d", rad, dam));
 			if (cast)
 			{
 				fire_ball(GF_COLD, dir, dam, rad);
 			}

 			break;
 		}
 		case SPELL_FIRE_BALL:
 		{
 			dam = 72;
 			rad = 2;

 			if (name) return ("Fire Ball");
 			if (desc) return (format("Fire a radius %d ball of fire for %d hp damage.", rad, dam));
 			if (desc_short) return (format("rad %d dam %d", rad, dam));
 			if (cast)
 			{
 				fire_ball(GF_FIRE, dir, dam, rad);
 			}

 			break;
 		}

	}

	/* Success */
	return ("Success!");
}



/*
 * Cast a spell, or output spell info, description.
 */
cptr do_druid_incantation(int mode, int spell, int dir)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int plev = p_ptr->lev;

	/* Spell-specific variables */
	int dam, dam1;
	int dur, dur1;
	int dice, sides;
	int rad;

	/* Function modes */
	bool cast = (mode == MODE_SPELL_CAST);
	bool name = (mode == MODE_SPELL_NAME);
	bool desc = (mode == MODE_SPELL_DESC);
	bool desc_short = (mode == MODE_SPELL_DESC_SHORT);

	/* Return the spell type name if that is what is being asked*/
	if (mode == MODE_SPELL_NOUN) return "incantation";
	if (mode == MODE_SPELL_VERB) return "recite";

	/* Spells. */
	switch (spell)
	{
		case DRUID_ACID_BOLT:
		{
			dice = 3 + ((plev - 1) / 5);
			sides = 5;

			if (name) return ("Acid bolt");
			if (desc) return (format("Fires a bolt of acid for %dd%d hp damage.", dice, sides));
			if (desc_short) return (format("dam %dd%d", dice, sides));
			if (cast)
			{
				fire_bolt(GF_ACID, dir, damroll((dice), (sides)));
			}

			break;
		}

		case DRUID_CURE_LIGHT_WOUNDS:
		{
			dice = 2;
			sides = 10;

			if (name) return ("Cure Light Wounds");
			if (desc) return (format("Reduces cuts and heals %dd%d hp", dice, sides));
			if (desc_short) return (format("heal %dd%d", dice, sides));
			if (cast)
			{
				(void)hp_player(damroll(dice, sides));
				set_cut(p_ptr->timed[TMD_CUT] - 15);
			}
			break;
		}

		case DRUID_DETECT_LIFE:
		{
			if (name) return ("Detect Life");
			if (desc) return ("Detects nearby living monsters.");
			if (desc_short) return (format("rad %d", DETECT_RADIUS));
			if (cast)
			{
				(void)detect(DETECT_RADIUS, DETECT_LIFE);
			}

			break;
		}

		case DRUID_CALL_LIGHT:
		{
			dice = 2;
			sides = plev / 2;
			rad = (plev / 10) + 1;

			if (name) return ("Call Light");
			if (desc) return (format("Permanently lights up a room or a radius %d area.", rad));
			if (desc_short) return (format("rad %d", rad));
			if (cast)
			{
				(void)light_area(damroll(dice, sides), rad);
			}

			break;
		}

		case DRUID_FIND_TRAPS_DOORS:
		{
			if (name) return ("Find Hidden Traps/Doors");
			if (desc) return ("Detects nearby traps, doors and stairs.");
			if (desc_short) return (format("rad %d", DETECT_RADIUS));
			if (cast)
			{
				(void)detect(DETECT_RADIUS, DETECT_DOORS_STAIRS_TRAPS);
			}

			break;
		}

		case DRUID_SLOW_POISON:
		{
			if (name) return ("Slow Poison");
			if (desc) return ("Reduces the level of the player's poison.");
			if (desc_short) return ("");
			if (cast)
			{
				(void)dec_timed(TMD_POISONED, p_ptr->timed[TMD_POISONED] / 2 + 1, TRUE);
			}

			break;
		}

		case DRUID_POISON_CLOUD:
		{
			rad = 4;

			/*
			 * Note the damage of the final poison cloud is 15% of the
			 * damage listed below, according to terrain.txt
			 */
			dam = 30 + (plev * 2);
			dam1 = (dam * f_info[FEAT_POISON_CLOUD].x_damage) / 100;

			if (name) return ("Poison Cloud");
			if (desc) return (format("Creates a radius %d cloud of poison that causes %d hp damage.", rad, dam1));
			if (desc_short) return (format("rad %d dam %d", rad, dam1));
			if (cast)
			{

				fire_effect_orb(GF_POIS, dir, dam, rad);
			}

			break;
		}

		case DRUID_NATURAL_ESCAPE:
		{
			/* Get the distance */
			/*int dam = MAX(plev, 20);*/

			int dam = plev * 8;

			if (name) return ("Natural Escape");
			if (desc) return ("Random displacement from/to native terrain.");
			if (desc_short) return (format("range %d", dam));
			if (cast)
			{
				/* Teleport */
				if (!teleport_player(dam, TRUE)) return (NULL);
			}

			break;

		}

		case DRUID_BARKSKIN:
		{

			dur = 25 + p_ptr->lev;

			if (name) return ("Bark Skin");
			if (desc) return (format("Temporarily increases armour class by 50 for %d turns.", dur));
			if (desc_short) return (format("dur %d turns.", dur));
			if (cast)
			{
				(void)inc_timed(TMD_SHIELD, dur, TRUE);
			}

			break;
		}

		case DRUID_NOURISHMENT:
		{
			if (name) return ("Nourishment");
			if (desc) return ("Magically renders the player well-fed.");
			if (desc_short) return ("");
			if (cast)
			{
				(void)set_food(PY_FOOD_MAX - 50);
			}

			break;
		}

		case DRUID_TURN_STONE_TO_MUD:
		{
			dam = 20 + randint(30);

			if (name) return ("Turn Stone to Mud");
			if (desc) return ("Removes one section of a normal wall to floor.");
			if (desc_short) return ("");
			if (cast)
			{
				(void)wall_to_mud(dir, dam);
			}

			break;
		}

		case DRUID_FROST_BEAM:
		{
			dice = 6 + ((plev - 3) / 4);
			sides = 8;

			if (name) return ("Frost Beam");
			if (desc) return (format("Fires a beam of frost for %dd%d hp damage.", dice, sides));
			if (desc_short) return (format("dam %dd%d", dice, sides));
			if (cast)
			{
				fire_beam(GF_COLD, dir, damroll(dice, sides), 0L);
			}

			break;
		}

		case DRUID_CURE_POISON:
		{
			if (name) return ("Cure Poison");
			if (desc) return ("Cures the player of poison.");
			if (desc_short) return ("");
			if (cast)
			{
				(void)clear_timed(TMD_POISONED, TRUE);
			}

			break;
		}

		case DRUID_TRAP_DOOR_DESTRUCTION:
		{
			if (name) return ("Trap/Door Destruction");
			if (desc) return ("Fires a beam that disarms traps and chests and destroys doors.");
			if (desc_short) return ("");
			if (cast)
			{
				destroy_door(dir);
			}

			break;
		}

		case DRUID_RESIST_HEAT_COLD:
		{
			dur = 20;

			if (name) return ("Resist Heat and Cold");
			if (desc) return (format("Temporary opposition to fire and frost for %d+1d%d turns.  Cumulative with equipment resistances.", dur, dur));
			if (desc_short) return (format("dur %d+1d%d turns", dur, dur));
			if (cast)
			{
				dur1 = randint(dur);

				(void)inc_timed(TMD_OPP_FIRE, dur1 + dur, TRUE);
				(void)inc_timed(TMD_OPP_COLD, dur1 + dur, TRUE);
			}

			break;
		}

		case DRUID_SPEAR_OF_LIGHT:
		{
			dice = 6;
			sides = 8;

			if (name) return ("Spear of Light");
			if (desc) return (format("Fires a line of weak light.  %dd%d hp damage for light-hating creatures.", dice, sides));
			if (desc_short) return (format("dam %dd%d", dice, sides));
			if (cast)
			{
				msg_print("A line of blue shimmering light appears.");
				(void)light_line(dir, damroll(dice, sides));
			}

			break;
		}

		case DRUID_FIRE_BEAM:
		{
			dice = 7 + (plev-3) / 4;
			sides = 7;

			if (name) return ("Fire Beam");
			if (desc) return (format("Fires a beam of fire for %dd%d hp damage", dice, sides));
			if (desc_short) return (format("dam %dd%d", dice, sides));
			if (cast)
			{
				(void)fire_beam(GF_FIRE, dir, damroll(dice, sides), 0L);
			}

			break;
		}

		case DRUID_STERILIZE:
		{

			dam = 1;
			rad = 8;

			if (name) return ("Sterilize");
			if (desc) return (format("Fires a radius %d ball that prevents monsters from multiplying.", rad));
			if (desc_short) return (format("rad %d", rad));
			if (cast)
			{
				fire_ball(GF_STERILIZE, dir, dam, rad);
			}

			break;
		}

		case DRUID_EXTINGUISH:
		{
			rad = 2;

			if (name) return ("Extinguish");
			if (desc) return (format("Attempts to extinguish any fires in a %d radius.", rad));
			if (desc_short) return (format("rad %d", rad));
			if (cast)
			{
				u32b flg = PROJECT_BOOM | PROJECT_WALL | PROJECT_GRID | PROJECT_EFCT;

				/* Try to put out fires*/
				project(SOURCE_PLAYER, rad, py, px, py, px, (plev * 75), GF_EXTINGUISH, flg, 0,0);
			}

			break;
		}

		case DRUID_CLEAR_AREA:
		{
			int rad1 = 3;
			rad = 4;

			dam = 50 + randint(30);

			if (name) return ("Clear Area");
			if (desc) return ("Clears an area of all normal walls.");
			if (desc_short) return (format("rad %d+1d%d", rad, rad1));
			if (cast)
			{
				rad += randint1(rad1);
				fire_star(GF_KILL_WALL, dam, rad, PROJECT_PASS | PROJECT_ROOM);
			}

			break;
		}

		case DRUID_CURE_CRITICAL_WOUNDS:
		{
			dice = 8;
			sides = 10;

			if (name) return ("Cure Critical Wounds");
			if (desc) return (format("Eliminates cuts and heals %dd%d hp.", dice, sides));
			if (desc_short) return (format("heal %dd%d", dice, sides));
			if (cast)
			{
				(void)hp_player(damroll(dice, sides));
				(void)set_cut(0);
			}

			break;
		}

		case DRUID_IDENTIFY:
		{
			if (name) return ("Identify");
			if (desc) return ("Identifies an object.");
			if (desc_short) return ("");
			if (cast)
			{
				if (!ident_spell()) return (NULL);
			}

			break;

		}

		case DRUID_CLEAR_AIR:
		{
			rad = 3;

			if (name) return ("Clear Air");
			if (desc) return (format("Clears the air of visible effects within a %d radius.", rad));
			if (desc_short) return (format("rad %d", rad));
			if (cast)
			{
				u32b flg = PROJECT_BOOM | PROJECT_WALL | PROJECT_EFCT;

				/* Clear the air of effects*/
				project(SOURCE_PLAYER, rad, py, px, py, px, 1, GF_CLEAR_AIR, flg, 0,0);
			}

			break;
		}

		case DRUID_DETECT_TERRAIN:
		{
			if (name)
			{
			       	return ("Detect Terrain");
			}
			if (desc)
			{
				return ("Maps all natural features on the entire dungeon level."
					"  At level 30 also reveals what natural beings can see.");
			}
			if (desc_short) return ("");
			if (desc_short) return (format("rad %d", DETECT_RADIUS));
			if (cast)
			{
				(void)detect(DETECT_RADIUS, DETECT_TERRAIN);
				if (plev >= 30) read_minds();
			}
			break;

		}

		case DRUID_EARTHQUAKE:
		{
			rad = 10;

			if (name) return ("Earthquake");
			if (desc) return (format("Creates a radius %d earthquake centered on the player.", rad));
			if (desc_short) return (format("rad %d", rad));
			if (cast)
			{
				earthquake(py, px, rad, FALSE);
			}

			break;
		}

		case DRUID_LIFE_DRAIN_BURST:
		{
			rad = 5;

			/*
			 * Note the damage of the final static is 30% of the
			 * damage listed below, according to terrain.txt
			 */
			dam = 400 + (plev * 6);  /*120 + plev times 2 damage*/
			dam1 = (dam * f_info[FEAT_LIFE_DRAIN].x_damage) / 100;

			if (name) return ("Life draining bursts");
			if (desc) return (format("Creates a radius %d orb of time released life draining bursts that causes %d hp damage.", rad, dam1));
			if (desc_short) return (format("rad %d dam %d", rad, dam1));
			if (cast)
			{
				fire_effect_orb(GF_LIFE_DRAIN, dir, dam, rad);
			}

			break;
		}

		case DRUID_ELEMENTAL_BRAND:
		{
			cptr extra = "";
			dur = 10 + p_ptr->lev / 2;

			if (p_ptr->timed[TMD_SLAY_ELEM])
			{
				dur = 5;
				extra = " extra";
			}

			if (name) return ("Elemental Weapon");
			if (desc) return (format("Temporarily makes your weapon glow with elemental brands for %d%s turns.", dur, extra));
			if (desc_short) return (format("dur %d%s turns.", dur, extra));
			if (cast)
			{
				(void)inc_timed(TMD_SLAY_ELEM, dur, TRUE);
			}

			break;
		}


		case DRUID_FROST_BALL:
		{
			dam = 25 + 7 * plev / 2;
			rad = 2;

			if (name) return ("Frost Ball");
			if (desc) return (format("Fires a radius %d ball of frost for %d hp damage.", rad, dam));
			if (desc_short) return (format("rad %d dam %d", rad, dam));
			if (cast)
			{
				fire_ball(GF_COLD, dir, dam, rad);
			}

			break;
		}


		case DRUID_HEAL:
		{
			/*Not as powerful for Rangers*/
			if (cp_ptr->flags & (CF_ZERO_FAIL)) dam = 300;
			else dam = 175;

			if (name) return ("Heal");
			if (desc) return (format("Eliminates stunning and cuts and heals %d hp.", dam));
			if (desc_short) return (format("heal %d hp.", dam));
			if (cast)
			{
				(void)hp_player(dam);
				(void)set_cut(0);
				(void)set_stun(0);
			}

			break;
		}

		case DRUID_DISPEL_LIFE:
		{
			dice = 1;
			sides = plev * 3;

			if (name) return ("Dispel Life");
			if (desc) return (format("Does %dd%d damage to all living creatures in line of sight.", dice, sides));
			if (desc_short) return (format("dam %dd%d", dice, sides));
			if (cast)
			{
				/* Calculate damage */
				if (dice > 1) dam = damroll(dice, sides);

				else dam = randint(sides);

				project_los(p_ptr->py, p_ptr->px, dam, GF_LIFE_DRAIN);
			}

			break;
		}

		case DRUID_FIRE_BALL:
		{
			dam = 50 + plev * 3;
			dam1 = plev * 2;
			rad = 2;

			if (name) return ("Fire Ball");
			if (desc) return (format("Fires a radius %d ball of fire for %d+1d%d hp damage.", rad, dam, dam1));
			if (desc_short) return (format("rad %d dam %d", rad, dam1));
			if (cast)
			{
				dam += randint(dam1);
				fire_ball(GF_FIRE, dir, dam, rad);
			}

			break;
		}

		case DRUID_DRAIN_LIFE_ARC:
		{
			dam = 125;
			dice = 5;
			sides = plev;

			if (name) return ("Life Draining Arc");
			if (desc) return (format("Fires an arc of life draining for %d + %dd%d hp damage.", dam, dice, sides));
			if (desc_short) return (format("dam %d + %dd%d", dam, dice, sides));
			if (cast)
			{
				/*figure the damage*/
				dam += damroll(dice, sides);

				fire_arc(GF_LIFE_DRAIN, dir, dam, 0, 30);
			}

			break;
		}

		case DRUID_MASS_IDENTIFY:
		{
			rad = 3;

			if (name) return ("Mass Identify");
			if (desc) return ("Identifies all nearby objects, including player equipment and inventory.");
			if (desc_short) return (format("rad %d", rad));
			if (cast)
			{
				mass_identify(rad);
			}

			break;
		}


		case DRUID_RESIST_ELEC:
		{
			dur = 40;

			if (name) return ("Resist Electricity");
			if (desc) return (format("Temporary opposition to electricity for %d+1d%d turns.  Cumulative with equipment resistances.", dur, dur));
			if (desc_short) return (format("dur%d+1d%d turns.", dur, dur));
			if (cast)
			{
				(void)inc_timed(TMD_OPP_ELEC, randint(dur) + dur, TRUE);
			}

			break;
		}

		case DRUID_RESIST_ACID:
		{
			dur = 40;

			if (name) return ("Resist Acid");
			if (desc) return (format("Temporary opposition to acid for %d+1d%d turns.  Cumulative with equipment resistances.", dur, dur));
			if (desc_short) return (format("dur %d+1d%d turns.", dur, dur));
			if (cast)
			{
				(void)inc_timed(TMD_OPP_ACID, randint(dur) + dur, TRUE);
			}

			break;
		}

		case DRUID_RESIST_POISON:
		{
			dur = 40;

			if (name) return ("Resist Poison");
			if (desc) return (format("Temporary resist to poison for %d+1d%d turns.  Cumulative with equipment resistances.", dur, dur));
			if (desc_short) return (format("dur %d+1d%d turns.", dur, dur));
			if (cast)
			{
				(void)inc_timed(TMD_OPP_POIS, randint(dur) + dur, TRUE);
			}

			break;
		}

		case DRUID_RESISTANCE:
		{
			dur = 30;

			if (name) return ("Resistance");
			if (desc) return (format("Temporary resist to the 5 elements for %d+1d%d turns.  Cumulative with equipment resistances.", dur, dur));
			if (desc_short) return (format("dur %d+1d%d turns.", dur, dur));
			if (cast)
			{
				int time_2 = randint(dur) + dur;
				(void)inc_timed(TMD_OPP_ACID, time_2, TRUE);
				(void)inc_timed(TMD_OPP_ELEC, time_2, TRUE);
				(void)inc_timed(TMD_OPP_FIRE, time_2, TRUE);
				(void)inc_timed(TMD_OPP_COLD, time_2, TRUE);
				(void)inc_timed(TMD_OPP_POIS, time_2, TRUE);
			}

			break;
		}

		case DRUID_HASTE_SELF:
		{
			dur1 = 5;
			dur = 20;

			if (name) return ("Haste Self");
			if (desc) return (format("Temporarily hasten yourself for %d+1d%d turns, or %d more turns if already hasted.",
											plev, dur, dur1));
			if (desc_short) return (format("dur %d turns.", dur));
			if (cast)
			{
				if (!p_ptr->timed[TMD_FAST])
				{
					(void)set_timed(TMD_FAST, randint(dur) + plev, TRUE);
				}
				else
				{
					(void)inc_timed(TMD_FAST, dur1, FALSE);
				}
			}

			break;
		}

		case DRUID_GLACIER:
		{
			/* Get the feature */
			feature_type *f_ptr = &f_info[FEAT_GLACIER];
			dam = f_ptr->x_timeout_set;
			dam1 = f_ptr->x_timeout_rand;

			if (name) return ("Glacier");
			if (desc) return (format("Creates a transparent and impenetrable ice barrier that lasts %d+d%d turns.", dam, dam1));
			if (desc_short) return (format("dur %d+d%d turns.", dam, dam1));
			if (cast)
			{
				if (!create_glacier()) return (NULL);
			}

			break;
		}

		case DRUID_CREATE_ELEMENTS:
		{
			rad = 2;

			if (name) return ("Create elements");
			if (desc) return (format("Creates native terrains in a radius %d area around the player.", rad));
			if (desc_short) return (format("rad %d", rad));
			if (cast)
			{
				if (!create_elements(p_ptr->py, p_ptr->px, rad)) return (NULL);
			}
			break;
		}

		case DRUID_FLICKER:
		{
			dam = 10;

			if (name) return ("Flicker");
			if (desc) return (format("A range %d random minor displacement.", dam));
			if (desc_short) return (format("range %d", dam));
			if (cast)
			{
				teleport_player(dam, FALSE);
			}

			break;
		}


		case DRUID_WORD_OF_RECALL:
		{
			if (name) return ("Word of Recall");
			if (desc) return ("Recalls you to the town, or to the recall level in the dungeon.");
			if (desc_short) return ("");
			if (cast)
			{
				if (!set_recall()) return (NULL);
			}

			break;
		}

		case DRUID_HEALING:
		{
			dam = 750;

			if (name) return ("Healing");
			if (desc) return (format("Eliminates stunning and cuts and heals %d hp.", dam));
			if (desc_short) return (format("heals %d", dam));
			if (cast)
			{
				(void)hp_player(dam);
				(void)set_cut(0);
				(void)set_stun(0);
			}

			break;
		}

		case DRUID_RESTORATION:
		{

			if (name) return ("Restoration");
			if (desc) return ("Restores all stats to their maximum.");
			if (desc_short) return ("");
			if (cast)
			{
				(void)do_res_stat(A_STR);
				(void)do_res_stat(A_INT);
				(void)do_res_stat(A_WIS);
				(void)do_res_stat(A_DEX);
				(void)do_res_stat(A_CON);
				(void)do_res_stat(A_CHR);
			}

			break;
		}

		case DRUID_REMEMBRANCE:
		{

			if (name) return ("Remembrance");
			if (desc) return ("Restores experience to maximum.");
			if (desc_short) return ("");
			if (cast)
			{
				(void)restore_level();
			}

			break;
		}

		case DRUID_SANDSTORM:
		{
			dam = plev * 3;
			dice = 6;
			sides = plev;

			if (name) return ("Sand storm");
			if (desc) return (format("Fires an arc of sand for %d + %dd%d hp damage",
				dam, dice, sides));
			if (desc_short) return (format("dam %d + %dd%d", dam, dice, sides));
			if (cast)
			{
				dam += damroll(dice, sides);
				fire_arc(GF_SAND, dir, dam, 0, 16);
			}

			break;
		}

		case DRUID_NATIVE_SAND:
		{

			dur = 50 + p_ptr->lev;
			dur1 = 10;

			if (name) return ("Sand Nativity");
			if (desc) return (format("Temporary nativity to sand for %d turns, or %d more turns if already temporarily native to sand.",
											dur, dur1));
			if (desc_short) return (format("dur %d turns.", dur));
			if (cast)
			{
				if (p_ptr->timed[TMD_NAT_SAND]) dur = dur1;

				(void)inc_timed(TMD_NAT_SAND, dur, TRUE);
			}

			break;
		}

		case DRUID_NATIVE_MUD:
		{
			dur = 50 + p_ptr->lev;
			dur1 = 10;

			if (name) return ("Mud Nativity");
			if (desc) return (format("Temporary nativity to mud for %d turns, or %d more turns if already temporarily native to mud.",
											dur, dur1));
			if (desc_short) return (format("dur %d turns.", dur));
			if (cast)
			{
				if (p_ptr->timed[TMD_NAT_MUD]) dur = dur1;

				(void)inc_timed(TMD_NAT_MUD, dur, TRUE);
			}

			break;
		}

		case DRUID_NATIVE_WATER:
		{
			dur = 50 + p_ptr->lev;
			dur1 = 10;

			if (name) return ("Water Nativity");
			if (desc) return (format("Temporary nativity to water for %d turns, or %d more turns if already temporarily native to water.",
											dur, dur1));
			if (desc_short) return (format("dur %d turns.", dur));
			if (cast)
			{
				if (p_ptr->timed[TMD_NAT_WATER]) dur = dur1;

				(void)inc_timed(TMD_NAT_WATER, dur, TRUE);
			}

			break;
		}

		case DRUID_NATIVE_OIL:
		{
			dur = 50 + p_ptr->lev;
			dur1 = 10;

			if (name) return ("Oil Nativity");
			if (desc) return (format("Temporary nativity to oil for %d turns, or %d more turns if already temporarily native to oil.",
											dur, dur1));
			if (desc_short) return (format("dur %d turns.", dur));
			if (cast)
			{
				if (p_ptr->timed[TMD_NAT_OIL]) dur = dur1;

				(void)inc_timed(TMD_NAT_OIL, dur, TRUE);
			}

			break;
		}

		case DRUID_NATIVE_LAVA:
		{
			dur = 50 + p_ptr->lev;
			dur1 = 10;

			if (name) return ("Lava Nativity");
			if (desc) return (format("Temporary nativity to lava for %d turns, or %d more turns if already temporarily native to lava.",
											dur, dur1));
			if (desc_short) return (format("dur %d turns.", dur));
			if (cast)
			{
				if (p_ptr->timed[TMD_NAT_LAVA]) dur = dur1;

				(void)inc_timed(TMD_NAT_LAVA, dur, TRUE);
			}

			break;
		}

		case DRUID_CHANNEL_LIGHTNING:
		{
			dice = 4 + plev / 3;
			sides = 10 + plev / 2;

			if (name) return ("Channel Lightning");
			if (desc) return (format("Fires a powerful bolt of lightning for %dd%d hp damage.",
				dice, sides));
			if (desc_short) return (format("dam %dd%d", dice, sides));

			if (cast)
			{
				(void)fire_bolt_beam_special(GF_ELEC, dir, damroll(dice, sides),
					MAX_RANGE, PROJECT_NO_EFCT);
			}

			break;
		}

		case DRUID_DISPEL_CURSE:
		{
			if (name) return ("Dispel Curse");
			if (desc) return ("Removes standard and heavy curses.");
			if (desc_short) return ("");
			if (cast)
			{
				remove_curse(FALSE);
			}

			break;
		}

		case DRUID_RECHARGE_ITEM:
		{
			if (name) return ("Greater Recharging");
			if (desc) return ("Attempts to recharge a wand, staff, or rod.");
			if (desc_short) return ("");
			if (cast)
			{
				(void)recharge(50, FALSE, 25 + (plev / 2));
			}

			break;
		}

		case DRUID_BRAND_AMMUNITION:
		{
			if (name) return ("Elemental Ammunition Brand");
			if (desc) return ("Attempts to brand one set of ammunition.");
			if (desc_short) return ("");
			if (cast)
			{
				(void)brand_ammo(TRUE);
			}

			break;
		}

		case DRUID_ENCHANT_ARMOUR:
		{
			if (name) return ("Enchant Armour");
			if (desc) return ("Attempts to enchant one piece of armor.");
			if (desc_short) return ("");
			if (cast)
			{
				(void)enchant_spell(0, 0, rand_int(3) + 2);
			}

			break;

		}

		case DRUID_BRAND_WEAPON:
		{
			if (name) return ("Elemental Weapon Brand");
			if (desc) return ("Attempts to brand one weapon.");
			if (desc_short) return ("");
			if (cast)
			{
				(void)brand_weapon(TRUE);
			}

			break;
		}

		case DRUID_WATER_CHAIN:
		{
			dam = 100;
			dice = 3 + plev / 10;
			sides = plev;

			if (name) return ("Ulmo's Wrath");
			if (desc) return (format("Fires chained beams of water for %d + %dd%d hp damage.",
				dam, dice, sides));
			if (desc_short) return (format("dam %d + %dd%d", dam, dice, sides));
			if (cast)
			{
				int max_hits = 7;
				int decrement = 10;

				if (one_in_(5))
				{
					max_hits = 7;
					decrement = 0;
				}

				dam += damroll(dice, sides);

				if (!beam_chain(GF_WATER, dam, max_hits, decrement)) return (NULL);
			}

			break;
		}

		case DRUID_CALL_HUORNS:
		{
			cptr extra = "";
			dam = plev;

			if (!p_ptr->timed[TMD_CALL_HOURNS])
			{
				extra = " extra";
				dam = plev / 10;
			}


			if (name) return ("Call Huorns");
			if (desc) return (format("Make nearby trees attack foes for %d%s player turns.", dam, extra));
			if (desc_short) return (format("%d%s player turns", dam, extra));
			if (cast)
			{
				set_timed(TMD_CALL_HOURNS, dam, TRUE);
			}

			break;
		}

		case DRUID_MASTER_ELEMENTS:
		{
			dam = p_ptr->lev * ((28 * p_ptr->lev) / 100);

			if (name) return ("Master Elements");
			if (desc) return (format("Cast a powerful ball of elements for %d hp damage.", dam));
			if (desc_short) return (format("dam %d", dam));
			if (cast)
			{
				if (!master_elements(dam, dir)) return (NULL);
			}

			break;
		}

		case DRUID_STEAL_POWERS:
		{
			if (name) return ("Steal Powers");
			if (desc) return (format("Cast attack spells like nearby animals and vortices."));
			if (desc_short) return ("");
			if (cast)
			{
				if (!steal_powers(dir)) return (NULL);
			}

			break;
		}

		default: break;
	}

	/* success  */
	return ("success");
}


cptr do_priest_prayer(int mode, int spell, int dir)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int plev = p_ptr->lev;

	/* Spell-specific variables */
	int dam, dam1;
	int dur, dur1;
	int dice, sides;
	int rad;

	cptr extra = "";

	/* Function modes */
	bool cast = (mode == MODE_SPELL_CAST);
	bool name = (mode == MODE_SPELL_NAME);
	bool desc = (mode == MODE_SPELL_DESC);
	bool desc_short = (mode == MODE_SPELL_DESC_SHORT);

	/* Return the spell type name if that is what is being asked*/
	if (mode == MODE_SPELL_NOUN) return "prayer";
	if (mode == MODE_SPELL_VERB) return "pray";

	switch (spell)
	{
		case PRAYER_DETECT_EVIL:
		{
			if (name) return ("Detect Evil");
			if (desc) return ("Detects all nearby evil monsters, even invisible ones.");
			if (desc_short) return (format("rad %d", DETECT_RADIUS));
			if (cast)
			{
				(void)detect(DETECT_RADIUS, DETECT_EVIL);
			}

			break;
		}

		case PRAYER_CURE_LIGHT_WOUNDS:
		{
			dice = 3;
			sides = 8;

			if (game_mode == GAME_NPPMORIA)
			{
				dice = 3;
				sides = 3;
			}

			if (name) return ("Cure Light Wounds");
			if (desc) return (format("Reduces cuts and heals %dd%d hp.", dice, sides));
			if (desc_short) return (format("heal %dd%d", dice, sides));
			if (cast)
			{
				(void)hp_player(damroll(dice, sides));
				(void)set_cut(p_ptr->timed[TMD_CUT] - 10);
			}

			break;
		}

		case PRAYER_BLESS:
		{
			dur = 12;
			if (p_ptr->timed[TMD_BLESSED])
			{
				extra = " extra";
				dur = 5;
			}


			if (name) return ("Bless");
			if (desc) return (format("Bonus to fighting ability and armour class for %d+d%d%s turns.", dur, dur, extra));
			if (desc_short) return (format("%d+d%d%s turns", dur, dur, extra));
			if (cast)
			{
				(void)inc_timed(TMD_BLESSED, randint(dur) + dur, TRUE);
			}

			break;
		}

		case PRAYER_REMOVE_FEAR:
		{

			if (name) return ("Remove Fear");
			if (desc) return ("Removes fear.");
			if (desc_short) return ("");
			if (cast)
			{
				(void)clear_timed(TMD_AFRAID, TRUE);
			}

			break;
		}

		case PRAYER_CALL_LIGHT:
		{
			dice = 2;
			sides = plev / 2;
			rad = (plev / 10) + 1;

			if (name) return ("Call Light");
			if (desc) return (format("Permanently lights up a room or a radius %d area.", rad));
			if (desc_short) return (format("rad %d", rad));
			if (cast)
			{
				(void)light_area(damroll(dice, sides), rad);
			}

			break;
		}

		case PRAYER_FIND_TRAPS_DOORS_STAIRS:
		{
			if (name) return ("Find Doors/Stairs/Traps");
			if (desc) return ("Detects nearby traps, doors and stairs.");
			if (desc_short) return (format("rad %d", DETECT_RADIUS));
			if (cast)
			{
				(void)detect(DETECT_RADIUS, DETECT_DOORS_STAIRS_TRAPS);
			}

			break;
		}

		case PRAYER_SHOCK_BOLT:
		{
			dice = 6;
			sides = 4;
			dam = (plev / ((cp_ptr->flags & CF_BLESS_WEAPON) ? 2 : 3));

			if (name) return ("Shock Bolt");
			if (desc) return (format("Fires a bolt of solid light for %d+%dd%d damage.", dam, dice, sides));
			if (desc_short) return (format("dam %d + %dd%d", dam, dice, sides));
			if (cast)
			{
				fire_bolt(GF_LIGHT, dir, (damroll(dice, sides) + dam));
			}

			break;
		}

		case PRAYER_SLOW_POISON:
		{
			if (name) return ("Slow Poison");
			if (desc) return ("Reduces the level of the player's poison.");
			if (desc_short) return ("");
			if (cast)
			{
				(void)dec_timed(TMD_POISONED, p_ptr->timed[TMD_POISONED] / 2 + 1, TRUE);
			}

			break;
		}

		case PRAYER_SCARE_MONSTER:
		{

			if (name) return ("Scare Monster");
			if (desc) return ("Attempts to scare one monster.");
			if (desc_short) return ("");
			if (cast)
			{
				(void)fear_monster(dir, plev);
			}

			break;

		}

		case PRAYER_PORTAL:
		{
			dam = plev * 3;

			if (name) return ("Portal");
			if (desc) return (format("Random minor displacement up to %d squares.", dam));
			if (desc_short) return (format("range %d", dam));
			if (cast)
			{
				teleport_player(dam, FALSE);
			}

			break;
		}

		case PRAYER_CURE_SERIOUS_WOUNDS:
		{
			dice = 5;
			sides = 10;

			if (game_mode == GAME_NPPMORIA)
			{
				dice = 4;
				sides = 4;
			}

			if (name)
			{
				if (game_mode == GAME_NPPMORIA)	return ("Cure Medium Wounds");
				else return ("Cure Serious Wounds");
			}
			if (desc) return (format("Reduces cuts and heals %dd%d hp.", dice, sides));
			if (desc_short) return (format("heal %dd%d", dice, sides));
			if (cast)
			{
				(void)hp_player(damroll(dice, sides));
				(void)set_cut((p_ptr->timed[TMD_CUT] / 2) - 20);
			}


			break;
		}

		case PRAYER_CHANT:
		{
			dur = 24;

			if (p_ptr->timed[TMD_BLESSED])
			{
				extra = " extra";
				dur = 5;
			}

			if (name) return ("Chant");
			if (desc) return (format("Bonus to fighting ability and armour class for %d+d%d%s turns.", dur, dur, extra));
			if (desc_short) return (format("dur %d+d%d%s turns.", dur, dur, extra));
			if (cast)
			{
				(void)inc_timed(TMD_BLESSED, randint(dur) + dur, TRUE);
			}

			break;
		}

		case PRAYER_SANCTUARY:
		{

			if (name) return ("Sanctuary");
			if (desc) return ("Attempts to sleep all adjacent monsters.");
			if (desc_short) return ("");
			if (cast)
			{
				(void)sleep_monsters_touch();
			}

			break;
		}

		case PRAYER_SATISFY_HUNGER:
		{
			if (name) return ("Satisfy Hunger");
			if (desc) return ("Magically renders the player well-fed.");
			if (desc_short) return ("");
			if (cast)
			{
				(void)set_food(PY_FOOD_MAX - 50);
			}

			break;
		}

		case PRAYER_REMOVE_CURSE:
		{
			if (name) return ("Remove Curse");
			if (desc) return ("Removes standard curses.");
			if (desc_short) return ("");
			if (cast)
			{
				remove_curse(FALSE);
			}

			break;
		}

		case PRAYER_RESIST_HEAT_COLD:
		{
			dur = 10;

			if (name) return ("Resist Heat and Cold");
			if (desc) return (format("Temporary opposition to fire and frost for %d+d%d turns.  Cumulative with equipment resistances.", dur, dur));
			if (desc_short) return (format("dur %d+1d%d turns.", dur, dur));
			if (cast)
			{

				dur += randint1(dur);

				(void)inc_timed(TMD_OPP_FIRE, dur, TRUE);
				(void)inc_timed(TMD_OPP_COLD, dur, TRUE);

			}

			break;
		}

		case PRAYER_NEUTRALIZE_POISON:
		{
			if (name) return ("Neutralize Poison");
			if (desc) return ("Cures the player of poison.");
			if (desc_short) return ("");
			if (cast)
			{
				(void)clear_timed(TMD_POISONED, TRUE);
			}

			break;
		}
		case PRAYER_ORB_OF_DRAINING:
		{
			dice = 10;
			sides = 5;
			dam = plev / ((cp_ptr->flags & CF_BLESS_WEAPON) ? 1 : 2);
			rad = 2;

			if (game_mode == GAME_NPPMORIA)
			{
				dice = 3;
				sides = 6;
				dam = plev;
			}

			if (name) return ("Orb of Draining");
			if (desc) return (format("Fires a radius %d orb of holy force that does %d+%dd%d damage.", rad, dam, dice, sides));
			if (desc_short) return (format("rad %d dam %d+%dd%d ", rad, dam, dice, sides));
			if (cast)
			{
				fire_orb(GF_HOLY_ORB, dir, (damroll(dice, sides) + dam), rad);
			}

			break;
		}

		case PRAYER_CURE_CRITICAL_WOUNDS:
		{
			dice = 8;
			sides = 10;

			if (game_mode == GAME_NPPMORIA)
			{
				dice = 8;
				sides = 4;
			}

			if (name)
			{
				if (game_mode == GAME_NPPMORIA)	return ("Cure Serious Wounds");
				else return ("Cure Critical Wounds");
			}

			if (desc) return (format("Eliminates cuts and heals %dd%d hp.", dice, sides));
			if (desc_short) return (format("heal %dd%d", dice, sides));
			if (cast)
			{
				(void)hp_player(damroll(dice, sides));
				(void)set_cut(0);
			}

			break;
		}

		case PRAYER_SENSE_INVISIBLE:
		{
			dur = 24;
			if (p_ptr->timed[TMD_SINVIS])
			{
				extra = " extra";
				dur = 10;
			}

			if (name) return ("Sense Invisible");
			if (desc) return (format("Allows player to see invisible monsters for %d%s turns.", dur, extra));
			if (desc_short) return (format("dur %d turns.", dur));
			if (cast)
			{
				(void)inc_timed(TMD_SINVIS, randint(dur) + dur, TRUE);
			}

			break;
		}

		case PRAYER_PROTECTION_FROM_EVIL:
		{
			dur = 25;
			dur1 = 3 * p_ptr->lev;
			if (p_ptr->timed[TMD_PROTEVIL])
			{
				extra = " additional";
				dur = dur / 10;
				dur1 = dur1 / 10;
			}

			if (name) return ("Protection from Evil");
			if (desc) return (format("Temporary protection from evil creatures for %dd%d%s turns.", dur1, dur, extra));
			if (desc_short) return (format("%d+d%d%s turns", dur1, dur, extra));
			if (cast)
			{
				(void)inc_timed(TMD_PROTEVIL, randint(dur) + dur1, TRUE);
			}

			break;
		}

		case PRAYER_EARTHQUAKE:
		{
			rad = 10;

			if (name) return ("Earthquake");
			if (desc) return (format("Creates a radius %d earthquake centered on the player.", rad));
			if (desc_short) return (format("rad %d", rad));
			if (cast)
			{
				earthquake(py, px, rad, FALSE);
			}

			break;
		}

		case PRAYER_SENSE_SURROUNDINGS:
		{
			if (name) return ("Sense Surroundings");
			if (desc) return ("Maps the local area, reveals doors and stairs.");
			if (desc_short) return (format("rad %d", (DETECT_RADIUS + 5)));
			if (cast)
			{
				detect(DETECT_RADIUS + 5, DETECT_MAP);
			}
			break;

		}

		case PRAYER_CURE_MORTAL_WOUNDS:
		{
			dice = 12;
			sides = 10;

			if (game_mode == GAME_NPPMORIA)
			{
				dice = 16;
				sides = 4;
			}

			if (name)
			{
				if (game_mode == GAME_NPPMORIA)	return ("Cure Critical Wounds");
				else return ("Cure Mortal Wounds");
			}

			if (desc) return (format("Eliminates stunning and cuts and heals %dd%d hp.", dice, sides));
			if (desc_short) return (format("heal %dd%d", dice, sides));
			if (cast)
			{
				(void)hp_player(damroll(dice, sides));
				(void)set_cut(0);
				(void)set_stun(0);
			}

			break;
		}

		case PRAYER_TURN_UNDEAD:
		{
			if (name) return ("Turn Undead");
			if (desc) return ("Repels all undead monsters in line of sight.");
			if (desc_short) return ("");
			if (cast)
			{
				(void)turn_undead(p_ptr->lev);
			}
			break;

		}

		case PRAYER_PRAYER:
		{
			dur = 48;
			if (p_ptr->timed[TMD_BLESSED])
			{
				extra = " extra";

				dur = 10;
			}

			if (name) return ("Prayer");
			if (desc) return (format("Bonus to fighting ability and armour class for %d+d%d%s turns.", dur, dur, extra));
			if (desc_short) return (format("dur %d+d%d%s turns", dur, dur, extra));
			if (cast)
			{
				(void)inc_timed(TMD_BLESSED, randint(dur) + dur, TRUE);
			}

			break;
		}

		case PRAYER_SUN_BEAM:
		{
			/* Reaches max damage (85+10d10, average 140) at plev 46. */
			dam = 85;
			dice = 1 + (plev - 1) / 5;
			sides = 10;

			if (name) return ("Sun Beam");
			if (desc) return (format("Fires an narrow arc of intense light energy"
				" for %d+%dd%d hp damage.", dam, dice, sides));
			if (desc_short) return (format("dam %d + %dd%d", dam , dice, sides));
			if (cast)
			{
				fire_arc(GF_LIGHT, dir, dam + damroll(dice, sides), 0, 30);
			}

			break;
		}

		case PRAYER_HEAL:
		{
			dam = 325;

			if (game_mode == GAME_NPPMORIA) dam = 200;

			if (name) return ("Heal");
			if (desc) return (format("Eliminates stunning and cuts and heals %d hp.", dam));
			if (desc_short) return (format("heal %d hp.", dam));
			if (cast)
			{
				(void)hp_player(dam);
				(void)set_cut(0);
				(void)set_stun(0);
			}

			break;
		}

		case PRAYER_DISPEL_EVIL:
		{
			dam = plev * 3;

			if (name) return ("Dispel Evil");
			if (desc) return (format("Does 1d%d damage to all evil creatures in line of sight.", dam));
			if (desc_short) return (format("dam 1d%d", dam));
			if (cast)
			{
				(void)dispel_evil(randint(dam));
			}

			break;
		}

		case PRAYER_GLYPH_OF_WARDING:
		{
			if (name) return ("Glyph of Warding");
			if (desc) return ("Creates a glyph of warding beneath you.");
			if (desc_short) return ("");
			if (cast)
			{
				(void)warding_glyph();
			}

			break;
		}

		case PRAYER_HOLY_WORD:
		{

			dam = dice = 150;

			if (game_mode == GAME_NPPMORIA)
			{
				dam = plev * 4;
				dice = 1000;
			}

			if (name) return ("Holy Word");
			if (desc) return (format("Dispels evil with %d hp damage, and eliminates stunning, fear, poison and cuts and heals %d hp.", dam, dice));
			if (desc_short) return (format("dam %d, heal %d", dam, dice));
			if (cast)
			{
				(void)dispel_evil(dam);
				(void)hp_player(dice);
				(void)clear_timed(TMD_AFRAID, TRUE);
				(void)clear_timed(TMD_POISONED, TRUE);
				(void)set_stun(0);
				(void)set_cut(0);
			}

			break;
		}

		case PRAYER_DETECT_MONSTERS:
		{
			if (name) return ("Detect Monsters");
			if (desc) return ("Detects all nearby creatures.");
			if (desc_short) return (format("rad %d", DETECT_RADIUS));
			if (cast)
			{
				(void)detect(DETECT_RADIUS, DETECT_MONSTERS);
			}

			break;
		}

		case PRAYER_DETECTION:
		{
			if (name) return ("Detection");
			if (desc) return ("Detects all nearby traps, doors, stairs, treasure, objects, and creatures.");
			if (desc_short) return (format("rad %d", DETECT_RADIUS));
			if (cast)
			{
				(void)detect(DETECT_RADIUS, DETECT_ALL);
			}

			break;
		}

		case PRAYER_PERCEPTION:
		{
			if (name) return ("Perception");
			if (desc) return ("Identifies an object.");
			if (desc_short) return ("");
			if (cast)
			{
				if (!ident_spell()) return (NULL);
			}

			break;
		}

		case PRAYER_PROBING:
		{
			if (name) return ("Probing");
			if (desc) return ("Learns many attributes of a monster or feature in sight, or any feature on the map.");
			if (desc_short) return ("");
			if (cast)
			{
				(void)probing();
			}

			break;
		}

		case PRAYER_CLAIRVOYANCE:
		{
			if (name) return ("Clairvoyance");
			if (desc) return ("Lights up the entire dungeon level and shows all objects on the dungeon floor.");
			if (desc_short) return ("");
			if (cast)
			{
				wiz_light();
			}

			break;
		}

		case PRAYER_CURE_SERIOUS_WOUNDS2:
		{
			dice = 5;
			sides = 15;

			if (name) return ("Cure Serious Wounds");
			if (desc) return (format("Eliminates cuts and heals %dd%d hp.", dice, sides));
			if (desc_short) return (format("heal %dd%d", dice, sides));
			if (cast)
			{
				(void)hp_player(damroll(dice, sides));
				(void)set_cut(0);
			}

			break;
		}

		case PRAYER_CURE_MORTAL_WOUNDS2:
		{
			dice = 12;
			sides = 15;

			if (name) return ("Cure Mortal Wounds");
			if (desc) return (format("Eliminates stunning and cuts and heals %dd%d hp.", dice, sides));
			if (desc_short) return (format("heal %dd%d", dice, sides));
			if (cast)
			{
				(void)hp_player(damroll(dice, sides));
				(void)set_cut(0);
				(void)set_stun(0);
			}

			break;
		}

		case PRAYER_HEALING:
		{
			dam = 2000;

			if (name) return ("Healing");
			if (desc) return (format("Eliminates stunning and cuts and heals %d hp.", dam));
			if (desc_short) return (format("heal %d", dam));
			if (cast)
			{
				(void)hp_player(dam);
				(void)set_cut(0);
				(void)set_stun(0);
			}

			break;
		}

		case PRAYER_RESTORATION:
		{
			if (name) return ("Restoration");
			if (desc) return ("Restores all stats to their maximum.");
			if (desc_short) return ("");
			if (cast)
			{
				(void)do_res_stat(A_STR);
				(void)do_res_stat(A_INT);
				(void)do_res_stat(A_WIS);
				(void)do_res_stat(A_DEX);
				(void)do_res_stat(A_CON);
				(void)do_res_stat(A_CHR);
			}

			break;
		}

		case PRAYER_REMEMBRANCE:
		{
			if (name) return ("Remembrance");
			if (desc) return ("Restores experience to maximum.");
			if (desc_short) return ("");
			if (cast)
			{
				(void)restore_level();
			}

			break;
		}

		case PRAYER_SUN_BURST:
		{
			dice = 25 + plev / 2;
			sides = 7;
			rad = 5;

			if (name) return ("Sun Burst");
			if (desc) return (format("You cast a radius %d orb of hard light that deals %dd%d damage.", rad, dice, sides));
			if (desc_short) return (format("rad %d dam %dd%d", rad, dice, sides));
			if (cast)
			{
				fire_orb(GF_LIGHT, dir, damroll(dice, sides), rad);
			}

			break;
		}

		case PRAYER_DISPEL_EVIL2:
		{
			dice = 2;
			sides = plev * 3;

			if (name) return ("Dispel Evil");
			if (desc) return (format("Does %dd%d damage to all evil creatures in line of sight.", dice, sides));
			if (desc_short) return (format("dam %dd%d", dice, sides));
			if (cast)
			{
				(void)dispel_evil(damroll(dice, sides));
			}

			break;
		}

		case PRAYER_BANISH_EVIL:
		{
			dam = 150;

			if (name) return ("Repel Evil");
			if (desc) return ("Attempts to teleport away all evil monsters in line of sight.");
			if (desc_short) return ("dam 150");
			if (cast)
			{
				if (banish_evil(dam))
				{
					msg_print("The power of your god banishes evil!");
				}
			}

			break;
		}

		case PRAYER_WORD_OF_DESTRUCTION:
		{
			rad = 15;

			if (name) return ("Word of Destruction");
			if (desc) return (format("Creates a radius %d earthquake.  Deletes all non-quest monsters.", rad));
			if (desc_short) return (format("rad %d", rad));
			if (cast)
			{
				destroy_area(py, px, rad);
			}

			break;
		}

		case PRAYER_JUDGEMENT_OF_MANDOS:
		{
			rad = 9;
			dice = 20;
			sides = 10;
			dam1 = 5 * (plev - 30);

			if (name) return ("Judgement of Mandos");
			if (desc) return (format("You release a massive starburst of holy energy."
				" Affected creatures suffer %d+%dd%d hp damage or twice as much if evil.",
				dam1, dice, sides));
			if (desc_short) return (format("rad %d dam %d + %dd%d", rad, dam1, dice, sides));
			if (cast)
			{
				dam = dam1 + damroll(dice, sides);
				fire_star(GF_HOLY_ORB, dam, rad, 0L);
			}
			break;

		}

		case PRAYER_UNBARRING_WAYS:
		{
			if (name) return ("Unbarring Ways");
			if (desc) return ("Fires a beam that disarms traps and chests and destroys doors.");
			if (desc_short) return ("");
			if (cast)
			{
				destroy_door(dir);
			}

			break;
		}

		case PRAYER_RECHARGING:
		{
			if (name) return ("Recharging");
			if (desc) return ("Attempts to recharge a wand, staff, or rod.");
			if (desc_short) return ("");
			if (cast)
			{
				(void)recharge(15 + (plev / 2), FALSE, 15 + (plev / 2));
			}

			break;
		}

		case PRAYER_DISPEL_CURSE:
		{
			if (name) return ("Dispel Curse");
			if (desc) return ("Removes standard and heavy curses.");
			if (desc_short) return ("");
			if (cast)
			{
				remove_curse(TRUE);
			}

			break;
		}

		case PRAYER_ENCHANT_WEAPON:
		{
			if (name) return ("Enchant Weapon");
			if (desc) return ("Attempts to enchant one weapon.");
			if (desc_short) return ("");
			if (cast)
			{
				(void) enchant_spell(rand_int(4) + 1, rand_int(4) + 1, 0);
			}

			break;
		}

		case PRAYER_ENCHANT_ARMOUR:
		{
			if (name) return ("Enchant Armour");
			if (desc) return ("Attempts to enchant one piece of armor.");
			if (desc_short) return ("");
			if (cast)
			{
				(void)enchant_spell(0, 0, rand_int(3) + 2);
			}

			break;

		}

		case PRAYER_ELEMENTAL_BRAND:
		{
			if (name) return ("Elemental Brand");
			if (desc) return ("Attempts to brand one weapon.");
			if (desc_short) return ("");
			if (cast)
			{
				(void)brand_weapon(TRUE);
			}

			break;
		}

		case PRAYER_BLINK:
		{
			dam = 10;

			if (name) return ("Blink");
			if (desc) return (format("Random minor displacement up to %d squares.", dam));
			if (desc_short) return (format("range %d", dam));
			if (cast)
			{
				teleport_player(dam, FALSE);
			}

			break;

		}

		case PRAYER_TELEPORT_SELF:
		{
			dam = (plev * 8);

			if (name) return ("Teleport Self");
			if (desc) return (format("Random major displacement up to %d squares.", dam));
			if (desc_short) return (format("range %d", dam));
			if (cast)
			{
				teleport_player(dam, FALSE);
			}

			break;
		}

		case PRAYER_TELEPORT_OTHER:
		{
			if (name) return ("Teleport Other");
			if (desc) return ("Attempts to teleport a monster away.");
			if (desc_short) return ("");
			if (cast)
			{
				(void)teleport_monster(dir);
			}

			break;
		}

		case PRAYER_TELEPORT_LEVEL:
		{
			if (name) return ("Teleport Level");
			if (desc) return ("Immediately takes you to the next level up or down.");
			if (desc_short) return ("");
			if (cast)
			{
				if(!teleport_player_level(SOURCE_PLAYER)) return (NULL);
			}

			break;

		}

		case PRAYER_WORD_OF_RECALL:
		{
			if (name) return ("Word of Recall");
			if (desc) return ("Recalls you to the town, or to the recall level in the dungeon.");
			if (desc_short) return ("");
			if (cast)
			{
				if (!set_recall()) return (NULL);
			}

			break;
		}

		case PRAYER_ALTER_REALITY:
		{
			if (name) return ("Alter Reality");
			if (desc) return ("Redraws the current level.");
			if (desc_short) return ("");
			if (cast)
			{
				/* Ironman */
				if (adult_ironman && !p_ptr->total_winner)
				{
					msg_print("Nothing happens.");
				}

				else
				{
					msg_print("The world changes!");

					/* Leaving */
					p_ptr->leaving = TRUE;
					p_ptr->autosave = TRUE;
				}

			}


			break;
		}

		case PRAYER_MASS_IDENTIFY:
		{
			rad = 3;

			if (name) return ("Mass Identify");
			if (desc) return ("Identifies all nearby objects, including player equipment and inventory.");
			if (desc_short) return (format("rad %d", rad));
			if (cast)
			{
				mass_identify(rad);
			}

			break;
		}

		case PRAYER_FIND_TRAPS:
		{
			if (name) return ("Find Traps");
			if (desc) return ("Detects nearby traps.");
			if (desc_short) return (format("rad %d", DETECT_RADIUS));
			if (cast)
			{
				(void)detect(DETECT_RADIUS, DETECT_TRAPS);
			}

			break;
		}

		case PRAYER_DETECT_DOORS_STAIRS:
		{
			if (name) return ("Detect Doors/Stairs");
			if (desc) return ("Detects nearby doors and stairs.");
			if (desc_short) return (format("rad %d", DETECT_RADIUS));
			if (cast)
			{
				(void)detect(DETECT_RADIUS, DETECT_DOORS_STAIRS);
			}

			break;
		}

		case PRAYER_CONFUSE_MONSTER:
		{
			if (name) return ("Confuse Monster");
			if (desc) return ("Attempts to confuse one monster.");
			if (desc_short) return (format(""));
			if (cast)
			{
				(void)confuse_monster(dir, plev);
			}

			break;
		}

		case PRAYER_CREATE_FOOD:
		 {
		 	if (name) return ("Create Food");
		 	if (desc) return ("Magically creates one ration of food.");
		 	if (desc_short) return ("");
		 	if (cast)
		 	{
		 		create_food();
		 	}
			break;
		}

		case PRAYER_DISPEL_UNDEAD:
		{
			dice = 1;
			sides = plev * 3;

			if (name) return ("Dispel Undead");
			if (desc) return (format("Does %dd%d damage to all evil creatures in line of sight.", dice, sides));
			if (desc_short) return (format("dam %dd%d", dice, sides));
			if (cast)
			{
				(void)dispel_undead(damroll(dice, sides));
			}

			break;
		}
	}

	/* success  */
	return ("Success!");
}


cptr cast_spell(int mode, int tval, int index, int dir)
{
	if (tval == TV_MAGIC_BOOK)
	{
		return do_mage_spell(mode, index, dir);
	}

	else if (tval == TV_DRUID_BOOK)
	{
		return do_druid_incantation(mode, index, dir);
	}
	else /*Priest*/
	{
		return do_priest_prayer(mode, index, dir);
	}
}

/*
 * Return the player realm for the spell_list table.  Assumes player is a spellcaster.
 * We don't return any error because this value is going to be looked up in a table,
 * & would cause the game to crash
 */
int get_player_spell_realm(void)
{
	/* Mage or priest spells? */
	if (cp_ptr->spell_book == TV_MAGIC_BOOK) 	return (MAGE_REALM);
	if (cp_ptr->spell_book == TV_PRAYER_BOOK)	return (PRIEST_REALM);
	/*Druid Book*/								return (DRUID_REALM);
}


cptr get_spell_name(int tval, int spell)
{
	if (tval == TV_MAGIC_BOOK)
		return do_mage_spell(MODE_SPELL_NAME, spell,0);
	else if (tval == TV_PRAYER_BOOK)
		return do_priest_prayer(MODE_SPELL_NAME, spell, 0);
	/*TV_DRUID_BOOK*/
	else return do_druid_incantation(MODE_SPELL_NAME, spell,0);
}
