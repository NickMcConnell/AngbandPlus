/*
 * File: x-spell.c
 * Purpose: Spell effect definitions and information about them
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
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
#include "effects.h"
#include "object/tvalsval.h"

/* TODO This code needs to be updated */
cptr get_spell_name(int spell)
{
	UNREFERENCED_PARAMETER(spell);
	return "T.B.D."; /* s_info[spell].name; */
}

void get_spell_info(int spell, char *p, size_t len)
{
	int plev = p_ptr->lev;

	/* Blank 'p' first */
	p[0] = '\0';


	/* Analyze the spell */
	switch (spell)
	{
	case SPELL_MAGIC_MISSILE:	
		strnfmt(p, len, " dam %dd4", 3 + ((plev - 1) / 5));
		break;
	case SPELL_DETECT_MONSTERS:	
		/* No entry */
		break;
	case SPELL_PHASE_DOOR:	
		strnfmt(p, len, " range 10");
		break;
	case SPELL_LIGHT_AREA:	
		strnfmt(p, len, " dam 2d%d", (plev / 2));
		break; 
	case SPELL_TREASURE_DETECTION:	
		strnfmt(p, len, " range %dd2+%d", (plev / 2), (plev / 10) + 1);
		break;
	case SPELL_CURE_LIGHT_WOUNDS:  /* Same as CURE_MINOR_WOUNDS, except harder */
		my_strcpy(p, " heal 15%", len);
		break;
	case SPELL_OBJECT_DETECTION:	
		/* No entry */
		break;
	case SPELL_FIND_TRAPS_DOORS:	
		/* No entry */
		break;
	case SPELL_STINKING_CLOUD:	
		strnfmt(p, len, " dam %d", 10 + (plev / 2));
		break;
	case SPELL_CREATE_CONFUSION:	
		/* No entry */
		break;
	case SPELL_LIGHTNING_BOLT:	
		strnfmt(p, len, " dam %dd6", (3 + ((plev - 5) / 6)));
		break;
	case SPELL_TRAP_DOOR_DESTRUCTION:	
		/* No entry */
		break;
	case SPELL_SLEEP_I:	
		/* No entry */
		break;
	case SPELL_CURE_POISON:	
		/* No entry */
		break;
	case SPELL_TELEPORT_SELF: /* TODO Check how far TELEPORT_SELF goes */	
		strnfmt(p, len, " range %d", plev * 5);
		break;
	case SPELL_SPEAR_OF_LIGHT:	
		strnfmt(p, len, " dam 6d8");
		break;
	case SPELL_FROST_BOLT:	
		strnfmt(p, len, " dam %dd8", (5 + ((plev - 5) / 4)));
		break;
	case SPELL_TURN_STONE_TO_MUD:	
		/* No entry */
		break;
	case SPELL_SATISFY_HUNGER:	
		/* No entry */
		break;
	case SPELL_RECHARGE_ITEM_I:	
		/* No entry */
		break;
	case SPELL_SLEEP_II:	
		/* No entry */
		break;
	case SPELL_POLYMORPH_OTHER:	
		/* No entry */
		break;
	case SPELL_IDENTIFY:	
		/* No entry */
		break;
	case SPELL_SLEEP_III:	
		/* No entry */
		break;
	case SPELL_FIRE_BOLT:	
		strnfmt(p, len, " dam %dd8", (6 + ((plev - 5) / 4)));
		break;
	case SPELL_SLOW_MONSTER:	
		/* No entry */
		break;
	case SPELL_FROST_BALL:	
		strnfmt(p, len, " dam %d", 30 + plev);
		break;
	case SPELL_RECHARGE_ITEM_II: /* greater recharging */	
		/* No entry */
		break;
	case SPELL_TELEPORT_OTHER:	
		/* No entry */
		break;
	case SPELL_HASTE_SELF:	
		strnfmt(p, len, " dur %d+d20", plev);
		break;
	case SPELL_FIRE_BALL:	
		strnfmt(p, len, " dam %d", 55 + plev);
		break;
	case SPELL_WORD_OF_DESTRUCTION:	
		strnfmt(p, len, " rad 15");
		break;
	case SPELL_GENOCIDE:	
		/* No entry */
		break;
	case SPELL_RESIST_FIRE:	
		strnfmt(p, len, " dur 20+d20");
		break;
	case SPELL_RESIST_COLD:	
		strnfmt(p, len, " dur 20+d20");
		break;
	case SPELL_RESIST_ACID:	
		strnfmt(p, len, " dur 20+d20");
		break;
	case SPELL_RESIST_POISON:	
		strnfmt(p, len, " dur 20+d20");
		break;
	case SPELL_RESISTANCE:	
		strnfmt(p, len, " dur 20+d20");
		break;
	case SPELL_DOOR_CREATION:	
		/* No entry */
		break;
	case SPELL_STAIR_CREATION:	
		/* No entry */
		break;
	case SPELL_TELEPORT_LEVEL:	
		/* No entry */
		break;
	case SPELL_SHAKE_GROUND:	
		strnfmt(p, len, " rad 10");
		break;
	case SPELL_WORD_OF_RECALL:	
		/* No entry */
		break;
	case SPELL_SENSE_EVIL: /* Alternative name for mage version */	
		/* No entry */
		break;
	case SPELL_DETECT_ENCHANTMENT:	
		/* No entry */
		break;
	case SPELL_RECHARGE_ITEM_III:	
		/* No entry */
		break;
	case SPELL_MASS_GENOCIDE:	
		/* No entry */
		break;
	case SPELL_HEROISM:	
		strnfmt(p, len, " dur 25+d25");
		break;
	case SPELL_SHIELD:	
		strnfmt(p, len, " dur 30+d20");
		break;
	case SPELL_BERSERKER:	
		strnfmt(p, len, " dur 25+d25");
		break;
	case SPELL_ESSENCE_OF_SPEED:	
		/* No entry */
		break;
	case SPELL_INVULNERABILITY:	
		/* No entry */
		break;
	case SPELL_ACID_BOLT:	
		strnfmt(p, len, " dam %dd8", (8 + ((plev - 5) / 4)));
		break;
	case SPELL_CLOUD_KILL:	
		strnfmt(p, len, " dam %d", 40 + (plev / 2));
		break;
	case SPELL_ACID_BALL:	
		strnfmt(p, len, " dam %d", 40 + plev);
		break;
	case SPELL_ICE_STORM:	
		strnfmt(p, len, " dam %d", 50 + (plev * 2));
		break;
	case SPELL_METEOR_SWARM:	
		strnfmt(p, len, " dam %dx%d", 30 + plev / 2, 2 + plev / 20);
		break;
	case SPELL_HELLFIRE:	
		strnfmt(p, len, " dam %d", 300 + plev * 2);
		break;
	case SPELL_MAGIC_REFLECTION:	
		/* No entry */
		break;
	case SPELL_CREATE_SIMPLE_TRAP:	
		/* No entry */
		break;
	case SPELL_CREATE_EFFECTIVE_TRAP:	
		/* No entry */
		break;
	case SPELL_CREATE_DANGEROUS_TRAP:	
		/* No entry */
		break;
	case SPELL_CREATE_DEATH_TRAP:	
		/* No entry */
		break;
	case SPELL_CREATE_COMPLEX_TRAP:	
		/* No entry */
		break;
	case SPELL_DETECT_EVIL:	
		/* No entry */
		break;
	case SPELL_CURE_MINOR_WOUNDS: /* Same as CURE_LIGHT_WOUNDS (mage), except easier */
		my_strcpy(p, " heal 15%", len);
		break;
	case SPELL_BLESS:	
		my_strcpy(p, " dur 12+d12", len); /* Why is this 'my_strcpy' and the rest 'strnfmt' ? */
		break;
	case SPELL_REMOVE_FEAR:	
		/* No entry */
		break;
	case SPELL_CALL_LIGHT:	
		strnfmt(p, len, " dam 2d%d", (plev / 2));
		break; 
	case SPELL_FIND_TRAPS:	
		/* No entry */
		break;
	case SPELL_DETECT_DOORS_STAIRS:	
		/* No entry */
		break;
	case SPELL_SLOW_POISON:	
		/* No entry */
		break;
	case SPELL_TERRIFY_CREATURE: /* AKA SCARE_MONSTER */	
		/* No entry */
		break;
	case SPELL_PORTAL:	
		strnfmt(p, len, " range %d", 3 * plev);
		break;
	case SPELL_CHANT:	
		my_strcpy(p, " dur 24+d24", len);
		break;
	case SPELL_SANCTUARY:	
		/* No entry */
		break;
	case SPELL_DIVINE_SATIATION:	
		/* No entry */
		break;
	case SPELL_REMOVE_CURSE:	
		/* No entry */
		break;
	case SPELL_RESIST_HEAT_COLD:	
		my_strcpy(p, " dur 10+d10", len);
		break;
	case SPELL_NEUTRALIZE_POISON:
		/* No entry */
		break;
	case SPELL_ORB_OF_DRAINING:	
		strnfmt(p, len, " %d+3d6", plev +
		        (plev / ((cp_ptr->flags & CF_BLESS_WEAPON) ? 2 : 4)));
		break;
	case SPELL_CURE_CRIT_WOUNDS:	
		my_strcpy(p, " heal 25%", len);
		break;
	case SPELL_SENSE_INVISIBLE:	
		my_strcpy(p, " dur 24+d24", len);
		break;
	case SPELL_PROTECTION_FROM_EVIL:	
		strnfmt(p, len, " dur %d+d25", 3 * plev);
		break;
	case SPELL_EARTHQUAKE:	
		strnfmt(p, len, " rad 10");
		break;
	case SPELL_SENSE_SURROUNDINGS:	
		/* No entry */
		break;
	case SPELL_CURE_CRIT_WOUNDS_II: /* Easier to cast? */	
		my_strcpy(p, " heal 25%", len);
		break;
	case SPELL_TURN_UNDEAD:	
		/* No entry */
		break;
	case SPELL_PRAYER:	
		my_strcpy(p, " dur 48+d48", len);
		break;
	case SPELL_DISPEL_UNDEAD:	
		strnfmt(p, len, " dam d%d", 3 * plev);
		break;
	case SPELL_HEAL:	
		my_strcpy(p, " heal 35%", len);
		break;
	case SPELL_HEALING:	
		my_strcpy(p, " heal 2000", len);
		break;
	case SPELL_DISPEL_EVIL:	
		strnfmt(p, len, " dam d%d", 3 * plev);
		break;
	case SPELL_GLYPH_OF_WARDING:	
		/* No entry */
		break;
	case SPELL_HOLY_WORD:	
		my_strcpy(p, " heal 1000", len);
		break;
	case SPELL_BLINK:	
		my_strcpy(p, " range 10", len);
		break;
	case SPELL_TELEPORT: /* Same as teleport self ? */	
		strnfmt(p, len, " range %d", plev*8);
		break;
	case SPELL_TELEPORT_AWAY: /* Same as teleport other ? */	
		/* No entry */
		break;
	case SPELL_ESCAPE_LEVEL: /* Same as teleport level ? */	
		/* No entry */
		break;
	case SPELL_ALTER_REALITY:	
		/* No entry */
		break;
	case SPELL_SENSE_MONSTERS:	
		/* No entry */
		break;
	case SPELL_DETECTION:	
		/* No entry */
		break;
	case SPELL_PERCEPTION:	
		/* No entry */
		break;
	case SPELL_PROBING:	
		/* No entry */
		break;
	case SPELL_CLAIRVOYANCE:	
		/* No entry */
		break;
	case SPELL_CURE_SER_WOUNDS:	
		my_strcpy(p, " heal 20%", len);
		break;
	case SPELL_CURE_SER_WOUNDS_II:	/* Easier to cast? */
		my_strcpy(p, " heal 20%", len);
		break;
	case SPELL_RESTORATION:	
		/* No entry */
		break;
	case SPELL_REMEMBRANCE:	
		/* No entry */
		break;
	case SPELL_UNBARRING_WAYS:	
		/* No entry */
		break;
	case SPELL_RECHARGING:	
		/* No entry */
		break;
	case SPELL_DISPEL_CURSE:	
		/* No entry */
		break;
	case SPELL_ENCHANT_WEAPON: /*   *ENCHANT WEAPON*   */	
		/* No entry */
		break;
	case SPELL_ENCHANT_ARMOR: /*   *ENCHANT ARMOR*   */	
		/* No entry */
		break;
	case SPELL_ELEMENTAL_BRAND:	
		/* No entry */
		break;
	case SPELL_DISPEL_UNDEAD_II:	
		strnfmt(p, len, " dam d%d", 4 * plev);
		break;
	case SPELL_DISPEL_EVIL_II:	
		strnfmt(p, len, " dam d%d", 4 * plev);
		break;
	case SPELL_BANISHMENT:	
		/* No entry */
		break;
	case SPELL_HEAVENS_WRATH:	
		strnfmt(p, len, " rad 15");
		break;
	case SPELL_ANNIHILATION:	
		my_strcpy(p, " dam 200", len);
		break;
	}
	return;
}


static int beam_chance(void)
{
	int plev = p_ptr->lev;
	return ((cp_ptr->flags & CF_BEAM) ? plev : (plev / 2));
}

bool cast_spell(int index)
{
	s16b py = p_ptr->py;
	s16b px = p_ptr->px;

	int dir;

	int plev = p_ptr->lev;

	/* Hack -- chance of "beam" instead of "bolt" */
	int beam = beam_chance();

	/* Spells. */
	switch (index)
	{
		case SPELL_MAGIC_MISSILE:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_bolt_or_beam(beam-10, GF_MISSILE, dir,
			                  damroll(3 + ((plev - 1) / 5), 4));
			break;
		}
		case SPELL_DETECT_MONSTERS:
		{
			(void)detect_monsters_normal(TRUE);
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
			(void)detect_treasure(TRUE);
			break;
		}
		case SPELL_CURE_LIGHT_WOUNDS:
		{
			(void)heal_player(15, 15); 
			(void)dec_timed(TMD_CUT, 20, TRUE); 
			(void)dec_timed(TMD_CONFUSED, 20, TRUE); 
			(void)clear_timed(TMD_BLIND, TRUE); 
			break;
		}
		case SPELL_OBJECT_DETECTION:
		{
			(void)detect_objects(TRUE);
			break;
		}
		case SPELL_FIND_TRAPS_DOORS:
		{
			(void)detect_traps(TRUE);
			(void)detect_doorstairs(TRUE);
			break;
		}
		case SPELL_STINKING_CLOUD:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_ball(GF_POIS, dir, 10 + (plev / 2), 2);
			break;
		}
		case SPELL_CREATE_CONFUSION:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			(void)confuse_monster(dir, plev);
			break;
		}
		case SPELL_LIGHTNING_BOLT:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_beam(GF_ELEC, dir,
			          damroll(3+((plev-5)/6), 6));
			break;
		}
		case SPELL_TRAP_DOOR_DESTRUCTION:
		{
			(void)destroy_doors_touch();
			break;
		}
		case SPELL_SLEEP_I:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			(void)sleep_monster(dir);
			break;
		}
		case SPELL_CURE_POISON:
		{
			(void)clear_timed(TMD_POISONED, TRUE);
			break;
		}
		case SPELL_TELEPORT_SELF:
		{
			teleport_player(plev * 5);
			break;
		}
		case SPELL_SPEAR_OF_LIGHT: /* spear of light */
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			msg_print("A line of blue shimmering light appears.");
			lite_line(dir);
			break;
		}
		case SPELL_FROST_BOLT:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_bolt_or_beam(beam-10, GF_COLD, dir,
			                  damroll(5+((plev-5)/4), 8));
			break;
		}
		case SPELL_TURN_STONE_TO_MUD:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
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
			return recharge(2 + plev / 5);
			break;
		}
		case SPELL_SLEEP_II:
		{
			(void)sleep_monsters_touch();
			break;
		}
		case SPELL_POLYMORPH_OTHER:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			(void)poly_monster(dir);
			break;
		}
		case SPELL_IDENTIFY:
		{
			return ident_spell();
		}
		case SPELL_SLEEP_III:
		{
			(void)sleep_monsters();
			break;
		}
		case SPELL_FIRE_BOLT:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_bolt_or_beam(beam, GF_FIRE, dir,
			                  damroll(6+((plev-5)/4), 8));
			break;
		}
		case SPELL_SLOW_MONSTER:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			(void)slow_monster(dir);
			break;
		}
		case SPELL_FROST_BALL:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_ball(GF_COLD, dir, 30 + (plev), 2);
			break;
		}
		case SPELL_RECHARGE_ITEM_II: /* greater recharging */
		{
			return recharge(50 + plev);
		}
		case SPELL_TELEPORT_OTHER:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			(void)teleport_monster(dir);
			break;
		}
		case SPELL_HASTE_SELF:
		{
			if (!p_ptr->timed[TMD_FAST])
				(void)set_timed(TMD_FAST, randint1(20) + plev, TRUE);
			else
				(void)inc_timed(TMD_FAST, randint1(5), TRUE);
			break;
		}
		case SPELL_FIRE_BALL:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_ball(GF_FIRE, dir, 55 + (plev), 2);
			break;
		}
		case SPELL_WORD_OF_DESTRUCTION:
		{
			destroy_area(py, px, 15, TRUE);
			break;
		}
		case SPELL_GENOCIDE:
		{
			return banishment();
		}
		case SPELL_RESIST_FIRE:
		{
			(void)inc_timed(TMD_OPP_FIRE, randint1(20) + 20, TRUE);
			break;
		}
		case SPELL_RESIST_COLD:
		{
			(void)inc_timed(TMD_OPP_COLD, randint1(20) + 20, TRUE);
			break;
		}
		case SPELL_RESIST_ACID:
		{
			(void)inc_timed(TMD_OPP_ACID, randint1(20) + 20, TRUE);
			break;
		}
		case SPELL_RESIST_POISON:
		{
			(void)inc_timed(TMD_OPP_POIS, randint1(20) + 20, TRUE);
			break;
		}
		case SPELL_RESISTANCE:
		{
			int time = randint1(20) + 20;
			(void)inc_timed(TMD_OPP_ACID, time, TRUE);
			(void)inc_timed(TMD_OPP_ELEC, time, TRUE);
			(void)inc_timed(TMD_OPP_FIRE, time, TRUE);
			(void)inc_timed(TMD_OPP_COLD, time, TRUE);
			(void)inc_timed(TMD_OPP_POIS, time, TRUE);
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
		case SPELL_SHAKE_GROUND:
		{
			earthquake(py, px, 10);
			break;
		}
		case SPELL_WORD_OF_RECALL:
		{
			set_recall();
			break;
		}
		case SPELL_SENSE_EVIL: /* Alternative name for mage version */
		{
			(void)detect_monsters_evil(TRUE);
			break;
		}
		case SPELL_DETECT_ENCHANTMENT:
		{
			(void)detect_objects_magic(TRUE);
			break;
		}
		case SPELL_RECHARGE_ITEM_III:
		{
			return recharge(100);
		}
		case SPELL_MASS_GENOCIDE:		
		{		
			mass_banishment();	
			break;	
		}		
		case SPELL_HEROISM:
		{
			(void)hp_player(10);
			(void)inc_timed(TMD_HERO, randint1(25) + 25, TRUE);
			(void)clear_timed(TMD_AFRAID, TRUE);
			break;
		}
		case SPELL_BERSERKER:
		{
			(void)hp_player(30);
			(void)inc_timed(TMD_SHERO, randint1(25) + 25, TRUE);
			(void)clear_timed(TMD_AFRAID, TRUE);
			break;
		}
		case SPELL_ESSENCE_OF_SPEED:
		{
			if (!p_ptr->timed[TMD_FAST])
				(void)set_timed(TMD_FAST, randint1(30) + 30 + plev, TRUE);
			else
				(void)inc_timed(TMD_FAST, randint1(10), TRUE);
			break;
		}
		case SPELL_INVULNERABILITY:
		{
			if (p_ptr->timed[TMD_INVULN])
			{
				msg_print("You feel that you are making a terrible mistake...");
				(void)clear_timed(TMD_INVULN, TRUE);
			}
			else
			{
				(void)set_timed(TMD_INVULN, 2000 + randint1(plev * 60), TRUE);
			}
			break;
		}
		case SPELL_ACID_BOLT:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_bolt_or_beam(beam, GF_ACID, dir, damroll(8+((plev-5)/4), 8));
			break;
		}
		case SPELL_CLOUD_KILL:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_ball(GF_POIS, dir, 40 + (plev / 2), 3);
			break;
		}
		case SPELL_ACID_BALL:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_ball(GF_ACID, dir, 40 + (plev), 2);
			break;
		}
		case SPELL_ICE_STORM:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_ball(GF_ICE, dir, 50 + (plev * 2), 3);
			break;
		}
		case SPELL_METEOR_SWARM:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_swarm(2 + plev / 20, GF_METEOR, dir, 30 + plev / 2, 1);
			break;
		}
		case SPELL_HELLFIRE:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_ball(GF_MANA, dir, 300 + (plev * 2), 3);
			break;
		}
		case SPELL_MAGIC_REFLECTION:
		{
/*			if (!p_ptr->timed[TMD_REFLECT])
				(void)set_timed(TMD_REFLECT, randint1(20) + 15, TRUE);
			else
				(void)inc_timed(TMD_REFLECT, randint1(10), TRUE);  */
			break; 
		}
/* TODO Implement trap creation spells. */
		case SPELL_CREATE_SIMPLE_TRAP:				
		{				
/*			if (!make_trap_from_spell(0, FALSE)) return (TRUE);			*/
			break;			
		}				
		case SPELL_CREATE_EFFECTIVE_TRAP:				
		{				
/*			if (!make_trap_from_spell(1, FALSE)) return (TRUE);			*/
			break;			
		}				
		case SPELL_CREATE_DANGEROUS_TRAP:				
		{				
/*			if (!make_trap_from_spell(2, FALSE)) return (TRUE);			*/
			break;			
		}				
		case SPELL_CREATE_DEATH_TRAP:				
		{				
/*			if (!make_trap_from_spell(3, FALSE)) return (TRUE);			*/
			break;			
		}				
		case SPELL_CREATE_COMPLEX_TRAP:				
		{				
/*			if (!make_trap_from_spell(3, FALSE)) return (TRUE);			*/
			break;			
		}				

/* ***************************************************************** */
/*   Spells above here were 'mage spells'                            */
/* ***************************************************************** */

		case SPELL_DETECT_EVIL:
		{
			(void)detect_monsters_evil(TRUE);
			break;
		}
		case SPELL_CURE_MINOR_WOUNDS:
		{
			(void)heal_player(15, 15); 
			(void)dec_timed(TMD_CUT, 20, TRUE); 
			(void)dec_timed(TMD_CONFUSED, 20, TRUE); 
			(void)clear_timed(TMD_BLIND, TRUE); 
			break;
		}
		case SPELL_BLESS:
		{
			(void)inc_timed(TMD_BLESSED, randint1(12) + 12, TRUE);
			break;
		}
		case SPELL_REMOVE_FEAR:
		{
			(void)clear_timed(TMD_AFRAID, TRUE);
			break;
		}
		case SPELL_CALL_LIGHT:
		{
			(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
			break;
		}
		case SPELL_FIND_TRAPS:
		{
			(void)detect_traps(TRUE);
			break;
		}
		case SPELL_DETECT_DOORS_STAIRS:
		{
			(void)detect_doorstairs(TRUE);
			break;
		}
		case SPELL_SLOW_POISON:
		{
			(void)set_timed(TMD_POISONED, p_ptr->timed[TMD_POISONED] / 2, TRUE);
			break;
		}
		case SPELL_TERRIFY_CREATURE: /* AKA SCARE_MONSTER */
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			(void)fear_monster(dir, plev);
			break;
		}
		case SPELL_PORTAL:
		{
			teleport_player(plev * 3);
			break;
		}
		case SPELL_CHANT:
		{
			(void)inc_timed(TMD_BLESSED, randint1(24) + 24, TRUE);
			break;
		}
		case SPELL_SANCTUARY:
		{
			(void)sleep_monsters_touch();
			break;
		}
		case SPELL_DIVINE_SATIATION:
		{
			(void)set_food(PY_FOOD_MAX - 1);
			break;
		}
		case SPELL_REMOVE_CURSE:
		{
			remove_curse();
			break;
		}
		case SPELL_RESIST_HEAT_COLD:
		{
			(void)inc_timed(TMD_OPP_FIRE, randint1(10) + 10, TRUE);
			(void)inc_timed(TMD_OPP_COLD, randint1(10) + 10, TRUE);
			break;
		} 
		case SPELL_NEUTRALIZE_POISON:
		{
			(void)clear_timed(TMD_POISONED, TRUE);
			break;
		}
		case SPELL_ORB_OF_DRAINING:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_ball(GF_HOLY_ORB, dir,
			          (damroll(3, 6) + plev +
			           (plev / ((cp_ptr->flags & CF_BLESS_WEAPON) ? 2 : 4))),
			          ((plev < 30) ? 2 : 3));
			break;
		}
		case SPELL_CURE_CRIT_WOUNDS:
		{
			(void)heal_player(25, 30); 
			(void)clear_timed(TMD_CUT, TRUE); 
			(void)clear_timed(TMD_AMNESIA, TRUE); 
			(void)clear_timed(TMD_CONFUSED, TRUE); 
			(void)clear_timed(TMD_BLIND, TRUE); 
			(void)clear_timed(TMD_POISONED, TRUE); 
			(void)clear_timed(TMD_STUN, TRUE); 
		}
		case SPELL_SENSE_INVISIBLE:
		{
			(void)inc_timed(TMD_SINVIS, randint1(24) + 24, TRUE);
			break;
		}
		case SPELL_PROTECTION_FROM_EVIL:
		{
			(void)inc_timed(TMD_PROTEVIL, randint1(25) + 3 * p_ptr->lev, TRUE);
			break;
		}
		case SPELL_EARTHQUAKE:
		{
			earthquake(py, px, 10);
			break;
		}
		case SPELL_SENSE_SURROUNDINGS:
		{
			map_area();
			break;
		}
		case SPELL_CURE_CRIT_WOUNDS_II: /* Easier to cast? */
		{
			(void)heal_player(25, 30); 
			(void)clear_timed(TMD_CUT, TRUE); 
			(void)clear_timed(TMD_AMNESIA, TRUE); 
			(void)clear_timed(TMD_CONFUSED, TRUE); 
			(void)clear_timed(TMD_BLIND, TRUE); 
			(void)clear_timed(TMD_POISONED, TRUE); 
			(void)clear_timed(TMD_STUN, TRUE); 
			break;
		}
		case SPELL_TURN_UNDEAD:
		{
			(void)turn_undead();
			break;
		}
		case SPELL_PRAYER:
		{
			(void)inc_timed(TMD_BLESSED, randint1(48) + 48, TRUE);
			break;
		}
		case SPELL_DISPEL_UNDEAD:
		{
			(void)dispel_undead(randint1(plev * 3));
			break;
		}
		case SPELL_HEAL:
		{
			int amt = (p_ptr->mhp * 35) / 100; 
			if (amt < 300) amt = 300; 

			(void)hp_player(amt);
			(void)clear_timed(TMD_STUN, TRUE);
			(void)clear_timed(TMD_CUT, TRUE);
			(void)clear_timed(TMD_AMNESIA, TRUE); 
			(void)clear_timed(TMD_CONFUSED, TRUE); 
			(void)clear_timed(TMD_BLIND, TRUE); 
			(void)clear_timed(TMD_POISONED, TRUE); 
			break;
		}
		case SPELL_DISPEL_EVIL:
		{
			(void)dispel_evil(randint1(plev * 3));
			break;
		}
		case SPELL_GLYPH_OF_WARDING:
		{
			warding_glyph();
			break;
		}
		case SPELL_HOLY_WORD:
		{
			(void)dispel_evil(randint1(plev * 4));
			(void)hp_player(1000);
			(void)clear_timed(TMD_AFRAID, TRUE);
			(void)clear_timed(TMD_POISONED, TRUE);
			(void)clear_timed(TMD_STUN, TRUE);
			(void)clear_timed(TMD_CUT, TRUE);
			break;
		}
		case SPELL_BLINK:
		{
			teleport_player(10);
			break;
		}
		case SPELL_TELEPORT: /* Same as teleport self ? */
		{
			teleport_player(plev * 8);
			break;
		}
		case SPELL_TELEPORT_AWAY: /* Same as teleport other ? */
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			(void)teleport_monster(dir);
			break;
		}
		case SPELL_ESCAPE_LEVEL: /* Same as teleport level ? */
		{
			(void)teleport_player_level();
			break;
		}
		case SPELL_ALTER_REALITY:
		{
			msg_print("The world changes!");
			p_ptr->leaving = TRUE;
			break;
		}
		case SPELL_SENSE_MONSTERS:
		{
			(void)detect_monsters_normal(TRUE);
			break;
		}
		case SPELL_DETECTION:
		{
			(void)detect_all(TRUE);
			break;
		}
		case SPELL_PERCEPTION:
		{
			return ident_spell();
		}
		case SPELL_PROBING:
		{
			(void)probing();
			break;
		}
		case SPELL_CLAIRVOYANCE:
		{
			wiz_lite();
			break;
		}
		case SPELL_CURE_SER_WOUNDS:
		{
			(void)heal_player(20, 25); 
			(void)clear_timed(TMD_CUT, TRUE); 
			(void)clear_timed(TMD_CONFUSED, TRUE); 
			(void)clear_timed(TMD_BLIND, TRUE);
			break;
		}
		case SPELL_CURE_SER_WOUNDS_II: /* Easier to cast? */
		{
			(void)heal_player(20, 25); 
			(void)clear_timed(TMD_CUT, TRUE); 
			(void)clear_timed(TMD_CONFUSED, TRUE); 
			(void)clear_timed(TMD_BLIND, TRUE);
			break;
		}
		case SPELL_HEALING:
		{
			(void)hp_player(2000);
			(void)clear_timed(TMD_STUN, TRUE);
			(void)clear_timed(TMD_CUT, TRUE);
			break;
		}
		case SPELL_RESTORATION:
		{
			(void)do_res_stat(A_STR);
			(void)do_res_stat(A_INT);
			(void)do_res_stat(A_WIS);
			(void)do_res_stat(A_DEX);
			(void)do_res_stat(A_CON);
			(void)do_res_stat(A_CHR);
			break;
		}
		case SPELL_REMEMBRANCE:
		{
			(void)restore_level();
			break;
		}
		case SPELL_UNBARRING_WAYS:
		{
			(void)destroy_doors_touch();
			break;
		}
		case SPELL_RECHARGING:
		{
			return recharge(15);
		}
		case SPELL_DISPEL_CURSE:
		{
			(void)remove_all_curse();
			break;
		}
		case SPELL_ENCHANT_WEAPON: /*   *ENCHANT WEAPON*   */
		{
			return enchant_spell(100 + randint1(4), 100 + randint1(4), 0); /* TODO Check this works! */
		}
		case SPELL_ENCHANT_ARMOR: /*   *ENCHANT ARMOR*   */
		{
			return enchant_spell(0, 0, 100 + randint1(3) + 1); /* TODO Check this works! */
		}
		case SPELL_ELEMENTAL_BRAND:
		{
			brand_weapon();
			break;
		}
		case SPELL_DISPEL_UNDEAD_II: /* Different to other dispel undead ? */
		{
			(void)dispel_undead(randint1(plev * 4));
			break;
		}
		case SPELL_DISPEL_EVIL_II: /* Different to other dispel evil ? */
		{
			(void)dispel_evil(randint1(plev * 4));
			break;
		}
		case SPELL_BANISHMENT:
		{
			if (banish_evil(100))
			{
				msg_print("The power of your god banishes evil!");
			}
			break;
		}
		case SPELL_HEAVENS_WRATH:
		{
			destroy_area(py, px, 15, TRUE);
			break;
		}
		case SPELL_ANNIHILATION:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			drain_life(dir, 200);
			break;
		}
	}
	/* Success */
	return (TRUE);
}
