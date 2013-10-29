//#include "powers_defines.h"

/*
 * Returns additional info for power
 */
cptr power_info(int power)
{
	static char s[40];
          
	strcpy(s, "");
                  
	switch (rp_ptr->racial_powers[power].type)
	{
		case PWR_ABSORB_MANA:

			sprintf(s, "mana +%d*grids", p_ptr->lev / 2);

			break;
		 
		case PWR_ALTER_REALITY:

			break;
		 
		case PWR_ATTACK_DIST:

			break;
		 
		case PWR_ATTACK_SUPER:

			sprintf(s, "+10,+%d", p_ptr->lev * 2 / 3);

			break;
		 
		case PWR_ATTACK_WHIRL:

			break;
		 
		case PWR_AWAKE_TREE:

			sprintf(s, "rad %d", 1 + p_ptr->lev / 20);

			break;
		 
		case PWR_BASILISK_GAZE:

			break;
		 
		case PWR_BEAM_OF_LIGHT:

			sprintf(s, "dam 6d8");

			break;
		 
		case PWR_BERSERK:

			sprintf(s, "dur 25+d25");

			break;
		 
		case PWR_BLINK:

			sprintf(s, "dist 10");

			break;
		 
		case PWR_BRAIN_SMASH:

			break;
		
		case PWR_BR_ACID_BALL:

			sprintf(s, "%d rad %d", p_ptr->lev * 6, 1 + p_ptr->lev/15);//(s, "%d len %d", p_ptr->lev * 4, 8 + p_ptr->lev/10);

			break;
		
		case PWR_BR_ACID_BOLT:

			sprintf(s, "%dd11", p_ptr->lev * 2/3);

			break;
		
		case PWR_BR_CHAOS_BALL:

			sprintf(s, "%d rad %d", p_ptr->lev * 4, 1 + p_ptr->lev/15);//(s, "%d len %d", p_ptr->lev * 4, 8 + p_ptr->lev/10);

			break;
		 
		case PWR_BR_CHAOS_BOLT:

			sprintf(s, "%dd7", p_ptr->lev * 2/3);

			break;
			
		case PWR_BR_COLD_BALL:

			sprintf(s, "%d rad %d", p_ptr->lev * 6, 1 + p_ptr->lev/15);//(s, "%d len %d", p_ptr->lev * 5, 8 + p_ptr->lev/10);

			break;
		 
		case PWR_BR_COLD_BOLT:

			sprintf(s, "%dd11", p_ptr->lev * 2/3);

			break;
		 
		case PWR_BR_CONFU_BALL:

			sprintf(s, "%d rad %d", p_ptr->lev * 4, 1 + p_ptr->lev/15);//(s, "%d len %d", p_ptr->lev * 3, 8 + p_ptr->lev/10);

			break;
		 
		case PWR_BR_CRYO_BALL:

			sprintf(s, "%d rad %d", p_ptr->lev * 4, 1 + p_ptr->lev/15);//(s, "%d len %d", p_ptr->lev * 2, 8 + p_ptr->lev/10);

			break;
		 
		case PWR_BR_DARK_BALL:

			sprintf(s, "%d rad %d", p_ptr->lev * 3, 1 + p_ptr->lev/15);//(s, "%d len %d", p_ptr->lev * 4, 8 + p_ptr->lev/10);

			break;
		 
		case PWR_BR_DARK_BOLT:

			sprintf(s, "%dd5", p_ptr->lev * 2/3);

			break;
		 
		case PWR_BR_DISEN_BALL:

			sprintf(s, "%d rad %d", p_ptr->lev * 3, 1 + p_ptr->lev/15);//(s, "%d len %d", p_ptr->lev * 3, 8 + p_ptr->lev/10);

			break;
		 
		case PWR_BR_DISEN_BOLT:

			sprintf(s, "%dd5", p_ptr->lev * 2/3);

			break;
		 
		case PWR_BR_ELEC_BALL:

			sprintf(s, "%d rad %d", p_ptr->lev * 6, 1 + p_ptr->lev/15);//(s, "%d len %d", p_ptr->lev * 5, 8 + p_ptr->lev/10);

			break;
		 
		case PWR_BR_ELEC_BOLT:

			sprintf(s, "%dd11", p_ptr->lev * 2/3);

			break;
		 
		case PWR_BR_FEAR_BALL:

			sprintf(s, "pwr %d", p_ptr->lev * 6);

			break;
		 
		case PWR_BR_FIRE_BALL:

			sprintf(s, "%d rad %d", p_ptr->lev * 6, 1 + p_ptr->lev/15);//(s, "%d len %d", p_ptr->lev * 5, 8 + p_ptr->lev/10);

			break;
		 
		case PWR_BR_FIRE_BOLT:

			sprintf(s, "%dd11", p_ptr->lev * 2/3);

			break;
		 
		 
		case PWR_BR_FORCE_BALL:

			sprintf(s, "%d rad %d", p_ptr->lev * 5/2, 1 + p_ptr->lev/15);//(s, "%d len %d", p_ptr->lev * 5, 8 + p_ptr->lev/10);

			break;
		 
		case PWR_BR_FORCE_BOLT:

			sprintf(s, "%dd4", p_ptr->lev * 2/3);

			break;
		 
		case PWR_BR_GRAVITY_BALL:

			sprintf(s, "%d rad %d", p_ptr->lev * 5/2, 1 + p_ptr->lev/15);//(s, "%d len %d", p_ptr->lev * 5, 8 + p_ptr->lev/10);

			break;
		 
		case PWR_BR_GRAVITY_BOLT:

			sprintf(s, "%dd4", p_ptr->lev * 2/3);

			break;
		 
		case PWR_BR_ICE_BALL:

			sprintf(s, "%d rad %d", p_ptr->lev * 4, 1 + p_ptr->lev/15);//(s, "%d len %d", p_ptr->lev * 7, 8 + p_ptr->lev/10);

			break;
			
		case PWR_BR_INERTIA_BALL:

			sprintf(s, "%d rad %d", p_ptr->lev * 3, 1 + p_ptr->lev/15);//(s, "%d len %d", p_ptr->lev * 3, 8 + p_ptr->lev/10);

			break;
		 
		case PWR_BR_INERTIA_BOLT:

			sprintf(s, "%dd5", p_ptr->lev * 2/3);

			break;
			
		case PWR_BR_LITE_BALL:

			sprintf(s, "%d rad %d", p_ptr->lev * 3, 1 + p_ptr->lev/15);//(s, "%d len %d", p_ptr->lev * 3, 8 + p_ptr->lev/10);

			break;
		 
		case PWR_BR_LITE_BOLT:

			sprintf(s, "%dd5", p_ptr->lev * 2/3);

			break;
		 
		case PWR_BR_MANA_BALL:

			sprintf(s, "%d rad %d", p_ptr->lev * 7/2, 1 + p_ptr->lev/15);//(s, "%d len %d", p_ptr->lev * 6, 8 + p_ptr->lev/10);

			break;
		
		case PWR_BR_MANA_BOLT:
		
			sprintf(s, "%dd6", p_ptr->lev * 2/3);
		
			break;
		 
		case PWR_BR_NETHR_BALL:

			sprintf(s, "%d rad %d", p_ptr->lev * 5, 1 + p_ptr->lev/15);//(s, "%d len %d", p_ptr->lev * 5, 8 + p_ptr->lev/10);

			break;
		 
		case PWR_BR_NETHR_BOLT:

			sprintf(s, "%dd9", p_ptr->lev * 2/3);

			break;
		 
		case PWR_BR_NEXUS_BALL:

			sprintf(s, "%d rad %d", p_ptr->lev * 4, 1 + p_ptr->lev/15);//(s, "%d len %d", p_ptr->lev * 5, 8 + p_ptr->lev/10);

			break;

		case PWR_BR_NEXUS_BOLT:
	
			sprintf(s, "%dd7", p_ptr->lev * 2/3);

			break;

		case PWR_BR_PLASMA_BALL:

			sprintf(s, "%d rad %d", p_ptr->lev * 5/2, 1 + p_ptr->lev/15);//(s, "%d len %d", p_ptr->lev * 4, 8 + p_ptr->lev/10);

			break;
		 
		case PWR_BR_PLASMA_BOLT:

			sprintf(s, "%dd4", p_ptr->lev * 2/3);

			break;
		 
		case PWR_BR_POIS_BALL:

			sprintf(s, "%d rad %d", p_ptr->lev * 5, 1 + p_ptr->lev/15);//(s, "%d len %d", p_ptr->lev * 6, 8 + p_ptr->lev/10);

			break;
		 
		case PWR_BR_POIS_BOLT:

			sprintf(s, "%dd9", p_ptr->lev * 2/3);

			break;
		 
		case PWR_BR_SHARD_BALL:

			sprintf(s, "%d rad %d", p_ptr->lev * 7/2, 1 + p_ptr->lev/15);//(s, "%d len %d", p_ptr->lev * 6, 8 + p_ptr->lev/10);

			break;

		case PWR_BR_SHARD_BOLT:

			sprintf(s, "%dd6", p_ptr->lev * 2/3);

			break;
		 		 
		case PWR_BR_SOUND_BALL:

			sprintf(s, "%d rad %d", p_ptr->lev * 5/2, 1 + p_ptr->lev/15);//(s, "%d len %d", p_ptr->lev * 6, 8 + p_ptr->lev/10);

			break;

		case PWR_BR_SOUND_BOLT:

			sprintf(s, "%dd4", p_ptr->lev * 2/3);

			break;
			
		case PWR_BR_TIME_BALL:

			sprintf(s, "%d rad %d", p_ptr->lev * 3, 1 + p_ptr->lev/15);//(s, "%d len %d", p_ptr->lev * 6, 8 + p_ptr->lev/10);

			break;

		case PWR_BR_TIME_BOLT:

			sprintf(s, "%dd5", p_ptr->lev * 2/3);

			break;


		case PWR_BR_WIND_BALL:

			sprintf(s, "%d rad %d", p_ptr->lev * 4, 1 + p_ptr->lev/15);//(s, "%d len %d", p_ptr->lev * 6, 8 + p_ptr->lev/10);

			break;
		 
		case PWR_CALL_LIGHT:

			break;
		 
		case PWR_CAST_ACID_BOLT:

			sprintf(s, "%dd10", (p_ptr->lev < 10 ? 0 : p_ptr->lev - 10) * 2/3 + 4);

			break;
		 
		case PWR_CAST_COLD_BALL:

			sprintf(s, "%d rad %d", p_ptr->lev * 4, (p_ptr->lev) > 35 ? 3 : 2);

			break;
		 
		case PWR_CAST_COLD_BOLT:

			sprintf(s, "%dd8", (p_ptr->lev < 10 ? 0 : p_ptr->lev - 10) * 3/4 + 3);

			break;
		 
		case PWR_CAST_DARK_BALL:

			sprintf(s, "%d rad %d", p_ptr->lev * 3, (p_ptr->lev > 30) ? 3 : 2);

			break;
		 
		case PWR_CAST_DARK_STORM:

			sprintf(s, "%d rad 20", p_ptr->lev * 5 / 2);

			break;
		 
		case PWR_CAST_DISENCHANT_BOLT:

			sprintf(s, "%dd8", (p_ptr->lev < 10 ? 0 : p_ptr->lev - 10) * 3/4 + 3);

			break;
		 
		case PWR_CAST_ELEC_BOLT:

			sprintf(s, "%dd8", (p_ptr->lev < 10 ? 0 : p_ptr->lev - 10) * 3/4 + 3);

			break;
		 
		case PWR_CAST_ELEC_BALL:

			sprintf(s, "%d rad %d", p_ptr->lev * 5, (p_ptr->lev) > 35 ? 3 : 2);

			break;
		 
		case PWR_CAST_ELEC_BEAM:

			sprintf(s, "%dd10", (p_ptr->lev < 20 ? 0 : p_ptr->lev - 20) * 2/3 + 3);

			break;
		 
		case PWR_CAST_FIRE_BALL:

			sprintf(s, "%d rad %d", p_ptr->lev * 4, (p_ptr->lev) > 35 ? 3 : 2);

			break;
		 
		case PWR_CAST_FIRE_BOLT:

			sprintf(s, "%dd8", (p_ptr->lev < 10 ? 0 : p_ptr->lev - 10) * 3/4 + 3);

			break;
		 
		case PWR_CAST_FIRE_JUMP:

			sprintf(s, "dam %d rad 1 red 25%%", p_ptr->lev * 8);

			break;
		 
		case PWR_CAST_ICE_BALL:

			sprintf(s, "%d rad %d", p_ptr->lev * 3, (p_ptr->lev) > 40 ? 4 : 3);

			break;
		 
		case PWR_CAST_ICE_BOLT:

			sprintf(s, "%dd12", (p_ptr->lev < 20 ? 0 : p_ptr->lev - 20) * 2/3 + 3);

			break;
		 
		case PWR_CAST_LITE_ORB:

			sprintf(s, "%d rad 1", p_ptr->lev * 5);

			break;
		 
		case PWR_CAST_MAGIC_MISSILE:

			sprintf(s, "%dd4", 3 + ((p_ptr->lev - 1) / 5));

			break;
		 
		case PWR_CAST_MANA_BOLT_1:

			sprintf(s, "%dd10", (p_ptr->lev < 10 ? 0 : p_ptr->lev - 10) / 5 + 3);

			break;
		 
		case PWR_CAST_MANA_BOLT_2:

			sprintf(s, "%dd14", (p_ptr->lev < 10 ? 0 : p_ptr->lev - 10) / 4 + 3);

			break;
		 
		case PWR_CAST_MANA_STORM:

			sprintf(s, "%d rad 5", p_ptr->lev * 7);

			break;
		 
		case PWR_CAST_NETHER_BALL:

			sprintf(s, "%d rad %d", p_ptr->lev * 9/2, (p_ptr->lev > 30) ? 3 : 2);                        

			break;
		 
		case PWR_CAST_NETHER_BOLT:

			sprintf(s, "%dd8", (p_ptr->lev < 11 ? 1 : p_ptr->lev - 9));

			break;
		 
		case PWR_CAST_NEXUS_BALL:

			sprintf(s, "%d rad %d", p_ptr->lev * 4, (p_ptr->lev) > 40 ? 4 : 3);

			break;
		 
		case PWR_CAST_NEXUS_BURST:

			sprintf(s, "dam %d", p_ptr->lev * 7 / 2);

			break;
		 
		case PWR_CAST_PLASMA_BOLT:

			sprintf(s, "%dd12", (p_ptr->lev < 20 ? 0 : p_ptr->lev - 20) * 2/3 + 3);

			break;
		 
		case PWR_CAST_PLASMA_BALL:

			sprintf(s, "%d rad %d", p_ptr->lev * 6, (p_ptr->lev) > 40 ? 4 : 3);

			break;
		 
		case PWR_CAST_STINKING_CLOUD:

			sprintf(s, "%d rad 2", 10 + (p_ptr->lev / 2));

			break;
		 
		case PWR_CAST_WATER_BALL:

			sprintf(s, "%d rad %d", p_ptr->lev * 7/2, (p_ptr->lev) > 45 ? 3 : 2);

			break;
		 
		case PWR_CAST_WATER_BEAM:

			sprintf(s, "%dd10", (p_ptr->lev < 20 ? 0 : p_ptr->lev - 20) * 2/3 + 3);

			break;
		 
		case PWR_CAST_WIND_BURST:

			sprintf(s, "dam %d", p_ptr->lev * 7 / 2);

			break;
		 
		case PWR_CAST_WIND_ORB:

			sprintf(s, "%d rad 1", p_ptr->lev * 4);

			break;
		 
		case PWR_CONFUSE:

			break;
		 
		case PWR_CURE_CRITICAL:

			sprintf(s, "heal 25%");

			break;
		 
		case PWR_CURE_LIGHT:

			sprintf(s, "heal 15%");

			break;
		 
		case PWR_CURE_SERIOUS:

			sprintf(s, "heal 20%");

			break;
		 
		case PWR_CURSE_BIG:

			sprintf(s, "dam 15d20");

			break;
		 
		case PWR_CURSE_MED:

			sprintf(s, "dam 8d10");

			break;
		 
		case PWR_CURSE_MORTAL:

			sprintf(s, "dam 35d35");

			break;
		 
		case PWR_CURSE_SMALL:

			sprintf(s, "dam 3d5");

			break;
		 
		case PWR_DETECT_ALL:

			break;
		 
		case PWR_DETECT_CHAOS:

			break;
		 
		case PWR_DETECT_EVIL:

			break;
		 
		case PWR_DETECT_LAW:

			break;
		 
		case PWR_DETECT_MONSTERS:

			break;
		 
		case PWR_DIMENSION_DOOR:

			sprintf(s, "rad %d", p_ptr->lev * 2);

			break;
		 
		case PWR_DISPEL_LIFE:

			sprintf(s, "dam %d", p_ptr->lev * 2);

			break;
		 
		case PWR_FORCE_CHARM_BY_GHOST:

			break;
		 
		case PWR_ENCHANT_WEAPON:

			break;
		 
		case PWR_FORCE_PSEUDOID:

			break;
		 
		case PWR_HASTE:

			sprintf(s, "dur %d+d20", p_ptr->lev);

			break;
		 
		case PWR_HEAL:

			sprintf(s, "heal 300");

			break;
		 
		case PWR_HEAL_BONES:

			sprintf(s, "heal corpse lvl x 5");

			break;
		 
		case PWR_HEAL_PET:

			sprintf(s, "heal %d%% or 3d10", p_ptr->lev);

			break;
		 
		case PWR_HEAL_WALL:

			sprintf(s, "heal %d", 100 + p_ptr->lev * 3);

			break;
		 
		case PWR_HEROISM:

			sprintf(s, "dur 25+d25");

			break;
		 
		case PWR_HOLY_ORB_SMALL:

			sprintf(s, "3d6 rad 1");

			break;
		 
		case PWR_IDENTIFY:

			break;
		 
		case PWR_LIGHT_HEAL:

			sprintf(s, "heal %d", ((p_ptr->lev < 20 ? 20 : p_ptr->lev) - 20) * 5 + 100);

			break;
		 
		case PWR_MAKE_ABYSS:

			sprintf(s, "rad %d", 5 + p_ptr->lev / 10);

			break;
		 
		case PWR_MAKE_FIRE:

			sprintf(s, "rad %d", 5 + p_ptr->lev / 10);

			break;
		 
		case PWR_MAKE_GRASS:

			sprintf(s, "rad %d", 5 + p_ptr->lev / 10);

			break;
		 
		case PWR_MAKE_ICE:

			sprintf(s, "rad %d", 5 + p_ptr->lev / 10);

			break;
		 
		case PWR_MAKE_LAVA:

			sprintf(s, "rad %d", 5 + p_ptr->lev / 10);

			break;
		 
		case PWR_MAKE_TREE:

			sprintf(s, "rad %d", 5 + p_ptr->lev / 10);

			break;
		 
		case PWR_MAKE_WALL:

			sprintf(s, "rad %d", 2 + p_ptr->lev / 20);

			break;
		 
		case PWR_MAKE_WATER:

			sprintf(s, "rad %d", 5 + p_ptr->lev / 10);

			break;
		 
		case PWR_OPPOSE_COLD:

			sprintf(s, "dur 30+d30");

			break;
		 
		case PWR_OPPOSE_ELEC:

			sprintf(s, "dur 30+d30");

			break;
		 
		case PWR_OPPOSE_FIRE:

			sprintf(s, "dur 30+d30");

			break;
		 
		case PWR_PARALYZE:

			break;
		 
		case PWR_PROJECT_CHAOS:

			sprintf(s, "dam %d", p_ptr->lev * 2);

			break;
	/*	 
		case PWR_PROJECT_CRYO:

			sprintf(s, "dam %d", p_ptr->lev * 3);

			break;
	*/	 
		case PWR_PROJECT_DISEN:

			sprintf(s, "dam %d", p_ptr->lev * 3 / 2);

			break;

		case PWR_PROJECT_SHARD:

			sprintf(s, "dam %d", p_ptr->lev * 2);

			break;

		case PWR_PROJECT_SOUND:

			sprintf(s, "dam %d", p_ptr->lev * 2);

			break;
	/*	 
		case PWR_REMEMBRANCE:

			break;
		 
		case PWR_RESISTANCE:

			sprintf(s, "dur 20+d20");

			break;
		 
		case PWR_RESTORATION:

			break;
		 
		case PWR_SCARE:

			break;
		 
		case PWR_SCARE_ALL:

			break;
		 
		case PWR_SET_PROJECT_ELEC:

			sprintf(s, "dam %dd8 rad %d dur 25", p_ptr->lev / 5, p_ptr->lev / 5);

			break;
		 
		case PWR_SHIFT_PLANES:

			sprintf(s, "dur 20+d20");

			break;
		 
		case PWR_SHRIEK:

			break;
		 
		case PWR_SLOW:

			break;
		 
		case PWR_SPAWN:

			break;
		 
		case PWR_SPIT_ACID:

			sprintf(s, "%dd5", 3 + p_ptr->lev / 4);

			break;
		 
		case PWR_SPIT_POISON:

			sprintf(s, "%dd5", 3 + p_ptr->lev / 3);

			break;
		 
		case PWR_STAR_HEAL:

			sprintf(s, "heal 1000");

			break;
		 
		case PWR_STEAL:

			break;
		 
		case PWR_SUMM_AINU:

			break;
		 
		case PWR_SUMM_BALROG:

			break;
		 
		case PWR_SUMM_DARK_ELF:

			break;
		 
		case PWR_SUMM_DEMON:

			break;
		 
		case PWR_SUMM_DEMON_SUMM:

			break;
		 
		case PWR_SUMM_DRAGON:

			break;
		 
		case PWR_SUMM_DRAGON_ANCIENT:

			break;
		 
		case PWR_SUMM_DRAGON_MATURE:

			break;
		 
		case PWR_SUMM_DRAGON_SUMM:

			break;
		 
		case PWR_SUMM_ELEMENTAL:

			break;
		 
		case PWR_SUMM_GOLEM:

			break;
		 
		case PWR_SUMM_HI_DEMON:

			break;
		 
		case PWR_SUMM_HI_DRAGON:

			break;
		 
		case PWR_SUMM_HI_UNDEAD:

			break;
		 
		case PWR_SUMM_HYDRA:

			break;
		 
		case PWR_SUMM_LAWFUL:

			break;
		 
		case PWR_SUMM_LICH:

			break;
		 
		case PWR_SUMM_MAGMA_ELEM_WALL:

			break;
		 
		case PWR_SUMM_OGRE:

			break;
		 
		case PWR_SUMM_ORC:

			break;
		 
		case PWR_SUMM_SPIDER:

			break;
		 
		case PWR_SUMM_TROLL:

			break;
		 
		case PWR_SUMM_ULTIMATE:

			break;
		 
		case PWR_SUMM_UNDEAD:

			break;
		 
		case PWR_SUMM_UNDEAD_DRAGON:

			break;
		 
		case PWR_SUMM_UNDEAD_SUMM:

			break;
		 
		case PWR_SUMM_VORTEX:

			break;
		 
		case PWR_SUMM_VROCK:

			break;
		 
		case PWR_SUMM_WIGHT_WRAITH:

			break;
		 
		case PWR_SUMM_YEEK:

			break;
		 
		case PWR_TELEPORT:

			sprintf(s, "dist 100");

			break;
		 
		case PWR_TELEPORT_AWAY:

			break;
		 
		case PWR_WEB_BALL:

			sprintf(s, "rad 2");

			break;
		 
		case PWR_WEB_RAY:

			break;
		 
		case PWR_WONDER:

			break;
		 
		case PWR_WORD_OF_DESTRUCTION:

			break;
		 
		case PWR_WORD_OF_RECALL:

			break;
	*/ 
	}
	return s;
}

