/* File: powers.c */

/*
 * Listing of "powers" that are available to items and spells. 
 *
 * Copyright (c) 1999 Leon Marrick, Ben Harrison, James E. Wilson, hi
 * Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"

/*
 * The descriptions for the powers
 */
info_entry power_info[POW_MAX] = 
{
	{0, NULL},
	{POW_HEAL_1,			"reduces cuts and heals you a little"},
	{POW_HEAL_2,			"reduces cuts and heals you a moderate amount"},
	{POW_HEAL_3,			"reduces cuts and heals you a large amount"},
	{POW_HEAL_4,			"heals you a very large amount, eliminates cuts and stunning"},
	{POW_HEAL_5,			"heals you fully, eliminates cuts and stunning"},	
	{POW_HEAL_CURE_1,		"heals some damage, cures stunning effects and reduces cuts"},	
	{POW_HEAL_CURE_2,		"heals a moderate amount, cures negative effects"},	
	{POW_HEAL_CURE_3,		"heals a large amount, cures negative effects"},	
	{POW_HEAL_CURE_4,		"heals you a very large amount, cures negative effects"},	
	{POW_LIFE,				"restores you to perfect health and condition"},	
	{POW_RESTORE_MANA,		"restores mana to full level"},
	{POW_RESTORE_MANA_INT,	"restores mana to full level, and also restores intelligence"},
	{POW_RESTORE_STR,		"restores your strength"},
	{POW_RESTORE_INT,		"restores your intelligence"},
	{POW_RESTORE_WIS,		"restores your wisdom"},		
	{POW_RESTORE_DEX,		"restores your dexterity"},		
	{POW_RESTORE_CON,		"restores your constitution"},		
	{POW_RESTORE_CHR,		"restores your charisma"},		
	{POW_RESTORE_STATS,		"restores all stats"},
	{POW_RESTORE_LEVEL,		"restores experience level"},
	{POW_RESTORE_ALL,		"restores experience level and all stats"},
	{POW_GAIN_STR,			"permanently raises your strength"},
	{POW_GAIN_INT,			"permanently raises your intelligence"},
	{POW_GAIN_WIS,			"permanently raises your wisdom"},		
	{POW_GAIN_DEX,			"permanently raises your dexterity"},		
	{POW_GAIN_CON,			"permanently raises your constitution"},		
	{POW_GAIN_CHR,			"permanently raises your charisma"},		
	{POW_BRAWN,				"raises your strength at the expense of your intelligence"},
	{POW_INTELLECT,			"raises your intelligence at the expense of your constitution"},
	{POW_CONTEMPLATION,		"raises your wisdom at the expense of your dexterity"},
	{POW_TOUGHNESS,			"raises your constitution at the expense of your charisma"},
	{POW_NIMBLENESS,		"raises your dexterity at the expense of your strength"},
	{POW_PLEASING,			"raises your charisma at the expense of your wisdom"},
	{POW_GAIN_ALL,			"permanently raises all stats"},
	{POW_GAIN_EXP,			"permanently raises your experience level"},
	{POW_CURE_CONFUSION,	"removes any confusion you currently feel"},
	{POW_CURE_DISEASE,		"rids your body of all disease"},
	{POW_CURE_POISON,		"removes all poison from your body"},
	{POW_CURE_POIS_DISE,	"removes all poison and disease from your body"},
	{POW_CURE_FEAR_POIS,	"removes fear from your mind and poison from your body"},
	{POW_CURE_TAINT,		"removes a temporary unholy taint from your soul"},
	{POW_CURE_ALL,			"removes all poison, disease, fear, cuts, stunning and confusion"},
	{POW_CURE_BODY,			"restores all stats, cures poison, and fully feeds you"},
	{POW_CLEAR_MIND,		"rids your mind of confusion and fear, cures blindness"},
	{POW_TELE_10,			"displaces you a short distance away"}, 
	{POW_TELE_MINOR,		"displaces you a medium distance away"},
	{POW_TELE_MAJOR,		"displaces you a major distance away"},
	{POW_TELE_OTHER,		"teleports an opponent away"},
	{POW_TELE_OTHER_BEAM,	"teleports a line of opponents away"},
	{POW_TELE_LEVEL,		"immediately takes you to the next level up or down"},
	{POW_TELE_CONTROL,		"displaces you to somewhere you choose"},
	{POW_WORD_RECALL,		"recalls you to the town, or back into the dungeon"},
	{POW_ALTER_REALITY,		"regenerates the dungeon level"},
	{POW_ARROW,				"fires a magical arrow"},
	{POW_BOLT_MISSILE,		"fires a single, unresistable, bolt of mana"},
	{POW_BOLT_ELEC,			"fires a bolt or beam of lightning"},	
	{POW_BOLT_COLD_1,		"fires a bolt or beam of cold"},
	{POW_BOLT_COLD_2,		"fires a powerful bolt or beam of cold"},
	{POW_BOLT_ACID_1,		"fires a bolt or beam of acid"},
	{POW_BOLT_ACID_2,		"fires a bolt or beam of acid"},
	{POW_BOLT_FIRE_1,		"fires a bolt or beam of fire"},
	{POW_BOLT_FIRE_2,		"fires a bolt or beam of fire"},
	{POW_BOLT_FIRE_3,		"fires a bolt or beam of fire"},
	{POW_BOLT_SOUND,		"fires a bolt or beam of sound"},
	{POW_BOLT_FORCE_1,		"fires a bolt or beam of force"},
	{POW_BOLT_FORCE_2,		"fires a bolt or beam of force"},
	{POW_BOLT_LITE,			"fires a bolt of light"},
	{POW_BOLT_DARK,			"fires a bolt of darkness"},
	{POW_BOLT_WATER,		"fires a bolt of water"},
	{POW_BOLT_MANA,			"fires a bolt or beam of pure mana"},
	{POW_BOLT_ACID_X,		"fires a bolt or beam of acid"},
	{POW_BOLT_ELEC_X,		"fires a bolt or beam of lightning"},
	{POW_BOLT_FIRE_X,		"fires a bolt or beam of fire"},
	{POW_BOLT_COLD_X,		"fires a bolt or beam of frost"},
	{POW_BOLT_MISSILE_X,	"fires a single, unresistable, bolt of mana"},
	{POW_BOLT_POISON_X,		"fires a bolt or beam of poison"},
	{POW_BOLT_MANA_X,		"fires a bolt or beam of pure mana"},
	{POW_BOLT_NEXUS_X,		"fires a bolt or beam of nexus"},
	{POW_BEAM_WEAK_LITE,	"fires a line of light, damaging light-hating creatures"},
	{POW_BEAM_NETHER,		"fires an ultra-powerful beam of nether (does not hurt undead)"},
	{POW_BALL_ACID_X,		"fires a ball of acid"},
	{POW_BALL_ELEC_X,		"fires a ball of lightning"},
	{POW_BALL_FIRE_X,		"fires a ball of fire"},
	{POW_BALL_COLD_X,		"fires a ball of frost"},
	{POW_BALL_POISON_X,		"fires a ball of poison"},
	{POW_BALL_COLD_ELEC_X,	"fires a large ball of frost and lightning"},
	{POW_BALL_FIRE_ACID_X,	"fires a large ball of fire and acid"},
	{POW_BALL_ELEM_X,		"fires a large ball of all four elements"},
	{POW_BALL_ANNIHILATION,	"fires a small, very powerful ball of chaos, nexus, light & dark"},
	{POW_BALL_POISON,		"fires a weak ball of poison"},
	{POW_BALL_ACID,			"fires a ball of acid"},
	{POW_BALL_ELEC_1,		"fires a ball of lightning"},
	{POW_BALL_ELEC_2,		"fires a powrful ball of lightning"},
	{POW_BALL_FIRE_1,		"fires a ball of fire"},
	{POW_BALL_FIRE_2,		"fires a large fire ball"},
	{POW_BALL_FIRE_3,		"fires a large, powerful, fire ball"},
	{POW_BALL_COLD_1,		"fires a ball of frost"},
	{POW_BALL_COLD_2,		"fires a large frost ball"},
	{POW_BALL_COLD_3,		"fires a large, powerful, frost ball"},
	{POW_BALL_SOUND,		"fires a ball of sound"},
	{POW_BALL_PLASMA,		"fires a ball of plasma"},
	{POW_BALL_MANA,			"fires a very powerful mana ball"},
	{POW_BALL_HOLY_1,		"fires a ball of holy force"},
	{POW_BALL_HOLY_2,		"fires a ball of holy force"},
	{POW_STAR_BEAM_W_LITE,	"fires beams of light in all directions"},
	{POW_STAR_BALL_ELEC,	"fires electricity in all directions"},
	{POW_BANISH,			"teleports away all evil monsters in line of sight"},
	{POW_BLIGHT,			"damages all animals and plants in line of sight"},
	{POW_BURST_ASTRAL,		"damages all creatures in line of sight"},
	{POW_DRAIN_LIFE_1,		"drains the life of an enemy"},
	{POW_DRAIN_LIFE_2,		"powerfully drains the life of an enemy"},
	{POW_DRAIN_LIFE_3,		"very powerfully drains the life of an enemy"},
	{POW_DISPEL_ALL,		"dispels all monsters in line of sight"},
	{POW_DISPEL_UNDEAD_1,	"dispels all undead in line of sight"},
	{POW_DISPEL_UNDEAD_2,	"dispels all undead in line of sight"},
	{POW_DISPEL_DEMON,		"dispels all demons in line of sight"},
	{POW_DISPEL_NON_EVIL,	"dispels all non-evil monsters in line of sight"},
	{POW_DISPEL_EVIL_3,		"dispels all evil monsters in line of sight"},
	{POW_DISPEL_EVIL_4,		"dispels all evil monsters in line of sight"},
	{POW_DISPEL_EVIL_5,		"dispels all evil monsters in line of sight"},
	{POW_HOLY_1,			"dispels all evil in line of sight and cures and heals you"},
	{POW_HOLY_2,			"dispels all evil in line of sight and cures and heals you"},
	{POW_GENOCIDE,			"removes all monsters of the symbol you choose from the level"},

	{POW_MASS_GENOCIDE,		"removes nearby monsters except uniques"},
	{POW_EARTHQUAKE,		"destroys the nearby dungeon"},
	{POW_DESTRUCTION,		"destroys objects and monsters, and banishes uniques"},
	{POW_LIGHT_AREA_1,		"permanently lights up the current room or nearby area"},
	{POW_LIGHT_AREA_2,		"permanently lights up the current room or nearby area"},
	{POW_DARK_AREA,			"permanently darkens up the current room or nearby area"},
	{POW_DETECT_MONSTERS,	"detects monsters on the current panel that are not invisible"},
	{POW_DETECT_EVIL,		"detects all evil monsters, even invisible ones"},
	{POW_DETECT_INVIS,		"detects all invisible monsters on the current panel"},
	{POW_DETECT_TRAP,		"detects all traps on the current panel"},
	{POW_DETECT_TREASURE,	"detects all treasure on the current panel"},
	{POW_DETECT_DOOR_STAIR,	"detects all doors and stairs on the current panel"},
	{POW_DETECT_TRAP_DOOR,	"detects hidden traps, stairs and doors on the current screen"},
	{POW_DETECT_ITEM,		"detects all objects on the current panel"},
	{POW_DETECT_ENCHANT,	"detects magical objects on the current panel"},
	{POW_DETECT_ALL,		"detects everything of interest on the panel"},
	{POW_ABSORB_HIT,		"temporarily reverses the effect of damage"},
	{POW_BLESS_1,			"provides a short-term bonus to hit and ac, immunity to taint"},
	{POW_BLESS_2,			"provides a medium-term bonus to hit and ac, immunity to taint"},
	{POW_BLESS_3,			"provides a long-term bonus to hit and ac, immunity to taint"},
	{POW_HEROISM,			"temporarily raises fighting skill and makes you immune to fear"},
	{POW_BOLDNESS,			"temporarily makes you immune to fear"},
	{POW_STABILITY,			"temporarily makes you immune to confusion and stunning"},
	{POW_SAFETY,			"temporarily protects you from dungeon traps"},
	{POW_RAGE_1,			"causes temporary berserk rage"},
	{POW_RAGE_2,			"causes temporary berserk rage"},
	{POW_RAGE_BLESS_RESIST,	"causes temporary berserk rage, blessing, and resistance"},
	{POW_SHIELD,			"temporarily increases your armour class"},
	{POW_INVIS_1,			"temporarily turns you invisible"},
	{POW_INVIS_2,			"temporarily turns you invisible"},
	{POW_RESILIENCE,		"temporarily raises your AC by 50 and reduces all damage by 66%"},
	{POW_INFRAVISION,		"temporarily increases the range of your infravision"},
	{POW_STEALTH,			"temporarily increases your stealth"},
	{POW_SEE_INVIS,			"provides temporary see invisible"},
	{POW_PROT_EVIL_1,		"provides temporary protection from lesser evil creatures"},
	{POW_PROT_EVIL_2,		"provides temporary protection from lesser evil creatures"},
	{POW_HASTE_SELF_1,		"temporarily hastes you"},
	{POW_HASTE_SELF_2,		"hastes you for a long duration"},
	{POW_HASTE_SELF_3,		"hastes you for a very long duration"},
	{POW_DISARM,			"disarms a trap"},
	{POW_DEST_TRAP_DOOR_1,	"destroys a line of traps and doors"},
	{POW_DEST_TRAP_DOOR_2,	"destroys all doors and traps next to you"},
	{POW_STONE_TO_MUD,		"melts a wall square to floor"},
	{POW_CREATE_DOOR,		"creates a barrier of doors around you"},
	{POW_CREATE_WALL,		"creates a barrier of walls around you"},
	{POW_CREATE_STAIR,		"creates a staircase going down nearby"},
	{POW_CREATE_TRAP,		"creates traps around you"},
	{POW_MAGIC_LOCK,		"magically locks all nearby closed doors"},
	{POW_ACQUIRE_1,			"creates a great item"},
	{POW_ACQUIRE_2,			"creates several great items"},
	{POW_AGGRAVATE,			"aggravates nearby monsters"},
	{POW_AGGRAVATE_SAFE,	"aggravates nearby monsters"},
	{POW_CONFUSE_MONSTER,	"attempts to confuse one monster"},
	{POW_CONFUSE_ALL,		"attempts to confuse all monsters in line of sight"},
	{POW_SLEEP_MONSTER,		"attempts to put a monster to sleep"},
	{POW_SLEEP_ADJACENT,	"attempts to put all adjacent monsters to sleep"},
	{POW_SLEEP_ALL,			"attempts to put all monsters in line of sight to sleep"},
	{POW_SLOW_MONSTER,		"attempts to slow a monster down"},
	{POW_SLOW_ALL,			"attempts to slow all monsters in line of sight"},
	{POW_CALM_MONSTER,		"attempts to calm a monster"},
	{POW_CALM_ANIMALS,		"attempts to calm all natural creatures in line of sight"},
	{POW_CALM_NON_EVIL,		"attempts to calm all non-evil creatures in line of sight"},
	{POW_CALM_NON_CHAOS,	"attempts to calm all non-chaotic creatures in line of sight"},
	{POW_CALM_ALL,			"attempts to calm all creatures in line of sight"},
	{POW_BLIND_MONSTER,		"attempts to blind a monster"},
	{POW_SCARE_MONSTER,		"attempts to frighten one monster"},
	{POW_SCARE_ALL,			"attempts to make all monsters in line of sight flee"},
	{POW_CALL_MONSTER,		"attempts to teleport a monster closer to you"},
	{POW_POLY_MONSTER,		"attempts to change a monster"},
	{POW_HEAL_MONSTER,		"attempts to heal a monster"},
	{POW_HASTE_MONSTER,		"attempts to haste a monster"},
	{POW_CLONE_MONSTER,		"attempts to clone a monster"},
	{POW_SATISFY_HUNGER,	"fully feeds you"},
	{POW_RECHARGE_1,		"recharges a staff, wand, rod or talisman"},
	{POW_RECHARGE_2,		"recharges a staff, wand, rod or talisman"},
	{POW_RECHARGE_3,		"powerfully recharges a staff, wand, rod or talisman"},
	{POW_HYPERCHARGE,		"reduces the recharge time for a rod or talisman"},
	{POW_IDENTIFY,			"identifies an object"},
	{POW_IDENTIFY_PACK,		"identifies everything being carried"},
	{POW_IDENTIFY_FULL,		"reveals all information about a specific object"},
	{POW_RES_ACID,			"temporarily protects from acid"},
	{POW_RES_ELEC,			"temporarily protects from electricity"},
	{POW_RES_FIRE,			"temporarily protects from fire"},
	{POW_RES_COLD,			"temporarily protects from cold"},
	{POW_RES_FIRE_COLD,		"temporarily protects from fire & frost"},
	{POW_RES_ACID_ELEC,		"temporarily protects from acid & electricity"},
	{POW_RES_LITE_DARK,		"temporarily protects from light & darkness"},
	{POW_RES_CHAOS_NEXUS,	"temporarily protects from chaos & nexus"},
	{POW_RES_POISON,		"temporarily protects from poison"},
	{POW_RES_DISEASE,		"temporarily protects from disease"},
	{POW_RES_SOUND,			"temporarily protects from sound"},
	{POW_RES_ELEMENTS,		"temporarily protects from all four elements"},
	{POW_RES_GREATER,		"temporarily protects from many things"},
	{POW_RESISTANCE,		"temporarily protects from all elements & poison"},
	{POW_GLYPH_WARDING,		"puts a strong glyph on the floor that monsters cannot pass over"},
	{POW_GLYPH_LESSER,		"puts a glyph on the floor that monsters cannot pass over"},
	{POW_GLYPH_HOLY,		"puts a sigil that blocks undead & demons and prevents summoning"},
	{POW_REMOVE_CURSE_1,	"removes standard curses"},
	{POW_REMOVE_CURSE_2,	"removes both normal and heavy curses"},
	{POW_MAP_1,				"maps the local area"},
	{POW_MAP_2,				"permanently lights and detects objects on the entire level"},
	{POW_MAP_3,				"permanently lights and detects all on the entire level"},
	{POW_PROBE_MONSTER,		"teaches about the attributes of a monster"},
	{POW_PROBE_ALL,			"teaches about the attributes of all visible monsters"},
	{POW_KNOW_ALL,			"reveals the entire map, IDs your pack, and raises your stats"},
	{POW_ENCHANT_WEAPON_1,	"increases a weapon's bonus to hit"},
	{POW_ENCHANT_WEAPON_2,	"powerfully increases a weapon's bonus to hit"},
	{POW_ENCHANT_ARMOR_1,	"increases armour's bonus to armour class"},
	{POW_ENCHANT_ARMOR_2,	"powerfully increases armour's bonus to armour class"},
	{POW_BRAND_WEAPON_ELMNT,"imbues weapons with elemental power"},
	{POW_BRAND_AMMO_ANML,	"makes arrows extra powerful against animals"},
	{POW_BRAND_AMMO_WOUND,	"makes arrows sharper and more powerful"},
	{POW_BRAND_AMMO_ELMNT,	"imbues arrows with elemental power"},
	{POW_BRAND_AMMO_HOLY,	"imbues arrows with holy might"},
	{POW_BIZZARE,			"causes powerful, random, effects"},
	{POW_CURSE_EQUIP_1,		"curses some of your equipment"},
	{POW_CURSE_EQUIP_2,		"curses all of your equipment"},
	{POW_SUM_MONSTER,		"summons monsters to fight against you"},
	{POW_SUM_UNDEAD,		"summons undead creatures to fight against you"},
	{POW_SUM_DRAGON,		"summons dragons to fight against you"},
	{POW_NAUSEA,			"induces vomiting"},
	{POW_POISON_SELF,		"poisons you"},
	{POW_BLIND_SELF,		"blinds you"},
	{POW_CONFUSE_SELF,		"confuses you"},
	{POW_SCARE_SELF,		"scares you"},
	{POW_SLOW_SELF,			"slows you"},
	{POW_PARALYZE,			"paralyzes you"},
	{POW_HALLUCINATE,		"causes you to hallucinate"},
	{POW_DISEASE,			"infects you with a disease"},
	{POW_DEFORM,			"deforms you and swaps your stats"},
	{POW_TAINT,				"places an evil taint on your soul"},
	{POW_AMNESIA,			"makes you forget things"},
	{POW_LOSE_STR,			"lowers your strength"},
	{POW_LOSE_INT,			"lowers your intelligence"},
	{POW_LOSE_WIS,			"lowers your wisdom"},		
	{POW_LOSE_DEX,			"lowers your dexterity"},		
	{POW_LOSE_CON,			"lowers your constitution"},		
	{POW_LOSE_CHR,			"lowers your charisma"},		
	{POW_LOSE_EXP,			"lowers your experience"},
	{POW_RUINATION,			"lowers your stats permanently and damages you"},
	{POW_DETONATE,			"damages you"},
	{POW_KILL_SELF,			"kills you"},
	{POW_SPELL_DURATION,	"provides a bonus to the duration of spells"},
	{POW_SPELL_DAMAGE,		"provides a bonus to the damage inflicted by spells"},
	{POW_SPELL_INFLUENCE,	"makes monster condition spells more effective"},
	{POW_DRAGON_BLACK,		"breathes acid (125+)"},
	{POW_DRAGON_BLUE,		"breathes lightning (125+)"},
	{POW_DRAGON_WHITE,		"breathes frost (125+)"},
	{POW_DRAGON_RED,		"breathes fire (125+)"},
	{POW_DRAGON_GREEN,		"breathes poison gas (150+)"},
	{POW_DRAGON_GOLD,		"breathes sound (100+)"},
	{POW_DRAGON_SILVER,		"breathes shards (100+)"},
	{POW_DRAGON_MH,			"breathes multi-hued (250+)"},
	{POW_DRAGON_SPIRIT,		"breathes force (250+)"},
	{POW_DRAGON_SHADOW,		"breathes nether (250+)"},
	{POW_DRAGON_ETHER,		"breathes light/darkness/confusion (250+)"},
	{POW_DRAGON_CHAOS,		"breathes chaos/disenchant/plasma/sound (350+)"},
	{POW_DRAGON_TIME,		"breathes time/inertia/nexus/nether (350+)"},
	{POW_DRAGON_POWER,		"breathes the elements (400+)"},
	{POW_RISK_HACK,			"either kills or rewards you"},
	{POW_WONDER_HACK,		"creates a random effect"},
	{POW_MUSIC_LYRE,		"Emulate the skillful ballads of a lyre"},
	{POW_MUSIC_HORN,		"Emulate the powerful tones of a horn"},
	{POW_MUSIC_FLUTE,		"Emulate the gentle melodies of a flute"},
	{POW_MUSIC_LUTE,		"Emulate the jolly dances of a lute"},
	{POW_MUSIC_DRUM,		"Emulate the rousing tempos of a drum"},
	{POW_MUSIC_HARP,		"Emulate the soothing songs of a harp"},
	{POW_SHRPOISON,			"poisons"},
	{POW_SHRBLIND,			"blinds"},
	{POW_SHRSCARE,			"scares"},
	{POW_SHRCONFUSE,		"confuses"},
	{POW_SHRHALLUCINATE,		"makes you see unseen things"},
	{POW_SHRPARALYZE,		"causes paralysis"},
	{POW_SHRNAIVITY,		"causes wondrous daydreams"},
	{POW_SHRSTUPIDITY,		"clouds your intelligence with empty rage"},
	{POW_SHRAMNESIA,		"clouds the consciousness"},
	{POW_SHRDISEASE,		"fills your mouth with disease"},
	{POW_SHRCURE_POISON,		"removes all poison from your body"},
	{POW_SHRCURE_DISEASE,		"rids your body of all disease"},
	{POW_SHRCURE_CONFUSION,		"removes any confusion you currently feel"},
	{POW_SHRHEAL_1,			"reduces cuts and heals you a little"},
	{POW_SHRHEAL_2,			"reduces cuts and heals you a moderate amount"},
	{POW_SHRSHIELD,			"temporarily increases your armour class"},
	{POW_SHRCLEAR_MIND,		"rids your mind of confusion and fear, cures blindness"},
	{POW_SHRRESTORE_STR,		"restores your strength"},
	{POW_SHRRESTORE_CON,		"restores your constitution"},
	{POW_SHRRESTORE_DEX,		"restores your dexterity"},
	{POW_SHRRESTORE_STATS,		"restores all stats"},
	{POW_PHLOGISTON,		"refuels your light source"}
};

/*
 * Calculate a spell's duration
 */
int apply_sp_mod(int value, int modifier)
{
	/* No modifier, do nothing */
	if (!modifier) return (value);

	/* Calculate duration bonus */
	return ((value * (modifier + 10)) + 9) / 10;
}

/*
 * Hack -- activate the ring of power
 */
static void ring_of_power(int dir)
{
	/* Pick a random effect */
	switch (randint(10))
	{
		case 1:
		case 2:
		{
			/* Message */
			message(MSG_EFFECT, 0, "You are surrounded by a malignant aura.");

			/* Decrease all stats (permanently) */
			(void)do_dec_stat(A_STR, 5, TRUE, FALSE);
			(void)do_dec_stat(A_INT, 5, TRUE, FALSE);
			(void)do_dec_stat(A_WIS, 5, TRUE, FALSE);
			(void)do_dec_stat(A_DEX, 5, TRUE, FALSE);
			(void)do_dec_stat(A_CON, 5, TRUE, FALSE);
			(void)do_dec_stat(A_CHR, 5, TRUE, FALSE);

			/* Lose some experience (permanently) */
			p_ptr->exp -= (p_ptr->exp / 4);
			p_ptr->max_exp -= (p_ptr->exp / 4);
			check_experience();

			break;
		}

		case 3:
		{
			/* Message */
			message(MSG_EFFECT, 0, "You are surrounded by a powerful aura.");

			/* Dispel monsters */
			project_los(GF_DISP_ALL, 1000);

			break;
		}

		case 4:
		case 5:
		case 6:
		{
			/* Mana Ball */
			fire_ball(GF_MANA, dir, 300, 3);

			break;
		}

		case 7:
		case 8:
		case 9:
		case 10:
		{
			/* Mana Bolt */
			fire_bolt(GF_MANA, dir, 250);

			break;
		}
	}
}

/* 
 * Spell damage calculation macro for use in do_power
 */
#define calc_damage(DD, DS, B) \
	damroll((DD), apply_sp_mod((DS), mdam)) + apply_sp_mod((B), mdam)

 /*
 * Actually use a power
 * beam is the base chance of beaming, dlev is the base level of damage, llev is the base
 * duration level, ilev is the base level for influence spells. obvious determines if the 
 * effect of the spell can be seen by the player.
 */
bool do_power(int idx, int sub, int dir, int beam, int dlev, int llev, int ilev, bool mods, bool *obvious)
{
	int i;
	int durat;
	int mdur, mdam;
	bool holy = ((cp_ptr->flags & CF_BLESS_WEAPON) ? TRUE : FALSE);

	if (p_ptr->taint) holy = FALSE;

	/* Assign modifiers, if allowed */
	if (mods)
	{
		mdur = p_ptr->sp_dur;
		mdam = p_ptr->sp_dam;
		ilev = apply_sp_mod(ilev, p_ptr->sp_inf);
	}
	else 
	{
		mdur = 0;
		mdam = 0;
	}

	/* We haven't seen anything yet */
	*obvious = FALSE;

	switch (idx)
	{
		case POW_HEAL_1:
		{
			if (heal_player(5, 10)) *obvious = TRUE;
			if (set_cut(p_ptr->cut - 10)) *obvious = TRUE;
			break;
		}
		case POW_HEAL_2:
		{
			if (heal_player(15, 20)) *obvious = TRUE;
			if (set_cut((p_ptr->cut / 2) - 35)) *obvious = TRUE;
			break;
		}
		case POW_HEAL_3:
		{
			if (heal_player(30, 30)) *obvious = TRUE;
			if (set_cut(0)) *obvious = TRUE;
			break;
		}
		case POW_HEAL_4:
		{
			if (heal_player(60, 40)) *obvious = TRUE;
			if (set_stun(0)) *obvious = TRUE;
			if (set_cut(0)) *obvious = TRUE;
			break;
		}
		case POW_HEAL_5:
		{
			if (heal_player(90, 100)) *obvious = TRUE;
			if (set_stun(0)) *obvious = TRUE;
			if (set_cut(0)) *obvious = TRUE;
			break;
		}
		case POW_HEAL_CURE_1:
		{
			if (heal_player(15, 20)) *obvious = TRUE;
			if (set_cut((p_ptr->cut / 2) - 35)) *obvious = TRUE;
			if (set_stun(0)) *obvious = TRUE;
			break;
		}
		case POW_HEAL_CURE_2:
		{
			if (heal_player(25, 30)) *obvious = TRUE;
			if (set_blind(0)) *obvious = TRUE;
			if (set_confused(0)) *obvious = TRUE;
			if (set_stun(0)) *obvious = TRUE;
			if (set_cut(0)) *obvious = TRUE;
			break;
		}
		case POW_HEAL_CURE_3:
		{
			if (heal_player(60, 40)) *obvious = TRUE;
			if (set_blind(0)) *obvious = TRUE;
			if (set_confused(0)) *obvious = TRUE;
			if (set_poisoned(0)) *obvious = TRUE;
			if (set_diseased(0)) *obvious = TRUE;
			if (set_stun(0)) *obvious = TRUE;
			if (set_cut(0)) *obvious = TRUE;
			break;
		}
		case POW_HEAL_CURE_4:
		{
			if (heal_player(90, 100)) *obvious = TRUE;
			if (set_stun(0)) *obvious = TRUE;
			if (set_cut(0)) *obvious = TRUE;
			if (set_blind(0)) *obvious = TRUE;
			if (set_confused(0)) *obvious = TRUE;
			if (set_poisoned(0)) *obvious = TRUE;
			if (set_diseased(0)) *obvious = TRUE;
			break;
		}
		case POW_LIFE:
		{
			message(MSG_EFFECT, 0, "You feel life flow through your body!");
			restore_exp();
			(void)set_poisoned(0);
			(void)set_blind(0);
			(void)set_confused(0);
			(void)set_diseased(0);
			(void)set_image(0);
			(void)set_stun(0);
			(void)set_cut(0);
			(void)do_res_stat(A_STR);
			(void)do_res_stat(A_CON);
			(void)do_res_stat(A_DEX);
			(void)do_res_stat(A_WIS);
			(void)do_res_stat(A_INT);
			(void)do_res_stat(A_CHR);

			/* Recalculate max. hitpoints */
			update_stuff();

			heal_player(100, 250);
			*obvious = TRUE;
			break;
		}
		case POW_RESTORE_MANA:
		{
			if (p_ptr->csp < p_ptr->msp)
			{
				p_ptr->csp = p_ptr->msp;
				p_ptr->csp_frac = 0;
				message(MSG_EFFECT, 0, "Your feel your head clear.");
				*obvious = TRUE;
				p_ptr->redraw |= (PR_MANA);
				p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
			}
			break;
		}
		case POW_RESTORE_MANA_INT:
		{
			if (do_res_stat(A_INT)) *obvious = TRUE;
			if (p_ptr->csp < p_ptr->msp)
			{
				p_ptr->csp = p_ptr->msp;
				p_ptr->csp_frac = 0;
				message(MSG_EFFECT, 0, "Your feel your head clear.");
				*obvious = TRUE;
				p_ptr->redraw |= (PR_MANA);
				p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
			}
			break;
		}
		case POW_RESTORE_STR:
		{
			if (do_res_stat(A_STR)) *obvious = TRUE;
			break;
		}
		case POW_RESTORE_INT:
		{
			if (do_res_stat(A_INT)) *obvious = TRUE;
			break;
		}
		case POW_RESTORE_WIS:
		{
			if (do_res_stat(A_WIS)) *obvious = TRUE;
			break;
		}
		case POW_RESTORE_DEX:
		{
			if (do_res_stat(A_DEX)) *obvious = TRUE;
			break;
		}
		case POW_RESTORE_CON:
		{
			if (do_res_stat(A_CON)) *obvious = TRUE;
			break;
		}
		case POW_RESTORE_CHR:
		{
			if (do_res_stat(A_CHR)) *obvious = TRUE;
			break;
		}
		case POW_RESTORE_STATS:
		{
			if (do_res_stat(A_STR)) *obvious = TRUE;
			if (do_res_stat(A_INT)) *obvious = TRUE;
			if (do_res_stat(A_WIS)) *obvious = TRUE;
			if (do_res_stat(A_DEX)) *obvious = TRUE;
			if (do_res_stat(A_CON)) *obvious = TRUE;
			if (do_res_stat(A_CHR)) *obvious = TRUE;
			break;
		}
		case POW_RESTORE_LEVEL:
		{
			if (restore_exp()) *obvious = TRUE;
			break;
		}
		case POW_RESTORE_ALL:
		{
			if (do_res_stat(A_STR)) *obvious = TRUE;
			if (do_res_stat(A_INT)) *obvious = TRUE;
			if (do_res_stat(A_WIS)) *obvious = TRUE;
			if (do_res_stat(A_DEX)) *obvious = TRUE;
			if (do_res_stat(A_CON)) *obvious = TRUE;
			if (do_res_stat(A_CHR)) *obvious = TRUE;
			if (restore_exp()) *obvious = TRUE;
			break;
		}
		case POW_BRAWN:
		{
			if (do_inc_stat(A_STR)) *obvious = TRUE;
			if (do_dec_stat(A_INT, 1, TRUE, FALSE)) *obvious = TRUE;
			break;
		}
		case POW_INTELLECT:
		{
			if (do_inc_stat(A_INT)) *obvious = TRUE;
			if (do_dec_stat(A_CON, 1, TRUE, FALSE)) *obvious = TRUE;
			break;
		}
		case POW_CONTEMPLATION:
		{
			if (do_inc_stat(A_WIS)) *obvious = TRUE;
			if (do_dec_stat(A_DEX, 1, TRUE, FALSE)) *obvious = TRUE;
			break;
		}
		case POW_NIMBLENESS:
		{
			if (do_inc_stat(A_DEX)) *obvious = TRUE;
			if (do_dec_stat(A_STR, 1, TRUE, FALSE)) *obvious = TRUE;
			break;
		}
		case POW_TOUGHNESS:
		{
			if (do_inc_stat(A_CON)) *obvious = TRUE;
			if (do_dec_stat(A_CHR, 1, TRUE, FALSE)) *obvious = TRUE;
			break;
		}
		case POW_PLEASING:
		{
			if (do_inc_stat(A_CHR)) *obvious = TRUE;
			if (do_dec_stat(A_WIS, 1, TRUE, FALSE)) *obvious = TRUE;
			break;
		}
		case POW_GAIN_STR:
		{
			if (do_inc_stat(A_STR)) *obvious = TRUE;
			break;
		}
		case POW_GAIN_INT:
		{
			if (do_inc_stat(A_INT)) *obvious = TRUE;
			break;
		}
		case POW_GAIN_WIS:
		{
			if (do_inc_stat(A_WIS)) *obvious = TRUE;
			break;
		}
		case POW_GAIN_DEX:
		{
			if (do_inc_stat(A_DEX)) *obvious = TRUE;
			break;
		}
		case POW_GAIN_CON:
		{
			if (do_inc_stat(A_CON)) *obvious = TRUE;
			break;
		}
		case POW_GAIN_CHR:
		{
			if (do_inc_stat(A_CHR)) *obvious = TRUE;
			break;
		}
		case POW_GAIN_ALL:
		{
			if (do_inc_stat(A_STR)) *obvious = TRUE;
			if (do_inc_stat(A_INT)) *obvious = TRUE;
			if (do_inc_stat(A_WIS)) *obvious = TRUE;
			if (do_inc_stat(A_DEX)) *obvious = TRUE;
			if (do_inc_stat(A_CON)) *obvious = TRUE;
			if (do_inc_stat(A_CHR)) *obvious = TRUE;
			break;
		}
		case POW_GAIN_EXP:
		{
			if (p_ptr->exp < PY_MAX_EXP)
			{
				s32b ee = (p_ptr->exp / 2) + 10;
				if (ee > 100000L) ee = 100000L;
				message(MSG_EFFECT, 0, "You feel more experienced.");
				gain_exp(ee);
				*obvious = TRUE;
			}
			break;
		}
		case POW_CURE_CONFUSION:
		{
			if (set_confused(0)) *obvious = TRUE;
			break;
		}
		case POW_CURE_DISEASE:
		{
			if (set_diseased(0)) *obvious = TRUE;
			break;
		}
		case POW_CURE_POISON:
		{
			if (set_poisoned(0)) *obvious = TRUE;
			break;
		}
		case POW_CURE_POIS_DISE:
		{
			if (set_poisoned(0)) *obvious = TRUE;
			if (set_diseased(0)) *obvious = TRUE;
			break;
		}
		 case POW_CURE_FEAR_POIS:
		{
			if (set_afraid(0)) *obvious = TRUE;
			if (set_poisoned(0)) *obvious = TRUE;
			break;
		}
		case POW_CURE_TAINT:
		{
			if ((p_ptr->taint_inv) || (p_ptr->taint > dlev * 25))
			{
				message(MSG_FAIL, 0, "The taint on your soul is too powerful at the moment!");
				*obvious = TRUE;
			}
			else if (set_taint(0)) *obvious = TRUE;
			break;
		}
		case POW_CURE_ALL:
		{
			if (set_blind(0)) *obvious = TRUE;
			if (set_poisoned(0)) *obvious = TRUE;
			if (set_diseased(0)) *obvious = TRUE;
			if (set_confused(0)) *obvious = TRUE;
			if (set_stun(0)) *obvious = TRUE;
			if (set_cut(0)) *obvious = TRUE;
			break;
		}
		case POW_CURE_BODY:
		{
			if (set_poisoned(0)) *obvious = TRUE;
			if (set_food(PY_FOOD_MAX - 1)) *obvious = TRUE;
			if (do_res_stat(A_STR)) *obvious = TRUE;
			if (do_res_stat(A_INT)) *obvious = TRUE;
			if (do_res_stat(A_WIS)) *obvious = TRUE;
			if (do_res_stat(A_DEX)) *obvious = TRUE;
			if (do_res_stat(A_CON)) *obvious = TRUE;
			if (do_res_stat(A_CHR)) *obvious = TRUE;
			break;
		}
		case POW_CLEAR_MIND:
		{
			if (set_stun(0)) *obvious = TRUE;
			if (set_blind(0)) *obvious = TRUE;
			if (set_afraid(0)) *obvious = TRUE;
			if (set_confused(0)) *obvious = TRUE;
			break;
		}
		case POW_TELE_10: 
		{
			teleport_player(10);
			*obvious = TRUE;
			break;
		}
		case POW_TELE_MINOR:
		{
			teleport_player(dlev * 3);
			*obvious = TRUE;
			break;
		}
		case POW_TELE_MAJOR:
		{
			teleport_player(dlev * 5);
			*obvious = TRUE;
			break;
		}
		case POW_TELE_OTHER:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			if (fire_ball(GF_AWAY_ALL, dir, MAX_SIGHT * 5, 0)) *obvious = TRUE;
			break;
		}
		case POW_TELE_OTHER_BEAM:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			if (fire_beam(GF_AWAY_ALL, dir, MAX_SIGHT * 5)) *obvious = TRUE;
			break;
		}
		case POW_TELE_LEVEL:
		{
			teleport_player_level();
			*obvious = TRUE;
			break;
		}
		case POW_TELE_CONTROL:
		{
			message(MSG_GENERIC, 0, "Choose a location to teleport to.");
			message_flush();
			dimen_door(20, 100 / dlev);
			*obvious = TRUE;
			break;
		}
		case POW_WORD_RECALL:
		{
			set_recall();
			*obvious = TRUE;
			break;
		}
		case POW_ALTER_REALITY:
		{
			if ((quest_check(p_ptr->max_depth) == QUEST_FIXED) || (quest_check(p_ptr->max_depth) == QUEST_FIXED_U))
			{
				message(MSG_FAIL, 0, "Nothing happens.");
			}
			else
			{
				message(MSG_EFFECT, 0, "The world changes!");
				p_ptr->min_depth++;
				if (p_ptr->min_depth >= p_ptr->depth) p_ptr->depth = p_ptr->min_depth;

				/* Leaving */
				p_ptr->leaving = TRUE;
			}

			*obvious = TRUE;
			break;
		}
		case POW_BOLT_ACID_X:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);

			(void)fire_bolt_or_beam(beam, GF_ACID, dir, 
				calc_damage(sub_spell_list[sub].dd, 
				sub_spell_list[sub].ds + (dlev / sub_spell_list[sub].lev_inc), 0));
			*obvious = TRUE;
			break;
		}
		case POW_BOLT_ELEC_X:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);

			(void)fire_bolt_or_beam(beam, GF_ELEC, dir, 
				calc_damage(sub_spell_list[sub].dd, 
				sub_spell_list[sub].ds + (dlev / sub_spell_list[sub].lev_inc), 0));
			*obvious = TRUE;
			break;
		}
		case POW_BOLT_FIRE_X:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);

			(void)fire_bolt_or_beam(beam, GF_FIRE, dir, 
				calc_damage(sub_spell_list[sub].dd, 
				sub_spell_list[sub].ds + (dlev / sub_spell_list[sub].lev_inc), 0));
			*obvious = TRUE;
			break;
		}
		case POW_BOLT_COLD_X:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);

			(void)fire_bolt_or_beam(beam, GF_COLD, dir, 
				calc_damage(sub_spell_list[sub].dd, 
				sub_spell_list[sub].ds + (dlev / sub_spell_list[sub].lev_inc), 0));
			*obvious = TRUE;
			break;
		}
		case POW_BOLT_MISSILE_X:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);

			(void)fire_bolt(GF_MISSILE, dir, 
				calc_damage(sub_spell_list[sub].dd, 
				sub_spell_list[sub].ds + (dlev / sub_spell_list[sub].lev_inc), 0));
			*obvious = TRUE;
			break;
		}
		case POW_BOLT_POISON_X:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);

			(void)fire_bolt_or_beam(beam, GF_POIS, dir,
				calc_damage(sub_spell_list[sub].dd, 
				sub_spell_list[sub].ds + (dlev / sub_spell_list[sub].lev_inc), 0));
			*obvious = TRUE;
			break;
		}
		case POW_BOLT_MANA_X:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);

			(void)fire_bolt_or_beam(beam, GF_MANA, dir,
				calc_damage(sub_spell_list[sub].dd, 
				sub_spell_list[sub].ds + (dlev / sub_spell_list[sub].lev_inc), 0));
			*obvious = TRUE;
			break;
		}
		case POW_BOLT_NEXUS_X:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);

			(void)fire_bolt_or_beam(beam, GF_NEXUS, dir,
				calc_damage(sub_spell_list[sub].dd, 
				sub_spell_list[sub].ds + (dlev / sub_spell_list[sub].lev_inc), 0));
			*obvious = TRUE;
			break;
		}
		case POW_BALL_ACID_X:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			(void)fire_ball(GF_ACID, dir, 
				calc_damage(0, 0, sub_spell_list[sub].bonus), sub_spell_list[sub].radius);
			*obvious = TRUE;

			break;
		}
		case POW_BALL_ELEC_X:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			(void)fire_ball(GF_ELEC, dir, 
				calc_damage(0, 0, sub_spell_list[sub].bonus), sub_spell_list[sub].radius);
			*obvious = TRUE;
			break;
		}
		case POW_BALL_FIRE_X:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			(void)fire_ball(GF_FIRE, dir, 
				calc_damage(0, 0, sub_spell_list[sub].bonus), sub_spell_list[sub].radius);
			*obvious = TRUE;
			break;
		}
		case POW_BALL_COLD_X:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			(void)fire_ball(GF_COLD, dir, 
				calc_damage(0, 0, sub_spell_list[sub].bonus), sub_spell_list[sub].radius);
			*obvious = TRUE;
			break;
		}
		case POW_BALL_POISON_X:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			(void)fire_ball(GF_POIS, dir, 
				calc_damage(0, 0, sub_spell_list[sub].bonus), sub_spell_list[sub].radius);
			*obvious = TRUE;
			break;
		}
		case POW_BALL_COLD_ELEC_X:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			(void)fire_ball_combo(GF_COLD, GF_ELEC, 0, 0, dir,
				calc_damage(0, 0, sub_spell_list[sub].bonus / 2), 
				sub_spell_list[sub].radius);
			*obvious = TRUE;
			break;
		}
		case POW_BALL_FIRE_ACID_X:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			(void)fire_ball_combo(GF_FIRE, GF_ACID, 0, 0, dir, 
				calc_damage(0, 0, sub_spell_list[sub].bonus / 2), 
				sub_spell_list[sub].radius);
			*obvious = TRUE;
			break;
		}
		case POW_BALL_ELEM_X:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			(void)fire_ball_combo(GF_COLD, GF_ELEC, GF_FIRE, GF_ACID, dir, 
				calc_damage(0, 0, sub_spell_list[sub].bonus / 4), 
				sub_spell_list[sub].radius);
			*obvious = TRUE;
			break;
		}
		case POW_ARROW:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			(void)fire_bolt(GF_ARROW, dir, calc_damage(0, 0, 125));
			*obvious = TRUE;
			break;
		}
		case POW_BOLT_MISSILE:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			(void)fire_bolt(GF_MISSILE, dir, calc_damage(2, 6, 0));
			*obvious = TRUE;
			break;
		}
		case POW_BOLT_ELEC:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			(void)fire_bolt_or_beam(beam - 10, GF_ELEC, dir,
				calc_damage(3 + ((dlev - 5) / 4), 6, 0));
			*obvious = TRUE;
			break;
		}
		case POW_BOLT_COLD_1:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			(void)fire_bolt_or_beam(beam - 10, GF_COLD, dir,
				calc_damage(5 + ((dlev - 5) / 4), 6, 0));
			*obvious = TRUE;
			break;
		}
		case POW_BOLT_COLD_2:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			(void)fire_bolt_or_beam(beam - 10, GF_COLD, dir,
				calc_damage(10 + ((dlev - 5) / 4), 6, 0));
			*obvious = TRUE;
			break;
		}
		case POW_BOLT_ACID_1:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			(void)fire_bolt_or_beam(beam, GF_ACID, dir,
				calc_damage(3 + (dlev / 5), 6, 0));
			*obvious = TRUE;
			break;
		}
		case POW_BOLT_ACID_2:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			(void)fire_bolt_or_beam(beam, GF_ACID, dir,
				calc_damage(3 + ((dlev - 5) / 4), 7, 0));
			*obvious = TRUE;
			break;
		}
		case POW_BOLT_FIRE_1:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			(void)fire_bolt_or_beam(beam, GF_FIRE, dir,	calc_damage(10, 6, 0));
			*obvious = TRUE;
			break;
		}
		case POW_BOLT_FIRE_2:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			(void)fire_bolt_or_beam(beam, GF_FIRE, dir,
				calc_damage(5 + (dlev / 5), 6, 0));
			*obvious = TRUE;
			break;
		}
		case POW_BOLT_FIRE_3:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			(void)fire_bolt_or_beam(beam, GF_FIRE, dir,
				calc_damage(7 + ((dlev - 5) / 3), 7, 0));
			*obvious = TRUE;
			break;
		}
		case POW_BOLT_SOUND:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			(void)fire_bolt_or_beam(beam - 10, GF_SOUND, dir,
				calc_damage(3 + ((dlev - 1) / 5), 3, 0));
			*obvious = TRUE;
			break;
		}
		case POW_BOLT_FORCE_1:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			(void)fire_bolt_or_beam(beam, GF_FORCE, dir,
				calc_damage(2 + ((dlev - 5) / 4), 6, 0));
			*obvious = TRUE;
			break;
		}
		case POW_BOLT_FORCE_2:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			(void)fire_bolt_or_beam(beam, GF_FORCE, dir,
				calc_damage(3 + (dlev / 5), 8, 0));
			*obvious = TRUE;
			break;
		}
		case POW_BOLT_LITE:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			(void)fire_bolt(GF_LITE, dir, calc_damage(4, 7, 0 ));
			*obvious = TRUE;
			break;
		}
		case POW_BOLT_DARK:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			(void)fire_bolt(GF_DARK, dir, calc_damage(4, 7, 0 ));
			*obvious = TRUE;
			break;
		}
		case POW_BOLT_WATER:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			(void)fire_bolt(GF_WATER, dir, calc_damage(5, 6, 0 ));
			*obvious = TRUE;
			break;
		}
		case POW_BOLT_MANA:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			(void)fire_bolt_or_beam(beam, GF_MANA, dir,	
 				calc_damage(6 + ((dlev - 5) / 4), 6, 0));
			*obvious = TRUE;
			break;
		}
		case POW_BEAM_WEAK_LITE:
		{ 
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			message(MSG_EFFECT, 0, "A line of blue shimmering light appears.");
			lite_line(dir, calc_damage(9, 8, 0));
			*obvious = TRUE;
			break;
		}
		case POW_BEAM_NETHER:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			(void)set_taint(p_ptr->taint + 3000);
			(void)fire_beam(GF_NETHER, dir,	calc_damage(8 * dlev, 4, 0));
			*obvious = TRUE;
			break;
		}
		case POW_BALL_POISON:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			(void)fire_ball(GF_POIS, dir, calc_damage(0, 0, 15), 2);
			*obvious = TRUE;
			break;
		}
		case POW_BALL_ACID:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			(void)fire_ball(GF_ACID, dir, calc_damage(0, 0, 35 + (dlev * 2)), 2);
			*obvious = TRUE;
			break;
		}
		case POW_BALL_ELEC_1:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			(void)fire_ball(GF_ELEC, dir, calc_damage(0, 0, 35 + (dlev * 2)), 2);
			*obvious = TRUE;
			break;
		}
		case POW_BALL_ELEC_2:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			(void)fire_ball(GF_ELEC, dir, calc_damage(0, 0, 210), 3);
			*obvious = TRUE;
			break;
		}
		case POW_BALL_FIRE_1:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			(void)fire_ball(GF_FIRE, dir, calc_damage(0, 0, 80), 2);;
			*obvious = TRUE;
			break;
		}
		case POW_BALL_FIRE_2:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			(void)fire_ball(GF_FIRE, dir, calc_damage(0, 0, 50 + (dlev)), (dlev < 40) ? 2 : 3);
			*obvious = TRUE;
			break;
		}
		case POW_BALL_FIRE_3:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			(void)fire_ball(GF_FIRE, dir, calc_damage(0, 0, 90 + (dlev * 2)), (dlev < 40) ? 3 : 4);
			*obvious = TRUE;
			break;
		}
		case POW_BALL_COLD_1:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			(void)fire_ball(GF_COLD, dir, calc_damage(0, 0, 30 + (dlev)), (dlev < 35) ? 2 : 3);
			*obvious = TRUE;
			break;
		}
		case POW_BALL_COLD_2:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			(void)fire_ball(GF_COLD, dir, calc_damage(0, 0, 60 + (dlev * 2)), (dlev < 35) ? 3 : 4);
			*obvious = TRUE;
			break;
		}
		case POW_BALL_COLD_3:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			(void)fire_ball(GF_COLD, dir, calc_damage(0, 0, 180), 4);
			*obvious = TRUE;
			break;
		}
		case POW_BALL_SOUND:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			(void)fire_ball(GF_SOUND, dir, calc_damage(0, 0, 25 + (dlev)), 2);
			*obvious = TRUE;
			break;
		}
		case POW_BALL_PLASMA:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			(void)fire_ball(GF_PLASMA, dir, calc_damage(4, 10, 140), 3);
			*obvious = TRUE;
			break;
		}
		case POW_BALL_MANA:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			(void)fire_ball(GF_MANA, dir, calc_damage(0, 0, 270 + (dlev * 2)), 3);
			*obvious = TRUE;
			break;
		}
		case POW_BALL_ANNIHILATION:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			/* 
			 * Note the ordering of attacks means that the squares will end up dark,
			 * and the monsters won't be teleported until the end 
			 */
			   
			(void)fire_ball_combo(GF_CHAOS, GF_LITE, GF_DARK, GF_NEXUS, dir, 
				calc_damage(0, 0, 200), 1);
			*obvious = TRUE;
			break;
		}
		case POW_BALL_HOLY_1:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);

			fire_ball(GF_HOLY_ORB, dir, calc_damage(3, 6, 40), 3);
			*obvious = TRUE;
			break;
		}
		case POW_BALL_HOLY_2:
		{
			int x = (p_ptr->lev + (p_ptr->lev / ((holy) ? 3 : 5)));
			int y = (((p_ptr->lev >= 30) && (holy)) ? 3 : 2);

			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			(void)fire_ball(GF_HOLY_ORB, dir, calc_damage(3, 6, x), y); 
			*obvious = TRUE;
			break;
		}
		case POW_STAR_BEAM_W_LITE:
		{
			if (!p_ptr->blind)
			{
				message(MSG_EFFECT, 0, "Light bursts out in all directions...");
				*obvious = TRUE;
			}
			for (i = 0; i < 8; i++) lite_line(ddd[i], calc_damage(6, 8, 0));
			break;
		}
		case POW_STAR_BALL_ELEC:
		{
			for (i = 0; i < 8; i++) fire_ball(GF_ELEC, ddd[i], calc_damage(0, 0, 140), 3);
			*obvious = TRUE;
			break;
		}
		case POW_BANISH:
		{
			if (project_los(GF_AWAY_EVIL, 100))
			{
				message(MSG_EFFECT, 0, "The power of your god banishes evil!");
				*obvious = TRUE;
			}
			break;
		}
		case POW_DRAIN_LIFE_1:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			fire_bolt(GF_DRAIN_ALL, dir, calc_damage(0, 0, 50 + dlev));
			*obvious = TRUE;
			break;
		}
		case POW_DRAIN_LIFE_2:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			fire_bolt(GF_DRAIN_ALL, dir, calc_damage(0, 0, 110));
			*obvious = TRUE;
			break;
		}
		case POW_DRAIN_LIFE_3:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			fire_bolt(GF_DRAIN_ALL, dir, calc_damage(0, 0, 180));
			*obvious = TRUE;
			break;
		}
		case POW_BLIGHT:
		{
			if (project_los(GF_DISP_PLANT, calc_damage(0, 0, dlev * 6))) *obvious = TRUE;
			if (project_los(GF_DISP_ANIMAL, calc_damage(0, 0, (3 * dlev) / 2))) *obvious = TRUE;
			break;
		}
		case POW_BURST_ASTRAL:
		{
			if (project_los(GF_ASTRAL, 25)) *obvious = TRUE;
			break;
		}
		case POW_DISPEL_ALL:
		{
			if (project_los(GF_DISP_ALL, calc_damage(0, 0, dlev * 6))) *obvious = TRUE;
			break;
		}
		case POW_DISPEL_UNDEAD_1:
		{
			if (project_los(GF_DISP_UNDEAD, calc_damage(1, dlev * 3, 0))) *obvious = TRUE;
			break;
		}
		case POW_DISPEL_UNDEAD_2:
		{
			if (project_los(GF_DISP_UNDEAD, calc_damage(1, dlev * 4, 0))) *obvious = TRUE;
			break;
		}
		case POW_DISPEL_DEMON:
		{
			if (project_los(GF_DISP_DEMON, calc_damage(1, dlev * 3, 0))) *obvious = TRUE;
			break;
		}
		case POW_DISPEL_NON_EVIL:
		{
			if (project_los(GF_DISP_NON_EVIL, calc_damage(1, dlev * 5, 0))) *obvious = TRUE;
			if (set_taint(p_ptr->taint + 2500)) *obvious = TRUE;
			break;
		}
		case POW_DISPEL_EVIL_3:
		{
			if (project_los(GF_DISP_EVIL, calc_damage(1, dlev * 3, 0))) *obvious = TRUE;
			break;
		}
		case POW_DISPEL_EVIL_4:
		{
			if (project_los(GF_DISP_EVIL, calc_damage(1, dlev * 4, 0))) *obvious = TRUE;
			break;
		}
		case POW_DISPEL_EVIL_5:
		{
			if (project_los(GF_DISP_EVIL, calc_damage(1, dlev * 5, 0))) *obvious = TRUE;
			break;
		}
		case POW_HOLY_1:
		{
			if (p_ptr->taint)
			{
				message(MSG_FAIL, 0, "You are punished for your mockery of holyness!");
				damage_player(50, "A tainted soul");
				*obvious = TRUE;
			}
			else
			{
				durat = randint(apply_sp_mod(25, mdur)) + apply_sp_mod((llev * 3), mdur);

				if (project_los(GF_DISP_EVIL, calc_damage(1, dlev * 6, 0))) *obvious = TRUE;
				if (heal_player(20, 20)) *obvious = TRUE;
				if (set_afraid(0)) *obvious = TRUE;
				if (set_diseased(0)) *obvious = TRUE;
				if (set_poisoned(0)) *obvious = TRUE;
				if (set_stun(0)) *obvious = TRUE;
				if (set_cut(0)) *obvious = TRUE;
				if (set_protevil(p_ptr->protevil + durat)) *obvious = TRUE;
			}
			break;
		}
		case POW_HOLY_2:
		{
			if (p_ptr->taint)
			{
				message(MSG_FAIL, 0, "You are punished for your mockery of holyness!");
				damage_player(50, "A tainted soul");
				*obvious = TRUE;
			}
			else
			{
				if (project_los(GF_DISP_EVIL, calc_damage(1, dlev * 4, 0))) *obvious = TRUE;
				if (hp_player(1000)) *obvious = TRUE;
				if (set_afraid(0)) *obvious = TRUE;
				if (set_diseased(0)) *obvious = TRUE;
				if (set_poisoned(0)) *obvious = TRUE;
				if (set_stun(0)) *obvious = TRUE;
				if (set_cut(0)) *obvious = TRUE;
			}
			break;
		}
		case POW_GENOCIDE:
		{
			genocide();
			*obvious = TRUE;
			break;
		}
		case POW_MASS_GENOCIDE:
		{
			mass_genocide();
			*obvious = TRUE;
			break;
		}
		case POW_EARTHQUAKE:
		{
			earthquake(p_ptr->py, p_ptr->px, 10);
			*obvious = TRUE;
			break;
		}
		case POW_DESTRUCTION:
		{
			destroy_area(p_ptr->py, p_ptr->px, 15, TRUE);
			*obvious = TRUE;
			break;
		}
		case POW_LIGHT_AREA_1: 
		{
			lite_area(calc_damage(2, 2, 0), 2);
			*obvious = TRUE;
			break;
		}
		case POW_LIGHT_AREA_2: 
		{
			lite_area(calc_damage(2, (dlev / 2), 0), (dlev / 10) + 1);
			*obvious = TRUE;
			break;
		}
		case POW_DARK_AREA:
		{
			if (!p_ptr->no_blind)
			{
				(void)set_blind(p_ptr->blind + 3 + randint(5));
			}
			unlite_area(calc_damage(2, (dlev / 2), 0), (dlev / 10) + 1);
			*obvious = TRUE;
			break;
		}
		case POW_DETECT_MONSTERS:
		{
			if (detect_monsters_normal()) *obvious = TRUE;
			break;
		}
		case POW_DETECT_EVIL:
		{
			if (detect_monsters_evil()) *obvious = TRUE;
			break;
		}
		case POW_DETECT_INVIS:
		{
			if (detect_monsters_invis()) *obvious = TRUE;
			break;
		}
		case POW_DETECT_TRAP:
		{
			if (detect_traps()) *obvious = TRUE;
			break;
		}
		case POW_DETECT_TRAP_DOOR:
		{
			if (detect_traps()) *obvious = TRUE;
			if (detect_doors()) *obvious = TRUE;
			if (detect_stairs()) *obvious = TRUE;
			break;
		}
		case POW_DETECT_DOOR_STAIR:
		{
			if (detect_doors()) *obvious = TRUE;
			if (detect_stairs()) *obvious = TRUE;
			break;
		}
		case POW_DETECT_TREASURE:
		{
			if (detect_treasure()) *obvious = TRUE;
			if (detect_objects_gold()) *obvious = TRUE;
			break;
		}
		case POW_DETECT_ITEM:
		{
			if (detect_objects_normal()) *obvious = TRUE;
			break;
		}
		case POW_DETECT_ENCHANT:
		{
			if (detect_objects_magic()) *obvious = TRUE;
			break;
		}
		case POW_DETECT_ALL:
		{
			if (detect_all()) *obvious = TRUE;
			break;
		}
		case POW_ABSORB_HIT:
		{
			durat = randint(apply_sp_mod(36, mdur)) + apply_sp_mod((llev * 2), mdur);

			if (set_absorb(p_ptr->absorb + durat)) *obvious = TRUE;
			break;
		}
		case POW_BLESS_1:
		{
			durat = randint(apply_sp_mod(12, mdur)) + apply_sp_mod(12, mdur);

			if (set_blessed(p_ptr->blessed + durat)) *obvious = TRUE;
			break;
		}
		case POW_BLESS_2:
		{
			durat = randint(apply_sp_mod(30, mdur)) + apply_sp_mod(30, mdur);

			if (set_blessed(p_ptr->blessed + durat)) *obvious = TRUE;
			break;
		}
		case POW_BLESS_3:
		{
			durat = randint(apply_sp_mod(75, mdur)) + apply_sp_mod(75, mdur);

			if (set_blessed(p_ptr->blessed + durat)) *obvious = TRUE;
			break;
		}
		case POW_HEROISM:
		{
			durat = randint(apply_sp_mod(10, mdur)) + apply_sp_mod(10, mdur);

			if (hp_player(10)) *obvious = TRUE;
			if (set_afraid(0)) *obvious = TRUE;
			if (set_hero(p_ptr->hero + durat)) *obvious = TRUE;
			if (set_tim_bravery(p_ptr->tim_bravery + durat)) *obvious = TRUE;
			break;
		}
		case POW_BOLDNESS:
		{
			durat = randint(apply_sp_mod(10, mdur)) + apply_sp_mod(10, mdur);

			if (set_afraid(0)) *obvious = TRUE;
			if (set_tim_bravery(p_ptr->tim_bravery + durat)) *obvious = TRUE;
			break;
		}
		case POW_STABILITY:
		{
			durat = randint(apply_sp_mod(16, mdur)) + apply_sp_mod(16, mdur);

			if (set_confused(0)) *obvious = TRUE;
			if (set_stun(0)) *obvious = TRUE;
			if (set_stability(p_ptr->stability + durat)) *obvious = TRUE;
			break;
		}
		case POW_SAFETY:
		{
			if (p_ptr->safety <= 0)
			{
				durat = randint(apply_sp_mod(5, mdur)) + 
					apply_sp_mod(((llev > 20) ? (llev + (llev / 5)) : (llev + 4)), mdur);

				if (set_safety(durat)) *obvious = TRUE;
			}
			else
			{
				durat = randint(apply_sp_mod(5, mdur));

				if (set_safety(p_ptr->safety + durat)) *obvious = TRUE;
			}
			break;
		}
		case POW_RAGE_1:
		{
			durat = randint(apply_sp_mod(25, mdur)) + apply_sp_mod(25, mdur);

			if (hp_player(20)) *obvious = TRUE;
			if (set_afraid(0)) *obvious = TRUE;
			if (set_rage(p_ptr->rage + durat)) *obvious = TRUE;
			if (set_tim_bravery(p_ptr->tim_bravery + durat)) *obvious = TRUE;
			break;
		}
		case POW_RAGE_2:
		{
			durat = randint(apply_sp_mod(50, mdur)) + apply_sp_mod(50, mdur);

			if (hp_player(20)) *obvious = TRUE;
			if (set_afraid(0)) *obvious = TRUE;
			if (set_rage(p_ptr->rage + durat)) *obvious = TRUE;
			if (set_tim_bravery(p_ptr->tim_bravery + durat)) *obvious = TRUE;
			break;
		}
		case POW_RAGE_BLESS_RESIST:
		{
			durat = randint(apply_sp_mod(50, mdur)) + apply_sp_mod(50, mdur);

			if (hp_player(30)) *obvious = TRUE;
			if (set_afraid(0)) *obvious = TRUE;
			if (set_rage(p_ptr->rage + durat)) *obvious = TRUE;
			if (set_tim_bravery(p_ptr->tim_bravery + durat)) *obvious = TRUE;
			if (set_blessed(p_ptr->blessed + durat)) *obvious = TRUE;
			if (set_tim_res(RS_ACD, p_ptr->tim_res[RS_ACD] + durat)) *obvious = TRUE;
			if (set_tim_res(RS_ELC, p_ptr->tim_res[RS_ELC] + durat)) *obvious = TRUE;
			if (set_tim_res(RS_FIR, p_ptr->tim_res[RS_FIR] + durat)) *obvious = TRUE;
			if (set_tim_res(RS_CLD, p_ptr->tim_res[RS_CLD] + durat)) *obvious = TRUE;
			if (set_tim_res(RS_PSN, p_ptr->tim_res[RS_PSN] + durat)) *obvious = TRUE;
			if (set_tim_res(RS_DIS, p_ptr->tim_res[RS_DIS] + durat)) *obvious = TRUE;
			break;
		}
		case POW_SHIELD:
		{
			durat = randint(apply_sp_mod(20, mdur)) + apply_sp_mod(30, mdur);

			if (set_shield(p_ptr->shield + durat)) *obvious = TRUE;
			break;
		}
		case POW_INVIS_1:
		{
			if (p_ptr->tim_invis <= 0)
			{
				durat = randint(apply_sp_mod(15, mdur)) + apply_sp_mod(llev, mdur);

				if (set_tim_invis(durat)) *obvious = TRUE;
			}
			else
			{
				durat = randint(apply_sp_mod(10, mdur));

				if (set_tim_invis(p_ptr->tim_invis + durat)) *obvious = TRUE;
			}
			break;
		}
		case POW_INVIS_2:
		{
			if (p_ptr->tim_invis <= 0)
			{
				durat = randint(apply_sp_mod(25, mdur)) + apply_sp_mod(25 + llev, mdur);

				if (set_tim_invis(durat)) *obvious = TRUE;
			}
			else
			{
				durat = randint(apply_sp_mod(10, mdur));

				if (set_tim_invis(p_ptr->tim_invis + durat)) *obvious = TRUE;
			}
			break;
		}
		case POW_RESILIENCE:
		{
			durat = randint(apply_sp_mod(8, mdur)) + apply_sp_mod(8, mdur);

			if (set_resilient(p_ptr->resilient + durat)) *obvious = TRUE;
			break;
		}
		case POW_INFRAVISION:
		{
			durat = randint(apply_sp_mod(50, mdur)) + apply_sp_mod(50, mdur);

			if (set_tim_infra(p_ptr->tim_infra + durat)) *obvious = TRUE;
			break;
		}
		case POW_STEALTH:
		{
			if (p_ptr->tim_stealth <= 0)
			{
				durat = randint(apply_sp_mod(25, mdur)) + apply_sp_mod(25 + llev, mdur);

				if (set_tim_stealth(durat)) *obvious = TRUE;
			}
			else
			{
				durat = randint(apply_sp_mod(10, mdur));
				
				if (set_tim_stealth(p_ptr->tim_stealth + durat)) *obvious = TRUE;
			}
			break;
		}
		case POW_SEE_INVIS:
		{
			durat = randint(apply_sp_mod(24, mdur)) + apply_sp_mod(24, mdur);

			if (set_tim_see_invis(p_ptr->tim_see_invis + durat)) *obvious = TRUE;
			break;
		}
		case POW_PROT_EVIL_1:
		{
			durat = randint(apply_sp_mod(25, mdur)) + apply_sp_mod(30, mdur);

			if (set_protevil(p_ptr->protevil + durat)) *obvious = TRUE;
			break;
		}
		case POW_PROT_EVIL_2:
		{
			durat = randint(apply_sp_mod(25, mdur)) + apply_sp_mod(3 * llev, mdur);

			if (set_protevil(p_ptr->protevil + durat)) *obvious = TRUE;
			break;
		}
		case POW_HASTE_SELF_1:
		{
			if (p_ptr->fast <= 0)
			{
				durat = randint(apply_sp_mod(20, mdur)) + apply_sp_mod(llev, mdur);

				if (set_fast(durat)) *obvious = TRUE;
			}
			else
			{
				durat = randint(apply_sp_mod(5, mdur));

				if (set_fast(p_ptr->fast + durat)) *obvious = TRUE;
			}
			break;
		}
		case POW_HASTE_SELF_2:
		{
			if (p_ptr->fast <= 0)
			{
				durat = randint(apply_sp_mod(30, mdur)) + apply_sp_mod(30 + llev, mdur);

				if (set_fast(durat)) *obvious = TRUE;
			}
			else
			{
				durat = randint(apply_sp_mod(10, mdur));

				if (set_fast(p_ptr->fast + durat)) *obvious = TRUE;
			}
			break;
		}
		case POW_HASTE_SELF_3:
		{
			if (p_ptr->fast <= 0)
			{
				durat = randint(apply_sp_mod(75, mdur)) + apply_sp_mod(75, mdur);

				if (set_fast(durat)) *obvious = TRUE;
			}
			else
			{
				durat = apply_sp_mod(5, mdur);

				if (set_fast(p_ptr->fast + durat)) *obvious = TRUE;
			}
			break;
		}
		case POW_DISARM:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			if (disarm_trap(dir)) *obvious = TRUE;
			break;
		}
		case POW_DEST_TRAP_DOOR_1:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			if (destroy_door(dir)) *obvious = TRUE;
			break;
		}
		case POW_DEST_TRAP_DOOR_2:
		{
			if (destroy_doors_touch()) *obvious = TRUE;
			break;
		}
		case POW_STONE_TO_MUD:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			if (wall_to_mud(dir)) *obvious = TRUE;
			break;
		}
		case POW_CREATE_DOOR:
		{
			if (door_creation()) *obvious = TRUE;
			break;
		}
		case POW_CREATE_WALL:
		{
			if (wall_creation()) *obvious = TRUE;
			break;
		}
		case POW_CREATE_STAIR:
		{
			stair_creation();
			*obvious = TRUE;
			break;
		}
		case POW_CREATE_TRAP:
		{
			if (trap_creation(2)) *obvious = TRUE;
			break;
		}
		case POW_MAGIC_LOCK:
		{
			if (magic_lock()) *obvious = TRUE;
			break;
		}
		case POW_ACQUIRE_1:
		{
			acquirement(p_ptr->py, p_ptr->px, 1, TRUE, TRUE);
			*obvious = TRUE;
			break;
		}
		case POW_ACQUIRE_2:
		{
			acquirement(p_ptr->py, p_ptr->px, randint(2) + 1, TRUE, TRUE);
			*obvious = TRUE;
			break;
		}
		case POW_AGGRAVATE_SAFE:
		{
			*obvious = TRUE;
			if (!get_check("Are you sure you wish to aggravate nearby monsters? ")) 
				return (FALSE);
			aggravate_monsters(0);
			break;
		}
		case POW_AGGRAVATE:
		{
			message(MSG_EFFECT, 0, "There is a high pitched humming noise.");
			*obvious = TRUE;
			aggravate_monsters(0);
			break;
		}
		case POW_CONFUSE_MONSTER:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			if (fire_bolt(GF_CONF_ALL, dir, ilev)) *obvious = TRUE;
			break;
		}
		case POW_CONFUSE_ALL:
		{
			if (project_los(GF_CONF_ALL, ilev)) *obvious = TRUE;
			break;
		}
		case POW_SLEEP_MONSTER:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			if (fire_bolt(GF_SLEEP_ALL, dir, ilev)) *obvious = TRUE;
			break;
		}
		case POW_SLEEP_ADJACENT:
		{
			if (sleep_monsters_touch(ilev)) *obvious = TRUE;
			break;
		}
		case POW_SLEEP_ALL:
		{
			if (project_los(GF_SLEEP_ALL, ilev)) *obvious = TRUE;
			break;
		}
		case POW_SLOW_MONSTER:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			if (fire_bolt(GF_SLOW_ALL, dir, ilev)) *obvious = TRUE;
			break;
		}
		case POW_SLOW_ALL:
		{
			if (project_los(GF_SLOW_ALL, ilev)) *obvious = TRUE;
			break;
		}
		case POW_CALM_MONSTER:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			if (fire_bolt(GF_CALM_ALL, dir, ((3 * ilev) / 2))) *obvious = TRUE;
			break;
		}
		case POW_CALM_ANIMALS:
		{
			if (project_los(GF_CALM_ANIMALS, ((3 * ilev) / 2))) *obvious = TRUE;
			break;
		}
		case POW_CALM_NON_EVIL:
		{
			if (project_los(GF_CALM_NON_EVIL, ((3 * ilev) / 2))) *obvious = TRUE;
			break;
		}
		case POW_CALM_NON_CHAOS:
		{
			if (project_los(GF_CALM_NON_CHAOS, 100)) *obvious = TRUE;
		}
		case POW_CALM_ALL:
		{
			if (project_los(GF_CALM_ALL, ((3 * ilev) / 2))) *obvious = TRUE;
			break;
		}
		case POW_BLIND_MONSTER:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			if (fire_bolt(GF_BLIND_ALL, dir, ilev)) *obvious = TRUE;
			break;
		}
		case POW_SCARE_MONSTER:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			if (fire_bolt(GF_SCARE_ALL, dir, ilev)) *obvious = TRUE;
			break;
		}
		case POW_SCARE_ALL:
		{
			if (project_los(GF_SCARE_ALL, ilev)) *obvious = TRUE;
			break;
		}
		case POW_CALL_MONSTER:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			if (fire_bolt(GF_CALL_ALL, dir, 0)) *obvious = TRUE;
			break;
		}
		case POW_POLY_MONSTER:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			if (fire_bolt(GF_POLY_ALL, dir, ilev)) *obvious = TRUE;
			break;
		}
		case POW_HEAL_MONSTER:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			if (fire_bolt(GF_HEAL_ALL, dir, damroll(4, 8))) *obvious = TRUE;
			break;
		}

		case POW_HASTE_MONSTER:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			if (fire_bolt(GF_SPEED_ALL, dir, 0)) *obvious = TRUE;
			break;
		}

		case POW_CLONE_MONSTER:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			if (fire_bolt(GF_CLONE_ALL, dir, 0)) *obvious = TRUE;
			break;
		}
		case POW_SATISFY_HUNGER:
		{
			if (set_food(PY_FOOD_MAX - 1)) *obvious = TRUE;
			break;
		}
		case POW_RECHARGE_1:
		{
			*obvious = TRUE;
			if (!recharge(dlev)) return (FALSE);
			break;
		}
		case POW_RECHARGE_2:
		{
			*obvious = TRUE;
			if (!recharge(15)) return (FALSE);
			break;
		}
		case POW_RECHARGE_3:
		{
			*obvious = TRUE;
			if (!recharge(60)) return (FALSE);
			break;
		}
		case POW_HYPERCHARGE:
		{
			*obvious = TRUE;
			if (!hypercharge()) return (FALSE);
			break;
		}
		case POW_IDENTIFY:
		{
			*obvious = TRUE;
			if (!ident_spell()) return (FALSE);
			break;
		}
		case POW_IDENTIFY_PACK:
		{
			identify_pack();
			*obvious = TRUE;
			break;
		}
		case POW_IDENTIFY_FULL:
		{
			*obvious = TRUE;
			if (!identify_fully()) return (FALSE);
			break;
		}
		case POW_RES_ACID:
		{
			durat = randint(apply_sp_mod(20, mdur)) + apply_sp_mod(20, mdur);

			if (set_tim_res(RS_ACD, p_ptr->tim_res[RS_ACD] + durat)) *obvious = TRUE;
			break;
		}
		case POW_RES_ELEC:
		{
			durat = randint(apply_sp_mod(20, mdur)) + apply_sp_mod(20, mdur);

			if (set_tim_res(RS_ELC, p_ptr->tim_res[RS_ELC] + durat)) *obvious = TRUE;
			break;
		}
		case POW_RES_FIRE:
		{
			durat = randint(apply_sp_mod(20, mdur)) + apply_sp_mod(20, mdur);

			if (set_tim_res(RS_FIR, p_ptr->tim_res[RS_FIR] + durat)) *obvious = TRUE;
			break;
		}
		case POW_RES_COLD:
		{
			durat = randint(apply_sp_mod(20, mdur)) + apply_sp_mod(20, mdur);

			if (set_tim_res(RS_CLD, p_ptr->tim_res[RS_CLD] + durat)) *obvious = TRUE;
			break;
		}
		case POW_RES_FIRE_COLD:
		{
			durat = randint(apply_sp_mod(10, mdur)) + apply_sp_mod(10, mdur);

			if (set_tim_res(RS_FIR, p_ptr->tim_res[RS_FIR] + durat)) *obvious = TRUE;
			if (set_tim_res(RS_CLD, p_ptr->tim_res[RS_CLD] + durat)) *obvious = TRUE;
			break;
		}
		case POW_RES_ACID_ELEC:
		{
			durat = randint(apply_sp_mod(10, mdur)) + apply_sp_mod(10, mdur);

			if (set_tim_res(RS_ACD, p_ptr->tim_res[RS_ACD] + durat)) *obvious = TRUE;
			if (set_tim_res(RS_ELC, p_ptr->tim_res[RS_ELC] + durat)) *obvious = TRUE;
			break;
		}
		case POW_RES_LITE_DARK:
		{
			durat = randint(apply_sp_mod(20, mdur)) + apply_sp_mod(20, mdur);

			if (set_tim_res(RS_LIT, p_ptr->tim_res[RS_LIT] + durat)) *obvious = TRUE;
			if (set_tim_res(RS_DRK, p_ptr->tim_res[RS_DRK] + durat)) *obvious = TRUE;
			break;
		}
		case POW_RES_CHAOS_NEXUS:
		{
			durat = randint(apply_sp_mod(20, mdur)) + apply_sp_mod(20, mdur);

			if (set_tim_res(RS_CHS, p_ptr->tim_res[RS_CHS] + durat)) *obvious = TRUE;
			if (set_tim_res(RS_NEX, p_ptr->tim_res[RS_NEX] + durat)) *obvious = TRUE;
			break;
		}
		case POW_RES_POISON:
		{
			durat = randint(apply_sp_mod(20, mdur)) + apply_sp_mod(20, mdur);

			if (set_tim_res(RS_PSN, p_ptr->tim_res[RS_PSN] + durat)) *obvious = TRUE;
			break;
		}
		case POW_RES_DISEASE:
		{
			durat = randint(apply_sp_mod(20, mdur)) + apply_sp_mod(20, mdur);

			if (set_tim_res(RS_DIS, p_ptr->tim_res[RS_DIS] + durat)) *obvious = TRUE;
			break;
		}
		case POW_RES_SOUND:
		{
			durat = randint(apply_sp_mod(20, mdur)) + apply_sp_mod(20, mdur);

			if (set_tim_res(RS_SND, p_ptr->tim_res[RS_SND] + durat)) *obvious = TRUE;
			break;
		}
		case POW_RES_ELEMENTS:
		{
			durat = randint(apply_sp_mod(llev / 2, mdur)) + apply_sp_mod(llev / 2, mdur);

			if (set_tim_res(RS_ACD, p_ptr->tim_res[RS_ACD] + durat)) *obvious = TRUE;
			if (set_tim_res(RS_ELC, p_ptr->tim_res[RS_ELC] + durat)) *obvious = TRUE;
			if (set_tim_res(RS_FIR, p_ptr->tim_res[RS_FIR] + durat)) *obvious = TRUE;
			if (set_tim_res(RS_CLD, p_ptr->tim_res[RS_CLD] + durat)) *obvious = TRUE;
			break;
		}
		case POW_RES_GREATER:
		{
			durat = randint(apply_sp_mod(llev / 3, mdur)) + apply_sp_mod(llev / 3, mdur);

			if (set_tim_res(RS_PSN, p_ptr->tim_res[RS_PSN] + durat)) *obvious = TRUE;
			if (set_tim_res(RS_DIS, p_ptr->tim_res[RS_DIS] + durat)) *obvious = TRUE;
			if (set_tim_res(RS_LIT, p_ptr->tim_res[RS_LIT] + durat)) *obvious = TRUE;
			if (set_tim_res(RS_DRK, p_ptr->tim_res[RS_DRK] + durat)) *obvious = TRUE;
			if (set_tim_res(RS_SND, p_ptr->tim_res[RS_SND] + durat)) *obvious = TRUE;
			if (set_tim_res(RS_SHR, p_ptr->tim_res[RS_SHR] + durat)) *obvious = TRUE;
			if (set_tim_res(RS_NEX, p_ptr->tim_res[RS_NEX] + durat)) *obvious = TRUE;
			if (set_tim_res(RS_NTH, p_ptr->tim_res[RS_NTH] + durat)) *obvious = TRUE;
			if (set_tim_res(RS_CHS, p_ptr->tim_res[RS_CHS] + durat)) *obvious = TRUE;
			if (set_tim_res(RS_WTR, p_ptr->tim_res[RS_WTR] + durat)) *obvious = TRUE;
			break;
		}
		case POW_RESISTANCE:
		{
			durat = randint(apply_sp_mod(20, mdur)) + apply_sp_mod(20, mdur);

			if (set_tim_res(RS_ACD, p_ptr->tim_res[RS_ACD] + durat)) *obvious = TRUE;
			if (set_tim_res(RS_ELC, p_ptr->tim_res[RS_ELC] + durat)) *obvious = TRUE;
			if (set_tim_res(RS_FIR, p_ptr->tim_res[RS_FIR] + durat)) *obvious = TRUE;
			if (set_tim_res(RS_CLD, p_ptr->tim_res[RS_CLD] + durat)) *obvious = TRUE;
			if (set_tim_res(RS_PSN, p_ptr->tim_res[RS_PSN] + durat)) *obvious = TRUE;
			break;
		}
		case POW_GLYPH_WARDING:
		{
			if (!warding_glyph(WG_GLYPH))
			{
				message(MSG_FAIL, 0, "The floor glows for a moment, but nothing happens.");
			}
			*obvious = TRUE;
			break;
		}
		case POW_GLYPH_LESSER:
		{
			if (!warding_glyph(WG_GLYPH_LESSER))
			{
				message(MSG_FAIL, 0, "The floor glows for a moment, but nothing happens.");
			}
			*obvious = TRUE;
			break;
		}
		case POW_GLYPH_HOLY:
		{
			if (p_ptr->taint)
			{
				message(MSG_FAIL, 0, "No holy authority answers your request.");
			}
			else if (!warding_glyph(WG_GLYPH_HOLY))
			{
				message(MSG_FAIL, 0, "The floor glows for a moment, but nothing happens.");
			}
			*obvious = TRUE;
			break;
		}
		case POW_REMOVE_CURSE_1:
		{
			if (remove_curse())
			{
				message(MSG_EFFECT, 0, "You feel as if someone is watching over you.");
				*obvious = TRUE;
			}
			break;
		}
		case POW_REMOVE_CURSE_2:
		{
			if (remove_all_curse())
			{
				message(MSG_EFFECT, 0, "You feel as if someone is watching over you.");
				*obvious = TRUE;
			}
			break;
		}
		case POW_MAP_1:
		{
			map_area();
			*obvious = TRUE;
			break;
		}
		case POW_MAP_2:
		{
			wiz_lite();
			*obvious = TRUE;
			break;
		}
		case POW_MAP_3:
		{
			wiz_lite();
			(void)detect_traps();
			(void)detect_doors();
			(void)detect_stairs();
			*obvious = TRUE;
			break;
		}
		case POW_PROBE_MONSTER:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			if (fire_bolt(GF_PROBE, dir, 0)) *obvious = TRUE;
			break;
		}
		case POW_PROBE_ALL:
		{
			if (project_los(GF_PROBE, 0))
			{
				message(MSG_GENERIC, 0, "That's all.");
				*obvious = TRUE;
			}
			break;
		}
		case POW_KNOW_ALL:
		{
			message(MSG_EFFECT, 0, "You begin to feel more enlightened...");
			message_flush();
			wiz_lite();
			(void)do_inc_stat(A_INT);
			(void)do_inc_stat(A_WIS);
			(void)detect_traps();
			(void)detect_doors();
			(void)detect_stairs();
			(void)detect_treasure();
			(void)detect_objects_gold();
			(void)detect_objects_normal();
			identify_pack();
			*obvious = TRUE;
			break;
		}
		case POW_ENCHANT_WEAPON_1:
		{
			*obvious = TRUE;
			if (!enchant_spell(1, 0)) return (FALSE);
			break;
		}
		case POW_ENCHANT_WEAPON_2:
		{
			*obvious = TRUE;
			if (!enchant_spell(randint(4), 0)) return (FALSE);
			break;
		}
		case POW_ENCHANT_ARMOR_1:
		{
			*obvious = TRUE;
			if (!enchant_spell(0, 1)) return (FALSE);
			break;
		}
		case POW_ENCHANT_ARMOR_2:
		{
			*obvious = TRUE;
			if (!enchant_spell(0, randint(4) + 1)) return (FALSE);
			break;
		}
		case POW_BRAND_WEAPON_ELMNT:
		{
			/* Hack - choose random brand */
			i = rand_int(4);
			switch (i)
			{
				case 0: i = EGO_BRAND_ACID; break;
				case 1: i = EGO_BRAND_ELEC; break;
				case 2: i = EGO_BRAND_FIRE; break;
				case 3: i = EGO_BRAND_COLD; break;
			}
			*obvious = TRUE;
			if (!brand_weapon(0, i, TRUE)) return (FALSE);
			break;
		}
		case POW_BRAND_AMMO_ANML:
		{	
			*obvious = TRUE;
			if (!brand_weapon(TV_ARROW, EGO_HURT_ANIMAL, FALSE)) return (FALSE);
			break;
		}
		case POW_BRAND_AMMO_WOUND:
		{	
			*obvious = TRUE;
			if (!brand_weapon(TV_ARROW, EGO_WOUNDING, TRUE)) return (FALSE);
			break;
		}
		case POW_BRAND_AMMO_ELMNT:
		{	
			/* 
			 * Hack - choose random brand 
			 */

			i = rand_int(4);
			switch (i)
			{
				case 0: i = EGO_AMMO_ACID; break;
				case 1: i = EGO_AMMO_ELEC; break;
				case 2: i = EGO_AMMO_FIRE; break;
				case 3: i = EGO_AMMO_COLD; break;
			}
			*obvious = TRUE;
			if (!brand_weapon(TV_ARROW, i, TRUE)) return (FALSE);
			break;
		}
		case POW_BRAND_AMMO_HOLY:
		{
			*obvious = TRUE;
			if (!brand_weapon(TV_ARROW, EGO_HURT_HOLY_MIGHT, TRUE)) return (FALSE);
			break;
		}
		case POW_BIZZARE:
		{
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);
			ring_of_power(dir);
			*obvious = TRUE;
			break;
		}
		case POW_CURSE_EQUIP_1:
		{
			i = rand_int(5);

			if ((i == 0) && curse_armor()) *obvious = TRUE;
			else if ((i == 1) && curse_weapon()) *obvious = TRUE;
			else if (curse_minor()) *obvious = TRUE;
			break;
		}
		case POW_CURSE_EQUIP_2:
		{
			if (curse_armor()) *obvious = TRUE;
			if (curse_weapon()) *obvious = TRUE;
			if (curse_minor()) *obvious = TRUE;
			break;
		}
		case POW_SUM_MONSTER:
		{
			for (i = 0; i < randint(4); i++)
			{
				if (summon_specific(p_ptr->py, p_ptr->px, p_ptr->depth, 0))
				{
					*obvious = TRUE;
				}
			}
			break;
		}
		case POW_SUM_UNDEAD:
		{
			for (i = 0; i < randint(3); i++)
			{
				if (summon_specific(p_ptr->py, p_ptr->px, p_ptr->depth, SUMMON_UNDEAD))
					*obvious = TRUE;
			}
			break;
		}
		case POW_SUM_DRAGON:
		{
		for (i = 0; i < randint(3); i++)
			{
				if (summon_specific(p_ptr->py, p_ptr->px, p_ptr->depth, SUMMON_DRAGON))
					*obvious = TRUE;
			}
			break;
		}
		case POW_NAUSEA:
		{
			message(MSG_EFFECT, 0, "You feel you must vomit!");
			(void)set_food(PY_FOOD_STARVE - 1);
			(void)set_poisoned(0);
			(void)set_paralyzed(p_ptr->paralyzed + 4);
			*obvious = TRUE;
			break;
		}
		case POW_POISON_SELF:
		{
			if (!p_ptr->no_poison && !resist_effect(RS_PSN))
			{
				if (set_poisoned(p_ptr->poisoned + rand_int(15) + 10))
				{
					*obvious = TRUE;
				}
			}
			break;
		}
		case POW_BLIND_SELF:
		{
			if (!p_ptr->no_blind)
			{
				if (set_blind(p_ptr->blind + rand_int(150) + 200))
				{
					*obvious = TRUE;
				}
			}
			break;
		}
		case POW_SCARE_SELF:
		{
			if (!p_ptr->bravery)
			{
				if (set_afraid(p_ptr->afraid + rand_int(10) + 20))
				{
					*obvious = TRUE;
				}
			}
			break;
		}
		case POW_CONFUSE_SELF:
		{
			if (!p_ptr->no_confuse)
			{
				if (set_confused(p_ptr->confused + rand_int(10) + 20))
				{
					*obvious = TRUE;
				}
			}
			break;
		}
		case POW_HALLUCINATE:
		{
			if (!resist_effect(RS_CHS))
			{
				if (set_image(p_ptr->image + rand_int(250) + 250))
				{
					*obvious = TRUE;
				}
			}
			break;
		}
		case POW_SLOW_SELF:
		{
			if (set_slow(p_ptr->slow + randint(25) + 15)) *obvious = TRUE;
			break;
		}
		case POW_PARALYZE:
		{
			if (!p_ptr->free_act)
			{
				if (set_paralyzed(p_ptr->paralyzed + rand_int(8) + 6))
				{
					*obvious = TRUE;
				}
			}
			break;
		}
		case POW_DISEASE:
		{
			if (!p_ptr->no_disease && !resist_effect(RS_DIS))
			{
				if(set_diseased(p_ptr->diseased + rand_int(30) + 80)) *obvious = TRUE;
			}
			break;
		}
		case POW_DEFORM:
		{
			message(MSG_EFFECT, 0, "You feel your flesh twist and contort");
			for (i = 0; i < 3; i++) scramble_stats();
			p_ptr->ht += (rand_int(11) - 5);
			p_ptr->wt += (rand_int(21) - 10);
			if (p_ptr->ht<20) p_ptr->ht = 20;
			if (p_ptr->ht<20) p_ptr->ht = 20;
			*obvious = TRUE;
			break;
		}
		case POW_TAINT:
		{
			if (!p_ptr->blessed)
			{
				if(set_taint(p_ptr->taint + rand_int(100) + 150)) *obvious = TRUE;
			}
			break;
		}
		case POW_AMNESIA:
		{
			if (lose_all_info())
			{
				message(MSG_EFFECT, 0, "Your memories fade away.");
				*obvious = TRUE;
			}
			break;
		}
		case POW_LOSE_STR:
		{
			if (do_dec_stat(A_STR, 1, FALSE, TRUE)) *obvious = TRUE;
			break;
		}
		case POW_LOSE_INT:
		{
			if (do_dec_stat(A_INT, 1, FALSE, TRUE)) *obvious = TRUE;
			break;
		}
		case POW_LOSE_WIS:
		{
			if (do_dec_stat(A_WIS, 1, FALSE, TRUE)) *obvious = TRUE;
			break;
		}
		case POW_LOSE_DEX:
		{
			if (do_dec_stat(A_DEX, 1, FALSE, TRUE)) *obvious = TRUE;
			break;
		}
		case POW_LOSE_CON:
		{
			if (do_dec_stat(A_CON, 1, FALSE, TRUE)) *obvious = TRUE;
			break;
		}
		case POW_LOSE_CHR:
		{
			if (do_dec_stat(A_CHR, 1, FALSE, TRUE)) *obvious = TRUE;
			break;
		}
		case POW_LOSE_EXP:
		{
			if (!p_ptr->hold_life && (p_ptr->exp > 0))
			{
				message(MSG_EFFECT, 0, "You feel your memories fade.");
				lose_exp(p_ptr->exp / 4);
				*obvious = TRUE;
			}
			break;
		}
		case POW_RUINATION:
		{
			message(MSG_EFFECT, 0, "Your nerves and muscles feel weak and lifeless!");
			damage_player(damroll(10, 10), "ruination");
			(void)do_dec_stat(A_STR, 3, TRUE, FALSE);
			(void)do_dec_stat(A_WIS, 3, TRUE, FALSE);
			(void)do_dec_stat(A_INT, 3, TRUE, FALSE);
			(void)do_dec_stat(A_DEX, 3, TRUE, FALSE);
			(void)do_dec_stat(A_CON, 3, TRUE, FALSE);
			(void)do_dec_stat(A_CHR, 3, TRUE, FALSE);
			*obvious = TRUE;
			break;
		}
		case POW_DETONATE:
		{
			message(MSG_EFFECT, 0, "Massive explosions rupture your body!");
			damage_player(damroll(50, 20), "detonation");
			(void)set_stun(p_ptr->stun + 75);
			(void)set_cut(p_ptr->cut + 5000);
			*obvious = TRUE;
			break;
		}
		case POW_KILL_SELF:
		{
			message(MSG_EFFECT, 0, "A feeling of Death flows through your body.");
			damage_player(5000, "a bad mistake");
			*obvious = TRUE;
			break;
		}
		case POW_SPELL_DURATION:
		{
			if (set_tim_sp_dur(77)) *obvious = TRUE;
			break;
		}
		case POW_SPELL_DAMAGE:
		{
			if (set_tim_sp_dam(77)) *obvious = TRUE;
			break;
		}
		case POW_SPELL_INFLUENCE:
		{
			if (set_tim_sp_inf(77)) *obvious = TRUE;
			break;
		}
		case POW_DRAGON_BLACK:
		case POW_DRAGON_BLUE:
		case POW_DRAGON_WHITE:
		case POW_DRAGON_RED:	
		case POW_DRAGON_GREEN:
		case POW_DRAGON_GOLD:		
		case POW_DRAGON_SILVER:
		case POW_DRAGON_MH:	
		case POW_DRAGON_SPIRIT:	
		case POW_DRAGON_SHADOW:	
		case POW_DRAGON_ETHER:	
		case POW_DRAGON_CHAOS:	
		case POW_DRAGON_TIME:		
		case POW_DRAGON_POWER:
		{
			cptr breath;
			int typ, power, chance;

			/* Get a direction for breathing (or abort) */
			if (!dir) if (!get_aim_dir(&dir)) return (FALSE);

			/* Switch (again) */
			switch (idx)
			{
				case POW_DRAGON_BLUE:
				{
					breath = "lightning";
					typ = GF_ELEC;
					power = 125;
					break;
				}

				case POW_DRAGON_WHITE:
				{
					breath = "frost";
					typ = GF_COLD;
					power = 125;
					break;
				}

				case POW_DRAGON_BLACK:
				{
					breath = "acid";
					typ = GF_ACID;
					power = 125;
					break;
				}

				case POW_DRAGON_RED:
				{
					breath = "fire";
					typ = GF_FIRE;
					power = 125;
					break;
				}

				case POW_DRAGON_GREEN:
				{
					breath = "poison gas";
					typ = GF_POIS;
					power = 150;
					break;
				}

				case POW_DRAGON_GOLD:
				{
					breath = "sound";
					typ = GF_SOUND;
					power = 100;
					break;
				}

				case POW_DRAGON_SILVER:
				{
					breath = "shards";
					typ = GF_SHARD;
					power = 100;
					break;
				}

				case POW_DRAGON_MH:
				{
					chance = rand_int(7);
					switch (chance)
					{
						case 0:	breath = "fire";		typ = GF_FIRE; break;
						case 1:	breath = "lightning";	typ = GF_ELEC; break;
						case 2:	breath = "frost";		typ = GF_COLD; break;
						case 3:	breath = "acid";		typ = GF_ACID; break;
						case 4:	breath = "poison gas";	typ = GF_POIS;	break;
						case 5:	breath = "shards";		typ = GF_SHARD; break;
						case 6:	breath = "sound";		typ = GF_SOUND; break;
					}
					power = 250;
					break;
				}

				case POW_DRAGON_SHADOW:
				{
					breath = "nether";
					typ = GF_NETHER;
					power = 250;
					break;
				}

				case POW_DRAGON_SPIRIT:
				{
					breath = "force";
					typ = GF_FORCE;
					power = 250;
					break;
				}

				case POW_DRAGON_ETHER:
				{
					chance = rand_int(2);
					switch (chance)
					{
						case 0:	breath = "light";		typ = GF_LITE; break;
						case 1:	breath = "darkness";	typ = GF_DARK; break;
					}
					power = 250;
					break;
				}

				case POW_DRAGON_CHAOS:
				{
					chance = rand_int(4);
					switch (chance)
					{
						case 0:	breath = "chaos";			typ = GF_CHAOS; break;
						case 1:	breath = "disenchantment";	typ = GF_DISENCHANT; break;
						case 2:	breath = "plasma";			typ = GF_PLASMA; break;
						case 3:	breath = "sound";			typ = GF_SOUND; break;
					}
					power = 350;
					break;
				}

				case POW_DRAGON_TIME:
				{
					chance = rand_int(4);
					switch (chance)
					{
						case 0:	breath = "time";			typ = GF_TIME; break;
						case 1:	breath = "inertia";			typ = GF_INERTIA; break;
						case 2:	breath = "plasma";			typ = GF_NEXUS; break;
						case 3:	breath = "nether";			typ = GF_NETHER; break;
					}
					power = 350;
					break;
				}

				case POW_DRAGON_POWER:
				{
					breath = "the elements";
					typ = GF_MISSILE;
					power = 400;
					break;
				}
			}
			
			/* Modify power according to level */
			power += rand_int((dlev * 2) + 1);

			/* Message */
			message_format(MSG_DSM, 0, "You invoke %s breath!", breath);

			/* Actual attack */
			fire_ball(typ, dir, power, 2);

			*obvious = TRUE;

			break;
		}
		case POW_RISK_HACK:
		{
			/* 
			 * Does nothing at the moment, potion of risk is hardcoded.
			 * The power only exists for the message when 'I'nspected
			 */
			break;
		}
		case POW_WONDER_HACK:
		{
			/* 
			 * Does nothing at the moment, wand of wander is hardcoded.
			 * The power only exists for the message when 'I'nspected
			 */
			break;
		}
		case POW_MUSIC_LYRE:
		{
			do_play(SV_MUSIC_LYRE, 5);
			break;
		}
		case POW_MUSIC_HORN:
		{
			do_play(SV_MUSIC_HORN, 5);
			break;
		}
		case POW_MUSIC_FLUTE:
		{
			do_play(SV_MUSIC_FLUTE, 5);
			break;
		}
		case POW_MUSIC_LUTE:
		{
			do_play(SV_MUSIC_LUTE, 5);
			break;
		}
		case POW_MUSIC_DRUM:
		{
			do_play(SV_MUSIC_DRUM, 5);
			break;
		}
		case POW_MUSIC_HARP:
		{
			do_play(SV_MUSIC_HARP, 5);
			break;
		}
		case POW_SHRPOISON:
		{
			if (cp_ptr->flags & CF_SHROOM_MAGIC)
			{
				message(MSG_EFFECT, 0, "As you eat the mushroom, you call on its Mother Spirit to help you.");

				cptr breath;
				int typ, power, chance;

				/* Get a direction for breathing (or abort) */
				if (!dir) if (!get_aim_dir(&dir)) return (FALSE);

				breath = "poison gas";
				typ = GF_POIS;
				power = 150;

				/* Modify power according to level */
				power += rand_int((dlev * 2) + 1);

				/* Message */
				message_format(MSG_DSM, 0, "You invoke %s breath!", breath);

				/* Actual attack */
				fire_ball(typ, dir, power, 2);

				*obvious = TRUE;
			}
			else
			{
				if (!p_ptr->no_poison && !resist_effect(RS_PSN))
				{
					if (set_poisoned(p_ptr->poisoned + rand_int(15) + 10))
					{
						*obvious = TRUE;
					}
				}
			}
			break;
		}
		case POW_SHRBLIND:
		{
			if (cp_ptr->flags & CF_SHROOM_MAGIC)
			{
				message(MSG_EFFECT, 0, "As you eat the mushroom, you call on its Mother Spirit to help you.");
				if (project_los(GF_BLIND_ALL, ilev)) *obvious = TRUE;
			}
			else
			{
				if (!p_ptr->no_blind)
				{
					if (set_blind(p_ptr->blind + rand_int(150) + 200))
					{
						*obvious = TRUE;
					}
				}
			}
			break;
		}
		case POW_SHRSCARE:
		{
			if (cp_ptr->flags & CF_SHROOM_MAGIC)
			{
				message(MSG_EFFECT, 0, "As you eat the mushroom, you call on its Mother Spirit to help you.");
				if (project_los(GF_SCARE_ALL, ilev)) *obvious = TRUE;
			}
			else
			{
				if (!p_ptr->bravery)
				{
					if (set_afraid(p_ptr->afraid + rand_int(10) + 20))
					{
						*obvious = TRUE;
					}
				}
			}
			break;
		}
		case POW_SHRCONFUSE:
		{
			if (cp_ptr->flags & CF_SHROOM_MAGIC)
			{
				message(MSG_EFFECT, 0, "As you eat the mushroom, you call on its Mother Spirit to help you.");
				if (project_los(GF_CONF_ALL, ilev)) *obvious = TRUE;
			}
			else
			{
				if (!p_ptr->no_confuse)
				{
					if (set_confused(p_ptr->confused + rand_int(10) + 20))
					{
						*obvious = TRUE;
					}
				}
			}
			break;
		}
		case POW_SHRHALLUCINATE:
		{
			if (cp_ptr->flags & CF_SHROOM_MAGIC)
			{
				message(MSG_EFFECT, 0, "As you eat the mushroom, you call on its Mother Spirit to help you.");
				durat = randint(apply_sp_mod(24, mdur)) + apply_sp_mod(24, mdur);
				if (set_tim_see_invis(p_ptr->tim_see_invis + durat)) *obvious = TRUE;
			}
			else
			{
				if (!resist_effect(RS_CHS))
				{
					if (set_image(p_ptr->image + rand_int(250) + 250))
					{
						*obvious = TRUE;
					}
				}
			}
			break;
		}
		case POW_SHRPARALYZE:
		{
			if (cp_ptr->flags & CF_SHROOM_MAGIC)
			{
				message(MSG_EFFECT, 0, "As you eat the mushroom, you call on its Mother Spirit to help you.");
				if (project_los(GF_SLEEP_ALL, ilev)) *obvious = TRUE;
			}
			else
			{
				if (!p_ptr->free_act)
				{
					if (set_paralyzed(p_ptr->paralyzed + rand_int(8) + 6))
					{
						*obvious = TRUE;
					}
				}
			}
			break;
		}
		case POW_SHRNAIVITY:
		{
			if (cp_ptr->flags & CF_SHROOM_MAGIC)
			{
				message(MSG_EFFECT, 0, "As you eat the mushroom, you call on its Mother Spirit to help you.");
				if (p_ptr->csp < p_ptr->msp)
				{
					p_ptr->csp = p_ptr->msp;
					p_ptr->csp_frac = 0;
					message(MSG_EFFECT, 0, "Your feel your head clear.");
					*obvious = TRUE;
					p_ptr->redraw |= (PR_MANA);
					p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
				}
			}
			else
			{
				if (do_dec_stat(A_WIS, 1, FALSE, TRUE)) *obvious = TRUE;
			}
			break;
		}
		case POW_SHRSTUPIDITY:
		{
			if (cp_ptr->flags & CF_SHROOM_MAGIC)
			{
				message(MSG_EFFECT, 0, "As you eat the mushroom, you call on its Mother Spirit to help you.");

				durat = randint(apply_sp_mod(25, mdur)) + apply_sp_mod(25, mdur);

				if (hp_player(20)) *obvious = TRUE;
				if (set_afraid(0)) *obvious = TRUE;
				if (set_rage(p_ptr->rage + durat)) *obvious = TRUE;
				if (set_tim_bravery(p_ptr->tim_bravery + durat)) *obvious = TRUE;
			}
			else
			{
				if (do_dec_stat(A_INT, 1, FALSE, TRUE)) *obvious = TRUE;
			}
			break;
		}
		case POW_SHRAMNESIA:
		{
			if (cp_ptr->flags & CF_SHROOM_MAGIC)
			{
				message(MSG_EFFECT, 0, "As you eat the mushroom, you call on its Mother Spirit to help you.");

				if (p_ptr->tim_invis <= 0)
				{
					durat = randint(apply_sp_mod(15, mdur)) + apply_sp_mod(llev, mdur);

					if (set_tim_invis(durat)) *obvious = TRUE;
				}
				else
				{
					durat = randint(apply_sp_mod(10, mdur));

					if (set_tim_invis(p_ptr->tim_invis + durat)) *obvious = TRUE;
				}
			}
			else
			{
				if (lose_all_info())
				{
					message(MSG_EFFECT, 0, "Your memories fade away.");
					*obvious = TRUE;
				}
			}
			break;
		}
		case POW_SHRDISEASE:
		{
			if (cp_ptr->flags & CF_SHROOM_MAGIC)
			{
				message(MSG_EFFECT, 0, "As you eat the mushroom, you call on its Mother Spirit to help you.");

				cptr breath;
				int typ, power, chance;

				/* Get a direction for breathing (or abort) */
				if (!dir) if (!get_aim_dir(&dir)) return (FALSE);

				breath = "nether";
				typ = GF_NETHER;
				power = 250;

				/* Modify power according to level */
				power += rand_int((dlev * 2) + 1);

				/* Message */
				message_format(MSG_DSM, 0, "You invoke %s breath!", breath);

				/* Actual attack */
				fire_ball(typ, dir, power, 2);

				*obvious = TRUE;

			}
			else
			{
				if (!p_ptr->no_disease && !resist_effect(RS_DIS))
				{
					if(set_diseased(p_ptr->diseased + rand_int(30) + 80)) *obvious = TRUE;
				}
			}
			break;
		}
		case POW_SHRCURE_POISON:
		{
			if (cp_ptr->flags & CF_SHROOM_MAGIC)
			{
				message(MSG_EFFECT, 0, "As you eat the mushroom, you call on its Mother Spirit to help you.");
			}
			if (set_poisoned(0)) *obvious = TRUE;
			break;
		}
		case POW_SHRCURE_DISEASE:
		{
			if (cp_ptr->flags & CF_SHROOM_MAGIC)
			{
				message(MSG_EFFECT, 0, "As you eat the mushroom, you call on its Mother Spirit to help you.");
			}
			if (set_diseased(0)) *obvious = TRUE;
			break;
		}
		case POW_SHRCURE_CONFUSION:
		{
			if (cp_ptr->flags & CF_SHROOM_MAGIC)
			{
				message(MSG_EFFECT, 0, "As you eat the mushroom, you call on its Mother Spirit to help you.");
			}
			if (set_confused(0)) *obvious = TRUE;
			break;
		}
		case POW_SHRHEAL_1:
		{
			if (cp_ptr->flags & CF_SHROOM_MAGIC)
			{
				message(MSG_EFFECT, 0, "As you eat the mushroom, you call on its Mother Spirit to help you.");
				if (heal_player(15, 20)) *obvious = TRUE;
				if (set_cut((p_ptr->cut / 2) - 35)) *obvious = TRUE;
			}
			else
			{
				if (heal_player(5, 10)) *obvious = TRUE;
				if (set_cut(p_ptr->cut - 10)) *obvious = TRUE;
			}
			break;
		}
		case POW_SHRHEAL_2:
		{
			if (cp_ptr->flags & CF_SHROOM_MAGIC)
			{
				message(MSG_EFFECT, 0, "As you eat the mushroom, you call on its Mother Spirit to help you.");
				if (heal_player(30, 30)) *obvious = TRUE;
				if (set_cut(0)) *obvious = TRUE;
			}
			else
			{
				if (heal_player(15, 20)) *obvious = TRUE;
				if (set_cut((p_ptr->cut / 2) - 35)) *obvious = TRUE;
			}
			break;
		}
		case POW_SHRSHIELD:
		{
			if (cp_ptr->flags & CF_SHROOM_MAGIC)
			{
				message(MSG_EFFECT, 0, "As you eat the mushroom, you call on its Mother Spirit to help you.");
				durat = randint(apply_sp_mod(40, mdur)) + apply_sp_mod(60, mdur);
			}
			else
			{
				durat = randint(apply_sp_mod(20, mdur)) + apply_sp_mod(30, mdur);
			}
			if (set_shield(p_ptr->shield + durat)) *obvious = TRUE;
			break;
		}
		case POW_SHRCLEAR_MIND:
		{
			if (cp_ptr->flags & CF_SHROOM_MAGIC)
			{
				message(MSG_EFFECT, 0, "As you eat the mushroom, you call on its Mother Spirit to help you.");
			}
			if (set_stun(0)) *obvious = TRUE;
			if (set_blind(0)) *obvious = TRUE;
			if (set_afraid(0)) *obvious = TRUE;
			if (set_confused(0)) *obvious = TRUE;
			break;
		}
		case POW_SHRRESTORE_STR:
		{
			if (cp_ptr->flags & CF_SHROOM_MAGIC)
			{
				message(MSG_EFFECT, 0, "As you eat the mushroom, you call on its Mother Spirit to help you.");
			}
			if (do_res_stat(A_STR)) *obvious = TRUE;
			break;
		}
		case POW_SHRRESTORE_CON:
		{
			if (cp_ptr->flags & CF_SHROOM_MAGIC)
			{
				message(MSG_EFFECT, 0, "As you eat the mushroom, you call on its Mother Spirit to help you.");
			}
			if (do_res_stat(A_CON)) *obvious = TRUE;
			break;
		}
		case POW_SHRRESTORE_DEX:
		{
			if (cp_ptr->flags & CF_SHROOM_MAGIC)
			{
				message(MSG_EFFECT, 0, "As you eat the mushroom, you call on its Mother Spirit to help you.");
			}
			if (do_res_stat(A_DEX)) *obvious = TRUE;
			break;
		}
		case POW_SHRRESTORE_STATS:
		{
			if (cp_ptr->flags & CF_SHROOM_MAGIC)
			{
				message(MSG_EFFECT, 0, "As you eat the mushroom, you call on its Mother Spirit to help you.");
			}
			if (do_res_stat(A_STR)) *obvious = TRUE;
			if (do_res_stat(A_INT)) *obvious = TRUE;
			if (do_res_stat(A_WIS)) *obvious = TRUE;
			if (do_res_stat(A_DEX)) *obvious = TRUE;
			if (do_res_stat(A_CON)) *obvious = TRUE;
			if (do_res_stat(A_CHR)) *obvious = TRUE;
			break;
		}
		case POW_PHLOGISTON:
		{
			phlogiston();
			*obvious = TRUE;
			break;
		}
	}

	return (TRUE);
}
