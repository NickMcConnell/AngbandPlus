/* File: x-spell.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */


#include "angband.h"
#include "script.h"


/*
 * The defines below must match the spell numbers in spell.txt
 * if they don't, "interesting" things will probably happen.
 *
 * It would be nice if we could get rid of this dependency.
 */
 
      /* Wizardry spells (finished) */
/* Magic for Beginners 0*/
#define SPELL_MAGIC_MISSILE             0
#define SPELL_DETECT_MONSTERS           1
#define SPELL_PHASE_DOOR                2
#define SPELL_LIGHT_AREA                3
#define SPELL_FIND_TRAPS_DOORS          4
#define SPELL_CURE_LIGHT_WOUNDS         5
#define SPELL_WIZARD_LOCK               7
#define SPELL_STINKING_CLOUD            11

/* Conjurings and Tricks 1*/
#define SPELL_CONFUSE_MONSTER           13
#define SPELL_LIGHTNING_BOLT            12
#define SPELL_TRAP_DOOR_DESTRUCTION     19
#define SPELL_CURE_POISON               25
#define SPELL_SLEEP_MONSTER             14
#define SPELL_TELEPORT_SELF             30
#define SPELL_SPEAR_OF_LIGHT            20
#define SPELL_FROST_BOLT                16
#define SPELL_WONDER                    15

/* Incantations and Illusions 2*/
#define SPELL_SATISFY_HUNGER            26
#define SPELL_RECHARGE_ITEM_I           50
#define SPELL_TURN_STONE_TO_MUD         21
#define SPELL_FIRE_BOLT                 18
#define SPELL_POLYMORPH_OTHER           35
#define SPELL_IDENTIFY                  8
#define SPELL_DETECT_INVISIBLE          9
#define SPELL_ACID_BOLT                 17
#define SPELL_SLOW_MONSTER              31

/* Sorcery and Evocations 3*/
#define SPELL_FROST_BALL                55
#define SPELL_MASS_SLEEP                39
#define SPELL_HASTE_SELF                29
#define SPELL_TELEPORT_OTHER            32
#define SPELL_FIRE_BALL                 57
#define SPELL_DETECT_ENCHANTMENT        10

/* Resistances of Scarabtarices 4*/
#define SPELL_RESIST_COLD               44
#define SPELL_RESIST_FIRE               45
#define SPELL_RESIST_POISON             46
#define SPELL_RESISTANCE                47
#define SPELL_SHIELD                    48

/* Raal's Tome of Destruction 5*/
#define SPELL_EXPLOSION                 37
#define SPELL_CLOUD_KILL                38
#define SPELL_ICE_STORM                 58
#define SPELL_ACID_BALL                 56
#define SPELL_METEOR_SWARM              60
#define SPELL_RIFT                      62
#define SPELL_SHOCK_WAVE                36

/* Mordenkainen's Escapes 6*/
#define SPELL_DOOR_CREATION             22
#define SPELL_EARTHQUAKE                23
#define SPELL_STAIR_CREATION            24
#define SPELL_TELEPORT_LEVEL            33
#define SPELL_WORD_OF_RECALL            34
#define SPELL_RUNE_OF_PROTECTION        49

/* Enchantments Of War 7*/
#define SPELL_HEROISM                   27
#define SPELL_BERSERKER                 28
#define SPELL_MOLTEN_LIGHTNING           6
#define SPELL_TSUNAMI                   64
#define SPELL_ENCHANT_WEAPON            52
#define SPELL_ENCHANT_ARMOR             51
#define SPELL_ELEMENTAL_BRAND           54

/* Kelek's Grimoire of Power 8*/
#define SPELL_BEDLAM                    40
#define SPELL_REND_SOUL                 41
#define SPELL_WORD_OF_DESTRUCTION       42
#define SPELL_RECHARGE_ITEM_II          53
#define SPELL_BANISHMENT                59
#define SPELL_CHAOS_STRIKE              43
#define SPELL_MASS_BANISHMENT           61
#define SPELL_MANA_STORM                63

        /* Prayers: */
/* Minor Healings and Defences 0*/
#define PRAYER_CURE_LIGHT_WOUNDS        0
#define PRAYER_DETECT_EVIL              1
#define PRAYER_BLESS                    2
#define PRAYER_BOLDNESS                 3
#define PRAYER_CALL_LIGHT               4
#define PRAYER_SLOW_POISON              5
#define PRAYER_CURE_SERIOUS_WOUNDS      6
#define PRAYER_SPEAR_OF_LIGHT           7
#define PRAYER_SATISFY_HUNGER           8

/* Godly Insights 1*/
#define PRAYER_FIND_TRAPS               9
#define PRAYER_DETECT_DOORS_STAIRS     10
#define PRAYER_DETECT_LIFE             11
#define PRAYER_REMOVE_CURSE            12
#define PRAYER_SENSE_INVISIBLE         13
#define PRAYER_SENSE_SURROUNDINGS      14
#define PRAYER_PERCEPTION              15
#define PRAYER_WORD_OF_RECALL          16
#define PRAYER_TRUE_SIGHT              17

/* Defences of Light 2*/
#define PRAYER_SCARE_MONSTER           19
#define PRAYER_NEUTRALIZE_POISON       18
#define PRAYER_RESIST_HEAT_COLD        20
#define PRAYER_CHANT                   22
#define PRAYER_SANCTUARY               24
#define PRAYER_CURE_CRITICAL_WOUNDS    21
#define PRAYER_PORTAL                  23
#define PRAYER_PROTECTION_FROM_EVIL    25
#define PRAYER_HEAL                    26

/* Exorcism and Dispelling 3*/
#define PRAYER_TURN_UNDEAD             27
#define PRAYER_CURE_MORTAL_WOUNDS      28
#define PRAYER_ORB_OF_DRAINING         30
#define PRAYER_PRAYER                  29
#define PRAYER_DISPEL_UNDEAD           31
#define PRAYER_DISPEL_EVIL             32
#define PRAYER_HOLY_WORD               33

/* Purifications and Healing 4*/
#define PRAYER_CURE_SERIOUS_WOUNDS2    34
#define PRAYER_CURE_MORTAL_WOUNDS2     35
#define PRAYER_CURING                  36
#define PRAYER_PURIFY                  37
#define PRAYER_DISPEL_CURSE            38
#define PRAYER_HEALING                 39
#define PRAYER_RESTORATION             40
#define PRAYER_REMEMBRANCE             41

/* Holy Infusions 5*/
#define PRAYER_UNBARRING_WAYS          42
#define PRAYER_RECHARGING              43
#define PRAYER_STARLITE                44
#define PRAYER_HASTE                   45
#define PRAYER_ENCHANT_ARMOUR          46
#define PRAYER_ENCHANT_WEAPON          47
#define PRAYER_SANCTIFY_BATTLE         48
#define PRAYER_RESISTANCE              49

/* Advanced Defences of Light 6*/
#define PRAYER_MASS_SLEEP              50
#define PRAYER_PRAYER_HO               51
#define PRAYER_TELEPORT_OTHER          52
#define PRAYER_DISPEL_SILVER           53
#define PRAYER_PROTECTION_FROM_EVIL2   54
#define PRAYER_BANISH_EVIL             55
#define PRAYER_GLYPH_OF_WARDING        56

/* Holy Words of Wisdom 7*/
#define PRAYER_BLINK                   57
#define PRAYER_DETECTION               58
#define PRAYER_FULL_IDENTIFY           59
#define PRAYER_WORD_OF_DESTRUCTION     60
#define PRAYER_TELEPATHY               61
#define PRAYER_ALTER_REALITY           62

/* Wrath of God 8*/
#define PRAYER_EARTHQUAKE              63
#define PRAYER_HOLY_LANCE              64
#define PRAYER_DISPEL_SILVER2          65
#define PRAYER_DISPEL_UNDEAD2          66
#define PRAYER_DISPEL_EVIL2            67
#define PRAYER_WORD_OF_DESTRUCTION2    68
#define PRAYER_ANNIHILATION            69

        /* Nature: */
/* Call of the Wild 0*/
#define NEWM_DETECT_ANIMAL              0
#define NEWM_CALL_LIGHT                 1
#define NEWM_SLOW_POISON                2
#define NEWM_PHASE_DOOR                 3
#define NEWM_SATISFY_HUNGER             4
#define NEWM_SPARK                      5
#define NEWM_SPEAR_OF_LIGHT             6
#define NEWM_TRAP_DOOR_DESTRUCTION      7
#define NEWM_TURN_STONE_TO_MUD          8

/* Communion with Nature 1*/
#define NEWM_NEUTRALIZE_POISON          9
#define NEWM_FROST_BOLT                10
#define NEWM_SLEEP_MONSTER             11
#define NEWM_SCARE_MONSTER             12
#define NEWM_FIND_TRAPS_DOORS          13
#define NEWM_STUN_MONSTER              14
#define NEWM_BUG_SPRAY                 15
#define NEWM_FIRE_BOLT                 16
#define NEWM_REMOVE_CURSE              17

/* Gifts of Nature 2*/
#define NEWM_RESIST_HEAT_COLD          18
#define NEWM_RESIST_ACID_ELEC          19
#define NEWM_DETECT_LIFE               20
#define NEWM_NATURAL_VITALITY          21
#define NEWM_RESIST_POISON             22
#define NEWM_EARTHQUAKE                23
#define NEWM_IDENTIFY                  24
#define NEWM_TELEPORT_OTHER            25

/* Druidic Lore 3*/
#define NEWM_DETECT_EVIL               26
#define NEWM_SONG_SCARE                27
#define NEWM_HERBAL_HEALING            28
#define NEWM_SENSE_INVISIBLE           29
#define NEWM_SENSE_SURROUNDINGS        30
#define NEWM_TRUE_SIGHT                31

/* Natural Combat 4*/
#define NEWM_NYMPH_BLESSING            32
#define NEWM_WITHER_FOE                33
#define NEWM_ELEC_STORM                34
#define NEWM_THUNDERCLAP               35
#define NEWM_BLIZZARD                  36

/* Primal Forces 5 (maybe should be book 6)*/
#define NEWM_STARLITE                  37
#define NEWM_HURLED_STAR1              38
#define NEWM_TSUNAMI                   39
#define NEWM_MOLTEN_LIGHTNING          40
#define NEWM_VOLCANIC_ERUPTION         41
#define NEWM_RIFT                      42

/* Bombadil's Songs 6*/
#define NEWM_SONG_LULLING              43
#define NEWM_SONG_HEROISM              44
#define NEWM_CALL_HELP                 45 // do this one later
#define NEWM_SONG_PROTECTION           46
#define NEWM_SONG_DISPELLING           47
#define NEWM_WARDING                   48
#define NEWM_RENEWAL                   49

/* Spirits of Nature 7*/
#define NEWM_ESSENCE_SPEED             50
#define NEWM_INFUSION                  51
#define NEWM_NATURE_BLESSING           52
#define NEWM_TELEPORT_LEVEL            53
#define NEWM_BANISH_UNNATURAL          54
#define NEWM_REMEMBRANCE               55
#define NEWM_ELEMENTAL_BRAND           56
#define NEWM_TELEPATHY                 57 // done up to here

/* Power of Enlightenment 8(mystics only, do later) */
#define NEWM_CHAOS_STRIKE              58
#define NEWM_BANISHMENT                59
#define NEWM_DISPEL_SILVER             60
#define NEWM_HURLED_STAR2              61
#define NEWM_MASS_BANISHMENT           62
#define NEWM_BR_FEAR_CONF              63
#define NEWM_BR_INER_STUN              64
#define NEWM_BR_BALANCE                65
#define NEWM_FULL_IDENTIFY             66

        /* Chance Magic */
/* Tourism & Travel 0*/
#define LUCK_ILLUMINATION              0
#define LUCK_DETECT_DOORS_STAIRS       1
#define LUCK_DETECT_TREASURE           2
#define LUCK_TRAP_DOOR_DESTRUCTION     3
#define LUCK_CAMERA_FLASH              4
#define LUCK_IDENTIFY                  5
#define LUCK_DOOR_CREATION             6
#define LUCK_WORD_OF_RECALL            7
#define LUCK_STAIR_CREATION            8

/* Minor Charms for Vacationers 1*/
#define LUCK_CURE_LIGHT_WOUNDS         9
#define LUCK_EXTRA_LUCK                10
#define LUCK_RESIST_COLD               11
#define LUCK_RECHARGING                12
#define LUCK_BUG_SPRAY                 13
#define LUCK_DETECT_INVISIBLE          14
#define LUCK_RESIST_POISON             15
#define LUCK_CURE_CAUSE                16
#define LUCK_WONDER                    17

/* Tricks & Detections 2(not for tourists)*/
#define LUCK_BLINK                     18
#define LUCK_CONFUSE_MONSTER           19
#define LUCK_DETECT_MONSTERS           20
#define LUCK_DETECT_TRAPS_OBJ          21
#define LUCK_SLOW_MONSTER              22
#define LUCK_MINI_DESTRUCTION          23
#define LUCK_TELEKINESIS               24 // do this one later
#define LUCK_SENSE_SURROUNDINGS        25
#define LUCK_DETECT_ENCHANTMENT        26

/* Guidebook for Tourists 3(only for tourists)*/
#define LUCK_DETECT_TRAPS              27
#define LUCK_DETECT_OBJECTS            28
#define LUCK_MAP_AREA                  29
#define LUCK_ADJUST_CURSE              30
#define LUCK_TELEKINESIS2              31 // do this one later
#define LUCK_DETECT_ENCHANTMENT2       32
#define LUCK_PROBING                   33
#define LUCK_MAP_LEVEL                 34
#define LUCK_FULL_IDENTIFY             35

/* Quick Getaways 4(not for tourists)*/
#define LUCK_HIT_N_RUN                 36
#define LUCK_TELEPORT_SELF             37
#define LUCK_TELEPORT_OTHER            38
#define LUCK_SLOW_MONSTERS             39
#define LUCK_EARTHQUAKE                40
#define LUCK_MASS_SLEEP                41
#define LUCK_SLIP_INTO_SHADOWS         42
#define LUCK_HASTE_SELF                43

/* The Lottery 5(not for rogues or escapists)*/
#define LUCK_POLYMORPH_OTHER           44
#define LUCK_ADJUST_SPEED              45
#define LUCK_BANISH_SUMMON             46 // done up to here
#define LUCK_AFFECT_SELF               47 // finish this book later
#define LUCK_AFFECT_OTHER              48
#define LUCK_POTLUCK_STATS             49
#define LUCK_AQUIREMENT                50

/* Mordenkainen's Escapes: RE 6(mostly not for tourists)*/
#define LUCK_BLINK_MONSTER             51
#define LUCK_DOOR_CREATION2            52
#define LUCK_STAIR_CREATION2           53
#define LUCK_TELEPORT_LEVEL            54
#define LUCK_WORD_OF_DESTRUCTION       55
#define LUCK_WORD_OF_RECALL2           56
#define LUCK_RUNE_OF_PROTECTION        57 // this spellbook all done

/* Rogue's Defences 7(not for tourists)*/
#define LUCK_RESIST_HEAT_ELEC          58
#define LUCK_TURN_UNDEAD               59
#define LUCK_BURST_OF_LIGHT            60
#define LUCK_CURING                    61
#define LUCK_REMOVE_CURSE              62
#define LUCK_RESTORE_CLEVERNESS        63
#define LUCK_RESISTANCE                64 // this spellbook all done

/* The Spreading of Chaos 8*/
#define LUCK_STINK                     65 // this one done
#define LUCK_BERSERKER                 66 // this one done
#define LUCK_BEDLAM                    67 // this one done
#define LUCK_CHAOS_STRIKE              68 // this one done
#define LUCK_RIFT                      69 // this one done
#define LUCK_BREATHE_CHAOS             70
#define LUCK_MASS_CHAOS                71 // this one done
#define LUCK_BIZZARE_EFFECTS           72

int get_spell_index(const object_type *o_ptr, int index)
{
	int realm, spell;
	int sval = o_ptr->sval;

	/* Check bounds */
	if ((index < 0) || (index >= SPELLS_PER_BOOK)) return -1;
	if ((sval < 0) || (sval >= BOOKS_PER_REALM)) return -1;

	/* Which spell school? */
	if (cp_ptr->spell_book == TV_MAGIC_BOOK)
		realm = 0;
	else if (cp_ptr->spell_book == TV_NEWM_BOOK)
		realm = 2;
	else if (cp_ptr->spell_book == TV_LUCK_BOOK)
		realm = 3;
	else
		realm = 1;

	/* Get the spell */
	spell = spell_list[realm][sval][index];
	if (spell == -1) return -1;

	return s_info[spell].spell_index;
}


cptr get_spell_name(int tval, int spell)
{
	if (tval == TV_MAGIC_BOOK)
		return s_name + s_info[spell].name;
	else if (tval == TV_NEWM_BOOK)
		return s_name + s_info[spell + (PY_MAX_SPELLS * 2)].name;
	else if (tval == TV_LUCK_BOOK)
		return s_name + s_info[spell + (PY_MAX_SPELLS * 3)].name;
	else
		return s_name + s_info[spell + PY_MAX_SPELLS].name;
}


void get_spell_info(int tval, int spell, char *p, size_t len)
{
	/* Blank 'p' first */
	p[0] = '\0';

	/* Mage spells */
	if (tval == TV_MAGIC_BOOK)
	{
		int plev = p_ptr->lev;

		/* Analyze the spell */
		switch (spell)
		{
		case SPELL_MAGIC_MISSILE:
			strnfmt(p, len, " dam %dd4", 3 + ((plev - 1) / 5));
			break;
		case SPELL_PHASE_DOOR:
			strnfmt(p, len, " range 10");
			break;
		case SPELL_LIGHT_AREA:
			strnfmt(p, len, " dam 2d%d", (plev / 2));
			break; 
		case SPELL_CURE_LIGHT_WOUNDS:
			strnfmt(p, len, " heal 2d8");
			break;
		case SPELL_STINKING_CLOUD:
			strnfmt(p, len, " dam %d", 10 + (plev / 2));
			break;
		case SPELL_LIGHTNING_BOLT:
			strnfmt(p, len, " dam %dd6", (3 + ((plev - 5) / 6)));
			break;
        case SPELL_MOLTEN_LIGHTNING:
			strnfmt(p, len, " dam %d + d%d", (plev*2)+50, (plev*4));
			break;
		case SPELL_FROST_BOLT:
			strnfmt(p, len, " dam %dd8", (5 + ((plev - 5) / 4)));
			break;
		case SPELL_ACID_BOLT:
			strnfmt(p, len, " dam %dd8", (8 + ((plev - 5) / 4)));
			break;
		case SPELL_FIRE_BOLT:
			strnfmt(p, len, " dam %dd8", (6 + ((plev - 5) / 4)));
			break;
		case SPELL_SPEAR_OF_LIGHT:
			strnfmt(p, len, " dam 6d8");
			break;
		case SPELL_HEROISM:
			strnfmt(p, len, " dur 25+d25");
			break;
		case SPELL_BERSERKER:
			strnfmt(p, len, " dur 25+d25");
			break;
		case SPELL_HASTE_SELF:
			strnfmt(p, len, " dur %d+d20", plev);
			break;
		case SPELL_TELEPORT_SELF:
			strnfmt(p, len, " range %d", plev * 5);
			break;
		case SPELL_SHOCK_WAVE:
			strnfmt(p, len, " dam %d", 10 + plev);
			break;
		case SPELL_EXPLOSION:
			strnfmt(p, len, " dam %d", 20 + plev * 2);
			break;
		case SPELL_CLOUD_KILL:
			strnfmt(p, len, " dam %d", 40 + (plev / 2));
			break;
		case SPELL_REND_SOUL:
			strnfmt(p, len, " dam 11d%d", plev);
			break;
		case SPELL_RESIST_COLD:
			strnfmt(p, len, " dur 20+d20");
			break;
		case SPELL_RESIST_FIRE:
			strnfmt(p, len, " dur 20+d20");
			break;
		case SPELL_RESIST_POISON:
			strnfmt(p, len, " dur 20+d20");
			break;
		case SPELL_RESISTANCE:
			strnfmt(p, len, " dur 20+d20");
			break;
		case SPELL_SHIELD:
			strnfmt(p, len, " dur 30+d20");
			break;
		case SPELL_FROST_BALL:
			strnfmt(p, len, " dam %d", 30 + plev);
			break;
		case SPELL_ACID_BALL:
			strnfmt(p, len, " dam %d", 40 + plev);
			break;
		case SPELL_FIRE_BALL:
			strnfmt(p, len, " dam %d", 55 + plev);
			break;
		case SPELL_ICE_STORM:
			strnfmt(p, len, " dam %d", 50 + (plev * 2));
			break;
		case SPELL_TSUNAMI: 
			strnfmt(p, len, " dam %d + d%d", (plev * 3), plev);
			break;
		case SPELL_METEOR_SWARM:
			strnfmt(p, len, " dam %dx%d", 30 + plev / 2, 2 + plev / 20);
			break;
		case SPELL_CHAOS_STRIKE:
			strnfmt(p, len, " dam 13d%d", plev);
			break;
		case SPELL_RIFT:
			strnfmt(p, len, " dam 40+%dd7", plev);
			break;
		case SPELL_MANA_STORM:
			strnfmt(p, len, " dam %d", 300 + plev * 2);
			break;
		}
	}

	/* Priest spells */
	if (tval == TV_PRAYER_BOOK)
	{
		int plev = p_ptr->lev;

		/* Analyze the spell */
		switch (spell)
		{
			case PRAYER_CURE_LIGHT_WOUNDS:
				my_strcpy(p, " heal 2d10", len);
				break;
			case PRAYER_BLESS:
				my_strcpy(p, " dur 12+d12", len);
				break;
			case PRAYER_CALL_LIGHT:
				strnfmt(p, len, " dam 2d%d", (plev / 2));
				break; 
			case PRAYER_PORTAL:
				strnfmt(p, len, " range %d", 3 * plev);
				break;
			case PRAYER_CURE_SERIOUS_WOUNDS:
				my_strcpy(p, " heal 4d10", len);
				break;
			case PRAYER_CHANT:
				my_strcpy(p, " dur 24+d24", len);
				break;
			case PRAYER_RESIST_HEAT_COLD:
				my_strcpy(p, " dur 10+d10", len);
				break;
			case PRAYER_ORB_OF_DRAINING:
				strnfmt(p, len, " %d+3d6", plev +
				        (plev / ((cp_ptr->flags & CF_BLESS_WEAPON) ? 2 : 4)));
				break;
			case PRAYER_HOLY_LANCE:
				strnfmt(p, len, " %d+d%d+2d7", plev, plev/2);
				break;
		    case PRAYER_RESISTANCE:
			     strnfmt(p, len, " dur 20+d20");
			     break;
			case PRAYER_CURE_CRITICAL_WOUNDS:
				my_strcpy(p, " heal 6d10", len);
				break;
			case PRAYER_SENSE_INVISIBLE:
				my_strcpy(p, " dur 24+d24", len);
				break;
			case PRAYER_TRUE_SIGHT:
				my_strcpy(p, " dur 35+d35", len);
				break;
			case PRAYER_PROTECTION_FROM_EVIL:
				strnfmt(p, len, " dur %d+d25", 3 * plev);
				break;
			case PRAYER_PROTECTION_FROM_EVIL2:
				strnfmt(p, len, " dur %d+d25", plev / 2);
				break;
			case PRAYER_CURE_MORTAL_WOUNDS:
				my_strcpy(p, " heal 8d10", len);
				break;
			case PRAYER_PRAYER:
				my_strcpy(p, " dur 48+d48", len);
				break;
			case PRAYER_PRAYER_HO:
				my_strcpy(p, " dur 48+d48", len);
				break;
			case PRAYER_DISPEL_UNDEAD:
				strnfmt(p, len, " dam d%d", 3 * plev);
				break;
			case PRAYER_DISPEL_SILVER:
				strnfmt(p, len, " dam d%d", (3 * plev)-1);
				break;
			case PRAYER_HEAL:
				my_strcpy(p, " heal 300", len);
				break;
			case PRAYER_DISPEL_EVIL:
				strnfmt(p, len, " dam d%d", 3 * plev);
				break;
			case PRAYER_HOLY_WORD:
				my_strcpy(p, " heal 1000", len);
				break;
			case PRAYER_CURE_SERIOUS_WOUNDS2:
				my_strcpy(p, " heal 4d10", len);
				break;
			case PRAYER_CURE_MORTAL_WOUNDS2:
				my_strcpy(p, " heal 8d10", len);
				break;
			case PRAYER_HEALING:
				my_strcpy(p, " heal 2000", len);
				break;
			case PRAYER_DISPEL_SILVER2:
				strnfmt(p, len, " dam d%d", (4 * plev)-1);
				break;
			case PRAYER_DISPEL_UNDEAD2:
				strnfmt(p, len, " dam d%d", 4 * plev);
				break;
			case PRAYER_DISPEL_EVIL2:
				strnfmt(p, len, " dam d%d", 4 * plev);
				break;
			case PRAYER_ANNIHILATION:
				my_strcpy(p, " dam 200", len);
				break;
			case PRAYER_BLINK:
				my_strcpy(p, " range 10", len);
				break;
	   	    case PRAYER_SPEAR_OF_LIGHT:
		        strnfmt(p, len, " dam 6d8");
			    break;
	   	    case PRAYER_STARLITE:
			    strnfmt(p, len, " dam 6d8");
			    break;
            case PRAYER_TELEPATHY:
				my_strcpy(p, " dur clvl +dclvl", len);
				break;
		}
	}

	/* Nature spells */
	if (tval == TV_NEWM_BOOK)
	{
		int plev = p_ptr->lev;

		/* Analyze the spell */
		switch (spell)
		{
			case NEWM_CALL_LIGHT:
				strnfmt(p, len, " dam 2d%d", (plev / 2));
				break; 
		    case NEWM_PHASE_DOOR:
			    strnfmt(p, len, " range 10");
			    break;
		    case NEWM_SPARK:
			    strnfmt(p, len, " dam %dd4", (2 + ((plev - 5) / 5)));
			    break;
		    case NEWM_SPEAR_OF_LIGHT:
			    strnfmt(p, len, " dam 6d8");
			    break;
		    case NEWM_FROST_BOLT:
			    strnfmt(p, len, " dam %dd8", (5 + ((plev - 5) / 4)));
			    break;
	   	    case NEWM_STUN_MONSTER:
			    strnfmt(p, len, " dam %d", plev / 8);
			    break;
		    case NEWM_BUG_SPRAY:
			    strnfmt(p, len, " dam 1 + d%d", plev / 2);
			    break;
		    case NEWM_FIRE_BOLT:
			    strnfmt(p, len, " dam %dd8", (6 + ((plev - 5) / 4)));
			    break;
			case NEWM_RESIST_HEAT_COLD:
				my_strcpy(p, " dur 12+d12", len);
				break;
			case NEWM_RESIST_ACID_ELEC:
				my_strcpy(p, " dur 12+d12", len);
				break;
			case NEWM_NATURAL_VITALITY:
				my_strcpy(p, " heal 3d(plev/4)", len);
				break;
		    case NEWM_RESIST_POISON:
			    strnfmt(p, len, " dur 14+d14");
			    break;
			case NEWM_SENSE_INVISIBLE:
				my_strcpy(p, " dur 24+d24", len);
				break;
			case NEWM_TRUE_SIGHT:
				my_strcpy(p, " dur 35+d35", len);
				break;
			case NEWM_NYMPH_BLESSING: /* (just like chant) */
				my_strcpy(p, " dur 24+d24", len);
				break;
		    case NEWM_WITHER_FOE:
			    strnfmt(p, len, " dam %dd8", plev / 7);
			    break;
		    case NEWM_ELEC_STORM:
			    strnfmt(p, len, " dam %d + d%d", plev+20, (plev*2));
			    break;
		    case NEWM_THUNDERCLAP:
			    strnfmt(p, len, " dam %d + d%d", plev, (plev*2));
			    break;
		    case NEWM_BLIZZARD:
			    strnfmt(p, len, " dam %d + d%d", plev+10, ((plev*10)/3));
			    break;
		    case NEWM_STARLITE:
			    strnfmt(p, len, " dam 10d8");
			    break;
		    case NEWM_HURLED_STAR1:
			    strnfmt(p, len, " total dam (clvl*2)+25 + d(clvl*2)");
			    break;
		    case NEWM_TSUNAMI: 
			    strnfmt(p, len, " dam %d + d%d", (plev * 3), plev);
			    break;
            case NEWM_MOLTEN_LIGHTNING:
			    strnfmt(p, len, " dam %d + d%d", (plev*2)+50, (plev*4));
			    break;
            case NEWM_VOLCANIC_ERUPTION:
			    strnfmt(p, len, " dam %d + d%d", (plev*3)+50, ((plev*10)/3));
			    break;
            case NEWM_RIFT:
			    strnfmt(p, len, " dam %d + %dd6", (plev*2)+10, plev);
			    break;
		    case NEWM_SONG_HEROISM:
			    strnfmt(p, len, " dur 25+d25");
			    break;
		    case NEWM_SONG_PROTECTION:
			    strnfmt(p, len, " dur 25+%d", (plev*2));
			    break;
            case NEWM_SONG_DISPELLING:
			    strnfmt(p, len, " dam d%d + d%d", ((plev*7)/3), plev);
			    break;
		    case NEWM_ESSENCE_SPEED:
			    strnfmt(p, len, " dur %d+d20", plev);
			    break;
		    case NEWM_TELEPATHY:
				my_strcpy(p, " dur clvl +dclvl", len);
				break;
		}
	}

	/* Chance spells */
	if (tval == TV_LUCK_BOOK)
	{
		int plev = p_ptr->lev;

		/* Analyze the spell */
		switch (spell)
		{
            case LUCK_ILLUMINATION:
				strnfmt(p, len, " dam 1d%d", (plev / 2));
				break; 
		    case LUCK_CAMERA_FLASH:
			    strnfmt(p, len, " total dam 4 + d(clvl/2) + d(clvl/5)");
				break;
		    case LUCK_HIT_N_RUN:
				strnfmt(p, len, " dam 2d%d", (plev / 3) + 1);
				break;
		    case LUCK_BURST_OF_LIGHT:
			    strnfmt(p, len, " dam 6d8");
			    break;
		    case LUCK_RESISTANCE:
			    strnfmt(p, len, " dur 20+d20");
			    break;
		    case LUCK_BERSERKER:
			    strnfmt(p, len, " dur 25+d25");
			    break;
		    case LUCK_CHAOS_STRIKE:
			    strnfmt(p, len, " dam %dd7", plev);
			    break;
		    case LUCK_RIFT:
			    strnfmt(p, len, " dam 40+%dd7", plev);
			    break;
		}
	}
	
	return;
}



static int beam_chance(void)
{
	int plev = p_ptr->lev;
	return ((cp_ptr->flags & CF_BEAM) ? plev : (plev / 2));
}


static void spell_wonder(int dir)
{
/* This spell should become more useful (more
   controlled) as the player gains experience levels.
   Thus, add 1/5 of the player's level to the die roll.
   This eliminates the worst effects later on, while
   keeping the results quite random.  It also allows
   some potent effects only at high level. */

	int py = p_ptr->py;
	int px = p_ptr->px;
	int plev = p_ptr->lev;
	int die = randint(100) + plev / 5;
	int beam = beam_chance();

	if (die > 100)
		msg_print("You feel a surge of power!");
	if (die < 8) clone_monster(dir);
	else if (die < 14) speed_monster(dir);
	else if (die < 26) heal_monster(dir);
	else if (die < 31) poly_monster(dir);
	else if (die < 36)
		fire_bolt_or_beam(beam - 10, GF_MISSILE, dir,
		                  damroll(3 + ((plev - 1) / 5), 4));
	else if (die < 41) confuse_monster(dir, plev);
	else if (die < 46) fire_ball(GF_POIS, dir, 20 + (plev / 2), 3);
	else if (die < 51) lite_line(dir);
	else if (die < 56)
		fire_beam(GF_ELEC, dir, damroll(3+((plev-5)/6), 6));
	else if (die < 61)
		fire_bolt_or_beam(beam-10, GF_COLD, dir,
		                  damroll(5+((plev-5)/4), 8));
	else if (die < 66)
		fire_bolt_or_beam(beam, GF_ACID, dir,
		                  damroll(6+((plev-5)/4), 8));
	else if (die < 71)
		fire_bolt_or_beam(beam, GF_FIRE, dir,
		                  damroll(8+((plev-5)/4), 8));
	else if (die < 76) drain_life(dir, 75);
	else if (die < 81) fire_ball(GF_ELEC, dir, 30 + plev / 2, 2);
	else if (die < 86) fire_ball(GF_ACID, dir, 40 + plev, 2);
	else if (die < 91) fire_ball(GF_ICE, dir, 70 + plev, 3);
	else if (die < 96) fire_ball(GF_FIRE, dir, 80 + plev, 3);
	else if (die < 101) drain_life(dir, 100 + plev);
	else if (die < 104) earthquake(py, px, 12);
	else if (die < 106) destroy_area(py, px, 15, TRUE);
	else if (die < 108) banishment();
	else if (die < 110) dispel_monsters(120);
	else /* RARE */
	{
		dispel_monsters(150);
		slow_monsters();
		sleep_monsters();
		hp_player(300);
	}
}



static bool cast_mage_spell(int spell)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int dir;

	int plev = p_ptr->lev;

	/* Hack -- chance of "beam" instead of "bolt" */
	int beam = beam_chance();

	/* Spells. */
	switch (spell)
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

		case SPELL_CURE_LIGHT_WOUNDS:
		{

			(void)hp_player(damroll(2, 8));
			(void)dec_timed(TMD_CUT, 15);
			break;
		}

		case SPELL_WIZARD_LOCK:
		{
				if (!get_aim_dir(&dir)) return;

				/* Spell will jam a door or create a jammed door
                /* in any one empty square. */
				fire_ball(GF_WIZLOCK, dir, 0, 0);
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
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_ball(GF_POIS, dir, 10 + (plev / 2), 2);
			break;
		}

		case SPELL_CONFUSE_MONSTER:
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
        
        case SPELL_MOLTEN_LIGHTNING:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_beam(GF_PLASMA, dir,
			          50 + (2 * plev) + randint(plev * 4));
			break;
		}

		case SPELL_TRAP_DOOR_DESTRUCTION:
		{
			(void)destroy_doors_touch();
			break;
		}

		case SPELL_SLEEP_MONSTER:
		{
            if (!get_aim_dir(&dir)) return (FALSE);
			(void)sleep_monster(dir);
			break;
		}

		case SPELL_CURE_POISON:
		{
			(void)clear_timed(TMD_POISONED);
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
		}

		case SPELL_WONDER: /* wonder */
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			(void)spell_wonder(dir);
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

		case SPELL_MASS_SLEEP:
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

		case SPELL_BEDLAM:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_ball(GF_OLD_CONF, dir, plev, 4);
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

		case SPELL_BANISHMENT:
		{
			return banishment();
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

		case SPELL_TSUNAMI: 
		{
			msg_print("You hurl mighty waves at your foes!");
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_ball(GF_WATER, dir, (plev*3) + randint(plev), plev / 11);
			break;
		}

		case SPELL_ICE_STORM:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_ball(GF_ICE, dir, 50 + (plev * 2), 3);
			break;
		} // 104 at L27, 120 at L35, 150

		case SPELL_METEOR_SWARM:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_swarm(2 + plev / 20, GF_METEOR, dir, 30 + plev / 2, 1);
			break;
		}

		case SPELL_MANA_STORM:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_ball(GF_MANA, dir, 300 + (plev * 2), 3);
			break;
           /* 340 at L20, 360 at L30, 380 at L40, 400 at L50 */
		}

		case SPELL_DETECT_INVISIBLE:
		{
			(void)detect_monsters_invis();
			break;
		}

		case SPELL_DETECT_ENCHANTMENT:
		{
			(void)detect_objects_magic();
			break;
		}

		case SPELL_SHOCK_WAVE:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_ball(GF_SOUND, dir, 10 + plev, 2);
			break;
		}

		case SPELL_EXPLOSION:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_ball(GF_SHARD, dir, 20 + (plev * 2), 2);
			break;
           /* 60 at L20, 80 at L30, 100 at L40, 120 at L50 */
		}

		case SPELL_MASS_BANISHMENT:
		{
			(void)mass_banishment();
			break;
		}

		case SPELL_RESIST_FIRE:
		{
			(void)inc_timed(TMD_OPP_FIRE, randint(20) + 20);
			break;
		}

		case SPELL_RESIST_COLD:
		{
			(void)inc_timed(TMD_OPP_COLD, randint(20) + 20);
			break;
		}

		case SPELL_ELEMENTAL_BRAND: /* elemental brand melee */
		{
			brand_weapon();
			break;
		}

		case SPELL_RESIST_POISON:
		{
			(void)inc_timed(TMD_OPP_POIS, randint(20) + 20);
			break;
		}

		case SPELL_RESISTANCE:
		{
			int time = randint(20) + 20;
			(void)inc_timed(TMD_OPP_ACID, time);
			(void)inc_timed(TMD_OPP_ELEC, time);
			(void)inc_timed(TMD_OPP_FIRE, time);
			(void)inc_timed(TMD_OPP_COLD, time);
			(void)inc_timed(TMD_OPP_POIS, time);
			break;
		}

		case SPELL_HEROISM:
		{
			(void)hp_player(10);
			(void)inc_timed(TMD_HERO, randint(25) + 25);
			(void)clear_timed(TMD_AFRAID);
			break;
		}

		case SPELL_SHIELD:
		{
			(void)inc_timed(TMD_SHIELD, randint(20) + 30);
			break;
		}

		case SPELL_BERSERKER:
		{
			(void)hp_player(30);
            int die = randint(75);
	        if ((p_ptr->timed[TMD_CHARM]) && (die > (plev + 25)))
	        {
                 msg_print("You're in too good a mood to go into a battle frenzy");
            }
            else 
            {
                 (void)clear_timed(TMD_AFRAID);
			     (void)clear_timed(TMD_CHARM);
			     if (plev > 20)
			     {
			     (void)inc_timed(TMD_SHERO, randint(25) + (plev + ((76 - die) / 3)));
                 }
                 else
                 {
			     (void)inc_timed(TMD_SHERO, randint(25) + (25));
                 }
            }
			break;
		}

		case SPELL_HASTE_SELF:
		{
			if (!p_ptr->timed[TMD_FAST])
			{
				(void)set_timed(TMD_FAST, randint(20) + plev);
			}
			else
			{
				(void)inc_timed(TMD_FAST, randint(5));
			}
			break;
		}

		case SPELL_RIFT:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_beam(GF_GRAVITY, dir, 40 + damroll(plev, 7));
			break;
            /* 61-180 at L20, 71-250 at L30, 81-320 at L40, 91-390 at L50 */
		}

		case SPELL_REND_SOUL: /* rend soul */
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_bolt_or_beam(beam / 4, GF_NETHER, dir, damroll(11, plev));
			break;
		}

		case SPELL_CHAOS_STRIKE: /* chaos strike */
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_bolt_or_beam(beam, GF_CHAOS, dir, damroll(13, plev));
			break;
		}

		case SPELL_RUNE_OF_PROTECTION: /* rune of protection */
		{
			(void)warding_glyph();
			break;
		}

		case SPELL_ENCHANT_ARMOR: /* enchant armor */
		{
			return enchant_spell(0, 0, rand_int(3) + plev / 20);
		}

		case SPELL_ENCHANT_WEAPON: /* enchant weapon */
		{
			return enchant_spell(rand_int(4) + plev / 20,
			                     rand_int(4) + plev / 20, 0);
		}
	}

	/* Success */
	return (TRUE);
}


static bool cast_priest_spell(int spell)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int dir;

	int plev = p_ptr->lev;

	switch (spell)
	{
		case PRAYER_DETECT_EVIL:
		{
			(void)detect_monsters_evil();
			break;
		}
		
		case PRAYER_DETECT_LIFE:
		{
			(void)detect_monsters_life();
			break;
		}

		case PRAYER_CURE_LIGHT_WOUNDS:
		{
			(void)hp_player(damroll(2, 10));
			(void)dec_timed(TMD_CUT, 10);
			break;
		}

		case PRAYER_BLESS:
		{
			(void)inc_timed(TMD_BLESSED, randint(12) + 12);
			break;
		}

		case PRAYER_BOLDNESS:
		{
			(void)clear_timed(TMD_AFRAID);
			if (rand_int(47) < (plev + 5)) (void)clear_timed(TMD_CHARM);
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
			(void)set_timed(TMD_POISONED, p_ptr->timed[TMD_POISONED] / 2);
			break;
		}

		case PRAYER_SPEAR_OF_LIGHT: /* spear of light */
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			msg_print("A ray of white light appears.");
			lite_line(dir);
			break;
		}
		
		case PRAYER_SCARE_MONSTER:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
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
			(void)set_timed(TMD_CUT, (p_ptr->timed[TMD_CUT] / 2) - 20);
			break;
		}

		case PRAYER_CHANT:
		{
			(void)inc_timed(TMD_BLESSED, randint(24) + 24);
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
			(void)inc_timed(TMD_OPP_FIRE, randint(10) + 10);
			(void)inc_timed(TMD_OPP_COLD, randint(10) + 10);
			break;
		}

		case PRAYER_NEUTRALIZE_POISON:
		{
			(void)clear_timed(TMD_POISONED);
			break;
		}

		case PRAYER_ORB_OF_DRAINING:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_ball(GF_HOLY_ORB, dir,
			          (damroll(3, 6) + plev +
			           (plev / ((cp_ptr->flags & CF_BLESS_WEAPON) ? 2 : 4))),
			          ((plev < 30) ? 2 : 3));
			break;
			/* 28-43 at L20, 40-55 at L30, 53-68 at L40, 65-80 at L50 */
		}
		
		case PRAYER_HOLY_LANCE:
        {
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_beam(GF_HOLY_ORB, dir,
			          damroll(2, 8) + plev + randint(plev/2));
			if (randint(plev/3) > 6) lite_line(dir);
			break;
			/* 32-46 at L20, 47-61 at L30, 62-76 at L40, 77-91 at L50 */
        }

		case PRAYER_CURE_CRITICAL_WOUNDS:
		{
			(void)hp_player(damroll(6, 10));
			(void)clear_timed(TMD_CUT);
			(void)clear_timed(TMD_STUN);
			break;
		}

		case PRAYER_SENSE_INVISIBLE:
		{
			(void)inc_timed(TMD_SINVIS, randint(24) + 24);
			break;
		}
		
        case PRAYER_TRUE_SIGHT:
        {
			(void)clear_timed(TMD_BLIND);
			(void)clear_timed(TMD_IMAGE);
			(void)detect_traps();
			(void)detect_doors();
			(void)detect_stairs();
			(void)detect_monsters_evil();
			(void)detect_monsters_invis();
			(void)inc_timed(TMD_TSIGHT, randint(35) + 35);
			break;
        }

		case PRAYER_PROTECTION_FROM_EVIL:
		{
			(void)inc_timed(TMD_PROTEVIL, randint(25) + 3 * p_ptr->lev);
			break;
		}

		case PRAYER_PROTECTION_FROM_EVIL2:
		{
			(void)inc_timed(TMD_PROTEVIL2, randint(25) + (p_ptr->lev / 2));
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
			(void)clear_timed(TMD_STUN);
			(void)clear_timed(TMD_CUT);
			break;
		}

		case PRAYER_TURN_UNDEAD:
		{
			(void)turn_undead();
			break;
		}

		case PRAYER_PRAYER:
		{
			(void)inc_timed(TMD_BLESSED, randint(48) + 48);
			break;
		}

		case PRAYER_PRAYER_HO:
		{
			(void)inc_timed(TMD_BLESSED, randint(48) + 48);
			break;
		}

		case PRAYER_DISPEL_UNDEAD:
		{
			(void)dispel_undead(randint(plev * 3));
			break;
		}

		case PRAYER_DISPEL_SILVER:
		{
			(void)dispel_silver(randint((plev * 3)-1));
			break;
		}

		case PRAYER_HEAL:
		{
			(void)hp_player(300);
			(void)clear_timed(TMD_STUN);
			(void)clear_timed(TMD_CUT);
			break;
		}

		case PRAYER_DISPEL_EVIL:
		{
			(void)dispel_evil(randint(plev * 3));
			break;
		}

		case PRAYER_MASS_SLEEP:
		{
			(void)sleep_monsters();
			break;
		}

		case PRAYER_GLYPH_OF_WARDING:
		{
			warding_glyph();
			break;
		}

		case PRAYER_RESISTANCE:
		{
			int time = randint(20) + 20;
			(void)inc_timed(TMD_OPP_ACID, time);
			(void)inc_timed(TMD_OPP_ELEC, time);
			(void)inc_timed(TMD_OPP_FIRE, time);
			(void)inc_timed(TMD_OPP_COLD, time);
			(void)inc_timed(TMD_OPP_POIS, time);
			break;
		}

		case PRAYER_HOLY_WORD:
		{
			(void)dispel_evil(randint(plev * 4));
			(void)hp_player(1000);
			(void)clear_timed(TMD_AFRAID);
			(void)clear_timed(TMD_POISONED);
			(void)clear_timed(TMD_STUN);
			(void)clear_timed(TMD_CUT);
			break;
		}
		
		case PRAYER_SANCTIFY_BATTLE:
        {
			(void)clear_timed(TMD_AFRAID);
			(void)clear_timed(TMD_CHARM);
			(void)clear_timed(TMD_FRENZY);
			(void)inc_timed(TMD_SANCTIFY, randint(plev/3) + (plev/3) + 7);
        }
		
		case PRAYER_TELEPATHY:
        {
			(void)inc_timed(TMD_ESP, randint(plev) + (plev/2));
        }

		case PRAYER_DETECTION:
		{
			(void)detect_all();
			break;
		}

		case PRAYER_PERCEPTION:
		{
			return ident_spell();
		}

		case PRAYER_FULL_IDENTIFY:
		{
			return identify_fully();
		}

/*		case PRAYER_CLAIRVOYANCE:
		{
			wiz_lite();
			break;
		}  (unused now) */

		case PRAYER_CURE_SERIOUS_WOUNDS2:
		{
			(void)hp_player(damroll(4, 10));
			(void)clear_timed(TMD_CUT);
			break;
		}

		case PRAYER_CURE_MORTAL_WOUNDS2:
		{
			(void)hp_player(damroll(8, 10));
			(void)clear_timed(TMD_STUN);
			(void)clear_timed(TMD_CUT);
			break;
		}

        case PRAYER_CURING:
        {
			(void)clear_timed(TMD_CONFUSED);
			(void)clear_timed(TMD_BLIND);
			(void)clear_timed(TMD_CHARM);
			(void)clear_timed(TMD_FRENZY);
			(void)clear_timed(TMD_AMNESIA);
			(void)clear_timed(TMD_POISONED);
			(void)clear_timed(TMD_CUT);
			(void)clear_timed(TMD_STUN);
			break;
        }

        case PRAYER_PURIFY:
        {
			p_ptr->silver = PY_SILVER_HEALTHY;
			p_ptr->slime = PY_SLIME_HEALTHY;
			(void)set_timed(TMD_POISONED, p_ptr->timed[TMD_POISONED] - ((plev/4) + 1));
        }
        
		case PRAYER_HEALING:
		{
			(void)hp_player(2000);
			(void)clear_timed(TMD_STUN);
			(void)clear_timed(TMD_CUT);
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

		case PRAYER_DISPEL_SILVER2:
		{
			(void)dispel_silver(randint(plev * 4));
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

		case PRAYER_BANISH_EVIL:
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

		case PRAYER_WORD_OF_DESTRUCTION2:
		{
			destroy_area(py, px, 15, TRUE);
			break;
		}

		case PRAYER_ANNIHILATION:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
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
			return recharge(15);
		}

		case PRAYER_DISPEL_CURSE:
		{
			(void)remove_all_curse();
			break;
		}

		case PRAYER_ENCHANT_WEAPON:
		{
			return enchant_spell(rand_int(4) + 1, rand_int(4) + 1, 0);
		}

		case PRAYER_ENCHANT_ARMOUR:
		{
			return enchant_spell(0, 0, rand_int(3) + 2);
		}

		case PRAYER_STARLITE:
		{
			{
				msg_print("You shine your light in all directions.");
			}
			lite_line(1);lite_line(2);lite_line(3);lite_line(4);
            lite_line(5);lite_line(6);lite_line(7);
            lite_line(8);lite_line(9);
/*            for (k = 0; k < 8; k++) lite_line(ddd[k]);  */
			break;
		}
        
   		case PRAYER_BLINK:
		{
			teleport_player(10);
			break;
		}

		case PRAYER_TELEPORT_OTHER:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			(void)teleport_monster(dir);
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

	/* Success */
	return (TRUE);
}

static bool cast_newm_spell(int spell)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

    int die;
	int dir;

	/* Hack -- chance of "beam" instead of "bolt" */
	int beam = beam_chance();

	int plev = p_ptr->lev;

	switch (spell)
	{
		case NEWM_DETECT_ANIMAL:
		{
			(void)detect_monsters_animal();
			break;
		}

		case NEWM_CALL_LIGHT:
		{
			(void)lite_area(damroll(2, (plev / 2)), (plev / 9) + 1);
			break;
		}

		case NEWM_SLOW_POISON:
		{
			(void)set_timed(TMD_POISONED, p_ptr->timed[TMD_POISONED] / 2);
			break;
		}

		case NEWM_PHASE_DOOR:
		{
			teleport_player(10);
			break;
		}

		case NEWM_SATISFY_HUNGER:
		{
			(void)set_food(PY_FOOD_MAX - 1);
			break;
		}

		case NEWM_SPARK:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
            range = 5;
			fire_beam(GF_ELEC, dir,
			          damroll(2+((plev-5)/5), 4));
			break;
		}

		case NEWM_SPEAR_OF_LIGHT:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			msg_print("A ray of white light appears.");
			lite_line(dir);
			break;
		}
		
		case NEWM_TRAP_DOOR_DESTRUCTION:
		{
			(void)destroy_doors_touch();
			break;
		}

		case NEWM_TURN_STONE_TO_MUD:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			(void)wall_to_mud(dir);
			break;
		}
		
		case NEWM_NEUTRALIZE_POISON:
		{
			(void)clear_timed(TMD_POISONED);
			break;
		}

		case NEWM_FROST_BOLT:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_bolt_or_beam(beam-10, GF_COLD, dir,
			                  damroll(5+((plev-5)/4), 8));
			break;
		}

		case NEWM_SLEEP_MONSTER:
		{	
            if (!get_aim_dir(&dir)) return (FALSE);
			(void)sleep_monster(dir);
			break;
		}
		
		case NEWM_SCARE_MONSTER:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			(void)fear_monster(dir, plev);
			break;
		}
		
		case NEWM_FIND_TRAPS_DOORS:
		{
			(void)detect_traps();
			(void)detect_doors();
			(void)detect_stairs();
			break;
		}
		
		case NEWM_STUN_MONSTER:
        {
			if (!get_aim_dir(&dir)) return (FALSE);
			spellswitch = 9; /* keeps you from discovering water immunity */
			fire_bolt_or_beam(3, GF_WATER, dir, plev / 8);
			break;
			/* spellswitch = 9 must be after targetting or it will change */
			/* the target prompt to one for the camera flash spell */
        }
        
        case NEWM_BUG_SPRAY:
        {
			(void)dispel_bug(1 + (randint(plev / 2)));
			break;
        }

		case NEWM_FIRE_BOLT:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_bolt_or_beam(beam, GF_FIRE, dir,
			                  damroll(6+((plev-5)/4), 8));
			break;
		}

		case NEWM_REMOVE_CURSE:
		{
			remove_curse();
			break;
		}

		case NEWM_RESIST_HEAT_COLD:
		{
			(void)inc_timed(TMD_OPP_FIRE, randint(12) + 12);
			(void)inc_timed(TMD_OPP_COLD, randint(12) + 12);
			break;
		}

		case NEWM_RESIST_ACID_ELEC:
		{
			(void)inc_timed(TMD_OPP_ACID, randint(12) + 12);
			(void)inc_timed(TMD_OPP_ELEC, randint(12) + 12);
			break;
		}
		
		case NEWM_DETECT_LIFE:
		{
			(void)detect_monsters_life();
			break;
		}
		
		case NEWM_NATURAL_VITALITY:
        {
			(void)set_timed(TMD_POISONED, (3 * p_ptr->timed[TMD_POISONED] / 4) - 5);
			(void)hp_player(damroll(3, plev / 4));
			(void)clear_timed(TMD_CUT);
            break;
        }

		case NEWM_RESIST_POISON:
		{
			(void)inc_timed(TMD_OPP_POIS, randint(14) + 14);
			break;
		}

		case NEWM_EARTHQUAKE:
		{
			earthquake(py, px, 10);
			break;
		}

		case NEWM_IDENTIFY:
		{
			return ident_spell();
		}

		case NEWM_TELEPORT_OTHER:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			(void)teleport_monster(dir);
			break;
		}
		
		case NEWM_DETECT_EVIL:
		{
			(void)detect_monsters_evil();
			break;
		}
		
		case NEWM_SONG_SCARE:
		{
		    scare_monsters();
			break;
		}
		
		case NEWM_HERBAL_HEALING:
        {
			(void)clear_timed(TMD_POISONED);
			(void)clear_timed(TMD_CUT);
			(void)set_timed(TMD_STUN, p_ptr->timed[TMD_STUN] - 3);
			(void)hp_player(30 + damroll((plev * 3 / 4), 5));
			break;
        }

		case NEWM_SENSE_INVISIBLE:
		{
			(void)inc_timed(TMD_SINVIS, randint(24) + 24);
			break;
		}

		case NEWM_SENSE_SURROUNDINGS:
		{
			map_area();
			break;
		}
		
        case NEWM_TRUE_SIGHT:
        {
			(void)clear_timed(TMD_BLIND);
			(void)clear_timed(TMD_IMAGE);
			(void)detect_traps();
			(void)detect_monsters_life();
			(void)detect_monsters_invis();
			(void)inc_timed(TMD_TSIGHT, randint(35) + 35);
			break;
        }
        
        case NEWM_NYMPH_BLESSING: /* (just like chant) */
		{
			(void)inc_timed(TMD_BLESSED, randint(24) + 24);
			break;
		}
		
		case NEWM_WITHER_FOE:
        {
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_bolt(GF_MANA, dir, damroll(plev / 7, 8));
				(void)confuse_monster(dir, plev + 10);
			    (void)slow_monster(dir);
			break;
        }

		case NEWM_ELEC_STORM:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_ball(GF_ELEC, dir, 20 + plev + randint(plev * 2), (plev / 14));
			break;
           /* 41-80 at L20, 51-110 at L30, 61-140 at L40, 71-170 at L50 */
		}

		case NEWM_THUNDERCLAP:
		{
			(void)dispel_ears(plev + randint(plev * 2));
			break;
           /* 21-60 at L20, 31-90 at L30, 41-120 at L40, 51-150 at L50 */
		}

		case NEWM_BLIZZARD:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_ball(GF_COLD, dir, 10 + plev + randint((plev * 10)/3), (plev / 12));
			break;
           /* 31-96 at L20, 41-140 at L30, 51-183 at L40, 61-226 at L50 */
		}
		
		case NEWM_STARLITE:
		{
			{
				msg_print("Unbearably bright light shines in all directions.");
			}
			strong_lite_line(1);strong_lite_line(2);strong_lite_line(3);
            strong_lite_line(4);strong_lite_line(5);strong_lite_line(6);
            strong_lite_line(7);strong_lite_line(8);strong_lite_line(9);
			break;
			/* player should take a little damage if not resistant to light */
		}
		
		case NEWM_HURLED_STAR1:
        {
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_ball(GF_LITE, dir, 5 + (plev) + randint(plev), 2);
			fire_ball(GF_FIRE, dir, 20 + (plev) + randint(plev), 2);
			break;
           /* LITE: 26-45 at L20, 36-65 at L30, 46-85 at L40, 56-105 at L50 + */
           /* FIRE: 41-60 at L20, 51-80 at L30, 61-100 at L40, 71-120 at L50 */
        }

		case NEWM_TSUNAMI: 
		{
			msg_print("You hurl mighty waves at your foes!");
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_ball(GF_WATER, dir, (plev*3) + randint(plev), plev / 11);
			break;
           /* 61-80 at L20, 91-120 at L30, 121-160 at L40, 151-200 at L50 */
		}
        
        case NEWM_MOLTEN_LIGHTNING:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_beam(GF_PLASMA, dir,
			          50 + (2 * plev) + randint(plev * 4));
			break;
           /* 91-170 at L20, 111-230 at L30, 131-290 at L40, 151-350 at L50 */
		}

        case NEWM_VOLCANIC_ERUPTION:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			msg_print("The earth convulses and erupts in molten lava!");
			fire_ball(GF_PLASMA, dir, 
                      50 + (3 * plev) + randint((plev * 10)/3), 1 + (plev / 16));
            spellswitch = 11;  /* activates earthquake */
			break;
           /* 111-176 at L20, 141-240 at L30, 171-303 at L40, 201-366 at L50 */
        }

		case NEWM_RIFT:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_beam(GF_GRAVITY, dir, 10 + (plev*2) + damroll(plev, 6));
			break;
            /* 71-170 at L20, 101-250 at L30, 131-330 at L40, 161-410 at L50 */
		}

		case NEWM_SONG_LULLING:
		{
			(void)slow_monsters();
			(void)sleep_monsters();
			break;
		}

		case NEWM_SONG_HEROISM:
		{
			(void)hp_player(10);
			(void)clear_timed(TMD_AFRAID);
			(void)inc_timed(TMD_HERO, randint(25) + 25);
			break;
		}

        case NEWM_CALL_HELP: /* do this spell after making helpful monsters */
        {
			(void)hp_player(2);
			break;
        }
        
        case NEWM_SONG_PROTECTION:
        {
			(void)inc_timed(TMD_PROTEVIL, randint(25) + 2 * plev);
			(void)inc_timed(TMD_WSHIELD, randint(25) + 2 * plev);
			break;
        }
        
        case NEWM_SONG_DISPELLING:
        {
			msg_print("An unbearable discord tortures your foes!");
            spellswitch = 10;  /* a song should not affect golems */
            (void)dispel_monsters(randint((plev * 7)/3));
			(void)dispel_evil(randint(plev));
			break;
        }

		case NEWM_WARDING:
		{
			warding_glyph();
			break;
		}

		case NEWM_RENEWAL:
		{
			(void)do_res_stat(A_STR);
			(void)do_res_stat(A_INT);
			(void)do_res_stat(A_WIS);
			(void)do_res_stat(A_DEX);
			(void)do_res_stat(A_CON);
			(void)do_res_stat(A_CHR);
			break;
		}
		
		case NEWM_ESSENCE_SPEED:
		{
			if (!p_ptr->timed[TMD_FAST])
			{
				(void)set_timed(TMD_FAST, randint(20) + plev);
			}
			else
			{
				(void)inc_timed(TMD_FAST, randint(5) + 1);
			}
			break;
		}
		
		case NEWM_INFUSION:
        {
			return recharge(plev);
        }
		
		case NEWM_NATURE_BLESSING:
		{
			(void)dispel_unnatural((plev * 11)/5);
			(void)hp_player(50 + (plev*6));
			(void)inc_timed(TMD_BLESSED, randint(71) + 71);
			(void)clear_timed(TMD_BLIND);
			(void)clear_timed(TMD_CHARM);
			(void)clear_timed(TMD_FRENZY);
			(void)clear_timed(TMD_AFRAID);
			(void)clear_timed(TMD_POISONED);
			(void)clear_timed(TMD_STUN);
			(void)clear_timed(TMD_CUT);
			break;
		}

		case NEWM_TELEPORT_LEVEL:
		{
			(void)teleport_player_level();
			break;
		}

		case NEWM_BANISH_UNNATURAL:
		{
			if (banish_unnatural(100))
			{
				msg_print("All unnatural monsters are purged from the area.");
			}
			break;
		}

		case NEWM_REMEMBRANCE:
		{
			(void)restore_level();
			break;
		}

		case NEWM_ELEMENTAL_BRAND: /* brand ammo */
		{
			return brand_ammo();
		}
		
		case NEWM_TELEPATHY:
        {
			(void)inc_timed(TMD_ESP, randint(plev) + (plev/2));
        }
        		 /* HERE4: still need last spellbook */
    }

	/* Success */
	return (TRUE);
}

static bool cast_luck_spell(int spell)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

    int die;
	int dir;

	/* Hack -- chance of "beam" instead of "bolt" */
	int beam = beam_chance();

	int plev = p_ptr->lev;

	switch (spell)
	{
		case LUCK_ILLUMINATION:
		{
			(void)lite_area(damroll(1, (plev / 2)), (plev / 10) + 1);
			break;
		}
		
		case LUCK_DETECT_DOORS_STAIRS:
		{
			(void)detect_doors();
			(void)detect_stairs();
			break;
		}
		
		case LUCK_DETECT_TREASURE:
        {
			(void)detect_treasure();
			(void)detect_objects_gold();
			break;
		}

		case LUCK_TRAP_DOOR_DESTRUCTION:
		{
            die = randint(99) + (randint(plev/5));
			(void)destroy_doors_touch();
			if (die > 100)
            {
               spellswitch = 6; /* increses radius for destroy traps */
               (void)destroy_doors_touch();
               spellswitch = 0;
            }
			break;
		}
		
		case LUCK_CAMERA_FLASH:
		{
            spellswitch = 9; /* changes targetting prompt */
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_ball(GF_WATER, dir, 0, plev/23);
			fire_ball(GF_LITE, dir, 4 + randint(plev/3), plev/15);
			/* GF_WATER for stunning effect */
			/* spellswitch also keeps you from discovering water immunity */
			/* WATER must be first because spellswitch resets before the 2nd effect */
			break;
		}

		case LUCK_IDENTIFY:
		{
			return ident_spell();
		}

		case LUCK_DOOR_CREATION:
		{
            die = randint(99) + (randint(plev/4));
            if (die < 3) return (project_los(GF_WIZLOCK, plev/5));
            else if (die < 11) earthquake(py, px, 5);
            else if (die < 16) (void)destroy_doors_touch();
            else if (die < 27) 
            {
                 spellswitch = 11; /* doesn't allow big damage to player */
                 earthquake(py, px, 3);
            }
            else if (die < 48)
            {
                 spellswitch = 11; /* doesn't allow big damage to player */
                 earthquake(py, px, 3);
			     (void)door_creation();
            }
            else if (die < 100) (void)door_creation();
            else if (die < 110)
            {
                 (void)destroy_doors_touch();
			     (void)door_creation();
            }
            else  /* (die > 109) */
            {
			     (void)hp_player(damroll(5, 5));
                 (void)destroy_doors_touch();
			     (void)door_creation();
            }
            spellswitch = 0;
			break;
		}

		case LUCK_STAIR_CREATION:
		{
            die = randint(99) + (randint(plev/4));
            if (die < 8)
            {
                 trap_creation();
			     msg_print("You hear the floor shifting.");
            }
            else if (die < 13) (void)teleport_player_level();
            else if (die < 18) (void)destroy_doors_touch();
            else if (die < 28) 
            {
                 spellswitch = 11; /* doesn't allow big damage to player */
                 earthquake(py, px, 3);
            }
            else if (die < 48)
            {
			     teleport_player(10 + plev/2);
			     (void)stair_creation();
            }
            else if (die < 100) (void)stair_creation();
            else if (die < 110)
            {
                 return (project_los(GF_GRAVITY, plev/6));
			     (void)stair_creation();
            }
            else  /* (die > 109) */
            {
			     (void)hp_player(damroll(7, 7));
                 return (project_los(GF_GRAVITY, plev/6));
			     (void)stair_creation();
            }
            spellswitch = 0;
			break;
		}

		case LUCK_WORD_OF_RECALL:
		{
            die = randint(99) + (randint(plev/4));
            if (die < 90) set_recall();
            else if (die < 96)
            {
                 (void)door_creation();
                 set_recall();
            }
            else if (die < 106)
            {
                 spellswitch = 8; /* quick recall */
                 set_recall();
            }
            else
            {
                 (void)door_creation();
                 spellswitch = 8; /* quick recall */
                 set_recall();
            }
            spellswitch = 0;
			break;
		}

		case LUCK_CURE_LIGHT_WOUNDS:
		{
			(void)hp_player(damroll(2, 7) + randint(plev / 4));
			(void)dec_timed(TMD_CUT, 10);
			break;
		}
		
		case LUCK_EXTRA_LUCK:
        {
            die = randint(99) + (randint(plev/4));
            if (die < 3)
            {
               msg_print("You feel quite unlucky."); 
               summon_specific(py, px, p_ptr->depth, SUMMON_UNDEAD);
            }
            else if (die < 5)
            {
               msg_print("You feel rather unlucky."); 
               summon_specific(py, px, p_ptr->depth, 0);
            }
            else if (die < 8) aggravate_monsters(0);
            else if (die < 11)
            {
                 msg_print("You feel lucky, but the feeling passes quickly."); 
                 (void)hp_player(randint(die));
            }
            else if (die < 15) (void)inc_timed(TMD_FRENZY, randint(die) + (die - 2));
            else if (die < 30) (void)inc_timed(TMD_BLESSED, randint(die) + (die - 4));
            else if (die < 45)
            {
                 (void)hp_player(30);
                 (void)clear_timed(TMD_AFRAID);
			     (void)clear_timed(TMD_CHARM);
			     (void)inc_timed(TMD_SHERO, randint(die) + (20));
            }
            else if (die < 60)
            {
                 (void)hp_player(20);
                 (void)clear_timed(TMD_AFRAID);
			     (void)clear_timed(TMD_CHARM);
			     (void)inc_timed(TMD_HERO, randint(25) + (25));
            }
            else if (die < 75)
            {
                 (void)hp_player(plev);
                 (void)clear_timed(TMD_BLIND);
			     (void)clear_timed(TMD_POISONED);
			     (void)inc_timed(TMD_SINFRA, randint(25) + (25));
                 (void)inc_timed(TMD_BLESSED, randint(die - 5) + (25));
            }
            else if (die < 90)
            {
                 (void)hp_player((die / 2) + 10);
                 (void)clear_timed(TMD_CUT);
                 (void)clear_timed(TMD_STUN);
			     (void)clear_timed(TMD_POISONED);
                 (void)inc_timed(TMD_PROTEVIL, randint(die / 2) + (25));
                 (void)inc_timed(TMD_WSHIELD, randint(die / 2) + (25));
            }
            else if (die < 95)
            {
                 (void)hp_player(die - 21);
                 (void)clear_timed(TMD_CUT);
                 (void)clear_timed(TMD_STUN);
			     (void)clear_timed(TMD_POISONED);
			     (void)clear_timed(TMD_AFRAID);
			     (void)clear_timed(TMD_CHARM);
			     (void)clear_timed(TMD_FRENZY);
			     (void)inc_timed(TMD_SHERO, randint(die / 3) + 20);
                 (void)inc_timed(TMD_WSHIELD, randint(die / 3) + 19);
            }
            else if (die < 100)
            {
                 (void)hp_player(die - 14);
                 (void)clear_timed(TMD_CUT);
                 (void)clear_timed(TMD_STUN);
			     (void)clear_timed(TMD_POISONED);
                 (void)inc_timed(TMD_OPP_FIRE, randint(die / 3) + (25));
                 (void)inc_timed(TMD_OPP_COLD, randint(die / 3) + (25));
                 (void)inc_timed(TMD_SANCTIFY, randint(die / 3) + (25));
            }
            else if (die < 105)
            {
                 (void)hp_player(die - 7);
                 (void)clear_timed(TMD_CUT);
                 (void)clear_timed(TMD_STUN);
			     (void)clear_timed(TMD_POISONED);
                 (void)inc_timed(TMD_OPP_FIRE, randint(die / 2) + (10));
                 (void)inc_timed(TMD_OPP_COLD, randint(die / 2) + (10));
                 (void)inc_timed(TMD_OPP_ACID, randint(die / 2) + (10));
                 (void)inc_timed(TMD_OPP_ELEC, randint(die / 2) + (10));
                 (void)inc_timed(TMD_OPP_POIS, randint(die / 2) + (10));
                 (void)inc_timed(TMD_HERO, randint(die / 3) + (25));
            }
            else /* (die > 104) */
            {
                 (void)hp_player(die - 3);
                 (void)clear_timed(TMD_AMNESIA);
                 (void)clear_timed(TMD_IMAGE);
                 (void)clear_timed(TMD_BLIND);
			     (void)clear_timed(TMD_POISONED);
                 p_ptr->silver = PY_SILVER_HEALTHY;
                 p_ptr->slime = PY_SLIME_HEALTHY;
                 (void)inc_timed(TMD_TSIGHT, randint(die / 3) + (25));
                 (void)inc_timed(TMD_ESP, randint(die / 3) + (25));
                 (void)inc_timed(TMD_BLESSED, randint(die / 3) + (25));
            }
			break;
        }

		case LUCK_RESIST_COLD:
        {
            die = randint(99) + (randint(plev/4));
            if (die < 10) (void)inc_timed(TMD_OPP_COLD, randint(die) + (10));
            else if (die < 45) (void)inc_timed(TMD_OPP_COLD, randint(10) + (10));
            else if (die < 80) (void)inc_timed(TMD_OPP_COLD, randint(15) + (10));
            else if (die < 100) (void)inc_timed(TMD_OPP_COLD, randint(die / 4) + (11));
            else if (die < 113) (void)inc_timed(TMD_OPP_COLD, randint(die / 3) + (15));
            else
            {
                spellswitch = 7;
                brand_weapon();
                (void)inc_timed(TMD_OPP_COLD, randint(20) + (10));
                spellswitch = 0;
            }
            break;
        }

		case LUCK_RECHARGING:
        {
            die = randint(99) + (randint(plev/5));
      		if (die < 4) return recharge(1);
      		else if (die < 50) return recharge(2 + plev / 5);
      		else if (die < 70) return recharge(2 + plev / 3);
      		else if (die < 85) return recharge(2 + plev / 2);
      		else if (die < 100) return recharge(3 + plev);
      		else if (die < 105) return recharge(10 + plev);
      		else return recharge(50 + plev);
			break;
        }
        
        case LUCK_BUG_SPRAY:
        {
            die = randint(99) + (randint(plev/5));
            if (die < 6) (void)dispel_bug(1 + (randint(plev / 3)));
			else if (die < 94) (void)dispel_bug(1 + (randint(plev / 2)));
			else if (die < 100) (void)dispel_bug(1 + (randint(plev - 3)));
			else (void)dispel_bug(4 + (randint(plev)));
			break;
        }

		case LUCK_DETECT_INVISIBLE:
		{
            die = randint(99) + (randint(plev/5));
			if (die < 10) (void)inc_timed(TMD_SINVIS, randint(4) + 2);
			else if (die < 70) (void)detect_monsters_invis();
			else if (die < 80) (void)inc_timed(TMD_SINVIS, randint(24) + 24);
			else if (die < 100)
			{
                 (void)detect_monsters_invis();
                 (void)inc_timed(TMD_SINVIS, randint(21) + 19);
            }
            else if (die < 106)
			{
                 (void)detect_monsters_invis();
                 (void)inc_timed(TMD_SINVIS, randint(35) + 35);
            }
            else
			{
                 (void)clear_timed(TMD_IMAGE);
                 (void)clear_timed(TMD_BLIND);
                 (void)detect_monsters_invis();
                 (void)inc_timed(TMD_TSIGHT, randint(35) + 35);
            }
            break;
		}

		case LUCK_RESIST_POISON:
		{
            die = randint(99) + (randint(plev/5));
            if (die < 10) (void)inc_timed(TMD_OPP_POIS, randint(5) + 5);
            else if (die < 80) (void)inc_timed(TMD_OPP_POIS, randint(20) + 20);
            else if (die < 90)
            {
               (void)set_timed(TMD_POISONED, p_ptr->timed[TMD_POISONED] / 2);
               (void)inc_timed(TMD_OPP_POIS, randint(20) + 20);
            }
            else if (die < 105)
            {
			   (void)clear_timed(TMD_POISONED);
               (void)inc_timed(TMD_OPP_POIS, randint(21) + 24);
            }
            else
            {
			   (void)clear_timed(TMD_POISONED);
               p_ptr->silver = PY_SILVER_HEALTHY;
               p_ptr->slime = PY_SLIME_HEALTHY;
               (void)inc_timed(TMD_OPP_POIS, randint(26) + 24);
            }
			break;
		}
        
        case LUCK_CURE_CAUSE:
        {
            die = randint(99) + (randint(plev/4));
            if (die < 5)
            {
               (void)inc_timed(TMD_CUT, randint(40) + 10);
               (void)inc_timed(TMD_STUN, randint(15) + 10);
            }
            else if (die < 10)
            {
               (void)inc_timed(TMD_CONFUSED, randint(10) + 10);
               (void)inc_timed(TMD_AMNESIA, randint(10) + 10);
			   msg_print("You feel your life slipping away!");
			   lose_exp(100 + randint(die * 100));
            }
            else if (die < 15) (void)inc_timed(TMD_CHARM, randint(14) + 11);
            else if (die < 19) (void)inc_timed(TMD_CONFUSED, randint(14) + 11);
            else if (die < 24) (void)inc_timed(TMD_BLIND, randint(26) + 24);
            else if (die < 29) (void)inc_timed(TMD_AFRAID, randint(14) + 11);
            else if (die < 33) (void)inc_timed(TMD_IMAGE, randint(14) + 11);
            else if (die < 40) (void)inc_timed(TMD_POISONED, randint(24) + 11);
            else if (die < 50)
            {
                 (void)set_timed(TMD_POISONED, p_ptr->timed[TMD_POISONED] / 2);
                 (void)set_timed(TMD_CUT, p_ptr->timed[TMD_CUT] / 2);
                 (void)set_timed(TMD_STUN, p_ptr->timed[TMD_STUN] / 2);
            }
            else if (die < 55) (void)clear_timed(TMD_POISONED);
            else if (die < 60)
            {
                 (void)clear_timed(TMD_CUT);
                 (void)clear_timed(TMD_STUN);
                 (void)clear_timed(TMD_BLIND);
                 (void)clear_timed(TMD_FRENZY);
                 (void)set_timed(TMD_POISONED, p_ptr->timed[TMD_POISONED] / 2);
            }
            else if (die < 80)
            {
                 (void)hp_player(1 + randint(plev - 2));
                 (void)clear_timed(TMD_CUT);
                 (void)clear_timed(TMD_STUN);
                 (void)clear_timed(TMD_CONFUSED);
                 (void)clear_timed(TMD_CHARM);
                 (void)clear_timed(TMD_AMNESIA);
                 (void)clear_timed(TMD_BLIND);
                 (void)clear_timed(TMD_FRENZY);
                 (void)clear_timed(TMD_POISONED);
            }
            else /* die > 79 */
            {
                 (void)hp_player(plev);
                 (void)clear_timed(TMD_CUT);
                 (void)clear_timed(TMD_STUN);
                 (void)clear_timed(TMD_CONFUSED);
                 (void)clear_timed(TMD_CHARM);
                 (void)clear_timed(TMD_IMAGE);
                 (void)clear_timed(TMD_AMNESIA);
                 (void)clear_timed(TMD_BLIND);
                 (void)clear_timed(TMD_FRENZY);
                 (void)clear_timed(TMD_POISONED);
			     if (p_ptr->silver > PY_SILVER_HEALTHY) p_ptr->silver = p_ptr->silver - 2;
			     if (p_ptr->silver < PY_SILVER_HEALTHY) p_ptr->silver = PY_SILVER_HEALTHY;
                 if (p_ptr->slime > PY_SLIME_HEALTHY) p_ptr->slime = p_ptr->slime - 4;
			     if (p_ptr->slime < PY_SLIME_HEALTHY) p_ptr->slime = PY_SLIME_HEALTHY;
            }
            if (die > 95)
            {
                 (void)set_food(p_ptr->food + plev + die + 100);  
                 (void)hp_player(randint(plev / 2));
			     if (p_ptr->silver > PY_SILVER_HEALTHY) p_ptr->silver = p_ptr->silver - 2;
			     if (p_ptr->silver < PY_SILVER_HEALTHY) p_ptr->silver = PY_SILVER_HEALTHY;
                 if (p_ptr->slime > PY_SLIME_HEALTHY) p_ptr->slime = p_ptr->slime - 4;
			     if (p_ptr->slime < PY_SLIME_HEALTHY) p_ptr->slime = PY_SLIME_HEALTHY;
            }
            if (die > 105)
            {
			     p_ptr->silver = PY_SILVER_HEALTHY;
			     p_ptr->slime = PY_SLIME_HEALTHY;
            }
            if (die > 110)
            {
                 (void)set_food(p_ptr->food + ((plev + die) * 2) + 100);  
                 (void)hp_player(randint(plev / 2));
            }
			break;
        }

		case LUCK_WONDER:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			(void)spell_wonder(dir);
			break;
		}
        
   		case LUCK_BLINK:
		{
			teleport_player(9 + randint(plev / 6));
			break;
		}

		case LUCK_CONFUSE_MONSTER:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			(void)confuse_monster(dir, plev);
			break;
		}

		case LUCK_DETECT_MONSTERS:
		{
			(void)detect_monsters_normal();
			break;
		}

		case LUCK_DETECT_TRAPS_OBJ:
		{
			(void)detect_traps();
			(void)detect_objects_normal();
			break;
        }

		case LUCK_SLOW_MONSTER:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			(void)slow_monster(dir);
			break;
		}

		case LUCK_MINI_DESTRUCTION:
		{
            spellswitch = 11;
			destroy_area(py, px, 5, TRUE);
			spellswitch = 0;
			break;
		}
		
		case LUCK_TELEKINESIS: /* to add later */
		{
            (void)dispel_bug(2);
			break;
		}
		
		case LUCK_TELEKINESIS2: /* to add later */
		{
            (void)dispel_bug(2);
			break;
		}

		case LUCK_SENSE_SURROUNDINGS:
		{
			map_area();
			break;
		}

		case LUCK_DETECT_ENCHANTMENT:
		{
			(void)detect_objects_magic();
			break;
		}

		case LUCK_DETECT_TRAPS:
		{
            die = randint(99) + (randint(plev/4));
			(void)detect_traps();
			if (die > 100) (void)destroy_doors_touch();
			if (die > 110)
            {
               spellswitch = 6; /* increses radius for destroy traps */
                                /* and keeps it from destroying doors */
               (void)destroy_doors_touch();
               spellswitch = 0;
            }
        }

		case LUCK_DETECT_OBJECTS:
		{
            die = randint(99) + (randint(plev/5));
            if (die > 105) (void)detect_objects_magic();
            else if (die > 5) (void)detect_objects_normal();
            else msg_print("You detect that there are objects on the level.");
/*            if (die < 7)
            {
               spellswitch = 5;  * creates object that's not good or great *
               if (randint(100) < 25) acquirement(py + randint(4), px + randint(4), 1, FALSE);
               else if (randint(100) < 50) acquirement(py + randint(4), px - randint(4), 1, FALSE);
               else if (randint(100) < 75) acquirement(py - randint(4), px + randint(4), 1, FALSE);
               else acquirement(py - randint(4), px - randint(4), 1, FALSE);
               spellswitch = 0;
            }
            if (die > 105)  * make good but not great object *
            {
               if (randint(100) < 25) acquirement(py + randint(4), px + randint(4), 1, FALSE);
               else if (randint(100) < 50) acquirement(py + randint(4), px - randint(4), 1, FALSE);
               else if (randint(100) < 75) acquirement(py - randint(4), px + randint(4), 1, FALSE);
               else acquirement(py - randint(4), px - randint(4), 1, FALSE);
            } */
			break;
        }

		case LUCK_MAP_AREA:
		{
            die = randint(99) + (randint(plev/5));
            if (die < 8)
            {
               spellswitch = 4; /* does not light room */
               (void)lite_area(damroll(2, (plev / 6)), (plev / 5));
               spellswitch = 0;
            }
            else if (die < 90) map_area();
            else
            {
               (void)lite_area(damroll(2, (plev / 4)), (plev / 5));
			   map_area();
            }
			break;
		}
		
		case LUCK_ADJUST_CURSE:
		{
            die = randint(99) + (randint(plev/5));
            if (die < 4) curse_weapon();
            else if (die < 15)
            {
                 spellswitch = 2; /* makes weapon a morgul weapon */
                 curse_weapon();
                 spellswitch = 0;
            }
            else if (die < 35)
            {
                 spellswitch = 3; /* damages weapon without removing current ego */
                 curse_weapon();
                 spellswitch = 0;
            }
            else if (die < 75)
            {
                 remove_curse();
            }
            else if (die < 85)
            {
                 remove_curse();
			     if (!get_aim_dir(&dir)) return (FALSE);
                 msg_print("You fire a small ball of destroy cursed objects.");
			     fire_ball(GF_HOLY_ORB, dir, 0, (plev/15));
            }
            else if (die < 95)
            {
                 remove_curse();
			     if (!get_aim_dir(&dir)) return (FALSE);
                 msg_print("You fire a ball of destroy cursed objects.");
			     fire_ball(GF_HOLY_ORB, dir, randint(plev/5), (plev/7));
            }
            else if (die < 105)
            {
                 remove_all_curse();
            }
            else 
            {
                 remove_all_curse();
			     if (!get_aim_dir(&dir)) return (FALSE);
                 msg_print("You fire a ball of destroy cursed objects.");
			     fire_ball(GF_HOLY_ORB, dir, randint(plev/4), (plev/8));
            }
			break;
        }
        
        case LUCK_DETECT_ENCHANTMENT2:
		{
            die = randint(99) + (randint(plev/5));
            if (die > 15) (void)detect_objects_magic();
            else if (die > 6)
            {
                  msg_print("The spell doesn't quite come off like it should.");
                  (void)detect_objects_normal();
            }
            else msg_print("You detect that there is magic on the level.");
			break;
        }

		case LUCK_PROBING:
		{
			(void)probing();
			break;
		}

		case LUCK_MAP_LEVEL:
		{
			if (randint(plev * 2) > 97) wiz_lite();
			else
			{
                if (randint(plev * 2) > 2) spellswitch = 1;
			    map_area(); /* spellswitch = 1 makes map_area map whole level */
			    spellswitch = 0;
            }
			break;
		}

		case LUCK_FULL_IDENTIFY:
		{
			return identify_fully();
		}
		
		case LUCK_HIT_N_RUN:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_bolt(GF_MISSILE, dir, damroll(2, (plev/3) + 1));
			teleport_player(10);
			break;
		}

		case LUCK_TELEPORT_SELF:
		{
			teleport_player(plev * 5);
			break;
		}

		case LUCK_TELEPORT_OTHER:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			(void)teleport_monster(dir);
			break;
		}
		
		case LUCK_SLOW_MONSTERS:
		{
			(void)slow_monsters();
			break;
		}

		case LUCK_EARTHQUAKE:
		{
			earthquake(py, px, 10);
			break;
		}

		case LUCK_MASS_SLEEP:
		{
			(void)sleep_monsters();
			break;
		}
		
		case LUCK_SLIP_INTO_SHADOWS:
		{
	             u32b t1, t2, t3;
	             u32b f1 = 0L, f2 = 0L, f3 = 0L;
	             object_type *o_ptr;
	             int k;
		         o_ptr = &inventory[k];

	         /* Check for aggravation */
	         for (k = INVEN_WIELD; k < INVEN_TOTAL; k++)
	         {
		         /* Skip non-objects */
		         if (!o_ptr->k_idx) continue;

		         /* Extract the flags */
		         object_flags(o_ptr, &t1, &t2, &t3);

		         /* Extract flags */
		         f3 |= t3;
	         }
             if (f3 & TR3_AGGRAVATE)
             {
                if (randint(plev * 2) > 85)
                {
                  msg_print("You are aggravating monsters, attempting to remove curse..");
                  if (remove_curse()) msg_print("You feel as if someone is watching over you.");
                }
                else msg_print("You can't slip into the shadows while you're aggravating monsters.");
    	        break;
             }   
             else (void)inc_timed(TMD_SHADOW, randint(plev) + 30);
             
    	     break;
		}

		case LUCK_HASTE_SELF:
		{
			if (!p_ptr->timed[TMD_FAST])
			{
				(void)set_timed(TMD_FAST, randint(20) + plev);
			}
			else
			{
				(void)inc_timed(TMD_FAST, randint(5));
			}
			break;
		}

		case LUCK_POLYMORPH_OTHER:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			(void)poly_monster(dir);
			break;
		}

        case LUCK_ADJUST_SPEED:
		{
            adjust = (randint(20) - 9);
            if (randint(100) < plev) adjust = adjust + 2;
            if (randint(501) < plev) adjust = adjust + randint(3);
            if ((adjust = 0) && (randint(45) < plev)) adjust = 9;
            else if (adjust = 0) adjust = (randint(3) - 5);
            if (adjust > 0) (void)set_timed(TMD_ADJUST, randint(25) + plev);
            if (adjust < 0) (void)set_timed(TMD_ADJUST, (plev + 25) - randint(plev));
			break;
		}

		case LUCK_DOOR_CREATION2:
		{
			(void)door_creation();
			break;
		}

		case LUCK_STAIR_CREATION2:
		{
			(void)stair_creation();
			break;
		}

		case LUCK_TELEPORT_LEVEL:
		{
			(void)teleport_player_level();
			break;
		}

		case LUCK_WORD_OF_DESTRUCTION:
		{
			destroy_area(py, px, 15, TRUE);
			break;
		}

		case LUCK_WORD_OF_RECALL2:
		{
            die = randint(99) + (randint(plev/4));
            if (die > 100)
            {
                 spellswitch = 8; /* quick recall */
                 set_recall();
                 spellswitch = 0;
            }
            else set_recall();
			break;
		}

		case LUCK_RUNE_OF_PROTECTION: /* rune of protection */
		{
			(void)warding_glyph();
			break;
		}
        
        case LUCK_BLINK_MONSTER:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			spellswitch = 13;
			(void)teleport_monster(dir);
			spellswitch = 0;
			break;
		}
		
		case LUCK_BANISH_SUMMON:
        {
            die = randint(99) + (randint(plev/3));
            if (die < 5)
            {
               msg_print("The spell gets a little out of hand..");
               summon_specific(py, px, p_ptr->depth + 10, 0);
            }
            else if (die < 10)
            {
               msg_print("You summon aminals! -I mean animals!");
               summon_specific(py, px, p_ptr->depth, SUMMON_ANIMAL);
            }
            else if (die < 15)
            {
               msg_print("You summon a dragon.");
               summon_specific(py, px, p_ptr->depth, SUMMON_DRAGON);
            }
            else if (die < 20)
            {
               msg_print("You summon an undead monster.");
               summon_specific(py, px, p_ptr->depth, SUMMON_UNDEAD);
            }
            else if (die < 25)
            {
               msg_print("You summon a demon.");
               summon_specific(py, px, p_ptr->depth, SUMMON_DEMON);
            }
            else if (die < 30)
            {
               msg_print("You summon something hairy.");
               summon_specific(py, px, p_ptr->depth, SUMMON_ANGEL);
            }
            else if (die < 35)
            {
               msg_print("You summon spiders.");
               summon_specific(py, px, p_ptr->depth, SUMMON_SPIDER);
            }
            else if (die < 50)
            {
               spellswitch = 14; /* chose what you summon */
               (void) banishment();
               spellswitch = 0;
            }
            else if (die < 55)
            {
               spellswitch = 15; /* scares all monsters */
               msg_print("With inexplicable authority, you tell the monsters to leave you alone.");
               (void)turn_undead();
               spellswitch = 0;
            }
            else if (die < 65) return banish_unnatural();
            else if (die < 75) return banish_evil(100);
            else if (die < 95) return banishment();
            else if (die < 105) return mass_banishment();
            else if (die < 106)
            {
                 (void)mass_banishment();
                 (void)banishment();
                 spellswitch = 14; /* chose what you summon */
                 (void)banishment();
                 spellswitch = 0;
            }
            else /* (die > 105) */
            {
                 (void)mass_banishment();
                 (void)banishment();
            }
        /* used (void) here instead of return because return exits the
         * function and doesn't go on to the next thing
         * ..hope that doesn't have any side effects. */
            break;  
        }
        
        case LUCK_AFFECT_SELF:
        {
            die = randint(99) + (randint(plev/4));
			break;
        }
        
        case LUCK_AFFECT_OTHER:
        {
            die = randint(99) + (randint(plev/4));
			break;
        }
        
        case LUCK_POTLUCK_STATS:
        {
            die = randint(99) + (randint(plev/4));
			break;
        }
        
        case LUCK_AQUIREMENT:
        {
            die = randint(99) + (randint(plev/4));
			break;
        }

		case LUCK_RESIST_HEAT_ELEC:
		{
			(void)inc_timed(TMD_OPP_FIRE, randint(10) + 10);
			(void)inc_timed(TMD_OPP_ELEC, randint(10) + 10);
			break;
		}

		case LUCK_TURN_UNDEAD:
		{
			(void)turn_undead();
			break;
		}
        
        case LUCK_BURST_OF_LIGHT:
        {
            msg_print("You activate a burst of light to stun your enemies.");
            /* need for loop because spellswitch resets at the end of project() */
            int way;
		    for (way = 0; way < 10; way++)
		    {
            spellswitch = 9;lite_line(way); /* prevents area from staying lit */
            }
            spellswitch = 9; /* prevents learning water immunity */
            return (project_los(GF_WATER, 0));
			break; /* like weak starlite except also stuns monsters */
        }

        case LUCK_CURING:
        {
			(void)clear_timed(TMD_CONFUSED);
			(void)clear_timed(TMD_BLIND);
			(void)clear_timed(TMD_CHARM);
			(void)clear_timed(TMD_AMNESIA);
			(void)clear_timed(TMD_POISONED);
			(void)clear_timed(TMD_CUT);
			(void)clear_timed(TMD_STUN);
			break;
        }

		case LUCK_REMOVE_CURSE:
		{
            die = randint(99) + (randint(plev/4));
            if (die > 101) remove_all_curse();
            else remove_curse();
			break;
		}
        
        case LUCK_RESTORE_CLEVERNESS:
        {
			(void)do_res_stat(A_INT);
			(void)do_res_stat(A_DEX);
			(void)do_res_stat(A_CHR);
			break;
		}
        
        case LUCK_RESISTANCE:
        {
			int time = randint(20) + 20;
			(void)inc_timed(TMD_OPP_ACID, time);
			(void)inc_timed(TMD_OPP_ELEC, time);
			(void)inc_timed(TMD_OPP_FIRE, time);
			(void)inc_timed(TMD_OPP_COLD, time);
			(void)inc_timed(TMD_OPP_POIS, time);
			break;
		}
		
		case LUCK_STINK:
        {
            die = randint(99) + (randint(plev/4));
   			if (die > 10)
            {
               if (!get_aim_dir(&dir)) return (FALSE);
            }
            if (die < 11) /* poison self */
            {
               msg_print("You smell something foul.");
			   if (!(p_ptr->resist_pois || p_ptr->timed[TMD_OPP_POIS]))
			   {
                  int dam;
                  if (die < 2) die = 2;
			      dam = ((109 - (die * 10)) / 3);
			      take_hit(dam, "a toxic smell");
			      (void)inc_timed(TMD_POISONED, rand_int(dam) + 10);
			   }
            }
            else if (die < 40) fire_ball(GF_POIS, dir, 10 + (plev / 2), 2);
            else if (die < 60)
            {
               fire_bolt_or_beam(beam-10, GF_POIS, dir,
                                 damroll(5 + (plev / 2), 3));
            }
			else if (die < 80) fire_ball(GF_POIS, dir, 15 + (plev), plev / 12);
			else if (die < 105) fire_ball(GF_POIS, dir, 40 + (plev / 2), 3);
			else /* (die > 100) */ fire_ball(GF_POIS, dir, 40 + (plev), 3);
			break;
        }

		case LUCK_BERSERKER:
		{
			(void)hp_player(30);
            die = randint(75);
	        if ((p_ptr->timed[TMD_CHARM]) && (die > (plev + 25)))
	        {
                 msg_print("You're in too good a mood to go into a battle frenzy");
            }
            else 
            {
                 (void)clear_timed(TMD_AFRAID);
			     (void)clear_timed(TMD_CHARM);
			     if (plev > 20)
			     {
			     (void)inc_timed(TMD_SHERO, randint(25) + (plev + ((76 - die) / 3)));
                 }
                 else
                 {
			     (void)inc_timed(TMD_SHERO, randint(25) + (25));
                 }
            }
			break;
		}

		case LUCK_BEDLAM:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			if (plev > 44) die = 5;
			else die = 4;
			fire_ball(GF_OLD_CONF, dir, plev, die);
			break;
		}

		case LUCK_CHAOS_STRIKE: /* weakened chaos strike */
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_bolt_or_beam(beam, GF_CHAOS, dir, damroll(plev, 8));
			break;
		}

		case LUCK_RIFT:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_beam(GF_GRAVITY, dir, 10 + (plev*2) + damroll(plev, 6));
			break;
            /* 71-170 at L20, 101-250 at L30, 131-330 at L40, 161-410 at L50 */
		}
		
		case LUCK_MASS_CHAOS: /* note lack of "elseif" */
        {
            return (project_los(GF_OLD_CONF, plev + 1)); /* no damage with OLD_CONF */
            return (project_los(GF_CHAOS, (plev/2)));
            if (randint(100) < 40) /* random type of stunning */
            {
               if (randint(100) < 50) return (project_los(GF_WATER, 5));
               else return (project_los(GF_SOUND, 4));
            }
            if (randint(100) < 30) return (project_los(GF_GRAVITY, randint(plev)));
            if (randint(100) < 35) slow_monsters();
            if (randint(100) < 30)
            {
               spellswitch = 13; /* blink monsters */
               (project_los(GF_AWAY_ALL, (2 + randint(15))));
			   spellswitch = 0;
            }
            if (randint(100) < 35) return (project_los(GF_BRFEAR, randint(plev)));
			break;
        }
    }

	/* Success */
	return (TRUE);
}

bool cast_spell(int tval, int index)
{
	if (tval == TV_MAGIC_BOOK)
	{
		return cast_mage_spell(index);
	}
	else if (tval == TV_NEWM_BOOK)
	{
		return cast_newm_spell(index);
	}
	else if (tval == TV_LUCK_BOOK)
	{
		return cast_luck_spell(index);
	}
	else
	{
		return cast_priest_spell(index);
	}
}
