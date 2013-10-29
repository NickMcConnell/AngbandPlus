/* File: x-spell.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statxement
 * are included in all such copies.  Other copyrights may also apply.
 */


#include "angband.h"
#include "cmds.h"
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
#define SPELL_TELEPORT_OTHER            32
#define SPELL_HASTE_SELF                29
#define SPELL_MASS_SLEEP                39
#define SPELL_FIRE_BALL                 57
#define SPELL_DETECT_ENCHANTMENT        10
#define SPELL_ELEMENTAL_BRAND           54 /* move to book 3 */

/* Resistances of Scarabtarices 4*/
#define SPELL_MANA_DEFENCE              65
#define SPELL_RESIST_COLD               44
#define SPELL_RESIST_FIRE               45
#define SPELL_RESIST_POISON             46
#define SPELL_RESISTANCE                47
#define SPELL_SHIELD                    48

/* Raal's Tome of Destruction 5*/
#define SPELL_CLOUD_KILL                38
#define SPELL_SHOCK_WAVE                36
#define SPELL_EXPLOSION                 37
#define SPELL_ACID_BALL                 56
#define SPELL_ICE_STORM                 58
#define SPELL_METEOR_SWARM              60
#define SPELL_RIFT                      62

/* Mordenkainen's Escapes 6*/
#define SPELL_DOOR_CREATION             22
#define SPELL_EARTHQUAKE                23
#define SPELL_SPHERE_CHARM1             66
#define SPELL_STAIR_CREATION            24
#define SPELL_TELEPORT_LEVEL            33
#define SPELL_WORD_OF_RECALL            34
#define SPELL_RUNE_OF_PROTECTION        49
#define SPELL_SUMMON_DEMONIC_AID        64

/* Tenser's Transformations 7*/
#define SPELL_MANA_DEFENCE2             6
#define SPELL_HEROISM                   27
#define SPELL_BERSERKER                 28
#define SPELL_SPHERE_CHARM2             67
#define SPELL_ENCHANT_WEAPON            52
#define SPELL_ENCHANT_ARMOR             51
#define SPELL_ELEMENTAL_STRIKE          68
#define SPELL_DRAGONFIRE                69

/* Kelek's Grimoire of Power 8*/
#define SPELL_BEDLAM                    40
#define SPELL_REND_SOUL                 41
#define SPELL_WORD_OF_DESTRUCTION       42
#define SPELL_RECHARGE_ITEM_II          53 /* moved from book 7 */
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

        /* Nature: -done except for call help */
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
#define NEWM_CALL_HELP                 45
#define NEWM_SONG_PROTECTION           46
#define NEWM_SONG_DISPELLING           47
#define NEWM_WARDING                   48
#define NEWM_RENEWAL                   49

/* Spirits of Nature 7*/
#define NEWM_ESSENCE_SPEED             50
#define NEWM_INFUSION                  51
#define NEWM_BANISH_UNNATURAL          54
#define NEWM_TELEPORT_LEVEL            53
#define NEWM_REMEMBRANCE               55
#define NEWM_EXTRA_BLOW                67
#define NEWM_NATURE_BLESSING           52
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

        /* Chance Magic -mostly done */
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
#define LUCK_STONE_TO_MUD              24
#define LUCK_SENSE_SURROUNDINGS        25
#define LUCK_DETECT_ENCHANTMENT        26

/* Guidebook for Tourists 3(only for tourists)*/
#define LUCK_DETECT_TRAPS              27
#define LUCK_DETECT_OBJECTS            28
#define LUCK_MAP_AREA                  29
#define LUCK_ADJUST_CURSE              30
#define LUCK_TELEKINESIS2              31
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

/* The Lottery 5(not for rogues)*/
#define LUCK_POLYMORPH_OTHER           44
#define LUCK_ADJUST_SPEED              45
#define LUCK_BANISH_SUMMON             46
#define LUCK_AFFECT_SELF               47 /* needs testing */
#define LUCK_AFFECT_OTHER              48 /* needs testing */
#define LUCK_POTLUCK_STATS             49 /* needs testing */
#define LUCK_AQUIREMENT                50 /* needs testing */

/* Mordenkainen's Escapes: RE 6(mostly not for tourists)*/
#define LUCK_BLINK_MONSTER             51
#define LUCK_DOOR_CREATION2            52
#define LUCK_STAIR_CREATION2           53
#define LUCK_TELEPORT_LEVEL            54
#define LUCK_WORD_OF_DESTRUCTION       55
#define LUCK_WORD_OF_RECALL2           56
#define LUCK_RUNE_OF_PROTECTION        57

/* Rogue's Defences 7(not for tourists)*/
#define LUCK_RESIST_HEAT_ELEC          58
#define LUCK_TURN_UNDEAD               59
#define LUCK_BURST_OF_LIGHT            60
#define LUCK_CURING                    61
#define LUCK_REMOVE_CURSE              62
#define LUCK_RESTORE_CLEVERNESS        63
#define LUCK_RESISTANCE                64

/* The Spreading of Chaos 8*/
#define LUCK_STINK                     65
#define LUCK_BERSERKER                 66
#define LUCK_BEDLAM                    67
#define LUCK_CHAOS_STRIKE              68
#define LUCK_RIFT                      69
#define LUCK_BREATHE_CHAOS             70 /* needs testing */
#define LUCK_MASS_CHAOS                71 /* needs testing */
#define LUCK_BIZZARE_EFFECTS           72 /* needs testing */

        /* Alchemy - done except for marked ones */
/* Beginner's Mixing 0*/
#define CHEM_CURE_LIGHT_WOUNDS         1
#define CHEM_STINKING_CLOUD            3
#define CHEM_DISINFECTANT              0
#define CHEM_LIGHT_AREA                2
#define CHEM_PHASE_DOOR                4
#define CHEM_FIND_TRAPS_DOORS          5
#define CHEM_DETECT_MONSTERS           6
#define CHEM_CURE_POISON               7
#define CHEM_ACID_ARROW                8

/* Commonly Used Potions 1*/
#define CHEM_INFRAVISION               9
#define CHEM_RESIST_ACID               10
#define CHEM_CURE_MODERATE_WOUNDS      11
#define CHEM_RESIST_HEAT_COLD          12
#define CHEM_SATISFY_HUNGER            13
#define CHEM_CHEMICAL_RAGE             14
#define CHEM_STRONG_DISINFECTANT       15
#define CHEM_RESIST_POISON             16
#define CHEM_HEAL                      17

/* Chemical Detections and Tools 2*/
#define CHEM_DETECT_TREASURE           18
#define CHEM_TRAP_DOOR_DESTRUCTION     19
#define CHEM_DETECT_OBJECT             20
#define CHEM_IDENTIFY                  21
#define CHEM_DETECT_INVISIBLE          22
#define CHEM_RESISTANCE                23
#define CHEM_DETECT_ENCHANTMENT        24
#define CHEM_ACID_COAT_AMMO            25

/* Quick Getaways (mainly for thieves)3*/
#define CHEM_BURST_OF_SPEED            26
#define CHEM_TELEPORT_SELF             27
#define CHEM_TELEPORT_OTHER            28
#define CHEM_SLOW_MONSTERS             29
#define CHEM_MASS_SLEEP                30
#define CHEM_HASTE_SELF                31
#define CHEM_TELEPORT_LEVEL            32 /* don't forget #33 */
#define CHEM_RUNE_OF_PROTECTION        34

/* Missile Magic (archers only)4*/
#define CHEM_LIGHTNING_BOLT            35
#define CHEM_RAIN_OF_ARROWS            36
#define CHEM_CHAOS_ARROW               37
#define CHEM_ROCKET_BLAST              38
#define CHEM_ENCHANT_TO_HIT            39
#define CHEM_ENCHANT_TO_DAM            40
#define CHEM_POISON_BRAND_AMMO         41
#define CHEM_ELEMENTAL_BRAND_AMMO      42

/* Chemical Combat (not for archers)5*/
#define CHEM_CONFUSE_MONSTER           43
#define CHEM_SLEEP_MONSTER             44
#define CHEM_WONDER                    52
#define CHEM_ELEMENT_BOLT              45
#define CHEM_POLYMORPH_OTHER           46
#define CHEM_SLOW_MONSTER              47
#define CHEM_ELEMENT_BALL              49 /* cold or fire */
#define CHEM_ACID_BALL                 50
#define CHEM_ICE_STORM                 51

/* Core of Alchemy (alchemists only)6*/
#define CHEM_CLOUDKILL1                53
#define CHEM_STONE_TO_MUD              55
#define CHEM_RECHARGING1               56
#define CHEM_OBJECT_TO_GOLD1           57 // do this one later
#define CHEM_EXPLOSION                 58
#define CHEM_RESTORATION               59
#define CHEM_POISON_BRAND_WEAPON1      60
#define CHEM_ELIXIR_OF_LIFE            33

/* Advanced Thievery (thieves only)7*/
#define CHEM_HIT_N_RUN                 61
#define CHEM_SLIP_INTO_SHADOWS         62
#define CHEM_POISON_BOLT               63
#define CHEM_CLOUDKILL2                64
#define CHEM_OBJECT_TO_GOLD2           66 // do this one later
#define CHEM_RESTORE_DEX1              48
#define CHEM_TELEKINESIS               74
#define CHEM_BEDLAM1                   65
#define CHEM_POISON_BRAND_WEAPON2      67

/* Advenced Archer's Tactics (archers only)8*/
#define CHEM_RECHARGING2               68
#define CHEM_SHIELD                    69
#define CHEM_BEDLAM2                   70
#define CHEM_RESTORE_DEX2              71
#define CHEM_ENCHANT_ARMOR             72
#define CHEM_METEOR_SWARM              73
#define CHEM_GLYPH_OF_WARDING          54

        /* Black Magic */
/* Dark Initiation (not for assassin) 0*/
#define DARK_SHADOW_STING              75
#define DARK_DETECT_EVIL               0
#define DARK_INFRAVISION               1
#define DARK_CALL_DARK                 2
#define DARK_SCARE_MONSTER             3
#define DARK_NOXIOUS_FUMES             4
#define DARK_CURE_POISON               5
#define DARK_SLEEP_MONSTER             6
#define DARK_SEE_INVISIBLE             8

/* Blood Novitiate (not for witch) 1*/
#define DARK_FIND_TRAPS                9
#define DARK_DETECT_DOORS_STAIRS       10
#define DARK_SHADOW_SHIFT              11
#define DARK_DETECT_LIVING             59
#define DARK_DRAIN_CHARGES             12 // needs testing ..it works! cool
#define DARK_STINK                     15
#define DARK_TURN_UNDEAD               13
#define DARK_DEATH_BOLT                14
#define DARK_DISPEL_UNDEAD1            16

/* Handbook of Curses and Poison (not for necromancer) 2*/
#define DARK_CAUSE_LIGHT_WOUNDS        17
#define DARK_WITCH_DISINFECTANT        18
#define DARK_CAUSE_SERIOUS_WOUNDS      19
#define DARK_TRAP_DOOR_DESTRUCTION     20
#define DARK_POISON_BOLT               21
#define DARK_SLIP_INTO_SHADOWS         22
#define DARK_RESIST_POISON             73
#define DARK_CAUSE_MORTAL_WOUNDS       23
#define DARK_BREATHE_WITHERING         25

/* Frightful Realms 3*/
#define DARK_WITHER_FOE                7
#define DARK_SCARE_MONSTERS            26
#define DARK_SPEAR_OF_DARK             27 // needs testing
#define DARK_ICE_SPEAR                 28 // needs testing
#define DARK_STENCH_OF_DEATH           29 // needs testing
#define DARK_BEAM_OF_DESTRUCTION       30 // needs testing
#define DARK_DESTRUCTION_BALL          31 // needs testing
#define DARK_VENOM_BOLT                24
#define DARK_CHAOS_STRIKE              32

/* Mysteries of Shadow 4*/
#define DARK_SHADOW_WARP               33
#define DARK_WIZARD_LOCK               34
#define DARK_SHADOW_MAPPING            35
#define DARK_BREAK_CURSE               36
#define DARK_PROBING                   37
#define DARK_IDENTIFY                  38
#define DARK_PROTECT_FROM_LIFELESS     45
#define DARK_EXORCISE_DEMONS           40
#define DARK_DETECT_ENCHANTMENT        41

/* Unholy Protection & Curing (not for assassin) 5*/
#define DARK_BOLDNESS                  42
#define DARK_TOURNIQUET                43
#define DARK_RESIST_POISON2            44
#define DARK_UNDEAD_SKIN               46
#define DARK_SUMMON_DEMONIC_AID        39
#define DARK_UNHOLY_RESISTANCE         47
#define DARK_SHADOW_BARRIER            48

/* Life Force Mastery (not for assassin) 6*/
#define DARK_DETECT_ALL_MONSTERS       49
#define DARK_ORB_OF_DEATH              50
#define DARK_REND_SOUL                 51
#define DARK_TELEPORT_OTHER            52
#define DARK_DISPEL_LIFE               53
#define DARK_VAMPIRIC_DRAIN            54
#define DARK_HOLD_LIFE                 55
#define DARK_GENOCIDE                  56

/* Metamorphoses 7*/
#define DARK_RECHARGING                57
#define DARK_CURE_MORTAL_WOUNDS        58
#define DARK_SPIRIT_OF_BALROG          60
#define DARK_HASTE                     61
#define DARK_SEE_ALL_FOES              64
#define DARK_BLOODWRATH                62
#define DARK_POISON_AMMO               63

/* Necronomicon (not for assassin) 8*/
#define DARK_DRAW_MANA                 65
#define DARK_WORD_OF_DESTRUCTION       66
#define DARK_HOLD                      67
#define DARK_DISPEL_UNDEAD2            68
#define DARK_BR_FEAR_CONF              69
#define DARK_WALL_OF_HELLFIRE          74
#define DARK_NETHER_BALL               70
#define DARK_DARKNESS_STORM            71
#define DARK_BECOME_LICH               72

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
	else if (cp_ptr->spell_book == TV_CHEM_BOOK)
		realm = 4;
	else if (cp_ptr->spell_book == TV_DARK_BOOK)
		realm = 5;
/*	else if (cp_ptr->spell_book == TV_MIND_BOOK)
		realm = 5; */
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
	else if (tval == TV_CHEM_BOOK)
		return s_name + s_info[spell + (PY_MAX_SPELLS * 4)].name;
	else if (tval == TV_DARK_BOOK)
		return s_name + s_info[spell + (PY_MAX_SPELLS * 5)].name;
/*	else if (tval == TV_MIND_BOOK)
		return s_name + s_info[spell + (PY_MAX_SPELLS * 6)].name; */
	else /* (prayers) */
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
		case SPELL_ELEMENTAL_STRIKE:
			strnfmt(p, len, " dur 30+d30");
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
		case SPELL_MANA_DEFENCE:
			strnfmt(p, len, " dur 30+d30");
			break;
        case SPELL_MANA_DEFENCE2:
			strnfmt(p, len, " dur 30+d20");
			break;
		case SPELL_SPHERE_CHARM1:
			strnfmt(p, len, " dur 60+d60");
			break;
        case SPELL_SPHERE_CHARM2:
			strnfmt(p, len, " dur 55+d55");
			break;
		case SPELL_FROST_BALL:
			strnfmt(p, len, " dam %d", 30 + plev);
			break;
		case SPELL_ACID_BALL:
			strnfmt(p, len, " dam %d", 40 + plev);
			break;
		case SPELL_DRAGONFIRE:
			strnfmt(p, len, " dam %dd6 + %dd8", plev/2, plev);
			break;
		case SPELL_FIRE_BALL:
			strnfmt(p, len, " dam %d", 55 + plev);
			break;
		case SPELL_ICE_STORM:
			strnfmt(p, len, " dam %d", 50 + (plev * 2));
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
			    strnfmt(p, len, " heal 3d%d", plev / 4);
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
			    strnfmt(p, len, " dam %d + d%d", (plev/2)+30, (plev*2));
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
				strnfmt(p, len, " dam 2d%d+1", (plev / 3) + 1);
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
	
	/* Alchemy */
	if (tval == TV_CHEM_BOOK)
	{
		int plev = p_ptr->lev;

		/* Analyze the spell */
		switch (spell)
		{
			case CHEM_CURE_LIGHT_WOUNDS:
				my_strcpy(p, " heal 2d9", len);
				break;
		    case CHEM_STINKING_CLOUD:
			    strnfmt(p, len, " dam %d", 10 + (plev / 2));
			    break;
		    case CHEM_ACID_ARROW:
			    strnfmt(p, len, " dam %dd6", (3 + ((plev - 6) / 6)));
			    break;
			case CHEM_CHAOS_ARROW:
			    strnfmt(p, len, " dam %dd7 + 1", plev/4);
			    break;
		    case CHEM_POISON_BOLT:
			    strnfmt(p, len, " dam %dd7", 7 + (plev / 5));
			    break;
			case CHEM_INFRAVISION:
				my_strcpy(p, " dur 80+d100", len);
				break;
		    case CHEM_RESIST_ACID:
				strnfmt(p, len, " dur 15+d%d", (plev / 2));
				break;
			case CHEM_RESIST_HEAT_COLD:
				my_strcpy(p, " dur 10+d12", len);
				break;
		    case CHEM_HIT_N_RUN:
				strnfmt(p, len, " dam 2d%d+1", (plev / 3) + 1);
				break;
		}
    }

	/* Black spells */
	if (tval == TV_DARK_BOOK)
	{
		int plev = p_ptr->lev;

		/* Analyze the spell */
		switch (spell)
		{
		    case DARK_INFRAVISION:
			    strnfmt(p, len, " dur %d+d%d ", (plev * 3) + 10), plev;
			    break;
			case DARK_SEE_INVISIBLE:
			    strnfmt(p, len, " dam %d+24", plev);
			    break;
		    case DARK_SHADOW_STING:
			    if (plev < 4) strnfmt(p, len, " dam 2d%d", plev * 2);
			    else if (plev < 8) strnfmt(p, len, " dam 2d%d", 6);
			    else if (plev < 15) strnfmt(p, len, " dam 2d%d", plev-1);
			    else strnfmt(p, len, " dam %dd13", (plev-1)/10);
			    break;
		    case DARK_NOXIOUS_FUMES:
			    strnfmt(p, len, " dam %d+d%d", 10 + (plev/2), plev/4);
			    break;
		    case DARK_WITHER_FOE:
			    strnfmt(p, len, " dam %dd8", plev / 7);
			    break;
			case DARK_DEATH_BOLT:
			    if (plev < 13) strnfmt(p, len, " dam %dd6", 3+(plev/5));
			    else strnfmt(p, len, " dam %d+d%d", (plev*8)/5, plev);
			    break;
		    case DARK_POISON_BOLT:
			    strnfmt(p, len, " dam %dd7", 7 + (plev / 5));
			    break;
		    case DARK_SPEAR_OF_DARK:
			    strnfmt(p, len, " dam %dd8", 3 + (plev/10));
			    break;
			case DARK_ICE_SPEAR:
			    strnfmt(p, len, " dam %dd8", 5 + (plev/5));
			    break;
			case DARK_STENCH_OF_DEATH:
			    strnfmt(p, len, " dam %d + d%d", 26 + plev/2, (plev*3)/2);
			    break;
            case DARK_BEAM_OF_DESTRUCTION:
			    strnfmt(p, len, " dam %d + %dd9", 15 + plev, plev/10);
			    break;
            case DARK_DESTRUCTION_BALL:
			    strnfmt(p, len, " dam %d + %dd11", 24 + plev, plev/10);
			    break;
		    case DARK_CHAOS_STRIKE:
			    strnfmt(p, len, " dam 13d%d", plev);
			    break;
		    case DARK_VENOM_BOLT:
			    strnfmt(p, len, " dam %dd11", 7+((plev+1)/4));
			    break;
			case DARK_RESIST_POISON:
				my_strcpy(p, " dur 20+d20", len);
				break;
			case DARK_RESIST_POISON2:
				my_strcpy(p, " dur 20+d20", len);
				break;
			case DARK_UNDEAD_SKIN:
				my_strcpy(p, " dur 20+d20", len);
				break;
			case DARK_UNHOLY_RESISTANCE:
				my_strcpy(p, " dur 20+d20", len);
				break;
			case DARK_SHADOW_BARRIER:
				my_strcpy(p, " dur 20+d20", len);
				break;
			case DARK_ORB_OF_DEATH:
			    strnfmt(p, len, " dam 3d6 + %d", plev+(plev/2));
			    break;
			case DARK_VAMPIRIC_DRAIN:
			    strnfmt(p, len, " dam %dd11", plev / 3);
			    break;
			case DARK_HOLD_LIFE:
			    strnfmt(p, len, " dur 20 + d%d", 8+(plev/2));
			    break;
			case DARK_SPIRIT_OF_BALROG:
				my_strcpy(p, " dur 30+d30", len);
				break;
		    case DARK_HASTE:
			    strnfmt(p, len, " dur %d+d20", plev);
			    break;
			case DARK_NETHER_BALL:
			    strnfmt(p, len, " dam %dd11", plev);
			    break;
			case DARK_BECOME_LICH:
			    strnfmt(p, len, " dur %d+d%d", plev+5, plev/2);
			    break;
			case DARK_WALL_OF_HELLFIRE:
                if (cp_ptr->flags & CF_ASSASSIN) strnfmt(p, len, " dur (25+d%d)/2", plev/2);
			    else strnfmt(p, len, " dur 25+d%d", plev/2);
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


static void spell_affect_self(int unused)
{
	int py = p_ptr->py;
	int px = p_ptr->px;
	int plev = p_ptr->lev;
	int die;

    die = randint(99) + randint(plev/5);
    if (spellswitch == 30) die += 4 + randint(6);
    if (die < 40) take_hit(randint((plev * 3)/2), "a backfiring spell");
    if (die > 45) (void)hp_player(randint(plev));
    if (die > 65) (void)hp_player(randint(plev));
    if (die > 90) (void)hp_player(randint(plev));
    die = randint(99) + randint(plev/5);
    if (die < 25) msg_print("Your position feels uncertain.");
    if (die < 15) teleport_player(damroll(plev/2, 4));
    else if (die < 25) teleport_player(damroll(2, plev/3 + 1));
    die = randint(99) + randint(plev/5);
    if (spellswitch == 30) die += 3 + randint(7);
    if (die < 5) (void)inc_timed(TMD_CHARM, randint(16) + 12);
    else if (die < 10) (void)inc_timed(TMD_CONFUSED, randint(19) + 11);
    else if (die < 15) (void)inc_timed(TMD_BLIND, randint(25) + 25);
    else if (die < 20) (void)inc_timed(TMD_AMNESIA, randint(19) + 11);
    else if (die < 24) (void)inc_timed(TMD_IMAGE, randint(19) + 11);
    else if (die < 30) (void)inc_timed(TMD_POISONED, randint(30) + 20);
    else if (die < 34)
    {
		   msg_print("You feel your life slipping away!");
		   lose_exp(100 + randint(die * 101));
    }
    die = randint(99) + randint(plev/5);
    if (spellswitch == 30) die += 3 + randint(7);
    if (die > 100)
    {
	     if (p_ptr->silver > PY_SILVER_HEALTHY) p_ptr->silver = p_ptr->silver - 7;
	     if (p_ptr->silver < PY_SILVER_HEALTHY) p_ptr->silver = PY_SILVER_HEALTHY;
    }
    if (die > 95)
    {
         if (p_ptr->slime > PY_SLIME_HEALTHY) p_ptr->slime = p_ptr->slime - 10;
	     if (p_ptr->slime < PY_SLIME_HEALTHY) p_ptr->slime = PY_SLIME_HEALTHY;
    }
    if (die > 90) (void)clear_timed(TMD_CHARM);
    if (die > 80) (void)clear_timed(TMD_AMNESIA);
    if (die > 75)
    {
         (void)clear_timed(TMD_CONFUSED);
         (void)clear_timed(TMD_BLIND);
         (void)clear_timed(TMD_FRENZY);
    }
    if (die > 60) (void)clear_timed(TMD_STUN);
    if (die > 55) (void)clear_timed(TMD_CUT);
    if (die > 50) (void)clear_timed(TMD_POISONED);
    die = randint(99) + randint(plev/5);
    if (spellswitch == 30) die += 3 + randint(7);
    if (die > 75) (void)set_food(p_ptr->food + ((randint(plev + die)) * 6)); 
    if (die < 25) (void)set_food(p_ptr->food - ((randint(plev + die)) * 11));  
    die = randint(99) + randint(plev/5);
    if (die < 34)
    {
         spadjust = (randint(25) - 11);
         if (randint(100) < plev) spadjust = spadjust + 2;
         if (randint(501) < plev) spadjust = spadjust + randint(3);
         if ((spadjust = 0) && (randint(60) < plev)) spadjust = randint(13);
         else if (spadjust = 0) spadjust = (randint(9) - 10);
         if (spadjust > 0) (void)set_timed(TMD_ADJUST, randint(25) + plev);
         if (spadjust < 0) (void)set_timed(TMD_ADJUST, (plev + 25) - randint(plev));
    }
    die = randint(99) + randint(plev/5);
    if (spellswitch == 30) die += 10;
    if (die < 10)
    {
         msg_print("You feel rather mundane.");
         (void)clear_timed(TMD_PROTEVIL);
         (void)clear_timed(TMD_HERO);
         (void)clear_timed(TMD_SHERO);
         (void)clear_timed(TMD_BLESSED);
         (void)clear_timed(TMD_SANCTIFY);
         (void)clear_timed(TMD_SHADOW);
         p_ptr->silver = p_ptr->silver + 1;
    }
    else if (die < 14)
    {
         msg_print("You feel vulnerable.");
         (void)clear_timed(TMD_SHIELD);
         (void)clear_timed(TMD_WSHIELD);
         (void)clear_timed(TMD_OPP_POIS);
         (void)clear_timed(TMD_OPP_ACID);
         (void)clear_timed(TMD_OPP_COLD);
         (void)clear_timed(TMD_OPP_FIRE);
         (void)clear_timed(TMD_OPP_ELEC);
         (void)clear_timed(TMD_FAST);
         spadjust = spadjust - 1;
    }
    else if (die < 18)
    {
    msg_print("Your eyes hurt for a moment.");
    (void)clear_timed(TMD_TSIGHT);
    (void)clear_timed(TMD_SINVIS);
    (void)clear_timed(TMD_SINFRA);
    (void)clear_timed(TMD_BRAIL);
    p_ptr->slime = p_ptr->slime + 1;
    }
    if ((die < 23) && (!p_ptr->timed[TMD_SUST_SPEED])) (void)inc_timed(TMD_SLOW, randint(25) + 15);
    die = randint(99) + randint(plev/5);
    if (spellswitch == 30) die += 4 + randint(6);
    if (die > 100) (void)inc_timed(TMD_SANCTIFY, randint(die / 2) + (25));
    else if (die > 95) (void)inc_timed(TMD_HERO, randint(die / 2) + (25));
    else if (die > 90) (void)inc_timed(TMD_SHERO, randint(die / 2) + (25));
    else if (die > 85) (void)inc_timed(TMD_BLESSED, randint(die) + (25));
    else if (die > 80) (void)inc_timed(TMD_WSHIELD, randint(die) + (25));
    else if (die > 75) (void)inc_timed(TMD_SHADOW, randint(die) + (25));
    else if (die > 70) (void)inc_timed(TMD_PROTEVIL, randint(die / 2) + (25));
    die = randint(99) + randint(plev/5);
    if (spellswitch == 30) die += 3 + randint(7);
    if (die > 98)
    {
		 int time = randint(20) + 20;
         (void)inc_timed(TMD_OPP_POIS, time);
         (void)inc_timed(TMD_OPP_ACID, time);
         (void)inc_timed(TMD_OPP_ELEC, time);
         (void)inc_timed(TMD_OPP_COLD, time);
         (void)inc_timed(TMD_OPP_FIRE, time);
    }
    else if (die > 90)
    {
	     int time = randint(20) + 20;
         (void)inc_timed(TMD_OPP_ACID, time);
         (void)inc_timed(TMD_OPP_ELEC, time);
    }
    else if (die > 82)
    {
	     int time = randint(20) + 20;
         (void)inc_timed(TMD_OPP_COLD, time);
         (void)inc_timed(TMD_OPP_FIRE, time);
    }
    else if (die > 74) (void)inc_timed(TMD_OPP_POIS, randint(21) + 21);
    die = randint(99) + randint(plev/5);
    if (spellswitch == 30) die += 3 + randint(7);
    if (die > 99) (void)inc_timed(TMD_ESP, randint(die * 2) + (25));
    else if (die > 89) (void)inc_timed(TMD_TSIGHT, randint(die * 2) + (25));
    else if (die > 85) (void)inc_timed(TMD_BRAIL, randint(die * 2) + (25));
    else if (die > 75) (void)inc_timed(TMD_SINVIS, randint(die * 2) + (25));
    else if (die > 65) (void)inc_timed(TMD_SINFRA, randint(die * 2) + (25));
    else if (die > 55) (void)detect_monsters_normal();
    else if (die > 50) (void)detect_monsters_invis();
    else if (die > 45) (void)detect_traps();
    else if (die > 42) (void)detect_doors();
    else if (die > 39) (void)detect_stairs();
    else if (die > 35)
    {
	     (void)detect_doors();
	     (void)detect_stairs();
    }
    else if (die > 32) (void)detect_objects_magic();
    else if (die > 27)
    {
	     (void)detect_treasure();
	     (void)detect_objects_gold();
    }
    else if (die > 22) (void)detect_objects_normal();
}

static void spell_adjust_curse(int unused)
{
	int py = p_ptr->py;
	int px = p_ptr->px;
	int plev = p_ptr->lev;
	int die;
	int dir;

    die = randint(99) + randint(plev/5);
    if (spellswitch == 30) die += 1 + randint(9);
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
         msg_print("You fire a small ball of destroy cursed objects.");
		 if (!get_aim_dir(&dir)) return;
		 fire_ball(GF_HOLY_ORB, dir, 0, (plev/15));
    }
    else if (die < 95)
    {
         remove_curse();
		 if (!get_aim_dir(&dir)) return;
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
		 if (!get_aim_dir(&dir)) return;
         msg_print("You fire a ball of destroy cursed objects.");
		 fire_ball(GF_HOLY_ORB, dir, randint(plev/4), (plev/8));
    }
}

static void spell_potluck_stats(int unused)
{
	int py = p_ptr->py;
	int px = p_ptr->px;
	int plev = p_ptr->lev;
	int die;

    int jackpot = randint(5);
    if ((spellswitch == 30) && (randint(100) < 34)) jackpot += randint(2);
    if ((goodluck > 11) && (randint(100) > 90)) spellswitch = 30;
        die = randint(100);
        int die2 = randint(99) + randint(plev/4) - randint(badluck);
        if (spellswitch == 30) die2 += 3 + randint(7);
        if (die < 33)
        {
           if (die2 < 5) dec_stat(A_CHR, 1, TRUE);     
           else if (die2 < 34) do_dec_stat(A_CHR);
           if (die2 > 65) do_res_stat(A_CHR);
           if ((die2 > 96) && (randint(5) == 1))
           {
              do_inc_stat(A_CHR);
              jackpot -= 1;
           }
        }
        die = randint(100);
        die2 = randint(99) + randint(plev/4) - randint(badluck);
        if (spellswitch == 30) die2 += 3 + randint(7);
        if (die < 33)
        {
           if (die2 < 5) dec_stat(A_WIS, 1, TRUE);     
           else if (die2 < 34) do_dec_stat(A_WIS);
           if (die2 > 65) do_res_stat(A_WIS);
           if ((die2 > 96) && (randint(5) == 1) && (jackpot > 0))
           {
              do_inc_stat(A_WIS);
              jackpot -= 1;
           }
        }
        die = randint(100);
        die2 = randint(99) + randint(plev/4) - randint(badluck);
        if (spellswitch == 30) die2 += 3 + randint(7);
        if (die < 33)
        {
           if (die2 < 5) dec_stat(A_INT, 1, TRUE);     
           else if (die2 < 34) do_dec_stat(A_INT);
           if (die2 > 65) do_res_stat(A_INT);
           if ((die2 > 96) && (randint(5) == 1) && (jackpot > 0))
           {
              do_inc_stat(A_INT);
              jackpot -= 1;
           }
        }
        die = randint(100);
        die2 = randint(99) + randint(plev/4) - randint(badluck);
        if (spellswitch == 30) die2 += 3 + randint(7);
        if (die < 33)
        {
           if (die2 < 5) dec_stat(A_DEX, 1, TRUE);     
           else if (die2 < 34) do_dec_stat(A_DEX);
           if (die2 > 65) do_res_stat(A_DEX);
           if ((die2 > 96) && (randint(5) == 1) && (jackpot > 0))
           {
              do_inc_stat(A_DEX);
              jackpot -= 1;
           }
        }
        die = randint(100);
        die2 = randint(99) + randint(plev/4) - randint(badluck);
        if (spellswitch == 30) die2 += 3 + randint(7);
        if (die < 33)
        {
           if (die2 < 5) dec_stat(A_CON, 1, TRUE);     
           else if (die2 < 34) do_dec_stat(A_CON);
           if (die2 > 65) do_res_stat(A_CON);
           if ((die2 > 96) && (randint(5) == 1) && (jackpot > 0))
           {
              do_inc_stat(A_CON);
              jackpot -= 1;
           }
        }
        die = randint(100);
        die2 = randint(99) + randint(plev/4) - randint(badluck);
        if (spellswitch == 30) die2 += 3 + randint(7);
        if (die < 33)
        {
           if (die2 < 5) dec_stat(A_STR, 1, TRUE);     
           else if (die2 < 34) do_dec_stat(A_STR);
           if (die2 > 65) do_res_stat(A_STR);
           if ((die2 > 96) && (randint(5) == 1) && (jackpot > 0))
           {
              do_inc_stat(A_STR);
              jackpot -= 1;
           }
        }
	if (jackpot > 1)
	{
       die = randint(99) + randint(plev/4) - randint(badluck/2);
       die2 = randint(100);
       int max1, cur1, max2, cur2, ii, jj;
       if (spellswitch == 30) die += 3 + randint(7);
       if ((die < 9) && (die2 < 67) && (goodluck < 7))
       {
          dec_stat(A_DEX, randint(2), TRUE);
		  dec_stat(A_WIS, randint(2), TRUE);
		  dec_stat(A_CON, randint(2), TRUE);
		  dec_stat(A_STR, randint(2), TRUE);
		  dec_stat(A_CHR, randint(2), TRUE);
		  dec_stat(A_INT, randint(2), TRUE);
		  if (goodluck > 0) jackpot += 1;
       }
       else if ((die < 9) && (goodluck < 7))
       {
			msg_print("Your body starts to scramble...");

			/* Pick a pair of stats */
			ii = rand_int(A_MAX);
			for (jj = ii; jj == ii; jj = rand_int(A_MAX)); /* loop */

			max1 = p_ptr->stat_max[ii];
			cur1 = p_ptr->stat_cur[ii];
			max2 = p_ptr->stat_max[jj];
			cur2 = p_ptr->stat_cur[jj];

			p_ptr->stat_max[ii] = max2;
			p_ptr->stat_cur[ii] = cur2;
			p_ptr->stat_max[jj] = max1;
			p_ptr->stat_cur[jj] = cur1;

			p_ptr->update |= (PU_BONUS);
		    if (goodluck > 1) jackpot += 1;
       }
       else if ((die < 12) && (goodluck < 17))
       {
          do_dec_stat(A_DEX);
          do_dec_stat(A_WIS);
          do_dec_stat(A_INT);
          do_dec_stat(A_STR);
          do_dec_stat(A_CHR);
		  do_dec_stat(A_CON);
		  if (goodluck > 6) jackpot += 1;
       }
       if (die > 90)
       {
	      do_res_stat(A_STR);
		  do_res_stat(A_CON);
		  do_res_stat(A_DEX);
		  do_res_stat(A_WIS);
		  do_res_stat(A_INT);
		  do_res_stat(A_CHR);
          jackpot -= 1;
       }
    }
   	if (jackpot > 2)
   	{
       die = randint(99) + randint(plev/5) - randint(badluck/2);
       if (die > 98)
       {
          do_inc_stat(A_CHR);
          do_inc_stat(A_WIS);
          do_inc_stat(A_INT);
          do_inc_stat(A_DEX);
          do_inc_stat(A_CON);
          do_inc_stat(A_STR);
          jackpot -= 2;
       }
       if (die < 16) jackpot -= 1;
       if ((die < 67) && (jackpot > 3)) jackpot -= randint(2);
    }
    if ((jackpot > 4) && (randint(100) < 9))
    {
       int dir = 1;
       spellswitch = 30;
       spell_potluck_stats(dir);
    }
    if ((jackpot > 1) && (randint(100) > 75)) p_ptr->luck += randint(jackpot);
    if ((jackpot < 1) && (randint(100) < 10)) p_ptr->luck -= 1;
    if ((jackpot < 0) && (randint(100) < 8)) p_ptr->luck -= 1;
}

static void spell_affect_other(int dir)
{
	int py = p_ptr->py;
	int px = p_ptr->px;
	int plev = p_ptr->lev;
	int die;
	int thirty = 0;
	int lottery = randint(2);
	if (randint(100) < (plev/4)) lottery += 1;
	int olot = lottery;
	if (randint(100) < (plev+2)/2) lottery += 1;
    if (spellswitch == 30) thirty = 1;

    /* damage effects */
    for (; lottery > 0; lottery -= 1)
    {
        die = randint(99) + randint(plev/5);
        if (thirty == 1) die += 3 + randint(7);
        if (die < 5) project_los(GF_DARK, randint(plev * (1 + randint(olot))));
        else if (die < 10) project_los(GF_LITE, randint(plev * (1 + randint(olot))));
        else if (die < 16) project_los(GF_POIS, randint(plev * (1 + randint(olot))));
        else if (die < 22) project_los(GF_COLD, randint(plev * (1 + randint(olot))));
        else if (die < 28) project_los(GF_FIRE, randint(plev * (1 + randint(olot))));
        else if (die < 34) project_los(GF_ACID, randint(plev * (1 + randint(olot))));
        else if (die < 40) project_los(GF_ELEC, randint(plev * (1 + randint(olot))));
        else if (die < 45) (void)dispel_bug(randint(plev * (1 + randint(olot))));
        else if (die < 48) (void)dispel_silver(randint(plev * (1 + olot)));
        else if (die < 52) (void)dispel_demon(randint(plev * (1 + olot)));
        else if (die < 56) (void)dispel_undead(randint(plev * (1 + olot)));
        else if (die < 60) (void)dispel_unnatural(randint(plev * (1 + olot)));
        else if (die < 64) project_los(GF_BRFEAR, randint(plev * (1 + olot)));
        else if (die < 68) project_los(GF_SHARD, randint(plev * (1 + olot)));
        else if (die < 72) project_los(GF_SOUND, randint(plev * (1 + olot)));
        else if (die < 76) project_los(GF_WATER, randint(plev * (1 + olot)));
        else if (die < 80) project_los(GF_NETHER, randint(plev * (1 + olot)));
        else if (die < 84) project_los(GF_PLASMA, randint(plev * (1 + olot)));
        else if (die < 88) project_los(GF_ICE, randint(plev * (1 + olot)));
        else if (die < 92) project_los(GF_GRAVITY, randint(plev * (1 + olot)));
        else if (die < 96) project_los(GF_CHAOS, randint(plev * (1 + olot)));
        else if (die < 100) project_los(GF_HOLY_ORB, randint(plev * (1 + olot)));
        else /* die > 99 */ (void)dispel_monsters(randint(plev * (2 + olot)));
    }
    
    /* other monster effects */
    if ((olot > 1) && (plev < 30)) olot -= 1;
    for (;olot > 0; olot -= 1)
    {
        die = randint(99) + randint(plev/4);
        if (thirty == 1) die += 3 + randint(7);
        if (die < 10) aggravate_monsters(0);
        else if (die < 19) project_los(GF_OLD_HEAL, 45 + randint(45) + (badluck*2) - plev/2);
        else if (die < 28) 
        {
             /* make sure it doesn't clone more than once */
             if (olot == 1)
             {
                 project_los(GF_OLD_CLONE, plev);
             }
             else
             {
                 spellswitch = 13; /* blink monsters */
                 project_los(GF_AWAY_ALL, 2 + randint(15));
                 spellswitch = 0;
             }
        }
        else if (die < 33) 
        {
           spellswitch = 13; /* blink monsters */
           project_los(GF_AWAY_ALL, 2 + randint(15));
           spellswitch = 0;
        }
        else if (die < 38) 
        {
           spellswitch = 14; /* chose what you summon */
           (void)banishment();
           spellswitch = 0;
        }
        else if (die < 48) project_los(GF_OLD_CONF, 45 + plev);
        else if (die < 58) project_los(GF_OLD_SLEEP, 10 + plev);
        else if (die < 68) project_los(GF_TURN_ALL, 55 + plev);
        else if (die < 78) project_los(GF_OLD_SLOW, 45 + plev);
        else if (die < 88) project_los(GF_AWAY_UNDEAD, plev + 10); /* banish unnatural */
        else if (die < 98) project_los(GF_AWAY_EVIL, plev + 10); /* banish evil */
        else if (die < 108) project_los(GF_AWAY_ALL, plev + 10); /* mass banishment */
        else /* die > 107 */ banishment();
    }
    
    if (randint(100) < 50) /* destruct/create effects */
    {
       die = randint(99) + randint(plev/5);
       if (thirty == 1) die += 3 + randint(7);
       if (die < 8)
       {
          fire_ball(GF_MAKE_TRAP, 1, 0, randint(2)); /* lots of traps.. */
          fire_ball(GF_MAKE_TRAP, 2, 0, randint(2));
          fire_ball(GF_MAKE_TRAP, 3, 0, randint(2));
          fire_ball(GF_MAKE_TRAP, 4, 0, randint(2));
          fire_ball(GF_MAKE_TRAP, 5, 0, randint(2));
          fire_ball(GF_MAKE_TRAP, 6, 0, randint(2));
          fire_ball(GF_MAKE_TRAP, 7, 0, randint(2));
          fire_ball(GF_MAKE_TRAP, 8, 0, randint(2));
          fire_ball(GF_MAKE_TRAP, 9, 0, randint(2));
       }
       else if (die < 16)
       {
          fire_ball(GF_KILL_DOOR, 1, 0, 4); /* trap/door destruction */
          fire_ball(GF_KILL_DOOR, 2, 0, 4); 
          fire_ball(GF_KILL_DOOR, 3, 0, 4); 
          fire_ball(GF_KILL_DOOR, 4, 0, 4); 
          fire_ball(GF_KILL_DOOR, 5, 0, 4); 
          fire_ball(GF_KILL_DOOR, 6, 0, 4); 
          fire_ball(GF_KILL_DOOR, 7, 0, 4); 
          fire_ball(GF_KILL_DOOR, 8, 0, 4); 
          fire_ball(GF_KILL_DOOR, 9, 0, 4); 
       }
       else if (die < 24)
       {
          fire_ball(GF_KILL_WALL, 1, (plev*2), randint(3)); /* destroy walls */
          fire_ball(GF_KILL_WALL, 2, (plev*2), randint(3)); 
          fire_ball(GF_KILL_WALL, 3, (plev*2), randint(3)); 
          fire_ball(GF_KILL_WALL, 4, (plev*2), randint(3)); 
          fire_ball(GF_KILL_WALL, 5, (plev*2), randint(3)); 
          fire_ball(GF_KILL_WALL, 6, (plev*2), randint(3)); 
          fire_ball(GF_KILL_WALL, 7, (plev*2), randint(3)); 
          fire_ball(GF_KILL_WALL, 8, (plev*2), randint(3)); 
          fire_ball(GF_KILL_WALL, 9, (plev*2), randint(3)); 
       }
       else if (die < 40)
       {
          fire_ball(GF_KILL_WALL, randint(8), 0, 1); 
          fire_ball(GF_KILL_WALL, randint(8), 0, 1); 
          fire_ball(GF_KILL_WALL, randint(8), 0, 1); 
          fire_ball(GF_KILL_WALL, randint(8), 0, 1); 
          fire_ball(GF_MAKE_DOOR, 1, 0, 1); 
          fire_ball(GF_MAKE_DOOR, 2, 0, 1); 
          fire_ball(GF_MAKE_DOOR, 3, 0, 1); 
          fire_ball(GF_MAKE_DOOR, 4, 0, 1); 
          fire_ball(GF_MAKE_DOOR, 5, 0, 1); 
          fire_ball(GF_MAKE_DOOR, 6, 0, 1); 
          fire_ball(GF_MAKE_DOOR, 7, 0, 1); 
          fire_ball(GF_MAKE_DOOR, 8, 0, 1); 
          fire_ball(GF_MAKE_DOOR, 9, 0, 1); 
          fire_ball(GF_MAKE_DOOR, randint(8), 0, 3); 
       }
       else if (die < 48)
       {
          if (randint(100) < 90) fire_ball(GF_WIZLOCK, 1, 0, 1); 
          if (randint(100) < 90) fire_ball(GF_WIZLOCK, 2, 0, 1); 
          if (randint(100) < 90) fire_ball(GF_WIZLOCK, 3, 0, 1); 
          if (randint(100) < 90) fire_ball(GF_WIZLOCK, 4, 0, 1); 
          if (randint(100) < 80) fire_ball(GF_WIZLOCK, 5, 0, 1); 
          if (randint(100) < 80) fire_ball(GF_WIZLOCK, 6, 0, 1); 
          if (randint(100) < 80) fire_ball(GF_WIZLOCK, 7, 0, 1); 
          if (randint(100) < 80) fire_ball(GF_WIZLOCK, 8, 0, 1); 
          if (randint(100) < 80) fire_ball(GF_WIZLOCK, 9, 0, 1); 
       }
       else if (die < 53)
       {
          fire_ball(GF_MANA, 1, 1, 5); /* destroy objects */
          fire_ball(GF_MANA, 2, 1, 5); 
          fire_ball(GF_MANA, 3, 1, 5); 
          fire_ball(GF_MANA, 4, 1, 5); 
          fire_ball(GF_MANA, 5, 1, 5); 
          fire_ball(GF_MANA, 6, 1, 5); 
          fire_ball(GF_MANA, 7, 1, 5); 
          fire_ball(GF_MANA, 8, 1, 5); 
          fire_ball(GF_MANA, 9, 1, 5); 
       }
       else if (die < 58)
       {
          fire_ball(GF_HOLY_ORB, 1, 0, 5); /* destroy cursed objects */
          fire_ball(GF_HOLY_ORB, 2, 0, 5); 
          fire_ball(GF_HOLY_ORB, 3, 0, 5); 
          fire_ball(GF_HOLY_ORB, 4, 0, 5); 
          fire_ball(GF_HOLY_ORB, 5, 0, 5); 
          fire_ball(GF_HOLY_ORB, 6, 0, 5); 
          fire_ball(GF_HOLY_ORB, 7, 0, 5); 
          fire_ball(GF_HOLY_ORB, 8, 0, 5); 
          fire_ball(GF_HOLY_ORB, 9, 0, 5); 
       }
       else if (die < 68) earthquake(py, px, randint(13));
       else if (die < 78) destroy_area(py, px, 4 + randint(14), TRUE);
       else if (die > 100)
       {
			msg_print("The world changes!");
			/* Leaving (alter reality) */
			p_ptr->leaving = TRUE;
       }
    }
    
    /* mapping & light/dark effects */
    die = randint(99) + randint(plev/5);
    if (thirty == 1) die += 3 + randint(7);
    if (die < 30) (void)lite_area(4, (plev / (4 + randint(5))) + 1);
    else if (die < 50) /* call dark */
    {
       /* most of the time use GF_DARK instead of DARK_WEAK */
       if (randint(100) < 75) spellswitch = 21; 
	   (void)unlite_area(4, (plev / (4 + randint(5))) + 1);
       spellswitch = 0;
    }
    if ((die < 80) && (randint(100) < 15)) map_area();
    if (die > 100) wiz_lite();
    
    /* other effects (may add more later if I think of them) */
    die = randint(99) + randint(plev/5);
    if (thirty == 1) die += 3 + randint(7);
    if (die > 95) /* telekinesis */
	{
       /* spellswitch 24 allows distance pickup */ 
       spellswitch = 24; 
       if (!get_aim_dir(&dir)) return;
	   do_telekinesis();
	   if (spellswitch == 24)
	   {
            /* chance of waking up monsters in path */
            if (randint(100) > plev + 29 - badluck + goodluck) fire_beam(GF_THROW, dir, 0);
       }
       spellswitch = 0;
	}
	else if (die > 90) /* create good object */
	{
         int where = randint(100);
	     if (where < 25) acquirement(py + randint(4), px + randint(4), 1, FALSE);
         else if (where < 50) acquirement(py + randint(4), px - randint(4), 1, FALSE);
         else if (where < 75) acquirement(py - randint(4), px + randint(4), 1, FALSE);
         else acquirement(py - randint(4), px - randint(4), 1, FALSE);
    }
	else if (die > 85)
	{
         int where = randint(100);
         int aquirenum = randint(2) + 1;
         spellswitch = 5;  /* creates object that's not good or great */
         if (where < 25) acquirement(py + randint(4), px + randint(4), aquirenum, FALSE);
         else if (where < 50) acquirement(py + randint(4), px - randint(4), aquirenum, FALSE);
         else if (where < 75) acquirement(py - randint(4), px + randint(4), aquirenum, FALSE);
         else acquirement(py - randint(4), px - randint(4), aquirenum, FALSE);
         spellswitch = 0;
    }
}

/*
 * Hook to specify staff or wand
 */
static bool item_tester_hook_charged(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_WAND:
		case TV_STAFF:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}

/*
 * Summon a creature of the specified type
 * copied from wizard2.c
 * This function is rather dangerous? why?
 */
static void do_call_help(int r_idx, bool slp)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int i, x, y;

	/* Paranoia */
	if (!r_idx) return;
	if (r_idx >= z_info->r_max-1) return;

	/* Try 10 times */
	for (i = 0; i < 10; i++)
	{
		int d = 1;

		/* Pick a location */
		scatter(&y, &x, py, px, d, 0);

		/* Require empty grids */
		if (!cave_empty_bold(y, x)) continue;

		/* Place it (allow groups) */
		if (place_monster_aux(y, x, r_idx, slp, TRUE)) break;
	}
}


static bool cast_mage_spell(int spell)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int dir;

	int plev = p_ptr->lev;

	/* Hack -- chance of "beam" instead of "bolt" */
	/* plev with beam flag, plev/2 otherwise */
	int beam = beam_chance();

    /* spellcasting bonus */	
	if (p_ptr->timed[TMD_BRAIL])
    {
       plev += 10;
       beam += 5;
    }

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
		} /* 10d8 at L21, 12d8 at L30, 14d8 at L40 */

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
		
		case SPELL_SUMMON_DEMONIC_AID:
        {
            int die;
            if (plev > 40) die = randint(100);
            else if (plev > 30) die = randint(90);
            else die = randint(80);
            if (die < 25) do_call_help(989, TRUE);
            else if (die < 50) do_call_help(991, TRUE);
            else if (die < 75) do_call_help(992, TRUE);
            else do_call_help(993, TRUE);

            if (randint(100) + badluck > 70 + goodluck)
            {
                (void)inc_timed(TMD_WITCH, 100 + randint(150-plev));
            }
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

        case SPELL_DRAGONFIRE:
		{
            range = 5;
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_beam(GF_FIRE, dir, damroll(plev/4, 6));
			fire_beam(GF_PLASMA, dir, damroll(plev/4, 6));
			fire_ball(GF_FIRE, dir, damroll(plev/2, 8), 3);
			fire_ball(GF_PLASMA, dir, damroll(plev/2, 8), 3);
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
			if (randint(100) < 10) fire_ball(GF_KILL_WALL, dir, damroll((beam-2), 2), 2);
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
		
		case SPELL_ELEMENTAL_STRIKE:
		{
			(void)inc_timed(TMD_HIT_ELEMENT, randint(30) + 30);
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

        case SPELL_MANA_DEFENCE:
		{
			(void)inc_timed(TMD_WSHIELD, randint(30) + 30);
			break;
		}

        case SPELL_MANA_DEFENCE2:
		{
			(void)inc_timed(TMD_WSHIELD, randint(20) + 30);
			break;
		}
		
		case SPELL_SPHERE_CHARM1:
		{
			(void)inc_timed(TMD_SPHERE_CHARM, randint(60) + 60);
			break;
		}
		
		case SPELL_SPHERE_CHARM2:
		{
			(void)inc_timed(TMD_SPHERE_CHARM, randint(55) + 55);
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
            if (randint(100) + badluck > 66 + goodluck)
            {
                (void)inc_timed(TMD_WITCH, 300 + randint(200-plev));
            }
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

    /* spellcasting bonus */	
	if (p_ptr->timed[TMD_BRAIL])
    {
       plev += 10;
    }

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
			if (randint(47) < (plev + 1 + goodluck/2)) (void)clear_timed(TMD_CHARM);
			if ((plev > 40) && (randint(goodluck) > 6)) (void)inc_timed(TMD_HERO, randint(plev-40) + 7);
			else if ((plev > 30) && (randint(goodluck) + randint(4) > 7)) (void)inc_timed(TMD_HERO, randint((plev-32)/2) + 3);
			break;
		}

		case PRAYER_CALL_LIGHT:
		{
            int toolow = plev/10;
            if (toolow < 1) toolow = 1;
            if ((p_ptr->luck > 20) && (randint(100) < 25)) toolow += 1;
			(void)lite_area(damroll(2, (plev / 2)), randint(toolow) + randint(toolow));
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
			          damroll((plev/10), 8) + plev + randint(plev/2));
			if (randint(plev/3) > 6) lite_line(dir);
			break;
			/* av34 at L20, av51 at L30, av68 at L40, av85 at L50 */
        }	/* averages not including line_lite which adds 6d8 */

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
			(void)dispel_evil(plev + randint(plev * 3));
			(void)hp_player(1000);
			(void)clear_timed(TMD_AFRAID);
			(void)clear_timed(TMD_CHARM);
			(void)clear_timed(TMD_FRENZY);
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
			break;
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
			(void)clear_timed(TMD_CHARM);
			(void)clear_timed(TMD_FRENZY);
			(void)clear_timed(TMD_POISONED);
			(void)clear_timed(TMD_CUT);
			(void)clear_timed(TMD_STUN);
			(void)clear_timed(TMD_BLIND);
			(void)clear_timed(TMD_AMNESIA);
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
			msg_print("You shine your light in all directions.");
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

    /* spellcasting bonus */	
	if (p_ptr->timed[TMD_BRAIL])
    {
       plev += 10;
       beam += 5;
    }

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
			(void)hp_player(12 + (damroll(3, plev / 3)));
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
			int toolow = plev / 7;
			if (toolow < 1) toolow = 1;
			if ((toolow < 1) && (plev > 11)) toolow = 2;
			int die = damroll(toolow, 8);
			fire_bolt(GF_MANA, dir, die);
			(void)confuse_monster(dir, plev + 10);
			if (die > 7) (void)slow_monster(dir);
			break;
        }

		case NEWM_ELEC_STORM:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_ball(GF_ELEC, dir, 30 + (plev/2) + randint(plev * 2), (plev / 14));
			break;
           /* averages: 60 at L20, 75 at L30, 90 at L40, 105 at L50 */
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
            spellswitch = 11;  /* activates earthquake */
			fire_ball(GF_PLASMA, dir, 
                      50 + (3 * plev) + randint((plev * 10)/3), 1 + (plev / 16));
			break;
           /* 111-176 at L20, 141-240 at L30, 171-303 at L40, 201-366 at L50 */
        }

		case NEWM_RIFT:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_beam(GF_GRAVITY, dir, 10 + (plev*2) + damroll(plev, 6));
			break;
            /* 71-170 at L20, 101-250 at L30, 131-330 at L40, 161-410 at L50 */
		}   /* stronger than mage version */

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

        case NEWM_CALL_HELP:
        {
            int die;
            if (plev > 30) die = randint(100);
            else if (plev > 20)  die = randint(90);
            else die = randint(80);
            if ((goodluck > 2) && (randint(103-goodluck) < 9)) die += 11;
            if (die < 8) do_call_help(994, TRUE);
            else if (die < 39) do_call_help(995, TRUE);
            else if (die < 70) do_call_help(996, TRUE);
            else do_call_help(997, TRUE);
			break;
        }
        
        case NEWM_SONG_PROTECTION:
        {
            int time = randint(25 + (plev/2)) + plev;
			(void)inc_timed(TMD_PROTEVIL, time);
			(void)inc_timed(TMD_WSHIELD, time);
			break;
        }
        
        case NEWM_SONG_DISPELLING:
        {
			msg_print("An unbearable discord tortures your foes!");
            spellswitch = 10;  /* a song should not affect golems */
            (void)dispel_monsters(plev/3 + randint(plev * 2));
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

        case NEWM_EXTRA_BLOW:
		{
			(void)inc_timed(TMD_XATTACK, randint(plev-5) + (plev/2));
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
			break;
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

    /* spellcasting bonus */	
	if (p_ptr->timed[TMD_BRAIL])
    {
       plev += 10;
       beam += 5;
    }

	switch (spell)
	{
		case LUCK_ILLUMINATION:
		{
			(void)lite_area(randint(plev / 2), (plev / 10) + 1);
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
            die = randint(99) + randint(plev/5);
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
            msg_print("What do you want to take a picture of?");
			if (!get_aim_dir(&dir)) return (FALSE);
			int flash = 3 + randint(plev/3);
			if (plev < 9) flash = 2 + randint(plev/3);
            spellswitch = 9;
			fire_ball(GF_WATER, dir, 1, plev/22);
            spellswitch = 9;
			fire_ball(GF_LITE, dir, flash-1, plev/15);
			/* GF_WATER for stunning effect */
			/* spellswitch keeps you from discovering water immunity */
			/* spellswitch also makes the light from the flash temporary */
			break;
		}

		case LUCK_IDENTIFY:
		{
			return ident_spell();
		}

		case LUCK_DOOR_CREATION:
		{
            die = randint(99) + randint(plev/4);
            if (die < 3) return (project_los(GF_WIZLOCK, plev/5));
            else if (die < 11) earthquake(py, px, 5);
            else if (die < 16) (void)destroy_doors_touch();
            else if (die < 27) 
            {
                 spellswitch = 11; /* doesn't allow big damage to player */
                 earthquake(py, px, 3);
            }
            else if (die < 42)
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
            die = randint(99) + randint(plev/4);
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
			     (void)stair_creation();
                 return (project_los(GF_GRAVITY, plev/6));
            }
            else  /* (die > 109) */
            {
			     (void)hp_player(damroll(7, 7));
			     (void)stair_creation();
                 return (project_los(GF_GRAVITY, plev/6));
            }
            spellswitch = 0;
			break;
		}

		case LUCK_WORD_OF_RECALL:
		{
            die = randint(99) + randint(plev/4);
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
            die = randint(99) + randint(plev/4);
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
			     (void)inc_timed(TMD_SINFRA, randint(50) + (50));
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
            die = randint(100) + randint(plev/4) + randint(goodluck/2);
            if ((die > 70) && (plev > 9) && (randint(100) < 4))
            {
                     msg_print("You feel a cold breeze.."); 
                     cold_dam(1 + randint(plev/3), "a random cold breeze");
                     if (cp_ptr->flags & CF_CUMBER_GLOVE) die += 2 + randint(5);
                     else die += 10 + (goodluck/5);
            }
            if (die < 10) (void)inc_timed(TMD_OPP_COLD, randint(die) + (10));
            else if (die < 45) (void)inc_timed(TMD_OPP_COLD, randint(10) + (10));
            else if (die < 80) (void)inc_timed(TMD_OPP_COLD, randint(17) + (10));
            else if (die < 100) (void)inc_timed(TMD_OPP_COLD, randint(die / 4) + (12));
            else if (die < 110) (void)inc_timed(TMD_OPP_COLD, randint(die / 3) + (17));
            else
            {
                spellswitch = 7;
                brand_weapon();
                (void)inc_timed(TMD_OPP_COLD, randint(20) + (11));
                spellswitch = 0;
            }
            break;
        }

		case LUCK_RECHARGING:
        {
            die = randint(99) + randint(plev/5);
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
            die = randint(99) + randint(plev/5);
            if (die < 6) (void)dispel_bug(1 + (randint(plev / 3)));
			else if (die < 94) (void)dispel_bug(1 + (randint(plev / 2)));
			else if (die < 100) (void)dispel_bug(1 + (randint(plev - 3)));
			else (void)dispel_bug(4 + (randint(plev)));
			break;
        }

		case LUCK_DETECT_INVISIBLE:
		{
            die = randint(99) + randint(plev/5);
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
            die = randint(99) + randint(plev/5);
            if (die < 10) (void)inc_timed(TMD_OPP_POIS, randint(5) + 5);
            else if (die < 25) (void)inc_timed(TMD_WOPP_POIS, randint(21) + 20);
            else if (die < 75) (void)inc_timed(TMD_OPP_POIS, randint(20) + 20);
            else if (die < 80)
            {
               (void)inc_timed(TMD_OPP_POIS, randint(20) + 20);
               (void)inc_timed(TMD_WOPP_POIS, randint(15) + 10);
            }
            else if (die < 90)
            {
               (void)set_timed(TMD_POISONED, p_ptr->timed[TMD_POISONED] / 2);
               (void)inc_timed(TMD_OPP_POIS, randint(20) + 20);
            }
            else if (die < 105)
            {
			   (void)clear_timed(TMD_POISONED);
               (void)inc_timed(TMD_OPP_POIS, randint(21) + 24);
               (void)inc_timed(TMD_WOPP_POIS, randint(15) + 15);
            }
            else
            {
			   (void)clear_timed(TMD_POISONED);
               p_ptr->silver = PY_SILVER_HEALTHY;
               p_ptr->slime = PY_SLIME_HEALTHY;
               (void)inc_timed(TMD_OPP_POIS, randint(26) + 24);
               (void)inc_timed(TMD_WOPP_POIS, randint(21) + 20);
            }
			break;
		}
        
        case LUCK_CURE_CAUSE:
        {
            die = randint(99) + randint(plev/4);
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
		
        case LUCK_STONE_TO_MUD: /* replaces rogue version of telekinesis */
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			(void)wall_to_mud(dir);
			break;
		}
		
		case LUCK_TELEKINESIS2:
		{
            /* spellswitch 24 allows distance pickup */ 
            spellswitch = 24; 
            if (!get_aim_dir(&dir)) return (FALSE);
			do_telekinesis();
			if (spellswitch == 24)
			{
               /* chance of waking up monsters in path */
               if (randint(100) > plev + 29 - badluck + goodluck) fire_beam(GF_THROW, dir, 0);
            }
            spellswitch = 0;
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
            die = randint(99) + randint(plev/4);
			(void)detect_traps();
			if (die > 98) (void)destroy_doors_touch();
			else if (die > 108)
            {
               spellswitch = 6; /* increses radius for destroy traps */
                                /* and keeps it from destroying doors */
               (void)destroy_doors_touch();
               spellswitch = 0;
            }
			break;
        }

		case LUCK_DETECT_OBJECTS:
		{
            die = randint(99) + randint(plev/5);
            if (die > 105) (void)detect_objects_magic();
            else if (die > 5) (void)detect_objects_normal();
            else msg_print("You detect that there are objects on the level.");
			break;
        }

		case LUCK_MAP_AREA:
		{
            die = randint(99) + randint(plev/5);
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
		
		case LUCK_ADJUST_CURSE: /* add chance to make the weapon blessed */
		{
            dir = 5;
            (void)spell_adjust_curse(dir);
			break; /* (direction is unused) */
        }
        
        case LUCK_DETECT_ENCHANTMENT2:
		{
            die = randint(99) + randint(plev/5);
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
			fire_bolt(GF_MISSILE, dir, 1 + damroll(2, (plev/3) + 1));
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
	             u32b t1, t2, t3, t4;
	             u32b f1 = 0L, f2 = 0L, f3 = 0L, f4 = 0L;
	             object_type *o_ptr;
	             int k;

	         /* Check for aggravation */
	         for (k = INVEN_WIELD; k < INVEN_TOTAL; k++)
	         {
                 o_ptr = &inventory[k];
                 
		         /* Skip non-objects */
		         if (!o_ptr->k_idx) continue;

		         /* Extract the flags */
		         object_flags(o_ptr, &t1, &t2, &t3, &t4);

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
            spadjust = randint(20) - 9;
            if (randint(100) < plev) spadjust += 2;
            if (randint(501) < plev) spadjust += randint(3);
            if ((spadjust == 0) && (randint(45) < plev)) spadjust = 6 + randint(3);
            if (spadjust == 0) spadjust = randint(3) - 5;
            if (spadjust > 0) (void)inc_timed(TMD_ADJUST, randint(25) + plev);
            if (spadjust < 0) (void)inc_timed(TMD_ADJUST, (plev + 25) - randint(plev));
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
            die = randint(99) + randint(plev/4);
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
            die = randint(99) + randint(plev/3);
            if (die < 5)
            {
               msg_print("The spell gets a little out of hand..");
               summon_specific(py, px, p_ptr->depth + 10, 0);
            }
            else if (die < 10)
            {
               msg_print("You summon animals.");
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
               (void)banishment();
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
            else if (die < 108)
            {
                 (void)mass_banishment();
                 (void)banishment();
                 spellswitch = 14; /* chose what you summon */
                 (void)banishment();
                 spellswitch = 0;
            }
            else /* (die > 107) */
            {
                 (void)mass_banishment();
                 (void)banishment();
            }
            break;  
        }
        
        case LUCK_AFFECT_SELF:
        {
            int unused = 0;
            (void)spell_affect_self(unused);
            die = randint(99) + randint(plev/5);
            if (die > 105) wiz_lite();
            else if (die > 100) return identify_fully();
            else if (die > 95) map_area();
            else if (die > 85) return ident_spell();
            else if (die > 80) restore_level();
            else if (die > 77) gain_exp(randint(plev * 3));
            else if (die > 70) p_ptr->luck = p_ptr->luck + randint(2);
            if (die < 8) p_ptr->luck = p_ptr->luck - randint(2);
            break;  
        }
        
        case LUCK_AFFECT_OTHER:
        {
            dir = 5;
            (void)spell_affect_other(dir);
			break; /* (direction is unused) */
        }
        
        case LUCK_POTLUCK_STATS:
        {
            dir = 5;
            (void)spell_potluck_stats(dir);
			break; /* (direction is unused) */
        }
        
        case LUCK_AQUIREMENT:
        {
            die = randint(99) + randint(plev/5);
            if ((plev == 50) && (goodluck > 0)) die += 1;
            if (goodluck > 10) die += 1;
            int lottery = randint(1000);
            int jackpot = 0;
            if (die == 110) jackpot = randint(11);
            if (jackpot == 10) lottery = 999;
            if (jackpot == 9) lottery = 776;
            if (jackpot == 8) lottery = 888;
            if (jackpot == 7) lottery = 777;
            if (jackpot == 6) lottery = 666;
            if (jackpot == 5) lottery = 555;
            if (jackpot == 4) lottery = 444;
            if (jackpot == 3) lottery = 333;
            int where = randint(100);
            if (die < 10)
            {
               msg_print("A broken bottle appears along with the guy who was drinking.");
               
               int aquirenum = randint(2);
               if ((plev > 47) && (randint(100) < 50)) aquirenum = aquirenum + randint(3);
               if ((plev > 49) && (randint(100) < 90)) aquirenum = aquirenum + randint(4);
               if (randint(100) < 3) aquirenum = 1;
               if (aquirenum == 9) do_call_help(712, TRUE); /* dark fairy fool */
               else if (aquirenum == 8) do_call_help(692, TRUE); /* lesser titan */
               else if (aquirenum == 7) do_call_help(546, TRUE); /* cyclops */
               else if (aquirenum == 6) do_call_help(554, TRUE); /* extril */
               else if (aquirenum == 5) do_call_help(508, TRUE); /* master assassin */
               else if (aquirenum == 4) do_call_help(459, TRUE); /* assassin */
               else if (aquirenum == 3) do_call_help(354, TRUE); /* lightning rogue */
               else if (aquirenum == 2) do_call_help(142, TRUE); /* bandit */
               else do_call_help(55, TRUE); /* scruffy looking hobbit */
            }
            if (die < 20)
            {
               msg_print("A broken bottle appears and shatters violently.");
               (void)inc_timed(TMD_CUT, 4);
            }
            else if (die < 35)
            {
               spellswitch = 5;  /* creates object that's not good or great */
               if (where < 25) acquirement(py + randint(4), px + randint(4), 1, FALSE);
               else if (where < 50) acquirement(py + randint(4), px - randint(4), 1, FALSE);
               else if (where < 75) acquirement(py - randint(4), px + randint(4), 1, FALSE);
               else acquirement(py - randint(4), px - randint(4), 1, FALSE);
               spellswitch = 0;
               take_hit(randint(10), "delivery price of aquirement");
               (void)inc_timed(TMD_STUN, 2);
            }
            else if (die < 45)
            {
               int aquirenum = randint(2) + 1;
               spellswitch = 5;  /* creates object that's not good or great */
               if (where < 25) acquirement(py + randint(4), px + randint(4), aquirenum, FALSE);
               else if (where < 50) acquirement(py + randint(4), px - randint(4), aquirenum, FALSE);
               else if (where < 75) acquirement(py - randint(4), px + randint(4), aquirenum, FALSE);
               else acquirement(py - randint(4), px - randint(4), aquirenum, FALSE);
               spellswitch = 0;
               take_hit(randint(9 + aquirenum), "delivery price of aquirement");
               (void)inc_timed(TMD_STUN, 1 + aquirenum);
            }
            else if (die < 75)  /* make good but not great object */
            {
               if (where < 25) acquirement(py + randint(4), px + randint(4), 1, FALSE);
               else if (where < 50) acquirement(py + randint(4), px - randint(4), 1, FALSE);
               else if (where < 75) acquirement(py - randint(4), px + randint(4), 1, FALSE);
               else acquirement(py - randint(4), px - randint(4), 1, FALSE);
               int price = damroll(2, randint(10)) - (goodluck/2) + (badluck/2);
               if (price < 1) price = 1;
               take_hit(price, "delivery price of aquirement");
               if (randint(100) < 25) summon_specific(py, px, p_ptr->depth + 1, 0);
               p_ptr->silver = p_ptr->silver + 1;
               (void)inc_timed(TMD_STUN, randint(5));
            }
            else if (die < 85)  /* make 2-3 good objects */
            {
               int aquirenum = randint(2) + 1;
               if (where < 25) acquirement(py + randint(4), px + randint(4), aquirenum, FALSE);
               else if (where < 50) acquirement(py + randint(4), px - randint(4), aquirenum, FALSE);
               else if (where < 75) acquirement(py - randint(4), px + randint(4), aquirenum, FALSE);
               else acquirement(py - randint(4), px - randint(4), aquirenum, FALSE);
               int price = damroll(2 + aquirenum, 2 + randint(8));
               price -= ((goodluck/4) * aquirenum);
               price += (badluck * aquirenum);
               if (price < 2) price = 2;
               take_hit(price, "delivery price of aquirement");
               if (randint(100) < 50) summon_specific(py, px, p_ptr->depth + aquirenum, 0);
               p_ptr->silver = p_ptr->silver + aquirenum;
               (void)inc_timed(TMD_STUN, aquirenum * randint(5));
            }
            else if (die < 95)  /* high cost aquirement */
            {
               if (where < 25) acquirement(py + randint(4), px + randint(4), 1, TRUE);
               else if (where < 50) acquirement(py + randint(4), px - randint(4), 1, TRUE);
               else if (where < 75) acquirement(py - randint(4), px + randint(4), 1, TRUE);
               else acquirement(py - randint(4), px - randint(4), 1, TRUE);
               int price = damroll(6, 8 + randint(7)) - (goodluck) + (badluck);
               if (price < 4) price = 4;
               take_hit(price, "delivery price of aquirement");
               p_ptr->silver = p_ptr->silver + 6;
               (void)inc_timed(TMD_STUN, 5 * randint(5));
               summon_specific(py, px, (plev * 3/2) + 1, 0);
            }
            else if (die < 100)  /* lower cost aquirement */
            {
               if (where < 25) acquirement(py + randint(4), px + randint(4), 1, TRUE);
               else if (where < 50) acquirement(py + randint(4), px - randint(4), 1, TRUE);
               else if (where < 75) acquirement(py - randint(4), px + randint(4), 1, TRUE);
               else acquirement(py - randint(4), px - randint(4), 1, TRUE);
               int price = damroll(6, 5 + randint(5)) - (goodluck * 2) + (badluck);
               if (price < 1) price = 1;
               take_hit(price, "delivery price of aquirement");
               p_ptr->silver = p_ptr->silver + 3;
               (void)inc_timed(TMD_STUN, 3 * randint(5));
               summon_specific(py, px, plev + 1, 0);
            }
            else if (die < 105)  /* very high cost *aquirement* */
            {
               int aquirenum = randint(2) + 1;
               if (where < 25) acquirement(py + randint(4), px + randint(4), aquirenum, TRUE);
               else if (where < 50) acquirement(py + randint(4), px - randint(4), aquirenum, TRUE);
               else if (where < 75) acquirement(py - randint(4), px + randint(4), aquirenum, TRUE);
               else acquirement(py - randint(4), px - randint(4), aquirenum, TRUE);
               int price = damroll(6, 7 + aquirenum + randint(7));
               price -= ((goodluck/2) * aquirenum);
               price += ((badluck/3) * aquirenum);
               if (price < (aquirenum + 3)) price = aquirenum + 3;
               take_hit(price, "delivery price of aquirement");
               p_ptr->silver = p_ptr->silver + 4 + aquirenum;
               (void)inc_timed(TMD_STUN, (4 + aquirenum) * randint(5));
               summon_specific(py, px, (plev * 3/2) + aquirenum, 0);
               summon_specific(py, px, p_ptr->depth + 8 + aquirenum, 0);
            }
            else /* (die > 104)  no cost aquirement */
            {
               msg_print("You've won the lottery!");
               int aquirenum = randint(2);
               if (lottery > 990) aquirenum += randint(2);
               if (lottery == 333) aquirenum += 1;
               if (lottery == 444) aquirenum = 4;
               if (lottery == 555) p_ptr->luck += 1;
               if (lottery == 666) p_ptr->luck -= randint(2);
               if (lottery == 777)
               {
                  aquirenum = 5;
                  p_ptr->luck += randint(2);
               }
               if (lottery == 776)
               {
                  (void)inc_timed(TMD_FRENZY, (9 + aquirenum) * randint(9));
                  msg_print("One number off of jackpot, you get really frustrated!");
                  aquirenum = 1;
               }
               if (lottery == 888)
               {
                  aquirenum = 8; /* good but not great */
                  if (where < 25) acquirement(py + randint(4), px + randint(4), aquirenum, FALSE);
                  else if (where < 50) acquirement(py + randint(4), px - randint(4), aquirenum, FALSE);
                  else if (where < 75) acquirement(py - randint(4), px + randint(4), aquirenum, FALSE);
                  else acquirement(py - randint(4), px - randint(4), aquirenum, FALSE);
                  msg_print("You've hit the jackpot of quantity but less quality.");
               }
               else
               {
               if (where < 25) acquirement(py + randint(4), px + randint(4), aquirenum, TRUE);
               else if (where < 50) acquirement(py + randint(4), px - randint(4), aquirenum, TRUE);
               else if (where < 75) acquirement(py - randint(4), px + randint(4), aquirenum, TRUE);
               else acquirement(py - randint(4), px - randint(4), aquirenum, TRUE);
               if (aquirenum == 5) msg_print("You've hit the extreme jackpot!!!");
               else if (aquirenum > 2) msg_print("You've hit the jackpot!!");
               else if (badluck > 10) take_hit(randint(10), "delivery price of aquirement");
               }
            }
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
            die = randint(99) + randint(plev/4);
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
            die = randint(99) + randint(plev/4);
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
			      if (p_ptr->weakresist_pois) dam = dam / 2;
			      take_hit(dam, "a toxic smell");
			      (void)inc_timed(TMD_POISONED, rand_int(dam) + 10);
			      if (p_ptr->weakresist_pois) (void)dec_timed(TMD_POISONED, randint(11));
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
			fire_bolt_or_beam(beam + 10, GF_CHAOS, dir, damroll(plev, 8));
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
            return (project_los(GF_CHAOS, (plev/2) + randint(plev)));
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
               (project_los(GF_AWAY_ALL, 2 + randint(15)));
			   spellswitch = 0;
            }
            if (randint(100) < 35) return (project_los(GF_BRFEAR, randint(plev)));
			break;
        }
        
        case LUCK_BREATHE_CHAOS:
        {
			if (!get_aim_dir(&dir)) return (FALSE);
			if (plev > 43) die = 4;
			else die = 3;
            int dieb = randint(62 + die);
			fire_ball(GF_CHAOS, dir, damroll(plev, 9) + (dieb/3), die);
			if (dieb < 30) fire_ball(GF_AWAY_ALL, dir, 1 + randint(8), die);
			else if (dieb < 60) fire_ball(GF_BRFEAR, dir, 1 + randint(12), die);
			else if (dieb < 63)
			{
                 spellswitch = 11;  /* activates earthquake */
                 range = 21; /* small earthquake */
                 fire_ball(GF_PLASMA, dir, 6 + randint(7), die);
            }
            else inc_timed(TMD_IMAGE, randint(3));
			break;
        }
        
        case LUCK_BIZZARE_EFFECTS:
        {
            die = randint(100) + goodluck - badluck/5;
            /* slightly raises chances of good stuff in called random effect functions */
            if (badluck < 13) spellswitch = 30; 
            /* those with very bad luck have a very small chance of getting very lucky */
            if ((badluck > 15) && (randint(100) < 4))
            {
               die += badluck + randint(badluck);
            }
            dir = 5;
            int unused = 0;
            if (die < 0) /* very rare: only every happens with 10+ bad luck */
            {
			   msg_print("You are surrounded by a malignant aura.");
			   /* Decrease all stats (permanently) */
			   (void)dec_stat(A_STR, 2 + randint(11), TRUE);
			   (void)dec_stat(A_INT, 2 + randint(11), TRUE);
			   (void)dec_stat(A_WIS, 2 + randint(11), TRUE);
			   (void)dec_stat(A_DEX, 2 + randint(11), TRUE);
			   (void)dec_stat(A_CON, 2 + randint(11), TRUE);
			   (void)dec_stat(A_CHR, 3 + randint(12), TRUE);
            }
            else if (die < 2)
            {
			   msg_print("You are surrounded by a malignant aura.");
			   /* Decrease all stats (permanently, but by less) */
			   (void)dec_stat(A_STR, randint(2), TRUE);
			   (void)dec_stat(A_INT, randint(2), TRUE);
			   (void)dec_stat(A_WIS, randint(2), TRUE);
			   (void)dec_stat(A_DEX, randint(2), TRUE);
			   (void)dec_stat(A_CON, randint(2), TRUE);
			   (void)dec_stat(A_CHR, randint(3), TRUE);
            }
            else if (die < 5) 
            {
			   msg_print("You are surrounded by an unpleasant aura.");
			   /* Decrease all stats (not permanently) */
			   (void)dec_stat(A_STR, randint(3), FALSE);
			   (void)dec_stat(A_INT, randint(3), FALSE);
			   (void)dec_stat(A_WIS, randint(3), FALSE);
			   (void)dec_stat(A_DEX, randint(3), FALSE);
			   (void)dec_stat(A_CON, randint(3), FALSE);
			   (void)dec_stat(A_CHR, randint(6), FALSE);
            }
            else if (die < 15) 
            {
               msg_print("You feel your life draining away.");
               p_ptr->exp -= (p_ptr->exp / 6);
			   p_ptr->max_exp -= (p_ptr->max_exp / 20);
			   check_experience();
            }
            else if (die < 25)
            {
               msg_print("You feel very unlucky.");
               p_ptr->luck -= 2 + randint(4);
            }
            else if (die < 35)
            {
               /* die2 range is 1 to 29 */
               int die2 = 11 + randint(13 + goodluck/4) - badluck/2;
               if (badluck > 15) die2 = die2 - 1;
               /* less than 12 can only happen with bad luck */
               if (die2 == 2) do_call_help(754, TRUE); /* veriety quylthulg */
               else if (die2 == 3) do_call_help(812, TRUE); /* clear dust bunnies */
               else if (die2 == 4) do_call_help(712, TRUE); /* dark fairy fool */
               else if (die2 == 5) do_call_help(638, TRUE); /* shadow minions */
               else if (die2 == 6) do_call_help(713, TRUE); /* horned devil lord */
               else if (die2 == 7) do_call_help(710, TRUE); /* dullahan */
               else if (die2 == 8) do_call_help(703, TRUE); /* skull druj */
               else if (die2 == 9) do_call_help(688, TRUE); /* barbazu */
               else if (die2 == 10) do_call_help(676, TRUE); /* quylthulg shrieker */
               else if (die2 == 11) do_call_help(684, TRUE); /* will be gnawing bug */
               else if (die2 == 12) do_call_help(661, TRUE); /* wemu vyrm */
               else if (die2 == 13) do_call_help(692, TRUE); /* rayem */
               else if (die2 == 14) do_call_help(659, TRUE); /* gorgon */
               else if (die2 == 15) do_call_help(644, TRUE); /* greater basilisk */
               else if (die2 == 16) do_call_help(631, TRUE); /* doppleganger */
               else if (die2 == 17) do_call_help(616, TRUE); /* shadow raven */
               else if (die2 == 18) do_call_help(604, TRUE); /* black unicorn */
               else if (die2 == 19) do_call_help(603, TRUE); /* giant ameoba slime */
               else if (die2 == 20) do_call_help(588, TRUE); /* giant static toadstool */
               else if (die2 == 21) do_call_help(598, TRUE); /* trooping fairies */
               else if (die2 == 22) do_call_help(511, TRUE); /* singing vyrms */
               else if (die2 == 23) do_call_help(509, TRUE); /* hellhound */
               else if (die2 == 24) do_call_help(569, TRUE); /* animated staff of summoning */
               /* higher than 22 can only happen with good luck */
               else if (die2 == 25) do_call_help(605, TRUE); /* ancient zhelung */
               else if (die2 == 26) do_call_help(400, TRUE); /* creeping adamantite coins */
               else if (die2 == 27) do_call_help(522, TRUE); /* hairy quylthulg */
               else if (die2 == 28) do_call_help(495, TRUE); /* mature green dragon */
               else if (die2 > 28) do_call_help(997, TRUE); /* white unicorn helper */
               /* 1 can only happen with maximum bad luck */
               else /* 1 */ do_call_help(798, TRUE); /* plague slime */
            }
            /* the following four effects have both good and bad possibilities */
            /* but the good ones happen more often */
            else if (die < 37) 
            {
               msg_print("You feel lucky.");
               p_ptr->luck += randint(2);
            }
            else if (die < 45) (void)spell_adjust_curse(dir);
            else if (die < 55) (void)spell_affect_self(unused);
            else if (die < 65) (void)spell_potluck_stats(dir);
            else if (die < 85) (void)spell_affect_other(dir);
            else if (die < 90)
            {
			     /* Mana Bolt */
			     if (!get_aim_dir(&dir)) return (FALSE);
                 fire_bolt(GF_MANA, dir, 200 + randint(plev) + 1);
            }
            else if (die < 95)
            {
			     /* Mana Ball */
			     if (!get_aim_dir(&dir)) return (FALSE);
			     fire_ball(GF_MANA, dir, 250 + randint(plev), 3);
            }
            else if (die < 100)
		    {
	             msg_print("You are surrounded by a powerful aura.");
			     /* Dispel monsters */
			     dispel_monsters(700 + randint(277));
		    }
		    else /* very rare: like best wonder effect but better */
		    {
                msg_print("You feel very lucky.");
                dispel_monsters((99 + randint(51)) * plev/10);
		        slow_monsters();
		        sleep_monsters();
		        hp_player(150 * (1 + randint(5)));
                p_ptr->luck += 2 + randint(3);
            }
			break;
        }
    }

	/* Success */
	return (TRUE);
}

static bool cast_chem_spell(int spell)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

    int die;
	int dir;

	int plev = p_ptr->lev;

    /* spellcasting bonus */	
	if (p_ptr->timed[TMD_BRAIL])
    {
       plev += 10;
    }

	switch (spell)
	{

		case CHEM_CURE_LIGHT_WOUNDS:
		{

			(void)hp_player(damroll(2, 9));
			(void)dec_timed(TMD_CUT, 10);
			break;
		}
		
		case CHEM_LIGHT_AREA:
		{
            die = randint(99) + randint(plev/5);
            if (plev > 25) die = die + 10;
            if (plev > 40) die = die + 10;
            if (plev > 50) die = die + 5;
            if (die < 65)
            {
               spellswitch = 4; /* does not light room */
			   if (plev > 13) (void)lite_area(damroll(2, (plev / 2)), 2 + randint(plev / 7));
			   else (void)lite_area(damroll(2, (plev / 2)), 2 + randint(2));
               spellswitch = 0;
            }
            else if (die < 91)
            {
               spellswitch = 4; /* does not light room */
			   if (plev > 17) (void)lite_area(damroll(4, (plev / 3)), 5 + randint(plev / 9));
			   else (void)lite_area(damroll(4, (plev / 3)), 3 + randint(3));
               spellswitch = 0;
            }
            else
            {
               (void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
            }
			break;
		}

		case CHEM_STINKING_CLOUD:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_ball(GF_POIS, dir, 10 + (plev / 2), 2);
			break;
		}

		case CHEM_DISINFECTANT:
		{
            spellswitch = 16; /* dispel slime in bug_spray function */
			(void)dec_timed(TMD_POISONED, 2 + randint(plev/2));
			(void)dec_timed(TMD_CUT, 5 + randint(plev/3));
			(void)dispel_bug(randint(plev / 2));
            if ((randint(100) < 34) && (p_ptr->slime > PY_SLIME_HEALTHY)) p_ptr->slime = p_ptr->slime - 1;
            if ((randint(100) < 10) && (plev > 9))
            {
               spellswitch = 16; /* dispel slime in bug_spray function */
			   (void)dec_timed(TMD_POISONED, randint(4));
			   (void)dec_timed(TMD_CUT, randint(plev/9));
               if ((randint(100) < (plev * 3)) && (p_ptr->slime > PY_SLIME_HEALTHY)) p_ptr->slime = p_ptr->slime - 1;
			   (void)dispel_bug(randint(plev / 3));
			   if (randint(100) < 75) (void)inc_timed(TMD_WOPP_POIS, randint(13) + 5);
            }
            spellswitch = 0;
			break;
		}

		case CHEM_PHASE_DOOR:
		{
			teleport_player(10);
			break;
		}

		case CHEM_FIND_TRAPS_DOORS:
		{
			(void)detect_traps();
			(void)detect_doors();
			(void)detect_stairs();
			break;
		}

		case CHEM_DETECT_MONSTERS:
		{
			(void)detect_monsters_normal();
			break;
		}

		case CHEM_CURE_POISON:
		{
			(void)clear_timed(TMD_POISONED);
			break;
		}

		case CHEM_ACID_ARROW:
		{
            /* if player is an archer, make sure he is wielding a bow */
			if ((cp_ptr->flags & CF_EXTRA_SHOT) &&
			    (p_ptr->ammo_tval != TV_ARROW))
            {
               msg_print("You must be wielding a bow to fire an acid arrow.");
               return (FALSE);
            }
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_bolt(GF_ACID, dir, damroll(3+((plev-6)/6), 6));
			break;
		}
		
		case CHEM_INFRAVISION:
		{
		    (void)inc_timed(TMD_SINFRA, randint(100) + 10 + (plev * 2));
			break;
		}

        case CHEM_RESIST_ACID:
		{
			(void)inc_timed(TMD_OPP_ACID, randint(plev/2) + 15);
			break;
		}

		case CHEM_CURE_MODERATE_WOUNDS:
		{
			(void)hp_player(damroll(6, 8));
			(void)set_timed(TMD_CUT, (p_ptr->timed[TMD_CUT] / 3) - 2);
			break;
		}

		case CHEM_RESIST_HEAT_COLD:
		{
			(void)inc_timed(TMD_OPP_FIRE, randint(12) + 10);
			(void)inc_timed(TMD_OPP_COLD, randint(12) + 10);
			break;
		}

		case CHEM_SATISFY_HUNGER:
		{
			(void)set_food(PY_FOOD_MAX - 1);
			break;
		}

		case CHEM_CHEMICAL_RAGE:
		{
			(void)hp_player(plev/3);
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

        case CHEM_STRONG_DISINFECTANT:
		{
            spellswitch = 16; /* dispel slime in bug_spray function */
			(void)dec_timed(TMD_POISONED, (5 + randint(plev)));
			(void)dec_timed(TMD_CUT, 10 + randint(plev/2));
			(void)dec_timed(TMD_STUN, 5 + randint(plev/2));
			(void)dispel_bug(2 + (randint(plev / 2)));
            if ((randint(100) < 34) && (p_ptr->slime > PY_SLIME_HEALTHY)) p_ptr->slime = p_ptr->slime - 2;
            if ((randint(100) < 34) && (p_ptr->silver > PY_SILVER_HEALTHY)) p_ptr->silver = p_ptr->silver - 1;
            if ((randint(100) < 10) && (plev > 24))
            {
               spellswitch = 16; /* dispel slime in bug_spray function */
			   (void)dec_timed(TMD_POISONED, randint(plev/2));
			   (void)dec_timed(TMD_CUT, randint(plev/5));
			   (void)dec_timed(TMD_STUN, randint(plev/8));
               if ((randint(100) < (plev * 3)) && (p_ptr->slime > PY_SLIME_HEALTHY)) p_ptr->slime = p_ptr->slime - 2;
               if ((randint(100) < (plev * 3)) && (p_ptr->silver > PY_SILVER_HEALTHY)) p_ptr->silver = p_ptr->silver - 1;
			   (void)dispel_bug(1 + (randint(plev / 2)));
			   (void)inc_timed(TMD_WOPP_POIS, randint(15) + 10);
            }
			(void)dec_timed(TMD_CONFUSED, 5 + randint(plev/2));
			(void)dec_timed(TMD_BLIND, 5 + randint(plev/2));
			(void)dec_timed(TMD_FRENZY, 3 + randint(plev/3));
			(void)dec_timed(TMD_AMNESIA, 3 + randint(plev/3));
			(void)dec_timed(TMD_IMAGE, randint(plev/3));
            spellswitch = 0;
			break;
		}

		case CHEM_RESIST_POISON:
		{
			(void)inc_timed(TMD_OPP_POIS, randint(20) + 20);
			break;
		}

		case CHEM_HEAL:
		{
			(void)hp_player(damroll(10, (plev/2))); /* (max250) */
			(void)clear_timed(TMD_STUN);
			(void)clear_timed(TMD_CUT);
			break;
		}
		
		case CHEM_DETECT_TREASURE:
        {
			(void)detect_treasure();
			(void)detect_objects_gold();
			break;
		}

		case CHEM_TRAP_DOOR_DESTRUCTION:
		{
			(void)destroy_doors_touch();
			break;
		}

		case CHEM_DETECT_OBJECT:
		{
			(void)detect_objects_normal();
			break;
        }

		case CHEM_IDENTIFY:
		{
			return ident_spell();
		}

		case CHEM_DETECT_INVISIBLE:
		{
			(void)detect_monsters_invis();
			break;
		}

		case CHEM_RESISTANCE:
		{
			int time = randint(20) + 20;
			(void)inc_timed(TMD_OPP_ACID, time);
			(void)inc_timed(TMD_OPP_ELEC, time);
			(void)inc_timed(TMD_OPP_FIRE, time);
			(void)inc_timed(TMD_OPP_COLD, time);
			(void)inc_timed(TMD_OPP_POIS, time);
			break;
		}

		case CHEM_DETECT_ENCHANTMENT:
		{
			(void)detect_objects_magic();
			break;
		}
		
		case CHEM_ACID_COAT_AMMO:
		{
            spellswitch = 17;
			return brand_ammo();
            spellswitch = 0;
		}
		
		case CHEM_BURST_OF_SPEED:
		{
			if (!p_ptr->timed[TMD_FAST])
			{
				(void)set_timed(TMD_FAST, 4 + randint(5) + (plev/8));
			}
			else
			{
				(void)inc_timed(TMD_FAST, 1 + randint(3));
			}
			break;
		}

		case CHEM_TELEPORT_SELF:
		{
			teleport_player(plev * 5);
			break;
		}
		
		case CHEM_SLOW_MONSTERS:
		{
			(void)slow_monsters();
			break;
		}

		case CHEM_MASS_SLEEP:
		{
			(void)sleep_monsters();
			break;
		}

		case CHEM_HASTE_SELF:
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

		case CHEM_TELEPORT_LEVEL:
		{
			(void)teleport_player_level();
			break;
		}

		case CHEM_RUNE_OF_PROTECTION:
		{
			(void)warding_glyph();
			break;
		}

		case CHEM_LIGHTNING_BOLT:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_beam(GF_ELEC, dir,
			          damroll(3+((plev-5)/6), 6));
			break;
		} // L20 5-30, L30 7-42, L40 8-48, L50 10-60

		case CHEM_RAIN_OF_ARROWS: /* get no earlier than L24 */
		{
            /* Make sure player is wielding a bow */
            if ((p_ptr->ammo_tval != TV_ARROW))
            {
               msg_print("You must be wielding a bow to fire a rain of arrows.");
               return (FALSE);
            }
			if (!get_aim_dir(&dir)) return (FALSE);
			/* fire two swarms to make damage more random */
			int arrowdmg = damroll((plev/12), (4 + randint(plev/6)));
			if (arrowdmg > 70) arrowdmg = 70;
			int manya = randint(3);
			if (plev < 32) manya = randint(4);
			if (plev < 25) manya = randint(5);
			fire_swarm(manya + 2, GF_ARROW, dir, arrowdmg, 1);
			arrowdmg = damroll((plev/12), (4 + randint(plev/5)));
			if (arrowdmg > 75) arrowdmg = 75;
			fire_swarm(1 + plev / 16, GF_ARROW, dir, arrowdmg + 1, 1);
            /* paranoia backfire to remind archer to use range weapon */
            if ((randint(100) < 20) && (!p_ptr->resist_fear) && (!p_ptr->timed[TMD_CHARM]))
            {
               inc_timed(TMD_AFRAID, rand_int(7) + 7);
               if (arrowdmg > 25) msg_print("You're a hotshot so you remind yourself not to melee.");
               else msg_print("You suddenly feel especially nervous about melee combat.");
            }
			break;
		}   /* 2-16 at L24, 2-18 at L30, 3-30 at L40, 4-48 at L50 (per shot) */
            /* 2-16 at L24, 2-20 at L30, 3-36 at L40, 4-56 at L50 (per shot) */
            /* min 4 arrows, max 9 arrows */

		case CHEM_CHAOS_ARROW:
		{
            /* Make sure player is wielding a bow */
            if ((p_ptr->ammo_tval != TV_ARROW))
            {
               msg_print("You must be wielding a bow to fire a chaos arrow.");
               return (FALSE);
            }
			if (!get_aim_dir(&dir)) return (FALSE);
			int arrowdmg = damroll(plev/4, 7) + 1;
			fire_bolt(GF_CHAOS, dir, arrowdmg);
            /* paranoia backfire to remind archer to use range weapon */
            if ((randint(100) < 20) && (!p_ptr->resist_fear) && (!p_ptr->timed[TMD_CHARM]))
            {
               inc_timed(TMD_AFRAID, rand_int(7) + 7);
               if (arrowdmg > 30) msg_print("You're a hotshot so you remind yourself not to melee.");
               else msg_print("You suddenly feel especially nervous about melee combat.");
            }
			break;
		}   /* 6d7 at L24, 8d7 at L32, 10d7 at L40, 12d7 at L50 */

		case CHEM_ROCKET_BLAST:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			int manya = 40;
			if (plev > 39) manya = 50;
			if (plev > 49) manya = 60;
			fire_ball(GF_SHARD, dir, manya + randint(plev/5) + damroll((plev/2), 2), 2);
			if (randint(100) < 34) fire_ball(GF_KILL_WALL, dir, damroll((plev/2), 2), 2);
			break;
            /* 50-60 at L20, 55-70 at L30, 70-90 at L40, 85-110 at L50 */
		}

		case CHEM_ENCHANT_TO_HIT:
		{
            spellswitch = 18; /* enchant only bow or arrows */
			return enchant_spell(rand_int(3) + plev / 20, 0, 0);
            spellswitch = 0;
		}

		case CHEM_ENCHANT_TO_DAM:
		{
            spellswitch = 18; /* enchant only bow or arrows */
			return enchant_spell(0, rand_int(3) + plev / 20, 0);
            spellswitch = 0;
		}
		
		case CHEM_POISON_BRAND_AMMO:
		{
            spellswitch = 19;
			return brand_ammo();
            spellswitch = 0;
		}
		
		case CHEM_ELEMENTAL_BRAND_AMMO:
		{
			return brand_ammo();
		}

		case CHEM_CONFUSE_MONSTER:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			(void)confuse_monster(dir, 23);
			break;
		}

		case CHEM_SLEEP_MONSTER:
		{
            if (!get_aim_dir(&dir)) return (FALSE);
			(void)sleep_monster(dir);
			break;
		}

		case CHEM_WONDER:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			(void)spell_wonder(dir);
			break;
		}
		
		case CHEM_ELEMENT_BOLT:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
            die = randint(100);
            if (die < 15) fire_bolt_or_beam(plev / 2, GF_ELEC, dir, 
                                            damroll(3+((plev-5)/6), 6));
            else if (die < 30) fire_bolt_or_beam(plev / 3, GF_COLD, dir,
                                                 damroll(5+((plev-5)/4), 7));
            else if (die < 45) fire_bolt_or_beam(plev / 3, GF_FIRE, dir,
                                                 damroll(6+((plev-5)/4), 7));
            else if (die < 60) fire_bolt_or_beam(plev / 3, GF_ACID, dir, 
                                                 damroll(8+((plev-5)/4), 7));
            else if (die < 70) fire_bolt_or_beam(plev - 1, GF_ELEC, dir, 
                                                 damroll(4+((plev-5)/6), 6));
            else if (die < 80) fire_bolt_or_beam(plev / 2, GF_COLD, dir,
                                                 damroll(5+((plev-5)/4), 8));
            else if (die < 90) fire_bolt_or_beam(plev / 2, GF_FIRE, dir,
                                                 damroll(8+((plev-5)/4), 8));
            else fire_bolt_or_beam(plev / 2, GF_ACID, dir, 
                                   damroll(6+((plev-5)/4), 8));
			break;
		}

		case CHEM_POLYMORPH_OTHER:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			(void)poly_monster(dir);
			break;
		}

		case CHEM_SLOW_MONSTER:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			(void)slow_monster(dir);
			break;
		}
		
		case CHEM_ELEMENT_BALL:
		{
            die = randint(100);
			if (!get_aim_dir(&dir)) return (FALSE);
			if (die < 50) fire_ball(GF_COLD, dir, 35 + (plev), 2);
			else fire_ball(GF_FIRE, dir, 45 + (plev), 2);
			break;
		}

		case CHEM_ACID_BALL:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_ball(GF_ACID, dir, 55 + (plev), 2);
			break;
		}

		case CHEM_ICE_STORM:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_ball(GF_ICE, dir, 50 + (plev * 2), 3);
			break;
		} // 104 at L27, 120 at L35, 150

		case CHEM_STONE_TO_MUD:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			(void)wall_to_mud(dir);
			break;
		}

		case CHEM_RECHARGING1:
		{
			return recharge(5 + randint(plev));
		}

		case CHEM_RECHARGING2:
		{
			return recharge(5 + randint(plev/2));
		}

		case CHEM_CLOUDKILL1:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_ball(GF_POIS, dir, 40 + (plev / 2), 3);
			break;
		}

		case CHEM_CLOUDKILL2:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_ball(GF_POIS, dir, 40 + (plev / 2), 3);
			break;
		}
		
		case CHEM_OBJECT_TO_GOLD1:
		{
			break;
		}

		case CHEM_EXPLOSION:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_ball(GF_SHARD, dir, 20 + (plev * 2), 2);
			if (randint(100) < 10) fire_ball(GF_KILL_WALL, dir, damroll((plev/3), 2), 2);
			break;
           /* 60 at L20, 80 at L30, 100 at L40, 120 at L50 */
		}

		case CHEM_RESTORATION:
		{
			(void)do_res_stat(A_STR);
			(void)do_res_stat(A_CON);
			(void)do_res_stat(A_INT);
			(void)do_res_stat(A_WIS);
			(void)do_res_stat(A_DEX);
			(void)do_res_stat(A_CHR);
			break;
		}
		
		case CHEM_HIT_N_RUN:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_bolt(GF_MISSILE, dir, 1 + damroll(2, (plev/3) + 1));
			teleport_player(10);
			break;
		}

		case CHEM_BEDLAM1:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_ball(GF_OLD_CONF, dir, plev, 4);
			break;
		}

		case CHEM_BEDLAM2:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_ball(GF_OLD_CONF, dir, plev, 4);
			break;
		}

		case CHEM_ENCHANT_ARMOR: /* enchant armor */
		{
			return enchant_spell(0, 0, rand_int(3) + plev / 20);
		}

		case CHEM_GLYPH_OF_WARDING:
		{
			(void)warding_glyph();
            /* paranoia backfire to remind archer to use range weapon */
            if ((randint(100) < 10) && (!p_ptr->resist_fear) && (!p_ptr->timed[TMD_CHARM]))
            {
               inc_timed(TMD_AFRAID, rand_int(7) + 7);
               msg_print("You suddenly feel especially nervous about melee combat.");
            }
			break;
		}
		
		case CHEM_POISON_BRAND_WEAPON1:
        {
            spellswitch = 20; /* poison brand weapon */
			brand_weapon();
            spellswitch = 0;
			break;
        }
		
		case CHEM_SLIP_INTO_SHADOWS:
		{
	             u32b t1, t2, t3, t4;
	             u32b f1 = 0L, f2 = 0L, f3 = 0L, f4 = 0L;
	             object_type *o_ptr;
	             int k;

	         /* Check for aggravation */
	         for (k = INVEN_WIELD; k < INVEN_TOTAL; k++)
	         {
                 o_ptr = &inventory[k];
                 
		         /* Skip non-objects */
		         if (!o_ptr->k_idx) continue;

		         /* Extract the flags */
		         object_flags(o_ptr, &t1, &t2, &t3, &t4);

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
		
		case CHEM_POISON_BOLT:
        {
			if (!get_aim_dir(&dir)) return (FALSE);
            fire_bolt_or_beam(plev / 4, GF_POIS, dir, 
                              damroll(7+(plev/5), 7));
			break;
        } /* 11d7 at L20, 13d7 at L30, 15d7 at L40, 17d7 at L50 */
          /* bigger than frost bolt weaker than fire bolt */
          
		case CHEM_TELEKINESIS:
		{
            /* spellswitch 24 allows distance pickup */ 
            spellswitch = 24; 
            if (!get_aim_dir(&dir)) return (FALSE);
			do_telekinesis();
			if (spellswitch == 24)
			{
               /* chance of waking up monsters in path */
               if (randint(100) > plev + 29 - badluck + goodluck) fire_beam(GF_THROW, dir, 0);
            }
            spellswitch = 0;
			break;
		}
        
        case CHEM_RESTORE_DEX1:
		{
        	(void)do_res_stat(A_DEX);
			break;
        }
        
        case CHEM_RESTORE_DEX2:
		{
        	(void)do_res_stat(A_DEX);
			break;
        }
		
		case CHEM_POISON_BRAND_WEAPON2:
        {
            spellswitch = 20; /* poison brand weapon */
			brand_weapon();
            spellswitch = 0;
			break;
        }
        
        case CHEM_ELIXIR_OF_LIFE: /* change plev to luck when you put luck in */
        {
            die = randint(99) + (randint(plev/5));
            if (die < 8)
            {
               msg_print("You make a terrible mistake with the ingrediants..");
               take_hit(damroll(6, 6), "a fatal failed attempt to make the Elixir of Life.");
			   (void)do_dec_stat(A_STR);
			   (void)do_dec_stat(A_CON);
               (void)set_food(PY_FOOD_WEAK + 10);
			   inc_timed(TMD_POISONED, randint(30) + 30);
			   inc_timed(TMD_CUT, randint(30) + 30);
			   inc_timed(TMD_STUN, randint(5) + 5);
            }
            else if (die < 19)
            {
               msg_print("You make a major mistake with the ingrediants..");
			   inc_timed(TMD_POISONED, randint(15) + 10);
			   inc_timed(TMD_IMAGE, randint(24) + 2);
			   inc_timed(TMD_BLIND, randint(24) + 2);
			   inc_timed(TMD_AMNESIA, randint(24) + 2);
			   inc_timed(TMD_CONFUSED, randint(24) + 2);
            }
            else if (die < 31)
            {
               msg_print("You make a mistake with the ingrediants..");
			   inc_timed(TMD_POISONED, randint(10) + 10);
               inc_timed(TMD_CUT, randint(3) + 1);
               (void)set_timed(TMD_STUN, p_ptr->timed[TMD_STUN] / 2);
			   (void)do_res_stat(A_DEX);
			   (void)do_res_stat(A_CHR);
			   (void)clear_timed(TMD_BLIND);
            }
            else if (die < 65)
            {
               msg_print("You didn't get it right, but this is still really good stuff.");
               hp_player(die * (plev/14));
			   (void)do_res_stat(A_WIS);
			   (void)do_res_stat(A_INT);
			   (void)clear_timed(TMD_BLIND);
			   (void)clear_timed(TMD_STUN);
			   (void)clear_timed(TMD_CUT);
			   if (p_ptr->silver > PY_SILVER_HEALTHY) p_ptr->silver = p_ptr->silver - 1;
			   if (p_ptr->silver < PY_SILVER_HEALTHY) p_ptr->silver = PY_SILVER_HEALTHY;
               if (p_ptr->slime > PY_SLIME_HEALTHY) p_ptr->slime = p_ptr->slime - 2;
			   if (p_ptr->slime < PY_SLIME_HEALTHY) p_ptr->slime = PY_SLIME_HEALTHY;
               if (p_ptr->timed[TMD_POISONED]) clear_timed(TMD_POISONED);
               else inc_timed(TMD_POISONED, randint(9) + 2);
            }
            else if (die < 100)
            {
               msg_print("Almost the perfect Elixir, you feel the life flowing in..");
			   (void)hp_player(die * (plev/11));
			   (void)clear_timed(TMD_BLIND);
			   (void)clear_timed(TMD_AFRAID);
			   (void)clear_timed(TMD_POISONED);
			   (void)clear_timed(TMD_STUN);
			   (void)clear_timed(TMD_CUT);
			   (void)do_res_stat(A_STR);
			   (void)do_res_stat(A_CON);
			   if (randint(100) < 34) restore_level();
			   if (p_ptr->silver > PY_SILVER_HEALTHY) p_ptr->silver = p_ptr->silver - 5;
			   if (p_ptr->silver < PY_SILVER_HEALTHY) p_ptr->silver = PY_SILVER_HEALTHY;
               if (p_ptr->slime > PY_SLIME_HEALTHY) p_ptr->slime = p_ptr->slime - 6;
			   if (p_ptr->slime < PY_SLIME_HEALTHY) p_ptr->slime = PY_SLIME_HEALTHY;
            }
            else if (die < 106)
            {
               msg_print("Amazingly close to perfect, you feel the life flowing in..");
			   restore_level();
			   (void)hp_player(300);
			   (void)clear_timed(TMD_BLIND);
			   (void)clear_timed(TMD_AFRAID);
			   (void)clear_timed(TMD_POISONED);
			   (void)clear_timed(TMD_STUN);
			   (void)clear_timed(TMD_CUT);
			   (void)do_res_stat(A_STR);
			   (void)do_res_stat(A_CON);
			   (void)do_res_stat(A_WIS);
			   (void)do_res_stat(A_INT);
			   (void)do_res_stat(A_DEX);
			   (void)do_res_stat(A_CHR);
			   if (p_ptr->silver > PY_SILVER_HEALTHY) p_ptr->silver = p_ptr->silver - 20;
			   if (p_ptr->silver < PY_SILVER_HEALTHY) p_ptr->silver = PY_SILVER_HEALTHY;
               if (p_ptr->slime > PY_SLIME_HEALTHY) p_ptr->slime = p_ptr->slime - 22;
			   if (p_ptr->slime < PY_SLIME_HEALTHY) p_ptr->slime = PY_SLIME_HEALTHY;
            }
            else             
            {
               msg_print("Wow, you've made a perfect mix of the Elixir of Life!");
               msg_print("You feel like you'll live forever.");
			   restore_level();
			   gain_exp(100);
			   (void)clear_timed(TMD_BLIND);
			   (void)clear_timed(TMD_AFRAID);
			   (void)clear_timed(TMD_POISONED);
			   (void)clear_timed(TMD_STUN);
			   (void)clear_timed(TMD_CUT);
			   int time = randint(50) + 50;
			   (void)inc_timed(TMD_OPP_ACID, time);
			   (void)inc_timed(TMD_OPP_ELEC, time);
			   (void)inc_timed(TMD_OPP_FIRE, time);
			   (void)inc_timed(TMD_OPP_COLD, time);
			   (void)inc_timed(TMD_OPP_POIS, time);
			   (void)do_res_stat(A_STR);
			   (void)do_res_stat(A_CON);
			   (void)do_res_stat(A_WIS);
			   (void)do_res_stat(A_INT);
			   (void)do_res_stat(A_DEX);
			   (void)do_res_stat(A_CHR);
			   if (p_ptr->silver > PY_SILVER_HEALTHY) p_ptr->silver = PY_SILVER_HEALTHY;
			   if (p_ptr->slime > PY_SLIME_HEALTHY) p_ptr->slime = PY_SLIME_HEALTHY;
			   (void)do_inc_stat(A_CON);
			   /* Recalculate max. hitpoints */
			   update_stuff();
			   (void)hp_player(5000);
            }
			break;
        }

		case CHEM_SHIELD:
		{
			(void)inc_timed(TMD_SHIELD, randint(20) + 30);
			break;
		}

		case CHEM_METEOR_SWARM:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_swarm(2 + plev / 20, GF_METEOR, dir, 30 + plev / 2, 1);
			break;
		}

    }

	/* Success */
	return (TRUE);
}

static bool cast_dark_spell(int spell)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int dir;

	int plev = p_ptr->lev;

	/* Hack -- chance of "beam" instead of "bolt" */
	int beam = beam_chance();

    /* spellcasting bonus */	
	if (p_ptr->timed[TMD_BRAIL])
    {
       plev += 10;
       beam += 5;
    }

	/* Spells. */
	switch (spell)
	{
		case DARK_DETECT_EVIL:
		{
			(void)detect_monsters_evil();
			break;
		}
		
        case DARK_SHADOW_STING:
        {
			if (!get_aim_dir(&dir)) return (FALSE);
            int dmod = 8;
            if (randint(30 + goodluck + badweap*2 - goodweap*3) > 25) dmod = 9;
            if (randint(plev + goodluck + badweap*2 - goodweap*3) > 46) dmod = 10;
            if ((badluck > 10) && (badweap < 1)) dmod = 8;
			if (plev < 4) fire_bolt(GF_NETHER, dir, damroll(3, (plev * 3)/2 ));
			else if (plev < 7) fire_bolt(GF_NETHER, dir, damroll(3, plev));
			else if (plev < 11) fire_bolt(GF_NETHER, dir, damroll(3, 6));
			else if (plev < 16) fire_bolt(GF_NETHER, dir, damroll(3, 7));
			else if (plev < 21) fire_bolt(GF_NETHER, dir, damroll(3, 8));
			else fire_bolt(GF_NETHER, dir, damroll((plev/7), dmod));
            if (randint(100) + badluck > 90 + goodluck)
            {
                (void)inc_timed(TMD_WITCH, 10 + randint(50-plev));
            }
			break;
        }

		
		case DARK_INFRAVISION:
		{
		    (void)inc_timed(TMD_SINFRA, randint(plev) + 10 + (plev * 3));
			break;
		}

		case DARK_CALL_DARK:
		{
            if (plev < 11) (void)inc_timed(TMD_SINFRA, 3 + randint(plev+2));
		    else (void)inc_timed(TMD_SINFRA, 3 + randint(plev-3));
            spellswitch = 21; /* uses GF_DARK instead of DARK_WEAK */
            int ifrad = (plev / 3) - 1;
            if (plev > 15) ifrad = 3 + randint(plev / 10);
            if (ifrad < 1) ifrad = 1;
            if (randint(100) < 11 + badweap*2 + goodluck/2 - ifrad) ifrad += 1;
			(void)unlite_area(damroll(2, (plev / 2)) + randint(ifrad*2) + 1, ifrad);
            spellswitch = 0;
			break;
		}

		case DARK_SCARE_MONSTER:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			(void)fear_monster(dir, plev);
			break;
		}

		case DARK_NOXIOUS_FUMES:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			if (plev > 7 + (randint(18))) spellswitch = 22; /* adds chance of sleep to GF_POIS */
			fire_ball(GF_POIS, dir, 10 + (plev / 2) + randint(plev / 4), 2);
			spellswitch = 0;
			break;
		}

		case DARK_SLEEP_MONSTER:
		{
            if (!get_aim_dir(&dir)) return (FALSE);
			(void)sleep_monster(dir);
			break;
		}

		case DARK_CURE_POISON:
		{
			(void)clear_timed(TMD_POISONED);
			break;
		}
		
		case DARK_WITHER_FOE:
        {
			if (!get_aim_dir(&dir)) return (FALSE);
			int witherf = plev / 7;
			if ((witherf < 1) && (plev > 11)) witherf = 2;
			if (witherf < 1) witherf = 1;
			(void)confuse_monster(dir, plev + 10);
			(void)slow_monster(dir);
			fire_bolt(GF_MANA, dir, damroll(witherf, 8));
			break;
        }

		case DARK_SEE_INVISIBLE:
		{
			(void)inc_timed(TMD_SINVIS, randint(plev) + 24);
			break;
		}

		case DARK_FIND_TRAPS:
		{
			(void)detect_traps();
			break;
		}

		case DARK_DETECT_DOORS_STAIRS:
		{
			(void)detect_doors();
			(void)detect_stairs();
			break;
		}
		
		case DARK_DETECT_LIVING:
		{
			(void)detect_monsters_life();
			break;
		}

		case DARK_SHADOW_SHIFT:
		{
			teleport_player(10);
			int die = randint(100);
			if (plev < 20) die += (20-plev);
			if (die < 75)
            {
               if (plev > 4) take_hit(randint(6), "shifting through the nether realm");
               else take_hit(randint(3), "shifting through the nether realm");
            }
			break;
		}

        case DARK_DRAIN_CHARGES:
        {
			int drained = 0;
	        int item;
            cptr q, s;
	        object_type *o_ptr;

            /* Restrict choices to staves or wands */
            item_tester_hook = item_tester_hook_charged;

            /* Get an item */
            q = "Drain which wand or staff? ";
            s = "You have no wand or staff to use.";
            if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

	        /* Get the object */
	        if (item >= 0)
	        {
		        o_ptr = &inventory[item];
	        }
	        else
	        {
	        	o_ptr = &o_list[0 - item];
	        }

			/* Drain charged wands/staves */
			if ((o_ptr->tval == TV_STAFF) ||
			    (o_ptr->tval == TV_WAND))
			{
				/* Charged? */
				if (o_ptr->pval)
				{
					drained = o_ptr->pval;
   						/* Uncharge */
					o_ptr->pval = 0;
				}
			}
			if (plev < 30) (void)hp_player((drained) * (plev/3));
			else if (plev < 40) (void)hp_player((drained+1) * (8 + randint(2)));
            else (void)hp_player((drained+1) * (plev/4));
            if (randint(100) + badluck > 80 + goodluck)
            {
                (void)inc_timed(TMD_WITCH, 10 + randint(plev));
            }
			break;
		}
		
		case DARK_STINK: /* similar but slightly more powerful than luck spell */
        {
            int die = randint(99 - badluck + goodluck) + randint(plev/4);
   			if (die > 10)
            {
               if (!get_aim_dir(&dir)) return (FALSE);
            }
            if (die < 11) /* poison self with cloud centered at self */
            {
               msg_print("You smell something foul.");
			   if (!(p_ptr->resist_pois || p_ptr->timed[TMD_OPP_POIS]))
			   {
                  int dam;
                  if (die < 2) die = 2;
			      dam = (109 - (die * 10)) / 3;
			      fire_ball(GF_POIS, 0, dam, 3);
			      if (p_ptr->weakresist_pois) dam = (dam / 2) - 1;
			      take_hit(dam, "a toxic smell");
			      (void)inc_timed(TMD_POISONED, rand_int(dam/2) + 10);
			      if (p_ptr->weakresist_pois) (void)dec_timed(TMD_POISONED, 2 + randint(9));
			   }
			   else fire_ball(GF_POIS, 0, 2 + randint(31), 3);
            }
            else if (die < 50) fire_ball(GF_POIS, dir, 10 + (plev / 2) + randint(goodluck), 2);
            else if (die < 60)
            {
               fire_bolt_or_beam(beam-10, GF_POIS, dir,
                                 damroll(5 + (plev / (1 + randint(2))), 3));
               fire_ball(GF_POIS, dir, 2 + randint(31), 3);
            }
			else if (die < 80) fire_ball(GF_POIS, dir, 15 + (plev), plev / 12);
			else if (die < 101) fire_ball(GF_POIS, dir, 40 + (plev / 2), 3);
			else /* (die > 100) */ fire_ball(GF_POIS, dir, 40 + (plev), 3);
			break;
        }

		case DARK_TURN_UNDEAD:
		{
			(void)turn_undead();
			break;
		}

		case DARK_DEATH_BOLT:
        {
			if (!get_aim_dir(&dir)) return (FALSE);
			spellswitch = 23; /* makes GF_OLD_DRAIN affect demons also */
			if (plev < 13) drain_life(dir, damroll(3 + ((plev) / 5), 6));
			else drain_life(dir, (plev*8)/5 + randint(plev));
			spellswitch = 0;
            if (randint(100) + badluck > 80 + goodluck)
            {
                (void)inc_timed(TMD_WITCH, 11 + randint(plev));
            }
			break;
        }  /* stronger than acid bolt, weaker than fire ball center */

		case DARK_DISPEL_UNDEAD1:
		{
			(void)dispel_undead(randint(plev * 3));
			break;
		}
		
        case DARK_CAUSE_LIGHT_WOUNDS:
        {
			if (!get_aim_dir(&dir)) return (FALSE);
			spellswitch = 23; /* makes GF_OLD_DRAIN affect demons also */
			int dclw = plev / 9;
			if (dclw < 1) dclw = 1;
			int sdmg = damroll(dclw+1, 4 + randint(dclw+1));
			if ((sdmg < 6) && (randint(100) < 40)) sdmg += randint(sdmg);
			if ((sdmg > 40) && (randint(100) < 67)) sdmg -= 5 + randint(sdmg/4);
            fire_ball(GF_OLD_DRAIN, dir, sdmg + 3, 0);
			spellswitch = 0;
			break;
        } /* 2d(5-6) at L6, 3d(5-7) at L18, 4d(5-8) at L27, */
          /* 5d(5-9) at L36, 6d(5-10) at L45 */
          /* av: ~9 at L6, ~13 at L18, ~19 at L27, ~23 at L36, ~29 at L45 */

        case DARK_WITCH_DISINFECTANT:
		{
            spellswitch = 16; /* dispel slime in bug_spray function */
			(void)inc_timed(TMD_CUT, randint(11) + 5);
			(void)dec_timed(TMD_POISONED, 4 + randint(plev/2));
			(void)dec_timed(TMD_STUN, 4 + randint(plev/2));
			(void)dispel_bug(1 + randint(plev / 2));
			(void)inc_timed(TMD_WOPP_POIS, randint(13) + 5);
            if ((randint(100) < 34) && (p_ptr->slime > PY_SLIME_HEALTHY)) p_ptr->slime -= 1;
            if ((randint(100) < 34) && (p_ptr->slime > PY_SLIME_HEALTHY)) p_ptr->slime -= 1;
            if ((randint(100) < 16) && (plev > 9))
            {
               spellswitch = 16; /* dispel slime in bug_spray function */
			   (void)dec_timed(TMD_POISONED, randint(6));
			   (void)dec_timed(TMD_STUN, randint(3));
               if ((randint(100) < (plev * 3)) && (p_ptr->slime > PY_SLIME_HEALTHY)) p_ptr->slime -= 1;
               if ((randint(100) < (plev * 3)) && (p_ptr->silver > PY_SILVER_HEALTHY)) p_ptr->silver -= 1;
			   (void)dispel_bug(1 + randint(plev / 3));
            }
			(void)dec_timed(TMD_CONFUSED, 5 + randint(plev/3));
			(void)dec_timed(TMD_BLIND, 5 + randint(plev/3));
			(void)dec_timed(TMD_FRENZY, 3 + randint(plev/4));
			(void)dec_timed(TMD_AMNESIA, 3 + randint(plev/4));
			(void)dec_timed(TMD_IMAGE, 1 + randint(plev/4));
            spellswitch = 0;
			break;
		}

        case DARK_CAUSE_SERIOUS_WOUNDS:
        {
			if (!get_aim_dir(&dir)) return (FALSE);
			spellswitch = 23; /* makes GF_OLD_DRAIN affect demons also */
			int dcsw = plev / 6;
			if (dcsw < 2) dcsw = 2;
			int sdmg = damroll(dcsw+2, 5 + randint(dcsw+1)) + randint(plev/2);
			if (sdmg < 6) sdmg *= 1 + randint(3);
			if ((sdmg < 12) && (randint(100) < 40)) sdmg += 1 + randint(sdmg);
			if ((sdmg > 99) && (randint(100) < 90)) sdmg -= 10 + randint(sdmg/4);
            fire_ball(GF_OLD_DRAIN, dir, sdmg + 10, 0);
			spellswitch = 0;
            if (randint(100) + badluck > 80 + goodluck)
            {
                (void)inc_timed(TMD_WITCH, 50 + randint(100-plev));
            }
			break;
        } /* 4d(6-8) at L6, 5d(6-9) at L18, 6d(6-10) at L24, */
          /* 8d(6-12) at L36, 10d(6-14) at L48 (before capping) */
          /* av: ~23.5 at L6, ~31 at L18, ~39 at L24, ~47 at L36, ~75 at L48 */
          /* on average slightly weaker than frost bolt */

        case DARK_TRAP_DOOR_DESTRUCTION:
   		{
			(void)destroy_doors_touch();
			break;
		}

        case DARK_RESIST_POISON:
		{
			(void)inc_timed(TMD_OPP_POIS, randint(20) + 20);
			break;
		}

        case DARK_POISON_BOLT:
        {
			if (!get_aim_dir(&dir)) return (FALSE);
            fire_bolt_or_beam(beam, GF_POIS, dir, 
                              damroll(7+(plev/5), 7));
			break;
        } /* 11d7 at L20, 13d7 at L30, 15d7 at L40, 17d7 at L50 */
             
        case DARK_SLIP_INTO_SHADOWS:
		{
	             u32b t1, t2, t3, t4;
	             u32b f1 = 0L, f2 = 0L, f3 = 0L, f4 = 0L;
	             object_type *o_ptr;
	             int k;

	         /* Check for aggravation */
	         for (k = INVEN_WIELD; k < INVEN_TOTAL; k++)
	         {
                 o_ptr = &inventory[k];
                 
		         /* Skip non-objects */
		         if (!o_ptr->k_idx) continue;

		         /* Extract the flags */
		         object_flags(o_ptr, &t1, &t2, &t3, &t4);

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
        
        case DARK_CAUSE_MORTAL_WOUNDS:
        {
			if (!get_aim_dir(&dir)) return (FALSE);
			spellswitch = 23; /* makes GF_OLD_DRAIN affect demons also */
			int dcmw = plev / 4;
			int sdmg = damroll(10 + randint(dcmw), dcmw+2);
			if ((sdmg < 80) && (plev > 39)) sdmg += 20 + randint(sdmg/3);
			if ((sdmg < 56) && (plev > 29)) sdmg += 10 + randint(sdmg/3);
			if (sdmg < 30) sdmg += 15 + randint(sdmg/2);
			if ((sdmg < 50) && (randint(100) < 67)) sdmg += 5 + randint(sdmg/3);
			if ((sdmg > 260) && (randint(100) < 50)) sdmg -= 5 + randint(sdmg/5);
            fire_ball(GF_OLD_DRAIN, dir, sdmg, 0);
			if (sdmg > 160) sdmg = 165;
			if (sdmg < 80) sdmg = sdmg + 10;
            fire_ball(GF_SHARD, dir, randint(sdmg/5), 2);
			spellswitch = 0;
            if (randint(100) + badluck > 66 + goodluck)
            {
                (void)inc_timed(TMD_WITCH, 100 + randint(200-plev));
            }
			break;
        } /* (11-15)d7 at L20, (11-16)d8 at L24, (11-19)d11 at L36, (11-22)d14 at L48 */
          /* av: ~57 at L20, ~66 at L24, ~98 at L36, ~135 at L48 (before capping)*/
          /* absolute minimum after capping and shard ball is 51 */

        case DARK_BREATHE_WITHERING:
        {
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_ball(GF_CONFUSION, dir, damroll(2, plev/5), plev/13);
			fire_ball(GF_OLD_SLOW, dir, damroll(2, plev/5), plev/13);
			fire_ball(GF_MANA, dir, damroll(plev/6, 3), plev/9);
			fire_ball(GF_MANA, dir, damroll(plev/5, 8), plev/13);
            if (randint(100) + badluck > 66 + goodluck)
            {
                (void)inc_timed(TMD_WITCH, 100 + randint(200-plev));
            }
			break;
        }
        
        case DARK_SHADOW_WARP:
        {
			int die = randint(100 + plev/11);
			teleport_player(plev * 5);
			int shifthit = 3 + randint(plev - 20);
			if (plev < 26) shifthit = 2 + randint(6);
			if (die < 22) (void)inc_timed(TMD_STUN, randint(4) + 2);
			else if (die < 44) (void)inc_timed(TMD_BLIND, randint(4) + 2);
			else if (die < 66) (void)inc_timed(TMD_CUT, randint(4) + 2);
			else if (die < 75) (void)inc_timed(TMD_BLIND, randint(2));
			else shifthit -= 1;
			if (randint(100) < 90) take_hit(shifthit, "shifting through the nether realm");
			break;
		}
		
        case DARK_WIZARD_LOCK:
		{
			if (!get_aim_dir(&dir)) return;

			/* Spell will jam a door or create a jammed door
            /* in any one empty square. */
			fire_ball(GF_WIZLOCK, dir, 0, 0);
			break;
		}

		case DARK_SHADOW_MAPPING:
		{
			map_area();
			break;
		}

        case DARK_BREAK_CURSE:
		{
			remove_curse();
			break;
		}

        case DARK_PROBING:
		{
			(void)probing();
			break;
		}

        case DARK_IDENTIFY:
		{
			return ident_spell();
		}
		
		case DARK_PROTECT_FROM_LIFELESS:
		{
			(void)inc_timed(TMD_PROTDEAD, randint(25) + ((1 + randint(2)) * p_ptr->lev));
			break;
		}

        case DARK_EXORCISE_DEMONS:
        {
			if (randint(100 + (badluck*2)) < (plev*3)/2) (void)dispel_demon(1 + randint((plev * 7)/2));
			else (void)dispel_demon(randint(plev * 3));
            if (randint(100) + badluck > 80 + goodluck)
            {
                (void)inc_timed(TMD_WITCH, 100 + randint(200-plev));
            }
			break; /* (plev * 3.5) */
		}
             
        case DARK_DETECT_ENCHANTMENT:
		{
			(void)detect_objects_magic();
			break;
		}

		case DARK_SCARE_MONSTERS:
        {
		    scare_monsters();
			break;
        }

		case DARK_SPEAR_OF_DARK:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			msg_print("A beam of darkness appears.");
			fire_beam(GF_DARK, dir, damroll(3 + (plev/10), 8));
			break;
		}

		case DARK_ICE_SPEAR:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_bolt_or_beam(beam + 5, GF_ICE, dir, damroll(5 + (plev/5), 8));
            if (randint(100) + badluck > 66 + goodluck)
            {
                (void)inc_timed(TMD_WITCH, 100 + randint(200-plev));
            }
			break;
		}

		case DARK_STENCH_OF_DEATH:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_ball(GF_POIS, dir, 13 + (plev / 2), 3);
			fire_ball(GF_NETHER, dir, 13 + (plev / 8), 4);
			dispel_life(randint((plev*3)/2));
			fire_ball(GF_OLD_CONF, dir, plev - 2, 4);
			spellswitch = 28; /* target self */
			fire_ball(GF_POIS, 0, 13, 3);
			if (p_ptr->resist_nethr) take_hit(randint(3), "the stench of death");
			else take_hit(randint(13), "the stench of death");
			if (p_ptr->resist_pois) (void)inc_timed(TMD_POISONED, randint(8) + 5);
			else (void)inc_timed(TMD_POISONED, randint(9) + 12);
            if (randint(100) + badluck > 66 + goodluck)
            {
                (void)inc_timed(TMD_WITCH, 200 + randint(200-plev));
            }
			break;
        }
 
        case DARK_BEAM_OF_DESTRUCTION:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			if (randint(95) < (plev - 15))
            {
               if (plev > 40) fire_beam(GF_ACID, dir, 1 + damroll((plev-39)/2, 7));
               else if (plev > 35) fire_beam(GF_ACID, dir, damroll((plev-34)/2, 6));
               else fire_beam(GF_ACID, dir, damroll(randint(3), 4));
            }
            spellswitch = 26; /* activate destruction */
			fire_beam(GF_SHARD, dir, 15 + plev + damroll(plev/10, 9));
            if (randint(100) + badluck > 80 + goodluck)
            {
                (void)inc_timed(TMD_WITCH, 100 + randint(200-plev));
            }
		}

 		case DARK_DESTRUCTION_BALL:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			if (randint(90) < (plev - 15))
            {
               if (plev > 40) fire_ball(GF_ACID, dir, 4 + damroll((plev-39)/2, 10), 2);
               else if (plev > 38) fire_ball(GF_ACID, dir, 1 + damroll((plev-37)/2, 9), 2);
               else fire_ball(GF_ACID, dir, damroll(randint(3), 7), 2);
            }
            spellswitch = 11;  /* activates earthquake */
            range = 21; /* small earthquake */
			fire_ball(GF_SHARD, dir, 24 + plev + damroll(plev/10, 11), 1 + plev/21);
            if (randint(100) + badluck > 80 + goodluck)
            {
                (void)inc_timed(TMD_WITCH, 100 + randint(200-plev));
            }
			break;
		}
             
		case DARK_VENOM_BOLT:
        {
			if (!get_aim_dir(&dir)) return (FALSE);
            fire_bolt_or_beam(beam + 5, GF_POIS, dir, 
                              damroll(7+((plev+1)/4), 7 + randint(plev/10)));
            if (randint(100) + badluck > 80 + goodluck)
            {
                (void)inc_timed(TMD_WITCH, 150 + randint(200-plev));
            }
			break;
        } /* 12d(8-9) at L20, 14d(8-10) at L30, 17d(8-11) at L40, 19d(8-12) at L50 */
             
		case DARK_CHAOS_STRIKE:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_bolt_or_beam(beam, GF_CHAOS, dir, damroll(13, plev));
            if (randint(100) + badluck > 66 + goodluck)
            {
                (void)inc_timed(TMD_WITCH, 200 + randint(200-plev));
            }
			break;
		}

		case DARK_BOLDNESS:
   		{
			(void)clear_timed(TMD_AFRAID);
			if (randint(47 + badluck) < (plev + 1 + goodluck/2)) (void)clear_timed(TMD_CHARM);
			if ((plev > 39) && (randint(goodluck) > 6)) (void)inc_timed(TMD_HERO, randint(plev/2) + 6);
			else if ((plev > 20) && (randint(goodluck) + randint(4) > 7)) (void)inc_timed(TMD_HERO, randint((plev-10)/3) + 2);
			break;
		}

        case DARK_RESIST_POISON2:
		{
			(void)inc_timed(TMD_OPP_POIS, randint(20) + 20);
			break;
		}
		
		case DARK_TOURNIQUET: /* the closest a necromancer gets to curing */
		{
			(void)clear_timed(TMD_STUN);
			(void)clear_timed(TMD_CUT);
			(void)set_timed(TMD_POISONED, (p_ptr->timed[TMD_POISONED] / 2) - 1);
			(void)inc_timed(TMD_WOPP_POIS, randint(5) + 3);
			(void)set_timed(TMD_SHERO, p_ptr->timed[TMD_SHERO] / 2);
			(void)dec_timed(TMD_IMAGE, 1 + randint(plev/10));
			(void)dec_timed(TMD_CONFUSED, 1 + randint(plev/10));
			break;
		}

		case DARK_UNDEAD_SKIN:
		{
			int time = randint(20) + 20;
			(void)inc_timed(TMD_OPP_COLD, time);
			(void)inc_timed(TMD_OPP_NETHR, time);
            if (randint(100) + badluck > 66 + goodluck)
            {
                (void)inc_timed(TMD_WITCH, 300 + randint(200-plev));
            }
			break;
		}
		
        case DARK_SUMMON_DEMONIC_AID:
        {
            int die;
            if (plev > 40) die = randint(100);
            else if (plev > 30) die = randint(95);
            else die = randint(90);
            if (die < 20) do_call_help(989, TRUE);
            else if (die < 40) do_call_help(990, TRUE);
            else if (die < 60) do_call_help(991, TRUE);
            else if (die < 80) do_call_help(992, TRUE);
            else do_call_help(993, TRUE);

            if (randint(100) + badluck > 67 + goodluck)
            {
                (void)inc_timed(TMD_WITCH, 100 + randint(200-plev));
            }
			break;
		}

		case DARK_UNHOLY_RESISTANCE:
		{
			int time = randint(20) + 20;
			(void)inc_timed(TMD_OPP_FIRE, time);
			(void)inc_timed(TMD_OPP_DARK, time);
			(void)inc_timed(TMD_OPP_COLD, time);
			(void)inc_timed(TMD_OPP_POIS, time);
			(void)inc_timed(TMD_OPP_NETHR, time);
            if (randint(100) + badluck > 66 + goodluck)
            {
                (void)inc_timed(TMD_WITCH, 200 + randint(200-plev));
            }
			break;
		}
		
		case DARK_SHADOW_BARRIER:
		{
			(void)inc_timed(TMD_SHIELD, randint(20) + 30);
            if (randint(100) + badluck > 80 + goodluck)
            {
                (void)inc_timed(TMD_WITCH, 100 + randint(200-plev));
            }
			break;
		}

		case DARK_DETECT_ALL_MONSTERS:
        {
			(void)detect_monsters_normal();
			(void)detect_monsters_invis();
			break;
        }

		case DARK_TELEPORT_OTHER:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			(void)teleport_monster(dir);
			break;
		}

        case DARK_ORB_OF_DEATH:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			spellswitch = 23; /* makes GF_OLD_DRAIN affect demons also */
			fire_ball(GF_OLD_DRAIN, dir,
			          (damroll(3, 6) + plev + (plev / 2)),
			          ((plev < 30) ? 2 : 3));
			spellswitch = 0;
            if (randint(100) + badluck > 66 + goodluck)
            {
                (void)inc_timed(TMD_WITCH, 200 + randint(200-plev));
            }
			break;
		}
             
        case DARK_DISPEL_LIFE: /* does not affect silver or demons */
		{
			if (randint(100 + (badluck*2)) < (plev*3)/2) (void)dispel_life(1 + randint((plev * 7)/2));
			else (void)dispel_life(randint(plev * 3));
			if ((!(p_ptr->timed[TMD_BECOME_LICH])) && (randint(100) > plev + 10 + (goodluck*2)))
            {
               take_hit(randint(plev + 5), "A spell of dispel living");
            }
            if (randint(100) + badluck > 66 + goodluck)
            {
                (void)inc_timed(TMD_WITCH, 300 + randint(200-plev));
            }
			break;
		}
		
		case DARK_VAMPIRIC_DRAIN:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			spellswitch = 27; /* does not affect silver monsters */
			int drainmuch = damroll(plev / 3, 11);
			fire_bolt(GF_OLD_DRAIN, dir, drainmuch);
			(void)hp_player(drainmuch + 10 + randint((drainmuch/2)-10));
			(void)set_food(p_ptr->food + 450 + randint(drainmuch * 5));
            if (randint(100) + badluck > 66 + goodluck)
            {
                (void)inc_timed(TMD_WITCH, 300 + randint(200-plev));
            }
			break;
		}
		
		case DARK_HOLD_LIFE:
		{
            (void)inc_timed(TMD_HOLDLIFE, randint(8 + plev/2) + 20);
			break;
		}

		case DARK_REND_SOUL:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_bolt_or_beam(beam / 4, GF_NETHER, dir, damroll(11, plev));
            if (randint(100) + badluck > 66 + goodluck)
            {
                (void)inc_timed(TMD_WITCH, 200 + randint(200-plev));
            }
			break;
		}
		
		case DARK_GENOCIDE:
		{
            if (randint(100) + badluck > 66 + goodluck)
            {
                (void)inc_timed(TMD_WITCH, 100 + randint(200-plev));
            }
			return banishment();
			break;
		}
		
		case DARK_RECHARGING:
		{
			return recharge(5 + plev);
		}

		case DARK_CURE_MORTAL_WOUNDS:
		{
			(void)hp_player(damroll(10, 5 + plev/10));
			(void)clear_timed(TMD_STUN);
			(void)clear_timed(TMD_CUT);
			break;
		}
        		
		case DARK_SPIRIT_OF_BALROG:
		{
			int time = randint(30) + 30;
			(void)inc_timed(TMD_OPP_FIRE, time);
			(void)inc_timed(TMD_OPP_DARK, time);
			(void)inc_timed(TMD_BALROG, time);
            if (randint(100) + badluck > 66 + goodluck)
            {
                (void)inc_timed(TMD_WITCH, 300 + randint(200-plev));
            }
			break;
		}

		case DARK_POISON_AMMO:
		{
            spellswitch = 19;
			return brand_ammo();
            spellswitch = 0;
		}

		case DARK_HASTE:
		{
			if (!p_ptr->timed[TMD_FAST])
			{
				(void)set_timed(TMD_FAST, randint(20) + plev);
			}
			else
			{
				(void)inc_timed(TMD_FAST, randint(4) + randint(plev/10));
			}
			break;
		}

		case DARK_BLOODWRATH:
        {
			(void)hp_player(5 + randint(10));
            (void)clear_timed(TMD_AFRAID);
            spadjust = 4 + randint(1 + (plev/10));
            int die = randint(75);
	        if ((p_ptr->timed[TMD_CHARM]) && (die > (plev + 25)))
	        {
                 msg_print("You're in too good a mood to go into a battle frenzy");
            }
            else 
            {
			     (void)clear_timed(TMD_CHARM);
			     if (plev > 20)
			     {
                    (void)set_timed(TMD_ADJUST, randint(25) + plev);
			        (void)inc_timed(TMD_SHERO, randint(25) + (plev + ((76 - die) / 3)));
                 }
                 else
                 {
			        int time = randint(25) + 25;
                    (void)set_timed(TMD_ADJUST, (time - 3) - randint(3));
			        (void)inc_timed(TMD_SHERO, time);
                 }
                 if (randint(100) + badluck > 70 + goodluck)
                 {
                    (void)inc_timed(TMD_WITCH, 150 + randint(200-plev));
                 }
            }
			break;
        }
        
        case DARK_SEE_ALL_FOES: /* full telepathy for necromancers */
        {
			(void)clear_timed(TMD_BLIND);
			(void)inc_timed(TMD_SINVIS, randint(35) + 35);
			if ((cp_ptr->flags & CF_BEAM) && (badluck < 3)) (void)inc_timed(TMD_ESP, randint(plev) + (plev/2));
			else if (plev + (goodluck/2) > 39) (void)inc_timed(TMD_ESP, randint(plev/2) + (plev/3));
			else if (randint(plev + (goodluck/2)) > 20) (void)inc_timed(TMD_ESP, randint(plev/2));
			else if (randint(plev + (goodluck/2)) > 5) (void)inc_timed(TMD_ESP, randint(plev/4));			
			break;
        }

        case DARK_DRAW_MANA:        
        {
            if (p_ptr->csp >= p_ptr->msp)
            {
                  msg_print("Your mana is already at maximum!");
                  if (randint(100) + badluck > 66 + goodluck)
                  {
                     (void)inc_timed(TMD_WITCH, 150 + randint(200-plev));
                  }
			      break;
            }
            /* less powerful version */
	        else if (!get_check("Draw a lot of mana? (more powerful but risky) "))
	        {
               int die = 2 + randint(plev/2);
               if (plev < 20) die = 2 + randint(10);
               if (p_ptr->chp < die + 1)
               {
                  msg_print("Not enough life to draw mana!");
                  if (randint(100) + badluck > 66 + goodluck)
                  {
                     (void)inc_timed(TMD_WITCH, 150 + randint(200-plev));
                  }
			      break;
               }
               /* this should never kill anyone */
               take_hit(die, "a bug in the game");
               p_ptr->csp += die + 1;
            }
            /* more powerful and risky version */
            else 
            {
               int die = (plev/2) + randint(plev * 2);
               take_hit((die/2) + rand_int(3), "Taking your life for power");
               p_ptr->csp += die + 1 + randint(4);
            }
            if (randint(100) + badluck > 66 + goodluck)
            {
                (void)inc_timed(TMD_WITCH, 100 + randint(200-plev));
            }
			break;
        }

		case DARK_WORD_OF_DESTRUCTION:
		{
			destroy_area(py, px, 15, TRUE);
			break;
		}

        case DARK_HOLD:
        {
			(void)hold_monsters();
            if (randint(100) + badluck > 80 + goodluck)
            {
                (void)inc_timed(TMD_WITCH, 100 + randint(200-plev));
            }
			break;
        }

		case DARK_DISPEL_UNDEAD2: /* less powerful for witches and assassins */
		{
			if (cp_ptr->flags & CF_BEAM)(void)dispel_undead(randint(plev * 4));
			else (void)dispel_undead(1 + randint((plev * 7)/2));
            if (randint(100) + badluck > 80 + goodluck)
            {
                (void)inc_timed(TMD_WITCH, 100 + randint(200-plev));
            }
			break;
		}
		
		case DARK_BR_FEAR_CONF:
        {
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_ball(GF_DARK, dir, damroll(plev/6, 3), plev/9);
			fire_ball(GF_MANA, dir, damroll(plev/5, 8), plev/12);
			fire_ball(GF_CONFUSION, dir, damroll(3, plev/5), plev/12);
			fire_ball(GF_BRFEAR, dir, damroll(3, plev/5), plev/12);
            if (randint(100) + badluck > 66 + goodluck)
            {
                (void)inc_timed(TMD_WITCH, 300 + randint(200-plev));
            }
			break;
        }
        
        /* chance of monsters warped through the nether realm (teleportation) */
        case DARK_NETHER_BALL:
        {
			if (!get_aim_dir(&dir)) return (FALSE);
			if ((randint(50) < plev) && (randint(100) < 80))
            {
               fire_ball(GF_NETHER, dir, (plev/5) + damroll(plev, 10), 3);
               spellswitch = 30; /* gravity breathers don't resist, undead do */
               fire_ball(GF_GRAVITY, dir, ((plev*5)/7) + randint(20), 3);
            }
			else if (randint(100) < 16)
            {
               fire_ball(GF_NETHER, dir, damroll(plev, 10), 3);
               spellswitch = 30; /* gravity breathers don't resist, undead do */
               fire_ball(GF_GRAVITY, dir, (plev/2) + randint(plev/3 + 3), 3);
            }
            else
            {
               fire_ball(GF_NETHER, dir, damroll(plev, 11), 3);
            }
            if (randint(100) + badluck > 66 + goodluck)
            {
                (void)inc_timed(TMD_WITCH, 400 + randint(200-plev));
            }
			break;
        }
        
        case DARK_DARKNESS_STORM: /* black equivelent of mana storm */
        {
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_ball(GF_DARK, dir, 240 + plev*2 + randint(plev+15), 3);
            if (randint(100) + badluck > 66 + goodluck)
            {
                (void)inc_timed(TMD_WITCH, 400 + randint(200-plev));
            }
			break;
        }
        
        case DARK_BECOME_LICH:
        {
            if (randint(100) + badluck > 66 + goodluck)
            {
                (void)inc_timed(TMD_WITCH, 450 + randint(200-plev));
            }
            if ((badluck > 9) && (randint(100) < 5))
            {
                take_hit(plev*10 + randint(plev*10), "taking your life to become an undead.");
            }
            if ((badluck > 5) && (randint(100) < 10))
            {
                take_hit(randint(plev), "taking your life to become an undead.");
            }
            (void)clear_timed(TMD_OPP_FIRE);
            (void)clear_timed(TMD_IMM_FIRE);
            if (randint(99 + goodluck) < 81) p_ptr->luck -= 1;
            if (randint(80) + badluck > plev)
            {
               int die = randint(100 + goodluck);
               if (die < 20)
               {
                  (void)do_dec_stat(A_WIS);
                  (void)do_dec_stat(A_CHR);
               }
               else if (die < 23) p_ptr->luck -= 1;
               else if (die < 53) (void)do_dec_stat(A_WIS);
               else if (die < 83) (void)do_dec_stat(A_CHR);
               else if (die > 100) (void)hp_player(plev);
               else if (die > 95) (void)hp_player(plev/3);
               else if (die > 90) (void)hp_player(plev/5);
            }
            (void)hp_player(plev+10);
			(void)clear_timed(TMD_AFRAID);
			(void)clear_timed(TMD_CHARM);
			(void)clear_timed(TMD_FRENZY);
   			(void)clear_timed(TMD_CONFUSED);
			(void)clear_timed(TMD_POISONED);
			(void)clear_timed(TMD_CUT);
            (void)inc_timed(TMD_BECOME_LICH, plev + 5 + randint(plev/2));
            break;
        }
        
        case DARK_WALL_OF_HELLFIRE: /* short lasting for assassins */
        {
            int time = randint(plev/2) + (25);
            if (randint(100) + badluck > 66 + goodluck)
            {
                (void)inc_timed(TMD_WITCH, 600 + randint(200-plev));
            }
            if (p_ptr->timed[TMD_BECOME_LICH])
            {
               if ((badluck > 9) && (randint(100) < 5))
               {
                  msg_print("The fires of hell eagerly consumes your lifeless flesh.");
                  fire_dam((time*3) + randint(time*2), "Summoning fire while in the form of an undead.");
               }
               else if ((badluck > 5) && (randint(100) < 10))
               {
                  msg_print("The fires of hell eagerly scorch your lifeless flesh.");
                  fire_dam((time*2) + randint(time), "Summoning fire while in the form of an undead.");
               }
               fire_dam((time*2) + randint(time), "Summoning fire while in the form of an undead.");
               msg_print("The summoned fire causes you intense pain so you banish it quickly.");
            }
            else
            {
               (void)clear_timed(TMD_OPP_COLD);
               if ((badluck > 5) && (randint(100) < 8))
               {
                  msg_print("The fires of hell eagerly scorch your evil flesh.");
                  fire_dam((time) + randint(time), "Summoning hellfire.");
               }
               else if ((badluck > 9) && (randint(100) < 4))
               {
                  msg_print("The fires of hell eagerly consumes your evil flesh.");
                  fire_dam((time*2) + randint(time), "Summoning hellfire.");
               }
               if (cp_ptr->flags & CF_ASSASSIN) (void)inc_timed(TMD_WSHIELD, time/2);
               else (void)inc_timed(TMD_WSHIELD, time);
               if (cp_ptr->flags & CF_ASSASSIN) (void)inc_timed(TMD_OPP_FIRE, time/2);
               else (void)inc_timed(TMD_OPP_FIRE, time);
               if (cp_ptr->flags & CF_ASSASSIN) (void)inc_timed(TMD_IMM_FIRE, time/5);
               else (void)inc_timed(TMD_IMM_FIRE, time/3);
            }
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
	else if (tval == TV_CHEM_BOOK)
	{
		return cast_chem_spell(index);
	}
	else if (tval == TV_DARK_BOOK)
	{
		return cast_dark_spell(index);
	}
/*	else if (tval == TV_MIND_BOOK)
	{
		return cast_chem_spell(index);
	} */
	else
	{
		return cast_priest_spell(index);
	}
}
