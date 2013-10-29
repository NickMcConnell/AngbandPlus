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
/* Magic for Beginners 0 */
#define SPELL_MAGIC_MISSILE             0
#define SPELL_DETECT_MONSTERS           1
#define SPELL_PHASE_DOOR                2
#define SPELL_LIGHT_AREA                3
#define SPELL_FIND_TRAPS_DOORS          4
#define SPELL_CURE_LIGHT_WOUNDS         5
#define SPELL_WIZARD_LOCK               7
#define SPELL_STINKING_CLOUD            11

/* Conjurings and Tricks 1 */
#define SPELL_CONFUSE_MONSTER           13
#define SPELL_LIGHTNING_BOLT            12
#define SPELL_TRAP_DOOR_DESTRUCTION     19
#define SPELL_CURE_POISON               25
#define SPELL_SLEEP_MONSTER             14
#define SPELL_TELEPORT_SELF             30
#define SPELL_SPEAR_OF_LIGHT            20
#define SPELL_FROST_BOLT                16
#define SPELL_WONDER                    15

/* Incantations and Illusions 2 */
#define SPELL_SATISFY_HUNGER            26
#define SPELL_RECHARGE_ITEM_I           50
#define SPELL_TURN_STONE_TO_MUD         21
#define SPELL_FIRE_BOLT                 18
#define SPELL_POLYMORPH_OTHER           35
#define SPELL_IDENTIFY                  8
#define SPELL_DETECT_INVISIBLE          9
#define SPELL_ACID_BOLT                 17
#define SPELL_SLOW_MONSTER              31

/* Sorcery and Evocations 3 */
#define SPELL_FROST_BALL                55
#define SPELL_TELEPORT_OTHER            32
#define SPELL_HASTE_SELF                29
#define SPELL_MASS_SLEEP                39
#define SPELL_FIRE_BALL                 57
#define SPELL_DETECT_ENCHANTMENT        10
#define SPELL_ELEMENTAL_BRAND           54 /* move to book 3 */

/* Resistances of Scarabtarices 4 */
#define SPELL_MANA_DEFENCE              65
#define SPELL_RESIST_COLD               44
#define SPELL_RESIST_FIRE               45
#define SPELL_RESIST_POISON             46
#define SPELL_RESISTANCE                47
#define SPELL_SHIELD                    48

/* Raal's Tome of Destruction 5 */
#define SPELL_CLOUD_KILL                38
#define SPELL_SHOCK_WAVE                36
#define SPELL_EXPLOSION                 37
#define SPELL_ACID_BALL                 56
#define SPELL_ICE_STORM                 58
#define SPELL_METEOR_SWARM              60
#define SPELL_RIFT                      62

/* Mordenkainen's Escapes 6 */
#define SPELL_DOOR_CREATION             22
#define SPELL_EARTHQUAKE                23
#define SPELL_SPHERE_CHARM1             66
#define SPELL_STAIR_CREATION            24
#define SPELL_TELEPORT_LEVEL            33
#define SPELL_WORD_OF_RECALL            34
#define SPELL_RUNE_OF_PROTECTION        49
#define SPELL_SUMMON_DEMONIC_AID        64

/* The Enchantments of War 7 */
#define SPELL_FALSE_LIFE	            6
#define SPELL_HEROISM                   27
#define SPELL_BERSERKER                 28
#define SPELL_SPHERE_CHARM2             67
#define SPELL_ENCHANT_WEAPON            52
#define SPELL_ENCHANT_ARMOR             51
#define SPELL_ELEMENTAL_BRAND2          68
#define SPELL_DRAGONFIRE                69

/* Kelek's Grimoire of Power 8 */
#define SPELL_BEDLAM                    40
#define SPELL_REND_SOUL                 41
#define SPELL_WORD_OF_DESTRUCTION       42
#define SPELL_RECHARGE_ITEM_II          53 /* moved from book 7 */
#define SPELL_BANISHMENT                59
#define SPELL_CHAOS_STRIKE              43
#define SPELL_MASS_BANISHMENT           61
#define SPELL_MANA_STORM                63

        /* Prayers: */
/* Minor Healings and Defences 0 */
#define PRAYER_CURE_LIGHT_WOUNDS        0
#define PRAYER_DETECT_EVIL              1
#define PRAYER_BLESS                    2
#define PRAYER_BOLDNESS                 3
#define PRAYER_CALL_LIGHT               4
#define PRAYER_SLOW_POISON              5
#define PRAYER_CURE_SERIOUS_WOUNDS      6
#define PRAYER_SPEAR_OF_LIGHT           7
#define PRAYER_SATISFY_HUNGER           8

/* Godly Insights 1 */
#define PRAYER_FIND_TRAPS               9
#define PRAYER_DETECT_DOORS_STAIRS     10
#define PRAYER_DETECT_LIFE             11
#define PRAYER_REMOVE_CURSE            12
#define PRAYER_SENSE_INVISIBLE         13
#define PRAYER_SENSE_SURROUNDINGS      14
#define PRAYER_PERCEPTION              15
#define PRAYER_WORD_OF_RECALL          16
#define PRAYER_TRUE_SIGHT              17

/* Defences of Light 2 */
#define PRAYER_NEUTRALIZE_POISON       18
#define PRAYER_SCARE_MONSTER           19
#define PRAYER_RESIST_HEAT_COLD        20
#define PRAYER_CHANT                   22
#define PRAYER_SANCTUARY               24
#define PRAYER_CURE_CRITICAL_WOUNDS    21
#define PRAYER_PORTAL                  23
#define PRAYER_PROTECTION_FROM_EVIL    25
#define PRAYER_HEAL                    26

/* Exorcism and Dispelling 3 */
#define PRAYER_TURN_UNDEAD             27
#define PRAYER_CURE_MORTAL_WOUNDS      28
#define PRAYER_ORB_OF_DRAINING         30
#define PRAYER_EARTHQUAKE              63
#define PRAYER_DISPEL_UNDEAD           31
#define PRAYER_DISPEL_EVIL             32
#define PRAYER_SILENCE_SUMMONS         29 /* replaces prayer */
#define PRAYER_HOLY_WORD               33

/* Purifications and Healing 4 */
#define PRAYER_CURE_SERIOUS_WOUNDS2    34
#define PRAYER_CURE_MORTAL_WOUNDS2     35
#define PRAYER_CURING                  36
#define PRAYER_PURIFY                  37
#define PRAYER_RESIST_POISON           38 /* healer only, was DISPEL_CURSE */
#define PRAYER_HEALING                 39
#define PRAYER_RESTORATION             40
#define PRAYER_REMEMBRANCE             41

/* Holy Infusions 5 */
#define PRAYER_UNBARRING_WAYS          42
#define PRAYER_RECHARGING              43
#define PRAYER_DAYLIGHT                44
#define PRAYER_RESIST_SILVER           53 /* Rsilver, charm, hallu & amnesia */
#define PRAYER_BLESS_WEAPON            47 /* changed from enchant weapon */
#define PRAYER_SANCTIFY_BATTLE         48
#define PRAYER_GLYPH_OF_WARDING        56

/* Divine Heroics 6 */
#define PRAYER_MASS_SLEEP              50
#define PRAYER_TRUCE				   70
#define PRAYER_BREATH_SHIELD           51
#define PRAYER_GODSPEED                45
#define PRAYER_TELEPORT_OTHER          52
#define PRAYER_PROTECTION_FROM_EVIL2   54
#define PRAYER_SILENCE_SUMMONS_H       46 /* Healer version */
#define PRAYER_HOLY_LANCE              64

/* Sacred Words of Wisdom 7 */
#define PRAYER_BLINK                   57
#define PRAYER_DETECTION               58
#define PRAYER_TELEPORT_OTHER2         61 /* replaces telepathy */
#define PRAYER_FULL_IDENTIFY           59
#define PRAYER_REFUGE				   60
#define PRAYER_RESISTANCE              49
#define PRAYER_ALTER_REALITY           62

/* Wrath of God 8 */
#define PRAYER_BANISH_EVIL             55
#define PRAYER_DISPEL_SILVER2          65
#define PRAYER_DISPEL_UNDEAD2          66
#define PRAYER_DISPEL_EVIL2            67
#define PRAYER_WORD_OF_DESTRUCTION     68
#define PRAYER_ANNIHILATION            69

        /* Nature */
/* Call of the Wild 0 */
#define NEWM_DETECT_ANIMAL              0
#define NEWM_CALL_LIGHT                 1
#define NEWM_SLOW_POISON                2
#define NEWM_PHASE_DOOR                 3
#define NEWM_SATISFY_HUNGER             4
#define NEWM_SPARK                      5
#define NEWM_SPEAR_OF_LIGHT             6
#define NEWM_TRAP_DOOR_DESTRUCTION      7
#define NEWM_TURN_STONE_TO_MUD          8

/* Communion with Nature 1 */
#define NEWM_NEUTRALIZE_POISON          9
#define NEWM_FROST_BOLT                10
#define NEWM_SLEEP_MONSTER             11
#define NEWM_SCARE_MONSTER             12
#define NEWM_NATURAL_VITALITY          21
#define NEWM_FIND_TRAPS_DOORS          13
#define NEWM_STUN_MONSTER              14
#define NEWM_BUG_SPRAY                 15

/* Gifts of Nature 2 */
#define NEWM_RESIST_HEAT_COLD          18
#define NEWM_RESIST_ACID_ELEC          19
#define NEWM_DETECT_LIFE               20
#define NEWM_REMOVE_CURSE              17
#define NEWM_PORTAL					   70
#define NEWM_HERBAL_HEALING            28
#define NEWM_IDENTIFY                  24
#define NEWM_EARTHQUAKE                23

/* Natural Combat 3 */
#define NEWM_FIRE_SPRAY				   71
#define NEWM_FIRE_BOLT                 16
#define NEWM_NYMPH_BLESSING            32
#define NEWM_MIGHTY_THROW			   69
#define NEWM_WITHER_FOE                33
#define NEWM_ELEC_STORM                34
#define NEWM_THUNDERCLAP               35
#define NEWM_BLIZZARD                  36

/* Druidic Lore 4 */
#define NEWM_SENSE_INVISIBLE           29
#define NEWM_SENSE_SURROUNDINGS        30
#define NEWM_RESIST_SILVER             26 /* was detect_evil */
#define NEWM_RESIST_POISON             22
#define NEWM_SONG_SCARE                27
#define NEWM_TELEPORT_OTHER            25
#define NEWM_TRUE_SIGHT                31

/* Bombadil's Songs 5 */
#define NEWM_SONG_LULLING              43
#define NEWM_SONG_HEROISM              44
#define NEWM_CALL_HELP                 45
#define NEWM_SPHERE_CHARM              68
#define NEWM_SONG_PROTECTION           46
#define NEWM_SONG_DISPELLING           47
#define NEWM_WARDING                   48
#define NEWM_RENEWAL                   49

/* Primal Forces 6 */
#define NEWM_STARLITE                  37
#define NEWM_HURLED_STAR1              38
#define NEWM_TSUNAMI                   39
#define NEWM_MOLTEN_LIGHTNING          40
#define NEWM_VOLCANIC_ERUPTION         41
#define NEWM_RIFT                      42

/* Spirits of Nature 7 */
#define NEWM_ESSENCE_SPEED             50
#define NEWM_INFUSION                  51
#define NEWM_BANISH_UNNATURAL          54
#define NEWM_TELEPORT_LEVEL            53
#define NEWM_REMEMBRANCE               55
#define NEWM_EXTRA_BLOW                67
#define NEWM_NATURE_BLESSING           52
#define NEWM_ELEMENTAL_BRAND           56
#define NEWM_TELEPATHY                 57 /* done up to here */

/* Power of Enlightenment 8 (mystics only, do later) */
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
#define LUCK_DETECT_TREASURE           2 /* changed to detect objects also */
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
#define LUCK_DISINFECTANT              28
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
#define LUCK_BURST_OF_LIGHT            60
#define LUCK_DETECT_ENCHANTMENT        26

/* Guidebook for Tourists 3(only for tourists)*/
#define LUCK_DETECT_TRAPS              27
#define LUCK_MAP_AREA                  29
#define LUCK_ADJUST_CURSE              30
#define LUCK_RECHARGING                12
#define LUCK_TELEKINESIS2              31
#define LUCK_DETECT_ENCHANTMENT2       32
#define LUCK_PROBING                   33
#define LUCK_MAP_LEVEL                 34
#define LUCK_FULL_IDENTIFY             35

/* Quick Getaways 4(not for tourists)*/
#define LUCK_HIT_N_RUN                 36
#define LUCK_BLINK_MONSTER             51
#define LUCK_TELEPORT_SELF             37
#define LUCK_TELEPORT_OTHER            38
#define LUCK_SLOW_MONSTERS             39
#define LUCK_MASS_AMNESIA              41
#define LUCK_SLIP_INTO_SHADOWS         42
#define LUCK_HASTE_SELF                43

/* The Lottery 5(not for rogues)*/
#define LUCK_LUCK_BOLT				   74
#define LUCK_POLYMORPH_OTHER           44
#define LUCK_ADJUST_SPEED              45
#define LUCK_BANISH_SUMMON             46
#define LUCK_AFFECT_SELF               47
#define LUCK_AFFECT_OTHER              48
#define LUCK_POTLUCK_STATS             49
#define LUCK_TREASURE_MAP              73
#define LUCK_AQUIREMENT                50

/* Lucky Escapes: RE 6(mostly not for tourists)*/
#define LUCK_DOOR_CREATION2            52 /* renamed to seal/open getaway */
#define LUCK_STAIR_CREATION2           53
#define LUCK_LULLABY                   24
#define LUCK_TELEPORT_LEVEL            54 /* change to taming */
#define LUCK_WORD_OF_DESTRUCTION       55
#define LUCK_TELEPORT_CONTROL          56
#define LUCK_RUNE_OF_PROTECTION        57

/* Rogue's Defences 7(not for tourists)*/
#define LUCK_RESIST_HEAT_ELEC          58
#define LUCK_BOOMERANG_THROW           59
#define LUCK_SENSE_SURROUNDINGS        25
#define LUCK_CURING                    61
#define LUCK_SUPER_ROGUE               62
#define LUCK_SUSTAIN_SPEED		       63
#define LUCK_RESISTANCE                64

/* The Spreading of Chaos 8*/
#define LUCK_STINK                     65
#define LUCK_BERSERKER                 66
#define LUCK_BEDLAM                    67
#define LUCK_EXTRA_BLOW                40
#define LUCK_CHAOS_STRIKE              68
#define LUCK_RIFT                      69
#define LUCK_BREATHE_CHAOS             70 /* needs testing */
#define LUCK_MASS_CHAOS                71 /* needs testing */
#define LUCK_BIZZARE_EFFECTS           72 /* needs testing */

        /* Alchemy - working on */
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
#define CHEM_SKILL_BOOST               9
#define CHEM_RESIST_ACID               10
#define CHEM_CURE_MODERATE_WOUNDS      11
#define CHEM_RESIST_HEAT_COLD          12
#define CHEM_SATISFY_HUNGER            13
#define CHEM_CHEMICAL_RAGE             14
#define CHEM_RESTORE_MEMORIES          15
#define CHEM_RESIST_POISON             16
#define CHEM_HEAL                      17

/* Chemical Detections and Tools 2*/
#define CHEM_DETECT_TREASURE           18 /* now also detect objects */
#define CHEM_TRAP_DOOR_DESTRUCTION     19
#define CHEM_CLEAR_MIND                20
#define CHEM_DETECT_INVISIBLE          22 /* detect hidden */
#define CHEM_IDENTIFY                  21
#define CHEM_DETECT_ENCHANTMENT        24
#define CHEM_ACID_COAT_AMMO            25

/* Quick Getaways (mainly for thieves)3*/
#define CHEM_BURST_OF_SPEED            26
#define CHEM_TELEPORT_SELF             27
#define CHEM_SLOW_MONSTERS             29
#define CHEM_MASS_AMNESIA              30 /* Mist of Amnesia */
#define CHEM_HASTE_SELF                31
#define CHEM_TELEPORT_OTHER            28
#define CHEM_TUNNEL_DIGGING            57
#define CHEM_TELEPORT_LEVEL            32
#define CHEM_EMERGENCY_GETAWAY_SHIELD  34

/* Missile Magic (archers only (still sval 4 but native deeper than before)4*/
#define CHEM_LIGHTNING_BOLT            35
#define CHEM_RAIN_OF_ARROWS            36
#define CHEM_CHAOS_ARROW               37
#define CHEM_ENCHANT_TO_HIT            39
#define CHEM_ENCHANT_TO_DAM            40
#define CHEM_POISON_BRAND_AMMO         41
#define CHEM_ROCKET_BLAST              38
#define CHEM_ELEMENTAL_BRAND_AMMO      42
#define CHEM_METEOR_SWARM              73

/* Chemical Combat (not for archers)5*/
#define CHEM_MIMMIC_MIMMIC             43
#define CHEM_MIMMIC_ZAP                44 /* hard-coded variable mana */
#define CHEM_ENHANCE_WAND              52 
#define CHEM_REPUGNANCE                45 /* maybe replace with potion transferral */
#define CHEM_ZOMBIE_FLAME              47
#define CHEM_LASTING_REPUGNANCE        46
#define CHEM_MAKE_GRENADE			   70
#define CHEM_ELEMENT_BALL              49 /* alchemical dragonbreath (almost same as the potion) */
#define CHEM_ACID_BALL                 50

/* Safety Precautions 6*/
#define CHEM_SAFETY_GOGGLES            68
#define CHEM_SUSTAIN_HEALTH            69
#define CHEM_BREATH_SHIELD             71
#define CHEM_ENCHANT_ARMOR             72
#define CHEM_GUARD_QUIVER			   75 /* quiver protection */
#define CHEM_RESISTANCE                23
#define CHEM_GLYPH_OF_WARDING          54

/* Advanced Thievery (thieves only)7*/
#define CHEM_HIT_N_RUN                 61
#define CHEM_SLIP_INTO_SHADOWS         62
#define CHEM_POISON_BOLT               63
#define CHEM_CLOUDKILL2                64
#define CHEM_FIND_VAULT                66
#define CHEM_RESTORE_DEX1              48
#define CHEM_TELEKINESIS               74
#define CHEM_BEDLAM					   65
#define CHEM_POISON_BRAND_WEAPON2      67

/* Core of Alchemy (alchemists only)8*/
#define CHEM_RECHARGING				   56
#define CHEM_SILVER_BULLET             55
#define CHEM_CLOUDKILL1                53
#define CHEM_ICE_STORM                 51
#define CHEM_EXPLOSION                 58
#define CHEM_POISON_BRAND_WEAPON1      60
#define CHEM_RESTORATION               59
#define CHEM_ELIXIR_OF_LIFE            33

        /* Black Magic */
/* Dark Initiation (not for assassin) 0*/
#define DARK_SHADOW_STING              75
#define DARK_DETECT_EVIL               0
#define DARK_ALERTNESS                 1
#define DARK_CALL_DARK                 2
#define DARK_SCARE_MONSTER             3
#define DARK_NOXIOUS_FUMES             4
#define DARK_CURE_POISON               5
#define DARK_SLEEP_MONSTER             6
#define DARK_SEE_INVISIBLE             8

/* Blood Novitiate 1*/
#define DARK_FIND_TRAPS                9
#define DARK_DETECT_DOORS_STAIRS       10
#define DARK_SHADOW_SHIFT              11
#define DARK_WITCH_DISINFECTANT        18
#define DARK_DRAIN_CHARGES             12
#define DARK_TRAP_DOOR_DESTRUCTION     20
#define DARK_DETECT_LIVING             59
#define DARK_STINK                     15
#define DARK_SHADOW_STEP               23 /* early version slip into the shadows */

/* Mysteries of Shadow 2*/
#define DARK_SHADOW_WARP               33
#define DARK_WIZARD_LOCK               34
#define DARK_SHADOW_MAPPING            35
#define DARK_BREAK_CURSE               36
#define DARK_RECHARGING                57
#define DARK_PROBING                   37
#define DARK_IDENTIFY                  38
#define DARK_EXORCISE_DEMONS           40
#define DARK_DETECT_ENCHANTMENT        41

/* Frightful Realms 3*/
#define DARK_POISON_BOLT               21 /* maybe replace */
#define DARK_SPEAR_OF_DARK             27
#define DARK_ICE_SPEAR                 28
#define DARK_BEAM_OF_DESTRUCTION       30
#define DARK_SCARE_MONSTERS            26
#define DARK_DESTRUCTION_BALL          31
#define DARK_VENOM_BOLT                24
#define DARK_CIRCLE_OF_FLAME	       19
#define DARK_CHAOS_STRIKE              32

/* Advanced Necromancy 4*/
#define DARK_DEMON_WARD				   17
#define DARK_WITHER_FOE                7
#define DARK_DEATH_BOLT                14
#define DARK_TURN_UNDEAD               13
#define DARK_PROTECT_FROM_LIFELESS     45
#define DARK_DISPEL_UNDEAD1            16
#define DARK_STENCH_OF_DEATH           29

/* Unholy Protection & Curing (not for assassin) 5*/
#define DARK_BOLDNESS                  42
#define DARK_FALSE_LIFE                43
#define DARK_RESIST_POISON2            44
#define DARK_UNDEAD_SKIN               46
#define DARK_HASTE                     61
#define DARK_SUMMON_DEMONIC_AID        39
#define DARK_UNHOLY_RESISTANCE         47
#define DARK_SHADOW_BARRIER            48
#define DARK_SPIRIT_OF_BALROG          60

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
#define DARK_SLIP_INTO_SHADOWS         22
#define DARK_DARKVISION                25
#define DARK_SEE_ALL_FOES              64 /* see invisible */
#define DARK_RESIST_POISON             73
#define DARK_BLOODWRATH                62
#define DARK_SNIPER_EYE                58
#define DARK_POISON_AMMO               63

/* Necronomicon (mostly not for assassin) 8*/
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
		realm = 6; */
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
		int curep;

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
			curep = (p_ptr->mhp * 10) / 100; /* 10% */
			strnfmt(p, len, " heal 2d8, min%d", curep);
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
			if (cp_ptr->flags & CF_POWER_SHIELD) strnfmt(p, len, " dam %dd9", (8 + ((plev - 4) / 4)));
            else strnfmt(p, len, " dam %dd8", (8 + ((plev - 5) / 4)));
			break;
		case SPELL_FIRE_BOLT:
			if (cp_ptr->flags & CF_POWER_SHIELD) strnfmt(p, len, " dam %dd8", (7 + ((plev - 4) / 4)));
			else strnfmt(p, len, " dam %dd8", (6 + ((plev - 5) / 4)));
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
		case SPELL_MANA_DEFENCE:
			strnfmt(p, len, " dur 30+d30");
			break;
        case SPELL_FALSE_LIFE:
			strnfmt(p, len, " dur %d+d%d", plev+10, plev+10);
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
		int curep;

		/* Analyze the spell */
		switch (spell)
		{
			case PRAYER_CURE_LIGHT_WOUNDS:
				curep = (p_ptr->mhp * 12) / 100; /* 12% */
				strnfmt(p, len, " heal 2d10, min%d", curep);
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
				curep = (p_ptr->mhp * 18) / 100; /* 18% */
				strnfmt(p, len, " heal 4d10, min%d", curep);
				break;
			case PRAYER_CHANT:
				if (cp_ptr->flags & CF_ZERO_FAIL) strnfmt(p, len, " dur %d+d%d", plev/2 + 25, plev/2 + 25);
				else strnfmt(p, len, " dur %d+d%d", plev/2 + 18, plev/2 + 20);
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
				curep = (p_ptr->mhp * 22) / 100; /* 22% */
				strnfmt(p, len, " heal 6d10, min%d", curep);
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
				curep = (p_ptr->mhp * 30) / 100; /* 30% */
				strnfmt(p, len, " heal 8d10, min%d", curep);
				break;
			case PRAYER_BREATH_SHIELD:
				strnfmt(p, len, " dur %d+d%d", (plev/3) + 7, plev/3);
				break;
			case PRAYER_DISPEL_UNDEAD:
				strnfmt(p, len, " dam d%d", 3 * plev);
				break;
			case PRAYER_RESIST_SILVER:
				if (plev < 40) strnfmt(p, len, " dur %d+d%d", plev, plev + 15);
				else strnfmt(p, len, " dur 39+d%d", (plev * 3)/2);
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
				curep = (p_ptr->mhp * 20) / 100; /* 20% */
				strnfmt(p, len, " heal 4d10, min%d", curep);
				break;
			case PRAYER_CURE_MORTAL_WOUNDS2:
				curep = (p_ptr->mhp * 33) / 100; /* 33% */
				strnfmt(p, len, " heal 8d10, min%d", curep);
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
	   	    case PRAYER_DAYLIGHT:
				strnfmt(p, len, " dur %d + d%d", 15 + plev/4, 10 + plev/2);
			    break;
            case PRAYER_GODSPEED:
				strnfmt(p, len, " dur %d + d%d", plev+2, 8 + plev/2);
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
				if (plev >= 9) strnfmt(p, len, " dam %d+d%d", plev/3, plev / 2 + 1);
			    else my_strcpy(p, " dur 3+d4", len);
			    break;
			case NEWM_RESIST_HEAT_COLD:
				my_strcpy(p, " dur 12+d12", len);
				break;
			case NEWM_RESIST_ACID_ELEC:
				my_strcpy(p, " dur 12+d12", len);
				break;
			case NEWM_NATURAL_VITALITY:
			    strnfmt(p, len, " heal %dd4+9", plev / 3);
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
			case NEWM_PORTAL:
				strnfmt(p, len, " range %d", 3 * plev);
				break;
			case NEWM_RESIST_SILVER:
				if (plev < 40) strnfmt(p, len, " dur %d+d%d", plev, plev + 15);
				else strnfmt(p, len, " dur 39+d%d", (plev * 3)/2);
				break;
		    case NEWM_FIRE_BOLT:
			    strnfmt(p, len, " dam %dd8", 6 + ((plev - 5) / 4));
			    break;
			case NEWM_FIRE_SPRAY:
			    strnfmt(p, len, " dam %d+d%d", 5 + (plev+3)/4, plev/2);
				break;
			case NEWM_NYMPH_BLESSING: /* (just like chant) */
				my_strcpy(p, " dur 24+d24", len);
				break;
			case NEWM_MIGHTY_THROW:
			{
				int endurance = (adj_con_fix[p_ptr->stat_ind[A_CON]]);
				if (p_ptr->prace == 16) endurance = 10;
				if ((cp_ptr->flags & CF_HEAVY_BONUS) || (p_ptr->prace == 17)) endurance += 2;
			    strnfmt(p, len, " dur %d +d%d", endurance*2, (plev / 2) + endurance);
			    break;
			}
		    case NEWM_SPHERE_CHARM:
            {
                int time = adj_chr_charm[p_ptr->stat_ind[A_CHR]] * 2;
			    int timeb = (plev * 3) / 2;
			    if (time + timeb < 74) strnfmt(p, len, " dur 37+d37");
			    else if (time + timeb > 150) strnfmt(p, len, " dur 73+d77");
			    else strnfmt(p, len, " dur d%+dd%", timeb, time);
			    break;
            }
		    case NEWM_WITHER_FOE:
			    strnfmt(p, len, " dam %dd8", plev / 7);
			    break;
		    case NEWM_ELEC_STORM:
			    strnfmt(p, len, " dam %d+d%d", (plev/2)+30, (plev*2));
			    break;
		    case NEWM_THUNDERCLAP:
			    strnfmt(p, len, " dam %d+d%d", (plev/2)+2, plev*-2);
			    break;
		    case NEWM_BLIZZARD:
			    strnfmt(p, len, " dam %d+d%d", plev+10, ((plev*10)/3));
			    break;
		    case NEWM_STARLITE:
			    strnfmt(p, len, " dam 10d8");
			    break;
		    case NEWM_HURLED_STAR1:
			    strnfmt(p, len, " total dam (clvl*2)+25 + d(clvl*2)");
			    break;
		    case NEWM_TSUNAMI: 
			    strnfmt(p, len, " dam %d+d%d", (plev * 3), plev);
			    break;
            case NEWM_MOLTEN_LIGHTNING:
			    strnfmt(p, len, " dam %d+d%d", (plev*2)+50, (plev*4));
			    break;
            case NEWM_VOLCANIC_ERUPTION:
			    strnfmt(p, len, " dam %d+d%d", (plev*3)+50, ((plev*10)/3));
			    break;
            case NEWM_RIFT:
			    strnfmt(p, len, " dam %d+%dd6", (plev*2)+10, plev);
			    break;
		    case NEWM_SONG_HEROISM:
			    strnfmt(p, len, " dur 25+d25");
			    break;
		    case NEWM_SONG_PROTECTION:
			    strnfmt(p, len, " dur 25+%d", (plev*2));
			    break;
            case NEWM_SONG_DISPELLING:
			    strnfmt(p, len, " dam d%d+d%d", ((plev*7)/3), plev);
			    break;
		    case NEWM_ESSENCE_SPEED:
			    strnfmt(p, len, " dur %d+d20", plev);
			    break;
		    case NEWM_TELEPATHY:
			    strnfmt(p, len, " dur %d+d%d", plev, plev);
				break;
		}
	}

	/* Chance spells */
	if (tval == TV_LUCK_BOOK)
	{
		int plev = p_ptr->lev;
		int curep;

		/* Analyze the spell */
		switch (spell)
		{
			case LUCK_CURE_LIGHT_WOUNDS:
				curep = (p_ptr->mhp * 11) / 100; /* 11% */
				strnfmt(p, len, " heal 2d%d+d%d, min%d", 2 + (p_ptr->luck/4), plev/4, curep);
				break;
            case LUCK_ILLUMINATION:
				strnfmt(p, len, " dam 1d%d", (plev / 2));
				break; 
		    case LUCK_CAMERA_FLASH:
			    strnfmt(p, len, " dam 1d%d", (plev / 4) + 1);
				break;
		    case LUCK_BUG_SPRAY:
				if (plev >= 9) strnfmt(p, len, " dam %d+d%d", plev/3, plev / 2 + 1);
			    else my_strcpy(p, " dur 3+d4", len);
			    break;
			case LUCK_DISINFECTANT:
				strnfmt(p, len, " dam %d + d%d", 2 + (plev+3)/6, (plev+2)/3);
			    break;
		    case LUCK_HIT_N_RUN:
				strnfmt(p, len, " dam 2d%d+1", (plev / 3) + 1);
				break;
		    case LUCK_BURST_OF_LIGHT:
			    strnfmt(p, len, " dam 6d8");
			    break;
	        case LUCK_SUSTAIN_SPEED:
				strnfmt(p, len, " dur %d+d%d", plev / 2, plev / 2);
				break;
			case LUCK_BOOMERANG_THROW:
				strnfmt(p, len, " dur %d+d%d", plev, (plev*3)/4);
				break;
			case LUCK_TELEPORT_CONTROL:
				strnfmt(p, len, " dur %d+d%d", plev, plev);
				break;
			case LUCK_SLIP_INTO_SHADOWS:
				strnfmt(p, len, " dur 30+d%d", plev);
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
		int curep;

		/* Analyze the spell */
		switch (spell)
		{
			case CHEM_DISINFECTANT:
			    strnfmt(p, len, " dam %d + d%d", 3 + plev/8, (plev+1)/2);
			    break;
            case CHEM_CURE_LIGHT_WOUNDS:
				curep = (p_ptr->mhp * 11) / 100; /* 11% */
				strnfmt(p, len, " heal 2d9, min%d", curep);
				break;
			case CHEM_CURE_MODERATE_WOUNDS:
				curep = (p_ptr->mhp * 20) / 100; /* 20% */
				strnfmt(p, len, " heal 6d8, min%d", curep);
				break;
			case CHEM_HEAL:
				curep = p_ptr->mhp / 3;
				strnfmt(p, len, " heal %dd9, min%d", plev/2, curep);
				break;
			case CHEM_STINKING_CLOUD:
			{
				if (plev < 3) strnfmt(p, len, " dam %d+d%d", plev + 2, 3);
				else if (plev < 9) strnfmt(p, len, " dam %d+d%d", plev + 2, (plev+3) / 2);
				else if (plev < 12) strnfmt(p, len, " dam %d+d%d", 11, plev / 2);
				else if (plev < 22) strnfmt(p, len, " dam %d+d%d", 11, plev / 2 - 1);
				else strnfmt(p, len, " dam %d+d%d", plev/2 + 1, 9);
			    /* else strnfmt(p, len, " dam %d", 10 + (plev / 2)); */
			    break;
			}
		    case CHEM_ACID_ARROW:
			    strnfmt(p, len, " dam %dd6", (3 + ((plev - 6) / 6)));
			    break;
			case CHEM_CHAOS_ARROW:
			    strnfmt(p, len, " dam %dd7 + 1", plev/4);
			    break;
			case CHEM_LIGHTNING_BOLT:
				strnfmt(p, len, " dam %dd6", (3 + ((plev - 5) / 6)));
				break;			
			case CHEM_ROCKET_BLAST:
			{
				int manya = 40;
				if (plev > 39) manya = 50;
				if (plev > 49) manya = 60;
			    strnfmt(p, len, " dam %d +d%d +%dd2", manya, plev/5, plev/2);
				break;
			}
		    case CHEM_POISON_BOLT:
			    strnfmt(p, len, " dam %dd7", 7 + (plev / 5));
			    break;
			case CHEM_SKILL_BOOST:
				strnfmt(p, len, " dur %d+d%d", (plev/2)+15, (plev/2)+20);
				break;
		    case CHEM_RESIST_ACID:
				strnfmt(p, len, " dur 15+d%d", (plev / 2));
				break;
			case CHEM_RESIST_HEAT_COLD:
				my_strcpy(p, " dur 10+d12", len);
				break;
			case CHEM_CLEAR_MIND:
				strnfmt(p, len, " dur 15+d%d", 10 + (plev / 2));
				break;
		    case CHEM_HIT_N_RUN:
				strnfmt(p, len, " dam 2d%d+1", (plev / 3) + 1);
				break;
			case CHEM_EMERGENCY_GETAWAY_SHIELD:
				strnfmt(p, len, " dur %d+d%d", 6 + (plev*3)/4, (plev*3)/4);
				break;
			case CHEM_SAFETY_GOGGLES:
				strnfmt(p, len, " dur %d+d%d", 10 + (plev*3)/4, plev);
				break;
			case CHEM_SUSTAIN_HEALTH:
				strnfmt(p, len, " dur %d+d%d", 8 + (plev*3)/4, plev);
				break;
			case CHEM_BREATH_SHIELD:
				strnfmt(p, len, " dur %d+d%d", plev/2 + 1, plev/3);
				break;
			case CHEM_SLIP_INTO_SHADOWS:
				strnfmt(p, len, " dur 30+d%d", plev);
				break;
			case CHEM_GUARD_QUIVER:
			{
				int scoreq = ((plev-20)/3) + ((goodluck + 1)/2);
				strnfmt(p, len, " dur %d+d%d", scoreq*5, plev + scoreq);
				break;
			}
			case CHEM_REPUGNANCE:
				strnfmt(p, len, " radius %d", 3 + (plev/8));
				break;
			case CHEM_LASTING_REPUGNANCE:
				strnfmt(p, len, " dur 20+d%d", plev/2 + 5);
				break;
			case CHEM_ZOMBIE_FLAME:
				strnfmt(p, len, " dam %dd6", plev/2);
				break;
			case CHEM_ELEMENT_BALL: /* alchemical dragonbreath */
				strnfmt(p, len, " dam %d", 40 + plev);
				break;
			case CHEM_ACID_BALL:
				strnfmt(p, len, " dam %d", 55 + plev);
				break;
			case CHEM_ICE_STORM:
				strnfmt(p, len, " dam %d", 50 + (plev * 2));
				break;
			case CHEM_CLOUDKILL1:
			case CHEM_CLOUDKILL2:
				strnfmt(p, len, " dam %d", 40 + (plev * 2));
				break;
		    case CHEM_SILVER_BULLET:
			    strnfmt(p, len, " dam %d + d%d", 25 + plev, plev);
			    break;
			case CHEM_EXPLOSION:
				strnfmt(p, len, " dam %d", 20 + (plev * 2));
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
		    case DARK_ALERTNESS:
			    strnfmt(p, len, " dur %d+d%d ", (plev * 3) + 10), plev;
			    break;
			case DARK_SEE_INVISIBLE:
			    strnfmt(p, len, " dam %d+24", plev);
			    break;
		    case DARK_CALL_DARK:
		    {
                if ((!p_ptr->darkvis) && (plev < 11)) strnfmt(p, len, " dam 10+d%d", plev+2);
		        else if (!p_ptr->darkvis) strnfmt(p, len, " dam 12+d%d", plev-3);
		        /* slight extra alertness if the player already has darkvision */
		        else if (plev < 11) strnfmt(p, len, " dam 4+d%d", plev+2);
                else strnfmt(p, len, " dam 6+d%d", plev-3);
                break;
            }
			case DARK_WITCH_DISINFECTANT:
				strnfmt(p, len, " dam %d + d%d", 3 + plev/8, (plev+1)/2);
				break;
			case DARK_SHADOW_STING:
			    if (plev < 4) strnfmt(p, len, " dam 2d%d", plev * 2);
			    else if (plev < 8) strnfmt(p, len, " dam 2d%d", 6);
			    else if (plev < 15) strnfmt(p, len, " dam 2d%d", plev-1);
			    else strnfmt(p, len, " dam %dd13", (plev-1)/10);
			    break;
			case DARK_DISPEL_UNDEAD1:
				strnfmt(p, len, " dam d%d", 3 * plev);
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
	        case DARK_CIRCLE_OF_FLAME:
			    strnfmt(p, len, " dam %d+d%d", 31 + ((plev+2)/4), (plev*9)/4);
			    break;
			case DARK_DEMON_WARD:
				if (plev < 12) strnfmt(p, len, " dur 4+d4");
				else strnfmt(p, len, " dur 4+d%d", plev/3);
				break;
			case DARK_FALSE_LIFE:
				strnfmt(p, len, " dur %d+d%d", plev, plev+10);
				break;
			case DARK_RESIST_POISON:
				my_strcpy(p, " dur 20+d20", len);
				break;
			case DARK_RESIST_POISON2:
				my_strcpy(p, " dur 20+d20", len);
				break;
			case DARK_SHADOW_STEP:
				my_strcpy(p, " dur 13+d13", len);
				break;
			case DARK_SLIP_INTO_SHADOWS:
				strnfmt(p, len, " dur 35+d%d", plev+5);
				break;
			case DARK_DARKVISION:
				strnfmt(p, len, " dur 20+d%d", plev);
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
			case DARK_SNIPER_EYE:
				strnfmt(p, len, " dur 20+d%d", plev-10);
				break;
			case DARK_VAMPIRIC_DRAIN:
			    strnfmt(p, len, " dam %dd11", plev / 3);
			    break;
			case DARK_SEE_ALL_FOES:
				my_strcpy(p, " dur 35+d35", len);
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


/*
 * Hook to specify staff or wand
 */
static bool item_tester_hook_drainable(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_WAND:
		case TV_STAFF:
		{
			return (TRUE);
		}
	}
	
	/* if at least clvl 30 then can drain other stuff too */
	if ((o_ptr->to_h > 0) || (o_ptr->to_d > 0) || (o_ptr->to_a > 0))
	{
		if (p_ptr->lev > 30) return (TRUE);
		
		/* maybe earlier for necromancers */
		if ((cp_ptr->flags & CF_ZERO_FAIL) && (p_ptr->lev > 20))
		{
           if (goodluck > 0) return (TRUE);
        }
    }

	return (FALSE);
}


static bool cast_mage_spell(int spell)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int dir, die;
	int fbdmg, dis, abdmg;

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
    /* smaller bonus from magic mastery */
    else if (p_ptr->skills[SKILL_DEV] > rp_ptr->r_dev + cp_ptr->c_dev + adj_int_dev[p_ptr->stat_ind[A_INT]] + (cp_ptr->x_dev * p_ptr->lev / 10) + 5)
    {
       plev += 2;
       beam += 1;
    }

	/* Spells. */
	switch (spell)
	{
		case SPELL_MAGIC_MISSILE:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_bolt_or_beam(beam-15, GF_MISSILE, dir,
			                  damroll(3 + ((plev - 1) / 5), 4));
			break;
		}

		case SPELL_DETECT_MONSTERS:
		{
            if (p_ptr->timed[TMD_2ND_THOUGHT])
            {
               msg_print("Your first sight supresses detection.");
            }
            else (void)detect_monsters_normal(1);
			break;
		}

		case SPELL_PHASE_DOOR:
		{
			bool controlled = FALSE;
            /* controlled teleport (random if you don't chose a target) */
		    if (p_ptr->telecontrol)
		    {
                if (control_tport(110 - plev, 12)) controlled = TRUE;
                if (!controlled) msg_print("You fail to control the teleportation.");
            }

            if (!controlled) teleport_player(10);
			break;
		}

		case SPELL_LIGHT_AREA:
		{
			(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
			break;
		}

		case SPELL_CURE_LIGHT_WOUNDS: /* min 10% */
		{
			int cure = damroll(2, 8);
			int curep = p_ptr->mhp / 10;
			if (cure < curep) cure = curep;
			if (cure > 39) cure = 39; /* maximum */
			(void)hp_player(cure);
			(void)dec_timed(TMD_CUT, 15);
			break;
		}

		case SPELL_WIZARD_LOCK:
		{
				if (!get_aim_dir(&dir)) return FALSE;

				/* Spell will jam a door or create a jammed door */
                /* in any one empty square. */
				fire_ball(GF_WIZLOCK, dir, 0, 0);
				break;
		}

		case SPELL_FIND_TRAPS_DOORS:
		{
            if (p_ptr->timed[TMD_2ND_THOUGHT])
            {
               msg_print("Your first sight supresses detection.");
            }
            else
            {
			  (void)detect_traps();
			  (void)detect_doorstairs(FALSE);
            }
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
			(void)confuse_monster(dir, plev + adj_chr_charm[p_ptr->stat_ind[A_CHR]]);
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
			(void)destroy_doors_touch(0);
			break;
		}

		case SPELL_SLEEP_MONSTER:
		{
            int pwr = plev + adj_chr_charm[p_ptr->stat_ind[A_CHR]];
            if (!get_aim_dir(&dir)) return (FALSE);
			(void)sleep_monster(dir, pwr);
			break;
		}

		case SPELL_CURE_POISON:
		{
			(void)clear_timed(TMD_POISONED);
			break;
		}

		case SPELL_TELEPORT_SELF:
		{
			bool controlled = FALSE;
			int dist = plev * 5;
			if (dist < 80) dist = 80;
            /* controlled teleport (random if you don't chose a target) */
		    if (p_ptr->telecontrol)
		    {
                if (control_tport(60 - plev, dist)) controlled = TRUE;
                if (!controlled) msg_print("You fail to control the teleportation.");
            }

            if (!controlled) teleport_player(plev * 5);
			break;
		}

		case SPELL_SPEAR_OF_LIGHT: /* spear of light */
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			msg_print("A line of blue shimmering light appears.");
			lite_line(dir, 9-(plev/3));
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
			(void)poly_monster(dir, plev + 1);
			break;
		}

		case SPELL_IDENTIFY:
		{
			return ident_spell();
		}

		case SPELL_MASS_SLEEP:
		{
            int pwr = plev + adj_chr_charm[p_ptr->stat_ind[A_CHR]];
			(void)sleep_monsters(pwr);
			break;
		}

		case SPELL_FIRE_BOLT:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			if (cp_ptr->flags & CF_POWER_SHIELD) fbdmg = damroll(7+((plev-4)/4), 8);
			else fbdmg = damroll(6+((plev-5)/4), 8);
			fire_bolt_or_beam(beam, GF_FIRE, dir, fbdmg);
			break;
		} /* 10d8 at L21, 12d8 at L30, 14d8 at L40 */

		case SPELL_SLOW_MONSTER:
		{
            int pwr = plev + adj_chr_charm[p_ptr->stat_ind[A_CHR]];
			if (!get_aim_dir(&dir)) return (FALSE);
			(void)slow_monster(dir, pwr);
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
			dis = plev + 12 + goodluck + randint(plev * 2);
            if (cp_ptr->flags & CF_ZERO_FAIL) dis += 8;
			(void)teleport_monster(dir, dis);
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
			fire_ball(GF_FIRE, dir, 55 + plev, 2);
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
            if (die < 25) do_call_help(989);
            else if (die < 50) do_call_help(991);
            else if (die < 75) do_call_help(992);
            else do_call_help(993);

            if (randint(100) + badluck > 84 + goodluck)
            {
                (void)inc_timed(TMD_WITCH, 100 + randint(150-plev));
            }
			break;
		}

		case SPELL_DOOR_CREATION:
		{
			(void)door_creation(0);
			break;
		}

		case SPELL_STAIR_CREATION:
		{
			(void)stair_creation(0);
			break;
		}

		case SPELL_TELEPORT_LEVEL:
		{
			(void)teleport_player_level(0);
			break;
		}

		case SPELL_EARTHQUAKE:
		{
			earthquake(py, px, 10, 0, 0, TRUE);
			break;
		}

		case SPELL_WORD_OF_RECALL:
		{
			set_recall(rand_int(20) + 15);
			break;
		}

		case SPELL_ACID_BOLT:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			if (cp_ptr->flags & CF_POWER_SHIELD) abdmg = damroll(8+((plev-4)/4), 9);
			else abdmg = damroll(8+((plev-5)/4), 8);
			fire_bolt_or_beam(beam, GF_ACID, dir, abdmg);
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
			if (!get_aim_dir(&dir)) return (FALSE);
            range = 5;
			fire_beam(GF_FIRE, dir, damroll(plev/4, 6));
            range = 5;
			fire_beam(GF_PLASMA, dir, damroll(plev/4, 6));
            range = 5;
			fire_ball(GF_FIRE, dir, damroll(plev/2, 8), 3);
            range = 5;
			fire_ball(GF_PLASMA, dir, damroll(plev/2, 8), 3);
			break;
		}

		case SPELL_ICE_STORM:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_ball(GF_ICE, dir, 50 + (plev * 2), 3);
			break;
		} /* 104 at L27, 120 at L35, 150 */

		case SPELL_METEOR_SWARM:
		{
            int swarmrad = 1;
			if (!get_aim_dir(&dir)) return (FALSE);
			if (plev + (goodluck+1)/2 + rand_int(6) > 49) swarmrad = 2;
			fire_swarm(2 + plev / 20, GF_METEOR, dir, 30 + plev/2 + randint(plev/2), swarmrad);
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
            if (p_ptr->timed[TMD_2ND_THOUGHT])
            {
               msg_print("Your first sight supresses detection.");
            }
            else 
            {
			  (void)detect_objects_magic();
            }
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
			brand_weapon(0);
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
		
		case SPELL_ELEMENTAL_BRAND2: /* war mage version */
		{
			brand_weapon(0);
			break;
		}

		case SPELL_HEROISM:
		{
			(void)hp_player(10);
			(void)clear_timed(TMD_AFRAID);
            if (p_ptr->peace)
        	{
                msg_print("The peaceful magic prevents you from becoming heroic.");
                break;
            }
			(void)inc_timed(TMD_HERO, randint(25) + 25);
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

        case SPELL_FALSE_LIFE:
		{
			(void)inc_timed(TMD_FALSE_LIFE, randint(plev + 10) + plev);
			(void)hp_player(plev);
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
            die = randint(75);
            if (p_ptr->peace)
        	{
                 msg_print("The peaceful magic prevents you from going into a rage.");
            }
	        else if ((p_ptr->timed[TMD_CHARM]) && (die > (plev + 25)))
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
			if (p_ptr->timed[TMD_SUST_SPEED])
			{
				msg_print("You cannot be hasted while your speed is sustained!");
				break;
			}
			
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
            if (randint(100) + badluck > 75 + goodluck)
            {
                (void)inc_timed(TMD_WITCH, 100 + randint(150-plev));
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
			return (warding_glyph());
		}

		case SPELL_ENCHANT_ARMOR: /* enchant armor */
		{
			return enchant_spell(0, 0, rand_int(3) + plev / 20, FALSE, FALSE);
		}

		case SPELL_ENCHANT_WEAPON: /* enchant weapon */
		{
			return enchant_spell(rand_int(4) + plev / 20,
			                     rand_int(4) + plev / 20, 0, FALSE, FALSE);
		}
	}

	/* Success */
	return (TRUE);
}


static bool cast_priest_spell(int spell)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int dir, die, dis;

	int plev = p_ptr->lev;

    /* spellcasting bonus */	
	if (p_ptr->timed[TMD_BRAIL])
    {
       plev += 10;
    }
    if ((goodweap) && (!badweap) && (randint(100) < 25 + goodluck/2))
    {
       plev += goodweap+1;
    }

	switch (spell)
	{
		case PRAYER_DETECT_EVIL:
		{
            if (p_ptr->timed[TMD_2ND_THOUGHT])
            {
               msg_print("Your first sight supresses detection.");
            }
            else 
            {
			  (void)detect_monsters_evil();
            }
			break;
		}
		
		case PRAYER_DETECT_LIFE:
		{
            if (p_ptr->timed[TMD_2ND_THOUGHT])
            {
               msg_print("Your first sight supresses detection.");
            }
            else 
            {
			  (void)detect_monsters_life();
            }
			break;
		}

		case PRAYER_CURE_LIGHT_WOUNDS:
		{
			int cure = damroll(2, 10);
			int curep = (p_ptr->mhp * 12) / 100;
			if (cure < curep) cure = curep;
			if (cure > 49) cure = 49; /* maximum */
			(void)hp_player(cure);
			(void)dec_timed(TMD_CUT, 10);
			break;
		}

		case PRAYER_BLESS:
		{
			(void)inc_timed(TMD_BLESSED, randint(11) + 11);
			break;
		}

		case PRAYER_BOLDNESS:
		{
			(void)clear_timed(TMD_AFRAID);
			if (randint(47) < (plev + 1 + goodluck/2)) (void)clear_timed(TMD_CHARM);
			if (p_ptr->peace) break;
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
            if (p_ptr->timed[TMD_2ND_THOUGHT])
            {
               msg_print("Your first sight supresses detection.");
            }
            else
            {
			  (void)detect_traps();
            }
			break;
		}

		case PRAYER_DETECT_DOORS_STAIRS:
		{
            if (p_ptr->timed[TMD_2ND_THOUGHT])
            {
               msg_print("Your first sight supresses detection.");
            }
            else
            {
			  (void)detect_doorstairs(FALSE);
            }
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
			lite_line(dir, 12-(plev/2));
			break;
		}
		
		case PRAYER_SCARE_MONSTER:
		{
            int pwr = plev + adj_chr_charm[p_ptr->stat_ind[A_CHR]];
			if (!get_aim_dir(&dir)) return (FALSE);
			if (randint(pwr + goodluck) > 50)
			{
				(void)fire_ball(GF_TURN_ALL, dir, pwr, 2);
			}
			else
			{
				(void)fear_monster(dir, pwr);
			}
			break;
		}

		case PRAYER_PORTAL:
		{
			bool controlled = FALSE;
			int dist = plev * 3;
			if (dist < 45) dist = 45;
            /* controlled teleport (random if you don't chose a target) */
		    if (p_ptr->telecontrol)
		    {
                if (control_tport(60 - plev, dist)) controlled = TRUE;
                if (!controlled) msg_print("You fail to control the teleportation.");
            }

            if (!controlled) teleport_player(plev * 3);
			break;
		}

		case PRAYER_CURE_SERIOUS_WOUNDS: /* min 18% */
		{
			int cure = damroll(4, 10);
			int curep = (p_ptr->mhp * 18) / 100;
			if (cure < curep) cure = curep;
			if (cure > 62) cure = 62; /* maximum */
			(void)hp_player(cure);
			(void)set_timed(TMD_CUT, (p_ptr->timed[TMD_CUT] / 2) - 20);
			break;
		}

		case PRAYER_CHANT: /* better for priests and healers */
		{
            int chant;
            if (cp_ptr->flags & CF_ZERO_FAIL) chant = randint(plev/2 + 25) + plev/2 + 25;
            else chant = randint(plev/2 + 20) + plev/2 + 18;
			(void)inc_timed(TMD_BLESSED, chant);
			break;
		}

		case PRAYER_SANCTUARY: /* improved */
		{
			int minplev;
			die = randint(100 + badluck);
            /* stronger for priests & healers */
			if ((cp_ptr->flags & CF_ZERO_FAIL) || (randint(100) < goodluck+1))
				minplev = plev+1;
			else minplev = (plev+1)/2;

			if ((minplev >= 24) && (die < (minplev * 3) / 2))
			{
				fire_spread(GF_OLD_SLEEP, plev + adj_chr_charm[p_ptr->stat_ind[A_CHR]], plev/8);
			}
			else if ((minplev >= 12) && (die < 40 + minplev * 2))
			{
				fire_spread(GF_OLD_SLEEP, (plev + adj_chr_charm[p_ptr->stat_ind[A_CHR]] + 3) / 2, 3);
			}
			else if ((minplev >= 10) && (die < 40 + minplev * 3))
			{
				fire_spread(GF_OLD_SLEEP, ((plev*2) + adj_chr_charm[p_ptr->stat_ind[A_CHR]] + 1) / 3, 2);
			}
			else
			{
				(void)sleep_monsters_touch((plev + adj_chr_charm[p_ptr->stat_ind[A_CHR]] + 3) / 2);
			}
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

		/* stops summoning and healing monster spells */
		case PRAYER_SILENCE_SUMMONS:
        {
			int die = ((plev+3)/4) + randint(plev/4 + goodluck);
			if (!get_aim_dir(&dir)) return (FALSE);
			if (die > 30)
			{
			   /* center spell on self with very wide radius */
               fire_spread(GF_SILENCE_S, (plev*2), 10);
               /* make sure to affect targetted monster */
               /* even if not in range of spread effect */
               fire_ball(GF_SILENCE_S, dir, plev, 1);
            }
			else if (die > 20)
			{
			   fire_ball(GF_SILENCE_S, dir, plev, 2);
            }
			else fire_ball(GF_SILENCE_S, dir, plev, 1);
            break;
        }
        
        case PRAYER_SILENCE_SUMMONS_H: /* Healer version */
        {
			int die = ((plev+3)/4) + randint(plev/4 + goodluck);
			if (!get_aim_dir(&dir)) return (FALSE);
			if (die > 31)
			{
			   /* center spell on self with very wide radius */
               fire_spread(GF_SILENCE_S, (plev*2), 10);
               /* make sure to affect targetted monster */
               /* even if not in range of spread effect */
               fire_ball(GF_SILENCE_S, dir, plev, 1);
            }
			else if (die > 22)
			{
			   fire_ball(GF_SILENCE_S, dir, plev, 2);
            }
			else fire_ball(GF_SILENCE_S, dir, plev, 1);
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
			if (plev > 40) range = 17;
			else range = 15;
			fire_beam(GF_HOLY_ORB, dir,
			          damroll((plev/10), 8) + plev + randint(plev/2));
			if (randint(plev/3) > 6) lite_line(dir, 0);
			break;
			/* av34 at L20, av51 at L30, av68 at L40, av85 at L50 */
        }	/* averages not including line_lite which adds 6d8 */

		case PRAYER_CURE_CRITICAL_WOUNDS: /* min 25% */
		{
			int cure = damroll(6, 10);
			int curep = (p_ptr->mhp * 25) / 100;
			if (cure < curep) cure = curep;
			(void)hp_player(cure);
			(void)clear_timed(TMD_CUT);
			(void)clear_timed(TMD_STUN);
			break;
		}

		case PRAYER_SENSE_INVISIBLE:
		{
            if (p_ptr->timed[TMD_2ND_THOUGHT])
            {
               msg_print("Your first sight supresses detection.");
            }
            else
            {
			  (void)inc_timed(TMD_SINVIS, randint(24) + 24);
            }
			break;
		}
		
        case PRAYER_TRUE_SIGHT:
        {
			(void)clear_timed(TMD_BLIND);
			(void)clear_timed(TMD_IMAGE);
			(void)clear_timed(TMD_2ND_THOUGHT);
			(void)detect_traps();
			(void)detect_doorstairs(FALSE);
			(void)detect_monsters_evil();
			(void)detect_monsters_invis();
			(void)inc_timed(TMD_TSIGHT, randint(35) + 35);
			break;
        }

		case PRAYER_PROTECTION_FROM_EVIL:
		{
			(void)inc_timed(TMD_PROTEVIL, randint(25) + 3 * plev);
			break;
		}

		case PRAYER_PROTECTION_FROM_EVIL2:
		{
			(void)inc_timed(TMD_PROTEVIL2, randint(25) + (plev / 2));
			break;
		}

		case PRAYER_EARTHQUAKE:
		{
			earthquake(py, px, 10, 0, 0, TRUE);
			break;
		}

		case PRAYER_SENSE_SURROUNDINGS:
		{
			map_area(FALSE);
			break;
		}

		case PRAYER_CURE_MORTAL_WOUNDS: /* min 30% */
		{
			int cure = damroll(8, 10);
			int curep = (p_ptr->mhp * 30) / 100;
			if (cure < curep) cure = curep;
			(void)hp_player(cure);
			(void)clear_timed(TMD_STUN);
			(void)clear_timed(TMD_CUT);
			break;
		}

		case PRAYER_TURN_UNDEAD:
		{
			int pwr = (p_ptr->lev + adj_chr_charm[p_ptr->stat_ind[A_CHR]])/2;
			if (adj_chr_charm[p_ptr->stat_ind[A_CHR]] < p_ptr->lev) pwr = p_ptr->lev - 1;
			(void)turn_undead(pwr);
			break;
		}

		case PRAYER_BREATH_SHIELD:
		{
			(void)inc_timed(TMD_BR_SHIELD, randint(plev/3) + (plev/3) + 7);
			break;
		}

		case PRAYER_DISPEL_UNDEAD:
		{
			(void)dispel_undead(randint(plev * 3));
			break;
		}

		case PRAYER_RESIST_SILVER:
		{
			if (plev < 40) (void)inc_timed(TMD_OPP_SILV, randint(15 + plev) + plev);
			else (void)inc_timed(TMD_OPP_SILV, randint((plev * 3)/2) + 39);
			break;
		}

		case PRAYER_HEAL: /* min 35% */
		{
			int cure = 300;
			int curep = (p_ptr->mhp * 35) / 100;
			if (cure < curep) cure = curep;
			(void)hp_player(cure);
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
            int pwr = plev + adj_chr_charm[p_ptr->stat_ind[A_CHR]];
			(void)sleep_monsters(pwr);
			break;
		}

		case PRAYER_GLYPH_OF_WARDING:
		{
			return (warding_glyph());
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
			(void)clear_timed(TMD_TERROR);
			(void)inc_timed(TMD_SANCTIFY, randint(plev/3) + (plev/3) + 7);
        }

		case PRAYER_TELEPORT_OTHER2:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			dis = plev + 12 + goodluck + randint(plev * 2);
            if (cp_ptr->flags & CF_ZERO_FAIL) dis += 8;
			(void)teleport_monster(dir, dis);
			break;
		}

		case PRAYER_TRUCE:
		{
			(void)truce();
			break;
		}

		case PRAYER_DETECTION:
		{
			if (p_ptr->timed[TMD_2ND_THOUGHT])
			{
				msg_print("Your first sight supresses detection.");
			}
			else
			{
				if (randint(60) < plev + goodluck + adj_int_spsk[p_ptr->stat_ind[A_WIS]])
					(void)detect_all(TRUE);
                else (void)detect_all(FALSE);
			}
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

		case PRAYER_CURE_SERIOUS_WOUNDS2: /* min 20% */
		{
			int cure = damroll(4, 10);
			int curep = (p_ptr->mhp * 20) / 100;
			if (cure < curep) cure = curep;
			(void)hp_player(cure);
			(void)clear_timed(TMD_CUT);
			break;
		}

		case PRAYER_CURE_MORTAL_WOUNDS2: /* min 33% */
		{
			int cure = damroll(8, 10);
			int curep = (p_ptr->mhp * 33) / 100;
			if (cure < curep) cure = curep;
			(void)hp_player(cure);
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
			(void)clear_timed(TMD_AMNESIA);
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

		case PRAYER_REFUGE:
		{
			(void)door_creation(10); /* maximum jamming except for chosen exit */
			msg_print("What direction do you want your exit to be?");
			if (get_aim_dir(&dir)) (void)disarm_trap(dir, 10);
			break;
		}

		case PRAYER_UNBARRING_WAYS: /* made into a beam */
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			(void)disarm_trap(dir, 0);
			/* make prayer a little different from equivelent wizardry spell */
			/* (void)destroy_doors_touch(0); */
			break;
		}
		
		case PRAYER_GODSPEED: /* die min==2, max==45 (max is extremely unlikely) */
		{
            int adjust;
            die = randint(10 + plev/4) + goodluck/2 + ((plev+5)/4);
			if ((plev > 40) && (die < 25)) die += randint(3 + plev/3);
			else if ((plev > 32) && (die < 20)) die += randint(4 + plev/3);
			if ((p_ptr->timed[TMD_SUST_SPEED]) && (die < 23))
			{
				msg_print("Your speed can't be adjusted while it is sustained!");
				break;
			}
			else if (p_ptr->timed[TMD_SUST_SPEED]) die -= (3 + randint(8));

            if (die > 35) adjust = 11;
            else if (die > 30) adjust = 10;
            else if (die > 25) adjust = 9;
            else if (die > 20) adjust = 8;
            else if (die > 15) adjust = 7;
            else if (die > 10) adjust = 6;
            else if (die > 5) adjust = 5;
            else adjust = 4;
            
            /* randomize some more */
            die = randint(100);
            if ((die > 94) && (adjust < 9)) adjust += randint(3) + 1;
            else if ((die > 89) && (adjust < 8)) adjust += randint(2);
            else if ((die < 6) && (adjust > 5)) adjust -= randint(2);
            if ((adjust > 11) && (goodluck < 16)) adjust = 11;
            else if ((adjust == 10) && (badluck > 2)) adjust = 10;
            else if ((adjust < 4) && (badluck < 16)) adjust = 4;
            else if ((adjust < 5) && (badluck < 1)) adjust = 5;
            
            /* if speed was already adjusted, don't decrease */
            if ((adjust > p_ptr->spadjust) || (badluck >= 7)) p_ptr->spadjust = adjust;

			if ((!p_ptr->timed[TMD_FAST]) && (!p_ptr->timed[TMD_ADJUST]))
			{
				(void)inc_timed(TMD_ADJUST, randint(8 + plev/2) + plev + 2);
			}
			else if (p_ptr->timed[TMD_ADJUST])
			{
				(void)inc_timed(TMD_ADJUST, randint(1 + plev/4) + 4);
			}
			else (void)inc_timed(TMD_ADJUST, randint(2 + plev/2) + plev);
			break;
		}

		case PRAYER_RECHARGING:
		{
			return recharge(14 + randint((plev * 3) / 4));
		}

#if old
		/* combined with bless object */
		case PRAYER_DISPEL_CURSE:
		{
			(void)remove_all_curse();
			break;
		}
#endif

		/* bless object (not just weapons) */
        case PRAYER_BLESS_WEAPON:
		{
			/* power = plev/4 to plev (round up so it can get to 50) */
			int power = (plev+3)/4 + randint(((plev * 3) + 2) / 4);
            return bless_weapon(power);
		}

#if old
		case PRAYER_ENCHANT_ARMOUR: /* removed */
		{
			return enchant_spell(0, 0, rand_int(3) + 2, FALSE, FALSE);
            break;
		}
#endif

		case PRAYER_DAYLIGHT:
		{
			if (p_ptr->depth) (void)inc_timed(TMD_DAYLIGHT, 15 + plev/4 + randint(10 + plev/2));
			else /* in town */
			{
				/* night or day */
				bool night;
			    if ((turn % (10L * TOWN_DAWN)) < ((10L * TOWN_DAWN) / 2)) night = FALSE;
				else night = TRUE;
				
				/* Bring the dawn early */
				if (night)
				{
					(void)inc_timed(TMD_DAYLIGHT, 15 + plev/4 + randint(10 + plev/2));
					town_illuminate(TRUE);
				}
				else 
				{
					msg_print("No need for magic, you're already in daylight.");
					return (FALSE);
				}
			}
            break;
		}
        
   		case PRAYER_BLINK:
		{
			bool controlled = FALSE;
            /* controlled teleport (random if you don't chose a target) */
		    if (p_ptr->telecontrol)
		    {
                if (control_tport(110 - plev, 12)) controlled = TRUE;
                if (!controlled) msg_print("You fail to control the teleportation.");
            }

            if (!controlled) teleport_player(10);
			break;
		}

		case PRAYER_TELEPORT_OTHER:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			dis = plev + 12 + goodluck + randint(plev * 2);
            if (cp_ptr->flags & CF_ZERO_FAIL) dis += 8;
			(void)teleport_monster(dir, dis);
			break;
		}

		case PRAYER_WORD_OF_RECALL:
		{
			set_recall(rand_int(20) + 15);
			break;
		}

		case PRAYER_ALTER_REALITY:
		{
			msg_print("The world changes!");

			/* Leaving */
			p_ptr->leaving = TRUE;

			break;
		}

		case PRAYER_ANNIHILATION:
		{
            if (!get_aim_dir(&dir)) return (FALSE);
            spellswitch = 8; /* enhance GF_OLD_DRAIN: may do more than 200 damage */
			drain_life(dir, 200);
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

    int die, dir, dis, pwr;
	int toolow;

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
            if (p_ptr->timed[TMD_2ND_THOUGHT])
            {
               msg_print("Your first sight supresses detection.");
            }
            else
            {
			  (void)detect_monsters_animal();
            }
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
			bool controlled = FALSE;
            /* controlled teleport (random if you don't chose a target) */
		    if (p_ptr->telecontrol)
		    {
                if (control_tport(110 - plev, 12)) controlled = TRUE;
                if (!controlled) msg_print("You fail to control the teleportation.");
            }

            if (!controlled) teleport_player(10);
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
			fire_beam(GF_ELEC, dir, damroll(2+((plev-5)/5), 4));
			break;
		}

		case NEWM_SPEAR_OF_LIGHT:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			msg_print("A ray of white light appears.");
			lite_line(dir, 12-(plev/2));
			break;
		}
		
		case NEWM_TRAP_DOOR_DESTRUCTION:
		{
			(void)destroy_doors_touch(0);
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
            int pwr = plev + adj_chr_charm[p_ptr->stat_ind[A_CHR]];
            if (!get_aim_dir(&dir)) return (FALSE);
			(void)sleep_monster(dir, pwr);
			break;
		}
		
		case NEWM_SCARE_MONSTER:
		{
            int pwr = plev + adj_chr_charm[p_ptr->stat_ind[A_CHR]];
			if (!get_aim_dir(&dir)) return (FALSE);
			if (randint(pwr + goodluck) > 50)
			{
				(void)fire_ball(GF_TURN_ALL, dir, pwr, 2);
			}
			else
			{
				(void)fear_monster(dir, pwr);
			}
			break;
		}
		
		case NEWM_FIND_TRAPS_DOORS:
		{
            if (p_ptr->timed[TMD_2ND_THOUGHT])
            {
               msg_print("Your first sight supresses detection.");
            }
            else
            {
			    (void)detect_traps();
			    (void)detect_doorstairs(FALSE);
            }
			break;
		}
		
		case NEWM_STUN_MONSTER:
        {
			if (!get_aim_dir(&dir)) return (FALSE);
			spellswitch = 9; /* keeps you from discovering water immunity */
			/* plev is not actual damage done, see GF_WATER in spells1.c */
			fire_bolt(GF_WATER, dir, plev);
			break;
			/* spellswitch = 9 must be after targetting or it will change */
			/* the target prompt to one for the camera flash spell */
        }
        
        case NEWM_BUG_SPRAY:
        {
			int dmg;
			if (plev >= 9) dmg = plev/3 + randint(plev / 2 + 1);
			else dmg = 3 + randint(4);
			fire_spread(GF_BUG_SPRAY, dmg, 7 + plev/6);
			break;
        }

		case NEWM_FIRE_BOLT:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_bolt_or_beam(beam, GF_FIRE, dir,
			                  damroll(6+((plev-5)/4), 8));
			break;
		}

		case NEWM_FIRE_SPRAY:
		{
			int rad = 5;
			if (plev >= 12) rad += 1;
			if (plev >= 35) rad += 1;
			if (rand_int(100) < (goodluck+1)*3) rad += 1;
			fire_spread(GF_FIRE, 5 + (plev+3)/4 + randint(plev/2), 6);
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
            if (p_ptr->timed[TMD_2ND_THOUGHT])
            {
               msg_print("Your first sight supresses detection.");
            }
            else
            {
			  (void)detect_monsters_life();
            }
			break;
		}
		
		case NEWM_NATURAL_VITALITY:
        {
			(void)set_timed(TMD_POISONED, (3 * p_ptr->timed[TMD_POISONED] / 4) - 5);
			(void)hp_player(9 + (damroll(plev / 3, 4)));
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
			earthquake(py, px, 10, 0, 0, TRUE);
			break;
		}

		case NEWM_IDENTIFY:
		{
			return ident_spell();
		}

		case NEWM_TELEPORT_OTHER:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			dis = plev + 12 + goodluck + randint(plev * 2);
            if (cp_ptr->flags & CF_ZERO_FAIL) dis += 8;
			(void)teleport_monster(dir, dis);
			break;
		}
		
		case NEWM_RESIST_SILVER:
		{
			if (plev < 40) (void)inc_timed(TMD_OPP_SILV, randint(15 + plev) + plev);
			else (void)inc_timed(TMD_OPP_SILV, randint((plev * 3)/2) + 39);
			break;
		}

		case NEWM_PORTAL:
		{
			bool controlled = FALSE;
			int dist = plev * 3;
			if (dist < 45) dist = 45;
            /* controlled teleport (random if you don't chose a target) */
		    if (p_ptr->telecontrol)
		    {
                if (control_tport(60 - plev, dist)) controlled = TRUE;
                if (!controlled) msg_print("You fail to control the teleportation.");
            }

            if (!controlled) teleport_player(plev * 3);
			break;
		}
		
		case NEWM_SONG_SCARE:
		{
            int pwr = plev + 2 + adj_chr_charm[p_ptr->stat_ind[A_CHR]];
		    scare_monsters(pwr);
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
            if (p_ptr->timed[TMD_2ND_THOUGHT])
            {
               msg_print("Your first sight supresses detection.");
            }
            else
            {
			  (void)inc_timed(TMD_SINVIS, randint(24) + 24);
            }
			break;
		}

		case NEWM_SENSE_SURROUNDINGS:
		{
            if (p_ptr->timed[TMD_2ND_THOUGHT])
            {
               msg_print("Your first sight supresses detection.");
            }
            else
            {
			  map_area(FALSE);
            }
			break;
		}
		
        case NEWM_TRUE_SIGHT:
        {
			(void)clear_timed(TMD_BLIND);
			(void)clear_timed(TMD_IMAGE);
			(void)clear_timed(TMD_2ND_THOUGHT);
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

		case NEWM_MIGHTY_THROW:
		{
			int time;
			int endurance = (adj_con_fix[p_ptr->stat_ind[A_CON]]);
			if (p_ptr->prace == 16) endurance = 10; /* golem race */
			if (endurance < 1)
			{
				msg_print("Your constitution is too low to cast this spell.");
				break;
			}
			if ((cp_ptr->flags & CF_HEAVY_BONUS) || (p_ptr->prace == 17)) endurance += 2;
			time = endurance*2 + randint((plev / 2) + endurance);
			(void)inc_timed(TMD_MIGHTY_HURL, time);

			/* try to pull free if being held */
			if ((p_ptr->timed[TMD_BEAR_HOLD]) &&
				(15 + adj_str_wgt[p_ptr->stat_ind[A_STR]] + goodluck/2 > randint(100)))
			{
				p_ptr->held_m_idx = 0;
				msg_print("You pull free.");
				clear_timed(TMD_BEAR_HOLD);
			}
			break;
		}
		
		case NEWM_WITHER_FOE:
        {
            pwr = plev + adj_chr_charm[p_ptr->stat_ind[A_CHR]];

			if (!get_aim_dir(&dir)) return (FALSE);
			toolow = plev / 7;
			if (toolow < 1) toolow = 1;
			if ((toolow < 2) && (plev > 11)) toolow = 2;
			die = damroll(toolow, 8);
			fire_bolt(GF_MANA, dir, die);
			(void)confuse_monster(dir, pwr + 10);
			if (die > 7) (void)slow_monster(dir, pwr);
			break;
        }

		case NEWM_ELEC_STORM:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_ball(GF_ELEC, dir, 30 + (plev/2) + randint(plev * 2), (plev / 14));
			break;
           /* averages: 60 at L20, 75 at L30, 90 at L40, 105 at L50 */
		}

		/* damage reduced (it's a stunning spell, it should be low damage) */
		case NEWM_THUNDERCLAP:
		{
			int rad = 10;
			if (plev >= 32) rad += (plev-26)/6;
			/* paranoia */
			if (plev < 3) plev = 3;
			fire_spread(GF_SOUND, (plev/2)+2 + randint(plev-2), rad);
			break;
           /* average damage ~= plev+1 */
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
            pwr = plev + adj_chr_charm[p_ptr->stat_ind[A_CHR]];
			(void)slow_monsters(pwr);
			(void)sleep_monsters(pwr);
			break;
		}

		case NEWM_SONG_HEROISM:
		{
			(void)hp_player(10);
			(void)clear_timed(TMD_AFRAID);
            if (p_ptr->peace)
        	{
                msg_print("The peaceful magic prevents you from becoming heroic.");
                break;
            }
			(void)inc_timed(TMD_HERO, randint(25) + 25);
			break;
		}

		case NEWM_SPHERE_CHARM:
		{
			int time = adj_chr_charm[p_ptr->stat_ind[A_CHR]] * 2;
			int timeb = (plev * 3) / 2;
			if (time + timeb < 74) (void)inc_timed(TMD_SPHERE_CHARM, randint(37) + 37);
			else if (time + timeb > 150) (void)inc_timed(TMD_SPHERE_CHARM, randint(77) + 73);
            (void)inc_timed(TMD_SPHERE_CHARM, randint(time) + timeb);
			break;
		}

        case NEWM_CALL_HELP:
        {
            int die;
            if (plev > 30) die = randint(100);
            else if (plev > 20)  die = randint(90);
            else die = randint(80);
            if ((goodluck > 2) && (randint(103-goodluck) < 9)) die += 11;
            if (die < 8) do_call_help(994);
            else if (die < 39) do_call_help(995);
            else if (die < 70) do_call_help(996);
            else do_call_help(997);
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
			return (warding_glyph());
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
			if (p_ptr->timed[TMD_SUST_SPEED])
			{
				msg_print("You cannot be hasted while your speed is sustained!");
				break;
			}
			
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
			return recharge(plev + 1);
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
			(void)teleport_player_level(0);
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
			return brand_ammo(0);
		}
		
		case NEWM_TELEPATHY:
        {
			(void)inc_timed(TMD_ESP, randint(plev) + (plev/2));
			break;
        }
        /* HERE4: still need last spellbook which isn't in the game yet */
    }

	/* Success */
	return (TRUE);
}

static bool cast_luck_spell(int spell)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

    int die, dir, dis, pwr;
	int flash, lottery, jackpot;
	int where, aquirenum, price;
	/* int ty, tx, time; */
    int way, dieb;
	bool controlled;

	/* Hack -- chance of "beam" instead of "bolt" */
	int beam = beam_chance();

	int plev = p_ptr->lev + (goodluck/4) - (badluck/4);

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
            if ((p_ptr->timed[TMD_2ND_THOUGHT]) && (goodluck < 14))
            {
               msg_print("Your first sight supresses detection.");
            }
            else
            {
			  (void)detect_doorstairs(FALSE);
            }
			break;
		}
		
		case LUCK_DETECT_TREASURE:
        {
            if ((p_ptr->timed[TMD_2ND_THOUGHT]) && (goodluck < 14))
            {
               msg_print("Your first sight supresses detection.");
            }
            else
            {
               die = randint(99) + randint(plev/5);
               if (die > 105) (void)detect_objects_magic();
			   else if (die > 75) (void)detect_objects_normal(TRUE);
               else if (die > 10) (void)detect_objects_normal(FALSE);
               else if (die > 5) msg_print("You detect treasure but forget the objects in your greed.");
               else if (die < 6) msg_print("You detect that there are objects on the level.");

			   if (die > 5) (void)detect_treasure();
            }
			break;
		}

		case LUCK_TRAP_DOOR_DESTRUCTION:
		{
            die = randint(99) + randint(plev/5);
			(void)destroy_doors_touch(0);
			if (die > 100)
            {
               (void)destroy_doors_touch(1);
            }
			break;
		}
		
		case LUCK_CAMERA_FLASH:
		{
            msg_print("What do you want to take a picture of?");
			if (!get_aim_dir(&dir)) return (FALSE);
			flash = 1 + randint(plev/4 + 1);
			if (plev < 9) flash = randint(plev/4 + 1);
            spellswitch = 9;
            range = 10;
			fire_ball(GF_WATER, dir, 1, plev/22);
            spellswitch = 9;
            range = 10;
			fire_ball(GF_LITE, dir, flash-1, plev/15);
			/* GF_WATER for stunning effect */
			/* spellswitch 9 keeps you from discovering water immunity */
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
            else if (die < 10) earthquake(py, px, 5, 0, 0, FALSE);
            else if (die < 15) (void)destroy_doors_touch(0);
            else if (die < 26) 
            {
                 earthquake(py, px, 3, 80, 0, FALSE);
            }
            else if (die < 41)
            {
                 earthquake(py, px, 3, 0, 0, FALSE);
			     (void)door_creation(0);
            }
            else if (die < 100) (void)door_creation(0);
            else if (die < 110)
            {
                 (void)destroy_doors_touch(0);
			     (void)door_creation(10);
            }
            else  /* (die > 109) */
            {
			     (void)hp_player(damroll(5, 5));
                 (void)destroy_doors_touch(2);
			     (void)door_creation(10);
            }
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
            else if (die < 13) (void)teleport_player_level(0);
            else if (die < 18) (void)destroy_doors_touch(0);
            else if (die < 28) 
            {
                 earthquake(py, px, 3, 0, 0, FALSE);
            }
            else if (die < 48)
            {
			     (void)stair_creation(5 + randint(20));
			     if (die + goodluck/2 > 38) (void)detect_doorstairs(TRUE);
            }
            else if (die < 100) (void)stair_creation(0);
            else if (die < 110)
            {
			     (void)stair_creation(0);
                 return (project_los(GF_GRAVITY, plev/6));
            }
            else  /* (die > 109) */
            {
			     (void)hp_player(damroll(7, 7));
			     (void)stair_creation(0);
                 return (project_los(GF_GRAVITY, plev/6));
            }
			break;
		}

		case LUCK_WORD_OF_RECALL:
		{
            die = randint(99) + randint(plev/4);
            if (die < 90) set_recall(rand_int(20) + 15);
            else if (die < 96)
            {
                 (void)door_creation(0);
                 set_recall(rand_int(20) + 15);
            }
            else if (die < 106)
            {
                 set_recall(rand_int(5) + 4);
            }
            else
            {
                 (void)door_creation(0);
                 spellswitch = 8; /* quick recall */
                 set_recall(rand_int(5) + 4);
            }
            spellswitch = 0;
			break;
		}

		case LUCK_CURE_LIGHT_WOUNDS: /* min 11% */
		{
			int cure = damroll(2, 2 + (p_ptr->luck/4)) + randint(plev / 4);
			int curep = (p_ptr->mhp * 11) / 100;
			if (cure < curep) cure = curep;
			if ((cure > 44) && (randint(80) > goodluck*2)) cure = 44; /* maximum */
			(void)hp_player(cure);
			(void)dec_timed(TMD_CUT, 10 + (goodluck/3));
			break;
		}
		
		case LUCK_EXTRA_LUCK:
        {
            int dep = p_ptr->depth;
            die = randint(99) + plev/5 + goodluck - (badluck/2);
            if (!dep) dep = 1;
            if (die < 3)
            {
               msg_print("You feel quite unlucky."); 
               summon_specific(py, px, dep + (badluck/2), SUMMON_UNDEAD);
            }
            else if (die < 5)
            {
               msg_print("You feel rather unlucky."); 
               summon_specific(py, px, dep + (badluck/2), 0);
            }
            else if ((die < 8) && (!p_ptr->peace)) aggravate_monsters(0);
            else if (die < 11)
            {
                 msg_print("You feel lucky, but the feeling passes quickly."); 
                 if (randint(100) < 40 + goodluck - badluck) (void)hp_player(randint(die*2)+2);
                 else (void)hp_player(randint(die));
            }
            else if (die < 15) (void)inc_timed(TMD_FRENZY, randint(die) + (die - 2));
            else if (die < 30) (void)inc_timed(TMD_BLESSED, randint(die) + (die - 4));
            else if (die < 45)
            {
                 (void)hp_player(30);
                 (void)clear_timed(TMD_AFRAID);
			     (void)clear_timed(TMD_CHARM);
			     if (!p_ptr->peace) (void)inc_timed(TMD_SHERO, randint(die) + (20));
            }
            else if (die < 60)
            {
                 (void)hp_player(20);
                 (void)clear_timed(TMD_AFRAID);
			     (void)clear_timed(TMD_CHARM);
			     if (!p_ptr->peace) (void)inc_timed(TMD_HERO, randint(25) + (25));
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
			     if (!p_ptr->peace) (void)inc_timed(TMD_SHERO, randint(die / 3) + 20);
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
			     (void)inc_timed(TMD_SINFRA, randint(die / 3) + (25));
			     if (!p_ptr->peace) (void)inc_timed(TMD_HERO, randint(die / 3) + (25));
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
                     cold_dam(1 + randint(plev/3), "a random cold breeze", plev-7);
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
                brand_weapon(1);
                (void)inc_timed(TMD_OPP_COLD, randint(20) + (11));
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
			int dmg;
			if (plev >= 9) dmg = plev/3 + randint(plev / 2 + 1);
			else dmg = 3 + randint(4);

			die = randint(92) + randint(plev/5) + goodluck - badluck/2;
            if ((die < 6) && (plev >= 8)) dmg = plev/4 + randint(plev / 3 + 1);
			else if (die < 6) dmg = 2 + randint(3);
			if ((die > 91) && (plev >= 8)) dmg = plev/2 + randint(plev / 2);
			else if (die > 91) dmg = 4 + randint(4);
			fire_spread(GF_BUG_SPRAY, dmg, 7 + plev/6);
			break;
        }

		case LUCK_DETECT_INVISIBLE:
		{
            die = randint(91) + plev/5 + goodluck - badluck;
            if ((p_ptr->timed[TMD_2ND_THOUGHT]) && (goodluck < 14) && (die < 106))
            {
               msg_print("Your first sight supresses detection.");
               break;
            }
			if (die < 10) (void)inc_timed(TMD_SINVIS, randint(4) + 2);
			else if (die < 60) (void)detect_monsters_invis();
			else if (die < 80) (void)inc_timed(TMD_SINVIS, randint(24) + 24);
			else if (die < 100)
			{
                 (void)detect_monsters_invis();
                 (void)inc_timed(TMD_SINVIS, randint(21) + 19);
            }
            else if (die < 105)
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
            die = randint(91) + randint(plev/5) + goodluck - badluck;
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
            die = randint(92) + randint(plev/4) + goodluck - badluck;
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
                 (void)set_timed(TMD_IMAGE, p_ptr->timed[TMD_IMAGE] / 2);
				 (void)clear_timed(TMD_CONFUSED);
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
			bool controlled = FALSE;
			int dist = 9 + randint(plev / 6);
            /* controlled teleport (random if you don't chose a target) */
		    if (p_ptr->telecontrol)
		    {
                if (control_tport(115 - (plev + goodluck), dist + 1)) controlled = TRUE;
                if (!controlled) msg_print("You fail to control the teleportation.");
            }

            if (!controlled) teleport_player(10);
			teleport_player(dist);
			break;
		}

		case LUCK_CONFUSE_MONSTER:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			(void)confuse_monster(dir, plev + adj_chr_charm[p_ptr->stat_ind[A_CHR]]);
			break;
		}

		case LUCK_DETECT_MONSTERS:
		{
            if ((p_ptr->timed[TMD_2ND_THOUGHT]) && (goodluck < 14))
            {
               msg_print("Your first sight supresses detection.");
            }
            else if ((!p_ptr->timed[TMD_2ND_THOUGHT]) && (goodluck > 11))
            {
			  (void)detect_monsters_normal(2);
            }
            else (void)detect_monsters_normal(1);
			break;
		}

		case LUCK_DETECT_TRAPS_OBJ: /* just detect traps */
		{
            if ((p_ptr->timed[TMD_2ND_THOUGHT]) && (goodluck < 14))
            {
               msg_print("Your first sight supresses detection.");
            }
            else
            {
			  (void)detect_traps();
            }
			break;
        }

		case LUCK_SLOW_MONSTER:
		{
            int pwr = plev + (goodluck/4) + adj_chr_charm[p_ptr->stat_ind[A_CHR]];
			if (!get_aim_dir(&dir)) return (FALSE);
			(void)slow_monster(dir, pwr);
			break;
		}

		case LUCK_MINI_DESTRUCTION:
		{
			earthquake(py, px, 5, 45, 0, TRUE);
			break;
		}
		
        case LUCK_LULLABY:
		{
            pwr = plev + adj_chr_charm[p_ptr->stat_ind[A_CHR]];
            fire_spread(GF_OLD_SLOW, pwr-2, 6 + plev/12);
            fire_spread(GF_OLD_SLEEP, pwr, 5 + plev/12);
			break;
		}
		
		case LUCK_TELEKINESIS2:
		{
			/* max distance for telekinesis */
            int dist = 9;
			if (goodluck)
            {
                dist += rand_int(goodluck+1);
                /* maybe boost max distance again */
                if (rand_int(100) < goodluck*2) dist += randint((goodluck+1)/2) * 2;
                if (dist < 8 + (goodluck+1)/3) dist = 8 + (goodluck+1)/3;
            }
            if (badluck >= 10) dist -= randint(badluck/4);
            do_telekinesis(dist);
			break;
		}

		case LUCK_SENSE_SURROUNDINGS:
		{
            if ((p_ptr->timed[TMD_2ND_THOUGHT]) && (goodluck < 12))
            {
               msg_print("Your first sight supresses detection.");
            }
            else
            {
			   map_area(FALSE);
            }
			break;
		}

		case LUCK_DETECT_ENCHANTMENT:
		{
            if ((p_ptr->timed[TMD_2ND_THOUGHT]) && (goodluck < 14))
            {
               msg_print("Your first sight supresses detection.");
            }
            else
            {
			  (void)detect_objects_magic();
            }
			break;
		}

		case LUCK_DETECT_TRAPS:
		{
            die = randint(99) + randint(plev/4);
            if ((p_ptr->timed[TMD_2ND_THOUGHT]) && (goodluck < 14))
            {
               msg_print("Your first sight supresses detection.");
            }
            else
            {
			  (void)detect_traps();
            }
			if (die > 99) (void)destroy_doors_touch(0);
			else if (die > 108)
            {
               (void)destroy_doors_touch(1);
            }
			break;
        }

		case LUCK_DISINFECTANT:
		{
            spellswitch = 16; /* damage slime in GF_bug_spray */
			(void)dec_timed(TMD_POISONED, plev/4 + randint((plev+1)/2));
			(void)dec_timed(TMD_CUT, plev/3 + randint((plev+2)/3));
			(void)dec_timed(TMD_STUN, plev/5 + randint((plev+2)/3));
			fire_spread(GF_BUG_SPRAY, 2 + ((plev+3)/6) + randint((plev+2)/3 + goodluck), 6 + plev/6);
            if ((randint(100) < 25+(plev/2)) && (p_ptr->slime > PY_SLIME_HEALTHY)) p_ptr->slime -= 1;
            if ((randint(100) < 10) && (plev > 14))
            {
			    spellswitch = 16; /* damage slime in GF_bug_spray */
			   (void)dec_timed(TMD_POISONED, randint((plev+2)/3));
			   (void)dec_timed(TMD_CUT, randint((plev+4)/5));
			   (void)dec_timed(TMD_STUN, randint((plev+5)/6));
               if ((randint(100) < (plev * 3)) && (p_ptr->slime > PY_SLIME_HEALTHY)) p_ptr->slime -= 1;
               if ((randint(100) < 2 + (plev/2)) && (p_ptr->silver > PY_SILVER_HEALTHY)) p_ptr->silver -= 1;
			   fire_spread(GF_BUG_SPRAY, 1 + randint((plev+2)/ 3 + goodluck), 6 + plev/6);
			   (void)inc_timed(TMD_WOPP_POIS, randint(plev/2 + 4) + 11);
            }
			break;
        }

		case LUCK_MAP_AREA:
		{
            if ((p_ptr->timed[TMD_2ND_THOUGHT]) && (goodluck < 12))
            {
               msg_print("Your first sight supresses detection.");
               break;
            }
            die = randint(99) + randint(plev/5);
            if (die < 8)
            {
               spellswitch = 4; /* does not light room */
               (void)lite_area(damroll(2, (plev / 6)), (plev / 5));
               spellswitch = 0;
            }
            else if (die < 90) map_area(FALSE);
            else if (die < 100)
            {
               (void)lite_area(damroll(2, (plev / 4)), (plev / 5));
			   if (die == 94) map_area(TRUE);
			   else map_area(FALSE);
            }
            else
            {
               (void)lite_area(damroll(2, (plev / 3)), (plev / 5));
			   map_area(TRUE);
            }
			break;
		}
		
		case LUCK_ADJUST_CURSE: /* add chance to make the weapon blessed */
		{
            (void)spell_adjust_curse();
			break; /* (direction is unused) */
        }
        
        case LUCK_DETECT_ENCHANTMENT2:
		{
            if ((p_ptr->timed[TMD_2ND_THOUGHT]) && (goodluck < 14))
            {
               msg_print("Your first sight supresses detection.");
               break;
            }
            die = randint(99) + randint(plev/5);
            if (die > 15) (void)detect_objects_magic();
            else if (die > 6)
            {
                  msg_print("The spell doesn't quite come off like it should.");
                  (void)detect_objects_normal(FALSE);
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
			int mapstr = randint((plev - 2)*2 + goodluck);
            if ((p_ptr->timed[TMD_2ND_THOUGHT]) && (goodluck < 12))
            {
               msg_print("Your first sight supresses detection.");
               break;
            }
            if (mapstr > 100) wiz_lite(TRUE);
			else if (mapstr > 94) wiz_lite(FALSE);
			else
			{
                if (randint(plev * 2 + goodluck - badluck) > 4) 
                    map_area(TRUE);
                else
                {
                    msg_print("The map seems to be incomplete..");
			        map_area(FALSE);
			    }
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
			controlled = FALSE;
            /* controlled teleport (random if you don't chose a target) */
		    if (p_ptr->telecontrol)
		    {
                if (control_tport(150 - (plev + goodluck), 12)) controlled = TRUE;
                if (!controlled) msg_print("You fail to control the teleportation.");
            }

            if (!controlled) teleport_player(10);
			break;
		}

		case LUCK_TELEPORT_SELF:
		{
			bool controlled = FALSE;
			int dist = plev * 5;
			if (dist < 80) dist = 80;
            /* controlled teleport (random if you don't chose a target) */
		    if (p_ptr->telecontrol)
		    {
                if (control_tport(65 - (plev + goodluck), dist)) controlled = TRUE;
                if (!controlled) msg_print("You fail to control the teleportation.");
            }

            if (!controlled) teleport_player(plev * 5);
			break;
		}

		case LUCK_TELEPORT_OTHER:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			dis = plev + 12 + goodluck + randint(plev * 2);
            if (cp_ptr->flags & CF_ZERO_FAIL) dis += 8;
			(void)teleport_monster(dir, dis);
			break;
		}
		
		case LUCK_SLOW_MONSTERS:
		{
            int pwr = plev + (goodluck/4) + adj_chr_charm[p_ptr->stat_ind[A_CHR]];
			(void)slow_monsters(pwr);
			break;
		}

        case LUCK_EXTRA_BLOW:
		{
			(void)inc_timed(TMD_XATTACK, randint(plev-6) + (plev/2));
			break;
		}

		case LUCK_MASS_AMNESIA: /* was plev*2 */
		{
            int pwr = ((plev * 7)/4) + (adj_chr_charm[p_ptr->stat_ind[A_CHR]] / 2);
			(void)mass_amnesia(pwr, 0, 0);
			break;
		}
		
		case LUCK_SLIP_INTO_SHADOWS:
		{
             if (p_ptr->aggravate)
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
			if ((p_ptr->timed[TMD_SUST_SPEED]) && (goodluck < 9))
			{
				msg_print("You cannot be hasted while your speed is sustained!");
				break;
			}
			
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

		case LUCK_LUCK_BOLT:
		{
			int fire, dmg;
			/* get absolute luck value */
			int aluck = 20 + goodluck - badluck;
			if (aluck < 3)
			{
				msg_print("The spell fails. You feel pathetic.");
				break;
			}
			/* projectile type */
			die = randint((aluck*5)/2 + 100);  /* max range 1 to 200 */
			if (die < 25) fire = 1;			/* bolt */
			else if (die < 100) fire = 2;	/* bolt or beam */
			else if (die < 125) fire = 3;	/* beam */
			else if (die < 150) fire = 4;	/* ball (radius 2) */
			else if (die < 167) fire = 5;	/* ball (radius 3) */
			else if (die < 184) fire = 6;	/* ball (radius 4) */
			else fire = 7;					/* los  */
			die = randint(150 + ((aluck*5)/4));  /* max range 1 to 200 */
			if (die < 10) dmg = randint((aluck/4) + 1); /* 1 to 1-6 to 1-11 */
			else if (die < 25) dmg = randint((aluck/2) + 5); /* 1-6 to 1-15 to 1-25 */
			else if (die < 50) dmg = damroll(2, (aluck+1)/4); /* 2 to 2d5 to 2d10 */
			else if (die < 75) dmg = randint(aluck + 10); /* 1-13 to 1-30 to 1-50 */
			else if (die < 100) dmg = damroll((aluck/10)+1, (aluck+1)/2); /* 1d2 to 3d10 to 5d20 */
			else if (die < 125) dmg = randint(aluck*2 + 20); /* 1-26 to 1-60 to 1-100 */
			else if (die < 150) dmg = damroll(((aluck+1)/4) + 1, ((aluck+1)/4) + 2); /* 2d3 to 6d7 to 11d12 */
			else if (die < 175) dmg = randint(aluck*2 + 20) + 10 + aluck/2; /* 12-37 to 21-80 to 31-130 */
			else dmg = damroll(((aluck+2)/5)+1, aluck); /* 2d3 to 5d20 to 9d40 */
			die = rand_int(150 + ((aluck*5)/4));  /* max range 0 to 199 */
			if (die < 16) dmg = (dmg+1)/2;
			else if (die < 33) dmg = ((dmg * 2) + 1) / 3;
			else if (die < 50) dmg = (dmg * 3) / 4;
			else if (die == 200) dmg += ((dmg+5)/6) * (73-(dmg/5)); /* can add 1000+ */
			else if (die > 198) dmg = dmg * 3;
			else if (die > 193) dmg = dmg * 2;
			else if (die > 185) dmg = (dmg * 3) / 2;
			else if ((die > 130) && (die < 150)) dmg = (dmg*5)/4;
			/* scale down insane damage */
			if (dmg > 1000) dmg = 999 + ((dmg-999) / 2);
			else if (dmg > 500) dmg = 498 + (((dmg-498)*2)/3);
			else if (dmg > 212) dmg = 208 + (((dmg-208)*3)/4);
			/* extra scale down for the most powerful project type */
			if ((fire == 7) && (dmg > 100)) dmg = 98 + (((dmg-98)*2)/3);
			
			if (fire == 7) project_los(GF_MANA, dmg);
			else
			{
				if (!get_aim_dir(&dir)) return (FALSE);
				if ((fire > 4) && (dmg < 50)) dmg += (51 - dmg)/2;
				if (fire == 1) fire_bolt(GF_MANA, dir, dmg);
				else if (fire == 2) fire_bolt_or_beam(aluck, GF_MANA, dir, dmg);
				else if (fire == 3) fire_beam(GF_MANA, dir, dmg);
				/* GF_MANA destroys objects so don't use it */
				else if (fire == 4) fire_ball(GF_THROW, dir, dmg, 2);
				else if (fire == 5) fire_ball(GF_THROW, dir, dmg, 3);
				else if (fire == 6) fire_ball(GF_THROW, dir, dmg, 4);
			}
			/* give some indication of how much damage was done */
			if (dmg < 12) msg_print("Ug, that spell seemed to be a dud.");
			else if ((dmg < 50) && (dmg < plev * 2)) msg_print("That was nothing special.");
			else if (dmg < 100) msg_print("Nice hit.");
			else if (dmg < 150) msg_print("That must have hurt!");
			else if (dmg < 200) msg_print("Wow, you really nailed 'em with that one.");
			else if (dmg < 500) msg_print("Holy %$*%@&, that was a great hit!");
			else
			{
				msg_print("You can't believe it did that much damage!");
				(void)inc_timed(TMD_STUN, randint(2 + (badluck/2)));
			}
			break;
		}

		case LUCK_POLYMORPH_OTHER:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			(void)poly_monster(dir, plev + (goodluck/3));
			break;
		}

        case LUCK_ADJUST_SPEED:
		{
			int adjust = randint(20) - 9;
            if (p_ptr->timed[TMD_SUST_SPEED])
			{
				msg_print("Your speed can't be adjusted while it is sustained!");
				break;
			}
            if (randint(100) < plev) adjust += 2;
            if (randint(501) < plev) adjust += randint(3);
            if ((adjust == 0) && (randint(45) < plev)) adjust = 6 + randint(3);
            else if (adjust == 0) adjust = randint(3) - 5;
            p_ptr->spadjust = adjust;
            if (adjust > 0) (void)inc_timed(TMD_ADJUST, randint(25) + plev);
            if (adjust < 0) (void)inc_timed(TMD_ADJUST, (plev + 25) - randint(plev));
			break;
		}

		case LUCK_DOOR_CREATION2: /* seal/open getaway */
		/* maybe: if rogue and clvl < X then it makes rubble instead */
		{
			int jam = 0;
			/* (void)door_creation(0); */
			if (get_check("Create a jammed door? (otherwise will be unlocked) "))
				jam = 10;
			spellswitch = 24; /* force specific target */
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_ball(GF_MAKE_DOOR, dir, jam, 1);
			break;
		}

		case LUCK_STAIR_CREATION2:
		{
			(void)stair_creation(0);
			break;
		}

		case LUCK_TELEPORT_LEVEL:
		{
			(void)teleport_player_level(0);
			break;
		}

		case LUCK_WORD_OF_DESTRUCTION:
		{
			destroy_area(py, px, 15, TRUE);
			break;
		}

		case LUCK_TELEPORT_CONTROL:
		{
			(void)inc_timed(TMD_TELECONTROL, randint(plev) + plev);
			break;
		}

		case LUCK_RUNE_OF_PROTECTION: /* rune of protection */
		{
			return (warding_glyph());
		}
        
        case LUCK_BLINK_MONSTER:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			dis = 8 + randint((plev / 5) + 2);
			die = plev/2 + (goodluck*2) + randint(plev/2 + 20);
            if (die > 39) dis += 3 + randint(3);
            else if (die > 24) dis += 1 + randint(3);
			/* short range beam */
			if (die < 55) range = 6;
			(void)teleport_monster(dir, dis);
			spellswitch = 0;
			break;
		}
		
		case LUCK_BANISH_SUMMON:
        {
            int dep = p_ptr->depth;
            die = randint(99 - badluck) + goodluck;
            if (randint(100) > badluck + 1) die += plev/3;
            if (!dep) dep = 1;
            if (die < 5)
            {
               msg_print("The spell gets a little out of hand..");
               summon_specific(py, px, p_ptr->depth + 10, 0);
            }
            else if (die < 10)
            {
               msg_print("You summon animals.");
               summon_specific(py, px, dep, SUMMON_ANIMAL);
            }
            else if (die < 15)
            {
               msg_print("You summon a dragon.");
               summon_specific(py, px, dep, SUMMON_DRAGON);
            }
            else if (die < 20)
            {
               msg_print("You summon an undead monster.");
               summon_specific(py, px, dep, SUMMON_UNDEAD);
            }
            else if (die < 25)
            {
               msg_print("You summon a demon.");
               summon_specific(py, px, dep, SUMMON_DEMON);
            }
            else if (die < 30)
            {
               msg_print("You summon something hairy.");
               summon_specific(py, px, dep, SUMMON_ANGEL);
            }
            else if (die < 35)
            {
               msg_print("You summon spiders.");
               summon_specific(py, px, dep, SUMMON_SPIDER);
            }
            else if (die < 50)
            {
               (void)summon_chosen();
            }
            else if (die < 55)
            {
               msg_print("With inexplicable authority, you tell the monsters to leave you alone.");
               (void)scare_monsters(999);
            }
            else if (die < 65) return banish_unnatural(85);
            else if (die < 75) return banish_evil(100);
            else if (die < 95) return banishment();
            else if (die < 105) return mass_banishment();
            else if (die < 108)
            {
                /* banish AND summon */ 
				(void)mass_banishment();
				(void)banishment();
				(void)summon_chosen();
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
            (void)spell_affect_self();
            die = randint(99) + randint(plev/5);
            if (die > 108) wiz_lite(TRUE);
            else if (die > 105) wiz_lite(FALSE);
            else if (die > 100) return identify_fully();
            else if (die > 95) map_area(FALSE);
            else if (die > 85) return ident_spell();
            else if (die > 80) restore_level();
            else if (die > 77) gain_exp(randint(plev * 3));
            else if (die > 70) p_ptr->luck = p_ptr->luck + randint(2);
            if (die < 8) p_ptr->luck = p_ptr->luck - randint(2);
            break;  
        }
        
        case LUCK_AFFECT_OTHER:
        {
			if (!get_aim_dir(&dir)) return (FALSE);
            (void)spell_affect_other(dir);
			break;
        }
        
        case LUCK_POTLUCK_STATS:
        {
            (void)spell_potluck_stats();
			break;
        }
        
        case LUCK_TREASURE_MAP:
        {
            (void)treasure_map();
			break;
        }
        
        case LUCK_AQUIREMENT:
        {
            die = randint(99) + randint(plev/5);
            if ((plev == 50) && (goodluck > 0)) die += 1;
            if (goodluck > 10) die += 1;
            lottery = randint(1000);
            jackpot = 0;
            if (die == 110) jackpot = randint(11);
            if (jackpot == 10) lottery = 999;
            if (jackpot == 9) lottery = 776;
            if (jackpot == 8) lottery = 888;
            if (jackpot == 7) lottery = 777;
            if (jackpot == 6) lottery = 666;
            if (jackpot == 5) lottery = 555;
            if (jackpot == 4) lottery = 444;
            if (jackpot == 3) lottery = 333;
            where = randint(100);
            if (die < 10)
            {
               msg_print("A broken bottle appears along with the guy who was drinking.");
               
               aquirenum = randint(2);
               if ((plev > 47) && (randint(100) < 50)) aquirenum += randint(3);
               if ((plev > 49) && (randint(100) < 90)) aquirenum += randint(4);
               if (randint(100) < 3) aquirenum = 1;
               if (aquirenum == 9) do_call_help(712); /* dark fairy fool */
               else if (aquirenum == 8) do_call_help(692); /* lesser titan */
               else if (aquirenum == 7) do_call_help(546); /* cyclops */
               else if (aquirenum == 6) do_call_help(554); /* extril */
               else if (aquirenum == 5) do_call_help(508); /* master assassin */
               else if (aquirenum == 4) do_call_help(459); /* assassin */
               else if (aquirenum == 3) do_call_help(354); /* lightning rogue */
               else if (aquirenum == 2) do_call_help(142); /* bandit */
               else do_call_help(55); /* scruffy looking hobbit */
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
               price = damroll(2, randint(10)) - (goodluck/2) + (badluck/2);
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
               price = damroll(2 + aquirenum, 2 + randint(8));
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
               price = damroll(6, 8 + randint(7)) - (goodluck) + (badluck);
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
               price = damroll(6, 5 + randint(5)) - (goodluck * 2) + (badluck);
               if (price < 1) price = 1;
               take_hit(price, "delivery price of aquirement");
               p_ptr->silver = p_ptr->silver + 3;
               (void)inc_timed(TMD_STUN, 3 * randint(5));
               summon_specific(py, px, plev + 1, 0);
            }
            else if (die < 105)  /* very high cost *aquirement* */
            {
               aquirenum = randint(2) + 1;
               if (where < 25) acquirement(py + randint(4), px + randint(4), aquirenum, TRUE);
               else if (where < 50) acquirement(py + randint(4), px - randint(4), aquirenum, TRUE);
               else if (where < 75) acquirement(py - randint(4), px + randint(4), aquirenum, TRUE);
               else acquirement(py - randint(4), px - randint(4), aquirenum, TRUE);
               price = damroll(6, 7 + aquirenum + randint(7));
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
               aquirenum = randint(2);
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

		case LUCK_BOOMERANG_THROW:
		{
			(void)inc_timed(TMD_THROW_RETURN, randint((plev*3)/4) + plev);
			break;
		}
        
        case LUCK_BURST_OF_LIGHT:
        {
            msg_print("You activate a burst of light to stun your enemies.");
            /* need loop because spellswitch resets at the end of project() */
		    for (way = 0; way < 10; way++)
		    {
            spellswitch = 9;lite_line(way, 15-(plev/2)); /* prevents area from staying lit */
            }
            spellswitch = 9;
			fire_spread(GF_WATER, 1, 9 + plev/10);
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

		case LUCK_SUPER_ROGUE:
		{
			int time;
			(void)clear_timed(TMD_FRENZY);
			if (goodluck > 0) time = 11 + (goodluck * 4) + randint(19);
			else time = 10 + randint(18);
			(void)inc_timed(TMD_SUPER_ROGUE, time);
			if (randint(50) == 1) p_ptr->luck += 1;
			break;
		}
        
        case LUCK_SUSTAIN_SPEED:
        {
			(void)do_res_stat(A_DEX);
			(void)inc_timed(TMD_SUST_SPEED, plev/2 + randint(plev/2));
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
            if (die <= 10) /* poison self */
            {
               msg_print("You smell something foul.");
			   if (!(p_ptr->resist_pois || p_ptr->timed[TMD_OPP_POIS]))
			   {
                  int dam;
                  if (die < 2) die = 2;
			      dam = ((109 - (die * 10)) / 3);
			      if (p_ptr->weakresist_pois) dam = dam / 2;
			      (void)inc_timed(TMD_POISONED, rand_int(dam) + 10);
			      take_hit(dam, "a toxic smell");
			   }
            }
            else if (die < 40) fire_ball(GF_POIS, dir, 11 + randint(plev), 2);
            else if (die < 60)
            {
               if (plev >= 27) fire_bolt_or_beam(beam-10, GF_POIS, dir,
                                 damroll(5 + (plev / 2), (plev/9)));
			   else fire_bolt_or_beam(beam-10, GF_POIS, dir,
                                 damroll(5 + (plev / 2), 3));
            }
			else if (die < 80) fire_ball(GF_POIS, dir, 15 + (plev) + randint(plev / 2), plev / 12);
			else if (die < 105) fire_ball(GF_POIS, dir, 40 + (plev / 2) + randint(plev / 2), 3);
			else /* (die > 100) */ fire_ball(GF_POIS, dir, 40 + (plev) + randint(plev), 3);
			break;
        }

		case LUCK_BERSERKER:
		{
			(void)hp_player(30);
            die = randint(75);
            if (p_ptr->peace)
        	{
                 msg_print("The peaceful magic prevents you from going into a rage.");
            }
	        else if ((p_ptr->timed[TMD_CHARM]) && (die > (plev + 25)))
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
		
		case LUCK_MASS_CHAOS: /* note intentional lack of "elseif" */
        {
            int pwr = plev + adj_chr_charm[p_ptr->stat_ind[A_CHR]];
            if (pwr < 60) pwr = 60;
            return (project_los(GF_OLD_CONF, pwr)); /* no damage with OLD_CONF */
            return (project_los(GF_CHAOS, (plev/2) + randint(plev)));
            if (randint(100) < 40) /* random type of stunning */
            {
               if (randint(100) < 50) return (project_los(GF_WATER, 5));
               else return (project_los(GF_SOUND, 4));
            }
            if (randint(100) < 30) return (project_los(GF_GRAVITY, randint(plev)));
            if (randint(100) < 35) slow_monsters(pwr);
            if (randint(100) < 30)
            {
               spellswitch = 13; /* blink monsters */
               (project_los(GF_AWAY_ALL, 2 + randint(15)));
            }
            if (randint(100) < 35) return (project_los(GF_BRFEAR, randint(pwr)));
			break;
        }
        
        case LUCK_BREATHE_CHAOS:
        {
			if (!get_aim_dir(&dir)) return (FALSE);
			if (plev > 43) die = 4;
			else die = 3;
            dieb = randint(62 + die);
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
			   (void)dec_stat(A_STR, randint(3), TRUE);
			   (void)dec_stat(A_INT, randint(3), TRUE);
			   (void)dec_stat(A_WIS, randint(3), TRUE);
			   (void)dec_stat(A_DEX, randint(3), TRUE);
			   (void)dec_stat(A_CON, randint(3), TRUE);
			   (void)dec_stat(A_CHR, randint(4), TRUE);
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
               if (die2 == 2) do_call_help(754); /* veriety quylthulg */
               else if (die2 == 3) do_call_help(812); /* clear dust bunnies */
               else if (die2 == 4) do_call_help(712); /* dark fairy fool */
               else if (die2 == 5) do_call_help(638); /* shadow minions */
               else if (die2 == 6) do_call_help(713); /* horned devil lord */
               else if (die2 == 7) do_call_help(710); /* dullahan */
               else if (die2 == 8) do_call_help(703); /* skull druj */
               else if (die2 == 9) do_call_help(688); /* barbazu */
               else if (die2 == 10) do_call_help(676); /* giant toadstool of iritation */
               else if (die2 == 11) do_call_help(684); /* gnawing bug */
               else if (die2 == 12) do_call_help(661); /* wemu vyrm */
               else if (die2 == 13) do_call_help(723); /* rayem */
               else if (die2 == 14) do_call_help(659); /* gorgon */
               else if (die2 == 15) do_call_help(644); /* greater basilisk */
               else if (die2 == 16) do_call_help(631); /* doppleganger */
               else if (die2 == 17) do_call_help(616); /* shadow raven */
               else if (die2 == 18) do_call_help(604); /* black unicorn */
               else if (die2 == 19) do_call_help(603); /* giant ameoba slime */
               else if (die2 == 20) do_call_help(588); /* giant static toadstool */
               else if (die2 == 21) do_call_help(598); /* trooping fairies */
               else if (die2 == 22) do_call_help(511); /* singing vyrms */
               else if (die2 == 23) do_call_help(509); /* hellhound */
               else if (die2 == 24) do_call_help(569); /* animated staff of summoning */
               /* higher than 24 can only happen with good luck */
               else if (die2 == 25) do_call_help(605); /* ancient zhelung */
               else if (die2 == 26) do_call_help(400); /* creeping adamantite coins */
               else if (die2 == 27) do_call_help(522); /* hairy quylthulg */
               else if (die2 == 28) do_call_help(495); /* mature green dragon */
               else if (die2 > 28) do_call_help(997); /* white unicorn helper */
               /* 1 can only happen with maximum bad luck */
               else /* 1 */ do_call_help(798); /* plague slime */
            }
            else if (die < 37) 
            {
               msg_print("You feel lucky.");
               p_ptr->luck += randint(2);
            }
            /* the following four effects have both good and bad possibilities */
            /* but the good ones happen more often */
            else if (die < 45) (void)spell_adjust_curse();
            else if (die < 55) (void)spell_affect_self();
            else if (die < 65) (void)spell_potluck_stats();
            else if (die < 85)
            {
			     if (!get_aim_dir(&dir)) return (FALSE);
                 (void)spell_affect_other(dir);
            }
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
                int pwr = plev + adj_chr_charm[p_ptr->stat_ind[A_CHR]];
                if (pwr < 65) pwr = 65;
                msg_print("You feel very lucky.");
                dispel_monsters((99 + randint(51)) * plev/10);
		        slow_monsters(pwr);
		        sleep_monsters(pwr);
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
    int die, dir, dis;
	int arrowdmg, manya, time;
	bool controlled;

	int py = p_ptr->py;
	int px = p_ptr->px;

	int plev = p_ptr->lev;

	/* Hack -- chance of "beam" instead of "bolt" */
	/* plev with beam flag, plev/2 otherwise */
	int beam = beam_chance();

    /* spellcasting bonus */	
	if (p_ptr->timed[TMD_BRAIL])
    {
       plev += 10;
    }

	switch (spell)
	{

		case CHEM_CURE_LIGHT_WOUNDS: /* min 11% */
		{
			int cure = damroll(2, 9);
			int curep = (p_ptr->mhp * 11) / 100;
			if (cure < curep) cure = curep;
			if (cure > 43) cure = 43; /* maximum */
			(void)hp_player(cure);
			(void)dec_timed(TMD_CUT, 10);
			break;
		}
		
		case CHEM_LIGHT_AREA:
		{
            die = randint(99) + randint(plev/5);
            if (plev >= 25) die = die + 10;
            if (plev >= 40) die = die + 10;
            if (plev >= 50) die = die + 5;
            if (die < 30)
            {
               spellswitch = 4; /* does not light whole room */
			   if (plev > 13) (void)lite_area(damroll(2, (plev / 2)), 2 + randint(plev / 7));
			   else (void)lite_area(randint(plev), 2 + randint(2));
               spellswitch = 0;
            }
            else if (die < 75)
            {
               spellswitch = 4; /* does not light whole room */
			   if (plev > 19) (void)lite_area(damroll(2, (plev / 2)), 3 + randint(plev / 6));
			   else (void)lite_area(damroll(2, (plev / 2)), 3 + randint(3));
               spellswitch = 0;
            }
            else if (die < 91)
            {
               spellswitch = 4; /* does not light whole room */
			   if (plev > 17) (void)lite_area(damroll(4, (plev / 3)), 5 + randint(plev / 6));
			   else (void)lite_area(damroll(4, (plev / 3)), 5 + randint(3));
               spellswitch = 0;
            }
            else if (die > 120)
            {
               (void)lite_area(damroll(3, (plev / 2)), (plev / 10) + 2);
            }
            else
            {
               (void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
            }
			break;
		}

		case CHEM_STINKING_CLOUD:
		{
			int sdam; /* wizard version is 10 + plev/2 (no randomness) */
			if (!get_aim_dir(&dir)) return (FALSE);
			/* alchemists get the spell very early and cheap, so nerf slightly for low level chrs */
			if (plev < 3) sdam = plev + 2 + randint(3);
			else if (plev < 9) sdam = plev + 2 + randint((plev+3) / 2);
			else if (plev < 12) sdam = 11 + randint(plev / 2);
			else if (plev < 22) sdam = 11 + randint(plev / 2 - 1);
			else sdam = plev/2 + 1 + randint(9);
			fire_ball(GF_POIS, dir, sdam, 2);
			break;
		}

		case CHEM_DISINFECTANT:
		{
            spellswitch = 16; /* damage slime in GF_bug_spray */
			fire_spread(GF_BUG_SPRAY, 3 + plev/8 + randint((plev+1)/2), 6 + plev/6);
			(void)dec_timed(TMD_POISONED, plev/4 + randint((plev+2)/3));
			(void)dec_timed(TMD_CUT, plev/3 + randint((plev+2)/3));
			(void)dec_timed(TMD_STUN, plev/5 + randint((plev+2)/3));
            if ((randint(100) < 25+(plev/2)) && (p_ptr->slime > PY_SLIME_HEALTHY)) p_ptr->slime -= 1;
            if ((randint(100) < 10) && (plev > 9))
            {
               spellswitch = 16; /* damage slime in GF_bug_spray */
			   (void)dec_timed(TMD_POISONED, randint((plev+2)/3));
			   (void)dec_timed(TMD_CUT, randint((plev+4)/5));
			   (void)dec_timed(TMD_STUN, randint((plev+5)/6));
               if ((randint(100) < (plev * 3)) && (p_ptr->slime > PY_SLIME_HEALTHY)) p_ptr->slime -= 1;
               if ((randint(100) < 2 + (plev/2)) && (p_ptr->silver > PY_SILVER_HEALTHY)) p_ptr->silver -= 1;
			   fire_spread(GF_BUG_SPRAY, 1 + randint((plev+1)/2), 6 + plev/6);
			   if (randint(100) < 75) (void)inc_timed(TMD_WOPP_POIS, randint(13) + 5);
			   (void)inc_timed(TMD_WOPP_POIS, randint((plev+1)/2) + 10);
            }
			break;
		}

        case CHEM_RESTORE_MEMORIES:
        {
			(void)clear_timed(TMD_CHARM);
			(void)clear_timed(TMD_AMNESIA);
			p_ptr->silver -= plev/10;
			(void)restore_level();
			break;
		}

		case CHEM_PHASE_DOOR:
		{
			bool controlled = FALSE;
            /* controlled teleport (random if you don't chose a target) */
		    if (p_ptr->telecontrol)
		    {
                if (control_tport(110 - plev, 12)) controlled = TRUE;
                if (!controlled) msg_print("You fail to control the teleportation.");
            }

            if (!controlled) teleport_player(10);
			break;
		}

		case CHEM_FIND_TRAPS_DOORS:
		{
            if (p_ptr->timed[TMD_2ND_THOUGHT])
            {
               msg_print("Your first sight supresses detection.");
            }
            else
            {
			    (void)detect_traps();
			    (void)detect_doorstairs(FALSE);
            }
			break;
		}

		case CHEM_DETECT_MONSTERS:
		{
            if (p_ptr->timed[TMD_2ND_THOUGHT])
            {
               msg_print("Your first sight supresses detection.");
            }
            else
            {
			  (void)detect_monsters_normal(1);
            }
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
		
		case CHEM_SKILL_BOOST:
		{
		    (void)inc_timed(TMD_SKILLFUL, plev/2 + 15 + randint((plev/2) + 20));
			break;
		}

        case CHEM_RESIST_ACID:
		{
			int wellmixed = plev + (goodluck*3)/2 + adj_int_spsk[p_ptr->stat_ind[A_INT]];
			int dur = randint(plev/2) + 15;
            (void)inc_timed(TMD_OPP_ACID, dur);
			if (randint(wellmixed) > 35) (void)inc_timed(TMD_ACID_BLOCK, dur);
			/* occationally happens even if not well mixed */
			else if (randint(100) < 16) (void)inc_timed(TMD_ACID_BLOCK, (dur/2)+1);
			break;
		}

		case CHEM_CURE_MODERATE_WOUNDS: /* min 20% */
		{
			int cure = damroll(6, 8);
			int curep = (p_ptr->mhp * 20) / 100;
			if (cure < curep) cure = curep;
			if (cure > 68) cure = 68; /* maximum */
			(void)hp_player(cure);
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
			int die = (adj_int_spsk[p_ptr->stat_ind[A_INT]] * 5) + (goodluck*2) + ((plev*3)/4);
			if (randint(die) > 25) (void)set_food(PY_FOOD_MAX - 1);
            else (void)set_food(p_ptr->food + 6500 + randint(1000));
			break;
		}

		case CHEM_CHEMICAL_RAGE:
		{
			(void)hp_player(plev/3);
            die = randint(75);
            if (p_ptr->peace)
        	{
                 msg_print("The peaceful magic prevents you from going into a rage.");
            }
	        else if ((p_ptr->timed[TMD_CHARM]) && (die > (plev + 25)))
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

		case CHEM_RESIST_POISON:
		{
			(void)inc_timed(TMD_OPP_POIS, randint(20) + 20);
			break;
		}

		case CHEM_HEAL:
		{
			int cure = damroll((plev/2), 9); /* (max225) */
			int curep = p_ptr->mhp / 3;
			if (cure < curep) cure = curep;
			(void)hp_player(cure);
			(void)clear_timed(TMD_STUN);
			(void)clear_timed(TMD_CUT);
			break;
		}
		
		case CHEM_DETECT_TREASURE:
        {
            if (p_ptr->timed[TMD_2ND_THOUGHT])
            {
               msg_print("Your first sight supresses detection.");
            }
            else
            {
			  (void)detect_treasure();
			  (void)detect_objects_normal(FALSE);
            }
			break;
		}

		case CHEM_TRAP_DOOR_DESTRUCTION:
		{
			(void)destroy_doors_touch(0);
			break;
		}

		case CHEM_CLEAR_MIND:
		{
			int time = 15 + randint(10 + plev/2);
            if (randint(75 + badluck) > 65)
            {
               msg_print("..That formula turned out a little wierd..");
               (void)inc_timed(TMD_2ND_THOUGHT, time);
            }
            (void)inc_timed(TMD_CLEAR_MIND, time);
			break;
        }

		case CHEM_IDENTIFY:
		{
			return ident_spell();
		}

		case CHEM_DETECT_INVISIBLE:
		{
            if (p_ptr->timed[TMD_2ND_THOUGHT])
            {
               msg_print("Your first sight supresses detection.");
            }
            else
            {
			  (void)detect_monsters_invis();
            }
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
            if (p_ptr->timed[TMD_2ND_THOUGHT])
            {
               msg_print("Your first sight supresses detection.");
            }
            else
            {
			  (void)detect_objects_magic();
            }
			break;
		}
		
		case CHEM_ACID_COAT_AMMO:
		{
			return brand_ammo(1);
		}
		
		case CHEM_BURST_OF_SPEED:
		{
			if (p_ptr->timed[TMD_SUST_SPEED])
			{
				msg_print("You cannot be hasted while your speed is sustained!");
				break;
			}
			
			if (!p_ptr->timed[TMD_FAST])
			{
				(void)set_timed(TMD_FAST, 4 + randint(4) + (plev/8));
			}
			else
			{
				(void)inc_timed(TMD_FAST, 1 + randint(3));
			}
			break;
		}

		case CHEM_TELEPORT_SELF:
		{
			bool controlled = FALSE;
			int dist = plev * 5;
			if (dist < 80) dist = 80;
            /* controlled teleport (random if you don't chose a target) */
		    if (p_ptr->telecontrol)
		    {
                if (control_tport(60 - plev, dist)) controlled = TRUE;
                if (!controlled) msg_print("You fail to control the teleportation.");
            }

            if (!controlled) teleport_player(plev * 5);
			break;
		}
		
		case CHEM_SLOW_MONSTERS:
		{
            int pwr = plev + adj_chr_charm[p_ptr->stat_ind[A_CHR]];
			(void)slow_monsters(pwr);
			break;
		}

		case CHEM_MASS_AMNESIA: /* was plev * 2 */
		{
            int pwr = ((plev * 7)/4) + (adj_chr_charm[p_ptr->stat_ind[A_CHR]] / 2);
			(void)mass_amnesia(pwr, 0, 0);
			break;
		}

		case CHEM_HASTE_SELF:
		{
			if (p_ptr->timed[TMD_SUST_SPEED])
			{
				msg_print("You cannot be hasted while your speed is sustained!");
				break;
			}
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

		case CHEM_TELEPORT_OTHER:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			dis = plev + 12 + goodluck + randint(plev * 2);
            if (cp_ptr->flags & CF_ZERO_FAIL) dis += 8;
			(void)teleport_monster(dir, dis);
			break;
		}

		case CHEM_TELEPORT_LEVEL:
		{
			(void)teleport_player_level(0);
			break;
		}

		case CHEM_EMERGENCY_GETAWAY_SHIELD:
		{
			int dur = 6 + (plev*3)/4 + (goodluck+1)/2 + randint((plev*3)/4);
			inc_timed(TMD_WSHIELD, dur);
			inc_timed(TMD_EMERGENCY_ESCAPE, dur);
			break;
		}

		case CHEM_SAFETY_GOGGLES:
		{
			int dur = 10 + (plev*3)/4 + (goodluck+1)/2 + randint(plev);
			inc_timed(TMD_SAFET_GOGGLES, dur);
			break;
		}

		case CHEM_SUSTAIN_HEALTH:
		{
			int dur = 8 + (plev*3)/4 + (goodluck+1)/2 + randint(plev);
			(void)inc_timed(TMD_SUSTAIN_HEALTH, dur);
			break;
		}
        
        case CHEM_BREATH_SHIELD:
		{
			(void)inc_timed(TMD_BR_SHIELD, randint(plev/3) + (plev/2) + 1);
			break;
        }

		case CHEM_GUARD_QUIVER:
		{
			int scoreq = ((plev-20)/3) + ((goodluck + 1)/2);
			(void)inc_timed(TMD_QUIVERGUARD, randint(plev + scoreq) + scoreq*5);
			if (plev > 35) scoreq += (plev - 35) * 5;
			if (rand_int(100) < scoreq) remove_cursed_quiver();
			break;
		}

		case CHEM_LIGHTNING_BOLT:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_beam(GF_ELEC, dir,
			          damroll(3+((plev-5)/6), 6));
			break;
		} /* L20 5-30, L30 7-42, L40 8-48, L50 10-60 */

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
			arrowdmg = damroll((plev/12), (4 + randint(plev/6)));
			if (arrowdmg > 70) arrowdmg = 70;
			manya = randint(3);
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
			arrowdmg = damroll(plev/4, 7) + 1;
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
			manya = 40;
			if (plev > 39) manya = 50;
			if (plev > 49) manya = 60;
			if (randint(100) < 34) fire_ball(GF_KILL_WALL, dir, damroll((plev/2), 2), 2);
			else if ((plev >= 30) && (goodluck)) (void)blast_a_wall(dir);
			fire_ball(GF_SHARD, dir, manya + randint(plev/5) + damroll((plev/2), 2), 2);
			break;
            /* 50-60 at L20, 55-70 at L30, 70-90 at L40, 85-110 at L50 */
		}

		case CHEM_ENCHANT_TO_HIT:
		{
			return enchant_spell(rand_int(3) + plev / 20, 0, 0, TRUE, FALSE);
		}

		case CHEM_ENCHANT_TO_DAM:
		{
			return enchant_spell(0, rand_int(3) + plev / 20, 0, TRUE, FALSE);
		}
		
		case CHEM_POISON_BRAND_AMMO:
		{
			return brand_ammo(3);
		}
		
		case CHEM_ELEMENTAL_BRAND_AMMO:
		{
			return brand_ammo(0);
		}

		case CHEM_MIMMIC_MIMMIC: /* replaced confuse*/
		{
			return mimmic_wand();
		}

		case CHEM_MIMMIC_ZAP: /* replaced sleep */
		{
			return zap_mimmic(FALSE);
		}

		case CHEM_ENHANCE_WAND: /* replaced wonder */
		{
			int power = (plev*4)/5;
			power = power + randint((power + p_ptr->skills[SKILL_DEV] - 1) / 2);
			/* max power */
			if ((plev >= 20) && (power > plev * 2 - 1)) power = plev * 2 - 1;
			else if ((plev < 20) && (power > 39)) power = 39;

			/* msg_format("enhance wand power is %d.", power); (testing) */

			return enhance_wand(power);
		}

#if norepugnance
		case CHEM_ELEMENT_BOLT:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
            die = randint(100);
            if (die < 15) fire_bolt_or_beam(beam, GF_ELEC, dir, 
                                            damroll(3+((plev-5)/6), 6));
            else if (die < 30) fire_bolt_or_beam(beam - 10, GF_COLD, dir,
                                                 damroll(5+((plev-5)/4), 7));
            else if (die < 45) fire_bolt_or_beam(beam - 10, GF_FIRE, dir,
                                                 damroll(6+((plev-5)/4), 7));
            else if (die < 60) fire_bolt_or_beam(beam - 10, GF_ACID, dir, 
                                                 damroll(8+((plev-5)/4), 7));
            else if (die < 70) fire_bolt_or_beam(beam + 2, GF_ELEC, dir, 
                                                 damroll(4+((plev-5)/6), 6));
            else if (die < 80) fire_bolt_or_beam(beam, GF_COLD, dir,
                                                 damroll(5+((plev-5)/4), 8));
            else if (die < 90) fire_bolt_or_beam(beam, GF_FIRE, dir,
                                                 damroll(8+((plev-5)/4), 8));
            else fire_bolt_or_beam(beam, GF_ACID, dir, 
                                   damroll(6+((plev-5)/4), 8));
			break;
		}
#endif

		case CHEM_REPUGNANCE:
		{
			fire_spread(GF_TURN_ALL, plev + ((goodluck+1)/2), 3 + (plev/8));
			inc_timed(TMD_STINKY, 3);
			break;
		}

		case CHEM_LASTING_REPUGNANCE:
		{
			fire_spread(GF_TURN_ALL, plev + ((goodluck+1)/2), 3);
			inc_timed(TMD_STINKY, rand_int(plev/2 + 5) + 20);
			break;
		}

		case CHEM_MAKE_GRENADE:
		{
			return craft_grenade();
		}

		case CHEM_ZOMBIE_FLAME:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			range = 14 + plev/12;
			fire_bolt_or_beam(beam-10, GF_ZOMBIE_FIRE, dir, damroll((plev/2), 6));
			break;
		}
		
		case CHEM_ELEMENT_BALL: /* alchemical dragonbreath */
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			range = 14 + plev/12;
			if (rand_int(100) < 48) fire_ball(GF_COLD, dir, 40 + plev, 2);
			else fire_ball(GF_FIRE, dir, 43 + plev, 2);
			if (randint(100) < 60 + ((badluck+1)/2)) (void)set_food(p_ptr->food - (30 + badluck + randint(60)));
			break;
		}

		case CHEM_ACID_BALL:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_ball(GF_ACID, dir, 55 + plev, 2);
			break;
		}

		case CHEM_ICE_STORM:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_ball(GF_ICE, dir, 50 + (plev * 2), 3);
			break;
		} /* 104 at L27, 120 at L35, 150 */

		case CHEM_SILVER_BULLET:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_bolt_or_beam(beam - 5, GF_SILVER_BLT, dir, 25 + plev + randint(plev));
			break;
		}

		case CHEM_RECHARGING:
		{
			return recharge(5 + randint(plev));
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
		
		case CHEM_TUNNEL_DIGGING:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			spellswitch = 31;
			/* spellswitch 31 makes it not stop at the first wall */
            (void)wall_to_mud(dir);
			break;
		}
		
		case CHEM_FIND_VAULT:
		{
            if (p_ptr->find_vault > 5)
            {
               msg_print("You are already seeking a vault.");
            }
            p_ptr->find_vault = goodluck + ((plev*3)/4) + randint(11);
            if (randint(p_ptr->find_vault + 10) > 39)
            {
               int dis = rand_int(51 - (p_ptr->find_vault / 2));
               (void)stair_creation(dis);
               /* separate roll */
               if (randint(p_ptr->find_vault + 10) > 35) (void)detect_doorstairs(TRUE);
            }
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
			controlled = FALSE;
            /* controlled teleport (random if you don't chose a target) */
		    if (p_ptr->telecontrol)
		    {
                if (control_tport(120 - plev, 12)) controlled = TRUE;
                if (!controlled) msg_print("You fail to control the teleportation.");
            }

            if (!controlled) teleport_player(10);
			break;
		}

		case CHEM_BEDLAM:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_ball(GF_OLD_CONF, dir, plev, 4);
			break;
		}

		case CHEM_ENCHANT_ARMOR: /* enchant armor */
		{
			return enchant_spell(0, 0, rand_int(3) + plev / 20, FALSE, FALSE);
		}

		case CHEM_GLYPH_OF_WARDING:
		{
			return (warding_glyph());
		}
		
		case CHEM_POISON_BRAND_WEAPON1:
        {
			brand_weapon(2);
			break;
        }
		
		case CHEM_SLIP_INTO_SHADOWS:
		{
             if (p_ptr->aggravate)
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
            fire_bolt_or_beam(beam - 10, GF_POIS, dir, 
                              damroll(7+(plev/5), 7));
			break;
        } /* 11d7 at L20, 13d7 at L30, 15d7 at L40, 17d7 at L50 */
          /* bigger than frost bolt weaker than fire bolt */
          
		case CHEM_TELEKINESIS:
		{
			do_telekinesis(9 + goodluck/2);
			break;
		}
        
		/* changed to restore cleverness */
        case CHEM_RESTORE_DEX1:
		{
			(void)do_res_stat(A_INT);
			(void)do_res_stat(A_DEX);
			(void)do_res_stat(A_CHR);
			break;
        }
		
		case CHEM_POISON_BRAND_WEAPON2:
        {
			brand_weapon(2);
			break;
        }
        
        case CHEM_ELIXIR_OF_LIFE: /* change plev to luck when you put luck in */
        {
            die = randint(99 + goodluck) + randint(plev/5);
			if ((goodluck > 11) || (randint(100) < goodluck)) die += randint(plev/4);
			if ((randint(100) < 11) && (goodluck)) die += goodluck + 1;
            if (die < 8)
            {
               msg_print("You make a terrible mistake with the ingrediants..");
               take_hit(damroll(6, 6), "a fatal failed attempt to make the Elixir of Life.");
			   (void)do_dec_stat(A_STR, 0);
			   (void)do_dec_stat(A_CON, 0);
               (void)set_food(PY_FOOD_WEAK + 10);
			   inc_timed(TMD_POISONED, randint(30) + 30);
			   inc_timed(TMD_CUT, randint(30) + 30);
			   inc_timed(TMD_STUN, randint(5) + 5);
            }
            else if (die < 19)
            {
               msg_print("You make a major mistake with the ingrediants..");
			   inc_timed(TMD_POISONED, randint(15) + 10);
			   inc_timed(TMD_IMAGE, randint(23) + 2);
			   inc_timed(TMD_BLIND, randint(23) + 2);
			   inc_timed(TMD_AMNESIA, randint(23) + 2);
			   inc_timed(TMD_CONFUSED, randint(23) + 2);
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
			   time = randint(50) + 50;
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

		case CHEM_METEOR_SWARM:
		{
            int swarmrad = 1;
			if (!get_aim_dir(&dir)) return (FALSE);
			if (plev + (goodluck+1)/2 + rand_int(5) > 49) swarmrad = 2;
			fire_swarm(2 + plev / 20, GF_METEOR, dir, 30 + plev/2 + randint(plev/3), swarmrad);
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

	int dir, die, dis, time, dmod, pwr;
	int ifrad, witherf;
	int drained, item, shifthit, dist, dam, drainmuch;
	bool controlled;
    cptr q, s;
	object_type *o_ptr;

	int plev = p_ptr->lev;

	/* Hack -- chance of "beam" instead of "bolt" */
	int beam = beam_chance();

    /* spellcasting bonus */	
	if (p_ptr->timed[TMD_BRAIL])
    {
       plev += 10;
       beam += 5;
    }
    if ((badweap) && (!goodweap) && (randint(100) < 25 + goodluck/2))
    {
       plev += badweap;
    }

	/* Spells. */
	switch (spell)
	{
		case DARK_DETECT_EVIL:
		{
            if (p_ptr->timed[TMD_2ND_THOUGHT])
            {
               msg_print("Your first sight supresses detection.");
            }
            else
            {
			  (void)detect_monsters_evil();
            }
			break;
		}
		
        case DARK_SHADOW_STING:
        {
			if (!get_aim_dir(&dir)) return (FALSE);
            dmod = 8;
            if (randint(30 + goodluck + badweap*2 - goodweap*3) > 25) dmod = 9;
            if (randint(plev + goodluck + badweap*2 - goodweap*3) > 46) dmod = 10;
            if ((badluck > 10) && (badweap < 1)) dmod = 8;
			if (plev < 4) fire_bolt(GF_NETHER, dir, damroll(3, (plev * 3)/2 ));
			else if (plev < 7) fire_bolt(GF_NETHER, dir, damroll(3, plev));
			else if (plev < 11) fire_bolt(GF_NETHER, dir, damroll(3, 6));
			else if (plev < 16) fire_bolt(GF_NETHER, dir, damroll(3, 7));
			else if (plev < 21) fire_bolt(GF_NETHER, dir, damroll(3, 8));
			else fire_bolt(GF_NETHER, dir, damroll((plev/7), dmod));
            if (randint(100) + badluck > 91 + goodluck)
            {
                (void)inc_timed(TMD_WITCH, 10 + randint(35-plev/2));
            }
			break;
        }

		
		case DARK_ALERTNESS:
		{
		    (void)inc_timed(TMD_SINFRA, randint(plev) + 10 + (plev * 3));
			break;
		}

		case DARK_CALL_DARK:
		{
            if ((!p_ptr->darkvis) && (plev < 11)) (void)inc_timed(TMD_DARKVIS, 10 + randint(plev+2));
		    else if (!p_ptr->darkvis) (void)inc_timed(TMD_DARKVIS, 12 + randint(plev-3));
		    /* slight extra alertness if the player already has darkvision */
		    else if (plev < 11) (void)inc_timed(TMD_WSINFRA, 4 + randint(plev+2));
            else (void)inc_timed(TMD_WSINFRA, 6 + randint(plev-3));
            ifrad = (plev / 3);
            if (plev > 11) ifrad = 3 + randint(plev / 6);
            if (ifrad < 2) ifrad = 2;
            if (randint(100) < 11 + badweap*2 + goodluck - ifrad) ifrad += 1;
			(void)unlite_area(damroll(2, ((plev+1) / 2)) + randint(ifrad + 1) + 1, ifrad, TRUE);
			/* in unlite_area function if from call dark spell: */
			/* if (rand_int(8) < p_ptr->lev) spellswitch = 23; */
			/* (spellswitch 23 damages monsters without them noticing) */
			break;
		}

		case DARK_SCARE_MONSTER:
		{
            pwr = plev + adj_chr_charm[p_ptr->stat_ind[A_CHR]];
			if (!get_aim_dir(&dir)) return (FALSE);
			if (randint(pwr + goodluck) > 50)
			{
				(void)fire_ball(GF_TURN_ALL, dir, pwr, 2);
			}
			else
			{
				(void)fear_monster(dir, pwr);
			}
			break;
		}

		/* stinking cloud + sleep */
		case DARK_NOXIOUS_FUMES:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			if (plev > 7 + (randint(18))) spellswitch = 22; /* adds chance of sleep to GF_POIS */
			fire_ball(GF_POIS, dir, 10 + (plev / 2) + randint((plev+3) / 4), 2);
			break;
		}

		case DARK_SLEEP_MONSTER:
		{
            int pwr = plev + adj_chr_charm[p_ptr->stat_ind[A_CHR]];
            if (!get_aim_dir(&dir)) return (FALSE);
			(void)sleep_monster(dir, pwr);
			break;
		}

		case DARK_CURE_POISON:
		{
			(void)clear_timed(TMD_POISONED);
			break;
		}
		
		case DARK_WITHER_FOE:
        {
            pwr = plev + adj_chr_charm[p_ptr->stat_ind[A_CHR]];
			if (!get_aim_dir(&dir)) return (FALSE);
			witherf = plev / 7;
			if ((witherf < 1) && (plev > 11)) witherf = 2;
			if (witherf < 1) witherf = 1;
			(void)confuse_monster(dir, pwr + 5);
			(void)slow_monster(dir, pwr);
			fire_bolt(GF_MANA, dir, damroll(witherf, 8));
			break;
        }

		case DARK_SEE_INVISIBLE:
		{
            if (p_ptr->timed[TMD_2ND_THOUGHT])
            {
               msg_print("Your first sight supresses detection.");
            }
            else
            {
			  (void)inc_timed(TMD_SINVIS, randint(plev) + 24);
            }
			break;
		}

		case DARK_FIND_TRAPS:
		{
            if (p_ptr->timed[TMD_2ND_THOUGHT])
            {
               msg_print("Your first sight supresses detection.");
            }
            else
            {
			  (void)detect_traps();
            }
			break;
		}

		case DARK_DETECT_DOORS_STAIRS:
		{
            if (p_ptr->timed[TMD_2ND_THOUGHT])
            {
               msg_print("Your first sight supresses detection.");
            }
            else
            {
			  (void)detect_doorstairs(FALSE);
            }
			break;
		}
		
		case DARK_DETECT_LIVING:
		{
            if (p_ptr->timed[TMD_2ND_THOUGHT])
            {
               msg_print("Your first sight supresses detection.");
            }
            else
            {
			  (void)detect_monsters_life();
            }
			break;
		}

		case DARK_SHADOW_SHIFT:
		{
			controlled = FALSE;
            /* controlled teleport (random if you don't chose a target) */
		    if (p_ptr->telecontrol)
		    {
                if (control_tport(100, 12)) controlled = TRUE;
                if (!controlled) msg_print("You fail to control the teleportation.");
            }

            if (!controlled) teleport_player(10);
			die = randint(100 - badluck);
			if (plev <= 20) die += (25-plev);
			if (die < 74)
            {
               if (plev >= 12) take_hit(randint(plev/6), "shifting through the nether realm");
               else take_hit(randint(2), "shifting through the nether realm");
            }
			break;
		}

        case DARK_DRAIN_CHARGES:
        {
			int healthis;
			int oldhp = p_ptr->chp;
			int maxhp = p_ptr->mhp;
			drained = 0;
			if (p_ptr->timed[TMD_FALSE_LIFE]) maxhp += 2 * (p_ptr->lev + 10);

            /* Restrict choices to staves or wands */
            item_tester_hook = item_tester_hook_drainable;

            /* Get an item */
            q = "Drain which wand or staff? ";
            s = "You have no wand or staff to use.";
            if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return FALSE;

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
				if (o_ptr->charges)
				{
					drained = o_ptr->charges;
   						/* Uncharge */
					o_ptr->charges = 0;
					/* higher value for higher level devices drained (1.5x) */
					if (k_info[o_ptr->k_idx].level >= 48)
                        drained = (drained * 3) / 2;
				}
				else if (((o_ptr->to_h > 0) || (o_ptr->to_d > 0) || (o_ptr->to_a > 0)) &&
				         (o_ptr->tval == TV_STAFF)) /* fall through */;
				else
				{
                    msg_print("That has no charges to drain.");
                    break;
                }
			}
			/* can't drain a staff's charges and combat bonuses at the same time */
			if ((!drained) && ((o_ptr->to_h > 0) || (o_ptr->to_d > 0) || (o_ptr->to_a > 0)))
			{
                /* drain & break other stuff */
                drained = o_ptr->to_h + o_ptr->to_d + o_ptr->to_a + 1;
                if (o_ptr->to_h > 0) o_ptr->to_h = 0 - 1;
                if (o_ptr->to_d > 0) o_ptr->to_d = 0 - 1;
                if (o_ptr->to_a > 0) o_ptr->to_a = 0 - 1;
            }
            /* heal based on amount drained */
			if (plev < 30) healthis = drained * (plev/3);
			else if (plev < 40) healthis = (drained+1) * (8 + randint(2));
            else healthis = (drained+2) * (plev/4);
            (void)hp_player(healthis);
            /* possible nourishment if you didn't need much healing */
            if ((maxhp - oldhp < healthis) && (p_ptr->food + 2000 < PY_FOOD_MAX))
            {
                healthis -= (maxhp - oldhp);
                if (healthis < 5) healthis = 5;
                if (healthis > 82) healthis = 80 + (healthis-80)/3;
                healthis = healthis * (3 + rand_int(8));
                (void)set_food(p_ptr->food + healthis);
            }
            if (randint(100) + badluck > 84 + goodluck)
            {
                (void)inc_timed(TMD_WITCH, 10 + randint(plev));
            }
			break;
		}
		
		case DARK_STINK: /* similar but slightly more powerful than luck spell */
        {
            die = randint(99 - badluck + goodluck) + randint(plev/4);
   			if (die > 10)
            {
               if (!get_aim_dir(&dir)) return (FALSE);
            }
            if (die <= 10) /* poison self with cloud centered at self */
            {
               msg_print("You smell something foul.");
			   if (!(p_ptr->resist_pois || p_ptr->timed[TMD_OPP_POIS]))
			   {
                  if (die < 2) die = 2;
			      dam = (109 - (die * 10)) / 3;
				  fire_spread(GF_POIS, 3 + randint(plev / 2), 1 + plev/6);
			      if (p_ptr->weakresist_pois) dam = (dam / 2) - 1;
			      (void)inc_timed(TMD_POISONED, rand_int(dam/2) + 10);
			      take_hit(dam, "a toxic smell");
			   }
			   else fire_spread(GF_POIS, 4 + randint(plev / 2), 1 + plev/6);
            }
            else if (die < 50) fire_ball(GF_POIS, dir, 11 + (plev / 2) + randint(goodluck), 2);
            else if (die < 60)
            {
               fire_bolt_or_beam(beam, GF_POIS, dir, damroll(5 + (plev / 2), 3));
               fire_ball(GF_POIS, dir, 2 + randint(31), 3);
            }
			else if (die < 80) fire_ball(GF_POIS, dir, 15 + (plev), plev / 12);
			else if (die < 101) fire_ball(GF_POIS, dir, 40 + (plev / 2) + randint(plev/2), 3);
			else /* (die > 100) */ fire_ball(GF_POIS, dir, 40 + (plev) + randint(plev), 3);
			break;
        }

		case DARK_TURN_UNDEAD:
		{
			int pwr = (p_ptr->lev + adj_chr_charm[p_ptr->stat_ind[A_CHR]])/2;
			if (adj_chr_charm[p_ptr->stat_ind[A_CHR]] < p_ptr->lev) pwr = p_ptr->lev - 1;
			(void)turn_undead(pwr);
			break;
		}

		case DARK_DEATH_BOLT:
        {
			if (!get_aim_dir(&dir)) return (FALSE);
			if (plev < 13) drain_life(dir, damroll(3 + ((plev) / 5), 6));
			else drain_life(dir, (plev*8)/5 + randint(plev));
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
		
        /* this and then speed of dark, then you're done */
		case DARK_DEMON_WARD:
        {
			int dur = randint(plev/3) + 4;
			if (plev < 12) dur = randint(4) + 4;
			(void)inc_timed(TMD_DEMON_WARD, dur);
			break;
        }

        case DARK_WITCH_DISINFECTANT:
		{
            spellswitch = 16; /* damage slime in GF_bug_spray */
			fire_spread(GF_BUG_SPRAY, 3 + plev/8 + randint((plev+1)/2), 6 + plev/6);
			(void)inc_timed(TMD_CUT, randint(6) + 3);
			/* now decreases poison by (poison/2) + d(poison/2) + 1 */
			if (p_ptr->timed[TMD_POISONED] < 6) clear_timed(TMD_POISONED);
			else if (p_ptr->timed[TMD_POISONED]) (void)dec_timed(TMD_POISONED, 1 + p_ptr->timed[TMD_POISONED]/2 + randint(p_ptr->timed[TMD_POISONED]/2));
			(void)dec_timed(TMD_STUN, 4 + randint(plev/2+1));
            if ((randint(100) < 34) && (p_ptr->slime > PY_SLIME_HEALTHY)) p_ptr->slime -= 1;
            if ((randint(100) < 34) && (p_ptr->slime > PY_SLIME_HEALTHY)) p_ptr->slime -= 1;
            if ((randint(100) < 16) && (plev > 9))
            {
	            spellswitch = 16; /* damage slime in GF_bug_spray */
			   clear_timed(TMD_POISONED);
			   (void)dec_timed(TMD_STUN, randint(4));
               if ((randint(100) < (plev * 3)) && (p_ptr->slime > PY_SLIME_HEALTHY)) p_ptr->slime -= 1;
               if ((randint(100) < (plev * 3)) && (p_ptr->silver > PY_SILVER_HEALTHY)) p_ptr->silver -= 1;
			   fire_spread(GF_BUG_SPRAY, 2 + randint(plev / 3 + 1), 6 + plev/6);
			   if ((plev < 20) && (goodluck > 1)) (void)inc_timed(TMD_OPP_POIS, randint(13) + 5);
			   else if (goodluck) (void)inc_timed(TMD_OPP_POIS, randint(13) + plev/4);
            }
			if (!p_ptr->timed[TMD_OPP_POIS])
            {
                if (plev >= 20) (void)inc_timed(TMD_WOPP_POIS, randint(13) + plev/4);
                else (void)inc_timed(TMD_WOPP_POIS, randint(13) + 5);
            }
			clear_timed(TMD_CONFUSED);
			clear_timed(TMD_BLIND);
			(void)dec_timed(TMD_FRENZY, 2 + randint(plev/4));
			(void)dec_timed(TMD_AMNESIA, 2 + randint(plev/4));
			(void)dec_timed(TMD_IMAGE, 2 + randint(plev/4));
			if (cp_ptr->flags & CF_ASSASSIN) (void)dec_timed(TMD_AFRAID, 5 + randint(plev/3));
			break;
		}

        case DARK_CIRCLE_OF_FLAME:
        {
			int dam = 31 + ((plev+2)/4) + randint((plev*9)/4);
			int rad = 5;
			if (plev >= 40) rad += 1;
			/* spread effect doesn't need a direction */
			/* if (!get_aim_dir(&dir)) return (FALSE); */
			fire_spread(GF_FIRE, dam, rad);
            if (randint(100) + badluck > 80 + goodluck)
            {
                (void)inc_timed(TMD_WITCH, 50 + randint(100-plev));
            }
			break;
        }

        case DARK_TRAP_DOOR_DESTRUCTION:
   		{
			(void)destroy_doors_touch(0);
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
        
        case DARK_SHADOW_STEP: /* now step into darkness */
        {
             (void)unlite_area(6, 6, FALSE);
             (void)inc_timed(TMD_DARKSTEP, randint(15 + goodluck) + 15);
    	     break;
        }
             
        case DARK_SLIP_INTO_SHADOWS:
		{
             if (p_ptr->aggravate)
             {
                if ((randint(plev * 2) > 84) && (goodluck > 2))
                {
                  msg_print("You are aggravating monsters, attempting to remove curse..");
                  if (remove_curse()) msg_print("You feel as if someone is watching over you.");
                }
                else msg_print("You can't slip into the shadows while you're aggravating monsters.");
    	        break;
             }   
             else (void)inc_timed(TMD_SHADOW, randint(plev+5) + 35);
             
    	     break;
		}

        case DARK_DARKVISION:
        {
            (void)inc_timed(TMD_DARKVIS, 20 + randint(plev));
			break;
        }
        
        case DARK_SHADOW_WARP:
        {
			controlled = FALSE;
			dist = plev * 5;
			if (dist < 80) dist = 80;
            /* controlled teleport (random if you don't chose a target) */
		    if (p_ptr->telecontrol)
		    {
                if (control_tport(60 - plev, dist)) controlled = TRUE;
                if (!controlled) msg_print("You fail to control the teleportation.");
            }

            if (!controlled) teleport_player(plev * 5);

			die = randint(100 + plev/8 + ((goodluck+1)/2) - badluck);
			shifthit = 1 + randint((plev + 2) / 4);
			if (plev < 11) shifthit = randint(4);
			if (shifthit > 11) shifthit = 11;

			if (die < 16) (void)inc_timed(TMD_STUN, randint(4) + 2);
			else if (die < 35) (void)inc_timed(TMD_BLIND, randint(4) + 1);
			else if (die < 45) (void)inc_timed(TMD_CUT, randint(4) + 1);
			else if (die < 67) (void)inc_timed(TMD_STUN, randint(2));
			else shifthit -= 1;
			if (randint(100) < 90 - goodluck) take_hit(shifthit, "shifting through the nether realm");
			break;
		}
		
        case DARK_WIZARD_LOCK:
		{
			if (!get_aim_dir(&dir)) return FALSE;

			/* Spell will jam a door or create a jammed door */
            /* in any one empty square. */
			fire_ball(GF_WIZLOCK, dir, 0, 0);
			break;
		}

		case DARK_SHADOW_MAPPING:
		{
			map_area(FALSE);
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
			break; /* (plev * 3.5) */
		}
             
        case DARK_DETECT_ENCHANTMENT:
		{
            if (p_ptr->timed[TMD_2ND_THOUGHT])
            {
               msg_print("Your first sight supresses detection.");
            }
            else
            {
			  (void)detect_objects_magic();
            }
			break;
		}

		case DARK_SCARE_MONSTERS:
        {
            int pwr = plev + 3 + adj_chr_charm[p_ptr->stat_ind[A_CHR]];
		    scare_monsters(pwr);
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
			fire_bolt_or_beam(beam + 15, GF_ICE, dir, damroll(5 + (plev/5), 8));
            if (randint(100) + badluck > 82 + goodluck)
            {
                (void)inc_timed(TMD_WITCH, 80 + randint(200-plev));
				/* good effect within a bad effect */
				if (randint(100) + badluck < plev + (goodluck*2) + 2)
				{
					fire_beam(GF_NETHER, dir, damroll(1 + (plev/10), 6));
				}
            }
			break;
		}

		case DARK_STENCH_OF_DEATH:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			/* this spell's got balls */
			fire_ball(GF_POIS, dir, 13 + (plev / 2), 3);
			fire_ball(GF_NETHER, dir, 13 + (plev / 8), 4);
			fire_ball(GF_OLD_CONF, dir, plev - 2, 4);
			dispel_life(randint((plev*3)/2));
			fire_spread(GF_POIS, 13, 4);
			if (p_ptr->resist_nethr) take_hit(randint(3), "the stench of death");
			else take_hit(randint(13), "the stench of death");
			if (p_ptr->resist_pois) (void)inc_timed(TMD_POISONED, randint(6) + 5);
			else (void)inc_timed(TMD_POISONED, randint(9) + 12);
            if (randint(100) + badluck > 80 + goodluck)
            {
                (void)inc_timed(TMD_WITCH, 110 + randint(200-plev));
            }
			break;
        }
 
        case DARK_BEAM_OF_DESTRUCTION:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			if (randint(95) < (plev - 15))
            {
               if (plev > 40) fire_beam(GF_ACID, dir, 1 + damroll((plev-37)/2, 7));
               else if (plev > 35) fire_beam(GF_ACID, dir, damroll((plev-32)/2, 6));
               else fire_beam(GF_ACID, dir, damroll(randint(3), 4));
            }
            spellswitch = 26; /* activate 1-spaces earthquakes in project() */
			/* lowered damage because the earthquakes do damage separately */
			fire_beam(GF_SHARD, dir, plev + damroll(plev/10, 9));
            if (randint(100) + badluck > 84 + goodluck)
            {
                (void)inc_timed(TMD_WITCH, 90 + randint(200-plev));
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
            /* L25 for necromancers, L35 for assassins */
            if ((goodluck) && ((cp_ptr->flags & CF_BEAM) && (plev >= 25)) || (plev >= 35))
                (void)blast_a_wall(dir);
            spellswitch = 11;  /* activates earthquake in project() */
            range = 21; /* small earthquake */
			fire_ball(GF_SHARD, dir, 24 + plev + damroll(plev/10, 11), 1 + plev/21);
            if (randint(100) + badluck > 80 + goodluck)
            {
                (void)inc_timed(TMD_WITCH, 90 + randint(200-plev));
            }
			break;
		}
             
		case DARK_VENOM_BOLT:
        {
			if (!get_aim_dir(&dir)) return (FALSE);
            fire_bolt_or_beam(beam + 5, GF_POIS, dir, 
                              damroll(7+((plev+1)/4), 7 + randint(plev/10)));
            if (randint(100) + badluck > 82 + goodluck)
            {
                (void)inc_timed(TMD_WITCH, 100 + randint(200-plev));
            }
			break;
        } /* 12d(8-9) at L20, 14d(8-10) at L30, 17d(8-11) at L40, 19d(8-12) at L50 */
             
		case DARK_CHAOS_STRIKE:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_bolt_or_beam(beam, GF_CHAOS, dir, damroll(13, plev));
            if (randint(100) + badluck > 82 + goodluck)
            {
                (void)inc_timed(TMD_WITCH, 110 + randint(200-plev));
            }
			break;
		}

		case DARK_BOLDNESS:
   		{
			(void)clear_timed(TMD_AFRAID);
			if (randint(46 + badluck) < (plev + 1 + goodluck/2)) (void)clear_timed(TMD_CHARM);
			if (p_ptr->peace) break;
			if ((plev > 39) && (randint(goodluck) > 6)) (void)inc_timed(TMD_HERO, randint(plev/2) + 6);
			else if ((plev > 20) && (randint(goodluck) + randint(4) > 7)) (void)inc_timed(TMD_HERO, randint((plev-10)/3) + 2);
			break;
		}

        case DARK_RESIST_POISON2:
		{
			(void)inc_timed(TMD_OPP_POIS, randint(20) + 20);
			break;
		}

        case DARK_FALSE_LIFE:
		{
			(void)inc_timed(TMD_FALSE_LIFE, randint(plev + 10) + plev + 10);
			(void)hp_player(plev);
			break;
		}

		case DARK_UNDEAD_SKIN:
		{
			time = randint(20) + 20;
			(void)inc_timed(TMD_OPP_COLD, time);
			(void)inc_timed(TMD_OPP_NETHR, time);
            if (randint(100) + badluck > 80 + goodluck)
            {
                (void)inc_timed(TMD_WITCH, 130 + randint(200-plev));
            }
			break;
		}
		
        case DARK_SUMMON_DEMONIC_AID:
        {
            if (plev > 40) die = randint(100);
            else if (plev > 30) die = randint(95);
            else die = randint(90);
            if (die < 20) do_call_help(989);
            else if (die < 40) do_call_help(990);
            else if (die < 60) do_call_help(991);
            else if (die < 80) do_call_help(992);
            else do_call_help(993);

            if (randint(100) + badluck > 70 + goodluck)
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
            if (randint(100) + badluck > 80 + goodluck)
            {
                (void)inc_timed(TMD_WITCH, 130 + randint(200-plev));
            }
			break;
		}
		
		case DARK_SHADOW_BARRIER:
		{
			(void)inc_timed(TMD_SHIELD, randint(20) + 30);
            if (randint(100) + badluck > 84 + goodluck)
            {
                (void)inc_timed(TMD_WITCH, 100 + randint(200-plev));
            }
			break;
		}

		case DARK_DETECT_ALL_MONSTERS:
        {
            if (p_ptr->timed[TMD_2ND_THOUGHT])
            {
               msg_print("Your first sight supresses detection.");
            }
            else
            {
			  (void)detect_monsters_normal(2);
			  (void)detect_monsters_invis();
            }
			break;
        }

		case DARK_TELEPORT_OTHER:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			dis = plev + 12 + goodluck + randint(plev * 2);
            if (cp_ptr->flags & CF_ZERO_FAIL) dis += 8;
			(void)teleport_monster(dir, dis);
			break;
		}

        case DARK_ORB_OF_DEATH:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_ball(GF_OLD_DRAIN, dir,
			          (damroll(3, 6) + plev + (plev / 2)),
			          ((plev < 30) ? 2 : 3));
            if (randint(100) + badluck > 75 + goodluck)
            {
                (void)inc_timed(TMD_WITCH, 130 + randint(200-plev));
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
            if (randint(100) + badluck > 75 + goodluck)
            {
                (void)inc_timed(TMD_WITCH, 130 + randint(200-plev));
            }
			break;
		}

		case DARK_VAMPIRIC_DRAIN:
		{
			int healmuch = 0;
			if (!get_aim_dir(&dir)) return (FALSE);
			drainmuch = damroll(plev / 3, 11);
			/* caps */
			if (drainmuch < (plev * 3) / 4) drainmuch = (plev * 3) / 4;
			if (drainmuch > 110 + plev) drainmuch = 110 + plev;
			/* do the damage & remember actual amount of damage done */
			healmuch = do_vampiric_drain(dir, drainmuch);
			if (healmuch < 1)
			{
				msg_print("The spell was not able to drain anything.");
				break;
			}
			(void)hp_player(healmuch);
			(void)set_food(p_ptr->food + 200 + randint(healmuch * 5));
			/* do not bloat */
			if ((p_ptr->food >= PY_FOOD_MAX) && (!badluck))
			{
				set_food(PY_FOOD_MAX - 1);
			}
            if (randint(100) + badluck > 75 + goodluck)
            {
                (void)inc_timed(TMD_WITCH, 130 + randint(200-plev));
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
            if (randint(100) + badluck > 75 + goodluck)
            {
                (void)inc_timed(TMD_WITCH, 150 + randint(200-plev));
            }
			break;
		}
		
		case DARK_GENOCIDE:
		{
            if (randint(100) + badluck > 80 + goodluck)
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

		case DARK_SNIPER_EYE:
		{
			/* doesn't get this spell until ~plev 30, but make sure of this */
            if (plev < 15) plev = 15;
            (void)inc_timed(TMD_SNIPER, 20 + randint(plev-5));
            break;
		}
        		
		case DARK_SPIRIT_OF_BALROG:
		{
			int time = randint(30) + 30;
			(void)inc_timed(TMD_OPP_FIRE, time);
			(void)inc_timed(TMD_OPP_DARK, time);
			(void)inc_timed(TMD_BALROG, time);
            if (randint(100) + badluck > 80 + goodluck)
            {
                (void)inc_timed(TMD_WITCH, 150 + randint(200-plev));
            }
			break;
		}

		case DARK_POISON_AMMO:
		{
			return brand_ammo(2);
		}

		/* maybe make a variation on the spell: speed of Darkness (like the priest's godspeed spell) */
		case DARK_HASTE:
		{
			if (p_ptr->timed[TMD_SUST_SPEED])
			{
				msg_print("You cannot be hasted while your speed is sustained!");
				break;
			}
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
            die = randint(75);
            if (p_ptr->peace)
        	{
                 msg_print("The peaceful magic prevents you from going into a rage.");
                 if ((goodluck) && (!p_ptr->timed[TMD_SUST_SPEED]))
                 {
			         /* speed bonus but no berserk */
                     p_ptr->spadjust = 4 + randint(1 + (plev/10));
                     (void)set_timed(TMD_ADJUST, randint(25) + plev);
                 }
            }
	        else if ((p_ptr->timed[TMD_CHARM]) && (die > (plev + 25)))
	        {
                 msg_print("You're in too good a mood to go into a battle frenzy");
            }
            else 
            {
			     if (!p_ptr->timed[TMD_SUST_SPEED]) p_ptr->spadjust = 4 + randint(1 + (plev/10));
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
                 if (randint(100) + badluck > 90 + goodluck)
                 {
                    (void)inc_timed(TMD_WITCH, 90 + randint(150-plev));
                 }
            }
			break;
		}
        
		case DARK_SEE_ALL_FOES:
		{
			int time = randint(35) + 35;
			if (p_ptr->timed[TMD_2ND_THOUGHT])
			{
				msg_print("Your first sight supresses detection.");
				break;
			}
			(void)clear_timed(TMD_BLIND);
			(void)inc_timed(TMD_SINVIS, time);
			if (p_ptr->lev > 32) (void)inc_timed(TMD_SINFRA, time);
			break;
		}

        case DARK_DRAW_MANA:        
        {
            if (p_ptr->csp >= p_ptr->msp)
            {
                  msg_print("Your mana is already at maximum!");
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
			      break;
               }
               /* this should never kill anyone */
               take_hit(die, "a bug in the game");
               p_ptr->csp += die + 1;
            }
            /* more powerful and risky version */
            else 
            {
               die = (plev/2) + randint(plev * 2);
               take_hit((die/2) + rand_int(3), "Taking your life for power");
               p_ptr->csp += die + 1 + randint(4);
            }
            /* not more than maximum */
            if (p_ptr->csp > p_ptr->msp) p_ptr->csp = p_ptr->msp;
            if (randint(100) + badluck > 84 + goodluck)
            {
                (void)inc_timed(TMD_WITCH, 90 + randint(150-plev));
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
            if (randint(100) + badluck > 84 + goodluck)
            {
                (void)inc_timed(TMD_WITCH, 100 + randint(150-plev));
            }
			break;
        }

		case DARK_DISPEL_UNDEAD2: /* less powerful for assassins */
		{
			if (cp_ptr->flags & CF_BEAM) (void)dispel_undead(randint(plev * 4));
			else (void)dispel_undead(1 + randint((plev * 7)/2)); /* x3.5 */
			break;
		}
		
		case DARK_BR_FEAR_CONF:
        {
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_ball(GF_DARK, dir, damroll(plev/6, 3), plev/9);
			fire_ball(GF_MANA, dir, damroll(plev/5, 8), plev/12);
			fire_ball(GF_CONFUSION, dir, damroll(3, plev/5), plev/12);
			fire_ball(GF_BRFEAR, dir, damroll(3, plev/5), plev/12);
            if (randint(100) + badluck > 80 + goodluck)
            {
                (void)inc_timed(TMD_WITCH, 150 + randint(200-plev));
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
            if (randint(100) + badluck > 80 + goodluck)
            {
                (void)inc_timed(TMD_WITCH, 150 + randint(200-plev));
            }
			break;
        }
        
        case DARK_DARKNESS_STORM: /* black equivelent of mana storm */
        {
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_ball(GF_DARK, dir, 270 + (plev * 2) + randint(plev+16), 3);
            if (randint(100) + badluck > 75 + goodluck)
            {
                (void)inc_timed(TMD_WITCH, 150 + randint(200-plev));
            }
		    if (!p_ptr->darkvis) (void)inc_timed(TMD_DARKVIS, plev/2 + randint(plev));
			break;
        }
        
        case DARK_BECOME_LICH:
        {
            if (randint(100) + badluck > 75 + goodluck)
            {
                (void)inc_timed(TMD_WITCH, 200 + randint(200-plev));
            }
            if ((badluck > 9) && (randint(100) < 5))
            {
                take_hit(plev + randint(plev*2), "taking your life to become an undead.");
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
               die = randint(100 + goodluck);
               if (die < 20)
               {
                  (void)do_dec_stat(A_WIS, 0);
                  (void)do_dec_stat(A_CHR, 0);
               }
               else if (die < 23) p_ptr->luck -= 1;
               else if (die < 53) (void)do_dec_stat(A_WIS, 0);
               else if (die < 83) (void)do_dec_stat(A_CHR, 0);
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
            time = randint(plev/2) + 25;
            if (randint(100) + badluck > 72 + goodluck)
            {
                (void)inc_timed(TMD_WITCH, 300 + randint(200-plev));
            }
            if (p_ptr->timed[TMD_BECOME_LICH])
            {
               if ((badluck > 9) && (randint(100) < 10))
               {
                  msg_print("The fires of hell eagerly consume your lifeless flesh.");
                  fire_dam((time*3) + randint(time*2), "Summoning fire while in the form of an undead.", plev-5);
               }
               else if ((badluck > 5) && (randint(100) < 22))
               {
                  msg_print("The fires of hell eagerly scorch your lifeless flesh.");
                  fire_dam((time*2) + randint(time), "Summoning fire while in the form of an undead.", plev-5);
               }
               else
               {
                  fire_dam(time + randint(time), "Summoning fire while in the form of an undead.", plev-5);
               }
               msg_print("The summoned fire causes you intense pain so you banish it quickly.");
            }
            else
            {
               (void)clear_timed(TMD_OPP_COLD);
               if (cp_ptr->flags & CF_ASSASSIN) time = time/2;
               if ((badluck > 5) && (randint(100) < 8))
               {
                  msg_print("The fires of hell eagerly scorch your evil flesh.");
                  fire_dam((time) + randint(time), "Summoning hellfire.", plev-5);
               }
               else if ((badluck > 9) && (randint(100) < 4))
               {
                  msg_print("The fires of hell eagerly consume your evil flesh.");
                  fire_dam((time*2) + randint(time), "Summoning hellfire.", plev-5);
               }
               (void)inc_timed(TMD_WSHIELD, time);
               (void)inc_timed(TMD_OPP_FIRE, time);
               (void)inc_timed(TMD_IMM_FIRE, time/3+2);
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
		return cast_mind_spell(index);
	} */
	else
	{
		return cast_priest_spell(index);
	}
}



