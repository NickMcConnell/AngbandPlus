/* EFGchange try to get away from stupid TR1/TR2/TR3 */

/* parameter to pass to object_success_permillage */
#define USE_NORMAL FALSE
#define USE_IMPAIRED TRUE

/* for now, these have to be in the same order as the TR?_FLAGS in defines.h */
enum object_flag
{
STR,
INT,
WIS,
DEX,
CON,
CHR,
XXX1a,
XXX2a,
STEALTH,
SEARCH,
INFRA,
TUNNEL,
SPEED,
BLOWS,
SHOTS,
MIGHT,
SLAY_ANIMAL,
SLAY_EVIL,
SLAY_UNDEAD,
SLAY_DEMON,
SLAY_ORC,
SLAY_TROLL,
SLAY_GIANT,
SLAY_DRAGON,
KILL_DRAGON,
KILL_DEMON,
KILL_UNDEAD,
BRAND_POIS,
BRAND_ACID,
BRAND_ELEC,
BRAND_FIRE,
BRAND_COLD,
SUST_STR,
SUST_INT,
SUST_WIS,
SUST_DEX,
SUST_CON,
SUST_CHR,
XXX1b,
XXX2b,
XXX3b,
XXX4b,
XXX5b,
XXX6b,
IM_ACID,
IM_ELEC,
IM_FIRE,
IM_COLD,
RES_ACID,
RES_ELEC,
RES_FIRE,
RES_COLD,
RES_POIS,
RES_FEAR,
RES_LITE,
RES_DARK,
RES_BLIND,
RES_CONFU,
RES_SOUND,
RES_SHARD,
RES_NEXUS,
RES_NETHR,
RES_CHAOS,
RES_DISEN,
SLOW_DIGEST,
FEATHER,
LITE,
REGEN,
TELEPATHY,
SEE_INVIS,
FREE_ACT,
HOLD_LIFE,
NO_FUEL,
XXX2c,
XXX3c,
XXX4c,
IMPACT,
TELEPORT,
AGGRAVATE,
DRAIN_EXP,
IGNORE_ACID,
IGNORE_ELEC,
IGNORE_FIRE,
IGNORE_COLD,
XXX5c,
XXX6c,
BLESSED,
ACTIVATE,
INSTA_ART,
EASY_KNOW,
HIDE_TYPE,
SHOW_MODS,
XXX7,
LIGHT_CURSE,
HEAVY_CURSE,
PERMA_CURSE,

NUM_FLAGS
};

typedef byte inscrip_type;
typedef u32b flag_block_type;
#define OBJECT_FLAGS_PER_BLOCK (8 * sizeof(flag_block_type))
#define OBJECT_FLAG_BLOCKS (1 + (NUM_FLAGS-1) / OBJECT_FLAGS_PER_BLOCK)


enum flag_format
{
	OBVIOUS_FORMAT_NAME,
	OBVIOUS_FORMAT_BOOST,
};

enum flag_effect
{
	FLAG_EFFECT_BRAND,
	FLAG_EFFECT_SLAY,
	FLAG_EFFECT_LOW_RESIST, /* can include ignores and immunities */
	FLAG_EFFECT_HIGH_RESIST,
	FLAG_EFFECT_POWER,
	FLAG_EFFECT_SUSTAIN,
	FLAG_EFFECT_CURSE,
	FLAG_EFFECT_STAT,
	FLAG_EFFECT_MIGHT, /* should include blows, shots, tunnel */

	FLAG_EFFECT_MAX
};

enum element_enum
{
	FIRE,
	COLD,
	ELEC,
	ACID,
	POIS,

	ELEMENT_MAX,
};
typedef enum element_enum element_type;

typedef u32b monster_flag;
typedef union {
	element_type element;
	monster_flag immune;
	monster_flag susceptible;
} flag_specific_type;

typedef struct 
{
	enum object_flag flag_idx;
	bool obvious;
	enum flag_effect category;
	flag_specific_type specific;
	enum flag_effect effect;
	enum flag_format format;
	char *desc;
} flag_struct;

