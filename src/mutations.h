/* New Mutation System, carefully crafted to be backwards compatible with the old ... 
   You are free to add new mutations (though extending MUT_FLAG_SIZE requires save file
   work), and place activatable mutations where ever you please.
   See mutations.c for the new.  mutation.c for the old (Soon to be removed)
*/
#define MUT_SPIT_ACID      0   /* Start of old mutations */
#define MUT_BR_FIRE        1
#define MUT_HYPN_GAZE      2
#define MUT_TELEKINESIS    3
#define MUT_TELEPORT       4
#define MUT_MIND_BLAST     5
#define MUT_RADIATION      6
#define MUT_VAMPIRISM      7
#define MUT_SMELL_METAL    8
#define MUT_SMELL_MONSTERS 9
#define MUT_BLINK          10
#define MUT_EAT_ROCK       11
#define MUT_SWAP_POS       12
#define MUT_SHRIEK         13
#define MUT_ILLUMINE       14
#define MUT_DET_CURSE      15
#define MUT_BERSERK        16
#define MUT_POLYMORPH      17
#define MUT_MIDAS_TCH      18
#define MUT_GROW_MOLD      19
#define MUT_RESIST         20
#define MUT_EARTHQUAKE     21
#define MUT_EAT_MAGIC      22
#define MUT_WEIGH_MAG      23
#define MUT_STERILITY      24
#define MUT_PANIC_HIT      25
#define MUT_DAZZLE         26
#define MUT_LASER_EYE      27
#define MUT_RECALL         28
#define MUT_BANISH         29
#define MUT_COLD_TOUCH     30
#define MUT_LAUNCHER       31

#define MUT_BERS_RAGE      32
#define MUT_COWARDICE      33
#define MUT_TELEPORT_RND   34
#define MUT_ALCOHOL        35
#define MUT_HALLU          36
#define MUT_FLATULENT      37
#define MUT_SCOR_TAIL      38
#define MUT_HORNS          39
#define MUT_BEAK           40
#define MUT_ATT_DEMON      41
#define MUT_PROD_MANA      42
#define MUT_SPEED_FLUX     43
#define MUT_BANISH_ALL     44
#define MUT_EAT_LIGHT      45
#define MUT_TRUNK          46
#define MUT_ATT_ANIMAL     47
#define MUT_TENTACLES      48
#define MUT_RAW_CHAOS      49
#define MUT_NORMALITY      50
#define MUT_WRAITH         51
#define MUT_POLY_WOUND     52
#define MUT_WASTING        53
#define MUT_ATT_DRAGON     54
#define MUT_WEIRD_MIND     55
#define MUT_NAUSEA         56
#define MUT_CHAOS_GIFT     57
#define MUT_WALK_SHAD      58
#define MUT_WARNING        59
#define MUT_INVULN         60
#define MUT_SP_TO_HP       61
#define MUT_HP_TO_SP       62
#define MUT_DISARM         63

#define MUT_HYPER_STR      64
#define MUT_PUNY           65
#define MUT_HYPER_INT      66
#define MUT_MORONIC        67
#define MUT_RESILIENT      68
#define MUT_XTRA_FAT       69
#define MUT_ALBINO         70
#define MUT_FLESH_ROT      71
#define MUT_SILLY_VOI      72
#define MUT_BLANK_FAC      73
#define MUT_ILL_NORM       74
#define MUT_XTRA_EYES      75
#define MUT_MAGIC_RES      76
#define MUT_XTRA_NOIS      77
#define MUT_INFRAVIS       78
#define MUT_XTRA_LEGS      79
#define MUT_SHORT_LEG      80
#define MUT_ELEC_TOUC      81
#define MUT_FIRE_BODY      82
#define MUT_WART_SKIN      83
#define MUT_SCALES         84
#define MUT_IRON_SKIN      85
#define MUT_WINGS          86
#define MUT_FEARLESS       87
#define MUT_REGEN          88
#define MUT_ESP            89
#define MUT_LIMBER         90
#define MUT_ARTHRITIS      91
#define MUT_BAD_LUCK       92
#define MUT_VULN_ELEM      93
#define MUT_MOTION         94
#define MUT_GOOD_LUCK      95   /* End of the old mutations */

#define MAX_MUTATIONS      96   /* see also MUT_FLAG_SIZE in defines.h */

typedef enum {
	MUT_RATING_AWFUL = -2,
	MUT_RATING_BAD   = -1,
	MUT_RATING_AVERAGE = 0,
	MUT_RATING_GOOD = 1,
	MUT_RATING_GREAT = 2
} mutation_rating;

typedef enum {
	MUT_TYPE_ACTIVATION = 0x01,	/* Mutation activates as a racial power (e.g. Harden to Elements) */
	MUT_TYPE_EFFECT = 0x02,     /* Mutation has a passive effect (e.g. Flatulence)*/
	MUT_TYPE_BONUS = 0x04       /* Mutation confers a passive bonus (e.g. +4 Str) */
} mutation_type;

typedef bool (*mut_pred)(int mut_idx);
extern bool mut_berserker_pred(int mut_idx);
extern bool mut_good_pred(int mut_idx);

extern void mut_calc_bonuses(void);
extern void mut_gain(int mut_idx);
extern bool mut_gain_random(mut_pred pred);
extern int  mut_get_spells(spell_info* spells, int max);
extern void mut_lock(int mut_idx);
extern bool mut_locked(int mut_idx);
extern void mut_lose(int mut_idx);
extern bool mut_lose_random(mut_pred pred);
extern bool mut_present(int mut_idx);
extern void mut_process(void);
extern int  mut_rating(int mut_idx);
extern int  mut_type(int mut_idx);
extern void mut_unlock(int mut_idx);
