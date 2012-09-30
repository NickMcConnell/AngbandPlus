/* File: script.h */

/* Purpose: definitions for S-Lang interface */

#ifdef USE_SLANG

#define FL_BLIND	0
#define FL_CONF		1
#define FL_POIS		2
#define FL_FEAR		3
#define FL_PARALYZ	4
#define FL_HALLU	5
#define FL_FAST		6
#define FL_SLOW		7
#define FL_SHIELD	8
#define FL_BLESS	9
#define FL_HERO		10
#define FL_SHERO	11
#define FL_PROTEVIL	12
#define FL_INVULN	13
#define FL_SEE_INV	14
#define FL_INFRA	15
#define FL_OPP_ACID	16
#define FL_OPP_ELEC	17
#define FL_OPP_FIRE	18
#define FL_OPP_COLD	19
#define FL_OPP_POIS	20
#define FL_STUN		21
#define FL_CUT		22
#define FL_FOOD		23

/* Detection types */
#define DET_TRAPS               1
#define DET_DOORS               2
#define DET_STAIRS              3
#define DET_TREASURE            4
#define DET_OBJECTS_GOLD        5
#define DET_OBJECTS_NORMAL      6
#define DET_OBJECTS_MAGIC       7
#define DET_MONSTERS_NORMAL     8
#define DET_MONSTERS_INVIS      9
#define DET_MONSTERS_EVIL      10
#define DET_MONSTERS_NONLIVING 11
#define DET_ALL                12

/* Miscellaneous effects */
#define MI_ID		0
#define MI_FULL_ID	1
#define MI_RES_LEV	2
#define MI_UNCURSE	3
#define MI_FULL_UNCURSE	4
#define MI_PROBING	5
#define MI_MK_STAIR	6
#define MI_GLYPH	7
#define MI_GENOCIDE	8
#define MI_MASS_GENO	9
#define MI_TELE_LEV	10
#define MI_MAP_AREA	11
#define MI_WIZ_LITE	12
#define MI_BRAND_WEAP	13
#define MI_BRAND_BOLT	14
#define MI_LITE_ROOM	15
#define MI_DESTROY	16
#define MI_QUAKE	17
#define MI_ALTER	18
#define MI_RAZORBACK	19
#define MI_RING_POWER	20

#endif /* USE_SLANG */
