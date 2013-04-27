/*
 * Spell types used by project(), and related functions.
 */
#define GF_ELEC		1
#define GF_POIS		2
#define GF_ACID		3
#define GF_COLD		4
#define GF_FIRE		5
#define GF_MISSILE	10
#define GF_ARROW		11
#define GF_PLASMA		12
#define GF_CRUSHER	13
#define GF_WATER		14
#define GF_LITE		15
#define GF_DARK		16
#define GF_LITE_WEAK	17
#define GF_DARK_WEAK	18
#define GF_SHARDS		20
#define GF_SOUND		21
#define GF_CONFUSION	22
#define GF_FORCE		23
#define GF_INERTIA	24
#define GF_MANA		26
#define GF_METEOR		27
#define GF_ICE			28
#define GF_CHAOS		30
#define GF_NETHER		31
#define GF_DISENCHANT	32
#define GF_NEXUS		33
#define GF_TIME		34
#define GF_GRAVITY	35
#define GF_KILL_WALL	40
#define GF_KILL_DOOR	41
#define GF_KILL_TRAP	42
#define GF_MAKE_WALL	45
#define GF_MAKE_DOOR	46
#define GF_MAKE_TRAP	47
#define GF_OLD_CLONE	51
#define GF_OLD_POLY			52
#define GF_OLD_HEAL			53
#define GF_OLD_SPEED	54
#define GF_OLD_SLOW			55
#define GF_OLD_CONF			56
#define GF_OLD_SLEEP	57
#define GF_OLD_DRAIN	58
#define GF_AWAY_UNDEAD	61
#define GF_AWAY_EVIL	62
#define GF_AWAY_ALL	63
#define GF_TURN_UNDEAD	64
#define GF_TURN_EVIL	65
#define GF_TURN_ALL	66
#define GF_DISP_UNDEAD	67
#define GF_DISP_EVIL	68
#define GF_DISP_ALL 69
#define GF_DISP_DEMON	70	/* New types for Zangband begin here... */
#define GF_DISP_LIVING	71
#define GF_ROCKET	72
#define GF_NUKE		73
#define GF_MAKE_GLYPH	74
#define GF_STASIS	75
#define GF_STONE_WALL	76
#define GF_DEATH_RAY	77
#define GF_STUN		78
#define GF_HOLY_FIRE	79
#define GF_HELL_FIRE	80
#define GF_DISINTEGRATE 81
#define GF_CHARM		82
#define GF_CONTROL_UNDEAD	 83
#define GF_CONTROL_ANIMAL	 84
#define GF_PSI		85
#define GF_PSI_DRAIN   86
#define GF_TELEKINESIS	87
#define GF_JAM_DOOR	88
#define GF_DOMINATION	89
#define GF_DISP_GOOD	90
#define GF_CRUSHER_MOD	91		/* Modify crusher timing */
#define GF_BFG 			92
#define GF_DEST_MUND		93

/* Maximum size of the gf_color array */
#define MAX_GF				94


/*
 * Bit flags for the "project()" function
 *
 *   JUMP: Jump directly to the target location (this is a hack)
 *   BEAM: Work as a beam weapon (affect every grid passed through)
 *   THRU: Continue "through" the target (used for "bolts"/"beams")
 *   STOP: Stop as soon as we hit a monster (used for "bolts")
 *   GRID: Affect each grid in the "blast area" in some way
 *   ITEM: Affect each object in the "blast area" in some way
 *   KILL: Affect each monster in the "blast area" in some way
 *   HIDE: Hack -- disable "visual" feedback from projection
 */
#define PROJECT_JUMP	0x0001
#define PROJECT_BEAM	0x0002
#define PROJECT_THRU	0x0004
#define PROJECT_STOP	0x0008
#define PROJECT_GRID	0x0010
#define PROJECT_ITEM	0x0020
#define PROJECT_KILL	0x0040
#define PROJECT_HIDE	0x0080
#define PROJECT_FAST	0x0100


/*
 * Bit flags for the "enchant()" function
 */
#define ENCH_TOHIT	0x01
#define ENCH_TODAM	0x02
#define ENCH_TOAC		0x04

/*
 * Bit flags for the "target_set" function XXX XXX XXX
 *
 *	KILL: Target monsters
 *	LOOK: Describe grid fully
 *	XTRA: Currently unused flag
 *	GRID: Select from all grids
 */
#define TARGET_KILL			0x01
#define TARGET_LOOK			0x02
#define TARGET_XTRA			0x04
#define TARGET_GRID			0x08

