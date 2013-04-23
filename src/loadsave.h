#define LOADSAVE_H
#define SFM_SPECIAL	0x80

/*
 * The next variable in the save file is also a version variable.
 */
#define SF_CONTINUE 0

/*
 * Save the base and ceiling for each skill.
 */
#define SF_SKILL_BASE 1

/*
 * Save the last turn on which remove curse was cast (see curse.diff).
 */
#define SF_CURSE 2

/*
 * Change object_type.info from a byte to a u16b (see pseudoid.diff).
 */
#define SF_16_IDENT 3

/*
 * Save the flag in the death_event array which indicates which events
 * have been observed (see deatheventtext.diff).
 */
#define SF_DEATHEVENTTEXT 4

/*
 * Create an option to suppress the prompt given by autosave (see quietsave.diff).
 */
#define SF_Q_SAVE 5

/*
 * Save the number of monsters observed to have been killed in each quest
 * (see questsee.diff).
 */
#define SF_QUEST_UNKNOWN 6

/*
 * Save more verbose window flags (see windowpri.diff).
 */
#define SF_3D_WINPRI 7

/*
 * Change cave_type.info from a byte to a u16b (see showtrap.diff)
 */
#define SF_16_CAVE_FLAG 8

/*
 * Save MAX_SKILLS in the save file to allow painless addition (but not
 * replacement) of skills.
 */
#define SF_SAVE_MAX_SKILLS 9

/*
 * Reorganise k_info.txt a bit
 */
#define SF_K_INFO_1 10

/*
 * Track which quests have been encountered.
 */
#define SF_QUEST_KNOWN 11

/*
 * Rearrange r_info.txt so that all of the unusual monsters are at the beginning.
 * Add Bokrug and various causes of death to it.
 */
#define SF_R_INFO_1 12

/*
 * Store the HP gained recently by the player's vampiric melee attacks.
 */
#define SF_STORE_VAMP 13

/*
 * Read quests directly into q_info rather than from the dungeon definition.
 */
#define SF_QUEST_DIRECT 14

/*
 * Distribute most ego items based on contents of e_info.txt.
 * Rearranges k_info to make this easier and removes object_type.xtra*.
 */
#define SF_EGO_DISTRO 15

/*
 * Add object_type.stack to track which stack an item belongs to.
 */
#define SF_STACK_IDX 17

/*
 * Remember if the player has seen an object at some point.
 */
#define SF_OBJECT_SEEN 18
