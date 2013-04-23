#define LOADSAVE_H
#define SFM_SPECIAL	0x80

/*
 * Save the base and ceiling for each skill.
 */
#define SF_SKILL_BASE	0x0002

/*
 * Save the last turn on which remove curse was cast (see curse.diff).
 */
#define SF_CURSE	0x0004

/*
 * Change object_type.info from a byte to a u16b (see pseudoid.diff).
 */
#define SF_16_IDENT	0x0008

/*
 * Save the flag in the death_event array which indicates which events
 * have been observed (see deatheventtext.diff).
 */
#define SF_DEATHEVENTTEXT	0x0010

/*
 * Create an option to suppress the prompt given by autosave (see quietsave.diff).
 */
#define SF_Q_SAVE	0x0020

/*
 * Save the number of monsters observed to have been killed in each quest
 * (see questsee.diff).
 */
#define SF_QUEST_UNKNOWN	0x0040

/*
 * Save more verbose window flags (see windowpri.diff).
 */
#define SF_3D_WINPRI	0x0080

/*
 * Change cave_type.info from a byte to a u16b (see showtrap.diff)
 */
#define SF_16_CAVE_FLAG	0x0100

/*
 * Save MAX_SKILLS in the save file to allow painless addition (but not
 * replacement) of skills.
 */
#define SF_SAVE_MAX_SKILLS 0x0200
