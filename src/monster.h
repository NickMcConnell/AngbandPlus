
/*
 * Special Monster Flags (all temporary)
 */
#define MFLAG_VIEW	0x01	/* Monster is in line of sight */
/* xxx */
#define MFLAG_BORN	0x10	/* Monster is still being born */
#define MFLAG_NICE	0x20	/* Monster is still being nice */
#define MFLAG_SHOW	0x40	/* Monster is recently memorized */
#define MFLAG_MARK	0x80	/* Monster is currently memorized */


/*
 * New monster blow methods
 */
#define RBM_HIT		1
#define RBM_TOUCH	2
#define RBM_PUNCH	3
#define RBM_KICK		4
#define RBM_CLAW		5
#define RBM_BITE		6
#define RBM_STING	7
#define RBM_XXX1		8
#define RBM_BUTT		9
#define RBM_CRUSH	10
#define RBM_ENGULF	11
#define RBM_CHARGE	12 /* TY */
#define RBM_CRAWL	13
#define RBM_DROOL	14
#define RBM_SPIT		15
#define RBM_XXX3		16
#define RBM_GAZE		17
#define RBM_WAIL		18
#define RBM_SPORE	19
#define RBM_XXX4		20
#define RBM_BEG		21
#define RBM_INSULT	22
#define RBM_MOAN		23
#define RBM_SHOW		24 /* TY */


/*
 * New monster blow effects
 */
#define RBE_HURT				1
#define RBE_POISON			2
#define RBE_UN_BONUS	3
#define RBE_UN_POWER	4
#define RBE_EAT_GOLD	5
#define RBE_EAT_ITEM	6
#define RBE_EAT_FOOD	7
#define RBE_EAT_LITE	8
#define RBE_ACID				9
#define RBE_ELEC				10
#define RBE_FIRE				11
#define RBE_COLD				12
#define RBE_BLIND			13
#define RBE_CONFUSE			14
#define RBE_TERRIFY			15
#define RBE_PARALYZE	16
#define RBE_LOSE_STR	17
#define RBE_LOSE_INT	18
#define RBE_LOSE_WIS	19
#define RBE_LOSE_DEX	20
#define RBE_LOSE_CON	21
#define RBE_LOSE_CHR	22
#define RBE_LOSE_ALL	23
#define RBE_SHATTER			24
#define RBE_EXP_10			25
#define RBE_EXP_20			26
#define RBE_EXP_40			27
#define RBE_EXP_80			28


/*** Monster flag values (hard-coded) ***/


/*
 * New monster race bit flags
 */
#define RF1_UNIQUE					0x00000001	/* Unique Monster */
#define RF1_QUESTOR					0x00000002	/* Quest Monster */
#define RF1_MALE						0x00000004	/* Male gender */
#define RF1_FEMALE					0x00000008	/* Female gender */
#define RF1_CHAR_CLEAR		0x00000010	/* Absorbs symbol */
#define RF1_CHAR_MULTI		0x00000020	/* Changes symbol */
#define RF1_ATTR_CLEAR		0x00000040	/* Absorbs color */
#define RF1_ATTR_MULTI		0x00000080	/* Changes color */
#define RF1_FORCE_DEPTH		0x00000100	/* Start at "correct" depth */
#define RF1_FORCE_MAXHP		0x00000200	/* Start with max hitpoints */
#define RF1_FORCE_SLEEP		0x00000400	/* Start out sleeping */
#define RF1_FORCE_EXTRA		0x00000800	/* Start out something */
#define RF1_FRIEND					0x00001000	/* Arrive with a friend */
#define RF1_FRIENDS					0x00002000	/* Arrive with some friends */
#define RF1_ESCORT					0x00004000	/* Arrive with an escort */
#define RF1_ESCORTS					0x00008000	/* Arrive with some escorts */
#define RF1_NEVER_BLOW		0x00010000	/* Never make physical blow */
#define RF1_NEVER_MOVE		0x00020000	/* Never make physical move */
#define RF1_RAND_25					0x00040000	/* Moves randomly (25%) */
#define RF1_RAND_50					0x00080000	/* Moves randomly (50%) */
#define RF1_ONLY_GOLD		0x00100000	/* Drop only gold */
#define RF1_ONLY_ITEM		0x00200000	/* Drop only items */
#define RF1_DROP_60					0x00400000	/* Drop an item/gold (60%) */
#define RF1_DROP_90					0x00800000	/* Drop an item/gold (90%) */
#define RF1_DROP_1D2			0x01000000	/* Drop 1d2 items/gold */
#define RF1_DROP_2D2			0x02000000	/* Drop 2d2 items/gold */
#define RF1_DROP_3D2			0x04000000	/* Drop 3d2 items/gold */
#define RF1_DROP_4D2			0x08000000	/* Drop 4d2 items/gold */
#define RF1_DROP_GOOD		0x10000000	/* Drop good items */
#define RF1_DROP_GREAT		0x20000000	/* Drop great items */
#define RF1_DROP_USEFUL		0x40000000	/* Drop "useful" items */
#define RF1_DROP_CHOSEN		0x80000000	/* Drop "chosen" items */

/*
 * New monster race bit flags
 */
#define RF2_STUPID					0x00000001	/* Monster is stupid */
#define RF2_SMART					0x00000002	/* Monster is smart */
#define RF2_CAN_SPEAK			0x00000004	/* TY: can speak */
#define RF2_REFLECTING				0x00000008	/* Reflects bolts */
#define RF2_INVISIBLE		0x00000010	/* Monster avoids vision */
#define RF2_COLD_BLOOD		0x00000020	/* Monster avoids infra */
#define RF2_EMPTY_MIND		0x00000040	/* Monster avoids telepathy */
#define RF2_WEIRD_MIND		0x00000080	/* Monster avoids telepathy? */
#define RF2_MULTIPLY			0x00000100	/* Monster reproduces */
#define RF2_REGENERATE		0x00000200	/* Monster regenerates */
#define RF2_SHAPECHANGER		0x00000400	/* TY: shapechanger */
#define RF2_ATTR_ANY		0x00000800	/* TY: Attr_any */
#define RF2_POWERFUL			0x00001000	/* Monster has strong breath */
#define RF2_ELDRITCH_HORROR			0x00002000	/* Sanity-blasting horror	*/
#define RF2_AURA_FIRE				0x00004000	/* Burns in melee */
#define RF2_AURA_ELEC				0x00008000	/* Shocks in melee */
#define RF2_OPEN_DOOR		0x00010000	/* Monster can open doors */
#define RF2_BASH_DOOR		0x00020000	/* Monster can bash doors */
#define RF2_PASS_WALL		0x00040000	/* Monster can pass walls */
#define RF2_KILL_WALL		0x00080000	/* Monster can destroy walls */
#define RF2_MOVE_BODY		0x00100000	/* Monster can move monsters */
#define RF2_KILL_BODY		0x00200000	/* Monster can kill monsters */
#define RF2_TAKE_ITEM		0x00400000	/* Monster can pick up items */
#define RF2_KILL_ITEM		0x00800000	/* Monster can crush items */
#define RF2_HEAL_ACID		0x01000000	/* Healed by acid */
#define RF2_HEAL_ELEC		0x02000000	/* Healed by electricity */
#define RF2_HEAL_FIRE		0x04000000	/* Healed by fire */
#define RF2_HEAL_COLD		0x08000000	/* Healed by cold */
#define RF2_HEAL_POIS		0x10000000	/* Healed by poison */
#define RF2_BRAIN_6			0x20000000
#define RF2_BRAIN_7			0x40000000
#define RF2_BRAIN_8			0x80000000

/*
 * New monster race bit flags
 */
#define RF3_ORC						0x00000001	/* Orc */
#define RF3_TROLL					0x00000002	/* Troll */
#define RF3_GIANT					0x00000004	/* Giant */
#define RF3_DRAGON					0x00000008	/* Dragon */
#define RF3_DEMON					0x00000010	/* Demon */
#define RF3_UNDEAD					0x00000020	/* Undead */
#define RF3_EVIL						0x00000040	/* Evil */
#define RF3_ANIMAL					0x00000080	/* Animal */
#define RF3_AMBERITE			0x00000100	/* TY: Amberite */
#define RF3_GOOD						0x00000200	/* Good */
#define RF3_DOOM						0x00000400	/* Minion of Id (?) */
#define RF3_NONLIVING		0x00000800	/* TY: Non-Living (?) */
#define RF3_HURT_LITE		0x00001000	/* Hurt by lite */
#define RF3_HURT_ROCK		0x00002000	/* Hurt by rock remover */
#define RF3_HURT_FIRE		0x00004000	/* Hurt badly by fire */
#define RF3_HURT_COLD		0x00008000	/* Hurt badly by cold */
#define RF3_IM_ACID					0x00010000	/* Resist acid a lot */
#define RF3_IM_ELEC					0x00020000	/* Resist elec a lot */
#define RF3_IM_FIRE					0x00040000	/* Resist fire a lot */
#define RF3_IM_COLD					0x00080000	/* Resist cold a lot */
#define RF3_IM_POIS					0x00100000	/* Resist poison a lot */
#define RF3_RES_TELE					0x00200000	/* Resist teleportation */
#define RF3_RES_NETH			0x00400000	/* Resist nether a lot */
#define RF3_RES_WATE			0x00800000	/* Resist water */
#define RF3_RES_PLAS			0x01000000	/* Resist plasma */
#define RF3_RES_NEXU			0x02000000	/* Resist nexus */
#define RF3_RES_DISE			0x04000000	/* Resist disenchantment */
#define RF3_RES_SOUN						0x08000000	/* Resist sound */
#define RF3_NO_FEAR					0x10000000	/* Cannot be scared */
#define RF3_NO_STUN					0x20000000	/* Cannot be stunned */
#define RF3_NO_CONF					0x40000000	/* Cannot be confused */
#define RF3_NO_SLEEP			0x80000000	/* Cannot be slept */

/*
 * New monster race bit flags
 */
#define RF4_SHRIEK					0x00000001	/* Shriek for help */
#define RF4_BO_SHARD					0x00000002	/* Shard bolt */
#define RF4_BL_SHARD					0x00000004	/* Shard blast */
#define RF4_ROCKET					0x00000008	/* TY: Rocket */
#define RF4_ARROW_1					0x00000010	/* Fire an arrow (light) */
#define RF4_ARROW_2					0x00000020	/* Fire an arrow (heavy) */
#define RF4_ARROW_3					0x00000040	/* Fire missiles (light) */
#define RF4_ARROW_4					0x00000080	/* Fire missiles (heavy) */
#define RF4_BR_ACID					0x00000100	/* Breathe Acid */
#define RF4_BR_ELEC					0x00000200	/* Breathe Elec */
#define RF4_BR_FIRE					0x00000400	/* Breathe Fire */
#define RF4_BR_COLD					0x00000800	/* Breathe Cold */
#define RF4_BR_POIS					0x00001000	/* Breathe Poison */
#define RF4_BR_NETH					0x00002000	/* Breathe Nether */
#define RF4_BR_LITE					0x00004000	/* Breathe Lite */
#define RF4_BR_DARK					0x00008000	/* Breathe Dark */
#define RF4_BR_CONF					0x00010000	/* Breathe Confusion */
#define RF4_BR_SOUN					0x00020000	/* Breathe Sound */
#define RF4_BR_CHAO					0x00040000	/* Breathe Chaos */
#define RF4_BR_DISE					0x00080000	/* Breathe Disenchant */
#define RF4_BR_NEXU					0x00100000	/* Breathe Nexus */
#define RF4_BR_TIME					0x00200000	/* Breathe Time */
#define RF4_BR_INER					0x00400000	/* Breathe Inertia */
#define RF4_BR_GRAV					0x00800000	/* Breathe Gravity */
#define RF4_BR_SHAR					0x01000000	/* Breathe Shards */
#define RF4_BR_PLAS					0x02000000	/* Breathe Plasma */
#define RF4_BR_WALL					0x04000000	/* Breathe Force */
#define RF4_BR_MANA					0x08000000	/* Breathe Mana */
#define RF4_BA_NUKE		0x10000000	/* TY: Nuke Ball */
#define RF4_BR_NUKE		0x20000000	/* TY: Toxic Breath */
#define RF4_BA_CHAO		0x40000000	/* TY: Logrus Ball */
#define RF4_BR_DISI					0x80000000 /* Breathe Disintegration */

/*
 * New monster race bit flags
 */
#define RF5_BA_ACID					0x00000001	/* Acid Ball */
#define RF5_BA_ELEC					0x00000002	/* Elec Ball */
#define RF5_BA_FIRE					0x00000004	/* Fire Ball */
#define RF5_BA_COLD					0x00000008	/* Cold Ball */
#define RF5_BA_POIS					0x00000010	/* Poison Ball */
#define RF5_BA_NETH					0x00000020	/* Nether Ball */
#define RF5_BA_WATE					0x00000040	/* Water Ball */
#define RF5_BA_MANA					0x00000080	/* Mana Storm */
#define RF5_BA_DARK					0x00000100	/* Darkness Storm */
#define RF5_DRAIN_MANA		0x00000200	/* Drain Mana */
#define RF5_MIND_BLAST		0x00000400	/* Blast Mind */
#define RF5_BRAIN_SMASH		0x00000800	/* Smash Brain */
#define RF5_CAUSE_1					0x00001000	/* Cause Light Wound */
#define RF5_CAUSE_2					0x00002000	/* Cause Serious Wound */
#define RF5_CAUSE_3					0x00004000	/* Cause Critical Wound */
#define RF5_CAUSE_4					0x00008000	/* Cause Mortal Wound */
#define RF5_BO_ACID					0x00010000	/* Acid Bolt */
#define RF5_BO_ELEC					0x00020000	/* Elec Bolt (unused) */
#define RF5_BO_FIRE					0x00040000	/* Fire Bolt */
#define RF5_BO_COLD					0x00080000	/* Cold Bolt */
#define RF5_CH_SHARD					0x00100000	/* Chain Shards */
#define RF5_BO_NETH					0x00200000	/* Nether Bolt */
#define RF5_BO_WATE					0x00400000	/* Water Bolt */
#define RF5_BO_MANA					0x00800000	/* Mana Bolt */
#define RF5_BO_PLAS					0x01000000	/* Plasma Bolt */
#define RF5_BO_ICEE					0x02000000	/* Ice Bolt */
#define RF5_MISSILE					0x04000000	/* Magic Missile */
#define RF5_SCARE					0x08000000	/* Frighten Player */
#define RF5_BLIND					0x10000000	/* Blind Player */
#define RF5_CONF						0x20000000	/* Confuse Player */
#define RF5_SLOW						0x40000000	/* Slow Player */
#define RF5_HOLD						0x80000000	/* Paralyze Player */

/*
 * New monster race bit flags
 */
#define RF6_HASTE					0x00000001	/* Speed self */
#define RF6_HAND_DOOM				0x00000002	/* Hand of Doom */
#define RF6_HEAL						0x00000004	/* Heal self */
#define RF6_XXX2						0x00000008	/* Heal a lot (?) */
#define RF6_BLINK					0x00000010	/* Teleport Short */
#define RF6_TPORT					0x00000020	/* Teleport Long */
#define RF6_XXX3						0x00000040	/* Move to Player (?) */
#define RF6_BR_SKULL					0x00000080	/* Breathe skulls */
#define RF6_TELE_TO					0x00000100	/* Move player to monster */
#define RF6_TELE_AWAY		0x00000200	/* Move player far away */
#define RF6_TELE_LEVEL		0x00000400	/* Move player vertically */
#define RF6_DARKNESS			0x00000800	/* Create Darkness */
#define RF6_TRAPS					0x00001000	/* Create Traps */
#define RF6_FORGET					0x00002000	/* Cause amnesia */
#define RF6_S_DOOM						0x00004000	/* Summon minions of Id */
#define RF6_S_HI_DOOM					0x00008000	/* Summon powerful minions of Id */
#define RF6_S_KIN					0x00010000	/* Summon "kin" */
#define RF6_S_CYBER					0x00020000	/* Summon Cyberdemons! */
#define RF6_S_MONSTER		0x00040000	/* Summon Monster */
#define RF6_S_MONSTERS		0x00080000	/* Summon Monsters */
#define RF6_S_ANT					0x00100000	/* Summon Ants */
#define RF6_S_SPIDER			0x00200000	/* Summon Spiders */
#define RF6_S_HOUND					0x00400000	/* Summon Hounds */
#define RF6_S_HYDRA					0x00800000	/* Summon Hydras */
#define RF6_S_ANGEL					0x01000000	/* Summon Angel */
#define RF6_S_DEMON					0x02000000	/* Summon Demon */
#define RF6_S_UNDEAD			0x04000000	/* Summon Undead */
#define RF6_S_DRAGON			0x08000000	/* Summon Dragon */
#define RF6_S_HI_UNDEAD		0x10000000	/* Summon Greater Undead */
#define RF6_S_HI_DRAGON		0x20000000	/* Summon Ancient Dragon */
#define RF6_S_WRAITH			0x40000000	/* Summon Unique Wraith */
#define RF6_S_UNIQUE			0x80000000	/* Summon Unique Monster */

