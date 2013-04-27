/* Nothing */
#define FEAT_NONE						0x00

/* Various */
#define FEAT_FLOOR					0x01
#define FEAT_INVIS					0x02
#define FEAT_GLYPH					0x03
#define FEAT_OPEN						0x04
#define FEAT_BROKEN					0x05
#define FEAT_LESS						0x06
#define FEAT_MORE						0x07

/* Traps */
#define FEAT_TRAP_DOOR				0x10
#define FEAT_TRAP_PIT				0x11
#define FEAT_TRAP_SPIKED_PIT		0x12
#define FEAT_TRAP_POISON_PIT		0x13
#define FEAT_TRAP_TY_CURSE			0x14
#define FEAT_TRAP_TELEPORT			0x15
#define FEAT_TRAP_FIRE				0x16
#define FEAT_TRAP_ACID				0x17
#define FEAT_TRAP_SLOW				0x18
#define FEAT_TRAP_LOSE_STR			0x19
#define FEAT_TRAP_LOSE_DEX			0x1A
#define FEAT_TRAP_LOSE_CON			0x1B
#define FEAT_TRAP_BLIND				0x1C
#define FEAT_TRAP_CONFUSE			0x1D
#define FEAT_TRAP_POISON			0x1E
#define FEAT_TRAP_SLEEP				0x1F

/* Doors */
#define FEAT_DOOR_CLOSED			0x20
#define FEAT_DOOR_LOCK_1			0x21
#define FEAT_DOOR_LOCK_2			0x22
#define FEAT_DOOR_LOCK_3			0x23
#define FEAT_DOOR_LOCK_4			0x24
#define FEAT_DOOR_LOCK_5			0x25
#define FEAT_DOOR_LOCK_6			0x26
#define FEAT_DOOR_LOCK_7			0x27
#define FEAT_DOOR_JAM_1				0x28
#define FEAT_DOOR_JAM_2				0x29
#define FEAT_DOOR_JAM_3				0x2A
#define FEAT_DOOR_JAM_4				0x2B
#define FEAT_DOOR_JAM_5				0x2C
#define FEAT_DOOR_JAM_6				0x2D
#define FEAT_DOOR_JAM_7				0x2E
#define FEAT_DOOR_JAM_8				0x2F

/* Extra */
#define FEAT_SECRET					0x30
#define FEAT_RUBBLE					0x31

/* Seams */
#define FEAT_MAGMA					0x32
#define FEAT_QUARTZ					0x33
#define FEAT_MAGMA_H					0x34
#define FEAT_QUARTZ_H				0x35
#define FEAT_MAGMA_K					0x36
#define FEAT_QUARTZ_K				0x37

/* Walls */
#define FEAT_WALL_EXTRA 			0x38
#define FEAT_WALL_INNER 			0x39
#define FEAT_WALL_OUTER 			0x3A
#define FEAT_WALL_SOLID 			0x3B
#define FEAT_PERM_EXTRA 			0x3C
#define FEAT_PERM_INNER 			0x3D
#define FEAT_PERM_OUTER 			0x3E
#define FEAT_PERM_SOLID 			0x3F

/* Explosive rune */
#define FEAT_MINOR_GLYPH			0x40

/* The Pattern */
#define FEAT_PATTERN_START 		0x41
#define FEAT_PATTERN_1				0x42
#define FEAT_PATTERN_2				0x43
#define FEAT_PATTERN_3				0x44
#define FEAT_PATTERN_4				0x45
#define FEAT_PATTERN_END			0x46
#define FEAT_PATTERN_OLD			0x47
#define FEAT_PATTERN_XTRA1 		0x48
#define FEAT_PATTERN_XTRA2 		0x49

/* Shops */
#define FEAT_SHOP_GENERAL			0x4A
#define FEAT_SHOP_ARMOR				0x4B
#define FEAT_SHOP_WEAPON			0x4C
#define FEAT_SHOP_TEMPLE			0x4D
#define FEAT_SHOP_ALCHEMIST		0x4E
#define FEAT_SHOP_MAGIC				0x4F
#define FEAT_SHOP_HOME				0x50
#define FEAT_SHOP_BLACK_MARKET	0x51
#define FEAT_SHOP_BOOKSTORE		0x52

/* Flavor floor terrain */
#define FEAT_FLOOR_MOSS				0x53
#define FEAT_FLOOR_LICHEN			0x54

/* Crushing ceiling (up) */
#define FEAT_CRUSHER_UP				0x55

/* Crusher trap */
#define FEAT_TRAP_CRUSHER			0x56

/* Flavor wall terrain */
#define FEAT_WALL_MOSS				0x60
#define FEAT_WALL_LICHEN			0x61

/* Crushing ceiling (down) */
#define FEAT_CRUSHER_DOWN			0x62

/* Barrel of goo */
#define FEAT_BARREL_GOO				0x63

/* Flaming barrel */
#define FEAT_BARREL_FLAMING		0x64

/* Bloody walls */
#define FEAT_WALL_RED				0x65
#define FEAT_WALL_GREEN 			0x66
#define FEAT_WALL_DARK				0x67

/* Bloody floors */
#define FEAT_FLOOR_RED				0x80
#define FEAT_FLOOR_GREEN			0x81
#define FEAT_FLOOR_DARK 			0x82

/* Liquids */
#define FEAT_WATER					0x83
#define FEAT_LAVA 					0x84



/*** Feature tests ***/

#define FTYPE_FLOOR			0x00000001L 	/* may accept objects */
#define FTYPE_GROUND 		0x00000002L 	/* may be walked on */
#define FTYPE_SOFT_WALL 	0x00000004L 	/* may be tunneled through */
#define FTYPE_HARD_WALL 	0x00000008L 	/* may not be tunneled through */
#define FTYPE_WALL			0x00000010L 	/* blocks vision */
#define FTYPE_GRANITE		0x00000020L 	/* is normal granite */
#define FTYPE_VEIN			0x00000040L 	/* is a magma or quartz vein */
#define FTYPE_TREASURE		0x00000080L 	/* contains treasure */
#define FTYPE_DOOR			0x00000100L 	/* is a door */
#define FTYPE_SHOP			0x00000200L 	/* is a shop */
#define FTYPE_STAIR			0x00000400L 	/* is a stair */
#define FTYPE_TRAP			0x00000800L 	/* is a trap */
#define FTYPE_BLOOD			0x00001000L 	/* is a bloodstained feature */
#define FTYPE_PATTERN		0x00002000L 	/* is the Pattern */
#define FTYPE_CRUSHER		0x00004000L 	/* is a crushing ceiling */
#define FTYPE_LIQUID 		0x00008000L 	/* is a liquid */
#define FTYPE_BARREL 		0x00010000L 	/* is a barrel */
#define FTYPE_NOMONSTER 	0x00020000L 	/* blocks monsters */
#define FTYPE_NOPLAYER		0x00040000L 	/* blocks the player */

#define is_floor(F) \
	((feat_test[F] & FTYPE_FLOOR) != 0)

#define is_ground(F) \
	((feat_test[F] & FTYPE_GROUND) != 0)

#define is_soft_wall(F) \
	((feat_test[F] & FTYPE_SOFT_WALL) != 0)

#define is_hard_wall(F) \
	((feat_test[F] & FTYPE_HARD_WALL) != 0)

#define is_wall(F) \
	((feat_test[F] & FTYPE_WALL) != 0)

#define is_granite(F) \
	((feat_test[F] & FTYPE_GRANITE) != 0)

#define is_vein(F) \
	((feat_test[F] & FTYPE_VEIN) != 0)

#define is_treasure(F) \
	((feat_test[F] & FTYPE_TREASURE) != 0)

#define is_door(F) \
	((feat_test[F] & FTYPE_DOOR) != 0)

#define is_shop(F) \
	((feat_test[F] & FTYPE_SHOP) != 0)

#define is_stair(F) \
	((feat_test[F] & FTYPE_STAIR) != 0)

#define is_trap(F) \
	((feat_test[F] & FTYPE_TRAP) != 0)

#define is_blood(F) \
	((feat_test[F] & FTYPE_BLOOD) != 0)

#define is_pattern(F) \
	((feat_test[F] & FTYPE_PATTERN) != 0)

#define is_crusher(F) \
	((feat_test[F] & FTYPE_CRUSHER) != 0)

#define is_liquid(F) \
	((feat_test[F] & FTYPE_LIQUID) != 0)

#define is_barrel(F) \
	((feat_test[F] & FTYPE_BARREL) != 0)

#define is_nomonster(F) \
	((feat_test[F] & FTYPE_NOMONSTER) != 0)

#define is_noplayer(F) \
	((feat_test[F] & FTYPE_NOPLAYER) != 0)


/*
 * Determines if a map location is "meaningful"
 */
#define in_bounds(Y,X) \
	(((unsigned)(Y) < (unsigned)(cur_hgt)) && \
	 ((unsigned)(X) < (unsigned)(cur_wid)))

/*
 * Determines if a map location is fully inside the outer walls
 * This is more than twice as expensive as "in_bounds()", but
 * often we need to exclude the outer walls from calculations.
 */
#define in_bounds_fully(Y,X) \
	(((Y) > 0) && ((Y) < cur_hgt-1) && \
	 ((X) > 0) && ((X) < cur_wid-1))

/*
 * Determines if a map location is currently "on screen" -RAK-
 * Note that "panel_contains(Y,X)" always implies "in_bounds2(Y,X)".
 */
#define panel_contains(Y,X) \
  (((Y) >= panel_row_min) && ((Y) <= panel_row_max) && \
	((X) >= panel_col_min) && ((X) <= panel_col_max))

/*
 * Determine if a bolt from (x1, y1) to (x2, y2) will reach its target
 */
#define projectable(y1, x1, y2, x2) \
	(project_test(y1, x1, y2, x2, project_in_range, project_simple, project_fail))

/*
 * Determine if a legal grid is a "floor" grid
 */
#define cave_floor(Y,X) \
	(!is_wall(cave_feat[Y][X]))

/*
 * Determine if a 'legal' grid is a 'barren' floor grid
 */
#define cave_barren(Y,X) \
	((cave_feat[Y][X] == FEAT_FLOOR) || (cave_feat[Y][X] == FEAT_CRUSHER_UP))

/*
 * Determine if a legal grid contains an object
 */
#define cave_object(Y,X) \
	(cave_o_idx[Y][X])

/*
 * Determine if a legal grid is good for placing objects on
 * Must be a barren floor without any current objects
 */
#define cave_clean(Y,X) \
	(cave_barren(Y,X) && !cave_object(Y,X))

/*
 * Determine if a legal grid contains a normal monster
 */
#define cave_monster(Y,X) \
	(cave_m_idx[Y][X] > 0)

/*
 * Determine if a monster is friendly
 */
#define is_friend(M) \
	((m_list[M].smart & (SM_FRIEND)) != 0)

/*
 * Determine if a legal grid contains a friendly monster
 */
#define cave_friend(Y,X) \
	(cave_monster(Y,X) && is_friend(cave_m_idx[Y][X]))

/*
 * Determine if a legal grid contains a hostile monster
 */
#define cave_hostile(Y,X) \
	(cave_monster(Y,X) && !is_friend(cave_m_idx[Y][X]))

/*
 * Determine if a legal grid contains the player
 */
#define cave_player(Y,X) \
	(cave_m_idx[Y][X] < 0)

/*
 * Determine if a legal grid does not contain a creature
 */
#define cave_dead(Y,X) \
	(!cave_m_idx[Y][X])

/*
 * Determine if a legal grid is an empty floor grid
 * Must be a floor without any creatures
 */
#define cave_empty(Y,X) \
	(cave_floor(Y,X) && cave_dead(Y,X))

/*
 * Determine if a "legal" grid is an "naked" floor grid
 * Must be a barren floor without objects or creatures
 */
#define cave_naked(Y,X) \
	(cave_clean(Y,X) && cave_dead(Y,X))

/*
 * Determine if the grid is known
 */
#define cave_mark(Y,X) \
	((cave_info[Y][X] & (CAVE_MARK)) != 0)

/*
 * Determine if the grid is magically lit
 */
#define cave_glow(Y,X) \
	((cave_info[Y][X] & (CAVE_GLOW)) != 0)

/*
 * Determine if the grid is 'icky'
 */
#define cave_icky(Y,X) \
	((cave_info[Y][X] & (CAVE_ICKY)) != 0)

/*
 * Determine if the grid is in a room
 */
#define cave_room(Y,X) \
	((cave_info[Y][X] & (CAVE_ROOM)) != 0)

/*
 * Determine if a "legal" grid is within "los" of the player
 */
#define player_has_los(Y,X) \
	((cave_info[Y][X] & (CAVE_VIEW)) != 0)

/*
 * Determine if a "legal" grid can be "seen" by the player
 */
#define player_can_see(Y,X) \
	((cave_info[Y][X] & (CAVE_SEEN)) != 0)

/*
 * Determine if the grid is torch-lit
 */
#define cave_torch(Y,X) \
	(player_can_see(Y, X) && !cave_glow(Y, X))

/*
 * Determine if the player has a light source
 */
#define player_has_no_lite \
	(!player_can_see(p_ptr->py, p_ptr->px))

/*
 * Determine if the player is a wraith
 */
#define player_is_wraith \
	((p_ptr->wraith_form) || (p_ptr->prace == RACE_SPECTRE))

