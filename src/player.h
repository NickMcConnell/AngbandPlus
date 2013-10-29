
/*
	This file should contain the enums and externs for accessing the
	variables previously accessed through the dreadful global p_ptr.
*/

/* ??? wrong already?  could be HP with an option to change max vs current? */

enum player_field_enum
{
	PLAYER_MIN_STAT,
	PLAYER_MAX_STAT = PLAYER_MIN_STAT + A_MAX - 1,
	PLAYER_HP,

	PLAYER_NUM_FIELDS
};

/*
enum player_which_field_enum
{
	PLAYER_MAX,
	PLAYER_CUR,
	PLAYER_MAX_AND_CUR,
};
*/

typedef enum player_field_enum player_field;
typedef enum player_which_field_enum player_which;

extern void player_set_max(player_field f, s32b value);

/* Presumably there should also be player_get and player_increase */

