#ifndef MON_CLASSES_H
#define MON_CLASSES_H

#include <src/monster.h>



/*
 * Information about ghost "templates".
 */
class ghost_template
{
public:

    ghost_template();

    QString t_name;	/* Ghost Name */
    byte t_gender;	/* Ghost gender (0 = male  1 = female) */
    byte t_race;	/* Ghost race */
    byte t_class; 	/* Ghost class */
    byte t_depth; 	/* Ghost native depth */

    // Wipe a player_ghost entry.  All variabled above need to be cleared.
    void ghost_template_wipe();
};


/*
 * Monster blow structure
 *
 *	- Method (RBM_*)
 *	- Effect (RBE_*)
 *	- Damage Dice
 *	- Damage Sides
 */
class monster_blow
{
public:
    byte method;
    byte effect;
    byte d_dice;
    byte d_side;
};



/*
 * Monster "race" information, including racial memories
 *
 *
 * Note that "cur_num" (and "max_num") represent the number of monsters
 * of the given race currently on (and allowed on) the current level.
 * This information yields the "dead" flag for Unique monsters.
 *
 * Note that "max_num" is reset when a new player is created.
 * Note that "cur_num" is reset when a new level is created.
 *
 * Maybe "x_attr", "x_char", "cur_num", and "max_num" should
 * be moved out of this array since they are not read from
 * "monster.txt".
 */

class monster_race
{
public:

    monster_race();

    QString r_name_full;
    QString r_name_short;
    QString r_text;				/* Text  */

    byte hdice;				/* Creatures hit dice count */
    byte hside;				/* Creatures hit dice sides */

    s16b ac;				/* Armour Class */

    s16b sleep;				/* Inactive counter (base) */
    byte aaf;				/* Area affect radius (1-100) */
    byte r_speed;			/* Speed (normally 110) */

    s32b mexp;				/* Exp value for kill */

    s16b extra;				/* Unused (for now) */

    byte freq_ranged;		/* Ranged attack frequency */
    byte mana;				/* Max mana */
    byte spell_power;		/* Power of (damage-dealing) spells */
    u32b mon_power;        	/* Monster Power Rating */

    u32b flags1;			/* Flags 1 (general) */
    u32b flags2;			/* Flags 2 (abilities) */
    u32b flags3;			/* Flags 3 (race/resist) */
    u32b flags4;			/* Flags 4 (inate/breath) */
    u32b flags5;			/* Flags 5 (normal spells) */
    u32b flags6;			/* Flags 6 (special spells) */
    u32b flags7;			/* Flags 7 (summon spells) */

    u32b r_native;			/*Terrains where monster is native*/

    monster_blow blow[MONSTER_BLOW_MAX]; /* Up to four blows per round */

    byte level;				/* Level of creature */
    byte rarity;			/* Rarity of creature */

    byte color_num;     //The number of any default color.  CUSTOM_COLOR for all others.
    QColor d_color;         /* Default monster color */
    QChar d_char;			/* Default monster character */

    byte max_num;			/* Maximum population allowed per level */
    byte cur_num;			/* Monster population on current level */

    QString tile_id;

    bool double_height_tile;

    bool is_unique(void);
    bool is_player_ghost(void);
    bool is_mimic(void);

    // Wipe a monster_race entry.  All variabled above need to be cleared.  Intended for player ghosts.
    void monster_race_wipe();

};


/*
 * Monster "lore" information
 *
 * Note that these fields are related to the "monster recall" and can
 * be scrapped if space becomes an issue, resulting in less "complete"
 * monster recall (no knowledge of spells, etc). XXX XXX XXX
 *
 */
class monster_lore
{
public:

    monster_lore();


    s16b sights;			/* Count sightings of this monster */
    s16b deaths;			/* Count deaths from this monster */

    s16b pkills;			/* Count monsters killed in this life */
    s16b tkills;			/* Count monsters killed in all lives */

    byte wake;				/* Number of times woken up (?) */
    byte ignore;			/* Number of times ignored (?) */

    byte xtra1;				/* Something (unused) */
    byte xtra2;				/* Something (unused) */

    byte drop_gold;			/* Max number of gold dropped at once */
    byte drop_item;			/* Max number of item dropped at once */

    byte ranged;			/* Observed ranged attacks */

    byte blows[MONSTER_BLOW_MAX]; /* Number of times each blow type was seen */

    u32b r_l_flags1;			/* Observed racial flags */
    u32b r_l_flags2;			/* Observed racial flags */
    u32b r_l_flags3;			/* Observed racial flags */
    u32b r_l_flags4;			/* Observed racial flags */
    u32b r_l_flags5;			/* Observed racial flags */
    u32b r_l_flags6;			/* Observed racial flags */
    u32b r_l_flags7;			/* Observed racial flags */

    u32b r_l_native;			/* Observed Nativity Flags*/

    // Wipe a monster_lore entry.  All variabled above need to be cleared.
    void monster_lore_wipe();
};


/*
 * Monster information, for a specific monster.
 *
 * Note: fy, fx constrain dungeon size to 256x256
 *
 * The "hold_o_idx" field points to the first object of a stack
 * of objects (if any) being carried by the monster (see above).
 */
class monster_type
{
public:

    monster_type();

    s16b r_idx;			/* Monster race index */

    byte fy;			/* Y location on map */
    byte fx;			/* X location on map */

    s16b hp;			/* Current Hit points */
    s16b maxhp;			/* Max Hit points */

    byte m_speed;		/* Monster "speed" */
    s16b m_energy;		/* Monster "energy" */

    s16b m_timed[MON_TMD_MAX];

    byte cdis;			/* Current dis from player */

    u32b mflag;			/* Extra monster flags */
    QColor m_color;			/* used only for shimmering monsters - current color */
    bool ml;			/* Monster is "visible" */
    bool project;		/* Player projectables can hit the monster (not quite the same as
                            being visible. */

    bool sidebar;		/* Monster is being displayed on the sidebar */

    s16b hold_o_idx;	/* Object being held (if any) */

    u32b smart;			/* Field for "smart_learn" */

    byte target_y;		/* Monster target */
    byte target_x;

    byte min_range;		/* What is the closest we want to be? */  /* Not saved */
    byte best_range;	/* How close do we want to be? */  /* Not saved */

    byte mana;          /* Current mana level */

    byte using_flow;	/*Which movement flow is the creature using?*/

    // Wipe a monster_type entry.  All variabled above need to be cleared.
    void monster_wipe();


    s16b get_mon_idx();
    bool mon_fully_healthy();


};

/*
 * Structure for building monster "lists"
 */
class move_moment_type
{
public:

    s16b m_idx;
    s16b moment;
};


/*
 * A stacked monster message entry
 */
class monster_race_message
{
public:

    s16b mon_race;		/* The race of the monster */
    byte mon_flags;		/* Flags: 0x01 means hidden monster, 0x02 means offscreen monster */
    s16b  msg_code;		/* The coded message */
    byte mon_count;		/* How many monsters triggered this message */
};

class monster_message_history
{
public:

    s16b monster_idx;	/* The monster */
    s16b message_code;		/* The coded message */
};


#endif // MON_CLASSES_H
