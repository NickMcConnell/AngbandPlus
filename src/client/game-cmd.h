/*
 * File: game-cmd.h
 * Purpose: Game commands
 */

#ifndef INCLUDED_GAME_CMD_H
#define INCLUDED_GAME_CMD_H

/*
 * All valid game commands.
 */
typedef enum cmd_code
{
    /* A "do nothing" command */
    CMD_NULL = 0,

    /* 
     * The main game commands
     */
    CMD_GO_UP,
    CMD_GO_DOWN,
    CMD_SEARCH,
    CMD_TOGGLE_SEARCH,
    CMD_WALK,
    CMD_JUMP,
    CMD_INSCRIBE,
    CMD_UNINSCRIBE,
    CMD_TAKEOFF,
    CMD_WIELD,
    CMD_DROP,
    CMD_BROWSE_SPELL,
    CMD_STUDY_BOOK,
    CMD_CAST,
    CMD_USE_STAFF,
    CMD_USE_WAND,
    CMD_USE_ROD,
    CMD_ACTIVATE,
    CMD_EAT,
    CMD_QUAFF,
    CMD_READ_SCROLL,
    CMD_REFILL,
    CMD_FIRE,
    CMD_THROW,
    CMD_PICKUP,
    CMD_AUTOPICKUP,
    CMD_DESTROY,
    CMD_DISARM,
    CMD_TUNNEL,
    CMD_OPEN,
    CMD_CLOSE,
    CMD_JAM,
    CMD_BASH,
    CMD_RUN,
    CMD_HOLD,
    CMD_ALTER,
    CMD_USE_ANY,
    CMD_QUIT,

    /* PWMAngband */
    CMD_POLY,
    CMD_BREATH,
    CMD_PROJECT,
    CMD_FOUNTAIN,
    CMD_DROP_GOLD,
    CMD_STEAL,
    CMD_EXAMINE
} cmd_code;

#define DIR_SKIP    -1
#define DIR_UNKNOWN 0

typedef union
{
    char *string;
    int number;
    int item;
    int direction;
} cmd_arg;

/* Maximum number of arguments a command needs to take. */
#define CMD_MAX_ARGS 2

enum cmd_arg_type
{
    arg_NONE = 0,
    arg_STRING,
    arg_NUMBER,
    arg_ITEM,
    arg_DIRECTION
};

/* Called by the game engine to get the player's next action. */
extern void process_command(cmd_code cmd);

#endif
