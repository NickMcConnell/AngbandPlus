/*
 * File: cmd-core.h
 * Purpose: Handles the queueing of game commands
 */

#ifndef INCLUDED_CMD_CORE_H
#define INCLUDED_CMD_CORE_H

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
    CMD_TOGGLE_STEALTH,
    CMD_WALK,
    CMD_JUMP,
    CMD_INSCRIBE,
    CMD_UNINSCRIBE,
    CMD_TAKEOFF,
    CMD_WIELD,
    CMD_DROP,
    CMD_BROWSE_SPELL,
    CMD_STUDY,
    CMD_CAST,
    CMD_USE_STAFF,
    CMD_USE_WAND,
    CMD_USE_ROD,
    CMD_ACTIVATE,
    CMD_EAT,
    CMD_QUAFF,
    CMD_READ_SCROLL,
    CMD_REFILL,
    CMD_USE,
    CMD_FIRE,
    CMD_THROW,
    CMD_PICKUP,
    CMD_AUTOPICKUP,
    CMD_IGNORE,
    CMD_DISARM,
    CMD_TUNNEL,
    CMD_OPEN,
    CMD_CLOSE,
    CMD_RUN,
    CMD_HOLD,
    CMD_ALTER,

    /* PWMAngband */
    CMD_POLY,
    CMD_BREATH,
    CMD_PROJECT,
    CMD_FOUNTAIN,
    CMD_DROP_GOLD,
    CMD_STEAL,
    CMD_EXAMINE,

    CMD_MAX
} cmd_code;

#define DIR_SKIP    -1
#define DIR_UNKNOWN 0

/*
 * Argument structures
 */

/*
 * The data of the argument
 */
union cmd_arg_data
{
    char *string;
    int number;
    struct object *obj;
    int direction;
};

/*
 * The type of the data
 */
enum cmd_arg_type
{
    arg_NONE = 0,
    arg_STRING,
    arg_NUMBER,
    arg_ITEM,
    arg_DIRECTION
};

/*
 * A single argument
 */
struct cmd_arg
{
    enum cmd_arg_type type;
    union cmd_arg_data data;
    char name[20];  /* Better than dynamic allocation */
};

/*
 * Maximum number of arguments a command needs to take.
 */
#define CMD_MAX_ARGS 2

/*
 * The struct command type is used to return details of the command the
 * game should carry out.
 *
 * 'command' should always have a valid cmd_code value, the other entries
 * may or may not be significant depending on the command being returned.
 */
struct command
{
    /* A valid command code. */
    cmd_code code;

    /* Arguments */
    struct cmd_arg arg[CMD_MAX_ARGS];
};

/*
 * Return codes for cmd_get_arg()
 */
enum cmd_return_codes
{
    CMD_OK = 0,
    CMD_ARG_NOT_PRESENT = -1,
    CMD_ARG_WRONG_TYPE = -2,
    CMD_ARG_ABORTED = -3
};

/* Called by the game engine to get the player's next action. */
extern void process_command(cmd_code ctx);

/* Set the args of a command */
extern void cmd_set_arg_string(struct command *cmd, const char *arg, const char *str);
extern void cmd_set_arg_number(struct command *cmd, const char *arg, int amt);
extern void cmd_set_arg_item(struct command *cmd, const char *arg, struct object *obj);
extern void cmd_set_arg_target(struct command *cmd, const char *arg, int target);

/* Get the args of a command */
extern int cmd_get_arg_string(struct command *cmd, const char *arg, const char **str);
extern int cmd_get_arg_number(struct command *cmd, const char *arg, int *amt);
extern int cmd_get_arg_item(struct command *cmd, const char *arg, struct object **obj);
extern int cmd_get_arg_target(struct command *cmd, const char *arg, int *target);

/* Get the args of a command, using the UI if none passed (or invalid) */
extern int cmd_get_string(struct command *cmd, const char *arg, const char **str,
    const char *initial, const char *title, const char *prompt);
extern int cmd_get_quantity(struct command *cmd, const char *arg, int *amt, int max, bool plural);
extern int cmd_get_item(struct command *cmd, const char *arg, struct object **obj,
    const char *prompt, const char *reject, item_tester filter, int mode);
extern int cmd_get_target(struct command *cmd, const char *arg, int *target);

#endif /* INCLUDED_CMD_CORE_H */
