#ifndef PLAYER_COMMAND_H
#define PLAYER_COMMAND_H


#include <src/defines.h>
#include <src/object_classes.h>


#define ARG_NUMBER          0x01
#define ARG_ITEM            0x02
#define ARG_DIRECTION       0x04
#define ARG_SLOT    		0x08
#define ARG_SPECIAL         0x10 // Needs a special dialog




//Player "rest_modes"
enum
{
    REST_BOTH_SP_HP = 1,
    REST_COMPLETE,
    REST_HP,
    REST_SP,
    REST_TURNCOUNT

};

//Player "commands"
// Any new command needs to be added to command_info structure below
enum
{
    CMD_NONE = 0,
    CMD_RESTING,
    CMD_RUNNING,
    CMD_WALK,
    CMD_JUMP,
    CMD_OPEN,
    CMD_CLOSE,
    CMD_SPIKE,
    CMD_DISARM,
    CMD_BASH,
    CMD_TUNNEL,
    CMD_ALTER,
    CMD_SEARCH,
    CMD_MAKE_TRAP,
    CMD_HOLD,
    CMD_TAKEOFF,
    CMD_WIELD,
    CMD_SWAP,
    CMD_ITEM_USE,
    CMD_REFUEL,
    CMD_FIRE,
    CMD_FIRE_NEAR,
    CMD_DROP,
    CMD_PICKUP,
    CMD_BROWSE,
    CMD_STUDY,
    CMD_CAST,
    CMD_DESTROY,
    CMD_EXAMINE,
    CMD_INSCRIBE,
    CMD_UNINSCRIBE,
    CMD_ACTIVATE,
    CMD_THROW,
    CMD_SETTINGS,
    CMD_MAX
};


class command_type
{

public:
    byte cmd_needs;                         // which arguments does the function need?
    void (*command_function)(cmd_arg args);  //What function should be called for this command?
    bool default_verify;                // What should be default verify be to process the command correctly.
    bool repeat_allowed;                    // Is repeating allowed?
    u16b repeat_num;                       // Automatic number of repeats.

    bool repeated_command_completed(void);
    bool keep_direction(void);
    bool needs_direction(void);
    bool needs_item(void);
    bool needs_quantity(void);
    bool needs_slot(void);
    bool is_special(void);
    QString prompt(int command);
    cmd_arg find_slot(object_type *o_ptr, cmd_arg args, int command);

};

extern command_type command_info[CMD_MAX];

extern void process_command(int item, s16b command);

#endif // PLAYER_COMMAND_H
