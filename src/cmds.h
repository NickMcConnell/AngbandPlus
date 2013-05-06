#ifndef INCLUDED_CMDS_H
#define INCLUDED_CMDS_H

#include "game-cmd.h"

/* 
 * Command handlers will take a pointer to the command structure
 * so that they can access any arguments supplied.
 */
typedef void (*cmd_handler_fn)(cmd_code code, cmd_arg args[]);


/* Types of item use */
typedef enum
{
	USE_TIMEOUT,
	USE_CHARGE,
	USE_SINGLE
} use_type;


#endif

