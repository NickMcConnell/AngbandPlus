/*
 * File: z-msg.h
 * Purpose: Message handling
 */

#ifndef INCLUDED_Z_MSG_H
#define INCLUDED_Z_MSG_H

/*** Message constants ***/

enum
{
    #define MSG(x, s) MSG_##x,
    #include "list-message.h"
    #undef MSG
};

#endif /* INCLUDED_Z_MSG_H */
