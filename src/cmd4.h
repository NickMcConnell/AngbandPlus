/* File: cmd4.h */

/*
 * Copyright (c) 1997 Ben Harrison
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.
 */

#ifndef INCLUDED_CMD4_H
#define INCLUDED_CMD4_H

#include "h-basic.h"


/* Replacement hook for "do_cmd_save_screen()" */
extern void (*screendump_aux)(void);


#endif /* INCLUDED_CMD4_H */

