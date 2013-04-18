/* File: borg9.h */
/* Purpose: Header file for "borg9.c" -BEN- */

#ifndef INCLUDED_BORG9_H
#define INCLUDED_BORG9_H

#include "angband.h"

#ifdef ALLOW_BORG

/*
 * This file provides support for "borg9.c".
 */
#ifndef BABLOS
extern void resurrect_borg(void);
extern void borg_write_map(bool ask);
extern errr borg_enter_score(void);

#endif /* bablos */

extern void borg_status(void);
/*
 * Initialize this file
 */
extern void borg_init_9(void);


#endif

#endif

