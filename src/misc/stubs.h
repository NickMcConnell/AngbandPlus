/* File: stubs.h */

/* Purpose: stubs stuff */

/*
 * Copyright (c) 1997-2009 Tim Baker
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#ifndef _INCLUDE_STUBS_H_
#define _INCLUDE_STUBS_H_

#include "shlib.h"

#define STUB_OFFSET(FIELD, STRUCT) (int) &((STRUCT *) 0L)->FIELD
#define STUB_SIZE(FIELD, STRUCT) (int) sizeof(((STRUCT *) 0L)->FIELD)
#define STUB_DESC(FIELD, STRUCT) #FIELD, STUB_OFFSET(FIELD, STRUCT), STUB_SIZE(FIELD, STRUCT)

typedef struct t_stub t_stub;
struct t_stub
{
	char *name; /* Symbol name */
	int offset; /* Offset in struct */
	int length; /* Length of field in struct */
};

extern int Stub_Load(ShLibRef shLib, t_stub *stub, void *data);
extern char *Stub_Error(void);

#endif /* _INCLUDE_STUBS_H_ */
