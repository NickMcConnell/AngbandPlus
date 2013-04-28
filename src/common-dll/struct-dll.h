/* File: struct-dll.h */

/* Purpose: read and write C structs and arrays */

/*
 * Copyright (c) 1997-2009 Tim Baker
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#ifndef INCLUDED_STRUCT_H
#define INCLUDED_STRUCT_H

#include "storage.h"

/* This macro should cause an error if the macro 'x' is undefined. */
/* The Tcl headers define STRINGIFY(). */
#define STRINGIFY2(x) (sizeof(x) ? #x : "")

/* "FIELD_OFFSET" clashes with VC++ 6.0 */
#define _FIELD_OFFSET(FIELD, STRUCT) (int) &((STRUCT *) 0L)->FIELD
#define FIELD_SIZE(FIELD, STRUCT) (int) sizeof(((STRUCT *) 0L)->FIELD)
#define FIELD_DESC(FIELD, STRUCT) #FIELD, _FIELD_OFFSET(FIELD, STRUCT), FIELD_SIZE(FIELD, STRUCT)
#define FIELD_SYN(SYN, FIELD, STRUCT) #SYN, _FIELD_OFFSET(FIELD, STRUCT), FIELD_SIZE(FIELD, STRUCT)

enum {
FLD_INT8, FLD_INT8U,
FLD_INT16, FLD_INT16U,
FLD_INT32, FLD_INT32U,
FLD_FLAGS8, FLD_FLAGS16, FLD_FLAGS32,
FLD_STRINGPTR,
FLD_STRING,
FLD_CHAR,
FLD_BOOL,
FLD_CALLBACK, /* t_field.data is a callback routine */
FLD_INDEX8, /* t_field.data is array of strings */
FLD_INDEX16 /* t_field.data is array of strings */
};

/* Constants for t_field.edit field */
enum {
EDIT_NO,
EDIT_YES,
EDIT_CALLBACK
};

/*
 * A single field in a structure
 */
typedef struct t_field t_field;
struct t_field
{
	char *text;		/* Title for list */
	int fOffset;	/* Offset of field */
	int fLength;	/* Size of field */
	unsigned char type; /* Type of item */
	unsigned char edit;	/* Edit this item? */
	long min;		/* Min value */
	long max;		/* Max value */
	void *data;		/* Reference constant (flags, callback) */
};

typedef struct StructType StructType;

/* Function definition for EDIT_CALLBACK */
typedef Tcl_Obj *(*StructToObjProc)(Tcl_Interp *interp, t_field *info,
	void *elem, int index);

typedef int (*StructInfoProc)(Tcl_Interp *interp, StructType *typePtr,
	int objc, Tcl_Obj *CONST objv[], int objOffset, void *elem, int index);

/* Custom search callbacks */
typedef int (*StructFindOptionProc)(Tcl_Interp *interp, StructType *typePtr,
	int objc, Tcl_Obj *CONST objPtr[], int *index);
typedef int (*StructFindMatchProc)(Tcl_Interp *interp, StructType *typePtr,
	void *elem, int index);

/*
 * Info about a struct
 */
struct StructType {
	char *name;
	unsigned char *elem;
	int elem_size;
	int max;
	t_field *info;
	int count;
	StructInfoProc infoProc;
	StructFindOptionProc findProc;
	StructFindMatchProc matchProc;
	Tcl_HashTable *nameTable;
};

DLL_EXTERN int Struct_Init(Tcl_Interp *interp);
DLL_EXTERN void Struct_Exit(Tcl_Interp *interp);
DLL_EXTERN int Struct_AddType(Tcl_Interp *interp, StructType *data);
DLL_EXTERN int Struct_Find(Tcl_Interp *interp, StructType *typePtr, int objc,
	Tcl_Obj *CONST objv[], int objOffset);
DLL_EXTERN int Struct_Flags(Tcl_Interp *interp, StructType *typePtr, int elemIndex,
	int objc, Tcl_Obj *CONST objv[], int objOffset);
DLL_EXTERN int Struct_GetTypeFromObj(Tcl_Interp *interp, StructType **typePtrPtr,
	Tcl_Obj *objPtr);
DLL_EXTERN int Struct_GetArrayIndexFromObj(Tcl_Interp *interp, StructType *typePtr,
	int *elemIndex, Tcl_Obj *objPtr);
DLL_EXTERN int Struct_Info(Tcl_Interp *interp, StructType *typePtr, int elemIndex,
	int objc, Tcl_Obj *CONST objv[], int objOffset);
DLL_EXTERN StructType *Struct_Lookup(Tcl_Interp *interp, char *name);
DLL_EXTERN int Struct_Set(Tcl_Interp *interp, StructType *typePtr, int elemIndex,
	int objc, Tcl_Obj *CONST objv[], int objOffset);

#endif /* INCLUDED_STRUCT_H */

