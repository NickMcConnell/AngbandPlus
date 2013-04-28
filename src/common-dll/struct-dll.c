/* File: struct-dll.c */

/* Purpose: read and write C structs and arrays */

/*
 * Copyright (c) 1997-2009 Tim Baker
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include <string.h>
#include <tcl.h>
#include "cmdinfo-dll.h"
#include "util-dll.h"
#include "struct-dll.h"
#include "dbwin.h"

StructType *g_struct = NULL;
int g_struct_count;

/* Map struct type by name to g_struct[] index */
Tcl_HashTable g_struct_hash;
static int initialized = 0;

Tcl_Obj *Struct_FieldToObj(Tcl_Interp *interp, t_field *info, void *data,
	int elemIndex)
{
	char buf[512];
	int i;
	long n = 0;
	int doInt = 0;
	int doString = 0;

	if (info->edit == EDIT_CALLBACK)
	{
		StructToObjProc callback = (StructToObjProc) info->data;
		return (*callback)(interp, info, data, elemIndex);
	}

	/* Point to the field data */
	data = (unsigned char *) data + info->fOffset;

	switch (info->type)
	{
		case FLD_INT8:
			n = (int) *((char *) data);
			doInt = 1;
			break;

		case FLD_INT8U:
			n = (int) *((unsigned char *) data);
			doInt = 1;
			break;

		case FLD_INT16:
			n = (int) *((short *) data);
			doInt = 1;
			break;

		case FLD_INT16U:
			n = (int) *((unsigned short *) data);
			doInt = 1;
			break;

		case FLD_INT32:
			n = (long) *((long *) data);
			doInt = 1;
			break;

		case FLD_INT32U:
			(void) sprintf(buf, "%lu", (unsigned long) *((unsigned long *) data));
			doString = 1;
			break;

		case FLD_FLAGS8:
#if 1
			(void) sprintf(buf, "0x%02X", (int) *((unsigned char *) data));
#else
			(void) sprintf(buf, "%#10.2X", (int) *((unsigned char *) data));
#endif
			doString = 1;
			break;

		case FLD_FLAGS16:
#if 1
			(void) sprintf(buf, "0x%04X", (unsigned short) *((unsigned short *) data));
#else
			(void) sprintf(buf, "%#10.4X", (unsigned short) *((unsigned short *) data));
#endif
			doString = 1;
			break;

		case FLD_FLAGS32:
#if 1
			(void) sprintf(buf, "0x%08lX", (unsigned long) *((unsigned long *) data));
#else
			(void) sprintf(buf, "%#10.8lX", (unsigned long) *((unsigned long *) data));
#endif
			doString = 1;
			break;

		case FLD_STRINGPTR:
			(void) sprintf(buf, "%s", (char *) *((char **) data));
			doString = 1;
			break;

		case FLD_STRING:
			(void) sprintf(buf, "%s", (char *) data);
			doString = 1;
			break;

		case FLD_BOOL:
			(void) sprintf(buf, "%s", *((unsigned char *) data) ? "TRUE" : "FALSE");
			doString = 1;
			break;

		case FLD_CHAR:
			(void) sprintf(buf, "%c", (char) *((char *) data));
			doString = 1;
			break;

		case FLD_CALLBACK:
			break;

		case FLD_INDEX8:
			i = *(unsigned char *) data;
			if ((i < 0) || (i >= info->max))
				(void) sprintf(buf, "FLD_INDEX8 %d", i);
			else
				(void) strcpy(buf, ((char **) info->data)[i]);
			doString = 1;
			break;

		case FLD_INDEX16:
			i = *(short *) data;
			if ((i < 0) || (i >= info->max))
				(void) sprintf(buf, "FLD_INDEX16 %d", i);
			else
				(void) strcpy(buf, ((char **) info->data)[i]);
			doString = 1;
			break;
	}

	if (doInt)
		return Tcl_NewLongObj(n);
	if (doString)
		return Tcl_NewStringObj(buf, -1);

	/* Error! */
	return Tcl_NewStringObj("???", -1);
}

/* set_info_string */
int Struct_ObjToField(Tcl_Interp *interp, Tcl_Obj *objPtr, t_field *info,
	void *data, int index)
{
	char *t;
	int intValue, type;
	long longValue;
	unsigned long uLongValue;

	/* Paranoia */
	if (!info->edit) return TCL_ERROR;

	type = info->type;

	/* Sometimes type is not FLD_CALLBACK */
	if (info->edit == EDIT_CALLBACK) type = FLD_CALLBACK;

	/* Point to the field data */
	data = (unsigned char *) data + info->fOffset;

	switch (info->type)
	{
		case FLD_INT8:
			if (Tcl_GetIntFromObj(interp, objPtr, &intValue) != TCL_OK)
				return TCL_ERROR;
			*(char *) data = (char) intValue;
			return TCL_OK;

		case FLD_INT8U:
			if (Tcl_GetIntFromObj(interp, objPtr, &intValue) != TCL_OK)
				return TCL_ERROR;
			*(unsigned char *) data = (unsigned char) intValue;
			return TCL_OK;

		case FLD_INT16:
			if (Tcl_GetIntFromObj(interp, objPtr, &intValue) != TCL_OK)
				return TCL_ERROR;
			*(short *) data = (short) intValue;
			return TCL_OK;

		case FLD_INT16U:
			if (Tcl_GetLongFromObj(interp, objPtr, &longValue) != TCL_OK)
				return TCL_ERROR;
			*(unsigned short *) data = (unsigned short) longValue;
			return TCL_OK;

		case FLD_INT32:
			if (Tcl_GetLongFromObj(interp, objPtr, &longValue) != TCL_OK)
				return TCL_ERROR;
			*(long *) data = (long) longValue;
			return TCL_OK;

		case FLD_INT32U:
			t = Tcl_GetString(objPtr);
			if (sscanf(t, "%lu", &uLongValue) != 1) return TCL_ERROR;
			*(unsigned long *) data = uLongValue;
			return TCL_OK;

		case FLD_FLAGS8:
			break;

		case FLD_FLAGS16:
			break;

		case FLD_FLAGS32:
			break;

		case FLD_STRINGPTR:
			break;

		case FLD_STRING:
			break;

		case FLD_BOOL:
			if (Tcl_GetBooleanFromObj(interp, objPtr, &intValue) != TCL_OK) break;
			*(unsigned char *) data = (unsigned char) intValue;
			return TCL_OK;

		case FLD_CHAR:
			t = Tcl_GetString(objPtr);
			if (!t[0]) return TCL_ERROR;
			*(char *) data = t[0];
			return TCL_OK;

		case FLD_CALLBACK:
			break;

		case FLD_INDEX8:
			if (Tcl_GetIndexFromObj(interp, objPtr, (CONST char **) info->data,
				"option", 0, &intValue) != TCL_OK) break;
			*(unsigned char *) data = intValue;
			return TCL_OK;

		case FLD_INDEX16:
			if (Tcl_GetIndexFromObj(interp, objPtr, (CONST char **) info->data,
				"option", 0, &intValue) != TCL_OK) break;
			*(unsigned short *) data = intValue;
			return TCL_OK;
	}

	return TCL_ERROR;
}

/* info_to_list */
Tcl_Obj *Struct_ToList(Tcl_Interp *interp, t_field info[], void *data,
	int elemIndex)
{
	Tcl_Obj *listObjPtr;
	int i;

	/* Create an empty list object */
	listObjPtr = Tcl_NewListObj(0, NULL);

	/* Check each field */
	for (i = 0; info[i].text; i++)
	{
		/* Field name */
		Tcl_ListObjAppendElement(interp, listObjPtr,
			Tcl_NewStringObj((char *) info[i].text, -1));

		/* Field value */
		Tcl_ListObjAppendElement(interp, listObjPtr,
			Struct_FieldToObj(interp, &info[i], data, elemIndex));
	}

	/* Return the list object */
	return listObjPtr;
}

/* info_2_hash */
int Struct_ToHash(t_field *info, Tcl_HashTable **tablePtrPtr)
{
	Tcl_HashTable *tablePtr;
	Tcl_HashEntry *hPtr;
	int i, new;

	/*
	 * Create a new hash table that maps field names to field
	 * info.
	 */
	tablePtr = (Tcl_HashTable *) Tcl_Alloc(sizeof(Tcl_HashTable));
	Tcl_InitHashTable(tablePtr, TCL_STRING_KEYS);

	/* Iterate over each field */
	for (i = 0; info[i].text; i++)
	{
		/* Create a new hash table entry with key = field name */
		hPtr = Tcl_CreateHashEntry(tablePtr, info[i].text, &new);

		/* Set the hash table entry with value = field index */
		Tcl_SetHashValue(hPtr, i);
	}

	(*tablePtrPtr) = tablePtr;

	/* Success */
	return 0;
}

/*
 * Convert a field name into the corresponding field index.
 * Return an error if the field name is unknown.
 */
/* field_name2index */
static int Struct_FindFieldByName(Tcl_Interp *interp, char *fieldName, Tcl_HashTable *tablePtr, int *fieldIndex)
{
	Tcl_HashEntry *hPtr;

	if ((hPtr = Tcl_FindHashEntry(tablePtr, fieldName)) == NULL)
	{
		/* Set the error */
		FormatResult(interp, "unknown field \"%s\"", fieldName);

		/* Failure */
		return TCL_ERROR;
	}

	/* Return the field index */
	(*fieldIndex) = (int) Tcl_GetHashValue(hPtr);

	/* Success */
	return TCL_OK;
}

/*
 * Convert a type name into the corresponding array index.
 * Return an error if the type name is unknown.
 */
/* type_name2index */
static int Struct_FindTypeByName(Tcl_Interp *interp, char *typeName, Tcl_HashTable *tablePtr, int *typeIndex)
{
	Tcl_HashEntry *hPtr;

	if ((hPtr = Tcl_FindHashEntry(tablePtr, typeName)) == NULL)
	{
		/* Set the error */
		FormatResult(interp, "unknown type \"%s\"", typeName);

		/* Failure */
		return TCL_ERROR;
	}

	/* Return the field index */
	(*typeIndex) = (int) Tcl_GetHashValue(hPtr);

	/* Success */
	return TCL_OK;
}

int Struct_GetTypeFromObj(Tcl_Interp *interp, StructType **typePtrPtr,
	Tcl_Obj *objPtr)
{
	char *t;
	int i;

	t = Tcl_GetString(objPtr);
	if (Struct_FindTypeByName(interp, t, &g_struct_hash, &i) != TCL_OK)
	{
		return TCL_ERROR;
	}
	(*typePtrPtr) = &g_struct[i];
	return TCL_OK;
}

int Struct_GetArrayIndexFromObj(Tcl_Interp *interp, StructType *typePtr,
	int *elemIndex, Tcl_Obj *objPtr)
{
	/* Get the array index */
	if (Tcl_GetIntFromObj(interp, objPtr, elemIndex) != TCL_OK)
	{
		return TCL_ERROR;
	}

	if (((*elemIndex) < 0) || ((*elemIndex) >= typePtr->max))
	{
		/* Set the error */
		FormatResult(interp, "bad array index \"%d\": must be from 0 to %d",
			(*elemIndex), typePtr->max - 1);

		/* Failure */
		return TCL_ERROR;
	}

	return TCL_OK;
}

int Struct_GetFieldFromObj(Tcl_Interp *interp, StructType *typePtr, int *fieldIndexPtr, Tcl_Obj *objPtr)
{
	char *t;

	/* Try a numerical index */
	if (Tcl_GetIntFromObj(interp, objPtr, fieldIndexPtr) != TCL_OK)
	{
		Tcl_ResetResult(interp);

		/* Get the field name */
		t = Tcl_GetString(objPtr);

		if (Struct_FindFieldByName(interp, t, typePtr->nameTable,
			fieldIndexPtr) != TCL_OK)
		{
			return TCL_ERROR;
		}
	}
	else if ((*fieldIndexPtr < 0) || (*fieldIndexPtr >= typePtr->count))
	{
		/* Set the error */
		FormatResult(interp, "bad field index \"%d\": must be from 0 to %d",
			*fieldIndexPtr, typePtr->count - 1);

		/* Failure */
		return TCL_ERROR;
	}

	return TCL_OK;
}

StructType *Struct_Lookup(Tcl_Interp *interp, char *name)
{
	int i;

	if (Struct_FindTypeByName(interp, name, &g_struct_hash, &i) != TCL_OK)
	{
		return NULL;
	}
	return &g_struct[i];
}

int Struct_AddType(Tcl_Interp *interp, StructType *data)
{
	StructType *typePtr;
	Tcl_HashEntry *hPtr;
	int i, j, new;

	g_struct = Array_Insert(g_struct, &g_struct_count, sizeof(StructType),
		g_struct_count);
	typePtr = &g_struct[g_struct_count - 1];

	typePtr->name = data->name;
	typePtr->elem = data->elem;
	typePtr->elem_size = data->elem_size;
	typePtr->max = data->max;
	typePtr->info = data->info;
	typePtr->infoProc = data->infoProc;
	typePtr->findProc = data->findProc;
	typePtr->matchProc = data->matchProc;
	Struct_ToHash(typePtr->info, &typePtr->nameTable);

	typePtr->count = 0;

	/* Check each field */
	for (i = 0; typePtr->info[i].text; i++)
	{
		int field_size = 0;

		/* Verify that the fLength field matches the field type */
		switch (typePtr->info[i].type)
		{
			case FLD_BOOL:
			case FLD_CHAR:
			case FLD_FLAGS8:
			case FLD_INDEX8:
			case FLD_INT8:
			case FLD_INT8U:
				field_size = 1;
				break;
			case FLD_FLAGS16:
			case FLD_INDEX16:
			case FLD_INT16:
			case FLD_INT16U:
				field_size = 2;
				break;
			case FLD_FLAGS32:
			case FLD_INT32:
			case FLD_INT32U:
				field_size = 4;
				break;
			case FLD_STRINGPTR:
				field_size = sizeof(void *);
				break;
		}
		if (field_size && (typePtr->info[i].fLength != field_size))
		{
			dbwin("WARNING: field='%s.%s' size=%d != fLength=%d\n",
				typePtr->name, typePtr->info[i].text, field_size,
				typePtr->info[i].fLength);
		}

		if ((typePtr->info[i].type == FLD_INDEX8) ||
			(typePtr->info[i].type == FLD_INDEX16))
		{
			for (j = 0; ((char **) typePtr->info[i].data)[j]; j++) ;
			typePtr->info[i].max = j;
		}

		/* Count the number of fields */
		++typePtr->count;
	}

	hPtr = Tcl_CreateHashEntry(&g_struct_hash, typePtr->name, &new);
	Tcl_SetHashValue(hPtr, g_struct_count - 1);

	return TCL_OK;
}

/* Public interface to "struct find" */
int Struct_Find(Tcl_Interp *interp, StructType *typePtr, int objc,
	Tcl_Obj *CONST objv[], int objOffset)
{
	int objC = objc - objOffset;
	Tcl_Obj *CONST *objV = objv + objOffset;
	char *t, **field_names;
	int i, j, index = 0;
	Tcl_Obj *listObjPtr, *objPtr;
	unsigned char *elem;

	static CONST char *fieldOp[] = {"==", "!=", "&", "!&", NULL};

	/* Default to searching forwards */
	int forwards = 1, backwards = 0;
	int first = 0, last = -1, delta = 1;

	/* Default to finding all matches */
	int request_limit = 0, match_limit = 0, cnt = 0;

	/* Default to starting from first/last index */
	int request_index = 0, start_from = 0;

	/* Default to not looking for field */
	int request_field = 0, match_field[10];
	char *field_value[10];
	int field_op[10];

	/* Default to not using custom find proc */
	int request_custom = 0;

	/* Initialize the custom search proc */
	if (typePtr->findProc != NULL)
	{
		(void) (*typePtr->findProc)(interp, typePtr, 0, NULL, NULL);
	}

	last = typePtr->max - 1;

	/* Scan arguments for options */
	for (i = 0; i < objC; )
	{
		static CONST char *findOption[] = {"-backwards", "-forwards", "-index",
			"-limit", "-field", NULL};
		int index;

		/* Try a standard option */
	    if (Tcl_GetIndexFromObj(interp, objV[i], findOption, "option",
			0, &index) == TCL_OK)
		{
			switch (index)
			{
				case 0: /* backwards */
					forwards = 0;
					backwards = 1;
					i += 1;
					break;

				case 1: /* forwards */
					forwards = 1;
					backwards = 0;
					i += 1;
					break;

				case 2: /* index */
					if (Tcl_GetIntFromObj(interp, objV[i+1], &index)
						!= TCL_OK)
					{
						return TCL_ERROR;
					}
					if ((index < 0) || (index >= last))
					{
						goto bad_index;
					}
					start_from = index;
					request_index = 1;
					i += 2;
					break;

				case 3: /* limit */
					if (Tcl_GetIntFromObj(interp, objV[i+1], &match_limit)
						!= TCL_OK)
					{
						return TCL_ERROR;
					}
					request_limit = 1;
					i += 2;
					break;

				case 4: /* field op value */
					t = Tcl_GetStringFromObj(objV[i+1], NULL);

					if (Struct_FindFieldByName(interp, t, typePtr->nameTable, &j)
						!= TCL_OK)
					{
						return TCL_ERROR;
					}
					match_field[request_field] = j;

				    if (Tcl_GetIndexFromObj(interp, objV[i+2], fieldOp,
				    	"op", 0, &index) != TCL_OK)
				    {
						return TCL_ERROR;
				    }
					field_op[request_field] = index;

					/* Get the value */
					t = Tcl_GetStringFromObj(objV[i+3], NULL);

					field_names = (char **) typePtr->info[j].data;

					switch (typePtr->info[j].type)
					{
						case FLD_FLAGS8:
						case FLD_FLAGS16:
						case FLD_FLAGS32:
							for (j = 0; field_names[j]; j++)
							{
								if (!strcmp(t, field_names[j]))
									break;
							}
							if (!field_names[j])
							{
								/* Set the error */
								FormatResult(interp, "unknown flag \"%s\"", t);

								/* Failure */
								return TCL_ERROR;
							}
							field_value[request_field] = (char *) j;
							break;

						default:
							field_value[request_field] = t;
							break;
					}

					request_field++;

					i += 4;
					break;
			}
			continue;
		}

		/* Try a custom option */
		if (typePtr->findProc != NULL)
		{
		    if ((*typePtr->findProc)(interp, typePtr, objC, objV, &i) != TCL_OK)
		    {
				return TCL_ERROR;
		    }
		    request_custom = 1;
		    continue;
		}

		/* Unknown option */
		return TCL_ERROR;
	}

	/* Search forwards */
	if (forwards)
	{
		if (request_index) first = start_from;
	}

	/* Search backwards */
	else if (backwards)
	{
		if (request_index) first = start_from;
		else first = last;
		last = 0;
		delta = -1;
	}

	/* Return a list of array indexes */
	listObjPtr = Tcl_NewListObj(0, NULL);

	/* Scan the array */
	for (index = first; ; index += delta)
	{
		/* Point to the array element */
		elem = typePtr->elem + typePtr->elem_size * index;

		/*
		 * Always call the custom proc, so it can ignore "invalid"/"empty"
		 * array entries.
		 */
		if (typePtr->matchProc != NULL)
		{
		    if ((*typePtr->matchProc)(interp, typePtr, elem, index))
				goto next;
		}

		for (i = 0; i < request_field; i++)
		{
			t_field *fieldPtr = &typePtr->info[match_field[i]];
			void *data;

			switch (fieldPtr->type)
			{
				case FLD_FLAGS8:
				case FLD_FLAGS16:
				case FLD_FLAGS32:
					data = (unsigned char *) elem + fieldPtr->fOffset;
					if ((((*(unsigned long *) data) &
						(1L << (int) field_value[i])) == 0)
						== (field_op[i] == 2))
						goto next;
					break;

				default:
					if (field_op[i] > 1)
					{
						FormatResult(interp,
							"illegal op \"%s\" on field \"%s\"",
							fieldOp[field_op[i]],
							fieldPtr->text);
						return TCL_ERROR;
					}
					objPtr = Struct_FieldToObj(interp, fieldPtr, elem, index);
					t = Tcl_GetString(objPtr);
					if ((strcmp(t, field_value[i]) == 0) == field_op[i])
						goto next;
					break;
			}
		}

		/* Found a match */
		Tcl_ListObjAppendElement(interp, listObjPtr, Tcl_NewIntObj(index));

		/* Return x matches */
		if (request_limit && (++cnt >= match_limit)) break;

next:
		/* End of search */
		if (index == last) break;
	}

	Tcl_SetObjResult(interp, listObjPtr);

	return TCL_OK;

bad_index:
	FormatResult(interp, "bad index \"%d\": must be between 0 and %d",
		index, typePtr->max - 1);

	return TCL_ERROR;

}

/* find array ?option value...? */
static int objcmd_find(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
/*	int objC = objc - infoCmd->depth; */
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;
	StructType *typePtr;

	/* Get the array info */
	if (Struct_GetTypeFromObj(interp, &typePtr, objV[1]) != TCL_OK)
	{
		return TCL_ERROR;
	}

	return Struct_Find(interp, typePtr, objc, objv, infoCmd->depth + 2);
}

/* Public interface to "struct flags" */
int Struct_Flags(Tcl_Interp *interp, StructType *typePtr, int elemIndex,
	int objc, Tcl_Obj *CONST objv[], int objOffset)
{
/*	int objC = objc - objOffset; */
	Tcl_Obj *CONST *objV = objv + objOffset;
	unsigned char *data, *elem;
	int fieldIndex, i;
	char **fieldNames, *t;
	t_field *fieldPtr;
	Tcl_Obj *listObjPtr;
	long flags;

	/* Point to the array element */
	elem = typePtr->elem + typePtr->elem_size *elemIndex;

	/* Get the field name */
	t = Tcl_GetStringFromObj(objV[0], NULL);

	/* Look up the field by name */
	if (Struct_FindFieldByName(interp, t, typePtr->nameTable, &fieldIndex)
		!= TCL_OK)
	{
		return TCL_ERROR;
	}

	/* Access the field info */
	fieldPtr = &typePtr->info[fieldIndex];

	/* Access the field data */
	data = (unsigned char *) elem + fieldPtr->fOffset;

	/* Verify the field type */
	switch (fieldPtr->type)
	{
		case FLD_FLAGS8:
			flags = *(unsigned char *) data;
			break;
		case FLD_FLAGS16:
			flags = *(unsigned short *) data;
			break;
		case FLD_FLAGS32:
			flags = *(unsigned long *) data;
			break;

		default:

			/* Set the error */
			FormatResult(interp, "field \"%s\" not flags", t);

			/* Failure */
			return TCL_ERROR;
	}

	/* Access the array of field names */
	fieldNames = (char **) fieldPtr->data;

	/* Verify the field names array */
	if (!fieldNames)
	{
		/* Set the error */
		FormatResult(interp, "field \"%s\" has no flag names", t);

		/* Failure */
		return TCL_ERROR;
	}

	/* Create an empty list object */
	listObjPtr = Tcl_NewListObj(0, NULL);

	for (i = 0; fieldNames[i]; i++)
	{
		if (flags & (1L << i))
		{
			Tcl_ListObjAppendElement(interp, listObjPtr,
				Tcl_NewStringObj(fieldNames[i], -1));
		}
	}

	/* Return the list object */
	Tcl_SetObjResult(interp, listObjPtr);

	return TCL_OK;
}

/* (struct set array index) ?field? ?value? */
int Struct_Set(Tcl_Interp *interp, StructType *typePtr, int elemIndex,
	int objc, Tcl_Obj *CONST objv[], int objOffset)
{
	int objC = objc - objOffset;
	Tcl_Obj *CONST *objV = objv + objOffset;

	char errorMsg[128], *t;
	unsigned char *elem;
	int fieldIndex;

	/* Point to the array element */
	elem = typePtr->elem + typePtr->elem_size * elemIndex;

	/* Only a single field */
	if (objC > 0)
	{
		/* Try a numerical index */
		if (Tcl_GetIntFromObj(interp, objV[0], &fieldIndex) != TCL_OK)
		{
			Tcl_ResetResult(interp);

			/* Get the field name */
			t = Tcl_GetString(objV[0]);

			if (Struct_FindFieldByName(interp, t, typePtr->nameTable,
				&fieldIndex) != TCL_OK)
			{
				return TCL_ERROR;
			}
		}
		else if ((fieldIndex < 0) || (fieldIndex >= typePtr->count))
		{
			/* Set the error */
			FormatResult(interp, "bad field index \"%d\": must be from 0 to %d",
				fieldIndex, typePtr->count - 1);

			/* Failure */
			return TCL_ERROR;
		}

		/* Set the field value */
		if (objC == 2)
		{
			if (Struct_ObjToField(interp, objV[1], &typePtr->info[fieldIndex],
				elem, elemIndex) != TCL_OK)
			{
				FormatResult(interp, "invalid field data \"%s\"",
					Tcl_GetString(objV[1]));
				return TCL_ERROR;
			}

			return TCL_OK;
		}

		/* Return the field value */
		Tcl_SetObjResult(interp, Struct_FieldToObj(interp,
			&typePtr->info[fieldIndex], elem, elemIndex));

		/* Done */
		return TCL_OK;
	}

	/* Dump the whole structure */
	Tcl_SetObjResult(interp, Struct_ToList(interp, typePtr->info, elem,
		elemIndex));

	return TCL_OK;
}

/* fieldinfo array index field */
static int objcmd_fieldinfo(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
/*	int objC = objc - infoCmd->depth; */
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;
	StructType *typePtr;
	int elemIndex;
	int fieldIndex;
	Tcl_Obj *listObjPtr;
	t_field *info;
	static char *field_types[] = {
		STRINGIFY2(FLD_INT8), STRINGIFY2(FLD_INT8U),
		STRINGIFY2(FLD_INT16), STRINGIFY2(FLD_INT16U),
		STRINGIFY2(FLD_INT32), STRINGIFY2(FLD_INT32U),
		STRINGIFY2(FLD_FLAGS8), STRINGIFY2(FLD_FLAGS16), STRINGIFY2(FLD_FLAGS32),
		STRINGIFY2(FLD_STRINGPTR),
		STRINGIFY2(FLD_STRING),
		STRINGIFY2(FLD_CHAR),
		STRINGIFY2(FLD_BOOL),
		STRINGIFY2(FLD_CALLBACK),
		STRINGIFY2(FLD_INDEX8),
		STRINGIFY2(FLD_INDEX16)
	};

	/* Get the array info */
	if (Struct_GetTypeFromObj(interp, &typePtr, objV[1]) != TCL_OK)
		return TCL_ERROR;

	/* Get the array index */
	if (Struct_GetArrayIndexFromObj(interp, typePtr, &elemIndex, objV[2])
		!= TCL_OK)
		return TCL_ERROR;

	/* Get the field index */
	if (Struct_GetFieldFromObj(interp, typePtr, &fieldIndex, objV[3])
		!= TCL_OK)
		return TCL_ERROR;

	info = &typePtr->info[fieldIndex];

	listObjPtr = Tcl_NewListObj(0, NULL);
	Tcl_ListObjAppendElement(interp, listObjPtr, Tcl_NewStringObj("name", -1));
	Tcl_ListObjAppendElement(interp, listObjPtr, Tcl_NewStringObj(info->text, -1));
	Tcl_ListObjAppendElement(interp, listObjPtr, Tcl_NewStringObj("offset", -1));
	Tcl_ListObjAppendElement(interp, listObjPtr, Tcl_NewIntObj(info->fOffset));
	Tcl_ListObjAppendElement(interp, listObjPtr, Tcl_NewStringObj("length", -1));
	Tcl_ListObjAppendElement(interp, listObjPtr, Tcl_NewIntObj(info->fLength));
	Tcl_ListObjAppendElement(interp, listObjPtr, Tcl_NewStringObj("type", -1));
	Tcl_ListObjAppendElement(interp, listObjPtr, Tcl_NewStringObj(field_types[info->type], -1));
	Tcl_ListObjAppendElement(interp, listObjPtr, Tcl_NewStringObj("edit", -1));
	Tcl_ListObjAppendElement(interp, listObjPtr, Tcl_NewBooleanObj(info->edit));
	Tcl_ListObjAppendElement(interp, listObjPtr, Tcl_NewStringObj("min", -1));
	Tcl_ListObjAppendElement(interp, listObjPtr, Tcl_NewIntObj(info->min));
	Tcl_ListObjAppendElement(interp, listObjPtr, Tcl_NewStringObj("max", -1));
	Tcl_ListObjAppendElement(interp, listObjPtr, Tcl_NewIntObj(info->max));
	Tcl_SetObjResult(interp, listObjPtr);

	return TCL_OK;
}

/* flags array index field */
static int objcmd_flags(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
/*	int objC = objc - infoCmd->depth; */
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;
	StructType *typePtr;
	int elemIndex;

	/* Get the array info */
	if (Struct_GetTypeFromObj(interp, &typePtr, objV[1]) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* Get the array index */
	if (Struct_GetArrayIndexFromObj(interp, typePtr, &elemIndex, objV[2])
		!= TCL_OK)
	{
		return TCL_ERROR;
	}

	return Struct_Flags(interp, typePtr, elemIndex, objc, objv,
		infoCmd->depth + 3);
}

/* (info array index) ?args ...? */
int Struct_Info(Tcl_Interp *interp, StructType *typePtr, int elemIndex,
	int objc, Tcl_Obj *CONST objv[], int objOffset)
{
	unsigned char *elem;

	/* Point to the array element */
	elem = typePtr->elem + typePtr->elem_size * elemIndex;

	/* Call the custom command */
	return (*typePtr->infoProc)(interp, typePtr, objc, objv,
		objOffset, elem, elemIndex);
}

/* info array index ?args ...? */
static int objcmd_info(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
/*	int objC = objc - infoCmd->depth; */
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;
	StructType *typePtr;
	int elemIndex;

	/* Get the array info */
	if (Struct_GetTypeFromObj(interp, &typePtr, objV[1]) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* Get the array index */
	if (Struct_GetArrayIndexFromObj(interp, typePtr, &elemIndex, objV[2])
		!= TCL_OK)
	{
		return TCL_ERROR;
	}

	return Struct_Info(interp, typePtr, elemIndex, objc, objv,
		infoCmd->depth + 3);
}

/* max array */
static int objcmd_max(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
/*	int objC = objc - infoCmd->depth; */
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	StructType *typePtr;

	/* Get the array info */
	if (Struct_GetTypeFromObj(interp, &typePtr, objV[1]) != TCL_OK)
	{
		return TCL_ERROR;
	}

	Tcl_SetObjResult(interp, Tcl_NewIntObj(typePtr->max));
	return TCL_OK;
}

/* set array index ?field? ?value? */
static int objcmd_set(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
/*	int objC = objc - infoCmd->depth; */
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;
	StructType *typePtr;
	int elemIndex;

	/* Get the array info */
	if (Struct_GetTypeFromObj(interp, &typePtr, objV[1]) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* Get the array index */
	if (Struct_GetArrayIndexFromObj(interp, typePtr, &elemIndex, objV[2])
		!= TCL_OK)
	{
		return TCL_ERROR;
	}

	return Struct_Set(interp, typePtr, elemIndex, objc, objv,
		infoCmd->depth + 3);
}

static CommandInit commandInit[] = {
	{0, "struct", 0, 0, (char *) NULL, (Tcl_ObjCmdProc *) NULL, (ClientData) 0},
		{1, "fieldinfo", 4, 4, "array index field", objcmd_fieldinfo, (ClientData) 0},
		{1, "find", 2, 0, "array ?arg ...?", objcmd_find, (ClientData) 0},
		{1, "flags", 4, 4, "array index field", objcmd_flags, (ClientData) 0},
		{1, "info", 4, 0, "array index option ?arg ...?", objcmd_info, (ClientData) 0},
		{1, "max", 2, 2, "array", objcmd_max, (ClientData) 0},
		{1, "set", 3, 5, "array index ?field? ?value?", objcmd_set, (ClientData) 0},
	{0, NULL, 0, 0, NULL, NULL, 0}
};

int Struct_Init(Tcl_Interp *interp)
{
	if (initialized) return TCL_OK;

	g_struct = Array_New(0, sizeof(StructType));
	g_struct_count = 0;

	Tcl_InitHashTable(&g_struct_hash, TCL_STRING_KEYS);

	(void) CommandInfo_Init(interp, commandInit, NULL);

	initialized = 1;

	return TCL_OK;
}

void Struct_Exit(Tcl_Interp *interp)
{
	if (g_struct)
		Tcl_FreeDebug(g_struct);
}
