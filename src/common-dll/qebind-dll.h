/* File: qebind.c */

/* Purpose: public interface to quasi-event package */

/*
 * Copyright (c) 1997-2009 Tim Baker
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#ifndef INCLUDED_QEBIND_H
#define INCLUDED_QEBIND_H

#include "storage.h"

typedef struct QE_BindingTable_ *QE_BindingTable;

/* Pass to QE_BindEvent */
typedef struct QE_Event {
	int type;
	int detail;
	ClientData clientData;
} QE_Event;

typedef struct QE_ExpandArgs {
	QE_BindingTable bindingTable;
	char which;
	ClientData object;
	Tcl_DString *result;
	int event;
	int detail;
	ClientData clientData;
} QE_ExpandArgs;

typedef void (*QE_ExpandProc)(QE_ExpandArgs *args);
DLL_EXTVAR int debug_bindings;

DLL_EXTERN int QE_BindInit(Tcl_Interp *interp);
DLL_EXTERN QE_BindingTable QE_CreateBindingTable(Tcl_Interp *interp);
DLL_EXTERN void QE_DeleteBindingTable(QE_BindingTable bindingTable);
DLL_EXTERN int QE_InstallEvent(QE_BindingTable bindingTable, char *name, QE_ExpandProc expand);
DLL_EXTERN int QE_InstallDetail(QE_BindingTable bindingTable, char *name, int eventType, QE_ExpandProc expand);
DLL_EXTERN int QE_CreateBinding(QE_BindingTable bindingTable,
	ClientData object, char *eventString, char *command, int append);
DLL_EXTERN int QE_DeleteBinding(QE_BindingTable bindingTable,
	ClientData object, char *eventString);
DLL_EXTERN int QE_GetBinding(QE_BindingTable bindingTable,
	ClientData object, char *eventString);
DLL_EXTERN int QE_GetAllBindings(QE_BindingTable bindingTable,
	ClientData object);
DLL_EXTERN int QE_GetEventNames(QE_BindingTable bindingTable);
DLL_EXTERN int QE_GetDetailNames(QE_BindingTable bindingTable, char *eventName);
DLL_EXTERN int QE_BindEvent(QE_BindingTable bindingTable, QE_Event *eventPtr);
DLL_EXTERN void QE_ExpandDouble(double number, Tcl_DString *result);
DLL_EXTERN void QE_ExpandNumber(long number, Tcl_DString *result);
DLL_EXTERN void QE_ExpandString(char *string, Tcl_DString *result);
DLL_EXTERN void QE_ExpandEvent(QE_BindingTable bindingTable, int eventType, Tcl_DString *result);
DLL_EXTERN void QE_ExpandDetail(QE_BindingTable bindingTable, int event, int detail, Tcl_DString *result);
DLL_EXTERN void QE_ExpandPattern(QE_BindingTable bindingTable, int eventType, int detail, Tcl_DString *result);
DLL_EXTERN void QE_ExpandUnknown(char which, Tcl_DString *result);
DLL_EXTERN int QE_BindCmd(QE_BindingTable bindingTable, int objOffset, int objc,
	Tcl_Obj *CONST objv[]);
DLL_EXTERN int QE_ConfigureCmd(QE_BindingTable bindingTable, int objOffset, int objc,
	Tcl_Obj *CONST objv[]);
DLL_EXTERN int QE_ExpandCmd(QE_BindingTable bindingTable, int objOffset, int objc,
	Tcl_Obj *CONST objv[]);
DLL_EXTERN int QE_GenerateCmd(QE_BindingTable bindingTable, int objOffset, int objc,
	Tcl_Obj *CONST objv[]);
DLL_EXTERN int QE_InstallCmd(QE_BindingTable bindingTable, int objOffset, int objc,
	Tcl_Obj *CONST objv[]);
DLL_EXTERN int QE_UnbindCmd(QE_BindingTable bindingTable, int objOffset, int objc,
	Tcl_Obj *CONST objv[]);
DLL_EXTERN int QE_UninstallCmd(QE_BindingTable bindingTable, int objOffset, int objc,
	Tcl_Obj *CONST objv[]);
DLL_EXTERN int QE_LinkageCmd(QE_BindingTable bindingTable, int objOffset, int objc,
	Tcl_Obj *CONST objv[]);

#endif /* INCLUDED_QEBIND_H */

