/* File: cmdinfo-dll.c */

/* Purpose: command stuff */

/*
 * Copyright (c) 1997-2009 Tim Baker
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#ifdef __LCC__
#define BAR(x) x
/* #define foo(x) */
int foo BAR((int x));
#endif

#include <tcl.h>
#include "cmdinfo-dll.h"

int CommandInfo_ObjCmd(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;
	int subCmdIndex;

	if (infoCmd->subCmd.count)
	{
		if (objC < 2)
		{
			Tcl_WrongNumArgs(interp, infoCmd->depth + 1, objv, "option ?arg ...?");
			return TCL_ERROR;
		}
		if (Tcl_GetIndexFromObj(interp, objV[1], (CONST char **) infoCmd->subCmd.name,
			"option", 0, &subCmdIndex) != TCL_OK)
		{
			return TCL_ERROR;
		}
		infoCmd = infoCmd->subCmd.info[subCmdIndex];
		return CommandInfo_ObjCmd((ClientData) infoCmd, interp, objc, objv);
	}

	if ((infoCmd->minArgs && (objC < infoCmd->minArgs)) ||
		(infoCmd->maxArgs && (objC > infoCmd->maxArgs)))
	{
		Tcl_WrongNumArgs(interp, infoCmd->depth + 1, objv,
			infoCmd->errorMsg);
		return TCL_ERROR;
	}

	return (*infoCmd->proc)(clientData, interp, objc, objv);
}

void CommandInfo_Add(CommandInfo *infoCmd, CommandInfo *infoSubCmd)
{
	int i;
	int alloc = infoCmd->subCmd.alloc;
	int count = infoCmd->subCmd.count;
	
	if ((count + 1) >= alloc)
	{
		CommandInfo **info = (CommandInfo **) Tcl_Alloc(sizeof(CommandInfo *) * (alloc + 5));
		char **name = (char **) Tcl_Alloc(sizeof(char *) * (alloc + 5 + 1));
		if (infoCmd->subCmd.count)
		{
			for (i = 0; i < count; i++)
			{
				name[i] = infoCmd->subCmd.name[i];
				info[i] = infoCmd->subCmd.info[i];
			}
			Tcl_Free((char *) infoCmd->subCmd.name);
			Tcl_Free((char *) infoCmd->subCmd.info);
		}
		infoCmd->subCmd.name = name;
		infoCmd->subCmd.info = info;
		infoCmd->subCmd.alloc += 5;
	}

	infoCmd->subCmd.name[count] = infoSubCmd->name;
	infoCmd->subCmd.name[count + 1] = NULL;
	infoCmd->subCmd.info[count] = infoSubCmd;
	++infoCmd->subCmd.count;
}

CommandInfo *CommandInfo_New(CommandInit *init)
{
	CommandInfo *infoCmd;

	infoCmd = (CommandInfo *) Tcl_Alloc(sizeof(CommandInfo));
	infoCmd->name = init->name;
	infoCmd->minArgs = init->minArgs;
	infoCmd->maxArgs = init->maxArgs;
	infoCmd->errorMsg = init->errorMsg;
	infoCmd->proc = init->proc;
	infoCmd->clientData = init->clientData;
	infoCmd->depth = init->depth;
	infoCmd->subCmd.name = NULL;
	infoCmd->subCmd.info = NULL;
	infoCmd->subCmd.count = 0;
	infoCmd->subCmd.alloc = 0;

	return infoCmd;
}

/*
 * Get the CommandInfo for a command or subcommand.
 */
CommandInfo *CommandInfo_GetInfoAux(Tcl_Interp *interp, char *names[],
	CommandInfo *infoCmd)
{
	int subCmdIndex;

#if 0
	if (strcmp(infoCmd->name, names[0]))
	{
		/* REPORT */
		return NULL;
	}
#endif

	/* This is the command we're looking for */
	if (names[1] == NULL)
	{
		return infoCmd;
	}

	/* Check subcommands */
	if (infoCmd->subCmd.count)
	{
		Tcl_Obj *nameObjPtr = Tcl_NewStringObj(names[1], -1);

		if (Tcl_GetIndexFromObj(interp, nameObjPtr, (CONST char **) infoCmd->subCmd.name,
			"option", 0, &subCmdIndex) != TCL_OK)
		{
			Tcl_DecrRefCount(nameObjPtr);
			return NULL;
		}
		Tcl_DecrRefCount(nameObjPtr);
		infoCmd = infoCmd->subCmd.info[subCmdIndex];
		return CommandInfo_GetInfoAux(interp, names + 1, infoCmd);
	}

	/* REPORT */
	return NULL;
}

CommandInfo *CommandInfo_GetInfo(Tcl_Interp *interp, char *names[])
{
	Tcl_CmdInfo cmdInfo;

	if (Tcl_GetCommandInfo(interp, names[0], &cmdInfo) == 0)
	{
		return NULL;
	}
	return CommandInfo_GetInfoAux(interp, names,
		(CommandInfo *) cmdInfo.objClientData);
}

int CommandInfo_InitAux(Tcl_Interp *interp, CommandInit *init, int index,
	CommandInfo *parent)
{
	int i;
	CommandInfo *infoCmd = NULL;

	/* Done */
	if (init[index].name == NULL) return -1;

	if (parent == NULL)
	{
		char *names[2];
		names[0] = init[index].name;
		names[1] = NULL;
		infoCmd = CommandInfo_GetInfo(interp, names);
	}

	if (infoCmd == NULL)
	{
		/* Create a new command */
		infoCmd = CommandInfo_New(&init[index]);
	
		if (parent == NULL)
		{
			Tcl_CreateObjCommand(interp, infoCmd->name,
				CommandInfo_ObjCmd, (ClientData) infoCmd, NULL);
		}
		else
		{
			CommandInfo_Add(parent, infoCmd);
		}
	}

	i = index++;
	while (init[index].depth > init[i].depth)
	{
		index = CommandInfo_InitAux(interp, init, index, infoCmd);
if (index == -1) break; /* ? */
	}

	return index;
}

int CommandInfo_Init(Tcl_Interp *interp, CommandInit *init, CommandInfo *parent)
{
	int i;

	for (i = 0; (i >= 0) && init[i].name; )
	{
		i = CommandInfo_InitAux(interp, init, i, parent);
	}

	return TCL_OK;
}

