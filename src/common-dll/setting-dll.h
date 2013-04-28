/* File: setting-dll.h */

/* Purpose: setting stuff */

/*
 * Copyright (c) 1997-2009 Tim Baker
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#ifndef INCLUDED_SETTING_H
#define INCLUDE_SETTING_H

/*
 * These constants define the commands which can be passed to a
 * SettingProc() callback routine.
 */
#define SETTING_GET 0 /* Return the current value */
#define SETTING_DEFAULT 1 /* Return the default value */
#define SETTING_SET 2 /* Set the current value */

/*
 * Foward declarations.
 */
typedef struct SettingType SettingType;
typedef struct SettingParam SettingParam;
typedef struct SettingGroup SettingGroup;
typedef struct SettingGroup_ *SettingGroupToken;

/*
 * Each setting can have a unique callback routine.
 */
typedef int (*SettingProc)(SettingParam *param);

/*
 * One of these structures exists for every game setting.
 */
struct SettingType
{
	cptr name; /* Keyword */
	cptr desc; /* Human-readable description */
	SettingProc proc;
	long data;
};

/*
 * Contains arguments passed to a SettingProc().
 */
struct SettingParam
{
	Tcl_Interp *interp;
	int objc;
	Tcl_Obj **objv;
	SettingType *setting;
	int cmd;
};

/*
 * A related group of settings.
 */
struct SettingGroup
{
	Tcl_Interp *interp;
	int count; /* Number of settings */
	SettingType *setting; /* Array of settings */
	Tcl_HashTable hash; /* Setting name -> setting[] index */
};

extern int Setting_FindByName(SettingGroupToken token, char *settingName,
	int *settingIndex);
extern int Setting_FromObj(SettingGroupToken token,
	SettingType **settingPtrPtr, Tcl_Obj *nameObjPtr);
extern int Setting_Add(SettingGroupToken token, SettingType *data);
extern SettingGroupToken Setting_Init(Tcl_Interp *interp);

#endif /* INCLUDE_SETTING_H */
