/* File: setting.c */

/* Purpose: the "setting" command */

/*
 * Copyright (c) 1997-2009 Tim Baker
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include <tcl.h>
#include "angband.h"
#include "tnb.h"
#include "interp.h"
#include "cmdinfo-dll.h"
#include "util-dll.h"
#include "icon.h" /* FIXME: only for allow_animation */

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
	void *data;
	int detail; /* Hack -- quasi-event detail. See Bind_Setting() */
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


#if defined(OANGBANDTK) || defined(ZANGBANDTK)

/* Setting callback for autosave_freq */
static int SettingProc_autosave_freq(SettingParam *param)
{
	Tcl_Interp *interp = param->interp;
	Tcl_Obj **objv = param->objv;

	int value;
	
	switch (param->cmd)
	{
		case SETTING_GET:
			IntResult(interp, autosave_freq);
			break;
		case SETTING_DEFAULT:
			break;
		case SETTING_SET:
			if (Tcl_GetIntFromObj(interp, objv[0], &value) != TCL_OK)
			{
				return TCL_ERROR;
			}
			if (value < 0 || value > 25000) value = 0;
			autosave_freq = value;
			Bind_Setting(param->setting->detail, value);
			break;
	}

	return TCL_OK;
}

#endif /* OANGBANDTK, ZANGBANDTK */

/* Setting callback for misc boolean options */
int SettingProc_bool(SettingParam *param)
{
	Tcl_Interp *interp = param->interp;
	Tcl_Obj **objv = param->objv;

	bool *var = (bool *) param->setting->data;
	int value;
	
	switch (param->cmd)
	{
		case SETTING_GET:
			BooleanResult(interp, (*var));
			break;
		case SETTING_DEFAULT:
			break;
		case SETTING_SET:
			if (Tcl_GetBooleanFromObj(interp, objv[0], &value) != TCL_OK)
			{
				return TCL_ERROR;
			}
			(*var) = value;

			/* XXX Hack -- don't generate <Setting> for Borg options */
			if (param->setting->detail != -1)
				Bind_Setting(param->setting->detail, value);
			break;
	}

	return TCL_OK;
}

/* Setting callback for allow_animation option */
int SettingProc_allow_animation(SettingParam *param)
{
	int result = SettingProc_bool(param);

	if (param->cmd == SETTING_SET)
	{
		if (allow_animation)
			angtk_start_timer();
		else
			angtk_stop_timer();
	}

	return result;
}

/* Setting callback for delay_factor */
static int SettingProc_delay_factor(SettingParam *param)
{
	Tcl_Interp *interp = param->interp;
	Tcl_Obj **objv = param->objv;

	int value;
	
	switch (param->cmd)
	{
		case SETTING_GET:
			IntResult(interp, op_ptr_delay_factor);
			break;
		case SETTING_DEFAULT:
			break;
		case SETTING_SET:
			if (Tcl_GetIntFromObj(interp, objv[0], &value) != TCL_OK)
			{
				return TCL_ERROR;
			}
			if (value < 0 || value > 9) value = 9;
			op_ptr_delay_factor = value;
			Bind_Setting(param->setting->detail, value);
			break;
	}

	return TCL_OK;
}

/* Setting callback for hitpoint_warn */
static int SettingProc_hitpoint_warn(SettingParam *param)
{
	Tcl_Interp *interp = param->interp;
	Tcl_Obj **objv = param->objv;

	int value;
	
	switch (param->cmd)
	{
		case SETTING_GET:
			IntResult(interp, op_ptr_hitpoint_warn);
			break;
		case SETTING_DEFAULT:
			break;
		case SETTING_SET:
			if (Tcl_GetIntFromObj(interp, objv[0], &value) != TCL_OK)
			{
				return TCL_ERROR;
			}
			if (value < 0 || value > 9) value = 9;
			op_ptr_hitpoint_warn = value;
			Bind_Setting(param->setting->detail, value);
			break;
	}

	return TCL_OK;
}

#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)
#define cheat_variable(i) op_ptr->opt[OPT_CHEAT+i]
#endif /* ANGBANDTK, KANGBANDTK */
#if defined(ZANGBANDTK)
#define cheat_variable(i) (*cheat_info[i].o_var)
#endif /* ZANGBANDTK */

/* Setting callback for cheating options */
static int SettingProc_cheat(SettingParam *param)
{
	Tcl_Interp *interp = param->interp;
	Tcl_Obj **objv = param->objv;

	int index = (int) param->setting->data;
	int value;
	
	switch (param->cmd)
	{
		case SETTING_GET:
			BooleanResult(interp, cheat_variable(index));
			break;
		case SETTING_DEFAULT:
			break;
		case SETTING_SET:
			if (Tcl_GetBooleanFromObj(interp, objv[0], &value) != TCL_OK)
			{
				return TCL_ERROR;
			}
			cheat_variable(index) = value;
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)
			if (op_ptr->opt[OPT_CHEAT+index])
			{
				op_ptr->opt[OPT_SCORE+index] = TRUE;
			}
#endif /* ANGBANDTK, KANGBANDTK */
#if defined(ZANGBANDTK)
			noscore |= (cheat_info[index].o_set * 256 +
				cheat_info[index].o_bit);
#endif /* ZANGBANDTK */
			Bind_Setting(param->setting->detail, value);
			break;
	}

	return TCL_OK;
}

#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)
#define option_variable(i) op_ptr->opt[i]
#endif /* ANGBANDTK, KANGBANDTK, OANGBANDTK */
#if defined(ZANGBANDTK)
#define option_variable(i) (*option_info[i].o_var)
#endif /* ZANGBANDTK */

/* Setting callback for regular boolean options */
int SettingProc_opt(SettingParam *param)
{
	Tcl_Interp *interp = param->interp;
	Tcl_Obj **objv = param->objv;

	int index = (int) param->setting->data;
	int value;
	
	switch (param->cmd)
	{
		case SETTING_GET:
			BooleanResult(interp, option_variable(index));
			break;
		case SETTING_DEFAULT:
			break;
		case SETTING_SET:
			if (Tcl_GetBooleanFromObj(interp, objv[0], &value) != TCL_OK)
			{
				return TCL_ERROR;
			}
			option_variable(index) = value;
			Bind_Setting(param->setting->detail, value);
			break;
	}

	return TCL_OK;
}

/*
 * Return the setting[] index for the setting with the given keyword.
 * Return an error if there is no such setting.
 */
int Setting_FindByName(SettingGroupToken token, char *settingName,
	int *settingIndex)
{
	SettingGroup *group = (SettingGroup *) token;
	Tcl_HashEntry *hPtr;

	/* Look up the hash table entry using the tval as a key */
	if ((hPtr = Tcl_FindHashEntry(&group->hash, settingName)) == NULL)
	{
		/* Set the error */
		FormatResult(g_interp, "unknown setting \"%s\"", settingName);

		/* Failure */
		return TCL_ERROR;
	}

	/* Return the g_setting[] index for this setting */
	(*settingIndex) = (int) Tcl_GetHashValue(hPtr);

	/* Success */
	return TCL_OK;
}

int Setting_FromObj(SettingGroupToken token, SettingType **settingPtrPtr,
	Tcl_Obj *nameObjPtr)
{
	SettingGroup *group = (SettingGroup *) token;
	char *t;
	int settingIndex;

	t = Tcl_GetString(nameObjPtr);
	if (Setting_FindByName(token, t, &settingIndex) != TCL_OK)
	{
		return TCL_ERROR;
	}
	(*settingPtrPtr) = &group->setting[settingIndex];

	return TCL_OK;
}

int Setting_Add(SettingGroupToken token, SettingType *data)
{
	SettingGroup *group = (SettingGroup *) token;
	SettingType *settingPtr;
	int new;
	Tcl_HashEntry *hPtr;

	if (data->name == NULL)
	{
		dbwin("Setting_Add: setting name is NULL\n");
	}

	group->setting = Array_Insert(group->setting, &group->count,
		sizeof(SettingType), group->count);
	settingPtr = &group->setting[group->count - 1];

	settingPtr->name = data->name;
	settingPtr->desc = data->desc;
	settingPtr->proc = data->proc;
	settingPtr->data = data->data;

	/* XXX Hack */
	settingPtr->detail = group->count;
	
	/* Create a new hash table entry with key = setting keyword */
	hPtr = Tcl_CreateHashEntry(&group->hash, settingPtr->name, &new);
	if (!new)
		panic("Setting_Add: duplicate setting \"%s\"", settingPtr->name);

	/* Set the hash table entry with value = setting[] index */
	Tcl_SetHashValue(hPtr, group->count - 1);

	return TCL_OK;
}

SettingGroupToken Setting_Init(Tcl_Interp *interp)
{
	SettingGroup *group;

	group = (SettingGroup *) Tcl_AllocDebug(sizeof(SettingGroup));
	group->interp = interp;
	group->count = 0;
	group->setting = Array_New(0, sizeof(SettingType));
	Tcl_InitHashTable(&group->hash, TCL_STRING_KEYS);

	return (SettingGroupToken) group;
}

int
objcmd_setting(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	SettingGroupToken token = (SettingGroupToken) infoCmd->clientData;
	static CONST char *cmdOptions[] = {"default", "desc", "set", NULL};
	int index;

	SettingParam param;
	SettingType *settingPtr;
	
    if (objC < 3)
    {
		Tcl_WrongNumArgs(interp, infoCmd->depth + 1, objv, "option setting ?arg ...?");
		return TCL_ERROR;
    }

    if (Tcl_GetIndexFromObj(interp, objV[1], cmdOptions, "option", 0, 
		&index) != TCL_OK)
	{
		return TCL_ERROR;
    }

	switch (index)
	{
		case 0: /* default */
			break;

		case 1: /* desc */
			if (Setting_FromObj(token, &settingPtr, objV[2]) != TCL_OK)
			{
				return TCL_ERROR;
		    }
			ExtToUtf_SetResult(interp, (char *) settingPtr->desc);
			break;

		case 2: /* set */
			if (Setting_FromObj(token, &settingPtr, objV[2]) != TCL_OK)
			{
				return TCL_ERROR;
		    }
		    param.interp = interp;
		    param.objc = objC - 3;
		    param.objv = (Tcl_Obj **) objV + 3;
		    param.setting = settingPtr;
		    if (objC == 4)
			    param.cmd = SETTING_SET;
		    else
		    	param.cmd = SETTING_GET;
		    return (*settingPtr->proc)(&param);
	}

	/* Success */
	return TCL_OK;
}

SettingGroupToken g_setting = NULL; /* The global setting info. */

#if defined(ZANGBANDTK)

static void verify_option_table(void)
{
	u32b flag[8] = {0L};
	int i, os, ob;

	for (i = 0; option_info[i].o_desc; i++)
	{
		os = option_info[i].o_set;
		ob = option_info[i].o_bit;

		if ((os < 0) || (os >= 8))
			quit_fmt("verify_option_table: bad os=%d", os);
		if ((ob < 0) || (ob >= 32))
			quit_fmt("verify_option_table: bad ob=%d", ob);
		if (flag[os] & (1L << ob))
			quit_fmt("verify_option_table: os=%d,ob=%d already set", os, ob);

		flag[os] |= (1L << ob);
	}

	/* Report unused bits */
	for (os = 0; os < 8; os++)
	{
		for (ob = 0; ob < 32; ob++)
		{
			if (!(flag[os] & (1L << ob)))
				dbwin("verify_option_table: os=%d ob=%d unused\n", os, ob);
		}
	}
}

#endif /* ZANGBANDTK */

/*
 * Initialize all the setting/option-related stuff.
 */
void settings_init(void)
{
	SettingGroup *group;
	SettingType setting;
	int i;

#if defined(ZANGBANDTK)
	verify_option_table();
#endif /* ZANGBANDTK */

	/* Allocate the game settings master */
	g_setting = Setting_Init(g_interp);

#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)
	for (i = 0; i < OPT_MAX; i++)
	{
		if ((i >= OPT_CHEAT) && (i < OPT_CHEAT + CHEAT_MAX))
			continue;
		setting.name = option_text[i];
		setting.desc = option_desc[i];
#endif /* ANGBANDTK, KANGBANDTK, OANGBANDTK */
#if defined(ZANGBANDTK)
	for (i = 0; option_info[i].o_desc; i++)
	{
		setting.name = option_info[i].o_text;
		setting.desc = option_info[i].o_desc;
#endif /* ZANGBANDTK */
		setting.proc = SettingProc_opt;
		setting.data = (void *) i;

		if (!setting.name)
			continue;

		/* Add this setting */
		Setting_Add(g_setting, &setting);
	}

	setting.name = "delay_factor";
	setting.desc = "Base delay factor";
	setting.proc = SettingProc_delay_factor;
	setting.data = 0;
	Setting_Add(g_setting, &setting);

	setting.name = "hitpoint_warn";
	setting.desc = "Base hitpoint warning";
	setting.proc = SettingProc_hitpoint_warn;
	setting.data = 0;
	Setting_Add(g_setting, &setting);

	setting.name = "use_sound";
	setting.desc = "Play sound effects";
	setting.proc = SettingProc_bool;
	setting.data = (void *) &use_sound;
	Setting_Add(g_setting, &setting);

	setting.name = "allow_animation";
	setting.desc = "Allow animation";
	setting.proc = SettingProc_allow_animation;
	setting.data = (void *) &allow_animation;
	Setting_Add(g_setting, &setting);

#if defined(OANGBANDTK)
	setting.name = "autosave";
	setting.desc = "Timed autosave";
	setting.proc = SettingProc_bool;
	setting.data = (void *) &autosave;
	Setting_Add(g_setting, &setting);

	setting.name = "autosave_freq";
	setting.desc = "Timed autosave frequency";
	setting.proc = SettingProc_autosave_freq;
	setting.data = 0;
	Setting_Add(g_setting, &setting);
#endif /* OANGBANDTK */

#if defined(ZANGBANDTK)
	setting.name = "autosave_l";
	setting.desc = "Autosave when entering new levels";
	setting.proc = SettingProc_bool;
	setting.data = (void *) &autosave_l;
	Setting_Add(g_setting, &setting);

	setting.name = "autosave_t";
	setting.desc = "Timed autosave";
	setting.proc = SettingProc_bool;
	setting.data = (void *) &autosave_t;
	Setting_Add(g_setting, &setting);

	setting.name = "autosave_freq";
	setting.desc = "Timed autosave frequency";
	setting.proc = SettingProc_autosave_freq;
	setting.data = 0;
	Setting_Add(g_setting, &setting);
#endif /* ZANGBANDTK */

	for (i = 0; i < CHEAT_MAX; i++)
	{
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)
		setting.name = option_text[OPT_CHEAT+i];
		setting.desc = option_desc[OPT_CHEAT+i];
#endif /* ANGBANDTK KANGBANDTK, OANGBANDTK */
#if defined(ZANGBANDTK)
		setting.name = cheat_info[i].o_text;
		setting.desc = cheat_info[i].o_desc;
#endif /* ZANGBANDTK */
		setting.proc = SettingProc_cheat;
		setting.data = (void *) i;

		if (!setting.name)
			continue;

		Setting_Add(g_setting, &setting);
	}

	/*
	 * Create a list of setting keywords for use with bind.c stuff.
	 */
	group = (SettingGroup *) g_setting;
	C_MAKE(keyword_setting, group->count + 1, cptr);
	for (i = 0; i < group->count; i++)
	{
		keyword_setting[i] = group->setting[i].name;
	}
	keyword_setting[i] = NULL;
}

void settings_exit(void)
{
	if (g_setting)
	{
		SettingGroup *group = (SettingGroup *) g_setting;
		Tcl_FreeDebug(group->setting);
		Tcl_FreeDebug(group);
	}
}

void Bind_Option(const char *name, int value)
{
	int index;

	if (Setting_FindByName(g_setting, (char *) name, &index) == TCL_OK)
	{
		SettingGroup *group = (SettingGroup *) g_setting;
		Bind_Setting(group->setting[index].detail, value);
	}
}

#ifdef ALLOW_BORG

/* Setting callback for Borg delay_factor */
static int SettingProc_borg_delay_factor(SettingParam *param)
{
	Tcl_Interp *interp = param->interp;
	Tcl_Obj **objv = param->objv;

	byte *var = (byte *) param->setting->data;
	int value;
	
	switch (param->cmd)
	{
		case SETTING_GET:
			IntResult(interp, (*var));
			break;
		case SETTING_DEFAULT:
			break;
		case SETTING_SET:
			if (Tcl_GetIntFromObj(interp, objv[0], &value) != TCL_OK)
			{
				return TCL_ERROR;
			}
			if (value < 0 || value > 9) value = 9;
			(*var) = value;
			break;
	}

	return TCL_OK;
}

ClientData borg_init_settings(SettingType *settings)
{
	SettingGroupToken token = Setting_Init(g_interp);
	SettingGroup *group = (SettingGroup *) token;
	int i;

	for (i = 0; settings[i].name; ++i)
	{
		if (streq(settings[i].name, "delay_factor"))
			settings[i].proc = SettingProc_borg_delay_factor;
		else
			settings[i].proc = SettingProc_bool;
		Setting_Add(token, &settings[i]);

		/* Hack -- See SettingProc_bool() */
		group->setting[i].detail = -1;
	}
	return token;
}

#endif /* ALLOW_BORG */

