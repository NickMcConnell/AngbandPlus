/* File: bind.c */

/* Purpose: quasi-event binding code */

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
#include "util-dll.h"
#include "qebind-dll.h"
#include "icon.h"

static QE_BindingTable bindingTable = NULL;

static void ExpandAsk(QE_ExpandArgs *args);
static void ExpandAssign(QE_ExpandArgs *args);
static void ExpandChoose(QE_ExpandArgs *args);
static void ExpandCursor(QE_ExpandArgs *args);
static void ExpandDungeon(QE_ExpandArgs *args);
static void ExpandGeneric(QE_ExpandArgs *args);
static void ExpandKeymap(QE_ExpandArgs *args);
static void ExpandPosition(QE_ExpandArgs *args);
static void ExpandPy(QE_ExpandArgs *args);
static void ExpandSetting(QE_ExpandArgs *args);
static void ExpandStatus(QE_ExpandArgs *args);
static void ExpandTarget(QE_ExpandArgs *args);
static void ExpandTrack(QE_ExpandArgs *args);

cptr keyword_assign[] = {"monster", "object", "character",
		"feature", "shopkeeper", "artifact", NULL};

cptr keyword_choose[] = {"item", "spell",
#if defined(KANGBANDTK)
	"cmd_pet",
#endif /* KANGBANDTK */
#if defined(OANGBANDTK)
	"ele_attack",
#endif /* ZANGBANDTK */
#if defined(ZANGBANDTK)
	"power", "mindcraft", "cmd_pet",
#endif /* ZANGBANDTK */
	NULL};

cptr keyword_cursor[] = {"show", "hide", NULL};
cptr keyword_dungeon[] = {"enter", "leave", "generate", NULL};

cptr keyword_inkey[] = {"cmd", "dir", "disturb", "item", "item_store",
	"more", "spell", "target",
#if defined(OANGBANDTK)
	"ele_attack",
#endif /* ZANGBANDTK */
#if defined(ZANGBANDTK)
	"mindcraft", "power", "cmd_pet",
#endif /* ZANGBANDTK */
	NULL};

cptr keyword_py[] = {"hitpoints", "mana", "food", "depth", "exp",
	"level", "armor_class", "name", "title", "gold",
#if defined(ZANGBANDTK)
	"race",
#endif /* ZANGBANDTK */
	NULL};

cptr keyword_stat[] = {"strength", "intelligence", "wisdom", "dexterity",
	"constitution", "charisma", NULL};
cptr keyword_status[] = {"cut", "stun", "hunger", "blind", "confused",
	"afraid", "poisoned", "state", "speed", "study", "winner",
#if defined(OANGBANDTK)
	"shape",
#endif /* */
#ifdef ALLOW_STATUS_EXTRA
	"blessed", "hero", "berserk", "acid", "cold", "elec", "fire",
	"pois", "protevil", "shield",
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(ZANGBANDTK)
	"invuln",
#endif /* */
	"fast", "slow",
	"infra", "see_invis", "recall", "image",
#if defined(KANGBANDTK)
	"anchor", "ghost", "invis", "oppose_cc", "oppose_ld",
	"oppose_ss", "oppose_neth", "oppose_nex", "levitate",
	"sus_str", "sus_int", "sus_wis", "sus_dex",
	"sus_con", "sus_chr",
#endif /* KANGBANDTK */
#if defined(OANGBANDTK)
	"ele_attack", "esp", "magicdef", "stealth",
#endif /* OANGBANDTK */
#if defined(ZANGBANDTK)
	"esp", "wraith",
#endif /* ZANGBANDTK */
#endif /* ALLOW_STATUS_EXTRA */
	NULL};

cptr *keyword_setting = NULL;
cptr keyword_target[] = {"set", "unset", "visibility", NULL};
cptr keyword_term[] = {"fresh", "inkey", NULL};
cptr keyword_track[] = {"health", "race", "object", "inventory", "equipment",
	"grid", "message", NULL};

cptr keyword_ask[] = {"quantity", "yes_no", "haggle", NULL };

typedef struct EventInfo {
	char *name; /* Name of event (ex "Stat", "Position") */
	int type; /* Event type (ex Stat, Position) */
	QE_ExpandProc expand; /* Callback to expand % in scripts */
	cptr *detail; /* Array of detail names, or NULL */
} EventInfo;

/*
 * Table to initialize event types. Elements must be in order of
 * EVENT_XXX constants.
 */
static EventInfo eventArray[] = {
	{"Position", EVENT_POSITION, ExpandPosition, NULL},
	{"Stat", EVENT_STAT, ExpandGeneric, keyword_stat},
	{"Target", EVENT_TARGET, ExpandTarget, keyword_target},
	{"Status", EVENT_STATUS, ExpandStatus, keyword_status},
	{"Inkey", EVENT_INKEY, ExpandGeneric, keyword_inkey},
	{"Cursor", EVENT_CURSOR, ExpandCursor, keyword_cursor},
	{"Assign", EVENT_ASSIGN, ExpandAssign, keyword_assign},
	{"Term", EVENT_TERM, ExpandGeneric, keyword_term},
	{"Choose", EVENT_CHOOSE, ExpandChoose, keyword_choose},
	{"Track", EVENT_TRACK, ExpandTrack, keyword_track},
	{"Py", EVENT_PY, ExpandPy, keyword_py},
	{"Setting", EVENT_SETTING, ExpandSetting, NULL},
	{"Dungeon", EVENT_DUNGEON, ExpandDungeon, keyword_dungeon},
	{"Keymap", EVENT_KEYMAP, ExpandKeymap, NULL},
	{"Ask", EVENT_ASK, ExpandAsk, keyword_ask},
	{NULL, 0}
};

/*
 * Wrapper around QE_ExpandString().
 */
void ExtToUtf_ExpandString(char *extString, Tcl_DString *result)
{
	char *utfString;
	Tcl_DString utfDString;

	utfString = Tcl_ExternalToUtfDString(NULL, extString, -1, &utfDString);
	QE_ExpandString(utfString, result);
	Tcl_DStringFree(&utfDString);
}

/* Handle %? */
static void DumpPercents(QE_ExpandArgs *args, QE_ExpandProc proc, CONST char *chars)
{
	char which = args->which;
	char buf[2];
	int i;

	buf[1] = '\0';

	Tcl_DStringStartSublist(args->result);
	for (i = 0; chars[i]; i++)
	{
		args->which = chars[i];
		buf[0] = chars[i];
		Tcl_DStringAppendElement(args->result, buf);
		Tcl_DStringAppend(args->result, " ", 1);
		(*proc)(args);
	}
	Tcl_DStringEndSublist(args->result);
	args->which = which;
}

/*
 * %-substitution for any event
 */
static void Percents_Generic(QE_ExpandArgs *args)
{
	switch (args->which)
	{
		case 'd': /* detail */
			QE_ExpandDetail(args->bindingTable, args->event, args->detail,
				args->result);
			break;

		case 'e': /* event */
			QE_ExpandEvent(args->bindingTable, args->event, args->result);
			break;

		case 'P': /* pattern */
			QE_ExpandPattern(args->bindingTable, args->event, args->detail, args->result);
			break;

		case 'W': /* object */
			QE_ExpandString((char *) args->object, args->result);
			break;

		default:
			QE_ExpandUnknown(args->which, args->result);
			break;
	}
}

/*
 * %-substitution for any event
 */
static void Percents_Any(QE_ExpandArgs *args, QE_ExpandProc proc, CONST char *chars)
{
	char chars2[64];

	switch (args->which)
	{
		case '?':
			strcpy(chars2, "WPed");
			strcat(chars2, chars);
			DumpPercents(args, proc, chars2);
			break;

		default:
			Percents_Generic(args);
			break;
	}
}

/*
 * %-substitution for EVENT_ASK
 */
static void ExpandAsk(QE_ExpandArgs *args)
{
	switch (args->detail)
	{
		case KEYWORD_ASK_HAGGLE + 1:
		case KEYWORD_ASK_QUANTITY + 1:
		case KEYWORD_ASK_YES_NO + 1:
		{
			struct {int min, max, show;} *clientData = args->clientData;
			switch (args->which)
			{
				case 'm': /* min */
					QE_ExpandNumber(clientData->min, args->result);
					break;

				case 'M': /* max */
					QE_ExpandNumber(clientData->max, args->result);
					break;

				case 's': /* show/hide */
					QE_ExpandNumber(clientData->show, args->result);
					break;

				default:
					Percents_Any(args, ExpandAsk, "mMs");
					break;
			}
			break;
		}
		default:
			Percents_Any(args, ExpandAsk, "mMs");
			break;
	}
}

/*
 * %-substitution for EVENT_ASSIGN
 */
static void ExpandAssign(QE_ExpandArgs *args)
{
	struct {int toIndex; t_assign *assignPtr;} *clientData = args->clientData;
	char buf[128];

	switch (args->which)
	{
		case 'I': /* -toindex */
			QE_ExpandNumber(clientData->toIndex, args->result);
			break;

		case 'a': /* -assign */
			QE_ExpandString(assign_print(buf, clientData->assignPtr),
				args->result);
			break;

		default:
			Percents_Any(args, ExpandAssign, "Ia");
			break;
	}
}

/*
 * %-substitution for EVENT_CHOOSE
 */
static void ExpandChoose(QE_ExpandArgs *args)
{
	struct {int other, show;} *clientData = args->clientData;

	switch (args->detail)
	{
		case KEYWORD_CHOOSE_ITEM + 1:
			switch (args->which)
			{
				case 'a': /* allow */
				{
					int allow = (clientData->other >> 8) & 0xF;
					Tcl_DStringStartSublist(args->result);
					if (allow & USE_INVEN)
						Tcl_DStringAppendElement(args->result, "inventory");
					if (allow & USE_EQUIP)
						Tcl_DStringAppendElement(args->result, "equipment");
					if (allow & USE_FLOOR)
						Tcl_DStringAppendElement(args->result, "floor");
					Tcl_DStringEndSublist(args->result);
					break;
				}

				case 'o': /* other */
				{
					static char *s[] = {"inventory", "equipment", "floor", NULL};
					QE_ExpandString(s[clientData->other & 0xF], args->result);
					break;
				}

				case 's': /* show/hide */
					QE_ExpandNumber(clientData->show, args->result);
					break;

				default:
					Percents_Any(args, ExpandChoose, "aos");
					break;
			}
			break;

		default:
			switch (args->which)
			{
				case 'o': /* other */
					QE_ExpandNumber(clientData->other, args->result);
					break;

				case 's': /* show/hide */
					QE_ExpandNumber(clientData->show, args->result);
					break;

				default:
					Percents_Any(args, ExpandChoose, "os");
					break;
			}
			break;
	}
}

/*
 * %-substitution for EVENT_CURSOR
 */
static void ExpandCursor(QE_ExpandArgs *args)
{
	struct {int y, x;} *clientData = args->clientData;

	switch (args->which)
	{
		case 'y': /* y */
			QE_ExpandNumber(clientData->y, args->result);
			break;

		case 'x': /* x */
			QE_ExpandNumber(clientData->x, args->result);
			break;

		default:
			Percents_Any(args, ExpandCursor, "yx");
			break;
	}
}

/*
 * %-substitution for EVENT_DUNGEON
 */
static void ExpandDungeon(QE_ExpandArgs *args)
{
	switch (args->which)
	{
		case 'c': /* depth */
			QE_ExpandNumber(p_ptr_depth, args->result);
			break;

		default:
			Percents_Any(args, ExpandDungeon, "c");
			break;
	}
}

/*
 * %-substitution for any event
 */
static void ExpandGeneric(QE_ExpandArgs *args)
{
	Percents_Any(args, Percents_Generic, "");
}

/*
 * %-substitution for EVENT_KEYMAP
 */
static void ExpandKeymap(QE_ExpandArgs *args)
{
	switch (args->which)
	{
		case 'c': /* ch */
			QE_ExpandString((char *) args->clientData, args->result);
			break;

		default:
			Percents_Any(args, ExpandKeymap, "c");
			break;
	}
}

/*
 * %-substitution for EVENT_POSITION
 */
static void ExpandPosition(QE_ExpandArgs *args)
{
	struct {int who, y1, x1, y2, x2;} *clientData = args->clientData;
	int number;

	switch (args->which)
	{
		case 'y':
			number = clientData->y2;
			QE_ExpandNumber(number, args->result);
			break;

		case 'x':
			number = clientData->x2;
			QE_ExpandNumber(number, args->result);
			break;

		default:
			Percents_Any(args, ExpandPosition, "yx");
			break;
	}
}

/*
 * %-substitution for EVENT_PY
 */
static void ExpandPy(QE_ExpandArgs *args)
{
	double number = 0;
	long expadv;
	char *chars = "";

	switch (args->detail)
	{
		case KEYWORD_PY_HP + 1:
			switch (args->which)
			{
				case 'c': /* chp */
					QE_ExpandNumber(p_ptr->chp, args->result);
					return;

				case 'm': /* mhp */
					QE_ExpandNumber(p_ptr->mhp, args->result);
					return;

				case 'f': /* chp/mhp */
					if (p_ptr->mhp) number = (double) p_ptr->chp / p_ptr->mhp;
					QE_ExpandDouble(number, args->result);
					return;
			}
			chars = "cmf";
			break;

		case KEYWORD_PY_SP + 1:
			switch (args->which)
			{
				case 'c': /* csp */
					QE_ExpandNumber(p_ptr->csp, args->result);
					return;

				case 'm': /* msp */
					QE_ExpandNumber(p_ptr->msp, args->result);
					return;

				case 'f': /* csp/msp */
					if (p_ptr->msp) number = (double) p_ptr->csp / p_ptr->msp;
					QE_ExpandDouble(number, args->result);
					return;
			}
			chars = "cmf";
			break;

		case KEYWORD_PY_FOOD + 1:
			switch (args->which)
			{
				case 'c': /* food */
					QE_ExpandNumber(p_ptr->food, args->result);
					return;

				case 'm': /* PY_FOOD_MAX */
					QE_ExpandNumber(PY_FOOD_MAX, args->result);
					return;

				case 'f': /* food/PY_FOOD_MAX */
					QE_ExpandDouble((double) p_ptr->food / PY_FOOD_MAX, args->result);
					return;
			}
			chars = "cmf";
			break;

		case KEYWORD_PY_DEPTH + 1:
			switch (args->which)
			{
				case 'c': /* current */
					QE_ExpandNumber(p_ptr_depth, args->result);
					return;

				case 'm': /* max_depth */
					QE_ExpandNumber(p_ptr_max_depth, args->result);
					return;
			}
			chars = "cm";
			break;

		case KEYWORD_PY_EXP + 1:
			switch (args->which)
			{
				case 'a': /* advance */
					if (p_ptr->lev >= PY_MAX_LEVEL)
					{
						expadv = 999999999;
					}
					else
					{
						expadv = (s32b)(player_exp[p_ptr->lev - 1] * p_ptr->expfact / 100L);
					}
					QE_ExpandNumber(expadv, args->result);
					return;

				case 'c': /* exp */
					QE_ExpandNumber(p_ptr->exp, args->result);
					return;

				case 'm': /* max_exp */
					QE_ExpandNumber(p_ptr->max_exp, args->result);
					return;
			}
			chars = "acm";
			break;

		case KEYWORD_PY_LEVEL + 1:
			switch (args->which)
			{
				case 'c': /* lev */
					QE_ExpandNumber(p_ptr->lev, args->result);
					return;

				case 'm': /* max_lev */
					QE_ExpandNumber(p_ptr_max_lev, args->result);
					return;
			}
			chars = "cm";
			break;

		case KEYWORD_PY_AC + 1:
			switch (args->which)
			{
				case 'c': /* dis_ac */
					QE_ExpandNumber(p_ptr->dis_ac, args->result);
					return;

				case 't': /* dis_to_a */
					QE_ExpandNumber(p_ptr->dis_to_a, args->result);
					return;
			}
			chars = "ct";
			break;

		case KEYWORD_PY_NAME + 1:
			switch (args->which)
			{
				case 'c': /* name */
					ExtToUtf_ExpandString(op_ptr_full_name, args->result);
					return;
			}
			chars = "c";
			break;

		case KEYWORD_PY_TITLE + 1:
			switch (args->which)
			{
				case 'c': /* title */
					ExtToUtf_ExpandString((char *) player_title[p_ptr->pclass][(p_ptr->lev-1)/5], args->result);
					return;
			}
			chars = "c";
			break;

		case KEYWORD_PY_GOLD + 1:
			switch (args->which)
			{
				case 'c': /* gold */
					QE_ExpandNumber(p_ptr->au, args->result);
					return;
			}
			chars = "c";
			break;

#if defined(ZANGBANDTK)

		/* The character's race may change in ZAngband! */
		case KEYWORD_PY_RACE + 1:
			switch (args->which)
			{
				case 'c': /* race */
					ExtToUtf_ExpandString((char *) rp_ptr->title, args->result);
					return;
			}
			chars = "c";
			break;

#endif /* ZANGBANDTK */
	}

	Percents_Any(args, ExpandPy, chars);
}

/*
 * %-substitution for EVENT_SETTING
 */
static void ExpandSetting(QE_ExpandArgs *args)
{
	switch (args->which)
	{
		case 'c': /* value */
			QE_ExpandNumber((int) args->clientData, args->result);
			break;

		default:
			Percents_Any(args, ExpandSetting, "c");
			break;
	}
}

/*
 * %-substitution for EVENT_STATUS
 */
static void ExpandStatus(QE_ExpandArgs *args)
{
	struct {char *format; int value;} *clientData = args->clientData;

	switch (args->which)
	{
		case 'f': /* format */
			QE_ExpandString(clientData->format, args->result);
			break;

		case 'v': /* value */
			QE_ExpandNumber(clientData->value, args->result);
			break;

		default:
			Percents_Any(args, ExpandStatus, "fv");
			break;
	}
}

/*
 * %-substitution for EVENT_TARGET
 */
static void ExpandTarget(QE_ExpandArgs *args)
{
	switch (args->which)
	{
		case 'w': /* who */
			QE_ExpandNumber(p_ptr_target_who, args->result);
			break;

		case 'r': /* r_idx */
			if (p_ptr_target_who > 0)
			{
				monster_type *m_ptr = &m_list[p_ptr_target_who];
				QE_ExpandNumber(m_ptr->r_idx, args->result);
			}
			else
			{
				QE_ExpandNumber(0, args->result);
			}
			break;

		case 'y': /* y */
			QE_ExpandNumber(p_ptr_target_row, args->result);
			break;

		case 'x': /* x */
			QE_ExpandNumber(p_ptr_target_col, args->result);
			break;

		case 'v': /* visibility */
			QE_ExpandNumber(target_vis, args->result);
			break;

		default:
			Percents_Any(args, ExpandTarget, "wryxv");
			break;
	}
}

/*
 * %-substitution for EVENT_TRACK
 */
static void ExpandTrack(QE_ExpandArgs *args)
{
	struct {int who; int y; int x;} *clientData = args->clientData;
	monster_type *m_ptr;

	switch (args->detail)
	{
		case KEYWORD_TRACK_EQUIPMENT + 1:
		case KEYWORD_TRACK_INVENTORY + 1:
		case KEYWORD_TRACK_MESSAGE + 1:
			ExpandGeneric(args);
			return;
	}

	switch (args->which)
	{
		case 'f': /* friend */
			m_ptr = &m_list[clientData->who];
			QE_ExpandNumber(monster_is_friend(m_ptr), args->result);
			break;

		case 'w': /* who */
			QE_ExpandNumber(clientData->who, args->result);
			break;

		case 'x': /* x */
			QE_ExpandNumber(clientData->x, args->result);
			break;

		case 'y': /* y */
			QE_ExpandNumber(clientData->y, args->result);
			break;

		default:
			Percents_Any(args, ExpandTrack, "fwxy");
			break;
	}
}

/*
 * Generate an EVENT_ASK quasi-event
 */
void Bind_Ask(int detail, int min, int max, int show)
{
	QE_Event event;
	struct {int min, max, show;} clientData;

	clientData.min = min;
	clientData.max = max;
	clientData.show = show;

	event.type = EVENT_ASK;
	event.detail = detail;
	event.clientData = (ClientData) &clientData;

	(void) QE_BindEvent(bindingTable, &event);
}

/*
 * Generate an EVENT_ASSIGN quasi-event
 */
void Bind_Assign(int to, int toIndex, t_assign *assignPtr)
{
	QE_Event event;
	struct {int toIndex; t_assign *assignPtr;} clientData;

	clientData.toIndex = toIndex;
	clientData.assignPtr = assignPtr;

	event.type = EVENT_ASSIGN;
	event.detail = to;
	event.clientData = (ClientData) &clientData;

	(void) QE_BindEvent(bindingTable, &event);
}

/*
 * Generate an EVENT_CHOOSE quasi-event
 */
void Bind_Choose(int detail, int other, int show)
{
	QE_Event event;
	struct {int other, show;} clientData;

	clientData.other = other;
	clientData.show = show;

	event.type = EVENT_CHOOSE;
	event.detail = detail;
	event.clientData = (ClientData) &clientData;

	(void) QE_BindEvent(bindingTable, &event);
}

/*
 * Generate an EVENT_CURSOR quasi-event
 */
void Bind_Cursor(int detail, int y, int x)
{
	QE_Event event;
	struct {int y, x;} clientData;

	clientData.y = y;
	clientData.x = x;

	event.type = EVENT_CURSOR;
	event.detail = detail;
	event.clientData = (ClientData) &clientData;

	(void) QE_BindEvent(bindingTable, &event);
}

/*
 * Generate a quasi-event
 */
void Bind_Generic(int eventType, int eventDetail)
{
	QE_Event event;

	event.type = eventType;
	event.detail = eventDetail;
	(void) QE_BindEvent(bindingTable, &event);
}

/*
 * Generate an EVENT_KEYMAP quasi-event
 */
void Bind_Keymap(int ch)
{
	QE_Event event;
	char string[3];

	if (iscntrl(ch))
	{
		(void) sprintf(string, "^%c", ch + 64);
	}
	else
	{
		(void) sprintf(string, "%c", ch);
	}

	event.type = EVENT_KEYMAP;
	event.detail = 0;
	event.clientData = (ClientData) string;

	(void) QE_BindEvent(bindingTable, &event);
}

/*
 * Generate an EVENT_POSITION quasi-event
 */
void Bind_Position(int who, int y1, int x1, int y2, int x2)
{
	QE_Event event;
	struct {int who, y1, x1, y2, x2;} clientData;

	clientData.who = who;
	clientData.y1 = y1;
	clientData.x1 = x1;
	clientData.y2 = y2;
	clientData.x2 = x2;

	event.type = EVENT_POSITION;
	event.detail = 0;
	event.clientData = (ClientData) &clientData;

	(void) QE_BindEvent(bindingTable, &event);
}

/*
 * Generate an EVENT_SETTING quasi-event
 */
void Bind_Setting(int detail, int value)
{
	QE_Event event;

	event.type = EVENT_SETTING;
	event.detail = detail;
	event.clientData = (ClientData) value;
	(void) QE_BindEvent(bindingTable, &event);
}

/*
 * Generate an EVENT_STATUS quasi-event
 */
void Bind_Status(int detail)
{
	QE_Event event;
	struct {char *format; int value;} clientData;

	clientData.format = player_status(detail - 1, &clientData.value);

	event.type = EVENT_STATUS;
	event.detail = detail;
	event.clientData = (ClientData) &clientData;
	(void) QE_BindEvent(bindingTable, &event);
}

/*
 * Generate an EVENT_TRACK quasi-event
 */
void Bind_Track(int detail, int who, int y, int x)
{
	QE_Event event;
	struct {int who; int y; int x;} clientData;

	/* .who is m_list[] for Track-health, r_info[] for Track-race */
	clientData.who = who;
	clientData.y = y;
	clientData.x = x;

	event.type = EVENT_TRACK;
	event.detail = detail;
	event.clientData = (ClientData) &clientData;

	(void) QE_BindEvent(bindingTable, &event);
}

static int qebind(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	return QE_BindCmd((QE_BindingTable) clientData, 0, objc, objv);
}

static int qeunbind(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	return QE_UnbindCmd((QE_BindingTable) clientData, 0, objc, objv);
}

static int qeconfigure(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	return QE_ConfigureCmd((QE_BindingTable) clientData, 0, objc, objv);
}

static int qedetailnames(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	char *eventName;

	if (objc != 2)
	{
		Tcl_WrongNumArgs(interp, 1, objv, "eventName");
		return TCL_ERROR;
	}
	eventName = Tcl_GetString(objv[1]);
	return QE_GetDetailNames((QE_BindingTable) clientData, eventName);
}

static int qeeventnames(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	if (objc != 1)
	{
		Tcl_WrongNumArgs(interp, 1, objv, (char *) NULL);
		return TCL_ERROR;
	}
	return QE_GetEventNames((QE_BindingTable) clientData);
}

static int qegenerate(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	return QE_GenerateCmd((QE_BindingTable) clientData, 0, objc, objv);
}

static int qeinstall(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	return QE_InstallCmd((QE_BindingTable) clientData, 0, objc, objv);
}

static int qelinkage(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	return QE_LinkageCmd((QE_BindingTable) clientData, 0, objc, objv);
}

static int qeuninstall(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	return QE_UninstallCmd((QE_BindingTable) clientData, 0, objc, objv);
}

/*
 * Initialize the quasi-event package and define events
 */
void init_bindings(void)
{
	EventInfo *eiPtr;
	int i;

	/* One-time package initialization */
	QE_BindInit(g_interp);

	bindingTable = QE_CreateBindingTable(g_interp);

	/*
	 * Create the "qebind" command. Because the bindingTable is passed
	 * as ClientData to the command, we are only allowed one binding
	 * table per interpreter.
	 */
	Tcl_CreateObjCommand(g_interp, "qebind", qebind,
		(ClientData) bindingTable, NULL);

	Tcl_CreateObjCommand(g_interp, "qeunbind", qeunbind,
		(ClientData) bindingTable, NULL);

	/* qeconfigure lets scripts configure bindings */
	Tcl_CreateObjCommand(g_interp, "qeconfigure", qeconfigure,
		(ClientData) bindingTable, NULL);

	/* qegenerate lets scripts generate events */
	Tcl_CreateObjCommand(g_interp, "qegenerate", qegenerate,
		(ClientData) bindingTable, NULL);

	/* qeinstall lets scripts add event types and details */
	Tcl_CreateObjCommand(g_interp, "qeinstall", qeinstall,
		(ClientData) bindingTable, NULL);

	/* qeuninstall lets scripts remove event types and details */
	Tcl_CreateObjCommand(g_interp, "qeuninstall", qeuninstall,
		(ClientData) bindingTable, NULL);

	/*  */
	Tcl_CreateObjCommand(g_interp, "qeeventnames", qeeventnames,
		(ClientData) bindingTable, NULL);

	/*  */
	Tcl_CreateObjCommand(g_interp, "qedetailnames", qedetailnames,
		(ClientData) bindingTable, NULL);

	/* */
	Tcl_CreateObjCommand(g_interp, "qelinkage", qelinkage,
		(ClientData) bindingTable, NULL);

	/* Hack -- Detail names determined at run-time */
	eventArray[EVENT_SETTING - 1].detail = keyword_setting;

	/* XXX FIXME: All this will break if QE_InstallXXX doesn't begin
	 * at 1 for event/detail codes */

	/* Define our event types */
	for (eiPtr = eventArray; eiPtr->name; eiPtr++)
	{
		/* Define the event type */
		eiPtr->type = QE_InstallEvent(bindingTable, eiPtr->name, eiPtr->expand);

		/* Check each detail */
		for (i = 0; eiPtr->detail && eiPtr->detail[i]; i++)
		{
			/* Define a detail for this event type */
			QE_InstallDetail(bindingTable, (char *) eiPtr->detail[i], eiPtr->type, NULL);
		}
	}
}

