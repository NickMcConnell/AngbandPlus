/* File: iomod.c */

/*
 * Copyright (c) 1998 Keldon Jones
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.
 */

/*
 * This file provides a Python interface to Angband's functions to interact
 * with the user (inkey, msg_print, etc).
 */

#include "angband.h"

/*
 * Print a message
 */
static PyObject *iomsg_print(PyObject *self, PyObject *args)
{
	char *buf;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, "s", &buf))
		return NULL;

	/* Print message */
	msg_print(buf);

	/* Return nothing */
	Py_INCREF(Py_None);
	return Py_None;
}

/*
 * Get a key.  If the optional argument is TRUE, do not wait for the key to
 * be pressed.
 */
static PyObject *ioinkey(PyObject *self, PyObject *args)
{
	char key;
	int scan = FALSE;

	/* Parse argument */
	if (!PyArg_ParseTuple(args, "|i", &scan))
		return NULL;

	/* Set scan flag */
	inkey_scan = scan;

	/* Get a key */
	key = inkey();

	/* Return "None" if no key pressed */
	if (!key)
	{
		/* Additional reference, return */
		Py_INCREF(Py_None);
		return Py_None;
	}

	/* Return key */
	return Py_BuildValue("c", key);
}

/*
 * Get a string from the user.  This function is passed a prompt string and
 * an optional default value.
 */
static PyObject *ioget_string(PyObject *self, PyObject *args)
{
	char buf[1024], *def = NULL, *prompt;

	/* Parse argument */
	if (!PyArg_ParseTuple(args, "s|s", &prompt, &def))
		return NULL;

	/* Copy default */
	if (def)
		strcpy(buf, def);
	else strcpy(buf, "");

	/* Get string */
	get_string(prompt, buf, 1024);

	/* Return string */
	return Py_BuildValue("s", buf);
}

/*
 * Verify something with the user
 */
static PyObject *ioget_check(PyObject *self, PyObject *args)
{
	char *prompt;
	bool result;

	/* Parse argument */
	if (!PyArg_ParseTuple(args, "s", &prompt))
		return NULL;

	/* Get check */
	result = get_check(prompt);

	/* Return result */
	return Py_BuildValue("b", result);
}

/*
 * Get an aiming direction
 */
static PyObject *ioget_aim_dir(PyObject *self, PyObject *args)
{
	int dir;
	bool result;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, ""))
		return NULL;

	/* Get direction */
	result = get_aim_dir(&dir);

	/* Check for failure */
	if (!result) dir = 0;

	/* Return */
	return Py_BuildValue("i", dir);
}

/*
 * Get a "repeated" direction
 */
static PyObject *ioget_rep_dir(PyObject *self, PyObject *args)
{
	int dir;
	bool result;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, ""))
		return NULL;

	/* Get direction */
	result = get_rep_dir(&dir);

	/* Check for failure */
	if (!result) dir = 0;

	/* Return */
	return Py_BuildValue("i", dir);
}

/*
 * Sleep for x microseconds
 */
static PyObject *iousleep(PyObject *self, PyObject *args)
{
	int usecs;

	/* Parse argument */
	if (!PyArg_ParseTuple(args, "i", &usecs))
		return NULL;

	/* Sleep */
	usleep(usecs);

	/* Return nothing */
	Py_INCREF(Py_None);
	return Py_None;
}

/*
 * Play a sound
 */
static PyObject *iosound(PyObject *self, PyObject *args)
{
	int val;

	/* Parse argument */
	if (!PyArg_ParseTuple(args, "i", &val))
		return NULL;

	/* Play sound */
	sound(val);

	/* Return nothing */
	Py_INCREF(Py_None);
	return Py_None;
}

/*
 * Clear the screen
 */
static PyObject *ioclear(PyObject *self, PyObject *args)
{
	/* Parse arguments */
	if (!PyArg_ParseTuple(args, ""))
		return NULL;

	/* Clear screen */
	Term_clear();

	/* Return nothing */
	Py_INCREF(Py_None);
	return Py_None;
}

/*
 * Refresh the screen
 */
static PyObject *iofresh(PyObject *self, PyObject *args)
{
	/* Parse arguments */
	if (!PyArg_ParseTuple(args, ""))
		return NULL;

	/* Refresh */
	Term_fresh();

	/* Return nothing */
	Py_INCREF(Py_None);
	return Py_None;
}

/*
 * Save the screen contents
 */
static PyObject *iosave(PyObject *self, PyObject *args)
{
	/* Parse arguments */
	if (!PyArg_ParseTuple(args, ""))
		return NULL;

	/* Save */
	Term_save();

	/* Return nothing */
	Py_INCREF(Py_None);
	return Py_None;
}

/*
 * Reload the screen contents
 */
static PyObject *ioload(PyObject *self, PyObject *args)
{
	/* Parse arguments */
	if (!PyArg_ParseTuple(args, ""))
		return NULL;

	/* Load */
	Term_load();

	/* Return nothing */
	Py_INCREF(Py_None);
	return Py_None;
}

/*
 * Display a string on the screen.
 */
static PyObject *ioput_str(PyObject *self, PyObject *args)
{
	int y, x;
	byte attr = TERM_WHITE;
	char *str;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, "(ii)s|b", &y, &x, &str, &attr))
		return NULL;

	/* Put the string */
	c_put_str(attr, str, y, x);

	/* Return nothing */
	Py_INCREF(Py_None);
	return Py_None;
}

/*
 * Erase n characters starting from (y, x).
 */
static PyObject *ioerase(PyObject *self, PyObject *args)
{
	int y, x = 0, n = 255;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, "(ii)|i", &y, &x, &n))
		return NULL;

	/* Erase */
	Term_erase(x, y, n);

	/* Return nothing */
	Py_INCREF(Py_None);
	return Py_None;
}

/*
 * Perform an "xtra" event.
 */
static PyObject *ioxtra(PyObject *self, PyObject *args)
{
	int n, v = 0;
	errr retval;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, "i|i", &n, &v))
		return NULL;

	/* Perform event */
	retval = Term_xtra(n, v);

	/* Return */
	return Py_BuildValue("i", retval);
}

/*
 * Get the cursor state and position
 */
static PyObject *ioget_cursor(PyObject *self, PyObject *args)
{
	int v, x, y;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, ""))
		return NULL;

	/* Acquire information */
	Term_get_cursor(&v);
	Term_locate(&x, &y);

	/* Return */
	return Py_BuildValue("(i(ii))", v, x, y);
}

/*
 * Set the cursor state and position
 */
static PyObject *ioset_cursor(PyObject *self, PyObject *args)
{
	int v, x, y;

	/* Acquire default location */
	Term_locate(&x, &y);

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, "i|(ii)", &v, &x, &y))
		return NULL;

	/* Set cursor state */
	Term_set_cursor(v);
	Term_gotoxy(x, y);

	/* Return nothing */
	Py_INCREF(Py_None);
	return Py_None;
}

/*
 * Extract the attr/char at a given location
 */
static PyObject *iowhat(PyObject *self, PyObject *args)
{
	int x, y;
	char c;
	byte a;

	/* Acquire default location */
	Term_locate(&x, &y);

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, "|(ii)", &x, &y))
		return NULL;

	/* Grab attr/char */
	Term_what(x, y, &a, &c);

	/* Return */
	return Py_BuildValue("(bc)", a, c);
}

/*
 * Put a single attr/char on the screen
 */
static PyObject *ioputch(PyObject *self, PyObject *args)
{
	int x, y;
	char c;
	byte a;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, "(ii)(bc)", &x, &y, &a, &c))
		return NULL;

	/* Put character */
	Term_putch(x, y, a, c);

	/* Return nothing */
	Py_INCREF(Py_None);
	return Py_None;
}

/*
 * Method table for all functions provided by the module
 */
static PyMethodDef ioMethods[] =
{
	{ "msg_print", iomsg_print, 1 },
	{ "inkey", ioinkey, 1 },
	{ "get_string", ioget_string, 1 },
	{ "get_check", ioget_check, 1 },
	{ "get_aim_dir", ioget_aim_dir, 1 },
	{ "get_rep_dir", ioget_rep_dir, 1 },
	{ "usleep", iousleep, 1 },
	{ "sound", iosound, 1 },
	{ "clear", ioclear, 1 },
	{ "fresh", iofresh, 1 },
	{ "save", iosave, 1 },
	{ "load", ioload, 1 },
	{ "put_str", ioput_str, 1 },
	{ "erase", ioerase, 1 },
	{ "xtra", ioxtra, 1 },
	{ "get_cursor", ioget_cursor, 1 },
	{ "set_cursor", ioset_cursor, 1 },
	{ "what", iowhat, 1 },
	{ "putch", ioputch, 1 },
	{ NULL, NULL }
};

/*
 * Helper function.  Add an integer to a dictionary.
 */
static void add_constant(PyObject *dict, char *name, int value)
{
	PyObject *temp;

	/* Form an object from the value */
	temp = Py_BuildValue("i", value);

	/* Set item */
	PyDict_SetItemString(dict, name, temp);

	/* Delete object */
	Py_DECREF(temp);
}

/*
 * Initialize the I/O module
 */
void initio(void)
{
	PyObject *m, *d;

	/* Tell the interpreter to insert the module */
	m = Py_InitModule("io", ioMethods);

	/* Add some symbolic constants to the module */
	d = PyModule_GetDict(m);

	/* Add each constant */
	add_constant(d, "TERM_XTRA_EVENT", TERM_XTRA_EVENT);
	add_constant(d, "TERM_XTRA_FLUSH", TERM_XTRA_FLUSH);
	add_constant(d, "TERM_XTRA_CLEAR", TERM_XTRA_CLEAR);
	add_constant(d, "TERM_XTRA_SHAPE", TERM_XTRA_SHAPE);
	add_constant(d, "TERM_XTRA_FROSH", TERM_XTRA_FROSH);
	add_constant(d, "TERM_XTRA_FRESH", TERM_XTRA_FRESH);
	add_constant(d, "TERM_XTRA_NOISE", TERM_XTRA_NOISE);
	add_constant(d, "TERM_XTRA_SOUND", TERM_XTRA_SOUND);
	add_constant(d, "TERM_XTRA_BORED", TERM_XTRA_BORED);
	add_constant(d, "TERM_XTRA_REACT", TERM_XTRA_REACT);
	add_constant(d, "TERM_XTRA_ALIVE", TERM_XTRA_ALIVE);
	add_constant(d, "TERM_XTRA_LEVEL", TERM_XTRA_LEVEL);
	add_constant(d, "TERM_XTRA_DELAY", TERM_XTRA_DELAY);
	add_constant(d, "TERM_DARK", TERM_DARK);
	add_constant(d, "TERM_WHITE", TERM_WHITE);
	add_constant(d, "TERM_SLATE", TERM_SLATE);
	add_constant(d, "TERM_ORANGE", TERM_ORANGE);
	add_constant(d, "TERM_RED", TERM_RED);
	add_constant(d, "TERM_GREEN", TERM_GREEN);
	add_constant(d, "TERM_BLUE", TERM_BLUE);
	add_constant(d, "TERM_UMBER", TERM_UMBER);
	add_constant(d, "TERM_L_DARK", TERM_L_DARK);
	add_constant(d, "TERM_L_WHITE", TERM_L_WHITE);
	add_constant(d, "TERM_VIOLET", TERM_VIOLET);
	add_constant(d, "TERM_YELLOW", TERM_YELLOW);
	add_constant(d, "TERM_L_RED", TERM_L_RED);
	add_constant(d, "TERM_L_GREEN", TERM_L_GREEN);
	add_constant(d, "TERM_L_BLUE", TERM_L_BLUE);
	add_constant(d, "TERM_L_UMBER", TERM_L_UMBER);
	add_constant(d, "SOUND_HIT", SOUND_HIT);
	add_constant(d, "SOUND_MISS", SOUND_MISS);
	add_constant(d, "SOUND_FLEE", SOUND_FLEE);
	add_constant(d, "SOUND_DROP", SOUND_DROP);
	add_constant(d, "SOUND_KILL", SOUND_KILL);
	add_constant(d, "SOUND_LEVEL", SOUND_LEVEL);
	add_constant(d, "SOUND_DEATH", SOUND_DEATH);
	add_constant(d, "SOUND_STUDY", SOUND_STUDY);
	add_constant(d, "SOUND_TELEPORT", SOUND_TELEPORT);
	add_constant(d, "SOUND_SHOOT", SOUND_SHOOT);
	add_constant(d, "SOUND_QUAFF", SOUND_QUAFF);
	add_constant(d, "SOUND_ZAP", SOUND_ZAP);
	add_constant(d, "SOUND_WALK", SOUND_WALK);
	add_constant(d, "SOUND_TPOTHER", SOUND_TPOTHER);
	add_constant(d, "SOUND_HITWALL", SOUND_HITWALL);
	add_constant(d, "SOUND_EAT", SOUND_EAT);
	add_constant(d, "SOUND_STORE1", SOUND_STORE1);
	add_constant(d, "SOUND_STORE2", SOUND_STORE2);
	add_constant(d, "SOUND_STORE3", SOUND_STORE3);
	add_constant(d, "SOUND_STORE4", SOUND_STORE4);
	add_constant(d, "SOUND_DIG", SOUND_DIG);
	add_constant(d, "SOUND_OPENDOOR", SOUND_OPENDOOR);
	add_constant(d, "SOUND_SHUTDOOR", SOUND_SHUTDOOR);
	add_constant(d, "SOUND_TPLEVEL", SOUND_TPLEVEL);
}

