/* File: monmod.c */

/*
 * Copyright (c) 1998 Keldon Jones
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.
 */

/*
 * This file provides a Python interface to Angband's "monster" data type,
 * and provide various methods for handling and manipulating them.
 */

#include "angband.h"

/*
 * The "monster" class.
 */
typedef struct MonObject MonObject;

struct MonObject
{
	PyObject_HEAD		/* Stuff common to all classes */
	int m_idx;		/* Index of monster in list */
	PyObject *mon_attr;	/* Dictionary of attributes */
};

/*
 * We need this "Mon_Type" variable throughout this module, but to define
 * it completely, we need function pointers that are defined later.  So
 * we need to "prototype" the variable by using the "staticforward"
 * keyword.  So strange....
 */
staticforward PyTypeObject Mon_Type;

/*
 * Macro to test whether an object is an Angband object.
 */
#define MonObject_Check(v)	((v)->ob_type == &Mon_Type)

/*
 * Pull a monster from the list.
 */
static MonObject *getMonObject(int m_idx)
{
	MonObject *self;

	/* Allocate memory (perhaps use Angband macros instead?) */
	self = PyObject_NEW(MonObject, &Mon_Type);

	/* Check failure */
	if (!self) return NULL;

	/* Set monster index */
	self->m_idx = m_idx;

	/* Create attribute dictionary, check for failure */
	if (!(self->mon_attr = PyDict_New()))
		return NULL;

	/* Return object */
	return self;
}

/*
 * Destructor.  Decrement references on all member variables and delete
 * memory for the object.
 */
static void Mon_dealloc(MonObject *self)
{
	/* Dealloc obj_attributes */
	Py_DECREF(self->mon_attr);

	/* Dealloc object */
	PyMem_DEL(self);
}

/*
 * Return a string representation of the monster
 */
static PyObject *Mon_desc(MonObject *self, PyObject *args)
{
	char buf[1024];
	int mode = 0x88;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, "|i", &mode))
		return NULL;

	/* Request a monster description */
	monster_desc(buf, &m_list[self->m_idx], mode);

	/* Form a string */
	return PyString_FromString(buf);
}

/*
 * Move a monster to a given location
 */
static PyObject *Mon_move_to(MonObject *self, PyObject *args)
{
	int y, x;
	monster_type *m_ptr = &m_list[self->m_idx];

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, "(ii)", &y, &x))
		return NULL;

	/* Swap the monster with whatever is in that spot */
	monster_swap(y, x, m_ptr->fy, m_ptr->fx);

	/* Return nothing */
	Py_INCREF(Py_None);
	return Py_None;
}

/*
 * Have a monster take damage
 */
static PyObject *Mon_take_hit(MonObject *self, PyObject *args)
{
	int dam;
	bool dead, fear;
	char *note = NULL;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, "i|s", &dam, &note))
		return NULL;

	/* Cause damage */
	dead = mon_take_hit(self->m_idx, dam, &fear, note);

	/* Return dead and afraid flags */
	return Py_BuildValue("(ii)", dead, fear);
}

/*
 * Table of class methods
 */
static PyMethodDef Mon_methods[] =
{
	{ "desc", (PyCFunction)Mon_desc, 1 },
	{ "move_to", (PyCFunction)Mon_move_to, 1 },
	{ "take_hit", (PyCFunction)Mon_take_hit, 1 },
	{ NULL, NULL }
};

/* XXX XXX Try to save some space */
#define LOOK_FIELD(text, field) \
	if (streq(name, text)) \
		return Py_BuildValue("i", m_list[self->m_idx].field)

/*
 * Return a monster attribute
 */
static PyObject *Mon_getattr(MonObject *self, char *name)
{
	PyObject *v;

	/* Attempt to find a "standard" field */
	LOOK_FIELD("r_idx", r_idx);
	LOOK_FIELD("fy", fy);
	LOOK_FIELD("fx", fx);
	LOOK_FIELD("hp", hp);
	LOOK_FIELD("maxhp", maxhp);
	LOOK_FIELD("csleep", csleep);
	LOOK_FIELD("mspeed", mspeed);
	LOOK_FIELD("energy", energy);
	LOOK_FIELD("stunned", stunned);
	LOOK_FIELD("confused", confused);
	LOOK_FIELD("monfear", monfear);
	LOOK_FIELD("cdis", cdis);
	LOOK_FIELD("mflag", mflag);
	LOOK_FIELD("ml", ml);
	LOOK_FIELD("hold_o_idx", hold_o_idx);
	LOOK_FIELD("ty", ty);
	LOOK_FIELD("tx", tx);
	LOOK_FIELD("spell", spell);
	LOOK_FIELD("friendly", friendly);
	LOOK_FIELD("expansion", expansion);
#ifdef DRS_SMART_OPTIONS
	LOOK_FIELD("smart", smart);
#endif

	/* Try to find in dictionary */
	v = PyDict_GetItemString(self->mon_attr, name);

	/* Check for success */
	if (v)
	{
		/* Return result */
		Py_INCREF(v);
		return v;
	}

	/* Try to find a method */
	return Py_FindMethod(Mon_methods, (PyObject *)self, name);
}

/* XXX XXX Try to save space */
#define SET_FIELD(text, field) \
	if (!done && streq(name, text)) \
	{ \
		m_list[self->m_idx].field = value; \
		done = TRUE; \
	}
/*
 * Set an monster attribute
 */
static int Mon_setattr(MonObject *self, char *name, PyObject *v)
{
	int value = 0;
	bool done = FALSE;

	/* Extract the value from the Python object */
	if (v) value = PyInt_AsLong(v);

	/* Attempt to set a "standard" field */
	SET_FIELD("r_idx", r_idx);
	SET_FIELD("fy", fy);
	SET_FIELD("fx", fx);
	SET_FIELD("hp", hp);
	SET_FIELD("maxhp", maxhp);
	SET_FIELD("csleep", csleep);
	SET_FIELD("mspeed", mspeed);
	SET_FIELD("energy", energy);
	SET_FIELD("stunned", stunned);
	SET_FIELD("confused", confused);
	SET_FIELD("monfear", monfear);
	SET_FIELD("cdis", cdis);
	SET_FIELD("mflag", mflag);
	SET_FIELD("ml", ml);
	SET_FIELD("hold_o_idx", hold_o_idx);
	SET_FIELD("ty", ty);
	SET_FIELD("tx", tx);
	SET_FIELD("spell", spell);
	SET_FIELD("friendly", friendly);
	SET_FIELD("expansion", expansion);
#ifdef DRS_SMART_OPTIONS
	SET_FIELD("smart", smart);
#endif

	/* Check for completion */
	if (done)
	{
		/* Update monster health bar */
		if (p_ptr->health_who == self->m_idx)
		{
			/* Redraw */
			p_ptr->redraw |= (PR_HEALTH);
		}

		/* Done */
		return 0;
	}

	/* Should we delete the item? */
	if (!v)
	{
		/* Attempt to delete */
		int retval = PyDict_DelItemString(self->mon_attr, name);

		/* Check for failure */
		if (retval < 0)
		{
			/* Set error string */
			PyErr_SetString(PyExc_AttributeError,
				"delete non-existing Mon attribute");
		}

		return retval;
	}

	/* Set attribute */
	return PyDict_SetItemString(self->mon_attr, name, v);
}


/*
 * Here is the complete definition of that bizarre "Mon_Type" that was
 * "prototyped" above.
 */
staticforward PyTypeObject Mon_Type =
{
	PyObject_HEAD_INIT(&PyType_Type)	/* Standard stuff */
	0,					/* Size */
	"Mon",					/* Name */
	sizeof(MonObject),			/* Basic Size */
	0,					/* Item Size */
	(destructor)Mon_dealloc,		/* Destructor */
	0,					/* Print */
	(getattrfunc)Mon_getattr,		/* Attribute getter */
	(setattrfunc)Mon_setattr,		/* Attribute setter */
	0,
	0,
	0,
	0,
	0,
	0
};

/*
 * Module function to return a Mon object from a monster index.
 */
static PyObject *monster_get(PyObject *self, PyObject *args)
{
	int m_idx = 0;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, "i", &m_idx))
		return NULL;

	/* Create an Mon and return it */
	return (PyObject *)getMonObject(m_idx);
}

/*
 * Summon a monster of a given type at a given location.
 */
static PyObject *monster_summon(PyObject *self, PyObject *args)
{
	int r_idx, y, x, slp = FALSE, grp = TRUE;
	int retval;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, "(ii)i|ii", &y, &x, &r_idx, &slp, &grp))
		return NULL;

	/* Summon */
	retval = place_monster_aux(y, x, r_idx, slp, grp);

	/* Return */
	return Py_BuildValue("i", retval);
}

/*
 * Summon a type of monster, given location and level.
 */
static PyObject *monster_summon_specific(PyObject *self, PyObject *args)
{
	int y, x, lev, type;
	int retval;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, "(ii)ii", &y, &x, &lev, &type))
		return NULL;

	/* Summon */
	retval = summon_specific(y, x, lev, type);

	/* Return */
	return Py_BuildValue("i", retval);
}

/*
 * Delete a monster by index
 */
static PyObject *monster_delete(PyObject *self, PyObject *args)
{
	int m_idx;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, "i", &m_idx))
		return NULL;

	/* Delete */
	delete_monster_idx(m_idx);

	/* Return nothing */
	Py_INCREF(Py_None);
	return Py_None;
}

/*
 * Return number of monster entries in use
 */
static PyObject *monster_m_max(PyObject *self, PyObject *args)
{
	/* Parse arguments */
	if (!PyArg_ParseTuple(args, ""))
		return NULL;

	/* Return */
	return Py_BuildValue("i", m_max);
}

/*
 * Function table for this module
 */
static PyMethodDef mon_methods[] =
{
	{ "get", monster_get, 1 },
	{ "summon", monster_summon, 1 },
	{ "summon_specific", monster_summon_specific, 1 },
	{ "delete", monster_delete, 1 },
	{ "m_max", monster_m_max, 1 },
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
 * Initialization function for this module.
 */
void initmonster()
{
	PyObject *m, *d;

	/* Create the module */
	m = Py_InitModule("monster", mon_methods);
	
	/* Get the module's dictionary */
	d = PyModule_GetDict(m);

	/* Add some symbolic constants to the module */
	add_constant(d, "SUMMON_ANT", SUMMON_ANT);
	add_constant(d, "SUMMON_SPIDER", SUMMON_SPIDER);
	add_constant(d, "SUMMON_HOUND", SUMMON_HOUND);
	add_constant(d, "SUMMON_HYDRA", SUMMON_HYDRA);
	add_constant(d, "SUMMON_ANGEL", SUMMON_ANGEL);
	add_constant(d, "SUMMON_DEMON", SUMMON_DEMON);
	add_constant(d, "SUMMON_UNDEAD", SUMMON_UNDEAD);
	add_constant(d, "SUMMON_DRAGON", SUMMON_DRAGON);
	add_constant(d, "SUMMON_HI_UNDEAD", SUMMON_HI_UNDEAD);
	add_constant(d, "SUMMON_HI_DRAGON", SUMMON_HI_DRAGON);
	add_constant(d, "SUMMON_WRAITH", SUMMON_WRAITH);
	add_constant(d, "SUMMON_UNIQUE", SUMMON_UNIQUE);
}
