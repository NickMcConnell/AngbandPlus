/* File: objmod.c */

#ifdef USE_PYTHON

/*
 * Copyright (c) 1998 Keldon Jones
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.
 */

/*
 * This file provides a Python interface to Angband's "object" data type,
 * and provide various methods for handling and manipulating them.
 */

#include "angband.h"

/*
 * The "object" class.  I can already tell that I'm going to get confused
 * between the two different uses of "object" in this file.
 */
typedef struct ObjObject ObjObject;

struct ObjObject
{
	PyObject_HEAD		/* Stuff common to all classes */
	PyObject *obj_attr;	/* Dictionary of attributes */
};

/*
 * We need this "Obj_Type" variable throughout this module, but to define
 * it completely, we need function pointers that are defined later.  So
 * we need to "prototype" the variable by using the "staticforward"
 * keyword.  So strange....
 */
staticforward PyTypeObject Obj_Type;

/*
 * Macro to test whether an object is an Angband object.
 */
#define ObjObject_Check(v)	((v)->ob_type == &Obj_Type)

/*
 * Helper function.  Sets an attribute given a string and an integer value.
 */
static void Obj_set_attribute(ObjObject *self, char *name, int value)
{
	PyObject *temp;

	/* Create object for value */
	temp = Py_BuildValue("i", value);

	/* Set attribute */
	PyDict_SetItemString(self->obj_attr, name, temp);

	/* Delete object */
	Py_DECREF(temp);
}

/*
 * Helper function.  Gets an integer attribute by name
 */
static int Obj_get_attribute(ObjObject *self, char *name)
{
	PyObject *temp;
	int retval;

	/* Try to get attribute */
	temp = PyDict_GetItemString(self->obj_attr, name);

	/* Check for failure */
	if (!temp) return -1;

	/* Extract integer */
	retval = PyInt_AsLong(temp);

	return retval;
}

/*
 * Copy the fields from an "object type" to an Obj class.
 */
static void object_to_Obj(ObjObject *self, object_type *o_ptr)
{
	PyObject *temp;

	Obj_set_attribute(self, "k_idx", o_ptr->k_idx);
	Obj_set_attribute(self, "iy", o_ptr->iy);
	Obj_set_attribute(self, "ix", o_ptr->ix);
	Obj_set_attribute(self, "tval", o_ptr->tval);
	Obj_set_attribute(self, "sval", o_ptr->sval);
	Obj_set_attribute(self, "pval", o_ptr->pval);
        Obj_set_attribute(self, "pval2", o_ptr->pval2);
        Obj_set_attribute(self, "pval3", o_ptr->pval3);
	Obj_set_attribute(self, "discount", o_ptr->discount);
	Obj_set_attribute(self, "number", o_ptr->number);
	Obj_set_attribute(self, "weight", o_ptr->weight);
	Obj_set_attribute(self, "name1", o_ptr->name1);
	Obj_set_attribute(self, "name2", o_ptr->name2);
	Obj_set_attribute(self, "xtra1", o_ptr->xtra1);
	Obj_set_attribute(self, "xtra2", o_ptr->xtra2);
	Obj_set_attribute(self, "to_h", o_ptr->to_h);
	Obj_set_attribute(self, "to_d", o_ptr->to_d);
	Obj_set_attribute(self, "to_a", o_ptr->to_a);
	Obj_set_attribute(self, "ac", o_ptr->ac);
	Obj_set_attribute(self, "dd", o_ptr->dd);
	Obj_set_attribute(self, "ds", o_ptr->ds);
	Obj_set_attribute(self, "timeout", o_ptr->timeout);
	Obj_set_attribute(self, "ident", o_ptr->ident);
	Obj_set_attribute(self, "marked", o_ptr->marked);
        Obj_set_attribute(self, "art_name", o_ptr->art_name);
        Obj_set_attribute(self, "art_flags1", o_ptr->art_flags1);
        Obj_set_attribute(self, "art_flags2", o_ptr->art_flags2);
        Obj_set_attribute(self, "art_flags3", o_ptr->art_flags3);
        Obj_set_attribute(self, "art_flags4", o_ptr->art_flags4);
	Obj_set_attribute(self, "next_o_idx", o_ptr->next_o_idx);
	Obj_set_attribute(self, "held_m_idx", o_ptr->held_m_idx);

	/* Set inscription */
	if (o_ptr->note)
	{
		/* Grab value */
		temp = Py_BuildValue("s", quark_str(o_ptr->note));

		/* Add to Object */
		PyDict_SetItemString(self->obj_attr, "note", temp);

		/* Delete temp object */
		Py_DECREF(temp);
	}
	else
	{
		/* Default value */
		temp = Py_BuildValue("s", "");

		/* Add to Object */
		PyDict_SetItemString(self->obj_attr, "note", temp);

		/* Delete temp object */
		Py_DECREF(temp);
	}
}

/*
 * Copy the fields from the Obj class to an "object_type".
 */
static void Obj_to_object(ObjObject *self, object_type *o_ptr)
{
	PyObject *temp;
	char *value;

	o_ptr->k_idx = Obj_get_attribute(self, "k_idx");
	o_ptr->iy = Obj_get_attribute(self, "iy");
	o_ptr->ix = Obj_get_attribute(self, "ix");
	o_ptr->tval = Obj_get_attribute(self, "tval");
	o_ptr->sval = Obj_get_attribute(self, "sval");
	o_ptr->pval = Obj_get_attribute(self, "pval");
        o_ptr->pval2 = Obj_get_attribute(self, "pval3");
        o_ptr->pval3 = Obj_get_attribute(self, "pval2");
	o_ptr->discount = Obj_get_attribute(self, "discount");
	o_ptr->number = Obj_get_attribute(self, "number");
	o_ptr->weight = Obj_get_attribute(self, "weight");
	o_ptr->name1 = Obj_get_attribute(self, "name1");
	o_ptr->name2 = Obj_get_attribute(self, "name2");
	o_ptr->xtra1 = Obj_get_attribute(self, "xtra1");
	o_ptr->xtra2 = Obj_get_attribute(self, "xtra2");
	o_ptr->to_h = Obj_get_attribute(self, "to_h");
	o_ptr->to_d = Obj_get_attribute(self, "to_d");
	o_ptr->to_a = Obj_get_attribute(self, "to_a");
	o_ptr->ac = Obj_get_attribute(self, "ac");
	o_ptr->dd = Obj_get_attribute(self, "dd");
	o_ptr->ds = Obj_get_attribute(self, "ds");
	o_ptr->timeout = Obj_get_attribute(self, "timeout");
	o_ptr->ident = Obj_get_attribute(self, "ident");
	o_ptr->marked = Obj_get_attribute(self, "marked");
        o_ptr->art_name = Obj_get_attribute(self, "art_name");
        o_ptr->art_flags1 = Obj_get_attribute(self, "art_flags1");
        o_ptr->art_flags2 = Obj_get_attribute(self, "art_flags2");
        o_ptr->art_flags3 = Obj_get_attribute(self, "art_flags3");
        o_ptr->art_flags4 = Obj_get_attribute(self, "art_flags4");
	o_ptr->next_o_idx = Obj_get_attribute(self, "next_o_idx");
	o_ptr->held_m_idx = Obj_get_attribute(self, "held_m_idx");

	/* Assume no inscription */
	o_ptr->note = 0;

	/* Get inscription */
	temp = PyDict_GetItemString(self->obj_attr, "note");

	/* Check for failure */
	if (!temp) return;

	/* Extract string */
	value = PyString_AsString(temp);

	/* Check for failure */
	if (!value) return;

	/* Check for valid inscription */
	if (strlen(value))
	{
		/* Set inscription */
		o_ptr->note = quark_add(value);
	}
}

/*
 * Constructor.  Allocate memory for the object.
 */
static ObjObject *newObjObject(int k_idx)
{
	ObjObject *self;
	object_type forge;

	/* Allocate memory (perhaps use Angband macros instead?) */
	self = PyObject_NEW(ObjObject, &Obj_Type);

	/* Check failure */
	if (!self) return NULL;

	/* Create attribute dictionary, check for failure */
	if (!(self->obj_attr = PyDict_New()))
		return NULL;

	/* Prepare the forge object based on the kind index */
	object_prep(&forge, k_idx);

	/* Copy the object type to the Obj class */
	object_to_Obj(self, &forge);

	/* Return object */
	return self;
}

/*
 * Destructor.  Decrement references on all member variables and delete
 * memory for the object.
 */
static void Obj_dealloc(ObjObject *self)
{
	/* Dealloc obj_attributes */
	Py_XDECREF(self->obj_attr);

	/* Dealloc object */
	PyMem_DEL(self);
}

/*
 * Drop an item near a location
 */
static PyObject *Obj_drop_near(ObjObject *self, PyObject *args)
{
        int y, x, chance;
	object_type forge;

	/* Parse arguments */
        if (!PyArg_ParseTuple(args, "iii", &chance, &y, &x))
		return NULL;

	/* Copy the Obj class to the object */
	Obj_to_object(self, &forge);

	/* Drop the item */
        drop_near(&forge, chance, y, x);

	/* Return nothing */
	Py_INCREF(Py_None);
	return (Py_None);
}

/*
 * Give the item to the player, return slot carried in
 */
static PyObject *Obj_carry(ObjObject *self, PyObject *args)
{
	object_type forge;
	int slot;
        bool final = FALSE;

	/* Parse arguments */
        if (!PyArg_ParseTuple(args, "b", &final))
		return NULL;

	/* Copy the Obj class to the object */
	Obj_to_object(self, &forge);

	/* Carry the item */
        slot = inven_carry(&forge, final);

	/* Return slot */
	return Py_BuildValue("i", slot);
}

/*
 * Apply some magic to an item
 */
static PyObject *Obj_apply_magic(ObjObject *self, PyObject *args)
{
	object_type forge;
	int lev = 0;
	bool okay = TRUE, good = FALSE, great = FALSE;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, "|ibbb", &lev, &okay, &good, &great))
		return NULL;

	/* Copy the Obj class to the object */
	Obj_to_object(self, &forge);
	
	/* Apply the magic */
	apply_magic(&forge, lev, okay, good, great);

	/* Copy the object back to the class */
	object_to_Obj(self, &forge);

	/* Return nothing */
	Py_INCREF(Py_None);
	return Py_None;
}

/*
 * Return a string representation of the object
 */
static PyObject *Obj_desc(ObjObject *self, PyObject *args)
{
	object_type forge;
	char buf[1024];
	int pref = 1, mode = 3;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, "|ii", &pref, &mode))
		return NULL;

	/* Copy the Obj class to the object */
	Obj_to_object(self, &forge);

	/* Obtain the description */
	object_desc(buf, &forge, pref, mode);

	/* Return the description */
	return PyString_FromString(buf);
}

/*
 * Table of class methods
 */
static PyMethodDef Obj_methods[] =
{
	{ "drop_near", (PyCFunction)Obj_drop_near, 1 },
	{ "carry", (PyCFunction)Obj_carry, 1 },
	{ "apply_magic", (PyCFunction)Obj_apply_magic, 1 },
	{ "desc", (PyCFunction)Obj_desc, 1 },
	{ NULL, NULL }
};

/*
 * Return a object attribute
 */
static PyObject *Obj_getattr(ObjObject *self, char *name)
{
	/* Try to find in dictionary */
	PyObject *v = PyDict_GetItemString(self->obj_attr, name);

	/* Check for success */
	if (v)
	{
		/* Return result */
		Py_INCREF(v);
		return v;
	}

	/* Try to find a method */
	return Py_FindMethod(Obj_methods, (PyObject *)self, name);
}

/*
 * Set an object attribute
 */
static int Obj_setattr(ObjObject *self, char *name, PyObject *v)
{
	/* Should we delete the item? */
	if (!v)
	{
		/* Attempt to delete */
		int retval = PyDict_DelItemString(self->obj_attr, name);

		/* Check for failure */
		if (retval < 0)
		{
			/* Set error string */
			PyErr_SetString(PyExc_AttributeError,
				"delete non-existing Obj attribute");
		}

		return retval;
	}

	/* Set attribute */
	return PyDict_SetItemString(self->obj_attr, name, v);
}

/*
 * Here is the complete definition of that bizarre "Obj_Type" that was
 * "prototyped" above.
 */
staticforward PyTypeObject Obj_Type =
{
        PyObject_HEAD_INIT(NULL)                /* Standard stuff */
	0,					/* Size */
	"Obj",					/* Name */
	sizeof(ObjObject),			/* Basic Size */
	0,					/* Item Size */
	(destructor)Obj_dealloc,		/* Destructor */
	0,					/* Print */
	(getattrfunc)Obj_getattr,		/* Attribute getter */
	(setattrfunc)Obj_setattr,		/* Attribute setter */
	0,
	0,
	0,
	0,
	0,
	0
};

/*
 * Module function to return a new Obj object.
 */
static PyObject *object_new(PyObject *self, PyObject *args)
{
	int type = 0;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, "|i", &type))
		return NULL;

	/* Create an Obj and return it */
	return (PyObject *)newObjObject(type);
}

/*
 * Returns a copy of a player inventory slot
 */
static PyObject *object_inventory(PyObject *self, PyObject *args)
{
	ObjObject *obj;
	int slot;

	/* Create a new object */
	obj = newObjObject(0);

	/* Find slot */
	if (!PyArg_ParseTuple(args, "i", &slot))
		return NULL;

	/* Copy object from inventory */
	object_to_Obj(obj, &inventory[slot]);

	/* Return the object */
	return (PyObject *)obj;
}

/*
 * Remove curses from items in the equipped inventory
 */
static PyObject *object_remove_curse(PyObject *self, PyObject *args)
{
	int retval;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, ""))
		return NULL;

	/* Remove curse */
	retval = remove_curse();

	/* Return */
	return Py_BuildValue("i", retval);
}

/*
 * Remove heavy curses from items in the equipped inventory
 */
static PyObject *object_remove_all_curse(PyObject *self, PyObject *args)
{
	int retval;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, ""))
		return NULL;

	/* Remove all curses */
	retval = remove_all_curse();

	/* Return */
	return Py_BuildValue("i", retval);
}

/*
 * Enchant an item.  Note that the user gets to choose the item to enchant.
 */
static PyObject *object_enchant_spell(PyObject *self, PyObject *args)
{
        int num_hit, num_dam, num_ac, num_pval, retval;

	/* Parse arguments */
        if (!PyArg_ParseTuple(args, "iiii", &num_hit, &num_dam, &num_ac, &num_pval))
		return NULL;

	/* Enchant */
        retval = enchant_spell(num_hit, num_dam, num_ac, num_pval);

	/* Return */
	return Py_BuildValue("i", retval);
}

/*
 * Identify an item.  Note that the user gets to choose the item.
 */
static PyObject *object_identify(PyObject *self, PyObject *args)
{
	int retval;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, ""))
		return NULL;

	/* Identify */
	retval = ident_spell();

	/* Return */
	return Py_BuildValue("i", retval);
}

/*
 * Fully identify an item.
 */
static PyObject *object_identify_fully(PyObject *self, PyObject *args)
{
	int retval;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, ""))
		return NULL;

	/* Identify fully */
	retval = identify_fully();

	/* Return */
	return Py_BuildValue("i", retval);
}

/*
 * Recharge an item.  Note that the user gets to choose the item.
 */
static PyObject *object_recharge(PyObject *self, PyObject *args)
{
	int num, retval;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, "i", &num))
		return NULL;

	/* Recharge */
	retval = recharge(num);

	/* Return */
	return Py_BuildValue("i", retval);
}


/*
 * Return number of object entries in use
 */
static PyObject *object_o_max(PyObject *self, PyObject *args)
{
	/* Parse arguments */
	if (!PyArg_ParseTuple(args, ""))
		return NULL;

	/* Return */
	return Py_BuildValue("i", o_max);
}

/*
 * Inventory Optimize
 */
static PyObject *object_inven_optimize(PyObject *self, PyObject *args)
{
        int i;

	/* Parse arguments */
        if (!PyArg_ParseTuple(args, "i", &i))
		return NULL;

        inven_item_optimize(i);

	/* Return nothing */
	Py_INCREF(Py_None);
	return Py_None;
}

/*
 * Inventory Increase
 */
static PyObject *object_inven_increase(PyObject *self, PyObject *args)
{
        int i, amt;

	/* Parse arguments */
        if (!PyArg_ParseTuple(args, "ii", &i, &amt))
		return NULL;

        inven_item_increase(i, amt);

	/* Return nothing */
	Py_INCREF(Py_None);
	return Py_None;
}

/*
 * Create the artifact of the specified number -- DAN
 */
static PyObject *create_named_art(PyObject *self, PyObject *args)
{
	object_type forge;
	object_type *q_ptr;
        int i;
        artifact_type *a_ptr;
        int a_idx;
	ObjObject *obj;

	/* Create a new object */
	obj = newObjObject(0);

	/* Parse arguments */
        if (!PyArg_ParseTuple(args, "i", &a_idx))
		return NULL;

        a_ptr = &a_info[a_idx];

	/* Get local object */
	q_ptr = &forge;

	/* Wipe the object */
	object_wipe(q_ptr);

	/* Ignore "empty" artifacts */
        if (!a_ptr->name)
        {
                /* Return nothing */
                Py_INCREF(Py_None);
                return Py_None;
        }

	/* Acquire the "kind" index */
	i = lookup_kind(a_ptr->tval, a_ptr->sval);

	/* Oops */
        if (!i)
        {
                /* Return nothing */
                Py_INCREF(Py_None);
                return Py_None;
        }

        if (a_ptr->cur_num == 1)
        {
                /* Return nothing */
                Py_INCREF(Py_None);
                return Py_None;
        }

	/* Create the artifact */
	object_prep(q_ptr, i);

	/* Save the name */
	q_ptr->name1 = a_idx;

	/* Extract the fields */
	q_ptr->pval = a_ptr->pval;
	q_ptr->ac = a_ptr->ac;
	q_ptr->dd = a_ptr->dd;
	q_ptr->ds = a_ptr->ds;
	q_ptr->to_a = a_ptr->to_a;
	q_ptr->to_h = a_ptr->to_h;
	q_ptr->to_d = a_ptr->to_d;
	q_ptr->weight = a_ptr->weight;

	/* Hack -- acquire "cursed" flag */
	if (a_ptr->flags3 & (TR3_CURSED)) q_ptr->ident |= (IDENT_CURSED);

	random_artifact_resistance(q_ptr);

        a_ptr->cur_num = 1;

	/* Copy object from inventory */
        object_to_Obj(obj, q_ptr);

	/* Return the object */
	return (PyObject *)obj;
}

/*
 * Function table for this module
 */
static PyMethodDef obj_methods[] =
{
	{ "new", object_new, 1 },
	{ "inventory", object_inventory, 1 },
        { "inven_increase", object_inven_increase, 1 },
        { "inven_optimize", object_inven_optimize, 1 },
	{ "remove_curse", object_remove_curse, 1 },
	{ "remove_all_curse", object_remove_all_curse, 1 },
	{ "enchant_spell", object_enchant_spell, 1 },
	{ "identify", object_identify, 1 },
	{ "identify_fully", object_identify_fully, 1 },
	{ "recharge", object_recharge, 1 },
	{ "o_max", object_o_max, 1 },
        { "create_artifact", create_named_art, 1, 1 },
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
void initobject()
{
	PyObject *m, *d;

	/* Create the module */
	m = Py_InitModule("object", obj_methods);
	
	/* Get the module's dictionary */
	d = PyModule_GetDict(m);

	/* Add some symbolic constants to the module */
	add_constant(d, "INVEN_PACK", INVEN_PACK);
	add_constant(d, "INVEN_WIELD", INVEN_WIELD);
	add_constant(d, "INVEN_BOW", INVEN_BOW);
        add_constant(d, "INVEN_RING", INVEN_RING);
	add_constant(d, "INVEN_NECK", INVEN_NECK);
	add_constant(d, "INVEN_LITE", INVEN_LITE);
	add_constant(d, "INVEN_BODY", INVEN_BODY);
	add_constant(d, "INVEN_OUTER", INVEN_OUTER);
	add_constant(d, "INVEN_ARM", INVEN_ARM);
	add_constant(d, "INVEN_HEAD", INVEN_HEAD);
	add_constant(d, "INVEN_HANDS", INVEN_HANDS);
	add_constant(d, "INVEN_FEET", INVEN_FEET);
	add_constant(d, "INVEN_TOTAL", INVEN_TOTAL);
	add_constant(d, "TV_SKELETON", TV_SKELETON);
	add_constant(d, "TV_BOTTLE", TV_BOTTLE);
	add_constant(d, "TV_SPIKE", TV_SPIKE);
	add_constant(d, "TV_CHEST", TV_CHEST);
	add_constant(d, "TV_SHOT", TV_SHOT);
	add_constant(d, "TV_ARROW", TV_ARROW);
	add_constant(d, "TV_BOLT", TV_BOLT);
	add_constant(d, "TV_BOW", TV_BOW);
	add_constant(d, "TV_DIGGING", TV_DIGGING);
	add_constant(d, "TV_HAFTED", TV_HAFTED);
	add_constant(d, "TV_POLEARM", TV_POLEARM);
	add_constant(d, "TV_SWORD", TV_SWORD);
	add_constant(d, "TV_BOOTS", TV_BOOTS);
	add_constant(d, "TV_GLOVES", TV_GLOVES);
	add_constant(d, "TV_HELM", TV_HELM);
	add_constant(d, "TV_CROWN", TV_CROWN);
	add_constant(d, "TV_SHIELD", TV_SHIELD);
	add_constant(d, "TV_CLOAK", TV_CLOAK);
	add_constant(d, "TV_SOFT_ARMOR", TV_SOFT_ARMOR);
	add_constant(d, "TV_HARD_ARMOR", TV_HARD_ARMOR);
	add_constant(d, "TV_DRAG_ARMOR", TV_DRAG_ARMOR);
	add_constant(d, "TV_LITE", TV_LITE);
	add_constant(d, "TV_AMULET", TV_AMULET);
	add_constant(d, "TV_RING", TV_RING);
	add_constant(d, "TV_STAFF", TV_STAFF);
	add_constant(d, "TV_WAND", TV_WAND);
	add_constant(d, "TV_ROD", TV_ROD);
	add_constant(d, "TV_SCROLL", TV_SCROLL);
	add_constant(d, "TV_POTION", TV_POTION);
	add_constant(d, "TV_FLASK", TV_FLASK);
	add_constant(d, "TV_FOOD", TV_FOOD);
	add_constant(d, "TV_MAGIC_BOOK", TV_MAGIC_BOOK);
	add_constant(d, "TV_PRAYER_BOOK", TV_PRAYER_BOOK);
	add_constant(d, "TV_GOLD", TV_GOLD);
	add_constant(d, "ART_GALADRIEL", ART_GALADRIEL);
	add_constant(d, "ART_ELENDIL", ART_ELENDIL);
	add_constant(d, "ART_THRAIN", ART_THRAIN);
	add_constant(d, "ART_CARLAMMAS", ART_CARLAMMAS);
	add_constant(d, "ART_INGWE", ART_INGWE);
	add_constant(d, "ART_DWARVES", ART_DWARVES);
	add_constant(d, "ART_BARAHIR", ART_BARAHIR);
	add_constant(d, "ART_TULKAS", ART_TULKAS);
	add_constant(d, "ART_NARYA", ART_NARYA);
	add_constant(d, "ART_NENYA", ART_NENYA);
	add_constant(d, "ART_VILYA", ART_VILYA);
	add_constant(d, "ART_POWER", ART_POWER);
	add_constant(d, "ART_RAZORBACK", ART_RAZORBACK);
	add_constant(d, "ART_BLADETURNER", ART_BLADETURNER);
	add_constant(d, "ART_SOULKEEPER", ART_SOULKEEPER);
	add_constant(d, "ART_ISILDUR", ART_ISILDUR);
	add_constant(d, "ART_ROHIRRIM", ART_ROHIRRIM);
	add_constant(d, "ART_BELEGENNON", ART_BELEGENNON);
	add_constant(d, "ART_CELEBORN", ART_CELEBORN);
	add_constant(d, "ART_ARVEDUI", ART_ARVEDUI);
	add_constant(d, "ART_CASPANION", ART_CASPANION);
	add_constant(d, "ART_THALKETTOTH", ART_THALKETTOTH);
	add_constant(d, "ART_THORIN", ART_THORIN);
	add_constant(d, "ART_CELEGORM", ART_CELEGORM);
	add_constant(d, "ART_ANARION", ART_ANARION);
	add_constant(d, "ART_MORGOTH", ART_MORGOTH);
	add_constant(d, "ART_BERUTHIEL", ART_BERUTHIEL);
	add_constant(d, "ART_THRANDUIL", ART_THRANDUIL);
	add_constant(d, "ART_THENGEL", ART_THENGEL);
	add_constant(d, "ART_HAMMERHAND", ART_HAMMERHAND);
	add_constant(d, "ART_DOR", ART_DOR);
	add_constant(d, "ART_HOLHENNETH", ART_HOLHENNETH);
	add_constant(d, "ART_GORLIM", ART_GORLIM);
	add_constant(d, "ART_GONDOR", ART_GONDOR);
	add_constant(d, "ART_COLLUIN", ART_COLLUIN);
	add_constant(d, "ART_HOLCOLLETH", ART_HOLCOLLETH);
	add_constant(d, "ART_THINGOL", ART_THINGOL);
	add_constant(d, "ART_THORONGIL", ART_THORONGIL);
	add_constant(d, "ART_COLANNON", ART_COLANNON);
	add_constant(d, "ART_LUTHIEN", ART_LUTHIEN);
	add_constant(d, "ART_TUOR", ART_TUOR);
	add_constant(d, "ART_CAMBELEG", ART_CAMBELEG);
	add_constant(d, "ART_CAMMITHRIM", ART_CAMMITHRIM);
	add_constant(d, "ART_PAURHACH", ART_PAURHACH);
	add_constant(d, "ART_PAURNIMMEN", ART_PAURNIMMEN);
	add_constant(d, "ART_PAURAEGEN", ART_PAURAEGEN);
	add_constant(d, "ART_PAURNEN", ART_PAURNEN);
	add_constant(d, "ART_CAMLOST", ART_CAMLOST);
	add_constant(d, "ART_FINGOLFIN", ART_FINGOLFIN);
	add_constant(d, "ART_FEANOR", ART_FEANOR);
	add_constant(d, "ART_DAL", ART_DAL);
	add_constant(d, "ART_THROR", ART_THROR);
	add_constant(d, "ART_MAEDHROS", ART_MAEDHROS);
	add_constant(d, "ART_ANGRIST", ART_ANGRIST);
	add_constant(d, "ART_NARTHANC", ART_NARTHANC);
	add_constant(d, "ART_NIMTHANC", ART_NIMTHANC);
	add_constant(d, "ART_DETHANC", ART_DETHANC);
	add_constant(d, "ART_RILIA", ART_RILIA);
	add_constant(d, "ART_BELANGIL", ART_BELANGIL);
	add_constant(d, "ART_CALRIS", ART_CALRIS);
	add_constant(d, "ART_ARUNRUTH", ART_ARUNRUTH);
	add_constant(d, "ART_GLAMDRING", ART_GLAMDRING);
	add_constant(d, "ART_AEGLIN", ART_AEGLIN);
	add_constant(d, "ART_ORCRIST", ART_ORCRIST);
	add_constant(d, "ART_GURTHANG", ART_GURTHANG);
	add_constant(d, "ART_ZARCUTHRA", ART_ZARCUTHRA);
	add_constant(d, "ART_MORMEGIL", ART_MORMEGIL);
	add_constant(d, "ART_GONDRICAM", ART_GONDRICAM);
	add_constant(d, "ART_CRISDURIAN", ART_CRISDURIAN);
	add_constant(d, "ART_AGLARANG", ART_AGLARANG);
	add_constant(d, "ART_RINGIL", ART_RINGIL);
	add_constant(d, "ART_ANDURIL", ART_ANDURIL);
	add_constant(d, "ART_ANGUIREL", ART_ANGUIREL);
	add_constant(d, "ART_ELVAGIL", ART_ELVAGIL);
	add_constant(d, "ART_FORASGIL", ART_FORASGIL);
	add_constant(d, "ART_CARETH", ART_CARETH);
	add_constant(d, "ART_STING", ART_STING);
	add_constant(d, "ART_HARADEKKET", ART_HARADEKKET);
	add_constant(d, "ART_GILETTAR", ART_GILETTAR);
	add_constant(d, "ART_DOOMCALLER", ART_DOOMCALLER);
	add_constant(d, "ART_THEODEN", ART_THEODEN);
	add_constant(d, "ART_PAIN", ART_PAIN);
	add_constant(d, "ART_OSONDIR", ART_OSONDIR);
	add_constant(d, "ART_TIL", ART_TIL);
	add_constant(d, "ART_AEGLOS", ART_AEGLOS);
	add_constant(d, "ART_OROME", ART_OROME);
	add_constant(d, "ART_NIMLOTH", ART_NIMLOTH);
	add_constant(d, "ART_EORLINGAS", ART_EORLINGAS);
	add_constant(d, "ART_DURIN", ART_DURIN);
	add_constant(d, "ART_EONWE", ART_EONWE);
	add_constant(d, "ART_BALLI", ART_BALLI);
	add_constant(d, "ART_LOTHARANG", ART_LOTHARANG);
	add_constant(d, "ART_MUNDWINE", ART_MUNDWINE);
	add_constant(d, "ART_BARUKKHELED", ART_BARUKKHELED);
	add_constant(d, "ART_WRATH", ART_WRATH);
	add_constant(d, "ART_ULMO", ART_ULMO);
	add_constant(d, "ART_AVAVIR", ART_AVAVIR);
	add_constant(d, "ART_GROND", ART_GROND);
	add_constant(d, "ART_TOTILA", ART_TOTILA);
	add_constant(d, "ART_THUNDERFIST", ART_THUNDERFIST);
	add_constant(d, "ART_BLOODSPIKE", ART_BLOODSPIKE);
	add_constant(d, "ART_FIRESTAR", ART_FIRESTAR);
	add_constant(d, "ART_TARATOL", ART_TARATOL);
	add_constant(d, "ART_AULE", ART_AULE);
	add_constant(d, "ART_NAR", ART_NAR);
	add_constant(d, "ART_ERIRIL", ART_ERIRIL);
	add_constant(d, "ART_OLORIN", ART_OLORIN);
	add_constant(d, "ART_DEATHWREAKER", ART_DEATHWREAKER);
	add_constant(d, "ART_TURMIL", ART_TURMIL);
	add_constant(d, "ART_BELTHRONDING", ART_BELTHRONDING);
	add_constant(d, "ART_BARD", ART_BARD);
	add_constant(d, "ART_CUBRAGOL", ART_CUBRAGOL);
}
#endif
