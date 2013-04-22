/* File: kindmod.c */

/*
 * Copyright (c) 1998 Keldon Jones
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.
 */

/*
 * This file provides a Python interface to Angband's "object kind" info.
 */

#include "angband.h"

/* XXX XXX Try to save some space */
#define LOOK_FIELD(text, field) \
	if (streq(kind_field, text)) \
		return Py_BuildValue("i", k_info[k_idx].field)

/*
 * Extract the value of a field given kind index and field name
 */
static PyObject *kind_get_field(PyObject *self, PyObject *args)
{
	int k_idx;
	char *kind_field;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, "is", &k_idx, &kind_field))
		return NULL;

	/* Check for various fields */
	LOOK_FIELD("name", name);
	LOOK_FIELD("text", text);
	LOOK_FIELD("tval", tval);
	LOOK_FIELD("sval", sval);
	LOOK_FIELD("pval", pval);
	LOOK_FIELD("to_h", to_h);
	LOOK_FIELD("to_d", to_d);
	LOOK_FIELD("to_a", to_a);
	LOOK_FIELD("dd", dd);
	LOOK_FIELD("ds", ds);
	LOOK_FIELD("weight", weight);
	LOOK_FIELD("flags1", flags1);
	LOOK_FIELD("flags2", flags2);
	LOOK_FIELD("flags3", flags3);
	LOOK_FIELD("level", level);
	LOOK_FIELD("extra", extra);
	LOOK_FIELD("flavor", flavor);
	LOOK_FIELD("easy_know", easy_know);
	LOOK_FIELD("aware", aware);
	LOOK_FIELD("tried", tried);

	/* Error -- bad field */
	PyErr_SetString(PyExc_AttributeError, "bad object kind field");
	return NULL;
}

/*
 * Function table for this module
 */
static PyMethodDef kind_methods[] =
{
	{ "get_field", kind_get_field, 1 },
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
void initkind()
{
	PyObject *m, *d;

	/* Create the module */
	m = Py_InitModule("kind", kind_methods);
	
	/* Get the module's dictionary */
	d = PyModule_GetDict(m);

	/* Add some symbolic constants to the module */
	add_constant(d, "TR1_STR", TR1_STR);
	add_constant(d, "TR1_INT", TR1_INT);
	add_constant(d, "TR1_WIS", TR1_WIS);
	add_constant(d, "TR1_DEX", TR1_DEX);
	add_constant(d, "TR1_CON", TR1_CON);
	add_constant(d, "TR1_CHR", TR1_CHR);
	add_constant(d, "TR1_XXX1", TR1_XXX1);
	add_constant(d, "TR1_XXX2", TR1_XXX2);
	add_constant(d, "TR1_STEALTH", TR1_STEALTH);
	add_constant(d, "TR1_SEARCH", TR1_SEARCH);
	add_constant(d, "TR1_INFRA", TR1_INFRA);
	add_constant(d, "TR1_TUNNEL", TR1_TUNNEL);
	add_constant(d, "TR1_SPEED", TR1_SPEED);
	add_constant(d, "TR1_BLOWS", TR1_BLOWS);
	add_constant(d, "TR1_SHOTS", TR1_SHOTS);
	add_constant(d, "TR1_MIGHT", TR1_MIGHT);
	add_constant(d, "TR1_SLAY_ANIMAL", TR1_SLAY_ANIMAL);
	add_constant(d, "TR1_SLAY_EVIL", TR1_SLAY_EVIL);
	add_constant(d, "TR1_SLAY_UNDEAD", TR1_SLAY_UNDEAD);
	add_constant(d, "TR1_SLAY_DEMON", TR1_SLAY_DEMON);
	add_constant(d, "TR1_SLAY_ORC", TR1_SLAY_ORC);
	add_constant(d, "TR1_SLAY_TROLL", TR1_SLAY_TROLL);
	add_constant(d, "TR1_SLAY_GIANT", TR1_SLAY_GIANT);
	add_constant(d, "TR1_SLAY_DRAGON", TR1_SLAY_DRAGON);
	add_constant(d, "TR1_KILL_DRAGON", TR1_KILL_DRAGON);
	add_constant(d, "TR1_XXX5", TR1_XXX5);
	add_constant(d, "TR1_XXX6", TR1_XXX6);
	add_constant(d, "TR1_XXX7", TR1_XXX7);
	add_constant(d, "TR1_BRAND_ACID", TR1_BRAND_ACID);
	add_constant(d, "TR1_BRAND_ELEC", TR1_BRAND_ELEC);
	add_constant(d, "TR1_BRAND_FIRE", TR1_BRAND_FIRE);
	add_constant(d, "TR1_BRAND_COLD", TR1_BRAND_COLD);
	add_constant(d, "TR2_SUST_STR", TR2_SUST_STR);
	add_constant(d, "TR2_SUST_INT", TR2_SUST_INT);
	add_constant(d, "TR2_SUST_WIS", TR2_SUST_WIS);
	add_constant(d, "TR2_SUST_DEX", TR2_SUST_DEX);
	add_constant(d, "TR2_SUST_CON", TR2_SUST_CON);
	add_constant(d, "TR2_SUST_CHR", TR2_SUST_CHR);
	add_constant(d, "TR2_XXX1", TR2_XXX1);
	add_constant(d, "TR2_XXX2", TR2_XXX2);
	add_constant(d, "TR2_XXX3", TR2_XXX3);
	add_constant(d, "TR2_XXX4", TR2_XXX4);
	add_constant(d, "TR2_XXX5", TR2_XXX5);
	add_constant(d, "TR2_XXX6", TR2_XXX6);
	add_constant(d, "TR2_IM_ACID", TR2_IM_ACID);
	add_constant(d, "TR2_IM_ELEC", TR2_IM_ELEC);
	add_constant(d, "TR2_IM_FIRE", TR2_IM_FIRE);
	add_constant(d, "TR2_IM_COLD", TR2_IM_COLD);
	add_constant(d, "TR2_RES_ACID", TR2_RES_ACID);
	add_constant(d, "TR2_RES_ELEC", TR2_RES_ELEC);
	add_constant(d, "TR2_RES_FIRE", TR2_RES_FIRE);
	add_constant(d, "TR2_RES_COLD", TR2_RES_COLD);
	add_constant(d, "TR2_RES_POIS", TR2_RES_POIS);
	add_constant(d, "TR2_RES_FEAR", TR2_RES_FEAR);
	add_constant(d, "TR2_RES_LITE", TR2_RES_LITE);
	add_constant(d, "TR2_RES_DARK", TR2_RES_DARK);
	add_constant(d, "TR2_RES_BLIND", TR2_RES_BLIND);
	add_constant(d, "TR2_RES_CONFU", TR2_RES_CONFU);
	add_constant(d, "TR2_RES_SOUND", TR2_RES_SOUND);
	add_constant(d, "TR2_RES_SHARD", TR2_RES_SHARD);
	add_constant(d, "TR2_RES_NEXUS", TR2_RES_NEXUS);
	add_constant(d, "TR2_RES_NETHR", TR2_RES_NETHR);
	add_constant(d, "TR2_RES_CHAOS", TR2_RES_CHAOS);
	add_constant(d, "TR2_RES_DISEN", TR2_RES_DISEN);
	add_constant(d, "TR3_SLOW_DIGEST", TR3_SLOW_DIGEST);
	add_constant(d, "TR3_FEATHER", TR3_FEATHER);
	add_constant(d, "TR3_LITE", TR3_LITE);
	add_constant(d, "TR3_REGEN", TR3_REGEN);
	add_constant(d, "TR3_TELEPATHY", TR3_TELEPATHY);
	add_constant(d, "TR3_SEE_INVIS", TR3_SEE_INVIS);
	add_constant(d, "TR3_FREE_ACT", TR3_FREE_ACT);
	add_constant(d, "TR3_HOLD_LIFE", TR3_HOLD_LIFE);
	add_constant(d, "TR3_XXX1", TR3_XXX1);
	add_constant(d, "TR3_XXX2", TR3_XXX2);
	add_constant(d, "TR3_XXX3", TR3_XXX3);
	add_constant(d, "TR3_XXX4", TR3_XXX4);
	add_constant(d, "TR3_IMPACT", TR3_IMPACT);
	add_constant(d, "TR3_TELEPORT", TR3_TELEPORT);
	add_constant(d, "TR3_AGGRAVATE", TR3_AGGRAVATE);
	add_constant(d, "TR3_DRAIN_EXP", TR3_DRAIN_EXP);
	add_constant(d, "TR3_IGNORE_ACID", TR3_IGNORE_ACID);
	add_constant(d, "TR3_IGNORE_ELEC", TR3_IGNORE_ELEC);
	add_constant(d, "TR3_IGNORE_FIRE", TR3_IGNORE_FIRE);
	add_constant(d, "TR3_IGNORE_COLD", TR3_IGNORE_COLD);
	add_constant(d, "TR3_XXX5", TR3_XXX5);
	add_constant(d, "TR3_XXX6", TR3_XXX6);
	add_constant(d, "TR3_BLESSED", TR3_BLESSED);
	add_constant(d, "TR3_ACTIVATE", TR3_ACTIVATE);
	add_constant(d, "TR3_INSTA_ART", TR3_INSTA_ART);
	add_constant(d, "TR3_EASY_KNOW", TR3_EASY_KNOW);
	add_constant(d, "TR3_HIDE_TYPE", TR3_HIDE_TYPE);
	add_constant(d, "TR3_SHOW_MODS", TR3_SHOW_MODS);
	add_constant(d, "TR3_XXX7", TR3_XXX7);
	add_constant(d, "TR3_LIGHT_CURSE", TR3_LIGHT_CURSE);
	add_constant(d, "TR3_HEAVY_CURSE", TR3_HEAVY_CURSE);
	add_constant(d, "TR3_PERMA_CURSE", TR3_PERMA_CURSE);
}
