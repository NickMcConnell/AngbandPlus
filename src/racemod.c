/* File: racemod.c */

/*
 * Copyright (c) 1998 Keldon Jones
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.
 */

/*
 * This file provides a Python interface to Angband's "monster race" info.
 */

#include "angband.h"

/* XXX XXX Try to save some space */
#define LOOK_FIELD(text, field) \
	if (streq(race_field, text)) \
		return Py_BuildValue("i", r_info[r_idx].field)

/*
 * Extract the value of a field given race index and field name
 */
static PyObject *race_get_field(PyObject *self, PyObject *args)
{
	int r_idx;
	char *race_field;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, "is", &r_idx, &race_field))
		return NULL;

	/* Check for various fields */
	LOOK_FIELD("name", name);
	LOOK_FIELD("text", text);
	LOOK_FIELD("hdice", hdice);
	LOOK_FIELD("hside", hside);
	LOOK_FIELD("ac", ac);
	LOOK_FIELD("sleep", sleep);
	LOOK_FIELD("aaf", aaf);
	LOOK_FIELD("speed", speed);
	LOOK_FIELD("mexp", mexp);
	LOOK_FIELD("freq_inate", freq_inate);
	LOOK_FIELD("freq_spell", freq_spell);
	LOOK_FIELD("flags1", flags1);
	LOOK_FIELD("flags2", flags2);
	LOOK_FIELD("flags3", flags3);
	LOOK_FIELD("flags4", flags4);
	LOOK_FIELD("flags5", flags5);
	LOOK_FIELD("flags6", flags6);
        LOOK_FIELD("flags7", flags7);
        LOOK_FIELD("flags8", flags8);
        LOOK_FIELD("flags9", flags9);
	LOOK_FIELD("level", level);
	LOOK_FIELD("rarity", rarity);
	LOOK_FIELD("max_num", max_num);
	LOOK_FIELD("cur_num", cur_num);

	/* Error -- bad field */
	PyErr_SetString(PyExc_AttributeError, "bad monster race field");
	return NULL;
}

/*
 * Function table for this module
 */
static PyMethodDef race_methods[] =
{
	{ "get_field", race_get_field, 1 },
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
void initrace()
{
	PyObject *m, *d;

	/* Create the module */
	m = Py_InitModule("race", race_methods);
	
	/* Get the module's dictionary */
	d = PyModule_GetDict(m);

	/* Add some symbolic constants to the module */
	add_constant(d, "RF1_UNIQUE", RF1_UNIQUE);
	add_constant(d, "RF1_QUESTOR", RF1_QUESTOR);
	add_constant(d, "RF1_MALE", RF1_MALE);
	add_constant(d, "RF1_FEMALE", RF1_FEMALE);
	add_constant(d, "RF1_CHAR_CLEAR", RF1_CHAR_CLEAR);
	add_constant(d, "RF1_CHAR_MULTI", RF1_CHAR_MULTI);
	add_constant(d, "RF1_ATTR_CLEAR", RF1_ATTR_CLEAR);
	add_constant(d, "RF1_ATTR_MULTI", RF1_ATTR_MULTI);
	add_constant(d, "RF1_FORCE_DEPTH", RF1_FORCE_DEPTH);
	add_constant(d, "RF1_FORCE_MAXHP", RF1_FORCE_MAXHP);
	add_constant(d, "RF1_FORCE_SLEEP", RF1_FORCE_SLEEP);
	add_constant(d, "RF1_FORCE_EXTRA", RF1_FORCE_EXTRA);
	add_constant(d, "RF1_FRIEND", RF1_FRIEND);
	add_constant(d, "RF1_FRIENDS", RF1_FRIENDS);
	add_constant(d, "RF1_ESCORT", RF1_ESCORT);
	add_constant(d, "RF1_ESCORTS", RF1_ESCORTS);
	add_constant(d, "RF1_NEVER_BLOW", RF1_NEVER_BLOW);
	add_constant(d, "RF1_NEVER_MOVE", RF1_NEVER_MOVE);
	add_constant(d, "RF1_RAND_25", RF1_RAND_25);
	add_constant(d, "RF1_RAND_50", RF1_RAND_50);
	add_constant(d, "RF1_ONLY_GOLD", RF1_ONLY_GOLD);
	add_constant(d, "RF1_ONLY_ITEM", RF1_ONLY_ITEM);
	add_constant(d, "RF1_DROP_60", RF1_DROP_60);
	add_constant(d, "RF1_DROP_90", RF1_DROP_90);
	add_constant(d, "RF1_DROP_1D2", RF1_DROP_1D2);
	add_constant(d, "RF1_DROP_2D2", RF1_DROP_2D2);
	add_constant(d, "RF1_DROP_3D2", RF1_DROP_3D2);
	add_constant(d, "RF1_DROP_4D2", RF1_DROP_4D2);
	add_constant(d, "RF1_DROP_GOOD", RF1_DROP_GOOD);
	add_constant(d, "RF1_DROP_GREAT", RF1_DROP_GREAT);
	add_constant(d, "RF1_DROP_USEFUL", RF1_DROP_USEFUL);
	add_constant(d, "RF1_DROP_CHOSEN", RF1_DROP_CHOSEN);
	add_constant(d, "RF2_STUPID", RF2_STUPID);
	add_constant(d, "RF2_SMART", RF2_SMART);
	add_constant(d, "RF2_INVISIBLE", RF2_INVISIBLE);
	add_constant(d, "RF2_COLD_BLOOD", RF2_COLD_BLOOD);
	add_constant(d, "RF2_EMPTY_MIND", RF2_EMPTY_MIND);
	add_constant(d, "RF2_WEIRD_MIND", RF2_WEIRD_MIND);
	add_constant(d, "RF2_MULTIPLY", RF2_MULTIPLY);
	add_constant(d, "RF2_REGENERATE", RF2_REGENERATE);
	add_constant(d, "RF2_POWERFUL", RF2_POWERFUL);
	add_constant(d, "RF2_OPEN_DOOR", RF2_OPEN_DOOR);
	add_constant(d, "RF2_BASH_DOOR", RF2_BASH_DOOR);
	add_constant(d, "RF2_PASS_WALL", RF2_PASS_WALL);
	add_constant(d, "RF2_KILL_WALL", RF2_KILL_WALL);
	add_constant(d, "RF2_MOVE_BODY", RF2_MOVE_BODY);
	add_constant(d, "RF2_KILL_BODY", RF2_KILL_BODY);
	add_constant(d, "RF2_TAKE_ITEM", RF2_TAKE_ITEM);
	add_constant(d, "RF2_KILL_ITEM", RF2_KILL_ITEM);
	add_constant(d, "RF2_BRAIN_1", RF2_BRAIN_1);
	add_constant(d, "RF2_BRAIN_2", RF2_BRAIN_2);
	add_constant(d, "RF2_BRAIN_3", RF2_BRAIN_3);
	add_constant(d, "RF2_BRAIN_4", RF2_BRAIN_4);
	add_constant(d, "RF2_BRAIN_5", RF2_BRAIN_5);
	add_constant(d, "RF2_BRAIN_6", RF2_BRAIN_6);
	add_constant(d, "RF2_BRAIN_7", RF2_BRAIN_7);
	add_constant(d, "RF2_BRAIN_8", RF2_BRAIN_8);
	add_constant(d, "RF3_ORC", RF3_ORC);
	add_constant(d, "RF3_TROLL", RF3_TROLL);
	add_constant(d, "RF3_GIANT", RF3_GIANT);
	add_constant(d, "RF3_DRAGON", RF3_DRAGON);
	add_constant(d, "RF3_DEMON", RF3_DEMON);
	add_constant(d, "RF3_UNDEAD", RF3_UNDEAD);
	add_constant(d, "RF3_EVIL", RF3_EVIL);
	add_constant(d, "RF3_ANIMAL", RF3_ANIMAL);
	add_constant(d, "RF3_HURT_LITE", RF3_HURT_LITE);
	add_constant(d, "RF3_HURT_ROCK", RF3_HURT_ROCK);
	add_constant(d, "RF3_IM_ACID", RF3_IM_ACID);
	add_constant(d, "RF3_IM_ELEC", RF3_IM_ELEC);
	add_constant(d, "RF3_IM_FIRE", RF3_IM_FIRE);
	add_constant(d, "RF3_IM_COLD", RF3_IM_COLD);
	add_constant(d, "RF3_IM_POIS", RF3_IM_POIS);
	add_constant(d, "RF3_RES_NETH", RF3_RES_NETH);
	add_constant(d, "RF3_RES_WATE", RF3_RES_WATE);
	add_constant(d, "RF3_RES_PLAS", RF3_RES_PLAS);
	add_constant(d, "RF3_RES_NEXU", RF3_RES_NEXU);
	add_constant(d, "RF3_RES_DISE", RF3_RES_DISE);
	add_constant(d, "RF3_NO_FEAR", RF3_NO_FEAR);
	add_constant(d, "RF3_NO_STUN", RF3_NO_STUN);
	add_constant(d, "RF3_NO_CONF", RF3_NO_CONF);
	add_constant(d, "RF3_NO_SLEEP", RF3_NO_SLEEP);
	add_constant(d, "RF4_SHRIEK", RF4_SHRIEK);
	add_constant(d, "RF4_ARROW_1", RF4_ARROW_1);
	add_constant(d, "RF4_ARROW_2", RF4_ARROW_2);
	add_constant(d, "RF4_ARROW_3", RF4_ARROW_3);
	add_constant(d, "RF4_ARROW_4", RF4_ARROW_4);
	add_constant(d, "RF4_BR_ACID", RF4_BR_ACID);
	add_constant(d, "RF4_BR_ELEC", RF4_BR_ELEC);
	add_constant(d, "RF4_BR_FIRE", RF4_BR_FIRE);
	add_constant(d, "RF4_BR_COLD", RF4_BR_COLD);
	add_constant(d, "RF4_BR_POIS", RF4_BR_POIS);
	add_constant(d, "RF4_BR_NETH", RF4_BR_NETH);
	add_constant(d, "RF4_BR_LITE", RF4_BR_LITE);
	add_constant(d, "RF4_BR_DARK", RF4_BR_DARK);
	add_constant(d, "RF4_BR_CONF", RF4_BR_CONF);
	add_constant(d, "RF4_BR_SOUN", RF4_BR_SOUN);
	add_constant(d, "RF4_BR_CHAO", RF4_BR_CHAO);
	add_constant(d, "RF4_BR_DISE", RF4_BR_DISE);
	add_constant(d, "RF4_BR_NEXU", RF4_BR_NEXU);
	add_constant(d, "RF4_BR_TIME", RF4_BR_TIME);
	add_constant(d, "RF4_BR_INER", RF4_BR_INER);
	add_constant(d, "RF4_BR_GRAV", RF4_BR_GRAV);
	add_constant(d, "RF4_BR_SHAR", RF4_BR_SHAR);
	add_constant(d, "RF4_BR_PLAS", RF4_BR_PLAS);
	add_constant(d, "RF4_BR_WALL", RF4_BR_WALL);
	add_constant(d, "RF4_BR_MANA", RF4_BR_MANA);
	add_constant(d, "RF5_BA_ACID", RF5_BA_ACID);
	add_constant(d, "RF5_BA_ELEC", RF5_BA_ELEC);
	add_constant(d, "RF5_BA_FIRE", RF5_BA_FIRE);
	add_constant(d, "RF5_BA_COLD", RF5_BA_COLD);
	add_constant(d, "RF5_BA_POIS", RF5_BA_POIS);
	add_constant(d, "RF5_BA_NETH", RF5_BA_NETH);
	add_constant(d, "RF5_BA_WATE", RF5_BA_WATE);
	add_constant(d, "RF5_BA_MANA", RF5_BA_MANA);
	add_constant(d, "RF5_BA_DARK", RF5_BA_DARK);
	add_constant(d, "RF5_DRAIN_MANA", RF5_DRAIN_MANA);
	add_constant(d, "RF5_MIND_BLAST", RF5_MIND_BLAST);
	add_constant(d, "RF5_BRAIN_SMASH", RF5_BRAIN_SMASH);
	add_constant(d, "RF5_CAUSE_1", RF5_CAUSE_1);
	add_constant(d, "RF5_CAUSE_2", RF5_CAUSE_2);
	add_constant(d, "RF5_CAUSE_3", RF5_CAUSE_3);
	add_constant(d, "RF5_CAUSE_4", RF5_CAUSE_4);
	add_constant(d, "RF5_BO_ACID", RF5_BO_ACID);
	add_constant(d, "RF5_BO_ELEC", RF5_BO_ELEC);
	add_constant(d, "RF5_BO_FIRE", RF5_BO_FIRE);
	add_constant(d, "RF5_BO_COLD", RF5_BO_COLD);
	add_constant(d, "RF5_BO_POIS", RF5_BO_POIS);
	add_constant(d, "RF5_BO_NETH", RF5_BO_NETH);
	add_constant(d, "RF5_BO_WATE", RF5_BO_WATE);
	add_constant(d, "RF5_BO_MANA", RF5_BO_MANA);
	add_constant(d, "RF5_BO_PLAS", RF5_BO_PLAS);
	add_constant(d, "RF5_BO_ICEE", RF5_BO_ICEE);
	add_constant(d, "RF5_MISSILE", RF5_MISSILE);
	add_constant(d, "RF5_SCARE", RF5_SCARE);
	add_constant(d, "RF5_BLIND", RF5_BLIND);
	add_constant(d, "RF5_CONF", RF5_CONF);
	add_constant(d, "RF5_SLOW", RF5_SLOW);
	add_constant(d, "RF5_HOLD", RF5_HOLD);
	add_constant(d, "RF6_HASTE", RF6_HASTE);
	add_constant(d, "RF6_HEAL", RF6_HEAL);
	add_constant(d, "RF6_BLINK", RF6_BLINK);
	add_constant(d, "RF6_TPORT", RF6_TPORT);
	add_constant(d, "RF6_TELE_TO", RF6_TELE_TO);
	add_constant(d, "RF6_TELE_AWAY", RF6_TELE_AWAY);
	add_constant(d, "RF6_TELE_LEVEL", RF6_TELE_LEVEL);
	add_constant(d, "RF6_DARKNESS", RF6_DARKNESS);
	add_constant(d, "RF6_TRAPS", RF6_TRAPS);
	add_constant(d, "RF6_FORGET", RF6_FORGET);
	add_constant(d, "RF6_S_MONSTER", RF6_S_MONSTER);
	add_constant(d, "RF6_S_MONSTERS", RF6_S_MONSTERS);
	add_constant(d, "RF6_S_ANT", RF6_S_ANT);
	add_constant(d, "RF6_S_SPIDER", RF6_S_SPIDER);
	add_constant(d, "RF6_S_HOUND", RF6_S_HOUND);
	add_constant(d, "RF6_S_HYDRA", RF6_S_HYDRA);
	add_constant(d, "RF6_S_ANGEL", RF6_S_ANGEL);
	add_constant(d, "RF6_S_DEMON", RF6_S_DEMON);
	add_constant(d, "RF6_S_UNDEAD", RF6_S_UNDEAD);
	add_constant(d, "RF6_S_DRAGON", RF6_S_DRAGON);
	add_constant(d, "RF6_S_HI_UNDEAD", RF6_S_HI_UNDEAD);
	add_constant(d, "RF6_S_HI_DRAGON", RF6_S_HI_DRAGON);
	add_constant(d, "RF6_S_WRAITH", RF6_S_WRAITH);
	add_constant(d, "RF6_S_UNIQUE", RF6_S_UNIQUE);
}
