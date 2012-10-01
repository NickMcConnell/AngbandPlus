/* File: spellmod.c */

#ifdef USE_PYTHON

/*
 * Copyright (c) 1998 Keldon Jones
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.
 */

/*
 * This file provides a Python interface to Angband's functions to cast
 * various spells and perform other magic-like effects.
 */

#include "angband.h"

/*
 * Teleport a monster away
 */
static PyObject *spell_teleport_away(PyObject *self, PyObject *args)
{
	int m_idx, dis = 100;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, "i|i", &m_idx, &dis))
		return NULL;

	/* Teleport */
	teleport_away(m_idx, dis);

	/* Return nothing */
	Py_INCREF(Py_None);
	return Py_None;
}

/*
 * Teleport the player
 */
static PyObject *spell_teleport_player(PyObject *self, PyObject *args)
{
	int dis;

	/* Parse argument */
	if (!PyArg_ParseTuple(args, "i", &dis))
		return NULL;

	/* Teleport */
	teleport_player(dis);

	/* Return nothing */
	Py_INCREF(Py_None);
	return Py_None;
}

/*
 * Teleport the player to a specific location
 */
static PyObject *spell_teleport_to(PyObject *self, PyObject *args)
{
	int y, x;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, "ii", &y, &x))
		return NULL;

	/* Teleport */
	teleport_player_to(y, x);

	/* Return nothing */
	Py_INCREF(Py_None);
	return Py_None;
}

/*
 * Teleport level
 */
static PyObject *spell_teleport_level(PyObject *self, PyObject *args)
{
	/* Parse arguments */
	if (!PyArg_ParseTuple(args, ""))
		return NULL;

	/* Teleport level */
	teleport_player_level();

	/* Return nothing */
	Py_INCREF(Py_None);
	return Py_None;
}

/*
 * Increase a player stat
 */
static PyObject *spell_inc_stat(PyObject *self, PyObject *args)
{
	int stat, retval;

	/* Parse argument */
	if (!PyArg_ParseTuple(args, "i", &stat))
		return NULL;

	/* Increment stat */
	retval = do_inc_stat(stat);

	/* Return */
	return Py_BuildValue("i", retval);
}

/*
 * Decrease a player stat
 */
static PyObject *spell_dec_stat(PyObject *self, PyObject *args)
{
	int stat, retval;

	/* Parse argument */
	if (!PyArg_ParseTuple(args, "i", &stat))
		return NULL;

	/* Decrement stat */
        retval = do_dec_stat(stat, STAT_DEC_NORMAL);

	/* Return */
	return Py_BuildValue("i", retval);
}

/*
 * Restore a player stat
 */
static PyObject *spell_res_stat(PyObject *self, PyObject *args)
{
	int stat, retval;

	/* Parse argument */
	if (!PyArg_ParseTuple(args, "i", &stat))
		return NULL;

	/* Restore stat */
	retval = do_res_stat(stat);

	/* Return */
	return Py_BuildValue("i", retval);
}

/*
 * Project a spell effect (see spells1.c)
 *
 * A Python "standard library" should have functions to fire bolts, beams,
 * and balls, rather than adding extra functions to the API which can be
 * emulated externally.
 */
static PyObject *spell_project(PyObject *self, PyObject *args)
{
	int who, rad, y, x, dam, typ, flg, retval;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, "iiiiiii", &who, &rad, &y, &x, &dam, &typ, &flg))
		return NULL;

	/* Project */
	retval = project(who, rad, y, x, dam, typ, flg);

	/* Return */
	return Py_BuildValue("i", retval);
}

/*
 * Heal the player
 */
static PyObject *spell_hp_player(PyObject *self, PyObject *args)
{
	int num, retval;

	/* Parse argument */
	if (!PyArg_ParseTuple(args, "i", &num))
		return NULL;

	/* Heal */
	retval = hp_player(num);

	/* Return */
	return Py_BuildValue("i", retval);
}

/*
 * Drop a glyph of warding
 */
static PyObject *spell_warding_glyph(PyObject *self, PyObject *args)
{
	/* Parse arguments */
	if (!PyArg_ParseTuple(args, ""))
		return NULL;

	/* Lay glyph */
	warding_glyph();

	/* Return nothing */
	Py_INCREF(Py_None);
	return Py_None;
}

/*
 * Acquire self knowledge
 */
static PyObject *spell_self_knowledge(PyObject *self, PyObject *args)
{
	/* Parse arguments */
	if (!PyArg_ParseTuple(args, ""))
		return NULL;

	/* Acquire knowledge */
        self_knowledge(NULL);

	/* Return nothing */
	Py_INCREF(Py_None);
	return Py_None;
}

/*
 * Detect traps
 */
static PyObject *spell_detect_traps(PyObject *self, PyObject *args)
{
	int retval;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, ""))
		return NULL;

	/* Detect */
	retval = detect_traps();

	/* Return */
	return Py_BuildValue("i", retval);
}

/*
 * Detect doors
 */
static PyObject *spell_detect_doors(PyObject *self, PyObject *args)
{
	int retval;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, ""))
		return NULL;

	/* Detect */
	retval = detect_doors();

	/* Return */
	return Py_BuildValue("i", retval);
}

/*
 * Detect stairs
 */
static PyObject *spell_detect_stairs(PyObject *self, PyObject *args)
{
	int retval;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, ""))
		return NULL;

	/* Detect */
	retval = detect_stairs();

	/* Return */
	return Py_BuildValue("i", retval);
}

/*
 * Detect treasure
 */
static PyObject *spell_detect_treasure(PyObject *self, PyObject *args)
{
	int retval;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, ""))
		return NULL;

	/* Detect */
	retval = detect_treasure();

	/* Return */
	return Py_BuildValue("i", retval);
}

/*
 * Detect gold objects
 */
static PyObject *spell_detect_objects_gold(PyObject *self, PyObject *args)
{
	int retval;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, ""))
		return NULL;

	/* Detect */
	retval = detect_objects_gold();

	/* Return */
	return Py_BuildValue("i", retval);
}

/*
 * Detect normal objects
 */
static PyObject *spell_detect_objects_normal(PyObject *self, PyObject *args)
{
	int retval;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, ""))
		return NULL;

	/* Detect */
	retval = detect_objects_normal();

	/* Return */
	return Py_BuildValue("i", retval);
}

/*
 * Detect magical objects
 */
static PyObject *spell_detect_objects_magic(PyObject *self, PyObject *args)
{
	int retval;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, ""))
		return NULL;

	/* Detect */
	retval = detect_objects_magic();

	/* Return */
	return Py_BuildValue("i", retval);
}

/*
 * Detect normal monsters
 */
static PyObject *spell_detect_monsters_normal(PyObject *self, PyObject *args)
{
	int retval;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, ""))
		return NULL;

	/* Detect */
	retval = detect_monsters_normal();

	/* Return */
	return Py_BuildValue("i", retval);
}

/*
 * Detect invisible monsters
 */
static PyObject *spell_detect_monsters_invis(PyObject *self, PyObject *args)
{
	int retval;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, ""))
		return NULL;

	/* Detect */
	retval = detect_monsters_invis();

	/* Return */
	return Py_BuildValue("i", retval);
}

/*
 * Detect evil monsters
 */
static PyObject *spell_detect_monsters_evil(PyObject *self, PyObject *args)
{
	int retval;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, ""))
		return NULL;

	/* Detect */
	retval = detect_monsters_evil();

	/* Return */
	return Py_BuildValue("i", retval);
}

/*
 * Detect everything
 */
static PyObject *spell_detect_all(PyObject *self, PyObject *args)
{
	int retval;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, ""))
		return NULL;

	/* Detect */
	retval = detect_all();

	/* Return */
	return Py_BuildValue("i", retval);
}

/*
 * Create a legal staircase
 */
static PyObject *spell_stair_creation(PyObject *self, PyObject *args)
{
	/* Parse arguments */
	if (!PyArg_ParseTuple(args, ""))
		return NULL;

	/* Create staircase */
	stair_creation();

	/* Return nothing */
	Py_INCREF(Py_None);
	return Py_None;
}

/*
 * Speed monsters in LOS
 */
static PyObject *spell_speed_monsters(PyObject *self, PyObject *args)
{
	int retval;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, ""))
		return NULL;

	/* Speed monsters */
	retval = speed_monsters();

	/* Return */
	return Py_BuildValue("i", retval);
}

/*
 * Slow monsters in LOS
 */
static PyObject *spell_slow_monsters(PyObject *self, PyObject *args)
{
	int retval;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, ""))
		return NULL;

	/* Slow monsters */
	retval = slow_monsters();

	/* Return */
	return Py_BuildValue("i", retval);
}

/*
 * Sleep monsters in LOS
 */
static PyObject *spell_sleep_monsters(PyObject *self, PyObject *args)
{
	int retval;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, ""))
		return NULL;

	/* Sleep monsters */
	retval = sleep_monsters();

	/* Return */
	return Py_BuildValue("i", retval);
}

/*
 * Banish evil monsters in LOS
 */
static PyObject *spell_banish_evil(PyObject *self, PyObject *args)
{
	int dist, retval;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, "i", &dist))
		return NULL;

	/* Banish monsters */
	retval = banish_evil(dist);

	/* Return */
	return Py_BuildValue("i", retval);
}

/*
 * Turn undead monsters in LOS
 */
static PyObject *spell_turn_undead(PyObject *self, PyObject *args)
{
	int retval;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, ""))
		return NULL;

	/* Turn undead */
	retval = turn_undead();

	/* Return */
	return Py_BuildValue("i", retval);
}

/*
 * Dispel undead monsters in LOS
 */
static PyObject *spell_dispel_undead(PyObject *self, PyObject *args)
{
	int dam, retval;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, "i", &dam))
		return NULL;

	/* Dispel undead */
	retval = dispel_undead(dam);

	/* Return */
	return Py_BuildValue("i", retval);
}

/*
 * Dispel evil monsters in LOS
 */
static PyObject *spell_dispel_evil(PyObject *self, PyObject *args)
{
	int dam, retval;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, "i", &dam))
		return NULL;

	/* Dispel evil */
	retval = dispel_evil(dam);

	/* Return */
	return Py_BuildValue("i", retval);
}

/*
 * Dispel monsters in LOS
 */
static PyObject *spell_dispel_monsters(PyObject *self, PyObject *args)
{
	int dam, retval;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, "i", &dam))
		return NULL;

	/* Dispel monsters */
	retval = dispel_monsters(dam);

	/* Return */
	return Py_BuildValue("i", retval);
}

/*
 * Aggravate monsters
 */
static PyObject *spell_aggravate_monsters(PyObject *self, PyObject *args)
{
	int who = -1;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, "|i", &who))
		return NULL;

	/* Aggravate */
	aggravate_monsters(who);

	/* Return nothing */
	Py_INCREF(Py_None);
	return Py_None;
}

/*
 * Genocide.  Note that the player gets to choose the symbol.
 */
static PyObject *spell_genocide(PyObject *self, PyObject *args)
{
	int retval;
        bool p_cast = TRUE;

	/* Parse arguments */
        if (!PyArg_ParseTuple(args, "b", &p_cast))
		return NULL;

	/* Genocide */
        retval = genocide(p_cast);

	/* Return */
	return Py_BuildValue("i", retval);
}

/*
 * Mass genocide
 */
static PyObject *spell_mass_genocide(PyObject *self, PyObject *args)
{
	int retval;
        bool p_cast = TRUE;

	/* Parse arguments */
        if (!PyArg_ParseTuple(args, "b", p_cast))
		return NULL;

	/* Genocide */
        retval = mass_genocide(p_cast);

	/* Return */
	return Py_BuildValue("i", retval);
}

/*
 * Probing
 */
static PyObject *spell_probing(PyObject *self, PyObject *args)
{
	int retval;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, ""))
		return NULL;

	/* Probe */
	retval = probing();

	/* Return */
	return Py_BuildValue("i", retval);
}

/*
 * Destroy an area
 */
static PyObject *spell_destroy_area(PyObject *self, PyObject *args)
{
	int y, x, r;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, "iii", &y, &x, &r))
		return NULL;

	/* Destroy */
	destroy_area(y, x, r, 0);

	/* Return nothing */
	Py_INCREF(Py_None);
	return Py_None;
}

/*
 * Cause an earthquake
 */
static PyObject *spell_earthquake(PyObject *self, PyObject *args)
{
	int y, x, r;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, "iii", &y, &x, &r))
		return NULL;

	/* Earthquake */
	earthquake(y, x, r);

	/* Return nothing */
	Py_INCREF(Py_None);
	return Py_None;
}

/*
 * Lite a room containing the given grid
 */
static PyObject *spell_lite_room(PyObject *self, PyObject *args)
{
	int y, x;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, "ii", &y, &x))
		return NULL;

	/* Lite room */
	lite_room(y, x);

	/* Return nothing */
	Py_INCREF(Py_None);
	return Py_None;
}

/*
 * Darken a room containing the given grid
 */
static PyObject *spell_unlite_room(PyObject *self, PyObject *args)
{
	int y, x;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, "ii", &y, &x))
		return NULL;

	/* Unlite room */
	unlite_room(y, x);

	/* Return nothing */
	Py_INCREF(Py_None);
	return Py_None;
}

/*
 * Call light around the player
 */
static PyObject *spell_lite_area(PyObject *self, PyObject *args)
{
	int dam, rad, retval;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, "ii", &dam, &rad))
		return NULL;

	/* Call light */
	retval = lite_area(dam, rad);

	/* Return */
	return Py_BuildValue("i", retval);
}

/*
 * Call darkness around the player
 */
static PyObject *spell_unlite_area(PyObject *self, PyObject *args)
{
	int dam, rad, retval;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, "ii", &dam, &rad))
		return NULL;

	/* Call light */
	retval = unlite_area(dam, rad);

	/* Return */
	return Py_BuildValue("i", retval);
}

/*
 * Fire a ball spell from the player
 */
static PyObject *spell_fire_ball(PyObject *self, PyObject *args)
{
	int typ, dir, dam, rad, retval;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, "iiii", &typ, &dir, &dam, &rad))
		return NULL;

	/* Fire */
	retval = fire_ball(typ, dir, dam, rad);

	/* Return */
	return Py_BuildValue("i", retval);
}

/*
 * Fire a bolt spell from the player
 */
static PyObject *spell_fire_bolt(PyObject *self, PyObject *args)
{
	int typ, dir, dam, retval;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, "iii", &typ, &dir, &dam))
		return NULL;

	/* Fire */
	retval = fire_bolt(typ, dir, dam);

	/* Return */
	return Py_BuildValue("i", retval);
}

/*
 * Fire a beam spell from the player
 */
static PyObject *spell_fire_beam(PyObject *self, PyObject *args)
{
	int typ, dir, dam, retval;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, "iii", &typ, &dir, &dam))
		return NULL;

	/* Fire */
	retval = fire_beam(typ, dir, dam);

	/* Return */
	return Py_BuildValue("i", retval);
}

/*
 * Fire a bolt or beam spell from the player
 */
static PyObject *spell_fire_bolt_or_beam(PyObject *self, PyObject *args)
{
	int prob, typ, dir, dam, retval;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, "iiii", &prob, &typ, &dir, &dam))
		return NULL;

	/* Fire */
	retval = fire_bolt_or_beam(prob, typ, dir, dam);

	/* Return */
	return Py_BuildValue("i", retval);
}

/*
 * Method table for all functions provided by the module
 */
static PyMethodDef spellMethods[] =
{
	{ "teleport_away", spell_teleport_away, 1 },
	{ "teleport_player", spell_teleport_player, 1 },
	{ "teleport_to", spell_teleport_to, 1 },
	{ "teleport_level", spell_teleport_level, 1 },
	{ "inc_stat", spell_inc_stat, 1 },
	{ "dec_stat", spell_dec_stat, 1 },
	{ "res_stat", spell_res_stat, 1 },
	{ "project", spell_project, 1 },
	{ "hp_player", spell_hp_player, 1 },
	{ "warding_glyph", spell_warding_glyph, 1 },
	{ "self_knowledge", spell_self_knowledge, 1 },
	{ "detect_traps", spell_detect_traps, 1 },
	{ "detect_doors", spell_detect_doors, 1 },
	{ "detect_stairs", spell_detect_stairs, 1 },
	{ "detect_treasure", spell_detect_treasure, 1 },
	{ "detect_objects_gold", spell_detect_objects_gold, 1 },
	{ "detect_objects_normal", spell_detect_objects_normal, 1 },
	{ "detect_objects_magic", spell_detect_objects_magic, 1 },
	{ "detect_monsters_normal", spell_detect_monsters_normal, 1 },
	{ "detect_monsters_invis", spell_detect_monsters_invis, 1 },
	{ "detect_monsters_evil", spell_detect_monsters_evil, 1 },
	{ "detect_all", spell_detect_all, 1 },
	{ "stair_creation", spell_stair_creation, 1 },
	{ "speed_monsters", spell_speed_monsters, 1 },
	{ "slow_monsters", spell_slow_monsters, 1 },
	{ "sleep_monsters", spell_sleep_monsters, 1 },
	{ "banish_evil", spell_banish_evil, 1 },
	{ "turn_undead", spell_turn_undead, 1 },
	{ "dispel_undead", spell_dispel_undead, 1 },
	{ "dispel_evil", spell_dispel_evil, 1 },
	{ "dispel_monsters", spell_dispel_monsters, 1 },
	{ "aggravate_monsters", spell_aggravate_monsters, 1 },
	{ "genocide", spell_genocide, 1 },
	{ "mass_genocide", spell_mass_genocide, 1 },
	{ "probing", spell_probing, 1 },
	{ "destroy_area", spell_destroy_area, 1 },
	{ "earthquake", spell_earthquake, 1 },
	{ "lite_room", spell_lite_room, 1 },
	{ "unlite_room", spell_unlite_room, 1 },
	{ "lite_area", spell_lite_area, 1 },
	{ "unlite_area", spell_unlite_area, 1 },
	{ "fire_ball", spell_fire_ball, 1 },
	{ "fire_bolt", spell_fire_bolt, 1 },
	{ "fire_beam", spell_fire_beam, 1 },
	{ "fire_bolt_or_beam", spell_fire_bolt_or_beam, 1 },
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
 * Initialize the spells module
 */
void initspell(void)
{
	PyObject *m, *d;

	/* Tell the interpreter to insert the module */
	m = Py_InitModule("spell", spellMethods);

	/* Add some symbolic constants to the module */
	d = PyModule_GetDict(m);

	/* Add each constant */
	add_constant(d, "A_STR", A_STR);
	add_constant(d, "A_INT", A_INT);
	add_constant(d, "A_WIS", A_WIS);
	add_constant(d, "A_DEX", A_DEX);
	add_constant(d, "A_CON", A_CON);
	add_constant(d, "A_CHR", A_CHR);
	add_constant(d, "GF_ARROW", GF_ARROW);
	add_constant(d, "GF_MISSILE", GF_MISSILE);
	add_constant(d, "GF_MANA", GF_MANA);
        add_constant(d, "GF_HOLY_FIRE", GF_HOLY_FIRE);
	add_constant(d, "GF_LITE_WEAK", GF_LITE_WEAK);
	add_constant(d, "GF_DARK_WEAK", GF_DARK_WEAK);
	add_constant(d, "GF_WATER", GF_WATER);
	add_constant(d, "GF_PLASMA", GF_PLASMA);
	add_constant(d, "GF_METEOR", GF_METEOR);
	add_constant(d, "GF_ICE", GF_ICE);
	add_constant(d, "GF_GRAVITY", GF_GRAVITY);
	add_constant(d, "GF_INERTIA", GF_INERTIA);
	add_constant(d, "GF_FORCE", GF_FORCE);
	add_constant(d, "GF_TIME", GF_TIME);
	add_constant(d, "GF_ACID", GF_ACID);
	add_constant(d, "GF_ELEC", GF_ELEC);
	add_constant(d, "GF_FIRE", GF_FIRE);
	add_constant(d, "GF_COLD", GF_COLD);
	add_constant(d, "GF_POIS", GF_POIS);
	add_constant(d, "GF_LITE", GF_LITE);
	add_constant(d, "GF_DARK", GF_DARK);
	add_constant(d, "GF_CONFUSION", GF_CONFUSION);
	add_constant(d, "GF_SOUND", GF_SOUND);
        add_constant(d, "GF_SHARDS", GF_SHARDS);
	add_constant(d, "GF_NEXUS", GF_NEXUS);
	add_constant(d, "GF_NETHER", GF_NETHER);
	add_constant(d, "GF_CHAOS", GF_CHAOS);
	add_constant(d, "GF_DISENCHANT", GF_DISENCHANT);
	add_constant(d, "GF_KILL_WALL", GF_KILL_WALL);
	add_constant(d, "GF_KILL_DOOR", GF_KILL_DOOR);
	add_constant(d, "GF_KILL_TRAP", GF_KILL_TRAP);
	add_constant(d, "GF_MAKE_WALL", GF_MAKE_WALL);
	add_constant(d, "GF_MAKE_DOOR", GF_MAKE_DOOR);
	add_constant(d, "GF_MAKE_TRAP", GF_MAKE_TRAP);
	add_constant(d, "GF_AWAY_UNDEAD", GF_AWAY_UNDEAD);
	add_constant(d, "GF_AWAY_EVIL", GF_AWAY_EVIL);
	add_constant(d, "GF_AWAY_ALL", GF_AWAY_ALL);
	add_constant(d, "GF_TURN_UNDEAD", GF_TURN_UNDEAD);
	add_constant(d, "GF_TURN_EVIL", GF_TURN_EVIL);
	add_constant(d, "GF_TURN_ALL", GF_TURN_ALL);
	add_constant(d, "GF_DISP_UNDEAD", GF_DISP_UNDEAD);
	add_constant(d, "GF_DISP_EVIL", GF_DISP_EVIL);
	add_constant(d, "GF_DISP_ALL", GF_DISP_ALL);
	add_constant(d, "GF_OLD_CLONE", GF_OLD_CLONE);
	add_constant(d, "GF_OLD_POLY", GF_OLD_POLY);
	add_constant(d, "GF_OLD_HEAL", GF_OLD_HEAL);
	add_constant(d, "GF_OLD_SPEED", GF_OLD_SPEED);
	add_constant(d, "GF_OLD_SLOW", GF_OLD_SLOW);
	add_constant(d, "GF_OLD_CONF", GF_OLD_CONF);
	add_constant(d, "GF_OLD_SLEEP", GF_OLD_SLEEP);
	add_constant(d, "GF_OLD_DRAIN", GF_OLD_DRAIN);
	add_constant(d, "PROJECT_JUMP", PROJECT_JUMP);
	add_constant(d, "PROJECT_BEAM", PROJECT_BEAM);
	add_constant(d, "PROJECT_THRU", PROJECT_THRU);
	add_constant(d, "PROJECT_STOP", PROJECT_STOP);
	add_constant(d, "PROJECT_GRID", PROJECT_GRID);
	add_constant(d, "PROJECT_ITEM", PROJECT_ITEM);
	add_constant(d, "PROJECT_KILL", PROJECT_KILL);
	add_constant(d, "PROJECT_HIDE", PROJECT_HIDE);
}
#endif
