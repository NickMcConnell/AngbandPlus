/* File: cavemod.c */

/*
 * Copyright (c) 1998 Keldon Jones
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.
 */

/*
 * This file contains an interface to Angband functions that deal with
 * the dungeon.
 */

#include "angband.h"

/*
 * Approximate distance between two points
 */
static PyObject *cave_distance(PyObject *self, PyObject *args)
{
	int y1, x1, y2, x2, d;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, "(ii)(ii)", &y1, &x1, &y2, &x2))
		return NULL;

	/* Calculate distance */
	d = distance(y1, x1, y2, x2);

	/* Return */
	return Py_BuildValue("i", d);
}

/*
 * Check LOS between two points
 */
static PyObject *cave_los(PyObject *self, PyObject *args)
{
	int y1, x1, y2, x2;
	bool retval;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, "(ii)(ii)", &y1, &x1, &y2, &x2))
		return NULL;

	/* Calculate LOS */
	retval = los(y1, x1, y2, x2);

	/* Return */
	return Py_BuildValue("i", retval);
}

/*
 * Can the player see a given grid?
 */
static PyObject *cave_player_can_see_bold(PyObject *self, PyObject *args)
{
	int y, x;
	bool retval;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, "(ii)", &y, &x))
		return NULL;

	/* Check for visibility */
	retval = player_can_see_bold(y, x);

	/* Return */
	return Py_BuildValue("i", retval);
}

/*
 * Is the player's grid dark?
 */
static PyObject *cave_no_lite(PyObject *self, PyObject *args)
{
	bool retval;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, ""))
		return NULL;

	/* Check lite */
	retval = no_lite();

	/* Return */
	return Py_BuildValue("i", retval);
}

/*
 * Determine if a given location may be "destroyed"
 */
static PyObject *cave_cave_valid_bold(PyObject *self, PyObject *args)
{
	int y, x;
	bool retval;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, "(ii)", &y, &x))
		return NULL;

	/* Check for destroy-ability */
	retval = cave_valid_bold(y, x);

	/* Return */
	return Py_BuildValue("i", retval);
}

/*
 * Return the attr/char code at a given map location
 */
static PyObject *cave_map_info(PyObject *self, PyObject *args)
{
	int y, x;
	byte a;
	char c;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, "(ii)", &y, &x))
		return NULL;

	/* Get map info */
	map_info(y, x, &a, &c);

	/* Return */
	return Py_BuildValue("(bc)", a, c);
}

/*
 * Map some of the surrounding area (ala "magic mapping")
 */
static PyObject *cave_map_area(PyObject *self, PyObject *args)
{
	/* Parse arguments */
	if (!PyArg_ParseTuple(args, ""))
		return NULL;

	/* Map the area */
	map_area();

	/* Return nothing */
	Py_INCREF(Py_None);
	return Py_None;
}

/*
 * Lite up the dungeon
 */
static PyObject *cave_wiz_lite(PyObject *self, PyObject *args)
{
	/* Parse arguments */
	if (!PyArg_ParseTuple(args, ""))
		return NULL;

	/* Lite the dungeon */
	wiz_lite();

	/* Return nothing */
	Py_INCREF(Py_None);
	return Py_None;
}

/*
 * Forget the dungeon map
 */
static PyObject *cave_wiz_dark(PyObject *self, PyObject *args)
{
	/* Parse arguments */
	if (!PyArg_ParseTuple(args, ""))
		return NULL;

	/* Forget the dungeon */
	wiz_dark();

	/* Return nothing */
	Py_INCREF(Py_None);
	return Py_None;
}

/*
 * Set the feature type of a grid
 */
static PyObject *cave_set_feat_hack(PyObject *self, PyObject *args)
{
	int y, x, feat;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, "(ii)i", &y, &x, &feat))
		return NULL;

	/* Set feature */
	cave_set_feat(y, x, feat);

	/* Return nothing */
	Py_INCREF(Py_None);
	return Py_None;
}

/*
 * Get the feature type of a grid
 */
static PyObject *cave_get_feat(PyObject *self, PyObject *args)
{
	int y, x, feat;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, "(ii)", &y, &x))
		return NULL;

	/* Get feature */
	feat = cave_feat[y][x];

	/* Return */
	return Py_BuildValue("i", feat);
}

/*
 * Set the set of flags of a grid
 */
static PyObject *cave_set_info(PyObject *self, PyObject *args)
{
	int y, x, info;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, "(ii)i", &y, &x, &info))
		return NULL;

	/* Set info */
	cave_info[y][x] = info;

	/* Return nothing */
	Py_INCREF(Py_None);
	return Py_None;
}

/*
 * Get the set of flags of a grid
 */
static PyObject *cave_get_info(PyObject *self, PyObject *args)
{
	int y, x, info;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, "(ii)", &y, &x))
		return NULL;

	/* Get info */
	info = cave_info[y][x];

	/* Return */
	return Py_BuildValue("i", info);
}

/*
 * Get the object index in a grid
 */
static PyObject *cave_get_o_idx(PyObject *self, PyObject *args)
{
	int y, x, o_idx;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, "(ii)", &y, &x))
		return NULL;

	/* Get object index */
	o_idx = cave_o_idx[y][x];

	/* Return */
	return Py_BuildValue("i", o_idx);
}

/*
 * Get the monster index in a grid
 */
static PyObject *cave_get_m_idx(PyObject *self, PyObject *args)
{
	int y, x, m_idx;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, "(ii)", &y, &x))
		return NULL;

	/* Get monster index */
	m_idx = cave_m_idx[y][x];

	/* Return */
	return Py_BuildValue("i", m_idx);
}

/*
 * "Find me a location"
 */
static PyObject *cave_scatter(PyObject *self, PyObject *args)
{
	int yp, xp, y, x, d;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, "iii", &y, &x, &d))
		return NULL;

	/* Scatter */
	scatter(&yp, &xp, y, x, d, 0);

	/* Return */
	return Py_BuildValue("(ii)", y, x);
}

/*
 * Delta X, Y of a direction
 */
static PyObject *cave_dd(PyObject *self, PyObject *args)
{
	int d, x, y;

	/* Parse argument */
	if (!PyArg_ParseTuple(args, "i", &d))
		return NULL;

	/* Get deltas */
	y = ddy[d];
	x = ddx[d];
	
	/* Return */
	return Py_BuildValue("(ii)", y, x);
}

/*
 * Return number of turns passed
 */
static PyObject *cave_turns(PyObject *self, PyObject *args)
{
	/* Parse arguments */
	if (!PyArg_ParseTuple(args, ""))
		return NULL;

	/* Return */
	return Py_BuildValue("i", turn);
}

/*
 * Set name used for this dungeon level
 */
static PyObject *cave_set_name(PyObject *self, PyObject *args)
{
	char *name;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, "s", &name))
		return NULL;

	/* Save name */
	strcpy(depth_name, name);

	/* Return nothing */
	Py_INCREF(Py_None);
	return Py_None;
}

/*
 * Load a dungeon map from a file
 */
static PyObject *cave_load_map(PyObject *self, PyObject *args)
{
	char *name, buf[1024], ch;
	FILE *fff;
	int row = 0, j, len;
	bool daytime = FALSE;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, "s", &name))
		return NULL;

	/* Check for daytime */
	if ((turn % (10L * TOWN_DAWN)) < ((10L * TOWN_DAWN) / 2))
	{
		/* Day */
		daytime = TRUE;
	}

	/* Build filename */
	path_build(buf, 1024, ANGBAND_DIR_SCPT, name);

	/* Open file */
	fff = my_fopen(buf, "r");

	/* Read until end of file */
	while (1)
	{
		/* Start at far left */
		j = 0;

		/* Begin reading */
		my_fgets(fff, buf, 1024);

		/* Check for EOF */
		if (feof(fff)) break;

		/* Check number of rows */
		if (row >= DUNGEON_HGT)
		{
			/* Error */
			PyErr_SetString(PyExc_ValueError, "too many lines");
			return NULL;
		}

		/* Get length */
		len = strlen(buf);

		/* Check for line too long */
		if (len > DUNGEON_WID)
		{
			/* Error */
			PyErr_SetString(PyExc_ValueError, "line too long");
			return NULL;
		}

		/* Scan line */
		for (j = 0; j < len; j++)
		{
			/* Acquire character */
			ch = buf[j];

			/* Switch on character */
			switch (ch)
			{
				/* Normal wall */
				case '#':
				{
					cave_set_feat(row, j, FEAT_WALL_EXTRA);
					break;
				}

				/* Floor */
				case '.':
				{
					cave_set_feat(row, j, FEAT_FLOOR);
					break;
				}

				/* Door */
				case '+':
				{
					cave_set_feat(row, j, FEAT_DOOR_HEAD);
					break;
				}

				/* Unpassable wall */
				case ' ':
				case 'X':
				{
					cave_set_feat(row, j, FEAT_PERM_EXTRA);
					break;
				}

				/* Down staircase */
				case '>':
				{
					cave_set_feat(row, j, FEAT_MORE);
					break;
				}

				/* Up staircase */
				case '<':
				{
					cave_set_feat(row, j, FEAT_LESS);
					break;
				}

				/* Store */
				case '1': case '2': case '3': case '4':
				case '5': case '6': case '7': case '8':
				{
					byte feat;

					feat = FEAT_SHOP_HEAD + ch - '1';
					cave_set_feat(row, j, feat);
					break;
				}

				/* Water */
				case '~':
				{
					cave_set_feat(row, j, FEAT_WATER_HEAD);
					break;
				}

				/* Tree */
				case 't':
				case 'T':
				{
					cave_set_feat(row, j, FEAT_TREE_HEAD);
					break;
				}

				/* Grass */
				case 'g':
				case 'G':
				{
					cave_set_feat(row, j, FEAT_GRASS);
					break;
				}

				/* Dirt */
				case 'd':
				case 'D':
				{
					cave_set_feat(row, j, FEAT_DIRT);
					break;
				}

				/* Rubble */
				case ':':
				{
					cave_set_feat(row, j, FEAT_RUBBLE);
					break;
				}
			}

			/* Lite if daytime in town */
			if (daytime)
			{
				/* Lite and memorize this grid */
				cave_info[row][j] |= (CAVE_GLOW | CAVE_MARK);
			}
			else
			{
				/* Darken and forget this grid */
				cave_info[row][j] &= ~(CAVE_GLOW | CAVE_MARK);
			}
		}

		/* Next row */
		row++;
	}

	/* Close file */
	my_fclose(fff);

	/* Return nothing */
	Py_INCREF(Py_None);
	return Py_None;
}

/*
 * Method table for all functions provided by the module
 */
static PyMethodDef caveMethods[] =
{
	{ "distance", cave_distance, 1 },
	{ "los", cave_los, 1 },
	{ "player_can_see_bold", cave_player_can_see_bold, 1 },
	{ "no_lite", cave_no_lite, 1 },
	{ "cave_valid_bold", cave_cave_valid_bold, 1 },
	{ "map_info", cave_map_info, 1 },
	{ "map_area", cave_map_area, 1 },
	{ "wiz_lite", cave_wiz_lite, 1 },
	{ "wiz_dark", cave_wiz_dark, 1 },
	{ "set_feat", cave_set_feat_hack, 1 },
	{ "get_feat", cave_get_feat, 1 },
	{ "set_info", cave_set_info, 1 },
	{ "get_info", cave_get_info, 1 },
	{ "get_o_idx", cave_get_o_idx, 1 },
	{ "get_m_idx", cave_get_m_idx, 1 },
	{ "scatter", cave_scatter, 1 },
	{ "dd", cave_dd, 1 },
	{ "turns", cave_turns, 1 },
	{ "set_name", cave_set_name, 1 },
	{ "load_map", cave_load_map, 1 },
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
 * Initialize the cave module
 */
void initcave(void)
{
	PyObject *m, *d;

	/* Tell the interpreter to insert the module */
	m = Py_InitModule("cave", caveMethods);

	/* Add some symbolic constants to the module */
	d = PyModule_GetDict(m);

	/* Add each constant */
	add_constant(d, "BLOCK_HGT", BLOCK_HGT);
	add_constant(d, "BLOCK_WID", BLOCK_WID);
	add_constant(d, "PANEL_HGT", PANEL_HGT);
	add_constant(d, "PANEL_WID", PANEL_WID);
	add_constant(d, "SCREEN_HGT", SCREEN_HGT);
	add_constant(d, "SCREEN_WID", SCREEN_WID);
	add_constant(d, "DUNGEON_HGT", DUNGEON_HGT);
	add_constant(d, "DUNGEON_WID", DUNGEON_WID);
	add_constant(d, "MAX_DEPTH", MAX_DEPTH);
	add_constant(d, "FF_SOLID", FF_SOLID);
	add_constant(d, "FF_OPAQUE", FF_OPAQUE);
	add_constant(d, "FF_PERMANENT", FF_PERMANENT);
	add_constant(d, "FF_NO_MISSILE", FF_NO_MISSILE);
	add_constant(d, "FF_HOLD_OBJECT", FF_HOLD_OBJECT);
	add_constant(d, "FF_DOOR", FF_DOOR);
	add_constant(d, "FEAT_NONE", FEAT_NONE);
	add_constant(d, "FEAT_FLOOR", FEAT_FLOOR);
	add_constant(d, "FEAT_GRASS", FEAT_GRASS);
	add_constant(d, "FEAT_DIRT", FEAT_DIRT);
	add_constant(d, "FEAT_INVIS", FEAT_INVIS);
	add_constant(d, "FEAT_GLYPH", FEAT_GLYPH);
	add_constant(d, "FEAT_OPEN", FEAT_OPEN);
	add_constant(d, "FEAT_BROKEN", FEAT_BROKEN);
	add_constant(d, "FEAT_LESS", FEAT_LESS);
	add_constant(d, "FEAT_MORE", FEAT_MORE);
	add_constant(d, "FEAT_TRAP_HEAD", FEAT_TRAP_HEAD);
	add_constant(d, "FEAT_TRAP_TAIL", FEAT_TRAP_TAIL);
	add_constant(d, "FEAT_SHOP_HEAD", FEAT_SHOP_HEAD);
	add_constant(d, "FEAT_SHOP_TAIL", FEAT_SHOP_TAIL);
	add_constant(d, "FEAT_TREE_HEAD", FEAT_TREE_HEAD);
	add_constant(d, "FEAT_TREE_TAIL", FEAT_TREE_TAIL);
	add_constant(d, "FEAT_WATER_HEAD", FEAT_WATER_HEAD);
	add_constant(d, "FEAT_WATER_TAIL", FEAT_WATER_TAIL);
	add_constant(d, "FEAT_LAVA_HEAD", FEAT_LAVA_HEAD);
	add_constant(d, "FEAT_LAVA_TAIL", FEAT_LAVA_TAIL);
	add_constant(d, "FEAT_DOOR_HEAD", FEAT_DOOR_HEAD);
	add_constant(d, "FEAT_DOOR_TAIL", FEAT_DOOR_TAIL);
	add_constant(d, "FEAT_SECRET", FEAT_SECRET);
	add_constant(d, "FEAT_RUBBLE", FEAT_RUBBLE);
	add_constant(d, "FEAT_MAGMA", FEAT_MAGMA);
	add_constant(d, "FEAT_QUARTZ", FEAT_QUARTZ);
	add_constant(d, "FEAT_MAGMA_H", FEAT_MAGMA_H);
	add_constant(d, "FEAT_QUARTZ_H", FEAT_QUARTZ_H);
	add_constant(d, "FEAT_MAGMA_K", FEAT_MAGMA_K);
	add_constant(d, "FEAT_QUARTZ_K", FEAT_QUARTZ_K);
	add_constant(d, "FEAT_WALL_EXTRA", FEAT_WALL_EXTRA);
	add_constant(d, "FEAT_WALL_INNER", FEAT_WALL_INNER);
	add_constant(d, "FEAT_WALL_OUTER", FEAT_WALL_OUTER);
	add_constant(d, "FEAT_WALL_SOLID", FEAT_WALL_SOLID);
	add_constant(d, "FEAT_PERM_EXTRA", FEAT_PERM_EXTRA);
	add_constant(d, "FEAT_PERM_INNER", FEAT_PERM_INNER);
	add_constant(d, "FEAT_PERM_OUTER", FEAT_PERM_OUTER);
	add_constant(d, "FEAT_PERM_SOLID", FEAT_PERM_SOLID);
	add_constant(d, "CAVE_MARK", CAVE_MARK);
	add_constant(d, "CAVE_GLOW", CAVE_GLOW);
	add_constant(d, "CAVE_ICKY", CAVE_ICKY);
	add_constant(d, "CAVE_ROOM", CAVE_ROOM);
	add_constant(d, "CAVE_SEEN", CAVE_SEEN);
	add_constant(d, "CAVE_VIEW", CAVE_VIEW);
	add_constant(d, "CAVE_TEMP", CAVE_TEMP);
	add_constant(d, "CAVE_WALL", CAVE_WALL);
}
