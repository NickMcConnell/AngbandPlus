/* File: objmod.c */

/*
 * Copyright (c) 1998 Keldon Jones
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.
 */

/*
 * This file provides a Python interface to Angband's "player" data type,
 * and provide various methods for handling and manipulating it.
 */

#include "angband.h"

/*
 * The "player" class.
 */
typedef struct PlayerObject PlayerObject;

struct PlayerObject
{
	PyObject_HEAD		/* Stuff common to all classes */
};

/*
 * We need this "Player_Type" variable throughout this module, but to define
 * it completely, we need function pointers that are defined later.  So
 * we need to "prototype" the variable by using the "staticforward"
 * keyword.  So strange....
 */
staticforward PyTypeObject Player_Type;

/*
 * Macro to test whether an object is a player
 */
#define PlayerObject_Check(v)	((v)->ob_type == &Player_Type)

/*
 * Create a new player object.
 */
static PlayerObject *getPlayerObject(void)
{
	PlayerObject *self;

	/* Allocate memory (perhaps use Angband macros instead?) */
	self = PyObject_NEW(PlayerObject, &Player_Type);

	/* Return object */
	return self;
}

/*
 * Destructor.  Decrement references on all member variables and delete
 * memory for the object.
 */
static void Player_dealloc(PlayerObject *self)
{
	/* Dealloc object */
	PyMem_DEL(self);
}

/*
 * Table of class methods
 */
static PyMethodDef Player_methods[] =
{
	{ NULL, NULL }
};

/* XXX XXX Try to save some typing */
#define LOOK_FIELD(text, field) \
	if (streq(name, text)) \
		return Py_BuildValue("i", p_ptr->field)

/*
 * Return a player attribute
 */
static PyObject *Player_getattr(PlayerObject *self, char *name)
{
	/* Attempt to find a "standard" field */
	LOOK_FIELD("py", py);
	LOOK_FIELD("px", px);
	LOOK_FIELD("psex", psex);
	LOOK_FIELD("prace", prace);
	LOOK_FIELD("pclass", pclass);
	LOOK_FIELD("maximize", maximize);
	LOOK_FIELD("preserve", preserve);
	LOOK_FIELD("age", age);
	LOOK_FIELD("ht", ht);
	LOOK_FIELD("wt", wt);
	LOOK_FIELD("sc", sc);
	LOOK_FIELD("au", au);
	LOOK_FIELD("max_depth", max_depth);
	LOOK_FIELD("depth", depth);
	LOOK_FIELD("max_lev", max_lev);
	LOOK_FIELD("lev", lev);
	LOOK_FIELD("max_exp", max_exp);
	LOOK_FIELD("exp", exp);
	LOOK_FIELD("mhp", mhp);
	LOOK_FIELD("chp", chp);
	LOOK_FIELD("msp", msp);
	LOOK_FIELD("csp", csp);
	LOOK_FIELD("blind", blind);
	LOOK_FIELD("confused", confused);
	LOOK_FIELD("poisoned", poisoned);
	LOOK_FIELD("afraid", afraid);
	LOOK_FIELD("paralyzed", paralyzed);
	LOOK_FIELD("image", image);
	LOOK_FIELD("fast", fast);
	LOOK_FIELD("slow", slow);
	LOOK_FIELD("shield", shield);
	LOOK_FIELD("blessed", blessed);
	LOOK_FIELD("hero", hero);
	LOOK_FIELD("shero", shero);
	LOOK_FIELD("protevil", protevil);
	LOOK_FIELD("invuln", invuln);
	LOOK_FIELD("tim_invis", tim_invis);
	LOOK_FIELD("tim_infra", tim_infra);
	LOOK_FIELD("oppose_acid", oppose_acid);
	LOOK_FIELD("oppose_elec", oppose_elec);
	LOOK_FIELD("oppose_fire", oppose_fire);
	LOOK_FIELD("oppose_cold", oppose_cold);
	LOOK_FIELD("oppose_pois", oppose_pois);
	LOOK_FIELD("stun", stun);
	LOOK_FIELD("cut", cut);
	LOOK_FIELD("food", food);
	LOOK_FIELD("word_recall", word_recall);
	LOOK_FIELD("energy", energy);
	LOOK_FIELD("confusing", confusing);
	LOOK_FIELD("searching", searching);
	LOOK_FIELD("target_who", target_who);
	LOOK_FIELD("target_row", target_row);
	LOOK_FIELD("target_col", target_col);
	LOOK_FIELD("health_who", health_who);
	LOOK_FIELD("energy_use", energy_use);
	LOOK_FIELD("cumber_armor", cumber_armor);
	LOOK_FIELD("cumber_glove", cumber_glove);
	LOOK_FIELD("heavy_wield", heavy_wield);
	LOOK_FIELD("heavy_shoot", heavy_shoot);
	LOOK_FIELD("icky_wield", icky_wield);
	LOOK_FIELD("cur_lite", cur_lite);
	LOOK_FIELD("notice", notice);
	LOOK_FIELD("update", update);
	LOOK_FIELD("redraw", redraw);
	LOOK_FIELD("window", window);
	LOOK_FIELD("immune_acid", immune_acid);
	LOOK_FIELD("immune_elec", immune_elec);
	LOOK_FIELD("immune_fire", immune_fire);
	LOOK_FIELD("immune_cold", immune_cold);
	LOOK_FIELD("resist_acid", resist_acid);
	LOOK_FIELD("resist_elec", resist_elec);
	LOOK_FIELD("resist_fire", resist_fire);
	LOOK_FIELD("resist_cold", resist_cold);
	LOOK_FIELD("resist_pois", resist_pois);
	LOOK_FIELD("resist_fear", resist_fear);
	LOOK_FIELD("resist_lite", resist_lite);
	LOOK_FIELD("resist_dark", resist_dark);
	LOOK_FIELD("resist_blind", resist_blind);
	LOOK_FIELD("resist_confu", resist_confu);
	LOOK_FIELD("resist_sound", resist_sound);
	LOOK_FIELD("resist_shard", resist_shard);
	LOOK_FIELD("resist_nexus", resist_nexus);
	LOOK_FIELD("resist_nethr", resist_nethr);
	LOOK_FIELD("resist_chaos", resist_chaos);
	LOOK_FIELD("resist_disen", resist_disen);
	LOOK_FIELD("sustain_str", sustain_str);
	LOOK_FIELD("sustain_int", sustain_int);
	LOOK_FIELD("sustain_wis", sustain_wis);
	LOOK_FIELD("sustain_dex", sustain_dex);
	LOOK_FIELD("sustain_con", sustain_con);
	LOOK_FIELD("sustain_chr", sustain_chr);
	LOOK_FIELD("slow_digest", slow_digest);
	LOOK_FIELD("ffall", ffall);
	LOOK_FIELD("lite", lite);
	LOOK_FIELD("regenerate", regenerate);
	LOOK_FIELD("telepathy", telepathy);
	LOOK_FIELD("see_inv", see_inv);
	LOOK_FIELD("free_act", free_act);
	LOOK_FIELD("hold_life", hold_life);
	LOOK_FIELD("impact", impact);
	LOOK_FIELD("aggravate", aggravate);
	LOOK_FIELD("teleport", teleport);
	LOOK_FIELD("exp_drain", exp_drain);
	LOOK_FIELD("bless_blade", bless_blade);
	LOOK_FIELD("dis_to_h", dis_to_h);
	LOOK_FIELD("dis_to_d", dis_to_d);
	LOOK_FIELD("dis_to_a", dis_to_a);
	LOOK_FIELD("dis_ac", dis_ac);
	LOOK_FIELD("to_h", to_h);
	LOOK_FIELD("to_d", to_d);
	LOOK_FIELD("to_a", to_a);
	LOOK_FIELD("ac", ac);
	LOOK_FIELD("see_infra", see_infra);
	LOOK_FIELD("skill_dis", skill_dis);
	LOOK_FIELD("skill_dev", skill_dev);
	LOOK_FIELD("skill_sav", skill_sav);
	LOOK_FIELD("skill_stl", skill_stl);
	LOOK_FIELD("skill_srh", skill_srh);
	LOOK_FIELD("skill_fos", skill_fos);
	LOOK_FIELD("skill_thn", skill_thn);
	LOOK_FIELD("skill_thb", skill_thb);
	LOOK_FIELD("skill_tht", skill_tht);
	LOOK_FIELD("skill_dig", skill_dig);
	LOOK_FIELD("noise", noise);
	LOOK_FIELD("num_blow", num_blow);
	LOOK_FIELD("num_fire", num_fire);
	LOOK_FIELD("ammo_mult", ammo_mult);
	LOOK_FIELD("ammo_tval", ammo_tval);
	LOOK_FIELD("pspeed", pspeed);

	/* Try to find a method */
	return Py_FindMethod(Player_methods, (PyObject *)self, name);
}

/* Forward declaration */
typedef struct set_func set_func;

/* Structure holding function to set a player field */
struct set_func
{
	char *name;		/* Name of field */
	bool (*func)(int v);	/* Function to set field */
};

/*
 * Table of all "setter" functions
 */
set_func setter_functions[] =
{
	{ "blind", set_blind },
	{ "confused", set_confused },
	{ "poisoned", set_poisoned },
	{ "afraid", set_afraid },
	{ "paralyzed", set_paralyzed },
	{ "image", set_image },
	{ "fast", set_fast },
	{ "slow", set_slow },
	{ "shield", set_shield },
	{ "blessed", set_blessed },
	{ "hero", set_hero },
	{ "shero", set_shero },
	{ "protevil", set_protevil },
	{ "invuln", set_invuln },
	{ "tim_invis", set_tim_invis },
	{ "tim_infra", set_tim_infra },
	{ "oppose_acid", set_oppose_acid },
	{ "oppose_elec", set_oppose_elec },
	{ "oppose_fire", set_oppose_fire },
	{ "oppose_cold", set_oppose_cold },
	{ "oppose_pois", set_oppose_pois },
	{ "stun", set_stun },
	{ "cut", set_cut },
	{ "food", set_food },
	{ NULL, NULL }
};

/* XXX XXX Try to save space */
#define SET_FIELD(text, field) \
	if (!done && streq(name, text)) \
	{ \
		p_ptr->field = value; \
		done = TRUE; \
	}

/* XXX XXX Read-only fields */
static cptr error_fields =
"#py#px#psex#prace#pclass#maximize#preserve#age#ht#wt#sc\
#cumber_armor#cumber_glove#heavy_wield#heavy_shoot#icky_wield\
#cur_lite#immune_acid#immune_elec#immune_fire#immune_cold\
#resist_acid#resist_elec#resist_fire#resist_cold#resist_pois\
#resist_fear#resist_lite#resist_dark#resist_blind#resist_confu\
#resist_sound#resist_shard#resist_nexus#resist_nethr#resist_chaos\
#resist_disen#sustain_str#sustain_int#sustain_wis#sustain_dex\
#sustain_con#sustain_chr#slow_digest#ffall#lite#regenerate#telepathy\
#see_inv#free_act#hold_life#impact#aggravate#teleport#exp_drain\
#bless_blade#dis_to_h#dis_to_d#dis_to_a#dis_ac#to_h#to_d#to_a\
#ac#see_infra#skill_dis#skill_dev#skill_sav#skill_stl#skill_srh\
#skill_fos#skill_thn#skill_thb#skill_tht#skill_dig#noise\
#num_blow#num_fire#ammo_mult#ammo_tval#pspeed#";

/*
 * Set a player attribute
 */
static int Player_setattr(PlayerObject *self, char *name, PyObject *v)
{
	int value = 0, i;
	char buf[1024];
	bool done = FALSE;

	/* Extract the value from the Python object */
	if (v) value = PyInt_AsLong(v);

	/* Start at beginning of setter functions */
	i = 0;

	/* Attempt to find a field with a "set" function */
	while (1)
	{
		/* Check for end */
		if (!setter_functions[i].name) break;

		/* Check this one */
		if (streq(name, setter_functions[i].name))
		{
			/* Call the function */
			(*setter_functions[i].func)(value);

			/* Return */
			return 0;
		}
		
		/* Next */
		i++;
	}

	/* Attempt to set a "standard" field */
	SET_FIELD("au", au);
	SET_FIELD("max_depth", max_depth);
	SET_FIELD("depth", depth);
	SET_FIELD("max_lev", max_lev);
	SET_FIELD("lev", lev);
	SET_FIELD("max_exp", max_exp);
	SET_FIELD("exp", exp);
	SET_FIELD("mhp", mhp);
	SET_FIELD("chp", chp);
	SET_FIELD("msp", msp);
	SET_FIELD("csp", csp);
	SET_FIELD("word_recall", word_recall);
	SET_FIELD("energy", energy);
	SET_FIELD("confusing", confusing);
	SET_FIELD("searching", searching);
	SET_FIELD("target_who", target_who);
	SET_FIELD("target_row", target_row);
	SET_FIELD("target_col", target_col);
	SET_FIELD("health_who", health_who);
	SET_FIELD("energy_use", energy_use);
	SET_FIELD("notice", notice);
	SET_FIELD("update", update);
	SET_FIELD("redraw", redraw);
	SET_FIELD("window", window);

	/* Check for completion */
	if (done)
	{
		/* Done */
		return 0;
	}

	/* Check for several "read-only" fields */
	sprintf(buf, "#%s#", name);

	/* Search */
	if (strstr(error_fields, buf))
	{
		/* Set error string */
		PyErr_SetString(PyExc_AttributeError,
				"Set read-only player field");
		
		return -1;
	}

	/* Non-existant field, throw error */
	PyErr_SetString(PyExc_AttributeError, "Try to set bad player field");

	/* Error */
	return -1;
}

/*
 * Here is the complete definition of that bizarre "Player_Type" that was
 * "prototyped" above.
 */
staticforward PyTypeObject Player_Type =
{
	PyObject_HEAD_INIT(&PyType_Type)	/* Standard stuff */
	0,					/* Size */
	"Player",				/* Name */
	sizeof(PlayerObject),			/* Basic Size */
	0,					/* Item Size */
	(destructor)Player_dealloc,		/* Destructor */
	0,					/* Print */
	(getattrfunc)Player_getattr,		/* Attribute getter */
	(setattrfunc)Player_setattr,		/* Attribute setter */
	0,
	0,
	0,
	0,
	0,
	0
};

/*
 * Module function to return a Player object
 */
static PyObject *player_get(PyObject *self, PyObject *args)
{
	/* Parse arguments */
	if (!PyArg_ParseTuple(args, ""))
		return NULL;

	/* Create an Mon and return it */
	return (PyObject *)getPlayerObject();
}

/*
 * Have the player take damage
 */
static PyObject *player_take_hit(PyObject *self, PyObject *args)
{
	int dam;
	char *who;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, "is", &dam, &who))
		return NULL;

	/* Take damage */
	take_hit(dam, who);

	/* Return nothing */
	Py_INCREF(Py_None);
	return Py_None;
}

/*
 * Handle any "redraw", "window", "notice", or "update" commands
 */
static PyObject *player_handle_stuff(PyObject *self, PyObject *args)
{
	/* Parse arguments */
	if (!PyArg_ParseTuple(args, ""))
		return NULL;

	/* Handle stuff */
	handle_stuff();

	/* Return nothing */
	Py_INCREF(Py_None);
	return Py_None;
}

/*
 * Move the player to a specific grid (no questions asked)
 */
static PyObject *player_player_place(PyObject *self, PyObject *args)
{
	int y, x;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, "ii", &y, &x))
		return NULL;

	/* Place the player */
	player_place(y, x);

	/* Return nothing */
	Py_INCREF(Py_None);
	return Py_None;
}

/*
 * Function table for this module
 */
static PyMethodDef player_methods[] =
{
	{ "get", player_get, 1 },
	{ "take_hit", player_take_hit, 1 },
	{ "handle_stuff", player_handle_stuff, 1 },
	{ "player_place", player_player_place, 1 },
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
void initplayer()
{
	PyObject *m, *d;

	/* Create the module */
	m = Py_InitModule("player", player_methods);
	
	/* Get the module's dictionary */
	d = PyModule_GetDict(m);

	/* Add some symbolic constants to the module */
	add_constant(d, "PY_MAX_EXP", PY_MAX_EXP);
	add_constant(d, "PY_MAX_GOLD", PY_MAX_GOLD);
	add_constant(d, "PY_MAX_LEVEL", PY_MAX_LEVEL);
	add_constant(d, "PY_FOOD_MAX", PY_FOOD_MAX);
	add_constant(d, "PY_FOOD_FULL", PY_FOOD_FULL);
	add_constant(d, "PY_FOOD_ALERT", PY_FOOD_ALERT);
	add_constant(d, "PY_FOOD_WEAK", PY_FOOD_WEAK);
	add_constant(d, "PY_FOOD_FAINT", PY_FOOD_FAINT);
	add_constant(d, "PY_FOOD_STARVE", PY_FOOD_STARVE);
	add_constant(d, "PY_REGEN_NORMAL", PY_REGEN_NORMAL);
	add_constant(d, "PY_REGEN_WEAK", PY_REGEN_WEAK);
	add_constant(d, "PY_REGEN_FAINT", PY_REGEN_FAINT);
	add_constant(d, "PY_REGEN_HPBASE", PY_REGEN_HPBASE);
	add_constant(d, "PY_REGEN_MNBASE", PY_REGEN_MNBASE);
	add_constant(d, "SEX_FEMALE", SEX_FEMALE);
	add_constant(d, "SEX_MALE", SEX_MALE);
	add_constant(d, "RACE_HUMAN", RACE_HUMAN);
	add_constant(d, "RACE_HALF_ELF", RACE_HALF_ELF);
	add_constant(d, "RACE_ELF", RACE_ELF);
	add_constant(d, "RACE_HOBBIT", RACE_HOBBIT);
	add_constant(d, "RACE_GNOME", RACE_GNOME);
	add_constant(d, "RACE_DWARF", RACE_DWARF);
	add_constant(d, "RACE_HALF_ORC", RACE_HALF_ORC);
	add_constant(d, "RACE_HALF_TROLL", RACE_HALF_TROLL);
	add_constant(d, "RACE_DUNADAN", RACE_DUNADAN);
	add_constant(d, "RACE_HIGH_ELF", RACE_HIGH_ELF);
	add_constant(d, "CLASS_WARRIOR", CLASS_WARRIOR);
	add_constant(d, "CLASS_MAGE", CLASS_MAGE);
	add_constant(d, "CLASS_PRIEST", CLASS_PRIEST);
	add_constant(d, "CLASS_ROGUE", CLASS_ROGUE);
	add_constant(d, "CLASS_RANGER", CLASS_RANGER);
	add_constant(d, "CLASS_PALADIN", CLASS_PALADIN);
	add_constant(d, "PN_COMBINE", PN_COMBINE);
	add_constant(d, "PN_REORDER", PN_REORDER);
	add_constant(d, "PU_BONUS", PU_BONUS);
	add_constant(d, "PU_TORCH", PU_TORCH);
	add_constant(d, "PU_HP", PU_HP);
	add_constant(d, "PU_MANA", PU_MANA);
	add_constant(d, "PU_SPELLS", PU_SPELLS);
	add_constant(d, "PU_FORGET_VIEW", PU_FORGET_VIEW);
	add_constant(d, "PU_UPDATE_VIEW", PU_UPDATE_VIEW);
	add_constant(d, "PU_FORGET_FLOW", PU_FORGET_FLOW);
	add_constant(d, "PU_UPDATE_FLOW", PU_UPDATE_FLOW);
	add_constant(d, "PU_MONSTERS", PU_MONSTERS);
	add_constant(d, "PU_DISTANCE", PU_DISTANCE);
	add_constant(d, "PR_MISC", PR_MISC);
	add_constant(d, "PR_TITLE", PR_TITLE);
	add_constant(d, "PR_LEV", PR_LEV);
	add_constant(d, "PR_EXP", PR_EXP);
	add_constant(d, "PR_STATS", PR_STATS);
	add_constant(d, "PR_ARMOR", PR_ARMOR);
	add_constant(d, "PR_HP", PR_HP);
	add_constant(d, "PR_MANA", PR_MANA);
	add_constant(d, "PR_GOLD", PR_GOLD);
	add_constant(d, "PR_DEPTH", PR_DEPTH);
	add_constant(d, "PR_HEALTH", PR_HEALTH);
	add_constant(d, "PR_CUT", PR_CUT);
	add_constant(d, "PR_STUN", PR_STUN);
	add_constant(d, "PR_HUNGER", PR_HUNGER);
	add_constant(d, "PR_BLIND", PR_BLIND);
	add_constant(d, "PR_CONFUSED", PR_CONFUSED);
	add_constant(d, "PR_AFRAID", PR_AFRAID);
	add_constant(d, "PR_POISONED", PR_POISONED);
	add_constant(d, "PR_STATE", PR_STATE);
	add_constant(d, "PR_SPEED", PR_SPEED);
	add_constant(d, "PR_STUDY", PR_STUDY);
	add_constant(d, "PR_EXTRA", PR_EXTRA);
	add_constant(d, "PR_BASIC", PR_BASIC);
	add_constant(d, "PR_MAP", PR_MAP);
	add_constant(d, "PW_INVEN", PW_INVEN);
	add_constant(d, "PW_EQUIP", PW_EQUIP);
	add_constant(d, "PW_PLAYER_0", PW_PLAYER_0);
	add_constant(d, "PW_PLAYER_1", PW_PLAYER_1);
	add_constant(d, "PW_MESSAGE", PW_MESSAGE);
	add_constant(d, "PW_OVERHEAD", PW_OVERHEAD);
	add_constant(d, "PW_MONSTER", PW_MONSTER);
	add_constant(d, "PW_OBJECT", PW_OBJECT);
	add_constant(d, "PW_SNAPSHOT", PW_SNAPSHOT);
	add_constant(d, "PW_BORG_1", PW_BORG_1);
	add_constant(d, "PW_BORG_2", PW_BORG_2);
}
