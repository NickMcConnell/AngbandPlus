/* File: interp.c */

/* Purpose: general script commands */

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
#include "struct-dll.h"
#include "util-dll.h"
#include "icon.h"

extern int objcmd_fontdesc(ClientData clientData, Tcl_Interp *interp,
	int objc, Tcl_Obj *CONST objv[]);

#if TK_MINOR_VERSION >= 2 && TK_MINOR_VERSION < 5
extern int objcmd_menuentrystate(ClientData clientData, Tcl_Interp *interp,
	int objc, Tcl_Obj *CONST objv[]);
#endif /* Tk 8.2-8.4 */
#if TK_MINOR_VERSION >= 2
extern int objcmd_photo_get(ClientData clientData, Tcl_Interp *interp,
	int objc, Tcl_Obj *CONST objv[]);
extern int objcmd_photo_mask(ClientData clientData, Tcl_Interp *interp,
	int objc, Tcl_Obj *CONST objv[]);
#endif /* Tk 8.2+ */
#if TK_MINOR_VERSION == 2
extern int objcmd_photo_copy(ClientData clientData, Tcl_Interp *interp,
	int objc, Tcl_Obj *CONST objv[]);
#endif /* Tk 8.2 */

/* d_attr characters */
char *g_attr_str = "dwsorgbuDWvyRGBU";

/* Last known target state */
int target_state = 0;

/* Visibility of target */
bool target_vis = FALSE;

#if defined(OANGBANDTK) || defined(ZANGBANDTK)
/* TRUE if current command is repeated */
bool command_repeating = FALSE;
#endif /* */

/* Set by scripts when running with the mouse so we can disturb if needed. */
bool mouse_running = FALSE;

/* Dimensions of the generated cave */
int g_cave_hgt = 0, g_cave_wid = 0;

/* NULL-terminated lists */
cptr *keyword_gender;
cptr *keyword_race;
cptr *keyword_class;
#if defined(OANGBANDTK)
cptr keyword_shape[] = {"Normal", "Mouse", "Ferret", "Hound", "Gazelle",
	"Lion", "Ent", "Bat", "Werewolf", "Vampire", "Wyrm", NULL};
#endif /* */

ANG_STORAGE_CLASS int inkey_flags = 0;
int inkey_book;
cptr inkey_options = NULL;
int exit_skip_save = FALSE;

/* Macro used to initialize the g_tval array below */
#define TVAL_DESC(tval) {#tval, tval}

/*
 * The g_tval array associates a textual string for each TV_XXX constant
 * with the integer value of the constant. It is used to initialize the
 * g_hash_tval hash table.
 */
t_tval g_tval[] = {
	{"TV_NONE", 0},
	TVAL_DESC(TV_SKELETON),
	TVAL_DESC(TV_BOTTLE),
	TVAL_DESC(TV_JUNK),
	TVAL_DESC(TV_SPIKE),
	TVAL_DESC(TV_CHEST),
#if defined(ZANGBANDTK)
	TVAL_DESC(TV_FIGURINE),
	TVAL_DESC(TV_STATUE),
	TVAL_DESC(TV_CORPSE),
#endif
	TVAL_DESC(TV_SHOT),
	TVAL_DESC(TV_ARROW),
	TVAL_DESC(TV_BOLT),
	TVAL_DESC(TV_BOW),
	TVAL_DESC(TV_DIGGING),
	TVAL_DESC(TV_HAFTED),
	TVAL_DESC(TV_POLEARM),
	TVAL_DESC(TV_SWORD),
	TVAL_DESC(TV_BOOTS),
	TVAL_DESC(TV_GLOVES),
	TVAL_DESC(TV_HELM),
	TVAL_DESC(TV_CROWN),
	TVAL_DESC(TV_SHIELD),
	TVAL_DESC(TV_CLOAK),
	TVAL_DESC(TV_SOFT_ARMOR),
	TVAL_DESC(TV_HARD_ARMOR),
	TVAL_DESC(TV_DRAG_ARMOR),
	TVAL_DESC(TV_LITE),
	TVAL_DESC(TV_AMULET),
	TVAL_DESC(TV_RING),
	TVAL_DESC(TV_STAFF),
	TVAL_DESC(TV_WAND),
	TVAL_DESC(TV_ROD),
	TVAL_DESC(TV_SCROLL),
	TVAL_DESC(TV_POTION),
	TVAL_DESC(TV_FLASK),
	TVAL_DESC(TV_FOOD),
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)
	TVAL_DESC(TV_MAGIC_BOOK),
	TVAL_DESC(TV_PRAYER_BOOK),
#endif /* ANGBANDTK, KANGBANDTK, OANGBANDTK */
#if defined(KANGBANDTK)
	TVAL_DESC(TV_ILLUSION_BOOK),
	TVAL_DESC(TV_NATURE_BOOK),
#endif /* KANGBANDTK*/
#if defined(OANGBANDTK)
	TVAL_DESC(TV_DRUID_BOOK),
	TVAL_DESC(TV_NECRO_BOOK),
#endif /* OANGBANDTK*/
#if defined(ZANGBANDTK)
	TVAL_DESC(TV_LIFE_BOOK),
	TVAL_DESC(TV_SORCERY_BOOK),
	TVAL_DESC(TV_NATURE_BOOK),
	TVAL_DESC(TV_CHAOS_BOOK),
	TVAL_DESC(TV_DEATH_BOOK),
	TVAL_DESC(TV_TRUMP_BOOK),
	TVAL_DESC(TV_ARCANE_BOOK),
#endif /* ZANGBANDTK */
	TVAL_DESC(TV_GOLD),
	{NULL, 0}
};

/*
 * The g_tval_str hash table is used to look up the integer value 
 * of a TV_XXX constant given the textual string.
 */
Tcl_HashTable *g_tval_str = NULL;

/*
 * The g_tval_const hash table is used to look up the textual string 
 * of a TV_XXX constant given the integer value. You could use a 101-element
 * array, but there are a lot of holes in TV_XXX constants.
 */
Tcl_HashTable *g_tval_const = NULL;

/*
 * Initialize the g_tval_by_XXX hash tables
 */
static void init_tval_hash(void)
{
	int i, new;
	Tcl_HashEntry *hPtr;

	/* Allocate the g_tval_str hash table */
	g_tval_str = (Tcl_HashTable *) Tcl_Alloc(sizeof(Tcl_HashTable));

	/* Initialize the g_tval_str hash table. */
	Tcl_InitHashTable(g_tval_str, TCL_STRING_KEYS);

	/* Allocate the g_tval_const hash table */
	g_tval_const = (Tcl_HashTable *) Tcl_Alloc(sizeof(Tcl_HashTable));

	/* Initialize the g_tval_const hash table. */
	Tcl_InitHashTable(g_tval_const, TCL_ONE_WORD_KEYS);

	/* Iterate over each key/value */
	for (i = 0; g_tval[i].key; i++)
	{
		/* Create a new hash table entry with key = TV_XXX string */
		hPtr = Tcl_CreateHashEntry(g_tval_str, g_tval[i].key, &new);

		/* Set the hash table entry with value = TV_XXX integer value */
		Tcl_SetHashValue(hPtr, g_tval[i].value);

		/* Create a new hash table entry with key = TV_XXX constant */
		hPtr = Tcl_CreateHashEntry(g_tval_const,
			(char *) g_tval[i].value, &new);

		/* Set the hash table entry with value = TV_XXX integer value */
		Tcl_SetHashValue(hPtr, g_tval[i].key);
	}
}

/*
 * Convert a TV_XXX string into the corresponding integer value.
 * Return an error if the tval is unknown.
 */
int angtk_tval_const(int *tval, cptr str)
{
	Tcl_HashEntry *hPtr;

	if ((hPtr = Tcl_FindHashEntry(g_tval_str, str)) == NULL)
	{
		/* Set the error */
		FormatResult(g_interp, "unknown tval \"%s\"", str);

		/* Failure */
		return TCL_ERROR;
	}

	/* Return the integer value of the TV_XXX constant */
	(*tval) = (int) Tcl_GetHashValue(hPtr);

	/* Success */
	return TCL_OK;
}

/*
 * Convert a TV_XXX constant into the corresponding string.
 * Return an error if the tval is unknown.
 */
int angtk_tval_string(cptr *str, int tval)
{
	Tcl_HashEntry *hPtr;

	/* Look up the hash table entry using the tval as a key */
	if ((hPtr = Tcl_FindHashEntry(g_tval_const, (char *) tval)) == NULL)
	{
		/* Set the error */
		FormatResult(g_interp, "unknown tval \"%d\"", tval);

		/* Failure */
		return TCL_ERROR;
	}

	/* Return the textual name for this TV_XXX constant */
	(*str) = (cptr) Tcl_GetHashValue(hPtr);

	/* Success */
	return TCL_OK;
}

#if defined(KANGBANDTK) || defined(ZANGBANDTK)

/*
 * Return a Tcl list of m_list[] indexes of pets
 */
static Tcl_Obj *DumpPets(void)
{
	int i;
	Tcl_Obj *listObjPtr;

	/* Create a new Tcl list object */
	listObjPtr = Tcl_NewListObj(0, NULL);

	/* Process the monsters */
	for (i = 1; i < m_max; i++)
	{
		/* Access the monster */
		monster_type *m_ptr = &m_list[i];

		/* Ignore "dead" monsters */
		if (!m_ptr->r_idx) continue;

		/* Append m_list[] index of friendly monster */
#if defined(KANGBANDTK)
		if (m_ptr->is_friendly)
#endif
#if defined(ZANGBANDTK)
		if (is_pet(m_ptr))
#endif
		{
			Tcl_ListObjAppendElement(g_interp, listObjPtr,
				Tcl_NewIntObj(i));
		}
	}

	return listObjPtr;
}

#endif /* KANGBANDTK, ZANGBANDTK */

#if defined(ZANGBANDTK)

/*
 * Return a Tcl list of descriptions of the character's mutations
 */
static Tcl_Obj *DumpMutations(void)
{
	int n, i = 0, j;
	Tcl_Obj *listObjPtr;
	cptr info[128];

	/* Create a new Tcl list object */
	listObjPtr = Tcl_NewListObj(0, NULL);

	/* Check muta1 flags */
	for (j = 0; j < 32; j++)
	{
		if (p_ptr->muta1 & (1L << j))
		{
			info[i++] = mut1_desc[j];
		}
	}

	/* Check muta2 flags */
	for (j = 0; j < 32; j++)
	{
		if (p_ptr->muta2 & (1L << j))
		{
			info[i++] = mut2_desc[j];
		}
	}

	/* Check muta3 flags */
	for (j = 0; j < 32; j++)
	{
		if (p_ptr->muta3 & (1L << j))
		{
			info[i++] = mut3_desc[j];
		}
	}

	/* Append each description to the list */
	for (n = i, i = 0; i < n; i++)
	{
		Tcl_ListObjAppendElement(g_interp, listObjPtr,
			ExtToUtf_NewStringObj((char *) info[i], -1));
	}

	return listObjPtr;
}

/*
 * Return a Tcl list of descriptions of the character's virtues
 */
static Tcl_Obj *DumpVirtues(void)
{
	char *vd;
	int v_nr;
	Tcl_Obj *listObjPtr;

	/* Create a new Tcl list object */
	listObjPtr = Tcl_NewListObj(0, NULL);

	for (v_nr = 0; v_nr < MAX_PLAYER_VIRTUES; v_nr++)
	{
		char v_name[20];

		int tester = p_ptr->virtues[v_nr];

		strcpy(v_name, virtue[p_ptr->vir_types[v_nr] - 1]);

		if ((p_ptr->vir_types[v_nr] == 0) ||
			(p_ptr->vir_types[v_nr] > MAX_VIRTUE))
			vd = format("Oops. No info about %s.", v_name);
		else if (tester < -100)
			vd = format("You are the polar opposite of %s.", v_name);
		else if (tester < -80)
			vd = format("You are an arch-enemy of %s.", v_name);
		else if (tester < -60)
			vd = format("You are a bitter enemy of %s.", v_name);
		else if (tester < -40)
			vd = format("You are an enemy of %s.", v_name);
		else if (tester < -20)
			vd = format("You have sinned against %s.", v_name);
		else if (tester < 0)
			vd = format("You have strayed from the path of %s.", v_name);
		else if (tester == 0)
			vd = format("You are neutral to %s.", v_name);
		else if (tester < 20)
			vd = format("You are somewhat virtuous in %s.", v_name);
		else if (tester < 40)
			vd = format("You are virtuous in %s.", v_name);
		else if (tester < 60)
			vd = format("You are very virtuous in %s.", v_name);
		else if (tester < 80)
			vd = format("You are a champion of %s.", v_name);
		else if (tester < 100)
			vd = format("You are a great champion of %s.", v_name);
		else
			vd = format("You are the living embodiment of %s.", v_name);

		Tcl_ListObjAppendElement(g_interp, listObjPtr,
			Tcl_NewStringObj(vd, -1));
	}

	if (wizard)
	{
		vd = format("Your overall alignment is %ld.", p_ptr->align);
		Tcl_ListObjAppendElement(g_interp, listObjPtr,
			Tcl_NewStringObj(vd, -1));
	}

	return listObjPtr;
}

#endif /* ZANGBANDTK */

/*
 * Dump object info into an array variable. Array names are:
 *		char, number, name, inscription, weight, tval, sval, icon
 * Extra name "label" for equipment...
 */
int dump_object_info(char *varName, object_type *o_ptr, int index)
{
	char buf[128], o_name[O_NAME_MAX];
	cptr str;
	u32b f1, f2, f3;

	/* Description */
	object_desc(o_name, o_ptr, TRUE, 3);
	if (ExtToUtf_SetArrayValueString(varName, "name", o_name) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* Inscription */
	if (get_user_inscription(o_ptr))
	{
		strcpy(buf, quark_str(get_user_inscription(o_ptr)));
	}
	else
	{
		buf[0] = '\0';
	}
	if (ExtToUtf_SetArrayValueString(varName, "inscription", buf) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* Char */
	if (SetArrayValueChar(varName, "char", index_to_label(index)) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* Number */
	if (SetArrayValueLong(varName, "number", o_ptr->number) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* Weight */
	if (SetArrayValueLong(varName, "weight", o_ptr->weight) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* Tval */
	(void) angtk_tval_string(&str, o_ptr->tval);
	if (SetArrayValueString(varName, "tval", (char *) str) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* Sval */
	if (SetArrayValueLong(varName, "sval", o_ptr->sval) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* K_idx */
	if (SetArrayValueLong(varName, "k_idx", o_ptr->k_idx) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* Icon */
	if (SetArrayValueString(varName, "icon",
		assign_print_object(buf, o_ptr)) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* Label in equipment */
	if (index >= INVEN_WIELD)
	{
		if (ExtToUtf_SetArrayValueString(varName, "label", (char *) mention_use(index))
			!= TCL_OK)
		{
			return TCL_ERROR;
		}
	}

	/* Known object */
	if (object_known_p(o_ptr))
	{
		/* Known */
		if (SetArrayValueLong(varName, "known", 1) != TCL_OK)
		{
			return TCL_ERROR;
		}

		/* Get the object flags */
		object_flags(o_ptr, &f1, &f2, &f3);

		/* Activate */
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(ZANGBANDTK)
		if (SetArrayValueLong(varName, "activate",
			(f3 & (TR3_ACTIVATE)) != 0) != TCL_OK)
#endif /* */
#if defined(OANGBANDTK)
		if (SetArrayValueLong(varName, "activate",
			o_ptr->xtra1 == OBJECT_XTRA_TYPE_ACTIVATION) != TCL_OK)
#endif /* */
		{
			return TCL_ERROR;
		}

		if (SetArrayValueLong(varName, "timeout", o_ptr->timeout) != TCL_OK)
		{
			return TCL_ERROR;
		}
	}

	/* Unknown object */
	else
	{
		/* Known */
		if (SetArrayValueLong(varName, "known", 0) != TCL_OK)
		{
			return TCL_ERROR;
		}
	}

	/* Success */
	return TCL_OK;
}

/* String names for values returned by angtk_find_artifact() */
cptr keyword_artifact_location[] = {
	"inventory", "equipment", "dungeon", "home", "unknown", NULL
};

/*
 * Return a constant representing the current "location" of the
 * given artifact. We look in the inventory and equipment, in the
 * cave, and in each of the Homes. If the artifact is not found
 * in any of these locations, we create a fully-known copy.
 * In any case, a copy of the object is returned to the caller.
 */
int angtk_find_artifact(int a_idx, object_type *o_ptr)
{
	int i;
	object_type *j_ptr;

#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)

	/* Access the Home */
	store_type *st_ptr = &store[STORE_HOME];

#endif /* ANGBANDTK, KANGBANDTK, OANGBANDTK */

	/* Check inventory, equipment */
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		/* Access the object */
		j_ptr = &inventory[i];

		/* This is the artifact */
		if (j_ptr->k_idx && (j_ptr->name1 == a_idx))
		{
			/* Return a copy of the object */
			*o_ptr = *j_ptr;

			/* Return 0 for inventory, 1 for equipment */
			return (i < INVEN_PACK) ? 0 : 1;
		}
	}

	/* Check cave */
	for (i = 1; i < o_max; i++)
	{
		/* Access the object */
		j_ptr = &o_list[i];

		/* This is the artifact */
		if (j_ptr->k_idx && (j_ptr->name1 == a_idx))
		{
			/* Return a copy of the object */
			*o_ptr = *j_ptr;

			return 2;
		}
	}

#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)

	/* Check Home */
	for (i = 0; i < st_ptr->stock_num; i++)
	{
		j_ptr = &st_ptr->stock[i];
		if (j_ptr->k_idx && (j_ptr->name1 == a_idx))
		{
			*o_ptr = *j_ptr;
			return 3;
		}
	}

#endif /* ANGBANDTK, KANGBANDTK, OANGBANDTK */

#if defined(ZANGBANDTK)

	/* Check Home(s) */
	for (i = 1; i < max_towns; i++)
	{
		/* Access the home */
		store_type *st_ptr = &town[i].store[STORE_HOME];

		int j;

		/* Check each stock item */
		for (j = 0; j < st_ptr->stock_num; j++)
		{
			/* Access the object */
			j_ptr = &st_ptr->stock[j];

			/* This is the artifact */
			if (j_ptr->k_idx && (j_ptr->name1 == a_idx))
			{
				/* Return a copy of the object */
				*o_ptr = *j_ptr;

				return 3;
			}
		}
	}

#endif /* ZANGBANDTK */

	/*
	 * Make a fake one. We're assuming that the artifact was created
	 * but is now lost. So we create a fully-known copy.
	 */
	object_wipe(o_ptr);
	if (!make_fake_artifact(o_ptr, a_idx))
	{
		/* Set the error */
		StaticResult(g_interp, "make_fake_artifact failed");

		/* Failure */
		return -1;
	}
	object_aware(o_ptr);
	object_known(o_ptr);
	o_ptr->ident |= (IDENT_MENTAL);
	return 4;
}

static int s_status_value;

/*
 * Prints status of hunger
 */
static char *state_hunger(void)
{
	/* Fainting / Starving */
	if (p_ptr->food < PY_FOOD_FAINT)
	{
		return "Weak";
	}

	/* Weak */
	else if (p_ptr->food < PY_FOOD_WEAK)
	{
		return "Weak";
	}

	/* Hungry */
	else if (p_ptr->food < PY_FOOD_ALERT)
	{
		return "Hungry";
	}

	/* Normal */
	else if (p_ptr->food < PY_FOOD_FULL)
	{
		return "";
	}

	/* Full */
	else if (p_ptr->food < PY_FOOD_MAX)
	{
		return "Full";
	}

	/* Gorged */
	else
	{
		return "Gorged";
	}
}

/*
 * Prints Blind status
 */
static char *state_blind(void)
{
	if (p_ptr->blind)
	{
		return "Blind";
	}
	else
	{
		return "";
	}
}

/*
 * Prints Confusion status
 */
static char *state_confused(void)
{
	if (p_ptr->confused)
	{
		return "Confused";
	}
	else
	{
		return "";
	}
}

/*
 * Prints Fear status
 */
static char *state_afraid(void)
{
	if (p_ptr->afraid)
	{
		return "Afraid";
	}
	else
	{
		return "";
	}
}

/*
 * Prints Poisoned status
 */
static char *state_poisoned(void)
{
	if (p_ptr->poisoned)
	{
		return "Poisoned";
	}
	else
	{
		return "";
	}
}

static int trunc_num(int n)
{
	/* Only 4 digits are allowed */
	if (n > 9999) n = 9999;

	/* Extensive */
	if (n >= 1000)
	{
		return (n / 100) * 100;
	}

	/* Long */
	else if (n >= 100)
	{
		return (n / 10) * 10;
	}

	/* Medium */
	else if (n >= 10)
	{
		return (n / 5) * 5;
	}

	/* Short */
	return n;
}

/*
 * Prints Searching, Resting, Paralysis, or 'count' status
 * Display is always exactly 10 characters wide (see below)
 *
 * This function was a major bottleneck when resting, so a lot of
 * the text formatting code was optimized in place below.
 */
static char *state_state(void)
{
	/* Paralysis */
	if (p_ptr->paralyzed)
	{
		return "Paralyzed!";
	}

	/* Resting */
	else if (p_ptr_resting)
	{
		int n = p_ptr_resting;

		/* Rest until healed */
		if (n == -1)
		{
			return "Rest *****";
		}

		/* Rest until done */
		else if (n == -2)
		{
			return "Rest &&&&&";
		}
		else
		{
			s_status_value = trunc_num(n);
			return "Rest %d";
		}
	}

	/* Repeating */
	else if (p_ptr_command_rep)
	{
		int n = p_ptr_command_rep;

		s_status_value = trunc_num(n);

		if (n > 999)
		{
			return "Rep. %d";
		}
		else
		{
			return "Repeat %d";
		}
	}

	/* Searching */
	else if (p_ptr->searching)
	{
		return "Searching";
	}

	/* Nothing interesting */
	else
	{
		return "";
	}
}

#if defined(OANGBANDTK)

/*
 * Prints current shape, if not normal.  -LM-
 */
static char *state_shape(void)
{
	char *shapedesc = "";

	if (SCHANGE)
		shapedesc = (char *) keyword_shape[p_ptr->schange];

	return shapedesc;
}

#endif /* OANGBANDTK */

/*
 * Prints the speed of a character.			-CJS-
 */
static char *state_speed(void)
{
	int n = p_ptr->pspeed;

	/* Hack -- Visually "undo" the Search Mode Slowdown */
	if (p_ptr->searching) n += 10;

	/* Fast */
	if (n > 110)
	{
		s_status_value = n - 110;
		return "Fast (%+d)";
	}

	/* Slow */
	else if (n < 110)
	{
		s_status_value = -(110 - n);
		return "Slow (%+d)";
	}

	/* Normal */
	return "";
}

static char *state_study(void)
{
	if (p_ptr->new_spells)
	{
		return "Study";
	}
	else
	{
		return "";
	}
}

static char *state_cut(void)
{
	int c = p_ptr->cut;

	if (c > 1000)
	{
		return "Mortal wound";
	}
	else if (c > 200)
	{
		return "Deep gash";
	}
	else if (c > 100)
	{
		return "Severe cut";
	}
	else if (c > 50)
	{
		return "Nasty cut";
	}
	else if (c > 25)
	{
		return "Bad cut";
	}
	else if (c > 10)
	{
		return "Light cut";
	}
	else if (c)
	{
		return "Graze";
	}
	else
	{
		return "";
	}
}

static char *state_stun(void)
{
	int s = p_ptr->stun;

	if (s > 100)
	{
		return "Knocked out";
	}
	else if (s > 50)
	{
		return "Heavy stun";
	}
	else if (s)
	{
		return "Stun";
	}
	else
	{
		return "";
	}
}

static char *state_winner(void)
{
	/* Wizard */
	if (p_ptr_wizard)
	{
		return "Wizard";
	}

	/* Winner */
	else if (p_ptr_total_winner || (p_ptr->lev > PY_MAX_LEVEL))
	{
		return "Winner";
	}

	/* Normal */
	else
	{
		return "";
	}
}

#ifdef ALLOW_STATUS_EXTRA

static char *state_extra(int pr)
{
	bool state = FALSE;

	static cptr status_extra_text[] = {
		"Blessed", "Hero", "Berserk", "Res Acid", "Res Cold", "Res Elec",
		"Res Fire", "Res Poison", "Prot Evil", "Shield",
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(ZANGBANDTK)
		"Invulnerable",
#endif /* */
		"Faster", "Slower", "Infravision", "See Invis", "Recall", "Hallucinate",
#if defined(KANGBANDTK)
		"T/S Anchor", "Ghostly", "Invisible", "Res Cha/Con", "Res Lig/Dar",
		"Res Sou/Sha", "Res Nether", "Res Nexus", "Levitate",
		"Sus Str", "Sus Int", "Sus Wis", "Sus Dex", "Sus Con", "Sus Chr",
#endif /* KANGBANDTK */
#if defined(OANGBANDTK)
		"Ele Attack", "ESP", "Res Magic", "Stealth",
#endif /* OANGBANDTK */
#if defined(ZANGBANDTK)
		"ESP", "Wraith",
#endif /* ZANGBANDTK */
		NULL
	};

	switch (pr)
	{
		case PR_BLESSED:
			if (p_ptr->blessed) state = TRUE;
			break;
		case PR_HERO:
			if (p_ptr->hero) state = TRUE;
			break;
		case PR_SHERO:
			if (p_ptr->shero) state = TRUE;
			break;
		case PR_OPPOSE_ACID:
			if (p_ptr->oppose_acid) state = TRUE;
			break;
		case PR_OPPOSE_COLD:
			if (p_ptr->oppose_cold) state = TRUE;
			break;
		case PR_OPPOSE_ELEC:
			if (p_ptr->oppose_elec) state = TRUE;
			break;
		case PR_OPPOSE_FIRE:
			if (p_ptr->oppose_fire) state = TRUE;
			break;
		case PR_OPPOSE_POIS:
			if (p_ptr->oppose_pois) state = TRUE;
			break;
		case PR_PROTEVIL:
			if (p_ptr->protevil) state = TRUE;
			break;
		case PR_SHIELD:
			if (p_ptr->shield) state = TRUE;
			break;
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(ZANGBANDTK)
		case PR_INVULN:
			if (p_ptr->invuln) state = TRUE;
			break;
#endif /* */
		case PR_FAST:
			if (p_ptr->fast) state = TRUE;
			break;
		case PR_SLOW:
			if (p_ptr->slow) state = TRUE;
			break;
		case PR_TIM_INFRA:
			if (p_ptr->tim_infra) state = TRUE;
			break;
		case PR_SEE_INVIS:
			if (p_ptr->tim_invis) state = TRUE;
			break;
		case PR_RECALL:
			if (p_ptr->word_recall) state = TRUE;
			break;
		case PR_IMAGE:
			if (p_ptr->image) state = TRUE;
			break;
#if defined(KANGBANDTK)
		case PR_ANCHOR:
			if (p_ptr->ts_anchor) state = TRUE;
			break;
		case PR_GHOST:
			if (p_ptr->ghostly) state = TRUE;
			break;
		case PR_INVIS:
			if (p_ptr->tim_pl_invis) state = TRUE;
			break;
		case PR_OPPOSE_CC:
			if (p_ptr->oppose_cc) state = TRUE;
			break;
		case PR_OPPOSE_LD:
			if (p_ptr->oppose_ld) state = TRUE;
			break;
		case PR_OPPOSE_SS:
			if (p_ptr->oppose_ss) state = TRUE;
			break;
		case PR_OPPOSE_NETH:
			if (p_ptr->oppose_nethr) state = TRUE;
			break;
		case PR_OPPOSE_NEX:
			if (p_ptr->oppose_nexus) state = TRUE;
			break;
		case PR_LEVITATE:
			if (p_ptr->tim_levitate) state = TRUE;
			break;
		case PR_SUS_STR:
			if (p_ptr->tim_sus_str) state = TRUE;
			break;
		case PR_SUS_INT:
			if (p_ptr->tim_sus_int) state = TRUE;
			break;
		case PR_SUS_WIS:
			if (p_ptr->tim_sus_wis) state = TRUE;
			break;
		case PR_SUS_DEX:
			if (p_ptr->tim_sus_dex) state = TRUE;
			break;
		case PR_SUS_CON:
			if (p_ptr->tim_sus_con) state = TRUE;
			break;
		case PR_SUS_CHR:
			if (p_ptr->tim_sus_chr) state = TRUE;
			break;
#endif /* KANGBANDTK */
#if defined(OANGBANDTK)
		case PR_ELE_ATTACK:
			if (p_ptr->ele_attack)
			{
				if (p_ptr->special_attack & ATTACK_ACID)
					return "Att Acid";
				if (p_ptr->special_attack & ATTACK_ELEC)
					return "Att Elec";
				if (p_ptr->special_attack & ATTACK_FIRE)
					return "Att Fire";
				if (p_ptr->special_attack & ATTACK_COLD)
					return "Att Cold";
				if (p_ptr->special_attack & ATTACK_POIS)
					return "Att Poison";
			}
			break;
		case PR_ESP:
			if (p_ptr->tim_esp) state = TRUE;
			break;
		case PR_MAGICDEF:
			if (p_ptr->magicdef) state = TRUE;
			break;
		case PR_STEALTH:
			if (p_ptr->superstealth) state = TRUE;
			break;
#endif /* OANGBANDTK */
#if defined(ZANGBANDTK)
		case PR_TIM_ESP:
			if (p_ptr->tim_esp) state = TRUE;
			break;
		case PR_WRAITH:
			if (p_ptr->wraith_form) state = TRUE;
			break;
#endif /* ZANGBANDTK */
	}

	if (state)
		return (char *) status_extra_text[pr];

	return "";
}

#endif /* ALLOW_STATUS_EXTRA */

char *player_status(int status, int *value)
{
	char *format;

	typedef char *(status_proc)(void);
	static status_proc *status_info[] = {
		state_cut,
		state_stun,
		state_hunger,
		state_blind,
		state_confused,
		state_afraid,
		state_poisoned,
		state_state,
		state_speed,
		state_study,
		state_winner
#if defined(OANGBANDTK)
		, state_shape
#endif /* */
	};

	s_status_value = 0;

#ifdef ALLOW_STATUS_EXTRA
	if (status >= KEYWORD_STATUS_EXTRA)
    {
		format = state_extra(status - KEYWORD_STATUS_EXTRA);
	}
    else
#endif /* ALLOW_STATUS_EXTRA */

	format = (*status_info[status])();
	(*value) = s_status_value;
	return format;
}

#if defined(ZANGBANDTK)

void blows_per_round(int *_blows, int *_muta_att)
{
	int muta_att = 0;

	if (p_ptr->muta2 & MUT2_HORNS)
		muta_att++;
	if (p_ptr->muta2 & MUT2_SCOR_TAIL)
		muta_att++;
	if (p_ptr->muta2 & MUT2_BEAK)
		muta_att++;
	if (p_ptr->muta2 & MUT2_TRUNK)
		muta_att++;
	if (p_ptr->muta2 & MUT2_TENTACLES)
		muta_att++;

	(*_blows) = p_ptr->num_blow;
	(*_muta_att) = muta_att;
}

void shots_per_round(int *_shots, int *_shots_frac)
{
	int energy_fire = 100;
	int shots, shots_frac;
	object_type *o_ptr = &inventory[INVEN_BOW];

	if (o_ptr->k_idx)
	{
		switch (o_ptr->sval)
		{
			case SV_SLING:
			{
				energy_fire = 50;
				break;
			}
			case SV_SHORT_BOW:
			{
				energy_fire = 100;
				break;
			}
			case SV_LONG_BOW:
			{
				energy_fire = 100;
				break;
			}
			case SV_LIGHT_XBOW:
			{
				energy_fire = 120;
				break;
			}
			case SV_HEAVY_XBOW:
			{
				if (p_ptr->stat_use[A_DEX] >= 16)
				{
					energy_fire = 150;
				}
				else
				{
					/* players with low dex will take longer to load */
					energy_fire = 200;
				}
			}
			break;
		}
	}

	shots = p_ptr->num_fire * 100;
	shots_frac = (shots * 100 / energy_fire) % 100;
	shots = shots / energy_fire;

	(*_shots) = shots;
	(*_shots_frac) = shots_frac;
}

#endif /* ZANGBANDTK */

/* should agree with calc_mana */
static void calc_mana_cumber(int *pGlove, int *pArmor)
{
#if defined(ANGBANDTK) || defined(KANGBANDTK)
	int msp, levels, cur_wgt, max_wgt;
	int penalty;

	*pGlove = *pArmor = 0;

	/* Hack -- Must be literate */
	if (!mp_ptr->spell_book)
		return;

	/* Extract "effective" player level */
	levels = (p_ptr->lev - mp_ptr->spell_first) + 1;

	/* Hack -- no negative mana */
	if (levels < 0)
		levels = 0;

	/* Extract total mana */
	msp = adj_mag_mana[p_ptr->stat_ind[mp_ptr->spell_stat]] * levels / 2;

	/* Hack -- usually add one mana */
	if (msp)
		msp++;

	/* Only mages are affected */
	if (p_ptr->cumber_glove)
	{
		/* Reduce mana */
		penalty = msp - (3 * msp) / 4;
		msp -= penalty;
		*pGlove = penalty;
	}

	/* Weigh the armor */
	cur_wgt = 0;
	cur_wgt += inventory[INVEN_BODY].weight;
	cur_wgt += inventory[INVEN_HEAD].weight;
	cur_wgt += inventory[INVEN_ARM].weight;
	cur_wgt += inventory[INVEN_OUTER].weight;
	cur_wgt += inventory[INVEN_HANDS].weight;
	cur_wgt += inventory[INVEN_FEET].weight;

	/* Determine the weight allowance */
	max_wgt = mp_ptr->spell_weight;

	/* Heavy armor penalizes mana */
	if (p_ptr->cumber_armor)
	{
		/* Reduce mana */
		penalty = ((cur_wgt - max_wgt) / 10);
		*pArmor = penalty;
	}
#endif /* ANGBANDTK KANGBANDTK */

#ifdef OANGBANDTK
	int msp, levels, cur_wgt, max_wgt;
	int penalty;

	*pGlove = *pArmor = 0;

	/* Hack -- Must possess some magical realm. */
	if (!mp_ptr->spell_realm)
		return;

	/* Extract "effective" player level */
	levels = (p_ptr->lev - mp_ptr->spell_first) + 1;

	/* Hack -- no negative mana */
	if (levels < 0)
		levels = 0;

	/* Extract total mana, using standard rounding. */
	msp =
		(adj_mag_mana[p_ptr->stat_ind[mp_ptr->spell_stat]] * levels +
		5) / 10;

	/* The weak spellcasters get half as much mana (rounded up) in Oangband. */
	switch (p_ptr->pclass)
	{
		case CLASS_ROGUE:
		case CLASS_RANGER:
		case CLASS_PALADIN:
		case CLASS_ASSASSIN:
		{
			msp = (msp + 1) / 2;
			break;
		}
		default:
		{
			break;
		}
	}

	/* Hack -- usually add one mana */
	if (msp)
		msp++;

	/* Only mage and Necromancer-type spellcasters are affected by gloves. */
	if (p_ptr->cumber_glove)
	{
		/* Reduce mana */
		penalty = msp - 3 * msp / 4;
		*pGlove = penalty;
	}

	/* Weigh the armor */
	cur_wgt = 0;
	cur_wgt += inventory[INVEN_BODY].weight;
	cur_wgt += inventory[INVEN_HEAD].weight;
	cur_wgt += inventory[INVEN_ARM].weight;
	cur_wgt += inventory[INVEN_OUTER].weight;
	cur_wgt += inventory[INVEN_HANDS].weight;
	cur_wgt += inventory[INVEN_FEET].weight;

	/* Determine the weight allowance */
	max_wgt = mp_ptr->spell_weight;

	/* Heavy armor penalizes mana by a percentage. */
	if (p_ptr->cumber_armor)
	{
		/* Subtract a percentage of maximum mana. */
		switch (p_ptr->pclass)
		{
				/* For these classes, mana is halved if armour 
				 * is 30 pounds over their weight limit. */
			case CLASS_MAGE:
			case CLASS_NECRO:
			case CLASS_DRUID:
			{
				penalty = msp * (cur_wgt - max_wgt) / 600;
				break;
			}

				/* Mana halved if armour is 40 pounds over weight limit. */
			case CLASS_PRIEST:
			{
				penalty = msp * (cur_wgt - max_wgt) / 800;
				break;
			}

				/* Mana halved if armour is 50 pounds over weight limit. */
			case CLASS_ROGUE:
			case CLASS_RANGER:
			case CLASS_ASSASSIN:
			{
				penalty = msp * (cur_wgt - max_wgt) / 1000;
				break;
			}

				/* Mana halved if armour is 60 pounds over weight limit. */
			case CLASS_PALADIN:
			{
				penalty = msp * (cur_wgt - max_wgt) / 1200;
				break;
			}

				/* For new classes created, but not yet added to this formula. */
			default:
			{
				penalty = msp * (cur_wgt - max_wgt) / 800;
				break;
			}
		}

		*pArmor = penalty;
	}
#endif /* OANGBANDTK */

#ifdef ZANGBANDTK
	int msp, levels, cur_wgt, max_wgt;
	int penalty;

	*pGlove = *pArmor = 0;

	/* Hack -- Must be literate */
	if (!mp_ptr->spell_book)
		return;

	if (p_ptr->pclass == CLASS_MINDCRAFTER)
	{
		levels = p_ptr->lev;
	}
	else
	{
		/* Extract "effective" player level */
		levels = (p_ptr->lev - mp_ptr->spell_first) + 1;
	}

	/* Hack -- no negative mana */
	if (levels < 0)
		levels = 0;

	/* Extract total mana */
	msp = adj_mag_mana[p_ptr->stat_ind[mp_ptr->spell_stat]] * levels / 2;

	/* Hack -- usually add one mana */
	if (msp)
		msp++;

	/* Hack: High mages have a 25% mana bonus */
	if (msp && (p_ptr->pclass == CLASS_HIGH_MAGE))
		msp += msp / 4;

	/* Only mages are affected */
	if (p_ptr->cumber_glove)
	{
		/* Reduce mana */
		penalty = msp - (3 * msp) / 4;
		msp -= penalty;
		*pGlove = penalty;
	}

	/* Weigh the armor */
	cur_wgt = 0;
	cur_wgt += inventory[INVEN_BODY].weight;
	cur_wgt += inventory[INVEN_HEAD].weight;
	cur_wgt += inventory[INVEN_ARM].weight;
	cur_wgt += inventory[INVEN_OUTER].weight;
	cur_wgt += inventory[INVEN_HANDS].weight;
	cur_wgt += inventory[INVEN_FEET].weight;

	/* Determine the weight allowance */
	max_wgt = mp_ptr->spell_weight;

	/* Heavy armor penalizes mana by a percentage.  -LM- */
	if (p_ptr->cumber_armor)
	{
		/* Subtract a percentage of maximum mana. */
		switch (p_ptr->pclass)
		{
				/* For these classes, mana is halved if armour
				 * is 30 pounds over their weight limit. */
			case CLASS_MAGE:
			case CLASS_HIGH_MAGE:
			{
				penalty = msp * (cur_wgt - max_wgt) / 600;
				break;
			}

				/* Mana halved if armour is 40 pounds over weight limit. */
			case CLASS_PRIEST:
			case CLASS_MINDCRAFTER:
			{
				penalty = msp * (cur_wgt - max_wgt) / 800;
				break;
			}

				/* Mana halved if armour is 50 pounds over weight limit. */
			case CLASS_ROGUE:
			case CLASS_RANGER:
			case CLASS_MONK:
			{
				penalty = msp * (cur_wgt - max_wgt) / 1000;
				break;
			}

				/* Mana halved if armour is 60 pounds over weight limit. */
			case CLASS_PALADIN:
			case CLASS_CHAOS_WARRIOR:
			case CLASS_WARRIOR_MAGE:
			{
				penalty = msp * (cur_wgt - max_wgt) / 1200;
				break;
			}

				/* For new classes created, but not yet added to this formula. */
			default:
			{
				penalty = msp * (cur_wgt - max_wgt) / 800;
				break;
			}
		}
		*pArmor = penalty;
	}
#endif /* ZANGBANDTK */
}

/*
 *--------------------------------------------------------------
 *
 * objcmd_player --
 *
 *	Implements the "player" script command.
 *
 *--------------------------------------------------------------
 */

int
objcmd_player(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	static CONST char *cmdOptions[] = {"ability", "age", "armor_class",
		"blows_per_round", "icon", "class", "depth", "died_from",
		"exp", "food", "gold", "height", "history", "hitpoints",
		"infravision", "level", "mana", "max_depth", "name", "position",
		"race", "sex", "shots_per_round", "social_class", "spell_book",
		"stat", "status", "title", "to_dam", "to_hit", "weight",
		"total_weight", "maximize", "preserve", "base_name", "flags",
		"is_dead", "turn", "max_level", "disturb", "cheat", "new_spells",
		"command_rep", "running", "prayer_or_spell", "health_who",
		"monster_race_idx", "life_rating", "mana_cumber", "sense",
#if defined(KANGBANDTK)
		"arena_number", "inside_arena", "inside_quest", "exit_bldg",
		"pets",
#endif /* KANGBANDTK */
#if defined(OANGBANDTK) || defined(ZANGBANDTK)
		"deadliness_conversion",
#endif /* KANGBANDTK */
#if defined(ZANGBANDTK)
		"mutations", "pets", "realm1", "realm2", "patron",
		"inside_arena", "inside_quest", "virtues",
#endif /* ZANGBANDTK */
		NULL};
	enum {IDX_ABILITY, IDX_AGE, IDX_ARMOR_CLASS,
		IDX_BLOWS_PER_ROUND, IDX_ICON, IDX_CLASS, IDX_DEPTH, IDX_DIED_FROM,
		IDX_EXP, IDX_FOOD, IDX_GOLD, IDX_HEIGHT, IDX_HISTORY, IDX_HITPOINTS,
		IDX_INFRAVISION, IDX_LEVEL, IDX_MANA, IDX_MAX_DEPTH, IDX_NAME, IDX_POSITION,
		IDX_RACE, IDX_SEX, IDX_SHOTS_PER_ROUND, IDX_SOCIAL_CLASS, IDX_SPELL_BOOK,
		IDX_STAT, IDX_STATUS, IDX_TITLE, IDX_TO_DAM, IDX_TO_HIT, IDX_WEIGHT,
		IDX_TOTAL_WEIGHT, IDX_MAXIMIZE, IDX_PRESERVE, IDX_BASE_NAME, IDX_FLAGS,
		IDX_IS_DEAD, IDX_TURN, IDX_MAX_LEVEL, IDX_DISTURB, IDX_CHEAT, IDX_NEW_SPELLS,
		IDX_COMMAND_REP, IDX_RUNNING, IDX_PRAYER_OR_SPELL, IDX_HEALTH_WHO,
		IDX_MONSTER_RACE_IDX, IDX_LIFE_RATING, IDX_MANA_CUMBER, IDX_SENSE,
#if defined(KANGBANDTK)
		IDX_ARENA_NUMBER, IDX_INSIDE_ARENA, IDX_INSIDE_QUEST, IDX_EXIT_BLDG,
		IDX_PETS
#endif /* KANGBANDTK */
#if defined(OANGBANDTK) || defined(ZANGBANDTK)
		IDX_DEADLINESS_CONVERSION,
#endif /* OANGBANDTK, ZANGBANDTK */
#if defined(ZANGBANDTK)
		IDX_MUTATIONS, IDX_PETS, IDX_REALM1, IDX_REALM2, IDX_PATRON,
		IDX_INSIDE_ARENA, IDX_INSIDE_QUEST, IDX_VIRTUES
#endif /* ZANGBANDTK */
		} option;
	int index;

	cptr cstr;
	object_type *o_ptr;
	int i, tmp;
#if defined(ZANGBANDTK)
	int stat_add[6];
#endif /* ZANGBANDTK */
	long expadv;
	double pct;
	char buf[512], *t, *p;
	Tcl_Obj *listObjPtr;

	static CONST char *abilityOptions[] = {"fighting", "bows_throw", "saving_throw",
		"stealth", "perception", "searching", "disarming", "magic_device",
		NULL};

	static struct {int rating; int max;} ability[] = {
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)
		{0, 12}, /* fighting */
		{0, 12}, /* bows_throw */
#endif
#if defined(OANGBANDTK) || defined(ZANGBANDTK)
		{0, 10}, /* fighting */
		{0, 10}, /* bows_throw */
#endif
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(ZANGBANDTK)
		{0, 6}, /* saving_throw */
#endif
#if defined(OANGBANDTK)
		{0, 7}, /* saving_throw */
#endif
		{0, 1}, /* stealth */
		{0, 6}, /* perception */
		{0, 6}, /* searching */
		{0, 8}, /* disarming */
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(ZANGBANDTK)
		{0, 6} /* magic_device */
#endif
#if defined(OANGBANDTK)
		{0, 8} /* magic_device */
#endif
	};

	/* Required number of arguments */
    if (objC < 2)
    {
		Tcl_WrongNumArgs(interp, infoCmd->depth + 1, objv, "option ?arg ...?");
		return TCL_ERROR;
    }

	/* Get requested option */
    if (Tcl_GetIndexFromObj(interp, objV[1], cmdOptions, "option", 0, 
		(int *) &option) != TCL_OK)
	{
		return TCL_ERROR;
    }

	switch (option)
	{
		case IDX_ABILITY: /* ability */

		    if (objC != 3)
		    {
				Tcl_WrongNumArgs(interp, infoCmd->depth + 2, objv, "ability");
				return TCL_ERROR;
		    }

		    if (Tcl_GetIndexFromObj(interp, objV[2], abilityOptions, "ability", 0, 
				&index) != TCL_OK)
			{
				return TCL_ERROR;
		    }

			/* Fighting Skill (with current weapon) */
			o_ptr = &inventory[INVEN_WIELD];
			tmp = p_ptr->to_h + o_ptr->to_h;
			ability[0].rating = p_ptr->skill_thn + (tmp * BTH_PLUS_ADJ);

			/* Shooting Skill (with current bow and normal missile) */
			o_ptr = &inventory[INVEN_BOW];
			tmp = p_ptr->to_h + o_ptr->to_h;
			ability[1].rating = p_ptr->skill_thb + (tmp * BTH_PLUS_ADJ);

			ability[2].rating = p_ptr->skill_sav;
			ability[3].rating = p_ptr->skill_stl;
			ability[4].rating = p_ptr->skill_fos;
			ability[5].rating = p_ptr->skill_srh;
			ability[6].rating = p_ptr->skill_dis;
			ability[7].rating = p_ptr->skill_dev;
			 
			FormatResult(interp, "%d %d",
				ability[index].rating, ability[index].max);
			break;

		case IDX_AGE: /* age */
			IntResult(interp, p_ptr->age);
			break;

		case IDX_ARMOR_CLASS: /* armor_class */
			FormatResult(interp, "%d %d", p_ptr->dis_ac, p_ptr->dis_to_a);
			break;

		case IDX_BLOWS_PER_ROUND: /* blows_per_round */
		{
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)
			IntResult(interp, p_ptr->num_blow);
#endif
#if defined(ZANGBANDTK)
			int blows, muta_att;
			blows_per_round(&blows, &muta_att);
			FormatResult(interp, muta_att ? "%d+%d" : "%d", blows, muta_att);
#endif
			break;
		}

		case IDX_ICON: /* icon */
		{
			char buf[128];

			index = 0;
#if defined(OANGBANDTK)
			if (SCHANGE)
				index = p_ptr->schange;
#endif /* */
			Tcl_SetResult(interp, assign_print2(buf, ASSIGN_CHARACTER, index),
				TCL_VOLATILE);
			break;
		}

		case IDX_CLASS: /* class */
			StaticResult(interp, (char *) cp_ptr->title);
			break;

		case IDX_DEPTH: /* depth */
			IntResult(interp, p_ptr_depth);
			break; 

		case IDX_DIED_FROM: /* died_from */
			if (!p_ptr_is_dead)
			{
				StaticResult(interp, "character is not dead");
				return TCL_ERROR;
			}
			ExtToUtf_SetResult(interp, p_ptr_died_from);
			break;

		case IDX_EXP: /* exp */
			if (p_ptr->lev >= PY_MAX_LEVEL) expadv = 999999999;
			else expadv = (s32b)(player_exp[p_ptr->lev - 1] * p_ptr->expfact / 100L);
			FormatResult(interp, "%ld %ld %ld", p_ptr->exp,
				p_ptr->max_exp, expadv);
			break;

		case IDX_FOOD: /* food */
			tmp = MIN(p_ptr->food,PY_FOOD_MAX);
			FormatResult(interp, "%d %d %f", p_ptr->food, PY_FOOD_MAX,
				(double) tmp / PY_FOOD_MAX);
			break;

		case IDX_GOLD: /* gold */
			LongResult(interp, p_ptr->au);
			break;

		case IDX_HEIGHT: /* height */
			IntResult(interp, p_ptr->ht);
			break;

		case IDX_HISTORY: /* history */
			buf[0] = '\0';
			for (i = 0; i < 4; i++)
			{
				(void) strcat(buf, format("%s\n", p_ptr_history[i]));
			}
			i = strlen(buf) - 1;
			while (buf[i] == '\n') buf[i--] = '\0';
			ExtToUtf_SetResult(interp, buf);
			break;

		case IDX_HITPOINTS: /* hitpoints */
			FormatResult(interp, "%d %d %f", p_ptr->chp, p_ptr->mhp,
				(double) p_ptr->chp / p_ptr->mhp);
			break;

		case IDX_INFRAVISION: /* infravision */
			IntResult(interp, p_ptr->see_infra * 10);
			break;

		case IDX_LEVEL: /* level */
			IntResult(interp, p_ptr->lev);
			break;

		case IDX_MANA: /* mana */
			if (p_ptr->msp) pct = (double) p_ptr->csp / p_ptr->msp;
			else pct = 0;
			FormatResult(interp, "%d %d %f", p_ptr->csp, p_ptr->msp, pct);
			break;

		case IDX_MANA_CUMBER: /* mana_cumber */
		{
			int glove, armor;
			calc_mana_cumber(&glove, &armor);
			FormatResult(interp, "%d %d", glove, armor);
			break;
		}

		case IDX_MAX_DEPTH: /* max_depth */
			IntResult(interp, p_ptr_max_depth);
			break; 

 		case IDX_NAME: /* name */
			if (objC == 3)
			{
				int len, base_len = 0;
				Tcl_DString extDString;
				char base[64];

				t = Tcl_GetStringFromObj(objV[2], &len);
				for (p = t; *p; p = (char*)Tcl_UtfNext(p))
				{
					Tcl_UniChar uniCh;
					int utfCharLen;

					utfCharLen = Tcl_UtfToUniChar(p, &uniCh);

					/* Control characters are not allowed */
					if (Tcl_UniCharIsControl(uniCh))
					{
						/* Set the error */
						Tcl_SetResult(interp,
							format("illegal char '%d' in character name",
							uniCh), TCL_VOLATILE);

						/* Failure */
						return TCL_ERROR;
					}

					/* This should restrict to valid OS filename characters */
					if (Tcl_UniCharIsAlnum(uniCh))
					{
						memcpy(base + base_len, p, utfCharLen);
						base_len += utfCharLen;
					}
					else
					{
						base[base_len++] = '_';
					}
				}

				/* Convert UTF8 to native encoding */
				Tcl_UtfToExternalDString(NULL, t, len, &extDString);

				/* Max 15 characters, 32-1 bytes */
				if ((Tcl_DStringLength(&extDString) > 31) ||
					(Tcl_NumUtfChars(t, len) > 15))
				{
					/* Set the error */
					Tcl_SetResult(interp,
						format("character name \"%s\" too long", t), TCL_VOLATILE);

					Tcl_DStringFree(&extDString);

					/* Failure */
					return TCL_ERROR;
				}
				(void) strcpy(op_ptr_full_name, Tcl_DStringValue(&extDString));
				Tcl_DStringFree(&extDString);

/*				process_player_name(FALSE);*/
				Tcl_UtfToExternalDString(NULL, base, base_len, &extDString);
				len = Tcl_DStringLength(&extDString);
				(void) strncpy(op_ptr_base_name, Tcl_DStringValue(&extDString), MIN(len, 31));
				Tcl_DStringFree(&extDString);

				Bind_Generic(EVENT_PY, KEYWORD_PY_NAME + 1);
			}
			ExtToUtf_SetResult(interp, (char *) op_ptr_full_name);
			break;

		case IDX_POSITION: /* position */
			FormatResult(interp, "%d %d", p_ptr_py, p_ptr_px);
			break;

		case IDX_RACE: /* race */
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)
			StaticResult(interp, (char *) p_name + rp_ptr->name);
#endif
#if defined(ZANGBANDTK)
			StaticResult(interp, (char *) rp_ptr->title);
#endif
			break;

		case IDX_SEX: /* sex */
			StaticResult(interp, (char *) sp_ptr->title);
			break;

		case IDX_SHOTS_PER_ROUND: /* shots_per_round */
		{
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)
			IntResult(interp, p_ptr->num_fire);
#endif
#if defined(ZANGBANDTK)
			int shots, shots_frac;
			shots_per_round(&shots, &shots_frac);
			FormatResult(interp, "%d.%d", shots, shots_frac);
#endif
			break;
		}

		case IDX_SOCIAL_CLASS: /* social_class */
			IntResult(interp, p_ptr->sc);
			break;

		case IDX_SPELL_BOOK: /* spell_book */
			listObjPtr = Tcl_NewListObj(0, NULL);
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)
			if (mp_ptr->spell_book)
			{
				(void) angtk_tval_string(&cstr, mp_ptr->spell_book);
				Tcl_ListObjAppendElement(interp, listObjPtr,
					Tcl_NewStringObj((char *) cstr,
					-1));
			}
#endif /* ANGBANDTK, KANGBANDTK, OANGBANDTK */
#if defined(ZANGBANDTK)
			if (p_ptr->realm1)
			{
				(void) angtk_tval_string(&cstr, p_ptr->realm1 + TV_LIFE_BOOK - 1);
				Tcl_ListObjAppendElement(interp, listObjPtr,
					Tcl_NewStringObj((char *) cstr, -1));
			}
			if (p_ptr->realm2)
			{
				(void) angtk_tval_string(&cstr, p_ptr->realm2 + TV_LIFE_BOOK - 1);
				Tcl_ListObjAppendElement(interp, listObjPtr,
					Tcl_NewStringObj((char *) cstr, -1));
			}
#endif /* ZANGBANDTK */
			Tcl_SetObjResult(interp, listObjPtr);
			break;

		case IDX_STAT: /* stat */
		    if (objC != 4)
		    {
				Tcl_WrongNumArgs(interp, infoCmd->depth + 2, objv, "stat varName");
				return TCL_ERROR;
		    }
		    if (Tcl_GetIndexFromObj(interp, objV[2], keyword_stat,
		    	"statName", 0, &index) != TCL_OK)
			{
				return TCL_ERROR;
		    }
			t = Tcl_GetStringFromObj(objV[3], NULL);
			if (SetArrayValueLong(t, "use", p_ptr->stat_use[index])
				!= TCL_OK)
			{
				return TCL_ERROR;
			}
			if (SetArrayValueLong(t, "top", p_ptr->stat_top[index])
				!= TCL_OK)
			{
				return TCL_ERROR;
			}
			if (SetArrayValueLong(t, "max", p_ptr->stat_max[index])
				!= TCL_OK)
			{
				return TCL_ERROR;
			}
			if (SetArrayValueLong(t, "race", rp_ptr->r_adj[index])
				!= TCL_OK)
			{
				return TCL_ERROR;
			}
			if (SetArrayValueLong(t, "class", cp_ptr->c_adj[index])
				!= TCL_OK)
			{
				return TCL_ERROR;
			}
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)
			if (SetArrayValueLong(t, "equip", p_ptr->stat_add[index])
				!= TCL_OK)
			{
				return TCL_ERROR;
			}
#endif /* ANGBANDTK, KANGBANDTK, OANGBANDTK */
#if defined(ZANGBANDTK)

			/* stat_add[] is not for equipment only in ZAngband */
			for (i = 0; i < 6; i++) stat_add[i] = 0;

			for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
			{
				object_type *o_ptr = &inventory[i];
				u32b f1, f2, f3;

				/* Skip non-objects */
				if (!o_ptr->k_idx) continue;

				/* Extract the item flags */
				object_flags(o_ptr, &f1, &f2, &f3);

				/* Affect stats */
				if (f1 & (TR1_STR)) stat_add[A_STR] += o_ptr->pval;
				if (f1 & (TR1_INT)) stat_add[A_INT] += o_ptr->pval;
				if (f1 & (TR1_WIS)) stat_add[A_WIS] += o_ptr->pval;
				if (f1 & (TR1_DEX)) stat_add[A_DEX] += o_ptr->pval;
				if (f1 & (TR1_CON)) stat_add[A_CON] += o_ptr->pval;
				if (f1 & (TR1_CHR)) stat_add[A_CHR] += o_ptr->pval;
			}

			if (SetArrayValueLong(t, "equip", stat_add[index])
				!= TCL_OK)
			{
				return TCL_ERROR;
			}

#endif /* ZANGBANDTK */
			break;

		case IDX_STATUS: /* status */
		    if (objC != 3)
			{
				Tcl_WrongNumArgs(interp, infoCmd->depth + 2, objv, "status");
				return TCL_ERROR;
		    }
		    if (Tcl_GetIndexFromObj(interp, objV[2], keyword_status,
				"status", 0, &index) != TCL_OK)
			{
				return TCL_ERROR;
			}
			t = player_status(index, &i);
			FormatResult(interp, "{%s} %d", t, i);
			break;

		case IDX_TITLE: /* title */
			ExtToUtf_SetResult(interp,
				(char *) player_title[p_ptr->pclass][(p_ptr->lev-1)/5]);
			break;

		case IDX_TO_DAM: /* to_dam */
			IntResult(interp, p_ptr->dis_to_d);
			break;

		case IDX_TO_HIT: /* to_hit */
			IntResult(interp, p_ptr->dis_to_h);
			break;

		case IDX_WEIGHT: /* weight */
			IntResult(interp, p_ptr->wt);
			break;

		case IDX_TOTAL_WEIGHT: /* total_weight */
			IntResult(interp, p_ptr->total_weight);
			break;

		case IDX_MAXIMIZE: /* maximize */
			BooleanResult(interp, p_ptr_maximize);
			break;

		case IDX_PRESERVE: /* preserve */
			BooleanResult(interp, p_ptr_preserve);
			break;

		case IDX_BASE_NAME: /* base_name */
			ExtToUtf_SetResult(interp, (char *) op_ptr_base_name);
			break;

		case IDX_FLAGS: /* flags */
			listObjPtr = dump_player_flags(interp);
			if (listObjPtr == NULL)
			{
				return TCL_ERROR;
			}
			Tcl_SetObjResult(interp, listObjPtr);
			break;

		case IDX_IS_DEAD: /* is_dead */
			BooleanResult(interp, p_ptr_is_dead);
			break;

		case IDX_TURN: /* turn */
			LongResult(interp, turn);
			break;

		case IDX_MAX_LEVEL: /* max_level */
			IntResult(interp, p_ptr_max_lev);
			break;

		case IDX_DISTURB: /* disturb */
			/* When is this allowed? */
			if (inkey_flags == 0)
			{
				disturb(0, 0);
			}
			break;

		case IDX_CHEAT: /* cheat */
		    if (objC != 3)
		    {
				Tcl_WrongNumArgs(interp, infoCmd->depth + 2, objv, "arrayName");
				return TCL_ERROR;
		    }
			t = Tcl_GetStringFromObj(objV[2], NULL);
			if (SetArrayValueLong(t, "death", (p_ptr_noscore & 0x0001) != 0)
				!= TCL_OK)
			{
				return TCL_ERROR;
			}
			if (SetArrayValueLong(t, "wizard", (p_ptr_noscore & 0x0002) != 0)
				!= TCL_OK)
			{
				return TCL_ERROR;
			}
			if (SetArrayValueLong(t, "debug", (p_ptr_noscore & 0x0008) != 0)
				!= TCL_OK)
			{
				return TCL_ERROR;
			}
			if (SetArrayValueLong(t, "borg", (p_ptr_noscore & 0x0010) != 0)
				!= TCL_OK)
			{
				return TCL_ERROR;
			}
			for (i = 0; i < CHEAT_MAX; i++)
			{
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)
				if (SetArrayValueLong(t, (char *) option_text[OPT_CHEAT+i],
					(op_ptr->opt[OPT_SCORE+i] != 0)) != TCL_OK)
				{
#endif /* ANGBANDTK, KANGBANDTK */
#if defined(ZANGBANDTK)
				if (SetArrayValueLong(t, (char *) cheat_info[i].o_text,
					(noscore & (cheat_info[i].o_set * 256 + cheat_info[i].o_bit)) != 0) != TCL_OK)
				{
#endif /* ZANGBANDTK */
					return TCL_ERROR;
				}
			}
			break;

		case IDX_NEW_SPELLS: /* new_spells */
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)
			if (!mp_ptr->spell_book)
#endif /* ANGBANDTK, KANGBANDTK, OANGBANDTK */
#if defined(ZANGBANDTK)
			if (!p_ptr->realm1)
#endif /* ZANGBANDTK */
			{
				StaticResult(interp, "character cannot read books");
				return TCL_ERROR;
			}
			IntResult(interp, p_ptr->new_spells);
			break;

		case IDX_COMMAND_REP: /* command_rep */
			IntResult(interp, p_ptr_command_rep);
			break;

		case IDX_RUNNING: /* running */
			IntResult(interp, p_ptr_running);
			break;

		case IDX_PRAYER_OR_SPELL: /* prayer_or_spell */
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)
			if (!mp_ptr->spell_book)
#endif /* ANGBANDTK, KANGBANDTK, OANGBANDTK */
#if defined(ZANGBANDTK)
			if (!p_ptr->realm1)
#endif /* ZANGBANDTK */
			{
				StaticResult(interp, "character cannot read books");
				return TCL_ERROR;
			}
			switch (mp_ptr->spell_book)
			{
#if defined(ANGBANDTK)
				case TV_MAGIC_BOOK: t = "spell"; break;
				case TV_PRAYER_BOOK: t = "prayer"; break;
				default: t = NULL;
#endif /* ANGBANDTK */
#if defined(KANGBANDTK)
				case TV_ILLUSION_BOOK:
				case TV_MAGIC_BOOK: t = "spell"; break;
				case TV_NATURE_BOOK:
				case TV_PRAYER_BOOK: t = "prayer"; break;
				default: t = NULL;
#endif /* KANGBANDTK */
#if defined(OANGBANDTK)
				case TV_PRAYER_BOOK: t = "prayer"; break;
				default: t = "spell"; break;
#endif /* OANGBANDTK */
#if defined(ZANGBANDTK)
				case TV_LIFE_BOOK: t = "prayer"; break;
				default: t = "spell"; break;
#endif /* ZANGBANDTK */
			}
			if (t == NULL)
			{
				quit_fmt("unhandled mp_ptr->spell_book %d",
					mp_ptr->spell_book);
			}
			StaticResult(interp, t);
			break;

		case IDX_HEALTH_WHO: /* health_who */
			/*
			 * Should I call health_track() to set PW_HEALTH?
			 * Should I call handle_stuff() to update the display?
			 */
			if (objC == 3)
			{
				int m_idx;
				if (Tcl_GetIntFromObj(interp, objV[2], &m_idx) != TCL_OK)
				{
					return TCL_ERROR;
				}
				if ((m_idx < 0) || (m_idx >= m_max))
				{
					FormatResult(interp,
						"bad m_list index \"%d\": must be between 0 and %d",
						m_idx, (int) m_max - 1);
				}
				p_ptr->health_who = m_idx;
				break;
			}
			IntResult(interp, p_ptr->health_who);
			break;

		case IDX_MONSTER_RACE_IDX: /* monster_race_idx */
			/*
			 * Should I call monster_race_track() to set PW_MONSTER?
			 * Should I call handle_stuff() to update the display?
			 */
			if (objC == 3)
			{
				int r_idx;
				if (Tcl_GetIntFromObj(interp, objV[2], &r_idx) != TCL_OK)
				{
					return TCL_ERROR;
				}
				if (!monster_race_valid(r_idx))
				{
					FormatResult(interp,
						"bad r_info index \"%d\": must be between 0 and %d",
						r_idx, (int) MAX_VALID_R_IDX - 1);
					return TCL_ERROR;
				}
				p_ptr->monster_race_idx = r_idx;
				break;
			}
			IntResult(interp, p_ptr->monster_race_idx);
			break;

		case IDX_LIFE_RATING: /* life_rating */
			i = (int) (((long) p_ptr_player_hp[PY_MAX_LEVEL - 1] * 200L) / 
				(2 * p_ptr->hitdie + ((PY_MAX_LEVEL - 1) *
				(p_ptr->hitdie + 1))));
			IntResult(interp, i);
			break;

		case IDX_SENSE: /* sense */
		{
			int mage, warrior;
			(void) sense_chance(&mage, &warrior);
			FormatResult(interp, "mage=%d warrior=%d", mage, warrior);
			break;
		}

#if defined(KANGBANDTK)
		case IDX_ARENA_NUMBER: /* arena_number */
			IntResult(interp, p_ptr->arena_number);
			break;

		case IDX_INSIDE_ARENA: /* inside_arena */
			IntResult(interp, p_ptr->inside_arena);
			break;

		case IDX_INSIDE_QUEST: /* inside_quest */
			IntResult(interp, p_ptr->inside_quest);
			break;

		case IDX_EXIT_BLDG: /* exit_bldg */
			IntResult(interp, p_ptr->exit_bldg);
			break;

		case IDX_PETS: /* pets */
			Tcl_SetObjResult(interp, DumpPets());
			break;
#endif /* KANGBANDTK */

#if defined(OANGBANDTK) || defined(ZANGBANDTK)
		case IDX_DEADLINESS_CONVERSION: /* deadliness_conversion */
			if (Tcl_GetIntFromObj(interp, objV[2], &index) == TCL_OK)
			{
				if ((index >= 0) && (index < 201))
				{
					IntResult(interp, deadliness_conversion[index]);
				}
				else if ((index < 0) && (ABS(index) < 201))
				{
					index *= -1;
					IntResult(interp, -deadliness_conversion[index]);
				}
			}
			break;
#endif /* OANGBANDTK, ZANGBANDTK */

#if defined(ZANGBANDTK)
		case IDX_MUTATIONS: /* mutations */
			Tcl_SetObjResult(interp, DumpMutations());
			break;

		case IDX_PETS: /* pets */
			Tcl_SetObjResult(interp, DumpPets());
			break;

		case IDX_REALM1: /* realm1 */
			StaticResult(interp, (char *) realm_names[p_ptr->realm1]);
			break;

		case IDX_REALM2: /* realm2 */
			StaticResult(interp, (char *) realm_names[p_ptr->realm2]);
			break;

		case IDX_PATRON: /* patron */
			ExtToUtf_SetResult(interp,
				(char *) chaos_patrons[p_ptr->chaos_patron]);
			break;

		case IDX_INSIDE_ARENA: /* inside_arena */
			BooleanResult(interp, p_ptr->inside_arena);
			break;

		case IDX_INSIDE_QUEST: /* inside_quest */
			IntResult(interp, p_ptr->inside_quest);
			break;

		case IDX_VIRTUES: /* virtues */
			Tcl_SetObjResult(interp, DumpVirtues());
			break;

#endif /* ZANGBANDTK */
	}

	return TCL_OK;
}

#if defined(ZANGBANDTK)

/*
 * An array of human-readable descriptions of each power, indexed
 * by POWER_XXX constants. These are similar to spell names.
 */
cptr power_desc[] = {

/* Racial power */
"Detect Doors & Traps",
"Create Food",
"Teleport",
"Remove Fear",
"Berserk",
"Shadow Shifting",
"Pattern Mindwalking",
"Berserk",
"Explosive Rune",
"Stone To Mud",
"Probing",
"Throw Boulder",
"Scare Monster",
"Spit Acid",
"Poison Dart",
"Detect Doors & Traps",
"Magic Missile",
"Breath Weapon",
"Mind Blast",
"Fire Bolt/Ball",
"Stone Skin",
"Restore Life",
"Restore Life",
"Drain Life",
"Scare Monster",
"Sleeping Dust",

/* Mutation power */
"Spit Acid",
"Fire Breath",
"Hypnotic Gaze",
"Telekinesis",
"Teleport",
"Mind Blast",
"Emit Radiation",
"Vampiric Drain",
"Smell Metal",
"Smell Monsters",
"Blink",
"Eat Rock",
"Swap Position",
"Shriek",
"Illuminate",
"Detect Curse",
"Berserk",
"Polymorph",
"Midas Touch",
"Grow Mold",
"Resist Elements",
"Earthquake",
"Eat Magic",
"Weigh Magic",
"Sterilize",
"Panic Hit",
"Dazzle",
"Laser Eye",
"Recall",
"Banish Evil",
"Cold Touch",
"Throw Object"
};

/*
 * Set a Tcl array variable with info about a power
 */
static void dump_power_info(char *varName, int power, bool racial, int min_level, int cost, int use_stat, int difficulty)
{
	char *type = racial ? "racial" : "mutation";
	int chance = 100 - racial_chance(min_level, use_stat, difficulty);

	SetArrayValueLong(varName, "chance", chance);
	SetArrayValueLong(varName, "cost", cost);
	SetArrayValueLong(varName, "level", min_level);
	ExtToUtf_SetArrayValueString(varName, "name", (char *) power_desc[power]);
	SetArrayValueString(varName, "stat", (char *) keyword_stat[use_stat]);
	SetArrayValueString(varName, "type", type);
}

/*
 * Set a Tcl array variable with info about a power
 */
static void DumpPowerAux(char *varName, int power)
{
    s16b plev = p_ptr->lev;

	switch (power)
	{
		/* Racial power */
		case POWER_DWARF:
			dump_power_info(varName, power, TRUE, 5, 5, A_WIS, 12);
			break;

		case POWER_HOBBIT:
			dump_power_info(varName, power, TRUE, 15, 10, A_INT, 10);
			break;

		case POWER_GNOME:
			dump_power_info(varName, power, TRUE, 5, (5 + (plev / 5)),
				A_INT, 12);
			break;

		case POWER_HALF_ORC:
			dump_power_info(varName, power, TRUE, 3, 5, A_WIS,
				(p_ptr->pclass == CLASS_WARRIOR?5:10));
			break;

		case POWER_HALF_TROLL:
			dump_power_info(varName, power, TRUE, 10, 12, A_WIS,
			(p_ptr->pclass == CLASS_WARRIOR) ? 6 : 12);
			break;

		/* Shadow Shifting */
		case POWER_AMBERITE_A:
			dump_power_info(varName, power, TRUE, 30, 50, A_INT, 50);
			break;

		/* Pattern Mindwalking */
		case POWER_AMBERITE_B:
			dump_power_info(varName, power, TRUE, 40, 75, A_WIS, 50);
			break;

		case POWER_BARBARIAN:
			dump_power_info(varName, power, TRUE, 8, 10, A_WIS,
				(p_ptr->pclass == CLASS_WARRIOR) ? 6 : 12);
			break;

		case POWER_HALF_OGRE:
			dump_power_info(varName, power, TRUE, 25, 35, A_INT, 15);
			break;

		case POWER_HALF_GIANT:
			dump_power_info(varName, power, TRUE, 20, 10, A_STR, 12);
			break;

		case POWER_HALF_TITAN:
			dump_power_info(varName, power, TRUE, 35, 20, A_INT, 12);
			break;

		case POWER_CYCLOPS:
			dump_power_info(varName, power, TRUE, 20, 15, A_STR, 12);
			break;

		case POWER_YEEK:
			dump_power_info(varName, power, TRUE, 15, 15, A_WIS, 10);
			break;

		case POWER_KLACKON:
			dump_power_info(varName, power, TRUE, 9, 9, A_DEX, 14);
			break;

		case POWER_KOBOLD:
			dump_power_info(varName, power, TRUE, 12, 8, A_DEX, 14);
			break;

		case POWER_NIBELUNG:
			dump_power_info(varName, power, TRUE, 10, 5, A_WIS, 10);
			break;

		case POWER_DARK_ELF:
			dump_power_info(varName, power, TRUE, 2, 2, A_INT, 9);
			break;

		case POWER_DRACONIAN:
			dump_power_info(varName, power, TRUE, 1, p_ptr->lev, A_CON, 12);
			break;

		case POWER_MIND_FLAYER:
			dump_power_info(varName, power, TRUE, 15, 12, A_INT, 14);
			break;

		case POWER_IMP:
			dump_power_info(varName, power, TRUE, 9, 15, A_WIS, 15);
			break;

		case POWER_GOLEM:
			dump_power_info(varName, power, TRUE, 20, 15, A_CON, 8);
			break;

		case POWER_SKELETON:
		case POWER_ZOMBIE:
			dump_power_info(varName, power, TRUE, 30, 30, A_WIS, 18);
			break;

		case POWER_VAMPIRE:
	   		dump_power_info(varName, power, TRUE, 2, (1 + (plev / 3)),
	   			A_CON, 9);
			break;

		case POWER_SPECTRE:
			dump_power_info(varName, power, TRUE, 4, 6, A_INT, 3);
			break;

		case POWER_SPRITE:
			dump_power_info(varName, power, TRUE, 12, 12, A_INT, 15);
			break;

		/* Mutation power */
		case POWER_SPIT_ACID:
			dump_power_info(varName, power, FALSE, 9, 9, A_DEX, 15);
			break;

		case POWER_BR_FIRE:
			dump_power_info(varName, power, FALSE, 20, p_ptr->lev, A_CON, 18);
			break;

		case POWER_HYPN_GAZE:
			dump_power_info(varName, power, FALSE, 12, 12, A_CHR, 18);
			break;

		case POWER_TELEKINES:
			dump_power_info(varName, power, FALSE, 9, 9, A_WIS, 14);
			break;

		case POWER_VTELEPORT:
			dump_power_info(varName, power, FALSE, 7, 7, A_WIS, 15);
			break;

		case POWER_MIND_BLST:
			dump_power_info(varName, power, FALSE, 5, 3, A_WIS, 15);
			break;

		case POWER_RADIATION:
			dump_power_info(varName, power, FALSE, 15, 15, A_CON, 14);
			break;

		case POWER_VAMPIRISM:
			dump_power_info(varName, power, FALSE, 2,
				(1 + (p_ptr->lev / 3)), A_CON, 9);
			break;

		case POWER_SMELL_MET:
			dump_power_info(varName, power, FALSE, 3, 2, A_INT, 12);
			break;

		case POWER_SMELL_MON:
			dump_power_info(varName, power, FALSE, 5, 4, A_INT, 15);
			break;

		case POWER_BLINK:
			dump_power_info(varName, power, FALSE, 3, 3, A_WIS, 12);
			break;

		case POWER_EAT_ROCK:
			dump_power_info(varName, power, FALSE, 8, 12, A_CON, 18);
			break;

		case POWER_SWAP_POS:
			dump_power_info(varName, power, FALSE, 15, 12, A_DEX, 16);
			break;

		case POWER_SHRIEK:
			dump_power_info(varName, power, FALSE, 20, 14, A_CON, 16);
			break;

		case POWER_ILLUMINE:
			dump_power_info(varName, power, FALSE, 3, 2, A_INT, 10);
			break;

		case POWER_DET_CURSE:
			dump_power_info(varName, power, FALSE, 7, 14, A_WIS, 14);
			break;

		case POWER_BERSERK:
			dump_power_info(varName, power, FALSE, 8, 8, A_STR, 14);
			break;

		case POWER_POLYMORPH:
			dump_power_info(varName, power, FALSE, 18, 20, A_CON, 18);
			break;

		case POWER_MIDAS_TCH:
			dump_power_info(varName, power, FALSE, 10, 5, A_INT, 12);
			break;

		case POWER_GROW_MOLD:
			dump_power_info(varName, power, FALSE, 1, 6, A_CON, 14);
			break;

		case POWER_RESIST:
			dump_power_info(varName, power, FALSE, 10, 12, A_CON, 12);
			break;

		case POWER_EARTHQUAKE:
			dump_power_info(varName, power, FALSE, 12, 12, A_STR, 16);
			break;

		case POWER_EAT_MAGIC:
			dump_power_info(varName, power, FALSE, 17, 1, A_WIS, 15);
			break;

		case POWER_WEIGH_MAG:
			dump_power_info(varName, power, FALSE, 6, 6, A_INT, 10);
			break;

		case POWER_STERILITY:
			dump_power_info(varName, power, FALSE, 12, 23, A_CHR, 15);
			break;

		case POWER_PANIC_HIT:
			dump_power_info(varName, power, FALSE, 10, 12, A_DEX, 14);
			break;

		case POWER_DAZZLE:
			dump_power_info(varName, power, FALSE, 7, 15, A_CHR, 8);
			break;

		case POWER_LASER_EYE:
			dump_power_info(varName, power, FALSE, 7, 10, A_WIS, 9);
			break;

		case POWER_RECALL:
			dump_power_info(varName, power, FALSE, 17, 50, A_INT, 16);
			break;

		case POWER_BANISH:
			dump_power_info(varName, power, FALSE, 25, 25, A_WIS, 18);
			break;

		case POWER_COLD_TOUCH:
			dump_power_info(varName, power, FALSE, 2, 2, A_CON, 11);
			break;

		case POWER_LAUNCHER:
			dump_power_info(varName, power, FALSE, 1, p_ptr->lev, A_STR, 6);
			break;
	}
}

/*
 * Get a list of POWER_XXX indexes of character powers.
 */
int get_powers(int *power)
{
	int num = 0;

	/* Racial power */
	switch (p_ptr->prace)
	{
		case RACE_DWARF:
			power[num++] = POWER_DWARF;
			break;

		case RACE_HOBBIT:
			power[num++] = POWER_HOBBIT;
			break;

		case RACE_GNOME:
			power[num++] = POWER_GNOME;
			break;

		case RACE_HALF_ORC:
			power[num++] = POWER_HALF_ORC;
			break;

		case RACE_HALF_TROLL:
			power[num++] = POWER_HALF_TROLL;
			break;

		case RACE_AMBERITE:
			power[num++] = POWER_AMBERITE_A;
			power[num++] = POWER_AMBERITE_B;
			break;

		case RACE_BARBARIAN:
			power[num++] = POWER_BARBARIAN;
			break;

		case RACE_HALF_OGRE:
			power[num++] = POWER_HALF_OGRE;
			break;

		case RACE_HALF_GIANT:
			power[num++] = POWER_HALF_GIANT;
			break;

		case RACE_HALF_TITAN:
			power[num++] = POWER_HALF_TITAN;
			break;

		case RACE_CYCLOPS:
			power[num++] = POWER_CYCLOPS;
			break;

		case RACE_YEEK:
			power[num++] = POWER_YEEK;
			break;

		case RACE_KLACKON:
			power[num++] = POWER_KLACKON;
			break;

		case RACE_KOBOLD:
			power[num++] = POWER_KOBOLD;
			break;

		case RACE_NIBELUNG:
			power[num++] = POWER_NIBELUNG;
			break;

		case RACE_DARK_ELF:
			power[num++] = POWER_DARK_ELF;
			break;

		case RACE_DRACONIAN:
			power[num++] = POWER_DRACONIAN;
			break;

		case RACE_MIND_FLAYER:
			power[num++] = POWER_MIND_FLAYER;
			break;

		case RACE_IMP:
			power[num++] = POWER_IMP;
			break;

		case RACE_GOLEM:
			power[num++] = POWER_GOLEM;
			break;

		case RACE_SKELETON:
			power[num++] = POWER_SKELETON;
			break;

		case RACE_ZOMBIE:
			power[num++] = POWER_ZOMBIE;
			break;

		case RACE_VAMPIRE:
			power[num++] = POWER_VAMPIRE;
			break;

		case RACE_SPECTRE:
			power[num++] = POWER_SPECTRE;
			break;

		case RACE_SPRITE:
			power[num++] = POWER_SPRITE;
			break;
	}

	/* Mutation power */
	if (p_ptr->muta1 & MUT1_SPIT_ACID)
	{
		power[num++] = POWER_SPIT_ACID;
	}
	if (p_ptr->muta1 & MUT1_BR_FIRE)
	{
		power[num++] = POWER_BR_FIRE;
	}
	if (p_ptr->muta1 & MUT1_HYPN_GAZE)
	{
		power[num++] = POWER_HYPN_GAZE;
	}
	if (p_ptr->muta1 & MUT1_TELEKINES)
	{
		power[num++] = POWER_TELEKINES;
	}
	if (p_ptr->muta1 & MUT1_VTELEPORT)
	{
		power[num++] = POWER_VTELEPORT;
	}
	if (p_ptr->muta1 & MUT1_MIND_BLST)
	{
		power[num++] = POWER_MIND_BLST;
	}
	if (p_ptr->muta1 & MUT1_RADIATION)
	{
		power[num++] = POWER_RADIATION;
	}
	if (p_ptr->muta1 & MUT1_VAMPIRISM)
	{
		power[num++] = POWER_VAMPIRISM;
	}
	if (p_ptr->muta1 & MUT1_SMELL_MET)
	{
		power[num++] = POWER_SMELL_MET;
	}
	if (p_ptr->muta1 & MUT1_SMELL_MON)
	{
		power[num++] = POWER_SMELL_MON;
	}
	if (p_ptr->muta1 & MUT1_BLINK)
	{
		power[num++] = POWER_BLINK;
	}
	if (p_ptr->muta1 & MUT1_EAT_ROCK)
	{
		power[num++] = POWER_EAT_ROCK;
	}
	if (p_ptr->muta1 & MUT1_SWAP_POS)
	{
		power[num++] = POWER_SWAP_POS;
	}
	if (p_ptr->muta1 & MUT1_SHRIEK)
	{
		power[num++] = POWER_SHRIEK;
	}
	if (p_ptr->muta1 & MUT1_ILLUMINE)
	{
		power[num++] = POWER_ILLUMINE;
	}
	if (p_ptr->muta1 & MUT1_DET_CURSE)
	{
		power[num++] = POWER_DET_CURSE;
	}
	if (p_ptr->muta1 & MUT1_BERSERK)
	{
		power[num++] = POWER_BERSERK;
	}
	if (p_ptr->muta1 & MUT1_POLYMORPH)
	{
		power[num++] = POWER_POLYMORPH;
	}
	if (p_ptr->muta1 & MUT1_MIDAS_TCH)
	{
		power[num++] = POWER_MIDAS_TCH;
	}
	if (p_ptr->muta1 & MUT1_GROW_MOLD)
	{
		power[num++] = POWER_GROW_MOLD;
	}
	if (p_ptr->muta1 & MUT1_RESIST)
	{
		power[num++] = POWER_RESIST;
	}
	if (p_ptr->muta1 & MUT1_EARTHQUAKE)
	{
		power[num++] = POWER_EARTHQUAKE;
	}
	if (p_ptr->muta1 & MUT1_EAT_MAGIC)
	{
		power[num++] = POWER_EAT_MAGIC;
	}
	if (p_ptr->muta1 & MUT1_WEIGH_MAG)
	{
		power[num++] = POWER_WEIGH_MAG;
	}
	if (p_ptr->muta1 & MUT1_STERILITY)
	{
		power[num++] = POWER_STERILITY;
	}
	if (p_ptr->muta1 & MUT1_PANIC_HIT)
	{
		power[num++] = POWER_PANIC_HIT;
	}
	if (p_ptr->muta1 & MUT1_DAZZLE)
	{
		power[num++] = POWER_DAZZLE;
	}
	if (p_ptr->muta1 & MUT1_LASER_EYE)
	{
		power[num++] = POWER_LASER_EYE;
	}
	if (p_ptr->muta1 & MUT1_RECALL)
	{
		power[num++] = POWER_RECALL;
	}
	if (p_ptr->muta1 & MUT1_BANISH)
	{
		power[num++] = POWER_BANISH;
	}
	if (p_ptr->muta1 & MUT1_COLD_TOUCH)
	{
		power[num++] = POWER_COLD_TOUCH;
	}
	if (p_ptr->muta1 & MUT1_LAUNCHER)
	{
		power[num++] = POWER_LAUNCHER;
	}

	/* Return number of powers */
	return num;
}

/*
 *--------------------------------------------------------------
 *
 * objcmd_power --
 *
 *	Implements the "power" script command.
 * 	Syntax:
 *		power get
 * 			Return list of racial/mutation power names
 *
 *		power info $power $arrayName
 * 			Set array with info about a power:
 * 				$arrayName(chance) - how hard to use
 * 				$arrayName(cost) - mana cost per use
 *				$arrayName(name) - name of this power
 * 				$arrayName(stat) - stat used
 * 				$arrayName(level) - min level to use
 * 				$arrayName(type) "racial"/"mutation"
 *
 *--------------------------------------------------------------
 */

int
objcmd_power(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	static CONST char *cmdOptions[] = {"get", "info", NULL};
	enum {IDX_GET, IDX_INFO} option;
	int index;

	Tcl_Obj *listObjPtr;
	char *t;
	int i, num, power[MAX_POWER];

	/* Required number of arguments */
    if (objC < 2)
    {
		Tcl_WrongNumArgs(interp, infoCmd->depth + 1, objv, "option ?arg ...?");
		return TCL_ERROR;
    }

	/* Get requested option */
    if (Tcl_GetIndexFromObj(interp, objV[1], cmdOptions, "option", 0, 
		(int *) &option) != TCL_OK)
	{
		return TCL_ERROR;
    }

	if (!character_generated)
	{
		StaticResult(interp, "character has not been generated yet");
		return TCL_ERROR;
	}

	switch (option)
	{
		case IDX_GET: /* get */
			num = get_powers(power);
			if (num)
			{
				listObjPtr = Tcl_NewListObj(0, NULL);
				for (i = 0; i < num; i++)
				{
					Tcl_ListObjAppendElement(interp, listObjPtr,
						Tcl_NewIntObj(power[i]));
				}
				Tcl_SetObjResult(interp, listObjPtr);
			}
			break;

		case IDX_INFO: /* info */
		    if (objC != 4)
		    {
				Tcl_WrongNumArgs(interp, infoCmd->depth + 2, objv, "power arrayName");
				return TCL_ERROR;
		    }
			if (Tcl_GetIntFromObj(interp, objV[2], &index)
				!= TCL_OK)
			{
				return TCL_ERROR;
			}
			t = Tcl_GetStringFromObj(objV[3], NULL);
			DumpPowerAux(t, index);
			break;
	}

	return TCL_OK;
}

#endif /* ZANGBANDTK */

/*
 * Return an error if the given object doesn't contain a valid
 * spell book k_info[] index for the character.
 */
static int GetBookFromObj(Tcl_Interp *interp, Tcl_Obj *objPtr, int *bookNum)
{
	int k_idx, tval;
	cptr cstr;

    /* Get the book number */
	if (Tcl_GetIntFromObj(interp, objPtr, &k_idx) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* Verify the book number */
	if ((k_idx <= 0) || (k_idx >= MAX_K_IDX))
	{
		/* Set the error */
		FormatResult(interp, "invalid object index \"%d\"", k_idx);

		/* Failure */
		return TCL_ERROR;
	}

	/* Get the tval */
	tval = k_info[k_idx].tval;

	/* Verify the tval */
#if defined(ANGBANDTK)
	if ((tval < TV_MAGIC_BOOK) ||
		(tval > TV_PRAYER_BOOK))
#endif /* */
#if defined(KANGBANDTK)
	if ((tval < TV_MAGIC_BOOK) ||
		(tval > TV_NATURE_BOOK))
#endif /* */
#if defined(OANGBANDTK)
	if ((tval < TV_MAGIC_BOOK) ||
		(tval > TV_NECRO_BOOK))
#endif /* */
#if defined(ZANGBANDTK)
	if ((tval < TV_LIFE_BOOK) ||
		(tval > TV_ARCANE_BOOK))
#endif /* */
	{
		/* Set the error */
		FormatResult(interp, "object \"%d\" is not a book", k_idx);

		/* Failure */
		return TCL_ERROR;
	}

	/* Verify the tval */
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)
	if (tval != mp_ptr->spell_book)
#endif /* ANGBANDTK, KANGBANDTK, OANGBANDTK */
#if defined(ZANGBANDTK)
	if ((tval != p_ptr->realm1 + (TV_LIFE_BOOK - 1)) &&
		(tval != p_ptr->realm2 + (TV_LIFE_BOOK - 1)))
#endif /* ZANGBANDTK */
	{
		/* Get the string for the tval */
		(void) angtk_tval_string(&cstr, tval);

		/* Set the error */
		FormatResult(interp, "character can't read \"%s\" books", cstr);

		/* Failure */
		return TCL_ERROR;
	}

	/* Return the book number */
	(*bookNum) = k_idx;

	/* Success */
	return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * objcmd_spell --
 *
 *	Implements the "spell" script command.
 * 	Syntax:
 *		spell find $k_idx ?-tester $boolean? ?-known $boolean?
 *		spell info $k_idx $spell $arrayName
 *			arrayName(char)
 *			arrayName(name)
 *			arrayName(level)
 *			arrayName(mana)
 *			arrayName(chance)
 *			arrayName(known)
 *			arrayName(info)
 *
 *--------------------------------------------------------------
 */

#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)

int
objcmd_spell(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	static CONST char *cmdOptions[] = {"find", "info", "memory", NULL};
	enum {IDX_FIND, IDX_INFO, IDX_MEMORY} option;
	int index;

	Tcl_Obj *listObjPtr;
    int k_idx, sval, spell, i, first;
	char *t;
#if defined(ANGBANDTK) || defined(KANGBANDTK)		
	u32b flags;
#endif /* */
	u32b forgotten, learned, worked;

	/* Default to ignoring known */
	int request_known = 0, match_known = 0;

	/* Default to ignoring spell_okay() */
	int request_tester = 0, match_tester = 0;

	/* Default to ignoring mana */
	int request_vmana = 0, match_vmana = 0;

	/* Required number of arguments */
	if (objC < 2)
	{
		/* Set the error */
		Tcl_WrongNumArgs(interp, infoCmd->depth + 1, objv, "option ?arg ...?");

		/* Failure */
		return TCL_ERROR;
	}

	/* Get the requested option */
    if (Tcl_GetIndexFromObj(interp, objV[1], cmdOptions, "option", 0, 
		(int *) &option) != TCL_OK)
    {
		return TCL_ERROR;
    }

	/* The character doesn't exist yet */
	if (!character_generated)
	{
		/* Set the error */
		StaticResult(interp, "character has not been generated yet");

		/* Failure */
		return TCL_ERROR;
	}

	/* The character is not a magic user */
	if (!mp_ptr->spell_book)
	{
		/* Set the error */
		StaticResult(interp, "character cannot read books");

		/* Failure */
		return TCL_ERROR;
	}

	switch (option)
	{
		case IDX_FIND: /* find */

			/* Required number of arguments */
			if (objC < 3)
		    {
				Tcl_WrongNumArgs(interp, infoCmd->depth + 2, objv, "bookNum");
				return TCL_ERROR;
			}

			/* Get the book number */
			if (GetBookFromObj(interp, objV[2], &k_idx) != TCL_OK)
			{
				return TCL_ERROR;
			}

			/* Get the sval */
			sval = k_info[k_idx].sval;

			/* Scan arguments for options */
			for (i = 3; i < objC; )
			{
				static CONST char *cmdOptions[] = {"-known", "-tester",
					"-verify_mana", NULL};

				/* Get the sub-option */
				if (Tcl_GetIndexFromObj(interp, objV[i], cmdOptions, "option",
					0, &index) != TCL_OK)
				{
					return TCL_ERROR;
				}

				switch (index)
				{
					case 0: /* -known */
						if (Tcl_GetBooleanFromObj(interp, objV[i+1],
							&match_known) != TCL_OK)
						{
							return TCL_ERROR;
						}
						request_known = 1;
						i += 2;
						break;

					case 1: /* -tester */
						if (Tcl_GetBooleanFromObj(interp, objV[i+1],
							&match_tester) != TCL_OK)
						{
							return TCL_ERROR;
						}
						request_tester = 1;
						i += 2;
						break;

					case 2: /* -verify_mana */
						if (Tcl_GetBooleanFromObj(interp, objV[i+1],
							&match_vmana) != TCL_OK)
						{
							return TCL_ERROR;
						}
						request_vmana = 1;
						i += 2;
						break;
				}
			}

			/*
			 * If we are not casting or studying a spell, then there is
			 * no need to check spell_okay().
			 */
			if ((p_ptr->command_cmd != 'G') &&
				(p_ptr->command_cmd != 'm') &&
				(p_ptr->command_cmd != 'p'))
			{
				request_tester = 0;
			}

			/*
			 * The list of indexes returned by this command are
			 * book-relative. That is, the first spell in a book
			 * is index 0, the next is 1, and so on, except for those
			 * spells which are "illegible".
			 */
			i = -1;

			/* Return spell list */
			listObjPtr = Tcl_NewListObj(0, NULL);

#if defined(ANGBANDTK) || defined(KANGBANDTK)		
			/* Examine each spell */
			for (spell = 0; spell < PY_MAX_SPELLS; spell++)
			{
				if (spell < 32)
				{
					flags = spell_flags[mp_ptr_spell_type][sval][0];
					learned = p_ptr->spell_learned1;
				}
				else
				{
					flags = spell_flags[mp_ptr_spell_type][sval][1];
					learned = p_ptr->spell_learned2;
				}

				/* Check for this spell */
				if (flags & (1L << (spell % 32)))
				{
#endif /* */
#if defined(OANGBANDTK)
			/* Examine each spell */
			for (spell = mp_ptr->book_start_index[sval];
				 spell < mp_ptr->book_start_index[sval + 1];
				 spell++)
			{
				if (spell < 32)
				{
					learned = p_ptr->spell_learned1;
				}
				else
				{
					learned = p_ptr->spell_learned2;
				}

				{
#endif /* */
					const magic_type *s_ptr;

					/* Zero-based spell index in this book */
					++i;

					/* Access the spell */
					s_ptr = &mp_ptr->info[spell];

					/* Skip illegible spells */
					if (s_ptr->slevel >= 99) continue;

					/* Return only known spells */
					if (request_known)
					{
						bool known = (learned & (1L << (spell % 32))) != 0;

						if (known != match_known)
						{
							continue;
						}
					}

					/* Return only "okay" spells */
					if (request_tester && match_tester)
					{
						bool known = p_ptr->command_cmd != 'G';
						if (!spell_okay(spell, known))
						{
							continue;
						}
					}

					/* Return only spells we have enough mana to cast.
					 * If verify_mana is TRUE, we can always try to cast. */
					if (request_vmana && match_vmana)
					{
						bool known = (learned & (1L << (spell % 32))) != 0;

						if (known && !verify_mana && (s_ptr->smana > p_ptr->csp))
						{
							continue;
						}
					}

					/* Append this spellindex to the list */
					Tcl_ListObjAppendElement(interp, listObjPtr,
						Tcl_NewIntObj(i));
				}
			}

			/* Return the list of spell indexes */
			Tcl_SetObjResult(interp, listObjPtr);
			break;

		case IDX_INFO: /* info */

			/* Required number of arguments */
		    if (objC < 5)
		    {
				Tcl_WrongNumArgs(interp, infoCmd->depth + 2, objv, "bookNum spellIndex arrayName");
				return TCL_ERROR;
		    }

		    /* Get the book number */
			if (GetBookFromObj(interp, objV[2], &k_idx) != TCL_OK)
			{
				return TCL_ERROR;
			}

		    /* Get the spell index */
			if (Tcl_GetIntFromObj(interp, objV[3], &spell) != TCL_OK)
			{
				return TCL_ERROR;
			}

			/* Verify the spell index */
			if ((spell < 0) || (spell >= PY_MAX_SPELLS))
			{
				/* REPORT */
				return TCL_ERROR;
			}

			/* Get the variable name */
			t = Tcl_GetStringFromObj(objV[4], NULL);

			/* Get the sval */
			sval = k_info[k_idx].sval;

			/* Find the first spell in the book */
#if defined(ANGBANDTK) || defined(KANGBANDTK)		
			first = -1;
			for (i = 0; i < PY_MAX_SPELLS; i++)
			{
				if (i < 32)
				{
					flags = spell_flags[mp_ptr_spell_type][sval][0];
				}
				else
				{
					flags = spell_flags[mp_ptr_spell_type][sval][1];
				}

				if (flags & (1L << (i % 32)))
				{
					first = i;
					break;
				}
			}
#endif
#if defined(OANGBANDTK)
			first = mp_ptr->book_start_index[sval];
#endif
			if (first == -1)
			{
				/* REPORT */
				return TCL_ERROR;
			}

			/* Get the spell index */
			spell += first;

			if (spell < 32)
			{
#if defined(ANGBANDTK) || defined(KANGBANDTK)		
				flags = spell_flags[mp_ptr_spell_type][sval][0];
#endif
				forgotten = p_ptr->spell_forgotten1;
				learned = p_ptr->spell_learned1;
				worked = p_ptr->spell_worked1;
			}
			else
			{
#if defined(ANGBANDTK) || defined(KANGBANDTK)		
				flags = spell_flags[mp_ptr_spell_type][sval][1];
#endif
				forgotten = p_ptr->spell_forgotten2;
				learned = p_ptr->spell_learned2;
				worked = p_ptr->spell_worked2;
			}

			/* Check for this spell */
#if defined(ANGBANDTK) || defined(KANGBANDTK)		
			if (flags & (1L << (spell % 32)))
#endif
#if defined(OANGBANDTK)
			if ((spell >= first) && (spell < mp_ptr->book_start_index[sval + 1]))
#endif
			{
				const magic_type *s_ptr;
				char info[80], *comment;

				/* Access the spell */
				s_ptr = &mp_ptr->info[spell];

				/* Skip illegible spells */
				if (s_ptr->slevel >= 99)
				{
					/* REPORT */
					return TCL_ERROR;
				}

				if (SetArrayValueChar(t, "char", I2A(spell - first)) != TCL_OK)
				{
					return TCL_ERROR;
				}
				if (ExtToUtf_SetArrayValueString(t, "name",
#if defined(ANGBANDTK) || defined(KANGBANDTK)
					(char *) spell_names[mp_ptr_spell_type][spell]) != TCL_OK)
#endif
#if defined(OANGBANDTK)
					(char *) spell_names[s_ptr->index]) != TCL_OK)
#endif
				{
					return TCL_ERROR;
				}
				if (SetArrayValueLong(t, "level", s_ptr->slevel) != TCL_OK)
				{
					return TCL_ERROR;
				}
				if (SetArrayValueLong(t, "mana", s_ptr->smana) != TCL_OK)
				{
					return TCL_ERROR;
				}
				if (SetArrayValueLong(t, "chance", spell_chance(spell))
					!= TCL_OK)
				{
					return TCL_ERROR;
				}
				if (SetArrayValueLong(t, "known",
					(learned & (1L << spell)) != 0) != TCL_OK)
				{
					return TCL_ERROR;
				}

#if defined(ANGBANDTK) || defined(KANGBANDTK)
				spell_info(info, spell);
#endif
#if defined(OANGBANDTK)
				spell_info(info, s_ptr->index);
#endif
				if (!info[0]) (void) strcpy(info, " ");
				comment = info;

				/* Describe the status of the spell */
				if (forgotten & (1L << (spell % 32)))
				{
					comment = " forgotten";
				}
				else if (!(learned & (1L << (spell % 32))))
				{
					comment = " unknown";
				}
				else if (!(worked & (1L << (spell % 32))))
				{
					comment = " untried";
				}
				if (ExtToUtf_SetArrayValueString(t, "info", comment + 1)
					!= TCL_OK)
				{
					return TCL_ERROR;
				}
			}			
			break;

		case IDX_MEMORY: /* memory */

			/* Required number of arguments */
		    if (objC != 4)
		    {
				Tcl_WrongNumArgs(interp, infoCmd->depth + 2, objv, "bookNum spellIndex");
				return TCL_ERROR;
		    }

		    /* Get the book number */
			if (GetBookFromObj(interp, objV[2], &k_idx) != TCL_OK)
			{
				return TCL_ERROR;
			}

		    /* Get the spell index */
			if (Tcl_GetIntFromObj(interp, objV[3], &spell) != TCL_OK)
			{
				return TCL_ERROR;
			}

			/* Verify the spell index */
			if ((spell < 0) || (spell >= PY_MAX_SPELLS))
			{
				/* REPORT */
				return TCL_ERROR;
			}

			/* Get the sval */
			sval = k_info[k_idx].sval;

			/* Find the first spell in the book */
#if defined(ANGBANDTK) || defined(KANGBANDTK)		
			first = -1;
			for (i = 0; i < PY_MAX_SPELLS; i++)
			{
				if (i < 32)
				{
					flags = spell_flags[mp_ptr_spell_type][sval][0];
				}
				else
				{
					flags = spell_flags[mp_ptr_spell_type][sval][1];
				}

				if (flags & (1L << (i % 32)))
				{
					first = i;
					break;
				}
			}
#endif
#if defined(OANGBANDTK)
			first = mp_ptr->book_start_index[sval];
#endif
			if (first == -1)
			{
				/* REPORT */
				return TCL_ERROR;
			}

			/* Get the spell index */
			spell += first;

			if (spell < 32)
			{
#if defined(ANGBANDTK) || defined(KANGBANDTK)		
				flags = spell_flags[mp_ptr_spell_type][sval][0];
#endif
			}
			else
			{
#if defined(ANGBANDTK) || defined(KANGBANDTK)		
				flags = spell_flags[mp_ptr_spell_type][sval][1];
#endif
			}

			/* Check for this spell */
#if defined(ANGBANDTK) || defined(KANGBANDTK)		
			if (flags & (1L << (spell % 32)))
#endif
#if defined(OANGBANDTK)
			if ((spell >= first) && (spell < mp_ptr->book_start_index[sval + 1]))
#endif
			{
				const magic_type *s_ptr;

				/* Access the spell */
				s_ptr = &mp_ptr->info[spell];

				/* Skip illegible spells */
				if (s_ptr->slevel >= 99)
				{
					/* REPORT */
					return TCL_ERROR;
				}

				/* Is the spell known? */
				if (spell_okay(spell, TRUE))
				{
#if defined(ANGBANDTK) || defined(KANGBANDTK)		
					/* Only OAngband 0.4.0 has spell descriptions */
#endif
#if defined(OANGBANDTK)
					StaticResult(interp, (char *) spell_tips[s_ptr->index]);
#endif
				}
				else
				{
					Tcl_ResetResult(interp);
				}
			}

			break;
	}

	/* Success */
	return TCL_OK;
}

#endif /* ANGBANDTK, KANGBANDTK, OANGBANDTK */

#if defined(ZANGBANDTK)

int
objcmd_spell(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	static CONST char *cmdOptions[] = {"find", "info", "memory", NULL};
	enum {IDX_FIND, IDX_INFO, IDX_MEMORY} option;
	int index;

	Tcl_Obj *listObjPtr;
    int k_idx, realm, sval, tval, spell, i = -1, first;
	char *t;
	u32b forgotten, learned, worked;

	/* Default to ignoring known */
	int request_known = 0, match_known = 0;

	/* Default to ignoring spell_okay() */
	int request_tester = 0, match_tester = 0;

	/* Default to ignoring mana */
	int request_vmana = 0, match_vmana = 0;

	/* Required number of arguments */
	if (objC < 2)
	{
		/* Set the error */
		Tcl_WrongNumArgs(interp, infoCmd->depth + 1, objv, "option ?arg ...?");

		/* Failure */
		return TCL_ERROR;
	}

	/* Get the requested option */
    if (Tcl_GetIndexFromObj(interp, objV[1], cmdOptions, "option", 0, 
		(int *) &option) != TCL_OK)
    {
		return TCL_ERROR;
    }

	/* The character doesn't exist yet */
	if (!character_generated)
	{
		/* Set the error */
		StaticResult(interp, "character has not been generated yet");

		/* Failure */
		return TCL_ERROR;
	}

	/* The character is not a magic user */
	if (!p_ptr->realm1 && !p_ptr->realm2)
	{
		/* Set the error */
		StaticResult(interp, "character cannot read books");

		/* Failure */
		return TCL_ERROR;
	}

	switch (option)
	{
		case IDX_FIND: /* find */

			/* Required number of arguments */
			if (objC < 3)
			{
				Tcl_WrongNumArgs(interp, infoCmd->depth + 2, objv, "bookNum");
				return TCL_ERROR;
			}

			/* Get the book number */
			if (GetBookFromObj(interp, objV[2], &k_idx) != TCL_OK)
			{
				return TCL_ERROR;
			}

			/* Get the tval */
			tval = k_info[k_idx].tval;

			/* Get the sval */
			sval = k_info[k_idx].sval;

			/* Get the realm */
			if (tval == p_ptr->realm2 + (TV_LIFE_BOOK - 1))
			{
				realm = p_ptr->realm2;
				forgotten = spell_forgotten2;
				learned = spell_learned2;
				worked = spell_worked2;
			}
			else
			{
				realm = p_ptr->realm1;
				forgotten = spell_forgotten1;
				learned = spell_learned1;
				worked = spell_worked1;
			}

			/* Scan arguments for options */
			for (i = 3; i < objC; )
			{
				static CONST char *cmdOptions[] = {"-known", "-tester",
					"-verify_mana", NULL};

				/* Get the sub-option */
				if (Tcl_GetIndexFromObj(interp, objV[i], cmdOptions, "option",
					0, &index) != TCL_OK)
				{
					return TCL_ERROR;
				}

				switch (index)
				{
					case 0: /* -known */
						if (Tcl_GetBooleanFromObj(interp, objV[i+1],
							&match_known) != TCL_OK)
						{
							return TCL_ERROR;
						}
						request_known = 1;
						i += 2;
						break;

					case 1: /* -tester */
						if (Tcl_GetBooleanFromObj(interp, objV[i+1],
							&match_tester) != TCL_OK)
						{
							return TCL_ERROR;
						}
						request_tester = 1;
						i += 2;
						break;

					case 2: /* -verify_mana */
						if (Tcl_GetBooleanFromObj(interp, objV[i+1],
							&match_vmana) != TCL_OK)
						{
							return TCL_ERROR;
						}
						request_vmana = 1;
						i += 2;
						break;
				}
			}

			/*
			 * If we are not casting or studying a spell, then there is
			 * no need to check spell_okay().
			 */
			if ((command_cmd != 'G') && (command_cmd != 'm'))
			{
				request_tester = 0;
			}

			/*
			 * The list of indexes returned by this command are
			 * book-relative. That is, the first spell in a book
			 * is index 0, the next is 1, and so on, except for those
			 * spells which are "illegible".
			 */
			i = -1;

			/* Return spell list */
			listObjPtr = Tcl_NewListObj(0, NULL);

			/* Extract spells */
			for (spell = 0; spell < 32; spell++)
			{
				/* Check for this spell */
				if (fake_spell_flags[sval] & (1L << spell))
				{
					magic_type *s_ptr;

					/* Zero-based spell index in this book */
					++i;

					/* Access the spell */
					s_ptr = &mp_ptr->info[realm-1][spell];

					/* Skip illegible spells */
					if (s_ptr->slevel >= 99) continue;

					/* Return only known spells */
					if (request_known)
					{
						bool known = (learned & (1L << spell)) != 0;

						if (known != match_known)
						{
							continue;
						}
					}

					/* Return only "okay" spells */
					if (request_tester && match_tester)
					{
						bool known = command_cmd != 'G';
						if (!spell_okay(spell, known, realm - 1))
						{
							continue;
						}
					}

					/* Return only spells we have enough mana to cast.
					 * If verify_mana is TRUE, we can always try to cast. */
					if (request_vmana && match_vmana)
					{
						bool known = (learned & (1L << spell)) != 0;
						if (known && !verify_mana && (s_ptr->smana > p_ptr->csp))
						{
							continue;
						}
					}

					/* Append this spellindex to the list */
					Tcl_ListObjAppendElement(interp, listObjPtr,
						Tcl_NewIntObj(i));
				}
			}

			/* Return the list of spell indexes */
			Tcl_SetObjResult(interp, listObjPtr);
			break;

		case IDX_INFO: /* info */

			/* Required number of arguments */
		    if (objC < 5)
		    {
				Tcl_WrongNumArgs(interp, infoCmd->depth + 2, objv, "bookNum spellIndex arrayName");
				return TCL_ERROR;
		    }

		    /* Get the book number */
			if (GetBookFromObj(interp, objV[2], &k_idx) != TCL_OK)
			{
				return TCL_ERROR;
			}

		    /* Get the spell index */
			if (Tcl_GetIntFromObj(interp, objV[3], &spell) != TCL_OK)
			{
				return TCL_ERROR;
			}

			/* Verify the spell index */
			if ((spell < 0) || (spell >= 32))
			{
				/* REPORT */
				return TCL_ERROR;
			}

			/* Get the variable name */
			t = Tcl_GetStringFromObj(objV[4], NULL);

			/* Get the tval */
			tval = k_info[k_idx].tval;

			/* Get the sval */
			sval = k_info[k_idx].sval;

			/* Get the realm */
			if (tval == p_ptr->realm2 + (TV_LIFE_BOOK - 1))
			{
				realm = p_ptr->realm2;
				forgotten = spell_forgotten2;
				learned = spell_learned2;
				worked = spell_worked2;
			}
			else
			{
				realm = p_ptr->realm1;
				forgotten = spell_forgotten1;
				learned = spell_learned1;
				worked = spell_worked1;
			}

			/* Find the first spell in the book */
			first = -1;
			for (i = 0; i < 32; i++)
			{
				if (fake_spell_flags[sval] & (1L << i))
				{
					first = i;
					break;
				}
			}

			if (first == -1)
			{
				/* REPORT */
				return TCL_ERROR;
			}

			/* Get the spell index */
			spell += first;

			/* Check for this spell */
			if (fake_spell_flags[sval] & (1L << spell))
			{
				magic_type *s_ptr;
				char info[80], *comment;

				/* Access the spell */
				s_ptr = &mp_ptr->info[realm-1][spell];

				/* Skip illegible spells */
				if (s_ptr->slevel >= 99)
				{
					/* REPORT */
					return TCL_ERROR;
				}

				if (SetArrayValueChar(t, "char", (char) I2A(spell - first))
					!= TCL_OK)
				{
					return TCL_ERROR;
				}
				if (ExtToUtf_SetArrayValueString(t, "name",
					(char *) spell_names[realm-1][spell]) != TCL_OK)
				{
					return TCL_ERROR;
				}
				if (SetArrayValueLong(t, "level", s_ptr->slevel) != TCL_OK)
				{
					return TCL_ERROR;
				}
				if (SetArrayValueLong(t, "mana", s_ptr->smana) != TCL_OK)
				{
					return TCL_ERROR;
				}
				if (SetArrayValueLong(t, "chance",
					spell_chance(spell, realm - 1)) != TCL_OK)
				{
					return TCL_ERROR;
				}
				if (SetArrayValueLong(t, "known",
					(learned & (1L << spell)) != 0) != TCL_OK)
				{
					return TCL_ERROR;
				}

				spell_info(info, spell, realm-1);
				if (!info[0]) (void) strcpy(info, " ");
				comment = info;

				/* Describe the status of the spell */
				if (forgotten & (1L << spell))
				{
					comment = " forgotten";
				}
				else if (!(learned & (1L << spell)))
				{
					comment = " unknown";
				}
				else if (!(worked & (1L << spell)))
				{
					comment = " untried";
				}
				if (ExtToUtf_SetArrayValueString(t, "info", comment + 1)
					!= TCL_OK)
				{
					return TCL_ERROR;
				}
			}			
			break;

		case IDX_MEMORY: /* memory */
			/* Only OAngband 0.4.0 has spell descriptions */
			Tcl_ResetResult(interp);
			break;
	}

	/* Success */
	return TCL_OK;
}

#endif /* ZANGBANDTK */

/*
 * Initializes lists of keywords for various things.
 */
static void init_keyword_misc(void)
{
	int i;

	/* A list of gender names */
	C_MAKE(keyword_gender, MAX_SEXES + 1, cptr);
	for (i = 0; i < MAX_SEXES; i++)
	{
		keyword_gender[i] = (char *) sex_info[i].title;
	}
	keyword_gender[MAX_SEXES] = NULL;

	/* A list of class names */
	C_MAKE(keyword_class, MAX_CLASS + 1, cptr);
	for (i = 0; i < MAX_CLASS; i++)
	{
		keyword_class[i] = (char *) class_info[i].title;
	}
	keyword_class[MAX_CLASS] = NULL;

	/* A list of race names */
	C_MAKE(keyword_race, MAX_P_IDX + 1, cptr);
	for (i = 0; i < MAX_P_IDX; i++)
	{
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)
		keyword_race[i] = (char *) p_name + p_info[i].name;
#endif
#if defined(ZANGBANDTK)
		keyword_race[i] = (char *) race_info[i].title;
#endif
	}
	keyword_race[MAX_P_IDX] = NULL;
}

/*
 * objcmd_home --
 *
 * 		home count $townNum
 * 		home info $townNum $index
 * 		home memory $townNum $index
 */

int
objcmd_home(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	static CONST char *cmdOptions[] = {"count", "info", "memory", "name",
		"max_towns",NULL};
	enum {IDX_COUNT, IDX_INFO, IDX_MEMORY, IDX_NAME, IDX_MAX_TOWNS} option;

	char *buffer, *varName;
	int item, n, town_idx;
	object_type *o_ptr;
	long length;
	store_type *st_ptr;
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)
	int max_towns = 1 + 1;
#endif


	/* Get requested option */
    if (Tcl_GetIndexFromObj(interp, objV[1], cmdOptions, "option", 0, 
		(int *) &option) != TCL_OK)
	{
		return TCL_ERROR;
    }

	/* Get the town number */
	if (Tcl_GetIntFromObj(interp, objV[2], &town_idx) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* Verify the town number */
	if ((town_idx < 1) || (town_idx >= max_towns))
	{
		/* Set the error */
		Tcl_SetResult(interp,
			format("bad town number \"%d\": must be from 1 to %d",
			town_idx, max_towns - 1), TCL_VOLATILE);

		/* Failure */
		return TCL_ERROR;
	}

#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)

	st_ptr = &store[STORE_HOME];

#endif /* A, K, O */

#if defined(ZANGBANDTK)

	/* Get the store info */
	st_ptr = &town[town_idx].store[STORE_HOME];

#endif /* Z */

	switch (option)
	{
		case IDX_COUNT: /* count */
			IntResult(interp, (int) st_ptr->stock_num);
			break;

		case IDX_INFO: /* info */

			/* Required number of arguments */
		    if (objC != 5)
		    {
				Tcl_WrongNumArgs(interp, infoCmd->depth + 2, objv, "town item arrayName");
				return TCL_ERROR;
		    }

			if (Tcl_GetIntFromObj(interp, objV[3], &item) != TCL_OK)
			{
				return TCL_ERROR;
			}
			if ((item < 0) || (item >= st_ptr->stock_num))
			{
				goto bad_index;
			}

			/* Get the array variable name to dump results in */
			varName = Tcl_GetStringFromObj(objV[4], NULL);

			/* Get item info */
			o_ptr = &st_ptr->stock[item];

			n = storedata.store_num;
			storedata.store_num = STORE_HOME;
			if (dump_object_info_store(varName, o_ptr, item) != TCL_OK)
			{
				storedata.store_num = n;
				return TCL_ERROR;
			}
			storedata.store_num = n;
			break;

		case IDX_MEMORY: /* memory */

			/* Required number of arguments */
		    if (objC != 4)
		    {
				Tcl_WrongNumArgs(interp, infoCmd->depth + 2, objv, "town item");
				return TCL_ERROR;
		    }

			if (Tcl_GetIntFromObj(interp, objV[3], &item) != TCL_OK)
			{
				return TCL_ERROR;
			}

			if ((item < 0) || (item >= st_ptr->stock_num))
			{
				goto bad_index;
			}

			/* Get object info */
			o_ptr = &st_ptr->stock[item];

			buffer = Tcl_Alloc(5 * 1024L);
			length = angtk_describe_object(o_ptr, buffer, FALSE);
			Tcl_SetObjResult(interp, ExtToUtf_NewStringObj(buffer, length));
			Tcl_Free(buffer);
			break;

		case IDX_NAME: /* name */
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)
			ExtToUtf_SetResult(interp, "Home");
#endif
#if defined(ZANGBANDTK)

			/* Hack -- The town names are not set until this is done... */
			if (town[1].name[0] == '\0')
			{
				/* Not sure if this is safe */
				process_dungeon_file("w_info.txt", 0, 0, max_wild_y, max_wild_x);
			}

			ExtToUtf_SetResult(interp, town[town_idx].name);
#endif
			break;

		case IDX_MAX_TOWNS: /* max_towns */
			IntResult(interp, max_towns);
			break;
	}

	return TCL_OK;

bad_index:
	Tcl_SetResult(interp,
		format("bad home index \"%d\": must be from 0 to %d",
		item, st_ptr->stock_num - 1), TCL_VOLATILE);
	return TCL_ERROR;
}

/* This is an information block set in store.c */
ANG_STORAGE_CLASS _storedata storedata = {0};

/* store.c */
extern char *store_cost(object_type *o_ptr);

/*
 * Dump object info into an array variable. Array names are:
 *		char, number, name, weight, tval, sval, icon, cost
 */
int dump_object_info_store(char *varName, object_type * o_ptr, int index)
{
	char buf[128], o_name[O_NAME_MAX];
	cptr cstr;

	/* Description */
	if (storedata.store_num == STORE_HOME)
	{
		object_desc(o_name, o_ptr, TRUE, 3);
	}
	else
	{
		object_desc_store(o_name, o_ptr, TRUE, 3);
	}
	if (ExtToUtf_SetArrayValueString(varName, "name", o_name) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* Char */
	if (SetArrayValueChar(varName, "char", I2A(index)) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* Number */
	if (SetArrayValueLong(varName, "number", o_ptr->number) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* Weight */
	if (SetArrayValueLong(varName, "weight", o_ptr->weight) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* Tval */
	(void) angtk_tval_string(&cstr, o_ptr->tval);
	if (SetArrayValueString(varName, "tval", (char *) cstr) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* Sval */
	if (SetArrayValueLong(varName, "sval", o_ptr->sval) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* K_idx */
	if (SetArrayValueLong(varName, "k_idx", o_ptr->k_idx) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* Icon */
	if (SetArrayValueString(varName, "icon", assign_print_object(buf, o_ptr))
		!= TCL_OK)
	{
		return TCL_ERROR;
	}

	/* Cost */
	if (SetArrayValueString(varName, "cost", store_cost(o_ptr)) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* Success */
	return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * objcmd_store --
 *
 *	Implements the "store" script command.
 * 	Syntax:
 *		store count     -- number of items
 *		store info      -- info about an item
 *      store ishome    -- return 1 if in the Home
 *		store memory    -- item memory
 *		store ownername -- name of owner
 *		store ownerrace -- race of owner
 *		store purchase <index> <quantity> -- purchase index'th item
 *		store purse     -- max purse
 *		store storename -- building name
 *		store worthless -- item is worthless
 *
 *--------------------------------------------------------------
 */

int
objcmd_store(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	static CONST char *cmdOptions[] = {"count", "info", "ishome", "memory",
		"ownername", "ownerrace", "purse", "storename",
		"worthless", "shopping", NULL};
	enum {IDX_COUNT, IDX_INFO, IDX_ISHOME, IDX_MEMORY,
		IDX_OWNERNAME, IDX_OWNERRACE, IDX_PURSE, IDX_STORENAME,
		IDX_WORTHLESS, IDX_SHOPPING} option;

	char *buffer, *t, *varName;
	int item;
	object_type *o_ptr;
	long length;

	/* Required number of arguments */
    if (objC < 2)
    {
		Tcl_WrongNumArgs(interp, infoCmd->depth + 1, objv, "option ?arg ...?");
		return TCL_ERROR;
    }

	/* Get requested option */
    if (Tcl_GetIndexFromObj(interp, objV[1], cmdOptions, "option", 0, 
		(int *) &option) != TCL_OK)
	{
		return TCL_ERROR;
    }

	if (option == IDX_SHOPPING)
	{
		Tcl_SetObjResult(interp, Tcl_NewBooleanObj(storedata.shopping));
		return TCL_OK;
	}

	if (!storedata.shopping)
	{
		Tcl_SetResult(interp, "character isn't shopping", TCL_VOLATILE);
		return TCL_ERROR;
	}

	switch (option)
	{
		case IDX_COUNT: /* count */
			IntResult(interp, (int) storedata.st_ptr->stock_num);
			break;

		case IDX_INFO: /* info */
			if (objC != 4)
			{
				Tcl_WrongNumArgs(interp, infoCmd->depth + 2, objv, "index arrayName");
				return TCL_ERROR;
			}
			if (Tcl_GetIntFromObj(interp, objV[2], &item) != TCL_OK)
			{
				return TCL_ERROR;
			}
			if ((item < 0) || (item >= storedata.st_ptr->stock_num))
			{
				goto bad_index;
			}

			/* Get the array variable name to dump results in */
			varName = Tcl_GetStringFromObj(objV[3], NULL);

			/* Get item info */
			o_ptr = &storedata.st_ptr->stock[item];

			if (dump_object_info_store(varName, o_ptr, item) != TCL_OK)
			{
				return TCL_ERROR;
			}
			break;

		case IDX_ISHOME: /* ishome */
			BooleanResult(interp, storedata.store_num == STORE_HOME);
			break;

		case IDX_MEMORY: /* memory */

			if (objC != 3)
			{
				Tcl_WrongNumArgs(interp, infoCmd->depth + 2, objv, "index");
				return TCL_ERROR;
			}
			if (Tcl_GetIntFromObj(interp, objV[2], &item) != TCL_OK)
			{
				return TCL_ERROR;
			}

			if ((item < 0) || (item >= storedata.st_ptr->stock_num))
			{
				goto bad_index;
			}

			/* Get object info */
			o_ptr = &storedata.st_ptr->stock[item];

			buffer = Tcl_Alloc(5 * 1024L);
			length = angtk_describe_object(o_ptr, buffer,
				storedata.store_num != STORE_HOME);
			Tcl_SetObjResult(interp, ExtToUtf_NewStringObj(buffer, length));
			Tcl_Free(buffer);
			break;

#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)
		case IDX_OWNERNAME: /* ownername */
			ExtToUtf_SetResult(interp,
				(char *) b_name + storedata.ot_ptr->owner_name);
			break;

		case IDX_OWNERRACE: /* ownerrace */
			ExtToUtf_SetResult(interp,
				(char *) p_name + p_info[storedata.ot_ptr->owner_race].name);
			break;
#endif
#if defined(ZANGBANDTK)
		case IDX_OWNERNAME: /* ownername */
			ExtToUtf_SetResult(interp,
				(char *) storedata.ot_ptr->owner_name);
			break;

		case IDX_OWNERRACE: /* ownerrace */
			ExtToUtf_SetResult(interp,
				(char *) race_info[storedata.ot_ptr->owner_race].title);
			break;
#endif

		case IDX_PURSE: /* purse */
			LongResult(interp, storedata.ot_ptr->max_cost);
			break;

		case IDX_STORENAME: /* storename */
			t = (char *) (f_name + f_info[FEAT_SHOP_HEAD + storedata.store_num].name);
			ExtToUtf_SetResult(interp, t);
			break;

		case IDX_WORTHLESS: /* worthless */
		{
			int worthless;

			if (objC != 3)
			{
				Tcl_WrongNumArgs(interp, infoCmd->depth + 2, objv, "index");
				return TCL_ERROR;
			}
			if (Tcl_GetIntFromObj(interp, objV[2], &item) != TCL_OK)
			{
				return TCL_ERROR;
			}
			if ((item < 0) || (item >= storedata.st_ptr->stock_num))
			{
				goto bad_index;
			}

			/* Get object info */
			o_ptr = &storedata.st_ptr->stock[item];

			/* Is it worthless? */
			worthless = (object_value(o_ptr) < 1) ||
				((o_ptr->tval == TV_CHEST) && (o_ptr->pval == 0))
#ifdef TNB_SQUELCH
				|| object_squelch_p(o_ptr)
#endif
			;
			BooleanResult(interp, worthless);
			break;
		}

		case IDX_SHOPPING: /* Avoid compiler warning */
			break;
	}

	return TCL_OK;

bad_index:
	Tcl_SetResult(interp,
		format("bad store index \"%d\": must be from 0 to %d",
		item, storedata.st_ptr->stock_num - 1), TCL_VOLATILE);
	return TCL_ERROR;
}

#if 0

/*
 * As one hack, I have Term_fresh() calling the "update" Tcl command. But
 * that get's called during startup before angtk_init() is called. So
 * I add this variable to fix my hack.
 */

static int Tcl_Initialized = 0;

#endif

/*
 *--------------------------------------------------------------
 *
 * angtk_eval --
 *
 *	Eval() a command with arguments.
 *
 *--------------------------------------------------------------
 */

void angtk_eval(char *command, ...)
{
	char *s = command;
	va_list vp;
	int objc = 0;
	Tcl_Obj *objv[40];
	int i, result;

#if 0
	/* Oops. Tcl isn't initialized yet */
	if (!Tcl_Initialized) return;
#endif

	/* Start processing variable argument list */
	va_start(vp, command);

	/* Process each string argument */
	while (s)
	{
		/* Append a new string object to the command object */
		/* XXX Some args are already ASCII, safe to translate? */
		objv[objc++] = ExtToUtf_NewStringObj(s, -1);
		Tcl_IncrRefCount(objv[objc - 1]);

		/* Get the next string argument */
		s = va_arg(vp, char *);
	}

	/* Finish processing variable argument list */
	va_end(vp);

	result = Tcl_EvalObjv(g_interp, objc, objv, TCL_EVAL_GLOBAL);

	for (i = 0; i < objc; i++)
	{
		Tcl_DecrRefCount(objv[i]);
	}

    if (result == TCL_ERROR)
    {
    	/* Report the error */
		Tcl_AddErrorInfo(g_interp,
			"\n    (inside angtk_eval)");
		Tcl_BackgroundError(g_interp);
    }
}

/*
 *--------------------------------------------------------------
 *
 * angtk_display_info --
 *
 *	Calls "angband_display info TITLE LIST" where LIST is a given
 *	array of strings, and TITLE is displayed in the window.
 *	Used for "Self Knowledge" and others.
 *
 *--------------------------------------------------------------
 */

static Tcl_Obj *s_info_list = NULL;

void angtk_display_info_init(void)
{
	s_info_list = Tcl_NewListObj(0, NULL);
}

void angtk_display_info_append(cptr s)
{
	Tcl_ListObjAppendElement(g_interp, s_info_list,
		ExtToUtf_NewStringObj(s, -1));
}

void angtk_display_info_done(cptr title)
{
	angtk_display_info_aux((char *) title, s_info_list);
}

void angtk_display_info_aux(char *title, Tcl_Obj *listObjPtr)
{
	int i, objc = 0, result;
	Tcl_Obj *objv[10];

	objv[objc++] = Tcl_NewStringObj("angband_display", -1);
	objv[objc++] = Tcl_NewStringObj("info", -1);
	objv[objc++] = Tcl_NewStringObj("show", -1);
	objv[objc++] = ExtToUtf_NewStringObj(title, -1);
	objv[objc++] = listObjPtr;

	for (i = 0; i < objc; i++)
		Tcl_IncrRefCount(objv[i]);

	result = Tcl_EvalObjv(g_interp, objc, objv, TCL_EVAL_GLOBAL);

	for (i = 0; i < objc; i++)
		Tcl_DecrRefCount(objv[i]);

    if (result == TCL_ERROR)
    {
    	/* Report the error */
		Tcl_AddErrorInfo(g_interp,
			"\n    (inside angtk_display_info_aux)");
		Tcl_BackgroundError(g_interp);
    }
}

void angtk_display_info(char *title, char **info, int count)
{
	int i, objc = 0;
	Tcl_Obj *objv[256];

	/* Paranoia */
	if (count > 256)
	{
		plog_fmt("angtk_display_info(): too many strings");
		count = 256;
	}

	/* Check each string */
	for (i = 0; i < count; i++)
	{
		/* Create a new string object */
		objv[objc++] = ExtToUtf_NewStringObj(info[i], -1);
	}

	/* Pass the title and list of strings to Tcl */
	angtk_display_info_aux(title, Tcl_NewListObj(objc, objv));
}

#ifdef ALLOW_BORG

#include "stubs.h"

/* This is the path of the author-specific directory */
static char *borg_path = NULL;

/* This is the path of the DLL */
static char *borg_dll = NULL;

/* This is the address of the command in the DLL */
void (*do_cmd_borg)(void) = NULL;

/* (borg) dll ?path? */
int
objcmd_borg_dll(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	/* Set the path */
	if (objC == 2)
	{
		char *fullName, *t;
		Tcl_DString extDString;

		/* Get the DLL path */
		t = Tcl_GetString(objV[1]);

		/* Translate the file name */
		fullName = UtfToExt_TranslateFileName(interp, t, &extDString);
		if (fullName == NULL)
		{
			/* Note: Tcl_DStringFree() is called for us */
			return TCL_ERROR;
		}

		/* Free the old, remember the new */
		if (borg_dll != NULL) string_free(borg_dll);
		borg_dll = (char *) string_make(fullName);

		Tcl_DStringFree(&extDString);

		return TCL_OK;
	}

	/* Return the current path */
	if (borg_dll)
	{
		Tcl_SetObjResult(interp, ExtToUtf_NewStringObj(borg_dll, -1));
	}

	return TCL_OK;
}

/* (borg) path ?path? */
int
objcmd_borg_path(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	/* Set the path */
	if (objC == 2)
	{
		char *fullName, *t;
		Tcl_DString extDString;

		/* Get the path */
		t = Tcl_GetString(objV[1]);

		/* Translate the file name */
		fullName = UtfToExt_TranslateFileName(interp, t, &extDString);
		if (fullName == NULL)
		{
			/* Note: Tcl_DStringFree() is called for us */
			return TCL_ERROR;
		}

		/* Free the old, remember the new */
		if (borg_path != NULL) string_free(borg_path);
		borg_path = (char *) string_make(fullName);

		Tcl_DStringFree(&extDString);

		return TCL_OK;
	}

	/* Return the current path */
	if (borg_path)
	{
		Tcl_SetObjResult(interp, ExtToUtf_NewStringObj(borg_path, -1));
	}

	return TCL_OK;
}

static CommandInit borgCmdInit[] = {
		{1, "setting", 0, 0, (char *) NULL, objcmd_setting, (ClientData) 0},
	{0, (char *) NULL, 0, 0, (char *) NULL, (Tcl_ObjCmdProc *) NULL, (ClientData) 0}
};

void borg_init(void)
{
	ShLibRef shLib;
	struct stub {
		void *(*Borg_GetSettings)(void);
		int (*Borg_Init)(Tcl_Interp *interp, char *path);
		void (*do_cmd_borg)(void);
	} stub;
	t_stub init[] = {
		{STUB_DESC(Borg_GetSettings, struct stub)},
		{STUB_DESC(Borg_Init, struct stub)},
		{STUB_DESC(do_cmd_borg, struct stub)},
		{NULL, 0, 0}
	};

	/* Do this first so the user's choice of DLL can be specified */
	angtk_eval("angband_borg", "preinit", NULL);

	/* Require a DLL */
	if (borg_dll == NULL)
	{
		plog_fmt("Borg shared-library not specified");
		return;
	}

	/* Require a path */
	if (borg_path == NULL)
	{
		plog_fmt("Borg path not specified");
		return;
	}

	/* Try to load the DLL */
	shLib = ShLib_Load(borg_dll);
	if (shLib == NULL)
	{
		plog_fmt("error loading %s", borg_dll);
		return;
	}

	/* Get the funcs */
	if (Stub_Load(shLib, init, &stub) == -1)
	{
		plog_fmt("%s: %s", borg_dll, Stub_Error());
		ShLib_Free(shLib);
		return;
	}

	/* Call the initializer */
	if ((*stub.Borg_Init)(g_interp, borg_path) != TCL_OK)
	{
		return;
	}

	/* This is the func called by Angband */
	do_cmd_borg = stub.do_cmd_borg;

	/*
	 * Mega-Hack -- After the Borg DLL is loaded, we ask it for a list
	 * of settings. After using this information to initialize a new
	 * SettingGroup (see setting.c), we install the "setting" subcommand
	 * of the "borg" command, with ClientData of the SettingGroupToken
	 * we created.
	 */
	{
		extern ClientData borg_init_settings(void *setting);

		/*
		 * Call the DLL to get a list of setting info, and initialize
		 * the author-specific settings.
		 */
		ClientData clientData = borg_init_settings((*stub.Borg_GetSettings)());

		/* Get info for the Tcl "borg" command */
		char *names[] = {"borg", NULL};
		CommandInfo *infoCmd = CommandInfo_GetInfo(g_interp, names);
		if (infoCmd == NULL)
		{
			plog_fmt("can't get info for borg() command:\n%s",
				Tcl_GetStringResult(g_interp));
		}

		/*
		 * Tell the "borg setting" subcommand which SettingGroupToken
		 * to use.
		 */
		borgCmdInit[0].clientData = clientData;

		/* Add the "borg setting" subcommand */
		(void) CommandInfo_Init(g_interp, borgCmdInit, infoCmd);
	}

	/* Final script initialization */
	angtk_eval("angband_borg", "postinit", NULL);
}

#endif /* ALLOW_BORG */

void HandleError(void)
{
	char message[1024], path[1024];
	CONST char *errorInfo;
	FILE *fp;

	/* Dump the stack to errors.txt */
	path_build(path, 1024, ANGBAND_DIR_COMMON, "errors.txt");
	fp = fopen(path, "a");
	if (fp != NULL)
	{
		errorInfo = Tcl_GetVar(g_interp, "errorInfo", TCL_GLOBAL_ONLY);
		fprintf(fp, "***** (inside HandleError)\n\n%s\n\n", errorInfo);
		fclose(fp);
	}

	/* Display a message and quit */
	sprintf(message, "The following error occurred:\n\n%s\n\n",
		Tcl_GetStringResult(g_interp));
	strcat(message, "Please send the errors.txt file to omnibandtk@shaw.ca");
	quit(message);
}

static CommandInit commandInit[] = {
	{0, "angband", 0, 0, (char *) NULL, (Tcl_ObjCmdProc *) NULL, (ClientData) 0},
		{1, "a_info", 0, 0, (char *) NULL, (Tcl_ObjCmdProc *) NULL, (ClientData) 0},
			{2, "find", 1, 0, (char *) NULL, objcmd_ARRAY_find, (ClientData) "artifact_type"},
			{2, "info", 3, 0, "a_idx option ?args ...?", objcmd_ARRAY_info, (ClientData) "artifact_type"},
			{2, "max", 1, 1, (char *) NULL, objcmd_ARRAY_max, (ClientData) "artifact_type"},
			{2, "set", 2, 4, "a_idx ?field? ?value?", objcmd_ARRAY_set, (ClientData) "artifact_type"},
		{1, "f_info", 0, 0, (char *) NULL, (Tcl_ObjCmdProc *) NULL, (ClientData) 0},
			{2, "find", 1, 0, (char *) NULL, objcmd_ARRAY_find, (ClientData) "feature_type"},
			{2, "info", 3, 0, "f_idx option ?args ...?", objcmd_ARRAY_info, (ClientData) "feature_type"},
			{2, "max", 1, 1, (char *) NULL, objcmd_ARRAY_max, (ClientData) "feature_type"},
			{2, "set", 2, 4, "f_idx ?field? ?value?", objcmd_ARRAY_set, (ClientData) "feature_type"},
		{1, "k_info", 0, 0, (char *) NULL, (Tcl_ObjCmdProc *) NULL, (ClientData) 0},
			{2, "find", 1, 0, (char *) NULL, objcmd_ARRAY_find, (ClientData) "object_kind"},
			{2, "info", 3, 0, "k_idx option ?args ...?", objcmd_ARRAY_info, (ClientData) "object_kind"},
			{2, "max", 1, 1, (char *) NULL, objcmd_ARRAY_max, (ClientData) "object_kind"},
			{2, "set", 2, 4, "k_idx ?field? ?value?", objcmd_ARRAY_set, (ClientData) "object_kind"},
		{1, "r_info", 0, 0, (char *) NULL, (Tcl_ObjCmdProc *) NULL, (ClientData) 0},
			{2, "find", 1, 0, (char *) NULL, objcmd_ARRAY_find, (ClientData) "monster_race"},
			{2, "info", 3, 0, "r_idx option ?args ...?", objcmd_ARRAY_info, (ClientData) "monster_race"},
			{2, "max", 1, 1, (char *) NULL, objcmd_ARRAY_max, (ClientData) "monster_race"},
			{2, "set", 2, 4, "r_idx ?field? ?value?", objcmd_ARRAY_set, (ClientData) "monster_race"},
		{1, "m_list", 0, 0, (char *) NULL, (Tcl_ObjCmdProc *) NULL, (ClientData) 0},
			{2, "find", 1, 0, (char *) NULL, objcmd_ARRAY_find, (ClientData) "monster_type"},
			{2, "info", 3, 0, "m_idx option ?args ...?", objcmd_ARRAY_info, (ClientData) "monster_type"},
			{2, "max", 1, 1, (char *) NULL, objcmd_ARRAY_max, (ClientData) "monster_type"},
			{2, "set", 2, 4, "m_idx ?field? ?value?", objcmd_ARRAY_set, (ClientData) "monster_type"},
		{1, "o_list", 0, 0, (char *) NULL, (Tcl_ObjCmdProc *) NULL, (ClientData) 0},
			{2, "find", 1, 0, (char *) NULL, objcmd_ARRAY_find, (ClientData) "object_type"},
			{2, "info", 3, 0, "o_idx option ?args ...?", objcmd_ARRAY_info, (ClientData) "object_type"},
			{2, "max", 1, 1, (char *) NULL, objcmd_ARRAY_max, (ClientData) "object_type"},
			{2, "set", 2, 4, "o_idx ?field? ?value?", objcmd_ARRAY_set, (ClientData) "object_type"},
		{1, "cave", 0, 0, (char *) NULL, objcmd_cave, (ClientData) 0},
		{1, "equipment", 0, 0, (char *) NULL, objcmd_equipment, (ClientData) 0},
		{1, "game", 0, 0, (char *) NULL, objcmd_game, (ClientData) 0},
		{1, "home", 3, 0, "option town ?args ...?", objcmd_home, (ClientData) 0},
		{1, "info", 0, 0, (char *) NULL, objcmd_info, (ClientData) 0},
		{1, "inkey_flags", 1, 1, (char *) NULL, objcmd_inkey_flags, (ClientData) 0},
		{1, "inkey_options", 1, 1, (char *) NULL, objcmd_inkey_options, (ClientData) 0},
		{1, "inkey_other", 1, 1, (char *) NULL, objcmd_inkey_other, (ClientData) 0},
		{1, "inventory", 0, 0, (char *) NULL, objcmd_inventory, (ClientData) 0},
		{1, "keypress", 2, 2, "string", objcmd_keypress, (ClientData) 0},
		{1, "macro", 0, 0, (char *) NULL, objcmd_macro, (ClientData) 0},
		{1, "message", 0, 0, (char *) NULL, objcmd_message, (ClientData) 0},
		{1, "player", 0, 0, (char *) NULL, objcmd_player, (ClientData) 0},
		{1, "setting", 0, 0, (char *) NULL, objcmd_setting, (ClientData) 0},
		{1, "spell", 0, 0, (char *) NULL, objcmd_spell, (ClientData) 0},
		{1, "store", 0, 0, (char *) NULL, objcmd_store, (ClientData) 0},
		{1, "equipinfo", 3, 3, "slot arrayName", objcmd_equipinfo, (ClientData) 0},
		{1, "inveninfo", 3, 3, "slot arrayName", objcmd_inveninfo, (ClientData) 0},
		{1, "keymap", 0, 0, (char *) NULL, objcmd_keymap, (ClientData) 0},
		{1, "init_icons", 5, 5, "width height depth style", objcmd_init_icons, (ClientData) 0},
		{1, "highscore", 0, 0, (char *) NULL, objcmd_highscore, (ClientData) 0},
		{1, "floor", 0, 0, (char *) NULL, objcmd_floor, (ClientData) 0},
		{1, "system", 0, 0, (char *) NULL, objcmd_system, (ClientData) 0},
		{1, "keycount", 0, 0, (char *) NULL, objcmd_keycount, (ClientData) 0},
#if defined(KANGBANDTK)
		{1, "building", 0, 0, (char *) NULL, objcmd_building, (ClientData) 0},
#endif /* KANGBANDTK */
#if defined(ZANGBANDTK)
		{1, "building", 0, 0, (char *) NULL, objcmd_building, (ClientData) 0},
		{1, "mindcraft", 0, 0, (char *) NULL, objcmd_mindcraft, (ClientData) 0},
		{1, "power", 0, 0, (char *) NULL, objcmd_power, (ClientData) 0},
#endif /* ZANGBANDTK */
#if TK_MINOR_VERSION >= 2 && TK_MINOR_VERSION < 5
	{0, "menuentrystate", 0, 0, (char *) NULL, objcmd_menuentrystate, (ClientData) 0},
#endif /* Tk 8.2-8.4 */
#if TK_MINOR_VERSION >= 2
	{0, "photoget", 0, 0, (char *) NULL, objcmd_photo_get, (ClientData) 0},
	{0, "photomask", 2, 3, "imageDst ?imageMask?", objcmd_photo_mask, (ClientData) 0},
#endif /* Tk 8.2 or 8.3 */
#if TK_MINOR_VERSION == 2
	{0, "photocopy", 7, 7, "imageSrc imageDst srcX srcY width height", objcmd_photo_copy, (ClientData) 0},
#endif /* Tk 8.2 */
#ifdef ALLOW_BORG
	{0, "borg", 0, 0, (char *) NULL, (Tcl_ObjCmdProc *) NULL, (ClientData) 0},
		{1, "dll", 1, 2, "?path?", objcmd_borg_dll, (ClientData) 0},
		{1, "path", 1, 2, "?path?", objcmd_borg_path, (ClientData) 0},
#endif /* ALLOW_BORG */
	{0, "fontdesc", 2, 2, "font", objcmd_fontdesc, (ClientData) 0},
	{0, (char *) NULL, 0, 0, (char *) NULL, (Tcl_ObjCmdProc *) NULL, (ClientData) 0}
};

extern void validate_file(cptr s, cptr fmt);

/*
 * Initialize stuff after Tcl/Tk but before a game is started.
 */
void angtk_init(void)
{
	char path[1024];
	int i;

	/* Hash table for object kind "tval" */
	init_tval_hash();

	/* Settings */
	settings_init();

	for (i = 0; commandInit[i].name; ++i)
	{
		if (streq(commandInit[i].name, "setting"))
		{
			extern void *g_setting;
			commandInit[i].clientData = g_setting;
			break;
		}
	}

	/* Tcl commands */
	(void) CommandInfo_Init(g_interp, commandInit, NULL);

#if 0
	ANGBAND_DIR_COMMON = NULL;
	path_build(path, 1024, ANGBAND_DIR_TK, "boot.tcl");
	validate_file(path, "Could not find a required file:\n\"%s\"\n"
		"Make sure your unzip utility supports long filenames.");	
	if (angtk_eval_file(path) == TCL_ERROR)
	{
		HandleError();
	}
	if (ANGBAND_DIR_COMMON == NULL)
	{
		quit_fmt("Fatal error:\nANGBAND_DIR_COMMON isn't set");
	}
#endif

	/* The "birth" command */
	init_birth();

	/* Cool quasi-event bindings */
	init_bindings();

	(void) init_const(g_interp);

	/* Standard color palette */
	init_palette();

#ifdef ALLOW_REMEMBER_RECALL
	wor_init();
#endif

#if 0
	/* Now Tcl is initialized */
	Tcl_Initialized = 1;
#endif

	/* Source the "startup script" */
	path_build(path, 1024, ANGBAND_DIR_COMMON_TK, "init-startup.tcl");
	validate_file(path, "Could not find a required file:\n\"%s\"\n"
		"Make sure your unzip utility supports long filenames.");	
	if (angtk_eval_file(path) == TCL_ERROR)
	{
		HandleError();
	}
}

/* Final cleanup before exiting */
void angtk_exit(void)
{
#ifdef ALLOW_SOUND
	sound_exit();
#endif

	map_exit();
	CanvasWidget_Exit();
	TreeCtrl_Exit();
	vault_exit();
	icons_exit();
	settings_exit();
	struct_exit();
	widget_exit();

#ifdef USE_PYTHON
	/* Cleanup Python */
	Py_Finalize();
#endif
}

/*
 * Initialize stuff after init_angband().
 */
void angtk_angband_initialized(void)
{
	int i;

	/* Ack! Angband has dynamic p_info array */
	/* Names of gender, race and class */
	init_keyword_misc();

	C_MAKE(g_feat_flag, MAX_F_IDX, byte);
	for (i = 0; i < MAX_F_IDX; i++)
	{
		g_feat_flag[i] = 0;
	}

	/* These are required for savefile loading (see lite_spot()) */
	for (i = 0; i < DUNGEON_HGT; i++)
	{
		/* Info about what feature/monster/object is known. */
		C_MAKE(g_grid[i], DUNGEON_WID, t_grid);

		/* Various visual effects. */
		C_MAKE(g_grid_effect[i], DUNGEON_WID, t_grid_effect);
	}

	map_init();

	init_debug();

	struct_init();

#ifdef ALLOW_STATUS_EXTRA
	redraw_init();
#endif /* ALLOW_STATUS_EXTRA */

	sound_init_1();

	/* Program is intialized */
	if (Tcl_EvalEx(g_interp, "angband_initialized", -1, TCL_EVAL_GLOBAL) != TCL_OK)
	{
		HandleError();
	}
}

/*
 * Tcl_Eval() a file, assuming the given filename is not UTF-8.
 */
int angtk_eval_file(char *extFileName)
{
	char *utfFileName;
	Tcl_DString dString;
	int result;

	utfFileName = Tcl_ExternalToUtfDString(NULL, extFileName, -1, &dString);
	result = Tcl_EvalFile(g_interp, utfFileName);
	Tcl_DStringFree(&dString);
	return result;
}

/*
 * XXX Mega-Hack: Let Tcl force dungeon recreation.
 */
int angtk_generate(void)
{
	int i;

	angtk_eval("angband_generate", NULL);

	if (Tcl_GetBooleanFromObj(g_interp, Tcl_GetObjResult(g_interp), &i) == TCL_OK)
	{
		return i;
	}

	return -1;
}

/*
 * Do stuff after a dungeon is generated.
 */
void angtk_cave_generated(void)
{
	int y, x;

	/* Check each grid for door alignment */
	for (y = 0; y < g_cave_hgt; y++)
	{
		for (x = 0; x < g_cave_wid; x++)
		{
			int feat = cave_feat(y, x);

			/* This is a door (or secret door) */
			if ((feat == FEAT_OPEN) || (feat == FEAT_BROKEN) ||
				(feat >= FEAT_DOOR_HEAD && feat <= FEAT_SECRET))
			{
				/* Note vertical doors */
				if (door_vertical(y, x))
				{
					g_grid[y][x].xtra |= GRID_XTRA_ISVERT;
				}
			}

			/* A wall or secret door */
			if (is_wall(y, x))
			{
				/* Note wall */
				g_grid[y][x].xtra |= GRID_XTRA_WALL;
			}

			/* Only because not currently saved */
			if (is_door(y, x))
			{
				/* Note wall */
				g_grid[y][x].xtra |= GRID_XTRA_DOOR;
			}
		}
	}

	g_icon_map_changed = TRUE;

	Bind_Generic(EVENT_DUNGEON, KEYWORD_DUNGEON_GEN + 1);
}

/*
 * Initialize stuff after the character has been generated.
 */
void angtk_character_generated(void)
{
	char path[1024];
	int y, x;

	icons_init();
	vault_init();
	if (widget_init() != TCL_OK)
		quit(Tcl_GetStringFromObj(Tcl_GetObjResult(g_interp), NULL));
	if (CanvasWidget_Init() != TCL_OK)
		quit(Tcl_GetStringFromObj(Tcl_GetObjResult(g_interp), NULL));
	if (TreeCtrl_Init() != TCL_OK)
		quit(Tcl_GetStringFromObj(Tcl_GetObjResult(g_interp), NULL));

	/* Source a file to create the interface */
	path_build(path, 1024, ANGBAND_DIR_COMMON_TK, "init-other.tcl");
	if (angtk_eval_file(path) == TCL_ERROR)
	{
		HandleError();
	}

	/* The icon environment must be initialized by a script. */
	if (g_icon_width == 0)
	{
		quit_fmt("Fatal error:\nIcons were not initialized.\n"
			"You must call \"angband init_icons\"");
	}

#if 1
	/* Process each grid */
	for (y = 0; y < DUNGEON_HGT; y++)
	{
		for (x = 0; x < DUNGEON_WID; x++)
		{
			/* A wall or secret door */
			if (is_wall(y, x))
			{
				/* Note wall */
				g_grid[y][x].xtra |= GRID_XTRA_WALL;
			}

			/* Only because not currently saved */
			if (is_door(y, x))
			{
				/* Note wall */
				g_grid[y][x].xtra |= GRID_XTRA_DOOR;
			}
		}
	}
#endif

	/*
	 * If we didn't read the g_grid[].xtra information from a savefile,
	 * then hack some right here. This gives us door alignment, but not
	 * pillars.
	 */
	if (!g_grid_xtra_init)
	{
dbwin("g_grid_xtra_init is FALSE\n");

		/* Process each grid */
		for (y = 0; y < DUNGEON_HGT; y++)
		{
			for (x = 0; x < DUNGEON_WID; x++)
			{
				int feat = cave_feat(y, x);

				/* This is a door (or secret door) */
				if ((feat == FEAT_OPEN) || (feat == FEAT_BROKEN) ||
					(feat >= FEAT_DOOR_HEAD && feat <= FEAT_SECRET))
				{
#if 0
					/* Note door */
					g_grid[y][x].xtra |= GRID_XTRA_DOOR;
#endif
					/* Note vertical doors */
					if (door_vertical(y, x))
					{
						g_grid[y][x].xtra |= GRID_XTRA_ISVERT;
					}
				}

#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)
				/* This fixes an incompatibility of 2.8.3 with 2.8.2. */
				cave_info(y, x) &= ~(CAVE_SEEN | CAVE_TEMP | CAVE_VIEW);
#endif /* ANGBANDTK, KANGBANDTK, OANGBANDTK */
			}
		}
    	
		/* Note that g_grid[].xtra is initialized. */
		g_grid_xtra_init = TRUE;
    }

#if defined(ZANGBANDTKxxx)

	/*
	 * wilderness_gen() calls town_illuminate() but g_feat_flag[] isn't
	 * initialized yet. This is a problem because Undead characters
	 * start at night, resulting in incorrect illumination.
	 */
	if (!vanilla_town && (p_ptr_depth == 0) && (turn == (30L * TOWN_DAWN) / 4 + 1))
	{
		town_illuminate(FALSE);
	}

#endif /* ZANGBANDTK */

	/* Should save g_cave_hgt/g_cave_wid. If a town vault gets set they
	 * will be given the correct values then. */
	if (!g_cave_hgt)
	{
#if defined(ANGBANDTK)
		g_cave_hgt = (p_ptr_depth == 0) ? SCREEN_WID : DUNGEON_HGT;
		g_cave_wid = (p_ptr_depth == 0) ? SCREEN_HGT : DUNGEON_WID;
#endif /* */
#if defined(KANGBANDTK)
		g_cave_hgt = DUNGEON_HGT;
		g_cave_wid = DUNGEON_WID;
#endif /* */
#if defined(OANGBANDTK)
		g_cave_hgt = (p_ptr_depth == 0) ? OLD_SCREEN_HGT : DUNGEON_HGT;
		g_cave_wid = (p_ptr_depth == 0) ? OLD_SCREEN_WID : DUNGEON_WID;
#endif /* */
#if defined(ZANGBANDTK)
		g_cave_hgt = cur_hgt;
		g_cave_wid = cur_wid;
#endif /* */
	}

#if 0
	/* Set shape for known walls */
	for (y = 1; y < g_cave_hgt - 1; y++)
	{
		for (x = 1; x < g_cave_wid - 1; x++)
		{
			if (is_wall(y, x) && (cave_info[y][x] & CAVE_MARK))
			{
				g_grid[y][x].shape = wall_shape(y, x, FALSE);
			}
		}
	}
#endif

	/*
	 * Assign icons to each grid. You have to do this *after* sourcing
	 * the startup script, because that's where icon types are defined
	 * and where icon assignments are made.
	 */
	g_icon_map_changed = TRUE;

#ifdef ALLOW_STATUS_EXTRA

	for (y = 0; y < PR_MAX; y++)	
		redraw_add(y);

#endif /* ALLOW_STATUS_EXTRA */

	/* If animation is enabled, start the animation timer */
	if (allow_animation) angtk_start_timer();
}

/*
 * Describe a cave location. This code should be (nearly) the
 * same as in target_set_aux() in xtra2.c. The location does
 * not need to be in bounds.
 */
void angtk_examine(int y, int x, char *out_val)
{
	cptr s1, s2, s3;

	bool boring;

	int feat;

	s16b this_o_idx, next_o_idx;

#if defined(KANGBANDTK) || defined(ZANGBANDTK)
	char tmp_val[80];
#endif /* KANGBANDTK ZANGBANDTK */

	/* Nothing to see */
	(void) strcpy(out_val, "");

	/* Not in bounds */
	if (!in_bounds_test(y, x)) return;

	/* Assume boring */
	boring = TRUE;

	/* Default */
	s1 = "You see ";
	s2 = "";
	s3 = "";

	/* The player */
	if (player_is_here(y, x))
	{
		/* Description */
		s1 = "You are ";

		/* Preposition */
		s2 = "on ";
	}


	/* Hack -- hallucination */
	if (p_ptr->image)
	{
		cptr name = "something strange";

		/* Display a message */
		sprintf(out_val, "%s%s%s%s", s1, s2, s3, name);

		/* Done */
		return;
	}


	/* Actual monsters */
	if (cave_m_idx(y, x) > 0)
	{
		monster_type *m_ptr = &m_list[cave_m_idx(y, x)];

		/* Visible */
		if (m_ptr->ml)
		{
			char m_name[80];
			char m_desc[80];

			/* Get the monster name ("a kobold") */
			monster_desc(m_name, m_ptr, 0x08);

			/* Describe */
			look_mon_desc(m_desc, cave_m_idx(y, x));
			sprintf(out_val, "%s%s%s%s (%s)",
			        s1, s2, s3, m_name, m_desc);

			/* Done */
			return;
		}
	}

#ifdef ALLOW_EASY_FLOOR

	if (easy_floor)
	{
		int floor_list[23], floor_num;

		/* Scan the pile of objects */
#if defined(ANGBANDTK) || defined(KANGBANDTK)
		floor_num = scan_floor(floor_list, 23, y, x, 0x02);
#endif
#if defined(OANGBANDTK) || defined(ZANGBANDTK)
		(void) scan_floor(floor_list, &floor_num, y, x, 0x02);
#endif

		/* There are some marked objects here */
		if (floor_num)
		{
			/* Only one known object */
			if (floor_num == 1)
			{
				char o_name[O_NAME_MAX];

				/* Acquire object */
				object_type *o_ptr = &o_list[floor_list[0]];

#ifdef ALLOW_EASY_SENSE

				/* Option: Make item sensing easy */
				if (easy_sense && player_can_see_bold(y, x))
				{
					/* Sense the object */
					(void) sense_object(o_ptr);
				}

#endif /* ALLOW_EASY_SENSE */

				/* Describe the object */
				object_desc(o_name, o_ptr, TRUE, 3);

				/* Message */
				sprintf(out_val, "%s%s%s%s", s1, s2, s3, o_name);
			}

			/* Multiple known objects */
			else
			{
#ifdef ALLOW_EASY_SENSE

				/*
				 * We don't usually sense an object until the character
				 * actively examines it (via the look command, for
				 * example). But since I know I will display a list
				 * of objects in the stack in the Recall Window, I
				 * sense each object in the stack here.
				 */

				/* Option: Make item sensing easy */
				if (easy_sense && player_can_see_bold(y, x))
				{
					int i;

					/* Check each object */
					for (i = 0; i < floor_num; i++)
					{
						/* Acquire object */
						object_type *o_ptr = &o_list[floor_list[i]];

						/* Sense the object */
						(void) sense_object(o_ptr);
					}
				}

#endif /* ALLOW_EASY_SENSE */

				/* Message */
				sprintf(out_val, "%s%s%sa pile of %d items",
					s1, s2, s3, floor_num);
			}

			/* Done */
			return;
		}
	}

	/* !easy_floor */
	else

#endif /* ALLOW_EASY_FLOOR */

	/* Scan all objects in the grid */
	for (this_o_idx = cave_o_idx(y, x); this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

		/* Acquire object */
		o_ptr = &o_list[this_o_idx];

		/* Acquire next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Describe it */
		if (o_ptr->marked)
		{
			char o_name[O_NAME_MAX];

#ifdef ALLOW_EASY_SENSE

			/* Option: Make item sensing easy */
			if (easy_sense && player_can_see_bold(y, x))
			{
				/* Sense the object */
				(void) sense_object(o_ptr);
			}

#endif /* ALLOW_EASY_SENSE */

			/* Obtain an object description */
			object_desc(o_name, o_ptr, TRUE, 3);

			/* Describe the object */
			sprintf(out_val, "%s%s%s%s", s1, s2, s3, o_name);

			/* Done */
			return;
		}
	}

	/* Get feature */
	feat = cave_feat(y, x);

#if defined(ZANGBANDTK)
	if (cave[y][x].mimic)
	{
		feat = cave[y][x].mimic;
	}
#endif /* ZANGBANDTK */

	/* Feature (apply "mimic") */
	feat = f_info[feat].mimic;

	/* Require knowledge about grid, or ability to see grid */
	if (!(cave_info(y, x) & (CAVE_MARK)) && !player_can_see_bold(y,x))
	{
		/* Forget feature */
		feat = FEAT_NONE;
	}

	/* Terrain feature if needed */
	if (boring || (feat > FEAT_INVIS))
	{
		cptr name = f_name + f_info[feat].name;

#if defined(KANGBANDTK)
		/* Hack -- special handling for building doors */
		if ((feat >= FEAT_BLDG_HEAD) && (feat <= FEAT_BLDG_TAIL))
		{
			int index = feat - FEAT_BLDG_HEAD;

			/* Handle gap in the building f_index no.s */
			if (index > 7)
				index -= 3;

			name = building[index].name;
		}
#endif /* KANGBANDTK */

#if defined(ZANGBANDTK)
		/* Hack -- special handling for building doors */
		if ((feat >= FEAT_BLDG_HEAD) && (feat <= FEAT_BLDG_TAIL))
		{
			name = building[feat - FEAT_BLDG_HEAD].name;
		}
#endif /* ZANGBANDTK */

		/* Hack -- handle unknown grids */
		if (feat == FEAT_NONE) return;

		/* Hack -- handle open floor grids */
		if (feat == FEAT_FLOOR) return;

		/* Pick a prefix */
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)
		if (*s2 && (feat >= FEAT_DOOR_HEAD)) s2 = "in ";
#endif /* */
#if defined(ZANGBANDTK)
		if (*s2 && ((feat >= FEAT_MINOR_GLYPH) &&
					(feat <= FEAT_PATTERN_XTRA2))) s2 = "on ";
		else if (*s2 && (feat >= FEAT_DOOR_HEAD)) s2 = "in ";
#endif /* ZANGBANDTK */

		/* Pick proper indefinite article */
		s3 = (is_a_vowel(name[0])) ? "an " : "a ";

#if defined(KANGBANDTK) || defined(ZANGBANDTK)
		/* Hack -- special introduction for store & building doors -KMW- */
		if (((feat >= FEAT_SHOP_HEAD) && (feat <= FEAT_SHOP_TAIL)) ||
			((feat >= FEAT_BLDG_HEAD) & (feat <= FEAT_BLDG_TAIL)))
		{
			s3 = "the entrance to the ";
		}
#endif /* KANGBANDTK, ZANGBANDTK */
#if defined(ANGBANDTK) || defined(OANGBANDTK)
		/* Hack -- special introduction for store doors */
		if ((feat >= FEAT_SHOP_HEAD) && (feat <= FEAT_SHOP_TAIL))
		{
			s3 = "the entrance to the ";
		}
#endif /* ANGBANDTK, OANGBANDTK */

#if defined(OANGBANDTK)
		/* Hack -- special treatment for certain terrain features. -LM- */
		if ((feat == FEAT_WATER) || (feat == FEAT_LAVA) || (feat == FEAT_TREE))
		{
			s3 = "";
		}
#endif /* */

#if defined(KANGBANDTK)
		/* Hack -- special handling for quest entrances */
		if (feat == FEAT_QUEST_ENTER)
		{
			/* Set the quest number temporary */
			int old_quest = p_ptr->inside_quest;
			p_ptr->inside_quest = cave_special[y][x];

			/* Get the quest text */
			init_flags = INIT_SHOW_TEXT;
			quest_text_line = 0;
			process_dungeon_file("q_info.txt", 0, 0, 0, 0);

			/* Build an expanded feature description */
			(void) sprintf(tmp_val, "%s (%s)", name,
				quest[cave_special[y][x]].name);

			name = tmp_val;

			/* Reset the old quest number */
			p_ptr->inside_quest = old_quest;
		}
#endif /* KANGBANDTK */

#if defined(ZANGBANDTK)
		/* "You see dirt." */
		if (feat == FEAT_DIRT)
		{
			s3 = "";
		}

		if (feat == FEAT_QUEST_ENTER)
		{
			int q_idx = cave[y][x].special;

			/* Access the quest */
			quest_type *q_ptr = &quest[q_idx];

			/* Perhaps initialize the quest name */
			if (!q_ptr->name[0])
			{
				int i, old_quest;

				/* Clear the text */
				for (i = 0; i < 10; i++)
				{
					quest_text[i][0] = '\0';
				}

				quest_text_line = 0;

				/* Set the quest number temporarily */
				old_quest = p_ptr->inside_quest;
				p_ptr->inside_quest = q_idx;

				/* Get the quest text */
				init_flags = INIT_SHOW_TEXT;
				process_dungeon_file("q_info.txt", 0, 0, 0, 0);

				/* Reset the old quest number */
				p_ptr->inside_quest = old_quest;
			}

			/* Build an expanded feature description */
			(void) sprintf(tmp_val, "%s (%s)", name, q_ptr->name);

			name = tmp_val;
		}
#endif /* ZANGBANDTK */

		/* Display a message */
		sprintf(out_val, "%s%s%s%s", s1, s2, s3, name);

		/* Done */
		return;
	}
}

#if 0

#define FO_PASS_WALL 0x0001 /* Can enter if ghostly */
#define FO_RES_HEAT 0x0002 /* Can enter if resist heat */
#define FO_WEIGHT 0x0004 /* Can enter if not encumbered */
#define FO_RANGER 0x0008 /* Can enter if Ranger */
#define FO_FFALL 0x0010 /* Can enter if feather falling */
#define FO_NO_ENTER 0x0020 /* Can't enter anyhow */

typedef struct feat_okay {
	int mimic; /* Use fields from other feature */
	cptr unknown; /* Message when unmarked feature is bumped into */
	cptr blocked; /* Message when feature can't be entered */
	int flags; /* FO_XXX flags */
} feat_okay;

feat_okay f_okay[] = {
0, NULL, NULL, 0, /* FEAT_NONE */
0, NULL, NULL, 0, /* FEAT_FLOOR */
0, NULL, NULL, 0, /* FEAT_INVIS */
0, NULL, NULL, 0, /* FEAT_GLYPH */
0, NULL, NULL, 0, /* FEAT_OPEN */
0, NULL, NULL, 0, /* FEAT_BROKEN */
0, NULL, NULL, 0, /* FEAT_LESS */
0, NULL, NULL, 0, /* FEAT_MORE */
0, NULL, NULL, 0, /* FEAT_QUEST_ENTER */
0, NULL, NULL, 0, /* FEAT_QUEST_EXIT */
0, NULL, NULL, 0, /* FEAT_QUEST_DOWN */
0, NULL, NULL, 0, /* FEAT_QUEST_UP */
0, NULL, NULL, 0, /* 0x0C */
0, NULL, NULL, 0, /* 0x0D */
0, NULL, NULL, 0, /* 0x0E */
0, NULL, NULL, 0, /* 0x0F */
0, NULL, NULL, 0, /* FEAT_TRAP_HEAD */
0, NULL, NULL, 0, /* 0x11 */
0, NULL, NULL, 0, /* 0x12 */
0, NULL, NULL, 0, /* 0x13 */
0, NULL, NULL, 0, /* 0x14 */
0, NULL, NULL, 0, /* 0x15 */
0, NULL, NULL, 0, /* 0x16 */
0, NULL, NULL, 0, /* 0x17 */
0, NULL, NULL, 0, /* 0x18 */
0, NULL, NULL, 0, /* 0x19 */
0, NULL, NULL, 0, /* 0x1A */
0, NULL, NULL, 0, /* 0x1B */
0, NULL, NULL, 0, /* 0x1C */
0, NULL, NULL, 0, /* 0x1D */
0, NULL, NULL, 0, /* 0x1E */
0, NULL, NULL, 0, /* FEAT_TRAP_TAIL */

/* FEAT_DOOR_HEAD */
0, "You feel a closed door blocking your way",
"There is a closed door blocking your way.", FO_PASS_WALL,

FEAT_DOOR_HEAD, NULL, NULL, 0, /* 0x21 */
FEAT_DOOR_HEAD, NULL, NULL, 0, /* 0x22 */
FEAT_DOOR_HEAD, NULL, NULL, 0, /* 0x23 */
FEAT_DOOR_HEAD, NULL, NULL, 0, /* 0x24 */
FEAT_DOOR_HEAD, NULL, NULL, 0, /* 0x25 */
FEAT_DOOR_HEAD, NULL, NULL, 0, /* 0x26 */
FEAT_DOOR_HEAD, NULL, NULL, 0, /* 0x27 */
FEAT_DOOR_HEAD, NULL, NULL, 0, /* 0x28 */
FEAT_DOOR_HEAD, NULL, NULL, 0, /* 0x29 */
FEAT_DOOR_HEAD, NULL, NULL, 0, /* 0x2A */
FEAT_DOOR_HEAD, NULL, NULL, 0, /* 0x2B */
FEAT_DOOR_HEAD, NULL, NULL, 0, /* 0x2C */
FEAT_DOOR_HEAD, NULL, NULL, 0, /* 0x2D */
FEAT_DOOR_HEAD, NULL, NULL, 0, /* 0x2E */
FEAT_DOOR_HEAD, NULL, NULL, 0, /* FEAT_DOOR_TAIL */
FEAT_MAGMA, NULL, NULL, 0, /* FEAT_SECRET */

/* FEAT_RUBBLE */
0, "You feel some rubble blocking your way.",
"There is rubble blocking your way.", FO_PASS_WALL,

/* FEAT_MAGMA */
0, "You feel a wall blocking your way.",
"There is a wall blocking your way.", FO_PASS_WALL,

FEAT_MAGMA, NULL, NULL, 0, /* FEAT_QUARTZ */
FEAT_MAGMA, NULL, NULL, 0, /* FEAT_MAGMA_H */
FEAT_MAGMA, NULL, NULL, 0, /* FEAT_QUARTZ_H */
FEAT_MAGMA, NULL, NULL, 0, /* FEAT_MAGMA_K */
FEAT_MAGMA, NULL, NULL, 0, /* FEAT_QUARTZ_K */
FEAT_MAGMA, NULL, NULL, 0, /* FEAT_WALL_EXTRA */
FEAT_MAGMA, NULL, NULL, 0, /* FEAT_WALL_INNER */
FEAT_MAGMA, NULL, NULL, 0, /* FEAT_WALL_OUTER */
FEAT_MAGMA, NULL, NULL, 0, /* FEAT_WALL_SOLID */

/* FEAT_PERM_INNER */
0, "You feel a wall blocking your way.",
"There is a wall blocking your way.", FO_NO_ENTER,

FEAT_PERM_INNER, NULL, NULL, 0, /* FEAT_PERM_INNER */
FEAT_PERM_INNER, NULL, NULL, 0, /* FEAT_PERM_OUTER */
FEAT_PERM_INNER, NULL, NULL, 0, /* FEAT_PERM_SOLID */
0, NULL, NULL, 0, /* FEAT_MINOR_GLYPH */

0, NULL, NULL, 0, /* FEAT_PATTERN_START */
0, NULL, NULL, 0, /* FEAT_PATTERN_1 */
0, NULL, NULL, 0, /* FEAT_PATTERN_2 */
0, NULL, NULL, 0, /* FEAT_PATTERN_3 */
0, NULL, NULL, 0, /* FEAT_PATTERN_4 */
0, NULL, NULL, 0, /* FEAT_PATTERN_END */
0, NULL, NULL, 0, /* FEAT_PATTERN_OLD */
0, NULL, NULL, 0, /* FEAT_PATTERN_XTRA1 */
0, NULL, NULL, 0, /* FEAT_PATTERN_XTRA2 */

0, NULL, NULL, 0, /* FEAT_SHOP_HEAD */

};

#endif

#if defined(KANGBANDTK)

/*
 * player_test_feature --
 *
 * Return TRUE if the character should be allowed to attempt to move
 * into the given cave location. If "message" is TRUE and the character
 * cannot move onto the location, then a message is displayed and the
 * location may be marked if it wasn't already.
 *
 * Valid bits for 'mode' are:
 *    0x01 - Display message
 *    0x02 - Apply damage; When character is at y,x, apply damage due to
 *           being unprotected on this grid. (NOT USED)
 */
bool player_test_feature(int y, int x, int mode)
{
	cptr msg = NULL;
	bool known = FALSE;
	bool doMsg = FALSE;
	bool note = FALSE;
	bool pass_wall = FALSE;

	/* Hack -- walking into unseen features obtains knowledge XXX XXX */
	if ((cave_info(y, x) & (CAVE_MARK)))
		known = TRUE;

	/* Character can pass through walls */
	if (p_ptr->ghostly)
		pass_wall = TRUE;

	/* Print a message if asked*/
	if (mode & 0x01) doMsg = TRUE;

	/* Examine the feature */
	switch (cave_feat(y, x))
	{
		case FEAT_DOOR_HEAD:
		case 0x22: case 0x23: case 0x24: case 0x25:
		case 0x26: case 0x27: case 0x28: case 0x29:
		case 0x2A: case 0x2B: case 0x2C: case 0x2D:
		case 0x2E:
		case FEAT_DOOR_TAIL:
			if (pass_wall) return (TRUE);
			if (!known)
			{
				msg = "You feel a door blocking your way";
				note = TRUE;
			}
			else
				msg = "There is a door in the way!";
#ifdef ALLOW_EASY_ALTER

			if (known && easy_alter)
			{
				/* Jammed door */
				if (cave_feat(y, x) >= FEAT_DOOR_HEAD + 0x08)
				{
					msg = "The door appears to be stuck.";
				}
				else
					return (TRUE);
			}

#endif /* ALLOW_EASY_ALTER */
			break;

		case FEAT_RUBBLE:
			if (pass_wall) return (TRUE);
			if (!known)
			{
				msg = "You feel a pile of rubble blocking your way.";
				note = TRUE;
			}
			else
				msg = "There is a pile of rubble in the way!";
			break;

		case FEAT_SECRET:
		case FEAT_MAGMA:
		case FEAT_QUARTZ:
		case FEAT_MAGMA_H:
		case FEAT_QUARTZ_H:
		case FEAT_MAGMA_K:
		case FEAT_QUARTZ_K:
		case FEAT_WALL_EXTRA:
		case FEAT_WALL_INNER:
		case FEAT_WALL_OUTER:
		case FEAT_WALL_SOLID:
			if (pass_wall) return (TRUE);
			if (!known)
			{
				msg = "You feel a wall blocking your way.";
				note = TRUE;
			}
			else
				msg = "There is a wall in the way!";
			break;

		case FEAT_PERM_EXTRA:
		case FEAT_PERM_INNER:
		case FEAT_PERM_OUTER:
		case FEAT_PERM_SOLID:
			if (!known)
			{
				msg = "You feel a wall blocking your way.";
				note = TRUE;
			}
			else
				msg = "There is a wall in the way!";
			break;

		case FEAT_TREES:
#if 1
			/* This behaviour is from ZAngband */
			if ((p_ptr->pclass == CLASS_RANGER) ||
				(p_ptr->pclass == CLASS_DRUID))
			{
				if (doMsg) p_ptr->energy_use += 10;
			}
			return (TRUE);
#else
			if ((p_ptr->pclass == CLASS_RANGER) ||
				(p_ptr->pclass == CLASS_DRUID))
			{
				return (TRUE);
			}
			if (!known)
			{
				if (!doMsg) return (TRUE);
				msg = "You feel a tree blocking your way.";
				cave_info(y, x) |= (CAVE_MARK);
				lite_spot(y, x);
				break;
			}
			msg = "The trees are in your way!";
#endif
			break;

		case FEAT_MOUNTAIN:
			if (p_ptr->ghostly) return (TRUE);
			if (!known)
			{
				msg = "You feel a mountain blocking your way.";
				note = TRUE;
			}
			else
				msg = "You can't climb the mountains!";
			break;

		/*
		 * FEAT_DEEP_WATER, FEAT_DEEP_LAVA and FEAT_SHALLOW_LAVA
		 * are all passable, but the character may suffer the
		 * effects. From ZAngband.
		 */
#if 0
		case FEAT_DEEP_WATER:
			if (p_ptr->levitate) return (TRUE);
			wt = (adj_str_wgt[p_ptr->stat_ind[A_STR]] * 100) / 2;
			if (p_ptr->total_weight < wt)
			{
				return (TRUE);
			}
			if (!known)
			{
				if (!doMsg) return (TRUE);
				msg = "You feel deep water blocking your way.";
				cave_info(y, x) |= (CAVE_MARK);
				lite_spot(y, x);
				break;
			}
			msg = "You can't swim with that much weight.";
			break;

		case FEAT_DEEP_LAVA:
			if (p_ptr->levitate)
			{
				if ((p_ptr->resist_fire) || (p_ptr->oppose_fire) ||
					(p_ptr->immune_fire))
				{
					return (TRUE);
				}
				msg = "The heat of the lava is too intense to cross!";
				break;
			}
			if (!known)
			{
				if (!doMsg) return (TRUE);
				msg = "You feel the heat of deep lava blocking your way.";
				cave_info(y, x) |= (CAVE_MARK);
				lite_spot(y, x);
				break;
			}
			msg = "There is a river of lava in the way!";

#if 0
			/* Are character's objects hurt by heat when character
			 * levitates over deep magma? */

			if ((mode & 0x02) && (y == p_ptr->py) && (x == p_ptr->px)
			{
				/* Message */
				msg_print("You are sinking in molten rock!");

				/* Hurt the character with fire */
				fire_dam(200, "lava");

				/* XXX Other effects: Character speed -40 */

				return (FALSE);
			}
#endif
			break;

		case FEAT_SHAL_LAVA:
			if ((p_ptr->resist_fire) || (p_ptr->immune_fire) || 
			    (p_ptr->oppose_fire) || (p_ptr->levitate))
			{
				return (TRUE);
			}
			if (!known)
			{
				if (!doMsg) return (TRUE);
				msg = "You feel the heat of shallow lava blocking your way.";
				cave_info(y, x) |= (CAVE_MARK);
				lite_spot(y, x);
				break;
			}
			msg = "The heat of the lava is too intense to cross!";
			break;
#endif /* 0 */

		case FEAT_CHASM:
			if (p_ptr->levitate) return (TRUE);
			if (!known)
			{
				msg = "You feel a chasm blocking your way.";
				note = TRUE;
			}
			else
				msg = "The chasm can't be crossed.";
			break;

		/* All other features are passable */
		default:
			return (TRUE);
			break;
	}

	/* The grid is unknown, and no message is requested */
	if (!known && !doMsg) return (TRUE);

	/* A message was requested */
	if (doMsg)
	{
		/* Print a message */
		if (msg) msg_print(msg);

		/* Mark unknown grid */
		if (note)
		{
			/* Memorize the grid */
			cave_info(y, x) |= (CAVE_MARK);
			angtk_feat_known(y, x);

			/* Draw the grid */
			lite_spot(y, x);
		}

		/* XXX Hack -- No energy is used */
		if (!(p_ptr->confused || p_ptr->stun || p_ptr->image))
			p_ptr_energy_use = 0;
	}

	/* Character cannot move here */
	return (FALSE);
}

#endif /* KANGBANDTK */

#if defined(ZANGBANDTK)

/*
 * Return TRUE if the character should be allowed to attempt to move
 * onto the given cave location. If "message" is TRUE and the character
 * cannot move onto the location, then a message is displayed and the
 * location may be marked if it wasn't already.
 *
 * Valid bits for 'mode' are:
 *    0x01 - Display message
 *    0x02 - Apply damage; When character is at y,x, apply damage due to
 *           being unprotected on this grid. (NOT USED)
 */
bool player_test_feature(int y, int x, int mode)
{
	cptr msg = NULL;
/*	int wt; */
	bool known = FALSE;
	bool message = FALSE;
	bool note = FALSE;
	bool pass_wall = FALSE;
	int feat = cave[y][x].feat;

	/* Hack -- walking into unseen features obtains knowledge XXX XXX */
	if ((cave[y][x].info & (CAVE_MARK))) known = TRUE;

	/* Player can not walk through "walls" unless in Shadow Form */
	if ((p_ptr->wraith_form) || (p_ptr->pass_wall))
		pass_wall = TRUE;

	/* Apply mimic field */
	if (cave[y][x].mimic) feat = cave[y][x].mimic;

	/* Print a message if asked */
	if (mode & 0x01) message = TRUE;

	/* Examine the feature */
	switch (feat)
	{
		case FEAT_DOOR_HEAD:
		case 0x21: case 0x22: case 0x23: case 0x24:
		case 0x25: case 0x26: case 0x27: case 0x28:
		case 0x29: case 0x2A: case 0x2B: case 0x2C:
		case 0x2D: case 0x2E:
		case FEAT_DOOR_TAIL:
			if (pass_wall) return (TRUE);
			if (!known)
			{
				msg = "You feel a closed door blocking your way";
				note = TRUE;
			}
			else
				msg = "There is a door in the way!";
#ifdef ALLOW_EASY_ALTER

			if (known && easy_alter)
			{
				/* Jammed door */
				if (feat >= FEAT_DOOR_HEAD + 0x08)
				{
					msg = "The door appears to be stuck.";
				}
				else
					return (TRUE);
			}

#endif /* ALLOW_EASY_ALTER */
			break;

		case FEAT_RUBBLE:
			if (pass_wall) return (TRUE);
			if (!known)
			{
				msg = "You feel some rubble blocking your way.";
				note = TRUE;
			}
			else
				msg = "There is a pile of rubble in the way!";
			break;

		case FEAT_SECRET:
		case FEAT_MAGMA:
		case FEAT_QUARTZ:
		case FEAT_MAGMA_H:
		case FEAT_QUARTZ_H:
		case FEAT_MAGMA_K:
		case FEAT_QUARTZ_K:
		case FEAT_WALL_EXTRA:
		case FEAT_WALL_INNER:
		case FEAT_WALL_OUTER:
		case FEAT_WALL_SOLID:
			if (pass_wall) return (TRUE);
			if (!known)
			{
				msg = "You feel a wall blocking your way.";
				note = TRUE;
			}
			else
				msg = "There is a wall in the way!";
			break;

		case FEAT_PERM_EXTRA:
		case FEAT_PERM_INNER:
		case FEAT_PERM_OUTER:
		case FEAT_PERM_SOLID:
			if (!known)
			{
				msg = "You feel a wall blocking your way.";
				note = TRUE;
			}
			else
				msg = "There is a wall in the way!";
			break;

		case FEAT_TREES:
			if ((p_ptr->pclass != CLASS_RANGER))
			{
				if (message) energy_use += 10;
			}
			return (TRUE);
			break;

		case FEAT_MOUNTAIN:
			if (!known)
			{
				msg = "You feel a mountain blocking your way.";
				note = TRUE;
			}
			else
				msg = "You can't climb the mountains!";
			break;

		case FEAT_DEEP_WATER:
#if 1
			/* The character may drown later */
			return (TRUE);
#else
			if (p_ptr->ffall) return (TRUE);
			wt = (adj_str_wgt[p_ptr->stat_ind[A_STR]] * 100) / 2;
			if (p_ptr->total_weight < wt) return (TRUE);
			if (!known)
			{
				msg = "You feel deep water blocking your way.";
				note = TRUE;
			}
			else
				msg = "You can't swim with that much weight.";
#endif
			break;

		case FEAT_DEEP_LAVA:
#if 1
			/* The character may burn later */
			return (TRUE);
#else
			if (!known)
			{
				msg = "You feel the heat of deep lava blocking your way.";
				note = TRUE;
			}
			else
				msg = "There is a river of lava in the way!";
#endif
			break;

		case FEAT_SHAL_LAVA:
#if 1
			/* The character may sizzle later */
			return (TRUE);
#else
			if (p_ptr->ffall) return (TRUE);
			if ((p_ptr->resist_fire) ||
				(p_ptr->immune_fire) || 
			    (p_ptr->oppose_fire))
			{
				return (TRUE);
			}
			if (!known)
			{
				msg = "You feel the heat of shallow lava blocking your way.";
				note = TRUE;
			}
			else
				msg = "The heat of the lava is too intense to cross!";
#endif
			break;

		case FEAT_DARK_PIT:
			if (p_ptr->ffall) return (TRUE);
			if (!known)
			{
				msg = "You feel a chasm blocking your way.";
				note = TRUE;
			}
			else
				msg = "You can't cross the chasm.";
			break;

		default:
			return (TRUE);
			break;
	}

	/* The grid is unknown, and no message is requested */
	if (!known && !message) return (TRUE);

	/* A message was requested */
	if (message)
	{
		/* Print a message */
		if (msg) msg_print(msg);

		/* Mark unknown grid */
		if (note)
		{
			/* Memorize the grid */
			cave[y][x].info |= (CAVE_MARK);
			angtk_feat_known(y, x); /* TNB */

			/* Draw the grid */
			lite_spot(y, x);
		}

		/* XXX Hack -- No energy is used */
		if (!(p_ptr->confused || p_ptr->stun || p_ptr->image))
			energy_use = 0;
	}

	/* Character cannot move here */
	return (FALSE);
}

#endif /* ZANGBANDTK */

