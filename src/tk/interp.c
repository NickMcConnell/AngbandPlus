/* File: interp.c */

/* Purpose: general script commands */

/*
 * Copyright (c) 1997-2001 Tim Baker
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "tnb.h"
#include "icon.h"


/* TRUE if current command is repeated */
bool command_repeating = FALSE;

int inkey_flags = 0;
int inkey_book;
int exit_skip_save = FALSE;


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
		if (is_pet(m_ptr))
		{
			Tcl_ListObjAppendElement(g_interp, listObjPtr,
				Tcl_NewIntObj(i));
		}
	}

	return listObjPtr;
}

static int s_status_value;

/*
 * Prints status of hunger
 */
static cptr state_hunger(void)
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
static cptr state_blind(void)
{
	if (p_ptr->tim.blind)
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
static cptr state_confused(void)
{
	if (p_ptr->tim.confused)
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
static cptr state_afraid(void)
{
	if (p_ptr->tim.afraid)
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
static cptr state_poisoned(void)
{
	if (p_ptr->tim.poisoned)
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
static cptr state_state(void)
{
	/* Paralysis */
	if (p_ptr->tim.paralyzed)
	{
		return "Paralyzed!";
	}

	/* Resting */
	else if (p_ptr->state.resting)
	{
		int n = p_ptr->state.resting;

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
	else if (p_ptr->cmd.rep)
	{
		int n = p_ptr->cmd.rep;

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
	else if (p_ptr->state.searching)
	{
		return "Searching";
	}

	/* Nothing interesting */
	else
	{
		return "";
	}
}

/*
 * Prints the speed of a character.			-CJS-
 */
static cptr state_speed(void)
{
	int n = p_ptr->pspeed;

	/* Hack -- Visually "undo" the Search Mode Slowdown */
	if (p_ptr->state.searching) n += 10;

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

static cptr state_study(void)
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

static cptr state_cut(void)
{
	int c = p_ptr->tim.cut;

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

static cptr state_stun(void)
{
	int s = p_ptr->tim.stun;

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

static cptr state_winner(void)
{
	/* Wizard */
	if (p_ptr->state.wizard)
	{
		return "Wizard";
	}

	/* Winner */
	else if (p_ptr->state.total_winner || (p_ptr->lev > PY_MAX_LEVEL))
	{
		return "Winner";
	}

	/* Normal */
	else
	{
		return "";
	}
}

cptr player_status(int status, int *value)
{
	cptr format;

	typedef cptr (status_proc)(void);
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
	};

	s_status_value = 0;

	format = (*status_info[status])();
	(*value) = s_status_value;
	return format;
}

static void blows_per_round(int *_blows, int *_muta_att)
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

static void shots_per_round(int *_shots, int *_shots_frac)
{
	int energy_fire = 100;
	int shots, shots_frac;
	object_type *o_ptr = &p_ptr->equipment[EQUIP_BOW];

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
				if (p_ptr->stat[A_DEX].use >= 16)
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

	static cptr cmdOptions[] = {"ability", "age", "armor_class",
		"blows_per_round", "died_from",
		"exp", "food", "gold", "height", "hitpoints",
		"infravision", "level", "mana", "position",
		"sex", "shots_per_round", "social_class",
		"title", "to_dam", "to_hit", "weight",
		"total_weight", "preserve", "base_name",
		"is_dead", "turn", "max_level", "disturb", "new_spells",
		"command_rep", "running", "prayer_or_spell", "health_who",
		"monster_race_idx", "life_rating",
		"pets", "realm1", "realm2", "patron",
		NULL};
	enum {IDX_ABILITY, IDX_AGE, IDX_ARMOR_CLASS,
		IDX_BLOWS_PER_ROUND, IDX_DIED_FROM,
		IDX_EXP, IDX_FOOD, IDX_GOLD, IDX_HEIGHT, IDX_HITPOINTS,
		IDX_INFRAVISION, IDX_LEVEL, IDX_MANA, IDX_POSITION,
		IDX_SEX, IDX_SHOTS_PER_ROUND, IDX_SOCIAL_CLASS,
		IDX_TITLE, IDX_TO_DAM, IDX_TO_HIT, IDX_WEIGHT,
		IDX_TOTAL_WEIGHT, IDX_PRESERVE, IDX_BASE_NAME,
		IDX_IS_DEAD, IDX_TURN, IDX_MAX_LEVEL, IDX_DISTURB, IDX_NEW_SPELLS,
		IDX_COMMAND_REP, IDX_RUNNING, IDX_PRAYER_OR_SPELL, IDX_HEALTH_WHO,
		IDX_MONSTER_RACE_IDX, IDX_LIFE_RATING,
		IDX_PETS, IDX_REALM1, IDX_REALM2, IDX_PATRON
		};
	int option;
	Tcl_Obj *resultPtr = Tcl_GetObjResult(interp);
	int index;

	object_type *o_ptr;
	int i, tmp;
	long expadv;
	cptr t;

	static cptr abilityOptions[] = {"fighting", "bows_throw", "saving_throw",
		"stealth", "perception", "searching", "disarming", "magic_device",
		NULL};

	static struct {int rating; int max;} ability[] = {
		{0, 10}, /* fighting */
		{0, 10}, /* bows_throw */
		{0, 6}, /* saving_throw */
		{0, 1}, /* stealth */
		{0, 6}, /* perception */
		{0, 6}, /* searching */
		{0, 8}, /* disarming */
		{0, 6} /* magic_device */
	};

	/* Required number of arguments */
    if (objC < 2)
    {
		Tcl_WrongNumArgs(interp, infoCmd->depth + 1, objv, "option ?arg ...?");
		return TCL_ERROR;
    }

	/* Get requested option */
    if (Tcl_GetIndexFromObj(interp, objV[1], cmdOptions, "option", 0, 
		&option) != TCL_OK)
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
			o_ptr = &p_ptr->equipment[EQUIP_WIELD];
			tmp = p_ptr->to_h + o_ptr->to_h;
			ability[0].rating = p_ptr->skills[SKILL_THN] + (tmp * BTH_PLUS_ADJ);
	
			/* Shooting Skill (with current bow and normal missile) */
			o_ptr = &p_ptr->equipment[EQUIP_BOW];
			tmp = p_ptr->to_h + o_ptr->to_h;
			ability[1].rating = p_ptr->skills[SKILL_THB] + (tmp * BTH_PLUS_ADJ);
	
			ability[2].rating = p_ptr->skills[SKILL_SAV];
			ability[3].rating = p_ptr->skills[SKILL_STL];
			ability[4].rating = p_ptr->skills[SKILL_FOS];
			ability[5].rating = p_ptr->skills[SKILL_SNS];
			ability[6].rating = p_ptr->skills[SKILL_DIS];
			ability[7].rating = p_ptr->skills[SKILL_DEV];
			 
			Tcl_SetStringObj(resultPtr, format("%d %d",
				ability[index].rating, ability[index].max), -1);
			break;

		case IDX_AGE: /* age */
			Tcl_SetIntObj(resultPtr, p_ptr->rp.age);
			break;

		case IDX_ARMOR_CLASS: /* armor_class */
			Tcl_SetStringObj(resultPtr,
				format("%d %d", p_ptr->dis_ac, p_ptr->dis_to_a), -1);
			break;

		case IDX_BLOWS_PER_ROUND: /* blows_per_round */
		{
			int blows, muta_att;
			blows_per_round(&blows, &muta_att);
			Tcl_SetStringObj(resultPtr,
				format(muta_att ? "%d+%d" : "%d", blows, muta_att), -1);
			break;
		}

		case IDX_DIED_FROM: /* died_from */
			if (!p_ptr->state.is_dead)
			{
				Tcl_SetStringObj(resultPtr, "character is not dead", -1);
				return TCL_ERROR;
			}
			ExtToUtf_SetResult(interp, p_ptr->state.died_from);
			break;

		case IDX_EXP: /* exp */
			if (p_ptr->lev >= PY_MAX_LEVEL) expadv = 999999999;
			else expadv = (s32b)(player_exp[p_ptr->lev - 1] * p_ptr->expfact / 100L);
			Tcl_SetStringObj(resultPtr, format("%ld %ld %ld", p_ptr->exp,
				p_ptr->max_exp, expadv), -1);
			break;

		case IDX_FOOD: /* food */
			Tcl_SetStringObj(resultPtr,
				format("%d %d", p_ptr->food, PY_FOOD_MAX), -1);
			break;

		case IDX_GOLD: /* gold */
			Tcl_SetStringObj(resultPtr, format("%ld", p_ptr->au), -1);
			break;

		case IDX_HEIGHT: /* height */
			Tcl_SetIntObj(resultPtr, p_ptr->rp.ht);
			break;

		case IDX_HITPOINTS: /* hitpoints */
			Tcl_SetStringObj(resultPtr,
				format("%d %d", p_ptr->chp, p_ptr->mhp), -1);
			break;

		case IDX_INFRAVISION: /* infravision */
			Tcl_SetIntObj(resultPtr, p_ptr->see_infra * 10);
			break;

		case IDX_LEVEL: /* level */
			Tcl_SetIntObj(resultPtr, p_ptr->lev);
			break;

		case IDX_MANA: /* mana */
			Tcl_SetStringObj(resultPtr,
				format("%d %d", p_ptr->csp, p_ptr->msp), -1);
			break;

		case IDX_POSITION: /* position */
			Tcl_SetStringObj(resultPtr, format("%d %d", p_ptr->py, p_ptr->px),
				-1);
			break;

		case IDX_SEX: /* sex */
			Tcl_SetStringObj(resultPtr, sp_ptr->title, -1);
			break;

		case IDX_SHOTS_PER_ROUND: /* shots_per_round */
		{
			int shots, shots_frac;
			shots_per_round(&shots, &shots_frac);
			Tcl_SetStringObj(resultPtr,
				format("%d.%d", shots, shots_frac), -1);
			break;
		}

		case IDX_SOCIAL_CLASS: /* social_class */
			Tcl_SetIntObj(resultPtr, p_ptr->rp.sc);
			break;

		case IDX_TITLE: /* title */
			ExtToUtf_SetResult(interp,
				player_title[p_ptr->rp.pclass][(p_ptr->lev-1)/5]);
			break;

		case IDX_TO_DAM: /* to_dam */
			Tcl_SetIntObj(resultPtr, p_ptr->dis_to_d);
			break;

		case IDX_TO_HIT: /* to_hit */
			Tcl_SetIntObj(resultPtr, p_ptr->dis_to_h);
			break;

		case IDX_WEIGHT: /* weight */
			Tcl_SetIntObj(resultPtr, p_ptr->rp.wt);
			break;

		case IDX_TOTAL_WEIGHT: /* total_weight */
			Tcl_SetIntObj(resultPtr, p_ptr->total_weight);
			break;

		case IDX_PRESERVE: /* preserve */
			Tcl_SetIntObj(resultPtr, preserve_mode);
			break;

		case IDX_BASE_NAME: /* base_name */
			ExtToUtf_SetResult(interp, player_base);
			break;

		case IDX_IS_DEAD: /* is_dead */
			Tcl_SetBooleanObj(resultPtr, p_ptr->state.is_dead);
			break;

		case IDX_TURN: /* turn */
			Tcl_SetLongObj(resultPtr, turn);
			break;

		case IDX_MAX_LEVEL: /* max_level */
			Tcl_SetIntObj(resultPtr, p_ptr->max_lev);
			break;

		case IDX_DISTURB: /* disturb */
			/* When is this allowed? */
			if (inkey_flags == 0)
			{
				disturb(FALSE);
			}
			break;

		case IDX_NEW_SPELLS: /* new_spells */
			if (!p_ptr->spell.r[0].realm)
			{
				Tcl_SetStringObj(resultPtr, "character cannot read books", -1);
				return TCL_ERROR;
			}
			Tcl_SetIntObj(resultPtr, p_ptr->new_spells);
			break;

		case IDX_COMMAND_REP: /* command_rep */
			Tcl_SetIntObj(resultPtr, p_ptr->cmd.rep);
			break;

		case IDX_RUNNING: /* running */
			Tcl_SetIntObj(resultPtr, p_ptr->state.running);
			break;

		case IDX_PRAYER_OR_SPELL: /* prayer_or_spell */
			if (!p_ptr->spell.r[0].realm)
			{
				Tcl_SetStringObj(resultPtr, "character cannot read books", -1);
				return TCL_ERROR;
			}
			switch (mp_ptr->spell_book)
			{
				case TV_LIFE_BOOK: t = "prayer"; break;
				default: t = "spell"; break;
			}
			if (t == NULL)
			{
				quit_fmt("unhandled mp_ptr->spell_book %d",
					mp_ptr->spell_book);
			}
			Tcl_SetStringObj(resultPtr, t, -1);
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
					Tcl_SetStringObj(resultPtr,
						format("bad m_list index \"%d\": must be between 0 and %d",
						m_idx, (int) m_max - 1), -1);
				}
				p_ptr->health_who = m_idx;
				break;
			}
			Tcl_SetIntObj(resultPtr, p_ptr->health_who);
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
				if (!((r_idx >= 0) && (r_idx < z_info->r_max)))
				{
					Tcl_SetStringObj(resultPtr,
						format("bad r_info index \"%d\": must be between 0 and %d",
						r_idx, (int) z_info->r_max - 1), -1);
					return TCL_ERROR;
				}
				p_ptr->monster_race_idx = r_idx;
				break;
			}
			Tcl_SetIntObj(resultPtr, p_ptr->monster_race_idx);
			break;

		case IDX_LIFE_RATING: /* life_rating */
			i = (int) (((long) p_ptr->player_hp[PY_MAX_LEVEL - 1] * 200L) / 
				(2 * p_ptr->rp.hitdie + ((PY_MAX_LEVEL - 1) *
				(p_ptr->rp.hitdie + 1))));
			Tcl_SetIntObj(resultPtr, i);
			break;
			
		case IDX_PETS: /* pets */
			Tcl_SetObjResult(interp, DumpPets());
			break;

		case IDX_REALM1: /* realm1 */
			Tcl_SetStringObj(resultPtr,
				realm_names[p_ptr->spell.r[0].realm], -1);
			break;

		case IDX_REALM2: /* realm2 */
			Tcl_SetStringObj(resultPtr,
				realm_names[p_ptr->spell.r[1].realm], -1);
			break;

		case IDX_PATRON: /* patron */
			ExtToUtf_SetResult(interp,
				chaos_patrons[p_ptr->chaos_patron]);
			break;

	}

	return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * angtk_eval --
 *
 *	Eval() a command with arguments.
 *
 *--------------------------------------------------------------
 */

void angtk_eval(cptr command, ...)
{
	cptr s = command;
	va_list vp;
	int objc = 0;
	Tcl_Obj *objv[40];
	int i, result;

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
		s = va_arg(vp, cptr);
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


static void HandleError(void)
{
	char path[1024];
	cptr errorInfo;
	FILE *fp;

	/* Dump the stack to errors.txt */
	path_make(path, ANGBAND_DIR_TK, "errors.txt");
	fp = fopen(path, "a");
	if (fp != NULL)
	{
		errorInfo = Tcl_GetVar(g_interp, "errorInfo", TCL_GLOBAL_ONLY);
		fprintf(fp, "***** (inside HandleError)\n\n%s\n\n", errorInfo);
		fclose(fp);
	}

	/* Display a message and quit */
	quit_fmt("The following error occurred:\n\n%s\n\n"
			 "Please examine the errors.txt file to see what happened.",
				Tcl_GetStringResult(g_interp));
}


static CommandInit commandInit[] = {
	{0, "angband", 0, 0, NULL, NULL, (ClientData) 0},
		{1, "cave", 0, 0, NULL, objcmd_cave, (ClientData) 0},
		{1, "game", 0, 0, NULL, objcmd_game, (ClientData) 0},
		{1, "inkey_flags", 1, 1, NULL, objcmd_inkey_flags, (ClientData) 0},
		{1, "inventory", 0, 0, NULL, objcmd_inventory, (ClientData) 0},
		{1, "keypress", 2, 2, "string", objcmd_keypress, (ClientData) 0},
		{1, "message", 0, 0, NULL, objcmd_message, (ClientData) 0},
		{1, "player", 0, 0, NULL, objcmd_player, (ClientData) 0},
		{1, "equipinfo", 3, 3, "slot arrayName", objcmd_equipinfo, (ClientData) 0},
		{1, "inveninfo", 3, 3, "slot arrayName", objcmd_inveninfo, (ClientData) 0},
		{1, "init_icons", 3, 3, "size depth", objcmd_init_icons, (ClientData) 0},
		{1, "floor", 0, 0, NULL, objcmd_floor, (ClientData) 0},
		{1, "keycount", 0, 0, NULL, objcmd_keycount, (ClientData) 0},
	{0, "fontdesc", 2, 2, "font", objcmd_fontdesc, (ClientData) 0},
	{0, NULL, 0, 0, NULL, NULL, (ClientData) 0}
};

/*
 * Initialize stuff after Tcl/Tk but before a game is started.
 */
void angtk_init(void)
{
	char path[1024];

	/* Tcl commands */
	CommandInfo_Init(g_interp, commandInit, NULL);

	/* Standard color palette */
	init_palette();
	
	/* Source the "startup script" */
	path_make(path, ANGBAND_DIR_TK, "init-startup.tcl");
	if (angtk_eval_file(path) == TCL_ERROR)
	{
		HandleError();
	}
}

/*
 * Initialize stuff after init_angband().
 */
void angtk_angband_initialized(void)
{
	char path[1024];

	/* Program is intialized */
	if (Tcl_EvalEx(g_interp, "angband_initialized", -1, TCL_EVAL_GLOBAL) != TCL_OK)
	{
		HandleError();
	}

	/* Source a file to create the interface */
	path_make(path, ANGBAND_DIR_TK, "init-other.tcl");
	if (angtk_eval_file(path) == TCL_ERROR)
	{
		HandleError();
	}

	/* The icon environment must be initialized by a script. */
	if (g_icon_size == 0)
	{
		quit_fmt("Fatal error:\nIcons were not initialized.\n"
			"You must call \"angband init_icons\"");
	}
}

/*
 * Tcl_Eval() a file, assuming the given filename is not UTF-8.
 */
int angtk_eval_file(cptr extFileName)
{
	cptr utfFileName;
	Tcl_DString dString;
	int result;

	utfFileName = Tcl_ExternalToUtfDString(NULL, extFileName, -1, &dString);
	result = Tcl_EvalFile(g_interp, utfFileName);
	Tcl_DStringFree(&dString);
	return result;
}

/*
 *--------------------------------------------------------------
 *
 * objcmd_cave --
 *
 *	Implements the "cave" script command.
 * 	Syntax:
 *		cave blocked y x -- can player move there
 *		cave examine y x -- describe what's there
 *		cave height -- height of cave
 *		cave width -- width of cave
 *		cave info -- get info about a grid
 *		cave wild_name -- get name of wilderness area
 *
 *--------------------------------------------------------------
 */

int
objcmd_cave(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	static cptr cmdOptions[] = {"wild_name", NULL};
	enum {IDX_WILD_NAME};
	int option;
	Tcl_Obj *resultPtr = Tcl_GetObjResult(interp);

    if (objC < 2)
    {
		Tcl_WrongNumArgs(interp, infoCmd->depth + 1, objv, "option ?arg ...?");
		return TCL_ERROR;
    }

    if (Tcl_GetIndexFromObj(interp, objV[1], cmdOptions, "option", 0, 
		&option) != TCL_OK)
	{
		return TCL_ERROR;
    }

	switch (option)
	{
	
		case IDX_WILD_NAME: /* wild_name */
			if (!character_dungeon)
			{
				/* Set the error */
				Tcl_SetStringObj(resultPtr, "dungeon has not been generated yet", -1);
				goto error;
			}
			if (!p_ptr->depth)
			{
				if (p_ptr->place_num)
				{
					ExtToUtf_SetResult(interp, place[p_ptr->place_num].name);
				}
				else
				{
					Tcl_SetStringObj(resultPtr, "Wilderness", -1);
				}
			}
			break;
	}

	/* Success */
	return TCL_OK;
	
error:

	/* Failure */
	return TCL_ERROR;
}


/*
 *--------------------------------------------------------------
 *
 * objcmd_floor --
 *
 *	Implements the "floor" script command.
 * 	Syntax:
 *
 *		floor find SEARCHCOMMAND ?arg arg ...?
 * 			Return list of indexes of matching objects
 *		floor info INDEX arrayName
 *		floor memory INDEX
 *
 *--------------------------------------------------------------
 */

int
objcmd_floor(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	static cptr cmdOptions[] = {"find", "memory",
		NULL};
	enum {IDX_FIND, IDX_MEMORY};
	int option;
	Tcl_Obj *resultPtr = Tcl_GetObjResult(interp);
	int index;

	Tcl_Obj *listObjPtr;
	char *buffer;
	int i;
	long length;
	object_type *o_ptr;
	int fy, fx;

	/* Default to finding all matches */
	int request_limit = 0, match_limit = 0, cnt = 0;

	/* Default to ignoring item_tester_okay() hook */
	int request_tester = 0, match_tester = 0;

	/* Default to no restriction on tval */
	int request_tval = 0, match_tval[10], tval_cnt = 0;

	if (objC < 2)
	{
		Tcl_WrongNumArgs(interp, infoCmd->depth + 1, objv, "option ?arg ...?");
		return TCL_ERROR;
	}

	if (Tcl_GetIndexFromObj(interp, objV[1], cmdOptions, "option", 0, 
		&option) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* XXX Hack -- Determine the location to display */
	fy = p_ptr->py;
	fx = p_ptr->px;

	switch (option)
	{
		case IDX_FIND: /* find */
		{
			bool (*old_tester_hook)(const object_type *) = item_tester_hook;
			bool (*temp_tester_hook)(const object_type *) = NULL;
			
			/* Scan arguments for options */
			for (i = 2; i < objC; )
			{
				static cptr cmdOptions[] = {"-limit", "-tester", NULL};

				/* Get the sub-option */
				if (Tcl_GetIndexFromObj(interp, objV[i], cmdOptions, "option",
					0, &index) != TCL_OK)
				{
					return TCL_ERROR;
				}

				switch (index)
				{
					case 0: /* Limit */
					{
						if (Tcl_GetIntFromObj(interp, objV[i+1], &match_limit)
							!= TCL_OK)
						{
							return TCL_ERROR;
						}
						request_limit = 1;
						i += 2;
						break;
					}

					case 1: /* Tester */
					{
						if (Tcl_GetBooleanFromObj(interp, objV[i+1],
							&match_tester) != TCL_OK)
						{
							return TCL_ERROR;
						}
						request_tester = 1;
						i += 2;
						break;
					}
				}
			}

			if (temp_tester_hook) item_tester_hook = temp_tester_hook;

			/* Return a list of o_list[] indexes */
			listObjPtr = Tcl_NewListObj(0, NULL);

			/* Scan all objects in the grid */
			OBJ_ITT_START (area(fx, fy)->o_idx, o_ptr)
			{
				if (request_tester && match_tester)
				{
					/* Accept TV_GOLD if no tester */
					if ((o_ptr->tval == TV_GOLD) && !item_tester_hook &&
						!item_tester_tval)
					{
						/* Nothing */
					}
					else if (!item_tester_okay(o_ptr)) 
					{
						continue;
					}
				}
				if (request_tval)
				{
					for (i = 0; i < tval_cnt; i++)
					{
						if (match_tval[0] == o_ptr->tval) break;
					}
					if (i == tval_cnt) continue;
				}
	
				/* Found a match */
				Tcl_ListObjAppendElement(interp, listObjPtr,
					Tcl_NewIntObj(_this_o_idx));
	
				/* Return x matches */
				if (request_limit && (++cnt >= match_limit)) break;
			}
			OBJ_ITT_END;

			/* XXX Hack -- Restore the hook */
			item_tester_hook = old_tester_hook;

			/* Return a list of o_list[] indexes */
			Tcl_SetObjResult(interp, listObjPtr);
			break;
		}

		case IDX_MEMORY: /* memory */

			if (Tcl_GetIntFromObj(interp, objV[2], &i) != TCL_OK)
			{
				return TCL_ERROR;
			}
			if (i <= 0 || i > o_max) goto bad_index;

			/* Get item info */
			o_ptr = &o_list[i];

			/* Illegal */
			if (!o_ptr->k_idx || (o_ptr->iy != fy) || (o_ptr->ix != fx))
			{
				goto bad_index;
			}

			C_MAKE(buffer, 5 * 1024L, char);
			length = angtk_describe_object(o_ptr, buffer, FALSE);
			Tcl_SetObjResult(interp, ExtToUtf_NewStringObj(buffer, length));
			FREE(buffer);
			break;
	}

	return TCL_OK;

bad_index:
	Tcl_SetStringObj(resultPtr, format("bad floor index \"%d\"", i), -1);
	return TCL_ERROR;
}


/*
 *--------------------------------------------------------------
 *
 * objcmd_game --
 *
 *	Implements the "game" script command.
 * 	Syntax:
 *		game abort ?confirm? -- Quit without saving
 *		game directory -- Get a directory pathname
 *		game keymap_dump -- Dump a keymap file
 *		game new -- Start a new game
 *		game open -- Open a save file
 *		game process_pref_file -- Process a preference file
 *		game quit -- Quit with save
 *
 *--------------------------------------------------------------
 */


/* List of directory keywords */
cptr keyword_path[] = {
	"ANGBAND_DIR_ROOT",
	"ANGBAND_DIR_USER",
	"ANGBAND_DIR_TK",
	NULL
};


int
objcmd_game(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	static cptr cmdOptions[] = {"abort", "tkdir" "version", NULL};
	enum {IDX_ABORT, IDX_TKDIR, IDX_VERSION};
	int option;
	Tcl_Obj *resultPtr = Tcl_GetObjResult(interp);
	int index;

	if (objC < 2)
	{
		Tcl_WrongNumArgs(interp, infoCmd->depth + 1, objv, "option ?arg ...?");
		return TCL_ERROR;
	}

	if (Tcl_GetIndexFromObj(interp, objV[1], cmdOptions, "option", 0, 
		&option) != TCL_OK)
	{
		return TCL_ERROR;
	}

	switch (option)
	{
		case IDX_ABORT: /* abort */
		{
			int confirm = 1;
			if (objC == 3)
			{
				static cptr abortSwitch[] = {"-noask", NULL};
				if (Tcl_GetIndexFromObj(interp, objV[2], abortSwitch,
					"switch", 0, &index) != TCL_OK)
				{
					return TCL_ERROR;
				}
				confirm = 0;
			}
			if (confirm && game_in_progress && character_generated)
			{
				int result;

				result = Tcl_EvalEx(g_interp,
					"tk_messageBox -icon warning -type okcancel -message \"Your character will not be saved!\" -title \"Quit Without Saving\"",
					-1, TCL_EVAL_GLOBAL);
				if (result == TCL_OK)
				{
					cptr s = Tcl_GetStringResult(g_interp);
					if (!strcmp(s, "cancel")) break;
				}
			}
			quit(NULL);
			break;
		}
		
		case IDX_TKDIR: /* Tk directory for game .tcl files */
		{
		 	/* Return the current directory path */
			ExtToUtf_SetResult(interp, ANGBAND_DIR_TK);
			break;
		}
		
		case IDX_VERSION: /* version */
			Tcl_SetStringObj(resultPtr, format("%d.%d.%d", VER_MAJOR,
				VER_MINOR, VER_PATCH), -1);
			break;
	}

	return TCL_OK;
}


/* init_icons $size $depth */
int
objcmd_init_icons(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
/*	int objC = objc - infoCmd->depth; */
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	int size, depth;
	
	/* Hack - ignore parameter */
	(void) objc;

	if (g_icon_size)
	{
		Tcl_SetResult(interp, (char *) "icons were already initialized", TCL_VOLATILE);
		return TCL_ERROR;
	}

	/* Get the size */
	if (Tcl_GetIntFromObj(interp, objV[1], &size) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* Get the depth */
	if (Tcl_GetIntFromObj(interp, objV[2], &depth) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* Initialize (quit on failure) */
	init_icons(size, depth);

	return TCL_OK;
}

/* Strings returned by "inkey_flags" command, indexed by INKEY_XXX defines. */
cptr inkey_to_str[] = {"", "INKEY_CMD", "INKEY_DIR", "INKEY_DISTURB",
	"INKEY_ITEM", "INKEY_ITEM_STORE", "INKEY_MORE", "INKEY_SPELL",
	"INKEY_TARGET", "INKEY_POWER", "INKEY_CMD_PET",
	NULL};

/* (inkey) flags */
int
objcmd_inkey_flags(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	/* Hack - ignore parameters */
	(void) objc;
	(void) objv;
	(void) clientData;

	Tcl_SetResult(interp, (char *) inkey_to_str[inkey_flags], TCL_VOLATILE);
	return TCL_OK;
}


/*
 *--------------------------------------------------------------
 *
 * objcmd_inventory --
 *
 *	Implements the "inventory" script command.
 * 	Syntax:
 *
 *		inventory count
 *			Return number of inventory items carried
 *
 *		inventory find SEARCHCOMMAND ?arg arg ...?
 * 			Return list of indexes of matching objects
 *
 *		inventory info INDEX VARNAME
 *			Return info about specific object
 *
 *		inventory memory INDEX
 *			Return memory about about specific object
 *
 *		inventory total_weight
 *			Return total weight carried
 *
 *		inventory weight_limit
 *			Return carrying capacity in 10ths of pounds
 *
 *--------------------------------------------------------------
 */

int
objcmd_inventory(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	static cptr cmdOptions[] = {
		"total_weight", "weight_limit",
		NULL};
	enum {IDX_TOTAL_WEIGHT, IDX_WEIGHT_LIMIT};
	int option;
	Tcl_Obj *resultPtr = Tcl_GetObjResult(interp);

	int i;

    if (objC < 2)
    {
		Tcl_WrongNumArgs(interp, infoCmd->depth + 1, objv, "option ?arg ...?");
		return TCL_ERROR;
    }

    if (Tcl_GetIndexFromObj(interp, objV[1], cmdOptions, "option", 0, 
		&option) != TCL_OK)
	{
		return TCL_ERROR;
    }

	switch (option)
	{
		case IDX_TOTAL_WEIGHT: /* total_weight */
			Tcl_SetIntObj(resultPtr, p_ptr->total_weight);
			break;

		case IDX_WEIGHT_LIMIT: /* weight_limit */

			/* Max carrying capacity in 10ths of pounds */
			i = adj_str_wgt[p_ptr->stat[A_STR].ind] * 100;
			Tcl_SetIntObj(resultPtr, i);
			break;
	}

	return TCL_OK;
}

/* keycount */
int
objcmd_keycount(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	/* Hack - ignore parameters */
	(void) objc;
	(void) objv;
	(void) clientData;

	Tcl_SetObjResult(interp,
		Tcl_NewBooleanObj(Term->key_head != Term->key_tail));
	return TCL_OK;
}

/* keypress $string */
int
objcmd_keypress(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
/*	int objC = objc - infoCmd->depth; */
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	char *t;
	int i;

	/* Hack - ignore parameters */
	(void) objc;
	(void) interp;

	t = Tcl_GetStringFromObj(objV[1], NULL);
	for (i = 0; t[i]; i++)
	{
		Term_keypress(t[i]);
	}
	return TCL_OK;
}


/*
 *--------------------------------------------------------------
 *
 * objcmd_message --
 *
 *	Implements the "message" script command.
 * 	Syntax:
 *		message color -- Return color for message $index
 *		message count -- Return number of saved messages
 *		message get $index -- Return most-recent number of messages
 *		message sound $index -- Return sound for message $index
 *
 *--------------------------------------------------------------
 */

int
objcmd_message(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	static cptr cmdOption[] = {"color", "count", "get", NULL};
	enum {IDX_COLOR, IDX_COUNT, IDX_GET};
	int option;
	Tcl_Obj *resultPtr = Tcl_GetObjResult(interp);

	int i, k;
	byte attr;

    if (objC < 2)
    {
		Tcl_WrongNumArgs(interp, infoCmd->depth + 1, objv, "option ?arg ...?");
		return TCL_ERROR;
    }

    if (Tcl_GetIndexFromObj(interp, objV[1], cmdOption, "option", 0, 
		&option) != TCL_OK)
	{
		return TCL_ERROR;
    }

	if (!character_generated)
	{
		Tcl_AppendStringsToObj(resultPtr,
			"character has not been generated yet", NULL);
		return TCL_ERROR;
	}

	switch (option)
	{
		case IDX_COLOR: /* color */
			if (Tcl_GetIntFromObj(interp, objV[2], &i) != TCL_OK)
			{
				return TCL_ERROR;
			}
			k = message_num();
			if (i < 0 || i >= k)
			{
				Tcl_SetStringObj(resultPtr, format("invalid message index \"%d\": "
					"must be from 0 to %d", i, k - 1), -1);
				return TCL_ERROR;
			}
			attr = TERM_WHITE;
			Tcl_SetStringObj(resultPtr, keyword_term_color[attr], -1);
			break;
		
		case IDX_COUNT: /* count */
			Tcl_SetIntObj(resultPtr, message_num());
			break;

		case IDX_GET: /* get */
			if (Tcl_GetIntFromObj(interp, objV[2], &i) != TCL_OK)
			{
				return TCL_ERROR;
			}
			k = message_num();
			if (i < 0 || i >= k)
			{
				Tcl_SetStringObj(resultPtr, format("invalid message index \"%d\": "
					"must be from 0 to %d", i, k - 1), -1);
				return TCL_ERROR;
			}
			ExtToUtf_SetResult(interp, message_str(i));
			break;
	}

	return TCL_OK;
}
