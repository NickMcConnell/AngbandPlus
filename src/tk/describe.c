/* File: describe.c */

/* Purpose: object recall */

/*
 * Copyright (c) 1997-2001 Tim Baker
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "tnb.h"

/*
 * Like strcpy() but returns the length of the string copied
 */
int strcpy_len(char *s1, const char *s2)
{
	int len = 0;
	while ((*s1++ = *s2++)) ++len;
	return len;
}

/* Global pointer into output buffer */
static char *s_buffer;

/*
 * Write a string to the output buffer, plus optional padding
 */
static void roll_off(cptr str, bool pad)
{
	if (pad) s_buffer += strcpy_len(s_buffer, "  ");
	s_buffer += strcpy_len(s_buffer, str);
}

/*
 * Remove characters from the output buffer
 */
static void roll_back(int count)
{
	s_buffer -= count;
}

/*
 * Write array of strings to output buffer
 */
static void roll_em(cptr *vp, int vn, cptr joiner, cptr finish)
{
	int		i;
	
	for (i = 0; i < vn; ++i)
	{
		roll_off(vp[i], FALSE);
		
		if (i < vn - 2)
			roll_off(", ", FALSE);
		else if (i == vn - 2)
			roll_off(joiner ? joiner : " and ", FALSE);
		else
			roll_off(finish ? finish : ".", FALSE);
	}
}

#define TR0_STAT_MASK	\
        (TR0_STR | TR0_INT | TR0_WIS | TR0_DEX | \
         TR0_CON | TR0_CHR)

#define TR1_SUST_MASK	\
        (TR1_SUST_STR | TR1_SUST_INT | TR1_SUST_WIS | TR1_SUST_DEX | \
         TR1_SUST_CON | TR1_SUST_CHR)

#define TR_STAT_MASK	0, TR0_STAT_MASK
#define TR_SUST_MASK	1, TR1_SUST_MASK

/*
 * Write object memory to a buffer
 */
long angtk_describe_object(object_type *o_ptr, char *buf, bool in_store)
{
	cptr vp[128];
	int vn;

	bool known = FALSE;
	bool k = FALSE;
	bool hack;
	
	/* Set the global. See roll_off(). */
	s_buffer = buf;

	/* Null-terminate it anyhow */
	s_buffer[0] = '\0';

	/* See if the object is "known" */
	if (object_known_p(o_ptr)) known = TRUE;

	/* Mega-Hack -- describe activation */
	if (FLAG(o_ptr, TR_ACTIVATE))
	{
		roll_off("It can be activated for ", k);
		roll_off(item_activation(o_ptr), FALSE);
		roll_off(" if it is being worn.", FALSE);

		k = TRUE;
	}

	/* Figurines, a hack */
	if (o_ptr->tval == TV_FIGURINE)
	{
		roll_off("It will transform into a pet when thrown.", k);
		k = TRUE;
	}

	/* Bad things */
	vn = 0;
	
	hack = FALSE;

	if (FLAG(o_ptr, TR_TY_CURSE)) vp[vn++] = "carries an ancient foul curse";

	if (cursed_p(o_ptr))
	{
		if (FLAG(o_ptr, TR_PERMA_CURSE))		vp[vn++] = "is permanently cursed";
		else if (FLAG(o_ptr, TR_HEAVY_CURSE)) 	vp[vn++] = "is heavily cursed";
		else if (known || (o_ptr->info & OB_SENSE)) vp[vn++] = "is cursed";
	}
	
	if (FLAG(o_ptr, TR_DRAIN_EXP))			vp[vn++] = "drains experience";
	if (FLAG(o_ptr, TR_TELEPORT)) 			vp[vn++] = "teleports randomly";
	if (FLAG(o_ptr, TR_AGGRAVATE))			vp[vn++] = "aggravates monsters";
	if (FLAG(o_ptr, TR_NO_MAGIC))    vp[vn++] = "disrupts magic";
	if (FLAG(o_ptr, TR_NO_TELE))     vp[vn++] = "inhibits teleportation";
	
	if (vn)
	{
		roll_off("Unfortunately it ", k);
		roll_em(vp, vn, NULL, NULL);
		
		k = TRUE;
		hack = TRUE;
	}

	if ((FLAG(o_ptr, TR_STAT_MASK)) && o_ptr->pval)
	{
		if (hack && (o_ptr->pval < 0))
		{
			roll_back(1);
			roll_off(",", FALSE);
		}
		else
		{
			if (k) roll_off("  ", FALSE);
			k = TRUE;
		}
		
		roll_off((o_ptr->pval > 0) ? "Increases " : (hack ? " and it decreases " : "Decreases "), FALSE);
		
		vn = 0;
		
		if (FLAG(o_ptr, TR_STR)) vp[vn++] = "Strength";
		if (FLAG(o_ptr, TR_INT)) vp[vn++] = "Intelligence";
		if (FLAG(o_ptr, TR_WIS)) vp[vn++] = "Wisdom";
		if (FLAG(o_ptr, TR_DEX)) vp[vn++] = "Dexterity";
		if (FLAG(o_ptr, TR_CON)) vp[vn++] = "Constitution";
		if (FLAG(o_ptr, TR_CHR)) vp[vn++] = "Charisma";

		if (vn == 6)
		{
			roll_off("all stats.", FALSE);
		}
		else
		{
			roll_em(vp, vn, NULL, NULL);
		}
	}

	/* Effects on skills */
	if (o_ptr->pval)
	{
		vn = 0;
	
		if (FLAG(o_ptr, TR_STEALTH))	vp[vn++] = "Stealth";
		if (FLAG(o_ptr, TR_SEARCH)) 	vp[vn++] = "Searching";
		if (FLAG(o_ptr, TR_INFRA)) 	vp[vn++] = "Infra-vision";
		if (FLAG(o_ptr, TR_BLOWS)) 	vp[vn++] = "number of attacks";
		if (FLAG(o_ptr, TR_SPEED)) 	vp[vn++] = "speed";
	
		if (vn)
		{
			roll_off(o_ptr->pval > 0 ? "Improves " : "Hinders ", k);
			roll_em(vp, vn, NULL, NULL);
			k = TRUE;
		}
	}

	/* Criticals */
	vn = 0;
	
	hack = FALSE;
	
	if (FLAG(o_ptr, TR_BRAND_ACID)) 	vp[vn++] = "acid";
	if (FLAG(o_ptr, TR_BRAND_ELEC))	vp[vn++] = "electricity";
	if (FLAG(o_ptr, TR_BRAND_FIRE)) 	vp[vn++] = "fire";
	if (FLAG(o_ptr, TR_BRAND_COLD))	vp[vn++] = "frost";
	if (FLAG(o_ptr, TR_BRAND_POIS))	vp[vn++] = "poison";

	if (vn)
	{
		roll_off("Does extra damage from ", k);
		roll_em(vp, vn, NULL, NULL);

		k = TRUE;
		hack = TRUE;
	}

	if (FLAG(o_ptr, TR_THROW))
	{
		if (k) roll_off("  ", FALSE);
		roll_off("It is perfectly balanced for throwing.", FALSE);

		k = TRUE;
		hack = TRUE;
	}

	/* Misc nastiness */
	vn = 0;

	if (FLAG(o_ptr, TR_CHAOTIC)) vp[vn++] = "produces chaotic effects";
	if (FLAG(o_ptr, TR_VAMPIRIC)) vp[vn++] = "drains life from your foes";
	if (FLAG(o_ptr, TR_IMPACT)) vp[vn++] = "causes earthquakes";
	if (FLAG(o_ptr, TR_VORPAL)) vp[vn++] = "cuts with supernatural sharpness";

	if (vn)
	{
		roll_off("It ", k);
		roll_em(vp, vn, NULL, NULL);

		k = TRUE;
		hack = TRUE;
	}

	/* What does it slay? */
	vn = 0;
	
	if (FLAG(o_ptr, TR_KILL_DRAGON) || FLAG(o_ptr, TR_SLAY_DRAGON)) 	vp[vn++] = "dragons";
	if (FLAG(o_ptr, TR_SLAY_ORC))   	vp[vn++] = "orcs";
	if (FLAG(o_ptr, TR_SLAY_TROLL)) 	vp[vn++] = "trolls";
	if (FLAG(o_ptr, TR_SLAY_GIANT)) 	vp[vn++] = "giants";
	if (FLAG(o_ptr, TR_SLAY_DEMON)) 	vp[vn++] = "demons";
	if (FLAG(o_ptr, TR_SLAY_UNDEAD)) 	vp[vn++] = "undead";
	if (FLAG(o_ptr, TR_SLAY_ANIMAL)) 	vp[vn++] = "animals";
	if (FLAG(o_ptr, TR_SLAY_EVIL)) 	vp[vn++] = "evil";

	if (vn)
	{
		roll_off("Especially deadly against ", k);
		roll_em(vp, vn, NULL, NULL);
		k = TRUE;
	}

	/* Sustain stats? */
	if (FLAG(o_ptr, TR_SUST_MASK))
	{
		roll_off("Sustains ", k);
				
		vn = 0;
		
		if (FLAG(o_ptr, TR_SUST_STR)) vp[vn++] = "Strength";
		if (FLAG(o_ptr, TR_SUST_INT)) vp[vn++] = "Intelligence";
		if (FLAG(o_ptr, TR_SUST_WIS)) vp[vn++] = "Wisdom";
		if (FLAG(o_ptr, TR_SUST_DEX)) vp[vn++] = "Dexterity";
		if (FLAG(o_ptr, TR_SUST_CON)) vp[vn++] = "Constitution";
		if (FLAG(o_ptr, TR_SUST_CHR)) vp[vn++] = "Charisma";

		if (vn == 6)
		{
			roll_off("all stats.", FALSE);
		}
		else
		{
			roll_em(vp, vn, NULL, NULL);
		}

		k = TRUE;
	}

	/* Immunities */
	vn = 0;
	
	if (FLAG(o_ptr, TR_IM_ACID)) 		vp[vn++] = "acid";
	if (FLAG(o_ptr, TR_IM_ELEC)) 		vp[vn++] = "electricity";
	if (FLAG(o_ptr, TR_IM_FIRE)) 		vp[vn++] = "fire";
	if (FLAG(o_ptr, TR_IM_COLD)) 		vp[vn++] = "cold";
	if (FLAG(o_ptr, TR_RES_BLIND)) 	vp[vn++] = "blindness";
	if (FLAG(o_ptr, TR_FREE_ACT))		vp[vn++] = "paralysis";
	if (FLAG(o_ptr, TR_RES_FEAR))		vp[vn++] = "fear";

	if (vn)
	{
		roll_off("Provides immunity to ", k);
		k = TRUE;

		roll_em(vp, vn, NULL, NULL);
	}

	/* Resistances */
	vn = 0;

	if (FLAG(o_ptr, TR_RES_ACID)) 		vp[vn++] = "acid";
	if (FLAG(o_ptr, TR_RES_ELEC)) 		vp[vn++] = "electricity";
	if (FLAG(o_ptr, TR_RES_FIRE)) 		vp[vn++] = "fire";
	if (FLAG(o_ptr, TR_RES_COLD)) 		vp[vn++] = "cold";
	if (FLAG(o_ptr, TR_RES_POIS)) 		vp[vn++] = "poison";

	if (FLAG(o_ptr, TR_RES_LITE)) 		vp[vn++] = "light";
	if (FLAG(o_ptr, TR_RES_DARK))		vp[vn++] = "dark";
	
	if (FLAG(o_ptr, TR_RES_CONF)) 		vp[vn++] = "confusion";
	if (FLAG(o_ptr, TR_RES_SOUND)) 	vp[vn++] = "sound";
	if (FLAG(o_ptr, TR_RES_SHARDS)) 	vp[vn++] = "shards";

	if (FLAG(o_ptr, TR_RES_NETHER)) 	vp[vn++] = "nether";
	if (FLAG(o_ptr, TR_RES_NEXUS)) 	vp[vn++] = "nexus";
	if (FLAG(o_ptr, TR_RES_CHAOS)) 	vp[vn++] = "chaos";
	if (FLAG(o_ptr, TR_RES_DISEN))		vp[vn++] = "disenchantment";
	if (FLAG(o_ptr, TR_HOLD_LIFE))		vp[vn++] = "life draining";

	if (vn)
	{
		roll_off("Provides resistance to ", k);
		roll_em(vp, vn, NULL, NULL);
		k = TRUE;
	}

	/* What's cool? */
	vn = 0;
	
	if (FLAG(o_ptr, TR_FEATHER)) 		vp[vn++] = "feather falling";
	if (FLAG(o_ptr, TR_SEE_INVIS)) 	vp[vn++] = "see invisible";
	if (FLAG(o_ptr, TR_TELEPATHY)) 	vp[vn++] = "telepathy";
	if (FLAG(o_ptr, TR_SLOW_DIGEST)) 	vp[vn++] = "slow digestion";
	if (FLAG(o_ptr, TR_REGEN)) 		vp[vn++] = "regeneration";

	if (vn)
	{
		roll_off("Enables ", k);
		roll_em(vp, vn, NULL, NULL);
		k = TRUE;
	}

	/* Cool too */
	vn = 0;

	if (FLAG(o_ptr, TR_REFLECT)) vp[vn++] = "reflects missiles";
	if (FLAG(o_ptr, TR_SH_FIRE)) vp[vn++] = "radiates fire";
	if (FLAG(o_ptr, TR_SH_ELEC)) vp[vn++] = "radiates electricity";

	if (vn)
	{
		roll_off("It ", k);
		roll_em(vp, vn, NULL, NULL);
		k = TRUE;
	}

	/* Powerful bows */
	vn = 0;
	
	if (FLAG(o_ptr, TR_XTRA_MIGHT)) 	vp[vn++] = "with extra might";
	if (FLAG(o_ptr, TR_XTRA_SHOTS)) 	vp[vn++] = "excessively fast";

	if (vn)
	{
		roll_off("Fires missiles ", k);
		roll_em(vp, vn, NULL, NULL);
		k = TRUE;
	}

	/* Misc */
	if (FLAG(o_ptr, TR_BLESSED))
	{
		roll_off("It has been blessed by the gods.", k);
		k = TRUE;
	}

	/* Hack -- describe lite's */
	if (o_ptr->tval == TV_LITE)
	{
		if (FLAG(o_ptr, TR_INSTA_ART))
		{
			roll_off("It provides light (radius 3) forever.", k);
		}
		else if (o_ptr->sval == SV_LITE_LANTERN)
		{
			roll_off("It provides light (radius 2) when fueled.", k);
		}
		else
		{
			roll_off("It provides light (radius 1) when fueled.", k);
		}

		k = TRUE;
	}

	/* Permanent light source */
	if (FLAG(o_ptr, TR_LITE))
	{
		roll_off("It provides light (radius 1) forever.", k);
		k = TRUE;
	}

	if (FLAG(o_ptr, TR_TUNNEL))
	{
		roll_off("It is ", k);
		roll_off((o_ptr->pval >= 0) ? "an effective" : "a useless", FALSE);
		roll_off(" digging tool.", FALSE);
		k = TRUE;
	}

	/* What does the object resist? */
	vn = 0;
	
	if (FLAG(o_ptr, TR_IGNORE_ACID)) 	vp[vn++] = "acid";
	if (FLAG(o_ptr, TR_IGNORE_ELEC)) 	vp[vn++] = "electricity";
	if (FLAG(o_ptr, TR_IGNORE_FIRE)) 	vp[vn++] = "fire";
	if (FLAG(o_ptr, TR_IGNORE_COLD)) 	vp[vn++] = "cold";

	if (vn)
	{
		roll_off("It cannot be harmed by ", k);
		roll_em(vp, vn, " or ", NULL);
		k = TRUE;
	}
	
	/* Permanently identified? */
	if (o_ptr->info & OB_MENTAL)
	{
		roll_off("It is permanently identified.", k);
		k = TRUE;
	}

	/* Store bought */
	if (!in_store && (o_ptr->info & OB_NO_EXP))
	{
		roll_off("It was purchased.", k);
		k = TRUE;
	}

	return s_buffer - buf;
}


/*
 * Set a field of a Tcl array variable
 */
int SetArrayValueChar(cptr varName, cptr field, char value)
{
	char string[20];

	strnfmt(string, 20, "%c", value);
	if (Tcl_SetVar2(g_interp, varName, field, string, TCL_LEAVE_ERR_MSG)
		== NULL)
	{
		return TCL_ERROR;
	}
	return TCL_OK;
}

/*
 * Set a field of a Tcl array variable
 */
int SetArrayValueLong(cptr varName, cptr field, long value)
{
	char string[20];

	strnfmt(string, 20, "%ld", value);
	if (Tcl_SetVar2(g_interp, varName, field, string, TCL_LEAVE_ERR_MSG)
		== NULL)
	{
		return TCL_ERROR;
	}
	return TCL_OK;
}

/*
 * Set a field of a Tcl array variable
 */
int SetArrayValueString(cptr varName, cptr field, cptr value)
{
	if (Tcl_SetVar2(g_interp, varName, field, value, TCL_LEAVE_ERR_MSG)
		== NULL)
	{
		return TCL_ERROR;
	}
	return TCL_OK;
}

/*
 * Dump object info into a Tcl array variable
 */
static int DumpObjectInfo(object_type *o_ptr, char *varName)
{
	int known;
	cptr note;

	/* Known? */
	known = object_known_p(o_ptr) ? 1 : 0;

	if (SetArrayValueLong(varName, "known", known) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* Some fields are returned only if the object is "known" */
	if (known)
	{
		if (SetArrayValueLong(varName, "activate",
			(FLAG(o_ptr, TR_ACTIVATE)) != 0) != TCL_OK)
		{
			return TCL_ERROR;
		}
		if (SetArrayValueLong(varName, "artifact", FLAG(o_ptr, TR_INSTA_ART))
			!= TCL_OK)
		{
			return TCL_ERROR;
		}
		if (SetArrayValueLong(varName, "pval", o_ptr->pval) != TCL_OK)
		{
			return TCL_ERROR;
		}
		if (SetArrayValueLong(varName, "to_a", o_ptr->to_a) != TCL_OK)
		{
			return TCL_ERROR;
		}
		if (SetArrayValueLong(varName, "to_d", o_ptr->to_d) != TCL_OK)
		{
			return TCL_ERROR;
		}
		if (SetArrayValueLong(varName, "to_h", o_ptr->to_h) != TCL_OK)
		{
			return TCL_ERROR;
		}
		if (SetArrayValueLong(varName, "timeout", o_ptr->timeout) != TCL_OK)
		{
			return TCL_ERROR;
		}
	}

	if (SetArrayValueLong(varName, "ac", o_ptr->ac) != TCL_OK)
	{
		return TCL_ERROR;
	}
	if (SetArrayValueLong(varName, "dd", o_ptr->dd) != TCL_OK)
	{
		return TCL_ERROR;
	}
	if (SetArrayValueLong(varName, "ds", o_ptr->ds) != TCL_OK)
	{
		return TCL_ERROR;
	}
	if (SetArrayValueLong(varName, "k_idx", o_ptr->k_idx) != TCL_OK)
	{
		return TCL_ERROR;
	}
	if (SetArrayValueLong(varName, "number", o_ptr->number) != TCL_OK)
	{
		return TCL_ERROR;
	}
	if (SetArrayValueLong(varName, "sval", o_ptr->sval) != TCL_OK)
	{
		return TCL_ERROR;
	}
	if (SetArrayValueLong(varName, "tval", o_ptr->tval) != TCL_OK)
	{
		return TCL_ERROR;
	}
	if (SetArrayValueLong(varName, "weight", o_ptr->weight) != TCL_OK)
	{
		return TCL_ERROR;
	}

	note = "";
	if (o_ptr->inscription)
	{
		note = quark_str(o_ptr->inscription);
	}
	if (ExtToUtf_SetArrayValueString(varName, "note", note) != TCL_OK)
	{
		return TCL_ERROR;
	}

	return TCL_OK;
}

/*
 * Get verbose info for an inventory item. This should be combined
 * with the "angband inventory" command. The main difference is it
 * returns more info.
 */
int objcmd_inveninfo(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
/*	int objC = objc - infoCmd->depth; */
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	int i_idx;
	char *varName;
	object_type *o_ptr;
	
	/* Hack - ignore parameter */
	(void) objc;

    if (Tcl_GetIntFromObj(interp, objV[1], &i_idx) != TCL_OK)
    {
		return TCL_ERROR;
    }

	/*** Verify index, existing object ***/

	/* Get the array variable name to dump results in */
	varName = Tcl_GetStringFromObj(objV[2], NULL);

	/* Grab the item */
	o_ptr = get_list_item(p_ptr->inventory, i_idx);

	return DumpObjectInfo(o_ptr, varName);
}

/* Textual name of each equipment slot */
cptr keyword_slot[] = {
	"INVEN_WIELD", "INVEN_BOW", "INVEN_LEFT", "INVEN_RIGHT", "INVEN_NECK",
	"INVEN_LITE", "INVEN_BODY", "INVEN_OUTER", "INVEN_ARM", "INVEN_HEAD",
	"INVEN_HANDS", "INVEN_FEET",
	NULL
};

/*
 * Get verbose info for an equipment item. This should be combined
 * with the "angband equipment" command. The main difference is it
 * returns more info.
 */
int
objcmd_equipinfo(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
/*	int objC = objc - infoCmd->depth; */
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	int i_idx;
	char *varName;
	object_type *o_ptr;
	
	/* Hack - ignore parameter */
	(void) objc;

	/* Get a numerical index or slot name */
    if (Tcl_GetIntFromObj(interp, objV[1], &i_idx) != TCL_OK)
    {
		Tcl_ResetResult(interp);
		if (Tcl_GetIndexFromObj(interp, objV[1], keyword_slot,
			"slot", 0, &i_idx) != TCL_OK)
		{
			return TCL_ERROR;
		}
    }

	/*** Verify index, existing object ***/

	/* Get the array variable name to dump results in */
	varName = Tcl_GetStringFromObj(objV[2], NULL);

	/* Grab the item */
	o_ptr = &p_ptr->equipment[i_idx];

	return DumpObjectInfo(o_ptr, varName);
}
