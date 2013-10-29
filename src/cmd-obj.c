/*
 * File: cmd-obj.c
 * Purpose: Handle objects in various ways
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * Copyright (c) 2007 Andrew Sidwell
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 2, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */
#include "angband.h"
#include "object/tvalsval.h"
#include "cmds.h"

void do_cmd_read_spell(object_type *o_ptr, int item, u16b snd, use_type use)
{
	UNREFERENCED_PARAMETER(snd);
	UNREFERENCED_PARAMETER(use);

	/* Take a turn */
	if ((p_ptr->pclass == CLASS_MAGE) || (p_ptr->pclass == CLASS_WARMAGE) || 
		(p_ptr->pclass == CLASS_PRIEST) || (p_ptr->pclass == CLASS_HIGHPRST))
	{
		p_ptr->energy_use = 75;
		if (p_ptr->lev>=45) p_ptr->energy_use = 25;
		else if (p_ptr->lev>=35) p_ptr->energy_use = 33;
		else if (p_ptr->lev>=15) p_ptr->energy_use = 50;
	}
	else if (p_ptr->pclass == CLASS_WARMAGE)
	{
		p_ptr->energy_use = 100;
		if (p_ptr->lev>=40) p_ptr->energy_use = 33;
		else if (p_ptr->lev>=25) p_ptr->energy_use = 50;
	}
	else
		p_ptr->energy_use = 100;

	/* Not identified yet */
	read_spell(o_ptr, (s16b) item);
}

/*** Inscriptions ***/

/* Can has inscrip pls */
static bool obj_has_inscrip(const object_type *o_ptr)
{
	return (o_ptr->note ? TRUE : FALSE);
}

/* Remove inscription */
static void obj_uninscribe(object_type *o_ptr, int item)
{
	UNREFERENCED_PARAMETER(item);
	o_ptr->note = 0;
	msg_print("Inscription removed.");

	p_ptr->notice |= (PN_COMBINE | PN_SQUELCH);
	p_ptr->redraw |= (PR_INVEN | PR_EQUIP);
}

/* Add inscription */
static void obj_inscribe(object_type *o_ptr, int item)
{
	char o_name[80];
	char tmp[80] = "";
	UNREFERENCED_PARAMETER(item);

	object_desc(o_name, sizeof(o_name), o_ptr, TRUE, ODESC_FULL);
	msg_format("Inscribing %s.", o_name);
	message_flush();

	/* Use old inscription */
	if (o_ptr->note)
		strnfmt(tmp, sizeof(tmp), "%s", quark_str(o_ptr->note));

	/* Get a new inscription (possibly empty) */
	if (get_string("Inscription: ", tmp, sizeof(tmp)))
	{
		o_ptr->note = quark_add(tmp);

		p_ptr->notice |= (PN_COMBINE | PN_SQUELCH);
		p_ptr->redraw |= (PR_INVEN | PR_EQUIP);
	}
}


/*** Examination ***/
static void obj_examine(object_type *o_ptr, int item)
{
	UNREFERENCED_PARAMETER(item);
	text_out_hook = text_out_to_screen;
	screen_save();

	object_info_header(o_ptr);
	if (!object_info(o_ptr, FALSE))
		text_out("\n\nThis item does not seem to possess any special abilities.");

	text_out_c(TERM_L_BLUE, "\n\n[Press any key to continue]\n");
	(void)anykey();

	screen_load();
}



/*** Taking off/putting on ***/

/* Can only take off non-cursed items */
static bool obj_can_takeoff(const object_type *o_ptr)
{
	return !cursed_p(o_ptr);
}

/* Can only put on wieldable items */
static bool obj_can_wear(const object_type *o_ptr)
{
	return (wield_slot(o_ptr) >= INVEN_WIELD);
}


/* Take off an item */
static void obj_takeoff(object_type *o_ptr, int item)
{
	/* 'item' marks the slot of the equipment inventory */
	UNREFERENCED_PARAMETER(o_ptr);
	(void)inven_takeoff(item, 255);
	p_ptr->energy_use = 50;
}

/* Wield or wear an item */
static void obj_wear(object_type *o_ptr, int item)
{
	int slot;
	object_type *equip_o_ptr;
	object_type *j_ptr;
	
	char o_name[80];

	unsigned n;

	/* jk */
	/* for the two-handed routines */
	bool shield_present;
	bool weapon_present;
/*	bool wield_new_twoh = FALSE; */
/*	bool gladiator_weapon = TRUE; */ /* TODO Support gladiators (dual weapon wield) */
	u32b f[OBJ_FLAG_N];
	u32b g[OBJ_FLAG_N];


	/* Check the slot */
	slot = wield_slot(o_ptr);
	equip_o_ptr = &inventory[slot];

	shield_present = (inventory[INVEN_ARM].k_idx != 0); /* Check for shield */
	object_flags(o_ptr, f);
	if ((slot==INVEN_WIELD) && shield_present && (f[3] & TR3_MUST2H))
	{
		msg_print("You can't wield this two-handed weapon with a shield.");
		return;
	}

	weapon_present = (inventory[INVEN_WIELD].k_idx != 0); /* Check for weapon */
	if (weapon_present)
	{
		j_ptr = &inventory[INVEN_WIELD];
		object_flags(j_ptr, g);
		if ((slot==INVEN_ARM) && (g[3] & TR3_MUST2H))
		{
			msg_print("You can't wield a shield while wielding a two-handed weapon");
			return;
		}
	}

	/* TODO Ammo too, if we implement an AMMO slot */
	/* TODO Should not be 'High Priest' specific if other classes later have NO_ATTACK flags */
	if ((cp_ptr->flags & CF_NO_ATTACK) && (o_ptr->tval != TV_RING) && 
		((slot == INVEN_WIELD) || (slot == INVEN_BOW)))
	{
		msg_print("As a High Priest, you can't bring yourself to wield this or any weapon.");
		return;
	}

	/* TODO Should not be 'Gladiator' specific if other classes later have NO_SHIELD flags */
	if ((cp_ptr->flags & CF_NO_SHIELD) && (slot == INVEN_ARM) && (o_ptr->tval == TV_SHIELD))
	{
		msg_print("As a gladiator, you can't wear this shield.");
		return;
	}

	/* TODO Should not be 'Gladiator' specific if other classes later have NO_SHIELD flags */
	if ((cp_ptr->flags & CF_NO_ARMOR) && (slot == INVEN_BODY))
	{
		msg_print("As a gladiator, you can't wear this armor.");
		return;
	}

	/* Check for existing wielded item */
	if (equip_o_ptr)
	{
		/* Prevent wielding into a cursed slot */
		if (cursed_p(equip_o_ptr))
		{
			/* Message */
			object_desc(o_name, sizeof(o_name), equip_o_ptr, FALSE, ODESC_BASE);
			msg_format("The %s you are %s appears to be cursed.",
			           o_name, describe_use(slot));

			return;
		}

		/* "!t" checks for taking off */
		n = check_for_inscrip(equip_o_ptr, "!t");
		while (n--)
		{
			/* Prompt */
			object_desc(o_name, sizeof(o_name), equip_o_ptr, TRUE, ODESC_FULL);

			/* Forget it */
			if (!get_check(format("Really take off %s? ", o_name))) return;
		}
	}

	wield_item(o_ptr, item);
}

/* Drop an item */
static void obj_drop(object_type *o_ptr, int item)
{
	int amt;

	amt = get_quantity(NULL, o_ptr->number);
	if (amt <= 0) return;

	/* Hack -- Cannot remove cursed items */
	if ((item >= INVEN_WIELD) && cursed_p(o_ptr))
	{
		msg_print("Hmmm, it seems to be cursed.");
		return;
	}

	inven_drop(item, amt);
	p_ptr->energy_use = 50;
}


/*** Casting and browsing ***/
/* TODO This code pretty much duplicates item_tester_browsable */
static bool obj_can_browse(const object_type *o_ptr)
{
   if ((p_ptr->pclass == CLASS_MAGE) || (p_ptr->pclass == CLASS_WARMAGE))
   {
      if (((s16b) o_ptr->tval == TV_HAFTED) && ((s16b) o_ptr->sval == SV_QUARTERSTAFF))
      {
         return ( ((s16b) o_ptr->name2 == EGO_STAFF_MAGI) ||
                  ((s16b) o_ptr->name2 == EGO_STAFF_ADEPT) ||
                  ((s16b) o_ptr->name2 == EGO_STAFF_ARCHMAGE) );
      }
   }
   return ((s16b) o_ptr->tval == TV_BOOK);
}

/* TODO Use this instead of testing in do_cmd_browse_aux */
bool obj_cast_pre(void)
{
	/* Warriors are illiterate */
	if (!cp_ptr->spell_stat)
	{
		msg_print("You cannot pray or produce magics.");
		return FALSE;
	}

	if (p_ptr->timed[TMD_BLIND] || no_lite())
	{
		msg_print("You cannot see!");
		return FALSE;
	}

	if (p_ptr->timed[TMD_CONFUSED])
	{
		msg_print("You are too confused!");
		return FALSE;
	}

	return TRUE;
}

/* Peruse spells in a book */
static void obj_browse(object_type *o_ptr, int item)
{
	UNREFERENCED_PARAMETER(item);
	do_cmd_browse_aux(o_ptr);
}

#if 0 /* Not needed anymore */
/* Study a book to gain a new spell */
static void obj_study(object_type *o_ptr, int item)
{
	int spell;
	UNREFERENCED_PARAMETER(item);

	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);
	handle_stuff();

	/* Choose a spell to study */
	spell = spell_choose_new(o_ptr);
	if (spell < 0) return;

	/* Learn the spell */
	spell_learn(spell);
	p_ptr->energy_use = 100;
}
#endif

#if 0 /* Based on Vanilla version */
/* Cast a spell from a book */
static void obj_cast(object_type *o_ptr, int item)
{
	int spell;
	cptr verb = ((cp_ptr->spell_stat == A_INT) ? "cast" : "recite");
	UNREFERENCED_PARAMETER(item);

	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);
	handle_stuff();

	/* Ask for a spell */
	spell = get_spell(o_ptr, verb, TRUE, FALSE);
	if (spell < 0)
	{
		cptr p = ((cp_ptr->spell_stat == A_INT) ? "spell" : "prayer");

		if (spell == -2) msg_format("You don't know any %ss in that book.", p);
		return;
	}

	/* Cast a spell */
	if (spell_cast(spell))
	    p_ptr->energy_use = 100;
}
#endif

/*
 * Cast a spell
 */
static void obj_cast(object_type *o_ptr, int item) /* do_cmd_cast_spell(void) */
{
	s16b spellno;
	UNREFERENCED_PARAMETER(o_ptr);
	UNREFERENCED_PARAMETER(item);
   
	spellno = select_spell();
	if (spellno == -1) return ;

	/* if exec_page returns FALSE, we don't have enough mana and aborted */
	if (exec_page(spellno))
	{
		/* Take a turn */
		p_ptr->energy_use = 100;
	}
}


/*** Using items the traditional way ***/

/* Determine if the player can read scrolls. */
static bool obj_read_pre(void)
{
	if (p_ptr->timed[TMD_BLIND])
	{
		msg_print("You can't see anything.");
		return FALSE;
	}

	if (no_lite())
	{
		msg_print("You have no light to read by.");
		return FALSE;
	}

	if (p_ptr->timed[TMD_CONFUSED])
	{
		msg_print("You are too confused to read!");
		return FALSE;
	}

	if (p_ptr->timed[TMD_AMNESIA])
	{
		msg_print("You can't remember how to read!");
		return FALSE;
	}

	return TRUE;
}

/* Basic tval testers */
static bool obj_is_staff(const object_type *o_ptr)  { return o_ptr->tval == TV_STAFF; }
static bool obj_is_wand(const object_type *o_ptr)   { return o_ptr->tval == TV_WAND; }
static bool obj_is_potion(const object_type *o_ptr) { return o_ptr->tval == TV_POTION; }
/* Scrolls and spell pages can be read */
static bool obj_is_scroll(const object_type *o_ptr) { return ((o_ptr->tval == TV_SCROLL) || (o_ptr->tval == TV_SPELL)); } 
static bool obj_is_food(const object_type *o_ptr)   { return o_ptr->tval == TV_FOOD; }

/* Determine if an object is zappable */
static bool obj_can_zap(const object_type *o_ptr)
{
	const object_kind *k_ptr = &k_info[o_ptr->k_idx];
	if (o_ptr->tval != TV_ROD) return FALSE;

	/* All still charging? */
	if (o_ptr->number <= (o_ptr->timeout + (k_ptr->time_base - 1)) / k_ptr->time_base) return FALSE;

	/* Otherwise OK */
	return TRUE;
}

/* Determine if an object is activatable */
static bool obj_can_activate(const object_type *o_ptr)
{
	u32b f[OBJ_FLAG_N];

	/* Not known */
	if (!object_known_p(o_ptr)) return (FALSE);

	/* Check the recharge */
	if (o_ptr->timeout) return (FALSE);

	/* Extract the flags */
	object_flags(o_ptr, f); 

	/* Check activation flag */
	return (f[3] & TR3_ACTIVATE) ? TRUE : FALSE;
}


/* Use a staff */
static void obj_use_staff(object_type *o_ptr, int item)
{
	do_cmd_use(o_ptr, item, MSG_USE_STAFF, USE_CHARGE);
}

/* Aim a wand */
static void obj_use_wand(object_type *o_ptr, int item)
{
	do_cmd_use(o_ptr, item, MSG_ZAP_ROD, USE_CHARGE);
}

/* Zap a rod */
static void obj_use_rod(object_type *o_ptr, int item)
{
	do_cmd_use(o_ptr, item, MSG_ZAP_ROD, USE_TIMEOUT);
}

/* Activate a wielded object */
static void obj_activate(object_type *o_ptr, int item)
{
	do_cmd_use(o_ptr, item, MSG_ACT_ARTIFACT, USE_TIMEOUT);
}

/* Eat some food */
static void obj_use_food(object_type *o_ptr, int item)
{
	do_cmd_use(o_ptr, item, MSG_EAT, USE_SINGLE);
}

/* Quaff a potion (from the pack or the floor) */
static void obj_use_potion(object_type *o_ptr, int item)
{
	do_cmd_use(o_ptr, item, MSG_QUAFF, USE_SINGLE);
}

/* Read a scroll or spell page (from the pack or floor) */
static void obj_use_scroll(object_type *o_ptr, int item)
{
	/* TODO Spell pages should not have USE_SINGLE. */
	if(o_ptr->tval == TV_SCROLL)
		do_cmd_use(o_ptr, item, MSG_GENERIC, USE_SINGLE);
	else
		do_cmd_read_spell(o_ptr, item, MSG_GENERIC, USE_SINGLE);
}

/*** Refuelling ***/

static bool obj_refill_pre(void)
{
   	object_type *o_ptr;

	o_ptr = &inventory[INVEN_LITE];

	if (o_ptr->tval != TV_LITE)
	{
		msg_print("You are not wielding a light.");
		return FALSE;
	} 
/* 
 * Can only refuel lanterns, Noldor lanterns and torches 
 *
 * Artifacts don't need fuelling.
 */
	else if (!HAS_FUEL(o_ptr) && !o_ptr->name1)
	{
		msg_print("Your light cannot be refilled.");
		return FALSE;
	}

	return TRUE;
}

static bool obj_can_refill(const object_type *o_ptr)
{
	const object_type *j_ptr = &inventory[INVEN_LITE];

	if (j_ptr->sval == SV_LITE_LANTERN)
	{
		/* Flasks of oil are okay */
		if (o_ptr->tval == TV_FLASK) return (TRUE);
	}

	/* Non-empty, non-everburning sources are okay */
	if ((o_ptr->tval == TV_LITE) &&
	    (o_ptr->sval == j_ptr->sval) &&
	    (o_ptr->timeout > 0)) /* TODO Don't need to check for everburning because dest is same as source? */
	{
		return (TRUE);
	}

	/* Assume not okay */
	return (FALSE);
}

static void obj_refill(object_type *o_ptr, int item)
{
	object_type *j_ptr = &inventory[INVEN_LITE];
	p_ptr->energy_use = 50;

	/* It's a lamp */
	if (j_ptr->sval == SV_LITE_LANTERN)
		refill_lamp(j_ptr, o_ptr, item);

	/* It's a torch */
	else if (j_ptr->sval == SV_LITE_TORCH)
		refuel_torch(j_ptr, o_ptr, item);
}




/*** Handling bits ***/

/* Item "action" type */
typedef struct
{
	void (*action)(object_type *, int);
	const char *desc;

	const char *prompt;
	const char *noop;

	bool (*filter)(const object_type *o_ptr);
	int mode;
	bool (*prereq)(void);
} item_act_t;


/* All possible item actions */
static item_act_t item_actions[] =
{
	/* Not setting IS_HARMLESS for this one because it could cause a true
	 * dangerous command to not be prompted, later.
	 */
	{ obj_uninscribe, "uninscribe",
	  "Un-inscribe which item? ", "You have nothing to un-inscribe.",
	  obj_has_inscrip, (USE_EQUIP | USE_INVEN | USE_FLOOR), NULL },

	{ obj_inscribe, "inscribe",
	  "Inscribe which item? ", "You have nothing to inscribe.",
	  NULL, (USE_EQUIP | USE_INVEN | USE_FLOOR | IS_HARMLESS), NULL },

	{ obj_examine, "examine",
	  "Examine which item? ", "You have nothing to examine.",
	  NULL, (USE_EQUIP | USE_INVEN | USE_FLOOR | IS_HARMLESS), NULL },

	/*** Takeoff/drop/wear ***/
	{ obj_takeoff, "takeoff",
	  "Take off which item? ", "You are not wearing anything you can take off.",
	  obj_can_takeoff, USE_EQUIP, NULL },

	{ obj_wear, "wield",
	  "Wear/Wield which item? ", "You have nothing you can wear or wield.",
	  obj_can_wear, (USE_INVEN | USE_FLOOR), NULL },

	{ obj_drop, "drop",
	  "Drop which item? ", "You have nothing to drop.",
	  NULL, (USE_EQUIP | USE_INVEN), NULL },

	/*** Spellbooks ***/
	{ obj_browse, "browse",
	  "Browse which book? ", "You have no books that you can read.",
	  obj_can_browse, (USE_INVEN | USE_FLOOR | IS_HARMLESS), NULL },
/*	{ obj_study, "study",
	  "Study which book? ", "You have no books that you can read.",
	  obj_can_browse, (USE_INVEN | USE_FLOOR), obj_study_pre },  */

/* TODO Do we need something here instead of obj_cast ? */
	{ obj_cast, "cast",
	  "Use which book? ", "You have no books that you can read.",
	  obj_can_browse, (USE_INVEN | USE_FLOOR), obj_cast_pre },  

	/*** Item usage ***/
	{ obj_use_staff, "use",
	  "Use which staff? ", "You have no staff to use.",
	  obj_is_staff, (USE_INVEN | USE_FLOOR), NULL },

	{ obj_use_wand, "aim",
      "Aim which wand? ", "You have no wand to aim.",
	  obj_is_wand, (USE_INVEN | USE_FLOOR), NULL },

	{ obj_use_rod, "zap",
      "Zap which rod? ", "You have no charged rods to zap.",
	  obj_can_zap, (USE_INVEN | USE_FLOOR), NULL },

	{ obj_activate, "activate",
      "Activate which item? ", "You have nothing to activate.",
	  obj_can_activate, USE_EQUIP, NULL },

	{ obj_use_food, "eat",
      "Eat which item? ", "You have nothing to eat.",
	  obj_is_food, (USE_INVEN | USE_FLOOR), NULL },

	{ obj_use_potion, "quaff",
      "Quaff which potion? ", "You have no potions to quaff.",
	  obj_is_potion, (USE_INVEN | USE_FLOOR), NULL },

	{ obj_use_scroll, "read",
      "Read what? ", "You have no scrolls or spells to read.",
	  obj_is_scroll, (USE_INVEN | USE_FLOOR), obj_read_pre },

	{ obj_refill, "refill",
      "Refuel with what fuel source? ", "You have nothing to refuel with.",
	  obj_can_refill, (USE_INVEN | USE_FLOOR), obj_refill_pre },
};


/* List matching up to item_actions[] */
typedef enum
{
	ACTION_UNINSCRIBE = 0,
	ACTION_INSCRIBE,
	ACTION_EXAMINE,
	ACTION_TAKEOFF,
	ACTION_WIELD,
	ACTION_DROP,

	ACTION_BROWSE,
	ACTION_CAST,

	ACTION_USE_STAFF,
	ACTION_AIM_WAND,
	ACTION_ZAP_ROD,
	ACTION_ACTIVATE,
	ACTION_EAT_FOOD,
	ACTION_QUAFF_POTION,
	ACTION_READ_SCROLL,
	ACTION_REFILL
} item_act;



/*** Old-style noun-verb functions ***/

/* Generic "do item action" function */
static void do_item(item_act act)
{
	int item;
	object_type *o_ptr;

	cptr q, s;

	if (item_actions[act].prereq)
	{
		if (!item_actions[act].prereq())
			return;
	}

	/* Get item */
	q = item_actions[act].prompt;
	s = item_actions[act].noop;
	item_tester_hook = item_actions[act].filter;
	if (!get_item(&item, q, s, item_actions[act].mode)) return;

	/* Get the item */
	if (item >= 0)
		o_ptr = &inventory[item];
	else
		o_ptr = &o_list[0 - item];

	item_actions[act].action(o_ptr, item);
}

void add_spells_mage_staff(object_type *o_ptr)
{
	bool found[MAX_SPELLS_PER_ITEM];
	s16b index[MAX_SPELLS_PER_ITEM];
	char buf;
	s16b i, j, cnt=0, cntlow=0, spells=0, spellno, unbound_spells = 0;
	bool add_spells = FALSE;

	for (i=0; i < z_info->s_max; i++)
	{
		if (has_spell(o_ptr, i)) index[spells++]=i;
	}
	if ((spells>0) && (o_ptr->timeout))
	{
		msg_print("Your staff vibrates, but nothing happens.");
		return;
	}

	if ((o_ptr->name2 == EGO_STAFF_MAGI) && (spells==1))
	{
		msg_print("Your staff comes to life!");
		cast_spell(index[0]); /* was exec_spell */
		o_ptr->timeout = 100 + (s16b) randint0(200);
		return;
	}

	if (spells)
	{
		if ( ((o_ptr->name2 == EGO_STAFF_ADEPT) && (spells<2)) ||
			  ((o_ptr->name2 == EGO_STAFF_ARCHMAGE) && (spells<3)) )
		{
			prt("", 0, 0);
			buf='x';
			while ((buf != 'A') && (buf != 'U'))
			{
				add_spells=get_com("Do you want to try to Add a spell to the staff or Use it?(A/U):", &buf);
				if ((buf=='a')) buf='A';
				if ((buf=='u')) buf='U';
				add_spells=(buf=='A');
			}
		}
	}
	else /* no spells yet */
	{
		add_spells = TRUE;
	}

	if (!add_spells)
	{
		/* Save the screen */
		Term_save();

		print_spells(index, spells);

		/* Clear the top line */
		prt("", 0, 0);

		/* Prompt user */
		if (!get_com("Which spell? ", &buf))
		{
			Term_load();
			return;
		}

		spellno=A2I(buf);
		if ((spellno<0) || (spellno >= spells))
		{
			Term_load();
			return;
		}
		Term_load();
		msg_print("Your staff comes to life!");
		cast_spell(index[spellno]); /* was exec_spell */
		o_ptr->timeout = 100 + (s16b) randint0(200);
	}
	else /* add spells */
	{
		for (j=0; j< z_info->s_max; j++) found[j]=FALSE;

		for (i=0; i<INVEN_PACK; i++)
		{
			if (inventory[i].tval == TV_SPELL)
			{
				unbound_spells++;
			}
			if (inventory[i].tval == TV_BOOK)
			{
				for (j=0; j < z_info->s_max; j++)
				{
					if (has_spell(&inventory[i], j))
					{
						if (!found[j])
						{
							index[cnt++]=j;
							if (s_info[j].scale <= 2) cntlow++;
						}
						found[j]=TRUE;
					}
				}
			}
		}
		if (!cnt)
		{
			if (!unbound_spells)
			{
				msg_print("You have no spells for the staff to adsorb!");
			}
			else
			{
				msg_print("You can only absorb spells bound in a book in your staff!");
			}
			return;
		}
		if ( ((o_ptr->name2 == EGO_STAFF_MAGI) && (cnt<4)) ||
			  ((o_ptr->name2 == EGO_STAFF_ADEPT) && ((cnt<8) || (cntlow<3))) ||
			  ((o_ptr->name2 == EGO_STAFF_ARCHMAGE) && ((cnt<12) || (cntlow<5))) )
		{
			msg_print("The staff flickers for a moment, but nothing happens.");
			return;
		}
		j = (s16b) randint1(cnt); /* TODO Check if should be randint0 */
		set_spell(o_ptr, index[j]);
		msg_format("The staff hums as it absorbs the spell of %s.",
					  s_info[index[j]].name + s_name);
	}
}

/* Wrappers */
void do_cmd_uninscribe(void) { do_item(ACTION_UNINSCRIBE); }
void do_cmd_inscribe(void) { do_item(ACTION_INSCRIBE); }
void do_cmd_observe(void) { do_item(ACTION_EXAMINE); }
void do_cmd_takeoff(void) { do_item(ACTION_TAKEOFF); }
void do_cmd_wield(void) { do_item(ACTION_WIELD); }
void do_cmd_drop(void) { do_item(ACTION_DROP); }
void do_cmd_browse(void) { do_item(ACTION_BROWSE); }
void do_cmd_cast(void) { do_item(ACTION_CAST); }
void do_cmd_pray(void) { do_item(ACTION_CAST); }
void do_cmd_use_staff(void) { do_item(ACTION_USE_STAFF); }
void do_cmd_aim_wand(void) { do_item(ACTION_AIM_WAND); }
void do_cmd_zap_rod(void) { do_item(ACTION_ZAP_ROD); }
void do_cmd_activate(void) { do_item(ACTION_ACTIVATE); }
void do_cmd_eat_food(void) { do_item(ACTION_EAT_FOOD); }
void do_cmd_quaff_potion(void) { do_item(ACTION_QUAFF_POTION); }
void do_cmd_read_scroll(void) { do_item(ACTION_READ_SCROLL); }
void do_cmd_refill(void) { do_item(ACTION_REFILL); }
