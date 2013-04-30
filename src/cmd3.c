/* File: cmd3.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 *
 * UnAngband (c) 2001-6 Andrew Doull. Modifications to the Angband 2.9.1
 * source code are released under the Gnu Public License. See www.fsf.org
 * for current GPL license details. Addition permission granted to
 * incorporate modifications in all Angband variants as defined in the
 * Angband variants FAQ. See rec.games.roguelike.angband for FAQ.
 */

#include "angband.h"






/*
 * Display inventory
 */
void do_cmd_inven(void)
{
	/* Hack -- Start in "inventory" mode */
	p_ptr->command_wrk = (USE_INVEN);

	/* Save screen */
	screen_save();

	/* Hack -- show empty slots */
	item_tester_full = TRUE;

	/* Display the inventory */
	show_inven();

	/* Hack -- hide empty slots */
	item_tester_full = FALSE;

	/* Prompt for a command */
	prt("(Inventory) Command: ", 0, 0);

	/* Hack -- Get a new command */
	p_ptr->command_new = inkey_ex();

	/* Load screen */
	screen_load();


	/* Hack -- Process "Escape" */
	if (p_ptr->command_new.key == ESCAPE)
	{
		/* Reset stuff */
		p_ptr->command_new.key = 0;
	}

	/* Hack -- Process normal keys */
	else
	{
		/* Hack -- Use "display" mode */
		p_ptr->command_see = TRUE;
	}
}


/*
 * Display equipment
 */
void do_cmd_equip(void)
{
	/* Hack -- Start in "equipment" mode */
	p_ptr->command_wrk = (USE_EQUIP);

	/* Save screen */
	screen_save();

	/* Hack -- show empty slots */
	item_tester_full = TRUE;

	/* Display the equipment */
	show_equip();

	/* Hack -- undo the hack above */
	item_tester_full = FALSE;

	/* Prompt for a command */
	prt("(Equipment) Command: ", 0, 0);

	/* Hack -- Get a new command */
	p_ptr->command_new = inkey_ex();

	/* Load screen */
	screen_load();


	/* Hack -- Process "Escape" */
	if (p_ptr->command_new.key == ESCAPE)
	{
		/* Reset stuff */
		p_ptr->command_new.key = 0;
	}

	/* Hack -- Process normal keys */
	else
	{
		/* Enter "display" mode */
		p_ptr->command_see = TRUE;
	}
}


/*
 * The "wearable" tester
 */
static bool item_tester_hook_wear(const object_type *o_ptr)
{
	/* Check if wearable/wieldable */
	if (wield_slot(o_ptr) >= INVEN_WIELD) return (TRUE);

	/* Assume not wearable */
	return (FALSE);
}


static int quiver_wield(int item, object_type *i_ptr)
{
  int slot = 0;
  bool use_new_slot = FALSE;
  int num;
  int i;

  /* was: num = get_quantity(NULL, i_ptr->number);*/
  num = i_ptr->number;

  /* Cancel */
  if (num <= 0) return 0;

  /* Check free space */
  if (!quiver_carry_okay(i_ptr, num, item))
    {
      msg_print("Your quiver needs more backpack space.");
      return 0;
    }

  /* Search for available slots. */
  for (i = INVEN_QUIVER; i < END_QUIVER; i++)
    {
      object_type *o_ptr;

      /* Get the item */
      o_ptr = &inventory[i];

      /* Accept empty slot, but keep searching for combining */
      if (!o_ptr->k_idx)
	{
	  slot = i;
	  use_new_slot = TRUE;
	  continue;
	}

      /* Accept slot that has space to absorb more */
      if (object_similar(o_ptr, i_ptr))
	{
	  slot = i;
	  use_new_slot = FALSE;
	  object_absorb(o_ptr, i_ptr, FALSE);
	  break;
	}
    }

  if (!slot)
    {
      /* No space. */
      msg_print("Your quiver is full.");
      return 0;
    }

  /* Use a new slot if needed */
  if (use_new_slot) 
    object_copy(&inventory[slot], i_ptr);

  /* Update "p_ptr->pack_size_reduce" */
  find_quiver_size();

  /* Reorder the quiver and return the perhaps modified slot */
  return reorder_quiver(slot);
}

/*
 * Mark the item as known cursed.
 */
void mark_cursed_feeling(object_type *o_ptr)
{
  switch (o_ptr->feeling)
    {
    case INSCRIP_ARTIFACT: o_ptr->feeling = INSCRIP_TERRIBLE; break;
    case INSCRIP_HIGH_EGO_ITEM: o_ptr->feeling = INSCRIP_WORTHLESS; break;
    case INSCRIP_EGO_ITEM: o_ptr->feeling = INSCRIP_WORTHLESS; break;
    default: o_ptr->feeling = INSCRIP_CURSED;
    }

  /* The object has been "sensed" */
  o_ptr->ident |= (IDENT_SENSE);
}


/*
 * Wield or wear a single item from the pack or floor
 * Now supports wearing multiple rings.
 */
void do_cmd_wield(void)
{
	int item, slot;

	object_type *o_ptr;
	object_type *j_ptr;
	object_type *i_ptr;
	object_type object_type_body;

	cptr act;

	cptr q, s;

	char o_name[80];

	u32b f1, f2, f3, f4;
	u32b n1, n2, n3, n4;
	u32b k1, k2, k3, k4;

	bool get_feat = FALSE;

	/* Hack -- Prevent player from wielding conflicting items */
	bool burn = FALSE;
	bool curse = FALSE;

	/* Hack -- Allow multiple rings to be wielded */
	int amt = 1;
	int rings = 0;

	/* Restrict the choices */
	item_tester_hook = item_tester_hook_wear;

	/* Get an item */
	q = "Wear/Wield which item? ";
	s = "You have nothing you can wear or wield.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR | USE_FEATG))) return;

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}

	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item];
	}

	/* In a bag? */
	if (o_ptr->tval == TV_BAG)
	{
		/* Get item from bag */
		if (!get_item_from_bag(&item, q, s, o_ptr)) return;

		/* Refer to the item */
		o_ptr = &inventory[item];
	}

	/* Got from feature */
	if (o_ptr->ident & (IDENT_STORE)) get_feat = TRUE;

	/* Check the slot */
	slot = wield_slot(o_ptr);

	/* Hack -- slot not allowed */
	if (slot < 0) return;

	/* Hack -- don't dual wield */
	if (slot == INVEN_ARM 
	    && o_ptr->tval != TV_SHIELD
	    && o_ptr->tval != TV_RING
	    && o_ptr->tval != TV_AMULET)
	  {
	    if (!get_check("Wield it in your off-hand? "))
	      {
		slot = INVEN_WIELD;
	      }
	  }

	/* Hack -- multiple rings */
	else if (o_ptr->tval == TV_RING)
	{
		i_ptr = &inventory[slot];

		/* Wear multiple rings */
		if (object_similar(o_ptr,i_ptr)) rings = 5-i_ptr->number;

		/* Get a quantity - take off existing rings */
		if (!rings) amt = get_quantity(NULL, (o_ptr->number < 5) ? o_ptr->number : 5);

		/* Get a quantity - add to existing rings */
		else amt = get_quantity(NULL, (o_ptr->number < rings) ? o_ptr->number : rings);

		/* Allow user abort */
		if (amt <= 0) return;
	}

	/* Hack - Throwing weapons can we wielded in the quiver too. 
	   Ask the player, unless he has already chosen the off-hand. */
	if ( is_known_throwing_item(o_ptr) 
		 && !IS_QUIVER_SLOT(slot) 
		 && slot != INVEN_ARM
		 && get_check("Do you want to put it in the quiver? ") ) 
		slot = INVEN_QUIVER;

	/* Source and destination identical */
	if (item == slot) return;

	/* Adjust amount */
	if (IS_QUIVER_SLOT(slot)) amt = o_ptr->number;

	/* Prevent wielding over 'built-in' item */
	if (!IS_QUIVER_SLOT(slot) && ((inventory[slot].ident & (IDENT_STORE)) != 0))
	{
		/* Describe it */
		object_desc(o_name, sizeof(o_name), &inventory[slot], FALSE, 0);

		/* Oops */
		msg_format("You cannot remove the %s.", o_name);

		/* Nope */
		return;
	}
	/* Prevent wielding into a cursed slot */
	else if (!IS_QUIVER_SLOT(slot) && cursed_p(&inventory[slot]))
	{
		/* Describe it */
		object_desc(o_name, sizeof(o_name), &inventory[slot], FALSE, 0);

		/* Message */
		msg_format("The %s you are %s appears to be cursed.",
		   o_name, describe_use(slot));

		mark_cursed_feeling(&inventory[slot]);

		/* Cancel the command */
		return;
	}
	/* Prevent wielding from cursed slot */
	else if ((item >= INVEN_WIELD) && cursed_p(&inventory[item]))
	{
		/* Describe it */
		object_desc(o_name, sizeof(o_name), &inventory[item], FALSE, 0);

		/* Message */
		msg_format("The %s you are %s appears to be cursed.",
		   o_name, describe_use(item));

		mark_cursed_feeling(&inventory[item]);

		/* Cancel the command */
		return;
	}

	/* Take a turn here so that racial object sensing is not free */
	switch (o_ptr->tval)
	{
		case TV_SWORD:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_DIGGING:
		{
			if (o_ptr->weight >= 200) p_ptr->energy_use = 100;
			else p_ptr->energy_use = 50;
			break;
		}
		case TV_BOW:
		{
			if (o_ptr->sval >= SV_HAND_XBOW) p_ptr->energy_use = 100;
			else p_ptr->energy_use = 50;
			break;
		}
		default:
		{
			p_ptr->energy_use = 100;
			break;
		}
	}

	/* Get object flags */
	object_flags(o_ptr,&f1,&f2,&f3,&f4);

	/* Check for racial conflicts */
	burn |= (f1 & (TR1_SLAY_NATURAL)) & (p_ptr->cur_flags4 & (TR4_ANIMAL));
	burn |= (f1 & (TR1_SLAY_ORC)) & (p_ptr->cur_flags4 & (TR4_ORC));
	burn |= (f1 & (TR1_SLAY_TROLL)) & (p_ptr->cur_flags4 & (TR4_TROLL));
	burn |= (f1 & (TR1_SLAY_GIANT)) & (p_ptr->cur_flags4 & (TR4_GIANT));
	burn |= (f4 & (TR4_SLAY_MAN)) & (p_ptr->cur_flags4 & (TR4_MAN));
	burn |= (f4 & (TR4_SLAY_ELF)) & (p_ptr->cur_flags4 & (TR4_ELF));
	burn |= (f4 & (TR4_SLAY_DWARF)) & (p_ptr->cur_flags4 & (TR4_DWARF));
	burn |= (f4 & (TR4_ANIMAL)) & (p_ptr->cur_flags1 & (TR1_SLAY_NATURAL));
	burn |= (f4 & (TR4_ORC)) & (p_ptr->cur_flags1 & (TR1_SLAY_ORC));
	burn |= (f4 & (TR4_TROLL)) & (p_ptr->cur_flags1 & (TR1_SLAY_TROLL));
	burn |= (f4 & (TR4_GIANT)) & (p_ptr->cur_flags1 & (TR1_SLAY_GIANT));
	burn |= (f4 & (TR4_MAN)) & (p_ptr->cur_flags4 & (TR4_SLAY_MAN));
	burn |= (f4 & (TR4_DWARF)) & (p_ptr->cur_flags4 & (TR4_SLAY_DWARF));
	burn |= (f4 & (TR4_ELF)) & (p_ptr->cur_flags4 & (TR4_SLAY_ELF));

	/* Evil players can wield racial conflict items but get cursed instead of burning */
	if ((burn != 0) && ((f4 & (TR4_EVIL)) || ((p_ptr->cur_flags4 & (TR4_EVIL)) != 0)))
	{
		curse = TRUE;
		burn = FALSE;
	}

	/* Check for elemental conflicts and high slays */
	burn |= (f1 & (TR1_SLAY_DRAGON)) & (p_ptr->cur_flags4 & (TR4_DRAGON));
	burn |= (f1 & (TR1_SLAY_UNDEAD)) & (p_ptr->cur_flags4 & (TR4_UNDEAD));
	burn |= (f1 & (TR1_SLAY_DEMON)) & (p_ptr->cur_flags4 & (TR4_DEMON));
	burn |= (f1 & (TR1_KILL_DEMON)) & (p_ptr->cur_flags4 & (TR4_DEMON));
	burn |= (f1 & (TR1_KILL_UNDEAD)) & (p_ptr->cur_flags4 & (TR4_UNDEAD));
	burn |= (f1 & (TR1_KILL_DRAGON)) & (p_ptr->cur_flags4 & (TR4_DRAGON));
	burn |= (f1 & (TR1_BRAND_HOLY)) & (p_ptr->cur_flags4 & (TR4_EVIL));
	burn |= (f1 & (TR1_BRAND_POIS)) & (p_ptr->cur_flags4 & (TR4_HURT_POIS));
	burn |= (f1 & (TR1_BRAND_ACID)) & (p_ptr->cur_flags4 & (TR4_HURT_ACID));
	burn |= (f1 & (TR1_BRAND_COLD)) & (p_ptr->cur_flags4 & (TR4_HURT_COLD));
	burn |= (f1 & (TR1_BRAND_ELEC)) & (p_ptr->cur_flags4 & (TR4_HURT_ELEC));
	burn |= (f1 & (TR1_BRAND_FIRE)) & (p_ptr->cur_flags4 & (TR4_HURT_FIRE));
	burn |= (f4 & (TR4_BRAND_LITE)) & (p_ptr->cur_flags4 & (TR4_HURT_LITE));

	burn |= (f4 & (TR4_EVIL)) & (p_ptr->cur_flags1 & (TR1_BRAND_HOLY));
	burn |= (f4 & (TR4_DRAGON)) & (p_ptr->cur_flags1 & (TR1_SLAY_DRAGON));
	burn |= (f4 & (TR4_UNDEAD)) & (p_ptr->cur_flags1 & (TR1_SLAY_UNDEAD));
	burn |= (f4 & (TR4_DEMON)) & (p_ptr->cur_flags1 & (TR1_SLAY_DEMON));
	burn |= (f4 & (TR4_DRAGON)) & (p_ptr->cur_flags1 & (TR1_KILL_DRAGON));
	burn |= (f4 & (TR4_UNDEAD)) & (p_ptr->cur_flags1 & (TR1_KILL_UNDEAD));
	burn |= (f4 & (TR4_DEMON)) & (p_ptr->cur_flags1 & (TR1_KILL_DEMON));
	burn |= (f4 & (TR4_HURT_POIS)) & (p_ptr->cur_flags1 & (TR1_BRAND_POIS));
	burn |= (f4 & (TR4_HURT_ACID)) & (p_ptr->cur_flags1 & (TR1_BRAND_ACID));
	burn |= (f4 & (TR4_HURT_COLD)) & (p_ptr->cur_flags1 & (TR1_BRAND_COLD));
	burn |= (f4 & (TR4_HURT_ELEC)) & (p_ptr->cur_flags1 & (TR1_BRAND_ELEC));
	burn |= (f4 & (TR4_HURT_FIRE)) & (p_ptr->cur_flags1 & (TR1_BRAND_FIRE));
	burn |= (f4 & (TR4_HURT_LITE)) & (p_ptr->cur_flags4 & (TR4_BRAND_LITE));

	if (burn != 0)
	{
		/* Warn the player */
		msg_print("Aiee! It feels burning hot!");

		/* Mark object as ungettable? */
		if (!(o_ptr->feeling) &&
			!(o_ptr->ident & (IDENT_SENSE))
			&& !(object_named_p(o_ptr)))
		{
	
			/* Sense the object */
			o_ptr->discount = INSCRIP_UNGETTABLE;
	
			/* The object has been "sensed" */
			o_ptr->ident |= (IDENT_SENSE);
		}

		return; /* The turn already taken for racial sensing */
	}

	/* Get local object */
	i_ptr = &object_type_body;

	/* Obtain local object */
	object_copy(i_ptr, o_ptr);

	/* Modify quantity */
	i_ptr->number = amt;

	/* Get a new show index */
	if (o_ptr->number > amt)
	{
		int j, k;

		/* Find the next free show index */
		for (j = 1; j < SHOWN_TOTAL; j++)
		{
			bool used = FALSE;

			/* Check all items */
			for (k = 0; k < INVEN_TOTAL; k++) if ((inventory[k].k_idx) && (inventory[k].show_idx == j)) used = TRUE;

			/* Already an item using this slot? */
			if (used) continue;

			/* Use this slot */
			break;
		}

		/* Set the show index for the item */
		if (j < SHOWN_TOTAL) i_ptr->show_idx = j;
		else i_ptr->show_idx = 0;

		/* Redraw stuff */
		p_ptr->redraw |= (PR_ITEM_LIST);
	}

	/* Ammo goes in quiver slots, which have special rules. */
	if (IS_QUIVER_SLOT(slot))
	  {
	    /* Put the ammo in the quiver */
	    slot = quiver_wield(item, i_ptr);

	    /* Can't do it; note the turn already wasted for racial sensing */
	    if (!slot) return;
	  }

	/* Reset stackc; assume no wands in quiver */
	i_ptr->stackc = 0;

	/* Sometimes use lower stack object */
	if (!object_charges_p(o_ptr) 
	    && (rand_int(o_ptr->number) < o_ptr->stackc))
	  {
	    if (amt >= o_ptr->stackc)
	      {
		i_ptr->stackc = o_ptr->stackc;

		o_ptr->stackc = 0;
	      }
	    else
	      {
		if (i_ptr->charges) i_ptr->charges--;
		if (i_ptr->timeout) i_ptr->timeout = 0;

		o_ptr->stackc -= amt;
	      }
	  }

	/* Decrease the item (from the pack); cancelling no longer possible */
	if (item >= 0)
	{
		/* Hack -- Forget what original stack may do */
		/* This allows us to identify the wielded item's function */
		if (o_ptr->number < amt) drop_may_flags(o_ptr);

		inven_item_increase(item, -amt);
		inven_item_optimize(item);

	}
	/* Decrease the item (from the floor) */
	else
	{
		floor_item_increase(0 - item, -amt);
		floor_item_optimize(0 - item);
		if (get_feat && (scan_feat(p_ptr->py,p_ptr->px) < 0)) cave_alter_feat(p_ptr->py,p_ptr->px,FS_GET_FEAT);
	}

	/* Get the wield slot */
	j_ptr = &inventory[slot];


	/* Special rules for similar rings */
	if (rings) 
	  {
	    /* Wear the new rings */
	    object_absorb(j_ptr, i_ptr, FALSE);
	  }
	else if (!IS_QUIVER_SLOT(slot))
	/* Normal rules for non-ammo */
	  {
	    /* Take off existing item */
	    if (j_ptr->k_idx)
	      (void)inven_takeoff(slot, 255);

	    /* Wear the new stuff */
	    object_copy(j_ptr, i_ptr);

	    /* Increment the equip counter by hand */
	    p_ptr->equip_cnt++;
	  }

	/* Increase the weight */
	p_ptr->total_weight += i_ptr->weight * amt;

	/* Important - clear this flag */
	j_ptr->ident &= ~(IDENT_STORE);

	/* Where is the item now */
	if (slot == INVEN_WIELD)
	{
		act = "You are wielding";
	}
	else if (slot == INVEN_ARM)
	{
		if (j_ptr->tval == TV_SHIELD) act = "You are wearing";
		else act = "You are off-hand wielding";
	}
	else if (j_ptr->tval == TV_INSTRUMENT)
	{
		act = "You are playing music with";
	}
	else if (slot == INVEN_BOW)
	{
		act = "You are shooting with";
	}
	else if (slot == INVEN_LITE)
	{
		act = "Your light source is";

		/* Light torch if not lit already */
		if (!(artifact_p(j_ptr)) && !(j_ptr->timeout))
		{
			j_ptr->timeout = j_ptr->charges;
			j_ptr->charges = 0;
		}
	}
	else if (!IS_QUIVER_SLOT(slot))
	{
		act = "You are wearing";
	}
	else
	{
		act = "You have readied";
	}

	/* Describe the result */
	object_desc(o_name, sizeof(o_name), j_ptr, TRUE, 3);

	/* Message */
	msg_format("%s %s (%c).", act, o_name, index_to_label(slot));

	/* Cursed! */
	if (curse || cursed_p(j_ptr))
	{
		/* Warn the player */
		msg_print("Oops! It feels deathly cold!");

		mark_cursed_feeling(j_ptr);
	}
	/* Not cursed */
	else
	{
		switch (j_ptr->feeling)
		{
			case INSCRIP_NONMAGICAL: j_ptr->feeling = INSCRIP_AVERAGE; break;
			case INSCRIP_ARTIFACT: j_ptr->feeling = INSCRIP_SPECIAL; break;
			case INSCRIP_HIGH_EGO_ITEM: j_ptr->feeling = INSCRIP_SUPERB; break;
			case INSCRIP_EGO_ITEM: j_ptr->feeling = INSCRIP_EXCELLENT; break;
			case INSCRIP_UNUSUAL: j_ptr->feeling = INSCRIP_MAGICAL; break;
		}
	}

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Recalculate torch */
	p_ptr->update |= (PU_TORCH);

	/* Recalculate mana */
	p_ptr->update |= (PU_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

	/* Update item list */
	p_ptr->redraw |= (PR_ITEM_LIST);

	/* Cannot learn quivered item effects */
	if (IS_QUIVER_SLOT(slot)) return;

	/* Get known flags */
	k1 = j_ptr->can_flags1;
	k2 = j_ptr->can_flags2;
	k3 = j_ptr->can_flags3;
	k4 = j_ptr->can_flags4;

	/* Some flags are instantly known */
	object_flags(j_ptr, &f1, &f2, &f3, &f4);

	/* Hack -- identify pval */
	if (f1 & (TR1_BLOWS | TR1_SHOTS | TR1_SPEED | TR1_STR |
		TR1_INT | TR1_DEX | TR1_CON | TR1_CHR)) j_ptr->ident |= (IDENT_PVAL);

	/* Hack -- the following are obvious from the displayed combat statistics */
	if (f1 & (TR1_BLOWS)) object_can_flags(j_ptr,TR1_BLOWS,0x0L,0x0L,0x0L, FALSE);
	else object_not_flags(j_ptr,TR1_BLOWS,0x0L,0x0L,0x0L, FALSE);

	if (f1 & (TR1_SHOTS)) object_can_flags(j_ptr,TR1_SHOTS,0x0L,0x0L,0x0L, FALSE);
	else object_not_flags(j_ptr,TR1_SHOTS,0x0L,0x0L,0x0L, FALSE);

	/* Hack -- the following are obvious from the displayed combat statistics */
	if (f1 & (TR1_SPEED)) object_can_flags(j_ptr,TR1_SPEED,0x0L,0x0L,0x0L, FALSE);
	else object_not_flags(j_ptr,TR1_SPEED,0x0L,0x0L,0x0L, FALSE);

	/* Hack -- the following are obvious from the displayed stats */
	if (f1 & (TR1_STR)) object_can_flags(j_ptr,TR1_STR,0x0L,0x0L,0x0L, FALSE);
	else object_not_flags(j_ptr,TR1_STR,0x0L,0x0L,0x0L, FALSE);

	if (f1 & (TR1_INT)) object_can_flags(j_ptr,TR1_INT,0x0L,0x0L,0x0L, FALSE);
	else object_not_flags(j_ptr,TR1_INT,0x0L,0x0L,0x0L, FALSE);

	if (f1 & (TR1_WIS)) object_can_flags(j_ptr,TR1_WIS,0x0L,0x0L,0x0L, FALSE);
	else object_not_flags(j_ptr,TR1_WIS,0x0L,0x0L,0x0L, FALSE);

	if (f1 & (TR1_DEX)) object_can_flags(j_ptr,TR1_DEX,0x0L,0x0L,0x0L, FALSE);
	else object_not_flags(j_ptr,TR1_DEX,0x0L,0x0L,0x0L, FALSE);

	if (f1 & (TR1_CON)) object_can_flags(j_ptr,TR1_CON,0x0L,0x0L,0x0L, FALSE);
	else object_not_flags(j_ptr,TR1_CON,0x0L,0x0L,0x0L, FALSE);

	if (f1 & (TR1_CHR)) object_can_flags(j_ptr,TR1_CHR,0x0L,0x0L,0x0L, FALSE);
	else object_not_flags(j_ptr,TR1_CHR,0x0L,0x0L,0x0L, FALSE);

#ifndef ALLOW_OBJECT_INFO_MORE
	/* Hack --- we do these here, because they are too computationally expensive in the 'right' place */
	if (f1 & (TR1_INFRA))
	{
		object_can_flags(j_ptr,TR1_INFRA,0x0L,0x0L,0x0L, FALSE);
	}
	else object_not_flags(j_ptr,TR1_INFRA,0x0L,0x0L,0x0L, FALSE);

	if (f3 & (TR3_LITE))
	{
		object_can_flags(j_ptr,0x0L,0x0L,TR3_LITE,0x0L, FALSE);
	}
	else object_not_flags(j_ptr,0x0L,0x0L,TR3_LITE,0x0L, FALSE);

	/* Hack --- also computationally expensive. But note we notice these only if we don't already */
	/* have these abilities */
	if ((f3 & (TR3_TELEPATHY)) && !(p_ptr->cur_flags3 & (TR3_TELEPATHY)))) object_can_flags(j_ptr,0x0L,0x0L,TR3_TELEPATHY,0x0L, FALSE);
	else object_not_flags(j_ptr,0x0L,0x0L,TR3_TELEPATHY,0x0L, FALSE);

	if ((f3 & (TR3_SEE_INVIS)) && !(p_ptr->cur_flags3 & (TR3_SEE_INVIS)) && (!p_ptr->tim_invis)) object_can_flags(j_ptr,0x0L,0x0L,TR3_SEE_INVIS,0x0L, FALSE);
	else object_not_flags(j_ptr,0x0L,0x0L,TR3_SEE_INVIS,0x0L, FALSE);
#endif

	/* Hack --- the following are either obvious or (relatively) unimportant */
	if (f3 & (TR3_BLESSED))
	{
		object_can_flags(j_ptr,0x0L,0x0L,TR3_BLESSED,0x0L, FALSE);
	}
	else object_not_flags(j_ptr,0x0L,0x0L,TR3_BLESSED,0x0L, FALSE);

	if (f3 & (TR3_LIGHT_CURSE)) object_can_flags(j_ptr,0x0L,0x0L,TR3_LIGHT_CURSE,0x0L, FALSE);
	else object_not_flags(j_ptr,0x0L,0x0L,TR3_LIGHT_CURSE,0x0L, FALSE);

	/* Check for new flags */
	n1 = j_ptr->can_flags1 & ~(k1);
	n2 = j_ptr->can_flags2 & ~(k2);
	n3 = j_ptr->can_flags3 & ~(k3);
	n4 = j_ptr->can_flags4 & ~(k4);

	if (n1 || n2 || n3 || n4) update_slot_flags(slot, n1, n2, n3, n4);
}


/*
 * The "drop" tester
 */
static bool item_tester_hook_droppable(const object_type *o_ptr)
{
	/* Forbid 'built-in' */
	if (o_ptr->ident & (IDENT_STORE)) return (FALSE);

	return (TRUE);
}


/*
 * The "takeoff" tester
 */
static bool item_tester_hook_removable(const object_type *o_ptr)
{
	/* Forbid not droppable */
	if (!item_tester_hook_droppable(o_ptr)) return (FALSE);

	/* Forbid cursed */
	if (o_ptr->ident & (IDENT_CURSED)) return (FALSE);

	return (TRUE);
}


/*
 * Take off an item
 */
void do_cmd_takeoff(void)
{
	int item;

	object_type *o_ptr;

	cptr q, s;

	item_tester_hook = item_tester_hook_removable;

	/* Get an item */
	q = "Take off which item? ";
	s = "You are not wearing anything to take off.";
	if (!get_item(&item, q, s, (USE_EQUIP))) return;

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}

	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item];
	}

	/* Item is 'built-in' to shape*/
	if (!IS_QUIVER_SLOT(item) && ((inventory[item].ident & (IDENT_STORE)) != 0))
	{
		/* Oops */
		msg_print("You cannot remove this.");

		/* Nope */
		return;
	}
	/* Item is cursed */
	else if (cursed_p(o_ptr))
	{
		/* Oops */
		msg_print("Hmmm, it seems to be cursed.");

		mark_cursed_feeling(o_ptr);

		/* Nope */
		return;
	}
	/* Cursed quiver */
	else if (IS_QUIVER_SLOT(item) && p_ptr->cursed_quiver)
	{
		/* Oops */
		msg_print("Your quiver is cursed!");

		/* Nope */
		return;
	}

	/* Take a partial turn */
	p_ptr->energy_use = 50;

	/* Take off the item */
	(void)inven_takeoff(item, 255);
}


/*
 * Drop an item
 */
void do_cmd_drop(void)
{
	int item, amt;

	object_type *o_ptr;

	cptr q, s;

	item_tester_hook = item_tester_hook_droppable;

	/* Get an item */
	q = "Drop which item? ";
	s = "You have nothing to drop.";
	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN))) return;

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}

	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item];
	}

	/* In a bag? */
	if (o_ptr->tval == TV_BAG)
	{
		/* Get item from bag -- hack: cancel to pick the bag itself */
		if (get_item_from_bag(&item, q, s, o_ptr)) o_ptr = &inventory[item];
	}

	/* Get a quantity */
	amt = get_quantity(NULL, o_ptr->number);

	/* Allow user abort */
	if (amt <= 0) return;

	/* Prevent wielding over 'built-in' item */
	if (!IS_QUIVER_SLOT(item) && ((inventory[item].ident & (IDENT_STORE)) != 0))
	{
		/* Oops */
		msg_print("You cannot remove this.");

		/* Nope */
		return;
	}
	/* Hack -- Cannot remove cursed items */
	else if ((item >= INVEN_WIELD) && cursed_p(o_ptr))
	{
		/* Oops */
		msg_print("Hmmm, it seems to be cursed.");

		mark_cursed_feeling(o_ptr);

		/* Nope */
		return;
	}
	/* Cursed quiver */
	else if (IS_QUIVER_SLOT(item) && p_ptr->cursed_quiver)
	{
		/* Oops */
		msg_print("Your quiver is cursed!");

		/* Nope */
		return;
	}

	/* Take a partial turn */
	p_ptr->energy_use = 50;

	/* Drop (some of) the item */
	inven_drop(item, amt);
}



/*
 * Destroy an item
 */
void do_cmd_destroy(void)
{
	int item, amt;
	int old_number;

	object_type *o_ptr;

	char o_name[80];

	char out_val[160];

	cptr q, s;

	bool get_feat = FALSE;

	/* Get an item */
	q = "Destroy which item? ";
	s = "You have nothing to destroy.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR | USE_FEATG))) return;

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}

	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item];
	}

	/* In a bag? */
	if (o_ptr->tval == TV_BAG)
	{
		/* Get item from bag */
		if (!get_item_from_bag(&item, q, s, o_ptr)) return;

		/* Refer to the item */
		o_ptr = &inventory[item];
	}

	/* Get feat */
	if (o_ptr->ident & (IDENT_STORE)) get_feat = TRUE;

	/* Get a quantity */
	amt = get_quantity(NULL, o_ptr->number);

	/* Allow user abort */
	if (amt <= 0) return;

	/* Describe the object */
	old_number = o_ptr->number;
	o_ptr->number = amt;
	object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);
	o_ptr->number = old_number;

	/* Verify destruction */
	if (verify_destroy && !auto_pickup_ignore(o_ptr))
	{
		sprintf(out_val, "Really destroy %s? ", o_name);
		if (!get_check(out_val)) return;
	}

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Containers release contents */
	if ((o_ptr->tval == TV_HOLD) && (o_ptr->name3 > 0))
	{
		if (animate_object(item)) return;

		/* Message */
		msg_format("You cannot destroy %s.", o_name);

		return;
	}

	/* Artifacts cannot be destroyed */
	if (artifact_p(o_ptr))
	{

		/* Message */
		msg_format("You cannot destroy %s.", o_name);

		/* Sense the object if allowed, don't sense ID'ed stuff */
		if ((o_ptr->discount == 0)
		&& !(o_ptr->ident & (IDENT_SENSE))
		 && !(object_named_p(o_ptr)))
		{
			o_ptr->discount = INSCRIP_UNBREAKABLE;

			/* The object has been "sensed" */
			o_ptr->ident |= (IDENT_SENSE);

			/* Combine the pack */
			p_ptr->notice |= (PN_COMBINE);

			/* Window stuff */
			p_ptr->window |= (PW_INVEN | PW_EQUIP);

		}

		/* Done */
		return;

	}

	/* Cursed equipments cannot be destroyed */
	if ((item >= INVEN_WIELD) && cursed_p(o_ptr))
	{
		/* Message */
		msg_format("The %s you are %s appears to be cursed.",
		           o_name, describe_use(item));

		mark_cursed_feeling(o_ptr);

		/* Done */
		return;
	}

	/* Message */
	msg_format("You destroy %s.", o_name);

	/* Sometimes use lower stack object */
	if (!object_charges_p(o_ptr) && (rand_int(o_ptr->number)< o_ptr->stackc))
	{
		if (amt >= o_ptr->stackc)
		{
			o_ptr->stackc = 0;
		}
		else
		{
			o_ptr->stackc -= amt;
		}
	}

	/* Eliminate the item (from the pack) */
	if (item >= 0)
	{
		if (o_ptr->number == amt) inven_drop_flags(o_ptr);

		inven_item_increase(item, -amt);
		inven_item_describe(item);
		inven_item_optimize(item);
	}
	/* Eliminate the item (from the floor) */
	else
	{
		floor_item_increase(0 - item, -amt);
		floor_item_describe(0 - item);
		floor_item_optimize(0 - item);
		if (get_feat && (scan_feat(p_ptr->py,p_ptr->px) < 0)) cave_alter_feat(p_ptr->py,p_ptr->px,FS_GET_FEAT);
	}

}


/*
 * Observe an item which has been *identify*-ed
 */
void do_cmd_observe(void)
{
	int item;

	object_type *o_ptr;

	char o_name[80];

	cptr q, s;

	bool stored = FALSE;

	/* Get an item */
	q = "Examine which item? ";
	s = "You have nothing to examine.";
	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR | USE_FEATG))) return;

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}

	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item];
	}

	/* In a bag? */
	if (o_ptr->tval == TV_BAG)
	{
		/* Get item from bag -- hack: cancel to pick the bag itself */
		if (get_item_from_bag(&item, q, s, o_ptr)) o_ptr = &inventory[item];
	}

	/* Hack - obviously interested enough in item */
	if (o_ptr->ident & (IDENT_STORE))
	{
		o_ptr->ident |= (IDENT_MARKED);

		/* No longer 'stored' */
		o_ptr->ident &= ~(IDENT_STORE);

		stored = TRUE;
	}

	/* Description */
	object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

	/* Describe */
	msg_format("Examining %s...", o_name);

	msg_print("");

	/* Save the screen */
	screen_save();

	/* Describe part of the player */
	if ((item >= 0) && (stored)) screen_self_object(o_ptr, item);

	/* Describe */
	else screen_object(o_ptr);

	(void)anykey();

	/* Store item again */
	if (stored) o_ptr->ident |= (IDENT_STORE);

	/* Load the screen */
	screen_load();
}



/*
 * Remove the inscription from an object
 * XXX Mention item (when done)?
 */
void do_cmd_uninscribe(void)
{
	int item;

	object_type *o_ptr;

	cptr q, s;

	int i;

	/* Get an item */
	q = "Un-inscribe which item? ";
	s = "You have nothing to un-inscribe.";
	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR))) return;

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}

	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item];
	}

	/* In a bag? */
	if (o_ptr->tval == TV_BAG)
	{
		/* Get item from bag */
		if (!get_item_from_bag(&item, q, s, o_ptr)) return;

		/* Refer to the item */
		o_ptr = &inventory[item];
	}

	/* Nothing to remove */
	if (!o_ptr->note)
	{
		msg_print("That item had no inscription to remove.");
		return;
	}

	/* Message */
	msg_print("Inscription removed.");

	/* Remove the incription */
	o_ptr->note = 0;

	/* Combine the pack */
	p_ptr->notice |= (PN_COMBINE);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP);

	/* Prompt to always inscribe? */
	if (!easy_autos) return;

	/* Do we inscribe all these ego items? */
	if (object_named_p(o_ptr) && (o_ptr->name2) && (e_info[o_ptr->name2].note))
	{
		e_info[o_ptr->name2].note = 0;

		/* Process objects */
		for (i = 1; i < o_max; i++)
		{
			/* Get the object */
			object_type *i_ptr = &o_list[i];

			/* Skip dead objects */
			if (!i_ptr->k_idx) continue;

			/* Not matching ego item */
			if (i_ptr->name2 != o_ptr->name2) continue;

			/* Auto-inscribe */
			if (object_named_p(i_ptr) || cheat_auto) i_ptr->note = 0;
		}
	}
	/* Do we inscribe all these object kinds? */
	else if (object_aware_p(o_ptr) && (k_info[o_ptr->k_idx].note))
	{
		k_info[o_ptr->k_idx].note = 0;

		/* Process objects */
		for (i = 1; i < o_max; i++)
		{
			/* Get the object */
			object_type *i_ptr = &o_list[i];

			/* Skip dead objects */
			if (!i_ptr->k_idx) continue;

			/* Not matching ego item */
			if (i_ptr->k_idx != o_ptr->k_idx) continue;

			/* Auto-inscribe */
			if (object_named_p(i_ptr) || cheat_auto) i_ptr->note = 0;
		}
	}
}


/*
 * Inscribe an object with a comment
 */
void do_cmd_inscribe(void)
{
	int item;

	object_type *o_ptr;

	char o_name[80];

	char tmp[80];

	cptr q, s;

	int i;

	/* Get an item */
	q = "Inscribe which item? ";
	s = "You have nothing to inscribe.";
	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR))) return;

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}

	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item];
	}

	/* In a bag? */
	if (o_ptr->tval == TV_BAG)
	{
		/* Get item from bag */
		if (!get_item_from_bag(&item, q, s, o_ptr)) return;

		/* Refer to the item */
		o_ptr = &inventory[item];
	}

	/* Describe the activity */
	object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

	/* Message */
	msg_format("Inscribing %s.", o_name);
	msg_print(NULL);

	/* Start with nothing */
	strcpy(tmp, "");

	/* Use old inscription */
	if (o_ptr->note)
	{
		/* Start with the old inscription */
		strnfmt(tmp, 80, "%s", quark_str(o_ptr->note));
	}

	/* Get a new inscription (possibly empty) */
	if (get_string("Inscription: ", tmp, 80))
	{
		/* Save the inscription */
		o_ptr->note = quark_add(tmp);

		/* Combine the pack */
		p_ptr->notice |= (PN_COMBINE);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);
	}

	/* Inscribe */
	if ((item < 0) && (auto_pickup_ignore(o_ptr))) o_ptr->ident &= ~(IDENT_MARKED);

	/* Prompt to always inscribe? */
	if (!easy_autos) return;

	/* Do we inscribe all these ego items? */
	if (object_named_p(o_ptr) && (o_ptr->name2))
	{
		e_info[o_ptr->name2].note = o_ptr->note;

		/* Process objects */
		for (i = 1; i < o_max; i++)
		{
			/* Get the object */
			object_type *i_ptr = &o_list[i];

			/* Skip dead objects */
			if (!i_ptr->k_idx) continue;

			/* Not matching ego item */
			if (i_ptr->name2 != o_ptr->name2) continue;

			/* Already has inscription */
			if (i_ptr->note) continue;

			/* Auto-inscribe */
			if (object_named_p(i_ptr) || cheat_auto) i_ptr->note = e_info[o_ptr->name2].note;

			/* Ignore */
			if (auto_pickup_ignore(o_ptr)) o_ptr->ident &= ~(IDENT_MARKED);

		}

	}
	/* Do we inscribe all these object kinds? */
	else if (object_aware_p(o_ptr))
	{
		k_info[o_ptr->k_idx].note = o_ptr->note;

		/* Process objects */
		for (i = 1; i < o_max; i++)
		{
			/* Get the object */
			object_type *i_ptr = &o_list[i];

			/* Skip dead objects */
			if (!i_ptr->k_idx) continue;

			/* Not matching ego item */
			if (i_ptr->k_idx != o_ptr->k_idx) continue;

			/* Already has inscription */
			if (i_ptr->note) continue;

			/* Auto-inscribe */
			if (object_named_p(i_ptr) || cheat_auto) i_ptr->note = o_ptr->note;

			/* Ignore */
			if (auto_pickup_ignore(o_ptr)) o_ptr->ident &= ~(IDENT_MARKED);

		}

	}
}

/*
 * An "item_tester_hook" for refilling lanterns
 */
static bool item_tester_refill_lantern(const object_type *o_ptr)
{
	/* Flasks of oil are okay */
	if ((o_ptr->tval == TV_FLASK) && (o_ptr->sval == SV_FLASK_OIL)) return (TRUE);

	/* Non-empty lanterns are okay */
	if ((o_ptr->tval == TV_LITE) &&
	    (o_ptr->sval == SV_LITE_LANTERN) &&
	    (o_ptr->charges > 0))
	{
		return (TRUE);
	}

	/* Assume not okay */
	return (FALSE);
}


/*
 * An "item_tester_hook" for refilling torches
 */
bool item_tester_refill_torch(const object_type *o_ptr)
{
	/* Torches are okay */
	if ((o_ptr->tval == TV_LITE) &&
	    (o_ptr->sval == SV_LITE_TORCH)) return (TRUE);

	/* Assume not okay */
	return (FALSE);
}

/*
 * An "item_tester_hook" for empty flasks
 */
static bool item_tester_empty_flask_or_lite(const object_type *o_ptr)
{
	/* Empty flasks are okay */
	if ((o_ptr->tval == TV_FLASK) && (o_ptr->sval == SV_FLASK_EMPTY)) return (TRUE);

	/* Empty bottles are okay */
	if ((o_ptr->tval == TV_HOLD) && (o_ptr->sval == SV_HOLD_BOTTLE) && !(o_ptr->name3)) return (TRUE);

	/* Lanterns are okay */
	if ((o_ptr->tval == TV_LITE) &&
	    (o_ptr->sval == SV_LITE_LANTERN))
	{
		return (TRUE);
	}

	/* Torches are okay */
	if ((o_ptr->tval == TV_LITE) &&
	    (o_ptr->sval == SV_LITE_TORCH)) return (TRUE);

	/* Assume not okay */
	return (FALSE);
}


/*
 * An "item_tester_hook" for refilling flasks
 */
static bool item_tester_refill_flask(const object_type *o_ptr)
{
	/* Flasks are okay */
	if ((o_ptr->tval == TV_FLASK) && (o_ptr->sval != SV_FLASK_EMPTY)) return (TRUE);

	/* Potions are okay */
	if (o_ptr->tval == TV_POTION)
	{
		return (TRUE);
	}

	/* Assume not okay */
	return (FALSE);
}


/*
 * Fill a flask
 */
void do_cmd_refill(void)
{
	int item, item2;

	object_type *o_ptr;
	object_type *i_ptr;
	object_type *j_ptr;

	object_type object_type_body;

	cptr q, s;

	bool unstack = FALSE;
	bool use_feat = FALSE;
	bool get_feat = FALSE;
	bool relite = FALSE;

	/* Restrict the choices */
	item_tester_hook = item_tester_empty_flask_or_lite;

	/* Hack -- prefer equipment */
	p_ptr->command_wrk = (USE_EQUIP);

	/* Get an item */
	q = "Fill/fuel which item? ";
	s = "You have nothing to fill or fuel.";
	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR))) return;

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}

	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item];
	}

	/* In a bag? */
	if (o_ptr->tval == TV_BAG)
	{
		/* Get item from bag */
		if (!get_item_from_bag(&item, q, s, o_ptr)) return;

		/* Refer to the item */
		o_ptr = &inventory[item];
	}

	if ((o_ptr->tval == TV_LITE) && (o_ptr->sval == SV_LITE_LANTERN))
	{
		/* Restrict the choices */
		item_tester_hook = item_tester_refill_lantern;

		/* Get an item */
		q = "Refill with which source of oil? ";
		s = "You have no sources of oil.";

		if (!get_item(&item2, q, s, (USE_INVEN | USE_FLOOR | USE_FEATU))) return;

	}
	else if ((o_ptr->tval == TV_LITE) && (o_ptr->sval == SV_LITE_TORCH))
	{
		/* Restrict the choices */
		item_tester_hook = item_tester_refill_torch;

		/* Get an item */
		q = "Refuel with which torch? ";
		s = "You have no extra torches.";

		if (!get_item(&item2, q, s, (USE_INVEN | USE_FLOOR | USE_FEATG))) return;
	}

	else
	{
		/* Restrict the choices */
		item_tester_hook = item_tester_refill_flask;

		/* Get an item to fill */
		q = "Fill from where? ";
		s = "You have nothing to fill it with.";

		if (!get_item(&item2, q, s, (USE_INVEN | USE_FLOOR | USE_FEATU))) return;
	}

	/* Get the item (in the pack) */
	if (item2 >= 0)
	{
		j_ptr = &inventory[item2];
	}

	/* Get the item (on the floor) */
	else
	{
		j_ptr = &o_list[0 - item2];
	}

	/* In a bag? */
	if (j_ptr->tval == TV_BAG)
	{
		/* Get item from bag */
		if (!get_item_from_bag(&item2, q, s, j_ptr)) return;

		/* Refer to the item */
		j_ptr = &inventory[item2];
	}

	/* Can't fuel self */
	if (item == item2) return;

	/* Take a partial turn */
	p_ptr->energy_use = 50;

	/* Switch off light source */
	if (o_ptr->timeout)
	{
		o_ptr->charges = o_ptr->timeout;
		o_ptr->timeout = 0;
		relite = TRUE;
	}

	if (o_ptr->number > 1)
	{
		/* Get local object */
		i_ptr = &object_type_body;

		/* Obtain a local object */
		object_copy(i_ptr, o_ptr);

		/* Modify quantity */
		i_ptr->number = 1;

		/* Reset stack counter */
		i_ptr->stackc = 0;

		/* Unstack the used item */
		o_ptr->number--;

		/* Adjust the weight */
		p_ptr->total_weight -= i_ptr->weight;

		o_ptr = i_ptr;

		unstack = TRUE;
	}

	if ((o_ptr->tval == TV_LITE) && (o_ptr->sval == SV_LITE_LANTERN))
	{

		/* Message */
		if (unstack) msg_print("You unstack your lantern.");

		/* Refuel */
		o_ptr->charges += j_ptr->charges;

		/* Message */
		msg_print("You fuel the lamp.");

		/* Comment */
		if (o_ptr->charges >= FUEL_LAMP)
		{
			o_ptr->charges = FUEL_LAMP;
			msg_print("The lamp is full.");
		}

		/* Get feat */
		if (j_ptr->ident & (IDENT_STORE)) use_feat = TRUE;
	}
	else if ((o_ptr->tval == TV_LITE) && (o_ptr->sval == SV_LITE_TORCH))
	{

		/* Message */
		if (unstack) msg_print("You unstack your torch.");

		/* Refuel */
		o_ptr->charges += j_ptr->charges + 5;

		/* Message */
		msg_print("You combine the torches.");

		/* Over-fuel message */
		if (o_ptr->charges >= FUEL_TORCH)
		{
			o_ptr->charges = FUEL_TORCH;
			msg_print("Your torch is fully fueled.");
		}

		/* Refuel message */
		else if (item == INVEN_LITE)
		{
			msg_print("Your torch glows more brightly.");
		}

		/* Get feat */
		if (j_ptr->ident & (IDENT_STORE)) get_feat = TRUE;
	}
	else
	{
		/* Fill empty bottle or flask with the potion */
		object_copy(o_ptr, j_ptr);

		/* Modify quantity */
		o_ptr->number = 1;

		/* Reset stack counter */
		o_ptr->stackc = 0;

		/* Use feat */
		if (o_ptr->ident & (IDENT_STORE)) use_feat = TRUE;

		/* No longer 'stored' */
		o_ptr->ident &= ~(IDENT_STORE);

		/* Adjust the weight */
		p_ptr->total_weight += o_ptr->weight;
	}

	if (unstack)
	{
		/* Carry */
		item = inven_carry(o_ptr);

		/* Don't relite */
		relite = FALSE;
	}

	/* Use fuel from a lantern */
	if ((j_ptr->tval == TV_LITE) && (j_ptr->sval == SV_LITE_LANTERN))
	{
		if (j_ptr->number > 1)
		{
			/* Get local object */
			i_ptr = &object_type_body;

			/* Obtain a local object */
			object_copy(i_ptr, j_ptr);

			/* Modify quantity */
			i_ptr->number = 1;

			/* Reset stack counter */
			i_ptr->stackc = 0;

			/* Reset the charges */
			i_ptr->charges = 0;

			/* No longer 'stored' */
			i_ptr->ident &= ~(IDENT_STORE);

			/* Unstack the used item */
			j_ptr->number--;

			/* Adjust the weight and carry */
			p_ptr->total_weight -= i_ptr->weight;
			item = inven_carry(i_ptr);

			/* Message */
			msg_print("You unstack your lantern.");
		}
		else
		{
			/* Use up last of fuel */
			j_ptr->charges = 0;
		}
	}
	/* Decrease the item (in the pack) */
	else if (item2 >= 0)
	{
		if (j_ptr->number == 1) inven_drop_flags(j_ptr);

		inven_item_increase(item2, -1);
		inven_item_describe(item2);
		inven_item_optimize(item2);
	}
	/* Decrease the item (from the floor) */
	else
	{
		floor_item_increase(0 - item2, -1);
		floor_item_describe(0 - item2);
		floor_item_optimize(0 - item2);
		if (get_feat && (scan_feat(p_ptr->py,p_ptr->px) < 0)) cave_alter_feat(p_ptr->py,p_ptr->px,FS_GET_FEAT);
		if (use_feat && (scan_feat(p_ptr->py,p_ptr->px) < 0)) cave_alter_feat(p_ptr->py,p_ptr->px,FS_USE_FEAT);
	}

	/* Relite if necessary */
	if (relite)
	{
		o_ptr->timeout = o_ptr->charges;
		o_ptr->charges = 0;
	}

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP);

	/* Lite if necessary */
	if ((item == INVEN_LITE) || (item2 == INVEN_LITE)) p_ptr->update |= (PU_TORCH);

}


/*
 * An "item_tester_hook" for light sources
 */
static bool item_tester_light_source(const object_type *o_ptr)
{
	/* Return "is a non-artifact light source" */
	return ((o_ptr->tval == TV_LITE) && !(artifact_p(o_ptr)));
}

/*
 * Light or douse a light source.
 */
void do_cmd_light_and_douse(void)
{
	int item;

	object_type *o_ptr;
	char o_name[80];

	cptr own_str = "";

	cptr q, s;
	
	byte flags = USE_EQUIP | USE_FLOOR;

	/* Check if using a torch */
	if (item_tester_refill_torch(&inventory[INVEN_LITE])) flags |= (USE_FEATH);
	
	/* Check if wielding a known fire brand */
	if (inventory[INVEN_WIELD].can_flags1 & (TR1_BRAND_FIRE)) flags |= (USE_FEATH);
	
	/* Restrict the choices */
	item_tester_hook = item_tester_light_source;

	/* Get an item (not in the backpack) */
	q = "Light or douse which light source?";
	s = "You have no light sources.";
	if (!get_item(&item, q, s, flags)) return;

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}

	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item];
	}

	/* In a bag? */
	if (o_ptr->tval == TV_BAG)
	{
		/* Get item from bag */
		if (!get_item_from_bag(&item, q, s, o_ptr)) return;

		/* Refer to the item */
		o_ptr = &inventory[item];
	}

	/* Get an object description */
	object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 1);

	/* Get an ownership string */
	if (item >= 0) own_str = "your";
	else if (o_ptr->number != 1) own_str = "some";
	else own_str = "the";


	/* Take a partial turn */
	p_ptr->energy_use = 50;


	/* Light the light source */
	if (!(o_ptr->timeout))
	{
		msg_format("You light %s %s.", own_str, o_name);
		o_ptr->timeout = o_ptr->charges;
		o_ptr->charges = 0;

		if (item < 0)
		{
			gain_attribute(p_ptr->py, p_ptr->px, 2, CAVE_XLOS, apply_halo, redraw_halo_gain);			
		}
	}

	/* Douse the light source */
	else
	{
		msg_format("You douse %s %s.", own_str, o_name);
		o_ptr->charges = o_ptr->timeout;
		o_ptr->timeout = 0;
		
		if (item < 0)
		{
			check_attribute_lost(p_ptr->py, p_ptr->px, 2, CAVE_XLOS, require_halo, has_halo, redraw_halo_loss, remove_halo, reapply_halo);			
		}
	}

	/* Update torch */
	p_ptr->update |= (PU_TORCH);

	/* Fully update the visuals */
	p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);
}




/*
 * Target command
 */
void do_cmd_target(void)
{
	/* Target set */
	if (target_set_interactive(TARGET_KILL))
	{
		msg_print("Target Selected.");
	}

	/* Target aborted */
	else
	{
		msg_print("Target Aborted.");
	}
}



/*
 * Look command
 */
void do_cmd_look(void)
{
	/* Look around */
	if (target_set_interactive(TARGET_LOOK))
	{
		msg_print("Target Selected.");
	}
}


/*
 * Allow the player to examine other sectors on the map
 */
void do_cmd_locate(void)
{
	int dir, y1, x1, y2, x2;

	char tmp_val[80];

	char out_val[160];


	/* Start at current panel */
	y2 = y1 = p_ptr->wy;
	x2 = x1 = p_ptr->wx;

	/* Show panels until done */
	while (1)
	{
		/* Describe the location */
		if ((y2 == y1) && (x2 == x1))
		{
			tmp_val[0] = '\0';
		}
		else
		{
			sprintf(tmp_val, "%s%s of",
			((y2 < y1) ? " North" : (y2 > y1) ? " South" : ""),
			((x2 < x1) ? " West" : (x2 > x1) ? " East" : ""));
		}

		/* Prepare to ask which way to look */
		sprintf(out_val,
		"Map sector [%d,%d], which is%s your sector.  Direction?",
		(y2 / PANEL_HGT), (x2 / PANEL_WID), tmp_val);

		/* More detail */
		if (center_player)
		{
			sprintf(out_val,
			"Map sector [%d(%02d),%d(%02d)], which is%s your sector.  Direction?",
			(y2 / PANEL_HGT), (y2 % PANEL_HGT),
			(x2 / PANEL_WID), (x2 % PANEL_WID), tmp_val);
		}

		/* Assume no direction */
		dir = 0;

		/* Get a direction */
		while (!dir)
		{
			char command;

			/* Get a command (or Cancel) */
			if (!get_com(out_val, &command)) break;

			/* Extract direction */
			dir = target_dir(command);

			/* Error */
			if (!dir) bell("Illegal direction for locate!");
		}

		/* No direction */
		if (!dir) break;

		/* Apply the motion */
		y2 += (ddy[dir] * PANEL_HGT);
		x2 += (ddx[dir] * PANEL_WID);

		/* Verify the row */
		if (y2 < 0) y2 = 0;
		if (y2 > DUNGEON_HGT - SCREEN_HGT) y2 = DUNGEON_HGT - SCREEN_HGT;

		/* Verify the col */
		if (x2 < 0) x2 = 0;
		if (x2 > DUNGEON_WID - SCREEN_WID) x2 = DUNGEON_WID - SCREEN_WID;

		/* Handle "changes" */
		if ((p_ptr->wy != y2) || (p_ptr->wx != x2))
		{
			/* Update panel */
			p_ptr->wy = y2;
			p_ptr->wx = x2;

			/* Redraw map */
			p_ptr->redraw |= (PR_MAP);

			/* Window stuff */
			p_ptr->window |= (PW_OVERHEAD);

			/* Handle stuff */
			handle_stuff();
		}
	}

	/* Verify panel */
	p_ptr->update |= (PU_PANEL);

	/* Handle stuff */
	handle_stuff();
}






/*
 * The table of "symbol info" -- each entry is a string of the form
 * "X:desc" where "X" is the trigger, and "desc" is the "info".
 */
static cptr ident_info[] =
{
	" :A dark grid",
	"!:A potion",
	"\":An amulet (or necklace)",
	"#:A wall, secret door or impassable terrain",
	"$:Treasure (gold or gems)",
	"%:A mineral vein, crack or vent",
	"&:A chest",
	"':An open door or cloud",
	"(:Soft armor",
	"):A shield",
	"*:A vein with treasure or interesting wall",
	"+:A closed/trapped door",
	",:Food (or mushroom patch)",
	"-:A wand (or rod)",
	".:Floor (or shallow water/oil)",
	"/:A polearm (Axe/Pike/etc)",
	"0:Fountain/well/altar/throne",
	"1:Entrance to General Store",
	"2:Entrance to Armory",
	"3:Entrance to Weaponsmith",
	"4:Entrance to Temple",
	"5:Entrance to Alchemy shop",
	"6:Entrance to Magic store",
	"7:Entrance to Black Market",
	"8:Entrance to your home",
	"9:Statue",
	"::Trees, rubble or deep water/lava/acid etc",
	";:Plants, broken floor or shallow lava/acid/mud etc",
	"<:An up staircase or falls",
	"=:A ring",
	">:A down staircase or chasm",
	"?:A scroll",
	"@:You",
	"A:Ancient Dragon/Wyrm",
	"B:Bird",
	"C:Canine",
	"D:mature Dragons",
	"E:Elemental",
	"F:Fish/Amphibian",
	"G:Ghost",
	"H:Hybrid",
	"I:Insect",
	"J:Snake",
	"K:Killer Beetle",
	"L:ghouls/Liches",
	"M:Maiar",
	"N:Nightsbane",
	"O:Ogre",
	"P:Giant",
	"Q:Quadrepeds",
	"R:Reptile",
	"S:Spider/Scorpion/Tick",
	"T:Troll",
	"U:Major Demon",
	"V:Vampire",
	"W:Wight/Wraith",
	"X:Xorn/Xaren/etc",
	"Y:apes/Yeti",
	"Z:Zephyr Hound",
	"[:Hard armor",
	"\\:A hafted weapon (mace/whip/etc)",
	"]:Misc. armor",
	"^:A trap",
	"_:A staff",
	/* "`:unused", */
	"a:Ant",
	"b:Bat",
	"c:Centipede",
	"d:Young Dragon",
	"e:Floating Eye",
	"f:Feline",
	"g:Golem",
	"h:Hobbit/Dwarf",
	"i:Icky Thing",
	"j:Jelly",
	"k:Goblin",
	"l:Elf",
	"m:Mold",
	"n:Naga",
	"o:Orc",
	"p:Priest",
	"q:Mage",
	"r:Rodent",
	"s:Skeleton",
	"t:Thief/Warrior",
	"u:Minor Demon",
	"v:Vortex",
	"w:Worm/Worm-Mass",
	"x:A bridge or unknown grid",
	"y:Hydra",
	"z:Zombie/Mummy",
	"{:A missile (arrow/bolt/shot)",
	"|:An edged weapon (sword/dagger/etc)",
	"}:A missle launcher (bow/crossbow/sling)",
	"~:A tool or miscellaneous item",
	NULL
};



/*
 * Sorting hook -- Comp function -- see below
 *
 * We use "u" to point to array of monster indexes,
 * and "v" to select the type of sorting to perform on "u".
 */
bool ang_sort_comp_hook(vptr u, vptr v, int a, int b)
{
	u16b *who = (u16b*)(u);

	u16b *why = (u16b*)(v);

	int w1 = who[a];
	int w2 = who[b];

	int z1, z2;


	/* Sort by player kills */
	if (*why >= 4)
	{
		/* Extract player kills */
		z1 = l_list[w1].pkills;
		z2 = l_list[w2].pkills;

		/* Compare player kills */
		if (z1 < z2) return (TRUE);
		if (z1 > z2) return (FALSE);
	}


	/* Sort by total kills */
	if (*why >= 3)
	{
		/* Extract total kills */
		z1 = l_list[w1].tkills;
		z2 = l_list[w2].tkills;

		/* Compare total kills */
		if (z1 < z2) return (TRUE);
		if (z1 > z2) return (FALSE);
	}

	/* Sort by monster level */
	if (*why >= 2)
	{
		/* Extract levels */
		z1 = r_info[w1].level;
		z2 = r_info[w2].level;

		/* Compare levels */
		if (z1 < z2) return (TRUE);
		if (z1 > z2) return (FALSE);
	}

	/* Sort by monster experience */
	if (*why >= 1)
	{
		/* Extract experience */
		z1 = r_info[w1].mexp;
		z2 = r_info[w2].mexp;

		/* Compare experience */
		if (z1 < z2) return (TRUE);
		if (z1 > z2) return (FALSE);
	}

	/* Compare indexes */
	return (w1 <= w2);
}


/*
 * Sorting hook -- Swap function -- see below
 *
 * We use "u" to point to array of monster indexes,
 * and "v" to select the type of sorting to perform.
 */
void ang_sort_swap_hook(vptr u, vptr v, int a, int b)
{
	u16b *who = (u16b*)(u);

	u16b holder;

	(void)v;

	/* Swap */
	holder = who[a];
	who[a] = who[b];
	who[b] = holder;
}



/*
 * Hack -- Display the "name" and "attr/chars" of a monster race
 */
static void roff_top(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	byte a1, a2;
	char c1, c2;


	/* Get the chars */
	c1 = r_ptr->d_char;
	c2 = r_ptr->x_char;

	/* Get the attrs */
	a1 = r_ptr->d_attr;
	a2 = r_ptr->x_attr;


	/* Clear the top line */
	Term_erase(0, 0, 255);

	/* Reset the cursor */
	Term_gotoxy(0, 0);

	/* A title (use "The" for non-uniques) */
	if (!(r_ptr->flags1 & (RF1_UNIQUE)))
	{
		Term_addstr(-1, TERM_WHITE, "The ");
	}

	/* Dump the name */
	Term_addstr(-1, TERM_WHITE, (r_name + r_ptr->name));

	if (!use_dbltile && !use_trptile)
	{
		/* Append the "standard" attr/char info */
		Term_addstr(-1, TERM_WHITE, " ('");
		Term_addch(a1, c1);
		Term_addstr(-1, TERM_WHITE, "')");

		/* Append the "optional" attr/char info */
		Term_addstr(-1, TERM_WHITE, "/('");
		Term_addch(a2, c2);
		if (use_bigtile && (a2 & 0x80)) Term_addch(255, -1);
		Term_addstr(-1, TERM_WHITE, "'):");
	}
}


/*
 * Identify a character, allow recall of monsters
 *
 * Several "special" responses recall "mulitple" monsters:
 *   ^A (all monsters)
 *   ^U (all unique monsters)
 *   ^N (all non-unique monsters)
 *
 * The responses may be sorted in several ways, see below.
 *
 * Note that the player ghosts are ignored, since they do not exist.
 */
void do_cmd_query_symbol(void)
{
	int i, n, r_idx;
	char sym, query;
	char buf[128];

	bool all = FALSE;
	bool uniq = FALSE;
	bool norm = FALSE;

	bool recall = FALSE;

	u16b why = 0;
	u16b *who;


	/* Get a character, or abort */
	if (!get_com("Enter character to be identified: ", &sym)) return;

	/* Find that character info, and describe it */
	for (i = 0; ident_info[i]; ++i)
	{
		if (sym == ident_info[i][0]) break;
	}

	/* Describe */
	if (sym == KTRL('A'))
	{
		all = TRUE;
		strcpy(buf, "Full monster list.");
	}
	else if (sym == KTRL('U'))
	{
		all = uniq = TRUE;
		strcpy(buf, "Unique monster list.");
	}
	else if (sym == KTRL('N'))
	{
		all = norm = TRUE;
		strcpy(buf, "Non-unique monster list.");
	}
	else if (ident_info[i])
	{
		sprintf(buf, "%c - %s.", sym, ident_info[i] + 2);
	}
	else
	{
		sprintf(buf, "%c - %s.", sym, "Unknown Symbol");
	}

	/* Display the result */
	prt(buf, 0, 0);


	/* Allocate the "who" array */
	who = C_ZNEW(z_info->r_max, u16b);

	/* Collect matching monsters */
	for (n = 0, i = 1; i < z_info->r_max - 1; i++)
	{
		monster_race *r_ptr = &r_info[i];
		monster_lore *l_ptr = &l_list[i];

		/* Nothing to recall */
		if (!cheat_know && !l_ptr->sights) continue;

		/* Require non-unique monsters if needed */
		if (norm && (r_ptr->flags1 & (RF1_UNIQUE))) continue;

		/* Require unique monsters if needed */
		if (uniq && !(r_ptr->flags1 & (RF1_UNIQUE))) continue;

		/* Collect "appropriate" monsters */
		if (all || (r_ptr->d_char == sym)) who[n++] = i;
	}

	/* Nothing to recall */
	if (!n)
	{
		/* XXX XXX Free the "who" array */
		FREE(who);

		return;
	}


	/* Prompt */
	put_str("Recall details? (k/p/y/n): ", 0, 40);

	/* Query */
	query = anykey().key;

	/* Restore */
	prt(buf, 0, 0);


	/* Sort by kills (and level) */
	if (query == 'k')
	{
		why = 4;
		query = 'y';
	}

	/* Sort by level */
	if (query == 'p')
	{
		why = 2;
		query = 'y';
	}

	/* Catch "escape" */
	if (query != 'y')
	{
		/* XXX XXX Free the "who" array */
		FREE(who);

		return;
	}

	/* Sort if needed */
	if (why)
	{
		/* Select the sort method */
		ang_sort_comp = ang_sort_comp_hook;
		ang_sort_swap = ang_sort_swap_hook;

		/* Sort the array */
		ang_sort(who, &why, n);
	}


	/* Start at the end */
	i = n - 1;

	/* Scan the monster memory */
	while (1)
	{
		/* Extract a race */
		r_idx = who[i];

		/* Hack -- Auto-recall */
		monster_race_track(r_idx);

		/* Hack -- Handle stuff */
		handle_stuff();

		/* Hack -- Begin the prompt */
		roff_top(r_idx);

		/* Hack -- Complete the prompt */
		Term_addstr(-1, TERM_WHITE, " [(r)ecall, ESC]");

		/* Interact */
		while (1)
		{
			/* Recall */
			if (recall)
			{
				/* Save screen */
				screen_save();

				/* Recall on screen */
				screen_roff(&r_info[who[i]], &l_list[who[i]]);

				/* Hack -- Complete the prompt (again) */
				Term_addstr(-1, TERM_WHITE, " [(r)ecall, ESC]");
			}

			/* Command */
			query = anykey().key;

			/* Unrecall */
			if (recall)
			{
				/* Load screen */
				screen_load();
			}

			/* Normal commands */
			if (query != 'r') break;

			/* Toggle recall */
			recall = !recall;
		}

		/* Stop scanning */
		if (query == ESCAPE) break;

		/* Move to "prev" monster */
		if (query == '-')
		{
			if (++i == n)
			{
				i = 0;
				if (!expand_list) break;
			}
		}

		/* Move to "next" monster */
		else
		{
			if (i-- == 0)
			{
				i = n - 1;
				if (!expand_list) break;
			}
		}
	}


	/* Re-display the identity */
	prt(buf, 0, 0);

	/* Free the "who" array */
	FREE(who);
}
