/* File: melee1.c */

/* Purpose: Monster attacks */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"


/*
 * Attack the player via physical attacks.
 */
bool make_attack_normal(int m_idx, byte divis)
{
	int attacked = 0;
	call_lua("monster_melee_attack", "(d)", "d", m_idx, &attacked);

	/* Above function returned 0; monster couldn't or didn't attack. */
	if (attacked == 0) return (FALSE);

	/* Assume we attacked */
	return (TRUE);
}

/* Check if the player has a shield. */
bool shield_has()
{
        object_type *o_ptr;

        o_ptr = &inventory[INVEN_WIELD];
        if (o_ptr->tval != TV_SHIELD) 
	{
		o_ptr = &inventory[INVEN_WIELD+1];
		if (o_ptr->tval == TV_SHIELD) return (TRUE);
	}
	else return (TRUE);

        return (FALSE);
}

/* Check if the player has a sword. */
bool sword_has()
{
        object_type *o_ptr;

        o_ptr = &inventory[INVEN_WIELD];
        if (!(o_ptr->tval == TV_WEAPON && o_ptr->itemskill == 12)) 
	{
		o_ptr = &inventory[INVEN_WIELD+1];
		if (o_ptr->tval == TV_WEAPON && o_ptr->itemskill == 12) return (TRUE);
	}
	else return (TRUE);

        return (FALSE);
}

/* Check if the player has a hafted weapon. */
bool hafted_has()
{
        object_type *o_ptr;

        o_ptr = &inventory[INVEN_WIELD];
        if (!(o_ptr->tval == TV_WEAPON && o_ptr->itemskill == 13)) 
	{
		o_ptr = &inventory[INVEN_WIELD+1];
		if (o_ptr->tval == TV_WEAPON && o_ptr->itemskill == 13) return (TRUE);
	}
	else return (TRUE);

        return (FALSE);
}

/* Check if the player has a polearm. */
bool polearm_has()
{
        object_type *o_ptr;

        o_ptr = &inventory[INVEN_WIELD];
        if (!(o_ptr->tval == TV_WEAPON && o_ptr->itemskill == 14)) 
	{
		o_ptr = &inventory[INVEN_WIELD+1];
		if (o_ptr->tval == TV_WEAPON && o_ptr->itemskill == 14) return (TRUE);
	}
	else return (TRUE);

        return (FALSE);
}

/* Check if the player has a rod. */
bool rod_has()
{
        object_type *o_ptr;

        o_ptr = &inventory[INVEN_WIELD];
        if (o_ptr->tval != TV_ROD) 
	{
		o_ptr = &inventory[INVEN_WIELD+1];
		if (o_ptr->tval == TV_ROD) return (TRUE);
	}
	else return (TRUE);

        return (FALSE);
}

/* Check if the player has a weapon equipped. */
bool unarmed()
{
        object_type *o_ptr;
        o_ptr = &inventory[INVEN_WIELD];

        if (o_ptr->tval == 0)
	{
		o_ptr = &inventory[INVEN_WIELD+1];
		if (o_ptr->tval == 0) return (TRUE);
	}

        return (FALSE);
}

/* Check if the player has a heavy armor. */
bool heavy_armor()
{
        object_type *o_ptr;
        o_ptr = &inventory[INVEN_BODY];

        if (o_ptr->tval != TV_HARD_ARMOR && o_ptr->tval != TV_DRAG_ARMOR) return (FALSE);
        else return (TRUE);
}