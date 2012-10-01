/* File: cmd5.c */

/* Purpose: Realmed Class commands */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */


#include "angband.h"

extern bool item_tester_hook_armour(object_type *o_ptr);

/* Maximum number of tries for teleporting */
#define MAX_TRIES 300

/*
 * Hook to determine if an object is drainable
 */
static bool item_tester_hook_scroll_amulet(object_type *o_ptr)
{
        if ((o_ptr->tval == TV_AMULET) || (o_ptr->tval == TV_SCROLL)) return (TRUE);

	/* Assume not */
	return (FALSE);
}

/* Return TRUE if the object is a weapon */
static bool item_tester_hook_weapon(object_type *o_ptr)
{
        if ((o_ptr->tval == TV_WEAPON) || (o_ptr->tval == TV_ROD)) return (TRUE);

	/* Assume not */
	return (FALSE);
}

/* Check if the weapon is a polearm */
static bool item_tester_hook_weapon_polearm(object_type *o_ptr)
{
        if (o_ptr->itemskill == 14) return (TRUE);

	/* Assume not */
	return (FALSE);
}

/* Check if the weapon is a dagger */
static bool item_tester_hook_weapon_dagger(object_type *o_ptr)
{
        if (o_ptr->itemskill == 15) return (TRUE);

	/* Assume not */
	return (FALSE);
}


/* Check if an ammo */
static bool item_tester_hook_ammo(object_type *o_ptr)
{
        if (o_ptr->tval == TV_AMMO) return (TRUE);

	/* Assume not */
	return (FALSE);
}

static bool effect_books(object_type *o_ptr)
{
        if (o_ptr->tval == TV_BOOK_ELEMENTAL || o_ptr->tval == TV_BOOK_ALTERATION || o_ptr->tval == TV_BOOK_MYSTICISM || o_ptr->tval == TV_BOOK_CONJURATION || o_ptr->tval == TV_BOOK_DIVINATION) return (TRUE);

	/* Assume not */
	return (FALSE);
}

static bool item_tester_hook_hard_drag_armor(object_type *o_ptr)
{
        if ((o_ptr->tval == TV_HARD_ARMOR) || (o_ptr->tval == TV_DRAG_ARMOR)) return (TRUE);

	/* Assume not */
	return (FALSE);
}

static bool item_tester_hook_helmet(object_type *o_ptr)
{
        if (o_ptr->tval == TV_HELM) return (TRUE);

	/* Assume not */
	return (FALSE);
}

static bool item_tester_hook_gauntlet(object_type *o_ptr)
{
        if (o_ptr->tval == TV_GLOVES) return (TRUE);

	/* Assume not */
	return (FALSE);
}

static bool item_tester_hook_boots(object_type *o_ptr)
{
        if (o_ptr->tval == TV_BOOTS) return (TRUE);

	/* Assume not */
	return (FALSE);
}

static bool item_tester_hook_soft_hard_armor(object_type *o_ptr)
{
        if ((o_ptr->tval == TV_HARD_ARMOR) || (o_ptr->tval == TV_SOFT_ARMOR)) return (TRUE);

	/* Assume not */
	return (FALSE);
}

static bool item_tester_hook_poisonable(object_type *o_ptr)
{
        if ((o_ptr->tval == TV_WEAPON) || (o_ptr->tval == TV_ROD) || (o_ptr->tval == TV_AMMO) || (o_ptr->tval == TV_GLOVES) || (o_ptr->tval == TV_THROWING)) return (TRUE);

	/* Assume not */
	return (FALSE);
}

/* Check if an arrow */
static bool item_tester_hook_arrow(object_type *o_ptr)
{
        if (o_ptr->tval == TV_AMMO && o_ptr->itemtype == 1) return (TRUE);

	/* Assume not */
	return (FALSE);
}

/* Check if a bolt */
static bool item_tester_hook_bolt(object_type *o_ptr)
{
        if (o_ptr->tval == TV_AMMO && o_ptr->itemtype == 2) return (TRUE);

	/* Assume not */
	return (FALSE);
}

/* Check if a bullet */
static bool item_tester_hook_bullet(object_type *o_ptr)
{
        if (o_ptr->tval == TV_AMMO && (o_ptr->itemtype == 3 || o_ptr->itemtype == 4)) return (TRUE);

	/* Assume not */
	return (FALSE);
}

/* Levelable items */
/* Everything except the following items. */
static bool item_tester_hook_levelable(object_type *o_ptr)
{
        if (!(wield_slot(o_ptr) == -1)) return (TRUE);

	/* Assume not */
	return (FALSE);
}

/*
 * Study a book to gain a new effects!
 */
void do_cmd_study(void)
{
        int item;
        object_type             *o_ptr;
        cptr q, s;

        /* Restrict choices to weapons */
        item_tester_hook = effect_books;

        /* Get an item */
        q = "Study which book? ";
        s = "You have no books!";
        if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP))) return;

        o_ptr = &inventory[item];

        if (o_ptr->tval == TV_BOOK_ELEMENTAL)
        {
                switch(o_ptr->sval)
                {
                        case 0:
                                msg_print("You learned the effect of Missile!");
                                p_ptr->elemental_effects |= ELEM_MISSILE;
                                break;
                        case 1:
                                msg_print("You learned the effect of Fire!");
                                p_ptr->elemental_effects |= ELEM_FIRE;
                                break;
                        case 2:
                                msg_print("You learned the effect of Cold!");
                                p_ptr->elemental_effects |= ELEM_COLD;
                                break;
                        case 3:
                                msg_print("You learned the effect of Electricity!");
                                p_ptr->elemental_effects |= ELEM_ELEC;
                                break;
                        case 4:
                                msg_print("You learned the effect of Acid!");
                                p_ptr->elemental_effects |= ELEM_ACID;
                                break;
                        case 5:
                                msg_print("You learned the effect of Poison!");
                                p_ptr->elemental_effects |= ELEM_POIS;
                                break;
                        case 6:
                                msg_print("You learned the effect of Light!");
                                p_ptr->elemental_effects |= ELEM_LITE;
                                break;
                        case 7:
                                msg_print("You learned the effect of Darkness!");
                                p_ptr->elemental_effects |= ELEM_DARK;
                                break;
                        case 8:
                                msg_print("You learned the effect of Physical!");
                                p_ptr->elemental_effects |= ELEM_PHYSICAL;
                                break;
                        case 12:
                                msg_print("You learned the effect of Radio!");
                                p_ptr->elemental_effects |= ELEM_RADIO;
                                break;
                        case 13:
                                msg_print("You learned the effect of Water!");
                                p_ptr->elemental_effects |= ELEM_WATER;
                                break;
                        case 14:
                                msg_print("You learned the effect of Chaos!");
                                p_ptr->elemental_effects |= ELEM_CHAOS;
                                break;
                        case 17:
                                msg_print("You learned the effect of Earth!");
                                p_ptr->elemental_effects |= ELEM_EARTH;
                                break;
                        case 18:
                                msg_print("You learned the effect of Sound!");
                                p_ptr->elemental_effects |= ELEM_SOUND;
                                break;
                        case 19:
                                msg_print("You learned the effect of Warp!");
                                p_ptr->elemental_effects |= ELEM_WARP;
                                break;
                        case 20:
                                msg_print("You learned the effect of Mana!");
                                p_ptr->elemental_effects |= ELEM_MANA;
                                break;
                        case 23:
                                msg_print("You learned the effect of Wind!");
                                p_ptr->elemental_effects |= ELEM_WIND;
                                break;

                }
        }
        if (o_ptr->tval == TV_BOOK_ALTERATION)
        {
                switch(o_ptr->sval)
                {
                        case 0:
                                msg_print("You learned the effect of Reduce Hit Rate!");
                                p_ptr->alteration_effects |= ALTER_REDUCE_HIT;
                                break;
                        case 1:
                                msg_print("You learned the effect of Reduce Defense!");
                                p_ptr->alteration_effects |= ALTER_REDUCE_DEF;
                                break;
                        case 2:
                                msg_print("You learned the effect of Reduce Speed!");
                                p_ptr->alteration_effects |= ALTER_REDUCE_SPEED;
                                break;
                        case 3:
                                msg_print("You learned the effect of Reduce Level!");
                                p_ptr->alteration_effects |= ALTER_REDUCE_LEVEL;
                                break;
                        case 4:
                                msg_print("You learned the effect of Life Blast!");
                                p_ptr->alteration_effects |= ALTER_LIFE_BLAST;
                                break;
                        case 5:
                                msg_print("You learned the effect of Lock!");
                                p_ptr->alteration_effects |= ALTER_LOCK;
                                break;
                        case 6:
                                msg_print("You learned the effect of Halve Power!");
                                p_ptr->alteration_effects |= ALTER_HALVE_POWER;
                                break;
                        case 7:
                                msg_print("You learned the effect of Halve Magic!");
                                p_ptr->alteration_effects |= ALTER_HALVE_MAGIC;
                                break;
                        case 8:
                                msg_print("You learned the effect of Stone to Mud!");
                                p_ptr->alteration_effects |= ALTER_STONE_TO_MUD;
                                break;
                        case 9:
                                msg_print("You learned the effect of Demoralize!");
                                p_ptr->alteration_effects |= ALTER_DEMORALIZE;
                                break;
                        case 10:
                                msg_print("You learned the effect of Retrograde!");
                                p_ptr->alteration_effects |= ALTER_RETROGRADE;
                                break;
                        case 11:
                                msg_print("You learned the effect of Evolve!");
                                p_ptr->alteration_effects |= ALTER_EVOLVE;
                                break;
                        case 12:
                                msg_print("You learned the effect of Un-Evolve!");
                                p_ptr->alteration_effects |= ALTER_UNEVOLVE;
                                break;
                        case 13:
                                msg_print("You learned the effect of Haste!");
                                p_ptr->alteration_effects |= ALTER_HASTE;
                                break;
                        case 14:
                                msg_print("You learned the effect of Raise Strength!");
                                p_ptr->alteration_effects |= ALTER_RAISE_STR;
                                break;
                        case 15:
                                msg_print("You learned the effect of Raise Intelligence!");
                                p_ptr->alteration_effects |= ALTER_RAISE_INT;
                                break;
                        case 16:
                                msg_print("You learned the effect of Raise Wisdom!");
                                p_ptr->alteration_effects |= ALTER_RAISE_WIS;
                                break;
                        case 17:
                                msg_print("You learned the effect of Raise Dexterity!");
                                p_ptr->alteration_effects |= ALTER_RAISE_DEX;
                                break;
                        case 18:
                                msg_print("You learned the effect of Raise Constitution!");
                                p_ptr->alteration_effects |= ALTER_RAISE_CON;
                                break;
                        case 19:
                                msg_print("You learned the effect of Raise Charisma!");
                                p_ptr->alteration_effects |= ALTER_RAISE_CHR;
                                break;
                        case 20:
                                msg_print("You learned the effect of Alter Position!");
                                p_ptr->alteration_effects |= ALTER_POSITION;
                                break;
                        case 21:
                                msg_print("You learned the effect of Haste Other!");
                                p_ptr->alteration_effects |= ALTER_HASTE_OTHER;
                                break;
                        case 22:
                                msg_print("You learned the effect of Physical Resistance!");
                                p_ptr->alteration_effects |= ALTER_PHYS_RESIST;
                                break;
                        case 23:
                                msg_print("You learned the effect of Magic Resistance!");
                                p_ptr->alteration_effects |= ALTER_MAGIC_RESIST;
                                break;
                        case 24:
                                msg_print("You learned the effect of Stoneskin!");
                                p_ptr->alteration_effects |= ALTER_STONESKIN;
                                break;
                        case 25:
                                msg_print("You learned the effect of Paralyze!");
                                p_ptr->alteration_effects |= ALTER_PARALYZE;
                                break;

                }
        }
        if (o_ptr->tval == TV_BOOK_MYSTICISM)
        {
                switch(o_ptr->sval)
                {
                        case 0:
                                msg_print("You learned the effect of Heal!");
                                p_ptr->healing_effects |= MYST_HEAL;
                                break;
                        case 1:
                                msg_print("You learned the effect of Restore Stats!");
                                p_ptr->healing_effects |= MYST_RESTORE_STATS;
                                break;
                        case 2:
                                msg_print("You learned the effect of Restore Status!");
                                p_ptr->healing_effects |= MYST_RESTORE_STATUS;
                                break;
                        case 3:
                                msg_print("You learned the effect of Harm!");
                                p_ptr->healing_effects |= MYST_HARM;
                                break;
			case 4:
                                msg_print("You learned the effect of War Blessing!");
                                p_ptr->healing_effects |= MYST_WAR_BLESSING;
                                break;
                        case 5:
                                msg_print("You learned the effect of Drain Life!");
                                p_ptr->healing_effects |= MYST_DRAIN_LIFE;
                                break;
                        case 6:
                                msg_print("You learned the effect of Revive Monster!");
                                p_ptr->healing_effects |= MYST_REVIVE_MONSTER;
                                break;
                        case 7:
                                msg_print("You learned the effect of Restore Mana!");
                                p_ptr->healing_effects |= MYST_RESTORE_MANA;
                                break;
			case 8:
                                msg_print("You learned the effect of Blessing!");
                                p_ptr->healing_effects |= MYST_BLESSING;
                                break;
			case 9:
                                msg_print("You learned the effect of Smite Undeads!");
                                p_ptr->healing_effects |= MYST_SMITE_UNDEADS;
                                break;
			case 10:
                                msg_print("You learned the effect of Smite Demons!");
                                p_ptr->healing_effects |= MYST_SMITE_DEMONS;
                                break;
			case 11:
                                msg_print("You learned the effect of Smite Evil!");
                                p_ptr->healing_effects |= MYST_SMITE_EVIL;
                                break;
			case 12:
                                msg_print("You learned the effect of Smite Good!");
                                p_ptr->healing_effects |= MYST_SMITE_GOOD;
                                break;

                }
        }
        if (o_ptr->tval == TV_BOOK_CONJURATION)
        {
                switch(o_ptr->sval)
                {
                        case 0:
                                msg_print("You learned the effect of Summon Kind!");
                                p_ptr->conjuration_effects |= CONJ_SUMMON_KIND;
                                break;
                        case 1:
                                msg_print("You learned the effect of Summon Specific!");
                                p_ptr->conjuration_effects |= CONJ_SUMMON_SPECIFIC;
                                break;
                        case 2:
                                msg_print("You learned the effect of Fire Fields!");
                                p_ptr->conjuration_effects |= CONJ_FIRE_FIELD;
                                break;
                        case 3:
                                msg_print("You learned the effect of Cold Fields!");
                                p_ptr->conjuration_effects |= CONJ_COLD_FIELD;
                                break;
                        case 4:
                                msg_print("You learned the effect of Electric Fields!");
                                p_ptr->conjuration_effects |= CONJ_ELEC_FIELD;
                                break;
                        case 5:
                                msg_print("You learned the effect of Webs!");
                                p_ptr->conjuration_effects |= CONJ_WEBS;
                                break;
                        case 6:
                                msg_print("You learned the effect of Grow Trees!");
                                p_ptr->conjuration_effects |= CONJ_GROW_TREES;
                                break;
                        case 7:
                                msg_print("You learned the effect of Thorned Vines!");
                                p_ptr->conjuration_effects |= CONJ_THORNS;
                                break;
                        case 8:
                                msg_print("You learned the effect of Storms!");
                                p_ptr->conjuration_effects |= CONJ_STORMS;
                                break;
                        case 9:
                                msg_print("You learned the effect of Conjure Item!");
                                p_ptr->conjuration_effects |= CONJ_ITEM;
                                break;
                        case 10:
                                msg_print("You learned the effect of Conjure Magic Item!");
                                p_ptr->conjuration_effects |= CONJ_MAGIC_ITEM;
                                break;
                        case 11:
                                msg_print("You learned the effect of Conjure Special Item!");
                                p_ptr->conjuration_effects |= CONJ_SPECIAL_ITEM;
                                break;

                }
        }
        if (o_ptr->tval == TV_BOOK_DIVINATION)
        {
                switch(o_ptr->sval)
                {
                        case 0:
                                msg_print("You learned the effect of Detect Monsters!");
                                p_ptr->divination_effects |= DIVI_DETECT_MONSTERS;
                                break;
                        case 1:
                                msg_print("You learned the effect of Detect Objects!");
                                p_ptr->divination_effects |= DIVI_DETECT_OBJECTS;
                                break;
                        case 2:
                                msg_print("You learned the effect of Detect Doors!");
                                p_ptr->divination_effects |= DIVI_DETECT_DOORS;
                                break;
                        case 3:
                                msg_print("You learned the effect of Detect Stairs!");
                                p_ptr->divination_effects |= DIVI_DETECT_STAIRS;
                                break;
                        case 4:
                                msg_print("You learned the effect of Detect Chests!");
                                p_ptr->divination_effects |= DIVI_DETECT_CHESTS;
                                break;
                        case 5:
                                msg_print("You learned the effect of Telepathy!");
                                p_ptr->divination_effects |= DIVI_TELEPATHY;
                                break;
                        case 6:
                                msg_print("You learned the effect of Identify!");
                                p_ptr->divination_effects |= DIVI_IDENTIFY;
                                break;
                        case 7:
                                msg_print("You learned the effect of Scan Monster!");
                                p_ptr->divination_effects |= DIVI_SCAN_MONSTER;
                                break;
                        case 8:
                                msg_print("You learned the effect of Reveal!");
                                p_ptr->divination_effects |= DIVI_REVEAL;
                                break;
			case 9:
                                msg_print("You learned the effect of Divination!");
                                p_ptr->divination_effects |= DIVI_DIVINATION;
                                break;
			case 10:
                                msg_print("You learned the effect of Restore Fate!");
                                p_ptr->divination_effects |= DIVI_RESTORE_FATE;
                                break;
			case 11:
                                msg_print("You learned the effect of Twist Fate: Monsters!");
                                p_ptr->divination_effects |= DIVI_FATE_MONSTERS;
                                break;
			case 12:
                                msg_print("You learned the effect of Twist Fate: Items!");
                                p_ptr->divination_effects |= DIVI_FATE_ITEMS;
                                break;
			case 13:
                                msg_print("You learned the effect of Twist Fate: Random Dungeons!");
                                p_ptr->divination_effects |= DIVI_FATE_DUNGEONS;
                                break;
                        
                }
        }

        inven_item_increase(item, -1);
        inven_item_describe(item);
        inven_item_optimize(item);
}

/* Such an usefull hack :) */
static bool check_ring(int art_type)
{
        int i;

        i = INVEN_RING;
        /* Scan the list of rings until we reach the end */
        while (p_ptr->body_parts[i - INVEN_WIELD] == INVEN_RING)
        {
                /* Found the ring we were looking for ? GREAT ! */
                if (inventory[i].k_idx && (inventory[i].name1 == art_type)) return TRUE;

                /* Next item */
                i++;
        }

        /* Found nothing ? blah return FALSE then */
        return FALSE;
}

/* Use the powers of your body monster! :) */
/* As of 1.8.0, this is now very, very, very fun! :) */
int use_body_power(int r_idx, bool only_number)
{
	int                     Power = -1;
        int                     num = 0, dir = 0 , i, j;

	int             powers[36];
	char            power_desc[36][80];
	char 		powdesc[120];

	bool            flag, redraw;
        int             ask, plev = p_ptr->lev;

	char            choice;

	char            out_val[160];
        monster_race    *r_ptr = &r_info[r_idx];
	object_type *q_ptr;

        int x=px,y=py,k,count;
	int rad;
	s32b dam;
	int realpower = 0;

	int spellstat;

	if (p_ptr->stat_ind[A_INT] >= p_ptr->stat_ind[A_WIS]) spellstat = A_INT;
	else spellstat = A_WIS;

        /* List the powers */
	i = 0;
	while (i < 20 && r_ptr->spell[i].type > 0) 
	{
		if (r_ptr->spell[i].type == 1)
		{
			if (r_ptr->spell[i].special3 == 1) sprintf(powdesc, "%s  Type: Bolt, %s  Pow: %d  Cost: %d", r_ptr->spell[i].name, get_element_name(r_ptr->spell[i].special1), r_ptr->spell[i].power, r_ptr->spell[i].cost);
			else sprintf(powdesc, "%s  Type: Bolt, %s  Pow: %d/(stat)-5  Cost: %d", r_ptr->spell[i].name, get_element_name(r_ptr->spell[i].special1), r_ptr->spell[i].power, r_ptr->spell[i].cost);
			strcpy(power_desc[num],powdesc);
			powers[num++]=i;
		}
		else if (r_ptr->spell[i].type == 2)
		{
			if (r_ptr->spell[i].special3 == 1) sprintf(powdesc, "%s  Type: Ball, %s  Pow: %d  Rad: %d  Cost: %d", r_ptr->spell[i].name, get_element_name(r_ptr->spell[i].special1), r_ptr->spell[i].power, r_ptr->spell[i].special2, r_ptr->spell[i].cost);
			else sprintf(powdesc, "%s  Type: Ball, %s  Pow: %d/(stat)-5  Rad: %d  Cost: %d", r_ptr->spell[i].name, get_element_name(r_ptr->spell[i].special1), r_ptr->spell[i].power, r_ptr->spell[i].special2, r_ptr->spell[i].cost);
			strcpy(power_desc[num],powdesc);
			powers[num++]=i;
		}
		else if (r_ptr->spell[i].type == 3)
		{
			sprintf(powdesc, "%s  Type: Healing  Pow: %d/(stat)-5  Cost: %d", r_ptr->spell[i].name, r_ptr->spell[i].power, r_ptr->spell[i].cost);
			strcpy(power_desc[num],powdesc);
			powers[num++]=i;
		}
		else if (r_ptr->spell[i].type == 4)
		{
			sprintf(powdesc, "%s  Type: Haste  Pow: %d  Cost: %d", r_ptr->spell[i].name, r_ptr->spell[i].power, r_ptr->spell[i].cost);
			strcpy(power_desc[num],powdesc);
			powers[num++]=i;
		}
		else if (r_ptr->spell[i].type == 5)
		{
			switch (r_ptr->spell[i].special1)
			{
				case 1:
				case 2:
				case 3:
				case 6:
				{
					sprintf(powdesc, "%s  Type: Haste  Pow: %d  Cost: %d", r_ptr->spell[i].name, r_ptr->spell[i].power, r_ptr->spell[i].cost);
					strcpy(power_desc[num],powdesc);
					powers[num++]=i;
					break;
				}
				
				default:
				{
					break; 
				}
			}
		}
		else if (r_ptr->spell[i].type == 6)
		{
			sprintf(powdesc, "%s   Type: Summon  Pow: %d  Num: %d   Dur: %d   Cost: %d", r_ptr->spell[i].name, r_ptr->spell[i].power, r_ptr->spell[i].special1, r_ptr->spell[i].special2, r_ptr->spell[i].cost);
			strcpy(power_desc[num],powdesc);
			powers[num++]=i;
		}
		else if (r_ptr->spell[i].type == 7)
		{
			monster_race *rr_ptr = &r_info[r_ptr->spell[i].power];
			cptr m_name = (r_name + rr_ptr->name);
			sprintf(powdesc, "%s   Type: Summon  Num: %d   Dur: %d  Cost: %d", r_ptr->spell[i].name, r_ptr->spell[i].special1, r_ptr->spell[i].special2, r_ptr->spell[i].cost);
			strcpy(power_desc[num],powdesc);
			powers[num++]=i;
		}
		else if (r_ptr->spell[i].type == 8)
		{
			sprintf(powdesc, "%s  Type: Teleport  Pow: %d  Cost: %d", r_ptr->spell[i].name, r_ptr->spell[i].power, r_ptr->spell[i].cost);
			strcpy(power_desc[num],powdesc);
			powers[num++]=i;
		}
		else if (r_ptr->spell[i].type == 999)
		{
			char *powname;

			/* Calls lua! */
			call_lua("get_scripted_spell_name", "(dd)", "s", r_idx, i+1, &powname);

			sprintf(powdesc, "%s  Cost: %d", powname, r_ptr->spell[i].cost);
			strcpy(power_desc[num],powdesc);
			powers[num++]=i;
		}
		i++;
	}

        if(!num) {msg_print("No powers to use.");return 0;}

        if(only_number) return num;

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	/* Build a prompt (accept all spells) */
	if (num <= 26)
	{
		/* Build a prompt (accept all spells) */
                strnfmt(out_val, 78, "(Powers %c-%c, *=List, ESC=exit) Use which power? ",
			I2A(0), I2A(num - 1));
	}
	else
	{
                strnfmt(out_val, 78, "(Powers %c-%c, *=List, ESC=exit) Use which power? ",
			I2A(0), '0' + num - 27);
	}

	/* Get a spell from the user */
	while (!flag && get_com(out_val, &choice))
	{
		/* Request redraw */
		if ((choice == ' ') || (choice == '*') || (choice == '?'))
		{
			/* Show the list */
			if (!redraw)
			{
				byte y = 1, x = 0;
				int ctr = 0;
				char dummy[80];

				strcpy(dummy, "");

				/* Show list */
				redraw = TRUE;

				/* Save the screen */
				Term_save();

				prt ("", y++, x);

				while (ctr < num && ctr < 17)
				{
					sprintf(dummy, "%c) %s", I2A(ctr), power_desc[ctr]);
					prt(dummy, y + ctr, x);
					ctr++;
				}
				while (ctr < num)
				{
					if (ctr < 26)
					{
						sprintf(dummy, " %c) %s", I2A(ctr), power_desc[ctr]);
					}
					else
					{
						sprintf(dummy, " %c) %s", '0' + ctr - 26, power_desc[ctr]);
					}
					prt(dummy, y + ctr - 17, x + 40);
					ctr++;
				}
				if (ctr < 17)
				{
					prt ("", y + ctr, x);
				}
				else
				{
					prt ("", y + 17, x);
				}
			}

			/* Hide the list */
			else
			{
				/* Hide list */
				redraw = FALSE;

				/* Restore the screen */
				Term_load();
			}

			/* Redo asking */
			continue;
		}

		if (choice == '\r' && num == 1)
		{
			choice = 'a';
		}

		if (isalpha(choice))
		{
			/* Note verify */
			ask = (isupper(choice));

			/* Lowercase */
			if (ask) choice = tolower(choice);

			/* Extract request */
			i = (islower(choice) ? A2I(choice) : -1);
		}
		else
		{
			ask = FALSE; /* Can't uppercase digits */

			i = choice - '0' + 26;
		}

		/* Totally Illegal */
		if ((i < 0) || (i >= num))
		{
			bell();
			continue;
		}

		/* Save the spell index */
		Power = powers[i];

		/* Verify it */
		if (ask)
		{
			char tmp_val[160];

			/* Prompt */
			strnfmt(tmp_val, 78, "Use %s? ", power_desc[i]);

			/* Belay that order */
			if (!get_check(tmp_val)) continue;
		}

		/* Stop the loop */
		flag = TRUE;
	}

	/* Restore the screen */
	if (redraw) Term_load();

	/* Abort if needed */
	if (!flag) 
	{
		energy_use = 0;
                return num;
	}
	
	/* Before we cast... do we have enough mana? */
	if (p_ptr->csp < r_ptr->spell[Power].cost) 
	{
		msg_print("Not enough mana to use this power.");
		return 0;
	}

	call_lua("player_before_magic", "", "");

	/* Actually use the power! */
	switch (r_ptr->spell[Power].type)
	{
		/* Bolt */
		case 1:
		{
			if (r_ptr->spell[i].special3 == 1)
			{
				dam = r_ptr->spell[Power].power;
			}
			else
			{
				realpower = r_ptr->spell[Power].power;
				if (r_ptr->spell[Power].scalefactor > 0)
				{
					realpower += (r_ptr->spell[Power].scale * (p_ptr->lev / r_ptr->spell[Power].scalefactor));
				}
				call_lua("spell_damages", "(ddd)", "d", realpower, spellstat, 0, &dam);
				dam = dam + multiply_divide(dam, p_ptr->abilities_monster_spells[Power] * 20, 100);
			}
			
			if(!get_aim_dir(&dir)) return;
			p_ptr->events[29017] = (p_ptr->abilities[(CLASS_MONSTER * 10) + 2] * 10) + (p_ptr->abilities_monster_spells[Power] * 20);
			fire_bolt(r_ptr->spell[Power].special1, dir, dam);
			p_ptr->events[29017] = 0;
			break;
		}
		/* Ball */
		case 2:
		{
			if (r_ptr->spell[i].special3 == 1)
			{
				dam = r_ptr->spell[Power].power;
			}
			else
			{
				realpower = r_ptr->spell[Power].power;
				if (r_ptr->spell[Power].scalefactor > 0)
				{
					realpower += (r_ptr->spell[Power].scale * (p_ptr->lev / r_ptr->spell[Power].scalefactor));
				}
				call_lua("spell_damages", "(ddd)", "d", realpower, spellstat, 0, &dam);
				dam = dam + multiply_divide(dam, p_ptr->abilities_monster_spells[Power] * 20, 100);
			}
			
			rad = r_ptr->spell[Power].special2 + (p_ptr->abilities_monster_spells[Power] / 10);
			if(!get_aim_dir(&dir)) return;
			p_ptr->events[29017] = (p_ptr->abilities[(CLASS_MONSTER * 10) + 2] * 10) + (p_ptr->abilities_monster_spells[Power] * 20);
			fire_ball(r_ptr->spell[Power].special1, dir, dam, rad);
			p_ptr->events[29017] = 0;
			break;
		}
		/* Heal */
		case 3:
		{
			int totalmult = 0;
			realpower = r_ptr->spell[Power].power;
			if (r_ptr->spell[Power].scalefactor > 0)
			{
				realpower += (r_ptr->spell[Power].scale * (p_ptr->lev / r_ptr->spell[Power].scalefactor));
			}
			call_lua("spell_damages", "(ddd)", "d", realpower, spellstat, 0, &dam);

			totalmult = p_ptr->abilities[(CLASS_MONSTER * 10) + 2] * 10;
			if (p_ptr->boss_abilities & (BOSS_DOUBLE_MAGIC)) totalmult += 100;
			dam = dam + multiply_divide(dam, totalmult, 100);

			lua_project(-2, 0, py, px, dam, GF_OLD_HEAL, 1);
			msg_print("You are healed!");
			update_and_handle();
			break;
		}
		/* Haste */
		case 4:
		{
			dam = r_ptr->spell[Power].power;
			dam += multiply_divide(dam, (p_ptr->abilities[(CLASS_MONSTER * 10) + 2] * 10) + (p_ptr->abilities_monster_spells[Power] * 20), 100);
			(void)set_fast(dam);
			update_and_handle();
			break;
		}
		/* Boost */
		case 5:
		{
			dam = r_ptr->spell[Power].power;
			if (r_ptr->spell[Power].special1 == 1 || r_ptr->spell[Power].special1 == 4)
			{
				p_ptr->str_boost = r_ptr->spell[Power].power + multiply_divide(r_ptr->spell[Power].power, (p_ptr->abilities[(CLASS_MONSTER * 10) + 2] * 10) + (p_ptr->abilities_monster_spells[Power] * 20), 100);
                                (void)set_str_boost(20 + p_ptr->abilities_monster_spells[Power]); 
			}
			if (r_ptr->spell[Power].special1 == 2 || r_ptr->spell[Power].special1 == 9)
			{
				p_ptr->dex_boost = r_ptr->spell[Power].power + multiply_divide(r_ptr->spell[Power].power, (p_ptr->abilities[(CLASS_MONSTER * 10) + 2] * 10) + (p_ptr->abilities_monster_spells[Power] * 20), 100);
                                (void)set_dex_boost(20 + p_ptr->abilities_monster_spells[Power]); 
			}
			if (r_ptr->spell[Power].special1 == 3 || r_ptr->spell[Power].special1 == 5)
			{
				p_ptr->int_boost = r_ptr->spell[Power].power + multiply_divide(r_ptr->spell[Power].power, (p_ptr->abilities[(CLASS_MONSTER * 10) + 2] * 10) + (p_ptr->abilities_monster_spells[Power] * 20), 100);
                                (void)set_int_boost(20 + p_ptr->abilities_monster_spells[Power]);
				p_ptr->wis_boost = r_ptr->spell[Power].power + multiply_divide(r_ptr->spell[Power].power, (p_ptr->abilities[(CLASS_MONSTER * 10) + 2] * 10) + (p_ptr->abilities_monster_spells[Power] * 20), 100);
                                (void)set_wis_boost(20 + p_ptr->abilities_monster_spells[Power]);
			}
			if ((r_ptr->spell[Power].special1 == 6) || (r_ptr->spell[Power].special1 == 7) || (r_ptr->spell[Power].special1 == 8)) 
			{
				p_ptr->str_boost = r_ptr->spell[Power].power + multiply_divide(r_ptr->spell[Power].power, (p_ptr->abilities[(CLASS_MONSTER * 10) + 2] * 10) + (p_ptr->abilities_monster_spells[Power] * 20), 100);
				p_ptr->dex_boost = r_ptr->spell[Power].power + multiply_divide(r_ptr->spell[Power].power, (p_ptr->abilities[(CLASS_MONSTER * 10) + 2] * 10) + (p_ptr->abilities_monster_spells[Power] * 20), 100);
				p_ptr->int_boost = r_ptr->spell[Power].power + multiply_divide(r_ptr->spell[Power].power, (p_ptr->abilities[(CLASS_MONSTER * 10) + 2] * 10) + (p_ptr->abilities_monster_spells[Power] * 20), 100);
				p_ptr->wis_boost = r_ptr->spell[Power].power + multiply_divide(r_ptr->spell[Power].power, (p_ptr->abilities[(CLASS_MONSTER * 10) + 2] * 10) + (p_ptr->abilities_monster_spells[Power] * 20), 100);
				(void)set_str_boost(20 + p_ptr->abilities_monster_spells[Power]);
				(void)set_dex_boost(20 + p_ptr->abilities_monster_spells[Power]);
				(void)set_int_boost(20 + p_ptr->abilities_monster_spells[Power]);
				(void)set_wis_boost(20 + p_ptr->abilities_monster_spells[Power]);
			}
			update_and_handle();
			break;
		}

		/* Summon Kind */
		case 6:
		{
			realpower = r_ptr->spell[Power].power;
			if (r_ptr->spell[Power].scalefactor > 0)
			{
				realpower += (r_ptr->spell[Power].scale * (p_ptr->lev / r_ptr->spell[Power].scalefactor));
			}
			for (j = 0; j < (r_ptr->spell[Power].special1 + (p_ptr->abilities_monster_spells[Power] / 5)); j++)
			{
				summon_specific_kind(py, px, realpower + p_ptr->abilities[(CLASS_MONSTER * 10) + 2] + (p_ptr->abilities_monster_spells[Power] * 2), r_ptr->spell[Power].summchar, FALSE, TRUE, r_ptr->spell[Power].special2 + p_ptr->abilities[(CLASS_MONSTER * 10) + 2] + (p_ptr->abilities_monster_spells[Power] * 2));
			}
			break;
		}

		/* Summon Specific */
		case 7:
		{
			for (j = 0; j < r_ptr->spell[Power].special1 + (p_ptr->abilities_monster_spells[Power] / 5); j++)
			{
				summon_specific_ridx(py, px, r_ptr->spell[Power].power, FALSE, TRUE, r_ptr->spell[Power].special2 + p_ptr->abilities[(CLASS_MONSTER * 10) + 2] + (p_ptr->abilities_monster_spells[Power] * 2));
			}
			break;
		}
		/* Phase door */
		case 8:
		{
			teleport_player(r_ptr->spell[Power].power + (p_ptr->abilities_monster_spells[Power] * 5));
			break;
		}
		/* Scripted spell */
		case 999:
		{
			call_lua("use_scripted_spell", "(d)", "", Power+1);
			break;
		}
		
		default:
		{
			break;
		}
	}	

	/* Remove some mana */
	p_ptr->csp -= r_ptr->spell[Power].cost;

	call_lua("player_after_magic", "", "");

	update_and_handle();

        energy_use = 100;
        return num;
}

void special_weapon_charge()
{
                char new_name[80];
                char dummy_name[80];
                char ch = 0;
                int item;
                int amber_power = 0;
                object_type             *o_ptr;
                cptr q, s;
                u32b f1, f2, f3, f4;

                /* Get an item */
                q = "Which item? ";
                s = "You have no items!";
                if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP | USE_FLOOR))) return;

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
                object_flags(o_ptr, &f1, &f2, &f3, &f4);
                if ((f4 & (TR4_LEVELS)) || (o_ptr->tweakpoints > 0))
                {
                        while (TRUE)
			{
                                if (!get_com("Do what? [N]ame item, [T]weak", &ch))
				{
					amber_power = 0;
					break;
				}

                                if (ch == 'N' || ch == 'n')
				{
					amber_power = 1;
					break;
				}

                                if (ch == 'T' || ch == 't')
				{
					amber_power = 2;
					break;
				}
                        }
                        if (amber_power == 1)
                        {
                                if (!(get_string("How do you want to call the item?", dummy_name, 80))) return;
                                else
                                {
                                        strcpy(new_name, dummy_name);
                                        o_ptr->art_name = quark_add(new_name);
                                }
                        }
                        if (amber_power == 2)
                        {
                                if (!(f4 & (TR4_LEVELS)) && !(f4 & (TR4_ENCHANTED)) && (o_ptr->tval != TV_ESSENCE)) add_item_ability(o_ptr, TRUE);
                                else add_item_ability(o_ptr, FALSE);
                        }
                }
                else
                {
                                if (!(get_string("How do you want to call this item?", dummy_name, 80))) return;
                                else
                                {
                                        strcpy(new_name, dummy_name);
                                        o_ptr->art_name = quark_add(new_name);
                                }
                }
                        
}

/* Make an object Eternal */
void object_eternality()
{
                int item;
                object_type             *o_ptr;
                cptr q, s;

                /* Get an item */
                q = "Which item? ";
                s = "You have no items!";
                if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP | USE_FLOOR))) return;

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
                o_ptr->art_flags4 |= TR4_ETERNAL;
                msg_print("This object is now Eternal!");
}

/* Make an object Magic */
void make_item_magic()
{
                int item;
                object_type             *o_ptr;
                cptr q, s;

                /* Get an item */
                q = "Which item? ";
                s = "You have no items!";
                if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP | USE_FLOOR))) return;

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
                if (o_ptr->art_flags1 || o_ptr->art_flags2 || o_ptr->art_flags3 || o_ptr->art_flags4)
                {
                        msg_print("This item has already been enchanted!");
                }
                else
                {
                        apply_magic(o_ptr, p_ptr->lev, TRUE, TRUE, TRUE, FALSE);
                        if (o_ptr->pval <= 0)
                        {
                                o_ptr->pval = randint((p_ptr->lev / 7));
                                if (o_ptr->pval < 1) o_ptr->pval = 1;
                        }
                }
}

/* Used in the next function */
bool corpse_explode(s32b dam, int x, int y, int rad, int typ)
{
	int flg = PROJECT_GRID | PROJECT_KILL;

	/* Hook into the "project()" function */
        (void)project(0, rad, y, x, dam, typ, flg);

	/* Assume seen */
	return (TRUE);
}

/* A fun and deadly ability! */
void corpse_explosion()
{
        int x, y;
        s32b explosionpower;
        cave_type *c_ptr;
        object_type *o_ptr;
        monster_race *r_ptr;
        if (p_ptr->csp < 30)
        {
                msg_print("You need at least 30 mana points!");
                return;
        }
        msg_print("Point the location of the corpse...");
        if (!tgt_pt(&x, &y)) return;
        c_ptr = &cave[y][x];
        if (!c_ptr->o_idx)
        {
                msg_print("No corpse here!");
                return;
        }
        o_ptr = &o_list[c_ptr->o_idx];
        if (o_ptr->tval != TV_CORPSE)
        {
                msg_print("No corpse here!");
                return;
        }
        else
        {
                if (o_ptr->sval != 1)
                {
                        msg_print("You must use a full corpse!");
                        return;
                }
                /* Damage are equal to the corpse's life */
                explosionpower = o_ptr->pval3;
                corpse_explode(explosionpower, x, y, 5, GF_MANA);
                msg_print("The corpse explode!");
                delete_object_idx(c_ptr->o_idx);
                p_ptr->csp -= 30;
                update_and_handle();
        }
}

/* Convert mp(some) into hp */
/* May also be used as an healing magic */
/* Or to power up Monster Magics... */
void mana_shield()
{
        int newhp;
        if (p_ptr->chp > p_ptr->mhp)
        {
                msg_print("You turn off your mana shield.");
                p_ptr->chp = p_ptr->mhp;
                update_and_handle();
        }
        else
        {
                newhp = p_ptr->csp / 6;
                p_ptr->chp += newhp;
                p_ptr->csp = 0;
                update_and_handle();
                msg_print("You create a shield of mana!");
        }
}

/* Add poison brand to an item */
void assassin_poison_weapon()
{
	int item;
        object_type             *o_ptr;
        cptr q, s;
        u32b f1, f2, f3, f4;

        /* Restrict choices to weapons */
        item_tester_hook = item_tester_hook_poisonable;

        /* Get an item */
        q = "Poison which weapon/ammo/glove? ";
        s = "You have no valid weapons/ammos/gloves!";
        if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP | USE_FLOOR))) return;

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
        msg_print("Damages type changed to Poison!");
        o_ptr->extra1 = GF_POIS;
}

/* May be used by any function... */
void place_field(int ftype, byte rad, int x, int y, s32b dam)
{
        int i,j,w;
        cave_type *c_ptr;

	dam += multiply_divide(dam, (p_ptr->skill[1] * 10), 100);
        for(j = y - rad; j < y + rad + 1; j++)
        for(i = x - rad; i < x + rad + 1; i++)
        if((distance(y, x, j, i) <= rad) && in_bounds(j,i) && cave_clean_bold(j, i))
        {
                cave_set_feat(j, i, ftype);
                c_ptr = &cave[j][i];
                c_ptr->field_damage = dam;
        }
}

/* The first ability to use fields! */
void place_field_ability()
{
        int fieldtype = 0;
        s32b manaamount = 0;
        int ii, ij;
        char ch = 0;
	  while (TRUE)
			{
                        if (!get_com("Field type? [F]ire, [C]old, [E]lectricity ", &ch))
				{
                                        return;
                                }

                                if (ch == 'F' || ch == 'f')
				{
                                        fieldtype = FEAT_FIRE_FIELD;
					break;
				}

                                if (ch == 'C' || ch == 'c')
				{
                                        fieldtype = FEAT_COLD_FIELD;
					break;
				}

                                if (ch == 'E' || ch == 'e')
				{
                                        fieldtype = FEAT_ELEC_FIELD;
					break;
				}

			}
        manaamount = get_quantity("Use how much mana? ", p_ptr->csp);
        /* Need at least 3 mana! */
        if (manaamount < 3)
        {
                msg_print("Need at least 3 mana!");
                return;
        }
        p_ptr->csp -= manaamount;
        /* Fields do not need lot of damages to be effective... */
        manaamount = manaamount;
        msg_print("Create where? ");
        if (!tgt_pt(&ii,&ij)) return;
        place_field(fieldtype, 3, ii, ij, manaamount);
        msg_print("You create a field.");
        update_and_handle();
}

/* May be used by any function... */
void place_field_monsters(int ftype, byte rad, int x, int y, s32b dam)
{
        int i,j,w;
        cave_type *c_ptr;

	dam += multiply_divide(dam, (p_ptr->skill[1] * 10), 100);
        for(j = y - rad; j < y + rad + 1; j++)
        for(i = x - rad; i < x + rad + 1; i++)
        if((distance(y, x, j, i) <= rad) && in_bounds(j,i) && cave_clean_bold(j, i))
        {
                cave_set_feat(j, i, ftype);
                c_ptr = &cave[j][i];
                c_ptr->field_damage = dam;
		c_ptr->owner = 1;
        }
}

void morph_memorize()
{
        int y,x;
        cave_type *c_ptr;
        monster_type *m_ptr;
        monster_race *r_ptr;

        if (!tgt_pt(&x,&y)) return;
        c_ptr = &cave[y][x];

        if (!(c_ptr->m_idx))
        {
                msg_print("You must target a monster!");
                return;
        }
        m_ptr = &m_list[c_ptr->m_idx];
        r_ptr = &r_info[m_ptr->r_idx];
        if (r_ptr->flags1 & RF1_UNIQUE)
        {
                msg_print("You cannot memorize this monster.");
                return;
        }
	else if (is_pet(m_ptr) || m_ptr->angered_pet == 1)
        {
                msg_print("You cannot memorize friendly monsters.");
                return;
        }
        else if (r_ptr->level > (p_ptr->abilities[(CLASS_MONSTER_MAGE * 10) + 1] * 2))
        {
                msg_print("You need a higher ability to memorize this monster.");
                return;
        }
        else if (m_ptr->level > (p_ptr->abilities[(CLASS_MONSTER_MAGE * 10) + 1] * 3))
        {
                msg_print("This monster's level is too high for your current ability.");
                return;
        }
        else
        {
                msg_print("You successfully memorized the monster!");
                p_ptr->memorized = m_ptr->r_idx;
        }
}

void morph_into_memorized()
{
        if (p_ptr->memorized == 0)
        {
                msg_print("You haven't memorized a monster yet!");
                return;
        }
        else
        {
                p_ptr->body_monster = p_ptr->memorized;
                msg_print("You feel your body changing shape...");
                update_and_handle();
        }
}

/* Used by the Alteration spell "Un-Evolve" */
/* Thanks to Howdy for fixing this one! :) */
void do_cmd_unevolve_monster(monster_type *m_ptr)
{
        int chosenbody = 0;
        int trynumbers = 0;
        int y,x;
        bool okaysignal = FALSE;
        monster_race *r_ptr;
        monster_race *b_ptr;
        cave_type *c_ptr;

        x = m_ptr->fx;
        y = m_ptr->fy;

        if (m_ptr->boss >= 1)
        {
                msg_print("You may not target elites or bosses!");
                return;
        }

        b_ptr = &r_info[m_ptr->r_idx];
        if (b_ptr->flags1 & (RF1_UNIQUE))
        {
                msg_print("Unique monsters can't be targetted!");
                return;
        }
	if (b_ptr->cursed > 0)
	{
		msg_print("You cannot unevolve this monster.");
		return;
	}
                while (!okaysignal)
                {
						/*Howdy: We do not want to unevolve monster into a player, so
							we want to generate a number 
							from 1 to max_r_idx - 1. The max_r_idx is defined in 
							the file variable.c

							WARNING! ALL ARRAYS ARE INDEXED 
							FROM 0 TO max_r_idx - 1.
							
							Hence, r_info[max_r_idx] is a SERIOUS ERROR!

							ANOTHER WARNING! DO *NOT* ASSUME THAT r_info[x] holds
							useful data for x=0,1,2...max_r_idx-1. There are some
							"holes" in the r_info.txt, esp. for x>2000...
						*/
						
						/*	Howdy: The randint(N) generates an integer number from 1 to N */
                        /*Howdy: Still the same mistake - "1102" looks like a 
							hand-crafted value. Don't use that - it WILL break in the next 
							release; A constant would do the trick nicely.*/
						/*WAS: chosenbody = randint(1102);
                          WAS: chosenbody += 19;*/
						chosenbody = randint(max_r_idx - 1);
                        r_ptr = &r_info[chosenbody];
                        /* Must have the same d_char */
                        if (b_ptr->d_char == r_ptr->d_char) 
                        {                                    
                                /* Must be LOWER than the monster's current rlev */
                                /* We're talking about evolution here... */
							/*Howdy: We do not want to allow a special monster, hence !(r_ptr->flags9 & (RF9_SPECIAL_GENE)) */
							if ((r_ptr->level < b_ptr->level) && (!(r_ptr->flags1 & (RF1_UNIQUE))) && !(r_ptr->flags9 & (RF9_SPECIAL_GENE)) && (r_ptr->cursed == 0)) {
								/*Howdy: We do not want to allow a monster to change into a "always friendly" person, 
									otherwise the battle mage could be changed into a "Jindar Soldier" and so on.
									
									If the selected creature is friendly and the current one is not - do not allow for this change.
									We must also disallow summoning the creatures that never normally appear.
								*/
								if ((r_ptr->flags7 & (RF7_FRIENDLY)) && !(b_ptr->flags7 & (RF7_FRIENDLY))) {
									okaysignal = FALSE; 
								} else {
									okaysignal = TRUE;
								}
							}
							else trynumbers += 1;
                                /* Try 10000 times... should be enough! */
                                if (trynumbers >= 10000)
                                {
                                        msg_print("This monster can't be un-evolved further!");
                                        return;
                                }
                        }                                    
                }
                msg_print("The monster became a lesser specie of it's kind!");
                m_ptr->r_idx = chosenbody;
		call_lua("monster_stats", "(Mbbddd)", "", m_ptr, FALSE, FALSE, m_ptr->summoned, 0, 0);
                lite_spot(y, x);
                update_and_handle();
}

/* Used by the Vision spell "Scan Monster" */
void scan_targetting()
{
        int y,x;
        cave_type *c_ptr;
        monster_type *m_ptr;

        if (!tgt_pt(&x,&y)) return;
        c_ptr = &cave[y][x];

        if (!c_ptr->m_idx)
        {
                msg_print("You must target a monster!");
                return;
        }

        /* Get the monster */
        m_ptr = &m_list[c_ptr->m_idx];

        /* And scan it! */
        scan_monster(m_ptr);
}

void recharge_crystal()
{
                int item;
                object_type             *o_ptr;
                cptr q, s;
                u32b f1, f2, f3, f4;

                /* Restrict choices to bows */
                item_tester_tval = TV_CRYSTAL;

                /* Get an item */
                q = "Enchant which crystal? ";
                s = "You have no crystal!";
                if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP | USE_FLOOR))) return;

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
                msg_print("You recharge the crystal!");
                if (o_ptr->pval < 10) o_ptr->pval = 10;
}

void sharpen_ammos()
{
                int item;
                object_type             *o_ptr;
                cptr q, s;
                u32b f1, f2, f3, f4;

                /* Restrict choices to bows */
                item_tester_hook = item_tester_hook_ammo;

                /* Get an item */
                q = "Sharpen which ammos? ";
                s = "You have no ammos!";
                if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP | USE_FLOOR))) return;

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
                if (o_ptr->art_flags1 & (TR1_SHARPENED))
                {
                        msg_print("These ammos has already been sharpened!");
                }
                else
                {
                msg_print("You improve the ammos!");
                o_ptr->dd += 1;
                o_ptr->ds += 1;
                o_ptr->art_flags1 |= TR1_SHARPENED;
                /* This is to prevent players to sell these ammos */
                o_ptr->ident |= (IDENT_BROKEN);
                }

}

/* Dagger Fatal Stab... */
void dagger_fatal_stab()
{
                int item, dir;
                object_type             *o_ptr;
                cptr q, s;

                /* Restrict choices to bows */
                item_tester_hook = item_tester_hook_weapon_dagger;

                /* Get an item */
                q = "Use which dagger? ";
                s = "You have no daggers!";
                if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP))) return;

                /* Get the item */
                o_ptr = &inventory[item];

                /* Cannot use self-made items for balance issue */
                if (!(o_ptr->ident & (IDENT_BROKEN)))
                {

                        if (!get_aim_dir(&dir)) return;
                        fatal_stab(dir, o_ptr);
                        inven_item_increase(item, -1);
                        inven_item_describe(item);
                        inven_item_optimize(item);
                        update_and_handle();
                }
                else msg_print("You cannot use a stolen, broken or self-made dagger!");
}


void enchanted_blood()
{
        int usedhp;
        usedhp = get_quantity("Use how much hp?", p_ptr->chp);

        take_hit(usedhp, "Enchanted Blood");

        p_ptr->csp += usedhp;
        if (p_ptr->csp > p_ptr->msp) p_ptr->csp = p_ptr->msp;

        msg_print("You regain mana at the cost of your life!");
        update_and_handle();
}

/* Alteration spell "Evolve" */
/* Thanks to Howdy for fixing this one! */
void do_cmd_evolve_monster(monster_type *m_ptr)
{
        int chosenbody = 0;
        int trynumbers = 0;
        bool okaysignal = FALSE;

        monster_race *r_ptr;
        monster_race *b_ptr;

	if (m_ptr->boss >= 1)
        {
                msg_print("You may not target elites or bosses!");
                return;
        }

        b_ptr = &r_info[m_ptr->r_idx];
        if (b_ptr->flags1 & (RF1_UNIQUE))
        {
                msg_print("Unique monsters can't be targetted!");
                return;
        }
	if (b_ptr->cursed > 0)
	{
		msg_print("You cannot unevolve this monster.");
		return;
	}
                while (!okaysignal)
                {
						/*Howdy: We do not want to evolve monster into a player, so
							we want to generate a number 
							from 1 to max_r_idx - 1. The max_r_idx is defined in 
							the file variable.c

							WARNING! ALL ARRAYS ARE INDEXED 
							FROM 0 TO max_r_idx - 1.
							
							Hence, r_info[max_r_idx] is a SERIOUS ERROR!

							ANOTHER WARNING! DO *NOT* ASSUME THAT r_info[x] holds
							useful data for x=0,1,2...max_r_idx-1. There are some
							"holes" in the r_info.txt, esp. for x>2000...
						*/
						
						/*	Howdy: The randint(N) generates an integer number from 1 to N */
						chosenbody = randint(max_r_idx - 1);
						
						/*Howdy: Why 3000?! */
						/*WAS: chosenbody = randint(3000); */
						
						/*Howdy: Why 19?! I do not grasp it... */
                        /*WAS: chosenbody += 19;	*/
                        
						r_ptr = &r_info[chosenbody];
                        /* Must have the same d_char */
                        if (b_ptr->d_char == r_ptr->d_char) 
                        {                                    
                                /* Must be HIGHER than the monster's current rlev */
                                /* We're talking about evolution here... */
								/*Howdy: We do not want to allow a special monster, hence !(r_ptr->flags9 & (RF9_SPECIAL_GENE)) */
							if ((r_ptr->level > b_ptr->level) && (r_ptr->level <= m_ptr->level) && (!(r_ptr->flags1 & (RF1_UNIQUE))) && !(r_ptr->flags9 & (RF9_SPECIAL_GENE)) && (r_ptr->cursed == 0)) {
								/*Howdy: We do not want to allow a monster to change into a "always friendly" person, 
									otherwise the battle mage could be changed into a "Jindar Soldier" and so on.
									
									If the selected creature is friendly and the current one is not - do not allow for this change.
									We must also disallow summoning the creatures that never normally appear.
								*/
								if ((r_ptr->flags7 & (RF7_FRIENDLY)) && !(b_ptr->flags7 & (RF7_FRIENDLY))) {
									okaysignal = FALSE; 
								} else {
									okaysignal = TRUE;
								}
							}
                                else trynumbers += 1;
                                /* Try 10000 times... should be enough! */
                                if (trynumbers >= 10000)
                                {
                                        msg_print("The monster failed to evolve...");
                                        return;
                                }
                        }                                    
                }
                msg_print("The monster evolve into a stronger kind!");
                m_ptr->r_idx = chosenbody;
                lite_spot(m_ptr->fy, m_ptr->fx);
                call_lua("monster_stats", "(Mbbddd)", "", m_ptr, FALSE, FALSE, m_ptr->summoned, 0, 0);
                update_and_handle();
}

/* Turn target monster into a soul...if it's weak enough. */
void capture_soul(int x, int y)
{
        int flg = PROJECT_GRID | PROJECT_KILL;
        s32b capchance = 0;
        bool fear = FALSE;
        cave_type       *c_ptr;
        monster_type    *m_ptr;
        monster_race    *r_ptr;

        c_ptr = &cave[y][x];

        /* First, check if there's a monster... */
        if (c_ptr->m_idx)
        {
                /* Get the monster */
                m_ptr = &m_list[c_ptr->m_idx];
                r_ptr = &r_info[m_ptr->r_idx];

                if (r_ptr->flags1 & (RF1_UNIQUE)) capchance = (m_ptr->maxhp / 20);
                else capchance = (m_ptr->maxhp / 5);

                /* Capture only if the monster has at most 20% of */
                /* it's max hp. 5% if Unique. Yes, uniques are hard */
                /* to capture...but you can capture ANY monsters in */
                /* the game! */
                if (m_ptr->hp <= capchance && m_ptr->lives == 0)
                {
                        int chances;
                        /* Are we trying to capture a friendly monster? */
                        if (is_pet(m_ptr) || m_ptr->angered_pet >= 1)
                        {
                                msg_print("You cannot capture the soul of friendly/angry monsters!");
                                return;
                        }
			/* The random boss cannot be captured. */
			if (m_ptr->r_idx == 1030 || (m_ptr->r_idx >= 2050 && m_ptr->r_idx <= 2099) || (r_ptr->flags7 & (RF7_SCALED)))
			{
				msg_print("The soul of this monster cannot be captured.");
				return;
			}

                        /* Even if weakened, it's not guaranteed */
                        /* to succeed... it depends on monster's level */
                        /* VS your level! */
                        if (randint(p_ptr->stat_ind[A_WIS]) >= randint(m_ptr->level + m_ptr->mind))
                        {
                                object_type     forge;
                                object_type     *q_ptr; 
                        
                                msg_print("You capture the monster's soul!");

                                /* Get local object */
                                q_ptr = &forge;
        
                                object_prep(q_ptr, lookup_kind(TV_SOUL, 1));
                                q_ptr->number = 1;
                                object_aware(q_ptr);
                                object_known(q_ptr);

                                /* Store the monster's info in the soul! */
                                q_ptr->pval = m_ptr->r_idx;

                                /* Let's blast the little bugger afterwards! ;) */
                                mon_take_hit(c_ptr->m_idx, (m_ptr->hp + 1), &fear, NULL);

                                (void)inven_carry(q_ptr, FALSE);
                        }
                        else msg_print("You failed to capture the soul.");
                }
                else msg_print("You must weaken this monster more!");
        }
        else
	{
		msg_print("There's no monster here!");
		return;
	}

	energy_use = 100;
}

/* Bind a soul to an item */
void soul_bind()
{
        int item, soul, x, i;
	bool res = FALSE;
	bool resfound = FALSE;
        object_type             *o_ptr;
        object_type             *q_ptr;
	object_kind		*k_ptr;
        monster_race            *r_ptr;
        cptr q, s;
	u32b f1, f2, f3, f4;

        /* Get an item */

        item_tester_tval = TV_SOUL;

        q = "Which soul do you want to bind? ";
        s = "You have no souls!";
        if (!get_item(&soul, q, s, (USE_INVEN | USE_EQUIP))) return;

        /* Get the item (in the pack) */
        if (soul >= 0)
        {
                q_ptr = &inventory[soul];
        }
        /* Get the soul's monster */
        r_ptr = &r_info[q_ptr->pval];

        /* Get an item */
        item_tester_tval = 0;
        q = "Which item do you want to enchant? ";
        s = "You have no items to enchant!";
        if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP | USE_FLOOR))) return;

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

	k_ptr = &k_info[o_ptr->k_idx];

        /* Enchant the item with the right properties! */
        /* The more abilities the monster has, the more */
        /* powers the item gets! Note that not all powers */
        /* actually makes the item better... */
        if (o_ptr->art_flags1 || o_ptr->art_flags2 || o_ptr->art_flags3 || o_ptr->art_flags4)
        {
                msg_print("This item is already magical.");
                return;
        }
        else if (o_ptr->tval == TV_HYPNOS || o_ptr->tval == TV_CRYSTAL)
        {
                msg_print("You cannot enchant this item.");
                return;
        }
	else if (k_ptr->level > (p_ptr->abilities[(CLASS_SOUL_GUARDIAN * 10) + 2] * 3))
        {
                msg_print("You need a higher Soul Binding ability level.");
                return;
        }
        else
        {

        /* Valid item, begin enchanting! */
        o_ptr->art_flags1 |= (TR1_ENCHANTED);

	for (i = 0; i < MAX_RESIST; i++)
	{
		if (r_ptr->resistances[i] > 0 || r_ptr->resistances[i] < 0) resfound = TRUE;
	}

	if (resfound)
	{
		while (!(res))
		{
			s16b resamount;
			int which;
			s16b respercent;

			which = randint(MAX_RESIST);

			if (r_ptr->resistances[which] > 0 || r_ptr->resistances[which] < 0)
			{
				respercent = r_ptr->level / 2;
				if (respercent > 25) respercent = 25;
				resamount = multiply_divide(r_ptr->resistances[i], respercent, 100);
				o_ptr->resistances[i] = resamount;
				res = TRUE;
			}
		}
	}

	o_ptr->to_a = r_ptr->ac / 4;         

        if (r_ptr->flags3 & (RF3_NO_FEAR))
                o_ptr->art_flags2 |= (TR2_RES_FEAR);
        if (r_ptr->flags3 & (RF3_NO_STUN))
                o_ptr->art_flags4 |= (TR4_SAFETY);
        if ((r_ptr->flags3 & (RF3_NO_CONF)))
                o_ptr->art_flags2 |= (TR2_RES_CONF);
        if (r_ptr->flags2 & (RF2_AURA_FIRE))
                o_ptr->art_flags3 |= (TR3_SH_FIRE);
        if (r_ptr->flags2 & (RF2_AURA_ELEC))
                o_ptr->art_flags3 |= (TR3_SH_ELEC);
        if (r_ptr->flags2 & (RF2_REFLECTING))
                o_ptr->art_flags2 |= (TR2_REFLECT);
        if (r_ptr->flags2 & (RF2_REGENERATE))
                o_ptr->art_flags3 |= (TR3_REGEN);
        if (r_ptr->flags2 & (RF2_INVISIBLE))
	{
		o_ptr->invisibility = (r_ptr->level / 5);
	}

        if (r_ptr->flags2 & (RF2_POWERFUL) || r_ptr->flags1 & (RF1_UNIQUE))
	{
                o_ptr->lifebonus = o_ptr->lifebonus + (r_ptr->level * 2);
		o_ptr->manabonus = o_ptr->manabonus + (r_ptr->level * 2);

		o_ptr->art_flags4 |= (TR4_LEVELS);
		o_ptr->art_flags4 |= (TR4_ETERNAL);
		o_ptr->tweakpoints = 2;
		o_ptr->kills = 0;
		o_ptr->level = 1;
	}

	if (r_ptr->lives > 0) o_ptr->lifebonus = o_ptr->lifebonus + (r_ptr->level);
	if (r_ptr->spells >= 2) o_ptr->manabonus = o_ptr->manabonus + (r_ptr->level);


	/*  Stats bonus */
	if (r_ptr->str > 5)
	{
		o_ptr->statsbonus[A_STR] = o_ptr->statsbonus[A_STR] + (r_ptr->str / 2);
	}
	if (r_ptr->dex > 5)
	{
		o_ptr->statsbonus[A_DEX] = o_ptr->statsbonus[A_DEX] + (r_ptr->dex / 2);
	}
	if (r_ptr->mind > 5)
	{
		o_ptr->statsbonus[A_INT] = o_ptr->statsbonus[A_INT] + (r_ptr->mind / 2);
		o_ptr->statsbonus[A_WIS] = o_ptr->statsbonus[A_WIS] + (r_ptr->mind / 2);
	}

        if (r_ptr->speed >= 130)
	{
		o_ptr->speedbonus = o_ptr->speedbonus + ((r_ptr->speed - 130) / 2);
	}
	if (r_ptr->attacks >= 2)
	{
		o_ptr->extrablows = o_ptr->extrablows + (r_ptr->attacks / 2);
	}

	/* Could raise Fighting, Shooting or Spellcraft. */
	if (r_ptr->skill_attack >= 2)
	{
		o_ptr->skillsbonus[0] = o_ptr->skillsbonus[0] + r_ptr->skill_attack;
	}
	if (r_ptr->skill_ranged >= 2)
	{
		o_ptr->skillsbonus[2] = o_ptr->skillsbonus[2] + r_ptr->skill_ranged;
	}
	if (r_ptr->skill_magic >= 2)
	{
		o_ptr->skillsbonus[1] = o_ptr->skillsbonus[1] + r_ptr->skill_magic;
	}

        if (o_ptr->tval == TV_WEAPON || o_ptr->tval == TV_ROD || o_ptr->tval == TV_AMMO)
        {
                /* Observe "maximal" attacks */
                for (x = 0; x < 20; x++)
                {
                        /* Examine "actual" blows */
                        if (r_ptr->attack[x].type)
                        {
                                if (r_ptr->attack[x].element != GF_PHYSICAL)
				{
					o_ptr->brandtype = r_ptr->attack[x].element;
					o_ptr->branddam = (maxroll(r_ptr->attack[x].ddice, r_ptr->attack[x].dside) * 4) * 5;
					o_ptr->brandrad = 0;
				}                                
                        }                                                 

                }
                /* Enchance the damages...maybe */
                if (o_ptr->tval == TV_WEAPON || o_ptr->tval == TV_ROD)
                {
			int percentdam;

			percentdam = 100;

			if (o_ptr->weight < 100) percentdam -= (100 - o_ptr->weight);
			if (percentdam < 10) percentdam = 10;

                        o_ptr->dd += multiply_divide(r_ptr->attack[0].ddice, percentdam, 100);
                        o_ptr->ds += multiply_divide(r_ptr->attack[0].dside, percentdam, 100);
                }
                if (o_ptr->tval == TV_AMMO)
                {
                        o_ptr->dd += r_ptr->attack[0].ddice / 4;
                        o_ptr->ds += r_ptr->attack[0].dside / 4;
                }                                
        }
        if (o_ptr->tval == TV_SOFT_ARMOR || o_ptr->tval == TV_HARD_ARMOR || o_ptr->tval == TV_DRAG_ARMOR)
        {
                /* Bind a monster's soul to an armor! */
                o_ptr->ac += (r_ptr->ac / 20);
        }
        if (o_ptr->tval == TV_BOOTS || o_ptr->tval == TV_GLOVES || o_ptr->tval == TV_HELM
        || o_ptr->tval == TV_CROWN || o_ptr->tval == TV_SHIELD || o_ptr->tval == TV_CLOAK
        || o_ptr->tval == TV_ARM_BAND)
        {
                /* Bind a monster's soul to a glove, boots, etc...! */
                /* MUCH lower than an armor. */
                o_ptr->ac += (r_ptr->ac / 200);
        }

	if (r_ptr->flags2 & (RF2_POWERFUL) || r_ptr->flags1 & (RF1_UNIQUE))
	{
        	o_ptr->to_h += (r_ptr->level * 4);
        	o_ptr->to_d += (r_ptr->level * 4);
	}
	else if (r_ptr->flags1 & (RF1_QUESTOR))
	{
		o_ptr->to_h += (r_ptr->level * 10);
        	o_ptr->to_d += (r_ptr->level * 10);
	}
	else
	{
		o_ptr->to_h += r_ptr->level * 2;
        	o_ptr->to_d += r_ptr->level * 2;
	} 

	object_flags(o_ptr, &f1, &f2, &f3, &f4);

	/* No tweak points when binding on items without level gaining. */
	/* This is to prevent binding on crafted items and still get the tweaks. */
	if (!(f4 & (TR4_LEVELS)))
	{
		o_ptr->tweakpoints = 0;
	}

	/* Add activations when binding spellcasters! */
	/* But only if the item doesn't already have something. */
	if (!(f3 & (TR3_ACTIVATE)))
	{
		int itemact = 0;
		for (i = 0; i < 20; i++)
		{
			if (r_ptr->spell[i].type > 0 && r_ptr->spell[i].type < 9)
			{
				strcpy(o_ptr->spell[itemact].name, r_ptr->spell[i].name);
				strcpy(o_ptr->spell[itemact].act, r_ptr->spell[i].act);
				o_ptr->spell[itemact].type = r_ptr->spell[i].type;
				o_ptr->spell[itemact].power = r_ptr->spell[i].power;
				o_ptr->spell[itemact].special1 = r_ptr->spell[i].special1;
				o_ptr->spell[itemact].special2 = r_ptr->spell[i].special2;
				o_ptr->spell[itemact].special3 = r_ptr->spell[i].special3;
				o_ptr->spell[itemact].summchar = r_ptr->spell[i].summchar;
				if (r_ptr->spell[i].cost <= 0) o_ptr->spell[itemact].cost = 0;
				else o_ptr->spell[itemact].cost = r_ptr->spell[i].cost / 2;

				itemact += 1;
			}
		}

		if (itemact >= 1) o_ptr->art_flags3 |= (TR3_ACTIVATE);
	}

	/* Binding a soul of nightmares, hmmm? */
	if (r_ptr->cursed > 0) o_ptr->cursed = r_ptr->level;

        o_ptr->ident |= (IDENT_BROKEN);
        msg_print("You bind the soul to your item!");
	inven_item_increase(soul, -1);
        inven_item_describe(soul);
        inven_item_optimize(soul);
        update_and_handle();
        energy_use = 100;
        }

}

/* Return "true" if a grid is "lite" */
bool cave_lit(int y, int x)
{
        cave_type *c_ptr = c_ptr = &cave[y][x];
        if (c_ptr->info & (CAVE_LITE)) return TRUE;

        /* Default */
        return FALSE;
}

/* Turn an item into gold! :) */
void stone_to_gold()
{
        int item;
        s32b goldamount;
        object_type             *o_ptr;
        object_kind *k_ptr;
        cptr q, s;
        u32b f1, f2, f3, f4;

        /* Get an item */
        q = "Turn which item? ";
        s = "You have no items!";
        if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP))) return;

        /* Get the item */
        o_ptr = &inventory[item];

        object_flags(o_ptr, &f1, &f2, &f3, &f4);
        if ((o_ptr->ident & (IDENT_BROKEN)) || (o_ptr->ident & (IDENT_STOREB)))
        {
                msg_print("You cannot turn this item.");
                return;
        }
        k_ptr = &k_info[o_ptr->k_idx];
	goldamount = multiply_divide(k_ptr->cost, (p_ptr->abilities[(CLASS_MAGE * 10) + 8] * 10), 100);
        p_ptr->au += goldamount;
        msg_format("You gained %ld golds!", goldamount);
        inven_item_increase(item, -1);
        inven_item_describe(item);
        inven_item_optimize(item);
        update_and_handle();
}

/* Construct a magical knight! */
void animate_knight()
{
        int item, x, y;
        s32b basehp = 0;
        s32b hit_bonus = 0;
        int dam_d = 0;
        int dam_s = 0;
        object_type *weapon_ptr;
        object_type *armor_ptr;
        object_type *helm_ptr;
        object_type *gauntlet_ptr;
        object_type *boots_ptr;
        cptr q, s;

        /* Get a weapon */
        item_tester_hook = item_tester_hook_weapon;

        q = "Use which weapon? ";
        s = "You have no weapons!";
        if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP))) return;

        /* Get the item */
        weapon_ptr = &inventory[item];
        dam_d = weapon_ptr->dd;
        dam_s = weapon_ptr->ds;
        hit_bonus += weapon_ptr->to_h;
        basehp += (1 + weapon_ptr->to_a);
        inven_item_increase(item, -1);
        inven_item_describe(item);
        inven_item_optimize(item);


        /* Get a hard or dragon armor */
        item_tester_hook = item_tester_hook_hard_drag_armor;

        q = "Use which armor? ";
        s = "You have no armors!";
        if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP))) return;

        /* Get the item */
        armor_ptr = &inventory[item];
        hit_bonus += armor_ptr->to_h;
        basehp += armor_ptr->ac + armor_ptr->to_a;
        inven_item_increase(item, -1);
        inven_item_describe(item);
        inven_item_optimize(item);


        /* Get an helm */
        item_tester_hook = item_tester_hook_helmet;

        q = "Use which helmet? ";
        s = "You have no helmets!";
        if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP))) return;

        /* Get the item */
        helm_ptr = &inventory[item];
        hit_bonus += helm_ptr->to_h;
        basehp += helm_ptr->ac + helm_ptr->to_a;
        inven_item_increase(item, -1);
        inven_item_describe(item);
        inven_item_optimize(item);

        /* Get gauntlets */
        item_tester_hook = item_tester_hook_gauntlet;

        q = "Use which gauntlets? ";
        s = "You have no gauntlets!";
        if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP))) return;

        /* Get the item */
        gauntlet_ptr = &inventory[item];
        hit_bonus += gauntlet_ptr->to_h;
        basehp += gauntlet_ptr->ac + gauntlet_ptr->to_a;
        inven_item_increase(item, -1);
        inven_item_describe(item);
        inven_item_optimize(item);

        /* Get boots */
        item_tester_hook = item_tester_hook_boots;

        q = "Use which boots? ";
        s = "You have no boots!";
        if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP))) return;

        /* Get the item */
        boots_ptr = &inventory[item];
        hit_bonus += boots_ptr->to_h;
        basehp += boots_ptr->ac + boots_ptr->to_a;
        inven_item_increase(item, -1);
        inven_item_describe(item);
        inven_item_optimize(item);

        /* May the knight be built! :) */
        if (!tgt_pt(&x,&y)) return;
        place_monster_animated(y, x, 1134, FALSE, TRUE, basehp, hit_bonus, dam_d, dam_s);
        msg_print("You animate a magical knight!");
        update_and_handle();
}

void add_slay_brand(object_type *o_ptr)
{
	int                     Power = -1;
        int                     num = 0, dir = 0 , i;

	int             powers[36];
	char            power_desc[36][80];

	bool            flag, redraw;
        int             ask;
        int             numbrand = 0;

        char            choice;

	char            out_val[160];

        u32b f1, f2, f3, f4;
        object_flags(o_ptr, &f1, &f2, &f3, &f4);

        numbrand = p_ptr->abilities[(CLASS_ROGUE * 10) + 5] / 14;

        while (numbrand > 0)
        {

        /* List the powers */
        strcpy(power_desc[num],"Slay Animal");powers[num++]=1;
        strcpy(power_desc[num],"Slay Orc");powers[num++]=2;
        strcpy(power_desc[num],"Slay Troll");powers[num++]=3;
        strcpy(power_desc[num],"Slay Giant");powers[num++]=4;
        strcpy(power_desc[num],"Slay Dragon");powers[num++]=5;
        strcpy(power_desc[num],"Slay Demon");powers[num++]=6;
        strcpy(power_desc[num],"Slay Undead");powers[num++]=7;

        if(!num) {msg_print("There are no available slay brands.");return 0;}

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	/* Build a prompt (accept all spells) */
	if (num <= 26)
	{
		/* Build a prompt (accept all spells) */
                strnfmt(out_val, 78, "(Brands %c-%c, *=List, ESC=exit) Add which slay brand? ",
			I2A(0), I2A(num - 1));
	}
	else
	{
                strnfmt(out_val, 78, "(Brands %c-%c, *=List, ESC=exit) Add which slay brand? ",
			I2A(0), '0' + num - 27);
	}

	/* Get a spell from the user */
	while (!flag && get_com(out_val, &choice))
	{
		/* Request redraw */
		if ((choice == ' ') || (choice == '*') || (choice == '?'))
		{
			/* Show the list */
			if (!redraw)
			{
				byte y = 1, x = 0;
				int ctr = 0;
				char dummy[80];

				strcpy(dummy, "");

				/* Show list */
				redraw = TRUE;

				/* Save the screen */
				Term_save();

				prt ("", y++, x);

                                while (ctr < num && ctr < 19)
				{
					sprintf(dummy, "%c) %s", I2A(ctr), power_desc[ctr]);
					prt(dummy, y + ctr, x);
					ctr++;
				}
				while (ctr < num)
				{
					if (ctr < 26)
					{
						sprintf(dummy, " %c) %s", I2A(ctr), power_desc[ctr]);
					}
					else
					{
						sprintf(dummy, " %c) %s", '0' + ctr - 26, power_desc[ctr]);
					}
                                        prt(dummy, y + ctr - 19, x + 40);
					ctr++;
				}
                                if (ctr < 19)
				{
					prt ("", y + ctr, x);
				}
				else
				{
                                        prt ("", y + 19, x);
				}
			}

			/* Hide the list */
			else
			{
				/* Hide list */
				redraw = FALSE;

				/* Restore the screen */
				Term_load();
			}

			/* Redo asking */
			continue;
		}

		if (choice == '\r' && num == 1)
		{
			choice = 'a';
		}

		if (isalpha(choice))
		{
			/* Note verify */
			ask = (isupper(choice));

			/* Lowercase */
			if (ask) choice = tolower(choice);

			/* Extract request */
			i = (islower(choice) ? A2I(choice) : -1);
		}
		else
		{
			ask = FALSE; /* Can't uppercase digits */

			i = choice - '0' + 26;
		}

		/* Totally Illegal */
		if ((i < 0) || (i >= num))
		{
			bell();
			continue;
		}

		/* Save the spell index */
		Power = powers[i];

		/* Verify it */
		if (ask)
		{
			char tmp_val[160];

			/* Prompt */
			strnfmt(tmp_val, 78, "Use %s? ", power_desc[i]);

			/* Belay that order */
			if (!get_check(tmp_val)) continue;
		}

		/* Stop the loop */
		flag = TRUE;
	}

	/* Restore the screen */
	if (redraw) Term_load();

	/* Abort if needed */
	if (!flag) 
	{
		energy_use = 0;
                return num;
	}

        switch(Power)
        {
                case 1:
                        msg_print("Your weapon becomes Animal Slayer!");
                        o_ptr->art_flags1 |= (TR1_SLAY_ANIMAL);
                        numbrand -= 1;
                        break;
                case 2:
                        msg_print("Your weapon becomes Orc Slayer!");
                        o_ptr->art_flags1 |= (TR1_SLAY_ORC);
                        numbrand -= 1;
                        break;
                case 3:
                        msg_print("Your weapon becomes Troll Slayer!");
                        o_ptr->art_flags1 |= (TR1_SLAY_TROLL);
                        numbrand -= 1;
                        break;
                case 4:
                        msg_print("Your weapon becomes Giant Slayer!");
                        o_ptr->art_flags1 |= (TR1_SLAY_GIANT);
                        numbrand -= 1;
                        break;
                case 5:
                        msg_print("Your weapon becomes Dragon Slayer!");
                        o_ptr->art_flags1 |= (TR1_SLAY_DRAGON);
                        numbrand -= 1;
                        break;
                case 6:
                        msg_print("Your weapon becomes Demon Slayer!");
                        o_ptr->art_flags1 |= (TR1_SLAY_DEMON);
                        numbrand -= 1;
                        break;
                case 7:
                        msg_print("Your weapon becomes Undead Slayer!");
                        o_ptr->art_flags1 |= (TR1_SLAY_UNDEAD);
                        numbrand -= 1;
                        break;
        }
        }
}

void ranger_entangle()
{
        int ii, ij, rad;
        rad = (p_ptr->abilities[(CLASS_RANGER * 10) + 2] / 2) + 1;
        if (!tgt_pt(&ii,&ij)) return;
	if ((ij != py && ii != px) && (!cave_empty_bold(ij,ii) || (cave[ij][ii].info & CAVE_ICKY) || (!(cave[ij][ii].info & CAVE_MARK) && !(cave[ij][ii].info & CAVE_LITE))))
        {
        	msg_print("You can't place a field there.");
        }
        else
	{
		if (distance(ij,ii,py,px) > 10)
		{
			msg_print("This is too far!");
		}
        	else place_field(FEAT_VINE_FIELD, rad, ii, ij, 0);
	}       
}

/* Thorned Vines ability */
void ranger_thorned_vines()
{
        int fieldtype = 0;
        int ii, ij;
	int spellstat;
	s32b dam;
	s32b dambonus;
	/* This determines casting power */
	if (p_ptr->stat_ind[A_INT] > p_ptr->stat_ind[A_WIS]) spellstat = (p_ptr->stat_ind[A_INT] - 5);
	else spellstat = (p_ptr->stat_ind[A_WIS] - 5);

	dambonus = (p_ptr->skill[1] * 10);

	dam = (spellstat * (p_ptr->abilities[(CLASS_RANGER * 10) + 7] * 10));
	dam += multiply_divide(dam, dambonus, 100);

	/* No lower than 0. */
	if (spellstat < 0) spellstat = 0;
        msg_print("Place where? ");
        if (!tgt_pt(&ii,&ij)) return;
	if ((ij != py && ii != px) && (!cave_empty_bold(ij,ii) || (cave[ij][ii].info & CAVE_ICKY) || (!(cave[ij][ii].info & CAVE_MARK) && !(cave[ij][ii].info & CAVE_LITE))))
        {
        	msg_print("You can't place a field there.");
        }
        else
        {
		if (distance(ij,ii,py,px) > 10)
		{
			msg_print("This is too far!");
		}
        	else place_field(FEAT_THORNED_VINES, (p_ptr->abilities[(CLASS_RANGER * 10) + 7] / 20) + 2, ii, ij, dam);
	}
        update_and_handle();
}

/* The Ranger's Force of nature! */
void ranger_force_of_nature()
{
        cave_type *c_ptr;
        c_ptr = &cave[py][px];

        if (c_ptr->feat == FEAT_TREES || c_ptr->feat == FEAT_GRASS || c_ptr->feat == FEAT_SNOW_TREES)
        {
                msg_print("You empower yourself with the essence of nature!");
                p_ptr->str_boost = (p_ptr->abilities[(CLASS_RANGER * 10) + 9] * 10);
                p_ptr->dex_boost = (p_ptr->abilities[(CLASS_RANGER * 10) + 9] * 10);
                p_ptr->con_boost = (p_ptr->abilities[(CLASS_RANGER * 10) + 9] * 10);
                (void)set_str_boost((p_ptr->abilities[(CLASS_RANGER * 10) + 9] / 2) + 10);
                (void)set_dex_boost((p_ptr->abilities[(CLASS_RANGER * 10) + 9] / 2) + 10);
                (void)set_con_boost((p_ptr->abilities[(CLASS_RANGER * 10) + 9] / 2) + 10);
                update_and_handle();
        }
        else msg_print("You must stand on trees or grass to use this ability!");
}

/* Paladin ability Blade Of Purity! :) */
void blade_of_purity()
{
        object_type     forge;
        object_type     *q_ptr;
        object_type     *o_ptr;
	int slot;
	char ch;

	get_com("Summon in which slot? (1 or 2) ", &ch);
	if (ch == '1') slot = INVEN_WIELD;
	else slot = INVEN_WIELD+1;

	if (one_weapon_wield())
	{
		if (inventory[INVEN_WIELD].k_idx)
		{
			u32b f1, f2, f3, f4;
			object_type *n_ptr = &inventory[INVEN_WIELD];
			object_flags(n_ptr, &f1, &f2, &f3, &f4);
			if (f4 & (TR4_MUST2H))
			{
				inven_takeoff(INVEN_WIELD, 255, FALSE);
				slot = INVEN_WIELD;
			}
		}
		else if (inventory[INVEN_WIELD+1].k_idx)
		{ 
			u32b f1, f2, f3, f4;
			object_type *n_ptr = &inventory[INVEN_WIELD+1];
			object_flags(n_ptr, &f1, &f2, &f3, &f4);
			if (f4 & (TR4_MUST2H))
			{
				inven_takeoff(INVEN_WIELD+1, 255, FALSE);
				slot = INVEN_WIELD+1;
			}
		}
	}
	o_ptr = &inventory[slot];

        if (o_ptr->timeout > 0)
        {
                msg_print("You can only summon one weapon at time!");
                return;
        }
        if (o_ptr->tval > 0) inven_takeoff(slot, 255, FALSE);

        /* Get local object */
        q_ptr = &forge;
        
        object_prep(q_ptr, lookup_kind(TV_WEAPON, 44));
        q_ptr->number = 1;
        object_aware(q_ptr);
        object_known(q_ptr);

        /* Store the monster's info in the soul! */
        q_ptr->dd += (p_ptr->abilities[(CLASS_PALADIN * 10) + 4] / 3);
        q_ptr->ds += (p_ptr->abilities[(CLASS_PALADIN * 10) + 4] / 3);
        q_ptr->to_h = p_ptr->abilities[(CLASS_PALADIN * 10) + 4] * 3;
        q_ptr->to_d = p_ptr->abilities[(CLASS_PALADIN * 10) + 4] * 3;
        q_ptr->timeout = 10 + ((p_ptr->abilities[(CLASS_PALADIN * 10) + 4] - 1) * 2);
	if (p_ptr->abilities[(CLASS_PALADIN * 10) + 4] >= 10) q_ptr->timeout = 0;

        msg_print("A sword of pure light appears in your hand!");
        object_copy(&inventory[slot], q_ptr);

}

/* Paladin's Shining Armor! :) */
void paladin_shining_armor()
{
        int item;
        object_type             *o_ptr;
        cptr q, s;
        u32b f1, f2, f3, f4;

        /* Restrict choices to soft and hard armors */
        item_tester_hook = item_tester_hook_soft_hard_armor;

        /* Get an item */
        q = "Enchant which armor? ";
        s = "You have no armor!";
        if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP | USE_FLOOR))) return;

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

        object_flags(o_ptr, &f1, &f2, &f3, &f4);
        if (f4 & (TR4_ENHANCED))
        {
                msg_print("This armor has already been enhanced.");
        }
        else
        {
                msg_print("You bless your armor with divine power!");
                o_ptr->ac += p_ptr->abilities[(CLASS_PALADIN * 10) + 8] * 5;
                o_ptr->to_h += p_ptr->abilities[(CLASS_PALADIN * 10) + 8] * 2;
                o_ptr->to_d += p_ptr->abilities[(CLASS_PALADIN * 10) + 8] * 2;
                o_ptr->to_a += p_ptr->abilities[(CLASS_PALADIN * 10) + 8] * 2;

                o_ptr->light = 5;

                if (p_ptr->abilities[(CLASS_PALADIN * 10) + 8] >= 5)
                {
                        o_ptr->resistances[GF_LITE] = p_ptr->abilities[(CLASS_PALADIN * 10) + 8] * 2;
                }
                if (p_ptr->abilities[(CLASS_PALADIN * 10) + 8] >= 10)
                {
                        o_ptr->resistances[GF_DARK] = p_ptr->abilities[(CLASS_PALADIN * 10) + 8] * 2;
                }
		if (p_ptr->abilities[(CLASS_PALADIN * 10) + 8] >= 15)
                {
                        o_ptr->lifebonus = o_ptr->lifebonus + (p_ptr->abilities[(CLASS_PALADIN * 10) + 8] * 5);
                }
		if (p_ptr->abilities[(CLASS_PALADIN * 10) + 8] >= 20)
                {
                        o_ptr->statsbonus[A_CON] += (p_ptr->abilities[(CLASS_PALADIN * 10) + 8]);
                }
                if (p_ptr->abilities[(CLASS_PALADIN * 10) + 8] >= 25)
                {
                        o_ptr->art_flags4 |= (TR4_SAFETY);
			o_ptr->art_flags2 |= (TR2_FREE_ACT);
                }
                if (p_ptr->abilities[(CLASS_PALADIN * 10) + 8] >= 70)
                {
                        o_ptr->art_flags4 |= (TR4_PROTECTION);
                }

		o_ptr->art_flags4 |= (TR4_ENHANCED);

                identify_fully_aux(o_ptr);
                object_aware(o_ptr);
                object_known(o_ptr);
                o_ptr->ident |= (IDENT_MENTAL);
                /* This is to prevent players to sell this armor */
                o_ptr->ident |= (IDENT_BROKEN);
        }

}

/* Monk's Grappling Throw! :) */
void monk_throw_counter(monster_type *m_ptr)
{
        int tgt_x, tgt_y;
        int rad = (p_ptr->abilities[(CLASS_MONK * 10) + 3] / 20) + 3;
        s32b dam = 0;
        monster_race *r_ptr = &r_info[m_ptr->r_idx]; 
        cave_type *c_ptr;
	call_lua("monk_damages", "", "l", &dam);
	dam += multiply_divide(dam, ((p_ptr->abilities[(CLASS_MONK * 10) + 3] - 1) * 5), 100);
        if (!tgt_pt(&tgt_x,&tgt_y)) return;
        if (cave_empty_bold(tgt_y,tgt_x) && distance(tgt_y, tgt_x, py, px) <= rad)
        {
                if ((p_ptr->stat_ind[A_STR] * 300) >= r_ptr->weight)
                {
                        c_ptr = &cave[m_ptr->fy][m_ptr->fx];
                        move_monster_spot(c_ptr->m_idx, tgt_x, tgt_y);
                        update_and_handle();
                        msg_print("You throw the monster!");
                        nevermiss = TRUE;
                        corpse_explode(dam, tgt_x, tgt_y, 0, GF_PHYSICAL);
                        nevermiss = FALSE;
                }
                else msg_print("The monster is too heavy to throw!");
        }
        else msg_print("You cannot throw the monster there.");
}

/* Conjure an item */
/* The item is automatically worn, and it cannot be removed */
/* until it expires, after which it will disappear. */
void conjure_item(int itemtval, int itemsval, int duration, bool magic, bool special)
{
        object_type     forge;
        object_type     *q_ptr;
        object_type     *o_ptr;
        int slot, whichslot;
	char ch;
	bool nodrop = FALSE;

        if (itemtval == TV_WEAPON || itemtval == TV_ROD || itemtval == TV_SHIELD || itemtval == TV_RANGED)
        {
		/*get_com("Summon in which slot? (1 or 2) ", &ch);
		if (ch == '1') slot = INVEN_WIELD;
		else slot = INVEN_WIELD+1;*/
		slot = INVEN_WIELD;
        }
	else if (itemtval == TV_AMMO)
        {
                slot = INVEN_AMMO;
        }
        else if (itemtval == TV_SOFT_ARMOR || itemtval == TV_HARD_ARMOR || itemtval == TV_DRAG_ARMOR)
        {
                slot = INVEN_BODY;
        }
        else if (itemtval == TV_ARM_BAND)
        {
                slot = INVEN_ARM;
        }
        else if (itemtval == TV_HELM || itemtval == TV_CROWN)
        {
                slot = INVEN_HEAD;
        }
        else if (itemtval == TV_GLOVES)
        {
                slot = INVEN_HANDS;
        }
        else if (itemtval == TV_BOOTS)
        {
                slot = INVEN_FEET;
        }
        else if (itemtval == TV_CLOAK)
        {
                slot = INVEN_OUTER;
        }
        else
        {
                msg_print("You cannot conjure this item.");
                return;
        }
        
	/*if (slot == INVEN_WIELD || slot == INVEN_WIELD+1)
	{
		if (one_weapon_wield() || two_weapon_wield())
		{
			u32b f1, f2, f3, f4;
			object_type *j_ptr;
			object_prep(j_ptr, lookup_kind(itemtval, itemsval));	
			object_flags(j_ptr, &f1, &f2, &f3, &f4);
			if (f4 & (TR4_MUST2H))
			{
				if (two_weapon_wield())
				{
					inven_takeoff(INVEN_WIELD, 255, FALSE);
					inven_takeoff(INVEN_WIELD+1, 255, FALSE);
					nodrop = TRUE;
				}
				else
				{
					if (inventory[INVEN_WIELD].k_idx) inven_takeoff(INVEN_WIELD, 255, FALSE);
					else if (inventory[INVEN_WIELD+1].k_idx) inven_takeoff(INVEN_WIELD+1, 255, FALSE);
					nodrop = TRUE;
				}
				slot = INVEN_WIELD;
			}
		}
	}*/
	o_ptr = &inventory[slot];

        if (o_ptr->timeout > 0)
        {
                msg_print("You can only summon one item of this type at time!");
                return;
        }
        if (o_ptr->tval > 0) inven_takeoff(slot, 255, FALSE);
	if (slot == INVEN_WIELD)
	{
		object_type *oo_ptr = &inventory[INVEN_WIELD+1];
		if (oo_ptr->tval > 0) inven_takeoff(INVEN_WIELD+1, 255, FALSE);
	}

        /* Get local object */
        q_ptr = &forge; 
        
        object_prep(q_ptr, lookup_kind(itemtval, itemsval));
	if (slot == INVEN_AMMO)
	{
		int ammonum;
		ammonum = (p_ptr->skill[25] / 2) + 1;
		if (ammonum > 99) ammonum = 99;
		q_ptr->number = ammonum;
	}
        else q_ptr->number = 1;
        object_aware(q_ptr);
        object_known(q_ptr);

        if (magic && special) apply_magic(q_ptr, p_ptr->lev, TRUE, TRUE, TRUE, TRUE);
        else if (magic) apply_magic(q_ptr, p_ptr->lev, TRUE, TRUE, TRUE, FALSE);
        q_ptr->timeout = duration;
        object_aware(q_ptr);
        object_known(q_ptr);
        q_ptr->ident |= (IDENT_MENTAL);

        msg_print("You conjure an item!");
        object_copy(&inventory[slot], q_ptr);
        total_weight += q_ptr->weight;
        inven_item_describe(slot);

	o_ptr = &inventory[slot];
	/* Possibly call an item event. */
	if (o_ptr->event_summon != 0)
	{
		call_lua("item_summon", "(Od)", "", o_ptr, o_ptr->event_summon);
	}
        update_and_handle();
}

/* Paladin's Bless Weapon! :) */
/* Was originally used by the old Justice Warrior, hence the name. */
void justice_bless_weapon()
{
        int item;
        object_type             *o_ptr;
        cptr q, s;
        u32b f1, f2, f3, f4;

        /* Restrict choices to soft and hard armors */
        item_tester_hook = item_tester_hook_weapon;

        /* Get an item */
        q = "Enchant which weapon? ";
        s = "You have no weapon!";
        if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP | USE_FLOOR))) return;

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

	object_flags(o_ptr, &f1, &f2, &f3, &f4);
        if (f4 & (TR4_ENHANCED))
        {
                msg_print("This weapon has already been enhanced.");
        }
        else
        {
                msg_print("You bless your weapon with divine power!");
                o_ptr->to_h += p_ptr->abilities[(CLASS_PALADIN * 10) + 7] * 2;
                o_ptr->to_d += p_ptr->abilities[(CLASS_PALADIN * 10) + 7] * 2;
                o_ptr->dd += p_ptr->abilities[(CLASS_PALADIN * 10) + 7] / 5;
		o_ptr->ds += p_ptr->abilities[(CLASS_PALADIN * 10) + 7] / 5;

		o_ptr->light = 5;

		o_ptr->brandtype = GF_LITE;
		o_ptr->branddam = p_ptr->abilities[(CLASS_PALADIN * 10) + 7] * 200;
		o_ptr->brandrad = 0;

                if (p_ptr->abilities[(CLASS_PALADIN * 10) + 7] >= 5)
                {
                        o_ptr->resistances[GF_LITE] = (p_ptr->abilities[(CLASS_PALADIN * 10) + 7] * 2);
                }
                if (p_ptr->abilities[(CLASS_PALADIN * 10) + 7] >= 10)
                {
                        o_ptr->statsbonus[A_STR] += (p_ptr->abilities[(CLASS_PALADIN * 10) + 7]);
                }
                if (p_ptr->abilities[(CLASS_PALADIN * 10) + 7] >= 15)
                {
                        o_ptr->statsbonus[A_DEX] += (p_ptr->abilities[(CLASS_PALADIN * 10) + 7]);
                }
                if (p_ptr->abilities[(CLASS_PALADIN * 10) + 7] >= 20)
                {
                        o_ptr->art_flags1 |= (TR1_SLAY_UNDEAD);
                        o_ptr->art_flags1 |= (TR1_SLAY_DEMON);
                }
                if (p_ptr->abilities[(CLASS_PALADIN * 10) + 7] >= 30)
                {
                        o_ptr->extrablows += (p_ptr->abilities[(CLASS_PALADIN * 10) + 7] / 5);
                }

		o_ptr->art_flags4 |= (TR4_ENHANCED);

                identify_fully_aux(o_ptr);
                object_aware(o_ptr);
                object_known(o_ptr);
                o_ptr->ident |= (IDENT_MENTAL);
                /* This is to prevent players to sell this armor */
                o_ptr->ident |= (IDENT_BROKEN);
        }

}

/* Use Alchemy and/or Crafting skill to combine two items and make a new one! */
void combine_items(bool batch)
{
        int item, item2, craftype, x, item1idx, item2idx, tbonus;
	int reqskill;
	int highestlevel = 0;
	int curse = 0;
        object_type             *o_ptr;
        object_type             *q_ptr;
        object_type             forge;
        object_type             *a_ptr;
        object_kind *k_ptr;
	object_kind *k2_ptr;
        cptr q, s;
        u32b f1, f2, f3, f4;
        bool crafted = FALSE;
        bool item1alc = FALSE;
        bool item2alc = FALSE;
        /* object_flags(o_ptr, &f1, &f2, &f3, &f4); */

        /* Get an item */
        q = "Choose a first item... ";
        s = "You have no items!";
        if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP))) return;

        /* Get the item (in the pack) */
        if (item >= 0)
        {
                o_ptr = &inventory[item];
        }
        if (is_alchemy(o_ptr)) item1alc = TRUE;

        item1idx = o_ptr->k_idx;
	curse += o_ptr->cursed;

        inven_item_increase(item, -1);
        inven_item_describe(item);
        inven_item_optimize(item);


        /* Get an item */
        q = "Choose a second item... ";
        s = "You have no second items!";
        if (!get_item(&item2, q, s, (USE_INVEN | USE_EQUIP))) return;

        /* Get the item (in the pack) */
        if (item2 >= 0)
        {
                q_ptr = &inventory[item2];
        }
        if (is_alchemy(q_ptr)) item2alc = TRUE;

        item2idx = q_ptr->k_idx;
	curse += q_ptr->cursed;

        inven_item_increase(item2, -1);
        inven_item_describe(item2);
        inven_item_optimize(item2);

	if ((o_ptr->tval == TV_POTION && o_ptr->extra5 == 1) || (q_ptr->tval == TV_POTION && q_ptr->extra5 == 1))
	{
		msg_print("The combination failed because you used an invalid potion.");
		return;
	}

	/* Remember the highest level. */
	k_ptr = &k_info[item1idx];
	k2_ptr = &k_info[item2idx];

	if (k_ptr->level > k2_ptr->level) highestlevel = k_ptr->level;
	else highestlevel = k2_ptr->level;
	

        /* Determine the nature of the item.*/
        /* Which skill should we need? Alchemy? Crafting? Or both? */
        if (item1alc && item2alc) craftype = 1;
        if (!item1alc && !item2alc) craftype = 2;
        if ((!item1alc && item2alc) || (item1alc && !item2alc)) craftype = 3;

        /* Look for an item which has the recipe...*/
        for (x = 1; x <= max_k_idx; x++)
        {
                k_ptr = &k_info[x];
                if (item1idx == k_ptr->recipe1 || item1idx == k_ptr->recipe2)
                {
                        if (item2idx == k_ptr->recipe1 || item2idx == k_ptr->recipe2)        
                        {
				if (craftype == 3) reqskill = (k_ptr->level * (k_ptr->extra+1)) / 2;
				else reqskill = (k_ptr->level * (k_ptr->extra+1)) / 2;

                                if (craftype == 1 && p_ptr->skill[10] < reqskill)
                                {
                                        msg_format("You failed to make the item. You need a Alchemy skill of at least %d !", reqskill);
                                        return;
                                }
                                if (craftype == 2 && p_ptr->skill[11] < reqskill)
                                {
                                        msg_format("You failed to make the item. You need a Crafting skill of at least %d !", reqskill);
                                        return;
                                }
                                if (craftype == 3 && (p_ptr->skill[10] < reqskill || p_ptr->skill[11] < reqskill))
                                {
                                        msg_format("You failed to make the item. You need Alchemy and Crafting skills of at least %d !", reqskill);
                                        return;
                                }

                                /* Get local object */
                                a_ptr = &forge;
        
                                object_prep(a_ptr, lookup_kind(k_ptr->tval, k_ptr->sval));
                                a_ptr->number = 1;

				if (a_ptr->tval == TV_POTION && (batch))
				{
					a_ptr->number += (1 + (p_ptr->abilities[(CLASS_ENCHANTER * 10) + 8] / 2));
					a_ptr->extra5 = 1;
				}

				if (a_ptr->tval == TV_AMMO && (p_ptr->skill[11] >= 2))
				{
					int ammonum;
					ammonum = p_ptr->skill[11] / 2;
					if (ammonum > 99) ammonum = 99;
					a_ptr->number = ammonum;
				}

                                object_aware(a_ptr);
                                object_known(a_ptr);

                                msg_print("You created a new item!");
                                if (is_weapon(a_ptr) || a_ptr->tval == TV_ROD || a_ptr->tval == TV_RANGED || a_ptr->tval == TV_THROWING)
                                {
					/* "Blue Steel" */
					if (highestlevel >= 120)
					{
						a_ptr->dd += multiply_divide(a_ptr->dd, p_ptr->skill[11], 100);
						a_ptr->ds += multiply_divide(a_ptr->ds, p_ptr->skill[11], 100);
						if (a_ptr->dd > (k_ptr->dd * 6)) a_ptr->dd = k_ptr->dd * 6;
						if (a_ptr->ds > (k_ptr->ds * 6)) a_ptr->ds = k_ptr->ds * 6;
						a_ptr->to_h += ((p_ptr->skill[11]) * 5);
						a_ptr->to_d += ((p_ptr->skill[11]) * 5);

						if (a_ptr->tval == TV_RANGED) a_ptr->extra4 += multiply_divide(a_ptr->extra4, p_ptr->skill[11], 100);
					}
					/* "Titanium" */
					else if (highestlevel >= 90)
					{
						a_ptr->dd += multiply_divide(a_ptr->dd, p_ptr->skill[11], 100);
						a_ptr->ds += multiply_divide(a_ptr->ds, p_ptr->skill[11], 100);
						if (a_ptr->dd > (k_ptr->dd * 5)) a_ptr->dd = k_ptr->dd * 5;
						if (a_ptr->ds > (k_ptr->ds * 5)) a_ptr->ds = k_ptr->ds * 5;
						a_ptr->to_h += ((p_ptr->skill[11]) * 3);
						a_ptr->to_d += ((p_ptr->skill[11]) * 3);

						if (a_ptr->tval == TV_RANGED) a_ptr->extra4 += multiply_divide(a_ptr->extra4, p_ptr->skill[11], 100);
					}
					/* "Adamantium" */
					else if (highestlevel >= 60)
					{
						a_ptr->dd += multiply_divide(a_ptr->dd, p_ptr->skill[11], 100);
						a_ptr->ds += multiply_divide(a_ptr->ds, p_ptr->skill[11], 100);
						if (a_ptr->dd > (k_ptr->dd * 4)) a_ptr->dd = k_ptr->dd * 4;
						if (a_ptr->ds > (k_ptr->ds * 4)) a_ptr->ds = k_ptr->ds * 4;
						a_ptr->to_h += ((p_ptr->skill[11]) * 2);
						a_ptr->to_d += ((p_ptr->skill[11]) * 2);

						if (a_ptr->tval == TV_RANGED) a_ptr->extra4 += multiply_divide(a_ptr->extra4, (p_ptr->skill[11] / 2), 100);
					}
					/* "Mithril" */
					else if (highestlevel >= 40)
					{
						a_ptr->dd += multiply_divide(a_ptr->dd, p_ptr->skill[11], 100);
						a_ptr->ds += multiply_divide(a_ptr->ds, p_ptr->skill[11], 100);
						if (a_ptr->dd > (k_ptr->dd * 3)) a_ptr->dd = k_ptr->dd * 3;
						if (a_ptr->ds > (k_ptr->ds * 3)) a_ptr->ds = k_ptr->ds * 3;
						a_ptr->to_h += ((p_ptr->skill[11]));
						a_ptr->to_d += ((p_ptr->skill[11]));

						if (a_ptr->tval == TV_RANGED) a_ptr->extra4 += multiply_divide(a_ptr->extra4, (p_ptr->skill[11] / 4), 100);
					}
					/* "Steel" */
					else if (highestlevel >= 20)
					{
						a_ptr->dd += multiply_divide(a_ptr->dd, p_ptr->skill[11], 100);
						a_ptr->ds += multiply_divide(a_ptr->ds, p_ptr->skill[11], 100);
						if (a_ptr->dd > (k_ptr->dd * 2)) a_ptr->dd = k_ptr->dd * 2;
						if (a_ptr->ds > (k_ptr->ds * 2)) a_ptr->ds = k_ptr->ds * 2;
						a_ptr->to_h += ((p_ptr->skill[11]) / 2);
						a_ptr->to_d += ((p_ptr->skill[11]) / 2);

						if (a_ptr->tval == TV_RANGED) a_ptr->extra4 += multiply_divide(a_ptr->extra4, (p_ptr->skill[11] / 6), 100);
					}
					/* "Iron" */
					else if (highestlevel >= 10)
					{
						a_ptr->dd += multiply_divide(a_ptr->dd, p_ptr->skill[11] / 2, 100);
						a_ptr->ds += multiply_divide(a_ptr->ds, p_ptr->skill[11] / 2, 100);
						if (a_ptr->dd > (k_ptr->dd + (k_ptr->dd / 2))) a_ptr->dd = k_ptr->dd + (k_ptr->dd / 2);
						if (a_ptr->ds > (k_ptr->ds + (k_ptr->ds / 2))) a_ptr->ds = k_ptr->ds + (k_ptr->ds / 2);
						a_ptr->to_h += ((p_ptr->skill[11]) / 3);
						a_ptr->to_d += ((p_ptr->skill[11]) / 3);

						if (a_ptr->tval == TV_RANGED) a_ptr->extra4 += multiply_divide(a_ptr->extra4, (p_ptr->skill[11] / 8), 100);
					}
					/* "Bronze" */
					else
					{
						a_ptr->dd += multiply_divide(a_ptr->dd, p_ptr->skill[11] / 3, 100);
						a_ptr->ds += multiply_divide(a_ptr->ds, p_ptr->skill[11] / 3, 100);
						if (a_ptr->dd > (k_ptr->dd + (k_ptr->dd / 4))) a_ptr->dd = k_ptr->dd + (k_ptr->dd / 4);
						if (a_ptr->ds > (k_ptr->ds + (k_ptr->ds / 4))) a_ptr->ds = k_ptr->ds + (k_ptr->ds / 4);
						a_ptr->to_h += ((p_ptr->skill[11]) / 4);
						a_ptr->to_d += ((p_ptr->skill[11]) / 4);

						if (a_ptr->tval == TV_RANGED) a_ptr->extra4 += multiply_divide(a_ptr->extra4, (p_ptr->skill[11] / 10), 100);
					}
                                }
				if (a_ptr->tval == TV_AMMO)
                                {
					if (highestlevel >= 120)
					{
						a_ptr->dd += multiply_divide(a_ptr->dd, p_ptr->skill[11], 100);
						a_ptr->ds += multiply_divide(a_ptr->ds, p_ptr->skill[11], 100);
						if (a_ptr->dd > (k_ptr->dd * 4)) a_ptr->dd = k_ptr->dd * 4;
						if (a_ptr->ds > (k_ptr->ds * 4)) a_ptr->ds = k_ptr->ds * 4;
						a_ptr->to_h += ((p_ptr->skill[11]) * 5);
						a_ptr->to_d += ((p_ptr->skill[11]) * 5);
					}
					else if (highestlevel >= 90)
					{
						a_ptr->dd += multiply_divide(a_ptr->dd, p_ptr->skill[11], 100);
						a_ptr->ds += multiply_divide(a_ptr->ds, p_ptr->skill[11], 100);
						if (a_ptr->dd > (k_ptr->dd * 3)) a_ptr->dd = k_ptr->dd * 3;
						if (a_ptr->ds > (k_ptr->ds * 3)) a_ptr->ds = k_ptr->ds * 3;
						a_ptr->to_h += ((p_ptr->skill[11]) * 3);
						a_ptr->to_d += ((p_ptr->skill[11]) * 3);
					}
					else if (highestlevel >= 60)
					{
						a_ptr->dd += multiply_divide(a_ptr->dd, p_ptr->skill[11] / 2, 100);
						a_ptr->ds += multiply_divide(a_ptr->ds, p_ptr->skill[11] / 2, 100);
						if (a_ptr->dd > (k_ptr->dd * 3)) a_ptr->dd = k_ptr->dd * 3;
						if (a_ptr->ds > (k_ptr->ds * 3)) a_ptr->ds = k_ptr->ds * 3;
						a_ptr->to_h += ((p_ptr->skill[11]) * 2);
						a_ptr->to_d += ((p_ptr->skill[11]) * 2);
					}
					else if (highestlevel >= 40)
					{
						a_ptr->dd += multiply_divide(a_ptr->dd, p_ptr->skill[11] / 3, 100);
						a_ptr->ds += multiply_divide(a_ptr->ds, p_ptr->skill[11] / 3, 100);
						if (a_ptr->dd > (k_ptr->dd * 2)) a_ptr->dd = k_ptr->dd * 2;
						if (a_ptr->ds > (k_ptr->ds * 2)) a_ptr->ds = k_ptr->ds * 2;
						a_ptr->to_h += ((p_ptr->skill[11]));
						a_ptr->to_d += ((p_ptr->skill[11]));
					}
					else if (highestlevel >= 20)
					{
						a_ptr->dd += multiply_divide(a_ptr->dd, p_ptr->skill[11] / 4, 100);
						a_ptr->ds += multiply_divide(a_ptr->ds, p_ptr->skill[11] / 4, 100);
						if (a_ptr->dd > (k_ptr->dd + (k_ptr->dd / 2))) a_ptr->dd = k_ptr->dd + (k_ptr->dd / 2);
						if (a_ptr->ds > (k_ptr->ds + (k_ptr->ds / 2))) a_ptr->ds = k_ptr->ds + (k_ptr->ds / 2);
						a_ptr->to_h += ((p_ptr->skill[11]) / 2);
						a_ptr->to_d += ((p_ptr->skill[11]) / 2);
					}
					else if (highestlevel >= 10)
					{
						a_ptr->dd += multiply_divide(a_ptr->dd, p_ptr->skill[11] / 5, 100);
						a_ptr->ds += multiply_divide(a_ptr->ds, p_ptr->skill[11] / 5, 100);
						if (a_ptr->dd > (k_ptr->dd + (k_ptr->dd / 5))) a_ptr->dd = k_ptr->dd + (k_ptr->dd / 5);
						if (a_ptr->ds > (k_ptr->ds + (k_ptr->ds / 5))) a_ptr->ds = k_ptr->ds + (k_ptr->ds / 5);
						a_ptr->to_h += ((p_ptr->skill[11]) / 3);
						a_ptr->to_d += ((p_ptr->skill[11]) / 3);
					}
					else
					{
						a_ptr->dd += multiply_divide(a_ptr->dd, p_ptr->skill[11] / 6, 100);
						a_ptr->ds += multiply_divide(a_ptr->ds, p_ptr->skill[11] / 6, 100);
						if (a_ptr->dd > (k_ptr->dd + (k_ptr->dd / 6))) a_ptr->dd = k_ptr->dd + (k_ptr->dd / 6);
						if (a_ptr->ds > (k_ptr->ds + (k_ptr->ds / 6))) a_ptr->ds = k_ptr->ds + (k_ptr->ds / 6);
						a_ptr->to_h += ((p_ptr->skill[11]) / 4);
						a_ptr->to_d += ((p_ptr->skill[11]) / 4);
					}
                                }
                                if (a_ptr->tval == TV_SOFT_ARMOR || a_ptr->tval == TV_HARD_ARMOR || a_ptr->tval == TV_DRAG_ARMOR
                                || a_ptr->tval == TV_SHIELD || a_ptr->tval == TV_HELM || a_ptr->tval == TV_CLOAK || a_ptr->tval == TV_BOOTS
                                || a_ptr->tval == TV_GLOVES || a_ptr->tval == TV_ARM_BAND || a_ptr->tval == TV_CROWN)
                                {
					if (highestlevel >= 120)
					{
						a_ptr->ac += multiply_divide(a_ptr->ac, p_ptr->skill[11] * 5, 100);
						a_ptr->to_h += ((p_ptr->skill[11]) * 5);
						a_ptr->to_d += ((p_ptr->skill[11]) * 5);
						a_ptr->to_a += ((p_ptr->skill[11]) * 5);
					}
					else if (highestlevel >= 90)
					{
						a_ptr->ac += multiply_divide(a_ptr->ac, p_ptr->skill[11] * 4, 100);
						a_ptr->to_h += ((p_ptr->skill[11]) * 3);
						a_ptr->to_d += ((p_ptr->skill[11]) * 3);
						a_ptr->to_a += ((p_ptr->skill[11]) * 3);
					}
					else if (highestlevel >= 60)
					{
						a_ptr->ac += multiply_divide(a_ptr->ac, p_ptr->skill[11] * 3, 100);
						a_ptr->to_h += ((p_ptr->skill[11]) * 2);
						a_ptr->to_d += ((p_ptr->skill[11]) * 2);
						a_ptr->to_a += ((p_ptr->skill[11]) * 2);
					}
					else if (highestlevel >= 40)
					{
						a_ptr->ac += multiply_divide(a_ptr->ac, p_ptr->skill[11] * 2, 100);
						a_ptr->to_h += ((p_ptr->skill[11]));
						a_ptr->to_d += ((p_ptr->skill[11]));
						a_ptr->to_a += ((p_ptr->skill[11]));
					}
					else if (highestlevel >= 20)
					{
						a_ptr->ac += multiply_divide(a_ptr->ac, p_ptr->skill[11], 100);
						a_ptr->to_h += ((p_ptr->skill[11]) / 2);
						a_ptr->to_d += ((p_ptr->skill[11]) / 2);
						a_ptr->to_a += ((p_ptr->skill[11]) / 2);
					}
					else if (highestlevel >= 10)
					{
						a_ptr->ac += multiply_divide(a_ptr->ac, p_ptr->skill[11] / 2, 100);
						a_ptr->to_h += ((p_ptr->skill[11]) / 3);
						a_ptr->to_d += ((p_ptr->skill[11]) / 3);
						a_ptr->to_a += ((p_ptr->skill[11]) / 3);
					}
					else
					{
						a_ptr->ac += multiply_divide(a_ptr->ac, p_ptr->skill[11] / 3, 100);
						a_ptr->to_h += ((p_ptr->skill[11]) / 4);
						a_ptr->to_d += ((p_ptr->skill[11]) / 4);
						a_ptr->to_a += ((p_ptr->skill[11]) / 4);
					}
                                }
				if (a_ptr->tval == TV_LITE)
				{
					if (highestlevel >= 120)
					{
						a_ptr->to_h += ((p_ptr->skill[11]) * 5);
						a_ptr->to_d += ((p_ptr->skill[11]) * 5);
					}
					else if (highestlevel >= 90)
					{
						a_ptr->to_h += ((p_ptr->skill[11]) * 3);
						a_ptr->to_d += ((p_ptr->skill[11]) * 3);
					}
					else if (highestlevel >= 60)
					{
						a_ptr->to_h += ((p_ptr->skill[11]) * 2);
						a_ptr->to_d += ((p_ptr->skill[11]) * 2);
					}
					else if (highestlevel >= 40)
					{
						a_ptr->to_h += ((p_ptr->skill[11]));
						a_ptr->to_d += ((p_ptr->skill[11]));
					}
					else if (highestlevel >= 20)
					{
						a_ptr->to_h += ((p_ptr->skill[11]) / 2);
						a_ptr->to_d += ((p_ptr->skill[11]) / 2);
					}
					else if (highestlevel >= 10)
					{
						a_ptr->to_h += ((p_ptr->skill[11]) / 3);
						a_ptr->to_d += ((p_ptr->skill[11]) / 3);
					}
					else
					{
						a_ptr->to_h += ((p_ptr->skill[11]) / 4);
						a_ptr->to_d += ((p_ptr->skill[11]) / 4);
					}
				}

				/* Is it cursed? */
				if (curse > 0) a_ptr->cursed = curse;

				/* Give tweak points. */
				tbonus = 0;
				if (p_ptr->abilities[(CLASS_ENCHANTER * 10) + 9] >= 1) tbonus = (p_ptr->abilities[(CLASS_ENCHANTER * 10) + 9] / 3) + 1;

				/* If you crafted something cursed, you get extra skill points. */
				/* As well as other bonuses. */
				if (a_ptr->cursed > 0)
				{
					tbonus += a_ptr->cursed / 5;
					if (a_ptr->dd > 0) a_ptr->dd += multiply_divide(a_ptr->dd, a_ptr->cursed, 100);
					if (a_ptr->ds > 0) a_ptr->ds += multiply_divide(a_ptr->ds, a_ptr->cursed, 100);
					if (a_ptr->ac > 0) a_ptr->ac += multiply_divide(a_ptr->ac, a_ptr->cursed, 100);
					if (a_ptr->to_h > 0) a_ptr->to_h += multiply_divide(a_ptr->to_h, a_ptr->cursed, 100);
					if (a_ptr->to_d > 0) a_ptr->to_d += multiply_divide(a_ptr->to_d, a_ptr->cursed, 100);
					if (a_ptr->to_a > 0) a_ptr->to_a += multiply_divide(a_ptr->to_a, a_ptr->cursed, 100);
				}

				if (p_ptr->skill[11] >= 10)
				{
					if (craftype == 2) a_ptr->tweakpoints = (p_ptr->skill[11] / 5) + tbonus;
					if (craftype == 3) a_ptr->tweakpoints = (((p_ptr->skill[10] + p_ptr->skill[11]) / 2) / 5) + tbonus;
				}

				object_aware(a_ptr);
				object_known(a_ptr);
				a_ptr->ident |= (IDENT_MENTAL);
                		a_ptr->ident |= (IDENT_BROKEN);

                                (void)inven_carry(a_ptr, FALSE);
                                crafted = TRUE;
				break;
                        }
                }
        }
        if (!crafted)
        {                
                msg_print("The combination resulted in nothing.");
        }
}

bool is_alchemy(object_type *o_ptr)
{
        if (o_ptr->tval == TV_POTION || o_ptr->tval == TV_SCROLL || o_ptr->tval == TV_WAND ||
        o_ptr->tval == TV_STAFF || o_ptr->tval == TV_BATERIE ||
        o_ptr->tval == TV_FLASK || o_ptr->tval == TV_LITE || o_ptr->tval == TV_CRYSTAL
        || o_ptr->tval == TV_HYPNOS || o_ptr->tval == TV_EGG || o_ptr->tval == TV_CORPSE || o_ptr->tval == TV_GOLD || o_ptr->tval == TV_LICIALHYD) return (TRUE);

        return (FALSE);
}

void decompose_item()
{
        int item;
	int curse = 0;
        object_type             *o_ptr;
        object_type             forge;
        object_type             *a_ptr;
        object_kind *k_ptr;
	u32b f1, f2, f3, f4;
        cptr q, s;

        /* Get an item */
        q = "Choose an item... ";
        s = "You have no items!";
        if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP))) return;

        /* Get the item (in the pack) */
        if (item >= 0)
        {
                o_ptr = &inventory[item];
        }

	object_flags(o_ptr, &f1, &f2, &f3, &f4);

	if (o_ptr->cursed > 0) curse = o_ptr->cursed / 2;

	if (o_ptr->tval == TV_POTION && o_ptr->extra5 == 1)
	{
		msg_print("You cannot decompose this potion.");
		return;
	}
	if (f4 & (TR4_CRAFTED))
	{
		msg_print("Enchanted crafted items cannot be decomposed.");
		return;
	}
	if (o_ptr->timeout > 0)
	{
		msg_print("You cannot decompose a charging or summoned item.");
		return;
	}
	if (o_ptr->name1)
	{
		msg_print("You cannot decompose artifacts.");
		return;
	}
	if (o_ptr->tval == TV_AMMO)
	{
		msg_print("You cannot decompose ammos.");
		return;
	}

        k_ptr = &k_info[o_ptr->k_idx];
	if (!is_alchemy(o_ptr) && (p_ptr->skill[11] < (k_ptr->level * 10)))
	{
		msg_format("You need a Crafting skill of at least %d to decompose this item.", (k_ptr->level * 10));
		return;
	}

        if (k_ptr->recipe1 != 0 && k_ptr->recipe2 != 0 && !o_ptr->timeout)
        {
                int rec1, rec2;
                rec1 = k_ptr->recipe1;
                rec2 = k_ptr->recipe2;

                inven_item_increase(item, -1);
                inven_item_describe(item);
                inven_item_optimize(item);

                k_ptr = &k_info[rec1];
                a_ptr = &forge;
        
                object_prep(a_ptr, lookup_kind(k_ptr->tval, k_ptr->sval));
                object_aware(a_ptr);
                object_known(a_ptr);

		a_ptr->ident |= (IDENT_BROKEN);

		a_ptr->cursed = curse;

                (void)inven_carry(a_ptr, FALSE);


                k_ptr = &k_info[rec2];
                a_ptr = &forge;
        
                object_prep(a_ptr, lookup_kind(k_ptr->tval, k_ptr->sval));
                object_aware(a_ptr);
                object_known(a_ptr);

		a_ptr->ident |= (IDENT_BROKEN);

		a_ptr->cursed = curse;

                (void)inven_carry(a_ptr, FALSE);
        }
        else msg_print("You cannot decompose this item.");
}

/* Zelar's legs breaking throw! */
void zelar_leg_throw_execute(monster_type *m_ptr)
{
        int tgt_x, tgt_y;
        int rad = 3;
	int ppower, mpower;
        s32b dam = 0;
        monster_race *r_ptr = &r_info[m_ptr->r_idx]; 
        cave_type *c_ptr;
	call_lua("monk_damages", "", "l", &dam);
	dam += multiply_divide(dam, ((p_ptr->abilities[(CLASS_ZELAR * 10) + 4] - 1) * 5), 100);
        if (!tgt_pt(&tgt_x,&tgt_y)) return;
        if (cave_empty_bold(tgt_y,tgt_x) && distance(tgt_y, tgt_x, py, px) <= rad)
        {
                if (((p_ptr->abilities[(CLASS_ZELAR * 10) + 4] * 100)+700) >= r_ptr->weight)
                {
                        c_ptr = &cave[m_ptr->fy][m_ptr->fx];
                        move_monster_spot(c_ptr->m_idx, tgt_x, tgt_y);
                        update_and_handle();
                        msg_print("You throw the monster!");
			ppower = p_ptr->abilities[(CLASS_ZELAR * 10) + 4] * 10;
			mpower = m_ptr->level + m_ptr->str;
			if (randint(ppower) >= randint(mpower))
			{
                        	if (m_ptr->boss < 1 && !(r_ptr->flags1 & (RF1_UNIQUE)))
                        	{
                                	m_ptr->abilities |= (MUTILATE_LEGS);
					msg_print("Legs have been broken!");
                        	}
			}
                        nevermiss = TRUE;
                        corpse_explode(dam, tgt_x, tgt_y, 0, GF_PHYSICAL);
                        nevermiss = FALSE;
                }
                else msg_print("The monster is too heavy to throw!");
        }
        else msg_print("You cannot throw the monster there.");
}

void dark_mist_ability()
{
        int ii, ij, rad;
        rad = (p_ptr->abilities[(CLASS_SHADOW * 10) + 6] / 5) + 1;
        if (!tgt_pt(&ii,&ij)) return;
        place_field(FEAT_DARK_MIST, rad, ii, ij, 0);        
}

/* Turn target monster into a soul...if it's weak enough. */
void talk_to_monster(int x, int y)
{
        cave_type       *c_ptr;
        monster_type    *m_ptr;
        monster_race    *r_ptr;

        c_ptr = &cave[y][x];

        /* First, check if there's a monster... */
        if (c_ptr->m_idx)
        {
                /* Get the monster */
                m_ptr = &m_list[c_ptr->m_idx];
                r_ptr = &r_info[m_ptr->r_idx];

		/* Speak if friendly. Hostile monsters won't talk with us! */
		if (is_pet(m_ptr))
		{
			if (r_ptr->extra2 > 0) show_dialog(r_ptr->extra2);
			else msg_print("Nothing to say.");
		}
		else msg_print("This monster is an enemy, and won't speak to you.");
        }
        else msg_print("Nobody there.");
}

void sharpen_arrows()
{
                int item;
                object_type             *o_ptr;
                cptr q, s;
                u32b f1, f2, f3, f4;

                /* Restrict choices to bows */
                item_tester_hook = item_tester_hook_arrow;

                /* Get an item */
                q = "Sharpen which arrows? ";
                s = "You have no arrows!";
                if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP | USE_FLOOR))) return;

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
                if (o_ptr->art_flags1 & (TR1_SHARPENED))
                {
                        msg_print("These arrows has already been sharpened!");
                }
                else
                {
                msg_print("You sharpen the arrows!");
                o_ptr->dd += 1;
                o_ptr->ds += 3;
                o_ptr->art_flags1 |= TR1_SHARPENED;
                /* This is to prevent players to sell these ammos */
                o_ptr->ident |= (IDENT_BROKEN);
                }

}

void sharpen_bolts()
{
                int item;
                object_type             *o_ptr;
                cptr q, s;
                u32b f1, f2, f3, f4;

                /* Restrict choices to bows */
                item_tester_hook = item_tester_hook_bolt;

                /* Get an item */
                q = "Sharpen which bolts? ";
                s = "You have no bolts!";
                if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP | USE_FLOOR))) return;

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
                if (o_ptr->art_flags1 & (TR1_SHARPENED))
                {
                        msg_print("These bolts has already been sharpened!");
                }
                else
                {
                msg_print("You sharpen the bolts!");
                o_ptr->dd += 1;
                o_ptr->ds += 3;
                o_ptr->art_flags1 |= TR1_SHARPENED;
                /* This is to prevent players to sell these ammos */
                o_ptr->ident |= (IDENT_BROKEN);
                }

}

void sharpen_shots()
{
                int item;
                object_type             *o_ptr;
                cptr q, s;
                u32b f1, f2, f3, f4;

                /* Restrict choices to bows */
                item_tester_hook = item_tester_hook_bullet;

                /* Get an item */
                q = "Sharpen which bullets? ";
                s = "You have no bullets!";
                if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP | USE_FLOOR))) return;

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
                if (o_ptr->art_flags1 & (TR1_SHARPENED))
                {
                        msg_print("These bullets has already been sharpened!");
                }
                else
                {
                msg_print("You sharpen the shot!");
                o_ptr->dd += 1;
                o_ptr->ds += 3;
                o_ptr->art_flags1 |= TR1_SHARPENED;
                /* This is to prevent players to sell these ammos */
                o_ptr->ident |= (IDENT_BROKEN);
                }

}

/* Fighter's Throw! */
void fighter_throw_execute(monster_type *m_ptr)
{
        int tgt_x, tgt_y;
        int rad = 3 + (p_ptr->abilities[(CLASS_FIGHTER * 10) + 7] / 20);
        s32b dam = 0;
        monster_race *r_ptr = &r_info[m_ptr->r_idx]; 
        cave_type *c_ptr;
	call_lua("monk_damages", "", "l", &dam);
	dam += multiply_divide(dam, ((p_ptr->abilities[(CLASS_FIGHTER * 10) + 7] - 1) * 10), 100);
        if (!tgt_pt(&tgt_x,&tgt_y)) return;
        if (cave_empty_bold(tgt_y,tgt_x) && distance(tgt_y, tgt_x, py, px) <= rad)
        {
                if (((p_ptr->abilities[(CLASS_FIGHTER * 10) + 7] * 100)+700) >= r_ptr->weight)
                {
                        c_ptr = &cave[m_ptr->fy][m_ptr->fx];
                        move_monster_spot(c_ptr->m_idx, tgt_x, tgt_y);
                        update_and_handle();
                        msg_print("You throw the monster!");
                        nevermiss = TRUE;
                        corpse_explode(dam, tgt_x, tgt_y, 0, GF_PHYSICAL);
                        nevermiss = FALSE;
                }
                else msg_print("The monster is too heavy to throw!");
        }
        else msg_print("You cannot throw the monster there.");
}

/* Conjure an item */
/* Can also conjure ammos, and you can specify the quantity. */
void conjure_item_any(int itemtval, int itemsval, int duration, int quantity, bool magic, bool special)
{
        object_type     forge;
        object_type     *q_ptr;
        object_type     *o_ptr;
        int slot, whichslot;
	char ch;
	bool nodrop = FALSE;

        if (itemtval == TV_WEAPON || itemtval == TV_ROD || itemtval == TV_SHIELD || itemtval == TV_RANGED)
        {
		/*get_com("Summon in which slot? (1 or 2) ", &ch);
		if (ch == '1') slot = INVEN_WIELD;
		else slot = INVEN_WIELD+1;*/
		slot = INVEN_WIELD;
        }
        else if (itemtval == TV_SOFT_ARMOR || itemtval == TV_HARD_ARMOR || itemtval == TV_DRAG_ARMOR)
        {
                slot = INVEN_BODY;
        }
        else if (itemtval == TV_ARM_BAND)
        {
                slot = INVEN_ARM;
        }
        else if (itemtval == TV_HELM || itemtval == TV_CROWN)
        {
                slot = INVEN_HEAD;
        }
        else if (itemtval == TV_GLOVES)
        {
                slot = INVEN_HANDS;
        }
        else if (itemtval == TV_BOOTS)
        {
                slot = INVEN_FEET;
        }
        else if (itemtval == TV_CLOAK)
        {
                slot = INVEN_OUTER;
        }
	else if (itemtval == TV_AMMO)
        {
                slot = INVEN_AMMO;
        }
	else if (itemtval == TV_LITE)
        {
                slot = INVEN_LITE;
        }
        else
        {
                msg_print("You cannot conjure this item.");
                return;
        }
        
	/*if (slot == INVEN_WIELD || slot == INVEN_WIELD+1)
	{
		if (one_weapon_wield() || two_weapon_wield())
		{
			u32b f1, f2, f3, f4;
			object_type *j_ptr;
			object_prep(j_ptr, lookup_kind(itemtval, itemsval));	
			object_flags(j_ptr, &f1, &f2, &f3, &f4);
			if (f4 & (TR4_MUST2H))
			{
				if (two_weapon_wield())
				{
					inven_takeoff(INVEN_WIELD, 255, FALSE);
					inven_takeoff(INVEN_WIELD+1, 255, FALSE);
					nodrop = TRUE;
				}
				else
				{
					if (inventory[INVEN_WIELD].k_idx) inven_takeoff(INVEN_WIELD, 255, FALSE);
					else if (inventory[INVEN_WIELD+1].k_idx) inven_takeoff(INVEN_WIELD+1, 255, FALSE);
					nodrop = TRUE;
				}
				slot = INVEN_WIELD;
			}
		}
	}*/
	o_ptr = &inventory[slot];

        if (o_ptr->timeout > 0)
        {
                msg_print("You can only summon one item of this type at time!");
                return;
        }
        if (o_ptr->tval > 0) inven_takeoff(slot, 255, FALSE);
	if (slot == INVEN_WIELD)
	{
		object_type *oo_ptr = &inventory[INVEN_WIELD+1];
		if (oo_ptr->tval > 0) inven_takeoff(INVEN_WIELD+1, 255, FALSE);
	}

        /* Get local object */
        q_ptr = &forge; 
        
        object_prep(q_ptr, lookup_kind(itemtval, itemsval));
        q_ptr->number = quantity;
        object_aware(q_ptr);
        object_known(q_ptr);

        if (magic && special) apply_magic(q_ptr, p_ptr->lev, TRUE, TRUE, TRUE, TRUE);
        else if (magic) apply_magic(q_ptr, p_ptr->lev, TRUE, TRUE, TRUE, FALSE);
        q_ptr->timeout = duration;
        object_aware(q_ptr);
        object_known(q_ptr);
        q_ptr->ident |= (IDENT_MENTAL);

        msg_print("You conjure an item!");
        object_copy(&inventory[slot], q_ptr);
        total_weight += q_ptr->weight;
        inven_item_describe(slot);

	o_ptr = &inventory[slot];
	/* Possibly call an item event. */
	if (o_ptr->event_summon != 0)
	{
		call_lua("item_summon", "(Od)", "", o_ptr, o_ptr->event_summon);
	}
        update_and_handle();
}

/* Make an object Levelable */
bool make_item_levelable()
{
	int item;
        object_type             *o_ptr;
        cptr q, s;
	u32b f1, f2, f3, f4;

	item_tester_hook = item_tester_hook_levelable;

        /* Get an item */
        q = "Which item? ";
        s = "You have no items!";
        if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP | USE_FLOOR))) return (FALSE);

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
	object_flags(o_ptr, &f1, &f2, &f3, &f4);

	if (f4 & (TR4_LEVELS))
	{
		msg_print("This item can already gain levels.");
		return (FALSE);
	}

	if ((f4 & (TR4_CRAFTED)) || o_ptr->tweakpoints > 0)
	{
		msg_print("You cannot use this on enchanted crafted items.");
		return (FALSE);
	}

	if (o_ptr->tval == TV_ESSENCE)
	{
		msg_print("You cannot use this on Essences.");
		return (FALSE);
	}

        o_ptr->art_flags4 |= TR4_LEVELS;
	o_ptr->tweakpoints = 2;
	o_ptr->kills = 0;
	o_ptr->level = 1;
	if (f4 & (TR4_MUST2H)) o_ptr->tweakpoints = o_ptr->tweakpoints * 2;
        msg_print("This object can now gain levels!");

	return (TRUE);
}

/* Choose a status effect. Returns FALSE if aborted. */
bool choose_command_element_status()
{
	int                     Power = -1;
        int                     num = 0, dir = 0 , i, j;

	int             powers[36];
	char            power_desc[36][80];

	bool            flag, redraw;
        int             ask;
        s32b dam;

        char            choice, ch;

	char            out_val[160];
        int k,count;

        /* List the choices */
	strcpy(power_desc[num], "Paralyze");
	powers[num++]=0;
	strcpy(power_desc[num], "Dominate");
	powers[num++]=1;
	strcpy(power_desc[num], "Sleep");
	powers[num++]=2;
	strcpy(power_desc[num], "Confuse");
	powers[num++]=3;
	strcpy(power_desc[num], "Fear");
	powers[num++]=4;
	
	/* Should not be called. Ever. */
        if(!num) {msg_print("No effects available.");return FALSE;}

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	/* Build a prompt (accept all spells) */
	if (num <= 26)
	{
		/* Build a prompt (accept all spells) */
                strnfmt(out_val, 78, "(Effects %c-%c, *=List, ESC=exit) Command which effect? ",
			I2A(0), I2A(num - 1));
	}
	else
	{
                strnfmt(out_val, 78, "(Effects %c-%c, *=List, ESC=exit) Command which effect? ",
			I2A(0), '0' + num - 27);
	}

	/* Get a spell from the user */
	while (!flag && get_com(out_val, &choice))
	{
		/* Request redraw */
		if ((choice == ' ') || (choice == '*') || (choice == '?'))
		{
			/* Show the list */
			if (!redraw)
			{
				byte y = 1, x = 0;
				int ctr = 0;
				char dummy[80];

				strcpy(dummy, "");

				/* Show list */
				redraw = TRUE;

				/* Save the screen */
				Term_save();

				prt ("", y++, x);

				while (ctr < num && ctr < 17)
				{
					sprintf(dummy, "%c) %s", I2A(ctr), power_desc[ctr]);
					prt(dummy, y + ctr, x);
					ctr++;
				}
				while (ctr < num)
				{
					if (ctr < 26)
					{
						sprintf(dummy, " %c) %s", I2A(ctr), power_desc[ctr]);
					}
					else
					{
						sprintf(dummy, " %c) %s", '0' + ctr - 26, power_desc[ctr]);
					}
					prt(dummy, y + ctr - 17, x + 40);
					ctr++;
				}
				if (ctr < 17)
				{
					prt ("", y + ctr, x);
				}
				else
				{
					prt ("", y + 17, x);
				}
			}

			/* Hide the list */
			else
			{
				/* Hide list */
				redraw = FALSE;

				/* Restore the screen */
				Term_load();
			}

			/* Redo asking */
			continue;
		}

		if (choice == '\r' && num == 1)
		{
			choice = 'a';
		}

		if (isalpha(choice))
		{
			/* Note verify */
			ask = (isupper(choice));

			/* Lowercase */
			if (ask) choice = tolower(choice);

			/* Extract request */
			i = (islower(choice) ? A2I(choice) : -1);
		}
		else
		{
			ask = FALSE; /* Can't uppercase digits */

			i = choice - '0' + 26;
		}

		/* Totally Illegal */
		if ((i < 0) || (i >= num))
		{
			bell();
			continue;
		}

		/* Save the spell index */
		Power = powers[i];

		/* Verify it */
		if (ask)
		{
			char tmp_val[160];

			/* Prompt */
			strnfmt(tmp_val, 78, "Use %s? ", power_desc[i]);

			/* Belay that order */
			if (!get_check(tmp_val)) continue;
		}

		/* Stop the loop */
		flag = TRUE;
	}

	/* Restore the screen */
	if (redraw) Term_load();

	/* Abort if needed */
	if (!flag) 
	{
		energy_use = 0;
                return FALSE;
	}

	/* Assign the value to the event variable. */
	if (Power == 0) p_ptr->events[29014] = 1;
	if (Power == 1) p_ptr->events[29014] = 2;
	if (Power == 2) p_ptr->events[29014] = 3;
	if (Power == 3) p_ptr->events[29014] = 4;
	if (Power == 4) p_ptr->events[29014] = 5;

	/* Success */
	return TRUE;
}

/* Prepare an Essence. */
/* Based on Soul Bind. */
void prepare_essence(object_type *o_ptr, int r_idx, int level, int mtype)
{
	call_lua("prepare_essence", "Oddd", "", o_ptr, r_idx, level, mtype);
}