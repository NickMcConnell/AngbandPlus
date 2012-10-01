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
/* Note: Valkyrie Spears are NOT counted... */
static bool item_tester_hook_weapon(object_type *o_ptr)
{
        if ((o_ptr->tval == TV_SWORD) || (o_ptr->tval == TV_HAFTED) || (o_ptr->tval == TV_POLEARM) || (o_ptr->tval == TV_ROD) || (o_ptr->tval == TV_DAGGER) || (o_ptr->tval == TV_AXE) || (o_ptr->tval == TV_HELL_STAFF) || (o_ptr->tval == TV_ZELAR_WEAPON)) return (TRUE);

	/* Assume not */
	return (FALSE);
}

/* Check if the weapon is a polearm */
static bool item_tester_hook_weapon_polearm(object_type *o_ptr)
{
        if ((o_ptr->tval == TV_POLEARM) || (o_ptr->tval == TV_VALKYRIE_SPEAR)) return (TRUE);

	/* Assume not */
	return (FALSE);
}

/* Check if the weapon is a dagger */
static bool item_tester_hook_weapon_dagger(object_type *o_ptr)
{
        if (o_ptr->tval == TV_DAGGER) return (TRUE);

	/* Assume not */
	return (FALSE);
}


/* Check if an ammo */
static bool item_tester_hook_ammo(object_type *o_ptr)
{
        if ((o_ptr->tval == TV_ARROW) || (o_ptr->tval == TV_BOLT) || (o_ptr->tval == TV_SHOT)) return (TRUE);

	/* Assume not */
	return (FALSE);
}

static bool effect_books(object_type *o_ptr)
{
        if (o_ptr->tval == TV_BOOK_ELEMENTAL || o_ptr->tval == TV_BOOK_ALTERATION || o_ptr->tval == TV_BOOK_HEALING || o_ptr->tval == TV_BOOK_CONJURATION || o_ptr->tval == TV_BOOK_DIVINATION) return (TRUE);

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
        if ((o_ptr->tval == TV_SWORD) || (o_ptr->tval == TV_HAFTED) || (o_ptr->tval == TV_POLEARM) || (o_ptr->tval == TV_ROD) || (o_ptr->tval == TV_DAGGER) || (o_ptr->tval == TV_AXE) || (o_ptr->tval == TV_HELL_STAFF) || (o_ptr->tval == TV_ZELAR_WEAPON) || (o_ptr->tval == TV_ARROW) || (o_ptr->tval == TV_BOLT) || (o_ptr->tval == TV_SHOT) || (o_ptr->tval == TV_GLOVES)) return (TRUE);

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
                                msg_print("You learned the effect of War Blessing!");
                                p_ptr->alteration_effects |= ALTER_WAR_BLESSING;
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
                                msg_print("You learned the effect of Blessing!");
                                p_ptr->alteration_effects |= ALTER_BLESSING;
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
        if (o_ptr->tval == TV_BOOK_HEALING)
        {
                switch(o_ptr->sval)
                {
                        case 0:
                                msg_print("You learned the effect of Heal!");
                                p_ptr->healing_effects |= HEAL_HEAL;
                                break;
                        case 1:
                                msg_print("You learned the effect of Restore Stats!");
                                p_ptr->healing_effects |= HEAL_RESTORE_STATS;
                                break;
                        case 2:
                                msg_print("You learned the effect of Restore Status!");
                                p_ptr->healing_effects |= HEAL_RESTORE_STATUS;
                                break;
                        case 3:
                                msg_print("You learned the effect of Cure Bleeding!");
                                p_ptr->healing_effects |= HEAL_CURE_BLEEDING;
                                break;                        
                        case 5:
                                msg_print("You learned the effect of Heal Others!");
                                p_ptr->healing_effects |= HEAL_HEAL_OTHERS;
                                break;
                        case 6:
                                msg_print("You learned the effect of Revive Monster!");
                                p_ptr->healing_effects |= HEAL_REVIVE_MONSTER;
                                break;
                        case 7:
                                msg_print("You learned the effect of Restore Mana!");
                                p_ptr->healing_effects |= HEAL_RESTORE_MANA;
                                break;
                        case 8:
                                msg_print("You learned the effect of Satisfy Hunger!");
                                p_ptr->healing_effects |= HEAL_SATISFY_HUNGER;
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
                                msg_print("You learned the effect of Detect Traps!");
                                p_ptr->divination_effects |= DIVI_DETECT_TRAPS;
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
        int rlev = ((r_ptr->level >= 1) ? r_ptr->level : 1);
        int x=px,y=py,k,count;
	int rad;
	s32b dam;
        s32b brdam = p_ptr->chp;
        s32b dieroll = p_ptr->lev / 2;

	int spellstat;

	spellstat = (p_ptr->stat_ind[A_INT] - 5);
	if (spellstat < 0) spellstat = 0;

        brdam = brdam + ((brdam * (p_ptr->abilities[(CLASS_MONSTER_MAGE * 10) + 1] * 5)) / 100);
        brdam = brdam + ((brdam * (p_ptr->abilities[(CLASS_APPRENTICE * 10) + 2] * 10)) / 100);
        dieroll = dieroll + ((dieroll * (p_ptr->abilities[(CLASS_MONSTER_MAGE * 10) + 1] * 5)) / 100);
        dieroll = dieroll + ((dieroll * (p_ptr->abilities[(CLASS_APPRENTICE * 10) + 2] * 10)) / 100);

        /* List the powers */
	i = 0;
	while (i < 20 && r_ptr->spell[i].type > 0) 
	{
		if (r_ptr->spell[i].type == 1)
		{
			sprintf(powdesc, "%s  Type: Bolt, %s  Dam: %d/level  Cost: %d", r_ptr->spell[i].name, get_element_name(r_ptr->spell[i].special1), r_ptr->spell[i].power, r_ptr->spell[i].cost);
			strcpy(power_desc[num],powdesc);
			powers[num++]=i;
		}
		else if (r_ptr->spell[i].type == 2)
		{
			sprintf(powdesc, "%s  Type: Ball, %s  Dam: %d/level  Rad: %d  Cost: %d", r_ptr->spell[i].name, get_element_name(r_ptr->spell[i].special1), r_ptr->spell[i].power, r_ptr->spell[i].special2, r_ptr->spell[i].cost);
			strcpy(power_desc[num],powdesc);
			powers[num++]=i;
		}
		else if (r_ptr->spell[i].type == 3)
		{
			sprintf(powdesc, "%s  Type: Healing  Pow: %d/level  Cost: %d", r_ptr->spell[i].name, r_ptr->spell[i].power, r_ptr->spell[i].cost);
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
			sprintf(powdesc, "%s   Type: Summon(%s)  Pow: %d  Num: %d   Dur: %d   Cost: %d", r_ptr->spell[i].name, mon_random_description(r_ptr->spell[i].summchar), r_ptr->spell[i].power, r_ptr->spell[i].special1, r_ptr->spell[i].special2, r_ptr->spell[i].cost);
			strcpy(power_desc[num],powdesc);
			powers[num++]=i;
		}
		else if (r_ptr->spell[i].type == 7)
		{
			monster_race *rr_ptr = &r_info[r_ptr->spell[i].power];
			cptr m_name = (r_name + rr_ptr->name);
			sprintf(powdesc, "%s   Type: Summon(%s)  Num: %d   Dur: %d  Cost: %d", r_ptr->spell[i].name, m_name, r_ptr->spell[i].special1, r_ptr->spell[i].special2, r_ptr->spell[i].cost);
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

	/* Actually use the power! */
	switch (r_ptr->spell[Power].type)
	{
		/* Bolt */
		case 1:
		{
			dam = r_ptr->spell[Power].power * spellstat;
			dam += ((dam * (p_ptr->abilities[(CLASS_MONSTER_MAGE * 10) + 1] * 20)) / 100);
			dam += ((dam * (p_ptr->abilities[(CLASS_APPRENTICE * 10) + 2] * 20)) / 100);
			if(!get_aim_dir(&dir)) return;
			fire_bolt(r_ptr->spell[Power].special1, dir, dam);
			break;
		}
		/* Ball */
		case 2:
		{
			dam = r_ptr->spell[Power].power * spellstat;
			dam += ((dam * (p_ptr->abilities[(CLASS_MONSTER_MAGE * 10) + 1] * 20)) / 100);
			dam += ((dam * (p_ptr->abilities[(CLASS_APPRENTICE * 10) + 2] * 20)) / 100);
			rad = r_ptr->spell[Power].special2;
			if(!get_aim_dir(&dir)) return;
			fire_ball(r_ptr->spell[Power].special1, dir, dam, rad);
			break;
		}
		/* Heal */
		case 3:
		{
			dam = r_ptr->spell[Power].power * spellstat;
			dam += ((dam * (p_ptr->abilities[(CLASS_MONSTER_MAGE * 10) + 1] * 20)) / 100);
			dam += ((dam * (p_ptr->abilities[(CLASS_APPRENTICE * 10) + 2] * 20)) / 100);
			p_ptr->chp += dam;
			if (p_ptr->chp > p_ptr->mhp) p_ptr->chp = p_ptr->mhp;
			msg_print("You are healed!");
			update_and_handle();
			break;
		}
		/* Haste */
		case 4:
		{
			dam = r_ptr->spell[Power].power;
			(void)set_fast(dam);
			update_and_handle();
			break;
		}
		/* Boost */
		case 5:
		{
			dam = r_ptr->spell[Power].power;
			if (r_ptr->spell[Power].special1 == 1)
			{
				p_ptr->str_boost = r_ptr->spell[Power].power;
                                (void)set_str_boost(20); 
			}
			if (r_ptr->spell[Power].special1 == 2)
			{
				p_ptr->dex_boost = r_ptr->spell[Power].power;
                                (void)set_dex_boost(20); 
			}
			if (r_ptr->spell[Power].special1 == 3)
			{
				p_ptr->int_boost = r_ptr->spell[Power].power;
                                (void)set_int_boost(20); 
			}
			if ((r_ptr->spell[Power].special1 == 6) || (r_ptr->spell[Power].special1 == 8)) 
			{
				p_ptr->str_boost = r_ptr->spell[Power].power;
				p_ptr->dex_boost = r_ptr->spell[Power].power;
				p_ptr->int_boost = r_ptr->spell[Power].power;
				(void)set_str_boost(20);
				(void)set_dex_boost(20);
				(void)set_int_boost(20);
			}
			update_and_handle();
			break;
		}

		/* Summon Kind */
		case 6:
		{
			for (j = 0; j < r_ptr->spell[Power].special1; j++)
			{
				summon_specific_kind(py, px, r_ptr->spell[Power].power, r_ptr->spell[Power].summchar, FALSE, TRUE, r_ptr->spell[Power].special2);
			}
			break;
		}

		/* Summon Specific */
		case 7:
		{
			for (j = 0; j < r_ptr->spell[Power].special1; j++)
			{
				summon_specific_ridx(py, px, r_ptr->spell[Power].power, FALSE, TRUE, r_ptr->spell[Power].special2);
			}
			break;
		}
		/* Phase door */
		case 8:
		{
			teleport_player(r_ptr->spell[Power].power);
			break;
		}
		
		default:
		{
			break;
		}
	}	

	/* Remove some mana */
	p_ptr->csp -= r_ptr->spell[Power].cost;

	update_and_handle();

        energy_use = 100;
        return num;
}

void do_cmd_lite_charm()
{
        int     dir;
        if (!get_aim_dir(&dir)) return;
        fire_bolt(GF_LITE_CONTROL, dir, 0);
        energy_use = 100;
        update_and_handle();
}

void do_cmd_bless_weapon(void)
{
                int item;
                object_type             *o_ptr;
                cptr q, s;
                u32b f1, f2, f3, f4;
                /* Get an item */
                q = "Bless which item? ";
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
                        msg_print("This item is already enchanted!");
                }
                else
                {
                msg_print("Your item glow with godly power!");
                apply_magic(o_ptr, p_ptr->lev, TRUE, TRUE, TRUE, FALSE);
                o_ptr->art_flags4 |= TR4_INDESTRUCTIBLE;
                identify_fully_aux(o_ptr);
                object_aware(o_ptr);
                object_known(o_ptr);
                o_ptr->ident |= (IDENT_MENTAL);
                /* This is to prevent players to sell this bow */
                o_ptr->ident |= (IDENT_BROKEN);
                }

                energy_use = 100;
}

void do_cmd_divine_bolt()
{
        if (p_ptr->csp >= 15)
        {
        int     dir, beam;
        int     plev = p_ptr->lev;
        int to_s2=p_ptr->to_s/2;
        int mto_s2=p_ptr->to_s/2;

        mto_s2 = (mto_s2==0)?1:mto_s2;

        beam = plev / 2;
        if (!get_aim_dir(&dir)) return;
        fire_bolt(GF_LITE, dir, 30 * p_ptr->lev * 5);
        p_ptr->csp -= 15;
        energy_use = 100;
        update_and_handle();
        }
        else msg_print("You do not have enough mana!");
}

void wave_kick()
{
               int dir;
               if (!get_aim_dir(&dir)) return;
               fire_bolt_or_beam(p_ptr->lev, GF_MANA, dir, p_ptr->to_d * 10);
               p_ptr->chp = p_ptr->chp / 2;
               update_and_handle();
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
                if (f4 & (TR4_CHARGEABLE))
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
                                
                                do_cmd_tweak(o_ptr);
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

void repair_weapon()
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
                if (o_ptr->pval3 < 20 && (o_ptr->tval == TV_SWORD || o_ptr->tval == TV_HAFTED || o_ptr->tval == TV_POLEARM
                 || o_ptr->tval == TV_MSTAFF || o_ptr->tval == TV_ROD || o_ptr->tval == TV_DAGGER || o_ptr->tval == TV_AXE || o_ptr->tval == TV_SWORD_DEVASTATION
                 || o_ptr->tval == TV_HELL_STAFF || o_ptr->tval == TV_VALKYRIE_SPEAR || o_ptr->tval == TV_ZELAR_WEAPON))
                {
                        msg_print("The weapon is repaired!");
                        o_ptr->pval3 = 20;
                }
                else msg_print("Either this is not a weapon, or it's durability is higher than 20!");
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

/* Let's give magic barriers to Hell Queens! */
/* Maybe some other classes/items will be able to use this. */
/* Note: Barrier cages were removed, but the code is still there... */
/* It might come back someday, but maybe not... */
void barrier_master()
{
	char ch = 0;
	int amber_power = 0;
	int dir = 0;
      
	  while (TRUE)
			{
                        if (!get_com("[B]arrier, [S]hield ", &ch))
				{
					amber_power = 0;
					break;
				}

                                if (ch == 'B' || ch == 'b')
				{
					amber_power = 1;
					break;
				}

                                if (ch == 'S' || ch == 's')
				{
					amber_power = 2;
					break;
				}

			}


        if (amber_power == 2)
        {
        int i,j;
        byte wrad = 1;

        msg_print("You surround yourself with barriers!");
        for(j = py - wrad; j < py + wrad + 1; j++)
        for(i = px - wrad; i < px + wrad + 1; i++)
        if((distance(py, px, j, i) <= wrad) && in_bounds(j,i))
        {
               
                 cave_set_feat(j, i, 188);
               
        }
        for(j = py; j < py + 1; j++)
        for(i = px; i < px + 1; i++)
        if((distance(py, px, j, i) <= wrad) && in_bounds(j,i))
        {
               
                 cave_set_feat(j, i, 224);
               
        }



        }

        if (amber_power == 1)
        {
        int i,j;
        msg_print("Place where? ");
        if (!tgt_pt(&i, &j)) return;
        /* cave_set_feat(j, i, 188);*/
        place_field(188, 0, i, j, 0);
        msg_print("You created a powerful magic barrier!");
        }

        if (amber_power == 3)
        {
        int i,j, x, y;
        byte wrad = 1;
        msg_print("Where do you want to create the cage?");
        if (!tgt_pt(&x, &y)) return;
        msg_print("You create a magic barrier cage!");
        for(j = y - wrad; j < y + wrad + 1; j++)
        for(i = x - wrad; i < x + wrad + 1; i++)
        cave_set_feat(j, i, 188);
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
                msg_print("You put poison on your weapon!");
                o_ptr->brandtype = GF_POIS;
		o_ptr->branddam = 50 * p_ptr->abilities[(CLASS_ROGUE * 10) + 5];
                /* This is to prevent players to sell this weapon */
                o_ptr->ident |= (IDENT_BROKEN);
}

/* Don't know what to do with your swords? */
void assassin_trap_weapon()
{
        int x, y, explosiontype;
        s32b explosionpower;
        u32b f1, f2, f3, f4;
        cave_type *c_ptr;
        object_type *o_ptr;
        msg_print("Point the location of the weapon...");
        if (!tgt_pt(&x, &y)) return;
        c_ptr = &cave[y][x];
        if (!c_ptr->o_idx)
        {
                msg_print("Nothing here!");
                return;
        }
        o_ptr = &o_list[c_ptr->o_idx];
        if (!item_tester_hook_weapon(o_ptr))
        {
                msg_print("This item is not a weapon!");
                return;
        }
        else
        {
                if (o_ptr->ident & (IDENT_BROKEN))
                {
                        msg_print("You cannot use this weapon.");
                        return;
                }
                /* Roll the damages... */
                object_flags(o_ptr, &f1, &f2, &f3, &f4);
                explosionpower = (o_ptr->dd * o_ptr->ds);
                explosionpower *= p_ptr->lev;
                explosionpower *= 3;
                explosionpower += o_ptr->to_d;
                if (o_ptr->pval >= 3) explosionpower *= o_ptr->pval / 3;
                if (f1 & (TR1_BRAND_FIRE)) explosiontype = GF_FIRE;
                else if (f1 & (TR1_BRAND_COLD)) explosiontype = GF_COLD;
                else if (f1 & (TR1_BRAND_ELEC)) explosiontype = GF_ELEC;
                else if (f1 & (TR1_BRAND_ACID)) explosiontype = GF_ACID;
                else if (f1 & (TR1_BRAND_POIS)) explosiontype = GF_POIS;
                else if (f4 & (TR4_BRAND_DARK)) explosiontype = GF_DARK;
                else if (f4 & (TR4_BRAND_LIGHT)) explosiontype = GF_LITE;
                else explosiontype = GF_MANA;
                if (f1 & (TR1_BRAND_FIRE)) explosionpower *= 3;
                else if (f1 & (TR1_BRAND_COLD)) explosionpower *= 3;
                else if (f1 & (TR1_BRAND_ELEC)) explosionpower *= 3;
                else if (f1 & (TR1_BRAND_ACID)) explosionpower *= 3;
                else if (f1 & (TR1_BRAND_POIS)) explosionpower *= 3;
                else if (f4 & (TR4_BRAND_DARK)) explosionpower *= 3;
                else if (f4 & (TR4_BRAND_LIGHT)) explosionpower *= 3;
                else if (f4 & (TR4_BRAND_MAGIC)) explosionpower *= 4;

                corpse_explode(explosionpower, x, y, 3, explosiontype);
                msg_print("The weapon explode!");
                delete_object_idx(c_ptr->o_idx);
                update_and_handle();
        }
}

/* Make your enemies sleep and assassinate them! */
void assassin_sleep_dart()
{
        int dir;
        if (!get_aim_dir(&dir)) return;
        (void)fire_bolt(GF_OLD_SLEEP, dir, 1);
}        

/* May be used by any function... */
void place_field(int ftype, byte rad, int x, int y, s32b dam)
{
        int i,j,w;
        cave_type *c_ptr;

        dam += (dam * (p_ptr->skill[1] * 10)) / 100;
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

/* Updated for the Elemental Lord in NewAngband 1.7.0! :) */
void explosive_throw()
{
                int item, explosiontype, dir;
                s32b explosionpower;
                object_type             *o_ptr;
                cptr q, s;
                u32b f1, f2, f3, f4;
		int spellstat;

		spellstat = (p_ptr->stat_ind[A_INT] - 5);
		if (spellstat < 0) spellstat = 0;

                /* Restrict choices to weapons */
                item_tester_hook = item_tester_hook_weapon;

                /* Get an item */
                q = "Throw which weapon? ";
                s = "You have no weapons!";
                if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP))) return;

                /* Get the item */
                o_ptr = &inventory[item];

                object_flags(o_ptr, &f1, &f2, &f3, &f4);
                if (o_ptr->ident & (IDENT_BROKEN))
                {
                        msg_print("You cannot use this weapon.");
                        return;
                }
                explosionpower = (o_ptr->dd * o_ptr->ds);
                explosionpower *= spellstat;
                explosionpower += ((explosionpower * p_ptr->dis_to_d) / 100);
		explosionpower += ((explosionpower * p_ptr->stat_ind[A_STR]) / 100);
                explosionpower *= o_ptr->pval;
		explosionpower += ((explosionpower * ((p_ptr->abilities[(CLASS_ELEM_LORD * 10) + 6] - 1) * 50)) / 100);

                explosiontype = p_ptr->elemlord;

                msg_print("You throw your exploding weapon!");
                if (!get_aim_dir(&dir)) return;
                fire_ball(explosiontype, dir, explosionpower, 3 + (p_ptr->abilities[(CLASS_ELEM_LORD * 10) + 6] / 20));
                inven_item_increase(item, -1);
                inven_item_describe(item);
                inven_item_optimize(item);
                update_and_handle();
}

void do_cmd_tweak(object_type *o_ptr)
{
        int tweakchoice = 0;
        char ch = 0;
        if (!get_com("[I]increase pval, [A]dd new ability?", &ch)) return;
        if (ch == 'I' || ch == 'i')
        {
                int pricemod = o_ptr->pval * 10000;
                int finalprice = 0;

                finalprice = pricemod + (10000 * o_ptr->pval);
                if (o_ptr->pval >= 10)
                {
                        msg_print("You cannot raise pval beyond 10 this way!");
                        return;
                }
                msg_format("It will cost %d gold to increase pval...", finalprice);
                if (!get_com("Increase pval? [Y/N]", &ch)) return;
                if (ch == 'Y' || ch == 'y')
                {
                        if (p_ptr->au >= finalprice)
                        {
                                msg_print("Your item glows with a blinding light!");
                                o_ptr->pval += 1;
                                p_ptr->au -= finalprice;
                                update_and_handle();
                        }
                        else msg_print("You do not have enough money.");
                }
        }
        if (ch == 'A' || ch == 'a')
        {
                add_item_ability(o_ptr);
        }
}

/* Remove any terrain feature(except perm walls) */
/* The ONLY way to remove magic barriers! */
void dispel_entity()
{
        int i,j;
        cave_type       *c_ptr;
        msg_print("Dispel where? ");
        if (!tgt_pt(&i, &j)) return;
        c_ptr = &cave[j][i];
        if (c_ptr->feat != FEAT_PERM_EXTRA && c_ptr->feat != FEAT_PERM_INNER && c_ptr->feat != FEAT_PERM_OUTER && c_ptr->feat != FEAT_PERM_SOLID && c_ptr->feat != 224)
        {
                cave_set_feat(j, i, FEAT_FLOOR);
                msg_print("You dispel the entity!");
        }
        else msg_print("Cannot dispel permanent walls...");
}

/* A special version of Trap Weapons... */
void sacrifice_weapon()
{
        int x, y, explosiontype;
        s32b explosionpower;
        u32b f1, f2, f3, f4;
        cave_type *c_ptr;
        object_type *o_ptr;
        msg_print("Point the location of the weapon...");
        if (!tgt_pt(&x, &y)) return;
        c_ptr = &cave[y][x];
        if (!c_ptr->o_idx)
        {
                msg_print("Nothing here!");
                return;
        }
        o_ptr = &o_list[c_ptr->o_idx];
        if (!item_tester_hook_weapon(o_ptr))
        {
                msg_print("This item is not a weapon!");
                return;
        }
        else
        {
                if (o_ptr->ident & (IDENT_BROKEN))
                {
                        msg_print("You cannot use this weapon.");
                        return;
                }                
                /* Roll the damages... */
                object_flags(o_ptr, &f1, &f2, &f3, &f4);
                explosionpower = (o_ptr->dd * o_ptr->ds);
                explosionpower *= p_ptr->lev;
                explosionpower += ((explosionpower * p_ptr->dis_to_d) / 100);
		explosionpower += ((explosionpower * p_ptr->stat_ind[A_STR]) / 100);
                explosionpower += o_ptr->to_d;
                if (o_ptr->pval >= 3) explosionpower *= o_ptr->pval / 3;
                if (f1 & (TR1_BRAND_FIRE)) explosiontype = GF_FIRE;
                else if (f1 & (TR1_BRAND_COLD)) explosiontype = GF_COLD;
                else if (f1 & (TR1_BRAND_ELEC)) explosiontype = GF_ELEC;
                else if (f1 & (TR1_BRAND_ACID)) explosiontype = GF_ACID;
                else if (f1 & (TR1_BRAND_POIS)) explosiontype = GF_POIS;
                else if (f4 & (TR4_BRAND_DARK)) explosiontype = GF_DARK;
                else if (f4 & (TR4_BRAND_LIGHT)) explosiontype = GF_LITE;
                else explosiontype = GF_MANA;
                if (f1 & (TR1_BRAND_FIRE)) explosionpower *= 3;
                else if (f1 & (TR1_BRAND_COLD)) explosionpower *= 3;
                else if (f1 & (TR1_BRAND_ELEC)) explosionpower *= 3;
                else if (f1 & (TR1_BRAND_ACID)) explosionpower *= 3;
                else if (f1 & (TR1_BRAND_POIS)) explosionpower *= 3;
                else if (f4 & (TR4_BRAND_DARK)) explosionpower *= 3;
                else if (f4 & (TR4_BRAND_LIGHT)) explosionpower *= 3;
                else if (f4 & (TR4_BRAND_MAGIC)) explosionpower *= 4;

                corpse_explode(explosionpower, x, y, 3, explosiontype);
                place_field(FEAT_FIRE_FIELD, 3, x, y, explosionpower);
                msg_print("The weapon explode in a fiery blaze!");
                delete_object_idx(c_ptr->o_idx);
                update_and_handle();
        }
}

/* A weak, radius 0 Cold Field, but it cost no mana at all. */
void ice_lord_frost()
{
        int fieldtype = 0;
        int ii, ij;
        msg_print("Create where? ");
        if (!tgt_pt(&ii,&ij)) return;
        place_field(FEAT_COLD_FIELD, 0, ii, ij, (p_ptr->lev * 10));
        msg_print("You create an Ice Field!");
        p_ptr->chp -= (p_ptr->chp / 5);
        update_and_handle();
}

/* Somewhat similar to the Assassin's "Trap Weapon" ability... */
void ice_shatter()
{
        int x, y;
        s32b explosionpower;
        u32b f1, f2, f3, f4;
        cave_type *c_ptr;
        object_type *o_ptr;
        msg_print("Point the location of the weapon...");
        if (!tgt_pt(&x, &y)) return;
        c_ptr = &cave[y][x];
        if (!c_ptr->o_idx)
        {
                msg_print("Nothing here!");
                return;
        }
        o_ptr = &o_list[c_ptr->o_idx];
        if (!item_tester_hook_weapon(o_ptr))
        {
                msg_print("This item is not a weapon!");
                return;
        }
        else
        {
                if (o_ptr->ident & (IDENT_BROKEN))
                {
                        msg_print("You cannot use this weapon.");
                        return;
                }                
                /* Roll the damages... */
                object_flags(o_ptr, &f1, &f2, &f3, &f4);
                explosionpower = (o_ptr->dd * o_ptr->ds);
                explosionpower *= p_ptr->lev * 2;
                explosionpower += ((explosionpower * p_ptr->to_d) / 100);
		explosionpower += ((explosionpower * p_ptr->stat_ind[A_STR]) / 100);
                explosionpower += o_ptr->to_d;
                if (o_ptr->pval >= 3) explosionpower *= o_ptr->pval / 3;
                if (f1 & (TR1_BRAND_FIRE)) explosionpower *= 3;
                else if (f1 & (TR1_BRAND_COLD)) explosionpower *= 3;
                else if (f1 & (TR1_BRAND_ELEC)) explosionpower *= 3;
                else if (f1 & (TR1_BRAND_ACID)) explosionpower *= 3;
                else if (f1 & (TR1_BRAND_POIS)) explosionpower *= 3;
                else if (f4 & (TR4_BRAND_DARK)) explosionpower *= 3;
                else if (f4 & (TR4_BRAND_LIGHT)) explosionpower *= 3;
                else if (f4 & (TR4_BRAND_MAGIC)) explosionpower *= 4;
                explosionpower = explosionpower / 4;

                place_field(FEAT_COLD_FIELD, 3, x, y, explosionpower);
                msg_print("The weapon explode into a blizzard!");
                delete_object_idx(c_ptr->o_idx);
                update_and_handle();
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
							if ((r_ptr->level < b_ptr->level) && (!(r_ptr->flags1 & (RF1_UNIQUE))) && !(r_ptr->flags9 & (RF9_SPECIAL_GENE))) {
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
                apply_monster_level_hp(m_ptr);
                lite_spot(y, x);
                update_and_handle();
}

void tree_explosion()
{
        int y,x;
        int flg = PROJECT_GRID | PROJECT_KILL;
        int dam = (p_ptr->lev * 200) * (p_ptr->to_s);
        cave_type *c_ptr;
        monster_type *m_ptr;
        monster_race *r_ptr;

        if (!tgt_pt(&x,&y)) return;
        c_ptr = &cave[y][x];

        if (c_ptr->feat != FEAT_TREES)
        {
                msg_print("You must target a tree!");
                return;
        }
        else
        {
                if (p_ptr->prace == RACE_ENT) dam = dam + (dam / 4);
                (void)project(0, 4, y, x, dam, GF_EARTH, flg);
                c_ptr->feat = FEAT_FLOOR;
                lite_spot(y, x);
                update_and_handle();
        }
}

/* Weapon Bomb... */
void battle_weapon_explode()
{
                int item, explosiontype, dir;
                s32b explosionpower;
                object_type             *o_ptr;
                cptr q, s;

                /* Restrict choices to bows */
                item_tester_hook = item_tester_hook_weapon;

                /* Get an item */
                q = "Use which weapon? ";
                s = "You have no weapons!";
                if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP))) return;

                /* Get the item */
                o_ptr = &inventory[item];

                /* Cannot use self-made items for balance issue */
                if (!(o_ptr->ident & (IDENT_BROKEN)))
                {

                        explosionpower = (o_ptr->pval * 5000) * p_ptr->to_s;

                        msg_print("You turn your weapon into an exploding ball of mana!");
                        if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_MANA, dir, explosionpower, 3);
                        inven_item_increase(item, -1);
                        inven_item_describe(item);
                        inven_item_optimize(item);
                        update_and_handle();
                }
                else msg_print("You cannot use a stolen, broken or self-made item!");
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
        vision_scan_monster(m_ptr);
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
		
        b_ptr = &r_info[m_ptr->r_idx];
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
							if ((r_ptr->level > b_ptr->level) && (r_ptr->level <= m_ptr->level) && (!(r_ptr->flags1 & (RF1_UNIQUE))) && !(r_ptr->flags9 & (RF9_SPECIAL_GENE))) {
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
                apply_monster_level_hp(m_ptr);
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
                /* the game...including Variaz! ;) Good luck though! */
                if (m_ptr->hp <= capchance)
                {
                        int chances;
                        /* Are we trying to capture a friendly monster? */
                        if (is_pet(m_ptr) || m_ptr->angered_pet >= 1)
                        {
                                msg_print("You cannot capture the soul of friendly/angry monsters!");
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
        else msg_print("There's no monster here!");
}

/* Bind a soul to an item */
void soul_bind()
{
        int item, x;
        object_type             *o_ptr;
        object_type             *q_ptr;
        monster_race            *r_ptr;
        cptr q, s;

        /* Get an item */

        item_tester_tval = TV_SOUL;

        q = "Which soul do you want to bind? ";
        s = "You have no souls!";
        if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP))) return;

        /* Get the item (in the pack) */
        if (item >= 0)
        {
                q_ptr = &inventory[item];
        }
        /* Get the soul's monster */
        r_ptr = &r_info[q_ptr->pval];
        inven_item_increase(item, -1);
        inven_item_describe(item);
        inven_item_optimize(item);

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

        /* Enchant the item with the right properties! */
        /* The more abilities the monster has, the more */
        /* powers the item gets! Note that not all powers */
        /* actually makes the item better... */
        if (o_ptr->art_flags1 || o_ptr->art_flags2 || o_ptr->art_flags3 || o_ptr->art_flags4)
        {
                msg_print("This item is already magical.");
                return;
        }
        else if (o_ptr->tval == TV_HYPNOS || o_ptr->tval == TV_LITE || o_ptr->tval == TV_CRYSTAL)
        {
                msg_print("You cannot enchant this item.");
                return;
        }
        else
        {
        /* Valid item, begin enchanting! */
        o_ptr->art_flags1 |= (TR1_ENCHANTED);

	o_ptr->fireres = r_ptr->fireres / 4;
	o_ptr->coldres = r_ptr->coldres / 4; 
	o_ptr->elecres = r_ptr->elecres / 4; 
	o_ptr->acidres = r_ptr->acidres / 4;
	o_ptr->poisres = r_ptr->poisres / 4; 
	o_ptr->lightres = r_ptr->lightres / 4; 
	o_ptr->darkres = r_ptr->darkres / 4; 
	o_ptr->warpres = r_ptr->warpres / 4;
	o_ptr->waterres = r_ptr->waterres / 4; 
	o_ptr->windres = r_ptr->windres / 4; 
	o_ptr->earthres = r_ptr->earthres / 4;
	o_ptr->soundres = r_ptr->soundres / 4; 
	o_ptr->radiores = r_ptr->radiores / 4; 
	o_ptr->chaosres = r_ptr->chaosres / 4; 
	o_ptr->physres = r_ptr->physres / 4; 
	o_ptr->manares = r_ptr->manares / 4;

	o_ptr->to_a = r_ptr->ac / 4;         

        if (r_ptr->flags3 & (RF3_NO_FEAR))
                o_ptr->art_flags2 |= (TR2_RES_FEAR);
        if (r_ptr->flags3 & (RF3_NO_STUN))
                o_ptr->art_flags2 |= (TR2_FREE_ACT);
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
                o_ptr->art_flags2 |= (TR2_INVIS);
        if (r_ptr->flags2 & (RF2_POWERFUL) || r_ptr->flags1 & (RF1_UNIQUE))
                o_ptr->art_flags2 |= (TR2_LIFE);
        if (r_ptr->flags2 & (RF2_SMART) || r_ptr->mind > 100)
                o_ptr->art_flags1 |= (TR1_INT);
        if (r_ptr->flags2 & (RF2_KILL_WALL) || r_ptr->str > 100)
                o_ptr->art_flags1 |= (TR1_STR);
	if (r_ptr->dex > 100)
                o_ptr->art_flags1 |= (TR1_DEX);
	if (r_ptr->mind > 100)
                o_ptr->art_flags1 |= (TR1_WIS);
        if (r_ptr->speed >= 130)
                o_ptr->art_flags1 |= (TR1_SPEED);
	if (r_ptr->attacks > 4)
		o_ptr->art_flags1 |= (TR1_BLOWS);
        if (is_weapon(o_ptr) || o_ptr->tval == TV_ARROW || o_ptr->tval == TV_BOLT || o_ptr->tval == TV_SHOT)
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
					o_ptr->branddam = maxroll(r_ptr->attack[x].ddice, r_ptr->attack[x].dside) * 4;
					o_ptr->brandrad = 0;
				}                                
                        }                                                 

                }
                /* Enchance the damages...maybe */
                if (o_ptr->tval == TV_SWORD || o_ptr->tval == TV_HAFTED || o_ptr->tval == TV_POLEARM ||
                o_ptr->tval == TV_AXE || o_ptr->tval == TV_ZELAR_WEAPON ||
                o_ptr->tval == TV_HELL_STAFF || o_ptr->tval == TV_MSTAFF || o_ptr->tval == TV_SWORD_DEVASTATION ||
                o_ptr->tval == TV_VALKYRIE_SPEAR)
                {
                        o_ptr->dd += r_ptr->attack[0].ddice / 4;
                        o_ptr->ds += r_ptr->attack[0].dside / 4;
                }
                if (o_ptr->tval == TV_ARROW || o_ptr->tval == TV_BOLT || o_ptr->tval == TV_SHOT || o_ptr->tval == TV_ROD)
                {
                        o_ptr->dd += r_ptr->attack[0].ddice / 6;
                        o_ptr->ds += r_ptr->attack[0].dside / 6;
                }                
                if (o_ptr->tval == TV_DAGGER)
                {
                        o_ptr->dd += r_ptr->attack[0].ddice / 10;
                        o_ptr->ds += r_ptr->attack[0].dside / 10;
                }                
        }
        if (o_ptr->tval == TV_SOFT_ARMOR || o_ptr->tval == TV_HARD_ARMOR || o_ptr->tval == TV_DRAG_ARMOR)
        {
                /* Bind a monster's soul to an armor! */
                o_ptr->ac += (r_ptr->ac / 10);
        }
        if (o_ptr->tval == TV_BOOTS || o_ptr->tval == TV_GLOVES || o_ptr->tval == TV_HELM
        || o_ptr->tval == TV_CROWN || o_ptr->tval == TV_SHIELD || o_ptr->tval == TV_CLOAK
        || o_ptr->tval == TV_ARM_BAND)
        {
                /* Bind a monster's soul to a glove, boots, etc...! */
                /* MUCH lower than an armor, but still good. */
                o_ptr->ac += (r_ptr->ac / 20);
        }

        o_ptr->to_h += r_ptr->level;
        o_ptr->to_d += r_ptr->level;
        o_ptr->pval = 1 + (p_ptr->abilities[(CLASS_SOUL_GUARDIAN * 10) + 2] / 3);
        if (o_ptr->pval < 1) o_ptr->pval = 1;
        o_ptr->ident |= (IDENT_BROKEN);
        msg_print("You bind the soul to your item!");
        update_and_handle();
        energy_use = 100;
        }

}

/* Spirit Swords! */
/* The funnest Samurai ability ever! :) */
int spirit_sword(object_type *o_ptr, s32b dam)
{
	int                     Power = -1;
        int                     num = 0, dir = 0 , i;

	int             powers[36];
	char            power_desc[36][80];

	bool            flag, redraw;
        int             ask;

        char            choice;

	char            out_val[160];

        u32b f1, f2, f3, f4;
        object_flags(o_ptr, &f1, &f2, &f3, &f4);


        /* List the powers */
        strcpy(power_desc[num],"Edge Spirit(Ranged physical attack.)");powers[num++]=1;
        if (f1 & (TR1_BRAND_FIRE)) {strcpy(power_desc[num],"Fire Spirit(Ranged fire attack.)");powers[num++]=2;}
        if (f1 & (TR1_BRAND_COLD)) {strcpy(power_desc[num],"Cold Spirit(Ranged cold attack.)");powers[num++]=3;}
        if (f1 & (TR1_BRAND_ELEC)) {strcpy(power_desc[num],"Thunder Spirit(Ranged electrical attack.)");powers[num++]=4;}
        if (f1 & (TR1_BRAND_ACID)) {strcpy(power_desc[num],"Acid Spirit(Ranged acid attack.)");powers[num++]=5;}
        if (f1 & (TR1_BRAND_POIS)) {strcpy(power_desc[num],"Poison Spirit(Ranged poison attack.)");powers[num++]=6;}
        if (f4 & (TR4_BRAND_LIGHT)) {strcpy(power_desc[num],"Light Spirit(Ranged light attack.)");powers[num++]=7;}
        if (f4 & (TR4_BRAND_DARK)) {strcpy(power_desc[num],"Dark Spirit(Ranged darkness attack.)");powers[num++]=8;}
        if (f4 & (TR4_BRAND_MAGIC)) {strcpy(power_desc[num],"Mana Spirit(Ranged magic attack.)");powers[num++]=9;}
        if (f4 & (TR4_LOWER_DEF)) {strcpy(power_desc[num],"Frail Spirit(Ranged 'Frailness' curse.)");powers[num++]=10;}
        if (f4 & (TR4_LOWER_HIT)) {strcpy(power_desc[num],"Inept Spirit(Ranged 'Ineptitude' curse.)");powers[num++]=11;}

        if(!num) {msg_print("You can't use any spirit sword abilites.");return 0;}

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
                        if (get_aim_dir(&dir))
                        fire_bolt(GF_PHYSICAL, dir, dam);
                        break;
                case 2:
                        if (get_aim_dir(&dir))
                        fire_bolt(GF_FIRE, dir, dam);
                        break;
                case 3:
                        if (get_aim_dir(&dir))
                        fire_bolt(GF_COLD, dir, dam);
                        break;
                case 4:
                        if (get_aim_dir(&dir))
                        fire_bolt(GF_ELEC, dir, dam);
                        break;
                case 5:
                        if (get_aim_dir(&dir))
                        fire_bolt(GF_ACID, dir, dam);
                        break;
                case 6:
                        if (get_aim_dir(&dir))
                        fire_bolt(GF_POIS, dir, dam);
                        break;
                case 7:
                        if (get_aim_dir(&dir))
                        fire_bolt(GF_LITE, dir, dam);
                        break;
                case 8:
                        if (get_aim_dir(&dir))
                        fire_bolt(GF_DARK, dir, dam);
                        break;
                case 9:
                        if (get_aim_dir(&dir))
                        fire_bolt(GF_MANA, dir, dam);
                        break;
                case 10:
                        if (get_aim_dir(&dir))
                        fire_bolt(GF_FRAILNESS, dir, 0);
                        break;

                case 11:
                        if (get_aim_dir(&dir))
                        fire_bolt(GF_INEPTITUDE, dir, 0);
                        break;
        }
        return num;
}

/* Return "true" if a grid is "lite" */
bool cave_lit(int y, int x)
{
        cave_type *c_ptr = c_ptr = &cave[y][x];
        if (c_ptr->info & (CAVE_LITE)) return TRUE;

        /* Default */
        return FALSE;
}

void drain_object()
{
                int item, drainamount;
                object_type             *o_ptr;
                cptr q, s;
                u32b f1, f2, f3, f4;

                /* Get an item */
                q = "Drain which item? ";
                s = "You have no items!";
                if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP))) return;

                /* Get the item */
                o_ptr = &inventory[item];

                object_flags(o_ptr, &f1, &f2, &f3, &f4);
                if (o_ptr->ident & (IDENT_BROKEN))
                {
                        msg_print("You cannot drain this item.");
                        return;
                }
                if (o_ptr->pval >= 1)
                {
                        drainamount = get_quantity("Drain how much power?", o_ptr->pval);
                        o_ptr->pval -= drainamount;
                        p_ptr->csp += drainamount * ((p_ptr->abilities[(CLASS_MAGE * 10) + 7] * 20) + 50);
                        if (p_ptr->csp > p_ptr->msp) p_ptr->csp = p_ptr->msp;
                        msg_print("You drain the item...");
                }
                else msg_print("This item doesn't have any power!");
                update_and_handle();
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
        goldamount = (k_ptr->cost * (p_ptr->abilities[(CLASS_MAGE * 10) + 8] * 10)) / 100;
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
        int basehp = 0;
        int hit_bonus = 0;
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
        basehp += weapon_ptr->to_a;
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

/* Priest ability Mace Of Heaven! :) */
void mace_of_heaven()
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
				slot = INVEN_WIELD;
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
        
        object_prep(q_ptr, lookup_kind(TV_HAFTED, 24));
        q_ptr->number = 1;
        object_aware(q_ptr);
        object_known(q_ptr);

        /* Store the monster's info in the soul! */
        q_ptr->dd += (p_ptr->abilities[(CLASS_PRIEST * 10) + 3] / 4);
        q_ptr->ds += (p_ptr->abilities[(CLASS_PRIEST * 10) + 3] / 4);
        q_ptr->to_h = p_ptr->lev;
        q_ptr->to_d = p_ptr->lev;
        q_ptr->timeout = 10 + ((p_ptr->abilities[(CLASS_PRIEST * 10) + 3] - 1) * 2);
	q_ptr->brandtype = GF_LITE;
	q_ptr->branddam = p_ptr->abilities[(CLASS_PRIEST * 10) + 3] * 50;
	q_ptr->brandrad = 0;        

        msg_print("A glowing, holy mace appear in your hand!");
        object_copy(&inventory[slot], q_ptr);
        total_weight += q_ptr->weight;
        update_and_handle();
}

/* Spike Trap! */
void set_spike_trap()
{
        int item;
        s32b trapdam;
        object_type             *o_ptr;
        cptr q, s;
	int spellstat;

	spellstat = (p_ptr->stat_ind[A_DEX] - 5);
	if (spellstat < 0) spellstat = 0;

        /* Restrict choices to weapons */
        item_tester_hook = item_tester_hook_weapon_polearm;

        /* Get an item */
        q = "Use which polearm? ";
        s = "You have no polearms!";
        if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP))) return;

        /* Get the item */
        o_ptr = &inventory[item];

        trapdam = ((maxroll(o_ptr->dd, o_ptr->ds) * 10) * p_ptr->abilities[(CLASS_ROGUE * 10) + 8]) * spellstat;

        place_field(FEAT_SPIKE_TRAP, 0, px, py, trapdam);
        msg_print("You set a Spike Trap!");

        inven_item_increase(item, -1);
        inven_item_describe(item);
        inven_item_optimize(item);
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

/* Gas Trap! */
void set_gas_trap()
{
        if (p_ptr->au >= 500)
        {
                place_field(FEAT_GAS_TRAP, 0, px, py, 0);
                msg_print("You set a Gas Trap!");
                p_ptr->au -= 500;
                update_and_handle();
        }
        else msg_print("You need at least 500 golds!");
}

/* Poison Trap! */
void set_poison_trap()
{
        int item;
        s32b trapdam;
        object_type             *o_ptr;
        cptr q, s;
        u32b f1, f2, f3, f4;
	int spellstat;

	spellstat = (p_ptr->stat_ind[A_DEX] - 5);
	if (spellstat < 0) spellstat = 0;

        /* Restrict choices to weapons */
        item_tester_hook = item_tester_hook_weapon;

        /* Get an item */
        q = "Use which weapon? ";
        s = "You have no weapons!";
        if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP))) return;

        /* Get the item */
        o_ptr = &inventory[item];

        object_flags(o_ptr, &f1, &f2, &f3, &f4);

        if (o_ptr->brandtype == GF_POIS)
        {
		trapdam = o_ptr->branddam;
		trapdam += ((trapdam * (p_ptr->abilities[(CLASS_ROGUE * 10) + 7] * 33)) / 100);
		trapdam = (trapdam * spellstat);

                place_field(FEAT_POISON_TRAP, 0, px, py, trapdam);
                msg_print("You set a Poison Trap!");

                inven_item_increase(item, -1);
                inven_item_describe(item);
                inven_item_optimize(item);
        }
        else msg_print("The weapon must be poison-branded!");
}

void ranger_entangle()
{
        int ii, ij, rad;
        rad = (p_ptr->abilities[(CLASS_RANGER * 10) + 2] / 5) + 1;
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
	/* This determines casting power */
	spellstat = (p_ptr->stat_ind[A_WIS] - 5);

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
        	else place_field(FEAT_THORNED_VINES, (p_ptr->abilities[(CLASS_RANGER * 10) + 7] / 30) + 2, ii, ij, (spellstat * (p_ptr->abilities[(CLASS_RANGER * 10) + 7] * 5)));
	}
        update_and_handle();
}

/* The Ranger's Force of nature! */
void ranger_force_of_nature()
{
        cave_type *c_ptr;
        c_ptr = &cave[py][px];

        if (c_ptr->feat == FEAT_TREES || c_ptr->feat == FEAT_GRASS)
        {
                msg_print("You empower yourself with the essence of nature!");
                p_ptr->str_boost = (p_ptr->abilities[(CLASS_RANGER * 10) + 9] * 3);
                p_ptr->dex_boost = (p_ptr->abilities[(CLASS_RANGER * 10) + 9] * 3);
                p_ptr->con_boost = (p_ptr->abilities[(CLASS_RANGER * 10) + 9] * 3);
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
        
        object_prep(q_ptr, lookup_kind(TV_SWORD, 46));
        q_ptr->number = 1;
        object_aware(q_ptr);
        object_known(q_ptr);

        /* Store the monster's info in the soul! */
        q_ptr->dd += (p_ptr->abilities[(CLASS_PALADIN * 10) + 4] / 4);
        q_ptr->ds += (p_ptr->abilities[(CLASS_PALADIN * 10) + 4] / 4);
        q_ptr->to_h = p_ptr->lev;
        q_ptr->to_d = p_ptr->lev;
        q_ptr->timeout = 10 + ((p_ptr->abilities[(CLASS_PALADIN * 10) + 4] - 1) * 2);        

        msg_print("A sword blessed with pure energy appear in your hand!");
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

        if (o_ptr->art_flags1 || o_ptr->art_flags2 || o_ptr->art_flags3 || o_ptr->art_flags4)
        {
                msg_print("You can only use this on non-magical armors!");
        }
        else
        {
                msg_print("You bless your armor with divine power!");
                o_ptr->ac += p_ptr->abilities[(CLASS_PALADIN * 10) + 8] * 5;
                o_ptr->to_h += p_ptr->abilities[(CLASS_PALADIN * 10) + 8] * 2;
                o_ptr->to_d += p_ptr->abilities[(CLASS_PALADIN * 10) + 8] * 2;
                o_ptr->to_a += p_ptr->abilities[(CLASS_PALADIN * 10) + 8] * 2;
                o_ptr->pval = (p_ptr->abilities[(CLASS_PALADIN * 10) + 8] / 4) + 1;

                o_ptr->art_flags3 |= (TR3_LITE);
                o_ptr->art_flags4 |= (TR4_INDESTRUCTIBLE);

                if (p_ptr->abilities[(CLASS_PALADIN * 10) + 8] >= 5)
                {
                        o_ptr->lightres = p_ptr->abilities[(CLASS_PALADIN * 10) + 8] * 2;
                }
                if (p_ptr->abilities[(CLASS_PALADIN * 10) + 8] >= 10)
                {
                        o_ptr->darkres = p_ptr->abilities[(CLASS_PALADIN * 10) + 8] * 2;
                }
                if (p_ptr->abilities[(CLASS_PALADIN * 10) + 8] >= 25)
                {
                        o_ptr->art_flags4 |= (TR4_SAFETY);
                }
                if (p_ptr->abilities[(CLASS_PALADIN * 10) + 8] >= 40)
                {
                        o_ptr->art_flags2 |= (TR2_LIFE);
                }
                if (p_ptr->abilities[(CLASS_PALADIN * 10) + 8] >= 70)
                {
                        o_ptr->art_flags4 |= (TR4_PROTECTION);
                }


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
        s32b dam = monk_damages();
        monster_race *r_ptr = &r_info[m_ptr->r_idx]; 
        cave_type *c_ptr;
	dam += ((dam * ((p_ptr->abilities[(CLASS_MONK * 10) + 3] - 1) * 5)) / 100);
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

        if (itemtval == TV_SWORD || itemtval == TV_HAFTED || itemtval == TV_POLEARM || itemtval == TV_AXE || itemtval == TV_DAGGER || itemtval == TV_ROD)
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
        else if (itemtval == TV_SHIELD || itemtval == TV_ARM_BAND)
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
        else if (itemtval == TV_BOW)
        {
                slot = INVEN_BOW;
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
        q_ptr->number = 1;
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
        update_and_handle();
}

/* Justice Warrior's Bless Weapon! :) */
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

        if (o_ptr->art_flags1 || o_ptr->art_flags2 || o_ptr->art_flags3 || o_ptr->art_flags4)
        {
                msg_print("You can only use this on non-magical weapons!");
        }
        else
        {
                msg_print("You bless your weapon with divine power!");
                o_ptr->to_h += p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 3] * 2;
                o_ptr->to_d += p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 3] * 2;
                o_ptr->pval = (p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 3] / 5) + 1;
                o_ptr->ds += p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 3] / 10;
                if (p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 3] >= 50) o_ptr->dd += 1;

                o_ptr->art_flags3 |= (TR3_LITE);
                o_ptr->art_flags4 |= (TR4_INDESTRUCTIBLE);

		o_ptr->brandtype = GF_LITE;
		o_ptr->branddam = p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 3] * 50;
		o_ptr->brandrad = 0;

                if (p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 3] >= 5)
                {
                        o_ptr->lightres = (p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 3] * 2);
                }
                if (p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 3] >= 10)
                {
                        o_ptr->art_flags1 |= (TR1_STR);
                }
                if (p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 3] >= 15)
                {
                        o_ptr->art_flags1 |= (TR1_DEX);
                }
                if (p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 3] >= 20)
                {
                        o_ptr->art_flags1 |= (TR1_SLAY_UNDEAD);
                        o_ptr->art_flags1 |= (TR1_SLAY_DEMON);
                }
                if (p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 3] >= 30)
                {
                        o_ptr->art_flags1 |= (TR1_BLOWS);
                }
                if (p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 3] >= 40)
                {
                        o_ptr->art_flags4 |= (TR4_LOWER_DEF);
                }
                if (p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 3] >= 50)
                {
                        o_ptr->art_flags2 |= (TR2_LIFE);
                }
                if (p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 3] >= 60)
                {
                        o_ptr->art_flags4 |= (TR4_LOWER_HIT);
                }


                identify_fully_aux(o_ptr);
                object_aware(o_ptr);
                object_known(o_ptr);
                o_ptr->ident |= (IDENT_MENTAL);
                /* This is to prevent players to sell this armor */
                o_ptr->ident |= (IDENT_BROKEN);
        }

}

/* Use Alchemy and/or Crafting skill to combine two items and make a new one! */
void combine_items()
{
        int item, item2, craftype, x, item1idx, item2idx;
        object_type             *o_ptr;
        object_type             *q_ptr;
        object_type             forge;
        object_type             *a_ptr;
        object_kind *k_ptr;
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

        inven_item_increase(item2, -1);
        inven_item_describe(item2);
        inven_item_optimize(item2);


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
                                if (craftype == 1 && p_ptr->skill[10] < (k_ptr->level / 2))
                                {
                                        msg_format("You failed to make the item. You need a Alchemy skill of at least %d !", k_ptr->level);
                                        return;
                                }
                                if (craftype == 2 && p_ptr->skill[11] < (k_ptr->level / 2))
                                {
                                        msg_format("You failed to make the item. You need a Crafting skill of at least %d !", k_ptr->level);
                                        return;
                                }
                                if (craftype == 3 && (p_ptr->skill[10] < (k_ptr->level / 4) || p_ptr->skill[11] < (k_ptr->level / 4)))
                                {
                                        msg_format("You failed to make the item. You need Alchemy and Crafting skills of at least %d !", (k_ptr->level / 2));
                                        return;
                                }

                                /* Get local object */
                                a_ptr = &forge;
        
                                object_prep(a_ptr, lookup_kind(k_ptr->tval, k_ptr->sval));
                                a_ptr->number = 1;

                                object_aware(a_ptr);
                                object_known(a_ptr);

                                msg_print("You created a new item!");
                                if (is_weapon(o_ptr))
                                {
                                        o_ptr->dd += (o_ptr->dd * p_ptr->skill[11]) / 100;
                                        o_ptr->ds += (o_ptr->dd * p_ptr->skill[11]) / 100;
                                }
                                if (o_ptr->tval == TV_SOFT_ARMOR || o_ptr->tval == TV_HARD_ARMOR || o_ptr->tval == TV_DRAG_ARMOR
                                || o_ptr->tval == TV_SHIELD || o_ptr->tval == TV_HELM || o_ptr->tval == TV_CLOAK || o_ptr->tval == TV_BOOTS
                                || o_ptr->tval == TV_GLOVES || o_ptr->tval == TV_ARM_BAND || o_ptr->tval == TV_CROWN)
                                {
                                        o_ptr->ac += (o_ptr->ac * (p_ptr->skill[11] * 3)) / 100;
                                }

                                (void)inven_carry(a_ptr, FALSE);
                                crafted = TRUE;
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
        o_ptr->tval == TV_STAFF || o_ptr->tval == TV_BATERIE || o_ptr->tval == TV_POTION2 ||
        o_ptr->tval == TV_FLASK || o_ptr->tval == TV_LITE || o_ptr->tval == TV_FOOD || o_ptr->tval == TV_CRYSTAL
        || o_ptr->tval == TV_HYPNOS || o_ptr->tval == TV_EGG || o_ptr->tval == TV_CORPSE || o_ptr->tval == TV_GOLD) return (TRUE);

        return (FALSE);
}

void decompose_item()
{
        int item;
        object_type             *o_ptr;
        object_type             forge;
        object_type             *a_ptr;
        object_kind *k_ptr;
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
        k_ptr = &k_info[o_ptr->k_idx];

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

                (void)inven_carry(a_ptr, FALSE);


                k_ptr = &k_info[rec2];
                a_ptr = &forge;
        
                object_prep(a_ptr, lookup_kind(k_ptr->tval, k_ptr->sval));
                object_aware(a_ptr);
                object_known(a_ptr);

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
        s32b dam = monk_damages();
        monster_race *r_ptr = &r_info[m_ptr->r_idx]; 
        cave_type *c_ptr;
	dam += ((dam * ((p_ptr->abilities[(CLASS_ZELAR * 10) + 4] - 1) * 5)) / 100);
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
                item_tester_tval = TV_ARROW;

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
                item_tester_tval = TV_BOLT;

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
                item_tester_tval = TV_SHOT;

                /* Get an item */
                q = "Sharpen which shots? ";
                s = "You have no shots!";
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
                        msg_print("These shots has already been sharpened!");
                }
                else
                {
                msg_print("You sharpen the shots!");
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
        s32b dam = monk_damages();
        monster_race *r_ptr = &r_info[m_ptr->r_idx]; 
        cave_type *c_ptr;
	dam += ((dam * ((p_ptr->abilities[(CLASS_FIGHTER * 10) + 7] - 1) * 10)) / 100);
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