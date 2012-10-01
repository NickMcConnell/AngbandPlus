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

extern void do_cmd_rerate(void);
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


/*
 * Allow user to choose a spell/prayer from the given book.
 *
 * If a valid spell is chosen, saves it in '*sn' and returns TRUE
 * If the user hits escape, returns FALSE, and set '*sn' to -1
 * If there are no legal choices, returns FALSE, and sets '*sn' to -2
 *
 * The "prompt" should be "cast", "recite", or "study"
 * The "known" should be TRUE for cast/pray, FALSE for study
 */
static int get_spell(int *sn, cptr prompt, int sval, bool known, object_type *o_ptr)
{
        int         realm = o_ptr->tval - TV_VALARIN_BOOK + 1;
	int         i;
        u32b        spell = -1;
	int         num = 0;
	int         ask;
        byte        spells[64];
	bool        flag, redraw, okay;
	char        choice;
	magic_type  *s_ptr;
	char        out_val[160];
        cptr        p = ((mp_ptr->spell_book == TV_VALARIN_BOOK) ? "prayer" : "spell");
        
#ifdef ALLOW_REPEAT /* TNB */

	/* Get the spell, if available */
	if (repeat_pull(sn))
	{
		/* Verify the spell */
                if (spell_okay(*sn, known, realm) || p_ptr->pclass == CLASS_HELLQUEEN)
		{
			/* Success */
			return (TRUE);
		}
	}

#endif /* ALLOW_REPEAT -- TNB */

	/* Extract spells */
        for (spell = 0; spell < 64; spell++)
	{
		/* Check for this spell */
                if ((fake_spell_flags[realm][sval][(spell < 32)] & (1L << (spell % 32))))
		{
			/* Collect this spell */
			spells[num++] = spell;
		}
	}

	/* Assume no usable spells */
	okay = FALSE;

	/* Assume no spells available */
	(*sn) = -2;

	/* Check for "okay" spells */
	for (i = 0; i < num; i++)
	{
		/* Look for "okay" spells */
                if (spell_okay(spells[i], known, realm) || p_ptr->pclass == CLASS_HELLQUEEN) okay = TRUE;
	}

	/* No "okay" spells */
	if (!okay) return (FALSE);

	/* Assume cancelled */
	*sn = (-1);

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	/* Show choices */
	if (show_choices)
	{
		/* Update */
		p_ptr->window |= (PW_SPELL);

		/* Window stuff */
		window_stuff();
	}


	/* Build a prompt (accept all spells) */
	strnfmt(out_val, 78, "(%^ss %c-%c, *=List, ESC=exit) %^s which %s? ",
		p, I2A(0), I2A(num - 1), prompt, p);

	/* Get a spell from the user */
	while (!flag && get_com(out_val, &choice))
	{
		/* Request redraw */
		if ((choice == ' ') || (choice == '*') || (choice == '?'))
		{
			/* Show the list */
			if (!redraw)
			{
				/* Show list */
				redraw = TRUE;

				/* Save the screen */
				Term_save();

				/* Display a list of spells */
                                print_spells(spells, num, 1, 13, realm);
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


		/* Note verify */
		ask = (isupper(choice));

		/* Lowercase */
		if (ask) choice = tolower(choice);

		/* Extract request */
		i = (islower(choice) ? A2I(choice) : -1);

		/* Totally Illegal */
		if ((i < 0) || (i >= num))
		{
			bell();
			continue;
		}

		/* Save the spell index */
		spell = spells[i];

		/* Require "okay" spells */
                if (!spell_okay(spell, known, realm))
		{
                        if (p_ptr->pclass == CLASS_HELLQUEEN) msg_print(NULL);
                        else
                        {
			bell();
			msg_format("You may not %s that %s.", prompt, p);
			continue;
                        }
		}
                


		/* Verify it */
		if (ask)
		{
			char tmp_val[160];

			/* Access the spell */
                        s_ptr = &realm_info[realm][spell%64];

			/* Prompt */
			strnfmt(tmp_val, 78, "%^s %s (%d mana, %d%% fail)? ",
                                prompt, spell_names[realm][spell%64],
                                s_ptr->smana, spell_chance(spell,realm));

			/* Belay that order */
			if (!get_check(tmp_val)) continue;
		}

		/* Stop the loop */
		flag = TRUE;
	}


	/* Restore the screen */
	if (redraw) Term_load();


	/* Show choices */
	if (show_choices)
	{
		/* Update */
		p_ptr->window |= (PW_SPELL);

		/* Window stuff */
		window_stuff();
	}


	/* Abort if needed */
	if (!flag) return (FALSE);

	/* Save the choice */
	(*sn) = spell;

#ifdef ALLOW_REPEAT /* TNB */

	repeat_push(*sn);

#endif /* ALLOW_REPEAT -- TNB */

	/* Success */
	return (TRUE);
}


/*
 * Peruse the spells/prayers in a book
 *
 * Note that *all* spells in the book are listed
 *
 * Note that browsing is allowed while confused or blind,
 * and in the dark, primarily to allow browsing in stores.
 */

/*
 * Helper function for browsing
 */
void do_cmd_browse_aux(object_type *o_ptr)
{
        int             sval;
	int		spell = -1;
	int		num = 0;
	
	byte		spells[64];

	/* Access the item's sval */
	sval = o_ptr->sval;
	
	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);
	
	/* Hack -- Handle stuff */
	handle_stuff();

	/* Extract spells */
        for (spell = 0; spell < 64; spell++)
	{
		/* Check for this spell */
                if ((fake_spell_flags[o_ptr->tval - TV_VALARIN_BOOK + 1][sval][(spell < 32)] & (1L << (spell % 32))))
		{
			/* Collect this spell */
			spells[num++] = spell;
		}
	}
	
	
	/* Save the screen */
	Term_save();
	
	/* Display the spells */
        print_spells(spells, num, 1, 13, o_ptr->tval - TV_VALARIN_BOOK + 1);
	
	/* Clear the top line */
	prt("", 0, 0);
	
	/* Prompt user */
	put_str("[Press any key to continue]", 0, 23);
	
	/* Wait for key */
	(void)inkey();
	
	/* Restore the screen */
	Term_load();
}

void do_cmd_browse(void)
{
        int             item;
	
	cptr q, s;
	
	object_type	*o_ptr;

	/* Warriors are illiterate */
	if (!(p_ptr->realm1 || p_ptr->realm2))
	{
		msg_print("You cannot read books!");
		return;
	}
#if 0 
	/* No lite */
	if (p_ptr->blind || no_lite())
	{
		msg_print("You cannot see!");
		return;
	}
	
	/* Confused */
	if (p_ptr->confused)
	{
		msg_print("You are too confused!");
		return;
	}
#endif 
	/* Restrict choices to "useful" books */
        item_tester_tval = mp_ptr->spell_book;
	
	/* Get an item */
	q = "Browse which book? ";
	s = "You have no books that you can read.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;
	
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

        do_cmd_browse_aux(o_ptr);
}


/*
 * Modify the realm_info array using the number of spell learned as a
 * modificator
 */
void calc_magic_bonus()
{
        int i, j, num[MAX_REALM];

        for(i = 0; i < MAX_REALM; i++)
        {
                num[i] = 0;

                for(j = 0; j < 64; j++)
                {
                        if((spell_learned[i][j < 32] & (1 << (j % 32))) &&
                           !(spell_forgotten[i][j < 32] & (1 << (j % 32))))
                                num[i]++;
                }
        }

        for(i = 0; i < MAX_REALM; i++)
                for(j = 0; j < 64; j++)
                {
                        int tmp;

                        /* Take the base info */
                        realm_info[i][j].slevel = realm_info_base[i][j].slevel;
                        realm_info[i][j].smana = realm_info_base[i][j].smana;
                        realm_info[i][j].sfail = realm_info_base[i][j].sfail;
                        realm_info[i][j].sexp = realm_info_base[i][j].sexp;

                        tmp = realm_info[i][j].smana;
                        tmp -= (tmp * num[i]) / 80;
                        tmp = (tmp < 1)?1:(tmp > 255)?255:tmp;
                        realm_info[i][j].smana = tmp;

                        tmp = realm_info[i][j].sfail;
                        tmp -= (tmp * num[i]) / 100;
                        tmp = (tmp < 5)?5:(tmp > 95)?95:tmp;
                        realm_info[i][j].sfail = tmp;
                }
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
                        case 9:
                                msg_print("You learned the effect of Nether!");
                                p_ptr->elemental_effects |= ELEM_NETHER;
                                break;
                        case 10:
                                msg_print("You learned the effect of Nexus!");
                                p_ptr->elemental_effects |= ELEM_NEXUS;
                                break;
                        case 11:
                                msg_print("You learned the effect of Plasma!");
                                p_ptr->elemental_effects |= ELEM_PLASMA;
                                break;
                        case 12:
                                msg_print("You learned the effect of Nuke!");
                                p_ptr->elemental_effects |= ELEM_NUKE;
                                break;
                        case 13:
                                msg_print("You learned the effect of Water!");
                                p_ptr->elemental_effects |= ELEM_WATER;
                                break;
                        case 14:
                                msg_print("You learned the effect of Chaos!");
                                p_ptr->elemental_effects |= ELEM_CHAOS;
                                break;
                        case 15:
                                msg_print("You learned the effect of Inertia!");
                                p_ptr->elemental_effects |= ELEM_INERTIA;
                                break;
                        case 16:
                                msg_print("You learned the effect of Time!");
                                p_ptr->elemental_effects |= ELEM_TIME;
                                break;
                        case 17:
                                msg_print("You learned the effect of Shards!");
                                p_ptr->elemental_effects |= ELEM_SHARDS;
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
                        case 21:
                                msg_print("You learned the effect of Force!");
                                p_ptr->elemental_effects |= ELEM_FORCE;
                                break;
                        case 22:
                                msg_print("You learned the effect of Gravity!");
                                p_ptr->elemental_effects |= ELEM_GRAVITY;
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
                        case 4:
                                msg_print("You learned the effect of Cure Black Breath!");
                                p_ptr->healing_effects |= HEAL_CURE_BLACK_BREATH;
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
                                msg_print("You learned the effect of Energize!");
                                p_ptr->healing_effects |= HEAL_ENERGIZE;
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


void do_poly_wounds(void)
{
	/* Changed to always provide at least _some_ healing */
	s16b wounds = p_ptr->cut;
	s16b hit_p = (p_ptr->mhp - p_ptr->chp);
	s16b change = damroll(p_ptr->lev, 5);
	bool Nasty_effect = (randint(5)==1);

	if (!(wounds || hit_p || Nasty_effect)) return;

	msg_print("Your wounds are polymorphed into less serious ones.");
	hp_player(change);
	if (Nasty_effect)
	{
		msg_print("A new wound was created!");
		take_hit(change/2, "a polymorphed wound");
		set_cut(change);
	}
	else
	{
		set_cut((p_ptr->cut)-(change/2));
	}
}

void do_poly_self(void)
{
	int power = p_ptr->lev;
	
	msg_print("You feel a change coming over you...");
	
	if ((power > rand_int(20)) && (rand_int(3) == 1))
	{
		char effect_msg[80] = "";
		int new_race, expfact, goalexpfact;
		
		/* Some form of racial polymorph... */
		power -= 10;
		
		if ((power > rand_int(5)) && (rand_int(4) == 1))
		{
			/* sex change */
			power -= 2;
			
			if (p_ptr->psex == SEX_MALE)
			{
				p_ptr->psex = SEX_FEMALE;
				sp_ptr = &sex_info[p_ptr->psex];
				sprintf(effect_msg,"female");
			}
			else
			{
				p_ptr->psex = SEX_MALE;
				sp_ptr = &sex_info[p_ptr->psex];
				sprintf(effect_msg,"male");
			}
		}
		
		if ( power>rand_int(30) && rand_int(5)==1 )
		{
			int tmp = 0;
			
			/* Harmful deformity */
			power -= 15;
			
			while (tmp < 6)
			{
				if ( rand_int(2)==1 )
				{
					(void)dec_stat(tmp, randint(6)+6, (randint(3)==1));
					power -= 1;
				}
				tmp++;
			}
			
			/* Deformities are discriminated against! */
			(void)dec_stat(A_CHR, randint(6), TRUE);
			
			if (effect_msg[0])
			{
				char tmp_msg[10];
				sprintf(tmp_msg,"%s",effect_msg);
				sprintf(effect_msg,"deformed %s",tmp_msg);
			}
			else
			{
				sprintf(effect_msg,"deformed");
			}
		}
		
		while ( power>rand_int(20) && rand_int(10)==1 )
		{
			/* Polymorph into a less mutated form */
			power -= 10;
			
			lose_mutation(0);
		}
		
		/* Restrict the race choices by exp penalty so weak polymorph
		always means weak race
		*/
		goalexpfact = 100 + 3*rand_int(power);
		
		do
		{
			new_race = rand_int(MAX_RACES);
			expfact = race_info[new_race].r_exp;
		}
		while ( (new_race == p_ptr->prace) && (expfact > goalexpfact) );
		
		if (effect_msg[0])
		{
			msg_format("You turn into a%s %s!",
                                (( new_race == RACE_ELF )?"n":""),
				race_info[new_race].title);
		}
		else
		{
			msg_format("You turn into a %s %s!", effect_msg,
				race_info[new_race].title);
		}
		
		p_ptr->prace = new_race;
		rp_ptr = &race_info[p_ptr->prace];
		
		/* Experience factor */
		p_ptr->expfact = rp_ptr->r_exp + cp_ptr->c_exp;
		
		/* Calculate the height/weight for males */
		if (p_ptr->psex == SEX_MALE)
		{
			p_ptr->ht = randnor(rp_ptr->m_b_ht, rp_ptr->m_m_ht);
			p_ptr->wt = randnor(rp_ptr->m_b_wt, rp_ptr->m_m_wt);
		}
		
		/* Calculate the height/weight for females */
		else if (p_ptr->psex == SEX_FEMALE)
		{
			p_ptr->ht = randnor(rp_ptr->f_b_ht, rp_ptr->f_m_ht);
			p_ptr->wt = randnor(rp_ptr->f_b_wt, rp_ptr->f_m_wt);
		}
		
		check_experience();
		p_ptr->max_plv = p_ptr->lev;
		
		p_ptr->redraw |= (PR_BASIC);
		
		p_ptr->update |= (PU_BONUS);
		
		handle_stuff();
		lite_spot(py, px);
	}

	if ((power > rand_int(30)) && (rand_int(6) == 1))
	{
		int tmp = 0;
		
		/* Abomination! */
		power -= 20;
		
		msg_print("Your internal organs are rearranged!");
		while (tmp < 6)
		{
			(void)dec_stat(tmp, randint(6)+6, (randint(3)==1));
			tmp++;
		}
		if (randint(6)==1)
		{
			msg_print("You find living difficult in your present form!");
			take_hit(damroll(randint(10),p_ptr->lev), "a lethal mutation");
			power -= 10;
		}
	}
	
	if ((power > rand_int(20)) && (rand_int(4) == 1))
	{
		power -= 10;
		
		do_cmd_rerate();
	}
	
	while ((power > rand_int(15)) && (rand_int(3) == 1))
	{
		power -= 7;
		(void) gain_random_mutation(0);
	}
	
	if (power > rand_int(5))
	{
		power -= 5;
		do_poly_wounds();
	}
	
	/* Note: earlier deductions may have left power < 0 already. */
	while (power > 0)
	{
		mutate_player();
		power--;
	}
}

/*
 * Brand the current weapon
 */
void brand_weapon(int brand_type)
{
	object_type *o_ptr;
	
	o_ptr = &inventory[INVEN_WIELD];
	
	/* you can never modify artifacts / ego-items */
	/* you can never modify cursed items */
	/* TY: You _can_ modify broken items (if you're silly enough) */
	if ((o_ptr->k_idx) &&
		(!artifact_p(o_ptr)) && (!ego_item_p(o_ptr)) &&
		(!(o_ptr->art_name)) && (!cursed_p(o_ptr)))
	{
		cptr act = NULL;
		
		/* Let's get the name before it is changed... */
		char o_name[80];
		object_desc(o_name, o_ptr, FALSE, 0);
		
		switch (brand_type)
		{
                case 6:
                        act = "glows with godly power.";
                        o_ptr->name2 = EGO_BLESS_BLADE;
                        o_ptr->pval = randint(4);
			break;
                case 5:
                        act = "seems very powerful.";
                        o_ptr->name2 = EGO_EARTHQUAKES;
                        o_ptr->pval = randint(3);
			break;
		case 4:
			act = "seems very unstable now.";
			o_ptr->name2 = EGO_TRUMP;
			o_ptr->pval = randint(2);
			break;
		case 3:
			act = "thirsts for blood!";
			o_ptr->name2 = EGO_VAMPIRIC;
			break;
		case 2:
			act = "is coated with poison.";
			o_ptr->name2 = EGO_BRAND_POIS;
			break;
		case 1:
                        act = "is engulfed in raw chaos!";
			o_ptr->name2 = EGO_CHAOTIC;
			break;
		default:
			if (rand_int(100) < 25)
			{
				act = "is covered in a fiery shield!";
				o_ptr->name2 = EGO_BRAND_FIRE;
			}
			else
			{
				act = "glows deep, icy blue!";
				o_ptr->name2 = EGO_BRAND_COLD;
			}
		}
		
		msg_format("Your %s %s", o_name, act);
		
		enchant(o_ptr, rand_int(3) + 4, ENCH_TOHIT | ENCH_TODAM);
	}
	else
	{
		if (flush_failure) flush();
		
		msg_print("The Branding failed.");
	}
}


static void call_the_(void)
{
	int i;
				
	if (cave_floor_bold(py-1,px-1) && cave_floor_bold(py-1, px) &&
		cave_floor_bold(py-1,px+1) && cave_floor_bold(py,px-1) &&
		cave_floor_bold(py,px+1) && cave_floor_bold(py+1,px-1) &&
		cave_floor_bold(py+1,px) && cave_floor_bold(py+1,px+1))
	{
		for (i = 1; i < 10; i++)
		{
			if (i-5) fire_ball(GF_ROCKET, i, 175, 2);
		}
		
		for (i = 1; i < 10; i++)
		{
			if (i-5) fire_ball(GF_MANA, i, 175, 3);
		}
		
		for (i = 1; i < 10; i++)
		{
			if (i-5) fire_ball(GF_NUKE, i, 175, 4);
		}
	}
	else
	{
		msg_format("You %s the %s too close to a wall!",
                        ((mp_ptr->spell_book == TV_VALARIN_BOOK) ? "recite" : "cast"),
                        ((mp_ptr->spell_book == TV_VALARIN_BOOK) ? "prayer" : "spell"));
		msg_print("There is a loud explosion!");

		/* Prevent destruction of quest levels and town */
		if (!is_quest(dun_level) && dun_level)
		{
			destroy_area(py, px, 20+(p_ptr->lev), TRUE);
			msg_print("The dungeon collapses...");
		}
		else
		{
			msg_print("The dungeon trembles...");
		}
		take_hit(100 + (randint(150)), "a suicidal Call the Void");
	}
}


/*
 * Fetch an item (teleport it right underneath the caster)
 */
void fetch(int dir, int wgt, bool require_los)
{
	int             ty, tx, i;
	bool            flag;
	cave_type       *c_ptr;
	object_type     *o_ptr;
	char            o_name[80];
	
	/* Check to see if an object is already there */
	if (cave[py][px].o_idx)
	{
		msg_print("You can't fetch when you're already standing on something.");
		return;
	}
	
	/* Use a target */
	if (dir == 5 && target_okay())
	{
		tx = target_col;
		ty = target_row;
		
		if (distance(py, px, ty, tx) > MAX_RANGE)
		{
			msg_print("You can't fetch something that far away!");
			return;
		}
		
		c_ptr = &cave[ty][tx];

		if (!c_ptr->o_idx)
		{
			msg_print("There is no object at this place.");
			return;
		}

		if (require_los && (!player_has_los_bold(ty, tx)))
		{
			msg_print("You have no direct line of sight to that location.");
			return;
		}
	}
	else
	{
		/* Use a direction */
		ty = py; /* Where to drop the item */
		tx = px;
		flag = FALSE;
		
		do
		{
			ty += ddy[dir];
			tx += ddx[dir];
			c_ptr = &cave[ty][tx];
			
			if ((distance(py, px, ty, tx)> MAX_RANGE)
				|| !cave_floor_bold(ty, tx)) return;
		}
		while(!c_ptr->o_idx);
	}
	
	o_ptr = &o_list[c_ptr->o_idx];
	
	if (o_ptr->weight > wgt)
	{
		/* Too heavy to 'fetch' */
		msg_print("The object is too heavy.");
		return;
	}
	
	i = c_ptr->o_idx;
	c_ptr->o_idx = o_ptr->next_o_idx;
	cave[py][px].o_idx = i; /* 'move' it */
	o_ptr->next_o_idx = 0;
	o_ptr->iy = py;
	o_ptr->ix = px;

	object_desc(o_name, o_ptr, TRUE, 0);
	msg_format("%^s flies through the air to your feet.", o_name);
	
	note_spot(py,px);
	p_ptr->redraw |= PR_MAP;
}

void shriek_effect()
{
        switch(randint(9))
        {
                case 1: case 5: case 8: case 9:
                        msg_print("You made a high pitched shriek!");
                        aggravate_monsters(1);
                        break;
                case 2: case 6:
                        msg_print("Oups! You call a monster.");
                        summon_specific(py, px, max_dlv[dungeon_type], 0);
                        break;
                case 3: case 7:
                        msg_print("The dungeon collapses!");
                        earthquake(py, px, 5);
                        break;
                case 4:
                        msg_print("Your shriek is so horrible that you damage your health!");
                        take_hit(damroll(p_ptr->lev/5,8),"a too vibrating sound");
                        break;
        }
}

void wild_magic(int spell)
{
	int counter = 0;
	int type = SUMMON_BIZARRE1 - 1 + randint(6);
	
	if (type < SUMMON_BIZARRE1) type = SUMMON_BIZARRE1;
	else if (type > SUMMON_BIZARRE6) type = SUMMON_BIZARRE6;
	
	switch(randint(spell) + randint(8) + 1)
	{
	case 1:
	case 2:
	case 3:
		teleport_player(10);
		break;
	case 4:
	case 5:
	case 6:
		teleport_player(100);
		break;
	case 7:
	case 8:
		teleport_player(200);
		break;
	case 9:
	case 10:
	case 11:
		unlite_area(10, 3);
		break;
	case 12:
	case 13:
	case 14:
		lite_area(damroll(2, 3), 2);
		break;
	case 15:
		destroy_doors_touch();
		break;
	case 16: case 17:
		wall_breaker();
	case 18:
		sleep_monsters_touch();
		break;
	case 19:
	case 20:
		trap_creation();
		break;
	case 21:
	case 22:
		door_creation();
		break;
	case 23:
	case 24:
	case 25:
		aggravate_monsters(1);
		break;
	case 26:
		/* Prevent destruction of quest levels and town */
		if (!is_quest(dun_level) && dun_level)
			earthquake(py, px, 5);
		break;
	case 27:
	case 28:
		(void) gain_random_mutation(0);
		break;
	case 29:
	case 30:
		apply_disenchant(0);
		break;
	case 31:
		lose_all_info();
		break;
	case 32:
		fire_ball(GF_CHAOS, 0, spell + 5, 1 + (spell / 10));
		break;
	case 33:
		wall_stone();
		break;
	case 34:
	case 35:
		while (counter++ < 8)
		{
			(void) summon_specific(py, px, (dun_level * 3) / 2, type);
		}
		break;
	case 36:
	case 37:
		activate_hi_summon();
		break;
	case 38:
		summon_cyber();
	default:
		activate_ty_curse();
	}
	
	return;
}



static void cast_valarin_spell(int spell)
{
	int	dir;
        int     plev = p_ptr->lev, i;
	bool    no_trump = FALSE;
        int to_s2=p_ptr->to_s;
        int mto_s2=p_ptr->to_s;

        mto_s2 = (mto_s2==0)?1:mto_s2;
	
	switch (spell)
	{
	   case 0: /* Detect Evil */
                       (void)detect_monsters_evil();
		       break;
           case 1: /* Call Light */
                       (void)lite_area(damroll(2, (plev / 2)), (plev / 10)*mto_s2 + 1);
		       break;
	   case 2: /* Bless */
                       (void)set_blessed(p_ptr->blessed + randint(12)+to_s2 + 12);
		       break; 
	   case 3: /* Remove Fear */
                       (void)set_afraid(0);
		       break;
           case 4: /* Cure Light Wounds */
                        (void)hp_player(damroll(2, 10)+to_s2);
			(void)set_cut(p_ptr->cut - 10);
		       break;
           case 5: /* Slow Poison */
                        (void)set_poisoned((p_ptr->poisoned / 2) - to_s2);
		       break;
           case 6: /* Detect Secret Doors */
			(void)detect_doors();
			(void)detect_stairs();
		       break;
           case 7: /* Detect Traps */
			(void)detect_traps();
		       break;

           case 8: /* Spear of Light */
                       if (!get_aim_dir(&dir)) return;
                       msg_print("A line of blue shimmering light appears.");
                       lite_line(dir);
                       break;
           case 9: /* Remove Curse */
			remove_curse();
		       break;
           case 10: /* Cure Serious Wounds */
                        (void)hp_player(damroll(6, 10)+to_s2);
			(void)set_cut(0);
		       break;
           case 11: /* Satisfy Hunger */
			(void)set_food(PY_FOOD_MAX - 1);
		       break;
           case 12: /* Holy Chant */
                       set_blessed(p_ptr->blessed + randint(24) + 12 + to_s2);
		       break;
           case 13: /* Sense Surroundings */
                       map_area();
		       break;
           case 14: /* Neutralize Poison */
                       set_poisoned(0);
		       break;
           case 15: /* resist fire&cold */
                       set_oppose_fire(p_ptr->oppose_fire + randint(10) + 10 + to_s2);
                       set_oppose_cold(p_ptr->oppose_cold + randint(10) + 10 + to_s2);
                       break;
           case 16: /* Cure critical wounds */
                        (void)hp_player(damroll(8, 10)+to_s2);
                        (void)set_stun(0);
			(void)set_cut(0);
                       break;
           case 17: /* Orb or Draining */
                       if (!get_aim_dir(&dir)) return;
                       fire_ball(GF_HOLY_FIRE, dir, (damroll(3, 6) + plev + (plev / ((p_ptr->pclass == 2 || p_ptr->pclass == CLASS_HIGH_MAGE) ? 2 : 4)))*mto_s2,
                                        ((plev < 30) ? 2 : 3)+to_s2);
		       break;
           case 18: /* Portal */
                        teleport_player(200 * mto_s2);
			break;
       case 19: /* Sense Unseen */
                       set_tim_invis(p_ptr->tim_invis + 20 + randint(20) + p_ptr->to_s);
                       break;
       case 20: /* Earthquake */
                       earthquake(py, px, 8 + to_s2);
		       break;
           case 21: /* Turn Undead */
			if (!get_aim_dir(&dir)) return;
                        fire_beam(GF_TURN_UNDEAD, dir, damroll(5, 10) * mto_s2);
			break;
           case 22: /* Perception */
                       ident_spell();
		       break;
           case 23: /* Holy Aura */
           {
                       int t = randint(25);
                       (void)set_holy(p_ptr->holy + t + to_s2 + 3 * plev);
                       (void)set_protevil(p_ptr->protevil + t + 3 * plev + to_s2);
		       break;
           }
           case 24: /* Heroism */
                        (void)set_hero(p_ptr->hero + randint(20)+ p_ptr->to_s + 20);
		       break;
           case 25: /* Word of recall */
                       recall_player();
		       break;
           case 26: /* Dispel undead */
                       dispel_undead(60 * mto_s2);
                       break;
           case 27: /* Heal */
                       (void)hp_player(150 * mto_s2);
                       (void)set_stun(0);
                       (void)set_cut(0);
		       break;
           case 28: /* Banish */
                       project_hack(GF_AWAY_ALL, 100 * mto_s2);
		       break;
           case 29: /* Dispel evil */
                       dispel_evil(120 * mto_s2);
		       break;
           case 30: /* Holy Word */
                        (void)dispel_evil(plev * 4 * mto_s2);
                        (void)hp_player(500 * mto_s2);
			(void)set_afraid(0);
			(void)set_poisoned(0);
			(void)set_stun(0);
			(void)set_cut(0);
		       break;
                case 31: /* animal taming */
                        if (!get_aim_dir(&dir)) return;
                        (void) charm_animal(dir, plev * mto_s2);
                        break;
                case 32: /* stone to mud */
			if (!get_aim_dir(&dir)) return;
			(void)wall_to_mud(dir);
                        break;
                case 33: /* sterilization */
                        set_no_breeders(no_breeds + randint(10) + 10 + p_ptr->to_s);
                        break;
                case 34: /* summon animal */
                        if (!(summon_specific_friendly(py, px, plev+to_s2, SUMMON_ANIMAL_RANGER, TRUE)))
                                no_trump = TRUE;
                        break;
                case 35: /* haste self */
			if (!p_ptr->fast)
			{
                                (void)set_fast(randint(20 + (plev) ) + plev+to_s2);
			}
			else
			{
                                (void)set_fast(p_ptr->fast + randint(5)+to_s2);
			}
                        break;
                case 36: /* call hounds */
                        msg_format("You magically summon hounds.");
                        for (i = 0; i < 6; i++)
                        {
                                summon_specific_friendly(py, px, dun_level + p_ptr->to_s, SUMMON_HOUND, TRUE);
                        }
                case 37: /* whirlwind attack */
                         {
                         int y = 0, x = 0;
                         cave_type       *c_ptr;
                         monster_type    *m_ptr;

                         for (dir = 0; dir <= 9; dir++)
                         {
                             y = py + ddy[dir];
                             x = px + ddx[dir];
                             c_ptr = &cave[y][x];

                             /* Get the monster */
                             m_ptr = &m_list[c_ptr->m_idx];

                             /* Hack -- attack monsters */
                             if (c_ptr->m_idx && (m_ptr->ml || cave_floor_bold(y, x)))
                           py_attack(y, x, -1);
                         }
                         }
                         break;

                case 38: /* unbarring ways */
                        (void)destroy_doors_touch();
                        break;
                case 39: /* sanctuary */
                        (void)sleep_monsters_touch();
                        break;
                case 40: /* dispel curse */
                        remove_all_curse();
                        break;
                case 41: /* tread water */
                        set_walk_water(p_ptr->walk_water + plev + randint(plev) + p_ptr->to_s);
                        break;
                case 42: /* healing */
                        (void)hp_player(300 + (p_ptr->to_s * 3));
			(void)set_stun(0);
			(void)set_cut(0);
			(void)set_poisoned(0);
                        (void)set_image(0);
                        (void)set_afraid(0);
                        break;
                case 43: /* bless weapon */
                        brand_weapon(6);
                        break;
                case 44: /* glyph of warding */
                        warding_glyph();
                        break;
                case 45: /* invis */
                        (void)set_invis(p_ptr->tim_invisible + randint(24) + p_ptr->to_s, 30);
                        break;
                case 46: /* CMW */
                        (void)hp_player(damroll(8 * mto_s2, 10));
                        (void)set_stun(0);
                        (void)set_cut(0);
                        break;
                case 47: /* mass sleep */
			(void)sleep_monsters();
                        break;
                case 48: /* remembrance */
                        (void)restore_level();
                        break;
                case 49: /* restoration */
                        (void)do_res_stat(A_STR);
                        (void)do_res_stat(A_INT);
                        (void)do_res_stat(A_WIS);
                        (void)do_res_stat(A_DEX);
                        (void)do_res_stat(A_CON);
                        (void)do_res_stat(A_CHR);
                        break;
                case 50: /* mass charm */
                        charm_monsters(100 * mto_s2);
                        break;
                case 51: /* wraithform */
                        set_shadow(p_ptr->wraith_form + randint(plev/2) + (plev/2)+to_s2);
                        break;
                case 52: /* chain lightning */
                        for (dir = 0; dir <= 9; dir++)
                                fire_beam(GF_ELEC, dir, damroll(5+(plev/10), 8) * mto_s2);
                        break;
                case 53: /* disintegration */
                        if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_DISINTEGRATE, dir,
                                  80 + (plev) * mto_s2, 3 + (plev/40) + to_s2);
                        break;
                case 54: /* blizard */
                        if (!get_aim_dir(&dir)) return;
			fire_ball(GF_COLD, dir,
                                70 + (plev)*mto_s2, (plev/12) + to_s2+1);
                        break;
                case 55: /* whirlpool */
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_WATER, dir,
                                100 + (plev)*mto_s2, (plev/12) + to_s2+1);
                        break;
                case 56: /* lightning storm */
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_ELEC, dir,
                                90 + (plev)*mto_s2, (plev/12) + to_s2+1);
                        break;
                case 57: /* meteor swarm */
           {
		       int x, y, dx, dy, d, count = 0;
		       int b = 10 + randint(10); 
		       for (i = 0; i < b; i++) {
			   do {
			       count++;
			       if (count > 1000)  break;
			       x = px - 5 + randint(10);
			       y = py - 5 + randint(10);
			       dx = (px > x) ? (px - x) : (x - px);
			       dy = (py > y) ? (py - y) : (y - py);
			       /* Approximate distance */
                   d = (dy > dx) ? (dy + (dx>>1)) : (dx + (dy>>1));
               } while ((d > 5) || (!(player_has_los_bold(y, x))));
			   
			   if (count > 1000)   break;
			   count = 0;
               project(0, 2, y, x, (plev*3+to_s2)/2, GF_METEOR, PROJECT_KILL|PROJECT_JUMP|PROJECT_ITEM);
		       }
		   }
                        break;
                case 58: /* infusion */
                       (void)recharge(plev * 2 + (p_ptr->to_s * 2));
                        break;
                case 59: /* alter reality */
                        alter_reality();
                        break;
                case 60: /* restore life */
			(void)restore_level();
                        hp_player(3000 * mto_s2);
                        break;
                case 61: /* call angel */
                {
                        msg_format("You magically summon an angel.");
                        summon_specific_friendly(py, px, dun_level + p_ptr->to_s, SUMMON_ANGEL, TRUE);
                        break;
                }
                case 62: /* earendil's star */
                        fire_ball(GF_LITE, 0, 150 * mto_s2, 8 + to_s2);
                        wiz_lite();
                        if (((p_ptr->prace == RACE_VAMPIRE)||(p_ptr->mimic_form == MIMIC_VAMPIRE)) && !(p_ptr->resist_lite))
                        {
                               msg_print("The starlight scorches your flesh!");
                               take_hit(50, "starlight");
                        }
                        break;
                case 63: /* divinity */
                        if(p_ptr->mimic_form != MIMIC_VALAR)
                                set_mimic(8 + randint(5) + to_s2, MIMIC_VALAR);
                        else
                                set_mimic(p_ptr->tim_mimic + 8 + randint(5) + to_s2, MIMIC_VALAR);
                        break;

	       default:
                 msg_format("You cast an unknown Valarin spell: %d.", spell);
		 msg_print(NULL);
	   }

	if (no_trump)
	{
                msg_print("Nobody answers to your call.");
	}
}



static void cast_magery_spell(int spell)
{
        int     dir, beam;
        int     plev = p_ptr->lev;
        int to_s2=p_ptr->to_s;
        int mto_s2=p_ptr->to_s;

        mto_s2 = (mto_s2==0)?1:mto_s2;

	if (p_ptr->pclass == CLASS_MAGE) beam = plev;
	else if (p_ptr->pclass == CLASS_HIGH_MAGE) beam = plev + 10;
	else beam = plev / 2;
	
	switch (spell)
	{
                case 0: /* Magic Missile */
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam-10, GF_MISSILE, dir,
                                                  damroll(3 + ((plev - 1) / 5), 4 * mto_s2));
                        break;
	   case 1: /* Phase Door */
                        teleport_player(10 * mto_s2);
		       break;
           case 2: /* Detect Monsters */
			(void)detect_monsters_normal();
		       break;
           case 3: /* Detect Traps */
			(void)detect_traps();
		       break; 
           case 4: /* Light Area */
                        (void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1+to_s2);
                       break;
           case 5: /* Detect Doors/Stairs */
                        (void)detect_doors();
                        (void)detect_stairs();
		       break; 
           case 6: /* Confuse Monster */
			if (!get_aim_dir(&dir)) return;
                        (void)confuse_monster(dir, ( plev * 3) / 2 * mto_s2);
			break;
                case 7: /* Scan Object */
                        psychometry();
                        break;

                case 8: /* Noxious Cloud */
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_POIS, dir,
                                10 + (plev / 2)*mto_s2, 2+to_s2);
                        break;
                case 9: /* Teleport */
                        teleport_player(plev * 5 * mto_s2);
                        break;
                case 10: /* Beam of Light */
                        if (!get_aim_dir(&dir)) return;
                        msg_print("A line of blue shimmering light appears.");
                        lite_line(dir);
                        break;
                case 11: /* Sleep Monster */
                        if (!get_aim_dir(&dir)) return;
                        (void)sleep_monster(dir);
                        break;
                case 12: /* Lightning Bolt */
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam-10, GF_ELEC, dir,
                                                  damroll(5+((plev-5)/4), 8 * mto_s2));
                        break;
                case 13: /* Stone to Mud */
			if (!get_aim_dir(&dir)) return;
			(void)wall_to_mud(dir);
                        break;
                case 14: /* Frost Bolt */
			if (!get_aim_dir(&dir)) return;
			fire_bolt_or_beam(beam-10, GF_COLD, dir,
                                damroll(6+((plev-5)/4) * mto_s2, 8));
                        break;
                case 15: /* Recharging */
                        (void)recharge(30 + to_s2);
                        break;

                case 16: /* Ethereal Eye */
                        map_area();
                        set_tim_invis(p_ptr->tim_invis + 20 + randint(20) + p_ptr->to_s);
                        break;
                case 17: /* Fire Bolt */
			if (!get_aim_dir(&dir)) return;
			fire_bolt_or_beam(beam, GF_FIRE, dir,
                                damroll(9+((plev-5)/4), 8 * mto_s2));
                        break;
                case 18: /* Identify */
			(void)ident_spell();
                        break;
                case 19: /* Typhoon Daze */
                        fire_ball(GF_CONFUSION, 0, 30 * mto_s2, 2 + to_s2);
                        break;
                case 20: /* Time Distortion */
                        slow_monsters();
                        break;
                case 21: /* Haste Self */
			if (!p_ptr->fast)
			{
                                (void)set_fast(randint(20 + (plev) ) + plev + to_s2);
			}
			else
			{
                                (void)set_fast(p_ptr->fast + randint(5) + to_s2);
			}
                        break;
                case 22: /* Elemental Blast */
			if (!get_aim_dir(&dir)) return;
			fire_bolt_or_beam(beam, GF_FIRE, dir,
                                damroll(5+((plev-5)/4), 8 * mto_s2));
                        fire_bolt_or_beam(beam, GF_COLD, dir,
                                damroll(5+((plev-5)/4), 8 * mto_s2));
                        fire_bolt_or_beam(beam, GF_ACID, dir,
                                damroll(5+((plev-5)/4), 8 * mto_s2));
                        fire_bolt_or_beam(beam, GF_ELEC, dir,
                                damroll(5+((plev-5)/4), 8 * mto_s2));
		       break;
                case 23: /* Teleport Away */
                        if (!get_aim_dir(&dir)) return;
                        (void)fire_beam(GF_AWAY_ALL, dir, plev);
                        break;

                case 24: /* Scan monster */
                {
                        monster_type *m_ptr;
                        monster_race *r_ptr;
                        int m;

                        msg_print("You recall legends of the fair and foul creatures of the world...");

                        if (!get_rep_dir(&dir)) break;
                        if (!cave[py + ddy[dir]][px + ddx[dir]].m_idx) break;
                        m_ptr = &m_list[cave[py + ddy[dir]][px + ddx[dir]].m_idx];
                        r_ptr = &r_info[m_ptr->r_idx];

                        msg_print("Now you have a better knowledge of this monster.");

                        r_ptr->r_wake = r_ptr->r_ignore = MAX_UCHAR;

                        /* Observe "maximal" attacks */
                        for (m = 0; m < 4; m++)
                        {
                        /* Examine "actual" blows */
			if (r_ptr->blow[m].effect || r_ptr->blow[m].method)
			{
				/* Hack -- maximal observations */
				r_ptr->r_blows[m] = MAX_UCHAR;
			}
                        }

                        /* Hack -- maximal drops */
                        r_ptr->r_drop_gold = r_ptr->r_drop_item =
                        (((r_ptr->flags1 & (RF1_DROP_4D2)) ? 8 : 0) +
                         ((r_ptr->flags1 & (RF1_DROP_3D2)) ? 6 : 0) +
                         ((r_ptr->flags1 & (RF1_DROP_2D2)) ? 4 : 0) +
                         ((r_ptr->flags1 & (RF1_DROP_1D2)) ? 2 : 0) +
                         ((r_ptr->flags1 & (RF1_DROP_90))  ? 1 : 0) +
                         ((r_ptr->flags1 & (RF1_DROP_60))  ? 1 : 0));

                        /* Hack -- but only "valid" drops */
                        if (r_ptr->flags1 & (RF1_ONLY_GOLD)) r_ptr->r_drop_item = 0;
                        if (r_ptr->flags1 & (RF1_ONLY_ITEM)) r_ptr->r_drop_gold = 0;

                        /* Hack -- observe many spells */
                        r_ptr->r_cast_inate = MAX_UCHAR;
                        r_ptr->r_cast_spell = MAX_UCHAR;

                        /* Hack -- know all the flags */
                        r_ptr->r_flags1 = r_ptr->flags1;
                        r_ptr->r_flags2 = r_ptr->flags2;
                        r_ptr->r_flags3 = r_ptr->flags3;
                        r_ptr->r_flags4 = r_ptr->flags4;
                        r_ptr->r_flags5 = r_ptr->flags5;
                        r_ptr->r_flags6 = r_ptr->flags6;

                       break;
                }
                case 25: /* Meditation */
                        set_meditation(p_ptr->meditation + 20 + randint(20) + p_ptr->to_s);
                        break;
                case 26: /* Gravitic Distortion */
                        fire_ball(GF_GRAVITY, 0, 10 * mto_s2, 2 + to_s2);
                        break;
                case 27: /* Dopplegangaer */
                {
                        int ii, ij;

                        msg_print("Choose a location for your doppleganger.");
                        if (!tgt_pt(&ii,&ij)) return;
                        if (!cave_empty_bold(ij,ii) || (cave[ij][ii].info & CAVE_ICKY))
                        {
                                msg_print("You can't put a doppleganger here!");
                                break;
                        }
                        place_monster_aux(ij, ii, test_monster_name("Doppleganger"), FALSE, FALSE, TRUE);
                        break;
                }
		       break;
                case 28: /* Firestorm */
                       if (!get_aim_dir(&dir)) return;
                       (void)fire_ball(GF_FIRE, dir, 100 * mto_s2, 2 + to_s2);
		       break;
                case 29: /* Force Shield */
                       (void)set_shield(p_ptr->shield + randint(20) + 30 + to_s2, 50 + to_s2);
		       break;
                case 30: /* Crippled Gaze */
                        if (!get_aim_dir(&dir)) return;
                        (void)fire_ball(GF_STUN_CONF, dir, 100 * mto_s2, 0);
                        (void)fire_ball(GF_OLD_SLOW, dir, 100 * mto_s2, 0);
                        break;
                case 31: /* Collapse Cieling */
                {
                        int y, x;

                       if (!tgt_pt(&x, &y)) break;
                       earthquake(y, x, 5 + to_s2);
		       break;
                }

                case 32: /* Gravitic Beam */
                       if (!get_aim_dir(&dir)) return;
                       (void)fire_beam(GF_GRAVITY, dir, 20 * mto_s2);
		       break;
                case 33: /* Sanctuary */
                       wall_stone();
		       break;
                case 34: /* Starbust */
                {
                        int i;

                        for (i = 1; i < 10; i++)
                        {
                                if (i - 5) fire_beam(GF_LITE, i, 90 * mto_s2);
                        }
                        break;
                }
                case 35: /* Statis Cage */
                        if (!get_aim_dir(&dir)) return;
                        (void)fire_ball(GF_OLD_SLEEP, dir, 100 * mto_s2, 0);
                        break;
                case 36: /* Elemental Shield */
                        (void)set_oppose_acid(p_ptr->oppose_acid + randint(20) + 20 + to_s2);
                        (void)set_oppose_elec(p_ptr->oppose_elec + randint(20) + 20 + to_s2);
                        (void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + 20 + to_s2);
                        (void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20 + to_s2);
                        (void)set_oppose_pois(p_ptr->oppose_pois + randint(20) + 20 + to_s2);
                        break;
                case 37: /* Mirror Guard */
                        set_tim_reflect(p_ptr->tim_reflect + 10 + randint(10) + to_s2);
                        break;
                case 38: /* Flare Gaze */
                        if (!get_aim_dir(&dir)) return;
                        (void)fire_ball_beam(GF_FIRE, dir, 100 * mto_s2, 3 + to_s2);
                        break;
                case 39: /* Force of the Elements */
                {
                        int i;

                        for (i = 1; i < 10; i++)
                        {
                                if (i-5)
                                {
                                        if(magik(50)) fire_bolt(GF_FIRE, i, damroll(15, 8 * mto_s2));
                                        if(magik(50)) fire_bolt(GF_COLD, i, damroll(14, 8 * mto_s2));
                                        if(magik(50)) fire_bolt(GF_ELEC, i, damroll(12, 8 * mto_s2));
                                        if(magik(50)) fire_bolt(GF_ACID, i, damroll(18, 8 * mto_s2));
                                }
                        }
                        break;
                }

                case 40: /* Earthquake */
                        earthquake(py, px, 5 + to_s2);
                        break;
                case 41: /* Polymorph */
			if (!get_aim_dir(&dir)) return;
			(void)poly_monster(dir);
                        break;
                case 42: /* Wall of Stone */
                        if (!get_aim_dir(&dir)) return;
                        project_hook(GF_STONE_WALL, dir, 1, PROJECT_BEAM | PROJECT_KILL | PROJECT_GRID);
                        break;
                case 43: /* Warp Space */
                        fire_ball(GF_GRAVITY, 0, 60 * mto_s2, 4 + to_s2);
                        break;
                case 44: /* Chaos Blast */
                        fire_ball(GF_CHAOS, 0, (100 + plev) * mto_s2, 5 + to_s2);
                        break;
                case 45: /* Lava Flow */
                        if (!get_aim_dir(&dir)) return;
                        project_hook(GF_LAVA_FLOW, dir, 5 + (plev / 7) + to_s2, PROJECT_BEAM | PROJECT_KILL | PROJECT_GRID);
                        break;
                case 46: /* Pyrrhic Blast */
                        fire_ball(GF_MANA, 0, 600 * mto_s2, 3 + to_s2);
                        take_hit(200, "the heat of a Pyrrhic Blast");
                        break;
                case 47: /* Word of Destruction */
                        destroy_area(py, px, 15+(p_ptr->to_s*2), TRUE);
                        break;

                case 48: /* Radiate Fear */
                        fire_ball(GF_FEAR, 0, 20 + to_s2, 10 + to_s2);
                        break;
                case 49: /* Probing */
                        probing();
                        break;
                case 50: /* Forcefull Graze */
                        if (!get_aim_dir(&dir)) return;
                        (void)fire_bolt(GF_STUN, dir, 50 + to_s2);
                        break;
                case 51: /* Recharging II */
                        recharge(40 + p_ptr->to_s);
                        break;
                case 52: /* Transmutation */
                        alchemy();
                        break;
                case 53: /* Self Scan */
                        self_knowledge(NULL);
                        identify_pack();
                        p_ptr->notice |= PN_COMBINE | PN_REORDER;
                        break;
                case 54: /* Id II */
                        identify_fully();
                        break;
                case 55: /* Clairvoyance */
                        wiz_lite_extra();
                        break;

                case 56: /* Volcano Flow */
                        if (!get_aim_dir(&dir)) return;
                        project_hook(GF_LAVA_FLOW, dir, 9 + (plev / 7) + to_s2, PROJECT_BEAM | PROJECT_KILL | PROJECT_GRID);
                        (void)fire_beam(GF_PLASMA, dir, damroll(15, 7 * mto_s2));
                        (void)fire_beam(GF_PLASMA, dir, damroll(15, 7 * mto_s2));
                        break;
                case 57: /* Plasma Eruption */
                        fire_ball(GF_PLASMA, 0, 400 * mto_s2, 6 + to_s2);
                        break;
                case 58: /* Annihilate */
                        if (!get_aim_dir(&dir)) return;
                        (void)fire_bolt(GF_MISSILE, dir, 500 * mto_s2);
                        break;
                case 59: /* Olbivion Blast */
                        mass_genocide(TRUE);
                        break;
                case 60: /* Mana Spin */
                {
                        int i;

                        for (i = 1; i < 10; i++)
                        {
                                if (i-5)
                                {
                                        fire_bolt(GF_MANA, i, damroll(10, 10 * mto_s2));
                                        fire_bolt(GF_MANA, i, damroll(10, 10 * mto_s2));
                                        fire_bolt(GF_MANA, i, damroll(10, 10 * mto_s2));
                                }
                        }
                        break;
                }
                case 61: /* Tidal Wave */
                        if (!get_aim_dir(&dir)) return;
                        (void)fire_ball(GF_WATER, dir, 450 * mto_s2, 3 + to_s2);
                        break;
                case 62: /* Anarchy Strike */
                {
                        int i, j;

                        for(j = 0; j < 5; j++)
                        {
                                i = randint(10);
                                while(i == 5) i = randint(10);
                                (void)fire_ball(GF_CHAOS, i, 250 * mto_s2, 3 + to_s2);
                        }
                        break;
                }
                case 63: /* Mana Strike */
                        if (!get_aim_dir(&dir)) return;
                        (void)fire_ball(GF_MANA, dir, 1200 * mto_s2, 0);
                        break;
           default:
                   msg_format("You cast an unknown Magery spell: %d.", spell);
		   msg_print(NULL);
	   }
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

static void cast_shadow_spell(int spell)
{
        int     dir, beam;
        int     plev = p_ptr->lev;
        int to_s2=p_ptr->to_s;
        int mto_s2=p_ptr->to_s;

        mto_s2 = (mto_s2==0)?1:mto_s2;

	if (p_ptr->pclass == CLASS_MAGE) beam = plev;
	else if (p_ptr->pclass == CLASS_HIGH_MAGE) beam = plev + 10;
	else beam = plev / 2;

	switch (spell)
	{
                case 0: /* Dark Hand */
                        if (!get_aim_dir(&dir)) return;
                        fire_bolt_or_beam(beam-10, GF_DARK, dir,
                               damroll(3 + ((plev - 1) / 5), 4 * mto_s2));
                        break;
                case 1: /* Sense unseen */
                        detect_monsters_invis();
                        break;
                case 2: /* dark light */
                        lite_room(py, px);
                        break;
                case 3: /* armor of void */
                        set_shield(p_ptr->shield + 10 + randint(5) + to_s2, 10 + to_s2);
                        break;
                case 4: /* fear of the shadows */
                        if (!get_aim_dir(&dir)) return;
                        fear_monster(dir, plev * mto_s2);
                        break;
                case 5: /* sense shadows */
                        detect_objects_normal();
                        break;
                case 6: /* shadow spikes */
                {
                        int i;

                        for (i = 1; i < 10; i++)
                        {
                                if (i - 5) fire_beam(GF_DARK, i, (6 + plev) * mto_s2);
                        }
                        break;
                }
                case 7: /* shadow hidding */
                        set_invis(p_ptr->tim_invis + 5 + randint(10) + p_ptr->to_s, 30);
                        break;

                case 8: /* Illusory Ball */
                        if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_OLD_CONF, dir, 13 * mto_s2, 2 + (plev / 20) + to_s2);
                        break;
                case 9: /* Darkness Storm */
                        fire_ball(GF_DARK, 0, (20 + plev) * mto_s2, 3 + (plev / 20) + to_s2);
                        break;
                case 10: /* Energies Channeling */
                        map_area();
                        break;
                case 11: /* Immaterial Beings */
                        summon_specific_friendly(py, px, plev + to_s2, SUMMON_GHOST, TRUE);
                        break;
                case 12: /* Shadow Enlightment */
                        ident_spell();
                        break;
                case 13: /* Disolve Mater */
                        if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_FORCE, dir, (50 + plev) * mto_s2, 0 + to_s2);
                        break;
                case 14: /* Invoke the Night */
                        if (!get_aim_dir(&dir)) return;
                        sleep_monster(dir);
                        break;
                case 15: /* Resist L&D */
                        set_oppose_ld(p_ptr->oppose_ld + 10 + randint(10) + to_s2);
                        break;

                case 16: /* Teleportation */
                        teleport_player(200 + p_ptr->to_s);
                        break;
                case 17: /* Absorb Light */
                {
                        int y, x, light = 0;
                        cave_type *c_ptr;

                        for(y = py - 6; y <= py + 6; y++)
                        {
                                for(x = px - 6; x <= px + 6; x++)
                                {
                                        if(!in_bounds(y, x)) continue;

                                        c_ptr = &cave[y][x];

                                        if (distance(y, x, py, px) > 6) continue;

                                        if (c_ptr->info & CAVE_GLOW)
                                        {
                                                light++;

                                                /* No longer in the array */
                                                c_ptr->info &= ~(CAVE_TEMP);

                                                /* Darken the grid */
                                                c_ptr->info &= ~(CAVE_GLOW);

                                                /* Hack -- Forget "boring" grids */
                                                if ((f_info[c_ptr->feat].flags1 & FF1_FLOOR) && !(f_info[c_ptr->feat].flags1 & FF1_REMEMBER))
                                                {
                                                        /* Forget the grid */
                                                        c_ptr->info &= ~(CAVE_MARK);

                                                        /* Notice */
                                                        note_spot(y, x);
                                                }

                                                /* Process affected monsters */
                                                if (c_ptr->m_idx)
                                                {
                                                        /* Update the monster */
                                                        update_mon(c_ptr->m_idx, FALSE);
                                                }

                                                /* Redraw */
                                                lite_spot(y, x);
                                        }
                                }
                        }
                        msg_print("The light around you is absorbed... and transformed into pure mana!");
                        p_ptr->csp += light * (5 + to_s2);
                        if (p_ptr->csp > p_ptr->msp)
			{
				p_ptr->csp = p_ptr->msp;
				p_ptr->csp_frac = 0;
				p_ptr->redraw |= (PR_MANA);
				p_ptr->window |= (PW_PLAYER);
				p_ptr->window |= (PW_SPELL);
			}
                        msg_print("Your feel your head clearer.");
                        break;
                }
                case 18: /* Shadow Regeneration */
                        hp_player(damroll(5 + (plev / 5), 8 + to_s2));
                        break;
                case 19: /* Steal Shadow */
                        dispel_monsters((60 + plev) * mto_s2);
                        take_hit(60, "the stealing of your shadow");
                        break;
                case 20: /* Storm in the Shadow */
                        project_meteor(3, GF_DARK, damroll(5 + to_s2, 20), PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | PROJECT_THRU);
                        break;
                case 21: /* Shadow of Life */
                {
                        int item, x, y;
                        object_type *o_ptr;

                        cptr q, s;

                        /* Restrict choices to corpses */
                        item_tester_tval = TV_CORPSE;

                        /* Get an item */
                        q = "Use which corpse? ";
                        s = "You have no corpse to use.";
                        if (!get_item(&item, q, s, (USE_FLOOR))) break;;

                        o_ptr = &o_list[0 - item];

                                if(randint(8)>=5-o_ptr->sval){
                                        msg_print("You touch the corpse ... the monster raise from the graveyard!");

                                        x=px;
                                        y=py;
                                        get_pos_player(5, &y, &x);
                                        place_monster_one(y, x, o_ptr->pval2, FALSE, TRUE);

                                        floor_item_increase(0 - item, -1);
                                        floor_item_describe(0 - item);
                                        floor_item_optimize(0 - item);
                                }
                        break;
                }
                case 22: /* Shadow of Dragon */
                        summon_specific_friendly(py, px, plev + to_s2, SUMMON_DRAGON, TRUE);
                        break;
                case 23: /* Banish Shadows */
                        dispel_monsters(180 * mto_s2);
                        break;

                case 24: /* Feelings of Darkness */
			probing();
                        break;
                case 25: /* Dark Bolt */
                        if (!get_aim_dir(&dir)) return;
                        fire_bolt(GF_DARK, dir, (230 + plev) * mto_s2);
                        break;
                case 26: /* Shadow orb */
                        if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_DARK, dir, (200 + plev) * mto_s2, 2 + to_s2);
                        break;
                case 27: /* Shadow of Undead */
                        summon_specific_friendly(py, px, plev + to_s2, SUMMON_UNDEAD, TRUE);
                        break;
                case 28: /* Shadow Rain */
                {
                        int y, x, i;

                        for(i = 0; i < 500 + (20 * to_s2); i++)
                        {
                                y = randint(cur_hgt) - 1;
                                x = randint(cur_wid) - 1;

                                project(0, 1, y, x, 50 * mto_s2, GF_DARK,
                                     PROJECT_STOP | PROJECT_KILL | PROJECT_GRID | PROJECT_ITEM | PROJECT_JUMP);
                        }
                        break;
                }
                case 29: /* Mirror of Shadow */
                        set_tim_reflect(p_ptr->tim_reflect + 5 + randint(10) + to_s2);
                        break;
                case 30: /* Comet from the Void */
                        if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_FORCE, dir, 400 * mto_s2, 2 + to_s2);
                        break;
                case 31: /* Call the Void */
                        call_the_();
                        break;

                case 32: /* Examin Shadow */
                {
                        monster_type *m_ptr;
                        monster_race *r_ptr;
                        int m;

                        msg_print("You recall legends of the fair and foul creatures of the world...");

                        if (!get_rep_dir(&dir)) break;
                        if (!cave[py + ddy[dir]][px + ddx[dir]].m_idx) break;
                        m_ptr = &m_list[cave[py + ddy[dir]][px + ddx[dir]].m_idx];
                        r_ptr = &r_info[m_ptr->r_idx];

                        msg_print("Now you have a better knowledge of this monster.");

                        r_ptr->r_wake = r_ptr->r_ignore = MAX_UCHAR;

                        /* Observe "maximal" attacks */
                        for (m = 0; m < 4; m++)
                        {
                        /* Examine "actual" blows */
			if (r_ptr->blow[m].effect || r_ptr->blow[m].method)
			{
				/* Hack -- maximal observations */
				r_ptr->r_blows[m] = MAX_UCHAR;
			}
                        }

                        /* Hack -- maximal drops */
                        r_ptr->r_drop_gold = r_ptr->r_drop_item =
                        (((r_ptr->flags1 & (RF1_DROP_4D2)) ? 8 : 0) +
                         ((r_ptr->flags1 & (RF1_DROP_3D2)) ? 6 : 0) +
                         ((r_ptr->flags1 & (RF1_DROP_2D2)) ? 4 : 0) +
                         ((r_ptr->flags1 & (RF1_DROP_1D2)) ? 2 : 0) +
                         ((r_ptr->flags1 & (RF1_DROP_90))  ? 1 : 0) +
                         ((r_ptr->flags1 & (RF1_DROP_60))  ? 1 : 0));

                        /* Hack -- but only "valid" drops */
                        if (r_ptr->flags1 & (RF1_ONLY_GOLD)) r_ptr->r_drop_item = 0;
                        if (r_ptr->flags1 & (RF1_ONLY_ITEM)) r_ptr->r_drop_gold = 0;

                        /* Hack -- observe many spells */
                        r_ptr->r_cast_inate = MAX_UCHAR;
                        r_ptr->r_cast_spell = MAX_UCHAR;

                        /* Hack -- know all the flags */
                        r_ptr->r_flags1 = r_ptr->flags1;
                        r_ptr->r_flags2 = r_ptr->flags2;
                        r_ptr->r_flags3 = r_ptr->flags3;
                        r_ptr->r_flags4 = r_ptr->flags4;
                        r_ptr->r_flags5 = r_ptr->flags5;
                        r_ptr->r_flags6 = r_ptr->flags6;

                       break;
                }
                case 33: /* *Shadow Enlightment* */
                        identify_fully();
                        break;
                case 34: /* Remove Curse */
                        remove_curse();
                        break;
                case 35: /* *Remove Curse* */
                        remove_all_curse();
                        break;
                case 36: /* Shadow Form */
                        set_shadow(p_ptr->wraith_form + randint(plev/2) + (plev/2) + to_s2);
                        break;

                case 37: /* Shadow Portal */
                        teleport_player(10 + to_s2);
                        break;
                case 38: /* Warping Rift */
                        fire_ball(GF_GRAVITY, 0, 100 * mto_s2, 4 + to_s2);
                        break;
                case 39: /* Void Jumping */
                        recall_player();
                        break;
                case 40: /* Shadow Distortion */
                        if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_GRAVITY, dir, 200 * mto_s2, 3 + to_s2);
                        break;
                case 41: /* Between Jump */
                {
                        int ij, ii;

                        if(special_flag){msg_print("Not on special levels!");break;}

                        msg_print("You open a between gate. Choose a destination.");
                        if (!tgt_pt(&ii,&ij)) break;
                        p_ptr->energy -= 60 - plev;
                        if (!cave_empty_bold(ij,ii) || (cave[ij][ii].info & CAVE_ICKY) ||
                        (distance(ij,ii,py,px) > plev + 2) ||
                        (!rand_int(plev * plev / 2)))
                        {
                                msg_print("You fail to exit the between correctly!");
                                p_ptr->energy -= 100;
                                get_pos_player(10,&ij,&ii);
                        }
                        cave_set_feat(py,px,FEAT_BETWEEN);
                        cave_set_feat(ij,ii,FEAT_BETWEEN);
                        cave[py][px].special = ii + (ij << 8);
                        cave[ij][ii].special = px + (py << 8);
                        break;
                }
                case 42: /* Between Maze */
                        fire_ball(GF_BETWEEN_GATE, 0, 1, 2 + to_s2);
                        break;

                case 43: /* Dispel Living */
                        dispel_living((60 + plev) * mto_s2);
                        break;
                case 44: /* Conjure Dragons */
                {
                        int i;

                        for(i = 0; i < 5 + to_s2; i++);
                                summon_specific_friendly(py, px, plev + to_s2, SUMMON_DRAGON, TRUE);
                        break;
                }
                case 45: /* Dispel Undead */
                        dispel_undead((120 + plev) * mto_s2);
                        break;
                case 46: /* Conjure Undeads */
                {
                        int i;

                        for(i = 0; i < 5 + to_s2; i++);
                                summon_specific_friendly(py, px, plev + to_s2, SUMMON_UNDEAD, TRUE);
                        break;
                }
                case 47: /* Dispel Demon */
                        dispel_demons((180 + plev) * mto_s2);
                        break;
                case 48: /* Conjure Demons */
                {
                        int i;

                        for(i = 0; i < 5 + to_s2; i++);
                                summon_specific_friendly(py, px, plev + to_s2, SUMMON_DEMON, TRUE);
                        break;
                }
                        break;

                case 49: /* Recharge I */
                        recharge(20 + p_ptr->to_s);
                        break;
                case 50: /* Sphere of Void */
                        fire_ball(GF_DISINTEGRATE, 0, 320 * mto_s2, 3 + to_s2);
                        break;
                case 51: /* Shadow Alteration */
                        alter_reality();
                        break;
                case 52: /* Recharge II */
                        recharge(40 + p_ptr->to_s);
                        break;
                case 53: /* Concentrate Light */
                {
                        int y, x, light = 0;
                        cave_type *c_ptr;

                        for(y = py - 6; y <= py + 6; y++)
                        {
                                for(x = px - 6; x <= px + 6; x++)
                                {
                                        if(!in_bounds(y, x)) continue;

                                        c_ptr = &cave[y][x];

                                        if (distance(y, x, py, px) > 6) continue;

                                        if (c_ptr->info & CAVE_GLOW)
                                        {
                                                light++;

                                                /* No longer in the array */
                                                c_ptr->info &= ~(CAVE_TEMP);

                                                /* Darken the grid */
                                                c_ptr->info &= ~(CAVE_GLOW);

                                                /* Hack -- Forget "boring" grids */
                                                if ((f_info[c_ptr->feat].flags1 & FF1_FLOOR) && !(f_info[c_ptr->feat].flags1 & FF1_REMEMBER))
                                                {
                                                        /* Forget the grid */
                                                        c_ptr->info &= ~(CAVE_MARK);

                                                        /* Notice */
                                                        note_spot(y, x);
                                                }

                                                /* Process affected monsters */
                                                if (c_ptr->m_idx)
                                                {
                                                        /* Update the monster */
                                                        update_mon(c_ptr->m_idx, FALSE);
                                                }

                                                /* Redraw */
                                                lite_spot(y, x);
                                        }
                                }
                        }
                        if (!get_aim_dir(&dir)) break;
                        msg_print("The light around you is absorbed... and released in a powerfull bolt!");
                        fire_bolt(GF_LITE, dir, damroll(light * mto_s2, p_ptr->lev));
                        break;
                }
                        break;
                case 54: /* Shield of Darkness */
                        (void)set_shield(p_ptr->shield + randint(20) + 30 + to_s2, 50 + to_s2);
                        break;
                case 55: /* Tunnel of Shadow */
                {
                        int min = ((dun_level - 5 - to_s2) < d_info[dungeon_type].mindepth)?d_info[dungeon_type].mindepth:(dun_level - 5 - to_s2);
                        int max = ((dun_level + 5 + to_s2) < d_info[dungeon_type].maxdepth)?d_info[dungeon_type].maxdepth:(dun_level + 5 + to_s2);
                        int dest = dun_level;

                        /* Ask for level */
                        {
                                char    ppp[80];

                                char    tmp_val[160];

                                /* Prompt */
                                sprintf(ppp, "Jump to level (%d-%d): ", min, max);

                                /* Default */
                                sprintf(tmp_val, "%d", dun_level);

                                /* Ask for a level */
                                if (!get_string(ppp, tmp_val, 10)) return;

                                /* Extract request */
                                dest = atoi(tmp_val);
                        }

                        /* Paranoia */
                        if (dest < 0) dest = 0;

                        /* Paranoia */
                        if (dest > d_info[dungeon_type].maxdepth) dest = d_info[dungeon_type].maxdepth;

                        /* Accept request */
                        msg_format("Atunnel of shadows is open to the level %d.", dest);

                        if (autosave_l)
                        {
                                is_autosave = TRUE;
                                msg_print("Autosaving the game...");
                                do_cmd_save_game();
                                is_autosave = FALSE;
                        }

                        /* Change level */
                        dun_level = dest;

                        p_ptr->inside_arena = 0;
                        leaving_quest = p_ptr->inside_quest;

                        /* Leaving an 'only once' quest marks it as failed */
                        if (leaving_quest &&
                                (quest[leaving_quest].flags & QUEST_FLAG_ONCE) &&
                                (quest[leaving_quest].status == QUEST_STATUS_TAKEN))
                        {
                                quest[leaving_quest].status = QUEST_STATUS_FAILED;
                        }

                        p_ptr->inside_quest = 0;
                        p_ptr->leftbldg = FALSE;

                        /* Leaving */
                        p_ptr->leaving = TRUE;
                        break;
                }
                case 56: /* Genocide */
                        genocide(TRUE);
                        break;
                case 57: /* Enslave Undead */
                        fire_ball(GF_CONTROL_UNDEAD, 0, 150 * mto_s2, 4 + to_s2);
                        break;
                case 58: /* Create Minor Ring */
        {
                int item, die = randint(100);
                object_type             *o_ptr;
                cptr q, s;

                /* Restrict choices to amulets */
                item_tester_tval = TV_RING;

                /* Get an item */
                q = "Use which ring? ";
                s = "You have no ring to use.";
                if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

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

                if(o_ptr->sval != SV_RING_NOTHING)
                {
                        msg_print("You must use a Ring of Nothing.");
                        break;
                }

                if(die <= 2)
                {
                        o_ptr->sval = SV_RING_TELEPORTATION;
                        apply_magic(o_ptr, dun_level, TRUE, TRUE, FALSE, FALSE);
                }
                else if(die <= 12)
                {
                        o_ptr->sval = SV_RING_FEATHER_FALL;
                        apply_magic(o_ptr, dun_level, TRUE, TRUE, FALSE, FALSE);
                }
                else if(die <= 22)
                {
                        o_ptr->sval = SV_RING_PROTECTION;
                        apply_magic(o_ptr, dun_level, TRUE, TRUE, FALSE, FALSE);
                }
                else if(die <= 32)
                {
                        o_ptr->sval = SV_RING_FREE_ACTION;
                        apply_magic(o_ptr, dun_level, TRUE, TRUE, FALSE, FALSE);
                }
                else if(die <= 42)
                {
                        o_ptr->sval = SV_RING_DAMAGE;
                        apply_magic(o_ptr, dun_level, TRUE, TRUE, FALSE, FALSE);
                }
                else if(die <= 52)
                {
                        o_ptr->sval = SV_RING_SLAYING;
                        apply_magic(o_ptr, dun_level, TRUE, TRUE, FALSE, FALSE);
                }
                else if(die <= 62)
                {
                        o_ptr->sval = SV_RING_SPEED;
                        apply_magic(o_ptr, dun_level, TRUE, TRUE, FALSE, FALSE);
                }
                else if(die <= 67)
                {
                        o_ptr->sval = SV_RING_RES_NEXUS;
                        apply_magic(o_ptr, dun_level, TRUE, TRUE, FALSE, FALSE);
                }
                else if(die <= 77)
                {
                        o_ptr->sval = SV_RING_RES_SHARDS;
                        apply_magic(o_ptr, dun_level, TRUE, TRUE, FALSE, FALSE);
                }
                else if(die <= 78)
                {
                        o_ptr->sval = SV_RING_ATTACKS;
                        apply_magic(o_ptr, dun_level, TRUE, TRUE, FALSE, FALSE);
                }
                else if(die <= 88)
                {
                        o_ptr->sval = SV_RING_INVIS;
                        apply_magic(o_ptr, dun_level, TRUE, TRUE, FALSE, FALSE);
                }
                else if(die <= 98)
                {
                        o_ptr->sval = SV_RING_FLYING;
                        apply_magic(o_ptr, dun_level, TRUE, TRUE, FALSE, FALSE);
                }
                else if(die <= 100)
                {
                        o_ptr->sval = SV_RING_PRECONITION;
                        apply_magic(o_ptr, dun_level, TRUE, TRUE, FALSE, FALSE);
                }
                o_ptr->k_idx = lookup_kind(o_ptr->tval, o_ptr->sval);

                p_ptr->notice |= (PN_COMBINE | PN_REORDER);
                p_ptr->window |= (PW_INVEN);
                break;
        }
                case 59: /* Control the Three */
                        if (check_ring(ART_NARYA) && check_ring(ART_NENYA) && check_ring(ART_VILYA))
                        {
                                msg_print("The power of the Elven Rings is released! You are now protected from the passing of the Time.");
                                set_tim_res_time(p_ptr->tim_res_time + 20 + randint(30) + (p_ptr->to_s * 2));
                                restore_level();
                        }
                        else
                                msg_print("You must wear two of the Three Rings of Power(Narya, Nenya and Vilya) to use this spell.");
                        break;
                case 60: /* Protection from Undeads */
                        if(p_ptr->black_breath)
                        {
                                msg_print("You are cured from the Black Breath!");
                                p_ptr->black_breath = FALSE;
                        }
                        set_protundead(p_ptr->protundead + 20 + randint(30) + to_s2);
                        break;
                case 61: /* Mass Genocide */
                        mass_genocide(TRUE);
                        break;
                case 62: /* Hellfire */
			if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_HELL_FIRE, dir,
                                666 * mto_s2, 3 + to_s2);
                                take_hit(50+randint(50), "the strain of casting Hellfire");
                        break;
                case 63: /* Control The Ring */
                        if(check_ring(ART_POWER))
                        {
                                fire_ball(GF_MANA, 0, 2000 * mto_s2, 5 + to_s2);
                                if (!get_aim_dir(&dir)) return;
                                fire_ball(GF_DARK, dir, 1500 * mto_s2, 2 + to_s2);
                                if (!get_aim_dir(&dir)) return;
                                fire_bolt(GF_FORCE, dir, 1000 * mto_s2);
                        }
                        else
                                msg_print("You must wear The One Ring, The Ring of Power to use this spell.");
                        break;

                default:
                        msg_format("You cast an unknown Shadow spell: %d.", spell);
                        msg_print(NULL);
                        break;
        }
}


static void cast_chaos_spell(int spell)
{
	int	dir, i, beam;
	int	plev = p_ptr->lev;
        int to_s2=p_ptr->to_s;
        int mto_s2=p_ptr->to_s;

        mto_s2 = (mto_s2==0)?1:mto_s2;

	if (p_ptr->pclass == CLASS_MAGE) beam = plev;
	else if (p_ptr->pclass == CLASS_HIGH_MAGE) beam = plev + 10;
	else beam = plev / 2;

	switch (spell)
	{
		case 0: /* Magic Missile */
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam-10, GF_MISSILE, dir,
                                                  damroll(3 + ((plev - 1) / 5) * mto_s2, 4));
                break;
        case 1: /* Trap / Door destruction, was: Blink */
			(void)destroy_doors_touch();
			break;
        case 2: /* Flash of Light == Light Area */
                        (void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1 + to_s2);
			break; 
        case 3: /* Touch of Confusion */
            if (!(p_ptr->confusing))
            {
                msg_print("Your hands start glowing.");
                p_ptr->confusing = TRUE;
            }
			break;
       case 4: /* Manaburst */
             if (!get_aim_dir(&dir)) return;
             fire_ball(GF_MISSILE, dir,
            (damroll(3, 5) + plev +
             (plev / (((p_ptr->pclass == CLASS_MAGE)
                || (p_ptr->pclass == CLASS_HIGH_MAGE)) ? 2 : 4))) * mto_s2,
            ((plev < 30) ? 2 : 3)+to_s2);
          /* Shouldn't actually use GF_MANA, as it will destroy all
       * items on the floor */
             break;
        case 5: /* Fire Bolt */
			if (!get_aim_dir(&dir)) return;
			fire_bolt_or_beam(beam, GF_FIRE, dir,
                                damroll(8+((plev-5)/4), 8)*mto_s2);
			break;
        case 6: /* Fist of Force ("Fist of Fun") */
			if (!get_aim_dir(&dir)) return;
           fire_ball(GF_DISINTEGRATE, dir,
               damroll(8+((plev-5)/4), 8)*mto_s2, to_s2);
            break;
		case 7: /* Teleport Self */
                        teleport_player(plev * 5*mto_s2);
			break;
        case 8: /* Wonder */
           {
           /* This spell should become more useful (more
              controlled) as the player gains experience levels.
              Thus, add 1/5 of the player's level to the die roll.
              This eliminates the worst effects later on, while
              keeping the results quite random.  It also allows
              some potent effects only at high level. */

               int die = randint(100) + plev / 5 + p_ptr->to_s;

               if (!get_aim_dir(&dir)) return;
               if (die > 100)
                   msg_print ("You feel a surge of power!");
               if (die < 8) clone_monster (dir);
               else if (die < 14) speed_monster (dir);
               else if (die < 26) heal_monster (dir);
               else if (die < 31) poly_monster (dir);
               else if (die < 36)
                   fire_bolt_or_beam (beam - 10,
                   GF_MISSILE, dir,
                   damroll(3 + ((plev - 1) / 5), 4)*mto_s2);
               else if (die < 41) confuse_monster (dir, plev*mto_s2);
               else if (die < 46) fire_ball (GF_POIS, dir, 20 + (plev / 2)*mto_s2, 3+to_s2);
               else if (die < 51) lite_line (dir);
               else if (die < 56)
                   fire_bolt_or_beam (beam - 10, GF_ELEC, dir,
                   damroll(3+((plev-5)/4),8)*mto_s2);
               else if (die < 61)
                   fire_bolt_or_beam (beam - 10, GF_COLD, dir,
                   damroll(5+((plev-5)/4),8)*mto_s2);
               else if (die < 66)
                   fire_bolt_or_beam (beam, GF_ACID, dir,
                   damroll(6+((plev-5)/4),8)*mto_s2);
               else if (die < 71)
                   fire_bolt_or_beam (beam, GF_FIRE, dir,
                   damroll(8+((plev-5)/4),8)*mto_s2);
               else if (die < 76) drain_life (dir, 75+(p_ptr->to_s*2));
               else if (die < 81) fire_ball (GF_ELEC, dir, 30 + plev*mto_s2 / 2, 2+to_s2);
               else if (die < 86) fire_ball (GF_ACID, dir, 40 + plev*mto_s2, 2+to_s2);
               else if (die < 91) fire_ball (GF_ICE, dir, 70 + plev*mto_s2, 3+to_s2);
               else if (die < 96) fire_ball (GF_FIRE, dir, 80 + plev*mto_s2, 3+to_s2);
               else if (die < 101) drain_life (dir, 100 + plev+to_s2);
               else if (die < 104) earthquake (py, px, 12+to_s2);
               else if (die < 106) destroy_area (py, px, 15, TRUE);
               else if (die < 108) genocide(TRUE);
               else if (die < 110) dispel_monsters (120+(p_ptr->to_s*2));
               else /* RARE */
               {
                   dispel_monsters (150+(p_ptr->to_s*2));
                   slow_monsters();
                   sleep_monsters();
                   hp_player (300+(p_ptr->to_s*3));
               }
               break;
           }
			break;
		case 9: /* Chaos Bolt */
			if (!get_aim_dir(&dir)) return;
			fire_bolt_or_beam(beam, GF_CHAOS, dir,
				damroll(10+((plev-5)/4), 8));
			break;
        case 10: /* Sonic Boom */
               msg_print("BOOM! Shake the room!");
                   project(0, 2+plev/10, py, px,
               45+plev*mto_s2, GF_SOUND, PROJECT_KILL|PROJECT_ITEM);
                   break;
        case 11: /* Doom Bolt -- always beam in 2.0.7 or later */
				if (!get_aim_dir(&dir)) return;
                fire_beam(GF_MANA, dir, damroll(11+((plev-5)/4), 8)*mto_s2);
			break;
		case 12: /* Fire Ball */
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_FIRE, dir,
                                        55 + (plev)*mto_s2, 2+to_s2);
			break;
		case 13: /* Teleport Other */
           if (!get_aim_dir(&dir)) return;
               (void)fire_beam(GF_AWAY_ALL, dir, plev*mto_s2);
			break;
		case 14: /* Word of Destruction */
                        destroy_area(py, px, 15+(p_ptr->to_s*2), TRUE);
			break;
                case 15: /* Invoke chaos */
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_CHAOS, dir,
                                        66 + (plev)*mto_s2, (plev / 5)+to_s2);
			break;
        case 16: /* Polymorph Other */
			if (!get_aim_dir(&dir)) return;
			(void)poly_monster(dir);
			break;
        case 17: /* Chain Lightning */
          for (dir = 0; dir <= 9; dir++)
            fire_beam(GF_ELEC, dir, damroll(5+(plev/10), 8)*mto_s2);
           break;
        case 18: /* Arcane Binding == Charging */
                        (void)recharge(40+to_s2);
			break;
        case 19: /* Disintegration */
			if (!get_aim_dir(&dir)) return;
           fire_ball(GF_DISINTEGRATE, dir,
               80 + (plev)*mto_s2, 3 + (plev/40)+to_s2);
               break;
            break;
        case 20: /* Alter Reality */
			msg_print("The world changes!");
                if (autosave_l)
                {
                    is_autosave = TRUE;
                    msg_print("Autosaving the game...");
                    do_cmd_save_game();
                    is_autosave = FALSE;
                }
			/* Leaving */
			p_ptr->leaving = TRUE;

			break;
        case 21: /* Polymorph Self */
            do_poly_self();
	    break;
        case 22: /* Chaos Branding */
		brand_weapon(1);
		break;
        case 23: /* Summon monster, demon */
		if (randint(3) == 1)
		{
                        if (summon_specific(py, px, (plev*3)*mto_s2/2, SUMMON_DEMON))
			{
				msg_print("The area fills with a stench of sulphur and brimstone.");
				msg_print("'NON SERVIAM! Wretch! I shall feast on thy mortal soul!'");
			}
        	}
		else
		{
                        if (summon_specific_friendly(py, px, (plev*3)*mto_s2/2,
				SUMMON_DEMON, (plev == 50 ? TRUE : FALSE)))
			{
				msg_print("The area fills with a stench of sulphur and brimstone.");
				msg_print("'What is thy bidding... Master?'");
			}
		}
		break;
        case 24: /* Beam of Gravity */
			if (!get_aim_dir(&dir)) return;
                fire_beam(GF_GRAVITY, dir, damroll(9+((plev-5)/4), 8)*mto_s2);
            break;
        case 25: /* Meteor Swarm  */
#if 1
           {
		       int x, y, dx, dy, d, count = 0;
		       int b = 10 + randint(10); 
		       for (i = 0; i < b; i++) {
			   do {
			       count++;
			       if (count > 1000)  break;
			       x = px - 5 + randint(10);
			       y = py - 5 + randint(10);
			       dx = (px > x) ? (px - x) : (x - px);
			       dy = (py > y) ? (py - y) : (y - py);
			       /* Approximate distance */
                   d = (dy > dx) ? (dy + (dx>>1)) : (dx + (dy>>1));
               } while ((d > 5) || (!(player_has_los_bold(y, x))));
			   
			   if (count > 1000)   break;
			   count = 0;
               project(0, 2, y, x, (plev*3+to_s2)/2, GF_METEOR, PROJECT_KILL|PROJECT_JUMP|PROJECT_ITEM);
		       }
		   }
	           break;
#else
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_METEOR, dir,
                                65 + (plev)*mto_s2, 3 + (plev/40)+to_s2);
			break;
#endif
		case 26: /* Flame Strike */
			fire_ball(GF_FIRE, 0,
                150 + (2*plev)*mto_s2, 8+to_s2);
			break;
        case 27: /* Call Chaos */
            call_chaos();
			break;
        case 28: /* Magic Rocket */
			if (!get_aim_dir(&dir)) return;
            msg_print("You launch a rocket!");
			fire_ball(GF_ROCKET, dir,
                                        120 + (plev)*mto_s2, 2+to_s2);
			break;
        case 29: /* Mana Storm */
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_MANA, dir,
                                300 + (plev * 2)*mto_s2, 4+to_s2);
            break;
        case 30: /* Breathe chaos */
               if (!get_aim_dir(&dir)) return;
               fire_ball(GF_CHAOS,dir,p_ptr->chp*mto_s2,
                     2+to_s2);
               break;
                case 31: /* Call the Void */
			call_the_();
			break;
	default:
		msg_format("You cast an unknown Chaos spell: %d.", spell);
		msg_print(NULL);
	}
}


static void cast_nether_spell(int spell)
{
        int     dir;
	int	beam;
	int	plev = p_ptr->lev;
        int to_s2=p_ptr->to_s;
        int mto_s2=p_ptr->to_s;
        bool no_trump = FALSE;

        mto_s2 = (mto_s2==0)?1:mto_s2;

	if (p_ptr->pclass == CLASS_MAGE) beam = plev;
	else if (p_ptr->pclass == CLASS_HIGH_MAGE) beam = plev + 10;
        else beam = plev / 2;

	switch (spell)
	{
       case 0: /* Detect Undead & Demons -> Unlife*/
       (void) detect_monsters_nonliving();
		       break;
       case 1: /* Detect Evil */
			(void)detect_monsters_evil();
         break;
       case 2: /* Horrify */
			if (!get_aim_dir(&dir)) return;
                        (void)fear_monster(dir, 5 + plev * mto_s2);
                        (void) stun_monster(dir, 5 + plev * mto_s2);
		       break; 
           case 3: /* Sleep I */
				if (!get_aim_dir(&dir)) return;
				(void)sleep_monster(dir);
		       break;
           case 4: /* Reveal Invisible */
                       set_tim_invis(p_ptr->tim_invis + 20 + randint(20) + p_ptr->to_s);
		       break;
           case 5: /* Lethargy */
                       if (!get_aim_dir(&dir)) return;
                       fire_bolt_or_beam(beam, GF_OLD_SLOW, dir,
                                         damroll(3+((plev-5)/4), 6) * mto_s2);
		       break;
       case 6: /* Resist Poison */
                       set_oppose_pois(p_ptr->oppose_pois + randint(20) + 20 + to_s2);
		       break;
       case 7: /* Stinkning cloud */
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_POIS, dir,
                                10 + (plev / 2)*mto_s2, 2+to_s2);
               break;

       case 8: /* Resist Cold */
               set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20 + to_s2);
               break;
       case 9: /* Black Dart */
               if (!get_aim_dir(&dir)) return;
               fire_bolt_or_beam(beam-10, GF_MISSILE, dir,
                                 damroll(4 + ((plev - 1) / 5), 6 * mto_s2));
               break;
        case 10: /* Invisibility */
        {
                int ii = p_ptr->lev/2 + randint(p_ptr->lev) + p_ptr->to_s;
                set_invis(p_ptr->tim_invisible + ii, 35);
                set_tim_invis(p_ptr->tim_invisible + ii);
                break;
        }
           case 11: /* Spear of Phantasms */
				if (!get_aim_dir(&dir)) return;
                                fire_beam(GF_TURN_ALL, dir,
                                          damroll(5 * mto_s2, 7));
         break;
       case 12: /* Levitation */
                set_tim_ffall(p_ptr->tim_ffall + 20 + randint(20) + p_ptr->to_s);
                break;
       case 13: /* Sleep II */
                       (void)sleep_monsters_touch();
		       break;
           case 14: /* Nether Bolt */
               if (!get_aim_dir(&dir)) return;
               fire_bolt_or_beam(beam, GF_NETHER, dir,
                                 damroll(5 + ((plev - 1) / 5), 8 * mto_s2));
		       break;
           case 15: /* Enslave Undead */
                        if (!get_aim_dir(&dir)) return;
                        (void)control_one_undead(dir, plev);
		       break;
           case 16: /* Icy Touch */
               if (!get_aim_dir(&dir)) return;
               fire_bolt_or_beam(beam, GF_ICE, dir,
                                 damroll(6 + (plev / 5), 8 * mto_s2));
		       break;
       case 17: /* Bloodlust */
               (void)set_shero(p_ptr->shero + randint(25) + 25);
               (void)set_blessed(p_ptr->blessed + randint(25) + 25);
       break;
       case 18: /* Werewolf Form*/
                        if(p_ptr->mimic_form != MIMIC_WEREWOLF)
                                set_mimic(25 + randint(25) + to_s2, MIMIC_WEREWOLF);
                        else
                                set_mimic(p_ptr->tim_mimic + 25 + randint(25) + to_s2, MIMIC_WEREWOLF);
       break;
       case 19: /* Draining Touch */
                   if (!get_aim_dir(&dir)) return;
                   if (drain_life(dir, (p_ptr->lev * 2 * mto_s2)))
                           hp_player(p_ptr->lev + randint(p_ptr->lev * mto_s2));
           break;
        case 20: /* Turn Undead */
				if (!get_aim_dir(&dir)) return;
                                fire_bolt_or_beam(beam, GF_TURN_UNDEAD, dir,
                                          damroll(6 * mto_s2, 8));
                break;
        case 21: /* Sleep III */
                       (void)sleep_monsters();
		       break;
       case 22: /* Call Undead*/
	{
                msg_print("You concentrate on the summoning of an undead creature...");

                if (!(summon_specific_friendly(py, px, plev+to_s2, SUMMON_UNDEAD, TRUE)))
                        no_trump = TRUE;

		break;
	}
        case 23: /* Open Chasm */
        {
                monster_type *m_ptr;
                monster_race *r_ptr;
                cave_type *c_ptr;
                int     ii = 0, ij = 0;


		if (!tgt_pt(&ii,&ij)) return;

                cave_set_feat(ij, ii, FEAT_DARK_PIT);
                msg_print("A chasm appears in the floor!");

                if(cave[ij][ii].m_idx)
                {
                        m_ptr = &m_list[cave[ij][ii].m_idx];
                        r_ptr = &r_info[m_ptr->r_idx];

                        if(r_ptr->flags7 & RF7_CAN_FLY)
                        {
                                msg_print("The monster flies over the chasm.");
                        }
                        else
                        {
                                if(!(r_ptr->flags1 & RF1_UNIQUE))
                                        msg_print("The monster falls in the chasm !");

                                delete_monster(ij, ii);
                        }
                }

                if(cave[ij][ii].o_idx)
                {
                        s16b this_o_idx, next_o_idx = 0;

                        c_ptr = &cave[ij][ii];

                        /* Scan all objects in the grid */
                        for (this_o_idx = c_ptr->o_idx; this_o_idx; this_o_idx = next_o_idx)
                        {
                                object_type *o_ptr;
                                bool plural = FALSE;

                                char o_name[80];
	
                                /* Acquire object */
                                o_ptr = &o_list[this_o_idx];

                                if(o_ptr->number > 1) plural = TRUE;

                                /* Acquire next object */
                                next_o_idx = o_ptr->next_o_idx;

                                /* Effect "observed" */
                                if (o_ptr->marked)
                                {
                                        object_desc(o_name, o_ptr, FALSE, 0);
                                }

                                /* Artifacts get to resist */
                                if (o_ptr->name1)
                                {
                                        /* Observe the resist */
                                        if (o_ptr->marked)
                                        {
                                                msg_format("The %s %s simply fly over the chasm!",
                                                           o_name, (plural ? "are" : "is"));
                                        }
                                }

                                /* Kill it */
                                else
                                {
                                        /* Delete the object */
                                        delete_object_idx(this_o_idx);

                                        /* Redraw */
                                        lite_spot(ij, ii);
                                }
                        }
               }
               break;
        }
        case 24: /* Dark Bolt */
                       if (!get_aim_dir(&dir)) return;
                       fire_bolt_or_beam(beam, GF_DARK, dir,
                                         damroll(8 * mto_s2, 8));
		       break;
       case 25: /* Vampiric Form */
                        if(p_ptr->mimic_form != MIMIC_VAMPIRE)
                                set_mimic(20 + randint(15) + to_s2, MIMIC_VAMPIRE);
                        else
                                set_mimic(p_ptr->tim_mimic + 20 + randint(15) + to_s2, MIMIC_VAMPIRE);
                        break;
        case 26: /* Raise Dead */
        {
                int item, x, y;
                object_type *o_ptr;

                cptr q, s;

                /* Restrict choices to corpses */
                item_tester_tval = TV_CORPSE;

                /* Get an item */
                q = "Use which corpse? ";
                s = "You have no corpse to use.";
                if (!get_item(&item, q, s, (USE_FLOOR))) break;;

                o_ptr = &o_list[0 - item];

                        if(randint(8)>=5-o_ptr->sval){
                                msg_print("You touch the corpse ... the monster raise from the graveyard!");

                                x=px;
                                y=py;
                                get_pos_player(5, &y, &x);
                                place_monster_one(y, x, o_ptr->pval2, FALSE, TRUE);

                                floor_item_increase(0 - item, -1);
                                floor_item_describe(0 - item);
                                floor_item_optimize(0 - item);
                        }
                break;
        }
       case 27: /* Spear of Darkness */
                       if (!get_aim_dir(&dir)) return;
                       fire_beam(GF_DARK, dir,
                                         damroll(8 * mto_s2, 8 + (plev / 5)));
		       break;
       case 28: /* Banish       */
                       banish_monsters(plev * 3 / 2 * mto_s2);
		       break;
        case 29: /* Dispel Good */
                (void)dispel_good(plev * 4 * mto_s2);
		break;
        case 30: /* Genocide */
		(void)genocide(TRUE);
		break;

        case 31: /* Detect Monsters */
                detect_monsters_normal();
        break;
        case 32: /* Hypnotic Gaze */
                msg_print("Your eyes look mesmerizing...");
                if (get_aim_dir(&dir))
                        (void) charm_monster(dir, p_ptr->lev * mto_s2);
        break;
        case 33: /* Piercing Gaze */
                map_area();
        break;
        case 34: /* Sense Magic */
                psychometry();
        break;
        case 35: /* Detection */
                (void)detect_all();
        break;
        case 36: /* Telepathy */
                (void)set_tim_esp(p_ptr->tim_esp + randint(30) + 25 + to_s2);
        break;
        case 37: /* Clairvoyance */
                wiz_lite();
        break;

        case 38: /* Resist Fire */
                set_oppose_fire(p_ptr->oppose_fire + randint(20) + 20 + p_ptr->to_s);
        break;
        case 39: /* Fiery Aura */
                set_tim_fire_aura(p_ptr->tim_fire_aura + 10 + randint(10) + p_ptr->to_s);
        break;
        case 40: /* Spear of Fire */
                       if (!get_aim_dir(&dir)) return;
                       fire_beam(GF_FIRE, dir,
                                         damroll(7 * mto_s2, 8 + (plev / 5)));
        break;
        case 41: /* Fireball */
                       if (!get_aim_dir(&dir)) return;
                       fire_ball(GF_FIRE, dir,
                                         80 * mto_s2, 2 + (plev / 10) + to_s2);
        break;
        case 42: /* Call Demon */
                msg_format("You magically summon a demon.");
                summon_specific_friendly(py, px, dun_level, SUMMON_DEMON, TRUE);
        break;
        case 43: /* Flame of Udun */
                        if(p_ptr->mimic_form != MIMIC_BALROG)
                                set_mimic(20 + randint(15) + to_s2, MIMIC_BALROG);
                        else
                                set_mimic(p_ptr->tim_mimic + 20 + randint(15) + to_s2, MIMIC_BALROG);
        break;
        case 44: /* Hellfire */
			if (!get_aim_dir(&dir)) return;
            fire_ball(GF_HELL_FIRE, dir,
                    666 * mto_s2, 3 + to_s2);
            take_hit(50+randint(50), "the strain of casting Hellfire");
        break;

        case 45: /* Confuse */
			if (!get_aim_dir(&dir)) return;
            (void)confuse_monster(dir, ( plev * 3) / 2 * mto_s2);
        break;
        case 46: /* Thraldom */
                msg_print("Your eyes look mesmerizing...");
                if (get_aim_dir(&dir))
                        (void) star_charm_monster(dir, p_ptr->lev / 2 * mto_s2);
        break;
        case 47: /* Polymorph Other */
			if (!get_aim_dir(&dir)) return;
			(void)poly_monster(dir);
        break;
        case 48: /* Polymorph Self */
                do_poly_self();
        break;
        case 49: /* Plague */
                project_hack(GF_POIS, 130 * mto_s2);
        break;
        case 50: /* Ravage Soul */
                if (!get_aim_dir(&dir)) return;
                /* A radius-0 ball may (1) be aimed at objects etc.,
                 * and will affect them; (2) may be aimed at ANY
                 * visible monster, unlike a 'bolt' which must travel
                 * to the monster. */

                fire_ball(GF_NETHER, dir,
                          damroll(8 + (plev / 10), 8 * mto_s2), 0 + to_s2);
        break;

        case 51: /* Absorb Light */
                unlite_area(10 * mto_s2, 5 + p_ptr->to_s);
                hp_player(damroll(plev / 5 + 1, 8 * mto_s2));
        break;
        case 52: /* Drain magic */
				{
					object_type * o_ptr;
					int lev, item;
                                        cptr  q, s;

					item_tester_hook = item_tester_hook_recharge;

					/* Get an item */
					q = "Drain which item? ";
					s = "You have nothing to drain.";
					if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) break;

					if (item >= 0)
					{
						o_ptr = &inventory[item];
					}
					else
					{
						o_ptr = &o_list[0 - item];
					}
					
					lev = k_info[o_ptr->k_idx].level;
					
					if (o_ptr->tval == TV_ROD)
					{
						if (o_ptr->pval > 0)
						{
							msg_print("You can't absorb energy from a discharged rod.");
						}
						else
						{
							p_ptr->csp += 2*lev;
							o_ptr->pval = 500;
						}
					}
					else
					{
						if (o_ptr->pval > 0)
						{
							p_ptr->csp += o_ptr->pval * lev;
							o_ptr->pval = 0;
						}
						else
						{
							msg_print("There's no energy there to absorb!");
						}
						o_ptr->ident |= IDENT_EMPTY;
					}
					
					if (p_ptr->csp > p_ptr->msp)
					{
						p_ptr->csp = p_ptr->msp;
					}
					
					p_ptr->notice |= (PN_COMBINE | PN_REORDER);
					p_ptr->window |= (PW_INVEN);
				}
        break;
        case 53: /* Spear of Death */
                       if (!get_aim_dir(&dir)) return;
                       fire_beam(GF_NETHER, dir,
                                 damroll(5 * mto_s2, 7 + plev / 5));
        break;
        case 54: /* Restore Life */
                (void)restore_level();
                hp_player(300 * mto_s2);
        break;
        case 55: /* Nightfall */
                       if (!get_aim_dir(&dir)) return;
                       fire_ball(GF_DARK, dir,
                                 200 * mto_s2, 5 + to_s2);
        break;
        case 56: /* Blood Curse */
                if (!get_aim_dir(&dir)) return;
                /* A radius-0 ball may (1) be aimed at objects etc.,
                 * and will affect them; (2) may be aimed at ANY
                 * visible monster, unlike a 'bolt' which must travel
                 * to the monster. */

                fire_ball(GF_MISSILE, dir, p_ptr->chp / 2, 0);
                take_hit(p_ptr->chp / 2, "casting a Blood Curse");
        break;
        case 57: /* Wraithworld */
                set_shadow(p_ptr->wraith_form + randint(plev/2) + (plev/2)+to_s2);
        break;

        case 58: /* Invoke Spirits */
           {
               int die = randint(100) + plev / 5+to_s2;
               if (!get_aim_dir(&dir)) return;

              msg_print("You call on the power of the dead...");
               if (die > 100)
                 msg_print ("You feel a surge of eldritch force!");

               if (die < 8) {
               msg_print("Oh no! Mouldering forms rise from the earth around you!");
               (void) summon_specific(py, px, dun_level, SUMMON_UNDEAD);
               } else if (die < 14) {
               msg_print("An unnamable evil brushes against your mind...");
               set_afraid(p_ptr->afraid + randint(4) + 4);
               } else if (die < 26) {
               msg_print("Your head is invaded by a horde of gibbering spectral voices...");
               set_confused(p_ptr->confused + randint(4) + 4);
               } else if (die < 31) {
               poly_monster (dir);
               } else if (die < 36) {
               fire_bolt_or_beam (beam - 10,
                          GF_MISSILE, dir,
                          damroll(3 + ((plev - 1) / 5), 4)*to_s2);
               } else if (die < 41) {
               confuse_monster (dir, plev);
               } else if (die < 46) {
               fire_ball (GF_POIS, dir, 20 + (plev / 2)*to_s2, 3+to_s2);
               } else if (die < 51) {
               lite_line (dir);
               } else if (die < 56) {
               fire_bolt_or_beam (beam - 10, GF_ELEC, dir,
                          damroll(3+((plev-5)/4),8)*to_s2);
               } else if (die < 61) {
               fire_bolt_or_beam (beam - 10, GF_COLD, dir,
                          damroll(5+((plev-5)/4),8)*to_s2);
               } else if (die < 66) {
               fire_bolt_or_beam (beam, GF_ACID, dir,
                          damroll(6+((plev-5)/4),8)*to_s2);
               } else if (die < 71) {
               fire_bolt_or_beam (beam, GF_FIRE, dir,
                          damroll(8+((plev-5)/4),8)*to_s2);
               } else if (die < 76) {
               drain_life (dir, 75+to_s2);
               } else if (die < 81) {
               fire_ball (GF_ELEC, dir, 30 + plev*to_s2 / 2, 2+to_s2);
               } else if (die < 86) {
               fire_ball (GF_ACID, dir, 40 + plev*to_s2, 2+to_s2);
               } else if (die < 91) {
               fire_ball (GF_ICE, dir, 70 + plev*to_s2, 3+to_s2);
               } else if (die < 96) {
               fire_ball (GF_FIRE, dir, 80 + plev*to_s2, 3+to_s2);
               } else if (die < 101) {
               drain_life (dir, 100 + plev+(p_ptr->to_s*2));
               } else if (die < 104) {
               earthquake (py, px, 12+to_s2);
               } else if (die < 106) {
               destroy_area (py, px, 15+to_s2, TRUE);
               } else if (die < 108) {
               genocide(TRUE);
               } else if (die < 110) {
               dispel_monsters (120+to_s2);
               } else { /* RARE */
               dispel_monsters (150+to_s2);
               slow_monsters();
               sleep_monsters();
               hp_player (300+(p_ptr->to_s*2));
               }

               if (die < 31)
                 msg_print("Sepulchral voices chuckle. 'Soon you will join us, mortal.'");
               break;
           }
        break;
        case 59: /* Vampiric Branding */
                brand_weapon(3);
        break;
        case 60: /* Dispel Life */
                dispel_living(plev * 4 * mto_s2);
        break;
        case 61: /* Word of Destruction */
                destroy_area(py, px, 15 + to_s2, TRUE);
        break;
        case 62: /* Summon Greater Undead */
                msg_format("You magically summon greater undeads.");
                summon_specific_friendly(py, px, dun_level, SUMMON_HI_UNDEAD_NO_UNIQUES, TRUE);
        break;
        case 63: /* Mass Genocide */
                mass_genocide(TRUE);
        break;

	default:
                msg_format("You cast an unknown Nether spell: %d.", spell);
		msg_print(NULL);
	}

	if (no_trump)
                msg_print("Nobody answers your call.");
}


static void cast_crusade_spell(int spell)
{
	int	dir;
	int	beam;
	int	plev = p_ptr->lev;
        int to_s2=p_ptr->to_s;
        int mto_s2=p_ptr->to_s;

        mto_s2 = (mto_s2==0)?1:mto_s2;

	if (p_ptr->pclass == CLASS_MAGE) beam = plev;
	else if (p_ptr->pclass == CLASS_HIGH_MAGE) beam = plev + 10;
	else beam = plev / 2;

	switch (spell)
	{
                case 0: /* Boldness */
                        set_afraid(0);
                break;
                case 1: /* Bless */
                       (void)set_blessed(p_ptr->blessed + randint(12) + to_s2 + 12);
                break;
                case 2: /* Infra */
                        set_tim_infra(p_ptr->tim_infra + 20 + randint(20) + to_s2);
                break;
                case 3: /* Flash  */
                        lite_area(damroll(2, (plev / 2)), (plev / 10) + 1 + to_s2);
                case 4: /* See invis */
                        set_tim_invis(p_ptr->tim_invis + 12 + randint(12) + to_s2);
                break;
                case 5: /* Touch of Confusion */
                    if (!(p_ptr->confusing))
                    {
                        msg_print("Your hands start glowing.");
                        p_ptr->confusing = TRUE;
                    }
                break;
                case 6: /* Invoke fear */
                        (void)fear_monster(dir, 5 + plev * mto_s2);
                break;
                case 7: /* Resist fire */
                        set_oppose_fire(p_ptr->oppose_fire + 10 + rand_int(10) + to_s2);
                break;

                case 8: /* Resist cold */
                        set_oppose_cold(p_ptr->oppose_cold + 12 + rand_int(10) + to_s2);
                break;
                case 9: /* Spear of light */
                       if (!get_aim_dir(&dir)) return;
                       msg_print("A line of blue shimmering light appears.");
                       lite_line(dir);
                       break;
                break;
                case 10: /* Sense foes */
                        detect_monsters_normal();
                break;
                case 11: /* Cure wounds & poison */
                        set_poisoned(0);
                        set_cut(0);
                break;
                case 12: /* Wolvish hunger */
                        if(p_ptr->mimic_form != MIMIC_WOLF)
                                set_mimic(10 + randint(8) + to_s2, MIMIC_WOLF);
                        else
                                set_mimic(p_ptr->tim_mimic + 10 + randint(8) + to_s2, MIMIC_WOLF);
                        break;
                break;
                case 13: /* Lightning lance */
                       if (!get_aim_dir(&dir)) return;
                       fire_bolt_or_beam(beam-10, GF_ELEC, dir,
                               damroll(6 + ((plev-5)/4), 8 * mto_s2));
                break;
                case 14: /* Charm foes */
                         if (!get_aim_dir(&dir)) return;
                         (void) charm_monster(dir, plev * mto_s2);
                break;
                case 15: /* Heroism */
                        (void)set_hero(p_ptr->hero + randint(10)+ p_ptr->to_s + 10);
                break;

                case 16: /* holy chant */
                       set_blessed(p_ptr->blessed + randint(20) + 20 + to_s2);
                break;
                case 17: /* Fiery aura */
                        set_tim_fire_aura(p_ptr->tim_fire_aura + 15 + randint(10) + p_ptr->to_s);
                break;
                case 18: /* Smite */
                       if (!get_aim_dir(&dir)) return;
                       fire_bolt_or_beam(beam-10, GF_COLD, dir,
                               damroll(8 + ((plev-5)/4), 8 * mto_s2));
                break;
                case 19: /* Polymorph wounds */
			do_poly_wounds();
                break;
                case 20: /* holy lance */
                       if (!get_aim_dir(&dir)) return;
                       fire_bolt_or_beam(beam-10, GF_HOLY_FIRE, dir,
                               damroll(8 + ((plev-5)/4), 9 * mto_s2));
                break;
                case 21: /* vision */
                        map_area();
                break;
                case 22: /* exorcise */
                        dispel_demons((50 * mto_s2) + plev);
                        dispel_undead((50 * mto_s2) + plev);
                break;
                case 23: /* prayer */
                      (void)set_blessed(p_ptr->blessed + randint(48) + 48 + to_s2);
                break;

                case 24: /* call thunder */
                       msg_print("BOOM! Shake the room!");
                       project(0, 2+plev/10, py, px,
                           45 + plev * mto_s2, GF_SOUND, PROJECT_KILL|PROJECT_ITEM);
                break;
                case 25: /* berserk rage */
                        (void)set_shero(p_ptr->shero + randint(35) + 25 + to_s2);
                        (void)set_blessed(p_ptr->blessed + randint(35) + 25 + to_s2);
                break;
                case 26: /* dragon's wrath */
                        if(p_ptr->mimic_form != MIMIC_DRAGON)
                                set_mimic(15 + randint(10) + to_s2, MIMIC_DRAGON);
                        else
                                set_mimic(p_ptr->tim_mimic + 15 + randint(10) + to_s2, MIMIC_DRAGON);
                        break;
                break;
                case 27: /* mass charm */
                        charm_monsters(100 * mto_s2);
                break;
                case 28: /* haste self */
			if (!p_ptr->fast)
			{
                                (void)set_fast(randint(20 + (plev) ) + plev + to_s2);
			}
			else
			{
                                (void)set_fast(p_ptr->fast + randint(5) + to_s2);
			}
                break;
                case 29: /* Wave of force */
                project(0, 3 + to_s2, py, px,
                        (120 + plev) * mto_s2,
                        GF_FORCE, PROJECT_KILL | PROJECT_ITEM | PROJECT_GRID | PROJECT_STOP);
                break;
                case 30: /* Invocation */
                       dispel_evil(150 * mto_s2);
                       if(rand_int(100) < 10) banish_evil(50 + p_ptr->to_s);
                break;
                case 31: /* Invulnerability */
                        (void)set_invuln(p_ptr->invuln + randint(3)+ to_s2 + 2);
                break;
	default:
                msg_format("You cast an unknown Crusade spell: %d.", spell);
		msg_print(NULL);
    }
}


static void cast_sigaldry_spell(int spell)
{
	int	dir;
	int	beam;
	int	plev = p_ptr->lev;
        int to_s2=p_ptr->to_s;
        int mto_s2=p_ptr->to_s;

        mto_s2 = (mto_s2==0)?1:mto_s2;

	if (p_ptr->pclass == CLASS_MAGE) beam = plev;
	else if (p_ptr->pclass == CLASS_HIGH_MAGE) beam = plev + 10;
	else beam = plev / 2;

	switch (spell)
	{
        case 0: /* Detect Monster */
                (void)detect_monsters_normal();
                break;
        case 1: /* Wizard Lock */
            if (!(get_aim_dir(&dir))) break;
            (void) wizard_lock(dir);
        break;
        case 2: /* Illuminate */
                (void)lite_area(damroll(2 * mto_s2, (plev / 2)), (plev / 10) + 1+to_s2);
        break;
        case 3: /* Locate Doors/Stairs */
			(void)detect_doors();
			(void)detect_stairs();
        break;
        case 4: /* Locate Traps */
			(void)detect_traps();
        break;
        case 5: /* Disruption I */
        {
                int typ = GF_MISSILE;

                if (!get_aim_dir(&dir)) return;
                /* A radius-0 ball may (1) be aimed at objects etc.,
                 * and will affect them; (2) may be aimed at ANY
                 * visible monster, unlike a 'bolt' which must travel
                 * to the monster. */

                if(randint(100) < plev)
                {
                        int die = rand_int(100);
                        if (die < 40) {
                                typ = GF_STUN_DAM;
                        } else if (die < 80) {
                                typ = GF_CONF_DAM;
                        } else {
                                typ = GF_STUN_CONF;
                        }
                }

                fire_ball(typ, dir,
                          damroll(5 * mto_s2, 4) + (plev / 5), 0 + to_s2);
        }
        break;
        case 6: /* Blink */
                teleport_player(10 + to_s2);
        break;
        case 7: /* Detect Treasures */
                (void)detect_treasure();
        break;
        case 8: /* Detect Objects */
			(void)detect_objects_normal();
			(void)detect_objects_gold();
        break;
        case 9: /* Warding I */
                set_shield(p_ptr->shield + rand_int(5) + 5 + to_s2, 50 + to_s2);
        break;
        case 10: /* Trap/Door Destruction */
			(void)destroy_doors_touch();
        break;
        case 11: /* Sleep I */
				if (!get_aim_dir(&dir)) return;
				(void)sleep_monster(dir);
        break;
        case 12: /* Preservation */
        {
                int item;
                object_type *o_ptr;
                cptr q, s;

                /* Restrict choices to corpses */
                item_tester_tval = TV_CORPSE;

                /* Get an item */
                q = "Preserve which corspe? ";
                s = "You have no corpse to preserve.";
                if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

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

                o_ptr->pval = 65535;
                msg_print("The corpse seem to be fresher.");
                break;
        }
        break;
        case 13: /* Confuse */
			if (!get_aim_dir(&dir)) return;
            (void)confuse_monster(dir, ( plev * 3) / 2 * mto_s2);
        break;
        case 14: /* Magic Aura */
                (void)set_holy(p_ptr->holy + 10 + randint(5) + to_s2);
        break;
        case 15: /* Appraise Item */
                psychometry();
        break;

        case 16: /* Resist Fire */
                set_oppose_fire(p_ptr->oppose_fire + 10 + rand_int(20) + p_ptr->to_s);
        break;
        case 17: /* Resist Cold */
                set_oppose_cold(p_ptr->oppose_cold + 10 + rand_int(20) + p_ptr->to_s);
        break;
        case 18: /* Finger of Pain */
                /* It doesn't need to travel to the monster, it just Jump at it */
                if (!get_aim_dir(&dir)) return;
                project_hook(GF_MISSILE, dir, damroll(5 + (plev / 5), 8 * mto_s2),
                             PROJECT_STOP | PROJECT_KILL | PROJECT_JUMP);
        break;
        case 19: /* Recharge I */
                (void)recharge(20 * mto_s2);
        break;
        case 20: /* Magic Map */
                map_area();
        break;
        case 21: /* Sleep II */
                       (void)sleep_monsters_touch();
        break;
        case 22: /* Teleport Self */
                teleport_player(200 * mto_s2);
        break;
        case 23: /* Identify */
			(void)ident_spell();
        break;

        case 24: /* True Strike */
                set_strike(p_ptr->strike + randint(25) + 25 + to_s2);
        break;
        case 25: /* Between Gate */
       {
             int ii, ij;

             if(special_flag){msg_print("Not on special levels!");break;}

             msg_print("You open a between gate. Choose a destination.");
             if (!tgt_pt(&ii,&ij)) return;
             p_ptr->energy -= 60 - plev;
             if (!cave_empty_bold(ij,ii) || (cave[ij][ii].info & CAVE_ICKY) ||
             (distance(ij,ii,py,px) > plev + 2+to_s2) ||
             (!rand_int(plev * plev / 2)))
             {
                 msg_print("You fail to exit the between correctly!");
                 p_ptr->energy -= 100;
                 get_pos_player(10+to_s2,&ij,&ii);
             }
             cave_set_feat(py,px,FEAT_BETWEEN);
             cave_set_feat(ij,ii,FEAT_BETWEEN);
             cave[py][px].special = ii + (ij << 8);
             cave[ij][ii].special = px + (py << 8);

             break;
       }
        case 26: /* Disruption II */
        {
                int typ = GF_MISSILE;

                if (!get_aim_dir(&dir)) return;
                /* A radius-0 ball may (1) be aimed at objects etc.,
                 * and will affect them; (2) may be aimed at ANY
                 * visible monster, unlike a 'bolt' which must travel
                 * to the monster. */

                if(randint(60) < plev)
                {
                        int die = rand_int(100);
                        if (die < 40) {
                                typ = GF_STUN_DAM;
                        } else if (die < 80) {
                                typ = GF_CONF_DAM;
                        } else {
                                typ = GF_STUN_CONF;
                        }
                }

                fire_ball(typ, dir,
                          damroll(10 * mto_s2, 10) + (plev / 5), 0 + to_s2);
        }
        break;
               break;
        case 27: /* Drain Magic I */
				{
					object_type * o_ptr;
					int lev, item;
                                        cptr  q, s;

                                        item_tester_hook = item_tester_hook_scroll_amulet;

					/* Get an item */
					q = "Drain which item? ";
					s = "You have nothing to drain.";
					if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) break;

					if (item >= 0)
					{
						o_ptr = &inventory[item];
					}
					else
					{
						o_ptr = &o_list[0 - item];
					}
					
					lev = k_info[o_ptr->k_idx].level;
					
                                        if(randint(100) < 50)
                                        {
                                                if (o_ptr->tval == TV_SCROLL)
                                                {
                                                        if (o_ptr->sval == SV_SCROLL_NOTHING)
                                                        {
                                                                msg_print("You can't absorb energy from that.");
                                                        }
                                                        else
                                                        {
                                                                p_ptr->csp += lev;
                                                                o_ptr->sval = SV_SCROLL_NOTHING;
                                                        }
                                                }
                                                else
                                                {
                                                        if ((o_ptr->sval == SV_AMULET_NOTHING) || (o_ptr->sval == SV_AMULET_NO_MAGIC))
                                                        {
                                                                msg_print("You can't absorb energy from that.");
                                                        }
                                                        else
                                                        {
                                                                p_ptr->csp += 2 * lev;
                                                                o_ptr->sval = SV_AMULET_NOTHING;
                                                        }
                                                }
                                        }
                                        /* Simply destroy the object */
                                        else
                                        {
                                                msg_print("The object gets destroyed.");
                                                if (o_ptr->tval == TV_SCROLL)
                                                {
                                                        o_ptr->sval = SV_SCROLL_NOTHING;
                                                }
                                                else
                                                {
                                                        o_ptr->sval = SV_AMULET_NOTHING;
                                                }
                                        }

                                        o_ptr->k_idx = lookup_kind(o_ptr->tval, o_ptr->sval);
                                        if (item >= 0)
                                        {
                                                inven_item_describe(item);
                                        }
                                        else
                                        {
                                                floor_item_describe(0 - item);
                                        }

					if (p_ptr->csp > p_ptr->msp)
					{
						p_ptr->csp = p_ptr->msp;
					}
					
					p_ptr->notice |= (PN_COMBINE | PN_REORDER);
					p_ptr->window |= (PW_INVEN);

                break;
        }
        case 28: /* Scribe Scroll */
        {
                int item, realm, spel;
                object_type *o_ptr, *q_ptr;
                cptr q, s;

                /* Restrict choices to scrolls */
                item_tester_tval = TV_SCROLL;

                /* Get an item */
                q = "Use which scroll? ";
                s = "You have no scroll to use.";
                if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

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

                if(o_ptr->sval != SV_SCROLL_NOTHING)
                {
                        msg_print("You must use a Scroll of Nothing.");
                        break;
                }

                if(o_ptr->number > 1)
                {
                        msg_print("You can't inscribe more than one scroll.");
                        break;
                }

                /* Restrict choices to spell books */
                item_tester_tval = mp_ptr->spell_book;
	
                /* Get an item */
                q = "Use which book? ";
                s = "You have no spell books!";
                if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;
	
                /* Get the item (in the pack) */
                if (item >= 0)
                {
                        q_ptr = &inventory[item];
                }
	
                /* Get the item (on the floor) */
                else
                {
                        q_ptr = &o_list[0 - item];
                }
	
                realm = q_ptr->tval - TV_VALARIN_BOOK + 1;
	
                /* Ask for a spell */
                if (!get_spell(&spel, "scribe", q_ptr->sval, TRUE, q_ptr))
                {
                        if (spell == -2)
                                msg_print("You don't know any spells in that book.");
                        return;
                }

                if(realm_info[realm][spel].smana + realm_info[REALM_SIGALDRY][spell].smana > p_ptr->csp)
                {
                        msg_print("You don't have enough mana to do that");
                        return;
                }

                /* Create the scroll */
                o_ptr->sval = SV_SCROLL_SPELL;
                o_ptr->pval = realm;
                o_ptr->pval2 = spel;
                o_ptr->k_idx = lookup_kind(o_ptr->tval, o_ptr->sval);

                /* Use some more mana */
                p_ptr->csp -= realm_info[realm][spel].smana + realm_info[REALM_SIGALDRY][spell].smana;

                /* Identify it */
                object_aware(o_ptr);

                if (item >= 0)
                {
                        inven_item_describe(item);
                }
                else
                {
                        floor_item_describe(0 - item);
                }

                p_ptr->notice |= (PN_COMBINE | PN_REORDER);
                p_ptr->window |= (PW_INVEN);
                break;
        }
        case 29: /* Infuse Amulet */
        {
                int item, die = randint(100);
                object_type             *o_ptr;
                cptr q, s;

                /* Restrict choices to amulets */
                item_tester_tval = TV_AMULET;

                /* Get an item */
                q = "Use which amulet? ";
                s = "You have no amulet to use.";
                if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

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

                if(o_ptr->sval != SV_AMULET_NOTHING)
                {
                        msg_print("You must use an Amulet of Nothing.");
                        break;
                }

                if(die <= 2)
                {
                        o_ptr->sval = SV_AMULET_WISDOM;
                        apply_magic(o_ptr, dun_level, TRUE, TRUE, FALSE, FALSE);
                }
                else if(die <= 12)
                {
                        o_ptr->sval = SV_AMULET_CHARISMA;
                        apply_magic(o_ptr, dun_level, TRUE, TRUE, FALSE, FALSE);
                }
                else if(die <= 22)
                {
                        o_ptr->sval = SV_AMULET_SEARCHING;
                        apply_magic(o_ptr, dun_level, TRUE, TRUE, FALSE, FALSE);
                }
                else if(die <= 32)
                {
                        o_ptr->sval = SV_AMULET_TELEPORT;
                        apply_magic(o_ptr, dun_level, TRUE, TRUE, FALSE, FALSE);
                }
                else if(die <= 42)
                {
                        o_ptr->sval = SV_AMULET_SLOW_DIGEST;
                        apply_magic(o_ptr, dun_level, TRUE, TRUE, FALSE, FALSE);
                }
                else if(die <= 52)
                {
                        o_ptr->sval = SV_AMULET_RESIST_ACID;
                        apply_magic(o_ptr, dun_level, TRUE, TRUE, FALSE, FALSE);
                }
                else if(die <= 62)
                {
                        o_ptr->sval = SV_AMULET_ADORNMENT;
                        apply_magic(o_ptr, dun_level, TRUE, TRUE, FALSE, FALSE);
                }
                else if(die <= 67)
                {
                        o_ptr->sval = SV_AMULET_THE_MAGI;
                        apply_magic(o_ptr, dun_level, TRUE, TRUE, FALSE, FALSE);
                }
                else if(die <= 77)
                {
                        o_ptr->sval = SV_AMULET_DOOM;
                        apply_magic(o_ptr, dun_level, TRUE, TRUE, FALSE, FALSE);
                }
                else if(die <= 78)
                {
                        o_ptr->sval = SV_AMULET_REFLECTION;
                        apply_magic(o_ptr, dun_level, TRUE, TRUE, FALSE, FALSE);
                }
                else if(die <= 88)
                {
                        o_ptr->sval = SV_AMULET_NO_MAGIC;
                        apply_magic(o_ptr, dun_level, TRUE, TRUE, FALSE, FALSE);
                }
                else if(die <= 98)
                {
                        o_ptr->sval = SV_AMULET_NO_TELE;
                        apply_magic(o_ptr, dun_level, TRUE, TRUE, FALSE, FALSE);
                }
                else if(die <= 100)
                {
                        o_ptr->sval = SV_AMULET_RESISTANCE;
                        apply_magic(o_ptr, dun_level, TRUE, TRUE, FALSE, FALSE);
                }
                o_ptr->k_idx = lookup_kind(o_ptr->tval, o_ptr->sval);

                p_ptr->notice |= (PN_COMBINE | PN_REORDER);
                p_ptr->window |= (PW_INVEN);
                break;
        }
        case 30: /* Glyph of Warding */
                        warding_glyph();
        break;
        case 31: /* Implosion */
                fire_ball(GF_IMPLOSION, dir, 200 * mto_s2, 2 + to_s2);
        break;
	default:
                msg_format("You cast an unknown Sigaldry spell: %d.", spell);
		msg_print(NULL);
    }
}

/* Improved for NewAngband */
int use_symbiotic_power(int r_idx, bool great, bool only_number)
{
	int                     Power = -1;
        int                     num = 0, dir = 0 , i;

	int             powers[36];
	char            power_desc[36][80];

	bool            flag, redraw;
        int             ask, plev = p_ptr->lev;

	char            choice;

	char            out_val[160];
        monster_race    *r_ptr = &r_info[r_idx];
        int rlev = ((r_ptr->level >= 1) ? r_ptr->level : 1);
        int x=px,y=py,k,count;
	int rad;
        s32b brdam = p_ptr->chp;
        s32b dieroll = p_ptr->lev / 2;
        brdam = brdam + ((brdam * (p_ptr->abilities[(CLASS_MONSTER_MAGE * 10) + 1] * 5)) / 100);
        brdam = brdam + ((brdam * (p_ptr->abilities[(CLASS_APPRENTICE * 10) + 2] * 10)) / 100);
        dieroll = dieroll + ((dieroll * (p_ptr->abilities[(CLASS_MONSTER_MAGE * 10) + 1] * 5)) / 100);
        dieroll = dieroll + ((dieroll * (p_ptr->abilities[(CLASS_APPRENTICE * 10) + 2] * 10)) / 100);

        /* List the powers */
        if(r_ptr->flags2 & RF2_MULTIPLY) {strcpy(power_desc[num],"Multiply");powers[num++]=90;}
        if(r_ptr->flags4 & RF4_SHRIEK) {strcpy(power_desc[num],"Agravate monsters");powers[num++]=0;}
        if(great) if(r_ptr->flags4 & RF4_ROCKET) {strcpy(power_desc[num],"Rocket");powers[num++]=1;}
        if(r_ptr->flags4 & RF4_ARROW_1) {strcpy(power_desc[num],"Arrow1");powers[num++]=2;}
        if(r_ptr->flags4 & RF4_ARROW_2) {strcpy(power_desc[num],"Arrow2");powers[num++]=3;}
        if(great) if(r_ptr->flags4 & RF4_ARROW_3) {strcpy(power_desc[num],"Arrow3");powers[num++]=4;}
        if(great) if(r_ptr->flags4 & RF4_ARROW_4) {strcpy(power_desc[num],"Arrow4");powers[num++]=5;}
        if(great) if(r_ptr->flags4 & RF4_BR_ACID) {strcpy(power_desc[num],"Breathe Acid");powers[num++]=6;}
        if(great) if(r_ptr->flags4 & RF4_BR_ELEC) {strcpy(power_desc[num],"Breathe Lightning");powers[num++]=7;}
        if(r_ptr->flags4 & RF4_BR_FIRE) {strcpy(power_desc[num],"Breathe Fire");powers[num++]=8;}
        if(r_ptr->flags4 & RF4_BR_COLD) {strcpy(power_desc[num],"Breathe Cold");powers[num++]=9;}
        if(great) if(r_ptr->flags4 & RF4_BR_POIS) {strcpy(power_desc[num],"Breathe Poison");powers[num++]=10;}
        if(great) if(r_ptr->flags4 & RF4_BR_NETH) {strcpy(power_desc[num],"Breathe Nether");powers[num++]=11;}
        if(r_ptr->flags4 & RF4_BR_LITE) {strcpy(power_desc[num],"Breathe Lite");powers[num++]=12;}
        if(great) if(r_ptr->flags4 & RF4_BR_DARK) {strcpy(power_desc[num],"Breathe Darkness");powers[num++]=13;}
        if(great) if(r_ptr->flags4 & RF4_BR_CONF) {strcpy(power_desc[num],"Breathe Confusion");powers[num++]=14;}
        if(great) if(r_ptr->flags4 & RF4_BR_SOUN) {strcpy(power_desc[num],"Breathe Sound");powers[num++]=15;}
        if(great) if(r_ptr->flags4 & RF4_BR_CHAO) {strcpy(power_desc[num],"Breathe Chaos");powers[num++]=16;}
        if(great) if(r_ptr->flags4 & RF4_BR_DISE) {strcpy(power_desc[num],"Breathe Disenchantment");powers[num++]=17;}
        if(great) if(r_ptr->flags4 & RF4_BR_NEXU) {strcpy(power_desc[num],"Breathe Nexus");powers[num++]=18;}
        if(great) if(r_ptr->flags4 & RF4_BR_TIME) {strcpy(power_desc[num],"Breathe Time");powers[num++]=19;}
        if(great) if(r_ptr->flags4 & RF4_BR_INER) {strcpy(power_desc[num],"Breathe Inertia");powers[num++]=20;}
        if(great) if(r_ptr->flags4 & RF4_BR_GRAV) {strcpy(power_desc[num],"Breathe Gravity");powers[num++]=21;}
        if(great) if(r_ptr->flags4 & RF4_BR_SHAR) {strcpy(power_desc[num],"Breathe Shards");powers[num++]=22;}
        if(great) if(r_ptr->flags4 & RF4_BR_PLAS) {strcpy(power_desc[num],"Breathe Plasma");powers[num++]=23;}
        if(great) if(r_ptr->flags4 & RF4_BR_WALL) {strcpy(power_desc[num],"Breathe Force");powers[num++]=24;}
        if(great) if(r_ptr->flags4 & RF4_BR_MANA) {strcpy(power_desc[num],"Breathe Mana");powers[num++]=25;}
        if(great) if(r_ptr->flags4 & RF4_BA_NUKE) {strcpy(power_desc[num],"Nuke Ball");powers[num++]=26;}
        if(great) if(r_ptr->flags4 & RF4_BR_NUKE) {strcpy(power_desc[num],"Breathe Nuke");powers[num++]=27;}
        if(great) if(r_ptr->flags4 & RF4_BA_CHAO) {strcpy(power_desc[num],"Chaos Ball");powers[num++]=28;}
        if(great) if(r_ptr->flags4 & RF4_BR_DISI) {strcpy(power_desc[num],"Breathe Disintegration");powers[num++]=29;}
        if(great) if(r_ptr->flags5 & RF5_BA_ACID) {strcpy(power_desc[num],"Acid Ball");powers[num++]=30;}
        if(great) if(r_ptr->flags5 & RF5_BA_ELEC) {strcpy(power_desc[num],"Lightning Ball");powers[num++]=31;}
        if(r_ptr->flags5 & RF5_BA_FIRE) {strcpy(power_desc[num],"Fire Ball");powers[num++]=32;}
        if(r_ptr->flags5 & RF5_BA_COLD) {strcpy(power_desc[num],"Cold Ball");powers[num++]=33;}
        if(great) if(r_ptr->flags5 & RF5_BA_POIS) {strcpy(power_desc[num],"Poison Ball");powers[num++]=34;}
        if(great) if(r_ptr->flags5 & RF5_BA_NETH) {strcpy(power_desc[num],"Nether Ball");powers[num++]=35;}
        if(r_ptr->flags5 & RF5_BA_WATE) {strcpy(power_desc[num],"Water Ball");powers[num++]=36;}
        if(great) if(r_ptr->flags5 & RF5_BA_MANA) {strcpy(power_desc[num],"Mana Ball");powers[num++]=37;}
        if(great) if(r_ptr->flags5 & RF5_BA_DARK) {strcpy(power_desc[num],"Darkness Ball");powers[num++]=38;}
        if(r_ptr->flags5 & RF5_CAUSE_1) {strcpy(power_desc[num],"Cause light wounds");powers[num++]=42;}
        if(r_ptr->flags5 & RF5_CAUSE_2) {strcpy(power_desc[num],"Cause medium wounds");powers[num++]=43;}
        if(r_ptr->flags5 & RF5_CAUSE_3) {strcpy(power_desc[num],"Cause critical wounds");powers[num++]=44;}
        if(r_ptr->flags5 & RF5_CAUSE_4) {strcpy(power_desc[num],"Cause mortal wounds");powers[num++]=45;}
        if(r_ptr->flags5 & RF5_BO_ACID) {strcpy(power_desc[num],"Acid Bolt");powers[num++]=46;}
        if(r_ptr->flags5 & RF5_BO_ELEC) {strcpy(power_desc[num],"Lightning Bolt");powers[num++]=47;}
        if(r_ptr->flags5 & RF5_BO_FIRE) {strcpy(power_desc[num],"Fire Bolt");powers[num++]=48;}
        if(r_ptr->flags5 & RF5_BO_COLD) {strcpy(power_desc[num],"Cold Bolt");powers[num++]=49;}
	if(r_ptr->flags5 & RF5_BO_POIS) {strcpy(power_desc[num],"Poison Bolt");powers[num++]=56;}
        if(r_ptr->flags5 & RF5_BO_NETH) {strcpy(power_desc[num],"Nether Bolt");powers[num++]=50;}
        if(r_ptr->flags5 & RF5_BO_WATE) {strcpy(power_desc[num],"Water Bolt");powers[num++]=51;}
        if(r_ptr->flags5 & RF5_BO_MANA) {strcpy(power_desc[num],"Mana Bolt");powers[num++]=52;}
        if(r_ptr->flags5 & RF5_BO_PLAS) {strcpy(power_desc[num],"Plasma Bolt");powers[num++]=53;}
        if(r_ptr->flags5 & RF5_BO_ICEE) {strcpy(power_desc[num],"Ice Bolt");powers[num++]=54;}
        if(r_ptr->flags5 & RF5_MISSILE) {strcpy(power_desc[num],"Missile");powers[num++]=55;}
        if(r_ptr->flags5 & RF5_SCARE) {strcpy(power_desc[num],"Scare");powers[num++]=41;}
        if(r_ptr->flags5 & RF5_BLIND) {strcpy(power_desc[num],"Blindness");powers[num++]=57;}
        if(r_ptr->flags5 & RF5_CONF) {strcpy(power_desc[num],"Confusion");powers[num++]=58;}
        if(r_ptr->flags5 & RF5_SLOW) {strcpy(power_desc[num],"Slow");powers[num++]=59;}
        if(r_ptr->flags5 & RF5_HOLD) {strcpy(power_desc[num],"Paralyse");powers[num++]=60;}
        if(r_ptr->flags6 & RF6_HASTE) {strcpy(power_desc[num],"Haste Self");powers[num++]=61;}
        if(great) if(r_ptr->flags6 & RF6_HAND_DOOM) {strcpy(power_desc[num],"Hand of Doom");powers[num++]=62;}
        if(r_ptr->flags6 & RF6_HEAL) {strcpy(power_desc[num],"Heal");powers[num++]=63;}
        if(r_ptr->flags6 & RF6_BLINK) {strcpy(power_desc[num],"Blink");powers[num++]=64;}
        if(r_ptr->flags6 & RF6_TPORT) {strcpy(power_desc[num],"Teleport");powers[num++]=65;}
        if(great) if(r_ptr->flags6 & RF6_TELE_TO) {strcpy(power_desc[num],"Teleport To");powers[num++]=66;}
        if(r_ptr->flags6 & RF6_TELE_AWAY) {strcpy(power_desc[num],"Teleport Away");powers[num++]=67;}
        if(great) if(r_ptr->flags6 & RF6_TELE_LEVEL) {strcpy(power_desc[num],"Teleport Level");powers[num++]=68;}
        if(r_ptr->flags6 & RF6_DARKNESS) {strcpy(power_desc[num],"Darkness");powers[num++]=69;}
        if(great) if(r_ptr->flags6 & RF6_TRAPS) {strcpy(power_desc[num],"Create Traps");powers[num++]=88;}
	if(great) if(r_ptr->flags6 & RF6_RAISE_DEAD) {strcpy(power_desc[num],"Raise the Dead");powers[num++]=89;}
        if(r_ptr->flags6 & RF6_S_BUG) {strcpy(power_desc[num],"Summon Sofware Bugs");powers[num++]=70;}
        if(r_ptr->flags6 & RF6_S_RNG) {strcpy(power_desc[num],"Summon RNG");powers[num++]=71;}
        if(great) if(r_ptr->flags6 & RF6_S_DRAGONRIDER) {strcpy(power_desc[num],"Summon DragonRider");powers[num++]=72;}
        if(r_ptr->flags6 & RF6_S_KIN) {strcpy(power_desc[num],"Summon Kin");powers[num++]=73;}
        if(great) if(r_ptr->flags6 & RF6_S_CYBER) {strcpy(power_desc[num],"Summon Cyberdemon");powers[num++]=74;}
        if(r_ptr->flags6 & RF6_S_MONSTER) {strcpy(power_desc[num],"Summon Monster");powers[num++]=75;}
        if(great) if(r_ptr->flags6 & RF6_S_MONSTERS) {strcpy(power_desc[num],"Summon Monsters");powers[num++]=76;}
        if(r_ptr->flags6 & RF6_S_ANT) {strcpy(power_desc[num],"Summon Ants");powers[num++]=77;}
        if(r_ptr->flags6 & RF6_S_SPIDER) {strcpy(power_desc[num],"Summon Spiders");powers[num++]=78;}
        if(great) if(r_ptr->flags6 & RF6_S_HOUND) {strcpy(power_desc[num],"Summon Hound");powers[num++]=79;}
        if(great) if(r_ptr->flags6 & RF6_S_HYDRA) {strcpy(power_desc[num],"Summon Hydras");powers[num++]=80;}
        if(great) if(r_ptr->flags6 & RF6_S_ANGEL) {strcpy(power_desc[num],"Summon Angel");powers[num++]=81;}
        if(great) if(r_ptr->flags6 & RF6_S_DEMON) {strcpy(power_desc[num],"Summon Demon");powers[num++]=82;}
        if(great) if(r_ptr->flags6 & RF6_S_UNDEAD) {strcpy(power_desc[num],"Summon Undead");powers[num++]=83;}
        if(great) if(r_ptr->flags6 & RF6_S_DRAGON) {strcpy(power_desc[num],"Summon Dragon");powers[num++]=84;}
        if(great) if(r_ptr->flags6 & RF6_S_HI_UNDEAD) {strcpy(power_desc[num],"Summon High Undead");powers[num++]=85;}
        if(great) if(r_ptr->flags6 & RF6_S_HI_DRAGON) {strcpy(power_desc[num],"Summon High Dragon");powers[num++]=86;}
        if(great) if(r_ptr->flags6 & RF6_S_WRAITH) {strcpy(power_desc[num],"Summon Wraith");powers[num++]=87;}
        if(p_ptr->body_monster == 1123) {strcpy(power_desc[num],"Spit Webs");powers[num++]=91;}

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
                strnfmt(out_val, 78, "(Powers %c-%c, *=List, ESC=exit) Use which power of your monster? ",
			I2A(0), I2A(num - 1));
	}
	else
	{
                strnfmt(out_val, 78, "(Powers %c-%c, *=List, ESC=exit) Use which power of your monster? ",
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

	if (r_ptr->flags2 & RF2_POWERFUL)
		rad = 1 + (p_ptr->lev/15);
	else
		rad = 1 + (p_ptr->lev/20);

        switch(Power)
        {
		case 90: /* Multiply */
			do_cmd_wiz_named_friendly(p_ptr->body_monster, FALSE);
			break;
                case 0: /* Shriek */
                        aggravate_monsters(-1);
                        break;
                case 1: /* Rocket */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_ROCKET, dir, brdam / 2, 1 + (p_ptr->lev/20));
                        break;
                case 2: /* Arrow1 */
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_ARROW, dir, damroll(dieroll,dieroll));
                        break;
                case 3: /* Arrow2 */
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_ARROW, dir, damroll((dieroll + 10),(dieroll + 10)));
                        break;
                case 4: /* Arrow3 */
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_ARROW, dir, damroll((dieroll + 20),(dieroll + 20)));
                        break;
                case 5: /* Arrow4 */
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_ARROW, dir, damroll((dieroll + 30),(dieroll + 30)));
                        break;
                case 6: /* Br acid */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_ACID, dir, brdam, rad);
                        break;
                case 7: /* Br elec */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_ELEC, dir, brdam, rad);
                        break;
                case 8: /* br fire */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_FIRE, dir, brdam, rad);
                        break;
                case 9: /* br cold */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_COLD, dir, brdam, rad);
                        break;
                case 10: /* br pois */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_POIS, dir, brdam, rad);
                        break;
                case 11: /* br neth */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_NETHER, dir, brdam, rad);
                        break;
                case 12: /* br lite */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_LITE, dir, brdam, rad);
                        break;
                case 13: /* br dark */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_DARK, dir, brdam, rad);
                        break;
                case 14: /* br conf */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_CONFUSION, dir, brdam, rad);
                        break;
                case 15: /* br soun */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_SOUND, dir, brdam, rad);
                        break;
                case 16: /* br chao */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_CHAOS, dir, brdam, rad);
                        break;
                case 17: /* br dise */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_DISENCHANT, dir, brdam, rad);
                        break;
                case 18: /* br nexu */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_NEXUS, dir, brdam, rad);
                        break;
                case 19: /* br time */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_TIME, dir, brdam, rad);
                        break;
                case 20: /* br iner */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_INERTIA, dir, brdam, rad);
                        break;
                case 21: /* br grav */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_GRAVITY, dir, brdam, rad);
                        break;
                case 22: /* br shar */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_SHARDS, dir, brdam, rad);
                        break;
                case 23: /* br plas */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_PLASMA, dir, brdam, rad);
                        break;
                case 24: /* br wall */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_FORCE, dir, brdam, rad);
                        break;
                case 25: /* br mana */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_MANA, dir, brdam, rad);
                        break;
                case 26: /* ba nuke */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_NUKE, dir, brdam, 1 + (p_ptr->lev/20));
                        break;
                case 27: /* br nuke */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_NUKE, dir, brdam, 1 + (p_ptr->lev/20));
                        break;
                case 28: /* ba chao */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_CHAOS, dir, damroll(dieroll,dieroll), 2);
                        break;
                case 29: /* br disi */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_DISINTEGRATE, dir, brdam, 1 + (p_ptr->lev/20));
                        break;
                case 30: /* ba acid */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_ACID, dir, damroll(dieroll,dieroll), 2);
                        break;
                case 31: /* ba elec */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_ELEC, dir, damroll(dieroll,dieroll), 2);
                        break;
                case 32: /* ba fire */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_FIRE, dir, damroll(dieroll,dieroll), 2);
                        break;
                case 33: /* ba cold */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_COLD, dir, damroll(dieroll,dieroll), 2);
                        break;
                case 34: /* ba pois */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_POIS, dir, damroll(dieroll,dieroll), 2);
                        break;
                case 35: /* ba neth */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_NETHER, dir, damroll(dieroll,dieroll), 2);
                        break;
                case 36: /* ba wate */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_WATER, dir, damroll(dieroll,dieroll), 2);
                        break;
                case 37: /* ba mana */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_MANA, dir, damroll(dieroll,dieroll), 2);
                        break;
                case 38: /* ba dark */
                        if (get_aim_dir(&dir))
                               fire_ball(GF_DARK, dir, damroll(dieroll,dieroll), 2);
                        break;
	        case 42: /* cause1 */
		        if (get_aim_dir(&dir))
		        {
                                fire_bolt(GF_MANA, dir, damroll(dieroll,dieroll));
			}
		        break;
	        case 43: /* cause2 */
		        if (get_aim_dir(&dir))
		        {
                                fire_bolt(GF_MANA, dir, damroll(dieroll + 5,dieroll + 5));
			}
		        break;
	        case 44: /* cause3 */
		        if (get_aim_dir(&dir))
		        {
                                fire_bolt(GF_MANA, dir, damroll(dieroll + 10,dieroll + 10));
			}
		        break;
	        case 45: /* cause4 */
		        if (get_aim_dir(&dir))
		        {
                                fire_bolt(GF_MANA, dir, damroll(dieroll + 15,dieroll + 15));
			}
		        break;
                case 46: /* bo acid */
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_ACID, dir, damroll(dieroll,dieroll) + (p_ptr->lev/3));
                        break;
                case 47: /* bo elec */
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_ELEC, dir, damroll(dieroll,dieroll) + (p_ptr->lev/3));
                        break;
                case 48: /* bo fire */
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_FIRE, dir, damroll(dieroll,dieroll) + (p_ptr->lev/3));
                        break;
                case 49: /* bo cold */
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_COLD, dir, damroll(dieroll,dieroll) + (p_ptr->lev/3));
                        break;
                case 56: /* bo pois */
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_POIS, dir, damroll(dieroll,dieroll) + (p_ptr->lev/3));
                        break;
                case 50: /* bo neth */
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_NETHER, dir, damroll(dieroll,dieroll) + (p_ptr->lev/3));
                        break;
                case 51: /* bo wate */
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_WATER, dir, damroll(dieroll,dieroll) + (p_ptr->lev/3));
                        break;
                case 52: /* bo mana */
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_MANA, dir, damroll(dieroll,dieroll) + (p_ptr->lev/3));
                        break;
                case 53: /* bo plas */
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_PLASMA, dir, damroll(dieroll,dieroll) + (p_ptr->lev/3));
                        break;
                case 54: /* bo ice */
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_ICE, dir, damroll(dieroll,dieroll) + (p_ptr->lev/3));
                        break;
                case 55: /* missile */
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_MISSILE, dir, damroll(dieroll,dieroll) + (p_ptr->lev/3));
                        break;
                case 57: /* blind */
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_CONFUSION, dir, damroll(dieroll,dieroll) + (p_ptr->lev/3));
                        break;
                case 41: /* scare */
			if (get_aim_dir(&dir))
				fear_monster(dir, plev);
                        break;
                case 58: /* conf */
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_CONFUSION, dir, damroll(dieroll,dieroll) + (p_ptr->lev/3));
                        break;
                case 59: /* slow */
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_OLD_SLOW, dir, damroll(dieroll,dieroll) + (p_ptr->lev/3));
                        break;
                case 60: /* hold */
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_OLD_SLEEP, dir, damroll(dieroll,dieroll) + (p_ptr->lev/3));
                        break;
                case 61: /* haste */
			if (!p_ptr->fast)
			{
                                (void)set_fast(randint(20 + (plev) ) + plev);
			}
			else
			{
                                (void)set_fast(p_ptr->fast + randint(5));
			}
                        break;
                case 62: /* hand of doom */
                        if (get_aim_dir(&dir))
		        {
                                fire_bolt(GF_MANA, dir, damroll(dieroll + 20,dieroll + 20) + (p_ptr->lev));
			}
                        break;
                case 63: /* heal */
                        hp_player(damroll(dieroll,dieroll));
                        break;
                case 64: /* Blink */
                        if(special_flag) {msg_print("No teleport on special levels ...");break;}
                        teleport_player(10);
                        break;
                case 65: /* Teleport */
                        if(special_flag) {msg_print("No teleport on special levels ...");break;}
                        teleport_player(plev * 5);
                        break;
                case 66: /* tele to */
                        {
                             int ii,ij;

                             if(special_flag) {msg_print("No teleport on special levels ...");break;}
                             msg_print("You go between. Show the destination to your Dragon.");
                             if (!tgt_pt(&ii,&ij)) return num;
                             p_ptr->energy -= 60 - plev;
                             if (!cave_empty_bold(ij,ii) || (cave[ij][ii].info & CAVE_ICKY) ||
                             (distance(ij,ii,py,px) > plev*20 + 2))
                             {
                                 msg_print("You fail to show the destination correctly!");
                                 p_ptr->energy -= 100;
                                 teleport_player(10);
                             }
                             else teleport_player_to(ij,ii);
                        }
                        break;
                case 67: /* tele away */
                        if(special_flag) {msg_print("No teleport on special levels ...");break;}
                        if (!get_aim_dir(&dir)) return num;
                        (void)fire_beam(GF_AWAY_ALL, dir, plev);
                        break;
		case 68: /* tele level */
                        if(special_flag) {msg_print("No teleport on special levels ...");break;}
			teleport_player_level();
			break;
                case 69: /* darkness */
                        (void)project(-1, 3, py, px, 0, GF_DARK_WEAK, PROJECT_GRID | PROJECT_KILL);
                        /* Unlite up the room */
                        unlite_room(py, px);
                        break;
	        case 88: /* create traps */
		        msg_print("You create traps ...");
			trap_creation();
		        break;
		case 89: /* raise the dead - uses the same code as the
			    nether spell*/
		{
			int item, x, y;
			object_type *o_ptr;

			cptr q, s;

			/* Restrict choices to corpses */
			item_tester_tval = TV_CORPSE;

			/* Get an item */
			q = "Use which corpse? ";
			s = "You have no corpse to use.";
			if (!get_item(&item, q, s, (USE_FLOOR))) break;;

			o_ptr = &o_list[0 - item];

                        if(randint(8)>=5-o_ptr->sval){
                                msg_print("You touch the corpse ... the monster raise from the graveyard!");

                                x=px;
                                y=py;
                                get_pos_player(5, &y, &x);
                                place_monster_one(y, x, o_ptr->pval2, FALSE, TRUE);

                                floor_item_increase(0 - item, -1);
                                floor_item_describe(0 - item);
                                floor_item_optimize(0 - item);
                        }
			break;
		}
                case 70: /* Summon bug */
                                msg_format("You magically code some software bugs.");
				for (k = 0; k < 6; k++)
				{
                                                count += summon_specific_friendly(y, x, rlev, SUMMON_BUG, TRUE);
				}
                        break;
                case 71: /* Summon RNG */
                                msg_format("You magically code some RNGs.");
				for (k = 0; k < 6; k++)
				{
                                                count += summon_specific_friendly(y, x, rlev, SUMMON_RNG, TRUE);
				}
                        break;
                case 72: /* Summon dragonrider */
                                msg_format("You magically summon a DragonRider.");
                                for (k = 0; k < 1; k++)
				{
                                                count += summon_specific_friendly(y, x, rlev, SUMMON_DRAGONRIDER, TRUE);
				}
                        break;
                case 73: /* Summon kin */
                                msg_format("You magically summon some Kins.");
				summon_kin_type = r_ptr->d_char; /* Big hack */
                                for (k = 0; k < 6; k++)
				{
                                                count += summon_specific_friendly(y, x, rlev, SUMMON_KIN, TRUE);
				}
                        break;
                case 74: /* Summon cyber */
                                msg_format("You magically summon a Cyberdemon.");
                                for (k = 0; k < 1; k++)
				{
                                                count += summon_specific_friendly(y, x, rlev, SUMMON_CYBER, TRUE);
				}
                        break;
                case 75: /* Summon monster */
                                msg_format("You magically summon a monster.");
                                for (k = 0; k < 1; k++)
				{
                                                count += summon_specific_friendly(y, x, rlev, 0, TRUE);
				}
                        break;
                case 76: /* Summon monsters */
                                msg_format("You magically summon monsters.");
                                for (k = 0; k < 6; k++)
				{
                                                count += summon_specific_friendly(y, x, rlev, 0, TRUE);
				}
                        break;
                case 77: /* Summon ant */
                                msg_format("You magically summon ants.");
                                for (k = 0; k < 6; k++)
				{
                                                count += summon_specific_friendly(y, x, rlev, SUMMON_ANT, TRUE);
				}
                        break;
                case 78: /* Summon spider */
                                msg_format("You magically summon spiders.");
                                for (k = 0; k < 6; k++)
				{
                                                count += summon_specific_friendly(y, x, rlev, SUMMON_SPIDER, TRUE);
				}
                        break;
                case 79: /* Summon hound */
                                msg_format("You magically summon hounds.");
                                for (k = 0; k < 6; k++)
				{
                                                count += summon_specific_friendly(y, x, rlev, SUMMON_HOUND, TRUE);
				}
                        break;
                case 80: /* Summon hydra */
                                msg_format("You magically summon hydras.");
                                for (k = 0; k < 6; k++)
				{
                                                count += summon_specific_friendly(y, x, rlev, SUMMON_HYDRA, TRUE);
				}
                        break;
                case 81: /* Summon angel */
                                msg_format("You magically summon an angel.");
                                for (k = 0; k < 1; k++)
				{
                                                count += summon_specific_friendly(y, x, rlev, SUMMON_ANGEL, TRUE);
				}
                        break;
                case 82: /* Summon demon */
                                msg_format("You magically summon a demon.");
                                for (k = 0; k < 1; k++)
				{
                                                count += summon_specific_friendly(y, x, rlev, SUMMON_DEMON, TRUE);
				}
                        break;
                case 83: /* Summon undead */
                                msg_format("You magically summon an undead.");
                                for (k = 0; k < 1; k++)
				{
                                                count += summon_specific_friendly(y, x, rlev, SUMMON_UNDEAD, TRUE);
				}
                        break;
                case 84: /* Summon dragon */
                                msg_format("You magically summon a dragon.");
                                for (k = 0; k < 1; k++)
				{
                                                count += summon_specific_friendly(y, x, rlev, SUMMON_DRAGON, TRUE);
				}
                        break;
                case 85: /* Summon hiundead */
                                msg_format("You magically summon greater undeads.");
                                for (k = 0; k < 8; k++)
				{
                                                count += summon_specific_friendly(y, x, rlev, SUMMON_HI_UNDEAD_NO_UNIQUES, TRUE);
				}
                        break;
                case 86: /* Summon hidragon */
                                msg_format("You magically summon greater dragons.");
                                for (k = 0; k < 8; k++)
				{
                                                count += summon_specific_friendly(y, x, rlev, SUMMON_HI_DRAGON_NO_UNIQUES, TRUE);
				}
                        break;
                case 87: /* Summon wraith */
                                msg_format("You magically summon Wraith.");
                                for (k = 0; k < 8; k++)
				{
                                                count += summon_specific_friendly(y, x, rlev, SUMMON_WRAITH, TRUE);
				}
                        break;
        }
        energy_use = 100;
        return num;
}

static void cast_symbiotic_spell(int spell)
{
        object_type *o_ptr;
        monster_race *r_ptr;
        int to_s2=p_ptr->to_s;

        to_s2 = (to_s2==0)?1:to_s2;

        /* Get the carried monster */
        o_ptr = &inventory[INVEN_CARRY];

	switch (spell)
	{
        case 0: /* Minor Symbiotic healing */
                if(o_ptr->k_idx)
                {
                        int max;

                        r_ptr = &r_info[o_ptr->pval];
                        max = maxroll(r_ptr->hdice, r_ptr->hside);
                        o_ptr->pval2 += damroll(2, 6)+to_s2;
                        if(o_ptr->pval2 > max)o_ptr->pval2 = max;

                        msg_print("Your monster is healed");

                        /* Display the monster hitpoints */
                        p_ptr->redraw |= (PR_MH);
                }
                else
                        msg_print("You are not in symbiosis.");
                break;

        case 1: /* Tangled Creepers */
                slow_monsters();
                break;

        case 2: /* Vamiric healing */
        {
                int dummy,plev=p_ptr->lev,dir;                        
                int max;

                r_ptr = &r_info[o_ptr->pval];
                max = maxroll(r_ptr->hdice, r_ptr->hside);

                if (!get_aim_dir(&dir)) return;
                dummy = plev + randint(plev) * MAX(1, plev/10)+(p_ptr->to_s*2);   /* Dmg */
                if (drain_life(dir, dummy))
                {
                        o_ptr->pval2 += dummy;
                        if(o_ptr->pval2 > max)o_ptr->pval2 = max;
                        p_ptr->redraw |= (PR_MH);
                }
                break;
       }

        case 3: /* Life transfer */
                if(o_ptr->k_idx)
                {
                        int max,hp;

                        r_ptr = &r_info[o_ptr->pval];
                        max = maxroll(r_ptr->hdice, r_ptr->hside);

                        hp = damroll(6, 15)+to_s2;
                        if(p_ptr->chp-hp > 0)
                        {
                                o_ptr->pval2 += hp;
                                if(o_ptr->pval2 > max)o_ptr->pval2 = max;
                                p_ptr->chp -= hp;
                                msg_print("Your monster is healed");

                                /* Redraw */
                                p_ptr->redraw |= (PR_HP);

                                /* Window stuff */
                                p_ptr->window |= (PW_PLAYER);

                                /* Display the monster hitpoints */
                                p_ptr->redraw |= (PR_MH);
                        }
                        else
                                msg_print("You can't do this in your weaken state.");
                }
                else
                        msg_print("You are not in symbiosis.");
                break;

        case 4: /* Satisfy Hunger */
                (void)set_food(PY_FOOD_MAX - 1);
                break;

        case 5: /* Symbiotic minor powers */
                if(o_ptr->k_idx) use_symbiotic_power(o_ptr->pval, FALSE, FALSE);
                else msg_print("You are not in symbiosis.");
                break;

        case 6: /* Summon a never-moving pet */
                summon_specific_friendly(py, px, dun_level, SUMMON_MINE,FALSE);
                break;

        case 16: /* Mana healing */
                if(o_ptr->k_idx)
                {
                        int max,hp;
             
                        r_ptr = &r_info[o_ptr->pval];
                        max = maxroll(r_ptr->hdice, r_ptr->hside);

                        hp = damroll(6, 15)+to_s2;
                        if(p_ptr->csp-hp > 0)
                        {
                                o_ptr->pval2 += hp;
                                if(o_ptr->pval2 > max)o_ptr->pval2 = max;
                                p_ptr->csp -= hp;
                                msg_print("Your monster is healed");

                                /* Redraw */
                                p_ptr->redraw |= (PR_MANA);

                                /* Window stuff */
                                p_ptr->window |= (PW_PLAYER);

                                /* Display the monster hitpoints */
                                p_ptr->redraw |= (PR_MH);
                        }
                        else
                                msg_print("You can't do this in your weaken state.");
                }
                else
                        msg_print("You are not in symbiosis.");
                break;

        case 17: /* Summon some never-moving pets */
                {
                int k;
                for(k=0;k<6;k++)
                        summon_specific_friendly(py, px, dun_level, SUMMON_MINE,FALSE);
                }
                break;

        case 18: /* Major Symbiotic healing */
                if(o_ptr->k_idx)
                {
                        int max;

                        r_ptr = &r_info[o_ptr->pval];
                        max = maxroll(r_ptr->hdice, r_ptr->hside);
                        o_ptr->pval2 += damroll(3, 60)+to_s2;
                        if(o_ptr->pval2 > max)o_ptr->pval2 = max;

                        msg_print("Your monster is healed");

                        /* Display the monster hitpoints */
                        p_ptr->redraw |= (PR_MH);
                }
                else
                        msg_print("You are not in symbiosis.");
                break;

        case 19: /* Healing */
                hp_player(damroll(2, 40)+to_s2);
                break;

        case 20: /* Major Symbiotic powers */
                if(o_ptr->k_idx) use_symbiotic_power(o_ptr->pval, TRUE, FALSE);
                else msg_print("You are not in symbiosis.");
                break;

        case 21: /* Use Ennemy's Powers */
        {
                int y,x;
                cave_type *c_ptr;
                monster_type *m_ptr;

                if (!tgt_pt(&x,&y)) return;
                c_ptr = &cave[y][x];

                if (!(c_ptr->m_idx)) break;
                m_ptr = &m_list[c_ptr->m_idx];
                use_symbiotic_power(m_ptr->r_idx, TRUE, FALSE);
                break;
        }

        default:
                msg_format("You cast an unknown Symbiotic spell: %d.", spell);
		msg_print(NULL);
    }
}

static void cast_music_spell(int spell)
{
        int     dir, dummy;
	int	plev = p_ptr->lev;
        int to_s2=p_ptr->to_s;
        to_s2 = (to_s2==0)?1:to_s2;

        if(p_ptr->class_extra1)
        {
                msg_print("You stop singing.");
                p_ptr->class_extra1 = MUSIC_NONE;
        }

        p_ptr->class_extra2 = spell;

	switch (spell)
	{
                case 0: /* Song of slowness */
                       msg_print("You start humming a slow, steady melody...");
                       p_ptr->class_extra1 = MUSIC_SLOW;
		       break;
                case 1:  /* Stop singing */
                       p_ptr->class_extra1 = MUSIC_NONE;
		       break;
                case 2:  /* The note which kill */
                       msg_print("You cry out in an ear-wracking voice...");
                       if (!get_aim_dir(&dir)) return;
                       fire_beam(GF_SOUND, dir,
                              damroll(4 + ((plev - 1) / 5) * p_ptr->to_s, 4));
		       break;
                case 3:  /* Stunning song */
                       msg_print("You weave a pattern of sounds to bewilder and daze...");
                       p_ptr->class_extra1 = MUSIC_STUN;
		       break;
                case 4:  /* Song of life */
                       msg_print("Life flows through you as you sing a song of healing...");
                       p_ptr->class_extra1 = MUSIC_LIFE;
		       break;
                case 5:  /* Song of mind */
                       msg_print("Your quiet music sharpens your sense of hearing...");
                       p_ptr->class_extra1 = MUSIC_MIND;
		       break;
                case 6:  /* Song of lite */
                       msg_print("Your uplifting song brings brightness to dark places...");
                       p_ptr->class_extra1 = MUSIC_LITE;
		       break;
                case 7:  /* Song of fury */
                       msg_print("Your sing of the heroic Elder Days...");
                       p_ptr->class_extra1 = MUSIC_FURY;
		       break;
                case 8:  /* Awareness song */
                       msg_print("As you start singing, you become more aware of the world around you.");
                       p_ptr->class_extra1 = MUSIC_AWARE;
                       break;
                case 9:  /* Song of knowledge */
                       msg_print("You recall the rich lore of the world...");
                       p_ptr->class_extra1 = MUSIC_ID;
                       break;
                case 10:  /* Between note */
                       msg_print("Your voice twists through space...");
                       teleport_player(100);
                       break;
                case 11:  /* The song which kill */
                       msg_print("You call out with a terrible curse...");
                       if (!get_aim_dir(&dir)) return;
                       fire_beam(GF_SOUND, dir,
                              damroll(10 + ((plev - 1) / 5) * p_ptr->to_s, 6));
                       break;
                case 12:  /* song of illusion */
                       msg_print("You weave a pattern of sounds to beguile and confuse...");
                       p_ptr->class_extra1 = MUSIC_ILLUSION;
                       break;
                case 13:  /* wall breaking song */
                       msg_print("You weave a pattern of sounds to contort and shatter...");
                       p_ptr->class_extra1 = MUSIC_WALL;
                       break;
                case 14:  /* song of resistance */
                       msg_print("You sing a song of perserverance against powers...");
                       p_ptr->class_extra1 = MUSIC_RESIST;
                       break;
                case 15:  /* song of time */
                       msg_print("You start singing swift and light folk-tunes...");
                       p_ptr->class_extra1 = MUSIC_TIME;
                       break;
                case 16:  /* Education song */
                {
                        monster_type *m_ptr;
                        monster_race *r_ptr;
                        int m;

                        msg_print("You recall legends of the fair and foul creatures of the world...");

                        if (!get_rep_dir(&dir)) break;
                        if (!cave[py + ddy[dir]][px + ddx[dir]].m_idx) break;
                        m_ptr = &m_list[cave[py + ddy[dir]][px + ddx[dir]].m_idx];
                        r_ptr = &r_info[m_ptr->r_idx];

                        msg_print("Now you have a better knowledge of this monster.");

                        r_ptr->r_wake = r_ptr->r_ignore = MAX_UCHAR;

                        /* Observe "maximal" attacks */
                        for (m = 0; m < 4; m++)
                        {
                        /* Examine "actual" blows */
			if (r_ptr->blow[m].effect || r_ptr->blow[m].method)
			{
				/* Hack -- maximal observations */
				r_ptr->r_blows[m] = MAX_UCHAR;
			}
                        }

                        /* Hack -- maximal drops */
                        r_ptr->r_drop_gold = r_ptr->r_drop_item =
                        (((r_ptr->flags1 & (RF1_DROP_4D2)) ? 8 : 0) +
                         ((r_ptr->flags1 & (RF1_DROP_3D2)) ? 6 : 0) +
                         ((r_ptr->flags1 & (RF1_DROP_2D2)) ? 4 : 0) +
                         ((r_ptr->flags1 & (RF1_DROP_1D2)) ? 2 : 0) +
                         ((r_ptr->flags1 & (RF1_DROP_90))  ? 1 : 0) +
                         ((r_ptr->flags1 & (RF1_DROP_60))  ? 1 : 0));

                        /* Hack -- but only "valid" drops */
                        if (r_ptr->flags1 & (RF1_ONLY_GOLD)) r_ptr->r_drop_item = 0;
                        if (r_ptr->flags1 & (RF1_ONLY_ITEM)) r_ptr->r_drop_gold = 0;

                        /* Hack -- observe many spells */
                        r_ptr->r_cast_inate = MAX_UCHAR;
                        r_ptr->r_cast_spell = MAX_UCHAR;

                        /* Hack -- know all the flags */
                        r_ptr->r_flags1 = r_ptr->flags1;
                        r_ptr->r_flags2 = r_ptr->flags2;
                        r_ptr->r_flags3 = r_ptr->flags3;
                        r_ptr->r_flags4 = r_ptr->flags4;
                        r_ptr->r_flags5 = r_ptr->flags5;
                        r_ptr->r_flags6 = r_ptr->flags6;

                       break;
                }
                case 17:  /* World Contortion */
                       msg_print("Reality whirls wildly as you sing a dizzying melody...");
                       fire_explosion(py, px, GF_AWAY_ALL, p_ptr->lev/15 + 1, 1 + (p_ptr->lev * 3));
                       break;
                case 18:  /* charming note */
                       msg_print("You chant a song of friendship...");
                       if (!get_aim_dir(&dir)) return;
                       fire_beam(GF_CHARM, dir,
                              damroll(10 + ((plev - 1) / 5) * p_ptr->to_s, 5));
                       break;
                case 19:  /* song of death */
                       msg_print("You unleash a furious barrage of sounds...");
                       if (!get_aim_dir(&dir)) return;
                       fire_beam(GF_SOUND, dir,
                              damroll(20 + ((plev - 1) / 5) * p_ptr->to_s, 10));
                       break;
                case 20:  /* vibration note */
                       msg_print("You sing an old song of the dwarven-smiths...");
                       brand_weapon(5); /* brand earthquake */
                       break;
                case 21:  /* vibration song */
                       msg_print("The fury of the Downfall of Numenor lashes out...");
                       p_ptr->class_extra1 = MUSIC_VIBRA;
                       break;
                case 22:  /* song of disruption */
                       msg_print("You sing of the primeval shaping of Middle-earth...");
                       alter_reality();
                       break;
                case 23:  /* Lay of Gil-Galad */
                       msg_print("You chant a powerful, heroic call to arms...");
            for (dummy = 0; dummy < 3 + (plev / 10); dummy++)
            {
                if (randint(10)>3)
                {
                 summon_specific_friendly(py, px, plev*to_s2, SUMMON_NO_UNIQUES, FALSE);
                }
                else
                {
                    if (summon_specific(py, px, plev, 0))
                    {
                        msg_print("A summoned creature gets angry!");
                    }
                }
            }
                       break;
                case 24:  /* hidding song */
                       msg_print("Your song carries you beyond the sight of mortal eyes...");
                       p_ptr->class_extra1 = MUSIC_HIDE;
                       break;
                case 25:  /* shriek of death */
                       msg_print("A chanting dark and fell rises, bearing death to the world...");
                       if (!get_aim_dir(&dir)) return;
                       fire_bolt(GF_SOUND, dir,
                              damroll(100 + ((plev - 1) / 5) * p_ptr->to_s, 20));
                       break;
                case 26:  /* charming song */
                       msg_print("You weave a slow, soothing melody of imploration...");
                       p_ptr->class_extra1 = MUSIC_CHARME;
                       break;
                case 27:  /* between shriek */
                       msg_print("The holy power of the Music of the Ainur enters you...");
                       p_ptr->class_extra1 = MUSIC_HOLY;
                       break;
                case 28:  /* destruction shriek */
                       msg_print("The unmeasurable destructive power of the Oceans crashes around you...");
                       destroy_area(py, px, 10, TRUE);
                       break;
                case 29:  /* song of liberty */
                       msg_print("You recall the valor of Fingolfin's challenge to the Dark Lord...");
                       p_ptr->class_extra1 = MUSIC_LIBERTY;
                       break;
                case 30:  /* song of the Undeads */
                       msg_print("The themes of life and revival are woven into your song...");
                       p_ptr->class_extra1 = MUSIC_RAISE;
                       break;
                case 31:  /* Immaterial song */
                       msg_print("You chant a deep and moving hymn of the shadowy Halls of Mandos...");
                       p_ptr->class_extra1 = MUSIC_SHADOW;
                       break;

                default:
                        msg_format("You sing an unknown song: %d.", spell);
                        msg_print(NULL);
        }
}

static void cast_tribal_spell(int spell)
{
        int     dir, beam;
        int     plev = p_ptr->lev;
        int to_s2=p_ptr->to_s;
        int mto_s2=p_ptr->to_s;

        mto_s2 = (mto_s2==0)?1:mto_s2;

	if (p_ptr->pclass == CLASS_MAGE) beam = plev;
	else if (p_ptr->pclass == CLASS_HIGH_MAGE) beam = plev + 10;
	else beam = plev / 2;

	switch (spell)
	{
                case 0: /* Slumber */
                        fire_ball(GF_OLD_SLEEP, 0, (5 + plev) * mto_s2, 2 + to_s2);
                        break;
                case 1: /* Lightning Bolt */
                        if (!get_aim_dir(&dir)) return;
                        fire_bolt_or_beam(beam-10, GF_ELEC, dir,
                                damroll(2 * mto_s2, 10));
                        break;
                case 2: /* Bewilder */
                        if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_OLD_CONF, dir, (7 + plev) * mto_s2, 1 + to_s2);
                        break;
                case 3: /* Song of Morning */
                        fire_ball(GF_LITE, 0, (10 + plev) * mto_s2, 5 + to_s2);
                        break;
                case 4: /* Recuperation */
                        hp_player(damroll(1 + to_s2, 20));
                        break;
                case 5: /* Meditate */
                        set_paralyzed(p_ptr->paralyzed + damroll(2, 6));
                        fire_ball(GF_MAKE_GLYPH, 0, 1, 4 + to_s2);
                        hp_player(damroll(25 + to_s2, 4));
                        set_fast(p_ptr->fast + damroll(16, 2) + to_s2);
                        do_res_stat(A_INT);
                        project_hack(GF_AWAY_ALL, 10 + to_s2);
                        break;
                case 6: /* Wolf Spirit */
                        if(p_ptr->mimic_form != MIMIC_WOLF)
                                set_mimic(10 + randint(10) + to_s2, MIMIC_WOLF);
                        else
                                set_mimic(p_ptr->tim_mimic + 10 + randint(10) + to_s2, MIMIC_WOLF);
                        break;
                case 7: /* Dress Wounds */
                        hp_player(damroll(3 + to_s2, 15));
                        set_cut(0);
                        break;

                case 8: /* Grow Berries */
                        set_food(p_ptr->food + 3000);
                        break;
                case 9: /* Sense Presence */
                        detect_monsters_normal();
                        break;
                case 10: /* Punishment */
                        fire_ball(GF_MANA, 0, damroll(25 + to_s2, 2), 4 + to_s2);
                        break;
                case 11: /* Life Drain */
                        dec_stat(A_STR, 5, TRUE);
                        dec_stat(A_CON, 5, TRUE);
                        dec_stat(A_DEX, 5, TRUE);
                        if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_MANA, dir, damroll(50 + to_s2, 50), 5 + to_s2);
                        break;
                case 12: /* Beetle Spirit */
                        if(p_ptr->mimic_form != MIMIC_INSECT)
                                set_mimic(10 + randint(10) + to_s2, MIMIC_INSECT);
                        else
                                set_mimic(p_ptr->tim_mimic + 10 + randint(10) + to_s2, MIMIC_INSECT);
                        break;
                case 13: /* Call Nature */
                        summon_specific_friendly(py, px, dun_level, SUMMON_ANT, FALSE);
                        summon_specific_friendly(py, px, dun_level, SUMMON_SPIDER, FALSE);
                        break;
                case 14: /* Bird Spirit */
                        if(p_ptr->mimic_form != MIMIC_SPARROW)
                                set_mimic(10 + randint(10) + to_s2, MIMIC_SPARROW);
                        else
                                set_mimic(p_ptr->tim_mimic + 10 + randint(10) + to_s2, MIMIC_SPARROW);
                        break;
                case 15: /* Winter's Fury */
                        fire_ball(GF_COLD, 0, damroll(10 + to_s2, 10), 4 + to_s2);
                        if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_COLD, dir, damroll(5 + to_s2, 18), 2 + to_s2);
                        break;

                case 16: /* Stun */
                        fire_ball(GF_OLD_CONF, 0, damroll(5 * mto_s2, 5), 6 + to_s2);
                        fire_ball(GF_OLD_SLEEP, 0, damroll(5 * mto_s2, 5), 6 + to_s2);
                        fire_ball(GF_OLD_SLOW, 0, damroll(5 * mto_s2, 5), 6 + to_s2);
                        break;
                case 17: /* Remove Evil */
                        remove_curse();
                        break;
                case 18: /* Destroy Traps */
                        fire_ball(GF_KILL_TRAP, 0, 1, 2 + to_s2);
                        break;
                case 19: /* Lore */
                        ident_spell();
                        break;
                case 20: /* Invoked Destruction */
                        invoke(GF_FIRE, damroll(75, 2));
                        break;
                case 21: /* Ruins */
                        fire_ball(GF_DESTRUCTION, 0, 180, 6 + to_s2);
                        break;
                case 22: /* Fright */
                        scare_monsters();
                        break;
                case 23: /* Winds of Displacement */
                        fire_ball(GF_AWAY_ALL, 0, 180, 1 + to_s2);
                        break;

                case 24: /* Death Chant */
                        dec_stat(A_STR, 15, FALSE);
                        dec_stat(A_CON, 15, FALSE);
                        dec_stat(A_DEX, 15, FALSE);
                        dec_stat(A_INT, 15, FALSE);
                        take_hit(25, "casting Death Chant");
                        fire_ball(GF_MANA, 0, damroll(200 + to_s2, 4), 4 + to_s2);
                        if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_MANA, dir, damroll(40 + to_s2, 20), 2 + to_s2);
                        break;
                case 25: /* Storm's Fury */
                        project_meteor(3, GF_ELEC, damroll(1 + to_s2, 150), PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | PROJECT_THRU);
                        break;
                case 26: /* Call Hydras */
                        summon_specific_friendly(py, px, dun_level, SUMMON_HYDRA, TRUE);
                        break;
                case 27: /* Lifeblood */
                        hp_player(damroll(1000, 2 + to_s2));
                        set_cut(0);
                        set_poisoned(0);
                        set_stun(0);
                        set_shield(p_ptr->shield + 10 + randint(10) + to_s2, 50 + to_s2);
                        do_res_stat(A_STR);
                        do_res_stat(A_INT);
                        do_res_stat(A_WIS);
                        do_res_stat(A_DEX);
                        do_res_stat(A_CON);
                        do_res_stat(A_CHR);
                        lose_exp(p_ptr->exp / 6);
                        break;
                case 28: /* Invoked Winds */
                        invoke(GF_AWAY_ALL, damroll(1, 200));
                        break;
                case 29: /* Dispel Evil */
                        fire_ball(GF_HOLY_FIRE, 0, 100 * mto_s2, 10 + to_s2);
                        dispel_evil(200 * mto_s2);
                        break;
                case 30: /* Mystic Forces */
                        fire_ball(GF_ELEC, 0, damroll(100, 10 + to_s2), 10 + to_s2);
                        if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_MANA, dir, damroll(1 + to_s2, 400), 5 + to_s2);
                        fire_beam(GF_ICE, dir, damroll(1 + to_s2, 100));
                        break;
                case 31: /*  */
                        break;

                default:
                        msg_format("You cast an unknown Tribal spell: %d.", spell);
                        msg_print(NULL);
        }
}

static void cast_druid_spell(int spell)
{
        int     dir, beam;
        int     plev = p_ptr->lev;
        int to_s2=p_ptr->to_s;
        int mto_s2=p_ptr->to_s;

        int amt = 0, att = 0;

        mto_s2 = (mto_s2==0)?1:mto_s2;

	if (p_ptr->pclass == CLASS_MAGE) beam = plev;
	else if (p_ptr->pclass == CLASS_HIGH_MAGE) beam = plev + 10;
	else beam = plev / 2;

	switch (spell)
	{
                case 0: /* Tunnel */
                {
                        magic_type *s_ptr = &realm_info[REALM_DRUID][0];
                        int d, i, min, ox, oy, x, y;
                        int tries = 0, dis = 10 + to_s2;
                        int xx = -1, yy = -1;
                        bool look = TRUE;

                        if(!p_ptr->class_extra5)
                        {
                        amt = get_quantity("What is the minimal amount of mana the tunnel end must have?", 255);

                        att = get_check("Does the mana have to match exactly the specified amount?");
                        
                        if(p_ptr->resist_continuum) {msg_print("The space-time continuum can't be disrupted."); return;}

                        if (p_ptr->anti_tele)
                        {
                                msg_print("A mysterious force prevents you from teleporting!");
                                return;
                        }

                        if (dis > 200) dis = 200; /* To be on the safe side... */

                        /* Minimum distance */
                        min = dis / 2;

                        /* Look until done */
                        while (look)
                        {
                                tries++;

                                /* Verify max distance */
                                if (dis > 200) dis = 200;

                                /* Try several locations */
                                for (i = 0; i < 500; i++)
                                {
                                        /* Pick a (possibly illegal) location */
                                        while (1)
                                        {
                                                y = rand_spread(py, dis);
                                                x = rand_spread(px, dis);
                                                d = distance(py, px, y, x);
                                                if ((d >= min) && (d <= dis)) break;
                                        }

                                        /* Ignore illegal locations */
                                        if (!in_bounds(y, x)) continue;

                                        /* Require "naked" floor space */
                                        if (!cave_naked_bold(y, x)) continue;

                                        /* Require a certain amount of mana */
                                        if (cave[y][x].mana < amt) continue;
                                        if ((att == TRUE) && (cave[y][x].mana > amt)) continue;

                                        /* No teleporting into vaults and such */
                                        if (cave[y][x].info & (CAVE_ICKY)) continue;

                                        /* This grid looks good */
                                        look = FALSE;

                                        /* Stop looking */
                                        break;
                                }

                                /* Increase the maximum distance */
                                dis = dis * 2;

                                /* Decrease the minimum distance */
                                min = min / 2;

                                /* Stop after MAX_TRIES tries */
                                if (tries > MAX_TRIES) return;
                        }

                        /* Sound */
                        sound(SOUND_TELEPORT);

                        /* Save the old location */
                        oy = py;
                        ox = px;

                        /* Move the player */
                        py = y;
                        px = x;

                        /* Absord some mana */
                        if(s_ptr->smana <= amt)
                        {
                                cave[y][x].mana -= s_ptr->smana;
                                p_ptr->csp += s_ptr->smana;
                        }
                        else
                        {
                                cave[y][x].mana -= amt;
                                p_ptr->csp += amt;
                        }

                        /* Redraw the old spot */
                        lite_spot(oy, ox);

                        while (xx < 2)
                        {
                                yy = -1;

                                while (yy < 2)
                                {
                                        if (xx == 0 && yy == 0)
                                        {
                                                /* Do nothing */
                                        }
                                        else
                                        {
                                                if (cave[oy+yy][ox+xx].m_idx)
                                                {
                                                        if ((r_info[m_list[cave[oy+yy][ox+xx].m_idx].r_idx].flags6
                                                            & RF6_TPORT) &&
                                                            !(r_info[m_list[cave[oy+yy][ox+xx].m_idx].r_idx].flags3
                                                            & RF3_RES_TELE))
                                                                /*
                                                                 * The latter limitation is to avoid
                                                                 * totally unkillable suckers...
                                                                 */
                                                        {
                                                                if (!(m_list[cave[oy+yy][ox+xx].m_idx].csleep))
                                                                        teleport_to_player(cave[oy+yy][ox+xx].m_idx);
                                                        }
                                                }
                                        }
                                        yy++;
                                }
                                xx++;
                        }

                        /* Redraw the new spot */
                        lite_spot(py, px);

                        /* Check for new panel (redraw map) */
                        verify_panel();

                        /* Update stuff */
                        p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW);

                        /* Update the monsters */
                        p_ptr->update |= (PU_DISTANCE);

                        /* Window stuff */
                        p_ptr->window |= (PW_OVERHEAD);

                        /* Handle stuff XXX XXX XXX */
                        handle_stuff();
                        }else msg_print("This level has already been drained!");
                        break;
                }
                case 1: /* Canalize Mana */
                        if(p_ptr->class_extra7 == CLASS_CANALIZE_MANA)
                        {
                                p_ptr->class_extra7 = CLASS_NONE;
                                msg_print("You stop canalizing the earth's mana.");
                        }
                        else
                        {
                                msg_print("You begin canalizing in yourself the earth's mana.");
                                p_ptr->class_extra7 = CLASS_CANALIZE_MANA;
                        }
                        break;
                case 2: /* Acid Bolt */
                        if (!get_aim_dir(&dir)) return;
                        fire_bolt(GF_ACID, dir,
                                damroll(3+((plev-5)/4), 4 * mto_s2));
                        break;
                case 3: /* Mana Path */
                        if(!p_ptr->class_extra5)
                        {
                        if(p_ptr->class_extra7 == CLASS_MANA_PATH)
                        {
                                p_ptr->class_extra7 = CLASS_NONE;
                                msg_print("You stop laying a mana path.");

                                p_ptr->update |= PU_BONUS;
                        }
                        else
                        {
                                msg_print("You begin laying a mana path.");

                                /* Ask for the amount of mana to lay down */
                                amt = get_quantity("What amount of mana do you want to lay on each step?", 255);
                                if(!amt) break;

                                /* Ask for laying type */
                                if(get_check("Do you want to absord the old mana before laying?"))
                                        att |= CLASS_MANA_PATH_ERASE;

                                p_ptr->class_extra7 = CLASS_MANA_PATH;
                                p_ptr->class_extra6 = amt + (att << 8) ;

                                p_ptr->update |= PU_BONUS;
                        }
                        }else msg_print("This level has already been drained!");
                        break;
                case 4: /* Forest Generation */
                {
                        int i,j;
                        byte rad = 2 + (plev / 25) + to_s2;

                        for(j = py - rad; j < py + rad; j++)
                        for(i = px - rad; i < px + rad; i++)
                                if((distance(py, px, j, i) <= rad) && in_bounds(j,i))
                                {
                                        if((cave[j][i].mana >= 20) && cave_clean_bold(j, i) && cave_empty_bold(j, i))
                                        {
                                                cave[j][i].mana -= 20;
                                                cave_set_feat(j, i, FEAT_TREES);
                                        }
                                }
                        break;
                }
                case 5: /* Druidistic Acid Beam */
                        if (!get_aim_dir(&dir)) return;
                        fire_druid_beam(GF_ACID, dir,
                                damroll(5+((plev-5)/4), 4 * mto_s2));
                        break;
                case 6: /* Raise Mountains */
                {
                        int i,j;
                        byte rad = 2 + (plev / 25) + to_s2;

                        for(j = py - rad; j < py + rad; j++)
                        for(i = px - rad; i < px + rad; i++)
                                if((distance(py, px, j, i) <= rad) && in_bounds(j,i))
                                {
                                        if((cave[j][i].mana >= 20) && cave_clean_bold(j, i) && cave_empty_bold(j, i))
                                        {
                                                cave[j][i].mana -= 20;
                                                cave_set_feat(j, i, FEAT_MOUNTAIN);
                                        }
                                }
                        break;
                }
                case 7: /* Stone Skin */
                        set_shield(p_ptr->shield + 10 + randint(10) + to_s2, 30 + (p_ptr->to_s * 2));
                        break;

                case 8: /* Infra */
                        set_tim_infra(p_ptr->tim_infra + 10 + randint(15) + to_s2);
                        break;
                case 9: /* Fire Bolt */
                        if (!get_aim_dir(&dir)) return;
                        fire_bolt(GF_FIRE, dir,
                                damroll(6 + ((plev-5)/4), 7 * mto_s2));
                        break;
                case 10: /* Fire Ball */
                        if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_FIRE, dir,
                                (40 + ((plev-5)/4)) * mto_s2, 2 + to_s2);
                        break;
                case 11: /* Enlight Traps */
                        detect_traps();
                        break;
                case 12: /* Fire Beam */
                        if (!get_aim_dir(&dir)) return;
                        fire_beam(GF_ACID, dir,
                                damroll(7 + ((plev-5)/4), 8 * mto_s2));
                        break;
                case 13: /* Druidistic Fire Bolt */
                        if(!p_ptr->class_extra5)
                        {
                        if (!get_aim_dir(&dir)) return;
                        fire_druid_bolt(GF_ACID, dir,
                                damroll(7 + ((plev-5)/4), 8 * mto_s2));
                        }else msg_print("This level has already been drained!");
                        break;
                case 14: /* Druidistic Fire Beam */
                        if(!p_ptr->class_extra5)
                        {
                        if (!get_aim_dir(&dir)) return;
                        fire_druid_beam(GF_ACID, dir,
                                damroll(7 + ((plev-5)/4), 8 * mto_s2));
                        }else msg_print("This level has already been drained!");
                        break;
                case 15: /* Create Lava */
                {
                        int i,j;
                        byte rad = 2 + (plev / 25) + to_s2;

                        for(j = py - rad; j < py + rad; j++)
                        for(i = px - rad; i < px + rad; i++)
                                if((distance(py, px, j, i) <= rad) && in_bounds(j,i))
                                {
                                        if((cave[j][i].mana >= 20) && cave_clean_bold(j, i) && cave_empty_bold(j, i))
                                        {
                                                cave[j][i].mana -= 20;
                                                cave_set_feat(j, i, FEAT_DEEP_LAVA);
                                        }
                                }
                        break;
                }

                case 16: /* Winds of Mana */
                        if(!p_ptr->class_extra5)
                        {
                        if(p_ptr->class_extra7 == CLASS_WINDS_MANA)
                        {
                                p_ptr->class_extra7 = CLASS_NONE;
                                msg_print("You stop expulsing mana winds.");
                        }
                        else
                        {
                                msg_print("You begin expulsing mana winds.");

                                /* Ask for the amount of mana to lay down */
                                amt = get_quantity("What amount of mana do you want to lay on each step?", 255);
                                if(!amt) break;

                                /* Ask for laying type */
                                if(get_check("Do you want to absord the old mana before laying?"))
                                        att |= CLASS_MANA_PATH_ERASE;

                                p_ptr->class_extra7 = CLASS_WINDS_MANA;
                                p_ptr->class_extra6 = amt + (att << 8) ;
                        }
                        }else msg_print("This level has already been drained!");
                        break;
                case 17: /* Summon Air Elem */
                {
                        int xx = px,yy = py;
                        msg_format("You magically summon an Air Elemental.");
                        scatter(&yy, &xx, py, px, 6, 0);
                        place_monster_aux(yy, xx, test_monster_name("Air elemental"), FALSE, FALSE, TRUE);
                        break;
                }
                case 18: /* Wispers from Afar */
                        ident_spell();
                        break;
                case 19: /* The Winds of Manwe */
                        fire_ball(GF_GRAVITY, 0, 10 * mto_s2, 4 + to_s2);
                        break;
                case 20: /* Bird View */
                        wiz_lite_extra();
                        break;
                case 21: /* *Wispers from Afar* */
                        identify_fully();
                        break;
                case 22: /* Windy Speed */
                        set_fast(p_ptr->fast + 5 + randint(5) + to_s2);
                        break;
                case 23: /* The Thunders of Manwe */
                        if(get_check("Do you want it to be a druidistic beam ?"))
                        {
                                if(!p_ptr->class_extra5)
                                {
                                        if (!get_aim_dir(&dir)) return;
                                        fire_druid_beam(GF_ELEC, dir,
                                                damroll(10 + ((plev-5)/4), 10 * mto_s2));
                                }else msg_print("This level has already been drained!");
                        }
                        else
                        {
                                if (!get_aim_dir(&dir)) return;
                                fire_beam(GF_ELEC, dir,
                                        damroll(10 + ((plev-5)/4), 9 * mto_s2));
                        }
                        break;

                case 24: /* Summon Aqua Golems */
                {
                        int xx = px,yy = py;
                        msg_format("You magically summon Aquatic Golems.");
                        scatter(&yy, &xx, py, px, 6, 0);
                        place_monster_aux(yy, xx, test_monster_name("Aquatic golem"), FALSE, TRUE, TRUE);
                        break;
                }
                case 25: /* Walk over the Water */
                        set_walk_water(p_ptr->walk_water + 50 + randint(40) + to_s2);
                        break;
                case 26: /* Flood */
                {
                        int i,j;
                        byte rad = 3 + (plev / 25) + to_s2;

                        for(j = py - rad - 1; j < py + rad + 1; j++)
                        for(i = px - rad - 1; i < px + rad + 1; i++)
                                if((distance(py, px, j, i) <= rad) && in_bounds(j,i))
                                {
                                        if((cave[j][i].mana >= 20) && cave_clean_bold(j, i) && cave_empty_bold(j, i))
                                        {
                                                cave[j][i].mana -= 20;
                                                cave_set_feat(j, i, FEAT_DEEP_WATER);
                                        }
                                }
                                else if((distance(py, px, j, i) <= rad + 1) && in_bounds(j,i))
                                {
                                        if((cave[j][i].mana >= 20) && cave_clean_bold(j, i) && cave_empty_bold(j, i))
                                        {
                                                cave[j][i].mana -= 20;
                                                cave_set_feat(j, i, FEAT_SHAL_WATER);
                                        }
                                }
                        break;
                }
                case 27: /* Summon Water Elems */
                {
                        int xx = px,yy = py;
                        msg_format("You magically summon Water Elementals.");
                        scatter(&yy, &xx, py, px, 6, 0);
                        place_monster_aux(yy, xx, test_monster_name("Water elemental"), FALSE, TRUE, TRUE);
                        break;
                }
                        break;
                case 28: /* Purification */
                        hp_player(300 + (p_ptr->to_s * 20));
                        set_blind(0);
                        set_confused(0);
                        set_poisoned(0);
                        set_stun(0);
                        set_cut(0);
                        break;
                case 29: /* Go Underwater */
                {
                        int ij, ii;

                        if((cave[py][px].feat == FEAT_DEEP_WATER) || (cave[py][px].feat == FEAT_SHAL_WATER))
                        {
                        
                        msg_print("You go underwater, choose a destination.");
                        if (!tgt_pt(&ii,&ij)) return;
                        p_ptr->energy -= 60 - plev;
                        if (!cave_empty_bold(ij,ii) || (cave[ij][ii].info & CAVE_ICKY) ||
                           (distance(ij,ii,py,px) > plev*10 + 2) || !(cave[ij][ii].info & CAVE_MARK) ||
                           ((cave[ij][ii].feat != FEAT_DEEP_WATER) && (cave[ij][ii].feat != FEAT_SHAL_WATER)))
                        {
                                msg_print("You fail to dive correctly!");
                                p_ptr->energy -= 100;
                                teleport_player(10);
                        }
                        else teleport_player_to(ij,ii);
                        } else msg_print("You must be on a water square.");
                        break;
                }
                case 30: /* Tidal Wave */
                        if (!get_aim_dir(&dir)) return;
                        (void)fire_ball(GF_WATER, dir, 400 * mto_s2, 4 + to_s2);
                        break;
                case 31: /* Flood Level */
                        msg_print("The world is being flooded !");
                        p_ptr->class_extra6 |= CLASS_FLOOD_LEVEL;
                        alter_reality();                        
                        break;

                case 32: /* Glyph of Warding */
                        warding_glyph();
                        break;
                case 33: /* Orb of Mana */
                        if (!get_aim_dir(&dir)) return;
                        (void)fire_ball(GF_MANA, dir, damroll(45, 10) * mto_s2 + (plev * 2), 2 + to_s2);
                        break;
                case 34: /* Gather mana */
                        if(!p_ptr->class_extra5)
                        {
                                
                        }else msg_print("This level has already been drained!");
                        break;
                case 35: /* Mirror of Mana */
                        set_tim_reflect(p_ptr->tim_reflect + 10 + randint(10) + to_s2);
                        break;
                case 36: /* Activate Rune of Mana */
                        if(!p_ptr->class_extra5)
                        {
                        if(p_ptr->class_extra7 == CLASS_CANALIZE_MANA)
                        {
                                p_ptr->class_extra7 = CLASS_NONE;
                                msg_print("You stop canalizing the earth's mana.");
                        }
                        else
                        {
                                msg_print("You begin canalizing in yourself the earth's mana.");
                                p_ptr->class_extra6 |= CLASS_CANALIZE_MANA_EXTRA;
                                p_ptr->class_extra7 = CLASS_CANALIZE_MANA;
                        }
                        break;
                        }else msg_print("This level has already been drained!");
                        break;
                case 37: /* Combine the 5 Elements */
			if (!get_aim_dir(&dir)) return;
			fire_bolt_or_beam(beam, GF_FIRE, dir,
                                damroll(10+((plev-5)/4), 8 * mto_s2));
                        fire_bolt_or_beam(beam, GF_COLD, dir,
                                damroll(10+((plev-5)/4), 8 * mto_s2));
                        fire_bolt_or_beam(beam, GF_ACID, dir,
                                damroll(10+((plev-5)/4), 8 * mto_s2));
                        fire_bolt_or_beam(beam, GF_ELEC, dir,
                                damroll(10+((plev-5)/4), 8 * mto_s2));
                        fire_bolt_or_beam(beam, GF_MANA, dir,
                                damroll(10+((plev-5)/4), 8 * mto_s2));
                        break;
                case 38: /* Shield of Mana */
                        set_shield(p_ptr->shield + 20 + randint(10) + to_s2, 100);
                        break;
                case 39: /* Drain Level's Mana */
                        if(!p_ptr->class_extra5)
                        {
                                int i, j;
                                long amt = 0;

                                for(i = 0; i < cur_wid; i++)
                                        for(j = 0; j < cur_hgt; j++)
                                        {
                                                amt += cave[j][i].mana / 75;
                                        }                                                

                                p_ptr->class_extra5 = TRUE;
                                if (!get_aim_dir(&dir)) return;
                                (void)fire_bolt(GF_MANA, dir, amt);
                        }else msg_print("This level has already been drained!");
                        break;

                default:
                        msg_format("You cast an unknown Druid spell: %d.", spell);
                        msg_print(NULL);
        }
}

static void cast_battle_spell(int spell)
{
        int     dir, beam;
        int     plev = p_ptr->lev;
        int to_s2=p_ptr->to_s;
        int mto_s2=p_ptr->to_s;

        mto_s2 = (mto_s2==0)?1:mto_s2;

	if (p_ptr->pclass == CLASS_MAGE) beam = plev;
	else if (p_ptr->pclass == CLASS_HIGH_MAGE) beam = plev + 10;
	else beam = plev / 2;
	
	switch (spell)
	{
           /* Book number 1 */
                case 0: /* Magic Missile */
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam-10, GF_MISSILE, dir,
                                                  damroll(3 + ((plev - 1) / 5), 4 * mto_s2));
                        break;
	   case 1: /* Phase Door */
                        teleport_player(10 * mto_s2);
		       break;
           case 2: /* Detect Monsters */
			(void)detect_monsters_normal();
		       break;
           case 3: /* Confuse Monster */
			if (!get_aim_dir(&dir)) return;
                        (void)confuse_monster(dir, ( plev * 3) / 2 * mto_s2);
			break;
           case 4: /* Scare Monster */
			if (!get_aim_dir(&dir)) return;
                        (void)fear_monsters();
			break;
           case 5: /* Illegible */
                       break;
           case 6: /* Illegible */
			break;
           case 7: /* Create Dagger */
                do_cmd_create_dagger();
                msg_print("You create a dagger!");
                break;

           /* Book number 2 */

                case 8: /* Magic Missile II */
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam-10, GF_MISSILE, dir,
                                                  damroll(5 + ((plev - 1) / 4), 6 * mto_s2));
                        break;
                case 9: /* Berserk I */
                        (void)set_shero(p_ptr->shero + randint(10) + 15 + to_s2);
                        break;
                case 10: /* Curing */
                        msg_print("A good energy surround you!");
                        (void)set_poisoned(0);
                        (void)set_cut(0);
                        break;
                case 11: /* Fire Spear */
			if (!get_aim_dir(&dir)) return;
                        fire_bolt_or_beam(beam-10, GF_FIRE, dir,
                                damroll(8+((plev-5)/4) * mto_s2, 7 * mto_s2));
                        break;
                case 12: /* Cold Spear */
			if (!get_aim_dir(&dir)) return;
                        fire_bolt_or_beam(beam-10, GF_COLD, dir,
                                damroll(8+((plev-5)/4) * mto_s2, 7 * mto_s2));
                        break;
                case 13: /* Elec Spear */
			if (!get_aim_dir(&dir)) return;
                        fire_bolt_or_beam(beam-10, GF_ELEC, dir,
                                damroll(8+((plev-5)/4) * mto_s2, 7 * mto_s2));
                        break;
                case 14: /* Fast I */
                        set_fast(p_ptr->fast + 3 + randint(5) + to_s2);
                        break;
                case 15: /* Create Sword */
                        do_cmd_create_sword();
                        msg_print("You create a sword!");
                        break;

                /* Book Number 3 */
                case 16: /* Heal Wave */
                        msg_print("You feel more healthy!");
                        hp_player(100 + (p_ptr->to_s * 20));
                        break;
                case 17: /* Long Phase door */
                        teleport_player(30 * mto_s2);
                        break;
                case 18: /* Magic Missile III */
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam-10, GF_MISSILE, dir,
                                                  damroll(15 + ((plev - 1) / 3), 10 * mto_s2));
                        break;
                case 19: /* Resistance */
                        msg_print("You feel very resistant...");
                        set_oppose_fire(p_ptr->oppose_fire + randint(20) + 30 + to_s2);
                        set_oppose_cold(p_ptr->oppose_cold + randint(20) + 30 + to_s2);
                        set_oppose_elec(p_ptr->oppose_cold + randint(20) + 30 + to_s2);
                        set_oppose_acid(p_ptr->oppose_cold + randint(20) + 30 + to_s2);
                        set_oppose_ld(p_ptr->oppose_cold + randint(20) + 30 + to_s2);
                        set_oppose_pois(p_ptr->oppose_cold + randint(20) + 30 + to_s2);
                        break;
                case 20: /* Berserk II */
                        (void)set_shero(p_ptr->shero + randint(30) + 30 + to_s2);
                        break;
                case 21: /* Confuse Monster II */
			if (!get_aim_dir(&dir)) return;
                        (void)confuse_monster(dir, ( plev * 10) * mto_s2);
			break;
                case 22: /* Restoration */
                        msg_print("An aura of purity surround you!");
                        hp_player(50 + (p_ptr->to_s * 20));
                        set_blind(0);
                        set_confused(0);
                        set_poisoned(0);
                        set_stun(0);
                        set_cut(0);
                        break;
                case 23: /* Create 2H Sword */
                        do_cmd_create_2hsword();
                        break;

                /* Book Number 4 */

                case 24: /* Fire Spear II */
			if (!get_aim_dir(&dir)) return;
                        fire_bolt_or_beam(beam-10, GF_FIRE, dir,
                                damroll(40, 20 * mto_s2));
                        fire_bolt_or_beam(beam-10, GF_FIRE, dir,
                                damroll(40, 20 * mto_s2));

                        break;
                case 25: /* Cold Spear II */
			if (!get_aim_dir(&dir)) return;
                        fire_bolt_or_beam(beam-10, GF_COLD, dir,
                                damroll(40, 20 * mto_s2));
                        fire_bolt_or_beam(beam-10, GF_COLD, dir,
                                damroll(40, 20 * mto_s2));

                        break;
                case 26: /* Elec Spear II */
			if (!get_aim_dir(&dir)) return;
                        fire_bolt_or_beam(beam-10, GF_ELEC, dir,
                                damroll(40, 20 * mto_s2));
                        fire_bolt_or_beam(beam-10, GF_ELEC, dir,
                                damroll(40, 20 * mto_s2));

                        break;
                case 27: /* Dark Spear */
			if (!get_aim_dir(&dir)) return;
                        fire_bolt_or_beam(beam-10, GF_DARK, dir,
                                damroll(42, 22 * mto_s2));
                        fire_bolt_or_beam(beam-10, GF_DARK, dir,
                                damroll(42, 22 * mto_s2));

                        break;
                case 28: /* Illegible */
		       break;
                case 29: /* Illegible */
		       break;
                case 30: /* Fast II */
                        set_fast(p_ptr->fast + 5 + randint(30) + to_s2);
                        break;
                case 31: /* Illegible */
                         break;

                /* Book number 5 */

                case 32: /* Flaming Sword */
                       do_cmd_create_flaming();
                       msg_print("You create a flaming sword!");
                       break;
                case 33: /* Cold Sword */
                       do_cmd_create_cold();
                       msg_print("You create a cold sword!");
                       break;
                case 34: /* Elec Sword */
                       do_cmd_create_elec();
                       msg_print("You create an electrical sword!");
                       break;
                case 35: /* Dark Spear II */
			if (!get_aim_dir(&dir)) return;
                        fire_bolt_or_beam(beam-10, GF_DARK, dir,
                                damroll(72, 32 * mto_s2));
                        fire_bolt_or_beam(beam-10, GF_DARK, dir,
                                damroll(72, 32 * mto_s2));
                        fire_bolt_or_beam(beam-10, GF_DARK, dir,
                                damroll(72, 32 * mto_s2));
                        break;
                case 36: /* Fire Spear III */
			if (!get_aim_dir(&dir)) return;
                        fire_bolt_or_beam(beam-10, GF_FIRE, dir,
                                damroll(70, 30 * mto_s2));
                        fire_bolt_or_beam(beam-10, GF_FIRE, dir,
                                damroll(70, 30 * mto_s2));
                        fire_bolt_or_beam(beam-10, GF_FIRE, dir,
                                damroll(70, 30 * mto_s2));

                        break;
                case 37: /* Cold Spear III */
			if (!get_aim_dir(&dir)) return;
                        fire_bolt_or_beam(beam-10, GF_COLD, dir,
                                damroll(70, 30 * mto_s2));
                        fire_bolt_or_beam(beam-10, GF_COLD, dir,
                                damroll(70, 30 * mto_s2));
                        fire_bolt_or_beam(beam-10, GF_COLD, dir,
                                damroll(70, 30 * mto_s2));

                        break;
                case 38: /* Elec Spear III */
			if (!get_aim_dir(&dir)) return;
                        fire_bolt_or_beam(beam-10, GF_ELEC, dir,
                                damroll(70, 30 * mto_s2));
                        fire_bolt_or_beam(beam-10, GF_ELEC, dir,
                                damroll(70, 30 * mto_s2));
                        fire_bolt_or_beam(beam-10, GF_ELEC, dir,
                                damroll(70, 30 * mto_s2));

                        break;
                case 39: /* Super Defense Sword */
                        do_cmd_create_super_defender();
                        msg_print("You create a powerful, defensive sword!");
                        break;

                /* Booki Number 6(last one) */
                case 40: /* Berserk III */
                        (void)set_shero(p_ptr->shero + randint(200) + 150 + to_s2);
                        break;
                case 41: /* Fast III */
                        set_fast(p_ptr->fast + 10 + randint(100) + to_s2);
                        break;
                case 42: /* Full Restore */
                        msg_print("A white light of health surround you!");
                        hp_player(800 + (p_ptr->to_s * 20));
                        set_blind(0);
                        set_confused(0);
                        set_poisoned(0);
                        set_stun(0);
                        set_cut(0);
                        break;
                case 43: /* Blast Of Devastation (most powerful attack spell) */
				if (!get_aim_dir(&dir)) return;
                                fire_bolt_or_beam(beam-10, GF_MANA, dir,
                                                  damroll(100 + ((plev - 1)), 80 * mto_s2));
                                fire_bolt_or_beam(beam-10, GF_MANA, dir,
                                                  damroll(100 + ((plev - 1)), 80 * mto_s2));
                                fire_bolt_or_beam(beam-10, GF_MANA, dir,
                                                  damroll(100 + ((plev - 1)), 80 * mto_s2));
                        break;
                case 44: /* Invulnerability */
                        (void)set_invuln(p_ptr->invuln + randint(50)+ to_s2 + 30);
                        break;
           default:
                   msg_format("You cast an unknown Battle spell: %d.", spell);
		   msg_print(NULL);
	   }
}


/*
 * Basicaly cast the given spell of the given realm
 */
void cast_spell(int realm, int spell)
{
		switch (realm)
		{
                case REALM_VALARIN: /* * VALARIN * */
                        cast_valarin_spell(spell);
			break;
                case REALM_MAGERY: /* * MAGERY * */
                        cast_magery_spell(spell);
			break;
                case REALM_SHADOW: /* * SHADOW * */
                        cast_shadow_spell(spell);
			break;
		case REALM_CHAOS: /* * CHAOS * */
			cast_chaos_spell(spell);
			break;
                case REALM_NETHER: /* * NETHER * */
                        cast_nether_spell(spell);
			break;
                case REALM_CRUSADE: /* CRUSADE */
                        cast_crusade_spell(spell);
			break;
                case REALM_SIGALDRY: /* SIGALDRY */
                        cast_sigaldry_spell(spell);
			break;
                case REALM_SYMBIOTIC: /* SYMBIOTIC */
                        cast_symbiotic_spell(spell);
			break;
                case REALM_MUSIC: /* MUSIC */
                        cast_music_spell(spell);
			break;
                case REALM_MAGIC: /* MAGIC */
                        cast_magic_spell(spell);
			break;
                case REALM_PRAYER: /* PRAYER */
                        cast_prayer_spell(spell);
			break;
                case REALM_ILLUSION: /* ILLUSION */
                        cast_illusion_spell(spell);
			break;
                case REALM_TRIBAL: /* TRIBAL */
                        cast_tribal_spell(spell);
			break;
                case REALM_DRUID: /* DRUIDISTIC */
                        cast_druid_spell(spell);
			break;
		    case REALM_BATTLE: /* BATTLE */
                        cast_battle_spell(spell);
                        break;
		default:
			msg_format("You cast a spell from an unknown realm: realm %d, spell %d.", realm, spell);
			msg_print(NULL);
		}
}

/*
 * Cast a spell
 */
/* NEWANGBAND: A new system! */
/* void do_cmd_cast(void)
{
        char ch = 0;
        if (p_ptr->confused)
        {
                msg_print("You cannot cast spells while confused!");
                return;
        }
        if (!get_com("[E]lemental, [B]attle, [H]ealing, [C]urse, [V]ision, [N]ature ", &ch)) return;
        if (ch == 'E' || ch == 'e')
        {
                cast_elemental_spell();
        }
        if (ch == 'B' || ch == 'b')
        {
                cast_battl_spell();
        }
        if (ch == 'H' || ch == 'h')
        {
                cast_healing_spell();
        }
        if (ch == 'C' || ch == 'c')
        {
                cast_curse_spell();
        }
        if (ch == 'V' || ch == 'v')
        {
                cast_vision_spell();
        }
        if (ch == 'N' || ch == 'n')
        {
                cast_nature_spell();
        }

	p_ptr->redraw |= (PR_MANA);
	
	p_ptr->window |= (PW_PLAYER);
	p_ptr->window |= (PW_SPELL);
        update_and_handle();
} */

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

/* Activation code for The Staff Of Balance */
void do_cmd_staff_balance()
{
	char ch = 0;
	int amber_power = 0;
	int dir = 0;
      
	msg_print("You invoke the powers of the staff!");
	  while (TRUE)
			{
                        if (!get_com("[C]reation, [B]arrier Shield, [M]utate, [P]lace Barrier, [T]rap ", &ch))
				{
					amber_power = 0;
					break;
				}

                                if (ch == 'C' || ch == 'c')
				{
					amber_power = 1;
					break;
				}

                                if (ch == 'B' || ch == 'b')
				{
					amber_power = 2;
					break;
				}

                                if (ch == 'M' || ch == 'm')
				{
                                        amber_power = 4;
					break;
				}

                                if (ch == 'P' || ch == 'p')
				{
                                        amber_power = 5;
					break;
				}

                                if (ch == 'T' || ch == 't')
				{
                                        amber_power = 6;
					break;
				}


			}

        if (amber_power == 1)
        {
        int i,j;
        byte rad = 8 + rand_int(10);

        msg_print("You create a new land...");
        for(j = py - rad; j < py + rad + 1; j++)
        for(i = px - rad; i < px + rad + 1; i++)
        if((distance(py, px, j, i) <= rad) && in_bounds(j,i))
        {
               
                        if (rand_int(20) >= 15)
                        {
                           
                           cave_set_feat(j, i, FEAT_DEEP_WATER);
                        }
                        else if (rand_int(20) >= 10)
                        {
                           
                           cave_set_feat(j, i, FEAT_SHAL_LAVA);
                        }
                        else if (rand_int(20) >= 5)
                        {
                           
                           cave_set_feat(j, i, FEAT_TREES);
                        }
                        else
                        {
                           
                           cave_set_feat(j, i, FEAT_SHAL_WATER);
                        }


               
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

        if (amber_power == 4)
        {
             msg_print("You transform yourself...you are no loner the same!");
             p_ptr->stat_add[A_STR] += rand_int(5) - rand_int(5);
             p_ptr->stat_add[A_INT] += rand_int(5) - rand_int(5);
             p_ptr->stat_add[A_WIS] += rand_int(5) - rand_int(5);
             p_ptr->stat_add[A_DEX] += rand_int(5) - rand_int(5);
             p_ptr->stat_add[A_CON] += rand_int(5) - rand_int(5);
             p_ptr->stat_add[A_CHR] += rand_int(5) - rand_int(5);

        }

        if (amber_power == 5)
        {
        if (p_ptr->csp >= 10)
        {
        int i,j;
        int letsbuild = 1;
        msg_print("Place where? ");
        if (!tgt_pt(&i, &j)) return;
        cave_set_feat(j, i, 188);
        msg_print("You created a powerful magic barrier!");
        p_ptr->csp -= 10;
        }
        else msg_print("You need at least 10 mana points...");
        }

        if (amber_power == 6)
        {
        if (p_ptr->csp >= 500)
        {
        int i,j, x, y;
        byte wrad = 1;
        msg_print("Where do you want to create the cage?");
        if (!tgt_pt(&x, &y)) return;
        msg_print("You create a magic barrier cage!");
        for(j = y - wrad; j < y + wrad + 1; j++)
        for(i = x - wrad; i < x + wrad + 1; i++)
        cave_set_feat(j, i, 188);
        p_ptr->csp -= 500;
        }       
        else msg_print("You need at least 500 spell points!");

        }

}

void do_cmd_flooding()
{
        int i,j;
        byte wrad = 1;
        
        for(j = py - wrad; j < py + wrad + 1; j++)
        for(i = px - wrad; i < px + wrad + 1; i++)
        if((distance(py, px, j, i) <= wrad) && in_bounds(j,i))
        {
                 if (cave[j][i].feat != 6 && cave[j][i].feat != 7)
                 cave_set_feat(j, i, FEAT_SHAL_WATER);
               
        }
}

void wave_kick()
{
               int dir;
               if (!get_aim_dir(&dir)) return;
               fire_bolt_or_beam(p_ptr->lev, GF_MANA, dir, p_ptr->to_d * 10);
               p_ptr->chp = p_ptr->chp / 2;
               update_and_handle();
}

void skatter_enchant_bow()
{
                int item;
                object_type             *o_ptr;
                cptr q, s;
                u32b f1, f2, f3, f4;

                /* Restrict choices to bows */
                item_tester_tval = TV_BOW;

                /* Get an item */
                q = "Enchant which bow? ";
                s = "You have no bows!";
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
                        msg_print("This bow has already been enchanted!");
                }
                else
                {
                msg_print("You enchant your bow!");
                apply_magic(o_ptr, p_ptr->lev, TRUE, TRUE, TRUE, FALSE);
                identify_fully_aux(o_ptr);
                object_aware(o_ptr);
                object_known(o_ptr);
                o_ptr->ident |= (IDENT_MENTAL);
                /* This is to prevent players to sell this bow */
                o_ptr->ident |= (IDENT_BROKEN);
                }

}

void firelord_fireball()
{
        int dir;
        if (!get_aim_dir(&dir)) return;
        msg_print("You cast a Fire Ball!");
        fire_ball(GF_FIRE, dir, p_ptr->chp, 3);
        p_ptr->chp -= p_ptr->chp / 3;
        update_and_handle();
}

void firelord_firestorm()
{
        int dir;
        if (!get_aim_dir(&dir)) return;
        msg_print("You create a powerful storm!");
        fire_ball(GF_PLASMA, dir, p_ptr->chp * 3, p_ptr->lev / 10 + 3);
        p_ptr->chp -= p_ptr->chp / 3;
        update_and_handle();
}

void skatter_exploding_arrow()
{
                int item, explosiontype, dir;
                s32b arrowpower;
                u32b f1, f2, f3, f4;

                object_type             *o_ptr;
                cptr q, s;

                /* Restrict choices to arrows */
                item_tester_tval = TV_ARROW;

                /* Get an item */
                q = "Use which arrow? ";
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
                object_flags(o_ptr, &f1, &f2, &f3, &f4);
                arrowpower = get_quantity("Use how much mana?", p_ptr->csp);
                if (arrowpower < 10)
                {
                        msg_print("You need to use at least 10 mana points!");
                        return;
                }
                p_ptr->csp -= arrowpower;
                arrowpower += damroll(o_ptr->dd, o_ptr->ds);
                arrowpower += o_ptr->to_d;
                arrowpower *= examine_bow();
                if (f1 & (TR1_BRAND_FIRE)) explosiontype = GF_FIRE;
                else if (f1 & (TR1_BRAND_COLD)) explosiontype = GF_COLD;
                else if (f1 & (TR1_BRAND_ELEC)) explosiontype = GF_ELEC;
                else if (f1 & (TR1_BRAND_ACID)) explosiontype = GF_ACID;
                else if (f1 & (TR1_BRAND_POIS)) explosiontype = GF_POIS;
                else if (f4 & (TR4_BRAND_DARK)) explosiontype = GF_DARK;
                else if (f4 & (TR4_BRAND_LIGHT)) explosiontype = GF_LITE;
                else explosiontype = GF_MANA;
                if (!get_aim_dir(&dir)) return;
                fire_ball(explosiontype, dir, arrowpower * (p_ptr->lev), 2);
            
                inven_item_increase(item, -1);
                inven_item_optimize(item);
                update_and_handle();

}

/* It is now a Chain attack... */
void hellqueen_mana_blast()
{
        int dir;
        s32b beampower;

        beampower = get_quantity("Use how much mana?", p_ptr->csp);
        if (!get_aim_dir(&dir)) return;
        chain_attack(dir, GF_MANA, beampower, 1, 20);
        p_ptr->csp -= beampower;
        energy_use = 100;
}
void devastation_beam()
{
        int dir;

        if (p_ptr->csp >= 180)
        {
        if (!get_aim_dir(&dir)) return;
        fire_beam(GF_MANA, dir, 50000);
        p_ptr->csp -= 180;
        energy_use = 100;
        }
        else msg_print("You don't have enough mana!");
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

/* Add poison brand to a dagger(or any sword-type weapons) */
void assassin_poison_weapon()
{
                int item;
                object_type             *o_ptr;
                cptr q, s;
                u32b f1, f2, f3, f4;

                /* Restrict choices to weapons */
                item_tester_hook = item_tester_hook_weapon;

                /* Get an item */
                q = "Poison which weapon? ";
                s = "You have no valid weapons!";
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
                if (!(o_ptr->art_flags1) && !(o_ptr->art_flags2) && !(o_ptr->art_flags3) && !(o_ptr->art_flags4))
                {
                        msg_print("You coat your weapon with poison!");
                        o_ptr->art_flags1 |= (TR1_BRAND_POIS);
                        if (p_ptr->abilities[(CLASS_ROGUE * 10) + 5] >= 14) add_slay_brand(o_ptr);
                        identify_fully_aux(o_ptr);
                        object_aware(o_ptr);
                        object_known(o_ptr);
                        o_ptr->ident |= (IDENT_MENTAL);
                        /* This is to prevent players to sell this weapon */
                        o_ptr->ident |= (IDENT_BROKEN);
                }
                else msg_print("You cannot use this on magical weapons!");
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
                explosionpower *= p_ptr->multiplier;
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

        dam += (dam * (p_ptr->skill_spellcraft * 6)) / 100;
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
                explosionpower *= p_ptr->lev;
                explosionpower *= p_ptr->multiplier;
                explosionpower += o_ptr->to_d * 10;
                explosionpower *= o_ptr->pval;
                explosionpower *= ((p_ptr->abilities[(CLASS_ELEM_LORD * 10) + 6] / 2)+1);

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
                explosionpower *= p_ptr->multiplier;
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
                explosionpower *= p_ptr->multiplier;
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

void call_dryad()
{
        int x, numnymphs;

        numnymphs = (p_ptr->abilities[(CLASS_RANGER * 10) + 6] / 5) + 1;

        for (x = 0; x < numnymphs; x++)
        {
                summon_specific_friendly(py, px, 1, 836, TRUE);
        }
        msg_print("You beautiful Nymphs!");
}

/* Used by the Nature spell "Un-Evolve" */
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
                        chosenbody = randint(1102);
                        chosenbody += 19;
                        r_ptr = &r_info[chosenbody];
                        /* Must have the same d_char */
                        if (b_ptr->d_char == r_ptr->d_char) 
                        {                                    
                                /* Must be LOWER than the monster's current rlev */
                                /* We're talking about evolution here... */
                                if ((r_ptr->level < b_ptr->level) && (!(r_ptr->flags1 & (RF1_UNIQUE)))) okaysignal = TRUE;
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
                (void)project(0, 4, y, x, dam, GF_SHARDS, flg);
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

/* New leader ability, but can be used by any other functions */
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
                        chosenbody = randint(3000);
                        chosenbody += 19;
                        r_ptr = &r_info[chosenbody];
                        /* Must have the same d_char */
                        if (b_ptr->d_char == r_ptr->d_char) 
                        {                                    
                                /* Must be HIGHER than the monster's current rlev */
                                /* We're talking about evolution here... */
                                if ((r_ptr->level > b_ptr->level) && (r_ptr->level <= m_ptr->level) && (!(r_ptr->flags1 & (RF1_UNIQUE)))) okaysignal = TRUE;
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
                        chances = (m_ptr->level - p_ptr->stat_ind[A_WIS]);
                        if (randint(100) >= chances)
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
        if ((r_ptr->flags3 & (RF3_IM_FIRE)) || (r_ptr->flags4 & (RF4_BR_FIRE)))
                o_ptr->art_flags2 |= (TR2_RES_FIRE);
        if ((r_ptr->flags3 & (RF3_IM_COLD)) || (r_ptr->flags4 & (RF4_BR_COLD)))
                o_ptr->art_flags2 |= (TR2_RES_COLD);
        if ((r_ptr->flags3 & (RF3_IM_ELEC)) || (r_ptr->flags4 & (RF4_BR_ELEC)))
                o_ptr->art_flags2 |= (TR2_RES_ELEC);
        if ((r_ptr->flags3 & (RF3_IM_ACID)) || (r_ptr->flags4 & (RF4_BR_ACID)))
                o_ptr->art_flags2 |= (TR2_RES_ACID);
        if ((r_ptr->flags3 & (RF3_IM_POIS)) || (r_ptr->flags4 & (RF4_BR_POIS)))
                o_ptr->art_flags2 |= (TR2_RES_POIS);
        if ((r_ptr->flags3 & (RF3_IM_FIRE)) && (r_ptr->flags4 & (RF4_BR_FIRE)))
                o_ptr->art_flags2 |= (TR2_IM_FIRE);
        if ((r_ptr->flags3 & (RF3_IM_COLD)) && (r_ptr->flags4 & (RF4_BR_COLD)))
                o_ptr->art_flags2 |= (TR2_IM_COLD);
        if ((r_ptr->flags3 & (RF3_IM_ELEC)) && (r_ptr->flags4 & (RF4_BR_ELEC)))
                o_ptr->art_flags2 |= (TR2_IM_ELEC);
        if ((r_ptr->flags3 & (RF3_IM_ACID)) && (r_ptr->flags4 & (RF4_BR_ACID)))
                o_ptr->art_flags2 |= (TR2_IM_ACID);
        if ((r_ptr->flags3 & (RF3_RES_NETH)) || (r_ptr->flags4 & (RF4_BR_NETH)))
                o_ptr->art_flags2 |= (TR2_RES_NETHER);
        if ((r_ptr->flags3 & (RF3_RES_NEXU)) || (r_ptr->flags4 & (RF4_BR_NEXU)))
                o_ptr->art_flags2 |= (TR2_RES_NEXUS);
        if (r_ptr->flags3 & (RF3_RES_DISE))
                o_ptr->art_flags2 |= (TR2_RES_DISEN);
        if (r_ptr->flags3 & (RF3_NO_FEAR))
                o_ptr->art_flags2 |= (TR2_RES_FEAR);
        if (r_ptr->flags3 & (RF3_NO_STUN))
                o_ptr->art_flags2 |= (TR2_FREE_ACT);
        if ((r_ptr->flags3 & (RF3_NO_CONF)) || (r_ptr->flags4 & (RF4_BR_CONF)))
                o_ptr->art_flags2 |= (TR2_RES_CONF);
        if (r_ptr->flags4 & (RF4_BR_CHAO) || r_ptr->flags7 & (RF7_RES_CHAOS))
                o_ptr->art_flags2 |= (TR2_RES_CHAOS);
        if (r_ptr->flags4 & (RF4_BR_LITE) || r_ptr->flags7 & (RF7_RES_LITE)) {
                o_ptr->art_flags2 |= (TR2_RES_LITE);
                o_ptr->art_flags3 |= (TR3_LITE);
        }
        if (r_ptr->flags4 & (RF4_BR_DARK) || r_ptr->flags7 & (RF7_RES_DARK))
                o_ptr->art_flags2 |= (TR2_RES_DARK);
        if (r_ptr->flags4 & (RF4_BR_SOUN) || r_ptr->flags7 & (RF7_RES_SOUND))
                o_ptr->art_flags2 |= (TR2_RES_SOUND);
        if (r_ptr->flags4 & (RF4_BR_SHAR) || r_ptr->flags7 & (RF7_RES_SHARDS))
                o_ptr->art_flags2 |= (TR2_RES_SHARDS);
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
        if (r_ptr->flags2 & (RF2_POWERFUL))
                o_ptr->art_flags2 |= (TR2_LIFE);
        if (r_ptr->flags2 & (RF2_SMART))
                o_ptr->art_flags1 |= (TR1_INT);
        if (r_ptr->flags2 & (RF2_KILL_WALL))
                o_ptr->art_flags1 |= (TR1_STR);
        if (r_ptr->flags6 & (RF6_HASTE))
                o_ptr->art_flags1 |= (TR1_SPEED);
        if (is_weapon(o_ptr) || o_ptr->tval == TV_ARROW || o_ptr->tval == TV_BOLT || o_ptr->tval == TV_SHOT)
        {
                /* Observe "maximal" attacks */
                for (x = 0; x < 4; x++)
                {
                        /* Examine "actual" blows */
                        if (r_ptr->blow[x].effect)
                        {
                                if (r_ptr->blow[x].effect == RBE_FIRE)
                                        o_ptr->art_flags1 |= (TR1_BRAND_FIRE);
                                if (r_ptr->blow[x].effect == RBE_COLD)
                                        o_ptr->art_flags1 |= (TR1_BRAND_COLD);
                                if (r_ptr->blow[x].effect == RBE_ELEC)
                                        o_ptr->art_flags1 |= (TR1_BRAND_ELEC);
                                if (r_ptr->blow[x].effect == RBE_ACID)
                                        o_ptr->art_flags1 |= (TR1_BRAND_ACID);
                                if (r_ptr->blow[x].effect == RBE_POISON)
                                        o_ptr->art_flags1 |= (TR1_BRAND_POIS);                                
                        }                                                 

                }
                /* Enchance the damages...maybe */
                if (o_ptr->tval == TV_SWORD || o_ptr->tval == TV_HAFTED || o_ptr->tval == TV_POLEARM ||
                o_ptr->tval == TV_AXE || o_ptr->tval == TV_ZELAR_WEAPON ||
                o_ptr->tval == TV_HELL_STAFF || o_ptr->tval == TV_MSTAFF || o_ptr->tval == TV_SWORD_DEVASTATION ||
                o_ptr->tval == TV_VALKYRIE_SPEAR)
                {
                        if (r_ptr->blow[0].d_dice >= 6) o_ptr->dd += r_ptr->blow[0].d_dice / 6;
                        if (r_ptr->blow[0].d_side >= 6) o_ptr->ds += r_ptr->blow[0].d_side / 6;
                }
                if (o_ptr->tval == TV_ARROW || o_ptr->tval == TV_BOLT || o_ptr->tval == TV_SHOT || o_ptr->tval == TV_ROD)
                {
                        if (r_ptr->blow[0].d_dice >= 12) o_ptr->dd += r_ptr->blow[0].d_dice / 12;
                        if (r_ptr->blow[0].d_side >= 12) o_ptr->ds += r_ptr->blow[0].d_side / 12;
                }                
                if (o_ptr->tval == TV_DAGGER)
                {
                        if (r_ptr->blow[0].d_dice >= 20) o_ptr->dd += r_ptr->blow[0].d_dice / 20;
                        if (r_ptr->blow[0].d_side >= 20) o_ptr->ds += r_ptr->blow[0].d_side / 20;
                }                
        }
        if (o_ptr->tval == TV_SOFT_ARMOR || o_ptr->tval == TV_HARD_ARMOR || o_ptr->tval == TV_DRAG_ARMOR)
        {
                /* Bind a monster's soul to an armor! */
                o_ptr->ac += (r_ptr->ac / 3);
        }
        if (o_ptr->tval == TV_SOFT_ARMOR || o_ptr->tval == TV_HARD_ARMOR || o_ptr->tval == TV_DRAG_ARMOR)
        {
                /* Bind a monster's soul to an armor! */
                o_ptr->ac += (r_ptr->ac / 3);
        }
        if (o_ptr->tval == TV_BOOTS || o_ptr->tval == TV_GLOVES || o_ptr->tval == TV_HELM
        || o_ptr->tval == TV_CROWN || o_ptr->tval == TV_SHIELD || o_ptr->tval == TV_CLOAK
        || o_ptr->tval == TV_ARM_BAND)
        {
                /* Bind a monster's soul to a glove, boots, etc...! */
                /* MUCH lower than an armor, but still good. */
                o_ptr->ac += (r_ptr->ac / 10);
        }

        o_ptr->to_h += r_ptr->level;
        o_ptr->to_d += r_ptr->level;
        o_ptr->pval = 1 + (p_ptr->abilities[(CLASS_SOUL_GUARDIAN * 10) + 2] / 3);
        if (o_ptr->pval < 1) o_ptr->pval = 1;
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
        object_type     *o_ptr = &inventory[INVEN_WIELD];

        if (o_ptr->timeout > 0)
        {
                msg_print("You can only summon one weapon at time!");
                return;
        }
        if (o_ptr->tval > 0) inven_takeoff(INVEN_WIELD, 255, FALSE);

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

        msg_print("A glowing, holy mace appear in your hand!");
        object_copy(&inventory[INVEN_WIELD], q_ptr);
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

        /* Restrict choices to weapons */
        item_tester_hook = item_tester_hook_weapon_polearm;

        /* Get an item */
        q = "Use which polearm? ";
        s = "You have no polearms!";
        if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP))) return;

        /* Get the item */
        o_ptr = &inventory[item];

        trapdam = ((maxroll(o_ptr->dd, o_ptr->ds) * 3) * p_ptr->abilities[(CLASS_ROGUE * 10) + 4]) * p_ptr->lev;

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

        /* Restrict choices to weapons */
        item_tester_hook = item_tester_hook_weapon;

        /* Get an item */
        q = "Use which weapon? ";
        s = "You have no weapons!";
        if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP))) return;

        /* Get the item */
        o_ptr = &inventory[item];

        object_flags(o_ptr, &f1, &f2, &f3, &f4);

        if (f1 & (TR1_BRAND_POIS))
        {

                trapdam = (maxroll(o_ptr->dd, o_ptr->ds) * ((p_ptr->abilities[(CLASS_ROGUE * 10) + 7]) + 1)) * p_ptr->lev;

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
        place_field(FEAT_VINE_FIELD, rad, ii, ij, 0);        
}

/* Thorned Vines ability */
void ranger_thorned_vines()
{
        int fieldtype = 0;
        int ii, ij;
        msg_print("Place where? ");
        if (!tgt_pt(&ii,&ij)) return;
        place_field(FEAT_THORNED_VINES, (p_ptr->abilities[(CLASS_RANGER * 10) + 7] / 30) + 2, ii, ij, (p_ptr->lev * (p_ptr->abilities[(CLASS_RANGER * 10) + 7] * 5)));
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
        object_type     *o_ptr = &inventory[INVEN_WIELD];

        if (o_ptr->timeout > 0)
        {
                msg_print("You can only summon one weapon at time!");
                return;
        }
        if (o_ptr->tval > 0) inven_takeoff(INVEN_WIELD, 255, FALSE);

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
        object_copy(&inventory[INVEN_WIELD], q_ptr);

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
                        o_ptr->art_flags2 |= (TR2_RES_LITE);
                }
                if (p_ptr->abilities[(CLASS_PALADIN * 10) + 8] >= 10)
                {
                        o_ptr->art_flags2 |= (TR2_RES_DARK);
                        o_ptr->art_flags2 |= (TR2_RES_NETHER);
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
        s32b dam = p_ptr->to_d * ((p_ptr->abilities[(CLASS_MONK * 10) + 3] / 4) + 1);
        monster_race *r_ptr = &r_info[m_ptr->r_idx]; 
        cave_type *c_ptr;
        if (!tgt_pt(&tgt_x,&tgt_y)) return;
        if (cave_empty_bold(tgt_y,tgt_x) && distance(tgt_y, tgt_x, py, px) <= rad)
        {
                if ((p_ptr->stat_ind[A_STR] * 100) >= r_ptr->weight)
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
        int slot;

        if (itemtval == TV_SWORD || itemtval == TV_HAFTED || itemtval == TV_POLEARM || itemtval == TV_AXE || itemtval == TV_DAGGER || itemtval == TV_ROD)
        {
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
        o_ptr = &inventory[slot];

        if (o_ptr->timeout > 0)
        {
                msg_print("You can only summon one item of this type at time!");
                return;
        }
        if (o_ptr->tval > 0) inven_takeoff(slot, 255, FALSE);

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
                msg_print("You bless your armor with divine power!");
                o_ptr->to_h += p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 3] * 2;
                o_ptr->to_d += p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 3] * 2;
                o_ptr->pval = (p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 3] / 5) + 1;
                o_ptr->ds += p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 3] / 10;
                if (p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 3] >= 50) o_ptr->dd += 1;

                o_ptr->art_flags3 |= (TR3_LITE);
                o_ptr->art_flags4 |= (TR4_BRAND_LIGHT);
                o_ptr->art_flags4 |= (TR4_INDESTRUCTIBLE);

                if (p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 3] >= 5)
                {
                        o_ptr->art_flags2 |= (TR2_RES_LITE);
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
                                if (craftype == 1 && p_ptr->skill_alchemy < (k_ptr->level / 2))
                                {
                                        msg_format("You failed to make the item. You need a Alchemy skill of at least %d !", k_ptr->level);
                                        return;
                                }
                                if (craftype == 2 && p_ptr->skill_crafting < (k_ptr->level / 2))
                                {
                                        msg_format("You failed to make the item. You need a Crafting skill of at least %d !", k_ptr->level);
                                        return;
                                }
                                if (craftype == 3 && (p_ptr->skill_alchemy < (k_ptr->level / 4) || p_ptr->skill_crafting < (k_ptr->level / 4)))
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
                                        o_ptr->dd += (o_ptr->dd * p_ptr->skill_crafting) / 100;
                                        o_ptr->ds += (o_ptr->dd * p_ptr->skill_crafting) / 100;
                                }
                                if (o_ptr->tval == TV_SOFT_ARMOR || o_ptr->tval == TV_HARD_ARMOR || o_ptr->tval == TV_DRAG_ARMOR
                                || o_ptr->tval == TV_SHIELD || o_ptr->tval == TV_HELM || o_ptr->tval == TV_CLOAK || o_ptr->tval == TV_BOOTS
                                || o_ptr->tval == TV_GLOVES || o_ptr->tval == TV_ARM_BAND || o_ptr->tval == TV_CROWN)
                                {
                                        o_ptr->ac += (o_ptr->ac * (p_ptr->skill_crafting * 3)) / 100;
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

/* Zelar's legs breaing throw! */
void zelar_leg_throw_execute(monster_type *m_ptr)
{
        int tgt_x, tgt_y;
        int rad = 3;
        s32b dam = (p_ptr->dis_to_d / 2) * ((p_ptr->abilities[(CLASS_ZELAR * 10) + 4] / 4) + 1);
        monster_race *r_ptr = &r_info[m_ptr->r_idx]; 
        cave_type *c_ptr;
        if (!tgt_pt(&tgt_x,&tgt_y)) return;
        if (cave_empty_bold(tgt_y,tgt_x) && distance(tgt_y, tgt_x, py, px) <= rad)
        {
                if (((p_ptr->abilities[(CLASS_ZELAR * 10) + 4] * 100)+700) >= r_ptr->weight)
                {
                        c_ptr = &cave[m_ptr->fy][m_ptr->fx];
                        move_monster_spot(c_ptr->m_idx, tgt_x, tgt_y);
                        update_and_handle();
                        msg_print("You throw the monster!");
                        if (m_ptr->boss < 1 && !(r_ptr->flags1 & (RF1_UNIQUE)))
                        {
                                m_ptr->abilities |= (MUTILATE_LEGS);
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

