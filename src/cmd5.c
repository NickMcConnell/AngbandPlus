/* File: cmd5.c */

/* Purpose: Spell/Prayer commands */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

#define MUT_CHAOS_PATRON 57

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
static int get_spell(int *sn, cptr prompt, int sval, bool known, bool realm_2)
{
	int i;
	int spell;
	int num = 0;
	int ask;
	byte spells[PY_MAX_SPELLS];
	bool flag, okay;
	char choice;
	const magic_type *s_ptr;
	char out_val[160];
	int use_realm = p_ptr->spell.r[realm_2 ? 1 : 0].realm;
	cptr p = ((mp_ptr->spell_book == TV_LIFE_BOOK) ? "prayer" : "spell");

	/* Get the spell, if available */
	if (repeat_pull(sn))
	{
		/* Verify the spell */
		if (spell_okay(*sn, known, use_realm - 1))
		{
			/* Success */
			return (TRUE);
		}
		else
		{
			/* Invalid repeat - reset it */
			repeat_clear();
		}
	}

	/* Extract spells */
	for (spell = 0; spell < 32; spell++)
	{
		/* Check for this spell */
		if ((fake_spell_flags[sval] & (1L << spell)))
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
		if (spell_okay(spells[i], known, use_realm - 1)) okay = TRUE;
	}

	/* No "okay" spells */
	if (!okay) return (FALSE);

	/* Assume cancelled */
	*sn = (-1);

	/* Nothing chosen yet */
	flag = FALSE;

	/* Save the screen */
	screen_save();

	/* Display a list of spells */
	print_spells(spells, num, 20, 1, use_realm - 1);

	/* Show choices */
	/* Update */
	p_ptr->window |= (PW_SPELL);

	/* Window stuff */
	window_stuff();

	/* Build a prompt (accept all spells) */
	(void)strnfmt(out_val, 78, "(%^ss, ESC=exit) %^s which %s? ", p, prompt, p);

	/* Get a spell from the user */
	while (get_com(out_val, &choice))
	{
		/* Note verify */
		ask = (isupper(choice));

		/* Lowercase */
		if (ask) choice = tolower(choice);

		/* Extract request */
		i = (islower(choice) ? A2I(choice) : -1);

		/* Totally Illegal */
		if ((i < 0) || (i >= num))
		{
			bell("Illegal spell choice!");
			continue;
		}

		/* Save the spell index */
		spell = spells[i];

		/* Require "okay" spells */
		if (!spell_okay(spell, known, use_realm - 1))
		{
			bell("Illegal spell choice!");
			msgf("You may not %s that %s.", prompt, p);
			continue;
		}

		/* Verify it */
		if (ask)
		{
			/* Access the spell */
			s_ptr = &mp_ptr->info[use_realm - 1][spell % 32];

			/* Belay that order */
			if (!get_check("%^s %s (%d mana, %d%% fail)? ",
						  prompt, spell_names[use_realm - 1][spell % 32],
						  spell_mana(spell, use_realm - 1),
						  spell_chance(spell, use_realm - 1))) continue;
		}

		/* Stop the loop */
		flag = TRUE;
		break;
	}


	/* Restore the screen */
	screen_load();


	/* Show choices */
	/* Update */
	p_ptr->window |= (PW_SPELL);

	/* Window stuff */
	window_stuff();


	/* Abort if needed */
	if (!flag) return (FALSE);

	/* Save the choice */
	(*sn) = spell;

	repeat_push(*sn);

	/* Success */
	return (TRUE);
}

static void roff_spell_life(int spell)
{
	int plev = p_ptr->lev;
	bool high = (p_ptr->rp.pclass == CLASS_PRIEST || p_ptr->rp.pclass == CLASS_HIGH_MAGE);

	switch(spell)
	{
		case 0:
			roff ("Lets you sense the location and type of any nearby living creatures.");
		    roff (CLR_VIOLET " Range: %i", MAX_DETECT);
			roff (CLR_L_DARK "  It will not detect non-living creatures such as undead or golems.");
			return;
		case 1:
			roff ("Cures you of ");
			roff (CLR_L_GREEN "8+1d10");
			roff (" damage and cures wounds somewhat.");
			return;
		case 2:
			roff ("Bestows a blessing on you for ");
			roff (CLR_YELLOW "12-24 turns");
			roff (".  While you are blessed, you have substantial bonuses to hit and to your armor class,");
			roff (" and a slight bonus to damage.");
			return;
		case 3:
			roff ("Creates a ball of magical ");
			roff (CLR_YELLOW "light ");
			roff ("centered on you, of ");
			roff (CLR_VIOLET "radius %i", plev/10 + 1);
			roff (" that will cause ");
			roff (CLR_RED "2d%i (up to 2d25) damage", plev/2);
			roff (" to monsters vulnerable to light (at its center), and will illuminate the area around you.");
			return;
		case 4:
			roff ("Lets you sense the presence of nearby traps, doors, secret doors, and stairs.");
			roff (CLR_VIOLET " Range: %i", MAX_DETECT);
			roff (CLR_L_DARK " You will receive a warning when you leave the trap-detected area.");
			return;
		case 5:
			roff ("Cures you of ");
			roff (CLR_L_GREEN "16+2d10");
			roff (" damage and cures most wounds, and reduces the duration of poison.");
			return;
		case 6:
			roff ("Cures you of fear and bestows heroism on you for ");
			roff (CLR_YELLOW "25-50 turns");
			roff ("." CLR_L_DARK " While you have heroism, you have a substantial bonus to hit, a bonus to damage,");
			roff (CLR_L_DARK " and you temporarily gain 10 hit points.");
			return;
		case 7:
			roff ("Dispels basic curses on all your equipment as well as any items in your inventory.");
			roff (CLR_L_DARK " Cursed equipment cannot be taken off while it is cursed, and may function more poorly");
			roff (CLR_L_DARK " than normal.");
			return;
		case 8:
			roff ("Bestows a blessing on you for ");
			roff (CLR_YELLOW "50-100 turns");
			roff ("." CLR_L_DARK " While you are blessed, you have substantial bonuses to hit and to your armor class,");
			roff (CLR_L_DARK " and a slight bonus to damage.");
			return;
		case 9:
			roff ("Cures you of ");
			roff (CLR_L_GREEN "32+4d10");
			roff (" damage and cures wounds, stunning, and poison completely.");
			return;
		case 10:
			roff ("Grants you the ability to see invisible monsters, for ");
			roff (CLR_YELLOW "24-48 turns");
			roff (".");
			return;
		case 11:
			roff ("Fires a ball of ");
			roff (CLR_L_BLUE "holy fire");
			roff (" at a target of your choice, of ");
			roff (CLR_VIOLET "radius %i", plev < 30 ? 3 : 2);
			roff (" that will cause ");
			roff (CLR_RED "3d6+%i (up to 3d6+%i) damage", plev + plev / (high ? 2 : 4),
						(high ? 75 : (50 + (50/4)) ));
			roff (" (at its center) to monsters in the blast.  ");
			roff (CLR_L_DARK "Evil monsters take double damage from holy fire, and it will destroy cursed objects.");
			return;
		case 12:
			roff ("Bestows protection from evil on you for ");
			roff (CLR_YELLOW "%i-%i (up to 151-175) turns", plev*3 + 1, plev*3 + 25);
			roff ("." CLR_L_DARK " While you are protected from evil, some attacks on you from evil creatures will be");
			roff (CLR_L_DARK " prevented entirely");
			return;
		case 13:
			roff ("Cures you of ");
			roff (CLR_L_GREEN "250+10d10");
			roff (" damage and cures wounds, stunning, fear, and poison completely.");
			return;
		case 14:
			roff ("Creates a magical glyph of warding on the floor where you are. ");
			roff (CLR_L_DARK "Monsters cannot cross a glyph of warding, although they will sometimes break the spell ");
			roff (CLR_L_DARK "through sheer will.");
			return;
		case 15:
			roff ("Allows you to transport between the wilderness and a level in a dungeon. ");
			roff ("The dungeon level you travel to will be the deepest you have ventured in that dungeon. ");
			roff ("If you cast this spell in the dungeon at a lower depth, you can \"reset\" where you will ");
			roff ("recall to in the future.  Transport does not take place immediately, and can be cancelled ");
			roff ("by another Word of Recall effect.");
			return;
		case 16:
			roff ("Satisfies your hunger completely, even if you are unable to eat normal food.");
			return;
		case 17:
			roff ("Cures you of ");
			roff (CLR_L_GREEN "64+8d10");
			roff (" damage and cures wounds, stunning, and poison completely.  Has an especially low mana cost.");
			return;
		case 18:
			roff ("Deals ");
			roff (CLR_RED "1-%i (up to 1-150) damage", plev*3);
			roff (" to all undead or demons nearby that are not obscured by opaque terrain.");
			return;
		case 19:
			roff ("Summons an angel to serve you for ");
			roff (CLR_YELLOW "150 turns");
			roff ("." CLR_L_DARK " There is a chance the summoning will work incompletely, and the creature will attack you.");
			roff (CLR_L_DARK " Otherwise, it will be your pet and under some limited control.");
			return;
		case 20:
			roff ("Deals ");
			roff (CLR_RED "1-%i (up to 1-200) damage", plev*4);
			roff (" to all evil monsters nearby that are not obscured by opaque terrain.");
			return;
		case 21:
			roff ("Causes all evil monsters nearby that are not obscured by opaque terrain to be teleported ");
			roff ("somewhere else." CLR_L_DARK " Some monsters may be able to resist teleportation.");
			return;
		case 22:
			roff ("Deals ");
			roff (CLR_RED "1-%i (up to 1-200) damage", plev*4);
			roff (" to all evil monsters nearby that are not obscured by opaque terrain. Also, ");
			roff ("cures you of ");
			roff (CLR_L_GREEN "1000");
			roff (" damage and cures wounds, stunning, fear, and poison completely.");
			return;
		case 23:
			roff ("Cures you of ");
			roff (CLR_L_GREEN "100");
			roff (" damage and restores your experience and all your stats.");
			return;
		case 24:
			roff ("Bestows you with a magical shield for ");
			roff (CLR_VIOLET "30-50 turns");
			roff ("." CLR_L_DARK "  While your shield is active you get a bonus of +50 to your armor class.");
			return;
		case 25:
			roff ("Dispels basic and heavy curses on all your equipment as well as any items in your inventory.");
			roff (CLR_L_DARK "  Cursed equipment cannot be taken off while it is cursed, and may function more poorly ");
			roff (CLR_L_DARK "than normal.");
			return;
		case 26:
			roff ("Creates a ball of magical ");
			roff (CLR_RED "fire ");
			roff ("centered on you, of ");
			roff (CLR_VIOLET "radius 6");
			roff (" that will cause ");
			roff (CLR_RED "200 damage");
			roff (" (at its center) to monsters in the blast.");
			roff (CLR_L_DARK "Fire destroys certain types of objects on the floor.");
			return;
		case 27:
			roff ("Causes the melee weapon you are currently weilding to become blessed by the gods.");
			roff (" This allows priests to weild the weapon without penalty, even if it is not a hafted weapon.");
			roff (" In addition, this may grant other powers to the weapon.  In addition to its mana cost, this ");
			roff ("spell costs ");
			roff (CLR_YELLOW "500 gold");
			roff (" to cast.");
			return;
		case 28:
			roff ("Creates a magical glyph of warding on the floor where you are, and on all adjacent grids. ");
			roff (CLR_L_DARK "Monsters cannot cross a glyph of warding, although they will sometimes break the spell ");
			roff (CLR_L_DARK "through sheer will.");
			return;
		case 29:
			roff ("Identifies one object fully, revealing all its hidden powers.");
			return;
		case 30:
			roff ("This awesome invocation deals damage and causes various status changes to all monsters not ");
			roff ("obscured by opaque terrain, heals and enhances you, and summons an angelic being that ");
			roff ("will serve you for a long time.");
			return;
		case 31:
			roff ("Makes you nearly invulnerable to damage for ");
			roff (CLR_YELLOW "7-14 turns. ");
			roff (CLR_L_DARK "While you are invulnerable, about 90%% of damage to you is prevented.");
			return;
		default:
			roff (CLR_L_RED "Unknown life spell.");
			return;
	}
}

static void roff_spell_sorcery(int spell)
{
	int plev = p_ptr->lev;
	bool high = (p_ptr->rp.pclass == CLASS_PRIEST || p_ptr->rp.pclass == CLASS_HIGH_MAGE);

	switch(spell)
	{
		case 0:
			roff ("Lets you sense the location and type of any nearby creatures.");
		    roff (CLR_VIOLET " Range: %i", MAX_DETECT);
			return;
		case 1:
			roff ("Teleports you to a random open space, about ");
			roff (CLR_VIOLET "10");
			roff (" spaces away from your current location.");
			return;
		case 2:
			roff ("Lets you sense the presence of nearby traps, doors, secret doors, and stairs.");
			roff (CLR_VIOLET " Range: %i", MAX_DETECT);
			roff (CLR_L_DARK " You will receive a warning when you leave the trap-detected area.");
			return;
		case 3:
			roff ("Creates a ball of magical ");
			roff (CLR_YELLOW "light ");
			roff ("centered on you, of ");
			roff (CLR_VIOLET "radius %i", plev/10 + 1);
			roff (" that will cause ");
			roff (CLR_RED "2d%i (up to 2d25) damage", plev/2);
			roff (" to monsters vulnerable to light (at its center), and will illuminate the area around you.");
			return;
		case 4:
			roff ("Attempts to cause a creature of your choice to become confused. ");
			roff (CLR_L_DARK "Confused monsters move randomly and may attack other monsters. ");
			roff (CLR_L_DARK "Some monsters may resist being confused. ");
			return;
		case 5:
			roff ("Teleports you to a random open space, about ");
			roff (CLR_VIOLET "%i (up to 250)", plev*5);
			roff (" spaces away from your current location.");
			return;
		case 6:
			roff ("Attempts to cause a creature of your choice to fall asleep. ");
			roff (CLR_L_DARK "Sleeping monsters take no actions until woken up. ");
			roff (CLR_L_DARK "Some monsters may resist sleep.");
			return;
		case 7:
			roff ("Attempts to cause a creature of your choice to move more slowly. ");
			roff (CLR_L_DARK "Some monsters may resist being slowed. ");
			return;
		case 8:
			roff ("Reveals the shape of the dungeon near you. ");
			roff (CLR_VIOLET " Range: %i", MAX_DETECT);
			return;
		case 9:
			roff ("Identifies an object, revealing its type and magical bonuses. " CLR_L_DARK "Some objects, ");
			roff (CLR_L_DARK "such as artifacts, may have hidden powers that are not revealed by basic identification.");
			return;
		case 10:
			roff ("Attempts to charm a creature of your choice. ");
			roff (CLR_L_DARK "Charmed monsters are friendly, and attack other foes. ");
			roff (CLR_L_DARK "Some monsters may resist being charmed. ");
			return;
		case 11:
			roff ("Attempts to cause all creatures in the area not obscured by opaque terrain to fall asleep. ");
			roff (CLR_L_DARK "Sleeping monsters take no actions until woken up. ");
			roff (CLR_L_DARK "Some monsters may resist sleep.");
			return;
		case 12:
			roff ("Teleports a monster of your choice elsewhere in the dungeon. ");
			roff (CLR_L_DARK "Some monsters may resist teleportation.");
			return;
		case 13:
			roff ("Grants you temporary extra speed (+10) for ");
			roff (CLR_YELLOW "%i-%i (up to 51-120) turns", plev+1, 2*plev+20);
			roff (".");
			return;
		case 14:
			roff ("Lets you sense the presence of nearby creatures, invisible creatures, objects, buried treasure, ");
			roff ("traps, doors, secret doors, and stairs.");
			roff (CLR_VIOLET " Range: %i", MAX_DETECT);
			roff (CLR_L_DARK " You will receive a warning when you leave the trap-detected area.");
			return;
		case 15:
			roff ("Identifies one object fully, revealing all its hidden powers.");
			return;
		case 16:
			roff ("Lets you sense the presence of nearby objects and buried treasure.");
			roff (CLR_VIOLET " Range: %i", MAX_DETECT);
			return;
		case 17:
			roff ("Lets you sense the presence of nearby enchanted objects.");
			roff (CLR_VIOLET " Range: %i", MAX_DETECT);
			return;
		case 18:
			roff ("Will recharge a wand, staff, or rod.");
			roff (CLR_L_DARK "There is a chance it will backfire and blow up or completely drain the item.");
			return;
		case 19:
			roff ("Enhances a weapon of your choice, permanently increasing its bonuses to hit and to damage.");
			roff (" In addition to its mana cost, this spell costs " CLR_YELLOW "200 gold");
			roff (" to cast.");
			roff (CLR_L_DARK "The more enchanted the weapon is to begin with, the less effect this will have.");
			return;
		case 20:
			roff ("Enhances an armor item of your choice, permanently increasing its bonus to armor class.");
			roff (" In addition to its mana cost, this spell costs " CLR_YELLOW "200 gold");
			roff (" to cast.");
			roff (CLR_L_DARK "The more enchanted the armor is to begin with, the less effect this will have.");
			return;
		case 21:
			roff ("Grants you the power of telepathy for ");
			roff (CLR_YELLOW "25-55 turns");
			roff (".");
			roff (CLR_L_DARK " With telepathy, you automatically sense nearby intelligent creatures.");
			return;
		case 22:
			roff ("Teleports you, at random, either one level up or down in the dungeon.");
			return;
		case 23:
			roff ("Allows you to transport between the wilderness and a level in a dungeon. ");
			roff ("The dungeon level you travel to will be the deepest you have ventured in that dungeon. ");
			roff ("If you cast this spell in the dungeon at a lower depth, you can \"reset\" where you will ");
			roff ("recall to in the future.  Transport does not take place immediately, and can be cancelled ");
			roff ("by another Word of Recall effect.");
			return;
		case 24:
			roff ("Attempts to cause a creature of your choice to fall into a deep coma. ");
			roff (CLR_L_DARK "Comatose monsters take no actions until woken up.");
			roff (CLR_L_DARK "No monsters are immune to stasis effects.");
			return;
		case 25:
			roff ("Turns an object of your choice into gold.  The amount of gold you get depends on the value ");
			roff ("of the object.");
			return;
		case 26:
			roff ("Teleports you to a space of your choice, " CLR_VIOLET "range 25. ");
			roff ("."  "If the space you choose is occupied, you will teleport to a random space instead.");
			return;
		case 27:
			roff ("Bestows you with a magical shield for ");
			roff (CLR_VIOLET "30-50 turns");
			roff ("." CLR_L_DARK "  While your shield is active you get a bonus of +50 to your armor class.");
			return;
		case 28:
			roff ("Creates a magical glyph of warding on the floor where you are. ");
			roff (CLR_L_DARK "Monsters cannot cross a glyph of warding, although they will sometimes break the spell ");
			roff (CLR_L_DARK "through sheer will.");
			return;
		case 29:
			roff ("Completely lights and maps the current level. ");
			roff ("Also grants you the power of telepathy for ");
			roff (CLR_YELLOW "25-55 turns");
			roff (".");
			roff (CLR_L_DARK " With telepathy, you automatically sense nearby intelligent creatures.");
			return;
		case 30:
			roff ("Causes the melee weapon you are currently weilding to become branded with an elemental power.");
			roff (" In addition to its mana cost, this ");
			roff ("spell costs ");
			roff (CLR_YELLOW "1000 gold");
			roff (" to cast.");
			return;
		case 31:
			roff ("Makes you nearly invulnerable to damage for ");
			roff (CLR_YELLOW "7-14 turns. ");
			roff (CLR_L_DARK "While you are invulnerable, about 90%% of damage to you is prevented.");
			return;
	}
}

static void roff_spell_nature(int spell)
{
	int plev = p_ptr->lev;
	bool high = (p_ptr->rp.pclass == CLASS_PRIEST || p_ptr->rp.pclass == CLASS_HIGH_MAGE);

	switch(spell)
	{
		case 0:
			roff ("Lets you sense the location and type of any nearby creatures.");
		    roff (CLR_VIOLET " Range: %i", MAX_DETECT);
			return;
		case 1:
			roff ("Cures you of ");
			roff (CLR_L_GREEN "6+1d8");
			roff (" damage and cures wounds somewhat.");
			return;
		case 2:
			roff ("Lets you sense the presence of nearby traps, doors, secret doors, and stairs.");
			roff (CLR_VIOLET " Range: %i", MAX_DETECT);
			roff (CLR_L_DARK " You will receive a warning when you leave the trap-detected area.");
			return;
		case 3:
			roff ("Satisfies your hunger completely, even if you are unable to eat normal food.");
			return;
		case 4:
			roff ("Creates a ball of magical ");
			roff (CLR_YELLOW "light ");
			roff ("centered on you, of ");
			roff (CLR_VIOLET "radius %i", plev/10 + 1);
			roff (" that will cause ");
			roff (CLR_RED "2d%i (up to 2d25) damage", plev/2);
			roff (" to monsters vulnerable to light (at its center), and will illuminate the area around you.");
			return;
		case 5:
			roff ("Attempts to charm one animal of your choice. ");
			roff (CLR_L_DARK "Charmed monsters are friendly, and attack other foes. ");
			roff (CLR_L_DARK "Some monsters may resist being charmed. ");
			return;
		case 6:
			roff ("Bestows you with a magical resistance to cold, fire, and electricity for ");
			roff (CLR_VIOLET "30-50 turns");
			roff ("." CLR_L_DARK "  While you are resistant, you take less damage from elemental attacks, and");
			roff (CLR_L_DARK " items in your backpack are somewhat protected from elemental-based destruction.");
			return;
		case 7:
			roff ("Cures you of ");
			roff (CLR_L_GREEN "12+2d8");
			roff (" damage and completely cures wounds and poison.");
			return;
		case 8:
			roff ("Turns one wall segment of your choice to mud.");
			roff (CLR_L_DARK " Will cause damage to creatures made of stone.");
			return;
		case 9:
			roff ("Fires a bolt of magical ");
			roff (CLR_L_BLUE "electricity ");
			roff ("at a target of your choice, doing ");
			roff (CLR_RED "%id8 (up to 14d8) damage", 3+((plev-5)/4));
			roff ("." CLR_L_DARK "  Bolt spells may occasionally fire \"beams\" that affect all monsters they pass through.");
			return;
		case 10:
			roff ("Fires a bolt of magical ");
			roff (CLR_BLUE "frost ");
			roff ("at a target of your choice, doing ");
			roff (CLR_RED "%id8 (up to 16d8) damage", 5+((plev-5)/4));
			roff ("." CLR_L_DARK "  Bolt spells may occasionally fire \"beams\" that affect all monsters they pass through.");
			return;
		case 11:
			roff ("Fires a beam of magical ");
			roff (CLR_YELLOW "light ");
			roff ("in a direction of your choice, doing ");
			roff (CLR_RED "6d8 damage");
			roff (" to all monsters in its path that are vulnerable to light, and illuminating the spaces it touches.");
			return;
		case 12:
			roff ("Creates a mass of entangling roots that attempts to ");
			roff ("cause all creatures in the area not obscured by opaque terrain to move more slowly. ");
			roff (CLR_L_DARK "Some monsters may resist being slowed.");
			return;
		case 13:
			roff ("Summons a group of animals to serve you for ");
			roff (CLR_YELLOW "150 turns");
			roff ("." CLR_L_DARK " There is a chance the summoning will work incompletely, and the creatures will attack you.");
			roff (CLR_L_DARK " Otherwise, they will be your pets and under some limited control.");
			return;
		case 14:
			roff ("Attempts to make a piece of armor of your choice resistant to corrosion. ");
			roff ("Such armors cannot be damaged by acid.  In addition to its mana cost, this spell costs ");
			roff (CLR_YELLOW "40 gold");
			roff (" to cast.");
			return;
		case 15:
			roff ("Cures you of ");
			roff (CLR_L_GREEN "1000");
			roff (" damage and cures wounds, stunning, and poison completely.");
			return;
		case 16:
			roff ("Completely surrounds you with jammed doors.");
			return;
		case 17:
			roff ("Lets you sense the presence of nearby creatures, invisible creatures, objects, buried treasure, ");
			roff ("traps, doors, secret doors, and stairs.");
			roff (CLR_VIOLET " Range: %i", MAX_DETECT);
			roff (CLR_L_DARK " You will receive a warning when you leave the trap-detected area.");
			return;
		case 18:
			roff ("Bestows you with a magical shield for ");
			roff (CLR_VIOLET "30-50 turns");
			roff ("." CLR_L_DARK "  While your shield is active you get a bonus of +50 to your armor class.");
			return;
		case 19:
			roff ("Bestows you with a magical resistance to cold, fire, electricity, acid, and poison for ");
			roff (CLR_VIOLET "30-50 turns");
			roff ("." CLR_L_DARK "  While you are resistant, you take less damage from elemental attacks, and");
			roff (CLR_L_DARK " items in your backpack are somewhat protected from elemental-based destruction.  ");
			roff (CLR_L_DARK " While you are resistant to poison, you cannot be poisoned.");
			return;
		case 20:
			roff ("Completely surrounds you with granite walls.");
			return;
		case 21:
			roff ("Completely surrounds you with water.");
			roff (CLR_L_DARK " Monsters unable to swim or fly cannot pass through deep water.");
			return;
		case 22:
			roff ("Identifies one object fully, revealing all its hidden powers.");
			return;
		case 23:
			roff ("Completely surrounds you with molten lava.");
			roff (CLR_L_DARK " Monsters unable to swim or fly cannot pass through deep lava, and monsters");
			roff (CLR_L_DARK " not resistant to fire will not enter lava.");
			return;
		case 24:
			roff ("Creates an earthquake near you of " CLR_VIOLET "radius 10");
			roff (".");
			roff (CLR_L_DARK " Earthquakes change the dungeon terrain, and can damage or kill monsters trapped");
			roff (CLR_L_DARK " in the new rock.");
			return;
		case 25:
			roff ("Causes you to attack all monsters adjacent to you simultaneously.");
			return;
		case 26:
			roff ("Summons a magical blizzard, doing up to" CLR_RED "%i (max 120)" CLR_BLUE "frost", 70+plev);
			roff ("-based damage to all monsters within " CLR_VIOLET "radius %i (up to 5)", (plev/12)+1);
			roff (" of the target.");
			roff (CLR_L_DARK " Frost-based damage destroys potions on the dungeon floor.");
			return;
		case 27:
			roff ("Summons a magical thunderstorm, doing up to" CLR_RED "%i (max 140)" CLR_L_BLUE "electricity", 90+plev);
			roff ("-based damage to all monsters within " CLR_VIOLET "radius %i (up to 5)", (plev/12)+1);
			roff (" of the target.");
			roff (CLR_L_DARK " Electricity-based damage destroys wands, rods, rings, and amulets on the dungeon floor.");
			return;
		case 28:
			roff ("Summons a magical whirlpool, doing up to" CLR_RED "%i (max 150)" CLR_BLUE "water", 100+plev);
			roff ("-based damage to all monsters within " CLR_VIOLET "radius %i (up to 5)", (plev/12)+1);
			roff (" of the target.");
			roff (CLR_L_DARK " Few monsters resist water-based damage.");
			return;
		case 29:
			roff ("Summons a magical burst of radiance centered on you, doing up to");
			roff (CLR_RED "150" CLR_ORANGE "radiance");
			roff ("-based damage to all monsters within " CLR_VIOLET "radius 8");
			roff (" of you, and also fully lights and maps the dungeon level.");
			roff (CLR_L_DARK " Monsters vulnerable to light take double damage.");
			roff (CLR_L_DARK " If you are vulnerable to light, you may be damaged as well.");
			return;
		case 30:
			roff ("Causes the melee weapon you are currently weilding to become branded with an elemental power.");
			roff (" In addition to its mana cost, this ");
			roff ("spell costs ");
			roff (CLR_YELLOW "500 gold");
			roff (" to cast.");
			return;
		case 31:
			roff ("This powerful invocation calls on the wrath of natural forces.  It severely disrupts the dungeon");
			roff (" terrain near you, and causes damage of various types to nearby monsters.");
			return;
	}
}

static void roff_spell_chaos(int spell)
{
	int plev = p_ptr->lev;
	bool high = (p_ptr->rp.pclass == CLASS_PRIEST || p_ptr->rp.pclass == CLASS_HIGH_MAGE);

	switch(spell)
	{
		case 0:
			roff ("Fires a magical ");
			roff (CLR_L_BLUE "missile ");
			roff ("at a target of your choice, doing ");
			roff (CLR_RED "%id4 (up to 12d4) damage", 3+((plev-1)/5));
			roff ("." CLR_L_DARK "  Bolt spells may occasionally fire \"beams\" that affect all monsters they pass through.");
			roff (CLR_L_DARK " Magic missile damage cannot be resisted.");
			return;
		case 1:
			roff ("Destroys all doors and traps within " CLR_VIOLET "radius 1");
			roff (" of you.");
			return;
		case 2:
			roff ("Creates a ball of magical ");
			roff (CLR_YELLOW "light ");
			roff ("centered on you, of ");
			roff (CLR_VIOLET "radius %i", plev/10 + 1);
			roff (" that will cause ");
			roff (CLR_RED "2d%i (up to 2d25) damage", plev/2);
			roff (" to monsters vulnerable to light (at its center), and will illuminate the area around you.");
			return;
		case 3:
			roff ("Embues you with a chaotic aura that will cause confusion against the next monster you hit with");
			roff (" a melee attack.");
			roff (CLR_L_DARK "Confused monsters move randomly and may attack other monsters. ");
			roff (CLR_L_DARK "Some monsters may resist being confused. ");
			return;
		case 4:
			roff ("Fires a ball of ");
			roff (CLR_L_BLUE "mana");
			roff (" at a target of your choice, of ");
			roff (CLR_VIOLET "radius %s", plev < 30 ? "3" : "2 (up to 3)");
			roff (" that will cause ");
			roff (CLR_RED "3d5+%i (up to 3d5+%i) damage", plev + plev / (high ? 2 : 4),
						(high ? 75 : (50 + (50/4)) ));
			roff (" (at its center) to monsters in the blast.  ");
			roff ("This will not affect objects on the floor.");
			roff (CLR_L_DARK " Mana-based damage cannot be resisted.");
			return;
		case 5:
			roff ("Attempts to polymorph a creature of your choice.");
			roff (CLR_L_DARK " Some monsters may resist being polymorphed.");
			return;
		case 6:
			roff ("Fires a " CLR_VIOLET "radius 0");
			roff (" ball of " CLR_ORANGE "disintegration");
			roff (" at a target of your choice that will cause ");
			roff (CLR_RED "%id6 (up to 19d6) damage", 8+((plev-5)/4));
			roff (" to the target.  ");
			roff (CLR_L_DARK " Monsters made of stone take double damage from disintegration.  ");
			roff (CLR_L_DARK "Disintegration will destroy any objects on the floor, and will also dissolve walls. ");
			roff (CLR_L_DARK "A radius 0 ball is like a bolt that can skip over other monsters in the way.");
			return;
		case 7:
			roff ("Teleports you to a random open space, about ");
			roff (CLR_VIOLET "%i (up to 250)", plev*5);
			roff (" spaces away from your current location.");
			return;
		case 8:
			roff ("Fires something random at a target of your choice.  The effects include various forms of elemental ");
			roff ("damage, status effects.  Some effects are bad.  However, your chance to get a good effect goes up as");
			roff (" you gain level, as does the damage caused.");
			return;
		case 9:
			roff ("Fires a bolt of magical ");
			roff (CLR_RED "fire ");
			roff ("at a target of your choice, doing ");
			roff (CLR_RED "%id8 (up to 19d8) damage", 8+((plev-5)/4));
			roff ("." CLR_L_DARK "  Bolt spells may occasionally fire \"beams\" that affect all monsters they pass through.");
			return;
		case 10:
			roff ("Fires a ball of ");
			roff (CLR_VIOLET "chaos");
			roff (" at a target of your choice, of ");
			roff (CLR_VIOLET "radius %i (up to 6)", 2+plev/12);
			roff (" that will cause ");
			roff (CLR_RED "%i (up to 30) damage", 5 + plev / 2);
			roff (" (at its center) to monsters in the blast.  ");
			roff (CLR_L_DARK " Chaos-based damage can cause polymorphing and confusion, and destroys objects on the floor.");
			return;
		case 11:
			roff ("Creates a ball of magical ");
			roff (CLR_L_WHITE "sound ");
			roff ("centered on you, of ");
			roff (CLR_VIOLET "radius %i (up to 7)", plev/10 + 2);
			roff (" that will cause ");
			roff (CLR_RED "%i (up to 95) damage", plev+45);
			roff (" (at its center).");
			roff (CLR_L_DARK " Sound-based damage destroys potions on the floor, and causes stunning.");
			return;
		case 12:
			roff ("Fires a beam of ");
			roff (CLR_L_BLUE "mana ");
			roff ("at a target of your choice, doing ");
			roff (CLR_RED "%id8 (up to 22d8) damage", 11+((plev-5)/4));
			roff (" to all creatures in its path.");
			roff (CLR_L_DARK " Mana-based damage cannot be resisted.");
			return;
		case 13:
			roff ("Fires a ball of ");
			roff (CLR_RED "fire");
			roff (" at a target of your choice, of ");
			roff (CLR_VIOLET "radius 2");
			roff (" that will cause ");
			roff (CLR_RED "%i (up to 105) damage", 55 + plev);
			roff (" (at its center) to monsters in the blast.  ");
			roff (CLR_L_DARK "Fire destroys certain types of objects on the floor.");
			return;
		case 14:
			roff ("Teleports a monster of your choice elsewhere in the dungeon. ");
			roff (CLR_L_DARK "Some monsters may resist teleportation.");
			return;
		case 15:
			roff ("Invokes pure destructive power, of " CLR_VIOLET "radius 15");
			roff (", centered on you.  This will transform the dungeon terrain and makes creatures and objects in the blast disappear.");
			return;
		case 16:
			roff ("Fires beams of ");
			roff (CLR_L_BLUE "electricity ");
			roff ("in all directions, doing ");
			roff (CLR_RED "%id8 (up to 10d8) damage", 5+(plev/10));
			roff (" to all creatures in its path.");
			return;
		case 17:
			roff ("Fires a bolt of ");
			roff (CLR_VIOLET "chaos ");
			roff ("at a target of your choice, doing ");
			roff (CLR_RED "%id8 (up to 21d8) damage", 10+((plev-5)/4));
			roff ("." CLR_L_DARK "  Bolt spells may occasionally fire \"beams\" that affect all monsters they pass through.");
			return;
		case 18:
			roff ("Will recharge a wand, staff, or rod.");
			roff (CLR_L_DARK "There is a chance it will backfire and blow up or completely drain the item.");
			return;
		case 19:
			roff ("Causes " CLR_RED "1-20");
			roff (" plus up to half your current hit points " CLR_RED "damage");
			roff (" to you.  For every 2 damage you take, you gain 1 mana.  There is a chance you will not be able");
			roff (" to control the mana, in which case a ball of " CLR_L_BLUE "mana");
			roff (" will be produced, of " CLR_VIOLET "radius 3");
			roff (" centered on you, that does damage to nearby monsters based on the damage you took.");
			return;
		case 20:
			roff ("Completely re-generates the dungeon level around you.");
			return;
		case 21:
			roff ("Polymorphs you; this may rearrange your basic attributes, change your sex or race, ");
			roff (" cause damage or stat draining, and may cause you to gain or lose a mutation.");
			return;
		case 22:
			roff ("Summons a demonic creature to serve you for ");
			roff (CLR_YELLOW "150 turns");
			roff (".  There is a chance you will get a group of demons instead.");
			roff (CLR_L_DARK " There is a chance the summoning will work incompletely, and the creatures will attack you.");
			roff (CLR_L_DARK " Otherwise, they will be your pets and under some limited control.");
			return;
		case 23:
			roff ("Allows you to breathe " CLR_VIOLET "chaos");
			roff (" at a target of your choice, with " CLR_VIOLET "radius 2");
			roff (" doing " CLR_RED "damage equal to your hit points");
			roff (" (at the center) to monsters in the blast.");
			roff (CLR_L_DARK " Chaos-based damage can cause polymorphing and confusion, and destroys objects on the floor.");
			return;
		case 24:
			roff ("Allows you to drain a wand, rod, or staff of charges in exchange for mana.  The amount of");
			roff (" mana you get is proportional to the charges on the item.");
			return;
		case 25:
			roff ("Polymorphs an item of your choice.  This may result in the item being destroyed, turned to gold, ");
			roff (" or being replaced by a newly-generated item.");
			roff (CLR_L_DARK " This has no effect on artifacts, and works poorly on cursed items.");
			return;
		case 26:
			roff ("Fires 10-20 magical meteors at random spaces in your field of vision.");
			roff ("Each meteor has " CLR_VIOLET "radius 2");
			roff (" and does " CLR_RED "%i (up to 75) damage", (plev*3)/2);
			roff (" (at its center) to monsters in the blast.");
			roff (CLR_L_DARK " Meteor-based damage cannot be resisted.");
			return;
		case 27:
			roff ("Calls upon the forces of chaos to release destructive energy that will fire ");
			roff ("one or many balls or beams of various random types.");
			return;
		case 28:
			roff ("Fires a " CLR_L_DARK "rocket");
			roff (" at a target of your choice, of " CLR_VIOLET "radius 2");
			roff (" that does " CLR_RED "%i (up to 170) damage", plev+120);
			roff (" (at its center) to monsters in the blast area.");
			roff (CLR_L_DARK " Few monsters can resist rocket-based damage.");
			return;
		case 29:
			roff ("Gives you a random mutation.");
			return;
		case 30:
			roff ("Fires a powerful ball of " CLR_L_BLUE "mana");
			roff (" at a target of your choice, of " CLR_VIOLET "radius 4");
			roff (" that does " CLR_RED "%i (up to 400) damage", 2*plev+200);
			roff (" (at its center) to monsters in the blast area.");
			roff (CLR_L_DARK " Mana-based damage cannot be resisted, and destroys objects on the floor.");
			return;
		case 31:
			roff ("This powerful invocation summons a tremendous amount of destructive energy.");
			return;
	}
}

static void roff_spell_death(int spell)
{
	int plev = p_ptr->lev;
	bool high = (p_ptr->rp.pclass == CLASS_PRIEST || p_ptr->rp.pclass == CLASS_HIGH_MAGE);

	switch(spell)
	{
		case 0:
			roff ("Lets you sense the location and type of any nearby non-living or evil creatures.");
		    roff (CLR_VIOLET " Range: %i", MAX_DETECT);
			return;
		case 1:
			roff ("Fires a " CLR_VIOLET "radius 0");
			roff (" ball of " CLR_ORANGE "unholy fire");
			roff (" at a target of your choice that will cause ");
			roff (CLR_RED "%id3 (up to 12d3) damage", 3+((plev-1)/5));
			roff (" to the target.  ");
			roff (CLR_L_DARK "Evil monsters take double damage from unholy fire, and it will destroy cursed objects. ");
			roff (CLR_L_DARK "A radius 0 ball is like a bolt that can skip over other monsters in the way.");
			return;
		case 2:
			roff ("Attempts to cause a creature of your choice to become afraid. ");
			roff (CLR_L_DARK "Fearful monsters run away from you until they recover their courage.");
			roff (CLR_L_DARK "Some monsters may resist being horrified. ");
			return;
		case 3:
			roff ("Fires a ball of ");
			roff (CLR_L_GREEN "poison gas ");
			roff ("at a targt of your choice, of ");
			roff (CLR_VIOLET "radius 2");
			roff (" that will cause ");
			roff (CLR_RED "%i (up to 35) damage", 10+plev/2);
			roff (" (at its center) to monsters in its area.");
			roff (CLR_L_DARK " Many monsters resist poison-based attacks.");
			return;
		case 4:
			roff ("Attempts to cause a creature of your choice to fall asleep. ");
			roff (CLR_L_DARK "Sleeping monsters take no actions until woken up. ");
			roff (CLR_L_DARK "Some monsters may resist sleep.");
			return;
		case 5:
			roff ("Bestows you with a magical resistance to poison for ");
			roff (CLR_VIOLET "20-40 turns");
			roff ("." CLR_L_DARK "  While you are resistant, you take less damage from posion-based attacks, and");
			roff (CLR_L_DARK " you cannot be poisoned.");
			return;
		case 6:
			roff ("Attempts to enslave one undead monster of your choice. ");
			roff (CLR_L_DARK "Enslaved undead are friendly, and attack other foes. ");
			return;
		case 7:
			roff ("Attempts to cause all creatures nearby to become afraid. ");
			roff (CLR_L_DARK "Fearful monsters run away from you until they recover their courage. ");
			roff (CLR_L_DARK "Some monsters may resist being horrified. ");
			return;
		case 8:
			roff ("Fires a ball of ");
			roff (CLR_L_DARK "negative energy ");
			roff ("at a targt of your choice, of ");
			roff (CLR_VIOLET "radius %i", (plev < 30) ? 2 : 3);
			roff (" that will cause ");
			roff (CLR_RED "3d6+%i (up to 3d6+%i) damage", plev + plev / (high ? 2:4), (high ? 75 : (50+(50/4))));
			roff (" (at its center) to monsters in its area.");
			roff (CLR_L_DARK " Undead and demons are not affected by negative energy.");
			return;
		case 9:
			roff ("Teleports you to a random open space, about ");
			roff (CLR_VIOLET "%i (up to 250)", plev*5);
			roff (" spaces away from your current location.");
			return;
		case 10:
			roff ("Summons a group of basic undead creatures to serve you for ");
			roff (CLR_YELLOW "100 turns");
			roff (".");
			roff (CLR_L_DARK " There is a chance the summoning will work incompletely, and the creatures will attack you.");
			roff (CLR_L_DARK " Otherwise, they will be your pets and under some limited control.");
			return;
		case 11:
			roff ("Hits a target of your choice with " CLR_L_DARK "vampiric draining ");
			roff (", causing ");
			roff (CLR_RED "%id%i (up to 5d50) damage", MAX(1, plev/10), plev);
			roff (" to the target, healing you up to 100 hit points, and satisfying some hunger.");
			roff (CLR_L_DARK " Undead and demons are not affected by draining.");
			return;
		case 12:
			roff ("Identifies an object, revealing its type and magical bonuses. " CLR_L_DARK "Some objects, ");
			roff (CLR_L_DARK "such as artifacts, may have hidden powers that are not revealed by basic identification.");
			return;
		case 13:
			roff ("Deals ");
			roff (CLR_RED "1-%i (up to 1-75) damage", plev*3/2);
			roff (" to all living monsters nearby that are not obscured by opaque terrain.");
			return;
		case 14:
			roff ("Deletes all non-unique monsters of a specified type on the level.  You do not gain ");
			roff ("experience or treasure from these monsters.  For each monster killed this way, you take ");
			roff (CLR_RED "1d4 damage");
			roff (".");
			return;
		case 15:
			roff ("Restores your experience level and all your stats.");
			return;
		case 16:
			roff ("Cures you of fear and bestows a berserk state on you for ");
			roff (CLR_YELLOW "25-50 turns");
			roff ("." CLR_L_DARK " While you are berserk, you have a large bonus to hit and to damage,");
			roff (CLR_L_DARK " and you temporarily gain 30 hit points.");
			return;
		case 17:
			roff ("Hits a target of your choice with " CLR_L_DARK "negative energy ");
			roff (", causing ");
			roff (CLR_RED "75 damage");
			roff (" to the target.");
			roff (CLR_L_DARK " Undead and demons are not affected by negative energy.");
			return;
		case 18:
			roff ("Fires a bolt of ");
			roff (CLR_L_DARK "nether ");
			roff ("at a target of your choice, doing ");
			roff (CLR_RED "%id8 (up to 17d8) damage", 6+((plev-5)/4));
			roff ("." CLR_L_DARK "  Bolt spells may occasionally fire \"beams\" that affect all monsters they pass through.");
			return;
		case 19:
			roff ("Cures you of fear and bestows a battle frenzy state on you for ");
			roff (CLR_YELLOW "25-50 turns");
			roff ("." CLR_L_DARK " While you are berserk, you have a large bonus to hit and to damage,");
			roff (CLR_L_DARK " you temporarily gain +10 speed, and you temporarily gain 30");
			roff (CLR_L_DARK " hit points, but you have a penalty to armor class.");
			return;
		case 20:
			roff ("Hits a target of your choice with " CLR_L_DARK "vampiric draining ");
			roff (", causing ");
			roff (CLR_RED "%i (up to 250) damage", 150 + 2*plev);
			roff (" to the target, healing you up to 100 hit points.");
			roff (CLR_L_DARK " Undead and demons are not affected by draining.");
			return;
		case 21:
			roff ("Fires a powerful ball of " CLR_L_DARK "darkness");
			roff (" at a target of your choice, of " CLR_VIOLET "radius 4");
			roff (" that does " CLR_RED "120 damage");
			roff (" (at its center) to monsters in the blast area.");
			roff (CLR_L_DARK " Creatures that are vulnerable to light resist darkness-based damage.");
			return;
		case 22:
			roff ("Summons a greater undead creature to serve you for ");
			roff (CLR_YELLOW "350 turns");
			roff (".");
			roff (CLR_L_DARK " There is a chance the summoning will work incompletely, and the creature will attack you.");
			roff (CLR_L_DARK " Otherwise, it will be your pet and under some limited control.");
			return;
		case 23:
			roff ("Deletes all non-unique monsters within" CLR_VIOLET " range %i", MAX_SIGHT);
			roff (" of you.  You do not gain ");
			roff ("experience or treasure from these monsters.  For each monster killed this way, you take ");
			roff (CLR_RED "1d3 damage");
			roff (".");
			return;
		case 24:
			roff ("Hits a target of your choice with a " CLR_L_WHITE "death ray ");
			roff (", causing ");
			roff (CLR_RED "%i (up to 2500) damage", 50*plev);
			roff (" to the target monster, if it is living.");
			roff (CLR_L_DARK " Monsters get a saving throw against death rays; uniques are usually not affected.");
			return;
		case 25:
			roff ("Identifies one object fully, revealing all its hidden powers.");
			return;
		case 26:
			roff ("Completely lights and maps the current level. ");
			return;
		case 27:
			roff ("Deals ");
			roff (CLR_RED "1-%i (up to 1-200) damage", plev*4);
			roff (" to all living monsters nearby that are not obscured by opaque terrain.");
			return;
		case 28:
			roff ("Reveals the ultimate horror of the Necronomicaon to all monsters in visual range.");
			roff ("  This may damage them, cause fear, and may teleport them away.");
			return;
		case 29:
			roff ("Fires a " CLR_VIOLET "radius 3");
			roff (" ball of " CLR_ORANGE "unholy fire");
			roff (" at a target of your choice that will cause ");
			roff (CLR_RED "666 damage", 3+((plev-1)/5));
			roff (" (at its center) to all monsters in the blast area.  It will also cause 50-100 damage to you. ");
			roff (CLR_L_DARK "Evil monsters take double damage from unholy fire, and it will destroy cursed objects.");
			return;
		case 30:
			roff ("This dreadful invocation deletes all non-unique monsters on the entire level.  You do not gain ");
			roff ("experience or treasure from these monsters.  For each monster killed this way, you take ");
			roff (CLR_RED "1d4 damage");
			roff (", and you also gain a point of mana.");
			return;
		case 31:
			roff ("Reforms your body out of pure negative energy for ");
			roff (CLR_YELLOW "3-%i (up to 3-33) turns", 2*plev/3);
			roff ("." CLR_L_DARK " While you are in wraithform, you have a huge bonus to armor class, ");
			roff (CLR_L_DARK " you reflect bolts and arrows, you can pass through walls, and damage done to you");
			roff (CLR_L_DARK " is significantly reduced.");
			return;
	}
}

static void roff_spell_conj(int spell)
{
	int plev = p_ptr->lev;
	bool high = (p_ptr->rp.pclass == CLASS_PRIEST || p_ptr->rp.pclass == CLASS_HIGH_MAGE);

	switch(spell)
	{
		case 0:
			roff ("Teleports you to a random open space, about ");
			roff (CLR_VIOLET "15");
			roff (" spaces away from your current location.");
			return;
		case 1:
			roff ("Fires a magical ");
			roff (CLR_L_BLUE "missile ");
			roff ("at a target of your choice, doing ");
			roff (CLR_RED "%id3 (up to 12d3) damage", 3+((plev-1)/5));
			roff ("." CLR_L_DARK "  Bolt spells may occasionally fire \"beams\" that affect all monsters they pass through.");
			roff (CLR_L_DARK " Magic missile damage cannot be resisted.");
			return;
		case 2:
			roff ("Summons an animal to serve you for ");
			roff (CLR_YELLOW "150 turns");
			roff ("." CLR_L_DARK " It will be your pet and under some limited control.");
			return;
		case 3:
			roff ("Creates a ball of magical ");
			roff (CLR_YELLOW "light ");
			roff ("centered on you, of ");
			roff (CLR_VIOLET "radius %i", plev/10 + 1);
			roff (" that will cause ");
			roff (CLR_RED "2d%i (up to 2d25) damage", plev/2);
			roff (" to monsters vulnerable to light (at its center), and will illuminate the area around you.");
			return;
		case 4:
			roff ("Cures you of ");
			roff (CLR_L_GREEN "6+1d8");
			roff (" damage.");
			return;
		case 5:
			roff ("Summons a phantom to serve you ");
			roff (CLR_YELLOW "for 300 turns");
			roff ("." CLR_L_DARK " It will be your pet and under some limited control.");
			return;
		case 6:
			roff ("Conjures a magic rope to ensnare a creature of your choice, slowing it. ");
			roff (CLR_L_DARK "Some monsters may resist being ensnared.");
			return;
		case 7:
			roff ("Fires a ball of ");
			roff (CLR_L_GREEN "poison gas ");
			roff ("at a targt of your choice, of ");
			roff (CLR_VIOLET "radius 2");
			roff (" that will cause ");
			roff (CLR_RED "%i (up to 35) damage", 10+plev/2);
			roff (" (at its center) to monsters in its area.");
			roff (CLR_L_DARK " Many monsters resist poison-based attacks.");
			return;
		case 8:
			roff ("Fires a ball of ");
			roff (CLR_VIOLET "bright colors ");
			roff ("at a targt of your choice, of ");
			roff (CLR_VIOLET "radius 2");
			roff (" that will cause ");
			roff (CLR_RED "%i (up to 10) damage", 5+plev/10);
			roff (" (at its center) to monsters in its area, and confuse them.");
			roff (CLR_L_DARK " Confused monsters move randomly and may attack other monsters. ");
			roff (CLR_L_DARK "Some monsters may resist being confused. ");
			return;
		case 9:
			roff ("Teleports you to a random open space, about ");
			roff (CLR_VIOLET "%i (up to 250)", MAX(250, plev*6));
			roff (" spaces away from your current location.");
			return;
		case 10:
			roff ("Creates a great web that is likely to ");
			roff ("cause creatures in the area not obscured by opaque terrain to move more slowly. ");
			roff (CLR_L_DARK "Some monsters may resist being slowed.");
			return;
		case 11:
			roff ("Summons a group of animals to serve you for ");
			roff (CLR_YELLOW "150 turns");
			roff ("." CLR_L_DARK " They will be your pets and under some limited control.");
			return;
		case 12:
			roff ("Teleports you to a space of your choice, " CLR_VIOLET "range 25. ");
			roff ("."  "If the space you choose is occupied, you will teleport to a random space instead.");
			return;
		case 13:
			roff ("Teleports a monster of your choice elsewhere in the dungeon. ");
			roff (CLR_L_DARK "Some monsters may resist teleportation.");
			return;
		case 14:
			roff ("Summons an elemental to serve you for ");
			roff (CLR_YELLOW "150 turns");
			roff ("." CLR_L_DARK " There is a chance the summoning will work incompletely, and the creature will attack you.");
			roff (CLR_L_DARK " Otherwise, it will be your pet and under some limited control.");
			return;
		case 15:
			roff ("Allows you to transport between the wilderness and a level in a dungeon. ");
			roff ("The dungeon level you travel to will be the deepest you have ventured in that dungeon. ");
			roff ("If you cast this spell in the dungeon at a lower depth, you can \"reset\" where you will ");
			roff ("recall to in the future.  Transport does not take place immediately, and can be cancelled ");
			roff ("by another Word of Recall effect.");
			return;
		case 16:
			roff ("Fires a bolt of magical ");
			roff (CLR_GREEN "acid ");
			roff ("at a target of your choice, doing ");
			roff (CLR_RED "%id6 (up to 15d6) damage", 6+((plev-1)/5));
			roff ("." CLR_L_DARK "  Bolt spells may occasionally fire \"beams\" that affect all monsters they pass through.");
			return;
		case 17:
			roff ("Fires a bolt of magical ");
			roff (CLR_RED "fire ");
			roff ("at a target of your choice, doing ");
			roff (CLR_RED "%id6 (up to 18d6) damage", 9+((plev-1)/5));
			roff ("." CLR_L_DARK "  Bolt spells may occasionally fire \"beams\" that affect all monsters they pass through.");
			return;
		case 18:
			roff ("Bestows you with a magical shield for ");
			roff (CLR_VIOLET "30-50 turns");
			roff ("." CLR_L_DARK "  While your shield is active you get a bonus of +50 to your armor class.");
			return;
		case 19:
			roff ("Summons a hydra to serve you for ");
			roff (CLR_YELLOW "150 turns");
			roff ("." CLR_L_DARK " There is a chance the summoning will work incompletely, and the creature will attack you.");
			roff (CLR_L_DARK " Otherwise, it will be your pet and under some limited control.");
			return;
		case 20:
			roff ("Summons a dragon to serve you for ");
			roff (CLR_YELLOW "150 turns");
			roff ("." CLR_L_DARK " There is a chance the summoning will work incompletely, and the creature will attack you.");
			roff (CLR_L_DARK " Otherwise, it will be your pet and under some limited control.");
			return;
		case 21:
			roff ("Causes all monsters nearby that are not obscured by opaque terrain to be teleported ");
			roff ("somewhere else." CLR_L_DARK " Some monsters may be able to resist teleportation.");
			return;
		case 22:
			roff ("Cures you of ");
			roff (CLR_L_GREEN "150+15d8");
			roff (" damage and cures wounds, stunning, and poison completely.");
			return;
		case 23:
			roff ("Completely lights and maps the current level. ");
			return;
		case 24:
			roff ("Teleports you to a space of your choice, " CLR_VIOLET "range %i (up to 80)", 30+plev);
			roff (". If the space you choose is occupied, you will teleport to a random space instead.");
			return;
		case 25:
			roff ("Fires balls of ");
			roff (CLR_L_GREEN "poison gass ");
			roff ("in multiple directions, each of " CLR_VIOLET "radius 3");
			roff (", each doing ");
			roff (CLR_RED "%i (up to 90) damage", 40+plev);
			roff (CLR_L_DARK " Many monsters resist poison-based attacks.");
			return;
		case 26:
			roff ("Completely re-generates the dungeon level around you.");
			return;
		case 27:
			roff ("Makes you ethereal for ");
			roff (CLR_VIOLET "20-40 turns");
			roff ("." CLR_L_DARK "  While you are ethereal, you can pass through solid rock, and you get a substantial");
			roff (" bonus to your armor class.");
			return;
		case 28:
			roff ("Summons a demonic creature to serve you for ");
			roff (CLR_YELLOW "150 turns");
			roff (".  There is a chance you will get a group of demons instead.");
			roff (CLR_L_DARK " There is a chance the summoning will work incompletely, and the creatures will attack you.");
			roff (CLR_L_DARK " Otherwise, they will be your pets and under some limited control.");
			return;
		case 29:
			roff ("Opens up a gateway to a dimension of death, dealing ");
			roff (CLR_RED "1-%i (up to 1-200) damage", 50+plev*3);
			roff (" to all monsters nearby that are not obscured by opaque terrain.");
			return;
		case 30:
			roff ("Summons a greater undead creature to serve you for ");
			roff (CLR_YELLOW "150 turns");
			roff ("." CLR_L_DARK " There is a chance the summoning will work incompletely, and the creature will attack you.");
			roff (CLR_L_DARK " Otherwise, it will be your pet and under some limited control.");
			return;
		case 31:
			roff ("Summons powerful creatures (angels, ancient dragons, or greater undead) to serve you.");
			return;
	}
}

static void roff_spell_arcane(int spell)
{
	int plev = p_ptr->lev;
	bool high = (p_ptr->rp.pclass == CLASS_PRIEST || p_ptr->rp.pclass == CLASS_HIGH_MAGE);

	switch(spell)
	{
		case 0:
			roff ("Fires a bolt of magical ");
			roff (CLR_L_BLUE "electricity ");
			roff ("at a target of your choice, doing ");
			roff (CLR_RED "%id3 (up to 12d3) damage", 3+((plev-1)/5));
			roff ("." CLR_L_DARK "  Bolt spells may occasionally fire \"beams\" that affect all monsters they pass through.");
			return;
		case 1:
			roff ("Lets you sense the location and type of any nearby creatures.");
		    roff (CLR_VIOLET " Range: %i", MAX_DETECT);
			return;
		case 2:
			roff ("Teleports you to a random open space, about ");
			roff (CLR_VIOLET "10");
			roff (" spaces away from your current location.");
			return;
		case 3:
			roff ("Creates a ball of magical ");
			roff (CLR_YELLOW "light ");
			roff ("centered on you, of");
			roff (CLR_VIOLET " radius %i", plev/10 + 1);
			roff (" that will cause ");
			roff (CLR_RED "2d%i (up to 2d25) damage", plev/2);
			roff (" to monsters vulnerable to light (at its center), and will illuminate the area around you.");
			return;
		case 4:
			roff ("Magically adds fuel to a torch or lantern.");
			return;
		case 5:
			roff ("Lets you sense the presence of nearby traps.");
			roff (CLR_VIOLET " Range: %i", MAX_DETECT);
			roff (CLR_L_DARK " You will receive a warning when you leave the trap-detected area.");
			return;
		case 6:
			roff ("Lets you momentarily sense the location and type of any nearby invisible creatures.");
		    roff (CLR_VIOLET " Range: %i", MAX_DETECT);
			return;
		case 7:
			roff ("Completely cures you of poison and fear.");
			return;
		case 8:
			roff ("Cures you of ");
			roff (CLR_L_GREEN "12+2d8");
			roff (" damage and cures most wounds.");
			return;
		case 9:
			roff ("Bestows you with a magical resistance to cold and fire for ");
			roff (CLR_VIOLET "20-40 turns");
			roff ("." CLR_L_DARK "  While you are resistant, you take less damage from elemental attacks, and");
			roff (CLR_L_DARK " items in your backpack are somewhat protected from elemental-based destruction.");
			return;
		case 10:
			roff ("Pseudo-identifies an item, giving you a general sense of whether it is enchanted or cursed.");
			return;
		case 11:
			roff ("Destroys all doors and traps within " CLR_VIOLET "radius 1");
			roff (" of you.");
			return;
		case 12:
			roff ("Grants you the ability to see invisible monsters, for ");
			roff (CLR_YELLOW "24-48 turns");
			roff (".");
			return;
		case 13:
			roff ("Bestows you with a magical resistance to blindness for ");
			roff (CLR_VIOLET "20-40 turns");
			roff (".");
			return;
		case 14:
			roff ("Turns one wall segment of your choice to mud.");
			roff (CLR_L_DARK " Will cause damage to creatures made of stone.");
			return;
		case 15:
			roff ("Fires a beam of magical ");
			roff (CLR_YELLOW "light ");
			roff ("in a direction of your choice, doing ");
			roff (CLR_RED "6d8 damage");
			roff (" to all monsters in its path that are vulnerable to light, and illuminating the spaces it touches.");
			return;
		case 16:
			roff ("Satisfies your hunger completely, even if you are unable to eat normal food.");
			return;
		case 17:
			roff ("Teleports you to a random open space, about ");
			roff (CLR_VIOLET "%i (up to 250)", plev*5);
			roff (" spaces away from your current location.");
			return;
		case 18:
			roff ("Bestows you with a magical resistance to cold, fire, acid, and electricity for ");
			roff (CLR_VIOLET "20-40 turns");
			roff ("." CLR_L_DARK "  While you are resistant, you take less damage from elemental attacks, and");
			roff (CLR_L_DARK " items in your backpack are somewhat protected from elemental-based destruction.");
			return;
		case 19:
			roff ("Bestows you with a magical resistance to poison for ");
			roff (CLR_VIOLET "20-40 turns");
			roff ("." CLR_L_DARK "  While you are resistant, you take less damage from posion-based attacks, and");
			roff (CLR_L_DARK " you cannot be poisoned.");
		case 20:
			roff ("Reveals the shape of the dungeon near you. ");
			roff (CLR_VIOLET " Range: %i", MAX_DETECT);
			return;
		case 21:
			roff ("Lets you sense the presence of nearby creatures, invisible creatures, objects, buried treasure, ");
			roff ("traps, doors, secret doors, and stairs.");
			roff (CLR_VIOLET " Range: %i", MAX_DETECT);
			roff (CLR_L_DARK " You will receive a warning when you leave the trap-detected area.");
			return;
		case 22:
			roff ("Teleports a monster of your choice elsewhere in the dungeon. ");
			roff (CLR_L_DARK "Some monsters may resist teleportation.");
			return;
		case 23:
			roff ("Identifies an object, revealing its type and magical bonuses. " CLR_L_DARK "Some objects, ");
			roff (CLR_L_DARK "such as artifacts, may have hidden powers that are not revealed by basic identification.");
			return;
		case 24:
			roff ("Cures you of ");
			roff (CLR_L_GREEN "48+8d8");
			roff (" damage and completely cures you of cuts, stunning, and poison.");
			return;
		case 25:
			roff ("Teleports you, at random, either one level up or down in the dungeon.");
			return;
		case 26:
			roff ("Fires a ball of ");
			roff (CLR_GREEN "acid, " CLR_RED "fire, " CLR_L_BLUE "electricity, ");
			roff ("or " CLR_BLUE "frost ");
			roff ("at a targt of your choice, of ");
			roff (CLR_VIOLET "radius 2");
			roff (" that will cause ");
			roff (CLR_RED "%i (up to 125) damage", 75+plev);
			roff (" (at its center) to monsters in its area. ");
			roff (CLR_L_DARK "Elemental damage may destroy certain objects on the floor.");
			return;
		case 27:
			roff ("Enhances an armor item of your choice, permanently increasing its bonus to armor class.");
			roff (" In addition to its mana cost, this spell costs " CLR_YELLOW "200 gold");
			roff (" to cast.");
			roff (CLR_L_DARK "The more enchanted the armor is to begin with, the less effect this will have.");
			return;
		case 28:
			roff ("Will recharge a wand, staff, or rod. ");
			roff (CLR_L_DARK "There is a chance it will backfire and blow up or completely drain the item.");
			return;
		case 29:
			roff ("Informs you about all the magic currently affecting you from spells, temporary effects, ");
			roff (" mutations, or magical equipment.");
			return;
		case 30:
			roff ("Allows you to transport between the wilderness and a level in a dungeon. ");
			roff ("The dungeon level you travel to will be the deepest you have ventured in that dungeon. ");
			roff ("If you cast this spell in the dungeon at a lower depth, you can \"reset\" where you will ");
			roff ("recall to in the future.  Transport does not take place immediately, and can be cancelled ");
			roff ("by another Word of Recall effect.");
			return;
		case 31:
			roff ("Completely lights and maps the current level. ");
			roff ("Also grants you the power of telepathy for ");
			roff (CLR_YELLOW "25-55 turns");
			roff (".");
			roff (CLR_L_DARK " With telepathy, you automatically sense nearby intelligent creatures.");
			return;
	}
}


static void do_cmd_browse_aux3(object_type *o_ptr, int spell)
{
	int realm = o_ptr->tval - TV_BOOKS_MIN;

	switch(realm+1)
	{
		case REALM_LIFE:	/* * LIFE * */
			roff_spell_life(spell);
			break;
		case REALM_SORCERY:	/* * SORCERY * */
			roff_spell_sorcery(spell);
			break;
		case REALM_NATURE:	/* * NATURE * */
			roff_spell_nature(spell);
			break;
		case REALM_CHAOS:	/* * CHAOS * */
			roff_spell_chaos(spell);
			roff (CLR_L_DARK " Casting chaotic magic can cause damaging side effects when you fail.");
			break;
		case REALM_DEATH:	/* * DEATH * */
			roff_spell_death(spell);
			if (o_ptr->sval == 3)
				roff (CLR_L_DARK " Attempting to cast spells from the Necronomicon may blast your sanity.");
			break;
		case REALM_CONJ:	/* * CONJURATION * */
			roff_spell_conj(spell);
			break;
		case REALM_ARCANE:	/* * ARCANE * */
			roff_spell_arcane(spell);
			break;
	}

	/* A little whitespace */
	roff("\n");
}

static const object_type *resize_o_ptr;
static int resize_spell;

static void resize_browse(void)
{
	/* Recall object */
	do_cmd_browse_aux3(resize_o_ptr, resize_spell);
}


static void do_cmd_browse_aux2(object_type *o_ptr, int spell)
{
	void (*old_hook) (void);
	int realm = o_ptr->tval - TV_BOOKS_MIN;

	/* Save the screen */
	screen_save();

	/* Start in top left */
	Term_gotoxy(0, 0);

	/* Show name of spell */
	roff("%s", spell_names[realm][spell]);

	/* Start the description a bit lower */
	roff("\n\n");

	/* Recall object */
	do_cmd_browse_aux3(o_ptr, spell);

	/* Remember what the resize hook was */
	old_hook = angband_term[0]->resize_hook;

	/* Hack - change the redraw hook so bigscreen works */
	angband_term[0]->resize_hook = resize_browse;

	/* Remember essentials for resizing */
	resize_o_ptr = o_ptr;
	resize_spell = spell;

	/* Wait for the player to read the info */
	(void)inkey();

	/* Hack - change the redraw hook so bigscreen works */
	angband_term[0]->resize_hook = old_hook;

	/* The size may have changed during the object description */
	angband_term[0]->resize_hook();

	/* Hack - Flush it */
	Term_fresh();

	/* Restore the screen */
	screen_load();
}


/*
 * Peruse the spells/prayers in a given book.  Note that we may
 * bypass do_cmd_browse by calling this function directly (as we
 * do from identify_fully_aux() which has the effect of allowing
 * any book to be browsed regardless of the player's realm choice.
 *
 * Note that *all* spells in the book are listed
 */
void do_cmd_browse_aux(const object_type *o_ptr)
{
	int sval;
	int spell;
	int num = 0;
	int increment = 0;
	bool home = FALSE;

	byte spells[PY_MAX_SPELLS];


	/* Access the item's sval */
	sval = o_ptr->sval;

	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();

	/* Extract spells */
	for (spell = 0; spell < 32; spell++)
	{
		/* Check for this spell */
		if ((fake_spell_flags[sval] & (1L << spell)))
		{
			/* Collect this spell */
			spells[num++] = spell;
		}
	}

	/* Only do the detailed browse for a realm we know. */
	if (o_ptr->tval == p_ptr->spell.r[0].realm + TV_BOOKS_MIN - 1 ||
		o_ptr->tval == p_ptr->spell.r[1].realm + TV_BOOKS_MIN - 1)
	{
		home = TRUE;
	}

	if (o_ptr->tval == REALM2_BOOK) increment = 32;

	/* Save the screen */
	screen_save();

	/* Display the spells */
	print_spells(spells, num, 20, 1, (o_ptr->tval - TV_BOOKS_MIN));

	/* Clear the top line */
	clear_msg();

	/* Get spell.  If no valid choices, just display the list */
	if (home)
	{
		while (get_spell(&spell, "learn about", sval, TRUE,
					   (bool)(increment ? TRUE : FALSE)))
		{
			do_cmd_browse_aux2(o_ptr, spell);
		}
	}

	/* Never got a chance to pick one? */
	if (spell == -2 || !home)
	{
		pause_line(0);
	}


	/* Restore the screen */
	screen_load();
}


/*
 * Peruse the spells/prayers in a book
 *
 * Note that browsing is allowed while confused or blind,
 * and in the dark, primarily to allow browsing in stores.
 */
void do_cmd_browse(void)
{
	object_type *o_ptr;
	cptr q, s;

	/* Warriors are illiterate */
	if (!(p_ptr->spell.r[0].realm || p_ptr->spell.r[1].realm))
	{
		msgf("You cannot read books!");
		return;
	}

	/* Restrict choices to books */
	item_tester_hook = item_tester_hook_is_book;

	/* Get an item */
	q = "Browse which book? ";
	s = "You have no books that you can read.";

	o_ptr = get_item(q, s, (USE_INVEN | USE_FLOOR));

	/* Not a valid item */
	if (!o_ptr) return;

	/* Print out the spells */
	do_cmd_browse_aux(o_ptr);
}


/*
 * Study a book to gain a new spell/prayer
 */
void do_cmd_study(void)
{
	int i, sval;
	int increment = 0;

	/* Spells of r[1].realm will have an increment of +32 */
	int spell = -1;

	cptr p = ((mp_ptr->spell_book == TV_SORCERY_BOOK) ? "spell" : "prayer");

	object_type *o_ptr;

	cptr q, s;

	if (!p_ptr->spell.r[0].realm)
	{
		msgf("You cannot read books!");
		return;
	}

	if (p_ptr->tim.blind || no_lite())
	{
		msgf("You cannot see!");
		return;
	}

	if (p_ptr->tim.confused)
	{
		msgf("You are too confused!");
		return;
	}

	if (!(p_ptr->new_spells))
	{
		msgf("You cannot learn any new %ss!", p);
		return;
	}

	msgf("You can learn %d new %s%s.", p_ptr->new_spells, p,
			   (p_ptr->new_spells == 1 ? "" : "s"));
	message_flush();

	/* Restrict choices to "useful" books */
	item_tester_tval = mp_ptr->spell_book;

	/* Get an item */
	q = "Study which book? ";
	s = "You have no books that you can read.";

	o_ptr = get_item(q, s, (USE_INVEN | USE_FLOOR));

	/* Not a valid item */
	if (!o_ptr) return;

	/* Access the item's sval */
	sval = o_ptr->sval;

	if (o_ptr->tval == REALM2_BOOK) increment = 32;

	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();

	/* Mage -- Learn a selected spell */
	if (mp_ptr->spell_book != TV_LIFE_BOOK)
	{
		/* Ask for a spell, allow cancel */
		if (!get_spell(&spell, "study", sval, FALSE,
					   (bool)(increment ? TRUE : FALSE))
			&& (spell == -1)) return;
	}

	/* Priest -- Learn a random prayer */
	else
	{
		int k = 0;

		int gift = -1;

		/* Extract spells */
		for (spell = 0; spell < 32; spell++)
		{
			/* Check spells in the book */
			if ((fake_spell_flags[sval] & (1L << spell)))
			{
				/* Skip non "okay" prayers */
				if (!spell_okay(spell, FALSE,
								p_ptr->spell.r[increment / 32].realm - 1))
					continue;

				/* Hack -- Prepare the randomizer */
				k++;

				/* Hack -- Apply the randomizer */
				if (one_in_(k)) gift = spell;
			}
		}

		/* Accept gift */
		spell = gift;
	}

	/* Nothing to study */
	if (spell < 0)
	{
		/* Message */
		msgf("You cannot learn any %ss in that book.", p);

		/* Abort */
		return;
	}


	/* Take a turn */
	p_ptr->state.energy_use = 100;

	if (increment) spell += increment;

	/* Learn the spell */
	p_ptr->spell.r[spell / 32].learned |= (1L << (spell % 32));

	/* Find the next open entry in "spell.order[]" */
	for (i = 0; i < PY_MAX_SPELLS; i++)
	{
		/* Stop at the first empty space */
		if (p_ptr->spell.order[i] == 99) break;
	}

	/* Add the spell to the known list */
	p_ptr->spell.order[i++] = spell;

	/* Mention the result */
	msgf(MSGT_STUDY, "You have learned the %s of %s.",
				   p, spell_names
				   [p_ptr->spell.r[increment / 32].realm - 1][spell % 32]);

	if (mp_ptr->spell_book == TV_LIFE_BOOK)
		chg_virtue(V_FAITH, 1);
	else
		chg_virtue(V_KNOWLEDGE, 1);

	/* Sound */
	sound(SOUND_STUDY);

	/* One less spell available */
	p_ptr->new_spells--;

	/* Message if needed */
	if (p_ptr->new_spells)
	{
		/* Message */
		msgf("You can learn %d more %s%s.",
				   p_ptr->new_spells, p, (p_ptr->new_spells != 1) ? "s" : "");
	}

	/* Redraw Study Status */
	p_ptr->redraw |= (PR_STUDY);

	/* Talking to yourself... */
	make_noise(1);
}


#define MAX_BIZARRE		6


static const int bizarre_num[MAX_BIZARRE] =
{
	SUMMON_BIZARRE1,
	SUMMON_BIZARRE2,
	SUMMON_BIZARRE3,
	SUMMON_BIZARRE4,
	SUMMON_BIZARRE5,
	SUMMON_BIZARRE6,
};


static void wild_magic(int spell)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

	switch (randint0(spell) + randint0(9))
	{
		case 1:
		case 2:
		case 3:
		{
			teleport_player(10);
			break;
		}
		case 4:
		case 5:
		case 6:
		{
			teleport_player(100);
			break;
		}
		case 7:
		case 8:
		{
			teleport_player(200);
			break;
		}
		case 9:
		case 10:
		case 11:
		{
			(void)unlite_area(10, 3);
			break;
		}
		case 12:
		case 13:
		case 14:
		{
			(void)lite_area(damroll(2, 3), 2);
			break;
		}
		case 15:
		{
			(void)destroy_doors_touch();
			break;
		}
		case 16:  case 17:
		{
			wall_breaker();
			break;
		}
		case 18:
		{
			(void)sleep_monsters_touch();
			break;
		}
		case 19:
		case 20:
		{
			(void)trap_creation();
			break;
		}
		case 21:
		case 22:
		{
			(void)door_creation();
			break;
		}
		case 23:
		case 24:
		case 25:
		{
			aggravate_monsters(0);
			break;
		}
		case 26:
		{
			(void)earthquake(px, py, 5);
			break;
		}
		case 27:
		case 28:
		{
			(void)gain_mutation(0);
			break;
		}
		case 29:
		case 30:
		{
			(void)apply_disenchant();
			break;
		}
		case 31:
		{
			(void)lose_all_info();
			break;
		}
		case 32:
		{
			(void)fire_ball(GF_CHAOS, 0, spell + 5, 1 + (spell / 10));
			break;
		}
		case 33:
		{
			(void)wall_stone();
			break;
		}
		case 34:
		case 35:
		{
			int i;
			int type = bizarre_num[randint0(6)];

			for (i = 0; i < 8; i++)
			{
				(void)summon_specific(0, px, py, (p_ptr->depth * 3) / 2, type,
									  TRUE, FALSE, FALSE);
			}
			break;
		}
		case 36:
		case 37:
		{
			(void)activate_hi_summon();
			break;
		}
		case 38:
		{
			(void)summon_cyber(-1, px, py);
			break;
		}
		default:
		{
			int count = 0;

			(void)activate_ty_curse(FALSE, &count);

			break;
		}
	}

	return;
}


static bool cast_life_spell(int spell)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

	int dir;
	int plev = p_ptr->lev;

	switch (spell)
	{
		case 0:				/* Detect Life */
			(void)detect_monsters_living();
			break;
		case 1:				/* Cure Light Wounds */
			(void)hp_player(8+damroll(1, 10));
			(void)inc_cut(-10);
			break;
		case 2:				/* Bless */
			(void)inc_blessed(rand_range(12, 24));
			break;
		case 3:				/* Call Light */
			(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
			break;
		case 4:				/* Detect Traps + Doors */
			(void)detect_traps(TRUE);
			(void)detect_doors();
			(void)detect_stairs();
			break;
		case 5:				/* Cure Medium Wounds */
			(void)hp_player(16+damroll(2, 10));
			(void)inc_cut(-40);
			(void)inc_poisoned(-p_ptr->tim.poisoned/2);
			break;
		case 6:				/* Heroism */
			(void)inc_hero(rand_range(25, 50));
			(void)hp_player(10);
			(void)clear_afraid();
			break;
		case 7:				/* Remove Curse */
			(void)remove_curse();
			break;
		case 8:				/* Holy Prayer */
			(void)inc_blessed(rand_range(50, 100));
			break;
		case 9:				/* Cure Critical Wounds */
			(void)hp_player(32+damroll(4, 10));
			(void)clear_stun();
			(void)clear_cut();
			(void)clear_poisoned();
			break;
		case 10:				/* Sense Unseen */
			(void)inc_tim_invis(rand_range(24, 48));
			break;
		case 11:				/* Holy Orb */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fire_ball(GF_HOLY_FIRE, dir,
							(damroll(3, 6) + plev +
							 (plev / ((p_ptr->rp.pclass == CLASS_PRIEST ||
									   p_ptr->rp.pclass ==
									   CLASS_HIGH_MAGE) ? 2 : 4))),
							((plev < 30) ? 2 : 3));

			break;
		case 12:				/* Protection from Evil */
			(void)inc_protevil(randint1(25) + 3 * p_ptr->lev);
			break;
		case 13:				/* Healing */
			(void)hp_player(250 + damroll(10,10));
			(void)clear_stun();
			(void)clear_cut();
			(void)clear_poisoned();
			(void)clear_afraid();
			break;
		case 14:				/* Glyph of Warding */
			(void)warding_glyph();
			break;
		case 15:				/* Word of Recall */
			(void)word_of_recall();
			break;
		case 16:				/* Satisfy Hunger */
			(void)set_food(PY_FOOD_MAX - 1);
			break;
		case 17:				/* Cure Mortal Wounds */
			(void)hp_player(64 + damroll(8,10));
			(void)clear_stun();
			(void)clear_cut();
			(void)clear_poisoned();
		case 18:				/* Dispel Undead + Demons */
			(void)dispel_undead(plev * 3);
			(void)dispel_demons(plev * 3);
			break;
		case 19:				/* Summon Angel */
			if(!player_summon(PSUM_ANGEL, 20+plev, FALSE, 150, 0)) msgf("No one answers your call.");
			break;
		case 20:				/* Dispel Evil */
			(void)dispel_evil(plev * 4);
			break;
		case 21:				/* Banishment */
			if (banish_evil(100))
			{
				msgf("The power of your god banishes evil!");
			}
			break;
		case 22:				/* Holy Word */
			(void)dispel_evil(plev * 4);
			(void)hp_player(1000);
			(void)clear_afraid();
			(void)clear_poisoned();
			(void)clear_stun();
			(void)clear_cut();
			break;
		case 23:				/* Restoration */
			(void)do_res_stat(A_STR);
			(void)do_res_stat(A_INT);
			(void)do_res_stat(A_WIS);
			(void)do_res_stat(A_DEX);
			(void)do_res_stat(A_CON);
			(void)do_res_stat(A_CHR);
			(void)restore_level();
			(void)hp_player(100);
			break;
		case 24:				/* Holy Armor */
			(void)inc_shield(rand_range(30, 50));
			break;
		case 25:				/* Dispel Curse */
			(void)remove_all_curse();
			break;
		case 26: 				/* Flame Strike */
			(void)fire_ball(GF_FIRE, 0, 200, 6);
			break;
		case 27:				/* Bless Weapon */
			if (p_ptr->au < 500) {
				msgf("You don't have enough gold.");
				return(FALSE);
			}
		    if (!bless_weapon()) return(FALSE);
			p_ptr->au = MIN(p_ptr->au-500, 0);  /* Costs 500 gold */
			break;
		case 28:				/* Circle of Protection */
			(void)warding_glyph();
			(void)glyph_creation();
			break;
		case 29:				/* Holy Vision */
			if (!identify_fully()) return(FALSE);
			break;
		case 30:				/* Divine Intervention */
			(void)project(0, 1, px, py, 777, GF_HOLY_FIRE, PROJECT_KILL);
			(void)dispel_monsters(plev * 4);
			(void)slow_monsters();
			(void)stun_monsters(plev * 4);
			(void)confuse_monsters(plev * 4);
			(void)turn_monsters(plev * 4);
			(void)stasis_monsters(plev * 4);
			(void)player_summon(PSUM_ANGEL, 45+plev, FALSE, 600, PSUM_FORCE_SUCCESS);
			(void)inc_shero(rand_range(25, 50));
			(void)hp_player(300);

			/* Haste */
			(void)inc_fast(randint1(20 + plev) + plev);

			(void)clear_afraid();
			break;
		case 31:				/* Holy Invulnerability */
			(void)inc_invuln(rand_range(7, 14));
			break;
		default:
			msgf("You cast an unknown Life spell: %d.", spell);
			message_flush();
	}

	make_noise(2);

	return TRUE;
}



static bool cast_sorcery_spell(int spell)
{
	int dir;
	int plev = p_ptr->lev;

	switch (spell)
	{
		case 0:				/* Detect Monsters */
			(void)detect_monsters_normal();
			break;
		case 1:				/* Phase Door */
			(void)teleport_player(10);
			break;
		case 2:				/* Detect Doors and Traps */
			(void)detect_traps(TRUE);
			(void)detect_doors();
			(void)detect_stairs();
			break;
		case 3:				/* Light Area */
			(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
			break;
		case 4:				/* Confuse Monster */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)confuse_monster(dir, (plev * 3) / 2);
			break;
		case 5:				/* Teleport */
			teleport_player(plev * 5);
			break;
		case 6:				/* Sleep Monster */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)sleep_monster(dir);
			break;
		case 7:				/* Slow Monster */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)slow_monster(dir);
			break;
		case 8:				/* Magic Mapping */
			map_area();
			break;
		case 9:				/* Identify */
			if(!ident_spell())
				return (FALSE);
			break;
		case 10:				/* Charm Monster */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)charm_monster(dir, plev);
			break;
		case 11:				/* Mass Sleep */
			(void)sleep_monsters();
			break;
		case 12:				/* Teleport Away */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fire_beam(GF_AWAY_ALL, dir, plev);
			break;
		case 13:				/* Haste Self */
			(void)inc_fast(randint1(20 + plev) + plev);
			break;
		case 14:				/* Detection True */
			(void)detect_all();
			break;
		case 15:				/* Identify True */
			if(!identify_fully())
				return (FALSE);
			break;
		case 16:				/* Detect Objects and Treasure */
			(void)detect_objects_normal();
			(void)detect_treasure();
			(void)detect_objects_gold();
			break;
		case 17:				/* Detect Enchantment */
			(void)detect_objects_magic();
			break;
		case 18:				/* Recharging */
			if (!recharge(plev * 4))
				return (FALSE);
			break;
		case 19:				/* Enchant Weapon */
			if (p_ptr->au < 200) {
				msgf("You don't have enough gold.");
				return(FALSE);
			}
		    if (!enchant_spell(randint1(4), randint1(4), 0)) return(FALSE);
			p_ptr->au = MIN(p_ptr->au-200, 0);  /* Costs 200 gold */
			break;
		case 20:				/* Enchant Armour */
			if (p_ptr->au < 200) {
				msgf("You don't have enough gold.");
				return(FALSE);
			}
		    if (!enchant_spell(0, 0, rand_range(2,5))) return(FALSE);
			p_ptr->au = MIN(p_ptr->au-200, 0);  /* Costs 200 gold */
			break;
		case 21:				/* Sense Minds */
			(void)inc_tim_esp(rand_range(25, 55));
			break;
		case 22:				/* Teleport Level */
			(void)teleport_player_level();
			break;
		case 23:				/* Word of Recall */
			word_of_recall();
			break;
		case 24:				/* Stasis */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)stasis_monster(dir);
			break;
		case 25:				/* Alchemy */
			if (!alchemy())
				return (FALSE);
			break;
		case 26:				/* Dimension Door */
			msgf("You open a dimensional gate. Choose a destination.");
			if (!dimension_door())
				return (FALSE);
			break;
		case 27:				/* Magic Shield */
			(void)inc_shield(rand_range(30, 50));
			break;
		case 28:				/* Rune of Warding */
			(void)warding_glyph();
			break;
		case 29:				/* Clairvoyance */
			wiz_lite();
			if (!(FLAG(p_ptr, TR_TELEPATHY)))
			{
				(void)inc_tim_esp(rand_range(25, 55));
			}
			break;
		case 30:				/* Weapon Branding */
			if (p_ptr->au < 1000) {
				msgf("You don't have enough gold.");
				return(FALSE);
			}
		    brand_weapon(0);
			p_ptr->au = MIN(p_ptr->au-1000, 0);  /* Costs 1000 gold */
			break;
		case 31:				/* Globe of Invulnerability */
			(void)inc_invuln(rand_range(9, 16));
			break;
		default:
			msgf("You cast an unknown Sorcery spell: %d.", spell);
			message_flush();
	}

	make_noise(2);

	return TRUE;
}


static bool cast_nature_spell(int spell)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

	int dir;
	int beam;
	int plev = p_ptr->lev;

	if (p_ptr->rp.pclass == CLASS_MAGE) beam = plev;
	else if (p_ptr->rp.pclass == CLASS_HIGH_MAGE) beam = plev + 10;
	else
		beam = plev / 2;

	switch (spell)
	{
		case 0:				/* Detect Creatures */
			(void)detect_monsters_normal();
			break;
		case 1:				/* First Aid */
			(void)hp_player(6+damroll(1, 8));
			(void)inc_cut(-15);
			break;
		case 2:				/* Detect Doors + Traps */
			(void)detect_traps(TRUE);
			(void)detect_doors();
			(void)detect_stairs();
			break;
		case 3:				/* Foraging */
			(void)set_food(PY_FOOD_MAX - 1);
			break;
		case 4:				/* Daylight */
			(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
			if ((FLAG(p_ptr, TR_HURT_LITE)) &&
				!(FLAG(p_ptr, TR_RES_LITE)) &&
				!(FLAG(p_ptr, TR_IM_LITE)))
			{
				msgf("The daylight scorches your flesh!");
				take_hit(damroll(2, 2), "daylight");
			}
			break;
		case 5:				/* Animal Taming */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)charm_animal(dir, (3*plev)/2);
			break;
		case 6:				/* Resist Environment */
			(void)inc_oppose_cold(rand_range(20, 40));
			(void)inc_oppose_fire(rand_range(20, 40));
			(void)inc_oppose_elec(rand_range(20, 40));
			break;
		case 7:				/* Cure Wounds + Poison */
			(void)hp_player(12+damroll(2,8));
			(void)clear_cut();
			(void)clear_poisoned();
			break;
		case 8:				/* Stone to Mud */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)wall_to_mud(dir);
			break;
		case 9:				/* Lightning Bolt */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fire_bolt_or_beam(beam - 10, GF_ELEC, dir,
									damroll(3 + ((plev - 5) / 4), 8));
			break;
		case 10:				/* Frost Bolt */
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_bolt_or_beam(beam - 10, GF_COLD, dir,
									damroll(5 + ((plev - 5) / 4), 8));
			break;
		case 11:				/* Ray of Sunlight */
			if (!get_aim_dir(&dir)) return FALSE;
			msgf("A line of sunlight appears.");
			(void)lite_line(dir, damroll(6, 8));
			break;
		case 12:				/* Entangle */
			(void)slow_monsters();
			break;
		case 13:				/* Summon Animals */
	        if(!player_summon(PSUM_ANIMAL, 20+(2*plev), TRUE, 150, 0)) msgf("Nothing answers your call.");
			break;
		case 14:				/* Protection from Corrosion */
			if (p_ptr->au < 40) {
				msgf("You don't have enough gold.");
				return(FALSE);
			}
		    if (!rustproof()) return(FALSE);
			p_ptr->au = MIN(p_ptr->au-40, 0);  /* Costs 40 gold */
			break;
		case 15:				/* Herbal Healing */
			(void)hp_player(1000);
			(void)clear_stun();
			(void)clear_cut();
			(void)clear_poisoned();
			break;
		case 16:				/* Wall of Wood */
			(void)door_creation();
			break;
		case 17:				/* Nature Awareness */
			map_area();
			(void)detect_traps(TRUE);
			(void)detect_doors();
			(void)detect_stairs();
			(void)detect_monsters_normal();
			break;
		case 18:				/* Stone Skin */
			(void)inc_shield(rand_range(30, 50));
			break;
		case 19:				/* Resistance True */
			(void)inc_oppose_acid(rand_range(30, 50));
			(void)inc_oppose_elec(rand_range(30, 50));
			(void)inc_oppose_fire(rand_range(30, 50));
			(void)inc_oppose_cold(rand_range(30, 50));
			(void)inc_oppose_pois(rand_range(30, 50));
			break;
		case 20:				/* Wall of Stone */
			(void)wall_stone();
			break;
		case 21:				/* Wall of Water */
			(void)water_creation();
			break;
		case 22:				/* Stone Tell */
			if(!identify_fully())
				return(FALSE);
			break;
		case 23:				/* Wall of Fire */
			(void)lava_creation();
			break;
		case 24:				/* Earthquake */
			(void)earthquake(px, py, 10);
			break;
		case 25:				/* Whirlwind Attack */
			whirlwind_attack();
			break;
		case 26:				/* Blizzard */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fire_ball(GF_COLD, dir, 70 + plev, (plev / 12) + 1);
			break;
		case 27:				/* Lightning Storm */
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_ball(GF_ELEC, dir, 90 + plev, (plev / 12) + 1);
			break;
		case 28:				/* Whirlpool */
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_ball(GF_WATER, dir, 100 + plev, (plev / 12) + 1);
			break;
		case 29:				/* Call Sunlight */
			(void)fire_ball(GF_LITE, 0, 150, 8);
			wiz_lite();
			if ((FLAG(p_ptr, TR_HURT_LITE)) &&
				!(FLAG(p_ptr, TR_RES_LITE)) &&
				!(FLAG(p_ptr, TR_IM_LITE)))
			{
				msgf("The sunlight scorches your flesh!");
				take_hit(50, "sunlight");
			}
			break;
		case 30:				/* Elemental Brand */
			if (p_ptr->au < 1000) {
				msgf("You don't have enough gold.");
				return(FALSE);
			}
			brand_weapon(0);
			p_ptr->au = MIN(p_ptr->au-1000, 0);  /* Costs 1000 gold */
			break;
		case 31:				/* Nature's Wrath */
			(void)dispel_monsters(plev * 4);
			(void)earthquake(px, py, 20 + (plev / 2));
			(void)project(0, 1 + plev / 12, px, py,
						  100 + plev, GF_DISINTEGRATE,
						  PROJECT_KILL | PROJECT_ITEM);
			(void)project(0, 5, p_ptr->px, p_ptr->py, 0, GF_MAKE_LAVA, PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE);
			break;
		default:
			msgf("You cast an unknown Nature spell: %d.", spell);
			message_flush();
	}

	make_noise(2);

	return TRUE;
}


static bool cast_chaos_spell(int spell)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

	int dir, i, beam;
	int plev = p_ptr->lev;

	object_type *o_ptr;
	cptr q, s;

	if (p_ptr->rp.pclass == CLASS_MAGE) beam = plev;
	else if (p_ptr->rp.pclass == CLASS_HIGH_MAGE) beam = plev + 10;
	else
		beam = plev / 2;

	switch (spell)
	{
		case 0:				/* Magic Missile */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fire_bolt_or_beam(beam - 10, GF_MISSILE, dir,
									damroll(3 + ((plev - 1) / 5), 4));
			break;
		case 1:				/* Trap / Door destruction */
			(void)destroy_doors_touch();
			break;
		case 2:				/* Flash of Light == Light Area */
			(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
			break;
		case 3:				/* Touch of Confusion */
			if (!p_ptr->state.confusing)
			{
				msgf("Your hands start glowing.");
				p_ptr->state.confusing = TRUE;
				p_ptr->redraw |= (PR_STATUS);
			}
			break;
		case 4:				/* Manaburst */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fire_ball(GF_MISSILE, dir,
							(damroll(3, 5) + plev +
							 (plev / (((p_ptr->rp.pclass == CLASS_MAGE) ||
									   (p_ptr->rp.pclass ==
										CLASS_HIGH_MAGE)) ? 2 : 4))),
							((plev < 30) ? 2 : 3));
			/* Shouldn't actually use GF_MANA, as it will destroy all
			 * items on the floor */
			break;
		case 5:				/* Polymorph Other */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)poly_monster(dir);
			break;
		case 6:				/* Fist of Force ("Fist of Fun") */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fire_ball(GF_DISINTEGRATE, dir,
							damroll(8 + ((plev - 5) / 4), 6), 0);
			break;
		case 7:				/* Teleport Self */
			teleport_player(plev * 5);
			break;
		case 8:				/* Wonder */
		{
			/*
			 * This spell should become more useful (more
			 * controlled) as the player gains experience levels.
			 * Thus, add 1/5 of the player's level to the die roll.
			 * This eliminates the worst effects later on, while
			 * keeping the results quite random.  It also allows
			 * some potent effects only at high level.
			 */
			int die = randint1(100) + plev / 5;

			if (die < 26)
				chg_virtue(V_CHANCE, 1);

			if (!get_aim_dir(&dir)) return FALSE;
			if (die > 100)
				msgf("You feel a surge of power!");
			if (die < 8) (void)clone_monster(dir);
			else if (die < 14) (void)speed_monster(dir);
			else if (die < 26) (void)heal_monster(dir);
			else if (die < 31) (void)poly_monster(dir);
			else if (die < 36)
				(void)fire_bolt_or_beam(beam - 10, GF_MISSILE, dir,
										damroll(3 + ((plev - 1) / 5), 4));
			else if (die < 41) (void)confuse_monster(dir, plev);
			else if (die < 46) (void)fire_ball(GF_POIS, dir, 20 + (plev / 2),
											   3);
			else if (die < 51) (void)lite_line(dir, damroll(6, 8));
			else if (die < 56)
				(void)fire_bolt_or_beam(beam - 10, GF_ELEC, dir,
										damroll(3 + ((plev - 5) / 4), 8));
			else if (die < 61)
				(void)fire_bolt_or_beam(beam - 10, GF_COLD, dir,
										damroll(5 + ((plev - 5) / 4), 8));
			else if (die < 66)
				(void)fire_bolt_or_beam(beam, GF_ACID, dir,
										damroll(6 + ((plev - 5) / 4), 8));
			else if (die < 71)
				(void)fire_bolt_or_beam(beam, GF_FIRE, dir,
										damroll(8 + ((plev - 5) / 4), 8));
			else if (die < 76) (void)drain_life(dir, 75);
			else if (die < 81) (void)fire_ball(GF_ELEC, dir, 30 + plev / 2, 2);
			else if (die < 86) (void)fire_ball(GF_ACID, dir, 40 + plev, 2);
			else if (die < 91) (void)fire_ball(GF_ICE, dir, 70 + plev, 3);
			else if (die < 96) (void)fire_ball(GF_FIRE, dir, 80 + plev, 3);
			else if (die < 101) (void)drain_life(dir, 100 + plev);
			else if (die < 104)
			{
				(void)earthquake(px, py, 12);
			}
			else if (die < 106)
			{
				(void)destroy_area(px, py, 15);
			}
			else if (die < 108)
			{
				(void)genocide(TRUE);
			}
			else if (die < 110) (void)dispel_monsters(120);
			else				/* RARE */
			{
				(void)dispel_monsters(150);
				(void)slow_monsters();
				(void)sleep_monsters();
				(void)hp_player(300);
			}
			break;
		}
		case 9:				/* Fire Bolt */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fire_bolt_or_beam(beam, GF_FIRE, dir,
									damroll(8 + ((plev - 5) / 4), 8));
			break;
		case 10:				/* Field of Chaos */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fire_ball(GF_CHAOS, dir, 5 + plev/2, 2 + plev/12);
			break;
		case 11:				/* Sonic Boom */
			msgf("BOOM! Shake the room!");
			(void)project(0, plev / 10 + 2, px, py,
						  45 + plev, GF_SOUND, PROJECT_KILL | PROJECT_ITEM);
			break;
		case 12:				/* Beam of Energy -- always beam in 2.0.7 or later */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fire_beam(GF_MANA, dir, damroll(11 + ((plev - 5) / 4), 8));
			break;
		case 13:				/* Fire Ball */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fire_ball(GF_FIRE, dir, plev + 55, 2);
			break;
		case 14:				/* Teleport Other */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fire_beam(GF_AWAY_ALL, dir, plev);
			break;
		case 15:				/* Word of Destruction */
			(void)destroy_area(px, py, 15);
			break;
		case 16:				/* Chain Lightning */
			for (dir = 0; dir <= 9; dir++)
				(void)fire_beam(GF_ELEC, dir, damroll(5 + (plev / 10), 8));
			break;
		case 17:				/* Chaos Bolt */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fire_bolt_or_beam(beam, GF_CHAOS, dir,
									damroll(10 + ((plev - 5) / 4), 8));
			break;
		case 18:				/* Energize Item == Charging */
			return recharge(20+(3*plev)/2);
		case 19:				/* Channel Energy */
			i = randint1(p_ptr->chp/2)+randint1(20);  /* drains up to 50% of current health + 1d20 */
			if (p_ptr->tim.invuln) {
				msgf("The spell has no effect.");
			} else {
				take_hit(i, "channeling chaotic energy");
				if (i/2 + p_ptr->csp >= p_ptr->msp || one_in_(5 + plev/5)) { /* overload */
					msgf("Raw energy overwhelms you!");
					(void)fire_ball(GF_MANA, 0, 50+i, 3);
				} else {
					msgf("Raw energy flows through you.");
					p_ptr->csp = ((p_ptr->csp+i > p_ptr->msp) ? p_ptr->msp : p_ptr->csp+i);  /* use maximum */
					p_ptr->update |= (PU_MANA);  /* redraw */
				}
			}
			break;
		case 20:				/* Alter Reality */
			alter_reality();
			break;
		case 21:				/* Polymorph Self */
			do_poly_self();
			break;
		case 22:				/* Summon monster, demon */
		{
			bool group = one_in_(52-plev);
	        if(!player_summon(PSUM_DEMON, 2*plev, group, 350, 0)) msgf("Nothing answers your call.");
			break;
		}
		case 23:				/* Breathe Logrus */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fire_ball(GF_CHAOS, dir, p_ptr->chp, 2);
			if (!player_has_mut(MUT_CHAOS_PATRON) && one_in_(10))
				(void)gain_mutation(MUT_CHAOS_PATRON);
			break;
		case 24:				/* Consume Magic */
		{
			int lev;
			item_tester_hook = item_tester_hook_recharge;

			/* Get an item */
			q = "Drain which item? ";
			s = "You have nothing to drain.";
			o_ptr = get_item(q, s, (USE_INVEN | USE_FLOOR));
			/* Not a valid item */
			if (!o_ptr) return (FALSE);

			lev = get_object_level(o_ptr);

			if (o_ptr->tval == TV_ROD)
			{
				if (o_ptr->pval > 0)
				{
					msgf("You can't absorb energy from a discharged rod.");
				}
				else
				{
					p_ptr->csp += 2 * lev;
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
					msgf("There's no energy there to absorb!");
				}
				o_ptr->info |= OB_EMPTY;
			}
				if (p_ptr->csp > p_ptr->msp)
			{
				p_ptr->csp = p_ptr->msp;
			}
				/* Notice changes */
			notice_inven();
			break;
		}
		case 25: 				/* Polymorph Item */
			if(!polymorph_item())
				return(FALSE);
			break;
		case 26:				/* Meteor Swarm */
		{
			int x, y;
			int b = rand_range(10, 20);
			for (i = 0; i < b; i++)
			{
				int count = 0;

				while (count < 1000)
				{
					count++;

					x = px - 5 + randint1(10);
					y = py - 5 + randint1(10);

					/* paranoia */
					if (!in_boundsp(x, y)) continue;

					/* keep going if not in LOS */
					if (!player_has_los_grid(parea(x, y))) continue;

					/* if close enough - exit */
					if (distance(px, py, x, y) < 6) break;
				}

				if (count >= 1000) break;

				(void)project(0, 2, x, y, (plev * 3) / 2, GF_METEOR,
							  PROJECT_KILL | PROJECT_JUMP | PROJECT_ITEM);
			}
		}
			break;
		case 27:				/* Call Chaos */
			call_chaos();
			if (!player_has_mut(MUT_CHAOS_PATRON))
				(void)gain_mutation(MUT_CHAOS_PATRON);
			break;
		case 28:				/* Magic Rocket */
			if (!get_aim_dir(&dir)) return FALSE;

			msgf("You launch a rocket!");
			(void)fire_ball(GF_ROCKET, dir, 120 + plev, 2);
			break;
		case 29:				/* Mutation */
			(void)gain_mutation(0);
			if (!player_has_mut(MUT_CHAOS_PATRON))
				(void)gain_mutation(MUT_CHAOS_PATRON);
			break;
		case 30:				/* Mana Storm */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fire_ball(GF_MANA, dir, 300 + (plev * 2), 4);
			break;
		case 31:				/* Call the Void */
			call_the_();
			if (!player_has_mut(MUT_CHAOS_PATRON))
				(void)gain_mutation(MUT_CHAOS_PATRON);
			break;
		default:
			msgf("You cast an unknown Chaos spell: %d.", spell);
			message_flush();
	}

	make_noise(2);

	return TRUE;
}


static bool cast_death_spell(int spell)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

	int dir;
	int beam;
	int plev = p_ptr->lev;
	int dummy = 0;
	int i;

	if (p_ptr->rp.pclass == CLASS_MAGE) beam = plev;
	else if (p_ptr->rp.pclass == CLASS_HIGH_MAGE) beam = plev + 10;
	else
		beam = plev / 2;

	switch (spell)
	{
		case 0:				/* Detect Evil & Unlife */
			(void)detect_monsters_nonliving();
			(void)detect_monsters_evil();
			break;
		case 1:				/* Malediction */
			if (!get_aim_dir(&dir)) return FALSE;
			/* A radius-0 ball may (1) be aimed at objects etc.,
			 * and will affect them; (2) may be aimed at ANY
			 * visible monster, unlike a 'bolt' which must travel
			 * to the monster. */

			(void)fire_ball(GF_HELL_FIRE, dir,
							damroll(3 + ((plev - 1) / 5), 3), 0);

			if (one_in_(5))
			{
				/* Special effect first */
				dummy = randint1(1000);
				if (dummy == 666)
					(void)fire_bolt(GF_DEATH_RAY, dir, plev * 50);
				else if (dummy < 500)
					(void)fire_bolt(GF_TURN_ALL, dir, plev);
				else if (dummy < 800)
					(void)fire_bolt(GF_OLD_CONF, dir, plev);
				else
					(void)fire_bolt(GF_STUN, dir, plev);
			}
			break;
		case 2:				/* Horrify */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fear_monster(dir, plev);
			(void)stun_monster(dir, plev);
			break;
		case 3:				/* Stinking Cloud */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fire_ball(GF_POIS, dir, 10 + (plev / 2), 2);
			break;
		case 4:				/* Black Sleep */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)sleep_monster(dir);
			break;
		case 5:				/* Resist Poison */
			(void)inc_oppose_pois(rand_range(20, 40));
			(void)inc_poisoned(-p_ptr->tim.poisoned/2);
			break;
		case 6:				/* Enslave the Undead */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)control_one_undead(dir, plev);
			break;
		case 7:				/* Terror */
			(void)turn_monsters(20 + plev);
			break;
		case 8:				/* Orb of Entropy */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fire_ball(GF_OLD_DRAIN, dir,
							(damroll(3, 6) + plev +
							 (plev / (((p_ptr->rp.pclass == CLASS_MAGE) ||
									   (p_ptr->rp.pclass ==
										CLASS_HIGH_MAGE)) ? 2 : 4))),
							((plev < 30) ? 2 : 3));
			break;
		case 9:				/* Shadow Gate */
			teleport_player(plev * 5);
			break;
		case 10:			/* Summon lesser undead */
	        if(!player_summon(PSUM_UNDEAD, 2*plev, TRUE, 100, 0)) msgf("Nothing answers your call.");
			break;
		case 11:				/* Vampiric Drain */
			if (!get_aim_dir(&dir)) return FALSE;

			dummy = plev + damroll(MAX(1, plev/10),plev);	/* Dmg */
			if (drain_gain_life(dir, dummy))
			{
				/*
				 * Hack - this only happens when monster is seen to
				 * be hit.
				 */
				chg_virtue(V_SACRIFICE, -1);
				chg_virtue(V_VITALITY, -1);

				/* Gain nutritional sustenance: 150/hp drained */
				/* A Food ration gives 5000 food points (by contrast) */
				/* Don't ever get more than "Full" this way */
				/* But if we ARE Gorged, it won't cure us */
				dummy = p_ptr->food + MIN(5000, 100 * dummy);
				if (p_ptr->food < PY_FOOD_MAX)	/* Not gorged already */
					(void)set_food(dummy >=
								   PY_FOOD_MAX ? PY_FOOD_MAX - 1 : dummy);
			}
			break;
		case 12:				/* Identify */
			if (!ident_spell())
				return (FALSE);
			break;
		case 13:				/* Dispel Life */
			(void)dispel_living(plev * 3 / 2);
			break;
		case 14:				/* Genocide */
			(void)genocide(TRUE);
			break;
		case 15:				/* Restore Life */
			(void)do_res_stat(A_STR);
			(void)do_res_stat(A_INT);
			(void)do_res_stat(A_WIS);
			(void)do_res_stat(A_DEX);
			(void)do_res_stat(A_CON);
			(void)do_res_stat(A_CHR);
			(void)restore_level();
			break;
		case 16:				/* Berserk */
			(void)inc_shero(rand_range(25, 50));
			(void)hp_player(30);
			(void)clear_afraid();
			break;
		case 17: 				/* Drain Life */
			(void)drain_life(dir, 75);
			break;
		case 18:				/* Nether Bolt */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fire_bolt_or_beam(beam, GF_NETHER, dir,
									damroll(6 + ((plev - 5) / 4), 8));
			break;
		case 19:				/* Battle Frenzy */
			(void)inc_shero(rand_range(25, 50));
			(void)hp_player(30);
			(void)clear_afraid();
			(void)inc_fast(rand_range(plev / 2, 20 + plev));
			break;
		case 20:				/* Consume Life */
			if (!get_aim_dir(&dir)) return FALSE;

			chg_virtue(V_SACRIFICE, -1);
			chg_virtue(V_VITALITY, -1);

			for (dummy = 0; dummy < 3; dummy++)
			{
				(void)drain_gain_life(dir, 150 + 2*plev);
			}
			break;
		case 21:				/* Darkness Storm */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fire_ball(GF_DARK, dir, 120, 4);
			break;
		case 22:				/* Summon Greater Undead */
	        if(!player_summon(PSUM_HI_UNDEAD, 50+randint1(plev), FALSE, 350, 0)) msgf("Nothing answers your call.");
			break;

		case 23:				/* Mass Genocide */
			(void)mass_genocide(TRUE);
			break;
		case 24:				/* Death Ray */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)death_ray(dir, plev);
			break;
		case 25:				/* Esoteria */
			if (!identify_fully())
				return FALSE;
			break;
		case 26: 				/* Reveal Secrets */
			(void)wiz_lite();
			break;
		case 27:				/* Oblivion */
			(void)dispel_living(plev * 4);
			break;
		case 28:				/* Blast Sanity */
			(void)mindblast_monsters(plev * 3);
			(void)turn_monsters(plev * 4);
			(void)banish_monsters(plev * 4);
			break;
		case 29:				/* Hellfire */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fire_ball(GF_HELL_FIRE, dir, 666, 3);
			take_hit(rand_range(50, 100), "the strain of casting Hellfire");
			break;
		case 30:				/* Omnicide */
			p_ptr->csp -= 100;

			/* Display doesn't show mana cost (100)
			 * as deleted until the spell has finished. This gives a
			 * false impression of how high your mana is climbing.
			 * Therefore, 'deduct' the cost temporarily before entering the
			 * loop, then add it back at the end so that the rest of the
			 * program can deduct it properly
			 */
			for (i = 1; i < m_max; i++)
			{
				monster_type *m_ptr = &m_list[i];
				monster_race *r_ptr = &r_info[m_ptr->r_idx];

				/* Paranoia -- Skip dead monsters */
				if (!m_ptr->r_idx) continue;

				/* Hack -- Skip Unique Monsters */
				if (FLAG(r_ptr, RF_UNIQUE)) continue;

				/* Hack -- Skip Quest Monsters */
				if (FLAG(r_ptr, RF_QUESTOR)) continue;

				/* Notice changes in view */
				if (FLAG(r_ptr, RF_LITE_1) || FLAG(r_ptr, RF_LITE_2))
				{
					/* Update some things */
					p_ptr->update |= (PU_MON_LITE);
				}

				/* Delete the monster */
				delete_monster_idx(i);

				/* Take damage */
				take_hit(randint1(4), "the strain of casting Omnicide");

				/* Absorb power of dead soul - up to twice max. mana */
				if (p_ptr->csp < (p_ptr->msp * 2))
					p_ptr->csp++;

				/* Visual feedback */
				move_cursor_relative(px, py);

				/* Redraw */
				p_ptr->redraw |= (PR_HP | PR_MANA);

				/* Window stuff */
				p_ptr->window |= (PW_PLAYER);
				p_ptr->window |= (PW_SPELL);

				/* Handle */
				handle_stuff();

				/* Fresh */
				Term_fresh();

				/* Delay */
				Term_xtra(TERM_XTRA_DELAY,
						  delay_factor * delay_factor * delay_factor);
			}

			/* Restore, ready to be deducted properly */
			p_ptr->csp += 100;

			break;
		case 31:				/* Wraithform */
			(void)inc_wraith_form(rand_range(3, 2*plev / 3));
			break;
		default:
			msgf("You cast an unknown Death spell: %d.", spell);
			message_flush();
	}

	make_noise(2);

	return TRUE;
}

/* "success" is not used now. */
static bool cast_conj_spell(int spell)
{
	int dir;
	int beam;
	int plev = p_ptr->lev;
	bool sum_fail = TRUE;

	if (p_ptr->rp.pclass == CLASS_MAGE) beam = plev;
	else if (p_ptr->rp.pclass == CLASS_HIGH_MAGE) beam = plev + 10;
	else
		beam = plev / 2;

	switch (spell)
	{
		case 0:				/* Phase Door */
				teleport_player(15);
			break;
		case 1:				/* Magic Missile */
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_bolt_or_beam(beam - 10, GF_MISSILE, dir,
									damroll(3 + ((plev - 1) / 5), 3));
			break;
		case 2: 			/* Summon Lesser Animal */
	        sum_fail = player_summon(PSUM_ANIMAL_LOW, 2*plev, FALSE, 150, PSUM_FORCE_SUCCESS);
			break;
		case 3:				/* Light */
			(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
			break;
		case 4:  			/* Heal Minor Wounds */
			(void)hp_player(6+damroll(1,8));
			break;
		case 5:				/* Summon Phantom */
	        sum_fail = player_summon(PSUM_PHANTOM, 2*plev, FALSE, 300, PSUM_FORCE_SUCCESS);
			break;
		case 6:				/* Magic Rope */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)slow_monster(dir);
			break;
		case 7:				/* Stinking Cloud */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fire_ball(GF_POIS, dir, 10 + (plev / 2), 2);
			break;
		case 8:				/* Glitterdust */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fire_ball(GF_CONFUSION, dir, 5 + (plev / 10), 2);
			break;
		case 9:				/* Teleport Self */
			teleport_player(MAX(250, plev*6));
			break;
		case 10:			/* Web */
			msgf ("Sticky strands appear everywhere.");
			(void)slow_monsters();
			break;
		case 11:			/* Summon Animals */
	        sum_fail = player_summon(PSUM_ANIMAL, 2*plev, TRUE, 150, PSUM_FORCE_SUCCESS);
			break;
		case 12:				/* Dimension Door */
			msgf("You open a dimensional gate. Choose a destination.");
			if (!dimension_door())
				return (FALSE);
			break;
		case 13:				/* Teleport Away */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fire_beam(GF_AWAY_ALL, dir, plev);
			break;
		case 14:				/* Summon Elemental */
	        sum_fail = player_summon(PSUM_ELEMENTAL, plev+randint1(plev), FALSE, 150, 0);
			break;
		case 15:				/* Word of Recall */
			word_of_recall();
			break;
		case 16:				/* Acid Arrow */
			if (!get_aim_dir(&dir)) return FALSE;
				(void)fire_bolt_or_beam(beam - 10, GF_ACID, dir,
										damroll(6 + ((plev - 1) / 5), 6));
			break;
		case 17:				/* Flame Arrow */
			if (!get_aim_dir(&dir)) return FALSE;
				(void)fire_bolt_or_beam(beam - 10, GF_FIRE, dir,
										damroll(9 + ((plev - 1) / 5), 6));
			break;
		case 18:				/* Wall of Force */
			(void)inc_shield(rand_range(30, 50));
			break;
		case 19:				/* Summon Hydra */
	        sum_fail = player_summon(PSUM_HYDRA, 2*plev, FALSE, 150, 0);
			break;
		case 20:				/* Summon Dragon */
	        sum_fail = player_summon(PSUM_DRAGON, 2*plev, FALSE, 350, 0);
			break;
		case 21:				/* Banishment */
			(void)banish_monsters(plev * 4);
			break;
		case 22:				/* Heal */
			(void)hp_player(150+damroll(15,8));
			(void)clear_stun();
			(void)clear_cut();
			(void)clear_poisoned();
			break;
		case 23:				/* Summon Radiance */
			(void)wiz_lite();
			break;
		case 24:				/* Portal */
			(void)dimension_door2();
			break;
		case 25:				/* Cloudkill */
			(void)scatter_ball(5+(plev/12), GF_POIS, 40+plev, 3);
			break;
		case 26:				/* Plane Shift */
			(void)alter_reality();
			break;
		case 27:				/* Etherealness */
			(void)inc_etherealness(rand_range(20,40));
			break;
		case 28:				/* Summon Demon */
	        sum_fail = player_summon(PSUM_DEMON, plev+randint1(plev), one_in_(55-plev), 350, 0);
			break;
		case 29:				/* Planar Rift */
			(void)dispel_monsters(50+plev * 3);
			break;
		case 30:				/* Summon Undead */
	        sum_fail = player_summon(PSUM_HI_UNDEAD, plev+randint1(plev), FALSE, 350, 0);
			break;
		case 31:				/* Greater Summoning */
			switch(randint1(4))
			{
				case 1:
					sum_fail = player_summon(PSUM_HI_UNDEAD, plev+randint1(plev), FALSE, 450, 0) ||
 							   player_summon(PSUM_HI_UNDEAD, plev+randint1(plev), FALSE, 450, 0);
					break;
				case 2:
					sum_fail = player_summon(PSUM_HI_DRAGON, 45+plev, FALSE, 450, 0);
					break;
				case 3:
			        sum_fail = player_summon(PSUM_DRAGON, 50+plev, TRUE, 600, PSUM_FORCE_SUCCESS);
					break;
				case 4:
					sum_fail = player_summon(PSUM_ANGEL, 50+plev, TRUE, 600, PSUM_FORCE_SUCCESS);
					break;
			}
			break;
		default:
			msgf("You cast an unknown Conjuration spell: %d.", spell);
			message_flush();
	}

	if (!sum_fail)
		msgf("Nothing answers your summons.");

	make_noise(2);

	return TRUE;
}


static bool cast_arcane_spell(int spell)
{
	int dir;
	int beam;
	int plev = p_ptr->lev;
	int dummy = 0;

	if (p_ptr->rp.pclass == CLASS_MAGE) beam = plev;
	else if (p_ptr->rp.pclass == CLASS_HIGH_MAGE) beam = plev + 10;
	else
		beam = plev / 2;

	switch (spell)
	{
		case 0:				/* Zap */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fire_bolt_or_beam(beam - 10, GF_PLASMA, dir,
									damroll(3 + ((plev - 1) / 5), 3));
			break;
		case 1:				/* Detect Monsters */
			(void)detect_monsters_normal();
			break;
		case 2:				/* Blink */
			teleport_player(10);
			break;
		case 3:				/* Light Area */
			(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
			break;
		case 4:				/* Fuel */
			phlogiston();
			break;
		case 5:				/* Detect Traps */
			(void)detect_traps(TRUE);
			break;
		case 6:				/* Detect Invisibility */
			(void)detect_monsters_invis();
			break;
		case 7:				/* Cure Poison and Fear */
			(void)clear_poisoned();
			(void)clear_afraid();
			break;
		case 8:				/* Cure Medium Wounds */
			(void)hp_player(12+damroll(2, 8));
			(void)inc_cut(-50);
			break;
		case 9:				/* Resist Cold and Fire */
			(void)inc_oppose_cold(rand_range(20, 40));
			(void)inc_oppose_fire(rand_range(20, 40));
			break;
		case 10:			/* Lore */
			if(!psychometry())
				return	(FALSE);
			break;
		case 11:			/* Dispel Traps */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)disarm_trap(dir);
			break;
		case 12:			/* See Invisible */
			(void)inc_tim_invis(rand_range(24, 48));
			break;
		case 13:			/* Resist Blindness */
			(void)inc_oppose_blind(rand_range(20, 40));
			break;
		case 14:			/* Stone to Mud */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)wall_to_mud(dir);
			break;
		case 15:			/* Ray of Light */
			if (!get_aim_dir(&dir)) return FALSE;

			msgf("A line of light appears.");
			(void)lite_line(dir, damroll(6, 8));
			break;
		case 16:			/* Satisfy Hunger */
			(void)set_food(PY_FOOD_MAX - 1);
			break;
		case 17:			/* Teleport */
			teleport_player(plev * 5);
			break;
		case 18:			/* Resist Elements */
			(void)inc_oppose_acid(rand_range(20, 40));
			(void)inc_oppose_cold(rand_range(20, 40));
			(void)inc_oppose_fire(rand_range(20, 40));
			(void)inc_oppose_elec(rand_range(20, 40));
			break;
		case 19:			/* Resist Poison */
			(void)inc_oppose_pois(rand_range(20, 40));
			break;
		case 20:			/* Magic Mapping */
			map_area();
			break;
		case 21:			/* Detection */
			(void)detect_all();
			break;
		case 22:			/* Teleport Away */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fire_beam(GF_AWAY_ALL, dir, plev);
			break;
		case 23:			/* Identify */
			if (!ident_spell())
				return (FALSE);
			break;
		case 24:			/* Cure Mortal Wounds */
			(void)hp_player(48 + damroll(8,8));
			(void)clear_stun();
			(void)clear_cut();
			(void)clear_poisoned();
			break;
		case 25:				/* Teleport Level */
			(void)teleport_player_level();
			break;
		case 26:				/* Elemental Ball */
			if (!get_aim_dir(&dir)) return FALSE;

			switch (randint1(4))
			{
				case 1: dummy = GF_FIRE;
					break;
				case 2: dummy = GF_ELEC;
					break;
				case 3: dummy = GF_COLD;
					break;
				default: dummy = GF_ACID;
					break;
			}
			(void)fire_ball(dummy, dir, 75 + (plev), 2);
			break;
		case 27:				/* Enchant Armor */
			if (p_ptr->au < 300) {
				msgf("You don't have enough gold.");
				return(FALSE);
			}
		    if (!enchant_spell(0, 0, randint1(3))) return(FALSE);
			p_ptr->au = MIN(p_ptr->au-300, 0);  /* Costs 300 gold */
			break;
		case 28:				/* Recharging */
			return recharge(plev * 4);
		case 29:				/* Self-Knowledge */
			self_knowledge();
			break;
		case 30:				/* Word of Recall */
			word_of_recall();
			break;
		case 31:				/* Clairvoyance */
			wiz_lite();
			if (!(FLAG(p_ptr, TR_TELEPATHY)))
			{
				(void)inc_tim_esp(rand_range(25, 55));
			}
			break;
		default:
			msgf("You cast an unknown Arcane spell: %d.", spell);
			message_flush();
	}

	make_noise(2);

	return TRUE;
}


/*
 * Cast a spell
 */
void do_cmd_cast(void)
{
	int sval, spell, realm;
	int chance, smana;
	int increment = 0;
	int use_realm;
	bool cast;

	const cptr prayer =
		((mp_ptr->spell_book == TV_LIFE_BOOK) ? "prayer" : "spell");

	object_type *o_ptr;

	const magic_type *s_ptr;

	cptr q, s;

	/* Require spell ability */
	if (!p_ptr->spell.r[0].realm)
	{
		msgf("You cannot cast spells!");
		return;
	}

	/* Require lite */
	if (p_ptr->tim.blind || no_lite())
	{
		msgf("You cannot see!");
		return;
	}

	/* Not when confused */
	if (p_ptr->tim.confused)
	{
		msgf("You are too confused!");
		return;
	}

	/* Restrict choices to spell books */
	item_tester_tval = mp_ptr->spell_book;

	/* Get an item */
	q = "Use which book? ";
	s = "You have no spell books!";

	o_ptr = get_item(q, s, (USE_INVEN | USE_FLOOR));

	/* Not a valid item */
	if (!o_ptr) return;

	/* Access the item's sval */
	sval = o_ptr->sval;

	if (o_ptr->tval == REALM2_BOOK) increment = 32;


	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();

	realm = p_ptr->spell.r[increment / 32].realm;

	/* Ask for a spell */
	if (!get_spell
		(&spell, ((mp_ptr->spell_book == TV_LIFE_BOOK) ? "recite" : "cast"),
		 sval, TRUE, (bool)(increment ? TRUE : FALSE)))
	{
		if (spell == -2)
			msgf("You don't know any %ss in that book.", prayer);
		return;
	}


	/* Access the spell */
	use_realm = p_ptr->spell.r[increment / 32].realm;

	s_ptr = &mp_ptr->info[use_realm - 1][spell];

	/* Get mana cost */
	smana = spell_mana(spell, use_realm - 1);

	/* Verify "dangerous" spells */
	if (smana > p_ptr->csp)
	{
		/* Warning */
		msgf("You do not have enough mana to %s this %s.",
				   ((mp_ptr->spell_book == TV_LIFE_BOOK) ? "recite" : "cast"),
				   prayer);

		/* Verify */
		if (!get_check("Attempt it anyway? ")) return;
	}


	/* Spell failure chance */
	chance = spell_chance(spell, use_realm - 1);

	/* Failed spell */
	if (randint0(100) < chance)
	{
		if (flush_failure) flush();

		msgf("You failed to get the %s off!", prayer);
		sound(SOUND_FAIL);

		if ((randint1(100) < chance) && (mp_ptr->spell_book == TV_LIFE_BOOK))
			chg_virtue(V_FAITH, -1);
		else if (randint1(100) < chance)
			chg_virtue(V_KNOWLEDGE, -1);


		else if ((o_ptr->tval == TV_CHAOS_BOOK) && (randint1(100) < spell))
		{
			msgf("You produce a chaotic effect!");
			wild_magic(spell);
		}
		else if ((o_ptr->tval == TV_DEATH_BOOK) && (randint1(100) < spell))
		{
			if ((sval == 3) && one_in_(2))
			{
				msgf("Your sanity is shaken by reading the Necronomicon!");

				/* Mind blast */
				if (!player_save(100))
				{
					if (!(FLAG(p_ptr, TR_RES_CONF)) && !p_ptr->tim.oppose_conf)
					{
						(void)inc_confused(rand_range(4, 8));
					}
					if (!(FLAG(p_ptr, TR_RES_CHAOS)) && !p_ptr->tim.oppose_conf && one_in_(3))
					{
						(void)inc_image(rand_range(150, 400));
					}
				}

				/* Lose int & wis */
				else if (!player_save(100))
				{
					(void)do_dec_stat(A_INT);
					(void)do_dec_stat(A_WIS);
				}
			}
			else
			{
				msgf("It hurts!");
				take_hit(damroll(o_ptr->sval + 1, 6), "a miscast Death spell");
				if ((spell > 15) && one_in_(6) &&
					 !(FLAG(p_ptr, TR_HOLD_LIFE)))
					lose_exp(spell * 250);
			}
		}
	}

	/* Process spell */
	else
	{
		if ((randint1(100) < chance) && (chance < 50))
		{
			if (mp_ptr->spell_book == TV_LIFE_BOOK)
				chg_virtue(V_FAITH, 1);
			else
				chg_virtue(V_KNOWLEDGE, 1);
		}

		/* Spells. */
		switch (realm)
		{
			case REALM_LIFE:	/* * LIFE * */
				cast = cast_life_spell(spell);
				break;
			case REALM_SORCERY:	/* * SORCERY * */
				cast = cast_sorcery_spell(spell);
				break;
			case REALM_NATURE:	/* * NATURE * */
				cast = cast_nature_spell(spell);
				break;
			case REALM_CHAOS:	/* * CHAOS * */
				cast = cast_chaos_spell(spell);
				break;
			case REALM_DEATH:	/* * DEATH * */
				cast = cast_death_spell(spell);
				break;
			case REALM_CONJ:	/* * CONJURATION * */
				cast = cast_conj_spell(spell);
				break;
			case REALM_ARCANE:	/* * ARCANE * */
				cast = cast_arcane_spell(spell);
				break;
			default:
				cast = FALSE;
				msgf("You cast a spell from an unknown realm: realm %d, spell %d.",
					 realm, spell);
				message_flush();
		}

		/* Canceled spells cost neither a turn nor mana */
		if (!cast) return;

		/* A spell was cast */
		if (!(p_ptr->spell.r[increment / 32].worked & (1L << spell)))
		{
			/* Experience: 5, 20, 45, or 80 * spell level */
			int book = 1 + (spell / 8);
			int exp = 5 * book * book * s_ptr->slevel;

			/* The spell worked */
			if (realm == p_ptr->spell.r[0].realm)
			{
				p_ptr->spell.r[0].worked |= (1L << spell);
			}
			else
			{
				p_ptr->spell.r[1].worked |= (1L << spell);
			}

			/* Gain experience */
			gain_exp(exp);

			if (mp_ptr->spell_book == TV_LIFE_BOOK)
				chg_virtue(V_FAITH, 1);
			else
				chg_virtue(V_KNOWLEDGE, 1);
		}
	}

	/* Take a turn */
	p_ptr->state.energy_use = 100;

	/* Sufficient mana */
	if (smana <= p_ptr->csp)
	{
		/* Use some mana */
		p_ptr->csp -= smana;
	}

	/* Over-exert the player */
	else
	{
		int oops = smana - p_ptr->csp;

		/* No mana left */
		p_ptr->csp = 0;
		p_ptr->csp_frac = 0;

		/* Message */
		msgf("You faint from the effort!");

		/* Hack -- Bypass free action */
		(void)inc_paralyzed(randint1(5 * oops + 1));

		if (mp_ptr->spell_book == TV_LIFE_BOOK)
			chg_virtue(V_FAITH, -10);
		else
			chg_virtue(V_KNOWLEDGE, -10);

		/* Damage CON (possibly permanently) */
		if (one_in_(2))
		{
			bool perm = one_in_(4);

			/* Message */
			msgf("You have damaged your health!");

			/* Reduce constitution */
			(void)dec_stat(A_CON, rand_range(15, 25), perm);
		}
	}

	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);
	p_ptr->window |= (PW_SPELL);
}


/*
 * Pray a prayer -- Unused in Zangband
 */
void do_cmd_pray(void)
{
	msgf("Praying is not used in %s. Use magic spell casting instead.",
			   VERSION_NAME);
}

/* Forward declare */
extern menu_type pet_menu[PET_CHOICE_MAX + 1];


/*
 * Dismiss some pets
 */
static bool cmd_pets_dismiss(int dummy)
{
	int dismissed = 0;

	monster_type *m_ptr;

	int pet_ctr;
	bool pets = FALSE, all_pets = FALSE;

	/* Ignore parameter */
	(void) dummy;

	if (get_check("Dismiss all pets? ")) all_pets = TRUE;

	/* Process the monsters (backwards) */
	for (pet_ctr = m_max - 1; pet_ctr >= 1; pet_ctr--)
	{
		/* Access the monster */
		m_ptr = &m_list[pet_ctr];

		if (is_pet(m_ptr))
		{
			bool delete_this = FALSE;

			if (all_pets)
				delete_this = TRUE;
			else
			{
				if (get_check("Dismiss %v? ", MONSTER_FMT(m_ptr, 0x80)))
					delete_this = TRUE;
			}

			if (delete_this)
			{
				/* Notice changes in view */
				if (r_info[m_ptr->r_idx].flags[6] &
					(RF6_LITE_1 | RF6_LITE_2))
				{
					/* Update some things */
					p_ptr->update |= (PU_MON_LITE);
				}

				delete_monster_idx(pet_ctr);
				dismissed++;
			}
		}
	}

	msgf("You have dismissed %d pet%s.", dismissed,
			   (dismissed == 1 ? "" : "s"));

	/* Calculate pets */
	/* Process the monsters (backwards) */
	for (pet_ctr = m_max - 1; pet_ctr >= 1; pet_ctr--)
	{
		/* Access the monster */
		m_ptr = &m_list[pet_ctr];

		if (is_pet(m_ptr))
		{
			/* Is it a pet? */
			pets = TRUE;
			break;
		}
	}

	/* Can only dismiss pets if actually have some */
	if (pets)
	{
		pet_menu[PET_DISMISS].flags |= MN_ACTIVE;
	}
	else
	{
		pet_menu[PET_DISMISS].flags &= ~(MN_ACTIVE);
	}

	/* Stay at menu */
	return (FALSE);
}


static bool cmd_pets_close(int dummy)
{
	/* Hack - ignore parameter */
	(void) dummy;

	/* Change follow distance */
	p_ptr->pet_follow_distance = PET_CLOSE_DIST;

	/* Stay at menu */
	return (FALSE);
}

static bool cmd_pets_follow(int dummy)
{
	/* Hack - ignore parameter */
	(void) dummy;

	/* Change follow distance */
	p_ptr->pet_follow_distance = PET_FOLLOW_DIST;

	/* Stay at menu */
	return (FALSE);
}

static bool cmd_pets_destroy(int dummy)
{
	/* Hack - ignore parameter */
	(void) dummy;

	/* Change follow distance */
	p_ptr->pet_follow_distance = PET_DESTROY_DIST;

	/* Stay at menu */
	return (FALSE);
}

static bool cmd_pets_space(int dummy)
{
	/* Hack - ignore parameter */
	(void) dummy;

	/* Change follow distance */
	p_ptr->pet_follow_distance = PET_SPACE_DIST;

	/* Stay at menu */
	return (FALSE);
}

static bool cmd_pets_away(int dummy)
{
	/* Hack - ignore parameter */
	(void) dummy;

	/* Change follow distance */
	p_ptr->pet_follow_distance = PET_AWAY_DIST;

	/* Stay at menu */
	return (FALSE);
}

static bool cmd_pets_doors(int dummy)
{
	/* Hack - ignore parameter */
	(void) dummy;

	/* Toggle the open doors flag */
	p_ptr->pet_open_doors = !p_ptr->pet_open_doors;

	if (p_ptr->pet_open_doors)
	{
		pet_menu[PET_OPEN_DOORS].text = "pets may open doors";
	}
	else
	{
		pet_menu[PET_OPEN_DOORS].text = "pets may not open doors";
	}


	/* Stay at menu */
	return (FALSE);
}

static bool cmd_pets_items(int dummy)
{
	int pet_ctr;

	monster_type *m_ptr;

		/* Hack - ignore parameter */
	(void) dummy;


	/* Toggle pet pickup flag */
	p_ptr->pet_pickup_items = !p_ptr->pet_pickup_items;

	/* Drop objects being carried by pets */
	if (!p_ptr->pet_pickup_items)
	{
		for (pet_ctr = m_max - 1; pet_ctr >= 1; pet_ctr--)
		{
			/* Access the monster */
			m_ptr = &m_list[pet_ctr];

			if (is_pet(m_ptr))
			{
				drop_object_list(&m_ptr->hold_o_idx,
								 m_ptr->fx, m_ptr->fy);
			}
		}
	}

	if (p_ptr->pet_pickup_items)
	{
		pet_menu[PET_TAKE_ITEMS].text = "pets may pick up items";
	}
	else
	{
		pet_menu[PET_TAKE_ITEMS].text = "pets may not pick up items";
	}

	/* Stay at menu */
	return (FALSE);
}


/* The menu used to interact with pets */
menu_type pet_menu[PET_CHOICE_MAX + 1] =
{
	{"Stay close", NULL, cmd_pets_close, MN_ACTIVE | MN_SELECT},
	{"Follow me", NULL, cmd_pets_follow, MN_ACTIVE | MN_SELECT},
	{"Seek and destroy", NULL, cmd_pets_destroy, MN_ACTIVE | MN_SELECT},
	{"Give me space", NULL, cmd_pets_space, MN_ACTIVE | MN_SELECT},
	{"Stay away", NULL, cmd_pets_away, MN_ACTIVE | MN_SELECT},
	{NULL, NULL, cmd_pets_doors, MN_ACTIVE},
	{NULL, NULL, cmd_pets_items, MN_ACTIVE},
	{"Display current pets", NULL, do_cmd_knowledge_pets, MN_ACTIVE | MN_CLEAR},
	{"Dismiss pets", NULL, cmd_pets_dismiss, MN_ACTIVE | MN_CLEAR},
	MENU_END
};


/*
 * Issue a pet command
 */
void do_cmd_pet(void)
{
	bool pets = FALSE;
	int pet_ctr;
	monster_type *m_ptr;
	int pet_select = -1;

	/* Calculate pets */
	/* Process the monsters (backwards) */
	for (pet_ctr = m_max - 1; pet_ctr >= 1; pet_ctr--)
	{
		/* Access the monster */
		m_ptr = &m_list[pet_ctr];

		if (is_pet(m_ptr))
		{
			/* Is it a pet? */
			pets = TRUE;
			break;
		}
	}

	/* Can only dismiss pets if actually have some */
	if (pets)
	{
		pet_menu[PET_DISMISS].flags |= MN_ACTIVE;
	}
	else
	{
		pet_menu[PET_DISMISS].flags &= ~(MN_ACTIVE);
	}

	/* Get current option */
	if (p_ptr->pet_follow_distance == PET_CLOSE_DIST)
	{
		pet_select = PET_STAY_CLOSE;
	}

	if (p_ptr->pet_follow_distance == PET_FOLLOW_DIST)
	{
		pet_select = PET_FOLLOW_ME;
	}

	if (p_ptr->pet_follow_distance == PET_DESTROY_DIST)
	{
		pet_select = PET_SEEK_AND_DESTROY;
	}

	if (p_ptr->pet_follow_distance == PET_SPACE_DIST)
	{
		pet_select = PET_ALLOW_SPACE;
	}

	if (p_ptr->pet_follow_distance == PET_AWAY_DIST)
	{
		pet_select = PET_STAY_AWAY;
	}

	/* Change option text depending on flag */
	if (p_ptr->pet_open_doors)
	{
		pet_menu[PET_OPEN_DOORS].text = "pets may open doors";
	}
	else
	{
		pet_menu[PET_OPEN_DOORS].text = "pets may not open doors";
	}

	/* Change option text depending on flag */
	if (p_ptr->pet_pickup_items)
	{
		pet_menu[PET_TAKE_ITEMS].text = "pets may pick up items";
	}
	else
	{
		pet_menu[PET_TAKE_ITEMS].text = "pets may not pick up items";
	}

	/* Interact with menu */
	display_menu(pet_menu, pet_select, FALSE, NULL, NULL);
}
