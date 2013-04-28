/* PosBand -- A variant of Angband roguelike
 *
 * Copyright (c) 2004 Ben Harrison, Robert Ruehlmann and others
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 * 
 * NPPAngband Copyright (c) 2003-2004 Jeff Greene
 * PosBand Copyright (c) 2004-2005 Alexander Ulyanov
 */

/* mon-summ.c: summoning specific monsters */

#include "posband.h"

/*
 * Hack -- the "type" of the current "summon specific"
 */
static int summon_specific_type = 0;


/*
 * Hack -- help decide if a monster race is "okay" to summon
 */
static bool summon_specific_okay(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	bool okay = FALSE;
	int i;


	/* Hack -- no specific type specified */
	if (!summon_specific_type) return (TRUE);


	/* Check our requirements */
	switch (summon_specific_type)
	{

		case SUMMON_ANT:
		{
			okay = ((r_ptr->d_char == 'a') &&
				!(r_ptr->flags1 & (RF1_UNIQUE)));
 			break;
 		}


		case SUMMON_SPIDER:
		{
			okay = ((r_ptr->d_char == 'S') &&
			        !(r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}

		case SUMMON_HOUND:
		{
			okay = (((r_ptr->d_char == 'C') || (r_ptr->d_char == 'Z')) &&
			        !(r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}

		case SUMMON_HYDRA:
		{
			okay = ((r_ptr->d_char == 'M') &&
			        !(r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}

		case SUMMON_AINU:
		{
			okay = ((r_ptr->d_char == 'A') &&
				!(r_ptr->flags1 & (RF1_UNIQUE)));

			break;
		}

		case SUMMON_DEMON:
		{
			okay = ((r_ptr->flags3 & (RF3_DEMON)) &&
				!(r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}


		case SUMMON_UNDEAD:
		{
			okay = ((r_ptr->flags3 & (RF3_UNDEAD)) &&
				!(r_ptr->flags1 & (RF1_UNIQUE)));

			break;
		}

		case SUMMON_DRAGON:
		{
			okay = ((r_ptr->flags3 & (RF3_DRAGON)) &&
				!(r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}
		
		case SUMMON_LAWFUL:
		{
			okay = ((r_ptr->flags3 & (RF3_LAWFUL)) &&
				!(r_ptr->flags1 & (RF1_UNIQUE)));
			break; 
		}
		
		case SUMMON_HI_DEMON:
		{
			okay = (r_ptr->d_char == 'U');
			break;
		}

		case SUMMON_HI_UNDEAD:
		{
			okay = ((r_ptr->d_char == 'L') ||
			        (r_ptr->d_char == 'V') ||
			        (r_ptr->d_char == 'W'));
			break;
		}


		case SUMMON_HI_DRAGON:
		{
			okay = (r_ptr->d_char == 'D');
			break;
		}
		
		case SUMMON_UNDEAD_DRAGON:
		{
			okay = (r_ptr->flags3 & (RF3_UNDEAD)) &&
				(r_ptr->flags3 & (RF3_DRAGON));
			break;
		}

		case SUMMON_WRAITH:
		{
			okay = ((r_ptr->d_char == 'W') &&
			        (r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}
		
		case SUMMON_ENT:
		{
			okay = (r_ptr->d_char == '%');
			break;
		}

		case SUMMON_UNIQUE:
		{
			if ((r_ptr->flags1 & (RF1_UNIQUE)) != 0) okay = TRUE;
			break;
		}


		case SUMMON_KIN:
		{
			okay = ((r_ptr->d_char == summon_kin_type) &&
				!(r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}

		case SUMMON_ANIMAL:
		{
			okay = ((r_ptr->flags3 & (RF3_ANIMAL)) &&
			        !(r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}

		case SUMMON_BERTBILLTOM:
		{
			okay = ((r_ptr->d_char == 'T') &&
				(r_ptr->flags1 & (RF1_UNIQUE)) &&
				  ((strstr((r_name + r_ptr->name), "Bert")) ||
				   (strstr((r_name + r_ptr->name), "Bill")) ||
				   (strstr((r_name + r_ptr->name), "Tom" ))));
			break;
		}


		case SUMMON_THIEF:
		{
			int effect;

			/* Scan through all the blows */
			for (i = 0; i < MONSTER_BLOW_MAX; i++)
			{
				/* Extract information about the blow effect */
				effect = r_ptr->blow[i].effect;
				if (effect == RBE_EAT_GOLD) okay = TRUE;
				if (effect == RBE_EAT_ITEM) okay = TRUE;
			}
			break;
		}

		case SUMMON_AIR_ELEM:
		{
		    	/* Hack -- allow wisps of fog, air spirits,
			   (greater) air elementals and smoke elementals */
			okay = (r_idx == 660 || r_idx == 163 || r_idx == 360 ||
			        r_idx == 371 || r_idx == 658 || r_idx == 659);
			break;
		}
		
		case SUMMON_WATER_ELEM:
		{
			/* Hack -- allow water vapor, water spirits,
			   (grater) water elementals and ice elementals */
			okay = (r_idx == 217 || r_idx == 351 || r_idx == 390 ||
			        r_idx == 634 || r_idx == 635 || r_idx == 636);
			break;
		}

		case SUMMON_FIRE_ELEM:
		{
			/* Hack -- allow wisps of smoke, fire spirits,
			   (grater) fire elementals and magma elementals */
			okay = (r_idx == 220 || r_idx == 349 || r_idx == 397 ||
			        r_idx == 655 || r_idx == 656 || r_idx == 657);
			break;
		}
		
		case SUMMON_YEEK:
		{
			/* Hack -- all non-unique 'y's */
			okay = ((r_ptr->d_char == 'y') &&
				!(r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}

		case SUMMON_ORC:
		{
			/* Hack -- all non-unique 'o's */
			okay = ((r_ptr->d_char == 'o') &&
				!(r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}
		
		case SUMMON_DARK_ELF:
		{
			/* Hack -- all non-unique 'h's with 'Dark ' in the name */
			okay = ((r_ptr->d_char == 'h') &&
				!(r_ptr->flags1 & (RF1_UNIQUE)) &&
				strstr(r_name + r_ptr->name, "Dark "));
			break;
		}
			
		case SUMMON_OGRE:
		{
			/* Hack -- all non-unique 'O's */
			okay = ((r_ptr->d_char == 'O') &&
				!(r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}

		case SUMMON_TROLL:
		{
			okay = ((r_ptr->flags3 & (RF3_TROLL)) &&
				!(r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}

		case SUMMON_GOLEM:
		{
			/* Hack -- all non-unique 'g's (are there any unique golems?..) */
			okay = ((r_ptr->d_char == 'g') &&
				!(r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}

		case SUMMON_VORTEX:
		{
			/* Hack -- all non-unique 'v's (are there any unique vortices?..) */
			okay = ((r_ptr->d_char == 'v') &&
				!(r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}

		case SUMMON_ELEMENTAL:
		{
			/* Hack -- all non-unique 'E's */
			okay = ((r_ptr->d_char == 'E') &&
				!(r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}

		case SUMMON_LICH:
		{
			/* Hack -- all non-unique 'L's */
			okay = ((r_ptr->d_char == 'L') &&
				!(r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}

		case SUMMON_UNDEAD_SUMM:
		{
			/* Hack -- all non-unique summoners */
			okay = (!(r_ptr->flags1 & (RF1_UNIQUE)) &&
				(r_ptr->flags7 & (RF7_S_UNDEAD | RF7_S_HI_UNDEAD)));
			break;
		}

		case SUMMON_DEMON_SUMM:
		{
			/* Hack -- all non-unique summoners */
			okay = (!(r_ptr->flags1 & (RF1_UNIQUE)) &&
				(r_ptr->flags7 & (RF7_S_DEMON | RF7_S_HI_DEMON)));
			break;
		}

		case SUMMON_DRAGON_SUMM:
		{
			/* Hack -- all non-unique summoners */
			okay = (!(r_ptr->flags1 & (RF1_UNIQUE)) &&
				(r_ptr->flags7 & (RF7_S_DRAGON | RF7_S_HI_DRAGON)));
			break;
		}

		case SUMMON_VROCK:
		{
			/* Hack -- non-unique demons with groups */
			okay = (!(r_ptr->flags1 & (RF1_UNIQUE)) &&
				(r_ptr->flags3 & (RF3_DEMON)) &&
				(r_ptr->flags1 & (RF1_FRIEND | RF1_FRIENDS)));
			break;
		}
		
		case SUMMON_BALROG:
		{
			/* Hack -- non-unique U's, either summoners or escorts */
			okay = (!(r_ptr->flags1 & (RF1_UNIQUE)) &&
				(r_ptr->flags7 & (RF7_S_DEMON | RF7_S_HI_DEMON) ||
				 r_ptr->flags1 & (RF1_ESCORT | RF1_ESCORTS)) &&
				(r_ptr->d_char == 'U'));
			break;
		}

		case SUMMON_MATURE_DRAGON:
		{
			/* Hack -- all 'd's with 'ature' or 'rake' in name */
			okay = ((r_ptr->d_char == 'd') &&
				!(r_ptr->flags1 & (RF1_UNIQUE)) &&
				(strstr(r_name + r_ptr->name, "ature") ||
				 strstr(r_name + r_ptr->name, "rake")));
			break;
		}
		
		case SUMMON_ULTIMATE:
		{
			/* Hack -- allow Great Power Wyrms, Greater Balrogs,
				Pit Fiends, Black Reavers, Master Q's,
				Aether Hounds, Bronze Golems, Jabberwocks */
			okay = (r_idx == 623 || r_idx == 573 || r_idx == 572 ||
			        r_idx == 524 || r_idx == 533 || r_idx == 531 ||
			        r_idx == 609 || r_idx == 517);
			break;
		}

		case SUMMON_WIGHT_WRAITH:
		{
			/* Hack -- all non-unique 'W's */
			okay = ((r_ptr->d_char == 'W') &&
				!(r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}


		default:
		{
			break;
		}

	}

	/* Result */
	return (okay);
}


/*
 * Place a monster (of the specified "type") near the given
 * location.  Return TRUE if a monster was actually summoned.
 *
 * We will attempt to place the monster up to 20 times before giving up.
 *
 * Note: SUMMON_UNIQUE and SUMMON_WRAITH (XXX) will summon Uniques
 * Note: SUMMON_HI_UNDEAD and SUMMON_HI_DRAGON may summon Uniques
 * Note: None of the other summon codes will ever summon Uniques.
 *
 * We usually do not summon monsters greater than the given depth.  -LM-
 *
 * Note that we use the new "monster allocation table" creation code
 * to restrict the "get_mon_num()" function to the set of "legal"
 * monsters, making this function much faster and more reliable.
 *
 * Note that this function may not succeed, though this is very rare.
 */
bool summon_specific(int y1, int x1, int lev, int type)
{
	int i, x, y, r_idx;

	monster_type *m_ptr;

	/* Look for a location */
	for (i = 0; i < 20; ++i)
	{
		/* Pick a distance */
		int d = (i / 15) + 1;

		/* Pick a location */
		scatter(&y, &x, y1, x1, d, 0);

		/* Require "empty" floor grid */
		if (!cave_empty_bold(y, x)) continue;

		/* Hack -- no summon on glyph of warding */
		if (cave_feat[y][x] == FEAT_GLYPH) continue;

		/* Okay */
		break;
	}

	/* Failure */
	if (i == 20) return (FALSE);

	/* Save the "summon" type */
	summon_specific_type = type;

	/* Require "okay" monsters */
	get_mon_num_hook = summon_specific_okay;

	/* Prepare allocation table */
	get_mon_num_prep();

	/* Pick a monster, using the given level */
	r_idx = get_mon_num(lev);

	/* Remove restriction */
	get_mon_num_hook = NULL;

	/* Prepare allocation table */
	get_mon_num_prep();

	/* Handle failure */
	if (!r_idx) return (FALSE);

	/* Attempt to place the monster (awake, allow groups) */
	if (!place_monster_aux(y, x, r_idx, FALSE, TRUE)) return (FALSE);

	/*hack - summoned monsters don't try to mimic*/
	m_ptr = &mon_list[cave_m_idx[y][x]];
	m_ptr->mimic_k_idx = 0;

	/* Success */
	return (TRUE);
}

/*
 * Place a pet (of the specified "type") near the given
 * location.  Return TRUE if a monster was actually summoned.
 *
 * We will attempt to place the monster up to 20 times before giving up.
 *
 * Note: This can never summon uniques.
 *
 * We usually do not summon monsters greater than the given depth.  -LM-
 *
 * Note that we use the new "monster allocation table" creation code
 * to restrict the "get_mon_num()" function to the set of "legal"
 * monsters, making this function much faster and more reliable.
 *
 * Note that this function may not succeed, though this is very rare.
 */
bool summon_specific_pet(int y1, int x1, int lev, int type)
{
	int i, x, y, r_idx;

	monster_type *m_ptr;

	/* Look for a location */
	for (i = 0; i < 20; ++i)
	{
		/* Pick a distance */
		int d = (i / 15) + 1;

		/* Pick a location */
		scatter(&y, &x, y1, x1, d, 0);

		/* Require "empty" floor grid */
		if (!cave_empty_bold(y, x)) continue;

		/* Hack -- no summon on glyph of warding */
		if (cave_feat[y][x] == FEAT_GLYPH) continue;

		/* Okay */
		break;
	}

	/* Failure */
	if (i == 20) return (FALSE);

	/* Save the "summon" type */
	summon_specific_type = type;
	
	/* Require "okay" monsters */
	get_mon_num_hook = summon_specific_okay;
	
	/* Prepare allocation table */
	get_mon_num_prep();

	/* Pick a monster, using the given level */
	r_idx = get_mon_num(lev);

	/* Remove restriction */
	get_mon_num_hook = NULL;

	/* Prepare allocation table */
	get_mon_num_prep();
	

	/* Handle failure */
	if (!r_idx) return (FALSE);

	summon_pets_hack = TRUE;

	/* Attempt to place the monster (awake, allow groups) */
	if (!place_monster_aux(y, x, r_idx, FALSE, TRUE))
	{
		summon_pets_hack = FALSE;
		return (FALSE);
	}

	summon_pets_hack = FALSE;

	/*hack - summoned monsters don't try to mimic*/
	m_ptr = &mon_list[cave_m_idx[y][x]];
	m_ptr->mimic_k_idx = 0;

	/* Success */
	return (TRUE);
}


