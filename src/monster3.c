/* File: monster3.c */

/*
 * This file contains the routines for handling the multi-tiered monster system.
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"

/* Hack - variables for caching purposes */

static s16b stored_unique;
static char mon_name[80];

/* 
 * Get the information for a monster based on its indexes.
 */
cptr monster_text(int r_idx, int u_idx)
{
	/* Paranoia - if this happens, we're in trouble */
	if (!r_idx) return (NULL);

	/* Unique */
	if (u_idx) return (u_text + u_info[u_idx].text);

	/* Normal Monster */
	else return (r_text + r_info[r_idx].text);
}

/* 
 * Get the information for a monster based on its indexes.
 */
static cptr monster_name_aux(int r_idx, int s_idx, int u_idx)
{
	char *t, *s;

	/* Paranoia - if this happens, we're in trouble */
	if (!r_idx) return (NULL);

	/* Unique */
	if (u_idx) return (u_name + u_info[u_idx].name);

	/* 
	 * Ego monster. Here is where we get fancy.
	 * There are two kinds of ego monster names - insertive, and non-insertive.
	 * Insertive names are those that go into the base name. For instance, 
	 * if the base monster is "novice warrior", and the ego name is "dwarven", the full name
	 * would be "novice dwarven warrior". Their format is "<dwarven>" for the go name, and 
	 * "novice ^warrior" for the base name.
     * Non insertive names are where the base-name is left intact. They follow the format
	 * "dwarven &", where the '&' determines the place of insertion for the base name.
     */
	if (s_idx)
	{
		t = mon_name;
		s = s_name + s_info[s_idx].name;

		/* Check if we have an "insertive" ego name */
		if (*s == '<')
		{
			s = r_name + r_info[r_idx].name;

			/* Copy the string */
			for (; *s; s++)
			{
				/* Insert the ego monster name */
				if (*s == '^') 
				{
					cptr p = s_name + s_info[s_idx].name;

					for (; *p; p++)
					{
						if (*p == '<') continue;
						if (*p == '>')
						{
							*t++ = ' ';
							continue;
						}

						*t++ = *p;
					}
				}

				else *t++ = *s;
			}
		}
		/* Not-insertive */
		else for (; *s; s++)
		{
			/* Insert the base monster name */
			if (*s == '&') 
			{
				cptr p = r_name + r_info[r_idx].name;

				for (; *p; p++)
				{
					*t++ = *p;
				}
			}

			else *t++ = *s;
		}

		/* Capitalize string */
		mon_name[0] = toupper(mon_name[0]);

		/* Terminate string */
		*t = '\0';	

		/* Return name */
		return (&mon_name[0]);
	}

	/* Normal Monster */
	t = mon_name;

	s = r_name + r_info[r_idx].name;

	/* Copy the string */
	for (; *s; s++)
	{
		if (*s == '^') continue;

		*t++ = *s;
	}

	/* Capitalize string */
	mon_name[0] = toupper(mon_name[0]);

	/* Terminate string */
	*t = '\0';	

	/* Return name */
	return (&mon_name[0]);
}

/* 
 * Get the information for a monster based on its race index alone.
 */
cptr monster_name_race(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Paranoia - if this happens, we're in trouble */
	if (!r_idx) return (NULL);

	/* Unique */
	if (r_ptr->flags1 & RF1_UNIQUE) 
	{
		int u_idx;
		monster_unique *u_ptr;

		for (u_idx = 0; u_idx < z_info->u_max; u_idx++)
		{
			u_ptr = &u_info[u_idx];

			/* Find correct u_ptr */
			if (u_ptr->r_idx == r_idx) break;
		}

		return (monster_name_aux(r_idx, 0, u_idx));
	}
	
	/* Not a unique */
	return (monster_name_aux(r_idx, 0, 0));
}

/* 
 * Get the information for a monster based on its indexes.
 */
cptr monster_name_idx(int r_idx, int s_idx, int u_idx)
{
	return (monster_name_aux(r_idx, s_idx, u_idx));
}

/* 
 * Get the information for a real monster (one that has an m_list entry)
 *
 * Note that this function must NEVER be called for a dead monster.
 */
cptr monster_name(const monster_type *m_ptr)
{
	return monster_name_aux(m_ptr->r_idx, m_ptr->s_idx, m_ptr->u_idx);
}

/*
 * The following functions return the pointer to where the actual monster information is
 * stored. If the monster is a normal monster, this is in r_info. If the monster is
 * a unique, our lives are a bit more complicated. 
 */

/* 
 * Get the information for a real monster (one that has an m_list entry)
 *
 * Note that this function must NEVER be called for a dead monster.
 */
monster_race *get_monster_real(const monster_type *m_ptr)
{
	int i;

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	/* Paranoia - if this happens, we're in trouble */
	if (!m_ptr->r_idx) quit(format("Error obtaining monster attributes code - missing r_idx."));
	if ((m_ptr->r_idx < 0) || (m_ptr->r_idx >= z_info->r_max))
		quit(format("Error obtaining monster attributes code - illegal r_idx"));

	/* Simple monster */
	if (!m_ptr->u_idx) 
	{
		return (r_ptr);
	}
	else
	{
		/* It's a unique */

		/* Paranoia - if this happens, we're in trouble */
		if ((m_ptr->u_idx < 0) || (m_ptr->u_idx >= z_info->u_max))
			quit(format("Error obtaining monster attributes code - illegal u_idx"));

		/* Optimization - check to see if the unique is already stored */
		if (stored_unique != m_ptr->u_idx)
		{
			/*
			 * Copy basic stats from u_ptr to the temporary monster. Note that name and text
			 * are never derived from it so no need to copy them.
			 */
			monster_unique *u_ptr = &u_info[m_ptr->u_idx];

			monster_temp.life = u_ptr->life;		
			monster_temp.ac = u_ptr->ac;
			monster_temp.sleep = u_ptr->sleep;			
			monster_temp.aaf = u_ptr->aaf;
			monster_temp.speed = u_ptr->speed;			
			monster_temp.mexp = u_ptr->mexp;
			monster_temp.freq_spell = 
				(u_ptr->freq_spell) ? u_ptr->freq_spell : r_ptr->freq_spell;
			monster_temp.level = u_ptr->level;			
			monster_temp.rarity = u_ptr->rarity;		
			monster_temp.d_attr = u_ptr->d_attr;		

			for (i = 0; i < 4; i++)
			{
				monster_temp.blow[i].method = u_ptr->blow[i].method;
				monster_temp.blow[i].effect = u_ptr->blow[i].effect;
				monster_temp.blow[i].d_dice = u_ptr->blow[i].d_dice;
				monster_temp.blow[i].d_side = u_ptr->blow[i].d_side;
			}

			/* d_char copied from r_ptr */
			monster_temp.d_char = r_ptr->d_char;
			monster_temp.x_char = r_ptr->d_char;
			monster_temp.x_attr = u_ptr->d_attr;

			/* Flags are a combination of both, except flags1 */
			monster_temp.flags1 = (u_ptr->flags1);		
			monster_temp.flags2 = (r_ptr->flags2 | u_ptr->flags2);		
			monster_temp.flags3 = (r_ptr->flags3 | u_ptr->flags3);
			monster_temp.flags4 = (r_ptr->flags4 | u_ptr->flags4);
			monster_temp.s_flags1 = (r_ptr->s_flags1 | u_ptr->s_flags1);		
			monster_temp.s_flags2 = (r_ptr->s_flags2 | u_ptr->s_flags2);		
			monster_temp.s_flags3 = (r_ptr->s_flags3 | u_ptr->s_flags3);		

			/* Remember the unique for next time */
			stored_unique = m_ptr->u_idx;
		}
	}

	/* Success */
	return (&monster_temp);
}

/* 
 * Get the information for a fake monster 
 *
 * Note that this uses a different monster "body".
 */
monster_race *get_monster_fake(int r_idx, int s_idx, int u_idx)
{
	int i;

	monster_race *r_ptr = &r_info[r_idx];
	monster_unique *u_ptr = &u_info[u_idx];

	/* Paranoia - if this happens, we're in trouble */
	if (!r_idx) quit(format("Error obtaining monster attributes code - missing r_idx."));
	if ((r_idx < 0) || (r_idx >= z_info->r_max))
		quit(format("Error obtaining monster attributes code - illegal r_idx"));

	/* Simple monster */
	if (!u_idx) return (r_ptr);

	/* Paranoia - if this happens, we're in trouble */
	if ((u_idx < 0) || (u_idx >= z_info->u_max))
		quit(format("Error obtaining monster attributes code - illegal u_idx"));

	/* The monster happens to be the one already stored */
	if (stored_unique == u_idx) return (&monster_temp);

	/* It's a unique */
	r_ptr = &r_info[u_ptr->r_idx];

	/*
	 * Copy basic stats from u_ptr to the temporary monster. Note that name and text
	 * are never derived from it so no need to copy them.
	 */
	monster_temp_fake.life = u_ptr->life;		
	monster_temp_fake.ac = u_ptr->ac;
	monster_temp_fake.sleep = u_ptr->sleep;			
	monster_temp_fake.aaf = u_ptr->aaf;
	monster_temp_fake.speed = u_ptr->speed;			
	monster_temp_fake.mexp = u_ptr->mexp;
	monster_temp_fake.freq_spell = (u_ptr->freq_spell) ? u_ptr->freq_spell : r_ptr->freq_spell;
	monster_temp_fake.level = u_ptr->level;			
	monster_temp_fake.rarity = u_ptr->rarity;		
	monster_temp_fake.d_attr = u_ptr->d_attr;		

	for (i = 0; i < 4; i++)
	{
		monster_temp_fake.blow[i].method = u_ptr->blow[i].method;
		monster_temp_fake.blow[i].effect = u_ptr->blow[i].effect;
		monster_temp_fake.blow[i].d_dice = u_ptr->blow[i].d_dice;
		monster_temp_fake.blow[i].d_side = u_ptr->blow[i].d_side;
	}

	/* hack - d_char copied from r_ptr, x_attr and x_char same as d_'s*/
	monster_temp_fake.d_char = r_ptr->d_char;
	monster_temp_fake.x_char = r_ptr->d_char;
	monster_temp_fake.x_attr = u_ptr->d_attr;

	/* Flags are a combination of both, except flags1 */
	monster_temp_fake.flags1 = (u_ptr->flags1);		
	monster_temp_fake.flags2 = (r_ptr->flags2 | u_ptr->flags2);		
	monster_temp_fake.flags3 = (r_ptr->flags3 | u_ptr->flags3);		
	monster_temp_fake.flags4 = (r_ptr->flags4 | u_ptr->flags4);		
	monster_temp_fake.s_flags1 = (r_ptr->s_flags1 | u_ptr->s_flags1);		
	monster_temp_fake.s_flags2 = (r_ptr->s_flags2 | u_ptr->s_flags2);		
	monster_temp_fake.s_flags3 = (r_ptr->s_flags3 | u_ptr->s_flags3);		

	/* Success */
	return (&monster_temp_fake);
}

/* 
 * Get the lore information
 */
monster_lore *get_lore_idx(int r_idx, int u_idx)
{
	int i;

	monster_lore *lr_ptr = &lr_list[r_idx];
	monster_lore *lu_ptr = &lu_list[u_idx];

	/* Paranoia - if this happens, we're in trouble */
	if (!r_idx) quit(format("Error obtaining monster attributes code - missing r_idx."));
	if ((r_idx < 0) || (r_idx >= z_info->r_max))
		quit(format("Error obtaining monster attributes code - illegal r_idx"));

	/* Simple monster */
	if (!u_idx) return (lr_ptr);

	/* Paranoia - if this happens, we're in trouble */
	if ((u_idx < 0) || (u_idx >= z_info->u_max))
		quit(format("Error obtaining monster attributes code - illegal u_idx"));

	/*
	 * Copy basic stats from u_ptr to the temporary monster. Note that name andtext,
	 * are never derived from it so no need to copy them.
	 */
	for (i = 0; i < 4; i++)
	{
		lore_temp.r_blows[i] = lu_ptr->r_blows[i];
	}

	lore_temp.r_cast = lu_ptr->r_cast;
	lore_temp.r_deaths = lu_ptr->r_deaths;
	lore_temp.r_drop_gold = lu_ptr->r_drop_gold;
	lore_temp.r_drop_item = lu_ptr->r_drop_item;
	lore_temp.r_ignore = lu_ptr->r_ignore;
	lore_temp.r_wake = lu_ptr->r_wake;
	lore_temp.r_tkills = lu_ptr->r_tkills;
	lore_temp.r_pkills = lu_ptr->r_pkills;
	lore_temp.r_sights = lu_ptr->r_sights;

	/* Flags are a combination of both */
	lore_temp.flags1 = (lu_ptr->flags1);		
	lore_temp.flags2 = (lr_ptr->flags2 | lu_ptr->flags2);		
	lore_temp.flags3 = (lr_ptr->flags3 | lu_ptr->flags3);
	lore_temp.flags4 = (lr_ptr->flags4 | lu_ptr->flags4);
	lore_temp.s_flags1 = (lr_ptr->s_flags1 | lu_ptr->s_flags1);		
	lore_temp.s_flags2 = (lr_ptr->s_flags2 | lu_ptr->s_flags2);		
	lore_temp.s_flags3 = (lr_ptr->s_flags3 | lu_ptr->s_flags3);		

	/* Success */
	return (&lore_temp);
}

/*
 * This function is used to enable the player to learn a monster ability. It needs to figure
 * out what part of the monster memory this ability belongs to, and place it there.
 *
 * "unseen" allows updating the info for monsters you can't see
 */
void lore_learn(const monster_type *m_ptr, int mode, u32b what, bool unseen)
{
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *lr_ptr = &lr_list[m_ptr->r_idx];

	/* Paranoia - if this happens, we're in trouble */
	if (!m_ptr->r_idx) quit(format("Error obtaining monster attributes code - missing r_idx."));
	if ((m_ptr->r_idx < 0) || (m_ptr->r_idx >= z_info->r_max))
		quit(format("Error obtaining monster attributes code - illegal r_idx"));
	if ((m_ptr->u_idx < 0) || (m_ptr->u_idx >= z_info->u_max))
		quit(format("Error obtaining monster attributes code - illegal u_idx"));

	/* Unseen monster */
	if (!unseen && !m_ptr->ml) return;

	if (!m_ptr->u_idx)
	{
		/* Learn something */
		switch (mode)
		{
			case LRN_FLAG1:   lr_ptr->flags1 |= (r_ptr->flags1 & what); break;
			case LRN_FLAG2:   lr_ptr->flags2 |= (r_ptr->flags2 & what); break;
			case LRN_FLAG3:   lr_ptr->flags3 |= (r_ptr->flags3 & what); break;
			case LRN_FLAG4:   lr_ptr->flags4 |= (r_ptr->flags4 & what); break;
			case LRN_S_FLAG1: lr_ptr->s_flags1 |= (r_ptr->s_flags1 & what); break;
			case LRN_S_FLAG2: lr_ptr->s_flags2 |= (r_ptr->s_flags2 & what); break;
			case LRN_S_FLAG3: lr_ptr->s_flags3 |= (r_ptr->s_flags3 & what); break;
			case LRN_CASTS:   if (lr_ptr->r_cast   < MAX_UCHAR) lr_ptr->r_cast++;   break;
			case LRN_SIGHTS:  if (lr_ptr->r_sights < MAX_SHORT) lr_ptr->r_sights++; break;
			case LRN_IGNORES: if (lr_ptr->r_ignore < MAX_UCHAR) lr_ptr->r_ignore++; break;
			case LRN_WAKES:   if (lr_ptr->r_wake   < MAX_UCHAR) lr_ptr->r_wake++;   break;
			case LRN_PDEATH:  if (lr_ptr->r_deaths < MAX_SHORT) lr_ptr->r_deaths++; break;
			case LRN_MDEATH:  if (lr_ptr->r_pkills < MAX_SHORT) lr_ptr->r_pkills++;
							  if (lr_ptr->r_tkills < MAX_SHORT) lr_ptr->r_tkills++; break;
			case LRN_BLOWS:   if (lr_ptr->r_blows[what] < MAX_UCHAR) lr_ptr->r_blows[what]++; break;
			case LRN_ITEM:    if (what > lr_ptr->r_drop_item) lr_ptr->r_drop_item = (byte)what; break;
			case LRN_GOLD:    if (what > lr_ptr->r_drop_gold) lr_ptr->r_drop_gold = (byte)what; break;
		}
	}
	else
	/* Unique - more complex handling */
	{
		monster_unique *u_ptr = &u_info[m_ptr->u_idx];
		monster_lore *lu_ptr = &lu_list[m_ptr->u_idx];

		bool unique_hack = ((r_ptr->flags1 & RF1_UNIQUE) ? TRUE : FALSE);

		/* Learn something */
		switch (mode)
		{
			case LRN_FLAG1:   lu_ptr->flags1 |= (u_ptr->flags1 & what);  break;
			case LRN_FLAG2:   lr_ptr->flags2 |= (r_ptr->flags2 & what);
							  lu_ptr->flags2 |= (u_ptr->flags2 & what);  break;
			case LRN_FLAG3:   lr_ptr->flags3 |= (r_ptr->flags3 & what);
							  lu_ptr->flags3 |= (u_ptr->flags3 & what);  break;
			case LRN_FLAG4:   lr_ptr->flags4 |= (r_ptr->flags4 & what);
							  lu_ptr->flags4 |= (u_ptr->flags4 & what);  break;
			case LRN_S_FLAG1: lr_ptr->s_flags1 |= (r_ptr->s_flags1 & what);
							  lu_ptr->s_flags1 |= (u_ptr->s_flags1 & what);  break;
			case LRN_S_FLAG2: lr_ptr->s_flags2 |= (r_ptr->s_flags2 & what);
							  lu_ptr->s_flags2 |= (u_ptr->s_flags2 & what);  break;
			case LRN_S_FLAG3: lr_ptr->s_flags3 |= (r_ptr->s_flags3 & what);
							  lu_ptr->s_flags3 |= (u_ptr->s_flags3 & what);  break;
			case LRN_CASTS:   if (lu_ptr->r_cast   < MAX_UCHAR) lu_ptr->r_cast++;   break;
			case LRN_SIGHTS:  if (lu_ptr->r_sights < MAX_SHORT) lu_ptr->r_sights++;
							  if (lr_ptr->r_sights < MAX_SHORT) lr_ptr->r_sights++; break;
			case LRN_IGNORES: if (lu_ptr->r_ignore < MAX_UCHAR) lu_ptr->r_ignore++; break;
			case LRN_WAKES:   if (lu_ptr->r_wake   < MAX_UCHAR) lu_ptr->r_wake++;   break;
			case LRN_PDEATH:  if (lu_ptr->r_deaths < MAX_SHORT) lu_ptr->r_deaths++; break;
			case LRN_MDEATH:  if (lu_ptr->r_pkills < MAX_SHORT) lu_ptr->r_pkills++;
							  if (lu_ptr->r_tkills < MAX_SHORT) lu_ptr->r_tkills++; 
							  if (unique_hack)
							  {
								/* 
								 * Hack which enables "special" uniques to be sorted correctly 
								 * in various monster memory functions
								 */
								if (lr_ptr->r_pkills < MAX_SHORT) lr_ptr->r_pkills++;
								if (lr_ptr->r_tkills < MAX_SHORT) lr_ptr->r_tkills++; break;
							  } break;
			case LRN_BLOWS:   if (lu_ptr->r_blows[what] < MAX_UCHAR) lu_ptr->r_blows[what]++;  break;
			case LRN_ITEM:    if (what > lu_ptr->r_drop_item) lu_ptr->r_drop_item = (byte)what; break;
			case LRN_GOLD:    if (what > lu_ptr->r_drop_gold) lu_ptr->r_drop_gold = (byte)what; break;
		}
	}

	return;
}
