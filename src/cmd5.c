/*
 * File: cmd5.c
 * Purpose: Spell and prayer casting/praying
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
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

static bool add_spell(object_type *o_ptr, s16b spellno)
{
	s16b count = 0, i;
	for (i=0; i<MAX_SPELLS_PER_ITEM; i++)
	{
		if (has_spell(o_ptr, i)) count++;
	}

	switch (o_ptr->sval)
	{
		case SV_BOOK_SMALL : 
			if (count==4)
			{
				msg_print("This book is full");
				return FALSE;
			}
		case SV_BOOK_AVG: 
			if (count==8)
			{
				msg_print("This book is full");
				return FALSE;
			}
		case SV_BOOK_LARGE : 
			if (count==12)
			{
				msg_print("This book is full");
				return FALSE;
			}
	}
	set_spell(o_ptr, spellno);
	return(TRUE);
}

bool item_tester_hook_book(const object_type *o_ptr)
{
   s16b count = 0, i;

   if (o_ptr->tval != TV_BOOK) return (FALSE);

   for (i=0; i<MAX_SPELLS_PER_ITEM; i++)
   {
      if (has_spell(o_ptr, i)) count++;
   }

   if ( ((o_ptr->sval == SV_BOOK_SMALL) && (count==4)) ||
        ((o_ptr->sval == SV_BOOK_AVG) && (count==8)) ||
        ((o_ptr->sval == SV_BOOK_LARGE) && (count==12)))
   {
      return (FALSE);
   }

   return (TRUE);
}

void read_spell(object_type *o_ptr, s16b item)
{
	object_type *j_ptr;
	int itemb;
	s16b level;
	s16b amt = 1;
	bool destroyed = FALSE;
	s16b chance, i, j, cnt = 0;
	s16b index[MAX_SPELLS_PER_ITEM];
	bool done, found;

	static s16b	was_reading = FALSE;
	static object_type *k_ptr;
	static s16b	itemr;

	if (!cp_ptr->spell_stat)
	{
		msg_print("You don't know how to read this!");
		return;
	}

	if (p_ptr->timed[TMD_READING] == 0)
	{
		if (was_reading)
		{
			msg_print("You know nothing more than before about this spell.");
			p_ptr->redraw |= PR_READ;
			redraw_stuff();
			was_reading = FALSE;

			return;
		}
		assert(o_ptr != NULL);
		level = s_info[o_ptr->sval].level;

		/* level + randint(2*level) means 1-42 + randint(2-84) */
		chance = level + (s16b) randint1(2*level);
		if (!object_aware_p(o_ptr) && !object_known_p(o_ptr) &&
			 (level+randint1(2*level)>p_ptr->lev) )
		{
			msg_print("You can't figure out which side is up!");
			return;
		}
		else
		{
			if (!object_aware_p(o_ptr) && !object_known_p(o_ptr))
			{
				msg_print("You figured out the use of this spell!");
			}

			/* The item was tried */
			object_tried(o_ptr);

			/* An identification was made */
			if (!object_aware_p(o_ptr))
			{
				object_aware(o_ptr);
				gain_exp((level + (p_ptr->lev >> 1)) / p_ptr->lev);
			}
		}

		/* p_ptr->lev/2 0 to 25											 */
		/* skill_dev^2 / 10 gives 0 to 12								*/
		/* rand_int(p_ptr->skill_dev) gives 0 to 11				  */
		/* scroll levels range from 0 to 45							 */
		chance=p_ptr->lev/2 + (p_ptr->state.skills[SKILL_DEVICE]*p_ptr->state.skills[SKILL_DEVICE])/360 +
			 (s16b) randint1(p_ptr->state.skills[SKILL_DEVICE]/6);
		if (p_ptr->lev/2 + (p_ptr->state.skills[SKILL_DEVICE]*p_ptr->state.skills[SKILL_DEVICE])/360 +
			 randint1(p_ptr->state.skills[SKILL_DEVICE]/6) <level)
		{
			switch(randint1(10))
			{
				case  1: 
					msg_print("You don't understand these runes.");
					break;
				case  2: 
					msg_print("You feel dizzy.");
					if (!p_ptr->state.resist_confu)
						inc_timed(TMD_CONFUSED, randint1(10) + 10, TRUE);
					else
						msg_print("But the feeling passes quickly.");
					break;
				case  3: msg_print("Your head spins.");
					if (!p_ptr->state.resist_sound)
							inc_timed(TMD_STUN, randint1(25) + 10, TRUE);
					else
						msg_print("But the feeling passes quickly.");
					break;
				case  4: 
					msg_print("You feel very dizzy.");
					if (!p_ptr->state.resist_confu)
						inc_timed(TMD_CONFUSED, randint1(25) + 25, TRUE);
					else
						msg_print("But the feeling passes quickly.");
					break;
				case  5: 
					msg_print("The runes fade away while reading.");
					destroyed = TRUE;
					break;
				case  6: 
					msg_print("You feel something strange happening.");
					if (!p_ptr->state.resist_nexus)
						teleport_player(10);
					else
						msg_print("But the feeling passes quickly.");
					break;
				case  7: 
					msg_print("The meaning seems to slip away.");
/*					inc_timed(TMD_SLIDING, randint1(10) + 10, TRUE); */ /* TODO Need to implement SLIDING */
					break;
				case  8: 
					msg_print("You cut your fingers on the sharp edge of the spell page.");
					set_timed(TMD_CUT, p_ptr->timed[TMD_CUT] + randint1(25) + 25, TRUE);
					break;
				case  9: 
					msg_print("The runes cloud your mind.");
					(void)unlite_area(10, 3);
					if (!p_ptr->state.resist_blind)
						inc_timed(TMD_BLIND, randint1(5) + 3, TRUE);
					else
						msg_print("But your vision soon becomes clear again.");
					break;
				case 10: 
					msg_print("The ink seems poisonous.");
					if (!(p_ptr->state.resist_pois || p_ptr->timed[TMD_OPP_POIS]))
						inc_timed(TMD_POISONED, 10 + randint1(10), TRUE);
					else
						msg_print("But your queasiness soon passes.");
					break;
			}
			if (destroyed)
			{
				inven_item_increase(item, -1);
				inven_item_describe(item);
				inven_item_optimize(item);
			}
			return;
		}
		if (get_check("You can read this spell now or meditate on it. Read now?"))
		{
			if (exec_page(o_ptr->sval))
			{
				inven_item_increase(item, -1);
				inven_item_describe(item);
				inven_item_optimize(item);
			}
			return;
		}
		chance = level*4 + (s16b) randint0(8*level);
		msg_format("You start to meditate on this spell.", chance);
		was_reading = TRUE;
		max_reading = chance;
		p_ptr->timed[TMD_READING] = chance;
		k_ptr = o_ptr;
		itemr = item;
		return;
	}
	else /* p_ptr->reading==1; */
	{
		bool priest = ( (p_ptr->pclass == CLASS_PRIEST) ||
							 (p_ptr->pclass == CLASS_PALADIN));
		bool highprst = (p_ptr->pclass == CLASS_HIGHPRST);

		o_ptr = k_ptr;
		item = itemr;

		p_ptr->redraw |= PR_READ;
		redraw_stuff();

		/* we can't read it if it's for the wrong class */
		if ( (!highprst) &&
			  (!(s_info[o_ptr->sval].sclass & (1L<<p_ptr->pclass))))
		{
			msg_format("This %s is totally alien to you.",
						  priest?"prayer":"spell");
			return;
		}

		msg_format("You are enlightened about this %s of %s.",
					  (s_info[o_ptr->sval].sclass&CLASS_PRIEST)?"prayer":"spell",
					  s_name + s_info[o_ptr->sval].name);

		/* did we read a spell we already knew? */
		found = FALSE;
		for (i=0; i<INVEN_PACK; i++)
		{
			if (inventory[i].tval == TV_BOOK)
			{
				for (j=0; j<z_info->s_max; j++)
				{
					if (has_spell(&inventory[i], j))
					{
						index[cnt++]=j;
					}
				}
			}
		}
		/* now compare known spells to the just read spell */
		for (i=0; ( (i<cnt) && !found); i++)
		{
			if (index[i]==o_ptr->sval) found = TRUE;
		}
		/* no exit, as adding it to a book does nothing if already known */
		if (found)
		{
			msg_print("This spell seems strangely familiar.");
		}

		max_reading = 0;
		was_reading = FALSE;

		done = FALSE;
		while (!done)
		{
			item_tester_hook = item_tester_hook_book;
			if (!get_item(&itemb, "Bind it in what? ", "You cannot bind it in anything - your memories of this spell fade.", (USE_EQUIP | USE_INVEN | USE_FLOOR)))
			{
				item_tester_hook = NULL;
				return;
			}
			item_tester_hook = NULL;

			/* Get the item (in the pack) */
			j_ptr = &inventory[itemb];
 
			/* if the book is stacked, take special care and split the stack  */
			/* note that books with spell-sets don't stack, even if they hold */
			/* the same spells																*/
			if (j_ptr->number>1)
			{
				object_type tmp_obj;
				tmp_obj = *j_ptr;
				tmp_obj.number = j_ptr->number - 1;
				j_ptr->number = 1;
				done=(add_spell(j_ptr, o_ptr->sval)!=-1);
				/* now destroy the original spell */
				if (done)
				{
					char o_name[240];
 
					inven_item_increase(item, -amt);
					inven_item_optimize(item);
				  
					object_desc(o_name, sizeof(o_name), &tmp_obj, FALSE, 0);
					msg_format("You unstack %s.", o_name);

					(void)inven_carry(&tmp_obj);
				}
				else
				{
					/* something went wrong, reset the stack to its original size */
					j_ptr->number += tmp_obj.number;
				}
			}
			else
			{

				done=(add_spell(j_ptr, o_ptr->sval)!=-1);
				/* now destroy the original spell */
				inven_item_increase(item, -amt);
				inven_item_optimize(item);
			}
		}
	}
}

/*
 * this tests if a spell-caster is wielding certain rings, which help
 * certain spells
 *
 * at the moment, SV_RING_FLAMES helps with fiery spells
 *                SV_RING_ELEC   helps with electricity spells
 *                SV_RING_ACID   helps with acidic spells
 *                SV_RING_ICE    helps with cold spells
 */
static bool elemental_ring_worn(s16b sval)
{
   object_type *o_ptr;

   o_ptr = &inventory[INVEN_LEFT];
   if ((o_ptr->tval == TV_RING) && (o_ptr->sval == sval)) 
	   return TRUE;
   o_ptr = &inventory[INVEN_RIGHT];
   if ((o_ptr->tval == TV_RING) && (o_ptr->sval == sval)) 
	   return TRUE;

   /* High Priests can wield 2 other rings */
   if (!(cp_ptr->flags & CF_EXTRA_RINGS))
	   return FALSE;
   o_ptr = &inventory[INVEN_WIELD];
   if ((o_ptr->tval == TV_RING) && (o_ptr->sval == sval)) 
	   return TRUE;
   o_ptr = &inventory[INVEN_BOW];
   if ((o_ptr->tval == TV_RING) && (o_ptr->sval == sval)) 
	   return TRUE;

   return FALSE;
}

/* 
 * this function tests if there is a ring that helps a certain spell
 * TODO Check if any spells missing
 */
static bool ring_helps_spell(int spellno)
{
   if ((spellno == SPELL_LIGHTNING_BOLT) && (elemental_ring_worn(SV_RING_ELEC)))
      return TRUE;

   if ((spellno == SPELL_FROST_BOLT) && (elemental_ring_worn(SV_RING_ICE)))
      return TRUE;

   if ((spellno == SPELL_FIRE_BOLT) && (elemental_ring_worn(SV_RING_FLAMES)))
      return TRUE;

   if ((spellno == SPELL_FROST_BALL) && (elemental_ring_worn(SV_RING_ICE)))
      return TRUE;

   if ((spellno == SPELL_FIRE_BALL) && (elemental_ring_worn(SV_RING_FLAMES)))
      return TRUE;

   if ((spellno == SPELL_ACID_BOLT) && (elemental_ring_worn(SV_RING_ACID)))
      return TRUE;

   if ((spellno == SPELL_ICE_STORM) && (elemental_ring_worn(SV_RING_ICE)))
      return TRUE;

   return FALSE;
}

/*
 * determine how many mana a spell will take
 * certain rings help
 */
static s16b spell_mana(int spellno)
{
   if (ring_helps_spell(spellno))
   {
      return (s_info[spellno].mana / 2);
   }
   else
   {
      return (s_info[spellno].mana);
   }
}



/*
 * Likelihood of failing to cast spell.
 */
s16b page_chance(int spellno)
{
	s16b chance, minfail;

/*	const magic_type *s_ptr; */

	/* Paranoia -- must be literate */
	if (!cp_ptr->spell_stat) 
		return (100);

	/* Get the spell */
/*	s_ptr = &mp_ptr->info[spell]; */
	/* Extract the base spell failure rate */
/*	chance = s_ptr->sfail; */

	/* Extract the base spell failure rate */
	chance = s_info[spellno].chance;

	/* Reduce failure rate by "effective" level adjustment */
/*	chance -= 3 * (p_ptr->lev - s_ptr->slevel); */

	/* Reduce failure rate by "effective" level adjustment */
	chance -= 3 * (p_ptr->lev - s_info[spellno].level);

	/* Reduce failure rate by INT/WIS adjustment */
	chance -= adj_mag_stat[p_ptr->state.stat_ind[cp_ptr->spell_stat]];

	/* Reduce failure rate by INT/WIS adjustment */
/*	chance -= 3 * ((s16b)adj_mag_stat[p_ptr->stat_ind[(s16b)cp_ptr->spell_stat]] - 1); */

	/* Not enough mana to cast */
/*	if (s_ptr->smana > p_ptr->csp)
	{
		chance += 5 * (s_ptr->smana - p_ptr->csp);
	} */

	/* Not enough mana to cast */
	if (spell_mana(spellno) > p_ptr->csp)
	{
		chance += 5 * (spell_mana(spellno) - p_ptr->csp);
	}

	/* Extract the minimum failure rate */
	minfail = adj_mag_fail[p_ptr->state.stat_ind[cp_ptr->spell_stat]];

	/* Extract the minimum failure rate */
/*	minfail = (s16b)adj_mag_fail[(s16b)p_ptr->stat_ind[(s16b)cp_ptr->spell_stat]]; */

	/* Non mage/priest characters never get better than 5 percent */
	if (!(cp_ptr->flags & CF_ZERO_FAIL))
	{
		if (minfail < 5) minfail = 5;
	}

	/* Non mage/priest characters never get too good */
	if ((p_ptr->pclass != CLASS_MAGE) && (p_ptr->pclass != CLASS_PRIEST) &&
		(p_ptr->pclass != CLASS_WARMAGE) && (p_ptr->pclass != CLASS_HIGHPRST))
	{
		if (minfail < 5) minfail = 5;
	}

	/* certain rings help */
	if (ring_helps_spell(spellno))
	{
		minfail = minfail / 2;
		chance = chance / 2;
	}

	/* Priest prayer penalty for "edged" weapons (before minfail) */
	if (p_ptr->state.icky_wield)
		chance += 25;

	/* Hack -- Priest prayer penalty for "edged" weapons  -DGK */
/*	if ((p_ptr->pclass == 2) && (p_ptr->icky_wield)) chance += 25; */

	/* Fear makes spells harder (before minfail) */
	/* Note that spells that remove fear have a much lower fail rate than
	 * surrounding spells, to make sure this doesn't cause mega fail */
	if (p_ptr->state.afraid) chance += 20;

	/* Minimal and maximal failure rate */
	if (chance < minfail) chance = minfail;
	if (chance > 50) chance = 50; /* My, aren't we nice? */

	/* Minimum failure rate */
/*	if (chance < minfail) chance = minfail; */


	/* Stunning makes spells harder (after minfail) */
	if (p_ptr->timed[TMD_STUN] > 50) chance += 25;
	else if (p_ptr->timed[TMD_STUN]) chance += 15;

	/* Stunning makes spells harder */
/*	if (p_ptr->stun > 50)
	{
		chance += 25;
	}
	else
	{
		if (p_ptr->stun>0) chance += 15;
	} */

	/* For classes affected by wearing gloves (INT casters) +25 failure chance */
	if (p_ptr->cumber_glove) 
		chance += chance +25;

	/* Amnesia doubles failure change */
	if (p_ptr->timed[TMD_AMNESIA]) chance *= 2;

	/* Always a 5 percent chance of working */
	if (chance > 95) chance = 95;

	/* Return the chance */
	return (chance);
}

/*
 * Print a list of spells (for browsing or casting or viewing).
 */
void print_spells(s16b *index, s16b count)
{
	s16b i, col;
	byte color;
	char out_val[160];
	cptr comment;
	char info[80];
	bool cheat_any = FALSE;
	spell_type *s_ptr;
	cptr scalestr[6] = {"small","avg  ","large","super"};
	cptr categ[MAX_SPELL_TYPES] = 
	{ 
		"Nature Forces",
		"Dark Forces",
		"Escape",
		"Heal",
		"Sense",
		"Change Other",
		"Change Item",
		"Change Self",
		"Change World"
	};

	/* Print column */
	col = 4;

	/* Title the list */
	prt("", 1, 0);
	put_str("Name", 1, col + 3);
	for (i = 0; i < count; i++)					 /* Dump the spells */
	{
		s_ptr = &s_info[index[i]];				 /* Access the spell */

		if (s_ptr->numcast >= (10 * s_ptr->level))
		{
			cheat_any = TRUE;
		}
	}

	/* TODO Consider supporting show_spell_numbers from Angband/64 */
	/* TODO Consider supporting cheat_spell_info and cheat_any from Angband/64 */
	put_str("Lv Ma Fail", 1, col + 27);

	my_strcpy(info,"",sizeof(info));
	my_strcpy(info,"",sizeof(info));

	for (i = 0; i < count; i++)					 /* Dump the spells */
	{
		s_ptr = &s_info[index[i]];				 /* Access the spell */
		color = (byte)TERM_L_GREEN;
		if (s_ptr->numcast == 0)
		{
			 color = (byte)TERM_L_BLUE;
		}
		if (s_ptr->numcast >= (10 * s_ptr->level))
		{
			 color = (byte)TERM_GREEN;
		}
		/* Was extra_new_spell_info */
		get_spell_info(index[i], info, sizeof(info)); /* TODO Check this works! */

		/* Use that info */
		comment = info;
		if ((s_ptr->numcast >= (10 * s_ptr->level) ) )
		{
			strnfmt(out_val, sizeof(out_val), "%c) %-23s%3d %2d  %2d%% %5s %-14s %s",
					  I2A(i), s_name + s_ptr->name, s_ptr->level, spell_mana(index[i]),
					  page_chance(index[i]), scalestr[(s16b)s_ptr->scale],
					  categ[s_ptr->type], info);
		}
		else if (s_ptr->numcast > 0)
		{
			strnfmt(out_val, sizeof(out_val), "%c) %-23s%3d %2d  %2d%%",
					  I2A(i), s_name + s_ptr->name, s_ptr->level, spell_mana(index[i]),
					  page_chance(index[i]));
		}
		else if (s_ptr->numcast == 0)
		{
			strnfmt(out_val, sizeof(out_val), "%c) %-23s%3d %2d",
					  I2A(i), s_name + s_ptr->name, s_ptr->level, spell_mana(index[i]));
		}
		prt("", 2+i, 0); /* this clears the line */
		c_put_str(color,out_val,2+i,col);
	}

	/* Clear the bottom line */
	prt("", 2 + i, 0);
}

/*
 * this function penalizes you for reading a spell
 * from a wrong class
 */
static void penalize_spell(void)
{
	switch(randint1(10))
	{
		/* in 50% of cases, lose all mana */
		case 1:
		case 2:
		case 3:
		case 4:
		case 5:
			msg_print("You feel numb and drained.");
			p_ptr->csp = 0;
			p_ptr->csp_frac = 0;
			break;
		/* in 20% of cases, induce hallucination */
		case 6:
		case 7:  
			msg_print("You're not sure of anything anymore.");
			inc_timed(TMD_IMAGE, randint1(50) + 50, TRUE);
			break;
		/* in 20% of cases, induce slowness and blindness */
		case 8:
		case 9:
			msg_print("Your eyes burn and you feel an odd calm.");
			inc_timed(TMD_SLOW, randint1(30) + 30, TRUE);
			inc_timed(TMD_BLIND, randint1(30) + 30, TRUE);
			break;
		/* in 10% of cases, reduce the stat */
		case 10: 
			/* are we an INT user? */
			if (cp_ptr->spell_stat == A_INT)
				do_dec_stat(A_INT, FALSE);
			else
				do_dec_stat(A_WIS, FALSE);
			break;
	}
}

/*
 * execute a page with a spell on it.
 * return TRUE if the spell should cease to exist
 *        FALSE if it shouldn't disappear
 */
bool exec_page(s16b spellno)
{
	s32b i;
	s16b chance, plev;
	bool wrong_class = FALSE, aborted = FALSE;

	plev = p_ptr->lev;

	/* Verify "dangerous" spells */
	if (spell_mana(spellno) > p_ptr->csp)
	{
		/* Warning */
		msg_print("You do not have enough mana to cast this spell.");

		/* Verify */
		if (!get_check("Attempt it anyway? "))
		{
			return FALSE;
		}
	}

	/* Spell failure chance */
	chance = page_chance(spellno);

	/* reading from a book is tested elsewhere, so this is stricly for direct */
	/* reading a spell */

	if (!(s_info[spellno].sclass & (1L<<p_ptr->pclass))) /* TODO Check this works */
	{
		/* not for your class? Take a penalty! */
		chance = chance / 2;
		wrong_class = TRUE;
	}

	/* Failed spell */
	i = randint0(100);
	if (i < chance)
	{
/*       if (flush_failure) flush(); */ /* TODO is this needed ? */
		msg_print("You failed to get the spell off!");
	}
	else /* Process spell */
	{
		s16b sqrtlev=0;
		s32b factor;

		aborted = cast_spell(spellno); /* was exec_spell */

		if (aborted) return (FALSE);

		/* now gain some experience: first time, 100%, second time 33%, then 11% etc. */

		while ( (sqrtlev*sqrtlev) < s_info[spellno].level) sqrtlev++;
		factor = 1 + (s_info[spellno].numcast << 3);
		gain_exp( (sqrtlev * s_info[spellno].mana * (s_info[spellno].scale + 1)) / factor); 
		s_info[spellno].numcast++; 
		if ((s_info[spellno].numcast == (10 * s_info[spellno].level)) /* && (! cheat_spell_info) */ ) /* TODO Implement cheat_spell_info ? */
		{
			msg_format("You feel you know more about the spell of %s.\n", s_name + s_info[spellno].name);
		}
	}

	/* Sufficient mana */
	if (s_info[spellno].mana <= p_ptr->csp)
	{
		/* Use some mana */
		p_ptr->csp -= spell_mana(spellno);
	}
	else /* Over-exert the player */
	{
		s16b oops = spell_mana(spellno) - p_ptr->csp;

		/* No mana left */
		p_ptr->csp = 0;
		p_ptr->csp_frac = 0;

		/* Message */
		msg_print("You faint from the effort!");

		/* Hack -- Bypass free action */
		inc_timed(TMD_PARALYZED, randint1(5 * oops), TRUE);

		/* Damage CON (possibly permanently) */
		if (randint0(100) < 50)
		{
			/* Message */
			msg_print("You have damaged your health!");

			if (randint0(100) < 25)
				do_dec_stat(A_CON, TRUE);
			else
				do_dec_stat(A_CON, FALSE);
		}
	}
	if (wrong_class)
	{
		penalize_spell();
	}
	p_ptr->redraw |= PR_MANA;

	return (TRUE);
}

#if 0

/*
 * Cast a spell
 */
void do_cmd_cast_spell(void)
{
   s16b         spellno;

   /* Check some conditions */
   if (p_ptr->blind>0)
   {
      msg_print("You can't see anything.");
      return;
   }
   if (no_lite())
   {
      msg_print("You have no light to read by.");
      return;
   }
   if (p_ptr->confused>0)
   {
      msg_print("You are too confused!");
      return;
   }
   
   spellno = select_spell();
   if (spellno == -1) return ;

   /* if exec_page returns FALSE, we don't have enough mana and aborted */
   if (exec_page(spellno))
   {
      /* Take a turn */
      energy_use = 100;
   }
}
#endif

/*
 * Determine if a spell is "okay" for the player to cast or study
 * The spell must be legible, not forgotten, and also, to cast,
 * it must be known, and to study, it must not be known.
 * When browsing a book, all legible spells are okay.
 */
bool spell_okay(int spell, bool known, bool browse)
{
	const magic_type *s_ptr;

	/* Get the spell */
	s_ptr = &mp_ptr->info[spell];

	/* Spell is illegible */
	if (s_ptr->slevel >= 99) return (FALSE);

	/* Spell is too hard */
	if (s_ptr->slevel > p_ptr->lev) return (browse);

	/* Spell is forgotten */
	if (p_ptr->spell_flags[spell] & PY_SPELL_FORGOTTEN)
	{
		/* Never okay */
		return (!browse);
	}

	/* Spell is learned */
	if (p_ptr->spell_flags[spell] & PY_SPELL_LEARNED)
	{
		/* Okay to cast or browse, not to study */
		return (known || browse);
	}

	/* Okay to study or browse, not to cast */
	return (!known || browse);
}

static void browse_book(const object_type *o_ptr)
{
	s16b count = 0, i;
	s16b index[MAX_SPELLS_PER_ITEM];

	for (i=0; i< z_info->s_max; i++)
	{
		if (has_spell(o_ptr, i)) index[count++]=i;
	}

	/* Save the screen */
	(void)Term_save();

	print_spells(index, count);

	/* Clear the top line */
	prt("", 0, 0);

	/* Prompt user */
	put_str(format("Spells in your %s - [Press any key to continue]",
			(s16b) o_ptr->tval==TV_BOOK?"book":"staff"),0, 0);

	/* Wait for key */
	(void)inkey();

	/* Restore the screen */
	(void)Term_load();
}

#if 0
/*
 * View the detailed description for a selected spell.
 */
static void browse_spell(int spell)
{
	const magic_type *s_ptr;

	char out_val[160];
	char help[20];

	const char *comment = help;

	byte line_attr;


	/* Redirect output to the screen */
	text_out_hook = text_out_to_screen;

	/* Save the screen */
	screen_save();

	/* Get the magic and spell info */
	s_ptr = &mp_ptr->info[spell];

	/* Get extra info */
	get_spell_info(spell, help, sizeof(help));

	/* Assume spell is known and tried */
	line_attr = TERM_WHITE;

	/* Analyze the spell */
	if (p_ptr->spell_flags[spell] & PY_SPELL_FORGOTTEN)
	{
		comment = " forgotten";
		line_attr = TERM_YELLOW;
	}
	else if (!(p_ptr->spell_flags[spell] & PY_SPELL_LEARNED))
	{
		if (s_ptr->slevel <= p_ptr->lev)
		{
			comment = " unknown";
			line_attr = TERM_L_BLUE;
		}
		else
		{
			comment = " difficult";
			line_attr = TERM_RED;
		}
	}
	else if (!(p_ptr->spell_flags[spell] & PY_SPELL_WORKED))
	{
		comment = " untried";
		line_attr = TERM_L_GREEN;
	}

	/* Show spell name and comment (if any) on first line of screen */
	if (streq(comment, ""))
	{
		strnfmt(out_val, sizeof(out_val), "%^s",
	    	    get_spell_name(spell));
	}
	else
	{
		strnfmt(out_val, sizeof(out_val), "%^s (%s)",
	    	    get_spell_name(spell),
	    	    /* Hack -- skip leading space */
	    	    ++comment);
	}

	/* Print, in colour */
	text_out_c(line_attr, out_val);

	/* Display the spell description */
	text_out("\n\n   ");

	text_out(s_text + s_info[spell]);
	text_out_c(TERM_L_BLUE, "\n\n[Press any key to continue]\n");

	/* Wait for input */
	(void)anykey();

	/* Load screen */
	screen_load();
}
#endif


static bool item_tester_browsable(const object_type *o_ptr)
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

/* select a spell from book 'item'. return -1 if not successful */
s16b select_spell_from_book(int item)
{
	object_type *o_ptr;
	s16b count, i, j, k, spellno;
	s16b index[MAX_SPELLS_PER_ITEM];
	char buf;

	/* Get the item */
	o_ptr = &inventory[item];

	count = 0;
	for (i=0; i<MAX_SPELLS_PER_ITEM; i++)
	{
		if (has_spell(o_ptr, i)) index[count++]=i;
	}

	if (count==0)
	{
		msg_print("This book is empty.");
		return -1;
	}

	object_kind_track(o_ptr->k_idx);
	handle_stuff();

	/* Save the screen */
	(void)Term_save();

	print_spells(index, count);
   
	/* Clear the top line */
	prt("", 0, 0);

	buf = '\0';

	/* Prompt user */
	if (!get_com("Which spell (press @ for number)? ", &buf))
	{
		(void)Term_load();
		return -1;
	}
	if (buf == '@')
	{
		char buf2[20];

		buf2[0]='\0';
		if (!get_string("Enter spell number: ", buf2, 3))
		{
			(void)Term_load();
			return -1;
		}
		spellno=(s16b)atol(buf2);
		if (spellno < 0)
		{
			(void)Term_load();
			return -1;
		}
		for (j=0; j<INVEN_PACK; j++)
		{
			if (inventory[j].tval == TV_BOOK)
			{
				for (k = 0; k < z_info->s_max; k++)
				{
					if (has_spell(&inventory[j], spellno)) break;
				}
				if (k < z_info->s_max) break;
			}
		}
		if (j == INVEN_PACK)
		{
			msg_print(NULL);
			msg_print("That spell is not in in a book in your inventory.");
			return -1;
		}
	}
	else
	{
		spellno=(s16b)A2I(buf);
		if ((spellno < 0) || (spellno >= count))
		{
			(void)Term_load();
			return -1;
		}
		spellno=index[spellno];
	}
	(void)Term_load();
	return (spellno);
}

s16b select_spell_by_number(void)
{
	char buf2[4];
	s16b i, j, spellno;
	cptr p;

	buf2[0]='\0';

	p = "Enter spell number (globally over all your books): ";
	if (!get_string(p, buf2, 3)) return -1;
	spellno = (s16b) atoi(buf2);

	for (i = 0; i < INVEN_PACK; i++)
	{
		if (inventory[i].tval == TV_BOOK)
		{
			for (j=0; j<z_info->s_max; j++)
			{
				if (has_spell(&inventory[i], spellno)) break;
			}
		if (j < z_info->s_max) break;
		}
	}
	if (i == INVEN_PACK)
	{
		msg_print(NULL);
		msg_print("That spell is not in in a book in your inventory.");
		return -1;
	}
	/* this is necessary to prevent messing up the screen */
	prt("                                                            ", 0, 0);
	return spellno;
}

/*
 * Auxiliary function for "get_item()" -- test an index
 */
static bool get_item_okay(s16b i)
{
   /* Illegal items */
   if ((i < 0) ||
      (i > (INVEN_TOTAL - 1))) return (FALSE); /* TODO Handle objects on floor */

   /* Verify the item */
   return (item_tester_okay(&inventory[i]));
}

s16b count_items(int *first_item, bool equip, bool inven, bool floor)
{
	s16b i, result = 0;
	UNREFERENCED_PARAMETER(floor); /* TODO Handle floor */

	if (inven)
	{
		for (i=0; i<INVEN_PACK; i++)
		{
			if (get_item_okay(i))
			{
				result++;
				(*first_item)=i;
			}
		}
	}
	if (equip)
	{
		for (i=INVEN_WIELD; i<INVEN_TOTAL; i++)
		{
			if (get_item_okay(i))
			{
				result++;
				(*first_item)=i;
			}
		}
	}
/*	if (floor)
	{
		for (i=INVEN_TOTAL; i<objects_on_floor(px, py); i++)
		{
			if (get_item_okay(i))
			{
				result++;
				(*first_item)=i;
			}
		}
	} */ /* TODO Handle floor */
	return (result);
}

/* this function selects a spell, either by book or by number */
s16b select_spell(void)
{
   s16b spellno, count;
   int item;

   /* Restrict choices to scrolls */
   item_tester_tval = (byte)TV_BOOK;
   item_tester_hook = NULL;
   count = count_items(&item, FALSE, TRUE, TRUE);

   /* Get an item (from inven or floor) */ /* TODO Handle floor */
   if ( (count != 1) &&
        !get_item(&item, "Cast spell from what? ", "You have nothing to read spells from.", USE_INVEN))
   {
      item_tester_tval = (byte)0;
      if ( (item == -1) || (item == -2) ) return -1;
   }
   item_tester_tval = (byte)0;

   /* item -3 means @ means cast spell by number, never mind the book */
   if (item != -3)
   {
      spellno = select_spell_from_book(item);
   }
   else
   {
      spellno = select_spell_by_number();
   }
   return (spellno);
}

/*
 * Peruse the spells/prayers in a Book
 *
 * Note that *all* spells in the book are listed
 *
 * TODO Check this works!
 */
void do_cmd_browse_aux(const object_type *o_ptr)
{
	int item = 0;
	
	if(!obj_cast_pre())
		return;

   /* Restrict choices to "useful" books */
   /* and certain ego-item staffs, if you are a mage */
   item_tester_tval = 0;     /* no restrictions on type */
   item_tester_hook = item_tester_browsable;

   /* Get an item (from inven or floor) */
   if (!get_item(&item, "Browse which book? ", "You have no books that you can read.", (USE_INVEN | USE_FLOOR)))
   {
      item_tester_hook = NULL;
      return;
   }
   item_tester_hook = NULL;

   /* Get the item (in the pack) */
   o_ptr = &inventory[item];

   object_kind_track(o_ptr->k_idx);
   handle_stuff();

   browse_book(o_ptr);
   return;
}

/* Cast the specified spell */
bool spell_cast(int spell)
{
	int chance;
	const magic_type *s_ptr;

    cptr p = ((cp_ptr->spell_stat == A_INT) ? 
	          "cast this spell" :
	          "recite this prayer");


	/* Get the spell */
	s_ptr = &mp_ptr->info[spell];


	/* Verify "dangerous" spells */
	if (s_ptr->smana > p_ptr->csp)
	{
		/* Warning */
		msg_format("You do not have enough mana to %s.", p);

		/* Flush input */
		flush();

		/* Verify */
		if (!get_check("Attempt it anyway? ")) return FALSE;
	}


	/* Spell failure chance */
	chance = page_chance(spell);

	/* Failed spell */
	if (randint0(100) < chance)
	{
		if (OPT(flush_failure)) flush();
		msg_print("You failed to concentrate hard enough!");
	}

	/* Process spell */
	else
	{
		/* Cast the spell */
		if (!cast_spell(spell)) return FALSE;

		/* A spell was cast */
		sound(MSG_SPELL);
		if (!(p_ptr->spell_flags[spell] & PY_SPELL_WORKED))
		{
			int e = s_ptr->sexp;

			/* The spell worked */
			p_ptr->spell_flags[spell] |= PY_SPELL_WORKED;

			/* Gain experience */
			gain_exp(e * s_ptr->slevel);

			/* Redraw object recall */
			p_ptr->redraw |= (PR_OBJECT);
		}
	}

	/* Sufficient mana */
	if (s_ptr->smana <= p_ptr->csp)
	{
		/* Use some mana */
		p_ptr->csp -= s_ptr->smana;
	}

	/* Over-exert the player */
	else
	{
		int oops = s_ptr->smana - p_ptr->csp;

		/* No mana left */
		p_ptr->csp = 0;
		p_ptr->csp_frac = 0;

		/* Message */
		msg_print("You faint from the effort!");

		/* Hack -- Bypass free action */
		(void)inc_timed(TMD_PARALYZED, randint1(5 * oops + 1), TRUE);

		/* Damage CON (possibly permanently) */
		if (randint0(100) < 50)
		{
			bool perm = (randint0(100) < 25);

			/* Message */
			msg_print("You have damaged your health!");

			/* Reduce constitution */
			(void)dec_stat(A_CON, perm);
		}
	}

	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);

	return TRUE;
}

#if 0
/*							
 * Print a list of spells (for browsing or casting or viewing).							
 *							
 * Vanilla Angband version.							
 */							
static void print_spells(const byte *spells, int num, s16b y, s16b x)							
{							
	int i, spell;						
							
	const magic_type *s_ptr;						
							
	char help[20];						
	char out_val[160];						
							
	const char *comment = help;						
							
	byte line_attr;						
							
	/* Title the list */						
	prt("", y, x);						
	put_str("Name", y, x + 5);						
	put_str("Lv Mana Fail Info", y, x + 35);						
							
	/* Dump the spells */						
	for (i = 0; i < num; i++)						
	{						
		/* Get the spell index */					
		spell = spells[i];					
							
		/* Get the spell info */					
		s_ptr = &mp_ptr->info[spell];					
							
		/* Skip illegible spells */					
		if (s_ptr->slevel >= 99)					
		{					
			strnfmt(out_val, sizeof(out_val), "  %c) %-30s", I2A(i), "(illegible)");				
			c_prt(TERM_L_DARK, out_val, y + i + 1, x);				
			continue;				
		}					
							
		/* Get extra info */					
		get_spell_info(cp_ptr->spell_stat, spell, help, sizeof(help)); /* TODO This code is obsolete and needs to be replaced */					
							
		/* Assume spell is known and tried */					
		comment = help;					
		line_attr = TERM_WHITE;					
							
		/* Analyze the spell */					
		if (p_ptr->spell_flags[spell] & PY_SPELL_FORGOTTEN)					
		{					
			comment = " forgotten";				
			line_attr = TERM_YELLOW;				
		}					
		else if (!(p_ptr->spell_flags[spell] & PY_SPELL_LEARNED))					
		{					
			if (s_ptr->slevel <= p_ptr->lev)				
			{				
				comment = " unknown";			
				line_attr = TERM_L_BLUE;			
			}				
			else				
			{				
				comment = " difficult";			
				line_attr = TERM_RED;			
			}				
		}					
		else if (!(p_ptr->spell_flags[spell] & PY_SPELL_WORKED))					
		{					
			comment = " untried";				
			line_attr = TERM_L_GREEN;				
		}					
							
		/* Dump the spell --(-- */					
		strnfmt(out_val, sizeof(out_val), "  %c) %-30s%2d %4d %3d%%%s",					
		        I2A(i), get_spell_name(spell),					
		        s_ptr->slevel, s_ptr->smana, page_chance(spell), comment);					
		c_prt(line_attr, out_val, y + i + 1, x);					
	}						
							
	/* Clear the bottom line */						
	prt("", y + i + 1, x);						
}							
#endif							
