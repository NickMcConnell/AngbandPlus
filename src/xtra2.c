/* File: xtra2.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * 						Jeff Greene, Diego Gonzalez
 *
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
#include "cmds.h"
#include "game-cmd.h"
#include "ui-menu.h"
#include "game-event.h"


/* Players with chaos or confusion resistance don't get confused*/
bool allow_player_confusion(void)
{
	if (p_ptr->state.resist_confu) return (FALSE);
	if (p_ptr->state.resist_chaos) return (FALSE);

	/*Don't have the right resists*/
	return (TRUE);
}

s32b get_experience_by_level(int level)
{
	if (game_mode == GAME_NPPMORIA)
	{
		return (player_exp_nppmoria[level]);
	}

	return (player_exp_nppangband[level]);
}


/*
 * Advance experience levels and print experience
 */
void check_experience(void)
{
	/* Hack -- lower limit */
	if (p_ptr->exp < 0) p_ptr->exp = 0;

	/* Hack -- lower limit */
	if (p_ptr->max_exp < 0) p_ptr->max_exp = 0;

	/* Hack -- upper limit */
	if (p_ptr->exp > PY_MAX_EXP) p_ptr->exp = PY_MAX_EXP;

	/* Hack -- upper limit */
	if (p_ptr->max_exp > PY_MAX_EXP) p_ptr->max_exp = PY_MAX_EXP;

	/* Hack -- maintain "max" experience */
	if (p_ptr->exp > p_ptr->max_exp) p_ptr->max_exp = p_ptr->exp;

	/* Redraw experience */
	p_ptr->redraw |= (PR_EXP);

	/* Handle stuff */
	handle_stuff();

	/* Lose levels while possible */
	while ((p_ptr->lev > 1) &&
	       (p_ptr->exp < (get_experience_by_level(p_ptr->lev-2) * p_ptr->expfact / 100L)))
	{
		/* Lose a level */
		p_ptr->lev--;

		/* Update some stuff */
		p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);

		/* Redraw some stuff */
		p_ptr->redraw |= (PR_LEV | PR_TITLE | PR_MONSTER);

		/* Handle stuff */
		handle_stuff();
	}

	/* Gain levels while possible */
	while ((p_ptr->lev < z_info->max_level) &&
	       (p_ptr->exp >= (get_experience_by_level(p_ptr->lev-1) * p_ptr->expfact / 100L)))
	{
		/* Gain a level */
		p_ptr->lev++;

		/* Message */
		message_format(MSG_LEVEL, p_ptr->lev, "Welcome to level %d.", p_ptr->lev);

		/* Save the highest level*/
		if (p_ptr->lev > p_ptr->max_lev)
		{
	     	/* update the highest level*/
			p_ptr->max_lev = p_ptr->lev;

			/* If auto-note taking enabled, write a note to the file every 5th level. */
            if ((adult_take_notes) && ((p_ptr->lev % 5) == 0))

			{

                    char buf[120];

                   /* Build the message */
                   sprintf(buf, "Reached level %d", p_ptr->lev);

                   /* Write message */
                   do_cmd_note(buf,  p_ptr->depth);

           	}


		}

		/* Update some stuff */
		p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);

		/* Redraw some stuff */
		p_ptr->redraw |= (PR_LEV | PR_TITLE | PR_EXP | PR_MONSTER);

		/* Handle stuff */
		handle_stuff();
	}
	
	/* Gain max levels while possible */
	while ((p_ptr->max_lev < z_info->max_level) &&
	       (p_ptr->max_exp >= (get_experience_by_level(p_ptr->max_lev-1) *
	                           p_ptr->expfact / 100L)))
	{
		/* Gain max level */
		p_ptr->max_lev++;

		/* Update some stuff */
		p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);

		/* Redraw some stuff */
		p_ptr->redraw |= (PR_LEV | PR_TITLE);

		/* Handle stuff */
		handle_stuff();
	}

}


/*
 * Gain experience
 */
void gain_exp(s32b amount)
{

	/* Gain some experience */
	p_ptr->exp += amount;

	/* Slowly recover from experience drainage */
	if (p_ptr->exp < p_ptr->max_exp)
	{
		/* Gain max experience (10%) */
		p_ptr->max_exp += amount / 10;
	}

	/* Check Experience */
	check_experience();

}


/*
 * Lose experience
 */
void lose_exp(s32b amount)
{
	/* Never drop below zero experience */
	if (amount > p_ptr->exp) amount = p_ptr->exp;

	/* Lose some experience */
	p_ptr->exp -= amount;

	/* Check Experience */
	check_experience();
}




/*
 * Hack -- Return the "automatic coin type" of a monster race
 * Used to allocate proper treasure when "Creeping coins" die
 *
 * Note the use of actual "monster names".  XXX XXX XXX
 */
int get_coin_type(const monster_race *r_ptr)
{
	cptr name = r_ptr->name_full;

	/* Analyze "coin" or golem monsters */
	if ((r_ptr->d_char == '$') || (r_ptr->d_char == 'g'))
	{
		/* Look for textual clues */
		if (strstr(name, " copper ")) return (SV_GOLD_COPPER);
		if (strstr(name, " silver ")) return (SV_GOLD_SILVER);
		if (strstr(name, " garnet ")) return (SV_GOLD_GARNET);
		if (strstr(name, " gold ")) return (SV_GOLD_GOLD);
		if (strstr(name, " mithril ")) return (SV_GOLD_MITHRIL);
		if (strstr(name, " opal")) return (SV_GOLD_OPALS);
		if (strstr(name, " sapphire")) return (SV_GOLD_SAPPHIRES);
		if (strstr(name, " ruby")) return (SV_GOLD_RUBIES);
		if (strstr(name, " emerald"))return (SV_GOLD_EMERALD);
		if (strstr(name, " diamond")) return (SV_GOLD_DIAMOND);
		if (strstr(name, " adamantite ")) return (SV_GOLD_ADAMANTITE);

		/* Look for textual clues */
		if (strstr(name, " Copper ")) return (SV_GOLD_COPPER);
		if (strstr(name, " Silver ")) return (SV_GOLD_SILVER);
		if (strstr(name, " Garnet ")) return (SV_GOLD_GARNET);
		if (strstr(name, " Gold ")) return (SV_GOLD_GOLD);
		if (strstr(name, " Mithril ")) return (SV_GOLD_MITHRIL);
		if (strstr(name, " Opal")) return (SV_GOLD_OPALS);
		if (strstr(name, " Sapphire")) return (SV_GOLD_SAPPHIRES);
		if (strstr(name, " Ruby")) return (SV_GOLD_RUBIES);
		if (strstr(name, " Emerald"))    return (SV_GOLD_EMERALD);
		if (strstr(name, " Diamond")) return (SV_GOLD_DIAMOND);
		if (strstr(name, " Adamantite ")) return (SV_GOLD_ADAMANTITE);
	}

	/* Assume nothing */
	return (0);
}



/* Helper function for monster_death - drop any objects the monster is holding */
static void mon_drop_held_objects(monster_type *m_ptr)
{
	s16b this_o_idx, next_o_idx = 0;
	object_type *i_ptr;
	object_type object_type_body;

	/* Drop objects being carried */
	for (this_o_idx = m_ptr->hold_o_idx; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

		/* Get the object */
		o_ptr = &o_list[this_o_idx];

		/*Remove the mark to hide when monsters carry this object*/
		o_ptr->ident &= ~(IDENT_HIDE_CARRY);

		/* Get the next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Paranoia */
		o_ptr->held_m_idx = 0;

		/* Get local object */
		i_ptr = &object_type_body;

		/* Copy the object */
		object_copy(i_ptr, o_ptr);

		/* Delete the object */
		delete_object_idx(this_o_idx);

		/* Drop it */
		drop_near(i_ptr, -1, m_ptr->fy, m_ptr->fx);
	}

	/* Forget objects */
	m_ptr->hold_o_idx = 0;
}

/*
 * Helper function for monster_death -
 * Intended only to drop Morgoth's special artifacts
 */
static void mon_drop_chosen_objects(monster_type *m_ptr)
{
	object_type *i_ptr;
	object_type object_type_body;

	/* Get local object */
	i_ptr = &object_type_body;

	/* Mega-Hack -- Prepare to make "Grond" */
	object_prep(i_ptr, lookup_kind(TV_HAFTED, SV_GROND));

	/* Mega-Hack -- Mark this item as "Grond" */
	i_ptr->art_num = ART_GROND;

	/* Mega-Hack -- Actually create "Grond" */
	apply_magic(i_ptr, -1, TRUE, TRUE, TRUE, FALSE);

	/* Remember history */
	object_history(i_ptr, ORIGIN_MORGOTH, 0);

	/* Drop it in the dungeon */
	drop_near(i_ptr, -1, m_ptr->fy, m_ptr->fx);

	/* Get local object */
	i_ptr = &object_type_body;

	/* Mega-Hack -- Prepare to make "Morgoth's crown" */
	object_prep(i_ptr, lookup_kind(TV_CROWN, SV_MORGOTH));

	/* Mega-Hack -- Mark this item as "Morgoth" */
	i_ptr->art_num = ART_MORGOTH;

	/* Mega-Hack -- Actually create "Morgoth" */
	apply_magic(i_ptr, -1, TRUE, TRUE, TRUE, FALSE);

	/* Remember history */
	object_history(i_ptr, ORIGIN_MORGOTH, 0);

	/* Drop it in the dungeon */
	drop_near(i_ptr, -1, m_ptr->fy, m_ptr->fx);
}

/*
 * Helper function for monster_death -
 * Drop the monster's normal objects
 */
static void mon_drop_loot(int m_idx)
{
	monster_type *m_ptr = &mon_list[m_idx];

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	int j;
	bool chest = (r_ptr->flags1 & (RF1_DROP_CHEST)) ? TRUE : FALSE;
	bool good = (r_ptr->flags1 & (RF1_DROP_GOOD)) ? TRUE : FALSE;
	bool great = (r_ptr->flags1 & (RF1_DROP_GREAT)) ? TRUE : FALSE;

	bool do_gold = (!(r_ptr->flags1 & (RF1_ONLY_ITEM)));
	bool do_item = (!(r_ptr->flags1 & (RF1_ONLY_GOLD)));
	bool visible = (m_ptr->ml || (r_ptr->flags1 & (RF1_UNIQUE)));

	int force_coin = get_coin_type(r_ptr);

	int dump_item = 0;
	int dump_gold = 0;

	int number_drops = 0;

	object_type *i_ptr;
	object_type object_type_body;

	/* Average dungeon and monster levels */
	s16b set_object_level = object_level = (effective_depth(p_ptr->depth) + r_ptr->level) / 2;

	/* Determine how much we can drop */
	if ((r_ptr->flags1 & (RF1_DROP_60)) && (rand_int(100) < 60)) number_drops++;
	if ((r_ptr->flags1 & (RF1_DROP_90)) && (rand_int(100) < 90)) number_drops++;
	if (r_ptr->flags1 & (RF1_DROP_1D2)) number_drops += damroll(1, 2);
	if (r_ptr->flags1 & (RF1_DROP_2D2)) number_drops += damroll(2, 2);
	if (r_ptr->flags1 & (RF1_DROP_3D2)) number_drops += damroll(3, 2);
	if (r_ptr->flags1 & (RF1_DROP_4D2)) number_drops += damroll(4, 2);

	/* Hack -- handle creeping coins */
	coin_type = force_coin;

	/* Drop some objects */
	for (j = 0; j < number_drops; j++)
	{
		bool interesting = FALSE;

		/* Re-set the object level */
		object_level = set_object_level;

		/* Get local object */
		i_ptr = &object_type_body;

		/* Wipe the object */
		object_wipe(i_ptr);

		/* work on the "too much junk" problem, large drops sometimes are less items with a "boost". */
		if ((randint(750) < (number_drops * number_drops)) && (!(r_ptr->flags1 & (RF1_UNIQUE))))
		{
			interesting = TRUE;
			number_drops -= 5;
			object_level += 5;

			/*Boundry Control*/
			if (number_drops < 0) number_drops = 0;
			if (object_level > MAX_DEPTH) object_level = MAX_DEPTH;
		}

		/* Make Gold */
		if (do_gold && (!chest) && (!do_item || (rand_int(100) < 50)))
		{
			/* Make some gold */
			if (!make_gold(i_ptr)) continue;

			/* Assume seen XXX XXX XXX */
			dump_gold++;
		}

		/* Make Object */
		else
		{
			if (chest)
			{
				if (!make_object(i_ptr, good, great, DROP_TYPE_CHEST, FALSE)) continue;
			}

			/* Make an object */
			else if (!make_object(i_ptr, good, great, DROP_TYPE_UNTHEMED, interesting)) continue;

			/* Remember history */
			if (visible) object_history(i_ptr, ORIGIN_DROP_KNOWN, m_ptr->r_idx);
			else object_history(i_ptr, ORIGIN_DROP_UNKNOWN, 0);

			/* Assume seen XXX XXX XXX */
			dump_item++;
		}

		/* Drop it in the dungeon */
		drop_near(i_ptr, -1, m_ptr->fy, m_ptr->fx);
	}

	/* Re-set the object level */
	object_level = set_object_level;

	/*If marked for a bonus item, create it and drop it */
	if (m_ptr->mflag & (MFLAG_BONUS_ITEM))
	{
		bool this_good = good;
		bool this_great = great;
		bool this_chest = chest;
		bool interesting = FALSE;

		char o_name[80];

		/* Get local object */
		i_ptr = &object_type_body;

		/* Wipe the object */
		object_wipe(i_ptr);

		if (one_in_(50)) this_chest = TRUE;
		if (one_in_(15)) this_great = TRUE;
		if (one_in_(5)) this_good = TRUE;
		if ((!this_good) && (!this_great) && (!this_chest))
		{
			object_level += 5;
			if (object_level > MAX_DEPTH) object_level = MAX_DEPTH;
			interesting = TRUE;
		}

		if (this_chest)
		{
			while (!make_object(i_ptr, TRUE, TRUE, DROP_TYPE_CHEST, FALSE)) continue;
		}

		/* Make an object */
		else while (!make_object(i_ptr, this_good, this_good, DROP_TYPE_UNTHEMED, interesting)) continue;

		/* Remember history */
		if (visible) object_history(i_ptr, ORIGIN_DROP_KNOWN, m_ptr->r_idx);
		else object_history(i_ptr, ORIGIN_DROP_UNKNOWN, 0);

		object_desc(o_name, sizeof(o_name), i_ptr, ODESC_PREFIX | ODESC_FULL);

		/* Drop it in the dungeon */
		drop_near(i_ptr, -1, m_ptr->fy, m_ptr->fx);


	}

	/* Reset the object level */
	object_level = effective_depth(p_ptr->depth);

	/* Reset "coin" type */
	coin_type = 0;

	/* Take note of any dropped treasure */
	if (visible && (dump_item || dump_gold))
	{
		/* Take notes on treasure */
		lore_treasure(m_idx, dump_item, dump_gold);
	}

}

/*
 * Helper function for monster-death.
 * Process the death of a quest monster.
 */
static void process_quest_monster_death(int i, int m_idx, bool *writenote)
{
	quest_type *q_ptr = &q_info[i];
	monster_type *m_ptr = &mon_list[m_idx];

	/* Not the right monster race for certain quests */
	if (quest_single_r_idx(q_ptr) || quest_fixed(q_ptr))
	{
		if (q_ptr->mon_idx != m_ptr->r_idx) return;
	}

	/* Not a quest that counts monster deaths */
	else if (quest_multiple_r_idx(q_ptr))
	{
		if (!(m_ptr->mflag & (MFLAG_QUEST))) return;
	}

	else if (quest_timed(q_ptr))
	{
		if (m_ptr->mflag & (MFLAG_QUEST))
		{
			q_ptr->q_num_killed++;
			p_ptr->redraw |= (PR_QUEST_ST);
			p_ptr->notice |= PN_QUEST_REMAIN;
		}
		return;
	}

	else return;

	/* Mark kills */
	q_ptr->q_num_killed++;

	/* Completed quest? */
	if (q_ptr->q_num_killed >= q_ptr->q_max_num)
	{
		/* Mark complete */
		quest_finished(q_ptr);

		/*
		 * Make a note of the completed quest, but not for fixed quests.
		 * That is a special note written later.
		 */
		if (!quest_fixed(q_ptr))
		{
			write_quest_note(TRUE);
			*writenote = FALSE;
		}
	}

	/*not done yet*/
	if (!(q_ptr->q_flags & (QFLAG_COMPLETED))) p_ptr->notice |= PN_QUEST_REMAIN;

	/* Update the quest status */
	p_ptr->redraw |= (PR_QUEST_ST);
}

/*
 * Handle the "death" of a monster.
 *
 * Disperse treasures centered at the monster location based on the
 * various flags contained in the monster flags fields.
 *
 * Check for "Quest" completion when a quest monster is killed.
 *
 * Note that only the player can induce "monster_death()" on Uniques or quest monsters.
 *
 * Note that monsters can now carry objects, and when a monster dies,
 * it drops all of its objects, which may disappear in crowded rooms.
 */
void monster_death(int m_idx, int who)
{
	int i;
	int total = 0;
	bool questlevel = FALSE;
	bool completed = FALSE;
	bool fixedquest = FALSE;
	bool writenote = TRUE;

	monster_type *m_ptr = &mon_list[m_idx];

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	/* Drop any objects the monster is carrying */
	if (m_ptr->hold_o_idx)
	{
		mon_drop_held_objects(m_ptr);
	}

	/* Mega-Hack -- drop "winner" treasures */
	if (r_ptr->flags1 & (RF1_DROP_CHOSEN))
	{
		mon_drop_chosen_objects(m_ptr);
	}

	/* Drop the monster's standard loot */
	mon_drop_loot(m_idx);

	/* Update monster list window */
	p_ptr->redraw |= (PR_MONLIST);

	/* Count incomplete quests */
	for (i = 0; i < z_info->q_max; i++)
	{
		quest_type *q_ptr = &q_info[i];

		/*
		 * Hack - don't count if player didn't kill, or on a town level
		 * This assumes only a player can kill quest monsters!!!!!
		 */
		if (((who != SOURCE_PLAYER) && (who != SOURCE_TRAP)) || (!p_ptr->depth)) continue;

		/* Quest level? */
		if ((q_ptr->base_level == p_ptr->depth) && !is_quest_complete(i))
		{
			/* We are on a quest level */
			questlevel = TRUE;

			/* Mark fixed quests */
			if (quest_fixed(q_ptr)) fixedquest = TRUE;

			process_quest_monster_death(i, m_idx, &writenote);

			/* We just completed the quest */
			if (q_ptr->q_flags & (QFLAG_COMPLETED))
			{
				completed = TRUE;
			}
		}

		/* Count remaining permanent quests */
		if (quest_fixed(q_ptr))
		{
			if (!is_quest_complete(i)) total++;
		}
	}

	/* If the player kills a Unique, and the notes option is on, write a note.
	 * If the unique is a guild questor, the note was already written */
   	if ((r_ptr->flags1 & (RF1_UNIQUE)) && (adult_take_notes) && (writenote))
	{

		char note2[120];
 		char real_name[120];

		/*write note for player ghosts*/
		if (r_ptr->flags2 & (RF2_PLAYER_GHOST))
		{
			/*paranoia*/
			/* Check there is a name/ghost first */
			if (player_ghost_name[0] == '\0')
			{
				/*Make sure the name has been created*/
				prepare_ghost_name();
			}

			my_strcpy(note2, format("Destroyed %^s", player_ghost_name), sizeof (note2));
		}

		/*All other uniques*/
		else
		{
			/* Get the monster's real name for the notes file */
			monster_desc_race(real_name, sizeof(real_name), m_ptr->r_idx);

			/* Write note */
       		if monster_nonliving(r_ptr) my_strcpy(note2, format("Destroyed %s", real_name), sizeof (note2));
			else my_strcpy(note2, format("Killed %s", real_name), sizeof (note2));
		}

 		do_cmd_note(note2, p_ptr->depth);
	}

	/* Only process dungeon kills */
	if (!p_ptr->depth) return;

	/* Hack, check if the Balrog of Moria just died in NPPMoria */
	if (game_mode == GAME_NPPMORIA)
	{
		/* Only the Balrog of Moria should have this flag */
		if (!(r_ptr->flags1 & (RF1_QUESTOR))) return;

		/* Set the variables so the game knows we are done */
		questlevel = TRUE;
		completed = TRUE;
		fixedquest = TRUE;
		total = FALSE;
	}


	/* Require a quest level */
	if (!questlevel) return;

	/* Require all quests on this level to be completed */
	if (!completed) return;

	/* Check quest type */
	if (!fixedquest)
	{
		/* Give a message */
		msg_print("You have completed your quest - collect your reward at the guild!");

		/* Turn on quest indicator */
		quest_indicator_timer = 50;
		quest_indicator_complete = TRUE;

		/* Redraw the status */
		p_ptr->redraw |= (PR_QUEST_ST);

		return;
	}

	/* Need some stairs */
	else if (total)
	{
		p_ptr->q_fame += 150;
		altered_inventory_counter += 50;
	}

 	/* Nothing left, game over... */
	else
 	{

		/* Total winner */
 		p_ptr->total_winner = TRUE;

 		/* Redraw the "title" */
 		p_ptr->redraw |= (PR_TITLE);

		p_ptr->q_fame += 500;
		altered_inventory_counter += 200;

 		/* Congratulations */
 		msg_print("*** CONGRATULATIONS ***");
 		msg_print("You have won the game!");
 		msg_print("You may retire (commit suicide) when you are ready.");

 		/* Write a note, if that option is on */
  		if (adult_take_notes)
 		{
	 		/* Variable for the date */
 			time_t ct = time((time_t*)0);
 			char long_day[25];
			file_putf(notes_file, "============================================================\n");
  		    (void)strftime(long_day, 25, "%m/%d/%Y at %I:%M %p", localtime(&ct));
  		    if (game_mode == GAME_NPPMORIA) file_putf(notes_file, "{{full_character_name}} slew The Balrog of Moria on %s.\n", long_day);
  		    else file_putf(notes_file, "{{full_character_name}} slew Morgoth on %s.\n", long_day);
 			file_putf(notes_file, "Long live {{full_character_name}}!\n");
 		    file_putf(notes_file, "Long live {{full_character_name}}!\n");
			file_putf(notes_file, "============================================================\n");
      	}
	}
}


/*Helper function to calculate the monster experience*/
static s32b calc_mon_exp(const monster_race *r_ptr)
{
	/*calculate the monster experience*/
	s32b new_exp = ((long)r_ptr->mexp * r_ptr->level) / p_ptr->lev;

	s16b new_level = p_ptr->max_lev;

	/*not a full point of experience to gain*/
	if (new_exp < 1) return (0);

	/*
	 * Check to make sure player is at level 50, so no adjustmetn necessary,
	 * also prevents next line from crashing the game
	 */
	while (new_level < z_info->max_level)
	{
		s32b remaining_exp;
		s32b net_exp_gain;

		/*
		 * Player is not gaining a new max level
		 * (in the player_exp chart level 1 exp-to-gain-next-level is at slot 0)
		 */
		if ((p_ptr->exp + new_exp) <= (get_experience_by_level(new_level-1)) * p_ptr->expfact / 100L) break;

		/*just checking this again*/
		if (new_exp < 1) break;

		/*figure out the remainder*/
		net_exp_gain = (p_ptr->exp + new_exp) - (get_experience_by_level(new_level-1) * p_ptr->expfact / 100L);

		/*add one level*/
		new_level++;

		/*player is going up a max level, adjust*/
		remaining_exp = ((long)net_exp_gain * (new_level - 1)) / new_level;

		/*slightly reduce new experience*/
		new_exp -= (net_exp_gain - remaining_exp);
	}

	return (new_exp);
}

/*
 * Decrease a monster's hit points, handle monster death.
 *
 * We return TRUE if the monster has been killed (and deleted).
 *
 * We announce monster death (using an optional "death message"
 * if given, and a otherwise a generic killed/destroyed message).
 *
 * Only "physical attacks" can induce the "You have slain" message.
 * Missile and Spell attacks will induce the "dies" message, or
 * various "specialized" messages.  Note that "You have destroyed"
 * and "is destroyed" are synonyms for "You have slain" and "dies".
 *
 * Invisible monsters induce a special "You have killed it." message.
 *
 * Hack -- we "delay" fear messages by passing around a "fear" flag.
 *
 * Consider decreasing monster experience over time, say, by using
 * "(m_exp * m_lev * (m_lev)) / (p_lev * (m_lev + n_killed))" instead
 * of simply "(m_exp * m_lev) / (p_lev)", to make the first monster
 * worth more than subsequent monsters.  This would also need to
 * induce changes in the monster recall code.  XXX XXX XXX
 */
bool mon_take_hit(int m_idx, int dam, bool *fear, cptr note, int who)
{
	monster_type *m_ptr = &mon_list[m_idx];

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	s32b new_exp, new_exp_frac;

	/* Redraw (later) if needed */
	if ((p_ptr->health_who == m_idx) || (m_ptr->sidebar)) p_ptr->redraw |= (PR_HEALTH);

	/* Allow the debugging of damage done. */
	if ((dam > 0) && (p_ptr->wizard))
	{
		if ((who == SOURCE_PLAYER) || (who == SOURCE_TRAP))
		{
			msg_format("You do %d (out of %d) damage.", dam, m_ptr->hp);
		}
		else msg_format("%d (out of %d) damage has been done.", dam, m_ptr->hp);

	}

	/* Wake it up */
	wake_monster_attack(m_ptr, MON_TMD_FLG_NOMESSAGE);

	/* Hurt it */
	m_ptr->hp -= dam;

	/* Update the monster list */
	p_ptr->redraw |= PR_MONLIST;

	/* It is dead now */
	if (m_ptr->hp < 0)
	{
		char m_name[80];

		/* Assume normal death sound */
		int soundfx = MSG_KILL;

		/* Play a special sound if the monster was unique */
		if (r_ptr->flags1 & RF1_UNIQUE)
		{
			/* Mega-Hack -- Morgoth -- see monster_death() */
			if (r_ptr->flags1 & RF1_DROP_CHOSEN)
				soundfx = MSG_KILL_KING;
			else
				soundfx = MSG_KILL_UNIQUE;
		}

		/* Extract monster name */
		monster_desc(m_name, sizeof(m_name), m_ptr, 0);

		/* Increase the noise level slightly. */
		if (add_wakeup_chance <= 8000) add_wakeup_chance += 300;

		/* Death by Missile/Spell attack */
		if (note)
		{
			/* Hack -- allow message suppression */
			if (strlen(note) <= 1)
			{
				/* Be silent */
			}

			else
			{
				message_format(soundfx, m_ptr->r_idx, "%^s%s", m_name, note);
			}

		}

		/* Death by physical attack -- invisible monster */
		else if (!m_ptr->ml)
		{
			if ((who == SOURCE_PLAYER) || (who == SOURCE_TRAP))
			{
				message_format(soundfx, m_ptr->r_idx, "You have killed %s.", m_name);
			}
			else message_format(soundfx, m_ptr->r_idx, "%^s has been killed.", m_name);
		}

		/* Death by Physical attack -- non-living monster */
		else if (monster_nonliving(r_ptr))
		{
			if ((who == SOURCE_PLAYER) || (who == SOURCE_TRAP))
			{
				message_format(soundfx, m_ptr->r_idx, "You have destroyed %s.", m_name);
			}
			else message_format(soundfx, m_ptr->r_idx, "%^s has been destroyed", m_name);
		}

		/* Death by Physical attack -- living monster */
		else
		{
			if ((who == SOURCE_PLAYER) || (who == SOURCE_TRAP))
			{
				message_format(soundfx, m_ptr->r_idx, "You have slain %s.", m_name);
			}
			else message_format(soundfx, m_ptr->r_idx, "%^s has been slain", m_name);
		}

		if ((who == SOURCE_PLAYER) || (who == SOURCE_TRAP))
		{

			/* Give some experience for the kill */
			new_exp = calc_mon_exp(r_ptr);

			/* Handle fractional experience */
			new_exp_frac = ((((long)r_ptr->mexp * r_ptr->level) % p_ptr->lev)
						 * 0x10000L / p_ptr->lev) + p_ptr->exp_frac;

			/* Keep track of experience */
			if (new_exp_frac >= 0x10000L)
			{
				new_exp++;
				p_ptr->exp_frac = (u16b)(new_exp_frac - 0x10000L);
			}
			else
			{
				p_ptr->exp_frac = (u16b)new_exp_frac;
			}

			/* Gain experience */
			gain_exp(new_exp);
		}

		/* Generate treasure */
		monster_death(m_idx, who);

		/* When the player kills a Unique, it stays dead */
		if (r_ptr->flags1 & (RF1_UNIQUE))
		{
			/* This is the "evil iggy" exception in Moria */
			if ((r_ptr->flags2 & (RF2_SPECIAL)) && !(r_ptr->flags1 & (RF1_QUESTOR)))
			{
				/* Do nothing.....yes, I know this is bad coding */
			}

			else r_ptr->max_num = 0;

			/* reputation bonus, except for the town unique */
	    	if ((who == SOURCE_PLAYER) || (who == SOURCE_TRAP))
	    	{
	    		if (r_ptr->level >= p_ptr->lev)p_ptr->q_fame += 5;
	    		altered_inventory_counter += 2;
	    	}
		}

		/* When the player kills a player ghost, the template needs to be deleted.
		 */
		if ((r_ptr->flags2 & (RF2_PLAYER_GHOST)) &&
			((who == SOURCE_PLAYER) || (who == SOURCE_TRAP)))
		{
			/* fame boost*/
			p_ptr->q_fame += 7;
			altered_inventory_counter += 5;
			delete_player_ghost_entry();
		}

		/* Recall even invisible uniques or winners */
		if (m_ptr->ml || (r_ptr->flags1 & (RF1_UNIQUE)))
		{
			/* Count kills this life */
			if (l_ptr->pkills < MAX_SHORT) l_ptr->pkills++;

			/* Count kills in all lives */
			if (l_ptr->tkills < MAX_SHORT) l_ptr->tkills++;

			/* Hack -- Auto-recall */
			monster_race_track(m_ptr->r_idx);
		}

		/* Delete the monster */
		delete_monster_idx(m_idx);

		/* Not afraid */
		(*fear) = FALSE;

		/* Monster is dead */
		return (TRUE);
	}

	/* Sometimes a monster gets scared by damage */
	if (!m_ptr->m_timed[MON_TMD_FEAR] && ((r_ptr->flags3 & (RF3_NO_FEAR)) == 0) && (dam > 0))
	{
		int percentage;

		/* Percentage of fully healthy */
		percentage = (100L * m_ptr->hp) / m_ptr->maxhp;

		/*
		 * Run (sometimes) if at 10% or less of max hit points,
		 * or (usually) when hit for half its current hit points
		 */
		if ((randint(10) >= percentage) ||
		    ((dam >= m_ptr->hp) && (!one_in_(5))))
		{
			int fear_amt;

			/* Hack -- note fear */
			(*fear) = TRUE;

			/* Hack -- Add some timed fear */
			fear_amt = rand_range(20, 30) + dam / 5;
			mon_inc_timed(m_idx, MON_TMD_FEAR, fear_amt, MON_TMD_FLG_NOMESSAGE);

			/*a monster can't be wary and afraid*/
			m_ptr->mflag &= ~(MFLAG_WARY);

		}
	}

	/* Monster will always go active */
	m_ptr->mflag |= (MFLAG_ACTV);

	/* Recalculate desired minimum range */
	if (dam > 0) m_ptr->min_range = 0;

	/* Not dead yet */
	return (FALSE);
}


/*
 * Modify the current panel to the given coordinates, adjusting only to
 * ensure the coordinates are legal, and return TRUE if anything done.
 *
 * The town should never be scrolled around.
 *
 * Note that monsters are no longer affected in any way by panel changes.
 *
 * As a total hack, whenever the current panel changes, we assume that
 * the "overhead view" window should be updated.
 */
bool modify_panel(term *t, int wy, int wx)
{
	int dungeon_hgt = p_ptr->cur_map_hgt;
	int dungeon_wid = p_ptr->cur_map_wid;

	int screen_hgt = (t == Term) ? (t->hgt - ROW_MAP - 1) : t->hgt;
	int screen_wid = (t == Term) ? (t->wid - COL_MAP - 1) : t->wid;

	/* Bigtile panels only have half the width */
	if (use_bigtile) screen_wid = screen_wid / 2;

	/* Verify wy, adjust if needed */
	if (wy > dungeon_hgt - screen_hgt) wy = dungeon_hgt - screen_hgt;
	if (wy < 0) wy = 0;

	/* Verify wx, adjust if needed */
	if (wx > dungeon_wid - screen_wid) wx = dungeon_wid - screen_wid;
	if (wx < 0) wx = 0;


	/* React to changes */
	if ((t->offset_y != wy) || (t->offset_x != wx))
	{
		/* Save wy, wx */
		t->offset_y = wy;
		t->offset_x = wx;

		/* Redraw map */
		p_ptr->redraw |= (PR_MAP);

		/* Changed */
		return (TRUE);
	}

	/* No change */
	return (FALSE);

}



/*
 * Change the current panel to the panel lying in the given direction.
 *
 * Return TRUE if the panel was changed.
 */
bool change_panel(int dir)
{
	bool changed = FALSE;
	int j;

	/* Scan windows */
	for (j = 0; j < ANGBAND_TERM_MAX; j++)
	{
		int screen_hgt, screen_wid;
		int wx, wy;

		term *t = angband_term[j];

		/* No window */
		if (!t) continue;

		/* No relevant flags */
		if ((j > 0) && !(op_ptr->window_flag[j] & PW_MAP)) continue;

		screen_hgt = (j == 0) ? (Term->hgt - ROW_MAP - 1) : t->hgt;
		screen_wid = (j == 0) ? (Term->wid - COL_MAP - 1) : t->wid;

		/* Bigtile panels only have half the width */
		if (use_bigtile) screen_wid = screen_wid / 2;

		/* Shift by half a panel */
		wy = t->offset_y + ddy[dir] * screen_hgt / 2;
		wx = t->offset_x + ddx[dir] * screen_wid / 2;

		/* Use "modify_panel" */
		if (modify_panel(t, wy, wx)) changed = TRUE;
	}

	return (changed);
}

void verify_panel(void)
{
	verify_panel_int(center_player);
}

void center_panel(void)
{
	verify_panel_int(TRUE);
}


/*
 * Verify the current panel (relative to the player location).
 *
 * By default, when the player gets "too close" to the edge of the current
 * panel, the map scrolls one panel in that direction so that the player
 * is no longer so close to the edge.
 *
 * The "center_player" option allows the current panel to always be centered
 * around the player, which is very expensive, and also has some interesting
 * gameplay ramifications.
 */
void verify_panel_int(bool centered)
{
	int wy, wx;
	int screen_hgt, screen_wid;

	int panel_wid, panel_hgt;

	int py = p_ptr->py;
	int px = p_ptr->px;

	int j;

	/* Scan windows */
	for (j = 0; j < ANGBAND_TERM_MAX; j++)
	{
		term *t = angband_term[j];

		/* No window */
		if (!t) continue;

		/* No relevant flags */
		if ((j > 0) && !(op_ptr->window_flag[j] & (PW_MAP))) continue;

		wy = t->offset_y;
		wx = t->offset_x;

		screen_hgt = (j == 0) ? (Term->hgt - ROW_MAP - 1) : t->hgt;
		screen_wid = (j == 0) ? (Term->wid - COL_MAP - 1) : t->wid;

		/* Bigtile panels only have half the width */
		if (use_bigtile) screen_wid = screen_wid / 2;

		panel_wid = screen_wid / 2;
		panel_hgt = screen_hgt / 2;

		/* Scroll screen vertically when off-center */
		if (center_player && (!p_ptr->running) &&
		    (py != wy + panel_hgt))
		{
			wy = py - panel_hgt;
		}

		/* Scroll screen vertically when 2 grids from top/bottom edge */
		else if ((py < wy + panel_change_offset_y) || (py >= wy + screen_hgt - panel_change_offset_y))
		{
			wy = py - panel_hgt;
		}

		/* Scroll screen horizontally when off-center */
		if (center_player && (!p_ptr->running) &&
		    (px != wx + panel_wid))
		{
			wx = px - panel_wid;
		}

		/* Scroll screen horizontally when 4 grids from left/right edge */
		else if ((px < wx + panel_change_offset_x) || (px >= wx + screen_wid - panel_change_offset_x))
		{
			wx = px - panel_wid;
		}

		/* Scroll if needed */
		if ((wy != t->offset_y) || (wx != t->offset_x)) modify_panel(t, wy, wx);
	}
}


/*** Targetting Code ***/



/*
 * Given a "source" and "target" location, extract a "direction",
 * which will move one step from the "source" towards the "target".
 *
 * Note that we use "diagonal" motion whenever possible.
 *
 * We return "5" if no motion is needed.
 */
int motion_dir(int y1, int x1, int y2, int x2)
{
	/* No movement required */
	if ((y1 == y2) && (x1 == x2)) return (5);

	/* South or North */
	if (x1 == x2) return ((y1 < y2) ? 2 : 8);

	/* East or West */
	if (y1 == y2) return ((x1 < x2) ? 6 : 4);

	/* South-east or South-west */
	if (y1 < y2) return ((x1 < x2) ? 3 : 1);

	/* North-east or North-west */
	if (y1 > y2) return ((x1 < x2) ? 9 : 7);

	/* Paranoia */
	return (5);
}


/**
 * Extract a direction (or zero) from a mousepress
 */
extern int mouse_dir(ui_event_data ke, bool locating)
{
	int i, y, x;

	int gy = KEY_GRID_Y(ke), gx = KEY_GRID_X(ke);

	int py = p_ptr->py, px = p_ptr->px;

	if (locating)
	{
		gy = ke.mousey;
		gx = ke.mousex;
		py = Term->hgt / 2;
		px = Term->wid / 2;
	}

	y = ABS(gy - py);
	x = ABS(gx - px);

	if ((y == 0) && (x == 0)) return (0);

	/* Click next to player */
	if ((y <= 1) && (x <= 1))
	{
		/* Get the direction */
		for (i = 0; i < 10; i++)
		{
			if ((ddx[i] == (gx - px)) && (ddy[i] == (gy - py))) break;
		}

		/* Take it */
		return (i);
	}

	if (2 * y < x)
	{
		y = 0;
		x = (x / (gx - px));
	}
	else if (2 * x < y)
	{
		x = 0;
		y = (y / (gy - py));
	}
	else
	{
		y = (y / (gy - py));
		x = (x / (gx - px));
	}

	for (i = 0; i < 10; i++)
	{
		if ((ddy[i] == y) && (ddx[i] == x)) break;
	}

	/* paranoia */
	if ((i % 5) == 0) i = 0;

	return (i);
}




/*
 * Extract a direction (or zero) from a character
 */
int target_dir(char ch)
{
	int d = 0;

	int mode;

	cptr act;

	cptr s;


	/* Already a direction? */
	if (isdigit((unsigned char)ch))
	{
		d = D2I(ch);
	}
	else if (isarrow(ch))
	{
		switch (ch)
		{
			case ARROW_DOWN:	d = 2; break;
			case ARROW_LEFT:	d = 4; break;
			case ARROW_RIGHT:	d = 6; break;
			case ARROW_UP:		d = 8; break;
		}
	}
	else
	{
		/* Roguelike */
		if (rogue_like_commands)
		{
			mode = KEYMAP_MODE_ROGUE;
		}

		/* Original */
		else
		{
			mode = KEYMAP_MODE_ORIG;
		}

		/* Extract the action (if any) */
		act = keymap_act[mode][(byte)(ch)];

		/* Analyze */
		if (act)
		{
			/* Convert to a direction */
			for (s = act; *s; ++s)
			{
				/* Use any digits in keymap */
				if (isdigit((unsigned char)*s)) d = D2I(*s);
			}
		}
	}

	/* Paranoia */
	if (d == 5) d = 0;

	/* Return direction */
	return (d);
}



int dir_transitions[10][10] =
{
	/* 0-> */ { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 },
	/* 1-> */ { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
	/* 2-> */ { 0, 0, 2, 0, 1, 0, 3, 0, 5, 0 },
	/* 3-> */ { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
	/* 4-> */ { 0, 0, 1, 0, 4, 0, 5, 0, 7, 0 },
	/* 5-> */ { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
	/* 6-> */ { 0, 0, 3, 0, 5, 0, 6, 0, 9, 0 },
	/* 7-> */ { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
	/* 8-> */ { 0, 0, 5, 0, 7, 0, 9, 0, 8, 0 },
	/* 9-> */ { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
};


/*
 * Get an "aiming direction" (1,2,3,4,6,7,8,9 or 5) from the user.
 *
 * Return TRUE if a direction was chosen, otherwise return FALSE.
 *
 * The direction "5" is special, and means "use current target".
 *
 * This function tracks and uses the "global direction", and uses
 * that as the "desired direction", if it is set.
 *
 * Note that "Force Target", if set, will pre-empt user interaction,
 * if there is a usable target already set.
 *
 * Currently this function applies confusion directly.
 */
bool get_aim_dir(int *dp, bool target_trap)
{
	/* Global direction */
	int dir = p_ptr->command_dir;

	ui_event_data ke;

	cptr p;

	/* Initialize */
	(*dp) = 0;

	/* Make some buttons */
	button_backup_all();
	button_kill_all();
	button_add("[ESCAPE]", ESCAPE);
	button_add("[CUR_TARGET]", 't');
	button_add("[CLOSEST]", 'c');
	event_signal(EVENT_MOUSEBUTTONS);

	/* Hack -- auto-target if requested */
	if (use_old_target && target_okay() && !dir) dir = 5;

	/* Ask until satisfied */
	while (!dir)
	{
		/* Choose a prompt */
		if (!target_okay())
			p = "Direction ('*' or <click> to target, 'c' for closest, Escape to cancel)? ";
		else
			p = "Direction ('5' for target, '*' or <click> to re-target, Escape to cancel)? ";

		/* Get a command (or Cancel) */
		if (!get_com_ex(p, &ke)) break;

		/* Analyze */
		switch (ke.key)
		{
			/* Mouse aiming */
			case DEFINED_XFF:
			{

				if (click_area(ke) == SIDEBAR_MON_MIN)
				{

					int m_idx = find_sidebar_mon_idx(ke);

					if (m_idx)
					{
						monster_type *m_ptr = &mon_list[m_idx];

						if (m_ptr->project)
						{
							health_track(m_idx);
							target_set_monster(m_idx);
							dir = 5;
							break;
						}
					}
				}

				else if (target_set_interactive(TARGET_KILL, KEY_GRID_X(ke), KEY_GRID_Y(ke)))
					dir = 5;

				break;
			}

			/* Set new target, use target if legal */
			case '*':
			{
				int mode = TARGET_KILL;
				if (target_trap) mode |= TARGET_TRAP;
				if (target_set_interactive(mode, -1, -1)) dir = 5;
				break;
			}

			/* Set to closest target */
			case 'c':
			{
				if (target_set_closest(TARGET_KILL)) dir = 5;
				break;
			}

			/* Use current target, if set and legal */
			case 't':
			case '5':
			case '0':
			case '.':
			{
				if (target_okay()) dir = 5;
				break;
			}

			/* Possible direction */
			default:
			{
				int keypresses_handled = 0;

				while (ke.key != 0)
				{
					int this_dir;

					/* XXX Ideally show and move the cursor here to indicate
					   the currently "Pending" direction. XXX */
					this_dir = target_dir(ke.key);

					if (this_dir)
					{
						dir = dir_transitions[dir][this_dir];
					}
					else
					{
						break;
					}

					if (lazymove_delay == 0 || ++keypresses_handled > 1)
						break;

					/* See if there's a second keypress within the defined
					   period of time. */
					inkey_scan = lazymove_delay;
					ke = inkey_ex();
				}
			}
		}

		/* Error */
		if (!dir) bell("Illegal aim direction!");
	}

	/* REstore buttons */
	button_restore();
	event_signal(EVENT_MOUSEBUTTONS);

	/* No direction */
	if (!dir) return (FALSE);

	/* Save the direction */
	p_ptr->command_dir = dir;

	/* Check for confusion */
	if (p_ptr->timed[TMD_CONFUSED])
	{
		/* Random direction */
		dir = ddd[randint0(8)];
	}

	/* Notice confusion */
	if (p_ptr->command_dir != dir)
	{
		/* Warn the user */
		msg_print("You are confused.");
	}

	/* Save direction */
	(*dp) = dir;

	/* A "valid" direction was entered */
	return (TRUE);
}


/*
 * Request a "movement" direction (1,2,3,4,6,7,8,9) from the user.
 *
 * Return TRUE if a direction was chosen, otherwise return FALSE.
 *
 * This function should be used for all "repeatable" commands, such as
 * run, walk, open, close, bash, disarm, spike, tunnel, etc, as well
 * as all commands which must reference a grid adjacent to the player,
 * and which may not reference the grid under the player.
 *
 * Directions "5" and "0" are illegal and will not be accepted.
 *
 * This function tracks and uses the "global direction", and uses
 * that as the "desired direction", if it is set.
 */
bool get_rep_dir(int *dp)
{
	int dir;

	ui_event_data ke;

	/* Initialize */
	(*dp) = 0;

	/* Global direction */
	dir = p_ptr->command_dir;

	/* Get a direction */
	while (!dir)
	{
		/* Paranoia XXX XXX XXX */
		message_flush();

		/* Get first keypress - the first test is to avoid displaying the
		   prompt for direction if there's already a keypress queued up
		   and waiting - this just avoids a flickering prompt if there is
		   a "lazy" movement delay. */
		inkey_scan = SCAN_INSTANT;
		ke = inkey_ex();
		inkey_scan = SCAN_OFF;

		if (ke.key != DEFINED_XFF && target_dir(ke.key) == 0)
		{
			prt("Direction or <click> (Escape to cancel)? ", 0, 0);
			ke = inkey_ex();
		}

		/* Check mouse coordinates */
		if (ke.key == DEFINED_XFF)
		{
			/*if (ke.button) */
			{
				int y = KEY_GRID_Y(ke);
				int x = KEY_GRID_X(ke);

				/* Calculate approximate angle */
				int angle = get_angle_to_target(p_ptr->py, p_ptr->px, y, x, 0);

				/* Convert angle to direction */
				if (angle < 15) dir = 6;
				else if (angle < 33) dir = 9;
				else if (angle < 59) dir = 8;
				else if (angle < 78) dir = 7;
				else if (angle < 104) dir = 4;
				else if (angle < 123) dir = 1;
				else if (angle < 149) dir = 2;
				else if (angle < 168) dir = 3;
				else dir = 6;
			}
		}

		/* Get other keypresses until a direction is chosen. */
		else
		{
			int keypresses_handled = 0;

			while (ke.key != 0)
			{
				int this_dir;

				if (ke.key == ESCAPE)
				{
					/* Clear the prompt */
					prt("", 0, 0);

					return (FALSE);
				}

				/* XXX Ideally show and move the cursor here to indicate
				   the currently "Pending" direction. XXX */
				this_dir = target_dir(ke.key);

				if (this_dir)
				{
					dir = dir_transitions[dir][this_dir];
				}

				if (lazymove_delay == 0 || ++keypresses_handled > 1)
					break;

				inkey_scan = lazymove_delay;
				ke = inkey_ex();
			}

			/* 5 is equivalent to "escape" */
			if (dir == 5)
			{
				/* Clear the prompt */
				prt("", 0, 0);

				return (FALSE);
			}
		}

		/* Oops */
		if (!dir) bell("Illegal repeatable direction!");
	}

	/* Clear the prompt */
	prt("", 0, 0);

	/* Save desired direction */
	p_ptr->command_dir = dir;

	/* Save direction */
	(*dp) = dir;

	/* Success */
	return (TRUE);
}


/*
 * Apply confusion, if needed, to a direction
 *
 * Display a message and return TRUE if direction changes.
 */
bool confuse_dir(int *dp)
{
	int dir;

	/* Default */
	dir = (*dp);

	/* Apply "confusion" */
	if (p_ptr->timed[TMD_CONFUSED])
	{
		/* Apply confusion XXX XXX XXX */
		if ((dir == 5) || (rand_int(100) < 75))
		{
			/* Random direction */
			dir = ddd[rand_int(8)];
		}
	}

	/* Notice confusion */
	if ((*dp) != dir)
	{
		/* Warn the user */
		msg_print("You are confused.");

		/* Save direction */
		(*dp) = dir;

		/* Confused */
		return (TRUE);
	}

	/* Not confused */
	return (FALSE);
}



