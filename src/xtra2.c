/* File: xtra2.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"

/*
 * Mega-Hack - Apply level raise bonuses for races that progress with levels
 */
static void check_race_special(void)
{
	cptr levname;

	/* Hack - Demons gain bulk every level */
	if (rp_ptr->special == RACE_SPECIAL_DEMON)
	{
		p_ptr->ht += 4 + randint(2);
		p_ptr->wt += 7 + randint(4);
	}

	/* Get the name for the new level */
	levname = rsp_ptr[p_ptr->lev/5]->name;

	/* Check if name changed from last level */
	if (strcmp(levname, rsp_ptr[(p_ptr->lev - 1) / 5]->name))
	{
		cptr t;
	
		/* Get the proper article */
		if (is_a_vowel(*levname)) t="an";
		else t="a";

		/* Message */
		message_format(MSG_LEVEL, -1, "A wave of %s passes through your body",
			((rp_ptr->special == RACE_SPECIAL_DEMON) ? "great evil" : "holiness"));
		message_format(MSG_LEVEL, -1, "You have grown into %s %s!", t, levname);
	}

	/* Check to see if you got a new power */
	if (rsp_ptr[(p_ptr->lev) / 5]->power != rsp_ptr[(p_ptr->lev-1)/5]->power)
	{
		if (!rsp_ptr[(p_ptr->lev-1)/5]->power) message(MSG_LEVEL, -1, "You gain a racial power!");
		else message(MSG_LEVEL, -1, "Your racial power has changed!");

		/* Reset power timer */
		p_ptr->racial_power = 0;
	}
	
	p_ptr->redraw |= (PR_STATS | PR_MISC);

	return;
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
		(p_ptr->exp < (player_exp[p_ptr->lev-2] * p_ptr->expfact / 100L)))
	{
		/* Lose a level */
		p_ptr->lev--;

		/* Update some stuff */
		p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);

		/* Redraw some stuff */
		p_ptr->redraw |= (PR_LEV | PR_TITLE | PR_EXP);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);

		/* Handle stuff */
		handle_stuff();
	}

	/* Gain levels while possible */
	while ((p_ptr->lev < PY_MAX_LEVEL) &&
			(p_ptr->exp >= (player_exp[p_ptr->lev-1] * p_ptr->expfact / 100L)))
	{
		/* Gain a level */
		p_ptr->lev++;

		/* Check if a "special" race */
		if ((rp_ptr->special) && (p_ptr->lev > p_ptr->max_lev)) check_race_special();

		/* Save the highest level */
		if (p_ptr->lev > p_ptr->max_lev) p_ptr->max_lev = p_ptr->lev;

		/* Message */
		message_format(MSG_LEVEL, p_ptr->lev, "Welcome to level %d.", p_ptr->lev);

		/* Update some stuff */
		p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);

		/* Redraw some stuff */
		p_ptr->redraw |= (PR_LEV | PR_TITLE | PR_EXP);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);

		/* Handle stuff */
		handle_stuff();
	}

	/* Gain max levels while possible */
	while ((p_ptr->max_lev < PY_MAX_LEVEL) &&
		(p_ptr->max_exp >= (player_exp[p_ptr->max_lev-1] *
							p_ptr->expfact / 100L)))
	{
		/* Gain max level */
		p_ptr->max_lev++;

		/* Update some stuff */
		p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS); 

		/* Redraw some stuff */
		p_ptr->redraw |= (PR_LEV | PR_TITLE);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);

		/* Handle stuff */
		handle_stuff();
	}
}

/*
 * Create magical stairs after finishing a quest monster.
 */
static void build_quest_stairs(int y, int x)
{
	int ny, nx;

	/* Stagger around */
	while (!cave_valid_bold(y, x))
	{
		/* Pick a location */
		scatter(&ny, &nx, y, x, 1);

		/* Stagger */
		y = ny; x = nx;
	}

	/* Destroy any objects */
	delete_object(y, x);

	/* Explain the staircase */
	message(MSG_QUEST_SUCCEED, TRUE, "A magical staircase appears...");

	/* Create stairs down */
	cave_set_feat(y, x, FEAT_MORE);

	/* Update the visuals */
	p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

	/* Fully update the flow */
	p_ptr->update |= (PU_FORGET_FLOW | PU_UPDATE_FLOW);
}

/*
 * Handle the "death" of a monster.
 *
 * Disperse treasures centered at the monster location based on the
 * various flags contained in the monster flags fields.
 *
 * Check for "Quest" completion when a quest monster is killed.
 *
 * Note that only the player can induce "monster_death()" on Uniques.
 * Thus (for now) all Quest monsters should be Uniques.
 *
 * Note that monsters can now carry objects, and when a monster dies,
 * it drops all of its objects, which may disappear in crowded rooms.
 */
void monster_death(int m_idx)
{
	int i, j, y, x;

	int dump_item = 0;
	int dump_gold = 0;

	int number = 0;
	int total = 0;

	bool questlevel = FALSE;
	bool completed = FALSE;
	bool fixedquest = FALSE;

	s16b this_o_idx, next_o_idx = 0;

	monster_type *m_ptr = &m_list[m_idx];

	monster_race *r_ptr = get_monster_real(m_ptr);

	bool visible = (m_ptr->ml || (m_ptr->u_idx));

	bool good = (r_ptr->flags1 & (RF1_DROP_GOOD)) ? TRUE : FALSE;
	bool great = (r_ptr->flags1 & (RF1_DROP_GREAT)) ? TRUE : FALSE;

	bool do_gold = (!(r_ptr->flags1 & (RF1_ONLY_ITEM)));
	bool do_item = (!(r_ptr->flags1 & (RF1_ONLY_GOLD)));

	object_type *i_ptr;
	object_type object_type_body;

	/* Get the location */
	y = m_ptr->fy;
	x = m_ptr->fx;

	/* Update monster list window */
	p_ptr->window |= (PW_VISIBLE);

	/* Drop objects being carried */
	for (this_o_idx = m_ptr->hold_o_idx; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

		/* Get the object */
		o_ptr = &o_list[this_o_idx];

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
		drop_near(i_ptr, -1, y, x);
	}

	/* Forget objects */
	m_ptr->hold_o_idx = 0;

	/* Mega-Hack -- drop "winner" treasures */
	if (r_ptr->flags1 & (RF1_DROP_CHOSEN))
	{
		/* Get local object */
		i_ptr = &object_type_body;

		/* Mega-Hack -- Prepare to make "Grond" */
		object_prep(i_ptr, lookup_kind(TV_HAFTED, SV_GROND));

		/* Mega-Hack -- Mark this item as "Grond" */
		i_ptr->a_idx = ART_GROND;

		/* Mega-Hack -- Actually create "Grond" */
		apply_magic(i_ptr, -1, TRUE, TRUE, TRUE, FALSE);

		/* Mark origin */
		i_ptr->origin_nature = ORIGIN_MORGOTH;

		/* Drop it in the dungeon */
		drop_near(i_ptr, -1, y, x);

		/* Get local object */
		i_ptr = &object_type_body;

		/* Mega-Hack -- Prepare to make "Morgoth" */
		object_prep(i_ptr, lookup_kind(TV_HEADGEAR, SV_MORGOTH));

		/* Mega-Hack -- Mark this item as "Morgoth" */
		i_ptr->a_idx = ART_MORGOTH;

		/* Mega-Hack -- Actually create "Morgoth" */
		apply_magic(i_ptr, -1, TRUE, TRUE, TRUE, FALSE);

		/* Mark origin */
		i_ptr->origin_nature = ORIGIN_MORGOTH;

		/* Drop it in the dungeon */
		drop_near(i_ptr, -1, y, x);
	}

	/* Determine how much we can drop */
	if ((r_ptr->flags1 & (RF1_DROP_60)) && (rand_int(100) < 60)) number++;
	if ((r_ptr->flags1 & (RF1_DROP_90)) && (rand_int(100) < 90)) number++;
	if (r_ptr->flags1 & (RF1_DROP_1D2)) number += damroll(1, 2);
	if (r_ptr->flags1 & (RF1_DROP_2D2)) number += damroll(2, 2);
	if (r_ptr->flags1 & (RF1_DROP_3D2)) number += damroll(3, 2);
	if (r_ptr->flags1 & (RF1_DROP_4D2)) number += damroll(4, 2);

	/* Average dungeon and monster levels */
	p_ptr->obj_depth  = (p_ptr->depth + r_ptr->level) / 2;

	/* Drop some objects */
	for (j = 0; j < number; j++)
	{
		/* Get local object */
		i_ptr = &object_type_body;

		/* Wipe the object */
		object_wipe(i_ptr);

		/* Hack - drop "mimic" items */
		if (r_ptr->flags1 & (RF1_DROP_MIMIC))
		{
 			if (!make_mimic(i_ptr, m_ptr->attr, r_ptr->d_char)) continue;

			/* Assume seen XXX XXX XXX */
			lore_learn(m_ptr, LRN_FLAG1, RF1_DROP_MIMIC, FALSE);

			/* Mark history */
			if (visible) 
				object_history(i_ptr, ORIGIN_DROP_KNOWN, m_ptr->r_idx, m_ptr->s_idx, m_ptr->u_idx);
			else object_history(i_ptr, ORIGIN_DROP_UNKNOWN, 0, 0, 0);

			/* Drop it in the dungeon */
			drop_near(i_ptr, -1, y, x);

			/* Don't drop other stuff */
			continue;
		}

		/* Make Gold */
		if (do_gold && (!do_item || (rand_int(100) < 50)))
		{
			/* Make some gold */
			if (!make_gold(i_ptr, 0)) continue;

			/* Assume seen XXX XXX XXX */
			dump_gold++;
		}

		/* Make Object */
		else
		{
			/* Make an object */
			if (!make_object(i_ptr, good, great, TRUE)) continue;

			/* Hack - if a unique, inscribe with his name */
			if (inscribe_unique && m_ptr->u_idx) i_ptr->note = quark_add(monster_name(m_ptr));

			/* Mark history */
			if (visible) 
				object_history(i_ptr, ORIGIN_DROP_KNOWN, m_ptr->r_idx, m_ptr->s_idx, m_ptr->u_idx);
			else object_history(i_ptr, ORIGIN_DROP_UNKNOWN, 0, 0, 0);

			/* Assume seen XXX XXX XXX */
			dump_item++;
		}

		/* Drop it in the dungeon */
		drop_near(i_ptr, -1, y, x);
	}

	/* Reset the object level */
	p_ptr->obj_depth = p_ptr->depth;

	/* Take note of any dropped treasure */
	if (visible && (dump_item || dump_gold))
	{
		/* Take notes on treasure */
		lore_treasure(m_idx, dump_item, dump_gold);
	}

	/* Only process dungeon kills */
	if (!p_ptr->depth) return;

	/* Count incomplete quests */
	for (i = 0; i < z_info->q_max; i++)
	{
		quest_type *q_ptr = &q_info[i];

		/* Quest level? */
		if (q_ptr->active_level == p_ptr->depth)
		{
			/* One on the level */
			questlevel = TRUE;

			/* Require "Quest Monsters" */
			if ((((q_ptr->type == QUEST_FIXED) || (q_ptr->type == QUEST_GUILD)) &&
				(q_ptr->mon_idx == m_ptr->r_idx)) ||
				(((q_ptr->type == QUEST_FIXED_U) || (q_ptr->type == QUEST_UNIQUE)) &&
				(q_ptr->mon_idx == m_ptr->u_idx)))
			{
				/* Mark kills */
				q_ptr->cur_num++;

				/* Completed quest? */
				if (q_ptr->cur_num == q_ptr->max_num)
				{
					/* Mark complete */
					q_ptr->active_level = 0;

					/* Mark fixed quests */
					if ((q_ptr->type == QUEST_FIXED) || (q_ptr->type == QUEST_FIXED_U)) 
						fixedquest = TRUE;

					/* One complete */
					completed = TRUE;
				}
			}
		}

		/* Count incomplete quests */
		if (q_ptr->active_level) total++;
	}

	/* Require a quest level */
	if (!questlevel) return;

	/* Require all quests on this level to be completed */
	if (!completed) return;

	/* Check quest type */
	if (!fixedquest) 
	{
		/* Give a message */
		message(MSG_QUEST_SUCCEED, TRUE, 
			"You have completed your quest - collect your reward at the guild!");
		
		return;
	}

	/* Need some stairs */
	else if (total)
	{
		/* Build magical stairs */
		build_quest_stairs(y, x);

		p_ptr->fame += 10;
	}

	/* Nothing left, game over... */
	else 
	{
		/* Total winner */
		p_ptr->total_winner = TRUE;

		/* Redraw the "title" */
		p_ptr->redraw |= (PR_TITLE);

		p_ptr->fame += 50;

		/* Congratulations */
		message(MSG_QUEST_SUCCEED, TRUE, "*** CONGRATULATIONS ***");
		message(MSG_QUEST_SUCCEED, FALSE, "You have won the game!");
		message(MSG_QUEST_SUCCEED, FALSE, "You may retire (commit suicide) when you are ready.");
	}
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
bool mon_take_hit(int m_idx, int dam, bool *fear, cptr note)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = get_monster_real(m_ptr);

	u32b new_exp, new_exp_frac;

	/* Redraw (later) if needed */
	if (p_ptr->health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);

	/* Wake it up */
	m_ptr->csleep = 0;

	/* Anger it */
	m_ptr->calmed = 0;

	/* Hurt it */
	m_ptr->hp -= dam;

	/* It is dead now */
	if (m_ptr->hp < 0)
	{
		char m_name[80];

		/* Extract monster name */
		monster_desc(m_name, m_ptr, 0);

		/* Death by Missile/Spell attack */
		if (note)
		{
			message_format(MSG_KILL, m_ptr->r_idx, "%^s%s", m_name, note);
		}

		/* Death by physical attack -- invisible monster */
		else if (!m_ptr->ml)
		{
			message_format(MSG_KILL, m_ptr->r_idx, "You have killed %s.", m_name);
		}

		/* Death by Physical attack -- non-living monster */
		else if ((r_ptr->flags4 & (RF4_DEMON)) || (r_ptr->flags4 & (RF4_UNDEAD)) ||
				 (r_ptr->flags4 & (RF4_PLANT)) || (r_ptr->flags1 & (RF1_STUPID)) ||
		         (strchr("Evg$|!?~=", r_ptr->d_char)))
		{
			message_format(MSG_KILL, m_ptr->r_idx, "You have destroyed %s.", m_name);
		}

		/* Death by Physical attack -- living monster */
		else
		{
			message_format(MSG_KILL, m_ptr->r_idx, "You have slain %s.", m_name);
		}

		/* Calculate experience */
		mon_exp(m_ptr->r_idx, m_ptr->s_idx, m_ptr->u_idx, &new_exp, &new_exp_frac);
		
		/* Handle fractional experience */
		new_exp_frac = ((new_exp_frac * 0x10000L) / 1000L) + p_ptr->exp_frac;

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
		
		/* Generate treasure */
		monster_death(m_idx);

		/* When the player kills a Unique, it stays dead */
		if (m_ptr->u_idx) 
		{
			u_info[m_ptr->u_idx].dead = TRUE;

			/* reputation bonus */
 			if (u_info[m_ptr->u_idx].level) p_ptr->fame++;
		}
		
		/* Recall even invisible uniques*/
		if (m_ptr->ml || (m_ptr->u_idx))
		{
			/* Count kills this life */
			lore_learn(m_ptr, LRN_MDEATH, 0, TRUE);

			/* Hack -- Auto-recall */
			monster_track(m_ptr->r_idx, m_ptr->u_idx);
		}

		/* Delete the monster */
		delete_monster_idx(m_idx);

		/* Not afraid */
		(*fear) = FALSE;

		/* Update monster list */
		p_ptr->window |= PW_VISIBLE;

		/* Monster is dead */
		return (TRUE);
	}

	/* Mega-Hack -- Pain cancels fear */
	if (m_ptr->monfear && (dam > 0))
	{
		int tmp = randint(dam);

		/* Cure a little fear */
		if (tmp < m_ptr->monfear)
		{
			/* Reduce fear */
			m_ptr->monfear -= tmp;
		}

		/* Cure all the fear */
		else
		{
			/* Cure fear */
			m_ptr->monfear = 0;

			/* No more fear */
			(*fear) = FALSE;
		}
	}

	/* Sometimes a monster gets scared by damage */
	if (!m_ptr->monfear && !(r_ptr->flags3 & (RF3_NO_FEAR)))
	{
		int chance = 0;

		/* Percentage of fully healthy */
		int percentage = (100L * m_ptr->hp) / m_ptr->maxhp;

		/*
		 * Run (sometimes) if at 10% or less of max hit points,
		 * or (usually) when hit for half its current hit points
		 * or sometimes when hit by a weapon of fear XXX XXX
		 */
		if (dam && (percentage <= 10)) chance = (105 - (percentage * 10));
		if ((dam >= m_ptr->hp) && (chance < 80)) chance = 80;
		if (*fear) chance = 100;

		if (rand_int(100) < chance)
		{
			/* Hack -- note fear */
			(*fear) = TRUE;

			/* Hack -- Add some timed fear */
			m_ptr->monfear = (randint(10) + (((dam >= m_ptr->hp) && (percentage > 7)) ?
			                   20 : ((11 - percentage) * 5)));
		}
	}

	/* Not dead yet */
	return (FALSE);
}

/*
 * Modify the current panel to the given coordinates, adjusting only to
 * ensure the coordinates are legal, and return TRUE if anything done.
 *
 * Hack -- The town should never be scrolled around.
 *
 * Note that monsters are no longer affected in any way by panel changes.
 *
 * As a total hack, whenever the current panel changes, we assume that
 * the "overhead view" window should be updated.
 */
static bool modify_panel(int wy, int wx)
{
	/* Verify wy, adjust if needed */
	if (p_ptr->cur_map_hgt < SCREEN_HGT) wy = 0;
	else if (wy > p_ptr->cur_map_hgt - SCREEN_HGT) wy = p_ptr->cur_map_hgt - SCREEN_HGT;
	else if (wy < 0) wy = 0;

	/* Verify wx, adjust if needed */
	if (p_ptr->cur_map_wid < SCREEN_WID) wx = 0;
	if (wx > p_ptr->cur_map_wid - SCREEN_WID) wx = p_ptr->cur_map_wid - SCREEN_WID;
	else if (wx < 0) wx = 0;

	/* React to changes */
	if ((p_ptr->wy != wy) || (p_ptr->wx != wx))
	{
		/* Save wy, wx */
		p_ptr->wy = wy;
		p_ptr->wx = wx;

		/* Redraw map */
		p_ptr->redraw |= (PR_MAP);

		/* Hack -- Window stuff */
		p_ptr->window |= (PW_OVERHEAD);

		/* Changed */
		return (TRUE);
	}

	/* No change */
	return (FALSE);
}

/*
 * Perform the minimum "whole panel" adjustment to ensure that the given
 * location is contained inside the current panel, and return TRUE if any
 * such adjustment was performed.
 */
static bool adjust_panel(int y, int x)
{
	int wy = p_ptr->wy;
	int wx = p_ptr->wx;

	/* Adjust as needed */
	while (y >= wy + SCREEN_HGT) wy += SCREEN_HGT;
	while (y < wy) wy -= SCREEN_HGT;

	/* Adjust as needed */
	while (x >= wx + SCREEN_WID) wx += SCREEN_WID;
	while (x < wx) wx -= SCREEN_WID;

	/* Use "modify_panel" */
	return (modify_panel(wy, wx));
}

/*
 * Change the current panel to the panel lying in the given direction.
 *
 * Return TRUE if the panel was changed.
 */
static bool change_panel(int dir)
{
	int wy = p_ptr->wy + ddy[dir] * PANEL_HGT;
	int wx = p_ptr->wx + ddx[dir] * PANEL_WID;

	/* Use "modify_panel" */
	return (modify_panel(wy, wx));
}

/* 
 * Hack - generate the current room description 
 */
static void get_room_desc(int room, char *name, char *text_visible, char *text_always)
{
	/* Initialize text */
	strcpy(text_always, "");
	strcpy(text_visible, "");

	/* Town or not in room */
	if (!room)
	{
		if (!p_ptr->depth)
		{
			/* Initialise town description */
			strcpy(name, "town");
			strcpy(text_always, "It feels like home.");
		}
		else
		{
			strcpy(name, "the dungeon");
			strcpy(text_visible, "It is a dangerous maze of corridors and rooms.");
		}

		return;
	}
	
	/* In room */
	switch (room_info[room].type)
	{
		case (ROOM_LARGE):
		{
			strcpy(name, "large chamber");
			strcpy(text_visible, "This chamber contains an inner room with its own monsters, treasures and traps.");
			return;
		}
		case (ROOM_NEST_RODENT):
		{
			strcpy(name, "rodent warren");
			strcpy(text_visible, "Many small creatures scurry around you, and the room is full of rodent hairs and droppings.");
			return;
		}
		case (ROOM_NEST_JELLY):
		{
			strcpy(name, "jelly pit");
			strcpy(text_always, "An overpowering stench pervades the air here, which is unnaturally humid.");
			return;
		}
		case (ROOM_NEST_TREASURE):
		{
			strcpy(name, "money pit");
			strcpy(text_visible, "This room seems to have been designed to keep intruders out. Or perhaps it was meant to keep the treasure in?");
			return;
		}
		case (ROOM_NEST_VORTEX):
		{
			strcpy(name, "vortex rift");
			strcpy(text_visible, "You feel a surge of elemental energies as you enter this room. ");
			strcat(text_visible, "The boundaries between the planes have been stretched thin here.");  
			return;
		}
		case (ROOM_NEST_ANIMAL):
		{
			strcpy(name, "zoo");
			strcpy(text_visible, "This room contains a wide assortment of animals, probably collected by some mad spellcaster.");
			return;
		}
		case (ROOM_NEST_HORROR):
		{
			strcpy(name, "horror pit");
			strcpy(text_visible, "You have entered a chamber full of pure horror. If the walls could talk, ");
			strcat(text_visible, "they would tell of unspeakable acts. The shadows seem to move around you.");
			return;
		}
		case (ROOM_NEST_UNDEAD):
		{
			strcpy(name, "graveyard");
			strcpy(text_visible, "This room is full of corpses. Some of them don't seem to be still.");
			return;
		}
		case (ROOM_PIT_ORC):
		{
			strcpy(name, "orc pit");
			strcpy(text_visible, "You have stumbled into the barracks of a group of war-hungry orcs.");
			return;
		}
		case (ROOM_PIT_TROLL):
		{
			strcpy(name, "troll pit");
			strcpy(text_visible, "You have stumbled into a conclave of several troll clans. Filth lines the walls, ");
			strcat(text_visible, "and the floor is covered with crushed bones and mangled equipment.");
			strcpy(text_always, "The stink is unbearable.");
			return;
		}
		case (ROOM_PIT_PERSON):
		{
			strcpy(name, "meeting area");
			strcpy(text_visible, "You have stumbled into a meeting area of various servents of evil.");
			return;
		}
		case (ROOM_PIT_GIANT):
		{
			strcat(name, "dragon cavern");
			strcpy(text_visible, "You have entered a room used as a breeding ground for dragons. ");
			return;
		}
		case (ROOM_PIT_DRAGON):
		{
			strcpy(name, "demon pit");
			strcpy(text_visible, "You have entered a chamber full of arcane symbols, and an overpowering smell of brimstone.");
			return;
		}
		case (ROOM_PIT_DEMON):
		{
			strcpy(name, "demon pit");
			strcpy(text_visible, "You have entered a chamber full of arcane symbols, and an overpowering smell of brimstone.");
			return;
		}
		case (ROOM_QUEST_VAULT):
		{
			strcpy(name, "mysterious vault");
			strcpy(text_visible, "As you enter this sealed chamber, you feel a sudden moment of awe. ");
			return;
		}
		case (ROOM_GREATER_VAULT):
		{
			strcpy(name, "greater vault");
			strcpy(text_visible, "This vast sealed chamber is amongst the largest of its kind and is filled with ");
			strcat(text_visible, "deadly monsters and rich treasure.");
			strcpy(text_always, "Beware!");
			return;
		}
		case (ROOM_LESSER_VAULT):
		{
			strcpy(name, "lesser vault");
			strcpy(text_visible, "This vault is larger than most you have seen and contains more than ");
			strcat(text_visible, "its share of monsters and treasure.");
			return;
		}
		case (ROOM_NORMAL):
		{
			int i, j, n;

			char *s;

			char buf_text1[240];
			char buf_text2[240];
			char buf_name1[16];
			char buf_name2[16];

			/* Clear the history text */
			buf_text1[0] = '\0';
			buf_text2[0] = '\0';

			/* Clear the name1 text */
			buf_name1[0] = '\0';

			/* Clear the name2 text */
			buf_name2[0] = '\0';
			
			i = 0;

			while ((j = room_info[room].section[i++]) != -1)
			{
				/* Visible description or always present? */
				if (d_info[j].seen)
				{
					/* Get the textual history */
					strcat(buf_text1, (d_text + d_info[j].text));
				}
				else
				{
					/* Get the textual history */
					strcat(buf_text2, (d_text + d_info[j].text));
				}

				/* Get the name1 text if needed */
				if (!strlen(buf_name1)) strcpy(buf_name1, (d_name + d_info[j].name1));

				/* Get the name2 text if needed */
				if (!strlen(buf_name2)) strcpy(buf_name2, (d_name + d_info[j].name2));
			}

			/* Skip leading spaces */
			for (s = buf_text1; *s == ' '; s++) /* loop */;

			/* Get apparent length */
			n = strlen(s);

			/* Kill trailing spaces */
			while ((n > 0) && (s[n-1] == ' ')) s[--n] = '\0';

			/* Set the visible description */
			strcpy(text_visible, s);

			/* Skip leading spaces */
			for (s = buf_text2; *s == ' '; s++) /* loop */;

			/* Get apparent length */
			n = strlen(s);

			/* Kill trailing spaces */
			while ((n > 0) && (s[n-1] == ' ')) s[--n] = '\0';

			/* Set the visible description */
			strcpy(text_always, s);

			/* Set room name */
			if (strlen(buf_name1)) strcpy(name, buf_name1);

			/* And add second room name if necessary */
			if (strlen(buf_name2))
			{
				if (strlen(buf_name1))
				{
					strcat(name, " ");
					strcat(name, buf_name2);
				}
				else
				{
					strcpy(name, buf_name2);
				}

			}

			return;
		}
	}
}

/*
 * Hack -- describe the given room info in the current "term" window
 */
void display_room_info(int room)
{
	int y;
	char first[2];
	char name[32];
	char text_visible[240];
	char text_always[240];

	/* Hack -- handle "xtra" mode */
	if (!character_dungeon) return;

	/* Erase the window */
	for (y = 0; y < Term->hgt; y++)
	{
		/* Erase the line */
		Term_erase(0, y, 255);
	}

	/* Begin recall */
	Term_gotoxy(0, 1);

	/* Get the actual room description */
	get_room_desc(room, name, text_visible, text_always);

	/* Output to the screen */
	text_out_hook = text_out_to_screen;

	/* Describe room */
	if (strlen(text_visible))
	{
		text_out(text_visible);

		if (strlen(text_always))
		{
			text_out("  ");
			text_out(text_always);
		}

	}
	else if (strlen(text_always))
	{
		text_out(text_always);
	}
	else
	{
		text_out("There is nothing remarkable about it.");
	}

	/* Clear the top line */
	Term_erase(0, 0, 255);

	/* Reset the cursor */
	Term_gotoxy(0, 0);

	/* Hack - set first character to upper */
	first[0] = name[0];
	first[1] = '\0';

	/* Dump the name */
	Term_addstr(-1, TERM_L_BLUE, first);

	/* Dump the name */
	Term_addstr(-1, TERM_L_BLUE, (name + 1));
}

/*
 * Hack -- describe players current location.
 */
void describe_room(bool force_full)
{
	int by = p_ptr->py / BLOCK_HGT;
	int bx = p_ptr->px / BLOCK_WID;
	int room = dun_room[by][bx];
	char name[32];
	char text_visible[240];
	char text_always[240];

	/* Hack -- handle "xtra" mode */
	if (!character_dungeon) return;

	/* Window stuff */
	p_ptr->window |= (PW_ROOM_INFO);

	if (!force_full && !display_room_desc) return;

	/* Get the actual room description */
	get_room_desc(room, name, text_visible, text_always);

	/* Display the room */
	if ((cave_info[p_ptr->py][p_ptr->px] & (CAVE_GLOW)) && 
		((cave_info[p_ptr->py][p_ptr->px] & (CAVE_ROOM)) || 
		!(p_ptr->depth)))
	{
		if (room_info[room].seen && !force_full)
		{
			message_format(MSG_ROOM_DESC, 0, "You have entered %s %s.",
				(is_a_vowel(name[0]) ? "an" : "a"), name);
		}
		else if ((strlen(text_visible)) && (strlen(text_always)))
		{
			/* Message */
			message_format(MSG_ROOM_DESC, 0, "You have entered %s %s. %s %s",
				(is_a_vowel(name[0]) ? "an" : "a"),	name, text_visible, text_always);

			/* Now seen */
			room_info[room].seen = TRUE;
		}
		else if (strlen(text_visible))
		{
			/* Message */
			message_format(MSG_ROOM_DESC, 0, "You have entered %s %s. %s",
				(is_a_vowel(name[0]) ? "an" : "a"),	name, text_visible);

			/* Now seen */
			room_info[room].seen = TRUE;
		}
		else if (strlen(text_always))
		{
			/* Message */
			message_format(MSG_ROOM_DESC, 0, "You have entered %s %s. %s",
				(is_a_vowel(name[0]) ? "an" : "a"),	name, text_always);

			/* Now seen */
			room_info[room].seen = TRUE;
		}
		else
		{
			/* Message */
			message_format(MSG_ROOM_DESC, 0, "You have entered %s %s. There is nothing remarkable about it.",
				(is_a_vowel(name[0]) ? "an" : "a"),	name);
			/* Now seen */
			room_info[room].seen = TRUE;
		}
	}
	else if ((strlen(text_always)) && ((cave_info[p_ptr->py][p_ptr->px] & (CAVE_ROOM)) ||
		!(p_ptr->depth)))
	{
		/* Message */
		message_format(MSG_ROOM_DESC, 0, "%s", text_always);
	}
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
void verify_panel(void)
{
	int wy = p_ptr->wy;
	int wx = p_ptr->wx;

	/* Scroll screen vertically when off-center */
	if (center_player && (!p_ptr->running || !run_avoid_center) &&
	    (p_ptr->py != wy + SCREEN_HGT / 2))
	{
		wy = p_ptr->py - SCREEN_HGT / 2;
	}

	/* Scroll screen vertically when 2 grids from top/bottom edge */
	else if ((p_ptr->py < wy + 2) || (p_ptr->py >= wy + SCREEN_HGT - 2))
	{
		wy = ((p_ptr->py - PANEL_HGT / 2) / PANEL_HGT) * PANEL_HGT;
	}

	/* Scroll screen horizontally when off-center */
	if (center_player && (!p_ptr->running || !run_avoid_center) &&
	    (p_ptr->px != wx + SCREEN_WID / 2))
	{
		wx = p_ptr->px - SCREEN_WID / 2;
	}

	/* Scroll screen horizontally when 4 grids from left/right edge */
	else if ((p_ptr->px < wx + 4) || (p_ptr->px >= wx + SCREEN_WID - 4))
	{
		wx = ((p_ptr->px - PANEL_WID / 2) / PANEL_WID) * PANEL_WID;
	}

	/* Scroll if needed */
	if (modify_panel(wy, wx))
	{
		/* Optional disturb on "panel change" */
		if (disturb_panel && !center_player) disturb(0);
	}
}

/*
 * Monster health description
 */
static void look_mon_desc(char *buf, int m_idx)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = get_monster_real(m_ptr);

	bool living = TRUE;
	int perc;

	/* Determine if the monster is "living" (vs "undead") */
	if (r_ptr->flags4 & (RF4_UNDEAD)) living = FALSE;
	if (r_ptr->flags4 & (RF4_DEMON)) living = FALSE;
	if (r_ptr->flags4 & (RF4_PLANT)) living = FALSE;
	if (strchr("Evg$|!?~=", r_ptr->d_char)) living = FALSE;

	/* Healthy monsters */
	if (m_ptr->hp >= m_ptr->maxhp)
	{
		/* No damage */
		strcpy(buf,(living ? "unhurt" : "undamaged"));
	}
	else
	{
		/* Calculate a health "percentage" */
		perc = 100L * m_ptr->hp / m_ptr->maxhp;

		if (perc >= 60)
		{
			strcpy(buf,(living ? "somewhat wounded" : "somewhat damaged"));
		}
		else if (perc >= 25)
		{
			strcpy(buf,(living ? "wounded" : "damaged"));
		}
		else if (perc >= 10)
		{
			strcpy(buf,(living ? "badly wounded" : "badly damaged"));
		}
		else strcpy(buf,(living ? "almost dead" : "almost destroyed"));
	}

	if (m_ptr->csleep) strcat(buf, ", asleep");
	if (m_ptr->bleeding) strcat(buf, ", bleeding");
	if (m_ptr->poisoned) strcat(buf, ", poisoned");
	if (m_ptr->blinded) strcat(buf, ", blinded");
	if (m_ptr->confused) strcat(buf, ", confused");
	if (m_ptr->monfear) strcat(buf, ", afraid");
	if (m_ptr->calmed) strcat(buf, ", calmed");
	if (m_ptr->stunned) strcat(buf, ", stunned");
}

/*
 * Angband sorting algorithm -- quick sort in place
 *
 * Note that the details of the data we are sorting is hidden,
 * and we rely on the "ang_sort_comp()" and "ang_sort_swap()"
 * function hooks to interact with the data, which is given as
 * two pointers, and which may have any user-defined form.
 */
static void ang_sort_aux(void *u, void *v, int p, int q)
{
	int z, a, b;

	/* Done sort */
	if (p >= q) return;

	/* Pivot */
	z = p;

	/* Begin */
	a = p;
	b = q;

	/* Partition */
	while (TRUE)
	{
		/* Slide i2 */
		while (!(*ang_sort_comp)(u, v, b, z)) b--;

		/* Slide i1 */
		while (!(*ang_sort_comp)(u, v, z, a)) a++;

		/* Done partition */
		if (a >= b) break;

		/* Swap */
		(*ang_sort_swap)(u, v, a, b);

		/* Advance */
		a++, b--;
	}

	/* Recurse left side */
	ang_sort_aux(u, v, p, b);

	/* Recurse right side */
	ang_sort_aux(u, v, b+1, q);
}

/*
 * Sorting hook -- Comp function -- see below
 *
 * We use "u" to point to array of monster indexes,
 * and "v" to select the type of sorting to perform on "u".
 */
bool ang_mon_sort_comp_hook(const void *u, const void *v, int a, int b)
{
	monster_list_entry *who = (monster_list_entry*)(u);

	u16b *why = (u16b*)(v);

	int r1 = who[a].r_idx;
	int r2 = who[b].r_idx;

	int z1, z2;

	/* Sort by player kills */
	if (*why >= 4)
	{
		/* Extract player kills */
		z1 = get_lore_idx(r1,who[a].u_idx)->r_pkills;
		z2 = get_lore_idx(r2,who[b].u_idx)->r_pkills;

		/* Compare player kills */
		if (z1 < z2) return (TRUE);
		if (z1 > z2) return (FALSE);
	}

	/* Sort by total kills */
	if (*why >= 3)
	{
		/* Extract total kills */
		z1 = get_lore_idx(r1,who[a].u_idx)->r_tkills;
		z2 = get_lore_idx(r2,who[b].u_idx)->r_tkills;

		/* Compare total kills */
		if (z1 < z2) return (TRUE);
		if (z1 > z2) return (FALSE);
	}

	/* Sort by monster level */
	if (*why >= 2)
	{
		/* Extract levels */
		z1 = get_monster_fake(r1, 0, who[a].u_idx)->level;
		z2 = get_monster_fake(r2, 0, who[b].u_idx)->level;

		/* Compare levels */
		if (z1 < z2) return (TRUE);
		if (z1 > z2) return (FALSE);
	}

	/* Sort by monster experience */
	if (*why >= 1)
	{
		/* Extract experience */
		z1 = get_monster_fake(r1, 0, who[a].u_idx)->mexp;
		z2 = get_monster_fake(r2, 0, who[b].u_idx)->mexp;

		/* Compare experience */
		if (z1 < z2) return (TRUE);
		if (z1 > z2) return (FALSE);
	}

	/* Compare indexes */
	return (r1 <= r2);
}

/*
 * Sorting hook -- Swap function -- see below
 *
 * We use "u" to point to array of monster indexes,
 * and "v" to select the type of sorting to perform.
 */
void ang_mon_sort_swap_hook(void *u, void *v, int a, int b)
{
	monster_list_entry *who = (monster_list_entry*)(u);

	monster_list_entry holder;

	/* Swap */
	holder = who[a];
	who[a] = who[b];
	who[b] = holder;
}

/*
 * Angband sorting algorithm -- quick sort in place
 *
 * Note that the details of the data we are sorting is hidden,
 * and we rely on the "ang_sort_comp()" and "ang_sort_swap()"
 * function hooks to interact with the data, which is given as
 * two pointers, and which may have any user-defined form.
 */
void ang_sort(void *u, void *v, int n)
{
	/* Sort the array */
	ang_sort_aux(u, v, 0, n-1);
}

/* 
 * Code for inserting the uniques into the appropriate locations in a monster list
 */
void saturate_mon_list(monster_list_entry *who, int *count, bool allow_base, bool spoil)
{
	int i, j;
	int counter = 0;
	monster_list_entry *temp_list;

	C_MAKE(temp_list, M_LIST_ITEMS, monster_list_entry);

	for (i = 0;i < *count; i++)
	{
		monster_race *r_ptr = &r_info[who[i].r_idx];

		if (!who[i].r_idx) continue;

		/* Found a unique */
		if (r_ptr->max_unique)
		{
			int found = 0;

			/* Real base monster */
			if (!(r_ptr->flags1 & RF1_UNIQUE) && allow_base)
			{
				temp_list[counter].r_idx = who[i].r_idx;
				temp_list[counter].u_idx = 0;
	
				counter++;
			}

			for (j = 1;j < z_info->u_max; j++)
			{
				monster_unique *u_ptr = &u_info[j];

				if (u_ptr->r_idx == who[i].r_idx)
				{
					if ((spoil) || (cheat_know) || (lu_list[j].r_sights))
					{
						temp_list[counter].u_idx = j;
						temp_list[counter].r_idx = who[i].r_idx;
	
						counter++;
					}

					found++;
				}

				/* Efficiency */
				if (found == r_ptr->max_unique) break;
			}	
		}
		/* Didn't find a unique */
		else
		{
			temp_list[counter].r_idx = who[i].r_idx;
			temp_list[counter].u_idx = 0;

			counter++;
		}
	}

	/* Copy the new list into the old one */
	for (i = 0; i < z_info->r_max + z_info->u_max; i++)
	{
		who[i].r_idx = temp_list[i].r_idx;
		who[i].u_idx = temp_list[i].u_idx;
	}

	(*count) = counter;

	FREE(temp_list);
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
sint motion_dir(int y1, int x1, int y2, int x2)
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

/*
 * Extract a direction (or zero) from a character
 */
sint target_dir(char ch)
{
	int d = 0;

	int mode;

	cptr act;
	cptr s;

	/* Already a direction? */
	if (isdigit(ch))
 	{
		d = D2I(ch);
	}

	else
	{
		/* Roguelike */
		if (rogue_like_commands) mode = KEYMAP_MODE_ROGUE;

		/* Original */
		else mode = KEYMAP_MODE_ORIG;

		/* Extract the action (if any) */
		act = keymap_act[mode][(byte)(ch)];

		/* Analyze */
		if (act)
		{
			/* Convert to a direction */
			for (s = act; *s; ++s)
			{
				/* Use any digits in keymap */
				if (isdigit(*s)) d = D2I(*s);
			}
		}
	}

	/* Paranoia */
	if (d == 5) d = 0;

	/* Return direction */
	return (d);
}

/*
 * Determine is a monster makes a reasonable target
 *
 * The concept of "targetting" was stolen from "Morgul" (?)
 *
 * The player can target any location, or any "target-able" monster.
 *
 * Currently, a monster is "target_able" if it is visible, and if
 * the player can hit it with a projection, and the player is not
 * hallucinating.  This allows use of "use closest target" macros.
 *
 * Future versions may restrict the ability to target "trappers"
 * and "mimics", but the semantics is a little bit weird.
 */
static bool target_able(int m_idx)
{
	monster_type *m_ptr;

	/* No monster */
	if (m_idx <= 0) return (FALSE);

	/* Get monster */
	m_ptr = &m_list[m_idx];

	/* Monster must be alive */
	if (!m_ptr->r_idx) return (FALSE);

	/* Monster must be visible */
	if (!m_ptr->ml) return (FALSE);

	/* Monster must be projectable */
	if (!projectable(p_ptr->py, p_ptr->px, m_ptr->fy, m_ptr->fx)) return (FALSE);

	/* Hack -- no targeting hallucinations */
	if (p_ptr->image) return (FALSE);

	/* Hack -- Never target trappers XXX XXX XXX */
	/* if (CLEAR_ATTR && (CLEAR_CHAR)) return (FALSE); */

	/* Assume okay */
	return (TRUE);
}

/*
 * Update (if necessary) and verify (if possible) the target.
 *
 * We return TRUE if the target is "okay" and FALSE otherwise.
 */
bool target_okay(void)
{
	/* No target */
	if (!p_ptr->target_set) return (FALSE);

	/* Accept "location" targets */
	if (p_ptr->target_who == 0) return (TRUE);

	/* Check "monster" targets */
	if (p_ptr->target_who > 0)
	{
		int m_idx = p_ptr->target_who;

		/* Accept reasonable targets */
		if (target_able(m_idx))
		{
			monster_type *m_ptr = &m_list[m_idx];

			/* Get the monster location */
			p_ptr->target_row = m_ptr->fy;
			p_ptr->target_col = m_ptr->fx;

			/* Good target */
			return (TRUE);
		}
	}

	/* Assume no target */
	return (FALSE);
}

/*
 * Set the target to a monster (or nobody)
 */
void target_set_monster(int m_idx)
{
	/* Acceptable target */
 	if ((m_idx > 0) && target_able(m_idx))
	{
		monster_type *m_ptr = &m_list[m_idx];

		/* Save target info */
		p_ptr->target_set = TRUE;
		p_ptr->target_who = m_idx;
		p_ptr->target_row = m_ptr->fy;
		p_ptr->target_col = m_ptr->fx;
	}

	/* Clear target */
	else
	{
		/* Reset target info */
		p_ptr->target_set = FALSE;
		p_ptr->target_who = 0;
		p_ptr->target_row = 0;
		p_ptr->target_col = 0;
	}
}

/*
 * Set the target to a location
 */
static void target_set_location(int y, int x)
{
	/* Legal target */
	if (in_bounds_fully(y, x))
	{
		/* Save target info */
		p_ptr->target_set = TRUE;
		p_ptr->target_who = 0;
		p_ptr->target_row = y;
		p_ptr->target_col = x;
	}

	/* Clear target */
	else
	{
		/* Reset target info */
		p_ptr->target_set = FALSE;
		p_ptr->target_who = 0;
		p_ptr->target_row = 0;
		p_ptr->target_col = 0;
	}
}

/*
 * Sorting hook -- comp function -- by "distance to player"
 *
 * We use "u" and "v" to point to arrays of "x" and "y" positions,
 * and sort the arrays by double-distance to the player.
 */
static bool ang_sort_comp_distance(const void *u, const void *v, int a, int b)
{
	byte *x = (byte*)(u);
	byte *y = (byte*)(v);

	int da, db, kx, ky;

	/* Absolute distance components */
	kx = x[a]; kx -= p_ptr->px; kx = ABS(kx);
	ky = y[a]; ky -= p_ptr->py; ky = ABS(ky);

	/* Approximate Double Distance to the first point */
	da = ((kx > ky) ? (kx + kx + ky) : (ky + ky + kx));

	/* Absolute distance components */
	kx = x[b]; kx -= p_ptr->px; kx = ABS(kx);
	ky = y[b]; ky -= p_ptr->py; ky = ABS(ky);

	/* Approximate Double Distance to the first point */
	db = ((kx > ky) ? (kx + kx + ky) : (ky + ky + kx));

	/* Compare the distances */
	return (da <= db);
}

/*
 * Sorting hook -- swap function -- by "distance to player"
 *
 * We use "u" and "v" to point to arrays of "x" and "y" positions,
 * and sort the arrays by distance to the player.
 */
static void ang_sort_swap_distance(void *u, void *v, int a, int b)
{
	byte *x = (byte*)(u);
	byte *y = (byte*)(v);

	byte temp;

	/* Swap "x" */
	temp = x[a];
	x[a] = x[b];
	x[b] = temp;

	/* Swap "y" */
	temp = y[a];
	y[a] = y[b];
	y[b] = temp;
}

/*
 * Hack -- help "select" a location (see below)
 */
static s16b target_pick(int y1, int x1, int dy, int dx)
{
	int i, v;
	int x2, y2, x3, y3, x4, y4;
	int b_i = -1, b_v = 9999;

	/* Scan the locations */
	for (i = 0; i < temp_n; i++)
	{
		/* Point 2 */
		x2 = temp_x[i];
		y2 = temp_y[i];

		/* Directed distance */
		x3 = (x2 - x1);
		y3 = (y2 - y1);

		/* Verify quadrant */
		if (dx && (x3 * dx <= 0)) continue;
		if (dy && (y3 * dy <= 0)) continue;

		/* Absolute distance */
		x4 = ABS(x3);
		y4 = ABS(y3);

		/* Verify quadrant */
		if (dy && !dx && (x4 > y4)) continue;
		if (dx && !dy && (y4 > x4)) continue;

		/* Approximate Double Distance */
		v = ((x4 > y4) ? (x4 + x4 + y4) : (y4 + y4 + x4));

		/* Penalize location XXX XXX XXX */

		/* Track best */
		if ((b_i >= 0) && (v >= b_v)) continue;

		/* Track best */
		b_i = i; b_v = v;
	}

	/* Result */
	return (b_i);
}

/*
 * Hack -- determine if a given location is "interesting"
 */
static bool target_set_interactive_accept(int y, int x)
{
	object_type *o_ptr;

	/* Player grids are always interesting */
	if (cave_m_idx[y][x] < 0) return (TRUE);

	/* Handle hallucination */
	if (p_ptr->image) return (FALSE);

	/* Visible monsters */
	if (cave_m_idx[y][x] > 0)
	{
		monster_type *m_ptr = &m_list[cave_m_idx[y][x]];

		/* Visible monsters */
		if (m_ptr->ml) return (TRUE);
	}

	/* Scan all objects in the grid */
	for (o_ptr = get_first_object(y, x); o_ptr; o_ptr = get_next_object(o_ptr))
	{
		/* Memorized object */
		if (o_ptr->marked) return (TRUE);
	}

	/* Visible trap */
	if (cave_t_idx[y][x])
	{
		trap_type *t_ptr = &t_list[cave_t_idx[y][x]];

		/* Visible traps */
		if (t_ptr->visible && !trap_lock(y,x)) return (TRUE);
	}

	/* Interesting memorized features */
	if (cave_info[y][x] & (CAVE_MARK))
	{
		/* Notice doors */
		if (cave_feat[y][x] == FEAT_OPEN) return (TRUE);
		if (cave_feat[y][x] == FEAT_BROKEN) return (TRUE);
		if (cave_feat[y][x] == FEAT_CLOSED) return (TRUE);

		/* Notice chests */
		if (cave_feat[y][x] == FEAT_CHEST) return (TRUE);
		if (cave_feat[y][x] == FEAT_QST_CHEST) return (TRUE);

		/* Notice stairs */
		if (cave_feat[y][x] == FEAT_LESS) return (TRUE);
		if (cave_feat[y][x] == FEAT_MORE) return (TRUE);

		/* Notice shops */
		if ((cave_feat[y][x] >= FEAT_SHOP_HEAD) &&
		    (cave_feat[y][x] <= FEAT_SHOP_TAIL)) return (TRUE);

		/* Notice rubble */
		if (cave_feat[y][x] == FEAT_RUBBLE) return (TRUE);

		/* Notice veins with treasure */
		if (cave_feat[y][x] == FEAT_MAGMA_K) return (TRUE);
		if (cave_feat[y][x] == FEAT_QUARTZ_K) return (TRUE);
	}

	/* Nope */
	return (FALSE);
}

/*
 * Prepare the "temp" array for "target_interactive_set"
 *
 * Return the number of target_able monsters in the set.
 */
static void target_set_interactive_prepare(int mode)
{
	int y, x;

	/* Reset "temp" array */
	temp_n = 0;

	/* Scan the current panel */
	for (y = p_ptr->wy; y < p_ptr->wy + SCREEN_HGT; y++)
	{
		for (x = p_ptr->wx; x < p_ptr->wx + SCREEN_WID; x++)
		{
			/* Check bounds */
			if (!in_bounds_fully(y, x)) continue;

			/* Require line of sight, unless "look" is "expanded" */
			if (!expand_look && !player_has_los_bold(y, x)) continue;

			/* Require "interesting" contents */
			if (!target_set_interactive_accept(y, x)) continue;

			/* Special mode */
			if (mode & (TARGET_KILL))
			{
				/* Must contain a monster */
				if (!(cave_m_idx[y][x] > 0)) continue;

				/* Must be a targettable monster */
			 	if (!target_able(cave_m_idx[y][x])) continue;
			}

			/* Save the location */
			temp_x[temp_n] = x;
			temp_y[temp_n] = y;
			temp_n++;
		}
	}

	/* Set the sort hooks */
	ang_sort_comp = ang_sort_comp_distance;
	ang_sort_swap = ang_sort_swap_distance;

	/* Sort the positions */
	ang_sort(temp_x, temp_y, temp_n);
}

/*
 * Examine a grid, return a keypress.
 *
 * The "mode" argument contains the "TARGET_LOOK" bit flag, which
 * indicates that the "space" key should scan through the contents
 * of the grid, instead of simply returning immediately.  This lets
 * the "look" command get complete information, without making the
 * "target" command annoying.
 *
 * The "info" argument contains the "commands" which should be shown
 * inside the "[xxx]" text.  This string must never be empty, or grids
 * containing monsters will be displayed with an extra comma.
 *
 * Note that if a monster is in the grid, we update both the monster
 * recall info and the health bar info to track that monster.
 *
 * This function correctly handles multiple objects per grid, and objects
 * and terrain features in the same grid, though the latter never happens.
 *
 * This function must handle blindness/hallucination.
 */
static int target_set_interactive_aux(int y, int x, int mode, cptr info)
{
	s16b this_o_idx, next_o_idx = 0;
	cptr s1, s2, s3;
	bool boring, floored;
	int feat, query;

	char out_val[256];

	/* Repeat forever */
	while (TRUE)
	{
		/* Paranoia */
		query = ' ';

		/* Assume boring */
		boring = TRUE;

		/* Default */
		s1 = "You see ";
		s2 = "";
		s3 = "";

		/* The player */
		if (cave_m_idx[y][x] < 0)
		{
			/* Description */
			s1 = "You are ";

			/* Preposition */
			s2 = "on ";
		}

		/* Hack -- hallucination */
		if (p_ptr->image)
		{
			cptr name = "something strange";

			/* Display a message */
			if (cheat_wizard)
				sprintf(out_val, "%s%s%s%s [%s] (%d:%d)", s1, s2, s3, name, info, y, x);
			else
				sprintf(out_val, "%s%s%s%s [%s]", s1, s2, s3, name, info);
			prt(out_val, 0, 0);
			move_cursor_relative(y, x);
			query = inkey();

			/* Stop on everything but "return" */
			if ((query != '\n') && (query != '\r')) break;

			/* Repeat forever */
			continue;
		}

		/* Actual monsters */
		if (cave_m_idx[y][x] > 0)
		{
			monster_type *m_ptr = &m_list[cave_m_idx[y][x]];
			monster_race *r_ptr = get_monster_real(m_ptr);

			/* Visible */
			if (m_ptr->ml)
			{
				bool recall = FALSE;

				char m_name[80];

				/* Not boring */
				boring = FALSE;

				/* Get the monster name ("a kobold") */
				monster_desc(m_name, m_ptr, 0x08);

				/* Hack -- track this monster race */
				monster_track(m_ptr->r_idx, m_ptr->u_idx);

				/* Hack -- health bar for this monster */
				health_track(cave_m_idx[y][x]);

				/* Hack -- handle stuff */
				handle_stuff();

				/* Interact */
				while (TRUE)
				{
					/* Recall */
					if (recall)
					{
						/* Save screen */
						screen_save();

						/* Recall on screen */
						screen_roff(m_ptr->r_idx, m_ptr->u_idx);

						/* Hack -- Complete the prompt (again) */
						Term_addstr(-1, TERM_WHITE, format("  [r,%s]", info));

						/* Command */
						query = inkey();

						/* Load screen */
						screen_load();
					}

					/* Normal */
					else
					{
						char buf[100];

						/* Describe the monster */
						look_mon_desc(buf, cave_m_idx[y][x]);

						/* Describe, and prompt for recall */
						if (cheat_wizard)
						{
							sprintf(out_val, "%s%s%s%s (%s) [r,%s] (%d:%d)",
						            s1, s2, s3, m_name, buf, info, y, x);
						}
						else
						{
							sprintf(out_val, "%s%s%s%s (%s) [r,%s]",
							        s1, s2, s3, m_name, buf, info);
						}

				
						prt(out_val, 0, 0);

						/* Place cursor */
						move_cursor_relative(y, x);

						/* Command */
						query = inkey();
					}

					/* Normal commands */
					if (query != 'r') break;

					/* Toggle recall */
					recall = !recall;
				}

				/* Stop on everything but "return"/"space" */
				if ((query != '\n') && (query != '\r') && (query != ' ')) break;

				/* Sometimes stop at "space" key */
				if ((query == ' ') && !(mode & (TARGET_LOOK))) break;

				/* Change the intro */
				s1 = "It is ";

				/* Hack -- take account of gender */
				if (r_ptr->flags4 & (RF4_FEMALE)) s1 = "She is ";
				else if (r_ptr->flags4 & (RF4_MALE)) s1 = "He is ";

				/* Use a preposition */
				s2 = "carrying ";

				/* Scan all objects being carried */
				for (this_o_idx = m_ptr->hold_o_idx; this_o_idx; this_o_idx = next_o_idx)
				{
					char o_name[80];

					object_type *o_ptr;

					/* Get the object */
					o_ptr = &o_list[this_o_idx];

					/* Get the next object */
					next_o_idx = o_ptr->next_o_idx;

					/* Obtain an object description */
					object_desc(o_name, o_ptr, TRUE, 3);

					/* Describe the object */
					sprintf(out_val, "%s%s%s%s [%s]", s1, s2, s3, o_name, info);
					prt(out_val, 0, 0);
					move_cursor_relative(y, x);
					query = inkey();

					/* Stop on everything but "return"/"space" */
					if ((query != '\n') && (query != '\r') && (query != ' ')) break;

					/* Sometimes stop at "space" key */
					if ((query == ' ') && !(mode & (TARGET_LOOK))) break;

					/* Change the intro */
					s2 = "also carrying ";
				}

				/* Double break */
				if (this_o_idx) break;

				/* Use a preposition */
				s2 = "on ";
			}
		}

		/* Assume not floored */
		floored = FALSE;

		/* Scan all objects in the grid */
		if (easy_floor)
		{
			int floor_list[24];
			int floor_num;

			/* Scan for floor objects */
			floor_num = scan_floor(floor_list, 24, y, x, 0x02);

			/* Actual pile */
			if (floor_num > 1)
			{
				/* Not boring */
				boring = FALSE;

				/* Floored */
				floored = TRUE;

				/* Describe */
				while (TRUE)
				{
					/* Describe the pile */
					if (cheat_wizard)
						sprintf(out_val, "%s%s%sa pile of %d objects [r,%s] (%d:%d)", s1, s2, s3, floor_num, info, y, x);
					else
						sprintf(out_val, "%s%s%sa pile of %d objects [r,%s]",
							s1, s2, s3, floor_num, info);
					prt(out_val, 0, 0);
					move_cursor_relative(y, x);
					query = inkey();

					/* Display objects */
					if (query == 'r')
					{
						/* Save screen */
						screen_save();

						/* Display */
						show_floor(floor_list, floor_num);

						/* Describe the pile */
						prt(out_val, 0, 0);
						query = inkey();

						/* Load screen */
						screen_load();

						/* Continue on 'r' only */
						if (query == 'r') continue;
					}

					/* Done */
					break;
				}

				/* Stop on everything but "return"/"space" */
				if ((query != '\n') && (query != '\r') && (query != ' ')) break;

				/* Sometimes stop at "space" key */
				if ((query == ' ') && !(mode & (TARGET_LOOK))) break;

				/* Change the intro */
				s1 = "It is ";

				/* Preposition */
				s2 = "on ";
			}
		}

		/* Scan all objects in the grid */
		for (this_o_idx = cave_o_idx[y][x]; this_o_idx; this_o_idx = next_o_idx)
		{
			object_type *o_ptr;

			/* Get the object */
			o_ptr = &o_list[this_o_idx];

			/* Get the next object */
			next_o_idx = o_ptr->next_o_idx;

			/* Skip objects if floored */
			if (floored) continue;

			/* Describe it */
			if (o_ptr->marked)
			{
				char o_name[80];

				/* Not boring */
				boring = FALSE;

				/* Obtain an object description */
				object_desc(o_name, o_ptr, TRUE, 3);

				/* Describe the object */
				if (cheat_wizard)
					sprintf(out_val, "%s%s%s%s [%s] (%d:%d)", s1, s2, s3, o_name, info, y, x);
				else
					sprintf(out_val, "%s%s%s%s [%s]", s1, s2, s3, o_name, info);
				prt(out_val, 0, 0);
				move_cursor_relative(y, x);
				query = inkey();

				/* Stop on everything but "return"/"space" */
				if ((query != '\n') && (query != '\r') && (query != ' ')) break;

				/* Sometimes stop at "space" key */
				if ((query == ' ') && !(mode & (TARGET_LOOK))) break;

				/* Change the intro */
				s1 = "It is ";

				/* Plurals */
				if (o_ptr->number != 1) s1 = "They are ";

				/* Preposition */
				s2 = "on ";
			}
		}

		/* Double break */
		if (this_o_idx) break;

		/* Actual traps */
		if (cave_t_idx[y][x] > 0)
		{
			trap_type *t_ptr = &t_list[cave_t_idx[y][x]];
			trap_widget *w_ptr = &w_info[t_ptr->w_idx];

			/* 
			 * Visible trap 
			 * Note that simple locks (LOCK flag) don't appear here
			 */
			if (t_ptr->visible && !trap_lock(y, x) && !trap_chest(y, x))
			{
				cptr t_name = w_name + w_ptr->name;

				if (!(cp_ptr->flags & CF_TRAP_KNOW))
				{
					switch (w_ptr->flags & WGF_TYPE_MASK)
					{
						case WGF_PIT:		t_name = "a pit"; break;
						case WGF_RUNE:		t_name = "a strange rune"; break;
						case WGF_SPOT:		t_name = "a discolored spot"; break;
						case WGF_DART:		t_name = "a dart trap"; break;
						case WGF_GAS:		t_name = "a gas trap"; break;
						case WGF_SLOTS:		t_name = "mysterious slots"; break;
						case WGF_ROCKS:		t_name = "loose rocks"; break;
					}
				}
				
				if (cheat_wizard)
					sprintf(out_val, "%s%s%s%s [%s] (%d:%d)", 
							s1, s2, s3, t_name, info, y, x);
				else
					sprintf(out_val, "%s%s%s%s [%s]", s1, s2, s3, t_name, info);
				prt(out_val, 0, 0);
				move_cursor_relative(y, x);
				query = inkey();

				/* Change the intro */
				s1 = "It is ";

				/* Preposition */
				s2 = "on ";

				boring = FALSE;
			}
		}

		/* Feature (apply "mimic") */
		feat = f_info[cave_feat[y][x]].mimic;

		/* Require knowledge about grid, or ability to see grid */
		if (!(cave_info[y][x] & (CAVE_MARK)) && !player_can_see_bold(y,x))
		{
			/* Forget feature */
			feat = FEAT_NONE;
		}

		/* Terrain feature if needed */
		if (boring || (feat > FEAT_FLOOR))
		{
			cptr name = f_name + f_info[feat].name;
			cptr lock = "";

			/* Hack -- handle unknown grids */
			if (feat == FEAT_NONE) name = "unknown grid";

			/* Pick a prefix */
			if (*s2 && (feat >= FEAT_OPEN) && (feat < FEAT_SHOP_HEAD)) s2 = "in ";

			/* Pick proper indefinite article */
			s3 = (is_a_vowel(name[0])) ? "an " : "a ";

			/* Hack -- special introduction for store doors */
			if ((feat >= FEAT_SHOP_HEAD) && (feat <= FEAT_SHOP_TAIL))
			{
				s3 = "the entrance to the ";
			}

			/* Hack - locks */
			if (cave_t_idx[y][x] && trap_lock(y, x) && (feat == FEAT_CLOSED))
			{
				lock = w_name + w_info[t_list[cave_t_idx[y][x]].w_idx].name;
			}

			/* Hack - chests */
			if (cave_t_idx[y][x] && trap_chest(y, x) && 
				((feat == FEAT_CHEST) || (feat == FEAT_QST_CHEST)))
			{
				lock = w_name + w_info[t_list[cave_t_idx[y][x]].w_idx].name;
			}

			/* Display a message */
			if (cheat_wizard)
				sprintf(out_val, "%s%s%s%s%s [%s] (%d:%d)", s1, s2, s3, name, lock, info, y, x);
			else
				sprintf(out_val, "%s%s%s%s%s [%s]", s1, s2, s3, name, lock, info);
			prt(out_val, 0, 0);
			move_cursor_relative(y, x);
			query = inkey();

			/* Stop on everything but "return"/"space" */
			if ((query != '\n') && (query != '\r') && (query != ' ')) break;
		}

		/* Stop on everything but "return" */
		if ((query != '\n') && (query != '\r')) break;
	}

	/* Keep going */
	return (query);
}

/*
 * Handle "target" and "look".
 *
 * Note that this code can be called from "get_aim_dir()".
 *
 * All locations must be on the current panel, unless the "scroll_target"
 * option is used, which allows changing the current panel during "look"
 * and "target" commands.  Currently, when "flag" is true, that is, when
 * "interesting" grids are being used, and a directional key is used, we
 * only scroll by a single panel, in the direction requested, and check
 * for any interesting grids on that panel.  The "correct" solution would
 * actually involve scanning a larger set of grids, including ones in
 * panels which are adjacent to the one currently scanned, but this is
 * overkill for this function.  XXX XXX
 *
 * Hack -- targetting/observing an "outer border grid" may induce
 * problems, so this is not currently allowed.
 *
 * The player can use the direction keys to move among "interesting"
 * grids in a heuristic manner, or the "space", "+", and "-" keys to
 * move through the "interesting" grids in a sequential manner, or
 * can enter "location" mode, and use the direction keys to move one
 * grid at a time in any direction.  The "t" (set target) command will
 * only target a monster (as opposed to a location) if the monster is
 * target_able and the "interesting" mode is being used.
 *
 * The current grid is described using the "look" method above, and
 * a new command may be entered at any time, but note that if the
 * "TARGET_LOOK" bit flag is set (or if we are in "location" mode,
 * where "space" has no obvious meaning) then "space" will scan
 * through the description of the current grid until done, instead
 * of immediately jumping to the next "interesting" grid.  This
 * allows the "target" command to retain its old semantics.
 *
 * The "*", "+", and "-" keys may always be used to jump immediately
 * to the next (or previous) interesting grid, in the proper mode.
 *
 * The "return" key may always be used to scan through a complete
 * grid description (forever).
 *
 * This command will cancel any old target, even if used from
 * inside the "look" command.
 */
bool target_set_interactive(int mode)
{
	int i, d, m, t, bd;

	int y = p_ptr->py;
	int x = p_ptr->px;

	bool done = FALSE;
	bool flag = ((mode & TARGET_FREE) ? FALSE :TRUE);

	char query;

	char info[80];

	/* Cancel target */
	target_set_monster(0);

	/* Prepare the "temp" array */
	target_set_interactive_prepare(mode);

	/* Start near the player */
	m = 0;

	/* Interact */
	while (!done)
	{
		/* Interesting grids */
		if (flag && temp_n)
		{
			y = temp_y[m];
			x = temp_x[m];

			/* Allow target */
			if ((cave_m_idx[y][x] > 0) && target_able(cave_m_idx[y][x]))
			{
				strcpy(info, "q,t,p,o,+,-,<dir>");
			}

			/* Dis-allow target */
			else
			{
				strcpy(info, "q,p,o,+,-,<dir>");
			}

			/* Describe and Prompt */
			query = target_set_interactive_aux(y, x, mode, info);

			/* Cancel tracking */
			/* health_track(0); */

			/* Assume no "direction" */
			d = 0;

			/* Analyze */
			switch (query)
			{
				case ESCAPE:
				case 'q':
				{
					done = TRUE;
					break;
				}

				case ' ':
				case '*':
				case '+':
				{
					if (++m == temp_n)
					{
						m = 0;
						if (!expand_list) done = TRUE;
					}
					break;
				}

				case '-':
				{
					if (m-- == 0)
					{
						m = temp_n - 1;
						if (!expand_list) done = TRUE;
					}
					break;
				}

				case 'p':
				{
					if (scroll_target)
					{
						/* Recenter around player */
						verify_panel();

						/* Handle stuff */
						handle_stuff();
					}

					y = p_ptr->py;
					x = p_ptr->px;
				}

				case 'o':
				{
					flag = FALSE;
					break;
				}

				case 'm':
				{
					break;
				}

				case 't':
				case '5':
				case '0':
				case '.':
				{
					int m_idx = cave_m_idx[y][x];

					if ((m_idx > 0) && target_able(m_idx))
					{
						health_track(m_idx);
						target_set_monster(m_idx);
						done = TRUE;
					}
					else
					{
						bell("Illegal target!");
					}
					break;
				}

				default:
				{
					/* Extract direction */
					d = target_dir(query);

					/* Oops */
					if (!d) bell("Illegal command for target mode!");

					break;
				}
			}

			/* Hack -- move around */
			if (d)
			{
				int old_y = temp_y[m];
				int old_x = temp_x[m];

				/* Find a new monster */
				i = target_pick(old_y, old_x, ddy[d], ddx[d]);

				/* Scroll to find interesting grid */
				if (scroll_target && (i < 0))
				{
					int old_wy = p_ptr->wy;
					int old_wx = p_ptr->wx;

					/* Change if legal */
					if (change_panel(d))
					{
						/* Recalculate interesting grids */
						target_set_interactive_prepare(mode);

						/* Find a new monster */
						i = target_pick(old_y, old_x, ddy[d], ddx[d]);

						/* Restore panel if needed */
						if ((i < 0) && modify_panel(old_wy, old_wx))
						{

							/* Recalculate interesting grids */
							target_set_interactive_prepare(mode);
						}

						/* Handle stuff */
						handle_stuff();
					}
				}

				/* Use interesting grid if found */
				if (i >= 0) m = i;
			}
		}

		/* Arbitrary grids */
		else
		{
			/* Default prompt */
			if (!(mode & TARGET_FREE)) strcpy(info, "q,t,p,m,<dir>");
			else strcpy(info, "q,t,p,<dir>");

			/* Describe and Prompt (enable "TARGET_LOOK") */
			query = target_set_interactive_aux(y, x, mode | TARGET_LOOK, info);

			/* Cancel tracking */
			/* health_track(0); */

			/* Assume no direction */
			d = 0;

			/* Analyze the keypress */
			switch (query)
			{
				case ESCAPE:
				case 'q':
				{
					done = TRUE;
					break;
				}

				case 'p':
				{
					if (scroll_target)
					{
						/* Recenter around player */
						verify_panel();

						/* Handle stuff */
						handle_stuff();
					}

					y = p_ptr->py;
					x = p_ptr->px;
				}

				case 'm':
				{
					if (!(mode & TARGET_FREE)) 
					{
						flag = TRUE;

						m = 0;
						bd = 999;

						/* Pick a nearby monster */
						for (i = 0; i < temp_n; i++)
						{
							t = distance(y, x, temp_y[i], temp_x[i]);

							/* Pick closest */
							if (t < bd)
							{
								m = i;
								bd = t;
							}
						}

						/* Nothing interesting */
						if (bd == 999) flag = FALSE;
					}

					break;
				}

				case 't':
				case '5':
				case '0':
				case '.':
				{
					target_set_location(y, x);
					done = TRUE;
					break;
				}

				default:
				{
					/* Extract a direction */
					d = target_dir(query);

					/* Oops */
					if (!d) bell("Illegal command for target mode!");

					break;
				}
			}

			/* Handle "direction" */
			if (d)
			{
				/* Move */
				x += ddx[d];
				y += ddy[d];

				if (scroll_target)
				{
					/* Slide into legality */
					if (x >= p_ptr->cur_map_wid - 1) x--;
					else if (x <= 0) x++;

					/* Slide into legality */
					if (y >= p_ptr->cur_map_hgt - 1) y--;
					else if (y <= 0) y++;

					/* Adjust panel if needed */
					if (adjust_panel(y, x))
					{
						/* Handle stuff */
						handle_stuff();

						/* Recalculate interesting grids */
						target_set_interactive_prepare(mode);
					}
				}

				else
				{
					/* Slide into legality */
					if (x >= p_ptr->wx + SCREEN_WID) x--;
					else if (x < p_ptr->wx) x++;

					/* Slide into legality */
					if (y >= p_ptr->wy + SCREEN_HGT) y--;
					else if (y < p_ptr->wy) y++;
				}
			}
		}
	}

	/* Forget */
	temp_n = 0;

	/* Clear the top line */
	prt("", 0, 0);

	if (scroll_target)
	{
		/* Recenter around player */
		verify_panel();

		/* Handle stuff */
		handle_stuff();
	}

	/* Failure to set target */
	if (!p_ptr->target_set) return (FALSE);

	/* Success */
	return (TRUE);
}

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
bool get_aim_dir(int *dp)
{
	int dir;
	char ch;
	cptr p;

	if (repeat_pull(dp))
	{
		/* Verify */
		if (!(*dp == 5 && !target_okay()))
		{
			return (TRUE);
		}
		else
		{
			/* Invalid repeat - reset it */
			repeat_clear();
		}
	}

	/* Initialize */
	(*dp) = 0;

	/* Global direction */
	dir = p_ptr->command_dir;

	/* Hack -- auto-target if requested */
	if (use_old_target && target_okay()) dir = 5;

	/* Ask until satisfied */
	while (!dir)
	{
		/* Choose a prompt */
		if (!target_okay())
		{
			p = "Direction ('*' to choose a target, Escape to cancel)? ";
		}
		else
		{
			p = "Direction ('5' for target, '*' to re-target, Escape to cancel)? ";
		}

		/* Get a command (or Cancel) */
		if (!get_com(p, &ch)) break;

		/* Analyze */
		switch (ch)
		{
			/* Set new target, use target if legal */
			case '*':
			{
				if (target_set_interactive(TARGET_KILL)) dir = 5;
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
				dir = target_dir(ch);
				break;
			}
		}

		/* Error */
		if (!dir) bell("Illegal aim direction!");
	}

	/* No direction */
	if (!dir) return (FALSE);

	/* Save the direction */
	p_ptr->command_dir = dir;

	/* Check for confusion */
	if (p_ptr->confused)
	{
		/* Random direction */
		dir = ddd[rand_int(8)];
	}

	/* Notice confusion */
	if (p_ptr->command_dir != dir)
	{
		/* Warn the user */
		message(MSG_EFFECT, 0, "You are confused.");
	}

	/* Save direction */
	(*dp) = dir;

	repeat_push(dir);

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

	char ch;

	cptr p;

	if (repeat_pull(dp))
	{
		return (TRUE);
	}

	/* Initialize */
	(*dp) = 0;

	/* Global direction */
	dir = p_ptr->command_dir;

	/* Get a direction */
	while (!dir)
	{
		/* Choose a prompt */
		p = "Direction (Escape to cancel)? ";

		/* Get a command (or Cancel) */
		if (!get_com(p, &ch)) break;

		/* Convert keypress into a direction */
		dir = target_dir(ch);

		/* Oops */
		if (!dir) bell("Illegal repeatable direction!");
	}

	/* Aborted */
	if (!dir) return (FALSE);

	/* Save desired direction */
	p_ptr->command_dir = dir;

	/* Save direction */
	(*dp) = dir;

	repeat_push(dir);

	/* Success */
	return (TRUE);
}

/*
 * Apply confusion, if needed, to a direction
 *
 * Display a message and return TRUE if direction changes.
 *
 * Note - this function also applies "panic", which is treated just like confusion
 */
bool confuse_dir(int *dp)
{
	int dir;
	int chance = 0;

	/* Default */
	dir = (*dp);

	/* Chance of random movement, based on confusion level */
	if ((chance < 75) && (p_ptr->confused > PY_CONF_BEFUDDLE)) chance = 75;
	if ((chance < 50) && (p_ptr->afraid > PY_FEAR_PANIC)) chance = 50;
	if ((chance < 50) && (p_ptr->confused > PY_CONF_CONFUSE)) chance = 50;
	if ((chance < 25) && (p_ptr->confused)) chance = 25;

	/* Apply "confusion" */
	if (chance)
	{
		/* Apply confusion XXX XXX XXX */
		if ((dir == 5) || (rand_int(100) < chance))
		{
			/* Random direction */
			dir = ddd[rand_int(8)];
		}
	}

	/* Notice confusion */
	if ((*dp) != dir)
	{
		/* Warn the user */
		if (p_ptr->confused) message(MSG_EFFECT, 0, "You are confused.");
		else message(MSG_EFFECT, 0, "You are panicking!");

		/* Save direction */
		(*dp) = dir;

		/* Confused */
		return (TRUE);
	}

	/* Not confused */
	return (FALSE);
}

/*
 * Centers a string within a 31 character string
 */
static void center_string(char *buf, cptr str)
{
	int i, j;

	/* Total length */
	i = strlen(str);

	/* Necessary border */
	j = 15 - i / 2;

	/* Mega-Hack */
	sprintf(buf, "%*s%s%*s", j, "", str, 31 - i - j, "");
}

/*
 * Display a "tomb-stone"
 */
static void print_tomb(time_t death_time)
{
	cptr p;

	char tmp[160];

	char buf[1024];

	FILE *fp;

	/* Clear screen */
	Term_clear();

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_FILE, "dead.txt");

	/* Open the News file */
	fp = my_fopen(buf, "r");

	/* Dump */
	if (fp)
	{
		int i = 0;

		/* Dump the file to the screen */
		while (0 == my_fgets(fp, buf, sizeof(buf)))
		{
			/* Display and advance */
			put_str(buf, i++, 0);
		}

		/* Close */
		my_fclose(fp);
	}

	/* King or Queen */
	if (p_ptr->total_winner || (p_ptr->lev > PY_MAX_LEVEL))
	{
		p = "Magnificent";
	}

	/* Normal */
	else
	{
#ifndef PREVENT_LOAD_C_TEXT
		p = c_text+cp_ptr->title[(p_ptr->lev-1)/5];
#else /* PREVENT_LOAD_C_TEXT */
		p = " ";
#endif /* PREVENT_LOAD_C_TEXT */
	}

	center_string(buf, op_ptr->full_name);
	put_str(buf, 6, 11);

	center_string(buf, "the");
	put_str(buf, 7, 11);

	center_string(buf, p);
	put_str(buf, 8, 11);

	center_string(buf, c_name + cp_ptr->name);
	put_str(buf, 10, 11);

	sprintf(tmp, "Level: %d", (int)p_ptr->lev);
	center_string(buf, tmp);
	put_str(buf, 11, 11);

	sprintf(tmp, "Exp: %ld", (long)p_ptr->exp);
	center_string(buf, tmp);
	put_str(buf, 12, 11);

	sprintf(tmp, "AU: %ld", (long)p_ptr->au);
	center_string(buf, tmp);
	put_str(buf, 13, 11);

	sprintf(tmp, "Killed on Level %d", p_ptr->depth);
	center_string(buf, tmp);
	put_str(buf, 14, 11);

	sprintf(tmp, "by %s.", p_ptr->died_from);
	center_string(buf, tmp);
	put_str(buf, 15, 11);

	sprintf(tmp, "%-.24s", ctime(&death_time));
	center_string(buf, tmp);
	put_str(buf, 17, 11);
}

/*
 * Hack - Know inventory and home items upon death
 */
static void death_knowledge(void)
{
	int i;

	object_type *o_ptr;

	store_type *st_ptr = &store[STORE_HOME];

	/* Hack -- Know everything in the inven/equip */
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Aware and Known */
		object_aware(o_ptr);
		object_known(o_ptr);

		/* Fully known */
		o_ptr->ident |= (IDENT_MENTAL);
	}

	/* Hack -- Know everything in the home */
	for (i = 0; i < st_ptr->stock_num; i++)
	{
		o_ptr = &st_ptr->stock[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Aware and Known */
		object_aware(o_ptr);
		object_known(o_ptr);

		/* Fully known */
		o_ptr->ident |= (IDENT_MENTAL);
	}

	/* Hack -- Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Handle stuff */
	handle_stuff();
}

/*
 * Display some character info
 */
static void show_info(void)
{
	int i, j, k;

	object_type *o_ptr;

	store_type *st_ptr = &store[STORE_HOME];

	/* Display player */
	display_player(0);

	/* Prompt for inventory */
	prt("Hit any key to see more information (ESC to abort): ", 23, 0);

	/* Allow abort at this point */
	if (inkey() == ESCAPE) return;

	/* Show equipment and inventory */

	/* Equipment -- if any */
	if (p_ptr->equip_cnt)
	{
		Term_clear();
		item_tester_full = TRUE;
		show_equip();
		prt("You are using: -more-", 0, 0);
		if (inkey() == ESCAPE) return;
	}

	/* Inventory -- if any */
	if (p_ptr->inven_cnt)
	{
		Term_clear();
		item_tester_full = TRUE;
		show_inven();
		prt("You are carrying: -more-", 0, 0);
		if (inkey() == ESCAPE) return;
	}

	/* Home -- if anything there */
	if (st_ptr->stock_num)
	{
		/* Display contents of the home */
		for (k = 0, i = 0; i < st_ptr->stock_num; k++)
		{
			/* Clear screen */
			Term_clear();

			/* Show 12 items */
			for (j = 0; (j < 12) && (i < st_ptr->stock_num); j++, i++)
			{
				byte attr;

				char o_name[80];
				char tmp_val[80];

				/* Get the object */
				o_ptr = &st_ptr->stock[i];

				/* Print header, clear line */
				sprintf(tmp_val, "%c) ", I2A(j));
				prt(tmp_val, j+2, 4);

				/* Get the object description */
				object_desc(o_name, o_ptr, TRUE, 3);

				/* Get the inventory color */
				attr = tval_to_attr[o_ptr->tval % N_ELEMENTS(tval_to_attr)];

				/* Display the object */
				c_put_str(attr, o_name, j+2, 7);
			}

			/* Caption */
			prt(format("Your home contains (page %d): -more-", k+1), 0, 0);

			/* Wait for it */
			if (inkey() == ESCAPE) return;
		}
	}
}

/*
 * Special version of 'do_cmd_examine'
 */
static void death_examine(void)
{
	int item;

	object_type *o_ptr;

	char o_name[80];

	cptr q, s;

	/* Set text_out hook */
	text_out_hook = text_out_to_screen;

	/* Get an item */
	q = "Examine which item? ";
	s = "You have nothing to examine.";

	while (TRUE)
 	{
		/* Clear the screen */
		Term_clear();

		if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP))) return;
 
		/* Get the item */
		o_ptr = &inventory[item];
 
		/* Fully known */
		o_ptr->ident |= (IDENT_MENTAL);
 
		/* Description */
		object_desc(o_name, o_ptr, TRUE, 3);
 
		/* Begin recall */
		Term_gotoxy(0, 1);

		/* Actually display the item */
		list_object(o_ptr, OBJECT_INFO_KNOWN);

		object_desc_store(o_name, o_ptr, TRUE, 3);

		/* Clear the top line */
		Term_erase(0, 0, 255);

		/* Reset the cursor */
		Term_gotoxy(0, 0);

		/* Dump the name */
		Term_addstr(-1, TERM_L_BLUE, o_name);

		(void) inkey();
	}
}

/*
 * Change the player into a Winner
 */
static void kingly(void)
{
	/* Hack -- retire in town */
	p_ptr->depth = 0;

	/* Fake death */
	strcpy(p_ptr->died_from, "Ripe Old Age");

	/* Restore the experience */
	p_ptr->exp = p_ptr->max_exp;

	/* Restore the level */
	p_ptr->lev = p_ptr->max_lev;

	/* Hack -- Instant Gold */
	p_ptr->au += 10000000L;

	/* Clear screen */
	Term_clear();

	/* Display a crown */
	put_str("#", 1, 34);
	put_str("#####", 2, 32);
	put_str("#", 3, 34);
	put_str(",,,  $$$  ,,,", 4, 28);
	put_str(",,=$   \"$$$$$\"   $=,,", 5, 24);
	put_str(",$$        $$$        $$,", 6, 22);
	put_str("*>         <*>         <*", 7, 22);
	put_str("$$         $$$         $$", 8, 22);
	put_str("\"$$        $$$        $$\"", 9, 22);
	put_str("\"$$       $$$       $$\"", 10, 23);
	put_str("*#########*#########*", 11, 24);
	put_str("*#########*#########*", 12, 24);

	/* Display a message */
	put_str("Veni, Vidi, Vici!", 15, 26);
	put_str("I came, I saw, I conquered!", 16, 21);
	put_str(format("All Hail the Mighty %s!", sp_ptr->winner), 17, 22);

	/* Flush input */
	flush();

	/* Wait for response */
	pause_line(Term->hgt - 1);
}

/*
 * Handle character death
 */
void do_player_death(void)
{
	int ch;

	bool wants_to_quit = FALSE;
	cptr p; 

	time_t death_time;

	/* Prompt */
	if (!cheat_wizard) p = "[(i)nformation, (m)essages, (f)ile dump, (v)iew scores, e(x)amine item, ESC]";
	else p = "[(i)nfo, (m)essages, (f)ile, (v)iew score, e(x)amine, (w)izard off, or ESC]";

	/* Handle retirement */
	if (p_ptr->total_winner) kingly();

	/* Save dead player */
	if (cheat_no_save)
	{
		message(MSG_CHEAT, 0, "Cheat mode enabled - no death save!");
		message_flush();
	}
	else if (!save_player())
	{
		message(MSG_FAIL, 0, "death save failed!");
		message_flush();
	}

	/* Get time of death */
	(void)time(&death_time);

	/* You are dead */
	print_tomb(death_time);

	/* Hack - Know everything upon death */
	death_knowledge();

	/* Enter player in high score list */
	enter_score(death_time);

	/* Flush all input keys */
	flush();

	/* Flush messages */
	message_flush();

	/* Forever */
	while (!wants_to_quit)
	{
		/* Describe options */
		Term_putstr(1, 23, -1, TERM_WHITE, p);

		/* Query */
		ch = inkey();

		switch (ch)
 		{
			/* Exit */
			case ESCAPE:
			{
				if (get_check("Do you want to quit? "))
					wants_to_quit = TRUE;
 
				break;
			}
 
			/* File dump */
			case 'f':
			case 'F':
 			{
				char ftmp[80];

				sprintf(ftmp, "%s.txt", op_ptr->base_name);

				if (get_string("File name: ", ftmp, 80))
				{
					if (ftmp[0] && (ftmp[0] != ' '))
					{
						errr err;
 
						/* Save screen */
						screen_save();
 
						/* Dump a character file */
						err = file_character(ftmp, FALSE);
 
						/* Load screen */
						screen_load();

						/* Check result */
						if (err)
						{
							message(MSG_GENERIC, 0, "Character dump failed!");
						}
						else
						{
							message(MSG_GENERIC, 0, "Character dump successful.");
						}
 
						/* Flush messages */
						message_flush();
 					}
 				}

				break;
 			}

			/* Show more info */
			case 'i':
			case 'I':
			{
				/* Save screen */
				screen_save();
 
				/* Show the character */
				show_info();
 
				/* Load screen */
				screen_load();
 
				break;
			}
 
			/* Show last messages */
			case 'm':
			case 'M':
			{
				/* Save screen */
				screen_save();
 
				/* Display messages */
				do_cmd_messages();
 
				/* Load screen */
				screen_load();

				break;
			}

			/* Show top scores */
			case 'v':
			case 'V':
			{
				/* Save screen */
				screen_save();

				/* Show the scores */
				top_twenty();

				/* Load screen */
				screen_load();

				break;
			}

			/* Examine an item */
			case 'x':
			case 'X':
			{
				/* Save screen */
				screen_save();

				/* Examine items */
				death_examine();

				/* Load screen */
				screen_load();

				break;
			}

			/* Examine an item */
			case 'w':
			case 'W':
			{
				if (!cheat_wizard) continue;

				if (!get_check("Confirm exiting wizard mode (will allow creation of new character)?")) 
					continue;

				cheat_wizard = FALSE;

				if (!save_player())
				{
					message(MSG_FAIL, 0, "death save failed!");
					message_flush();
				}
			}
		}
	}
}
