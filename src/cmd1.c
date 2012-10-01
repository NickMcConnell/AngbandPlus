/* File: cmd1.c */

/* Purpose: Movement commands (part 1) */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"
#define MAX_VAMPIRIC_DRAIN 100


/*
 * Extract the "total damage" from a given object hitting a given monster.
 *
 * Note that "flasks of oil" do NOT do fire damage, although they
 * certainly could be made to do so.  XXX XXX
 *
 * Note that most brands and slays are x3, except Slay Animal (x2),
 * Slay Evil (x2), and Kill dragon (x5).
 */
s32b tot_dam_aux(object_type *o_ptr, s32b tdam, monster_type *m_ptr)
{
	int mult = 1;

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

        u32b f1, f2, f3, f4;

	/* Extract the flags */
        object_flags(o_ptr, &f1, &f2, &f3, &f4);

	/* Some "weapons" and "ammo" do extra damage */
	switch (o_ptr->tval)
	{
		case TV_AMMO:
		case TV_WEAPON:
		case TV_ROD:
		case TV_DIGGING:
		{
			/* Slay Animal */
			if ((f1 & (TR1_SLAY_ANIMAL)) &&
			    (r_ptr->flags3 & (RF3_ANIMAL)))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= (RF3_ANIMAL);
				}

                                if (mult < 4) mult = 4;
			}

			/* Slay Evil */
			if ((f1 & (TR1_SLAY_EVIL)) &&
			    (r_ptr->flags3 & (RF3_EVIL)))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= (RF3_EVIL);
				}

                                if (mult < 4) mult = 4;
			}

			/* Slay Undead */
                        if ((f1 & (TR1_SLAY_UNDEAD)) && (r_ptr->flags3 & (RF3_UNDEAD)))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= (RF3_UNDEAD);
				}

                                if (mult < 6) mult = 6;
			}

                        /* Slay Demon */
                        if ((f1 & (TR1_SLAY_DEMON)) &&
                            (r_ptr->flags3 & (RF3_DEMON)))
			{
				if (m_ptr->ml)
				{
                                        r_ptr->r_flags3 |= (RF3_DEMON);
				}

                                if (mult < 6) mult = 6;
			}

			/* Slay Orc */
			if ((f1 & (TR1_SLAY_ORC)) &&
			    (r_ptr->flags3 & (RF3_ORC)))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= (RF3_ORC);
				}

                                if (mult < 6) mult = 6;
			}

			/* Slay Troll */
			if ((f1 & (TR1_SLAY_TROLL)) &&
			    (r_ptr->flags3 & (RF3_TROLL)))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= (RF3_TROLL);
				}



                                if (mult < 6) mult = 6;
			}

			/* Slay Giant */
			if ((f1 & (TR1_SLAY_GIANT)) &&
			    (r_ptr->flags3 & (RF3_GIANT)))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= (RF3_GIANT);
				}

                                if (mult < 6) mult = 6;
			}

			/* Slay Dragon  */
			if ((f1 & (TR1_SLAY_DRAGON)) &&
			    (r_ptr->flags3 & (RF3_DRAGON)))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= (RF3_DRAGON);
				}

                                if (mult < 6) mult = 6;
			}

			/* Execute Dragon */
			if ((f1 & (TR1_KILL_DRAGON)) &&
			    (r_ptr->flags3 & (RF3_DRAGON)))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= (RF3_DRAGON);
				}

                                if (mult < 10) mult = 10;

			}

                        /* Slay Male */
                        if ((f4 & (TR4_SLAY_MALE)) &&
                            (r_ptr->flags1 & (RF1_MALE)))
			{
				if (m_ptr->ml)
				{
                                        r_ptr->r_flags1 |= (RF1_MALE);
				}

                                if (mult < 6) mult = 6;
			}

                        /* Slay Female */
                        if ((f4 & (TR4_SLAY_FEMALE)) &&
                            (r_ptr->flags1 & (RF1_FEMALE)))
			{
				if (m_ptr->ml)
				{
                                        r_ptr->r_flags1 |= (RF1_FEMALE);
				}

                                if (mult < 6) mult = 6;
			}

			break;
		}
	}


	/* Return the total damage */
	return (tdam * mult);
}


/*
 * Search for hidden things
 */
void search(void)
{
	int y, x, chance;

	s16b this_o_idx, next_o_idx = 0;

	cave_type *c_ptr;

	/* Start with base search ability */
        chance = (p_ptr->stat_ind[A_INT] / 2) + (p_ptr->stat_ind[A_DEX] / 2);

	/* Penalize various conditions */
	if (p_ptr->blind || no_lite()) chance = chance / 10;
	if (p_ptr->confused || p_ptr->image) chance = chance / 10;

	/* Search the nearby grids, which are always in bounds */
	for (y = (py - 1); y <= (py + 1); y++)
	{
		for (x = (px - 1); x <= (px + 1); x++)
		{
			/* Sometimes, notice things */
			if (rand_int(100) < chance)
			{
#ifdef USE_PYTHON
                                if (perform_event(EVENT_SEARCH, Py_BuildValue("(ii)", y, x))) return;
#endif
				/* Access the grid */
				c_ptr = &cave[y][x];

				/* Invisible trap */
				if ((c_ptr->t_idx != 0) &&
				    !(c_ptr->info & CAVE_TRDT))
				{
					/* Pick a trap */
					pick_trap(y, x);

					/* Message */
					msg_print("You have found a trap.");

					/* Disturb */
					disturb(0, 0);
				}

				/* Secret door */
				if (c_ptr->feat == FEAT_SECRET)
				{
					/* Message */
					msg_print("You have found a secret door.");

					/* Pick a door XXX XXX XXX */
					cave_set_feat(y, x, FEAT_DOOR_HEAD + 0x00);

					/* Disturb */
					disturb(0, 0);
				}

				/* Secret ice door */
				if (c_ptr->feat == FEAT_ICE_SECRET)
				{
					/* Message */
					msg_print("You have found a secret door.");

					/* Pick a door XXX XXX XXX */
					cave_set_feat(y, x, FEAT_ICE_DOOR_HEAD + 0x00);

					/* Disturb */
					disturb(0, 0);
				}

				/* Scan all objects in the grid */
				for (this_o_idx = c_ptr->o_idx; this_o_idx; this_o_idx = next_o_idx)
				{
					object_type *o_ptr;
					
					/* Acquire object */
					o_ptr = &o_list[this_o_idx];

					/* Acquire next object */
					next_o_idx = o_ptr->next_o_idx;

					/* Skip non-chests */
					if (o_ptr->tval != TV_CHEST) continue;

					/* Skip non-trapped chests */
					if (!o_ptr->pval) continue;

					/* Identify once */
					if (!object_known_p(o_ptr))
					{
						/* Message */
						msg_print("You have discovered a trap on the chest!");

						/* Know the trap */
						object_known(o_ptr);

						/* Notice it */
						disturb(0, 0);
					}
				}
			}
		}
	}
	/* Now, look for events(hidden switch) on the spot we're on! */
	c_ptr = &cave[py][px];
	if (c_ptr->event == 9)
	{
		char ch;
		cave_type *c2_ptr;
		/* If the event has a condition, ressolve it. */
		if (c_ptr->eventcond > 0)
		{
			if (p_ptr->events[c_ptr->eventcond] == c_ptr->eventcondval)
			{
				get_com("You have found a secret switch! Press it? [y/n]", &ch);
				if (ch == 'y' || ch == 'Y')
				{
					c2_ptr = &cave[c_ptr->eventextra][c_ptr->eventtype];
					c2_ptr->feat = c_ptr->eventextra2;

					/* Set events */
					p_ptr->events[c_ptr->eventset] = c_ptr->eventsetval;

					lite_spot(c_ptr->eventextra, c_ptr->eventtype);

					msg_print("You press the switch...");
					update_and_handle();
				}
			}
		}
		else 
		{
			get_com("You have found a secret switch! Press it? [y/n]", &ch);
			if (ch == 'y' || ch == 'Y')
			{
				c2_ptr = &cave[c_ptr->eventextra][c_ptr->eventtype];
				c2_ptr->feat = c_ptr->eventextra2;

				/* Set events */
				p_ptr->events[c_ptr->eventset] = c_ptr->eventsetval;

				lite_spot(c_ptr->eventextra, c_ptr->eventtype);

				msg_print("You press the switch...");
				update_and_handle();
			}
		}
	}
	if (c_ptr->event == 10)
	{
		char ch;
		cave_type *c2_ptr;
		int difflevel = (dun_level * 3);
		/* If the event has a condition, ressolve it. */
		if (c_ptr->eventcond > 0)
		{
			if (p_ptr->events[c_ptr->eventcond] == c_ptr->eventcondval)
			{
				if (randint(p_ptr->stat_ind[A_INT]) >= randint(difflevel))
				{
					get_com("You have found a secret switch! Press it? [y/n]", &ch);
					if (ch == 'y' || ch == 'Y')
					{
						c2_ptr = &cave[c_ptr->eventextra][c_ptr->eventtype];
						c2_ptr->feat = c_ptr->eventextra2;

						/* Set events */
						p_ptr->events[c_ptr->eventset] = c_ptr->eventsetval;

						lite_spot(c_ptr->eventextra, c_ptr->eventtype);

						msg_print("You press the switch...");
						update_and_handle();
					}
				}
			}
		}
		else 
		{
			if (randint(p_ptr->stat_ind[A_INT]) >= randint(difflevel))
			{
				get_com("You have found a secret switch! Press it? [y/n]", &ch);
				if (ch == 'y' || ch == 'Y')
				{
					c2_ptr = &cave[c_ptr->eventextra][c_ptr->eventtype];
					c2_ptr->feat = c_ptr->eventextra2;

					/* Set events */
					p_ptr->events[c_ptr->eventset] = c_ptr->eventsetval;

					lite_spot(c_ptr->eventextra, c_ptr->eventtype);

					msg_print("You press the switch...");
					update_and_handle();
				}
			}
		}
	}
}




/*
 * Player "wants" to pick up an object or gold.
 * Note that we ONLY handle things that can be picked up.
 * See "move_player()" for handling of other things.
 */
void carry(int pickup)
{
        if(!p_ptr->disembodied) py_pickup_floor(pickup);
}


/*
 * Handle player hitting a real trap
 */
static void hit_trap(void)
{
	bool ident=FALSE;

	cave_type *c_ptr;

	/* Disturb the player */
	disturb(0, 0);

	/* Get the cave grid */
	c_ptr = &cave[py][px];
	if (c_ptr->t_idx != 0)
	{
		ident = player_activate_trap_type(py, px, NULL, -1);
		if (ident)
		{
			t_info[c_ptr->t_idx].ident = TRUE;
			msg_format("You identified the trap as %s.",
				   t_name + t_info[c_ptr->t_idx].name);
		}
	}
}


void touch_zap_player(monster_type *m_ptr)
{
	int aura_damage = 0;
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	if (r_ptr->flags2 & (RF2_AURA_FIRE))
	{
		
			char aura_dam[80];

			aura_damage = damroll(1 + (r_ptr->level / 26), 1 + (r_ptr->level / 17));

			/* Hack -- Get the "died from" name */
			monster_desc(aura_dam, m_ptr, 0x88);

			msg_print("You are suddenly very hot!");

			take_hit(aura_damage, aura_dam);
			r_ptr->r_flags2 |= RF2_AURA_FIRE;
			handle_stuff();
	}


	if (r_ptr->flags2 & (RF2_AURA_ELEC))
	{
			char aura_dam[80];

			aura_damage = damroll(1 + (r_ptr->level / 26), 1 + (r_ptr->level / 17));

			/* Hack -- Get the "died from" name */
			monster_desc(aura_dam, m_ptr, 0x88);

			msg_print("You get zapped!");
			take_hit(aura_damage, aura_dam);
			r_ptr->r_flags2 |= RF2_AURA_ELEC;
			handle_stuff();
	}
}

/*
 * Carried monster can attack too.
 * Based on monst_attack_monst.
 */
void incarnate_monster_attack(s16b m_idx, int x, int y)
{
        monster_type    *t_ptr = &m_list[m_idx];
        monster_race    *r_ptr;
        monster_race    *tr_ptr = &r_info[t_ptr->r_idx];
        cave_type       *c_ptr;
	int             ap_cnt;
	int             ac, rlev,pt;
        char            t_name[80];
        char            temp[80];
	bool            blinked = FALSE, touched = FALSE;
	byte            y_saver = t_ptr->fy;
	byte            x_saver = t_ptr->fx;


        if(!p_ptr->body_monster) return;
        c_ptr = &cave[y][x];

        r_ptr = &r_info[p_ptr->body_monster];

	/* Not allowed to attack */
        if (r_ptr->flags1 & RF1_NEVER_BLOW) return;

	/* Total armor */
	ac = tr_ptr->ac;

	/* Extract the effective monster level */
	rlev = ((r_ptr->level >= 1) ? r_ptr->level : 1);

	/* Get the monster name (or "it") */
	monster_desc(t_name, t_ptr, 0);

	/* Assume no blink */
	blinked = FALSE;

        if (!t_ptr->ml)
	{
		msg_print("You hear noise.");
	}

	/* NewAngband 1.8.0: New monster system, new code! :) */
	/* Go trough every blows of the monster */
        for (ap_cnt = 0; ap_cnt < r_ptr->attacks; ap_cnt++)
	{
		bool visible = FALSE;
		bool obvious = FALSE;

		int             powers[36];
		char            power_desc[36][80];
		char 		powdesc[120];

		int num = 0;
		int i;
		int Power = -1;
                s32b damage = 0;

		bool            flag, redraw;
        	int             ask, plev = p_ptr->lev;

		char            choice;

		char            out_val[160];

		cptr act = NULL;

                /* Extract the attack infomation */
		int effect = r_ptr->blow[ap_cnt].effect;
		int method = r_ptr->blow[ap_cnt].method;
		int d_dice = r_ptr->blow[ap_cnt].d_dice;
		int d_side = r_ptr->blow[ap_cnt].d_side;

		/* Stop attacking if the target dies! */
		if (t_ptr->fx != x_saver || t_ptr->fy != y_saver)
			break;

		if (blinked) /* Stop! */
		{
			/* break; */
		}

		/* Extract visibility (before blink) */
                visible = TRUE;
		
		/* List the powers */
		i = 0;
		while (i < 20 && r_ptr->attack[i].type > 0) 
		{
			if (r_ptr->attack[i].type == 1)
			{
				sprintf(powdesc, "%s  Element: %s  Dam: %dd%d", r_ptr->attack[i].name, get_element_name(r_ptr->attack[i].element), r_ptr->attack[i].ddice, r_ptr->attack[i].dside);
				strcpy(power_desc[num],powdesc);
				powers[num++]=i;
			}
			i++;
		}

        	if(!num) {msg_print("No attacks to use.");return;}

		/* Nothing chosen yet */
		flag = FALSE;

		/* No redraw yet */
		redraw = FALSE;

		/* Build a prompt (accept all spells) */
		if (num <= 26)
		{
			/* Build a prompt (accept all spells) */
        	        strnfmt(out_val, 78, "(Attacks %c-%c, *=List, ESC=exit) Use which attack? ",
			I2A(0), I2A(num - 1));
		}
		else
		{
        	        strnfmt(out_val, 78, "(Attacks %c-%c, *=List, ESC=exit) Use which attack? ",
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

		/* Actually use the power! */
		switch (r_ptr->attack[Power].type)
		{
			/* Melee attack */
			case 1:
			{
				int hit;
				call_lua("player_hit_monster", "(Md)", "d", t_ptr, 0, &hit);
				if (hit == 1)
				{
					int flg = PROJECT_GRID | PROJECT_KILL;
					nevermiss = TRUE;
					msg_format("You hit %s", t_name);
					damage = damroll(r_ptr->attack[Power].ddice, r_ptr->attack[Power].dside);
        				damage *= (p_ptr->skill[18] + 1);
        				damage += ((damage * p_ptr->dis_to_d) / 100);
        				damage += ((damage * p_ptr->stat_ind[A_STR]) / 100);
					/* Some counters... */
					if ((r_ptr->countertype == 1 || r_ptr->countertype == 3 || r_ptr->countertype == 17 || r_ptr->countertype == 19) && randint(100) <= r_ptr->counterchance)
					{
						if (monster_hit_player(t_ptr, 0))
						{
							msg_format("%s blocked your attack!", t_name);
							damage = 0;
						}
					}
					if ((r_ptr->countertype == 4 || r_ptr->countertype == 6  || r_ptr->countertype == 21 || r_ptr->countertype == 23) && randint(100) <= r_ptr->counterchance)
					{
						msg_format("%s blocked your attack!", t_name);
						damage = 0;
					}
					/* Returning counter! */
					if ((r_ptr->countertype == 7 || r_ptr->countertype == 9) && randint(100) <= r_ptr->counterchance)
					{
                                        	msg_print("Damages are reflected to you!");
                                        	(void)project(m_idx, 0, py, px, damage, r_ptr->attack[Power].element, flg);
					}
					/* Block & Return counter! */
					if ((r_ptr->countertype == 10 || r_ptr->countertype == 12) && randint(100) <= r_ptr->counterchance)
					{
						if (monster_hit_player(t_ptr, 0))
						{
							msg_format("%s blocked your attack!", t_name);
							msg_print("Damages are reflected to you!");
                                        		(void)project(m_idx, 0, py, px, damage, r_ptr->attack[Power].element, flg);
							damage = 0;
						}
					}
					if ((r_ptr->countertype == 13 || r_ptr->countertype == 15) && randint(100) <= r_ptr->counterchance)
					{
						msg_format("%s blocked your attack!", t_name);
						msg_print("Damages are reflected to you!");
                                        	(void)project(m_idx, 0, py, px, damage, r_ptr->attack[Power].element, flg);
						damage = 0;
					}
					melee_attack = TRUE;
					(void)project(0, 0, t_ptr->fy, t_ptr->fx, damage, r_ptr->attack[Power].element, flg);
					melee_attack = FALSE;
					nevermiss = FALSE;
				}
				else msg_format("You miss %s", t_name);
				break;
			}
		
			default:
			{
				break;
			}
		}	

		update_and_handle();
	}

	/* Blink away */
	if (blinked)
	{
                msg_print("You flee laughing!");
		
                teleport_player(MAX_SIGHT * 2 + 5);
	}
}

/*
 * Fetch an attack description from dam_*.txt files.
 */

static void flavored_attack(int percent, char* output) {
  if (percent < 5) {
    if (!flavored_attacks) strcpy(output, "You scratch %s.");
    else get_rnd_line("dam_none.txt", output);

  } else if (percent < 30) {
    if (!flavored_attacks) strcpy(output, "You hit %s.");
    else get_rnd_line("dam_med.txt", output);

  } else if (percent < 60) {
    if (!flavored_attacks) strcpy(output, "You wound %s.");
    else get_rnd_line("dam_lots.txt", output);

  } else if (percent < 95) {
    if (!flavored_attacks) strcpy(output, "You cripple %s.");
    else get_rnd_line("dam_huge.txt", output);

  } else {
    if (!flavored_attacks) strcpy(output, "You demolish %s.");
    else get_rnd_line("dam_xxx.txt", output);

  }

}

/*
 * Player attacks a (poor, defenseless) creature        -RAK-
 *
 * If no "weapon" is available, then "punch" the monster one time.
 */
void py_attack(int y, int x, int max_blow)
{
        int             num = 0;
        s32b            k;

	cave_type       *c_ptr = &cave[y][x];

	monster_type    *m_ptr = &m_list[c_ptr->m_idx];
	monster_race    *r_ptr = &r_info[m_ptr->r_idx];

	object_type     *o_ptr;

	char            m_name[80];


	bool            fear = FALSE;
	bool            mdeath = FALSE;

	bool            backstab = FALSE;
	bool            vorpal_cut = FALSE;

	bool            stab_fleeing = FALSE;
	bool            do_quake = FALSE;
	bool            drain_msg = TRUE;
	int             drain_result = 0, drain_heal = 0;
	int             drain_left = MAX_VAMPIRIC_DRAIN;
        u32b            f1, f2, f3, f4; /* A massive hack -- life-draining weapons */
	bool            no_extra = FALSE;
        int             weap;
        int             totalcombo = 0;
        int             maxcombo = 0;
        int             oldpy = py;
        int             oldpx = px;
	int		hit = 0;
        char            ch;

        /* First and foremost, determine the maximum combos the */
        /* player can do(if any) */
        if (p_ptr->skill[7] >= 1)
        {
                int combatskill = p_ptr->skill[7];
                while (combatskill >= 5)
                {
                        maxcombo += 1;
                        combatskill -= 5;
                }
                {
                        int combochance = combatskill * 20;

                        if (combochance > 0 && randint(100) <= combochance)
                        {
                                maxcombo += 1;
                        }
                }
        }

	/* Disturb the player */
	disturb(0, 0);

        if (r_info[p_ptr->body_monster].flags1 & RF1_NEVER_BLOW)
        {
                msg_print("You cannot attack in this form!");
                return;
        }

#ifdef USE_PYTHON
        if (perform_event(EVENT_ATTACK, Py_BuildValue("()"))) return;
#endif

        if (p_ptr->skill[6] >= 30)
	{
		if ((m_ptr->csleep) && (m_ptr->ml))
		{
			/* Can't backstab creatures that we can't see, right? */
			backstab = TRUE;
		}
		else if ((m_ptr->monfear) && (m_ptr->ml))
		{
			stab_fleeing = TRUE;
		}
	}

	/* Extract monster name (or "it") */
	monster_desc(m_name, m_ptr, 0);

	/* Auto-Recall if possible and visible */
	if (m_ptr->ml) monster_race_track(m_ptr->r_idx);

	/* Track a new monster */
	if (m_ptr->ml) health_track(c_ptr->m_idx);

	/* Stop if friendly */
	if (is_pet(m_ptr) &&
	    !(p_ptr->stun || p_ptr->confused || p_ptr->image))
	{
		if (!(inventory[INVEN_WIELD].art_name))
		{
			msg_format("You stop to avoid hitting %s.", m_name);
			return;
		}

	}


	/* Handle player fear */
	if (p_ptr->afraid)
	{
		/* Message */
		if (m_ptr->ml)
			msg_format("You are too afraid to attack %s!", m_name);
		else
			msg_format ("There is something scary in your way!");

		/* Done */
		return;
	}

        /* Monsters that can dont use weapons, us etheir natural attacks */
        if ((p_ptr->body_monster) && (!r_info[p_ptr->body_monster].body_parts[BODY_WEAPON]))
        {
                incarnate_monster_attack(c_ptr->m_idx, y, x);
        }
        /* Otherwise use your weapon(s) */
        else
        {

        /* Attack with first weapon, then with second weapon */
        for (weap = 0; weap <= 1; weap++)
        {
        /* Monster is already dead ? oh :( */
        if (mdeath) break;

        /* Reset the blows counter */
        num = 0;

	/* Access the weapon */
        o_ptr = &inventory[INVEN_WIELD + weap];
	current_weapon = &inventory[INVEN_WIELD + weap];

        /* No weapon found? Go for natural attacks then... */
        /*if (o_ptr->tval == 0 && p_ptr->prace == RACE_MONSTER)*/
        if (o_ptr->tval == 0 && p_ptr->body_monster != 0 && unarmed())
        {
                incarnate_monster_attack(c_ptr->m_idx, y, x);
                return;
        }

        object_flags(o_ptr, &f1, &f2, &f3, &f4);

        if(!(f4 & TR4_NEVER_BLOW))
        {
        int num_blow;

	if (weap == 0) num_blow = p_ptr->num_blow;
	else num_blow = p_ptr->num_blow2;

        /* Restrict to max_blow(if max_blow >= 0) */
        if((max_blow >= 0) && (num_blow > max_blow)) num_blow = max_blow;

	/* Attack once for each legal blow */
        while (num++ < num_blow && c_ptr->m_idx && px == oldpx && py == oldpy)
	{
                int daggerbonus = 0;
                bool usedcombo = FALSE;
		bool blocked = FALSE;

                /* Maybe the player want to use a special move instead...*/
                /* Offer the choice...if possible. */
                if (totalcombo < maxcombo && !mdeath)
                {
			if (unarmed()) get_com("Use a special ability? [y/n/no[t] this turn]", &ch);
			else if (weap == 0) get_com("Use a special ability(weapon 1)? [y/n/no[t] this turn]", &ch);
                        else get_com("Use a special ability(weapon 2)? [y/n/no[t] this turn]", &ch);

                        if (ch == 'y' || ch == 'Y')
                        {
				combatfeat = TRUE;
                                do_cmd_racial_power(1);
                                usedcombo = TRUE;
                                totalcombo += 1;
				combatfeat = FALSE;
                        }

                        if (ch == 't' || ch == 'T')
                        {
                                totalcombo = maxcombo;
                        }
                }

                /* Piercing Stab passive feat */
                if (dagger_check() == TRUE && p_ptr->skill[15] >= 5) daggerbonus = p_ptr->to_d / 4;

                if (!usedcombo)
                {

                /* Test for hit */
		call_lua("player_hit_monster", "(Md)", "d", m_ptr, daggerbonus, &hit);
                if (hit == 1)
		{
			bool critical_hit = FALSE;
			int pcrit = p_ptr->abilities[(CLASS_FIGHTER * 10) + 3] * 5;
			int mcritres = m_ptr->level + m_ptr->str;

			/* Check for Fighter's critical hits! */
			if (p_ptr->abilities[(CLASS_FIGHTER * 10) + 3] >= 1)
			{
				if (randint(pcrit) >= randint(mcritres))
				{
					critical_hit = TRUE;
				}
			}
			

			/* Sound */
			sound(SOUND_HIT);

			/* Hack -- bare hands do one damage */
			k = 1;

			if (f1 & TR1_VORPAL)
				vorpal_cut = TRUE;
			else vorpal_cut = FALSE;

                        if (!o_ptr->k_idx)
                        {
				/* If we have nothing in this slots, but we have in another one */
				/* We're not unarmed. Skip attack phase. */
				if (unarmed())
				{
					call_lua("monk_damages", "", "l", &k);
				
					if (backstab)
					{
						backstab = FALSE;
                                        	k *= (3 + p_ptr->abilities[(CLASS_ROGUE * 10) + 1]);
					}
					else if (stab_fleeing)
					{
                                        	k *= (3 + p_ptr->abilities[(CLASS_ROGUE * 10) + 1]) / 2;
					}

                                	/* No negative damage */
                                	if (k < 0) k = 0;
				}
				else continue;                              
                        }
                        else
			{
				call_lua("weapon_damages", "", "l", &k);
                                k = tot_dam_aux(o_ptr, k, m_ptr);

                                if (backstab)
				{
					backstab = FALSE;
                                        k *= (3 + p_ptr->abilities[(CLASS_ROGUE * 10) + 1]);
				}
				else if (stab_fleeing)
				{
                                        k *= (3 + p_ptr->abilities[(CLASS_ROGUE * 10) + 1]) / 2;
				}

                        	/*k = critical_norm(o_ptr->weight, o_ptr->to_h, k);*/

			}

                        /* May it clone the monster ? */
                        if ((f4 & TR4_CLONE) && magik(30))
			{
                                msg_format("Oh no ! Your weapon clones %^s!", m_name);
                                multiply_monster(c_ptr->m_idx, FALSE, TRUE);
			}

                        /* Penalty for could-2H when having a shield */
                        if ((f4 & TR4_COULD2H) && inventory[INVEN_ARM].k_idx)
                             k /= 2;
	
			/* Physical resistance of monsters... */
			k -= ((k * r_ptr->resistances[GF_PHYSICAL]) / 100);
			r_ptr->r_resist[GF_PHYSICAL] = 1;

                                /* Bosses/Elites can be immune to weapons... */
                                if (m_ptr->abilities & (BOSS_RETURNING))
                                {
                                        int returndamages;
                                        returndamages = k / 2;
                                        msg_print("You hurt yourself!");
                                        take_hit(returndamages, "A monster ability");
                                }
				/* Returning counter! */
				if ((r_ptr->countertype == 7 || r_ptr->countertype == 9) && randint(100) <= r_ptr->counterchance)
				{
                                        msg_print("Damages are reflected to you!");
                                        take_hit(k, "Melee returning counter");
				}
				/* Block & Return counter! */
				if ((r_ptr->countertype == 10 || r_ptr->countertype == 12) && randint(100) <= r_ptr->counterchance)
				{
					if (monster_hit_player(m_ptr, 0))
					{
						msg_format("%s blocked your attack!", m_name);
						msg_print("Damages are reflected to you!");
                                        	take_hit(k, "Melee returning counter");
						k = 0;
						blocked = TRUE;
					}
				}
				if ((r_ptr->countertype == 13 || r_ptr->countertype == 15) && randint(100) <= r_ptr->counterchance)
				{
					msg_format("%s blocked your attack!", m_name);
					msg_print("Damages are reflected to you!");
                                        take_hit(k, "Melee returning counter");
					k = 0;
					blocked = TRUE;
				}

                                if (m_ptr->abilities & (BOSS_HALVE_DAMAGES))
                                {
                                        k = k / 2;
                                }
                                if (m_ptr->abilities & (BOSS_IMMUNE_WEAPONS))
                                {
                                        k = 0;
                                        msg_print("The monster seems to be immune...");
                                }

				/* Some counters... */
				if ((r_ptr->countertype == 1 || r_ptr->countertype == 3 || r_ptr->countertype == 17 || r_ptr->countertype == 19) && randint(100) <= r_ptr->counterchance)
				{
					if (monster_hit_player(m_ptr, 0))
					{
						msg_format("%s blocked your attack!", m_name);
						k = 0;
						blocked = TRUE;
					}
				}
				if ((r_ptr->countertype == 4 || r_ptr->countertype == 6 || r_ptr->countertype == 21 || r_ptr->countertype == 23) && randint(100) <= r_ptr->counterchance)
				{
					msg_format("%s blocked your attack!", m_name);
					k = 0;
					blocked = TRUE;
				}


			/* No negative damage */
			if (k < 0) k = 0;

			/* Lower defense? */
                        if (o_ptr->art_flags4 & (TR4_LOWER_DEF) && !(blocked))
                        {
                                int defamount;
                                char m_name[80];

                                /* Get "the monster" or "it" */
                                monster_desc(m_name, m_ptr, 0);

                                defamount = (damroll(o_ptr->dd, o_ptr->ds) * 3) / 10;
                                if (m_ptr->defense <= 0) defamount = 0;
                                msg_format("%s loses %d defense!", m_name, defamount);
                                m_ptr->defense -= defamount;
                                if (m_ptr->defense <= 0) m_ptr->defense = 0;
                        }
                        /* Lower hit rate? */
                        if (o_ptr->art_flags4 & (TR4_LOWER_HIT) && !(blocked))
                        {
                                int hitamount;
                                char m_name[80];

                                /* Get "the monster" or "it" */
                                monster_desc(m_name, m_ptr, 0);

                                hitamount = (damroll(o_ptr->dd, o_ptr->ds) * 3) / 10;
                                if (m_ptr->hitrate <= 0) hitamount = 0;
                                msg_format("%s loses %d hit rate!", m_name, hitamount);
                                m_ptr->hitrate -= hitamount;
                                if (m_ptr->hitrate <= 0) m_ptr->hitrate = 0;
                        }
			/* Zelar's Disabling Blows! */
                        if (unarmed() && p_ptr->abilities[(CLASS_ZELAR * 10) + 5] >= 1 && !(blocked))
                        {
                                int defreduction = (p_ptr->abilities[(CLASS_ZELAR * 10) + 5] * 15);
                                int speedreduction = 1 + (p_ptr->abilities[(CLASS_ZELAR * 10) + 5] / 2);

                                if (!(m_ptr->abilities & (BOSS_IMMUNE_WEAPONS)) && !(r_ptr->flags1 & (RF1_UNIQUE)))
                                {
                                        m_ptr->hitrate -= defreduction;
                                        m_ptr->defense -= defreduction;
                                        m_ptr->mspeed -= speedreduction;
                                        msg_format("Def/Hit loss: %d, Speed loss: %d.", defreduction, speedreduction);
                                }
                        }

			/* Message */
			if (!(backstab || stab_fleeing) && !(blocked))
			{
                                        if (strchr("vwjmelX,.*", r_ptr->d_char)) {
                                          msg_format("You hit %s.", m_name);
                                        } else {
                                          char buff[255];

                                          flavored_attack((100*k)/m_ptr->maxhp, buff);
                                          msg_format(buff, m_name);
                                        }
			}
			else if (backstab && !(blocked))
				msg_format("You cruelly stab the helpless, sleeping %s!",
				    (r_name + r_info[m_ptr->r_idx].name));
			else if (!(blocked))
				msg_format("You backstab the fleeing %s!",
				    (r_name + r_info[m_ptr->r_idx].name));

			/* Fighter's critical hits! */
			if ((k > 0) && (critical_hit))
			{
				msg_format("%s receives critical hit!", (r_name + r_info[m_ptr->r_idx].name));
				k = k * 2;
				if (!(r_ptr->flags1 & (RF1_UNIQUE)) && !(r_ptr->flags3 & (RF3_NO_STUN)))
				{
					m_ptr->seallight = 2 + (p_ptr->abilities[(CLASS_FIGHTER * 10) + 3] / 20);
				}
			}

			/* Complex message */
			if (wizard)
			{
				msg_format("You do %d (out of %d) damage.", k, m_ptr->hp);
			}
			
			/* Damage, check for fear and death */
			if (mon_take_hit(c_ptr->m_idx, k, &fear, NULL))
			{
				mdeath = TRUE;
				break;
			}
			
			if (is_pet(m_ptr))
			{
				msg_format("%^s gets angry!", m_name);
				set_pet(m_ptr, FALSE);
			}
			if (unarmed()) o_ptr = &inventory[INVEN_HANDS];
			if (o_ptr->brandtype > 0 && !(blocked) && !(mdeath))
			{
				int flg = PROJECT_GRID | PROJECT_KILL;
				no_magic_return = TRUE;
				(void)project(0, o_ptr->brandrad, m_ptr->fy, m_ptr->fx, (o_ptr->branddam * (o_ptr->pval + 1)), o_ptr->brandtype, flg);
				no_magic_return = FALSE;
			}
			/* Access the weapon */
        		o_ptr = &inventory[INVEN_WIELD + weap];
        		object_flags(o_ptr, &f1, &f2, &f3, &f4);
			
			touch_zap_player(m_ptr);

		}

		/* Player misses */
		else
		{
			/* Sound */
			sound(SOUND_MISS);

			backstab = FALSE; /* Clumsy! */

			/* Message */
			msg_format("You miss %s.", m_name);
		}
                } /* End of combo checks */
                update_and_handle();
	}
        }
        else
        {
                msg_print("You can't attack with that weapon.");
        }
        }
        }

	/* Hack -- delay fear messages */
	if (fear && m_ptr->ml)
	{
		/* Sound */
		sound(SOUND_FLEE);

		/* Message */
		msg_format("%^s flees in terror!", m_name);
	}

	/* Mega-Hack -- apply earthquake brand */
	if (do_quake)
	{
		/* Prevent destruction of quest levels and town */
		if (!is_quest(dun_level) && dun_level)
			earthquake(py, px, 10);
	}
}



static bool pattern_tile(int y, int x)
{
	return ((cave[y][x].feat <= FEAT_PATTERN_XTRA2) &&
	    (cave[y][x].feat >= FEAT_PATTERN_START));
}


static bool pattern_seq(int c_y, int c_x, int n_y, int n_x)
{
	if (!(pattern_tile(c_y, c_x)) && !(pattern_tile(n_y, n_x)))
		return TRUE;

	if (cave[n_y][n_x].feat == FEAT_PATTERN_START)
	{
		if ((!(pattern_tile(c_y, c_x))) &&
		    !(p_ptr->confused || p_ptr->stun || p_ptr->image))
		{
                        if (get_check("If you start walking the Straight Road, you must walk the whole way. Ok? "))
				return TRUE;
			else
				return FALSE;
		}
		else
			return TRUE;
	}
	else if ((cave[n_y][n_x].feat == FEAT_PATTERN_OLD) ||
	    (cave[n_y][n_x].feat == FEAT_PATTERN_END) ||
	    (cave[n_y][n_x].feat == FEAT_PATTERN_XTRA2))
	{
		if (pattern_tile(c_y, c_x))
		{
			return TRUE;
		}
		else
		{
                        msg_print("You must start walking the Straight Road from the startpoint.");
			return FALSE;
		}
	}
	else if ((cave[n_y][n_x].feat == FEAT_PATTERN_XTRA1)||
	    (cave[c_y][c_x].feat == FEAT_PATTERN_XTRA1))
	{
		return TRUE;
	}
	else if (cave[c_y][c_x].feat == FEAT_PATTERN_START)
	{
		if (pattern_tile(n_y, n_x))
			return TRUE;
		else
		{
                        msg_print("You must walk the Straight Road in correct order.");
			return FALSE;
		}
	}
	else if ((cave[c_y][c_x].feat == FEAT_PATTERN_OLD) ||
	    (cave[c_y][c_x].feat == FEAT_PATTERN_END) ||
	    (cave[c_y][c_x].feat == FEAT_PATTERN_XTRA2))
	{
		if (!pattern_tile(n_y, n_x))
		{
                        msg_print("You may not step off from the Straight Road.");
			return FALSE;
		}
		else
		{
			return TRUE;
		}
	}
	else
	{
		if (!pattern_tile(c_y, c_x))
		{
                        msg_print("You must start walking the Straight Road from the startpoint.");
			return FALSE;
		}
		else
		{
			byte ok_move = FEAT_PATTERN_START;
			switch (cave[c_y][c_x].feat)
			{
				case FEAT_PATTERN_1:
					ok_move = FEAT_PATTERN_2;
					break;
				case FEAT_PATTERN_2:
					ok_move = FEAT_PATTERN_3;
					break;
				case FEAT_PATTERN_3:
					ok_move = FEAT_PATTERN_4;
					break;
				case FEAT_PATTERN_4:
					ok_move = FEAT_PATTERN_1;
					break;
				default:
					if (wizard)
                                                msg_format("Funny Straight Road walking, %d.", cave[c_y][c_x]);
					return TRUE; /* Goof-up */
			}

			if ((cave[n_y][n_x].feat == ok_move) ||
			    (cave[n_y][n_x].feat == cave[c_y][c_x].feat))
				return TRUE;
			else
			{
				if (!pattern_tile(n_y, n_x))
                                        msg_print("You may not step off from the Straight Road.");
				else
                                        msg_print("You must walk the Straight Road in correct order.");

				return FALSE;
			}
		}
	}
}



bool player_can_enter(byte feature)
{
	bool pass_wall;

	/* Player can not walk through "walls" unless in Shadow Form */
        if (p_ptr->wraith_form)
		pass_wall = TRUE;
	else
		pass_wall = FALSE;

	
	switch (feature)
	{
		case FEAT_DEEP_WATER:
			{
                                int wt = (max_carry() * 100) / 2;
				if ((total_weight < wt) || (p_ptr->ffall))
					return (TRUE);
				else
					return (FALSE);
			}

		case FEAT_SHAL_LAVA:
			{
				if ((p_ptr->ffall))
					return (TRUE);
				else
					return (FALSE);
			}

		case FEAT_DEEP_LAVA:
			{
				if ((p_ptr->ffall))
					return (TRUE);
				else
					return (FALSE);
			}

		case FEAT_TREES:
		case FEAT_SNOW_TREES:
			{
                                if ((p_ptr->ffall) || (p_ptr->pclass==CLASS_RANGER)|| (p_ptr->abilities[(CLASS_RANGER * 10) + 1] >= 1))
					return (TRUE);
				else
					return (FALSE);
			}

                default:
			{
                                if ((p_ptr->climb) && (f_info[feature].flags1 & FF1_CAN_CLIMB))
					return (TRUE);
                                if ((p_ptr->fly) && (f_info[feature].flags1 & FF1_CAN_FLY))
					return (TRUE);
                                else if ((p_ptr->ffall) && ((f_info[feature].flags1 & FF1_CAN_FLY) || (f_info[feature].flags1 & FF1_CAN_LEVITATE)))
					return (TRUE);
                                else if ((pass_wall) && (f_info[feature].flags1 & FF1_CAN_PASS))
					return (TRUE);
                                else if (f_info[feature].flags1 & FF1_NO_WALK)
                                        return (FALSE);
			}
	}

	return (TRUE);
}


/*
 * Move player in the given direction, with the given "pickup" flag.
 *
 * This routine should (probably) always induce energy expenditure.
 *
 * Note that moving will *always* take a turn, and will *always* hit
 * any monster which might be in the destination grid.  Previously,
 * moving into walls was "free" and did NOT hit invisible monsters.
 */
void move_player_aux(int dir, int do_pickup, int run)
{
        int y, x, tmp;

	cave_type *c_ptr;
	monster_type *m_ptr;
	monster_race *r_ptr;

	char m_name[80];

	bool p_can_pass_walls = FALSE;

	bool oktomove = TRUE;

	/* This one is for the Elemental Lord's aura. */
	int spellstat;

	spellstat = (p_ptr->stat_ind[A_INT] - 5);
	if (spellstat < 0) spellstat = 0;

        if (p_ptr->disembodied)
                tmp = dir;
        else
                tmp = dir;
  
        /* Find the result of moving */
        y = py + ddy[tmp];
        x = px + ddx[tmp];

	/* Examine the destination */
	c_ptr = &cave[y][x];

	/* Get the monster */
	m_ptr = &m_list[c_ptr->m_idx];

	/* Race info. */
	r_ptr = &r_info[m_ptr->r_idx];

	/* Run a script? */
	call_lua("before_player_move", "(dd)", "", x, y);

	/* Player can not walk through "walls"... */
	/* unless in Shadow Form */
        if (p_ptr->wraith_form)
		p_can_pass_walls = TRUE;

	if ((cave[y][x].feat >= FEAT_PERM_EXTRA) &&
	    (cave[y][x].feat <= FEAT_PERM_SOLID))
	{
		p_can_pass_walls = FALSE;
	}

	/* Hack -- attack monsters */
	if (c_ptr->m_idx && (m_ptr->ml || cave_floor_bold(y,x) || p_can_pass_walls))
	{

		/* Attack -- only if we can see it OR it is not in a wall */
		if (is_pet(m_ptr) &&
		    !(p_ptr->confused || p_ptr->image || p_ptr->stun) &&
		    (pattern_seq(py, px, y, x)) &&
		    ((cave_floor_bold(y, x)) || (p_can_pass_walls)))
		{
			m_ptr->csleep = 0;

			/* Extract monster name (or "it") */
			monster_desc(m_name, m_ptr, 0);

			/* Auto-Recall if possible and visible */
			if (m_ptr->ml) monster_race_track(m_ptr->r_idx);

			/* Track a new monster */
			if (m_ptr->ml) health_track(c_ptr->m_idx);

			/* displace? */
			if (cave_floor_bold(py, px) ||
			    (r_info[m_ptr->r_idx].flags2 & RF2_PASS_WALL))
			{
				if (r_ptr->flags7 & (RF7_NEVER_MOVE_FRIENDLY))
				{
					msg_format("You cannot push %s", m_name);
					energy_use = 0;
					oktomove = FALSE;
				}
				else
				{
					msg_format("You push past %s.", m_name);
					m_ptr->fy = py;
					m_ptr->fx = px;
					cave[py][px].m_idx = c_ptr->m_idx;
					c_ptr->m_idx = 0;
					update_mon(cave[py][px].m_idx, TRUE);
				}
			}
			else
			{
				msg_format("%^s is in your way!", m_name);
				energy_use = 0;
				oktomove = FALSE;
			}

			/* now continue on to 'movement' */
		}
		else
		{
                        /* py_attack(y, x,-1);*/
			call_lua("py_attack", "(ddd)", "d", y, x, -1);
			update_and_handle();
			oktomove = FALSE;
		}
	}
	/* No monster, but check for events. */
	/* A specific item is needed to enter this grid. */
	else if (c_ptr->event == 3)
	{
		if (c_ptr->eventcond > 0)
		{
			if (p_ptr->events[c_ptr->eventcond] == c_ptr->eventcondval)
			{
				int j = 0;
				object_type *o_ptr;
				bool canenter = FALSE;
        			while (j <= 52)
        			{
                			o_ptr = &inventory[j];

                			if ((o_ptr->tval == c_ptr->eventtype) && (o_ptr->sval == c_ptr->eventextra))
					{
						canenter = TRUE;
						j = 52;
					}

                			j++;
        			}
				if (!(canenter))
				{
					msg_print("You cannot open this door.");
					running = 0;
					oktomove = FALSE;
				}
				else oktomove = TRUE;

				p_ptr->events[c_ptr->eventset] = c_ptr->eventsetval;
			}
			else oktomove = TRUE;
		}
		else
		{
			int j = 0;
			object_type *o_ptr;
			bool canenter = FALSE;
        		while (j <= 52)
        		{
                		o_ptr = &inventory[j];

                		if ((o_ptr->tval == c_ptr->eventtype) && (o_ptr->sval == c_ptr->eventextra))
				{
					canenter = TRUE;
					j = 52;
				}

                		j++;
        		}
			if (!(canenter))
			{
				msg_print("You cannot open this door.");
				running = 0;
				oktomove = FALSE;
			}
			else oktomove = TRUE;

			p_ptr->events[c_ptr->eventset] = c_ptr->eventsetval;
		}
	}

	else if ((c_ptr->feat == FEAT_DARK_PIT) && (!p_ptr->ffall && !p_ptr->fly))
	{
		msg_print("You can't cross the chasm.");
		running = 0;
		oktomove = FALSE;
	}

        else if ((c_ptr->feat == FEAT_MOUNTAIN || c_ptr->feat == FEAT_GLACIER) && !p_ptr->climb && !p_can_pass_walls)
	{
		msg_print("You can't climb the mountains!");
		running = 0;
		oktomove = FALSE;
	}
	/*
         * Player can't move through trees
         * 
         * Rangers and Ents can move
	 */
        else if (c_ptr->feat == FEAT_TREES || c_ptr->feat == FEAT_SNOW_TREES)
	{
                oktomove = FALSE;
                if ((p_ptr->pclass == CLASS_RANGER) || p_ptr->fly || p_can_pass_walls || p_ptr->abilities[(CLASS_RANGER * 10) + 1] >= 1) oktomove=TRUE;
	}

	else if ((c_ptr->feat >= FEAT_QUEST_ENTER) &&
		(c_ptr->feat <= FEAT_QUEST_EXIT))
	{
		oktomove = TRUE;
	}

        else if ((c_ptr->feat >= FEAT_ALTAR_HEAD) &&
                (c_ptr->feat <= FEAT_ALTAR_TAIL))
        {
                oktomove = TRUE;
        }

#ifdef ALLOW_EASY_DISARM /* TNB */

	/* Disarm a visible trap */
	else if ((do_pickup != easy_disarm) &&
		(c_ptr->t_idx != 0) && (c_ptr->info & CAVE_TRDT))
	{
                (void) do_cmd_disarm_aux(y, x, tmp);
		return;
	}

#endif /* ALLOW_EASY_DISARM -- TNB */

	/* Player can not walk through "walls" unless in wraith form...*/
        else if ((f_info[c_ptr->feat].flags1 & FF1_WALL) &&
                (!(f_info[c_ptr->feat].flags1 & FF1_CAN_PASS) ||
                 (!p_can_pass_walls)))
	{
		oktomove = FALSE;

		/* Disturb the player */
		disturb(0, 0);

		/* Notice things in the dark */
		if ((!(c_ptr->info & (CAVE_MARK))) &&
		    (p_ptr->blind || !(c_ptr->info & (CAVE_LITE))))
		{
			/* Rubble */
			if (c_ptr->feat == FEAT_RUBBLE)
			{
				msg_print("You feel some rubble blocking your way.");
				c_ptr->info |= (CAVE_MARK);
				lite_spot(y, x);
			}

			/* Closed door */
			else if (c_ptr->feat < FEAT_SECRET)
			{
				msg_print("You feel a closed door blocking your way.");
				c_ptr->info |= (CAVE_MARK);
				lite_spot(y, x);
			}

                        /* Mountain and levitation */
                        else if ((c_ptr->feat == FEAT_MOUNTAIN || c_ptr->feat == FEAT_GLACIER) && p_ptr->climb)
			{
                                oktomove=TRUE;
                        }

                        /* Between */
                        else if (c_ptr->feat == FEAT_BETWEEN)
			{
                                oktomove=TRUE;
			}

			/* Wall (or secret door) */
			else
			{
				msg_print("You feel a wall blocking your way.");
				c_ptr->info |= (CAVE_MARK);
				lite_spot(y, x);
			}
		}

		/* Notice things */
		else
		{
			/* Rubble */
			if (c_ptr->feat == FEAT_RUBBLE)
			{
                                if (!easy_tunnel)
                                {
                                        msg_print("There is rubble blocking your way.");

                                        if (!(p_ptr->confused || p_ptr->stun || p_ptr->image))
                                                energy_use = 0;
                                        /*
                                         * Well, it makes sense that you lose time bumping into
                                         * a wall _if_ you are confused, stunned or blind; but
                                         * typing mistakes should not cost you a turn...
                                         */
                                }
                                else
                                {
                                        do_cmd_tunnel_aux(y, x, dir);
                                        return;
                                }
			}
			/* Closed doors */
			else if (c_ptr->feat < FEAT_SECRET || (c_ptr->feat >= FEAT_ICE_DOOR_HEAD && c_ptr->feat < FEAT_ICE_SECRET))
			{
#ifdef ALLOW_EASY_OPEN

				if (easy_open)
				{
					if (easy_open_door(y, x)) return;
				}
				else

#endif /* ALLOW_EASY_OPEN */

				{
					msg_print("There is a closed door blocking your way.");

					if (!(p_ptr->confused || p_ptr->stun || p_ptr->image))
						energy_use = 0;
				}
			}

                        /* Mountain and levitation */
                        else if ((c_ptr->feat == FEAT_MOUNTAIN || c_ptr->feat == FEAT_GLACIER) && (p_ptr->climb || p_ptr->fly))
			{
                                oktomove=TRUE;
			}

                        /* Between */
                        else if (c_ptr->feat == FEAT_BETWEEN)
			{
                                oktomove=TRUE;
			}

			/* Wall (or secret door) */
                        else
			{
                                if (!easy_tunnel)
                                {
                                        msg_print("There is a wall blocking your way.");

                                        if (!(p_ptr->confused || p_ptr->stun || p_ptr->image))
                                                energy_use = 0;
                                }
                                else
                                {
                                        do_cmd_tunnel_aux(y, x, dir);
                                        return;
                                }
			}
		}

		/* Sound */
		sound(SOUND_HITWALL);
	}

	/* Normal movement */
	if (!pattern_seq(py, px, y, x))
	{
		if (!(p_ptr->confused || p_ptr->stun || p_ptr->image))
		{
			energy_use = 0;
		}

		disturb(0,0); /* To avoid a loop with running */

		oktomove = FALSE;
	}

	/* Normal movement */
	if (oktomove)
	{
                int oy, ox;

#ifdef USE_PYTHON
                if (perform_event(EVENT_MOVE, Py_BuildValue("(iiii)", y, x, py, px))) return;
#endif

		/* Save old location */
		oy = py;
		ox = px;

		/* Move the player */
		py = y;
		px = x;

		/* Redraw new spot */
		lite_spot(py, px);

		/* Redraw old spot */
		lite_spot(oy, ox);

		/* Sound */
		/* sound(SOUND_WALK); */

		/* Check for new panel (redraw map) */
		verify_panel();

		/* Run a script? */
		call_lua("after_player_move", "(dd)", "", x, y);

		/* Auras! */
		/* The Paladin's Aura Of Life! :) */
                if (p_ptr->abilities[(CLASS_PALADIN * 10) + 2] >= 1) aura_of_life();

		/* Elemental Lord's aura! */
                if (p_ptr->abilities[(CLASS_ELEM_LORD * 10) + 5] >= 1 && p_ptr->auraon) elem_lord_aura((p_ptr->abilities[(CLASS_ELEM_LORD * 10) + 5] * 5) * spellstat, 2 + (p_ptr->abilities[(CLASS_ELEM_LORD * 10) + 5] / 30));

                /* Justice Warrior's Aura Of Evil Repulsing! */
                if (p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 2] >= 1) aura_repulse_evil(2 + (p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 2] / 20));

                /* For get everything when requested hehe I'm *NASTY* */
                if (dun_level && (d_info[dungeon_type].flags1 & DF1_FORGET))
                {
                        wiz_dark();
                }

		/* Update stuff */
                p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW);

		/* Update the monsters */
		p_ptr->update |= (PU_DISTANCE);

		/* Window stuff */
                if (!run) p_ptr->window |= (PW_OVERHEAD);


		/* Spontaneous Searching */
                if (randint(100) < ((p_ptr->stat_ind[A_INT] / 5) + (p_ptr->stat_ind[A_DEX] / 5)))
		{
			search();
		}

		/* Continuous Searching */
		if (p_ptr->searching)
		{
			search();
		}

		/* Handle "objects" */
#ifdef ALLOW_EASY_DISARM /* TNB */

		carry(do_pickup != always_pickup);

#else /* ALLOW_EASY_DISARM -- TNB */

		carry(do_pickup);

#endif /* ALLOW_EASY_DISARM -- TNB */

		/* First, check for a script. */
		if (c_ptr->script == 1)
		{
			call_lua(c_ptr->script_name, "()", "");
		}
		/* Handle special events(if any) */
		else if (c_ptr->event == 1)
		{
			/* If the event has a condition, ressolve it. */
			if (c_ptr->eventcond > 0)
			{
				if (p_ptr->events[c_ptr->eventcond] == c_ptr->eventcondval)
				{
					show_dialog(c_ptr->eventtype);
					/* Set events */
					p_ptr->events[c_ptr->eventset] = c_ptr->eventsetval;
				}
			}
			else 
			{
				show_dialog(c_ptr->eventtype);

				/* Set events */
				p_ptr->events[c_ptr->eventset] = c_ptr->eventsetval;
			}
		}
		else if (c_ptr->event == 4)
		{
			/* If the event has a condition, ressolve it. */
			if (c_ptr->eventcond > 0)
			{
				if (p_ptr->events[c_ptr->eventcond] == c_ptr->eventcondval)
				{
					/* Player enters a new quest */
                			p_ptr->oldpy = py;
                			p_ptr->oldpx = px;
					/* Only change this if we're in town. */
					if (p_ptr->inside_quest == 0)
					{
						p_ptr->startx = px;
						p_ptr->starty = py;
					}

					p_ptr->inside_quest = c_ptr->eventtype;

					/* Set events */
					p_ptr->events[c_ptr->eventset] = c_ptr->eventsetval;

					/* Set dun level to 0. It may change when quest is generated. */
					/*dun_level = 0;*/

					p_ptr->questx = c_ptr->eventextra;
					p_ptr->questy = c_ptr->eventextra2;
					p_ptr->leaving = TRUE;
				}
			}
			else
			{
				/* Player enters a new quest */
                		p_ptr->oldpy = py;
                		p_ptr->oldpx = px;
				/* Only change this if we're in town. */
				if (p_ptr->inside_quest == 0)
				{
					p_ptr->startx = px;
					p_ptr->starty = py;
				}
				
				p_ptr->inside_quest = c_ptr->eventtype;

				/* Set events */
				p_ptr->events[c_ptr->eventset] = c_ptr->eventsetval;

				/* Set dun level to 0. It may change when quest is generated. */
				/*dun_level = 0;*/

				p_ptr->questx = c_ptr->eventextra;
				p_ptr->questy = c_ptr->eventextra2;
				p_ptr->leaving = TRUE;
			}
		}
		else if (c_ptr->event == 5 && c_ptr->feat != 6)
		{
			/* If the event has a condition, ressolve it. */
			if (c_ptr->eventcond > 0)
			{
				if (p_ptr->events[c_ptr->eventcond] == c_ptr->eventcondval)
				{
					/* Player enters a new quest */
                			p_ptr->oldpy = py;
                			p_ptr->oldpx = px;
					p_ptr->startx = c_ptr->eventtype;
					p_ptr->starty = c_ptr->eventextra;

					p_ptr->inside_quest = 0;
					p_ptr->wild_mode = FALSE;
					dun_level = 0;

					/* Set events */
					p_ptr->events[c_ptr->eventset] = c_ptr->eventsetval;

					p_ptr->leaving = TRUE;
				}
			}
			else
			{
				/* Player enters a new quest */
                		p_ptr->oldpy = py;
                		p_ptr->oldpx = px;
				p_ptr->startx = c_ptr->eventtype;
				p_ptr->starty = c_ptr->eventextra;

				p_ptr->inside_quest = 0;
				p_ptr->wild_mode = FALSE;
				dun_level = 0;

				/* Set events */
				p_ptr->events[c_ptr->eventset] = c_ptr->eventsetval;

				p_ptr->leaving = TRUE;
			}
		}
		/* Event 6 is an item, 7 is a monster. Just set events if needed... */
		else if (c_ptr->event == 6 || c_ptr->event == 7)
		{
			/* If the event has a condition, ressolve it. */
			if (c_ptr->eventcond > 0)
			{
				if (p_ptr->events[c_ptr->eventcond] == c_ptr->eventcondval)
				{
					/* Set events */
					p_ptr->events[c_ptr->eventset] = c_ptr->eventsetval;
				}
			}
			else p_ptr->events[c_ptr->eventset] = c_ptr->eventsetval;
		}
		/* Change town. */
		else if (c_ptr->event == 8)
		{
			/* If the event has a condition, ressolve it. */
			if (c_ptr->eventcond > 0)
			{
				if (p_ptr->events[c_ptr->eventcond] == c_ptr->eventcondval)
				{
					/* Player enters a new quest */
                			p_ptr->oldpy = py;
                			p_ptr->oldpx = px;
					p_ptr->startx = c_ptr->eventextra;
					p_ptr->starty = c_ptr->eventextra2;

					p_ptr->town_num = c_ptr->eventtype;
					p_ptr->inside_quest = 0;
					p_ptr->wild_mode = FALSE;
					dun_level = 0;

					/* Set events */
					p_ptr->events[c_ptr->eventset] = c_ptr->eventsetval;

					p_ptr->leaving = TRUE;
				}
			}
			else
			{
				/* Player enters a new quest */
                		p_ptr->oldpy = py;
                		p_ptr->oldpx = px;
				p_ptr->startx = c_ptr->eventextra;
				p_ptr->starty = c_ptr->eventextra2;

				p_ptr->town_num = c_ptr->eventtype;
				p_ptr->inside_quest = 0;
				p_ptr->wild_mode = FALSE;
				dun_level = 0;

				/* Set events */
				p_ptr->events[c_ptr->eventset] = c_ptr->eventsetval;

				p_ptr->leaving = TRUE;
			}
		}
		/* If there is no event, and we are on the town's borders... */
		/* Go out in the wild! :) */
		else if ((dun_level == 0) && ((px <= 1) || (py <= 1) || (px >= (p_ptr->cur_wid - 2)) || (py >= (p_ptr->cur_hgt - 2))))
		{
			if (((px <= 1) && ((p_ptr->wild_x - 1) < 0)) || ((py <= 1) && ((p_ptr->wild_y - 1) < 0)) || ((px >= (p_ptr->cur_wid - 2)) && ((p_ptr->wild_x + 2) > wild_max_x)) || ((py >= (p_ptr->cur_hgt - 2)) && ((p_ptr->wild_y + 2) > wild_max_y)))
			{
				msg_print("You have reached the edge of the world.");
				oktomove = FALSE;
			}
			else
			{
				/* Player enters a new quest */ 
                		p_ptr->oldpy = py;
                		p_ptr->oldpx = px;
				p_ptr->wild_startx = px;
				p_ptr->wild_starty = py;

				/* Determine the spot in the wild. */
				if (px <= 1) p_ptr->wild_startx = (MAX_WID - 3);
				if (px >= (p_ptr->cur_wid - 2)) p_ptr->wild_startx = 2;
				if (py <= 1) p_ptr->wild_starty = (MAX_HGT - 3);
				if (py >= (p_ptr->cur_hgt - 2)) p_ptr->wild_starty = 2;

				if (px <= 1) p_ptr->wild_x--;
				if (px >= (p_ptr->cur_wid - 2)) p_ptr->wild_x++;
				if (py <= 1) p_ptr->wild_y--;
				if (py >= (p_ptr->cur_hgt - 2)) p_ptr->wild_y++;

				p_ptr->wild_mode = TRUE;
				p_ptr->inside_quest = 0;
				dun_level = 0;

				p_ptr->leaving = TRUE;
			}
		}


		/* Handle "store doors" */
		else if ((c_ptr->feat >= FEAT_SHOP_HEAD) &&
		    (c_ptr->feat <= FEAT_SHOP_TAIL))
		{
			/* Disturb */
			disturb(0, 0);

			/* Hack -- Enter store */
			command_new = '_';
		}

		/* Handle "building doors" -KMW- */
		else if ((c_ptr->feat >= FEAT_BLDG_HEAD) &&
		    (c_ptr->feat <= FEAT_BLDG_TAIL))
		{
			/* Disturb */
			disturb(0, 0);

			/* Hack -- Enter building */
			command_new = ']';
		}

                else if (cave[y][x].feat >= FEAT_ALTAR_HEAD &&
                         cave[y][x].feat <= FEAT_ALTAR_TAIL)
                         {
                            cptr name = f_name + f_info[cave[y][x].feat].name;
                            cptr pref = (is_a_vowel(name[0])) ? "an" : "a";
                            msg_format("You see %s %s.", pref, name);
                         }

		/* Discover invisible traps */
		else if ((c_ptr->t_idx != 0) &&
			 !(f_info[cave[y][x].feat].flags1 & FF1_DOOR))
		{
			/* Disturb */
			disturb(0, 0);

			if (!(c_ptr->info & CAVE_TRDT))
			{
				/* Message */
				msg_print("You found a trap!");

				/* Pick a trap */
				pick_trap(py, px);
			}

			/* Hit the trap */
			hit_trap();
		}
	}
}

void move_player(int dir, int do_pickup)
{
        move_player_aux(dir, do_pickup, 0);
        /* Do an update */
        update_and_handle();
}


/*
 * Hack -- Check for a "known wall" (see below)
 */
static int see_wall(int dir, int y, int x)
{
	/* Get the new location */
	y += ddy[dir];
	x += ddx[dir];

	/* Illegal grids are not known walls */
	if (!in_bounds2(y, x)) return (FALSE);

	/* Non-wall grids are not known walls */
	if (cave[y][x].feat < FEAT_SECRET) return (FALSE);

        if ((cave[y][x].feat == FEAT_DEEP_WATER) ||
           ((cave[y][x].feat >= FEAT_SHAL_WATER) &&
            (cave[y][x].feat <= FEAT_GRASS)) || (cave[y][x].feat == FEAT_SNOW)) return (FALSE);

	if ((cave[y][x].feat >= FEAT_SHOP_HEAD) &&
	    (cave[y][x].feat <= FEAT_SHOP_TAIL)) return (FALSE);

	if ((cave[y][x].feat >= FEAT_BLDG_HEAD) &&
	    (cave[y][x].feat <= FEAT_BLDG_TAIL)) return (FALSE);

        if ((f_info[cave[y][x].feat].flags1 & FF1_CAN_RUN)) return (FALSE);

	/* Must be known to the player */
	if (!(cave[y][x].info & (CAVE_MARK))) return (FALSE);

	/* Default */
	return (TRUE);
}


/*
 * Hack -- Check for an "unknown corner" (see below)
 */
static int see_nothing(int dir, int y, int x)
{
	/* Get the new location */
	y += ddy[dir];
	x += ddx[dir];

	/* Illegal grids are unknown */
	if (!in_bounds2(y, x)) return (TRUE);

	/* Memorized grids are always known */
	if (cave[y][x].info & (CAVE_MARK)) return (FALSE);

	/* Non-floor grids are unknown */
	if (!cave_floor_bold(y, x)) return (TRUE);

	/* Viewable door/wall grids are known */
	if (player_can_see_bold(y, x)) return (FALSE);

	/* Default */
	return (TRUE);
}





/*
 * The running algorithm:                       -CJS-
 *
 * In the diagrams below, the player has just arrived in the
 * grid marked as '@', and he has just come from a grid marked
 * as 'o', and he is about to enter the grid marked as 'x'.
 *
 * Of course, if the "requested" move was impossible, then you
 * will of course be blocked, and will stop.
 *
 * Overview: You keep moving until something interesting happens.
 * If you are in an enclosed space, you follow corners. This is
 * the usual corridor scheme. If you are in an open space, you go
 * straight, but stop before entering enclosed space. This is
 * analogous to reaching doorways. If you have enclosed space on
 * one side only (that is, running along side a wall) stop if
 * your wall opens out, or your open space closes in. Either case
 * corresponds to a doorway.
 *
 * What happens depends on what you can really SEE. (i.e. if you
 * have no light, then running along a dark corridor is JUST like
 * running in a dark room.) The algorithm works equally well in
 * corridors, rooms, mine tailings, earthquake rubble, etc, etc.
 *
 * These conditions are kept in static memory:
 * find_openarea         You are in the open on at least one
 * side.
 * find_breakleft        You have a wall on the left, and will
 * stop if it opens
 * find_breakright       You have a wall on the right, and will
 * stop if it opens
 *
 * To initialize these conditions, we examine the grids adjacent
 * to the grid marked 'x', two on each side (marked 'L' and 'R').
 * If either one of the two grids on a given side is seen to be
 * closed, then that side is considered to be closed. If both
 * sides are closed, then it is an enclosed (corridor) run.
 *
 * LL           L
 * @x          LxR
 * RR          @R
 *
 * Looking at more than just the immediate squares is
 * significant. Consider the following case. A run along the
 * corridor will stop just before entering the center point,
 * because a choice is clearly established. Running in any of
 * three available directions will be defined as a corridor run.
 * Note that a minor hack is inserted to make the angled corridor
 * entry (with one side blocked near and the other side blocked
 * further away from the runner) work correctly. The runner moves
 * diagonally, but then saves the previous direction as being
 * straight into the gap. Otherwise, the tail end of the other
 * entry would be perceived as an alternative on the next move.
 *
 * #.#
 * ##.##
 * .@x..
 * ##.##
 * #.#
 *
 * Likewise, a run along a wall, and then into a doorway (two
 * runs) will work correctly. A single run rightwards from @ will
 * stop at 1. Another run right and down will enter the corridor
 * and make the corner, stopping at the 2.
 *
 * #@x    1
 * ########### ######
 * 2        #
 * #############
 * #
 *
 * After any move, the function area_affect is called to
 * determine the new surroundings, and the direction of
 * subsequent moves. It examines the current player location
 * (at which the runner has just arrived) and the previous
 * direction (from which the runner is considered to have come).
 *
 * Moving one square in some direction places you adjacent to
 * three or five new squares (for straight and diagonal moves
 * respectively) to which you were not previously adjacent,
 * marked as '!' in the diagrams below.
 *
 * ...!   ...
 * .o@!   .o.!
 * ...!   ..@!
 * !!!
 *
 * You STOP if any of the new squares are interesting in any way:
 * for example, if they contain visible monsters or treasure.
 *
 * You STOP if any of the newly adjacent squares seem to be open,
 * and you are also looking for a break on that side. (that is,
 * find_openarea AND find_break).
 *
 * You STOP if any of the newly adjacent squares do NOT seem to be
 * open and you are in an open area, and that side was previously
 * entirely open.
 *
 * Corners: If you are not in the open (i.e. you are in a corridor)
 * and there is only one way to go in the new squares, then turn in
 * that direction. If there are more than two new ways to go, STOP.
 * If there are two ways to go, and those ways are separated by a
 * square which does not seem to be open, then STOP.
 *
 * Otherwise, we have a potential corner. There are two new open
 * squares, which are also adjacent. One of the new squares is
 * diagonally located, the other is straight on (as in the diagram).
 * We consider two more squares further out (marked below as ?).
 *
 * We assign "option" to the straight-on grid, and "option2" to the
 * diagonal grid, and "check_dir" to the grid marked 's'.
 *
 * .s
 * @x?
 * #?
 *
 * If they are both seen to be closed, then it is seen that no
 * benefit is gained from moving straight. It is a known corner.
 * To cut the corner, go diagonally, otherwise go straight, but
 * pretend you stepped diagonally into that next location for a
 * full view next time. Conversely, if one of the ? squares is
 * not seen to be closed, then there is a potential choice. We check
 * to see whether it is a potential corner or an intersection/room entrance.
 * If the square two spaces straight ahead, and the space marked with 's'
 * are both blank, then it is a potential corner and enter if find_examine
 * is set, otherwise must stop because it is not a corner.
 */




/*
 * Hack -- allow quick "cycling" through the legal directions
 */
static byte cycle[] =
{ 1, 2, 3, 6, 9, 8, 7, 4, 1, 2, 3, 6, 9, 8, 7, 4, 1 };

/*
 * Hack -- map each direction into the "middle" of the "cycle[]" array
 */
static byte chome[] =
{ 0, 8, 9, 10, 7, 0, 11, 6, 5, 4 };

/*
 * The direction we are running
 */
static byte find_current;

/*
 * The direction we came from
 */
static byte find_prevdir;

/*
 * We are looking for open area
 */
static bool find_openarea;

/*
 * We are looking for a break
 */
static bool find_breakright;
static bool find_breakleft;



/*
 * Initialize the running algorithm for a new direction.
 *
 * Diagonal Corridor -- allow diaginal entry into corridors.
 *
 * Blunt Corridor -- If there is a wall two spaces ahead and
 * we seem to be in a corridor, then force a turn into the side
 * corridor, must be moving straight into a corridor here. ???
 *
 * Diagonal Corridor    Blunt Corridor (?)
 *       # #                  #
 *       #x#                 @x#
 *       @p.                  p
 */
static void run_init(int dir)
{
	int             row, col, deepleft, deepright;
	int             i, shortleft, shortright;


	/* Save the direction */
	find_current = dir;

	/* Assume running straight */
	find_prevdir = dir;

	/* Assume looking for open area */
	find_openarea = TRUE;

	/* Assume not looking for breaks */
	find_breakright = find_breakleft = FALSE;

	/* Assume no nearby walls */
	deepleft = deepright = FALSE;
	shortright = shortleft = FALSE;

	/* Find the destination grid */
	row = py + ddy[dir];
	col = px + ddx[dir];

	/* Extract cycle index */
	i = chome[dir];

	/* Check for walls */
	if (see_wall(cycle[i+1], py, px))
	{
		find_breakleft = TRUE;
		shortleft = TRUE;
	}
	else if (see_wall(cycle[i+1], row, col))
	{
		find_breakleft = TRUE;
		deepleft = TRUE;
	}

	/* Check for walls */
	if (see_wall(cycle[i-1], py, px))
	{
		find_breakright = TRUE;
		shortright = TRUE;
	}
	else if (see_wall(cycle[i-1], row, col))
	{
		find_breakright = TRUE;
		deepright = TRUE;
	}

	/* Looking for a break */
	if (find_breakleft && find_breakright)
	{
		/* Not looking for open area */
		find_openarea = FALSE;

		/* Hack -- allow angled corridor entry */
		if (dir & 0x01)
		{
			if (deepleft && !deepright)
			{
				find_prevdir = cycle[i - 1];
			}
			else if (deepright && !deepleft)
			{
				find_prevdir = cycle[i + 1];
			}
		}

		/* Hack -- allow blunt corridor entry */
		else if (see_wall(cycle[i], row, col))
		{
			if (shortleft && !shortright)
			{
				find_prevdir = cycle[i - 2];
			}
			else if (shortright && !shortleft)
			{
				find_prevdir = cycle[i + 2];
			}
		}
	}
}


/*
 * Update the current "run" path
 *
 * Return TRUE if the running should be stopped
 */
static bool run_test(void)
{
	int         prev_dir, new_dir, check_dir = 0;
	int         row, col;
	int         i, max, inv;
	int         option = 0, option2 = 0;
	cave_type   *c_ptr;

	/* Where we came from */
	prev_dir = find_prevdir;


	/* Range of newly adjacent grids */
	max = (prev_dir & 0x01) + 1;


	/* Look at every newly adjacent square. */
	for (i = -max; i <= max; i++)
	{
		s16b this_o_idx, next_o_idx = 0;


		/* New direction */
		new_dir = cycle[chome[prev_dir] + i];

		/* New location */
		row = py + ddy[new_dir];
		col = px + ddx[new_dir];

		/* Access grid */
		c_ptr = &cave[row][col];


		/* Visible monsters abort running */
		if (c_ptr->m_idx)
		{
			monster_type *m_ptr = &m_list[c_ptr->m_idx];

			/* Visible monster */
			if (m_ptr->ml) return (TRUE);
		}

		/* Visible objects abort running */
		for (this_o_idx = c_ptr->o_idx; this_o_idx; this_o_idx = next_o_idx)
		{
			object_type *o_ptr;
			
			/* Acquire object */
			o_ptr = &o_list[this_o_idx];

			/* Acquire next object */
			next_o_idx = o_ptr->next_o_idx;

			/* Visible object */
			if (o_ptr->marked) return (TRUE);
		}


		/* Assume unknown */
		inv = TRUE;

		/* Check memorized grids */
		if (c_ptr->info & (CAVE_MARK))
		{
			bool notice = TRUE;

			/* Examine the terrain */
			switch (c_ptr->feat)
			{
				/* Floors */
				case FEAT_FLOOR:

				/* Secret doors */
				case FEAT_SECRET:

				/* Normal veins */
				case FEAT_MAGMA:
				case FEAT_QUARTZ:

				/* Hidden treasure */
				case FEAT_MAGMA_H:
				case FEAT_QUARTZ_H:

				/* Walls */
				case FEAT_WALL_EXTRA:
				case FEAT_WALL_INNER:
				case FEAT_WALL_OUTER:
				case FEAT_WALL_SOLID:
				case FEAT_PERM_EXTRA:
				case FEAT_PERM_INNER:
				case FEAT_PERM_OUTER:
				case FEAT_PERM_SOLID:
				/* dirt, grass, trees, ... */
				case FEAT_SHAL_WATER:
				case FEAT_DIRT:
				case FEAT_GRASS:
				case FEAT_DARK_PIT:
				case FEAT_TREES:
				case FEAT_MOUNTAIN:
				case FEAT_SNOW:
				case FEAT_SNOW_TREES:
				case FEAT_GLACIER:
				case FEAT_ICE_WALL:
				case FEAT_PERM_ICE_WALL:
				{
					/* Ignore */
					notice = FALSE;

					/* Done */
					break;
				}

				/* quest features */
				case FEAT_QUEST_ENTER:
				case FEAT_QUEST_EXIT:
				{
					/* Notice */
					notice = TRUE;

					/* Done */
					break;
				}

				case FEAT_DEEP_LAVA:
				case FEAT_SHAL_LAVA:
				{

					/* Done */
					break;
				}

				case FEAT_DEEP_WATER:
				{
					/* Ignore */
					if (p_ptr->ffall) notice = FALSE;

					/* Done */
					break;
				}

				/* Open doors */
				case FEAT_OPEN:
				case FEAT_BROKEN:
				case FEAT_ICE_OPEN:
				case FEAT_ICE_BROKEN:
				{
					/* Option -- ignore */
					if (find_ignore_doors) notice = FALSE;

					/* Done */
					break;
				}

				/* Stairs */
				case FEAT_LESS:
				case FEAT_MORE:
				{
					/* Option -- ignore */
					if (find_ignore_stairs) notice = FALSE;

					/* Done */
					break;
				}
			}

                        if(f_info[c_ptr->feat].flags1 & FF1_DONT_NOTICE_RUNNING) notice = FALSE;

			/* Interesting feature */
			if (notice) return (TRUE);

			/* The grid is "visible" */
			inv = FALSE;
		}

		/* Analyze unknown grids and floors */
		if (inv || cave_floor_bold(row, col))
		{
			/* Looking for open area */
			if (find_openarea)
			{
				/* Nothing */
			}

			/* The first new direction. */
			else if (!option)
			{
				option = new_dir;
			}

			/* Three new directions. Stop running. */
			else if (option2)
			{
				return (TRUE);
			}

			/* Two non-adjacent new directions.  Stop running. */
			else if (option != cycle[chome[prev_dir] + i - 1])
			{
				return (TRUE);
			}

			/* Two new (adjacent) directions (case 1) */
			else if (new_dir & 0x01)
			{
				check_dir = cycle[chome[prev_dir] + i - 2];
				option2 = new_dir;
			}

			/* Two new (adjacent) directions (case 2) */
			else
			{
				check_dir = cycle[chome[prev_dir] + i + 1];
				option2 = option;
				option = new_dir;
			}
		}

		/* Obstacle, while looking for open area */
		else
		{
			if (find_openarea)
			{
				if (i < 0)
				{
					/* Break to the right */
					find_breakright = TRUE;
				}

				else if (i > 0)
				{
					/* Break to the left */
					find_breakleft = TRUE;
				}
			}
		}
	}


	/* Looking for open area */
	if (find_openarea)
	{
		/* Hack -- look again */
		for (i = -max; i < 0; i++)
		{
			new_dir = cycle[chome[prev_dir] + i];

			row = py + ddy[new_dir];
			col = px + ddx[new_dir];

			/* Access grid */
			c_ptr = &cave[row][col];

			/* Unknown grid or non-wall XXX XXX XXX cave_floor_grid(c_ptr)) */
			if (!(c_ptr->info & (CAVE_MARK)) ||
			    ((c_ptr->feat < FEAT_SECRET) ||
                            (c_ptr->feat == FEAT_DEEP_WATER) || (c_ptr->feat == FEAT_SNOW) ||
                            ((c_ptr->feat >= FEAT_SHAL_WATER) &&
				 (c_ptr->feat <= FEAT_GRASS))))

			{
				/* Looking to break right */
				if (find_breakright)
				{
					return (TRUE);
				}
			}

			/* Obstacle */
			else
			{
				/* Looking to break left */
				if (find_breakleft)
				{
					return (TRUE);
				}
			}
		}

		/* Hack -- look again */
		for (i = max; i > 0; i--)
		{
			new_dir = cycle[chome[prev_dir] + i];

			row = py + ddy[new_dir];
			col = px + ddx[new_dir];

			/* Access grid */
			c_ptr = &cave[row][col];

			/* Unknown grid or non-wall XXX XXX XXX cave_floor_grid(c_ptr)) */
			if (!(c_ptr->info & (CAVE_MARK)) ||
			    ((c_ptr->feat < FEAT_SECRET) ||
                            (c_ptr->feat == FEAT_DEEP_WATER) || (c_ptr->feat == FEAT_SNOW) ||
                            ((c_ptr->feat >= FEAT_SHAL_WATER) &&
				 (c_ptr->feat <= FEAT_GRASS))))

			{
				/* Looking to break left */
				if (find_breakleft)
				{
					return (TRUE);
				}
			}

			/* Obstacle */
			else
			{
				/* Looking to break right */
				if (find_breakright)
				{
					return (TRUE);
				}
			}
		}
	}


	/* Not looking for open area */
	else
	{
		/* No options */
		if (!option)
		{
			return (TRUE);
		}

		/* One option */
		else if (!option2)
		{
			/* Primary option */
			find_current = option;

			/* No other options */
			find_prevdir = option;
		}

		/* Two options, examining corners */
		else if (find_examine && !find_cut)
		{
			/* Primary option */
			find_current = option;

			/* Hack -- allow curving */
			find_prevdir = option2;
		}

		/* Two options, pick one */
		else
		{
			/* Get next location */
			row = py + ddy[option];
			col = px + ddx[option];

			/* Don't see that it is closed off. */
			/* This could be a potential corner or an intersection. */
			if (!see_wall(option, row, col) ||
			    !see_wall(check_dir, row, col))
			{
				/* Can not see anything ahead and in the direction we */
				/* are turning, assume that it is a potential corner. */
				if (find_examine &&
				    see_nothing(option, row, col) &&
				    see_nothing(option2, row, col))
				{
					find_current = option;
					find_prevdir = option2;
				}

				/* STOP: we are next to an intersection or a room */
				else
				{
					return (TRUE);
				}
			}

			/* This corner is seen to be enclosed; we cut the corner. */
			else if (find_cut)
			{
				find_current = option2;
				find_prevdir = option2;
			}

			/* This corner is seen to be enclosed, and we */
			/* deliberately go the long way. */
			else
			{
				find_current = option;
				find_prevdir = option2;
			}
		}
	}


	/* About to hit a known wall, stop */
	if (see_wall(find_current, py, px))
	{
		return (TRUE);
	}


	/* Failure */
	return (FALSE);
}



/*
 * Take one step along the current "run" path
 */
void run_step(int dir)
{
	/* Start running */
	if (dir)
	{
		/* Hack -- do not start silly run */
		if (see_wall(dir, py, px) &&
		   (cave[py+ddy[dir]][px+ddx[dir]].feat != FEAT_TREES) && (cave[py+ddy[dir]][px+ddx[dir]].feat != FEAT_SNOW_TREES))
		{
			/* Message */
			msg_print("You cannot run in that direction.");

			/* Disturb */
			disturb(0, 0);

			/* Done */
			return;
		}

		/* Calculate torch radius */
		p_ptr->update |= (PU_TORCH);

		/* Recenter the player view */
		if (center_player) verify_panel();

		/* Initialize */
		run_init(dir);
	}

	/* Keep running */
	else
	{
		/* Update run */
		if (run_test())
		{
			/* Disturb */
			disturb(0, 0);

			/* Done */
			return;
		}
	}

	/* Decrease the run counter */
	if (--running <= 0) return;

	/* Take time */
	energy_use = 100;


	/* Move the player, using the "pickup" flag */
#ifdef ALLOW_EASY_DISARM /* TNB */

        move_player_aux(find_current, FALSE, 1);

#else /* ALLOW_EASY_DISARM -- TNB */

        move_player_aux(find_current, always_pickup, 1);

#endif /* ALLOW_EASY_DISARM -- TNB */
}

/*
 * Take care of the various things that can happen when you step
 * into a space. (Objects, traps, and stores.)
 */
void step_effects(int y, int x, int do_pickup)
{
	/* Handle "objects" */
        py_pickup_floor(do_pickup);

	/* Handle "store doors" */
        if ((cave[y][x].feat >= FEAT_SHOP_HEAD) &&
                (cave[y][x].feat <= FEAT_SHOP_TAIL))
	{
		/* Disturb */
		disturb(0, 0);

		/* Hack -- Enter store */
                command_new = KTRL('V');
	}

	/* Discover/set off traps */
        else if (cave[y][x].t_idx != 0)
	{
		/* Disturb */
		disturb(0, 0);

		if (!(cave[y][x].info & CAVE_TRDT))
		{
			/* Message */
			msg_print("You found a trap!");

			/* Pick a trap */
			pick_trap(y, x);
		}

		/* Hit the trap */
                hit_trap();
	}
}

/*
 * Issue a pet command
 */
void do_cmd_pet(void)
{
	int             i = 0;
	int             num = 0;
	int             powers[36];
	char            power_desc[36][80];
	bool            flag, redraw;
	int             ask;
	char            choice;
	char            out_val[160];
	int             pets = 0, pet_ctr = 0;
	bool            all_pets = FALSE;
	monster_type    *m_ptr;
	monster_race	*r_ptr;


	for (num = 0; num < 36; num++)
	{
		powers[num] = 0;
		strcpy(power_desc[num], "");
	}

	num = 0;

	if (p_ptr->confused)
	{
		msg_print("You are too confused to command your pets");
		energy_use = 0;
		return;
	}

	/* Calculate pets */
	/* Process the monsters (backwards) */
	for (pet_ctr = m_max - 1; pet_ctr >= 1; pet_ctr--)
	{
		/* Access the monster */
		m_ptr = &m_list[pet_ctr];

		r_ptr = &r_info[m_ptr->r_idx];

		if (is_pet(m_ptr) && !(r_ptr->flags7 & RF7_TOWNSFOLK) && !(r_ptr->flags7 & RF7_GUARD) && (r_ptr->extra2 == 0)) pets++;
	}

	if (pets == 0)
	{
		msg_print("You have no pets.");
		energy_use = 0;
		return;
	}
	else
	{
		strcpy(power_desc[num], "dismiss pets");
		powers[num++] = 1;
		strcpy(power_desc[num], "call pets");
		powers[num++] = 2;
		strcpy(power_desc[num], "follow me");
		powers[num++] = 6;
		strcpy(power_desc[num], "seek and destroy");
		powers[num++] = 3;
		if (p_ptr->pet_open_doors)
			strcpy(power_desc[num], "disallow open doors");
		else
			strcpy(power_desc[num], "allow open doors");
		powers[num++] = 4;
		if (p_ptr->pet_pickup_items)
			strcpy(power_desc[num], "disallow pickup items");
		else
			strcpy(power_desc[num], "allow pickup items");
		powers[num++] = 5;
	}

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	/* Build a prompt (accept all spells) */
	if (num <= 26)
	{
		/* Build a prompt (accept all spells) */
		strnfmt(out_val, 78, "(Command %c-%c, *=List, ESC=exit) Select a command: ",
			I2A(0), I2A(num - 1));
	}
	else
	{
		strnfmt(out_val, 78, "(Command %c-%c, *=List, ESC=exit) Select a command: ",
			I2A(0), '0' + num - 27);
	}

	/* Get a command from the user */
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

				prt("", y++, x);

				while (ctr < num)
				{
					sprintf(dummy, "%c) %s", I2A(ctr), power_desc[ctr]);
					prt(dummy, y + ctr, x);
					ctr++;
				}

				if (ctr < 17)
				{
					prt("", y + ctr, x);
				}
				else
				{
					prt("", y + 17, x);
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
		return;
	}

	switch (powers[i])
	{
		case 1: /* Dismiss pets */
		{
			int Dismissed = 0;

			if (get_check("Dismiss all pets? ")) all_pets = TRUE;

			/* Process the monsters (backwards) */
			for (pet_ctr = m_max - 1; pet_ctr >= 1; pet_ctr--)
			{
				/* Access the monster */
				m_ptr = &m_list[pet_ctr];

				r_ptr = &r_info[m_ptr->r_idx];

				if (is_pet(m_ptr) && !(r_ptr->flags7 & RF7_TOWNSFOLK) && !(r_ptr->flags7 & RF7_GUARD) && (r_ptr->extra2 == 0)) /* Get rid of it! */
				{
					bool delete_this = FALSE;

					if (all_pets)
						delete_this = TRUE;
					else
					{
						char friend_name[80], check_friend[80];
						monster_desc(friend_name, m_ptr, 0x80);
						sprintf(check_friend, "Dismiss %s? ", friend_name);

						if (get_check(check_friend))
							delete_this = TRUE;
					}

					if (delete_this)
					{
						delete_monster_idx(pet_ctr);
						Dismissed++;
					}
				}
			}

			msg_format("You have dismissed %d pet%s.", Dismissed,
				(Dismissed == 1 ? "" : "s"));
			break;
		}
		/* Call pets */
		case 2:
		{
			p_ptr->pet_follow_distance = 1;
			break;
		}
                /* "Seek and destroy" */
		case 3:
		{
			p_ptr->pet_follow_distance = 255;
			break;
		}
		/* flag - allow pets to open doors */
		case 4:
		{
			p_ptr->pet_open_doors = !p_ptr->pet_open_doors;
			break;
		}
		/* flag - allow pets to pickup items */
		case 5:
		{
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
						monster_drop_carried_objects(m_ptr);
					}
				}
			}

			break;
		}
		/* "Follow Me" */
		case 6:
		{
			p_ptr->pet_follow_distance = 6;
			break;
		}
	}
}

/* A very simple hit rate system...yet, it's effective! */
bool player_hit_monster(monster_type *m_ptr, int bonus)
{
        int phit, proll, mroll;
        int mistpenalities = 25 + (p_ptr->abilities[(CLASS_SHADOW * 10) + 6] / 2);
        cave_type *c_ptr;

        /* First, let's calculate the player's hit rate! */
        phit = (p_ptr->lev + p_ptr->to_h) + bonus;

        /* Somehow, we should not miss this attack... */
        if (nevermiss == TRUE) return (TRUE);

        /* If the hit rate is negative or 0, well, give up, you can't hit! ;) */
        if (phit <= 0) return (FALSE);

        /* Now, let's roll the dices! Player's hit rate VS monster's def */
        proll = randint(phit);
        mroll = randint(m_ptr->defense);

        /* Enemies in the dark mist are easier to hit! */
        c_ptr = &cave[m_ptr->fy][m_ptr->fx];
        if (c_ptr->feat == FEAT_DARK_MIST)
        {
                int rollpenality;
                rollpenality = mroll * (mistpenalities / 100);
                mroll -= rollpenality;
        }

        if (proll >= mroll) return (TRUE);
        else if (always_hit_check()) return (TRUE);
        else return (FALSE);
}

/* How a monster hit you is not much more complicated... */
bool monster_hit_player(monster_type *m_ptr, int bonus)
{
	int hit;
	call_lua("monster_hit_player", "(Md)", "d", m_ptr, bonus, &hit);

	if (hit == 1) return (TRUE);

	return (FALSE);
}

/* And a monster hitting another monster... */
bool monster_hit_monster(monster_type *m_ptr, monster_type *t_ptr)
{
	int hit;
	call_lua("monster_hit_monster", "(MM)", "d", m_ptr, t_ptr, &hit);

	if (hit == 1) return (TRUE);

	return (FALSE);
}

bool always_hit_check()
{
        u32b f1, f2, f3, f4;
        int i;
        object_type *o_ptr;

        i = 24;
        while (i <= 52)
        {
                /* Get the item */
                o_ptr = &inventory[i];

                /* Examine the item */
                object_flags(o_ptr, &f1, &f2, &f3, &f4);

                /* Check for the ALWAYS_HIT flag */
                if (o_ptr->k_idx && (f4 & (TR4_ALWAYS_HIT)))
                {
                        return (TRUE);
                }

                i++;
        }
        /* Default */
        return (FALSE);
}        

bool protection_check()
{
        u32b f1, f2, f3, f4;
        int i;
        object_type *o_ptr;

        i = 24;
        while (i <= 52)
        {
                /* Get the item */
                o_ptr = &inventory[i];

                /* Examine the item */
                object_flags(o_ptr, &f1, &f2, &f3, &f4);

                /* Check for the PROTECTION flag */
                if (o_ptr->k_idx && (f4 & (TR4_PROTECTION)))
                {
                        return (TRUE);
                }

                i++;
        }
        /* Default */
        return (FALSE);
}        

bool standing_on_forest()
{
        cave_type *c_ptr;
        c_ptr = &cave[py][px];
        if (c_ptr->feat == FEAT_TREES || c_ptr->feat == FEAT_SNOW_TREES || c_ptr->feat == FEAT_GRASS) return (TRUE);

        /* Default */
        return (FALSE);
}

/* New Monster Mage ability! :) */
void monstrous_wave()
{
        int dir, typ;
        s32b dam;
        monster_race *r_ptr = &r_info[p_ptr->body_monster];

	bool visible = FALSE;
	bool obvious = FALSE;

	int             powers[36];
	char            power_desc[36][80];
	char 		powdesc[120];
	int num = 0;
	int i;
	int Power = -1;
        s32b damage = 0;

	bool            flag, redraw;
        int             ask;

	char            choice;

	char            out_val[160];
	cptr act = NULL;
		
	/* List the powers */
	i = 0;
	while (i < 20 && r_ptr->attack[i].type > 0) 
	{
		if (r_ptr->attack[i].type == 1)
		{
			sprintf(powdesc, "%s  Element: %s  Dam: %dd%d", r_ptr->attack[i].name, get_element_name(r_ptr->attack[i].element), r_ptr->attack[i].ddice, r_ptr->attack[i].dside);
			strcpy(power_desc[num],powdesc);
			powers[num++]=i;
		}
		i++;
	}

        if(!num) {msg_print("No attacks to use.");return;}

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	/* Build a prompt (accept all spells) */
	if (num <= 26)
	{
		/* Build a prompt (accept all spells) */
        	strnfmt(out_val, 78, "(Attacks %c-%c, *=List, ESC=exit) Use which attack? ",
		I2A(0), I2A(num - 1));
	}
	else
	{
        	strnfmt(out_val, 78, "(Attacks %c-%c, *=List, ESC=exit) Use which attack? ",
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
                return;
	}

        dam = (damroll(r_ptr->attack[Power].ddice, r_ptr->attack[Power].dside));
	dam = dam * (p_ptr->skill[18] + 1);
	dam += ((dam * p_ptr->to_d) / 100);
	dam += ((dam * p_ptr->stat_ind[A_STR]) / 100);
	dam += ((dam * ((p_ptr->abilities[(CLASS_MONSTER_MAGE * 10) + 7] - 1) * 20)) / 100);

        typ = r_ptr->attack[Power].type;

        /* Actually cast the wave! */
        if (!get_rep_dir(&dir)) return; 
        chain_attack(dir, typ, dam, 0, 5);
        energy_use = 100;
}

/* Critical Hits ability! */
s32b critical_hits(s32b dam, monster_type *m_ptr)
{
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	if (!(r_ptr->flags3 & (RF3_NO_STUN)))
	{
		int ppower = p_ptr->abilities[(CLASS_FIGHTER * 10) + 3] * 5;
		int mpower = m_ptr->level + m_ptr->str;
		if (randint(ppower) >= randint(mpower))
		{
			msg_print("Critical hit!");
			if (!(r_ptr->flags1 & (RF1_UNIQUE)))
			{
				m_ptr->seallight = 2 + (p_ptr->abilities[(CLASS_FIGHTER * 10) + 3] / 20);
			}
			dam = dam * 2;
			return (dam);
		}
	}

	/* Default */
	return (dam);
}