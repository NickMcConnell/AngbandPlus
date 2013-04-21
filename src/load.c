/* File: load.c */

/*
 * Copyright (c) 1997 Ben Harrison, and others
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"


/*
 * Local "savefile" pointer
 */
static FILE *fff;

/*
 * Hack -- old "encryption" byte
 */
static byte xor_byte;

/*
 * Hack -- simple "checksum" on the actual values
 */
static u32b v_check = 0L;

/*
 * Hack -- simple "checksum" on the encoded bytes
 */
static u32b x_check = 0L;




/*
 * Hack -- Show information on the screen, one line at a time.
 *
 * Avoid the top two lines, to avoid interference with "msg_print()".
 */
static void note(cptr msg)
{
	static int y = 2;

	/* Draw the message */
	prt(msg, y, 0);

	/* Advance one line (wrap if needed) */
	if (++y >= screen_y)
		y = 2;

	/* Flush it */
	Term_fresh();
}



/*
 * The following functions are used to load the basic building blocks
 * of savefiles.  They also maintain the "checksum" info for 2.7.0+
 */

static byte sf_get(void)
{
	byte c, v;

	/* Get a character, decode the value */
	c = getc(fff) & 0xFF;
	v = c ^ xor_byte;
	xor_byte = c;

	/* Maintain the checksum info */
	v_check += v;
	x_check += xor_byte;

	/* Return the value */
	return (v);
}

static void rd_byte(byte * ip)
{
	*ip = sf_get();
}

static void rd_u16b(u16b * ip)
{
	(*ip) = sf_get();
	(*ip) |= ((u16b) (sf_get()) << 8);
}

static void rd_s16b(s16b * ip)
{
	rd_u16b((u16b *) ip);
}

static void rd_u32b(u32b * ip)
{
	(*ip) = sf_get();
	(*ip) |= ((u32b) (sf_get()) << 8);
	(*ip) |= ((u32b) (sf_get()) << 16);
	(*ip) |= ((u32b) (sf_get()) << 24);
}

static void rd_s32b(s32b * ip)
{
	rd_u32b((u32b *) ip);
}


/*
 * Hack -- read a string
 */
static void rd_string(char *str, int max)
{
	int i;

	/* Read the string */
	for (i = 0; TRUE; i++)
	{
		byte tmp8u;

		/* Read a byte */
		rd_byte(&tmp8u);

		/* Collect string while legal */
		if (i < max)
			str[i] = tmp8u;

		/* End of string */
		if (!tmp8u)
			break;
	}

	/* Terminate */
	str[max - 1] = '\0';
}


/*
 * Hack -- strip some bytes
 */
static void strip_bytes(int n)
{
	byte tmp8u;

	/* Strip the bytes */
	while (n--)
		rd_byte(&tmp8u);
}


/*
 * Read an object
 */
static object_type *rd_item_aux(bool store)
{
	u32b f1, f2, f3;
	s16b kind;

	object_kind *k_ptr;

	char buf[128];

	object_type *o_ptr;


	/* Kind */
	rd_s16b(&kind);

	/* No object here. */
	if (kind == 0)
		return NULL;

	if (store)
	{
		MAKE(o_ptr, object_type);
	}
	else
	{
		o_ptr = new_object();
	}

	o_ptr->k_idx = kind;

	/* Special flags. */
	rd_u32b(&o_ptr->flags1);
	rd_u32b(&o_ptr->flags2);
	rd_u32b(&o_ptr->flags3);

	/* Type/Subtype */
	rd_byte(&o_ptr->tval);
	rd_byte(&o_ptr->sval);

	/* Special pval */
	rd_s16b(&o_ptr->pval);

	rd_byte(&o_ptr->discount);
	rd_byte(&o_ptr->number);
	rd_s16b(&o_ptr->weight);
	rd_s16b(&o_ptr->chp);
	rd_s16b(&o_ptr->mhp);

	rd_byte(&o_ptr->stuff);
	rd_byte(&o_ptr->fate);

	rd_byte(&o_ptr->name1);
	rd_byte(&o_ptr->name2);
	rd_s16b(&o_ptr->timeout);

	rd_s16b(&o_ptr->to_h);
	rd_s16b(&o_ptr->to_d);
	rd_s16b(&o_ptr->to_a);
	rd_s16b(&o_ptr->ac);
	rd_byte(&o_ptr->dd);
	rd_byte(&o_ptr->ds);

	rd_byte(&o_ptr->ident);

	rd_byte(&o_ptr->marked);

	rd_byte(&o_ptr->tag);

	/* Inscription */
	rd_string(buf, 128);

	/* Save the inscription */
	if (buf[0])
		o_ptr->note = quark_add(buf);

	/* Obtain the "kind" template */
	k_ptr = &k_info[o_ptr->k_idx];

	/* Hack -- notice "broken" items */
	if (k_ptr->cost <= 0)
		o_ptr->ident |= (IDENT_BROKEN);


	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);


	/* Paranoia */
	if (o_ptr->name1)
	{
		artifact_type *a_ptr;

		/* Obtain the artifact info */
		a_ptr = &a_info[o_ptr->name1];

		/* Verify that artifact */
		if (!a_ptr->name)
			o_ptr->name1 = 0;
	}

	/* Paranoia */
	if (o_ptr->name2)
	{
		ego_item_type *e_ptr;

		/* Obtain the ego-item info */
		e_ptr = &e_info[o_ptr->name2];

		/* Verify that ego-item */
		if (!e_ptr->name)
			o_ptr->name2 = 0;
	}

	return o_ptr;
}



static object_type *rd_item(void)
{
	return rd_item_aux(FALSE);
}

static object_type *rd_item_store(void)
{
	return rd_item_aux(TRUE);
}


/*
 * Read a monster
 */
static void rd_monster(monster_type * m_ptr)
{
	object_type *o_ptr;

	/* Read the monster race */
	rd_s16b(&m_ptr->r_idx);

	/* Read the other information */
	rd_byte(&m_ptr->fy);
	rd_byte(&m_ptr->fx);
	rd_s16b(&m_ptr->hp);
	rd_s16b(&m_ptr->maxhp);
	rd_s16b(&m_ptr->csleep);
	rd_byte(&m_ptr->mspeed);
	rd_byte(&m_ptr->energy);
	rd_byte(&m_ptr->stunned);
	rd_byte(&m_ptr->confused);
	rd_byte(&m_ptr->monfear);
	rd_byte(&m_ptr->is_pet);
	rd_byte(&m_ptr->fate);
	rd_s16b(&m_ptr->random_name_idx);
	rd_s16b(&m_ptr->mflag);

	/* Read the monster's inventory. */
	while (1)
	{
		o_ptr = rd_item();

		if (!o_ptr)
			break;

		monster_inven_carry(m_ptr, o_ptr);
	}
}




/*
 * Read the monster lore
 */
static void rd_lore(int r_idx)
{
	byte tmp8u;

	monster_race *r_ptr = &r_info[r_idx];


	/* Count sights/deaths/kills */
	rd_s16b(&r_ptr->r_sights);
	rd_s16b(&r_ptr->r_deaths);
	rd_s16b(&r_ptr->r_pkills);
	rd_s16b(&r_ptr->r_tkills);

	/* Count wakes and ignores */
	rd_byte(&r_ptr->r_wake);
	rd_byte(&r_ptr->r_ignore);

	/* Extra stuff */
	rd_byte(&r_ptr->r_xtra1);
	rd_byte(&r_ptr->r_xtra2);

	/* Count drops */
	rd_byte(&r_ptr->r_drop_gold);
	rd_byte(&r_ptr->r_drop_item);

	/* Count spells */
	rd_byte(&r_ptr->r_cast_inate);
	rd_byte(&r_ptr->r_cast_spell);

	/* Count blows of each type */
	rd_byte(&r_ptr->r_blows[0]);
	rd_byte(&r_ptr->r_blows[1]);
	rd_byte(&r_ptr->r_blows[2]);
	rd_byte(&r_ptr->r_blows[3]);

	/* Memorize flags */
	rd_u32b(&r_ptr->r_flags1);
	rd_u32b(&r_ptr->r_flags2);
	rd_u32b(&r_ptr->r_flags3);
	rd_u32b(&r_ptr->r_flags4);
	rd_u32b(&r_ptr->r_flags5);
	rd_u32b(&r_ptr->r_flags6);
	rd_u32b(&r_ptr->r_flags7);


	/* Read the "Racial" monster limit per level */
	rd_byte(&r_ptr->max_num);

	/* Later (?) */
	rd_byte(&tmp8u);
	rd_byte(&tmp8u);
	rd_byte(&tmp8u);

	/* Repair the lore flags */
	r_ptr->r_flags1 &= r_ptr->flags1;
	r_ptr->r_flags2 &= r_ptr->flags2;
	r_ptr->r_flags3 &= r_ptr->flags3;
	r_ptr->r_flags4 &= r_ptr->flags4;
	r_ptr->r_flags5 &= r_ptr->flags5;
	r_ptr->r_flags6 &= r_ptr->flags6;
	r_ptr->r_flags7 &= r_ptr->flags7;
}


/*
 * Read a store
 */
static errr rd_store(int n)
{
	store_type *st_ptr = &store[n];
	object_type *o_ptr;

	/* Read the basic info */
	rd_u32b(&st_ptr->store_open);
	rd_s16b(&st_ptr->insult_cur);
	rd_byte(&st_ptr->owner);
	rd_s16b(&st_ptr->good_buy);
	rd_s16b(&st_ptr->bad_buy);

	/* Read the items */
	while (1)
	{
		o_ptr = rd_item_store();

		if (!o_ptr)
			break;

		/* Location */
		rd_byte(&o_ptr->iy);
		rd_byte(&o_ptr->ix);

		insert_to_global_list(o_ptr, &(st_ptr->stock),
			(n == 7 ? WORLD_HOME : WORLD_STORE));
	}

	/* Success */
	return (0);
}


/*
 * Read RNG state.
 */
static void rd_randomizer(void)
{
	int i;

	u16b tmp16u;

	/* Tmp */
	rd_u16b(&tmp16u);

	/* Place */
	rd_u16b(&Rand_place);

	/* State */
	for (i = 0; i < RAND_DEG; i++)
	{
		rd_u32b(&Rand_state[i]);
	}

	/* Accept */
	Rand_quick = FALSE;
}



/*
 * Read options (ignore most pre-2.8.0 options)
 *
 * Note that the normal options are now stored as a set of 256 bit flags,
 * plus a set of 256 bit masks to indicate which bit flags were defined
 * at the time the savefile was created.  This will allow new options
 * to be added, and old options to be removed, at any time, without
 * hurting old savefiles.
 *
 * The window options are stored in the same way, but note that each
 * window gets 32 options, and their order is fixed by certain defines.
 */
static void rd_options(void)
{
	int i, n;

	byte b;

	u16b c;

	u32b flag[8];
	u32b mask[8];


	/*** Special info */

	/* Read "delay_factor" */
	rd_byte(&b);
	op_ptr->delay_factor = b;

	/* Read "hitpoint_warn" */
	rd_byte(&b);
	op_ptr->hitpoint_warn = b;


	/*** Cheating options ***/

	rd_u16b(&c);

	if (c & 0x0002)
		p_ptr->wizard = TRUE;

	/* Extract the cheating flags */
	for (i = 0; i < CHEAT_MAX; ++i)
	{
		p_ptr->cheat[i] = (c & (0x0100 << i)) ? TRUE : FALSE;
	}

	/*** Normal Options ***/

	/* Read the option flags */
	for (n = 0; n < 8; n++)
		rd_u32b(&flag[n]);

	/* Read the option masks */
	for (n = 0; n < 8; n++)
		rd_u32b(&mask[n]);

	/* Analyze the options */
	for (i = 0; i < OPT_MAX; i++)
	{
		int os = i / 32;
		int ob = i % 32;

		/* Process real entries */
		if (option_text[i])
		{
			/* Process saved entries */
			if (mask[os] & (1L << ob))
			{
				/* Set flag */
				if (flag[os] & (1L << ob))
				{
					/* Set */
					op_ptr->opt[i] = TRUE;
				}

				/* Clear flag */
				else
				{
					/* Set */
					op_ptr->opt[i] = FALSE;
				}
			}
		}
	}


	/*** Window Options ***/

	/* Read the window flags */
	for (n = 0; n < 8; n++)
		rd_u32b(&flag[n]);

	/* Read the window masks */
	for (n = 0; n < 8; n++)
		rd_u32b(&mask[n]);

	/* Analyze the options */
	for (n = 0; n < 8; n++)
	{
		/* Analyze the options */
		for (i = 0; i < 32; i++)
		{
			/* Process valid flags */
			if (window_flag_desc[i])
			{
				/* Process valid flags */
				if (mask[n] & (1L << i))
				{
					/* Set */
					if (flag[n] & (1L << i))
					{
						/* Set */
						op_ptr->window_flag[n] |= (1L << i);
					}
				}
			}
		}
	}
}


/*
 * Read the "extra" information
 */
static errr rd_extra(void)
{
	int i;

	byte tmp8u;
	u16b tmp16u;

	rd_string(op_ptr->full_name, 32);

	rd_string(p_ptr->died_from, 80);

	for (i = 0; i < 4; i++)
	{
		rd_string(p_ptr->history[i], 60);
	}

	/* Class/Race/Gender/Spells */
	rd_byte(&p_ptr->prace);
	rd_byte(&p_ptr->pclass);
	rd_byte(&p_ptr->psex);
	rd_byte(&p_ptr->prace_info);

	/* Special Race/Class info */
	rd_byte(&p_ptr->hitdie);
	rd_byte(&p_ptr->expfact);

	/* Age/Height/Weight */
	rd_s16b(&p_ptr->age);
	rd_s16b(&p_ptr->ht);
	rd_s16b(&p_ptr->wt);

	/* Read the stat info */
	for (i = 0; i < 6; i++)
		rd_s16b(&p_ptr->stat_max[i]);
	for (i = 0; i < 6; i++)
		rd_s16b(&p_ptr->stat_cur[i]);

	rd_s32b(&p_ptr->au);

	rd_s32b(&p_ptr->max_exp);
	rd_s32b(&p_ptr->exp);
	rd_u16b(&p_ptr->exp_frac);

	rd_s16b(&p_ptr->lev);

	/* read arena information and rewards -KMW- */
	rd_byte(&p_ptr->which_arena);
	rd_byte(&p_ptr->which_quest);
	rd_s16b(&p_ptr->which_town);
	rd_s16b(&p_ptr->which_arena_layout);

	for (i = 0; i < MAX_ARENAS; i++)
		rd_s16b(&p_ptr->arena_number[i]);
	for (i = 0; i < MAX_REWARDS; i++)
		rd_byte(&rewards[i]);

	for (i = 0; i < MAX_BOUNTIES; i++)
	{
		rd_s16b(&bounties[i][0]);
		rd_s16b(&bounties[i][1]);
	}

	rd_byte(&p_ptr->exit_bldg);
	rd_s16b(&p_ptr->s_idx);
	rd_s16b(&p_ptr->load_dungeon);

	rd_s16b(&p_ptr->mhp);
	rd_s16b(&p_ptr->chp);
	rd_u16b(&p_ptr->chp_frac);

	rd_s32b(&p_ptr->grace);
	rd_s32b(&p_ptr->god_favor);
	rd_s32b(&p_ptr->luck);
	rd_byte(&p_ptr->pgod);

	p_ptr->pets_notice = 0;
	rd_byte(&p_ptr->number_pets);

	rd_byte(&p_ptr->is_evil);

	rd_s16b(&p_ptr->msp);
	rd_s16b(&p_ptr->csp);
	rd_u16b(&p_ptr->csp_frac);

	rd_s16b(&p_ptr->msane);
	rd_s16b(&p_ptr->csane);
	rd_u16b(&p_ptr->csane_frac);

	rd_s16b(&p_ptr->max_lev);
	rd_s16b(&p_ptr->max_depth);

	/* Hack -- Repair maximum player level */
	if (p_ptr->max_lev < p_ptr->lev)
		p_ptr->max_lev = p_ptr->lev;

	/* Hack -- Repair maximum dungeon level */
	if (p_ptr->max_depth < 0)
		p_ptr->max_depth = 1;

	/* More info */
	rd_s16b(&p_ptr->sc);

	/* Read the flags */
	rd_s16b(&p_ptr->blind);
	rd_s16b(&p_ptr->paralyzed);
	rd_s16b(&p_ptr->confused);
	rd_s16b(&p_ptr->food);
	rd_s16b(&p_ptr->energy);
	rd_s16b(&p_ptr->fast);
	rd_s16b(&p_ptr->slow);
	rd_s16b(&p_ptr->afraid);
	rd_s16b(&p_ptr->cut);
	rd_s16b(&p_ptr->stun);
	rd_s16b(&p_ptr->poisoned);
	rd_s16b(&p_ptr->image);
	rd_s16b(&p_ptr->protevil);
	rd_s16b(&p_ptr->invuln);
	rd_s16b(&p_ptr->hero);
	rd_s16b(&p_ptr->shero);
	rd_s16b(&p_ptr->shield);
	rd_s16b(&p_ptr->blessed);
	rd_s16b(&p_ptr->tim_invis);
	rd_s16b(&p_ptr->word_recall);
	rd_s16b(&p_ptr->see_infra);
	rd_s16b(&p_ptr->tim_infra);
	rd_s16b(&p_ptr->oppose_fire);
	rd_s16b(&p_ptr->oppose_cold);
	rd_s16b(&p_ptr->oppose_acid);
	rd_s16b(&p_ptr->oppose_elec);
	rd_s16b(&p_ptr->oppose_pois);
	rd_s16b(&p_ptr->shape_timed);
	rd_byte(&p_ptr->shape);
	rd_s16b(&p_ptr->immov_cntr);

	rd_u32b(&p_ptr->mutations1);
	rd_u32b(&p_ptr->mutations2);
	rd_u32b(&p_ptr->mutations3);

	rd_byte(&p_ptr->confusing);
	rd_byte(&p_ptr->searching);
	rd_byte(&p_ptr->maximize);
	rd_byte(&p_ptr->preserve);

	/* Future use */
	for (i = 0; i < 48; i++)
		rd_byte(&tmp8u);

	/* Hack -- the two "special seeds" */
	rd_u32b(&seed_flavor);
	rd_u32b(&seed_town);
	rd_u32b(&seed_dungeon);
	rd_u32b(&seed_wild);

	/* Special stuff */
	rd_u16b(&p_ptr->panic_save);
	rd_u16b(&p_ptr->total_winner);
	rd_u16b(&p_ptr->noscore);


	/* Read "death" */
	rd_byte(&p_ptr->is_dead);

	/* Read "feeling" */
	rd_s16b(&feeling);

	/* Turn of last "feeling" */
	rd_s32b(&old_turn);

	/* Current turn */
	rd_s32b(&turn);

	/* Read the player_hp array */
	rd_u16b(&tmp16u);

	/* Incompatible save files */
	if (tmp16u > PY_MAX_LEVEL)
	{
		note(format("Too many (%u) hitpoint entries!", tmp16u));
		return (25);
	}

	/* Read the player_hp array */
	for (i = 0; i < tmp16u; i++)
	{
		rd_s16b(&p_ptr->player_hp[i]);
	}


	return (0);
}


/*
 * Read the player's inventory.
 */

static errr rd_inventory(void)
{
	object_type *o_ptr;
	s16b slot;
	bool foo;

	while (1)
	{
		o_ptr = rd_item();

		/* All done. */
		if (!o_ptr)
			break;

		/* Insert into the stack. */
		foo = inven_carry(o_ptr);

		/* Read equipment slot. */
		rd_s16b(&slot);

		if (foo && slot >= 0)
		{
			equipment[slot] = o_ptr;
		}
	}

	return 0;
}



/*
 * Read the saved messages
 */
static void rd_messages(void)
{
	int i;
	char buf[128];
	byte p;

	s16b num;

	/* Total */
	rd_s16b(&num);

	/* Read the messages */
	for (i = 0; i < num; i++)
	{
		/* Read the message */
		rd_string(buf, 128);
		rd_byte(&p);

		/* Save the message */
		message_add(buf, p);
	}
}

/*
 * Read the dungeon
 *
 * The monsters/objects must be loaded in the same order
 * that they were stored, since the actual indexes matter.
 *
 * Note that the size of the dungeon is now hard-coded to
 * DUNGEON_HGT by DUNGEON_WID, and any dungeon with another
 * size will be silently discarded by this routine.
 */
static errr rd_dungeon(void)
{
	int i, y, x;

	s16b depth;
	s16b py, px;
	s16b ymax, xmax;

	object_type *o_ptr;

	byte count;
	byte tmp8u;

	u16b limit;


	/*** Basic info ***/

	/* Header info */
	rd_s16b(&depth);
	rd_s16b(&p_ptr->inside_special);
	rd_s16b(&p_ptr->wild_y);
	rd_s16b(&p_ptr->wild_x);
	rd_s16b(&p_ptr->wilderness_py);
	rd_s16b(&p_ptr->wilderness_px);
	rd_s16b(&p_ptr->wilderness_depth);
	rd_s16b(&py);
	rd_s16b(&px);

	rd_s16b(&ymax);
	rd_s16b(&xmax);
	
	/* Ignore illegal dungeons */
	if ((depth < 0) || (depth >= MAX_DEPTH))
	{
		note(format("Ignoring illegal dungeon depth (%d)", depth));
		msg_print(NULL);
		return (1);
	}

	/* Ignore illegal dungeons */
	if ((ymax != DUNGEON_HGT) || (xmax != DUNGEON_WID))
	{
		/* XXX XXX XXX */
		note(format("Ignoring illegal dungeon size (%d,%d).", xmax, ymax));
		msg_print(NULL);
		return (1);
	}

	/* Ignore illegal dungeons */
	if ((px < 0) || (px >= DUNGEON_WID) || (py < 0) || (py >= DUNGEON_HGT))
	{
		note(format("Ignoring illegal player location (%d,%d).", px, py));
		msg_print(NULL);
		return (1);
	}



	/*** Run length decoding ***/

	/* Load the dungeon data */
	for (x = y = 0; y < DUNGEON_HGT;)
	{
		/* Grab RLE info */
		rd_byte(&count);
		rd_byte(&tmp8u);

		/* Apply the RLE info */
		for (i = count; i > 0; i--)
		{
			/* Extract "info" */
			cave_info[y][x] = tmp8u;

			/* Advance/Wrap */
			if (++x >= DUNGEON_WID)
			{
				/* Wrap */
				x = 0;

				/* Advance/Wrap */
				if (++y >= DUNGEON_HGT)
					break;
			}
		}
	}


	/*** Run length decoding ***/

	/* Load the dungeon data */
	for (x = y = 0; y < DUNGEON_HGT;)
	{
		/* Grab RLE info */
		rd_byte(&count);
		rd_byte(&tmp8u);

		/* Apply the RLE info */
		for (i = count; i > 0; i--)
		{
			/* Extract "feat" */
			cave_feat[y][x] = tmp8u;

			/* Advance/Wrap */
			if (++x >= DUNGEON_WID)
			{
				/* Wrap */
				x = 0;

				/* Advance/Wrap */
				if (++y >= DUNGEON_HGT)
					break;
			}
		}
	}


	/*** Player ***/

	/* Save depth */
	p_ptr->depth = depth;

	/* Place player in dungeon */
	if (!player_place(py, px))
	{
		note(format("Cannot place player (%d,%d)!", py, px));
		return (162);
	}

	/*** Objects ***/

	/* Read the dungeon items */
	while (1)
	{
		byte ix, iy;
		object_type *o_ptr = rd_item();

		if (!o_ptr)
			break;

		/* Location */
		rd_byte(&iy);
		rd_byte(&ix);

		floor_carry(iy, ix, o_ptr);
	}

	/* Hack: Scatter the items in a store. */
	if (p_ptr->inside_special == SPECIAL_STORE)
	{

		for (o_ptr = store[p_ptr->s_idx].stock; o_ptr != NULL;
			o_ptr = o_ptr->next_global)
		{

			floor_carry(o_ptr->iy, o_ptr->ix, o_ptr);
		}
	}

	/*** Monsters ***/

	/* Read the monster count */
	rd_u16b(&limit);

	/* Hack -- verify */
	if (limit >= MAX_M_IDX)
	{
		note(format("Too many (%d) monster entries!", limit));
		return (161);
	}

	/* Read the monsters */
	for (i = 1; i < limit; i++)
	{
		monster_type *n_ptr;
		monster_type monster_type_body;


		/* Get local monster */
		n_ptr = &monster_type_body;

		/* Clear the monster */
		WIPE(n_ptr, monster_type);

		/* Read the monster */
		rd_monster(n_ptr);

		/* Place monster in dungeon */
		if (!monster_place(n_ptr->fy, n_ptr->fx, n_ptr))
		{
			note(format("Cannot place monster %d", i));
			return (162);
		}
	}

	/*** Success ***/

	/* The dungeon is ready */
	character_dungeon = TRUE;

	/* Success */
	return (0);
}

/*
 * Read a spell.
 */

static void rd_spell(spell * s_ptr)
{
	proj_node *pnode;
	proj_node *pnode_prev = NULL;
	int i;
	byte len;

	rd_string(s_ptr->name, 30);
	rd_string(s_ptr->desc, 30);
	rd_byte(&s_ptr->class);
	rd_byte(&s_ptr->level);
	rd_byte(&s_ptr->mana);
	rd_byte(&s_ptr->untried);
	rd_byte(&s_ptr->unknown);

	/* Number of nodes. */
	rd_byte(&len);

	/* Create the head pf the list. */
	MAKE(s_ptr->proj_list, proj_node);
	pnode = s_ptr->proj_list;

	/* Read in the node data, creating the list. */
	for (i = 0; i < len; i++)
	{
		rd_u32b(&pnode->proj_flags);
		rd_byte(&pnode->safe);
		rd_byte(&pnode->attack_kind);
		rd_byte(&pnode->radius);
		rd_s16b(&pnode->dam_dice);
		rd_s16b(&pnode->dam_sides);

		/* Save the previous node */
		pnode_prev = pnode;

		/* Create a new node */
		MAKE(pnode->next, proj_node);
		pnode = pnode->next;
	}

	/* Hack -- remove the final leftover node. */
	if (pnode_prev != NULL)
	{
		KILL(pnode_prev->next, proj_node);
	}
}

/*
 * Actually read the savefile
 */
static errr rd_savefile_new_aux(void)
{
	int i;

	byte tmp8u;
	u16b tmp16u;
	u32b tmp32u;


	/* Mention the savefile version */
	note(format("Loading a %d.%d.%d savefile...", sf_major, sf_minor,
			sf_patch));


	/* Strip the version bytes */
	strip_bytes(4);

	/* Hack -- decrypt */
	xor_byte = sf_extra;


	/* Clear the checksums */
	v_check = 0L;
	x_check = 0L;


	/* Operating system info */
	rd_u32b(&sf_xtra);

	/* Time of savefile creation */
	rd_u32b(&sf_when);

	/* Number of resurrections */
	rd_u16b(&sf_lives);

	/* Number of times played */
	rd_u16b(&sf_saves);


	/* Later use (always zero) */
	rd_u32b(&tmp32u);

	/* Later use (always zero) */
	rd_u32b(&tmp32u);


	/* Read RNG state */
	rd_randomizer();
	if (arg_fiddle)
		note("Loaded Randomizer Info");


	/* Then the options */
	rd_options();
	if (arg_fiddle)
		note("Loaded Option Flags");


	/* Then the "messages" */
	rd_messages();
	if (arg_fiddle)
		note("Loaded Messages");


	/* Monster Memory */
	rd_u16b(&tmp16u);

	/* Incompatible save files */
	if (tmp16u > MAX_R_IDX)
	{
		note(format("Too many (%u) monster races!", tmp16u));
		return (21);
	}

	/* Read the available records */
	for (i = 0; i < tmp16u; i++)
	{
		monster_race *r_ptr;

		/* Read the lore */
		rd_lore(i);

		/* Access that monster */
		r_ptr = &r_info[i];
	}
	if (arg_fiddle)
		note("Loaded Monster Memory");


	/* Object Memory */
	rd_u16b(&tmp16u);

	/* Incompatible save files */
	if (tmp16u > MAX_K_IDX)
	{
		note(format("Too many (%u) object kinds!", tmp16u));
		return (22);
	}

	/* Read the object memory */
	for (i = 0; i < tmp16u; i++)
	{
		byte tmp8u;

		object_kind *k_ptr = &k_info[i];

		rd_byte(&tmp8u);

		k_ptr->aware = (tmp8u & 0x01) ? TRUE : FALSE;
		k_ptr->tried = (tmp8u & 0x02) ? TRUE : FALSE;
	}
	if (arg_fiddle)
		note("Loaded Object Memory");


	/* Load the Quests */
	rd_u16b(&tmp16u);

	/* Incompatible save files */
	if (tmp16u > MAX_QUESTS)
	{
		note(format("Too many (%u) quests!", tmp16u));
		return (23);
	}

	/* Load the Quest Information */
	for (i = 0; i < tmp16u; i++)
	{
		rd_byte(&tmp8u);
		quest_status[i] = tmp8u;
	}
	if (arg_fiddle)
		note("Loaded Quests");


	/* Load the recipe recall info. */

	rd_u16b(&tmp16u);

	if (tmp16u > MAX_RECIPES)
	{
		note(format("Too many (%u) recipes!", tmp16u));
		return 23;
	}

	for (i = 0; i < tmp16u; i++)
	{
		rd_byte(&tmp8u);
		recipe_recall[i] = tmp8u;
	}

	/* Load the random artifacts. */
	rd_u16b(&tmp16u);
	if (tmp16u > MAX_RANDARTS)
	{
		note(format("Too many (%u) random artifacts!", tmp16u));
		return 23;
	}

	for (i = 0; i < tmp16u; i++)
	{
		random_artifact *ra_ptr = &random_artifacts[i];

		rd_string(ra_ptr->name_full, 80);
		rd_string(ra_ptr->name_short, 80);
		rd_byte(&ra_ptr->level);
		rd_byte(&ra_ptr->attr);
		rd_u32b(&ra_ptr->cost);
		rd_s16b(&ra_ptr->activation);
		rd_byte(&ra_ptr->generated);
	}

	/* Load the random spells. */

	rd_u16b(&tmp16u);
	if (tmp16u > MAX_SPELLS)
	{
		note(format("Too many (%u) random spells!", tmp16u));
		return 23;
	}

	rd_u16b(&spell_num);

	for (i = 0; i < spell_num; i++)
	{
		spell *rspell = &spells[i];

		rd_spell(rspell);
	}

	if (arg_fiddle)
		note("Loaded Random Spells");


	/* Load the Artifacts */
	rd_u16b(&tmp16u);

	/* Incompatible save files */
	if (tmp16u > MAX_A_IDX)
	{
		note(format("Too many (%u) artifacts!", tmp16u));
		return (24);
	}

	/* Read the artifact flags */
	for (i = 0; i < tmp16u; i++)
	{
		rd_byte(&tmp8u);
		a_info[i].cur_num = tmp8u;
		rd_byte(&tmp8u);
		rd_byte(&tmp8u);
		rd_byte(&tmp8u);
	}
	if (arg_fiddle)
		note("Loaded Artifacts");


	/* Read the extra stuff */
	if (rd_extra())
		return (25);
	if (arg_fiddle)
		note("Loaded extra information");


	/* Important -- Initialize the sex */
	sp_ptr = &sex_info[p_ptr->psex];

	/* Important -- Initialize the race/class */
	rp_ptr = &race_info[p_ptr->prace];
	cp_ptr = &class_info[p_ptr->pclass];

	/* Read the inventory */
	if (rd_inventory())
	{
		note("Unable to read inventory");
		return (21);
	}


	/* Read the stores */
	rd_u16b(&tmp16u);
	for (i = 0; i < tmp16u; i++)
	{
		if (rd_store(i))
			return (22);
	}

	/* I'm not dead. (yet)... */
	if (!p_ptr->is_dead)
	{
		/* Dead players have no dungeon */
		note("Restoring Dungeon...");

		if (rd_dungeon())
		{
			note("Error reading dungeon data");
			return (34);
		}

	}

	/* Hack -- no ghosts */
	r_info[MAX_R_IDX - 1].max_num = 0;


	/* Success */
	return (0);
}


/*
 * Actually read the savefile
 */
errr rd_savefile_new(void)
{
	errr err;

	/* The savefile is a binary file */
	fff = my_fopen(savefile, "rb");

	/* Paranoia */
	if (!fff)
		return (-1);

	/* Call the sub-function */
	err = rd_savefile_new_aux();

	/* Check for errors */
	if (ferror(fff))
		err = -1;

	/* Close the file */
	my_fclose(fff);

	/* Result */
	return (err);
}


/*
 * Attempt to load a temporary dungeon.
 */
bool load_dungeon(s16b tag)
{
	char temp[128];
	char path[1024];

	/* Paranoia. */
	if (tag > 999 || tag < 0)
		return TRUE;

	sprintf(temp, "%s.%d", op_ptr->base_name, tag);
	path_build(path, 1024, ANGBAND_DIR_SAVE, temp);

	fff = my_fopen(path, "rb");

	if (!fff)
		return TRUE;

	xor_byte = 0;
	v_check = 0L;
	x_check = 0L;

	/* Read the dungeon. */
	if (rd_dungeon())
		return TRUE;

	/* Check for errors */
	if (ferror(fff))
		return TRUE;

	/* Close the file */
	my_fclose(fff);

	/* Delete the file. */
	fd_kill(path);

	return FALSE;
}
