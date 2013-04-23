#define SAVE_C
/* File: save.c */

/* Purpose: interact with savefiles */

#include "angband.h"
#include "loadsave.h"

/*
 * Some "local" parameters, used to help write savefiles
 */

static FILE     *fff;           /* Current save "file" */

static byte     xor_byte;       /* Simple encryption */

static u32b     v_stamp = 0L;   /* A simple "checksum" on the actual values */
static u32b     x_stamp = 0L;   /* A simple "checksum" on the encoded bytes */



/*
 * These functions place information into a savefile a byte at a time
 */

static void sf_put(byte v)
{
	/* Encode the value, write a character */
	xor_byte ^= v;
	(void)putc((int)xor_byte, fff);

	/* Maintain the checksum info */
	v_stamp += v;
	x_stamp += xor_byte;
}

static void wr_byte(byte v)
{
	sf_put(v);
}

/*
 * Hack -- strip some bytes
 */
static void strip_bytes(int n)
{
	/* Strip the bytes */
	while (n--) wr_byte(0);
}

static void wr_u16b(u16b v)
{
	sf_put((byte)(v & 0xFF));
	sf_put((byte)((v >> 8) & 0xFF));
}

static void wr_s16b(s16b v)
{
	wr_u16b((u16b)v);
}

static void wr_u32b(u32b v)
{
	sf_put((byte)(v & 0xFF)); 
	sf_put((byte)((v >> 8) & 0xFF));
	sf_put((byte)((v >> 16) & 0xFF));
	sf_put((byte)((v >> 24) & 0xFF));
}

static void wr_s32b(s32b v)
{
	wr_u32b((u32b)v);
}

static void wr_string(cptr str)
{
	while (*str)
	{
		wr_byte(*str);
		str++;
	}
	wr_byte(*str);
}


/*
 * These functions write info in larger logical records
 */


/*
 * Write an "item" record
 */
static void wr_item(object_type *o_ptr)
{
	u16b k_idx = convert_k_idx(o_ptr->k_idx, sf_flags_now, sf_flags_sf);
	wr_s16b(k_idx);

	/* Location */
	wr_byte(o_ptr->iy);
	wr_byte(o_ptr->ix);

	wr_byte(o_ptr->tval);
	wr_byte(0); /* Was sval, but sval is unused in all versions. */
	wr_s16b(o_ptr->pval);

	wr_byte(o_ptr->discount);
	wr_byte(o_ptr->number);
	wr_s16b(o_ptr->weight);

	wr_byte(o_ptr->name1);
	wr_byte(o_ptr->name2);
	wr_s16b(o_ptr->timeout);

	wr_s16b(o_ptr->to_h);
	wr_s16b(o_ptr->to_d);
	wr_s16b(o_ptr->to_a);
	wr_s16b(o_ptr->ac);
	wr_byte(o_ptr->dd);
	wr_byte(o_ptr->ds);

	if (!has_flag(SF_16_IDENT))
	{
		byte temp = (o_ptr->ident & 0xFE) | (0x01 * ((o_ptr->ident & IDENT_SENSE) == IDENT_SENSE));
		wr_byte(temp);
	}
	else
	{
		wr_u16b(o_ptr->ident);
	}

	wr_byte(o_ptr->marked);

    wr_u32b(o_ptr->flags1);
    wr_u32b(o_ptr->flags2);
    wr_u32b(o_ptr->flags3);

	/* Held by monster index */
	wr_s16b(o_ptr->held_m_idx);

	/* Extra information */
	if (has_flag(SF_EGO_DISTRO))
	{
		wr_byte(o_ptr->activation);
	}
	else if (o_ptr->activation)
	{
		wr_byte(4); /* EGO_XTRA_ACTIVATE */
		wr_byte(o_ptr->activation);
	}
	else /* Luckily, no other EGO_XTRA_* flags need to be recreated. */
	{
		strip_bytes(2);
	}

	/* Save the inscription (if any) */
	wr_string(quark_str(o_ptr->note));
	
	/* Save the randart name, if any. */
	wr_string(quark_str(o_ptr->art_name));

	/* Set the stack number. */
	if (has_flag(SF_STACK_IDX)) wr_byte(o_ptr->stack);
}


/*
 * Write a "monster" record
 */
static void wr_monster(monster_type *m_ptr)
{
	wr_s16b(convert_r_idx(m_ptr->r_idx, sf_flags_now, sf_flags_sf));
	wr_byte(m_ptr->fy);
	wr_byte(m_ptr->fx);
	wr_byte(m_ptr->generation);
	wr_s16b(m_ptr->hp);
	wr_s16b(m_ptr->maxhp);
	wr_s16b(m_ptr->csleep);
	wr_byte(m_ptr->mspeed);
	wr_s16b(m_ptr->energy);
	wr_byte(m_ptr->stunned);
	wr_byte(m_ptr->confused);
	wr_byte(m_ptr->monfear);
    wr_u32b(m_ptr->smart);
	wr_byte(0);
}


/*
 * Write a "lore" record
 */
static void wr_lore(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Count sights/deaths/kills */
	wr_s16b(r_ptr->r_sights);
	wr_s16b(r_ptr->r_deaths);
	wr_s16b(r_ptr->r_pkills);
	wr_s16b(r_ptr->r_tkills);

	/* Count wakes and ignores */
	wr_byte(r_ptr->r_wake);
	wr_byte(r_ptr->r_ignore);

	/* Extra stuff */
#if 0
	wr_byte(r_ptr->r_xtra1);
	wr_byte(r_ptr->r_xtra2);
#else
	wr_u16b(0);
#endif

	/* Count drops */
	wr_byte(r_ptr->r_drop_gold);
	wr_byte(r_ptr->r_drop_item);

	/* Count spells */
	wr_byte(r_ptr->r_cast_inate);
	wr_byte(r_ptr->r_cast_spell);

	/* Count blows of each type */
	wr_byte(r_ptr->r_blows[0]);
	wr_byte(r_ptr->r_blows[1]);
	wr_byte(r_ptr->r_blows[2]);
	wr_byte(r_ptr->r_blows[3]);

	/* Memorize flags */
	wr_u32b(r_ptr->r_flags1);
	wr_u32b(r_ptr->r_flags2);
	wr_u32b(r_ptr->r_flags3);
	wr_u32b(r_ptr->r_flags4);
	wr_u32b(r_ptr->r_flags5);
	wr_u32b(r_ptr->r_flags6);


	/* Monster limit per level */
	wr_byte(r_ptr->max_num);

	/* Later (?) */
	wr_byte(0);
	wr_byte(0);
	wr_byte(0);
}

/*
 * Write a "death event" record
 */
static void wr_death(void)
{
	uint i;
	u16b UNREAD(tmp16u);
		for (i = 0; i < MAX_DEATH_EVENTS; i++)
		{
			death_event_type *d_ptr = &death_event[i];
			if (!d_ptr->r_idx) break;
			if (i%15 == 0)
			{
				if (i) wr_u16b(tmp16u);
				tmp16u = 0;
			}
			if (d_ptr->flags & EF_KNOWN)
			{
				tmp16u |= 1<<(i%15);
			}
		}
		/* Terminate by setting the 16th bit. */
		tmp16u |= 1<<15;
		wr_u16b(tmp16u);
}

/*
 * Write information about an element of k_info.
 */
static void wr_xtra(s16b k_idx)
{
	byte tmp8u = 0;
	object_kind dummy, *k_ptr = &dummy;

	/* Write from a blank record, if none known. */
	WIPE(k_ptr, dummy);

	/* Find what the save version expects the k_idx to be. */
	k_idx = convert_k_idx(k_idx, sf_flags_now, sf_flags_sf);

	/* Use that k_idx, if any. */
	if (k_idx >= 0) k_ptr = &k_info[k_idx];

	/* Write stuff. */
	if (k_ptr->aware) tmp8u |= 0x01;
	if (k_ptr->tried) tmp8u |= 0x02;
	wr_byte(tmp8u);

	if (has_flag(SF_OBJECT_SEEN)) wr_byte(k_ptr->seen);
}


/*
 * Write a "store" record
 */
static void wr_store(store_type *st_ptr)
{
	int j;
	
	/* Save the type */
	wr_byte(st_ptr->type);

	/* Save the position */
	wr_byte(st_ptr->x);
	wr_byte(st_ptr->y);
	/* Save the "open" counter */
	wr_u32b(st_ptr->store_open);

	/* Save the "insults" */
	wr_s16b(st_ptr->insult_cur);

	/* Save the current owner */
	wr_byte(st_ptr->bought);
	if (has_flag(SF_QUEST_DIRECT))
	{
		wr_s16b(st_ptr->owner);
	}
	else
	{
		s16b owner = convert_owner(st_ptr->owner, sf_flags_now, sf_flags_sf);
		if (owner < 0) quit("Unsupported shopkeeper.");
		wr_byte(owner % MAX_OWNERS);
	}

	/* Save the stock size */
	wr_byte((byte)(st_ptr->stock_num));

	/* Save the "haggle" info */
	wr_s16b(st_ptr->good_buy);
	wr_s16b(st_ptr->bad_buy);

	/* Save the stock */
	for (j = 0; j < st_ptr->stock_num; j++)
	{
		/* Save each item in stock */
		wr_item(&st_ptr->stock[j]);
	}
}


/*
 * Write RNG state
 */
static errr wr_randomizer(void)
{
	int i;

	/* Zero */
	wr_u16b(0);
	
	/* Place */
	wr_u16b(Rand_place);
	
	/* State */
	for (i = 0; i < RAND_DEG; i++)
	{
		wr_u32b(Rand_state[i]);
	}
	
	/* Success */
	return (0);
}


/*
 * Write the "options"
 */
static void wr_options(void)
{
	int i;

	u16b c;
	byte tmp8u;


	/*** Oops ***/

	/* Oops */
	for (i = 0; i < 4; i++) wr_u32b(0L);


	/*** Special Options ***/

	/* Write "delay_factor" */
	wr_byte((byte)(delay_factor));

	/* Write "hitpoint_warn" */
	wr_byte((byte)(hitpoint_warn));


	/*** Cheating options ***/

	c = 0;

	if (cheat_wzrd) c |= 0x0002;

	if (cheat_peek) c |= 0x0100;
	if (cheat_hear) c |= 0x0200;
	if (cheat_room) c |= 0x0400;
	if (cheat_xtra) c |= 0x0800;
	if (cheat_live) c |= 0x2000;
	if (cheat_skll) c |= 0x4000;

	wr_u16b(c);

    /* Autosave info */
    wr_byte(autosave_l);
	tmp8u = (autosave_t) ? 1 : 0;
	if (has_flag(SF_Q_SAVE) && autosave_q) tmp8u |= 2;
    wr_byte(tmp8u);
    wr_s16b(autosave_freq);

	/*** Extract options ***/

	/* Analyze the options */
	for (i = 0; option_info[i].o_desc; i++)
	{
		int os = option_info[i].o_set;
		int ob = option_info[i].o_bit;

		/* Process real entries */
		if (option_info[i].o_var)
		{
			/* Set */
			if (*option_info[i].o_var)
			{
				/* Set */
				option_flag[os] |= (1L << ob);
			}
			
			/* Clear */
			else
			{
				/* Clear */
				option_flag[os] &= ~(1L << ob);
			}
		}
	}


	/*** Normal options ***/

	/* Dump the flags */
	for (i = 0; i < 8; i++) wr_u32b(option_flag[i]);

	/* Dump the masks */
	for (i = 0; i < 8; i++) wr_u32b(option_mask[i]);


	/*** Window options ***/

	/* Dump the flags */
	for (i = 0; i < 8; i++)
	{
		if (has_flag(SF_3D_WINPRI))
		{
			for (c = 0; c < 32; c++)
			{
				byte pri = (windows[i].pri[c])%16;
				pri += 16*((windows[i].rep[c])%16);
				wr_byte(pri);
			}
		}
		else
		{
			/* The current display should be as good as any. */
			wr_u32b(1<<(windows[i].current));
		}
	}

	/* Dump the masks */
	for (i = 0; i < 8; i++) wr_u32b(windows[i].mask);
}


/*
 * Hack -- Write the "ghost" info
 */
static void wr_ghost(void)
{
	int i;

	monster_race *r_ptr = r_info+MON_PLAYER_GHOST;

	/* Name */
	wr_string(r_name + r_ptr->name);

	/* Visuals */
	wr_byte(r_ptr->d_char);
	wr_byte(r_ptr->d_attr);

	/* Level/Rarity */
	wr_byte(r_ptr->level);
	wr_byte(r_ptr->rarity);
	wr_byte(r_ptr->cur_num);
	wr_byte(r_ptr->max_num);

	/* Misc info */
	wr_byte(r_ptr->hdice);
	wr_byte(r_ptr->hside);
	wr_s16b(r_ptr->ac);
	wr_s16b(r_ptr->sleep);
	wr_byte(r_ptr->aaf);
	wr_byte(r_ptr->speed);

	/* Experience */
	wr_s32b(r_ptr->mexp);

	/* Extra */
#if 0
	wr_s16b(r_ptr->extra);
#else
	wr_s16b(0);
#endif

	/* Frequency */
	wr_byte(r_ptr->freq_inate);
	wr_byte(r_ptr->freq_spell);
	wr_byte(r_ptr->num_blows);

	/* Flags */
	wr_u32b(r_ptr->flags1);
	wr_u32b(r_ptr->flags2);
	wr_u32b(r_ptr->flags3);
	wr_u32b(r_ptr->flags4);
	wr_u32b(r_ptr->flags5);
	wr_u32b(r_ptr->flags6);

	/* Attacks */
	for (i = 0; i < 4; i++)
	{
		wr_byte(r_ptr->blow[i].method);
		wr_byte(r_ptr->blow[i].effect);
		wr_byte(r_ptr->blow[i].d_dice);
		wr_byte(r_ptr->blow[i].d_side);
	}
}


/*
 * Write some "extra" info
 */
static void wr_extra(void)
{
	int i,j;

	wr_string(player_name);

	wr_string(format("%.1023s", died_from));

	for (i = 0; i < 4; i++)
	{
		wr_string(history[i]);
	}

	/* Race/Template/Gender/Spells */
	wr_byte(p_ptr->prace);
	wr_byte(p_ptr->ptemplate);
	wr_byte(p_ptr->psex);
	wr_byte(0);     /* oops */

	wr_byte(p_ptr->hitdie);
    wr_u16b(p_ptr->expfact);

	wr_s16b(p_ptr->age);
	wr_s16b(p_ptr->ht);
	wr_s16b(p_ptr->wt);
	wr_s16b(p_ptr->birthday);
	wr_s16b(p_ptr->startdate);

	/* Dump the stats (maximum and current) */
	for (i = 0; i < 6; ++i) wr_s16b(p_ptr->stat_max[i]);
	for (i = 0; i < 6; ++i) wr_s16b(p_ptr->stat_cur[i]);

	/* Ignore the transient stats */
	for (i = 0; i < 12; ++i) wr_s16b(0);

	wr_u32b(p_ptr->au);

	wr_u32b(p_ptr->exp);
	wr_u16b(p_ptr->exp_frac);


	if (has_flag(SF_SAVE_MAX_SKILLS))
	{
		j = MAX_SKILLS;
		wr_byte(j);
	}
	else
	/* Without SAVE_MAX_SKILLS, there are assumed to be 27 skills. */
	{
		j = 27;
	}
	
	for (i=0; i<j; i++)
	{
		wr_byte(skill_set[i].value);
		wr_byte(skill_set[i].max_value);
		if (has_flag(SF_SKILL_BASE))
		{
			wr_byte(skill_set[i].base);
			wr_byte(skill_set[i].ceiling);
		}
		wr_u16b(skill_set[i].exp_to_raise);
		wr_u16b(skill_set[i].experience);
	}

	wr_s16b(p_ptr->mhp);
	wr_s16b(p_ptr->chp);
	wr_u16b(p_ptr->chp_frac);

	wr_s16b(p_ptr->msp);
	wr_s16b(p_ptr->csp);
	wr_u16b(p_ptr->csp_frac);

	wr_s16b(p_ptr->mchi);
	wr_s16b(p_ptr->cchi);
	wr_u16b(p_ptr->chi_frac);

	/* Max Player and Dungeon Levels */
	if (has_flag(SF_QUEST_DIRECT))
	{
		wr_s16b(p_ptr->max_dlv);
	}
	else
	{
		for (i = 0; i < 20; i++)
		{
			if (cur_dungeon == i) wr_s16b(p_ptr->max_dlv);
			else wr_s16b(0);
		}
	}

	/* More info */
	wr_s16b(0);     /* oops */
	wr_s16b(0);     /* oops */
	wr_s16b(0);     /* oops */
	wr_s16b(0);     /* oops */
	wr_s16b(p_ptr->sc);
	wr_s16b(0);     /* oops */

	wr_s16b(0);             /* old "rest" */
	wr_s16b(p_ptr->blind);
	wr_s16b(p_ptr->paralyzed);
	wr_s16b(p_ptr->confused);
	wr_s16b(p_ptr->food);
	wr_s16b(0);     /* old "food_digested" */
	wr_s16b(0);     /* old "protection" */
	wr_s16b(p_ptr->energy);
	wr_s16b(p_ptr->fast);
	wr_s16b(p_ptr->slow);
	wr_s16b(p_ptr->afraid);
	wr_s16b(p_ptr->cut);
	wr_s16b(p_ptr->stun);
	wr_s16b(p_ptr->poisoned);
	wr_s16b(p_ptr->image);
	wr_s16b(p_ptr->protevil);
	wr_s16b(p_ptr->invuln);
	wr_s16b(p_ptr->hero);
	wr_s16b(p_ptr->shero);
	wr_s16b(p_ptr->shield);
	wr_s16b(p_ptr->blessed);
	wr_s16b(p_ptr->tim_invis);
	wr_s16b(p_ptr->word_recall);
	wr_s16b(p_ptr->see_infra);
	wr_s16b(p_ptr->tim_infra);
	wr_s16b(p_ptr->oppose_fire);
	wr_s16b(p_ptr->oppose_cold);
	wr_s16b(p_ptr->oppose_acid);
	wr_s16b(p_ptr->oppose_elec);
	wr_s16b(p_ptr->oppose_pois);
    wr_s16b(p_ptr->tim_esp);
    wr_s16b(p_ptr->wraith_form);
	if (has_flag(SF_STORE_VAMP)) wr_s16b(p_ptr->vamp_drain);
	strip_bytes(18);

    wr_s16b(p_ptr->chaos_patron);
    wr_u32b(p_ptr->muta1);
    wr_u32b(p_ptr->muta2);
    wr_u32b(p_ptr->muta3);

	wr_byte(p_ptr->confusing);

	/* Store p_ptr->ritual temporarily as it may need to be changed. */
	j = p_ptr->ritual;

	/* This is duplicated information, but it was used. */
	if (!has_flag(SF_QUEST_DIRECT))
	{
		for (i = 0; i < 8; i++)
		{
			store_type *st_ptr = find_house(i);
			if (st_ptr) wr_byte(st_ptr->bought);
			else wr_byte(0);
		}
		if (p_ptr->ritual == TOWN_NONE) j = 9;
	}
	wr_byte(j); /* p_ptr->ritual */
	wr_byte(p_ptr->sneaking);
	wr_byte(0);

	/* Future use */
	for (i = 0; i < 12; i++) wr_u32b(0L);

	/* Ignore some flags */
	wr_u32b(0L);    /* oops */
	wr_u32b(0L);    /* oops */
	wr_u32b(0L);    /* oops */


	/* Write the "object seeds" */
	wr_u32b(seed_flavor);
	wr_u32b(seed_wild);
	for(i=0;i<12;i++)
	{
		for(j=0;j<12;j++)
		{
			wr_u32b(wild_grid[i][j].seed);
			wr_byte(wild_grid[i][j].dungeon);
			wr_byte(wild_grid[i][j].road_map);
		}
	}


	/* Special stuff */
	wr_u16b(panic_save);
	wr_u16b(total_winner);
	wr_u16b(noscore);


	/* Write death */
	wr_byte(death);

	/* Write feeling */
	wr_byte((byte)(feeling));

	/* Turn of last "feeling" */
	wr_s32b(old_turn);

	/* Current turn */
	wr_s32b(turn);

	if (has_flag(SF_CURSE))
		wr_s32b(curse_turn);
}



/*
 * Write the current dungeon
 */
static void wr_dungeon(void)
{
	int i, y, x;

	byte tmp8u;
	u16b tmp16u;

	byte count;
	u16b prev_char;

	cave_type *c_ptr;


	/*** Basic info ***/

	/* Dungeon specific info follows */
	wr_u16b(dun_level);
	wr_u16b(dun_offset);
	wr_u16b(dun_bias);
	wr_byte(cur_town);
	wr_byte(cur_dungeon);
	wr_byte(recall_dungeon);
	wr_byte(came_from);
	wr_u16b(num_repro);
	wr_u16b(py);
	wr_u16b(px);
	wr_u16b(wildx);
	wr_u16b(wildy);
	wr_u16b(cur_hgt);
	wr_u16b(cur_wid);
	wr_u16b(max_panel_rows);
	wr_u16b(max_panel_cols);


	/*** Simple "Run-Length-Encoding" of cave ***/

	/* Note that this will induce two wasted bytes */
	count = 0;
	prev_char = 0;

	/* Dump the cave */
	for (y = 0; y < cur_hgt; y++)
	{
		for (x = 0; x < cur_wid; x++)
		{
			/* Get the cave */
			c_ptr = &cave[y][x];

			/* Extract the cave flags */
			tmp16u = c_ptr->info;
			if (!has_flag(SF_16_CAVE_FLAG)) tmp16u &= 0x00FF;
			
			/* If the run is broken, or too full, flush it */
			if ((tmp16u != prev_char) || (count == MAX_UCHAR))
			{
				wr_byte((byte)count);
				if (has_flag(SF_16_CAVE_FLAG))
					wr_u16b(prev_char);
				else
					wr_byte((byte)prev_char);

				prev_char = tmp16u;
				count = 1;
			}

			/* Continue the run */
			else
			{
				count++;
			}
		}
	}

	/* Flush the data (if any) */
	if (count)
	{
		wr_byte((byte)count);
		if (has_flag(SF_16_CAVE_FLAG))
			wr_u16b(prev_char);
		else
			wr_byte((byte)prev_char);
	}


	/*** Simple "Run-Length-Encoding" of cave ***/

	/* Note that this will induce two wasted bytes */
	count = 0;
	prev_char = 0;

	/* Dump the cave */
	for (y = 0; y < cur_hgt; y++)
	{
		for (x = 0; x < cur_wid; x++)
		{
			/* Get the cave */
			c_ptr = &cave[y][x];

			/* Extract a byte */
			tmp8u = c_ptr->feat;
			
			/* If the run is broken, or too full, flush it */
			if ((tmp8u != prev_char) || (count == MAX_UCHAR))
			{
				wr_byte((byte)count);
				wr_byte((byte)prev_char);
				prev_char = tmp8u;
				count = 1;
			}

			/* Continue the run */
			else
			{
				count++;
			}
		}
	}

	/* Flush the data (if any) */
	if (count)
	{
		wr_byte((byte)count);
		wr_byte((byte)prev_char);
	}




	/* Compact the objects */
	compact_objects(0);
	/* Compact the monsters */
	compact_monsters(0);

	/*** Dump objects ***/

	/* Total objects */
	wr_u16b(o_max);

	/* Dump the objects */
	for (i = 1; i < o_max; i++)
	{
		object_type *o_ptr = &o_list[i];

		/* Dump it */
		wr_item(o_ptr);
	}


	/*** Dump the monsters ***/


	/* Total monsters */
	wr_u16b(m_max);

	/* Dump the monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];
		
		/* Dump it */
		wr_monster(m_ptr);
	}
}



static void wr_spell_flags(void)
{
	int i, j;
	/* Read spell info */
	for (i=0;i<MAX_SCHOOL;i++)
	{
		u32b learned = 0, worked = 0, forgot = 0;

		for (j = 0; j < MAX_SPELLS_PER_BOOK; j++)
		{
			u32b f = 1L << j;
			const magic_type *s_ptr = num_to_spell(i*MAX_SPELLS_PER_BOOK+j);

			/* Not a real spell. */
			if (!s_ptr) continue;

			if (s_ptr->flags & MAGIC_LEARNED) learned |= f;
			if (s_ptr->flags & MAGIC_WORKED) worked |= f;
			if (s_ptr->flags & MAGIC_FORGOT) forgot |= f;
		}

		wr_u32b(learned);
		wr_u32b(worked);
		wr_u32b(forgot);
	}

	for (i = 0; i < 128; i++)
	{
		wr_byte(spell_order[i]);
	}
}

/*
 * Actually write a save-file
 */
static bool wr_savefile_new(void)
{
	int        i;

	u32b              now;

	byte            tmp8u;
	u16b            tmp16u;


	/* Guess at the current time */
	now = time((time_t *)0);


	/* Note the operating system */
	sf_xtra = 0L;

	/* Note when the file was saved */
	sf_when = now;

	/* Note the number of saves */
	sf_saves++;


	/*** Actually write the file ***/

    /* Dump the file header */
	xor_byte = 0;
	    wr_byte(sf_major);
	xor_byte = 0;
	    wr_byte(sf_minor);
	xor_byte = 0;
	wr_byte(sf_patch);
	xor_byte = 0;

	tmp8u = (byte)(rand_int(256));
	wr_byte(tmp8u);


	/* Reset the checksum */
	v_stamp = 0L;
	x_stamp = 0L;

	/* Additional u16b savefile flags. */
	if (sf_flags_sf[0] & 1<<SF_CONTINUE)
	{
		for (i = 1; i < MAX_SF_VAR; i++)
		{
			wr_u16b(sf_flags_sf[i]);
			if (~sf_flags_sf[i] & SF_CONTINUE) break;
		}
	}

	/* Operating system */
	wr_u32b(sf_xtra);


	/* Time file last saved */
	wr_u32b(sf_when);

	/* Number of past lives */
	wr_u16b(sf_lives);

	/* Number of times saved */
	wr_u16b(sf_saves);


	/* Space */
	wr_u32b(0L);
	wr_u32b(0L);


	/* Write the RNG state */
	wr_randomizer();


	/* Write the boolean "options" */
	wr_options();


	/* Dump the number of "messages" */
	tmp16u = message_num();
	if (compress_savefile && (tmp16u > 40)) tmp16u = 40;
	wr_u16b(tmp16u);

	/* Dump the messages (oldest first!) */
	for (i = tmp16u - 1; i >= 0; i--)
	{
		wr_string(message_str((short)i));
	}


	/* Dump the monster lore */
	tmp16u = convert_r_idx(MAX_R_IDX, sf_flags_now, sf_flags_sf);
	wr_u16b(tmp16u);
	for (i = 0; i < tmp16u; i++) wr_lore(MAX(0, convert_r_idx(i, sf_flags_sf, sf_flags_now)));

	/* Dump the death event lore */
	if (has_flag(SF_DEATHEVENTTEXT)) wr_death();

	/* Dump the object memory */
	tmp16u = convert_k_idx(MAX_K_IDX, sf_flags_now, sf_flags_sf);
	wr_u16b(tmp16u);
	for (i = 0; i < tmp16u; i++) wr_xtra(i);


	/* Hack -- Dump the quests */
	tmp16u = MAX_Q_IDX;
	wr_u16b(tmp16u);
	for (i = 0; i < tmp16u; i++)
	{
		quest_type *q_ptr = q_list+i;
		wr_byte(q_ptr->level);
		wr_s16b(convert_r_idx(q_ptr->r_idx, sf_flags_now, sf_flags_sf));
		wr_byte(q_ptr->dungeon);
		wr_byte(q_ptr->cur_num);
		wr_byte(q_ptr->max_num);
		if (has_flag(SF_QUEST_UNKNOWN)) wr_byte(q_ptr->cur_num_known);
		if (has_flag(SF_QUEST_KNOWN)) wr_byte(q_ptr->known);
	}

	/* Hack -- Dump the artifacts */
	tmp16u = MAX_A_IDX;
	wr_u16b(tmp16u);
	for (i = 0; i < tmp16u; i++)
	{
		artifact_type *a_ptr = &a_info[i];
		wr_byte(a_ptr->cur_num);
		wr_byte(0);
		wr_byte(0);
		wr_byte(0);
	}



	/* Write the "extra" information */
	wr_extra();


	/* Dump the "player hp" entries */
	tmp16u = 100;
	wr_u16b(tmp16u);
	for (i = 0; i < tmp16u; i++)
	{
		wr_s16b(player_hp[i]);
	}

	wr_spell_flags();

		/* Dump spirit info */
	for (i=0;i<MAX_SPIRITS;i++)
	{
		wr_u16b(spirits[i].pact);
		wr_u32b(spirits[i].annoyance);
		wr_string(spirits[i].name);
	}


	/* Write the inventory */
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		object_type *o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Dump index */
		wr_u16b((short)i);

		/* Dump object */
		wr_item(o_ptr);
	}

	/* Add a sentinel */
	wr_u16b(0xFFFF);


	/* Note the stores */
	tmp16u = MAX_STORES_TOTAL;

	/* Old savefiles can't accept more than 96 shops. */
	if (!has_flag(SF_QUEST_DIRECT) && tmp16u > 96) tmp16u = 96;

	wr_u16b(tmp16u);

	/* Dump the stores */
	for (i = 0; i < tmp16u; i++) wr_store(&store[i]);


	/* Player is not dead, write the dungeon */
	if (!death)
	{
		/* Dump the dungeon */
		wr_dungeon();

		/* Dump the ghost */
		wr_ghost();
	}


	/* Write the "value check-sum" */
	wr_u32b(v_stamp);

	/* Write the "encoded checksum" */
	wr_u32b(x_stamp);


	/* Error in save */
	if (ferror(fff) || (fflush(fff) == EOF)) return FALSE;

	/* Successful save */
	return TRUE;
}


/*
 * Medium level player saver
 *
 * XXX XXX XXX Angband 2.8.0 will use "fd" instead of "fff" if possible
 */
static bool save_player_aux(char *name)
{
	bool    ok = FALSE;

	int             fd = -1;

	int             mode = 0644;


	/* No file yet */
	fff = NULL;


	/* File type is "SAVE" */
	FILE_TYPE(FILE_TYPE_SAVE);


	/* Create the savefile */
	fd = fd_make(name, mode);

	/* File is okay */
	if (fd >= 0)
	{
		/* Close the "fd" */
		(void)fd_close(fd);

		/* Open the savefile */
		fff = my_fopen(name, "wb");

		/* Successful open */
		if (fff)
		{
			/* Write the savefile */
			if (wr_savefile_new()) ok = TRUE;

			/* Attempt to close it */
			if (my_fclose(fff)) ok = FALSE;
		}

		/* Remove "broken" files */
		if (!ok) (void)fd_kill(name);
	}


	/* Failure */
	if (!ok) return (FALSE);

	/* Successful save */
	character_saved = TRUE;

	/* Success */
	return (TRUE);
}

/*
 * Attempt to save the player in a savefile
 *
 * This routine is only capable of creating a save file for the current version
 * and for 4.1.0, but the rest of the file should cope with any desired save
 * file.
 */
bool save_player(bool as_4_1_0)
{

	int             result = FALSE;

	char    safe[1024];

	/* If a 4.1.0 savefile is required, provide one. */
	if (as_4_1_0)
	{
		WIPE(sf_flags_sf, sf_flags_sf);
		sf_flags_sf[0] = SF_SKILL_BASE;
		sf_major = 4;
		sf_minor = 1;
		sf_patch = 0;
	}
	/* If not, simply find the current version. */
	else
	{
		current_version(sf_flags_sf, &sf_major, &sf_minor, &sf_patch);
	}

	

#if defined(SET_UID) && defined(SECURE)

	/* Get "games" permissions */
	beGames();

#endif


	/* New savefile */
	strcpy(safe, savefile);
	strcat(safe, ".new");

#ifdef VM
	/* Hack -- support "flat directory" usage on VM/ESA */
	strcpy(safe, savefile);
	strcat(safe, "n");
#endif /* VM */

	/* Remove it */
	fd_kill(safe);

	/* Attempt to save the player */
	if (save_player_aux(safe))
	{
		char temp[1024];

		/* Old savefile */
		strcpy(temp, savefile);
		strcat(temp, ".old");

#ifdef VM
		/* Hack -- support "flat directory" usage on VM/ESA */
		strcpy(temp, savefile);
		strcat(temp, "o");
#endif /* VM */

		/* Remove it */
		fd_kill(temp);

		/* Preserve old savefile */
		fd_move(savefile, temp);

		/* Activate new savefile */
		fd_move(safe, savefile);

		/* Remove preserved savefile */
		fd_kill(temp);

		/* Hack -- Pretend the character was loaded */
		character_loaded = TRUE;

#ifdef VERIFY_SAVEFILE

		/* Lock on savefile */
		strcpy(temp, savefile);
		strcat(temp, ".lok");

		/* Remove lock file */
		fd_kill(temp);

#endif

		/* Success */
		result = TRUE;
	}


#ifdef SET_UID

# ifdef SECURE

	/* Drop "games" permissions */
	bePlayer();

# endif

#endif


	/* Return the result */
	return (result);
}

