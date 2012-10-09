/* File: save.c */

/* Purpose: save the current game into a savefile */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

#ifdef __DJGPP__
#include "unistd.h"
#endif

/*
 * These functions place information into a savefile a byte at a time
 */

static void sf_put(FILE *fff, byte v)
{
   (void)putc((int)v, fff);
}

static void wr_byte(FILE *fff, byte v)
{
   sf_put(fff, v);
}

static void wr_u16b(FILE *fff, u16b v)
{
   sf_put(fff, (byte)(v & 0xFF));
   sf_put(fff, (byte)((v >> 8) & 0xFF));
}

static void wr_s16b(FILE *fff, s16b v)
{
   wr_u16b(fff, (u16b)v);
}

static void wr_u32b(FILE *fff, u32b v)
{
   sf_put(fff, (byte)(v & 0xFF));
   sf_put(fff, (byte)((v >> 8) & 0xFF));
   sf_put(fff, (byte)((v >> 16) & 0xFF));
   sf_put(fff, (byte)((v >> 24) & 0xFF));
}

static void wr_u64b(FILE *fff, u64b v)
{
   sf_put(fff, (byte)(v & 0xFF));
   sf_put(fff, (byte)((v >> 8) & 0xFF));
   sf_put(fff, (byte)((v >> 16) & 0xFF));
   sf_put(fff, (byte)((v >> 24) & 0xFF));
   sf_put(fff, (byte)((v >> 32) & 0xFF));
   sf_put(fff, (byte)((v >> 40) & 0xFF));
   sf_put(fff, (byte)((v >> 48) & 0xFF));
   sf_put(fff, (byte)((v >> 56) & 0xFF));
}

static void wr_s32b(FILE *fff, s32b v)
{
   wr_u32b(fff, (u32b)v);
}

/* jk */
static void wr_bool(FILE *fff, bool b)
{
   wr_byte(fff, (byte)(b?1:0));
}

static void wr_string(FILE *fff, cptr str)
{
   while ((int)(*str)!=0)
   {
      wr_byte(fff, (byte)(*str));
      str++;
   }
   wr_byte(fff, (byte)(*str));
}

/*
 * These functions write info in larger logical records
 */

void wr_spell_set(FILE *fff, object_type *i_ptr)
{
   s16b i;
   for (i=0;i<(MAX_SPELLS_PER_ITEM/16);i++)
   {
dlog(DEBUGSAVE,"save.c: wr_spell_set: spells[%d] = %016x\n",
               i, s_list[i_ptr->spell_set].spells[i]);
      wr_u16b(fff, s_list[i_ptr->spell_set].spells[i]);
   }
}

/* 
 * Write an "item log" record
 */
static void wr_item_log(FILE *fff, item_log_type *l_ptr)
{
   wr_u32b(fff, l_ptr->where);
   wr_u32b(fff, l_ptr->found_when);
   wr_u16b(fff, l_ptr->whose);
   wr_s16b(fff, l_ptr->mlevel);
   wr_s16b(fff, l_ptr->slevel);
dlog(DEBUGSAVE2,"save.c: wr_item_log: where %08lx when %ld whose %d mlevel %d slevel %d\n",
                l_ptr->where, l_ptr->found_when, l_ptr->whose, l_ptr->mlevel, l_ptr->slevel); 
}

/*
 * Write an "item" record
 */
static void wr_item(FILE *fff, object_type *i_ptr)
{
dlog(DEBUGSAVE2,"save.c: wr_item: k_idx %d @ %d,%d,%d tv %d sv %d p1v %ld p2v %d disc %d num %d wgt %d\n",
     i_ptr->k_idx,i_ptr->ix,i_ptr->iy,i_ptr->iz,i_ptr->tval,i_ptr->sval,i_ptr->p1val,i_ptr->p2val,
     i_ptr->discount,i_ptr->number,i_ptr->weight);
dlog(DEBUGSAVE2,"save.c: wr_item: n1 %d n2 %d tim %d toh %d tod %d toa %d ac %d dd %d ds %d\n",
     i_ptr->name1,i_ptr->name2,i_ptr->timeout,i_ptr->to_h,i_ptr->to_d,i_ptr->to_a,
     i_ptr->ac,i_ptr->dd,i_ptr->ds);
dlog(DEBUGSAVE2,"save.c: wr_item: ident %d mark %d xtra1 %d xtra2 %ld\n",
     i_ptr->ident, i_ptr->marked, i_ptr->xtra1, i_ptr->xtra2);

   wr_s16b(fff, i_ptr->k_idx);
/* jk - these were bytes */
   wr_s16b(fff, i_ptr->ix);
   wr_s16b(fff, i_ptr->iy);
   wr_s16b(fff, i_ptr->iz);

   wr_byte(fff, i_ptr->tval);
   wr_s16b(fff, i_ptr->sval);
   wr_s32b(fff, i_ptr->p1val);
   wr_s16b(fff, i_ptr->p2val);

   wr_byte(fff, i_ptr->discount);
   wr_byte(fff, i_ptr->number);
   wr_s16b(fff, i_ptr->weight);

   wr_byte(fff, i_ptr->name1);
   wr_byte(fff, i_ptr->name2);
   wr_s16b(fff, i_ptr->timeout);

   wr_s16b(fff, i_ptr->to_h);
   wr_s16b(fff, i_ptr->to_d);
   wr_s16b(fff, i_ptr->to_a);
   wr_s16b(fff, i_ptr->ac);
   wr_byte(fff, i_ptr->dd);
   wr_byte(fff, i_ptr->ds);

   wr_u16b(fff, i_ptr->ident);
   wr_byte(fff, i_ptr->marked);

   wr_byte(fff, i_ptr->xtra1);
/* jk - was byte */
   wr_s32b(fff, i_ptr->xtra2);
   if (i_ptr->spell_set)
   {
dlog(DEBUGITEMS,"save.c: wr_item: saving spell_set %d\n", i_ptr->spell_set);
   }
   wr_s16b(fff, i_ptr->spell_set);
   if (i_ptr->spell_set) wr_spell_set(fff, i_ptr);
   wr_item_log(fff, &i_ptr->log);

   /* Save the inscription (if any) */
   if (i_ptr->note)
   {
      wr_string(fff, quark_str(i_ptr->note));
   }
   else
   {
      wr_string(fff, "");
   }
}

/*
 * Write a "monster" record
 */
static void wr_monster(FILE *fff, monster_type *m_ptr)
{
dlog(DEBUGSAVE2,"save.c: wr_monster r_idx %d @ %d,%d,%d hp %d maxhp %d cslp %d mspd %d\n",
     m_ptr->r_idx, m_ptr->fx, m_ptr->fy, m_ptr->fz, m_ptr->hp, m_ptr->maxhp, m_ptr->csleep,
     m_ptr->mspeed);
dlog(DEBUGSAVE2,"save.c: wr_monster energy %d confused %d afraid %d stun %d\n",
     m_ptr->energy, m_ptr->confused, m_ptr->afraid, m_ptr->stun);
   wr_s16b(fff, m_ptr->r_idx);
/* jk - was byte fy, fx */
   wr_s16b(fff, m_ptr->fx);
   wr_s16b(fff, m_ptr->fy);
   wr_s16b(fff, m_ptr->fz);
   wr_s16b(fff, m_ptr->hp);
   wr_s16b(fff, m_ptr->maxhp);
   wr_s16b(fff, m_ptr->csleep);
   wr_byte(fff, m_ptr->mspeed);
   wr_byte(fff, m_ptr->energy);

   wr_s16b(fff, m_ptr->confused);              /* Timed -- Confusion           */
   wr_s16b(fff, m_ptr->afraid);                /* Timed -- Fear                */
/* jk */
   wr_s16b(fff, m_ptr->escaping);              /* Timed -- Escaping            */
   wr_byte(fff, m_ptr->oldspeed);              /* old speed before escaping    */
   wr_s16b(fff, m_ptr->stun);                  /* Timed -- Stun                */
   wr_u16b(fff, m_ptr->attacked);
   wr_byte(fff, m_ptr->breed_counter);
   wr_byte(fff, m_ptr->last_hit);
}

/*
 * Write a "lore" record
 */
static void wr_lore(FILE *fff, s16b r_idx)
{
   monster_race *r_ptr = &r_info[r_idx];
dlog(DEBUGSAVE2,"save.c: wr_lore sig %d dt %d pk %d tk %d wak %d ign %d xt1 %d xt2 %d drg %d dri %d\n",
     r_ptr->r_sights,r_ptr->r_deaths,r_ptr->r_pkills,r_ptr->r_tkills,r_ptr->r_wake,
     r_ptr->r_ignore,r_ptr->r_xtra1,r_ptr->r_xtra2,r_ptr->r_drop_gold,
     r_ptr->r_drop_item);
dlog(DEBUGSAVE2,"save.c: wr_lore ina %d spl %d bl1 %d bl2 %d bl3 %d bl4 %d max %d\n",
     r_ptr->r_cast_inate,r_ptr->r_cast_spell,r_ptr->r_blows[0],r_ptr->r_blows[1],
     r_ptr->r_blows[2],r_ptr->r_blows[3],r_ptr->max_num);
dlog(DEBUGSAVE2,"save.c: wr_lore f1 %16Lx  f2 %16Lx f3 %16Lx f4 %16Lx f5 %16Lx f6 %16Lx\n",
     r_ptr->r_flags1,r_ptr->r_flags2,r_ptr->r_flags3,r_ptr->r_flags4,
     r_ptr->r_flags5,r_ptr->r_flags6);

   /* Count sights/deaths/kills */
   wr_s16b(fff, r_ptr->r_sights);
   wr_s16b(fff, r_ptr->r_deaths);
   wr_s16b(fff, r_ptr->r_pkills);
   wr_s16b(fff, r_ptr->r_tkills);
   wr_u32b(fff, r_ptr->first_kill);

   /* Count wakes and ignores */
   wr_byte(fff, r_ptr->r_wake);
   wr_byte(fff, r_ptr->r_ignore);

   /* Extra stuff */
   wr_byte(fff, r_ptr->r_xtra1);
   wr_byte(fff, r_ptr->r_xtra2);

   /* Count drops */
   wr_byte(fff, r_ptr->r_drop_gold);
   wr_byte(fff, r_ptr->r_drop_item);

   /* Count spells */
   wr_byte(fff, r_ptr->r_cast_inate);
   wr_byte(fff, r_ptr->r_cast_spell);

   /* Count blows of each type */
   wr_byte(fff, r_ptr->r_blows[0]);
   wr_byte(fff, r_ptr->r_blows[1]);
   wr_byte(fff, r_ptr->r_blows[2]);
   wr_byte(fff, r_ptr->r_blows[3]);

   /* Memorize flags */
   wr_u64b(fff, r_ptr->r_flags1);
   wr_u64b(fff, r_ptr->r_flags2);
   wr_u64b(fff, r_ptr->r_flags3);
   wr_u64b(fff, r_ptr->r_flags4);
   wr_u64b(fff, r_ptr->r_flags5);
   wr_u64b(fff, r_ptr->r_flags6);

   /* Monster limit per level */
   wr_byte(fff, r_ptr->max_num);
}

static void wr_flavors(FILE *fff)
{
   s16b i;
   for (i=0; i<MAX_FLAVORS; i++)
   {
      wr_string(fff, flavor[i].name);
      wr_byte(fff, flavor[i].color);
   }
}

/*
 * Write a "trap item" record
 */
static void wr_trap_item(FILE *fff, trap_item_type *tr_ptr)
{
   s16b i;

   wr_bool(fff, tr_ptr->inuse); /* should always be TRUE, since we just compacted */
   for (i=0; i < MAX_TRAPS_IN_SET; i++)
   {
      wr_byte(fff, tr_ptr->type[i]);
      wr_bool(fff, tr_ptr->found[i]);
   }
   wr_s16b(fff, tr_ptr->tx);
   wr_s16b(fff, tr_ptr->ty);
   wr_s16b(fff, tr_ptr->tz);
}

/*
 * Write an "xtra" record
 */
static void wr_xtra(FILE *fff, s16b k_idx)
{
   byte tmp8u = 0;

   object_kind *k_ptr = &k_info[k_idx];

   if (k_ptr->aware) tmp8u |= 0x01;
   if (k_ptr->tried) tmp8u |= 0x02;

dlog((DEBUGSAVE2|DEBUGITEMS),"save.c: wr_xtra: k_idx %d aware %d tried %d\n", k_idx, k_ptr->aware, k_ptr->tried);
   wr_byte(fff, tmp8u);
}

/*
 * Write a "store" record
 */
static void wr_store(FILE *fff, store_type *st_ptr)
{
   s16b j;

dlog(DEBUGSAVE,"save.c: wr_store: open %ld ins %d own %d num %d good %d bad %d\n",
     st_ptr->store_open,st_ptr->insult_cur,st_ptr->owner,st_ptr->stock_num,
     st_ptr->good_buy,st_ptr->bad_buy);
   /* Save the "open" counter */
   wr_u32b(fff, st_ptr->store_open);

   /* Save the "insults" */
   wr_s16b(fff, st_ptr->insult_cur);

   /* Save the current owner */
   wr_byte(fff, st_ptr->owner);

   /* Save the type of the store */
   wr_byte(fff, st_ptr->store_type);
   wr_s16b(fff, st_ptr->sx);
   wr_s16b(fff, st_ptr->sy);

   /* Save the stock size */
   wr_byte(fff, st_ptr->stock_num);

   /* Save the "haggle" info */
   wr_s16b(fff, st_ptr->good_buy);
   wr_s16b(fff, st_ptr->bad_buy);

dlog(DEBUGSAVE,"save.c: wr_store writing %d items\n", st_ptr->stock_num);

   /* Save the stock */
   for (j = 0; j < st_ptr->stock_num; j++)
   {
dlog(DEBUGSAVE2,"save.c: wr_store writing item %d of %d\n", j, st_ptr->stock_num);
      /* Save each item in stock */
      wr_item(fff, &st_ptr->stock[j]);
      wr_item_log(fff, &st_ptr->stock[j].log);
   }
}

/*
 * Write the "options"
 */
static void wr_options(FILE *fff)
{
   s16b i,k, os, ob;

   u32b c;
   u32b flag[8];
   u32b mask[8];

   u64b opt[5];

   /*** Normal options ***/

   /* Clear the option flag sets */
   for (i = 0; i < 5; i++) opt[i] = 0LL;

   /* we assign the options in options[] an option-set (os) and */
   /* an option-bit (ob) on the file, starting at set 0 bit 0.  */
   os = 0;
   ob = -1;

   /* Analyze the options */
   /* Analyze the options, but not the cheating (page -1) options! */
   for (i = 0; (options[i].page > 0) ; i++)
   {
      ob++;
      if (ob == 64)
      {
         os++;
         ob = 0;
      }
dlog(DEBUGSAVE2,"save.c: wr_options: option set %d bit %d text %s res %d\n",
                os, ob, options[i].descr, *options[i].variable);

      /* Extract a variable setting, if possible */
      if (*options[i].variable)
      {
          opt[os] |= (1LL << ob);
dlog(DEBUGSAVE2,"save.c: wr_options: opt[%d] now %016Lx\n", i, opt[os]);
      }
   }

   /* Write the options */
   for (i = 0; i < 5; i++)
   {
dlog(DEBUGSAVE2,"save.c: wr_options: writing options %d = %016Lx\n", i, opt[i]);
       wr_u64b(fff, opt[i]);
   }

   /*** Special Options ***/

   /* Write "delay_spd" */
   wr_byte(fff, delay_spd);

   /* Write "hitpoint_warn" */
   wr_byte(fff, hitpoint_warn);

   /*** Cheating options ***/

   c = 0;

   if (wizard) c |= 0x0002;

   if (cheat_spell_info)       c |= 0x00000080L;
   if (cheat_peek)             c |= 0x00000100L;
   if (cheat_hear)             c |= 0x00000200L;
   if (cheat_room)             c |= 0x00000400L;
   if (cheat_xtra)             c |= 0x00000800L;
   if (cheat_know)             c |= 0x00001000L;
   if (cheat_live)             c |= 0x00002000L;
   if (cheat_hitpoints)        c |= 0x00004000L;
   if (cheat_mode)             c |= 0x00008000L;
   if (cheat_numeric_skills)   c |= 0x00010000L;

dlog(DEBUGSAVE2,"save.c: wr_options: cheating options %04x\n", c);
   wr_u32b(fff, c);
   /*** Window Options ***/
   for (i = 0; i < 8; i++)
   {
      /* Flags */
      flag[i] = op_ptr->window_flag[i];

      /* Mask */
      mask[i] = 0L;

      /* Build the mask */
      for (k = 0; k < 32; k++)
      {
         /* Set mask */
         if (window_flag_desc[k])
         {
            mask[i] |= (1L << k);
         }
      }
   }

   /* Dump the flags */
   for (i = 0; i < 8; i++)
   {
dlog(DEBUGSAVE2,"save.c: wr_options: flag[%d] val %08lx ,ask %08lx\n", i, flag[i], mask[i]);
      wr_u32b(fff, flag[i]);
      wr_u32b(fff, mask[i]);
   }
}

/*
 * Write some "extra" info
 */
static void wr_extra(FILE *fff)
{
   s16b i;

   wr_u32b(fff, debuglevel);

   wr_string(fff, player_name);

   wr_string(fff, died_from);

   for (i = 0; i < 4; i++)
   {
       wr_string(fff, history[i]);
   }
   for (i = 0; i < 4; i++)
   {
       wr_string(fff, teacher[i]);
   }

   /* Race/Class/Gender/Spells */
   wr_byte(fff, p_ptr->prace);
   wr_s16b(fff, p_ptr->pclass);
   wr_byte(fff, p_ptr->psex);

   wr_byte(fff, p_ptr->hitdie);
   wr_s16b(fff, p_ptr->expfact);
   wr_bool(fff, p_ptr->teach_birth);
   if (p_ptr->teach_birth)
   {
      wr_s16b(fff, p_ptr->teach_dis);
      wr_s16b(fff, p_ptr->teach_dev);
      wr_s16b(fff, p_ptr->teach_sav);
      wr_s16b(fff, p_ptr->teach_stl);
      wr_s16b(fff, p_ptr->teach_srh);
      wr_s16b(fff, p_ptr->teach_pcp);
      wr_s16b(fff, p_ptr->teach_thn);
      wr_s16b(fff, p_ptr->teach_thb);
      wr_s16b(fff, p_ptr->teach_tht);
      wr_s16b(fff, p_ptr->teach_exp);
      for (i=0; i < 6; i++) wr_s16b(fff, p_ptr->teach_stat[i]);
   }


   wr_s16b(fff, p_ptr->age);
   wr_s16b(fff, p_ptr->ht);
   wr_s16b(fff, p_ptr->wt);

/* jk - write this info */
   wr_bool(fff, p_ptr->twohands);
   wr_byte(fff, p_ptr->tactic);
   wr_byte(fff, p_ptr->movement);

   /* Dump the stats (maximum and current) */
   for (i=0; i < 6; i++)
   {
      wr_s16b(fff, p_ptr->stat_max[i]);
      wr_s16b(fff, p_ptr->stat_cur[i]);
      wr_s16b(fff, p_ptr->stat_cnt[i]);
      wr_s16b(fff, p_ptr->stat_los[i]);
   }

   wr_u32b(fff, p_ptr->au);

   wr_u32b(fff, p_ptr->max_exp);
   wr_u32b(fff, p_ptr->exp);
   wr_u16b(fff, p_ptr->exp_frac);
   wr_s16b(fff, p_ptr->lev);

   wr_s16b(fff, p_ptr->mhp);
   wr_s16b(fff, p_ptr->chp);
   wr_u16b(fff, p_ptr->chp_frac);

   wr_s16b(fff, p_ptr->msp);
   wr_s16b(fff, p_ptr->csp);
   wr_u16b(fff, p_ptr->csp_frac);

   /* Max Player and Dungeon Levels */
   wr_s16b(fff, p_ptr->max_plv);
   wr_s16b(fff, p_ptr->max_dlv);

   /* More info */
   wr_s16b(fff, p_ptr->sc);

   wr_s16b(fff, p_ptr->blind);
   wr_s16b(fff, p_ptr->paralyzed);
   wr_s16b(fff, p_ptr->confused);
   wr_s32b(fff, p_ptr->food);
   wr_s16b(fff, p_ptr->energy);
   wr_s16b(fff, p_ptr->fast);
   wr_s16b(fff, p_ptr->slow);
   wr_s16b(fff, p_ptr->afraid);
   wr_s16b(fff, p_ptr->cut);
   wr_s16b(fff, p_ptr->stun);
   wr_byte(fff, p_ptr->arena_state);
/* jk */
   wr_bool(fff, save_levels);
   wr_s16b(fff, p_ptr->sliding);
   wr_bool(fff, p_ptr->sliding_now);
   wr_s16b(fff, p_ptr->fire);
   wr_s16b(fff, p_ptr->cold);
   wr_s16b(fff, p_ptr->acid);
   wr_s16b(fff, p_ptr->elec);
   wr_s16b(fff, p_ptr->reading);

   wr_s16b(fff, p_ptr->poisoned);
   wr_s16b(fff, p_ptr->image);
   wr_s16b(fff, p_ptr->protevil);
   wr_u16b(fff, p_ptr->invuln);
   wr_s16b(fff, p_ptr->hero);
   wr_s16b(fff, p_ptr->shero);
   wr_s16b(fff, p_ptr->shield);
   wr_s16b(fff, p_ptr->blessed);
   wr_s16b(fff, p_ptr->tim_invis);
   wr_s16b(fff, p_ptr->word_recall);
   wr_s16b(fff, p_ptr->see_infra);
   wr_s16b(fff, p_ptr->tim_infra);
   wr_s16b(fff, p_ptr->oppose_fire);
   wr_s16b(fff, p_ptr->oppose_cold);
   wr_s16b(fff, p_ptr->oppose_acid);
   wr_s16b(fff, p_ptr->oppose_elec);
   wr_s16b(fff, p_ptr->oppose_pois);

   wr_byte(fff, p_ptr->confusing);
   wr_byte(fff, p_ptr->searching);
   wr_byte(fff, p_ptr->reflecting);

   /* Special stuff */
   wr_u16b(fff, panic_save);
   wr_u16b(fff, total_winner);
   wr_u16b(fff, noscore);

   /* Write death */
   wr_byte(fff, death);

   /* Write feeling */
   wr_byte(fff, feeling);

   /* Turn of last "feeling" */
   wr_s32b(fff, old_turn);

   /* Current turn */
   wr_s32b(fff, turn);

   /* Home inventory */
dlog(DEBUGSAVE,"save.c: wr_extra: saving home with %d items\n", store[0].stock_num);
   wr_store(fff, &store[0]);
}

/*
 * Write the current dungeon
 *
 */
static void wr_dungeon(FILE *fff, s16b baselevel)
{
   s16b i, j, y, x;
   s16b old_sublevel = -1;

   cave_cell_type *c_ptr;

   object_type *i_ptr;

/* jk - now for keeping the indexes of non-monster inventory items in */
   s16b index[MAX_I_IDX];
   s16b number;

   old_sublevel = sublevel;

   sublevel = 0;

   /* Dungeon specific info follows */
   wr_u16b(fff, num_repro);
   wr_u16b(fff, panel_max_rows);
   wr_u16b(fff, panel_max_cols);

   /* Compact the objects */
   compact_objects(0,TRUE);
   compact_traps();

dlog(DEBUGSAVE,"save.c: wr_dungeon: repro %d hgt %d wid %d p_ptr->wys %d cols %d\n",
              num_repro, cur_hgt, cur_wid, panel_max_rows, panel_max_cols);

   /* Dump the cave, write all used sublevels including the main level at 0 */
   for (sublevel = 0; sublevel < MAX_SUB_LEVELS; sublevel++)
   {
      if (dungeon.level_used[sublevel])
      {
         wr_byte(fff, 1);
         cur_wid = dungeon.level_wid[sublevel];
         cur_hgt = dungeon.level_hgt[sublevel];
         wr_u16b(fff, cur_wid);
         wr_u16b(fff, cur_hgt);
         wr_u16b(fff, dungeon.level_name[sublevel]);
         wr_u16b(fff, dungeon.level_depth[sublevel]);

         for (y = 0; y < cur_hgt; y++)
         {
            for (x = 0; x < cur_wid; x++)
            {
               /* Get the cave */
               c_ptr = &dungeon.level[sublevel][y][x];

                /* Paranoia */
               for (j = objects_on_floor(x,y)-1;j>=0;j--)
               {
                  object_type *i_ptr=get_item_pointer_floor_xy(j,x,y);
                  i_ptr->ix = x;
                  i_ptr->iy = y;
                  i_ptr->iz = sublevel;
               }

               /* Paranoia */
               if (c_ptr->m_idx)
               {
                  monster_type *m_ptr = &mn_list[c_ptr->m_idx];
                  m_ptr->fx = x;
                  m_ptr->fy = y;
                  m_ptr->fz = sublevel;
               }
               if (c_ptr->t_idx!=0)
               {
                  trap_item_type *tr_ptr = &t_list[c_ptr->t_idx];
                  tr_ptr->inuse = TRUE;
                  tr_ptr->tx = x;
                  tr_ptr->ty = y;
                  tr_ptr->tz = sublevel; /* this is the main level */
               }
               wr_u32b(fff, c_ptr->fdat);
               wr_u16b(fff, c_ptr->mtyp);
               wr_u16b(fff, c_ptr->styp);
               wr_s32b(fff, c_ptr->extra);
               wr_u16b(fff, c_ptr->memory_mtyp);
               wr_u16b(fff, c_ptr->memory_styp);
dlog(DEBUGSAVE2,"save.c: wr_dungeon:cave %d,%d,%d fdat %08lx mtyp %d styp %d extra %d mem mt %d st %d\n",
                x, y, sublevel, c_ptr->fdat, c_ptr->mtyp, c_ptr->styp, c_ptr->extra,
                c_ptr->memory_mtyp, c_ptr->memory_styp);
               /* we don't save t_idx, m_idx or i_idx: the objects are later on placed */
               /* on the correct grids based on the x,y,z values in them */
            }
         }
      }
      else /* unused sublevel */
      {
         wr_byte(fff, 0);
      }
   }
   sublevel = old_sublevel;
dlog(DEBUGSAVE,"save.c: wr_dungeon: calling compact_monsters sublevel %d\n", sublevel);

   /* Compact the monsters - don't cut back on the inventory ones */
   compact_monsters(0,FALSE);

   /* Dump the "real" monsters */
dlog(DEBUGSAVE,"save.c: wr_dungeon: writing %d monsters\n", mn_max);
   wr_u16b(fff, mn_max);
   for (i = 1; i < mn_max; i++)
   {
      monster_type *m_ptr = &mn_list[i];

dlog(DEBUGSAVE2,"save.c: wr_dungeon: writing monster %d of %d (%s) @ %ld\n",
                i, mn_max, r_name + r_info[m_ptr->r_idx].name, ftell(fff));
      wr_monster(fff, m_ptr);
      if (m_ptr->has_drop)
      {
         s16b j;
         s16b is_idx= item_set_this_monster(i);
         /* find the correct item_set belonging to this monster */
         item_set_type *is_ptr = &is_list[is_idx];

         number = items_in_set(is_idx);
dlog(DEBUGSAVE2,"save.c: wr_dungeon: writing %d items in monster inv is_idx %d @ %ld\n",
                number, is_idx, ftell(fff));
         wr_byte(fff, (byte)number);
         if (number>0)
         {
            for (j = 0; j < number; j++)
            {
               object_type *i_ptr = &i_list[is_ptr->index[j]];
dlog(DEBUGSAVE2,"save.c: wr_dungeon: writing monster item %d of %d index %d @ %ld\n",
                j, number, is_ptr->index[j], ftell(fff));
               wr_item(fff, i_ptr);
            }
         }
      }
      else
      {
         wr_byte(fff, 0);
      }
   }

   /* find out how many objects in i_list are on the floor, ie not in */
   /* monster inventories, signaled by ix and iy being -1             */
   /* monster inventories are saved with the monsters!                */
   number = 1;
   for (i = 1;i<i_max; i++)
   {
      if (i_list[i].ix!=-1)
      {
         index[number++]=i;
      }
   }

dlog(DEBUGSAVE,"save.c: wr_dungeon: writing %d floor items\n", number);
   /* Total objects */
   wr_u16b(fff, number);

   /* Dump the "real" items */
   for (i = 1; i < number; i++)
   {
dlog(DEBUGSAVE2,"save.c: wr_dungeon: writing floor item %d of %d\n", i, number);
      i_ptr = &i_list[index[i]];
      wr_item(fff, i_ptr);
   }

   wr_u16b(fff, t_max);
dlog((DEBUGSAVE|DEBUGTRAPS),"save.c: wr_dungeon: writing %d traps\n", t_max);
   for (i=1; i<t_max;i++)
   {
      trap_item_type *tr_ptr = &t_list[i];
dlog((DEBUGSAVE2|DEBUGTRAPS),"save.c: wr_dungeon: writing trap_item %d of %d\n", i, t_max);
      wr_trap_item(fff, tr_ptr);
   }
}

static void wr_dungeon_info(FILE *fff)
{
dlog(DEBUGSAVE,"save.c: wr_dungeon_info: writing p_ptr->depth as %d/%d\n",
               p_ptr->mdepth, p_ptr->sdepth);
   wr_s16b(fff, p_ptr->mdepth);
   wr_s16b(fff, p_ptr->sdepth);

}

/* jk - dump level_info array */
static void wr_level_info(FILE *fff)
{
   s16b i;

   for (i=0;i<MAX_LEVEL;i++)
   {
      wr_byte(fff, level_info[i].saved?1:0);
      wr_u32b(fff, level_info[i].first_visit);
      wr_u32b(fff, level_info[i].last_visit);
   }
}

/*
 * this function removes a ghost-file
 */
void remove_ghost_file(s16b num)
{
   char buf[1024];
   s16b tries;
   int fd;

   strcpy (buf, ANGBAND_DIR_BONE);
   strcat (buf, format("ghost%03d", num));
dlog(DEBUGGHOST,"save.c: remove_ghost_file: name %s\n", buf);

#if defined(MACINTOSH) && !defined(applec)
   /* Global -- "save file" */
   _ftype = 'LOAD';
#endif
   fd=fd_open(buf, O_RDONLY);
   tries = 0;
   /* if the file cannot be opened, don't remove it */
   if (fd<0) return;
   while (tries<100)
   {
      /* if we cannot lock it */
      if (fd_lock(fd, F_WRLCK))
      {
         msg_format("Locking error on ghost file %s, waiting 10 seconds.", buf);
         tries++;
         delay(10);
      }
      else /* break this loop */
      {
         tries = 200;
      }
   }
   if (tries != 200)
   {
      /* not removing this file isn't really so bad, after all */
      return;
   }
   (void)fd_kill(buf);
   (void)fd_close(fd);
}

/*
 * this function writes a ghost (as an entry in r_info)
 * to the savefile
 */
void wr_ghost(FILE *fff, s16b r_idx)
{
   s16b i, count, g_idx;

   monster_race *r_ptr = &r_info[r_idx];
   g_idx = r_idx - r_number;

dlog(DEBUGGHOST,"save.c: wr_ghost: starting @ r_idx %d g_idx %d\n", r_idx, g_idx);

   wr_string(fff, r_name + r_ptr->name);
   wr_string(fff, r_text + r_ptr->text);

dlog(DEBUGGHOST,"save.c: wr_ghost: name %s\n", r_name + r_ptr->name);
dlog(DEBUGGHOST,"save.c: wr_ghost: text %s\n", r_text + r_ptr->text);

   wr_byte(fff, r_ptr->hdice);
   wr_byte(fff, r_ptr->hside);
   wr_s16b(fff, r_ptr->ac);
   wr_s16b(fff, r_ptr->sleep);
   wr_byte(fff, r_ptr->aaf);
   wr_byte(fff, r_ptr->speed);
   wr_s32b(fff, r_ptr->mexp);
dlog(DEBUGGHOST,"save.c: wr_ghost: hd %d hs %d ac %d sleep %d aaf %d spd %d mexp %d\n",
                r_ptr->hdice, r_ptr->hside, r_ptr->ac, r_ptr->sleep, r_ptr->aaf, r_ptr->speed,
                r_ptr->mexp);

   wr_byte(fff, r_ptr->freq_inate);
   wr_byte(fff, r_ptr->freq_spell);
dlog(DEBUGGHOST,"save.c: wr_ghost:ina %d spl %d\n", r_ptr->freq_inate, r_ptr->freq_spell);

   wr_u64b(fff, r_ptr->flags1);
   wr_u64b(fff, r_ptr->flags2);
   wr_u64b(fff, r_ptr->flags3);
dlog(DEBUGGHOST,"save.c: wr_ghost: flags1 %16Lx flags2 %16Lx flags3 %16Lx\n",
                r_ptr->flags1, r_ptr->flags2, r_ptr->flags3);
   wr_u64b(fff, r_ptr->flags4);
   wr_u64b(fff, r_ptr->flags5);
   wr_u64b(fff, r_ptr->flags6);
dlog(DEBUGGHOST,"save.c: wr_ghost: flags4 %16Lx flags5 %16Lx flags6 %16Lx\n",
                r_ptr->flags4, r_ptr->flags5, r_ptr->flags6);

   /* corpse properties */
   wr_u32b(fff, r_ptr->corpse_gives_alw);
   wr_u32b(fff, r_ptr->corpse_takes_alw);
   wr_u32b(fff, r_ptr->corpse_gives_smt);
   wr_u32b(fff, r_ptr->corpse_takes_smt);
dlog(DEBUGGHOST,"save.c: wr_ghost: corpse gva %08lx tka %08lx gvs %08lx tks %08lx\n",
                r_ptr->corpse_gives_alw, r_ptr->corpse_takes_alw,
                r_ptr->corpse_gives_smt, r_ptr->corpse_takes_smt);

   wr_s16b(fff, r_ptr->corpse_chance);
   wr_s16b(fff, r_ptr->corpse_nutrition);
   wr_s16b(fff, r_ptr->corpse_weight);
   wr_s32b(fff, r_ptr->corpse_spoiling);
   wr_s16b(fff, r_ptr->corpse_chance_bones);
   wr_byte(fff, r_ptr->corpse_chance_gives);
   wr_byte(fff, r_ptr->corpse_chance_takes);

dlog(DEBUGGHOST,"save.c: wr_ghost: corpse chance %d nut %d wgt %d spoil %ld chb %d chg %d cht %d\n",
                r_ptr->corpse_chance, r_ptr->corpse_nutrition, r_ptr->corpse_weight,
                r_ptr->corpse_spoiling, r_ptr->corpse_chance_bones, r_ptr->corpse_chance_gives,
                r_ptr->corpse_chance_takes);

   /* blow information */
   for (i=0; i<4; i++)
   {
      wr_byte(fff, r_ptr->blow[i].method);
      wr_byte(fff, r_ptr->blow[i].effect);
      wr_byte(fff, r_ptr->blow[i].d_dice);
      wr_byte(fff, r_ptr->blow[i].d_side);
dlog(DEBUGGHOST,"save.c: wr_ghost: blow %d meth %d eff %d %dd%d\n",
                i, r_ptr->blow[i].method, r_ptr->blow[i].effect,
                r_ptr->blow[i].d_dice, r_ptr->blow[i].d_side);
   }

   wr_byte(fff, r_ptr->level);
   wr_byte(fff, r_ptr->rarity);

   wr_byte(fff, r_ptr->d_attr);
   wr_byte(fff, (byte)r_ptr->d_char);

   wr_byte(fff, r_ptr->x_attr);
   wr_byte(fff, (byte)r_ptr->x_char);
dlog(DEBUGGHOST,"save.c: wr_ghost: lev %d rar %d da %d dc %d xa %d da %d\n",
                r_ptr->level, r_ptr->rarity, r_ptr->d_attr, r_ptr->d_char,
                r_ptr->x_attr, r_ptr->x_char);

   wr_byte(fff, r_ptr->max_num);
   wr_byte(fff, r_ptr->cur_num);

   wr_byte(fff, r_ptr->max_gen);
   wr_s16b(fff, r_ptr->r_sights);
   wr_s16b(fff, r_ptr->r_deaths);

   wr_s16b(fff, r_ptr->r_pkills);
   wr_s16b(fff, r_ptr->r_tkills);
dlog(DEBUGGHOST,"save.c: wr_ghost: mnm %d cnm %d gen %d rs %d rd %d r_pk %d r_tk %d\n",
                r_ptr->max_num, r_ptr->cur_num, r_ptr->max_gen, r_ptr->r_sights,
                r_ptr->r_deaths, r_ptr->r_pkills, r_ptr->r_tkills);

   wr_byte(fff, r_ptr->r_wake);
   wr_byte(fff, r_ptr->r_ignore);

   wr_byte(fff, r_ptr->r_xtra1);
   wr_byte(fff, r_ptr->r_xtra2);

   wr_byte(fff, r_ptr->r_drop_gold);
   wr_byte(fff, r_ptr->r_drop_item);

   wr_byte(fff, r_ptr->r_cast_inate);
   wr_byte(fff, r_ptr->r_cast_spell);
dlog(DEBUGGHOST,"save.c: wr_ghost: r_w %d r_i %d r_x1 %d r_x2 %d dg %d di %d rci %d rcs %d\n",
                r_ptr->r_wake, r_ptr->r_ignore, r_ptr->r_xtra1, r_ptr->r_xtra2,
                r_ptr->r_drop_gold, r_ptr->r_drop_item, r_ptr->r_cast_inate,
                r_ptr->r_cast_spell);

   for (i=0; i<4; i++)
   {
      wr_byte(fff, r_ptr->r_blows[i]);
   }
dlog(DEBUGGHOST,"save.c: wr_ghost: rb0 %d rb1 %d rb2 %d rb3 %d\n",
                r_ptr->r_blows[0], r_ptr->r_blows[1], r_ptr->r_blows[2], r_ptr->r_blows[3]);

   wr_u64b(fff, r_ptr->r_flags1);
   wr_u64b(fff, r_ptr->r_flags2);
   wr_u64b(fff, r_ptr->r_flags3);
dlog(DEBUGGHOST,"save.c: wr_ghost: rflags1 %16Lx rflags2 %16Lx rflags3 %16Lx\n",
                r_ptr->r_flags1, r_ptr->r_flags2, r_ptr->r_flags3);
   wr_u64b(fff, r_ptr->r_flags4);
   wr_u64b(fff, r_ptr->r_flags5);
   wr_u64b(fff, r_ptr->r_flags6);
dlog(DEBUGGHOST,"save.c: wr_ghost: rflags4 %16Lx rflags5 %16Lx rflags6 %16Lx\n",
                r_ptr->r_flags4, r_ptr->r_flags5, r_ptr->r_flags6);
   count = 0;
   for (i=0; i<ITEM_SET_SIZE; i++)
   {
      if (ghost_info[g_idx].inv[i].k_idx)
         count++;
      else
         break;
   }
   wr_s16b(fff, count);
dlog((DEBUGGHOST | DEBUGSAVE),"save.c: wr_ghost: %d items\n",count);
   if (count>0)
   {
      for (i=0; i<count; i++)
      {
         wr_item(fff, &ghost_info[g_idx].inv[i]);
      }
   }
dlog(DEBUGGHOST,"save.c: wr_ghost: ending\n");
}

/*
 * This function writes all the ghosts a player can meet in this game
 * multi-player capabilities prevent us from reusing ghosts from a
 * common pool other than at startup of a new game
 */
static void wr_ghost_info(FILE *fff)
{
   s16b i;

   wr_s16b(fff, r_number_total - r_number);
   for (i=r_number; i<r_number_total; i++)
   {
dlog(DEBUGGHOST,"save.c: wr_ghost_info: about to write ghost %d of %d\n", i, r_number_total);
      wr_ghost(fff, i);
dlog(DEBUGGHOST,"save.c: wr_ghost_info: written ghost %d of %d\n", i, r_number_total);
   }
dlog(DEBUGGHOST,"save.c: wr_ghost_info: ending\n");
}

/*
 * Actually write a save-file
 */
static bool wr_savefile(FILE *fff)
{
   s16b                 i;

   u32b                now;

   byte                tmp8u;
   u16b                tmp16u;

   /* Guess at the current time */
   now = time((time_t *)0);

   /* Note the operating system */
   sf_xtra = 0L;

   /* Note when the file was saved */
   sf_when = now;

   /* Note the number of saves */
   sf_saves++;

   /*** Actually write the file ***/

dlog(DEBUGSAVE,"save.c: wr_savefile: writing version\n");
   wr_byte(fff, VERSION_MAJOR);
   wr_byte(fff, VERSION_MINOR);
   wr_byte(fff, VERSION_PATCH);
   tmp8u = rand_int(256);
   wr_byte(fff, tmp8u);

   /* Operating system */
   wr_u32b(fff, sf_xtra);

   /* Time file last saved */
   wr_u32b(fff, sf_when);

   /* Number of past lives */
   wr_u16b(fff, sf_lives);

   /* Number of times saved */
   wr_u16b(fff, sf_saves);

   /* Space */
   wr_u32b(fff, 0L);
   wr_u32b(fff, 0L);

   /* Write the boolean "options" */
dlog(DEBUGSAVE,"save.c: wr_savefile: writing options\n");
   wr_options(fff);

dlog(DEBUGSAVE,"save.c: wr_savefile: writing flavors\n");
   wr_flavors(fff);

dlog(DEBUGSAVE,"save.c: wr_savefile: writing dungeon info\n");
   wr_dungeon_info(fff);

dlog(DEBUGSAVE,"save.c: wr_savefile: writing level info\n");
   wr_level_info(fff);

dlog(DEBUGSAVE,"save.c: wr_savefile: writing ghost info\n");
   wr_ghost_info(fff);

   /* Dump the monster lore, including ghosts' lore */
dlog(DEBUGSAVE,"save.c: wr_savefile: writing lore (%d monsters)\n", r_number_total);
   tmp16u = r_number_total;
   wr_u16b(fff, tmp16u);
   for (i = 0; i < tmp16u; i++)
   {
dlog(DEBUGSAVE2,"save.c: wr_savefile: writing lore %d of %d\n", i, tmp16u);
      wr_lore(fff, i);
   }

dlog(DEBUGSAVE,"save.c: wr_savefile: writing %d trap knowledge records\n", t_number);
   tmp16u = t_number;
   wr_u16b(fff, tmp16u);
   for (i=0; i< tmp16u; i++)
   {
      trap_type *t_ptr = &t_info[i];
      wr_s16b(fff, t_ptr->known);
      wr_bool(fff, t_ptr->ident);
      wr_u32b(fff, t_ptr->flags);
   }

   /* Dump the object memory */
dlog(DEBUGSAVE,"save.c: wr_savefile: writing %d object memory records\n", k_number);
   tmp16u = k_number;
   wr_u16b(fff, tmp16u);
   for (i = 0; i < tmp16u; i++)
   {
      wr_xtra(fff, i);
      wr_item_log(fff, &k_info[i].log);
   }

   /* Hack -- Dump the quests */
dlog(DEBUGSAVE,"save.c: wr_savefile: writing %d quests\n", MAX_Q_IDX);
   tmp16u = MAX_Q_IDX;
   wr_u16b(fff, tmp16u);
   for (i = 0; i < tmp16u; i++)
   {
      wr_byte(fff, q_list[i].level);
   }

dlog(DEBUGSAVE,"save.c: wr_savefile: writing %d artifacts\n", a_number);
   /* Hack -- Dump the artifacts */
   tmp16u = a_number;
   wr_u16b(fff, tmp16u);
   for (i = 0; i < tmp16u; i++)
   {
      artifact_type *a_ptr = &a_info[i];
      wr_byte(fff, a_ptr->cur_num);
      wr_u16b(fff, a_ptr->ident);
      wr_item_log(fff, &a_ptr->log);
   }

dlog(DEBUGSAVE,"save.c: wr_savefile: writing %d ego-items\n", e_number);
   /* hack -- dump the ego-items */
   tmp16u = e_number;
   wr_u16b(fff, tmp16u);
   for (i = 0; i < tmp16u; i++)
   {
      ego_item_type *e_ptr = &e_info[i];
      wr_item_log(fff, &e_ptr->log);
   }

dlog(DEBUGSAVE,"save.c: wr_savefile: writing extra info\n");
   /* Write the "extra" information */
   wr_extra(fff);

   /* Dump the "player hp" entries */
dlog(DEBUGSAVE,"save.c: wr_savefile: writing player hp entries\n");
   tmp16u = PY_MAX_LEVEL;
   wr_u16b(fff, tmp16u);
   for (i = 0; i < tmp16u; i++)
   {
      wr_s16b(fff, player_hp[i]);
      wr_u32b(fff, level_reached[i]);
   }

   /* Dump the "spell numcast" entries */
   for (i=0; i < s_number; i++)
   {
dlog(DEBUGSAVE,"save.c: wr_savefile: writing spell numcast entries spell %d numcast %d\n",
               i, s_info[i].numcast);
      wr_u32b(fff, s_info[i].numcast);
   }

dlog(DEBUGSAVE,"save.c: wr_savefile: writing arena info\n");
   /* write some info about the arena */
   wr_bool(fff, been_in_arena);
   wr_s32b(fff, arena_reward);
   wr_s32b(fff, arena_previous_monsters);
   wr_s32b(fff, arena_monsters);
   for (i=1; i<=PY_MAX_LEVEL; i++)
   {
     wr_s16b(fff, arena_visit_level[i]);
   }

dlog(DEBUGSAVE,"save.c: wr_savefile: writing %d inventory items\n", INVEN_TOTAL);
   /* Write the inventory */
   for (i = 0; i < INVEN_TOTAL; i++)
   {
dlog(DEBUGSAVE2,"save.c: wr_savefile: writing inventory item %d of %d\n", i, INVEN_TOTAL);
      wr_item(fff, &inventory[i]);
   }
   /* Player is not dead, write position */
   if (!death)
   {
       wr_u16b(fff, px);
       wr_u16b(fff, py);
dlog(DEBUGSAVE,"save.c: wr_savefile: player at %d,%d\n", px, py);
   }

   /* Error in save */
   if (ferror(fff) || (fflush(fff) == EOF)) return FALSE;

   /* Successful save */
dlog(DEBUGSAVE,"save.c: wr_savefile: ending\n");
   return TRUE;
}

/*
 * this function changes the last 2 characters in string
 * so that we code a number up to 1296 in radix-36 there.
 */
void make_extension(char *string, s16b number)
{
   s16b mod;
   s16b len = strlen(string);

/* we use a radix-36 count here, so <10 gives 0-9, >=10 gives A-Z */
/* we have 36*36 = 1296 max levels, which seem enough */

   mod=number % 36;
   string[len-1]=(mod<10)?48+mod:65+(mod-10);

   number= number / 36;
   mod=number % 36;
   string[len-2]=(mod<10)?48+mod:65+(mod-10);
}

void rm_level(s16b level)
{
   char temp[255];
   sprintf(temp,"%s.Lxx",levelfile);

   make_extension(temp,level);

#if defined(MACINTOSH) && !defined(applec)
   /* Global -- "save file" */
   _ftype = 'SAVE';
#endif

   (void)remove(temp);
}


void compress_progress_indicator(int doing_compress,
                                 unsigned long int compress_progress, unsigned long int compress_total)
{
   char buf[80];
   u16b result;
   static u16b old_result = -1;

   result = (u16b)((100L*compress_progress)/compress_total);

   if (result == old_result) return;
   old_result = result;

   if (result > 100) result = 100;

   sprintf(buf, "%s: %3d%%",doing_compress?"Compressing":"Decompressing", result);
   msg_print("");
   prt(buf, 80-strlen(buf), 0);
   Term_fresh();
   Term_flush();
}

void compress_file(cptr name_in, cptr name_out, cptr type)
{
   FILE *in_file, *out_file;
   bool old_inkey_scan = inkey_scan;
   inkey_scan = TRUE;
dlog(DEBUGSAVE,"save.c: compress_file: about to compress savefile %s into %s\n",
     name_in, name_out);

#if defined(MACINTOSH) && !defined(applec)
   /* Global -- "save file" */
   _ftype = 'LOAD';
#endif

   /* Create the savefile */
   /* Open the savefile */
   in_file = my_fopen(name_in, "rb");
   /* Successful open */
   if (!in_file)
   {
      (void)remove(name_in);
      msg_format("save.c: compress_file: Error opening %s for reading.",
                 name_in);
      return;
   }
   compress_length = my_flength(in_file);
   if (my_fclose(in_file))
   {
      msg_format("save.c: compress_file: Error closing %s before compressing.",
                 name_in);
      return;
   }

#if defined(MACINTOSH) && !defined(applec)
   /* Global -- "save file" */
   _ftype = 'SAVE';
#endif

   out_file = my_fopen(name_out, "wb");
   if (!out_file)
   {
      (void)remove(name_out);
      msg_format("save.c: compress_file: Error opening %s for writing.",
                 name_out);
      return;
   }
   if (my_fclose(out_file))
   {
      msg_format("save.c: compress_file: Error closing %s before compressing.",
                 name_out);
   }

   msg_format("Compressing %s.", type);
dlog(DEBUGCMPRS,"save.c: compress_file: compressing %s file %s into %s \n",
     type, name_in, name_out);
#ifdef COMPRESS_LZW
   (void)lzw_compress_file(0, name_in, name_out);
#endif
#ifdef COMPRESS_RLE
   (void)rle_compress_file(0, name_in, name_out);
#endif
   compress_length = 0L;
   out_file = my_fopen(name_out, "rb");
   if (!out_file)
   {
      (void)remove(name_out);
      msg_format("save.c: compress_file: Error opening %s after compressing.",
                 name_out);
      return;
   }
dlog(DEBUGCMPRS,"save.c: compress_file: outfile %s (%ld)\n",
               name_out, my_flength(out_file));
   if (my_fclose(out_file))
   {
      msg_format("save.c: compress_file: Error closing %s after compressing.",
                 name_out);
   }

dlog(DEBUGCMPRS,"save.c: compress_file: removing %s\n", name_in);
   (void)remove(name_in); /* we've decompressed it - now remove it */
   msg_print(NULL);
   inkey_scan = old_inkey_scan;
}

static void close_compress_file(cptr name, cptr type)
{
   FILE *in_file;
   char tempname[1024];

#if defined(MACINTOSH) && !defined(applec)
   /* Global -- "save file" */
   _ftype = 'SAVE';
#endif
   strcpy(tempname, savefile);
   strcat(tempname, ".cmp");

dlog(DEBUGCMPRS,"save.c: close_compress_file: name %s, type %s, temp %s\n",
               name, type, tempname);
   in_file = my_fopen(name, "rb");
dlog(DEBUGCMPRS,"save.c: close_compress_file: file %s (%ld)\n",
     name, my_flength(in_file));
  (void)my_fclose(in_file);

   compress_file(name, tempname, type);
dlog(DEBUGCMPRS,"save.c: close_compress_file: compression done!\n");
   in_file = my_fopen(tempname, "rb");
dlog(DEBUGCMPRS,"save.c: close_compress_file: after compress: outfile %s (%ld)\n",
     tempname, my_flength(in_file));
  (void)my_fclose(in_file);

   fd_move(tempname, name);
   in_file = my_fopen(name, "rb");
dlog(DEBUGCMPRS,"save.c: close_compress_file: after move: infile %s (%ld)\n",
     name, my_flength(in_file));
   (void)my_fclose(in_file);
}

/*
 * Actually write a level
 */
bool wr_level(void)
{
   s16b         i;
   u16b        tmp16u;
   bool        ok = FALSE;
   char        temp[255];
   FILE       *fff = NULL;

dlog(DEBUGSAVE,"save.c: wr_level: starting, levelfile=%s\n", levelfile);
   sprintf(temp,"%s.Lxx",levelfile); /* L for Level file */

   make_extension(temp,p_ptr->mdepth);

#if defined(MACINTOSH) && !defined(applec)
   /* Global -- "save file" */
   _ftype = 'SAVE';
#endif
dlog(DEBUGSAVE,"save.c: wr_level: file %s\n", temp);

   /* don't leave levelfiles around if we don't save levels */
   /* so remove the previous one, if any                    */
   /* but don't remove the savefile for level 00 with town  */
   if (!save_levels && sf_lastfile_name[0] &&
       (sf_lastfile_name[strlen(sf_lastfile_name)-1] != '0') && (sf_lastfile_name[strlen(sf_lastfile_name)-1] != '0') )
   {
      (void)fd_kill(sf_lastfile_name);
      strcpy(sf_lastfile_name, temp);
   }

   /* Create the savefile */
   fff = my_fopen(temp, "wb");

   /* Successful open */
   if (!fff)
   {
      (void)remove(temp);
   }
   else
   {
      /* leave signature */
      wr_byte(fff, (byte)'A');
      wr_byte(fff, (byte)'6');
      wr_byte(fff, (byte)'4');

      tmp16u = num_stores;

      msg_print(NULL);
      msg_print("Saving old level:");
dlog(DEBUGSAVE,"save.c: wr_level: %d stores to write\n", num_stores);

      /* Dump the stores */
      wr_u16b(fff, tmp16u);
      /* store number 0 is home, and it is not written here! */
      for (i = 1; i < tmp16u; i++)
      {
dlog(DEBUGSAVE,"save.c: wr_level: writing store %d of %d\n", i, num_stores);
         wr_store(fff, &store[i]);
      }

      /* Dump the dungeon */
      wr_dungeon(fff, p_ptr->mdepth);

      wr_s16b(fff, px);
      wr_s16b(fff, py);
dlog(DEBUGSAVE,"save.c: wr_level: player at %d,%d\n", px, py);

      /* Error in save */
      if (ferror(fff) || (fflush(fff) == EOF))
      {
         return FALSE;
      }

      (void)my_fclose(fff);

      /* Successful save */
      ok = TRUE;
      level_info[p_ptr->mdepth].saved=TRUE;
      character_saved = TRUE;
   }

   if (compress_savefile)
   {
#ifdef ALLOW_COMPRESSION
      close_compress_file(temp, "levelfile");
#else
      msg_print("Sorry, compression-routines were not compiled in this binary!");
#endif
   }
   else
      msg_print(NULL);

   /* Failure */
   return(ok);
}


/*
 * Medium level player saver
 *
 * XXX XXX XXX Angband 2.8.0 will use "fd" instead of "fff" if possible
 */
static bool save_player_aux(char *name)
{
   bool        ok = FALSE;
   s16b        fd = -1;
   s16b        mode = 0644;
   FILE        *fff = NULL;

#if defined(MACINTOSH) && !defined(applec)
   /* Global -- "save file" */
   _ftype = 'SAVE';
#endif

dlog(DEBUGSAVE,"save.c: save_player_aux: starting name %s\n", name);
   /* Create the savefile */
   fd = fd_make(name, mode);

   /* File is okay */
   if (fd >= 0)
   {
      /* Close the "fd" */
      (void)fd_close(fd);

      /* Open the savefile */
      fff = my_fopen(name, "wb");
dlog((DEBUGGHOST|DEBUGSAVE),"save.c: save_player_aux: file %s opened\n", name);
      /* Successful open */
      if (fff)
      {
         /* leave signature */
         wr_byte(fff, (byte)'A');
         wr_byte(fff, (byte)'6');
         wr_byte(fff, (byte)'4');

         /* Write the savefile */
         /* we need to close it afterwards, *unless* we called  */
         /* close_compress_file, which moves the file around so */
         /* it's probably unsafe to keep it opened              */
         if (wr_savefile(fff))
         {
dlog((DEBUGGHOST|DEBUGSAVE),"save.c: save_player_aux: file %s saved (%ld bytes)\n",
     name, my_flength(fff));
            if (compress_savefile)
            {
#ifdef ALLOW_COMPRESSION
               (void)my_fclose(fff);
               close_compress_file(name, "savefile");
               ok = TRUE;
#else
               msg_print("Sorry, compression-routines not compiled!");
               if (my_fclose(fff)) ok = FALSE;
dlog((DEBUGGHOST|DEBUGSAVE),"save.c: save_player_aux: file %s closed1 ok %d\n", name, ok);
#endif
             }
             else
             {
                /* Attempt to close it */
                ok = TRUE;
                if (my_fclose(fff)) ok = FALSE;
dlog((DEBUGGHOST|DEBUGSAVE),"save.c: save_player_aux: file %s closed2 ok %d\n", name, ok);
             }
         }
dlog((DEBUGGHOST|DEBUGSAVE),"save.c: save_player_aux: wr_savefile called\n", name);

      }

      /* Remove "broken" files */
      if (!ok)
      {
dlog((DEBUGGHOST|DEBUGSAVE),"save.c: save_player_aux: file %s removed\n", name);
         (void)remove(name);
      }
   }

   /* Failure */
   if (!ok)
   {
dlog((DEBUGGHOST|DEBUGSAVE),"save.c: save_player_aux: returning FALSE\n");
      return (FALSE);
   }

   /* Successful save */
   character_saved = TRUE;

   /* Success */
dlog((DEBUGGHOST|DEBUGSAVE),"save.c: save_player_aux: ending\n");
   return (TRUE);
}

void rm_savefile(void)
{
   char        safe[1024];

   /* New savefile */
   strcpy(safe, savefile);
   /* Remove it */
   fd_kill(safe);
}

/*
 * Attempt to save the player in a savefile
 * return FALSE if failed, TRUE if succeeded
 */
bool save_player(void)
{
   s16b         result = FALSE;
   char        safe[1024];

/* jk */
   bool        old_inkey_scan = inkey_scan;
   inkey_scan = TRUE;

dlog(DEBUGSAVE,"save.c: save_player: starting\n");
#ifdef SET_UID

# ifdef SECURE

   /* Get "games" permissions */
   beGames();

# endif

#endif

   /* New savefile */
   strcpy(safe, savefile);
   strcat(safe, ".new");

#ifdef VM
   /* Hack -- support "flat directory" usage on VM/ESA */
   strcpy(safe, savefile);
   strcat(safe, "n");
#endif /* VM */

dlog(DEBUGSAVE,"save.c: save_player: killing safe: %s\n", safe);
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
#endif/* VM */

dlog(DEBUGSAVE,"save.c: save_player: killing temp: %s\n", temp);
      /* Remove it */
      fd_kill(temp);

      /* Preserve old savefile */
dlog(DEBUGSAVE,"save.c: save_player: moving %s to %s\n", savefile, temp);
      fd_move(savefile, temp);

dlog(DEBUGSAVE,"save.c: save_player: moving %s to %s\n", safe, savefile);
      /* Activate new savefile */
      fd_move(safe, savefile);

      /* Remove preserved savefile */
dlog(DEBUGSAVE,"save.c: save_player: killing temp: %s\n", temp);
      fd_kill(temp);

      /* Hack -- Pretend the character was loaded */
      character_loaded = TRUE;

#ifdef VERIFY_SAVEFILE

      /* Lock on savefile */
      strcpy(temp, savefile);
      strcat(temp, ".lok");

      /* Remove lock file */
dlog(DEBUGSAVE,"save.c: save_player: killing temp: %s\n", temp);
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


   inkey_scan = old_inkey_scan;
   /* Return the result */
dlog(DEBUGSAVE,"save.c: save_player: ending result %d\n", result);
   return (result);
}



/*
 * Attempt to Load a "savefile"
 *
 * On multi-user systems, you may only "read" a savefile if you will be
 * allowed to "write" it later, this prevents painful situations in which
 * the player loads a savefile belonging to someone else, and then is not
 * allowed to save his game when he quits.
 *
 * We return "TRUE" if the savefile was usable, and we set the global
 * flag "character_loaded" if a real, living, character was loaded.
 *
 * Note that we always try to load the "current" savefile, even if
 * there is no such file, so we must check for "empty" savefile names.
 */
bool load_player(void)
{
   s16b         fd = -1;

   errr        err = 0;

#ifdef VERIFY_TIMESTAMP
   struct stat statbuf;
#endif

   cptr        what = "generic";

   /* Paranoia */
   turn = 0;

   /* Paranoia */
   death = FALSE;

   /* Allow empty savefile name */
   if (!savefile[0])
   {
      return (TRUE);
   }

#if !defined(MACINTOSH) && !defined(WINDOWS) && \
   !defined(ACORN) && !defined(VM)

   /* XXX XXX XXX Fix this */

   /* Verify the existance of the savefile */
   if (access(savefile, 0) < 0)
   {
       /* Give a message */
       msg_format("Savefile '%s' not found.", savefile);
       msg_print(NULL);

       /* Allow this */
       return (TRUE);
   }

#endif

#ifdef VERIFY_SAVEFILE

   /* Verify savefile usage */
   if (!err)
   {
       FILE *fkk;

       char temp[1024];

       /* Extract name of lock file */
       strcpy(temp, savefile);
       strcat(temp, ".lok");

       /* Check for lock */
       fkk = my_fopen(temp, "r");

       /* Oops, lock exists */
       if (fkk)
       {
           /* Close the file */
           my_fclose(fkk);

           /* Message */
           msg_print("Savefile is currently in use.");
           msg_print(NULL);

           /* Oops */
           return (FALSE);
       }

       /* Create a lock file */
       fkk = my_fopen(temp, "w");

       /* Dump a line of info */
       fprintf(fkk, "Lock file for savefile '%s'\n", savefile);

       /* Close the lock file */
       my_fclose(fkk);
   }

#endif

   /* Okay */
   if (!err)
   {
       /* Open the savefile */
       fd = fd_open(savefile, O_RDONLY);

       /* No file */
       if (fd < 0) err = -1;

       /* Message (below) */
       if (err) what = format("Cannot open savefile %s", savefile);
   }

   /* Process file */
   if (!err)
   {

#ifdef VERIFY_TIMESTAMP
       /* Get the timestamp */
       (void)fstat(fd, &statbuf);
#endif

       /* Close the file */
       (void)fd_close(fd);
   }

   if (!err)
   {
      /* Clear screen */
      clear_screen();

      err = rd_savefile_new();

      /* Message (below) */
      if (err) what = "Cannot parse savefile";
   }

   /* Paranoia */
   if (!err)
   {
      /* Invalid turn */
      if (!turn) err = -1;

      /* Message (below) */
      if (err) what = "Broken savefile";
   }

#ifdef VERIFY_TIMESTAMP
   /* Verify timestamp */
   if (!err && !arg_wizard)
   {
      /* Hack -- Verify the timestamp */
      if (sf_when > (statbuf.st_ctime + 100) ||
          sf_when < (statbuf.st_ctime - 100))
      {
         /* Message */
         what = "Invalid timestamp";

         /* Oops */
         err = -1;
      }
   }
#endif

   /* Okay */
   if (!err)
   {
      if (!save_levels) rd_level();

      /* Player is dead */
      if (death)
      {
         /* Player is no longer "dead" */
         death = FALSE;

         /* Hack -- delay death */
         if (arg_wizard) return (TRUE);

         /* Count the past lives */
         sf_lives++;

         /* Forget the turn, and old_turn */
         turn = old_turn = 0;

         /* Done */
         return (TRUE);
      }

      /* A character was loaded */
      character_loaded = TRUE;

      /* Still alive */
      if (p_ptr->chp >= 0)
      {
         /* Reset cause of death */
         (void)strcpy(died_from, "(alive and well)");
      }

      /* Success */
      return (TRUE);
   }


#ifdef VERIFY_SAVEFILE

   /* Verify savefile usage */
   if (TRUE)
   {
      char temp[1024];

      /* Extract name of lock file */
      strcpy(temp, savefile);
      strcat(temp, ".lok");

      /* Remove lock */
      fd_kill(temp);
   }

#endif


   /* Message */
   msg_format("Error (%s) reading %d.%d.%d savefile.",
              what, sf_major, sf_minor, sf_patch);
   msg_print(NULL);


   /* Oops */
   return (FALSE);
}

void write_info_header(char *file, u16b number, u32b name_size, u32b text_size)
{
   char buf[1024];
   FILE *fff = NULL;

   strcpy (buf, ANGBAND_DIR_DATA);
   strcat (buf, file);

#if defined(MACINTOSH) && !defined(applec)
   /* Global -- "save file" */
   _ftype = 'SAVE';
#endif

   fff = my_fopen(buf, "wb");

   /* Successful open */
   if (!fff)
   {
      quit(format("Unable to open header file %s for writing.",file));
   }
   else
   {
dlog(DEBUGTEMPL,"save.c: write_info_header: file %s name %ld text %ld number %d\n",
                file, name_size, text_size, number);
      wr_byte(fff, VERSION_MAJOR);
      wr_byte(fff, VERSION_MINOR);
      wr_byte(fff, VERSION_PATCH);
      wr_u32b(fff, name_size);
      wr_u32b(fff, text_size);
      wr_u16b(fff, number);
   }
   (void)my_fclose(fff);
}

void write_info_data(char *file, char *array, u16b number, u16b itemsize,
                     char *name, u32b name_size, char *text, u32b text_size)
{
   char buf[1024];
   s16b fd;
   s16b mode = 0644;

   strcpy (buf, ANGBAND_DIR_DATA);
   strcat (buf, file);

#if defined(MACINTOSH) && !defined(applec)
   /* Global -- "save file" */
   _ftype = 'SAVE';
#endif

   fd = fd_make(buf, mode);

   /* Successful open */
   if (fd<0)
   {
       fd_kill(buf);
       fd = fd_make(buf, mode);
   }
   /* failure means we don't write, and rebuild from the *_info.txt files */
   /* the next time we need this file */
   if (fd<0)
   {
dlog(DEBUGALWAYS,"save.c: write_info_data: failed to open file '%s' for writing!", buf);
      return;
   }
   else
   {
      fd_write(fd, array, number * itemsize);
      fd_write(fd, name, name_size);
      fd_write(fd, text, text_size);

      if (fd_close(fd))
      {
         quit(format("Unable to close data file %s after writing.", file));
      }
   }
}
