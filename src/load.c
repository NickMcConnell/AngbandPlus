/* File: load.c */

/* jk - all attempts at converting older savefiles removed - sorry */
/* Purpose: support for loading savefiles -BEN- */

#include "angband.h"

/*
 * This file is responsible for loading savefiles
 *
 * Note that this file should not use the random number generator, the
 * object flavors, the visual attr/char mappings, or anything else which
 * is initialized *after* or *during* the "load character" function.
 */

/*
 * This function determines if the version of the savefile
 * currently being read is older than version "x.y.z".
 */
static bool older_than(byte x, byte y, byte z)
{
   /* Much older, or much more recent */
   if (sf_major < x) return (TRUE);
   if (sf_major > x) return (FALSE);

   /* Distinctly older, or distinctly more recent */
   if (sf_minor < y) return (TRUE);
   if (sf_minor > y) return (FALSE);

   /* Barely older, or barely more recent */
   if (sf_patch < z) return (TRUE);
   if (sf_patch > z) return (FALSE);

   /* Identical versions */
   return (FALSE);
}

/*
 * The following functions are used to load the basic building blocks
 * of savefiles.  They also maintain the "checksum" info for 2.7.0+
 */

static byte sf_get(FILE *fff)
{
   byte c;

   c = getc(fff);
   return (c);
}

static void rd_byte(FILE *fff, byte *ip)
{
   (*ip) = sf_get(fff);
}

static void rd_u16b(FILE *fff, u16b *ip)
{
   (*ip) = sf_get(fff);
   (*ip) |= ((u16b)(sf_get(fff)) << 8);
}

static void rd_s16b(FILE *fff, s16b *ip)
{
   rd_u16b(fff, (u16b*)ip);
}

static void rd_u32b(FILE *fff, u32b *ip)
{
   (*ip) = (u32b) sf_get(fff);
   (*ip) |= ((u32b)(sf_get(fff)) << 8);
   (*ip) |= ((u32b)(sf_get(fff)) << 16);
   (*ip) |= ((u32b)(sf_get(fff)) << 24);
}

static void rd_u64b(FILE *fff, u64b *ip)
{
   (*ip) = sf_get(fff);
   (*ip) |= ((u64b)(sf_get(fff)) << 8);
   (*ip) |= ((u64b)(sf_get(fff)) << 16);
   (*ip) |= ((u64b)(sf_get(fff)) << 24);
   (*ip) |= ((u64b)(sf_get(fff)) << 32);
   (*ip) |= ((u64b)(sf_get(fff)) << 40);
   (*ip) |= ((u64b)(sf_get(fff)) << 48);
   (*ip) |= ((u64b)(sf_get(fff)) << 56);
}

static void rd_bool(FILE *fff, bool *b)
{
   (*b)= (sf_get(fff)!=0);
}

static void rd_s32b(FILE *fff, s32b *ip)
{
   rd_u32b(fff, (u32b*)ip);
}

/*
 * Hack -- read a string
 */
static void rd_string(FILE *fff, char *str, s16b max)
{
   s16b i;

   /* Read the string */
   for (i = 0; TRUE; i++)
   {
      byte tmp8u;

      /* Read a byte */
      rd_byte(fff, &tmp8u);
      /* Collect string while legal */
      if (i < max) str[i] = tmp8u;
      if (i>=max) quit("load.c: rd_string: String too long");

      /* End of string */
      if (!tmp8u) break;
   }

   /* Terminate */
   str[max-1] = '\0';
}

s16b decompress_file(cptr name_in, cptr name_out)
{
   FILE *in_file, *out_file;
dlog(DEBUGSAVE,"load.c: decompress_file: about to decompress savefile %s into %s\n",
     name_in, name_out);

#if defined(MACINTOSH) && !defined(applec)
   /* Global -- "save file" */
   _ftype = 'LOAD';
#endif

   /* Open the savefile */
   in_file = my_fopen(name_in, "rb");
   /* Successful open */
   if (!in_file)
   {
      (void)remove(name_in);
      msg_format("load.c: decompress_file: Error opening %s for reading.",
                 name_in);
      return 1;
   }

#if defined(MACINTOSH) && !defined(applec)
   /* Global -- "save file" */
   _ftype = 'SAVE';
#endif

   out_file = my_fopen(name_out, "wb");
   if (!out_file)
   {
      (void)remove(name_out);
      msg_format("load.c: decompress_file: Error opening %s for writing.",
                 name_out);
      return -1;
   }
   if (my_fclose(in_file))
   {
      msg_format("load.c: decompress_file: Error closing %s before decompressing.",
                 name_in);
   }
   if (my_fclose(out_file))
   {
      msg_format("load.c: decompress_file: Error closing %s before decompressing.",
                 name_out);
   }
#ifdef COMPRESS_LZW
   (void)lzw_compress_file(1, name_in, name_out);
#endif
#ifdef COMPRESS_RLE
   (void)rle_compress_file(1, name_in, name_out);
#endif
   (void)remove(name_in); /* we've decompressed it - now remove it */
   prt("", 0, MESSAGE_ROW);
   msg_print(NULL);
   return(0);
}


/*
 * test if a file has been compressed (first three bytes TZ0)
 * if so, decompresses this file
 */
static FILE *test_decompress_file(cptr name)
{
   FILE *fff;
   char tempname[1024];
   byte tmp1, tmp2, tmp3;
   bool was_decompressed = FALSE;

#if defined(MACINTOSH) && !defined(applec)
   /* Global -- "load file" */
   _ftype = 'LOAD';
#endif

   fff = my_fopen(name, "rb");

   /* Unsuccessful open */
   if (!fff) return (NULL);
   rd_byte(fff, &tmp1);
   rd_byte(fff, &tmp2);
   rd_byte(fff, &tmp3);
   if ( ((char)tmp1 == 'Z') &&
        ((char)tmp2 == '6') &&
        ((char)tmp3 == '4'))
   {
#ifdef ALLOW_COMPRESSION
      strcpy(tempname, savefile);
      strcat(tempname, ".cmp");
      (void)my_fclose(fff);
dlog(DEBUGCMPRS,"load.c: test_decompress_file: decompressing %s into %s\n",
               name, tempname);
      decompress_file(name, tempname);
      /* fd_move doesn't work on DOS if the original file */
      /* is still there... */
dlog(DEBUGCMPRS,"load.c: test_decompress_file: killing %s\n",
     name);
      fd_kill(name);
dlog(DEBUGCMPRS,"load.c: test_decompress_file: moving %s into %s\n",
               tempname, name);
      fd_move(tempname, name);
      fff = my_fopen(name, "rb");
dlog(DEBUGCMPRS,"load.c: test_decompress_file: length %s %ld bytes\n",
               name, my_flength(fff));
      rd_byte(fff, &tmp1);
      rd_byte(fff, &tmp2);
      rd_byte(fff, &tmp3);
      was_decompressed = TRUE;
   }
   if ( ((char)tmp1 != 'A') ||
        ((char)tmp2 != '6') ||
        ((char)tmp3 != '4'))
   {
dlog(DEBUGALWAYS, "load.c: test_decompress_file: unknown file signature in %s (%s): 0x%02x 0x%02x 0x%02x\n",
                  name, was_decompressed?"after decompression":"file was not compressed",
                  tmp1, tmp2, tmp3);
       (void)my_fclose(fff);
       return(NULL);
   }

#else
      (void)my_fclose(fff);
      quit("load.c: test_decompress_file: this Angband/64 executable was compiled without support for compressed savefiles!");
   }
#endif
   return (fff);
}


s16b rd_spell_set(FILE *fff)
{
   s16b i;
   s16b set = s_pop();
dlog((DEBUGSAVE|DEBUGITEMS|DEBUGEXTRA),"load.c: rd_spell_set: using set %d\n", set);
   if (!set)
      quit("Error: too many spell_sets requested while reading savefile");

   for (i=0;i<(MAX_SPELLS_PER_ITEM/16);i++)
   {
      rd_u16b(fff, &s_list[set].spells[i]);
dlog((DEBUGSAVE|DEBUGITEMS),"load.c: rd_spell_set: spells[%d] = %016x\n",
              i, s_list[set].spells[i]);
   }
   /* we now use this set */
   s_list[set].inuse = TRUE;
   return (set);
}

/* 
 * Read an "item log" record
 */
static void rd_item_log(FILE *fff, item_log_type *l_ptr)
{
   rd_u32b(fff, &l_ptr->where);
   rd_u32b(fff, &l_ptr->found_when);
   rd_u16b(fff, &l_ptr->whose);
   rd_s16b(fff, &l_ptr->mlevel);
   rd_s16b(fff, &l_ptr->slevel);
dlog(DEBUGSAVE2,"load.c: rd_item_log: where %08lx when %ld whose %d mlevel %d slevel %d\n",
                l_ptr->where, l_ptr->found_when, l_ptr->whose, l_ptr->mlevel, l_ptr->slevel); 
}


/*
 * Read an item
 *
 */
static void rd_item(FILE *fff,object_type *i_ptr)
{
   char note[128]="";

   /* Kind */
   rd_s16b(fff, &i_ptr->k_idx);
   /* Location */
/* jk - these were bytes */
   rd_s16b(fff, &i_ptr->ix);
   rd_s16b(fff, &i_ptr->iy);
   rd_s16b(fff, &i_ptr->iz);

   /* Type/Subtype */
   rd_byte(fff, &i_ptr->tval);
   rd_s16b(fff, &i_ptr->sval);
   rd_s32b(fff, &i_ptr->p1val);
   rd_s16b(fff, &i_ptr->p2val);

   rd_byte(fff, &i_ptr->discount);
   rd_byte(fff, &i_ptr->number);
   rd_s16b(fff, &i_ptr->weight);

   rd_byte(fff, &i_ptr->name1);
   rd_byte(fff, &i_ptr->name2);
   rd_s16b(fff, &i_ptr->timeout);

   rd_s16b(fff, &i_ptr->to_h);
   rd_s16b(fff, &i_ptr->to_d);
   rd_s16b(fff, &i_ptr->to_a);

   rd_s16b(fff, &i_ptr->ac);
   rd_byte(fff, &i_ptr->dd);
   rd_byte(fff, &i_ptr->ds);

   rd_u16b(fff, &i_ptr->ident);
   rd_byte(fff, &i_ptr->marked);

   /* Special powers */
   rd_byte(fff, &i_ptr->xtra1);
   rd_s32b(fff, &i_ptr->xtra2);

   rd_s16b(fff, &i_ptr->spell_set);
   if (i_ptr->spell_set)
   {
dlog(DEBUGITEMS|DEBUGEXTRA,"load.c: rd_item: spell_set requested\n");
      i_ptr->spell_set = rd_spell_set(fff);
dlog(DEBUGITEMS,"load.c: rd_item: spell_set filled %d\n", i_ptr->spell_set);
   }
   rd_item_log(fff, &i_ptr->log);

   /* Inscription */
   rd_string(fff, note, 128);

   /* Save the inscription */
   if (note[0])
   {
      i_ptr->note = quark_add(note);
   }
   else
   {
      i_ptr->note = 0;
   }
dlog(DEBUGSAVE2,"load.c: rd_item: k_idx %d @ %d,%d,%d tv %d sv %d p1v %ld p2v %d disc %d num %d wgt %d\n",
     i_ptr->k_idx,i_ptr->ix,i_ptr->iy,i_ptr->iz,i_ptr->tval,i_ptr->sval,i_ptr->p1val,i_ptr->p2val,
     i_ptr->discount,i_ptr->number,i_ptr->weight);
dlog(DEBUGSAVE2,"load.c: rd_item: n1 %d n2 %d tim %d toh %d tod %d toa %d ac %d dd %d ds %d\n",
     i_ptr->name1,i_ptr->name2,i_ptr->timeout,i_ptr->to_h,i_ptr->to_d,i_ptr->to_a,
     i_ptr->ac,i_ptr->dd,i_ptr->ds);
dlog(DEBUGSAVE2,"load.c: rd_item: ident %d mark %d xtra1 %d xtra2 %ld f_where %ld f_whose %d f_lev %d/%d\n",
     i_ptr->ident, i_ptr->marked, i_ptr->xtra1, i_ptr->xtra2);
}

/*
 * Read a monster
 */
static void rd_monster(FILE *fff, monster_type *m_ptr)
{
   /* Hack -- wipe */
   WIPE(m_ptr, monster_type);

   /* Read the monster race */
   rd_s16b(fff, &m_ptr->r_idx);

   /* Read the other information */
/* jk - was byte */
   rd_s16b(fff, &m_ptr->fx);
   rd_s16b(fff, &m_ptr->fy);
   rd_s16b(fff, &m_ptr->fz);

   rd_s16b(fff, &m_ptr->hp);
   rd_s16b(fff, &m_ptr->maxhp);
   rd_s16b(fff, &m_ptr->csleep);
   rd_byte(fff, &m_ptr->mspeed);
   rd_byte(fff, &m_ptr->energy);

   rd_s16b(fff, &m_ptr->confused);             /* Timed -- Confusion           */
   rd_s16b(fff, &m_ptr->afraid);               /* Timed -- Fear                */
/* jk */
   rd_s16b(fff, &m_ptr->escaping);             /* Timed -- Escaping            */
   rd_byte(fff, &m_ptr->oldspeed);             /* old speed before escaping    */
   rd_s16b(fff, &m_ptr->stun);                 /* Timed -- Stun                */
   rd_u16b(fff, &m_ptr->attacked);
   rd_byte(fff, &m_ptr->breed_counter);
   rd_byte(fff, &m_ptr->last_hit);

dlog(DEBUGSAVE2,"load.c: rd_monster r_idx %d @ %d,%d,%d hp %d maxhp %d cslp %d mspd %d\n",
     m_ptr->r_idx, m_ptr->fx, m_ptr->fy, m_ptr->fz, m_ptr->hp, m_ptr->maxhp, m_ptr->csleep,
     m_ptr->mspeed);
dlog(DEBUGSAVE2,"load.c: rd_monster energy %d confused %d afraid %d stun %d\n",
     m_ptr->energy, m_ptr->confused, m_ptr->afraid, m_ptr->stun);
}

void test_monster_inventory(FILE *fff, s16b m_idx)
{
   byte number;
   s16b j,is_idx;

   rd_byte(fff, &number);
dlog(DEBUGSAVE,"load.c: test_monster_inventory: reading %d items @ %ld\n", number, ftell(fff)-1);
   if (number==0)
   {
      mn_list[m_idx].has_drop = FALSE;
      return;
   }

   mn_list[m_idx].has_drop = TRUE;
   is_idx = is_pop();

   if (is_idx==-1)
      quit("load.c: test_monster_inventory: no room in is_list");
   is_list[is_idx].x = -1;
   is_list[is_idx].y = m_idx;
   is_list[is_idx].z = 0;

dlog(DEBUGSAVE,"load.c: test_monster_inventory: reading %d items\n", number);

   for (j=0;j<number;j++)
   {
      s16b i_idx = i_pop();
      if (i_idx)
      {
         object_type *i_ptr = &i_list[i_idx];
dlog(DEBUGSAVE2,"load.c: test_monster_inventory: read item %d of %d at i_idx %d\n",
                j, number, i_idx);
         rd_item(fff, i_ptr);
         is_list[is_idx].index[j] = i_idx;
         i_ptr->ix = -1;
         i_ptr->iy = m_idx;
         i_ptr->iz = 0;
      }
      else
      {
         quit("load.c: reading monster inventory, i_pop failed");
      }
   }
}

/*
 * this function reads a single flavor from disk
 */

static void rd_flavors(FILE *fff)
{
   s16b i;
   for (i=0; i<MAX_FLAVORS; i++)
   {
      rd_string(fff, flavor[i].name, MAX_FLAVOR_LENGTH);
      rd_byte(fff, &flavor[i].color);
   }
}
/*
 * Read a "trap item" record
 */
static void rd_trap_item(FILE *fff, trap_item_type *tr_ptr)
{
   s16b i;

   rd_bool(fff, &tr_ptr->inuse);
   for (i=0; i < MAX_TRAPS_IN_SET; i++)
   {
      rd_byte(fff, &tr_ptr->type[i]);
      rd_bool(fff, &tr_ptr->found[i]);
   }
   rd_s16b(fff, &tr_ptr->tx);
   rd_s16b(fff, &tr_ptr->ty);
   rd_s16b(fff, &tr_ptr->tz);

dlog((DEBUGSAVE|DEBUGTRAPS),"load.c: rd_trap_item inuse %d @ %d,%d,%d\n",
               tr_ptr->inuse, tr_ptr->tx,tr_ptr->ty, tr_ptr->tz);
}

/*
 * Read the monster lore
 */
static void rd_lore(FILE *fff, s16b r_idx)
{
   monster_race *r_ptr = &r_info[r_idx];

   /* Count sights/deaths/kills */
   rd_s16b(fff, &r_ptr->r_sights);
   rd_s16b(fff, &r_ptr->r_deaths);
   rd_s16b(fff, &r_ptr->r_pkills);
   rd_s16b(fff, &r_ptr->r_tkills);
   rd_u32b(fff, &r_ptr->first_kill);

   /* Count wakes and ignores */
   rd_byte(fff, &r_ptr->r_wake);
   rd_byte(fff, &r_ptr->r_ignore);

   /* Extra stuff */
   rd_byte(fff, &r_ptr->r_xtra1);
   rd_byte(fff, &r_ptr->r_xtra2);

   /* Count drops */
   rd_byte(fff, &r_ptr->r_drop_gold);
   rd_byte(fff, &r_ptr->r_drop_item);

   /* Count spells */
   rd_byte(fff, &r_ptr->r_cast_inate);
   rd_byte(fff, &r_ptr->r_cast_spell);

   /* Count blows of each type */
   rd_byte(fff, &r_ptr->r_blows[0]);
   rd_byte(fff, &r_ptr->r_blows[1]);
   rd_byte(fff, &r_ptr->r_blows[2]);
   rd_byte(fff, &r_ptr->r_blows[3]);

   /* Memorize flags */
   rd_u64b(fff, &r_ptr->r_flags1);
   rd_u64b(fff, &r_ptr->r_flags2);
   rd_u64b(fff, &r_ptr->r_flags3);
   rd_u64b(fff, &r_ptr->r_flags4);
   rd_u64b(fff, &r_ptr->r_flags5);
   rd_u64b(fff, &r_ptr->r_flags6);

   /* Read the "Racial" monster limit per level */
   rd_byte(fff, &r_ptr->max_num);

   /* Repair the lore flags */
   r_ptr->r_flags1 &= r_ptr->flags1;
   r_ptr->r_flags2 &= r_ptr->flags2;
   r_ptr->r_flags3 &= r_ptr->flags3;
   r_ptr->r_flags4 &= r_ptr->flags4;
   r_ptr->r_flags5 &= r_ptr->flags5;
   r_ptr->r_flags6 &= r_ptr->flags6;

dlog(DEBUGSAVE2,"load.c: rd_lore sig %d dt %d pk %d tk %d wak %d ign %d xt1 %d xt2 %d drg %d dri %d\n",
     r_ptr->r_sights,r_ptr->r_deaths,r_ptr->r_pkills,r_ptr->r_tkills,r_ptr->r_wake,
     r_ptr->r_ignore,r_ptr->r_xtra1,r_ptr->r_xtra2,r_ptr->r_drop_gold,
     r_ptr->r_drop_item);
dlog(DEBUGSAVE2,"load.c: rd_lore ina %d spl %d bl1 %d bl2 %d bl3 %d bl4 %d max %d\n",
     r_ptr->r_cast_inate,r_ptr->r_cast_spell,r_ptr->r_blows[0],r_ptr->r_blows[1],
     r_ptr->r_blows[2],r_ptr->r_blows[3],r_ptr->max_num);
dlog(DEBUGSAVE2,"load.c: rd_lore f1 %16Lx  f2 %16Lx f3 %16Lx f4 %16Lx f5 %16Lx f6 %16Lx\n",
     r_ptr->r_flags1,r_ptr->r_flags2,r_ptr->r_flags3,r_ptr->r_flags4,
     r_ptr->r_flags5,r_ptr->r_flags6);
}

/*
 * Read a store
 */
static errr rd_store(FILE *fff, s16b n)
{
   store_type *st_ptr = &store[n];

   s16b j;

   byte num;

   /* Nothing in stock */
   st_ptr->stock_num = 0;

   /* Clear any old items */
   for (j = 0; j < STORE_INVEN_MAX; j++)
   {
      invwipe(&st_ptr->stock[j]);
   }

   /* Read the basic info */
   rd_u32b(fff, &st_ptr->store_open);
   rd_s16b(fff, &st_ptr->insult_cur);
   rd_byte(fff, &st_ptr->owner);
   rd_byte(fff, &st_ptr->store_type);
   rd_s16b(fff, &st_ptr->sx);
   rd_s16b(fff, &st_ptr->sy);
   rd_byte(fff, &num);
   if (num>STORE_INVEN_MAX)
   {
      msg_format("store %d stocks %d items, max %d", n, num, STORE_INVEN_MAX);
      return (1);
   }

   rd_s16b(fff, &st_ptr->good_buy);
   rd_s16b(fff, &st_ptr->bad_buy);
dlog(DEBUGSAVE,"load.c: rd_store: open %ld ins %d own %d num %d good %d bad %d\n",
   st_ptr->store_open,st_ptr->insult_cur,st_ptr->owner,st_ptr->stock_num,
   st_ptr->good_buy,st_ptr->bad_buy);

dlog(DEBUGSAVE,"load.c: rd_store reading %d items\n", num);

   /* Read the items */
   for (j = 0; j < num; j++)
   {
dlog(DEBUGSAVE2,"load.c: rd_store reading item %d of %d\n", j, num);

      /* Read the item */
      rd_item(fff, &st_ptr->stock[j]);
      rd_item_log(fff, &st_ptr->stock[j].log);
   }
   st_ptr->stock_num = num;
   /* Success */
   return (0);
}

/*
 * Read options
 *
 */
static void rd_options(FILE *fff)
{
   s16b i, n, os, ob;

   byte b;

   u32b c;

   u64b opt[5];

   u32b flag[8];
   u32b mask[8];

   /*** Normal options ***/

   /* Read the options */
   for (i=0; i<5; i++)
   {
      rd_u64b(fff, &opt[i]);
dlog(DEBUGSAVE2,"load.c: rd_options: loading options %d = %016Lx\n", i, opt[i]);
   }

   /*** Special options ***/

   /* Read "delay_spd" */
   rd_byte(fff, &b);
   delay_spd = b;

   /* Read "hitpoint_warn" */
   rd_byte(fff, &b);
   hitpoint_warn = b;


   /*** Cheating options ***/

   rd_u32b(fff, &c);
dlog(DEBUGSAVE2,"load.c: rd_options: cheating options %04x\n", c);

   if (c & 0x0002) wizard = TRUE;

   cheat_spell_info     = (c & 0x00000080L) ? TRUE : FALSE;
   cheat_peek           = (c & 0x00000100L) ? TRUE : FALSE;
   cheat_hear           = (c & 0x00000200L) ? TRUE : FALSE;
   cheat_room           = (c & 0x00000400L) ? TRUE : FALSE;
   cheat_xtra           = (c & 0x00000800L) ? TRUE : FALSE;
   cheat_know           = (c & 0x00001000L) ? TRUE : FALSE;
   cheat_live           = (c & 0x00002000L) ? TRUE : FALSE;
   cheat_hitpoints      = (c & 0x00004000L) ? TRUE : FALSE;
   cheat_mode           = (c & 0x00008000L) ? TRUE : FALSE;
   cheat_numeric_skills = (c & 0x00010000L) ? TRUE : FALSE;


   /*** Normal Options ***/

   /* we assign the options in options[] an option-set (os) and */
   /* an option-bit (ob) on the file, starting at set 0 bit 0.  */
   os = 0;
   ob = -1;
   /* Analyze the options, but not the cheating (page -1) options! */
   for (i = 0; options[i].page>0; i++)
   {
      ob++;
      if (ob == 64)
      {
         os++;
         ob = 0;
      }

       /* Extract a variable setting, if possible */
       (*options[i].variable) = (opt[os] & (1LL << ob)) ? TRUE : FALSE;
dlog(DEBUGSAVE2,"load.c: rd_options: option set %d bit %d text %s res %d\n",
                os, ob, options[i].descr, *options[i].variable);
   }

   /*** Window Options ***/

   for (i = 0; i < 8; i++)
   {
      rd_u32b(fff, &flag[i]);
      rd_u32b(fff, &mask[i]);
dlog(DEBUGSAVE2,"load.c: rd_options: flag[%d] val %08lx mask %08lx\n", i, flag[i], mask[i]);
   }

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
 * Read/Write the "extra" information
 */

static errr rd_extra(FILE *fff)
{
   s16b i;
   byte tmp8u;

   rd_u32b(fff, &debuglevel);
   rd_string(fff, player_name, 32);
   rd_string(fff, died_from, 80);
   for (i = 0; i < 4; i++)
   {
      rd_string(fff, history[i], 70);
   }
   for (i = 0; i < 4; i++)
   {
      rd_string(fff, teacher[i], 80);
   }

   /* Class/Race/Gender/Spells */
   rd_byte(fff, &p_ptr->prace);
   rd_s16b(fff, &p_ptr->pclass);
   rd_byte(fff, &p_ptr->psex);

   /* Special Race/Class info */
   rd_byte(fff, &p_ptr->hitdie);
   rd_s16b(fff, &p_ptr->expfact);

   rd_bool(fff, &p_ptr->teach_birth);
   if (p_ptr->teach_birth)
   {
      rd_s16b(fff, &p_ptr->teach_dis);
      rd_s16b(fff, &p_ptr->teach_dev);
      rd_s16b(fff, &p_ptr->teach_sav);
      rd_s16b(fff, &p_ptr->teach_stl);
      rd_s16b(fff, &p_ptr->teach_srh);
      rd_s16b(fff, &p_ptr->teach_pcp);
      rd_s16b(fff, &p_ptr->teach_thn);
      rd_s16b(fff, &p_ptr->teach_thb);
      rd_s16b(fff, &p_ptr->teach_tht);
      rd_s16b(fff, &p_ptr->teach_exp);
      for (i=0; i < 6; i++) rd_s16b(fff, &p_ptr->teach_stat[i]);
   }


   /* Age/Height/Weight */
   rd_s16b(fff, &p_ptr->age);
   rd_s16b(fff, &p_ptr->ht);
   rd_s16b(fff, &p_ptr->wt);

/* jk */
   rd_bool(fff, &p_ptr->twohands);
   rd_byte(fff, &p_ptr->tactic);
   rd_byte(fff, &p_ptr->movement);

   /* Read the stat info */
   for (i=0; i < 6; i++)
   {
      rd_s16b(fff, &p_ptr->stat_max[i]);
      rd_s16b(fff, &p_ptr->stat_cur[i]);
      rd_s16b(fff, &p_ptr->stat_cnt[i]);
      rd_s16b(fff, &p_ptr->stat_los[i]);
   }

   rd_u32b(fff, &p_ptr->au);

   rd_s32b(fff, &p_ptr->max_exp);
   rd_s32b(fff, &p_ptr->exp);
   rd_u16b(fff, &p_ptr->exp_frac);

   rd_s16b(fff, &p_ptr->lev);

   rd_s16b(fff, &p_ptr->mhp);
   rd_s16b(fff, &p_ptr->chp);
   rd_u16b(fff, &p_ptr->chp_frac);

   rd_s16b(fff, &p_ptr->msp);
   rd_s16b(fff, &p_ptr->csp);
   rd_u16b(fff, &p_ptr->csp_frac);

   rd_s16b(fff, &p_ptr->max_plv);
   rd_s16b(fff, &p_ptr->max_dlv);

   /* More info */
   rd_s16b(fff, &p_ptr->sc);

   /* Read the flags */
   rd_s16b(fff, &p_ptr->blind);
   rd_s16b(fff, &p_ptr->paralyzed);
   rd_s16b(fff, &p_ptr->confused);
   rd_s32b(fff, &p_ptr->food);
   rd_s16b(fff, &p_ptr->energy);
   rd_s16b(fff, &p_ptr->fast);
   rd_s16b(fff, &p_ptr->slow);
   rd_s16b(fff, &p_ptr->afraid);
   rd_s16b(fff, &p_ptr->cut);
   rd_s16b(fff, &p_ptr->stun);
   rd_byte(fff, &p_ptr->arena_state);
/* jk */
   rd_bool(fff, &save_levels);
   rd_s16b(fff, &p_ptr->sliding);
   rd_bool(fff, &p_ptr->sliding_now);
/* enormous hack: sliding_now shouldn't be TRUE, that means we've saved */
/* in the middle of a slide! - when we can't use keys to direct things...*/
   p_ptr->sliding_now = FALSE;
   rd_s16b(fff, &p_ptr->fire);
   rd_s16b(fff, &p_ptr->cold);
   rd_s16b(fff, &p_ptr->acid);
   rd_s16b(fff, &p_ptr->elec);
   rd_s16b(fff, &p_ptr->reading);

   rd_s16b(fff, &p_ptr->poisoned);
   rd_s16b(fff, &p_ptr->image);
   rd_s16b(fff, &p_ptr->protevil);
   rd_u16b(fff, &p_ptr->invuln);
   rd_s16b(fff, &p_ptr->hero);
   rd_s16b(fff, &p_ptr->shero);
   rd_s16b(fff, &p_ptr->shield);
   rd_s16b(fff, &p_ptr->blessed);
   rd_s16b(fff, &p_ptr->tim_invis);
   rd_s16b(fff, &p_ptr->word_recall);
   rd_s16b(fff, &p_ptr->see_infra);
   rd_s16b(fff, &p_ptr->tim_infra);
   rd_s16b(fff, &p_ptr->oppose_fire);
   rd_s16b(fff, &p_ptr->oppose_cold);
   rd_s16b(fff, &p_ptr->oppose_acid);
   rd_s16b(fff, &p_ptr->oppose_elec);
   rd_s16b(fff, &p_ptr->oppose_pois);

   rd_byte(fff, &p_ptr->confusing);
   rd_byte(fff, &p_ptr->searching);
   rd_byte(fff, &p_ptr->reflecting);

   /* Special stuff */
   rd_u16b(fff, &panic_save);
   rd_u16b(fff, &total_winner);
   rd_u16b(fff, &noscore);

   /* Read "death" */
   rd_byte(fff, &tmp8u);
   death = tmp8u;

   /* Read "feeling" */
   rd_byte(fff, &tmp8u);
   feeling = tmp8u;

   /* Turn of last "feeling" */
   rd_s32b(fff, &old_turn);

   /* Current turn */
   rd_s32b(fff, &turn);

   /* Home inventory */
dlog(DEBUGSAVE,"load.c: rd_extra: reading home\n");
   if (rd_store(fff, 0)) return (1);
dlog(DEBUGSAVE,"load.c: rd_extra: read home, %d items\n", store[0].stock_num);
   return (0);
}

/*
 * Read the player inventory
 *
 * Note that the inventory changed in Angband 2.7.4.  Two extra
 * pack slots were added and the equipment was rearranged.  Note
 * that these two features combine when parsing old save-files, in
 * which items from the old "aux" slot are "carried", perhaps into
 * one of the two new "inventory" slots.
 *
 * Note that the inventory is "re-sorted" later by "dungeon()".
 */
static errr rd_inventory(FILE *fff)
{
   s16b i;

   object_type *i_ptr;

   /* No weight */
   p_ptr->total_weight = 0;

   /* No items */
   inven_cnt = 0;
   equip_cnt = 0;

   /* Read until done */
   for (i=0; i<INVEN_TOTAL; i++)
   {
dlog(DEBUGSAVE,"load.c: rd_inventory: reading item %d of %d\n", i, INVEN_TOTAL);
      rd_item(fff, &inventory[i]);
      i_ptr = &inventory[i];
      p_ptr->total_weight += (i_ptr->number * i_ptr->weight);
      if (i_ptr->k_idx)
      {
         if (i >= INVEN_WIELD)
         {
            equip_cnt++;
            p_ptr->equip_weight += (i_ptr->number * i_ptr->weight);
         }
         else
         {
            inven_cnt++;
         }
      }
   }
   /* Success */
   return(0);
}

/*
 * Read the dungeon (new method)
 *
 */
static errr rd_dungeon(FILE *fff)
{
   s16b i, x = 0, y = 0;
   byte tmp;
   u16b start,limit;
   cave_cell_type *c_ptr = NULL;
   s16b old_sublevel = -1;

   bool save_inkey_scan = inkey_scan;

   old_sublevel = sublevel;

   sublevel = 0;

   inkey_scan=TRUE;

   rd_u16b(fff, &num_repro);
   rd_u16b(fff, &panel_max_rows);
   rd_u16b(fff, &panel_max_cols);
dlog(DEBUGSAVE,"load.c: rd_dungeon: sublevel %d repro %d hgt %d wid %d p_ptr->wys %d cols %d\n",
               sublevel, num_repro, cur_hgt, cur_wid, panel_max_rows, panel_max_cols);

   /* Read in the actual "cave" data */

   /* Get the cave */
   for (sublevel=0; sublevel < MAX_SUB_LEVELS; sublevel++)
   {
      rd_byte(fff, &tmp);
      if (tmp)
      {
         rd_u16b(fff, &cur_wid);
         rd_u16b(fff, &cur_hgt);
         dungeon.level_wid[sublevel] = cur_wid;
         dungeon.level_hgt[sublevel] = cur_hgt;
         rd_u16b(fff, &dungeon.level_name[sublevel]);
         rd_u16b(fff, &dungeon.level_depth[sublevel]);

         /* allocate some space and point to it. */
         if (sublevel>0)
         {
            allocate_sublevel(sublevel);
         }
         dungeon.level_used[sublevel] = TRUE;

         /* Get the level */
         for (y = 0; y < cur_hgt; y++)
         {
            for (x = 0; x < cur_wid; x++)
            {
               /* Get the cave */
               c_ptr = &dungeon.level[sublevel][y][x];

               rd_u32b(fff, &c_ptr->fdat);
               c_ptr->fdat &= ~(CAVE_OLD_LITE | CAVE_OLD_VIEW);
               rd_u16b(fff, &c_ptr->mtyp);
               rd_u16b(fff, &c_ptr->styp);
               rd_s32b(fff, &c_ptr->extra);
               rd_u16b(fff, &c_ptr->memory_mtyp);
               rd_u16b(fff, &c_ptr->memory_styp);

dlog(DEBUGSAVE2,"load.c: rd_dungeon: sublevel[%d] cave %d,%d fdat %08lx mtyp %d styp %d extra %d mem mt %d st %d\n",
                sublevel, x, y, c_ptr->fdat, c_ptr->mtyp, c_ptr->styp, c_ptr->extra,
                c_ptr->memory_mtyp, c_ptr->memory_styp);
            }
         }
      }
      else
      {
         dungeon.level_used[sublevel] = FALSE;
      }
   }

   /* Read the monster count */
   rd_u16b(fff, &limit);

   /* Hack -- verify */
   if (limit >= MAX_M_IDX + MAX_GHOSTS)
   {
      msg_format("Too many (%d) monster entries!", limit);
      return (161);
   }
dlog(DEBUGSAVE,"load.c: rd_dungeon: reading %d monsters\n", limit);

   /* Read the monsters, starting at 1 */
   for (i = 1; i < limit; i++)
   {
      s16b m_idx;

      monster_type *m_ptr;
      monster_race *r_ptr;

      monster_type forge;

      /* Forge */
      m_ptr = &forge;

      /* Read the monster */
dlog(DEBUGSAVE2,"load.c: rd_dungeon: reading monster %d of %d @ %ld\n", i, limit, ftell(fff));
      rd_monster(fff, m_ptr);

      /* Access grid */
      if (m_ptr->fz && !dungeon.level_used[m_ptr->fz])
      {
         quit("load.c: rd_dungeon: monster exists on unknown sublevel?");
      }
      c_ptr = &dungeon.level[m_ptr->fz][m_ptr->fy][m_ptr->fx];

      /* if there is already a monster in that square */
      if (c_ptr->m_idx != 0)
      {
         byte num_items;
         object_type monster_inv;

         dlog(DEBUGALWAYS,"load.c: rd_dungeon: reading another monster @ %d,%d - %d (%s) exists there\n",
                          m_ptr->fx, m_ptr->fy, c_ptr->m_idx,
                          r_name + r_info[mn_list[c_ptr->m_idx].r_idx].name);
         /* skip any items belonging to that monster */
         rd_byte(fff, &num_items);
         dlog(DEBUGALWAYS,"load.c: rd_dungeon: discarding %d items\n", num_items);
         while (num_items > 0)
         {
            rd_item(fff, &monster_inv);
            num_items--;
         }
         continue;
      }

      /* Access race */
      r_ptr = &r_info[m_ptr->r_idx];

      /* Hack -- ignore "broken" monsters */
      if (m_ptr->r_idx <= 0)
      {
dlog(DEBUGSAVE,"load.c: rd_dungeon: r_idx %d @ cave %d,%d,%d: broken monster\n",
              m_ptr->r_idx, m_ptr->fx, m_ptr->fy, m_ptr->fz);
         continue;
      }

      /* Get a new record */
      m_idx = mn_pop();

      /* Oops */
      if (!m_idx)
      {
         msg_format("Too many (%d) monsters!", mn_max);
         return (162);
      }

      /* Acquire place */
      m_ptr = &mn_list[m_idx];

      /* Copy the item */
      (*m_ptr) = forge;

      /* Mark the location */
      c_ptr->m_idx = m_idx;

      /* Count XXX XXX XXX */
      r_ptr->cur_num++;

      test_monster_inventory(fff, i);
   }

   /* Read the item count */
   rd_u16b(fff, &limit);
dlog(DEBUGSAVE,"load.c: rd_dungeon: %d items on floor\n", limit);

   /* Hack -- verify */
   /* jk - why was this 512, not MAX_I_IDX? */
   if (limit >= MAX_I_IDX)
   {
      msg_format("Too many (%d max %d) object entries!", limit, MAX_I_IDX);
      return (151);
   }

dlog(DEBUGSAVE,"load.c: rd_dungeon: reading %d floor items\n", limit);

   /* Read the dungeon items */
   for (i = 1; i < limit; i++)
   {
      object_type forge;
dlog(DEBUGSAVE2,"load.c: rd_dungeon: reading floor item %d of %d\n", i, limit);

      /* strange things happen otherwise, you have been warned! */
      forge.spell_set = 0;
      invwipe(&forge);

      /* Read the item */
      rd_item(fff, &forge);
      sublevel = forge.iz;
      x = forge.ix;
      y = forge.iy;

      /* Access grid, if necessary on sublevel */
      if (sublevel && !dungeon.level_used[sublevel])
      {
         quit("load.c: rd_dungeon: object exists on unknown sublevel!\n");
      }

      /* Skip dead objects - there shouldn't be any, since i_list was */
      /* compacted before saving */
      if (!forge.k_idx) continue;

      /* Mark the location */
      if (floor_carry(&forge,x,y)==-1)
      {
         dlog(DEBUGALWAYS,"Error in load.c adding item %d of %d to floor @ %d,%d\n",i ,limit, x, y);
      }
   }

   /* Read the trap count */
   rd_u16b(fff, &limit);

   /* Hack -- verify */
   if (limit >= MAX_TR_IDX)
   {
      msg_format("Too many (%d) trap entries!", limit);
      return (162);
   }
dlog(DEBUGSAVE,"load.c: rd_dungeon: reading %d traps\n", limit);

   /* Read the traps */
   start = 1;
   for (i = start; i < limit; i++)
   {
      s16b tr_idx;
      cave_cell_type *c_ptr;
      trap_item_type *tr_ptr;

      trap_item_type forge;

      /* Forge */
      tr_ptr = &forge;

dlog(DEBUGSAVE,"load.c: rd_dungeon: reading trap_item %d of %d\n", i, t_max);
      /* Read the trap */
      rd_trap_item(fff, tr_ptr);
      tr_idx = t_pop();

      /* Oops */
      if (tr_idx==-1)
      {
          msg_format("Too many (%d) traps!", t_max);
          return (162);
      }

      /* Acquire place */
      tr_ptr = &t_list[tr_idx];
dlog(DEBUGSAVE,"load.c: rd_dungeon: place acquired\n");

      /* Copy the item */
      (*tr_ptr) = forge;
dlog(DEBUGSAVE,"load.c: rd_dungeon: copied, pointing to %d,%d\n", tr_ptr->tx, tr_ptr->ty);

      /* Access grid, if necessary on sublevel */
      sublevel = tr_ptr->tz;
      if (sublevel && !dungeon.level_used[sublevel])
      {
         quit("load.c: rd_dungeon: trap exists on unknown sublevel!\n");
      }

      c_ptr = &dungeon.level[sublevel][tr_ptr->ty][tr_ptr->tx];
      c_ptr->t_idx = tr_idx;
dlog(DEBUGSAVE,"load.c: rd_dungeon: end of loop\n");
   }
dlog(DEBUGSAVE,"load.c: rd_dungeon: step 2\n");


   for (sublevel = 0; sublevel < MAX_SUB_LEVELS; sublevel++)
   {
      if (!dungeon.level_used[sublevel]) continue;

      /* Hack -- clean up the dungeon */
      for (y = 0; y < dungeon.level_hgt[sublevel]; y++)
      {
         for (x = 0; x < dungeon.level_wid[sublevel]; x++)
         {
            cave_cell_type *c_ptr = &dungeon.level[sublevel][y][x];

            /* Hack -- convert nothing-ness into floors */
            if (c_ptr->mtyp == 0x00)
            {
              (void)set_grid_type(x, y, DUNG_FLOOR,
                                  DUNG_FLOOR_NORMAL, GRID_KEEP, 0);
            }
            if (test_grid_ptr(c_ptr,DUNG_FLOOR,DUNG_FLOOR_TRAP) &&
                (c_ptr->t_idx == 0))
            {
dlog(DEBUGSAVE,"load.c: hidden trap read in at %d,%d with t_idx 0, set to normal floor\n",x,y);
               (void)set_grid_type(x, y, DUNG_FLOOR,
                                   DUNG_FLOOR_NORMAL, GRID_KEEP, 0);
            }
            if ((c_ptr->mtyp == DUNG_TRAP) && (c_ptr->t_idx == 0))
            {
dlog(DEBUGSAVE,"load.c: DUNG_TRAP read in at %d,%d with t_idx 0, set to normal floor\n",x,y);
               (void)set_grid_type(x, y, DUNG_FLOOR,
                                   DUNG_FLOOR_NORMAL, GRID_KEEP, 0);
            }

         }
      }
   }

   /* we have read the dungeon */
   character_dungeon = TRUE;


   /* Success */
/* jk */
   inkey_scan=save_inkey_scan;
dlog(DEBUGSAVE,"load.c: rd_dungeon: step 3\n");

   /* restore the original sublevel */
   sublevel = old_sublevel;

   /* and set the wid, hgt of the current dungeon */
   cur_wid = dungeon.level_wid[sublevel];
   cur_hgt = dungeon.level_hgt[sublevel];

   return (0);
}

/*
 * Actually read a level
 */
errr rd_level(void)
{
   s16b        i, err = 1;
   char        temp[255];
   FILE       *fff = NULL;
dlog(DEBUGSAVE,"load.c: rd_level entering\n");

   sprintf(temp,"%s.LZZ",levelfile); /* L for Level file */

   make_extension(temp, p_ptr->mdepth);
dlog(DEBUGSAVE,"load.c: rd_level extension created\n");

   /* remember the last file we read in */
   strcpy(sf_lastfile_name, temp);

   /* Open the savefile */
dlog(DEBUGFLOW,"load.c: rd_level: starting with file %s\n", temp);
   fff=test_decompress_file(temp);

dlog(DEBUGSAVE,"load.c: rd_level file %s\n", temp);
   /* Successful open */
   if (!fff)
   {
      (void)remove(temp);
      err=1;
dlog(DEBUGALWAYS,"load.c: rd_level(): error opening %s\n",temp);
      msg_format("Error opening level-savefile '%s' for level %d, generating new dungeon!", temp, p_ptr->mdepth);
      return(err);
   }
   else
   {
      store_built = FALSE;
      rd_u16b(fff, &num_stores);
dlog(DEBUGSAVE,"load.c: rd_level: %d stores to read\n", num_stores);
      if (num_stores>0)
      {
         /* store number 0 is home, and it is not read here! */
         for (i = 1; i < num_stores; i++)
         {
dlog(DEBUGSAVE,"load.c: rd_level: reading store %d of %d\n", i, num_stores);
            if (rd_store(fff, i)) return (22);
         }
         store_built = TRUE;
      }

      /* Read the dungeon */
      rd_dungeon(fff);

      rd_s16b(fff, &px);
      rd_s16b(fff, &py);
dlog(DEBUGSAVE,"load.c: rd_level: player at %d,%d\n", px, py);

      /* Error in save */
      if (ferror(fff) || (fflush(fff) == EOF)) return FALSE;
      my_fclose(fff);

      /* Successful load */
      err = 0;
   }

   /* Failure */
   return(err);
}

static void rd_dungeon_info(FILE *fff)
{
   rd_s16b(fff, &p_ptr->mdepth);
   rd_s16b(fff, &p_ptr->sdepth);
dlog(DEBUGSAVE,"load.c: rd_dungeon_info: p_ptr->depth read in as %d/%d\n",
               p_ptr->mdepth, p_ptr->sdepth);
dlog(DEBUGSAVE,"load.c: rd_dungeon_info: MAX_LEVEL %d p_ptr->depth %d/%d\n",
               MAX_LEVEL, p_ptr->mdepth, p_ptr->sdepth);
   baselevel = p_ptr->mdepth;
   sublevel = p_ptr->sdepth;
}

static void rd_level_info(FILE *fff)
{
   s16b i;
   for (i=0;i<MAX_LEVEL;i++)
   {
     byte tmp;
     rd_byte(fff, &tmp);
     level_info[i].saved=(tmp==1);
     rd_u32b(fff, &level_info[i].first_visit);
     rd_u32b(fff, &level_info[i].last_visit);
dlog(DEBUGSAVE2,"load.c: rd_level_info: level %d of %d: saved %d last_visit %ld\n",
               i, MAX_LEVEL, tmp, level_info[i].last_visit);
   }
}

/*
 * this function reads one ghost in file fff
 * returning false if an error occurs
 */
static bool rd_ghost(FILE *fff, s16b r_idx)
{
   monster_race *r_ptr = NULL;
   s16b i, ghost_items, g_idx;
   byte tmpb;

   char temp[1024];

   r_ptr = &r_info[r_idx];
   g_idx = r_idx - r_number;

dlog((DEBUGGHOST | DEBUGSAVE),"load.c: rd_ghost: starting @ r_idx %d g_idx %d\n", r_idx, g_idx);

   rd_string(fff, temp, 1024);
dlog((DEBUGGHOST | DEBUGSAVE),"load.c: rd_ghost: name %s len %d\n", temp, strlen(temp));
   if (r_name_size + strlen(temp) > r_name_size_total)
      return(FALSE);

   strcpy(r_name + r_name_size, temp);
   r_ptr->name = r_name_size;
   r_name_size += strlen(temp) + 1;

   rd_string(fff, temp, 1024);
dlog((DEBUGGHOST | DEBUGSAVE),"load.c: rd_ghost: desc %s len %d\n", temp, strlen(temp));
   if (r_text_size + strlen(temp) > r_text_size_total)
      return(FALSE);

   strcpy(r_text + r_text_size, temp);
   r_ptr->text = r_text_size;
   r_text_size += strlen(temp) + 1;

   rd_byte(fff, &r_ptr->hdice);
   rd_byte(fff, &r_ptr->hside);
   rd_s16b(fff, &r_ptr->ac);
   rd_s16b(fff, &r_ptr->sleep);
   rd_byte(fff, &r_ptr->aaf);
   rd_byte(fff, &r_ptr->speed);
   rd_s32b(fff, &r_ptr->mexp);
dlog((DEBUGGHOST | DEBUGSAVE),"load.c: rd_ghost: hd %d hs %d ac %d sleep %d aaf %d spd %d mexp %d\n",
                r_ptr->hdice, r_ptr->hside, r_ptr->ac, r_ptr->sleep, r_ptr->aaf, r_ptr->speed,
                r_ptr->mexp);

   rd_byte(fff, &r_ptr->freq_inate);
   rd_byte(fff, &r_ptr->freq_spell);
dlog((DEBUGGHOST | DEBUGSAVE),"load.c: rd_ghost:ina %d spl %d\n", r_ptr->freq_inate, r_ptr->freq_spell);

   rd_u64b(fff, &r_ptr->flags1);
   rd_u64b(fff, &r_ptr->flags2);
   rd_u64b(fff, &r_ptr->flags3);
dlog((DEBUGGHOST | DEBUGSAVE),"load.c: rd_ghost: flags1 %16Lx flags2 %16Lx flags3 %16Lx\n",
                r_ptr->flags1, r_ptr->flags2, r_ptr->flags3);

   rd_u64b(fff, &r_ptr->flags4);
   rd_u64b(fff, &r_ptr->flags5);
   rd_u64b(fff, &r_ptr->flags6);
dlog((DEBUGGHOST | DEBUGSAVE),"load.c: rd_ghost: flags4 %16Lx flags5 %16Lx flags6 %16Lx\n",
                r_ptr->flags4, r_ptr->flags5, r_ptr->flags6);

   /* corpse properties */
   rd_u32b(fff, &r_ptr->corpse_gives_alw);
   rd_u32b(fff, &r_ptr->corpse_takes_alw);
   rd_u32b(fff, &r_ptr->corpse_gives_smt);
   rd_u32b(fff, &r_ptr->corpse_takes_smt);
dlog((DEBUGGHOST | DEBUGSAVE),"load.c: rd_ghost: corpse gva %08lx tka %08lx gvs %08lx tks %08lx\n",
                r_ptr->corpse_gives_alw, r_ptr->corpse_takes_alw,
                r_ptr->corpse_gives_smt, r_ptr->corpse_takes_smt);

   rd_s16b(fff, &r_ptr->corpse_chance);
   rd_s16b(fff, &r_ptr->corpse_nutrition);
   rd_s16b(fff, &r_ptr->corpse_weight);
   rd_s32b(fff, &r_ptr->corpse_spoiling);
   rd_s16b(fff, &r_ptr->corpse_chance_bones);
   rd_byte(fff, &r_ptr->corpse_chance_gives);
   rd_byte(fff, &r_ptr->corpse_chance_takes);

dlog((DEBUGGHOST | DEBUGSAVE),"load.c: rd_ghost: corpse chance %d nut %d wgt %d spoil %ld chb %d chg %d cht %d\n",
                r_ptr->corpse_chance, r_ptr->corpse_nutrition, r_ptr->corpse_weight,
                r_ptr->corpse_spoiling, r_ptr->corpse_chance_bones, r_ptr->corpse_chance_gives,
                r_ptr->corpse_chance_takes);

   /* blow information */
   for (i=0; i<4; i++)
   {
      rd_byte(fff, &r_ptr->blow[i].method);
      rd_byte(fff, &r_ptr->blow[i].effect);
      rd_byte(fff, &r_ptr->blow[i].d_dice);
      rd_byte(fff, &r_ptr->blow[i].d_side);
dlog((DEBUGGHOST | DEBUGSAVE),"load.c: rd_ghost: blow %d meth %d eff %d %dd%d\n",
                i, r_ptr->blow[i].method, r_ptr->blow[i].effect,
                r_ptr->blow[i].d_dice, r_ptr->blow[i].d_side);
   }

   rd_byte(fff, &r_ptr->level);
   rd_byte(fff, &r_ptr->rarity);

   rd_byte(fff, &r_ptr->d_attr);
   rd_byte(fff, &tmpb);
   r_ptr->d_char=(char)tmpb;

   rd_byte(fff, &r_ptr->x_attr);
   rd_byte(fff, &tmpb);
   r_ptr->x_char=(char)tmpb;
dlog((DEBUGGHOST | DEBUGSAVE),"load.c: rd_ghost: lev %d rar %d da %d dc %d xa %d da %d\n",
                r_ptr->level, r_ptr->rarity, r_ptr->d_attr, r_ptr->d_char,
                r_ptr->x_attr, r_ptr->x_char);

   rd_byte(fff, &r_ptr->max_num);
   rd_byte(fff, &r_ptr->cur_num);

   rd_byte(fff, &r_ptr->max_gen);
   rd_s16b(fff, &r_ptr->r_sights);
   rd_s16b(fff, &r_ptr->r_deaths);

   rd_s16b(fff, &r_ptr->r_pkills);
   rd_s16b(fff, &r_ptr->r_tkills);
dlog((DEBUGGHOST | DEBUGSAVE),"load.c: rd_ghost: mnm %d cnm %d gen %d rs %d rd %d r_pk %d r_tk %d\n",
                r_ptr->max_num, r_ptr->cur_num, r_ptr->max_gen, r_ptr->r_sights,
                r_ptr->r_deaths, r_ptr->r_pkills, r_ptr->r_tkills);

   rd_byte(fff, &r_ptr->r_wake);
   rd_byte(fff, &r_ptr->r_ignore);

   rd_byte(fff, &r_ptr->r_xtra1);
   rd_byte(fff, &r_ptr->r_xtra2);

   rd_byte(fff, &r_ptr->r_drop_gold);
   rd_byte(fff, &r_ptr->r_drop_item);

   rd_byte(fff, &r_ptr->r_cast_inate);
   rd_byte(fff, &r_ptr->r_cast_spell);
dlog((DEBUGGHOST | DEBUGSAVE),"load.c: rd_ghost: r_w %d r_i %d r_x1 %d r_x2 %d dg %d di %d rci %d rcs %d\n",
                r_ptr->r_wake, r_ptr->r_ignore, r_ptr->r_xtra1, r_ptr->r_xtra2,
                r_ptr->r_drop_gold, r_ptr->r_drop_item, r_ptr->r_cast_inate,
                r_ptr->r_cast_spell);

   for (i=0; i<4; i++)
   {
      rd_byte(fff, &r_ptr->r_blows[i]);
   }
dlog((DEBUGGHOST | DEBUGSAVE),"load.c: rd_ghost: rb0 %d rb1 %d rb2 %d rb3 %d\n",
                r_ptr->r_blows[0], r_ptr->r_blows[1], r_ptr->r_blows[2], r_ptr->r_blows[3]);

   rd_u64b(fff, &r_ptr->r_flags1);
   rd_u64b(fff, &r_ptr->r_flags2);
   rd_u64b(fff, &r_ptr->r_flags3);
dlog((DEBUGGHOST | DEBUGSAVE),"load.c: rd_ghost: rflags1 %16Lx rflags2 %16Lx rflags3 %16Lx\n",
                r_ptr->r_flags1, r_ptr->r_flags2, r_ptr->r_flags3);
   rd_u64b(fff, &r_ptr->r_flags4);
   rd_u64b(fff, &r_ptr->r_flags5);
   rd_u64b(fff, &r_ptr->r_flags6);
dlog((DEBUGGHOST | DEBUGSAVE),"load.c: rd_ghost: rflags4 %16Lx rflags5 %16Lx rflags6 %16Lx\n",
                r_ptr->r_flags4, r_ptr->r_flags5, r_ptr->r_flags6);

   rd_s16b(fff, &ghost_items);
dlog((DEBUGGHOST | DEBUGSAVE),"load.c: rd_ghost: reading %d items\n", ghost_items);
   if (ghost_items>0)
   {
      for (i=0; i<ghost_items; i++)
      {
         object_type forge;
dlog((DEBUGGHOST | DEBUGSAVE2),"load.c: rd_ghost: reading item %d of %d\n",
                              i, ghost_items);
         if (i >= ITEM_SET_SIZE)
         {
            dlog(DEBUGALWAYS,"load.c: rd_ghost: more ghost inventory found than possible? (%d)\n", i);
            continue;
         }
         /* strange things happen otherwise, you have been warned! */
         forge.spell_set = 0;
         invwipe(&forge);

         rd_item(fff, &forge);
         ghost_info[g_idx].inv[i]=forge;
      }
   }
   else
   {
dlog((DEBUGGHOST | DEBUGSAVE),"load.c: rd_ghost: no items\n", ghost_items);
   }

   return (TRUE);
}

/*
 * This function reads all the ghosts from a savefile
 */
static void rd_ghost_info(FILE *fff)
{
   s16b i;
   s16b ghost_num;

dlog((DEBUGGHOST | DEBUGSAVE),"load.c: rd_ghost_info: starting\n");
   rd_s16b(fff, &ghost_num);
   r_number_total = r_number + ghost_num;
dlog((DEBUGGHOST | DEBUGSAVE),"load.c: rd_ghost_info: reading %d ghosts, r_number_total now %d, r_number %d\n",
                ghost_num, r_number_total, r_number);
   for (i=r_number; i<r_number_total; i++)
   {
dlog((DEBUGGHOST | DEBUGSAVE),"load.c: rd_ghost_info: about to read ghost @ r_idx %d ghost_index %d\n",
                i, i-r_number);
      rd_ghost(fff, i);
      ghost_info[i-r_number].r_idx = i;
   }
dlog((DEBUGGHOST | DEBUGSAVE),"load.c: rd_ghost_info: ending\n");
}

/*
 * Actually read the savefile
 */
static errr rd_savefile_new_aux(FILE *fff)
{
   s16b i;

   byte tmp8u;
   u16b tmp16u;
   u32b tmp32u;

   /* Strip the version bytes */
   rd_byte(fff, &sf_major);
   rd_byte(fff, &sf_minor);
   rd_byte(fff, &sf_patch);
   rd_byte(fff, &tmp8u);

   /* Mention the savefile version */
   prt(format("Loading a %d.%d.%d savefile...",
               sf_major, sf_minor, sf_patch), 0, MESSAGE_ROW);

   /* Hack -- Warn about "obsolete" versions */
   if (older_than(2,7,10))
   {
      quit("Obsolete save file.");
   }

   /* Operating system info */
   rd_u32b(fff, &sf_xtra);

   /* Time of savefile creation */
   rd_u32b(fff, &sf_when);

   /* Number of resurrections */
   rd_u16b(fff, &sf_lives);

   /* Number of times played */
   rd_u16b(fff, &sf_saves);

   /* Later use (always zero) */
   rd_u32b(fff, &tmp32u);

   /* Later use (always zero) */
   rd_u32b(fff, &tmp32u);

   /* Then the options */
dlog(DEBUGSAVE,"rd_savefile_new_aux: reading options\n");
   rd_options(fff);

dlog(DEBUGSAVE,"rd_savefile_new_aux: reading flavors\n");
   rd_flavors(fff);

dlog(DEBUGSAVE,"rd_savefile_new_aux: reading dungeon_info\n");
   rd_dungeon_info(fff);

dlog(DEBUGSAVE,"rd_savefile_new_aux: reading level_info\n");
   rd_level_info(fff);

   rd_ghost_info(fff);

   /* Monster Memory */
   rd_u16b(fff, &tmp16u);

   /* Incompatible save files */
   if (tmp16u > r_number_total)
   {
       msg_format("Too many (%u) monster races!", tmp16u);
       return (21);
   }
dlog(DEBUGSAVE,"load.c: rd_savefile_new_aux: reading %d lore records\n", tmp16u);

   /* Read the available records */
   for (i = 0; i < tmp16u; i++)
   {
      monster_race *r_ptr;

      /* Read the lore */
dlog(DEBUGSAVE2,"load.c: rd_savefile_new_aux: reading lore %d of %d\n", i, tmp16u);
               rd_lore(fff, i);

      /* Access that monster */
      r_ptr = &r_info[i];
   }

   rd_u16b(fff, &tmp16u);
   if (tmp16u > t_number)
   {
      msg_format("Too many (%u) trap kinds!", tmp16u);
      return (35);
   }

dlog(DEBUGSAVE,"load.c: rd_savefile_new_aux: reading %d trap records\n", tmp16u);
   for (i=0; i< tmp16u; i++)
   {
      trap_type *t_ptr = &t_info[i];
      rd_s16b(fff, &t_ptr->known);
      rd_bool(fff, &t_ptr->ident);
      rd_u32b(fff, &t_ptr->flags);
dlog(DEBUGSAVE2,"load.c: rd_savefile_new_aux: reading trap %d of %d known %d ident %d flags %08lx\n",
               i, tmp16u, t_ptr->known, t_ptr->ident);
   }

   /* Object Memory */
   rd_u16b(fff, &tmp16u);

   /* Incompatible save files */
   if (tmp16u > k_number)
   {
      msg_format("Too many (%u) object kinds!", tmp16u);
      return (22);
   }
dlog((DEBUGSAVE|DEBUGITEMS),"load.c: rd_savefile_new_aux: reading %d object memory records\n", tmp16u);

   /* Read the object memory */
   for (i = 0; i < tmp16u; i++)
   {
      byte tmp8u;

      object_kind *k_ptr = &k_info[i];

      rd_byte(fff, &tmp8u);

      k_ptr->aware = (tmp8u & 0x01) ? TRUE: FALSE;
      k_ptr->tried = (tmp8u & 0x02) ? TRUE: FALSE;
      rd_item_log(fff, &k_ptr->log);
dlog((DEBUGSAVE2|DEBUGITEMS),"load.c: rd_savefile_new_aux: reading object mem %d of %d aware %d tried %d\n",
          i, tmp16u, k_ptr->aware, k_ptr->tried);
   }

   /* Load the Quests */
   rd_u16b(fff, &tmp16u);
dlog(DEBUGSAVE,"load.c: rd_savefile_new_aux: reading %d quests\n", tmp16u);

   /* Incompatible save files */
   if (tmp16u > 4)
   {
      msg_format("Too many (%u) quests!", tmp16u);
      return (23);
   }

   /* Load the Quests */
   for (i = 0; i < tmp16u; i++)
   {
      rd_byte(fff, &tmp8u);
      q_list[i].level = tmp8u;
dlog(DEBUGSAVE2,"load.c: rd_savefile_new_aux: reading quest %d of %d level %d\n",
               i, tmp16u, q_list[i].level);
   }

   /* Load the Artifacts */
   rd_u16b(fff, &tmp16u);
dlog(DEBUGSAVE,"load.c: rd_savefile: reading %d artifacts\n", tmp16u);

   /* Incompatible save files */
   if (tmp16u > a_number)
   {
      msg_format("Too many (%u) artifacts!", tmp16u);
      return (24);
   }

   /* Read the artifact flags */
   for (i = 0; i < tmp16u; i++)
   {
     artifact_type *a_ptr = &a_info[i];
      rd_byte(fff, &tmp8u);
      a_ptr->cur_num = tmp8u;
      rd_u16b(fff, &a_ptr->ident);
      rd_item_log(fff, &a_ptr->log);
dlog(DEBUGSAVE,"load.c: rd_savefile_new_aux: reading artifact %d of %d cur_num %d\n",
               i, tmp16u, a_info[i].cur_num);
   }

   /* hack -- read the ego-items */
   rd_u16b(fff, &tmp16u);
dlog(DEBUGSAVE,"load.c: rd_savefile: reading %d ego-items\n", tmp16u);
   /* Incompatible save files */
   if (tmp16u > e_number)
   {
      msg_format("Too many (%u) ego-items!", tmp16u);
      return (24);
   }

   for (i = 0; i < tmp16u; i++)
   {
      ego_item_type *e_ptr = &e_info[i];
      rd_item_log(fff, &e_ptr->log);
   }

   /* Read the extra stuff */
   if (rd_extra(fff)) return (26);

   /* Read the player_hp array */
   rd_u16b(fff, &tmp16u);

   /* Incompatible save files */
   if (tmp16u > PY_MAX_LEVEL)
   {
      msg_format("Too many (%u) hitpoint entries!", tmp16u);
      return (25);
   }

dlog(DEBUGSAVE,"load.c: rd_savefile_new_aux: reading %d player_hp[] entries\n", tmp16u);
   /* Read the player_hp array */
   for (i = 0; i < tmp16u; i++)
   {
      rd_s16b(fff, &player_hp[i]);
      rd_u32b(fff, &level_reached[i]);
dlog(DEBUGSAVE2,"load.c: rd_savefile_new_aux: reading hp %d of %d value %d\n",
                 i, tmp16u, player_hp[i]);
   }

dlog(DEBUGSAVE,"load.c: rd_savefile_new_aux: reading %d s_info[].numcast entries\n", s_number);
   /* Read the "spell numcast" entries */
   for (i=0; i < s_number; i++)
   {
      rd_u32b(fff, &s_info[i].numcast);
dlog(DEBUGSAVE2,"load.c: rd_savefile: reading spell numcast entry %d of %d value %d\n",
                 i, s_number, s_info[i].numcast);
   }

   /* load some info about the arena */
   rd_bool(fff, &been_in_arena);
   rd_s32b(fff, &arena_reward);
   rd_s32b(fff, &arena_previous_monsters);
   rd_s32b(fff, &arena_monsters);
   for (i=1; i<=PY_MAX_LEVEL; i++)
   {
      rd_s16b(fff, &arena_visit_level[i]);
dlog(DEBUGSAVE,"load.c: rd_savefile_new_aux: reading arena_visit_level %d of %d value %d\n",
               i, tmp16u, arena_visit_level[i]);
   }

   /* Important -- Initialize the sex */
   sp_ptr = &sex_info[p_ptr->psex];

   /* Important -- Initialize the race/class */
   rp_ptr = &race_info[p_ptr->prace];
   cp_ptr = &class_info[p_ptr->pclass];

   /* Read the inventory */
   if (rd_inventory(fff))
   {
      msg_print("Unable to read inventory");
      return (21);
   }
   /* I'm not dead yet... */
   if (!death)
   {
      rd_u16b(fff, &px);
      rd_u16b(fff, &py);
   }

   /* Success */
   return (0);
}

/*
 * Actually read the savefile
 *
 */
errr rd_savefile_new(void)
{
   errr err;
   FILE *fff = NULL;

   /* The savefile is a binary file */

dlog(DEBUGSAVE,"load.c: rd_savefile_new: starting with file savefile\n");
   fff = test_decompress_file(savefile);

   /* Paranoia */
   if (!fff) return (-1);

   /* Call the sub-function */
   err = rd_savefile_new_aux(fff);

   /* Check for errors */
   if (ferror(fff)) err = -1;

   /* Close the file */
   my_fclose(fff);

   /* Result */
   return (err);
}

errr read_info_header(char *file, u16b *number, u16b max_num,
                      u32b *name_size, u32b *text_size)
{
   char buf[1024];
   FILE *fff = NULL;

   strcpy (buf, ANGBAND_DIR_DATA);
   strcat (buf, file);

#if defined(MACINTOSH) && !defined(applec)
   /* Global -- "save file" */
   _ftype = 'SAVE';
#endif

   fff = my_fopen(buf, "rb");

   /* Successful open */
   if (!fff)
   {
      return(1);
   }
   else
   {
      byte maj, min, pat;

      rd_byte(fff, &maj);
      rd_byte(fff, &min);
      rd_byte(fff, &pat);
      if ((maj!=VERSION_MAJOR) || (min!=VERSION_MINOR) || (pat!=VERSION_PATCH))
      {
         my_fclose(fff);
         return(2);
         quit(format("Version mismatch while reading f_info data file: read %d.%d.%d expected %d.%d.%d",
             maj, min, pat, VERSION_MAJOR, VERSION_MINOR, VERSION_PATCH));
      }
      rd_u32b(fff, name_size);
      rd_u32b(fff, text_size);
      rd_u16b(fff, number);
      if (*number > max_num)
      {
         my_fclose(fff);
         return (3);
         quit(format("Too many monsters in datafile %s (%d max %d) f_info.",
                     file, *number, max_num));
      }

   }
   return(0);
}

errr read_info_data(char *file, char *array, u16b number, u16b itemsize,
                   char *name, u32b name_size, char *text, u32b text_size)
{
   char buf[1024];
   s16b fd;

   strcpy (buf, ANGBAND_DIR_DATA);
   strcat (buf, file);

#if defined(MACINTOSH) && !defined(applec)
   /* Global -- "save file" */
   _ftype = 'LOAD';
#endif

   fd = fd_open(buf, O_RDONLY);

   /* Successful open */
   if (fd<0)
   {
      return (1);
   }
   else
   {
      fd_read(fd, array, number * itemsize);
      fd_read(fd, name, name_size);
      fd_read(fd, text, text_size);

      if (fd_close(fd))
      {
         return (2);
      }
   }
   return (0);
}

/*
 * This function makes a ghost inventory for ghost g_idx
 * mirrored from the player's, with the gold, without the
 * artifacts, of max ITEM_SET_SIZE-1 random chosen items.
 */
static void handle_ghost_inventory(s16b g_idx)
{
   s16b i, j, k, count;
   s16b index[INVEN_TOTAL];

dlog((DEBUGGHOST | DEBUGSAVE),"load.c: handle_ghost_inventory: starting g_idx %d\n");

   /* Read the inventory */
   for (i = 0; i < INVEN_TOTAL; i++)
   {
      index[i]=0;
   }
   /* now find the non-artifacts in there */
   j=0;

   for (i = 0; i < INVEN_TOTAL; i++)
   {
      if ((!inventory[i].name1) && (inventory[i].k_idx!=0))
      {
         char i_name[128];
         object_desc(i_name, &inventory[i], TRUE, 3);
dlog((DEBUGGHOST | DEBUGSAVE),"load.c: handle_ghost_inventory: inv[%d] index %d = non-artifact %s\n",
                i, j, i_name);

         index[j++]=i;
      }
   }
   /* don't carry everything! */
   if (j>3) (j=j/2);
   /* we want ITEM_SET_SIZE -  maximum (1 item needed for gold!) */
   if (j > (ITEM_SET_SIZE)-1)
   {
      while (j > (ITEM_SET_SIZE-1) )
      {
         /* wipe a random item */
         i=randint(j);
dlog((DEBUGGHOST | DEBUGSAVE),"load.c: handle_ghost_inventory: wiping item %d\n", i);
         if (i<j-1)
         {
dlog((DEBUGGHOST | DEBUGSAVE),"load.c: handle_ghost_inventory: scrolling down %d to %d\n", i, j-1);
            /* scroll the ones above down if needed */
            for (k=i; k<(j-1); k++)
            {
               index[k]=index[k+1];
            }
         }
         j--;
      }
      count = ITEM_SET_SIZE-1;
   }
   else
   {
      count = j;
   }
dlog((DEBUGGHOST | DEBUGSAVE),"load.c: handle_ghost_inventory: %d items, max %d\n",
                j, ITEM_SET_SIZE-1);
   /* now store it */
   for (i=0; i<count; i++)
   {
      ghost_info[g_idx].inv[i]=inventory[index[i]];
{
   char i_name[80];
   object_type *i_ptr = &ghost_info[g_idx].inv[i];
   object_desc(i_name, i_ptr, TRUE, 3);
dlog((DEBUGGHOST | DEBUGSAVE),"load.c: handle_ghost_inventory: tv %d pv %d sv %d name %s\n",
                i_ptr->tval, i_ptr->sval, i_ptr->tval, i_name);
}
   }

   create_gold_item(&ghost_info[g_idx].inv[count]);
   ghost_info[g_idx].inv[count].p1val = p_ptr->au;
dlog((DEBUGGHOST | DEBUGSAVE),"load.c: handle_ghost_inventory: p1val %ld au %ld in item %d\n",
   ghost_info[g_idx].inv[count].p1val, p_ptr->au, count);

}

/*
 * This function tries to translate all spells the current player has
 * into monster spells for a ghost. It returns the number of successfully
 * translated spells (max 88).
 */
static s16b handle_ghost_spells(s16b r_idx)
{
   monster_race *r_ptr = &r_info[r_idx];
   s16b          i, j, result = 0;
   s16b          found[MAX_SPELLS_PER_ITEM];

   for (j=0; j<s_number; j++) found[j]=FALSE;

   for (i=0; i<INVEN_PACK; i++)
   {
      object_type *i_ptr = &inventory[i];
      bool item_ok = FALSE;

      if (i_ptr->tval == TV_BOOK) item_ok = TRUE;
      if ( (p_ptr->pclass & MAGE_MAGIC_CLASS) &&
           (i_ptr->tval==TV_HAFTED) && (i_ptr->sval==SV_QUARTERSTAFF))
      {
         if ((i_ptr->name2==EGO_MAGE) || (i_ptr->name2==EGO_ADEPT) ||
             (i_ptr->name2==EGO_ARCHMAGE)) item_ok = TRUE;
      }
      if (item_ok)
      {
         for (j=0; j<s_number; j++)
         {
            found[j] |= (has_spell(&inventory[i], j));
         }
      }
   }
for (i=0; i<s_number; i++)
{
   if (found[i])
      dlog((DEBUGGHOST | DEBUGSAVE),"load.c: handle_ghost_spells: found spell %d\n", i);
}
   /* please note that we can translate 60 different spells */
   /* flags4, 5 and 6: the spells. */
   if (found[1])
   {
      result++;
      r_ptr->flags5 |= RF5_MISSILE;
   }
   if (found[2])
   {
      result++;
      r_ptr->flags6 |= RF6_DARKNESS;
   }
   if (found[3])
   {
      result++;
      r_ptr->flags6 |= RF6_BLINK;
   }
   if (found[4])
   {
      result++;
      r_ptr->flags4 |= RF4_BR_LITE;
   }
   if (found[5])
   {
      result++;
      r_ptr->flags6 |= RF6_DARKNESS;
   }
   if (found[6] && (randint(10)<2))
   {
      result++;
      r_ptr->flags6 |= RF6_HEAL;
   }
   if (found[7])
   {
      result++;
      r_ptr->flags6 |= RF6_DARKNESS;
   }
   if (found[8])
   {
      result++;
      r_ptr->flags6 |= RF6_TRAPS;
   }
   if (found[9])
   {
      result++;
      r_ptr->flags4 |= RF4_BR_POIS;
   }
   if (found[20])
   {
      result++;
      r_ptr->flags4 |= RF4_BR_CONF;
   }
   if (found[20])
   {
      result++;
      r_ptr->flags4 |= RF4_BR_ELEC;
   }
   if (found[22])
   {
      result++;
      r_ptr->flags2 |= RF2_BASH_DOOR;
   }
   /* this isn't exactly sleep, but it's close :-) */
   if (found[23])
   {
      result++;
      r_ptr->flags5 |= RF5_HOLD;
   }
   if (found[23])
   {
      result++;
      r_ptr->flags6 |= RF6_TPORT;
   }
   if (found[26])
   {
      result++;
      r_ptr->flags4 |= RF4_BR_LITE;
   }
   if (found[27])
   {
      result++;
      r_ptr->flags5 |= RF5_BO_COLD;
   }
   if (found[28])
   {
      result++;
      r_ptr->flags2 |= RF2_KILL_WALL;
   }
   if (found[31])
   {
      result++;
      r_ptr->flags5 |= RF5_DRAIN_MANA;
   }
   if (found[32])
   {
      result++;
      r_ptr->flags5 |= RF5_HOLD;
   }
   /* polymorph is close to nexus */
   if (found[33])
   {
      result++;
      r_ptr->flags4 |= RF4_BR_NEXU;
   }
   if (found[34])
   {
      result++;
      r_ptr->flags6 |= RF6_FORGET;
   }
   if (found[35])
   {
      result++;
      r_ptr->flags5 |= RF5_HOLD;
   }
   if (found[36])
   {
      result++;
      r_ptr->flags5 |= RF5_BO_FIRE;
   }
   if (found[37])
   {
      result++;
      r_ptr->flags5 |= RF5_SLOW;
   }
   if (found[40])
   {
      result++;
      r_ptr->flags5 |= RF5_BA_COLD;
   }
   if (found[41])
   {
      result++;
      r_ptr->flags5 |= RF5_DRAIN_MANA;
   }
   if (found[42])
   {
      result++;
      r_ptr->flags6 |= RF6_TELE_AWAY;
   }
   if (found[43])
   {
      result++;
      r_ptr->flags6 |= RF6_HASTE;
   }
   if (found[44])
   {
      result++;
      r_ptr->flags5 |= RF5_BA_FIRE;
   }
   /* genocide really pays off now... */
   if (found[46])
   {
      result++;
      if (p_ptr->max_plv>40)
      {
         r_ptr->flags6 |= RF6_S_HOUND | RF6_S_DEMON | RF6_S_UNDEAD | RF6_S_DRAGON;
      }
      else if (p_ptr->max_plv>30)
      {
         r_ptr->flags6 |= RF6_S_HOUND | RF6_S_DRAGON;
      }
      else if (p_ptr->max_plv>20)
      {
         r_ptr->flags6 |= RF6_S_MONSTERS | RF6_S_SPIDER;
      }
      else if (p_ptr->max_plv>10)
      {
         r_ptr->flags6 |= RF6_S_MONSTER | RF6_S_ANT;
      }
      else
      {
         r_ptr->flags6 |= RF6_S_MONSTER;
      }
   }
   if (found[50])
   {
      result++;
      r_ptr->flags3 |= RF3_IM_FIRE;
   }
   if (found[51])
   {
      result++;
      r_ptr->flags3 |= RF3_IM_COLD;
   }
   if (found[52])
   {
      result++;
      r_ptr->flags3 |= RF3_IM_ACID;
   }
   if (found[53])
   {
      result++;
      r_ptr->flags3 |= RF3_IM_POIS;
   }
   if (found[60])
   {
      result++;
      r_ptr->flags2 |= RF2_BASH_DOOR;
   }
   if (found[62])
   {
      result++;
      r_ptr->flags6 |= RF6_TELE_LEVEL;
   }
   if (found[64])
   {
      result++;
      r_ptr->flags6 |= RF6_TELE_LEVEL;
   }
   if (found[70])
   {
      result++;
      r_ptr->flags6 |= RF6_TRAPS;
      r_ptr->flags6 |= RF6_FORGET;
   }
   if (found[71])
   {
      result++;
      r_ptr->flags6 |= RF6_TRAPS;
      r_ptr->flags6 |= RF6_DARKNESS;
   }
   if (found[72])
   {
      result++;
      r_ptr->flags5 |= RF5_DRAIN_MANA;
   }
   if (found[73])
   {
      result++;
      if (p_ptr->max_plv>40)
      {
         r_ptr->flags6 |= RF6_S_HOUND | RF6_S_DEMON | RF6_S_UNDEAD | RF6_S_DRAGON;
      }
      else if (p_ptr->max_plv>30)
      {
         r_ptr->flags6 |= RF6_S_HOUND | RF6_S_DRAGON;
      }
      else if (p_ptr->max_plv>20)
      {
         r_ptr->flags6 |= RF6_S_MONSTERS | RF6_S_SPIDER;
      }
      else if (p_ptr->max_plv>10)
      {
         r_ptr->flags6 |= RF6_S_MONSTER | RF6_S_ANT;
      }
      else
      {
         r_ptr->flags6 |= RF6_S_MONSTER;
      }
   }
   if (found[74])
   {
      result++;
      if (p_ptr->max_plv>40)
      {
         r_ptr->flags6 |= RF6_S_ANGEL | RF6_S_DEMON | RF6_S_HYDRA | RF6_S_DRAGON;
      }
      else if (p_ptr->max_plv>30)
      {
         r_ptr->flags6 |= RF6_S_HYDRA | RF6_S_DRAGON;
      }
      else if (p_ptr->max_plv>20)
      {
         r_ptr->flags6 |= RF6_S_MONSTERS | RF6_S_SPIDER;
      }
      else if (p_ptr->max_plv>10)
      {
         r_ptr->flags6 |= RF6_S_MONSTER | RF6_S_ANT;
      }
      else
      {
         r_ptr->flags6 |= RF6_S_MONSTER;
      }
   }
   if (found[83])
   {
      result++;
      r_ptr->flags6 |= RF6_HASTE;
   }
   if (found[90])
   {
      result++;
      r_ptr->flags4 |= RF4_BR_ACID;
   }
   if (found[91])
   {
      result++;
      r_ptr->flags5 |= RF5_BA_POIS;
   }
   if (found[92])
   {
      result++;
      r_ptr->flags5 |= RF5_BA_ACID;
   }
   if (found[93])
   {
      result++;
      r_ptr->flags5 |= RF5_BA_COLD;
   }
   /* meteor storm becomes darkness storm */
   if (found[94])
   {
      result++;
      r_ptr->flags5 |= RF5_BA_DARK;
   }
   if (found[95])
   {
      result++;
      r_ptr->flags5 |= RF5_BA_MANA;
   }
   /* somewhere around here the priest spells start */
   if (found[150])
   {
      result++;
      r_ptr->flags6 |= RF6_DARKNESS;
   }
   if (found[154])
   {
      result++;
      r_ptr->flags4 |= RF4_BR_LITE;
   }
   if (found[155])
   {
      result++;
      r_ptr->flags6 |= RF6_TRAPS;
   }
   if (found[156])
   {
      result++;
      r_ptr->flags6 |= RF6_DARKNESS;
   }
   if (found[161])
   {
      result++;
      r_ptr->flags6 |= RF6_TPORT;
   }
   if (found[164])
   {
      result++;
      r_ptr->flags5 |= RF5_HOLD;
   }
   if (found[167])
   {
      result++;
      r_ptr->flags3 |= (RF3_IM_FIRE | RF3_IM_COLD);
   }
   if (found[170])
   {
      result++;
      r_ptr->flags3 |= RF3_IM_POIS;
   }
   /* holy orb becomes chaos */
   if (found[171])
   {
      result++;
      r_ptr->flags4 |= RF4_BR_CHAO;
   }
   if (found[172] && (randint(10)<2))
   {
      result++;
      r_ptr->flags6 |= RF6_HEAL;
   }
   if (found[176])
   {
      result++;
      r_ptr->flags6 |= RF6_DARKNESS;
   }
   if (found[177] && (randint(10)<3))
   {
      result++;
      r_ptr->flags6 |= RF6_HEAL;
   }
   if (found[178])
   {
      result++;
      r_ptr->flags6 |= RF6_S_UNDEAD;
   }
   if (found[181])
   {
      result++;
      r_ptr->flags6 |= RF6_S_UNDEAD;
   }
   if (found[182] && (randint(10)<4))
   {
      result++;
      r_ptr->flags6 |= RF6_HEAL;
   }
   if (found[183])
   {
      result++;
      r_ptr->flags6 |= RF6_S_UNDEAD;
   }
   if (found[184])
   {
      result++;
      r_ptr->flags6 |= RF6_TRAPS;
   }
   if (found[185] && (randint(10)<6))
   {
      result++;
      r_ptr->flags6 |= RF6_HEAL;
   }
   if (found[185])
   {
      result++;
      r_ptr->flags3 |= RF3_IM_POIS;
      r_ptr->flags3 |= RF3_NO_FEAR;
      r_ptr->flags3 |= RF3_NO_SLEEP;
   }
   if (found[190])
   {
      result++;
      r_ptr->flags6 |= RF6_BLINK;
   }
   if (found[191])
   {
      result++;
      r_ptr->flags6 |= RF6_TPORT;
   }
   if (found[192])
   {
      result++;
      r_ptr->flags6 |= RF6_TELE_AWAY;
   }
   if (found[193])
   {
      result++;
      r_ptr->flags6 |= RF6_TELE_LEVEL;
   }
   if (found[194])
   {
      result++;
      r_ptr->flags6 |= RF6_TELE_LEVEL;
   }
   /* change level becomes breathe mana */
   if (found[195])
   {
      result++;
      r_ptr->flags5 |= RF5_BA_MANA;
   }
   if (found[200])
   {
      result++;
      r_ptr->flags6 |= RF6_DARKNESS;
   }
   if (found[201])
   {
      result++;
      r_ptr->flags6 |= RF6_DARKNESS;
      r_ptr->flags6 |= RF6_TRAPS;
   }
   if (found[202])
   {
      result++;
      r_ptr->flags6 |= RF6_FORGET;
   }
   if (found[204])
   {
      result++;
      r_ptr->flags4 |= RF4_BR_LITE;
   }
   if (found[210] && (randint(10)<3))
   {
      result++;
      r_ptr->flags6 |= RF6_HEAL;
   }
   if (found[211] && (randint(10)<4))
   {
      result++;
      r_ptr->flags6 |= RF6_HEAL;
   }
   if (found[212])
   {
      result++;
      r_ptr->flags6 |= RF6_HEAL;
   }
   /* restoration */
   if (found[213])
   {
      /* if we have 2 blows or more: */
      if (r_ptr->blow[1].method)
      {
         result++;
         switch(randint(6))
         {
            case 1: r_ptr->blow[1].method=RBE_LOSE_STR;
            case 2: r_ptr->blow[2].method=RBE_LOSE_INT;
            case 3: r_ptr->blow[3].method=RBE_LOSE_WIS;
            case 4: r_ptr->blow[4].method=RBE_LOSE_DEX;
            case 5: r_ptr->blow[5].method=RBE_LOSE_CON;
            case 6: r_ptr->blow[6].method=RBE_LOSE_CHR;
         }
      }
   }
   if (found[220])
   {
      result++;
      r_ptr->flags2 |= RF2_BASH_DOOR;
   }
   if (found[221])
   {
      result++;
      r_ptr->flags5 |= RF5_DRAIN_MANA;
   }
   if (found[230])
   {
      result++;
      r_ptr->flags6 |= RF6_S_UNDEAD;
   }
   if (found[231])
   {
      result++;
      r_ptr->flags6 |= RF6_S_UNDEAD;
   }
   if (found[232])
   {
      result++;
      r_ptr->flags6 |= RF6_S_HI_UNDEAD;
   }
   if (found[234])
   {
      result++;
      r_ptr->flags5 |= RF5_BRAIN_SMASH;
      r_ptr->flags5 |= RF5_CAUSE_3;
   }
dlog((DEBUGGHOST | DEBUGSAVE),"load.c: handle_ghost_spells: flag1 %08lx flag2 %08lx flag3 %08lx\n",
                r_ptr->flags1, r_ptr->flags2, r_ptr->flags3);
dlog((DEBUGGHOST | DEBUGSAVE),"load.c: handle_ghost_spells: flag4 %08lx flag5 %08lx flag6 %08lx\n",
                r_ptr->flags4, r_ptr->flags5, r_ptr->flags6);
   return (result); /* number of successfully translated spells */
}

/*
 * This function makes the player into a ghost.
 * returning r_idx where the ghost is made, or -1 if
 * creating the ghost failed.
 */
s16b player_to_ghost(void)
{
   s16b          i;
   s16b          g_idx = -1;
   s16b          r_idx = -1;
   char          temp[1024];
   s16b          dam, alertness;
   monster_race *r_ptr;
   ghost_type   *g_ptr;

   /* try to find a free slot in ghost_info */
   for (i=0; i<MAX_GHOSTS; i++)
   {
      if (!ghost_info[i].r_idx)
      {
         g_idx = i;
         break;
      }
   }
dlog((DEBUGGHOST | DEBUGSAVE),"load.c: player_to_ghost: g_idx %d\n", g_idx);
   /* if we have no space in ghost_info, we return FALSE */
   if (g_idx == -1) return(-1);

   /* look for a free place in the monster race array to leave this ghost */
   for (i = r_number; i < MAX_R_IDX; i++)
   {
      if (!r_info[i].name)
      {
         r_idx = i;
         break;
      }
   }
dlog((DEBUGGHOST | DEBUGSAVE),"load.c: player_to_ghost: r_idx %d\n", r_idx);
   /* if we have no space in r_info, we return FALSE */
   if (r_idx == -1) return (-1);

   r_idx = i;
   r_ptr = &r_info[r_idx];

   g_ptr = &ghost_info[g_idx];

   g_ptr->r_idx = r_idx;

   handle_ghost_inventory(g_idx);

   sprintf(temp, format("The Ghost of %s", player_name));
dlog((DEBUGGHOST | DEBUGSAVE),"load.c: player_to_ghost: name %s len %d\n",
                temp, strlen(temp));
   /* if adding this history overflows the space, don't make a ghost */
   if (r_name_size + strlen(temp)>= r_name_size_total)
      return (-1);
dlog((DEBUGGHOST | DEBUGSAVE),"load.c: player_to_ghost: r_name_size grows from %ld to %ld\n",
                r_name_size, r_name_size + strlen(temp)+1);
   r_ptr->name = r_name_size;

dlog((DEBUGGHOST | DEBUGSAVE),"load.c: player_to_ghost: r_ptr->name for r_idx %d now points to %08ld\n",
                r_idx, r_ptr->name);

   strcpy(r_name + r_name_size, temp);
   r_name_size += strlen(temp) + 1;

   sprintf(temp,format("The Ghost of %s the %s %s (%s). ",
                       player_name,
                       rp_ptr->title,
                       cp_ptr->title,
                       player_title[p_ptr->pclass][(p_ptr->lev-1)/5]));
   strcat(temp,format("%s visited the dungeons of Angband down to level %d, but was killed by %s on level %d.",
                      (p_ptr->psex==SEX_MALE)?"He":"She",
                      p_ptr->max_dlv, died_from, p_ptr->mdepth));
dlog((DEBUGGHOST | DEBUGSAVE),"load.c: player_to_ghost: text %s len %d\n",
                temp, strlen(temp));

   /* if adding this history overflows the space, don't make a ghost */
   if (r_text_size + strlen(temp)>= r_text_size_total)
      return (-1);

dlog((DEBUGGHOST | DEBUGSAVE),"load.c: player_to_ghost: r_text_size grows from %ld to %ld\n",
                r_text_size, r_text_size + strlen(temp)+1);

   r_ptr->text = r_text_size;
dlog((DEBUGGHOST | DEBUGSAVE),"load.c: player_to_ghost: r_ptr->text for r_idx %d now points to %08ld\n",
                r_idx, r_ptr->text);
   strcpy(r_text + r_text_size, temp);
   r_text_size += strlen(temp) + 1;
   r_number_total++;

   /* hitpoints: hside + hdice for the new ghost */
   if (p_ptr->mhp>1000)
      r_ptr->hside = 100;
   else if (p_ptr->mhp>800)
      r_ptr->hside = 80;
   else if (p_ptr->mhp>600)
      r_ptr->hside = 60;
   else if (p_ptr->mhp>400)
      r_ptr->hside = 40;
   else if (p_ptr->mhp>200)
      r_ptr->hside = 20;
   else
      r_ptr->hside = 10;
dlog((DEBUGGHOST | DEBUGSAVE),"load.c: player_to_ghost: mhp %d hside %d\n", p_ptr->mhp, r_ptr->hside);

   /* a little bonus - 3 times player's hp */
   r_ptr->hdice = 5*(p_ptr->mhp / r_ptr->hside);
dlog((DEBUGGHOST | DEBUGSAVE),"load.c: player_to_ghost: mhp %d hdice %d total %d\n",
                p_ptr->mhp, r_ptr->hdice, r_ptr->hside * r_ptr->hdice);

   r_ptr->ac = p_ptr->to_a;
   r_ptr->speed = p_ptr->pspeed;
   r_ptr->mexp = p_ptr->max_exp / 3;
   if (r_ptr->mexp>50000) r_ptr->mexp = 50000;
   alertness = (p_ptr->skill_srh + p_ptr->skill_pcp) / 12;
   if (alertness > 15) r_ptr->sleep = 0;
   else if (alertness > 12) r_ptr->sleep = 1;
   else if (alertness > 10) r_ptr->sleep = 3;
   else if (alertness > 8) r_ptr->sleep = 5;
   else if (alertness > 5) r_ptr->sleep = 10;
   else r_ptr->sleep = 15;
dlog((DEBUGGHOST | DEBUGSAVE),"load.c: player_to_ghost: alertness %d sleep %d\n",
                alertness, r_ptr->sleep);

   /* direct damage */
   for (i=0; i<3; i++)
   {
      r_ptr->blow[i].method = 0;
      r_ptr->blow[i].effect = 0;
      r_ptr->blow[i].d_side = 0;
      r_ptr->blow[i].d_dice = 0;
   }

   /* Do the damage first, so blows are defined once spells are added */
   dam = p_ptr->to_d;
   for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
   {
      if ((i==INVEN_LEFT) || (i==INVEN_RIGHT)) continue;
      if (i==INVEN_BOW) continue;
      if (inventory[i].k_idx) dam += inventory[i].to_d;
   }
   dam = (p_ptr->num_blow1 + p_ptr->num_blow2) * dam;
dlog((DEBUGGHOST | DEBUGSAVE),"load.c: player_to_ghost: blow1 %d blow2 %d to_d %d dam total %d\n",
                p_ptr->num_blow1, p_ptr->num_blow2, p_ptr->to_d, dam);

   if (dam > 500)
   {
      r_ptr->blow[0].method = RBM_TOUCH;
      r_ptr->blow[0].effect = RBE_HURT;
      r_ptr->blow[0].d_side = (byte)10;
      r_ptr->blow[0].d_dice = (byte)(dam / 40);
      r_ptr->blow[1].method = RBM_TOUCH;
      r_ptr->blow[1].effect = RBE_HURT;
      r_ptr->blow[1].d_side = (byte)10;
      r_ptr->blow[1].d_dice = (byte)(dam / 40);
      r_ptr->blow[2].method = RBM_TOUCH;
      r_ptr->blow[2].effect = RBE_HURT;
      r_ptr->blow[2].d_side = (byte)10;
      r_ptr->blow[2].d_dice = (byte)(dam / 40);
      r_ptr->blow[3].method = RBM_TOUCH;
      r_ptr->blow[3].effect = RBE_HURT;
      r_ptr->blow[3].d_side = (byte)10;
      r_ptr->blow[3].d_dice = (byte)(dam / 40);
dlog((DEBUGGHOST | DEBUGSAVE),"load.c: player_to_ghost: dam %d = %dd%d\n",
                dam, 10, (dam / 40));
   }
   else if (dam > 300)
   {
      r_ptr->blow[0].method = RBM_TOUCH;
      r_ptr->blow[0].effect = RBE_HURT;
      r_ptr->blow[0].d_side = (byte)10;
      r_ptr->blow[0].d_dice = (byte)(dam / 30);
      r_ptr->blow[1].method = RBM_TOUCH;
      r_ptr->blow[1].effect = RBE_HURT;
      r_ptr->blow[1].d_side = (byte)10;
      r_ptr->blow[1].d_dice = (byte)(dam / 30);
      r_ptr->blow[2].method = RBM_TOUCH;
      r_ptr->blow[2].effect = RBE_HURT;
      r_ptr->blow[2].d_side = (byte)10;
      r_ptr->blow[2].d_dice = (byte)(dam / 30);
dlog((DEBUGGHOST | DEBUGSAVE),"load.c: player_to_ghost: dam %d = %dd%d\n",
                dam, 10, (dam / 30));
   }
   else if (dam > 150)
   {
      r_ptr->blow[0].method = RBM_TOUCH;
      r_ptr->blow[0].effect = RBE_HURT;
      r_ptr->blow[0].d_side = (byte)10;
      r_ptr->blow[0].d_dice = (byte)(dam / 20);
      r_ptr->blow[1].method = RBM_TOUCH;
      r_ptr->blow[1].effect = RBE_HURT;
      r_ptr->blow[1].d_side = (byte)10;
      r_ptr->blow[1].d_dice = (byte)(dam / 20);
dlog((DEBUGGHOST | DEBUGSAVE),"load.c: player_to_ghost: dam %d = %dd%d\n",
                dam, 10, (dam / 20));
   }
   else
   {
      r_ptr->blow[0].method = RBM_TOUCH;
      r_ptr->blow[0].effect = RBE_HURT;
      r_ptr->blow[0].d_side = (byte)10;
      r_ptr->blow[0].d_dice = (byte)(dam / 10);
      if (r_ptr->blow[0].d_dice==0) r_ptr->blow[0].d_dice = 1;
dlog((DEBUGGHOST | DEBUGSAVE),"load.c: player_to_ghost: dam %d = %dd%d\n",
                dam, 10, (dam / 10));
   }

   /* add the spells */
   r_ptr->flags1=0L;
   r_ptr->flags2=0L;
   r_ptr->flags3=0L;
   r_ptr->flags4=0L;
   r_ptr->flags5=0L;
   r_ptr->flags6=0L;
   i=handle_ghost_spells(r_idx);

   /* if we know a lot, cast them often! */
   /* note 88 is maximum */
   if (i > 80)
      r_ptr->freq_inate = r_ptr->freq_spell = 90;
   else if (i > 70)
      r_ptr->freq_inate = r_ptr->freq_spell = 80;
   else if (i > 60)
      r_ptr->freq_inate = r_ptr->freq_spell = 70;
   else if (i > 50)
      r_ptr->freq_inate = r_ptr->freq_spell = 60;
   else if (i > 40)
      r_ptr->freq_inate = r_ptr->freq_spell = 50;
   else if (i > 30)
      r_ptr->freq_inate = r_ptr->freq_spell = 40;
   else if (i > 20)
      r_ptr->freq_inate = r_ptr->freq_spell = 30;
   else if (i > 10)
      r_ptr->freq_inate = r_ptr->freq_spell = 20;
   else if (i > 0)
      r_ptr->freq_inate = r_ptr->freq_spell = 10;
dlog((DEBUGGHOST | DEBUGSAVE),"load.c: player_to_ghost: %d spells known, freq_spell %d\n",
                i, r_ptr->freq_inate);

   /* flags1 */
   r_ptr->flags1 |= (p_ptr->psex==SEX_MALE)?RF1_MALE:RF1_FEMALE;
   /* make sure the ghost starts healthy */
   r_ptr->flags1 |= RF1_UNIQUE | RF1_FORCE_MAXHP;

   /* flags2 */
   r_ptr->flags2 |= RF2_SMART | RF2_INVISIBLE | RF2_COLD_BLOOD | RF2_PASS_WALL;

   /* flags3 */
   r_ptr->flags3 |= RF3_UNDEAD | RF3_EVIL;

   /* if we don't have a resistance spell (which would be in flags3 by now!)  */
   /* and we also don't have permanent resistance/immunity, we are hurt by it */
   if (!(r_ptr->flags3 & RF3_IM_COLD) && !(p_ptr->resist_cold) && !(p_ptr->immune_cold))
      r_ptr->flags3 |= RF3_HURT_COLD;
   else if (p_ptr->immune_cold)
      r_ptr->flags3 |= RF3_IM_COLD;

   if (!(r_ptr->flags3 & RF3_IM_FIRE) && !(p_ptr->resist_fire) && !(p_ptr->immune_fire))
      r_ptr->flags3 |= RF3_HURT_FIRE;
   else if (p_ptr->immune_fire)
      r_ptr->flags3 |= RF3_IM_FIRE;

   if (p_ptr->immune_acid) r_ptr->flags3 |= RF3_IM_ACID;
   if (p_ptr->immune_elec) r_ptr->flags3 |= RF3_IM_ELEC;

   if (p_ptr->resist_pois) r_ptr->flags3 |= RF3_IM_POIS;
   if (p_ptr->resist_neth) r_ptr->flags3 |= RF3_RES_NETH;
   if (p_ptr->resist_nexus) r_ptr->flags3 |= RF3_RES_NEXU;
   if (p_ptr->resist_disen) r_ptr->flags3 |= RF3_RES_DISE;
   if (p_ptr->resist_fear) r_ptr->flags3 |= RF3_NO_FEAR;
   if (p_ptr->resist_conf) r_ptr->flags3 |= RF3_NO_CONF;

   /* shooting players make shooting ghosts.... */
   if ((p_ptr->xtra_shots) || (p_ptr->xtra_might))
   {
      object_type *i_ptr = &inventory[INVEN_AMMO];
dlog((DEBUGGHOST | DEBUGSAVE),"load.c: player_to_ghost: xtra_shots %d xtra_might %d : RF4_ARROW_2\n",
                p_ptr->xtra_shots, p_ptr->xtra_might);
      r_ptr->flags4 |= RF4_ARROW_2;
      if (i_ptr->k_idx)
      {
         /* we are shooting special ammo? This will be a powerfull ghost... */
         if (i_ptr->name2 == EGO_WOUNDING)
         {
            r_ptr->flags4 |= RF4_ARROW_4;
dlog((DEBUGGHOST | DEBUGSAVE),"load.c: player_to_ghost: ammo EGO_WOUNDING : RF4_ARROW_4\n");
         }
         else if (i_ptr->name2)
         {
            r_ptr->flags4 |= RF4_ARROW_3;
dlog((DEBUGGHOST | DEBUGSAVE),"load.c: player_to_ghost: ammo EGO : RF4_ARROW_3\n");
         }
      }

   }
   else
   {
      if (inventory[INVEN_AMMO].k_idx)
      {
         r_ptr->flags4 |= RF4_ARROW_1;
dlog((DEBUGGHOST | DEBUGSAVE),"load.c: player_to_ghost: ammo : RF4_ARROW_1\n");
      }
   }


dlog(DEBUGSAVE,"load.c: player_to_ghost: f1 %08lx  f2 %08lx f3 %08lx f4 %08lx f5 %08lx f6 %08lx\n",
     r_ptr->flags1,r_ptr->flags2,r_ptr->flags3,r_ptr->flags4,r_ptr->flags5,r_ptr->flags6);

   r_ptr->corpse_chance = 0;
   r_ptr->corpse_chance_bones = 0;

   /* even Morgoth only has level 100 */
   r_ptr->level = (byte)(128L*(u32b)p_ptr->max_plv / 100);

   r_ptr->d_attr = TERM_SLATE;
   r_ptr->d_char = 'G';
   r_ptr->x_attr = TERM_SLATE;
   r_ptr->x_char = 'G';
   r_ptr->max_num = 1;

   /* the lore variables: */
   r_ptr->r_sights=0;
   r_ptr->r_deaths=0;

   r_ptr->r_pkills=0;
   r_ptr->r_tkills=0;

   r_ptr->r_wake=0;
   r_ptr->r_ignore=0;

   r_ptr->r_xtra1=0;
   r_ptr->r_xtra2=0;

   r_ptr->r_drop_gold=0;
   r_ptr->r_drop_item=0;

   r_ptr->r_cast_inate=0;
   r_ptr->r_cast_spell=0;

   r_ptr->r_blows[0] = r_ptr->r_blows[1] = 0;
   r_ptr->r_blows[2] = r_ptr->r_blows[3] = 0;

   r_ptr->r_flags1 = r_ptr->r_flags2 = r_ptr->r_flags3 =0L;
   r_ptr->r_flags4 = r_ptr->r_flags5 = r_ptr->r_flags6 = 0L;

dlog((DEBUGGHOST | DEBUGSAVE),"load.c: player_to_ghost: returning r_idx %d\n", r_idx);

   return (r_idx);
};


/*
 * This function reads all the ghosts a player can meet in this game
 * multi-player capabilities prevent us from reusing ghosts from a
 * common pool other than at startup of a new game
 * this is the function for startup of a game
 */
bool rd_ghost_files(void)
{
   s16b i, r_idx;
   char buf[1024];
   FILE *fff = NULL;

   r_idx = r_number;
   r_number_total = r_number;
dlog((DEBUGGHOST | DEBUGSAVE),"load.c: rd_ghost_files: about to try %d ghost-files in %s\n",
                MAX_GHOSTS, ANGBAND_DIR_BONE);

   for (i=0; i < MAX_GHOSTS; i++)
   {
      strcpy (buf, ANGBAND_DIR_BONE);
      strcat (buf, format("ghost%03d", i));

#if defined(MACINTOSH) && !defined(applec)
      /* Global -- "save file" */
      _ftype = 'LOAD';
#endif

      fff = my_fopen(buf, "rb");

      /* Unsuccessful open */
      if (!fff)
      {
         WIPE(&ghost_info[i], ghost_type);
         continue; /* no ghost file at this number */
      }
      else
      {
dlog((DEBUGGHOST | DEBUGSAVE),"load.c: rd_ghost_files: %s exists, about to read ghost info\n", buf);

         if (!rd_ghost(fff, r_idx))
         {
dlog((DEBUGGHOST | DEBUGSAVE),"load.c: rd_ghost_files: ghost info error in %s\n", buf);
            WIPE(&ghost_info[i], ghost_type);
dlog((DEBUGGHOST | DEBUGSAVE),"load.c: rd_ghost_files: ghost info[%d] now wiped \n", i);

            my_fclose(fff);
            return (FALSE);
         }
dlog((DEBUGGHOST | DEBUGSAVE),"load.c: rd_ghost_files: read ghost r_idx %d r_number_total %d r_number %d\n",
                 r_idx, r_number_total, r_number);
         r_idx++;
         r_number_total++;
      }
      my_fclose(fff);
   }
   return (TRUE);
};

/*
 * This functions make a ghost from the current player
 * and saves this ghost somewhere in lib/ghost/
 */

bool make_new_ghost(void)
{
   s16b r_idx, i;
   FILE *fff;
   char buf[1024];
dlog((DEBUGGHOST | DEBUGSAVE),"load.c: make_new_ghost: starting\n");
   r_idx = player_to_ghost();

dlog((DEBUGGHOST | DEBUGSAVE),"load.c: make_new_ghost: r_idx %d\n", r_idx);
   if (r_idx==-1) return (FALSE);


   for (i=0; i < MAX_GHOSTS; i++)
   {
      strcpy (buf, ANGBAND_DIR_BONE);
      strcat (buf, format("ghost%03d", i));
dlog((DEBUGGHOST | DEBUGSAVE),"load.c: make_new_ghost: testing %s\n", buf);
#if defined(MACINTOSH) && !defined(applec)
      /* Global -- "save file" */
      _ftype = 'LOAD';
#endif

      fff = my_fopen(buf, "rb");

      /* Successful open */
      if (!fff)
      {
dlog((DEBUGGHOST | DEBUGSAVE),"load.c: make_new_ghost: %s doesn't exist yet.\n", buf);
         break;
      }
      else
      {
         my_fclose(fff);
      }
   }
dlog((DEBUGGHOST | DEBUGSAVE),"load.c: make_new_ghost: buf %s i %d max %d\n",
                buf, i, MAX_GHOSTS);
   /* if we have tested all without finding a non-existing file */
   if (i==MAX_GHOSTS) return (FALSE);
   my_fclose(fff);
   fff = my_fopen(buf, "w");
/* we should add locking here, but I'm not sure on the difference between */
/* fd_ style file operations and FILE *fff style file ops. There seems to */
/* be no locking for FILE *fff's. */
   if (!fff)
   {
dlog((DEBUGGHOST | DEBUGSAVE),"load.c: make_new_ghost: cannot open %s for writing\n", buf);
      return (FALSE);
   }

   wr_ghost(fff, r_idx);
   my_fclose(fff);
   return (TRUE);
}
