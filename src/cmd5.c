/* File: cmd5.c */

/* Purpose: Spell/Prayer commands */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

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
   object_type *i_ptr;

   i_ptr = &inventory[INVEN_LEFT];
   if ((i_ptr->tval == TV_RING) && (i_ptr->sval == sval)) return (TRUE);
   i_ptr = &inventory[INVEN_RIGHT];
   if ((i_ptr->tval == TV_RING) && (i_ptr->sval == sval)) return (TRUE);

   /* High Priests can wield 2 other rings */
   if (p_ptr->pclass != CLASS_HIGHPRST) return (FALSE);
   i_ptr = &inventory[INVEN_WIELD];
   if ((i_ptr->tval == TV_RING) && (i_ptr->sval == sval)) return (TRUE);
   i_ptr = &inventory[INVEN_BOW];
   if ((i_ptr->tval == TV_RING) && (i_ptr->sval == sval)) return (TRUE);

   return FALSE;
}

/* 
 * this function tests if there is a ring that helps a certain spell
 */
static bool ring_helps_spell(s16b spellno)
{
   if ((spellno == SV_SPELL_MLIGHTNING_BOLT) && (elemental_ring_worn(SV_RING_ELEC)))
   {
      return TRUE;
   }   
   if ((spellno == SV_SPELL_MFROST_BOLT) && (elemental_ring_worn(SV_RING_ICE)))
   {
      return TRUE;
   }   
   if ((spellno == SV_SPELL_MFIRE_BOLT) && (elemental_ring_worn(SV_RING_FLAMES)))
   {
      return TRUE;
   }   
   if ((spellno == SV_SPELL_MFROST_BALL) && (elemental_ring_worn(SV_RING_ICE)))
   {
      return TRUE;
   }   
   if ((spellno == SV_SPELL_MFIRE_BALL) && (elemental_ring_worn(SV_RING_FLAMES)))
   {
      return TRUE;
   }   
   if ((spellno == SV_SPELL_MACID_BOLT) && (elemental_ring_worn(SV_RING_ACID)))
   {
      return TRUE;
   }   
   if ((spellno == SV_SPELL_MICE_STORM) && (elemental_ring_worn(SV_RING_ICE)))
   {
      return TRUE;
   }   
   return FALSE;
}

/*
 * determine how many mana a spell will take
 * certain rings help
 */
static s16b spell_mana(s16b spellno)
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
 * how big is the chance of successfully casting a spell?
 */
s16b page_chance(s16b spellno)
{
   s16b         chance, minfail;

   /* Extract the base spell failure rate */
   chance = s_info[spellno].chance;
   /* Reduce failure rate by "effective" level adjustment */
   chance -= 3 * (p_ptr->lev - s_info[spellno].level);

   /* Reduce failure rate by INT/WIS adjustment */
   chance -= 3 * ((s16b)adj_mag_stat[p_ptr->stat_ind[(s16b)cp_ptr->spell_stat]] - 1);

   /* Not enough mana to cast */
   if (spell_mana(spellno) > p_ptr->csp)
   {
       chance += 5 * (spell_mana(spellno) - p_ptr->csp);
   }

   /* Extract the minimum failure rate */
   minfail = (s16b)adj_mag_fail[(s16b)p_ptr->stat_ind[(s16b)cp_ptr->spell_stat]];

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

   /* Hack -- Priest prayer penalty for "edged" weapons  -DGK */
   if ((p_ptr->pclass == 2) && (p_ptr->icky_wield)) chance += 25;

   /* Minimum failure rate */
   if (chance < minfail) chance = minfail;

   /* Stunning makes spells harder */
   if (p_ptr->stun > 50)
   {
      chance += 25;
   }
   else
   {
      if (p_ptr->stun>0) chance += 15;
   }

   /* wearing gloves makes it harder */
   if (p_ptr->cumber_glove) chance += chance +25;

   /* Always a 5 percent chance of working */
   if (chance > 95) chance = 95;

   /* Return the chance */
dlog(DEBUGITEMS,"cmd5.c: page_change returning %d\n", chance);
   return (chance);
}

void extra_new_spell_info(char *p, s16b spellno)
{
   s16b plev = p_ptr->lev;

   /* Default */
   strcpy(p, "");

   if ( (cheat_spell_info) || (s_info[spellno].numcast >= (10 * s_info[spellno].level) ) )
   {

      /* Analyze the spell */
      switch (spellno)
      {
         case SV_SPELL_MMAGIC_MISSILE:         sprintf(p, " dam %dd4", 3+((plev-1)/5)); break;
         case SV_SPELL_MPHASE_DOOR:            sprintf(p, " range 10"); break;
         case SV_SPELL_MTREASURE_DETECTION:    sprintf(p, " range %dd2+%d", (plev / 2), (plev / 10) + 1); break;
         case SV_SPELL_MCURE_LIGHT_WOUNDS:     sprintf(p, " heal 2d8"); break;
         case SV_SPELL_MSTINKING_CLOUD:        sprintf(p, " dam %d rad 2", 10+(plev / 2)); break;
         case SV_SPELL_MLIGHTNING_BOLT:        sprintf(p, " dam %dd3+8", 3+(plev-5)/4); break;
         case SV_SPELL_MTELEPORT_SELF:         sprintf(p, " range %d", plev * 5); break;
         case SV_SPELL_MFROST_BOLT:            sprintf(p, " dam %dd8", 5+(plev-5)/4); break;
         case SV_SPELL_MFIRE_BOLT:             sprintf(p, " dam %dd8", 8+(plev-5)/5); break;
         case SV_SPELL_MFROST_BALL:            sprintf(p, " dam %d rad 2", 30+plev); break;
         case SV_SPELL_MFIRE_BALL:             sprintf(p, " dam %d rad 2", 55+plev); break;
         case SV_SPELL_MWORD_OF_DESTRUCTION:   sprintf(p, " rad 15"); break;
         case SV_SPELL_MEARTHQUAKE:            sprintf(p, " rad 10"); break;
         case SV_SPELL_MACID_BOLT:             sprintf(p, " dam %dd8", 6+(plev-5)/4); break;
         case SV_SPELL_MCLOUDKILL:             sprintf(p, " dam %d rad 3", 20+plev/2); break;
         case SV_SPELL_MACID_BALL:             sprintf(p, " dam %d rad 2", 40+plev); break;
         case SV_SPELL_MICE_STORM:             sprintf(p, " dam %d rad 3", 70+plev); break;
         case SV_SPELL_MMETEOR_SWARM:          sprintf(p, " dam %d rad 3", 65+plev); break;
         case SV_SPELL_MHELLFIRE:              sprintf(p, " dam %d rad 3", 300+plev*2); break;

         case SV_SPELL_PCURE_LIGHT_WOUNDS:     sprintf(p, " 2d10"); break;
         case SV_SPELL_PCALL_LIGHT:            sprintf(p, " dam 2d%d rad %d",plev/2, plev/10+1); break;
         case SV_SPELL_PPORTAL:                sprintf(p, " range %d", plev*3); break;
         case SV_SPELL_PCURE_MEDIUM_WOUNDS:    sprintf(p, " 4d10"); break;
         case SV_SPELL_PORB_OF_DRAINING:       sprintf(p, " 3d6+%d rad %d",
                                                         plev+(plev/((p_ptr->pclass==2)?2:4)),
                                                         (plev<30)?2:3); break;
         case SV_SPELL_PCURE_SER_WOUNDS:       sprintf(p, " 6d10"); break;
         case SV_SPELL_PEARTHQUAKE:            sprintf(p, " rad 10"); break;
         case SV_SPELL_PCURE_CRIT_WOUNDS:      sprintf(p, " 8d10"); break;
         case SV_SPELL_PHEAL:                  sprintf(p, " 300"); break;
         case SV_SPELL_PHOLY_WORD:             sprintf(p, " 1000"); break;
         case SV_SPELL_PBLINK:                 sprintf(p, " range 10"); break;
         case SV_SPELL_PTELEPORT:              sprintf(p, " range %d", plev*8); break;
         case SV_SPELL_PCURE_SER_WOUNDS_EASY:  sprintf(p, " heal 4d10"); break;
         case SV_SPELL_PCURE_CRIT_WOUNDS_EASY: sprintf(p, " heal 8d10"); break;
         case SV_SPELL_PHEALING:               sprintf(p, " heal 2000"); break;
         case SV_SPELL_PWORD_OF_DESTRUCTION:   sprintf(p, " rad 15"); break;
         case SV_SPELL_PANNIHILATION:          sprintf(p, " dam 200"); break;
      }
   }
}

void print_spells(s16b *index, s16b count)
{
   s16b       i, col;
   byte       color;
   char       out_val[160];
   cptr       comment;
   char       info[80];
   bool       cheat_any = FALSE;
   spell_type *s_ptr;
   cptr       scalestr[6] = {"small","avg  ","large","super"};
   cptr       categ[MAX_SPELL_TYPES] = { "Nature Forces",
                                         "Dark Forces",
                                         "Escape",
                                         "Heal",
                                         "Sense",
                                         "Change Other",
                                         "Change Item",
                                         "Change Self",
                                         "Change World" };

   /* Print column */
   col = 4;

   /* Title the list */
   prt("", 0, 1);
   put_str("Name", col + 3, 1);
   for (i = 0; i < count; i++)                /* Dump the spells */
   {
      s_ptr = &s_info[index[i]];             /* Access the spell */
dlog(DEBUGITEMS,"cmd5.c: print_spells: spell %d of %d = %d\n", i, count, index[i]);
      if (s_ptr->numcast >= (10 * s_ptr->level))
      {
         cheat_any = TRUE;
      }
   }

   if (show_spell_numbers == FALSE)
   {
      if ( (cheat_spell_info == TRUE) || (cheat_any == TRUE ) )
      {
         put_str("Lv Ma Fail Scale Category        Extra", col + 27, 1);
      }
      else
      {
         put_str("Lv Ma Fail", col + 27, 1);
      }
   }
   else
   {
      if ( (cheat_spell_info == TRUE) || (cheat_any == TRUE ) )
      {
         put_str("Num Lv Ma Fail Scale Category        Extra", col + 27, 1);
      }
      else
      {
         put_str("Num Lv Ma Fail", col + 27, 1);
      }
   }
   strcpy(info,"");
   strcpy(info,"");

   for (i = 0; i < count; i++)                /* Dump the spells */
   {
       s_ptr = &s_info[index[i]];             /* Access the spell */
       color = (byte)TERM_L_GREEN;
       if (s_ptr->numcast == 0)
       {
          color = (byte)TERM_L_BLUE;
       }
       if (s_ptr->numcast >= (10 * s_ptr->level))
       {
          color = (byte)TERM_GREEN;
       }

       extra_new_spell_info(info, index[i]);  /* Get extra info */

       /* Use that info */
       comment = info;
      if ( (cheat_spell_info) || (s_ptr->numcast >= (10 * s_ptr->level) ) )
      {
         if (show_spell_numbers == FALSE)
         {
            sprintf(out_val, "%c) %-23s%3d %2d  %2d%% %5s %-14s %s",
                    I2A(i), s_name + s_ptr->name, s_ptr->level, spell_mana(index[i]),
                    page_chance(index[i]), scalestr[(s16b)s_ptr->scale],
                    categ[s_ptr->type], info);
         }
         else
         {
            sprintf(out_val, "%c) %03d %-23s%3d %2d  %2d%% %5s %-14s %s",
                    I2A(i), index[i], s_name + s_ptr->name, s_ptr->level, spell_mana(index[i]),
                    page_chance(index[i]), scalestr[(s16b)s_ptr->scale],
                    categ[s_ptr->type], info);
         }
      }
      else if (s_ptr->numcast > 0)
      {
         if (show_spell_numbers == FALSE)
         {
            sprintf(out_val, "%c) %-23s%3d %2d  %2d%%",
                    I2A(i), s_name + s_ptr->name, s_ptr->level, spell_mana(index[i]),
                    page_chance(index[i]));
         }
         else
         {
            sprintf(out_val, "%c) %03d %-23s%3d %2d  %2d%%",
                    I2A(i), index[i], s_name + s_ptr->name, s_ptr->level, spell_mana(index[i]),
                    page_chance(index[i]));
         }
      }
      else if (s_ptr->numcast == 0)
      {
         if (show_spell_numbers == FALSE)
         {
            sprintf(out_val, "%c) %-23s%3d %2d",
                    I2A(i), s_name + s_ptr->name, s_ptr->level, spell_mana(index[i]));
         }
         else
         {
            sprintf(out_val, "%c) %03d %-23s%3d %2d",
                    I2A(i), index[i], s_name + s_ptr->name, s_ptr->level, s_ptr->mana);
         }
      }
      prt("", 0, 2+i); /* this clears the line */
      c_put_str(color,out_val,col,2+i);
   }

   /* Clear the bottom line */
   prt("", 0, 2 + i);
}

static void browse_book(object_type *i_ptr)
{
   s16b count = 0, i;
   s16b index[MAX_SPELLS_PER_ITEM];

dlog(DEBUGITEMS,"cmd5.c: browse_book: i_ptr->spell_set %d\n", i_ptr->spell_set);
   for (i=0; i< (s16b)s_number; i++)
   {
      if (has_spell(i_ptr, i)) index[count++]=i;
   }
dlog(DEBUGITEMS,"cmd5.c: browse_book: %d spells found\n", count);

   /* Save the screen */
   (void)Term_save();

   print_spells(index, count);

   /* Clear the top line */
   prt("", 0, MESSAGE_ROW);

   /* Prompt user */
   put_str(format("Spells in your %s - [Press any key to continue]",
           (s16b)i_ptr->tval==TV_BOOK?"book":"staff"),MESSAGE_ROW, 0);

   /* Wait for key */
   (void)inkey();

   /* Restore the screen */
   (void)Term_load();
}

static bool item_tester_browsable(object_type *i_ptr)
{
   if ((p_ptr->pclass == CLASS_MAGE) || (p_ptr->pclass == CLASS_WARMAGE))
   {
      if (((s16b)i_ptr->tval == TV_HAFTED) && ((s16b)i_ptr->sval == SV_QUARTERSTAFF))
      {
         return ( ((s16b)i_ptr->name2 == EGO_MAGE) ||
                  ((s16b)i_ptr->name2 == EGO_ADEPT) ||
                  ((s16b)i_ptr->name2 == EGO_ARCHMAGE) );
      }
   }
   return ((s16b)i_ptr->tval == TV_BOOK);
}

/*
 * Peruse the spells/prayers in a Book
 *
 * Note that *all* spells in the book are listed
 */
void do_cmd_browse(void)
{
   s16b                item = 0;

   object_type         *i_ptr = NULL;
/* jk */
   s16b amt = 1;

   /* Warriors are illiterate */
   if (cp_ptr->spell_stat == (byte)0)
   {
       msg_print("You cannot read books!");
       return;
   }

   /* No lite */
   if ( (p_ptr->blind != 0) || no_lite())
   {
       msg_print("You cannot see!");
       return;
   }

   /* Confused */
   if (p_ptr->confused>0)
   {
      msg_print("You are too confused!");
      return;
   }

   /* Restrict choices to "useful" books */
   /* and certain ego-item staffs, if you are a mage */
   item_tester_tval=(byte)0;     /* no restrictions on type */
   item_tester_hook = item_tester_browsable;

   /* Get an item (from inven or floor) */
   if (!get_item(&item, &amt, "Browse which book? ", TRUE, TRUE, TRUE))
   {
      if (item == -2) msg_print("You have no books that you can read.");
      item_tester_hook = NULL;
      return;
   }
   item_tester_hook = NULL;

   /* Get the item (in the pack) */
   i_ptr=get_item_pointer(item);

   object_kind_track(i_ptr->k_idx);
   handle_stuff();

   browse_book(i_ptr);
   return;
}

/* select a spell from book 'item'. return -1 if not successful */
static s16b select_spell_from_book(s16b item)
{
   object_type *i_ptr;
   s16b         count, i, j, k, spellno;
   s16b         index[MAX_SPELLS_PER_ITEM];
   char         buf;

   /* Get the item */
   i_ptr=get_item_pointer(item);

   count = 0;
   for (i=0; i<MAX_SPELLS_PER_ITEM; i++)
   {
      if (has_spell(i_ptr, i)) index[count++]=i;
   }

   if (count==0)
   {
      msg_print("This book is empty.");
      return -1;
   }

   object_kind_track(i_ptr->k_idx);
   handle_stuff();

   /* Save the screen */
   (void)Term_save();

   print_spells(index, count);
   
   /* Clear the top line */
   prt("", 0, MESSAGE_ROW);

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
            for (k=0; k<s_number; k++)
            {
               if (has_spell(&inventory[j], spellno)) break;
            }
            if (k < s_number) break;
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

static s16b select_spell_by_number(void)
{
   char buf2[4];
   s16b i, j, spellno;

   buf2[0]='\0';
   prt("Enter spell number (globally over all your books):", 0, 0);
   if (!askfor_aux(buf2, 3))
   {
      return -1;
   }
   spellno=atoi(buf2);
   for (i=0; i<INVEN_PACK; i++)
   {
      if (inventory[i].tval == TV_BOOK)
      {
         for (j=0; j<s_number; j++)
         {
            if (has_spell(&inventory[i], spellno)) break;
         }
         if (j < s_number) break;
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

/* this function selects a spell, either by book or by number */
static s16b select_spell(void)
{
   s16b item, spellno, count, amt = 1;

   /* Restrict choices to scrolls */
   item_tester_tval = (byte)TV_BOOK;
   item_tester_hook = NULL;
   count = count_items(&item, FALSE, TRUE, TRUE);

   /* Get an item (from inven or floor) */
   if ( (count != 1) &&
        !get_item(&item, &amt, "Cast spell from what? ", FALSE, TRUE, TRUE))
   {
      if (item == -2) msg_print("You have nothing to read spells from.");
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

dlog(DEBUGITEMS,"cmd5.c: do_cmd_cast_spell: spellno %d\n", spellno);

   /* if exec_page returns FALSE, we don't have enough mana and aborted */
   if (exec_page(spellno))
   {
      /* Take a turn */
      energy_use = 100;
   }
}

/*
 * execute a certain spell
 * return TRUE on abortion!
 */
bool exec_spell(s16b spellno)
{
   bool beam;
   s16b dir, plev = p_ptr->lev;

   /* Hack -- chance of "beam" instead of "bolt" */
   beam = ((p_ptr->pclass == 1) ? plev : (plev / 2));

   /* Spells.  */
   switch (spellno)
   {
      case SV_SPELL_MMAGIC_MISSILE:
          if (!get_aim_dir(&dir)) return (TRUE);
          fire_bolt_or_beam(beam-10, GF_MISSILE, dir,
                            damroll(3 + ((plev - 1) / 5), 4));
          break;
      case SV_SPELL_MDETECT_MONSTERS:
          (void)detect_monsters();
          break;
      case SV_SPELL_MPHASE_DOOR:
          teleport_player(10);
          break;
      case SV_SPELL_MLIGHT_AREA:
          (void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
          break;
      case SV_SPELL_MTREASURE_DETECTION:
          (void)detect_treasure();
          break;
      case SV_SPELL_MCURE_LIGHT_WOUNDS:
          (void)hp_player(damroll(2, 8));
          (void)set_cut(p_ptr->cut - 15);
          break;
      case SV_SPELL_MOBJECT_DETECTION:
          (void)detect_object();
          break;
      case SV_SPELL_MFIND_TRAPS_DOORS:
          (void)detect_sdoor();
          (void)detect_trap();
          break;
      case SV_SPELL_MSTINKING_CLOUD:
          if (!get_aim_dir(&dir)) return (TRUE);
          fire_ball(GF_POIS, dir, 10 + (plev / 2), 2);
          break;
      case SV_SPELL_MCONFUSION:
          if (!get_aim_dir(&dir)) return (TRUE);
          (void)confuse_monster(dir, plev);
          break;
      case SV_SPELL_MLIGHTNING_BOLT:
          if (!get_aim_dir(&dir)) return (TRUE);
          fire_bolt_or_beam(beam-10, GF_ELEC, dir,
                            damroll(min(3, 3+(plev-5)/4),8));
          break;
      case SV_SPELL_MTRAPDOOR_DESTRUCT:
          (void)destroy_doors_touch();
          break;
      case SV_SPELL_MSLEEP_I:
          if (!get_aim_dir(&dir)) return (TRUE);
          (void)sleep_monster(dir);
          break;
      case SV_SPELL_MCURE_POISON:
          (void)set_poisoned(0);
          break;
      case SV_SPELL_MTELEPORT_SELF:
          teleport_player(plev * 5);
          break;
      case SV_SPELL_MSPEAR_OF_LIGHT:
          if (!get_aim_dir(&dir)) return (TRUE);
          msg_print("A line of blue shimmering light appears.");
          lite_line(dir);
          break;
      case SV_SPELL_MFROST_BOLT:
          if (!get_aim_dir(&dir)) return (TRUE);
          fire_bolt_or_beam(beam-10, GF_COLD, dir,
                            damroll(min(5, 5+(plev-5)/4),8));
          break;
      case SV_SPELL_MTURN_STONE_TO_MUD:
          if (!get_aim_dir(&dir)) return (TRUE);
          (void)wall_to_mud(dir);
          break;
      case SV_SPELL_MCREATE_FOOD:
          (void)set_food(PY_FOOD_MAX - 1);
          break;
      case SV_SPELL_MRECHARGE_ITEM_I:
          (void)recharge(5);
          break;
      case SV_SPELL_MSLEEP_II:
          (void)sleep_monsters_touch();
          break;
      case SV_SPELL_MPOLYMORPH_OTHER:
          if (!get_aim_dir(&dir)) return (TRUE);
          (void)poly_monster(dir);
          break;
      case SV_SPELL_MIDENTIFY:
          (void)ident_spell();
          break;
      case SV_SPELL_MSLEEP_III:
          (void)sleep_monsters();
          break;
      case SV_SPELL_MFIRE_BOLT:
          if (!get_aim_dir(&dir)) return (TRUE);
          fire_bolt_or_beam(beam, GF_FIRE, dir,
                            damroll(min(8, 8+(plev-5)/4),8));
          break;
      case SV_SPELL_MSLOW_MONSTER:
          if (!get_aim_dir(&dir)) return (TRUE);
          (void)slow_monster(dir);
          break;
      case SV_SPELL_MFROST_BALL:
          if (!get_aim_dir(&dir)) return (TRUE);
          fire_ball(GF_COLD, dir, 30 + plev, 2);
          break;
      case SV_SPELL_MRECHARGE_ITEM_II:
          (void)recharge(40);
          break;
      case SV_SPELL_MTELEPORT_OTHER:
          if (!get_aim_dir(&dir)) return (TRUE);
          (void)teleport_monster(dir);
          break;
      case SV_SPELL_MHASTE_SELF:
          if (!p_ptr->fast)
              (void)set_fast(randint(20) + plev);
          else
              (void)set_fast(p_ptr->fast + randint(5));
          break;
      case SV_SPELL_MFIRE_BALL:
          if (!get_aim_dir(&dir)) return (TRUE);
          fire_ball(GF_FIRE, dir, 55 + plev, 2);
          break;
      case SV_SPELL_MWORD_OF_DESTRUCTION:
          destroy_area(px, py, 15, TRUE);
          break;
      case SV_SPELL_MGENOCIDE_HARD:
          (void)genocide();
          break;
      case SV_SPELL_MRESIST_FIRE:
          (void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + 20);
          break;
      case SV_SPELL_MRESIST_COLD:
          (void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20);
          break;
      case SV_SPELL_MRESIST_ACID:
          (void)set_oppose_acid(p_ptr->oppose_acid + randint(20) + 20);
          break;
      case SV_SPELL_MRESIST_POISON:
          (void)set_oppose_pois(p_ptr->oppose_pois + randint(20) + 20);
          break;
      case SV_SPELL_MRESISTANCE:
          (void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + 20);
          (void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20);
          (void)set_oppose_acid(p_ptr->oppose_acid + randint(20) + 20);
          (void)set_oppose_pois(p_ptr->oppose_pois + randint(20) + 20);
          break;
      case SV_SPELL_MDOOR_CREATION:
          (void)door_creation();
          break;
      case SV_SPELL_MSTAIR_CREATION:
          (void)stair_creation();
          break;
      case SV_SPELL_MTELEPORT_LEVEL:
          (void)teleport_player_level();
          break;
      case SV_SPELL_MEARTHQUAKE:
          earthquake(py, px, 10);
          break;
      case SV_SPELL_MWORD_OF_RECALL:
          if (!p_ptr->word_recall)
          {
              p_ptr->word_recall = rand_int(20) + 15;
              msg_print("The air about you becomes charged...");
          }
          else
          {
              p_ptr->word_recall = 0;
              msg_print("A tension leaves the air around you...");
          }
          break;
      case SV_SPELL_MDETECT_EVIL:
          (void)detect_evil();
          break;
      case SV_SPELL_MDETECT_ENCHANTMENT:
          (void)detect_magic();
          break;
      case SV_SPELL_MRECHARGE_ITEM_III:
          recharge(100);
          break;
      case SV_SPELL_MMASS_GENOCIDE:
          (void)mass_genocide();
          break;
      case SV_SPELL_MHEROISM:
          (void)hp_player(10);
          (void)set_hero(p_ptr->hero + randint(25) + 25);
          (void)set_afraid(0);
          break;
      case SV_SPELL_MSHIELD:
          (void)set_shield(p_ptr->shield + randint(20) + 30);
          break;
      case SV_SPELL_MBERSERKER:
          (void)hp_player(30);
          (void)set_shero(p_ptr->shero + randint(25) + 25);
          (void)set_afraid(0);
          break;
      case SV_SPELL_MESSENCE_OF_SPEED:
          if (!p_ptr->fast)
          {
              (void)set_fast(randint(30) + 30 + plev);
          }
          else
          {
              (void)set_fast(p_ptr->fast + randint(10));
          }
          break;
      case SV_SPELL_MINVULNERABILITY:
          if (p_ptr->invuln)
          {
             msg_print("You feel that you are making a terrible mistake...");
             (void)set_invuln(0);
          }
          else
          {
             (void)set_invuln(2000 + randint(p_ptr->lev * 60));
          }
          break;
      case SV_SPELL_MACID_BOLT:
          if (!get_aim_dir(&dir)) return (TRUE);
          fire_bolt_or_beam(beam, GF_ACID, dir,
                            damroll(min(6, 6+(plev-5)/4), 8));
          break;
      case SV_SPELL_MCLOUDKILL:
          if (!get_aim_dir(&dir)) return (TRUE);
          fire_ball(GF_POIS, dir,
                    20 + (plev / 2), min(3, 3 + (plev-10)/10));
          break;
      case SV_SPELL_MACID_BALL:
          if (!get_aim_dir(&dir)) return (TRUE);
          fire_ball(GF_ACID, dir,
                    40 + (plev), min(2, 2 + (plev-10) / 10));
          break;
      case SV_SPELL_MICE_STORM:
          if (!get_aim_dir(&dir)) return (TRUE);
          fire_ball(GF_COLD, dir,
                    70 + (plev), min(3, 3 + (plev-10) / 10));
          break;
      case SV_SPELL_MMETEOR_SWARM:
          if (!get_aim_dir(&dir)) return (TRUE);
          fire_ball(GF_METEOR, dir,
                    65 + (plev), min(3, 3 + (plev-20) / 10));
          break;
      case SV_SPELL_MHELLFIRE:
          if (!get_aim_dir(&dir)) return (TRUE);
          fire_ball(GF_MANA, dir,
                    300 + (plev * 2), 3 + (plev>35)?1:0);
          break;
      case SV_SPELL_MREFLECTION:
          if (p_ptr->reflecting)
             (void)set_reflecting(randint(10));
          else
             (void)set_reflecting(15+randint(20));
          break;
      case SV_SPELL_MCREATESIMPLETRAP:
          if (!make_trap_from_spell(0, FALSE)) return (TRUE);
          break;
      case SV_SPELL_MCREATEEFFECTIVETRAP:
          if (!make_trap_from_spell(1, FALSE)) return (TRUE);
          break;
      case SV_SPELL_MCREATEDANGEROUSTRAP:
          if (!make_trap_from_spell(2, FALSE)) return (TRUE);
          break;
      case SV_SPELL_MCREATEDEATHTRAP:
          if (!make_trap_from_spell(3, FALSE)) return (TRUE);
          break;
      case SV_SPELL_MCREATECOMPLEXTRAP:
          if (!make_trap_from_spell(3, FALSE)) return (TRUE);
          break;
      case SV_SPELL_PDETECT_EVIL:
          (void)detect_evil();
          break;
      case SV_SPELL_PCURE_LIGHT_WOUNDS:
          (void)hp_player(damroll(2, 10));
          (void)set_cut(p_ptr->cut - 10);
          break;
      case SV_SPELL_PBLESS:
          (void)set_blessed(p_ptr->blessed + randint(12) + 12);
          break;
      case SV_SPELL_PREMOVE_FEAR:
          (void)set_afraid(0);
          break;
      case SV_SPELL_PCALL_LIGHT:
          (void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
          break;
      case SV_SPELL_PFIND_TRAPS:
          (void)detect_trap();
          break;
      case SV_SPELL_PDET_DOORS_STAIRS:
          (void)detect_sdoor();
          break;
      case SV_SPELL_PSLOW_POISON:
          (void)set_poisoned(p_ptr->poisoned / 2);
          break;
      case SV_SPELL_PBLIND_CREATURE:
          if (!get_aim_dir(&dir)) return (TRUE);
          (void)fear_monster(dir, plev);
          break;
      case SV_SPELL_PPORTAL:
          teleport_player(plev * 3);
          break;
      case SV_SPELL_PCURE_MEDIUM_WOUNDS:
          (void)hp_player(damroll(4, 10));
          (void)set_cut((p_ptr->cut / 2) - 20);
          break;
      case SV_SPELL_PCHANT:
          (void)set_blessed(p_ptr->blessed + randint(24) + 24);
          break;
      case SV_SPELL_PSANCTUARY:
          (void)sleep_monsters_touch();
          break;
      case SV_SPELL_PCREATE_FOOD:
          (void)set_food(PY_FOOD_MAX - 1);
          break;
      case SV_SPELL_PREMOVE_CURSE:
          remove_curse();
          break;
      case SV_SPELL_PRESIST_HEAT_COLD:
          (void)set_oppose_fire(p_ptr->oppose_fire + randint(10) + 10);
          (void)set_oppose_cold(p_ptr->oppose_cold + randint(10) + 10);
          break;
      case SV_SPELL_PNEUTRALIZE_POISON:
          (void)set_poisoned(0);
          break;
      case SV_SPELL_PORB_OF_DRAINING:
          if (!get_aim_dir(&dir)) return (TRUE);
          fire_ball(GF_HOLY_ORB, dir,
                    (damroll(3,6) + plev +
                     (plev / ((p_ptr->pclass == 2) ? 2 : 4))),
                    ((plev < 30) ? 2 : 3));
          break;
      case SV_SPELL_PCURE_SER_WOUNDS:
          (void)hp_player(damroll(6, 10));
          (void)set_cut(0);
          break;
      case SV_SPELL_PSENSE_INVISIBLE:
          (void)set_tim_invis(p_ptr->tim_invis + randint(24) + 24);
          break;
      case SV_SPELL_PPROT_FROM_EVIL:
          (void)set_protevil(p_ptr->protevil + randint(25) + 3 * p_ptr->lev);
          break;
      case SV_SPELL_PEARTHQUAKE:
          earthquake(py, px, 10);
          break;
      case SV_SPELL_PSENSE_SURROUNDINGS:
          map_area();
          break;
      case SV_SPELL_PCURE_CRIT_WOUNDS:
          (void)hp_player(damroll(8, 10));
          (void)set_stun(0);
          (void)set_cut(0);
          break;
      case SV_SPELL_PTURN_UNDEAD:
          (void)turn_undead();
          break;
      case SV_SPELL_PPRAYER:
          (void)set_blessed(p_ptr->blessed + randint(48) + 48);
          break;
      case SV_SPELL_PDISPEL_UNDEAD_SMALL:
          (void)dispel_undead(plev * 3);
          break;
      case SV_SPELL_PHEAL:
          (void)hp_player(300);
          (void)set_stun(0);
          (void)set_cut(0);
          break;
      case SV_SPELL_PDISPEL_EVIL:
          (void)dispel_evil(plev * 3);
          break;
      case SV_SPELL_PGLYPH_OF_WARDING:
          warding_glyph();
          break;
      case SV_SPELL_PHOLY_WORD:
          (void)dispel_evil(plev * 4);
          (void)hp_player(1000);
          (void)set_afraid(0);
          (void)set_poisoned(0);
          (void)set_stun(0);
          (void)set_cut(0);
          break;
      case SV_SPELL_PBLINK:
          teleport_player(10);
          break;
      case SV_SPELL_PTELEPORT:
          teleport_player(plev * 8);
          break;
      case SV_SPELL_PTELEPORT_AWAY:
          if (!get_aim_dir(&dir)) return (TRUE);
          (void)teleport_monster(dir);
          break;
      case SV_SPELL_PTELEPORT_LEVEL:
          (void)teleport_player_level();
          break;
      case SV_SPELL_PALTER_REALITY:
          msg_print("The world changes!");
          new_level_flag = TRUE;
          break;
      case SV_SPELL_PDETECT_MONSTERS:
          (void)detect_monsters();
          break;
      case SV_SPELL_PDETECTION:
          (void)detection();
          break;
      case SV_SPELL_PPERCEPTION:
          (void)ident_spell();
          break;
      case SV_SPELL_PPROBING:
          (void)probing();
          break;
      case SV_SPELL_PCLAIRVOYANCE:
          wiz_lite();
          break;
      case SV_SPELL_PCURE_SER_WOUNDS_EASY:
          (void)hp_player(damroll(4, 10));
          (void)set_cut(0);
          break;
      case SV_SPELL_PCURE_CRIT_WOUNDS_EASY:
          (void)hp_player(damroll(8, 10));
          (void)set_stun(0);
          (void)set_cut(0);
          break;
      case SV_SPELL_PHEALING:
          (void)hp_player(2000);
          (void)set_stun(0);
          (void)set_cut(0);
          break;
      case SV_SPELL_PRESTORATION:
          (void)do_res_stat(A_STR);
          (void)do_res_stat(A_INT);
          (void)do_res_stat(A_WIS);
          (void)do_res_stat(A_DEX);
          (void)do_res_stat(A_CON);
          (void)do_res_stat(A_CHR);
          break;
      case SV_SPELL_PREMEMBRANCE:
          (void)restore_level();
          break;
      case SV_SPELL_PUNBARRING_WAYS:
          (void)destroy_doors_touch();
          break;
      case SV_SPELL_PRECHARGING:
          (void)recharge(15);
          break;
      case SV_SPELL_PDISPEL_CURSE:
          (void)remove_all_curse();
          break;
      case SV_SPELL_PENCHANT_WEAPON:
/* jk - *enchant* effects */
          (void)enchant_spell(100+rand_int(4) + 1, 100+rand_int(4) + 1, 0);
          break;
      case SV_SPELL_PENCHANT_ARMOUR:
          (void)enchant_spell(0, 0, 100+rand_int(3) + 2);
          break;
      case SV_SPELL_PELEMENTAL_BRAND:
          brand_weapon();
          break;
      case SV_SPELL_PDISPEL_UNDEAD_LARGE:
          (void)dispel_undead(plev * 4);
          break;
      case SV_SPELL_PDISPEL_EVIL_LARGE:
          (void)dispel_evil(plev * 4);
          break;
      case SV_SPELL_PBANISHMENT:
          if (banish_evil(100))
          {
              msg_print("The power of your god banishes evil!");
          }
          break;
      case SV_SPELL_PWORD_OF_DESTRUCTION:
          destroy_area(px, py, 15, TRUE);
          break;
      case SV_SPELL_PANNIHILATION:
          if (!get_aim_dir(&dir)) return (TRUE);
          drain_life(dir, 200);
          break;

      default: msg_format("Unknown page spell no %d", spellno);
               break;
   }
   return (FALSE);
}

/*
 * this function penalizes you for reading a spell
 * from a wrong class
 */
static void penalize_spell(void)
{
   switch(randint(10))
   {
      /* in 50% of cases, lose all mana */
      case 1:
      case 2:
      case 3:
      case 4:
      case 5:  msg_print("You feel numb and drained.");
               p_ptr->csp = 0;
               p_ptr->csp_frac = 0;
               break;
      /* in 20% of cases, induce hallucination */
      case 6:
      case 7:  msg_print("You're not sure of anything anymore.");
               set_image(p_ptr->image + rand_int(50) + 50);
               break;
      /* in 20% of cases, induce slowness and blindness */
      case 8:
      case 9:  msg_print("Your eyes burn and you feel an odd calm.");
               set_slow(p_ptr->slow + randint(30) + 30);
               set_blind(p_ptr->blind + randint(30) + 30);
               break;
      /* in 10% of cases, reduce the stat */
      case 10: if (p_ptr->pclass != CLASS_HIGHPRST)
               {
                  /* are we an INT user? */
                  if (bit_class(p_ptr->pclass) & MAGE_MAGIC_CLASS)
                  {
                     (void)do_dec_stat(A_INT, STAT_DEC_NORMAL);
                  }
                  else
                  {
                     (void)do_dec_stat(A_WIS, STAT_DEC_NORMAL);
                  }
               }
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
   s16b chance, plev, i;
   bool highprst = (p_ptr->pclass == CLASS_HIGHPRST);
   bool wrong_class = FALSE, aborted = FALSE;

   plev = p_ptr->lev;

dlog(DEBUGITEMS,"cmd5.c: exec_page: starting, spellno %d\n", spellno);
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
dlog(DEBUGITEMS,"cmd5.c: exec_page: spellno %d chance %d\n", spellno, chance);

   /* reading from a book is tested elsewhere, so this is stricly for direct */
   /* reading a spell */
   if ( (!highprst) &&
        (!(s_info[spellno].sclass & (1L<<p_ptr->pclass))))
   {
      /* not for your class? Take a penalty! */
      chance = chance / 2;
      wrong_class = TRUE;
   }

   /* Failed spell */
   i = rand_int(100);
dlog(DEBUGITEMS,"cmd5.c: exec_page: i %d chance %d\n", i, chance);
   if (i < chance)
   {
       if (flush_failure) flush();
       msg_print("You failed to get the spell off!");
   }

   /* Process spell */
   else
   {
      s16b sqrtlev=0, factor;

dlog(DEBUGITEMS,"cmd5.c: exec_page: calling exec_spell\n");
      aborted = exec_spell(spellno);
dlog(DEBUGITEMS,"cmd5.c: exec_page: exec_spell called\n");
      if (aborted) return (FALSE);

      /* now gain some experience: first time, 100%, second time 33%, then 11% etc. */

      while ( (sqrtlev*sqrtlev) < s_info[spellno].level) sqrtlev++;
      factor = 1 + (s_info[spellno].numcast << 3);
      gain_exp( (sqrtlev * s_info[spellno].mana * (s_info[spellno].scale + 1)) / factor);
      s_info[spellno].numcast++;
      if ((s_info[spellno].numcast == (10 * s_info[spellno].level)) && (! cheat_spell_info) )
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

   /* Over-exert the player */
   else
   {
      s16b oops = spell_mana(spellno) - p_ptr->csp;

      /* No mana left */
      p_ptr->csp = 0;
      p_ptr->csp_frac = 0;

      /* Message */
      msg_print("You faint from the effort!");

      /* Hack -- Bypass free action */
      (void)set_paralyzed(p_ptr->paralyzed + randint(5 * oops + 1));

      /* Damage CON (possibly permanently) */
      if (rand_int(100) < 50)
      {
         s16b stat_dec_mode;
         if (rand_int(100) < 25)
         {
            stat_dec_mode = STAT_DEC_PERMANENT;
         }
         else
         {
            stat_dec_mode = STAT_DEC_NORMAL;
         }

         /* Message */
         msg_print("You have damaged your health!");

         /* Reduce constitution */

         (void)dec_stat(A_CON, 15 + randint(10), stat_dec_mode);
      }
   }
   if (wrong_class)
   {
      penalize_spell();
   }
   p_ptr->redraw1 |= (PR1_MANA);
dlog(DEBUGITEMS,"cmd5.c: exec_page: returning\n");

   return (TRUE);
}

static bool add_spell(object_type *i_ptr, s16b spellno)
{
   s16b count = 0, i;
   for (i=0; i<MAX_SPELLS_PER_ITEM; i++)
   {
      if (has_spell(i_ptr, i)) count++;
   }
dlog(DEBUGITEMS,"cmd5.c: add_spell: i_ptr->spell_set %d spellno %d spells existing %d\n", i_ptr->spell_set, spellno, count);

   switch (i_ptr->sval)
   {
      case SV_BOOK_SMALL : if (count==4)
                           {
                              msg_print("This book is full");
                              return FALSE;
                           }
      case SV_BOOK_AVG: if (count==8)
                           {
                              msg_print("This book is full");
                              return FALSE;
                           }
      case SV_BOOK_LARGE : if (count==12)
                           {
                              msg_print("This book is full");
                              return FALSE;
                           }
   }
dlog(DEBUGITEMS,"cmd5.c: add_spell: calling set_spell (spell %d)\n", spellno);
   set_spell(i_ptr, spellno);
   return(TRUE);
}

bool item_tester_hook_book(object_type *i_ptr)
{
   s16b count = 0, i;

   if (i_ptr->tval != TV_BOOK) return (FALSE);

   for (i=0; i<MAX_SPELLS_PER_ITEM; i++)
   {
      if (has_spell(i_ptr, i)) count++;
   }

   if ( ((i_ptr->sval == SV_BOOK_SMALL) && (count==4)) ||
        ((i_ptr->sval == SV_BOOK_AVG) && (count==8)) ||
        ((i_ptr->sval == SV_BOOK_LARGE) && (count==12)))
   {
      return (FALSE);
   }

   return (TRUE);
}

void read_spell(object_type *i_ptr, s16b item)
{
   object_type *j_ptr;
   s16b        itemb, level;
   s16b        amt = 1;
   bool        destroyed = FALSE;
   s16b        chance, i, j, cnt = 0;
   s16b        index[MAX_SPELLS_PER_ITEM];
   bool        done, found;

   static s16b        was_reading = FALSE;
   static object_type *k_ptr;
   static s16b        itemr;

   if ((p_ptr->pclass == CLASS_WARRIOR) || (p_ptr->pclass == CLASS_GLADIATR))
   {
      msg_print("You don't know how to read this!");
      return;
   }

   if (p_ptr->reading == 0)
   {
      if (was_reading)
      {
         msg_print("You know nothing more than before about this spell.");
         p_ptr->redraw2 |= (PR2_READ);
         redraw_stuff();
         was_reading = FALSE;

         return;
      }
      if (i_ptr == NULL)
      {
         dlog(DEBUGALWAYS,"cmd5.c: read_spell: called with i_ptr == NULL, p_ptr->reading %d was_reading %d\n",
                          p_ptr->reading, was_reading);
         dump_stack(NULL);
         msg_print("Something went wrong - read the log");
         return;
      }
      level = s_info[i_ptr->sval].level;

      /* level + randint(2*level) means 1-42 + randint(2-84) */
      chance = level + randint(2*level);
dlog(DEBUGITEMS,"cmd5.c: read_spell aware %d known %d chance %d p_ptr->lev %d\n",
          object_aware_p(i_ptr), object_known_p(i_ptr), chance, p_ptr->lev);
      if (!object_aware_p(i_ptr) && !object_known_p(i_ptr) &&
          (level+randint(2*level)>p_ptr->lev) )
      {
         msg_print("You can't figure out which side is up!");
         return;
      }
      else
      {
         if (!object_aware_p(i_ptr) && !object_known_p(i_ptr))
         {
            msg_print("You figured out the use of this spell!");
         }

         /* The item was tried */
         object_tried(i_ptr);

         /* An identification was made */
         if (!object_aware_p(i_ptr))
         {
            object_aware(i_ptr);
            gain_exp((level + (p_ptr->lev >> 1)) / p_ptr->lev);
         }
      }

      /* p_ptr->lev/2 0 to 25                                  */
      /* skill_dev^2 / 10 gives 0 to 12                        */
      /* rand_int(p_ptr->skill_dev) gives 0 to 11              */
      /* scroll levels range from 0 to 45                      */
      chance=p_ptr->lev/2 + (p_ptr->skill_dev*p_ptr->skill_dev)/360 +
          randint(p_ptr->skill_dev/6);
dlog(DEBUGITEMS,"cmd5.c: read_spell chance %d level %d\n", chance, level);
      if (p_ptr->lev/2 + (p_ptr->skill_dev*p_ptr->skill_dev)/360 +
          randint(p_ptr->skill_dev/6) <level)
      {
         switch(randint(10))
         {
            case  1: msg_print("You don't understand these runes.");
                     break;
            case  2: msg_print("You feel dizzy.");
                     if (!p_ptr->resist_conf)
                        set_confused(p_ptr->confused + rand_int(10) + 10);
                     break;
            case  3: msg_print("Your head spins.");
                     set_stun(p_ptr->stun + rand_int(25) + 10);
                     break;
            case  4: msg_print("You feel very dizzy.");
                     if (!p_ptr->resist_conf)
                        set_confused(p_ptr->confused + rand_int(25) + 25);
                     break;
            case  5: msg_print("The runes fade away while reading.");
                     destroyed = TRUE;
                     break;
            case  6: msg_print("You feel something strange happening.");
                     teleport_player(10);
                     break;
            case  7: msg_print("The meaning seems to slip away.");
                     set_sliding(p_ptr->sliding + randint(10) + 10);
                     break;
            case  8: msg_print("You cut your fingers on the sharp edge of the spell.");
                     set_cut(p_ptr->cut + randint(25) + 25);
                     break;
            case  9: msg_print("The runes cloud your mind.");
                     (void)unlite_area(10, 3);
                     if (!p_ptr->resist_blind)
                        set_blind(p_ptr->blind + 3 + randint(5));
                     break;
            case 10: msg_print("The ink seems poisonous.");
                     if (!(p_ptr->resist_pois || p_ptr->oppose_pois))
                        set_poisoned(p_ptr->poisoned + rand_int(10) + 10);
                     break;
         }
         if (destroyed)
         {
            item_increase(item, -1, px, py);
            item_describe(item, px, py);
            item_optimize(item, px, py);
         }
         return;
      }
      if (get_check("You can read this spell now or meditate on it. Read now?"))
      {
         if (exec_page(i_ptr->sval))
         {
            item_increase(item, -1, i_ptr->ix, i_ptr->iy);
            item_describe(item, i_ptr->ix, i_ptr->iy);
            item_optimize(item, i_ptr->ix, i_ptr->iy);
         }
         return;
      }
      chance = level*4 + randint(8*level);
      msg_format("You start to meditate on this spell.", chance);
dlog(DEBUGITEMS,"cmd5.c: read_spell: meditating on spell item %d spellno %d\n",
                itemr, i_ptr->sval);
      was_reading = TRUE;
      max_reading = chance;
      p_ptr->reading = chance;
      k_ptr = i_ptr;
      itemr = item;
      return;
   }
   else /* p_ptr->reading==1; */
   {
      bool priest = ( (p_ptr->pclass == CLASS_PRIEST) ||
                      (p_ptr->pclass == CLASS_PALADIN));
      bool highprst = (p_ptr->pclass == CLASS_HIGHPRST);

      i_ptr = k_ptr;
      item = itemr;
dlog(DEBUGITEMS,"cmd5.c: read_spell: meditated on spell item %d spellno %d\n",
                itemr, i_ptr->sval);

      p_ptr->redraw2 |= (PR2_READ);
      redraw_stuff();

      /* we can't read it if it's for the wrong class */
      if ( (!highprst) &&
           (!(s_info[i_ptr->sval].sclass & (1L<<p_ptr->pclass))))
      {
         msg_format("This %s is totally alien to you.",
                    priest?"prayer":"spell");
         return;
      }

      msg_format("You are enlightened about this %s of %s.",
                 (s_info[i_ptr->sval].sclass&CLASS_PRIEST)?"prayer":"spell",
                 s_name + s_info[i_ptr->sval].name);

      /* did we read a spell we already knew? */
      found = FALSE;
      for (i=0; i<INVEN_PACK; i++)
      {
         if (inventory[i].tval == TV_BOOK)
         {
            for (j=0; j<s_number; j++)
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
         if (index[i]==i_ptr->sval) found = TRUE;
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
         if (!get_item(&itemb, &amt, "Bind it in what? ", FALSE, TRUE, TRUE))
         {
            item_tester_hook = NULL;
            if (itemb == -2)
            {
               msg_print("You cannot bind it in anything - your memories of this spell fade.");
               msg_print(NULL);
            }

            return;
         }
         item_tester_hook = NULL;

         /* Get the item (in the pack) */
         j_ptr=get_item_pointer(itemb);
 
         /* if the book is stacked, take special care and split the stack  */
         /* note that books with spell-sets don't stack, even if they hold */
         /* the same spells                                                */
         if (j_ptr->number>1)
         {
            object_type tmp_obj;
            tmp_obj = *j_ptr;
            tmp_obj.number = j_ptr->number - 1;
            j_ptr->number = 1;
            done=(add_spell(j_ptr, i_ptr->sval)!=-1);
            /* now destroy the original spell */
            if (done)
            {
               char i_name[240];
 
               item_increase(item, -amt, px, py);
               item_optimize(item, px, py);
              
               object_desc(i_name, &tmp_obj, FALSE, 0);
               msg_format("You unstack %s.", i_name);

               (void)inven_carry(&tmp_obj,tmp_obj.number);
            }
            else
            {
               /* something went wrong, reset the stack to its original size */
               j_ptr->number += tmp_obj.number;
            }
         }
         else
         {

dlog(DEBUGITEMS,"cmd5.c: read_spell: adding spell %d to book %d\n", i_ptr->sval, itemb);
            done=(add_spell(j_ptr, i_ptr->sval)!=-1);
            /* now destroy the original spell */
            item_increase(item, -amt, px, py);
            item_optimize(item, px, py);
         }
      }
   }
}

