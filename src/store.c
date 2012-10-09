/* File: store.c */

/* Purpose: Store commands */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

s16b current_store = 0;

#define MAX_COMMENT_1   10

static cptr comment_1[MAX_COMMENT_1] =
{
   "Okay.",
   "Fine.",
   "Accepted!",
   "Agreed!",
   "Done!",
   "Taken!",
   "Deal!",
   "D'Accord!",
   "But of course. Anything else?",
   "Oh all right."
};

#define MAX_COMMENT_2A  6

static cptr comment_2a[MAX_COMMENT_2A] =
{
   "You try my patience.  %s is final.",
   "My patience grows thin.  %s is final.",
   "No way. %s is final.",
   "You're mad - %s is final.",
   "Think again - %s is my final offer.",
   "My patience is finite. %s will do."
};

#define MAX_COMMENT_2B  13

static cptr comment_2b[MAX_COMMENT_2B] =
{
   "I can take no less than %s gold pieces.",
   "I will accept no less than %s gold pieces.",
   "Ha!  No less than %s gold pieces.",
   "You knave!  No less than %s gold pieces.",
   "That's a pittance!  I want %s gold pieces.",
   "That's an insult!  I want %s gold pieces.",
   "As if!  How about %s gold pieces?",
   "My arse!  How about %s gold pieces?",
   "May the fleas of 1000 orcs molest you!  Try %s gold pieces.",
   "May your most favourite parts go moldy!  Try %s gold pieces.",
   "May Morgoth find you tasty!  Perhaps %s gold pieces?",
   "Your mother was an Ogre!  Perhaps %s gold pieces?",
   "May you meet a cult of Troll Priests! I'm saying %s gold pieces."
};
#define MAX_COMMENT_3A  5

static cptr comment_3a[MAX_COMMENT_3A] =
{
   "You try my patience.  %s is final.",
   "My patience grows thin.  %s is final.",
   "My patience is ending. %s is final.",
   "I'm running out of patience. %s is final.",
   "I don't want to repeat myself: %s is final."
};

#define MAX_COMMENT_3B  15

static cptr comment_3b[MAX_COMMENT_3B] =
{
   "Perhaps %s gold pieces?",
   "How about %s gold pieces?",
   "I will pay no more than %s gold pieces.",
   "I can afford no more than %s gold pieces.",
   "Be reasonable.  How about %s gold pieces?",
   "I'll buy it as scrap for %s gold pieces.",
   "That is too much!  How about %s gold pieces?",
   "That looks war surplus!  Say %s gold pieces?",
   "Never!  %s is more like it.",
   "That's an insult!  %s is more like it.",
   "%s gold pieces and be thankful for it!",
   "%s gold pieces and not a copper more!",
   "That old junk? No more than %s gold pieces.",
   "My grandmother had one of those! %s gold pieces.",
   "Be serious. %s gold pieces is a good price!"
};

#define MAX_COMMENT_4A  5

static cptr comment_4a[MAX_COMMENT_4A] =
{
   "Enough!  You have abused me once too often!",
   "Arghhh!  I have had enough abuse for one day!",
   "That does it!  You shall waste my time no more!",
   "This is getting nowhere!  I'm going to Londis!",
   "I have better things to do than haggle all day!"
};

#define MAX_COMMENT_4B  6

static cptr comment_4b[MAX_COMMENT_4B] =
{
   "Leave my store!",
   "Get out of my sight!",
   "Begone, you scoundrel!",
   "Out, out, out!",
   "You! Leave immediately!",
   "Sorry, we only serve sentient beings."
};

#define MAX_COMMENT_5   10

static cptr comment_5[MAX_COMMENT_5] =
{
   "Try again.",
   "Ridiculous!",
   "You will have to do better than that!",
   "Do you wish to do business or not?",
   "You've got to be kidding!",
   "You'd better be kidding!",
   "You try my patience.",
   "Hmmm, nice weather we're having.",
   "Hey, I'm a shopkeeper, not a nitwit!",
   "Try another store, with a senile owner!"
};

#define MAX_COMMENT_6   5

static cptr comment_6[MAX_COMMENT_6] =
{
   "I must have heard you wrong.",
   "I'm sorry, I missed that.",
   "I'm sorry, what was that?",
   "Sorry, what was that again?",
   "Did you really say what I think you said?"
};

/*
 * Successful haggle.
 */
static void say_comment_1(void)
{
   msg_print(comment_1[rand_int(MAX_COMMENT_1)]);
}

/*
 * Continue haggling (player is buying)
 */
static void say_comment_2(s32b value, s16b annoyed)
{
   char tmp_val[80];

   /* Prepare a string to insert */
   sprintf(tmp_val, "%ld", (long)value);

   /* Final offer */
   if (annoyed > 0)
   {
      /* Formatted message */
      msg_format(comment_2a[rand_int(MAX_COMMENT_2A)], tmp_val);
   }

   /* Normal offer */
   else
   {
      /* Formatted message */
      msg_format(comment_2b[rand_int(MAX_COMMENT_2B)], tmp_val);
   }
}

/*
 * Continue haggling (player is selling)
 */
static void say_comment_3(s32b value, s16b annoyed)
{
   char tmp_val[80];

   /* Prepare a string to insert */
   sprintf(tmp_val, "%ld", (long)value);

   /* Final offer */
   if (annoyed > 0)
   {
      /* Formatted message */
      msg_format(comment_3a[rand_int(MAX_COMMENT_3A)], tmp_val);
   }

   /* Normal offer */
   else
   {
      /* Formatted message */
      msg_format(comment_3b[rand_int(MAX_COMMENT_3B)], tmp_val);
   }
}

/*
 * Kick 'da bum out.                                    -RAK-
 */
static void say_comment_4(void)
{
   msg_print(comment_4a[rand_int(MAX_COMMENT_4A)]);
   msg_print(comment_4b[rand_int(MAX_COMMENT_4B)]);
}

/*
 * You are insulting me
 */
static void say_comment_5(void)
{
   msg_print(comment_5[rand_int(MAX_COMMENT_5)]);
}

/*
 * That makes no sense.
 */
static void say_comment_6(void)
{
   msg_print(comment_6[rand_int(5)]);
}

/*
 * Messages for reacting to purchase prices.
 */

#define MAX_COMMENT_7A  4

static cptr comment_7a[MAX_COMMENT_7A] =
{
   "Arrgghh!",
   "You bastard!",
   "You hear someone sobbing...",
   "The shopkeeper howls in agony!"
};

#define MAX_COMMENT_7B  5

static cptr comment_7b[MAX_COMMENT_7B] =
{
   "Damn!",
   "You fiend!",
   "The shopkeeper curses at you.",
   "The shopkeeper glares at you.",
   "Blast. Cursed wares!"
};

#define MAX_COMMENT_7C  4

static cptr comment_7c[MAX_COMMENT_7C] =
{
   "Cool!",
   "You've made my day!",
   "The shopkeeper giggles.",
   "The shopkeeper laughs loudly."
};

#define MAX_COMMENT_7D  5

static cptr comment_7d[MAX_COMMENT_7D] =
{
   "Yipee!",
   "I think I'll retire!",
   "The shopkeeper jumps for joy.",
   "The shopkeeper smiles gleefully.",
   "The shopkeeper seems delighted."
};

#define MAX_SWAP_COMMENT 4

static cptr swap_comment[MAX_SWAP_COMMENT] =
{
   "Let's swap your %s (%c) for my %s.",
   "Swap your %s (%c) for this wonderful %s?",
   "Trade your %s (%c) for my %s?",
   "I'll take your %s (%c) in payment for my %s."
};

#define MAX_SWAP_COMMENT2 4

static cptr swap_comment2[MAX_SWAP_COMMENT2] =
{
   "Great!",
   "All right!",
   "Good deal, eh?",
   "Glad to be rid of that!"
};

#define MAX_SWAP_COMMENT_STORE 4

static cptr swap_comment_store[MAX_SWAP_COMMENT_STORE] =
{
   "I offer %s in payment for %s.",
   "Trade %s against your %s?",
   "Can I pay with %s for your %s?",
   "Swap this wonderfull %s for your %s?"
};

/*
 * what flavor is a store of?
 * this is needed to browse the owner-table etc.
 */
s16b store_flavor_type(s16b type)
{
   if (type<=DUNG_ENTR_HOME)
   {
      return(type);
   }
   else
   {
      if ((type >= DUNG_ENTR_ARM_STRT) && (type <= DUNG_ENTR_ARM_END)) return (DUNG_ENTR_ARMOR);
      if ((type >= DUNG_ENTR_WPN_STRT) && (type <= DUNG_ENTR_WPN_END)) return (DUNG_ENTR_WEAPON);
      if ((type >= DUNG_ENTR_MAG_STRT) && (type <= DUNG_ENTR_MAG_END)) return (DUNG_ENTR_MAGIC);
      if ((type >= DUNG_ENTR_TPL_STRT) && (type <= DUNG_ENTR_TPL_END)) return (DUNG_ENTR_TEMPLE);
      if ((type >= DUNG_ENTR_ALC_STRT) && (type <= DUNG_ENTR_ALC_END)) return (DUNG_ENTR_ALCHEM);
      if ((type >= DUNG_ENTR_GNR_STRT) && (type <= DUNG_ENTR_GNR_END)) return (DUNG_ENTR_GENERL);
            return (-1);
   }
}

static s16b store_flavor(s16b which)
{
   store_type *st_ptr = &store[which];
dlog(DEBUGSTORE,"store.c: store_flavor: which %d st_ptr %08lx\n", which, st_ptr);

   return store_flavor_type(st_ptr->store_type);
}

/*
 * make a quick check if a certain tval may be acceptable
 * before the whole cpu-intensive actions like apply_magic
 * are done
 */
static bool quick_check_store_tval(s16b which, s16b tval)
{
   s16b        i, store_tval;
   store_type *st_ptr = &store[which];

   for (i=0; store_accepts[st_ptr->store_type][i].tval; i++)
   {
      store_tval = store_accepts[st_ptr->store_type][i].tval;

      /* tval -1 means store buys anything */
      if ( (store_tval == -1) || (store_tval == tval))
      {
         return (TRUE);
      }
   }
   return (FALSE);
}

/*
 * Determine if a store will purchase the given item
 *
 * Note that a shop-keeper must refuse to buy "worthless" items
 */
static bool store_will_buy(s16b which, object_type *i_ptr)
{
   s16b        i;
   s16b        store_tval, store_sval;
   store_type *st_ptr = &store[which];

dlog(DEBUGSTORE,"store.c: store_will_buy: tv %d sv %d pv %d k_idx %d id %04x\n",
                i_ptr->tval, i_ptr->sval, i_ptr->tval, i_ptr->k_idx, i_ptr->ident);

   /* Hack -- The Home is simple */
   if (st_ptr->store_type == DUNG_ENTR_HOME) return (TRUE);

dlog(DEBUGSTORE,"store.c: store_will_buy: value %ld\n",
                object_value(i_ptr));

   /* Ignore "worthless" items */
   if (object_value(i_ptr) <= 0L) return (FALSE);

   for (i=0; store_accepts[st_ptr->store_type][i].tval; i++)
   {
      store_tval = store_accepts[st_ptr->store_type][i].tval;
      store_sval = store_accepts[st_ptr->store_type][i].sval;

      /* tval -1 means store buys anything */
      if (store_tval == -1)
      {
         return(TRUE);
      }
      if (store_tval == i_ptr->tval)
      {
         /* sval -1 means store buys any article with this tval */
         if (store_sval == -1)
         {
            return(TRUE);
         }
         /* sval -2 means store buys any article with priestly flavor */
         /* these have to be spells */
         if (store_sval == -2)
         {
dlog(DEBUGSTORE,"store.c: store_will_buy: store_sval -2 s_info %08x sval %d\n",
                s_info[i_ptr->sval].sclass, i_ptr->sval);
            if ((s_info[i_ptr->sval].sclass & BIT_CLASS_PRIEST) ||
                (s_info[i_ptr->sval].sclass & BIT_CLASS_PALADIN))
            {
               return (TRUE);
            }
            else
               return (FALSE);
         }
         /* sval -3 means store buys any article with mage flavor */
         /* these have to be spells */
         if (store_sval == -3)
         {
dlog(DEBUGSTORE,"store.c: store_will_buy: store_sval -3 s_info %08x sval %d\n",
                s_info[i_ptr->sval].sclass, i_ptr->sval);
            if ((s_info[i_ptr->sval].sclass & BIT_CLASS_ROGUE) ||
                (s_info[i_ptr->sval].sclass & BIT_CLASS_MAGE) ||
                (s_info[i_ptr->sval].sclass & BIT_CLASS_RANGER))
            {
               return(TRUE);
            }
            else
               return(FALSE);
         }
         /* sval -4 means only accept weapons that priests can carry without */
         /* penalty */
         if (store_sval == -4)
         {
            if ( (i_ptr->name2==EGO_BLESS_BLADE) ||
                 (i_ptr->name2==EGO_HA) )
            {
               return (TRUE);
            }
            else
               return (FALSE);
         }
         /* else compare normal svals */
         if (store_sval == i_ptr->sval)
         {
            return(TRUE);
         }
      }
   }

   /* Assume okay */
   return (FALSE);
}

/*
 * Determine if a store item can "absorb" another item
 *
 * See "object_similar()" for the same function for the "player"
 */
static bool store_object_similar(object_type *i_ptr, object_type *j_ptr)
{
   /* Hack -- Identical items cannot be stacked */
   if (i_ptr == j_ptr)
   {
dlog(DEBUGSTORE,"store.c: store_object_similar: same pointer\n");
      return (0);
   }

   /* Different objects cannot be stacked */
   if (i_ptr->k_idx != j_ptr->k_idx)
   {
dlog(DEBUGSTORE,"store.c: store_object_similar: k_idx differs\n");
      return (0);
   }

   /* Different charges (etc) cannot be stacked */
   /* else shops would have to stock too many items */
   if (i_ptr->p1val != j_ptr->p1val)
   {
dlog(DEBUGSTORE,"store.c: store_object_similar: p1val differs\n");
      return (0);
   }

   /* Require many identical values */
   if (i_ptr->to_h  !=  j_ptr->to_h)
   {
dlog(DEBUGSTORE,"store.c: store_object_similar: to_h differs\n");
      return (0);
   }
   if (i_ptr->to_d  !=  j_ptr->to_d)
   {
dlog(DEBUGSTORE,"store.c: store_object_similar: to_d differs\n");
      return (0);
   }
   if (i_ptr->to_a  !=  j_ptr->to_a)
   {
dlog(DEBUGSTORE,"store.c: store_object_similar: to_a differs\n");
      return (0);
   }

   /* Require identical "artifact" names */
   if (i_ptr->name1 != j_ptr->name1)
   {
dlog(DEBUGSTORE,"store.c: store_object_similar: name1 differs\n");
      return (0);
   }

   /* Require identical "ego-item" names */
   if (i_ptr->name2 != j_ptr->name2)
   {
dlog(DEBUGSTORE,"store.c: store_object_similar: name2 differs\n");
      return (0);
   }

   /* Hack -- Never stack "powerful" items */
   if (i_ptr->xtra1 || j_ptr->xtra1)
   {
dlog(DEBUGSTORE,"store.c: store_object_similar: xtra1 differs\n");
      return (0);
   }

   /* Hack -- Never stack recharging items */
   if (i_ptr->timeout || j_ptr->timeout)
   {
dlog(DEBUGSTORE,"store.c: store_object_similar: timeout differs\n");
      return (0);
   }

   /* Require many identical values */
   if (i_ptr->ac    !=  j_ptr->ac)
   {
dlog(DEBUGSTORE,"store.c: store_object_similar: ac differs\n");
      return (0);
   }
   if (i_ptr->dd    !=  j_ptr->dd)
   {
dlog(DEBUGSTORE,"store.c: store_object_similar: dd differs\n");
      return (0);
   }
   if (i_ptr->ds    !=  j_ptr->ds)
   {
dlog(DEBUGSTORE,"store.c: store_object_similar: ds differs\n");
      return (0);
   }

   /* Hack -- Never stack chests */
   if (i_ptr->tval == TV_CHEST) return (0);

   /* Require matching discounts */
   if (i_ptr->discount != j_ptr->discount)
   {
dlog(DEBUGSTORE,"store.c: store_object_similar: discount differs\n");
      return (0);
   }

   /* They match, so they must be similar */
   return (TRUE);
}

/*
 * Allow a store item to absorb another item
 */
static void store_object_absorb(object_type *i_ptr, object_type *j_ptr)
{
   s16b total = i_ptr->number + j_ptr->number;

   /* Combine quantity, lose excess items */
   i_ptr->number = (total > 99) ? 99 : total;
dlog(DEBUGSTORE,"store.c: store_object_absorb: total now %d\n",
                i_ptr->number);
}

/*
 * Add the item "i_ptr" to a real stores inventory.
 *
 * If the item is "worthless", it is thrown away (except in the home).
 *
 * If the item cannot be combined with an object already in the inventory,
 * make a new slot for it, and calculate its "per item" price.  Note that
 * this price will be negative, since the price will not be "fixed" yet.
 * Adding an item to a "fixed" price stack will not change the fixed price.
 *
 * In all cases, return the slot (or -1) where the object was placed
 */
static s16b store_carry(s16b which, object_type *i_ptr)
{
   s16b         i, slot;
   s32b         value, j_value;
   object_type *j_ptr;
   /* if I use st_ptr instead of referring to store[which], i get SIGSEGV */
   /* errors in this routine when it's called often. cause unknown        */
   /* store_type  *st_ptr = &store[which]; */

   /* Evaluate the object */
   value = object_value(i_ptr);

   /* Cursed/Worthless items "disappear" when sold */
   if (value <= 0)
   {
dlog(DEBUGSTORE,"store.c: store_carry: crap, returning -1\n");
      return (-1);
   }

   /* Erase the inscription */
   i_ptr->note = 0;

   /* rods are magically charged when a store buys them */
   if ((i_ptr->tval == TV_ROD) && (i_ptr->p1val > 0))
   {
      i_ptr->p1val = 0;
   }

   /* Check each existing item (try to combine) */
   for (slot = 0; slot < store[which].stock_num; slot++)
   {
      /* Get the existing item */
      j_ptr = &store[which].stock[slot];

      /* Can the existing items be incremented? */
      if (store_object_similar(j_ptr, i_ptr))
      {
         /* Hack -- extra items disappear */
         store_object_absorb(j_ptr, i_ptr);

         /* All done */
dlog(DEBUGSTORE,"store.c: store_carry: returning %d after similar\n", slot);
         return (slot);
      }
   }

   /* No space left for one more item? */
   if (store[which].stock_num > (STORE_INVEN_MAX-1))
   {
dlog(DEBUGSTORE,"store.c: store_carry: full, returning -1\n");
      return (-1);
   }

   /* Check existing slots to see if we must "slide" */
   for (slot = 0; slot < store[which].stock_num; slot++)
   {
      /* Get that item */
      j_ptr = &store[which].stock[slot];

      /* Objects sort by decreasing type */
      if (i_ptr->tval > j_ptr->tval) break;
      if (i_ptr->tval < j_ptr->tval) continue;

      /* Objects sort by increasing sval */
      if (i_ptr->sval < j_ptr->sval) break;
      if (i_ptr->sval > j_ptr->sval) continue;

      /* Evaluate that slot */
      j_value = object_value(j_ptr);

      /* Objects sort by decreasing value */
      if (value > j_value) break;
      if (value < j_value) continue;
   }
dlog(DEBUGSTORE,"store.c: store_carry: sliding from %d down to %d\n", store[which].stock_num, slot);

   /* Slide the others up */
   for (i = store[which].stock_num; i > slot; i--)
   {
      store[which].stock[i] = store[which].stock[i-1];
   }

   /* More stuff now */
   store[which].stock_num++;

   /* Insert the new item */
   store[which].stock[slot] = *i_ptr;
   /* Return the location */
dlog(DEBUGSTORE,"store.c: store_carry: returning %d after insert\n", slot);

   return (slot);
}

/*
 * This function will keep 'crap' out of the black market.
 * Crap is defined as any item that is "available" elsewhere
 * Based on a suggestion by "Lee Vogt" <lvogt@cig.mcel.mot.com>
 */
static bool black_market_crap(object_type *i_ptr)
{
   s16b         i, j;

   /* Ego items are never crap */
   if (i_ptr->name2) return (FALSE);

   /* Good items are never crap */
   if (i_ptr->to_a > 0) return (FALSE);
   if (i_ptr->to_h > 0) return (FALSE);
   if (i_ptr->to_d > 0) return (FALSE);

   /* Check the other "normal" stores */
   for (i = 0; i < 6; i++)
   {
      /* Check every item in the store */
      for (j = 0; j < store[i].stock_num; j++)
      {
         object_type *j_ptr = &store[i].stock[j];

         /* Duplicate item "type", assume crappy */
         if (i_ptr->k_idx == j_ptr->k_idx) return (TRUE);
      }
   }

   /* Assume okay */
   return (FALSE);
}

/*
 * Special "mass production" computation
 */
static s16b mass_roll(s16b num, s16b max)
{
   s16b i, t = 0;
   for (i = 0; i < num; i++) t += rand_int(max);
   return (t);
}

/*
 * Certain "cheap" objects should be created in "piles"
 * Some objects can be sold at a "discount" (in small piles)
 */
static void mass_produce(object_type *i_ptr)
{
   s16b size = 1;
   s16b discount = 0;

   s32b cost = object_value(i_ptr);

   /* Analyze the type */
   switch (i_ptr->tval)
   {
      /* Food, Flasks, and Lites */
      case TV_FOOD:
      case TV_FLASK:
      case TV_LITE:
         if (cost <= 5L) size += mass_roll(3,5);
         if (cost <= 20L) size += mass_roll(3,5);
         break;

      case TV_POTION:
      case TV_SCROLL:
         if (cost <= 60L) size += mass_roll(3,5);
         if (cost <= 240L) size += mass_roll(1,5);
         break;

      case TV_SOFT_ARMOR:
      case TV_HARD_ARMOR:
      case TV_SHIELD:
      case TV_GLOVES:
      case TV_BOOTS:
      case TV_CLOAK:
      case TV_HELM:
      case TV_CROWN:
      case TV_SWORD:
      case TV_POLEARM:
      case TV_HAFTED:
      case TV_DIGGING:
      case TV_BOW:
         if (i_ptr->name2) break;
         if (cost <= 10L) size += mass_roll(3,5);
         if (cost <= 100L) size += mass_roll(3,5);
         break;

      case TV_SPIKE:
      case TV_SHOT:
      case TV_ARROW:
      case TV_BOLT:
         if (cost <= 5L) size += mass_roll(5,5);
         if (cost <= 50L) size += mass_roll(5,5);
         if (cost <= 500L) size += mass_roll(5,5);
         break;
   }

   /* Pick a discount */
   if (cost < 5)
   {
      discount = 0;
   }
   else if (rand_int(25) == 0)
   {
      discount = 25;
   }
   else if (rand_int(150) == 0)
   {
      discount = 50;
   }
   else if (rand_int(300) == 0)
   {
      discount = 75;
   }
   else if (rand_int(500) == 0)
   {
      discount = 90;
   }

   /* Save the discount */
   i_ptr->discount = discount;

   /* Save the total pile size */
   i_ptr->number = size - (size * discount / 100);
}

/*
 * Create one item in a store's stock
 * we return the number of tries it took us to get an item
 */
static s16b store_create_item(s16b which)
{
   s16b        k_idx, tval, sval, level, slot;
   u16b        cnt;
   bool        ok = FALSE;
   object_type object;
   object_type *i_ptr;
   store_type *st_ptr = &store[which];

/* jk - now enables higher level items to creep in if your character is */
/* high enough level */
   level = STORE_OBJ_LEVEL +
           rand_range(1, STORE_OBJ_LEVEL+rand_int(p_ptr->lev/2)) +
           p_ptr->lev / 2;

/* jk - higher level items for the black market and specialty stores! */
   if (st_ptr->store_type >= DUNG_ENTR_MARKET)
   {
      level = (2*level)+10;
      if (level>MAX_LEVEL) level = MAX_LEVEL;
   }

   ok = FALSE;
   cnt = 0;
   while (!ok)
   {
      cnt++;
      if (cnt == STORE_ITEM_TRIES) break;

      k_idx = get_obj_num(level);
      /* we fail sometimes, but much more often not */
      /* so just try again if it doesn't work */
      if (!k_idx) continue;
      tval = k_info[k_idx].tval;

      /* make a quick check if this tval may be acceptable after processing */
      if (!quick_check_store_tval(which, tval)) continue;

      sval = k_info[k_idx].sval;
/* jk - no chests in stores */
      if (tval == TV_CHEST) continue;

      invcopy (&object, k_idx);
dlog(DEBUGSTORE,"store.c: store_create_item: 1 k_idx %d @ %d,%d tv %d sv %d pv %ld %s\n",
     object.k_idx,object.ix,object.iy,object.tval,object.sval,object.p1val,
     k_name+k_info[object.k_idx].name);

      apply_magic(&object, level, (p_ptr->mdepth>10), (p_ptr->mdepth>50), (p_ptr->mdepth>100));

      /* no artifacts please */
      if (artifact_p(&object)) 
      {
dlog(DEBUGALWAYS,"store.c: store_create_item: created artifact %d (%s), rejected, wiping\n",
                 object.name1, a_name + a_info[object.name1].name);
         a_info[object.name1].cur_num = 0;
         continue;
      }

      /* if it isn't ok for this store, try again */
      if (!store_will_buy(which, &object)) continue;

      /* The item is "known" */
      object_known(&object);

      /* Prune the black market */
      if (st_ptr->store_type == DUNG_ENTR_MARKET)
      {
         /* Hack -- No "crappy" items */
         if (black_market_crap(&object)) continue;
      }

      /* Mass produce and/or Apply discount */
      mass_produce(&object);
      ok = TRUE; /* this is a good object */
   }

   /* we lost.... */
   if (cnt==STORE_ITEM_TRIES) return (cnt);

   /* Hack -- Charge lite's */
   if (object.tval == TV_LITE)
   {
      if (object.sval == SV_LITE_TORCH) object.p1val = FUEL_TORCH / randint(3);
      if (object.sval == SV_LITE_LANTERN) object.p1val = FUEL_LAMP / randint(3);
   }
dlog(DEBUGSTORE,"store.c: store_create_item: 2 k_idx %d @ %d,%d tv %d sv %d pv %ld disc %d num %d wgt %d\n",
     object.k_idx,object.ix,object.iy,object.tval,object.sval,object.p1val,
     object.discount,object.number,object.weight);
dlog(DEBUGSTORE,"store.c: store_create_item: 2 n1 %d n2 %d tim %d toh %d tod %d toa %d ac %d dd %d ds %d\n",
     object.name1,object.name2,object.timeout,object.to_h,object.to_d,object.to_a,
     object.ac,object.dd,object.ds);
dlog(DEBUGSTORE,"store.c: store_create_item: 2 ident %d mark %d xtra1 %d xtra2 %ld\n",
     object.ident, object.marked, object.xtra1, object.xtra2);
   i_ptr = &object;
   slot = store_carry(which, i_ptr);
dlog(DEBUGSTORE,"store.c: store_create_item: slot %d\n", slot);
   return (cnt);
}

/*
 * Create inventory for store
 */
void store_create_stock(s16b which)
{
   store_type *st_ptr = &store[which];
   s16b wanted = (STORE_INVEN_MAX/2) + rand_int(STORE_INVEN_MAX / 2);
   s16b cnt;

dlog(DEBUGSTORE,"store.c: store_create_stock: start store %d, wanting %d\n", which, wanted);
   /* jk - don't create things in the home... */
   if (st_ptr->store_type != DUNG_ENTR_HOME)
   {
      while (st_ptr->stock_num < wanted)
      {
         /* did we have much trouble creating this item? */
         cnt = store_create_item(which);

         /* if so, create less items! */
         if (cnt > (STORE_ITEM_TRIES/2))
         {
            wanted--;
            if (wanted<0) wanted = 0;
         }
         /* it is possible to create an item and not increase st_ptr->stock_num */
         /* when we create an item that can be absorbed into another            */
         /* prevent this by once in a while decreasing wanted                   */
         if (randint(3)==1) wanted--;
dlog(DEBUGSTORE,"store.c: store_create_stock: item created, wanted now %d\n", wanted);
      }
   }
}

/*
 * Create store in town
 */
void create_town_store(s16b which, s16b x, s16b y)
{
   store_type *st_ptr;

dlog((DEBUGSTORE | DEBUGGENER),"store.c: town_create_store: entering num_stores %d at %d,%d which %d\n",
                                            num_stores, x, y, which);

   if (num_stores==MAX_STORES_LEVEL)
      quit("Creating too many stores!");

   if ( (which != DUNG_ENTR_HOME) && (num_stores == 0) )
      quit("store.c: create_town_store: attemting to overwrite home");

   st_ptr = &store[num_stores];

   /* clean everything */
   store_init(num_stores);

   /* we make type 'which' here */
   st_ptr->store_type = which;
dlog(DEBUGSTORE, "store.c: town_create_store: store %d type %d created\n", num_stores, which);

   /* and it needs to have some stuff in it */

   store_create_stock(num_stores);
dlog(DEBUGSTORE,"store.c: town_create_store: stock created\n");
   /* we are placing it here on the map */
   st_ptr->sx = x;
   st_ptr->sy = y;
   (void)set_grid_type(x, y, DUNG_ENTR,
                       DUNG_ENTR_GSTART+st_ptr->store_type, GRID_KEEP, 0);
   dungeon.level[sublevel][y][x].extra = num_stores;
dlog((DEBUGSTORE | DEBUGGENER),"store.c: town_create_store: created store, extra %d grid mtyp %d styp %d @ %d,%d\n",
           dungeon.level[sublevel][y][x].extra, dungeon.level[sublevel][y][x].mtyp, dungeon.level[sublevel][y][x].styp, x, y);

   /* now let the caller know what store we created */
   num_stores++;
}

/*
 * Create store
 */
void create_store(s16b type, s16b x, s16b y)
{
   store_type *st_ptr;

dlog((DEBUGSTORE|DEBUGGENER),"store.c: create_store entering num_stores %d at %d,%d\n", num_stores, x, y);

   if (num_stores==MAX_STORES_LEVEL)
      quit("Creating too many stores!");

   if ( (type != DUNG_ENTR_HOME) && (num_stores == 0) )
      quit("store.c: create_town_store: attemting to overwrite home");

   st_ptr = &store[num_stores];

dlog((DEBUGSTORE|DEBUGGENER),"store.c: create_store at %d,%d\n", x, y);

   store_init(num_stores);
dlog(DEBUGSTORE,"store.c: create_store type %d\n", type);

   st_ptr->store_type = type;
   store_create_stock(num_stores);
   st_ptr->sx = x;
   st_ptr->sy = y;
   dungeon.level[sublevel][y][x].extra = num_stores;
   /* make entrance door */
   (void)set_grid_type(x, y, DUNG_ENTR, type, GRID_KEEP, 0);

dlog((DEBUGSTORE|DEBUGGENER),"store.c: create_store created store, extra %d grid mtyp %d styp %d\n",
           dungeon.level[sublevel][y][x].extra, dungeon.level[sublevel][y][x].mtyp, dungeon.level[sublevel][y][x].styp);

   num_stores++;
}

/*
 * Let a shop-keeper React to a purchase
 *
 * We paid "price", it was worth "value", and we thought it was worth "guess"
 */
static void purchase_analyze(s32b price, s32b value, s32b guess)
{
   /* Item was worthless, but we bought it */
   if ((value <= 0) && (price > value))
   {
      msg_print(comment_7a[rand_int(MAX_COMMENT_7A)]);
   }

   /* Item was cheaper than we thought, and we paid more than necessary */
   else if ((value < guess) && (price > value))
   {
      msg_print(comment_7b[rand_int(MAX_COMMENT_7B)]);
   }

   /* Item was a good bargain, and we got away with it */
   else if ((value > guess) && (value < (4 * guess)) && (price < value))
   {
      msg_print(comment_7c[rand_int(MAX_COMMENT_7C)]);
   }

   /* Item was a great bargain, and we got away with it */
   else if ((value > guess) && (price < value))
   {
      msg_print(comment_7d[rand_int(MAX_COMMENT_7D)]);
   }
}

/*
 * Buying and selling adjustments for race combinations.
 * Entry[owner][player] gives the basic "cost inflation".
 */
static byte rgold_adj[MAX_RACES][MAX_RACES] =
{
                        /*Hum, HfE, Elf,  Hal, Gno, Dwa, HfO, HfT, Dun, HiE, Drue */

/* Human        */      { 100, 105, 105, 110, 113, 115, 120, 125, 100, 105,  100},
/* Half-Elf     */      { 110, 100, 100, 105, 110, 120, 125, 130, 110, 100,  100},
/* Elf          */      { 110, 105, 100, 105, 110, 120, 125, 130, 110, 100,  100},
/* Halfling     */      { 115, 110, 105,  95, 105, 110, 115, 130, 115, 105,  100},
/* Gnome        */      { 115, 115, 110, 105,  95, 110, 115, 130, 115, 110,  100},
/* Dwarf        */      { 115, 120, 120, 110, 110,  95, 125, 135, 115, 120,  100},
/* Half-Orc     */      { 115, 120, 125, 115, 115, 130, 110, 115, 115, 125,  125},
/* Half-Troll   */      { 110, 115, 115, 110, 110, 130, 110, 110, 110, 115,  130},
/* Dunedain     */      { 100, 105, 105, 110, 113, 115, 120, 125, 100, 105,  100},
/* High_Elf     */      { 110, 105, 100, 105, 110, 120, 125, 130, 110, 100,  100},
/* Druedain     */      { 100, 100, 100, 100, 100, 100, 125, 130, 100, 100,  100}

};

/*
 * Determine the price of an item (qty one) in a store.
 *
 * This function takes into account the player's charisma, and the
 * shop-keepers friendliness, and the shop-keeper's base greed, but
 * never lets a shop-keeper lose money in a transaction.
 *
 * The "greed" value should exceed 100 when the player is "buying" the
 * item, and should be less than 100 when the player is "selling" it.
 *
 * Hack -- the black market always charges twice as much as it should.
 *
 * Charisma adjustment runs from 80 to 130
 * Racial adjustment runs from 95 to 130
 *
 * Since greed/charisma/racial adjustments are centered at 100, we need
 * to adjust (by 200) to extract a usable multiplier.  Note that the
 * "greed" value is always something (?).
 */
static s32b price_item(s16b which, object_type *i_ptr, s16b greed, bool flip)
{
   s16b    factor;
   s16b    adjust;
   s32b    price;
   store_type *st_ptr = &store[which];
   owner_type *ot_ptr;

   ot_ptr = &owners[store_flavor(which)][st_ptr->owner];

   /* Get the value of one of the items */
   price = object_value(i_ptr);
dlog(DEBUGSTORE,"store.c: price_item: which %d price %ld\n", which, price);
   /* Worthless items are not sold for free :-) */
   if (price <= 0) return (1L);

   /* Compute the racial factor */
   factor = rgold_adj[ot_ptr->owner_race][p_ptr->prace];

   /* Add in the charisma factor */
   factor += adj_chr_gold[p_ptr->stat_ind[A_CHR]];

   /* Shop is buying */
   if (flip)
   {
      /* Adjust for greed */
      adjust = 100 + (300 - (greed + factor));

      /* Never get "silly" */
      if (adjust > 100) adjust = 100;

      /* Mega-Hack -- Black market sucks */
      if (st_ptr->store_type == DUNG_ENTR_MARKET) price = price / 2;
   }

   /* Shop is selling */
   else
   {
      /* Adjust for greed */
      adjust = 100 + ((greed + factor) - 300);

      /* Never get "silly" */
      if (adjust < 100) adjust = 100;

      /* Mega-Hack -- Black market sucks */
      if (st_ptr->store_type == DUNG_ENTR_MARKET) price = price * 2;
   }

   /* Compute the final price (with rounding) */
   price = (price * adjust + 50L) / 100L;
dlog(DEBUGSTORE,"store.c: price_item: which %d price2 %ld\n", which, price);

   /* Note -- Never become "free" */
   if (price <= 0L) return (1L);
dlog(DEBUGSTORE,"store.c: price_item: which %d price3 %ld\n", which, price);

   /* Return the price */
   return (price);
}

/*
 * Check to see if the shop will be carrying too many objects   -RAK-
 * Note that the shop, just like a player, will not accept things
 * it cannot hold.  Before, one could "nuke" potions this way.
 */
static bool store_check_num(s16b which, object_type *i_ptr)
{
   s16b        i;
   object_type *j_ptr;
   store_type  *st_ptr = &store[which];

   /* Free space is always usable */
   if (st_ptr->stock_num < STORE_INVEN_MAX) return TRUE;

   /* The "home" acts like the player */
   if (st_ptr->store_type == DUNG_ENTR_HOME)
   {
      /* Check all the items */
      for (i = 0; i < st_ptr->stock_num; i++)
      {
         /* Get the existing item */
         j_ptr = &st_ptr->stock[i];

         /* Can the new object be combined with the old one? */
         if (object_similar(j_ptr, i_ptr)) return (TRUE);
      }
   }

   /* Normal stores do special stuff */
   else
   {
      /* Check all the items */
      for (i = 0; i < st_ptr->stock_num; i++)
      {
         /* Get the existing item */
         j_ptr = &st_ptr->stock[i];

         /* Can the new object be combined with the old one? */
         if (store_object_similar(j_ptr, i_ptr)) return (TRUE);
      }
   }

   /* But there was no room at the inn... */
   return (FALSE);
}

/*
 * Add the item "i_ptr" to the inventory of the "Home"
 *
 * In all cases, return the slot (or -1) where the object was placed
 *
 * Note that this is a hacked up version of "inven_carry()".
 *
 * Also note that it may not correctly "adapt" to "knowledge" becoming
 * known, the player may have to pick stuff up and drop it again.
 */
static s16b home_carry(s16b which, object_type *i_ptr)
{
   s16b               slot;
   s32b               value, j_value;
   s16b               i;
   object_type *j_ptr;
   store_type *st_ptr = &store[which];

   /* Check each existing item (try to combine) */
   for (slot = 0; slot < st_ptr->stock_num; slot++)
   {
      /* Get the existing item */
      j_ptr = &st_ptr->stock[slot];

      /* The home acts just like the player */
      if (object_similar(j_ptr, i_ptr))
      {
         /* Save the new number of items */
         object_absorb(j_ptr, i_ptr,i_ptr->number);

         /* All done */
         return (slot);
      }
   }

   /* No space? */
   if (st_ptr->stock_num >= STORE_INVEN_MAX) return (-1);

   /* Determine the "value" of the item */
   value = object_value(i_ptr);

   /* Check existing slots to see if we must "slide" */
   for (slot = 0; slot < st_ptr->stock_num; slot++)
   {
      /* Get that item */
      j_ptr = &st_ptr->stock[slot];

      /* Objects sort by decreasing type */
      if (i_ptr->tval > j_ptr->tval) break;
      if (i_ptr->tval < j_ptr->tval) continue;

      /* Can happen in the home */
      if (!object_aware_p(i_ptr)) continue;
      if (!object_aware_p(j_ptr)) break;

      /* Objects sort by increasing sval */
      if (i_ptr->sval < j_ptr->sval) break;
      if (i_ptr->sval > j_ptr->sval) continue;

      /* Objects in the home can be unknown */
      if (!object_known_p(i_ptr)) continue;
      if (!object_known_p(j_ptr)) break;

      /* Objects sort by decreasing value */
      j_value = object_value(j_ptr);
      if (value > j_value) break;
      if (value < j_value) continue;
   }

   /* Slide the others up */
   for (i = st_ptr->stock_num; i > slot; i--)
   {
      st_ptr->stock[i] = st_ptr->stock[i-1];
   }

   /* More stuff now */
   st_ptr->stock_num++;

   /* Insert the new item */
   st_ptr->stock[slot] = *i_ptr;

   /* Return the location */
   return (slot);
}

/*
 * Increase, by a given amount, the number of a certain item
 * in a certain store.  This can result in zero items.
 */
static void store_item_increase(s16b which, s16b item, s16b num)
{
   object_type *i_ptr;
   store_type  *st_ptr = &store[which];

   /* Get the item */
   i_ptr = &st_ptr->stock[item];

   /* Verify the number */
   if ((i_ptr->number + num) > 99)
   {
      num = 99 - i_ptr->number;
   }
   else if ((i_ptr->number + num) < 0)
   {
      num = 0 - i_ptr->number;
   }

   /* if we decrease the number of rods, take care of the maximum recharge-time */
   if ((i_ptr->tval == TV_ROD) && (i_ptr->p1val>0))
   {
      i_ptr->p1val = max(i_ptr->number * get_rod_charge(i_ptr), i_ptr->p1val);
   }

   /* if we decrease the number of charged staffs/wands, take care of the maximum charge */
   if ( ( (i_ptr->tval == TV_WAND) || (i_ptr->tval == TV_STAFF) ) && (i_ptr->p1val>0) && (num < 0) )
   {
      /* note: the factor 2 is necessary for rounding - give the player the benefit of the doubt */
      s16b new_charge = 2 * (i_ptr->p1val * (i_ptr->number + num) ) / i_ptr->number;
      i_ptr->p1val = (new_charge/2) + (new_charge & 1);
   }
dlog(DEBUGITEMS,"object4.c: store_item_increase: 2 current num %d, change %d, total %d\n", i_ptr->number, num, i_ptr->number+num);

   /* Save the new number */
   i_ptr->number += num;
}

/*
 * Remove a slot if it is empty
 */
static void store_item_optimize(s16b which, s16b item)
{
   s16b         j;
   object_type *i_ptr;
   store_type  *st_ptr = &store[which];

   /* Get the item */
   i_ptr = &st_ptr->stock[item];

   /* Must exist */
   if (!i_ptr->k_idx) return;

   /* Must have no items */
   if (i_ptr->number) return;

   /* if this was an artifact, remove it with care */
   if (i_ptr->name1)
   {
      a_info[i_ptr->name1].cur_num = 99;
   }

   /* One less item */
   st_ptr->stock_num--;

   /* Slide everyone */
   for (j = item; j < st_ptr->stock_num; j++)
   {
      st_ptr->stock[j] = st_ptr->stock[j + 1];
   }

   /* Nuke the final slot */
   invwipe(&st_ptr->stock[st_ptr->stock_num]);
}

/*
 * Attempt to delete (some of) a random item from the store
 * Hack -- we attempt to "maintain" piles of items when possible.
 */
static void store_delete_item(s16b which)
{
   s16b what, num;
   store_type  *st_ptr = &store[which];

   /* Pick a random slot */
   what = rand_int(st_ptr->stock_num);

   /* Determine how many items are here */
   num = st_ptr->stock[what].number;

   /* Hack -- sometimes, only destroy half the items */
   if (rand_int(100) < 50) num = (num + 1) / 2;

   /* Hack -- sometimes, only destroy a single item */
   if (rand_int(100) < 50) num = 1;

   /* Actually destroy (part of) the item */
   store_item_increase(which, what, -num);
   store_item_optimize(which, what);
}

/*
 * checks if all the necessary items for this store are there
 */
static bool check_store_necessary_items(s16b which)
{
   store_type         *st_ptr = &store[which];
   object_type        *i_ptr;
   s16b                i = 0, j, sval, tval;
   bool                ok = TRUE, found;

   /* check all necessary items */
   for (j=0; store_accepts[st_ptr->store_type][j].tval; j++)
   {
      tval = store_accepts[st_ptr->store_type][j].tval;
      sval = store_accepts[st_ptr->store_type][j].sval;

      /* don't bother if this item doesn't matter */
      if (store_accepts[st_ptr->store_type][j].chance == 0) continue;

      found = FALSE;
      /* Check all the items in store */
      for (i = 0; i < st_ptr->stock_num; i++)
      {
         /* Get the existing item */
         i_ptr = &st_ptr->stock[i];

         /* is it the necessary item? */
         if ( (tval == -1) ||
              ( (i_ptr->tval == tval) && ( (sval == -1) || (i_ptr->sval == sval) ) ) )
         {
            found = TRUE;
         }
         /* sval -2 means store buys any article with priestly flavor */
         /* these have to be spells */
         if ( (tval == i_ptr->tval) && (sval == -2) )
         {
            if ( (s_info[i_ptr->sval].sclass & BIT_CLASS_PRIEST) ||
                 (s_info[i_ptr->sval].sclass & BIT_CLASS_PALADIN) )
            {
               found = TRUE;
            }
         }
         /* sval -3 means store buys any article with mage flavor */
         /* these have to be spells */
         if ( (tval == i_ptr->tval) && (sval == -3) )
         {
            if ( (s_info[i_ptr->sval].sclass & BIT_CLASS_ROGUE) ||
                 (s_info[i_ptr->sval].sclass & BIT_CLASS_MAGE) ||
                 (s_info[i_ptr->sval].sclass & BIT_CLASS_RANGER) )
            {
               found = TRUE;
            }
         }
         /* sval -4 means only accept weapons that priests can carry without */
         /* penalty */
         if ( (sval == -4) &&
              ( (i_ptr->name2==EGO_BLESS_BLADE) || (i_ptr->name2==EGO_HA) ) )
         {
            found = TRUE;
         }
      }
      if ( (found == FALSE) && (rand_int(100) < store_accepts[st_ptr->store_type][j].chance) )
      {
         ok = FALSE;
      }
   }
   return (ok);
}

/*
 * Creates a random item and gives it to a store
 *
 */
static void store_add_inventory(s16b which, s16b num)
{
   s16b                tries, cnt;
   store_type         *st_ptr = &store[which];

dlog(DEBUGSTORE,"store.c: store_add_inventory: which %d stock_num %d max %d\n",
                which, st_ptr->stock_num, STORE_INVEN_MAX);

   /* Paranoia -- no room left */
   if (st_ptr->stock_num >= STORE_INVEN_MAX)
   {
      return;
   }

   /* consider some items */
   for (tries = 0; tries < num; tries++)
   {
      cnt = store_create_item(which);
dlog(DEBUGSTORE,"store.c: store_add_inventory: item %d cost %d tries\n", tries, cnt);
      /* if so, create less items! */
      if ( (!good_store_items) && (cnt > (STORE_ITEM_TRIES/2)) )
      {
         tries++;
dlog(DEBUGSTORE,"store.c: store_add_inventory: increasing item to %d\n", tries);
      }
   }
}

/*
 * Eliminate need to bargain if player has haggled well in the past
 */
static bool noneedtobargain(s16b which, s32b minprice)
{
   store_type *st_ptr = &store[which];
   s32b good = st_ptr->good_buy;
   s32b bad = st_ptr->bad_buy;

   /* Cheap items are "boring" */
   if (minprice < 10L) return (TRUE);

   /* Perfect haggling */
   if (good == MAX_SHORT) return (TRUE);

   /* Reward good haggles, punish bad haggles, notice price */
   if (good > ((3 * bad) + (5 + (minprice/50)))) return (TRUE);

   /* Return the flag */
   return (FALSE);
}


/*
 * Update the bargain info
 */
static void updatebargain(s16b which, s32b price, s32b minprice)
{
   store_type *st_ptr = &store[which];
   /* Allow haggling to be turned off */
   if (no_haggle_flag) return;

   /* Cheap items are "boring" */
   if (minprice < 10L) return;

   /* Count the successful haggles */
   if (price == minprice)
   {
      /* Just count the good haggles */
      if (st_ptr->good_buy < MAX_SHORT)
      {
         st_ptr->good_buy++;
      }
   }

   /* Count the failed haggles */
   else
   {
      /* Just count the bad haggles */
      if (st_ptr->bad_buy < MAX_SHORT)
      {
         st_ptr->bad_buy++;
      }
   }
}

/*
 * Re-displays a single store entry
 */
static void display_entry(s16b which, s16b pos)
{
   s16b                i;
   object_type        *i_ptr;
   s32b                x;
   store_type         *st_ptr = &store[which];
   char                i_name[80];
   char                out_val[160];
   owner_type         *ot_ptr;
   s16b maxwid = 75;

   ot_ptr = &owners[store_flavor(which)][st_ptr->owner];


   /* Get the item */
   i_ptr = &st_ptr->stock[pos];

   /* Get the "offset" */
   i = (pos % 12);

   /* Label it, clear the line --(-- */
   (void)sprintf(out_val, "%c) ", I2A(i));
   prt(out_val, 0, i+6);

   /* Describe an item in the home */
   if (st_ptr->store_type == DUNG_ENTR_HOME)
   {
      maxwid = 75;

      /* Leave room for weights, if necessary -DRS- */
      if (show_inven_weight) maxwid -= 10;

      /* Describe the object */
      object_desc(i_name, i_ptr, TRUE, 3);
      i_name[maxwid] = '\0';
      c_put_str(tval_to_attr[i_ptr->tval], i_name, 3, i + 6);

      /* Show weights, if turned on -DRS- */
      if (show_inven_weight)
      {
         /* Only show the weight of an individual item */
         s16b wgt = i_ptr->weight;
         (void)sprintf(out_val, "%3d.%d lb", wgt / 10, wgt % 10);
         put_str(out_val, 68, i + 6);
      }
   }

   /* Describe an item (fully) in a store */
   else
   {
      /* Must leave room for the "price" */
      maxwid = 65;

      /* Leave room for weights, if necessary -DRS- */
      if (show_store_weight) maxwid -= 7;

      /* Describe the object (fully) */
      object_desc_store(i_name, i_ptr, TRUE, 3);
      i_name[maxwid] = '\0';
      c_put_str(tval_to_attr[i_ptr->tval], i_name, 3, i+6);

      /* Show weights, if turned on -DRS- */
      if (show_store_weight)
      {
         /* Only show the weight of an individual item */
         s16b wgt = i_ptr->weight;
         (void)sprintf(out_val, "%3d.%d", wgt / 10, wgt % 10);
         put_str(out_val, 61, i + 6);
      }
dlog(DEBUGSTORE,"store.c: display_entry: which %d pos %d name %s price %ld\n",
                which, pos, i_name, (long)price_item(which, i_ptr, ot_ptr->min_inflate, FALSE));
      /* Display a "fixed" cost */
      if (i_ptr->ident & ID_FIXED)
      {
         /* Extract the "minimum" price */
         x = price_item(which, i_ptr, ot_ptr->min_inflate, FALSE);

         /* Actually draw the price (not fixed) */
         (void)sprintf(out_val, "%9ld F", (long)x);
         put_str(out_val, 68, i + 6);
      }

      /* Display a "taxed" cost */
      else if (no_haggle_flag)
      {
         /* Extract the "minimum" price */
         x = price_item(which, i_ptr, ot_ptr->min_inflate, FALSE);

         /* Hack -- Apply Sales Tax if needed */
         if (!noneedtobargain(which, x)) x += x / 10;

         /* Actually draw the price (with tax) */
         (void)sprintf(out_val, "%9ld  ", (long)x);
         put_str(out_val, 68, i + 6);
      }

      /* Display a "haggle" cost */
      else
      {
         /* Extrect the "maximum" price */
         x = price_item(which, i_ptr, ot_ptr->max_inflate, FALSE);

         /* Actually draw the price (not fixed) */
         (void)sprintf(out_val, "%9ld  ", (long)x);
         put_str(out_val, 68, i + 6);
      }
   }
}

/*
 * Displays a store's inventory                 -RAK-
 * All prices are listed as "per individual object".  -BEN-
 */
static s16b display_inventory(s16b which, s16b page)
{
   s16b       i, k;
   store_type *st_ptr = &store[which];

   /* Display the next 12 items */
   for (k = 0; k < 12; k++)
   {
      /* Do not display "dead" items */
      if ((page * 12) + k >= st_ptr->stock_num) break;

      /* Display that line */
      display_entry(which, k + (12 * page));
   }

   /* Erase the extra lines and the "more" prompt */
   for (i = k; i < 13; i++) prt("", 0, i + 6);

   /* Assume "no current page" */
   put_str("             ", 5, 20);

   /* Visual reminder of "more items" */
   if (st_ptr->stock_num > 12)
   {
      /* Indicate the "current page" */
      put_str(format("(Page %1d of %1d)", page + 1, (st_ptr->stock_num+11)/12), 5, 20);
dlog(DEBUGSTORE,"store.c: display_inventory: page %d stock_num %d\n", page, st_ptr->stock_num);
   }
   return (page);
}

/*
 * Displays players gold                                        -RAK-
 */
static void store_prt_gold(void)
{
   char out_val[64];

   prt("Gold Remaining: ", 53, 19);

   sprintf(out_val, "%9ld", (long)p_ptr->au);
   prt(out_val, 68, 19);
}

/*
 * Displays store (after clearing screen)               -RAK-
 */
static void display_store(s16b which, s16b page)
{
   store_type *st_ptr = &store[which];
   char        buf[255];
   owner_type *ot_ptr;

   ot_ptr = &owners[store_flavor(which)][st_ptr->owner];
dlog(DEBUGSTORE,"store.c: display_store: which %d type %d flavor %d\n",
           which, st_ptr->store_type, store_flavor(which));
   /* Erase the screen */
   clear_screen();

   /* The "Home" is special */
   if (st_ptr->store_type == DUNG_ENTR_HOME)
   {
      /* Put the owner name */
      put_str("Your Home", 2, 3);

      /* Label the item descriptions */
      put_str("Item Description", 3, 5);

      /* If showing weights, show label */
      if (show_inven_weight)
      {
         put_str("Weight", 70, 5);
      }
   }

   /* Normal stores */
   else
   {
      cptr store_name = (f_name + f_info[get_f_idx(DUNG_ENTR, st_ptr->store_type)].name);
      cptr owner_name = (ot_ptr->owner_name);
      cptr race_name = race_info[ot_ptr->owner_race].title;

      /* Put the owner name and race */
      sprintf(buf, "%s (%s) - %s", owner_name, race_name, store_name);
      put_str(buf, 2, 3);

      /* Label the item descriptions */
      put_str("Item Description", 3, 5);

      /* If showing weights, show label */
      if (show_store_weight)
      {
          put_str("Weight", 60, 5);
      }

      /* Label the asking price (in stores) */
      put_str("Price", 72, 5);
   }

   /* Display the current gold */
   store_prt_gold();

   /* Draw in the inventory */
   display_inventory(which, page);
}

/*
 * Get the ID of a store item and return its value      -RAK-
 */
static s16b get_stock(s16b *com_val, cptr pmt, s16b i, s16b j)
{
   char        command;

   char        out_val[160];


   /* Paranoia XXX XXX XXX */
   msg_print(NULL);

   /* Assume failure */
   *com_val = (-1);

   /* Build the prompt */
   (void)sprintf(out_val, "(Items %c-%c, ESC to exit) %s",
                 I2A(i), I2A(j), pmt);

   /* Ask until done */
   while (TRUE)
   {
      s16b k;

      /* Escape */
      if (!get_com(out_val, &command)) break;

      /* Convert */
      k = (ang_islower(command) ? A2I(command) : -1);

      /* Legal responses */
      if ((k >= i) && (k <= j))
      {
         *com_val = k;
         break;
      }

      /* Oops */
      bell("Illegal store object choice!");
   }

   /* Clear the prompt */
   prt("", 0, MESSAGE_ROW);

   /* Cancel */
   if (command == ESCAPE) return (FALSE);

   /* Success */
   return (TRUE);
}

/*
 * Increase the insult counter and get angry if too many -RAK-
 */
static s16b increase_insults(s16b which)
{
   store_type *st_ptr = &store[which];

   owner_type *ot_ptr;

   ot_ptr = &owners[store_flavor(which)][st_ptr->owner];

   /* Increase insults */
   st_ptr->insult_cur++;

   /* Become insulted */
   if (st_ptr->insult_cur > ot_ptr->insult_max)
   {
      /* Complain */
      say_comment_4();

      /* Reset insults */
      st_ptr->insult_cur = 0;
      st_ptr->good_buy = 0;
      st_ptr->bad_buy = 0;

      /* Open tomorrow */
      st_ptr->store_open = turn + 25000 + randint(25000);

      /* Closed */
      return (TRUE);
   }

   /* Not closed */
   return (FALSE);
}

/*
 * Decrease insults                                     -RAK-
 */
static void decrease_insults(s16b which)
{
   store_type *st_ptr = &store[which];

   /* Decrease insults */
   if (st_ptr->insult_cur) st_ptr->insult_cur--;
}

/*
 * Have insulted while haggling                         -RAK-
 */
static s16b haggle_insults(s16b which)
{
   /* Increase insults */
   if (increase_insults(which)) return (TRUE);

   /* Display and flush insult */
   say_comment_5();

   /* Still okay */
   return (FALSE);
}

/*
 * Mega-Hack -- Enable "increments"
 */
static bool allow_inc = FALSE;

/*
 * Mega-Hack -- Last "increment" during haggling
 */
static s32b last_inc = 0L;


/*
 * Get a haggle
 */
static s16b get_haggle(cptr pmt, s32b *poffer, s32b price, s16b final)
{
   s32b                i;

   cptr                p;

   char                buf[128];
   char                out_val[160];


   /* Paranoia XXX XXX XXX */
   msg_print(NULL);

   /* Clear old increment if necessary */
   if (!allow_inc) last_inc = 0L;

   /* Final offer, or no increment */
   if (final || !allow_inc || !last_inc)
   {
       sprintf(buf, "%s [accept] ", pmt);
   }

   /* Old (negative) increment, and not final */
   else if (last_inc < 0)
   {
       sprintf(buf, "%s [-%ld] ", pmt, (long)(ABS(last_inc)));
   }

   /* Old (positive) increment, and not final */
   else
   {
       sprintf(buf, "%s [+%ld] ", pmt, (long)(ABS(last_inc)));
   }

   /* Ask until done */
   while (TRUE)
   {
      /* Default */
      strcpy(out_val, "");

      /* Ask the user */
      if (!get_string(buf, out_val, 32)) return (FALSE);

      /* Skip leading spaces */
      for (p = out_val; *p == ' '; p++) ;

      /* Return accepts default */
      if (*p == '\0')
      {
         /* Accept current price */
         if (final || !allow_inc || !last_inc)
         {
            *poffer = price;
            last_inc = 0L;
         }

         /* Use previous increment again */
         else
         {
            *poffer += last_inc;
         }

         /* Done */
         break;
      }

      /* Extract a number */
      i = atol(p);

      /* Handle "incremental" number */
      if ((*p == '+' || *p == '-'))
      {
         /* Allow increments */
         if (allow_inc)
         {
            /* Use the given "increment" */
            *poffer += i;
            last_inc = i;
            break;
         }
      }

      /* Handle normal number */
      else
      {
         /* Use the given "number" */
         *poffer = i;
         last_inc = 0L;
         break;
      }

      /* Warning */
      msg_print("Invalid response.");
   }

   /* Success */
   return (TRUE);
}


/*
 * Receive an offer (from the player)
 *
 * Return TRUE if offer is NOT okay
 */
static bool receive_offer(s16b which, cptr pmt, s32b *poffer,
                          s32b last_offer, s16b factor,
                          s32b price, s16b final)
{
   /* Haggle till done */
   while (TRUE)
   {
      /* Get a haggle (or cancel) */
      if (!get_haggle(pmt, poffer, price, final)) return (TRUE);

      /* Acceptable offer */
      if (((*poffer) * factor) >= (last_offer * factor)) break;

      /* Insult, and check for kicked out */
      if (haggle_insults(which)) return (TRUE);

      /* Reject offer (correctly) */
      (*poffer) = last_offer;
   }

   /* Success */
   return (FALSE);
}

/*
 * try to swap an item for something in the players inventory
 */
static bool check_swap_items_inv(s16b which, s16b store_item,
                                 object_type *i_ptr, s16b *page)
{
   s32b         item_price = object_value(i_ptr);
   s16b         index[INVEN_PACK], cnt = 0, item, i, comment_no;
   char         i_name[80], j_name[80];
   object_type *j_ptr;
   bool         cont;
   char         query;

   if (!inven_carry_okay(i_ptr))
   {
       object_desc_store(i_name, i_ptr, TRUE, 3);
dlog(DEBUGSTORE,"store.c: check_swap_items_inv: %s cannot be carried\n",
       i_name);
       return (FALSE);
   }

   /* Check every item in the pack */
   for (i = 0; i < INVEN_PACK; i++)
   {
      s32b this_price;
      j_ptr = &inventory[i];
      if (!j_ptr->k_idx) continue;

      if (!store_will_buy(which,j_ptr)) continue;

      this_price = object_value(j_ptr);

      if ( (this_price>=(item_price*9)/10) &&
           (this_price<=(item_price*5)/4) )
      {
         index[cnt++]=i;
      }
   }
   /* choose player item */
   if (!cnt) return (FALSE);
   item = index[rand_int(cnt)];
   j_ptr = &inventory[item];
   object_desc(j_name, j_ptr, FALSE, 0x40);

   /* describe shop item */
   object_desc_store(i_name, i_ptr, FALSE, 0x40);

   cont = FALSE;
   comment_no = rand_int(MAX_SWAP_COMMENT);
   while (!cont)
   {
      prt(format("%s (y/n/look)", format(swap_comment[comment_no],
                 j_name, index_to_label(item), i_name)), 0, MESSAGE_ROW);

      query=inkey();
      switch(query)
      {
         case 'Y':
         case 'y':
         {
            object_type store_obj, player_obj;
            s16b store_pos;

            store_obj = *i_ptr;
            player_obj = *j_ptr;

            store_obj.number = 1;
            player_obj.number = 1;

            store_item_increase(which, store_item, -1);
            store_item_optimize(which, store_item);

            item_increase(item, -1, px, py);
            item_optimize(item, px, py);

            object_aware(&store_obj);
            object_tried(&store_obj);
            test_new_object(&store_obj);
            (void)inven_carry(&store_obj, 1);
            store_pos=store_carry(which, &player_obj);

            prt("", 0, MESSAGE_ROW);
            msg_print(swap_comment2[rand_int(MAX_SWAP_COMMENT2)]);
            msg_print(NULL);

            /* display the page where our swapped object went to */
            *page = (store_pos)/12;
            display_inventory(which, *page);

            return (TRUE);
            break;
         }
         case 'L':
         case 'l':
         {
            char tmp_name[80], tmp;
            object_desc(tmp_name, j_ptr, FALSE, 3);
            prt(format("Your %s - more",tmp_name), 0, MESSAGE_ROW);
            tmp=inkey();
            break;
         }
         case 'N':
         case 'n':
            prt("", 0, MESSAGE_ROW);
            cont = TRUE;
            break;
         default:
            bell("Unrecognized key");
      }

   }

   msg_print(NULL);
   return (FALSE);
}

/*
 * try to swap an item for something in the store inventory
 */
static bool check_swap_items_store(s16b which, object_type *i_ptr, s16b *page)
{
   s32b         item_price = object_value(i_ptr);
   s16b         index[STORE_INVEN_MAX], cnt = 0, item, i, comment_no;
   char         i_name[80], j_name[80];
   object_type *j_ptr;
   bool         cont;
   char         query;
   store_type  *st_ptr = &store[which];

   if (!store_will_buy(which, i_ptr))
   {
       object_desc_store(i_name, i_ptr, TRUE, 3);
dlog(DEBUGSTORE,"store.c: check_swap_items_store: %s cannot be bought\n",
       i_name);
       return (FALSE);
   }

   /* Check every item in the store inventory */
   for (i = 0; i < st_ptr->stock_num; i++)
   {
      s32b this_price;
      j_ptr = &st_ptr->stock[i];
      if (!j_ptr->k_idx) continue;

      if (!inven_carry_okay(j_ptr)) continue;

      this_price = object_value(j_ptr);

      if ( (this_price>=(item_price*9)/10) &&
           (this_price<=(item_price*5)/4) )
      {
         index[cnt++]=i;
      }
   }
   /* choose player item */
   if (!cnt) return (FALSE);
   item = index[rand_int(cnt)];
   j_ptr = &st_ptr->stock[item];
   object_desc(j_name, j_ptr, FALSE, 0x40);

   /* describe player item */
   object_desc_store(i_name, i_ptr, FALSE, 0x40);

   cont = FALSE;
   comment_no = rand_int(MAX_SWAP_COMMENT_STORE);
   while (!cont)
   {
      prt(format("%s (y/n/look)", format(swap_comment_store[comment_no],
                 i_name, j_name)), 0, MESSAGE_ROW);

      query=inkey();
      switch(query)
      {
         case 'Y':
         case 'y':
         {
            object_type store_obj, player_obj;
            s16b store_pos;

            player_obj = *i_ptr;
            store_obj = *j_ptr;

            store_obj.number = 1;
            player_obj.number = 1;

            store_item_increase(which, item, -1);
            store_item_optimize(which, item);

            item_increase(item, -1, px, py);
            item_optimize(item, px, py);

            object_aware(&store_obj);
            object_tried(&store_obj);
            test_new_object(&store_obj);
            (void)inven_carry(&store_obj, 1);
            store_pos=store_carry(which, &player_obj);

            prt("", 0, MESSAGE_ROW);
            msg_print(swap_comment2[rand_int(MAX_SWAP_COMMENT2)]);
            msg_print(NULL);

            /* display the page where our swapped object went to */
            *page = (store_pos)/12;
            display_inventory(which, *page);

            return (TRUE);
            break;
         }
         case 'L':
         case 'l':
         {
            char tmp_name[80], tmp;
            object_desc(tmp_name, i_ptr, FALSE, 3);
            prt(format("Your %s - more",tmp_name), 0, MESSAGE_ROW);
            tmp=inkey();
            break;
         }
         case 'N':
         case 'n':
            prt("", 0, MESSAGE_ROW);
            cont = TRUE;
            break;
         default:
            bell("Unrecognized key");
      }

   }

   msg_print(NULL);
   return (FALSE);
}


/*
 * Haggling routine                                     -RAK-
 *
 * Return TRUE if purchase is NOT successful
 */
static bool purchase_haggle(s16b which, s16b store_item, object_type *i_ptr, s32b *price, s16b *page)
{
   s32b        cur_ask, final_ask;
   s32b        last_offer, offer;
   s32b        x1, x2, x3;
   s32b        min_per, max_per;
   s16b        flag, loop_flag, noneed, offer_no = 0;
   s16b        annoyed = 0, final = FALSE;
   bool        cancel = FALSE;
   cptr        pmt = "Asking";
   char        out_val[160];
   owner_type *ot_ptr;
   store_type *st_ptr = &store[which];

   ot_ptr = &owners[store_flavor(which)][st_ptr->owner];

   *price = 0;

   /* Extract the starting offer and the final offer */
   cur_ask = price_item(which, i_ptr, ot_ptr->max_inflate, FALSE);
   final_ask = price_item(which, i_ptr, ot_ptr->min_inflate, FALSE);
   /* Determine if haggling is necessary */
   noneed = noneedtobargain(which, final_ask);

   /* Go right to final price if player has bargained well */
   if (noneed || no_haggle_flag)
   {
      /* No need to haggle */
      if (noneed)
      {
         /* Message summary */
         msg_print("You eventually agree upon the price.");
         msg_print(NULL);
      }

      /* No haggle option */
      else
      {
         /* Message summary */
         msg_print("You quickly agree upon the price.");
         msg_print(NULL);

         /* Apply Sales Tax */
         final_ask += final_ask / 10;
      }

      /* Final price */
      cur_ask = final_ask;

      /* Go to final offer */
      pmt = "Final Offer";
      final = TRUE;
   }

   /* Haggle for the whole pile */
   cur_ask *= i_ptr->number;
   final_ask *= i_ptr->number;

   /* XXX XXX XXX Display commands */

   /* Haggle parameters */
   min_per = ot_ptr->haggle_per;
   max_per = min_per * 3;

   /* Mega-Hack -- artificial "last offer" value */
   last_offer = object_value(i_ptr) * i_ptr->number;
   last_offer = last_offer * (200 - (int)(ot_ptr->max_inflate)) / 100L;
   if (last_offer <= 0) last_offer = 1;

   /* No offer yet */
   offer = 0;

   /* No incremental haggling yet */
   allow_inc = FALSE;

   /* Haggle until done */
   for (flag = FALSE; !flag; )
   {
      loop_flag = TRUE;
      offer_no++;
dlog(DEBUGSTORE,"store.c: purchase_haggle: loop %d cur_ask %ld\n",
                offer_no, cur_ask);

      while (!flag && loop_flag)
      {
         (void)sprintf(out_val, "%s :  %ld", pmt, (long)cur_ask);
         put_str(out_val, 0, 1);
         cancel = receive_offer(which, "What do you offer? ",
                                  &offer, last_offer, 1, cur_ask, final);

dlog(DEBUGSTORE,"store.c: purchase_haggle: offer %ld\n", offer);

         if (cancel)
         {
            flag = TRUE;
         }
         else if (offer > cur_ask)
         {
            say_comment_6();
            offer = last_offer;
         }
         else if (offer == cur_ask)
         {
            flag = TRUE;
            *price = offer;
         }
         else
         {
            loop_flag = FALSE;
         }
      }

dlog(DEBUGSTORE,"store.c: purchase_haggle: flag %d\n", flag);

      if (!flag)
      {
         x1 = 100 * (offer - last_offer) / (cur_ask - last_offer);
         if (x1 < min_per)
         {
            if (haggle_insults(which))
            {
               flag = TRUE;
               cancel = TRUE;
            }
         }
         else if (x1 > max_per)
         {
            x1 = x1 * 3 / 4;
            if (x1 < max_per) x1 = max_per;
         }
         x2 = rand_range(x1-2, x1+2);
         x3 = ((cur_ask - offer) * x2 / 100L) + 1;
         /* don't let the price go up */
         if (x3 < 0) x3 = 0;
         cur_ask -= x3;

         /* we bargained long and hard, now make him an offer */
         /* he can't refuse.... */
         if (offer_no>3 && (randint(2)==1))
         {
dlog(DEBUGSTORE,"store.c: purchase_haggle: calling check_swap_items_inv\n");

            if (check_swap_items_inv(which, store_item, i_ptr, page))
            {
               *price = -1;
               return (FALSE);
            }
         }

         /* Too little */
         if (cur_ask < final_ask)
         {
            final = TRUE;
            cur_ask = final_ask;
            pmt = "Final Offer";
            annoyed++;
            if (annoyed > 3)
            {
               (void)(increase_insults(which));
               cancel = TRUE;
               flag = TRUE;
            }
         }
         else if (offer >= cur_ask)
         {
            flag = TRUE;
            *price = offer;
         }

         if (!flag)
         {
            last_offer = offer;
            allow_inc = TRUE;
            prt("", 0, 1);
            (void)sprintf(out_val, "Your last offer: %ld",
                          (long)last_offer);
            put_str(out_val, 39, 1);
            say_comment_2(cur_ask, annoyed);
         }
      }
   }

   /* Cancel */
   if (cancel) return (TRUE);

   /* Update bargaining info */
   updatebargain(which, *price, final_ask);

   /* Do not cancel */
   return (FALSE);
}

/*
 * Haggling routine                                     -RAK-
 *
 * Return TRUE if purchase is NOT successful
 */
static bool sell_haggle(s16b which, object_type *i_ptr, s32b *price, s16b *page)
{
   s32b               purse, cur_ask, final_ask;
   s32b               last_offer = 0, offer = 0;
   s32b               x1, x2, x3;
   s32b               min_per, max_per;

   s16b               flag, loop_flag, noneed, offer_no = 0;
   s16b               annoyed = 0, final = FALSE;

   bool               cancel = FALSE;

   cptr               pmt = "Offer";

   char               out_val[160];

   owner_type        *ot_ptr;
   store_type        *st_ptr = &store[which];

   ot_ptr = &owners[store_flavor(which)][st_ptr->owner];

   *price = 0;


   /* Obtain the starting offer and the final offer */
   cur_ask = price_item(which, i_ptr, ot_ptr->max_inflate, TRUE);
   final_ask = price_item(which, i_ptr, ot_ptr->min_inflate, TRUE);

   /* Determine if haggling is necessary */
   noneed = noneedtobargain(which, final_ask);

   /* Get the owner's payout limit */
   purse = (s32b)(ot_ptr->max_cost);

   /* Go right to final price if player has bargained well */
   if (noneed || no_haggle_flag || (final_ask >= purse))
   {
      /* No reason to haggle */
      if (final_ask >= purse)
      {
         s32b diff = final_ask - purse;
         while ( final_ask > purse )
         {
            diff = (diff / 2)-1;
            final_ask = final_ask - diff;
            purse = purse + (diff/4);
dlog(DEBUGSTORE,"store.c: sell_haggle: final_ask (%ld) > purse (%ld): diff %ld\n",
                final_ask, purse, diff);
         }

         /* Message */
         msg_print("You instantly agree upon the price.");
         msg_print(NULL);

         /* Offer full purse */
         final_ask = purse;
      }

      /* No need to haggle */
      else if (noneed)
      {
         /* Message */
         msg_print("You eventually agree upon the price.");
         msg_print(NULL);
      }

      /* No haggle option */
      else
      {
         /* Message summary */
         msg_print("You quickly agree upon the price.");
         msg_print(NULL);

         /* Apply Sales Tax */
         final_ask -= final_ask / 10;
      }

      /* Final price */
      cur_ask = final_ask;

      /* Final offer */
      final = TRUE;
      pmt = "Final Offer";
   }

   /* Haggle for the whole pile */
   cur_ask *= i_ptr->number;
   final_ask *= i_ptr->number;

   /* XXX XXX XXX Display commands */

   /* Haggling parameters */
   min_per = ot_ptr->haggle_per;
   max_per = min_per * 3;

   /* Mega-Hack -- artificial "last offer" value */
   last_offer = object_value(i_ptr) * i_ptr->number;
   last_offer = last_offer * ot_ptr->max_inflate / 100L;

   /* No offer yet */
   offer = 0;

   /* No incremental haggling yet */
   allow_inc = FALSE;

   /* Haggle */
   for (flag = FALSE; !flag; )
   {
      /* XXX XXX XXX XXX */
      do
      {
         loop_flag = TRUE;
         offer_no++;

         (void)sprintf(out_val, "%s :  %ld", pmt, (long)cur_ask);
         put_str(out_val, 0, 1);
         cancel = receive_offer(which, "What price do you ask? ",
                              &offer, last_offer, -1, cur_ask, final);

         if (cancel)
         {
            flag = TRUE;
         }
         else if (offer < cur_ask)
         {
            say_comment_6();
            /* rejected, reset offer for incremental haggling */
            offer = last_offer;
         }
         else if (offer == cur_ask)
         {
            flag = TRUE;
            *price = offer;
         }
         else
         {
            loop_flag = FALSE;
         }
      }
      while (!flag && loop_flag);
      /* XXX XXX XXX XXX */

      if (!flag)
      {
         x1 = 100 * (last_offer - offer) / (last_offer - cur_ask);
         if (x1 < min_per)
         {
            if (haggle_insults(which))
            {
               flag = TRUE;
               cancel = TRUE;
            }
         }
         else if (x1 > max_per)
         {
            x1 = x1 * 3 / 4;
            if (x1 < max_per) x1 = max_per;
         }
         x2 = rand_range(x1-2, x1+2);
         x3 = ((offer - cur_ask) * x2 / 100L) + 1;
         /* don't let the price go down */
         if (x3 < 0) x3 = 0;
         cur_ask += x3;

         /* we bargained long and hard, now make him an offer */
         /* he can't refuse.... */
         if (offer_no>3 && (randint(10-offer_no)==1))
         {
            if (check_swap_items_store(which, i_ptr, page))
            {
               *price = -1;
               return (FALSE);
            }
         }

         if (cur_ask > final_ask)
         {
            cur_ask = final_ask;
            final = TRUE;
            pmt = "Final Offer";
            annoyed++;
            if (annoyed > 3)
            {
               flag = TRUE;
               (void)(increase_insults(which));
            }
         }
         else if (offer <= cur_ask)
         {
            flag = TRUE;
            *price = offer;
         }

         if (!flag)
         {
            last_offer = offer;
            allow_inc = TRUE;
            prt("", 0, 1);
            (void)sprintf(out_val,
                         "Your last bid %ld", (long)last_offer);
            put_str(out_val, 39, 1);
            say_comment_3(cur_ask, annoyed);
         }
      }
   }

   /* Cancel */
   if (cancel) return (TRUE);

   /* Update bargaining info */
   updatebargain(which, *price, final_ask);

   /* Do not cancel */
   return (FALSE);
}

/*
 * make a guess about how much to buy of something.
 * most of the time 1, other times max 10.
 */
static s16b guess_number_to_buy(object_type *i_ptr)
{
   s16b number = 1;

   switch(i_ptr->tval)
   {
      case TV_NOTHING:
      case TV_SKELETON:
      case TV_BOTTLE:
      case TV_JUNK:
      case TV_BOW:
      case TV_DIGGING:
      case TV_HAFTED:
      case TV_POLEARM:
      case TV_SWORD:
      case TV_BOOTS:
      case TV_GLOVES:
      case TV_HELM:
      case TV_CROWN:
      case TV_SHIELD:
      case TV_CLOAK:
      case TV_SOFT_ARMOR:
      case TV_HARD_ARMOR:
      case TV_DRAG_ARMOR:
      case TV_AMULET:
      case TV_RING:
      case TV_BOOK:
      case TV_SPELL:
      case TV_ROD:
      case TV_CHEST: number = 1;
                     break;

      case TV_SPIKE:
      case TV_SHOT:
      case TV_STAFF:
      case TV_SCROLL:
      case TV_POTION:
      case TV_FLASK:
      case TV_FOOD:
      case TV_GOLD:
      case TV_WAND:
      case TV_ARROW:
      case TV_BOLT: number = (i_ptr->number>10)?10:i_ptr->number;
                    break;

      case TV_LITE: number = (i_ptr->sval == SV_LITE_TORCH)?i_ptr->number:1;
                    break;
   }
   return (number);
}

/*
 * Buy an item from a store                             -RAK-
 */
static void store_purchase(s16b which, s16b *page)
{
   s16b                i, amt, choice;
   s16b                item, item_new;

   s32b                price, best = 0;

   object_type         sell_obj;
   object_type        *i_ptr;

   char                i_name[80];

   char                out_val[160];
   store_type         *st_ptr = &store[which];
   owner_type         *ot_ptr;

   ot_ptr = &owners[store_flavor(which)][st_ptr->owner];

   /* Empty? */
   if (st_ptr->stock_num <= 0)
   {
      if (st_ptr->store_type == DUNG_ENTR_HOME)
         msg_print("Your home is empty.");
      else
         msg_print("I am currently out of stock.");
      return;
   }

   /* Find the number of objects on this and following pages */
   i = (st_ptr->stock_num - (*page)*12);

   /* And then restrict it to the current page */
   if (i > 12) i = 12;

   /* Prompt */
   if (st_ptr->store_type == DUNG_ENTR_HOME)
   {
      sprintf(out_val, "Which item do you want to take? ");
   }
   else
   {
      sprintf(out_val, "Which item are you interested in? ");
   }

   /* Get the item number to be bought */
   if (!get_stock(&item, out_val, 0, i-1)) return;

   /* Get the actual index */
   item = item + (*page) * 12;

   /* Get the actual item */
   i_ptr = &st_ptr->stock[item];

   /* Assume the player wants just one of them */
   amt = 1;

   if ( (i_ptr->tval == TV_SPELL) && (st_ptr->store_type != DUNG_ENTR_HOME) )
   {
      /* we can't read it if it's for the wrong class */
      if ( (p_ptr->pclass != CLASS_HIGHPRST) &&
           (!(s_info[i_ptr->sval].sclass & (1L<<p_ptr->pclass))))
      {
         if (!get_check("You know you cannot use this spell. Really buy it?"))
         {
            return;
         }
      }
   }

   /* Hack -- get a "sample" object */
   sell_obj = *i_ptr;
   sell_obj.number = amt;

   /* Hack -- require room in pack */
   if (!inven_carry_okay(&sell_obj))
   {
      msg_print("You cannot carry that many different items.");
      return;
   }

   if (st_ptr->store_type != DUNG_ENTR_HOME)
   {
      /* Determine the "best" price (per item) */
      best = price_item(which, &sell_obj, ot_ptr->min_inflate, FALSE);
   }

   /* Find out how many the player wants */
   if (i_ptr->number > 1)
   {
      s16b num_guess = 0;

      /* Hack -- note cost of "fixed" items */
      if ((st_ptr->store_type != DUNG_ENTR_HOME) && (i_ptr->ident & ID_FIXED))
      {
         msg_format("That costs %ld gold per item.", (long)(best));
      }

      num_guess = guess_number_to_buy(i_ptr);

      if ( (num_guess * best) > p_ptr->au )
      {
         num_guess = (p_ptr->au / best);
      }

      /* Get a quantity */
      amt = get_quantity(NULL, i_ptr->number, num_guess);

      /* Allow user abort */
      if (amt <= 0) return;
   }

   /* Create the object to be sold (structure copy) */
   sell_obj = *i_ptr;
   sell_obj.number = amt;
   /* if we decrease the number of charged staffs/wands, take care of the maximum charge */
   if ( ( (i_ptr->tval == TV_WAND) || (i_ptr->tval == TV_STAFF) ) && (i_ptr->p1val>0) && (i_ptr->number>1))
   {
      s16b new_charge = (2 * amt * i_ptr->p1val ) / i_ptr->number;
      char tmp_name[80];
      sell_obj.p1val = (new_charge/2) + (new_charge & 1);
      object_desc_store(tmp_name, &sell_obj, FALSE, 0);
      msg_format("Your %s will have %d charge%s\n", tmp_name, sell_obj.p1val, (sell_obj.p1val == 1)?"":"s");
   }

   /* Hack -- require room in pack */
   if (!inven_carry_okay(&sell_obj))
   {
      msg_print("You cannot carry that many items.");
      return;
   }

   /* Attempt to buy it */
   if (st_ptr->store_type != DUNG_ENTR_HOME)
   {
      /* Fixed price, quick buy */
      if (i_ptr->ident & ID_FIXED)
      {
         /* Assume accept */
         choice = 0;

         /* Go directly to the "best" deal */
         price = (best * sell_obj.number);
      }

      /* Haggle for it */
      else
      {
         /* Describe the object (fully) */
         object_desc_store(i_name, &sell_obj, TRUE, 3);

         /* Message */
         msg_format("Buying %s (%c).", i_name, I2A(item % 12 ));
         msg_print(NULL);

         /* Haggle for a final price */
         choice = purchase_haggle(which, item, &sell_obj, &price, page);

         /* Hack -- Got kicked out */
         if (st_ptr->store_open >= turn) return;

         /* Hack -- swapped item */
         if (price == -1) return;
      }


      /* Player wants it */
      if (choice == 0)
      {
         /* Fix the item price (if "correctly" haggled) */
         if (price == (best * sell_obj.number)) i_ptr->ident |= ID_FIXED;

         /* Player can afford it */
         if (p_ptr->au >= price)
         {
            /* Say "okay" */
            say_comment_1();

            /* Be happy */
            decrease_insults(which);

            /* Spend the money */
            p_ptr->au -= price;

            /* Update the display */
            store_prt_gold();

            /* Hack -- buying an item makes you aware of it */
            object_aware(&sell_obj);
            object_tried(&sell_obj);
            sell_obj.log.mlevel = p_ptr->mdepth;
            sell_obj.log.slevel = p_ptr->sdepth;
            /* encode which store we bought it */
            /* owner can range from 0 to 19 */
            /* store_type can range from 0 to 29 */
            sell_obj.log.whose = 30*st_ptr->store_type + st_ptr->owner;
            sell_obj.log.where = OBJ_FOUND_STORE;

            /* Hack -- clear the "fixed" flag from the item */
            sell_obj.ident &= ~ID_FIXED;

            /* Describe the transaction */
            object_desc(i_name, &sell_obj, TRUE, 3);

            /* Message */
            msg_format("You bought %s for %ld gold.", i_name, (long)price);

            /* Let the player carry it (as if he picked it up) */
            test_new_object(&sell_obj);
            item_new = inven_carry(&sell_obj,sell_obj.number);

            /* Describe the final result */
            object_desc(i_name, &inventory[item_new], TRUE, 3);

            /* Message */
            msg_format("You have %s (%c).",
                       i_name, index_to_label(item_new));

            /* Handle stuff */
            handle_stuff();

            /* Note how many slots the store used to have */
            i = st_ptr->stock_num;

            /* Remove the bought items from the store */
            store_item_increase(which, item, -amt);
            store_item_optimize(which, item);

            /* Item is still here */
            if (i == st_ptr->stock_num)
            {
                /* Redraw the item */
               display_entry(which, item);
            }

            /* The item is gone */
            else
            {
               display_inventory(which, (*page));
            }
         }

         /* Player cannot afford it */
         else
         {
            /* Simple message (no insult) */
            msg_print("You do not have enough gold.");
         }
      }
   }

   /* Home is much easier */
   else
   {
      /* Carry the item */
      item_new = inven_carry(&sell_obj,sell_obj.number);

      /* Describe just the result */
      object_desc(i_name, &inventory[item_new], TRUE, 3);

      /* Message */
      msg_format("You have %s (%c).", i_name, index_to_label(item_new));

      /* Handle stuff */
      handle_stuff();

      /* Take note if we take the last one */
      i = st_ptr->stock_num;

      /* Remove the items from the home */
      store_item_increase(which, item, -amt);
      store_item_optimize(which, item);

      /* Hack -- Item is still here */
      if (i == st_ptr->stock_num)
      {
         /* Redraw the item */
         display_entry(which, item);
      }

      /* The item is gone */
      else
      {
         /* Nothing left */
         if (st_ptr->stock_num == 0) (*page) = 0;

         /* Redraw everything */
         display_inventory(which, (*page));
      }
   }

   /* Not kicked out */
   return;
}

/*
 * An "item_tester_hook" for selling to shops
 */
static bool item_tester_shop_buys(object_type *i_ptr)
{
   /* test against the current store */
   return (store_will_buy(current_store, i_ptr));
}

/*
 * Sell an item to the store (or home)
 */
static void store_sell(s16b which, s16b *page)
{
   s16b                choice;
   s16b                item, item_pos;
   s16b                amt = 0;

   s32b                price, value, dummy;

   object_type         sold_obj;
   object_type        *i_ptr;
   store_type         *st_ptr = &store[which];

   cptr                pmt = "Sell which item? ";

   char                i_name[80];


   /* Prepare a prompt */
   if (st_ptr->store_type == DUNG_ENTR_HOME) pmt = "Drop which item? ";

   /* Only allow items the store will buy */
   current_store = which;

   item_tester_hook = item_tester_shop_buys;

   /* Get an item (from equip or inven) */
   if (!get_item(&item, &amt, pmt, TRUE, TRUE, FALSE))
   {
      if (item == -2)
      {
         msg_print("You have nothing that I want.");
      }
      current_store = 0;
      item_tester_hook = NULL;
      return;
   }

   current_store = 0;
   item_tester_hook = NULL;

   /* did we abort choosing the number? */
   if (amt == 0)
   {
      return;
   }

   /* Get the item (on the floor) - there was code to sell from the floor? */
   i_ptr = &inventory[item];

   if (!store_will_buy(which, i_ptr))
   {
      msg_print("Why would I buy that?");
      return;
   }

   /* Create the object to be sold (structure copy) */
   sold_obj = *i_ptr;
   sold_obj.number = amt;
   if ((i_ptr->tval == TV_ROD) && (i_ptr->p1val>0))
   {
      /* the dropped rods cannot recharge longer than amt * max_charge */
      sold_obj.p1val = min(sold_obj.p1val, amt * get_rod_charge(&sold_obj));
   }
   if ((i_ptr->tval == TV_WAND) || (i_ptr->tval == TV_STAFF)) 
   {
      /* if we have 4 drop 1, 25% of charges in dropped, 75% in kept */
      sold_obj.p1val = (amt * i_ptr->p1val) / i_ptr->number;
   }

   /* Get a full description */
   object_desc(i_name, &sold_obj, TRUE, 3);

   /* Remove any inscription for stores */
   if (st_ptr->store_type != DUNG_ENTR_HOME) sold_obj.note = 0;

   /* Is there room in the store (or the home?) */
   if (!store_check_num(which, &sold_obj))
   {
      if (st_ptr->store_type == DUNG_ENTR_HOME)
         msg_print("Your home is full.");
      else
         msg_print("I have not the room in my store to keep it.");
      return;
   }


   /* Real store */
   if (st_ptr->store_type != DUNG_ENTR_HOME)
   {
      /* Describe the transaction */
      msg_format("Selling %s (%c).", i_name, index_to_label(item));
      msg_print(NULL);

      /* Haggle for it */
      choice = sell_haggle(which, &sold_obj, &price, page);

      /* Kicked out */
      if (st_ptr->store_open >= turn) return;

      /* Sold... */
      if (choice == 0)
      {
         /* Say "okay" */
         say_comment_1();

         /* Be happy */
         decrease_insults(which);

         /* Get some money */
         p_ptr->au += price;

         /* Update the display */
         store_prt_gold();

         /* Get the inventory item */
         i_ptr = &inventory[item];

         /* Get the "apparent" value */
         dummy = object_value(&sold_obj) * sold_obj.number;

         /* Become "aware" of the item */
         object_aware(&sold_obj);

         /* Know the item fully */
         object_tried(&sold_obj);

         /* Know the item fully */
         object_known(&sold_obj);

         /* Re-Create the now-identified object that was sold */
         sold_obj.number = amt;

         /* Get the "actual" value */
         value = object_value(&sold_obj) * sold_obj.number;

         /* Get the description all over again */
         object_desc(i_name, &sold_obj, TRUE, 3);

         /* Describe the result (in message buffer) */
         msg_format("You sold %s for %ld gold.", i_name, (long)price);

         /* Analyze the prices (and comment verbally) */
         purchase_analyze(price, value, dummy);

         /* Take the item from the player, describe the result */
         item_increase(item, -amt, px, py);
         item_describe(item, px, py);
         item_optimize(item, px, py);

         /* Handle stuff */
         handle_stuff();

         /* The store gets that (known) item */
         item_pos = store_carry(which, &sold_obj);

         /* Re-display if item is now in store */
         if (item_pos >= 0)
         {
            (*page) = item_pos / 12;
            display_inventory(which, (*page));
         }
      }
   }

   /* Player is at home */
   else
   {
      /* Describe */
      msg_format("You drop %s.", i_name);

dlog(DEBUGSTORE,"store.c: store_sell: which %d home: dropped %s\n", which, i_name);

      /* Take it from the players inventory */
      item_increase(item, -amt, px, py);
      item_describe(item, px, py);
      item_optimize(item, px, py);

      /* Handle stuff */
      handle_stuff();

      /* Let the store (home) carry it */
      item_pos = home_carry(which, &sold_obj);

      /* Update store display */
      if (item_pos >= 0)
      {
         (*page) = item_pos / 12;
         display_inventory(which, (*page));
      }
   }
}

/*
 * Hack -- set this to leave the store
 */
static bool leave_store = FALSE;


/*
 * Process a command in a store
 *
 * Note that we must allow the use of a few "special" commands
 * in the stores which are not allowed in the dungeon, and we
 * must disable some commands which are allowed in the dungeon
 * but not in the stores.
 */
static void store_process_command(s16b which, s16b *page)
{
   store_type *st_ptr = &store[which];

   /* Parse the command */
   switch (p_ptr->command_cmd)
   {
      /* Leave */
      case ESCAPE:
         leave_store = TRUE;
         break;

      /* Browse */
      case ' ':
         if (st_ptr->stock_num > 12)
         {
dlog(DEBUGSTORE,"store.c: store_process_command: store %d page was %d, stock_num / 12 %d\n",
         which, (*page), st_ptr->stock_num / 12);
            (*page)++;
            if (((*page)*12) >= st_ptr->stock_num) (*page) = 0;
dlog(DEBUGSTORE,"store.c: store_process_command: store %d page now %d before display_inv\n",which, (*page));
            display_inventory(which, (*page));
dlog(DEBUGSTORE,"store.c: store_process_command: store %d page now %d after display_inv\n",which, (*page));
         }
         break;

      /* Redraw */
      case KTRL('R'):
         do_cmd_redraw();
         display_store(which, (*page));
         break;

      /* Get (purchase) */
      case ',':
      case 'g':
         /* store_purchase can change the current page! */
         store_purchase(which, page); break;

      /* Drop (Sell) */
      case 'd':
         /* store_sell can change the current page! */
         store_sell(which, page); break;

      /* Ignore */
      case '\n':
      case '\r':
      case '\t':
         break;

      /*** Inventory Commands ***/

      /* Wear/wield equipment */
      case 'w':
         do_cmd_wield(); break;

      /* Take off equipment */
      case 't':
         do_cmd_takeoff(); break;

      /* Destroy an item */
      case 'k':
         do_cmd_destroy(); break;

      /* Equipment list */
      case 'e':
         do_cmd_equip(); break;

      /* Inventory list */
      case 'i':
         do_cmd_inven(); break;

      /* Browse a book */
      case 'b':
         do_cmd_browse(); break;

      /*** Various commands ***/

      /* Identify an object */
      case 'I':
         do_cmd_observe(); break;

      /*** Use various objects ***/

      /* Inscribe an object */
      case '{':
         do_cmd_inscribe(); break;

      /* Uninscribe an object */
      case '}':
         do_cmd_uninscribe(); break;

      /*** Help and Such ***/

      /* Help */
      case '?':
         do_cmd_help("help.hlp"); break;

      /* Identify symbol */
      case '/':
         do_cmd_query_symbol(); break;

      /* Character description */
      case 'C':
         do_cmd_change_name();
         display_store(which, (*page));
         break;

      /*** System Commands ***/

      case '@':
         do_cmd_system_command(); break;

      /*** Misc Commands ***/

      /* Take notes */
      case ':':
         do_cmd_note(); break;

      /* Version info */
      case 'V':
         do_cmd_version(); break;

      /* Repeat level feeling */
      case KTRL('F'):
         do_cmd_feeling(); break;

      /* Show previous message */
      case KTRL('O'):
         do_cmd_message_one(); break;

      /* Show previous messages */
      case KTRL('P'):
         do_cmd_messages(); break;

      /* Check artifacts */
      case '~':
         do_cmd_check_artifacts(NULL, TRUE); break;

      /* Check uniques */
      case '|':
         do_cmd_check_uniques(NULL, TRUE); break;

      /* Check kills */
      case 'K':
         do_cmd_check_kills(NULL, TRUE); break;

      /* Load "screen dump" */
      case '(':
         do_cmd_load_screen(); break;

      /* Save "screen dump" */
      case ')':
         do_cmd_save_screen(); break;

      /* Hack -- Unknown command */
      default:
         if (st_ptr->store_type == DUNG_ENTR_HOME)
         {
            msg_print("That command does not work in your home.");
         }
         else
         {
            msg_print("That command does not work in stores.");
         }
         break;
   }
}


/*
 * Enter a store, and interact with it.
 *
 * Note that we use the standard "request_command()" function
 * to get a command, allowing us to use "p_ptr->command_arg" and all
 * command macros and other nifty stuff, but we then do weird
 * things with the "g" (get), "p" (pray/purchase), "m" (magic),
 * "d" (drop), and "s" (search/sell) commands, depending on
 * whether we are in a store or the home.
 */
void do_cmd_store(void)
{
   s16b                 which, page;

   s16b                 tmp_chr;
   store_type          *st_ptr;
   owner_type          *ot_ptr;
   cave_cell_type           *c_ptr;


   /* Acceps the player grid */
   c_ptr = &dungeon.level[sublevel][py][px];

   /* Verify a store */
   if (c_ptr->mtyp != DUNG_ENTR)
   {
      msg_print("You see no store here.");
      return;
   }

   /* Extract the store code */
   which = c_ptr->extra;

dlog(DEBUGSTORE,"store.c: do_cmd_store extra %d grid mtyp %d styp %d at %d,%d\n",
           dungeon.level[sublevel][py][px].extra, dungeon.level[sublevel][py][px].mtyp, dungeon.level[sublevel][py][px].styp, px, py);



   /* Hack -- Check the "locked doors" */
   if ((store[which].store_open >= turn) && (store[which].store_type != DUNG_ENTR_HOME))
   {
      msg_print("The doors are locked.");
      return;
   }

   /* Hack -- Character is in "icky" mode */
   character_icky = TRUE;

   /* No command argument */
   p_ptr->command_arg = 0;

   /* No repeated command */
   p_ptr->command_rep = 0;

   /* No automatic command */
   p_ptr->command_new = 0;

   /* Save the store and owner pointers */
   st_ptr = &store[which];
   ot_ptr = &owners[which][st_ptr->owner];

   /* Start at the beginning */
   page = 0;

dlog(DEBUGSTORE,"store.c: do_cmd_store page set to 0\n");



   /* Display the store */
   display_store(which, page);

   /* Do not leave */
   leave_store = FALSE;

   /* Interact with plawer */
   while (!leave_store)
   {
      /* Hack -- Clear line 1 */
      prt("", 0, 1);

      /* Hack -- Check the charisma */
      tmp_chr = p_ptr->stat_use[A_CHR];

      /* Clear */
      clear_from(21);

      /* Basic commands */
      prt(" ESC) Exit from Building.", 0, 22);

      /* Browse if necessary */
      if (st_ptr->stock_num > 12)
      {
          prt(" SPACE) Next page of stock", 0, 23);
      }

      /* Home commands */
      if (st_ptr->store_type == DUNG_ENTR_HOME)
      {
          prt(" g) Get an item.", 40, 22);
          prt(" d) Drop an item.", 40, 23);
      }

      /* Shop commands XXX XXX XXX */
      else
      {
          prt(" p) Purchase an item.", 40, 22);
          prt(" s) Sell an item.", 40, 23);
      }

      /* Prompt */
      prt("You may: ", 0, 21);

      /* Get a command */
      request_command();

      /* Home commands XXX XXX XXX */
      if (st_ptr->store_type == DUNG_ENTR_HOME)
      {
          /* Convert */
          switch (p_ptr->command_cmd)
          {
              /* Command "p" -> "purchase" */
              case 'p': p_ptr->command_cmd = 'g'; break;

              /* Command "m" -> "purchase" */
              case 'm': p_ptr->command_cmd = 'g'; break;

              /* Command "s" -> "sell" */
              case 's': p_ptr->command_cmd = 'd'; break;
          }
      }

      /* Shop commands XXX XXX XXX */
      else
      {
          /* Convert and restrict */
          switch (p_ptr->command_cmd)
          {
              /* Command "d" -> "drop" (ignore) */
              case 'd': p_ptr->command_cmd = '`'; break;

              /* Command "p" -> "purchase" */
              case 'p': p_ptr->command_cmd = 'g'; break;

              /* Command "m" -> "purchase" */
              case 'm': p_ptr->command_cmd = 'g'; break;

              /* Command "s" -> "sell" */
              case 's': p_ptr->command_cmd = 'd'; break;
          }
      }

      /* Process the command */
      store_process_command(which, &page);

      /* Hack -- Character is still in "icky" mode */
      character_icky = TRUE;

      /* Notice stuff */
      notice_stuff();

      /* Handle stuff */
      handle_stuff();

      /* XXX XXX XXX Pack Overflow */
      if (inventory[INVEN_PACK].k_idx)
      {
         /* Hack -- Flee from the store */
         if (st_ptr->store_type != DUNG_ENTR_HOME)
         {
            /* Message */
            msg_print("Your pack is so full that you flee the store...");
            msg_print(NULL);

            /* Leave */
            leave_store = TRUE;
         }

         /* Hack -- Flee from the home */
         else if (!store_check_num(which, &inventory[INVEN_PACK]))
         {
            /* Message */
            msg_print("Your pack is so full that you flee your home...");
            msg_print(NULL);

            /* Leave */
            leave_store = TRUE;
         }

         /* Hack -- Drop items into the home */
         else
         {
            s16b item_pos;

            object_type sold_obj;

            char i_name[80];

            /* Grab a copy of the item */
            sold_obj = inventory[INVEN_PACK];

            /* Give a message */
            msg_print("Your pack overflows!");

            /* Describe it */
            object_desc(i_name, &sold_obj, TRUE, 3);

            /* Message */
            msg_format("You drop %s.", i_name);

            /* Remove it from the players inventory */
            item_increase(INVEN_PACK, -999, px, py);
            item_describe(INVEN_PACK, px, py);
            item_optimize(INVEN_PACK, px, py);

            /* Handle stuff */
            handle_stuff();

            /* Let the store (home) carry it */
            item_pos = home_carry(which, &sold_obj);

            /* Redraw the home */
            if (item_pos >= 0)
            {
               page = (item_pos / 12) * 12;
               display_inventory(which, page);
            }
         }
      }

      /* Hack -- Redisplay store prices if charisma changes */
      if (tmp_chr != p_ptr->stat_use[A_CHR]) display_inventory(which, page);

      /* Hack -- get kicked out of the store */
      if (st_ptr->store_open >= turn) leave_store = TRUE;
   }

   /* Forget the store number, etc */

   /* Free turn XXX XXX XXX */
   energy_use = 0;

   /* Hack -- Character is no longer in "icky" mode */
   character_icky = FALSE;

   /* Hack -- Cancel automatic command */
   p_ptr->command_new = 0;

   /* Hack -- Cancel "see" mode */
   p_ptr->command_see = FALSE;

   /* Update stuff */
   p_ptr->update |= (PU_VIEW);
   p_ptr->update |= (PU_MONSTERS);

   /* Redraw stuff */
   p_ptr->redraw1 |= (PR1_WIPE | PR1_BASIC | PR1_EXTRA);

   /* Redraw map */
   p_ptr->redraw1 |= (PR1_MAP);

   /* Window stuff */
   p_ptr->window |= (PW_OVERHEAD);
}

/*
 * Shuffle one of the stores.
 */
void store_shuffle(void)
{
   s16b        i, j, n, which;
   store_type *st_ptr;
   owner_type *ot_ptr;

   /* Pick a real store to shuffle */
   n = rand_int(num_stores);

   /* Save the store index */
   which = n;

   /* Activate that store */
   st_ptr = &store[which];

   /* don't shuffle home */
   if (st_ptr->store_type == DUNG_ENTR_HOME) return;

   /* Pick a new owner */
   for (j = st_ptr->owner; j == st_ptr->owner; )
   {
      st_ptr->owner = rand_int(MAX_OWNERS);
   }

   /* Activate the new owner */
   ot_ptr = &owners[which][st_ptr->owner];

   /* Reset the owner data */
   st_ptr->insult_cur = 0;
   st_ptr->store_open = 0;
   st_ptr->good_buy = 0;
   st_ptr->bad_buy = 0;

   /* Hack -- discount all the items */
   for (i = 0; i < st_ptr->stock_num; i++)
   {
      object_type *i_ptr;

      /* Get the item */
      i_ptr = &st_ptr->stock[i];

      /* Hack -- Sell all old items for "half price" */
      i_ptr->discount = 50;

      /* Hack -- Items are no longer "fixed price" */
      i_ptr->ident &= ~ID_FIXED;

      /* Mega-Hack -- Note that the item is "on sale" */
      i_ptr->note = quark_add("on sale");
   }
}

/*
 * this function will prune a number of items from a
 * store's inventory
 */
static void store_prune_inventory(s16b which)
{
   store_type *st_ptr = &store[which];
   s16b j;

   if (st_ptr->store_type == DUNG_ENTR_HOME) return;

   /* Mega-Hack -- prune the black market */
   if (st_ptr->store_type == DUNG_ENTR_MARKET)
   {
      /* Destroy crappy black market items */
      for (j = st_ptr->stock_num - 1; j >= 0; j--)
      {
         object_type *i_ptr = &st_ptr->stock[j];

         /* Destroy crappy items */
         if (black_market_crap(i_ptr))
         {
            /* Destroy the item */
            store_item_increase(which, j, 0 - i_ptr->number);
            store_item_optimize(which, j);
         }
      }
   }

   /* Choose the number of slots to keep */
   j = st_ptr->stock_num;

   /* Sell a few items */
   j = j - randint(STORE_TURNOVER) - (STORE_TURNOVER / 3);

   /* Never keep more than "STORE_MAX_KEEP" slots */
   if (j > STORE_MAX_KEEP) j = STORE_MAX_KEEP;

   /* Always "keep" at least "STORE_MIN_KEEP" items */
   if (j < STORE_MIN_KEEP) j = STORE_MIN_KEEP;

   /* Hack -- prevent "underflow" */
   if (j < 0) j = 0;

dlog(DEBUGSTORE,"store.c: store_prune_inventory: stock_num %d j %d\n", st_ptr->stock_num, j);

   /* Destroy objects until only "j" slots are left */
   while (st_ptr->stock_num > j) store_delete_item(which);
}

/*
 * Maintain the inventory at the stores.
 */
void store_maint(s16b which)
{
   s16b         j;
   s16b         old_rating = rating;
   bool         ok = FALSE;

   store_type  *st_ptr;
   owner_type  *ot_ptr;

   /* Activate that store */
   st_ptr = &store[which];

   if (st_ptr->store_type == DUNG_ENTR_HOME) return;

   /* Activate the new owner */
   ot_ptr = &owners[which][st_ptr->owner];

   /* Store keeper forgives the player */
   st_ptr->insult_cur = 0;

dlog(DEBUGSTORE,"store.c: store_maint: which %d store %s owner %s\n", 
                which, f_name + f_info[get_f_idx(DUNG_ENTR, st_ptr->store_type)].name,
                ot_ptr->owner_name);

   /* home should not be processed */

   while (ok == FALSE)
   {
      store_prune_inventory(which);

      /* Choose the number of slots to fill */
      j = st_ptr->stock_num;

      /* Buy some more items */
      j = j + randint(STORE_TURNOVER);

      /* Never keep more than "STORE_MAX_KEEP" slots */
      if (j > STORE_MAX_KEEP) j = STORE_MAX_KEEP;

      /* Always "keep" at least "STORE_MIN_KEEP" items */
      if (j < STORE_MIN_KEEP) j = STORE_MIN_KEEP;

      /* Hack -- prevent "overflow" */
      if (j >= STORE_INVEN_MAX) j = STORE_INVEN_MAX - 1;

      /* Acquire some new items */
      store_add_inventory(which, j-st_ptr->stock_num);

      ok = check_store_necessary_items(which);
   }

   /* Hack -- Restore the rating */
   rating = old_rating;
}

/*
 * Initialize the stores
 */
void store_init(s16b which)
{
   s16b         k;
   store_type  *st_ptr;
   owner_type  *ot_ptr;

   /* Activate that store */
   st_ptr = &store[which];

   /* Pick an owner */
   st_ptr->owner = rand_int(MAX_OWNERS);

   /* Activate the new owner */
   ot_ptr = &owners[which][st_ptr->owner];

   /* Initialize the store */
   st_ptr->store_open = 0;
   st_ptr->insult_cur = 0;
   st_ptr->good_buy = 0;
   st_ptr->bad_buy = 0;

   /* Nothing in stock */
   st_ptr->stock_num = 0;

   /* Clear any old items */
   for (k = 0; k < STORE_INVEN_MAX; k++)
   {
      invwipe(&st_ptr->stock[k]);
   }
}
