/* descriptions, mostly string handling code */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

/*
 * Certain items have a flavor
 * This function is used only by "flavor_init()"
 * note that TERM_DARK makes an item look unflavored?
 */
bool object_has_flavor(s16b k_idx)
{
   object_kind *k_ptr = &k_info[k_idx];

dlog(DEBUGITEMS,"object1.c: object_has_flavor: k_idx %d flavor %d flags3 & TR3_NOFLAVOR %08lx\n",
                k_idx, k_ptr->flavor, (k_ptr->flags3 & TR3_NOFLAVOR)!=0);

   if (k_ptr->flags3 & TR3_NOFLAVOR) return (FALSE);
   if (k_ptr->flavor) return (TRUE);

   /* Check for flavor */
   switch (k_ptr->tval)
   {
      /* The standard "flavored" items */
      case TV_AMULET:
      case TV_RING:
      case TV_STAFF:
      case TV_WAND:
      case TV_SCROLL:
      case TV_SPELL:
      case TV_POTION:
      case TV_ROD:
         return (TRUE);

      /* food without flavor has the NO_FLAVOR flag! */
      case TV_FOOD:
         return (TRUE);
   }

   /* Assume no flavor */
   return (FALSE);
}

/*
 * Certain items, if aware, are known instantly
 * This function is used only by "flavor_init()"
 */
bool object_easy_know(s16b k_idx)
{
   object_kind *k_ptr = &k_info[k_idx];

   /* Analyze the "tval" */
   switch (k_ptr->tval)
   {
      /* Simple items */
      case TV_FLASK:
      case TV_JUNK:
      case TV_BOTTLE:
      case TV_SKELETON:
      case TV_CORPSE:
      case TV_SPIKE:
         return (TRUE);

      /* All Food, Potions, Scrolls, Rods */
      case TV_FOOD:
      case TV_POTION:
      case TV_SCROLL:
      case TV_SPELL:
      case TV_ROD:
         return (TRUE);

      /* Some Rings, Amulets, Lites */
      case TV_RING:
      case TV_AMULET:
      case TV_LITE:
         if (k_ptr->flags3 & TR3_EASY_KNOW) return (TRUE);
         return (FALSE);
   }

   /* Nope */
   return (FALSE);
}

bool item_easy_know(object_type *i_ptr)
{
   if (object_easy_know(i_ptr->k_idx)) return TRUE;
   /* gloves of free action should be easy known to provide free action */
   if ((i_ptr->name2) && (e_info[i_ptr->name2].flags3 & TR3_EASY_KNOW)) return (TRUE);

   /* Nope */
   return (FALSE);
}
   

/*
 * invent scroll titles for all flavors from start to end (inclusive)
 */
static void make_titles(s16b start, s16b end)
{
   s16b i,j;

   /* Scrolls (random titles, always white) */
   for (i = start; i <= end; i++)
   {
      /* Get a new title */
      while (TRUE)
      {
         char buf[80];

         bool okay;

         /* Start a new title */
         buf[0] = '\0';

         /* Collect words until done */
         while (1)
         {
              s16b q, s;

              char tmp[80];

              /* Start a new word */
              tmp[0] = '\0';

              /* Choose one or two syllables */
              s = ((rand_int(100) < 30) ? 1 : 2);

              /* Add a one or two syllable word */
              for (q = 0; q < s; q++)
              {
                  /* Add the syllable */
                  strcat(tmp, syllables[rand_int(MAX_SYLLABLES)]);
              }

              /* Stop before getting too long */
              if (strlen(buf) + 1 + strlen(tmp) > 15) break;

              /* Add a space */
              strcat(buf, " ");

              /* Add the word */
              strcat(buf, tmp);
          }

          /* Save the title */
          strcpy(flavor[i].name, buf+1);
          /* Assume okay */
          okay = TRUE;

          /* Check for "duplicate" scroll titles */
          for (j = start; j < i; j++)
          {
              cptr hack1 = flavor[j].name;
              cptr hack2 = flavor[i].name;

              /* Compare first four characters */
              if (*hack1++ != *hack2++) continue;
              if (*hack1++ != *hack2++) continue;
              if (*hack1++ != *hack2++) continue;
              if (*hack1++ != *hack2++) continue;

              /* Not okay */
              okay = FALSE;

              /* Stop looking */
              break;
          }

          /* Break when done */
          if (okay) break;
      }
dlog(DEBUGITEMS,"object1.c: make_titles: i %d title %s\n", i, flavor[i].name);
   }
}

/*
 * random ize a group of flavors
 * starting at offset, ending at end (inclusive!)
 */
void object_flavor_randomize(s16b offset, s16b end)
{
   s16b i, j;
   flavor_type temp_flavor;

dlog(DEBUGITEMS,"object1.c: object_flavor_randomize: %d to %d\n", offset, end);
   for (i = offset; i <= end; i++)
   {
      j = offset + rand_int(end-offset);
dlog(DEBUGITEMS,"object1.c: object_flavor_randomize: swap %d of %d (%d with %d)\n", i, end, i, j);
      temp_flavor = flavor[i];
      flavor[i] = flavor[j];
      flavor[j] = temp_flavor;
   }
}

/* jk - if this is in flavor_init, all kind of debugging with item names */
/* becomes impossible during loading, level generation at the start.     */

void object_material_init(void)
{
   s16b         i;

dlog(DEBUGITEMS,"object1.c: object_material_init: starting\n");

   /* search for max number of ring flavors */
   for (i = FLAVOR_RING_START; (flavor[i].name[0] != '?'); i++);
   object_flavor_randomize(FLAVOR_RING_START, FLAVOR_RING_END);
dlog(DEBUGITEMS,"object1.c: object_material_init: rings done\n");

   /* search for max number of amulet flavors */
   for (i = FLAVOR_AMULET_START; (flavor[i].name[0] != '?'); i++);
   object_flavor_randomize(FLAVOR_AMULET_START, FLAVOR_AMULET_END);
dlog(DEBUGITEMS,"object1.c: object_material_init: amulets done\n");

   /* search for max number of staff flavors */
   for (i = FLAVOR_STAFF_START; (flavor[i].name[0] != '?'); i++);
   object_flavor_randomize(FLAVOR_STAFF_START, FLAVOR_STAFF_END);
dlog(DEBUGITEMS,"object1.c: object_material_init: staffs done\n");

   /* search for max number of wand flavors */
   for (i = FLAVOR_WAND_START; (flavor[i].name[0] != '?'); i++);
   object_flavor_randomize(FLAVOR_WAND_START, FLAVOR_WAND_END);
dlog(DEBUGITEMS,"object1.c: object_material_init: wands done\n");

   /* search for max number of rod flavors */
   for (i = FLAVOR_ROD_START; (flavor[i].name[0] != '?'); i++);
   object_flavor_randomize(FLAVOR_ROD_START, FLAVOR_ROD_END);
dlog(DEBUGITEMS,"object1.c: object_material_init: rods done\n");

   /* search for max number of food flavors */
   for (i = FLAVOR_FOOD_START; (flavor[i].name[0] != '?'); i++);
   object_flavor_randomize(FLAVOR_FOOD_START, FLAVOR_FOOD_END);
dlog(DEBUGITEMS,"object1.c: object_material_init: food done\n");

   /* search for max number of potion flavors */
   for (i = FLAVOR_POTION_START; (flavor[i].name[0] != '?'); i++);
   object_flavor_randomize(FLAVOR_POTION_START, FLAVOR_POTION_END);
dlog(DEBUGITEMS,"object1.c: object_material_init: potions done\n");

   /* search for max number of scroll flavors */
   /* fill misc_to_char at the same time. */
   for (i = FLAVOR_SCROLL_START; i <= FLAVOR_SCROLL_END; i++)
   {
      misc_to_char[i]='?';
      misc_to_attr[i]=TERM_WHITE;
   }
   make_titles(FLAVOR_SCROLL_START, FLAVOR_SCROLL_END);
   object_flavor_randomize(FLAVOR_SCROLL_START, FLAVOR_SCROLL_END);
dlog(DEBUGITEMS,"object1.c: object_material_init: scrolls done\n");

   /* search for max number of spell flavors */
   /* fill misc_to_char at the same time. */
   make_titles(FLAVOR_SPELL_START, FLAVOR_SPELL_END);
   object_flavor_randomize(FLAVOR_SPELL_START, FLAVOR_SPELL_END);
dlog(DEBUGITEMS,"object1.c: object_material_init: spells done\n");
   /* copy the bolt/beam/ball graphics */
   for (i=FLAVOR_BOLTBEAM_HSTART; i < FLAVOR_BOLTBEAM_HSTART+(5*16); i++)
   {
      misc_to_char[i]=flavor[i].name[0];
      misc_to_attr[i]=flavor[i].color;
   }

#if (debuglevel & DEBUGITEMS)
   for (i=0; i < MAX_FLAVORS; i++)
   {
      dlog(DEBUGITEMS,"object1.c: object_material_init: flavor %d name %s color %d\n",
                      i, flavor[i].name, flavor[i].color);
   }
#endif
}

/*
 * this function adds the prepared flavors to the correct objects
 */
void make_objects_flavored(void)
{
   s16b         i;

   /* Analyze every object */
   for (i = 1; i < k_number; i++)
   {
      object_kind *k_ptr = &k_info[i];

      /* Skip "empty" objects */
      if (!k_ptr->name) continue;
      if (!object_has_flavor(i)) continue;

      switch (k_ptr->tval)
      {
         case TV_RING  : k_ptr->flavor = FLAVOR_RING_START+k_ptr->sval; break;
         case TV_AMULET: k_ptr->flavor = FLAVOR_AMULET_START+k_ptr->sval; break;
         case TV_STAFF : k_ptr->flavor = FLAVOR_STAFF_START+k_ptr->sval; break;
         case TV_WAND  : k_ptr->flavor = FLAVOR_WAND_START+k_ptr->sval; break;
         case TV_ROD   : k_ptr->flavor = FLAVOR_ROD_START+k_ptr->sval; break;
         case TV_FOOD  : k_ptr->flavor = FLAVOR_FOOD_START+k_ptr->sval; break;
         case TV_POTION: k_ptr->flavor = FLAVOR_POTION_START+k_ptr->sval; break;
         case TV_SCROLL: k_ptr->flavor = FLAVOR_SCROLL_START+k_ptr->sval; break;
         case TV_SPELL : k_ptr->flavor = FLAVOR_SPELL_START+k_ptr->sval; break;
      }
      if (k_ptr->flavor)
      {
         k_ptr->d_attr = flavor[k_ptr->flavor].color;
         misc_to_char[k_ptr->flavor]=k_ptr->d_char;
         misc_to_attr[k_ptr->flavor]=k_ptr->d_attr;
      }

dlog(DEBUGITEMS,"object1.c: make_objects_flavored: k_idx %d name %s flavor: (nr %d) %s (color %d)\n",
                i, k_name + k_ptr->name, k_ptr->flavor, flavor[k_ptr->flavor].name, flavor[k_ptr->flavor].color);
dlog(DEBUGITEMS,"object1.c: make_objects_flavored: d_char %d d_attr %d x_char %d x_attr %d, misc_to_char %d misc_to_attr %d\n",
                k_ptr->d_char, k_ptr->d_attr, k_ptr->x_char, k_ptr->x_attr,
                misc_to_char[k_ptr->flavor], misc_to_attr[k_ptr->flavor]);
   }
}

/*
 * Prepare the "variable" part of the "k_info" array.
 *
 * The "color"/"metal"/"type" of an item is its "flavor".
 * For the most part, flavors are assigned randomly each game.
 *
 * Initialize descriptions for the "colored" objects, including:
 * Rings, Amulets, Staffs, Wands, Rods, Food, Potions, Scrolls.
 *
 * The first 4 entries for potions are fixed (Water, Apple Juice,
 * Slime Mold Juice, Unused Potion).
 *
 * Scroll titles are always between 6 and 14 letters long.  This is
 * ensured because every title is composed of whole words, where every
 * word is from 1 to 8 letters long (one or two syllables of 1 to 4
 * letters each), and that no scroll is finished until it attempts to
 * grow beyond 15 letters.  The first time this can happen is when the
 * current title has 6 letters and the new word has 8 letters, which
 * would result in a 6 letter scroll title.
 *
 * Duplicate titles are avoided by requiring that no two scrolls share
 * the same first four letters (not the most efficient method, and not
 * the least efficient method, but it will always work).
 *
 */
void flavor_init(void)
{
   s16b i;

   /* Analyze every object */
   for (i = 1; i < k_number; i++)
   {
      object_kind *k_ptr = &k_info[i];

      /* Skip "empty" objects */
      if (!k_ptr->name) continue;

      /* Check for a "flavor" */
      k_ptr->flavor = object_has_flavor(i);

      /* No flavor yields aware */
      if (!k_ptr->flavor) k_ptr->aware = TRUE;

      /* Check for "easily known" */
      k_ptr->easy_know = object_easy_know(i);
dlog(DEBUGITEMS,"object1.c: flavor_init: k_idx %d flavor %d aware %d easy_know %d\n",
                i, k_ptr->flavor, k_ptr->aware, k_ptr->easy_know);
   }
   make_objects_flavored();
}

/*
 * this function currently only serves to make some kinds of trap
 * known to rogues
 */
void races_init(void)
{
   t_info[TRAP_OF_DAGGER_I].known = randint(50) + 50;
   t_info[TRAP_OF_POISON_NEEDLE].known = randint(50) + 50;
   t_info[TRAP_OF_FIRE_BOLT].known = randint(50) + 50;
}

/*
 * Print a char "c" into a string "t", as if by sprintf(t, "%c", c),
 * and return a pointer to the terminator (t + 1).
 */
static char *object_desc_chr(char *t, char c)
{
   /* Copy the char */
   *t++ = c;

   /* Terminate */
   *t = '\0';

   /* Result */
   return (t);
}


/*
 * Print a string "s" into a string "t", as if by strcpy(t, s),
 * and return a pointer to the terminator.
 */
char *object_desc_str(char *t, cptr s)
{
   /* Copy the string */
   while (*s) *t++ = *s++;

   /* Terminate */
   *t = '\0';

   /* Result */
   return (t);
}



/*
 * Print an unsigned number "n" into a string "t", as if by
 * sprintf(t, "%u", n), and return a pointer to the terminator.
 */
static char *object_desc_num(char *t, u32b n)
{
   u32b p;

   /* if n is too big, the loop below will loop forever, since p*10 cannot */
   /* be contained in an u32b                                              */
   /* Find "size" of "n" */
   for (p = 1; n >= p * 10;)
   {
      u32b old_p = p;

      p = p * 10;
      if (p < old_p)
      {
         *t++='*';
         *t='\0';
         return (t);
      }
      old_p=p;
   }

   /* Dump each digit */
   while (p >= 1)
   {
      /* Dump the digit */
      *t++ = '0' + n / p;

      /* Remove the digit */
      n = n % p;

      /* Process next digit */
      p = p / 10L;
   }

   /* Terminate */
   *t = '\0';

   /* Result */
   return (t);
}

/*
 * Print an signed number "v" into a string "t", as if by
 * sprintf(t, "%+d", n), and return a pointer to the terminator.
 * Note that we always print a sign, either "+" or "-".
 */
static char *object_desc_int(char *t, s32b v)
{
   uint p, n;

   /* Negative */
   if (v < 0)
   {
      /* Take the absolute value */
      n = 0 - v;

      /* Use a "minus" sign */
      *t++ = '-';
   }

   /* Positive (or zero) */
   else
   {
      /* Use the actual number */
      n = v;

      /* Use a "plus" sign */
      *t++ = '+';
   }

   /* Find "size" of "n" */
   for (p = 1; n >= p * 10; p = p * 10) ;
dlog(DEBUGITEMS,"object1.c: object_desc_int: v %d n %d p %d\n", v, n, p);
   /* Dump each digit */
   while (p >= 1)
   {
      /* Dump the digit */
      *t++ = '0' + n / p;

      /* Remove the digit */
      n = n % p;

      /* Process next digit */
      p = p / 10;
   }

   /* Terminate */
   *t = '\0';

   /* Result */
   return (t);
}

/*
 * Creates a description of the item "i_ptr", and stores it in "out_val".
 *
 * One can choose the "verbosity" of the description, including whether
 * or not the "number" of items should be described, and how much detail
 * should be used when describing the item.
 *
 * The given "buf" must be 80 chars long to hold the longest possible
 * description, which can get pretty long, including incriptions, such as:
 * "no more Maces of Disruption (Defender) (+10,+10) [+5] (+3 to stealth)".
 * Note that the inscription will be clipped to keep the total description
 * under 79 chars (plus a terminator).
 *
 * Note the use of "object_desc_num()" and "object_desc_int()" as hyper-efficient,
 * portable, versions of some common "sprintf()" commands.
 *
 * Note that all ego-items (when known) append an "Ego-Item Name", unless
 * the item is also an artifact, which should NEVER happen.
 *
 * Note that all artifacts (when known) append an "Artifact Name", so we
 * have special processing for "Specials" (artifact Lites, Rings, Amulets).
 * The "Specials" never use "modifiers" if they are "known", since they
 * have special "descriptions", such as "The Necklace of the Dwarves".
 *
 * Special Lite's use the "k_info" base-name (Phial, Star, or Arkenstone),
 * plus the artifact name, just like any other artifact, if known.
 *
 * Special Ring's and Amulet's, if not "aware", use the same code as normal
 * rings and amulets, and if "aware", use the "k_info" base-name (Ring or
 * Amulet or Necklace).  They will NEVER "append" the "k_info" name.  But,
 * they will append the artifact name, just like any artifact, if known.
 *
 * None of the Special Rings/Amulets are "EASY_KNOW", though they could be,
 * at least, those which have no "pluses", such as the three artifact lites.
 *
 * Hack -- Display "The One Ring" as "a Plain Gold Ring" until aware.
 *
 * If "pref" then a "numeric" prefix will be pre-pended.
 *
 * Mode:
 *     0 -- The Cloak of Death
 *     1 -- The Cloak of Death [1,+3]
 *     2 -- The Cloak of Death [1,+3] (+2 to Stealth)
 *     3 -- The Cloak of Death [1,+3] (+2 to Stealth) {nifty}
 *     mode & 0x20 -- no color added, even if color_known_items==TRUE (for shops)
 *     mode & 0x40 -- no info added beyond basename
 *                    you see a ring drop. not you see a fluorite ring of .... drop.
 *                    artifacts are recognised if they are known.
 *     mode & 0x80 -- no ego-item or artifact extensions are added
 */
void object_desc(char *buf, object_type *i_ptr, bool pref, s16b mode)
{
   char                basenm[255], modstr[255];
   s16b                power;
   bool                aware = FALSE;
   bool                known = FALSE;

   bool                append_name = FALSE;

   bool                show_weapon = FALSE;
   bool                show_armour = FALSE;
   bool                show_base   = FALSE;
   bool                do_flavor = FALSE;

   cptr                s, u;
   char                *t;

   char                p1 = '(', p2 = ')';
   char                b1 = '[', b2 = ']';
   char                c1 = '{', c2 = '}';

   char                tmp_val[160], flav_aware[80],flav_unaware[80];

   u64b                f1, f2, f3;

   object_kind         *k_ptr = &k_info[i_ptr->k_idx];

dlog(DEBUGITEMS,"object1.c: object_desc: tv %d sv %d p1v %d p2v %d k_idx %d nm %s pref %d mode %d flav %d\n",
               i_ptr->tval, i_ptr->sval, i_ptr->p1val, i_ptr->p2val, i_ptr->k_idx, k_name + k_ptr->name,
               pref, mode, k_ptr->flavor);

   /* Extract some flags */
   object_flags(i_ptr, &f1, &f2, &f3);

   /* See if the object is "aware" */
   if (object_aware_p(i_ptr)) aware = TRUE;

   /* See if the object is "known" */
   if (object_known_p(i_ptr)) known = TRUE;

   /* Extract default "base" string */
   strcpy(basenm, (k_name + k_ptr->name));
   /* Assume no "modifier" string */
   strcpy(modstr, "");

dlog(DEBUGITEMS,"object1.c: object_desc: aware %d known %d tried %d basenm %s modstr %s\n",
                aware, known, object_tried_p(i_ptr), basenm, modstr);
   /* Analyze the object */
   switch (i_ptr->tval)
   {
       /* Some objects are easy to describe */
       case TV_BOTTLE:
       case TV_JUNK:
       case TV_SPIKE:
       case TV_FLASK:
       case TV_CHEST:
           break;

       /* Missiles/ Bows/ Weapons */
       case TV_SHOT:
       case TV_BOLT:
       case TV_ARROW:
/* jk - it is not called the Red Arrow of Gondor for nothing */
          if ((i_ptr->sval == SV_ARROW_HEAVY) && (i_ptr->name1 == ART_RED_GONDOR))
          {
             strcpy(basenm, "# Seeker Arrow");
             strcpy(modstr, "Red");
          }

       case TV_BOW:
       case TV_HAFTED:
       case TV_POLEARM:
       case TV_SWORD:
       case TV_DIGGING:
           if (i_ptr->ident & ID_EQUIPTESTED) show_weapon = TRUE;
           break;

       /* Armour */
       case TV_BOOTS:
       case TV_GLOVES:
       case TV_CLOAK:
       case TV_CROWN:
       case TV_HELM:
       case TV_SHIELD:
       case TV_SOFT_ARMOR:
       case TV_HARD_ARMOR:
       case TV_DRAG_ARMOR:
           if (i_ptr->ident & ID_EQUIPTESTED) show_armour = TRUE;
           break;

       /* Lites (including a few "Specials") */
       case TV_LITE:
           break;

       /* Rings (including a few "Specials") */
       case TV_RING:

           do_flavor=TRUE;
           strcpy(flav_aware,"& Ring~");
           strcpy(flav_unaware,"& # Ring~");

           /* special provision for iron/silver/gold/mithril rings */
           if (k_ptr->flavor == 0)
           {
              strcpy(flav_aware, basenm);
              strcpy(flav_unaware, basenm);
              do_flavor = FALSE;
           }

           /* Hack -- The One Ring */
           if (!aware && (i_ptr->sval == SV_RING_POWER))
           {
               strcpy(modstr, "Plain Gold");
               do_flavor = FALSE;
           }

           break;

       /* Amulets (including a few "Specials") */
       case TV_AMULET:

           do_flavor=TRUE; strcpy(flav_aware,"& Amulet~"); strcpy(flav_unaware,"& # Amulet~");
           break;

       case TV_STAFF:

           do_flavor=TRUE; strcpy(flav_aware,"& Staff~"); strcpy(flav_unaware,"& # Staff~");
           break;

       case TV_WAND:

           do_flavor=TRUE; strcpy(flav_aware,"& Wand~"); strcpy(flav_unaware,"& # Wand~");
           break;

       case TV_ROD:

           do_flavor=TRUE; strcpy(flav_aware,"& Rod~"); strcpy(flav_unaware,"& # Rod~");
           break;

       case TV_SCROLL:

           do_flavor=TRUE; strcpy(flav_aware,"& Scroll~"); strcpy(flav_unaware,"& Scroll~ titled \"#\"");
           break;

       case TV_SPELL:

           do_flavor=TRUE; strcpy(flav_aware,"& Spell~"); strcpy(flav_unaware,"& Spell~ reading \"#\"");
           break;

       case TV_POTION:

           if ( ((mode & 0x40) == 0) && ( !(k_ptr->flags3 & TR3_NOFLAVOR) ) )
           {
              /* Color the object */
              strcpy(modstr, flavor[k_ptr->flavor].name);
              if (aware) append_name = TRUE;
              strcpy(basenm, aware ? "& Potion~" : "& # Potion~");
           }
           else
           {
              strcpy(basenm, "& Potion~");
              if (k_ptr->flags3 & TR3_NOFLAVOR) append_name = TRUE;
           }
#if 0
           do_flavor=TRUE; strcpy(flav_aware,"& Potion~"); strcpy(flav_unaware,"& # Potion~");
#endif
           break;

       case TV_FOOD:

           if ( ((mode & 0x40) == 0) && ( !(k_ptr->flags3 & TR3_NOFLAVOR) ) )
           {
              /* Color the object */
              strcpy(modstr, flavor[k_ptr->flavor].name);
              if (aware) append_name = TRUE;
              strcpy(basenm, aware ? "& Mushroom~" : "& # Mushroom~");
           }
           else if (!(k_ptr->flags3 & TR3_NOFLAVOR) )
           {
              strcpy(basenm, "& Mushroom~");
           }
           /* no else here, all mushrooms are flavored, other, non-flavored food has */
           /* another basename (food ration, pint of fine wine etc                   */

           break;

       case TV_BOOK:
       {
           s16b count = 0;
           s16b i;

           if ((mode & 0x40) == 0)
           {
              strcpy(modstr, basenm);
              for (i=0; i<s_number; i++)
              {
                 if (has_spell(i_ptr, i)) count++;
              }
              strcpy(basenm, format("& #~ (with %d spell%s)", count,
                     ((count>1)||(count==0))?"s":""));
           }
           else
              strcpy(basenm, "& Book~ of Spells");

           break;
       }
       /* Hack -- Gold/Gems */
       case TV_GOLD:
/* jk */
           /* extensive description */
           if ((mode & 0x40) == 0)
           {
              if ((i_ptr->number>1) && (pref))
              {
                 strcpy(buf, format("%d pieces of %s worth %ld",
                        i_ptr->number, basenm, i_ptr->p1val));
              }
              else
              {
                 strcpy(buf, format("%spiece of %s worth %ld",
                        pref ? "a ":"", basenm, i_ptr->p1val));
              }
              return;
           }
           else
           {
              strcpy(buf, format("%spiece%s of %s",
                     (pref && (i_ptr->number==1))?"a ":"some ",
                     (i_ptr->number>1)?"s":"", basenm));
              return;
           }

       case TV_CORPSE:
           {
              s16b r_idx = i_ptr->sval;
              /* the Corpse of Lagduf the Snaga */
              if (r_info[r_idx].flags1 & RF1_UNIQUE)
              {
                 strcpy(basenm, format("& corpse~ of %s", r_name + r_info[r_idx].name));
              }
              /* an orc corpse */
              else
                 strcpy(basenm, format("& %s corpse~", r_name + r_info[r_idx].name));

              break;
           }
       case TV_SKELETON:
           {
              if (i_ptr->sval > SV_SKELETON_BROKEN_END)
              {
                 s16b r_idx = i_ptr->sval - SV_SKELETON_BROKEN_END;
                 strcpy(basenm, format("& %s skeleton~", r_name + r_info[r_idx].name));
              }
              break;
           }

       /* Used in the "inventory" routine */
       default:
           strcpy(buf, "(nothing)");
           return;
   }
   /* is this a flavored object, and isn't it an artifact we're aware of? */
   if ( (do_flavor) && (! (artifact_p(i_ptr) && aware) ) )
   {
dlog(DEBUGITEMS,"object1.c: object_desc: do_flavor TRUE: flavor %d flav_aware %s flav_unaware %s\n",
                k_ptr->flavor, flav_aware, flav_unaware);
      /* describe beyond base item? */
      if ((mode & 0x40) == 0)
      {
         if (aware) append_name = TRUE;
         if ( !(k_ptr->flags3 & TR3_NOFLAVOR) )
         {
            /* Color the object */
            strcpy(modstr, flavor[k_ptr->flavor].name);
            strcpy(basenm, aware ? flav_aware : flav_unaware);
         }
         /* this has no flavor */
         else
         {
            strcpy(basenm, flav_aware);
         }
      }
      /* unaware, make it a Light Blue Potion */
      else
      {
         strcpy(modstr, flavor[k_ptr->flavor].name);
         strcpy(basenm, flav_unaware);
      }
   }

dlog(DEBUGITEMS,"object1.c: object_desc: modstd %s basenm %s append_name %d\n",
                modstr, basenm, append_name);

   /* Start dumping the result */
   t = buf;

   /* The object "expects" a "number" */
   if (basenm[0] == '&')
   {
       /* Skip the ampersand (and space) */
       s = basenm + 2;

       /* No prefix */
       if (!pref)
       {
           /* Nothing */
       }

       /* Hack -- None left */
       else if (i_ptr->number <= 0)
       {
           t = object_desc_str(t, "no more ");
       }

       /* Extract the number */
       else if (i_ptr->number > 1)
       {
           t = object_desc_num(t, i_ptr->number);
           t = object_desc_chr(t, ' ');
       }

       /* Hack -- The only one of its kind */
       else if (known && artifact_p(i_ptr))
       {
           t = object_desc_str(t, "The ");
       }

       /* A single one, with a vowel in the modifier */
       else if ((*s == '#') && (is_a_vowel(modstr[0])))
       {
           t = object_desc_str(t, "an ");
       }

       /* A single one, with a vowel */
       else if (is_a_vowel(*s))
       {
           t = object_desc_str(t, "an ");
       }

       /* A single one, without a vowel */
       else
       {
           t = object_desc_str(t, "a ");
       }
   }

   /* Hack -- objects that "never" take an article */
   else
   {
       /* No ampersand */
       s = basenm;

       /* No pref */
       if (!pref)
       {
           /* Nothing */
       }

       /* Hack -- all gone */
       else if (i_ptr->number <= 0)
       {
           t = object_desc_str(t, "no more ");
       }

       /* Prefix a number if required */
       else if (i_ptr->number > 1)
       {
           t = object_desc_num(t, i_ptr->number);
           t = object_desc_chr(t, ' ');
       }

       /* Hack -- The only one of its kind */
       else if (known && artifact_p(i_ptr))
       {
           t = object_desc_str(t, "The ");
       }

       /* Hack -- single items get no prefix */
       else
       {
           /* Nothing */
       }
   }

   /* Paranoia -- skip illegal tildes */
   /* while (*s == '~') s++; */
/* jk - add color to known items */
   if ( (!(mode & 0x20)) && (!(mode & 0x40)) && (color_known_items) && (aware) &&
        (mode>=0) && (!(k_ptr->flags3 & TR3_NOFLAVOR)) )
   {
      switch (i_ptr->tval)
      {
         case (TV_POTION) : t=object_desc_str(t,flavor[FLAVOR_POTION_START+i_ptr->sval].name);
                            t=object_desc_chr(t,' ');
                            break;
         case (TV_STAFF)  : t=object_desc_str(t,flavor[FLAVOR_STAFF_START+i_ptr->sval].name);
                            t=object_desc_chr(t,' ');
                            break;
         case (TV_WAND)   : t=object_desc_str(t,flavor[FLAVOR_WAND_START+i_ptr->sval].name);
                            t=object_desc_chr(t,' ');
                            break;
         case (TV_ROD)    : t=object_desc_str(t,flavor[FLAVOR_ROD_START+i_ptr->sval].name);
                            t=object_desc_chr(t,' ');
                            break;
         case (TV_AMULET) : t=object_desc_str(t,flavor[FLAVOR_AMULET_START+i_ptr->sval].name);
                            t=object_desc_chr(t,' ');
                            break;
         case (TV_RING)   : t=object_desc_str(t,flavor[FLAVOR_RING_START+i_ptr->sval].name);
                            t=object_desc_chr(t,' ');
                            break;
         case (TV_FOOD)   : t=object_desc_str(t,flavor[FLAVOR_FOOD_START+i_ptr->sval].name);
                            t=object_desc_chr(t,' ');
                            break;
      }
   }

   /* Copy the string */
   for ( ; *s; s++)
   {
      /* Pluralizer */
      if (*s == '~')
      {
         /* Add a plural if needed */
         if (i_ptr->number != 1)
         {
            char k = t[-1];

            /* XXX XXX XXX Mega-Hack */

            /* Hack -- "Cutlass-es" and "Torch-es" */
            if ((k == 's') || (k == 'h')) *t++ = 'e';

            /* Add an 's' */
            *t++ = 's';
         }
      }

      /* Modifier */
      else if (*s == '#')
      {
         /* Insert the modifier */
         for (u = modstr; *u; u++) *t++ = *u;
      }

      /* Normal */
      else
      {
         /* Copy */
         *t++ = *s;
      }
   }

   /* Terminate */
   *t = '\0';

   if ((mode & 0x1f) >=1 )
   {
      if ( (!(mode & ITEM_DESC_COLOR)) && (color_known_items) && (aware) &&
           ( (i_ptr->tval==TV_SCROLL) || (i_ptr->tval==TV_SPELL) ) )
      {
         t=object_desc_str(t," titled \"");
         switch (i_ptr->tval)
         {
            case TV_SCROLL: t=object_desc_str(t,flavor[FLAVOR_SCROLL_START+i_ptr->sval].name);
                            break;
            case TV_SPELL : t=object_desc_str(t,flavor[FLAVOR_SPELL_START+i_ptr->sval].name);
                            break;
         }
         t=object_desc_chr(t,'"');
      }
   }


   /* Append the "kind name" to the "base name" */
   if (append_name)
   {
      t = object_desc_str(t, " of ");
      t = object_desc_str(t, (k_name + k_ptr->name));
   }

   /* Hack -- Append "Artifact" or "Special" names */
   if (known && ((mode & 0x80) == 0))
   {
      /* Grab any artifact name */
      if (i_ptr->name1)
      {
         artifact_type *a_ptr = &a_info[i_ptr->name1];

         t = object_desc_chr(t, ' ');
         t = object_desc_str(t, (a_name + a_ptr->name));
      }

      /* Grab any ego-item name */
      else if (i_ptr->name2)
      {
         ego_item_type *e_ptr = &e_info[i_ptr->name2];

if (i_ptr->tval == TV_CHEST)
   dlog(DEBUGTRAPS,"object1.c: object_desc: adding ego-item name %d to chest\n",
                   i_ptr->name2);

         t = object_desc_chr(t, ' ');
         t = object_desc_str(t, (e_name + e_ptr->name));
      }
   }

   /* No more details wanted */
dlog(DEBUGITEMS,"object1.c: object_desc: mode & 0x1f = %d, testing return\n", (mode & 0x1f));
   if ((mode & 0x1f) < 1) return;

   /* Hack -- Chests must be described in detail */
   if (i_ptr->tval == TV_CHEST)
   {
dlog(DEBUGTRAPS,"object1.c: object_desc: chest known %d p1val %d xtra2 %d\n",
               known, i_ptr->p1val, i_ptr->xtra2);

      /* Not searched yet */
      if (!known)
      {
         /* Nothing */
      }
      /* May be "empty" */
      else if (!i_ptr->p1val)
      {
         t = object_desc_str(t, " (empty)");
      }
      else if (!i_ptr->xtra2)
      {
         t = object_desc_str(t, " (untrapped)");
      }
      else if (i_ptr->xtra2==-1)
      {
         t = object_desc_str(t, " (disarmed)");
      }
      /* May be "disarmed" */
      else
      {
         char trapname[80];
         trap_item_type *tr_ptr = &t_list[i_ptr->xtra2];
         describe_trap(trapname, tr_ptr);
dlog(DEBUGTRAPS,"object1.c: object_desc: trapname now %s\n", trapname);
         if (trapname[0])
         {
            t = object_desc_str(t," ");
            t = object_desc_str(t,trapname);
         }
      }
   }

   /* Display the item like a weapon */

   if (f3 & TR3_SHOW_MODS) show_weapon = TRUE;

   show_base = FALSE;
   if (need_tries(i_ptr) && (i_ptr->ident & ID_EQUIPTESTED)) show_base = TRUE;

   /* Display the item like a weapon */
   if (i_ptr->to_h && i_ptr->to_d) show_weapon = TRUE;

   /* Display the item like armour */
   if (i_ptr->ac) show_armour = TRUE;

   /* Dump base weapon info */
   switch (i_ptr->tval)
   {
      /* Missiles and Weapons */
      case TV_SHOT:
      case TV_BOLT:
      case TV_ARROW:
      case TV_HAFTED:
      case TV_POLEARM:
      case TV_SWORD:
      case TV_DIGGING:

         /* Append a "damage" string if allowed */
         if (show_base)
         {
            t = object_desc_chr(t, ' ');
            t = object_desc_chr(t, p1);
            t = object_desc_num(t, i_ptr->dd);
            t = object_desc_chr(t, 'd');
            t = object_desc_num(t, i_ptr->ds);
            t = object_desc_chr(t, p2);
         }
         /* All done */
         break;

      /* Bows get a special "damage string" */
      case TV_BOW:

         /* Mega-Hack -- Extract the "base power" */
         power = (i_ptr->sval % 10);

         if (show_base)
         {
            /* Append a special "damage" string */
            t = object_desc_chr(t, ' ');
            t = object_desc_chr(t, p1);
            t = object_desc_chr(t, 'x');
            t = object_desc_num(t, power);
            t = object_desc_chr(t, p2);
         }

         /* All done */
         break;
   }


   /* Add the weapon bonuses */
   if (known)
   {
      /* Show the tohit/todam on request */
      if (show_weapon)
      {
         t = object_desc_chr(t, ' ');
         t = object_desc_chr(t, p1);
         t = object_desc_int(t, i_ptr->to_h);
         t = object_desc_chr(t, ',');
         t = object_desc_int(t, i_ptr->to_d);
         t = object_desc_chr(t, p2);
      }

      /* Show the tohit if needed */
      else if (i_ptr->to_h)
      {
         t = object_desc_chr(t, ' ');
         t = object_desc_chr(t, p1);
         t = object_desc_int(t, i_ptr->to_h);
         t = object_desc_chr(t, p2);
      }

      /* Show the todam if needed */
      else if (i_ptr->to_d)
      {
          t = object_desc_chr(t, ' ');
          t = object_desc_chr(t, p1);
          t = object_desc_int(t, i_ptr->to_d);
          t = object_desc_chr(t, p2);
      }
   }


   /* Add the armor bonuses */
   if (known)
   {
      /* Show the armor class info */
      if (show_armour)
      {
         t = object_desc_chr(t, ' ');
         t = object_desc_chr(t, b1);
dlog(DEBUGITEMS,"object1.c: object_desc: show_armour, ac %d - about to describe\n", i_ptr->ac);
         if (i_ptr->ac <0)
         {
            t = object_desc_int(t, i_ptr->ac);
         }
         else
         {
            t = object_desc_num(t, i_ptr->ac);
         }
         t = object_desc_chr(t, ',');
dlog(DEBUGITEMS,"object1.c: object_desc: show_armour, to_a %d - about to describe\n", i_ptr->to_a);
         t = object_desc_int(t, i_ptr->to_a);
         t = object_desc_chr(t, b2);
      }

      /* No base armor, but does increase armor */
      else if (i_ptr->to_a)
      {
         t = object_desc_chr(t, ' ');
         t = object_desc_chr(t, b1);
         t = object_desc_int(t, i_ptr->to_a);
         t = object_desc_chr(t, b2);
      }
   }

   /* Hack -- always show base armor */
   else if (show_armour)
   {
      t = object_desc_chr(t, ' ');
      t = object_desc_chr(t, b1);
      t = object_desc_num(t, i_ptr->ac);
      t = object_desc_chr(t, b2);
   }


   /* No more details wanted */
   if (mode < 2) return;


   /* Hack -- Wands and Staffs have charges */
   if (known &&
       ((i_ptr->tval == TV_STAFF) ||
        (i_ptr->tval == TV_WAND)))
   {
      /* Dump " (N charges)" */
      t = object_desc_chr(t, ' ');
      t = object_desc_chr(t, p1);
      t = object_desc_num(t, i_ptr->p1val);
      t = object_desc_str(t, " charge");
      if (i_ptr->p1val != 1) t = object_desc_chr(t, 's');
      t = object_desc_chr(t, p2);
   }

   /* Hack -- Rods have a "charging" indicator */
   else if (known && (i_ptr->tval == TV_ROD))
   {
      /* Hack -- Dump " (charging)" if relevant */
      if (i_ptr->number>1)
      {
         s16b numch = (i_ptr->p1val + get_rod_charge(i_ptr)-1) / get_rod_charge(i_ptr);
         /* needed for the Trap of No Return for example */
         if (numch > i_ptr->number) numch = i_ptr->number;
         if (numch > 0)
         {
            t = object_desc_str(t, " (");
            t = object_desc_num(t, numch);
            t = object_desc_str(t, " charging)");
         }
      }
      else
      {
         if (i_ptr->p1val) t = object_desc_str(t, " (charging)");
      }
   }

   /* Hack -- Process Lanterns/Torches */
   else if ((i_ptr->tval == TV_LITE) && (!artifact_p(i_ptr)))
   {
      /* Hack -- Turns of light for normal lites */
      t = object_desc_str(t, " (with ");
      t = object_desc_num(t, i_ptr->p1val);
      t = object_desc_str(t, " turns of light)");
   }

   /* Dump "p1val" flags for wearable items */
   if (known && (f1 & TR1_P1VAL_MASK) && (i_ptr->p1val != 0))
   {
      /* Start the display */
      t = object_desc_chr(t, ' ');
      t = object_desc_chr(t, p1);

      /* Dump the "p1val" itself */
      t = object_desc_int(t, i_ptr->p1val);

      /* Do not display the "p1val" flags */
      if (f3 & TR3_HIDE_TYPE)
      {
         /* Nothing */
      }

      /* Speed */
      else if (f1 & TR1_SPEED1)
      {
         /* Dump " to speed" */
         t = object_desc_str(t, " to speed");
      }

      /* Attack speed */
      else if (f1 & TR1_BLOWS1)
      {
         /* Add " attack" */
         t = object_desc_str(t, " attack");

         /* Add "attacks" */
         if (ABS(i_ptr->p1val) != 1) t = object_desc_chr(t, 's');
      }

/* jk - add vampirism */
      else if (f1 & TR1_VAMPIRIC1)
      {
         t = object_desc_str(t," vampirism");
      }

      /* Stealth */
      else if (f1 & TR1_STEALTH1)
      {
         /* Dump " to stealth" */
         t = object_desc_str(t, " to stealth");
      }

      /* Search */
      else if (f1 & TR1_SEARCH1)
      {
         /* Dump " to searching" */
         t = object_desc_str(t, " to searching");
      }

      /* Infravision */
      else if (f1 & TR1_INFRA1)
      {
         /* Dump " to infravision" */
         t = object_desc_str(t, " to infravision");
      }

      /* Finish the display */
      t = object_desc_chr(t, p2);
   }

   /* Dump "p2val" flags for wearable items */
   if (known && (f1 & TR1_P2VAL_MASK) && (i_ptr->p2val != 0))
   {
      /* Start the display */
      t = object_desc_chr(t, ' ');
      t = object_desc_chr(t, p1);

      /* Dump the "p1val" itself */
      t = object_desc_int(t, i_ptr->p2val);

      /* Do not display the "p1val" flags */
      if (f3 & TR3_HIDE_TYPE)
      {
         /* Nothing */
      }

      /* Speed */
      else if (f1 & TR1_SPEED2)
      {
         /* Dump " to speed" */
         t = object_desc_str(t, " to speed");
      }

      /* Attack speed */
      else if (f1 & TR1_BLOWS2)
      {
         /* Add " attack" */
         t = object_desc_str(t, " attack");

         /* Add "attacks" */
         if (ABS(i_ptr->p1val) != 1) t = object_desc_chr(t, 's');
      }

/* jk - add vampirism */
      else if (f1 & TR1_VAMPIRIC1)
      {
         t = object_desc_str(t," vampirism");
      }

      /* Stealth */
      else if (f1 & TR1_STEALTH2)
      {
         /* Dump " to stealth" */
         t = object_desc_str(t, " to stealth");
      }

      /* Search */
      else if (f1 & TR1_SEARCH2)
      {
         /* Dump " to searching" */
         t = object_desc_str(t, " to searching");
      }

      /* Infravision */
      else if (f1 & TR1_INFRA2)
      {
         /* Dump " to infravision" */
         t = object_desc_str(t, " to infravision");
      }

      /* Finish the display */
      t = object_desc_chr(t, p2);
   }

   if (known && i_ptr->timeout) /* Indicate "charging" artifacts */
   {
      /* Hack -- Dump " (charging)" if relevant */
      t = object_desc_str(t, " (charging)");
   }

   /* No more details wanted */
   if (mode < 3) return;
dlog(DEBUGITEMS,"object1.c: object_desc: mode = %d, not returned yet\n", mode);

   /* No inscription yet */
   tmp_val[0] = '\0';

   /* Use the standard inscription if available */
   if (i_ptr->note)
   {
      strcpy(tmp_val, quark_str(i_ptr->note));
   }

   /* Note "cursed" if the item is known to be cursed */
   else if (cursed_p(i_ptr) && (known || (i_ptr->ident & ID_SENSE)))
   {
      strcpy(tmp_val, "cursed");
   }

   /* Mega-Hack -- note empty wands/staffs */
   else if (!known && (i_ptr->ident & ID_EMPTY))
   {
      strcpy(tmp_val, "empty");
   }

   /* Note "tried" if the object has been tested unsuccessfully */
   else if (!aware && object_tried_p(i_ptr))
   {
      strcpy(tmp_val, "tried");
   }

   /* Note the discount, if any */
   else if (i_ptr->discount)
   {
      object_desc_num(tmp_val, i_ptr->discount);
      strcat(tmp_val, "% off");
   }

   /* Append the inscription, if any */
   if (tmp_val[0])
   {
      s16b n;

      /* Hack -- How much so far */
      n = (t - buf);

      /* Paranoia -- do not be stupid */
      if (n > 75) n = 75;

      /* Hack -- shrink the inscription */
      tmp_val[75 - n] = '\0';

      /* Append the inscription */
      t = object_desc_chr(t, ' ');
      t = object_desc_chr(t, c1);
      t = object_desc_str(t, tmp_val);
      t = object_desc_chr(t, c2);
   }
}


/*
 * Hack -- describe an item currently in a store's inventory
 * This allows an item to *look* like the player is "aware" of it
 */
void object_desc_store(char *buf, object_type *i_ptr, bool pref, s16b mode)
{
   /* Save the "aware" flag */
   bool hack_aware = k_info[i_ptr->k_idx].aware;

   /* Save the "known" flag */
   bool hack_known = (i_ptr->ident & ID_KNOWN) ? TRUE : FALSE;

   /* Save the "tried" flag */
   bool hack_tried = (i_ptr->ident & ID_EQUIPTESTED) ? TRUE : FALSE;

   /* Set the "known" flag */
   i_ptr->ident |= ID_KNOWN;

   /* Set the "tried" flag */
   i_ptr->ident |= ID_EQUIPTESTED;

   /* Force "aware" for description */
   k_info[i_ptr->k_idx].aware = TRUE;

   /* Describe the object */
   object_desc(buf, i_ptr, pref, (mode | ITEM_DESC_COLOR ));

   /* Restore "aware" flag */
   k_info[i_ptr->k_idx].aware = hack_aware;

   /* Clear the known flag */
   if (!hack_known) i_ptr->ident &= ~ID_KNOWN;

   /* Clear the tried flag */
   if (!hack_tried) i_ptr->ident &= ~ID_EQUIPTESTED;
}

/*
 * Determine the "Activation" (if any) for an artifact
 * Return a string, or NULL for "no activation"
 * this is only called from object_desc and from the artifact generation
 * in wizard1.c. It might be called for an item without activation!
 */
cptr item_activation(object_type *i_ptr)
{
   u64b f1, f2, f3;

   /* Extract the flags */
   object_flags(i_ptr, &f1, &f2, &f3);

dlog(DEBUGITEMS,"object1.c: item_activation (item %d %d/%d): f3 & TR3_ACTIVATE %d / artif rod %d\n",
                i_ptr->k_idx, i_ptr->tval, i_ptr->sval,
                (f3 & TR3_ACTIVATE)?1:0,
                ( (i_ptr->tval==TV_ROD) && (i_ptr->name1!=0) )?1:0);

   /* Some artifacts can be activated */
   switch (i_ptr->name1)
   {
      case ART_NARTHANC:
          return "fire bolt (9d8) every 8+d8 turns";
      case ART_NIMTHANC:
          return "frost bolt (6d8) every 7+d7 turns";
      case ART_DETHANC:
          return "lightning bolt (4d8) every 6+d6 turns";
      case ART_RILIA:
          return "stinking cloud (12) every 4+d4 turns";
      case ART_BELANGIL:
          return "frost ball (48) every 5+d5 turns";
      case ART_DAL:
          return "remove fear and cure poison every 5 turns";
      case ART_RINGIL:
          return "frost ball (100) every 300 turns";
      case ART_ANDURIL:
          return "fire ball (72) every 400 turns";
      case ART_FIRESTAR:
          return "large fire ball (72) every 100 turns";
      case ART_FEANOR:
          return "haste self (20+d20 turns) every 200 turns";
      case ART_THEODEN:
          return "drain life (120) every 400 turns";
      case ART_TURMIL:
          return "drain life (90) every 70 turns";
      case ART_CASPANION:
          return "door and trap destruction every 10 turns";
      case ART_SUN:
          return "large lite ball (20d10) and berserk rage every 400 turns";
      case ART_AVAVIR:
          return "word of recall every 200 turns";
      case ART_TARATOL:
          return "haste self (20+d20 turns) every 100+d100 turns";
      case ART_ERIRIL:
          return "identify every 10 turns";
      case ART_OLORIN:
          return "probing every 20 turns";
      case ART_EONWE:
          return "mass genocide every 1000 turns";
      case ART_LOTHARANG:
          return "cure wounds (4d7) every 3+d3 turns";
      case ART_CUBRAGOL:
          return "fire branding of bolts every 999 turns";
      case ART_ARUNRUTH:
          return "frost bolt (12d8) every 500 turns";
      case ART_AEGLOS:
          return "frost ball (100) every 500 turns";
      case ART_OROME:
          return "stone to mud every 5 turns";
      case ART_SOULKEEPER:
          return "heal (1000) every 888 turns";
      case ART_BELEGENNON:
          return "phase door every 2 turns";
      case ART_CELEBORN:
          return "genocide every 500 turns";
      case ART_LUTHIEN:
          return "restore life levels every 450 turns";
      case ART_ULMO:
          return "teleport away every 150 turns";
      case ART_COLLUIN:
          return "resistance (20+d20 turns) every 111 turns";
      case ART_HOLCOLLETH:
          return "Sleep II every 55 turns";
      case ART_THINGOL:
          return "recharge item I every 70 turns";
      case ART_COLANNON:
          return "teleport every 45 turns";
      case ART_TOTILA:
          return "confuse monster every 15 turns";
      case ART_CAMMITHRIM:
          return "magic missile (2d6) every 2 turns";
      case ART_PAURHACH:
          return "fire bolt (9d8) every 8+d8 turns";
      case ART_PAURNIMMEN:
          return "frost bolt (6d8) every 7+d7 turns";
      case ART_PAURAEGEN:
          return "lightning bolt (4d8) every 6+d6 turns";
      case ART_PAURNEN:
          return "acid bolt (5d8) every 5+d5 turns";
      case ART_FINGOLFIN:
          return "a magical arrow (150) every 90+d90 turns";
      case ART_HOLHENNETH:
          return "detection every 55+d55 turns";
      case ART_GONDOR:
          return "heal (500) every 500 turns";
      case ART_RAZORBACK:
          return "star ball (150) every 1000 turns";
      case ART_BLADETURNER:
          return "berserk rage, bless, and resistance every 400 turns";
      case ART_GALADRIEL:
          return "illumination every 10+d10 turns";
      case ART_ELENDIL:
          return "magic mapping every 50+d50 turns";
      case ART_THRAIN:
          return "clairvoyance every 100+d100 turns";
      case ART_INGWE:
          return "dispel evil (x5) every 300+d300 turns";
      case ART_CARLAMMAS:
          return "protection from evil every 225+d225 turns";
      case ART_TULKAS:
          return "haste self (75+d75 turns) every 150+d150 turns";
      case ART_NARYA:
          return "large fire ball (120) every 225+d225 turns";
      case ART_NENYA:
          return "large frost ball (200) every 325+d325 turns";
      case ART_VILYA:
          return "large lightning ball (250) every 425+d425 turns";
      case ART_POWER:
          return "bizarre things every 450+d450 turns";
/* jk */
      case ART_HURIN:
          return "berserker and +10 to speed (50) every 100+d200 turns";
      case ART_SARUMAN:
          return "intensive confuse in a 2+1d2 radius every 100+d100 turns";
      case ART_GOTHMOG:
          return "large fire ball (300) every 200+d200 turns";

      case ART_CLEARTHINKING:
          return "heal confusion, restore mana and confuse monsters in a 2+1d2 radius every 150 turns";
      case ART_DREAMS:
          return "get detection and sleep monsters in a 2+1d2 radius every 150 turns";
      case ART_ESCAPE:
          return "cast a darkness ball (150), darken a room, blink and +10 to speed (50) every 150 turns";
      case ART_PLENTY:
          return "grant healing(200) and some food every 150 turns";
      case ART_KNOWLEDGE:
          return "grant illumination, light ball(150), identify item and dispel evil(300) every 150 turns";
   }

   /* Require dragon scale mail */
/* jk - not any more, see boots of jumping */

   if (i_ptr->tval == TV_DRAG_ARMOR)
   {
      /* Branch on the sub-type */
      switch (i_ptr->sval)
      {
         case SV_DRAGON_BLUE:
             return "breathe lightning (100) every 450+d450 turns";
         case SV_DRAGON_WHITE:
             return "breathe frost (110) every 450+d450 turns";
         case SV_DRAGON_BLACK:
             return "breathe acid (130) every 450+d450 turns";
         case SV_DRAGON_GREEN:
             return "breathe poison gas (150) every 450+d450 turns";
         case SV_DRAGON_RED:
             return "breathe fire (200) every 450+d450 turns";
         case SV_DRAGON_MULTIHUED:
             return "breathe multi-hued (250) every 225+d225 turns";
         case SV_DRAGON_BRONZE:
             return "breathe confusion (120) every 450+d450 turns";
         case SV_DRAGON_GOLD:
             return "breathe sound (130) every 450+d450 turns";
         case SV_DRAGON_CHAOS:
             return "breathe chaos/disenchant (220) every 300+d300 turns";
         case SV_DRAGON_LAW:
             return "breathe sound/shards (230) every 300+d300 turns";
         case SV_DRAGON_BALANCE:
             return "breathe balance (250) every 300+d300 turns";
         case SV_DRAGON_SHINING:
             return "breathe light/darkness (200) every 300+d300 turns";
         case SV_DRAGON_POWER:
             return "breathe the elements (300) every 300+d300 turns";
      }
   }
   if ((i_ptr->tval==TV_BOOTS) && (i_ptr->name2==EGO_JUMP))
   {
      return "phase door every 10+d10 turns";
   }
   if ((i_ptr->tval==TV_HELM) && (i_ptr->name2==EGO_NOLDOR))
   {
      return "detect treasure every 10+d20 turns";
   }
   if ((i_ptr->tval==TV_HAFTED) && (i_ptr->sval==SV_QUARTERSTAFF))
   {
      if (i_ptr->name2==EGO_MAGE)
      {
         return "cast one spell 100+d200 turns without fail by a mage";
      }
      else if (i_ptr->name2==EGO_ADEPT)
      {
         return "cast 1 of 2 spells every 100+d200 turns without fail by a mage";
      }
      else if (i_ptr->name2==EGO_ARCHMAGE)
      {
         return "cast 1 of 3 spells every 100+d200 turns without fail by a mage";
      }
   }

   /* Oops */
   return NULL;
}

/*
 * describe how the player got an item
 */
void expand_item_log(item_log_type *l_ptr, char *log_description)
{
   char where_str[80];
   char result[2048];

/*
 * describe the level this item was smurfed on
 */
   if (l_ptr->mlevel)
   {
      if (depth_in_feet)
      {
         if (l_ptr->slevel == 0)
         {
            sprintf(where_str,"at %d ft.", l_ptr->mlevel*50);
         }
         else
         {
            sprintf(where_str,"at %d/%d ft.", l_ptr->mlevel*50, l_ptr->slevel*5);
         }
      }
      else
      {
         if (l_ptr->slevel == 0)
         {
            sprintf(where_str,"on level %d.", l_ptr->mlevel);
         }
         else
         {
            sprintf(where_str,"on level %d/%d.", l_ptr->mlevel, l_ptr->slevel);
         }
      }
   }
   else
   {
      sprintf(where_str,"in town.");
   }

   if (l_ptr->where & OBJ_FOUND_MONSTER)
   {
      char m_name[80];
      monster_type forge;
      forge.r_idx = l_ptr->whose;
      forge.ml = TRUE;
      monster_desc(m_name, &forge, 0x88);
      if (l_ptr->mlevel == 0)
      {
         strcpy(result, format("Found %s", where_str));
      }
      else
      {
         /* if we disregard that this is found by a monster and may be part of a stack,
          * where did we find it?
          */
         switch (l_ptr->where & ~(OBJ_FOUND_MONSTER|OBJ_FOUND_SOME) )
         {
            case OBJ_FOUND_ROOM  : strcpy(result, format("Carried by %s in a room %s", m_name, where_str)); break;
            case OBJ_FOUND_TUNNEL: strcpy(result, format("Carried by %s in a tunnel %s", m_name, where_str)); break;
            case OBJ_FOUND_MAZE  : strcpy(result, format("Carried by %s in a maze %s", m_name, where_str)); break;
            case OBJ_FOUND_VAULT : strcpy(result, format("Carried by %s in a vault %s", m_name, where_str)); break;
            case OBJ_FOUND_NEST  : strcpy(result, format("Carried by %s in a monster-nest %s", m_name, where_str)); break;
            case OBJ_FOUND_PIT   : strcpy(result, format("Carried by %s in a monster-pit %s", m_name, where_str)); break;
            default: strcpy(result, format("Carried by %s in an unexplained location %s", m_name, where_str)); break;
         }
      }
   }
/*
 * it was not carried by a monster. It may have been bought...
 */
   else if (l_ptr->where & OBJ_FOUND_STORE)
   {
      s16b st_type, st_owner, flavor;
      char s_name[80];
      char o_name[80];
      st_type = l_ptr->whose / 30;
      flavor = store_flavor_type(st_type);
      st_owner = l_ptr->whose % 30;
      strcpy(s_name, f_name + f_info[get_f_idx(DUNG_ENTR, st_type)].name);
      strcpy(o_name, owners[flavor][st_owner].owner_name);
      strcpy(result, format("Bought from %s in %s %s %s", o_name, (l_ptr->mlevel==0)?"the":"a", s_name, where_str));
   }
/*
 * it was not carried by a monster, nor bought. It may have been lying around somewhere...
 */
   else
   {
      switch (l_ptr->where & ~(OBJ_FOUND_MONSTER|OBJ_FOUND_SOME) )
      {
         case OBJ_FOUND_BIRTH  : strcpy(result, format("This was part of your starting equipment.")); break;
         case OBJ_FOUND_ROOM   : strcpy(result, format("Found in a room %s", where_str)); break;
         case OBJ_FOUND_TRAP   : strcpy(result, format("Flung out of a trap %s", where_str)); break;
         case OBJ_FOUND_POISON : strcpy(result, format("Created by poison gas %s", where_str)); break;
         case OBJ_FOUND_PLENTY : strcpy(result, format("A Gift from the Ancient Rod of Plenty %s", where_str)); break;
         case OBJ_FOUND_TUNNEL : strcpy(result, format("Found in a tunnel %s", where_str)); break;
         case OBJ_FOUND_MAZE   : strcpy(result, format("Found in a maze %s", where_str)); break;
         case OBJ_FOUND_WIZARD : strcpy(result, format("You cheated and created this %s", where_str)); break;
         case OBJ_FOUND_KILLED : strcpy(result, format("You slew this monster %s", where_str)); break;
         case OBJ_FOUND_TRAPZAP: strcpy(result, format("A trap killed this monster %s", where_str)); break;
         case OBJ_FOUND_WIZKILL: strcpy(result, format("You cheated and slew this monster %s", where_str)); break;
         case OBJ_FOUND_MONKILL: 
            {
               char m_name[80];
               monster_type forge;
               forge.r_idx = l_ptr->whose;
               forge.ml = TRUE;
               monster_desc(m_name, &forge, 0x88);
               strcpy(result, format("This monster was killed by %s %s", m_name, where_str));
            }
            break;
         default: strcpy(result, format("This was obtained in an unknown manner (%d,%d) %d %lx", 
                                        l_ptr->mlevel, l_ptr->slevel, l_ptr->whose, l_ptr->where)); break; 
      }
   }
   /* now make sure we say something if this is part of a stack */
   if (l_ptr->where & OBJ_FOUND_SOME)
   {
dlog(DEBUGITEMS,"object1.c: expand_item_log: result %s\n", result);
dlog(DEBUGITEMS,"object1.c: expand_item_log: where %08lx when %d whose %d mlevel %d slevel %d\n",
                l_ptr->where, l_ptr->found_when, l_ptr->whose,
                l_ptr->mlevel, l_ptr->slevel);
      strcat(result," You remember this only about a part of this set of items.");
   }
   strcpy(log_description, result);
}

/*
 * this function adds string src obj_info_type info,
 * and makes it more lines if necessary (linelen > max_len)
 */
static s16b add_obj_info_line(obj_info_type *info, s16b curr_line, s16b max_len, char *src)
{
   s16b numcr = 0, startpos = 0;

   /* split when lines overflow */
   if (strlen(src)>max_len)
   {
      while ( (strlen(src) - numcr * max_len ) > max_len)
      {
         s16b pos = (numcr+1)*max_len;
         /* search backwards for a space - which has to be there! */
         while (src[pos]!=' ') pos--;
         /* temporary split the string here */
         src[pos]=0;
         strcpy(info->str[curr_line++], src+startpos);
         /* next time, start 1 char further */
         startpos = pos+1;
         /* restore the string */
         src[pos]=' ';
         numcr++;
      }
      /* and get the last bit as well */
      strcpy(info->str[curr_line++], src+startpos);
   }
   else
   {
      strcpy(info->str[curr_line++], src);
   }
   return (curr_line);
}

/*
 * this function describes the origins of an item
 */
static s16b identify_fully_origin(object_type *i_ptr, s16b curr_line, s16b max_len, obj_info_type *info)
{
   char  tempstr[2048];

   expand_item_log(&i_ptr->log, tempstr);
   curr_line = add_obj_info_line(info, curr_line, max_len, tempstr);
   strcpy(info->str[curr_line++],"  ");
   return (curr_line);
}

/*
 * this functions describes a mages staff
 */
static s16b identify_fully_mage_staff(object_type *i_ptr, s16b curr_line,
                                      s16b max_len, obj_info_type *info)
{
   s16b j, cnt = 0;

   for (j=0; j<s_number; j++)
   {
      if (!has_spell(i_ptr, j)) continue;
      add_obj_info_line(info, curr_line, max_len, format("It can cast %s.", s_info[j].name + s_name));
      cnt++;
   }
   if (!cnt && (i_ptr->name2==EGO_MAGE))
      strcpy(info->str[curr_line++], "It can still absorb a spell.");
   else if (!cnt && (i_ptr->name2==EGO_ADEPT))
      strcpy(info->str[curr_line++], "It can still absorb two spells.");
   else if ((cnt==1) && (i_ptr->name2==EGO_ADEPT))
      strcpy(info->str[curr_line++], "It can still absorb a spell.");
   else if (!cnt && (i_ptr->name2==EGO_ARCHMAGE))
      strcpy(info->str[curr_line++], "It can still absorb three spells.");
   else if ((cnt==1) && (i_ptr->name2==EGO_ARCHMAGE))
      strcpy(info->str[curr_line++], "It can still absorb two spells.");
   else if ((cnt==2) && (i_ptr->name2==EGO_ARCHMAGE))
      strcpy(info->str[curr_line++], "It can still absorb a spell.");
   return (curr_line);
}

/*
 * this function describes a spell
 */
static s16b identify_fully_spell(object_type *i_ptr, s16b curr_line,
                                 s16b max_len, obj_info_type *info)
{
   cptr scales[5] = {"trivial","simple","advanced","difficult"};
   s16b lev = s_info[i_ptr->sval].level;
   s16b j, num = 0;
   char tempstr[1024];

   for (j=0; j<MAX_CLASS; j++)
   {
     if (s_info[i_ptr->sval].sclass & (1L<<j))
     {
        num++;
     }
   }
   if (num>0)
   {
      sprintf(tempstr,"It can be used by ");
   }
   for (j=0; j<MAX_CLASS; j++)
   {
     if (s_info[i_ptr->sval].sclass & (1L<<j))
     {
        /* all add ", ", the one before last adds " and ", the last adds "." */
        if (num>2)
        {
           strcat(tempstr, format("a%s %s, ",
                                  is_a_vowel(class_info[j].title[0])?"n":"",
                                  class_info[j].title));
        }
        else if (num==2)
        {
           strcat(tempstr, format("a%s %s and ",
                                  is_a_vowel(class_info[j].title[0])?"n":"",
                                  class_info[j].title));
        }
        else if (num==1)
        {
           strcat(tempstr, format("a%s %s.",
                                  is_a_vowel(class_info[j].title[0])?"n":"",
                                  class_info[j].title));
        }
        num--;
     }
   }
   curr_line = add_obj_info_line(info, curr_line, max_len, tempstr);

   if (lev<5)
      sprintf(tempstr,"It is an obvious, ");
   else if (lev<12)
      sprintf(tempstr,"It is a well-known, ");
   else if (lev<30)
      sprintf(tempstr,"It is a common, ");
   else if (lev<45)
      sprintf(tempstr,"It is a rare, ");
   else
      sprintf(tempstr,"It is an obscure, ");

   strcat(tempstr, format("%s spell ", scales[s_info[i_ptr->sval].scale]));

   switch (s_info[i_ptr->sval].type)
   {
      case 0: strcat(tempstr, "of the 'Attack with Nature' category.");      break;
      case 1: strcat(tempstr, "of the 'Attack with Dark Forces' category."); break;
      case 2: strcat(tempstr, "of the 'Escape' category.");                  break;
      case 3: strcat(tempstr, "of the 'Heal' category.");                    break;
      case 4: strcat(tempstr, "of the 'Sense' category.");                   break;
      case 5: strcat(tempstr, "of the 'Change Other Being' category.");      break;
      case 6: strcat(tempstr, "of the 'Change Item' category.");             break;
      case 7: strcat(tempstr, "of the 'Change Self' category.");              break;
      case 8: strcat(tempstr, "of the 'Change World' category.");            break;
   }
   curr_line = add_obj_info_line(info, curr_line, max_len, tempstr);

   return (curr_line);
}

/*
 * this function describes all effects from flags1
 */
static s16b identify_fully_flags1(object_type *i_ptr, s16b curr_line,
                                  s16b max_len, obj_info_type *info)
{
   u64b f1,f2,f3;
   char tempstr[1024], firststr[1024], laststr[80];
   bool done_one = FALSE;

   object_flags(i_ptr, &f1, &f2, &f3);

   /* empty the info and the last part of the info */
   tempstr[0]=0;
   laststr[0]=0;

   /* And then describe it fully - first the first part */
   if (i_ptr->p1val >0 )
   {
      sprintf(firststr,"It positively affects ");
   }
   else if (i_ptr->p1val < 0)
   {
      sprintf(firststr,"It has a negative effect on ");
   }
   else if (i_ptr->p1val == 0)
   {
      return (curr_line);
   }

   if (f1 & TR1_STR1)
   {
      sprintf(laststr,"your strength");
      done_one = TRUE;
   }
   if (f1 & TR1_INT1)
   {
      strcat(tempstr, laststr);
      if (!done_one)
      {
         done_one = TRUE;
      }
      else
      {
         strcat(tempstr,", ");
      }
      sprintf(laststr,"your intelligence");
   }
   if (f1 & TR1_WIS1)
   {
      strcat(tempstr, laststr);
      if (!done_one)
      {
         done_one = TRUE;
      }
      else
      {
         strcat(tempstr,", ");
      }
      sprintf(laststr,"your wisdom");
   }
   if (f1 & TR1_DEX1)
   {
      strcat(tempstr, laststr);
      if (!done_one)
      {
         done_one = TRUE;
      }
      else
      {
         strcat(tempstr,", ");
      }
      sprintf(laststr,"your dexterity");
   }
   if (f1 & TR1_CON1)
   {
      strcat(tempstr, laststr);
      if (!done_one)
      {
         done_one = TRUE;
      }
      else
      {
         strcat(tempstr,", ");
      }
      sprintf(laststr,"your constitution");
   }
   if (f1 & TR1_CHR1)
   {
      strcat(tempstr, laststr);
      if (!done_one)
      {
         done_one = TRUE;
      }
      else
      {
         strcat(tempstr,", ");
      }
      sprintf(laststr,"your charisma");
   }
   if (f1 & TR1_LITE1)
   {
      strcat(tempstr, laststr);
      if (!done_one)
      {
         done_one = TRUE;
      }
      else
      {
         strcat(tempstr,", ");
      }
      sprintf(laststr,"your permanent lite radius");
   }

   if (f1 & TR1_STEALTH1)
   {
      strcat(tempstr, laststr);
      if (!done_one)
      {
         done_one = TRUE;
      }
      else
      {
         strcat(tempstr,", ");
      }
      sprintf(laststr,"your stealth");
   }
   if (f1 & TR1_SEARCH1)
   {
      strcat(tempstr, laststr);
      if (!done_one)
      {
         done_one = TRUE;
      }
      else
      {
         strcat(tempstr,", ");
      }
      sprintf(laststr,"your searching");
   }
   if (f1 & TR1_INFRA1)
   {
      strcat(tempstr, laststr);
      if (!done_one)
      {
         done_one = TRUE;
      }
      else
      {
         strcat(tempstr,", ");
      }
      sprintf(laststr,"your infravision");
   }
   if (f1 & TR1_TUNNEL1)
   {
      strcat(tempstr, laststr);
      if (!done_one)
      {
         done_one = TRUE;
      }
      else
      {
         strcat(tempstr,", ");
      }
      sprintf(laststr,"your ability to dig");
   }
   if (f1 & TR1_SPEED1)
   {
      strcat(tempstr, laststr);
      if (!done_one)
      {
         done_one = TRUE;
      }
      else
      {
         strcat(tempstr,", ");
      }
      sprintf(laststr,"your speed");
   }
   if (f1 & TR1_BLOWS1)
   {
      strcat(tempstr, laststr);
      if (!done_one)
      {
         done_one = TRUE;
      }
      else
      {
         strcat(tempstr,", ");
      }
      sprintf(laststr,"your attack speed");
   }

   if (f1 & TR1_SHOTS1)
   {
      strcat(tempstr, laststr);
      if (!done_one)
      {
         done_one = TRUE;
      }
      else
      {
         strcat(tempstr,", ");
      }
      sprintf(laststr,"your shooting speed");
   }
   if (f1 & TR1_MIGHT1)
   {
      strcat(tempstr, laststr);
      if (!done_one)
      {
         done_one = TRUE;
      }
      else
      {
         strcat(tempstr,", ");
      }
      sprintf(laststr,"your shooting power");
   }
   if (f1 & TR1_VAMPIRIC1)
   {
      strcat(tempstr, laststr);
      if (!done_one)
      {
         done_one = TRUE;
      }
      else
      {
         strcat(tempstr,", ");
      }
      sprintf(laststr,"your healing during battle");
   }
   if (f1 & TR1_MAGIC1)
   {
      strcat(tempstr, laststr);
      if (!done_one)
      {
         done_one = TRUE;
      }
      else
      {
         strcat(tempstr,", ");
      }
      sprintf(laststr,"your skill with magical devices");
   }

   if (laststr[0]!=0)
   {
      /* tempstr = your strength, your intelligence */
      /* laststr = your constitution */
      if ((tempstr[0] != '\0') && (tempstr[0] != ','))
      {
         /* remove the last ", " */
         tempstr[strlen(tempstr)-2]=0;
         if (tempstr[0] != 0)
         {
            strcat(tempstr," and ");
         }
         strcat(tempstr,laststr);
         strcat(firststr, tempstr);
         strcat(firststr,".");
         curr_line = add_obj_info_line(info, curr_line, max_len, firststr);
      }
      /* tempstr = , or <empty>  */
      /* laststr = your charisma */
      else if ((tempstr[0] == ',') || (tempstr[0] == '\0') )
      {
         strcpy(tempstr,laststr);
         strcat(firststr, tempstr);
         strcat(firststr,".");
         curr_line = add_obj_info_line(info, curr_line, max_len, firststr);
      }
   }


   if (i_ptr->p2val != 0)
   {
      /* empty the info and the last part of the info */
      firststr[0]=0;
      tempstr[0]=0;
      laststr[0]=0;
      done_one = FALSE;

      /* And then describe it fully - first the first part */
      if (i_ptr->p2val > 0 )
      {
         if (i_ptr->p1val > 0)
         {
            if (i_ptr->p2val < i_ptr->p1val)
            {
               sprintf(firststr,"It has a smaller positive effect on ");
            }
            else if (i_ptr->p2val == i_ptr->p1val)
            {
               sprintf(firststr,"It has the same positive effect on ");
            }
            else
            {
               sprintf(firststr,"It has a bigger positive effect on ");
            }
         }
         else
         {
            sprintf(firststr,"It has a positive effect on ");
         }
      }
      else if (i_ptr->p2val < 0)
      {
         if (i_ptr->p1val < 0)
         {
            if (i_ptr->p2val < i_ptr->p1val)
            {
               sprintf(firststr,"It has a bigger negative effect on ");
            }
            else if (i_ptr->p2val == i_ptr->p1val)
            {
               sprintf(firststr,"It has the same negative effect on ");
            }
            else
            {
               sprintf(firststr,"It has a smaller negative effect on ");
            }
         }
         else
         {
            sprintf(firststr,"It has a negative effect on ");
         }
      }

      if (f1 & TR1_STR2)
      {
         sprintf(laststr,"your strength");
         done_one = TRUE;
      }
      if (f1 & TR1_INT2)
      {
         strcat(tempstr, laststr);
         if (!done_one)
         {
            done_one = TRUE;
         }
         else
         {
            strcat(tempstr,", ");
         }
         sprintf(laststr,"your intelligence");
      }
      if (f1 & TR1_WIS2)
      {
         strcat(tempstr, laststr);
         if (!done_one)
         {
            done_one = TRUE;
         }
         else
         {
            strcat(tempstr,", ");
         }
         sprintf(laststr,"your wisdom");
      }
      if (f1 & TR1_DEX2)
      {
         strcat(tempstr, laststr);
         if (!done_one)
         {
            done_one = TRUE;
         }
         else
         {
            strcat(tempstr,", ");
         }
         sprintf(laststr,"your dexterity");
      }
      if (f1 & TR1_CON2)
      {
         strcat(tempstr, laststr);
         if (!done_one)
         {
            done_one = TRUE;
         }
         else
         {
            strcat(tempstr,", ");
         }
         sprintf(laststr,"your constitution");
      }
      if (f1 & TR1_CHR2)
      {
         strcat(tempstr, laststr);
         if (!done_one)
         {
            done_one = TRUE;
         }
         else
         {
            strcat(tempstr,", ");
         }
         sprintf(laststr,"your charisma");
      }
      if (f1 & TR1_LITE2)
      {
         strcat(tempstr, laststr);
         if (!done_one)
         {
            done_one = TRUE;
         }
         else
         {
            strcat(tempstr,", ");
         }
         sprintf(laststr,"your permanent lite radius");
      }

      if (f1 & TR1_STEALTH2)
      {
         strcat(tempstr, laststr);
         if (!done_one)
         {
            done_one = TRUE;
         }
         else
         {
            strcat(tempstr,", ");
         }
         sprintf(laststr,"your stealth");
      }
      if (f1 & TR1_SEARCH2)
      {
         strcat(tempstr, laststr);
         if (!done_one)
         {
            done_one = TRUE;
         }
         else
         {
            strcat(tempstr,", ");
         }
         sprintf(laststr,"your searching");
      }
      if (f1 & TR1_INFRA2)
      {
         strcat(tempstr, laststr);
         if (!done_one)
         {
            done_one = TRUE;
         }
         else
         {
            strcat(tempstr,", ");
         }
         sprintf(laststr,"your infravision");
      }
      if (f1 & TR1_TUNNEL2)
      {
         strcat(tempstr, laststr);
         if (!done_one)
         {
            done_one = TRUE;
         }
         else
         {
            strcat(tempstr,", ");
         }
         sprintf(laststr,"your ability to dig");
      }
      if (f1 & TR1_SPEED2)
      {
         strcat(tempstr, laststr);
         if (!done_one)
         {
            done_one = TRUE;
         }
         else
         {
            strcat(tempstr,", ");
         }
         sprintf(laststr,"your speed");
      }
      if (f1 & TR1_BLOWS2)
      {
         strcat(tempstr, laststr);
         if (!done_one)
         {
            done_one = TRUE;
         }
         else
         {
            strcat(tempstr,", ");
         }
         sprintf(laststr,"your attack speed");
      }

      if (f1 & TR1_SHOTS2)
      {
         strcat(tempstr, laststr);
         if (!done_one)
         {
            done_one = TRUE;
         }
         else
         {
            strcat(tempstr,", ");
         }
         sprintf(laststr,"your shooting speed");
      }
      if (f1 & TR1_MIGHT2)
      {
         strcat(tempstr, laststr);
         if (!done_one)
         {
            done_one = TRUE;
         }
         else
         {
            strcat(tempstr,", ");
         }
         sprintf(laststr,"your shooting power");
      }
      if (f1 & TR1_VAMPIRIC2)
      {
         strcat(tempstr, laststr);
         if (!done_one)
         {
            done_one = TRUE;
         }
         else
         {
            strcat(tempstr,", ");
         }
         sprintf(laststr,"your healing during battle");
      }
      if (f1 & TR1_MAGIC2)
      {
         strcat(tempstr, laststr);
         if (!done_one)
         {
            done_one = TRUE;
         }
         else
         {
            strcat(tempstr,", ");
         }
         sprintf(laststr,"your skill with magical devices");
      }

      if (laststr[0] != '\0')
      {
         /* tempstr = your strength, your intelligence */
         /* laststr = your constitution */
         if ((tempstr[0] != '\0') && (tempstr[0] != ','))
         {
            /* remove the last ", " */
            tempstr[strlen(tempstr)-2]=0;
            if (tempstr[0] != 0)
            {
               strcat(tempstr," and ");
            }
            strcat(tempstr,laststr);
            strcat(firststr, tempstr);
            strcat(firststr,".");
            curr_line = add_obj_info_line(info, curr_line, max_len, firststr);
         }
         /* tempstr = , or <empty>  */
         /* laststr = your charisma */
         else if ((tempstr[0] == ',') || (tempstr[0] == '\0') )
         {
            strcpy(tempstr,laststr);
            strcat(firststr, tempstr);
            strcat(firststr,".");
            curr_line = add_obj_info_line(info, curr_line, max_len, firststr);
         }
      }
   }

   if (f1 & TR1_BRAND_ACID) strcpy(info->str[curr_line++],"It does extra damage from acid.");
   if (f1 & TR1_BRAND_ELEC) strcpy(info->str[curr_line++],"It does extra damage from electricity.");
   if (f1 & TR1_BRAND_FIRE) strcpy(info->str[curr_line++],"It does extra damage from fire.");
   if (f1 & TR1_BRAND_COLD) strcpy(info->str[curr_line++],"It does extra damage from frost.");
   if (f1 & TR1_IMPACT)     strcpy(info->str[curr_line++],"It can cause earthquakes.");
   if (f1 & TR1_KILL_DRAGON) strcpy(info->str[curr_line++],"It is a great bane of dragons.");
   if (f1 & TR1_SLAY_DRAGON) strcpy(info->str[curr_line++],"It is especially deadly against dragons.");
   if (f1 & TR1_SLAY_ORC)    strcpy(info->str[curr_line++],"It is especially deadly against orcs.");
   if (f1 & TR1_KILL_ORC)    strcpy(info->str[curr_line++],"It is a great bane of orcs.");
   if (f1 & TR1_SLAY_TROLL)  strcpy(info->str[curr_line++],"It is especially deadly against trolls.");
   if (f1 & TR1_KILL_TROLL)  strcpy(info->str[curr_line++],"It is a great bane of trolls.");
   if (f1 & TR1_SLAY_GIANT)  strcpy(info->str[curr_line++],"It is especially deadly against giants.");
   if (f1 & TR1_KILL_GIANT)  strcpy(info->str[curr_line++],"It is a great bane of giants.");
   if (f1 & TR1_SLAY_DEMON)  strcpy(info->str[curr_line++],"It strikes at demons with holy wrath.");
   if (f1 & TR1_KILL_DEMON)  strcpy(info->str[curr_line++],"It delivers holy vengeance against demons.");
   if (f1 & TR1_SLAY_UNDEAD) strcpy(info->str[curr_line++],"It strikes at undead with holy wrath.");
   if (f1 & TR1_KILL_UNDEAD) strcpy(info->str[curr_line++],"It delivers holy vengeance against undead.");
   if (f1 & TR1_SLAY_EVIL)   strcpy(info->str[curr_line++],"It fights against evil with holy fury.");
   if (f1 & TR1_KILL_EVIL)   strcpy(info->str[curr_line++],"It rages against evil with holy fury.");
   if (f1 & TR1_SLAY_ANIMAL) strcpy(info->str[curr_line++],"It is especially deadly against natural creatures.");
   if (f1 & TR1_KILL_ANIMAL) strcpy(info->str[curr_line++],"It is a great bane of natural creatures.");

   return (curr_line);
}

/*
 * this function describes all effects from flags2
 */
static s16b identify_fully_flags2(object_type *i_ptr, s16b curr_line,
                                  s16b max_len, obj_info_type *info)
{
   u64b f1,f2,f3;

   object_flags(i_ptr, &f1, &f2, &f3);

   if (f2 & TR2_SUST_STR)   strcpy(info->str[curr_line++],"It sustains your strength.");
   if (f2 & TR2_SUST_INT)   strcpy(info->str[curr_line++],"It sustains your intelligence.");
   if (f2 & TR2_SUST_WIS)   strcpy(info->str[curr_line++],"It sustains your wisdom.");
   if (f2 & TR2_SUST_DEX)   strcpy(info->str[curr_line++],"It sustains your dexterity.");
   if (f2 & TR2_SUST_CON)   strcpy(info->str[curr_line++],"It sustains your constitution.");
   if (f2 & TR2_SUST_CHR)   strcpy(info->str[curr_line++],"It sustains your charisma.");

   if (f2 & TR2_IM_ACID)    strcpy(info->str[curr_line++],"It provides immunity to acid.");
   if (f2 & TR2_IM_ELEC)    strcpy(info->str[curr_line++],"It provides immunity to electricity.");
   if (f2 & TR2_IM_FIRE)    strcpy(info->str[curr_line++],"It provides immunity to fire.");
   if (f2 & TR2_IM_COLD)    strcpy(info->str[curr_line++],"It provides immunity to cold.");

   if (f2 & TR2_FREE_ACT)   strcpy(info->str[curr_line++],"It provides immunity to paralysis.");
   if (f2 & TR2_HOLD_LIFE)  strcpy(info->str[curr_line++],"It provides resistance to life draining.");

   if (f2 & TR2_RES_ACID)   strcpy(info->str[curr_line++],"It provides resistance to acid.");
   if (f2 & TR2_RES_ELEC)   strcpy(info->str[curr_line++],"It provides resistance to electricity.");
   if (f2 & TR2_RES_FIRE)   strcpy(info->str[curr_line++],"It provides resistance to fire.");
   if (f2 & TR2_RES_COLD)   strcpy(info->str[curr_line++],"It provides resistance to cold.");
   if (f2 & TR2_RES_POIS)   strcpy(info->str[curr_line++],"It provides resistance to poison.");
   if (f2 & TR2_RES_FEAR)   strcpy(info->str[curr_line++],"It provides resistance to fear.");
   if (f2 & TR2_RES_LITE)   strcpy(info->str[curr_line++],"It provides resistance to light.");
   if (f2 & TR2_RES_DARK)   strcpy(info->str[curr_line++],"It provides resistance to dark.");
   if (f2 & TR2_RES_BLIND)  strcpy(info->str[curr_line++],"It provides resistance to blindness.");
   if (f2 & TR2_RES_CONF)   strcpy(info->str[curr_line++],"It provides resistance to confusion.");
   if (f2 & TR2_RES_SOUND)  strcpy(info->str[curr_line++],"It provides resistance to sound.");
   if (f2 & TR2_RES_SHARDS) strcpy(info->str[curr_line++],"It provides resistance to shards.");
   if (f2 & TR2_RES_NETHER) strcpy(info->str[curr_line++],"It provides resistance to nether.");
   if (f2 & TR2_RES_NEXUS)  strcpy(info->str[curr_line++],"It provides resistance to nexus.");
   if (f2 & TR2_RES_CHAOS)  strcpy(info->str[curr_line++],"It provides resistance to chaos.");
   if (f2 & TR2_RES_DISEN)  strcpy(info->str[curr_line++],"It provides resistance to disenchantment.");
   return (curr_line);
}

/*
 * this function describes all effects from flags3
 */
static s16b identify_fully_flags3(object_type *i_ptr, s16b curr_line,
                                  s16b max_len, obj_info_type *info)
{
   u64b f1,f2,f3;

   object_flags(i_ptr, &f1, &f2, &f3);

   if (f3 & TR3_FEATHER)     strcpy(info->str[curr_line++],"It induces feather falling.");
   if (f3 & TR3_SEE_INVIS)   strcpy(info->str[curr_line++],"It allows you to see invisible monsters.");
   if (f3 & TR3_TELEPATHY)   strcpy(info->str[curr_line++],"It gives telepathic powers.");
   if (f3 & TR3_SLOW_DIGEST) strcpy(info->str[curr_line++],"It slows your metabolism.");
   if (f3 & TR3_REGEN)       strcpy(info->str[curr_line++],"It speeds your regenerative powers.");

   if (f3 & TR3_DRAIN_EXP)   strcpy(info->str[curr_line++],"It drains experience.");
   if (f3 & TR3_TELEPORT)    strcpy(info->str[curr_line++],"It induces random teleportation.");
   if (f3 & TR3_AGGRAVATE)   strcpy(info->str[curr_line++],"It aggravates nearby creatures.");
   if (f3 & TR3_BLESSED)     strcpy(info->str[curr_line++],"It has been blessed by the gods.");

   if (cursed_p(i_ptr))
   {
      if (f3 & TR3_PERMA_CURSE) strcpy(info->str[curr_line++],"It is permanently cursed.");
      else if (f3 & TR3_HEAVY_CURSE)
      {
         if (i_ptr->name1==ART_MELKOR) strcpy(info->str[curr_line++],"It stays heavily cursed.");
         else strcpy(info->str[curr_line++],"It is heavily cursed.");
      }
      else
      {
         strcpy(info->str[curr_line++],"It is cursed.");
      }
   }
/* */
   if (must_2h(i_ptr))
      strcpy(info->str[curr_line++],"It must be wielded two-handed.");
   else if (could_2h(i_ptr))
      strcpy(info->str[curr_line++],"It can be wielded two-handed.");

   if (f3 & TR3_IGNORE_ACID) strcpy(info->str[curr_line++],"It cannot be harmed by acid.");
   if (f3 & TR3_IGNORE_ELEC) strcpy(info->str[curr_line++],"It cannot be harmed by electricity.");
   if (f3 & TR3_IGNORE_FIRE) strcpy(info->str[curr_line++],"It cannot be harmed by fire.");
   if (f3 & TR3_IGNORE_COLD) strcpy(info->str[curr_line++],"It cannot be harmed by cold.");
   return (curr_line);
}

/*
 * this function describes all effects from corpses
 */
static s16b identify_fully_corpse(object_type *i_ptr, s16b curr_line,
                                  s16b max_len, obj_info_type *info)
{

   cptr chance[10] = {"almost never", "very rarely",
                      "rarely", "once in a while",
                      "sometimes", "possibly",
                      "probably", "often",
                      "very often", "nearly always" };

   s16b r_idx     = i_ptr->sval;
   u32b flag_ga   = r_info[r_idx].corpse_gives_alw;
   u32b flag_gs   = r_info[r_idx].corpse_gives_smt;
   u32b flag_ta   = r_info[r_idx].corpse_takes_alw;
   u32b flag_ts   = r_info[r_idx].corpse_takes_smt;
   byte chance_g  = r_info[r_idx].corpse_chance_gives;
   byte chance_t  = r_info[r_idx].corpse_chance_takes;
   cptr cg = chance[chance_g/10];
   cptr ct = chance[chance_t/10];

   if (flag_ga & CORPSE_GIVES_ACID)
      strcpy(info->str[curr_line++],"It gives you an acid skin.");
   if (flag_gs & CORPSE_GIVES_ACID)
      strcpy(info->str[curr_line++],format("It %s gives you an acid skin.",cg));
   if (flag_ta & CORPSE_TAKES_ACID)
      strcpy(info->str[curr_line++],"It removes your acid skin.");
   if (flag_ts & CORPSE_TAKES_ACID)
      strcpy(info->str[curr_line++],format("It %s removes your acid skin.",ct));

   if (flag_ga & CORPSE_GIVES_FIRE)
      strcpy(info->str[curr_line++],"It gives you a fiery skin.");
   if (flag_gs & CORPSE_GIVES_FIRE)
      strcpy(info->str[curr_line++],format("It %s gives you a fiery skin.",cg));
   if (flag_ta & CORPSE_TAKES_FIRE)
      strcpy(info->str[curr_line++],"It removes your fiery skin.");
   if (flag_ts & CORPSE_TAKES_FIRE)
      strcpy(info->str[curr_line++],format("It %s removes your fiery skin.",ct));

   if (flag_ga & CORPSE_GIVES_COLD)
      strcpy(info->str[curr_line++],"It gives you a cold and brittle skin.");
   if (flag_gs & CORPSE_GIVES_COLD)
      strcpy(info->str[curr_line++],format("It %s gives you a cold and brittle skin.",cg));
   if (flag_ta & CORPSE_TAKES_COLD)
      strcpy(info->str[curr_line++],"It removes your cold and brittle skin.");
   if (flag_ts & CORPSE_TAKES_COLD)
      strcpy(info->str[curr_line++],format("It %s removes your cold and brittle skin.",ct));

   if (flag_ga & CORPSE_GIVES_ELEC)
      strcpy(info->str[curr_line++],"It gives you sparking fingers.");
   if (flag_gs & CORPSE_GIVES_ELEC)
      strcpy(info->str[curr_line++],format("It %s gives you sparking fingers.",cg));
   if (flag_ta & CORPSE_TAKES_ELEC)
      strcpy(info->str[curr_line++],"It stops your fingers sparking.");
   if (flag_ts & CORPSE_TAKES_ELEC)
      strcpy(info->str[curr_line++],format("It %s stops your fingers sparking.",ct));

   if (flag_ga & CORPSE_GIVES_STR)
      strcpy(info->str[curr_line++],"It increases your strength.");
   if (flag_gs & CORPSE_GIVES_STR)
      strcpy(info->str[curr_line++],format("It %s increases your strength.",cg));
   if (flag_ta & CORPSE_TAKES_STR)
      strcpy(info->str[curr_line++],"It lowers your strength.");
   if (flag_ts & CORPSE_TAKES_STR)
      strcpy(info->str[curr_line++],format("It %s lowers your strength.",ct));

   if (flag_ga & CORPSE_GIVES_DEX)
      strcpy(info->str[curr_line++],"It increases your dexterity.");
   if (flag_gs & CORPSE_GIVES_DEX)
      strcpy(info->str[curr_line++],format("It %s increases your dexterity.",cg));
   if (flag_ta & CORPSE_TAKES_DEX)
      strcpy(info->str[curr_line++],"It lowers your dexterity.");
   if (flag_ts & CORPSE_TAKES_DEX)
      strcpy(info->str[curr_line++],format("It %s lowers your dexterity.",ct));

   if (flag_ga & CORPSE_GIVES_INT)
      strcpy(info->str[curr_line++],"It increases your intelligence.");
   if (flag_gs & CORPSE_GIVES_INT)
      strcpy(info->str[curr_line++],format("It %s increases your intelligence.",cg));
   if (flag_ta & CORPSE_TAKES_INT)
      strcpy(info->str[curr_line++],"It lowers your intelligence.");
   if (flag_ts & CORPSE_TAKES_INT)
      strcpy(info->str[curr_line++],format("It %s lowers your intelligence.",ct));

   if (flag_ga & CORPSE_GIVES_WIS)
      strcpy(info->str[curr_line++],"It increases your wisdom.");
   if (flag_gs & CORPSE_GIVES_WIS)
      strcpy(info->str[curr_line++],format("It %s increases your wisdom.",cg));
   if (flag_ta & CORPSE_TAKES_WIS)
      strcpy(info->str[curr_line++],"It lowers your wisdom.");
   if (flag_ts & CORPSE_TAKES_WIS)
      strcpy(info->str[curr_line++],format("It %s lowers your wisdom.",ct));

   if (flag_ga & CORPSE_GIVES_CON)
      strcpy(info->str[curr_line++],"It increases your constitution.");
   if (flag_gs & CORPSE_GIVES_CON)
      strcpy(info->str[curr_line++],format("It %s increases your constitution.",cg));
   if (flag_ta & CORPSE_TAKES_CON)
      strcpy(info->str[curr_line++],"It lowers your constitution.");
   if (flag_ts & CORPSE_TAKES_CON)
      strcpy(info->str[curr_line++],format("It %s lowers your constitution.",ct));

   if (flag_ga & CORPSE_GIVES_CHR)
      strcpy(info->str[curr_line++],"It increases your charisma.");
   if (flag_gs & CORPSE_GIVES_CHR)
      strcpy(info->str[curr_line++],format("It %s increases your charisma.",cg));
   if (flag_ta & CORPSE_TAKES_CHR)
      strcpy(info->str[curr_line++],"It lowers your charisma.");
   if (flag_ts & CORPSE_TAKES_CHR)
      strcpy(info->str[curr_line++],format("It %s lowers your charisma.",ct));

   if (flag_ga & CORPSE_GIVES_BLINDNESS)
      strcpy(info->str[curr_line++],"It removes sight.");
   if (flag_gs & CORPSE_GIVES_BLINDNESS)
      strcpy(info->str[curr_line++],format("It %s removes sight.",cg));
   if (flag_ta & CORPSE_TAKES_BLINDNESS)
      strcpy(info->str[curr_line++],"It restores sight.");
   if (flag_ts & CORPSE_TAKES_BLINDNESS)
      strcpy(info->str[curr_line++],format("It %s removes sight.",ct));

   if (flag_ga & CORPSE_GIVES_CONFUSION)
      strcpy(info->str[curr_line++],"It removes confusion.");
   if (flag_gs & CORPSE_GIVES_CONFUSION)
      strcpy(info->str[curr_line++],format("It %s removes confusion.",cg));
   if (flag_ta & CORPSE_TAKES_CONFUSION)
      strcpy(info->str[curr_line++],"It restores confusion.");
   if (flag_ts & CORPSE_TAKES_CONFUSION)
      strcpy(info->str[curr_line++],format("It %s removes confusion.",ct));

   if (flag_ga & CORPSE_GIVES_VOMIT)
      strcpy(info->str[curr_line++],"It seriously unsettles your stomach.");
   if (flag_gs & CORPSE_GIVES_VOMIT)
      strcpy(info->str[curr_line++],format("It %s seriously unsettles your stomach.",cg));

   if (flag_ga & CORPSE_GIVES_TELEPORT)
      strcpy(info->str[curr_line++],"It makes you teleport.");
   if (flag_gs & CORPSE_GIVES_TELEPORT)
      strcpy(info->str[curr_line++],format("It %s makes you teleport.",cg));

   if (flag_ga & CORPSE_GIVES_PEARL)
      strcpy(info->str[curr_line++],"It contains a pearl.");
   if (flag_gs & CORPSE_GIVES_PEARL)
      strcpy(info->str[curr_line++],format("It %s contains a pearl.",cg));

   if (flag_ga & CORPSE_GIVES_POISON)
      strcpy(info->str[curr_line++],"It poisons you, worse if it's older.");
   if (flag_gs & CORPSE_GIVES_POISON)
      strcpy(info->str[curr_line++],format("It %s poisons you, worse if it's older.",cg));
   if (flag_ta & CORPSE_TAKES_POISON)
      strcpy(info->str[curr_line++],"It removes poison from you, better if it's fresh.");
   if (flag_ts & CORPSE_TAKES_POISON)
      strcpy(info->str[curr_line++],format("It %s removes poison from you, better if it's fresh.",ct));

   if (flag_ga & CORPSE_GIVES_BADPOISON)
      strcpy(info->str[curr_line++],"It lethally poisons you, worse if it's older.");
   if (flag_gs & CORPSE_GIVES_BADPOISON)
      strcpy(info->str[curr_line++],format("It %s poisons lethally you, worse if it's older.",cg));
   if (flag_ta & CORPSE_TAKES_BADPOISON)
      strcpy(info->str[curr_line++],"It removes poison from you quickly, better if it's fresh.");
   if (flag_ts & CORPSE_TAKES_BADPOISON)
      strcpy(info->str[curr_line++],format("It %s removes poison from you quickly, better if it's fresh.",ct));

   if (flag_ga & CORPSE_GIVES_PARALYZE)
      strcpy(info->str[curr_line++],"It paralyzes you.");
   if (flag_gs & CORPSE_GIVES_PARALYZE)
      strcpy(info->str[curr_line++],format("It %s paralyzes you.",cg));

   if (flag_ga & CORPSE_GIVES_MANA)
      strcpy(info->str[curr_line++],"It restores your mana.");
   if (flag_gs & CORPSE_GIVES_MANA)
      strcpy(info->str[curr_line++],format("It %s restores your mana.", cg));
   if (flag_ta & CORPSE_TAKES_MANA)
      strcpy(info->str[curr_line++],"It drains your mana.");
   if (flag_ts & CORPSE_TAKES_MANA)
      strcpy(info->str[curr_line++],format("It %s drains your mana.", cg));

   if (flag_ga & CORPSE_GIVES_SCREAM)
      strcpy(info->str[curr_line++],"It makes you scream in anguish.");
   if (flag_gs & CORPSE_GIVES_SCREAM)
      strcpy(info->str[curr_line++],format("It %s makes you scream in anguish.", cg));

   if (flag_ga & CORPSE_GIVES_SCREAM)
      strcpy(info->str[curr_line++],"It makes you scream on top of you lungs.");
   if (flag_gs & CORPSE_GIVES_SCREAM)
      strcpy(info->str[curr_line++],format("It %s makes you scream on top of your lungs.", cg));

   if (flag_ga & CORPSE_GIVES_THROAT)
      strcpy(info->str[curr_line++],"It makes your throat swell shut.");
   if (flag_gs & CORPSE_GIVES_THROAT)
      strcpy(info->str[curr_line++],format("It %s makes your throat swell shut.", cg));

   if (flag_ga & CORPSE_GIVES_BADTHROAT)
      strcpy(info->str[curr_line++],"It makes your throat swell shut a long time.");
   if (flag_gs & CORPSE_GIVES_BADTHROAT)
      strcpy(info->str[curr_line++],format("It %s makes your throat swell shut a long time.", cg));

   if (flag_ga & CORPSE_GIVES_NEXUS)
      strcpy(info->str[curr_line++],"It contains nexus essence.");
   if (flag_gs & CORPSE_GIVES_NEXUS)
      strcpy(info->str[curr_line++],format("It %s contains nexus essence.", cg));

   if (flag_ga & CORPSE_GIVES_BLINK)
      strcpy(info->str[curr_line++],"It makes you blink.");
   if (flag_gs & CORPSE_GIVES_BLINK)
      strcpy(info->str[curr_line++],format("It %s makes you blink.", cg));

   if (flag_ga & CORPSE_GIVES_SPEED)
      strcpy(info->str[curr_line++],"It makes you fast.");
   if (flag_gs & CORPSE_GIVES_SPEED)
      strcpy(info->str[curr_line++],format("It %s makes you fast.", cg));
   if (flag_ta & CORPSE_TAKES_SPEED)
      strcpy(info->str[curr_line++],"It makes you slow.");
   if (flag_ts & CORPSE_TAKES_SPEED)
      strcpy(info->str[curr_line++],format("It %s makes you slow.", ct));

   if (flag_ga & CORPSE_GIVES_SLEEP)
      strcpy(info->str[curr_line++],"It makes you sleepy.");
   if (flag_gs & CORPSE_GIVES_SLEEP)
      strcpy(info->str[curr_line++],format("It %s makes you sleepy.", cg));

   if (flag_ga & CORPSE_GIVES_BERSERKER)
      strcpy(info->str[curr_line++],"It makes you berserk.");
   if (flag_gs & CORPSE_GIVES_BERSERKER)
      strcpy(info->str[curr_line++],format("It %s makes you berserk.", cg));

   if (flag_ga & CORPSE_GIVES_INFLATE)
      strcpy(info->str[curr_line++],"It makes a very great meal.");
   if (flag_gs & CORPSE_GIVES_INFLATE)
      strcpy(info->str[curr_line++],format("It %s makes a very great meal.", cg));


   return (curr_line);
}

/*
 * Describe a "fully identified" item
 * set only_log to view only where an item came from
 */
bool identify_fully_aux(object_type *i_ptr, bool only_log)
{
   s16b                i = 0, j, k, log_lines, after_origin = 0;
   u64b                f1, f2, f3;
   obj_info_type       info;
   char                temp[1024];

   /* Extract the flags */
   object_flags(i_ptr, &f1, &f2, &f3);

   log_lines = 0;

   object_desc(temp, i_ptr, TRUE, 3);
   log_lines = add_obj_info_line(&info, log_lines, 63, temp);
   strcpy(info.str[log_lines++],"--------------------------------------");

   log_lines = identify_fully_origin(i_ptr, log_lines, 63, &info);

   after_origin = log_lines;

   if (!only_log)
   {
      object_kind *k_ptr = &k_info[i_ptr->k_idx];

      if (k_ptr->text)
      {
         log_lines = add_obj_info_line(&info, log_lines, 63, k_text + k_ptr->text);
      }

      if (artifact_p(i_ptr) && (a_info[i_ptr->name1].text))
      {
         strcpy(info.str[log_lines++],"  ");

         sprintf(temp,"%s", a_text + a_info[i_ptr->name1].text);
         log_lines = add_obj_info_line(&info, log_lines, 63, temp);
         strcpy(info.str[log_lines++],"  ");
      }

      if (ego_item_p(i_ptr) && (e_info[i_ptr->name2].text))
      {
         strcpy(info.str[log_lines++],"  ");

         sprintf(temp,"%s", e_text + e_info[i_ptr->name2].text);
         log_lines = add_obj_info_line(&info, log_lines, 63, temp);
         strcpy(info.str[log_lines++],"  ");
      }
      if ((wield_slot(i_ptr) != INVEN_BOW) && (wield_slot(i_ptr) != INVEN_AMMO))
      {
         if (i_ptr->to_h < 0)
         {
             strcpy(temp, "It decreases your chance to hit in melee.");
             log_lines = add_obj_info_line(&info, log_lines, 63, temp);
         }
         if (i_ptr->to_h > 0)
         {
             strcpy(temp, "It increases your chance to hit in melee.");
             log_lines = add_obj_info_line(&info, log_lines, 63, temp);
         }
         if (i_ptr->to_d < 0)
         {
             strcpy(temp, "It decreases your effectiveness in melee.");
             log_lines = add_obj_info_line(&info, log_lines, 63, temp);
         }
         if (i_ptr->to_d > 0)
         {
             strcpy(temp, "It increases your effectiveness in melee.");
             log_lines = add_obj_info_line(&info, log_lines, 63, temp);
         }
      }
      else
      {
         if (i_ptr->to_h < 0)
         {
             strcpy(temp, "It decreases your chance to hit from a distance.");
             log_lines = add_obj_info_line(&info, log_lines, 63, temp);
         }
         if (i_ptr->to_h > 0)
         {
             strcpy(temp, "It increases your chance to hit from a distance.");
             log_lines = add_obj_info_line(&info, log_lines, 63, temp);
         }
         if (i_ptr->to_d < 0)
         {
             strcpy(temp, "It decreases your effectiveness from a distance.");
             log_lines = add_obj_info_line(&info, log_lines, 63, temp);
         }
         if (i_ptr->to_d > 0)
         {
             strcpy(temp, "It increases your effectiveness from a distance.");
             log_lines = add_obj_info_line(&info, log_lines, 63, temp);
         }
      }
      if (i_ptr->to_a < 0)
      {
          strcpy(temp, "It makes you careless in melee.");
          log_lines = add_obj_info_line(&info, log_lines, 63, temp);
      }
      if (i_ptr->to_a > 0)
      {
          strcpy(temp, "It protects you from blows in melee.");
          log_lines = add_obj_info_line(&info, log_lines, 63, temp);
      }

      /* Require activation ability, or being an artifact rod */
      if ( (f3 & TR3_ACTIVATE) ||
           ((i_ptr->tval == TV_ROD) && (i_ptr->name1!=0)) )
      {
          char temp2[1024];
          if (i_ptr->tval != TV_ROD)
          {
             strcpy(temp, "It can be activated for ");
             strcpy(temp2,item_activation(i_ptr));
             strcat(temp2," if it is being worn.");
             strcat(temp, temp2);
             log_lines = add_obj_info_line(&info, log_lines, 63, temp);
          }
          else
          {
             strcpy(temp, "It can be zapped to ");
             strcpy(temp2, item_activation(i_ptr));
             strcat(temp, temp2);
             strcat(temp2, ".");
             log_lines = add_obj_info_line(&info, log_lines, 63, temp);
          }
      }

      if (i_ptr->tval == TV_LITE)
      {
          if (artifact_p(i_ptr))
          {
              strcpy(info.str[log_lines++],"It provides light (radius 3) forever.");
          }
          else if (i_ptr->sval == SV_LITE_LANTERN)
          {
              strcpy(info.str[log_lines++],"It provides light (radius 2) when fueled.");
          }
          else if (i_ptr->sval == SV_LITE_NOLDOR)
          {
              strcpy(info.str[log_lines++],"It provides light (radius 4) when fueled.");
          }
          else
          {
              strcpy(info.str[log_lines++],"It provides light (radius 1) when fueled.");
          }
      }

      if ((i_ptr->tval==TV_HAFTED) && (i_ptr->sval==SV_QUARTERSTAFF))
      {
         if ((i_ptr->name2==EGO_MAGE) ||
             (i_ptr->name2==EGO_ADEPT) ||
             (i_ptr->name2==EGO_ARCHMAGE))
         {
            log_lines = identify_fully_mage_staff(i_ptr, log_lines, 63, &info);
         }
      }

      if (i_ptr->tval == TV_CHEST)
      {
         trap_type *t_ptr;

         if (!i_ptr->xtra2)
           strcpy(info.str[log_lines++],"The chest is not trapped.");
         else
         {
            trap_item_type *tr_ptr = &t_list[i_ptr->xtra2];
            for (i=0;i<MAX_TRAPS_IN_SET;i++)
            {
               if (tr_ptr->type[i])
               {
                  t_ptr=&t_info[tr_ptr->type[i]];
                  strcpy(info.str[log_lines++],t_name+t_ptr->text);
               }
            }
         } /* traps */
      } /* chest */
      if (i_ptr->tval == TV_SPELL)
      {
         log_lines = identify_fully_spell(i_ptr, log_lines, 63, &info);
      }
      if (i_ptr->tval == TV_BOOK)
      {
         s16b index[MAX_SPELLS_PER_ITEM], s, count = 0;
         switch (i_ptr->sval)
         {
            case SV_BOOK_SMALL:  strcpy(info.str[log_lines++], "It can hold 4 spells.");
                                 break;
            case SV_BOOK_AVG:    strcpy(info.str[log_lines++], "It can hold 8 spells.");
                                 break;
            case SV_BOOK_LARGE:  strcpy(info.str[log_lines++], "It can hold 12 spells.");
                                 break;
         }
         for (s=0; s < (s16b)s_number; s++)
         {
            if (has_spell(i_ptr, s)) index[count++]=s;
         } 
         if (count == 0)
         {
            strcpy(info.str[log_lines++], "It holds no spells.");
         }
         else
         {
            spell_type *s_ptr;
            bool        cheat_any = FALSE;
            cptr        scalestr[6] = {"small","avg  ","large","super"};
            cptr        categ[MAX_SPELL_TYPES] = { "Nature Forces",
                                                    "Dark Forces",
                                                    "Escape",
                                                    "Heal",
                                                    "Sense",
                                                    "Change Other",
                                                    "Change Item",
                                                    "Change Self",
                                                    "Change World" };
            sprintf(temp, "It holds %d spells:", count); 
            strcpy(info.str[log_lines++], temp);
            for (s=0; s < count; s++)
            {
               s_ptr = &s_info[index[i]];
               cheat_any |= (s_ptr->numcast >= (10 * s_ptr->level));
            }
            if (cheat_any)
            {
               strcpy(info.str[log_lines++], "Knowledge  Spell                  Lev Mana Fail Scale Category        Extra");
            }
            else
            {
               strcpy(info.str[log_lines++], "Knowledge  Spell                  Lev Mana Fail");
            }
            for (s=0; s < count; s++)
            {
               char extra_info[80];

               s_ptr = &s_info[index[s]];
               if (s_ptr->numcast == 0)
               {
                  sprintf(temp, "untried    %-23s%3d  %2d",
                  s_name + s_ptr->name, s_ptr->level, s_ptr->mana);
               }
               else if (s_ptr->numcast >= (10 * s_ptr->level))
               {
                  extra_new_spell_info(extra_info, index[s]);
                  sprintf(temp, "well known %-23s%3d  %2d  %2d%%  %5s %-14s %s",
                  s_name + s_ptr->name, s_ptr->level, s_ptr->mana,
                  page_chance(index[s]), scalestr[(s16b)s_ptr->scale],
                  categ[s_ptr->type], extra_info);
               }
               else if (s_ptr->numcast > 0)
               {
                  sprintf(temp, "known      %-23s%3d  %2d  %2d%%",
                  s_name + s_ptr->name, s_ptr->level, s_ptr->mana,
                  page_chance(index[s]));
               }
               strcpy(info.str[log_lines++], temp);
            }
         }
      }
      log_lines = identify_fully_flags1(i_ptr, log_lines, 63, &info);
      log_lines = identify_fully_flags2(i_ptr, log_lines, 63, &info);
      log_lines = identify_fully_flags3(i_ptr, log_lines, 63, &info);

      if (i_ptr->tval == TV_CORPSE)
      {
         log_lines = identify_fully_corpse(i_ptr, log_lines, 63, &info);
      }
   } /* !nolog */

   msg_print(NULL);

   /* Save the screen */
   Term_save();

   /* Erase the screen */
   for (k = 1; k < 24; k++) prt("", 13, k);

   /* Label the information */
   prt("     Item Attributes:", 15, 1);

   /* We will print on top of the map (column 13) */
   for (k = 2, j = 0; j < log_lines; j++)
   {
      /* Show the info */
      prt(info.str[j], 15, k++);

      /* Every 20 entries (lines 2 to 21), start over */
      if ((k == 20) && (j+1 < log_lines))
      {
         char c;

         prt("-- more --", 15, k);
         c=inkey();
         if (c==ESCAPE) break;
         for ( ; k > 2; k--) prt("", 15, k);
      }
   }
   if (log_lines == after_origin)
   {
      if (only_log)
      {
         prt("You have not yet studied this item.", 15, 2+log_lines);
         k +=(2+log_lines);
      }
      else
      {
         prt("This item has no special powers.", 15, 2+log_lines);
         k +=(2+log_lines);
      }
   }

   /* Wait for it */
   prt("[Press any key to continue]", 15, k);
   inkey();

   /* Restore the screen */
   Term_load();

   /* Gave knowledge */
   return (TRUE);
}



/*
 * Convert an inventory index into a one character label
 * Note that the label does NOT distinguish inven/equip.
 */
s16b index_to_label(s16b i)
{
   /* Indexes for "inven" are easy */
   if (i < INVEN_WIELD) return (I2A(i));

   /* Indexes for "equip" are offset */
   return (I2A(i - INVEN_WIELD));
}

/*
 * Convert a label into the index of an item in the "inven"
 * Return "-1" if the label does not indicate a real item
 */
s16b label_to_inven(s16b c)
{
   s16b i;

   /* Convert */
   i = (ang_islower(c) ? A2I(c) : -1);

   /* Verify the index */
   if ((i < 0) || (i > INVEN_PACK)) return (-1);

   /* Empty slots can never be chosen */
   if (!inventory[i].k_idx) return (-1);

   /* Return the index */
   return (i);
}

/* jk
 * Convert a label into the index of an item on the "floor"
 * Return "-1" if the label does not indicate a real item
 * return floor_index+MAX_INVEN (so as to be comparable with the other
 * label_to??? functions
 */
s16b label_to_floor(s16b c)
{
   s16b i;

   /* Convert */
   i = (ang_islower(c) ? A2I(c) : -1);

   /* Verify the index */
   if ((i < 0) || (i > MAX_FLOOR_ITEMS)) return (-1);

   /* Empty slots can never be chosen */
   if (!floor_item(i)) return (-1);

   /* Return the index */
   return (i+INVEN_TOTAL);
}

/*
 * Convert a label into the index of a item in the "equip"
 * Return "-1" if the label does not indicate a real item
 */
s16b label_to_equip(s16b c)
{
   s16b i;

   /* Convert */
   i = (ang_islower(c) ? A2I(c) : -1) + INVEN_WIELD;

   /* Verify the index */
   if ((i < INVEN_WIELD) || (i >= INVEN_TOTAL)) return (-1);

   /* Empty slots can never be chosen */
   if (!inventory[i].k_idx) return (-1);

   /* Return the index */
   return (i);
}

/*
 * Determine which equipment slot (if any) an item likes
 */
s16b wield_slot(object_type *i_ptr)
{
   /* Slot for equipment */
   switch (i_ptr->tval)
   {
      case TV_DIGGING:
      case TV_HAFTED:
      case TV_POLEARM:
      case TV_SWORD:
         return (INVEN_WIELD);

      case TV_BOW:
         return (INVEN_BOW);

      case TV_ARROW:
      case TV_BOLT:
      case TV_SHOT:
         return (INVEN_AMMO);

      case TV_RING:

         if (p_ptr->pclass == CLASS_HIGHPRST)
         {
            if (!inventory[INVEN_WIELD].k_idx) return (INVEN_WIELD);
            if (!inventory[INVEN_BOW].k_idx) return (INVEN_BOW);
         }

         /* Use the right hand first */
         if (!inventory[INVEN_LEFT].k_idx) return (INVEN_LEFT);

         /* Use the left hand for swapping (by default) */
         return (INVEN_RIGHT);

      case TV_AMULET:
         return (INVEN_NECK);

      case TV_LITE:
         return (INVEN_LITE);

      case TV_DRAG_ARMOR:
      case TV_HARD_ARMOR:
      case TV_SOFT_ARMOR:
         return (INVEN_BODY);

      case TV_CLOAK:
         return (INVEN_OUTER);

      case TV_SHIELD:
         return (INVEN_ARM);

      case TV_CROWN:
      case TV_HELM:
         return (INVEN_HEAD);

      case TV_GLOVES:
         return (INVEN_HANDS);

      case TV_BOOTS:
         return (INVEN_FEET);
   }

   /* No slot available */
   return (-1);
}


/*
 * Return a string mentioning how a given item is carried
 */
cptr mention_use(s16b i)
{
   cptr p;

   /* Examine the location */
   switch (i)
   {
      case INVEN_WIELD:
/* jk */
         if (p_ptr->twohands)
         {
            p = "Wield 2H";
         }
         else
         {
            if ( (p_ptr->pclass == CLASS_GLADIATR) &&
                 (inventory[INVEN_ARM].k_idx))
            {
               p="1st weap";
            }
            else
            {
               p="Wielding";
            }
         }
         if (p_ptr->pclass == CLASS_HIGHPRST)
         {
            p = "Left I";
         }
         break;
      case INVEN_BOW:
      {
         p = "Firing";
         if (p_ptr->pclass == CLASS_HIGHPRST)
         {
            p = "Left II";
         }
         break;
      }
      case INVEN_LEFT:
      {
         p = "Left";
         if (p_ptr->pclass == CLASS_HIGHPRST)
         {
            p = "Right I";
         }
         break;
      }
      case INVEN_RIGHT:
      {
         p = "Right";
         if (p_ptr->pclass == CLASS_HIGHPRST)
         {
            p = "Right II";
         }
         break;
      }
      case INVEN_NECK:  p = "Neck"; break;
      case INVEN_LITE:  p = "Light"; break;
      case INVEN_BODY:  p = "Body"; break;
      case INVEN_OUTER: p = "Cloak"; break;
      case INVEN_ARM:
      {
         if (p_ptr->pclass == CLASS_GLADIATR)
         {
            p = "2nd weap";
         }
         else
         {
            p = "On Arm";
         }
         break;
      }
      case INVEN_HEAD:  p = "On head"; break;
      case INVEN_HANDS: p = "On hands"; break;
      case INVEN_FEET:  p = "On feet"; break;
      case INVEN_AMMO:  p = "Ammo"; break;
      default:          p = "In pack"; break;
   }

   if (i>=INVEN_TOTAL) p = "On ground";

   /* Hack -- Heavy weapon */
   if (i == INVEN_WIELD)
   {
      object_type *i_ptr;
      i_ptr = &inventory[i];
      if (adj_str_hold[p_ptr->stat_ind[A_STR]] < i_ptr->weight / 10)
      {
         if (p_ptr->twohands)
         {
            p = "Lift 2H";
         }
         else
         {
            p="Lifting";
         }
      }
   }

   /* Hack -- Heavy bow */
   if (i == INVEN_BOW)
   {
      object_type *i_ptr;
      i_ptr = &inventory[i];
      if (adj_str_hold[p_ptr->stat_ind[A_STR]] < i_ptr->weight / 10)
      {
         p = "Lifting";
      }
   }

   /* Return the result */
   return (p);
}


/*
 * Return a string describing how a given item is being worn.
 * Currently, only used for items in the equipment, not inventory.
 */
cptr describe_use(s16b i)
{
   cptr p;

   switch (i)
   {
      case INVEN_WIELD:
/* jk */
         if (p_ptr->twohands)
         {
            p = "attacking monsters with using both hands";
         }
         else
         {
            if ( (p_ptr->pclass == CLASS_GLADIATR) &&
                 (inventory[INVEN_ARM].k_idx))
            {
               p="attacking monsters with primary weapon";
            }
            else if (p_ptr->pclass == CLASS_HIGHPRST)
            {
               p="wearing on your left hand";
            }
            else
            {
               p="attacking monsters with";
            }
         }
         break;
      case INVEN_BOW:
         if (p_ptr->pclass == CLASS_HIGHPRST)
         {
            p="wearing on your left hand";
         }
         else
         {
            p = "shooting missiles with";
         }
         break;
      case INVEN_LEFT:
         if (p_ptr->pclass == CLASS_HIGHPRST)
         {
            p="wearing on your right hand";
         }
         else
         {
            p="wearing on your left hand";
         }
         break;
      case INVEN_RIGHT: p = "wearing on your right hand"; break;
      case INVEN_NECK:  p = "wearing around your neck"; break;
      case INVEN_LITE:  p = "using to light the way"; break;
      case INVEN_BODY:  p = "wearing on your body"; break;
      case INVEN_OUTER: p = "wearing on your back"; break;
      case INVEN_AMMO:  p = "shooting with"; break;
      case INVEN_ARM:
         if (p_ptr->pclass == CLASS_GLADIATR)
         {
            p = "attacking with secondary weapon";
         }
         else
         {
            p = "wearing on your arm";
         }
         break;
      case INVEN_HEAD:  p = "wearing on your head"; break;
      case INVEN_HANDS: p = "wearing on your hands"; break;
      case INVEN_FEET:  p = "wearing on your feet"; break;
      default:          p = "carrying in your pack"; break;
   }

   /* Hack -- Heavy weapon */
   if (i == INVEN_WIELD)
   {
      object_type *i_ptr;
      i_ptr = &inventory[i];
      if (adj_str_hold[p_ptr->stat_ind[A_STR]] < i_ptr->weight / 10)
      {
/* jk */
         if (p_ptr->twohands) p = "just lifting with both hands"; else p="just lifting";
      }
   }

   /* Hack -- Heavy bow */
   if (i == INVEN_BOW)
   {
      object_type *i_ptr;
      i_ptr = &inventory[i];
      if (adj_str_hold[p_ptr->stat_ind[A_STR]] < i_ptr->weight / 10)
      {
         p = "just lifting";
      }
   }

   /* Return the result */
   return p;
}

/*
 * Check an item against the item tester info
 */
bool item_tester_okay(object_type *i_ptr)
{
   /* Hack -- allow listing empty slots */
   if (item_tester_full) return (TRUE);

   /* Require an item */
   if (!i_ptr->k_idx) return (FALSE);

   /* Hack -- ignore "gold" */
/* jk - removed this */
/*   if (i_ptr->tval == TV_GOLD) return (FALSE); */

   /* Check the tval */
   if (item_tester_tval)
   {
      if (!(item_tester_tval == i_ptr->tval)) return (FALSE);
   }

   /* Check the hook */
   if (item_tester_hook)
   {
      if (!(*item_tester_hook)(i_ptr)) return (FALSE);
   }

   /* Assume okay */
   return (TRUE);
}

/*
 * Choice window "shadow" of the "show_inven()" function
 */
void display_inven(void)
{
   register s16b i, n, z = 0;
   object_type *i_ptr;
   byte     attr = TERM_WHITE;
   char     tmp_val[80];
   char     i_name[80];

   /* Find the "final" slot */
   for (i = 0; i < INVEN_PACK; i++)
   {
      i_ptr = &inventory[i];

      /* Track non-empty slots */
      if (i_ptr->k_idx) z = i + 1;
   }

   /* Display the pack */
   for (i = 0; i < z; i++)
   {
      /* Examine the item */
      i_ptr = &inventory[i];

      /* Start with an empty "index" */
      tmp_val[0] = tmp_val[1] = tmp_val[2] = ' ';

      /* Is this item "acceptable"? */
      if (item_tester_okay(i_ptr))
      {
         /* Prepare an "index" */
         tmp_val[0] = index_to_label(i);

         /* Bracket the "index" --(-- */
         tmp_val[1] = ')';
      }

      /* Display the index (or blank space) */
      Term_putstr(0, i, 3, TERM_WHITE, tmp_val);

      /* Obtain an item description */
      object_desc(i_name, i_ptr, TRUE, 3);

      /* Obtain the length of the description */
      n = strlen(i_name);

      /* Get a color */
      if (use_color) attr = tval_to_attr[i_ptr->tval % 128];

      /* Display the entry itself */
      Term_putstr(3, i, n, attr, i_name);

      /* Erase the rest of the line */
      Term_erase(3+n, i, 80);

      /* Display the weight if needed */
      if (show_choose_weight && i_ptr->weight)
      {
         s16b wgt = i_ptr->weight * i_ptr->number;
         sprintf(tmp_val, "%3d.%1d lb", wgt / 10, wgt % 10);
         Term_putstr(71, i, -1, TERM_WHITE, tmp_val);
      }
   }

   /* Erase the rest of the window */
   clear_from(z);
}

/* jk - display floor now is important as well */

void display_floor(s16b x,s16b y)
{
   register s16b i, n, z = 0;
   object_type *i_ptr;
   byte     attr = TERM_WHITE;
   char     tmp_val[80];
   char     i_name[80];

   /* Display the floor */
   z = objects_on_floor(x,y);
   for (i = 0; i < z; i++)
   {
      /* Examine the item */
      i_ptr = get_item_pointer_floor_xy(i,x,y);

      /* Start with an empty "index" */
      tmp_val[0] = tmp_val[1] = tmp_val[2] = ' ';

      /* Is this item "acceptable"? */
      if (item_tester_okay(i_ptr))
      {
         /* Prepare an "index" */
         tmp_val[0] = index_to_label(i);

         /* Bracket the "index" --(-- */
         tmp_val[1] = ')';
      }

      /* Display the index (or blank space) */
      Term_putstr(0, i, 3, TERM_WHITE, tmp_val);

      /* Obtain an item description */
      object_desc(i_name, i_ptr, TRUE, 3);

      /* Obtain the length of the description */
      n = strlen(i_name);

      /* Get a color */
      if (use_color) attr = tval_to_attr[i_ptr->tval % 128];

      /* Display the entry itself */
      Term_putstr(3, i, n, attr, i_name);

      /* Erase the rest of the line */
      Term_erase(3+n, i, 80);

      /* Display the weight if needed */
      if (show_choose_weight && i_ptr->weight)
      {
         s16b wgt = i_ptr->weight * i_ptr->number;
         sprintf(tmp_val, "%3d.%1d lb", wgt / 10, wgt % 10);
         Term_putstr(71, i, -1, TERM_WHITE, tmp_val);
      }
   }

   /* Erase the rest of the window */
   clear_from(z);
}

/*
 * Choice window "shadow" of the "show_equip()" function
 */
void display_equip(void)
{
   register    s16b i, n;
   object_type *i_ptr;
   byte        attr = TERM_WHITE;

   char        tmp_val[80];

   char        i_name[80];

   /* Display the equipment */
   for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
   {
      if ((i == INVEN_BODY) && (p_ptr->pclass == CLASS_GLADIATR)) continue;
      if ((i == INVEN_AMMO) && (p_ptr->pclass == CLASS_HIGHPRST)) continue;
      if ((i == INVEN_ARM) && (p_ptr->twohands == TRUE)) continue;

      /* Examine the item */
      i_ptr = &inventory[i];

      /* Start with an empty "index" */
      tmp_val[0] = tmp_val[1] = tmp_val[2] = ' ';

      /* Is this item "acceptable"? */
      if (item_tester_okay(i_ptr))
      {
         /* Prepare an "index" */
         tmp_val[0] = index_to_label(i);

         /* Bracket the "index" --(-- */
         tmp_val[1] = ')';
      }

      /* Display the index (or blank space) */
      Term_putstr(0, i - INVEN_WIELD, 3, TERM_WHITE, tmp_val);

      /* Obtain an item description */
      object_desc(i_name, i_ptr, TRUE, 3);

      if ((i==INVEN_ARM) && p_ptr->twohands) continue;

      /* Obtain the length of the description */
      n = strlen(i_name);

      /* Get the color */
      if (use_color) attr = tval_to_attr[i_ptr->tval % 128];

      /* Display the entry itself */
      Term_putstr(3, i - INVEN_WIELD, n, attr, i_name);

      /* Erase the rest of the line */
      Term_erase(3+n, i - INVEN_WIELD, 80);

      /* Display the slot description (if needed) */
      if (show_choose_label)
      {
         Term_putstr(61, i - INVEN_WIELD, -1, TERM_WHITE, "<--");
         Term_putstr(65, i - INVEN_WIELD, -1, TERM_WHITE, mention_use(i));
      }

      /* Display the weight (if needed) */
      if (show_choose_weight && i_ptr->weight)
      {
         s16b wgt = i_ptr->weight * i_ptr->number;
         s16b col = (show_choose_label ? 52 : 71);
         sprintf(tmp_val, "%3d.%1d lb", wgt / 10, wgt % 10);
         Term_putstr(col, i - INVEN_WIELD, -1, TERM_WHITE, tmp_val);
      }
   }

   /* Erase the rest of the window */
   clear_from(INVEN_TOTAL - INVEN_WIELD);
}

/*
 * Display the inventory.
 *
 * Hack -- do not display "trailing" empty slots
 */
void show_inven(void)
{
   int i, j, k, l, z = 0;
   int col, len, lim;

   object_type *i_ptr;

   char tmp_val[80];

   char i_name[80];

   int out_index[23];
   byte out_color[23];
   char out_desc[23][80];

   /* Default length */
   len = 79 - 50;

   /* Maximum space allowed for descriptions */
   lim = 79 - 3;

   /* Require space for weight (if needed) */
   if (show_inven_weight) lim -= 9;

   /* Find the "final" slot */
   for (i = 0; i < INVEN_PACK; i++)
   {
      i_ptr = &inventory[i];

      /* Skip non-objects */
      if (!i_ptr->k_idx) continue;

      /* Track */
      z = i + 1;
   }

   /* Scan the inventory list */
   for (k = 0, i = 0; i < z; i++)
   {
      i_ptr = &inventory[i];

      /* Is this item acceptable? */
      if (!item_tester_okay(i_ptr)) continue;

      /* Description */
      object_desc(i_name, i_ptr, TRUE, 3);

      /* Truncate the description */
      i_name[lim] = 0;

      /* Save the index */
      out_index[k] = i;

      /* Acquire inventory color */
      out_color[k] = tval_to_attr[i_ptr->tval & 0x7F];

      /* Save the description */
      strcpy(out_desc[k], i_name);

      /* Extract the maximal length (see below) */
      l = strlen(out_desc[k]) + (2 + 3);

      /* Increase length for weight (if needed) */
      if (show_inven_weight) l += 9;

      /* Maintain the max-length */
      if (l > len) len = l;

      /* Advance the entry */
      k++;
   }

   /* Hack -- Find a column to start in */
   col = (len > 76) ? 0 : (79 - len);

   /* Output each entry */
   for (j = 0; j < k; j++)
   {
      /* Get the index */
      i = out_index[j];

      /* Get the item */
      i_ptr = &inventory[i];

      /* Clear the line */
      prt("", col ? col - 2 : col, j+1);

      /* Prepare an index --(-- */
      sprintf(tmp_val, "%c)", index_to_label(i));

      /* Clear the line with the (possibly indented) index */
      put_str(tmp_val, col, j+1);

      /* Display the entry itself */
      c_put_str(out_color[j], out_desc[j], col + 3, j+1);

      /* Display the weight if needed */
      if (show_inven_weight)
      {
         int wgt = i_ptr->weight * i_ptr->number;
         sprintf(tmp_val, "%3d.%d lb", wgt / 10, wgt % 10);
         put_str(tmp_val, 71, j+1);
      }
   }

   /* Make a "shadow" below the list (only if needed) */
   if (j && (j < 23)) prt("", col ? col - 2 : col, j+1);
}

/*
 * Display the equipment.
 */
void show_equip(void)
{
   int i, j, k, l;
   int col, len, lim;

   object_type *i_ptr;

   char tmp_val[80];

   char i_name[80];

   int out_index[23];
   byte out_color[23];
   char out_desc[23][80];

   /* Default length */
   len = 79 - 50;

   /* Maximum space allowed for descriptions */
   lim = 79 - 3;

   /* Require space for labels (if needed) */
   if (show_equip_label) lim -= (8 + 2);

   /* Require space for weight (if needed) */
   if (show_equip_weight) lim -= 9;

   /* Scan the equipment list */
   for (k = 0, i = INVEN_WIELD; i < INVEN_TOTAL; i++)
   {
      if ((i == INVEN_BODY) && (p_ptr->pclass == CLASS_GLADIATR)) continue;
      if ((i == INVEN_AMMO) && (p_ptr->pclass == CLASS_HIGHPRST)) continue;
      if ((i == INVEN_ARM) && (p_ptr->twohands == TRUE)) continue;

      i_ptr = &inventory[i];

      /* Is this item acceptable? */
      if (!item_tester_okay(i_ptr)) continue;

      /* Description */
      object_desc(i_name, i_ptr, TRUE, 3);

      /* Truncate the description */
      i_name[lim] = 0;

      /* Save the index */
      out_index[k] = i;

      /* Acquire inventory color */
      out_color[k] = tval_to_attr[i_ptr->tval & 0x7F];

      /* Save the description */
      strcpy(out_desc[k], i_name);

      /* Extract the maximal length (see below) */
      l = strlen(out_desc[k]) + (2 + 3);

      /* Increase length for labels (if needed) */
      if (show_equip_label) l += (14 + 2);

      /* Increase length for weight (if needed) */
      if (show_equip_weight) l += 9;

      /* Maintain the max-length */
      if (l > len) len = l;

      /* Advance the entry */
      k++;
   }

   /* Hack -- Find a column to start in */
   col = (len > 76) ? 0 : (79 - len);

   /* Output each entry */
   for (j = 0; j < k; j++)
   {
      /* Get the index */
      i = out_index[j];

      /* Get the item */
      i_ptr = &inventory[i];

      /* Clear the line */
      prt("", col ? col - 2 : col, j+1);

      /* Prepare an index --(-- */
      sprintf(tmp_val, "%c)", index_to_label(i));

      /* Clear the line with the (possibly indented) index */
      put_str(tmp_val, col, j+1);

      /* Use labels */
      if (show_equip_label)
      {
         /* Mention the use */
         sprintf(tmp_val, "%-8s: ", mention_use(i));
         put_str(tmp_val, col + 3, j+1);

         /* Display the entry itself */
         c_put_str(out_color[j], out_desc[j], col + 3 + 8 + 1, j+1);
      }

      /* No labels */
      else
      {
         /* Display the entry itself */
         c_put_str(out_color[j], out_desc[j], col + 3, j+1);
      }

      /* Display the weight if needed */
      if (show_equip_weight)
      {
         int wgt = i_ptr->weight * i_ptr->number;
         sprintf(tmp_val, "%3d.%d lb", wgt / 10, wgt % 10);
         put_str(tmp_val, 71, j+1);
      }
   }

   /* Make a "shadow" below the list (only if needed) */
   if (j && (j < 23)) prt("", col ? col - 2 : col, j+1);
}

/*
 * Display the floor.
 */
void show_floor(s16b x, s16b y)
{
   int i, j, k, l, z;
   int col, len, lim;

   object_type *i_ptr;

   char tmp_val[80];

   char i_name[80];

   int out_index[23];
   byte out_color[23];
   char out_desc[23][80];

   /* Default length */
   len = 79 - 50;

   /* Maximum space allowed for descriptions */
   lim = 79 - 3;

   /* Require space for weight (if needed) */
   if (show_floor_weight) lim -= 9;

   /* Find the "final" slot */
   z = objects_on_floor(x,y);

   /* Scan the inventory list */
   for (k = 0, i = 0; i < z; i++)
   {
      /* Examine the item */
      i_ptr = get_item_pointer_floor_xy(i,x,y);

      /* Is this item acceptable? */
      if (!item_tester_okay(i_ptr)) continue;

      /* Description */
      object_desc(i_name, i_ptr, TRUE, 3);

      /* Truncate the description */
      i_name[lim] = 0;

      /* Save the index */
      out_index[k] = i;

      /* Acquire inventory color */
      out_color[k] = tval_to_attr[i_ptr->tval & 0x7F];

      /* Save the description */
      strcpy(out_desc[k], i_name);

      /* Extract the maximal length (see below) */
      l = strlen(out_desc[k]) + (2 + 3);

      /* Increase length for weight (if needed) */
      if (show_floor_weight) l += 9;

      /* Maintain the max-length */
      if (l > len) len = l;

      /* Advance the entry */
      k++;
   }

   /* Hack -- Find a column to start in */
   col = (len > 76) ? 0 : (79 - len);

   /* Output each entry */
   for (j = 0; j < k; j++)
   {
      /* Get the index */
      i = out_index[j];

      /* Get the item */
      i_ptr = &inventory[i];

      /* Clear the line */
      prt("", col ? col - 2 : col, j+1);

      /* Prepare an index --(-- */
      sprintf(tmp_val, "%c)", index_to_label(i));

      /* Clear the line with the (possibly indented) index */
      put_str(tmp_val, col, j+1);

      /* Display the entry itself */
      c_put_str(out_color[j], out_desc[j], col + 3, j+1);

      /* Display the weight if needed */
      if (show_floor_weight)
      {
         int wgt = i_ptr->weight * i_ptr->number;
         sprintf(tmp_val, "%3d.%d lb", wgt / 10, wgt % 10);
         put_str(tmp_val, 71, j+1);
      }
   }

   /* Make a "shadow" below the list (only if needed) */
   if (j && (j < 23)) prt("", col ? col - 2 : col, j+1);
}

/*
 * Verify the choice of an item.
 */
static bool verify(cptr prompt, s16b item)
{
   char        i_name[80];
   char        out_val[160];
   object_type *i_ptr;

/* jk - allowed for floor items */
   /* Describe */
   i_ptr=get_item_pointer(item);
   object_desc(i_name, i_ptr, TRUE, 3);

   /* Prompt */
   (void)sprintf(out_val, "%s %s? ", prompt, i_name);

   /* Query */
   return (get_check(out_val));
}


/*
 * Hack -- allow user to "prevent" certain choices
 */
/* jk - adapted for multi-floor items */
static bool get_item_allow(s16b i)
{
   cptr s;

   object_type *i_ptr;

   i_ptr=get_item_pointer(i);

   /* No inscription */
   if (!i_ptr->note) return (TRUE);

   /* Find a '!' */
   s = strchr(quark_str(i_ptr->note), '!');

   /* Process preventions */
   while (s)
   {
      /* Check the "restriction" */
      if ((s[1] == p_ptr->command_cmd) || (s[1] == '*'))
      {
         /* Verify the choice */
         if (!verify("Really try", i)) return (FALSE);
      }

      /* Find another '!' */
      s = strchr(s + 1, '!');
   }

   /* Allow it */
   return (TRUE);
}

/*
 * Auxiliary function for "get_item()" -- test an index
 */
static bool get_item_okay(s16b i)
{
   /* Illegal items */
   if ((i < 0) ||
      (i > (INVEN_TOTAL+objects_on_floor(px,py)-1))) return (FALSE);

   /* Verify the item */
   return (item_tester_okay(get_item_pointer(i)));
}

/*
 * Find the "first" inventory object with the given "tag".
 *
 * A "tag" is a char "n" appearing as "@n" anywhere in the
 * inscription of an object.
 *
 * Also, the tag "@xn" will work as well, where "n" is a tag-char,
 * and "x" is the "current" p_ptr->command_cmd code.
 */
static s16b get_tag(s16b *cp, char tag)
{
   s16b i;
   cptr s;
/* jk - to allow for searching on the floor */

   /* Check every object */
   for (i = 0; i < INVEN_PACK;i++)
   {
      object_type *i_ptr = &inventory[i];
      if (!i_ptr->k_idx) continue;         /* Skip empty objects */
      if (!i_ptr->note) continue;          /* Skip empty inscriptions */
      s = strchr(quark_str(i_ptr->note), '@'); /* Find a '@' */
      while (s)       /* Process all tags */
      {
         if (s[1] == tag)        /* Check the normal tags */
         {
            *cp = i;          /* Save the actual inventory ID */
            return (TRUE);       /* Success */
         }
         if ((s[1] == p_ptr->command_cmd) && (s[2] == tag))         /* Check the special tags */
         {
            *cp = i;          /* Save the actual inventory ID */
            return (TRUE);       /* Success */
         }
         s = strchr(s + 1, '@');    /* Find another '@' */
      }
   }

   /* Check every object */
   for (i = objects_on_floor(px,py)-1; i >=0; i--)
   {
      object_type *i_ptr = get_item_pointer_floor(i);
      if (!i_ptr->note) continue;          /* Skip empty inscriptions */
      s = strchr(quark_str(i_ptr->note), '@'); /* Find a '@' */
      while (s)       /* Process all tags */
      {
         if (s[1] == tag)        /* Check the normal tags */
         {
            *cp = i+INVEN_TOTAL; /* Save the actual inventory ID */
            return (TRUE);       /* Success */
         }
         if ((s[1] == p_ptr->command_cmd) && (s[2] == tag)) /* Check the special tags */
         {
            *cp = i+INVEN_TOTAL; /* Save the actual inventory ID */
            return (TRUE);       /* Success */
         }
         s = strchr(s + 1, '@');    /* Find another '@' */
      }
   }

   /* No such tag */
   return (FALSE);
}

extern s16b count_items(s16b *first_item, bool equip, bool inven, bool floor)
{
   s16b i, result = 0;

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
   if (floor)
   {
      for (i=INVEN_TOTAL; i<objects_on_floor(px, py); i++)
      {
         if (get_item_okay(i))
         {
            result++;
            (*first_item)=i;
         }
      }
   }
   return (result);
}

/*
 * return an appropriate color for displaying the currently carried weight
 */
s16b weight_color(void)
{
   s16b max_weight;
   max_weight = weight_limit();

   if (p_ptr->total_weight < (max_weight / 3) )
   {
      return (TERM_WHITE);
   }
   else if (p_ptr->total_weight < (max_weight / 2) )
   {
      return (TERM_GREEN);
   }
   else if ( (p_ptr->total_weight >= (max_weight / 2)) && (p_ptr->total_weight < (2*(max_weight/3))))
   {
      return (TERM_YELLOW);
   }
   else if ( (p_ptr->total_weight >= (2*(max_weight/3)) ) && (p_ptr->total_weight < max_weight))
   {
      return (TERM_ORANGE);
   }
   else
   {
      return (TERM_RED);
   }
}

/*
 * Let the user select an item, return its "index"
 *
 * The selected item must satisfy the "item_tester_hook()" function,
 * if that hook is set, and the "item_tester_tval", if that value is set.
 *
 * All "item_tester" restrictions are cleared before this function returns.
 *
 * The user is allowed to choose acceptable items from the equipment,
 * inventory, or floor, respectively, if the proper flag was given,
 * and there are any acceptable items in that location.  Note that
 * the equipment or inventory are displayed (even if no acceptable
 * items are in that location) if the proper flag was given.
 *
 * Note that the user must press "-" to specify the item on the floor,
 * JK - - selects the first item on the floor.
 * and there is no way to "examine" the item on the floor, while the
 * use of "capital" letters will "examine" an inventory/equipment item,
 * and prompt for its use.
 *
 * If a legal item is selected, we save it in "cp" and return TRUE.
 * If this "legal" item is on the floor, we use a "cp" equal to zero
 * minus the dungeon index of the item on the floor.
 * IF THE ITEM IS ON THE FLOOR, WE RETURN INVEN_TOTAL+FLOOR_INDEX.
 * NOT ZERO ANYMORE!
 *
 * Otherwise, we return FALSE, and set "cp" to:
 *   -1 for "User hit space/escape"
 *   -2 for "No legal items to choose"
 *   -3 for "User pressed @"
 *
 * p_ptr->command_new is used when viewing the inventory or equipment
 * to allow the user to enter a command while viewing those screens, and
 * also to induce "auto-enter" of stores, and other such stuff.
 *
 * p_ptr->command_see may be set before calling this function to start
 * out in "browse" mode.  It is cleared before this function returns.
 *
 * p_ptr->command_wrk is used to choose between equip/inven listings.
 * If it is 2 then we are viewing inventory, else 1 equipment.
 * 0 floor
 * why was p_ptr->command_wrk defined as s16b and used as int?
 *
 * We always erase the prompt when we are done.
 *
 * Note that "Term_save()" / "Term_load()" blocks must not overlap.
 */
bool get_item(s16b *cp, s16b *amt, cptr pmt, bool equip, bool inven, bool floor)
{
   char         n1, n2, which = ' ';
   s16b         k, i1, i2, e1, e2, fl1, fl2;
   bool         ver, done, item;
   char         out_val1[80], out_val2[80], out_val3[80];
   object_type *i_ptr;
   char         i_name[80];
   s16b         index_floor[MAX_FLOOR_ITEMS+INVEN_TOTAL];
   s16b         index_equip[INVEN_TOTAL-INVEN_WIELD];
   s16b         index_inven[INVEN_WIELD];
   s16b         max_floor = 0;
   s16b         max_equip = 0;
   s16b         max_inven = 0;

   msg_print(NULL);

   done = FALSE; /* Not done */
   item = FALSE; /* No item selected */
   *cp = -1;     /* Default to "no item" (see above) */

   /* if there is nothing in inven, on floor and in equip, return */
   if (!inven && !equip && !floor) return (FALSE);

   for (k=0;k<MAX_FLOOR_ITEMS;k++) index_floor[k]=0;
   for (k=0;k<INVEN_TOTAL-INVEN_WIELD;k++) index_equip[k]=0;
   for (k=0;k<INVEN_WIELD;k++) index_inven[k]=0;

   /* Full inventory */
   i1 = 0;
   i2 = INVEN_PACK - 1;

   /* Forbid inventory */
   if (!inven) i2 = -1;

   /* Restrict inventory indexes */
   while ((i1 <= i2) && (!get_item_okay(i1))) i1++;
   while ((i1 <= i2) && (!get_item_okay(i2))) i2--;
   for (k=i1;k<=i2;k++)
   {
      if (get_item_okay(k)) index_inven[max_inven++]=k;
   }

   /* Full equipment */
   e1 = INVEN_WIELD;
   e2 = INVEN_TOTAL - 1;

   /* Forbid equipment */
   if (!equip) e2 = -1;

   /* Restrict equipment indexes */
   while ((e1 <= e2) && (!get_item_okay(e1))) e1++;
   while ((e1 <= e2) && (!get_item_okay(e2))) e2--;
   for (k=e1;k<=e2;k++)
   {
      if (get_item_okay(k)) index_equip[max_equip++]=k;
   }

   /* Full floor */
   fl1 = INVEN_TOTAL;
   fl2 = INVEN_TOTAL+objects_on_floor(px,py)-1;

   /* Forbid equipment */
   if (!floor) fl2 = -1;

   /* Restrict equipment indexes */
   while ((fl1 <= fl2) && (!get_item_okay(fl1))) fl1++;
   while ((fl1 <= fl2) && (!get_item_okay(fl2))) fl2--;
   /* fill index */
   for (k=fl1;k<=fl2;k++)
    {
      if (get_item_okay(k)) index_floor[max_floor++]=k;
   }

   /* Verify choices */
   if ((max_inven==0) && (max_equip==0) && (max_floor==0))
   {
      /* Cancel p_ptr->command_see */
      p_ptr->command_see = FALSE;

      /* Hack -- Nothing to choose */
      *cp = -2;

      /* Done */
      done = TRUE;
   }

   /* Analyze choices */
   else
   {
      /* Hack -- Start on equipment if requested */
      if (p_ptr->command_see && p_ptr->command_wrk==2 && equip)
      {
         p_ptr->command_wrk = 1;
      }

      /* Use inventory if allowed */
      else if (inven)
      {
         p_ptr->command_wrk = 2;
      }

      /* Use equipment if allowed */
      else if (equip)
      {
         p_ptr->command_wrk = 1;
      }

      /* Use inventory for floor */
      else
      {
         p_ptr->command_wrk = 0;
      }
   }

   /* Hack -- start out in "display" mode */
   if (p_ptr->command_see)
   {
      Term_save();
   }

   /* Repeat until done */
   while (!done)
   {
#if 0
      /* weight display is nice, but takes a lot of room and is not   */
      /* relevant to what you see: you see the weight you're carrying */
      /* not the weight of the pile on the floor, etc.                */
      sprintf(out_val2, "%d.%d lb ",
              p_ptr->total_weight / 10, p_ptr->total_weight % 10);
#else
      out_val2[0]='\0';
#endif

      /* Viewing inventory */
      if (p_ptr->command_wrk==2)
      {
         /* Extract the legal requests */
         n1 = I2A(i1);
         n2 = I2A(i2);
         if (p_ptr->command_see) show_inven(); /* Redraw if needed */
         /* Begin the prompt */
         sprintf(out_val1, "Inven:");

         out_val3[0]='\0';
         /* Some legal items */
         if (i1 <= i2)
         {
            /* Build the prompt */
            sprintf(out_val3, " %c-%c,", index_to_label(i1), index_to_label(i2));
         }

         /* Indicate ability to "view" */
         if (!p_ptr->command_see) strcat(out_val3, " * to see,");

         /* Append */
         if ((e1<=e2) && (fl1<=fl2))
         {
            strcat(out_val3," / for Equip/Floor,");
         }
         else if (e1<=e2)
         {
            strcat(out_val3, " / for Equip,");
         }
         else if (fl1<=fl2)
         {
            strcat(out_val3, " / for Floor,");
         }
      }

      /* Viewing equipment */
      else if (p_ptr->command_wrk==1)
      {
         /* Extract the legal requests */
         n1 = I2A(e1 - INVEN_WIELD);
         n2 = I2A(e2 - INVEN_WIELD);
         if (p_ptr->command_see) show_equip();  /* Redraw if needed */
         /* Begin the prompt */
         sprintf(out_val1, "Equip:");

         out_val3[0]='\0';
         /* Some legal items */
         if (e1 <= e2)
         {
            /* Build the prompt */
            sprintf(out_val3, " %c-%c,", index_to_label(e1), index_to_label(e2));
         }

         /* Indicate ability to "view" */
         if (!p_ptr->command_see) strcat(out_val3, " * to see,");

         /* Append */
         if ((i1<=i2) && (fl1<=fl2))
         {
            strcat(out_val3," / for Inven/Floor,");
         }
         else if (i1<=i2)
         {
            strcat(out_val3, " / for Inven,");
         }
         else if (fl1<=fl2)
         {
            strcat(out_val3, " / for Floor,");
         }
      }

      /* Viewing floor */
      else if (p_ptr->command_wrk==0)
      {
         /* Extract the legal requests */
         n1 = I2A(fl1-INVEN_TOTAL);
         n2 = I2A(fl2-INVEN_TOTAL);
         if (p_ptr->command_see) show_floor(px,py); /* Redraw if needed */
         /* Begin the prompt */
         sprintf(out_val1, "Floor:");

         out_val3[0]='\0';
         /* Some legal items */
         if (fl1 <= fl2)
         {
            /* Build the prompt */
            sprintf(out_val3, " %c-%c,",I2A(fl1-INVEN_TOTAL), I2A(fl2-INVEN_TOTAL));
         }

         /* Indicate ability to "view" */
         if (!p_ptr->command_see) strcat(out_val3, " * to see,");

         /* Append */
         if ((e1<=e2) && (i1<=i2))
         {
            strcat(out_val3," / for Equip/Inven,");
         }
         else if ((e1<=e2))
         {
            strcat(out_val3, " / for Equip,");
         }
         else if ((i1<=i2))
         {
            strcat(out_val3, " / for Inven,");
         }
      }

      strcat(out_val3, " ESC - ");                   /* Finish the prompt */

      prt("", 0, MESSAGE_ROW);

      c_put_str(TERM_WHITE, out_val1, 0, MESSAGE_ROW);
      c_put_str(weight_color(), out_val2, 0+strlen(out_val1), MESSAGE_ROW);
      c_put_str(TERM_WHITE,out_val3, 0+strlen(out_val1)+strlen(out_val2), MESSAGE_ROW);
      c_put_str(TERM_WHITE ,pmt, 0+strlen(out_val1)+strlen(out_val2)+strlen(out_val3), MESSAGE_ROW);

      which = inkey();                           /* Get a key */
      switch (which)                             /* Parse it */
      {
         case ESCAPE:

            done = TRUE;
            break;
         case '@':
            (*cp)=-3;
            done = TRUE;
            break;

         case '*':
         case '?':
         case ' ':

            /* Show/hide the list */
            if (!p_ptr->command_see)
            {
               Term_save();
               p_ptr->command_see = TRUE;
            }
            else
            {
               Term_load();
               p_ptr->command_see = FALSE;
            }
            break;

         /* - means first item on the floor, and pressing /a gets tiring */
         case '-':

            if (fl1==fl2)
            {
               k=fl1;

               /* Validate the item */
               if (!get_item_okay(k))
               {
                  bell("Item not allowed here");
                  break;
               }

               /* Allow player to "refuse" certain actions */
               if (!get_item_allow(k))
               {
                  done = TRUE;
                  break;
               }
               /* Accept that choice */
               (*cp) = k;
               item = TRUE;
               done = TRUE;
               break;
            }

            /* - with more than 1 item on the floor is '/' */
            if (!inven && !equip && !floor)
            {
               bell("Cannot switch item selector!");
               break;
            }

            /* Fix screen */
            if (p_ptr->command_see)
            {
               Term_load();
               Term_save();
            }

            /* Switch to other stack */
            if (p_ptr->command_wrk==2) /* now inven */
            {
               if ((floor) && (fl1<=fl2))
               {
                  p_ptr->command_wrk=0;
               }
               else if ((equip) && (e1<=e2))
               {
                  p_ptr->command_wrk=1;
               }
            }
            else if (p_ptr->command_wrk==1) /* now equip */
            {
               if ((inven) && (i1<=i2))
               {
                  p_ptr->command_wrk=2;
               }
               else if ((floor) && (fl1<=fl2))
               {
                  p_ptr->command_wrk=0;
               }
            }
            else if (p_ptr->command_wrk==0) /* now floor */
            {
               if ((equip) && (e1<=e2))
               {
                  p_ptr->command_wrk=1;
               }
               else if ((inven) && (i1<=i2))
               {
                  p_ptr->command_wrk=2;
               }
            }
            break;

         case '/':

            /* Verify legality */
            if (!inven && !equip && !floor)
            {
               bell("Cannot switch item selector!");
               break;
            }

            /* Fix screen */
            if (p_ptr->command_see)
            {
               Term_load();
               Term_save();
            }

            /* Switch to other stack */
            if (p_ptr->command_wrk==2) /* now inven */
            {
               if ((floor) && (fl1<=fl2))
               {
                  p_ptr->command_wrk=0;
               }
               else if ((equip) && (e1<=e2))
               {
                  p_ptr->command_wrk=1;
               }
            }
            else if (p_ptr->command_wrk==1) /* now equip */
            {
               if ((inven) && (i1<=i2))
               {
                  p_ptr->command_wrk=2;
               }
               else if ((floor) && (fl1<=fl2))
               {
                  p_ptr->command_wrk=0;
               }
            }
            else if (p_ptr->command_wrk==0) /* now floor */
            {
               if ((equip) && (e1<=e2))
               {
                  p_ptr->command_wrk=1;
               }
               else if ((inven) && (i1<=i2))
               {
                  p_ptr->command_wrk=2;
               }
            }

            /* Need to redraw */
            break;

         case '0':
         case '1': case '2': case '3':
         case '4': case '5': case '6':
         case '7': case '8': case '9':

            /* XXX XXX Look up that tag */
            if (!get_tag(&k, which))
            {
               bell("No tag found like that!");
               break;
            }

            /* Validate the item */
            if (!get_item_okay(k))
            {
               bell("Item not allowed here");
               break;
            }

            /* Allow player to "refuse" certain actions */
            if (!get_item_allow(k))
            {
               done = TRUE;
               break;
            }

            /* Use that item */
            (*cp) = k;
            item = TRUE;
            done = TRUE;
            break;

         case '\n':
         case '\r':

            /* Choose "default" inventory item */
            if (p_ptr->command_wrk==2)
            {
               k = ((i1 == i2) ? i1 : -1);
            }

            /* Choose "default" equipment item */
            else if (p_ptr->command_wrk==1)
            {
               k = ((e1 == e2) ? e1 : -1);
            }

            /* Choose "default" floor item */
            else if (p_ptr->command_wrk==0)
            {
               k = ((fl1 == fl2) ? fl1 : -1);
            }


            /* Validate the item */
            if (!get_item_okay(k))
            {
               bell("Item not allowed here");
               break;
            }

            /* Allow player to "refuse" certain actions */
            if (!get_item_allow(k))
            {
               done = TRUE;
               break;
            }

            /* Accept that choice */
            (*cp) = k;
            item = TRUE;
            done = TRUE;
            break;

         default:

            /* Extract "query" setting */
            ver = ang_isupper(which);
            if (ver) which = tolower(which);

            /* Convert letter to inventory index */
            if (p_ptr->command_wrk==2)
            {
               k = label_to_inven(which);
            }

            /* Convert letter to equipment index */
            else if (p_ptr->command_wrk==1)
            {
               k = label_to_equip(which);
            }

            /* Convert letter to floor index */
            else if (p_ptr->command_wrk==0)
            {
               k = label_to_floor(which);
            }

            /* Validate the item */
            if (!get_item_okay(k))
            {
               bell("Item not allowed here");
               break;
            }

            /* Verify, abort if requested */
            if (ver && !verify("Try", k))
            {
               done = TRUE;
               break;
            }

            /* Allow player to "refuse" certain actions */
            if (!get_item_allow(k))
            {
               done = TRUE;
               break;
            }

            /* Accept that choice */
            (*cp) = k;
            item = TRUE;
            done = TRUE;
            break;
      }
   }

   /* Fix the screen if necessary */
   if (p_ptr->command_see) Term_load();

   /* Hack -- Cancel "display" */
   p_ptr->command_see = FALSE;

   /* Forget the item_tester_tval restriction */
   item_tester_tval = 0;

   /* Forget the item_tester_hook restriction */
   item_tester_hook = NULL;

   /* Clear the prompt line */
   prt("", 0, 0);

   /* did we abort for any reason? */
   if (!item) return (FALSE);

   /* test to see how many of that object there are */
   i_ptr=get_item_pointer(*cp);

   /* if we say amt==-1, we are called from do_cmd_wield, were we can but wield 1 */
   /* of everything except arrows! */
   if ((*amt)==-1)
   {
      if (wield_slot(i_ptr)==INVEN_AMMO)
      {
         (*amt) = 0;
      }
      else
      {
         (*amt) = 1;
      }
   }

   /* if amt==1, only one is allowed (zapping a rod, for example) */
   /* else, ask how many */
   if (((*amt)!=1) && (i_ptr->number>1) && ((*cp)!=-1) && ((*cp)!=-2))
   {
      object_desc(i_name, i_ptr, FALSE, 0);
      (*amt)=get_quantity(format("How many (1-%d) %s? ",i_ptr->number,i_name),
                          i_ptr->number,i_ptr->number);
   }

   /* if we choose an item there's only one of, make sure we want 1 of that item */
   if ((i_ptr->number==1) && ((*cp)!=-1) &&
       ((*cp)!=-2) && ((*amt)==0))
   {
      (*amt)=1;
   }

   /* if we aborted, we returned earlier on! */
   return (TRUE);
}

