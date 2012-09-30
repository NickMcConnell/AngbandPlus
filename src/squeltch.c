/*
 * Squeltch.c        Iain McFall 22/Aug/2000       imcfall@lineone.net
 * This file contains functions for the menu management and 
 * actual destroying of unwanted items
 */


/* Issues to consider:

1. Artifacts are never destroyed == Possible abuse
2. Fixed!  
3. Performance? - the squeltch_? calls are called a _lot_ but I think
   they are pretty fast. No noticable lag on my PII. Probably Negligable.
4. Probably a bit verbose at times but (hopefully) readable.
5. Adjustment of the aware property for object kinds. If non-flavoured items
   are not made aware of until the character steps on one/buys one then the
   squeltch menu will not spoil the player with things to come.	Not setting
   non-flavoured items aware property to true at the beginning of a game
   does not, as far as I can tell, make any difference.
6. Nasty assumptions are made about the #define's i.e. if (k_ptr->squeltch>DESTROY_ALL)
   Changing the defines for ITEM_? DESTROY_? SENSE_? can have
   a nasty effect on this module.
7. No more room in ITEM_POTION1! (Well not for the screen anyway!)
8. New tval types can easily be integrated into existing categories by adding to 
   init_categories().
9. New categories can be created by adding new ITEM_? defines, and adding to the
   string array categories[]. They will default to destroy none/all only without 
   extra help.
*/


#include "angband.h"

#define DESTROY_NONE    0
#define DESTROY_CURSED	1
#define DESTROY_AVERAGE 2
#define DESTROY_GOOD	3
#define DESTROY_ALL		4


#define ITEM_SWORD			1
#define ITEM_HAFTED			2
#define ITEM_BLUNT			3
#define ITEM_AXE                        4
#define ITEM_BOOK_DAEM                  5
#define ITEM_BOW                        6
#define ITEM_AMMO                       7
#define ITEM_DIGGER                     8

#define ITEM_BODY_ARMOUR        9
#define ITEM_DRAG_ARMOUR        10
#define ITEM_OTHER_ARMOUR       11

#define ITEM_RING                       12
#define ITEM_AMULET                     13

#define ITEM_POTION1            14
#define ITEM_POTION2            15
#define ITEM_SCROLL                     16
#define ITEM_ROD                        17
#define ITEM_STAFF                      18
#define ITEM_WAND                       19

#define ITEM_FOOD                       20
#define ITEM_CRAP                       21
#define ITEM_FIRESTONE          22
#define ITEM_ESSENCE            23
#define ITEM_PARCHMENT          24
#define ITEM_INSTRUMENT         25
#define ITEM_RUNE                       26
#define ITEM_STONE                      27
#define ITEM_BOOK_SPIR          28
#define ITEM_BOOK_SONG          29
#define ITEM_BOOK_SYMB          30

#define ITEM_BOOK_VALA          31
#define ITEM_BOOK_MAGE          32
#define ITEM_BOOK_SHAD          33
#define ITEM_BOOK_CHAO          34
#define ITEM_BOOK_NETH          35
#define ITEM_BOOK_CRUS          36
#define ITEM_BOOK_SIGA          37
#define ITEM_BOOK_MAGI          38
#define ITEM_BOOK_PRAY          39
#define ITEM_BOOK_ILLU          40
#define ITEM_BOOK_TRIB          41

#define ITEM_CORPSE                     42

#define ITEM_MAX                        43


static int cat_type[256];

static cptr categories[] =
{
        NULL,"Swords","Polearms","Blunt Weapons","Axe","Daemon Books",
        "Bows","Ammo","Dig/M.Staff/Boom/Trap",
        "Body Armour","Dragon Armour","Other Armour",
        "Rings","Amulets",
        "Potions", "Potions(2!)","Scrolls",
        "Rods","Staffs","Wands",
        "Food","Boring Stuff", "Firestones","Essences","Parchments",
        "Instruments","Runes","Stones",
        "Spirit Books","Songbooks","Symbiotic Books","Valarin Books","Magery Books",
        "Shadow Books","Chaos Books","Nether Books","Crusade Books","Sigaldry Books",
        "Magic Books","Prayer Books","Illusion Books","Tribal Books","Corpses"
};

static bool sq_init = FALSE;	



static char listsym[] =
{
	'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
	'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
	'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
	'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
	'0', '1', '2', '3', '4', '5', '6', '7', '8', '9','\0'
};

/*
  Strip an "object name" into a buffer, pluralising as appropriate
*/
static void get_plural_name(char *buf, int k_idx)
{
	char *t;

	object_kind *k_ptr = &k_info[k_idx];

	cptr str = (k_name + k_ptr->name);


	/* Skip past leading characters */
	while ((*str == ' ') || (*str == '&')) str++;

	/* Copy useful chars */
	for (t = buf; *str; str++)
	{
		if (*str != '~') 
		{
			*t++ = *str;
		}
		else
		{
			/* 'es' suffix for stuff ending in 's' or 'h' */
			if ((*(t-1)=='s') || (*(t-1)=='h')) *t++ = 'e';
		
			*t++ = 's';
		}
	}

	/* Terminate the new name */
	*t = '\0';
}

static void init_categories(void)
{
	int i;

	/* C_WIPE? */
	for (i = 1; i < 256; i++) cat_type[i]=0;
	
	/* Stuff to hurt with */
        cat_type[TV_AXE]=ITEM_AXE;
	cat_type[TV_SWORD]=ITEM_SWORD;
	cat_type[TV_POLEARM]=ITEM_HAFTED;
	cat_type[TV_HAFTED]=ITEM_BLUNT;
	cat_type[TV_BOW]=ITEM_BOW;
	cat_type[TV_SHOT]=ITEM_AMMO;
	cat_type[TV_ARROW]=ITEM_AMMO;
	cat_type[TV_BOLT]=ITEM_AMMO;
	cat_type[TV_DIGGING]=ITEM_DIGGER;
	cat_type[TV_MSTAFF]=ITEM_DIGGER;
	cat_type[TV_BOOMERANG]=ITEM_DIGGER;
        cat_type[TV_TRAPKIT]=ITEM_DIGGER;
	
	/* Stuff to protect me */
	cat_type[TV_SOFT_ARMOR]=ITEM_BODY_ARMOUR;
	cat_type[TV_HARD_ARMOR]=ITEM_BODY_ARMOUR;
	cat_type[TV_DRAG_ARMOR]=ITEM_DRAG_ARMOUR;
	cat_type[TV_HELM]=ITEM_OTHER_ARMOUR;
	cat_type[TV_CROWN]=ITEM_OTHER_ARMOUR;
	cat_type[TV_BOOTS]=ITEM_OTHER_ARMOUR;
	cat_type[TV_CLOAK]=ITEM_OTHER_ARMOUR;
	cat_type[TV_GLOVES]=ITEM_OTHER_ARMOUR;
	cat_type[TV_SHIELD]=ITEM_OTHER_ARMOUR;

	/* Basic dungeon necessities */
	cat_type[TV_RING]=ITEM_RING;
	cat_type[TV_AMULET]=ITEM_AMULET;
	cat_type[TV_POTION]=ITEM_POTION1;
	cat_type[TV_POTION2]=ITEM_POTION2;
	cat_type[TV_SCROLL]=ITEM_SCROLL;
	cat_type[TV_ROD]=ITEM_ROD;
        cat_type[TV_ROD_MAIN]=ITEM_ROD;
	cat_type[TV_STAFF]=ITEM_STAFF;
	cat_type[TV_WAND]=ITEM_WAND;

	/* Boring stuff */
	cat_type[TV_FOOD]=ITEM_FOOD;
	cat_type[TV_SKELETON]=ITEM_CRAP;
	cat_type[TV_BOTTLE]=ITEM_CRAP;
	cat_type[TV_SPIKE]=ITEM_CRAP;
	cat_type[TV_CORPSE]=ITEM_CORPSE;
	cat_type[TV_JUNK]=ITEM_CRAP;
	cat_type[TV_LITE]=ITEM_CRAP;
	cat_type[TV_FLASK]=ITEM_CRAP;
	cat_type[TV_PARCHEMENT]=ITEM_PARCHMENT;

	/* Stuff for particular classes */
	cat_type[TV_FIRESTONE]=ITEM_FIRESTONE;
	cat_type[TV_BATERIE]=ITEM_ESSENCE;
	cat_type[TV_INSTRUMENT]=ITEM_INSTRUMENT;
	cat_type[TV_RUNE1]=ITEM_RUNE;
	cat_type[TV_RUNE2]=ITEM_RUNE;
	cat_type[TV_DRUID_BOOK]=ITEM_STONE;
	cat_type[TV_MUSIC_BOOK]=ITEM_BOOK_SONG;
        cat_type[TV_SPIRIT_BOOK]=ITEM_BOOK_SPIR;
	cat_type[TV_SYMBIOTIC_BOOK]=ITEM_BOOK_SYMB;
	cat_type[TV_VALARIN_BOOK]=ITEM_BOOK_VALA;
	cat_type[TV_MAGERY_BOOK]=ITEM_BOOK_MAGE;
	cat_type[TV_SHADOW_BOOK]=ITEM_BOOK_SHAD;
	cat_type[TV_CHAOS_BOOK]=ITEM_BOOK_CHAO;
	cat_type[TV_NETHER_BOOK]=ITEM_BOOK_NETH;
	cat_type[TV_CRUSADE_BOOK]=ITEM_BOOK_CRUS;
	cat_type[TV_SIGALDRY_BOOK]=ITEM_BOOK_SIGA;
	cat_type[TV_MAGIC_BOOK]=ITEM_BOOK_MAGI;
	cat_type[TV_PRAYER_BOOK]=ITEM_BOOK_PRAY;
	cat_type[TV_ILLUSION_BOOK]=ITEM_BOOK_ILLU;
	cat_type[TV_TRIBAL_BOOK]=ITEM_BOOK_TRIB;
	cat_type[TV_DAEMON_BOOK]=ITEM_BOOK_DAEM;

	sq_init=TRUE;
}

static bool is_weapon(int kind)
{
	if (!sq_init) init_categories();
	return (cat_type[kind]<ITEM_BODY_ARMOUR);
}

static bool is_armour(int kind)
{
	if (!sq_init) init_categories();
	return ((cat_type[kind]>ITEM_DIGGER) && (cat_type[kind]<ITEM_RING));
}

static bool can_be_cursed(int kind)
{
	if (!sq_init) init_categories();
	return(cat_type[kind]<ITEM_POTION1);
}


void do_cmd_squeltch_options(void)
{
	int i,count,row,col,category,num,kind;
	bool cats[ITEM_MAX];
	int toshow[ITEM_MAX];
	int choice[65];
	char ch;
	char buf[160];
	object_kind *k_ptr;

	/* Initialise the typeval categories*/
	if (!sq_init) init_categories();

	while (1)
	{
		/* Clear screen */
		Term_clear();

		/* Clear the Array */
		for (i=0;(i<ITEM_MAX);i++) cats[i]=FALSE;

		/* Get the tvals we've heard of */
		for ( i = 1; (i < max_k_idx); i++)
		{
			k_ptr = &k_info[i];

			/* Forget Specials */
			if (k_ptr->flags3 & (TR3_INSTA_ART)) continue;

			/* Got to have a type */
			if (!k_ptr->tval) continue;

			/* No Gold! */
			if (k_ptr->tval==TV_GOLD) continue;

			/* Some things are just not categorised... (TV_RANDART,TV_HYPNOS etc)*/
			if (!cat_type[k_ptr->tval]) continue;

			/* This category needs to go on the list (if aware)*/
			if (k_ptr->aware) cats[cat_type[k_ptr->tval]] = TRUE;
		}

		count=0;

		/* Crude way to count the categories */
		for (i = 1; i < ITEM_MAX; i++)
			if (cats[i])
			{
				toshow[count++]=i;
			}

		/* Put the categories on the screen */
		for (i = 0; i < count; i++)
		{
			row = 2 + (i % 14);
			col = 30 * (i / 14);
			ch = listsym[i];
			 
			prt(format("[%c] %s", ch, categories[toshow[i]]), row, col);
		}

		Term_gotoxy(0, 20);

		roff("Select a letter to enter a submenu where you can select ");
		roff("individual items.\nIn the submenu you can also use:\n");
		roff("^A:Destroy all, ^N:Destroy None, ^S:Step all");  


		/* Get a valid keypress or exit */
		if (!get_com("Select an object type :",&ch)) return;

		for (i = 0; i <count ; i++)
		{
			if (listsym[i] == ch) break;
		}

		/* Invalid responses kick you out... */
		if (i>=count) return;

		/* Got it! */
	    category=toshow[i];

		/* Clear screen again */
		Term_clear();

	
		while (1)
		{
			num=0;

			/* Move the cursor */
			Term_gotoxy(0, 1);

			/* Dump the right choices for this category */
			c_roff(TERM_WHITE, "Don't Destroy-->");
			if (category<ITEM_POTION1) c_roff(TERM_GREEN, "Destroy Cursed-->");
			if ((category<ITEM_RING) || (category==ITEM_INSTRUMENT)) c_roff(TERM_UMBER, "Destroy Average-->");
			if (category<ITEM_RING) c_roff(TERM_BLUE, "Destroy Good-->");
			c_roff(TERM_RED, "DESTROY ALL!");

			for (i = 1; i < max_k_idx; i++)
			{
				k_ptr = &k_info[i];

				/* Analyze matching items */
				if (cat_type[k_ptr->tval] == category)
				if (!(k_ptr->flags3 & TR3_INSTA_ART))
                                if (!(k_ptr->flags3 & TR3_NORM_ART))
				if (k_ptr->aware)
				{
					byte attr;

					kind=k_ptr->tval;

					/* Wrap around */
					if (k_ptr->squeltch>DESTROY_ALL) k_ptr->squeltch=DESTROY_NONE;
		
					/* Move along if necessary */
					if ((k_ptr->squeltch==DESTROY_CURSED) && (!can_be_cursed(kind))) k_ptr->squeltch=DESTROY_AVERAGE;

					/* Instruments are awkward... */
					if (k_ptr->squeltch==DESTROY_AVERAGE) 
						if (! (is_armour(kind) || is_weapon(kind) || (kind==TV_INSTRUMENT)) )
							k_ptr->squeltch=DESTROY_ALL;
		
					/* Only weapons and armour can be considered good... */
					if (k_ptr->squeltch==DESTROY_GOOD)
						if (! (is_armour(kind) || is_weapon(kind)) )
							k_ptr->squeltch=DESTROY_ALL;

					/* Acquire the name,position and index */
					get_plural_name(buf, i);
					row = 3 + (num % 21);
					col = 30 * (num / 21);
					ch = listsym[num];

					/* Get a colour... */
					switch (k_ptr->squeltch)
					{
					case DESTROY_NONE:		attr=TERM_WHITE;
											break;
					case DESTROY_CURSED:	attr=TERM_GREEN;
											break;
					case DESTROY_AVERAGE:	attr=TERM_UMBER;
											break;
					case DESTROY_GOOD:		attr=TERM_BLUE;
											break;
					case DESTROY_ALL:		attr=TERM_RED;
											break;
					/* oops */
					default:				attr=TERM_VIOLET;
					}
					
					/* Show it */
					c_prt(attr,format("[%c] %s", ch, buf), row, col);
					
					/* Remember the object index */
					choice[num++] = i;
				}
			}

			/* Get a valid response or get kicked out */
			if (!get_com("Select an object:",&ch)) break;

			/* Destroy All! */
			if (ch == KTRL('A'))
			{
				for (i = 0; i < num; i++)
				{
					k_ptr = &k_info[choice[i]];
		
					k_ptr->squeltch=DESTROY_ALL;
				}

				continue;
			}

			/* Destroy None */
			if (ch == KTRL('N'))
			{
				for (i = 0; i < num; i++)
				{
					k_ptr = &k_info[choice[i]];
		
					k_ptr->squeltch=DESTROY_NONE;
				}

				continue;
			}

			/* Step All */
			if (ch == KTRL('S'))
			{
				for (i = 0; i < num; i++)
				{
					k_ptr = &k_info[choice[i]];
		
					k_ptr->squeltch++;
				}

				continue;
			}

			/* Find a valid keypress or get kicked out */
			for (i = 0; i <num ; i++)
			{
				if (listsym[i] == ch) break;
			}

			if (i>=num) break;

			/* Adjust the squeltch level */
			k_ptr = &k_info[choice[i]];
		
			k_ptr->squeltch++;
		

		} /*Squeltch Selection */

	}/* category Selection */
	

	return;
}


/*
 * Determine whether a particular object should be squeltched or not.
 * It gets a bit paranoid at times but I'm pretty sure that it won't
 * destroy anything you didn't tell it to...  Sensing stuff in the rest
 * of the program can be a bit strange.
 * Broken items get squeltched with cursed. Is that right?	
 * Items sensed as 'uncursed' but not known are ignored.
 */   

#define PSEUDO_ID_CURSED 0
#define PSEUDO_ID_AVERAGE 1
#define PSEUDO_ID_GOOD 2
#define PSEUDO_ID_KEEP 3

static bool destroy_it(object_type *o_ptr)
{
	object_kind *k_ptr = &k_info[o_ptr->k_idx];
	bool known;
	int sense;

	/* Destroy none */
	if (k_ptr->squeltch == DESTROY_NONE) return (FALSE);


	/* Unaware things won't be destroyed. */
	if (!object_aware_p(o_ptr)) return (FALSE);

	/* Inscribed things won't be destroyed! */
	if (o_ptr->note) return (FALSE);

	/* Keep Artifacts -- they cannot be destroyed anyway */
	if (artifact_p(o_ptr) || o_ptr->art_name) return (FALSE);

#if 0 /* Maybe .. */

	/* Destroy empty chests */
	if ((o_ptr->tval==TV_CHEST) && (o_ptr->pval==0)) return (TRUE);

#endif

	/* Destroy all */
	if (k_ptr->squeltch == DESTROY_ALL) return (TRUE);


	/*
	 * Ego items should stay (uncursed ones) because there's
	 * no "Destroy Excellent"
	 */
	if ((o_ptr->name2 || o_ptr->name2b) && !cursed_p(o_ptr)) return (FALSE);


	/* Get object ID status */
	known = (bool)object_known_p(o_ptr);

	/* To be on the safe side */
	sense = PSEUDO_ID_KEEP;

	/* Sensed and not ID'ed */
	if (!known && (o_ptr->ident & (IDENT_SENSE)))
	{
		/* Analysed pseudo-ID status */
		switch (o_ptr->sense)
		{
			/* Strong -- cursed normal items; Weak -- everything cursed */
			case SENSE_CURSED:
			/* Strong -- cursed ego */
			case SENSE_WORTHLESS:
			/* Broken items -- as per the comment above */
			case SENSE_BROKEN:
			{
				sense = PSEUDO_ID_CURSED;
				break;
			}

			/* Strong -- normal items w/o bonuses */
			case SENSE_AVERAGE:
			{
				sense = PSEUDO_ID_AVERAGE;
				break;
			}

			/* Strong -- normal items w/ bonuses */
			case SENSE_GOOD_HEAVY:
			{
				sense = PSEUDO_ID_GOOD;
				break;
			}

			/*
			 * This includes:
			 * SENSE_NONE (not sensed yet)
			 * SENSE_EXCELLENT (Strong -- ego)
			 * SENSE_TERRIBLE (Strong -- cursed artefacts)
			 * SENSE_SPECIAL (Strong -- artefacts)
			 * SENSE_GOOD_LIGHT (Weak -- everything good)
			 */
			default:
			{
				sense = PSEUDO_ID_KEEP;
				break;
			}
		}
	}


	/* Destoy good stuff */
	if (k_ptr->squeltch == DESTROY_GOOD)
	{
		/* Pseudo-ID */
		if (!known)
		{
			if (sense <= PSEUDO_ID_GOOD) return (TRUE);
			else return (FALSE);
		}

		/* ID'ed */
		else
		{
			return(TRUE);
		}
	}

	/* Check for average destroying */
	else if (k_ptr->squeltch == DESTROY_AVERAGE)
	{
		/* Pseudo-ID */
		if (!known)
		{
			if (sense <= PSEUDO_ID_AVERAGE) return (TRUE);
			return (FALSE);
		}

		/* ID'ed */
		else
		{
			if (is_weapon(o_ptr->tval))
			{
				if ((o_ptr->to_h + o_ptr->to_d) < 1) return (TRUE);
				else return (FALSE);
			}

			if (is_armour(o_ptr->tval))
			{
				if (o_ptr->to_a < 1) return (TRUE);
				else return (FALSE);
			}

			if (o_ptr->tval == TV_INSTRUMENT)
			{
				return (TRUE);
			}

			return (FALSE);
		}
	}

	/* Check for cursed stuff */
	else if (k_ptr->squeltch == DESTROY_CURSED)
	{
		/* Pseudo-ID */
		if (!known)
		{
			if (sense <= PSEUDO_ID_CURSED) return (TRUE);

			/* Hack -- Certain items are always generated cursed */
			if ((o_ptr->tval == TV_RING) || (o_ptr->tval == TV_AMULET))
			{
				if (k_ptr->aware &&
				    (k_ptr->flags3 & (TR3_CURSED | TR3_HEAVY_CURSE)))
				{
					return (TRUE);
				}
			}

			return (FALSE);
		}

		/* ID'ed */
		else
		{
			if (cursed_p(o_ptr)) return (TRUE);
			else return (FALSE);
		}
	}

	/* Shouldn't get here... */
	return (FALSE);
}


/* Check the floor for "crap" */
void squeltch_grid(void)
{
	s16b this_o_idx, next_o_idx = 0;

	char o_name[80];
	

	/* Scan the pile of objects */
	for (this_o_idx = cave[py][px].o_idx; this_o_idx; this_o_idx = next_o_idx)
	{
		/* Acquire object */
		object_type *o_ptr = &o_list[this_o_idx];

		/* We've now seen one of these */
		if (!k_info[o_ptr->k_idx].flavor) 
		{
			object_aware(o_ptr);
		} 

		/* Acquire next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Do we want it? */
		if (destroy_it(o_ptr))
		{
			/* Describe the object */
			object_desc(o_name, o_ptr, TRUE, 3);

			/* Print a message */
			msg_format("Auto-destroying %s.", o_name);

			/* Destroy the item */
			delete_object_idx(this_o_idx);

		}
	}
}

/* Check the inventory for "crap" */
void squeltch_inventory(void)
{
	int i;

	bool found = TRUE;
	
	while (found)
	{

		/* Sometimes an index in the inventory is skipped */
		found = FALSE;

		for (i=0;i<INVEN_PACK;i++)
		{
			object_type *o_ptr = &inventory[i];

			/* Do we want it? */
			if (destroy_it(o_ptr))
			{
				char o_name[80];

				/* Describe the object */
				object_desc(o_name, o_ptr, TRUE, 3);

				/* Print a message */
				msg_format("Auto-destroying %s.", o_name);
				
				/* Remove it */
				inven_item_increase(i, -o_ptr->number);
			
				inven_item_optimize(i);

				found=TRUE;
			}
		}
	}
}
