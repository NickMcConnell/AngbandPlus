#define WIZARD1_C
/* File: wizard1.c */

/* Purpose: Spoiler generation -BEN- */

#include "angband.h"


#ifdef ALLOW_SPOILERS


/*
 * The spoiler file being created
 */
static FILE *fff = NULL;



/*
 * Extract a textual representation of an attribute
 */
static cptr attr_to_text(byte a)
{
	if (/* a >= 0 && */ a < N_ELEMENTS(color_names))
		return color_names[a];
	else
		return "Icky";
}



/*
 * Item Spoilers by: benh@phial.com (Ben Harrison)
 */


/*
 * The basic items categorized by type
 */
static name_centry group_item[] =
{
	{ TV_SHOT,              "Ammo" },
	{ TV_ARROW,               NULL },
	{ TV_BOLT,                NULL },

	{ TV_BOW,               "Bows" },

	{ TV_SWORD,             "Weapons" },
	{ TV_POLEARM,     NULL },
	{ TV_HAFTED,      NULL },
	{ TV_DIGGING,     NULL },

	{ TV_SOFT_ARMOR,        "Armour (Body)" },
	{ TV_HARD_ARMOR,          NULL },
	{ TV_DRAG_ARMOR,          NULL },

	{ TV_CLOAK,             "Armour (Misc)" },
	{ TV_SHIELD,      NULL },
	{ TV_HELM,                NULL },
	{ TV_CROWN,               NULL },
	{ TV_GLOVES,      NULL },
	{ TV_BOOTS,               NULL },

	{ TV_AMULET,    "Amulets" },
	{ TV_RING,              "Rings" },

	{ TV_SCROLL,    "Scrolls" },
	{ TV_POTION,    "Potions" },
	{ TV_FOOD,              "Food" },

	{ TV_ROD,               "Rods" },
	{ TV_WAND,              "Wands" },
	{ TV_STAFF,             "Staffs" },

	{ TV_BOOK,    "Books" },

	{ TV_CHARM, "Charm" },

	{ TV_CHEST,             "Chests" },

	{ TV_SPIKE,             "Various" },
	{ TV_LITE,                NULL },
	{ TV_FLASK,               NULL },
	{ TV_JUNK,                NULL },
	{ TV_BOTTLE,      NULL },
	{ TV_SKELETON,    NULL },

	{ 0, "" }
};





/*
 * Describe the kind
 */
static void kind_info(char *buf, char *dam, char *wgt, int *lev, s32b *val,
	int k)
{
	object_type forge;
	object_type *q_ptr;

	object_kind *k_ptr;


	/* Get local object */
	q_ptr = &forge;

	/* Prepare a fake item */
	object_prep(q_ptr, k);

	/* Obtain the "kind" info */
	k_ptr = &k_info[q_ptr->k_idx];

	/* It is known */
	q_ptr->ident |= (IDENT_KNOWN);

	/* Cancel bonuses */
	q_ptr->pval = 0;
	q_ptr->to_a = 0;
	q_ptr->to_h = 0;
	q_ptr->to_d = 0;


	/* Level (is this appropriate?) */
	(*lev) = object_k_level(k_ptr);

	/* Value */
	(*val) = object_value(q_ptr, FALSE);


	/* Hack */
	if (!buf || !dam || !wgt) return;


	/* Description (too brief) */
	strnfmt(buf, ONAME_MAX, "%v", object_desc_f3, q_ptr, OD_SHOP, 0);


	/* Misc info */
	strcpy(dam, "");

	/* Damage */
	switch (q_ptr->tval)
	{
		/* Bows */
		case TV_BOW:
		{
			break;
		}

		/* Ammo */
		case TV_SHOT:
		case TV_BOLT:
		case TV_ARROW:
		{
			sprintf(dam, "%dd%d", q_ptr->dd, q_ptr->ds);
			break;
		}

		/* Weapons */
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_DIGGING:
		{
			sprintf(dam, "%dd%d", q_ptr->dd, q_ptr->ds);
			break;
		}

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
		{
			sprintf(dam, "%d", q_ptr->ac);
			break;
		}
	}


	/* Weight */
	sprintf(wgt, "%3d.%d", q_ptr->weight / 10, q_ptr->weight % 10);
}


/*
 * Open a given text file in the user directory for writing.
 */
static FILE *my_fopen_wiz(cptr fname)
{
	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Drop priv's */
	safe_setuid_drop();

	/* Build and open the filename with the standard name. */
	fff = my_fopen_path(ANGBAND_DIR_USER, fname, "w");

	/* Grab priv's */
	safe_setuid_grab();

	/* Warn of errors. */
	if (!fff) msg_print("Cannot create spoiler file.");

	return fff;
}

/*
 * Close fff, and report success/failure.
 */
static void my_fclose_wiz(void)
{
	if (ferror(fff) || my_fclose(fff))
	{
		msg_print("Cannot close spoiler file.");
	}
	else
	{
		msg_print("Successfully created a spoiler file.");
	}
}

/*
 * Create a spoiler file for items
 */
static void spoil_obj_desc(void)
{
	int i, k, s, t, n = 0;

	u16b who[200];

	C_TNEW(o_name, ONAME_MAX, char);

	char wgt[80];
	char dam[80];


	/* Header */
	fprintf(fff, "Spoiler File -- Basic Items (2.?.?)\n\n\n");

	/* More Header */
	fprintf(fff, "%-45s     %8s%7s%5s%9s\n",
		"Description", "Dam/AC", "Wgt", "Lev", "Cost");
	fprintf(fff, "%-45s     %8s%7s%5s%9s\n",
		"----------------------------------------",
		"------", "---", "---", "----");

	/* List the groups */
	for (i = 0; TRUE; i++)
	{
		/* Write out the group title */
		if (group_item[i].str)
		{
			/* Hack -- bubble-sort by cost and then level */
			for (s = 0; s < n - 1; s++)
			{
				for (t = 0; t < n - 1; t++)
				{
					int i1 = t;
					int i2 = t + 1;

					int e1;
					int e2;

					s32b t1;
					s32b t2;

					kind_info(NULL, NULL, NULL, &e1, &t1, who[i1]);
					kind_info(NULL, NULL, NULL, &e2, &t2, who[i2]);

					if ((t1 > t2) || ((t1 == t2) && (e1 > e2)))
					{
						int tmp = who[i1];
						who[i1] = who[i2];
						who[i2] = tmp;
					}
				}
			}

			/* Spoil each item */
			for (s = 0; s < n; s++)
			{
				int e;
				s32b v;

				/* Describe the kind */
				kind_info(o_name, dam, wgt, &e, &v, who[s]);

				/* Dump it */
				fprintf(fff, "     %-45s%8s%7s%5d%9ld\n",
					o_name, dam, wgt, e, (long)(v));
			}

			/* Start a new set */
			n = 0;

			/* Notice the end */
			if (!group_item[i].idx) break;

			/* Start a new set */
			fprintf(fff, "\n\n%s\n\n", group_item[i].str);
		}

		/* Acquire legal item types */
		for (k = 1; k < MAX_K_IDX; k++)
		{
			object_kind *k_ptr = &k_info[k];

			/* Skip wrong tval's */
			if (k_ptr->tval != group_item[i].idx) continue;

			/* Hack -- skip items which only have special generation methods. */
			if (!kind_created_p(k_ptr)) continue;

			/* Save the index */
			who[n++] = k;
		}
	}

	TFREE(o_name);
}


/*
 * Artifact Spoilers by: randy@PICARD.tamu.edu (Randy Hutson)
 * Largely rewritten now, though...
 */

/*
 * The artifacts categorized by type
 */
static name_centry group_artifact[] =
{
	{ TV_SWORD,             "Edged Weapons" },
	{ TV_POLEARM,   "Polearms" },
	{ TV_HAFTED,    "Hafted Weapons" },
	{ TV_BOW,               "Bows" },

	{ TV_SOFT_ARMOR,        "Body Armor" },
	{ TV_HARD_ARMOR,          NULL },
	{ TV_DRAG_ARMOR,          NULL },

	{ TV_CLOAK,             "Cloaks" },
	{ TV_SHIELD,    "Shields" },
	{ TV_HELM,              "Helms/Crowns" },
	{ TV_CROWN,               NULL },
	{ TV_GLOVES,    "Gloves" },
	{ TV_BOOTS,             "Boots" },

	{ TV_LITE,              "Light Sources" },
	{ TV_AMULET,    "Amulets" },
	{ TV_RING,              "Rings" },

	{ 0, NULL }
};



/*
 * These are used for "+3 to STR, DEX", etc. These are separate from
 * the other pval affected traits to simplify the case where an object
 * affects all stats.  In this case, "All stats" is used instead of
 * listing each stat individually.
 */

/*
 * Write out `n' of the character `c' to the spoiler file
 */
static void spoiler_out_n_chars(int n, char c)
{
	while (--n >= 0) fputc(c, fff);
}

/*
 * Write out `n' blank lines to the spoiler file
 */
static void spoiler_blanklines(int n)
{
	spoiler_out_n_chars(n, '\n');
}

/*
 * Write a line to the spoiler file and then "underline" it with hypens
 */
static void spoiler_underline(cptr str)
{
	fprintf(fff, "%s\n", str);
	spoiler_out_n_chars(strlen(str), '-');
	fprintf(fff, "\n");
}



static void print_header(void)
{
	char buf[80];

	sprintf(buf, "Artifact Spoilers for %s Version %s", GAME_NAME,
	GAME_VERSION);
	spoiler_underline(buf);

}

/*
 * Create a spoiler file for artifacts
 */
static void spoil_artifact(void)
{
	int i, j;

	object_type forge;
	object_type *q_ptr;

	const bool old_spoil_art = spoil_art;
	const bool old_spoil_base = spoil_base;
	const bool old_cheat_item = cheat_item;

	/* Use full spoilers, but no cheating. */
	spoil_art = spoil_base = TRUE;
	cheat_item = FALSE;

	/* Dump the header */
	print_header();

	/* List the artifacts by tval */
	for (i = 0; group_artifact[i].idx; i++)
	{
		/* Write out the group title */
		if (group_artifact[i].str)
		{
			spoiler_blanklines(2);
			spoiler_underline(group_artifact[i].str);
			spoiler_blanklines(1);
		}

		/* Now search through all of the artifacts */
		for (j = 1; j < MAX_A_IDX; ++j)
		{
			artifact_type *a_ptr = &a_info[j];

			/* We only want objects in the current group */
			if (k_info[a_ptr->k_idx].tval != group_artifact[i].idx) continue;

			/* Get local object */
			q_ptr = &forge;

			/* Wipe the object */
			object_wipe(q_ptr);

			/* Attempt to "forge" the artifact */
			if (!make_fake_artifact(q_ptr, j)) continue;

			/* Know most things about the object. */
			object_known(q_ptr);

			my_fprintf(fff, "%v\n", object_desc_f3, q_ptr, OD_ART | OD_SHOP, 1);

			/* Describe the artifact in a relatively brief way. */
			identify_fully_file(q_ptr, fff, TRUE);

			/* Provide some allocation data. */
			fprintf(fff, "     Level %d", a_ptr->level);

			if (a_ptr->level2 && a_ptr->level2 != a_ptr->level)
			{
				fprintf(fff, "/%d", a_ptr->level2);
			}

			fprintf(fff, ", Rarity %d, %d.%d lbs, %ld Gold\n", a_ptr->rarity,
				a_ptr->weight/10, a_ptr->weight%10, a_ptr->cost);
		}
	}

	/* Reset spoilers. */
	spoil_art = old_spoil_art;
	spoil_base = old_spoil_base;
	cheat_item = old_cheat_item;
}





/*
 * Create a spoiler file for monsters   -BEN-
 */
static void spoil_mon_desc(void)
{
	int i, n = 0;

	C_TNEW(who, MAX_R_IDX, s16b);

	char nam[80];
	char lev[80];
	char rar[80];
	char spd[80];
	char ac[80];
	char hp[80];
	char exp[80];


	/* Dump the header */

	fprintf(fff, "Monster Spoilers for %s Version %s\n", GAME_NAME,
		GAME_VERSION);
	fprintf(fff, "------------------------------------------\n\n");

	/* Dump the header */
	fprintf(fff, "%-40.40s%4s%4s%6s%8s%4s  %11.11s\n",
		"Name", "Lev", "Rar", "Spd", "Hp", "Ac", "Visual Info");
	fprintf(fff, "%-40.40s%4s%4s%6s%8s%4s  %11.11s\n",
		"----", "---", "---", "---", "--", "--", "-----------");

	/* Scan the monsters (except the ghost) */
	for (i = 1; i < MAX_R_IDX; i++)
	{
		monster_race *r_ptr = &r_info[i];

		/* Hack - skip "fake" monsters. */
		if (is_fake_monster(r_ptr)) continue;

		/* Use that monster */
		if (r_ptr->name) who[n++] = i;
	}


	/* Scan again */
	for (i = 0; i < n; i++)
	{
		monster_race *r_ptr = &r_info[who[i]];
		cptr pre;

		/* Get the "name" */
		if (r_ptr->flags1 & (RF1_GUARDIAN)) pre = "[G]";
		else if (r_ptr->flags1 & (RF1_UNIQUE)) pre = "[U]";
		else pre = "The";

		strnfmt(nam, N_ELEMENTS(nam), "%s %.*v", pre,
			N_ELEMENTS(nam)-strlen(pre)-1, monster_desc_aux_f3, r_ptr, 1, 0);

		/* Level */
		sprintf(lev, "%d", r_ptr->level);

		/* Rarity */
		sprintf(rar, "%d", r_ptr->rarity);

		/* Speed */
		if (r_ptr->speed >= 110)
		{
			sprintf(spd, "+%d", (r_ptr->speed - 110));
		}
		else
		{
			sprintf(spd, "-%d", (110 - r_ptr->speed));
		}

		/* Armor Class */
		sprintf(ac, "%d", r_ptr->ac);

		/* Hitpoints */
		if ((r_ptr->flags1 & (RF1_FORCE_MAXHP)) || (r_ptr->hside == 1))
		{
			sprintf(hp, "%d", r_ptr->hdice * r_ptr->hside);
		}
		else
		{
			sprintf(hp, "%dd%d", r_ptr->hdice, r_ptr->hside);
		}


		/* Power */
		sprintf(exp, "%ld", (long)(r_ptr->mexp));

		/* Hack -- use visual instead */
		sprintf(exp, "%s '%c'", attr_to_text(r_ptr->gfx.da), r_ptr->gfx.dc);

		/* Dump the info */
		fprintf(fff, "%-40.40s%4s%4s%6s%8s%4s  %11.11s\n",
			nam, lev, rar, spd, hp, ac, exp);
	}

	/* End it */
	fprintf(fff, "\n");

	/* Free the "who" array */
	TFREE(who);
}




/*
 * Monster spoilers by: smchorse@ringer.cs.utsa.edu (Shawn McHorse)
 *
 * Primarily based on code already in mon-desc.c, mostly by -BEN-
 */

/*
 * Buffer text to the given file. (-SHAWN-)
 * This is basically c_roff() from mon-desc.c with a few changes.
 */
static void spoil_out_aux(cptr str)
{
	cptr r;

	/* Line buffer */
	static char roff_buf[256];

	/* Current pointer into line roff_buf */
	static char *roff_p = roff_buf;

	/* Last space saved into roff_buf */
	static char *roff_s = NULL;

	/* Special handling for "new sequence" */
	if (!str)
	{
		if (roff_p != roff_buf) roff_p--;
		while (*roff_p == ' ' && roff_p != roff_buf) roff_p--;
		if (roff_p == roff_buf) fprintf(fff, "\n");
		else
		{
			*(roff_p + 1) = '\0';
			fprintf(fff, "%s\n\n", roff_buf);
		}
		roff_p = roff_buf;
		roff_s = NULL;
		roff_buf[0] = '\0';
		return;
	}

	/* Scan the given string, character at a time */
	for (; *str; str++)
	{
		char ch = *str;
		int wrap = (ch == '\n');

		if (!ISPRINT(ch)) ch = ' ';
		if (roff_p >= roff_buf + 75) wrap = 1;
		if ((ch == ' ') && (roff_p + 2 >= roff_buf + 75)) wrap = 1;

		/* Handle line-wrap */
		if (wrap)
		{
			*roff_p = '\0';
			r = roff_p;
			if (roff_s && (ch != ' '))
			{
				*roff_s = '\0';
				r = roff_s + 1;
			}
			fprintf(fff, "%s\n", roff_buf);
			roff_s = NULL;
			roff_p = roff_buf;
			while (*r) *roff_p++ = *r++;
		}

		/* Save the char */
		if ((roff_p > roff_buf) || (ch != ' '))
		{
			if (ch == ' ') roff_s = roff_p;
			*roff_p++ = ch;
		}
	}
}

/*
 * Use a format string with spoil_out().
 */
static void spoil_out(cptr fmt, ...)
{
	if (fmt)
	{
		char buf[1024];
		va_list vp;
		va_start(vp, fmt);
		vstrnfmt(buf, sizeof(buf), fmt, vp);
		va_end(vp);

		spoil_out_aux(buf);
	}
	else
	{
		spoil_out_aux(fmt);
	}
}

/*
 * Create a spoiler file for monsters (-SHAWN-)
 */
static void spoil_mon_info(void)
{
	int n, x, y;
	bool breath, magic;
	u32b flags1, flags2, flags3, flags4, flags5, flags6;

	const bool old_spoil_mon = spoil_mon;

	byte a;
	char c, c2;
	byte old_moncol[MAX_MONCOL];

	/* Give full information. */
	spoil_mon = TRUE;

	for (n = 0; n < MAX_MONCOL; n++) old_moncol[n] = moncol[n].gfx.xa;

	/* Hack - hide some information. */
	moncol[0].gfx.xa = moncol[8].gfx.xa = moncol[18].gfx.xa = TERM_DARK;

	/* Dump the header */
	spoil_out("Monster Spoilers for %s Version %s\n",
		GAME_NAME, GAME_VERSION);
	spoil_out("------------------------------------------\n\n");

	/*
	 * List all monsters in order (except the ghost).
	 */
	for (n = 1; n < MAX_R_IDX; n++)
	{

		monster_race *r_ptr = &r_info[n];

		/* Skip "fake" monsters. */
		if (is_fake_monster(r_ptr)) continue;

		/* Extract the flags */
		flags1 = r_ptr->flags1;
		flags2 = r_ptr->flags2;
		flags3 = r_ptr->flags3;
		flags4 = r_ptr->flags4;
		flags5 = r_ptr->flags5;
		flags6 = r_ptr->flags6;
		breath = FALSE;
		magic = FALSE;


		/* Prefix */
		if (flags1 & (RF1_GUARDIAN))
		{
			spoil_out("[G] ");
		}
		else if (flags1 & (RF1_UNIQUE))
		{
			spoil_out("[U] ");
		}
		else
		{
			spoil_out("The ");
		}

		/* Name */
		spoil_out("%v  (", monster_desc_aux_f3, r_ptr, 1, 0);

		/* Color */
		spoil_out(attr_to_text(r_ptr->gfx.da));

		/* Symbol --(-- */
		spoil_out(" '%c')\n", r_ptr->gfx.dc);


		/* Indent */
		spoil_out("=== ");

		/* Number */
		spoil_out("Num:%d  ", n);

		/* Level */
		spoil_out("Lev:%d  ", r_ptr->level);

		/* Rarity */
		spoil_out("Rar:%d  ", r_ptr->rarity);

		/* Speed */
		spoil_out("Spd:%+d  ", (r_ptr->speed - 110));

		/* Hitpoints */
		if ((flags1 & (RF1_FORCE_MAXHP)) || (r_ptr->hside == 1))
		{
			spoil_out("Hp:%d  ", r_ptr->hdice * r_ptr->hside);
		}
		else
		{
			spoil_out("Hp:%dd%d  ", r_ptr->hdice, r_ptr->hside);
		}

		/* Armor Class */
		spoil_out("Ac:%d  ", r_ptr->ac);

		/* Power */
		spoil_out("Power:%ld\n", (long)(r_ptr->mexp));

		/* Clear the screen before every monster. */
		Term_clear();

		/* Display the monster on screen. */
		screen_roff(n);

		/* Dump the on-screen display (excluding the title). */
		for (c2 = 0, y = 1; y < Term->hgt; y++)
		{
			for (x = 0; x < Term->wid; x++)
			{
				/* Check the character. */
				Term_what(x, y, &a, &c);

				/* Ignore blanked text. */
				if (a == TERM_DARK) continue;

				/* Ignore repeated spaces. */
				if (c == ' ' && c2 == ' ') continue;

				/* Dump the character. */
				spoil_out("%c", c);

				/* Remember the character. */
				c2 = c;
			}

			/* Put a space at the end of every line. */
			if (c2 != ' ') spoil_out(" ");
		}

		spoil_out(NULL);
	}

	/* Restore spoil_mon. */
	spoil_mon = old_spoil_mon;

	/* Restore moncol[]. */
	for (n = 0; n < MAX_MONCOL; n++) moncol[n].gfx.xa = old_moncol[n];

	/* Don't leave a monster display lying around. */
	Term_clear();
}

typedef const struct option_list option_list;
struct option_list
{
	cptr title;
	cptr fname;
	void (*func)(void);
	char ch;
	byte x;
	byte y;
};

static option_list spoiler_list[] =
{
	{"Brief Object Info", "obj-desc.spo", spoil_obj_desc, '1', 5, 5},
	{"Brief Artifact Info", "artifact.spo", spoil_artifact, '2', 6, 5},
	{"Brief Monster Info", "mon-desc.spo", spoil_mon_desc, '3', 7, 5},
	{"Full Monster Info", "mon-info.spo", spoil_mon_info, '4', 8, 5},
};

/*
 * Create Spoiler files         -BEN-
 */
void do_cmd_spoilers(void)
{
	int i;
	
	option_list *this;

	/* Enter "icky" mode */
	character_icky = TRUE;

	/* Save the screen */
	Term_save();

	/* Interact */
	while (1)
	{
		/* Clear screen */
		Term_clear();

		/* Info */
		mc_put_fmt(2, 0, "Create a spoiler file.");

		FOR_ALL_IN(spoiler_list, this)
		{
			mc_put_fmt(this->y, this->x, "(%c) %s (%s)",
				this->ch, this->title, this->fname);
		}

		/* Prompt */
		mc_put_fmt(12, 0, "Command: ");

		/* Get a choice */
		i = inkey();

		/* Escape */
		if (i == ESCAPE) break;

		FOR_ALL_IN(spoiler_list, this)
		{
			if (i == this->ch) goto good;
		}

		/* Hack - the above loop has two exit points. */
		if (FALSE)
		{
good:
			/* Output the spoilers to the appropriate file. */
			if (my_fopen_wiz(this->fname))
			{
				(*this->func)();
				my_fclose_wiz();
			}
		}
		else
		{
			/* None found. */
			bell("Illegal command for spoilers!");
		}

		/* Flush messages */
		msg_print(NULL);
	}

	/* Restore the screen */
	Term_load();

	/* Leave "icky" mode */
	character_icky = FALSE;
}


#else

#ifdef MACINTOSH
static int i = 0;
#endif

#endif


