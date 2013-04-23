#define MONSTER1_C
/* File: mon-desc.c */

/* Purpose: describe monsters (using monster memory) */

/*
 * Copyright (c) 1989 James E. Wilson, Christopher J. Stuart
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"


/*
 * Pronoun arrays, by gender.
 */
static cptr wd_he[3] =
{ "it", "he", "she" };
static cptr wd_his[3] =
{ "its", "his", "her" };


/*
 * Pluralizer.  Args(count, singular, plural)
 */
#define plural(c,s,p) \
	(((c) == 1) ? (s) : (p))






/*
 * Determine if the "armor" is known
 * The higher the level, the fewer kills needed.
 */
static bool know_armour(monster_race *r_ptr)
{
	s32b level = r_ptr->level;

	s32b kills = r_ptr->r_tkills;

	/* Normal monsters */
	if (kills > 304 / (4 + level)) return (TRUE);

	/* Skip non-uniques */
	if (!(r_ptr->flags1 & (RF1_UNIQUE))) return (FALSE);

	/* Unique monsters */
	if (kills > 304 / (38 + (5*level) / 4)) return (TRUE);

	/* Assume false */
	return (FALSE);
}


/*
 * Determine if the "damage" of the given attack is known
 * the higher the level of the monster, the fewer the attacks you need,
 * the more damage an attack does, the more attacks you need
 */
static bool know_damage(int r_idx, int i)
{
	monster_race *r_ptr = &r_info[r_idx];

	s32b level = r_ptr->level;

	s32b a = r_ptr->r_blows[i];

	s32b d1 = r_ptr->blow[i].d_dice;
	s32b d2 = r_ptr->blow[i].d_side;

	s32b d = d1 * d2;

	/* Normal monsters */
	if ((4 + level) * a > 80 * d) return (TRUE);

	/* Skip non-uniques */
	if (!(r_ptr->flags1 & (RF1_UNIQUE))) return (FALSE);

	/* Unique monsters */
	if ((4 + level) * (2 * a) > 80 * d) return (TRUE);

	/* Assume false */
	return (FALSE);
}


/* Colourful monster descriptions to make the important points more obvious. */
#define MONCOL_DEATH (moncol[0].gfx.xa) /* How many times you've fought to the death */
#define MONCOL_FLAVOUR (moncol[1].gfx.xa) /* The flavour text from r_info.txt */
#define MONCOL_DEPTH (moncol[2].gfx.xa) /* Normal depth and speed */
#define MONCOL_AURA (moncol[3].gfx.xa) /* Any defensive auras it may have */
#define MONCOL_ESCORT (moncol[4].gfx.xa) /* Any escort it may have */
#define MONCOL_INATE (moncol[5].gfx.xa) /* Any inate attacks it may have */
#define MONCOL_BREATH (moncol[6].gfx.xa) /* Any breath attacks it may have */
#define MONCOL_MAGIC (moncol[7].gfx.xa) /* Any magical attacks it may have */
#define MONCOL_ACHP (moncol[8].gfx.xa) /* The AC and HP of the monster */
#define MONCOL_ABLE1 (moncol[9].gfx.xa) /* Bashing down doors, destroying items, etc. */
#define MONCOL_ABLE2 (moncol[10].gfx.xa) /* Breeding explosively, invisibility, etc. */
#define MONCOL_WEAK (moncol[11].gfx.xa) /* Susceptibilities to specific attacks */
#define MONCOL_ELEM (moncol[12].gfx.xa) /* Elemental resistances */
#define MONCOL_RESIST (moncol[13].gfx.xa) /* Other resistances */
#define MONCOL_IMMUN (moncol[14].gfx.xa) /* Immunity to stunning, fear, confusion or sleep */
#define MONCOL_OBSERVE (moncol[15].gfx.xa) /* How observant it is */
#define MONCOL_DROP (moncol[16].gfx.xa) /* What it can drop */
#define MONCOL_ATTACK (moncol[17].gfx.xa) /* What melee attacks it has */
#define MONCOL_QUEST (moncol[18].gfx.xa) /* If it is a quest monster */

/* "will" if always true and omniscient, "may" otherwise. */
#define DDE_MAY ((omniscient && d_ptr->num >= d_ptr->denom) ? "will" : "may")

/*
 * Display information about death events
 */
static cptr describe_death_events(int r_idx, cptr he, bool omniscient)
{
	u16b j;
	s16b start = -1, end = -1;
	cptr s = format("When %s dies, ", he);

	/* First count the interesting events */
	for (j = 0; j < MAX_DEATH_EVENTS; j++)
	{
		death_event_type *d_ptr = &death_event[j];

		/* Ignore incorrect entries */
		if (d_ptr->r_idx > r_idx) break;
		if (!d_ptr->r_idx) break;
		if (d_ptr->r_idx < r_idx) continue;

		/* Ignore unknown entries */
		if (!omniscient && ~d_ptr->flags & EF_KNOWN) continue;

		/* Ignore DEATH_NOTHING entries */
		if (d_ptr->type == DEATH_NOTHING) continue;

		/* Locate the first interesting entry */
		if (start == -1) start = j;

		/* Keep looking for the last interesting entry */
		end = j;
	}

	/* Nothing to do. */
	if (end == -1) return "";

	/* Then loop through them. There may be unknown ones in the range, but no incorrect ones. */
	for (j = start; j <= end; j++)
	{
		death_event_type *d_ptr = &death_event[j];

		/* Ignore unknown entries */
		if (!omniscient && ~d_ptr->flags & EF_KNOWN) continue;

		/* Ignore DEATH_NOTHING entries */
		if (d_ptr->type == DEATH_NOTHING) continue;

		/* Prepare to finish the string */
		if (j != start && j == end)
			s = format("%sand ", s);

		switch (d_ptr->type)
		{
			case DEATH_OBJECT:
			{
				make_item_type *i_ptr = &(d_ptr->par.item);
				object_type o, *o_ptr = &o;
				object_prep(o_ptr, i_ptr->k_idx);
				if (i_ptr->max > 1) o_ptr->number = UNKNOWN_OBJECT_NUMBER;
				if (i_ptr->flags & EI_ART)
					o_ptr->name1 = i_ptr->x_idx;
				if (i_ptr->flags & EI_EGO)
					o_ptr->name2 = i_ptr->x_idx;
				s = format("%s%s %s drop %v", s, he, DDE_MAY,
					object_desc_f3, o_ptr, OD_ART | OD_SHOP, 0);
				break;
			}
			case DEATH_MONSTER:
			{
				make_monster_type *i_ptr = &d_ptr->par.monster;
				monster_race *r_ptr = &r_info[i_ptr->num];
				s = format("%s%v %s be created", s, monster_desc_aux_f3, r_ptr,
					i_ptr->max, MDF_INDEF, DDE_MAY);
				break;
			}
			case DEATH_EXPLODE:
			{
				make_explosion_type *i_ptr = &d_ptr->par.explosion;
				s = format("%s%s %s explode in a ball of %s of radius %d", s,
					he, DDE_MAY, lookup_gf(i_ptr->method)->desc, i_ptr->radius);
				break;
			}
			case DEATH_COIN:
			{
				make_coin_type *i_ptr = &d_ptr->par.coin;
				char coin[80];
				int i;
				for (i = 0; ((coin[i] = FORCELOWER(coin_types[i_ptr->metal][i]))) != '\0'; i++);
				s = format("%s%s %s only drop %s coins", s, he, DDE_MAY, coin);
				break;
			}
			case DEATH_NOTHING: /* But nothing happens. */
			break;
			default: /* Shouldn't get here, but... */
			if (alert_failure)
				msg_format("Strange death event %d encountered.", d_ptr->type);
		}
		if (j == end)
			s = format("%s. ", s);
		else
			s = format("%s, ", s);
	}
	return s;
}

typedef struct roff_monster_type roff_monster_type;

struct roff_monster_type
{
	int set;
	u32b value;
	cptr adjective;
	cptr noun;
};

/*
 * Return the type of monster something is (e.g. an undead dragon) on demand.
 * This doesn't allow conditional colour codes, but could if c_roff() had
 * escape sequences for them. It doesn't print anything directly as it doesn't
 * know what colour everything should be by default.
 */
static cptr roff_monster(u32b flags2, u32b flags3)
{
	roff_monster_type cats[] =
	{
		{2, RF2_ELDRITCH_HORROR, "sanity-blasting", "eldritch horror"},
		{3, RF3_ANIMAL, "natural", "animal"},
		{3, RF3_EVIL, "evil", "evil monster"},
		{3, RF3_GOOD, "good", "good creature"},
		{3, RF3_UNDEAD, "undead", "undead thing"},
		{2, RF2_PHANTOM, "phantasmal", "phantom"},
		{3, RF3_DRAGON, "draconic", "dragon"},
		{3, RF3_DEMON, "demoniac", "demon"},
		{3, RF3_CTHULOID, "Cthuloid", "Cthuloid entity"},
		{3, RF3_GIANT, "gigantic", "giant"},
		{3, RF3_TROLL, "trollish", "troll"},
		{3, RF3_ORC, "orcish", "orc"},
		{2, RF2_ELEMENTAL, "elemental", "elemental spirit"},
		{2, RF2_MIMIC, "mimicking", "mimic"},
/* {2, RF2_CULTIST, "cultist", "cultist"}, */ /* Too obscure */
/* {3, RF2_SHAMAN, "shamanist", "shaman"}, */ /* Too obscure */
		{3, RF3_GREAT_OLD_ONE, "Great Old One", "Great Old One"},
	};

	u32b flags[4];

	uint i, j;

	/* Put the input in the flags array for convenience. */
	flags[2] = flags2;
	flags[3] = flags3;

	/* Count the number of applicable flags. */
	for (i = j = 0; i < N_ELEMENTS(cats); i++)
	{
		if (flags[cats[i].set] & cats[i].value) j++;
	}

	if (!j)
	{
		/* No applicable categories, so use something simple. */
		return "This creature";
	}
	else
	{
		cptr out = "This";

		/* Write out the strings, putting the last as a noun. */
		for (i = 0; j; i++)
		{
			if (flags[cats[i].set] & cats[i].value)
				out = format("%s %s", out,
					(--j) ? cats[i].adjective : cats[i].noun);
		}
		return out;
	}
}



/*
 * Hack - extract and interpret any damage string in a monster spell
 * description for a given monster.
 * This does not consider what the actual spell does, only what it says it
 * does below.
 */
static cptr convert_spell_text(cptr string, monster_race *r_ptr)
{
	cptr start = strchr(string, '(');

	assert(start != format(NULL)); /* Caller */

	/* Numerical strings are always preceded by ( to simplify spoil_flag. */
	if (!start) return string;

	/* !spoil_flag hides the numerical information. */
	if (!spoil_flag)
	{
		/* Assume that there is nothing to display after the numerical term. */
		return format("%.*s", start-string-1, string);
	}

	/* Is there a LEV term to evaluate? */
	if ((r_ptr->r_tkills || spoil_mon) && strstr(start, "LEV"))
	{
		string = format("%v", evaluate_text_f3, string, "LEV", r_ptr->level);
	}
	
	/* Is there a MHP term to evaluate? */
	if ((know_armour(r_ptr) || spoil_mon) && strstr(start, "MHP"))
	{
		string = format("%v", evaluate_text_f3, string, "MHP",
			r_ptr->hdice * r_ptr->hside);
	}

	return string;
}


/*
 * Hack -- display monster information using "roff()"
 *
 * This function should only be called with the cursor placed at the
 * left edge of the screen, on a cleared line, in which the recall is
 * to take place.  One extra blank line is left after the recall.
 */
static void roff_aux(int r_idx)
{
	monster_race *r_ptr;

	bool old = FALSE;
	bool sin = FALSE;

	int m, n, r;

	cptr p, q;

	int msex = 0;

	bool breath = FALSE;
	bool magic = FALSE;

	u32b flags1;
	u32b flags2;
	u32b flags3;
	u32b flags4;
	u32b flags5;
	u32b flags6;

	int vn = 0;
	cptr vp[64];

	monster_race        save_mem;



	/* Access the race and lore */
	r_ptr = &r_info[r_idx];


	/* Cheat -- Know everything */
	if (spoil_mon)
	{
		/* XXX XXX XXX */

		/* Save the "old" memory */
		save_mem = *r_ptr;

		/* Hack -- Maximal info */
		r_ptr->r_wake = r_ptr->r_ignore = MAX_UCHAR;

		/* Observe "maximal" attacks */
		for (m = 0; m < 4; m++)
		{
			/* Examine "actual" blows */
			if (r_ptr->blow[m].effect || r_ptr->blow[m].method)
			{
				/* Hack -- maximal observations */
				r_ptr->r_blows[m] = MAX_UCHAR;
			}
		}

		/* Hack -- maximal drops */
		r_ptr->r_drop_gold = r_ptr->r_drop_item =
		(((r_ptr->flags1 & (RF1_DROP_4D2)) ? 8 : 0) +
		((r_ptr->flags1 & (RF1_DROP_3D2)) ? 6 : 0) +
		((r_ptr->flags1 & (RF1_DROP_2D2)) ? 4 : 0) +
		((r_ptr->flags1 & (RF1_DROP_1D2)) ? 2 : 0) +
		((r_ptr->flags1 & (RF1_DROP_90))  ? 1 : 0) +
		((r_ptr->flags1 & (RF1_DROP_60))  ? 1 : 0));

		/* Hack -- but only "valid" drops */
		if (r_ptr->flags1 & (RF1_ONLY_GOLD)) r_ptr->r_drop_item = 0;
		if (r_ptr->flags1 & (RF1_ONLY_ITEM)) r_ptr->r_drop_gold = 0;

		/* Hack -- observe many spells */
		r_ptr->r_cast_inate = MAX_UCHAR;
		r_ptr->r_cast_spell = MAX_UCHAR;

		/* Hack -- know all the flags */
		r_ptr->r_flags1 = r_ptr->flags1;
		r_ptr->r_flags2 = r_ptr->flags2;
		r_ptr->r_flags3 = r_ptr->flags3;
		r_ptr->r_flags4 = r_ptr->flags4;
		r_ptr->r_flags5 = r_ptr->flags5;
		r_ptr->r_flags6 = r_ptr->flags6;
	}


	/* Extract a gender (if applicable) */
	if (r_ptr->flags1 & (RF1_FEMALE)) msex = 2;
	else if (r_ptr->flags1 & (RF1_MALE)) msex = 1;


	/* Obtain a copy of the "known" flags */
	flags1 = (r_ptr->flags1 & r_ptr->r_flags1);
	flags2 = (r_ptr->flags2 & r_ptr->r_flags2);
	flags3 = (r_ptr->flags3 & r_ptr->r_flags3);
	flags4 = (r_ptr->flags4 & r_ptr->r_flags4);
	flags5 = (r_ptr->flags5 & r_ptr->r_flags5);
	flags6 = (r_ptr->flags6 & r_ptr->r_flags6);


	/* Assume some "obvious" flags */
	if (r_ptr->flags1 & (RF1_UNIQUE)) flags1 |= (RF1_UNIQUE);
	if (r_ptr->flags1 & RF1_GUARDIAN) flags1 |= (RF1_GUARDIAN);
	if (r_ptr->flags1 & (RF1_MALE)) flags1 |= (RF1_MALE);
	if (r_ptr->flags1 & (RF1_FEMALE)) flags1 |= (RF1_FEMALE);

	/* Assume some "creation" flags */
	if (r_ptr->flags1 & (RF1_FRIENDS)) flags1 |= (RF1_FRIENDS);
	if (r_ptr->flags1 & (RF1_ESCORT)) flags1 |= (RF1_ESCORT);
	if (r_ptr->flags1 & (RF1_ESCORTS)) flags1 |= (RF1_ESCORTS);

	/* Killing a monster reveals some properties */
	if (r_ptr->r_tkills || spoil_mon)
	{
		/* Know "race" flags */
		if (r_ptr->flags3 & (RF3_ORC)) flags3 |= (RF3_ORC);
		if (r_ptr->flags3 & (RF3_TROLL)) flags3 |= (RF3_TROLL);
		if (r_ptr->flags3 & (RF3_GIANT)) flags3 |= (RF3_GIANT);
		if (r_ptr->flags3 & (RF3_DRAGON)) flags3 |= (RF3_DRAGON);
		if (r_ptr->flags3 & (RF3_DEMON)) flags3 |= (RF3_DEMON);
		if (r_ptr->flags3 & (RF3_CTHULOID)) flags3 |= (RF3_CTHULOID);
		if (r_ptr->flags3 & (RF3_UNDEAD)) flags3 |= (RF3_UNDEAD);
		if (r_ptr->flags3 & (RF3_EVIL)) flags3 |= (RF3_EVIL);
		if (r_ptr->flags3 & (RF3_GOOD)) flags3 |= (RF3_GOOD);
		if (r_ptr->flags3 & (RF3_ANIMAL)) flags3 |= (RF3_ANIMAL);
		if (r_ptr->flags3 & (RF3_GREAT_OLD_ONE)) flags3 |= (RF3_GREAT_OLD_ONE);

		/* Know "forced" flags */
		if (r_ptr->flags1 & (RF1_FORCE_MAXHP)) flags1 |= (RF1_FORCE_MAXHP);
	}


	/* Require a flag to show kills */
	if (!(show_details))
	{
		/* nothing */
	}

	/* Treat uniques differently */
	else if (flags1 & (RF1_UNIQUE))
	{
		/* Hack -- Determine if the unique is "dead" */
		bool dead = (r_ptr->max_num == 0) ? TRUE : FALSE;

		/* We've been killed... */
		if (r_ptr->r_deaths)
		{
			/* Killed ancestors */
			c_roff(MONCOL_DEATH, format("%^s has slain %d of your ancestors",
						wd_he[msex], r_ptr->r_deaths));

			/* But we've also killed it */
			if (dead)
			{
				c_roff(MONCOL_DEATH, format(", but you have avenged %s!  ",
							plural(r_ptr->r_deaths, "him", "them")));
			}

			/* Unavenged (ever) */
			else
			{
				c_roff(MONCOL_DEATH, format(", who %s unavenged.  ",
							plural(r_ptr->r_deaths, "remains", "remain")));
			}
		}

		/* Dead unique who never hurt us */
		else if (dead)
		{
			c_roff(MONCOL_DEATH, "You have slain this foe.  ");
		}
	}

	/* Not unique, but killed us */
	else if (r_ptr->r_deaths)
	{
		/* Dead ancestors */
		c_roff(MONCOL_DEATH, format("%d of your ancestors %s been killed by this creature, ",
					r_ptr->r_deaths, plural(r_ptr->r_deaths, "has", "have")));

		/* Some kills this life */
		if (r_ptr->r_pkills)
		{
			c_roff(MONCOL_DEATH, format("and you have exterminated at least %d of the creatures.  ",
						r_ptr->r_pkills));
		}

		/* Some kills past lives */
		else if (r_ptr->r_tkills)
		{
			c_roff(MONCOL_DEATH, format("and %s have exterminated at least %d of the creatures.  ",
						"your ancestors", r_ptr->r_tkills));
		}

		/* No kills */
		else
		{
			c_roff(MONCOL_DEATH, format("and %s is not ever known to have been defeated.  ",
						wd_he[msex]));
		}
	}

	/* Normal monsters */
	else
	{
		/* Killed some this life */
		if (r_ptr->r_pkills)
		{
			c_roff(MONCOL_DEATH, format("You have killed at least %d of these creatures.  ",
						r_ptr->r_pkills));
		}

		/* Killed some last life */
		else if (r_ptr->r_tkills)
		{
			c_roff(MONCOL_DEATH, format("Your ancestors have killed at least %d of these creatures.  ",
						r_ptr->r_tkills));
		}

		/* Killed none */
		else
		{
			c_roff(MONCOL_DEATH, "No battles to the death are recalled.  ");
		}
	}


	/* Descriptions */
	if (show_details)
	{
		char buf[2048];

		/* Simple method */
		strcpy(buf, r_text + r_ptr->text);

		/* Dump it */
		c_roff(MONCOL_FLAVOUR, buf);
		c_roff(MONCOL_FLAVOUR, "  ");
	}


	/* Nothing yet */
	old = FALSE;

	/* Describe location */
	if (r_ptr->level == 0)
	{
		c_roff(MONCOL_DEPTH, format("%^s lives in the town", roff_monster(flags2, flags3)));
		old = TRUE;
	}
	else if (r_ptr->r_tkills || spoil_mon)
	{
		bool force = ((flags1 & RF1_GUARDIAN) && (flags1 & RF1_UNIQUE));
		s16b depth = r_ptr->level;
		cptr when, pre, post;
		if (force)
		{
			int i;
			for (i = 0; i < MAX_Q_IDX; i++)
			{
				quest_type *q_ptr = &q_list[i];
				if (q_ptr->r_idx == r_ptr-r_info)
				{
					depth = q_ptr->level+dun_defs[q_ptr->dungeon].offset;
					break;
				}
			}
			/* A quest monster without a quest?! */
			if (i == MAX_Q_IDX) force = FALSE;
		}
		if (force)
		{
			when = "always";
		}
		else
		{
			when = "normally";
		}
		if (depth_in_feet)
		{
			pre = "at depths of ";
			post = " feet";
			depth *= 50;
		}
		else
		{
			pre = "on dungeon level ";
			post = "";
		}
		c_roff(MONCOL_DEPTH, format("%^s is %s found %s%d%s", roff_monster(flags2, flags3), when, pre, depth, post));
		old = TRUE;
	}


	/* Describe movement */
	if (TRUE)
	{
		/* Introduction */
		if (old)
		{
			c_roff(MONCOL_DEPTH, ", and ");
		}
		else
		{
			c_roff(MONCOL_DEPTH, format("%^s ", roff_monster(flags2, flags3)));
			old = TRUE;
		}
		c_roff(MONCOL_DEPTH, "moves");

		/* Random-ness */
		if ((flags1 & (RF1_RAND_50)) || (flags1 & (RF1_RAND_25)))
		{
			/* Adverb */
			if ((flags1 & (RF1_RAND_50)) && (flags1 & (RF1_RAND_25)))
			{
				c_roff(MONCOL_DEPTH, " extremely");
			}
			else if (flags1 & (RF1_RAND_50))
			{
				c_roff(MONCOL_DEPTH, " somewhat");
			}
			else if (flags1 & (RF1_RAND_25))
			{
				c_roff(MONCOL_DEPTH, " a bit");
			}

			/* Adjective */
			c_roff(MONCOL_DEPTH, " erratically");

			/* Hack -- Occasional conjunction */
			if (r_ptr->speed != 110) c_roff(MONCOL_DEPTH, ", and");
		}

		/* Speed */
		if (r_ptr->speed > 110)
		{
			if (r_ptr->speed > 130) c_roff(MONCOL_DEPTH, " incredibly");
			else if (r_ptr->speed > 120) c_roff(MONCOL_DEPTH, " very");
			c_roff(MONCOL_DEPTH, " quickly");
		}
		else if (r_ptr->speed < 110)
		{
			if (r_ptr->speed < 90) c_roff(MONCOL_DEPTH, " incredibly");
			else if (r_ptr->speed < 100) c_roff(MONCOL_DEPTH, " very");
			c_roff(MONCOL_DEPTH, " slowly");
		}
		else
		{
			c_roff(MONCOL_DEPTH, " at normal speed");
		}

		/* Also give as energy. */
		c_roff(MONCOL_DEPTH, format(" (%d energy/move, %d energy/attack)", extract_energy[r_ptr->speed], TURN_ENERGY/r_ptr->num_blows));
	}

	/* The code above includes "attack speed" */
	if (flags1 & (RF1_NEVER_MOVE))
	{
		/* Introduce */
		if (old)
		{
			c_roff(MONCOL_DEPTH, ", but ");
		}
		else
		{
			c_roff(MONCOL_DEPTH, format("%^s ", roff_monster(flags2, flags3)));
			old = TRUE;
		}

		/* Describe */
		c_roff(MONCOL_DEPTH, "does not deign to chase intruders");
	}

	/* End this sentence */
	if (old)
	{
		c_roff(MONCOL_DEPTH, ".  ");
		old = FALSE;
	}



	if ((flags2 & (RF2_AURA_FIRE)) && (flags2 & (RF2_AURA_ELEC)))
	{
		c_roff(MONCOL_AURA, format("%^s is surrounded by flames and electricity.  ", wd_he[msex]));
	}
	else if (flags2 & (RF2_AURA_FIRE))
	{
		c_roff(MONCOL_AURA, format("%^s is surrounded by flames.  ", wd_he[msex]));
	}
	else if (flags2 & (RF2_AURA_ELEC))
	{
		c_roff(MONCOL_AURA, format("%^s is surrounded by electricity.  ", wd_he[msex]));
	}

	if (flags2 & (RF2_REFLECTING))
	{
		c_roff(MONCOL_AURA, format("%^s reflects bolt spells.  ", wd_he[msex]));
	}

	if (flags2 & (RF2_RUN_AWAY))
	{
		c_roff(MONCOL_AURA, format("%^s runs away after attacking.  ", wd_he[msex]));
	}


	/* Describe escorts */
	if ((flags1 & (RF1_ESCORT)) || (flags1 & (RF1_ESCORTS)))
	{
		c_roff(MONCOL_ESCORT, format("%^s usually appears with escorts.  ",
					wd_he[msex]));
	}

	/* Describe friends */
	else if (flags1 & (RF1_FRIENDS))
	{
		c_roff(MONCOL_ESCORT, format("%^s usually appears in groups.  ",
					wd_he[msex]));
	}


	/* Collect inate attacks */
	vn = 0;
	if (flags4 & (RF4_SHRIEK)) vp[vn++] = "shriek for help";
	if (flags4 & (RF4_XXX3)) vp[vn++] = "do something";
	if (flags4 & (RF4_BA_SHARD))      vp[vn++] = "produce shard balls";
	if (flags4 & (RF4_ARROW_1)) vp[vn++] = "fire an arrow (1d6)";
	if (flags4 & (RF4_ARROW_2)) vp[vn++] = "fire arrows (3d6)";
	if (flags4 & (RF4_ARROW_3)) vp[vn++] = "fire a missile (5d6)";
	if (flags4 & (RF4_ARROW_4)) vp[vn++] = "fire missiles (7d6)";

	/* Describe inate attacks */
	if (vn)
	{
		/* Intro */
		c_roff(MONCOL_INATE, format("%^s", wd_he[msex]));

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) c_roff(MONCOL_INATE, " may ");
			else if (n < vn-1) c_roff(MONCOL_INATE, ", ");
			else c_roff(MONCOL_INATE," or ");

			/* Dump */
			c_roff(MONCOL_INATE,vp[n]);
		}

		/* End */
		c_roff(MONCOL_INATE, ".  ");
	}


	/* Collect breaths */
	vn = 0;
	if (flags4 & (RF4_BR_ACID)) vp[vn++] = "acid (up to MHP/3<1600;)";
	if (flags4 & (RF4_BR_ELEC)) vp[vn++] = "lightning (up to MHP/3<1600;)";
	if (flags4 & (RF4_BR_FIRE)) vp[vn++] = "fire (up to MHP/3<1600;)";
	if (flags4 & (RF4_BR_COLD)) vp[vn++] = "frost (up to MHP/3<1600;)";
	if (flags4 & (RF4_BR_POIS)) vp[vn++] = "poison (up to MHP/3<800;)";
	if (flags4 & (RF4_BR_NETH)) vp[vn++] = "nether (up to MHP/6<550;)";
	if (flags4 & (RF4_BR_LITE)) vp[vn++] = "light (up to MHP/6<400;)";
	if (flags4 & (RF4_BR_DARK)) vp[vn++] = "darkness (up to MHP/6<400;)";
	if (flags4 & (RF4_BR_CONF)) vp[vn++] = "confusion (up to MHP/6<400;)";
	if (flags4 & (RF4_BR_SOUN)) vp[vn++] = "sound (up to MHP/6<400;)";
	if (flags4 & (RF4_BR_CHAO)) vp[vn++] = "chaos (up to MHP/6<600;)";
	if (flags4 & (RF4_BR_DISE)) vp[vn++] = "disenchantment (up to MHP/6<500;)";
	if (flags4 & (RF4_BR_NEXU)) vp[vn++] = "nexus (up to MHP/3<250;)";
	if (flags4 & (RF4_BR_TIME)) vp[vn++] = "time (up to MHP/3<150;)";
	if (flags4 & (RF4_BR_INER)) vp[vn++] = "inertia (up to MHP/6<200;)";
	if (flags4 & (RF4_BR_GRAV)) vp[vn++] = "gravity (up to MHP/3<200;)";
	if (flags4 & (RF4_BR_SHAR)) vp[vn++] = "shards (up to MHP/6<400;)";
	if (flags4 & (RF4_BR_PLAS)) vp[vn++] = "plasma (up to MHP/6<150;)";
	if (flags4 & (RF4_BR_WALL)) vp[vn++] = "force (up to MHP/6<200;)";
	if (flags4 & (RF4_BR_MANA)) vp[vn++] = "mana (up to MHP/3<250;)";
	if (flags4 & (RF4_BR_NUKE))     vp[vn++] = "toxic waste (up to MHP/3<800;)";
	if (flags4 & (RF4_BR_DISI))     vp[vn++] = "disintegration (up to MHP/3<300;)";

	/* Describe breaths */
	if (vn)
	{
		/* Note breath */
		breath = TRUE;

		/* Intro */
		c_roff(MONCOL_BREATH, format("%^s", wd_he[msex]));

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) c_roff(MONCOL_BREATH, " may breathe ");
			else if (n < vn-1) c_roff(MONCOL_BREATH, ", ");
			else c_roff(MONCOL_BREATH, " or ");

			/* Dump */
			c_roff(MONCOL_BREATH, convert_spell_text(vp[n], r_ptr));
		}
	}


	/* Collect spells */

	/* If "LEV" is followed by some mathematical terms, the game will combine
	 * them into a single number. Because of this, the format is important.
	 * In fact, it should always be LEV*A/B+C+EdF. */

	vn = 0;
	if (flags5 & (RF5_BA_ACID)) vp[vn++] = "produce acid balls (LEV*3+15;)";
	if (flags5 & (RF5_BA_ELEC)) vp[vn++] = "produce lightning balls (LEV*3/2+8;)";
	if (flags5 & (RF5_BA_FIRE)) vp[vn++] = "produce fire balls (LEV*7/2+10;)";
	if (flags5 & (RF5_BA_COLD)) vp[vn++] = "produce frost balls (LEV*3/2+10;)";
	if (flags5 & (RF5_BA_POIS)) vp[vn++] = "produce poison balls (12d2)";
	if (flags5 & (RF5_BA_NETH)) vp[vn++] = "produce nether balls (LEV+50;+10d10)";
	if (flags5 & (RF5_BA_WATE)) vp[vn++] = "produce water balls (50+1dLEV*5/2;)";
	if (flags4 & (RF4_BA_NUKE))     vp[vn++] = "produce balls of radiation (LEV;+10d6)";
	if (flags5 & (RF5_BA_MANA)) vp[vn++] = "invoke mana storms (LEV*5;+10d10)";
	if (flags5 & (RF5_BA_DARK)) vp[vn++] = "invoke darkness storms (LEV*5;+10d10)";
	if (flags4 & (RF4_BA_CHAO))     vp[vn++] = "invoke raw chaos (LEV*2;+10d10)";
	if (flags6 & (RF6_DREAD_CURSE))        vp[vn++] = "invoke the Dread Curse of Azathoth (66-90% of HP)";
	if (flags5 & (RF5_DRAIN_MANA)) vp[vn++] = "drain mana";
	if (flags5 & (RF5_MIND_BLAST)) vp[vn++] = "cause mind blasting (8d8)";
	if (flags5 & (RF5_BRAIN_SMASH)) vp[vn++] = "cause brain smashing (12d15)";
	if (flags5 & (RF5_CAUSE_1))     vp[vn++] = "cause light wounds (3d8) and cursing";
	if (flags5 & (RF5_CAUSE_2))     vp[vn++] = "cause serious wounds (8d8) and cursing";
	if (flags5 & (RF5_CAUSE_3))     vp[vn++] = "cause critical wounds (10d15) and cursing";
	if (flags5 & (RF5_CAUSE_4)) vp[vn++] = "cause mortal wounds (15d15)";
	if (flags5 & (RF5_BO_ACID)) vp[vn++] = "produce acid bolts (LEV/3;+7d8)";
	if (flags5 & (RF5_BO_ELEC)) vp[vn++] = "produce lightning bolts (LEV/3;+4d8)";
	if (flags5 & (RF5_BO_FIRE)) vp[vn++] = "produce fire bolts (LEV/3;+9d8)";
	if (flags5 & (RF5_BO_COLD)) vp[vn++] = "produce frost bolts (LEV/3;+6d8)";
	if (flags5 & (RF5_BO_POIS)) vp[vn++] = "do nothing (RF5_BO_POIS)";
	if (flags5 & (RF5_BO_NETH)) vp[vn++] = "produce nether bolts (LEV*3/2+30;+5d5)";
	if (flags5 & (RF5_BO_WATE)) vp[vn++] = "produce water bolts (LEV;+10d10)";
	if (flags5 & (RF5_BO_MANA)) vp[vn++] = "produce mana bolts (50+1dLEV*7/2;)";
	if (flags5 & (RF5_BO_PLAS)) vp[vn++] = "produce plasma bolts (LEV+10;+8d7)";
	if (flags5 & (RF5_BO_ICEE)) vp[vn++] = "produce ice bolts (LEV;+6d6)";
	if (flags5 & (RF5_MISSILE)) vp[vn++] = "produce magic missiles (LEV/3;+2d6)";
	if (flags5 & (RF5_SCARE)) vp[vn++] = "terrify";
	if (flags5 & (RF5_BLIND)) vp[vn++] = "blind";
	if (flags5 & (RF5_CONF)) vp[vn++] = "confuse";
	if (flags5 & (RF5_SLOW)) vp[vn++] = "slow";
	if (flags5 & (RF5_HOLD)) vp[vn++] = "paralyze";
	if (flags6 & (RF6_HASTE)) vp[vn++] = "haste-self";

	if (flags6 & (RF6_HEAL)) vp[vn++] = "heal-self";
	if (flags6 & (RF6_XXX2)) vp[vn++] = "do something";
	if (flags6 & (RF6_BLINK)) vp[vn++] = "blink-self";
	if (flags6 & (RF6_TPORT)) vp[vn++] = "teleport-self";
	if (flags6 & (RF6_XXX3)) vp[vn++] = "do something";
	if (flags6 & (RF6_XXX4)) vp[vn++] = "do something";
	if (flags6 & (RF6_TELE_TO)) vp[vn++] = "teleport to";
	if (flags6 & (RF6_TELE_AWAY)) vp[vn++] = "teleport away";
	if (flags6 & (RF6_TELE_LEVEL)) vp[vn++] = "teleport level";
	if (flags6 & (RF6_XXX5)) vp[vn++] = "do something";
	if (flags6 & (RF6_DARKNESS)) vp[vn++] = "create darkness";
	if (flags6 & (RF6_TRAPS)) vp[vn++] = "create traps";
	if (flags6 & (RF6_FORGET)) vp[vn++] = "cause amnesia";
	if (flags6 & (RF6_S_MONSTER))       vp[vn++] = "summon a monster";
	if (flags6 & (RF6_S_MONSTERS)) vp[vn++] = "summon monsters";
	if (flags6 & (RF6_S_KIN))       vp[vn++] = "summon aid";
	if (flags6 & (RF6_S_ANT)) vp[vn++] = "summon ants";
	if (flags6 & (RF6_S_SPIDER)) vp[vn++] = "summon spiders";
	if (flags6 & (RF6_S_HOUND)) vp[vn++] = "summon hounds";
	if (flags6 & (RF6_S_HYDRA)) vp[vn++] = "summon hydras";
		if (flags6 & (RF6_S_IB)) vp[vn++] = "summon beings of Ib";
	if (flags6 & (RF6_S_CTHULOID)) vp[vn++] = "summon a Cthuloid entity";
	if (flags6 & (RF6_S_DEMON)) vp[vn++] = "summon a demon";
	if (flags6 & (RF6_S_UNDEAD)) vp[vn++] = "summon an undead";
	if (flags6 & (RF6_S_DRAGON)) vp[vn++] = "summon a dragon";
	if (flags6 & (RF6_S_HI_UNDEAD)) vp[vn++] = "summon Greater Undead";
	if (flags6 & (RF6_S_HI_DRAGON)) vp[vn++] = "summon Ancient Dragons";
	if (flags6 & (RF6_S_REAVER))     vp[vn++] = "summon Black Reavers";
	if (flags6 & (RF6_S_GOO))        vp[vn++] = "summon Great Old Ones";
	if (flags6 & (RF6_S_UNIQUE)) vp[vn++] = "summon Unique Monsters";

	/* Describe spells */
	if (vn)
	{
		/* Note magic */
		magic = TRUE;

		/* Intro */
		if (breath)
		{
			c_roff(MONCOL_MAGIC, ", and is also");
		}
		else
		{
			c_roff(MONCOL_MAGIC, format("%^s is", wd_he[msex]));
		}

		/* Verb Phrase */
		c_roff(MONCOL_MAGIC, " magical, casting spells");

		/* Adverb */
		if (flags2 & (RF2_SMART)) c_roff(MONCOL_MAGIC, " intelligently");

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) c_roff(MONCOL_MAGIC, " which ");
			else if (n < vn-1) c_roff(MONCOL_MAGIC, ", ");
			else c_roff(MONCOL_MAGIC, " or ");

			/* Dump */
			c_roff(MONCOL_MAGIC, convert_spell_text(vp[n], r_ptr));
		}
	}


	/* End the sentence about inate/other spells */
	if (breath || magic)
	{
		/* Total casting */
		m = r_ptr->r_cast_inate + r_ptr->r_cast_spell;

		/* Average frequency */
		n = (r_ptr->freq_inate + r_ptr->freq_spell) / 2;

		/* Describe the spell frequency */
		if (m > 100)
		{
			c_roff(MONCOL_MAGIC, format("; 1 time in %d", 100 / n));
		}

		/* Guess at the frequency */
		else if (m)
		{
			n = ((n + 9) / 10) * 10;
			c_roff(MONCOL_MAGIC, format("; about 1 time in %d", 100 / n));
		}

		/* End this sentence */
		c_roff(MONCOL_MAGIC, ".  ");
	}


	/* Describe monster "toughness" */
	if (know_armour(r_ptr) || spoil_mon)
	{
		/* Armor */
		c_roff(MONCOL_ACHP, format("%^s has an armor rating of %d",
					wd_he[msex], r_ptr->ac));

		/* Maximized hitpoints */
		if (flags1 & (RF1_FORCE_MAXHP))
		{
			c_roff(MONCOL_ACHP, format(" and a life rating of %d.  ",
						r_ptr->hdice * r_ptr->hside));
		}

		/* Variable hitpoints */
		else
		{
			c_roff(MONCOL_ACHP, format(" and a life rating of %dd%d.  ",
						r_ptr->hdice, r_ptr->hside));
		}
	}



	/* Collect special abilities. */
	vn = 0;
	if (flags2 & (RF2_OPEN_DOOR)) vp[vn++] = "open doors";
	if (flags2 & (RF2_BASH_DOOR)) vp[vn++] = "bash down doors";
	if (flags2 & (RF2_PASS_WALL)) vp[vn++] = "pass through walls";
	if (flags2 & (RF2_KILL_WALL)) vp[vn++] = "bore through walls";
	if (flags2 & (RF2_MOVE_BODY)) vp[vn++] = "push past weaker monsters";
	if (flags2 & (RF2_KILL_BODY)) vp[vn++] = "destroy weaker monsters";
	if (flags2 & (RF2_TAKE_ITEM)) vp[vn++] = "pick up objects";
	if (flags2 & (RF2_KILL_ITEM)) vp[vn++] = "destroy objects";

	/* Describe special abilities. */
	if (vn)
	{
		/* Intro */
		c_roff(MONCOL_ABLE1, format("%^s", wd_he[msex]));

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) c_roff(MONCOL_ABLE1, " can ");
			else if (n < vn-1) c_roff(MONCOL_ABLE1, ", ");
			else c_roff(MONCOL_ABLE1, " and ");

			/* Dump */
			c_roff(MONCOL_ABLE1, vp[n]);
		}

		/* End */
		c_roff(MONCOL_ABLE1, ".  ");
	}


	/* Describe special abilities. */
	if (flags2 & (RF2_INVISIBLE))
	{
		c_roff(MONCOL_ABLE2, format("%^s is invisible.  ", wd_he[msex]));
	}
	if (flags2 & (RF2_COLD_BLOOD))
	{
		c_roff(MONCOL_ABLE2, format("%^s is cold blooded.  ", wd_he[msex]));
	}
	if (flags2 & (RF2_EMPTY_MIND))
	{
		c_roff(MONCOL_ABLE2, format("%^s is not detected by telepathy.  ", wd_he[msex]));
	}
	if (flags2 & (RF2_WEIRD_MIND))
	{
		c_roff(MONCOL_ABLE2, format("%^s is rarely detected by telepathy.  ", wd_he[msex]));
	}
	if (flags2 & (RF2_MULTIPLY))
	{
		c_roff(MONCOL_ABLE2, format("%^s breeds explosively.  ", wd_he[msex]));
	}
	if (flags2 & (RF2_REGENERATE))
	{
		c_roff(MONCOL_ABLE2, format("%^s regenerates quickly.  ", wd_he[msex]));
	}


	/* Collect susceptibilities */
	vn = 0;
	if (flags3 & (RF3_HURT_ROCK)) vp[vn++] = "rock remover";
	if (flags3 & (RF3_HURT_LITE)) vp[vn++] = "bright light";
	if (flags3 & (RF3_HURT_FIRE)) vp[vn++] = "fire";
	if (flags3 & (RF3_HURT_COLD)) vp[vn++] = "cold";

	/* Describe susceptibilities */
	if (vn)
	{
		/* Intro */
		c_roff(MONCOL_WEAK, format("%^s", wd_he[msex]));

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) c_roff(MONCOL_WEAK, " is hurt by ");
			else if (n < vn-1) c_roff(MONCOL_WEAK, ", ");
			else c_roff(MONCOL_WEAK, " and ");

			/* Dump */
			c_roff(MONCOL_WEAK, vp[n]);
		}

		/* End */
		c_roff(MONCOL_WEAK, ".  ");
	}


	/* Collect immunities */
	vn = 0;
	if (flags3 & (RF3_IM_ACID)) vp[vn++] = "acid";
	if (flags3 & (RF3_IM_ELEC)) vp[vn++] = "lightning";
	if (flags3 & (RF3_IM_FIRE)) vp[vn++] = "fire";
	if (flags3 & (RF3_IM_COLD)) vp[vn++] = "cold";
	if (flags3 & (RF3_IM_POIS)) vp[vn++] = "poison";
	if (flags3 & (RF3_IM_WATER)) vp[vn++] = "water";

	/* Describe immunities */
	if (vn)
	{
		/* Intro */
		c_roff(MONCOL_ELEM, format("%^s", wd_he[msex]));

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) c_roff(MONCOL_ELEM, " resists ");
			else if (n < vn-1) c_roff(MONCOL_ELEM, ", ");
			else c_roff(MONCOL_ELEM, " and ");

			/* Dump */
			c_roff(MONCOL_ELEM, vp[n]);
		}

		/* End */
		c_roff(MONCOL_ELEM, ".  ");
	}


	/* Collect resistances */
	vn = 0;
	if (flags3 & (RF3_RES_NETH)) vp[vn++] = "nether";
	if (flags3 & (RF3_RES_PLAS)) vp[vn++] = "plasma";
	if (flags3 & (RF3_RES_NEXU)) vp[vn++] = "nexus";
	if (flags3 & (RF3_RES_DISE)) vp[vn++] = "disenchantment";
	if (flags3 & (RF3_RES_TELE)) vp[vn++] = "teleportation";

	/* Describe resistances */
	if (vn)
	{
		/* Intro */
		c_roff(MONCOL_RESIST, format("%^s", wd_he[msex]));

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) c_roff(MONCOL_RESIST, " resists ");
			else if (n < vn-1) c_roff(MONCOL_RESIST, ", ");
			else c_roff(MONCOL_RESIST, " and ");

			/* Dump */
			c_roff(MONCOL_RESIST, vp[n]);
		}

		/* End */
		c_roff(MONCOL_RESIST, ".  ");
	}


	/* Collect non-effects */
	vn = 0;
	if (flags3 & (RF3_NO_STUN)) vp[vn++] = "stunned";
	if (flags3 & (RF3_NO_FEAR)) vp[vn++] = "frightened";
	if (flags3 & (RF3_NO_CONF)) vp[vn++] = "confused";
	if (flags3 & (RF3_NO_SLEEP)) vp[vn++] = "slept";

	/* Describe non-effects */
	if (vn)
	{
		/* Intro */
		c_roff(MONCOL_IMMUN, format("%^s", wd_he[msex]));

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) c_roff(MONCOL_IMMUN, " cannot be ");
			else if (n < vn-1) c_roff(MONCOL_IMMUN, ", ");
			else c_roff(MONCOL_IMMUN, " or ");

			/* Dump */
			c_roff(MONCOL_IMMUN, vp[n]);
		}

		/* End */
		c_roff(MONCOL_IMMUN, ".  ");
	}


	/* Do we know how aware it is? */
	if ((((int)r_ptr->r_wake * (int)r_ptr->r_wake) > r_ptr->sleep) ||
		(r_ptr->r_ignore == MAX_UCHAR) ||
		((r_ptr->sleep == 0) && (r_ptr->r_tkills >= 10)))
	{
		cptr act;

		if (r_ptr->sleep > 200)
		{
			act = "prefers to ignore";
		}
		else if (r_ptr->sleep > 95)
		{
			act = "pays very little attention to";
		}
		else if (r_ptr->sleep > 75)
		{
			act = "pays little attention to";
		}
		else if (r_ptr->sleep > 45)
		{
			act = "tends to overlook";
		}
		else if (r_ptr->sleep > 25)
		{
			act = "takes quite a while to see";
		}
		else if (r_ptr->sleep > 10)
		{
			act = "takes a while to see";
		}
		else if (r_ptr->sleep > 5)
		{
			act = "is fairly observant of";
		}
		else if (r_ptr->sleep > 3)
		{
			act = "is observant of";
		}
		else if (r_ptr->sleep > 1)
		{
			act = "is very observant of";
		}
		else if (r_ptr->sleep > 0)
		{
			act = "is vigilant for";
		}
		else
		{
			act = "is ever vigilant for";
		}

		c_roff(MONCOL_OBSERVE, format("%^s %s intruders, which %s may notice from %d feet.  ",
					wd_he[msex], act, wd_he[msex], 10 * r_ptr->aaf));
	}


	/* Drops gold and/or items */
	if (r_ptr->r_drop_gold || r_ptr->r_drop_item)
	{
		/* No "n" needed */
		sin = FALSE;

		/* Intro */
		c_roff(MONCOL_DROP, format("%^s may carry", wd_he[msex]));

		/* Count maximum drop */
		n = MAX(r_ptr->r_drop_gold, r_ptr->r_drop_item);

		/* One drop (may need an "n") */
		if (n == 1)
		{
			c_roff(MONCOL_DROP, " a");
			sin = TRUE;
		}

		/* Two drops */
		else if (n == 2)
		{
			c_roff(MONCOL_DROP, " one or two");
		}

		/* Many drops */
		else
		{
			c_roff(MONCOL_DROP, format(" up to %d", n));
		}


		/* Great */
		if (flags1 & (RF1_DROP_GREAT))
		{
			p = " exceptional";
		}

		/* Good (no "n" needed) */
		else if (flags1 & (RF1_DROP_GOOD))
		{
			p = " good";
			sin = FALSE;
		}

		/* Okay */
		else
		{
			p = NULL;
		}


		/* Objects */
		if (r_ptr->r_drop_item)
		{
			/* Handle singular "an" */
			if (sin) c_roff(MONCOL_DROP, "n");
			sin = FALSE;

			/* Dump "object(s)" */
			if (p) c_roff(MONCOL_DROP, p);
			c_roff(MONCOL_DROP, " object");
			if (n != 1) c_roff(MONCOL_DROP, "s");

			/* Conjunction replaces variety, if needed for "gold" below */
			p = " or";
		}

		/* Treasures */
		if (r_ptr->r_drop_gold)
		{
			/* Cancel prefix */
			if (!p) sin = FALSE;

			/* Handle singular "an" */
			if (sin) c_roff(MONCOL_DROP, "n");
			sin = FALSE;

			/* Dump "treasure(s)" */
			if (p) c_roff(MONCOL_DROP, p);
			c_roff(MONCOL_DROP, " treasure");
			if (n != 1) c_roff(MONCOL_DROP, "s");
		}

		/* End this sentence */
		c_roff(MONCOL_DROP, ".  ");
	}

	/* Include death events here. */
	c_roff(MONCOL_DROP, describe_death_events(r_idx, wd_he[msex], spoil_mon));

	/* Count the number of "known" attacks */
	for (n = 0, m = 0; m < 4; m++)
	{
		/* Skip non-attacks */
		if (!r_ptr->blow[m].method) continue;

		/* Count known attacks */
		if (r_ptr->r_blows[m]) n++;
	}

	/* Examine (and count) the actual attacks */
	for (r = 0, m = 0; m < 4; m++)
	{
		int method, effect, d1, d2;

		blow_method_type *b_ptr = get_blow_method(r_ptr->blow[m].method);

		/* Skip non-attacks. */
		if (!b_ptr) continue;

		/* Skip unknown attacks */
		if (!r_ptr->r_blows[m]) continue;


		/* Extract the attack info */
		method = r_ptr->blow[m].method;
		effect = r_ptr->blow[m].effect;
		d1 = r_ptr->blow[m].d_dice;
		d2 = r_ptr->blow[m].d_side;


		/* Method string. */
		p = b_ptr->name;


		/* Default effect */
		q = NULL;

		/* Acquire the effect */
		switch (effect)
		{
			case RBE_HURT: q = "attack"; break;
			case RBE_POISON: q = "poison"; break;
			case RBE_UN_BONUS: q = "disenchant"; break;
			case RBE_UN_POWER: q = "drain charges"; break;
			case RBE_EAT_GOLD: q = "steal gold"; break;
			case RBE_EAT_ITEM: q = "steal items"; break;
			case RBE_EAT_FOOD: q = "eat your food"; break;
			case RBE_EAT_LITE: q = "absorb light"; break;
			case RBE_ACID: q = "shoot acid"; break;
			case RBE_ELEC:  q = "electrocute"; break;
			case RBE_FIRE: q = "burn"; break;
			case RBE_COLD: q = "freeze"; break;
			case RBE_BLIND: q = "blind"; break;
			case RBE_CONFUSE: q = "confuse"; break;
			case RBE_TERRIFY: q = "terrify"; break;
			case RBE_PARALYZE: q = "paralyze"; break;
			case RBE_LOSE_STR: q = "reduce strength"; break;
			case RBE_LOSE_INT: q = "reduce intelligence"; break;
			case RBE_LOSE_WIS: q = "reduce wisdom"; break;
			case RBE_LOSE_DEX: q = "reduce dexterity"; break;
			case RBE_LOSE_CON: q = "reduce constitution"; break;
			case RBE_LOSE_CHR: q = "reduce charisma"; break;
			case RBE_LOSE_ALL: q = "reduce all stats"; break;
			case RBE_SHATTER: q = "shatter"; break;
			case RBE_EXP_10: q = "lower experience (by 10d6+)"; break;
			case RBE_EXP_20: q = "lower experience (by 20d6+)"; break;
			case RBE_EXP_40: q = "lower experience (by 40d6+)"; break;
			case RBE_EXP_80: q = "lower experience (by 80d6+)"; break;
		}


		/* Introduce the attack description */
		if (!r)
		{
			c_roff(MONCOL_ATTACK, format("%^s can ", wd_he[msex]));
		}
		else if (r < n-1)
		{
			c_roff(MONCOL_ATTACK, ", ");
		}
		else
		{
			c_roff(MONCOL_ATTACK, ", and ");
		}


		/* Hack -- force a method */
		if (!p) p = "do something weird";

		/* Describe the method */
		c_roff(MONCOL_ATTACK, p);


		/* Describe the effect (if any) */
		if (q)
		{
			/* Describe the attack type */
			c_roff(MONCOL_ATTACK, " to ");
			c_roff(MONCOL_ATTACK, q);

			/* Describe damage (if known) */
			if (d1 && d2 && know_damage(r_idx, m))
			{
				/* Display the damage */
				c_roff(MONCOL_ATTACK, " with damage");
				c_roff(MONCOL_ATTACK, format(" %dd%d", d1, d2));
			}
		}


		/* Count the attacks as printed */
		r++;
	}

	/* Finish sentence above */
	if (r)
	{
		c_roff(MONCOL_ATTACK, ".  ");
	}

	/* Notice lack of attacks */
	else if (flags1 & (RF1_NEVER_BLOW))
	{
		c_roff(MONCOL_ATTACK, format("%^s has no physical attacks.  ", wd_he[msex]));
	}

	/* Or describe the lack of knowledge */
	else
	{
		c_roff(MONCOL_ATTACK, format("Nothing is known about %s attack.  ", wd_his[msex]));
	}


	/* Notice "Quest" monsters */
	if (flags1 & (RF1_GUARDIAN))
	{
		if (r_ptr->max_num)
		{
		c_roff(MONCOL_QUEST, "You feel an intense desire to kill this monster...  ");
	}
		else
		{
			c_roff(MONCOL_QUEST, "You felt an intense desire to kill this monster...  ");
		}
	}


	/* All done */
	c_roff(TERM_WHITE, "\n");


	/* Hack -- Restore monster memory */
	if (spoil_mon)
	{
		/* Restore memory */
		*r_ptr = save_mem;
	}
}





/*
 * Hack -- Display the "name" and "attr/chars" of a monster race
 */
void roff_top(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	byte a1, a2;
	char c1, c2;


	/* Access the chars */
	c1 = r_ptr->gfx.dc;
	c2 = r_ptr->gfx.xc;

	/* Access the attrs */
	a1 = r_ptr->gfx.da;
	a2 = r_ptr->gfx.xa;

	/* Clear the top line */
	Term_erase(0, 0, 255);

	/* Dump the name */
	mc_put_fmt(0, 0, "%^v (%v)/(%v):", monster_desc_aux_f3, r_ptr, 1, MDF_DEF,
		get_symbol_f2, a1, c1, get_symbol_f2, a2, c2);
}



/*
 * Hack -- describe the given monster race at the top of the screen
 */
void screen_roff(int r_idx)
{
	/* Begin recall */
	Term_erase(0, 1, 255);

	/* Recall monster */
	roff_aux(r_idx);

	/* Describe monster */
	roff_top(r_idx);
}
