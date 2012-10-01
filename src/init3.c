/* File: init3.c */

/*
 * Copyright (c) 1997 Ben Harrison
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"

#include "init.h"


/*
 * This file is used to initialize the town, special building and
 * quest templates
 */


/*
 * Initialize buildings
 */
errr init_buildings(void)
{
	int i, j;

	for (i = 0; i < MAX_BLDG; i++)
	{
		building[i].name[0] = '\0';
		building[i].owner_name[0] = '\0';
		building[i].owner_race[0] = '\0';

		for (j = 0; j < 6; j++)
		{
			building[i].act_names[j][0] = '\0';
			building[i].member_costs[j] = 0;
			building[i].other_costs[j] = 0;
			building[i].letters[j] = 0;
			building[i].actions[j] = 0;
			building[i].action_restr[j] = 0;
		}

		for (j = 0; j < MAX_CLASS; j++)
		{
			building[i].member_class[j] = 0;
		}

		for (j = 0; j < MAX_RACES; j++)
		{
			building[i].member_race[j] = 0;
		}

#if 0
		for (j = 0; j < MAX_REALM+1; j++)
		{
			building[i].member_realm[j] = 0;
		}
#endif
	}

	return (0);
}


/*
 * Random dungeon grid effects
 */
#define RANDOM_NONE         0x00
#define RANDOM_FEATURE      0x01
#define RANDOM_MONSTER      0x02
#define RANDOM_OBJECT       0x04
#define RANDOM_EGO          0x08
#define RANDOM_ARTIFACT     0x10
#define RANDOM_TRAP         0x20


typedef struct dungeon_grid dungeon_grid;

struct dungeon_grid
{
	int		feature;		/* Terrain feature */
	int		monster;		/* Monster */
	int		object;		/* Object */
	int		ego;			/* Ego-Item */
	int		artifact;		/* Artifact */
	int		trap;			/* Trap */
	int		cave_info;		/* Flags for CAVE_MARK, CAVE_GLOW, CAVE_ICKY, CAVE_ROOM */
	int		special;		/* Reserved for special terrain info */
	int		random;		/* Number of the random effect */
};


static dungeon_grid letter[255];


/*
 * Process "F:<letter>:<terrain>:<cave_info>:<monster>:<object>:<ego>:<artifact>:<trap>:<special>" -- info for dungeon grid
 */
static errr parse_line_feature(char *buf)
{
	int num;
	char *zz[9];


	if (init_flags & INIT_ONLY_BUILDINGS) return (0);

	/* Tokenize the line */
	if ((num = tokenize(buf+2, 9, zz)) > 1)
	{
		/* Letter to assign */
		int index = zz[0][0];

		/* Reset the info for the letter */
		letter[index].feature = 0;
		letter[index].monster = 0;
		letter[index].object = 0;
		letter[index].ego = 0;
		letter[index].artifact = 0;
		letter[index].trap = 0;
		letter[index].cave_info = 0;
		letter[index].special = 0;
		letter[index].random = 0;

		switch (num)
		{
			/* Special */
			case 9:
				letter[index].special = atoi(zz[8]);
				/* Fall through */
			/* Trap */
			case 8:
				if (zz[7][0] == '*')
				{
					letter[index].random |= RANDOM_TRAP;

					if (zz[7][1])
					{
						zz[7]++;
						letter[index].trap = atoi(zz[7]);
					}
				}
				else
				{
					letter[index].trap = atoi(zz[7]);
				}
				/* Fall through */
			/* Artifact */
			case 7:
				if (zz[6][0] == '*')
				{
					letter[index].random |= RANDOM_ARTIFACT;

					if (zz[6][1])
					{
						zz[6]++;
						letter[index].artifact = atoi(zz[6]);
					}
				}
				else
				{
					letter[index].artifact = atoi(zz[6]);
				}
				/* Fall through */
			/* Ego-item */
			case 6:
				if (zz[5][0] == '*')
				{
					letter[index].random |= RANDOM_EGO;

					if (zz[5][1])
					{
						zz[5]++;
						letter[index].ego = atoi(zz[5]);
					}
				}
				else
				{
					letter[index].ego = atoi(zz[5]);
				}
				/* Fall through */
			/* Object */
			case 5:
				if (zz[4][0] == '*')
				{
					letter[index].random |= RANDOM_OBJECT;

					if (zz[4][1])
					{
						zz[4]++;
						letter[index].object = atoi(zz[4]);
					}
				}
				else
				{
					letter[index].object = atoi(zz[4]);
				}
				/* Fall through */
			/* Monster */
			case 4:
				if (zz[3][0] == '*')
				{
					letter[index].random |= RANDOM_MONSTER;
					if (zz[3][1])
					{
						zz[3]++;
						letter[index].monster = atoi(zz[3]);
					}
				}
				else
				{
					letter[index].monster = atoi(zz[3]);
				}
				/* Fall through */
			/* Cave info */
			case 3:
				letter[index].cave_info = atoi(zz[2]);
				/* Fall through */
			/* Feature */
			case 2:
				if (zz[1][0] == '*')
				{
					letter[index].random |= RANDOM_FEATURE;
					if (zz[1][1])
					{
						zz[1]++;
						letter[index].feature = atoi(zz[1]);
					}
				}
				else
				{
					letter[index].feature = atoi(zz[1]);
				}
				break;
		}

		return (0);
	}

	return (PARSE_ERROR_GENERIC);
}


/*
 * Process "F:<letter>:<terrain>:<cave_info>:<monster>:<object>:<ego>:<artifact>:<trap>:<special>" -- info for dungeon grid
 */
static errr parse_line_building(char *buf)
{
	int i;
	char *zz[33];
	int index;
	char *s;


	/* Find the colon after the building number */
	s = strchr(buf + 2, ':');

	/* Verify that colon */
	if (!s) return (PARSE_ERROR_GENERIC);

	/* Nuke the colon, advance to the sub-index */
	*s++ = '\0';

	/* Paranoia -- require a sub-index */
	if (!*s) return (PARSE_ERROR_GENERIC);

	/* Get the building number */
	index = atoi(buf + 2);

	/* Building definition sub-index */
	switch (s[0])
	{
		/* Building name, owner, race */
		case 'N':
		{
			if (tokenize(s + 2, 3, zz) == 3)
			{
				/* Name of the building */
				strcpy(building[index].name, zz[0]);

				/* Name of the owner */
				strcpy(building[index].owner_name, zz[1]);

				/* Race of the owner */
				strcpy(building[index].owner_race, zz[2]);

				break;
			}

			return (PARSE_ERROR_TOO_FEW_ARGUMENTS);
		}

		/* Building Action */
		case 'A':
		{
			if (tokenize(s + 2, 8, zz) >= 7)
			{
				/* Index of the action */
				int action_index = atoi(zz[0]);

				/* Name of the action */
				strcpy(building[index].act_names[action_index], zz[1]);

				/* Cost of the action for members */
				building[index].member_costs[action_index] = atoi(zz[2]);

				/* Cost of the action for non-members */
				building[index].other_costs[action_index] = atoi(zz[3]);

				/* Letter assigned to the action */
				building[index].letters[action_index] = zz[4][0];

				/* Action code */
				building[index].actions[action_index] = atoi(zz[5]);

				/* Action restriction */
				building[index].action_restr[action_index] = atoi(zz[6]);

				break;
			}

			return (PARSE_ERROR_TOO_FEW_ARGUMENTS);
		}

		/* Building Classes */
		case 'C':
		{
			if (tokenize(s + 2, MAX_CLASS, zz) == MAX_CLASS)
			{
				for (i = 0; i < MAX_CLASS; i++)
				{
					building[index].member_class[i] = atoi(zz[i]);
				}

				break;
			}

			return (PARSE_ERROR_TOO_FEW_ARGUMENTS);
		}

		/* Building Races */
		case 'R':
		{
			if (tokenize(s + 2, MAX_RACES, zz) == MAX_RACES)
			{
				for (i = 0; i < MAX_RACES; i++)
				{
					building[index].member_race[i] = atoi(zz[i]);
				}

				break;
			}

			return (PARSE_ERROR_TOO_FEW_ARGUMENTS);
		}

#if 0
		/* Building Realms */
		case 'M':
		{
			if (tokenize(s+2, MAX_REALM, zz, 0) == MAX_REALM)
			{
				for (i = 0; i < MAX_REALM; i++)
				{
					building[index].member_realm[i+1] = atoi(zz[i]);
				}

				break;
			}

			return (PARSE_ERROR_TOO_FEW_ARGUMENTS);
		}
#endif

		default:
		{
			return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
		}
	}

	return (0);
}


/*
 * Check for equipment
 */
static bool check_ego(int tval)
{
	switch (tval)
	{
		case TV_SWORD:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_DIGGING:
		case TV_BOW:
		case TV_BOLT:
		case TV_ARROW:
		case TV_SHOT:
		case TV_HARD_ARMOR:
		case TV_SOFT_ARMOR:
		case TV_SHIELD:
		case TV_CLOAK:
		case TV_CROWN:
		case TV_HELM:
		case TV_BOOTS:
		case TV_GLOVES:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}


/*
 * Parse a sub-file of the "extra info"
 * 
 * Note in the placement of terrain, monsters, objects and traps a certain
 * heirarchy has been implemented as follows:
 * 
 * 1. Monsters
 * 2. Artifacts
 * 3. Traps
 * 4. Ego items
 * 5. Other objects
 * 6. Terrain
 *
 * Note that this order means that specifying an artifact index will override
 * a trap designation or an object or ego type designation.  If no artifacts
 * are present, placing a trap will prevent the placing of an object of any
 * kind. Ego items have been coded to require an associated object. 
 */
static errr process_dungeon_file_aux(char *buf, int ymin, int xmin, int ymax, int xmax, int *y, int *x)
{
	int i;

	char *zz[33];


	/* Skip "empty" lines */
	if (!buf[0]) return (0);

	/* Skip "blank" lines */
	if (isspace(buf[0])) return (0);

	/* Skip comments */
	if (buf[0] == '#') return (0);

	/* Require "?:*" format */
	if (buf[1] != ':') return (PARSE_ERROR_GENERIC);


	/* Process "%:<fname>" */
	if (buf[0] == '%')
	{
		/* Attempt to Process the given file */
		return (process_dungeon_file(buf + 2, ymin, xmin, ymax, xmax));
	}


	/* Process "N:<index>:<name>" */
	if (buf[0] == 'N')
	{
		int num = tokenize(buf + 2, 33, zz);

		plot_type *pt_ptr;
		
		/* Have we enough parameters? */
		if (num < 2) return (PARSE_ERROR_TOO_FEW_ARGUMENTS);

		/* Get the plot */
		pt_ptr = &(plots[atoi(zz[0])]);

		if (init_flags & (INIT_ASSIGN | INIT_SHOW_TEXT))
		{
			strcpy(pt_ptr->name, zz[1]);
		}

		/* Success */
		return (0);
	}


	/* Process "F:<letter>:<terrain>:<cave_info>:<monster>:<object>:<ego>:<artifact>:<trap>:<special>" -- info for dungeon grid */
	if (buf[0] == 'F')
	{
		return parse_line_feature(buf);
	}

	/* Process "D:<dungeon>" -- info for the cave grids */
	else if (buf[0] == 'D')
	{
		object_type object_type_body;

		/* Acquire the text */
		char *s = buf + 2;

		/* Length of the text */
		int len = strlen(s);

		if (init_flags & INIT_ONLY_BUILDINGS) return (0);

		for (*x = xmin, i = 0; ((*x < xmax) && (i < len)); (*x)++, s++, i++)
		{
			int idx = s[0];

			int object_index = letter[idx].object;
			int ego_index = letter[idx].ego;
			int monster_index = letter[idx].monster;
			int random = letter[idx].random;
			int artifact_index = letter[idx].artifact;

			/* Lay down a floor */
			cave_set_feat(*y, *x, letter[idx].feature);

			/* Only the features */
			if (init_flags & INIT_ONLY_FEATURES) continue;

			/* Cave info */
			cave_info[*y][*x] |= letter[idx].cave_info;

			/* Create a monster */
			if (random & RANDOM_MONSTER)
			{
				monster_level = base_level + monster_index;

				place_monster(*y, *x, TRUE, TRUE);

				monster_level = base_level;
			}
			else if (monster_index)
			{
				bool group = FALSE;

				/* Make alive again */
				if (r_info[monster_index].flags1 & RF1_UNIQUE)
				{
					r_info[monster_index].cur_num = 0;
					r_info[monster_index].max_num = 1;

					/* Allow the escort to be generated */
					group = TRUE;
				}

				/* Place it */
				place_monster_aux(*y, *x, monster_index, TRUE, group, FALSE);
			}

			/* Random artifact */
			if (random & RANDOM_ARTIFACT)
			{
				while (TRUE)
				{
					artifact_index = randint(z_info->a_max);

					/* Ignore "empty" artifacts */
					if (!a_info[artifact_index].name)
						continue;

					/* Must not be a quest item */
					if (a_info[artifact_index].flags3 & TR3_QUESTITEM)
						continue;

					/* Artifact "rarity roll" */
					if (rand_int(a_info[artifact_index].rarity) != 0)
						continue;

					/* Ignore artifacts already generated */
					if (a_info[artifact_index].cur_num == 0)
						break;
				}

				/* Create the artifact */
				create_named_art(artifact_index, *y, *x);

				a_info[artifact_index].cur_num = 1;
			}
			/* Specific artifact */
			else if (artifact_index)
			{
				/* Create the artifact */
				create_named_art(artifact_index, *y, *x);

				a_info[artifact_index].cur_num = 1;
			}

			/* Random object or trap */
			else if ((random & RANDOM_OBJECT) && (random & RANDOM_TRAP))
			{
				object_level = base_level + object_index;

				/*
				 * Random trap and random treasure defined
				 * 25% chance for trap and 75% chance for object
				 */
				if (rand_int(100) < 75)
				{
					place_object(*y, *x, FALSE, FALSE);
				}
				else
				{
					place_trap(*y, *x);
				}

				object_level = base_level;
			}
			/* Random trap */
			else if (random & RANDOM_TRAP)
			{
				place_trap(*y, *x);
			}

			/* Random ego with associated random object */
			else if ((random & RANDOM_EGO) && (random & RANDOM_OBJECT))
			{
				/* Get local object */
				object_type *o_ptr = &object_type_body;

				while (TRUE)
				{
					object_index = randint(z_info->k_max);

					if (check_ego(k_info[object_index].tval))
						break;
				}

				/* Create the item */
				object_prep(o_ptr, object_index);

				/* Apply magic (no messages, no artifacts) */
				apply_magic(o_ptr, base_level, FALSE, TRUE, TRUE);

				(void)drop_near(o_ptr, -1, *y, *x);
			}
			/* Random ego for associated specific object */
			else if ((random & RANDOM_EGO) && object_index)
			{
				/* Get local object */
				object_type *o_ptr = &object_type_body;

				/* Create the item */
				object_prep(o_ptr, object_index);

				if (o_ptr->tval == TV_GOLD)
				{
					coin_type = object_index - OBJ_GOLD_LIST;
					make_gold(o_ptr);
					coin_type = 0;
				}

				/* Apply magic (no messages, no artifacts) */
				apply_magic(o_ptr, base_level, FALSE, TRUE, TRUE);

				(void)drop_near(o_ptr, -1, *y, *x);
			}
			/* Specific ego for associated specific object */
			/* Note that if the ego type isn't appropriate */
			/* we simply create a normal object. */
			else if (ego_index && object_index)
			{
				/* Get local object */
				object_type *o_ptr = &object_type_body;

				/* Create the item */
				object_prep(o_ptr, object_index);

				if (o_ptr->tval == TV_GOLD)
				{
					coin_type = object_index - OBJ_GOLD_LIST;
					make_gold(o_ptr);
					coin_type = 0;
				}

				/* Apply magic (no messages, no artifacts) */
				/* Note that we may get inflated bonuses if this */
				/* step independently creates an ego item */
				apply_magic(o_ptr, base_level, FALSE, TRUE, FALSE);

				/* Create the ego item */
				create_named_ego(o_ptr, ego_index, base_level);

				(void)drop_near(o_ptr, -1, *y, *x);
			}
			/* Random object */
			else if (random & RANDOM_OBJECT)
			{
				object_level = base_level + object_index;

				/* Create an out of deep object */
				if (rand_int(100) < 75)
					place_object(*y, *x, FALSE, FALSE);
				else if (rand_int(100) < 80)
					place_object(*y, *x, TRUE, FALSE);
				else
					place_object(*y, *x, TRUE, TRUE);

				object_level = base_level;
			}
			/* Specific object */
			else if (object_index)
			{
				/* Get local object */
				object_type *o_ptr = &object_type_body;

				/* Create the item */
				object_prep(o_ptr, object_index);

				if (o_ptr->tval == TV_GOLD)
				{
					coin_type = object_index - OBJ_GOLD_LIST;
					make_gold(o_ptr);
					coin_type = 0;
				}

				/* Apply magic (no messages, no artifacts) */
				apply_magic(o_ptr, base_level, FALSE, TRUE, FALSE);

				(void)drop_near(o_ptr, -1, *y, *x);
			}

			/* Terrain special */
			cave_special[*y][*x] = letter[idx].special;
		}

		(*y)++;

		return (0);
	}

	/* Process "Q:<number>:<command>:... -- quest info */
	else if (buf[0] == 'Q')
	{
		int num = tokenize(buf + 2, 33, zz);
		quest_type *q_ptr;

		/* Have we enough parameters? */
		if (num < 3) return (PARSE_ERROR_TOO_FEW_ARGUMENTS);

		/* Get the quest */
		q_ptr = &(quest[atoi(zz[0])]);

		/* Process "Q:<q_index>:Q:<type>:<num_mon>:<cur_num>:<max_num>:<level>:<r_idx>:<k_idx>:<flags>" -- quest info */
		if (zz[1][0] == 'Q')
		{
			if (init_flags & INIT_ASSIGN)
			{
				monster_race *r_ptr;
				artifact_type *a_ptr;

				if (num < 9) return (PARSE_ERROR_TOO_FEW_ARGUMENTS);

				q_ptr->type    = atoi(zz[2]);
				q_ptr->num_mon = atoi(zz[3]);
				q_ptr->cur_num = atoi(zz[4]);
				q_ptr->max_num = atoi(zz[5]);
				q_ptr->level   = atoi(zz[6]);
				q_ptr->r_idx   = atoi(zz[7]);
				q_ptr->k_idx   = atoi(zz[8]);

				if (num > 9)
					q_ptr->flags = atoi(zz[9]);

				/* Random Quests - choose max_num */
				if ((q_ptr->type == QUEST_TYPE_RANDOM) && (zz[5][0] == '*'))
					q_ptr->max_num = 5 + randint(q_ptr->level / 5);

				/* Random Quests - choose r_idx */
				if ((q_ptr->type == QUEST_TYPE_RANDOM) && (zz[7][0] == '*'))
				{
					int j, r_idx;

					for (j = 0; j < 100; j++)
					{
						/* Random monster 5 - 10 levels out of depth */
						r_idx = get_mon_num(q_ptr->level + 4 + randint(q_ptr->level / 10));
						r_ptr = &r_info[r_idx];

						/* No random quests for aquatic monsters */
						if (r_ptr->flags2 & RF2_AQUATIC) continue;

						/* No random quests for multiplying monsters */
						if (r_ptr->flags2 & RF2_MULTIPLY) continue;

						/* No quests to kill friendly monsters */
						if (r_ptr->flags2 & RF2_FRIENDLY) continue;

						/* Save the index if the monster is deeper than current monster */
						if (!q_ptr->r_idx || (r_info[r_idx].level > r_info[q_ptr->r_idx].level))
						{
							q_ptr->r_idx = r_idx;
						}

						/* Accept monsters that are 2 - 6 levels
						 * out of depth depending on the quest level
						 */
						if (r_ptr->level > (q_ptr->level + (q_ptr->level / 20) + 1)) break;
					}
				}

				r_ptr = &r_info[q_ptr->r_idx];

				/* Handle unique monsters */
				if (r_ptr->flags1 & RF1_UNIQUE)
				{
					r_ptr->flags1 |= RF1_QUESTOR;
					q_ptr->max_num = 1;
				}

				a_ptr = &a_info[q_ptr->k_idx];
				a_ptr->flags3 |= TR3_QUESTITEM;
			}
			return (0);
		}

		/* Process "Q:<q_index>:N:<name>" -- quest name */
		else if (zz[1][0] == 'N')
		{
			if (init_flags & (INIT_ASSIGN | INIT_SHOW_TEXT))
			{
				strcpy(q_ptr->name, zz[2]);
			}

			return (0);
		}

		/* Process "Q:<q_index>:T:<text>" -- quest description line */
		else if (zz[1][0] == 'T')
		{
			if (init_flags & INIT_SHOW_TEXT)
			{
				strcpy(quest_text[quest_text_line], zz[2]);
				quest_text_line++;
			}

			return (0);
		}
	}

	/* Process "P:<y>:<x>" -- player position */
	else if (buf[0] == 'P')
	{
		if (init_flags & INIT_CREATE_DUNGEON)
		{
			if (tokenize(buf + 2, 2, zz) == 2)
			{
#if 0
				int panels_x, panels_y;

				/* Hack - Set the dungeon size */
				panels_y = (*y / SCREEN_HGT);
				if (*y % SCREEN_HGT) panels_y++;
				cur_hgt = panels_y * SCREEN_HGT;

				panels_x = (*x / SCREEN_WID);
				if (*x % SCREEN_WID) panels_x++;
				cur_wid = panels_x * SCREEN_WID;

				/* Choose a panel row */
				max_panel_rows = (cur_hgt / SCREEN_HGT) * 2 - 2;
				if (max_panel_rows < 0) max_panel_rows = 0;

				/* Choose a panel col */
				max_panel_cols = (cur_wid / SCREEN_WID) * 2 - 2;
				if (max_panel_cols < 0) max_panel_cols = 0;

				/* Assume illegal panel */
				panel_row = max_panel_rows;
				panel_col = max_panel_cols;
#endif /* 0 */
				/* Place player in a quest level */
				if (p_ptr->inside_quest || p_ptr->inside_arena)
				{
#if 0
					p_ptr->py = atoi(zz[0]);
					p_ptr->px = atoi(zz[1]);
#endif

					player_place(atoi(zz[0]), atoi(zz[1]));

					/* Delete the monster (if any) */
					delete_monster(p_ptr->py, p_ptr->px);
				}
				/* Place player in the town */
				else if (!p_ptr->oldpx && !p_ptr->oldpy)
				{
					p_ptr->oldpy = atoi(zz[0]);
					p_ptr->oldpx = atoi(zz[1]);
				}
			}
		}

		return (0);
	}

	/* Process "B:<Index>:<Command>:..." -- Building definition */
	else if (buf[0] == 'B')
	{
		return parse_line_building(buf);
	}

	/* Failure */
	return (PARSE_ERROR_GENERIC);
}


static char tmp[8];
static cptr variant = "ZANGBAND";


/*
 * Helper function for "process_dungeon_file()"
 */
static cptr process_dungeon_file_expr(char **sp, char *fp)
{
	cptr v;

	char *b;
	char *s;

	char b1 = '[';
	char b2 = ']';

	char f = ' ';

	/* Initial */
	s = (*sp);

	/* Skip spaces */
	while (isspace(*s)) s++;

	/* Save start */
	b = s;

	/* Default */
	v = "?o?o?";

	/* Analyze */
	if (*s == b1)
	{
		const char *p;
		const char *t;

		/* Skip b1 */
		s++;

		/* First */
		t = process_dungeon_file_expr(&s, &f);

		/* Oops */
		if (!*t)
		{
			/* Nothing */
		}

		/* Function: IOR */
		else if (streq(t, "IOR"))
		{
			v = "0";
			while (*s && (f != b2))
			{
				t = process_dungeon_file_expr(&s, &f);
				if (*t && !streq(t, "0")) v = "1";
			}
		}

		/* Function: AND */
		else if (streq(t, "AND"))
		{
			v = "1";
			while (*s && (f != b2))
			{
				t = process_dungeon_file_expr(&s, &f);
				if (*t && streq(t, "0")) v = "0";
			}
		}

		/* Function: NOT */
		else if (streq(t, "NOT"))
		{
			v = "1";
			while (*s && (f != b2))
			{
				t = process_dungeon_file_expr(&s, &f);
				if (*t && !streq(t, "0")) v = "0";
			}
		}

		/* Function: EQU */
		else if (streq(t, "EQU"))
		{
			v = "1";
			if (*s && (f != b2))
			{
				t = process_dungeon_file_expr(&s, &f);
			}
			while (*s && (f != b2))
			{
				p = t;
				t = process_dungeon_file_expr(&s, &f);
				if (*t && !streq(p, t)) v = "0";
			}
		}

		/* Function: LEQ */
		else if (streq(t, "LEQ"))
		{
			v = "1";
			if (*s && (f != b2))
			{
				t = process_dungeon_file_expr(&s, &f);
			}
			while (*s && (f != b2))
			{
				p = t;
				t = process_dungeon_file_expr(&s, &f);
				if (*t && (strcmp(p, t) > 0)) v = "0";
			}
		}

		/* Function: GEQ */
		else if (streq(t, "GEQ"))
		{
			v = "1";
			if (*s && (f != b2))
			{
				t = process_dungeon_file_expr(&s, &f);
			}
			while (*s && (f != b2))
			{
				p = t;
				t = process_dungeon_file_expr(&s, &f);
				if (*t && (strcmp(p, t) < 0)) v = "0";
			}
		}

		/* Oops */
		else
		{
			while (*s && (f != b2))
			{
				t = process_dungeon_file_expr(&s, &f);
			}
		}

		/* Verify ending */
		if (f != b2) v = "?x?x?";

		/* Extract final and Terminate */
		if ((f = *s) != '\0') *s++ = '\0';
	}

	/* Other */
	else
	{
		/* Accept all printables except spaces and brackets */
		while (isprint(*s) && !strchr(" []", *s)) ++s;

		/* Extract final and Terminate */
		if ((f = *s) != '\0') *s++ = '\0';

		/* Variable */
		if (*b == '$')
		{
			/* System */
			if (streq(b+1, "SYS"))
			{
				v = ANGBAND_SYS;
			}

			/* Graphics */
			else if (streq(b+1, "GRAF"))
			{
				v = ANGBAND_GRAF;
			}
#if 0
			else if (streq(b+1, "MONOCHROME"))
			{
				if (arg_monochrome)
					v = "ON";
				else
					v = "OFF";
			}
#endif
			/* Race */
			else if (streq(b+1, "RACE"))
			{
				v = p_name + rp_ptr->name;
			}

			/* Class */
			else if (streq(b+1, "CLASS"))
			{
				v = c_name + cp_ptr->name;
			}
#if 0
			/* Player name */
			else if (streq(b+1, "PLAYER"))
			{
				v = player_base;
			}
#endif
			/* Town */
			else if (streq(b+1, "TOWN"))
			{
				sprintf(tmp, "%d", p_ptr->plot_num);
				v = tmp;
			}

			/* Level */
			else if (streq(b+1, "LEVEL"))
			{
				sprintf(tmp, "%d", p_ptr->lev);
				v = tmp;
			}

			/* Current quest number */
			else if (streq(b+1, "QUEST_NUMBER"))
			{
				sprintf(tmp, "%d", p_ptr->inside_quest);
				v = tmp;
			}

			/* Number of last quest */
			else if (streq(b+1, "LEAVING_QUEST"))
			{
				sprintf(tmp, "%d", leaving_quest);
				v = tmp;
			}

			/* Leaving the dungeon by stairs? */
			else if (streq(b+1, "LEAVING_DUNGEON"))
			{
				sprintf(tmp, "%d", p_ptr->leaving_dungeon);
				v = tmp;
			}

			/* Quest status */
			else if (prefix(b+1, "QUEST"))
			{
				/* "QUEST" uses a special parameter to determine the number of the quest */
				sprintf(tmp, "%d", quest[atoi(b+6)].status);
				v = tmp;
			}

			/* Variant name */
			else if (streq(b+1, "VARIANT"))
			{
				v = variant;
			}
#if 0
			/* Wilderness */
			else if (streq(b+1, "WILDERNESS"))
			{
				if (vanilla_town)
					sprintf(tmp, "NONE");
				else if (lite_town)
					sprintf(tmp, "LITE");
				else
					sprintf(tmp, "NORMAL");
				v = tmp;
			}
#endif /* 0 */
		}

		/* Constant */
		else
		{
			v = b;
		}
	}

	/* Save */
	(*fp) = f;

	/* Save */
	(*sp) = s;

	/* Result */
	return (v);
}


errr process_dungeon_file(cptr name, int ymin, int xmin, int ymax, int xmax)
{
	FILE *fp;

	char buf[1024];

	int num = -1;

	errr err = 0;

	bool bypass = FALSE;

	int x = xmin, y = ymin;


	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_QEST, name);

	/* Open the file */
	fp = my_fopen(buf, "r");

	/* No such file */
	if (!fp) return (-1);


	/* Process the file */
	while (0 == my_fgets(fp, buf, 1024))
	{
		/* Count lines */
		num++;


		/* Skip "empty" lines */
		if (!buf[0]) continue;

		/* Skip "blank" lines */
		if (isspace(buf[0])) continue;

		/* Skip comments */
		if (buf[0] == '#') continue;


		/* Process "?:<expr>" */
		if ((buf[0] == '?') && (buf[1] == ':'))
		{
			char f;
			cptr v;
			char *s;

			/* Start */
			s = buf + 2;

			/* Parse the expr */
			v = process_dungeon_file_expr(&s, &f);

			/* Set flag */
			bypass = (streq(v, "0") ? TRUE : FALSE);

			/* Continue */
			continue;
		}

		/* Apply conditionals */
		if (bypass) continue;


		/* Process "%:<file>" */
		if (buf[0] == '%')
		{
			/* Process that file if allowed */
			(void)process_dungeon_file(buf + 2, ymin, xmin, ymax, xmax);

			/* Continue */
			continue;
		}


		/* Process the line */
		err = process_dungeon_file_aux(buf, ymin, xmin, ymax, xmax, &y, &x);

		/* Oops */
		if (err) break;
	}

	/* Errors */
	if (err)
	{
		cptr oops;

		/* Error string */
		oops = (((err > 0) && (err < PARSE_ERROR_MAX)) ? err_str[err] : "unknown");

		/* Oops */
		msg_format("Error %d (%s) at line %d of '%s'.", err, oops, num, name);
		msg_format("Parsing '%s'.", buf);
		message_flush();
	}


	/* Close the file */
	my_fclose(fp);

	/* Result */
	return (err);
}


