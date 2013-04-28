/* File: icon1.c */

/* Purpose: icon stuff */

/*
 * Copyright (c) 1997-2009 Tim Baker
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include <tcl.h>
#include "angband.h"
#include "tnb.h"
#include "interp.h"
#include "cmdinfo-dll.h"
#include "util-dll.h"
#include "icon.h"
#include "widget.h"

#include <sys/stat.h>

extern int AngbandTk_CmdChooseFont _ANSI_ARGS_((ClientData clientData,
	 Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]));

unsigned char *g_palette_rgb;
t_assign_group g_assign[ASSIGN_MAX];
t_assign g_assign_none;
t_grid *g_grid[DUNGEON_HGT] = {0};
int g_grid_xtra_init = 0;
t_flavor *g_flavor = NULL; /* Array of flavor types */
int g_flavor_count = 0; /* Number of flavors */
Tcl_HashTable g_flavor_table; /* Map flavor name to g_flavor[] index */
t_sprite *g_sprite = NULL; /* Array of sprites */
int g_sprite_count;  /* Number of sprites */
t_alternate *g_alternate = NULL; /* Array of alternate icon info */
int g_alternate_count;  /* Number of elems in g_alternate[] array */
t_effect *g_effect = NULL; /* Array of effect icon info */
int *g_feat_lite = NULL;
int *g_background = NULL;
byte *g_feat_flag = NULL;
t_darken g_darken[3];
TintTable g_yellow;
t_assign *g_icon_map[ICON_LAYER_MAX][DUNGEON_HGT];
t_grid_effect *g_grid_effect[DUNGEON_HGT];
t_assign *g_assignshape[GRID_SHAPE_MAX] = {NULL};
bool g_assignshape_inuse = FALSE;
bool g_icon_map_changed = FALSE;
int g_torchlite; /* Use 4 levels of torch lite */
int *g_image_monster, *g_image_object;
bool g_daytime = FALSE; /* Day or night */

cptr keyword_wall[] = { "not", "single", "ns", "we", "corner_nw",
	"corner_ne", "corner_sw", "corner_se", "tri_n", "tri_s",
	"tri_w", "tri_e", "quad", NULL };

/* #define USE_STARTUP_LOG */

#ifdef USE_STARTUP_LOG

static FILE *startup_fp = NULL;

static void startup_log_open(void)
{
	char path[1024];

	path_build(path, 1024, ANGBAND_DIR_ROOT, "startup-bin.log");
	startup_fp = fopen(path, "w");
}

static void startup_log_close(void)
{
	if (startup_fp)
		fclose(startup_fp);
}

void startup_log(char *fmt, ...)
{
	va_list args;
    char buf[512];

	if (!startup_fp) return;

	va_start(args, fmt);
	vsprintf(buf, fmt, args);
	va_end(args);
   
	fprintf(startup_fp, "%s\n", buf);
	fflush(startup_fp);
}

#endif /* USE_STARTUP_LOG */

/*
 * Calculate the light radius for this floor grid
 */
void angtk_view_floor(int y, int x, int info, int torch)
{
	/* Special lighting effects */
	if (view_special_lite)
	{
		/* Handle "blind" */
		if (p_ptr->blind)
		{
			/* Use "dark gray" */
			g_grid[y][x].dark = GRID_LITE_DARK;
		}

		/* Handle "seen" grids */
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)
		else if (info & (CAVE_SEEN))
#endif /* ANGBANDTK, KANGBANDTK */
#if defined(ZANGBANDTK)
		else if (info & (CAVE_VIEW))
#endif /* ZANGBANDTK */
		{
			/* Only lit by "torch" lite */
			if (view_yellow_lite && torch)
			{
				/* Use "yellow" */
				g_grid[y][x].dark = GRID_LITE_TORCH;
			}

			else
			{
				g_grid[y][x].dark = GRID_LITE_NORMAL;
			}
		}

		/* Handle "dark" grids */
		else if (!(info & (CAVE_GLOW)))
		{
			/* Use "dark gray" */
			g_grid[y][x].dark = GRID_LITE_DARK;
		}

		/* Handle "view_bright_lite" */
		else if (view_bright_lite)
		{
			/* Use "gray" */
			g_grid[y][x].dark = GRID_LITE_DARK;
		}
	}
	else
	{
		g_grid[y][x].dark = GRID_LITE_TORCH;
	}
}

/*
 * Calculate the light radius for this wall grid
 */
void angtk_view_wall(int y, int x, int info, int torch)
{
	/* Special lighting effects (walls only) */
	if (view_granite_lite)
	{
		/* Handle "blind" */
		if (p_ptr->blind)
		{
			/* Use "dark gray" */
			g_grid[y][x].dark = GRID_LITE_DARK;
		}

		/* Handle "seen" grids */
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)
		else if (info & (CAVE_SEEN))
#endif /* ANGBANDTK, KANGBANDTK */
#if defined(ZANGBANDTK)
		else if (info & (CAVE_VIEW))
#endif /* ZANGBANDTK */
		{
			if (view_yellow_lite && torch)
			{
				g_grid[y][x].dark = GRID_LITE_TORCH;
			}

			else
			{
				g_grid[y][x].dark = GRID_LITE_NORMAL;
			}
		}

		/* Handle "view_bright_lite" */
		else if (view_bright_lite)
		{
			/* Use "gray" */
			g_grid[y][x].dark = GRID_LITE_DARK;
		}
	}
	else
	{
		g_grid[y][x].dark = GRID_LITE_TORCH;
	}
}

#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)

/*
 * These next 2 routines are here only because I want to "flicker" the
 * character's light when the fuel is running low. Previous to 2.8.3,
 * there were 2 routines, forget_lite() and update_lite().
 */

static void forget_lite(void)
{
	int y, x, r;

	p_ptr->cur_lite = 0;
	update_view();

	r = p_ptr->old_lite;
	for (y = p_ptr->py - r; y <= p_ptr->py + r; y++)
	{
		for (x = p_ptr->px - r; x <= p_ptr->px + r; x++)
		{
			angtk_wipe_spot(y, x);
		}
	}
}

static void update_lite(void)
{
	int y, x, r;

	p_ptr->cur_lite = p_ptr->old_lite;
	update_view();

	r = p_ptr->cur_lite;
	for (y = p_ptr->py - r; y <= p_ptr->py + r; y++)
	{
		for (x = p_ptr->px - r; x <= p_ptr->px + r; x++)
		{
			angtk_wipe_spot(y, x);
		}
	}
}

#endif /* ANGBANDTK, KANGBANDTK */

/*
 * Visually flicker the character's light source
 */
void angtk_flicker(void)
{
	unsigned long ms;

	ms = Milliseconds();
	forget_lite(); Term_fresh();
	Term_xtra(TERM_XTRA_DELAY, 60 - (Milliseconds() - ms));

	ms = Milliseconds();
	update_lite(); Term_fresh();
	Term_xtra(TERM_XTRA_DELAY, 60 - (Milliseconds() - ms));

	ms = Milliseconds();
	forget_lite(); Term_fresh();
	Term_xtra(TERM_XTRA_DELAY, 60 - (Milliseconds() - ms));

	update_lite(); Term_fresh();
}

/*
 * This routine fills the given t_grid struct with the indices of
 * any known feature, object or monster at the given cave location.
 * This routine does not consider light radius.
 */
void get_grid_info(int y, int x, t_grid *gridPtr)
{
	byte feat;
	byte info;

	s16b this_o_idx, next_o_idx;

	s16b m_idx;

	/*
	 * Hallucination: The original game assigns a random image to
	 * an object or monster, and sometimes (1 in 256) to a feature.
	 */

	gridPtr->f_idx = 0;
	gridPtr->o_idx = 0;
	gridPtr->m_idx = 0;

#ifdef ALLOW_PILE_IMAGE

	/*
	 * Note: This doesn't really depend on the easy_floor option.
	 * We could add another option just for this.
	 */

	/* Option: Use a unique icon for piles */
	if (easy_floor)
	{
		/* Forget the pile */
		gridPtr->xtra &= ~(GRID_XTRA_PILE);
	}

#endif /* ALLOW_PILE_IMAGE */

	/* Feature */
	feat = cave_feat(y, x);

#if defined(ZANGBANDTK)
	/* Apply "mimic" field */
	if (cave[y][x].mimic)
		feat = cave[y][x].mimic;
#endif /* ZANGBANDTK */

	/* Apply mimic field */
	feat = f_info[feat].mimic;

	/* Monster/Player */
	m_idx = cave_m_idx(y, x);

#if defined(ZANGBANDTK)
	if ((y == p_ptr_py) && (x == p_ptr_px))
		m_idx = -1;
#endif /* ZANGBANDTK */

	/* Handle "player" */
	if (m_idx < 0)
	{
		/* Remember the character index */
		gridPtr->m_idx = m_idx;

		/* Remember the feature index (for masked icon) */
		gridPtr->f_idx = feat;

		/* Done */
		return;
	}

	/* Cave flags */
	info = cave_info(y, x);

	/* Boring grids (floors, etc) */
	if (g_feat_flag[feat] & FEAT_FLAG_BORING)
	{
		/* Memorized (or seen) floor */
		if ((info & (CAVE_MARK)) ||
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)
		    (info & (CAVE_SEEN)))
#endif /* ANGBANDTK, KANGBANDTK */
#if defined(ZANGBANDTK)
		    (((info & (CAVE_LITE)) ||
		      ((info & (CAVE_GLOW)) &&
		       (info & (CAVE_VIEW)))) &&
		     !p_ptr->blind))
#endif /* ZANGBANDTK */
		{
			/* Remember the feature index */
			gridPtr->f_idx = feat;
		}

		/* Unknown */
		else
		{
			/* Nothing */
		}
	}

	/* Interesting grids (non-floors) */
	else
	{
		/* Memorized grids */
		if (info & (CAVE_MARK))
		{
			/* Remember the feature index */
			gridPtr->f_idx = feat;
		}

		/* Unknown */
		else
		{
			/* Nothing */
		}
	}

	/* Objects */
	for (this_o_idx = cave_o_idx(y, x); this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

		/* Acquire object */
		o_ptr = &o_list[this_o_idx];

		/* Acquire next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Memorized objects */
		if (!o_ptr->marked) continue;

#ifdef ALLOW_PILE_IMAGE

		/* Option: Use a unique icon for piles */
		if (easy_floor)
		{
			/* We already saw a marked object */
			if (gridPtr->o_idx)
			{
				/* This is a pile */
				gridPtr->xtra |= (GRID_XTRA_PILE);

				/* Stop */
				break;
			}

			/* Remember the top-most object index */
			gridPtr->o_idx = this_o_idx;

			/* Next */
			continue;
		}

#endif /* ALLOW_PILE_IMAGE */

		/* Remember the top-most object index */
		gridPtr->o_idx = this_o_idx;

		/* Stop */
		break;
	}

	/* Monsters */
	if (m_idx > 0)
	{
		/* Visible monster */
		if (m_list[m_idx].ml)
		{
			/* Remember the monster index */
			gridPtr->m_idx = m_idx;
		}
	}
}

/*
 * This should return TRUE if we are displaying the cave in fullbright.
 * Can't simply check the time of day and dungeon level because of the
 * messy indeterminate period when leaving the current level for the
 * next.
 */
int is_daytime(void)
{
	return g_daytime;
}

/*
 * This is the routine that determines the actual icon(s) used to
 * represent the given cave location. We use the global g_grid[] array
 * to first determine what monster/object/feature is visible at the
 * given location. Then we read the assignment for the specific monster/
 * object/feature from the g_assign[] array.
 */
void get_display_info(int y, int x, t_display *displayPtr)
{
	int m_idx, o_idx, f_idx;

	/* Access the global cave memory */
	t_grid *gridPtr = &g_grid[y][x];

	/* Get the darkness factor */
	int dark = gridPtr->dark;

	/* Determine if there is daylight in the town */
	int daytime = is_daytime();

	int layer;

	t_assign assign;
	IconSpec iconSpec;

	m_idx = gridPtr->m_idx;
	o_idx = gridPtr->o_idx;
	f_idx = gridPtr->f_idx;

	/* The grid is completely uninteresting */
	if (!m_idx && !o_idx && !f_idx)
	{
		/* All other values are uninitialized */
		displayPtr->blank = TRUE;

		/* Done */
		return;
	}

	/* */
	displayPtr->blank = FALSE;
	displayPtr->tint = NULL;
	displayPtr->anim = FALSE;

	if (m_idx || o_idx)
	{
		/* Character */
		if (m_idx == -1)
		{
			int k = 0;

			/*
			 * Currently only one icon is assigned to the character. We
			 * could allow different icons to be used depending on the
			 * state of the character (badly wounded, invincible, etc).
			 */

#if defined(OANGBANDTK)
			/* OAngbandTk allows an icon for each shape */
			if (SCHANGE)
				k = p_ptr->schange;
#endif /* */

			assign = g_assign[ASSIGN_CHARACTER].assign[k];
		}

		/* Monster */
		else if (m_idx > 0)
		{
			/* Get the monster race */
			int r_idx = m_list[m_idx].r_idx;

			/* XXX Hack -- Hallucination */
			if (p_ptr->image)
			{
				/* Get a random monster race */
				r_idx = g_image_monster[r_idx];
			}

			/* Get the icon assigned to the monster race */
			assign = g_assign[ASSIGN_MONSTER].assign[r_idx];
		}

		/* Object */
		else if (o_idx)
		{
#ifdef ALLOW_PILE_IMAGE

			/* This a pile of objects */
			if (easy_floor && (gridPtr->xtra & GRID_XTRA_PILE))
			{
				/* Get the icon assigned to object kind zero */
				assign = g_assign[ASSIGN_OBJECT].assign[0];
			}

			/* Not a pile */
			else
			{
				/* Get the object kind */
				int k_idx = o_list[o_idx].k_idx;

				/* XXX Hack -- Hallucination */
				if (p_ptr->image)
				{
					/* Get a random object kind */
					k_idx = g_image_object[k_idx];
				}

				/* Get the icon assigned to the object kind */
				assign = g_assign[ASSIGN_OBJECT].assign[k_idx];
			}

#else /* ALLOW_PILE_IMAGE */

			/* Get the object kind */
			int k_idx = o_list[o_idx].k_idx;

			/* Get the icon assigned to the object kind */
			assign = g_assign[ASSIGN_OBJECT].assign[k_idx];

#endif /* ALLOW_PILE_IMAGE */
		}

		/*
		 * Now we have the assignment for the character, monster, or object.
		 * The assignment may be TYPE_ALTERNATE, TYPE_FLAVOR, or
		 * TYPE_SPRITE, which we must resolve into a "real" icon type and
		 * index (for example, the current frame of a sprite).
		 *
		 * XXX TYPE_ALTERNATE is currently used only for objects and
		 * features, but feature assignments must already be resolved
		 * in set_grid_assign(). That's why TYPE_ALTERNATE is only
		 * checked for objects (see above).
		 */

		switch (assign.assignType)
		{
			/*
			 * TYPE_ALTERNATE assignments use one of two icons,
			 * depending on some property of the object.
			 */
			case ASSIGN_TYPE_ALTERNATE:
			{
				/* Access the alternate */
				t_alternate *alternatePtr = &g_alternate[assign.alternate.index];

				/* Default to the first frame */
				int index = 0;

				switch (alternatePtr->reason)
				{
					case REASON_NONE:
						break;

					case REASON_NUMBER:
						if (o_idx == 0)
							dbwin("requesting REASON_NUMBER but o_idx==0");
						else if (o_list[o_idx].number == 1) ++index;
						break;

					case REASON_IDENT:
						if (o_idx == 0)
							dbwin("requesting REASON_IDENT but o_idx==0");
						else if (object_known_p(&o_list[o_idx])) ++index;
						break;

					case REASON_FRIEND:
						if (m_idx <= 0)
							dbwin("requesting REASON_FRIEND but m_idx<=0");
						else if (monster_is_friend(&m_list[m_idx])) ++index;
						break;
				}

				/* Get the type and index of the frame */
				iconSpec = alternatePtr->icon[index];
				break;
			}

			/* Resolve flavor */
			case ASSIGN_TYPE_FLAVOR:
			{
				/* Access the flavor */
				t_flavor *flavor = &g_flavor[assign.flavor.group];

				/* Get the flavor index */
				int index = flavor->sorted[assign.flavor.index];

				/* Get the icon */
				iconSpec = flavor->icon[index];
				break;
			}

			/* Resolve icon */
			case ASSIGN_TYPE_ICON:
			{
				iconSpec.type = assign.icon.type;
				iconSpec.index = assign.icon.index;
				iconSpec.ascii = assign.icon.ascii;

				/* XXX Hack -- Multi-hued ascii icons are animated */
				if (iconSpec.ascii != -1)
				{
					if (g_ascii[iconSpec.ascii].mode != ASCII_NORMAL)
					{
						/* This grid is animated */
						displayPtr->anim = TRUE;
					}
					if (m_idx > 0 &&
						g_ascii[iconSpec.ascii].altFriend != -1 &&
						monster_is_friend(&m_list[m_idx]))
					{
						iconSpec.ascii = g_ascii[iconSpec.ascii].altFriend;
					}
				}
				break;
			}

			/* Resolve sprite */
			case ASSIGN_TYPE_SPRITE:
			{
				/* Access the sprite */
				t_sprite *spritePtr = &g_sprite[assign.sprite.index];

				/* Get the current frame */
				if ((spritePtr->frame >= 0) && (spritePtr->count > spritePtr->frame))
					iconSpec = spritePtr->icon[spritePtr->frame];
				else
				{
					iconSpec.type = ICON_TYPE_DEFAULT;
					iconSpec.index = 0;
					iconSpec.ascii = -1;
				}

				/* This grid is animated */
				displayPtr->anim = TRUE;
			}
		}
	}

	/* No character, monster or object */
	else
	{
		iconSpec.type = ICON_TYPE_NONE;
	}

	/* Set the foreground icon */
	iconSpec.dark = 0;
	displayPtr->fg = iconSpec;

	/* Don't bother calculating background if foreground is not masked */
#if 1 /* Aug 12 2004 */
	if ((g_icon_type[iconSpec.type].rle_data == NULL) &&
		(g_icon_type[iconSpec.type].width == g_icon_width) &&
		(g_icon_type[iconSpec.type].height == g_icon_height))
#else
	if (g_icon_type[iconSpec.type].rle_data == NULL)
#endif
	{
		/* Other layers uninitialized */
		displayPtr->bg[ICON_LAYER_1].type = ICON_TYPE_NONE;

		/* Done */
		return;
	}

	/* The feature is not known */
	if (f_idx == 0)
	{
		/* Other layers uninitialized */
		displayPtr->bg[ICON_LAYER_1].type = ICON_TYPE_BLANK;

		/* Done */
		return;
	}

	if (g_icon_map_changed)
	{
		int y, x;

		for (y = 0; y < DUNGEON_HGT; y++)
		{
			for (x = 0; x < DUNGEON_WID; x++)
			{
				set_grid_assign(y, x);
			}
		}
		g_icon_map_changed = FALSE;
	}

	/* Option: Use 4 levels of lighting. */
	if (g_torchlite)
	{
		/* Grid is lit by the character's light source */
		if (dark == GRID_LITE_TORCH)
		{
			/* Calculate distance from py,px */
			dark = MAX(ABS(x - p_ptr_px), ABS(y - p_ptr_py)) - 1;

			/* We may have dark == -1 at py,px */
			if (dark < 0) dark = 0;

			/* Light radius > 3 not allowed */
			else if (dark > 3) dark = 3;
		}

		/* Grid isn't lit by character's light source */
		else
		{
			/* Maximum darkness */
			dark = 3;
		}
	}

	/* In the Town */
	if (daytime)
	{
		/* Use bright light */
		dark = 0;
	}

	/*
	 * Get the icon from the global icon map. The g_icon_map[]
	 * array allows us to use different icons for the same
	 * feature index. For example, doors may be vertical or
	 * horizontal, and some walls are actually pillars.
	 */
	for (layer = 0; layer < ICON_LAYER_MAX; layer++)
	{
		assign = g_icon_map[layer][y][x];

		/* Resolve sprite */
		if (assign.assignType == ASSIGN_TYPE_SPRITE)
		{
			/* Access the sprite */
			t_sprite *spritePtr = &g_sprite[assign.sprite.index];

			/* Get the type and index of the current frame */
			if ((spritePtr->frame >= 0) && (spritePtr->count > spritePtr->frame))
				iconSpec = spritePtr->icon[spritePtr->frame];
			else
			{
				iconSpec.type = ICON_TYPE_DEFAULT;
				iconSpec.index = 0;
				iconSpec.ascii = -1;
			}

			/* This grid is animated */
			displayPtr->anim = TRUE;
		}

		/* Must be an icon */
		else
		{
			iconSpec.type = assign.icon.type;
			iconSpec.index = assign.icon.index;
			iconSpec.ascii = assign.icon.ascii;

			/* Only layer 1 is required */
			if (iconSpec.type == ICON_TYPE_NONE)
			{
				displayPtr->bg[layer] = iconSpec;
				break;
			}

			/* XXX Hack -- Multi-hued ascii icons are animated */
			if (iconSpec.ascii != -1)
			{
				if (g_ascii[iconSpec.ascii].mode != ASCII_NORMAL)
				{
					/* This grid is animated */
					displayPtr->anim = TRUE;
				}
			}
		}
#if 0
		/*
		 * Note that TYPE_ALTERNATE assignments must already have
		 * been resolved in set_grid_assign() or this will bomb.
		 * And TYPE_SPRITE assignments will bomb if lighting is
		 * FT_LIGHT_ICON and not FT_LIGHT_TINT.
		 */

		if (dark)
		{
			/* Examine the lighting mode for this feature */
			switch (g_feat_lite[f_idx])
			{
				/* Use icon series for lighting */
				case FT_LIGHT_ICON:

					/* Paranoia: only icons use light */
					if (assign.assignType == ASSIGN_TYPE_ICON)
					{
						if (iconSpec.type > ICON_TYPE_DEFAULT)
							iconSpec.index += dark;
					}
					break;

				/* Use tint table for lighting (slow) */
				case FT_LIGHT_TINT:
					if (g_icon_depth == 8)
						displayPtr->tint = g_darken[dark-1].table;
					break;
			}
		}
#endif

		/* A darkened copy of the icon exists, or will exist */
		if ((g_icon_type[iconSpec.type].dark_data &&
			g_icon_type[iconSpec.type].dark_data[iconSpec.index]) ||
			(g_icon_type[iconSpec.type].flags[iconSpec.index] & ICON_FLAG_DARK))
		{
			iconSpec.dark = dark;
		}
		else
		{
			iconSpec.dark = 0;
		}

		displayPtr->bg[layer] = iconSpec;
	}
}

bool is_wall(int y, int x)
{
	int f_idx = cave_feat(y, x);

#if defined(ZANGBANDTK)

	/* Apply "mimic" field */
	if (cave[y][x].mimic)
		f_idx = cave[y][x].mimic;

#endif /* ZANGBANDTK */

	/* Apply mimic field */
	f_idx = f_info[f_idx].mimic;

	/* FIXME: wall for each variant */
	if ((f_idx == FEAT_SECRET) ||
		((f_idx >= FEAT_MAGMA) && (f_idx <= FEAT_PERM_SOLID)))
	{
		return TRUE;
	}
	return FALSE;
}

bool is_door(int y, int x)
{
	int f_idx = cave_feat(y, x);

#if defined(ZANGBANDTK)

	/* Apply "mimic" field */
	if (cave[y][x].mimic)
		f_idx = cave[y][x].mimic;

#endif /* ZANGBANDTK */

	/* Apply mimic field */
	f_idx = f_info[f_idx].mimic;

	if (f_idx == FEAT_OPEN || f_idx == FEAT_BROKEN)
		return TRUE;
	if (f_idx >= FEAT_DOOR_HEAD && f_idx <= FEAT_DOOR_TAIL)
		return TRUE;
	return FALSE;
}

int wall_shape(int y, int x, bool force)
{
	int i, j;
	bool wall[3][3];
	int n = 0, nswe = 0;
	int col0n = 0, col2n = 0;
	int row0n = 0, row2n = 0;
	int shape;

	if (!in_bounds_test(y, x))
		return GRID_SHAPE_NOT;

	/* Require knowledge unless forced */
	if (!force && !(cave_info(y, x) & CAVE_MARK))
		return GRID_SHAPE_NOT;

	/* Require wall or secret door */
	if (!(g_grid[y][x].xtra & GRID_XTRA_WALL))
		return GRID_SHAPE_NOT;

	for (j = 0; j < 3; j++)
	{
		int yy = y - 1 + j;

		for (i = 0; i < 3; i++)
		{
			int xx = x - 1 + i;
			bool known;

			if ((i == 1) && (j == 1))
				continue;

			if (!in_bounds_test(yy, xx))
			{
				wall[j][i] = FALSE;
				continue;
			}

			known = force || ((cave_info(yy, xx) & CAVE_MARK) != 0);

			wall[j][i] = known &&
				(g_grid[yy][xx].xtra & (GRID_XTRA_WALL | GRID_XTRA_DOOR));

			if (wall[j][i])
			{
				++n;
				if (!i)
					++col0n;
				else if (i == 1)
					++nswe;
				else if (i == 2)
					++col2n;

				if (!j)
					++row0n;
				else if (j == 1)
					++nswe;
				else if (j == 2)
					++row2n;
			}
		}
	}

	/* Surrounded */
	if (n == 8)
	{
		return GRID_SHAPE_SINGLE;
	}

	/* Single, or no wall to N, S, W, or E */
	if (!n || (!nswe))
	{
		/* A solitary wall */
		if (!force)
		{
			/* Get the actual shape */
			int shape = wall_shape(y, x, TRUE);

			/* Not actually single */
			if (shape != GRID_SHAPE_SINGLE)
			{
				switch (shape)
				{
					case GRID_SHAPE_NS:
					case GRID_SHAPE_WE:
						return shape;
					case GRID_SHAPE_CORNER_NW:
					case GRID_SHAPE_CORNER_NE:
					case GRID_SHAPE_CORNER_SW:
					case GRID_SHAPE_CORNER_SE:
						return shape;
					case GRID_SHAPE_TRI_N:
					case GRID_SHAPE_TRI_S:
						return GRID_SHAPE_WE;
					case GRID_SHAPE_TRI_W:
					case GRID_SHAPE_TRI_E:
						return GRID_SHAPE_NS;
					case GRID_SHAPE_QUAD:
						return GRID_SHAPE_NS; /* FIXME: corner instead? */
				}
			}
		}

		return GRID_SHAPE_SINGLE;
	}

#if 1

	shape = GRID_SHAPE_NOT;

	if (nswe == 4)
	{
		shape = GRID_SHAPE_QUAD;

		if (n < 6) return shape;

		if (n == 6)
		{
			if (row0n == 3)
				shape = GRID_SHAPE_TRI_S;
			else if (row2n == 3)
				shape = GRID_SHAPE_TRI_N;
			else if (col0n == 3)
				shape = GRID_SHAPE_TRI_E;
			else if (col2n == 3)
				shape = GRID_SHAPE_TRI_W;

			return shape;
		}

		/* 7 grids, so must be corner */
		if (!wall[0][0])
			shape = GRID_SHAPE_CORNER_SE;
		else if (!wall[2][0])
			shape = GRID_SHAPE_CORNER_NE;
		else if (!wall[0][2])
			shape = GRID_SHAPE_CORNER_SW;
		else
			shape = GRID_SHAPE_CORNER_NW;

		return shape;
	}

	if (nswe == 3)
	{
		/* Walls to N and S */
		if (wall[0][1] && wall[2][1])
		{
			if (col0n == 3 || col2n == 3)
				shape = GRID_SHAPE_NS;
			else if (wall[1][0])
				shape = GRID_SHAPE_TRI_W;
			else
				shape = GRID_SHAPE_TRI_E;
		}

		/* Walls to W and E */
		else
		{
			if (row0n == 3 || row2n == 3)
				shape = GRID_SHAPE_WE;
			else if (wall[0][1])
				shape = GRID_SHAPE_TRI_N;
			else
				shape = GRID_SHAPE_TRI_S;
		}

		return shape;
	}

	if (nswe == 2)
	{
		if (wall[0][1] && wall[2][1])
			shape = GRID_SHAPE_NS;
		if (wall[1][0] && wall[1][2])
			shape = GRID_SHAPE_WE;

		if (wall[0][1] && wall[1][0])
			shape = GRID_SHAPE_CORNER_SE;
		if (wall[0][1] && wall[1][2])
			shape = GRID_SHAPE_CORNER_SW;
		if (wall[2][1] && wall[1][0])
			shape = GRID_SHAPE_CORNER_NE;
		if (wall[2][1] && wall[1][2])
			shape = GRID_SHAPE_CORNER_NW;

		return shape;
	}

	if (nswe == 1)
	{
		if (wall[0][1] || wall[2][1])
			shape = GRID_SHAPE_NS;
		else
			shape = GRID_SHAPE_WE;

		return shape;
	}

	/* Should never happen */
	return shape;

#else /* not 1 */

	/* One surrounding grid missing */
	if (n == 7)
	{
		/* Not missing N, S, W or E */
		if (nswe == 4)
		{
			/* Corner */
			if (!wall[0][0])
				return GRID_SHAPE_CORNER_SE;
			if (!wall[2][0])
				return GRID_SHAPE_CORNER_NE;
			if (!wall[0][2])
				return GRID_SHAPE_CORNER_SW;
			return GRID_SHAPE_CORNER_NW;
		}

		/* Flat */
		if (!wall[0][1] || !wall[2][1])
			return GRID_SHAPE_WE;

		return GRID_SHAPE_NS;
	}

	/* Two missing grids */
	if (n == 6)
	{
		/* 2 corners missing, so TRI or QUAD */
		if (nswe == 4)
		{
			if (row0n == 3)
				return GRID_SHAPE_TRI_S;
			if (row2n == 3)
				return GRID_SHAPE_TRI_N;
			if (col0n == 3)
				return GRID_SHAPE_TRI_E;
			if (col2n == 3)
				return GRID_SHAPE_TRI_W;

			/* Diagonally oposite corners missing */
			return GRID_SHAPE_QUAD;
		}

		if (nswe == 2)
		{
			if (wall[0][1])
				return GRID_SHAPE_NS;
			return GRID_SHAPE_WE;
		}

		/* 2 in same row/col missing, so NS or WE */
		if (row0n == 1 || row2n == 1)
			return GRID_SHAPE_WE;
		if (col0n == 1 || col2n == 1)
			return GRID_SHAPE_NS;

		/* 1 in different row/col missing, so TRI */
		if (row0n == 3 || row2n == 3)
			return wall[1][0] ? GRID_SHAPE_TRI_W : GRID_SHAPE_TRI_E;
		return wall[0][1] ? GRID_SHAPE_TRI_N : GRID_SHAPE_TRI_S;
	}

	if (n == 5)
	{
		/* Three corners missing */
		if (nswe == 4)
			return GRID_SHAPE_QUAD;

		if (!row0n || !row2n)
			return GRID_SHAPE_WE;
		if (!col0n || !col2n)
			return GRID_SHAPE_NS;
	}

	if (nswe == 4)
	{
		return GRID_SHAPE_QUAD;
	}

	if (nswe == 3)
	{
		if (wall[0][1] && wall[2][1])
			return wall[1][0] ? GRID_SHAPE_TRI_W : GRID_SHAPE_TRI_E;
		return wall[0][1] ? GRID_SHAPE_TRI_N : GRID_SHAPE_TRI_S;
	}

	if (n == 3)
	{
		/* Sticks out from wall */
		if (row0n == 3 || row2n == 3)
			return GRID_SHAPE_NS;
		if (col0n == 3 || col2n == 3)
			return GRID_SHAPE_WE;
	}

	if (nswe == 2)
	{
		if (wall[0][1] && wall[2][1])
			return GRID_SHAPE_NS;
		if (wall[1][0] && wall[1][2])
			return GRID_SHAPE_WE;

		if (wall[0][1] && wall[1][0])
			return GRID_SHAPE_CORNER_SE;
		if (wall[0][1] && wall[1][2])
			return GRID_SHAPE_CORNER_SW;
		if (wall[2][1] && wall[1][0])
			return GRID_SHAPE_CORNER_NE;
		/* if (wall[2][1] && wall[1][2]) */
			return GRID_SHAPE_CORNER_NW;
	}

	if (nswe == 1)
	{
		if (wall[0][1] || wall[2][1])
			return GRID_SHAPE_NS;
		return GRID_SHAPE_WE;
	}

	/* Only 2 touching, neither are N, S, W or E */
	if (n == 2)
	{
		if (wall[0][1] || wall[2][1])
			return GRID_SHAPE_NS;
		return GRID_SHAPE_WE;
	}

	/* Only 1 touching */
	if (n == 1)
	{
		if (wall[0][1] || wall[2][1])
			return GRID_SHAPE_NS;
		return GRID_SHAPE_WE;
	}

	return GRID_SHAPE_SINGLE;

#endif /* not 1 */
}

/*
 * Called when knowledge about many grids changes.
 */
void angtk_cave_changed(void)
{
	int y, x;
	DoubleLink *link;

	/* Do not use g_cave_hgt/_wid */

	for (y = 0; y < DUNGEON_HGT; y++)
	{
		for (x = 0; x < DUNGEON_WID; x++)
		{
			get_grid_info(y, x, &g_grid[y][x]);
			g_grid[y][x].shape = wall_shape(y, x, FALSE);
			set_grid_assign(y, x);
			map_symbol_set(y, x);
		}
	}

	/* May 4 2009 */
	for (link = WidgetListMapped.head; link; link = link->next)
	{
		Widget *widgetPtr = DoubleLink_Data(link, Widget);
		Widget_Wipe(widgetPtr);
	}
}

/*
 * Updates knowledge about a grid, including shape.
 * Called when a dungeon feature changes.
 */
void angtk_feat_changed(int y, int x)
{
	int d, shape;

	/* Forget walls and doors */
	g_grid[y][x].xtra &= ~(GRID_XTRA_DOOR | GRID_XTRA_WALL);

	/* Forget shape */
	g_grid[y][x].shape = 0;

	/* A wall (or secret door) */
	if (is_wall(y, x))
	{
		/* Note wall */
		g_grid[y][x].xtra |= GRID_XTRA_WALL;

		/* Get shape */
		g_grid[y][x].shape = wall_shape(y, x, FALSE);
	}

	/* A door */
	else if (is_door(y, x))
	{
		/* Note door */
		g_grid[y][x].xtra |= GRID_XTRA_DOOR;
	}

	set_grid_assign(y, x);

	for (d = 0; d < 8; d++)
	{
		int yy = y + ddy_ddd[d];
		int xx = x + ddx_ddd[d];

		if (in_bounds_test(yy, xx) && (g_grid[yy][xx].xtra & GRID_XTRA_WALL))
		{
			shape = wall_shape(yy, xx, FALSE);
			if (shape != g_grid[yy][xx].shape)
			{
				g_grid[yy][xx].shape = shape;
				set_grid_assign(yy, xx);

				if (character_dungeon &&
					g_assignshape_inuse)
				{
					widget_invalidate_shape(yy, xx);
				}
			}
		}
	}
}

/*
 * Sets the shape for the given grid and surrounding grids.
 * Called when a grid becomes known.
 */
void angtk_feat_known(int y, int x)
{
	int d, shape;

	if (!(g_grid[y][x].xtra & (GRID_XTRA_WALL | GRID_XTRA_DOOR)))
		return;

	if (g_grid[y][x].xtra & GRID_XTRA_WALL)
	{
		shape = wall_shape(y, x, FALSE);
		if (shape != g_grid[y][x].shape)
		{
			g_grid[y][x].shape = shape;
			set_grid_assign(y, x);
		}
	}

	/* A wall or door affects shape of surrounding grids */
	for (d = 0; d < 8; d++)
	{
		int yy = y + ddy_ddd[d];
		int xx = x + ddx_ddd[d];

		if (in_bounds_test(yy, xx) && (g_grid[yy][xx].xtra & GRID_XTRA_WALL))
		{
			shape = wall_shape(yy, xx, FALSE);
			if (shape != g_grid[yy][xx].shape)
			{
				g_grid[yy][xx].shape = shape;
				set_grid_assign(yy, xx);

				if (character_dungeon &&
					g_assignshape_inuse)
				{
					widget_invalidate_shape(yy, xx);
				}
			}
		}
	}
}

/*
 * This routine determines the icon to use for the given cave
 * location. It is called after the dungeon is created or loaded
 * from the savefile, and whenever a feature changes. It handles
 * the possible TYPE_ALTERNATE assignments used to display doors
 * and pillars. It handles any special vault icons as well.
 */
void set_grid_assign(int y, int x)
{
	int feat = cave_feat(y, x);
	t_assign assign, assignArray[ICON_LAYER_MAX];
	IconSpec iconSpec;
	int layer, shape;

	/* The dungeon isn't ready yet */
	if (!character_dungeon) return;

	/* Paranoia */
	if (g_icon_map[ICON_LAYER_1][0] == NULL) return;

#if defined(ZANGBANDTK)

	/* Apply "mimic" field */
	if (cave[y][x].mimic)
		feat = cave[y][x].mimic;

#endif /* ZANGBANDTK */

	/* Apply mimic field */
	feat = f_info[feat].mimic;

g_grid[y][x].xtra &= ~0x0001;

	/* If there is a custom vault for this level, use it */
	if (vault_icon(y, x, TRUE, assignArray))
	{
		for (layer = 0; layer < ICON_LAYER_MAX; layer++)
		{
			g_icon_map[layer][y][x] = assignArray[layer];
		}
		return;
	}

	/* Get the assignment for this feature */
	assign = g_assign[ASSIGN_FEATURE].assign[feat];

	/* Get the shape of this grid */
	shape = g_grid[y][x].shape;

	/* If a door, set shape now */
	if (is_door(y, x)) /* XTRA_DOOR */
	{
		if (g_grid[y][x].xtra & GRID_XTRA_ISVERT)
			shape = GRID_SHAPE_NS;
		else
			shape = GRID_SHAPE_WE;
	}

	/* A valid shape */
	if ((shape > 0) && (shape < GRID_SHAPE_MAX))
	{
		t_assign assign2;

		/* Get the assignment for this shape */
		assign2 = g_assignshape[shape][feat];

		/* This is not a TYPE_DEFAULT icon */
		if ((assign2.assignType != ASSIGN_TYPE_ICON) ||
			(assign2.icon.type != ICON_TYPE_DEFAULT))
		{
			/* Use the shape icon */
			assign = assign2;
		}

		/*
		 * XXX Hack -- Remember if there is a second shape assignment.
		 * The second assignment uses an "unknown" floor.
		 */
		assign2 = g_assignshape[shape][MAX_F_IDX + feat];
		if ((assign2.assignType != ASSIGN_TYPE_ICON) ||
			(assign2.icon.type != ICON_TYPE_DEFAULT))
		{
			/* Use the shape icon */
			g_grid[y][x].xtra |= 0x0001;
		}
	}

	if (assign.assignType == ASSIGN_TYPE_ALTERNATE)
	{
		t_alternate *alternatePtr = &g_alternate[assign.alternate.index];
		int index;

		/* This is a door */
		if ((feat == FEAT_OPEN) || (feat == FEAT_BROKEN) ||
			((feat >= FEAT_DOOR_HEAD) && (feat <= FEAT_DOOR_TAIL)))
		{
			/* The reason must be REASON_FEATURE */
			/* Index 0 is horizontal door, 1 is vertical door */
			index = (g_grid[y][x].xtra & GRID_XTRA_ISVERT) != 0;
		}

		/* This is a granite wall, or a magma/quartz stream */
		/* Note: INNER/OUTER/SOLID granite mimics EXTRA */
		else if ((feat == FEAT_WALL_EXTRA) || (feat == FEAT_MAGMA) ||
			(feat == FEAT_QUARTZ))
		{
			/* The reason must be REASON_FEATURE */
			/* Index 0 is normal granite wall, 1 is pillar */
			index = (g_grid[y][x].xtra & GRID_XTRA_PILLAR) != 0;
		}

		/* This is a floor */
		else if (feat == FEAT_FLOOR)
		{
			/* The reason must be REASON_FEATURE */
			/* Index 0 is hallway, 1 is floor */
			index = (cave_info(y, x) & CAVE_ROOM) != 0;
		}

		/* Strangeness */
		else
		{
			index = 0;
		}

		iconSpec = alternatePtr->icon[index];

		assign.assignType = ASSIGN_TYPE_ICON;
		assign.icon.type = iconSpec.type;
		assign.icon.index = iconSpec.index;
		assign.icon.ascii = iconSpec.ascii;
	}

	/* Remember the icon in the global icon map */
	g_icon_map[ICON_LAYER_1][y][x] = assign;

	layer = ICON_LAYER_2;

	if (feat != g_background[feat])
	{
		/* Swap foreground & background */
		g_icon_map[ICON_LAYER_2][y][x] = assign;

		feat = g_background[feat];
		assign = g_assign[ASSIGN_FEATURE].assign[feat];

		if (assign.assignType == ASSIGN_TYPE_ALTERNATE)
		{
			t_alternate *alternatePtr = &g_alternate[assign.alternate.index];
			int index;

			/* This is a door */
			if ((feat == FEAT_OPEN) || (feat == FEAT_BROKEN) ||
				((feat >= FEAT_DOOR_HEAD) && (feat <= FEAT_DOOR_TAIL)))
			{
				/* The reason must be REASON_FEATURE */
				/* Index 0 is horizontal door, 1 is vertical door */
				index = (g_grid[y][x].xtra & GRID_XTRA_ISVERT) != 0;
			}

			/* This is a granite wall, or a magma/quartz stream */
			/* Note: INNER/OUTER/SOLID granite mimics EXTRA */
			else if ((feat == FEAT_WALL_EXTRA) || (feat == FEAT_MAGMA) ||
				(feat == FEAT_QUARTZ))
			{
				/* The reason must be REASON_FEATURE */
				/* Index 0 is normal granite wall, 1 is pillar */
				index = (g_grid[y][x].xtra & GRID_XTRA_PILLAR) != 0;
			}

			/* This is a floor */
			else if (feat == FEAT_FLOOR)
			{
				/* The reason must be REASON_FEATURE */
				/* Index 0 in hallway, 1 in room */
				index = (cave_info(y, x) & CAVE_ROOM) != 0;
			}

			/* Strangeness */
			else
			{
				index = 0;
			}

			iconSpec = alternatePtr->icon[index];

			assign.assignType = ASSIGN_TYPE_ICON;
			assign.icon.type = iconSpec.type;
			assign.icon.index = iconSpec.index;
			assign.icon.ascii = iconSpec.ascii;
		}

		/* Swap foreground & background */
		g_icon_map[ICON_LAYER_1][y][x] = assign;

		/* Wipe layers 3 & 4 */
		layer = ICON_LAYER_3;
	}

	/* Wipe the remaining layers */
	for (; layer < ICON_LAYER_MAX; layer++)
	{
		g_icon_map[layer][y][x] = g_assign_none;
	}
}

/*
 * Determine the icon type/index of a real icon type from the given
 * icon type/index. This routine is used to get the actual frame of
 * a sprite for example. Usually the given icon type/index is returned.
 */
void FinalIcon(IconSpec *iconOut, t_assign *assignPtr, int hack,
		object_type *o_ptr, monster_type *m_ptr)
{
	switch (assignPtr->assignType)
	{
		case ASSIGN_TYPE_ALTERNATE:
		{
			t_alternate *alternatePtr = &g_alternate[assignPtr->alternate.index];
			int index = 1;

			/* An object */
			if (o_ptr)
			{
				switch (alternatePtr->reason)
				{
					case REASON_NUMBER:
						if (o_ptr->number != 1) index = 0;
						break;

					case REASON_IDENT:
						if (!object_known_p(o_ptr)) index = 0;
						break;
				}
			}

			/* A monster */
			if (m_ptr)
			{
				switch (alternatePtr->reason)
				{
					case REASON_FRIEND:
						if (!monster_is_friend(m_ptr)) index = 0;
						break;
				}
			}

			/* XXX Hack -- Vault wants first icon */
			else if (hack == 1)
			{
				index = 0;
			}
			(*iconOut) = alternatePtr->icon[index];
#if 0
			iconOut->type = alternatePtr->icon[1].type;
			iconOut->index = alternatePtr->icon[1].index;
			iconOut->ascii = alternatePtr->icon[1].ascii;
#endif
			break;
		}

		case ASSIGN_TYPE_FLAVOR:
		{
			t_flavor *flavorPtr = &g_flavor[assignPtr->flavor.group];
			int index = flavorPtr->sorted[assignPtr->flavor.index];
			(*iconOut) = flavorPtr->icon[index];
#if 0
			iconOut->type = flavorPtr->icon[index].type;
			iconOut->index = flavorPtr->icon[index].index;
			iconOut->ascii = flavorPtr->icon[index].ascii;
#endif
			break;
		}

		case ASSIGN_TYPE_ICON:
			iconOut->type = assignPtr->icon.type;
			iconOut->index = assignPtr->icon.index;
			iconOut->ascii = assignPtr->icon.ascii;
			if (m_ptr != NULL &&
				iconOut->ascii != -1 &&
				g_ascii[iconOut->ascii].altFriend != -1)
			{
				iconOut->ascii = g_ascii[iconOut->ascii].altFriend;
			}
			break;

		case ASSIGN_TYPE_SPRITE:
		{
			t_sprite *spritePtr = &g_sprite[assignPtr->sprite.index];
			if ((spritePtr->frame >= 0) && (spritePtr->count > spritePtr->frame))
				(*iconOut) = spritePtr->icon[spritePtr->frame];
			else
			{
				iconOut->type = ICON_TYPE_DEFAULT;
				iconOut->index = 0;
				iconOut->ascii = -1;
			}
#if 0
			iconOut->type = spritePtr->icon[spritePtr->frame].type;
			iconOut->index = spritePtr->icon[spritePtr->frame].index;
			iconOut->ascii = spritePtr->icon[spritePtr->frame].ascii;
#endif
			break;
		}
	}
}

int read_dark_file(char *fileName)
{
	FILE *fp;
	int count, table_count, index[16], i;
	char buf[80];

	if ((fp = fopen(fileName, "r")) == NULL)
	{
		return TCL_ERROR;
	}

	count = 0;
	table_count = 0;

	/* Parse the file */
	while (!feof(fp))
	{
		/* Read a line */
		if (!fgets(buf, 80, fp)) continue;

		/* Skip comments */
		if (buf[0] == '#') continue;

		/* Scan 16 values */
		if (sscanf(buf, "%d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d ",
			&index[0], &index[1], &index[2], &index[3], &index[4],
			&index[5], &index[6], &index[7], &index[8], &index[9],
			&index[10], &index[11], &index[12], &index[13], &index[14],
			&index[15]) != 16) continue;

		/* Remember 16 values */
		for (i = 0; i < 16; i++)
		{
			g_darken[table_count].table[count++] = index[i];
		}

		/* Next table */
		if (count == 256)
		{
			table_count++;
			count = 0;
		}

		/* Error */
		if (table_count > 3) break;
	}

	/* Close the file */
	fclose(fp);

	if (table_count != 3) return TCL_ERROR;

	/* Success */
	return TCL_OK;
}

#ifdef USE_HERMES
#include "Hermes.h"
HermesHandle g_hermes_conv;
HermesHandle g_hermes_palette;
#endif /* USE_HERMES */

void init_palette(void)
{
	char path[1024], path2[1024];
	int i;

	/*
	 * The colors in this 256-color palette are indexed by each byte
	 * of icon data.
	 */
	path_build(path2, 1024, ANGBAND_DIR_TK, "config");
path_build(path2, 1024, ANGBAND_DIR_COMMON_TK, "config");
	path_build(path, 1024, path2, "palette_256");

	if (Palette_Init(g_interp, path) != TCL_OK)
		quit(Tcl_GetStringResult(g_interp));

	g_palette_rgb = Palette_GetRGB();

	/*
	 * Tint table for light effect.
	 */
	for (i = 0; i < 3; i++)
	{
		g_darken[i].brightness = 0;
		g_darken[i].contrast = 0;
		g_darken[i].gamma = 1.0;
		Colormap_One2OneTable(g_darken[i].table);
	}
	path_build(path2, 1024, ANGBAND_DIR_TK, "config");
path_build(path2, 1024, ANGBAND_DIR_COMMON_TK, "config");
	path_build(path, 1024, path2, "dark");
	if (read_dark_file(path) != TCL_OK)
	{
		quit_fmt("error reading \"%s\"", path);
	}

#ifdef USE_HERMES
	if (!Hermes_Init()) quit_fmt("Hermes_Init() failed");
	g_hermes_conv = Hermes_ConverterInstance(HERMES_CONVERT_NORMAL);
	if (!g_hermes_conv) quit_fmt("Hermes_ConverterInstance() failed");
	g_hermes_palette = Hermes_PaletteInstance();
	for (i = 0; i < 256; i++)
	{
		int32 *p = Hermes_PaletteGet(g_hermes_palette) + i;
		*p =
			g_palette_rgb[i * 3] << 16L |
			g_palette_rgb[i * 3 + 1] << 8L |
			g_palette_rgb[i * 3 + 2];
	}
#endif /* USE_HERMES */
}

/* 
 * Is the door horizontal or vertical?
 * Return 0 for horizontal, 1 for vertical
 */
bool door_vertical(int y, int x)
{
	bool wall_left, wall_right, wall_above, wall_below;
	bool door_right, door_left;
	int f_left, f_right;

#if 1 /* Nov 2 2004 */
	wall_left = is_wall(y, x-1);
	wall_right = is_wall(y, x+1);

	wall_above = is_wall(y-1, x);
	wall_below = is_wall(y+1, x);
#else /* cave_floor_bold() catches rubble too */
	wall_left = !cave_floor_bold(y, x-1);
	wall_right = !cave_floor_bold(y, x+1);

	wall_above = !cave_floor_bold(y-1, x);
	wall_below = !cave_floor_bold(y+1, x);
#endif

	/* Walls on both horizontal sides */
	if (wall_left && wall_right) return (0);

	/* Walls on both vertical sides */
	if (wall_above && wall_below) return (1);

	/* Check for doors on either horizontal side */
	f_left = cave_feat(y, x - 1);
	f_right = cave_feat(y, x + 1);

	/* Note we also check secret doors */
	door_left = (f_left == FEAT_OPEN) || (f_left == FEAT_BROKEN) ||
		(f_left >= FEAT_DOOR_HEAD && f_left <= FEAT_SECRET);
	door_right = (f_right == FEAT_OPEN) || (f_right == FEAT_BROKEN) ||
		(f_right >= FEAT_DOOR_HEAD && f_right <= FEAT_SECRET);

	/* Doors on left and right */
	if (door_left && door_right) return (0);

	/* Door on one side and wall on the other side */
	if ((door_left && wall_right) || (door_right && wall_left)) return (0);

	/* Default to vertical door (handles stacked vertical doors too!) */
	return (1);
}

#if 0
IconPtr SetIconBitsRLE(IconPtr bg, IconPtr fg, TintTable t, IconPtr b)
{
	IconPtr dst = b;
	int i;

	/* Tint */
	if (t)
	{
		for (i = 0; i < ICON_LENGTH; i++)
		{
			*dst++ = *(t + *bg++);
		}

		/* Return the address of the buffer we wrote into */
		return b;
	}

	/*
	 * In the simplest case (non-tinted), just return
	 * the given address of the icon data.
	 */
	return bg;
}

/*
 * bg -- background bits
 * fg -- foreground bits, or NULL
 * mk -- mask bits for fg, or also NULL
 * t  -- tint table, or NULL
 * b  -- result bits
 *
 * The tint could be applied to the fg + bg, or to the bg only. Since
 * tint tables are currently only used for lighting purposes, we tint
 * the background but not the foreground.
 *
 * This is not quite right. We should be able to tint the foreground
 * and/or the background separately. For example, if Rubble is masked,
 * the foreground should be tinted, and the background may or may not
 * be tinted (depending on whether the floor is tinted).
 */
IconPtr SetIconBits(IconPtr bg, IconPtr fg, IconPtr mk, TintTable t, IconPtr b)
{
	int i;

	/* Masked */
	if (fg)
	{
		/* Tint */
		if (t)
		{
			for (i = 0; i < ICON_LENGTH; i++)
			{
#if 0 /* pixel = tint(fg + bg) */

				*b++ = *(t + ((*bg++ & *mk++) | *fg++));

#else /* pixel = fg + tint(bg) */

				*b++ = (*(t + *bg++) & *mk++) | *fg++;
#endif
			}
		}

		/* No tint */
		else
		{
			for (i = 0; i < ICON_LENGTH; i++)
			{
				*b++ = (*bg++ & *mk++) | *fg++;
			}
		}
	}

	/* Not masked */
	else
	{
		/* Tint */
		if (t)
		{
			for (i = 0; i < ICON_LENGTH; i++)
			{
				*b++ = *(t + *bg++);
			}
		}

		/* No tint */
		else
		{
			/*
			 * In the simplest case (non-masked, non-tinted), just return
			 * the given address of the icon data.
			 */
			return bg;
		}
	}

	/* Return the address of the buffer we wrote into */
	return b - ICON_LENGTH;
}
#endif /* 0 */

int update_sprites(void)
{
	static unsigned long last_update = 0;
	static unsigned long last_call = 0;
	unsigned long tick_count, ticks;
	int i, update = 0;

	/* Get the system tick count */
	tick_count = Milliseconds();

	/* Don't update twice the same tick (not bloody likely) */
	if (tick_count == last_call) return 0;

	/* Remember the tick count */
	last_call = tick_count;

	/* Calculate the elapsed ticks */
	ticks = MIN(tick_count - last_update, 1000);

	/* No time has passed (yeah, right) */
	if (ticks == 0) return 0;

	/* Remember the tick count */
	last_update = tick_count;

	/* Check each sprite */
	for (i = 0; i < g_sprite_count; ++i)
	{
		/* Access the sprite */
		t_sprite *sprite = &g_sprite[i];

		/* Get the current frame */
		int frame = sprite->frame;

		/* Get the number of frames */
		int count = sprite->count;

		/* -1 backwards, 0 always forwards, 1 forwards */
		int reverse = sprite->reverse;

		/* Paranoia: require 2 frames */
		if (count < 2) continue;

		/* Another tick(s) added */
		sprite->ticks += ticks;

		/* The frame delay has been exceeded */
		if (sprite->ticks >= sprite->speed)
		{
			/* Reset tick counter */
			sprite->ticks = 0;

			/* This sprite always goes from first to last frame */
			if (reverse == 0)
			{
				/* Increase frame count. */
				++frame;

				/* Wrap around if needed */
				if (frame >= count) frame = 0;
			}

			/* Cycle forwards */
			else if (reverse > 0)
			{
				/* Increase frame count. */
				++frame;

				if (frame >= count)
				{
					/* Requires minimum 2 icons */
					frame = count - 2;

					/* Cycle in reverse now */
					reverse = -1;
				}
			}

			/* Cycle backwards */
			else
			{
				/* Note unsigned values */
				if (frame == 0)
				{
					/* Requires minimum 2 icons */
					frame = 1;

					/* Cycle forwards now */
					reverse = +1;
				}

				else
				{
					/* Decrease frame count. */
					--frame;
				}
			}

			/* Remember the current frame */
			sprite->frame = frame;

			/* Remember reverse status */
			sprite->reverse = reverse;

			/* Note it needs redrawing */
			sprite->changed = TRUE;

			/* A sprite changed */
			update = 1;
		}

		/* The sprite is unchanged */
		else
		{
			/* No redrawing needed */
			sprite->changed = FALSE;
		}
	}

	/* Increment the ascii-type frame delay */
	g_ascii_ticks += ticks;

	/* The ascii-type frame delay was exceeded */
	if (g_ascii_ticks >= g_ascii_delay)
	{
		/* ASCII_ATTR_MULTI uses one of 16 standard colors */
		g_ascii_multi = (g_ascii_multi + 1) % 16;

		/* ASCII_SHAPECHANGER uses random character */
		/* XXX This should be a valid monster/object char only */
		g_ascii_char = rand_int(126 - 32 + 1); /* printable only */

		/* Clear the tick counter */
		g_ascii_ticks = 0;

		/* We need to redraw */
		return TRUE;
	}

	return update;
}

/*
 * Return TRUE if the given icon type/index can be "animated"
 */
bool is_sprite(t_assign *assignPtr)
{
	/* Sprites are animated */
	if (assignPtr->assignType == ASSIGN_TYPE_SPRITE) return TRUE;

	/* */
	if (assignPtr->assignType != ASSIGN_TYPE_ICON) return FALSE;

	/* XXX Hack -- Multi-hued ascii icons are animated */
	if (assignPtr->icon.ascii != -1)
	{
		if (g_ascii[assignPtr->icon.ascii].mode != ASCII_NORMAL)
		{
			return TRUE;
		}
	}

	/* Nope */
	return FALSE;
}

/*
 * Recalculate the indices used for monsters and objects during
 * hallucination.
 */
void angtk_image_reset(void)
{
	int i;

	/* Randomize monsters */
	for (i = 1; i < MAX_R_IDX; i++)
	{
		int r_idx;
		monster_race *r_ptr;

		/* Infinite loop */
		while (1)
		{
			/* Pick a random non-unique */
			r_idx = randint(MAX_R_IDX - 1);

			/* Access the monster race */
			r_ptr = &r_info[r_idx];

			/* Require real race */
			if (!r_ptr->name) continue;

			/* Skip uniques */
			if (r_ptr->flags1 & RF1_UNIQUE) continue;

			/* Stop */
			break;
		}

		/* Remember the monster race */
		g_image_monster[i] = r_idx;
	}

	/* Randomize objects */
	for (i = 1; i < MAX_K_IDX; i++)
	{
		int k_idx;
		object_kind *k_ptr;

		/* Infinite loop */
		while (1)
		{
			/* Pick a random object kind */
			k_idx = randint(MAX_K_IDX - 1);

			/* Access the object kind */
			k_ptr = &k_info[k_idx];

			/* Require real kind */
			if (!k_ptr->name) continue;

			/* Skip artifacts */
			if (k_ptr->flags3 & (TR3_INSTA_ART)) continue;

			/* Stop */
			break;
		}

		/* Remember the object kind */
		g_image_object[i] = k_idx;
	}
}

/* Option: Animation */
bool allow_animation = TRUE;

/* Debugging: Calculate average time between animation updates */
#define TIMER_STATSxxx

#ifdef TIMER_STATS

static unsigned long _sum = 0, _count = 0, _last = 0;

/*
 * Debug command. You should find a cave location with some animation
 * going on, call "timer reset", wait a few seconds, then call
 * "timer average" to get the averate time between updates.
 *
 * Usage:
 *    timer average -- current delay between animation updates in ms
 *    timer reset   -- zero the timer statistics
 */
static int
objcmd_timer(ClientData dummy, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	static char *cmdOption[] = {"average", "reset", NULL};
	int index;

	/* Required number of arguments */
    if (objc < 2)
    {
		Tcl_WrongNumArgs(interp, 1, objv, "option ?arg ...?");
		return TCL_ERROR;
    }

	/* Get requested option */
    if (Tcl_GetIndexFromObj(interp, objv[1], cmdOption, "option", 0, 
		&index) != TCL_OK)
	{
		return TCL_ERROR;
    }

	switch (index)
	{
		case 0: /* average */
			if (_count == 0) _count = 1;
			IntResult(interp, _sum / _count);
			break;

		case 1: /* reset */
			_last = Milliseconds();
			_sum = _count = 0;
			break;
	}

	/* Success */
	return TCL_OK;
}

#endif /* TIMER_STATS */

/*
 * Animation timer (3 implementations):
 *
 * The first method uses Tcl_CreateTimerHandler() to repeatedly reschedule
 * our timer callback. The average delay between calls is about 57ms on my
 * machine.
 *
 * The second method calls the Win32 SetTimer() call with a custom window
 * class. The average delay is the same as the first method, unless there
 * is rapid mouse motion, in which case it drops to about 22ms.
 *
 * The third method uses SetTimer() without a custom window class. But
 * I'm not sure if it executes asynchronously, so I won't use it.
 *
 * Note that the last two implementations may no longer compile properly.
 */

/* Number of milliseconds between timer callback */
#define TIMER_TICKS 10

static Tcl_TimerToken g_timer_token = NULL;

/*
 * Tcl_CreateTimerHandler() callback
 */
static void AngbandTimerProc(ClientData clientData)
{
	/* Reschedule the animation timer */
	g_timer_token = Tcl_CreateTimerHandler(TIMER_TICKS, AngbandTimerProc, 0);

	/* No animation while repeating a command */
#if 1
	if (p_ptr_running || p_ptr_resting) return;

	/*
	 * If you look at do_cmd_bash() in ZAngband, command_rep is
	 * set before get_rep_dir() is called, disabling animation
	 * when asking for a direction. So if the command is repeated
	 * and we are not actively waiting for a key, then skip animation.
	 */
	if (p_ptr_command_rep &&
		(!inkey_flags || (inkey_flags == INKEY_DISTURB))) return;

#else
	if (p_ptr_running || p_ptr_command_rep || p_ptr_resting) return;
#endif

#ifdef TIMER_STATS

	if (_last == 0)
		_last = Milliseconds();
	else
	{
		_sum += Milliseconds() - _last;
		_last = Milliseconds();
		_count++;
	}

#endif /* TIMER_STATS */

	/* Perform animation */
	angtk_idle();
}

/*
 * One-time initialization for the animation timer
 */
static void init_timer(void)
{
#ifdef TIMER_STATS
	Tcl_CreateObjCommand(g_interp, "timer", objcmd_timer, NULL, NULL);
#endif /* TIMER_STATS */
}

/*
 * One-time cleanup for the animation timer
 */
static void free_timer(void)
{
	/* The timer was started */
	if (g_timer_token != NULL)
	{
		/* Cancel the timer */
		Tcl_DeleteTimerHandler(g_timer_token);

		/* Forget the timer */
		g_timer_token = NULL;
	}
}

/*
 * Start the animation timer
 */
void angtk_start_timer(void)
{
	/* The timer is already sheduled */
	if (g_timer_token != NULL) return;

	/* Shedule the timer */
	g_timer_token = Tcl_CreateTimerHandler(TIMER_TICKS, AngbandTimerProc, 0);
}

/*
 * Stop the animation timer
 */
void angtk_stop_timer(void)
{
	/* The timer is scheduled */
	if (g_timer_token != NULL)
	{
		/* Cancel the timer */
		Tcl_DeleteTimerHandler(g_timer_token);

		/* Forget the timer */
		g_timer_token = NULL;
	}

	/*
	 * XXX Hack -- When animation is disabled, use TERM_VIOLET for
	 * all multi-hued ascii type icons.
	 */
	g_ascii_multi = TERM_VIOLET;
}

void angtk_flavor_swap(int n, int a, int b)
{
	int tmp;

	tmp = g_flavor[n].sorted[a];
	g_flavor[n].sorted[a] = g_flavor[n].sorted[b];
	g_flavor[n].sorted[b] = tmp;
}

static int init_flavor_aux(int n, char *desc, int tval, int count,
	byte *color)
{
	Tcl_HashEntry *hPtr;
	t_flavor flavor;
	int i, dummy;
	IconSpec defaultIcon = {ICON_TYPE_DEFAULT, 0, -1};

	flavor.desc = (char *) string_make(desc);
	flavor.tval = tval;
	flavor.count = count;
	C_MAKE(flavor.sorted, count, int);
	C_MAKE(flavor.icon, count, IconSpec);
	C_MAKE(flavor.color, count, byte);

	for (i = 0; i < count; i++)
	{
		flavor.sorted[i] = i;
		flavor.color[i] = color[i];
		flavor.icon[i] = defaultIcon;
	}

	hPtr = Tcl_CreateHashEntry(&g_flavor_table, desc, &dummy);
	Tcl_SetHashValue(hPtr, (ClientData) n);
	g_flavor[n++] = flavor;

	return n;
}

void angtk_flavor_init(int *max, byte **attr)
{
	int n = 0;

#ifdef BORG_TK
	if (g_flavor != NULL) return;
#endif

#if 0
	if (init_flavor_txt())
		quit("error creating lib/data/flavor_col");
#endif

	C_MAKE(g_flavor, FLAVOR_MAX, t_flavor);
	g_flavor_count = FLAVOR_MAX;

	/*
	 * This table maps a flavor name to an index in the g_flavor[] array.
	 */
	Tcl_InitHashTable(&g_flavor_table, TCL_STRING_KEYS);

	/* Note: indexes are hard-coded by flavor_init() */
	n = init_flavor_aux(n, "amulet", TV_AMULET, max[n], attr[n]);
	n = init_flavor_aux(n, "mushroom", TV_FOOD,  max[n], attr[n]);
	n = init_flavor_aux(n, "potion", TV_POTION,  max[n], attr[n]);
	n = init_flavor_aux(n, "ring", TV_RING,  max[n], attr[n]);
	n = init_flavor_aux(n, "rod", TV_ROD,  max[n], attr[n]);
	n = init_flavor_aux(n, "staff", TV_STAFF,  max[n], attr[n]);
	(void) init_flavor_aux(n, "wand", TV_WAND,  max[n], attr[n]);
}

/* This must get called after every icons_unload() call */
void icons_setup(int width, int height, int depth, int style)
{
	int i, y, x, y2, x2;
	t_assign assign;
	t_icon_type icon_data, *iconTypePtr = &icon_data;
	IconSpec iconDefault = {ICON_TYPE_DEFAULT, 0, -1};
	unsigned char *rgb = Colormap_GetRGB();

	/* Setup the Icon library */
	if (Icon_Setup(g_interp, width, height, depth, style) != TCL_OK)
	{
		quit(Tcl_GetStringFromObj(Tcl_GetObjResult(g_interp), NULL));
	}

	/*
	 * The TYPE_NONE/"none" type icon is a single masked icon with an empty
	 * mask. It is suitable for equipment displays when no item is present
	 * in a slot.
	 */
	iconTypePtr->desc = "none";
	iconTypePtr->icon_count = 1;
	iconTypePtr->icon_data = (IconPtr) Tcl_AllocDebug(g_icon_length);
	iconTypePtr->char_table = NULL;
	iconTypePtr->font = NULL;
	for (i = 0; i < g_icon_length; i++)
	{
		iconTypePtr->icon_data[i] = 0x00;
	}

	iconTypePtr->dynamic = FALSE;
	iconTypePtr->depth = g_icon_depth;
	iconTypePtr->bypp = g_pixel_size;
	iconTypePtr->width = g_icon_width;
	iconTypePtr->height = g_icon_height;
	iconTypePtr->pitch = g_icon_width * g_pixel_size;
	iconTypePtr->length = g_icon_length;
	iconTypePtr->pixels = g_icon_pixels;

	Icon_AddType(iconTypePtr);

	g_icon_type[ICON_TYPE_NONE].rle_pixel = 0;
	Icon_MakeRLE(&g_icon_type[ICON_TYPE_NONE]);

	/*
	 * The TYPE_BLANK/"blank" icon type is a single black unmasked icon
	 */
	iconTypePtr->desc = "blank";
	iconTypePtr->icon_count = 1;
	iconTypePtr->icon_data = (IconPtr) Tcl_AllocDebug(g_icon_length);
	iconTypePtr->char_table = NULL;
	iconTypePtr->font = NULL;
	for (i = 0; i < g_icon_length; i++)
	{
		if (g_icon_depth != 8)
			iconTypePtr->icon_data[i] = 0; /* Black (RGB 0,0,0) */
		else
			iconTypePtr->icon_data[i] = COLORMAP_BLACK;
	}

	iconTypePtr->dynamic = FALSE;
	iconTypePtr->depth = g_icon_depth;
	iconTypePtr->bypp = g_pixel_size;
	iconTypePtr->width = g_icon_width;
	iconTypePtr->height = g_icon_height;
	iconTypePtr->pitch = g_icon_width * g_pixel_size;
	iconTypePtr->length = g_icon_length;
	iconTypePtr->pixels = g_icon_pixels;

	Icon_AddType(iconTypePtr);

	/*
	 * The TYPE_DEFAULT/"default" icon type is a single multicolored
	 * unmasked icon. If we see it, it probably means we forgot to
	 * assign an icon to something.
	 */
	iconTypePtr->desc = "default";
	iconTypePtr->icon_count = 1;
	iconTypePtr->icon_data = (IconPtr) Tcl_AllocDebug(g_icon_length);
	iconTypePtr->char_table = NULL;
	iconTypePtr->font = NULL;

	if (((g_icon_width == 16) || (g_icon_width == 24) ||
		(g_icon_width == 32)) && (g_icon_width == g_icon_height))
	{
		y2 = 0;
		for (y = 0; y < 16; y++)
		{
			int dy = 0;
			if (g_icon_width == 24)
				if (!(y & 1)) dy++;
			if (g_icon_width == 32) dy++;
			x2 = 0;
			for (x = 0; x < 16; x++)
			{
				int dx = 0;
				if (g_icon_width == 24)
					if (!(x & 1)) dx++;
				if (g_icon_width == 32) dx++;
				PixelSet_RGB(iconTypePtr->icon_data + (x2 * g_pixel_size) +
					(y2 * g_icon_width * g_pixel_size),
					rgb[0], rgb[1], rgb[2], g_pixel_size);
				PixelSet_RGB(iconTypePtr->icon_data + ((x2 + dx) * g_pixel_size) +
					(y2 * g_icon_width * g_pixel_size),
					rgb[0], rgb[1], rgb[2], g_pixel_size);
				PixelSet_RGB(iconTypePtr->icon_data + (x2 * g_pixel_size) +
					((y2 + dy) * g_icon_width * g_pixel_size),
					rgb[0], rgb[1], rgb[2], g_pixel_size);
				PixelSet_RGB(iconTypePtr->icon_data + ((x2 + dx) * g_pixel_size) +
					((y2 + dy) * g_icon_width * g_pixel_size),
					rgb[0], rgb[1], rgb[2], g_pixel_size);
				rgb += 3;
				x2 += dx ? 2 : 1;
			}
			y2 += dy ? 2 : 1;
		}
	}

	/* Strange icon size */
	else
	{
		y2 = 0;
		while (y2 < g_icon_height)
		{
			for (y = 0; y < 16; y++)
			{
				x2 = 0;
				while (x2 < g_icon_width)
				{
					for (x = 0; x < 16; x++)
					{
						PixelSet_RGB(iconTypePtr->icon_data +
							(x2 * g_pixel_size) +
							(y2 * g_icon_width * g_pixel_size),
							rgb[y * 16 * 3 + x * 3 + 0], rgb[y * 16 * 3 + x * 3 + 1],
							rgb[y * 16 * 3 + x * 3 + 2], g_pixel_size);
						x2 += 1;
						if (x2 == g_icon_width)
							break;
					}
				}
				y2 += 1;
				if (y2 == g_icon_height)
					break;
			}
		}
	}

	iconTypePtr->dynamic = FALSE;
	iconTypePtr->depth = g_icon_depth;
	iconTypePtr->bypp = g_pixel_size;
	iconTypePtr->width = g_icon_width;
	iconTypePtr->height = g_icon_height;
	iconTypePtr->pitch = g_icon_width * g_pixel_size;
	iconTypePtr->length = g_icon_length;
	iconTypePtr->pixels = g_icon_pixels;

	Icon_AddType(iconTypePtr);

	assign.assignType = ASSIGN_TYPE_ICON;
	assign.icon.type = ICON_TYPE_DEFAULT;
	assign.icon.index = 0;
	assign.icon.ascii = -1;

	/* Set default icon for the character */
	for (i = 0; i < g_assign[ASSIGN_CHARACTER].count; i++)
	{
		g_assign[ASSIGN_CHARACTER].assign[i] = assign;
	}

	/* Set default icon for each monster */
	for (i = 0; i < g_assign[ASSIGN_MONSTER].count; i++)
	{
		g_assign[ASSIGN_MONSTER].assign[i] = assign;
	}
	g_assign[ASSIGN_MONSTER].assign[0].icon.type = ICON_TYPE_NONE;

	/* Set default icon for each object */
	for (i = 0; i < g_assign[ASSIGN_OBJECT].count; i++)
	{
		g_assign[ASSIGN_OBJECT].assign[i] = assign;
	}
	g_assign[ASSIGN_OBJECT].assign[0].icon.type = ICON_TYPE_NONE;

	/* Set default icon for each feature */
	for (i = 0; i < g_assign[ASSIGN_FEATURE].count; i++)
	{
		g_assign[ASSIGN_FEATURE].assign[i] = assign;
		g_background[i] = i;
		g_feat_lite[i] = FT_LIGHT_NONE;
	}
	g_assign[ASSIGN_FEATURE].assign[FEAT_NONE].icon.type = ICON_TYPE_NONE;

	for (i = 0; i < GRID_SHAPE_MAX; i++)
	{
		int j;
		for (j = 0; j < MAX_F_IDX * 2; j++)
		{
			g_assignshape[i][j] = assign;
		}
	}
	g_assignshape_inuse = FALSE;

	/* Array of alternates */
	g_alternate = Array_New(0, sizeof(t_alternate));
	g_alternate_count = 0;

	/* Array of sprites */
	g_sprite = Array_New(0, sizeof(t_sprite));
	g_sprite_count = 0;

	/* Set default icon for each effect */
	for (i = 0; i < EFFECT_SPELL_MAX; i++)
	{
		/* Default ball icon */
		g_effect[EFFECT_SPELL_BALL].icon[i] = iconDefault;

		/* Default bolt icon (first of 4) */
		g_effect[EFFECT_SPELL_BOLT].icon[i] = iconDefault;
	}

	/* Set default icon for each effect */
	for (i = 0; i < EFFECT_AMMO_MAX; i++)
	{
		g_effect[EFFECT_AMMO].icon[i] = iconDefault;
	}

	/* Initialize the custom town display stuff */
	vault_setup();

	/* Clear the color hash table */
	Palette_ResetHash();

	/* Initialize a tint table for drawing torch light (NOT USED) */
	Palette_TintTable(5, 127, g_yellow);

	CanvasWidget_Setup();
	TreeCtrl_Setup();
	widget_setup();

	/* Now we can safely use lite_spot() */
	angtk_lite_spot = angtk_lite_spot_real;

	g_icon_map_changed = TRUE;
}

/* Must call icons_setup() after this */
void icons_unload(void)
{
	int i;

	if (g_alternate == NULL) return;

	CanvasWidget_Unload();
	TreeCtrl_Unload();
	vault_unload();

	Icon_Unload(g_interp);

	if (g_alternate)
	{
		for (i = 0; i < g_alternate_count; i++)
			if (g_alternate[i].icon)
				Tcl_FreeDebug(g_alternate[i].icon);
		Tcl_FreeDebug(g_alternate);
		g_alternate = NULL;
	}

	if (g_sprite)
	{
		for (i = 0; i < g_sprite_count; i++)
			if (g_sprite[i].icon)
				Tcl_FreeDebug(g_sprite[i].icon);
		Tcl_FreeDebug(g_sprite);
		g_sprite = NULL;
	}
}

/*
 * Initialize the icon environment. This should be called once with
 * the desired dimensions of the icons to use (16x16, 24x24 or 32x32).
 */
void icons_init(void)
{
	int i, n;

	/* Initialize the Icon library */
	if (Icon_Init(g_interp) != TCL_OK)
	{
		quit(Tcl_GetStringFromObj(Tcl_GetObjResult(g_interp), NULL));
	}

	/* Allocate array of t_assign for each monster */
	g_assign[ASSIGN_MONSTER].count = MAX_R_IDX;
	C_MAKE(g_assign[ASSIGN_MONSTER].assign, MAX_R_IDX, t_assign);

	/* Allocate array of t_assign for each object */
	g_assign[ASSIGN_OBJECT].count = MAX_K_IDX;
	C_MAKE(g_assign[ASSIGN_OBJECT].assign, MAX_K_IDX, t_assign);

	/* Allocate array of t_assign for the character */
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(ZANGBANDTK)
	n = 1;
#endif
#if defined(OANGBANDTK)
	/* OAngbandTk allows an icon for each shape */
	n = MAX_SHAPE;
#endif /* */
	g_assign[ASSIGN_CHARACTER].count = n;
	C_MAKE(g_assign[ASSIGN_CHARACTER].assign, n, t_assign);

	/* Allocate array of t_assign for each feature */
	g_assign[ASSIGN_FEATURE].count = MAX_F_IDX;
	C_MAKE(g_assign[ASSIGN_FEATURE].assign, MAX_F_IDX, t_assign);

	/*
	 * This is an array of t_icon types, one for every grid in
	 * the cave! The icons are only those assigned to features,
	 * never to monsters, objects, or the character. The icons
	 * are not calculated for visibility or light radius. This
	 * array allows different icons to be assigned to the same
	 * feature type. For example, doors are horizontal or vertical,
	 * and pillars have different icons, and the town has a varied
	 * set of icons.
	 */
	for (i = 0; i < DUNGEON_HGT; i++)
	{
		int layer;
		for (layer = 0; layer < ICON_LAYER_MAX; layer++)
			C_MAKE(g_icon_map[layer][i], DUNGEON_WID, t_assign);
	}

	/*
	 * A feature can use lighting in one of three ways:
	 * (1) FT_LIGHT_NONE means ignore lighting
	 * (2) FT_LIGHT_ICON means use a sequence of icons
	 * (3) FT_LIGHT_TINT means use the g_darken[] tint table (slow)
	 */
	C_MAKE(g_feat_lite, MAX_F_IDX, int);

	/*
	 * When a feature is masked, or a masked icon is drawn on
	 * a feature, we may use the icon assigned to a different feature
	 * as the background.
	 */
	C_MAKE(g_background, MAX_F_IDX, int);


	for (i = 0; i < GRID_SHAPE_MAX; i++)
	{
		C_MAKE(g_assignshape[i], MAX_F_IDX * 2, t_assign);
	}

	/* Array of effect info */
	C_MAKE(g_effect, EFFECT_MAX, t_effect);

	C_MAKE(g_effect[EFFECT_SPELL_BALL].icon, EFFECT_SPELL_MAX, IconSpec);
	C_MAKE(g_effect[EFFECT_SPELL_BOLT].icon, EFFECT_SPELL_MAX, IconSpec);
	C_MAKE(g_effect[EFFECT_AMMO].icon, EFFECT_AMMO_MAX, IconSpec);

	g_effect[EFFECT_SPELL_BALL].name = (char **) keyword_effect_spell;
	g_effect[EFFECT_SPELL_BOLT].name = (char **) keyword_effect_spell;
	g_effect[EFFECT_AMMO].name = (char **) keyword_effect_ammo;

	/* Add some new commands to the global interpreter */
	{
		extern CommandInit assignCmdInit[];
		(void) CommandInfo_Init(g_interp, assignCmdInit, NULL);
	}

#ifdef PLATFORM_WIN
	Tcl_CreateObjCommand(g_interp, "tk_chooseFont", AngbandTk_CmdChooseFont,
		NULL, NULL);
#endif /* PLATFORM_WIN */

	g_assign_none.assignType = ASSIGN_TYPE_ICON;
	g_assign_none.icon.type = ICON_TYPE_NONE;
	g_assign_none.icon.index = 0;
	g_assign_none.icon.ascii = -1;

	/* Hack -- indices for hallucination */
	C_MAKE(g_image_monster, MAX_R_IDX, int);
	C_MAKE(g_image_object, MAX_K_IDX, int);

	/* Randomize the hallucination indices */
	angtk_image_reset();

	/* Initialize the animation timer */
	init_timer();

#ifdef USE_STARTUP_LOG
	startup_log_open();
#endif /* USE_STARTUP_LOG */
}

/*
 * Clean up the icon environment
 */
void icons_exit(void)
{
	/* Make sure Tk fonts are freed or else */
	Icon_Exit(g_interp);

	/* Free the animation timer */
	free_timer();

#ifdef USE_STARTUP_LOG
	startup_log_close();
#endif /* USE_STARTUP_LOG */

	icons_unload();
}
