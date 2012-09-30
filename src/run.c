/* File: run.c */

/* Purpose: running code */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"
#include "script.h"

/*
 * Check for a "known wall"
 */
static int see_wall(int x, int y)
{
	pcave_type *pc_ptr;

	feature_type *f_ptr;

	/* Illegal grids are "walls" */
	if (!in_boundsp(x, y)) return (TRUE);

	pc_ptr = parea(x, y);

	f_ptr = &f_info[pc_ptr->feat];

	/* Return block-los status */
	return (f_ptr->flags & FF_BLOCK);
}


/*
 * Check for an "unknown corner"
 */
static int see_nothing(int x, int y)
{
	cave_type *c_ptr;
	pcave_type *pc_ptr;

	/* Illegal grids are unknown */
	if (!in_boundsp(x, y)) return (FALSE);

	c_ptr = area(x, y);
	pc_ptr = parea(x, y);

	/* Memorized grids are always known */
	if (pc_ptr->feat) return (FALSE);

	/* Non-floor grids are unknown */
	if (cave_wall_grid(c_ptr)) return (TRUE);

	/* Viewable door/wall grids are known */
	if (player_can_see_grid(pc_ptr)) return (FALSE);

	/* Default */
	return (TRUE);
}

/*
 * Check for "interesting" terrain
 */
static int see_interesting(int x, int y)
{
	cave_type *c_ptr;
	pcave_type *pc_ptr;
	object_type *o_ptr;

	/* Illegal grids are boring */
	if (!in_boundsp(x, y)) return (FALSE);

	c_ptr = area(x, y);
	pc_ptr = parea(x, y);

	/* Check for a monster */
	if (c_ptr->m_idx)
	{
		monster_type *m_ptr = &m_list[c_ptr->m_idx];

		/* If it's visible, it's interesting */
		if (m_ptr->ml) return (TRUE);
	}

	/* Check for objects */
	OBJ_ITT_START(c_ptr->o_idx, o_ptr)
	{
		/* If it's visible, it's interesting */
		if (o_ptr->info & OB_SEEN) return (TRUE);
	}
	OBJ_ITT_END;

	/* Check for traps */
	if (is_visible_trap(c_ptr)) return (TRUE);

	/* Check for building */
	if (is_build(c_ptr)) return (TRUE);

	/* Check memorized grids */
	if (pc_ptr->feat)
	{
		switch (c_ptr->feat)
		{
		case FEAT_DEEP_LAVA:
		case FEAT_DEEP_ACID:
		case FEAT_DEEP_SWAMP:
			return (TRUE);

		case FEAT_SHAL_SWAMP:
		case FEAT_SHAL_ACID:
		case FEAT_SHAL_LAVA:
		/* Water */
		case FEAT_DEEP_WATER:
		case FEAT_OCEAN_WATER:
			/* Levitation makes these feats boring */
			if (FLAG(p_ptr, TR_FEATHER)) break;

			return (TRUE);

		/* Open doors */
		case FEAT_OPEN:
		case FEAT_BROKEN:
			if (find_ignore_doors) break;

			return (TRUE);

		/* Closed doors */
		case FEAT_CLOSED:
			return (TRUE);

		/* Stairs */
		case FEAT_LESS:
		case FEAT_MORE:
			if (find_ignore_stairs) break;

			return (TRUE);
		}
	}

	/* Boring */
	return (FALSE);
}


#define RUN_MODE_START    0  /* Beginning of run */
#define RUN_MODE_OPEN     1  /* Running in a room or open area */
#define RUN_MODE_CORRIDOR 2  /* Running in a corridor */
#define RUN_MODE_WALL     3  /* Running along a wall */
#define RUN_MODE_FINISH   4  /* End of run */

/* 
 * Due to a fortunate coincidence, there are exactly 32 interesting
 * 2-move combinations. We only consider combinations where the
 * second move shares at least one component (N, E, S or W) with
 * the first move.
 *
 * Since there are 32, we can use bit fields to simplify the tables.
 */
#define RUN_N_NW  0x00000001L
#define RUN_N_N   0x00000002L
#define RUN_N_NE  0x00000004L
#define RUN_NE_NW 0x00000008L
#define RUN_NE_N  0x00000010L
#define RUN_NE_NE 0x00000020L
#define RUN_NE_E  0x00000040L
#define RUN_NE_SE 0x00000080L
#define RUN_E_NE  0x00000100L
#define RUN_E_E   0x00000200L
#define RUN_E_SE  0x00000400L
#define RUN_SE_NE 0x00000800L
#define RUN_SE_E  0x00001000L
#define RUN_SE_SE 0x00002000L
#define RUN_SE_S  0x00004000L
#define RUN_SE_SW 0x00008000L
#define RUN_S_SE  0x00010000L
#define RUN_S_S   0x00020000L
#define RUN_S_SW  0x00040000L
#define RUN_SW_SE 0x00080000L
#define RUN_SW_S  0x00100000L
#define RUN_SW_SW 0x00200000L
#define RUN_SW_W  0x00400000L
#define RUN_SW_NW 0x00800000L
#define RUN_W_SW  0x01000000L
#define RUN_W_W   0x02000000L
#define RUN_W_NW  0x04000000L
#define RUN_NW_SW 0x08000000L
#define RUN_NW_W  0x10000000L
#define RUN_NW_NW 0x20000000L
#define RUN_NW_N  0x40000000L
#define RUN_NW_NE 0x80000000L

/* All the moves that start with each direction */
#define RUN_N  (RUN_N_NW  | RUN_N_N  | RUN_N_NE)
#define RUN_NE (RUN_NE_NW | RUN_NE_N | RUN_NE_NE | RUN_NE_E | RUN_NE_SE)
#define RUN_E  (RUN_E_NE  | RUN_E_E  | RUN_E_SE)
#define RUN_SE (RUN_SE_NE | RUN_SE_E | RUN_SE_SE | RUN_SE_S | RUN_SE_SW)
#define RUN_S  (RUN_S_SE  | RUN_S_S  | RUN_S_SW)
#define RUN_SW (RUN_SW_SE | RUN_SW_S | RUN_SW_SW | RUN_SW_W | RUN_SW_NW)
#define RUN_W  (RUN_W_SW  | RUN_W_W  | RUN_W_NW)
#define RUN_NW (RUN_NW_SW | RUN_NW_W | RUN_NW_NW | RUN_NW_N | RUN_NW_NE)

#define TEST_NONE     0
#define TEST_WALL     1
#define TEST_UNSEEN   2
#define TEST_FLOOR    3
#define TEST_FLOOR_S  4

/*
 * The tests.
 *
 * Some of these are hard to understand. Don't worry too
 * much, unless you really need to modify the table.
 *
 * The order the groups of checks are in is important!
 * Don't reorder them unless you're sure you know what
 * you're doing. The order within each group is arbitrary.
 *
 * Each test consists of:
 *
 *   What type of square test to do (if any), and what
 *   square to test as an offset from the player.
 *
 *   What other moves must be valid (0 for none). If there
 *   are multiple moves listed, the test passes if any of
 *   the listed moves are valid.
 *
 *   What moves to remove from consideration if both the
 *   checks listed pass.
 */
static const struct
{
	int test;
	int dx, dy;
	u32b test_mask;
	u32b remove_mask;
} run_checks[] = {

/*
 * If the player starts out by moving into a branch corridor:
 *
 * ###    ###
 * b.@ -> b..
 * #.#    #@#
 * #a#    #a#
 *
 * we should realize that the player means to continue to the square marked
 * 'a', and we should ignore the possibility of moving to 'b' instead.
 *
 * To recognize this, we check for the presence of a floor tile that the
 * player might have come from next to, and remove any moves that would
 * result in "doubling back".
 *
 * This test is done *before* removing impossible moves, because
 * we need to know which direction the player came from.
 */
{TEST_FLOOR_S,  0, -1, RUN_S, RUN_NE | RUN_NW},
{TEST_FLOOR_S,  1,  0, RUN_W, RUN_NE | RUN_SE},
{TEST_FLOOR_S,  0,  1, RUN_N, RUN_SE | RUN_SW},
{TEST_FLOOR_S, -1,  0, RUN_E, RUN_NW | RUN_SW},

/* Eliminate impossible moves */
{TEST_WALL, -2, -2, 0,        RUN_NW_NW},
{TEST_WALL, -1, -2, 0,        RUN_NW_N | RUN_N_NW},
{TEST_WALL,  0, -2, 0,        RUN_NW_NE | RUN_N_N | RUN_NE_NW},
{TEST_WALL,  1, -2, 0,        RUN_NE_N | RUN_N_NE},
{TEST_WALL,  2, -2, 0,        RUN_NE_NE},
{TEST_WALL,  2, -1, 0,        RUN_NE_E | RUN_E_NE},
{TEST_WALL,  2,  0, 0,        RUN_NE_SE | RUN_E_E | RUN_SE_NE},
{TEST_WALL,  2,  1, 0,        RUN_SE_E | RUN_E_SE},
{TEST_WALL,  2,  2, 0,        RUN_SE_SE},
{TEST_WALL,  1,  2, 0,        RUN_SE_S | RUN_S_SE},
{TEST_WALL,  0,  2, 0,        RUN_SE_SW | RUN_S_S | RUN_SW_SE},
{TEST_WALL, -1,  2, 0,        RUN_SW_S | RUN_S_SW},
{TEST_WALL, -2,  2, 0,        RUN_SW_SW},
{TEST_WALL, -2,  1, 0,        RUN_SW_W | RUN_W_SW},
{TEST_WALL, -2,  0, 0,        RUN_SW_NW | RUN_W_W | RUN_NW_SW},
{TEST_WALL, -2, -1, 0,        RUN_NW_W | RUN_W_NW},
{TEST_WALL, -1, -1, 0,        RUN_NW},
{TEST_WALL,  0, -1, 0,        RUN_N},
{TEST_WALL,  1, -1, 0,        RUN_NE},
{TEST_WALL,  1,  0, 0,        RUN_E},
{TEST_WALL,  1,  1, 0,        RUN_SE},
{TEST_WALL,  0,  1, 0,        RUN_S},
{TEST_WALL, -1,  1, 0,        RUN_SW},
{TEST_WALL, -1,  0, 0,        RUN_W},

/*
 * Allow the player to run in a pillared corridor with
 * a radius-2 light source, by removing some diagonal
 * moves into unknown squares. Example:
 *
 *  #.#
 * #...#
 * ##@##
 *
 * Note that this, like the previous set, can result
 * in missing an actual (but unusual) branch.
 * Generally the player will see the branch once we
 * move another step, but by that point it's too
 * late, so we keep going forward.
 */
{TEST_UNSEEN, -2, -2, RUN_N | RUN_W, RUN_NW_NW},
{TEST_UNSEEN,  2, -2, RUN_N | RUN_E, RUN_NE_NE},
{TEST_UNSEEN,  2,  2, RUN_S | RUN_E, RUN_SE_SE},
{TEST_UNSEEN, -2,  2, RUN_S | RUN_W, RUN_SW_SW},

/*
 * Ensure that the player will take unknown corners by
 * preventing orthagonal moves to unknown squares when
 * a diagonal move exists.
 *
 * Example:
 *
 *  ##
 *  .@
 *  .#
 *
 * In this situation we remove all the 'west' moves, because
 * they are to unknown squares, and it's possible to move
 * southwest instead.
 *
 * Note that this can miss a branch if the unknown square
 * is actually a floor square and cutting corners is
 * enabled.
 *
 * We have to be careful that we differentiate between these
 * two situations:
 *
 * ###       ##
 *  .@  vs   .@
 * #.#       .#
 *
 * The first we should NOT turn because there is obviously
 * a real branch. We handle this by (indirectly) checking
 * for the presence of a wall in the "unknown" direction,
 * in this case the wall SWW of the player.
 */
{TEST_UNSEEN, -1, -2, RUN_NE_N | RUN_NW_N, RUN_N_NW},
{TEST_UNSEEN,  0, -2, RUN_NE_N | RUN_NW_N, RUN_N_N},
{TEST_UNSEEN,  1, -2, RUN_NE_N | RUN_NW_N, RUN_N_NE},
{TEST_UNSEEN,  2, -1, RUN_NE_E | RUN_SE_E, RUN_E_NE},
{TEST_UNSEEN,  2,  0, RUN_NE_E | RUN_SE_E, RUN_E_E},
{TEST_UNSEEN,  2,  1, RUN_NE_E | RUN_SE_E, RUN_E_SE},
{TEST_UNSEEN,  1,  2, RUN_SE_S | RUN_SW_S, RUN_S_SE},
{TEST_UNSEEN,  0,  2, RUN_SE_S | RUN_SW_S, RUN_S_S},
{TEST_UNSEEN, -1,  2, RUN_SE_S | RUN_SW_S, RUN_S_SW},
{TEST_UNSEEN, -2,  1, RUN_NW_W | RUN_SW_W, RUN_W_SW},
{TEST_UNSEEN, -2,  0, RUN_NW_W | RUN_SW_W, RUN_W_W},
{TEST_UNSEEN, -2, -1, RUN_NW_W | RUN_SW_W, RUN_W_NW},

/* Prefer going straight over zig-zagging */
{TEST_NONE,  0,  0, RUN_N_N,  RUN_NE_NW | RUN_NW_NE},
{TEST_NONE,  0,  0, RUN_E_E,  RUN_NE_SE | RUN_SE_NE},
{TEST_NONE,  0,  0, RUN_S_S,  RUN_SE_SW | RUN_SW_SE},
{TEST_NONE,  0,  0, RUN_W_W,  RUN_NW_SW | RUN_SW_NW},

/* Prefer moving diagonal then orthagonal over the reverse */
{TEST_NONE,  0,  0, RUN_NW_W, RUN_W_NW},
{TEST_NONE,  0,  0, RUN_NW_N, RUN_N_NW},
{TEST_NONE,  0,  0, RUN_NE_N, RUN_N_NE},
{TEST_NONE,  0,  0, RUN_NE_E, RUN_E_NE},
{TEST_NONE,  0,  0, RUN_SE_E, RUN_E_SE},
{TEST_NONE,  0,  0, RUN_SE_S, RUN_S_SE},
{TEST_NONE,  0,  0, RUN_SW_S, RUN_S_SW},
{TEST_NONE,  0,  0, RUN_SW_W, RUN_W_SW},
};

static const u32b valid_dir_mask[10] = {
/* 0 */ 0,
/* 1 */ RUN_NW | RUN_W | RUN_SW | RUN_S | RUN_SE,
/* 2 */ RUN_SW | RUN_S | RUN_SE,
/* 3 */ RUN_SW | RUN_S | RUN_SE | RUN_E | RUN_NE,
/* 4 */ RUN_NW | RUN_W | RUN_SW,
/* 5 */ 0,
/* 6 */ RUN_SE | RUN_E | RUN_NE,
/* 7 */ RUN_NE | RUN_N | RUN_NW | RUN_W | RUN_SW,
/* 8 */ RUN_NE | RUN_N | RUN_NW,
/* 9 */ RUN_SE | RUN_E | RUN_NE | RUN_N | RUN_NW
};

static const u32b basic_dir_mask[10] = {
/* 0 */ 0,
/* 1 */ RUN_SW,
/* 2 */ RUN_S,
/* 3 */ RUN_SE,
/* 4 */ RUN_W,
/* 5 */ 0,
/* 6 */ RUN_E,
/* 7 */ RUN_NW,
/* 8 */ RUN_N,
/* 9 */ RUN_NE
};

/*
 * Lists of walls that, if all set, mean we're
 * probably in a corridor.
 *
 * Generally we decide we're in a corridor if
 * two opposite squares are walls, or one square
 * and the two opposite corners, or all four
 * corners.
 */
static const u32b corridor_test_mask[] = {
RUN_N | RUN_S,
RUN_E | RUN_W,
RUN_N | RUN_SE | RUN_SW,
RUN_E | RUN_NW | RUN_SW,
RUN_S | RUN_NE | RUN_NW,
RUN_W | RUN_SE | RUN_NE,
RUN_NW | RUN_NE | RUN_SE | RUN_SW
};


/*
 * Lists of walls that, if all set, mean we're probably running
 * alongside a wall.
 *
 * For horizontal or vertical walls this means that at least two walls
 * are present on one side along our path.
 *
 * ToDo: support diagonal walls
 */
static const u32b wall_test_mask[10][6] =
{
/* 0 */ {0, 0, 0, 0, 0, 0},
/* 1 */ {0, 0, 0, 0, 0, 0},
/* 2 */ {
		RUN_E | RUN_SE,
		RUN_E | RUN_NE,
		RUN_W | RUN_SW,
		RUN_W | RUN_NW,
		RUN_SE | RUN_NE,
		RUN_SW | RUN_NW,
	},

/* 3 */ {0, 0, 0, 0, 0, 0},
/* 4 */ {
		RUN_S | RUN_SW,
		RUN_S | RUN_SE,
		RUN_N | RUN_NW,
		RUN_N | RUN_NE,
		RUN_SW | RUN_SE,
		RUN_NW | RUN_NE,
	},
/* 5 */ {0, 0, 0, 0, 0, 0},
/* 6 */ {
		RUN_S | RUN_SW,
		RUN_S | RUN_SE,
		RUN_N | RUN_NW,
		RUN_N | RUN_NE,
		RUN_SW | RUN_SE,
		RUN_NW | RUN_NE,
	},
/* 7 */ {0, 0, 0, 0, 0, 0},
/* 8 */ {
		RUN_E | RUN_SE,
		RUN_E | RUN_NE,
		RUN_W | RUN_SW,
		RUN_W | RUN_NW,
		RUN_SE | RUN_NE,
		RUN_SW | RUN_NW,
	},
/* 9 */ {0, 0, 0, 0, 0, 0},
};


/*
 * Determine the run algorithm to use.
 *
 * If we seem to be in a corridor, use the "follow" algorithm.
 * otherwise, use the "open" algorithm.
 *
 * Note that we delay selecting the mode to use until after
 * the player has moved the first step of the run.
 *
 * In general determining which algorithm to use is complicated.
 */
static void run_choose_mode(void)
{
	int px = p_ptr->px;
	int py = p_ptr->py;
	unsigned int i;
	u32b wall_dirs = 0;

	/* Check valid dirs */
	for (i = 1; i < 10; i++)
	{
		if (see_wall(px + ddx[i], py + ddy[i]))
			wall_dirs |= basic_dir_mask[i];
	}

	/* Check for evidence we're in a corridor */
	for (i = 0; i < NUM_ELEMENTS(corridor_test_mask); i++)
	{
		/*
		 * If none of the elements in the mask are _not_
		 * set, we're in a corridor.
		 */
		if (!(~wall_dirs & corridor_test_mask[i]))
		{
			p_ptr->run.mode = RUN_MODE_CORRIDOR;
			return;
		}
	}

	/* Check for evidence we're following a wall */
	for (i = 0; i < NUM_ELEMENTS(wall_test_mask[p_ptr->run.cur_dir]); i++)
	{
		/*
		 * If none of the elements in the mask are _not_
		 * set, we're following a wall.
		 */
		if (!(~wall_dirs & wall_test_mask[p_ptr->run.cur_dir][i]))
		{
			p_ptr->run.mode = RUN_MODE_WALL;
			return;
		}
	}

	/* Assume we're in the open */
	p_ptr->run.mode = RUN_MODE_OPEN;
}


/*
 * Check if anything interesting is nearby
 */
static int check_interesting(void)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

	int i;

	for (i = 0; i < 8; i++)
	{
		/* Ignore the square we just moved off of */
		if (ddd[i] == p_ptr->run.old_dir)
			continue;

		/* Subtract rather than add so the previous check works */
		if (see_interesting(px - ddx_ddd[i], py - ddy_ddd[i]))
			return (TRUE);
	}

	return (FALSE);
}

/*
 * The corridor running algorithm.
 *
 * We start by determining all two-move combinations that the player could possibly
 * make, where the first move shares at least one component (N, E, S, or W) with
 * the move made last turn.
 *
 * Then we use various tests (given in the run_checks[] table above) to remove
 * some of the possibilities.
 *
 * If, after all the checks, all the two-move combinations left start with the
 * same move, we use that move. Otherwise, we're at a branch, so we stop.
 *
 * See run_checks[] for more detailed comments on some of the situations we
 * test for.
 */
static void run_corridor(int starting)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

	u32b valid_dirs = 0;
	unsigned int i;

	/* Check if we're next to something interesting. If we are, stop. */
	if (check_interesting())
	{
		p_ptr->run.mode = RUN_MODE_FINISH;
		return;
	}

	/* Add all possibly-legal dirs depending on previous direction */
	valid_dirs = valid_dir_mask[p_ptr->run.old_dir];

	/* Do magic */
	for (i = 0; i < NUM_ELEMENTS(run_checks); i++)
	{
		if (run_checks[i].remove_mask & valid_dirs)
		{
			int dx = run_checks[i].dx;
			int dy = run_checks[i].dy;

			u32b test_mask = run_checks[i].test_mask;

			/* Check mask if present */
			if (test_mask)
			{
				if (!(valid_dirs & test_mask))
					continue;
			}

			/* Check square if present */
			if (dx || dy)
			{
				bool ok = TRUE;

				switch (run_checks[i].test)
				{
				case TEST_NONE:
					break;

				case TEST_WALL:
					ok = see_wall(px + dx, py + dy);
					break;

				case TEST_UNSEEN:
					ok = see_nothing(px + dx, py + dy);
					break;

				case TEST_FLOOR:
					ok = !see_wall(px + dx, py + dy) &&
					     !see_nothing(px + dx, py + dy);
					break;

				case TEST_FLOOR_S:
					ok = starting &&
					     !see_wall(px + dx, py + dy) &&
					     !see_nothing(px + dx, py + dy);
					break;
				}

				if (!ok) continue;
			}

			valid_dirs &= ~run_checks[i].remove_mask;
		}
	}

	/* If there are no valid paths left, stop */
	if (!valid_dirs)
	{
		p_ptr->run.mode = RUN_MODE_FINISH;
		return;
	}

	p_ptr->run.cur_dir = 0;

	/* Check if we can go a single direction */
	if      (!(valid_dirs & ~RUN_SW)) p_ptr->run.cur_dir = 1;
	else if (!(valid_dirs & ~RUN_S))  p_ptr->run.cur_dir = 2;
	else if (!(valid_dirs & ~RUN_SE)) p_ptr->run.cur_dir = 3;
	else if (!(valid_dirs & ~RUN_W))  p_ptr->run.cur_dir = 4;
	else if (!(valid_dirs & ~RUN_E))  p_ptr->run.cur_dir = 6;
	else if (!(valid_dirs & ~RUN_NW)) p_ptr->run.cur_dir = 7;
	else if (!(valid_dirs & ~RUN_N))  p_ptr->run.cur_dir = 8;
	else if (!(valid_dirs & ~RUN_NE)) p_ptr->run.cur_dir = 9;

	/* If we can go multiple directions, we're at a branch. Stop. */
	if (!p_ptr->run.cur_dir)
	{
		p_ptr->run.mode = RUN_MODE_FINISH;
		return;
	}

	p_ptr->run.old_dir = p_ptr->run.cur_dir;

	/* XXX Don't-cut-corners code goes here */
}


/*
 * Run in an open area
 *
 * XXX Write this
 */
static void run_open(void)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

	int dir = p_ptr->run.old_dir;
	int dx = ddx[dir];
	int dy = ddy[dir];

	if (check_interesting())
	{
		p_ptr->run.mode = RUN_MODE_FINISH;
		return;
	}

	p_ptr->run.cur_dir = dir;

	if (see_wall(px + dx, py + dy))
	{
		p_ptr->run.mode = RUN_MODE_FINISH;
		return;
	}
}


static const int run_wall_check[10][2][2] =
{
/* 0 */ {{0, 0}, {0, 0}},
/* 1 */ {{0, 0}, {0, 0}},
/* 2 */ {{4, 1}, {6, 3}},
/* 3 */ {{0, 0}, {0, 0}},
/* 4 */ {{2, 1}, {8, 7}},
/* 5 */ {{0, 0}, {0, 0}},
/* 6 */ {{2, 3}, {8, 9}},
/* 7 */ {{0, 0}, {0, 0}},
/* 8 */ {{4, 7}, {6, 9}},
/* 9 */ {{0, 0}, {0, 0}},
};


/*
 * Run alongside a wall
 *
 * XXX Write this
 */
static void run_wall(void)
{
	unsigned int i;

	int px = p_ptr->px;
	int py = p_ptr->py;

	int dir = p_ptr->run.old_dir;
	int dx = ddx[dir];
	int dy = ddy[dir];

	if (check_interesting())
	{
		p_ptr->run.mode = RUN_MODE_FINISH;
		return;
	}

	/* Check if we are coming up to an opening in the wall */
	for (i = 0; i < NUM_ELEMENTS(run_wall_check[dir]); i++)
	{
		int wall_dir  = run_wall_check[dir][i][0];
		int floor_dir = run_wall_check[dir][i][1];

		if ( see_wall(px + ddx[wall_dir ], py + ddy[wall_dir ]) &&
	            !see_wall(px + ddx[floor_dir], py + ddy[floor_dir]))
		{
			p_ptr->run.mode = RUN_MODE_FINISH;
			return;
		}
	}

	p_ptr->run.cur_dir = dir;

	if (see_wall(px + dx, py + dy))
	{
		p_ptr->run.mode = RUN_MODE_FINISH;
		return;
	}
}



/*
 * Take one step along a run path
 */
void run_step(int dir)
{
	if (dir)
	{
		/* Don't start by running into a wall! */
		if (see_wall(p_ptr->px + ddx[dir], p_ptr->py + ddy[dir]))
		{
			/* Message */
			msgf("You cannot run in that direction.");

			/* Disturb */
			disturb(FALSE);

			/* Done */
			return;
		}

		/* Start by moving this direction */
		p_ptr->run.cur_dir = dir;
		p_ptr->run.old_dir = dir;

		/* In "start run" state */
		p_ptr->run.mode = RUN_MODE_START;
	}
	else
	{
		int starting = FALSE;
		
		/* If we've just started our run, determine the algorithm now */
		if (p_ptr->run.mode == RUN_MODE_START)
		{
			starting = TRUE;
			run_choose_mode();
		}

		/* Use the selected algorithm */
		switch (p_ptr->run.mode)
		{
		case RUN_MODE_OPEN:
			run_open();
			break;

		case RUN_MODE_CORRIDOR:
			run_corridor(starting);
			break;

		case RUN_MODE_WALL:
			run_wall();
			break;

		case RUN_MODE_FINISH:
			break;

		default:
			msgf("Error: Bad run mode %i.", p_ptr->run.mode);
			msgf("Please submit a bug report.");
			disturb(FALSE);
			return;
		}
	}

	/* Check for end of run */
	if (p_ptr->run.mode == RUN_MODE_FINISH)
	{
		disturb(FALSE);

		return;
	}

	/* Decrease the run counter */
	p_ptr->state.running--;

	/* Use energy */
	p_ptr->state.energy_use = 100;

	/* Take a step */
	move_player(p_ptr->run.cur_dir, FALSE);
}
