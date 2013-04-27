// File: map.c
// Purpose: low level dungeon routines -BEN-


#include "utumno.h"

extern int MAP_CENTER_X, MAP_CENTER_Y;

/*
 * Memorize all objects at location (x, y)
 */
void memorize_all(int x, int y)
{
    CGrid *g_ptr = &cave[y][x];
    CItem *i_ptr = g_ptr->i_ptr;

    while (i_ptr) {
        // Mark it
        i_ptr->SetMarked(TRUE);

        // Get next
        i_ptr = i_ptr->next_i_ptr;
    }
}


/*
 * Forget all objects at location (x, y)
 */
void forget_all(int x, int y)
{
    CGrid *g_ptr = &cave[y][x];
    CItem *i_ptr = g_ptr->i_ptr;

    while (i_ptr) {
        // Unmark it
        i_ptr->SetMarked(FALSE);

        // Get next
        i_ptr = i_ptr->next_i_ptr;
    }
}


/*
 * Is this location a permawall?
 */
bool CGrid::is_permawall(void)
{
    if (get_feat() == CF_PERMANENT_BASIC) return TRUE;
    if (get_feat() == CF_PERMANENT_INNER) return TRUE;
    if (get_feat() == CF_PERMANENT_OUTER) return TRUE;
    if (get_feat() == CF_PERMANENT_SOLID) return TRUE;
    return FALSE;
}


// Is a location a wall?
bool CGrid::is_wall(void)
{
    if (get_feat() == CF_GRANITE_BASIC) return TRUE;
    if (get_feat() == CF_GRANITE_INNER) return TRUE;
    if (get_feat() == CF_GRANITE_OUTER) return TRUE;
    if (get_feat() == CF_GRANITE_SOLID) return TRUE;
    if (is_permawall()) return TRUE;
    return FALSE;
}


// Is a location a locked door?
bool CGrid::is_locked_door(void)
{
    if (get_feat() == CF_DOOR_LOCKED_1) return TRUE;
    if (get_feat() == CF_DOOR_LOCKED_2) return TRUE;
    if (get_feat() == CF_DOOR_LOCKED_3) return TRUE;
    if (get_feat() == CF_DOOR_LOCKED_4) return TRUE;
    if (get_feat() == CF_DOOR_LOCKED_5) return TRUE;
    if (get_feat() == CF_DOOR_LOCKED_6) return TRUE;
    if (get_feat() == CF_DOOR_LOCKED_7) return TRUE;
    return FALSE;
}


// Is a location a door?
bool CGrid::is_door(void)
{
    if (get_feat() == CF_DOOR_OPEN) return TRUE;
    if (get_feat() == CF_DOOR_BROKEN) return TRUE;
    if (get_feat() == CF_DOOR_CLOSED) return TRUE;
    if (is_locked_door()) return TRUE;
    if (get_feat() == CF_DOOR_SECRET) return TRUE;
    return FALSE;
}


// Is a location a store door?
bool CGrid::is_store_door(void)
{
    if ((get_feat() >= 0x08) && (get_feat() <= 0x0F)) return TRUE;
    return FALSE;
}


// Is a location a visible trap?
bool CGrid::is_visible_trap(void)
{
    if ((get_feat() >= 0x10) && (get_feat() <= 0x1F)) return TRUE;
    return FALSE;
}


// Get the lock strength of a door
int CGrid::get_door_lock_strength(void)
{
    return (get_feat() & 0x07);
}


// Get the type of a store
int CGrid::get_store_type(void)
{
    return (get_feat() & 0x07);
}


/*
 * Is a given location "valid" for placing things?
 *
 * Permanent walls, stairs, store doors are never "valid".
 *
 * A grid with an artifact in it is never valid.
 *
 * This function is often "combined" with "floor_grid_bold(y,x)"
 * or one of the other similar "functions".
 *
 * Forbid perma-walls
 * Forbid stairs and store doors
 * Forbid artifact grids
 */
bool valid_grid(int y, int x)
{
    CGrid *g_ptr;
    CItem *i_ptr;

    // Must be in bounds
    if (!in_bounds(y, x)) return FALSE;

    // Get the pointer
    g_ptr = &cave[y][x];

    // No permawalls, store doors, stairs
    if (g_ptr->is_permawall()) return FALSE;
    if (g_ptr->is_store_door()) return FALSE;
    //: Fix stairs

    // Get first item
    i_ptr = g_ptr->i_ptr;

    while (i_ptr) {
        // Is it an artifact?
        if (i_ptr->isArtifact()) return FALSE;

        // Get next item
        i_ptr = i_ptr->next_i_ptr;
    }

    // Must be valid
    return TRUE;
}



/*
 * Approximate Distance between two points.
 *
 * When either the X or Y component dwarfs the other component,
 * this function is almost perfect, and otherwise, it tends to
 * over-estimate about one grid per fifteen grids of distance.
 *
 * Algorithm: hypot(dx,dy) = max(dx,dy) + min(dx,dy) / 2
 */
int distance(int x1, int y1, int x2, int y2)
{
    int dx, dy, d;

    /* Find the absolute y/x distance components */
    dx = (x1 > x2) ? (x1 - x2) : (x2 - x1);
    dy = (y1 > y2) ? (y1 - y2) : (y2 - y1);

    /* Hack -- approximate the distance */
    d = (dy > dx) ? (dy + (dx >> 1)) : (dx + (dy >> 1));

    /* Return the distance */
    return d;
}


/*
 * A simple, fast, integer-based line-of-sight algorithm.  By Joseph Hall,
 * 4116 Brewster Drive, Raleigh NC 27606.  Email to jnh@ecemwl.ncsu.edu.
 *
 * Returns TRUE if a line of sight can be traced from (x1,y1) to (x2,y2).
 *
 * The LOS begins at the center of the tile (x1,y1) and ends at the center of
 * the tile (x2,y2).  If los() is to return TRUE, all of the tiles this line
 * passes through must be floor tiles, except for (x1,y1) and (x2,y2).
 *
 * We assume that the "mathematical corner" of a non-floor tile does not
 * block line of sight.
 *
 * Because this function uses (short) ints for all calculations, overflow may
 * occur if dx and dy exceed 90.
 *
 * Once all the degenerate cases are eliminated, the values "qx", "qy", and
 * "m" are multiplied by a scale factor "f1 = abs(dx * dy * 2)", so that
 * we can use integer arithmetic.
 *
 * We travel from start to finish along the longer axis, starting at the border
 * between the first and second tiles, where the y offset = .5 * slope, taking
 * into account the scale factor.  See below.
 *
 * Also note that this function and the "move towards target" code do NOT
 * share the same properties.  Thus, you can see someone, target them, and
 * then fire a bolt at them, but the bolt may hit a wall, not them.  However,
 * by clever choice of target locations, you can sometimes throw a "curve".
 *
 * Note that "line of sight" is not "reflexive" in all cases.
 *
 * Use the "projectable()" routine to test "spell/missile line of sight".
 *
 * Use the "update_view()" function to determine player line-of-sight.
 */
bool los(int y1, int x1, int y2, int x2)
{
    // Delta, absolute, signs, fractions, scanners, scale factors
    int dx, dy, ax, ay, sx, sy, qx, qy, tx, ty, f1, f2;

    // Slope, or 1/Slope, of LOS
    int m;


    /* Extract the offset */
    dx = x2 - x1;
    dy = y2 - y1;

    /* Extract the absolute offset */
    ax = ABS(dx);
    ay = ABS(dy);


    /* Handle adjacent (or identical) grids */
    if ((ax < 2) && (ay < 2)) return (TRUE);


    /* Paranoia -- require "safe" origin */
    /* if (!in_bounds(y1, x1)) return (FALSE); */


    /* Directly South/North */
    if (!dx) {
        /* South -- check for walls */
        if (dy > 0) {
            for (ty = y1 + 1; ty < y2; ty++) {
                if (!floor_grid_bold(ty,x1)) return (FALSE);
            }
        }

        /* North -- check for walls */
        else {
            for (ty = y1 - 1; ty > y2; ty--) {
                if (!floor_grid_bold(ty,x1)) return (FALSE);
            }
        }

        /* Assume los */
        return (TRUE);
    }

    /* Directly East/West */
    if (!dy) {
        /* East -- check for walls */
        if (dx > 0) {
            for (tx = x1 + 1; tx < x2; tx++) {
                if (!floor_grid_bold(y1,tx)) return (FALSE);
            }
        }

        /* West -- check for walls */
        else {
            for (tx = x1 - 1; tx > x2; tx--) {
                if (!floor_grid_bold(y1,tx)) return (FALSE);
            }
        }

        /* Assume los */
        return (TRUE);
    }


    /* Extract some signs */
    sx = (dx < 0) ? -1 : 1;
    sy = (dy < 0) ? -1 : 1;


    /* Vertical "knights" */
    if (ax == 1) {
        if (ay == 2) {
            if (floor_grid_bold(y1 + sy, x1)) return (TRUE);
        }
    }

    /* Horizontal "knights" */
    else if (ay == 1) {
        if (ax == 2) {
            if (floor_grid_bold(y1, x1 + sx)) return (TRUE);
        }
    }


    /* Calculate scale factor div 2 */
    f2 = (ax * ay);

    /* Calculate scale factor */
    f1 = f2 << 1;


    // Travel horizontally
    if (ax >= ay) {
        /* Let m = dy / dx * 2 * (dy * dx) = 2 * dy * dy */
        qy = ay * ay;
        m = qy << 1;
        
        tx = x1 + sx;

        /* Consider the special case where slope == 1. */
        if (qy == f2) {
            ty = y1 + sy;
            qy -= f1;
        }
        else {
            ty = y1;
        }

        /* Note (below) the case (qy == f2), where */
        /* the LOS exactly meets the corner of a tile. */
        while (x2 - tx) {
            if (!floor_grid_bold(ty,tx)) return (FALSE);
            
            qy += m;

            if (qy < f2) {
                tx += sx;
            }
            else if (qy > f2) {
                ty += sy;
                if (!floor_grid_bold(ty,tx)) return (FALSE);
                qy -= f1;
                tx += sx;
            }
            else {
                ty += sy;
                qy -= f1;
                tx += sx;
            }
        }
    }

    /* Travel vertically */
    else {
        /* Let m = dx / dy * 2 * (dx * dy) = 2 * dx * dx */
        qx = ax * ax;
        m = qx << 1;

        ty = y1 + sy;
        
        if (qx == f2) {
            tx = x1 + sx;
            qx -= f1;
        }
        else {
            tx = x1;
        }

        /* Note (below) the case (qx == f2), where */
        /* the LOS exactly meets the corner of a tile. */
        while (y2 - ty) {
            if (!floor_grid_bold(ty,tx)) return (FALSE);
            
            qx += m;
            
            if (qx < f2) {
                ty += sy;
            }
            else if (qx > f2) {
                tx += sx;
                if (!floor_grid_bold(ty,tx)) return (FALSE);
                qx -= f1;
                ty += sy;
            }
            else {
                tx += sx;
                qx -= f1;
                ty += sy;
            }
        }
    }

    /* Assume los */
    return (TRUE);
}





/*
 * Can the player "see" the given grid in detail?
 *
 * He must have vision, illumination, and line of sight.
 *
 * Note -- "MAP_LITE" is only set if the "torch" has "los()".
 * So, given "MAP_LITE", we know that the grid is "fully visible".
 *
 * Note that "MAP_GLOW" makes little sense for a wall, since it would mean
 * that a wall is visible from any direction.  That would be odd.  Except
 * under wizard light, which might make sense.  Thus, for walls, we require
 * not only that they be "MAP_GLOW", but also, that they be adjacent to a
 * grid which is not only "MAP_GLOW", but which is a non-wall, and which is
 * in line of sight of the player.
 *
 * This extra check is expensive, but it provides a more "correct" semantics.
 *
 * Note that we should not run this check on walls which are "outer walls" of
 * the dungeon, or we will induce a memory fault, but actually verifying all
 * of the locations would be extremely expensive.
 *
 * Thus, to speed up the function, we assume that all "perma-walls" which are
 * "MAP_GLOW" are "illuminated" from all sides.  This is correct for all cases
 * except "vaults" and the "buildings" in town.  But the town is a hack anyway,
 * and the player has more important things on his mind when he is attacking a
 * monster vault.  It is annoying, but an extremely important optimization.
 *
 * We could check the four adjacent neighbors instead of all eight, but this
 * would cause the "corners" of illuminated rooms to appear "dark".
 */
bool CPlayer::can_see_bold(int y, int x)
{
    int i;
    CGrid *g_ptr;

    // Blind players see nothing
    if (GetBlind()) return FALSE;

    // Access the cave grid
    g_ptr = &cave[y][x];

    // Note that "torch-lite" yields "illumination"
    if (g_ptr->flags & MAP_LITE) return TRUE;

    // Require line of sight to the grid
    if (!player_has_los_bold(y, x)) return FALSE;

    // Require "perma-lite" of the grid
    if (!(g_ptr->flags & MAP_GLOW)) return FALSE;

    /* Assume perma-lit viewable floors are illuminated */
    if (floor_grid_bold(y, x)) return (TRUE);

    /* Mega-Hack -- Prevent memory faults (see above) */
    if (g_ptr->get_feat() == CF_PERMANENT_SOLID) return (TRUE);

    /* Hack -- verify walls */
    for (i = 0; i < 8; i++) {
        /* Extract adjacent (legal) location */
        int yy = y + ddy[ddd[i]];
        int xx = x + ddx[ddd[i]];

        /* Check for adjacent perma-lit viewable floor */
        if ((floor_grid_bold(yy, xx)) &&
            (player_has_los_bold(yy, xx)) &&
            (cave[yy][xx].flags & MAP_GLOW))
        {
            /* Assume the wall is really illuminated */
            return TRUE;
        }
    }

    // Assume not visible
    return FALSE;
}



/*
 * Returns true if the player's grid is dark
 */
bool CPlayer::no_lite(void)
{
    return !can_see_bold(GetY(), GetX());
}









/*
 * Memorize the given grid (or object) if it is "interesting"
 *
 * This function should only be called on "legal" grids.
 *
 * This function should be called every time the "memorization" of
 * a grid (or the object in a grid) is called into question.
 *
 * Note that the player always memorized all "objects" which are seen,
 * using a different method than the one used for terrain features,
 * which not only allows a lot of optimization, but also prevents the
 * player from "knowing" when objects are dropped out of sight but in
 * memorized grids.
 *
 * Note that the player always memorizes "interesting" terrain features
 * (everything but floors and invisible traps).  This allows incredible
 * amounts of optimization in various places.
 *
 * Note that the player is allowed to memorize floors and invisible
 * traps under various circumstances, and with various options set.
 *
 * This function is slightly non-optimal, since it memorizes objects
 * and terrain features separately, though both are dependant on
 * p_ptr->can_see_bold().
 */
void note_spot(int y, int x)
{
    CGrid *g_ptr = &cave[y][x];


    /* Hack -- memorize objects */
    if (g_ptr->i_ptr) {
        /* Memorize visible objects */
        if (p_ptr->can_see_bold(y, x)) {
            /* Memorize */
            memorize_all(x, y);
        }
    }


    /* Hack -- memorize grids */
    if (!(g_ptr->flags & MAP_KNOW)) {
        /* Memorize visible grids */
        if (p_ptr->can_see_bold(y, x)) {
            // Memorize all grids
            g_ptr->flags |= MAP_KNOW;
        }
    }
}


static bool is_wall(int x, int y)
{
    CGrid *g_ptr;

    if (x < 0) return TRUE;
    if (y < 0) return TRUE;
    if (x >= cur_wid) return TRUE;
    if (y >= cur_hgt) return TRUE;

    g_ptr = &cave[y][x];

    return g_ptr->is_wall();
}


static bool appear_wall(int x, int y)
{
    CGrid *g_ptr;

    if (x < 0) return TRUE;
    if (y < 0) return TRUE;
    if (x >= cur_wid) return TRUE;
    if (y >= cur_hgt) return TRUE;

    g_ptr = &cave[y][x];

    if (!(g_ptr->flags & MAP_KNOW)) return TRUE;

    if (g_ptr->get_feat() == CF_GRANITE_BASIC) return TRUE;
    if (g_ptr->get_feat() == CF_GRANITE_INNER) return TRUE;
    if (g_ptr->get_feat() == CF_GRANITE_OUTER) return TRUE;
    if (g_ptr->get_feat() == CF_GRANITE_SOLID) return TRUE;
    if (g_ptr->get_feat() == CF_PERMANENT_BASIC) return TRUE;
    if (g_ptr->get_feat() == CF_PERMANENT_INNER) return TRUE;
    if (g_ptr->get_feat() == CF_PERMANENT_OUTER) return TRUE;
    if (g_ptr->get_feat() == CF_PERMANENT_SOLID) return TRUE;
    if (g_ptr->get_feat() == CF_DOOR_SECRET) return TRUE;

    return FALSE;
}


void draw_floor(int ix, int iy, int variant, bool glowing)
{
    switch (variant % 6) {
        case 0:
        case 1:
        case 2:
            draw_tile(ix, iy, "features/floor00", !glowing, "base", 0, 0); break;
        case 3:
            draw_tile(ix, iy, "features/floor01", !glowing, "base", 0, 0); break;
        case 4:
            draw_tile(ix, iy, "features/floor02", !glowing, "base", 0, 0); break;
        case 5:
            draw_tile(ix, iy, "features/floor03", !glowing, "base", 0, 0); break;
    }
}


static byte get_feat_safe(int y, int x)
{
    // Out of bounds
    if ((x < 0) || (y < 0)) return CF_PERMANENT_SOLID;
    if ((x >= cur_wid) || (y >= cur_hgt)) return CF_PERMANENT_SOLID;

    // Return the real feature
    return cave[y][x].get_feat();
}

static bool is_lit_safe(int y, int x)
{
    // Out of bounds
    if ((x < 0) || (y < 0)) return FALSE;
    if ((x >= cur_wid) || (y >= cur_hgt)) return FALSE;

    // Return the real value
    return cave[y][x].is_glowing();
}


/*
 * Draw layer 1 of a single tile
 */
void draw_layer_1(int x, int y, int ix, int iy, bool hilited)
{
    CGrid *g_ptr;
    CItem *i_ptr;
    bool glowing, known;

    if (x < 0) return;
    if (y < 0) return;
    if (x >= cur_wid) return;
    if (y >= cur_hgt) return;

    g_ptr = &cave[y][x];
    i_ptr = g_ptr->i_ptr;

    glowing = g_ptr->is_glowing();
    known = (g_ptr->flags & MAP_KNOW);

    if (known) {
        switch (g_ptr->get_feat()) {
            case CF_FLOOR:
            case CF_TRAP_INVIS:
                draw_floor(ix, iy, g_ptr->variant, glowing);
                break;
            case CF_STAIR_DOWN_NE:
                draw_tile(ix, iy, "features/stdne00", !glowing, "base", 0, 0);
                break;
            case CF_STAIR_DOWN_NW:
                draw_tile(ix, iy, "features/stdnw00", !glowing, "base", 0, 0);
                break;
            case CF_STAIR_DOWN_SW:
                draw_tile(ix, iy, "features/stdsw00", !glowing, "base", 0, 0);
                break;
            case CF_STAIR_DOWN_SE:
                draw_tile(ix, iy, "features/stdse00", !glowing, "base", 0, 0);
                break;
            case CF_DOOR_OPEN:
            case CF_DOOR_BROKEN:
                draw_floor(ix, iy, g_ptr->variant, glowing);
                break;
            case CF_DOOR_CLOSED:
            case CF_DOOR_LOCKED_1:
            case CF_DOOR_LOCKED_2:
            case CF_DOOR_LOCKED_3:
            case CF_DOOR_LOCKED_4:
            case CF_DOOR_LOCKED_5:
            case CF_DOOR_LOCKED_6:
            case CF_DOOR_LOCKED_7:
                draw_floor(ix, iy, g_ptr->variant, glowing);
                break;
            case CF_TRAP_TRAP_DOOR:
            case CF_TRAP_OPEN_PIT:
            case CF_TRAP_SPIKED_PIT:
            case CF_TRAP_POISON_PIT:
            case CF_TRAP_SUMMON:
            case CF_TRAP_TELEPORT:
            case CF_TRAP_FIRE:
            case CF_TRAP_ACID:
            case CF_TRAP_DART_SLOW:
            case CF_TRAP_DART_STR:
            case CF_TRAP_DART_DEX:
            case CF_TRAP_DART_CON:
            case CF_TRAP_GAS_BLIND:
            case CF_TRAP_GAS_CONFUSE:
            case CF_TRAP_GAS_POISON:
            case CF_TRAP_GAS_SLEEP:
                draw_floor(ix, iy, g_ptr->variant, glowing);
                draw_tile(ix, iy, "trap", FALSE, "base", 0, 0);
                break;
            case CF_RUBBLE:
                draw_floor(ix, iy, g_ptr->variant, glowing);
                draw_tile(ix, iy, "features/rubble", !glowing, "base", 0, 0);
                break;
            default:
                //: lots still here
                break;
        }
    }

    // Draw hilite
    if (hilited) draw_tile(ix, iy, "hilite", FALSE, "base", 0, 0);
    
    // Draw any items
    if (i_ptr && i_ptr->GetMarked()) {
        bool in_stack = FALSE;
        CItem *j_ptr = i_ptr;
        while (j_ptr->next_i_ptr) {
            in_stack = TRUE;
            j_ptr = j_ptr->next_i_ptr;
            if (j_ptr->GetWeight() > i_ptr->GetWeight()) i_ptr = j_ptr;
        }
        CObjectKind *k_ptr = i_ptr->get_k_ptr();
        //: if (in_stack) draw_tile(ix, iy, "stack", !glowing);
        draw_tile_idx(ix, iy, k_ptr->tile, !glowing, "base", 0, 0);
    }
}


/*
 * A sprite to dump to the screen.
 */
struct sprite_type {
    s32b depth;
    int ix, iy, idx;
    bool glowing;
    char *scene;
    int view, frame;
};

const int MAX_SPRITES = 8192;

static sprite_type sprites[MAX_SPRITES];
static int nsprites;

/*
 * Add a sprite
 */
void add_sprite(s32b depth, int px, int py, int idx, bool glowing, char *scene, int view,
    int frame)
{
    // Add it to the list
    sprites[nsprites].depth = depth;
    sprites[nsprites].ix = px;
    sprites[nsprites].iy = py;
    sprites[nsprites].idx = idx;
    sprites[nsprites].glowing = glowing;
    sprites[nsprites].scene = scene;
    sprites[nsprites].view = view;
    sprites[nsprites].frame = frame;
    nsprites++;
}
void add_sprite(s32b depth, int px, int py, char *name, bool glowing, char *scene, int view,
    int frame)
{
    int idx = locate_tile(name);
    if (idx == -1) {
        quit("error in add_sprite");
    }
    add_sprite(depth, px, py, idx, glowing, scene, view, frame);
}


/*
 * Reset sprite list
 */
void start_layer_2(void) { nsprites = 0; }


/*
 * Compare sprite depth
 */
int compare_depth(const void *a, const void *b)
{
    sprite_type *x = (sprite_type *)a;
    sprite_type *y = (sprite_type *)b;
    if (x->depth < y->depth) return -1;
    if (x->depth > y->depth) return 1;
    return 0;
}


/*
 * Draw all sprites (sorted)
 */
void end_layer_2(void)
{
    int i;

    qsort(sprites, nsprites, sizeof(sprite_type), compare_depth);
    for (i = 0; i < nsprites; i++) {
        sprite_type *s = &sprites[i];
        draw_tile_idx(s->ix, s->iy, s->idx, s->glowing, s->scene, s->view, s->frame);
    }
}


/*
 * Draw a single tile
 */
void draw_layer_2(int x, int y, int ix, int iy, int player_x, int player_y)
{
    CGrid *g_ptr;
    CMonster *m_ptr;
    bool glowing, known;
    s32b base_depth;

    if (x < 0) return;
    if (y < 0) return;
    if (x >= cur_wid) return;
    if (y >= cur_hgt) return;

    base_depth = x*1000+y*1000;

    g_ptr = &cave[y][x];
    m_ptr = g_ptr->m_ptr;

    glowing = g_ptr->is_glowing();
    known = (g_ptr->flags & MAP_KNOW);

    switch (g_ptr->get_feat()) {
        //: Fix stairdraw
        case CF_STAIR_UP_NE:
            if (known) {
                add_sprite(base_depth, ix, iy, "features/stune00", !glowing, "base", 0, 0);
            }
            break;
        case CF_STAIR_UP_NW:
            if (known) {
                add_sprite(base_depth, ix, iy, "features/stunw00", !glowing, "base", 0, 0);
            }
            break;
        case CF_STAIR_UP_SW:
            if (known) {
                add_sprite(base_depth, ix, iy, "features/stusw00", !glowing, "base", 0, 0);
            }
            break;
        case CF_STAIR_UP_SE:
            if (known) {
                add_sprite(base_depth, ix, iy, "features/stuse00", !glowing, "base", 0, 0);
            }
            break;
        case CF_GRANITE_BASIC:
        case CF_GRANITE_INNER:
        case CF_GRANITE_OUTER:
        case CF_GRANITE_SOLID:
        case CF_PERMANENT_BASIC:
        case CF_PERMANENT_INNER:
        case CF_PERMANENT_OUTER:
        case CF_PERMANENT_SOLID:
        case CF_DOOR_SECRET:
            if (known) {
                if (!appear_wall(x-1, y)) {
                    byte f = get_feat_safe(y, x-1);
                    if ((f != CF_STAIR_UP_NE) && (f != CF_STAIR_UP_NW) &&
                        (f != CF_STAIR_UP_SW) && (f != CF_STAIR_UP_SE)) {
                        add_sprite(base_depth-500, ix, iy, "features/walnw00", FALSE, "base",
                            0, 0);
                    }
                }
                if (!appear_wall(x, y-1)) {
                    byte f = get_feat_safe(y-1, x);
                    if ((f != CF_STAIR_UP_NE) && (f != CF_STAIR_UP_NW) &&
                        (f != CF_STAIR_UP_SW) && (f != CF_STAIR_UP_SE)) {
                        add_sprite(base_depth-500, ix, iy, "features/walne00", FALSE, "base",
                            0, 0);
                    }
                }
            }
            if (known) {
                if (!appear_wall(x+1, y)) {
                    bool se_glow = is_lit_safe(y, x+1);
                    if (get_feat_safe(y, x+1) == CF_STAIR_UP_SE) {
                        // draw nothing
                    }
                    else if (get_feat_safe(y, x+1) == CF_STAIR_UP_NE) {
                        add_sprite(base_depth+500, ix, iy, "features/stwse00", !se_glow,
                            "base", 0, 0);
                    }
                    else if (appear_wall(x, y-1)) {
                        if (g_ptr->variant & 1) {
                            add_sprite(base_depth+500, ix, iy, "features/walse00", !se_glow,
                                "base", 0, 0);
                        }
                        else {
                            add_sprite(base_depth+500, ix, iy, "features/walse01", !se_glow,
                                "base", 0, 0);
                        }
                    }
                    else {
                        if (get_feat_safe(y-1, x) == CF_STAIR_DOWN_SE) {
                            add_sprite(base_depth+500, ix, iy, "features/walse00s", !se_glow,
                                "base", 0, 0);
                        }
                        else if (g_ptr->variant & 1) {
                            add_sprite(base_depth+500, ix, iy, "features/walse00t", !se_glow,
                                "base", 0, 0);
                        }
                        else {
                            add_sprite(base_depth+500, ix, iy, "features/walse01t", !se_glow,
                                "base", 0, 0);
                        }
                    }
                }
                if (!appear_wall(x, y+1)) {
                    bool sw_glow = is_lit_safe(y+1, x);
                    if (get_feat_safe(y+1, x) == CF_STAIR_UP_SW) {
                        // draw nothing
                    }
                    else if (get_feat_safe(y+1, x) == CF_STAIR_UP_NW) {
                        add_sprite(base_depth+500, ix, iy, "features/stwsw00", !sw_glow,
                            "base", 0, 0);
                    }
                    else if (appear_wall(x-1, y)) {
                        if (g_ptr->variant & 1) {
                            add_sprite(base_depth+500, ix, iy, "features/walsw00", !sw_glow,
                                "base", 0, 0);
                        }
                        else {
                            add_sprite(base_depth+500, ix, iy, "features/walsw01", !sw_glow,
                                "base", 0, 0);
                        }
                    }
                    else {
                        if (get_feat_safe(y, x-1) == CF_STAIR_DOWN_SW) {
                            add_sprite(base_depth+500, ix, iy, "features/walsw00s", !sw_glow,
                                "base", 0, 0);
                        }
                        else if (g_ptr->variant & 1) {
                            add_sprite(base_depth+500, ix, iy, "features/walsw00t", !sw_glow,
                                "base", 0, 0);
                        }
                        else {
                            add_sprite(base_depth+500, ix, iy, "features/walsw01t", !sw_glow,
                                "base", 0, 0);
                        }
                    }
                }
            }
            break;
        case CF_DOOR_OPEN:
        case CF_DOOR_BROKEN:
            if (known) {
                bool nw = (is_wall(x-1, y) && is_wall(x+1, y));
                bool ne = (is_wall(x, y-1) && is_wall(x, y+1));
                char buf[80];
                if (nw) {
                    strcpy(buf, "features/drnw00o");
                }
                else if (ne) {
                    strcpy(buf, "features/drne00o");
                }
                else {
                    strcpy(buf, "features/drnw00o");
                }
                if (!appear_wall(x-1, y-1)) strcat(buf, "t");
                add_sprite(base_depth-490, ix, iy, buf, !glowing, "base", 0, 0);
            }
            break;
        case CF_DOOR_CLOSED:
        case CF_DOOR_LOCKED_1:
        case CF_DOOR_LOCKED_2:
        case CF_DOOR_LOCKED_3:
        case CF_DOOR_LOCKED_4:
        case CF_DOOR_LOCKED_5:
        case CF_DOOR_LOCKED_6:
        case CF_DOOR_LOCKED_7:
            if (known) {
                bool nw = (is_wall(x-1, y) && is_wall(x+1, y));
                bool ne = (is_wall(x, y-1) && is_wall(x, y+1));
                char buf[80];
                if (nw) {
                    strcpy(buf, "features/drnw00c");
                }
                else if (ne) {
                    strcpy(buf, "features/drne00c");
                }
                else {
                    strcpy(buf, "features/drnw00c");
                }
                add_sprite(base_depth, ix, iy, buf, !glowing, "base", 0, 0);
            }
            break;
        default:
            //: lots still here
            break;
    }

    // Must have a visible monster
    if (m_ptr && m_ptr->is_visible()) {
        // Get race
        CMonsterRace *r_ptr = m_ptr->get_r_ptr();

        // Get subtile x, y
        int subtile_x = x*1000;
        int subtile_y = y*1000;
        if ((m_ptr->action >= 1) && (m_ptr->action <= 9)) {
            int busy = m_ptr->get_busy();
            subtile_x -= ddx[m_ptr->action] * busy*10;
            subtile_y -= ddy[m_ptr->action] * busy*10;
        }

        // Get screen x, y, depth
        s32b depth = subtile_x + subtile_y;
        subtile_x -= player_x;
        subtile_y -= player_y;
        int sx, sy;
        subtile_to_pixel(subtile_x, subtile_y, &sx, &sy);
        sx += MAP_CENTER_X;
        sy += MAP_CENTER_Y;

        // Add sprite
        add_sprite(depth, sx, sy, r_ptr->tile, !glowing, "base", 0, 0);
    }
}




/*
 * Some comments on the cave grid flags.  -BEN-
 *
 *
 * One of the major bottlenecks in previous versions of Angband was in
 * the calculation of "line of sight" from the player to various grids,
 * such as monsters.  This was such a nasty bottleneck that a lot of
 * silly things were done to reduce the dependancy on "line of sight",
 * for example, you could not "see" any grids in a lit room until you
 * actually entered the room, and there were all kinds of bizarre grid
 * flags to enable this behavior.  This is also why the "call light"
 * spells always lit an entire room.
 *
 * The code below provides functions to calculate the "field of view"
 * for the player, which, once calculated, provides extremely fast
 * calculation of "line of sight from the player", and to calculate
 * the "field of torch lite", which, again, once calculated, provides
 * extremely fast calculation of "which grids are lit by the player's
 * lite source".  In addition to marking grids as "GRID_VIEW" and/or
 * "GRID_LITE", as appropriate, these functions maintain an array for
 * each of these two flags, each array containing the locations of all
 * of the grids marked with the appropriate flag, which can be used to
 * very quickly scan through all of the grids in a given set.
 *
 * To allow more "semantically valid" field of view semantics, whenever
 * the field of view (or the set of torch lit grids) changes, all of the
 * grids in the field of view (or the set of torch lit grids) are "drawn"
 * so that changes in the world will become apparent as soon as possible.
 * This has been optimized so that only grids which actually "change" are
 * redrawn, using the "temp" array and the "GRID_TEMP" flag to keep track
 * of the grids which are entering or leaving the relevent set of grids.
 *
 * These new methods are so efficient that the old nasty code was removed.
 *
 * Note that there is no reason to "update" the "viewable space" unless
 * the player "moves", or walls/doors are created/destroyed, and there
 * is no reason to "update" the "torch lit grids" unless the field of
 * view changes, or the "light radius" changes.  This means that when
 * the player is resting, or digging, or doing anything that does not
 * involve movement or changing the state of the dungeon, there is no
 * need to update the "view" or the "lite" regions, which is nice.
 *
 * Note that the calls to the nasty "los()" function have been reduced
 * to a bare minimum by the use of the new "field of view" calculations.
 *
 * I wouldn't be surprised if slight modifications to the "update_view()"
 * function would allow us to determine "reverse line-of-sight" as well
 * as "normal line-of-sight", which would allow monsters to use a more
 * "correct" calculation to determine if they can "see" the player.  For
 * now, monsters simply "cheat" somewhat and assume that if the player
 * has "line of sight" to the monster, then the monster can "pretend"
 * that it has "line of sight" to the player.
 *
 *
 * The "update_lite()" function maintains the "MAP_LITE" flag for each
 * grid and maintains an array of all "MAP_LITE" grids.
 *
 * This set of grids is the complete set of all grids which are lit by
 * the players light source, which allows p_ptr->can_see_bold() to work
 * very quickly.
 *
 * Note that every "MAP_LITE" grid is also a "MAP_VIEW" grid, and in
 * fact, the player (unless blind) can always "see" all grids which are
 * marked as "MAP_LITE", unless they are "off screen".
 *
 *
 * The "update_view()" function maintains the "MAP_VIEW" flag for each
 * grid and maintains an array of all "MAP_VIEW" grids.
 *
 * This set of grids is the complete set of all grids within line of sight
 * of the player, allowing the "player_has_los_bold()" macro to work very
 * quickly.
 *
 *
 * The current "update_view()" algorithm uses the "MAP_XTRA" flag as a
 * temporary internal flag to mark those grids which are not only in view,
 * but which are also "easily" in line of sight of the player.  This flag
 * is always cleared when we are done.
 *
 *
 * The current "update_lite()" and "update_view()" algorithms use the
 * "MAP_TEMP" flag, and the array of grids which are marked as "MAP_TEMP",
 * to keep track of which grids were previously marked as "MAP_LITE" or
 * "MAP_VIEW", which allows us to optimize the "screen updates".
 *
 * The "MAP_TEMP" flag, and the array of "MAP_TEMP" grids, is also used
 * for various other purposes, such as spreading lite or darkness during
 * "lite_room()" / "unlite_room()", and for calculating monster flow.
 *
 *
 * Any grid can be marked as "MAP_GLOW" which means that the grid itself is
 * in some way permanently lit.  However, for the player to "see" anything
 * in the grid, as determined by "player_can_see()", the player must not be
 * blind, the grid must be marked as "MAP_VIEW", and, in addition, "wall"
 * grids, even if marked as "perma lit", are only illuminated if they touch
 * a grid which is not a wall and is marked both "MAP_GLOW" and "MAP_VIEW".
 *
 *
 * To simplify various things, a grid may be marked as "MAP_KNOW", meaning
 * that even if the player cannot "see" the grid, he "knows" the terrain in
 * that grid.  This is used to "remember" walls/doors/stairs/floors when they
 * are "seen" or "detected", and also to "memorize" floors, after "wiz_lite()",
 * or when one of the "memorize floor grids" options induces memorization.
 *
 * Items are "memorized" in a different way, using a special "marked" flag
 * on the object itself, which is set when an object is observed or detected.
 *
 *
 * A grid may be marked as "MAP_ROOM" which means that it is part of a "room",
 * and should be illuminated by "lite room" and "darkness" spells.
 *
 *
 * A grid may be marked as "MAP_ICKY" which means it is part of a "vault",
 * and should be unavailable for "teleportation" destinations.
 *
 *
 * Note that the new "update_view()" method allows, among other things, a room
 * to be "partially" seen as the player approaches it, with a growing cone of
 * floor appearing as the player gets closer to the door.  Also, by not turning
 * on the "memorize perma-lit grids" option, the player will only "see" those
 * floor grids which are actually in line of sight.
 *
 * And my favorite "plus" is that you can now use a special option to draw the
 * "floors" in the "viewable region" brightly (actually, to draw the *other*
 * grids dimly), providing a "pretty" effect as the player runs around, and
 * to efficiently display the "torch lite" in a special color.
 *
 *
 * Some comments on the "update_view()" algorithm...
 *
 * The algorithm is very fast, since it spreads "obvious" grids very quickly,
 * and only has to call "los()" on the borderline cases.  The major axes/diags
 * even terminate early when they hit walls.  I need to find a quick way
 * to "terminate" the other scans.
 *
 * Note that in the worst case (a big empty area with say 5% scattered walls),
 * each of the 1500 or so nearby grids is checked once, most of them getting
 * an "instant" rating, and only a small portion requiring a call to "los()".
 *
 * The only time that the algorithm appears to be "noticeably" too slow is
 * when running, and this is usually only important in town, since the town
 * provides about the worst scenario possible, with large open regions and
 * a few scattered obstructions.  There is a special "efficiency" option to
 * allow the player to reduce his field of view in town, if needed.
 *
 * In the "best" case (say, a normal stretch of corridor), the algorithm
 * makes one check for each viewable grid, and makes no calls to "los()".
 * So running in corridors is very fast, and if a lot of monsters are
 * nearby, it is much faster than the old methods.
 *
 * Note that resting, most normal commands, and several forms of running,
 * plus all commands executed near large groups of monsters, are strictly
 * more efficient with "update_view()" that with the old "compute los() on
 * demand" method, primarily because once the "field of view" has been
 * calculated, it does not have to be recalculated until the player moves
 * (or a wall or door is created or destroyed).
 *
 * Note that we no longer have to do as many "los()" checks, since once the
 * "view" region has been built, very few things cause it to be "changed"
 * (player movement, and the opening/closing of doors, changes in wall status).
 * Note that door/wall changes are only relevant when the door/wall itself is
 * in the "view" region.
 *
 * The algorithm seems to only call "los()" from zero to ten times, usually
 * only when coming down a corridor into a room, or standing in a room, just
 * misaligned with a corridor.  So if, say, there are five "nearby" monsters,
 * we will be reducing the calls to "los()".
 *
 * I am thinking in terms of an algorithm that "walks" from the central point
 * out to the maximal "distance", at each point, determining the "view" code
 * (above).  For each grid not on a major axis or diagonal, the "view" code
 * depends on the "floor_grid_bold()" and "view" of exactly two other grids
 * (the one along the nearest diagonal, and the one next to that one, see
 * "update_view_aux()"...).
 *
 * We "memorize" the viewable space array, so that at the cost of under 3000
 * bytes, we reduce the time taken by "forget_view()" to one assignment for
 * each grid actually in the "viewable space".  And for another 3000 bytes,
 * we prevent "erase + redraw" ineffiencies via the "seen" set.  These bytes
 * are also used by other routines, thus reducing the cost to almost nothing.
 *
 * A similar thing is done for "forget_lite()" in which case the savings are
 * much less, but save us from doing bizarre maintenance checking.
 *
 * In the worst "normal" case (in the middle of the town), the reachable space
 * actually reaches to more than half of the largest possible "circle" of view,
 * or about 800 grids, and in the worse case (in the middle of a dungeon level
 * where all the walls have been removed), the reachable space actually reaches
 * the theoretical maximum size of just under 1500 grids.
 *
 * Each grid G examines the "state" of two (?) other (adjacent) grids, G1 & G2.
 * If G1 is lite, G is lite.  Else if G2 is lite, G is half.  Else if G1 and G2
 * are both half, G is half.  Else G is dark.  It only takes 2 (or 4) bits to
 * "name" a grid, so (for MAX_RAD of 20) we could use 1600 bytes, and scan the
 * entire possible space (including initialization) in one step per grid.  If
 * we do the "clearing" as a separate step (and use an array of "view" grids),
 * then the clearing will take as many steps as grids that were viewed, and the
 * algorithm will be able to "stop" scanning at various points.
 * Oh, and outside of the "torch radius", only "lite" grids need to be scanned.
 */








/*
 * Actually erase the entire "lite" array, redrawing every grid
 */
void forget_lite(void)
{
    int i, x, y;

    /* Clear them all */
    for (i = 0; i < lite_n; i++) {
        x = lite[i].x;
        y = lite[i].y;

        /* Forget "LITE" flag */
        cave[y][x].flags &= ~MAP_LITE;
    }

    /* None left */
    lite_n = 0;
}


/*
 * This function allows us to efficiently add a grid to the "lite" array.
 * Note that we are never called for illegal grids, or for grids which
 * have already been placed into the "lite" array, and we are never
 * called when the "lite" array is full.
 */
static inline void cave_lite_hack(int y, int x)
{
    cave[y][x].flags |= MAP_LITE;
    lite[lite_n].x = x;
    lite[lite_n].y = y;
    lite_n++;
}


/*
 * Update the set of grids "illuminated" by the player's lite.
 *
 * This routine needs to use the results of "update_view()"
 *
 * Note that "blindness" does NOT affect "torch lite".  Be careful!
 *
 * We optimize most lites (all non-artifact lites) by using "obvious"
 * facts about the results of "small" lite radius, and we attempt to
 * list the "nearby" grids before the more "distant" ones in the
 * array of torch-lit grids.
 *
 * We will correctly handle "large" radius lites, though currently,
 * it is impossible for the player to have more than radius 3 lite.
 *
 * We assume that "radius zero" lite is in fact no lite at all.
 *
 *     Torch     Lantern     Artifacts
 *     (etc)
 *                              ***
 *                 ***         *****
 *      ***       *****       *******
 *      *@*       **@**       ***@***
 *      ***       *****       *******
 *                 ***         *****
 *                              ***
 */
void update_lite(void)
{
    int i, x, y, min_x, max_x, min_y, max_y;


    /*** Special case ***/

    // Hack -- Player has no lite
    if (p_ptr->get_cur_lite() <= 0) {
        // Forget the old lite
        forget_lite();

        // All done
        return;
    }


    /*** Save the old "lite" grids for later ***/

    // Clear them all
    for (i = 0; i < lite_n; i++) {
        x = lite[i].x;
        y = lite[i].y;

        // Mark the grid as not "lite"
        cave[y][x].flags &= ~MAP_LITE;

        // Mark the grid as "seen"
        cave[y][x].flags |= MAP_TEMP;

        // Add it to the "seen" set
        temp_y[temp_n] = y;
        temp_x[temp_n] = x;
        temp_n++;
    }

    /* None left */
    lite_n = 0;


    /*** Collect the new "lite" grids ***/

    // Player grid
    cave_lite_hack(p_ptr->GetY(), p_ptr->GetX());

    // Radius 1 -- torch radius
    if (p_ptr->get_cur_lite() >= 1) {
        // Adjacent grid
        cave_lite_hack(p_ptr->GetY()+1, p_ptr->GetX());
        cave_lite_hack(p_ptr->GetY()-1, p_ptr->GetX());
        cave_lite_hack(p_ptr->GetY(), p_ptr->GetX()+1);
        cave_lite_hack(p_ptr->GetY(), p_ptr->GetX()-1);

        /* Diagonal grids */
        cave_lite_hack(p_ptr->GetY()+1, p_ptr->GetX()+1);
        cave_lite_hack(p_ptr->GetY()+1, p_ptr->GetX()-1);
        cave_lite_hack(p_ptr->GetY()-1, p_ptr->GetX()+1);
        cave_lite_hack(p_ptr->GetY()-1, p_ptr->GetX()-1);
    }

    /* Radius 2 -- lantern radius */
    if (p_ptr->get_cur_lite() >= 2) {
        /* South of the player */
        if (floor_grid_bold(p_ptr->GetY()+1, p_ptr->GetX())) {
            cave_lite_hack(p_ptr->GetY()+2, p_ptr->GetX());
            cave_lite_hack(p_ptr->GetY()+2, p_ptr->GetX()+1);
            cave_lite_hack(p_ptr->GetY()+2, p_ptr->GetX()-1);
        }

        /* North of the player */
        if (floor_grid_bold(p_ptr->GetY()-1, p_ptr->GetX())) {
            cave_lite_hack(p_ptr->GetY()-2, p_ptr->GetX());
            cave_lite_hack(p_ptr->GetY()-2, p_ptr->GetX()+1);
            cave_lite_hack(p_ptr->GetY()-2, p_ptr->GetX()-1);
        }

        /* East of the player */
        if (floor_grid_bold(p_ptr->GetY(), p_ptr->GetX()+1)) {
            cave_lite_hack(p_ptr->GetY(), p_ptr->GetX()+2);
            cave_lite_hack(p_ptr->GetY()+1, p_ptr->GetX()+2);
            cave_lite_hack(p_ptr->GetY()-1, p_ptr->GetX()+2);
        }

        /* West of the player */
        if (floor_grid_bold(p_ptr->GetY(), p_ptr->GetX()-1)) {
            cave_lite_hack(p_ptr->GetY(), p_ptr->GetX()-2);
            cave_lite_hack(p_ptr->GetY()+1, p_ptr->GetX()-2);
            cave_lite_hack(p_ptr->GetY()-1, p_ptr->GetX()-2);
        }
    }

    /* Radius 3+ -- artifact radius */
    if (p_ptr->get_cur_lite() >= 3) {
        int d, p;

        /* Maximal radius */
        p = p_ptr->get_cur_lite();

        /* Paranoia -- see "LITE_MAX" */
        if (p > 5) p = 5;

        /* South-East of the player */
        if (floor_grid_bold(p_ptr->GetY()+1, p_ptr->GetX()+1)) {
            cave_lite_hack(p_ptr->GetY()+2, p_ptr->GetX()+2);
        }

        /* South-West of the player */
        if (floor_grid_bold(p_ptr->GetY()+1, p_ptr->GetX()-1)) {
            cave_lite_hack(p_ptr->GetY()+2, p_ptr->GetX()-2);
        }

        /* North-East of the player */
        if (floor_grid_bold(p_ptr->GetY()-1, p_ptr->GetX()+1)) {
            cave_lite_hack(p_ptr->GetY()-2, p_ptr->GetX()+2);
        }

        /* North-West of the player */
        if (floor_grid_bold(p_ptr->GetY()-1, p_ptr->GetX()-1)) {
            cave_lite_hack(p_ptr->GetY()-2, p_ptr->GetX()-2);
        }

        /* Maximal north */
        min_y = p_ptr->GetY() - p;
        if (min_y < 0) min_y = 0;

        /* Maximal south */
        max_y = p_ptr->GetY() + p;
        if (max_y > cur_hgt-1) max_y = cur_hgt-1;

        /* Maximal west */
        min_x = p_ptr->GetX() - p;
        if (min_x < 0) min_x = 0;
        
        /* Maximal east */
        max_x = p_ptr->GetX() + p;
        if (max_x > cur_wid-1) max_x = cur_wid-1;

        /* Scan the maximal box */
        for (y = min_y; y <= max_y; y++) {
            for (x = min_x; x <= max_x; x++) {
                int dy = (p_ptr->GetY() > y) ? (p_ptr->GetY() - y) :
                    (y - p_ptr->GetY());
                int dx = (p_ptr->GetX() > x) ? (p_ptr->GetX() - x) :
                    (x - p_ptr->GetX());

                /* Skip the "central" grids (above) */
                if ((dy <= 2) && (dx <= 2)) continue;

                /* Hack -- approximate the distance */
                d = (dy > dx) ? (dy + (dx>>1)) : (dx + (dy>>1));

                /* Skip distant grids */
                if (d > p) continue;

                /* Viewable, nearby, grids get "torch lit" */
                if (player_has_los_bold(y, x)) {
                    /* This grid is "torch lit" */
                    cave_lite_hack(y, x);
                }
            }
        }
    }


    /*** Complete the algorithm ***/

    /* Draw the new grids */
    for (i = 0; i < lite_n; i++) {
        y = lite[i].y;
        x = lite[i].x;

        /* Update fresh grids */
        if (cave[y][x].flags & MAP_TEMP) continue;

        /* Note */
        note_spot(y, x);
    }

    /* Clear them all */
    for (i = 0; i < temp_n; i++) {
        y = temp_y[i];
        x = temp_x[i];

        /* No longer in the array */
        cave[y][x].flags &= ~MAP_TEMP;

        /* Update stale grids */
        if (cave[y][x].flags & MAP_LITE) continue;
    }

    /* None left */
    temp_n = 0;
}


/*
 * Clear the viewable space
 */
void forget_view(void)
{
    int i;
    CGrid *g_ptr;

    /* Clear them all */
    for (i = 0; i < view_n; i++) {
        int y = view[i].y;
        int x = view[i].x;

        /* Access the grid */
        g_ptr = &cave[y][x];

        /* Forget that the grid is viewable */
        g_ptr->flags &= ~MAP_VIEW;
    }

    /* None left */
    view_n = 0;
}



/*
 * Hack -- Local version of "floor_grid_bold(Y,X)"
 */
#define floor_grid_hack(C) \
    (!((C)->get_feat() & 0x20))

/*
 * This function allows us to efficiently add a grid to the "view" array.
 * Note that we are never called for illegal grids, or for grids which
 * have already been placed into the "view" array, and we are never
 * called when the "view" array is full.
 */
static inline void cave_view_hack(CGrid *g_ptr, int y, int x)
{
    g_ptr->flags |= MAP_VIEW;
    view[view_n].x = x;
    view[view_n].y = y;
    view_n++;
}



/*
 * Helper function for "update_view()" below
 *
 * We are checking the "viewability" of grid (y,x) by the player.
 *
 * This function assumes that (y,x) is legal (i.e. on the map).
 *
 * Grid (y1,x1) is on the "diagonal" between (py,px) and (y,x)
 * Grid (y2,x2) is "adjacent", also between (py,px) and (y,x).
 *
 * Note that we are using the "MAP_XTRA" field for marking grids as
 * "easily viewable".  This bit is cleared at the end of "update_view()".
 *
 * This function adds (y,x) to the "viewable set" if necessary.
 *
 * This function now returns "TRUE" if vision is "blocked" by grid (y,x).
 */
static bool update_view_aux(int y, int x, int y1, int x1, int y2, int x2)
{
    bool f1, f2, v1, v2, z1, z2, wall;

    CGrid *g_ptr;

    CGrid *g1_g_ptr;
    CGrid *g2_g_ptr;


    /* Access the grids */
    g1_g_ptr = &cave[y1][x1];
    g2_g_ptr = &cave[y2][x2];


    /* Check for walls */
    f1 = (floor_grid_hack(g1_g_ptr));
    f2 = (floor_grid_hack(g2_g_ptr));

    /* Totally blocked by physical walls */
    if (!f1 && !f2) return (TRUE);


    /* Check for visibility */
    v1 = (f1 && (g1_g_ptr->flags & MAP_VIEW));
    v2 = (f2 && (g2_g_ptr->flags & MAP_VIEW));

    /* Totally blocked by "unviewable neighbors" */
    if (!v1 && !v2) return (TRUE);


    /* Access the grid */
    g_ptr = &cave[y][x];


    /* Check for walls */
    wall = (!floor_grid_hack(g_ptr));


    /* Check the "ease" of visibility */
    z1 = (v1 && (g1_g_ptr->flags & MAP_XTRA));
    z2 = (v2 && (g2_g_ptr->flags & MAP_XTRA));

    /* Hack -- "easy" plus "easy" yields "easy" */
    if (z1 && z2) {
        g_ptr->flags |= MAP_XTRA;
        cave_view_hack(g_ptr, y, x);
        return wall;
    }

    /* Hack -- primary "easy" yields "viewed" */
    if (z1) {
        cave_view_hack(g_ptr, y, x);
        return wall;
    }


    /* Hack -- "view" plus "view" yields "view" */
    if (v1 && v2) {
        /* g_ptr->flags |= MAP_XTRA; */
        cave_view_hack(g_ptr, y, x);
        return wall;
    }


    /* Mega-Hack -- the "los()" function works poorly on walls */
    if (wall) {
        cave_view_hack(g_ptr, y, x);
        return wall;
    }


    /* Hack -- check line of sight */
    if (los(p_ptr->GetY(), p_ptr->GetX(), y, x)) {
        cave_view_hack(g_ptr, y, x);
        return wall;
    }


    // Assume no line of sight
    return TRUE;
}



/*
 * Calculate the viewable space
 *
 *  1: Process the player
 *  1a: The player is always (easily) viewable
 *  2: Process the diagonals
 *  2a: The diagonals are (easily) viewable up to the first wall
 *  2b: But never go more than 2/3 of the "full" distance
 *  3: Process the main axes
 *  3a: The main axes are (easily) viewable up to the first wall
 *  3b: But never go more than the "full" distance
 *  4: Process sequential "strips" in each of the eight octants
 *  4a: Each strip runs along the previous strip
 *  4b: The main axes are "previous" to the first strip
 *  4c: Process both "sides" of each "direction" of each strip
 *  4c1: Each side aborts as soon as possible
 *  4c2: Each side tells the next strip how far it has to check
 *
 * Note that the octant processing involves some pretty interesting
 * observations involving when a grid might possibly be viewable from
 * a given grid, and on the order in which the strips are processed.
 *
 * Note the use of the mathematical facts shown below, which derive
 * from the fact that (1 < sqrt(2) < 1.5), and that the length of the
 * hypotenuse of a right triangle is primarily determined by the length
 * of the longest side, when one side is small, and is strictly less
 * than one-and-a-half times as long as the longest side when both of
 * the sides are large.
 *
 *   if (manhatten(dy,dx) < R) then (hypot(dy,dx) < R)
 *   if (manhatten(dy,dx) > R*3/2) then (hypot(dy,dx) > R)
 *
 *   hypot(dy,dx) is approximated by (dx+dy+MAX(dx,dy)) / 2
 *
 * These observations are important because the calculation of the actual
 * value of "hypot(dx,dy)" is extremely expensive, involving square roots,
 * while for small values (up to about 20 or so), the approximations above
 * are correct to within an error of at most one grid or so.
 *
 * Observe the use of "full" and "over" in the code below, and the use of
 * the specialized calculation involving "limit", all of which derive from
 * the observations given above.  Basically, we note that the "circle" of
 * view is completely contained in an "octagon" whose bounds are easy to
 * determine, and that only a few steps are needed to derive the actual
 * bounds of the circle given the bounds of the octagon.
 *
 * Note that by skipping all the grids in the corners of the octagon, we
 * place an upper limit on the number of grids in the field of view, given
 * that "full" is never more than 20.  Of the 1681 grids in the "square" of
 * view, only about 1475 of these are in the "octagon" of view, and even
 * fewer are in the "circle" of view, so 1500 or 1536 is more than enough
 * entries to completely contain the actual field of view.
 *
 * Note also the care taken to prevent "running off the map".  The use of
 * explicit checks on the "validity" of the "diagonal", and the fact that
 * the loops are never allowed to "leave" the map, lets "update_view_aux()"
 * use the optimized "floor_grid_bold()" macro, and to avoid the overhead
 * of multiple checks on the validity of grids.
 *
 * Note the "optimizations" involving the "se","sw","ne","nw","es","en",
 * "ws","wn" variables.  They work like this: While travelling down the
 * south-bound strip just to the east of the main south axis, as soon as
 * we get to a grid which does not "transmit" viewing, if all of the strips
 * preceding us (in this case, just the main axis) had terminated at or before
 * the same point, then we can stop, and reset the "max distance" to ourself.
 * So, each strip (named by major axis plus offset, thus "se" in this case)
 * maintains a "blockage" variable, initialized during the main axis step,
 * and checks it whenever a blockage is observed.  After processing each
 * strip as far as the previous strip told us to process, the next strip is
 * told not to go farther than the current strip's farthest viewable grid,
 * unless open space is still available.  This uses the "k" variable.
 *
 * Note the use of "inline" macros for efficiency.  The "floor_grid_hack()"
 * macro is a replacement for "floor_grid_bold()" which takes a pointer to
 * a cave grid instead of its location.  The "cave_view_hack()" macro is a
 * chunk of code which adds the given location to the "view" array if it
 * is not already there, using both the actual location and a pointer to
 * the cave grid.  See above.
 *
 * By the way, the purpose of this code is to reduce the dependancy on the
 * "los()" function which is slow, and, in some cases, not very accurate.
 *
 * It is very possible that I am the only person who fully understands this
 * function, and for that I am truly sorry, but efficiency was very important
 * and the "simple" version of this function was just not fast enough.  I am
 * more than willing to replace this function with a simpler one, if it is
 * equally efficient, and especially willing if the new function happens to
 * derive "reverse-line-of-sight" at the same time, since currently monsters
 * just use an optimized hack of "you see me, so I see you", and then use the
 * actual "projectable()" function to check spell attacks.
 */
void update_view(void)
{
    int n, m, d, k, y, x, z;
    int se, sw, ne, nw, es, en, ws, wn;
    int full, over;
    CGrid *g_ptr;


    /*** Initialize ***/

    /* Full radius (20) */
    full = MAX_SIGHT;

    /* Octagon factor (30) */
    over = MAX_SIGHT * 3 / 2;


    /*** Step 0 -- Begin ***/

    /* Save the old "view" grids for later */
    for (n = 0; n < view_n; n++) {
        x = view[n].x;
        y = view[n].y;

        /* Access the grid */
        g_ptr = &cave[y][x];

        /* Mark the grid as not in "view" */
        g_ptr->flags &= ~(MAP_VIEW);

        /* Mark the grid as "seen" */
        g_ptr->flags |= MAP_TEMP;

        /* Add it to the "seen" set */
        temp_x[temp_n] = x;
        temp_y[temp_n] = y;
        temp_n++;
    }

    /* Start over with the "view" array */
    view_n = 0;


    /*** Step 1 -- adjacent grids ***/

    /* Now start on the player */
    x = p_ptr->GetX();
    y = p_ptr->GetY();

    /* Access the grid */
    g_ptr = &cave[y][x];

    /* Assume the player grid is easily viewable */
    g_ptr->flags |= MAP_XTRA;

    /* Assume the player grid is viewable */
    cave_view_hack(g_ptr, y, x);


    /*** Step 2 -- Major Diagonals ***/

    /* Hack -- Limit */
    z = full * 2 / 3;

    /* Scan south-east */
    for (d = 1; d <= z; d++) {
        g_ptr = &cave[y+d][x+d];
        g_ptr->flags |= MAP_XTRA;
        cave_view_hack(g_ptr, y+d, x+d);
        if (!floor_grid_hack(g_ptr)) break;
    }

    /* Scan south-west */
    for (d = 1; d <= z; d++) {
        g_ptr = &cave[y+d][x-d];
        g_ptr->flags |= MAP_XTRA;
        cave_view_hack(g_ptr, y+d, x-d);
        if (!floor_grid_hack(g_ptr)) break;
    }

    /* Scan north-east */
    for (d = 1; d <= z; d++) {
        g_ptr = &cave[y-d][x+d];
        g_ptr->flags |= MAP_XTRA;
        cave_view_hack(g_ptr, y-d, x+d);
        if (!floor_grid_hack(g_ptr)) break;
    }

    /* Scan north-west */
    for (d = 1; d <= z; d++) {
        g_ptr = &cave[y-d][x-d];
        g_ptr->flags |= MAP_XTRA;
        cave_view_hack(g_ptr, y-d, x-d);
        if (!floor_grid_hack(g_ptr)) break;
    }


    /*** Step 3 -- major axes ***/

    /* Scan south */
    for (d = 1; d <= full; d++) {
        g_ptr = &cave[y+d][x];
        g_ptr->flags |= MAP_XTRA;
        cave_view_hack(g_ptr, y+d, x);
        if (!floor_grid_hack(g_ptr)) break;
    }

    /* Initialize the "south strips" */
    se = sw = d;

    /* Scan north */
    for (d = 1; d <= full; d++) {
        g_ptr = &cave[y-d][x];
        g_ptr->flags |= MAP_XTRA;
        cave_view_hack(g_ptr, y-d, x);
        if (!floor_grid_hack(g_ptr)) break;
    }

    /* Initialize the "north strips" */
    ne = nw = d;

    /* Scan east */
    for (d = 1; d <= full; d++) {
        g_ptr = &cave[y][x+d];
        g_ptr->flags |= MAP_XTRA;
        cave_view_hack(g_ptr, y, x+d);
        if (!floor_grid_hack(g_ptr)) break;
    }

    /* Initialize the "east strips" */
    es = en = d;

    /* Scan west */
    for (d = 1; d <= full; d++) {
        g_ptr = &cave[y][x-d];
        g_ptr->flags |= MAP_XTRA;
        cave_view_hack(g_ptr, y, x-d);
        if (!floor_grid_hack(g_ptr)) break;
    }

    /* Initialize the "west strips" */
    ws = wn = d;


    /*** Step 4 -- Divide each "octant" into "strips" ***/

    /* Now check each "diagonal" (in parallel) */
    for (n = 1; n <= over / 2; n++) {
        int ypn, ymn, xpn, xmn;


        // Acquire the "bounds" of the maximal circle
        z = over - n - n;
        if (z > full - n) z = full - n; 
        while ((z + n + (n>>1)) > full) z--;


        /* Access the four diagonal grids */
        ypn = y + n;
        ymn = y - n;
        xpn = x + n;
        xmn = x - n;


        /* South strip */
        if (ypn < cur_hgt-1) {
            /* Maximum distance */
            m = MIN(z, (cur_hgt-1) - ypn);

            /* East side */
            if ((xpn <= cur_wid-1) && (n < se)) {
                /* Scan */
                for (k = n, d = 1; d <= m; d++) {
                    /* Check grid "d" in strip "n", notice "blockage" */
                    if (update_view_aux(ypn+d,xpn,ypn+d-1,xpn-1,ypn+d-1,xpn)) {
                        if (n + d >= se) break;
                    }

                    /* Track most distant "non-blockage" */
                    else {
                        k = n + d;
                    }
                }

                /* Limit the next strip */
                se = k + 1;
            }

            // West side
            if ((xmn >= 0) && (n < sw)) {
                // Scan
                for (k = n, d = 1; d <= m; d++) {
                    // Check grid "d" in strip "n", notice "blockage"
                    if (update_view_aux(ypn+d,xmn,ypn+d-1,xmn+1,ypn+d-1,xmn)) {
                        if (n + d >= sw) break;
                    }

                    /* Track most distant "non-blockage" */
                    else {
                        k = n + d;
                    }
                }

                /* Limit the next strip */
                sw = k + 1;
            }
        }


        /* North strip */
        if (ymn > 0) {
            /* Maximum distance */
            m = MIN(z, ymn);

            /* East side */
            if ((xpn <= cur_wid-1) && (n < ne)) {
                /* Scan */
                for (k = n, d = 1; d <= m; d++) {
                    /* Check grid "d" in strip "n", notice "blockage" */
                    if (update_view_aux(ymn-d,xpn,ymn-d+1,xpn-1,ymn-d+1,xpn)) {
                        if (n + d >= ne) break;
                    }

                    /* Track most distant "non-blockage" */
                    else {
                        k = n + d;
                    }
                }

                // Limit the next strip
                ne = k + 1;
            }

            // West side
            if ((xmn >= 0) && (n < nw)) {
                // Scan
                for (k = n, d = 1; d <= m; d++) {
                    /* Check grid "d" in strip "n", notice "blockage" */
                    if (update_view_aux(ymn-d,xmn,ymn-d+1,xmn+1,ymn-d+1,xmn)) {
                        if (n + d >= nw) break;
                    }

                    // Track most distant "non-blockage"
                    else {
                        k = n + d;
                    }
                }

                /* Limit the next strip */
                nw = k + 1;
            }
        }


        // East strip
        if (xpn < cur_wid-1) {
            // Maximum distance
            m = MIN(z, (cur_wid-1) - xpn);

            // South side
            if ((ypn <= cur_hgt-1) && (n < es)) {
                /* Scan */
                for (k = n, d = 1; d <= m; d++) {
                    // Check grid "d" in strip "n", notice "blockage"
                    if (update_view_aux(ypn,xpn+d,ypn-1,xpn+d-1,ypn,xpn+d-1)) {
                        if (n + d >= es) break;
                    }

                    // Track most distant "non-blockage"
                    else {
                        k = n + d;
                    }
                }

                /* Limit the next strip */
                es = k + 1;
            }

            /* North side */
            if ((ymn >= 0) && (n < en)) {
                /* Scan */
                for (k = n, d = 1; d <= m; d++) {
                    /* Check grid "d" in strip "n", notice "blockage" */
                    if (update_view_aux(ymn,xpn+d,ymn+1,xpn+d-1,ymn,xpn+d-1)) {
                        if (n + d >= en) break;
                    }

                    /* Track most distant "non-blockage" */
                    else {
                        k = n + d;
                    }
                }

                /* Limit the next strip */
                en = k + 1;
            }
        }


        /* West strip */
        if (xmn > 0) {
            /* Maximum distance */
            m = MIN(z, xmn);

            /* South side */
            if ((ypn <= cur_hgt-1) && (n < ws)) {
                /* Scan */
                for (k = n, d = 1; d <= m; d++) {
                    /* Check grid "d" in strip "n", notice "blockage" */
                    if (update_view_aux(ypn,xmn-d,ypn-1,xmn-d+1,ypn,xmn-d+1)) {
                        if (n + d >= ws) break;
                    }

                    /* Track most distant "non-blockage" */
                    else {
                        k = n + d;
                    }
                }

                /* Limit the next strip */
                ws = k + 1;
            }

            /* North side */
            if ((ymn >= 0) && (n < wn))
            {
                /* Scan */
                for (k = n, d = 1; d <= m; d++)
                {
                    /* Check grid "d" in strip "n", notice "blockage" */
                    if (update_view_aux(ymn,xmn-d,ymn+1,xmn-d+1,ymn,xmn-d+1))
                    {
                        if (n + d >= wn) break;
                    }

                    /* Track most distant "non-blockage" */
                    else
                    {
                        k = n + d;
                    }
                }

                /* Limit the next strip */
                wn = k + 1;
            }
        }
    }


    /*** Step 5 -- Complete the algorithm ***/

    /* Update all the new grids */
    for (n = 0; n < view_n; n++) {
        y = view[n].y;
        x = view[n].x;

        /* Access the grid */
        g_ptr = &cave[y][x];

        /* Clear the "MAP_XTRA" flag */
        g_ptr->flags &= ~MAP_XTRA;

        /* Update only newly viewed grids */
        if (g_ptr->flags & MAP_TEMP) continue;

        /* Note */
        note_spot(y, x);
    }

    /* Wipe the old grids, update as needed */
    for (n = 0; n < temp_n; n++) {
        y = temp_y[n];
        x = temp_x[n];

        /* Access the grid */
        g_ptr = &cave[y][x];

        /* No longer in the array */
        g_ptr->flags &= ~MAP_TEMP;

        /* Update only non-viewable grids */
        if (g_ptr->flags & MAP_VIEW) continue;
    }

    /* None left */
    temp_n = 0;
}






/*
 * Hack -- map a 20-grid radius ala "magic mapping"
 */
void map_area(void)
{
    int x, y;
    CGrid *g_ptr;

    /* Scan that area */
    for (x = p_ptr->GetX() - 20; x <= p_ptr->GetX() + 20; x++) {
        for (y = p_ptr->GetY() - 20; y <= p_ptr->GetY() + 20; y++) {
            // Must be in bounds
            if (!in_bounds2(y, x)) continue;

            // Must be within 20 squares
            if (distance(p_ptr->GetX(), p_ptr->GetY(), x, y) > 20) continue;

            // Get the g_ptr
            g_ptr = &cave[y][x];

            // Memorize
            g_ptr->flags |= MAP_KNOW;
        }
    }
}



/*
 * Light up the dungeon.
 *
 * XXX XXX XXX Hack -- This function is basically a hack.
 */
void wiz_lite(void)
{
    int y, x;

    CGrid *g_ptr;


    // Perma-light everything
    for (x = 0; x < cur_wid; x++) {
        for (y = 0; y < cur_hgt; y++) {
            /* Access the grid */
            g_ptr = &cave[y][x];

            /* Memorize all objects */
            memorize_all(x, y);

            // Memorize the grid
            g_ptr->flags |= MAP_KNOW;
        }
    }

    /* Update the monsters */
    p_ptr->set_update(p_ptr->get_update() | PU_MONSTERS);
}


/*
 * Forget the dungeon map (ala "Thinking of Maud...").
 */
void forget_map(void)
{
    int x, y;


    /* Forget every grid */
    for (x = 0; x < cur_wid; x++) {
        for (y = 0; y < cur_hgt; y++) {
            CGrid *g_ptr = &cave[y][x];

            /* Process the grid */
            g_ptr->flags &= ~MAP_KNOW;

            /* Forget the objects */
            forget_all(x, y);
        }
    }

    // Mega-Hack -- Forget the view and lite
    p_ptr->set_update(p_ptr->get_update() | PU_UN_VIEW | PU_UN_LITE);

    // Update the view and lite
    p_ptr->set_update(p_ptr->get_update() | PU_VIEW | PU_LITE);

    // Update the monsters 
    p_ptr->set_update(p_ptr->get_update() | PU_MONSTERS);
}





/*
 * Calculate "incremental motion". Used by project() and shoot().
 * Assumes that (*y,*x) lies on the path from (y1,x1) to (y2,x2).
 */
void mmove2(int *y, int *x, int y1, int x1, int y2, int x2)
{
    int dx, dy, dist, shift;

    /* Extract the distance travelled */
    dx = (*x < x1) ? x1 - *x : *x - x1;
    dy = (*y < y1) ? y1 - *y : *y - y1;

    /* Number of steps */
    dist = (dy > dx) ? dy : dx;

    /* We are calculating the next location */
    dist++;


    /* Calculate the total distance along each axis */
    dy = (y2 < y1) ? (y1 - y2) : (y2 - y1);
    dx = (x2 < x1) ? (x1 - x2) : (x2 - x1);

    /* Paranoia -- Hack -- no motion */
    if (!dy && !dx) return;


    /* Move mostly vertically */
    if (dy > dx) {
        /* Extract a shift factor */
        shift = (dist * dx + (dy-1) / 2) / dy;

        /* Sometimes move along the minor axis */
        *x = (x2 < x1) ? (x1 - shift) : (x1 + shift);

        /* Always move along major axis */
        *y = (y2 < y1) ? (y1 - dist) : (y1 + dist);
    }

    /* Move mostly horizontally */
    else {
        /* Extract a shift factor */
        shift = (dist * dy + (dx-1) / 2) / dx;

        /* Sometimes move along the minor axis */
        *y = (y2 < y1) ? (y1 - shift) : (y1 + shift);

        /* Always move along major axis */
        *x = (x2 < x1) ? (x1 - dist) : (x1 + dist);
    }
}


/*
 * Determine if a bolt spell cast from (y1,x1) to (y2,x2) will arrive
 * at the final destination, assuming no monster gets in the way.
 *
 * This is slightly (but significantly) different from "los(y1,x1,y2,x2)".
 */
bool projectable(int y1, int x1, int y2, int x2)
{
    int dist, y, x;

    /* Start at the initial location */
    y = y1, x = x1;

    /* See "project()" */
    for (dist = 0; dist < MAX_RANGE; dist++) {
        /* Never pass through walls */
        if (dist && !floor_grid_bold(y, x)) break;

        /* Check for arrival at "final target" */
        if ((x == x2) && (y == y2)) return TRUE;

        /* Calculate the new location */
        mmove2(&y, &x, y1, x1, y2, x2);
    }


    /* Assume obstruction */
    return FALSE;
}



/*
 * Standard "find me a location" function
 *
 * Obtains a legal location within the given distance of the initial
 * location, and with "los()" from the source to destination location
 *
 * This function is often called from inside a loop which searches for
 * locations while increasing the "d" distance.
 *
 * Currently the "m" parameter is unused.
 */
void scatter(int *yp, int *xp, int y, int x, int d, int m)
{
    int nx, ny;

    /* Unused */
    m = m;

    /* Pick a location */
    while (TRUE) {
        /* Pick a new location */
        nx = rand_spread(x, d);
        ny = rand_spread(y, d);

        /* Ignore illegal locations and outer walls */
        if (!in_bounds(y, x)) continue;

        /* Ignore "excessively distant" locations */
        if ((d > 1) && (distance(x, y, nx, ny) > d)) continue;

        /* Require "line of sight" */
        if (los(y, x, ny, nx)) break;
    }

    /* Save the location */
    *xp = nx;
    *yp = ny;
}




/*
 * Hack -- Check if a level is a "quest" level
 */
bool is_quest(int level)
{
    int i;

    /* Town is never a quest */
    if (!level) return FALSE;

    /* Check quests */
    for (i = 0; i < MAX_Q_IDX; i++) {
        /* Check for quest */
        if (q_list[i].level == level) return TRUE;
    }

    /* Nope */
    return FALSE;
}



/*
 * Convert from subtile coordinates to pixel coordinates.
 */
void subtile_to_pixel(int sx, int sy, int *px, int *py)
{
    *px = (int)floor(((float)(sx-sy))*32/1000 + 0.5);
    *py = (int)floor(((float)(sx+sy))*16/1000 + 0.5);
}