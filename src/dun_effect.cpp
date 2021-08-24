/* File: effects.c */

/*
 * Special lingering spell effects.
 *
 * Copyright (c) 2007
 * Diego Gonzalez and Jeff Greene
 *
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
 */

#include "src/npp.h"


/* Maximum number of delayed effect bursts */
#define MAX_BURST 100


// The game assumes these two vectors are the same size
/* Coordinates of burst effects */
static QVector<coord> effect_bursts;

/* Spell types of burst effects */
static QVector<byte> effect_burst_gf;



/*
 * Get the indexes of effects at a given effects grid.
 *
 * Return the number of effect indexes acquired.
 *
 * Never acquire more than "size" object indexes, and never return a
 * number bigger than "size", even if more floor objects exist.
 *
 */
int scan_effects_grid(int *effects, int size, int y, int x)
{
    int this_x_idx, next_x_idx;

    int num = 0;

    /* Sanity */
    if (!in_bounds(y, x)) return (0);

    /* Scan all effects in the grid */
    for (this_x_idx = dungeon_info[y][x].effect_idx; this_x_idx; this_x_idx = next_x_idx)
    {
        effect_type *x_ptr;

        /* Get the effect */
        x_ptr = &x_list[this_x_idx];

        /* Get the next effect */
        next_x_idx = x_ptr->next_x_idx;

        /* Accept this item */
        effects[num++] = this_x_idx;

        /* Enforce size limit */
        if (num >= size) break;
    }

    /* Result */
    return (num);
}


/*
 * Determine if an effect can "absorb" a second effect
 *
 * See "effect_absorb()" for the actual "absorption" code.
 *
 */
static bool effect_similar(const effect_type *x_ptr, const effect_type *x2_ptr)
{
    /*Require identical coordinates*/
    if (x_ptr->x_cur_y != x2_ptr->x_cur_y)		return (FALSE);
    if (x_ptr->x_cur_x != x2_ptr->x_cur_x)		return (FALSE);

    /* Require certain types */
    if (x_ptr->x_type != x2_ptr->x_type)		return (FALSE);
    if (x_ptr->x_type == EFFECT_TRAP_SMART)		return (FALSE);
    if (x_ptr->x_type == EFFECT_TRAP_DUMB)		return (FALSE);
    if (x_ptr->x_type == EFFECT_TRAP_PLAYER)	return (FALSE);
    if (x_ptr->x_type == EFFECT_GLYPH)			return (FALSE);
    if (x_ptr->x_type == EFFECT_ROCKS)			return (FALSE);

    /* Require identical terrain types (this check also assures similar gf types) */
    if (x_ptr->x_f_idx != x2_ptr->x_f_idx)		return (FALSE);

    /* Require identical sources types */
    if (x_ptr->x_source != x2_ptr->x_source)	return (FALSE);

    /* They match, so they must be similar */
    return (TRUE);
}


/*
 * Allow one effect to "absorb" another, assuming they are similar.
 *
 * These assumptions are enforced by the "effect_similar()" code.
 */
static void effect_absorb(effect_type *x_ptr, const effect_type *x2_ptr)
{

    switch (x_ptr->x_type)
    {
        case EFFECT_PERMANENT_CLOUD:
        {
            /* Simply increase the power */
            x_ptr->x_power += x2_ptr->x_power;

            if (x_ptr->x_power > EFFECT_POWER_MAX) x_ptr->x_power = EFFECT_POWER_MAX;

            break;
        }
        case EFFECT_SHIMMERING_CLOUD:
        {
            u32b power =  (MAX(x_ptr->x_power,  1) * MAX(x_ptr->x_countdown,  1));
            u32b power2 = (MAX(x2_ptr->x_power, 1) * MAX(x2_ptr->x_countdown, 1));

            /* Add the countdown, and extract the power */
            int new_countdown = x_ptr->x_countdown + x2_ptr->x_countdown;
            if (new_countdown > UCHAR_MAX) new_countdown = UCHAR_MAX;
            x_ptr->x_countdown = (byte)new_countdown;

            x_ptr->x_power = ((power + power2) / (x_ptr->x_countdown));

            /* Calculate repeats */
            power =  (power * MAX(x_ptr->x_repeats,  1));
            power2 = (power * MAX(x_ptr->x_repeats,  1));

            /* Calc a new time to repeat */
            x_ptr->x_repeats = ((power + power2) / (x_ptr->x_countdown * x_ptr->x_power));

            if (x_ptr->x_power > EFFECT_POWER_MAX) x_ptr->x_power = EFFECT_POWER_MAX;

            break;

        }
        case EFFECT_LINGERING_CLOUD:
        {
            u32b power =  (MAX(x_ptr->x_power,  1) * MAX(x_ptr->x_countdown,  1));
            u32b power2 = (MAX(x2_ptr->x_power, 1) * MAX(x2_ptr->x_countdown, 1));

            /* Add the countdown, and extract the power */
            int new_countdown = x_ptr->x_countdown + x2_ptr->x_countdown;
            if (new_countdown > UCHAR_MAX) new_countdown = UCHAR_MAX;
            x_ptr->x_countdown = (byte)new_countdown;

            x_ptr->x_power = ((power + power2) / (x_ptr->x_countdown));

            if (x_ptr->x_power > EFFECT_POWER_MAX) x_ptr->x_power = EFFECT_POWER_MAX;

            break;
        }
        /* Oops! */
        default:
            pop_up_message_box(" Error: Unknown effect type.");

            break;
    }
}



/*
 * Place the effect in a stack of effects.
 */
static void place_effect_idx(int x_idx, int y, int x)
{
    /* Get this effect */
    effect_type *x_ptr = &x_list[x_idx];

    /*
     * Handle next_x_idx.  Traps (and glyphs) always have first priority.
     */
    if (cave_any_trap_bold(y, x) || (cave_hidden_object_bold(y, x)))
    {
        x_ptr->next_x_idx = x_list[dungeon_info[y][x].effect_idx].next_x_idx;
        x_list[dungeon_info[y][x].effect_idx].next_x_idx = x_idx;
    }
    else
    {
        x_ptr->next_x_idx = dungeon_info[y][x].effect_idx;
        dungeon_info[y][x].effect_idx = x_idx;
    }

    /* Update some CAVE_* flags */
    update_los_proj_move(y, x);

    /* Redraw grid if necessary */
    if (character_dungeon && !(x_ptr->x_flags & (EF1_HIDDEN)))
    {
        light_spot(y, x);
    }
}


/*
 * Should always be called only after x_pop();
 */
void effect_prep(int x_idx, byte type, u16b f_idx, byte y, byte x, byte countdown,
                            byte repeats, u16b power, s16b source, u16b flags)
{
    int this_x_idx, next_x_idx;

    /* Get this effect */
    effect_type *x_ptr = &x_list[x_idx];

    /* Wipe it */
    x_ptr->effect_wipe();

    /*Fill in the data*/
    x_ptr->x_type = type;
    x_ptr->x_f_idx = f_idx;
    x_ptr->x_cur_y = y;
    x_ptr->x_cur_x = x;
    x_ptr->x_countdown = countdown;
    x_ptr->x_repeats = repeats;
    x_ptr->x_power = power;
    x_ptr->x_source = source;
    x_ptr->x_flags = flags;

    /* Scan objects in that grid for combination */
    for (this_x_idx = dungeon_info[y][x].effect_idx; this_x_idx; this_x_idx = next_x_idx)
    {
        effect_type *x2_ptr;

        /* Get the object */
        x2_ptr = &x_list[this_x_idx];

        /* Get the next object */
        next_x_idx = x2_ptr->next_x_idx;

        /* Check for combination */
        if (effect_similar(x2_ptr, x_ptr))
        {
            /* Combine the items */
            effect_absorb(x2_ptr, x_ptr);

            /*We don't need it anymore*/
            x_ptr->effect_wipe();

            /* We didn't use the slot. */
            x_cnt--;

            /* We used the last slot */
            if ((x_idx + 1) == x_max) x_max--;

            /* Result */
            return;
        }

    }

    /*Put it in the proper order*/
    place_effect_idx(x_idx, y, x);
}


/*
 * Set a lingering cloud effect.  Return false if it could not be set.
 */
bool set_effect_lingering_cloud(int f_idx, byte y, byte x,	u16b power, s16b source, u16b flag)
{
    int x_idx = x_pop();
    int countdown;
    int effect_power;

    feature_type *f_ptr = &f_info[f_idx];

    /*All full*/
    if (!x_idx) return (FALSE);

    /* Determine how powerful this should be. */
    effect_power = (f_ptr->x_damage * power) / 100;

    /* Calculate countdown */
    countdown = (effect_power / 1000) + f_ptr->x_timeout_set + randint(f_ptr->x_timeout_rand);

    /* Keep it in bounds */
    if (countdown >= UCHAR_MAX) countdown = UCHAR_MAX - 1;

    /* Create the effect */
    effect_prep(x_idx, EFFECT_LINGERING_CLOUD, f_idx, y, x, (byte)countdown, 0, effect_power, source, flag);

    return (TRUE);
}


/*
 * Set a glacier effect.  Return false if it could not be set.
 */
bool set_effect_glacier(int f_idx, byte y, byte x, s16b source, u16b flag)
{
    int x_idx = x_pop();
    int countdown;

    /* Get the feature */
    feature_type *f_ptr = &f_info[f_idx];

    /* All full */
    if (!x_idx) return (FALSE);

    /* Calculate countdown */
    countdown = f_ptr->x_timeout_set + randint(f_ptr->x_timeout_rand);

    /* Keep it in bounds */
    if (countdown >= UCHAR_MAX) countdown = UCHAR_MAX - 1;

    /* Create the effect */
    effect_prep(x_idx, EFFECT_GLACIER, f_idx, y, x, (byte)countdown, 0, 0, source, flag);

    return (TRUE);
}


/*
 * Set an inscription effect. Return TRUE on success
 */
bool set_effect_inscription(int f_idx, byte y, byte x, s16b source, u16b flag)
{
    /* Get the first effect on that grid */
    int x_idx = dungeon_info[y][x].effect_idx;

    /* Scan the effects */
    while (x_idx)
    {
        /* Get the effect */
        effect_type *x_ptr = &x_list[x_idx];

        /* Ignore locations already inscribed */
        if (x_ptr->x_type == EFFECT_INSCRIPTION) return (FALSE);

        /* Get the next effect */
        x_idx = x_ptr->next_x_idx;
    }

    /* Get an empty entry */
    x_idx = x_pop();

    /* All full */
    if (!x_idx) return (FALSE);

    /* Dumb effect */
    flag |= (EF1_SKIP);

    /* Create the effect */
    effect_prep(x_idx, EFFECT_INSCRIPTION, f_idx, y, x, 0, 0, 0, source, flag);

    return (TRUE);
}


/*
 * Set a shimmering cloud effect.  Return false if it could not be set.
 */
bool set_effect_shimmering_cloud(int f_idx, byte y, byte x, byte repeats, u16b power, s16b source, u16b flag)
{
    feature_type *f_ptr = &f_info[f_idx];
    int x_idx = x_pop();
    int countdown;
    int effect_power;

    /*All full*/
    if (!x_idx) return (FALSE);

    /* Determine how powerful this should be. */
    effect_power = (f_ptr->x_damage * power) / 100;

    /* These are hidden, and displayed as bursts. */
    flag |= (EF1_HIDDEN);

    /* Get the countdown */
    countdown = f_ptr->x_timeout_set + randint(f_ptr->x_timeout_rand);

    /* Keep it in bounds */
    if (countdown >= UCHAR_MAX) countdown = UCHAR_MAX - 1;

    /* Create the effect */
    effect_prep(x_idx, EFFECT_SHIMMERING_CLOUD, f_idx, y, x, (byte)countdown, repeats, effect_power, source, flag);

    return (TRUE);
}


/*
 * Set a permanent cloud effect.  Return false if it could not be set.
 * Currently assumes neither players nor monsters can set permanent effects (use of SOURCE_EFFECT)
 */
bool set_effect_permanent_cloud(int f_idx, byte y, byte x,	u16b power, u16b flag)
{
    int x_idx = x_pop();

    /*All full*/
    if (!x_idx) return (FALSE);

    flag |= (EF1_PERMANENT  | EF1_HURT_PLAY);

    effect_prep(x_idx, EFFECT_PERMANENT_CLOUD, f_idx, y, x, 1, 1, power, SOURCE_EFFECT, flag);

    return (TRUE);
}


/*
 * Set a passive trap effect.  Return false if it could not be set.
 */
bool set_effect_trap_passive(int f_idx, byte y, byte x)
{
    int x_idx = x_pop();
    u16b flags = (EF1_TRAP_DUMB | EF1_PERMANENT | EF1_SKIP | EF1_HIDDEN | EF1_HURT_PLAY);

    /*All full*/
    if (!x_idx) return (FALSE);

    effect_prep(x_idx, EFFECT_TRAP_DUMB, f_idx, y, x, 1, 1, 0, SOURCE_TRAP, flags);

    return (TRUE);
}


/*
 * Set a smart trap effect.  Return false if it could not be set.
 */
bool set_effect_trap_smart(int f_idx, byte y, byte x, u16b flags)
{
    int x_idx = x_pop();
    int countdown;
    int trap_power;

    feature_type *f_ptr = &f_info[f_idx];

    /*All full*/
    if (!x_idx) return (FALSE);

    trap_power = (f_ptr->x_damage * (MAX(1, p_ptr->depth)) / 100);

    /*
     * Avoid unfair instadeaths for the player by creating the trap only partially charged.
     */
    countdown = (f_ptr->x_timeout_set + rand_int(f_ptr->x_timeout_rand)) /2;
    if (countdown <=3) countdown = 4;

    /*Keep it in bounds*/
    if (countdown >= UCHAR_MAX) countdown = UCHAR_MAX - 1;

    flags |= (EF1_TRAP_SMART | EF1_PERMANENT | EF1_HIDDEN | EF1_HURT_PLAY);

    effect_prep(x_idx, EFFECT_TRAP_SMART, f_idx, y, x, (byte)countdown, 1, trap_power, SOURCE_TRAP, flags);

    return (TRUE);
}


/*
 * Set a monster trap effect.  Return false if it could not be set.
 */
bool set_effect_trap_player(int f_idx, byte y, byte x)
{
    int x_idx = x_pop();
    u16b flags = (EF1_TRAP_PLAYER | EF1_PERMANENT | EF1_SKIP);

    /*All full*/
    if (!x_idx) return (FALSE);

    effect_prep(x_idx, EFFECT_TRAP_PLAYER, f_idx, y, x, 1, 1, 0, SOURCE_TRAP, flags);

    /* RE-do the flow */
    p_ptr->update |= (PU_FLOW_DOORS | PU_FLOW_NO_DOORS);

    return (TRUE);
}


/*
 * Set a glyph.  Return false if it could not be set.
 */
bool set_effect_glyph(byte y, byte x)
{
    int x_idx = x_pop();
    u16b flags = (EF1_GLYPH | EF1_PERMANENT | EF1_SKIP);

    /*All full*/
    if (!x_idx) return (FALSE);

    effect_prep(x_idx, EFFECT_GLYPH, FEAT_GLYPH_WARDING, y, x, 1, 1, 0, SOURCE_TRAP, flags);

    /* RE-do the flow */
    p_ptr->update |= (PU_FLOW_DOORS | PU_FLOW_NO_DOORS);

    return (TRUE);
}

/*
 * Set some rubble.  Return false if it could not be set.
 */
bool set_effect_rocks(int f_idx, byte y, byte x)
{
    int x_idx = x_pop();
    u16b flags = (EF1_PERMANENT | EF1_SKIP);

    //Hack - plain rubble doesn't have an object.
    if (f_idx == FEAT_RUBBLE_HIDDEN_OBJECT) flags |= EF1_OBJECT;

    // 25% of the time
    else if (f_idx == FEAT_LOOSE_ROCK)
    {
        if (one_in_(4)) flags |= EF1_OBJECT;
    }

    /*All full*/
    if (!x_idx) return (FALSE);

    effect_prep(x_idx, EFFECT_ROCKS, f_idx, y, x, 1, 1, 0, SOURCE_OTHER, flags);

    /* RE-do the flow */
    p_ptr->update |= (PU_FLOW_DOORS | PU_FLOW_NO_DOORS);

    return (TRUE);
}


/*
 * Move an effect from index i1 to index i2 in the effect list
 */
static void compact_effects_aux(int effect_old, int effect_new)
{
    int i;

    int y, x;

    effect_type *x_ptr = &x_list[effect_old];

    /* Do nothing */
    if (effect_old == effect_new) return;

    /* Repair effects */
    for (i = 1; i < x_max; i++)
    {
        /* Skip "dead" effects */
        if (!x_list[i].x_type) continue;

        /* Repair "next" pointers */
        if (x_list[i].next_x_idx == effect_old)
        {
            /* Repair */
            x_list[i].next_x_idx = effect_new;
        }
    }

    /* Get location */
    y = x_ptr->x_cur_y;
    x = x_ptr->x_cur_x;

    /* Hack -- move effect */
    COPY(&x_list[effect_new], &x_list[effect_old], effect_type);

    /* Hack -- wipe hole */
    x_ptr->effect_wipe();

    /* Repair grid */
    if (dungeon_info[y][x].effect_idx == effect_old)
    {
        /* Repair */
        dungeon_info[y][x].effect_idx = effect_new;
    }
}


/*
 * Simple routine to compact effects
 */
void compact_effects(void)
{
    int i;

    /* Excise dead effects (backwards!) */
    for (i = x_max - 1; i >= 1; i--)
    {
        effect_type *x_ptr = &x_list[i];

        /* Skip real effects */
        if (x_ptr->x_type) continue;

        /* Move last effect into open hole */
        compact_effects_aux(x_max - 1, i);

        /* Compress "x_max" */
        x_max--;
    }
}


/*
 * Make sure there is no smart trap projectable to a square within 6 squares.
 */
static bool found_smart_trap(int orig_y, int orig_x)
{
    int begin_y, end_y, begin_x, end_x, y, x;

    /* For efficiency, set the boundaries first, make sure they are in bounds. */
    begin_y = orig_y - 10;
    if (begin_y < 0) begin_y = 0;
    begin_x = orig_x - 10;
    if (begin_x < 0) begin_x = 0;
    end_y = orig_y + 10;
    if (end_y >= p_ptr->cur_map_hgt) end_y = p_ptr->cur_map_hgt - 1;
    end_x = orig_x + 10;
    if (end_x >= p_ptr->cur_map_wid) end_x = p_ptr->cur_map_wid - 1;

    /*Search each grid for a smart trap that is projectable.  */
    for (y = begin_y; y <= end_y; y++)
    {
        for (x = begin_x; x <= end_x; x++)
        {
            /* Not a smart trap there. */
            if (!cave_smart_trap_bold(y, x)) continue;

            /* If in line of sight, return true. */
            if (projectable(orig_y, orig_x, y, x, PROJECT_CHCK)) return (TRUE);
        }
    }

    /* No projectable smart trap found*/
    return (FALSE);
}


void pick_and_set_trap(int y, int x, int mode)
{
    u16b feat;

    /* Paranoia */
    if (cave_any_trap_bold(y, x)) return;

    if (cave_hidden_object_bold(y, x)) return;

    /* No NPP terrains option turned on */
    if (birth_classic_dungeons) mode = EFFECT_TRAP_DUMB;

    /*Make sure there aren't too many smart traps in the same place*/
    else if (mode != EFFECT_TRAP_DUMB)
    {
        /*There is a nearby trap in line of sight*/
        if (found_smart_trap(y, x)) mode = EFFECT_TRAP_DUMB;
    }

    /* Hack - find a trap to determine trap type. */
    feat = pick_trap(mode);

    /* Set either set a smart or dumb trap*/
    if (feat_ff2_match(feat, FF2_TRAP_SMART))
    {
       QString dummy_string;
       u16b flags = fire_trap_smart(feat, y, x, MODE_FLAGS, &dummy_string);

       set_effect_trap_smart(feat, y, x, flags);
    }
    else if (feat_ff2_match(feat, FF2_TRAP_PASSIVE))
    {
        set_effect_trap_passive(feat, y, x);
    }
}


/*
 * Places a random trap at the given location.
 *
 * The location must be a legal, naked, floor grid.
 *
 */
void place_trap(int y, int x, byte mode)
{
    /* Paranoia */
    if (!in_bounds(y, x)) return;

    /* Require suitable grid. Ignore monsters */
    if (!cave_trappable_bold(y, x)) return;

    /* Place a trap */
    pick_and_set_trap(y, x, mode);
}




/*
 * Delete all the effects when player leaves the level
 *
 * Note -- we do NOT visually reflect these (irrelevant) changes
 *
 * Hack -- we clear the "cave_x_idx[y][x]" field for every grid,
 * and the "x_ptr->next_x_idx" field for every effect, since
 * we know we are clearing every effect.  Technically, we only
 * clear those fields for grids/ containing effects and traps,
 * and we clear it once for every such effect.
 */
void wipe_x_list(void)
{
    int i;

    /* Delete the existing effects */
    for (i = 1; i < x_max; i++)
    {
        effect_type *x_ptr = &x_list[i];

        /* Skip dead effects */
        if (!x_ptr->x_type) continue;

        /* Hack -- see above */
        dungeon_info[x_ptr->x_cur_y][x_ptr->x_cur_x].effect_idx = 0;

        /* Wipe it */
        x_ptr->effect_wipe();
    }

    /* Reset "x_max" */
    x_max = 1;

    /* Reset "x_cnt" */
    x_cnt = 0;
}


/*
 * Delete a dungeon effect
 *
 * Handle "stacks" of effects correctly.
 */
void delete_effect_idx(int x_idx)
{
    effect_type *x_ptr = &x_list[x_idx];

    int y = x_ptr->x_cur_y;
    int x = x_ptr->x_cur_x;

    /* Glyphs and traps affect monster movement.  The monsters need to re-think things. */
    if ((x_ptr->x_type == EFFECT_GLYPH) || (x_ptr->x_type == EFFECT_TRAP_PLAYER))
    {
        /* RE-do the flow */
        p_ptr->update |= (PU_FLOW_DOORS | PU_FLOW_NO_DOORS);
    }

    /* Excise & handle stacks */
    excise_effect_idx(x_idx);

    /* Wipe the effects */
    x_ptr->effect_wipe();

    /* Update some CAVE_* flags */
    update_los_proj_move(y, x);

    /* Visual update */
    light_spot(y, x);

    /* Count effects */
    x_cnt--;
}




/*
 * Deletes all effects at given location
 */
void delete_effects(int y, int x)
{
    s16b this_x_idx, next_x_idx = 0;

    /* Paranoia */
    if (!in_bounds(y, x)) return;

    /* Scan all effects in the grid */
    for (this_x_idx = dungeon_info[y][x].effect_idx; this_x_idx; this_x_idx = next_x_idx)
    {
        effect_type *x_ptr;

        /* Get the effects */
        x_ptr = &x_list[this_x_idx];

        /* Get the next effects */
        next_x_idx = x_ptr->next_x_idx;

        /* Wipe the effect */
        x_ptr->effect_wipe();

        /* Count effects */
        x_cnt--;
    }

    /* Effects are gone */
    dungeon_info[y][x].effect_idx = 0;

    /* Update some CAVE_* flags */
    update_los_proj_move(y, x);

    /* Visual update */
    light_spot(y, x);
}


/*
 * Excise an effect from a stack of effectss
 */
void excise_effect_idx(int x_idx)
{
    effect_type *x_ptr;

    s16b this_x_idx, next_x_idx = 0;

    s16b prev_x_idx = 0;

    int y, x;

    /* Object */
    x_ptr = &x_list[x_idx];

    y = x_ptr->x_cur_y;
    x = x_ptr->x_cur_x;

    /* Scan all effects in the grid */
    for (this_x_idx = dungeon_info[y][x].effect_idx; this_x_idx; this_x_idx = next_x_idx)
    {
        effect_type *x2_ptr;

        /* Get the effect */
        x2_ptr = &x_list[this_x_idx];

        /* Get the next effect */
        next_x_idx = x2_ptr->next_x_idx;

        /* Done */
        if (this_x_idx == x_idx)
        {
            /* No previous */
            if (prev_x_idx == 0)
            {
                /* Remove from list */
                dungeon_info[y][x].effect_idx = next_x_idx;
            }

            /* Real previous */
            else
            {
                effect_type *x3_ptr;

                /* Previous effects */
                x3_ptr = &x_list[prev_x_idx];

                /* Remove from list */
                x3_ptr->next_x_idx = next_x_idx;
            }

            /* Forget next pointer */
            x_ptr->next_x_idx = 0;

            /* Done */
            break;
        }

        /* Save prev_o_idx */
        prev_x_idx = this_x_idx;
    }
}


/*
 * Get and return the index of a "free" effect.  Assumes
 * an effect will be created.
 *
 * This routine should almost never fail, but in case it does,
 * we must be sure to handle "failure" of this routine.
 */
s16b x_pop(void)
{
    int i;

    /* First attempt to recycle dead effects */
    for (i = 1; i < x_max; i++)
    {
        effect_type *x_ptr;

        /* Get the effect */
        x_ptr = &x_list[i];

        /* Skip live effect */
        if (x_ptr->x_type) continue;

        /* Count effects */
        x_cnt++;

        /* Use this effects */
        return (i);
    }

    /*
     * We didn't find an empty slot
     * Initial allocation
     */
    if (x_max < z_info->x_max)
    {
        /* Get next space */
        i = x_max;

        /* Expand effect array */
        x_max++;

        /* Count effects */
        x_cnt++;

        /* Use this effect */
        return (i);
    }

    /* Oops */
    return (0);
}



/*
 * A quick burst at the effect square.  Hurt everything, except possibly the player.
 * We would use explosion, but that turns inso a project_ball where it can
 * actually create effects.
 */
static bool effect_burst(const effect_type *x_ptr)
{
    feature_type *f_ptr = &f_info[x_ptr->x_f_idx];

    int y = x_ptr->x_cur_y;
    int x = x_ptr->x_cur_x;

    /* Add the  bitflags */
    u32b flg = (PROJECT_JUMP | PROJECT_STOP | PROJECT_KILL | PROJECT_ITEM | PROJECT_GRID);

    /* We have now seen this feature */
    if (player_can_see_bold(y, x)) f_ptr->f_everseen = TRUE;

    /* Hurt the character unless he controls the spell */
    if (x_ptr->x_flags & (EF1_HURT_PLAY)) flg |= (PROJECT_PLAY);

    /* Don't bother with graphics if the player is resting, running, etc. */
    if (CHECK_DISTURB(FALSE)) flg |= (PROJECT_HIDE);

    /* Leave graphics for later in empty grids */
    else if (player_has_los_bold(y, x) && panel_contains(y, x))
    {
        /* Don't display anything now */
        flg |= (PROJECT_HIDE);

        // It is important both of these vectors are added together.
        effect_bursts.append(make_coords(y, x));
        effect_burst_gf.append(f_ptr->x_gf_type);
    }

    /* Make a effect 1 square */
    return (project(x_ptr->x_source, 1, y, x, y, x, x_ptr->x_power, f_ptr->x_gf_type, flg, 0, 0));
}


/* Hit the monster or player at one spot*/
static void effect_damage(const effect_type *x_ptr)
{
    feature_type *f_ptr = &f_info[x_ptr->x_f_idx];

    /*Quick access to effect coordinates*/
    int y = x_ptr->x_cur_y;
    int x = x_ptr->x_cur_x;

    /* Get the source of the effect */
    s16b source = x_ptr->x_source;

    /* The source is a monster race */
    if (source > 0)
    {
        /* Hack - The project functions cannot receive a race index */
        source = SOURCE_EFFECT;
    }

    /*We have something here to damage*/
    if ((x_ptr->x_power > 0) && (dungeon_info[y][x].has_monster()))
    {
        /*It is a creature*/
        if (dungeon_info[y][x].monster_idx > 0)
        {
            monster_type *m_ptr = &mon_list[dungeon_info[y][x].monster_idx];

            /* Hack - Don't hurt the same race*/
            if (m_ptr->r_idx != x_ptr->x_source)
            {
                monster_race *r_ptr = &r_info[m_ptr->r_idx];

                /*Check the HURT_EVIL flag first, then hurt the monster*/
                if (!(x_ptr->x_flags & (EF1_HURT_EVIL)) || (r_ptr->flags3 & (RF3_EVIL)))
                {

                    (void)project_m(source, y, x, x_ptr->x_power, f_ptr->x_gf_type, 0L);
                }
            }
        }
        /* Then it must be the player, unless he is not supposed to be hurt by this. */
        else if ((x_ptr->x_flags & (EF1_HURT_PLAY))  && !is_player_immune(f_ptr->x_gf_type))
        {
            QString name;

            QString kb_str;

            /* Get the feature name */
            name = feature_desc(x_ptr->x_f_idx, TRUE, TRUE);

            switch (x_ptr->x_f_idx)
            {
                /*Just a couple exceptions to the default, breathing*/
                case FEAT_EFFECT_CLOUD_STATIC:
                {
                    kb_str = (QString("getting shocked by %1") .arg(name));
                    break;
                }

                case FEAT_EFFECT_CLOUD_NETHER:
                case FEAT_EFFECT_CLOUD_CHAOS:
                case FEAT_EFFECT_CLOUD_DISENCHANTMENT:
                case FEAT_EFFECT_CLOUD_NEXUS:
                case FEAT_EFFECT_CLOUD_TIME:
                case FEAT_EFFECT_CLOUD_CONFUSION:
                {
                    kb_str = (QString("standing in a %1") .arg(name));
                    break;
                }

                case FEAT_EFFECT_CLOUD_STEAM:
                {
                    kb_str = (QString("getting scalded by %1") .arg(name));
                    break;
                }

                /*Most cases*/
                default: kb_str = (QString("breathing in %1") .arg(name));
                break;
            }

            /* Show a message */
            if (!p_ptr->timed[TMD_BLIND])
            {
                message(QString("You are %1!") .arg(kb_str));
            }

            else message(QString("The air is not safe here!"));

            /*Hit the player*/
            (void) project_p(source, y, x, x_ptr->x_power, f_ptr->x_gf_type, kb_str);
        }
    }
}


/*
 * Process an individual effect.
 * We assume this is an active effect.
 */
static void process_effect(int x_idx)
{
    /* Get this effect */
    effect_type *x_ptr = &x_list[x_idx];

    /*Quick access to effect coordinates*/
    int y = x_ptr->x_cur_y;
    int x = x_ptr->x_cur_x;

    /*reduce the timeout, if there is one*/
    if ((x_ptr->x_countdown > 0) && (x_ptr->x_type != EFFECT_PERMANENT_CLOUD)) x_ptr->x_countdown--;

    /* Handle active effects */
    if (x_ptr->x_countdown > 0)
    {
        switch (x_ptr->x_type)
        {

            /*Hurt the player or monster on site*/
            case EFFECT_LINGERING_CLOUD:
            case EFFECT_PERMANENT_CLOUD:
            {
                /*Hurt the creature at this spot*/
                effect_damage(x_ptr);

                /* Animate the cloud */
                if (player_can_see_bold(y, x) && !(x_ptr->x_flags & (EF1_HIDDEN)) &&
                    (!dungeon_info[y][x].has_monster()))
                {
                    /* Redraw */
                    light_spot(y, x);
                }

                break;
            }

            /*
             * Mega - Hack - Special handling for smart traps.
             * This mainly makes it safe to teleport for game balance purposes.
             */
            case EFFECT_TRAP_SMART:
            {
                if (x_ptr->x_countdown > 1)	return;

                /* Path is not in line of sight of the player. */
                if (!player_can_fire_bold(y, x))
                {
                    if (distance(y, x, p_ptr->py, p_ptr->px) > MAX_SIGHT)
                    {
                        /*Full re-set*/
                        x_ptr->x_countdown = f_info[x_ptr->x_f_idx].x_timeout_set +
                                 randint(f_info[x_ptr->x_f_idx].x_timeout_rand);
                    }

                    else x_ptr->x_countdown += randint(3);
                }

                /* Animate the trap*/
                if (player_can_see_bold(y, x) && !(x_ptr->x_flags & (EF1_HIDDEN)) &&
                    (!dungeon_info[y][x].has_monster()))
                {
                    /* Redraw */
                    light_spot(y, x);
                }

                break;
            }

            case EFFECT_GLACIER:
            {
                /* Animate the glacier */
                if (player_can_see_bold(y, x) && !(x_ptr->x_flags & (EF1_HIDDEN)) &&
                   (!dungeon_info[y][x].has_monster()))
                {
                    /* Redraw */
                    light_spot(y, x);
                }
                break;
            }

            default: break;
        }
    }

    /*We are ready for something to happen, or stop happening */
    else /*(!x_ptr->x_countdown)*/
    {
        switch (x_ptr->x_type)
        {
            /*Quick radius 1 burst at that site*/
            case EFFECT_SHIMMERING_CLOUD:
            {
                /*
                 *  Hack - Don't do anything if the player is the owner of the effect and
                 *  is out of line of fire (teleportation abuse)
                 */
                if ((!player_can_fire_bold(y, x)) && (x_ptr->x_source == SOURCE_PLAYER)) break;

                effect_burst(x_ptr);

                break;
            }

            /*
             * Note we need to return rather than just break here
             * to avoid the trap charging before it fires.
             */
            case EFFECT_TRAP_SMART:
            {
                int path;
                bool fire = FALSE;

                /* Path is not in line of sight of the player. */
                if (!player_can_fire_bold(y, x))
                {
                    if (distance(y, x, p_ptr->py, p_ptr->px) > MAX_SIGHT)
                    {
                        /*Full re-set*/
                        x_ptr->x_countdown = f_info[x_ptr->x_f_idx].x_timeout_set +
                             randint(f_info[x_ptr->x_f_idx].x_timeout_rand);
                    }

                    else x_ptr->x_countdown += randint(3);

                    return;
                }

                if (x_ptr->x_flags & (EF1_SM_TRAP_LOS))
                    fire = TRUE;
                else
                {
                    /* Check what kinds of spells can hit player */
                    path = projectable(y, x, p_ptr->py, p_ptr->px, PROJECT_CHCK);

                    /*Not a clear shot*/
                    if (path == PROJECT_NO) return;

                    /*Ball, orb, or beam spells only need to be in line of fire*/
                    if ((x_ptr->x_flags & (EF1_SM_TRAP_BALL)) ||
                        (x_ptr->x_flags & (EF1_SM_TRAP_BEAM)) ||
                        (x_ptr->x_flags & (EF1_SM_TRAP_ORB)))
                    {
                        if (path >= PROJECT_NOT_CLEAR)   fire = TRUE;
                    }
                    else /* handle EF1_SM_TRAP_BOLT - skip if statement for effeciency */
                    {
                        if (path == PROJECT_CLEAR) fire = TRUE;
                    }
                }

                /* Fire! */
                if (fire)
                {
                    QString dummy_string;
                    (void)fire_trap_smart(x_ptr->x_f_idx, x_ptr->x_cur_y, x_ptr->x_cur_x, MODE_ACTION, &dummy_string);

                    /*Disturb the player*/
                    disturb (TRUE, TRUE);

                    /* Hack - Special handling for smart traps*/
                    x_ptr->x_countdown = f_info[x_ptr->x_f_idx].x_timeout_set +
                                 randint(f_info[x_ptr->x_f_idx].x_timeout_rand);
                }

                /* Hack - Special exit for smart traps*/
                return;
            }

            default: break;
        }

        /*Re-set the effect*/
        if (x_ptr->x_repeats > 0)
        {
            /*reset the countdown*/
            x_ptr->x_countdown = f_info[x_ptr->x_f_idx].x_timeout_set +
                                 randint(f_info[x_ptr->x_f_idx].x_timeout_rand);

            /*Reduce the count, unless we shouldn't*/
            if (!(x_ptr->x_flags & (EF1_PERMANENT))) x_ptr->x_repeats--;
        }
    }

    /* Effects eventually "die" */
    if ((!x_ptr->x_repeats) && (!x_ptr->x_countdown))
    {
        /* Effect is "dead" */
        delete_effect_idx(x_idx);
    }
}


/*
 * Draw the delayed burst effects, if any
 */
static void show_burst_effects(void)
{
    /* We need something to show */
    if (effect_bursts.size() && player_can_observe())
    {
        /* Show the burst effects */
        for (int i = 0; i < effect_bursts.size(); i++)
        {
            //Paranoia
            if (i >= effect_burst_gf.size()) break;

            /* Get coordinates */
            int y = effect_bursts.at(i).y;
            int x = effect_bursts.at(i).x;

            ui_animate_ball(y, x, 1, effect_burst_gf.at(i), 0L);
        }
    }

    /* Reset the vectors */
    effect_bursts.clear();
    effect_burst_gf.clear();
}


/*
 * Process effects.
 */
void process_effects(void)
{
    int i;

    /* Paranoia */
    notice_stuff();

    /* Process all effects */
    for (i = 0; i < x_max; i++)
    {

        /*Some effects don't get processed*/
        if (x_list[i].x_flags & (EF1_SKIP)) continue;

        /* There is an effect here */
        if (x_list[i].x_type) process_effect(i);

        /*
         * If the effect still exists and there is another effect
         * on that grid, flush the stacked monster messages
         */
        if (x_list[i].x_type && x_list[i].next_x_idx) flush_monster_messages();

        /*continue*/;
    }

    /* Show the delayed burst effects, if any */
    show_burst_effects();

    /* Paranoia */
    notice_stuff();

    /*We can't let this array get too bloated.  There will be high turnover. */
    if ((x_cnt + 30) > x_max) compact_effects();
    else if ((x_max + 20) > z_info->x_max) compact_effects();
}

