/*
 * File: project-feat.c
 * Purpose: Projection effects on terrain
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * Copyright (c) 2016 MAngband and PWMAngband Developers
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 2, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */


#include "s-angband.h"


/*
 * Feature handlers
 */


typedef struct project_feature_handler_context_s
{
    struct actor *who;
    int r;
    struct chunk *cave;
    int y;
    int x;
    int dam;
    int type;
    bool line_sight;
    bool line_sound;
    bool is_blind;
    bool obvious;
} project_feature_handler_context_t;


typedef void (*project_feature_handler_f)(project_feature_handler_context_t *);


static void project_feature_handler_ACID(project_feature_handler_context_t *context)
{
    /* Grid is in line of sight and player is not blind */
    if (context->line_sight && !context->is_blind) context->obvious = true;
}


static void project_feature_handler_ELEC(project_feature_handler_context_t *context)
{
    /* Grid is in line of sight and player is not blind */
    if (context->line_sight && !context->is_blind) context->obvious = true;
}


static void project_feature_handler_FIRE(project_feature_handler_context_t *context)
{
    /* Grid is in line of sight and player is not blind */
    if (context->line_sight && !context->is_blind) context->obvious = true;

    /* No effect on special levels */
    if (special_level(context->cave->depth)) return;

    /* Burn trees */
    if (square_isstrongtree(context->cave, context->y, context->x))
    {
        /* Message */
        if (context->line_sound)
            msg(context->who->player, "The tree burns!");

        /* Burn the tree */
        square_burn_tree(context->cave, context->y, context->x);
    }

    /* Destroy trees */
    else if (square_iswitheredtree(context->cave, context->y, context->x))
    {
        /* Message */
        if (context->line_sound)
            msg(context->who->player, "The tree burns to the ground!");

        /* Destroy the tree */
        if (!context->cave->depth) trees_in_town--;
        square_destroy_tree(context->cave, context->y, context->x);

        /* Reapply illumination */
        square_illuminate(context->who->player, context->cave, context->y, context->x, is_daytime());
        square_light_spot(context->cave, context->y, context->x);

        /* Update the visuals */
        update_visuals(context->cave->depth);

        /* Fully update the flow */
        fully_update_flow(context->cave->depth);
    }

    /* Burn grass */
    if (square_isgrass(context->cave, context->y, context->x))
    {
        /* Destroy the grass */
        square_burn_grass(context->cave, context->y, context->x);
    }
}


static void project_feature_handler_COLD(project_feature_handler_context_t *context)
{
    /* Grid is in line of sight and player is not blind */
    if (context->line_sight && !context->is_blind) context->obvious = true;
}


static void project_feature_handler_POIS(project_feature_handler_context_t *context)
{
    /* Grid is in line of sight and player is not blind */
    if (context->line_sight && !context->is_blind) context->obvious = true;
}


/* Light up the grid */
static void project_feature_handler_LIGHT(project_feature_handler_context_t *context)
{
    const int x = context->x;
    const int y = context->y;

    /* Turn on the light */
    sqinfo_on(context->cave->squares[y][x].info, SQUARE_GLOW);

    /* Grid is in line of sight and player is not blind */
    if (context->line_sight && !context->is_blind) context->obvious = true;

    /* Note changes to viewable region */
    note_viewable_changes(context->cave->depth, y, x, true);
}


/* Darken the grid */
static void project_feature_handler_DARK(project_feature_handler_context_t *context)
{
    const int x = context->x;
    const int y = context->y;
    int i;

    /* No effect outside of the dungeon during day */
    if ((context->cave->depth <= 0) && is_daytime()) return;

    /* No effect on special levels */
    if (special_level(context->cave->depth)) return;

    /* Turn off the light */
    square_unglow(context->cave, y, x);

    /* Check everyone */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *p = player_get(i);

        /* If he's not here, skip him */
        if (p->depth != context->cave->depth) continue;

        /* Hack -- forget "boring" grids */
        if (square_isview(p, y, x) && !square_isnormal(context->cave, y, x))
            square_forget(p, y, x);
    }

    /* Grid is in line of sight and player is not blind */
    if (context->line_sight && !context->is_blind) context->obvious = true;

    /* Note changes to viewable region */
    note_viewable_changes(context->cave->depth, y, x, true);
}


static void project_feature_handler_SOUND(project_feature_handler_context_t *context)
{
    /* Grid is in line of sight and player is not blind */
    if (context->line_sight && !context->is_blind) context->obvious = true;
}


static void project_feature_handler_SHARD(project_feature_handler_context_t *context)
{
    /* Grid is in line of sight and player is not blind */
    if (context->line_sight && !context->is_blind) context->obvious = true;
}


static void project_feature_handler_NEXUS(project_feature_handler_context_t *context)
{
    /* Grid is in line of sight and player is not blind */
    if (context->line_sight && !context->is_blind) context->obvious = true;
}


static void project_feature_handler_NETHER(project_feature_handler_context_t *context)
{
    /* Grid is in line of sight and player is not blind */
    if (context->line_sight && !context->is_blind) context->obvious = true;
}


static void project_feature_handler_CHAOS(project_feature_handler_context_t *context)
{
    /* Grid is in line of sight and player is not blind */
    if (context->line_sight && !context->is_blind) context->obvious = true;
}


static void project_feature_handler_DISEN(project_feature_handler_context_t *context)
{
    /* Grid is in line of sight and player is not blind */
    if (context->line_sight && !context->is_blind) context->obvious = true;
}


static void project_feature_handler_WATER(project_feature_handler_context_t *context)
{
    /* Grid is in line of sight and player is not blind */
    if (context->line_sight && !context->is_blind) context->obvious = true;
}


static void project_feature_handler_ICE(project_feature_handler_context_t *context)
{
    /* Grid is in line of sight and player is not blind */
    if (context->line_sight && !context->is_blind) context->obvious = true;
}


static void project_feature_handler_GRAVITY(project_feature_handler_context_t *context)
{
    /* Grid is in line of sight and player is not blind */
    if (context->line_sight && !context->is_blind) context->obvious = true;
}


static void project_feature_handler_INERT(project_feature_handler_context_t *context)
{
    /* Grid is in line of sight and player is not blind */
    if (context->line_sight && !context->is_blind) context->obvious = true;
}


static void project_feature_handler_FORCE(project_feature_handler_context_t *context)
{
    /* Grid is in line of sight and player is not blind */
    if (context->line_sight && !context->is_blind) context->obvious = true;
}


static void project_feature_handler_TIME(project_feature_handler_context_t *context)
{
    /* Grid is in line of sight and player is not blind */
    if (context->line_sight && !context->is_blind) context->obvious = true;
}


static void project_feature_handler_PLASMA(project_feature_handler_context_t *context)
{
    /* Grid is in line of sight and player is not blind */
    if (context->line_sight && !context->is_blind) context->obvious = true;
}


static void project_feature_handler_METEOR(project_feature_handler_context_t *context)
{
    /* Grid is in line of sight and player is not blind */
    if (context->line_sight && !context->is_blind) context->obvious = true;
}


static void project_feature_handler_MISSILE(project_feature_handler_context_t *context)
{
    /* Grid is in line of sight and player is not blind */
    if (context->line_sight && !context->is_blind) context->obvious = true;
}


static void project_feature_handler_MANA(project_feature_handler_context_t *context)
{
    /* Grid is in line of sight and player is not blind */
    if (context->line_sight && !context->is_blind) context->obvious = true;
}


static void project_feature_handler_HOLY_ORB(project_feature_handler_context_t *context)
{
    /* Grid is in line of sight and player is not blind */
    if (context->line_sight && !context->is_blind) context->obvious = true;
}


static void project_feature_handler_ARROW_X(project_feature_handler_context_t *context)
{
    /* Grid is in line of sight and player is not blind */
    if (context->line_sight && !context->is_blind) context->obvious = true;
}


static void project_feature_handler_ARROW_1(project_feature_handler_context_t *context)
{
    /* Grid is in line of sight and player is not blind */
    if (context->line_sight && !context->is_blind) context->obvious = true;
}


static void project_feature_handler_ARROW_2(project_feature_handler_context_t *context)
{
    /* Grid is in line of sight and player is not blind */
    if (context->line_sight && !context->is_blind) context->obvious = true;
}


static void project_feature_handler_ARROW_3(project_feature_handler_context_t *context)
{
    /* Grid is in line of sight and player is not blind */
    if (context->line_sight && !context->is_blind) context->obvious = true;
}


static void project_feature_handler_ARROW_4(project_feature_handler_context_t *context)
{
    /* Grid is in line of sight and player is not blind */
    if (context->line_sight && !context->is_blind) context->obvious = true;
}


static void project_feature_handler_BOULDER(project_feature_handler_context_t *context)
{
    /* Grid is in line of sight and player is not blind */
    if (context->line_sight && !context->is_blind) context->obvious = true;
}


static void project_feature_handler_LIGHT_WEAK(project_feature_handler_context_t *context)
{
    project_feature_handler_LIGHT(context);
}


static void project_feature_handler_DARK_WEAK(project_feature_handler_context_t *context)
{
    project_feature_handler_DARK(context);
}


/* Destroy walls (and doors) */
static void project_feature_handler_KILL_WALL(project_feature_handler_context_t *context)
{
    const int x = context->x;
    const int y = context->y;

    /* No effect on special levels */
    if (special_level(context->cave->depth)) return;

    /* Non-walls (etc) */
    if (square_ispassable(context->cave, y, x)) return;

    /* Permanent walls */
    if (square_isperm(context->cave, y, x) || square_isborder(context->cave, y, x))
        return;

    /* Different treatment for different walls */
    if (square_isrock(context->cave, y, x))
    {
        /* Message */
        if (context->line_sound)
        {
            msg(context->who->player, "The wall turns into mud!");
            context->obvious = true;
        }

        /* Destroy the wall */
        square_destroy_wall(context->cave, y, x);

        /* Update the visuals */
        update_visuals(context->cave->depth);

        /* Fully update the flow */
        fully_update_flow(context->cave->depth);
    }
    else if (square_hasgoldvein(context->cave, y, x))
    {
        /* Message */
        if (context->line_sound)
        {
            msg(context->who->player, "The vein turns into mud!");
            msg(context->who->player, "You have found something!");
            context->obvious = true;
        }

        /* Destroy the wall */
        square_destroy_wall(context->cave, y, x);

        /* Update the visuals */
        update_visuals(context->cave->depth);

        /* Fully update the flow */
        fully_update_flow(context->cave->depth);

        /* Place some gold */
        place_gold(context->who->player, context->cave, y, x, object_level(context->cave->depth),
            ORIGIN_FLOOR);
    }
    else if (square_ismagma(context->cave, y, x) || square_isquartz(context->cave, y, x))
    {
        /* Message */
        if (context->line_sound)
        {
            msg(context->who->player, "The vein turns into mud!");
            context->obvious = true;
        }

        /* Destroy the wall */
        square_destroy_wall(context->cave, y, x);

        /* Update the visuals */
        update_visuals(context->cave->depth);

        /* Fully update the flow */
        fully_update_flow(context->cave->depth);
    }
    else if (square_isrubble(context->cave, y, x))
    {
        /* Message */
        if (context->line_sound)
        {
            msg(context->who->player, "The rubble turns into mud!");
            context->obvious = true;
        }

        /* Destroy the rubble */
        square_destroy_rubble(context->cave, y, x);

        /* Update the visuals */
        update_visuals(context->cave->depth);

        /* Fully update the flow */
        fully_update_flow(context->cave->depth);

        /* Hack -- place an object */
        if (magik(10))
        {
            /* Found something */
            if (context->line_sound)
                msg(context->who->player, "There was something buried in the rubble!");

            /* Place object */
            place_object(context->who->player, context->cave, y, x,
                object_level(context->cave->depth), false, false, ORIGIN_RUBBLE, 0);
        }
    }
    else if (square_home_iscloseddoor(context->cave, y, x))
    {
        /* Message */
        if (context->line_sound)
        {
            msg(context->who->player, "The door resists.");
            context->obvious = true;
        }
    }
    else if (square_isdoor(context->cave, y, x))
    {
        struct monster *mon = square_monster(context->cave, y, x);

        /* Reveal mimics */
        if (mon && is_mimicking(mon))
            become_aware(context->who->player, context->cave, mon);
        else
        {
            /* Hack -- special message */
            if (context->line_sound)
            {
                msg(context->who->player, "The door crumbles to dust!");
                context->obvious = true;
            }

            /* Destroy the feature */
            square_destroy_door(context->cave, y, x);

            /* Update the visuals */
            update_visuals(context->cave->depth);
        }
    }
}


/* Destroy Doors (and traps) */
static void project_feature_handler_KILL_DOOR(project_feature_handler_context_t *context)
{
    const int x = context->x;
    const int y = context->y;

    /* Reveal mimics */
    if (square_isdoor(context->cave, y, x))
    {
        struct monster *mon = square_monster(context->cave, y, x);

        if (mon && is_mimicking(mon))
        {
            become_aware(context->who->player, context->cave, mon);
            return;
        }
    }

    /* Destroy all doors and traps */
    if (square_isplayertrap(context->cave, y, x) || square_isdoor(context->cave, y, x))
    {
        /* Message */
        if (context->line_sound)
        {
            msg(context->who->player, "There is a bright flash of light!");
            context->obvious = true;
        }

        /* Destroy the feature */
        if (square_isdoor(context->cave, y, x))
            square_destroy_door(context->cave, y, x);
        else
            square_destroy_trap(context->who->player, context->cave, y, x);

        /* Visibility change */
        if (square_issecretdoor(context->cave, y, x) ||
            square_basic_iscloseddoor(context->cave, y, x))
        {
            update_visuals(context->cave->depth);
            /*note_viewable_changes(context->cave->depth, y, x, false);*/
        }
    }
}


/* Destroy Traps (and Locks) */
static void project_feature_handler_KILL_TRAP(project_feature_handler_context_t *context)
{
    const int x = context->x;
    const int y = context->y;

    /* Reveal secret doors */
    if (square_issecretdoor(context->cave, y, x))
    {
        place_closed_door(context->cave, y, x);

        /* Observe */
        if (context->line_sound) context->obvious = true;
    }

    /* Destroy traps, unlock doors */
    if (square_isplayertrap(context->cave, y, x))
    {
        /* Message */
        if (context->line_sound)
        {
            msg(context->who->player, "There is a bright flash of light!");
            context->obvious = true;
        }

        /* Destroy the trap */
        square_destroy_trap(context->who->player, context->cave, y, x);
    }
    else if (square_islockeddoor(context->cave, y, x))
    {
        /* Unlock the door */
        square_unlock_door(context->cave, y, x);

        /* Message */
        if (context->line_sound)
        {
            msg(context->who->player, "Click!");
            context->obvious = true;
        }
    }
}


/* Make doors */
static void project_feature_handler_MAKE_DOOR(project_feature_handler_context_t *context)
{
    const int x = context->x;
    const int y = context->y;

    /* Require a grid without monsters and players */
    if (square_monster(context->cave, y, x) || square_isplayer(context->cave, y, x)) return;

    /* Require a floor grid */
    if (!square_isanyfloor(context->cave, y, x)) return;

    /* Push objects off the grid */
    if (square_object(context->cave, y, x)) push_object(context->who->player, context->cave, y, x);

    /* Create a closed door */
    square_close_door(context->cave, y, x);

    /* Observe */
    if (context->line_sound) context->obvious = true;

    /* Update the visuals */
    update_visuals(context->cave->depth);
}


/* Make traps */
static void project_feature_handler_MAKE_TRAP(project_feature_handler_context_t *context)
{
    const int x = context->x;
    const int y = context->y;

    /* Require an "empty" floor grid */
    if (!square_isopen(context->cave, y, x)) return;

    /* Create a trap */
    square_add_trap(context->cave, y, x);
    context->obvious = true;
}


/* Place a wall */
static void project_feature_handler_STONE_WALL(project_feature_handler_context_t *context)
{
    const int x = context->x;
    const int y = context->y;

    /* Require a "naked" floor grid */
    if (!square_isempty(context->cave, y, x)) return;

    /* Place a wall */
    square_add_wall(context->cave, y, x);

    /* Observe */
    if (context->line_sound) context->obvious = true;

    /* Update the visuals */
    update_visuals(context->cave->depth);

    /* Fully update the flow */
    fully_update_flow(context->cave->depth);
}


static void project_feature_handler_RAISE(project_feature_handler_context_t *context) {}
static void project_feature_handler_IDENTIFY(project_feature_handler_context_t *context) {}


static const project_feature_handler_f feature_handlers[] =
{
    #define ELEM(a, b, c, d, e, f, g, h, col, pvp) project_feature_handler_##a,
    #include "../common/list-elements.h"
    #undef ELEM
    #define PROJ_ENV(a, b, obv, col, desc, pvp) project_feature_handler_##a,
    #include "../common/list-project-environs.h"
    #undef PROJ_ENV
    #define PROJ_MON(a, b, obv, col, desc, pvp) NULL,
    #include "../common/list-project-monsters.h"
    #undef PROJ_MON
    NULL
};


/*
 * Called from project() to affect terrain features
 *
 * Called for projections with the PROJECT_GRID flag set, which includes
 * beam, ball and breath effects.
 *
 * who is the caster
 * r is the distance from the centre of the effect
 * c is the current cave
 * (y, x) the coordinates of the grid being handled
 * dam is the "damage" from the effect at distance r from the centre
 * typ is the projection (GF_) type
 *
 * Returns whether the effects were obvious
 *
 * Note that this function determines if the player can see anything that
 * happens by taking into account: blindness, line-of-sight, and illumination.
 */
bool project_f(struct actor *who, int r, struct chunk *c, int y, int x, int dam, int typ)
{
    bool obvious = false;
    project_feature_handler_context_t context;
    project_feature_handler_f feature_handler;
    bool line_sight = false;
    bool line_sound = false;
    bool is_blind = false;

    /* Set the player info */
    if (who->player)
    {
        line_sight = square_isview(who->player, y, x);
        line_sound = square_isseen(who->player, y, x);
        is_blind = who->player->timed[TMD_BLIND];
    }

    context.who = who;
    context.r = r;
    context.cave = c;
    context.y = y;
    context.x = x;
    context.dam = dam;
    context.type = typ;
    context.line_sight = line_sight;
    context.line_sound = line_sound;
    context.is_blind = is_blind;
    context.obvious = obvious;

    feature_handler = feature_handlers[typ];

    if (feature_handler != NULL) feature_handler(&context);

    /* Return "Anything seen?" */
    return context.obvious;
}
