/*
 * File: project-feat.c
 * Purpose: Projection effects on terrain
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * Copyright (c) 2019 MAngband and PWMAngband Developers
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
    struct source *origin;
    int r;
    struct chunk *cave;
    struct loc grid;
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

    /* No effect on special levels and towns */
    if (special_level(&context->cave->wpos) || in_town(&context->cave->wpos))
        return;

    /* Burn trees */
    if (square_isstrongtree(context->cave, &context->grid))
    {
        /* Message */
        if (context->line_sound)
            msg(context->origin->player, "The tree burns!");

        /* Burn the tree */
        square_burn_tree(context->cave, &context->grid);
    }

    /* Destroy trees */
    else if (square_iswitheredtree(context->cave, &context->grid))
    {
        /* Message */
        if (context->line_sound)
            msg(context->origin->player, "The tree burns to the ground!");

        /* Destroy the tree */
        square_destroy_tree(context->cave, &context->grid);

        /* Reapply illumination */
        square_illuminate(context->origin->player, context->cave, &context->grid, is_daytime());
        square_light_spot(context->cave, &context->grid);

        /* Update the visuals */
        update_visuals(&context->cave->wpos);

        /* Fully update the flow */
        fully_update_flow(&context->cave->wpos);
    }

    /* Burn grass */
    if (square_isgrass(context->cave, &context->grid))
    {
        /* Destroy the grass */
        square_burn_grass(context->cave, &context->grid);
    }

    /* Can create lava if extremely powerful. */
    if ((context->dam > randint1(1800) + 600) && square_isfloor(context->cave, &context->grid))
    {
        /* Forget the floor, make lava. */
        square_set_feat(context->cave, &context->grid, FEAT_LAVA);

        /* Objects that have survived should move */
        push_object(context->origin->player, context->cave, &context->grid);
    }
}


static void project_feature_handler_COLD(project_feature_handler_context_t *context)
{
    /* Grid is in line of sight and player is not blind */
    if (context->line_sight && !context->is_blind) context->obvious = true;

    /* Sufficiently intense cold can solidify lava. */
    if ((context->dam > randint1(900) + 300) && square_isfiery(context->cave, &context->grid))
    {
        bool occupied = square_isoccupied(context->cave, &context->grid);

        if (one_in_(2))
            square_set_feat(context->cave, &context->grid, FEAT_FLOOR);
        else if (one_in_(2) && !occupied)
            square_set_feat(context->cave, &context->grid, FEAT_RUBBLE);
        else
            square_set_feat(context->cave, &context->grid, FEAT_PASS_RUBBLE);
    }
}


static void project_feature_handler_POIS(project_feature_handler_context_t *context)
{
    /* Grid is in line of sight and player is not blind */
    if (context->line_sight && !context->is_blind) context->obvious = true;
}


/* Light up the grid */
static void project_feature_handler_LIGHT(project_feature_handler_context_t *context)
{
    struct loc grid = context->grid;

    /* Turn on the light */
    sqinfo_on(square(context->cave, &grid)->info, SQUARE_GLOW);

    /* Grid is in line of sight and player is not blind */
    if (context->line_sight && !context->is_blind) context->obvious = true;

    /* Note changes to viewable region */
    note_viewable_changes(&context->cave->wpos, &grid);
}


/* Darken the grid */
static void project_feature_handler_DARK(project_feature_handler_context_t *context)
{
    struct loc grid = context->grid;
    int i;

    /* No effect outside of the dungeon during day */
    if ((context->cave->wpos.depth == 0) && is_daytime()) return;

    /* No effect on special levels */
    if (special_level(&context->cave->wpos)) return;

    /* Turn off the light */
    square_unglow(context->cave, &grid);

    /* Check everyone */
    for (i = 1; i <= NumPlayers; i++)
    {
        struct player *p = player_get(i);

        /* If he's not here, skip him */
        if (!wpos_eq(&p->wpos, &context->cave->wpos)) continue;

        /* Hack -- forget "boring" grids */
        if (square_isview(p, &grid) && !square_isnormal(context->cave, &grid))
            square_forget(p, &grid);
    }

    /* Grid is in line of sight and player is not blind */
    if (context->line_sight && !context->is_blind) context->obvious = true;

    /* Note changes to viewable region */
    note_viewable_changes(&context->cave->wpos, &grid);
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

    /* Sufficiently intense cold can solidify lava. */
    if ((context->dam > randint1(900) + 300) && square_isfiery(context->cave, &context->grid))
    {
        bool occupied = square_isoccupied(context->cave, &context->grid);

        if (one_in_(2))
            square_set_feat(context->cave, &context->grid, FEAT_FLOOR);
        else if (one_in_(2) && !occupied)
            square_set_feat(context->cave, &context->grid, FEAT_RUBBLE);
        else
            square_set_feat(context->cave, &context->grid, FEAT_PASS_RUBBLE);
    }
}


static void project_feature_handler_GRAVITY(project_feature_handler_context_t *context)
{
    /* Grid is in line of sight and player is not blind */
    if (context->line_sight && !context->is_blind) context->obvious = true;
}


static void project_feature_handler_INERTIA(project_feature_handler_context_t *context)
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

    /* Can create lava if extremely powerful. */
    if ((context->dam > randint1(1800) + 600) && square_isfloor(context->cave, &context->grid))
    {
        /* Forget the floor, make lava. */
        square_set_feat(context->cave, &context->grid, FEAT_LAVA);

        /* Objects that have survived should move */
        push_object(context->origin->player, context->cave, &context->grid);
    }
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
    struct loc grid = context->grid;

    /* No effect on special levels or in towns */
    if (special_level(&context->cave->wpos) || in_town(&context->cave->wpos)) return;

    /* Non-walls (etc) */
    if (square_ispassable(context->cave, &grid) && !square_seemslikewall(context->cave, &grid))
        return;

    /* Permanent walls */
    if (square_isperm(context->cave, &grid) || square_isborder(context->cave, &grid))
        return;

    /* Different treatment for different walls */
    if (square_isrubble(context->cave, &grid))
    {
        /* Message */
        if (context->line_sound)
        {
            msg(context->origin->player, "The rubble turns into mud!");
            context->obvious = true;
        }

        /* Destroy the rubble */
        square_destroy_rubble(context->cave, &grid);

        /* Update the visuals */
        update_visuals(&context->cave->wpos);

        /* Fully update the flow */
        fully_update_flow(&context->cave->wpos);

        /* Hack -- place an object */
        if (magik(10))
        {
            /* Found something */
            if (context->line_sound)
                msg(context->origin->player, "There was something buried in the rubble!");

            /* Place object */
            place_object(context->origin->player, context->cave, &grid,
                object_level(&context->cave->wpos), false, false, ORIGIN_RUBBLE, 0);
        }
    }
    else if (square_home_iscloseddoor(context->cave, &grid))
    {
        /* Message */
        if (context->line_sound)
        {
            msg(context->origin->player, "The door resists.");
            context->obvious = true;
        }
    }
    else if (square_isdoor(context->cave, &grid) && !square_seemslikewall(context->cave, &grid))
    {
        struct monster *mon = square_monster(context->cave, &grid);

        /* Reveal mimics */
        if (mon && monster_is_camouflaged(mon))
            become_aware(context->origin->player, context->cave, mon);
        else
        {
            /* Hack -- special message */
            if (context->line_sound)
            {
                msg(context->origin->player, "The door crumbles to dust!");
                context->obvious = true;
            }

            /* Destroy the feature */
            square_destroy_door(context->cave, &grid);

            /* Update the visuals */
            update_visuals(&context->cave->wpos);
        }
    }
    else if (square_hasgoldvein(context->cave, &grid))
    {
        /* Message */
        if (context->line_sound)
        {
            msg(context->origin->player, "The vein turns into mud!");
            msg(context->origin->player, "You have found something!");
            context->obvious = true;
        }

        /* Destroy the wall */
        square_destroy_wall(context->cave, &grid);

        /* Update the visuals */
        update_visuals(&context->cave->wpos);

        /* Fully update the flow */
        fully_update_flow(&context->cave->wpos);

        /* Place some gold */
        place_gold(context->origin->player, context->cave, &grid,
            object_level(&context->cave->wpos), ORIGIN_FLOOR);
    }
    else if (square_ismagma(context->cave, &grid) || square_isquartz(context->cave, &grid))
    {
        /* Message */
        if (context->line_sound)
        {
            msg(context->origin->player, "The vein turns into mud!");
            context->obvious = true;
        }

        /* Destroy the wall */
        square_destroy_wall(context->cave, &grid);

        /* Update the visuals */
        update_visuals(&context->cave->wpos);

        /* Fully update the flow */
        fully_update_flow(&context->cave->wpos);
    }
    else if (square_isrock(context->cave, &grid))
    {
        /* Message */
        if (context->line_sound)
        {
            msg(context->origin->player, "The wall turns into mud!");
            context->obvious = true;
        }

        /* Destroy the wall */
        square_destroy_wall(context->cave, &grid);

        /* Update the visuals */
        update_visuals(&context->cave->wpos);

        /* Fully update the flow */
        fully_update_flow(&context->cave->wpos);
    }
}


/* Destroy Doors */
static void project_feature_handler_KILL_DOOR(project_feature_handler_context_t *context)
{
    struct loc grid = context->grid;

    /* Reveal mimics */
    if (square_isdoor(context->cave, &grid))
    {
        struct monster *mon = square_monster(context->cave, &grid);

        if (mon && monster_is_camouflaged(mon))
        {
            become_aware(context->origin->player, context->cave, mon);
            return;
        }
    }

    /* Destroy all doors */
    if (square_isdoor(context->cave, &grid))
    {
        /* Message */
        if (context->line_sound)
        {
            msg(context->origin->player, "There is a bright flash of light!");
            context->obvious = true;
        }

        /* Destroy the feature */
        square_destroy_door(context->cave, &grid);

        /* Visibility change */
        if (square_issecretdoor(context->cave, &grid) ||
            square_basic_iscloseddoor(context->cave, &grid))
        {
            update_visuals(&context->cave->wpos);
            /*note_viewable_changes(&context->cave->wpos, &grid);*/
        }
    }
}


/* Disable traps, unlock doors */
static void project_feature_handler_KILL_TRAP(project_feature_handler_context_t *context)
{
    struct loc grid = context->grid;

    /* Reveal secret doors */
    if (square_issecretdoor(context->cave, &grid))
    {
        place_closed_door(context->cave, &grid);

        /* Observe */
        if (context->line_sound) context->obvious = true;
    }

    /* Disable traps, unlock doors */
    if (square_isplayertrap(context->cave, &grid))
    {
        /* Message */
        if (context->line_sound)
        {
            msg(context->origin->player, "The trap seizes up.");
            context->obvious = true;
        }

        /* Disable the trap */
        square_disable_trap(context->origin->player, context->cave, &grid);
    }
    else if (square_islockeddoor(context->cave, &grid))
    {
        /* Unlock the door */
        square_unlock_door(context->cave, &grid);

        /* Message */
        if (context->line_sound)
        {
            msg(context->origin->player, "Click!");
            context->obvious = true;
        }
    }
}


/* Make doors */
static void project_feature_handler_MAKE_DOOR(project_feature_handler_context_t *context)
{
    struct loc grid = context->grid;

    /* Require a grid without monsters and players */
    if (square_monster(context->cave, &grid) || square_isplayer(context->cave, &grid))
        return;

    /* Require a floor grid */
    if (!square_isanyfloor(context->cave, &grid)) return;

    /* Push objects off the grid */
    if (square_object(context->cave, &grid))
        push_object(context->origin->player, context->cave, &grid);

    /* Create a closed door */
    square_close_door(context->cave, &grid);

    /* Observe */
    if (context->line_sound) context->obvious = true;

    /* Update the visuals */
    update_visuals(&context->cave->wpos);
}


/* Make traps */
static void project_feature_handler_MAKE_TRAP(project_feature_handler_context_t *context)
{
    struct loc grid = context->grid;
    int i;

    /* Require an "empty" floor grid */
    if (!square_isopen(context->cave, &grid)) return;

    /* Create a trap, try to notice it */
    if (one_in_(4))
    {
        square_add_trap(context->cave, &grid);

        /* Check everyone */
        for (i = 1; i <= NumPlayers; i++)
        {
            struct player *p = player_get(i);

            /* If he's not here, skip him */
            if (!wpos_eq(&p->wpos, &context->cave->wpos)) continue;

            square_reveal_trap(p, &grid, false, false);
        }
    }
    context->obvious = true;
}


/* Place a wall */
static void project_feature_handler_STONE_WALL(project_feature_handler_context_t *context)
{
    struct loc grid = context->grid;

    /* Require a "naked" floor grid */
    if (!square_isempty(context->cave, &grid)) return;

    /* Place a wall */
    square_add_wall(context->cave, &grid);

    /* Observe */
    if (context->line_sound) context->obvious = true;

    /* Update the visuals */
    update_visuals(&context->cave->wpos);

    /* Fully update the flow */
    fully_update_flow(&context->cave->wpos);
}


static void project_feature_handler_RAISE(project_feature_handler_context_t *context) {}
static void project_feature_handler_AWAY_EVIL(project_feature_handler_context_t *context) {}
static void project_feature_handler_AWAY_SPIRIT(project_feature_handler_context_t *context) {}
static void project_feature_handler_AWAY_ALL(project_feature_handler_context_t *context) {}
static void project_feature_handler_TURN_UNDEAD(project_feature_handler_context_t *context) {}
static void project_feature_handler_TURN_LIVING(project_feature_handler_context_t *context) {}
static void project_feature_handler_TURN_ALL(project_feature_handler_context_t *context) {}
static void project_feature_handler_DISP_UNDEAD(project_feature_handler_context_t *context) {}
static void project_feature_handler_DISP_EVIL(project_feature_handler_context_t *context) {}
static void project_feature_handler_DISP_ALL(project_feature_handler_context_t *context) {}
static void project_feature_handler_SLEEP_UNDEAD(project_feature_handler_context_t *context) {}
static void project_feature_handler_SLEEP_EVIL(project_feature_handler_context_t *context) {}
static void project_feature_handler_SLEEP_ALL(project_feature_handler_context_t *context) {}
static void project_feature_handler_MON_CLONE(project_feature_handler_context_t *context) {}
static void project_feature_handler_MON_POLY(project_feature_handler_context_t *context) {}
static void project_feature_handler_MON_HEAL(project_feature_handler_context_t *context) {}
static void project_feature_handler_MON_SPEED(project_feature_handler_context_t *context) {}
static void project_feature_handler_MON_SLOW(project_feature_handler_context_t *context) {}
static void project_feature_handler_MON_CONF(project_feature_handler_context_t *context) {}
static void project_feature_handler_MON_HOLD(project_feature_handler_context_t *context) {}
static void project_feature_handler_MON_STUN(project_feature_handler_context_t *context) {}
static void project_feature_handler_MON_DRAIN(project_feature_handler_context_t *context) {}
static void project_feature_handler_MON_CRUSH(project_feature_handler_context_t *context) {}
static void project_feature_handler_PSI(project_feature_handler_context_t *context) {}
static void project_feature_handler_PSI_DRAIN(project_feature_handler_context_t *context) {}
static void project_feature_handler_CURSE(project_feature_handler_context_t *context) {}
static void project_feature_handler_CURSE2(project_feature_handler_context_t *context) {}
static void project_feature_handler_DRAIN(project_feature_handler_context_t *context) {}
static void project_feature_handler_COMMAND(project_feature_handler_context_t *context) {}
static void project_feature_handler_TELE_TO(project_feature_handler_context_t *context) {}
static void project_feature_handler_TELE_LEVEL(project_feature_handler_context_t *context) {}
static void project_feature_handler_MON_BLIND(project_feature_handler_context_t *context) {}
static void project_feature_handler_DRAIN_MANA(project_feature_handler_context_t *context) {}
static void project_feature_handler_FORGET(project_feature_handler_context_t *context) {}
static void project_feature_handler_BLAST(project_feature_handler_context_t *context) {}
static void project_feature_handler_SMASH(project_feature_handler_context_t *context) {}
static void project_feature_handler_CONTROL(project_feature_handler_context_t *context) {}
static void project_feature_handler_PROJECT(project_feature_handler_context_t *context) {}


/* Place a tree */
static void project_feature_handler_TREES(project_feature_handler_context_t *context)
{
    struct loc grid = context->grid;

    /* Only half of the time */
    if (magik(50)) return;

    /* Require a "naked" floor grid */
    if (!square_isempty(context->cave, &grid)) return;

    /* Place a tree */
    square_add_tree(context->cave, &grid);

    /* Observe */
    if (context->line_sound) context->obvious = true;

    /* Update the visuals */
    update_visuals(&context->cave->wpos);

    /* Fully update the flow */
    fully_update_flow(&context->cave->wpos);
}


static void project_feature_handler_AWAY_ANIMAL(project_feature_handler_context_t *context) {}


static const project_feature_handler_f feature_handlers[] =
{
    #define ELEM(a, b, c, d) project_feature_handler_##a,
    #include "../common/list-elements.h"
    #undef ELEM
    #define PROJ(a) project_feature_handler_##a,
    #include "../common/list-projections.h"
    #undef PROJ
    NULL
};


/*
 * Called from project() to affect terrain features
 *
 * Called for projections with the PROJECT_GRID flag set, which includes
 * beam, ball and breath effects.
 *
 * origin is the origin of the effect
 * r is the distance from the centre of the effect
 * c is the current cave
 * (y, x) the coordinates of the grid being handled
 * dam is the "damage" from the effect at distance r from the centre
 * typ is the projection (PROJ_) type
 *
 * Returns whether the effects were obvious
 *
 * Note that this function determines if the player can see anything that
 * happens by taking into account: blindness, line-of-sight, and illumination.
 */
bool project_f(struct source *origin, int r, struct chunk *c, struct loc *grid, int dam, int typ)
{
    bool obvious = false;
    project_feature_handler_context_t context;
    project_feature_handler_f feature_handler;
    bool line_sight = false;
    bool line_sound = false;
    bool is_blind = false;

    /* Set the player info */
    if (origin->player)
    {
        line_sight = square_isview(origin->player, grid);
        line_sound = square_isseen(origin->player, grid);
        is_blind = origin->player->timed[TMD_BLIND];
    }

    context.origin = origin;
    context.r = r;
    context.cave = c;
    loc_copy(&context.grid, grid);
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
