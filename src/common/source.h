/*
 * File: source.h
 * Purpose: Type that allows various different origins for an effect
 */

#ifndef EFFECT_SOURCE_H
#define EFFECT_SOURCE_H

/*
 * Structure that tells you where an effect came from
 */
struct source
{
    int idx;
    struct player *player;
    struct monster *monster;
    struct trap *trap;
    struct player *target;  /* Hack -- wraithed player as target */
};

/*
 * Generate different forms of the source for projection and effect functions
 */
extern void source_trap(struct source *source, struct trap *trap);
extern void source_monster(struct source *source, struct monster *monster);
extern void source_player(struct source *source, int idx, struct player *player);
extern void source_both(struct source *source, struct player *player, struct monster *monster);

extern bool source_null(struct source *source);
extern bool source_equal(struct source *source1, struct source *source2);
extern bool source_equal_player_or_monster(struct source *source1, struct source *source2);

#endif /* EFFECT_SOURCE_H */
