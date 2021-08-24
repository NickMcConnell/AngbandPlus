/*
 * File: mon-predicate.h
 * Purpose: Monster predicates
 */

#ifndef MONSTER_PREDICATE_H
#define MONSTER_PREDICATE_H

extern bool monster_is_nonliving(const struct monster_race *race);
extern bool monster_is_destroyed(const struct monster_race *race);
extern bool monster_passes_walls(const struct monster_race *race);
extern bool monster_is_invisible(const struct monster_race *race);
extern bool monster_is_unique(const struct monster_race *race);
extern bool monster_is_stupid(const struct monster_race *race);
extern bool monster_is_smart(const struct monster_race *race);
extern bool monster_is_evil(const struct monster_race *race);
extern bool monster_is_powerful(const struct monster_race *race);

extern bool monster_is_in_view(struct player *p, int m_idx);
extern bool monster_is_visible(struct player *p, int m_idx);
extern bool monster_is_camouflaged(const struct monster *mon);
extern bool monster_is_obvious(struct player *p, int m_idx, const struct monster *mon);
extern bool monster_is_mimicking(const struct monster *mon);

#endif /* MONSTER_PREDICATE_H */
