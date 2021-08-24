/*
 * File: mon-blow-methods.h
 * Purpose: Functions for managing monster melee methods.
 */

#ifndef MON_BLOW_METHODS_H
#define MON_BLOW_METHODS_H

/*
 * List of blow method constants
 */
enum monster_blow_method_e
{
    #define RBM(x, c, s, miss, p, m, a, d, f) RBM_##x,
    #include "list-blow-methods.h"
    #undef RBM
    RBM_MAX
};

/*
 * Storage class for monster_blow_effect_e.
 */
typedef byte monster_blow_method_t;

/* Functions */
extern bool monster_blow_method_cut(monster_blow_method_t method);
extern bool monster_blow_method_stun(monster_blow_method_t method);
extern bool monster_blow_method_miss(monster_blow_method_t method);
extern bool monster_blow_method_physical(monster_blow_method_t method);
extern const char *monster_blow_method_flavor(monster_blow_method_t method);
extern bool monster_blow_method_is_valid(monster_blow_method_t method);
extern int monster_blow_method_message(monster_blow_method_t method);
extern const char *monster_blow_method_action(monster_blow_method_t method);
extern const char *monster_blow_method_description(monster_blow_method_t method);
extern monster_blow_method_t blow_method_name_to_idx(const char *string);

#endif /* MON_BLOW_METHODS_H */
