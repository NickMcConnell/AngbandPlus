/*
 * File: mon-blow-effects.h
 * Purpose: Functions for managing monster melee effects.
 */

#ifndef MON_BLOW_EFFECTS_H
#define MON_BLOW_EFFECTS_H

/*
 * List of monster blow effects
 */
enum monster_blow_effect_e
{
    #define RBE(x, p, e, d) RBE_##x,
    #include "list-blow-effects.h"
    #undef RBE
    RBE_MAX
};

enum
{
    RBE_TYPE_MVP,   /* make_attack_normal (MvP) */
    RBE_TYPE_PVX,   /* py_attack_real */
    RBE_TYPE_MVM    /* make_attack_normal (MvM) */
};

/*
 * Storage for context information for effect handlers called in make_attack_normal().
 *
 * The members of this struct are initialized in an order-dependent way (to be more cross-
 * platform). If the members change, make sure to change any initializers. Ideally, this
 * should eventually used named initializers.
 */
typedef struct melee_effect_handler_context_s
{
	struct player *p;
	struct monster *mon;
    struct actor *target;
	struct monster_lore *target_l_ptr;
	int rlev;
	monster_blow_method_t method;
	int ac;
	char *ddesc;
	bool obvious;
    bool flav;
    bool visible;
    bool dead;
    bool do_blind;
    bool do_conf;
    bool do_fear;
    bool do_quake;
    bool do_para;
    int do_stun;
	int blinked;
    bool do_break;
	int damage;
    byte note_dies;
    byte style;
} melee_effect_handler_context_t;

/*
 * Melee blow effect handler.
 */
typedef void (*melee_effect_handler_f)(melee_effect_handler_context_t *);

/*
 * Storage class for monster_blow_effect_e.
 */
typedef byte monster_blow_effect_t;

/* Functions */
extern int monster_blow_effect_power(monster_blow_effect_t effect);
extern int monster_blow_effect_eval(monster_blow_effect_t effect);
extern bool monster_blow_effect_is_valid(monster_blow_effect_t effect);
extern const char *monster_blow_effect_description(monster_blow_effect_t effect);
extern melee_effect_handler_f melee_handler_for_blow_effect(monster_blow_effect_t effect);
extern monster_blow_effect_t blow_effect_name_to_idx(const char *string);

#endif /* MON_BLOW_EFFECTS_H */
