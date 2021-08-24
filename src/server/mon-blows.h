/*
 * File: mon-blows.h
 * Purpose: Monster melee module.
 */

#ifndef MON_BLOWS_H
#define MON_BLOWS_H

enum
{
    TYPE_MVP,   /* make_attack_normal (MvP) */
    TYPE_PVX,   /* py_attack_real */
    TYPE_MVM    /* make_attack_normal (MvM) */
};

struct blow_method
{
    char *name;
    bool cut;
    bool stun;
    bool miss;
    bool phys;
    int msgt;
    char *act_msg;
    char *desc;
    char *flavor;
    struct blow_method *next;
};

extern struct blow_method *blow_methods;

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
    struct source *target;
	struct monster_lore *target_l_ptr;
	int rlev;
	struct blow_method *method;
	int ac;
	char *ddesc;
	bool obvious;
    char flav[160];
    bool visible;
    bool dead;
    bool do_blind;
    bool do_conf;
    bool do_fear;
    bool do_quake;
    bool do_para;
    int do_stun;
	int blinked;
	int damage;
    byte note_dies;
    byte style;
} melee_effect_handler_context_t;

/*
 * Melee blow effect handler.
 */
typedef void (*melee_effect_handler_f)(melee_effect_handler_context_t *);

struct blow_effect
{
    char *name;
    int power;
    int eval;
    char *desc;
    byte lore_attr;         /* Color of the attack used in lore text */
    byte lore_attr_resist;  /* Color used in lore text when resisted */
    byte lore_attr_immune;  /* Color used in lore text when resisted strongly */
    char *effect_type;
    int resist;
    struct blow_effect *next;
};

extern struct blow_effect *blow_effects;

/* Functions */
extern const char *monster_blow_method_action(struct blow_method *method);
extern melee_effect_handler_f melee_handler_for_blow_effect(const char *name);
extern byte blow_method_index(const char *name);
extern byte blow_effect_index(const char *name);

#endif /* MON_BLOWS */
