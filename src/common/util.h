/*
 * File: util.h
 * Purpose: Utility functions
 */

#ifndef INCLUDED_UTIL_H
#define INCLUDED_UTIL_H

#define DIR_TARGET  5

extern const char *stat_names[STAT_MAX];
extern const char *stat_names_reduced[STAT_MAX];
extern s16b ddx[10];
extern s16b ddy[10];

extern void cleanup_p_race(void);
extern void cleanup_realm(void);
extern void free_effect(struct effect *source);
extern void cleanup_class(void);
extern void cleanup_body(void);
extern size_t obj_desc_name_format(char *buf, size_t max, size_t end, const char *fmt,
    const char *modstr, bool pluralise);
extern void object_kind_name(char *buf, size_t max, const struct object_kind *kind, bool aware);
extern int lookup_sval(int tval, const char *name);
extern int lookup_sval_silent(int tval, const char *name);
extern void object_short_name(char *buf, size_t max, const char *name);
extern struct object_kind *lookup_kind(int tval, int sval);
extern struct object_kind *lookup_kind_silent(int tval, int sval);
extern struct object_kind *lookup_kind_by_name(int tval, const char *name);
extern void cnv_stat(int val, char *out_val, size_t out_len);
extern void get_next_incarnation(char *name, size_t len);
extern bool get_previous_incarnation(char *name, size_t len);
extern const char *strip_suffix(const char *name);
extern const char *likert(int x, int y, byte *attr);
extern s32b adv_exp(s16b lev, s16b expfact);
extern bool object_test(struct player *p, item_tester tester, const struct object *obj);
extern const char *get_title(struct player *p);
extern s16b get_speed(struct player *p);
extern void get_plusses(struct player *p, struct player_state *state, int* dd, int* ds,
    int* mhit, int* mdam, int* shit, int* sdam);
extern s16b get_melee_skill(struct player *p);
extern s16b get_ranged_skill(struct player *p);
extern byte get_dtrap(struct player *p);
extern int get_diff(struct player *p);
extern struct timed_grade *get_grade(int i);
extern struct player_class *player_id2class(guid id);
extern struct player_class *lookup_player_class(const char *name);
extern int player_cmax(void);
extern struct player_race *player_id2race(guid id);
extern int player_rmax(void);
extern int player_bmax(void);
extern ignore_type_t ignore_type_of(const struct object *obj);
extern bool ego_has_ignore_type(struct ego_item *ego, ignore_type_t itype);
extern struct monster_base *lookup_monster_base(const char *name);
extern struct monster_race *lookup_monster(const char *name);
extern s16b modify_stat_value(int value, int amount);
extern int message_lookup_by_name(const char *name);
extern void player_embody(struct player *p);
extern const struct magic_realm *lookup_realm(const char *name);
extern struct trap_kind *lookup_trap(const char *desc);
extern int recharge_failure_chance(const struct object *obj, int strength);
extern int race_modifier(const struct player_race *race, int mod, int lvl, bool poly); 
extern int class_modifier(const struct player_class *clazz, int mod, int lvl);

#endif /* INCLUDED_UTIL_H */
