/*
 * File: externs.h
 * Purpose: Extern declarations (variables and functions)
 */

#ifndef INCLUDED_EXTERNS_H
#define INCLUDED_EXTERNS_H

#include "guid.h"

/* util.c */
extern int tval_find_idx(const char *name);
extern const char *tval_find_name(int tval);
extern size_t obj_desc_name_format(char *buf, size_t max, size_t end, const char *fmt,
    const char *modstr, bool pluralise);
extern int lookup_sval(int tval, const char *name);
extern object_kind *lookup_kind(int tval, int sval);
extern void cnv_stat(int val, char *out_val, size_t out_len);
extern int color_char_to_attr(char c);
extern int color_text_to_attr(const char *name);
extern bool get_incarnation(int n, char *name, size_t len);
extern const char *likert(int x, int y, byte *attr);
extern s32b adv_exp(s16b lev, s16b expfact);
extern bool item_tester_okay(struct player *p, const object_type *o_ptr);
extern bool obj_is_ammo(struct player *p, const object_type *o_ptr);
extern player_type *get_player(int Ind);
extern const char *get_title(struct player *p);
extern s16b get_speed(struct player *p);
extern void get_plusses(struct player *p, int* pfhit, int* pfdam, int* pmhit, int* pmdam,
    int* pshit, int* psdam);
extern byte get_dtrap(struct player *p);
extern struct player_class *player_id2class(guid id);
extern int player_cmax(void);
extern struct player_race *player_id2race(guid id);
extern int player_rmax(void);

/* variable.c */
extern u32b *num_names;
extern const char ***name_sections;
extern maxima *z_info;
extern object_kind *k_info;
extern struct player_race *races;
extern struct player_class *classes;
extern social_type *soc_info;
extern struct hint *hints;
extern monster_race *r_info;
extern s16b cfg_fps;
extern bool item_tester_full;
extern byte item_tester_tval;
extern bool (*item_tester_hook)(struct player *p, const object_type *o_ptr);
extern const char *stat_names[A_MAX];
extern const char *stat_names_reduced[A_MAX];
extern color_type color_table[MAX_COLORS];
extern player_sex sex_info[MAX_SEXES];
extern char *ANGBAND_DIR_EDIT;
extern char *ANGBAND_DIR_FILE;
extern char *ANGBAND_DIR_PREF;
extern char *ANGBAND_DIR_USER;

#endif /* INCLUDED_EXTERNS_H */
