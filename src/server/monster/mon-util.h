/*
 * File: mon-util.h
 * Purpose: Structures and functions for monster utilities.
 */

#ifndef MONSTER_UTILITIES_H
#define MONSTER_UTILITIES_H

/** Constants **/

/*
 * Bit flags for the "monster_desc" function
 */
#define MDESC_OBJE      0x01    /* Objective (or Reflexive) */
#define MDESC_POSS      0x02    /* Possessive (or Reflexive) */
#define MDESC_IND1      0x04    /* Indefinites for hidden monsters */
#define MDESC_IND2      0x08    /* Indefinites for visible monsters */
#define MDESC_PRO1      0x10    /* Pronominalize hidden monsters */
#define MDESC_PRO2      0x20    /* Pronominalize visible monsters */
#define MDESC_HIDE      0x40    /* Assume the monster is hidden */
#define MDESC_SHOW      0x80    /* Assume the monster is visible */
#define MDESC_CAPITAL   0x100   /* Capitalise */

/** Macros **/

/** Structures **/

/** Variables **/
extern char summon_kin_type;

/** Functions **/
extern monster_base *lookup_monster_base(const char *name);
extern bool match_monster_bases(const monster_base *base, ...);
extern void plural_aux(char *name, size_t max);
extern void display_monlist(struct player *p, bool do_cmd);
extern void player_desc(struct player *p, char *desc, size_t max, struct player *q, bool capitalize);
extern void monster_desc(struct player *p, char *desc, size_t max, const monster_type *m_ptr,
    int mode);
extern void update_mon(int depth, int m_idx, bool full);
extern void update_monsters(int depth, bool full);
extern s16b monster_carry(struct monster *m, object_type *j_ptr, bool force);
extern void monster_swap(int depth, int y1, int x1, int y2, int x2);
extern int summon_specific(struct player *p, int y1, int x1, int lev, int type, int delay, int chance);
extern bool clone_mon(int Ind, int depth, int m_idx);
extern void aware_player(struct player *p, struct player *q);
extern void become_aware(struct player *p, struct monster *m_ptr);
extern bool is_mimicking(struct monster *m_ptr);
extern void update_smart_learn(struct monster *m, struct player *p, int flag);
extern void update_player(int Ind);
extern void update_players(void);
extern bool summon_specific_race(int Ind, int depth, int y1, int x1, int r_idx, unsigned char num);
extern bool summon_specific_race_somewhere(int Ind, int depth, int r_idx, unsigned char num);
extern int race_index(char *name);
extern int race_index_fuzzy(char *name);
extern bool is_humanoid(const monster_race *r_ptr);
extern bool is_half_humanoid(const monster_race *r_ptr);
extern void update_monlist(struct monster *m);

#endif /* MONSTER_UTILITIES_H */
