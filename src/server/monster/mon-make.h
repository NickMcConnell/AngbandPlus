/*
 * File: mon-make.h
 * Purpose: Structures and functions for monster creation / deletion.
 */

#ifndef MONSTER_MAKE_H
#define MONSTER_MAKE_H

/** Constants **/

/** Macros **/

/** Structures **/

/** Variables **/

/** Functions **/
extern void delete_monster_idx(struct cave *c, int m_idx);
extern void delete_monster(int depth, int y, int x);
extern void compact_monsters(struct cave *c, int num_to_compact);
extern void wipe_mon_list(struct cave *c);
extern void get_mon_num_prep(void);
extern s16b get_mon_num(int depth, int level);
extern void mon_create_drops(struct player *p, int m_idx);
extern s16b place_monster(struct player *p, monster_type *n_ptr, byte origin);
extern int mon_hp(const struct monster_race *r_ptr, aspect hp_aspect);
extern bool place_new_monster(struct player *p, struct cave *c, int y, int x, int r_idx,
    byte mon_flag, byte origin);
extern bool pick_and_place_monster(struct player *p, struct cave *c, int y, int x, int depth,
    byte mon_flag, byte origin);
extern bool pick_and_place_distant_monster(struct player *p, struct cave *c, int dis, byte mon_flag);
extern void monster_give_xp(int Ind, struct monster *m_ptr, bool split);
extern bool mon_take_hit(int Ind, struct monster *m_ptr, int dam, bool *fear, const char *note);
extern void monster_drop_carried(int Ind, struct monster *m_ptr, int num, bool visible,
    int *dump_item, int *dump_gold);
extern void monster_drop_corpse(int Ind, struct monster *m_ptr);

#endif /* MONSTER_MAKE_H */
