/*
 * File: mon-lore.h
 * Purpose: Structures and functions for monster recall.
 */

#ifndef MONSTER_LORE_H
#define MONSTER_LORE_H

/** Constants **/

/** Macros **/

/** Structures **/

/** Variables **/

/** Functions **/
extern void get_global_lore(struct player *p, int r_idx, monster_lore* l_ptr);
extern void monster_info_screen(struct player *p, int r_idx);
extern void describe_monster(struct player *p, int r_idx);
extern void lore_do_probe(struct player *p, int m_idx);
extern void lore_treasure(int Ind, struct monster *m_ptr, int num_item, int num_gold);
extern void monster_flags_known(const monster_race *r_ptr, const monster_lore *l_ptr,
    bitflag flags[RF_SIZE]);

#endif /* MONSTER_LORE_H */
