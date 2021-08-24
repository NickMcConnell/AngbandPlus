/*
 * File: mon-group.h
 * Purpose: Monster group behaviours
 */

#ifndef MON_GROUP_H
#define MON_GROUP_H

struct mon_group_list_entry
{
    int midx;
    struct mon_group_list_entry *next;
};

struct monster_group
{
    int index;
    int leader;
    struct mon_group_list_entry *member_list;
};

extern struct monster_group *monster_group_new(void);
extern void monster_group_free(struct monster_group *group);
extern void monster_remove_from_groups(struct chunk *c, struct monster *mon);
extern int monster_group_index_new(struct chunk *c);
extern void monster_add_to_group(struct monster *mon, struct monster_group *group);
extern void monster_group_start(struct chunk *c, struct monster *mon, int which);
extern void monster_group_assign(struct chunk *c, struct monster *mon,
    struct monster_group_info *info, bool loading);
extern int monster_group_index(struct monster_group *group);
extern struct monster_group *monster_group_by_index(struct chunk *c, int index);
extern bool monster_group_change_index(struct chunk *c, int new_index, int old_index);
extern struct monster_group *summon_group(struct chunk *c, struct monster *mon);
extern void monster_group_rouse(struct player *p, struct chunk *c, struct monster *mon);
extern int monster_primary_group_size(struct chunk *c, const struct monster *mon);
extern struct monster *group_monster_tracking(struct chunk *c, const struct monster *mon);
extern int monster_group_leader_idx(struct monster_group *group);
extern struct monster *monster_group_leader(struct chunk *c, struct monster *mon);
extern void monster_groups_verify(struct chunk *c);

#endif /* MON_GROUP_H */
