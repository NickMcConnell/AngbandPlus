/*
 * File: mon-list.h
 * Purpose: Monster list construction.
 */

#ifndef MONSTER_LIST_H
#define MONSTER_LIST_H

typedef enum monster_list_section_e
{
	MONSTER_LIST_SECTION_LOS = 0,
	MONSTER_LIST_SECTION_ESP,
	MONSTER_LIST_SECTION_MAX
} monster_list_section_t;

typedef struct monster_list_entry_s
{
	struct monster_race *race;
	u16b count[MONSTER_LIST_SECTION_MAX];
	u16b asleep[MONSTER_LIST_SECTION_MAX];
	s16b dx[MONSTER_LIST_SECTION_MAX], dy[MONSTER_LIST_SECTION_MAX];
	byte attr;
} monster_list_entry_t;

typedef struct monster_list_s
{
	monster_list_entry_t *entries;
	size_t entries_size;
	u16b distinct_entries;
    bool sorted;
	u16b total_entries[MONSTER_LIST_SECTION_MAX];
	u16b total_monsters[MONSTER_LIST_SECTION_MAX];
} monster_list_t;

extern monster_list_t *monster_list_new(struct player *p);
extern void monster_list_free(monster_list_t *list);
extern void monster_list_init(struct player *p);
extern void monster_list_finalize(struct player *p);
extern monster_list_t *monster_list_shared_instance(struct player *p);
extern void monster_list_reset(struct player *p, monster_list_t *list);
extern void monster_list_collect(struct player *p, monster_list_t *list);
extern int monster_list_standard_compare(const void *a, const void *b);
extern int monster_list_compare_exp(const void *a, const void *b);
extern void monster_list_sort(monster_list_t *list, int (*compare)(const void *, const void *));
extern byte monster_list_entry_line_color(struct player *p, const monster_list_entry_t *entry);

#endif /* MONSTER_LIST_H */