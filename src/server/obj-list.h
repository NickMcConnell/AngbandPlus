/*
 * File: obj-list.h
 * Purpose: Object list construction.
 */

#ifndef OBJECT_LIST_H
#define OBJECT_LIST_H

#define MAX_ITEMLIST 2560

typedef enum object_list_section_e
{
    OBJECT_LIST_SECTION_LOS = 0,
    OBJECT_LIST_SECTION_NO_LOS,
    OBJECT_LIST_SECTION_MAX
} object_list_section_t;

typedef struct object_list_entry_s
{
	struct object *object;
	u16b count[OBJECT_LIST_SECTION_MAX];
	s16b dx, dy;
    struct player *player;
} object_list_entry_t;

typedef struct object_list_s
{
	object_list_entry_t *entries;
	size_t entries_size;
    u16b distinct_entries;
	u16b total_entries[OBJECT_LIST_SECTION_MAX];
	u16b total_objects[OBJECT_LIST_SECTION_MAX];
	bool sorted;
} object_list_t;

extern object_list_t *object_list_new(void);
extern void object_list_free(object_list_t *list);
extern void object_list_init(struct player *p);
extern void object_list_finalize(struct player *p);
extern object_list_t *object_list_shared_instance(struct player *p);
extern void object_list_reset(object_list_t *list);
extern void object_list_collect(struct player *p, object_list_t *list);
extern int object_list_standard_compare(const void *a, const void *b);
extern void object_list_sort(object_list_t *list, int (*compare)(const void *, const void *));
extern byte object_list_entry_line_attribute(struct player *p, const object_list_entry_t *entry);
extern void object_list_format_name(struct player *p, const object_list_entry_t *entry,
    char *line_buffer, size_t size);

#endif /* OBJECT_LIST_H */
