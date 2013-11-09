#ifndef INCLUDED_INT_MAP_H
#define INCLUDED_INT_MAP_H

typedef void* vptr;
typedef struct int_map_s int_map_t;
typedef int_map_t *int_map_ptr;
typedef void (*free_value_f)(vptr v);

extern int_map_ptr int_map_alloc(free_value_f free);
extern void        int_map_free(int_map_ptr map);

extern void        int_map_add(int_map_ptr map, int key, vptr val);
extern int         int_map_delete(int_map_ptr map, int key);
extern vptr        int_map_find(int_map_ptr map, int key);
extern int         int_map_contains(int_map_ptr map, int key);
extern void        int_map_clear(int_map_ptr map);

#endif