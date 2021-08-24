#ifndef INCLUDED_POINT_MAP_H
#define INCLUDED_POINT_MAP_H

#include "map.h"
#include "rect.h"

typedef struct point_map_s point_map_t, *point_map_ptr;
typedef struct point_map_iter_s point_map_iter_t, *point_map_iter_ptr;

typedef void (*point_map_free_f)(void *v);
typedef void (*point_map_iter_f)(point_t key, void *val);
typedef void (*point_map_iter_int_f)(point_t key, int val);

extern point_map_ptr point_map_alloc(point_map_free_f free);
extern void          point_map_free(point_map_ptr map);

extern void          point_map_add(point_map_ptr map, point_t key, void *val);
extern void          point_map_add_int(point_map_ptr map, point_t key, int val);
extern bool          point_map_delete(point_map_ptr map, point_t key);
extern void *        point_map_detach(point_map_ptr map, point_t key);
extern void *        point_map_find(point_map_ptr map, point_t key);
extern int           point_map_find_int(point_map_ptr map, point_t key);
extern bool          point_map_contains(point_map_ptr map, point_t key);
extern void          point_map_clear(point_map_ptr map);
extern int           point_map_count(point_map_ptr map);
extern void          point_map_iter(point_map_ptr map, point_map_iter_f f);
extern void          point_map_iter_int(point_map_ptr map, point_map_iter_int_f f);

                     /* Iteration */
extern point_map_iter_ptr
                     point_map_iter_alloc(point_map_ptr map);
extern void          point_map_iter_free(point_map_iter_ptr iter);
extern bool          point_map_iter_is_valid(point_map_iter_ptr iter);
extern void *        point_map_iter_current(point_map_iter_ptr iter);
extern point_t       point_map_iter_current_key(point_map_iter_ptr iter);
extern void          point_map_iter_next(point_map_iter_ptr iter);

#endif
