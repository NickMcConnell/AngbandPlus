#ifndef INCLUDED_INT_MAP_H
#define INCLUDED_INT_MAP_H

typedef struct int_map_s int_map_t;
typedef int_map_t *int_map_ptr;
typedef struct int_map_iter_s int_map_iter_t;
typedef int_map_iter_t *int_map_iter_ptr;
typedef void (*int_map_free_f)(void *v);

extern int_map_ptr int_map_alloc(int_map_free_f free);
extern void        int_map_free(int_map_ptr map);

extern void        int_map_add(int_map_ptr map, int key, void *val);
extern int         int_map_delete(int_map_ptr map, int key);
extern void *      int_map_find(int_map_ptr map, int key);
extern int         int_map_contains(int_map_ptr map, int key);
extern void        int_map_clear(int_map_ptr map);
extern int         int_map_count(int_map_ptr map);

                        /* Iteration */
extern int_map_iter_ptr int_map_iter_alloc(int_map_ptr map);
extern void             int_map_iter_free(int_map_iter_ptr iter);
extern int              int_map_iter_is_valid(int_map_iter_ptr iter);
extern void *           int_map_iter_current(int_map_iter_ptr iter);
extern int              int_map_iter_current_key(int_map_iter_ptr iter);
extern void             int_map_iter_next(int_map_iter_ptr iter);

#endif
