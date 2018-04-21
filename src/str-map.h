#ifndef INCLUDED_STR_MAP_H
#define INCLUDED_STR_MAP_H

typedef struct str_map_s str_map_t;
typedef str_map_t *str_map_ptr;
typedef struct str_map_iter_s str_map_iter_t;
typedef str_map_iter_t *str_map_iter_ptr;
typedef void (*str_map_free_f)(void *v);

extern str_map_ptr str_map_alloc(str_map_free_f free);
extern void        str_map_free(str_map_ptr map);

extern void        str_map_add(str_map_ptr map, const char *key, void *val); /* key is copied */
extern int         str_map_delete(str_map_ptr map, const char *key);
extern void *      str_map_find(str_map_ptr map, const char *key);
extern int         str_map_contains(str_map_ptr map, const char *key);
extern void        str_map_clear(str_map_ptr map);
extern int         str_map_count(str_map_ptr map);

                        /* Iteration */
extern str_map_iter_ptr str_map_iter_alloc(str_map_ptr map);
extern void             str_map_iter_free(str_map_iter_ptr iter);
extern int              str_map_iter_is_valid(str_map_iter_ptr iter);
extern void *           str_map_iter_current(str_map_iter_ptr iter);
extern const char *     str_map_iter_current_key(str_map_iter_ptr iter);
extern void             str_map_iter_next(str_map_iter_ptr iter);

#endif
