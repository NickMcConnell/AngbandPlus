#ifndef INCLUDED_STR_MAP_H
#define INCLUDED_STR_MAP_H

typedef const char* cptr;
typedef void* vptr;
typedef struct str_map_s str_map_t;
typedef str_map_t *str_map_ptr;
typedef void (*free_value_f)(vptr v);

extern str_map_ptr str_map_alloc(free_value_f free);
extern void        str_map_free(str_map_ptr map);

extern void        str_map_add(str_map_ptr map, cptr key, vptr val);
extern int         str_map_delete(str_map_ptr map, cptr key);
extern vptr        str_map_find(str_map_ptr map, cptr key);
extern int         str_map_contains(str_map_ptr map, cptr key);
extern void        str_map_clear(str_map_ptr map);

#endif