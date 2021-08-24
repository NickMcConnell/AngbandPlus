#ifndef INCLUDED_C_VEC_H
#define INCLUDED_C_VEC_H

#include "h-basic.h"

typedef struct vec_s vec_t, *vec_ptr;
typedef void (*vec_free_f)(vptr v);
typedef void (*vec_item_f)(vptr v);
typedef bool (*vec_item_p)(vptr v);
typedef int  (*vec_cmp_f) (const void *l, const void *r);

extern int vec_compare_int(const void *l, const void *r);

extern vec_ptr vec_alloc(vec_free_f free);
extern void    vec_free(vec_ptr vec);

extern void    vec_add(vec_ptr vec, vptr obj);
extern void    vec_add_int(vec_ptr vec, int val);
extern void    vec_clear(vec_ptr vec);
extern vptr    vec_get(vec_ptr vec, int i);
extern vptr    vec_random(vec_ptr vec);
extern int     vec_get_int(vec_ptr vec, int i);
extern int     vec_random_int(vec_ptr vec);
extern void    vec_delete(vec_ptr vec, int i);        /* frees vec[i] */
extern void    vec_set(vec_ptr vec, int i, vptr obj); /* frees old vec[i] */
extern void    vec_set_int(vec_ptr vec, int i, int val);
extern void    vec_swap(vec_ptr vec, int i, int j);
extern int     vec_length(vec_ptr vec);

extern void    vec_push(vec_ptr vec, vptr obj);
extern vptr    vec_pop(vec_ptr vec);

extern void    vec_for_each(vec_ptr vec, vec_item_f f);
extern vec_ptr vec_filter(vec_ptr vec, vec_item_p p);
extern void    vec_sort(vec_ptr vec, vec_cmp_f f);
extern bool    vec_is_sorted(vec_ptr vec, vec_cmp_f f);

extern void    vec_sort_range(vec_ptr vec, int start, int stop, vec_cmp_f f);
extern bool    vec_is_sorted_range(vec_ptr vec, int start, int stop, vec_cmp_f f);

extern void    vec_quick_sort(vec_ptr vec, vec_cmp_f f);
extern void    vec_merge_sort(vec_ptr vec, vec_cmp_f f);

extern int     vec_compare(vec_ptr left, vec_ptr right, vec_cmp_f f);

/* XXX This doesn't belong here ... Stats on vec<int> for wizard statistics */
typedef struct {
    double mean;
    double variance;
    double sigma;
    int    max;
} int_stat_t, *int_stat_ptr;
extern int_stat_t int_calc_stats(vec_ptr v);

#endif
