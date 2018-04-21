#include "c-vec.h"

#include <assert.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

struct vec_s
{
    vptr      *objs;
    int        size;
    int        len;
    vec_free_f free;
};

/* For vectors, it is common to want a vec<int> and the speed difference
   between vec<int> and vec<int*> can be significant (as much as 3x). Since
   time immemorial, it has been the practice to stuff ints into pointer fields
   so that a vec<void*> could be a vec<int> or a vec<my_type*> as needed (and
   only coded once!).

   However, gcc whines about converting int <-> void* on 64 bit architectures,
   and as far as I could figure out, the only way to shut up this warning is to
   convert intptr_t <-> void*. This works just fine on Windows, btw.

   All we really need for the correctness of this long standing, traditional
   idiom is sizeof(int) <= sizeof(void*), and I really wish gcc would lighten
   up a bit ;)
*/

int vec_compare_int(const void *left, const void *right)
{
    intptr_t l = (intptr_t)left;
    intptr_t r = (intptr_t)right;
    if (l < r) return -1;
    if (r < l) return 1;
    return 0;
}

static void _grow(vec_ptr vec, int size)
{
    if (size > vec->size)
    {
        vptr   *old_objs = vec->objs;
        int     i;

        vec->size = vec->size * 2;
        if (vec->size < size)
            vec->size = size;

        vec->objs = malloc(vec->size * sizeof(vptr));
        memset(vec->objs, 0, vec->size * sizeof(vptr)); /* unnecessary */

        for (i = 0; i < vec->len; i++)
            vec->objs[i] = old_objs[i];

        free(old_objs);
    }
}

vec_ptr vec_alloc(vec_free_f free)
{
    vec_ptr result = malloc(sizeof(vec_t));
    result->objs = 0;
    result->size = 0;
    result->len = 0;
    result->free = free;
    return result;
}

void vec_free(vec_ptr vec)
{
    if (vec)
    {
        vec_clear(vec);
        free(vec->objs);
        free(vec);
    }
}

void vec_add(vec_ptr vec, vptr obj)
{
    int i = vec->len;
    
    if (i >= vec->size)
        _grow(vec, i + 1);

    vec->len++;
    vec->objs[i] = obj;
}

void vec_add_int(vec_ptr vec, int val)
{
    vec_add(vec, (vptr)(intptr_t)val);
}

void vec_clear(vec_ptr vec)
{
    if (vec->len)
    {
        if (vec->free)
        {
            int i;
            for (i = 0; i < vec->len; i++)
                vec->free(vec->objs[i]);
        }
        memset(vec->objs, 0, vec->size * sizeof(vptr));
        vec->len = 0;
    }
}

vptr vec_get(vec_ptr vec, int i)
{
    vptr res = 0;
    assert(i >= 0 && i < vec->len);
    if (i < vec->len)
    {
        assert(vec->objs);
        res = vec->objs[i];
    }
    return res;
}

int vec_get_int(vec_ptr vec, int i)
{
    vptr pv = vec_get(vec, i);
    return (int)(intptr_t)pv;
}

void vec_set(vec_ptr vec, int i, vptr obj)
{
    assert(i >= 0 && i < vec->len);
    if (i < vec->len)
    {
        assert(vec->objs);
        if (vec->free)
            vec->free(vec->objs[i]);
        vec->objs[i] = obj;
    }
}

void vec_set_int(vec_ptr vec, int i, int val)
{
    vec_set(vec, i, (vptr)(intptr_t)val);
}

void vec_swap(vec_ptr vec, int i, int j)
{
    vptr tmp;
    assert(i >= 0 && i < vec->len);
    assert(j >= 0 && j < vec->len);
    tmp = vec->objs[i];
    vec->objs[i] = vec->objs[j];
    vec->objs[j] = tmp;
}

int vec_length(vec_ptr vec)
{
    return vec->len;
}

void vec_push(vec_ptr vec, vptr obj)
{
    vec_add(vec, obj);
}

vptr vec_pop(vec_ptr vec)
{
    vptr result = NULL;
    assert(vec->len);
    if (vec->len)
    {
        vec->len--;
        result = vec->objs[vec->len];
    }
    return result;
}

void vec_for_each(vec_ptr vec, vec_item_f f)
{
    int i;
    for (i = 0; i < vec->len; i++)
        f(vec->objs[i]);
}

int vec_compare(vec_ptr left, vec_ptr right, vec_cmp_f f)
{
    int i;
    int cl = left->len;
    int cr = right->len;
    for (i = 0; i < cl && i < cr; i++)
    {
        vptr l = left->objs[i];
        vptr r = right->objs[i];
        int  n = f(l, r);

        if (n != 0)
            return n;
    }

    if (i < cr)
    {
        assert(i >= cl);
        return -1;
    }
    if (i < cl)
    {
        assert(i >= cr);
        return 1;
    }
    return 0;
}

static void _insertion_sort(vptr vec[], int left, int right, vec_cmp_f f)
{
    int j;
    for (j = left + 1; j <= right; j++)
    {
        vptr key = vec[j];
        int  i = j - 1;
        while (i >= left && f(vec[i], key) > 0)
        {
            vec[i + 1] = vec[i];
            i--;
        }
        vec[i + 1] = key;
    }
}

static void _swap(vptr vec[], int i, int j)
{
    vptr t = vec[i];
    vec[i] = vec[j];
    vec[j] = t;
}

static int _median3_partition(vptr vec[], int left, int right, vec_cmp_f f)
{
    int center = (left + right) / 2;

    assert(right - left + 1 >= 3);

    /* sort <v[l], v[c], v[r]> in place */
    if (f(vec[left], vec[center]) > 0)
        _swap(vec, left, center);
    if (f(vec[center], vec[right]) > 0)
    {
        _swap(vec, center, right);
        if (f(vec[left], vec[center]) > 0)
            _swap(vec, left, center);
    }

    /* v[c] is the median, put it in v[r-1] */
    _swap(vec, center, right - 1);

    /* Note: Median of 3 partioning allows us to remove the bounds checking
       in the while loops below since we know:
       v[l] <= v[r-1] <= v[r]

       i will run from l+1 up, but will stop on r-1 at the worst since
       v[r-1] = v[r-1]. IOW, the pivot always guards sliding from the left

       j will run from r-2 down, but will stop on l at the worst since
       v[l] <= v[r-1]

       Otherwise, this is Sedgewick's partition routine, which "burns the
       candle at both ends". You can see Knuth for a good discussion, but
       the tricky part is coding without sentinels.

       Note: This partition is much better than the "leftwall" approach when
       large numbers of duplicate keys are present. Just mentally think what
       happens when A[l..r] contains a single key. (See Cormen for this
       partition version).

       Finally, Median of Three is a must in case the data is already sorted.*/
    {
        vptr pivot = vec[right - 1];
        int  i = left + 1;
        int  j = right - 2;
        while (1)
        {
            while (f(vec[i], pivot) < 0) i++;
            while (f(vec[j], pivot) > 0) j--;

            if (i < j)
                _swap(vec, i, j);
            else
                break;

            i++;
            j--;
        }
        _swap(vec, i, right - 1);

        return i;
    }
}

static void _quick_sort(vptr vec[], int left, int right, vec_cmp_f f)
{
    if (right - left + 1 < 20)
        _insertion_sort(vec, left, right, f);
    else
    {
        int i = _median3_partition(vec, left, right, f);
        _quick_sort(vec, left, i - 1, f);
        _quick_sort(vec, i + 1, right, f);
    }
}

static void _merge(vptr vec[], int p, int q, int r, vec_cmp_f f)
{
    int n1 = q - p + 1;
    int n2 = r - q;
    vptr *ls = malloc(n1 * sizeof(vptr));
    vptr *rs = malloc(n2 * sizeof(vptr));
    int i1 = 0, i2 = 0, i;

    for (i1 = 0; i1 < n1; i1++)
        ls[i1] = vec[p + i1];
    for (i2 = 0; i2 < n2; i2++)
        rs[i2] = vec[q + 1 + i2];

    i1 = 0; i2 = 0;
    for (i = p; i <= r; i++)
    {
        vptr l = i1 < n1 ? ls[i1] : 0;
        vptr r = i2 < n2 ? rs[i2] : 0;
        vptr c;

        if (!l) { c = r; i2++; }
        else if (!r) { c = l; i1++; }
        else if (f(l, r) <= 0) { c = l; i1++; }
        else { c = r; i2++; }

        assert (c);
        vec[i] = c;
    }

    free(ls);
    free(rs);
}

static void _merge_sort(vptr vec[], int left, int right, vec_cmp_f f)
{
    if (left < right)
    {
        int middle = (left + right) / 2;
        _merge_sort(vec, left, middle, f);
        _merge_sort(vec, middle + 1, right, f);
        _merge(vec, left, middle, right, f);
    }
}

bool vec_is_sorted(vec_ptr vec, vec_cmp_f f)
{
    return vec_is_sorted_range(vec, 0, vec->len - 1, f);
}

bool vec_is_sorted_range(vec_ptr vec, int start, int stop, vec_cmp_f f)
{
    int i;
    for (i = start; i < stop; i++)
    {
        if (f(vec->objs[i], vec->objs[i+1]) > 0)
            return FALSE;
    }
    return TRUE;
}

void vec_quick_sort(vec_ptr vec, vec_cmp_f f)
{
    _quick_sort(vec->objs, 0, vec->len - 1, f);
    assert(vec_is_sorted(vec, f));
}

void vec_sort_range(vec_ptr vec, int start, int stop, vec_cmp_f f)
{
    assert(0 <= start && start < vec->len);
    assert(0 <= stop && stop < vec->len);
    assert(start <= stop);
    _quick_sort(vec->objs, start, stop, f);
    assert(vec_is_sorted_range(vec, start, stop, f));
}

void vec_merge_sort(vec_ptr vec, vec_cmp_f f)
{
    _merge_sort(vec->objs, 0, vec->len - 1, f);
    assert(vec_is_sorted(vec, f));
}

void vec_sort(vec_ptr vec, vec_cmp_f f)
{
    vec_quick_sort(vec, f);
}
