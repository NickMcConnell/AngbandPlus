#include "point_map.h"

#include <assert.h>

typedef struct _node_s _node_t, *_node_ptr;

struct _node_s
{
    hash_t    hash;
    point_t   key;
    void     *val;
    _node_ptr next;
};

struct point_map_s
{
    _node_ptr     *buckets;
    int            prime_idx;
    int            count;
    point_map_free_f free;
};

struct point_map_iter_s
{
    point_map_ptr map;
    int           bucket;
    _node_ptr     node;
};

static hash_t _hash(point_t key) /* XXX review */
{
    hash_t h = key.x;
    h <<= 16;
    h += key.y;
    return h;
}

static void _grow(point_map_ptr map)
{
    int        i;
    _node_ptr *old_buckets = map->buckets;
    int        old_prime = hash_tbl_primes[map->prime_idx];
    int        new_prime;

    map->prime_idx++;
    new_prime = hash_tbl_primes[map->prime_idx];

    map->buckets = malloc(new_prime * sizeof(_node_ptr));
    memset(map->buckets, 0, new_prime * sizeof(_node_ptr));

    for (i = 0; i < old_prime; i++)
    {
        _node_ptr current = old_buckets[i];
        _node_ptr next;
        hash_t    bucket;

        while (current)
        {
            next = current->next;
            
            bucket = current->hash % new_prime;
            current->next = map->buckets[bucket];
            map->buckets[bucket] = current;

            current = next;
        }
    }

    free(old_buckets);
}

point_map_ptr point_map_alloc(point_map_free_f free)
{
    point_map_ptr result = malloc(sizeof(point_map_t));
    result->buckets = 0;
    result->count = 0;
    result->prime_idx = 0;
    result->free = free;
    return result;
}

void point_map_free(point_map_ptr map)
{
    if (map)
    {
        point_map_clear(map);
        free(map);
    }
}

void point_map_add(point_map_ptr map, point_t key, void *val)
{
    hash_t    hash = _hash(key);
    int       prime = hash_tbl_primes[map->prime_idx];
    hash_t    bucket;
    _node_ptr current;

    if ( !prime 
      || (prime < MAX_PRIME && map->count > 2*prime/3) ) /* Tweak Me! */
    {
        _grow(map);
        prime = hash_tbl_primes[map->prime_idx];
    }

    bucket = hash % prime;
    current = map->buckets[bucket];

    while (current)
    {
        if (current->hash == hash && point_equals(key, current->key))
        {
            if (map->free)
                map->free(current->val);
            current->val = val;
            return;
        }
        current = current->next;
    }

    map->count++;

    current = malloc(sizeof(_node_t));
    current->hash = hash;
    current->key = key;
    current->val = val;
    current->next = map->buckets[bucket];
    map->buckets[bucket] = current;
}

void point_map_add_int(point_map_ptr map, point_t key, int val)
{
    point_map_add(map, key, (void *)(intptr_t)val);
}

bool point_map_delete(point_map_ptr map, point_t key)
{
    void *val = point_map_detach(map, key);
    if (val && map->free)
        map->free(val);
    return val != 0;
}
void *point_map_detach(point_map_ptr map, point_t key)
{
    void *result = 0;
    if (map->count > 0)
    {
        hash_t    hash = _hash(key);
        int       prime = hash_tbl_primes[map->prime_idx];
        hash_t    bucket = hash % prime;
        _node_ptr current = map->buckets[bucket];
        _node_ptr last = 0;

        while (current)
        {
            if (current->hash == hash && point_equals(current->key, key))
            {
                if (!last)
                    map->buckets[bucket] = current->next;
                else
                    last->next = current->next;
                
                result = current->val;
                free(current);
                map->count--;
                break;
            }

            last = current;
            current = current->next;
        }
    }
    return result;
}

void *point_map_find(point_map_ptr map, point_t key)
{
    if (map->count > 0)
    {
        hash_t    hash = _hash(key);
        int       prime = hash_tbl_primes[map->prime_idx];
        hash_t    bucket = hash % prime;
        _node_ptr current = map->buckets[bucket];

        while (current)
        {
            if (current->hash == hash && point_equals(current->key, key))
                return current->val;
            current = current->next;
        }
    }
    return NULL;
}

int point_map_find_int(point_map_ptr map, point_t key)
{
    void *pv = point_map_find(map, key);
    return (int)(intptr_t)pv;
}

bool point_map_contains(point_map_ptr map, point_t key)
{
    if (map->count > 0)
    {
        hash_t    hash = _hash(key);
        int       prime = hash_tbl_primes[map->prime_idx];
        hash_t    bucket = hash % prime;
        _node_ptr current = map->buckets[bucket];

        while (current)
        {
            if (current->hash == hash && point_equals(current->key, key))
                return TRUE;
            current = current->next;
        }
    }
    return FALSE;
}

void point_map_clear(point_map_ptr map)
{
    int i;
    int prime = hash_tbl_primes[map->prime_idx];

    for (i = 0; i < prime; i++)
    {
        _node_ptr current = map->buckets[i];
        _node_ptr next;

        while (current)
        {
            next = current->next;
            if (map->free)
                map->free(current->val);
            free(current);
            current = next;
        }        
    }
    free(map->buckets);

    map->buckets = 0;
    map->count = 0;
    map->prime_idx = 0;
}

int point_map_count(point_map_ptr map)
{
    return map->count;
}

/* Iteration */
void point_map_iter(point_map_ptr map, point_map_iter_f f)
{
    int i;
    int prime = hash_tbl_primes[map->prime_idx];

    for (i = 0; i < prime; i++)
    {
        _node_ptr current;
        for (current = map->buckets[i]; current; current = current->next)
            f(current->key, current->val);
    }
}
void point_map_iter_int(point_map_ptr map, point_map_iter_int_f f)
{
    int i;
    int prime = hash_tbl_primes[map->prime_idx];

    for (i = 0; i < prime; i++)
    {
        _node_ptr current;
        for (current = map->buckets[i]; current; current = current->next)
        {
            int val = (int)(intptr_t)current->val;
            f(current->key, val);
        }
    }
}

point_map_iter_ptr point_map_iter_alloc(point_map_ptr map)
{
    point_map_iter_ptr result = malloc(sizeof(point_map_iter_t));

    assert(map);

    result->map = map;
    result->bucket = 0;
    result->node = 0;

    if (map->count)
    {
        int prime = hash_tbl_primes[map->prime_idx];
        int i;

        for (i = 0; i < prime; i++)
        {
            result->bucket = i;
            result->node = map->buckets[i];
            if (result->node)
                break;
        }
    }
    return result;
}

void point_map_iter_free(point_map_iter_ptr iter)
{
    assert(iter);
    free(iter);
}

bool point_map_iter_is_valid(point_map_iter_ptr iter)
{
    assert(iter);
    return iter->node != 0;
}

void *point_map_iter_current(point_map_iter_ptr iter)
{
    assert(point_map_iter_is_valid(iter));
    return iter->node->val;
}

point_t point_map_iter_current_key(point_map_iter_ptr iter)
{
    assert(point_map_iter_is_valid(iter));
    return iter->node->key;
}

void point_map_iter_next(point_map_iter_ptr iter)
{
    assert(point_map_iter_is_valid(iter));

    iter->node = iter->node->next;
    if (!iter->node)
    {
        int prime = hash_tbl_primes[iter->map->prime_idx];
        int i;

        for (i = iter->bucket + 1; i < prime; i++)
        {
            iter->bucket = i;
            iter->node = iter->map->buckets[i];
            if (iter->node)
                break;
        }
    }
}
