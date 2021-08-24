#include "int_map.h"

#include <assert.h>


typedef struct _node_s _node_t, *_node_ptr;

struct _node_s
{
    hash_t   hash;
    int       key;
    void     *val;
    _node_ptr next;
};

struct int_map_s
{
    _node_ptr     *buckets;
    int            prime_idx;
    int            count;
    int_map_free_f free;
};

struct int_map_iter_s
{
    int_map_ptr map;
    int bucket;
    _node_ptr node;
};

static hash_t _hash(int key)
{
    return (hash_t)key;
}

static void _grow(int_map_ptr map)
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

int_map_ptr int_map_alloc(int_map_free_f free)
{
    int_map_ptr result = malloc(sizeof(int_map_t));
    result->buckets = 0;
    result->count = 0;
    result->prime_idx = 0;
    result->free = free;
    return result;
}

void int_map_free(int_map_ptr map)
{
    if (map)
    {
        int_map_clear(map);
        free(map);
    }
}
void int_map_add_int(int_map_ptr map, int key, int val)
{
    int_map_add(map, key, (void *)(intptr_t)val);
}
void int_map_add(int_map_ptr map, int key, void *val)
{
    hash_t      hash = _hash(key);
    int         prime = hash_tbl_primes[map->prime_idx];
    hash_t      bucket;
    _node_ptr   current;

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
        if (current->hash == hash && key == current->key)
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

bool int_map_delete(int_map_ptr map, int key)
{
    void *val = int_map_detach(map, key);
    if (val && map->free)
        map->free(val);
    return val != 0;
}
void *int_map_detach(int_map_ptr map, int key)
{
    void *result = NULL;
    if (map->count > 0)
    {
        hash_t    hash = _hash(key);
        int       prime = hash_tbl_primes[map->prime_idx];
        hash_t    bucket = hash % prime;
        _node_ptr current = map->buckets[bucket];
        _node_ptr last = 0;

        while (current)
        {
            if (current->hash == hash && current->key == key)
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

int int_map_find_int(int_map_ptr map, int key)
{
    void *pv = int_map_find(map, key);
    return (int)(intptr_t)pv;
}

void *int_map_find(int_map_ptr map, int key)
{
    if (map->count > 0)
    {
        hash_t    hash = _hash(key);
        int       prime = hash_tbl_primes[map->prime_idx];
        hash_t    bucket = hash % prime;
        _node_ptr current = map->buckets[bucket];

        while (current)
        {
            if (current->hash == hash && current->key == key)
                return current->val;
            current = current->next;
        }
    }
    return NULL;
}

bool int_map_contains(int_map_ptr map, int key)
{
    if (map->count > 0)
    {
        hash_t    hash = _hash(key);
        int       prime = hash_tbl_primes[map->prime_idx];
        hash_t    bucket = hash % prime;
        _node_ptr current = map->buckets[bucket];

        while (current)
        {
            if (current->hash == hash && current->key == key)
                return TRUE;
            current = current->next;
        }
    }
    return FALSE;
}

void int_map_clear(int_map_ptr map)
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

int int_map_count(int_map_ptr map)
{
    return map->count;
}

/* Iteration */
void int_map_iter(int_map_ptr map, int_map_iter_f f)
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
vec_ptr int_map_filter(int_map_ptr map, int_map_filter_f f)
{
    vec_ptr v = vec_alloc(NULL);
    int     i;
    int     prime = hash_tbl_primes[map->prime_idx];

    for (i = 0; i < prime; i++)
    {
        _node_ptr current;
        for (current = map->buckets[i]; current; current = current->next)
        {
            if (f(current->key, current->val))
                vec_add(v, current->val);
        }
    }

    return v;
}

int_map_iter_ptr int_map_iter_alloc(int_map_ptr map)
{
    int_map_iter_ptr result = malloc(sizeof(int_map_iter_t));

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

void int_map_iter_free(int_map_iter_ptr iter)
{
    assert(iter);
    free(iter);
}

bool int_map_iter_is_valid(int_map_iter_ptr iter)
{
    assert(iter);
    return iter->node != 0;
}

void * int_map_iter_current(int_map_iter_ptr iter)
{
    assert(int_map_iter_is_valid(iter));
    return iter->node->val;
}

int int_map_iter_current_key(int_map_iter_ptr iter)
{
    assert(int_map_iter_is_valid(iter));
    return iter->node->key;
}

void int_map_iter_next(int_map_iter_ptr iter)
{
    assert(int_map_iter_is_valid(iter));

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
