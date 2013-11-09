#include "int-map.h"

#include <stdlib.h>
#include <string.h>

typedef unsigned int _hash_t;

typedef struct _node_s _node_t;
typedef _node_t* _node_ptr;

struct _node_s
{
    _hash_t   hash;
    int       key;
    vptr      val;
    _node_ptr next;
};

struct int_map_s
{
    _node_ptr   *buckets;
    int          prime_idx;
    int          count;
    free_value_f free;
};

static int _primes[] = {
    0,
    11,
    23,
    47,
    89,
    181,
    367,
    733,
    1471,
    2953,
    6007,
    12203,
    24509,
    49003,
    98009,
    196501,
    393203,
    786241,
    1572869,
    3145739,
    6291469,
    12582917,
    25165843,
    50331653,
    100663319
};

#define _MAX_PRIME 100663319

static _hash_t _hash(int key)
{
    return (_hash_t)key;
}

static void _grow(int_map_ptr map)
{
    int        i;
    _node_ptr *old_buckets = map->buckets;
    int        old_prime = _primes[map->prime_idx];
    int        new_prime;

    map->prime_idx++;
    new_prime = _primes[map->prime_idx];

    map->buckets = malloc(new_prime * sizeof(_node_ptr));
    memset(map->buckets, 0, new_prime * sizeof(_node_ptr));

    for (i = 0; i < old_prime; i++)
    {
        _node_ptr current = old_buckets[i];
        _node_ptr next;
        _hash_t   bucket;

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

int_map_ptr int_map_alloc(free_value_f free)
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
    int_map_clear(map);
    free(map);
}

void int_map_add(int_map_ptr map, int key, vptr val)
{
    _hash_t      hash = _hash(key);
    int             prime = _primes[map->prime_idx];
    _hash_t      bucket;
    _node_ptr    current;

    if ( !prime 
      || (prime < _MAX_PRIME && map->count > 2*prime/3) ) /* Tweak Me! */
    {
        _grow(map);
        prime = _primes[map->prime_idx];
    }

    bucket = hash % prime;
    current = map->buckets[bucket];

    while (current)
    {
        if ( current->hash == hash
          && key == current->key )
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

int int_map_delete(int_map_ptr map, int key)
{
    if (map->count > 0)
    {
        _hash_t   hash = _hash(key);
        int       prime = _primes[map->prime_idx];
        _hash_t   bucket = hash % prime;
        _node_ptr current = map->buckets[bucket];
        _node_ptr last = 0;

        while (current)
        {
            if ( current->hash == hash
              && current->key == key )
            {
                if (!last)
                    map->buckets[bucket] = current->next;
                else
                    last->next = current->next;
                
                if (map->free)
                    map->free(current->val);
                free(current);
                map->count--;

                return 1;
            }

            last = current;
            current = current->next;
        }
    }
    return 0;
}

vptr int_map_find(int_map_ptr map, int key)
{
    if (map->count > 0)
    {
        _hash_t   hash = _hash(key);
        int       prime = _primes[map->prime_idx];
        _hash_t   bucket = hash % prime;
        _node_ptr current = map->buckets[bucket];

        while (current)
        {
            if ( current->hash == hash
              && current->key == key )
            {
                return current->val;
            }

            current = current->next;
        }
    }
    return 0;
}

int int_map_contains(int_map_ptr map, int key)
{
    if (map->count > 0)
    {
        _hash_t   hash = _hash(key);
        int       prime = _primes[map->prime_idx];
        _hash_t   bucket = hash % prime;
        _node_ptr current = map->buckets[bucket];

        while (current)
        {
            if ( current->hash == hash
              && current->key == key )
            {
                return 1;
            }

            current = current->next;
        }
    }
    return 0;
}

void int_map_clear(int_map_ptr map)
{
    int i;
    int prime = _primes[map->prime_idx];

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