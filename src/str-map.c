#include "str-map.h"

#include <stdlib.h>
#include <string.h>
#include <assert.h>

typedef unsigned int _hash_t;

typedef struct _node_s _node_t;
typedef _node_t* _node_ptr;

struct _node_s
{
    _hash_t   hash;
    char     *key;
    void     *val;
    _node_ptr next;
};

struct str_map_s
{
    _node_ptr     *buckets;
    int            prime_idx;
    int            count;
    str_map_free_f free;
};

struct str_map_iter_s
{
    str_map_ptr map;
    int bucket;
    _node_ptr node;
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

_hash_t _hash(const char *str) /* djb2 hash algorithm */
{
    _hash_t hash = 5381;
    int c;

    while ((c = *str++))
        hash = ((hash << 5) + hash) + c; /* hash * 33 + c */

    return hash;
}

static void _grow(str_map_ptr map)
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

str_map_ptr str_map_alloc(str_map_free_f free)
{
    str_map_ptr result = malloc(sizeof(str_map_t));
    result->buckets = 0;
    result->count = 0;
    result->prime_idx = 0;
    result->free = free;
    return result;
}

void str_map_free(str_map_ptr map)
{
    str_map_clear(map);
    free(map);
}

void str_map_add(str_map_ptr map, const char *key, void *val)
{
    _hash_t      hash = _hash(key);
    int          prime = _primes[map->prime_idx];
    _hash_t      bucket;
    _node_ptr    current;
    int          len;

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
          && strcmp(key, current->key) == 0 )
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
    
    len = strlen(key);
    current->key = malloc(len + 1);
    strcpy(current->key, key);

    current->val = val;
    current->next = map->buckets[bucket];
    map->buckets[bucket] = current;
}

int str_map_delete(str_map_ptr map, const char *key)
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
              && strcmp(current->key, key) == 0 )
            {
                if (!last)
                    map->buckets[bucket] = current->next;
                else
                    last->next = current->next;
                
                if (map->free)
                    map->free(current->val);
                free(current->key);
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

void * str_map_find(str_map_ptr map, const char *key)
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
              && strcmp(current->key, key) == 0 )
            {
                return current->val;
            }

            current = current->next;
        }
    }
    return 0;
}

int str_map_contains(str_map_ptr map, const char *key)
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
              && strcmp(current->key, key) == 0 )
            {
                return 1;
            }

            current = current->next;
        }
    }
    return 0;
}

void str_map_clear(str_map_ptr map)
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
            free(current->key);
            free(current);
            current = next;
        }        
    }
    free(map->buckets);

    map->buckets = 0;
    map->count = 0;
    map->prime_idx = 0;
}

int str_map_count(str_map_ptr map)
{
    return map->count;
}

/* Iteration */
str_map_iter_ptr str_map_iter_alloc(str_map_ptr map)
{
    str_map_iter_ptr result = malloc(sizeof(str_map_iter_t));

    assert(map);

    result->map = map;
    result->bucket = 0;
    result->node = 0;

    if (map->count)
    {
        int prime = _primes[map->prime_idx];
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

void str_map_iter_free(str_map_iter_ptr iter)
{
    assert(iter);
    free(iter);
}

int str_map_iter_is_valid(str_map_iter_ptr iter)
{
    assert(iter);
    return iter->node != 0;
}

void * str_map_iter_current(str_map_iter_ptr iter)
{
    assert(str_map_iter_is_valid(iter));
    return iter->node->val;
}

const char * str_map_iter_current_key(str_map_iter_ptr iter)
{
    assert(str_map_iter_is_valid(iter));
    return iter->node->key;
}

void str_map_iter_next(str_map_iter_ptr iter)
{
    assert(str_map_iter_is_valid(iter));

    iter->node = iter->node->next;
    if (!iter->node)
    {
        int prime = _primes[iter->map->prime_idx];
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
