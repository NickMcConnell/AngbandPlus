#include "angband.h" /* for sym_wizard */
#include "int_map.h" /* for sym_wizard */
/*#include "z-sym.h"

#include "c-vec.h" */
#include "map.h"
#include <assert.h>

/************************************************************************
 * Custom Hash Table
 * Unlike str_map, we need not copy and free keys. They are already
 * allocated in symbol pages. This halves our memory usage, so is
 * worth the effort.
 ************************************************************************/
typedef struct _node_s _node_t, *_node_ptr;
struct _node_s
{
    hash_t hash;
    sym_t  sym;
    cptr   key;      
    _node_ptr next;
}; /* packed for 24 bytes on x86_64 (hash,key,sym,next is 32b) */

typedef struct {
    _node_ptr *buckets;
    int prime_idx;
    int count;
    int grow;
} _map_t, *_map_ptr;

/* XXX temp analysis code */
static doc_ptr _doc = NULL;
static bool _doc_pct = FALSE;
static void _map_doc(_map_ptr map, doc_ptr doc);

static hash_t _hash(cptr str) /* djb2 hash algorithm */
{
    hash_t hash = 5381;
    int c;

    while ((c = *str++))
        hash = ((hash << 5) + hash) + c;

    return hash;
}
/* sample distribution of mon_race->name
Histogram: 1097 (P=2953 for 37% load)
 0: 2047
 1: 737
 2: 150
 3: 16
 4: 3 
Note P=1471 gives 75% load. For yuks, here is that distribution:
Histogram: 1097 (P=1471)
 0: 696
 1: 522
 2: 198
 3: 44
 4: 9
 5: 1
 6: 1
 cf _map_add which grows at 66% load
 P=2953 wastes 11.6kB */
static _map_ptr _map_alloc(void)
{
    _map_ptr result = malloc(sizeof(_map_t));
    result->buckets = 0;
    result->count = 0;
    result->prime_idx = 0;
    result->grow = 100;
    return result;
}
static void _map_clear(_map_ptr map)
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
            free(current);
            current = next;
        }        
    }
    free(map->buckets);

    map->buckets = 0;
    map->count = 0;
    map->prime_idx = 0;
}
static void _map_free(_map_ptr map)
{
    if (map)
    {
        _map_clear(map);
        free(map);
    }
}
static sym_t _map_find(_map_ptr map, cptr key)
{
    if (map->count > 0)
    {
        hash_t    hash = _hash(key);
        int       prime = hash_tbl_primes[map->prime_idx];
        hash_t    bucket = hash % prime;
        _node_ptr current = map->buckets[bucket];

        while (current)
        {
            if (current->hash == hash && strcmp(current->key, key) == 0)
                return current->sym;
            current = current->next;
        }
    }
    return 0;
}
static void _map_grow(_map_ptr map)
{
    int        i;
    _node_ptr *old_buckets = map->buckets;
    int        old_prime = hash_tbl_primes[map->prime_idx];
    int        new_prime;

    if (_doc && map->count)
    {
        doc_printf(_doc, "<color:D>%5d", map->count);
        _map_doc(map, _doc);
        doc_insert(_doc, "</color>\n");
    }
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
    if (_doc && map->count)
    {
        doc_printf(_doc, "%5d", map->count);
        _map_doc(map, _doc);
        doc_newline(_doc);
    }
}
static void _map_add(_map_ptr map, cptr key, sym_t sym)
{
    hash_t    hash = _hash(key);
    int       prime = hash_tbl_primes[map->prime_idx];
    hash_t    bucket;
    _node_ptr current;

    if ( !prime 
      || (prime < MAX_PRIME && map->count*100/prime > map->grow) )
    {
        _map_grow(map);
        prime = hash_tbl_primes[map->prime_idx];
    }

    bucket = hash % prime;
    current = map->buckets[bucket];

    while (current)
    {
        if (current->hash == hash && strcmp(key, current->key) == 0)
        {
            current->sym = sym;
            return;
        }
        current = current->next;
    }

    map->count++;

    current = malloc(sizeof(_node_t));
    current->hash = hash;
    current->key = key;
    current->sym = sym;
    current->next = map->buckets[bucket];
    map->buckets[bucket] = current;
}

static double _map_efficiency(_map_ptr map, int_map_ptr h, int max)
{
    double e = 0.;
    int i;
    if (!map->count) return e;
    for (i = 1; i <= max; i++)
    {
        int ct = int_map_find_int(h, i);
        /* ct*i elements have chain length i */
        e += (double)ct * i * (i + 1) / 2;
    }
    return e / map->count;
}
static void _map_doc(_map_ptr map, doc_ptr doc)
{
    int_map_ptr h = int_map_alloc(NULL); /* histogram */
    int i, max = 0, cb, tot = 0, unused = 0;
    int p = hash_tbl_primes[map->prime_idx];
    for (i = 0; i < p; i++)
    {
        _node_ptr n;
        int ct = 0;
        for (n = map->buckets[i]; n; n = n->next, ct++)
        {
        }
        if (ct > max) max = ct;
        int_map_add_int(h, ct, int_map_find_int(h, ct) + 1);
    }
    cb = map->count * sizeof(_node_t) + p * sizeof(_node_ptr) + sizeof(_map_t); 
    doc_printf(doc, " %5d %.2f %3d.%1dK", p, _map_efficiency(map, h, max), cb/1024, (cb*10/1024)%10);
    for (i = 0; i <= max; i++)
    {
        int ct = int_map_find_int(h, i);
        tot += ct * i;
        if (_doc_pct)
        {
            if (i == 0)
            {
                unused = ct;
                doc_printf(doc, " .%03d", ct*1000/p);
            }
            else if (tot < map->count)
                doc_printf(doc, " .%03d", tot*1000/map->count);
            else if (!unused)
                doc_insert(doc, " 1.00");
            else
                doc_printf(doc, " .%03d", (p - unused)*1000/p);
        }
        else
            doc_printf(doc, " %4d", ct);
    }
    int_map_free(h);
}

/************************************************************************
 * Symbols
 ************************************************************************/
#define _SYM_PAGE_SIZE (5*1024)

static vec_ptr _pages;
static vec_ptr _tops;
static _map_ptr _symbols;
static int _page;
static int _top;
static int _next;

static void _grow(void)
{
    char *page = malloc(_SYM_PAGE_SIZE);
    vec_add(_pages, page);
    _page++;
    assert(_page == vec_length(_pages) - 1);
    _top = 0;
}
sym_t sym_add(cptr str)
{
    sym_t sym;
    int   cb, i;
    char *page;
    cptr  key;

    /* verify sym_init() called */
    assert(_symbols);
    assert(_pages);
    assert(_page == vec_length(_pages) - 1);

    /* existing symbol? */
    sym = _map_find(_symbols, str);
    if (sym) return sym;

    /* room on current page for new string? */
    cb = strlen(str);
    if (!cb) return 0; /* "" is sym_null (0) */

    assert(cb <= _SYM_PAGE_SIZE); /* symbols should be short ... say 30 max */
    if (_top + cb + 1 > _SYM_PAGE_SIZE) _grow();

    /* reserve new symbol */
    sym = _next++;
    assert(sym == _next - 1); /* sym_t u16b vs. int _next */
    vec_add_int(_tops, _page*_SYM_PAGE_SIZE + _top);
    assert(vec_length(_tops) == _next);

    /* copy str to _pages[_page][_top] and remember symbol for this str */
    page = vec_get(_pages, _page);
    key = &page[_top]; /* remember location of key */
    for (i = 0; i < cb; i++)
        page[_top++] = str[i];
    page[_top++] = '\0';
    _map_add(_symbols, key, sym); /* *after* copying key for _hash(key) */

    assert(_top <= _SYM_PAGE_SIZE);
    assert(vec_length(_tops) == _symbols->count + 1); /* +1 is for sym_null */

    return sym;
}
sym_t sym_find(cptr str)
{
    return _map_find(_symbols, str);
}
bool sym_equals(sym_t left, cptr right)
{
    if (!left) return FALSE;
    return left == sym_find(right);
}
cptr sym_str(sym_t sym)
{
    int p, o, top;
    char *page;
    cptr str;

    if (!sym) return ""; /* paranoia */

    assert(0 < sym && sym < _next);
    top = vec_get_int(_tops, sym);
    p = top / _SYM_PAGE_SIZE;
    o = top % _SYM_PAGE_SIZE; 
    assert(p <= _page);
    assert(!(p == _page && o >= _top));

    page = vec_get(_pages, p);
    str = page + o;

    assert(_map_find(_symbols, str) == sym);

    return str;
}
void sym_startup(void)
{
    char *page;

    assert(sizeof(char) == 1);

    _pages = vec_alloc(free);
    _symbols = _map_alloc();
    _tops = vec_alloc(NULL);

    page = malloc(_SYM_PAGE_SIZE);
    page[0] = '\0'; /* sym_t of 0 is reserved (#null) */
    vec_add(_pages, page);
    vec_add_int(_tops, 0);

    _page = 0;
    _top = 1;
    _next = 1;
}
void sym_shutdown(void)
{
    vec_free(_tops);
    _map_free(_symbols);
    vec_free(_pages);

    _pages = NULL;
    _symbols = NULL;
    _top = _page = _next = 0;
}
#ifdef DEVELOPER
void sym_doc(doc_ptr doc)
{
    int_map_ptr h = int_map_alloc(NULL); /* histogram */
    int i, max = 0, cb;
    int p = hash_tbl_primes[_symbols->prime_idx];
    for (i = 0; i < p; i++)
    {
        _node_ptr n;
        int ct = 0;
        /* doc_printf(doc, "%d: ", i); */
        for (n = _symbols->buckets[i]; n; n = n->next, ct++)
        {
        }
        if (ct > max) max = ct;
        /* doc_printf(doc, "%d\n", ct); */
        int_map_add_int(h, ct, int_map_find_int(h, ct) + 1);
    }
    doc_printf(doc, "<color:R>Symbols  :</color> %d\n", _symbols->count);
    doc_printf(doc, "<color:R>Pages    :</color> %d\n", vec_length(_pages));
    doc_printf(doc, "<color:R>PageSize :</color> %d\n", _SYM_PAGE_SIZE);
    doc_printf(doc, "<color:R>Current  :</color> _page=%d, _top=%d\n", _page, _top);

    cb = _page*_SYM_PAGE_SIZE + _top;
    doc_printf(doc, "<color:R>Used     :</color> %d.%02dkB <color:D>%dB</color>\n",
        cb/1024, (cb*100/1024)%100, cb);

    cb = vec_length(_pages) * _SYM_PAGE_SIZE;
    doc_printf(doc, "<color:R>Allocated:</color> %d.%02dkB <color:D>%dB</color>\n",
        cb/1024, (cb*100/1024)%100, cb);

    /* x86_64:             24                    8                                  */
    cb = _symbols->count * sizeof(_node_t) + p * sizeof(_node_ptr) + sizeof(_map_t); 
    doc_printf(doc, "<color:R>Overhead :</color> %d.%02dkB <color:D>%dB</color>\n",
        cb/1024, (cb*100/1024)%100, cb);

    doc_printf(doc, "\n<color:R>Key Distribution (<color:U>P=%d</color>,"
        "<color:U>L=%d%%</color>,<color:U>G=%d%%</color>)</color>\n",
        p, _symbols->count * 100 / MAX(1, p), _symbols->grow);
    for (i = 0; i <= max; i++)
    {
        int ct = int_map_find_int(h, i);
        doc_printf(doc, " %d: %d\n", i, ct);
    }
    int_map_free(h);
}
static void _hash_tbl_load(doc_ptr doc, int grow)
{
    _map_ptr map = _map_alloc();
    int      i;

    _doc = doc;
    map->grow = grow;

    doc_printf(doc, "\n\n<color:o>Hash Table Trace (<color:U>G=%d%%</color>)</color>\n", map->grow);
    doc_printf(doc, "<color:U>%5.5s %5.5s %4.4s %6.6s", "Count", "P", "E", "Space");
    for (i = 0; i < 15; i++)
        doc_printf(doc, " %4d", i);
    doc_insert(doc, "</color>\n");

    for (i = 1; i < _next; i++)
    {
        cptr s = sym_str(i);
        _map_add(map, s, i);
    }

    doc_printf(doc, "<color:G>%5d", map->count);
    _map_doc(map, doc);
    doc_insert(doc, "</color>\n");

    _doc = NULL;
    _map_free(map);
}
static void _hash_tbl_loads(doc_ptr doc)
{
    int thresholds[] = { 65, 80, 100, 120, 140, 160, 200, 300, 500, 0 };
    int i;
    _doc_pct = TRUE;
    for (i = 0; ; i++)
    {
        int grow = thresholds[i];
        if (!grow) break;
        _hash_tbl_load(doc, grow);
    }

    doc_printf(doc, "\n<style:normal><color:B>Note:</color> <indent>"
        "The <color:U>E</color> column gives the hash table efficiency, and is "
        "the average number of key comparisons to lookup each key in the "
        "table. These numbers should always be larger than 1, but the closer "
        "to 1 the better, obviously.</indent></style>\n");

    doc_printf(doc, "\n<style:normal><color:B>Note:</color> <indent>"
        "The <color:U>Space</color> column gives the amount of memory used by the hash "
        "table. More efficient hash tables generally use more space since they "
        "possess more un-occupied buckets.</indent></style>\n");

    if (_doc_pct)
    {
        doc_printf(doc, "\n<style:normal><color:B>Note:</color> <indent>"
          "To read the hash table traces, note that the <color:U>0</color> column shows the "
          "fraction of empty buckets. Multiply this number by %d times the current "
          "prime to figure the amount of wasted space. The remaining columns after "
          "<color:U>0</color> show the cummulative fraction of elements in buckets whose chain "
          "length is less than or equal to the column heading. Use this value to "
          "figure the efficiency of the hash table. The last column is an exception, "
          "however, since its cummulative fraction would always be 1. Instead, it shows "
          "the fraction of occupied buckets. Add this number to the number in the <color:U>0</color> "
          "column and you should get 1.</indent></style>\n",
          sizeof(void*));
    }
}
void sym_wizard(void)
{
    doc_ptr doc = doc_alloc(120);
    doc_insert(doc, "<style:screenshot>");
    sym_doc(doc);
    _hash_tbl_loads(doc);
    doc_insert(doc, "</style>");
    screen_save();
    doc_display(doc, "Symbols", 0);
    screen_load();
    doc_free(doc);
}
void sym_test(void)
{
    /* XXX */
    sym_wizard();
}
/* mon_race->name on x86_64:
Symbols  : 1097
Pages    : 3
PageSize : 8192
Current  : _page=2, _top=728
Used     : 16.71kB 17112B         <= 17107B actual, each _grow wastes space on each page
Allocated: 24.00kB 24576B
Overhead : 48.79kB 49968B         <= 29376B on x86_32 ... 8byte pointers cost ~20kB
                                  <= 11.6kB wasted using P=2953 vs P=1471 (see above)
Key Distribution
 0: 2047                          <= 16kB wasted on NULL pointers
 1: 737
 2: 150
 3: 16
 4: 3

Here are smaller page sizes:
Symbols  : 1097
Pages    : 35
PageSize : 512
Current  : _page=34, _top=12
Used     : 17.01kB 17420B        <= 17107B actual; note increased wastage vs 8k page size
Allocated: 17.50kB 17920B        <= note decreased vs 8k page size
Overhead : 48.79kB 49968B

Symbols  : 1097
Pages    : 17
PageSize : 1024
Current  : _page=16, _top=883
Used     : 16.86kB 17267B
Allocated: 17.00kB 17408B
Overhead : 48.79kB 49968B

Overhead does not include size of _pages: roughly 8*_pages->allocated (at most 2x _pages->count)
*/
#else
void sym_wizard(void) {}
void sym_test(void) {}
#endif
