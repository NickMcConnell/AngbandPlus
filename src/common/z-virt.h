/*
 * File: z-virt.h
 * Purpose: Memory management routines
 */

#ifndef INCLUDED_Z_VIRT_H
#define INCLUDED_Z_VIRT_H

/* Wipe an array of type T[N], at location P, and return P */
#define C_WIPE(P, N, T) \
    (memset((P), 0, (N) * sizeof(T)))

/* Wipe a thing of type T, at location P, and return P */
#define WIPE(P, T) \
    (memset((P), 0, sizeof(T)))

/* Load an array of type T[N], at location P1, from another, at location P2 */
#define C_COPY(P1, P2, N, T) \
    (memcpy((P1), (P2), (N) * sizeof(T)))

/* Load a thing of type T, at location P1, from another, at location P2 */
#define COPY(P1, P2, T) \
    (memcpy((P1), (P2), sizeof(T)))

/* Allocate, and return, an array of type T[N] */
#define C_RNEW(N, T) \
    (T*)(mem_alloc((N) * sizeof(T)))

/* Allocate, and return, a thing of type T */
#define RNEW(T) \
    (T*)(mem_alloc(sizeof(T)))

/* Allocate, wipe, and return an array of type T[N] */
#define C_ZNEW(N, T) \
    (T*)(C_WIPE(C_RNEW(N, T), N, T))

/* Allocate, wipe, and return a thing of type T */
#define ZNEW(T) \
    (T*)(WIPE(RNEW(T), T))

/* Replacements for malloc() and friends that die on failure. */
extern void* mem_alloc(size_t len);
extern void* mem_zalloc(size_t len);
extern void mem_free(void *p);
extern void *mem_realloc(void *p, size_t len);

extern char *string_make(const char *str);
extern void string_free(char *str);
extern char *string_append(char *s1, const char *s2);

/* Free an array of length "len" */
extern void mem_nfree(void **p, size_t len);

/* Free a bidimentional array of length "len" with its variable lengths "plen" */
extern void strings_free(const char ***p, u32b *plen, size_t len);

enum
{
    MEM_POISON_ALLOC = 0x00000001,
    MEM_POISON_FREE  = 0x00000002
};

extern unsigned int mem_flags;

#endif
