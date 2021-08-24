/*
 * File: z-virt.h
 * Purpose: Memory management routines
 */

#ifndef INCLUDED_Z_VIRT_H
#define INCLUDED_Z_VIRT_H

/*
 * Replacements for malloc() and friends that die on failure.
 */
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
