/*
 * File: z-util.h
 * Purpose: Low level utilities
 */

#ifndef INCLUDED_Z_UTIL_H
#define INCLUDED_Z_UTIL_H


/*
 * Available variables
 */

/* Temporary Vars */
extern char char_tmp;
extern byte byte_tmp;
extern sint sint_tmp;
extern uint uint_tmp;
extern long long_tmp;
extern errr errr_tmp;

/* Temporary Pointers */
extern const char *cptr_tmp;
extern void *vptr_tmp;

/* Constant pointers (NULL) */
extern const char *cptr_null;
extern void *vptr_null;

/* A bizarre void * that always points at itself */
extern void *vptr_self;

/* The name of the program */
extern char *argv0;

/*
 * Aux functions
 */
extern void (*plog_aux)(const char *);
extern void (*quit_aux)(const char *);
extern void (*assert_aux)(void);


/*
 * Available Functions
 */

/*
 * Return "s" (or not) depending on whether n is singular.
 */
#define PLURAL(n) (((n) == 1)? "": "s")
#define SINGULAR(n) (((n) == 1)? "s": "")

/*
 * Return the verb form matching the given count
 */
#define VERB_AGREEMENT(count, singular, plural) (((count) == 1)? (singular): (plural))

/* Function that does nothing */
extern void func_nothing(void);

/* Functions that return basic "errr" codes */
extern errr func_success(void);
extern errr func_problem(void);
extern errr func_failure(void);

/* Functions that return bools */
extern bool func_true(void);
extern bool func_false(void);

/* Count the number of characters in a UTF-8 encoded string */
extern size_t utf8_strlen(char *s);

/* Clip a null-terminated UTF-8 string 's' to 'n' unicode characters. */
extern void utf8_clipto(char *s, size_t n);

/* Case insensitive comparison between two strings */  
extern int my_stricmp(const char *s1, const char *s2);

/* Case insensitive comparison between two strings, up to n characters long. */
extern int my_strnicmp(const char *a, const char *b, int n);

/* Case-insensitive strstr */
extern char *my_stristr(const char *haystack, const char *needle);

/*
 * Copy up to 'bufsize'-1 characters from 'src' to 'buf' and NULL-terminate
 * the result.  The 'buf' and 'src' strings may not overlap.
 *
 * Returns: strlen(src).  This makes checking for truncation
 * easy.  Example:
 *   if (my_strcpy(buf, src, sizeof(buf)) >= sizeof(buf)) ...;
 *
 * This function should be equivalent to the strlcpy() function in BSD.
 */
extern size_t my_strcpy(char *buf, const char *src, size_t bufsize);

/*
 * Try to append a string to an existing NULL-terminated string, never writing
 * more characters into the buffer than indicated by 'bufsize', and
 * NULL-terminating the buffer.  The 'buf' and 'src' strings may not overlap.
 *
 * my_strcat() returns strlen(buf) + strlen(src).  This makes checking for
 * truncation easy.  Example:
 *   if (my_strcat(buf, src, sizeof(buf)) >= sizeof(buf)) ...;
 *
 * This function should be equivalent to the strlcat() function in BSD.
 */
extern size_t my_strcat(char *buf, const char *src, size_t bufsize);

/*
 * Capitalise string 'buf'
 */
extern void my_strcap(char *buf);

/*
 * Test equality, prefix, suffix
 */
extern bool streq(const char *s, const char *t);
extern bool prefix(const char *s, const char *t);
extern bool suffix(const char *s, const char *t);

/*
 * Skip occurrences of a character
 */
extern void strskip(char *s, const char c, const char e);
extern void strescape(char *s, const char c);

/*
 * Determines if a string is "empty"
 */
extern bool contains_only_spaces(const char *s);

/*
 * Print an error message
 */
extern void plog(const char *str);

/*
 * Exit, with optional message
 */
extern void quit(const char *str);

/*
 * Check if a char is a vowel
 */
extern bool is_a_vowel(int ch);

/* hturn manipulations */
extern u32b ht_diff(hturn *ht_ptr1, hturn *ht_ptr2);
extern char *ht_show(hturn *ht_ptr);
extern void ht_copy(hturn *ht_ptr1, hturn *ht_ptr2);
extern void ht_add(hturn *ht_ptr, u32b value);
extern void ht_reset(hturn *ht_ptr);
extern bool ht_zero(hturn *ht_ptr);
extern int ht_cmp(hturn *ht_ptr1, hturn *ht_ptr2);
extern u32b ht_div(hturn *ht_ptr, s16b value);

/*
 * Sorting functions
 */
extern void sort(void *array, size_t nmemb, size_t smemb,
    int (*comp)(const void *a, const void *b));

/*
 * Create a hash for a string
 */
extern u32b djb2_hash(const char *str);

/*
 * Mathematical functions
 */
extern int mean(int *nums, int size);
extern int variance(int *nums, int size);

/* Tests a condition and possibly aborts, using the "assert" macro */
#define my_assert(p) \
    if (assert_aux && !(p)) (*assert_aux)(); \
    assert(p)

#endif
