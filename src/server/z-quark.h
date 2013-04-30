/*
 * File: z-quark.h
 * Purpose: Save memory by storing strings in a global array, ensuring
 * that each is only allocated once.
 */

#ifndef INCLUDED_Z_QUARK_H
#define INCLUDED_Z_QUARK_H

/* Return a quark for the string 'str' */
extern quark_t quark_add(const char *str);

/* Return the string corresponding to the quark */
extern const char *quark_str(quark_t q);

/* Initialise the quarks package */
extern errr quarks_init(void);

/* De-initialise the quarks package */
extern errr quarks_free(void);

#endif /* !INCLUDED_Z_QUARK_H */
