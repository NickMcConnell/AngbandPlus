/*
 * File: hint.h
 * Purpose: Hint structure
 */

#ifndef HINT_H
#define HINT_H

/*
 * A hint.
 */
struct hint
{
    char *hint;
    struct hint *next;
};

extern struct hint *hints;

#endif /* HINT_H */
