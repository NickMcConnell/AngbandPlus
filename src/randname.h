#ifndef RANDNAME_H
#define RANDNAME_H

/*
 * The different types of name randname.c can generate
 * which is also the number of sections in names.txt
 */
typedef enum
{
	RANDNAME_TOLKIEN = 1,
	RANDNAME_SCROLL,
	RANDNAME_PONY_FIRST, /* Special, triggers a different name generator */
	RANDNAME_PONY_LAST, /* Shouldn't be called. Will act as above, though */
	RANDNAME_DRAGON,
	RANDNAME_ZEBRA_M,
	RANDNAME_ZEBRA_F,
	RANDNAME_DDOG_M,
	RANDNAME_DDOG_F,
	RANDNAME_GRIFFON_M,
	RANDNAME_GRIFFON_F,
	RANDNAME_BUFFALO_M,
	RANDNAME_BUFFALO_F,
	RANDNAME_MULE_M,
	RANDNAME_MULE_F,
	RANDNAME_HUMAN_M,
	RANDNAME_HUMAN_F,
 
	/* End of type marker - not a valid name type */
	RANDNAME_NUM_TYPES 
} randname_type;

/*
 * Make a random name.
 */
extern size_t randname_make(randname_type name_type, size_t min, size_t max, char *word_buf, size_t buflen, const char ***wordlist);

#endif /* RANDNAME_H */
