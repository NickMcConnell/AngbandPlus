#ifndef INCLUDED_STRING_H
#define INCLUDED_STRING_H

/* TODO */

typedef struct {
	int len;
	int cap;
	char *buf;
} string;

extern void string_init(string *str);
extern void string_clear(string *str);

extern void string_reserve(string *str, int cap);
extern void string_trim(string *str);

extern void string_append(string *str, char *what);
extern void string_format(string *str, char *fmt, ...);

#endif