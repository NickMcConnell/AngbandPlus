/*
 * File: parser.h
 * Purpose: Init file parser library
 */

#ifndef PARSER_H
#define PARSER_H

/*
 *
 * The basic structure of the parser is as follows: there is a table of hooks
 * which are run when a directive matching their format is encountered. When the
 * hook is called, all the arguments it declares in its format have been parsed
 * out and can be accessed with parser_get*().
 */

struct parser;

struct parser_state
{
    enum parser_error error;
    unsigned int line;
    unsigned int col;
    char *msg;
};

extern struct parser *parser_new(void);
extern enum parser_error parser_parse(struct parser *p, const char *line);
extern void parser_destroy(struct parser *p);
extern void *parser_priv(struct parser *p);
extern void parser_setpriv(struct parser *p, void *v);
extern errr parser_reg(struct parser *p, const char *fmt,
    enum parser_error (*func)(struct parser *p));
extern enum parser_error ignored(struct parser *p);
extern bool parser_hasval(struct parser *p, const char *name);
extern const char *parser_getsym(struct parser *p, const char *name);
extern const char *parser_getstr(struct parser *p, const char *name);
extern int parser_getint(struct parser *p, const char *name);
extern unsigned int parser_getuint(struct parser *p, const char *name);
extern struct random parser_getrand(struct parser *p, const char *name);
extern char parser_getchar(struct parser *p, const char *name);
extern int parser_getstate(struct parser *p, struct parser_state *s);
extern void parser_setstate(struct parser *p, unsigned int col, const char *msg);

#endif /* PARSER_H */
