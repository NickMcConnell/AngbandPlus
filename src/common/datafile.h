/*
 * File: datafile.h
 * Purpose: Data file reading and writing routines
 */

#ifndef DATAFILE_H
#define DATAFILE_H

/*
 * Parse errors
 */
enum parser_error
{
    #define PARSE_ERROR(a, b) PARSE_ERROR_##a,
    #include "list-parser-errors.h"
    #undef PARSE_ERROR

    PARSE_ERROR_MAX
};

struct file_parser
{
    const char *name;
    struct parser *(*init)(void);
    errr (*run)(struct parser *p);
    errr (*finish)(struct parser *p);
    void (*cleanup)(void);
};

extern const char *parser_error_str[PARSE_ERROR_MAX + 1];

extern errr run_parser(struct file_parser *fp);
extern errr parse_file_quit_not_found(struct parser *p, const char *filename);
extern errr parse_file(struct parser *p, const char *filename);
extern void cleanup_parser(struct file_parser *fp);
extern int lookup_flag(const char **flag_table, const char *flag_name);
extern errr grab_rand_value(random_value *value, const char **value_type,
    const char *name_and_value);
extern errr grab_int_value(int *value, const char **value_type, const char *name_and_value);
extern errr grab_index_and_int(int *value, int *index, const char **value_type, const char *prefix,
    const char *name_and_value);
extern errr grab_base_and_int(int *value, char **base, const char *name_and_value);
extern errr grab_name(const char *from, const char *what, const char *list[], int max, int *num);
extern errr grab_flag(bitflag *flags, const size_t size, const char **flag_table,
    const char *flag_name);
extern errr remove_flag(bitflag *flags, const size_t size, const char **flag_table,
    const char *flag_name);

#endif /* DATAFILE_H */
