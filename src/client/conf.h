/*
 * File: conf.h
 * Purpose: INI file configuration
 */

#ifndef INCLUDED_CONF_H
#define INCLUDED_CONF_H

extern void conf_init(void* param);
extern void conf_save(void);
extern bool conf_section_exists(const char *section);
extern const char *conf_get_string(const char *section, const char *name, const char *default_value);
extern s32b conf_get_int(const char *section, const char *name, s32b default_value);
extern void conf_set_string(const char *section, const char *name, const char *value);
extern void conf_set_int(const char *section, const char *name, s32b value);
extern void conf_append_section(const char *sectionFrom, const char *sectionTo, const char *filename);
extern void conf_timer(int ticks);
extern bool conf_exists(void);
extern void clia_init(int argc, const char **argv);
extern bool clia_read_string(char *dst, int len, const char *key);
extern bool clia_read_int(s32b *dst, const char *key);

#endif /* INCLUDED_CONF_H */
