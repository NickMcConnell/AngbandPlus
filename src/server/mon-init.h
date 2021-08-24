/*
 * File: mon-init.h
 * Purpose: Parsing functions for monsters and monster base types
 */

#ifndef MONSTER_INIT_H
#define MONSTER_INIT_H

extern const char *r_info_flags[];
extern const char *r_info_spell_flags[];
extern struct file_parser meth_parser;
extern struct file_parser eff_parser;
extern struct file_parser pain_parser;
extern struct file_parser mon_spell_parser;
extern struct file_parser mon_base_parser;
extern struct file_parser monster_parser;
extern struct file_parser pit_parser;

#endif /* MONSTER_INIT_H */
