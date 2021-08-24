/*
 * File: ui-spell.h
 * Purpose: Spell UI handing
 */

#ifndef UI_SPELL_H
#define UI_SPELL_H


/* Maximum number of spell pages */
#define MAX_PAGES    10

struct spell_info
{
    char info[NORMAL_WID];
    spell_flags flag;
    char desc[MSG_LEN];
};

/* Spell information array */
extern struct spell_info spell_info[MAX_PAGES][MAX_SPELLS_PER_PAGE];

extern void textui_book_browse(int book);
extern int textui_get_spell(int book, const char *verb, bool (*spell_filter)(int, int));
extern bool spell_okay_to_study(int book, int spell_index);
extern bool spell_okay_to_cast(int book, int spell);
extern int textui_obj_cast(int book, int *dir);
extern int textui_obj_project(int book, int *dir);

#endif
