/* macro.h */

/*
 * Purpose: Macro code
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 2, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */

extern int max_macrotrigger;
extern const char* macro_template;
extern const char* macro_modifier_chr;
extern const char* macro_modifier_name[MAX_MACRO_MOD];
extern const char* macro_trigger_name[MAX_MACRO_TRIGGER];
extern const char* macro_trigger_keycode[2][MAX_MACRO_TRIGGER];
extern s16b macro__num;
extern const char** macro__pat;
extern const char** macro__act;
extern char macro_buffer[1024];

extern int macro_find_exact(const char* pat);
extern errr macro_add(const char* pat, const char* act);
extern void macro_init(void);
extern void macro_free(void);
extern void macro_trigger_free(void);

extern int macro_find_check(const char* pat);
extern int macro_find_maybe(const char* pat);
extern int macro_find_ready(const char* pat);
