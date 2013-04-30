/*
 * File: files.h
 * Purpose: Game file interface
 */

#ifndef FILES_H
#define FILES_H

/* files.c */
extern bool process_pref_file(const char *name, bool quiet);
extern bool process_pref_file_xtra(const char *name);
extern errr show_file(int Ind, const char *name, const char *what, int line, int color);
extern bool process_player_name(struct player *p, bool sf);
extern int process_player_name_aux(const char *name, const char *base, const char *save,
    bool sf);
extern void close_game(void);
extern void exit_game_panic(void);
extern void setup_exit_handler(void);
extern errr file_character_server(int Ind, char *base, char *name, bool ladder);
extern void common_file_peruse(int Ind, u32b query);
extern void copy_file_info(int Ind, const char *name, int line, int color);
extern void kingly(int Ind);

#endif /* FILES_H */
