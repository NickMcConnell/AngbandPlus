/*
 * File: s-util.h
 * Purpose: Utility functions
 */

#ifndef S_UTILS_H
#define S_UTILS_H

extern char color_attr_to_char(int a);
extern void fill_prevent_inscription(bool *arr, quark_t quark);
extern void update_prevent_inscriptions(struct player *p);
extern void alloc_info_icky(struct player *p);
extern s16b get_last_info_line(struct player *p);
extern cave_view_type* get_info(struct player *p, int y, int x);
extern void free_info_icky(struct player *p);
extern void alloc_header_icky(struct player *p, const char *header);
extern const char *get_header(struct player *p, const char *header);
extern void free_header_icky(struct player *p);
extern void set_ghost_flag(struct player *p, s16b flag, bool report);
extern void notify_player(struct player *p, char *header, u16b term, bool symbol);
extern void notify_player_popup(struct player *p, char *header, u16b term, u16b pop);
extern const char *player_poss(struct player *p);
extern const char *player_self(struct player *p);
extern bool strrep(char *dest, size_t len, const char *src, const char *search, const char *replace);
extern void strrepall(char *dest, size_t len, const char *src, const char *search,
    const char *replace);
extern void clean_name(char *buf, char *name);

#endif /* S_UTILS_H */
