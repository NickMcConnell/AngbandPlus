/*
 * File: ui-message.h
 * Purpose: Message handling
 */

#ifndef INCLUDED_UI_MESSAGE_H
#define INCLUDED_UI_MESSAGE_H

/* Message iterator */
typedef struct
{
    char *str;
    u16b type;
    u16b count;
    byte color;
} message_iter;

/* Functions */
extern void messages_init(void);
extern void messages_free(void);
extern u16b messages_num(void);
extern void message_add(const char *str, u16b type);
extern const char *message_str(u16b age);
extern u16b message_count(u16b age);
extern u16b message_type(u16b age);
extern byte message_color(u16b age);
extern void message_color_define(u16b type, byte color);
extern byte message_type_color(u16b type);
extern int message_lookup_by_sound_name(const char *name);
extern const char *message_sound_name(int message);
extern const char *message_last(void);
extern void message_del(u16b age);
extern void message_first(message_iter *iter);
extern void message_next(message_iter *iter);
extern void sound(int type);
extern void bell(const char *reason);
extern void c_msg_print_aux(const char *msg, u16b type);
extern void c_msg_print(const char *msg);

#endif /* INCLUDED_UI_MESSAGE_H */
