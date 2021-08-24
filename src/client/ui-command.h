/*
 * File: ui-command.h
 * Purpose: Deal with UI only command processing.
 */

#ifndef UI_COMMAND_H
#define UI_COMMAND_H

extern void do_cmd_redraw(void);
extern void do_cmd_xxx_options(void);
extern void do_cmd_unknown(void);
extern void textui_cmd_suicide(void);
extern void textui_cmd_rest(void);
extern void textui_quit(void);
extern void do_cmd_save_screen(void);

extern bool cmd_target_interactive(int mode);
extern void cmd_chat_close(int i);
extern void cmd_chat_cycle(int dir);

#endif /* UI_COMMAND_H */
