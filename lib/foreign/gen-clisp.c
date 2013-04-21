#include "clisp.h"

extern object module__gen_clisp__object_tab[];

subr_ module__gen_clisp__subr_tab[1];
uintC module__gen_clisp__subr_tab_size = 0;
subr_initdata module__gen_clisp__subr_tab_initdata[1];

object module__gen_clisp__object_tab[1];
object_initdata module__gen_clisp__object_tab_initdata[1];
uintC module__gen_clisp__object_tab_size = 0;

extern void (z_quit)();
extern void (bell)();
extern void (pause_line)();
extern void (clear_from)();
extern void (prt)();
extern void (put_str)();
extern void (c_put_str)();
extern void (msg_print)();
extern int (Term_putstr)();
extern void (Term_queue_char)();
extern void (Term_gotoxy)();
extern int (Term_set_cursor)();
extern int (Term_clear)();
extern int (Term_fresh)();
extern int (Term_save)();
extern int (Term_load)();
extern int (Term_xtra)();
extern int (Term_inkey)();
extern sint8 (inkey)();
extern int (init_gui)();
extern void (init_angband)();
extern void (macro_add)();

void module__gen_clisp__init_function_1(module)
  var module_* module;
{ }

void module__gen_clisp__init_function_2(module)
  var module_* module;
{
  register_foreign_function(&z_quit,"z_quit",1024);
  register_foreign_function(&bell,"bell",1024);
  register_foreign_function(&pause_line,"pause_line",1024);
  register_foreign_function(&clear_from,"clear_from",1024);
  register_foreign_function(&prt,"prt",1024);
  register_foreign_function(&put_str,"put_str",1024);
  register_foreign_function(&c_put_str,"c_put_str",1024);
  register_foreign_function(&msg_print,"msg_print",1024);
  register_foreign_function(&Term_putstr,"Term_putstr",1024);
  register_foreign_function(&Term_queue_char,"Term_queue_char",1024);
  register_foreign_function(&Term_gotoxy,"Term_gotoxy",1024);
  register_foreign_function(&Term_set_cursor,"Term_set_cursor",1024);
  register_foreign_function(&Term_clear,"Term_clear",1024);
  register_foreign_function(&Term_fresh,"Term_fresh",1024);
  register_foreign_function(&Term_save,"Term_save",1024);
  register_foreign_function(&Term_load,"Term_load",1024);
  register_foreign_function(&Term_xtra,"Term_xtra",1024);
  register_foreign_function(&Term_inkey,"Term_inkey",1024);
  register_foreign_function(&inkey,"inkey",1024);
  register_foreign_function(&init_gui,"init_gui",1024);
  register_foreign_function(&init_angband,"init_angband",1024);
  register_foreign_function(&macro_add,"macro_add",1024);
}
