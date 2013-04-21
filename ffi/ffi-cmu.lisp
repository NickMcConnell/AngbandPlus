;;; Please do not edit this _GENERATED_ file.


(in-package :langband-ffi)

(alien:def-alien-type byte c-call:unsigned-char)
(export 'byte)

(alien:def-alien-type cptr c-call:c-string)
(export 'cptr)

(alien:def-alien-type errr c-call:int)
(export 'errr)


(alien:def-alien-routine ("current_ui" c_current_ui) c-call:int)

(alien:def-alien-routine ("z_quit" c_quit!) c-call:void (msg (* char) :in))

(alien:def-alien-routine ("bell" c_bell!) c-call:void (msg (* char) :in))

(alien:def-alien-routine ("pause_line" c-pause-line!)
                         c-call:void
                         (row c-call:int :in))

(alien:def-alien-routine ("clear_from" c-clear-from!)
                         c-call:void
                         (row c-call:int :in))

(alien:def-alien-routine ("prt" c_prt!)
                         c-call:void
                         (text (* char) :in)
                         (row c-call:int :in)
                         (col c-call:int :in))

(alien:def-alien-routine ("print_coloured_token" c-prt-token!)
                         c-call:void
                         (colour byte)
                         (token c-call:int :in)
                         (row c-call:int :in)
                         (col c-call:int :in))

(alien:def-alien-routine ("print_coloured_stat" c-prt-stat!)
                         c-call:void
                         (colour byte)
                         (stat c-call:int :in)
                         (row c-call:int :in)
                         (col c-call:int :in))

(alien:def-alien-routine ("print_coloured_number" c-prt-number!)
                         c-call:void
                         (colour byte)
                         (number c-call:long :in)
                         (padding c-call:int :in)
                         (row c-call:int :in)
                         (col c-call:int :in))

(alien:def-alien-routine ("msg_print" c_msg_print!)
                         c-call:void
                         (msg (* char) :in))

(alien:def-alien-routine ("Term_putstr" c_term_putstr!)
                         errr
                         (col c-call:int :in)
                         (row c-call:int :in)
                         (something c-call:int :in)
                         (colour byte)
                         (text (* char) :in))

(alien:def-alien-routine ("Term_queue_char" c-term-queue-char!)
                         c-call:void
                         (row c-call:int :in)
                         (col c-call:int :in)
                         (colour byte)
                         (the-char char))

(alien:def-alien-routine ("Term_gotoxy" c-term-gotoxy!)
                         c-call:void
                         (row c-call:int :in)
                         (col c-call:int :in))

(alien:def-alien-routine ("Term_set_cursor" c-set-cursor&)
                         errr
                         (col c-call:int :in))

(alien:def-alien-routine ("Term_clear" c-term-clear!) errr)

(alien:def-alien-routine ("Term_fresh" c-term-fresh!) errr)

(alien:def-alien-routine ("Term_save" c-term-save!) errr)

(alien:def-alien-routine ("Term_load" c-term-load!) errr)

(alien:def-alien-routine ("Term_xtra" c-term-xtra&)
                         errr
                         (msg c-call:int :in)
                         (arg c-call:int :in))

(alien:def-alien-routine ("Term_inkey" c-term-inkey&)
                         errr
                         (text (* char) :in)
                         (row c-call:int :in)
                         (col c-call:int :in))

(alien:def-alien-routine ("inkey" c-inkey!) char)

(alien:def-alien-routine ("init_c_side" init-c-side&)
                         errr
                         (ui cptr)
                         (base-path cptr)
                         (debug-level c-call:int :in))

(alien:def-alien-routine ("init_angband" c-init.angband!) c-call:void)

(alien:def-alien-routine ("macro_add" c_macro_add&)
                         c-call:void
                         (key cptr)
                         (value cptr))

(alien:def-alien-routine ("set_lisp_system" c-set-lisp-system!)
                         c-call:void
                         (type c-call:int :in))

#+use-callback-from-c

(alien:def-alien-routine ("set_lisp_callback" c-set-lisp-callback!)
                         c-call:void
                         (ptr alien:unsigned :in))

#+using-sound

(alien:def-alien-routine ("load_sound" c-load-sound&)
                         errr
                         (msg c-call:int :in)
                         (fname cptr))

(alien:def-alien-routine ("test_calling_2" c_test_calling!)
                         c-call:int
                         (msg (* char) :in))

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export
   '(c_current_ui c_quit! c_bell! c-pause-line! c-clear-from! c_prt!
     c-prt-token! c-prt-stat! c-prt-number! c_msg_print! c_term_putstr!
     c-term-queue-char! c-term-gotoxy! c-set-cursor& c-term-clear!
     c-term-fresh! c-term-save! c-term-load! c-term-xtra& c-term-inkey&
     c-inkey! init-c-side& c-init.angband! c_macro_add& c-set-lisp-system!
     c-set-lisp-callback! c-load-sound& c_test_calling!)))

;;; End of generated file.
