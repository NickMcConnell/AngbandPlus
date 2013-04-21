;;; Please do not edit this _GENERATED_ file.


(in-package :org.langband.ffi)
(sb-alien:define-alien-type angbyte unsigned-char)

(export 'angbyte)
(sb-alien:define-alien-type cptr c-string)

(export 'cptr)
(sb-alien:define-alien-type errr int)

(export 'errr)


(declaim (inline c_current_ui))
(sb-alien:define-alien-routine ("current_ui" c_current_ui)
           int)


(declaim (inline c_quit!))
(sb-alien:define-alien-routine ("z_quit" c_quit!)
           void
           (msg (* char) :in))


(declaim (inline c-clear-from!))
(sb-alien:define-alien-routine ("clear_from" c-clear-from!)
           void
           (row int :in))


(declaim (inline c-prt-token!))
(sb-alien:define-alien-routine ("print_coloured_token" c-prt-token!)
           void
           (colour angbyte)
           (token int :in)
           (row int :in)
           (col int :in))


(declaim (inline c-prt-stat!))
(sb-alien:define-alien-routine ("print_coloured_stat" c-prt-stat!)
           void
           (colour angbyte)
           (stat int :in)
           (row int :in)
           (col int :in))


(declaim (inline c-prt-number!))
(sb-alien:define-alien-routine ("print_coloured_number" c-prt-number!)
           void
           (colour angbyte)
           (number long :in)
           (padding int :in)
           (row int :in)
           (col int :in))


(declaim (inline c_term_putstr!))
(sb-alien:define-alien-routine ("Term_putstr" c_term_putstr!)
           errr
           (col int :in)
           (row int :in)
           (something int :in)
           (colour angbyte)
           (text (* char) :in))


(declaim (inline c_term_erase!))
(sb-alien:define-alien-routine ("Term_erase" c_term_erase!)
           errr
           (col int :in)
           (row int :in)
           (something int :in))


(declaim (inline c-term-queue-char!))
(sb-alien:define-alien-routine ("Term_queue_char" c-term-queue-char!)
           void
           (row int :in)
           (col int :in)
           (colour angbyte)
           (the-char char))


(declaim (inline c-term-gotoxy!))
(sb-alien:define-alien-routine ("Term_gotoxy" c-term-gotoxy!)
           void
           (row int :in)
           (col int :in))


(declaim (inline c-set-cursor&))
(sb-alien:define-alien-routine ("Term_set_cursor" c-set-cursor&)
           errr
           (col int :in))


(declaim (inline c-term-clear!))
(sb-alien:define-alien-routine ("Term_clear" c-term-clear!)
           errr)


(declaim (inline c-term-fresh!))
(sb-alien:define-alien-routine ("Term_fresh" c-term-fresh!)
           errr)


(declaim (inline c-term-save!))
(sb-alien:define-alien-routine ("Term_save" c-term-save!)
           errr)


(declaim (inline c-term-load!))
(sb-alien:define-alien-routine ("Term_load" c-term-load!)
           errr)


(declaim (inline c-term-xtra&))
(sb-alien:define-alien-routine ("Term_xtra" c-term-xtra&)
           errr
           (msg int :in)
           (arg int :in))


(declaim (inline c-inkey!))
(sb-alien:define-alien-routine ("inkey" c-inkey!)
           char)


(declaim (inline init_c-side&))
(sb-alien:define-alien-routine ("init_c_side" init_c-side&)
           errr
           (ui cptr)
           (base-path cptr)
           (debug-level int :in))


(declaim (inline cleanup-c-side&))
(sb-alien:define-alien-routine ("cleanup_c_side" cleanup-c-side&)
           errr)


(declaim (inline c_macro_add&))
(sb-alien:define-alien-routine ("macro_add" c_macro_add&)
           void
           (key cptr)
           (value cptr))


(declaim (inline c-set-lisp-system!))
(sb-alien:define-alien-routine ("set_lisp_system" c-set-lisp-system!)
           void
           (type int :in))


#+use-callback-from-c

(declaim (inline c-set-lisp-callback!))

#+use-callback-from-c
(sb-alien:define-alien-routine ("set_lisp_callback" c-set-lisp-callback!)
           void
           (name cptr)
           (ptr unsigned :in))


(declaim (inline c-get-term-height))
(sb-alien:define-alien-routine ("get_term_height" c-get-term-height)
           int)


(declaim (inline c-get-term-width))
(sb-alien:define-alien-routine ("get_term_width" c-get-term-width)
           int)


#+win32

(declaim (inline c-set-hinst!))

#+win32
(sb-alien:define-alien-routine ("setHINST" c-set-hinst!)
           int
           (val long :in))


#+using-sound

(declaim (inline c-load-sound&))

#+using-sound
(sb-alien:define-alien-routine ("load_sound" c-load-sound&)
           errr
           (msg int :in)
           (fname cptr))


(eval-when (:execute :load-toplevel :compile-toplevel)
  (export
   '(c_current_ui c_quit! c-clear-from! c-prt-token! c-prt-stat! c-prt-number!
     c_term_putstr! c_term_erase! c-term-queue-char! c-term-gotoxy!
     c-set-cursor& c-term-clear! c-term-fresh! c-term-save! c-term-load!
     c-term-xtra& c-inkey! init_c-side& cleanup-c-side& c_macro_add&
     c-set-lisp-system! c-set-lisp-callback! c-get-term-height c-get-term-width
     c-set-hinst! c-load-sound&)))

;;; End of generated file.
