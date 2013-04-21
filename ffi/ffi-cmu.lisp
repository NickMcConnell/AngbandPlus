;;; Please do not edit this _GENERATED_ file.


(in-package :org.langband.ffi)
(alien:def-alien-type cptr c-call:c-string)

(export 'cptr)
(alien:def-alien-type errr c-call:int)

(export 'errr)


(declaim (inline c_current_ui))
(alien:def-alien-routine ("current_ui" c_current_ui)
           c-call:int)


(declaim (inline c_quit!))
(alien:def-alien-routine ("z_quit" c_quit!)
           c-call:void
           (msg (* char) :in))


(declaim (inline c-clear-from!))
(alien:def-alien-routine ("clear_from" c-clear-from!)
           c-call:void
           (row c-call:int :in))


(declaim (inline c-prt-token!))
(alien:def-alien-routine ("print_coloured_token" c-prt-token!)
           c-call:void
           (colour c-call:int :in)
           (token c-call:int :in)
           (row c-call:int :in)
           (col c-call:int :in))


(declaim (inline c-prt-stat!))
(alien:def-alien-routine ("print_coloured_stat" c-prt-stat!)
           c-call:void
           (colour c-call:int :in)
           (stat c-call:int :in)
           (row c-call:int :in)
           (col c-call:int :in))


(declaim (inline c-prt-number!))
(alien:def-alien-routine ("print_coloured_number" c-prt-number!)
           c-call:void
           (colour c-call:int :in)
           (number c-call:long :in)
           (padding c-call:int :in)
           (row c-call:int :in)
           (col c-call:int :in))


(declaim (inline c_term_putstr!))
(alien:def-alien-routine ("my_Term_putstr" c_term_putstr!)
           errr
           (col c-call:int :in)
           (row c-call:int :in)
           (something c-call:int :in)
           (colour c-call:int :in)
           (text (* char) :in))


(declaim (inline c_term_erase!))
(alien:def-alien-routine ("Term_erase" c_term_erase!)
           errr
           (col c-call:int :in)
           (row c-call:int :in)
           (something c-call:int :in))


(declaim (inline c-term-queue-char!))
(alien:def-alien-routine ("my_Term_queue_char" c-term-queue-char!)
           c-call:void
           (row c-call:int :in)
           (col c-call:int :in)
           (colour c-call:int :in)
           (the-char c-call:int :in)
           (tcolour c-call:int :in)
           (tchar c-call:int :in))


(declaim (inline c-term-gotoxy!))
(alien:def-alien-routine ("Term_gotoxy" c-term-gotoxy!)
           c-call:void
           (row c-call:int :in)
           (col c-call:int :in))


(declaim (inline c-set-cursor&))
(alien:def-alien-routine ("my_Term_set_cursor" c-set-cursor&)
           errr
           (col c-call:int :in))


(declaim (inline c-term-clear!))
(alien:def-alien-routine ("Term_clear" c-term-clear!)
           errr)


(declaim (inline c-term-flush!))
(alien:def-alien-routine ("Term_flush" c-term-flush!)
           errr)


(declaim (inline c-term-fresh!))
(alien:def-alien-routine ("Term_fresh" c-term-fresh!)
           errr)


(declaim (inline c-term-save!))
(alien:def-alien-routine ("Term_save" c-term-save!)
           errr)


(declaim (inline c-term-load!))
(alien:def-alien-routine ("Term_load" c-term-load!)
           errr)


(declaim (inline c-term-xtra&))
(alien:def-alien-routine ("Term_xtra" c-term-xtra&)
           errr
           (msg c-call:int :in)
           (arg c-call:int :in))


(declaim (inline c-inkey!))
(alien:def-alien-routine ("inkey" c-inkey!)
           char)


(declaim (inline init_c-side&))
(alien:def-alien-routine ("init_c_side" init_c-side&)
           errr
           (ui cptr)
           (base-path cptr)
           (debug-level c-call:int :in))


(declaim (inline cleanup-c-side&))
(alien:def-alien-routine ("cleanup_c_side" cleanup-c-side&)
           errr)


(declaim (inline c_macro_add&))
(alien:def-alien-routine ("macro_add" c_macro_add&)
           c-call:void
           (key cptr)
           (value cptr))


(declaim (inline init-macro-system&))
(alien:def-alien-routine ("macro_init" init-macro-system&)
           c-call:void)


(declaim (inline c-set-lisp-system!))
(alien:def-alien-routine ("set_lisp_system" c-set-lisp-system!)
           c-call:void
           (type c-call:int :in))


#+use-callback-from-c

(declaim (inline c-set-lisp-callback!))

#+use-callback-from-c
(alien:def-alien-routine ("set_lisp_callback" c-set-lisp-callback!)
           c-call:void
           (name cptr)
           (ptr alien:unsigned :in))


(declaim (inline c-get-term-height))
(alien:def-alien-routine ("get_term_height" c-get-term-height)
           c-call:int)


(declaim (inline c-get-term-width))
(alien:def-alien-routine ("get_term_width" c-get-term-width)
           c-call:int)


#+win32

(declaim (inline c-set-hinst!))

#+win32
(alien:def-alien-routine ("setHINST" c-set-hinst!)
           c-call:int
           (val c-call:long :in))


#+using-sound

(declaim (inline c-load-sound&))

#+using-sound
(alien:def-alien-routine ("load_sound" c-load-sound&)
           errr
           (msg c-call:int :in)
           (fname cptr))


#+image-support

(declaim (inline paint-gfx-image&))

#+image-support
(alien:def-alien-routine ("paint_gfx_image" paint-gfx-image&)
           c-call:int
           (fname cptr)
           (type cptr)
           (x c-call:int :in)
           (y c-call:int :in))


#+image-support

(declaim (inline load-gfx-image&))

#+image-support
(alien:def-alien-routine ("load_gfx_image" load-gfx-image&)
           c-call:int
           (fname cptr)
           (type cptr))


#+image-support

(declaim (inline load-scaled-image&))

#+image-support
(alien:def-alien-routine ("load_scaled_image" load-scaled-image&)
           c-call:int
           (fname cptr)
           (idx c-call:int :in)
           (wid c-call:int :in)
           (hgt c-call:int :in))


(eval-when (:execute :load-toplevel :compile-toplevel)
  (export
   '(c_current_ui c_quit! c-clear-from! c-prt-token! c-prt-stat! c-prt-number!
     c_term_putstr! c_term_erase! c-term-queue-char! c-term-gotoxy!
     c-set-cursor& c-term-clear! c-term-flush! c-term-fresh! c-term-save!
     c-term-load! c-term-xtra& c-inkey! init_c-side& cleanup-c-side&
     c_macro_add& init-macro-system& c-set-lisp-system! c-set-lisp-callback!
     c-get-term-height c-get-term-width c-set-hinst! c-load-sound&
     paint-gfx-image& load-gfx-image& load-scaled-image&)))

;;; End of generated file.
