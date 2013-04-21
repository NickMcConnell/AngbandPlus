;;; Please do not edit this _GENERATED_ file.


(in-package :langband-ffi)

(alien:def-alien-type byte c-call:unsigned-char)
(export 'byte)

(alien:def-alien-type cptr c-call:c-string)
(export 'cptr)

(alien:def-alien-type errr c-call:int)
(export 'errr)


(alien:def-alien-routine ("z_quit" c-quit!) c-call:void (msg cptr))
(export 'c-quit!)

(alien:def-alien-routine ("bell" c-bell!) c-call:void (msg cptr))
(export 'c-bell!)

(alien:def-alien-routine ("pause_line" c-pause-line!)
                         c-call:void
                         (row c-call:int :in))
(export 'c-pause-line!)

(alien:def-alien-routine ("clear_from" c-clear-from!)
                         c-call:void
                         (row c-call:int :in))
(export 'c-clear-from!)

(alien:def-alien-routine ("prt" c-prt!)
                         c-call:void
                         (text cptr)
                         (row c-call:int :in)
                         (col c-call:int :in))
(export 'c-prt!)

(alien:def-alien-routine ("put_str" c-put-str!)
                         c-call:void
                         (text cptr)
                         (row c-call:int :in)
                         (col c-call:int :in))
(export 'c-put-str!)

(alien:def-alien-routine ("c_put_str" c-col-put-str!)
                         c-call:void
                         (colour byte)
                         (text cptr)
                         (row c-call:int :in)
                         (col c-call:int :in))
(export 'c-col-put-str!)

(alien:def-alien-routine ("print_coloured_token" c-prt-token!)
                         c-call:void
                         (colour byte)
                         (token c-call:int :in)
                         (row c-call:int :in)
                         (col c-call:int :in))
(export 'c-prt-token!)

(alien:def-alien-routine ("print_coloured_stat" c-prt-stat!)
                         c-call:void
                         (colour byte)
                         (stat c-call:int :in)
                         (row c-call:int :in)
                         (col c-call:int :in))
(export 'c-prt-stat!)

(alien:def-alien-routine ("print_coloured_number" c-prt-number!)
                         c-call:void
                         (colour byte)
                         (number c-call:long :in)
                         (padding c-call:int :in)
                         (row c-call:int :in)
                         (col c-call:int :in))
(export 'c-prt-number!)

(alien:def-alien-routine ("msg_print" c-print-message!) c-call:void (msg cptr))
(export 'c-print-message!)

(alien:def-alien-routine ("Term_putstr" c-term-putstr!)
                         errr
                         (row c-call:int :in)
                         (col c-call:int :in)
                         (something c-call:int :in)
                         (colour byte)
                         (text cptr))
(export 'c-term-putstr!)

(alien:def-alien-routine ("Term_queue_char" c-term-queue-char!)
                         c-call:void
                         (row c-call:int :in)
                         (col c-call:int :in)
                         (colour byte)
                         (text char))
(export 'c-term-queue-char!)

(alien:def-alien-routine ("Term_gotoxy" c-term-gotoxy!)
                         c-call:void
                         (row c-call:int :in)
                         (col c-call:int :in))
(export 'c-term-gotoxy!)

(alien:def-alien-routine ("Term_set_cursor" c-set-cursor&)
                         errr
                         (col c-call:int :in))
(export 'c-set-cursor&)

(alien:def-alien-routine ("Term_clear" c-term-clear!) errr)
(export 'c-term-clear!)

(alien:def-alien-routine ("Term_fresh" c-term-fresh!) errr)
(export 'c-term-fresh!)

(alien:def-alien-routine ("Term_save" c-term-save!) errr)
(export 'c-term-save!)

(alien:def-alien-routine ("Term_load" c-term-load!) errr)
(export 'c-term-load!)

(alien:def-alien-routine ("Term_xtra" c-term-xtra&)
                         errr
                         (msg c-call:int :in)
                         (arg c-call:int :in))
(export 'c-term-xtra&)

(alien:def-alien-routine ("Term_inkey" c-term-inkey&)
                         errr
                         (text cptr)
                         (row c-call:int :in)
                         (col c-call:int :in))
(export 'c-term-inkey&)

(alien:def-alien-routine ("inkey" c-inkey!) char)
(export 'c-inkey!)

(alien:def-alien-routine ("init_gui" c-init-gui!)
                         errr
                         (argc c-call:int :in)
                         (argv (* cptr) :in))
(export 'c-init-gui!)

(alien:def-alien-routine ("init_angband" c-init.angband!) c-call:void)
(export 'c-init.angband!)

(alien:def-alien-routine ("macro_add" c-macro-add&)
                         c-call:void
                         (key cptr)
                         (value cptr))
(export 'c-macro-add&)

#+use-callback-from-c

(alien:def-alien-routine ("set_lisp_callback" set_lisp_callback!)
                         c-call:void
                         (ptr alien:unsigned :in))

#+use-callback-from-c

(export 'set_lisp_callback!)

#+using-sound

(alien:def-alien-routine ("load_sound" c-load-sound&)
                         errr
                         (msg c-call:int :in)
                         (fname cptr))

#+using-sound

(export 'c-load-sound&)

;;; End of generated file.
