;;; Please do not edit this _GENERATED_ file.


(in-package :langband-ffi)
(ff:def-foreign-type byte :unsigned-char)

(export 'byte)
(ff:def-foreign-type cptr (* :char))

(export 'cptr)
(ff:def-foreign-type errr :int)

(export 'errr)

(ff:def-foreign-call (c-quit! "z_quit") ((msg cptr)) :returning :void)

(export 'c-quit!)
(ff:def-foreign-call (c-bell! "bell") ((msg cptr)) :returning :void)

(export 'c-bell!)
(ff:def-foreign-call (c-pause-line! "pause_line") ((row :int)) :returning :void)

(export 'c-pause-line!)
(ff:def-foreign-call (c-clear-from! "clear_from") ((row :int)) :returning :void)

(export 'c-clear-from!)
(ff:def-foreign-call (c-prt! "prt") ((text cptr) (row :int) (col :int)) :returning :void)

(export 'c-prt!)
(ff:def-foreign-call (c-put-str! "put_str") ((text cptr) (row :int) (col :int)) :returning :void)

(export 'c-put-str!)
(ff:def-foreign-call (c-col-put-str! "c_put_str") ((colour byte) (text cptr)
                                                   (row :int) (col :int)) :returning :void)

(export 'c-col-put-str!)
(ff:def-foreign-call (c-prt-token! "print_coloured_token") ((colour byte)
                                                            (token :int)
                                                            (row :int)
                                                            (col :int)) :returning :void)

(export 'c-prt-token!)
(ff:def-foreign-call (c-prt-stat! "print_coloured_stat") ((colour byte)
                                                          (stat :int)
                                                          (row :int) (col :int)) :returning :void)

(export 'c-prt-stat!)
(ff:def-foreign-call (c-prt-number! "print_coloured_number") ((colour byte)
                                                              (number :long)
                                                              (padding :int)
                                                              (row :int)
                                                              (col :int)) :returning :void)

(export 'c-prt-number!)
(ff:def-foreign-call (c-print-message! "msg_print") ((msg cptr)) :returning :void)

(export 'c-print-message!)
(ff:def-foreign-call (c-term-putstr! "Term_putstr") ((row :int) (col :int)
                                                     (something :int)
                                                     (colour byte) (text cptr)) :returning errr)

(export 'c-term-putstr!)
(ff:def-foreign-call (c-term-queue-char! "Term_queue_char") ((row :int)
                                                             (col :int)
                                                             (colour byte)
                                                             (text :char)) :returning :void)

(export 'c-term-queue-char!)
(ff:def-foreign-call (c-term-gotoxy! "Term_gotoxy") ((row :int) (col :int)) :returning :void)

(export 'c-term-gotoxy!)
(ff:def-foreign-call (c-set-cursor& "Term_set_cursor") ((col :int)) :returning errr)

(export 'c-set-cursor&)
(ff:def-foreign-call (c-term-clear! "Term_clear") nil :returning errr)

(export 'c-term-clear!)
(ff:def-foreign-call (c-term-fresh! "Term_fresh") nil :returning errr)

(export 'c-term-fresh!)
(ff:def-foreign-call (c-term-save! "Term_save") nil :returning errr)

(export 'c-term-save!)
(ff:def-foreign-call (c-term-load! "Term_load") nil :returning errr)

(export 'c-term-load!)
(ff:def-foreign-call (c-term-xtra& "Term_xtra") ((msg :int) (arg :int)) :returning errr)

(export 'c-term-xtra&)
(ff:def-foreign-call (c-term-inkey& "Term_inkey") ((text cptr) (row :int)
                                                   (col :int)) :returning errr)

(export 'c-term-inkey&)
(ff:def-foreign-call (c-inkey! "inkey") nil :returning :char)

(export 'c-inkey!)
(ff:def-foreign-call (c-init-gui! "init_gui") ((argc :int) (argv (* cptr))) :returning errr)

(export 'c-init-gui!)
(ff:def-foreign-call (c-init.angband! "init_angband") nil :returning :void)

(export 'c-init.angband!)
(ff:def-foreign-call (c-macro-add& "macro_add") ((key cptr) (value cptr)) :returning :void)

(export 'c-macro-add&)

#+use-callback-from-c
(ff:def-foreign-call (set_lisp_callback! "set_lisp_callback") ((ptr)) :returning :void)

#+use-callback-from-c

(export 'set_lisp_callback!)

#+using-sound
(ff:def-foreign-call (c-load-sound& "load_sound") ((msg :int) (fname cptr)) :returning errr)

#+using-sound

(export 'c-load-sound&)

;;; End of generated file.
