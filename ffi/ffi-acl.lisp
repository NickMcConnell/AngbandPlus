;;; Please do not edit this _GENERATED_ file.


(in-package :org.langband.ffi)
(ff:def-foreign-type cptr (* :char))

(ff:def-foreign-type errr :int)


(ff:def-foreign-call (c_current_ui "current_ui") nil :returning :int)

(ff:def-foreign-call (c_quit! "z_quit") ((msg (* :void))) :returning :void)

(ff:def-foreign-call (c-clear-from! "clear_from") ((row :int)) :returning :void)

(ff:def-foreign-call (c-prt-token! "print_coloured_token") ((colour :int)
                                                            (token :int)
                                                            (row :int)
                                                            (col :int)) :returning :void)

(ff:def-foreign-call (c-prt-stat! "print_coloured_stat") ((colour :int)
                                                          (stat :int)
                                                          (row :int) (col :int)) :returning :void)

(ff:def-foreign-call (c-prt-number! "print_coloured_number") ((colour :int)
                                                              (number :long)
                                                              (padding :int)
                                                              (row :int)
                                                              (col :int)) :returning :void)

(ff:def-foreign-call (c_term_putstr! "my_Term_putstr") ((col :int) (row :int)
                                                        (something :int)
                                                        (colour :int)
                                                        (text (* :void))) :returning errr)

(ff:def-foreign-call (c_term_erase! "Term_erase") ((col :int) (row :int)
                                                   (something :int)) :returning errr)

(ff:def-foreign-call (c-term-queue-char! "my_Term_queue_char") ((row :int)
                                                                (col :int)
                                                                (colour :int)
                                                                (the-char :int)
                                                                (tcolour :int)
                                                                (tchar :int)) :returning :void)

(ff:def-foreign-call (c-term-gotoxy! "Term_gotoxy") ((row :int) (col :int)) :returning :void)

(ff:def-foreign-call (c-set-cursor& "my_Term_set_cursor") ((col :int)) :returning errr)

(ff:def-foreign-call (c-term-clear! "Term_clear") nil :returning errr)

(ff:def-foreign-call (c-term-flush! "Term_flush") nil :returning errr)

(ff:def-foreign-call (c-term-fresh! "Term_fresh") nil :returning errr)

(ff:def-foreign-call (c-term-save! "Term_save") nil :returning errr)

(ff:def-foreign-call (c-term-load! "Term_load") nil :returning errr)

(ff:def-foreign-call (c-term-xtra& "Term_xtra") ((msg :int) (arg :int)) :returning errr)

(ff:def-foreign-call (c-inkey! "inkey") nil :returning :char)

(ff:def-foreign-call (init_c-side& "init_c_side") ((ui cptr) (base-path cptr)
                                                   (debug-level :int)) :returning errr)

(ff:def-foreign-call (cleanup-c-side& "cleanup_c_side") nil :returning errr)

(ff:def-foreign-call (c_macro_add& "macro_add") ((key cptr) (value cptr)) :returning :void)

(ff:def-foreign-call (init-macro-system& "macro_init") nil :returning :void)

(ff:def-foreign-call (c-set-lisp-system! "set_lisp_system") ((type :int)) :returning :void)


#+use-callback-from-c
(ff:def-foreign-call (c-set-lisp-callback! "set_lisp_callback") ((name cptr)
                                                                 (ptr)) :returning :void)

(ff:def-foreign-call (c-get-term-height "get_term_height") nil :returning :int)

(ff:def-foreign-call (c-get-term-width "get_term_width") nil :returning :int)


#+win32
(ff:def-foreign-call (c-set-hinst! "setHINST") ((val :long)) :returning :int)


#+using-sound
(ff:def-foreign-call (c-load-sound& "load_sound") ((msg :int) (fname cptr)) :returning errr)


#+image-support
(ff:def-foreign-call (paint-gfx-image& "paint_gfx_image") ((fname cptr)
                                                           (type cptr) (x :int)
                                                           (y :int)) :returning :int)


#+image-support
(ff:def-foreign-call (load-gfx-image& "load_gfx_image") ((fname cptr)
                                                         (type cptr)) :returning :int)


#+image-support
(ff:def-foreign-call (load-scaled-image& "load_scaled_image") ((fname cptr)
                                                               (idx :int)
                                                               (wid :int)
                                                               (hgt :int)) :returning :int)


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
