;;; Please do not edit this _GENERATED_ file.


(in-package :org.langband.ffi)
(ffi:def-c-type cptr c-string)

(ffi:def-c-type errr int)


(ffi:def-call-out c_current_ui (:name "current_ui") (:language :stdc) (:arguments ) (:return-type int))

(ffi:def-call-out c_quit! (:name "z_quit") (:language :stdc) (:arguments (msg
                                                                          c-string)) (:return-type nil))

(ffi:def-call-out c-clear-from! (:name "clear_from") (:language :stdc) (:arguments (row
                                                                                    int)) (:return-type nil))

(ffi:def-call-out c-prt-token! (:name "print_coloured_token") (:language :stdc) (:arguments (colour
                                                                                             int)(token
                                                                                                  int)(row
                                                                                                       int)(col
                                                                                                            int)) (:return-type nil))

(ffi:def-call-out c-prt-stat! (:name "print_coloured_stat") (:language :stdc) (:arguments (colour
                                                                                           int)(stat
                                                                                                int)(row
                                                                                                     int)(col
                                                                                                          int)) (:return-type nil))

(ffi:def-call-out c-prt-number! (:name "print_coloured_number") (:language :stdc) (:arguments (colour
                                                                                               int)(number
                                                                                                    long)(padding
                                                                                                          int)(row
                                                                                                               int)(col
                                                                                                                    int)) (:return-type nil))

(ffi:def-call-out c_term_putstr! (:name "my_Term_putstr") (:language :stdc) (:arguments (col
                                                                                         int)(row
                                                                                              int)(something
                                                                                                   int)(colour
                                                                                                        int)(text
                                                                                                             c-string)) (:return-type errr))

(ffi:def-call-out c_term_erase! (:name "Term_erase") (:language :stdc) (:arguments (col
                                                                                    int)(row
                                                                                         int)(something
                                                                                              int)) (:return-type errr))

(ffi:def-call-out c-term-queue-char! (:name "my_Term_queue_char") (:language :stdc) (:arguments (row
                                                                                                 int)(col
                                                                                                      int)(colour
                                                                                                           int)(the-char
                                                                                                                int)(tcolour
                                                                                                                     int)(tchar
                                                                                                                          int)) (:return-type nil))

(ffi:def-call-out c-term-gotoxy! (:name "Term_gotoxy") (:language :stdc) (:arguments (row
                                                                                      int)(col
                                                                                           int)) (:return-type nil))

(ffi:def-call-out c-set-cursor& (:name "my_Term_set_cursor") (:language :stdc) (:arguments (col
                                                                                            int)) (:return-type errr))

(ffi:def-call-out c-term-clear! (:name "Term_clear") (:language :stdc) (:arguments ) (:return-type errr))

(ffi:def-call-out c-term-flush! (:name "Term_flush") (:language :stdc) (:arguments ) (:return-type errr))

(ffi:def-call-out c-term-fresh! (:name "Term_fresh") (:language :stdc) (:arguments ) (:return-type errr))

(ffi:def-call-out c-term-save! (:name "Term_save") (:language :stdc) (:arguments ) (:return-type errr))

(ffi:def-call-out c-term-load! (:name "Term_load") (:language :stdc) (:arguments ) (:return-type errr))

(ffi:def-call-out c-term-xtra& (:name "Term_xtra") (:language :stdc) (:arguments (msg
                                                                                  int)(arg
                                                                                       int)) (:return-type errr))

(ffi:def-call-out c-inkey! (:name "inkey") (:language :stdc) (:arguments ) (:return-type char))

(ffi:def-call-out init_c-side& (:name "init_c_side") (:language :stdc) (:arguments (ui
                                                                                    c-string)(base-path
                                                                                              c-string)(debug-level
                                                                                                        int)) (:return-type errr))

(ffi:def-call-out cleanup-c-side& (:name "cleanup_c_side") (:language :stdc) (:arguments ) (:return-type errr))

(ffi:def-call-out c_macro_add& (:name "macro_add") (:language :stdc) (:arguments (key
                                                                                  c-string)(value
                                                                                            c-string)) (:return-type nil))

(ffi:def-call-out init-macro-system& (:name "macro_init") (:language :stdc) (:arguments ) (:return-type nil))

(ffi:def-call-out c-set-lisp-system! (:name "set_lisp_system") (:language :stdc) (:arguments (type
                                                                                              int)) (:return-type nil))


#+use-callback-from-c
(ffi:def-call-out c-set-lisp-callback! (:name "set_lisp_callback") (:language :stdc) (:arguments (name
                                                                                                  c-string)(ptr
                                                                                                            c-pointer)) (:return-type nil))

(ffi:def-call-out c-get-term-height (:name "get_term_height") (:language :stdc) (:arguments ) (:return-type int))

(ffi:def-call-out c-get-term-width (:name "get_term_width") (:language :stdc) (:arguments ) (:return-type int))


#+win32
(ffi:def-call-out c-set-hinst! (:name "setHINST") (:language :stdc) (:arguments (val
                                                                                 long)) (:return-type int))


#+using-sound
(ffi:def-call-out c-load-sound& (:name "load_sound") (:language :stdc) (:arguments (msg
                                                                                    int)(fname
                                                                                         c-string)) (:return-type errr))


#+image-support
(ffi:def-call-out paint-gfx-image& (:name "paint_gfx_image") (:language :stdc) (:arguments (fname
                                                                                            c-string)(type
                                                                                                      c-string)(x
                                                                                                                int)(y
                                                                                                                     int)) (:return-type int))


#+image-support
(ffi:def-call-out load-gfx-image& (:name "load_gfx_image") (:language :stdc) (:arguments (fname
                                                                                          c-string)(type
                                                                                                    c-string)) (:return-type int))


#+image-support
(ffi:def-call-out load-scaled-image& (:name "load_scaled_image") (:language :stdc) (:arguments (fname
                                                                                                c-string)(idx
                                                                                                          int)(wid
                                                                                                               int)(hgt
                                                                                                                    int)) (:return-type int))


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
