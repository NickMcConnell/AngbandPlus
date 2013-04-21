;;; Please do not edit this _GENERATED_ file.


(in-package :langband-ffi)
(ffi:def-c-type byte uchar)

(export 'byte)
(ffi:def-c-type cptr c-string)

(export 'cptr)
(ffi:def-c-type errr int)

(export 'errr)

(ffi:def-call-out c-quit! (:name "z_quit") (:language :stdc) (:arguments (msg
                                                                          c-string)) (:return-type nil))

(export 'c-quit!)
(ffi:def-call-out c-bell! (:name "bell") (:language :stdc) (:arguments (msg
                                                                        c-string)) (:return-type nil))

(export 'c-bell!)
(ffi:def-call-out c-pause-line! (:name "pause_line") (:language :stdc) (:arguments (row
                                                                                    int)) (:return-type nil))

(export 'c-pause-line!)
(ffi:def-call-out c-clear-from! (:name "clear_from") (:language :stdc) (:arguments (row
                                                                                    int)) (:return-type nil))

(export 'c-clear-from!)
(ffi:def-call-out c-prt! (:name "prt") (:language :stdc) (:arguments (text
                                                                      c-string)(row
                                                                                int)(col
                                                                                     int)) (:return-type nil))

(export 'c-prt!)
(ffi:def-call-out c-put-str! (:name "put_str") (:language :stdc) (:arguments (text
                                                                              c-string)(row
                                                                                        int)(col
                                                                                             int)) (:return-type nil))

(export 'c-put-str!)
(ffi:def-call-out c-col-put-str! (:name "c_put_str") (:language :stdc) (:arguments (colour
                                                                                    byte)(text
                                                                                          c-string)(row
                                                                                                    int)(col
                                                                                                         int)) (:return-type nil))

(export 'c-col-put-str!)
(ffi:def-call-out c-prt-token! (:name "print_coloured_token") (:language :stdc) (:arguments (colour
                                                                                             byte)(token
                                                                                                   int)(row
                                                                                                        int)(col
                                                                                                             int)) (:return-type nil))

(export 'c-prt-token!)
(ffi:def-call-out c-prt-stat! (:name "print_coloured_stat") (:language :stdc) (:arguments (colour
                                                                                           byte)(stat
                                                                                                 int)(row
                                                                                                      int)(col
                                                                                                           int)) (:return-type nil))

(export 'c-prt-stat!)
(ffi:def-call-out c-prt-number! (:name "print_coloured_number") (:language :stdc) (:arguments (colour
                                                                                               byte)(number
                                                                                                     long)(padding
                                                                                                           int)(row
                                                                                                                int)(col
                                                                                                                     int)) (:return-type nil))

(export 'c-prt-number!)
(ffi:def-call-out c-print-message! (:name "msg_print") (:language :stdc) (:arguments (msg
                                                                                      c-string)) (:return-type nil))

(export 'c-print-message!)
(ffi:def-call-out c-term-putstr! (:name "Term_putstr") (:language :stdc) (:arguments (row
                                                                                      int)(col
                                                                                           int)(something
                                                                                                int)(colour
                                                                                                     byte)(text
                                                                                                           c-string)) (:return-type errr))

(export 'c-term-putstr!)
(ffi:def-call-out c-term-queue-char! (:name "Term_queue_char") (:language :stdc) (:arguments (row
                                                                                              int)(col
                                                                                                   int)(colour
                                                                                                        byte)(text
                                                                                                              char)) (:return-type nil))

(export 'c-term-queue-char!)
(ffi:def-call-out c-term-gotoxy! (:name "Term_gotoxy") (:language :stdc) (:arguments (row
                                                                                      int)(col
                                                                                           int)) (:return-type nil))

(export 'c-term-gotoxy!)
(ffi:def-call-out c-set-cursor& (:name "Term_set_cursor") (:language :stdc) (:arguments (col
                                                                                         int)) (:return-type errr))

(export 'c-set-cursor&)
(ffi:def-call-out c-term-clear! (:name "Term_clear") (:language :stdc) (:arguments ) (:return-type errr))

(export 'c-term-clear!)
(ffi:def-call-out c-term-fresh! (:name "Term_fresh") (:language :stdc) (:arguments ) (:return-type errr))

(export 'c-term-fresh!)
(ffi:def-call-out c-term-save! (:name "Term_save") (:language :stdc) (:arguments ) (:return-type errr))

(export 'c-term-save!)
(ffi:def-call-out c-term-load! (:name "Term_load") (:language :stdc) (:arguments ) (:return-type errr))

(export 'c-term-load!)
(ffi:def-call-out c-term-xtra& (:name "Term_xtra") (:language :stdc) (:arguments (msg
                                                                                  int)(arg
                                                                                       int)) (:return-type errr))

(export 'c-term-xtra&)
(ffi:def-call-out c-term-inkey& (:name "Term_inkey") (:language :stdc) (:arguments (text
                                                                                    c-string)(row
                                                                                              int)(col
                                                                                                   int)) (:return-type errr))

(export 'c-term-inkey&)
(ffi:def-call-out c-inkey! (:name "inkey") (:language :stdc) (:arguments ) (:return-type char))

(export 'c-inkey!)
(ffi:def-call-out c-init-gui! (:name "init_gui") (:language :stdc) (:arguments (argc
                                                                                int)(argv
                                                                                     (c-ptr
                                                                                      c-string))) (:return-type errr))

(export 'c-init-gui!)
(ffi:def-call-out c-init.angband! (:name "init_angband") (:language :stdc) (:arguments ) (:return-type nil))

(export 'c-init.angband!)
(ffi:def-call-out c-macro-add& (:name "macro_add") (:language :stdc) (:arguments (key
                                                                                  c-string)(value
                                                                                            c-string)) (:return-type nil))

(export 'c-macro-add&)

#+use-callback-from-c
(ffi:def-call-out set_lisp_callback! (:name "set_lisp_callback") (:language :stdc) (:arguments (ptr
                                                                                                c-pointer)) (:return-type nil))

#+use-callback-from-c

(export 'set_lisp_callback!)

#+using-sound
(ffi:def-call-out c-load-sound& (:name "load_sound") (:language :stdc) (:arguments (msg
                                                                                    int)(fname
                                                                                         c-string)) (:return-type errr))

#+using-sound

(export 'c-load-sound&)

;;; End of generated file.
