;;; Please do not edit this _GENERATED_ file.


(in-package :org.langband.ffi)
(ffi:def-c-type cptr c-string)

(ffi:def-c-type errr int)


(ffi:def-call-out c_current_ui (:name "current_ui") (:language :stdc) (:arguments ) (:return-type int))

(ffi:def-call-out c_quit! (:name "z_quit") (:language :stdc) (:arguments (msg
                                                                          c-string)) (:return-type nil))

(ffi:def-call-out c-clear-from! (:name "clear_from") (:language :stdc) (:arguments (row
                                                                                    int)) (:return-type nil))

(ffi:def-call-out term-activate& (:name "my_term_activate") (:language :stdc) (:arguments (term-num
                                                                                           int)) (:return-type int))

(ffi:def-call-out c-prt-token! (:name "print_coloured_token") (:language :stdc) (:arguments (term
                                                                                             int)(colour
                                                                                                  int)(token
                                                                                                       int)(row
                                                                                                            int)(col
                                                                                                                 int)) (:return-type nil))

(ffi:def-call-out c-prt-stat! (:name "print_coloured_stat") (:language :stdc) (:arguments (term
                                                                                           int)(colour
                                                                                                int)(stat
                                                                                                     int)(row
                                                                                                          int)(col
                                                                                                               int)) (:return-type nil))

(ffi:def-call-out c-prt-number! (:name "print_coloured_number") (:language :stdc) (:arguments (term
                                                                                               int)(colour
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

(ffi:def-call-out c-term-queue-char! (:name "my_Term_queue_char") (:language :stdc) (:arguments (col
                                                                                                 int)(row
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

(ffi:def-call-out c-term_fresh! (:name "Term_fresh") (:language :stdc) (:arguments ) (:return-type errr))

(ffi:def-call-out c-term-save! (:name "Term_save") (:language :stdc) (:arguments ) (:return-type errr))

(ffi:def-call-out c-term-load! (:name "Term_load") (:language :stdc) (:arguments ) (:return-type errr))

(ffi:def-call-out c-term-xtra& (:name "Term_xtra") (:language :stdc) (:arguments (msg
                                                                                  int)(arg
                                                                                       int)) (:return-type errr))

(ffi:def-call-out c-term-keypress (:name "Term_keypress") (:language :stdc) (:arguments (key
                                                                                         int)) (:return-type errr))

(ffi:def-call-out c-inkey! (:name "inkey") (:language :stdc) (:arguments ) (:return-type char))

(ffi:def-call-out init_c-side& (:name "init_c_side") (:language :stdc) (:arguments (ui
                                                                                    c-string)(source-path
                                                                                              c-string)(config-path
                                                                                                        c-string)(gfx-path
                                                                                                                  c-string)(flags
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

(ffi:def-call-out c-get-cur-term (:name "my_get_current_term") (:language :stdc) (:arguments ) (:return-type int))


#+win32
(ffi:def-call-out c-set-hinst! (:name "setHINST") (:language :stdc) (:arguments (val
                                                                                 long)) (:return-type int))

(ffi:def-call-out c-init-sound-system& (:name "init_sound_system") (:language :stdc) (:arguments (size
                                                                                                  int)) (:return-type int))

(ffi:def-call-out c-load_sound-effect& (:name "load_sound_effect") (:language :stdc) (:arguments (fname
                                                                                                  c-string)(idx
                                                                                                            int)) (:return-type errr))

(ffi:def-call-out c-get-sound-status (:name "get_sound_status") (:language :stdc) (:arguments ) (:return-type int))

(ffi:def-call-out c-paint-gfx-image& (:name "paint_gfx_image") (:language :stdc) (:arguments (fname
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

(ffi:def-call-out c-texture_background! (:name "textureBackground") (:language :stdc) (:arguments (term
                                                                                                   int)(fname
                                                                                                        c-string)(alpha
                                                                                                                  int)) (:return-type int))

(ffi:def-call-out c-init-frame-system& (:name "init_frame_system") (:language :stdc) (:arguments (act-size
                                                                                                  int)(pre-size
                                                                                                       int)) (:return-type int))

(ffi:def-call-out c-add_frame! (:name "add_frame") (:language :stdc) (:arguments (key
                                                                                  int)(name
                                                                                       c-string)) (:return-type int))

(ffi:def-call-out c-add-frame-coords! (:name "add_frame_coords") (:language :stdc) (:arguments (key
                                                                                                int)(x
                                                                                                     int)(y
                                                                                                          int)(w
                                                                                                               int)(h
                                                                                                                    int)) (:return-type int))

(ffi:def-call-out c-add_frame-tileinfo! (:name "add_frame_tileinfo") (:language :stdc) (:arguments (key
                                                                                                    int)(tw
                                                                                                         int)(th
                                                                                                              int)(font
                                                                                                                   c-string)(bg
                                                                                                                             c-string)) (:return-type int))

(ffi:def-call-out c-add-frame-gfxinfo! (:name "add_frame_gfxinfo") (:language :stdc) (:arguments (key
                                                                                                  int)(use-tiles
                                                                                                       int)) (:return-type int))

(ffi:def-call-out c-has_frame (:name "has_frame") (:language :stdc) (:arguments (key
                                                                                 int)(type
                                                                                      int)) (:return-type int))

(ffi:def-call-out c-activate-frame! (:name "activate_frame") (:language :stdc) (:arguments (key
                                                                                            int)) (:return-type int))

(ffi:def-call-out c-deactivate-frame! (:name "deactivate_frame") (:language :stdc) (:arguments (key
                                                                                                int)) (:return-type int))

(ffi:def-call-out c-clean-frame! (:name "clean_frame") (:language :stdc) (:arguments (key
                                                                                      int)) (:return-type int))

(ffi:def-call-out c-wipe-frame! (:name "wipe_frame") (:language :stdc) (:arguments (key
                                                                                    int)) (:return-type int))

(ffi:def-call-out c-get-frame-columns (:name "get_frame_columns") (:language :stdc) (:arguments (key
                                                                                                 int)(type
                                                                                                      int)) (:return-type int))

(ffi:def-call-out c-get-frame-rows (:name "get_frame_rows") (:language :stdc) (:arguments (key
                                                                                           int)(type
                                                                                                int)) (:return-type int))

(ffi:def-call-out c-get-frame-tile-width (:name "get_frame_tile_width") (:language :stdc) (:arguments (key
                                                                                                       int)(type
                                                                                                            int)) (:return-type int))

(ffi:def-call-out c-get-frame-tile-height (:name "get_frame_tile_height") (:language :stdc) (:arguments (key
                                                                                                         int)(type
                                                                                                              int)) (:return-type int))

(ffi:def-call-out c-get_frame-gfx-tiles (:name "get_frame_gfx_tiles") (:language :stdc) (:arguments (key
                                                                                                     int)(type
                                                                                                          int)) (:return-type int))


(eval-when (:execute :load-toplevel :compile-toplevel)
  (export
   '(c_current_ui c_quit! c-clear-from! term-activate& c-prt-token! c-prt-stat!
     c-prt-number! c_term_putstr! c_term_erase! c-term-queue-char!
     c-term-gotoxy! c-set-cursor& c-term-clear! c-term-flush! c-term_fresh!
     c-term-save! c-term-load! c-term-xtra& c-term-keypress c-inkey!
     init_c-side& cleanup-c-side& c_macro_add& init-macro-system&
     c-set-lisp-system! c-set-lisp-callback! c-get-cur-term c-set-hinst!
     c-init-sound-system& c-load_sound-effect& c-get-sound-status
     c-paint-gfx-image& load-gfx-image& load-scaled-image&
     c-texture_background! c-init-frame-system& c-add_frame!
     c-add-frame-coords! c-add_frame-tileinfo! c-add-frame-gfxinfo! c-has_frame
     c-activate-frame! c-deactivate-frame! c-clean-frame! c-wipe-frame!
     c-get-frame-columns c-get-frame-rows c-get-frame-tile-width
     c-get-frame-tile-height c-get_frame-gfx-tiles)))

;;; End of generated file.
