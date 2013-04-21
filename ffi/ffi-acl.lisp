;;; Please do not edit this _GENERATED_ file.


(in-package :org.langband.ffi)
(ff:def-foreign-type cptr (* :char))

(ff:def-foreign-type errr :int)


(ff:def-foreign-call (c_current_ui "current_ui") nil :returning :int)

(ff:def-foreign-call (c_quit! "z_quit") ((msg (* :void))) :returning :void)

(ff:def-foreign-call (c-clear-from! "clear_from") ((row :int)) :returning :void)

(ff:def-foreign-call (term-activate& "my_term_activate") ((term-num :int)) :returning :int)

(ff:def-foreign-call (c-prt-token! "print_coloured_token") ((term :int)
                                                            (colour :int)
                                                            (token :int)
                                                            (row :int)
                                                            (col :int)) :returning :void)

(ff:def-foreign-call (c-prt-stat! "print_coloured_stat") ((term :int)
                                                          (colour :int)
                                                          (stat :int)
                                                          (row :int) (col :int)) :returning :void)

(ff:def-foreign-call (c-prt-number! "print_coloured_number") ((term :int)
                                                              (colour :int)
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

(ff:def-foreign-call (c-term-queue-char! "my_Term_queue_char") ((col :int)
                                                                (row :int)
                                                                (colour :int)
                                                                (the-char :int)
                                                                (tcolour :int)
                                                                (tchar :int)) :returning :void)

(ff:def-foreign-call (c-term-gotoxy! "Term_gotoxy") ((row :int) (col :int)) :returning :void)

(ff:def-foreign-call (c-set-cursor& "my_Term_set_cursor") ((col :int)) :returning errr)

(ff:def-foreign-call (c-term-clear! "Term_clear") nil :returning errr)

(ff:def-foreign-call (c-term-flush! "Term_flush") nil :returning errr)

(ff:def-foreign-call (c-term_fresh! "Term_fresh") nil :returning errr)

(ff:def-foreign-call (c-term-save! "Term_save") nil :returning errr)

(ff:def-foreign-call (c-term-load! "Term_load") nil :returning errr)

(ff:def-foreign-call (c-term-xtra& "Term_xtra") ((msg :int) (arg :int)) :returning errr)

(ff:def-foreign-call (c-term-keypress "Term_keypress") ((key :int)) :returning errr)

(ff:def-foreign-call (c-inkey! "inkey") nil :returning :char)

(ff:def-foreign-call (init_c-side& "init_c_side") ((ui cptr) (source-path cptr)
                                                   (config-path cptr)
                                                   (gfx-path cptr) (flags :int)) :returning errr)

(ff:def-foreign-call (cleanup-c-side& "cleanup_c_side") nil :returning errr)

(ff:def-foreign-call (c_macro_add& "macro_add") ((key cptr) (value cptr)) :returning :void)

(ff:def-foreign-call (init-macro-system& "macro_init") nil :returning :void)

(ff:def-foreign-call (c-set-lisp-system! "set_lisp_system") ((type :int)) :returning :void)


#+use-callback-from-c
(ff:def-foreign-call (c-set-lisp-callback! "set_lisp_callback") ((name cptr)
                                                                 (ptr)) :returning :void)

(ff:def-foreign-call (c-get-cur-term "my_get_current_term") nil :returning :int)


#+win32
(ff:def-foreign-call (c-set-hinst! "setHINST") ((val :long)) :returning :int)

(ff:def-foreign-call (c-init-sound-system& "init_sound_system") ((size :int)) :returning :int)

(ff:def-foreign-call (c-load_sound-effect& "load_sound_effect") ((fname cptr)
                                                                 (idx :int)) :returning errr)

(ff:def-foreign-call (c-get-sound-status "get_sound_status") nil :returning :int)

(ff:def-foreign-call (c-paint-gfx-image& "paint_gfx_image") ((fname cptr)
                                                             (type cptr)
                                                             (x :int) (y :int)) :returning :int)


#+image-support
(ff:def-foreign-call (load-gfx-image& "load_gfx_image") ((fname cptr)
                                                         (type cptr)) :returning :int)


#+image-support
(ff:def-foreign-call (load-scaled-image& "load_scaled_image") ((fname cptr)
                                                               (idx :int)
                                                               (wid :int)
                                                               (hgt :int)) :returning :int)

(ff:def-foreign-call (c-texture_background! "textureBackground") ((term :int)
                                                                  (fname cptr)
                                                                  (alpha :int)) :returning :int)

(ff:def-foreign-call (c-init-frame-system& "init_frame_system") ((act-size
                                                                  :int)
                                                                 (pre-size
                                                                  :int)) :returning :int)

(ff:def-foreign-call (c-add_frame! "add_frame") ((key :int) (name cptr)) :returning :int)

(ff:def-foreign-call (c-add-frame-coords! "add_frame_coords") ((key :int)
                                                               (x :int)
                                                               (y :int)
                                                               (w :int)
                                                               (h :int)) :returning :int)

(ff:def-foreign-call (c-add_frame-tileinfo! "add_frame_tileinfo") ((key :int)
                                                                   (tw :int)
                                                                   (th :int)
                                                                   (font cptr)
                                                                   (bg cptr)) :returning :int)

(ff:def-foreign-call (c-add-frame-gfxinfo! "add_frame_gfxinfo") ((key :int)
                                                                 (use-tiles
                                                                  :int)) :returning :int)

(ff:def-foreign-call (c-has_frame "has_frame") ((key :int) (type :int)) :returning :int)

(ff:def-foreign-call (c-activate-frame! "activate_frame") ((key :int)) :returning :int)

(ff:def-foreign-call (c-deactivate-frame! "deactivate_frame") ((key :int)) :returning :int)

(ff:def-foreign-call (c-clean-frame! "clean_frame") ((key :int)) :returning :int)

(ff:def-foreign-call (c-wipe-frame! "wipe_frame") ((key :int)) :returning :int)

(ff:def-foreign-call (c-get-frame-columns "get_frame_columns") ((key :int)
                                                                (type :int)) :returning :int)

(ff:def-foreign-call (c-get-frame-rows "get_frame_rows") ((key :int)
                                                          (type :int)) :returning :int)

(ff:def-foreign-call (c-get-frame-tile-width "get_frame_tile_width") ((key
                                                                       :int)
                                                                      (type
                                                                       :int)) :returning :int)

(ff:def-foreign-call (c-get-frame-tile-height "get_frame_tile_height") ((key
                                                                         :int)
                                                                        (type
                                                                         :int)) :returning :int)

(ff:def-foreign-call (c-get_frame-gfx-tiles "get_frame_gfx_tiles") ((key :int)
                                                                    (type :int)) :returning :int)


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
