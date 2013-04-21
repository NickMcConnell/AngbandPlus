;;; Please do not edit this _GENERATED_ file.


(in-package :org.langband.ffi)
(fli:define-foreign-type cptr () ':pointer)

(fli:define-foreign-type errr () ':int)


(fli:define-foreign-function (c_current_ui "current_ui") nil
   :result-type :int :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c_quit! "z_quit") ((msg :pointer))
   :result-type :void :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-clear-from! "clear_from") ((row :int))
   :result-type :void :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (term-activate& "my_term_activate") ((term-num
                                                                   :int))
   :result-type :int :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-prt-token! "print_coloured_token") ((term :int)
                                                                    (colour
                                                                     :int)
                                                                    (token
                                                                     :int)
                                                                    (row :int)
                                                                    (col :int))
   :result-type :void :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-prt-stat! "print_coloured_stat") ((term :int)
                                                                  (colour :int)
                                                                  (stat :int)
                                                                  (row :int)
                                                                  (col :int))
   :result-type :void :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-prt-number! "print_coloured_number") ((term
                                                                       :int)
                                                                      (colour
                                                                       :int)
                                                                      (number
                                                                       :long)
                                                                      (padding
                                                                       :int)
                                                                      (row
                                                                       :int)
                                                                      (col
                                                                       :int))
   :result-type :void :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c_term_putstr! "my_Term_putstr") ((col :int)
                                                                (row :int)
                                                                (something
                                                                 :int)
                                                                (colour :int)
                                                                (text :pointer))
   :result-type errr :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c_term_erase! "Term_erase") ((col :int)
                                                           (row :int)
                                                           (something :int))
   :result-type errr :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-term-queue-char! "my_Term_queue_char") ((col
                                                                         :int)
                                                                        (row
                                                                         :int)
                                                                        (colour
                                                                         :int)
                                                                        (the-char
                                                                         :int)
                                                                        (tcolour
                                                                         :int)
                                                                        (tchar
                                                                         :int))
   :result-type :void :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-term-gotoxy! "Term_gotoxy") ((row :int)
                                                             (col :int))
   :result-type :void :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-set-cursor& "my_Term_set_cursor") ((col :int))
   :result-type errr :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-term-clear! "Term_clear") nil
   :result-type errr :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-term-flush! "Term_flush") nil
   :result-type errr :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-term_fresh! "Term_fresh") nil
   :result-type errr :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-term-save! "Term_save") nil
   :result-type errr :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-term-load! "Term_load") nil
   :result-type errr :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-term-xtra& "Term_xtra") ((msg :int) (arg :int))
   :result-type errr :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-term-keypress "Term_keypress") ((key :int))
   :result-type errr :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-inkey! "inkey") nil
   :result-type :char :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (init_c-side& "init_c_side") ((ui :pointer)
                                                           (source-path
                                                            :pointer)
                                                           (config-path
                                                            :pointer)
                                                           (gfx-path :pointer)
                                                           (flags :int))
   :result-type errr :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (cleanup-c-side& "cleanup_c_side") nil
   :result-type errr :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c_macro_add& "macro_add") ((key :pointer)
                                                         (value :pointer))
   :result-type :void :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (init-macro-system& "macro_init") nil
   :result-type :void :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-set-lisp-system! "set_lisp_system") ((type
                                                                      :int))
   :result-type :void :language :c :calling-convention :stdcall :module :lang-ffi)


#+use-callback-from-c
(fli:define-foreign-function (c-set-lisp-callback! "set_lisp_callback") ((name
                                                                          :pointer)
                                                                         (ptr
                                                                          :ptr))
   :result-type :void :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-get-cur-term "my_get_current_term") nil
   :result-type :int :language :c :calling-convention :stdcall :module :lang-ffi)


#+win32
(fli:define-foreign-function (c-set-hinst! "setHINST") ((val :long))
   :result-type :int :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-init-sound-system& "init_sound_system") ((size
                                                                          :int))
   :result-type :int :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-load_sound-effect& "load_sound_effect") ((fname
                                                                          :pointer)
                                                                         (idx
                                                                          :int))
   :result-type errr :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-get-sound-status "get_sound_status") nil
   :result-type :int :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-paint-gfx-image& "paint_gfx_image") ((fname
                                                                      :pointer)
                                                                     (type
                                                                      :pointer)
                                                                     (x :int)
                                                                     (y :int))
   :result-type :int :language :c :calling-convention :stdcall :module :lang-ffi)


#+image-support
(fli:define-foreign-function (load-gfx-image& "load_gfx_image") ((fname
                                                                  :pointer)
                                                                 (type
                                                                  :pointer))
   :result-type :int :language :c :calling-convention :stdcall :module :lang-ffi)


#+image-support
(fli:define-foreign-function (load-scaled-image& "load_scaled_image") ((fname
                                                                        :pointer)
                                                                       (idx
                                                                        :int)
                                                                       (wid
                                                                        :int)
                                                                       (hgt
                                                                        :int))
   :result-type :int :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-texture_background! "textureBackground") ((term
                                                                           :int)
                                                                          (fname
                                                                           :pointer)
                                                                          (alpha
                                                                           :int))
   :result-type :int :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-init-frame-system& "init_frame_system") ((act-size
                                                                          :int)
                                                                         (pre-size
                                                                          :int))
   :result-type :int :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-add_frame! "add_frame") ((key :int)
                                                         (name :pointer))
   :result-type :int :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-add-frame-coords! "add_frame_coords") ((key
                                                                        :int)
                                                                       (x :int)
                                                                       (y :int)
                                                                       (w :int)
                                                                       (h :int))
   :result-type :int :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-add_frame-tileinfo! "add_frame_tileinfo") ((key
                                                                            :int)
                                                                           (tw
                                                                            :int)
                                                                           (th
                                                                            :int)
                                                                           (font
                                                                            :pointer)
                                                                           (bg
                                                                            :pointer))
   :result-type :int :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-add-frame-gfxinfo! "add_frame_gfxinfo") ((key
                                                                          :int)
                                                                         (use-tiles
                                                                          :int))
   :result-type :int :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-has_frame "has_frame") ((key :int) (type :int))
   :result-type :int :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-activate-frame! "activate_frame") ((key :int))
   :result-type :int :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-deactivate-frame! "deactivate_frame") ((key
                                                                        :int))
   :result-type :int :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-clean-frame! "clean_frame") ((key :int))
   :result-type :int :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-wipe-frame! "wipe_frame") ((key :int))
   :result-type :int :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-get-frame-columns "get_frame_columns") ((key
                                                                         :int)
                                                                        (type
                                                                         :int))
   :result-type :int :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-get-frame-rows "get_frame_rows") ((key :int)
                                                                  (type :int))
   :result-type :int :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-get-frame-tile-width "get_frame_tile_width") ((key
                                                                               :int)
                                                                              (type
                                                                               :int))
   :result-type :int :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-get-frame-tile-height "get_frame_tile_height") ((key
                                                                                 :int)
                                                                                (type
                                                                                 :int))
   :result-type :int :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-get_frame-gfx-tiles "get_frame_gfx_tiles") ((key
                                                                             :int)
                                                                            (type
                                                                             :int))
   :result-type :int :language :c :calling-convention :stdcall :module :lang-ffi)


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
