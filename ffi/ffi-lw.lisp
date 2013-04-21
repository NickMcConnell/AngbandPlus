;;; Please do not edit this _GENERATED_ file.


(in-package :org.langband.ffi)
(fli:define-foreign-type cptr () '(:reference-pass :ef-mb-string))


(fli:define-foreign-function (c_current_ui "current_ui") nil
   :result-type :int :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-listen-for-event "listenForEvent") ((option
                                                                     :int))
   :result-type :int :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (init-c-side& "init_c_side") ((ui
                                                            (:reference-pass
                                                             :ef-mb-string))
                                                           (source-path
                                                            (:reference-pass
                                                             :ef-mb-string))
                                                           (config-path
                                                            (:reference-pass
                                                             :ef-mb-string))
                                                           (data-path
                                                            (:reference-pass
                                                             :ef-mb-string))
                                                           (flags :int))
   :result-type :int :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (cleanup-c-side& "cleanup_c_side") nil
   :result-type :int :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-set-lisp-system! "set_lisp_system") ((type
                                                                      :int))
   :result-type :void :language :c :calling-convention :stdcall :module :lang-ffi)


#+use-callback-from-c
(fli:define-foreign-function (c-set-lisp-callback! "set_lisp_callback") ((name
                                                                          (:reference-pass
                                                                           :ef-mb-string))
                                                                         (ptr
                                                                          :ptr))
   :result-type :void :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-init-sound-system& "init_sound_system") ((size
                                                                          :int))
   :result-type :int :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-get-sound-status "get_sound_status") nil
   :result-type :int :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-load-sound-effect& "load_sound_effect") ((fname
                                                                          (:reference-pass
                                                                           :ef-mb-string))
                                                                         (idx
                                                                          :int))
   :result-type :int :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-play-sound-effect "play_sound_effect") ((idx
                                                                         :int))
   :result-type :int :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-load-music-file& "load_music_file") ((fname
                                                                      (:reference-pass
                                                                       :ef-mb-string))
                                                                     (idx :int))
   :result-type :int :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-play-music-file "play_music_file") ((idx :int))
   :result-type :int :language :c :calling-convention :stdcall :module :lang-ffi)


#+image-support
(fli:define-foreign-function (load-gfx-image& "load_gfx_image") ((fname
                                                                  (:reference-pass
                                                                   :ef-mb-string))
                                                                 (idx :int)
                                                                 (transcolour
                                                                  :unsigned))
   :result-type :int :language :c :calling-convention :stdcall :module :lang-ffi)


#+image-support
(fli:define-foreign-function (c-load-texture& "load_texture") ((idx :int)
                                                               (fname
                                                                (:reference-pass
                                                                 :ef-mb-string))
                                                               (twid :int)
                                                               (thgt :int)
                                                               (alpha
                                                                :unsigned))
   :result-type :int :language :c :calling-convention :stdcall :module :lang-ffi)


#+image-support
(fli:define-foreign-function (c-get-image-width "get_image_width") ((idx :int))
   :result-type :int :language :c :calling-convention :stdcall :module :lang-ffi)


#+image-support
(fli:define-foreign-function (c-get-image-height "get_image_height") ((idx
                                                                       :int))
   :result-type :int :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-init-frame-system& "init_frame_system") ((act-size
                                                                          :int)
                                                                         (pre-size
                                                                          :int))
   :result-type :int :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-add-frame! "add_frame") ((key :int)
                                                         (name
                                                          (:reference-pass
                                                           :ef-mb-string)))
   :result-type :int :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-add-frame-coords! "add_frame_coords") ((key
                                                                        :int)
                                                                       (x :int)
                                                                       (y :int)
                                                                       (w :int)
                                                                       (h :int))
   :result-type :int :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-add-frame-tileinfo! "add_frame_tileinfo") ((key
                                                                            :int)
                                                                           (tw
                                                                            :int)
                                                                           (th
                                                                            :int)
                                                                           (font
                                                                            (:reference-pass
                                                                             :ef-mb-string)))
   :result-type :int :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-add-frame-gfxinfo! "add_frame_gfxinfo") ((key
                                                                          :int)
                                                                         (use-tiles
                                                                          :int))
   :result-type :int :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-add-frame-bg! "add_frame_bg") ((key :int)
                                                               (img-idx :int))
   :result-type :int :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-has_frame "has_frame") ((key :int) (type :int))
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

(fli:define-foreign-function (c-full-blit "exp_full_blit") ((num :short)
                                                            (x :short)
                                                            (y :short)
                                                            (img :unsigned)
                                                            (flag :short))
   :result-type :int :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-transparent-blit "exp_transparent_blit") ((num
                                                                           :short)
                                                                          (x
                                                                           :short)
                                                                          (y
                                                                           :short)
                                                                          (img
                                                                           :unsigned)
                                                                          (flag
                                                                           :short))
   :result-type :int :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-clear-coords! "exp_clear_coords") ((num :short)
                                                                   (x :short)
                                                                   (y :short)
                                                                   (w :short)
                                                                   (h :short))
   :result-type :int :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-flush-coords! "exp_flush_coords") ((num :short)
                                                                   (x :short)
                                                                   (y :short)
                                                                   (w :short)
                                                                   (h :short))
   :result-type :int :language :c :calling-convention :stdcall :module :lang-ffi)


(eval-when (:execute :load-toplevel :compile-toplevel)
  (export
   '(c_current_ui c-listen-for-event init-c-side& cleanup-c-side&
     c-set-lisp-system! c-set-lisp-callback! c-init-sound-system&
     c-get-sound-status c-load-sound-effect& c-play-sound-effect
     c-load-music-file& c-play-music-file load-gfx-image& c-load-texture&
     c-get-image-width c-get-image-height c-init-frame-system& c-add-frame!
     c-add-frame-coords! c-add-frame-tileinfo! c-add-frame-gfxinfo!
     c-add-frame-bg! c-has_frame c-get-frame-columns c-get-frame-rows
     c-get-frame-tile-width c-get-frame-tile-height c-get_frame-gfx-tiles
     c-full-blit c-transparent-blit c-clear-coords! c-flush-coords!)))

;;; End of generated file.
