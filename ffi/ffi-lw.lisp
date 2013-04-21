;;; Please do not edit this _GENERATED_ file.


(in-package :org.langband.ffi)
(fli:define-foreign-type cptr () '(:reference-pass :ef-mb-string))


(fli:define-foreign-function (c_current_ui "lbui_current_ui")
    nil
    :result-type :int :language :c 
    :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-listen-for-event "lbui_listen_for_event")
    ((option :int))
    :result-type :int :language :c 
    :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-init-c-side& "lbui_init_c_side")
    ((ui (:reference-pass :ef-mb-string))
     (source-path (:reference-pass :ef-mb-string))
     (config-path (:reference-pass :ef-mb-string))
     (data-path (:reference-pass :ef-mb-string)) (flags :int))
    :result-type :int :language :c 
    :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-cleanup-c-side& "lbui_cleanup_c_side")
    nil
    :result-type :int :language :c 
    :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-set-lisp-system! "lbui_set_lisp_system")
    ((type :int))
    :result-type :void :language :c 
    :calling-convention :stdcall :module :lang-ffi)


#+use-callback-from-c
(fli:define-foreign-function (c-set-lisp-callback! "lbui_set_lisp_callback")
    ((name (:reference-pass :ef-mb-string)) (ptr :ptr))
    :result-type :void :language :c 
    :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-init-sound-system& "lbui_init_sound_system")
    ((size :int))
    :result-type :int :language :c 
    :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-activate-sound-system& "lbui_activate_sound_system")
    nil
    :result-type :int :language :c 
    :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-get-sound-status "lbui_get_sound_status")
    nil
    :result-type :int :language :c 
    :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-load-sound-effect& "lbui_load_sound_effect")
    ((fname (:reference-pass :ef-mb-string)) (idx :int))
    :result-type :int :language :c 
    :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-play-sound-effect "lbui_play_sound_effect")
    ((idx :int) (channel :short) (loops :short))
    :result-type :int :language :c 
    :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-halt-sound-effects "lbui_halt_sound_effects")
    ((channel :short))
    :result-type :int :language :c 
    :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-load-music-file& "lbui_load_music_file")
    ((fname (:reference-pass :ef-mb-string)) (idx :int))
    :result-type :int :language :c 
    :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-play-music-file "lbui_play_music_file")
    ((idx :int) (loops :short))
    :result-type :int :language :c 
    :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-halt-music "lbui_halt_music")
    nil
    :result-type :int :language :c 
    :calling-convention :stdcall :module :lang-ffi)


#+image-support
(fli:define-foreign-function (load-gfx-image& "lbui_load_gfx_image")
    ((fname (:reference-pass :ef-mb-string)) (idx :int) (transcolour :unsigned))
    :result-type :int :language :c 
    :calling-convention :stdcall :module :lang-ffi)


#+image-support
(fli:define-foreign-function (c-load-texture& "lbui_load_texture")
    ((idx :int) (fname (:reference-pass :ef-mb-string)) (twid :int) (thgt :int)
     (alpha :unsigned))
    :result-type :int :language :c 
    :calling-convention :stdcall :module :lang-ffi)


#+image-support
(fli:define-foreign-function (c-get-image-width "lbui_get_image_width")
    ((idx :int))
    :result-type :int :language :c 
    :calling-convention :stdcall :module :lang-ffi)


#+image-support
(fli:define-foreign-function (c-get-image-height "lbui_get_image_height")
    ((idx :int))
    :result-type :int :language :c 
    :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-init-frame-system& "lbui_init_frame_system")
    ((act-size :int) (pre-size :int))
    :result-type :int :language :c 
    :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-add-frame! "lbui_add_frame")
    ((key :int) (name (:reference-pass :ef-mb-string)))
    :result-type :int :language :c 
    :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-add-frame-coords! "lbui_add_frame_coords")
    ((key :int) (x :int) (y :int) (w :int) (h :int))
    :result-type :int :language :c 
    :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-add-frame-tileinfo! "lbui_add_frame_tileinfo")
    ((key :int) (tw :int) (th :int))
    :result-type :int :language :c 
    :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-add-frame-fontinfo! "lbui_add_frame_fontinfo")
    ((key :int) (font (:reference-pass :ef-mb-string)) (ptsize :int)
     (style :int))
    :result-type :int :language :c 
    :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-add-frame-gfxinfo! "lbui_add_frame_gfxinfo")
    ((key :int) (use-tiles :int))
    :result-type :int :language :c 
    :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-add-frame-bg! "lbui_add_frame_bg")
    ((key :int) (img-idx :int))
    :result-type :int :language :c 
    :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-has_frame "lbui_has_frame")
    ((key :int) (type :int))
    :result-type :int :language :c 
    :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-get-frame-columns "lbui_get_frame_columns")
    ((key :int) (type :int))
    :result-type :int :language :c 
    :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-get-frame-rows "lbui_get_frame_rows")
    ((key :int) (type :int))
    :result-type :int :language :c 
    :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-get-frame-tile-width "lbui_get_frame_tile_width")
    ((key :int) (type :int))
    :result-type :int :language :c 
    :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-get-frame-tile-height "lbui_get_frame_tile_height")
    ((key :int) (type :int))
    :result-type :int :language :c 
    :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-get_frame-gfx-tiles "lbui_get_frame_gfx_tiles")
    ((key :int) (type :int))
    :result-type :int :language :c 
    :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-get-window-width "lbui_get_window_width")
    nil
    :result-type :int :language :c 
    :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-get-window-height "lbui_get_window_height")
    nil
    :result-type :int :language :c 
    :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-full-blit "lbui_full_blit")
    ((num :short) (x :short) (y :short) (img :unsigned) (flag :short))
    :result-type :int :language :c 
    :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-transparent-blit "lbui_transparent_blit")
    ((num :short) (x :short) (y :short) (img :unsigned) (flag :short))
    :result-type :int :language :c 
    :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-clear-coords! "lbui_clear_coords")
    ((num :short) (x :short) (y :short) (w :short) (h :short))
    :result-type :int :language :c 
    :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-flush-coords! "lbui_flush_coords")
    ((num :short) (x :short) (y :short) (w :short) (h :short))
    :result-type :int :language :c 
    :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-recalculate-frame-placements! "lbui_recalculate_frame_placements")
    ((arg :int))
    :result-type :int :language :c 
    :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-install-font-in-frame! "lbui_install_font_in_frame")
    ((key :int) (font (:reference-pass :ef-mb-string)) (ptsize :int)
     (style :int))
    :result-type :int :language :c 
    :calling-convention :stdcall :module :lang-ffi)


(eval-when (:execute :load-toplevel :compile-toplevel)
  (export
   '(c_current_ui c-listen-for-event c-init-c-side& c-cleanup-c-side&
     c-set-lisp-system! c-set-lisp-callback! c-init-sound-system&
     c-activate-sound-system& c-get-sound-status c-load-sound-effect&
     c-play-sound-effect c-halt-sound-effects c-load-music-file&
     c-play-music-file c-halt-music load-gfx-image& c-load-texture&
     c-get-image-width c-get-image-height c-init-frame-system& c-add-frame!
     c-add-frame-coords! c-add-frame-tileinfo! c-add-frame-fontinfo!
     c-add-frame-gfxinfo! c-add-frame-bg! c-has_frame c-get-frame-columns
     c-get-frame-rows c-get-frame-tile-width c-get-frame-tile-height
     c-get_frame-gfx-tiles c-get-window-width c-get-window-height c-full-blit
     c-transparent-blit c-clear-coords! c-flush-coords!
     c-recalculate-frame-placements! c-install-font-in-frame!)))

;;; End of generated file.
