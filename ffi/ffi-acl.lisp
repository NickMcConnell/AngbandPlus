;;; Please do not edit this _GENERATED_ file.


(in-package :org.langband.ffi)
(ff:def-foreign-type cptr (* :char))


(ff:def-foreign-call (c_current_ui "lbui_current_ui")
   nil
   :returning :int)

(ff:def-foreign-call (c-listen-for-event "lbui_listen_for_event")
   ((option :int))
   :returning :int)

(ff:def-foreign-call (c-init-c-side& "lbui_init_c_side")
   ((ui cptr) (source-path cptr) (config-path cptr) (data-path cptr)
    (flags :int))
   :returning :int)

(ff:def-foreign-call (c-cleanup-c-side& "lbui_cleanup_c_side")
   nil
   :returning :int)

(ff:def-foreign-call (c-set-lisp-system! "lbui_set_lisp_system")
   ((type :int))
   :returning :void)


#+use-callback-from-c
(ff:def-foreign-call (c-set-lisp-callback! "lbui_set_lisp_callback")
   ((name cptr) (ptr))
   :returning :void)

(ff:def-foreign-call (c-init-sound-system& "lbui_init_sound_system")
   ((size :int))
   :returning :int)

(ff:def-foreign-call (c-activate-sound-system& "lbui_activate_sound_system")
   nil
   :returning :int)

(ff:def-foreign-call (c-get-sound-status "lbui_get_sound_status")
   nil
   :returning :int)

(ff:def-foreign-call (c-load-sound-effect& "lbui_load_sound_effect")
   ((fname cptr) (idx :int))
   :returning :int)

(ff:def-foreign-call (c-play-sound-effect "lbui_play_sound_effect")
   ((idx :int) (channel :short) (loops :short))
   :returning :int)

(ff:def-foreign-call (c-halt-sound-effects "lbui_halt_sound_effects")
   ((channel :short))
   :returning :int)

(ff:def-foreign-call (c-load-music-file& "lbui_load_music_file")
   ((fname cptr) (idx :int))
   :returning :int)

(ff:def-foreign-call (c-play-music-file "lbui_play_music_file")
   ((idx :int) (loops :short))
   :returning :int)

(ff:def-foreign-call (c-halt-music "lbui_halt_music")
   nil
   :returning :int)


#+image-support
(ff:def-foreign-call (load-gfx-image& "lbui_load_gfx_image")
   ((fname cptr) (idx :int) (transcolour :unsigned-long))
   :returning :int)


#+image-support
(ff:def-foreign-call (c-load-texture& "lbui_load_texture")
   ((idx :int) (fname cptr) (twid :int) (thgt :int) (alpha :unsigned-long))
   :returning :int)


#+image-support
(ff:def-foreign-call (c-get-image-width "lbui_get_image_width")
   ((idx :int))
   :returning :int)


#+image-support
(ff:def-foreign-call (c-get-image-height "lbui_get_image_height")
   ((idx :int))
   :returning :int)

(ff:def-foreign-call (c-init-frame-system& "lbui_init_frame_system")
   ((act-size :int) (pre-size :int))
   :returning :int)

(ff:def-foreign-call (c-add-frame! "lbui_add_frame")
   ((key :int) (name cptr))
   :returning :int)

(ff:def-foreign-call (c-add-frame-coords! "lbui_add_frame_coords")
   ((key :int) (x :int) (y :int) (w :int) (h :int))
   :returning :int)

(ff:def-foreign-call (c-add-frame-tileinfo! "lbui_add_frame_tileinfo")
   ((key :int) (tw :int) (th :int))
   :returning :int)

(ff:def-foreign-call (c-add-frame-fontinfo! "lbui_add_frame_fontinfo")
   ((key :int) (font cptr) (ptsize :int) (style :int))
   :returning :int)

(ff:def-foreign-call (c-add-frame-gfxinfo! "lbui_add_frame_gfxinfo")
   ((key :int) (use-tiles :int))
   :returning :int)

(ff:def-foreign-call (c-add-frame-bg! "lbui_add_frame_bg")
   ((key :int) (img-idx :int))
   :returning :int)

(ff:def-foreign-call (c-has_frame "lbui_has_frame")
   ((key :int) (type :int))
   :returning :int)

(ff:def-foreign-call (c-get-frame-columns "lbui_get_frame_columns")
   ((key :int) (type :int))
   :returning :int)

(ff:def-foreign-call (c-get-frame-rows "lbui_get_frame_rows")
   ((key :int) (type :int))
   :returning :int)

(ff:def-foreign-call (c-get-frame-tile-width "lbui_get_frame_tile_width")
   ((key :int) (type :int))
   :returning :int)

(ff:def-foreign-call (c-get-frame-tile-height "lbui_get_frame_tile_height")
   ((key :int) (type :int))
   :returning :int)

(ff:def-foreign-call (c-get_frame-gfx-tiles "lbui_get_frame_gfx_tiles")
   ((key :int) (type :int))
   :returning :int)

(ff:def-foreign-call (c-get-window-width "lbui_get_window_width")
   nil
   :returning :int)

(ff:def-foreign-call (c-get-window-height "lbui_get_window_height")
   nil
   :returning :int)

(ff:def-foreign-call (c-full-blit "lbui_full_blit")
   ((num :short) (x :short) (y :short) (img :unsigned-long) (flag :short))
   :returning :int)

(ff:def-foreign-call (c-transparent-blit "lbui_transparent_blit")
   ((num :short) (x :short) (y :short) (img :unsigned-long) (flag :short))
   :returning :int)

(ff:def-foreign-call (c-clear-coords! "lbui_clear_coords")
   ((num :short) (x :short) (y :short) (w :short) (h :short))
   :returning :int)

(ff:def-foreign-call (c-flush-coords! "lbui_flush_coords")
   ((num :short) (x :short) (y :short) (w :short) (h :short))
   :returning :int)

(ff:def-foreign-call (c-recalculate-frame-placements! "lbui_recalculate_frame_placements")
   ((arg :int))
   :returning :int)

(ff:def-foreign-call (c-install-font-in-frame! "lbui_install_font_in_frame")
   ((key :int) (font cptr) (ptsize :int) (style :int))
   :returning :int)


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
