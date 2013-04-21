;;; Please do not edit this _GENERATED_ file.


(in-package :org.langband.ffi)
(ffi:def-c-type cptr c-string)


(ffi:def-call-out c_current_ui (:name "lbui_current_ui")
   (:language :stdc)
   (:arguments )
   (:return-type int))

(ffi:def-call-out c-listen-for-event (:name "lbui_listen_for_event")
   (:language :stdc)
   (:arguments (option int) )
   (:return-type int))

(ffi:def-call-out c-init-c-side& (:name "lbui_init_c_side")
   (:language :stdc)
   (:arguments (ui c-string) (source-path c-string) (config-path c-string) (data-path
                                                                            c-string) (flags
                                                                                       int) )
   (:return-type int))

(ffi:def-call-out c-cleanup-c-side& (:name "lbui_cleanup_c_side")
   (:language :stdc)
   (:arguments )
   (:return-type int))

(ffi:def-call-out c-set-lisp-system! (:name "lbui_set_lisp_system")
   (:language :stdc)
   (:arguments (type int) )
   (:return-type nil))


#+use-callback-from-c
(ffi:def-call-out c-set-lisp-callback! (:name "lbui_set_lisp_callback")
   (:language :stdc)
   (:arguments (name c-string) (ptr c-pointer) )
   (:return-type nil))

(ffi:def-call-out c-init-sound-system& (:name "lbui_init_sound_system")
   (:language :stdc)
   (:arguments (size int) )
   (:return-type int))

(ffi:def-call-out c-activate-sound-system& (:name "lbui_activate_sound_system")
   (:language :stdc)
   (:arguments )
   (:return-type int))

(ffi:def-call-out c-get-sound-status (:name "lbui_get_sound_status")
   (:language :stdc)
   (:arguments )
   (:return-type int))

(ffi:def-call-out c-load-sound-effect& (:name "lbui_load_sound_effect")
   (:language :stdc)
   (:arguments (fname c-string) (idx int) )
   (:return-type int))

(ffi:def-call-out c-play-sound-effect (:name "lbui_play_sound_effect")
   (:language :stdc)
   (:arguments (idx int) (channel short) (loops short) )
   (:return-type int))

(ffi:def-call-out c-halt-sound-effects (:name "lbui_halt_sound_effects")
   (:language :stdc)
   (:arguments (channel short) )
   (:return-type int))

(ffi:def-call-out c-load-music-file& (:name "lbui_load_music_file")
   (:language :stdc)
   (:arguments (fname c-string) (idx int) )
   (:return-type int))

(ffi:def-call-out c-play-music-file (:name "lbui_play_music_file")
   (:language :stdc)
   (:arguments (idx int) (loops short) )
   (:return-type int))

(ffi:def-call-out c-halt-music (:name "lbui_halt_music")
   (:language :stdc)
   (:arguments )
   (:return-type int))


#+image-support
(ffi:def-call-out load-gfx-image& (:name "lbui_load_gfx_image")
   (:language :stdc)
   (:arguments (fname c-string) (idx int) (transcolour uint) )
   (:return-type int))


#+image-support
(ffi:def-call-out c-load-texture& (:name "lbui_load_texture")
   (:language :stdc)
   (:arguments (idx int) (fname c-string) (twid int) (thgt int) (alpha uint) )
   (:return-type int))


#+image-support
(ffi:def-call-out c-get-image-width (:name "lbui_get_image_width")
   (:language :stdc)
   (:arguments (idx int) )
   (:return-type int))


#+image-support
(ffi:def-call-out c-get-image-height (:name "lbui_get_image_height")
   (:language :stdc)
   (:arguments (idx int) )
   (:return-type int))

(ffi:def-call-out c-init-frame-system& (:name "lbui_init_frame_system")
   (:language :stdc)
   (:arguments (act-size int) (pre-size int) )
   (:return-type int))

(ffi:def-call-out c-add-frame! (:name "lbui_add_frame")
   (:language :stdc)
   (:arguments (key int) (name c-string) )
   (:return-type int))

(ffi:def-call-out c-add-frame-coords! (:name "lbui_add_frame_coords")
   (:language :stdc)
   (:arguments (key int) (x int) (y int) (w int) (h int) )
   (:return-type int))

(ffi:def-call-out c-add-frame-tileinfo! (:name "lbui_add_frame_tileinfo")
   (:language :stdc)
   (:arguments (key int) (tw int) (th int) )
   (:return-type int))

(ffi:def-call-out c-add-frame-fontinfo! (:name "lbui_add_frame_fontinfo")
   (:language :stdc)
   (:arguments (key int) (font c-string) (ptsize int) (style int) )
   (:return-type int))

(ffi:def-call-out c-add-frame-gfxinfo! (:name "lbui_add_frame_gfxinfo")
   (:language :stdc)
   (:arguments (key int) (use-tiles int) )
   (:return-type int))

(ffi:def-call-out c-add-frame-bg! (:name "lbui_add_frame_bg")
   (:language :stdc)
   (:arguments (key int) (img-idx int) )
   (:return-type int))

(ffi:def-call-out c-has_frame (:name "lbui_has_frame")
   (:language :stdc)
   (:arguments (key int) (type int) )
   (:return-type int))

(ffi:def-call-out c-get-frame-columns (:name "lbui_get_frame_columns")
   (:language :stdc)
   (:arguments (key int) (type int) )
   (:return-type int))

(ffi:def-call-out c-get-frame-rows (:name "lbui_get_frame_rows")
   (:language :stdc)
   (:arguments (key int) (type int) )
   (:return-type int))

(ffi:def-call-out c-get-frame-tile-width (:name "lbui_get_frame_tile_width")
   (:language :stdc)
   (:arguments (key int) (type int) )
   (:return-type int))

(ffi:def-call-out c-get-frame-tile-height (:name "lbui_get_frame_tile_height")
   (:language :stdc)
   (:arguments (key int) (type int) )
   (:return-type int))

(ffi:def-call-out c-get_frame-gfx-tiles (:name "lbui_get_frame_gfx_tiles")
   (:language :stdc)
   (:arguments (key int) (type int) )
   (:return-type int))

(ffi:def-call-out c-get-window-width (:name "lbui_get_window_width")
   (:language :stdc)
   (:arguments )
   (:return-type int))

(ffi:def-call-out c-get-window-height (:name "lbui_get_window_height")
   (:language :stdc)
   (:arguments )
   (:return-type int))

(ffi:def-call-out c-full-blit (:name "lbui_full_blit")
   (:language :stdc)
   (:arguments (num short) (x short) (y short) (img uint) (flag short) )
   (:return-type int))

(ffi:def-call-out c-transparent-blit (:name "lbui_transparent_blit")
   (:language :stdc)
   (:arguments (num short) (x short) (y short) (img uint) (flag short) )
   (:return-type int))

(ffi:def-call-out c-clear-coords! (:name "lbui_clear_coords")
   (:language :stdc)
   (:arguments (num short) (x short) (y short) (w short) (h short) )
   (:return-type int))

(ffi:def-call-out c-flush-coords! (:name "lbui_flush_coords")
   (:language :stdc)
   (:arguments (num short) (x short) (y short) (w short) (h short) )
   (:return-type int))

(ffi:def-call-out c-recalculate-frame-placements! (:name "lbui_recalculate_frame_placements")
   (:language :stdc)
   (:arguments (arg int) )
   (:return-type int))

(ffi:def-call-out c-install-font-in-frame! (:name "lbui_install_font_in_frame")
   (:language :stdc)
   (:arguments (key int) (font c-string) (ptsize int) (style int) )
   (:return-type int))


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
