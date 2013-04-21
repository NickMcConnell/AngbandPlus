;;; Please do not edit this _GENERATED_ file.


(in-package :org.langband.ffi)
(sb-alien:define-alien-type cptr c-string)

(export 'cptr)


(declaim (inline c_current_ui))
(sb-alien:define-alien-routine ("lbui_current_ui" c_current_ui)
           int)


(declaim (inline c-listen-for-event))
(sb-alien:define-alien-routine ("lbui_listen_for_event" c-listen-for-event)
           int
           (option int :in))


(declaim (inline c-init-c-side&))
(sb-alien:define-alien-routine ("lbui_init_c_side" c-init-c-side&)
           int
           (ui cptr)
           (source-path cptr)
           (config-path cptr)
           (data-path cptr)
           (flags int :in))


(declaim (inline c-cleanup-c-side&))
(sb-alien:define-alien-routine ("lbui_cleanup_c_side" c-cleanup-c-side&)
           int)


(declaim (inline c-set-lisp-system!))
(sb-alien:define-alien-routine ("lbui_set_lisp_system" c-set-lisp-system!)
           void
           (type int :in))


#+use-callback-from-c

(declaim (inline c-set-lisp-callback!))

#+use-callback-from-c
(sb-alien:define-alien-routine ("lbui_set_lisp_callback" c-set-lisp-callback!)
           void
           (name cptr)
           (ptr unsigned :in))


(declaim (inline c-init-sound-system&))
(sb-alien:define-alien-routine ("lbui_init_sound_system" c-init-sound-system&)
           int
           (size int :in))


(declaim (inline c-activate-sound-system&))
(sb-alien:define-alien-routine ("lbui_activate_sound_system" c-activate-sound-system&)
           int)


(declaim (inline c-get-sound-status))
(sb-alien:define-alien-routine ("lbui_get_sound_status" c-get-sound-status)
           int)


(declaim (inline c-load-sound-effect&))
(sb-alien:define-alien-routine ("lbui_load_sound_effect" c-load-sound-effect&)
           int
           (fname cptr)
           (idx int :in))


(declaim (inline c-play-sound-effect))
(sb-alien:define-alien-routine ("lbui_play_sound_effect" c-play-sound-effect)
           int
           (idx int :in)
           (channel short :in)
           (loops short :in))


(declaim (inline c-halt-sound-effects))
(sb-alien:define-alien-routine ("lbui_halt_sound_effects" c-halt-sound-effects)
           int
           (channel short :in))


(declaim (inline c-load-music-file&))
(sb-alien:define-alien-routine ("lbui_load_music_file" c-load-music-file&)
           int
           (fname cptr)
           (idx int :in))


(declaim (inline c-play-music-file))
(sb-alien:define-alien-routine ("lbui_play_music_file" c-play-music-file)
           int
           (idx int :in)
           (loops short :in))


(declaim (inline c-halt-music))
(sb-alien:define-alien-routine ("lbui_halt_music" c-halt-music)
           int)


#+image-support

(declaim (inline load-gfx-image&))

#+image-support
(sb-alien:define-alien-routine ("lbui_load_gfx_image" load-gfx-image&)
           int
           (fname cptr)
           (idx int :in)
           (transcolour unsigned :in))


#+image-support

(declaim (inline c-load-texture&))

#+image-support
(sb-alien:define-alien-routine ("lbui_load_texture" c-load-texture&)
           int
           (idx int :in)
           (fname cptr)
           (twid int :in)
           (thgt int :in)
           (alpha unsigned :in))


#+image-support

(declaim (inline c-get-image-width))

#+image-support
(sb-alien:define-alien-routine ("lbui_get_image_width" c-get-image-width)
           int
           (idx int :in))


#+image-support

(declaim (inline c-get-image-height))

#+image-support
(sb-alien:define-alien-routine ("lbui_get_image_height" c-get-image-height)
           int
           (idx int :in))


(declaim (inline c-init-frame-system&))
(sb-alien:define-alien-routine ("lbui_init_frame_system" c-init-frame-system&)
           int
           (act-size int :in)
           (pre-size int :in))


(declaim (inline c-add-frame!))
(sb-alien:define-alien-routine ("lbui_add_frame" c-add-frame!)
           int
           (key int :in)
           (name cptr))


(declaim (inline c-add-frame-coords!))
(sb-alien:define-alien-routine ("lbui_add_frame_coords" c-add-frame-coords!)
           int
           (key int :in)
           (x int :in)
           (y int :in)
           (w int :in)
           (h int :in))


(declaim (inline c-add-frame-tileinfo!))
(sb-alien:define-alien-routine ("lbui_add_frame_tileinfo" c-add-frame-tileinfo!)
           int
           (key int :in)
           (tw int :in)
           (th int :in))


(declaim (inline c-add-frame-fontinfo!))
(sb-alien:define-alien-routine ("lbui_add_frame_fontinfo" c-add-frame-fontinfo!)
           int
           (key int :in)
           (font cptr)
           (ptsize int :in)
           (style int :in))


(declaim (inline c-add-frame-gfxinfo!))
(sb-alien:define-alien-routine ("lbui_add_frame_gfxinfo" c-add-frame-gfxinfo!)
           int
           (key int :in)
           (use-tiles int :in))


(declaim (inline c-add-frame-bg!))
(sb-alien:define-alien-routine ("lbui_add_frame_bg" c-add-frame-bg!)
           int
           (key int :in)
           (img-idx int :in))


(declaim (inline c-has_frame))
(sb-alien:define-alien-routine ("lbui_has_frame" c-has_frame)
           int
           (key int :in)
           (type int :in))


(declaim (inline c-get-frame-columns))
(sb-alien:define-alien-routine ("lbui_get_frame_columns" c-get-frame-columns)
           int
           (key int :in)
           (type int :in))


(declaim (inline c-get-frame-rows))
(sb-alien:define-alien-routine ("lbui_get_frame_rows" c-get-frame-rows)
           int
           (key int :in)
           (type int :in))


(declaim (inline c-get-frame-tile-width))
(sb-alien:define-alien-routine ("lbui_get_frame_tile_width" c-get-frame-tile-width)
           int
           (key int :in)
           (type int :in))


(declaim (inline c-get-frame-tile-height))
(sb-alien:define-alien-routine ("lbui_get_frame_tile_height" c-get-frame-tile-height)
           int
           (key int :in)
           (type int :in))


(declaim (inline c-get_frame-gfx-tiles))
(sb-alien:define-alien-routine ("lbui_get_frame_gfx_tiles" c-get_frame-gfx-tiles)
           int
           (key int :in)
           (type int :in))


(declaim (inline c-get-window-width))
(sb-alien:define-alien-routine ("lbui_get_window_width" c-get-window-width)
           int)


(declaim (inline c-get-window-height))
(sb-alien:define-alien-routine ("lbui_get_window_height" c-get-window-height)
           int)


(declaim (inline c-full-blit))
(sb-alien:define-alien-routine ("lbui_full_blit" c-full-blit)
           int
           (num short :in)
           (x short :in)
           (y short :in)
           (img unsigned :in)
           (flag short :in))


(declaim (inline c-transparent-blit))
(sb-alien:define-alien-routine ("lbui_transparent_blit" c-transparent-blit)
           int
           (num short :in)
           (x short :in)
           (y short :in)
           (img unsigned :in)
           (flag short :in))


(declaim (inline c-clear-coords!))
(sb-alien:define-alien-routine ("lbui_clear_coords" c-clear-coords!)
           int
           (num short :in)
           (x short :in)
           (y short :in)
           (w short :in)
           (h short :in))


(declaim (inline c-flush-coords!))
(sb-alien:define-alien-routine ("lbui_flush_coords" c-flush-coords!)
           int
           (num short :in)
           (x short :in)
           (y short :in)
           (w short :in)
           (h short :in))


(declaim (inline c-recalculate-frame-placements!))
(sb-alien:define-alien-routine ("lbui_recalculate_frame_placements" c-recalculate-frame-placements!)
           int
           (arg int :in))


(declaim (inline c-install-font-in-frame!))
(sb-alien:define-alien-routine ("lbui_install_font_in_frame" c-install-font-in-frame!)
           int
           (key int :in)
           (font cptr)
           (ptsize int :in)
           (style int :in))


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
