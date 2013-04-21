#|

DESC: ffi/ffi-defs.lisp - the foreign declarations that [L] uses

|#

(in-package :org.langband.ffi)

;;(def-foreign-type angbyte int) ;; argh
(def-foreign-type cptr c-string8)

(def-foreign-function ("lbui_current_ui" c_current_ui)
    :returns 'int)

(def-foreign-function ("lbui_listen_for_event" c-listen-for-event)
    :args '((int option))
    :returns 'int)

(def-foreign-function ("lbui_init_c_side" c-init-c-side&)
    :returns 'int
    :args '((cptr ui)
	    (cptr source-path)
	    (cptr config-path)
	    (cptr data-path)
	    (int flags)
	    ))

(def-foreign-function ("lbui_cleanup_c_side" c-cleanup-c-side&)
    :returns 'int)


(def-foreign-function ("lbui_set_lisp_system" c-set-lisp-system!)
    :returns 'void
    :args '((int type)))

(def-foreign-function ("lbui_set_lisp_callback" c-set-lisp-callback!)
    :returns 'void
    :args '((cptr name)
	    (ptr-type ptr)
	    )
    :only-when 'use-callback-from-c)


(def-foreign-function ("lbui_init_sound_system" c-init-sound-system&)
    :returns 'int
    :args '((int size)))

(def-foreign-function ("lbui_activate_sound_system" c-activate-sound-system&)
    :returns 'int
    :args nil)

(def-foreign-function ("lbui_get_sound_status" c-get-sound-status)
    :returns 'int)

(def-foreign-function ("lbui_load_sound_effect" c-load-sound-effect&)
    :returns 'int
    :args '((cptr fname)
	    (int idx)))

(def-foreign-function ("lbui_play_sound_effect" c-play-sound-effect)
    :returns 'int
    :args '((int idx)
	    (short channel)
	    (short loops)))

(def-foreign-function ("lbui_halt_sound_effects" c-halt-sound-effects)
    :returns 'int
    :args '((short channel)))

(def-foreign-function ("lbui_load_music_file" c-load-music-file&)
    :returns 'int
    :args '((cptr fname)
	    (int idx)))

(def-foreign-function ("lbui_play_music_file" c-play-music-file)
    :returns 'int
    :args '((int idx)
	    (short loops)))

(def-foreign-function ("lbui_halt_music" c-halt-music)
    :returns 'int
    :args nil)


(def-foreign-function ("lbui_load_gfx_image" load-gfx-image&)
    :returns 'int
    :args '((cptr fname)
	    (int idx)
	    (unsigned transcolour)
	    )
    :only-when 'image-support)

(def-foreign-function ("lbui_load_texture" c-load-texture&)
    :returns 'int
    :args '((int idx)
	    (cptr fname)
	    (int twid)
	    (int thgt)
	    (unsigned alpha)
	    )
    :only-when 'image-support)

(def-foreign-function ("lbui_get_image_width" c-get-image-width)
    :returns 'int
    :args '((int idx))
    :only-when 'image-support)

(def-foreign-function ("lbui_get_image_height" c-get-image-height)
    :returns 'int
    :args '((int idx))
    :only-when 'image-support)

;;; Subwindow/Frame stuff
(def-foreign-function ("lbui_init_frame_system" c-init-frame-system&)
    :returns 'int
    :args '((int act-size)
	    (int pre-size)))


(def-foreign-function ("lbui_add_frame" c-add-frame!)
    :returns 'int
    :args '((int key)
	    (cptr name)))

(def-foreign-function ("lbui_add_frame_coords" c-add-frame-coords!)
    :returns 'int
    :args '((int key)
	    (int x)
	    (int y)
	    (int w)
	    (int h)))

(def-foreign-function ("lbui_add_frame_tileinfo" c-add-frame-tileinfo!)
    :returns 'int
    :args '((int key)
	    (int tw)
	    (int th)
	    ))

(def-foreign-function ("lbui_add_frame_fontinfo" c-add-frame-fontinfo!)
    :returns 'int
    :args '((int key)
	    (cptr font)
	    (int ptsize)
	    (int style)
	    ))

(def-foreign-function ("lbui_add_frame_gfxinfo" c-add-frame-gfxinfo!)
    :returns 'int
    :args '((int key)
	    (int use-tiles)))

(def-foreign-function ("lbui_add_frame_bg" c-add-frame-bg!)
    :returns 'int
    :args '((int key)
	    (int img-idx)))

(def-foreign-function ("lbui_has_frame" c-has_frame)
    :returns 'int
    :args '((int key)
	    (int type)))

(def-foreign-function ("lbui_get_frame_columns" c-get-frame-columns)
    :returns 'int
    :args '((int key)
	    (int type)))

(def-foreign-function ("lbui_get_frame_rows" c-get-frame-rows)
    :returns 'int
    :args '((int key)
	    (int type)))

(def-foreign-function ("lbui_get_frame_tile_width" c-get-frame-tile-width)
    :returns 'int
    :args '((int key)
	    (int type)))

(def-foreign-function ("lbui_get_frame_tile_height" c-get-frame-tile-height)
    :returns 'int
    :args '((int key)
	    (int type)))

(def-foreign-function ("lbui_get_frame_gfx_tiles" c-get_frame-gfx-tiles)
    :returns 'int
    :args '((int key)
	    (int type)))

(def-foreign-function ("lbui_get_window_width" c-get-window-width)
    :returns 'int
    :args nil)

(def-foreign-function ("lbui_get_window_height" c-get-window-height)
    :returns 'int
    :args nil)


(def-foreign-function ("lbui_full_blit" c-full-blit)
    :returns 'int
    :args '((short num)
	    (short x)
	    (short y)
	    (unsigned img)
	    (short flag)))

(def-foreign-function ("lbui_transparent_blit" c-transparent-blit)
    :returns 'int
    :args '((short num)
	    (short x)
	    (short y)
	    (unsigned img)
	    (short flag)))

(def-foreign-function ("lbui_clear_coords" c-clear-coords!)
    :returns 'int
    :args '((short num)
	    (short x)
	    (short y)
	    (short w)
	    (short h)))

(def-foreign-function ("lbui_flush_coords" c-flush-coords!)
    :returns 'int
    :args '((short num)
	    (short x)
	    (short y)
	    (short w)
	    (short h)))


(def-foreign-function ("lbui_recalculate_frame_placements" c-recalculate-frame-placements!)
    :returns 'int
    :args '((int arg)))

(def-foreign-function ("lbui_install_font_in_frame" c-install-font-in-frame!)
    :returns 'int
    :args '((int key)
	    (cptr font)
	    (int ptsize)
	    (int style)
	    ))
