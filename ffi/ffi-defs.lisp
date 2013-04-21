#|

DESC: ffi/ffi-defs.lisp - the foreign declarations that [L] uses

|#

(in-package :org.langband.ffi)

;;(def-foreign-type angbyte int) ;; argh
(def-foreign-type cptr c-string8)

(def-foreign-function ("current_ui" c_current_ui)
    :returns 'int)

(def-foreign-function ("listenForEvent" c-listen-for-event)
    :args '((int option))
    :returns 'int)

(def-foreign-function ("init_c_side" init-c-side&)
    :returns 'int
    :args '((cptr ui)
	    (cptr source-path)
	    (cptr config-path)
	    (cptr gfx-path)
	    (int flags)
	    ))

(def-foreign-function ("cleanup_c_side" cleanup-c-side&)
    :returns 'int)


(def-foreign-function ("set_lisp_system" c-set-lisp-system!)
    :returns 'void
    :args '((int type)))

(def-foreign-function ("set_lisp_callback" c-set-lisp-callback!)
    :returns 'void
    :args '((cptr name)
	    (ptr-type ptr)
	    )
    :only-when 'use-callback-from-c)


(def-foreign-function ("init_sound_system" c-init-sound-system&)
    :returns 'int
    :args '((int size)))

(def-foreign-function ("load_sound_effect" c-load-sound-effect&)
    :returns 'int
    :args '((cptr fname)
	    (int idx)))

(def-foreign-function ("get_sound_status" c-get-sound-status)
    :returns 'int)

(def-foreign-function ("play_sound_effect" c-play-sound-effect)
    :returns 'int
    :args '((int idx)))


(def-foreign-function ("load_gfx_image" load-gfx-image&)
    :returns 'int
    :args '((cptr fname)
	    (int idx)
	    (unsigned transcolour)
	    )
    :only-when 'image-support)

(def-foreign-function ("load_texture" c-load-texture&)
    :returns 'int
    :args '((int idx)
	    (cptr fname)
	    (int twid)
	    (int thgt)
	    (unsigned alpha)
	    )
    :only-when 'image-support)

(def-foreign-function ("get_image_width" c-get-image-width)
    :returns 'int
    :args '((int idx))
    :only-when 'image-support)

(def-foreign-function ("get_image_height" c-get-image-height)
    :returns 'int
    :args '((int idx))
    :only-when 'image-support)

;;; Subwindow/Frame stuff
(def-foreign-function ("init_frame_system" c-init-frame-system&)
    :returns 'int
    :args '((int act-size)
	    (int pre-size)))


(def-foreign-function ("add_frame" c-add-frame!)
    :returns 'int
    :args '((int key)
	    (cptr name)))

(def-foreign-function ("add_frame_coords" c-add-frame-coords!)
    :returns 'int
    :args '((int key)
	    (int x)
	    (int y)
	    (int w)
	    (int h)))

(def-foreign-function ("add_frame_tileinfo" c-add-frame-tileinfo!)
    :returns 'int
    :args '((int key)
	    (int tw)
	    (int th)
	    (cptr font)
	    ))

(def-foreign-function ("add_frame_gfxinfo" c-add-frame-gfxinfo!)
    :returns 'int
    :args '((int key)
	    (int use-tiles)))

(def-foreign-function ("add_frame_bg" c-add-frame-bg!)
    :returns 'int
    :args '((int key)
	    (int img-idx)))

(def-foreign-function ("has_frame" c-has_frame)
    :returns 'int
    :args '((int key)
	    (int type)))

(def-foreign-function ("get_frame_columns" c-get-frame-columns)
    :returns 'int
    :args '((int key)
	    (int type)))

(def-foreign-function ("get_frame_rows" c-get-frame-rows)
    :returns 'int
    :args '((int key)
	    (int type)))

(def-foreign-function ("get_frame_tile_width" c-get-frame-tile-width)
    :returns 'int
    :args '((int key)
	    (int type)))

(def-foreign-function ("get_frame_tile_height" c-get-frame-tile-height)
    :returns 'int
    :args '((int key)
	    (int type)))

(def-foreign-function ("get_frame_gfx_tiles" c-get_frame-gfx-tiles)
    :returns 'int
    :args '((int key)
	    (int type)))

(def-foreign-function ("exp_full_blit" c-full-blit)
    :returns 'int
    :args '((short num)
	    (short x)
	    (short y)
	    (unsigned img)
	    (short flag)))

(def-foreign-function ("exp_transparent_blit" c-transparent-blit)
    :returns 'int
    :args '((short num)
	    (short x)
	    (short y)
	    (unsigned img)
	    (short flag)))

(def-foreign-function ("exp_clear_coords" c-clear-coords!)
    :returns 'int
    :args '((short num)
	    (short x)
	    (short y)
	    (short w)
	    (short h)))

(def-foreign-function ("exp_flush_coords" c-flush-coords!)
    :returns 'int
    :args '((short num)
	    (short x)
	    (short y)
	    (short w)
	    (short h)))

