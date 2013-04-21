;;; Please do not edit this _GENERATED_ file.


(in-package :org.langband.ffi)
(sb-alien:define-alien-type cptr c-string)

(export 'cptr)
(sb-alien:define-alien-type errr int)

(export 'errr)


(declaim (inline c_current_ui))
(sb-alien:define-alien-routine ("current_ui" c_current_ui)
           int)


(declaim (inline c_quit!))
(sb-alien:define-alien-routine ("z_quit" c_quit!)
           void
           (msg (* char) :in))


(declaim (inline c-clear-from!))
(sb-alien:define-alien-routine ("clear_from" c-clear-from!)
           void
           (row int :in))


(declaim (inline term-activate&))
(sb-alien:define-alien-routine ("my_term_activate" term-activate&)
           int
           (term-num int :in))


(declaim (inline c-prt-token!))
(sb-alien:define-alien-routine ("print_coloured_token" c-prt-token!)
           void
           (term int :in)
           (colour int :in)
           (token int :in)
           (row int :in)
           (col int :in))


(declaim (inline c-prt-stat!))
(sb-alien:define-alien-routine ("print_coloured_stat" c-prt-stat!)
           void
           (term int :in)
           (colour int :in)
           (stat int :in)
           (row int :in)
           (col int :in))


(declaim (inline c-prt-number!))
(sb-alien:define-alien-routine ("print_coloured_number" c-prt-number!)
           void
           (term int :in)
           (colour int :in)
           (number long :in)
           (padding int :in)
           (row int :in)
           (col int :in))


(declaim (inline c_term_putstr!))
(sb-alien:define-alien-routine ("my_Term_putstr" c_term_putstr!)
           errr
           (col int :in)
           (row int :in)
           (something int :in)
           (colour int :in)
           (text (* char) :in))


(declaim (inline c_term_erase!))
(sb-alien:define-alien-routine ("Term_erase" c_term_erase!)
           errr
           (col int :in)
           (row int :in)
           (something int :in))


(declaim (inline c-term-queue-char!))
(sb-alien:define-alien-routine ("my_Term_queue_char" c-term-queue-char!)
           void
           (col int :in)
           (row int :in)
           (colour int :in)
           (the-char int :in)
           (tcolour int :in)
           (tchar int :in))


(declaim (inline c-term-gotoxy!))
(sb-alien:define-alien-routine ("Term_gotoxy" c-term-gotoxy!)
           void
           (row int :in)
           (col int :in))


(declaim (inline c-set-cursor&))
(sb-alien:define-alien-routine ("my_Term_set_cursor" c-set-cursor&)
           errr
           (col int :in))


(declaim (inline c-term-clear!))
(sb-alien:define-alien-routine ("Term_clear" c-term-clear!)
           errr)


(declaim (inline c-term-flush!))
(sb-alien:define-alien-routine ("Term_flush" c-term-flush!)
           errr)


(declaim (inline c-term_fresh!))
(sb-alien:define-alien-routine ("Term_fresh" c-term_fresh!)
           errr)


(declaim (inline c-term-save!))
(sb-alien:define-alien-routine ("Term_save" c-term-save!)
           errr)


(declaim (inline c-term-load!))
(sb-alien:define-alien-routine ("Term_load" c-term-load!)
           errr)


(declaim (inline c-term-xtra&))
(sb-alien:define-alien-routine ("Term_xtra" c-term-xtra&)
           errr
           (msg int :in)
           (arg int :in))


(declaim (inline c-term-keypress))
(sb-alien:define-alien-routine ("Term_keypress" c-term-keypress)
           errr
           (key int :in))


(declaim (inline c-inkey!))
(sb-alien:define-alien-routine ("inkey" c-inkey!)
           char)


(declaim (inline init_c-side&))
(sb-alien:define-alien-routine ("init_c_side" init_c-side&)
           errr
           (ui cptr)
           (source-path cptr)
           (config-path cptr)
           (gfx-path cptr)
           (flags int :in))


(declaim (inline cleanup-c-side&))
(sb-alien:define-alien-routine ("cleanup_c_side" cleanup-c-side&)
           errr)


(declaim (inline c_macro_add&))
(sb-alien:define-alien-routine ("macro_add" c_macro_add&)
           void
           (key cptr)
           (value cptr))


(declaim (inline init-macro-system&))
(sb-alien:define-alien-routine ("macro_init" init-macro-system&)
           void)


(declaim (inline c-set-lisp-system!))
(sb-alien:define-alien-routine ("set_lisp_system" c-set-lisp-system!)
           void
           (type int :in))


#+use-callback-from-c

(declaim (inline c-set-lisp-callback!))

#+use-callback-from-c
(sb-alien:define-alien-routine ("set_lisp_callback" c-set-lisp-callback!)
           void
           (name cptr)
           (ptr unsigned :in))


(declaim (inline c-get-cur-term))
(sb-alien:define-alien-routine ("my_get_current_term" c-get-cur-term)
           int)


#+win32

(declaim (inline c-set-hinst!))

#+win32
(sb-alien:define-alien-routine ("setHINST" c-set-hinst!)
           int
           (val long :in))


(declaim (inline c-init-sound-system&))
(sb-alien:define-alien-routine ("init_sound_system" c-init-sound-system&)
           int
           (size int :in))


(declaim (inline c-load_sound-effect&))
(sb-alien:define-alien-routine ("load_sound_effect" c-load_sound-effect&)
           errr
           (fname cptr)
           (idx int :in))


(declaim (inline c-get-sound-status))
(sb-alien:define-alien-routine ("get_sound_status" c-get-sound-status)
           int)


(declaim (inline c-paint-gfx-image&))
(sb-alien:define-alien-routine ("paint_gfx_image" c-paint-gfx-image&)
           int
           (fname cptr)
           (type cptr)
           (x int :in)
           (y int :in))


#+image-support

(declaim (inline load-gfx-image&))

#+image-support
(sb-alien:define-alien-routine ("load_gfx_image" load-gfx-image&)
           int
           (fname cptr)
           (type cptr))


#+image-support

(declaim (inline load-scaled-image&))

#+image-support
(sb-alien:define-alien-routine ("load_scaled_image" load-scaled-image&)
           int
           (fname cptr)
           (idx int :in)
           (wid int :in)
           (hgt int :in))


(declaim (inline c-texture_background!))
(sb-alien:define-alien-routine ("textureBackground" c-texture_background!)
           int
           (term int :in)
           (fname cptr)
           (alpha int :in))


(declaim (inline c-init-frame-system&))
(sb-alien:define-alien-routine ("init_frame_system" c-init-frame-system&)
           int
           (act-size int :in)
           (pre-size int :in))


(declaim (inline c-add_frame!))
(sb-alien:define-alien-routine ("add_frame" c-add_frame!)
           int
           (key int :in)
           (name cptr))


(declaim (inline c-add-frame-coords!))
(sb-alien:define-alien-routine ("add_frame_coords" c-add-frame-coords!)
           int
           (key int :in)
           (x int :in)
           (y int :in)
           (w int :in)
           (h int :in))


(declaim (inline c-add_frame-tileinfo!))
(sb-alien:define-alien-routine ("add_frame_tileinfo" c-add_frame-tileinfo!)
           int
           (key int :in)
           (tw int :in)
           (th int :in)
           (font cptr)
           (bg cptr))


(declaim (inline c-add-frame-gfxinfo!))
(sb-alien:define-alien-routine ("add_frame_gfxinfo" c-add-frame-gfxinfo!)
           int
           (key int :in)
           (use-tiles int :in))


(declaim (inline c-has_frame))
(sb-alien:define-alien-routine ("has_frame" c-has_frame)
           int
           (key int :in)
           (type int :in))


(declaim (inline c-activate-frame!))
(sb-alien:define-alien-routine ("activate_frame" c-activate-frame!)
           int
           (key int :in))


(declaim (inline c-deactivate-frame!))
(sb-alien:define-alien-routine ("deactivate_frame" c-deactivate-frame!)
           int
           (key int :in))


(declaim (inline c-clean-frame!))
(sb-alien:define-alien-routine ("clean_frame" c-clean-frame!)
           int
           (key int :in))


(declaim (inline c-wipe-frame!))
(sb-alien:define-alien-routine ("wipe_frame" c-wipe-frame!)
           int
           (key int :in))


(declaim (inline c-get-frame-columns))
(sb-alien:define-alien-routine ("get_frame_columns" c-get-frame-columns)
           int
           (key int :in)
           (type int :in))


(declaim (inline c-get-frame-rows))
(sb-alien:define-alien-routine ("get_frame_rows" c-get-frame-rows)
           int
           (key int :in)
           (type int :in))


(declaim (inline c-get-frame-tile-width))
(sb-alien:define-alien-routine ("get_frame_tile_width" c-get-frame-tile-width)
           int
           (key int :in)
           (type int :in))


(declaim (inline c-get-frame-tile-height))
(sb-alien:define-alien-routine ("get_frame_tile_height" c-get-frame-tile-height)
           int
           (key int :in)
           (type int :in))


(declaim (inline c-get_frame-gfx-tiles))
(sb-alien:define-alien-routine ("get_frame_gfx_tiles" c-get_frame-gfx-tiles)
           int
           (key int :in)
           (type int :in))


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
