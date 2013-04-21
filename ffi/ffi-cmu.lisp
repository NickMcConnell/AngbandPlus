;;; Please do not edit this _GENERATED_ file.


(in-package :org.langband.ffi)
(alien:def-alien-type cptr c-call:c-string)

(export 'cptr)
(alien:def-alien-type errr c-call:int)

(export 'errr)


(declaim (inline c_current_ui))
(alien:def-alien-routine ("current_ui" c_current_ui)
           c-call:int)


(declaim (inline c_quit!))
(alien:def-alien-routine ("z_quit" c_quit!)
           c-call:void
           (msg (* char) :in))


(declaim (inline c-clear-from!))
(alien:def-alien-routine ("clear_from" c-clear-from!)
           c-call:void
           (row c-call:int :in))


(declaim (inline term-activate&))
(alien:def-alien-routine ("my_term_activate" term-activate&)
           c-call:int
           (term-num c-call:int :in))


(declaim (inline c-prt-token!))
(alien:def-alien-routine ("print_coloured_token" c-prt-token!)
           c-call:void
           (term c-call:int :in)
           (colour c-call:int :in)
           (token c-call:int :in)
           (row c-call:int :in)
           (col c-call:int :in))


(declaim (inline c-prt-stat!))
(alien:def-alien-routine ("print_coloured_stat" c-prt-stat!)
           c-call:void
           (term c-call:int :in)
           (colour c-call:int :in)
           (stat c-call:int :in)
           (row c-call:int :in)
           (col c-call:int :in))


(declaim (inline c-prt-number!))
(alien:def-alien-routine ("print_coloured_number" c-prt-number!)
           c-call:void
           (term c-call:int :in)
           (colour c-call:int :in)
           (number c-call:long :in)
           (padding c-call:int :in)
           (row c-call:int :in)
           (col c-call:int :in))


(declaim (inline c_term_putstr!))
(alien:def-alien-routine ("my_Term_putstr" c_term_putstr!)
           errr
           (col c-call:int :in)
           (row c-call:int :in)
           (something c-call:int :in)
           (colour c-call:int :in)
           (text (* char) :in))


(declaim (inline c_term_erase!))
(alien:def-alien-routine ("Term_erase" c_term_erase!)
           errr
           (col c-call:int :in)
           (row c-call:int :in)
           (something c-call:int :in))


(declaim (inline c-term-queue-char!))
(alien:def-alien-routine ("my_Term_queue_char" c-term-queue-char!)
           c-call:void
           (col c-call:int :in)
           (row c-call:int :in)
           (colour c-call:int :in)
           (the-char c-call:int :in)
           (tcolour c-call:int :in)
           (tchar c-call:int :in))


(declaim (inline c-term-gotoxy!))
(alien:def-alien-routine ("Term_gotoxy" c-term-gotoxy!)
           c-call:void
           (row c-call:int :in)
           (col c-call:int :in))


(declaim (inline c-set-cursor&))
(alien:def-alien-routine ("my_Term_set_cursor" c-set-cursor&)
           errr
           (col c-call:int :in))


(declaim (inline c-term-clear!))
(alien:def-alien-routine ("Term_clear" c-term-clear!)
           errr)


(declaim (inline c-term-flush!))
(alien:def-alien-routine ("Term_flush" c-term-flush!)
           errr)


(declaim (inline c-term_fresh!))
(alien:def-alien-routine ("Term_fresh" c-term_fresh!)
           errr)


(declaim (inline c-term-save!))
(alien:def-alien-routine ("Term_save" c-term-save!)
           errr)


(declaim (inline c-term-load!))
(alien:def-alien-routine ("Term_load" c-term-load!)
           errr)


(declaim (inline c-term-xtra&))
(alien:def-alien-routine ("Term_xtra" c-term-xtra&)
           errr
           (msg c-call:int :in)
           (arg c-call:int :in))


(declaim (inline c-term-keypress))
(alien:def-alien-routine ("Term_keypress" c-term-keypress)
           errr
           (key c-call:int :in))


(declaim (inline c-inkey!))
(alien:def-alien-routine ("inkey" c-inkey!)
           char)


(declaim (inline init_c-side&))
(alien:def-alien-routine ("init_c_side" init_c-side&)
           errr
           (ui cptr)
           (source-path cptr)
           (config-path cptr)
           (gfx-path cptr)
           (flags c-call:int :in))


(declaim (inline cleanup-c-side&))
(alien:def-alien-routine ("cleanup_c_side" cleanup-c-side&)
           errr)


(declaim (inline c_macro_add&))
(alien:def-alien-routine ("macro_add" c_macro_add&)
           c-call:void
           (key cptr)
           (value cptr))


(declaim (inline init-macro-system&))
(alien:def-alien-routine ("macro_init" init-macro-system&)
           c-call:void)


(declaim (inline c-set-lisp-system!))
(alien:def-alien-routine ("set_lisp_system" c-set-lisp-system!)
           c-call:void
           (type c-call:int :in))


#+use-callback-from-c

(declaim (inline c-set-lisp-callback!))

#+use-callback-from-c
(alien:def-alien-routine ("set_lisp_callback" c-set-lisp-callback!)
           c-call:void
           (name cptr)
           (ptr alien:unsigned :in))


(declaim (inline c-get-cur-term))
(alien:def-alien-routine ("my_get_current_term" c-get-cur-term)
           c-call:int)


#+win32

(declaim (inline c-set-hinst!))

#+win32
(alien:def-alien-routine ("setHINST" c-set-hinst!)
           c-call:int
           (val c-call:long :in))


(declaim (inline c-init-sound-system&))
(alien:def-alien-routine ("init_sound_system" c-init-sound-system&)
           c-call:int
           (size c-call:int :in))


(declaim (inline c-load_sound-effect&))
(alien:def-alien-routine ("load_sound_effect" c-load_sound-effect&)
           errr
           (fname cptr)
           (idx c-call:int :in))


(declaim (inline c-get-sound-status))
(alien:def-alien-routine ("get_sound_status" c-get-sound-status)
           c-call:int)


(declaim (inline c-paint-gfx-image&))
(alien:def-alien-routine ("paint_gfx_image" c-paint-gfx-image&)
           c-call:int
           (fname cptr)
           (type cptr)
           (x c-call:int :in)
           (y c-call:int :in))


#+image-support

(declaim (inline load-gfx-image&))

#+image-support
(alien:def-alien-routine ("load_gfx_image" load-gfx-image&)
           c-call:int
           (fname cptr)
           (type cptr))


#+image-support

(declaim (inline load-scaled-image&))

#+image-support
(alien:def-alien-routine ("load_scaled_image" load-scaled-image&)
           c-call:int
           (fname cptr)
           (idx c-call:int :in)
           (wid c-call:int :in)
           (hgt c-call:int :in))


(declaim (inline c-texture_background!))
(alien:def-alien-routine ("textureBackground" c-texture_background!)
           c-call:int
           (term c-call:int :in)
           (fname cptr)
           (alpha c-call:int :in))


(declaim (inline c-init-frame-system&))
(alien:def-alien-routine ("init_frame_system" c-init-frame-system&)
           c-call:int
           (act-size c-call:int :in)
           (pre-size c-call:int :in))


(declaim (inline c-add_frame!))
(alien:def-alien-routine ("add_frame" c-add_frame!)
           c-call:int
           (key c-call:int :in)
           (name cptr))


(declaim (inline c-add-frame-coords!))
(alien:def-alien-routine ("add_frame_coords" c-add-frame-coords!)
           c-call:int
           (key c-call:int :in)
           (x c-call:int :in)
           (y c-call:int :in)
           (w c-call:int :in)
           (h c-call:int :in))


(declaim (inline c-add_frame-tileinfo!))
(alien:def-alien-routine ("add_frame_tileinfo" c-add_frame-tileinfo!)
           c-call:int
           (key c-call:int :in)
           (tw c-call:int :in)
           (th c-call:int :in)
           (font cptr)
           (bg cptr))


(declaim (inline c-add-frame-gfxinfo!))
(alien:def-alien-routine ("add_frame_gfxinfo" c-add-frame-gfxinfo!)
           c-call:int
           (key c-call:int :in)
           (use-tiles c-call:int :in))


(declaim (inline c-has_frame))
(alien:def-alien-routine ("has_frame" c-has_frame)
           c-call:int
           (key c-call:int :in)
           (type c-call:int :in))


(declaim (inline c-activate-frame!))
(alien:def-alien-routine ("activate_frame" c-activate-frame!)
           c-call:int
           (key c-call:int :in))


(declaim (inline c-deactivate-frame!))
(alien:def-alien-routine ("deactivate_frame" c-deactivate-frame!)
           c-call:int
           (key c-call:int :in))


(declaim (inline c-clean-frame!))
(alien:def-alien-routine ("clean_frame" c-clean-frame!)
           c-call:int
           (key c-call:int :in))


(declaim (inline c-wipe-frame!))
(alien:def-alien-routine ("wipe_frame" c-wipe-frame!)
           c-call:int
           (key c-call:int :in))


(declaim (inline c-get-frame-columns))
(alien:def-alien-routine ("get_frame_columns" c-get-frame-columns)
           c-call:int
           (key c-call:int :in)
           (type c-call:int :in))


(declaim (inline c-get-frame-rows))
(alien:def-alien-routine ("get_frame_rows" c-get-frame-rows)
           c-call:int
           (key c-call:int :in)
           (type c-call:int :in))


(declaim (inline c-get-frame-tile-width))
(alien:def-alien-routine ("get_frame_tile_width" c-get-frame-tile-width)
           c-call:int
           (key c-call:int :in)
           (type c-call:int :in))


(declaim (inline c-get-frame-tile-height))
(alien:def-alien-routine ("get_frame_tile_height" c-get-frame-tile-height)
           c-call:int
           (key c-call:int :in)
           (type c-call:int :in))


(declaim (inline c-get_frame-gfx-tiles))
(alien:def-alien-routine ("get_frame_gfx_tiles" c-get_frame-gfx-tiles)
           c-call:int
           (key c-call:int :in)
           (type c-call:int :in))


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
